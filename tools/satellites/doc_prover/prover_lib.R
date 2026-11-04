# prover_lib.R — moteur PUR du satellite doc_prover (jalon 4, slice S5 N6).
#
# Rôle : vérifier HEADLESS les assertions d'un document (défaut : le runbook
# docs/mcp/DRIVE_SESSION_RUNBOOK.md) contre l'état RÉEL du dépôt — fichiers,
# ancres de lignes, commits, contrats JSON — et classer un verdict par claim :
#   "pass"     — l'assertion du runbook est mesurée vraie sur le dépôt ;
#   "fail"     — mesurée FAUSSE = FINDING DE DRIFT (filé, pas réparé ici) ;
#   "untested" — non mesurable dans ce contexte (jamais deviné).
#
# Pureté : evaluate_claim / build_report / classify_drive_state / gate_live_ok
# ne lisent pas l'horloge, n'écrivent rien, ne lancent aucun processus — tout
# l'état arrive par les arguments (ctx : repo_root, drive_dir, git_cat, probe).
# Les seules fonctions impures (probes, horloge, subprocess) vivent en queue de
# fichier, clairement séparées, et ne sont appelées que par run_prover.R.
#
# Dépendances : base R stricte. jsonlite (DÉJÀ présent dans renv.lock, jamais
# installé ici) n'est utilisé que via requireNamespace et seulement pour lire
# les fixtures/JSON — dégradation gracieuse en "untested" documentée s'il manque.
#
# Le satellite n'est JAMAIS sourcé par l'app (0 app change, par construction) ;
# la conventions gate voit modules/ + R/ du package — pas tools/satellites/
# (cécité de garde assumée, documentée dans README.md).
#
# Règle 🔴 du runbook (§5), codée dans classify_drive_state : un état
# « heartbeat gelé + result.json:running » est classé JOB IN FLIGHT — le
# libellé "DEAD" n'existe nulle part dans ce moteur.

# ---------------------------------------------------------------------------
# Surface publique
# ---------------------------------------------------------------------------

#' Évalue un claim contre le contexte (pur).
#' @param claim list avec id, type ("static"|"live"), doc, line, assertion, expected
#' @param ctx   list(repo_root, drive_dir, git_cat, probe, now)
#' @return list(id, verdict "pass"|"fail"|"untested", evidence)
evaluate_claim <- function(claim, ctx) {
  .prover_out <- function(id, verdict, evidence) {
    list(id = id, verdict = verdict, evidence = evidence)
  }
  cid <- .prover_claim_id(claim)
  if (!.prover_is_claim(claim)) {
    return(.prover_out(cid, "untested",
      "claim malformé (id/type/line/assertion/expected attendus) — jamais évalué"))
  }
  checkers <- .prover_checkers()
  chk <- checkers[[claim$id]]
  if (is.null(chk)) {
    return(.prover_out(cid, "untested",
      sprintf("checker inconnu pour l'id « %s » — le claim n'est pas inventé", claim$id)))
  }
  res <- tryCatch(
    chk(claim, ctx),
    error = function(e) list(verdict = "untested",
      evidence = paste0("erreur du checker absorbée : ", conditionMessage(e)))
  )
  verdict <- if (is.list(res) && !is.null(res$verdict)) res$verdict else "untested"
  if (!verdict %in% c("pass", "fail", "untested")) verdict <- "untested"
  evidence <- if (is.list(res) && !is.null(res$evidence)) as.character(res$evidence) else "evidence absente"
  .prover_out(cid, verdict, evidence)
}

#' Agrège les résultats en structure de rapport (pur).
#' @param claims  liste des claims (métadonnées pour les lignes du rapport)
#' @param results liste de list(id, verdict, evidence) — alignée ou non sur claims
#' @param meta    liste libre (généré par le runner : horodatage, --live, etc.)
#' @return list(meta, rows, counts, findings, verdict_global)
build_report <- function(claims, results, meta = list()) {
  by_id <- list()
  for (cl in claims) {
    if (is.list(cl) && !is.null(cl$id)) by_id[[as.character(cl$id)]] <- cl
  }
  rows <- lapply(results, function(res) {
    if (!is.list(res)) {
      return(list(id = NA_character_, type = NA_character_, line = NA_integer_,
                  verdict = "untested", evidence = "résultat non-list — absorbé",
                  doc = NA_character_, assertion = NA_character_, expected = NA_character_))
    }
    id <- if (is.null(res$id)) NA_character_ else as.character(res$id)
    cl <- by_id[[id]]
    verdict <- if (!is.null(res$verdict) && res$verdict %in% c("pass", "fail", "untested")) {
      res$verdict
    } else "untested"
    list(
      id = id,
      type = if (!is.null(cl$type)) as.character(cl$type) else NA_character_,
      doc = if (!is.null(cl$doc)) as.character(cl$doc) else NA_character_,
      line = if (!is.null(cl$line)) as.integer(cl$line) else NA_integer_,
      assertion = if (!is.null(cl$assertion)) as.character(cl$assertion) else NA_character_,
      expected = if (!is.null(cl$expected)) as.character(cl$expected) else NA_character_,
      verdict = verdict,
      evidence = if (!is.null(res$evidence)) as.character(res$evidence) else "evidence absente"
    )
  })
  verdicts <- vapply(rows, function(r) r$verdict, character(1))
  counts <- list(
    pass = sum(verdicts == "pass"),
    fail = sum(verdicts == "fail"),
    untested = sum(verdicts == "untested")
  )
  findings <- Filter(function(r) r$verdict == "fail", rows)
  verdict_global <- if (counts$fail == 0L) "runbook ✅ complet" else
    sprintf("%d findings de drift filés", counts$fail)
  list(meta = meta, rows = rows, counts = counts, findings = findings,
       verdict_global = verdict_global)
}

#' Classe l'état drive (pur) — règles de rendu S1 + règle 🔴 du runbook §5.
#' @param ready  ready.json parsé (list) ou NULL s'il est absent
#' @param result result.json parsé (list) ou NULL
#' @param now    horloge de référence (POSIXct) — toujours passée, jamais Sys.time()
#' @return list(label, why) ; label ∈ VIVANT | JOB IN FLIGHT | STALE — HB GELÉ |
#'         NO_SESSION | READY.JSON ILLISIBLE — JAMAIS "DEAD".
classify_drive_state <- function(ready, result, now = Sys.time()) {
  .lab <- function(label, why) list(label = label, why = why)
  if (is.null(ready)) {
    return(.lab("NO_SESSION",
      "ready.json absent — aucun client vivant (contrat : ne pas inventer de session)"))
  }
  hb <- .prover_parse_iso(ready$hb_at)
  if (is.na(hb)) {
    return(.lab("READY.JSON ILLISIBLE",
      "hb_at illisible — la fraîcheur est indécidable, jamais interprétée comme vivante"))
  }
  timeout <- suppressWarnings(as.numeric(ready$hb_timeout_s))
  if (length(timeout) != 1L || is.na(timeout) || timeout <= 0) timeout <- 15
  # piège R mesuré : `now - hb` (difftime units="auto") rend des MINUTES dès
  # que l'écart est dans [60 s, 1 h) — 537 s sortaient « 8.95 » ; units="secs"
  # explicite obligatoire.
  age <- as.numeric(difftime(now, hb, units = "secs"))
  running <- !is.null(result) && !is.null(result$status) &&
    identical(as.character(result$status), "running")
  if (age <= timeout) {
    return(.lab("VIVANT", sprintf("heartbeat frais : %.1f s ≤ hb_timeout_s (%.0f s)", age, timeout)))
  }
  if (running) {
    return(.lab("JOB IN FLIGHT",
      sprintf("heartbeat gelé (%.1f s > %.0f s) MAIS result.json porte « running » ⇒ travail en cours, PAS une session morte (règle 🔴, leçon 537 s)", age, timeout)))
  }
  .lab("STALE — HB GELÉ",
    sprintf("heartbeat gelé (%.1f s > %.0f s) et pas de « running » en vol — ne pas armer ce token", age, timeout))
}

#' Gate « session armée détectée » (pur) — predicate sur la sonde, jamais sur
#' la session : ready.json frais ET hb_n qui avance.
gate_live_ok <- function(probe) {
  !is.null(probe) && is.list(probe) &&
    isTRUE(probe$fresh) && isTRUE(probe$hb_advance)
}

# ---------------------------------------------------------------------------
# Validation de claim + helpers purs
# ---------------------------------------------------------------------------

.prover_claim_id <- function(claim) {
  if (is.list(claim) && !is.null(claim$id)) as.character(claim$id) else NA_character_
}

.prover_is_claim <- function(claim) {
  is.list(claim) && length(claim) > 0L &&
    !is.null(claim$id) && is.character(as.character(claim$id)) && nzchar(as.character(claim$id)) &&
    !is.null(claim$type) && as.character(claim$type) %in% c("static", "live") &&
    !is.null(claim$line) && !is.na(suppressWarnings(as.integer(claim$line))) &&
    !is.null(claim$assertion) && nzchar(as.character(claim$assertion)) &&
    !is.null(claim$expected) && nzchar(as.character(claim$expected))
}

# lecture de fichier gracieuse (UTF-8) — NULL si absent/illisible
.prover_read <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"),
           error = function(e) NULL)
}

.prover_path <- function(ctx, ...) {
  file.path(ctx$repo_root, ...)
}

# fenêtre de lignes 1-based inclusive ; caractère vide si hors bornes
.prover_slice <- function(lines, from, to) {
  if (is.null(lines)) return(character(0))
  n <- length(lines)
  from <- max(1L, as.integer(from)); to <- min(n, as.integer(to))
  if (from > n || from > to) return(character(0))
  lines[from:to]
}

# premiers numéros de ligne (1-based) où le motif fixe apparaît
.prover_find_lines <- function(lines, pattern, fixed = TRUE, max_n = 5L) {
  if (is.null(lines)) return(integer(0))
  hits <- which(grepl(pattern, lines, fixed = fixed))
  head(hits, max_n)
}

# fenêtre contient-elle le motif fixe ?
.prover_window_has <- function(lines, from, to, patterns) {
  win <- .prover_slice(lines, from, to)
  all(vapply(patterns, function(p) any(grepl(p, win, fixed = TRUE)), logical(1)))
}

# parse ISO-8601 UTC (« 2026-11-03T22:30:01Z », avec ou sans fraction)
.prover_parse_iso <- function(x) {
  if (is.null(x)) return(as.POSIXct(NA))
  x <- as.character(x)
  pt <- suppressWarnings(as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  if (is.na(pt)) pt <- suppressWarnings(as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"))
  if (is.na(pt)) pt <- suppressWarnings(as.POSIXct(sub("Z$", "", x), format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  pt
}

# noms JSON d'un fichier via jsonlite — NULL gracieux si jsonlite absent
.prover_json_names <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(NULL)
  if (is.null(path) || !file.exists(path)) return(NULL)
  ok <- tryCatch({ j <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE); TRUE },
                 error = function(e) FALSE)
  if (!ok) return(NULL)
  j <- tryCatch(jsonlite::fromJSON(path, simplifyDataFrame = TRUE), error = function(e) NULL)
  if (is.null(j)) return(NULL)
  names(j)
}

# ---------------------------------------------------------------------------
# Registre des checkers — nom de claim -> function(claim, ctx).
# Chaque checker rend list(verdict = "pass"|"fail"|"untested", evidence = chr).
# Les ancres du RUNBOOK citées en commentaire sont celles mesurées le 2026-11-03.
# ---------------------------------------------------------------------------

.prover_checkers <- function() {
  list(

    # -- STATIC 1 — le launcher cité existe, ancre :4 porte le contrat header --
    `static-launcher` = function(claim, ctx) {
      p <- .prover_path(ctx, "tools", "launch_dev_drive.R")
      lines <- .prover_read(p)
      if (is.null(lines)) {
        return(list(verdict = "fail", evidence = "tools/launch_dev_drive.R ABSENT du dépôt"))
      }
      l4 <- if (length(lines) >= 4L) lines[4L] else ""
      if (grepl("The ONLY supported way", l4, fixed = TRUE)) {
        list(verdict = "pass",
             evidence = paste0("tools/launch_dev_drive.R:4 = « ", l4, " »"))
      } else {
        list(verdict = "fail",
             evidence = paste0("tools/launch_dev_drive.R:4 ne porte pas la citation ; mesuré : « ",
                               l4, " »"))
      }
    },

    # -- STATIC 2 — les trois outils read-only cités sont enregistrés ---------
    `static-tools-exist` = function(claim, ctx) {
      lines <- .prover_read(.prover_path(ctx, "scripts", "mcp_server.R"))
      if (is.null(lines)) {
        return(list(verdict = "untested", evidence = "scripts/mcp_server.R illisible — fait indisponible"))
      }
      txt <- paste(lines, collapse = "\n")
      tools3 <- c("transcripto_drive_status", "transcripto_drive_read_result", "transcripto_drive_snapshot")
      found <- vapply(tools3, function(t) {
        grepl(paste0('list\\(name\\s*=\\s*"', t, '"'), txt, perl = TRUE)
      }, logical(1))
      if (all(found)) {
        reg <- .prover_find_lines(lines, 'list(name = "transcripto_drive_', fixed = TRUE, max_n = 8L)
        list(verdict = "pass",
             evidence = paste0("3/3 outils enregistrés dans scripts/mcp_server.R (registrations mesurées aux lignes ",
                               paste(reg, collapse = ", "), ")"))
      } else {
        list(verdict = "fail",
             evidence = paste0("outils ABSENTS des registrations : ",
                               paste(tools3[!found], collapse = ", ")))
      }
    },

    # -- STATIC 3 — ancre runbook l.19 : les 3 outils nommés à mcp_server 104-106
    `static-anchor-mcp-banner` = function(claim, ctx) {
      lines <- .prover_read(.prover_path(ctx, "scripts", "mcp_server.R"))
      if (is.null(lines)) {
        return(list(verdict = "untested", evidence = "scripts/mcp_server.R illisible — fait indisponible"))
      }
      tools3 <- c("transcripto_drive_status", "transcripto_drive_read_result", "transcripto_drive_snapshot")
      # passe doc 2026-11-04 : le runbook réparé cite :104-107 (snapshot à la 107)
      win <- .prover_slice(lines, 104, 107)
      ok <- vapply(tools3, function(t) any(grepl(t, win, fixed = TRUE)), logical(1))
      snap_lines <- .prover_find_lines(lines, "transcripto_drive_snapshot", fixed = TRUE, max_n = 3L)
      if (all(ok)) {
        list(verdict = "pass",
             evidence = paste0("les lignes 104-107 nomment les trois outils read-only (ré-ancrage passe doc 2026-11-04 ; snapshot première occurrence : ligne ",
                               if (length(snap_lines)) as.character(snap_lines[1]) else "?", ")"))
      } else {
        list(verdict = "fail",
             evidence = paste0(
               "fenêtre 104-107 mesurée : ", paste(win, collapse = " | "),
               " — « transcripto_drive_snapshot » ABSENT de la fenêtre citée (première occurrence réelle : ligne ",
               if (length(snap_lines)) as.character(snap_lines[1]) else "?",
               ") — ancre dérivée (drift)"))
      }
    },

    # -- STATIC 4 — ancre runbook l.21/l.38 : code STALE_SESSION à :219 -------
    `static-anchor-stale-code` = function(claim, ctx) {
      lines <- .prover_read(.prover_path(ctx, "scripts", "mcp_server.R"))
      if (is.null(lines)) {
        return(list(verdict = "untested", evidence = "scripts/mcp_server.R illisible — fait indisponible"))
      }
      # passe doc 2026-11-04 : ré-ancré de :219 (périmé) à :231 (première occurrence réelle)
      l231 <- if (length(lines) >= 231L) lines[231L] else ""
      if (grepl("STALE_SESSION", l231, fixed = TRUE)) {
        list(verdict = "pass", evidence = paste0("scripts/mcp_server.R:231 = « ", l231, " »"))
      } else {
        occ <- .prover_find_lines(lines, "STALE_SESSION", fixed = TRUE, max_n = 6L)
        list(verdict = "fail",
             evidence = paste0(
               "scripts/mcp_server.R:231 ne porte PAS le code ; mesuré : « ", l231,
               " ». Le code existe ailleurs (occurrences mesurées : ",
               paste(occ, collapse = ", "),
               " ; table .ts_domain_codes à la première d'entre elles) — SUBSTANCE VRAIE, ancre dérivée (drift)"))
      }
    },

    # -- STATIC 5 — les commits cités existent (faits INJECTÉS via ctx) -------
    `static-commits-exist` = function(claim, ctx) {
      hashes <- c("7cccd6c4af46c75f285dce992ab6dc0c0c756cfe",
                  "db12712259cc11da5e6e877e525f095b59fd5fbe",
                  "59c488803e3486ca41fccb64bbbe6f6d5df16a1c")
      g <- ctx$git_cat
      if (is.null(g) || !all(hashes %in% names(g))) {
        return(list(verdict = "untested",
                    evidence = "faits git_cat indisponibles dans ctx — jamais devinés"))
      }
      types <- vapply(hashes, function(h) as.character(g[[h]]), character(1))
      if (all(types == "commit")) {
        list(verdict = "pass",
             evidence = paste0("git cat-file -t : ",
                               paste(paste0(substr(hashes, 1, 8), "=", types), collapse = ", "),
                               " — les 3 commits cités existent"))
      } else {
        list(verdict = "fail",
             evidence = paste0("objets non-commit : ",
                               paste(paste0(substr(hashes, 1, 8), "=", types), collapse = ", ")))
      }
    },

    # -- STATIC 6 — contrat result.json = 13 champs (épinglé S1) --------------
    `static-result-contract` = function(claim, ctx) {
      lines <- .prover_read(.prover_path(ctx, "tools", "pkg_skeleton", "R", "drive_watcher.R"))
      if (is.null(lines)) {
        return(list(verdict = "untested",
                    evidence = "tools/pkg_skeleton/R/drive_watcher.R illisible — contrat S1 non mesurable"))
      }
      fields13 <- c("protocol", "ack_seq", "status", "applied_at", "active_module",
                    "armed", "preserve_data", "errors", "warnings", "snapshot",
                    "job", "descriptor", "write_error")
      win <- .prover_slice(lines, 1936, 1990)
      hit <- vapply(fields13, function(f) {
        any(grepl(paste0("^\\s*", f, "\\s*="), win, perl = TRUE))
      }, logical(1))
      fix <- .prover_json_names(.prover_path(ctx, "tools", "satellites", "watchtower",
                                             "fixtures", "alive", "result.json"))
      fix_ok <- if (is.null(fix)) NA else setequal(fix, fields13)
      if (all(hit) && (is.na(fix_ok) || fix_ok)) {
        list(verdict = "pass",
             evidence = paste0(
               "13/13 champs mesurés au writer (drive_watcher.R:1936-1990) : ",
               paste(fields13, collapse = ", "),
               if (is.na(fix_ok)) "" else " ; fixture watchtower conformes (mêmes 13 noms)",
               " — la SUBSTANCE du runbook (status/applied_at documentés l.10/l.75) est couverte"))
      } else {
        list(verdict = "fail",
             evidence = paste0("champs ABSENTS de la fenêtre 1936-1990 : ",
                               paste(fields13[!hit], collapse = ", "),
                               if (!is.na(fix_ok) && !fix_ok) " ; fixture watchtower DIVERGENTE" else ""))
      }
    },

    # -- STATIC 7 — la commande 3 lit un champ `module` top-level qui n'existe pas
    `static-result-module-field` = function(claim, ctx) {
      lines <- .prover_read(.prover_path(ctx, "tools", "pkg_skeleton", "R", "drive_watcher.R"))
      if (is.null(lines)) {
        return(list(verdict = "untested",
                    evidence = "tools/pkg_skeleton/R/drive_watcher.R illisible — fait indisponible"))
      }
      win <- .prover_slice(lines, 1936, 1990)
      # passe doc 2026-11-04 : le runbook réparé lit « active_module » (champ
      # top-level RÉEL) — le claim vérifie ce champ, plus l'absence d'un
      # top-level « module » (qui n'existe que DANS le bloc job)
      top_active <- any(grepl("^\\s*active_module\\s*=", win, perl = TRUE))
      top_module <- any(grepl("^\\s*module\\s*=", win, perl = TRUE))
      job_module <- any(grepl("module\\s*=\\s*as\\.character\\(job\\$module\\)", .prover_slice(lines, 4801, 4827), perl = TRUE))
      fix <- .prover_json_names(.prover_path(ctx, "tools", "satellites", "watchtower",
                                             "fixtures", "alive", "result.json"))
      fix_has <- if (is.null(fix)) NA else "active_module" %in% fix
      if (top_active && !top_module) {
        list(verdict = "pass", evidence = paste0(
          "le champ top-level du payload result.json est « active_module » (fenêtre 1936-1990 ; ",
          if (job_module) "« module » confirmé DANS le bloc job 4801-4827" else "bloc job non mesurable",
          if (!is.na(fix_has)) paste0(" ; fixture watchtower : active_module ", if (fix_has) "présent" else "ABSENT") else "",
          ") — commande 3 réparée « Select-Object status, active_module, applied_at » cohérente (passe doc 2026-11-04)"))
      } else {
        list(verdict = "fail",
             evidence = paste0(
               "contrat mesuré inattendu (1936-1990) : active_module présent = ", top_active,
               " ; un top-level « module » existe = ", top_module,
               if (job_module) " ; « module » dans le bloc job (4801-4827)" else "",
               " — la commande 3 du runbook (l.75) ne correspond plus au contrat : drift SUBSTANTIEL"))
      }
    },

    # -- STATIC 8 — ready.json publie hb_at/hb_n/hb_timeout_s (défaut 15 s) ---
    `static-ready-contract` = function(claim, ctx) {
      lines <- .prover_read(.prover_path(ctx, "tools", "pkg_skeleton", "R", "drive_watcher.R"))
      if (is.null(lines)) {
        return(list(verdict = "untested",
                    evidence = "tools/pkg_skeleton/R/drive_watcher.R illisible — fait indisponible"))
      }
      need <- c("hb_at", "hb_n", "hb_timeout_s")
      win <- .prover_slice(lines, 561, 660)
      hit <- vapply(need, function(f) any(grepl(paste0("^\\s*", f, "\\s*="), win, perl = TRUE)), logical(1))
      def15 <- NA
      d <- .prover_find_lines(lines, "ts_drive_hb_timeout <- function", fixed = TRUE, max_n = 1L)
      if (length(d) == 1L) {
        body <- .prover_slice(lines, d, d + 8L)
        def15 <- any(grepl("return(15)", body, fixed = TRUE))
      }
      fix <- .prover_json_names(.prover_path(ctx, "tools", "satellites", "watchtower",
                                             "fixtures", "alive", "ready.json"))
      fix_ok <- if (is.null(fix)) NA else all(need %in% fix)
      if (all(hit) && isTRUE(def15) && (is.na(fix_ok) || fix_ok)) {
        list(verdict = "pass",
             evidence = paste0(
               "champs mesurés au writer ready (ts_drive_write_ready, 561-660) : ",
               paste(need[hit], collapse = ", "),
               " ; ts_drive_hb_timeout() retourne bien 15 par défaut (return(15) mesuré) ; ",
               "fixture watchtower : ", if (is.na(fix_ok)) "non lue (jsonlite absent — dégradation documentée)" else "les 3 champs présents"))
      } else {
        list(verdict = "fail",
             evidence = paste0("champs manquants writer : ", paste(need[!hit], collapse = ", "),
                               " ; défaut 15 s mesuré : ", if (is.na(def15)) "indéterminable" else def15))
      }
    },

    # -- STATIC 9 — les citations drive_watcher.R pointent un fichier EXISTANT
    # passe doc 2026-11-04 : le runbook réparé cite le NOUVEAU chemin
    # tools/pkg_skeleton/R/drive_watcher.R — le claim vérifie ce chemin
    `static-core-path` = function(claim, ctx) {
      old <- .prover_path(ctx, "R", "core", "drive_watcher.R")
      new <- .prover_path(ctx, "tools", "pkg_skeleton", "R", "drive_watcher.R")
      if (file.exists(new)) {
        list(verdict = "pass", evidence = paste0(
          "tools/pkg_skeleton/R/drive_watcher.R existe (runbook réparé : citations l.47/215/217/265 au nouveau chemin — passe doc 2026-11-04",
          if (!file.exists(old)) " ; l'ancien R/core/drive_watcher.R reste absent, conforme" else " ; ⚠️ l'ancien R/core/drive_watcher.R RÉAPPARU",
          ")"))
      } else {
        list(verdict = "fail",
             evidence = paste0(
               "tools/pkg_skeleton/R/drive_watcher.R ABSENT — les citations du runbook pointent un chemin inexistant (substance couverte par static-functions-exist)"))
      }
    },

    # -- STATIC 10 — les fonctions citées existent AU NOUVEAU chemin ----------
    `static-functions-exist` = function(claim, ctx) {
      lines <- .prover_read(.prover_path(ctx, "tools", "pkg_skeleton", "R", "drive_watcher.R"))
      if (is.null(lines)) {
        return(list(verdict = "untested",
                    evidence = "tools/pkg_skeleton/R/drive_watcher.R illisible — fait indisponible"))
      }
      checks <- c(
        ts_drive_session_id = "^ts_drive_session_id\\s*<-\\s*function",
        ts_drive_ready_fresh = "^ts_drive_ready_fresh\\s*<-\\s*function",
        session_id_pub = "^\\s*session_id\\s*="
      )
      res <- lapply(checks, function(pt) .prover_find_lines(lines, pt, fixed = FALSE, max_n = 3L))
      all_found <- all(vapply(res, length, integer(1)) > 0L)
      if (all_found) {
        list(verdict = "pass",
             evidence = paste0(
               "mesuré au nouveau chemin : ts_drive_session_id déf. l.",
               res$ts_drive_session_id[1], " ; ts_drive_ready_fresh déf. l.",
               res$ts_drive_ready_fresh[1], " ; session_id publié l.",
               paste(res$session_id_pub, collapse = ", "),
               " — substance du §4.4 VRAIE (seul le chemin/numéro cité par le runbook a dérivé, filé ailleurs)"))
      } else {
        missing <- names(checks)[vapply(res, length, integer(1)) == 0L]
        list(verdict = "fail",
             evidence = paste0("symboles absents de drive_watcher.R : ", paste(missing, collapse = ", ")))
      }
    },

    # -- STATIC 11 — la règle 🔴 est-elle écrite à AGENTS.md:7 ? --------------
    `static-agents-rule` = function(claim, ctx) {
      lines <- .prover_read(.prover_path(ctx, "AGENTS.md"))
      if (is.null(lines)) {
        return(list(verdict = "untested", evidence = "AGENTS.md illisible — fait indisponible"))
      }
      # passe doc 2026-11-04 : ré-ancré de :7 (MODE BOOST) à :11 (règle 🔴)
      l11 <- if (length(lines) >= 11L) lines[11L] else ""
      if (grepl("RÈGLE OPÉRATIONNELLE", l11, fixed = TRUE) && grepl("ready_fresh", l11, fixed = TRUE)) {
        list(verdict = "pass", evidence = "AGENTS.md:11 porte la règle opérationnelle 🔴 (ré-ancrage passe doc 2026-11-04)")
      } else {
        occ <- .prover_find_lines(lines, "RÈGLE OPÉRATIONNELLE", fixed = TRUE, max_n = 3L)
        list(verdict = "fail",
             evidence = paste0(
               "AGENTS.md:11 mesuré = « ", substr(l11, 1, 120),
               "… » — la règle 🔴 « ne JAMAIS conclure “session morte”… » mesurée à la ligne ",
               if (length(occ)) paste(occ, collapse = ", ") else "INTRROUVABLE",
               " (cité AGENTS.md:11 aux l.226/236/251 du runbook) — ancre dérivée (drift)"))
      }
    },

    # -- STATIC 12 — ancres MAJEURES du contrat tools/_drive/README.md --------
    `static-drive-readme-anchors` = function(claim, ctx) {
      lines <- .prover_read(.prover_path(ctx, "tools", "_drive", "README.md"))
      if (is.null(lines)) {
        return(list(verdict = "untested", evidence = "tools/_drive/README.md illisible — fait indisponible"))
      }
      items <- list(
        c("l.20 result.json « Read this, never stdout »", 20, 20, "Read this, never stdout"),
        c("l.36-52 contrat heartbeat (hb_at/hb_n/hb_timeout_s)", 36, 52, "hb_at"),
        c("l.36-52 hb_n compteur", 36, 52, "hb_n"),
        c("l.36-52 hb_timeout_s défaut 15 s", 36, 52, "hb_timeout_s"),
        c("l.50-52 dé-armement ne supprime pas ready.json", 50, 52, "Disarming"),
        c("l.124-125 « no live client »", 124, 125, "no live client"),
        c("l.150-160 volet B REFUSED, not queued", 150, 160, "REFUSED, not queued"),
        c("l.188-209 résidu job long (852,7 s)", 188, 209, "852.7"),
        c("l.197-203 snapshot frais par poll", 197, 203, "scenario per poll"),
        c("l.269-283 rotation de token onglet visible", 269, 283, "ROTATES the token"),
        c("l.319-322 session antérieure non armable", 319, 322, "cannot be armed")
      )
      misses <- character(0)
      for (it in items) {
        if (!.prover_window_has(lines, as.integer(it[2]), as.integer(it[3]), it[4])) {
          misses <- c(misses, it[1])
        }
      }
      if (length(misses) == 0L) {
        list(verdict = "pass",
             evidence = "11/11 ancres majeures mesurées : chaque fenêtre citée porte la substance citée")
      } else {
        list(verdict = "fail",
             evidence = paste0("ancres majeures sans la substance citée : ", paste(misses, collapse = " ; ")))
      }
    },

    # -- STATIC 13 — ancres FINES du contrat (off-by-N attendues) -------------
    `static-drive-readme-minor` = function(claim, ctx) {
      lines <- .prover_read(.prover_path(ctx, "tools", "_drive", "README.md"))
      if (is.null(lines)) {
        return(list(verdict = "untested", evidence = "tools/_drive/README.md illisible — fait indisponible"))
      }
      # passe doc 2026-11-04 : ancres réparées — :39 (cadence ~3 s), :40
      # (hb_n 0 at boot), :145 (étape 7), :557-558 (citation boot lent)
      items <- list(
        c("l.185 : cadence ~3 s à README:39", 39, 39, "~3 s"),
        c("l.115 : hb_n « 0 at boot » à README:40", 40, 40, "0 at boot"),
        c("l.182 : étape 7 « Disarm by removing arm.json » à README:145", 145, 145, "7. Disarm by removing"),
        c("l.190 : « not evidence of a lost write » à README:557-558", 557, 558, "not evidence of a lost write")
      )
      misses <- character(0)
      whereis <- character(0)
      for (it in items) {
        if (!.prover_window_has(lines, as.integer(it[2]), as.integer(it[3]), it[4])) {
          misses <- c(misses, it[1])
          real <- .prover_find_lines(lines, it[4], fixed = TRUE, max_n = 1L)
          whereis <- c(whereis, paste0(it[4], " -> ligne réelle ",
                                       if (length(real)) as.character(real[1]) else "absente"))
        }
      }
      if (length(misses) == 0L) {
        list(verdict = "pass", evidence = "4/4 ancres fines mesurées exactes")
      } else {
        list(verdict = "fail",
             evidence = paste0("ancres fines dérivées (", length(misses), "/4) : ",
                               paste(misses, collapse = " ; "),
                               " — positions réelles mesurées : ", paste(whereis, collapse = " ; ")))
      }
    },

    # -- STATIC 14 — ancres de l'audit cité -----------------------------------
    `static-audit-anchors` = function(claim, ctx) {
      lines <- .prover_read(.prover_path(ctx, "docs", "audits", "COUNTS_NON_INTEGER_2026-10-24.md"))
      if (is.null(lines)) {
        return(list(verdict = "untested", evidence = "docs/audits/COUNTS_NON_INTEGER_2026-10-24.md illisible/absent — fait indisponible"))
      }
      items <- list(
        c("l.15-19 table des 3 outils STALE_SESSION", 15, 19, "STALE_SESSION"),
        c("l.23-25 pid 74844 mort", 23, 25, "74844"),
        c("l.26-27 rsession 48220 sur le port", 26, 27, "48220"),
        c("l.28-29 result.json 20 h d'âge", 28, 29, "20 h"),
        c("l.31-32 « aucune visibilité »", 31, 32, "aucune visibilité")
      )
      misses <- character(0)
      for (it in items) {
        if (!.prover_window_has(lines, as.integer(it[2]), as.integer(it[3]), it[4])) {
          misses <- c(misses, it[1])
        }
      }
      if (length(misses) == 0L) {
        list(verdict = "pass",
             evidence = "5/5 ancres de l'audit mesurées : chaque fenêtre citée porte la substance citée (§1, l.22/88-89/33 du runbook)")
      } else {
        list(verdict = "fail",
             evidence = paste0("ancres audit sans la substance citée : ", paste(misses, collapse = " ; ")))
      }
    },

    # ------------------------------------------------------------------ LIVE --
    # Chaque claim live exige sa sonde dans ctx$probe (collectée UNIQUEMENT par
    # run_prover.R sous --live) — sans sonde : untested, jamais inventé.

    `live-hb-advance` = function(claim, ctx) {
      pr <- ctx$probe
      if (is.null(pr) || !is.list(pr)) {
        return(list(verdict = "untested",
                    evidence = "gate live non satisfait — pas de sonde (--live absent ou session non détectée)"))
      }
      if (isTRUE(pr$hb_advance)) {
        list(verdict = "pass",
             evidence = sprintf("mesuré : hb_n %s -> %s (2 lectures espacées ~4 s) — session vivante",
                                format(pr$hb_n1), format(pr$hb_n2)))
      } else {
        list(verdict = "fail",
             evidence = sprintf("mesuré : hb_n %s -> %s — NE bat pas (commande 2 du runbook : handshake mort)",
                                format(pr$hb_n1), format(pr$hb_n2)))
      }
    },

    `live-hb-age` = function(claim, ctx) {
      pr <- ctx$probe
      if (is.null(pr) || !is.list(pr) || is.null(pr$age_s) || is.na(pr$age_s)) {
        return(list(verdict = "untested",
                    evidence = "gate live non satisfait — âge du heartbeat non mesuré"))
      }
      ok <- is.finite(pr$age_s) && pr$age_s >= -5 && pr$age_s <= pr$hb_timeout_s
      if (ok) {
        list(verdict = "pass",
             evidence = sprintf("âge heartbeat mesuré %.1f s ≤ hb_timeout_s (%.0f s)", pr$age_s, pr$hb_timeout_s))
      } else {
        list(verdict = "fail",
             evidence = sprintf("âge heartbeat mesuré %.1f s > hb_timeout_s (%.0f s) — session non fraîche", pr$age_s, pr$hb_timeout_s))
      }
    },

    `live-launch-arms` = function(claim, ctx) {
      pr <- ctx$probe
      if (is.null(pr) || !is.list(pr)) {
        return(list(verdict = "untested",
                    evidence = "gate live non satisfait — pas de sonde (launch_dev_drive.R n'est PAS relancé par le prover : invasif)"))
      }
      if (is.na(pr$pid_alive)) {
        return(list(verdict = "untested",
                    evidence = "vitalité du PID indéterminable (tasklist indisponible) — jamais deviné"))
      }
      if (isTRUE(pr$pid_alive) && isTRUE(pr$armed) && isTRUE(pr$hb_advance)) {
        list(verdict = "pass",
             evidence = sprintf("session armée mesurée : pid %s vivant, armed=true, hb_n avance — signes du lancement supporté (§4.1)",
                                format(pr$pid)))
      } else {
        list(verdict = "fail",
             evidence = sprintf("pid_alive=%s armed=%s hb_advance=%s — la session armée ne présente pas les signes mesurés attendus",
                                format(pr$pid_alive), format(pr$armed), format(pr$hb_advance)))
      }
    },

    `live-rearm-keeps` = function(claim, ctx) {
      pr <- ctx$probe
      rr <- if (!is.null(pr) && is.list(pr)) pr$rearm else NULL
      if (is.null(rr)) {
        return(list(verdict = "untested",
                    evidence = "re-arm non exécuté (exige --live ; écriture limitée au drop-box gitigné tools/_drive/*.json, .gitignore:100)"))
      }
      if (isTRUE(rr$token_same) && isTRUE(rr$armed) && isTRUE(rr$hb_advance)) {
        list(verdict = "pass",
             evidence = sprintf("re-arm mesuré : session_token CONSERVÉ (%s), armed=true, hb_n %s -> %s (repart — signal attendu de reprise, §4.2)",
                                rr$token, format(rr$hb_n1), format(rr$hb_n2)))
      } else if (!isTRUE(rr$token_same)) {
        list(verdict = "fail",
             evidence = "le token a CHANGÉ après re-arm — contredit §4.2 (la même session est conservée)")
      } else {
        list(verdict = "fail",
             evidence = sprintf("re-arm mesuré incomplet : token_same=%s armed=%s hb_advance=%s",
                                format(rr$token_same), format(rr$armed), format(rr$hb_advance)))
      }
    },

    `live-stale-tool` = function(claim, ctx) {
      pr <- ctx$probe
      st <- if (!is.null(pr) && is.list(pr)) pr$stale_tool else NULL
      if (is.null(st)) {
        return(list(verdict = "untested",
                    evidence = "aucun handshake PÉRIMÉ observé pendant la sonde (la session détectée est vivante) — l'expérience exige un ready.json stale + l'outil read-only ; jamais fabriquée"))
      }
      if (identical(as.character(st$code), "STALE_SESSION")) {
        list(verdict = "pass",
             evidence = "transcripto_drive_status a répondu STALE_SESSION sur le handshake périmé (§1)")
      } else {
        list(verdict = "fail",
             evidence = paste0("réponse mesurée de l'outil sur handshake périmé : « ",
                               if (!is.null(st$code)) as.character(st$code) else "vide",
                               " » ≠ STALE_SESSION"))
      }
    },

    `live-job-inflight` = function(claim, ctx) {
      pr <- ctx$probe
      inf <- if (!is.null(pr) && is.list(pr)) pr$inflight else NULL
      if (is.null(inf) || !isTRUE(inf$observed)) {
        return(list(verdict = "untested",
                    evidence = "état « ready.json périmé + result.json:running » non observé pendant la sonde — la fenêtre de job en vol n'est pas fabricable sans job long réel ; jamais inventée"))
      }
      st <- classify_drive_state(inf$ready, inf$result, inf$now)
      if (identical(st$label, "JOB IN FLIGHT")) {
        list(verdict = "pass",
             evidence = paste0("état réel observé classé JOB IN FLIGHT : ", st$why,
                               " — jamais DEAD (règle 🔴 §5)"))
      } else {
        list(verdict = "fail",
             evidence = paste0("état « running en vol » mesuré mais classé « ", st$label,
                               " » ≠ JOB IN FLIGHT — contredit la règle 🔴"))
      }
    }
  )
}

# ---------------------------------------------------------------------------
# SONDES IMPURES — appelées UNIQUEMENT par run_prover.R (jamais en test) :
# elles lisent l'horloge, dorment, invoquent des processus. Rien ici ne
# fonctionne sur la foi d'une session supposée : chaque fait est mesuré.
# ---------------------------------------------------------------------------

#' Sonde la session drive : 2 lectures de ready.json espacées ~4 s (commande 2),
#' âge du heartbeat (commande 1), vitalité du PID, result.json, re-arm éventuel.
#' @param do_rearm      écrire arm.json (même token) puis relire — §4.2
#' @param do_stale_tool si handshake périmé : interroger l'outil read-only (§1)
probe_drive_session <- function(drive_dir, wait_s = 4L,
                                do_rearm = FALSE, do_stale_tool = FALSE,
                                repo_root = NULL) {
  out <- list(ready_missing = TRUE, fresh = FALSE, hb_advance = FALSE,
              age_s = NA_real_, hb_timeout_s = NA_real_, hb_n1 = NA_integer_,
              hb_n2 = NA_integer_, pid = NA_integer_, pid_alive = NA,
              armed = NA, stale_handshake = FALSE, result_status = NA_character_,
              rearm = NULL, stale_tool = NULL, inflight = NULL)
  ready_path <- file.path(drive_dir, "ready.json")
  if (!file.exists(ready_path) || !requireNamespace("jsonlite", quietly = TRUE)) {
    return(out)
  }
  read_ready <- function() {
    tryCatch(jsonlite::fromJSON(ready_path, simplifyDataFrame = TRUE),
             error = function(e) NULL)
  }
  r1 <- read_ready()
  if (is.null(r1)) return(out)
  out$ready_missing <- FALSE
  hb1 <- .prover_parse_iso(r1$hb_at)
  now1 <- Sys.time()
  out$age_s <- if (!is.na(hb1)) as.numeric(difftime(now1, hb1, units = "secs")) else NA_real_
  out$hb_timeout_s <- suppressWarnings(as.numeric(r1$hb_timeout_s))
  if (length(out$hb_timeout_s) != 1L || is.na(out$hb_timeout_s) || out$hb_timeout_s <= 0) {
    out$hb_timeout_s <- 15
  }
  out$hb_n1 <- suppressWarnings(as.integer(r1$hb_n))
  out$pid <- suppressWarnings(as.integer(r1$pid))
  out$armed <- isTRUE(r1$armed)
  out$fresh <- !is.na(out$age_s) && out$age_s >= -5 && out$age_s <= out$hb_timeout_s

  # vitalité du PID (Windows : tasklist ; échec = NA, jamais deviné)
  if (!is.na(out$pid)) {
    tl <- tryCatch({
      o <- system2("tasklist", c("/FI", paste0('"PID eq ', out$pid, '"')), stdout = TRUE, stderr = FALSE)
      any(grepl(as.character(out$pid), o, fixed = TRUE))
    }, error = function(e) NA, warning = function(w) NA)
    out$pid_alive <- tl
  }

  # commande 2 : hb_n avance-t-il ?
  Sys.sleep(wait_s)
  r2 <- read_ready()
  if (!is.null(r2)) {
    out$hb_n2 <- suppressWarnings(as.integer(r2$hb_n))
    out$hb_advance <- !is.na(out$hb_n1) && !is.na(out$hb_n2) && out$hb_n2 > out$hb_n1
    out$armed <- isTRUE(r2$armed)
  }
  out$stale_handshake <- !is.na(out$age_s) && out$age_s > out$hb_timeout_s

  # result.json (commande 3)
  res_path <- file.path(drive_dir, "result.json")
  result <- if (file.exists(res_path)) {
    tryCatch(jsonlite::fromJSON(res_path, simplifyDataFrame = TRUE), error = function(e) NULL)
  } else NULL
  out$result_status <- if (!is.null(result) && !is.null(result$status)) as.character(result$status) else NA_character_

  # règle 🔴 : fenêtre « job en vol » observée ?
  if (out$stale_handshake && !is.null(result) &&
      identical(out$result_status, "running")) {
    out$inflight <- list(observed = TRUE, ready = r2 %||% r1, result = result, now = Sys.time())
  }

  # §4.2 re-arm — écriture UNIQUEMENT sur demande explicite (--live), drop-box gitignorée
  if (do_rearm && out$fresh && out$hb_advance && !is.null(r2) && !is.null(r2$session_token)) {
    tok <- as.character(r2$session_token)
    arm_path <- file.path(drive_dir, "arm.json")
    ok <- tryCatch({
      writeLines(jsonlite::toJSON(list(token = tok, armed = TRUE), auto_unbox = TRUE), arm_path)
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (ok) {
      Sys.sleep(wait_s)
      r3 <- read_ready()
      if (!is.null(r3)) {
        out$rearm <- list(
          token = tok,
          token_same = identical(as.character(r3$session_token), tok),
          armed = isTRUE(r3$armed),
          hb_n1 = out$hb_n2,
          hb_n2 = suppressWarnings(as.integer(r3$hb_n))
        )
        out$rearm$hb_advance <- !is.na(out$rearm$hb_n1) && !is.na(out$rearm$hb_n2) &&
          out$rearm$hb_n2 > out$rearm$hb_n1
      }
    }
  }

  # §1 outil read-only sur handshake périmé — subprocess lourd, opt-in --live
  if (do_stale_tool && out$stale_handshake && !is.null(repo_root)) {
    req <- c(
      '{"jsonrpc":"2.0","id":0,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"doc-prover","version":"1.0"}}}',
      '{"jsonrpc":"2.0","method":"notifications/initialized"}',
      '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"transcripto_drive_status","arguments":{}}}'
    )
    ans <- tryCatch(
      suppressWarnings(system2("Rscript",
        c("--no-init-file", shQuote(file.path(repo_root, "scripts", "mcp_server.R"))),
        input = req, stdout = TRUE, stderr = FALSE, timeout = 180)),
      error = function(e) NULL, warning = function(w) NULL)
    code <- NA_character_
    if (length(ans)) {
      hit <- ans[grepl("STALE_SESSION", ans, fixed = TRUE)]
      if (length(hit)) {
        m <- regmatches(hit[1], regexpr("STALE_SESSION", hit[1], fixed = TRUE))
        code <- if (length(m)) m else "STALE_SESSION"
      } else {
        code <- paste(ans[1:min(3, length(ans))], collapse = " | ")
      }
    }
    out$stale_tool <- list(code = code, ran = !is.null(ans))
  }

  out
}

# petit %||% local (base R, pas de dépendance rlang)
`%||%` <- function(a, b) if (is.null(a)) b else a

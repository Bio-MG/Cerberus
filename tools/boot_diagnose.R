# =============================================================================
# tools/boot_diagnose.R — boot instrumenté de diagnostic (F2, 2026-11-04)
# =============================================================================
# CONTEXTE (finding F2, ledger round8 §4) : le boot de l'app est
# non-déterministe — des boots meurent après « Listening » (exit 127, aucune
# erreur dans le log) ou restent vivants sans listener, ready.json jamais
# écrit (server() jamais atteint) ; un boot sur plusieurs initialise
# complètement. Diagnostic MESURÉ, jamais déduit : ce lanceur enveloppe
# l'UNIQUE boot supporté (tools/launch_dev_drive.R — VERBATIM, aucune
# réimplémentation) d'une sonde externe concourante qui horodate la ligne de
# vie complète du boot, puis capture le code de sortie réel du process.
#
# LES MARQUEURS DÉJÀ ÉMIS PAR L'APP (aucune édition de app.R requise) :
#   « Listening on http://127.0.0.1:PORT »          → httpuv écoute
#   « [TranscriptoShiny drive] session_token=... »  → server() a atteint
#     l'attach drive (app.R:761) — LE discriminant « server() a couru »
#   « [drive] ready.json write FAILED at attach: »  → l'attach a couru mais
#     l'écriture du handshake a échoué (drive_watcher.R:5021)
#
# CE QUE LA SONDE MESURE (cycle 1 s, logs horodatés QC/boot_diagnose/<ts>/) :
#   - GET http://127.0.0.1:<port>/ → code HTTP réel (le client peut-il parler
#     au serveur ?) — via curl:: si présent, repli socket TCP sinon ;
#   - tools/_drive/ready.json : existence + mtime (le handshake est-il posé ?) ;
#   - population Rscript + WS total (tasklist CSV) + RAM libre (CIM, 1 cycle
#     sur 5) — corrélation RAM/process parallèles exigée par la mission.
#
# SÉCURITÉ (discipline runbook §3-B) :
#   - le wrapper attend l'app avec un délai --max-secs ; au délai il tue
#     l'arbre DU SEUL process app qu'il a lui-même lancé (taskkill /T /F sur
#     le pid identifié à l'éclosion — jamais un process préexistant, jamais
#     rsession) et consigne exit_code=124 TIMEOUT_KILLED — kill POSSÉDÉ et
#     documenté, arbre complet = zéro fuite mirai ;
#   - à la sortie naturelle de l'app, AUCUN kill : le code de sortie réel est
#     capturé (c'est LA donnée du signature A « exit 127 ») ;
#   - le résumé final appelle tools/mirai_janitor.R --list (lecture seule)
#     pour consigner l'état du pool mirai à la fin du boot ;
#   - aucune écriture dans tools/_drive/ (la sonde LIT ready.json, jamais plus).
#
# RÉPARATION (F2 passe 2, 2026-11-05 — défauts MESURÉS passe 1) : les verdicts
# reposaient sur applog (capture bash-level du stdout de l'app) avec DEUX
# cécités distinctes :
#   (1) le stdout R-level est BUFFERISÉ PAR BLOCS et le tampon est PERDU au
#       taskkill /F ; « Listening » (httpuv, C-level) passait, le marqueur R
#       « session_token= » non ;
#   (2) readLines() sur un log contenant des octets invalides dans la locale
#       du lecteur échoue SILENCIEUSEMENT (tryCatch → NULL) À CHAQUE CYCLE —
#       mesuré : la sonde n'a JAMAIS vu « Listening » (204 cycles http=NA)
#       alors que la ligne était dans le fichier ;
#   (3) la sonde wait=FALSE pouvait rendre pid=0 et tourner orpheline
#       (boucle infinie) — désormais BORNÉE (self_stop à max_cycles).
# CORRECTIFS SANS AUCUNE ÉDITION DE L'APP : le wrapper génère un shim
# R_PROFILE_USER dans le répertoire QC du run et le pose en environnement du
# SEUL process app lancé (hérité par ses enfants PSOCK éventuels) :
#   - il source le .Rprofile projet VERBATIM (activation renv préservée —
#     R_PROFILE_USER REMPLACE le .Rprofile du cwd, d'où le source explicite) ;
#   - il écrit des marqueurs horodatés « MARK <t> pid=<pid> phase=… » via
#     cat(file=, append=) qui OUVRE/ÉCRIT/FERME le fichier à chaque appel ⇒
#     NON bufferisé, les lignes sont sur disque même après un kill /F.
# BÉNÉFICE DIAGNOSTIC F2 : si les workers PSOCK lisent les profils, chaque
# worker écrit SON propre timeline renv (profile_start→profile_done) ⇒
# l'hypothèse « activation renv ~54 s/worker » est falsifiable directement ;
# si un seul pid apparaît, l'hypothèse est morte (les workers --vanilla ne
# lisent aucun profil) — les deux issues sont une mesure, jamais une déduction.
# FIABILITÉ DES VERDICTS (nouveau) :
#   - listening = applog « Listening » OU preuve sonde (http>=200, externe,
#     kill-proof) — DIAG porte l'evidence ;
#   - server_ran porte SON evidence : ready.json (fichier, kill-proof) >
#     applog session_token (INDICATIF seulement — perdu au kill) > none ;
#   - stall_proxy_s = écart t_listening → t_first_http_200 (sonde 1 s) ;
#   - stats profils : renv_s du process main + des workers éventuels.
#
# USAGE
#   LC_ALL=French_France.65001 Rscript tools/boot_diagnose.R --observe \
#       [--max-secs 240] [--app-args "--headless"]
#   (--probe est un enfant interne — ne pas l'invoquer à la main ;
#    SANS verbe : usage seul, aucune action, discipline run_dock.R ;
#    stdout machine : grep 'DIAG:' — le bruit renv de .Rprofile est normal)
#
# SORTIE (machine-parseable, lignes DIAG:) :
#   DIAG: exit_code=<n|124> end_mode=<owned_kill|timeout_124|self_exit>
#     (MESURÉ 2026-11-05 : le timeout system2 de cet hôte rend 0 SANS tuer
#     l'arbre de façon déterministe — end_mode = le fait vérifié par sondage
#     CIM, exit_code seul n'est PAS fiable ici)
#   DIAG: server_ran=<yes|no> evidence=<ready.json|applog|none>
#   DIAG: ready_json=<yes|no> ready_write_failed=<yes|no>
#   DIAG: http_ok=<yes|no> http_max=<code>
#   DIAG: stall_proxy_s=<n|NA>   (t_listening → t_first_http_200 STRICT, sonde)
#   DIAG: http500_after_listening_s=<n|NA>  (signature F2 mesurée 2026-11-05 :
#     refus TCP ~177 s après Listening, puis 500 sans JAMAIS de 200)
#   DIAG: profiles=<n_pids> main_renv_s=<n|NA> worker_renv_s=<list|NA>
#   DIAG: leaks=<n>                  (janitor --list à la fin)
# =============================================================================

.bd_root <- function() {
  args_src <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  base <- if (!is.null(args_src)) dirname(normalizePath(args_src, winslash = "/")) else getwd()
  root <- normalizePath(file.path(base, ".."), winslash = "/", mustWork = FALSE)
  # Fallback identique à launch_dev_drive.R : si app.R n'est pas là, le cwd
  # (la recette impose cwd = racine du dépôt) fait foi.
  if (!file.exists(file.path(root, "app.R"))) {
    root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  }
  root
}

.bd_now <- function() format(Sys.time(), "%H:%M:%OS3")

.bd_log <- function(path, ...) {
  cat(sprintf("[%s] ", .bd_now()), ..., "\n", sep = "", file = path, append = TRUE)
}

.bd_rscript <- function() "D:/Data_science/R-4.4.2/bin/x64/Rscript.exe"

.bd_r_population <- function() {
  out <- suppressWarnings(tryCatch(
    system2("tasklist", c("/FI", shQuote("IMAGENAME eq Rscript.exe"),
                          "/FO", "CSV", "/NH"),
            stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)))
  rows <- out[grepl('"?[0-9]+"?', out) & !grepl("INFO:", out)]
  kbs <- suppressWarnings(as.numeric(gsub("[^0-9]", "",
    regmatches(rows, regexpr('"[0-9 ,]+ Ko"?\\s*$', rows)))))
  pids <- suppressWarnings(as.integer(gsub('"', "",
    regmatches(rows, regexpr(',"?[0-9]+",', rows)))))
  list(n = length(rows), total_mb = round(sum(kbs, na.rm = TRUE) / 1024),
       pids = pids[!is.na(pids)])
}

.bd_rscript_pids_matching <- function(pattern) {
  # PIDs Rscript dont la cmdline porte `pattern` (l'app = chaîne shim
  # bin/Rscript.exe → cmd.exe → x64/Rscript.exe ; seuls les Rscript comptent).
  # Une requête CIM.
  ps <- paste0(
    "[Console]::OutputEncoding=[Text.Encoding]::UTF8; ",
    "Get-CimInstance Win32_Process -Filter \"Name='Rscript.exe'\" | ",
    "Where-Object { $_.CommandLine -match '", pattern, "' } | ",
    "ForEach-Object { '{0}|{1}' -f $_.ProcessId, $_.ParentProcessId }")
  out <- suppressWarnings(tryCatch(
    system2("powershell", c("-NoProfile", "-Command", shQuote(ps)),
            stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)))
  out <- out[nzchar(trimws(out))]
  parts <- strsplit(out, "|", fixed = TRUE)
  parts <- parts[vapply(parts, function(p) length(p) == 2L, logical(1))]
  if (length(parts) == 0L) return(data.frame(pid = integer(0), ppid = integer(0)))
  data.frame(pid = as.integer(vapply(parts, `[`, character(1), 1)),
             ppid = as.integer(vapply(parts, `[`, character(1), 2)))
}

.bd_free_ram_mb <- function() {
  out <- suppressWarnings(tryCatch(
    system2("powershell", c("-NoProfile", "-Command",
      shQuote("Write-Output ([long](Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory)")),
      stdout = TRUE, stderr = FALSE),
    error = function(e) NA_real_))
  suppressWarnings(as.numeric(trimws(out[1]))) / 1024
}

.bd_http_get <- function(port, timeout_s = 2) {
  if (is.na(port)) return(NA_integer_)
  tryCatch({
    if (requireNamespace("curl", quietly = TRUE)) {
      as.integer(curl::curl_fetch_memory(
        sprintf("http://127.0.0.1:%d/", as.integer(port)),
        handle = curl::new_handle(timeout = timeout_s))$status_code)
    } else {
      s <- suppressWarnings(tryCatch(
        socketConnection("127.0.0.1", port, open = "r+", timeout = timeout_s),
        error = function(e) NULL))
      if (is.null(s)) return(0L)
      close(s)
      1L
    }
  }, error = function(e) 0L)
}

# ── lecture byte-level (2ᵉ cause de cécité, MESURÉE 2026-11-05) ──────────────
# readLines() échoue silencieusement (tryCatch → NULL À CHAQUE CYCLE) sur un
# log contenant des octets invalides dans la locale du lecteur — mesuré : la
# sonde n'a JAMAIS vu « Listening » (204 cycles http=NA) alors que la ligne
# était dans applog. Correctif : lecture BINAIRE + motifs 100 % ASCII avec
# useBytes=TRUE — aucune interprétation d'encodage n'a plus lieu.
.bd_read_all <- function(path) {
  tryCatch({
    sz <- file.size(path)
    if (is.na(sz) || sz == 0) return("")
    raw <- readBin(path, "raw", n = sz)
    # NUL embarqués MESURÉS (2026-11-05) : le tampon de connexion R (sink vers
    # app_r.log) est flushé avec son remplissage zéro au-delà de la fin
    # logique — rawToChar erre (« embedded nul in string »). Strip AVANT
    # conversion : lecture binaire + motifs 100 % ASCII avec useBytes=TRUE.
    raw <- raw[raw != as.raw(0)]
    if (!length(raw)) return("")
    s <- rawToChar(raw)
    Encoding(s) <- "bytes"
    s
  }, error = function(e) "")
}

# ── shim R_PROFILE_USER : marqueur NON bufferisé + capture stdout/stderr ─────
# (1) Marqueurs : cat(file=, append=) qui OUVRE/ÉCRIT/FERME le fichier à chaque
#     appel — rien ne reste dans un tampon de process, donc rien n'est perdu
#     au kill. Le shim REMPLACE le .Rprofile projet dans CE process
#     (sémantique R_PROFILE_USER) : il le source VERBATIM d'abord — activation
#     renv préservée, y compris pour tout worker PSOCK héritant la variable.
# (2) Capture : sink() de stdout ET stderr vers app_r.log — MESURÉ 2026-11-05 :
#     un fichier tenu par un redirect bash est ILLISIBLE par un 3ᵉ process
#     (violation de partage, « cannot open the connection ») ⇒ la sonde ne
#     pouvait JAMAIS lire applog pendant le run (cause première de la cécité,
#     devant la locale et le buffering) ; un fichier tenu par une connection R
#     de l'app est LISIBLE pendant le run et les écritures TRAVERSENT à chaque
#     cat/message (validé par mesure) ⇒ contenu complet SUR DISQUE même au
#     taskkill /F, et sondable en temps réel.
.bd_make_shim <- function(shim_path, marker_path, applog_r, root) {
  mk <- function(p) gsub("\\\\", "/", normalizePath(p, winslash = "/", mustWork = FALSE))
  mpath <- mk(marker_path)
  apath <- mk(applog_r)
  rprofile <- mk(file.path(root, ".Rprofile"))
  tpl <- '
# Généré par tools/boot_diagnose.R — shim R_PROFILE_USER, ne pas éditer.
.mk <- function(phase, extra = "") {
  cat(sprintf("MARK %s pid=%d phase=%s%s\\n",
      format(Sys.time(), "%H:%M:%OS3"), Sys.getpid(), phase, extra),
      file = "___MARKER___", append = TRUE)
}
.mk("profile_start")
.bd_renv_ok <- tryCatch(
  { source("___RPROFILE___", local = new.env(parent = globalenv())); "yes" },
  error = function(e) paste0("no: ", conditionMessage(e)))
.mk("profile_done", paste0(" renv_ok=", .bd_renv_ok))
rm(.mk, .bd_renv_ok)
.bd_con <- file("___APPLOG___", open = "wt")
sink(.bd_con)
sink(.bd_con, type = "message")
'
  shim <- gsub("___RPROFILE___", rprofile, tpl, fixed = TRUE)
  shim <- gsub("___MARKER___", mpath, shim, fixed = TRUE)
  shim <- gsub("___APPLOG___", apath, shim, fixed = TRUE)
  writeLines(shim, shim_path, useBytes = TRUE)
  invisible(shim_path)
}

# parse marker.log → liste par pid (t en secondes depuis minuit local)
.bd_t2s <- function(x) {
  as.numeric(strptime(paste(Sys.Date(), x), "%Y-%m-%d %H:%M:%OS"))
}
.bd_parse_markers <- function(marker_path) {
  txt <- tryCatch(readLines(marker_path, warn = FALSE), error = function(e) character(0))
  txt <- txt[grepl("^MARK ", txt)]
  if (!length(txt)) return(data.frame(pid = integer(0), phase = character(0),
                                      t = numeric(0), renv_ok = character(0)))
  parts <- strsplit(sub("^MARK ", "", txt), " ", fixed = TRUE)
  # format : <t> pid=<n> phase=<x> [renv_ok=<y>] — le 1er token EST l'horodatage
  getf <- function(ps, k) {
    hit <- grep(paste0("^", k, "="), ps, value = TRUE)
    if (length(hit)) sub(paste0("^", k, "="), "", hit[1]) else ""
  }
  df <- do.call(rbind, lapply(parts, function(ps) data.frame(
    pid = suppressWarnings(as.integer(getf(ps, "pid"))),
    phase = getf(ps, "phase"),
    t = suppressWarnings(.bd_t2s(ps[1])),
    renv_ok = getf(ps, "renv_ok"),
    stringsAsFactors = FALSE)))
  df[!is.na(df$pid) & !is.na(df$t), , drop = FALSE]
}

# timeline sonde : premier cycle d'un code http donné (externe, kill-proof).
# MESURÉ (boot 2026-11-05 17:50) : la signature F2 réelle = connexions
# REFUSÉES (http=0) ~177 s après « Listening », puis HTTP 500 (shiny répond
# en erreur), 200 n'étant JAMAIS atteint dans la fenêtre — d'où 200 STRICT
# pour le proxy de déblocage, et la trace séparée du premier 500.
.bd_first_http_code <- function(probe_txt, code) {
  cyc <- grep("^\\[[0-9:.]+\\] PROBE cyc=", probe_txt)
  if (!length(cyc)) return(NA_character_)
  ts <- sub("^\\[([0-9:.]+)\\].*$", "\\1", probe_txt[cyc])
  hs <- suppressWarnings(as.integer(sub(".* http=(NA|[0-9]+).*$", "\\1", probe_txt[cyc])))
  ok <- which(!is.na(hs) & hs == code)
  if (length(ok)) ts[ok[1]] else NA_character_
}
.bd_first_http200 <- function(probe_txt) .bd_first_http_code(probe_txt, 200L)

# ── mode enfant : la sonde concourante (ne jamais invoquer à la main) ────────
# BORNÉE : max_cycles cycles puis auto-stop (le spawn wait=FALSE peut rendre
# pid=0 — un orphelin possible devient impossible : la sonde se tue seule).
.bd_probe <- function(applog, probelog, max_cycles) {
  port <- NA_integer_
  server_ran <- FALSE
  ready_seen <- FALSE
  ready_mtime <- ""
  cycle <- 0L
  ready_path <- file.path(.bd_root(), "tools", "_drive", "ready.json")
  repeat {
    cycle <- cycle + 1L
    if (cycle > max_cycles) {
      .bd_log(probelog, "PROBE self_stop max_cycles=", max_cycles)
      quit(save = "no", status = 0L)
    }
    txt <- .bd_read_all(applog)
    if (is.na(port) && nzchar(txt)) {
      m <- regmatches(txt, regexpr("127\\.0\\.0\\.1:[0-9]+", txt, useBytes = TRUE))
      if (length(m) > 0L) {
        port <- suppressWarnings(as.integer(sub(".*:", "", m[1])))
        if (!is.na(port)) .bd_log(probelog, "PROBE listening port=", port)
      }
    }
    if (!server_ran && nzchar(txt) &&
        grepl("session_token=", txt, fixed = TRUE, useBytes = TRUE)) {
      server_ran <- TRUE
      .bd_log(probelog, "PROBE server_ran (session_token= vu)")
    }
    if (nzchar(txt) &&
        grepl("ready.json write FAILED", txt, fixed = TRUE, useBytes = TRUE)) {
      .bd_log(probelog, "PROBE ready_write_FAILED (message app vu)")
    }
    http <- .bd_http_get(port)
    rd <- file.exists(ready_path)
    if (rd && !ready_seen) .bd_log(probelog, "PROBE ready.json APParu")
    if (rd) {
      ready_seen <- TRUE
      ready_mtime <- format(file.mtime(ready_path), "%H:%M:%OS3")
    }
    pop <- .bd_r_population()
    line <- sprintf("PROBE cyc=%d http=%s ready=%d%s rscript_n=%d rscript_mb=%d",
                    cycle, ifelse(is.na(http), "NA", as.character(http)),
                    as.integer(rd),
                    if (nzchar(ready_mtime)) paste0(" mtime=", ready_mtime) else "",
                    pop$n, pop$total_mb)
    if (cycle %% 5L == 1L) {
      ram <- .bd_free_ram_mb()
      line <- paste0(line, sprintf(" ram_free_mb=%s",
                  if (is.na(ram)) "NA" else as.character(round(ram))))
    }
    .bd_log(probelog, line)
    Sys.sleep(1)
  }
}

# ── mode wrapper : observe le boot complet et résume ─────────────────────────
.bd_observe <- function(max_secs, app_args, applog = NA_character_) {
  root <- .bd_root()
  stamp <- format(Sys.time(), "%Y%m%dT%H%M%S")
  dir <- file.path(root, "QC", "boot_diagnose", stamp)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  applog <- file.path(dir, "app.log")
  probelog <- file.path(dir, "probe.log")
  summary_path <- file.path(dir, "summary.txt")
  marker_path <- file.path(dir, "marker.log")
  shim_path <- file.path(dir, "profile_shim.R")
  applog_r <- file.path(dir, "app_r.log") # log primaire : tenu PAR l'app (sink), lisible pendant le run, kill-proof
  file.create(applog, probelog, summary_path, marker_path, applog_r, showWarnings = FALSE)
  .bd_make_shim(shim_path, marker_path, applog_r, root)

  lc <- Sys.getenv("LC_ALL", "")
  if (!identical(lc, "French_France.65001")) {
    cat("[boot_diagnose] REFUS : LC_ALL='", lc,
        "' — la recette exige LC_ALL=French_France.65001 (Git Bash : ",
        "LC_ALL=French_France.65001 Rscript tools/boot_diagnose.R --observe)\n",
        sep = "")
    quit(save = "no", status = 2L)
  }
  pre <- .bd_r_population()
  pre_launch <- .bd_rscript_pids_matching("launch_dev_drive")
  pre_probe <- .bd_rscript_pids_matching("boot_diagnose.R --probe")
  ram <- .bd_free_ram_mb()
  .bd_log(summary_path, "boot_diagnose ", stamp)
  .bd_log(summary_path, "pre_existing rscript_n=", pre$n,
          " rscript_mb=", pre$total_mb, " ram_free_mb=", round(ram),
          " pre_launch_pids=", paste(pre_launch$pid, collapse = ","),
          " pre_probe_pids=", paste(pre_probe$pid, collapse = ","))
  cat(sprintf("[boot_diagnose] logs : %s\n", dir))

  # 1) sonde concourante (enfant ; lit APPLOG + ready.json, n'écrit que SON log)
  # ⚠️ MESURES system2 (2026-11-04) : (a) le chemin de redirection stdout= ne
  # supporte PAS les espaces (le dépôt en contient) ; (b) la sortie d'un enfant
  # long reste bufferisée côté fichier. D'où le design retenu : la redirection
  # de l'app est faite au niveau BASH par l'appelant (sortie de l'app ET du
  # wrapper dans le même fichier, capté par --applog) ; le wrapper ne redirige
  # RIEN (l'enfant hérite stdout/stderr du wrapper = le fichier bash) et la
  # sonde lit --applog.
  if (is.na(applog) || !nzchar(applog)) {
    cat("[boot_diagnose] --applog est requis (chemin du log bash-level)
")
    quit(save = "no", status = 2L)
  }
  probe_pid <- suppressWarnings(system2(
    .bd_rscript(),
    c(shQuote(file.path(root, "tools", "boot_diagnose.R")),
      "--probe", shQuote(applog_r), shQuote(probelog),
      as.character(max_secs + 10L)),
    wait = FALSE))
  .bd_log(summary_path, "probe_spawned pid=", probe_pid)

  # 2) l'app : l'UNIQUE boot supporté, VERBATIM — capture du code de sortie.
  # AUCUNE redirection ici : l'enfant hérite du wrapper (le fichier --applog).
  # R_PROFILE_USER = shim marqueur (hérité par l'app ET ses workers PSOCK
  # éventuels) ; dé-posé après le retour pour ne pas polluer janitor/probe.
  Sys.setenv(R_PROFILE_USER = shim_path)
  t0 <- Sys.time()
  res <- suppressWarnings(tryCatch(
    system2(.bd_rscript(),
            c(shQuote(file.path(root, "tools", "launch_dev_drive.R")), app_args),
            timeout = max_secs),
    error = function(e) structure("", status = -1L)))
  Sys.unsetenv("R_PROFILE_USER")
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  raw_code <- if (is.null(attr(res, "status"))) 0L else as.integer(attr(res, "status"))
  timed_out <- identical(raw_code, 124L)

  # 3) FIABILITÉ DU STATUT DE FIN (MESURÉ 2026-11-05 ×2) : le mécanisme
  #    timeout de system2 sur cet hôte rend un code 0 à max_secs SANS tuer
  #    l'arbre de façon déterministe, et le flush final du stdout (y compris
  #    la ligne « Listening » bufferisée) n'atterrit dans applog qu'à la
  #    MORT RÉELLE du process. Le seul fait fiable = le sondage CIM des pids
  #    launch_dev_drive NOUVEAUX (snapshot pré-boot : l'app opérateur, si
  #    elle existe, n'est JAMAIS visée). Séquence : tuer possédé si vivant →
  #    attendre la mort → laisser le flush atterrir → LIRE les logs.
  kill_note <- ""
  Sys.sleep(1)
  nowp <- .bd_rscript_pids_matching("launch_dev_drive")
  mine <- nowp[!(nowp$pid %in% pre_launch$pid), , drop = FALSE]
  app_was_alive <- nrow(mine) > 0L
  if (app_was_alive) {
    sel <- !(mine$ppid %in% mine$pid)
    top <- if (any(sel)) mine[sel, , drop = FALSE] else mine
    for (p in top$pid) {
      tk <- suppressWarnings(tryCatch(
        system2("taskkill", c("/PID", as.character(p), "/T", "/F"),
                stdout = TRUE, stderr = TRUE),
        error = function(e) "erreur taskkill"))
      kill_note <- paste0(kill_note, " taskkill(", p, ")=",
                          if (any(grepl("ERROR|erreur", tk, ignore.case = TRUE))) "FAIL" else "OK")
    }
    for (i in 1:10) {
      Sys.sleep(1)
      left <- .bd_rscript_pids_matching("launch_dev_drive")
      if (all(left$pid %in% pre_launch$pid)) break
    }
  }
  end_mode <- if (app_was_alive) "owned_kill" else
    if (timed_out) "timeout_124" else
    if (elapsed >= max_secs - 2) "ended_at_limit(host_timeout|self_exit)" else "self_exit"
  .bd_log(summary_path, "app_ended elapsed_s=", round(elapsed, 1),
          " exit_code=", as.character(raw_code), " end_mode=", end_mode, kill_note)
  Sys.sleep(2) # le flush final du stdout atterrit à la mort du process

  # 4) stop de la sonde — kill par ligne de commande (le pid de wait=FALSE
  #    peut être 0) ; snapshot pré-boot = jamais un process étranger.
  for (p in .bd_rscript_pids_matching("boot_diagnose.R --probe")$pid) {
    if (!(p %in% pre_probe$pid)) {
      suppressWarnings(tryCatch(
        system2("taskkill", c("/PID", as.character(p), "/F"),
                stdout = TRUE, stderr = TRUE), error = function(e) NULL))
    }
  }
  Sys.sleep(1)

  # 5) résumé (logs + janitor lecture seule) — verdicts DÉ-BLINDÉS :
  #    app_r.log = capture sink de l'APP elle-même (lisible pendant le run,
  #    écritures traversantes, kill-proof — MESURÉ 2026-11-05) ; applog bash
  #    reste une trace console brute (partage exclusif = illisible au run).
  app_txt <- .bd_read_all(applog_r)
  probe_txt <- tryCatch(readLines(probelog, warn = FALSE), error = function(e) character(0))
  listened_applog <- grepl("Listening on", app_txt, fixed = TRUE, useBytes = TRUE)
  port <- NA_integer_
  m <- regmatches(app_txt, regexpr("127\\.0\\.0\\.1:[0-9]+", app_txt, useBytes = TRUE))
  if (length(m) > 0L) port <- suppressWarnings(as.integer(sub(".*:", "", m[1])))
  first200 <- .bd_first_http200(probe_txt)
  listening <- listened_applog || !is.na(first200)
  listening_ev <- if (listened_applog) "applog" else
    if (!is.na(first200)) "probe_http" else "none"
  server_ran <- grepl("session_token=", app_txt, fixed = TRUE, useBytes = TRUE)
  write_failed <- grepl("ready.json write FAILED", app_txt, fixed = TRUE, useBytes = TRUE)
  ready_seen <- any(grepl("ready.json APParu", probe_txt, fixed = TRUE))
  server_ev <- if (ready_seen) "ready.json" else
    if (server_ran) "applog" else "none"
  hs <- regmatches(probe_txt, regexpr("http=[0-9]+", probe_txt))
  http_max <- if (length(hs)) max(suppressWarnings(as.integer(sub("http=", "", hs))), na.rm = TRUE) else 0L
  # stats profils (marker.log — non bufferisé, kill-proof)
  mk <- .bd_parse_markers(marker_path)
  n_pids <- length(unique(mk$pid))
  starts <- tapply(mk$t, mk$pid, min)
  dones <- tapply(mk$t[mk$phase == "profile_done"],
                  mk$pid[mk$phase == "profile_done"], min)
  common <- intersect(names(dones), names(starts))
  renv_s <- if (length(common)) dones[common] - starts[common] else numeric(0)
  main_pid <- if (n_pids > 0L) names(which.min(starts)) else NA_character_
  main_renv_s <- if (!is.na(main_pid) && main_pid %in% common) {
    round(as.numeric(renv_s[main_pid]), 1) } else NA_real_
  worker_renv_s <- if (length(renv_s) > 1L || (length(renv_s) == 1L && n_pids > 1L)) {
    w <- renv_s[setdiff(names(renv_s), main_pid)]
    if (length(w)) paste(sprintf("%s=%.1f", names(w), as.numeric(w)), collapse = ",")
    else NA_character_
  } else NA_character_
  # stall proxy : t_listening → t_first_http_200 STRICT (sonde 1 s) ; le
  # premier 500 est tracé séparément (signature F2 : 500 SANS jamais 200).
  # Ancre t_listening = la ligne « PROBE listening port= » de la SONDE
  # (horodatée, kill-proof) — applog ne porte PAS d'horodatage (lignes brutes
  # de l'app) ; sans cette ancre, stall_proxy est honnêtement NA.
  gap <- function(t) {
    li_line <- grep("PROBE listening port=", probe_txt, value = TRUE)
    if (!length(li_line) || is.na(t)) return(NA_real_)
    lst_s <- suppressWarnings(.bd_t2s(sub("^\\[([0-9:.]+)\\].*$", "\\1", li_line[1])))
    t_s <- suppressWarnings(.bd_t2s(t))
    if (is.na(lst_s) || is.na(t_s)) NA_real_ else round(max(0, t_s - lst_s), 1)
  }
  stall_proxy <- gap(first200)
  first500 <- .bd_first_http_code(probe_txt, 500L)
  http500_gap <- gap(first500)
  jan <- suppressWarnings(tryCatch(
    system2(.bd_rscript(),
            c("--no-init-file", shQuote(file.path(root, "tools", "mirai_janitor.R")),
              "--list"),
            stdout = TRUE, stderr = FALSE), error = function(e) character(0)))
  leaks <- sum(grepl("— LEAK|LEAK$", jan))
  .bd_log(summary_path, "verdict listening=", ifelse(listening, "yes", "no"),
          " evidence=", listening_ev, " port=", port,
          " server_ran_evidence=", server_ev,
          " ready_write_failed=", ifelse(write_failed, "yes", "no"),
          " ready_json_seen=", ifelse(ready_seen, "yes", "no"),
          " http_max=", http_max, " stall_proxy_s=", stall_proxy,
          " http500_after_listening_s=", http500_gap,
          " profile_pids=", n_pids, " main_renv_s=", main_renv_s,
          " worker_renv_s=", ifelse(is.na(worker_renv_s), "NA", worker_renv_s),
          " mirai_leaks=", leaks)
  cat("DIAG: exit_code=", as.character(raw_code), " end_mode=", end_mode,
      "\n", sep = "")
  cat("DIAG: listening=", ifelse(listening, "yes", "no"),
      " evidence=", listening_ev, " port=", port, "\n", sep = "")
  cat("DIAG: server_ran=", ifelse(server_ev != "none", "yes", "no"),
      " evidence=", server_ev, "\n", sep = "")
  cat("DIAG: ready_json=", ifelse(ready_seen, "yes", "no"),
      " ready_write_failed=", ifelse(write_failed, "yes", "no"), "\n", sep = "")
  cat("DIAG: http_ok=", ifelse(http_max == 200L, "yes", "no"),
      " http_max=", http_max, "\n", sep = "")
  cat("DIAG: stall_proxy_s=", ifelse(is.na(stall_proxy), "NA", as.character(stall_proxy)),
      " http500_after_listening_s=",
      ifelse(is.na(http500_gap), "NA", as.character(http500_gap)), "\n", sep = "")
  cat("DIAG: profiles=", n_pids,
      " main_renv_s=", ifelse(is.na(main_renv_s), "NA", as.character(main_renv_s)),
      " worker_renv_s=", ifelse(is.na(worker_renv_s), "NA", worker_renv_s),
      "\n", sep = "")
  cat("DIAG: leaks=", leaks, "\n", sep = "")
  cat(sprintf("[boot_diagnose] logs : %s\n", dir))
  invisible(0L)
}

# ── CLI ──────────────────────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 3L && args[1] == "--probe") {
  max_cycles <- if (length(args) >= 4L) max(1L, suppressWarnings(as.integer(args[4]))) else 3600L
  if (is.na(max_cycles)) max_cycles <- 3600L
  .bd_probe(args[2], args[3], max_cycles)
  quit(save = "no", status = 0L)
}

usage <- function() {
  cat(paste0("Usage : LC_ALL=French_France.65001 Rscript tools/boot_diagnose.R ",
             "--observe [--max-secs N] [--app-args \"--headless\"]\n",
             "  SANS verbe : usage seul, aucune action (discipline run_dock.R).\n",
             "  Au délai --max-secs : kill possédé de l'arbre du seul process app lancé.\n"))
}
if (length(args) == 0L || !("--observe" %in% args)) {
  usage()
  quit(save = "no", status = 2L)
}
max_secs <- 240L
app_args <- character(0)
i <- match("--max-secs", args)
if (!is.na(i)) {
  if (i == length(args)) { cat("[boot_diagnose] --max-secs attend une valeur\n")
    quit(save = "no", status = 2L) }
  v <- suppressWarnings(as.integer(args[i + 1]))
  if (is.na(v) || v <= 0L) { cat("[boot_diagnose] --max-secs attend un entier > 0\n")
    quit(save = "no", status = 2L) }
  max_secs <- v
}
i <- match("--app-args", args)
if (!is.na(i)) {
  if (i == length(args)) { cat("[boot_diagnose] --app-args attend une valeur\n")
    quit(save = "no", status = 2L) }
  app_args <- strsplit(args[i + 1], "\\s+")[[1]]
}
i <- match("--applog", args)
if (!is.na(i)) {
  if (i == length(args)) { cat("[boot_diagnose] --applog attend une valeur
")
    quit(save = "no", status = 2L) }
  applog <- normalizePath(args[i + 1], winslash = "/", mustWork = FALSE)
}
quit(save = "no", status = .bd_observe(max_secs, app_args, applog))

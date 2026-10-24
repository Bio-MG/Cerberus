#!/usr/bin/env Rscript
# =============================================================================
# tools/run_all_gates.R — META-GATE : les gardes du projet en une commande
#                          TranscriptoShiny ("Cerberus")
# =============================================================================
# POURQUOI CE FICHIER EXISTE (mesure 2026-10-24)
# ----------------------------------------------
# Les six gardes vivaient disperses : chacun a son script, son drapeau, son
# format de resume et son code de sortie. Rien ne les ordonnait, donc « lancer
# les gardes » etait un RITUEL DE MEMOIRE : six commandes, dans un ordre que
# personne ne verifiait. Un garde oublie n'est pas un garde : c'est un
# avertissement qu'on ne lit pas.
#
# ORIGINE — CE FICHIER EST UNE RECONSTRUCTION
# -------------------------------------------
# L'original (lot 6, session 2026-10-12) n'a JAMAIS ete commite et a disparu de
# l'arbre ; il n'existe dans aucun objet git (mesure : `git rev-list --all
# --objects`, `git fsck`, checkpoints, packs, corbeilles — rien). Voir HANDOFF
# §« OUTILS PERDUS ». Ce fichier est reconstruit depuis trois sources, et la
# troisieme fait foi :
#   1. la description du HANDOFF (CLI : --only/--quiet/--json/--timeout ;
#      sortie VERT/ROUGE/ABSENT ; exit 0/1 ; registre fige) ;
#   2. le compte d'assertions de son test (73) ;
#   3. `docs/audits/gates_history/latest.json` (2026-10-12), qui contient le
#      REGISTRE MESURE : 6 ids, dans l'ordre, avec leurs durees et le format
#      exact du champ `details` (`err=0 warn=1`, `desaccord=0`).
# Le champ `details` produit ici est IDENTIQUE a celui de latest.json, ce qui
# est la seule validation disponible du format. Les gardes `mcp` et `docs_hash`
# portaient `details: ""` : c'est reproduit.
#
# ⚠️ SCHEMA : `latest.json` porte DEUX cles de plus (`status_claims_exit`,
# `json_parse_ok`) que ce script n'emet pas et ne doit pas emettre — elles
# decrivent la VERIFICATION de la sortie (relance ciblee de status_claims,
# validite du JSON), pas la sortie elle-meme. Elles appartiennent a
# `tools/scheduled_verify.R`, qui enveloppe ce script. Un run de
# `run_all_gates.R` seul produit donc un JSON VALIDE mais plus court.
#
# ⚠️ LE GARDE `mcp` EST UNE HYPOTHESE DECLAREE. Il n'y a aucune trace de ce
# qu'il executait (0,8 s, `details: ""`). On lui fait VERIFIER QUE LE SERVEUR
# MCP PARSE : un serveur qui ne parse pas est un agent sans interface, et 0,8 s
# est le temps d'un parse. Marque `reconstructed = TRUE` dans le registre pour
# qu'aucun lecteur ne le confonde avec l'original.
#   Indice en faveur du parse : 0,8 s est le cout d'un PROCESSUS R, pas d'un
#   `parse()` en memoire (mesure ici : 0,0 s). L'original lancait donc
#   probablement un sous-processus pour le meme travail. On garde le parse EN
#   PROCESSUS : le resultat est identique et 0,8 s de moins. L'ecart de duree
#   entre latest.json et un run neuf est donc ATTENDU sur cette ligne seule.
#
# USAGE
#   Rscript tools/run_all_gates.R [--only=id1,id2] [--quiet] [--json]
#                                 [--timeout=SECONDES] [--root=DIR]
#
#   --only=…       : ne lance que les gardes nommes (ids du registre).
#   --quiet        : pas de table, seulement la ligne de verdict final.
#   --json         : sortie JSON (schema de gates_history/latest.json).
#   --timeout=SEC  : au-dela, le garde est ROUGE « timeout » (defaut : 900).
#   --allow-absent : un ABSENT ne fait plus echouer la melee (il reste affiche
#                    ABSENT : on tolere, on n'efface pas).
#   --root=DIR     : racine du projet (defaut : deduite du chemin du script).
#                    Existe pour les tests, qui lancent des FAUX gardes.
#
# CODES DE SORTIE
#   0 = tous VERT
#   1 = au moins un ROUGE ou un ABSENT
#   2 = erreur d'usage (id inconnu, --json sans jsonlite, racine invalide)
#
# 🔴 ABSENT N'EST PAS VERT. Un garde introuvable est une CAPACITE PERDUE, pas un
# succes silencieux : c'est exactement ainsi que les six outils du lot 6-7 ont
# disparu sans que rien ne rougisse (2026-10-24). Un ABSENT fait donc echouer la
# melee comme un ROUGE. `--allow-absent` existe pour les cas assumes (clone sans
# les outils locaux), et il est EXPLICITE dans la sortie quand il sert.
#
# ⚠️ LE TIMEOUT EST UN VERDICT, PAS UNE MISE A MORT. Base R ne sait pas tuer un
# processus enfant (`system2()` n'a pas de `timeout=`). Un garde qui se bloque
# bloque donc le meta-gate ; ce qu'on garantit, c'est qu'il ne sera pas classe
# VERT : le depassement est mesure APRES le retour et rend ROUGE. Le dire est
# plus honnete que de pretendre interrompre.
#
# ⚠️ CHEMINS AVEC ESPACES. La racine du projet contient « SHINYAPP test (git
# work) » : les ARGUMENTS passent par `shQuote()`. Un `system2()` non quote rend
# 5 en silence (lecon du lot 7, perdue avec le reste, retrouvee ici).
#
# 🔴 MAIS PAS LA COMMANDE. `system2()` quote DEJA son premier argument sur
# Windows ; le re-quoter produit un nom de fichier litteral et un echec
# « not found ». Mesure du 2026-10-24 :
#   system2(shQuote(exe), args = shQuote(p))
#   -> Error: '"\"D:/Data_science/R-4.4.2/bin/x64/Rscript.exe\""' not found
# La bonne forme est `system2(exe, args = shQuote(p))`. La lecon « tout passer
# par shQuote() » est donc VRAIE pour `args` et FAUSSE pour `command`.
# =============================================================================

# ── Registre FIGE (donnee, pas code) ----------------------------------------
# L'ordre est celui de `gates_history/latest.json` : le meta-gate doit produire
# les memes lignes dans le meme ordre, sinon deux runs ne se comparent pas.
ts_gates_registry <- function() {
  list(
    list(id = "conventions",   script = "tools/check_conventions.R",      kind = "exit"),
    list(id = "duplication",   script = "tools/check_duplication.R",      kind = "exit"),
    list(id = "hermeticite",   script = "tools/check_renv_hermeticity.R", kind = "exit"),
    list(id = "mcp",           script = "scripts/mcp_server.R",           kind = "parse",
         reconstructed = TRUE),
    list(id = "status_claims", script = "tools/check_status_claims.R",    kind = "exit"),
    list(id = "docs_hash",     script = "tools/docs_hash_manifest.R",     kind = "exit"),
    # GARDE 7, AJOUTE le 2026-10-24 (marque `added`, pas `reconstructed` : il
    # n'a pas d'original perdu, il n'existait simplement pas). Il repond a la
    # seule question que la perte du lot 6-7 laissait sans reponse : « ce qui
    # vit dans tools/ est-il vraiment dans le depot ? ». Les six outils perdus
    # auraient tous ete nommes par ce garde le jour meme.
    # Il est AJOUTE EN DERNIER pour que les six ids historiques gardent leur
    # rang : un `latest.json` d'avant reste comparable ligne a ligne.
    list(id = "tools_tracked", script = "tools/check_tools_tracked.R",    kind = "exit",
         added = TRUE)
  )
}

ts_gates_ids <- function() {
  vapply(ts_gates_registry(), function(g) g$id, character(1))
}

# ── Racine du projet : deduite du script, jamais du repertoire courant ------
# Un `setwd()` oublie ferait lancer les gardes depuis n'importe ou ; la racine
# doit etre une PROPRIETE du fichier, pas de l'invocation.
ts_gates_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", args, value = TRUE)
  if (!length(hit)) return(getwd())
  script <- normalizePath(sub("^--file=", "", hit[1]), mustWork = FALSE)
  normalizePath(file.path(dirname(script), ".."), mustWork = FALSE)
}

# ── Executable R de CE processus -------------------------------------------
# `Rscript` de la meme installation que le R courant : prendre `Rscript` du PATH
# pourrait lancer une AUTRE version, donc un AUTRE renv.
ts_gates_rscript <- function() {
  exe <- if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  file.path(R.home("bin"), exe)
}

# ── Extraction de `details` depuis la sortie du garde -----------------------
# Les gardes n'ont pas de format commun : deux ecrivent « Resume », un
# « Résumé », un autre « desaccord ». On ne s'accroche donc PAS au titre mais
# aux NOMBRES, et l'accent devient sans importance.
ts_gate_details <- function(output, id) {
  txt <- paste(output, collapse = "\n")
  grab <- function(pat) {
    m <- regmatches(txt, regexpr(pat, txt, perl = TRUE))
    if (!length(m)) return(NA_integer_)
    suppressWarnings(as.integer(sub(pat, "\\1", m, perl = TRUE)))
  }
  # `check_status_claims.R` ne compte pas des erreurs : il compte des DESACCORDS.
  if (identical(id, "status_claims")) {
    d <- grab("(\\d+)\\s*desaccord")
    return(if (is.na(d)) "" else sprintf("desaccord=%d", d))
  }
  e <- grab("(\\d+)\\s*erreur\\(s\\)")
  w <- grab("(\\d+)\\s*avertissement")
  parts <- character(0)
  if (!is.na(e)) parts <- c(parts, sprintf("err=%d", e))
  if (!is.na(w)) parts <- c(parts, sprintf("warn=%d", w))
  paste(parts, collapse = " ")
}

# ── Lancement d'UN garde ----------------------------------------------------
# Trois verdicts seulement, et pas quatre : VERT (exit 0), ROUGE (exit != 0,
# timeout, ou parse casse), ABSENT (le script n'existe pas).
ts_gate_run <- function(root, gate, timeout_s) {
  path <- file.path(root, gate$script)
  if (!file.exists(path)) {
    return(list(id = gate$id, verdict = "ABSENT", exit = NA_integer_,
                seconds = 0, details = "", output = character(0)))
  }
  t0 <- Sys.time()
  if (identical(gate$kind, "parse")) {
    # Parse EN PROCESSUS : un `-e` devrait re-quoter un chemin qui contient des
    # espaces et des parentheses, pour un resultat identique. La syntaxe est la
    # meme dans les deux cas.
    res <- tryCatch({ parse(path, keep.source = FALSE); NULL },
                    error = function(e) conditionMessage(e))
    secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    if (is.null(res)) {
      return(list(id = gate$id, verdict = "VERT", exit = 0L,
                  seconds = round(secs, 1), details = "", output = character(0)))
    }
    return(list(id = gate$id, verdict = "ROUGE", exit = 1L,
                seconds = round(secs, 1), details = "parse_error",
                output = res))
  }
  # `shQuote()` sur le SCRIPT seulement : `system2()` quote deja `command` sur
  # Windows (voir l'avertissement en tete). Re-quoter l'executable echoue.
  launch_err <- NULL
  out <- suppressWarnings(tryCatch(
    system2(ts_gates_rscript(), args = shQuote(path),
            stdout = TRUE, stderr = TRUE),
    error = function(e) { launch_err <<- conditionMessage(e); character(0) }))
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  # Un echec de LANCEMENT n'est pas un code de sortie du garde : le distinguer,
  # sinon un `127` de `system()` se confond avec un garde qui aurait rendu 127.
  if (!is.null(launch_err)) {
    return(list(id = gate$id, verdict = "ROUGE", exit = NA_integer_,
                seconds = round(secs, 1), details = "launch_error",
                output = launch_err))
  }
  verdict <- if (identical(as.integer(status), 0L)) "VERT" else "ROUGE"
  details <- ts_gate_details(out, gate$id)
  if (secs > timeout_s) {
    verdict <- "ROUGE"
    details <- paste0(details, if (nzchar(details)) " " else "", "timeout")
  }
  list(id = gate$id, verdict = verdict, exit = as.integer(status),
       seconds = round(secs, 1), details = details, output = out)
}

# ── Compteur de fichiers de test (identite de l'arbre) ---------------------
# `latest.json` porte `tree_identity.census_total` : deux runs ne se comparent
# que si l'arbre est comparable. Lu dans le census, jamais recompte ici (le
# census a UN generateur : tools/build_census.R).
ts_gates_census <- function(root) {
  p <- file.path(root, "docs", "audits", "census.txt")
  if (!file.exists(p)) return(list(total = NA_integer_, source = NA_character_))
  ln <- readLines(p, warn = FALSE)
  n <- sum(grepl("^CENSUS:\\s+test-", ln))
  list(total = as.integer(n), source = p)
}

# ── Arguments ---------------------------------------------------------------
ts_gates_args <- function(argv) {
  opt <- list(only = NULL, quiet = FALSE, json = FALSE, timeout = 900,
              root = NULL, allow_absent = FALSE)
  for (a in argv) {
    if (identical(a, "--quiet")) { opt$quiet <- TRUE; next }
    if (identical(a, "--json"))  { opt$json <- TRUE;  next }
    if (identical(a, "--allow-absent")) { opt$allow_absent <- TRUE; next }
    if (grepl("^--only=", a)) {
      opt$only <- trimws(strsplit(sub("^--only=", "", a), ",")[[1]])
      opt$only <- opt$only[nzchar(opt$only)]
      next
    }
    if (grepl("^--timeout=", a)) {
      v <- suppressWarnings(as.numeric(sub("^--timeout=", "", a)))
      if (is.na(v) || v <= 0) stop("usage: --timeout attend un nombre > 0", call. = FALSE)
      opt$timeout <- v
      next
    }
    if (grepl("^--root=", a)) { opt$root <- sub("^--root=", "", a); next }
    stop(sprintf("usage: argument inconnu '%s'", a), call. = FALSE)
  }
  opt
}

# ── Table texte -------------------------------------------------------------
ts_gates_print <- function(res, quiet) {
  if (isTRUE(quiet)) return(invisible(NULL))
  w <- max(nchar(vapply(res, function(r) r$id, character(1))), 5L)
  cat(sprintf("%-*s  %-7s  %-4s  %6s  %s\n", w, "GARDE", "VERDICT", "exit",
              "s", "details"))
  for (r in res) {
    cat(sprintf("%-*s  %-7s  %-4s  %6.1f  %s\n", w, r$id, r$verdict,
                if (is.na(r$exit)) "-" else as.character(r$exit),
                r$seconds, r$details))
  }
  invisible(NULL)
}

ts_gates_verdict <- function(res) {
  if (any(vapply(res, function(r) r$verdict != "VERT", logical(1)))) "ROUGE" else "VERT"
}

# ── Main --------------------------------------------------------------------
# ⚠️ REND LE CODE DE SORTIE **INVISIBLEMENT**. Mesure 2026-10-24 : appelee sans
# affectation (`capture.output(ts_gates_main(...))`), la fonction imprimait
# `[1] 0` APRES le JSON, ce qui rendait la sortie machine illisible
# (« trailing garbage »). Une fonction dont le metier est d'ecrire une sortie
# machine ne doit pas y ajouter sa propre valeur de retour. `quit(status =
# invisible(x))` fonctionne a l'identique.
ts_gates_main <- function(argv = commandArgs(trailingOnly = TRUE)) {
  opt <- tryCatch(ts_gates_args(argv), error = function(e) e)
  if (inherits(opt, "condition")) {
    message("ERREUR: ", conditionMessage(opt))
    return(invisible(2L))
  }
  root <- if (is.null(opt$root)) ts_gates_root() else normalizePath(opt$root, mustWork = FALSE)
  if (!dir.exists(root)) {
    message("ERREUR: racine introuvable : ", root)
    return(invisible(2L))
  }
  reg <- ts_gates_registry()
  if (!is.null(opt$only)) {
    unknown <- setdiff(opt$only, ts_gates_ids())
    if (length(unknown)) {
      message("ERREUR: garde(s) inconnu(s) : ", paste(unknown, collapse = ", "),
              " (connus : ", paste(ts_gates_ids(), collapse = ", "), ")")
      return(invisible(2L))
    }
    reg <- reg[vapply(reg, function(g) g$id %in% opt$only, logical(1))]
  }
  # 🔴 ZERO GARDE N'EST PAS UN SUCCES. `any(logical(0))` vaut FALSE, donc une
  # selection vide rendrait « VERDICT: VERT (0 garde(s)) » : le mensonge vert
  # exact que ce fichier existe pour empecher. Une selection qui ne retient rien
  # est une ERREUR D'USAGE.
  if (!length(reg)) {
    message("ERREUR: la selection ne retient aucun garde ",
            "(ids connus : ", paste(ts_gates_ids(), collapse = ", "), ")")
    return(invisible(2L))
  }
  if (isTRUE(opt$json) && !requireNamespace("jsonlite", quietly = TRUE)) {
    message("ERREUR: --json exige le paquet jsonlite")
    return(invisible(2L))
  }

  res <- lapply(reg, function(g) ts_gate_run(root, g, opt$timeout))
  census <- ts_gates_census(root)
  # Le verdict est calcule a partir des verdicts REELS. `--allow-absent` ne
  # reecrit PAS un ABSENT en VERT (ce serait effacer l'information au lieu de
  # la tolerer) : il change seulement la REGLE d'echec. La ligne reste ABSENT.
  has_red <- any(vapply(res, function(r) identical(r$verdict, "ROUGE"), logical(1)))
  has_absent <- any(vapply(res, function(r) identical(r$verdict, "ABSENT"), logical(1)))
  verdict <- if (has_red || (has_absent && !isTRUE(opt$allow_absent))) "ROUGE" else "VERT"

  # `--json` est une sortie MACHINE : il supprime la table, sinon stdout n'est
  # plus du JSON et le `json_parse_ok` de gates_history/latest.json ne pourrait
  # pas valoir `true`. (Choix explicite : `--json` implique `--quiet` pour la
  # table. L'original n'a pas ete retrouve, ce point est une decision.)
  ts_gates_print(res, opt$quiet || isTRUE(opt$json))
  if (isTRUE(opt$json)) {
    payload <- list(
      horodate_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      tree_identity = list(census_total = census$total,
                           census_source = census$source),
      gates = lapply(res, function(r) list(
        id = r$id, verdict = r$verdict, exit = r$exit,
        s = r$seconds, details = r$details)),
      gates_exit = if (identical(verdict, "VERT")) 0L else 1L,
      verdict = verdict)
    cat(jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE,
                         null = "null", na = "null"), "\n")
  } else {
    cat(sprintf("\nVERDICT: %s (%d garde(s), %s)\n", verdict, length(res),
                format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")))
  }
  if (identical(verdict, "VERT")) invisible(0L) else invisible(1L)
}

if (sys.nframe() == 0L) {
  quit(save = "no", status = ts_gates_main())
}

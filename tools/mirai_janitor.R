# =============================================================================
# tools/mirai_janitor.R — nettoyeur des workers mirai orphelins (F1, 2026-11-04)
# =============================================================================
# CONTEXTE (finding F1, ledger round8 §4 — MÉCANISME CORRIGÉ PAR MESURE le
# 2026-11-04, cf. runbook §7) :
#   Tuer le processus R de l'app d'un kill dur (TaskStop, Stop-Process -Force,
#   taskkill /F sans /T) ne laisse AUCUNE chance à `onStop(stop_spatial_daemons)`
#   (app.R, fin de server()) de s'exécuter : TerminateProcess détruit le process
#   par le noyau SANS exécuter de code utilisateur. Le nettoyage `mirai::daemons(0)`
#   n'a donc jamais lieu. MESURE IMPORTANTE (falsification ×3, 2026-11-04) : un
#   pool LOCAL `daemons(n)` de mirai 2.7.2 se nettoie LUI-MÊME au kill dur du host
#   (autoexit sur chute de socket — même pour des daemons à peine éclos) : la fuite
#   massive S6 (53 Rscript) n'est PAS reproductible dans ce mode. Les modes de fuite
#   RÉELS mesurés : (a) un daemon lancé manuellement `mirai::daemon(url, asyncdial=TRUE)`
#   vers un host mort RETENTE SA DIAL indéfiniment et persiste (orphan validé live :
#   survit 30+ s, seul le janitor le retire) ; (b) fenêtres de course (daemons
#   éclos pendant le kill) — non reproduites mais non exclues. Le janitor reste
#   LE filet de sécurité : il détecte TOUT Rscript de signature mirai orphelin
#   (chaîne de parents → PID mort) et ne tue QUE lui, preuve PPID imprimée.
#
# CE QUE CET OUTIL FAIT (et ne fait pas) :
#   - SANS verbe   : usage seul, AUCUNE action (même discipline que run_dock.R).
#   - --list       : liste les candidats mirai (Rscript dont la ligne de
#                    commande porte la signature mirai), classe LEAK /
#                    ATTACHED / PRESERVED par preuve de chaîne de parents,
#                    n'exécute AUCUN kill. Exit 0.
#   - --kill       : pour CHAQUE candidat LEAK, preuve imprimée AVANT le kill,
#                    puis taskkill /T /F UN PID À LA FOIS (racine de chaîne
#                    d'abord : l'arbre /T du dispatcher couvre ses daemons),
#                    re-mesure finale. Exit 0 si population résiduelle = 0,
#                    exit 1 sinon.
#   - --json       : comme --list, sortie JSON machine.
#
# DISCIPLINE DE KILL (docs/mcp/DRIVE_SESSION_RUNBOOK.md §3-B) :
#   - un PID à la fois, preuve PPID imprimée avant chaque kill ;
#   - NE JAMAIS toucher rsession.exe / R.exe (RStudio, process de l'opérateur),
#     ni un candidat ATTACHED (sa chaîne remonte à un process vivant) ;
#   - en cas de doute (preuve ambiguë) : classé PRESERVED et signalé, jamais tué.
#
# LIMITES HONNÊTES :
#   - la détection lit CommandLine via CIM (Get-CimInstance) ; un process lancé
#     avec une ligne de commande masquée ne serait pas vu (jamais mesuré ici) ;
#   - un PID parent réutilisé par un process sans rapport apparaît « vivant » :
#     le candidat est alors classé ATTACHED/PRESERVED (conservateur — jamais de
#     kill sur une preuve ambiguë) ;
#   - l'outil ne répare PAS la cause : seul un arrêt propre (onStop) ou un kill
#     de l'arbre complet évite la fuite ; note root-cause au runbook (docs/
#     local) et README outils.
# =============================================================================

.mj_signature <- function(cmdline) {
  # Signatures des processus de la famille « pool/plateforme » — MESURÉES le
  # 2026-11-04 sur pools vivants :
  #   (a) workers mirai LOCAUX (daemons(n)) ET workers PSOCK (future
  #       plan(multisession), parallelly) — les DEUX partagent le flag
  #       Rscript --default-packages=datasets,utils,grDevices,graphics,stats,methods
  #       (mesuré : les « daemons » d'un boot sain sont des PSOCK workRSOCK,
  #       pas du mirai — global.R:220 plan(multisession)) ;
  #   (b) workers/dispatchers mirai lancés manuellement :
  #       Rscript -e "mirai::daemon(...)"/"mirai::dispatcher(...)" ;
  #   (c) LE PROCESS APP LUI-MÊME (launch_dev_drive.R) — la population de
  #       fuite S6 (« ~6 Rscript par boot », pic 53) = l'app orpheline + ses
  #       pools, lorsque le kill a porté sur le shim/bash et non sur le
  #       process x64 réel (mesuré le 2026-11-04 : un taskkill de l'arbre du
  #       wrapper LAISSE l'app x64 vivante). Sans (c), les workers d'une app
  #       orpheline seraient classés ATTACHED (propriétaire vivant) et
  #       préservés à tort.
  # VECTORISÉE (utilisée comme filtre de lignes ET sur une ligne seule).
  res <- rep(FALSE, length(cmdline))
  ok <- !is.na(cmdline) & nzchar(cmdline)
  res[ok] <- grepl("--default-packages=datasets,utils,grDevices,graphics,stats,methods",
                   cmdline[ok], fixed = TRUE) |
             (grepl("mirai", cmdline[ok], fixed = TRUE) &
              grepl("daemon|dispatcher", cmdline[ok])) |
             grepl("launch_dev_drive.R", cmdline[ok], fixed = TRUE)
  res
}

.mj_processes <- function() {
  # Une seule requête CIM : pid, parent, nom, WS (Mo), cmdline — famille R
  # entière. Le "|" est exclu des champs (cmdline nettoyée).
  ps <- paste0(
    "[Console]::OutputEncoding=[Text.Encoding]::UTF8; ",
    "Get-CimInstance Win32_Process | Where-Object { $_.Name -match ",
    "'^(Rscript|R|rsession)\\.exe$' } | ForEach-Object { ",
    "'{0}|{1}|{2}|{3}|{4}' -f $_.ProcessId, $_.ParentProcessId, $_.Name, ",
    "[math]::Round($_.WorkingSetSize/1MB), ",
    "(($_.CommandLine -replace '\\|',' ') -replace \"`r|`n\",' ') }"
  )
  out <- suppressWarnings(tryCatch(
    system2("powershell", c("-NoProfile", "-Command", shQuote(ps)),
            stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)))
  out <- out[nzchar(trimws(out))]
  parts <- strsplit(out, "|", fixed = TRUE)
  parts <- parts[vapply(parts, function(p) length(p) >= 5L, logical(1))]
  if (length(parts) == 0L) return(data.frame(
    pid = integer(0), ppid = integer(0), name = character(0),
    ws_mb = numeric(0), cmdline = character(0), stringsAsFactors = FALSE))
  data.frame(
    pid     = as.integer(vapply(parts, `[`, character(1), 1)),
    ppid    = as.integer(vapply(parts, `[`, character(1), 2)),
    name    = vapply(parts, `[`, character(1), 3),
    ws_mb   = suppressWarnings(as.numeric(vapply(parts, `[`, character(1), 4))),
    cmdline = vapply(parts, function(p) paste(p[-(1:4)], collapse = "|"),
                     character(1)),
    stringsAsFactors = FALSE)
}

.mj_chain <- function(pid, procs, max_depth = 32L) {
  # Remonte depuis le candidat : chaque maillon mirai est enregistré ; le
  # premier parent ABSENT du système prouve la fuite (root_dead = TRUE, le pid
  # mort est rendu) ; le premier parent VIVANT non-mirai est le propriétaire
  # (ATTACHED). Plus de max_depth maillons : preuve ambiguë (NA, conservateur).
  steps <- character(0)
  cur <- as.integer(pid)
  for (i in seq_len(max_depth)) {
    idx <- match(cur, procs$pid)
    if (is.na(idx)) {
      return(list(steps = steps, root_dead = TRUE,
                  dead_ppid = cur, owner_pid = NA_integer_))
    }
    row <- procs[idx, ]
    if (!.mj_signature(row$cmdline) && length(steps) > 0L) {
      return(list(steps = steps, root_dead = FALSE, dead_ppid = NA_integer_,
                  owner_pid = row$pid))
    }
    steps <- c(steps, sprintf("%s pid=%d parent=%d ws=%.0fMo cmd=%.90s",
                              row$name, row$pid, row$ppid,
                              if (is.na(row$ws_mb)) 0 else row$ws_mb,
                              row$cmdline))
    cur <- row$ppid
  }
  list(steps = steps, root_dead = NA, dead_ppid = NA_integer_,
       owner_pid = NA_integer_)
}

.mj_classify <- function(procs) {
  cand <- procs[.mj_signature(procs$cmdline), , drop = FALSE]
  if (nrow(cand) == 0L) return(data.frame(
    pid = integer(0), ppid = integer(0), verdict = character(0),
    evidence = character(0), depth = integer(0), stringsAsFactors = FALSE))
  out <- do.call(rbind, lapply(seq_len(nrow(cand)), function(i) {
    ch <- .mj_chain(cand$pid[i], procs)
    verdict <- if (isTRUE(ch$root_dead)) "LEAK"
               else if (is.na(ch$root_dead)) "PRESERVED (preuve ambiguë)"
               else "ATTACHED"
    evidence <- if (isTRUE(ch$root_dead)) {
      paste0(c(ch$steps,
               sprintf("racine MORTE : parent pid %d absent du système",
                       ch$dead_ppid)), collapse = " <- ")
    } else if (!is.na(ch$owner_pid)) {
      paste0(c(ch$steps,
               sprintf("propriétaire VIVANT pid %d (%s)", ch$owner_pid,
                       procs$name[match(ch$owner_pid, procs$pid)])),
             collapse = " <- ")
    } else {
      paste(c(ch$steps, "chaîne non résolue (conservateur)"), collapse = " <- ")
    }
    data.frame(pid = cand$pid[i], ppid = cand$ppid[i], verdict = verdict,
               evidence = evidence, depth = length(ch$steps),
               stringsAsFactors = FALSE)
  }))
  out[order(out$pid), ]
}

.mj_kill_one <- function(pid) {
  out <- suppressWarnings(tryCatch(
    system2("taskkill", c("/PID", as.character(as.integer(pid)), "/T", "/F"),
            stdout = TRUE, stderr = TRUE),
    error = function(e) paste("ERREUR taskkill :", conditionMessage(e))))
  !any(grepl("ERROR|erreur", out, ignore.case = TRUE))
}

.mj_json_escape <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\"", "\\\\\"", x)
  x <- gsub("[\r\n]", " ", x)
  x
}

.mj_emit_json <- function(cls, procs) {
  # JSON minimal sans dépendance (le janitor doit tourner même hors renv).
  items <- vapply(seq_len(nrow(cls)), function(i) {
    ws <- procs$ws_mb[match(cls$pid[i], procs$pid)]
    sprintf('{"pid": %d, "verdict": "%s", "ws_mb": %s, "evidence": "%s"}',
            cls$pid[i], .mj_json_escape(cls$verdict[i]),
            if (is.na(ws)) "null" else sprintf("%.0f", ws),
            .mj_json_escape(cls$evidence[i]))
  }, character(1))
  leaks <- cls$pid[cls$verdict == "LEAK"]
  cat(sprintf('{\n  "timestamp": "%s",\n  "leaked": [%s],\n  "candidates": [%s]\n}\n',
              format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
              paste(if (length(leaks)) as.character(leaks) else "", collapse = ", "),
              paste(items, collapse = ",\n    ")))
}

.mj_run <- function(kill = FALSE, json = FALSE) {
  procs <- .mj_processes()
  cls <- .mj_classify(procs)
  leaks <- cls[cls$verdict == "LEAK", , drop = FALSE]
  others <- cls[cls$verdict != "LEAK", , drop = FALSE]

  if (json) {
    .mj_emit_json(cls, procs)
    return(invisible(if (nrow(leaks) > 0L) 3L else 0L))
  }

  cat(sprintf("[janitor] processes famille R vus : %d | candidats mirai : %d\n",
              nrow(procs), nrow(cls)))
  if (nrow(cls) == 0L) {
    cat("[janitor] AUCUN candidat mirai — pool propre ou absent. Rien à faire.\n")
    return(invisible(0L))
  }
  for (i in seq_len(nrow(cls))) {
    cat(sprintf("[janitor] pid %d — %s\n  preuve : %s\n",
                cls$pid[i], cls$verdict[i], cls$evidence[i]))
  }
  if (nrow(others) > 0L) {
    cat(sprintf(paste0("[janitor] %d candidat(s) ATTACHED/PRESERVED — JAMAIS ",
                       "touchés (discipline runbook §3-B : preuve PPID avant ",
                       "tout kill).\n"), nrow(others)))
  }
  if (!kill) {
    cat(sprintf("[janitor] --list : %d fuite(s) mirai identifiée(s). Aucun kill exécuté (relancer avec --kill).\n",
                nrow(leaks)))
    return(invisible(0L))
  }
  if (nrow(leaks) == 0L) {
    cat("[janitor] --kill : aucune fuite à tuer. Rien fait.\n")
    return(invisible(0L))
  }
  # Kill UN PID À LA FOIS, preuve déjà imprimée, racine de chaîne d'abord
  # (chaîne la plus profonde = plus proche du parent mort = dispatcher) :
  ord <- leaks[order(-leaks$depth, leaks$pid), , drop = FALSE]
  for (i in seq_len(nrow(ord))) {
    cat(sprintf("[janitor] taskkill /T /F pid %d ... ", ord$pid[i]))
    ok <- .mj_kill_one(ord$pid[i])
    cat(if (isTRUE(ok)) "OK\n" else "ÉCHEC (voir sortie taskkill)\n")
  }
  Sys.sleep(1)
  procs2 <- .mj_processes()
  cls2 <- .mj_classify(procs2)
  left <- cls2[cls2$verdict == "LEAK", , drop = FALSE]
  cat(sprintf("[janitor] re-mesure : %d fuite(s) résiduelle(s) / %d candidat(s) mirai restant(s)\n",
              nrow(left), nrow(cls2)))
  if (nrow(left) > 0L) {
    for (i in seq_len(nrow(left))) {
      cat(sprintf("[janitor] pid %d — %s\n  preuve : %s\n",
                  left$pid[i], left$verdict[i], left$evidence[i]))
    }
    return(invisible(1L))
  }
  cat("[janitor] population de fuites = 0 (mesuré). Critère F1 atteint.\n")
  invisible(0L)
}

args <- commandArgs(trailingOnly = TRUE)
unknown <- setdiff(args, c("--list", "--kill", "--json"))
if (length(unknown) > 0L || length(args) != 1L) {
  cat("Usage : Rscript tools/mirai_janitor.R [--list | --kill | --json]\n",
      "  SANS verbe : usage seul, aucune action (discipline run_dock.R).\n",
      "  --list : liste + preuve PPID, aucun kill.\n",
      "  --kill : kill un par un des LEAK uniquement, preuve avant chaque kill.\n",
      "  --json : sortie machine de --list (exit 3 si fuite présente).\n")
  quit(save = "no", status = 2L)
}
quit(save = "no",
     status = .mj_run(kill = "--kill" %in% args, json = "--json" %in% args))

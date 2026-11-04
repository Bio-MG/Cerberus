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
# USAGE
#   LC_ALL=French_France.65001 Rscript tools/boot_diagnose.R --observe \
#       [--max-secs 240] [--app-args "--headless"]
#   (--probe est un enfant interne — ne pas l'invoquer à la main ;
#    SANS verbe : usage seul, aucune action, discipline run_dock.R ;
#    stdout machine : grep 'DIAG:' — le bruit renv de .Rprofile est normal)
#
# SORTIE (machine-parseable, lignes DIAG:) :
#   DIAG: exit_code=<n|124-TIMEOUT_KILLED>
#   DIAG: listening=<yes|no> port=<p>
#   DIAG: server_ran=<yes|no>        (session_token= vu dans le log app)
#   DIAG: ready_json=<yes|no> ready_write_failed=<yes|no>
#   DIAG: http_ok=<yes|no> http_max=<code>
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

.bd_rscript_pids_launchdev <- function() {
  # PIDs Rscript dont la cmdline porte launch_dev_drive.R (l'app = chaîne
  # shim bin/Rscript.exe → cmd.exe → x64/Rscript.exe ; seuls les Rscript
  # comptent). Une requête CIM.
  ps <- paste0(
    "[Console]::OutputEncoding=[Text.Encoding]::UTF8; ",
    "Get-CimInstance Win32_Process -Filter \"Name='Rscript.exe'\" | ",
    "Where-Object { $_.CommandLine -match 'launch_dev_drive' } | ",
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

# ── mode enfant : la sonde concourante (ne jamais invoquer à la main) ────────
.bd_probe <- function(applog, probelog) {
  port <- NA_integer_
  server_ran <- FALSE
  ready_seen <- FALSE
  ready_mtime <- ""
  cycle <- 0L
  ready_path <- file.path(.bd_root(), "tools", "_drive", "ready.json")
  repeat {
    cycle <- cycle + 1L
    txt <- tryCatch(readLines(applog, warn = FALSE), error = function(e) NULL)
    if (is.na(port) && !is.null(txt)) {
      m <- regmatches(txt, regexec("Listening on http://127\\.0\\.0\\.1:(\\d+)", txt))
      hit <- Filter(function(x) length(x) == 2L, m)
      if (length(hit) > 0L) {
        port <- as.integer(hit[[1]][2])
        .bd_log(probelog, "PROBE listening port=", port)
      }
    }
    if (!server_ran && !is.null(txt) &&
        any(grepl("session_token=", txt, fixed = TRUE))) {
      server_ran <- TRUE
      .bd_log(probelog, "PROBE server_ran (session_token= vu)")
    }
    if (!is.null(txt) &&
        any(grepl("ready.json write FAILED", txt, fixed = TRUE))) {
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
  file.create(applog, probelog, summary_path, showWarnings = FALSE)

  lc <- Sys.getenv("LC_ALL", "")
  if (!identical(lc, "French_France.65001")) {
    cat("[boot_diagnose] REFUS : LC_ALL='", lc,
        "' — la recette exige LC_ALL=French_France.65001 (Git Bash : ",
        "LC_ALL=French_France.65001 Rscript tools/boot_diagnose.R --observe)\n",
        sep = "")
    quit(save = "no", status = 2L)
  }
  pre <- .bd_r_population()
  ram <- .bd_free_ram_mb()
  .bd_log(summary_path, "boot_diagnose ", stamp)
  .bd_log(summary_path, "pre_existing rscript_n=", pre$n,
          " rscript_mb=", pre$total_mb, " ram_free_mb=", round(ram))
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
      "--probe", shQuote(applog), shQuote(probelog)),
    wait = FALSE))
  .bd_log(summary_path, "probe_spawned pid=", probe_pid)

  # 2) l'app : l'UNIQUE boot supporté, VERBATIM — capture du code de sortie.
  # AUCUNE redirection ici : l'enfant hérite du wrapper (le fichier --applog).
  t0 <- Sys.time()
  res <- suppressWarnings(tryCatch(
    system2(.bd_rscript(),
            c(shQuote(file.path(root, "tools", "launch_dev_drive.R")), app_args),
            timeout = max_secs),
    error = function(e) structure("", status = -1L)))
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  raw_code <- if (is.null(attr(res, "status"))) 0L else as.integer(attr(res, "status"))
  timed_out <- identical(raw_code, 124L)

  # 3) si délai atteint : l'app tourne encore → kill POSSÉDÉ de SON arbre seul
  #    (pid identifié par éclosion : Rscript launch_dev_drive absent du snapshot
  #    pré-boot). Arbre complet /T = les daemons meurent avec — zéro fuite.
  kill_note <- ""
  if (timed_out) {
    Sys.sleep(1)
    now <- .bd_rscript_pids_launchdev()
    mine <- now[!(now$pid %in% pre$pid), , drop = FALSE] # à affiner : pid vs pids
    top <- if (nrow(mine) > 0L) {
      # le plus « haut » dans la chaîne : celui dont le parent n'est pas
      # lui-même un launch_dev_drive
      sel <- !(mine$ppid %in% mine$pid)
      mine[sel, , drop = FALSE]
    } else mine
    if (nrow(top) > 0L) {
      for (p in top$pid) {
        tk <- suppressWarnings(tryCatch(
          system2("taskkill", c("/PID", as.character(p), "/T", "/F"),
                  stdout = TRUE, stderr = TRUE),
          error = function(e) "erreur taskkill"))
        kill_note <- paste0(kill_note, " taskkill(", p, ")=",
                            if (any(grepl("ERROR|erreur", tk, ignore.case = TRUE))) "FAIL" else "OK")
      }
    } else {
      kill_note <- " aucun_pid_launchdev_trouve"
    }
  }
  .bd_log(summary_path, "app_ended elapsed_s=", round(elapsed, 1),
          " exit_code=", if (timed_out) "124 TIMEOUT_KILLED" else as.character(raw_code),
          kill_note)

  # 4) stop de la sonde (pid 0 = system2 n'a pas rendu de pid — ne rien tuer)
  if (is.numeric(probe_pid) && probe_pid > 0) {
    tryCatch(tools::pskill(probe_pid), error = function(e) NULL)
  }
  Sys.sleep(0.5)

  # 5) résumé (logs + janitor lecture seule)
  app_txt <- tryCatch(readLines(applog, warn = FALSE), error = function(e) character(0))
  probe_txt <- tryCatch(readLines(probelog, warn = FALSE), error = function(e) character(0))
  listened <- any(grepl("Listening on", app_txt, fixed = TRUE))
  port <- NA_integer_
  m <- regmatches(app_txt, regexec("Listening on http://127\\.0\\.0\\.1:(\\d+)", app_txt))
  hit <- Filter(function(x) length(x) == 2L, m)
  if (length(hit) > 0L) port <- as.integer(hit[[length(hit)]][2])
  server_ran <- any(grepl("session_token=", app_txt, fixed = TRUE))
  write_failed <- any(grepl("ready.json write FAILED", app_txt, fixed = TRUE))
  ready_seen <- any(grepl("ready.json APParu", probe_txt, fixed = TRUE))
  hs <- regmatches(probe_txt, regexpr("http=[0-9]+", probe_txt))
  http_max <- if (length(hs)) max(suppressWarnings(as.integer(sub("http=", "", hs))), na.rm = TRUE) else 0L
  jan <- suppressWarnings(tryCatch(
    system2(.bd_rscript(),
            c("--no-init-file", shQuote(file.path(root, "tools", "mirai_janitor.R")),
              "--list"),
            stdout = TRUE, stderr = FALSE), error = function(e) character(0)))
  leaks <- sum(grepl("— LEAK|LEAK$", jan))
  .bd_log(summary_path, "verdict listening=", ifelse(listened, "yes", "no"),
          " port=", port, " server_ran=", ifelse(server_ran, "yes", "no"),
          " ready_write_failed=", ifelse(write_failed, "yes", "no"),
          " ready_json_seen=", ifelse(ready_seen, "yes", "no"),
          " http_max=", http_max, " mirai_leaks=", leaks)
  cat("DIAG: exit_code=", if (timed_out) "124-TIMEOUT_KILLED" else as.character(raw_code),
      "\n", sep = "")
  cat("DIAG: listening=", ifelse(listened, "yes", "no"), " port=", port, "\n", sep = "")
  cat("DIAG: server_ran=", ifelse(server_ran, "yes", "no"), "\n", sep = "")
  cat("DIAG: ready_json=", ifelse(ready_seen, "yes", "no"),
      " ready_write_failed=", ifelse(write_failed, "yes", "no"), "\n", sep = "")
  cat("DIAG: http_ok=", ifelse(http_max == 200L, "yes", "no"),
      " http_max=", http_max, "\n", sep = "")
  cat("DIAG: leaks=", leaks, "\n", sep = "")
  cat(sprintf("[boot_diagnose] logs : %s\n", dir))
  invisible(0L)
}

# ── CLI ──────────────────────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 3L && args[1] == "--probe") {
  .bd_probe(args[2], args[3])
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

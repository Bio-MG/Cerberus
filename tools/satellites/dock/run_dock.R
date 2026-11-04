# =============================================================================
# tools/satellites/dock/run_dock.R — N3 cerberus-dock : CLI (effets)
# =============================================================================
# Fiche (fait foi) : PROPOSAL_N_SATELLITES_LAUNCH_ROADMAP_2026-10-26.md §2-S7 ;
# contrat figé : tools/satellites/dock/MANIFEST.md v1.0.0 (verbes + exit codes).
#
# RÈGLE NON NÉGOCIABLE : le dock ne démarre JAMAIS rien sans la commande
# explicite de l'opérateur — il n'existe QUE les verbes ci-dessous, aucun
# watcher, aucun daemon, aucun hook de boot de l'app. Sans verbe : usage.
#
# Usage :
#   Rscript tools/satellites/dock/run_dock.R --start  <name>
#   Rscript tools/satellites/dock/run_dock.R --health <name>
#   Rscript tools/satellites/dock/run_dock.R --status
#   Rscript tools/satellites/dock/run_dock.R --stop   <name>
#
# Effets (ce fichier) : spawn/kill via processx (dépôt verrouillé — zéro
# nouvelle dépendance), sonde HTTP via curl (verrouillé), état runtime dans
# tools/_drive/satellites/ (gitigné). La DÉCISION (validation manifeste,
# verdict santé) vit dans dock_lib.R (pure, testée par test-cerberus-dock.R).
# Ce fichier est sous tools/ : hors conventions gate par construction (note
# MANIFEST.md §1).
# =============================================================================

.dock_self_dir <- function() {
  # racine = 2 niveaux au-dessus de ce script (tools/satellites/dock/)
  args <- commandArgs(trailingOnly = FALSE)
  fa <- grep("^--file=", args, value = TRUE)
  if (length(fa) == 1L) {
    self <- normalizePath(sub("^--file=", "", fa[1]), winslash = "/", mustWork = TRUE)
    return(dirname(self))
  }
  # repli : cwd du dépôt (lancement depuis la racine, usage documenté)
  file.path(getwd(), "tools", "satellites", "dock")
}

.dock_root <- function() normalizePath(file.path(.dock_self_dir(), "..", "..", ".."),
                                       winslash = "/", mustWork = TRUE)

.dock_usage <- function() {
  cat("cerberus-dock — h\u00f4te des satellites (contrat MANIFEST.md v1.0.0)\n\n")
  cat("Verbes (UN verbe par invocation, jamais de d\u00e9marrage implicite) :\n")
  cat("  --start  <name>   d\u00e9marre le satellite (commande explicite), attend la sant\u00e9\n")
  cat("  --health <name>   sonde la sant\u00e9 (OK/FAIL/TIMEOUT)\n")
  cat("  --status          \u00e9tat par satellite (pid/port/sant\u00e9) — ne d\u00e9marre RIEN\n")
  cat("  --stop   <name>   arr\u00eate (kill de l'arbre) + retire le pid-file\n")
  cat("\nExit : 0 succ\u00e8s \u00b7 1 sant\u00e9 FAIL/TIMEOUT \u00b7 2 refus (manifeste/inconnu/\u00e9teint/d\u00e9j\u00e0 d\u00e9marr\u00e9)\n")
}

.dock_load_manifest <- function() {
  source(file.path(.dock_self_dir(), "dock_lib.R"), local = FALSE) # global : la lib sert le dispatch aussi
  m <- ts_dock_manifest_load(file.path(.dock_self_dir(), "dock_manifest.json"))
  if (!isTRUE(ts_dock_manifest_valid(m))) {
    stop("Manifeste dock INVALIDE (format fig\u00e9 v1.0.0 \u2014 voir MANIFEST.md)",
         call. = FALSE)
  }
  m
}

.dock_state_dir <- function() {
  d <- file.path(.dock_root(), "tools", "_drive", "satellites")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  d
}

.dock_pid_alive <- function(pid) {
  if (is.null(pid) || is.na(pid) || pid <= 0) return(FALSE)
  # processx verrouillé : ni ps_exists ni process$new(pid=) (mesurés absents)
  # → sonde Windows native tasklist (host-first, note README Limites)
  out <- tryCatch(
    system2("tasklist", c("/FI", shQuote(paste0("PID eq ", as.integer(pid))),
                          "/FO", "CSV", "/NH"),
            stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  )
  any(grepl(paste0('"?', as.integer(pid), '"?'), out), na.rm = TRUE) &&
    !any(grepl("INFO:", out))
}

#' Kill de l'ARBRE du pid (Windows taskkill /T) — TRUE si taskkill sort 0.
.dock_kill_tree <- function(pid) {
  out <- suppressWarnings(system2("taskkill",
                                  c("/PID", as.character(as.integer(pid)),
                                    "/T", "/F"),
                                  stdout = TRUE, stderr = TRUE))
  !any(grepl("ERROR|erreur", out, ignore.case = TRUE))
}

.dock_read_pid_file <- function(path) {
  if (!file.exists(path)) return(NULL)
  json <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE),
                   error = function(e) NULL)
  json
}

.dock_probe_health <- function(sat) {
  t0 <- Sys.time()
  res <- tryCatch({
    r <- curl::curl_fetch_memory(sat$health$url,
                                 handle = curl::new_handle(timeout = sat$health$timeout_s))
    list(status = as.integer(r$status_code), error = NULL)
  }, error = function(e) list(status = NA_integer_, error = conditionMessage(e)))
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  verdict <- ts_dock_health_verdict(res$status, as.integer(sat$health$expect_status),
                                    elapsed, sat$health$timeout_s)
  if (!is.null(res$error)) {
    # échec de connexion : le détail = LA cause réseau (jamais « sonde
    # malformée », qui serait un mensonge — la sonde s'est bien exécutée,
    # c'est le serveur qui est injoignable)
    verdict$verdict <- "FAIL"
    verdict$detail <- paste0("injoignable \u2014 ", res$error)
  }
  verdict
}

.dock_cmd_start <- function(sat, manifest) {
  pid_path <- ts_dock_pid_file(.dock_root(), sat$name)
  prev <- .dock_read_pid_file(pid_path)
  if (!is.null(prev) && .dock_pid_alive(prev$pid)) {
    cat(sprintf("[dock] %s : D\u00c9J\u00c0 D\u00c9MARR\u00c9 (pid %s) \u2014 refus, jamais un doublon de process\n",
                sat$name, prev$pid))
    quit(save = "no", status = 2L)
  }
  # garde honn\u00eate : sant\u00e9 d\u00e9j\u00e0 OK sans pid vivant (port occup\u00e9 par un autre process)
  v0 <- .dock_probe_health(sat)
  if (identical(v0$verdict, "OK")) {
    cat(sprintf("[dock] %s : sant\u00e9 d\u00e9j\u00e0 OK sur le port %s (process non g\u00e9r\u00e9 par le dock) \u2014 refus\n",
                sat$name, sat$port))
    quit(save = "no", status = 2L)
  }
  log_file <- file.path(.dock_state_dir(), paste0(sat$name, ".log"))
  cmd <- vapply(sat$command, function(e) e[[1]], character(1))
  p <- processx::process$new(
    cmd[1], cmd[-1],
    wd = normalizePath(file.path(.dock_root(), sat$cwd), winslash = "/", mustWork = TRUE),
    stdout = log_file, stderr = "2>&1",
    cleanup = FALSE,    # le satellite SURVIT au CLI (mesuré : défaut = kill à la sortie R)
    supervise = FALSE
  )
  # attendre la sant\u00e9 (budget = timeout + 2 intervalles de gr\u00e2ce)
  deadline <- sat$health$timeout_s + 2 * sat$health$interval_s
  t0 <- Sys.time()
  verdict <- NULL
  repeat {
    Sys.sleep(sat$health$interval_s)
    verdict <- .dock_probe_health(sat)
    if (identical(verdict$verdict, "OK")) break
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > deadline) break
    if (!p$is_alive()) {
      cat(sprintf("[dock] %s : le process est MORT pendant le d\u00e9marrage (log : %s)\n",
                  sat$name, log_file))
      quit(save = "no", status = 2L)
    }
  }
  jsonlite::write_json(
    list(pid = p$get_pid(), started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
         command = cmd, manifest_format_version = manifest$format_version),
    pid_path, auto_unbox = TRUE, pretty = TRUE)
  if (identical(verdict$verdict, "OK")) {
    cat(sprintf("[dock] %s : D\u00c9MARR\u00c9 (pid %s, port %s) \u2014 %s\n",
                sat$name, p$get_pid(), sat$port, verdict$detail))
    quit(save = "no", status = 0L)
  }
  cat(sprintf("[dock] %s : d\u00e9marr\u00e9 (pid %s) MAIS sant\u00e9 %s \u2014 %s (log : %s)\n",
              sat$name, p$get_pid(), verdict$verdict, verdict$detail, log_file))
  quit(save = "no", status = 1L)
}

.dock_cmd_health <- function(sat) {
  v <- .dock_probe_health(sat)
  cat(sprintf("[dock] %s : sant\u00e9 %s \u2014 %s\n", sat$name, v$verdict, v$detail))
  quit(save = "no", status = if (identical(v$verdict, "OK")) 0L else 1L)
}

.dock_cmd_status <- function(manifest) {
  cat(sprintf("[dock] manifeste v%s \u2014 %d satellite(s)\n",
              manifest$format_version, length(manifest$satellites)))
  for (sat in manifest$satellites) {
    pid_path <- ts_dock_pid_file(.dock_root(), sat$name)
    prev <- .dock_read_pid_file(pid_path)
    running <- !is.null(prev) && .dock_pid_alive(prev$pid)
    v <- .dock_probe_health(sat)
    cat(sprintf("  %-12s port %-5s pid %-8s running=%-5s sant\u00e9=%-7s %s\n",
                sat$name, sat$port,
                if (is.null(prev$pid)) "-" else as.character(prev$pid),
                if (running) "oui" else "non",
                v$verdict, v$detail))
  }
  quit(save = "no", status = 0L)
}

.dock_cmd_stop <- function(sat) {
  pid_path <- ts_dock_pid_file(.dock_root(), sat$name)
  prev <- .dock_read_pid_file(pid_path)
  if (is.null(prev)) {
    cat(sprintf("[dock] %s : non d\u00e9marr\u00e9 (aucun pid-file) \u2014 no-op annonc\u00e9\n", sat$name))
    quit(save = "no", status = 0L)
  }
  if (!.dock_pid_alive(prev$pid)) {
    unlink(pid_path)
    cat(sprintf("[dock] %s : pid %s d\u00e9j\u00e0 disparu \u2014 pid-file retir\u00e9 (no-op annonc\u00e9)\n",
                sat$name, prev$pid))
    quit(save = "no", status = 0L)
  }
  killed <- .dock_kill_tree(prev$pid)
  Sys.sleep(0.5)
  unlink(pid_path)
  v <- .dock_probe_health(sat)
  if (isTRUE(killed) && !identical(v$verdict, "OK")) {
    cat(sprintf("[dock] %s : ARR\u00caT\u00c9 (pid %s) \u2014 port lib\u00e9r\u00e9 (%s)\n",
                sat$name, prev$pid, v$detail))
    quit(save = "no", status = 0L)
  }
  if (isTRUE(killed)) {
    cat(sprintf("[dock] %s : pid %s tu\u00e9 MAIS sant\u00e9 encore %s \u2014 %s (autre process sur le port ?)\n",
                sat$name, prev$pid, v$verdict, v$detail))
    quit(save = "no", status = 1L)
  }
  cat(sprintf("[dock] %s : \u00c9CHEC de l'arr\u00eat du pid %s (processx)\n", sat$name, prev$pid))
  quit(save = "no", status = 2L)
}

# ── Dispatch (sans verbe : usage SEUL — jamais d'action) ----------------------

args <- commandArgs(trailingOnly = TRUE)
verb <- NULL
name <- NULL
if (length(args) >= 1L) verb <- args[1]
if (length(args) >= 2L) name <- args[2]

if (is.null(verb) || !verb %in% c("--start", "--health", "--status", "--stop")) {
  .dock_usage()
  quit(save = "no", status = if (is.null(verb)) 0L else 2L)
}

manifest <- tryCatch(.dock_load_manifest(), error = function(e) {
  cat(sprintf("[dock] ERREUR : %s\n", conditionMessage(e)))
  quit(save = "no", status = 2L)
})

if (verb == "--status") .dock_cmd_status(manifest)

if (is.null(name)) {
  cat("[dock] ERREUR : verbe", verb, "exige un nom de satellite\n")
  .dock_usage()
  quit(save = "no", status = 2L)
}
sat <- ts_dock_satellite(manifest, name)
if (is.null(sat)) {
  cat(sprintf("[dock] ERREUR : satellite inconnu '%s' (manifeste : %s)\n",
              name, paste(vapply(manifest$satellites, function(s) s$name, character(1)),
                          collapse = ", ")))
  quit(save = "no", status = 2L)
}
if (verb == "--start" && !isTRUE(sat$enabled)) {
  cat(sprintf("[dock] %s : enabled=false \u2014 refus honn\u00eate (jamais d\u00e9marr\u00e9 en silence)\n",
              sat$name))
  quit(save = "no", status = 2L)
}

switch(verb,
  "--start"  = .dock_cmd_start(sat, manifest),
  "--health" = .dock_cmd_health(sat),
  "--stop"   = .dock_cmd_stop(sat)
)

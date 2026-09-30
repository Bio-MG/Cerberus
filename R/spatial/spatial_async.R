# =============================================================================
# R/utils_spatial_async.R — mirai daemon pool + reactivePoll progress tracking
# =============================================================================
# v5 (vague 5 — Phase 6 stats preload): R/utils_spatial_stats.R (B1 neighborhood
# enrichment, B3 composition diff, B4 Getis-Ord hotspots, B6 Ripley's K) is now
# part of the default `source_files` preloaded into every daemon, and
# .verify_spatial_daemons() checks the 4 new functions are actually visible
# after preload (has_enrichment / has_diffcomp / has_hotspots / has_ripley) —
# same "verify what actually happened, not just that daemons(n) returned"
# philosophy as v4 below. A daemon pool started BEFORE this change (i.e. a
# running R process that already called init_spatial_daemons() with the old
# source_files) will keep reporting "degrade" with these 4 names listed as
# missing until "Reinitialiser les daemons" (mod_spatial.R) is clicked, or the
# app is restarted — init_spatial_daemons() itself is a no-op once
# .spatial_async_env$daemons_ready is TRUE (see below), it does not
# auto-reload a pool that is already up.
#
# v4 (Chantier 3 refonte — daemon preload hardening). Two problems found on
# top of the reference-pipeline architecture fix (see
# R/utils_spatial_reference.R and mod_spatial_deconv.R):
#
#   1. source_files used RELATIVE paths, resolved fresh inside each daemon
#      by that daemon's OWN getwd() at the time source() runs -- if a daemon
#      process ever starts with a different working directory than the main
#      Shiny process, preload silently fails file-by-file with only a
#      message() nobody sees (daemons print to their own stdout, not the
#      user-facing Shiny UI). Paths are now resolved to ABSOLUTE paths ONCE,
#      in the main process (which knows its own getwd() reliably), before
#      being shipped into mirai::everywhere().
#
#   2. There was no verification step: init_spatial_daemons() declared the
#      pool "ready" the instant mirai::daemons(n) returned, regardless of
#      whether the preload (everywhere()) actually succeeded on every
#      worker. This is exactly the class of bug that made "RCTD/Label
#      Transfer always fail" so hard to diagnose from the UI -- the daemon
#      badge said "actifs" the whole time. init_spatial_daemons() now runs
#      .verify_spatial_daemons() right after preload and records a real
#      ready/degraded state (spatial_daemon_status()), with a short
#      human-readable diagnostic (spatial_daemon_diagnostics_text()) the UI
#      can surface directly (see mod_spatial.R's daemon badge).
#
#   3. helpers_io.R / helpers_sc.R are REMOVED from the default
#      source_files. They existed here only so mod_spatial_deconv.R's old
#      daemon-side reference reload (load_single_cell_data()/
#      prepare_seurat_object()) could find those functions -- functions
#      that, in this codebase's actual helpers_sc.R, do not exist. Now that
#      reference parsing happens once on the main process and the daemon
#      only reads a self-contained prepared artifact (base R + BPCells), no
#      spatial daemon task needs either file anymore. Removing the false
#      dependency is the point of this fix, not an incidental cleanup.
#
# v3 (Chantier 2 refonte — deconvolution daemon fix, superseded by v4 above):
# `source_files` used to list ONLY R/utils_spatial_*.R (io/multi/niche).
# helpers_io.R and helpers_sc.R were added at that time to fix RCTD/Label
# Transfer's daemon-side reference reload — v4 above removes that need
# entirely instead, which is the more robust fix requested for Chantier 3.
#
# v2 (post-test-3): added .timeout support (mirai() has a native `.timeout`
# arg — a hung task now errors out after MIRAI_TASK_TIMEOUT_MS instead of
# blocking forever) and reset_spatial_daemons() (recover a poisoned pool
# from the UI, no R restart needed).
#
# Daemons are process-level (shared by every Shiny session running in this R
# process), so init_spatial_daemons() is idempotent — it only spawns workers
# the first time it is called, and pre-loads Seurat/BPCells + this project's
# own helper files on every daemon via mirai::everywhere() so individual
# mirai() calls stay small (no need to re-serialize function bodies/library()
# calls on every single task).
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

.spatial_async_env <- new.env(parent = emptyenv())
.spatial_async_env$daemons_ready    <- FALSE
.spatial_async_env$daemons_verified <- FALSE
# QW-3 (2026-09-30) : le nombre de daemons se lit dans la config — la
# constante TS_MIRAI_N_DAEMONS existait mais était ignorée (audit C-2).
.spatial_async_env$n_daemons        <- if (exists("TS_MIRAI_N_DAEMONS")) TS_MIRAI_N_DAEMONS else 6L
.spatial_async_env$base_dir         <- NULL
.spatial_async_env$source_files_abs <- character(0)
.spatial_async_env$last_diagnostics <- NULL

# Hard ceiling for any single spatial async task (clustering, deconvolution,
# Moran's I, sketch UMAP, neighborhood enrichment, hotspots, Ripley's K) —
# after this, the ExtendedTask errors out instead of hanging forever, so the
# UI always eventually gets actionable feedback.
MIRAI_TASK_TIMEOUT_MS <- if (exists("TS_MIRAI_TIMEOUT_MS")) TS_MIRAI_TIMEOUT_MS else 20 * 60 * 1000L  # 20 minutes
# RCTD (mode="rctd", doublet_mode="full") peut aussi tourner longtemps,
# independamment de la taille de la reference (observe en reel : une
# reference lymph node ~73k cellules / ~20 types a depasse le plafond
# partage de 20 min) — plafond dedie, meme logique que Label Transfer.
RCTD_TIMEOUT_MS <- if (exists("TS_RCTD_TIMEOUT_MS")) TS_RCTD_TIMEOUT_MS else 40 * 60 * 1000L  # 40 minutes
# Label Transfer (mod_spatial_deconv.R, mode="labeltransfer") runs TWO
# SCTransform calls (reference + query) plus FindTransferAnchors/TransferData
# — reported to hit the shared 20-minute ceiling even on a moderately-sized
# reference on CPU-only hardware, especially without the optional
# `glmGamPoi` package. Given a genuinely longer-running-by-design task, use
# a longer, dedicated ceiling rather than raising the shared one.
LABEL_TRANSFER_TIMEOUT_MS <- if (exists("TS_LABEL_TRANSFER_TIMEOUT")) TS_LABEL_TRANSFER_TIMEOUT else 45 * 60 * 1000L  # 45 minutes

# ── v6 (4E-4) — le pool devient un POOL APPLICATIF partage ─────────────────
# Decision utilisateur 2026-09-14 (`STATUS.md` §2am.1, option (b)) : un seul
# pool mirai partage par l'application, et non un pool « spatial » auquel on
# grefferait un second pool SC (interdit : un seul mecanisme de workers).
#
# Consequence MESUREE : le pool demarre UNE fois et sa liste de preload est
# FIGEE a cet instant (`init_spatial_daemons()` est idempotent). Un daemon qui
# n'a pas preloade un fichier ne peut pas resoudre les fonctions qu'il definit —
# mesure : `could not find function "main_only_helper"`. La liste par defaut
# doit donc couvrir TOUS les domaines async de l'application des le premier
# demarrage, sinon la branche echoue au premier appel reel.
#
# Le domaine SC contribue ici la fermeture de Milo (4E-1), mesuree le
# 2026-09-16 : config + R/core/{io_helpers,state,provenance,validation} +
# R/sc/{sc_velocity,sc_abundance_design,sc_abundance_milo,sc_abundance_milo_views}.
# Cout mesure : +0,6 s sur l'init du pool (7,4 s -> 8,0 s pour 2 daemons).
# scCODA (4E-2) reste EXCLU : reticulate/TensorFlow initialise Python par
# processus, donc un daemon de plus = un interpreteur Python de plus.
#
# ⚠️ Un pool demarre AVANT ce changement garde l'ancien preload : le badge
# passera en « degrade » et il faut « Reinitialiser les daemons »
# (reset_spatial_daemons()), exactement comme pour le preload v5.
APP_DAEMON_SOURCE_FILES <- c(
  # --- Spatial (v5) ---
  "R/spatial/spatial_async.R",
  "R/spatial/spatial_io.R",
  "R/spatial/spatial_multi.R",
  "R/spatial/spatial_niche.R",
  "R/spatial/spatial_deconv_prep.R",
  "R/spatial/spatial_deconv_tasks.R",
  "R/spatial/spatial_stats.R",
  # --- SC : abondance differentielle Milo (4E-4) — ORDRE : config, core, domaine ---
  "config/defaults.R",
  "config/thresholds.R",
  "R/core/io_helpers.R",
  "R/core/state.R",
  "R/core/provenance.R",
  "R/core/validation.R",
  "R/sc/sc_velocity.R",
  "R/sc/sc_abundance_design.R",
  "R/sc/sc_abundance_milo.R",
  "R/sc/sc_abundance_milo_views.R"
)

#' Resolve the application root WITHOUT trusting getwd() (4E-4)
#'
#' v4 resolved source_files to absolute paths in the main process — but still
#' derived them from `getwd()`. Measured 2026-09-16 : under `testthat::test_file()`
#' the working directory is the TEST directory, so every preload path pointed
#' nowhere, `file.exists()` was FALSE inside every daemon, the preload was
#' skipped with only a `message()` nobody reads, and the pool reported
#' "degraded" while the first real job failed with « could not find function ».
#' The same silent degradation would happen in production if the app were ever
#' launched from another directory.
#'
#' Walking up to the `app.R` marker is the same rule the test helpers use
#' (`helper-source.R`), so main process and harness agree by construction.
#'
#' @return Absolute path to the directory containing `app.R`, or `getwd()`
#'   if no marker is found (never fatal: the caller warns on missing files).
.resolve_app_base_dir <- function(marker = "app.R", max_up = 8L) {
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  for (i in seq_len(max_up)) {
    if (file.exists(file.path(d, marker))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

#' Initialize the mirai daemon pool used by all spatial async tasks
#'
#' Idempotent: safe to call multiple times — daemons are only spawned once
#' per R process. Pre-loads Seurat + BPCells and sources this project's
#' helper files (ABSOLUTE paths, resolved here in the main process) on every
#' daemon, then runs a post-init verification pass and records the result.
#'
#' @param n_daemons Integer, number of persistent background R processes.
#' @param source_files Character vector of project file paths (relative to
#'   the app's working directory) to source() inside every daemon. Defaults to
#'   APP_DAEMON_SOURCE_FILES — the APPLICATION-wide preload (spatial + SC DA),
#'   because the pool is started once and its preload is frozen at that moment
#'   (see the v6 changelog above).
#' @return invisible(TRUE) on success, invisible(FALSE) if mirai is missing.
init_spatial_daemons <- function(n_daemons = if (exists("TS_MIRAI_N_DAEMONS")) TS_MIRAI_N_DAEMONS else 6L,
                                 source_files = APP_DAEMON_SOURCE_FILES) {
  if (!requireNamespace("mirai", quietly = TRUE)) {
    warning("Package 'mirai' manquant : les calculs spatiaux asynchrones (clustering, ",
            "deconvolution, indice de Moran) seront indisponibles. Installez-le via ",
            "install.packages('mirai').")
    return(invisible(FALSE))
  }

  if (isTRUE(.spatial_async_env$daemons_ready)) return(invisible(TRUE))

  # Resolve ABSOLUTE paths ONCE, here (main process) -- never re-derived
  # relative to whatever a given daemon's own getwd() is. Le repertoire de base
  # est resolu par le marqueur app.R, PAS par getwd() (cf. .resolve_app_base_dir).
  base_dir <- .resolve_app_base_dir()
  abs_files <- normalizePath(file.path(base_dir, source_files), winslash = "/", mustWork = FALSE)
  .spatial_async_env$base_dir         <- base_dir
  .spatial_async_env$source_files_abs <- abs_files
  .spatial_async_env$n_daemons        <- n_daemons

  # Un fichier de preload absent n'est PAS fatal cote daemon (le `for` se
  # contente d'un message() invisible depuis Shiny) : on le signale ICI, sur le
  # processus principal, sinon la panne n'apparait qu'au premier job reel.
  .missing_files <- abs_files[!file.exists(abs_files)]
  if (length(.missing_files) > 0L) {
    warning(sprintf(
      paste0("init_spatial_daemons() : %d fichier(s) de preload INTROUVABLE(S) ",
             "— le pool demarrera DEGRADE (repertoire de base : %s). Manquant(s) : %s"),
      length(.missing_files), base_dir,
      paste(basename(.missing_files), collapse = ", ")),
      call. = FALSE)
  }

  mirai::daemons(n_daemons)

  tryCatch({
    mirai::everywhere({
      suppressPackageStartupMessages({
        if (requireNamespace("Seurat", quietly = TRUE))  library(Seurat)
        if (requireNamespace("BPCells", quietly = TRUE)) library(BPCells)
      })
      for (.f in .abs_files) {
        if (file.exists(.f)) {
          source(.f)
        } else {
          message(sprintf("[spatial daemon] Fichier attendu introuvable (chemin absolu) : %s", .f))
        }
      }
    }, .abs_files = abs_files)
  }, error = function(e) {
    warning("mirai::everywhere() a echoue lors du preload des daemons : ", conditionMessage(e))
  })

  # Post-init verification: mark the pool ready ONLY once we know what
  # actually happened on the workers, not just that daemons(n) returned.
  verify <- tryCatch(.verify_spatial_daemons(n_daemons),
                      error = function(e) list(ok = FALSE, missing = conditionMessage(e),
                                               n_checks = 0L, n_distinct_pids = 0L))
  .spatial_async_env$daemons_verified <- isTRUE(verify$ok)
  .spatial_async_env$last_diagnostics <- verify
  .spatial_async_env$daemons_ready    <- TRUE

  if (isTRUE(verify$ok)) {
    message(sprintf("[spatial] %d daemon(s) mirai initialises et verifies (%d PID distincts / %d tests).",
                    n_daemons, verify$n_distinct_pids %||% NA, verify$n_checks %||% NA))
  } else {
    warning(sprintf("[spatial] Daemons demarres mais verification post-init EN ECHEC (%s). ",
                    paste(verify$missing, collapse = "; ")),
            "Consultez spatial_daemon_diagnostics_text() ou le badge 'Session' de l'onglet Spatial.")
  }

  invisible(TRUE)
}

#' Point d'entree NEUTRE du pool applicatif partage (4E-4)
#'
#' Alias documente de init_spatial_daemons() pour les domaines qui ne sont pas
#' spatiaux (aujourd'hui : la DA Milo du module SC). Il n'existe QU'UN pool de
#' workers dans l'application (regle 8) : cette fonction ne cree rien de plus,
#' elle garantit que le pool applicatif est demarre — c'est-a-dire que le
#' preload APP_DAEMON_SOURCE_FILES a bien ete applique aux daemons.
#'
#' Idempotent : sans effet si le pool est deja pret.
#'
#' @param ... Arguments transmis a init_spatial_daemons() (n_daemons,
#'   source_files). A ne renseigner que pour un besoin explicite.
#' @return invisible(TRUE) si le pool est disponible et verifie,
#'   invisible(FALSE) si mirai est absent (l'appelant doit alors le DIRE a
#'   l'utilisateur : le repli synchrone de run_job() est trace, jamais muet).
ensure_app_daemons <- function(...) {
  if (!requireNamespace("mirai", quietly = TRUE)) return(invisible(FALSE))
  init_spatial_daemons(...)
  invisible(isTRUE(.spatial_async_env$daemons_ready))
}

#' Are the spatial mirai daemons started? (pool up, NOT necessarily verified)
spatial_daemons_ready <- function() isTRUE(.spatial_async_env$daemons_ready)

#' Three-state daemon status for the UI badge
#'
#' @return One of "inactive" (pool never started), "degraded" (pool started
#'   but post-init verification found a problem), "ready" (pool started and
#'   verified).
spatial_daemon_status <- function() {
  if (!isTRUE(.spatial_async_env$daemons_ready)) return("inactive")
  if (isTRUE(.spatial_async_env$daemons_verified)) return("ready")
  "degraded"
}

#' Short, human-readable diagnostic text for the daemon pool
#'
#' Intended for direct display in the UI (e.g. inside a collapsed
#' <details> under the "degraded" badge state) so a preload problem is
#' visible WITHOUT opening the R console.
spatial_daemon_diagnostics_text <- function() {
  d <- .spatial_async_env$last_diagnostics
  if (is.null(d)) return("Aucun diagnostic disponible -- daemons jamais initialises.")

  files_txt <- if (length(.spatial_async_env$source_files_abs) > 0) {
    paste(paste0("  - ", .spatial_async_env$source_files_abs), collapse = "\n")
  } else "  (aucun)"

  txt <- sprintf(
    "Repertoire de base : %s\nFichiers preloades (chemins absolus) :\n%s\nTests : %s | PIDs distincts : %s | OK : %s",
    .spatial_async_env$base_dir %||% "?",
    files_txt,
    as.character(d$n_checks %||% "?"),
    as.character(d$n_distinct_pids %||% "?"),
    isTRUE(d$ok)
  )
  if (length(d$missing) > 0) {
    txt <- paste0(txt, "\nManquant(s)/erreur(s) : ", paste(d$missing, collapse = "; "))
  }
  txt
}

#' Best-effort post-init verification of the daemon preload
#'
#' Runs a handful of tiny diagnostic tasks through the pool and checks that
#' required packages/functions are visible where they should be. Since
#' mirai::everywhere() applies identically to every current AND future
#' daemon in the pool (single local machine, same filesystem), a handful of
#' checks is a strong-enough signal in practice — sequential blocking calls
#' do not guarantee hitting every distinct daemon PID (a fast idle worker
#' can serve more than its "share"), so this also reports how many DISTINCT
#' PIDs it reached, as a diagnostic rather than a hard coverage guarantee.
#'
#' @param n_daemons Integer, pool size (drives how many checks to run).
#' @return list(ok=, n_checks=, n_distinct_pids=, results=, missing=).
.verify_spatial_daemons <- function(n_daemons) {
  n_checks <- max(3L * n_daemons, 6L)
  results <- vector("list", n_checks)
  for (i in seq_len(n_checks)) {
    results[[i]] <- tryCatch(
      mirai::mirai({
        list(
          pid           = Sys.getpid(),
          wd            = getwd(),
          has_bpcells   = requireNamespace("BPCells", quietly = TRUE),
          has_seurat    = requireNamespace("Seurat", quietly = TRUE),
          has_rann      = requireNamespace("RANN", quietly = TRUE),
          has_write_log = exists("write_mirai_log", mode = "function"),
          has_integrate = exists("integrate_spatial_sketches", mode = "function"),
          has_niches    = exists("compute_spatial_niches", mode = "function"),
          # v5 (vague 5 — Phase 6 stats): confirms R/utils_spatial_stats.R
          # actually preloaded without error -- see that file's header.
          # NAMES MUST MATCH EXACTLY what R/utils_spatial_stats.R defines —
          # a previous draft of that file used different names
          # (spatial_composition_diff/spatial_hotspots_gi/spatial_ripley_k),
          # which is what produced a "has_enrichment; has_ripley missing"
          # degraded badge even though the file itself sourced fine.
          has_enrichment = exists("spatial_neighborhood_enrichment", mode = "function"),
          has_diffcomp   = exists("compute_composition_differential", mode = "function"),
          has_hotspots   = exists("compute_getis_ord_hotspots", mode = "function"),
          has_ripley     = exists("ripley_k_random_labeling", mode = "function")
        )
      })[],
      error = function(e) list(error = conditionMessage(e))
    )
  }

  checks <- c("has_bpcells", "has_seurat", "has_rann", "has_write_log", "has_integrate", "has_niches",
              "has_enrichment", "has_diffcomp", "has_hotspots", "has_ripley")
  ok_vec <- vapply(results, function(r) {
    !is.null(r) && is.null(r$error) && all(vapply(checks, function(chk) isTRUE(r[[chk]]), logical(1)))
  }, logical(1))

  pids <- unique(vapply(results, function(r) as.character(r$pid %||% NA), character(1)))

  missing <- character(0)
  bad <- results[!ok_vec]
  if (length(bad) > 0) {
    for (chk in checks) {
      if (any(vapply(bad, function(r) is.null(r$error) && !isTRUE(r[[chk]]), logical(1)))) {
        missing <- c(missing, chk)
      }
    }
    errs <- unique(unlist(lapply(bad, function(r) r$error)))
    if (length(errs) > 0) missing <- c(missing, paste0("error: ", errs))
  }

  list(
    ok              = all(ok_vec) && length(results) > 0,
    n_checks        = n_checks,
    n_distinct_pids = length(stats::na.omit(pids)),
    results         = results,
    missing         = unique(missing)
  )
}

#' Stop and free the daemon pool (e.g. on app shutdown / testing teardown)
stop_spatial_daemons <- function() {
  if (isTRUE(.spatial_async_env$daemons_ready) && requireNamespace("mirai", quietly = TRUE)) {
    tryCatch(mirai::daemons(0), error = function(e) NULL)
  }
  .spatial_async_env$daemons_ready    <- FALSE
  .spatial_async_env$daemons_verified <- FALSE
  invisible(NULL)
}

#' Recover a possibly-poisoned daemon pool WITHOUT restarting R
#'
#' Tears the whole pool down (mirai::daemons(0), killing every worker
#' process — no zombie survives a full teardown) and respawns fresh
#' processes, re-running the same post-init verification as a normal
#' startup. Call this from the UI ("Reinitialiser les daemons") whenever a
#' task fails unexpectedly, times out, or the badge shows "degrade" — this
#' is ALSO the fix for a pool started before R/utils_spatial_stats.R
#' existed in source_files (see v5 changelog): init_spatial_daemons() alone
#' will not reload an already-`daemons_ready` pool, but reset always does.
#'
#' @param n_daemons Integer, pool size (default: whatever was last used).
#' @return invisible(TRUE)/(FALSE), see init_spatial_daemons().
reset_spatial_daemons <- function(n_daemons = NULL) {
  n <- n_daemons %||% .spatial_async_env$n_daemons %||% 6L
  stop_spatial_daemons()
  init_spatial_daemons(n_daemons = n)
}

#' Build a stable per-session, per-task log file path
#'
#' @param session Shiny session object (used for session$token uniqueness).
#' @param task_name Short slug, e.g. "cluster", "deconv", "moran".
#' @return Character path inside tempdir().
spatial_log_path <- function(session, task_name) {
  file.path(tempdir(), sprintf("spatial_%s_%s.log", task_name, session$token))
}

#' Truncate/create a log file before starting a new async run
#' @param file Character path.
#' @return invisible(file)
reset_log <- function(file) {
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  cat("", file = file)
  invisible(file)
}

#' Write a timestamped progress line — call this *inside* a mirai daemon
#'
#' @param file Character path to the log file (same one the tracker reads).
#' @param message Character, user-facing progress message (French).
#' @param step,total Optional integers rendered as a "[step/total]" prefix.
write_mirai_log <- function(file, message, step = NULL, total = NULL) {
  prefix <- if (!is.null(step) && !is.null(total)) sprintf("[%d/%d] ", step, total) else ""
  line <- sprintf("%s | %s%s\n", format(Sys.time(), "%H:%M:%S"), prefix, message)
  cat(line, file = file, append = TRUE)
}

#' reactivePoll-based tracker for a task's log file
#'
#' @param session Shiny session.
#' @param log_file Character path (stable for the module's lifetime).
#' @param interval_ms Poll interval in milliseconds.
#' @return A reactive expression returning the log's lines (character vector).
create_reactive_tracker <- function(session, log_file, interval_ms = 1000) {
  shiny::reactivePoll(
    intervalMillis = interval_ms,
    session        = session,
    checkFunc = function() {
      if (file.exists(log_file)) file.info(log_file)$mtime else NA
    },
    valueFunc = function() {
      if (file.exists(log_file)) readLines(log_file, warn = FALSE) else character(0)
    }
  )
}

#' Turn tracked log lines into a small textual/numeric progress summary
#'
#' @param log_lines Character vector, as returned by create_reactive_tracker().
#' @return list(text = <last line or placeholder>, pct = <0-100 or NA>).
parse_log_progress <- function(log_lines) {
  if (length(log_lines) == 0) return(list(text = "En attente...", pct = NA_real_))
  last <- utils::tail(log_lines, 1)
  m <- regmatches(last, regexec("\\[(\\d+)/(\\d+)\\]", last))[[1]]
  pct <- if (length(m) == 3) round(100 * as.numeric(m[2]) / as.numeric(m[3])) else NA_real_
  list(text = last, pct = pct)
}
#' Defensive guard: validate a QC pass_idx against the currently active dataset
#'
#' Root cause fixed: shared_rv$qc_pass_idx computed for a PREVIOUS dataset
#' could reach a NEW (smaller/different) dataset's BPCells matrix inside a
#' mirai daemon -> opaque "vctrs::vec_slice" OOB crash. Called on the MAIN
#' thread right before every async $invoke() consuming pass_idx.
#'
#' @param pass_idx Integer vector or NULL (shared_rv$qc_pass_idx), expected
#'   to carry "dataset"/"n_total" attributes set by mod_spatial_qc.R.
#' @param dataset_name Character, global_data$active_spatial_dataset.
#' @param n_total Integer, global_data$spatial_obj$n_total.
#' @return pass_idx unchanged if valid for this dataset, else NULL.
safe_pass_idx <- function(pass_idx, dataset_name, n_total) {
  if (is.null(pass_idx) || length(pass_idx) == 0) return(pass_idx)
  ds_tag <- attr(pass_idx, "dataset")
  if (!is.null(ds_tag) && !is.null(dataset_name) && !identical(ds_tag, dataset_name)) return(NULL)
  if (!is.null(n_total) && is.finite(n_total) &&
      (max(pass_idx, na.rm = TRUE) > n_total || min(pass_idx, na.rm = TRUE) < 1)) return(NULL)
  pass_idx
}

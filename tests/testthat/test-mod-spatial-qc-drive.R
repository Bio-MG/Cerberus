# =============================================================================
# test-mod-spatial-qc-drive.R â€” Drive live control for the Spatial local hotspot
# action `spatial-qc-btn_hotspots` (module `spatial_qc`).
# =============================================================================
# Phase E. `run_spatial_hotspots()` makes the same domain call as the human
# `eventReactive`, on the frozen `k` and the RULE-resolved metric.
# `R/spatial/spatial_stats.R` is untouched.
#
# THE CHOICE THIS FILE PINS, and it is the one that could most easily have been got
# wrong: `n_results` COUNTS SIGNIFICANT SPOTS, not rows of the result table. The
# table has one row per spatial element, so `nrow()` would report ~1000 for a run
# that found two hotspots. A count that does not mean what the field is called is
# the same failure the `empty` distinction exists to prevent, so a run with no
# significant spot is `empty` - a legitimate outcome, not an error.
#
# What this file has to prove, and why each one is here:
#   - the frozen set is CLOSED, and the deconvolution branch is out of reach;
#   - the metric is decided by a rule over a DECLARED order, and an undecidable
#     case returns a REASON so the poller can publish `not_ready`;
#   - a missing RANN is refused with `state = missing_dependency` before the
#     domain is reached;
#   - the state probe RECOUNTS the stored table, so a re-import cannot leave a
#     stale count behind;
#   - the human observer and the drive action move the SAME two slots.
# =============================================================================

source_project_file("config/defaults.R")
source_project_file("R/core/io_helpers.R")            # %||%
source_project_file("R/core/error_state.R")           # ts_error_state
source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")
source_project_file("R/spatial/spatial_stats.R")      # the domain
source_project_file("modules/spatial/mod_spatial_qc.R")  # the code under test

if (!exists(".tr", envir = globalenv()))
  assign(".tr", function(key) key, envir = globalenv())
if (!exists(".t_fmt", envir = globalenv()))
  assign(".t_fmt", function(template, ...) {
    vals <- list(...)
    for (nm in names(vals)) {
      template <- gsub(paste0("{", nm, "}"), format(vals[[nm]]), template, fixed = TRUE)
    }
    template
  }, envir = globalenv())

# --- Fixtures ----------------------------------------------------------------
# 40 spots on a grid, ids that are opaque so nothing biological is implied.
.spqc_coords <- function(n = 40L) {
  set.seed(909)
  data.frame(
    id = sprintf("SPOT%03d", seq_len(n)),
    x  = as.numeric(seq_len(n) %% 8L),
    y  = as.numeric(seq_len(n) %/% 8L),
    stringsAsFactors = FALSE
  )
}

# The QC table as the QC step writes it. `log_nCount` is a monotone copy of
# `nCount`, so the two are correlated and the preference order is testable.
.spqc_metrics <- function(n = 40L, n_hot = 0L) {
  set.seed(909)
  base <- stats::runif(n, 1, 10)
  # A block of genuinely elevated counts, so a real Getis-Ord finds something.
  if (n_hot > 0L) base[seq_len(n_hot)] <- base[seq_len(n_hot)] + 12
  data.frame(
    id        = sprintf("SPOT%03d", seq_len(n)),
    nCount    = base * 100,
    nFeature  = stats::runif(n, 500, 2000),
    pct_mt    = stats::runif(n, 1, 8),
    pct_ribo  = stats::runif(n, 10, 40),
    log_nCount = log1p(base * 100),
    row.names = sprintf("SPOT%03d", seq_len(n)),
    stringsAsFactors = FALSE
  )
}

# A Getis-Ord table shaped like the real one. `n_sig` is the number of NON-"NS"
# rows, which is what `n_results` must read - and it is deliberately different
# from the 40 rows.
.spqc_record <- function(n = 40L, n_sig = 6L) {
  hot <- rep("NS", n)
  hot[seq_len(n_sig)] <- c(rep("Hotspot (chaud)", ceiling(n_sig / 2)),
                           rep("Coldspot (froid)", floor(n_sig / 2)))[seq_len(n_sig)]
  data.frame(
    id      = sprintf("SPOT%03d", seq_len(n)),
    value   = stats::runif(n, 1, 10),
    gi_star = stats::rnorm(n, 0, 1),
    p_value = stats::runif(n, 0, 0.2),
    hotspot = hot,
    stringsAsFactors = FALSE
  )
}

# --- Global patching ---------------------------------------------------------
.spqc_patch <- function(name, value) {
  existed <- exists(name, envir = globalenv(), inherits = FALSE)
  old <- if (existed) get(name, envir = globalenv()) else NULL
  assign(name, value, envir = globalenv())
  list(value = old, existed = existed)
}

.spqc_restore <- function(saved) {
  for (nm in names(saved)) {
    e <- saved[[nm]]
    if (isTRUE(e$existed)) assign(nm, e$value, envir = globalenv())
    else if (exists(nm, envir = globalenv(), inherits = FALSE)) rm(list = nm, envir = globalenv())
  }
}

# The patch records MUST be keyed by the patched NAME: `.spqc_restore()` iterates
# over `names(saved)`, so a differently named list would leave the real function
# patched and silently poison every LATER test.
.spqc_mock_domain <- function(record = NULL, fail = NULL, seen = NULL) {
  out <- list()
  out[["compute_getis_ord_hotspots"]] <- .spqc_patch("compute_getis_ord_hotspots",
    function(coords, values, k_neighbors = 30L, ...) {
      if (!is.null(seen)) {
        seen$coords <- coords
        seen$values <- values
        seen$k <- k_neighbors
      }
      if (!is.null(fail)) stop(fail, call. = FALSE)
      if (!is.null(record)) return(record)
      .spqc_record(n = length(values))
    })
  out
}

# A server that reports ONLY `pkgs` as installed, so the refusal branch is
# reachable without touching this host's library.
.spqc_force_present <- function(pkgs) {
  .spqc_patch("requireNamespace", function(package, ..., quietly = FALSE) {
    package %in% pkgs
  })
}

.spqc_state <- function(with_coords = TRUE, with_qc = TRUE) {
  gd <- shiny::reactiveValues()
  if (with_coords) gd$spatial_obj <- list(coords = .spqc_coords())
  gd$language <- "fr"
  rv <- shiny::reactiveValues()
  if (with_qc) rv$qc_metrics <- .spqc_metrics()
  list(gd = gd, rv = rv)
}

# -----------------------------------------------------------------------------
test_that("the hotspot drive input set is frozen and excludes the deconvolution branch", {
  inputs <- .spatial_qc_drive_inputs()

  # The name set is pinned: this is what stops a session-derived parameter from
  # being added silently later.
  expect_setequal(names(inputs), c("hotspot_source", "hotspot_k", "hotspot_alpha"))
  expect_identical(inputs$hotspot_source, "qc")
  expect_identical(inputs$hotspot_k, 30L)
  expect_identical(inputs$hotspot_alpha, 0.05)

  # The metric is DELIBERATELY absent: it is resolved by rule, and freezing a
  # column name would either error or silently pick whatever the QC step wrote
  # first. The cell type of the deconvolution branch is absent for the same reason
  # and one more: that branch needs a prior deconvolution, which is not on the
  # drive surface, so a fallback to it would be a lie.
  expect_false(any(c("hotspot_qc_metric", "hotspot_deconv_celltype") %in% names(inputs)))

  # The frozen alpha MUST stay the domain's own 0.05: `compute_getis_ord_hotspots()`
  # hard-codes 1.96 / 0.05 in its classification, so a frozen value that disagreed
  # would be a false statement in the provenance block.
  expect_identical(inputs$hotspot_alpha, 0.05)
})

# -----------------------------------------------------------------------------
test_that("the metric is resolved by rule over a declared order", {
  # nCount wins: it is the first element of the order AND what the human
  # selectInput picks by default, so a drive run and a human click agree.
  # Deleting a reactiveValues FIELD also needs a reactive context, hence the
  # isolate() blocks: a bare `s$rv$qc_metrics$nCount <- NULL` throws
  # "Can't access reactive value ... outside of reactive consumer", and the test
  # would fail for a reason that has nothing to do with the rule.
  s <- .spqc_state()
  expect_identical(.spatial_hotspot_metric(s$rv)$metric, "nCount")

  # nCount absent -> the next declared metric, in order.
  shiny::isolate(s$rv$qc_metrics$nCount <- NULL)
  expect_identical(.spatial_hotspot_metric(s$rv)$metric, "log_nCount")

  # Only a metric OUTSIDE the declared order is left -> a REASON in its own field,
  # and it names no column. The reason is a length-1 character too, which is exactly
  # why the return type is a named field and not a bare string.
  shiny::isolate({
    s$rv$qc_metrics$log_nCount <- NULL
    s$rv$qc_metrics$nFeature <- NULL
    s$rv$qc_metrics$pct_mt <- NULL
    s$rv$qc_metrics$pct_ribo <- NULL
    s$rv$qc_metrics$other <- stats::runif(40)
  })
  out <- .spatial_hotspot_metric(s$rv)
  expect_null(out$metric)
  expect_match(as.character(out$reason), "no numeric QC metric", fixed = TRUE)

  # A declared name that is present but NOT numeric is skipped, not accepted: a
  # character column would fail deep inside the domain instead of here.
  s2 <- .spqc_state()
  shiny::isolate({
    s2$rv$qc_metrics$nCount <- as.character(s2$rv$qc_metrics$nCount)
    s2$rv$qc_metrics$log_nCount <- NULL
  })
  expect_identical(.spatial_hotspot_metric(s2$rv)$metric, "nFeature")

  # No QC table at all.
  s3 <- .spqc_state(with_qc = FALSE)
  expect_match(.spatial_hotspot_metric(s3$rv)$reason, "qc_metrics is NULL", fixed = TRUE)
})

# -----------------------------------------------------------------------------
test_that("readiness names each missing prerequisite instead of returning FALSE", {
  # no coordinates
  s <- .spqc_state(with_coords = FALSE)
  expect_match(.spatial_qc_drive_ready(s$rv, s$gd), "coords is NULL", fixed = TRUE)
  # coordinates without the columns the domain reads
  s <- .spqc_state()
  s$gd$spatial_obj <- list(coords = data.frame(id = sprintf("S%02d", 1:40)))
  expect_match(.spatial_qc_drive_ready(s$rv, s$gd), "coords is NULL", fixed = TRUE)
  # no usable metric
  s <- .spqc_state(with_qc = FALSE)
  expect_match(.spatial_qc_drive_ready(s$rv, s$gd), "qc_metrics is NULL", fixed = TRUE)
  # and the state probe publishes not_ready, not a false TRUE
  st <- .spatial_qc_drive_state(s$rv, s$gd, function() "done", "ran")
  expect_identical(st$status, "not_ready")
  expect_false(st$ready)
  # ready when everything is present
  s <- .spqc_state()
  expect_true(isTRUE(.spatial_qc_drive_ready(s$rv, s$gd)))
})

# -----------------------------------------------------------------------------
test_that("the drive action scores the spots on the frozen k and the resolved metric", {
  seen <- new.env(parent = emptyenv())
  saved <- .spqc_mock_domain(seen = seen)
  on.exit(.spqc_restore(saved), add = TRUE)

  s <- .spqc_state()
  closed <- character(0)
  res <- shiny::isolate(.spatial_qc_run_drive(s$gd, s$rv,
    function(status, error = NULL) closed <<- c(closed, status)))

  expect_identical(res$status, "done")
  expect_identical(res$step, "ran")
  expect_identical(closed, "done")
  # n_results is the OBSERVED significant count (6) while 40 elements were scored.
  expect_identical(res$n_results, 6L)
  expect_false(identical(res$n_results, 40L))

  # The frozen k reached the domain, and the metric is the one the rule chose.
  expect_identical(seen$k, 30L)
  expect_length(seen$values, 40L)
  expect_true(all(names(seen$values) == sprintf("SPOT%03d", 1:40)))
  # Only `rownames()`-shaped reads of reactiveValues are used elsewhere; the
  # comparison below must be wrapped or it throws outside a reactive consumer.
  expect_identical(unname(seen$values[[1L]]),
                   shiny::isolate(s$rv$qc_metrics)$nCount[[1L]])
  # The whole table is stored, and the parameters move with it.
  stored <- shiny::isolate(s$rv$hotspot_result)
  expect_true(is.data.frame(stored))
  expect_identical(nrow(stored), 40L)
  params <- shiny::isolate(s$rv$hotspot_params)
  expect_identical(params$metric, "nCount")
  expect_identical(params$source, "qc")
  expect_identical(params$k_neighbors, 30L)
})

# -----------------------------------------------------------------------------
test_that("a missing RANN is refused before the domain is reached", {
  seen <- new.env(parent = emptyenv())
  saved <- .spqc_mock_domain(seen = seen)
  saved[["requireNamespace"]] <- .spqc_force_present(character(0))
  on.exit(.spqc_restore(saved), add = TRUE)

  err <- tryCatch(run_spatial_hotspots(.spqc_coords(), .spqc_metrics()),
                  condition = function(e) e)
  expect_s3_class(err, "spatial_stats_error")
  expect_identical(ts_error_state(err), "missing_dependency")
  expect_match(conditionMessage(err), "RANN", fixed = TRUE)
  expect_match(conditionMessage(err), "nothing is downloaded", fixed = TRUE)
  expect_false(grepl("[A-Za-z]:[/\\\\]", conditionMessage(err)))
  # The scoring domain was never reached.
  expect_null(seen$coords)
})

# -----------------------------------------------------------------------------
test_that("the REAL domain call succeeds and the rule picks a usable metric", {
  # No mock: RANN is a small, pure, closed-form neighbour search, so running it
  # proves the wrapper composes with the REAL domain rather than only with a
  # stand-in. A run with a genuinely elevated block must find significant spots,
  # which is also the only way to see the `empty` branch from the other side.
  if (!requireNamespace("RANN", quietly = TRUE)) skip("RANN is not installed")
  res <- run_spatial_hotspots(.spqc_coords(), .spqc_metrics(n_hot = 12L))
  expect_true(isTRUE(res$ok))
  expect_true(is.data.frame(res$record))
  expect_identical(nrow(res$record), 40L)
  expect_identical(res$n_elements, 40L)
  expect_true(res$n_results > 0L)
  expect_identical(res$params$metric, "nCount")
  # The classification vocabulary is the domain's, and every spot is classified.
  expect_setequal(unique(res$record$hotspot),
                  c("Hotspot (chaud)", "Coldspot (froid)", "NS"))
})

# -----------------------------------------------------------------------------
test_that("a metric with no variation is the domain's own refusal, not a thin success", {
  # A constant column makes the domain's variance guard fire (S == 0). That is the
  # honest outcome, and the action must report it as an ERROR with no count, not as
  # an `empty` success: nothing was scored.
  if (!requireNamespace("RANN", quietly = TRUE)) skip("RANN is not installed")
  qc <- .spqc_metrics()
  qc$nCount <- 100
  err <- tryCatch(run_spatial_hotspots(.spqc_coords(), qc), condition = function(e) e)
  expect_s3_class(err, "spatial_stats_error")
  expect_match(conditionMessage(err), "Variance nulle", fixed = TRUE)
})

# -----------------------------------------------------------------------------
test_that("a failure publishes no count, even over a previous map", {
  s <- .spqc_state()
  s$rv$hotspot_result <- .spqc_record(n_sig = 6L)
  saved <- .spqc_mock_domain(fail = "simulated Getis-Ord failure")
  on.exit(.spqc_restore(saved), add = TRUE)

  closed <- list()
  res <- shiny::isolate(.spatial_qc_run_drive(s$gd, s$rv,
    function(status, error = NULL) closed[[1L]] <<- list(status = status, error = error)))

  expect_identical(res$status, "error")
  expect_identical(res$n_results, 0L)
  expect_identical(closed[[1L]]$status, "error")
  expect_true(nzchar(closed[[1L]]$error))
  # The state probe counts the STALE table, which is why the verdict must not carry
  # a count: the caller reads `status`, not a number.
  st <- .spatial_qc_drive_state(s$rv, s$gd, function() "error", "error")
  expect_identical(st$status, "error")
  expect_identical(st$steps$hotspots, "error")
})

# -----------------------------------------------------------------------------
test_that("no significant spot is `empty`, and its job still closes as done", {
  # The JOB vocabulary has no `empty`: done/error/invalid/timeout/session_lost. A
  # metric with no local cluster is a job that COMPLETED, and for a spatial metric
  # that is a real answer rather than a failure. Passing "empty" to
  # ts_drive_job_finish() would be REFUSED and the job would hang to its timeout.
  saved <- .spqc_mock_domain(record = .spqc_record(n = 40L, n_sig = 0L))
  on.exit(.spqc_restore(saved), add = TRUE)

  s <- .spqc_state()
  closed <- character(0)
  res <- shiny::isolate(.spatial_qc_run_drive(s$gd, s$rv,
    function(status, error = NULL) closed <<- c(closed, status)))
  expect_identical(res$status, "empty")
  expect_identical(res$n_results, 0L)
  expect_identical(closed, "done")
  # The table is STILL stored: `empty` is a verdict about significance, not about
  # the existence of the scored elements.
  expect_identical(nrow(shiny::isolate(s$rv$hotspot_result)), 40L)
})

# -----------------------------------------------------------------------------
test_that("the state probe recounts the stored table and never remembers a count", {
  s <- .spqc_state()
  s$rv$hotspot_result <- .spqc_record(n_sig = 6L)
  st <- .spatial_qc_drive_state(s$rv, s$gd, function() "done", "ran")
  expect_identical(st$n_results, 6L)
  expect_identical(st$steps$hotspots, "ran")
  expect_identical(st$module, "spatial_qc")
  expect_identical(st$action, "run_pipeline")

  # Re-importing wipes the slot: the count goes with it, so no stale number can
  # survive a new dataset. This is why the probe RECOUNTS instead of remembering.
  s$rv$hotspot_result <- NULL
  expect_identical(.spatial_qc_drive_state(s$rv, s$gd, function() "idle")$n_results, 0L)

  # A table with no `hotspot` column is counted as 0 rather than erroring.
  s$rv$hotspot_result <- data.frame(id = sprintf("S%02d", 1:40))
  expect_identical(.spatial_qc_drive_state(s$rv, s$gd, function() "done")$n_results, 0L)

  st2 <- .spatial_qc_drive_state(s$rv, s$gd, function() "done", "nonsense")
  expect_identical(st2$steps$hotspots, "error")
})

# -----------------------------------------------------------------------------
test_that("the token is published as a long job and no DOM button is bound", {
  src <- readLines(file.path(ts_project_root(), "modules", "spatial", "mod_spatial_qc.R"),
                   warn = FALSE, encoding = "UTF-8")
  expect_length(grep('ts_drive_publish_token\\(.*"spatial-qc-btn_hotspots"', src), 1L)
  expect_length(grep('ts_drive_bind_button\\(.*"spatial-qc-btn_hotspots"', src), 0L)
  expect_identical(TS_DRIVE_SPATIAL_QC_BUTTON, "spatial-qc-btn_hotspots")
  expect_identical(TS_DRIVE_SPATIAL_QC_MODULE, "spatial_qc")
  expect_identical(.SPATIAL_QC_DRIVE_BUTTON, TS_DRIVE_SPATIAL_QC_BUTTON)
  expect_identical(.SPATIAL_QC_DRIVE_MODULE, TS_DRIVE_SPATIAL_QC_MODULE)
  expect_true(TS_DRIVE_SPATIAL_QC_BUTTON %in% TS_DRIVE_BUTTONS)
  expect_true(TS_DRIVE_SPATIAL_QC_MODULE %in% TS_DRIVE_MODULES)

  # The real registry: long = TRUE is what gives `running` a producer, since the
  # job is synchronous.
  gd <- shiny::reactiveValues()
  gd$drive_registry <- new.env(parent = emptyenv())
  counter <- shiny::reactiveVal(0L)
  expect_true(ts_drive_publish_token(gd, TS_DRIVE_SPATIAL_QC_BUTTON, counter,
                                     ready = function() TRUE, long = TRUE))
  entry <- shiny::isolate(gd$drive_registry)[[TS_DRIVE_SPATIAL_QC_BUTTON]]
  expect_true(isTRUE(entry$long))
  expect_identical(ts_drive_token_of(gd, TS_DRIVE_SPATIAL_QC_BUTTON), counter)

  ts_drive_job_clear()
  on.exit(try(ts_drive_job_clear(), silent = TRUE), add = TRUE)
  job <- ts_drive_job_begin(1L, "spatial_qc", "run_pipeline",
                            TS_DRIVE_SPATIAL_QC_BUTTON)
  expect_true(is.list(job))
  expect_identical(job$status, "running")
  expect_false(ts_drive_job_finish("bulk-de-run_de", status = "done", job_id = job$job_id))
  expect_true(ts_drive_job_finish(TS_DRIVE_SPATIAL_QC_BUTTON, status = "done",
                                  job_id = job$job_id))
  expect_identical(ts_drive_job_pending()$status, "done")
  ts_drive_job_clear()
  expect_false(isTRUE(ts_drive_job_busy()))
})

# -----------------------------------------------------------------------------
# THE READER-SIDE CONTRACT, learned the hard way on 2026-09-25: the signature slot
# broke because the two writers disagreed and the test agreed with the writer.
# Here both triggers are executed and the slots they moved are compared.
# -----------------------------------------------------------------------------
test_that("the human observer and the drive action move the SAME two slots", {
  saved <- .spqc_mock_domain(record = .spqc_record(n_sig = 6L))
  saved[["renderDT"]]   <- .spqc_patch("renderDT", function(...) NULL)
  saved[["nav_select"]] <- .spqc_patch("nav_select", function(...) invisible(TRUE))
  # The module's observers log through `spatial_log_path()` and build a progress
  # tracker through `create_reactive_tracker()`, both from
  # R/spatial/spatial_async.R. This file does not source that file: the MIRROR
  # ENGINE is not what these tests are about, so both are stubbed to a throwaway
  # path and a no-op tracker rather than pulled in.
  # The module's Moran's I observer builds a progress tracker at init through
  # `create_reactive_tracker()` (R/spatial/spatial_async.R), which this file does
  # not source: it belongs to the MIRROR ENGINE and has no bearing on the action.
  # It is stubbed with the only shape the module uses - a zero-argument function
  # returning the log lines - rather than pulling the whole async file in.
  saved[["create_reactive_tracker"]] <- .spqc_patch("create_reactive_tracker",
    function(session, log_file, interval_ms = 1000) function() character(0))
  saved[["spatial_log_path"]] <- .spqc_patch("spatial_log_path",
    function(...) tempfile(fileext = ".log"))
  on.exit(.spqc_restore(saved), add = TRUE)

  # --- 1. the HUMAN path, through the module's own observer ------------------
  h <- .spqc_state()
  shiny::testServer(mod_spatial_qc_server,
                    args = list(global_data = h$gd, shared_rv = h$rv), {
                      session$setInputs(btn_hotspots = 1, hotspot_source = "qc",
                                        hotspot_qc_metric = "nCount", hotspot_k = 30)
                      session$flushReact()
                    })
  human_result <- shiny::isolate(h$rv$hotspot_result)
  human_params <- shiny::isolate(h$rv$hotspot_params)

  # --- 2. the DRIVE path, on its own state -----------------------------------
  d <- .spqc_state()
  shiny::isolate(.spatial_qc_run_drive(d$gd, d$rv, function(status, error = NULL) NULL))
  driven_result <- shiny::isolate(d$rv$hotspot_result)
  driven_params <- shiny::isolate(d$rv$hotspot_params)

  expect_true(is.data.frame(human_result))
  expect_true(is.data.frame(driven_result))
  expect_setequal(names(human_result), names(driven_result))
  expect_identical(nrow(human_result), nrow(driven_result))

  # The status panel reads BOTH slots and counts by CLASS, so both must move.
  expect_identical(human_params$metric, "nCount")
  expect_identical(driven_params$metric, "nCount")
  expect_setequal(names(human_params), names(driven_params))
  expect_setequal(unique(human_result$hotspot), unique(driven_result$hotspot))

  # The exact expressions the status panel and the map evaluate.
  for (obj in list(human_result, driven_result)) {
    expect_true(all(c("id", "value", "gi_star", "p_value", "hotspot") %in% names(obj)))
    expect_true(is.character(obj$hotspot))
  }
  expect_identical(sum(human_result$hotspot != "NS"), sum(driven_result$hotspot != "NS"))
})

# -----------------------------------------------------------------------------
test_that("the shared writer refuses anything that is not a table, and validates the metric", {
  rv <- shiny::reactiveValues()
  # A bare matrix: the shape a careless writer would produce from the run's value.
  err <- tryCatch(spatial_hotspot_store(matrix(1, 2, 2), list(metric = "nCount"), rv),
                  condition = function(e) e)
  expect_s3_class(err, "spatial_stats_error")
  expect_identical(ts_error_state(err), "invalid_input")
  expect_null(shiny::isolate(rv$hotspot_result))

  # Parameters without exactly one metric are refused, BEFORE anything is written:
  # a half-written pair is what an empty params block looks like to a reader.
  err2 <- tryCatch(spatial_hotspot_store(.spqc_record(), list(), rv),
                   condition = function(e) e)
  expect_s3_class(err2, "spatial_stats_error")
  expect_null(shiny::isolate(rv$hotspot_params))

  # A real table with a real metric moves both slots.
  res <- .spqc_record(n_sig = 3L)
  spatial_hotspot_store(res, list(source = "qc", metric = "nCount", k_neighbors = 30L), rv)
  expect_identical(shiny::isolate(rv$hotspot_result), res)
  expect_identical(shiny::isolate(rv$hotspot_params)$metric, "nCount")
})

# =============================================================================
# S1 â€” the hotspot READERS. Two defects, one signature.
#
# D1 (reader source divergence). `output$hotspot_map` read `hotspot_result()`,
#     an `eventReactive(input$btn_hotspots, ...)` declared at
#     mod_spatial_qc.R:781. The drive action dispatches `spqc_drive_counter()`
#     (:850, observer :864) and the Spatial pipeline writes
#     `shared_rv$hotspot_result` directly (mod_spatial_pipeline.R:702) â€” NEITHER
#     ever sets `input$btn_hotspots`, so the map could not re-evaluate after a
#     driven run or a pipeline run. Its three siblings read the `reactiveVal`
#     (:880, :913, :942) and did render. MEASURED on a live session: before the
#     fix the map stayed empty while the histogram, the table and the status chip
#     all carried real content.
#
# D2 (navigation gate). The module's own results `navset_card_underline()` at
#     mod_spatial_qc.R:396 declared NO `id` and NO `selected`, so bslib showed
#     panel 1 ("overview", :397) and the hotspots panel (:424) was 4th of 4 with
#     nothing â€” human or drive â€” able to select it. Under Shiny's default
#     `suspendWhenHidden = TRUE` that kept all three panel outputs suspended and
#     the download button `disabled`.
#
# WHY THE ASSERTION BELOW IS SHAPED THE WAY IT IS. It seeds the STORE and never
# touches the button. A human click satisfies the eventReactive, which is exactly
# why the human path was never the failing one and why a test that clicked the
# button would have passed against the broken reader. The drive and the pipeline
# cannot click it, so "renders from the store alone" is the property that
# separates a working reader from a decorative one.
# =============================================================================

.spqc_reader_patches <- function() {
  saved <- .spqc_mock_domain(record = .spqc_record(n_sig = 6L))
  saved[["nav_select"]] <- .spqc_patch("nav_select", function(...) invisible(TRUE))
  # The Moran's I observer builds a tracker from R/spatial/spatial_async.R, which
  # this file does not source (see the identical comment in the block above).
  saved[["create_reactive_tracker"]] <- .spqc_patch("create_reactive_tracker",
    function(session, log_file, interval_ms = 1000) function() character(0))
  saved[["spatial_log_path"]] <- .spqc_patch("spatial_log_path",
    function(...) tempfile(fileext = ".log"))
  # The map's own theme call. Stubbed rather than sourced: this block is about
  # WHICH SOURCE the reader binds, not about how the plot is styled. An identity
  # theme keeps the returned object a plain ggplot, so the assertions can read
  # its data instead of its appearance.
  saved[["ts_theme"]] <- .spqc_patch("ts_theme", function(...) ggplot2::theme_minimal())
  saved
}

# WHY THE READ IS INLINED EVERYWHERE. `output` exists ONLY inside the evaluation
# block `testServer()` hands to its expression, so a reader defined at file level
# cannot see it â€” it fails with "object 'output' not found", which says nothing
# about the app. The READ is therefore repeated (three lines, three times) and the
# two pure helpers below are shared. Duplicating a lookup beats hiding a real
# defect behind a scoping mistake.
#
# Inside `testServer` a `renderPlot` hands back the RENDERED FRAME (a list with
# `src`/`width`/`height`), not the ggplot it was given â€” so the plot object cannot
# be inspected with `ggplot_build()` here. The frame's `src` is the encoded image
# itself, which turns out to be a BETTER probe than the object would have been:
# two different tables encode to two different images, so "the reader
# re-evaluated" is decidable by comparing bytes rather than by trusting a count.
.spqc_frame <- function(plot) {
  if (inherits(plot, "spqc_unreadable_output") || is.null(plot)) return(NA_character_)
  if (!is.list(plot) || is.null(plot$src)) return(NA_character_)
  as.character(plot$src)
}

# A red assertion that cannot say WHY is a red assertion that has to be
# re-debugged. The reader's own message travels with it.
.spqc_why <- function(plot) {
  if (inherits(plot, "spqc_unreadable_output")) {
    m <- plot$msg
    if (is.null(m) || !nzchar(m)) {
      "<shiny.silent.error: the render declined without a message, which is what req() does>"
    } else m
  } else if (is.null(plot)) "the output was NULL" else "the output was a rendered frame"
}

# The sentinel constructor, so the four inline reads stay one line each.
.spqc_unreadable <- function(e) {
  structure(list(msg = conditionMessage(e)), class = "spqc_unreadable_output")
}

test_that("D1: the hotspot map renders from the STORE, with the button never set", {
  saved <- .spqc_reader_patches()
  on.exit(.spqc_restore(saved), add = TRUE)

  st <- .spqc_state()
  # The store already holds a result â€” which is what the drive action writes at
  # :300 and what the pipeline writes at mod_spatial_pipeline.R:702. No
  # `btn_hotspots` is set anywhere below: that is the whole point.
  shiny::isolate(st$rv$hotspot_result <- .spqc_record(n_sig = 6L))

  shiny::testServer(mod_spatial_qc_server,
                    args = list(global_data = st$gd, shared_rv = st$rv), {
                      session$flushReact()
                      map <- tryCatch(output$hotspot_map, error = .spqc_unreadable)
                      expect_false(inherits(map, "spqc_unreadable_output"),
                                   info = paste("the map reader produced no value from the",
                                                "store alone; no btn_hotspots was ever set.",
                                                "reader said:", .spqc_why(map)))
                      # Not merely "something": an actual encoded frame.
                      expect_true(nchar(.spqc_frame(map)) > 0L,
                                  info = .spqc_why(map))
                    })
})

test_that("D1: the map RE-EVALUATES when the store changes, so it is bound to the slot", {
  saved <- .spqc_reader_patches()
  on.exit(.spqc_restore(saved), add = TRUE)

  st <- .spqc_state()
  shiny::isolate(st$rv$hotspot_result <- .spqc_record(n = 40L, n_sig = 4L))

  shiny::testServer(mod_spatial_qc_server,
                    args = list(global_data = st$gd, shared_rv = st$rv), {
                      session$flushReact()
                      first <- tryCatch(output$hotspot_map, error = .spqc_unreadable)
                      expect_true(nchar(.spqc_frame(first)) > 0L, info = .spqc_why(first))

                      # A second, DIFFERENT table in the same store. If the reader
                      # were still cached behind the eventReactive this would be
                      # the stale first frame, which IS the defect. The store is
                      # reached through the closure, not a bare `s`: there is no
                      # such object inside `testServer`.
                      shiny::isolate({
                        st$rv$hotspot_result <- .spqc_record(n = 25L, n_sig = 9L)
                      })
                      session$flushReact()
                      second <- tryCatch(output$hotspot_map, error = .spqc_unreadable)
                      expect_true(nchar(.spqc_frame(second)) > 0L, info = .spqc_why(second))
                      # Different bytes = a different frame = it really re-ran.
                      expect_false(identical(.spqc_frame(first), .spqc_frame(second)))
                    })
})

test_that("D1: an EMPTY store leaves the map empty instead of drawing a stale frame", {
  saved <- .spqc_reader_patches()
  on.exit(.spqc_restore(saved), add = TRUE)

  st <- .spqc_state()
  shiny::isolate(st$rv$hotspot_result <- .spqc_record(n_sig = 6L))

  shiny::testServer(mod_spatial_qc_server,
                    args = list(global_data = st$gd, shared_rv = st$rv), {
                      session$flushReact()
                      first <- tryCatch(output$hotspot_map, error = .spqc_unreadable)
                      expect_true(nchar(.spqc_frame(first)) > 0L, info = .spqc_why(first))
                      shiny::isolate({ st$rv$hotspot_result <- NULL })
                      session$flushReact()
                      cleared <- tryCatch(output$hotspot_map, error = .spqc_unreadable)
                      # Same guard the three siblings already use. A reader that
                      # kept drawing would be showing a result the app no longer
                      # holds, which is the "no stale success survives" rule.
                      expect_true(is.null(cleared) || inherits(cleared, "spqc_unreadable_output"),
                                  info = paste("a cleared store must clear the frame, but the",
                                               "reader still produced:", .spqc_why(cleared)))
                    })
})

test_that("D1: a HUMAN click still computes, even with no reader of the eventReactive", {
  # THE REGRESSION THIS BLOCK EXISTS FOR. `hotspot_result` is a lazy
  # `eventReactive`, and before S1 its only dependent in the whole module was
  # `output$hotspot_map`. Re-pointing the map at the store â€” the right fix for
  # the drive, which cannot click a button â€” removed that dependent, and a Shiny
  # reactive with no dependents is NEVER EVALUATED. The human click would
  # invalidate an eventReactive nobody read, and the store would stay empty.
  #
  # This is asserted against the HUMAN path, with the button, because that is the
  # path the regression lives on: a drive-only test would not have caught it.
  saved <- .spqc_reader_patches()
  on.exit(.spqc_restore(saved), add = TRUE)

  st <- .spqc_state(with_qc = TRUE)
  shiny::testServer(mod_spatial_qc_server,
                    args = list(global_data = st$gd, shared_rv = st$rv), {
                      session$flushReact()
                      expect_null(shiny::isolate(st$rv$hotspot_result))
                      session$setInputs(btn_hotspots = 1, hotspot_source = "qc",
                                        hotspot_qc_metric = "nCount", hotspot_k = 30)
                      session$flushReact()
                      res <- shiny::isolate(st$rv$hotspot_result)
                      expect_true(is.data.frame(res))
                      expect_identical(nrow(res), 40L)
                      expect_true(all(c("id", "value", "gi_star", "p_value",
                                        "hotspot") %in% names(res)))
                    })
})

test_that("D2: the hotspots results navset has a STABLE id, and a hotspots panel in it", {
  # The module UI is built from bslib's page builders and resolves two free
  # symbols at CALL time (`i18n` and `.tr_plain`, both defined in global.R, which
  # this file does not source). Both are stubbed for the render and restored.
  # Stubbing the translator is what lets the assertion be on ids and structure
  # rather than on a translated title.
  #
  # NOT `.spqc_patch()` here: that helper returns `list(value = old, existed = )`,
  # and `list(value = NULL, existed = FALSE)` DROPS the `value` element entirely
  # (the same `NULL`-assignment trap the drive code documents). The restore then
  # iterates an atomic element. `assign`/`exists` written out is the fix.
  library(bslib)
  stubbed <- c("i18n", ".tr_plain")
  before <- stats::setNames(
    vapply(stubbed, exists, logical(1), envir = globalenv(), inherits = FALSE),
    stubbed)
  # Only fetch what is actually there: `get(..., inherits = FALSE)` on an absent
  # name is itself an error, which is the whole reason this is not `.spqc_patch()`.
  old <- lapply(stubbed, function(nm) {
    if (isTRUE(before[[nm]])) get(nm, envir = globalenv(), inherits = FALSE) else NULL
  })
  names(old) <- stubbed
  assign("i18n", list(t = function(x) x), envir = globalenv())
  assign(".tr_plain", function(key) key, envir = globalenv())
  on.exit({
    for (nm in stubbed) {
      if (isTRUE(before[[nm]])) assign(nm, old[[nm]], envir = globalenv())
      else if (exists(nm, envir = globalenv(), inherits = FALSE)) rm(list = nm, envir = globalenv())
    }
  }, add = TRUE)

  html <- as.character(mod_spatial_qc_ui("spatial-qc"))
  # The id is what `ts_drive_perform_nav()` addresses, and what a human click is
  # recorded against. Without it the panel is reachable by neither.
  expect_match(html, 'id="spatial-qc-qc_results"', fixed = TRUE,
               info = "the inner results navset must carry a stable, namespaced id")
  # The four outputs that live INSIDE that panel, so the id is proven to be on the
  # navset that hosts them and not on some other element. A `value="hotspots"`
  # assertion is deliberately NOT used: bslib consumes `value=` to build the tab
  # list rather than emitting it as an HTML attribute, so its absence from the
  # rendered string says nothing either way.
  for (oid in c("hotspot_map", "hotspot_hist", "hotspot_table", "dl_hotspot_csv")) {
    expect_match(html, sprintf('id="spatial-qc-%s"', oid), fixed = TRUE)
  }

  # And the id is declared as DATA, not spelled twice: the nav plan and the UI
  # must not be able to drift apart. `get0()` rather than a bare name: in the
  # broken state the constant does not exist, and a bare reference would raise
  # "object not found" â€” an ERROR that says nothing about the missing
  # capability. One clean failure is the whole point of a red test.
  sub_id <- get0("TS_DRIVE_SPATIAL_QC_SUB_TABS_ID", envir = globalenv())
  sub_tab <- get0("TS_DRIVE_SPATIAL_QC_SUB_TAB", envir = globalenv())
  expect_true(is.character(sub_id),
              info = "the navset id must be a declared constant, not a literal in the watcher")
  expect_true(is.character(sub_tab),
              info = "the hotspots panel value must be a declared constant too")
  if (!is.character(sub_id) || !is.character(sub_tab)) return()
  expect_match(html, sub_id, fixed = TRUE)
  expect_identical(sub_tab, "hotspots")
})

# =============================================================================
# S1.5 â€” ONE writer for the hotspot slots.
#
# MEASURED, not assumed. Both callers pass the output of the same domain function
# (`compute_getis_ord_hotspots()`), so the scientific payload is byte-identical:
#   40 x 5, columns id/value/gi_star/p_value/hotspot, and the same significant
#   count. The WHOLE divergence is in the GUARD. Measured on this host:
#
#   input                      spatial_hotspot_store()      direct assignment
#   result is a matrix         REFUSED spatial_stats_error   STORED (rows = 2)
#   params carry no metric     REFUSED spatial_stats_error   STORED
#   metric is NA               REFUSED spatial_stats_error   STORED (metric = NA)
#   metric has length 2        REFUSED spatial_stats_error   STORED (metric = a/b)
#
# plus a write-ORDER difference (params then result, versus result then params).
#
# WHY THE MATRIX ROW IS THE ONE THAT MATTERS. Three readers
# (`hotspot_status_ui`, `hotspot_hist`, `hotspot_table`), the drive state probe
# and the CSV export all treat the slot as a data.frame with those five columns.
# `hotspot_table` does `formatRound(c("value","gi_star","p_value"), 3)` and the
# status panel does `res$hotspot == "Hotspot (chaud)"`. A matrix in that slot
# satisfies neither, and the store's guard is the only thing currently keeping it
# out. That is a reader-breaking value reaching three readers at once, not a style
# difference.
#
# WHY THE METRIC IS NOT "HARMONISED". The two defaults DIFFER BY DECLARATION, and
# both are deliberate in their own module:
#   - `mod_spatial_qc.R:383` â€” the metric `selectInput` passes NO `selected`, so the
#     control defaults to its first choice, and `.spatial_hotspot_metric_order()`
#     puts `nCount` first precisely so a drive run and a human click agree.
#   - `mod_spatial_pipeline.R:228` â€” the metric `selectInput` passes
#     `selected = "log_nCount"` EXPLICITLY, mirrored at :313 and at the `%||%`
#     fallback on :680.
# So this slice UNIFIES THE WRITER and PRESERVES EACH CALLER'S DECLARED METRIC.
# Making the two agree would be a scientific default change, and it is not this
# slice's to make. The blocks below PIN the difference so it cannot be quietly
# erased by a later refactor.
# =============================================================================

# WHY A RESOLVER. In the pre-fix state the shared writer does not exist under its
# public name, and a bare reference raises "object not found" — an ERROR, which
# reads as "this test is broken" rather than "the capability is missing". One
# clean failure per block is the whole point of a red test.
.spqc_writer <- function() get0("spatial_hotspot_store", envir = globalenv())
.spqc_needs_writer <- function() {
  w <- .spqc_writer()
  if (!is.function(w)) {
    testthat::expect_true(is.function(w),
      info = "spatial_hotspot_store() must exist as a module-level function both callers can reach")
    return(NULL)
  }
  w
}
# A store seam that RECORDS the order of its `$<-` assignments, so the write
# order of the shared writer is a measurable fact rather than an inference from
# reading the source. `$<-` dispatches on class, so this is the one shape that
# works; a plain list and a plain environment both turned out to be incapable of
# observing the order (see the block that uses it for the full account).
# Registered with `registerS3method()`, and BOTH details are load-bearing:
#
#   * into `asNamespace("base")` — `$<-` is an INTERNAL generic, and its dispatch
#     does not consult the caller's environment the way an ordinary S3 generic
#     does. Measured: a plain `assign()` into `globalenv()` leaves the method
#     invisible to the writer, and every assignment falls through to the default
#     list behaviour — no record, no error.
#   * `registerS3method()` rather than `getS3method()` alone — the latter reads
#     the table without adding to it.
#
# That invisibility is why THREE earlier versions of the order assertion were
# vacuous: a plain list (R copies on assignment), an environment (`names()` returns
# bindings SORTED, not inserted), and an unregistered method (never dispatched).
# All three were caught by the falsification harness reordering the two writes and
# watching the test stay green. A test that cannot fail is worse than no test,
# because it is counted as coverage.
.spqc_recorder <- function() {
  env <- new.env(parent = emptyenv())
  env$order <- character(0)
  env$values <- list()
  structure(list(), class = "spqc_recorder", .rec = env)
}

local({
  fn <- function(x, name, value) {
    env <- attr(x, ".rec")
    env$order <- c(env$order, name)
    env$values[[name]] <- value
    x
  }
  registerS3method("$<-", "spqc_recorder", fn, envir = asNamespace("base"))
})

# Every `<file>:<line>` in the app that ASSIGNS either hotspot slot.
#
# The pattern is anchored on the ASSIGNMENT TARGET (`$hotspot_result <-`), not on
# "the line mentions hotspot_result and also contains a `$`". The looser form was
# tried first and was WRONG: `hotspot_result <- eventReactive(input$btn_hotspots, {`
# is a LOCAL binding that matches any `$`-heuristic, because `input$` appears later
# on the same line. A guard that cannot tell a slot write from a local variable is
# a guard that reports the wrong population — the failure mode this file exists to
# avoid, and it bit the probe rather than the code.
#
# `Sys.glob()` is relative to the WORKING DIRECTORY, and testthat runs a test file
# with the wd at `tests/testthat`, so a bare glob silently returns nothing and this
# scan would "pass" by finding zero writers — a false negative in the other
# direction. The root comes from the helper instead.
.spqc_slot_writers <- function() {
  root <- ts_project_root()
  files <- c(Sys.glob(file.path(root, "R", "*.R")),
             Sys.glob(file.path(root, "R", "*", "*.R")),
             Sys.glob(file.path(root, "modules", "*.R")),
             Sys.glob(file.path(root, "modules", "*", "*.R")),
             Sys.glob(file.path(root, "modules", "*", "*", "*.R")))
  # Reported RELATIVE to the repository root, so the expectation below is
  # independent of where the suite happens to run from.
  files <- gsub("\\\\", "/", files, fixed = TRUE)
  # Strip the root by LENGTH, not by regex. Two regex attempts failed here: the
  # escaped-root pattern did not match, and the follow-up `strsplit(x, ":")` split
  # a WINDOWS DRIVE LETTER off the front of every path (`D:/...` -> `"D"`). A
  # structured return value removes the whole class of bug: nothing is parsed back.
  root_n <- nchar(gsub("\\", "/", root, fixed = TRUE))
  files <- substring(files, root_n + 2L)
  out <- list()
  for (f in unique(files)) {
    full <- file.path(root, f)
    if (!file.exists(full)) next
    txt <- readLines(full, warn = FALSE)
    for (i in seq_along(txt)) {
      if (grepl("^\\s*#", txt[i])) next
      m <- regmatches(txt[i], regexpr("\\$hotspot_(result|params)\\s*<-", txt[i]))
      if (!length(m) || !nzchar(m)) next
      out[[length(out) + 1L]] <- list(
        file = f, line = i,
        slot = sub("\\s*<-$", "", sub("^\\$hotspot_", "", m)))
    }
  }
  out
}

test_that("S1.5: the hotspot slots have exactly ONE writer in the whole app", {
  writers <- .spqc_slot_writers()
  # The expectation is not "a small number": it is that every assignment lives
  # inside ONE function body, in ONE file. Before S1.5 there were four assignments
  # in two files — the writer's pair, plus the pipeline's inlined pair.
  expect_length(writers, 2L)
  files <- vapply(writers, function(w) w$file, "")
  lines <- vapply(writers, function(w) w$line, 1L)
  slots <- vapply(writers, function(w) w$slot, "")
  expect_identical(unique(files), "modules/spatial/mod_spatial_qc.R")
  # The two are in the SAME BLOCK: the `params` write is the last statement of the
  # `if (!is.null(params))` guard and the `result` write is the first statement
  # after it, so the closing brace sits between them and the measured gap is 2, not
  # 1. An earlier expectation of exactly 1 was wrong about the writer's own shape.
  # A third assignment anywhere else — or the pipeline's inlined pair coming back —
  # breaks the length or the file assertion above.
  #
  # `max(diff(...))`, not `diff(...)`: with four writers the gaps are a VECTOR, and
  # `expect_lte()` on a length-3 object raises "Result of comparison must be
  # TRUE, FALSE, or NA" — an ERROR, which reads as a broken test rather than as the
  # extra writers it was written to catch.
  expect_lte(max(diff(sort(lines))), 2L)
  # Named explicitly, because "2 in 1 file" would also be satisfied by one caller
  # writing the same slot twice and never touching the other.
  expect_identical(sort(slots), c("params", "result"))
})

test_that("S1.5: the shared writer is named as PUBLIC, because two modules own it", {
  # A dot-prefixed name claims a privacy that stopped being true the moment a
  # second module started writing the same slots. The name is part of the fix, and
  # this is the assertion that keeps it from drifting back.
  expect_true(exists("spatial_hotspot_store", envir = globalenv()))
  expect_false(exists(".spatial_hotspot_store", envir = globalenv()))
  w <- .spqc_needs_writer()
  if (is.null(w)) return()
  # Still module level, not a server closure: that is what lets a second module
  # and an offline test reach it without a live session. Measured, and the reason
  # NO extraction into R/ was needed.
  expect_identical(environment(w), globalenv())
})

test_that("S1.5: BOTH callers are refused the same bad values, through the writer", {
  # The pipeline's params, verbatim from mod_spatial_pipeline.R:703.
  pipeline_params <- list(source = "qc", metric = "log_nCount", k_neighbors = 30)
  # The QC drive's params, from run_spatial_hotspots().
  qc_params <- list(source = "qc", metric = "nCount", k_neighbors = 30L)

  w <- .spqc_needs_writer()
  if (is.null(w)) return()
  for (nm in c("qc", "pipeline")) {
    params <- if (nm == "qc") qc_params else pipeline_params
    for (bad in list(
      list(what = "a matrix result", res = matrix(1, 2, 2), params = params),
      list(what = "no metric in params", res = .spqc_record(), params = list(source = "qc")),
      list(what = "an NA metric", res = .spqc_record(), params = list(metric = NA)),
      list(what = "a length-2 metric", res = .spqc_record(),
           params = list(metric = c("nCount", "log_nCount")))
    )) {
      rv <- shiny::reactiveValues()
      e <- tryCatch(w(bad$res, bad$params, rv),
                    condition = function(e) e)
      expect_s3_class(e, "spatial_stats_error")
      expect_identical(ts_error_state(e), "invalid_input")
      # Nothing half-written: the store's whole point is that the pair moves
      # together or not at all.
      expect_null(shiny::isolate(rv$hotspot_result))
      expect_null(shiny::isolate(rv$hotspot_params))
      expect_true(nzchar(bad$what))
    }
  }
})

test_that("S1.5: each caller KEEPS its own declared metric, and the shape is identical", {
  # The anti-harmonisation lock. If a later refactor makes both callers agree, this
  # goes red and the change has to be argued as a scientific decision rather than
  # slipped in as a side effect of unifying the writer.
  w <- .spqc_needs_writer()
  if (is.null(w)) return()
  rv_qc <- shiny::reactiveValues(); rv_pl <- shiny::reactiveValues()
  rec <- .spqc_record(n = 40L, n_sig = 6L)
  w(rec, list(source = "qc", metric = "nCount", k_neighbors = 30L), rv_qc)
  w(rec, list(source = "qc", metric = "log_nCount", k_neighbors = 30), rv_pl)

  expect_identical(shiny::isolate(rv_qc$hotspot_params$metric), "nCount")
  expect_identical(shiny::isolate(rv_pl$hotspot_params$metric), "log_nCount")
  # Same result table, same params NAMES, so every reader and the export see one
  # shape whichever module produced it.
  expect_identical(shiny::isolate(rv_qc$hotspot_result), shiny::isolate(rv_pl$hotspot_result))
  expect_identical(names(shiny::isolate(rv_qc$hotspot_params)),
                   names(shiny::isolate(rv_pl$hotspot_params)))
  expect_identical(names(shiny::isolate(rv_qc$hotspot_result)),
                   c("id", "value", "gi_star", "p_value", "hotspot"))
})

test_that("S1.5: the pipeline's DECLARED default metric is still log_nCount, in the source", {
  # The block above pins that the writer PASSES A METRIC THROUGH. It does not pin
  # WHICH metric the pipeline declares, so it would stay green through a change that
  # quietly harmonised `log_nCount` into the QC rule's `nCount` — exactly the
  # scientific default change this slice was told not to make, arriving as a side
  # effect of unifying the writer. This block is the guard for that.
  #
  # The default is declared in THREE places and all three must agree, because a
  # divergence between them is a real defect: the `selectInput(selected=)` the user
  # sees, the `updateSelectInput(selected=)` that re-imposes it, and the `%||%`
  # fallback that applies BEFORE the control is ever populated.
  src <- readLines(file.path(ts_project_root(), "modules", "spatial", "mod_spatial_pipeline.R"),
                   warn = FALSE)
  body <- paste(src, collapse = "\n")

  # (1) the control's own declaration. `[\s\S]{0,200}?` and NOT `[^)]*`: the call
  # spans three lines and contains `i18n$t("Metrique")`, whose closing parenthesis
  # stops a `[^)]*` match dead one line too early. That is the third source-lock
  # regex in this session to assume one line where the code has three — after
  # `Sys.glob` resolving against the wrong directory and `strsplit` splitting a
  # drive letter. A source lock that silently matches nothing is worse than none.
  expect_match(body, 'selectInput\\(ns\\("hotspot_metric"\\)[\\s\\S]{0,200}?selected = "log_nCount"',
               perl = TRUE)
  # (2) the server-side re-declaration
  expect_match(body, 'updateSelectInput\\(session, "hotspot_metric"[\\s\\S]{0,200}?selected = input\\$hotspot_metric %\\|\\|% "log_nCount"',
               perl = TRUE)
  # (3) the fallback used before the control exists
  expect_match(body, 'metric <- input\\$hotspot_metric %\\|\\|% "log_nCount"', perl = TRUE)
  # And NOT the QC rule's metric: if `nCount` ever appears as this module's
  # default, this is the assertion that says so out loud.
  expect_false(grepl('metric <- input\\$hotspot_metric %\\|\\|% "nCount"', body, perl = TRUE))
  # The QC module's own rule, for contrast: it declares NO `selected`, so its
  # control defaults to the first choice and the rule mirrors that.
  expect_match(paste(readLines(file.path(ts_project_root(), "modules", "spatial",
                                        "mod_spatial_qc.R"), warn = FALSE), collapse = "\n"),
               'c\\("nCount", "log_nCount", "nFeature", "pct_mt", "pct_ribo"\\)', perl = TRUE)
})

test_that("S1.5: the writer writes PARAMS BEFORE RESULT, so no reader sees a bare result", {
  # Measured difference: the store writes params then result; the pipeline's
  # inlined block wrote result then params. In one Shiny flush the batch hides it,
  # but the order is the invariant, and it costs nothing to hold.
  # The `$<-` method that makes the order observable lives at file level, next to
  # the other fixtures, because a seam defined inside a `test_that` cannot dispatch.
  w <- .spqc_needs_writer()
  if (is.null(w)) return()
  # A store seam that RECORDS the order of its assignments.
  #
  # Two earlier attempts at this assertion were VACUOUS, and the falsification
  # harness is what proved it:
  #   1. a plain `list` — R copies a list on assignment, so the caller's object is
  #      never touched and the order read back was always "nothing happened";
  #   2. an `environment` — `names()` on an environment returns its bindings in
  #      SORTED order, not insertion order, so `c("hotspot_params",
  #      "hotspot_result")` came out alphabetically correct WHATEVER the writer did.
  #      Reordering the two writes left the test green.
  # A test that cannot fail is worse than no test, because it is counted as
  # coverage. This one dispatches on `$<-`, so the order is a fact, not a hope.
  env <- new.env(parent = emptyenv())
  env$order <- character(0)
  env$values <- list()
  rec <- structure(list(), class = "spqc_recorder", .rec = env)
  w(.spqc_record(), list(source = "qc", metric = "nCount", k_neighbors = 30L), rec)

  expect_identical(env$order, c("hotspot_params", "hotspot_result"))
  # And both are really populated, not merely named.
  expect_true(is.list(env$values$hotspot_params))
  expect_true(is.data.frame(env$values$hotspot_result))
})

test_that("S1.5: a stored result RECOUNTS, so neither caller can leave a stale count", {
  # The state probe recomputes `n_results` from the stored table rather than
  # remembering it, which is what makes a caller swap safe. Asserted through the
  # real probe for both metrics.
  w <- .spqc_needs_writer()
  if (is.null(w)) return()
  for (m in c("nCount", "log_nCount")) {
    rv <- shiny::reactiveValues()
    w(.spqc_record(n = 40L, n_sig = 4L),
      list(source = "qc", metric = m, k_neighbors = 30L), rv)
    st <- .spatial_qc_drive_state(rv, list(), shiny::reactiveVal("idle"), NULL)
    expect_identical(st$n_results, 4L)
    # Re-store a different table under the other metric: the count must follow.
    w(.spqc_record(n = 25L, n_sig = 9L),
      list(source = "qc", metric = m, k_neighbors = 30L), rv)
    st2 <- .spatial_qc_drive_state(rv, list(), shiny::reactiveVal("idle"), NULL)
    expect_identical(st2$n_results, 9L)
  }
})

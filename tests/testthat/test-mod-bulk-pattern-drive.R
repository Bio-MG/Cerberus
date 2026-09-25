# =============================================================================
# test-mod-bulk-pattern-drive.R — Drive live control for the Bulk profile
# clustering action `bulk-pattern-run_pattern` (module `bulk_pattern`).
# =============================================================================
# Phase D. `run_pattern_profile()` repeats the human observer's five steps on a
# FROZEN input set, with the ONE parameter that cannot be frozen - the group
# column - resolved by RULE (.bulk_pattern_group_column) instead of by value.
# `R/bulk/bulk_pattern.R` is untouched: its exported surface is frozen by
# test-bulk-pattern-contract-freeze.R, so the wrapper lives in the module layer.
#
# What this file has to prove, and why each one is here:
#   - the frozen set is CLOSED and contains no path and no session-derived value;
#   - the group column is decided by a rule, and an undecidable case returns a
#     REASON (so the poller can publish `not_ready`) instead of guessing a column;
#   - `n_results` is an OBSERVED cluster count, not the frozen `k` echoed back;
#   - a failure publishes no count, so a stale clustering is never read as an
#     outcome (the Phase B rule, and the Phase C fix);
#   - the human observer and the drive action store the SAME shape, asserted from
#     the READER's side - the signature defect of 2026-09-25 lived exactly there.
# =============================================================================

source_project_file("config/defaults.R")
source_project_file("config/thresholds.R")
source_project_file("R/core/io_helpers.R")            # %||%
source_project_file("R/core/error_state.R")           # ts_error_state
source_project_file("R/core/provenance.R")            # new_provenance_entry
source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")
source_project_file("R/bulk/bulk_pattern.R")          # the frozen domain
source_project_file("modules/bulk/mod_bulk_pattern.R")  # the code under test

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
# 8 synthetic samples, two groups. `numeric_col` is deliberately NOT a usable
# group: the rule requires a factor/character with >= 2 distinct values.
.bpd_meta <- function(condition = "condition") {
  data.frame(
    condition   = factor(rep(c("A", "B"), each = 4L)),
    batch       = factor(rep(c("b1", "b2"), times = 4L)),
    numeric_col = seq_len(8),
    row.names   = paste0("S", seq_len(8)),
    stringsAsFactors = FALSE
  )
}

.bpd_mat <- function(n_genes = 40L, n_samples = 8L) {
  set.seed(4242)
  matrix(rnorm(n_genes * n_samples, mean = 6, sd = 1),
         nrow = n_genes, ncol = n_samples,
         dimnames = list(sprintf("G%02d", seq_len(n_genes)),
                         paste0("S", seq_len(n_samples))))
}

# A contrast with 14 significant UP genes, so the wrapper's >= 10 floor passes
# with the frozen source "up".
.bpd_de <- function(n_sig = 14L) {
  data.frame(
    gene           = sprintf("G%02d", seq_len(30L)),
    log2FoldChange = c(rep(2.5, n_sig), rep(0.1, 30L - n_sig)),
    padj           = c(rep(0.001, n_sig), rep(0.9, 30L - n_sig)),
    baseMean       = rep(100, 30L),
    stringsAsFactors = FALSE
  )
}

# A clustering record shaped like the real one, with `n_clusters` DISTINCT
# clusters so the test can tell an observed count from the frozen k.
.bpd_record <- function(k = 4L, n_clusters = 3L, genes = NULL) {
  genes <- genes %||% sprintf("G%02d", seq_len(12L))
  cl <- rep(seq_len(n_clusters), length.out = length(genes))
  list(
    type = "bulk_pattern_clusters", status = "valid",
    clusters = data.frame(gene = genes, cluster = as.integer(cl),
                          stringsAsFactors = FALSE),
    cluster_profiles = data.frame(group = "G01", cluster = seq_len(n_clusters),
                                  mean_z = 0, stringsAsFactors = FALSE),
    group_column = "condition", k = as.integer(k), seed = 15L,
    summary = list(n_genes_input = length(genes), n_genes_used = length(genes),
                   n_genes_not_found = 0L, n_genes_constant = 0L,
                   n_samples = 8L, n_samples_na = 0L, n_groups = 2L,
                   group_levels = c("A", "B")),
    parameters = list(group_column = "condition", k = as.integer(k), seed = 15L,
                      nstart = 10L, itermax = 50L),
    qc = list(n_genes_input = length(genes), n_genes_used = length(genes),
              n_samples_na = 0L, n_groups = 2L),
    warnings = character(0), provenance = list(), analysis_id = "bulk-pattern-clusters"
  )
}

# --- Global patching ---------------------------------------------------------
# A name that does not exist here is legitimate: the module calls `renderDT` and
# `nav_select` unqualified, and they only resolve because DT and bslib are
# ATTACHED in the app. `existed = FALSE` means "remove the binding on restore".
.bpd_patch <- function(name, value) {
  existed <- exists(name, envir = globalenv(), inherits = FALSE)
  old <- if (existed) get(name, envir = globalenv()) else NULL
  assign(name, value, envir = globalenv())
  list(value = old, existed = existed)
}

.bpd_restore <- function(saved) {
  for (nm in names(saved)) {
    e <- saved[[nm]]
    if (isTRUE(e$existed)) assign(nm, e$value, envir = globalenv())
    else if (exists(nm, envir = globalenv(), inherits = FALSE)) rm(list = nm, envir = globalenv())
  }
}

# Install the domain mocks. `clusters` lets a test force a cluster count.
# NB: the patch records are MERGED WITH [[<- , never with c(): a record whose
# `value` is NULL (a name that did not exist here) is DROPPED by c(), which
# silently turns the record into a bare value and breaks the restore.
.bpd_mock_domain <- function(record = NULL, fail = NULL, seen = NULL) {
  out <- list()
  out[["run_pattern_clustering"]] <- .bpd_patch("run_pattern_clustering",
    function(vst_mat, metadata, group_column, genes = NULL, k, seed = 15L, ...) {
      if (!is.null(seen)) {
        seen$vst <- vst_mat
        seen$group_column <- group_column
        seen$genes <- genes
        seen$k <- k
        seen$seed <- seed
      }
      if (!is.null(fail)) stop(fail, call. = FALSE)
      if (!is.null(record)) return(record)
      .bpd_record(k = k, n_clusters = 3L, genes = genes)
    })
  out[["assert_bulk_pattern_result"]] <- .bpd_patch("assert_bulk_pattern_result",
    function(result, context = "") invisible(TRUE))
  out
}

.bpd_state <- function(meta = .bpd_meta(), with_de = TRUE, condition_col = "condition") {
  gd <- shiny::reactiveValues()
  gd$bulk_obj <- list(counts = .bpd_mat(), metadata = meta)
  gd$language <- "fr"
  rv <- shiny::reactiveValues()
  rv$vst_mat <- .bpd_mat()
  if (with_de) {
    rv$contrasts <- list("B_vs_A" = .bpd_de())
    rv$active_contrast <- "B_vs_A"
  }
  if (!is.null(condition_col)) rv$active_condition_col <- condition_col
  list(gd = gd, rv = rv)
}

# -----------------------------------------------------------------------------
test_that("the pattern drive input set is frozen and carries no group column", {
  inputs <- .bulk_pattern_drive_inputs()

  # The name set is pinned: this is what stops a session-derived parameter from
  # being added silently later.
  expect_setequal(names(inputs),
                  c("pattern_source", "pattern_k", "pattern_seed",
                    "padj_thresh", "lfc_thresh"))
  expect_identical(inputs$pattern_source, "up")
  expect_identical(inputs$pattern_k, 4L)
  expect_identical(inputs$pattern_seed, 15L)
  expect_identical(inputs$padj_thresh, 0.05)
  expect_identical(inputs$lfc_thresh, 1)

  # `group_column` is DELIBERATELY absent: it is resolved by rule, and freezing a
  # column name would either error or silently pick the first metadata column.
  expect_false("group_column" %in% names(inputs))
})

# -----------------------------------------------------------------------------
test_that("the group column is resolved by rule, preferring the step 2 column", {
  s <- .bpd_state()
  expect_identical(.bulk_pattern_group_column(s$rv, s$gd)$column, "condition")

  # Recorded by step 2, wins over the single-candidate fallback.
  s2 <- .bpd_state(condition_col = "batch")
  expect_identical(.bulk_pattern_group_column(s2$rv, s2$gd)$column, "batch")

  # Nothing recorded, exactly ONE usable column -> still decidable.
  s3 <- .bpd_state(condition_col = NULL, meta = .bpd_meta()[, "condition", drop = FALSE])
  expect_identical(.bulk_pattern_group_column(s3$rv, s3$gd)$column, "condition")

  # Nothing recorded and SEVERAL usable columns -> a REASON in its own field, and
  # it names no column. The reason is a character too, which is exactly why the
  # return type is a named field and not a bare string.
  amb <- .bpd_state(condition_col = NULL)
  out <- .bulk_pattern_group_column(amb$rv, amb$gd)
  expect_null(out$column)
  # `as.character()` so a writer that resolves ANY column here yields a failure
  # rather than an error inside expect_match().
  expect_match(as.character(out$reason), "candidates", fixed = TRUE)

  # A recorded column that is not in the metadata, or defines < 2 groups.
  gone <- .bpd_state(condition_col = "not_a_column")
  expect_match(.bulk_pattern_group_column(gone$rv, gone$gd)$reason, "absent", fixed = TRUE)
  bad <- .bpd_state(condition_col = "numeric_col")
  expect_match(.bulk_pattern_group_column(bad$rv, bad$gd)$reason, "two groups", fixed = TRUE)

  # No metadata at all.
  empty <- .bpd_state()
  empty$gd$bulk_obj <- list(counts = .bpd_mat())
  expect_match(.bulk_pattern_group_column(empty$rv, empty$gd)$reason, "no metadata", fixed = TRUE)
})

# -----------------------------------------------------------------------------
test_that("readiness names each missing prerequisite instead of returning FALSE", {
  # no dataset
  s <- .bpd_state(); s$gd$bulk_obj <- NULL
  expect_match(.bulk_pattern_drive_ready(s$rv, s$gd), "bulk_obj is NULL", fixed = TRUE)
  # no VST
  s <- .bpd_state(); s$rv$vst_mat <- NULL
  expect_match(.bulk_pattern_drive_ready(s$rv, s$gd), "VST matrix", fixed = TRUE)
  # no active contrast
  s <- .bpd_state(with_de = FALSE)
  expect_match(.bulk_pattern_drive_ready(s$rv, s$gd), "active contrast", fixed = TRUE)
  # undecidable group column
  s <- .bpd_state(condition_col = NULL)
  rdy <- .bulk_pattern_drive_ready(s$rv, s$gd)
  expect_false(isTRUE(rdy))
  expect_match(as.character(rdy), "candidates", fixed = TRUE)
  # and the state probe publishes not_ready, not a false TRUE
  st <- .bulk_pattern_drive_state(s$rv, s$gd, function() "done", "ran")
  expect_identical(st$status, "not_ready")
  expect_false(st$ready)
  # ready when everything is present
  s <- .bpd_state()
  expect_true(isTRUE(.bulk_pattern_drive_ready(s$rv, s$gd)))
})

# -----------------------------------------------------------------------------
test_that("the drive action clusters on the frozen inputs and the resolved column", {
  seen <- new.env(parent = emptyenv())
  saved <- .bpd_mock_domain(seen = seen)
  on.exit(.bpd_restore(saved), add = TRUE)

  s <- .bpd_state()
  closed <- character(0)
  res <- shiny::isolate(.bulk_pattern_run_drive(s$gd, s$rv,
    function(status, error = NULL) closed <<- c(closed, status)))

  expect_identical(res$status, "done")
  expect_identical(res$step, "ran")
  expect_identical(closed, "done")
  # n_results is the OBSERVED cluster count (the mock returns 3 distinct clusters
  # while k is frozen at 4), so it cannot be the input echoed back.
  expect_identical(res$n_results, 3L)
  expect_false(identical(res$n_results, .bulk_pattern_drive_inputs()$pattern_k))

  # The frozen values and the rule-resolved column reached the domain call.
  expect_identical(seen$k, 4L)
  expect_identical(seen$seed, 15L)
  expect_identical(seen$group_column, "condition")
  expect_identical(seen$genes, sprintf("G%02d", seq_len(14L)))
  expect_identical(dim(seen$vst), c(40L, 8L))

  # The whole record is stored, and the results tab is selected.
  stored <- shiny::isolate(s$rv$pattern_result)
  expect_true(is.list(stored))
  expect_identical(stored$k, 4L)
  expect_true(is.data.frame(stored$clusters))
  expect_identical(shiny::isolate(s$rv$active_tab), "tab_pattern")
})

# -----------------------------------------------------------------------------
test_that("too few significant genes is an honest error, not a thin success", {
  saved <- .bpd_mock_domain()
  on.exit(.bpd_restore(saved), add = TRUE)

  # Only 3 genes pass the frozen thresholds -> below the domain's >= 10 floor.
  s <- .bpd_state()
  s$rv$contrasts <- list("B_vs_A" = .bpd_de(n_sig = 3L))
  closed <- list()
  res <- shiny::isolate(.bulk_pattern_run_drive(s$gd, s$rv,
    function(status, error = NULL) closed[[1L]] <<- list(status = status, error = error)))

  expect_identical(res$status, "error")
  expect_identical(res$step, "error")
  expect_identical(res$n_results, 0L)
  expect_identical(closed[[1L]]$status, "error")
  expect_true(nzchar(closed[[1L]]$error))
  expect_null(shiny::isolate(s$rv$pattern_result))
})

# -----------------------------------------------------------------------------
test_that("a failure publishes no count, even over a previous clustering", {
  s <- .bpd_state()
  s$rv$pattern_result <- .bpd_record(k = 4L, n_clusters = 3L)
  saved <- .bpd_mock_domain(fail = "simulated clustering failure")
  on.exit(.bpd_restore(saved), add = TRUE)

  closed <- list()
  res <- shiny::isolate(.bulk_pattern_run_drive(s$gd, s$rv,
    function(status, error = NULL) closed[[1L]] <<- list(status = status, error = error)))

  expect_identical(res$status, "error")
  expect_identical(res$n_results, 0L)
  expect_identical(closed[[1L]]$status, "error")
  # The state probe counts the STALE record, which is why the verdict must not
  # carry a count: the caller reads `status`, not a number.
  st <- .bulk_pattern_drive_state(s$rv, s$gd, function() "error", "error")
  expect_identical(st$status, "error")
  expect_identical(st$step <- st$steps$pattern, "error")
})

# -----------------------------------------------------------------------------
test_that("the token is published as a long job and no DOM button is bound", {
  src <- readLines(file.path(ts_project_root(), "modules", "bulk", "mod_bulk_pattern.R"),
                   warn = FALSE, encoding = "UTF-8")
  expect_length(grep('ts_drive_publish_token\\(.*"bulk-pattern-run_pattern"', src), 1L)
  expect_length(grep('ts_drive_bind_button\\(.*"bulk-pattern-run_pattern"', src), 0L)
  expect_identical(TS_DRIVE_BULK_PATTERN_BUTTON, "bulk-pattern-run_pattern")
  expect_identical(TS_DRIVE_BULK_PATTERN_MODULE, "bulk_pattern")
  expect_identical(.BULK_PATTERN_DRIVE_BUTTON, TS_DRIVE_BULK_PATTERN_BUTTON)
  expect_identical(.BULK_PATTERN_DRIVE_MODULE, TS_DRIVE_BULK_PATTERN_MODULE)

  # The REAL registry: long = TRUE is what gives `running` a producer, since the
  # job is synchronous.
  gd <- shiny::reactiveValues()
  gd$drive_registry <- new.env(parent = emptyenv())
  counter <- shiny::reactiveVal(0L)
  expect_true(ts_drive_publish_token(gd, TS_DRIVE_BULK_PATTERN_BUTTON, counter,
                                     ready = function() TRUE, long = TRUE))
  entry <- shiny::isolate(gd$drive_registry)[[TS_DRIVE_BULK_PATTERN_BUTTON]]
  expect_true(isTRUE(entry$long))
  expect_identical(ts_drive_token_of(gd, TS_DRIVE_BULK_PATTERN_BUTTON), counter)

  # The job API the module's close callback relies on, against its real contract.
  ts_drive_job_clear()
  on.exit(try(ts_drive_job_clear(), silent = TRUE), add = TRUE)
  job <- ts_drive_job_begin(1L, "bulk_pattern", "run_pipeline",
                            TS_DRIVE_BULK_PATTERN_BUTTON)
  expect_true(is.list(job))
  expect_identical(job$status, "running")
  expect_null(ts_drive_job_pending())
  expect_false(ts_drive_job_finish("bulk-de-run_de", status = "done", job_id = job$job_id))
  expect_true(ts_drive_job_finish(TS_DRIVE_BULK_PATTERN_BUTTON, status = "done",
                                 job_id = job$job_id))
  expect_identical(ts_drive_job_pending()$status, "done")
  ts_drive_job_clear()
  expect_false(isTRUE(ts_drive_job_busy()))
})

# -----------------------------------------------------------------------------
# THE READER-SIDE CONTRACT, learned the hard way on 2026-09-25: the signature slot
# broke because the two writers disagreed and the test agreed with the writer.
# Here both triggers are executed and their stored objects compared.
# -----------------------------------------------------------------------------
test_that("the human observer and the drive action store the SAME shape", {
  saved <- .bpd_mock_domain(record = .bpd_record(k = 4L, n_clusters = 3L))
  saved[["renderDT"]]   <- .bpd_patch("renderDT", function(...) NULL)
  saved[["nav_select"]] <- .bpd_patch("nav_select", function(...) invisible(TRUE))
  on.exit(.bpd_restore(saved), add = TRUE)

  # --- 1. the HUMAN path, through the module's own observer ------------------
  h <- .bpd_state()
  shiny::testServer(mod_bulk_pattern_server,
                    args = list(global_data = h$gd, shared_rv = h$rv), {
                      session$setInputs(run_pattern = 1, pattern_group = "condition",
                                        pattern_k = 4, pattern_seed = 15,
                                        pattern_source = "up")
                    })
  human <- shiny::isolate(h$rv$pattern_result)

  # --- 2. the DRIVE path, on its own state -----------------------------------
  d <- .bpd_state()
  shiny::isolate(.bulk_pattern_run_drive(d$gd, d$rv, function(status, error = NULL) NULL))
  driven <- shiny::isolate(d$rv$pattern_result)

  expect_true(is.list(human))
  expect_true(is.list(driven))
  nm_h <- if (is.list(human)) names(human) else character(0)
  nm_d <- if (is.list(driven)) names(driven) else character(0)
  expect_setequal(nm_h, nm_d)

  # The exact expressions the three readers evaluate.
  for (obj in list(human, driven)) {
    expect_true(is.data.frame(obj$clusters))
    expect_identical(ncol(obj$clusters), 2L)
    expect_true(is.data.frame(obj$cluster_profiles))
    # `summary` is a LIST of 8 scalars (as in the domain record); a flattened
    # vector would break build_pattern_table_export(), which reads it by name.
    expect_true(is.list(obj$summary))
    expect_identical(length(obj$summary), 8L)
    expect_true(all(c("n_genes_used", "n_groups", "group_levels") %in% names(obj$summary)))
    expect_true(is.integer(obj$k))
    expect_identical(as.integer(obj$k), 4L)
  }
  # build_pattern_table_export() reads $summary, so the summary must be a list,
  # not a flattened vector.
  expect_true(is.list(human$summary) && is.list(driven$summary))
})

# -----------------------------------------------------------------------------
test_that("the shared writer refuses anything that is not a clustering record", {
  rv <- shiny::reactiveValues()
  err <- tryCatch(.bulk_pattern_store(list(k = 4L), rv), condition = function(e) e)
  expect_s3_class(err, "bulk_pattern_error")
  expect_identical(ts_error_state(err), "invalid_input")
  expect_null(shiny::isolate(rv$pattern_result))
})

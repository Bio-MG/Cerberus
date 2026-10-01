# =============================================================================
# test-mod-sc-pathways-drive.R — Drive live control for the SC pathway
# enrichment action `sc-pathways-run_pathway` (module `sc_pathways`).
# =============================================================================
# Phase E (docs/mcp_propagation.md §1.6/§9), rewritten from the Phase-F WIP
# backup against the MERGED implementation's own naming: the frozen set is
# `.SC_PATHWAYS_DRIVE_INPUTS()` (source/db/org/pval, UPPER_CASE), the gene
# source is the marker table — a DECLARED PREREQUISITE carried by readiness —
# and the 100-gene cap is the human path's own `head(md$gene, 100)`, hardcoded
# in BOTH paths (the WIP's `.sc_pathways_store()` / `sc_pathway_max_genes`
# wrapper layer does not exist here and must not come back).
#
# What this file has to prove, and why each one is here:
#   - the frozen set is CLOSED and carries no gene list and no cap;
#   - readiness names each missing prerequisite instead of returning FALSE;
#   - `n_results` is the number of ENRICHED pathways (nrow), not the gene count;
#   - a failure publishes no count, so a stale enrichment is never read as an
#     outcome (the Phase B rule, and the Phase C fix);
#   - the human observer and the drive action write the SAME two shared slots
#     (`shared_rv$pathway_results` / `$pathway_db`).
# =============================================================================

source_project_file("R/core/io_helpers.R")            # %||%, detect_gene_id_type
source_project_file("R/core/error_state.R")
source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")
source_project_file("R/core/pathway_helpers.R")       # run_pathway_enrichment
source_project_file("modules/sc/mod_sc_pathways.R")   # the code under test

if (!exists(".tr", envir = globalenv()))
  assign(".tr", function(key) key, envir = globalenv())
if (!exists(".tr_plain", envir = globalenv()))
  assign(".tr_plain", function(key) key, envir = globalenv())
if (!exists(".t_fmt", envir = globalenv()))
  assign(".t_fmt", function(template, ...) {
    vals <- list(...)
    for (nm in names(vals)) {
      template <- gsub(paste0("{", nm, "}"), format(vals[[nm]]), template, fixed = TRUE)
    }
    template
  }, envir = globalenv())
suppressPackageStartupMessages(library(shiny))

# --- Fixtures ----------------------------------------------------------------
# An SC object: the rownames are the enrichment universe, exactly what the drive
# action passes (`universe = rownames(global_data$sc_obj)`).
.scp_obj <- function(n_genes = 60L, n_cells = 20L) {
  set.seed(909)
  matrix(rnorm(n_genes * n_cells), nrow = n_genes, ncol = n_cells,
         dimnames = list(sprintf("G%03d", seq_len(n_genes)),
                         paste0("C", seq_len(n_cells))))
}

# A marker table with n DISTINCT genes, sorted like a real Step-4 output
# (descending score): the cap `head(md$gene, 100)` must take the FIRST 100.
.scp_markers <- function(n = 120L) {
  data.frame(
    gene  = sprintf("G%03d", seq_len(n)),
    score = seq_len(n) * 0.1,
    stringsAsFactors = FALSE
  )
}

# An enrichment table shaped like clusterProfiler's result: what nrow() counts.
.scp_table <- function(n = 4L) {
  data.frame(
    ID          = sprintf("GO:%05d", seq_len(n)),
    Description = sprintf("pathway %d", seq_len(n)),
    GeneRatio   = rep("10/100", n), BgRatio = rep("200/20000", n),
    pvalue      = rep(1e-4, n), p.adjust = rep(1e-3, n),
    geneID      = rep("G001/G002", n), Count = rep(10L, n),
    stringsAsFactors = FALSE
  )
}

# --- Global patching ---------------------------------------------------------
.scp_patch <- function(name, value) {
  existed <- exists(name, envir = globalenv(), inherits = FALSE)
  old <- if (existed) get(name, envir = globalenv()) else NULL
  assign(name, value, envir = globalenv())
  list(value = old, existed = existed)
}

.scp_restore <- function(saved) {
  for (nm in names(saved)) {
    e <- saved[[nm]]
    if (isTRUE(e$existed)) assign(nm, e$value, envir = globalenv())
    else if (exists(nm, envir = globalenv(), inherits = FALSE)) rm(list = nm, envir = globalenv())
  }
}

# The domain mock: records what the enrichment actually received (the only way to
# prove the FROZEN values and the cap reached it), and can fail on demand.
# `detect_gene_id_type` is patched to "symbol" so the ENSG remap is a no-op —
# the remap path is the human path's own helper, not this action's contract.
.scp_mock_domain <- function(record = NULL, fail = NULL, seen = NULL) {
  out <- list()
  out[["detect_gene_id_type"]] <- .scp_patch("detect_gene_id_type",
    function(gene_ids) "symbol")
  out[["run_pathway_enrichment"]] <- .scp_patch("run_pathway_enrichment",
    function(genes, organism = "human", database = "GOBP", pval_cutoff = 0.05,
             universe = NULL, ...) {
      if (!is.null(seen)) {
        seen$genes <- genes
        seen$organism <- organism
        seen$database <- database
        seen$pval_cutoff <- pval_cutoff
        seen$universe <- universe
      }
      if (!is.null(fail)) stop(fail, call. = FALSE)
      if (!is.null(record)) return(record)
      .scp_table(n = 4L)
    })
  out
}

.scp_state <- function(n_markers = 120L, with_markers = TRUE) {
  gd <- shiny::reactiveValues()
  gd$sc_obj <- .scp_obj()
  gd$language <- "fr"
  rv <- shiny::reactiveValues()
  if (with_markers) rv$markers_data <- .scp_markers(n_markers)
  list(gd = gd, rv = rv)
}

# -----------------------------------------------------------------------------
test_that("the pathway drive input set is frozen and carries no gene list", {
  inputs <- .SC_PATHWAYS_DRIVE_INPUTS()

  # The name set is pinned: this is what stops a session-derived parameter from
  # being added silently later.
  expect_setequal(names(inputs), c("source", "db", "org", "pval"))
  expect_identical(inputs$source, "markers")
  expect_identical(inputs$db, "GOBP")
  expect_identical(inputs$org, "human")
  expect_identical(inputs$pval, 0.05)

  # The gene source is a PREREQUISITE (the marker table, gated by readiness),
  # never a parameter: no input may carry a gene list.
  expect_false(any(grepl("gene", names(inputs), fixed = TRUE)))
  # And the 100-gene cap is the human path's own `head(..., 100)`, hardcoded in
  # both paths — the WIP's declared `sc_pathway_max_genes` must not come back.
  expect_false(any(grepl("max_genes", names(inputs), fixed = TRUE)))
  expect_false(any(grepl("cap", names(inputs), fixed = TRUE)))
})

# -----------------------------------------------------------------------------
test_that("readiness names each missing prerequisite instead of returning FALSE", {
  # no SC object
  s <- .scp_state(); s$gd$sc_obj <- NULL
  expect_match(.sc_pathways_drive_ready(s$rv, s$gd), "sc_obj is NULL", fixed = TRUE)
  # no marker table
  s <- .scp_state(with_markers = FALSE)
  expect_match(.sc_pathways_drive_ready(s$rv, s$gd), "marker table", fixed = TRUE)
  # an empty marker table is as useless as a missing one
  s <- .scp_state()
  s$rv$markers_data <- data.frame(gene = character(0), stringsAsFactors = FALSE)
  expect_match(.sc_pathways_drive_ready(s$rv, s$gd), "marker table", fixed = TRUE)
  # and the state probe publishes not_ready, not a false TRUE
  st <- .sc_pathways_drive_state(s$rv, s$gd, function() "done", "ran")
  expect_identical(st$status, "not_ready")
  expect_false(st$ready)
  # ready when everything is present
  s <- .scp_state()
  expect_true(isTRUE(.sc_pathways_drive_ready(s$rv, s$gd)))
})

# -----------------------------------------------------------------------------
test_that("the drive action enriches the marker table on the frozen inputs", {
  seen <- new.env(parent = emptyenv())
  saved <- .scp_mock_domain(seen = seen)
  on.exit(.scp_restore(saved), add = TRUE)

  s <- .scp_state()
  closed <- character(0)
  res <- shiny::isolate(.sc_pathways_run_drive(s$gd, s$rv,
    function(status, error = NULL) closed <<- c(closed, status)))

  expect_identical(res$status, "done")
  expect_identical(res$step, "ran")
  expect_identical(closed, "done")
  # n_results is the number of ENRICHED PATHWAYS (4), NOT the 100 genes tested:
  # the two numbers cannot be confused.
  expect_identical(res$n_results, 4L)

  # The frozen values and the cap reached the domain call: 120 markers in, the
  # FIRST 100 tested, on GOBP / human / 0.05, against the object's rownames.
  expect_length(seen$genes, 100L)
  expect_identical(seen$genes[1L], "G001")
  expect_identical(seen$genes[100L], "G100")
  expect_identical(seen$organism, "human")
  expect_identical(seen$database, "GOBP")
  expect_identical(seen$pval_cutoff, 0.05)
  expect_identical(seen$universe, rownames(.scp_obj()))

  # The SAME two shared slots the human observer writes.
  stored <- shiny::isolate(s$rv$pathway_results)
  expect_true(is.data.frame(stored))
  expect_identical(nrow(stored), 4L)
  expect_identical(shiny::isolate(s$rv$pathway_db), "GOBP")
})

# -----------------------------------------------------------------------------
test_that("too few markers is an honest error, not a thin success", {
  saved <- .scp_mock_domain()
  on.exit(.scp_restore(saved), add = TRUE)

  s <- .scp_state(n_markers = 5L)
  closed <- list()
  res <- shiny::isolate(.sc_pathways_run_drive(s$gd, s$rv,
    function(status, error = NULL) closed[[1L]] <<- list(status = status, error = error)))

  expect_identical(res$status, "error")
  expect_identical(res$step, "error")
  expect_identical(res$n_results, 0L)
  expect_identical(closed[[1L]]$status, "error")
  expect_match(closed[[1L]]$error, "too few genes", fixed = TRUE)
  expect_null(shiny::isolate(s$rv$pathway_results))
})

# -----------------------------------------------------------------------------
test_that("a missing marker table is refused before the domain is reached", {
  seen <- new.env(parent = emptyenv())
  saved <- .scp_mock_domain(seen = seen)
  on.exit(.scp_restore(saved), add = TRUE)

  s <- .scp_state(with_markers = FALSE)
  closed <- list()
  res <- shiny::isolate(.sc_pathways_run_drive(s$gd, s$rv,
    function(status, error = NULL) closed[[1L]] <<- list(status = status, error = error)))
  expect_identical(res$status, "error")
  expect_match(closed[[1L]]$error, "no marker table", fixed = TRUE)
  # The domain was never called.
  expect_null(seen$genes)
})

# -----------------------------------------------------------------------------
test_that("a failure publishes no count, even over a previous enrichment", {
  s <- .scp_state()
  s$rv$pathway_results <- .scp_table(4L)
  s$rv$pathway_db <- "GOBP"
  saved <- .scp_mock_domain(fail = "simulated enrichment failure")
  on.exit(.scp_restore(saved), add = TRUE)

  closed <- list()
  res <- shiny::isolate(.sc_pathways_run_drive(s$gd, s$rv,
    function(status, error = NULL) closed[[1L]] <<- list(status = status, error = error)))

  expect_identical(res$status, "error")
  expect_identical(res$n_results, 0L)
  expect_identical(closed[[1L]]$status, "error")
  # The state probe counts the STALE table, which is why the verdict must not
  # carry a count: the caller reads `status`, not a number.
  st <- .sc_pathways_drive_state(s$rv, s$gd, function() "error", "error")
  expect_identical(st$status, "error")
  expect_identical(st$steps$pathways, "error")
})

# -----------------------------------------------------------------------------
test_that("an enrichment with no pathway is an honest error, not a thin success", {
  saved <- .scp_mock_domain(record = .scp_table(0L))
  on.exit(.scp_restore(saved), add = TRUE)

  s <- .scp_state()
  closed <- list()
  res <- shiny::isolate(.sc_pathways_run_drive(s$gd, s$rv,
    function(status, error = NULL) closed[[1L]] <<- list(status = status, error = error)))
  expect_identical(res$status, "error")
  expect_match(closed[[1L]]$error, "no enriched pathway", fixed = TRUE)
  expect_null(shiny::isolate(s$rv$pathway_results))
})

# -----------------------------------------------------------------------------
test_that("the state probe recounts the stored table and never remembers a count", {
  s <- .scp_state()
  s$rv$pathway_results <- .scp_table(7L)
  st <- .sc_pathways_drive_state(s$rv, s$gd, function() "done", "ran")
  expect_identical(st$n_results, 7L)
  expect_identical(st$steps$pathways, "ran")
  expect_identical(st$module, "sc_pathways")
  expect_identical(st$action, "run_pathway")
  expect_identical(st$has_data, TRUE)

  # A new step-4 run clears the slots: the count goes with them, so no stale
  # number can survive a new analysis.
  s$rv$pathway_results <- NULL
  expect_identical(.sc_pathways_drive_state(s$rv, s$gd, function() "idle")$n_results, 0L)

  # A missing step label is `skipped`; an unknown one is refused into `error`
  # rather than published verbatim.
  st2 <- .sc_pathways_drive_state(s$rv, s$gd, function() "done")
  expect_identical(st2$steps$pathways, "skipped")
  st3 <- .sc_pathways_drive_state(s$rv, s$gd, function() "done", "nonsense")
  expect_identical(st3$steps$pathways, "error")
})

# -----------------------------------------------------------------------------
test_that("the token is published as a long job and no DOM button is bound", {
  src <- readLines(file.path(ts_project_root(), "modules", "sc", "mod_sc_pathways.R"),
                   warn = FALSE, encoding = "UTF-8")
  expect_length(grep('ts_drive_publish_token\\(.*"sc-pathways-run_pathway"', src), 1L)
  expect_length(grep('ts_drive_bind_button\\(.*"sc-pathways-run_pathway"', src), 0L)
  # The state probe is wired through the EXISTING `state =` seam.
  expect_length(grep("state = sc_pathways_drive_state", src, fixed = TRUE), 1L)
  expect_identical(TS_DRIVE_SC_PATHWAYS_BUTTON, "sc-pathways-run_pathway")
  expect_identical(TS_DRIVE_SC_PATHWAYS_MODULE, "sc_pathways")
  expect_identical(.SC_PATHWAYS_DRIVE_BUTTON, TS_DRIVE_SC_PATHWAYS_BUTTON)
  expect_identical(.SC_PATHWAYS_DRIVE_MODULE, TS_DRIVE_SC_PATHWAYS_MODULE)
  expect_true(TS_DRIVE_SC_PATHWAYS_BUTTON %in% TS_DRIVE_BUTTONS)
  expect_true(TS_DRIVE_SC_PATHWAYS_MODULE %in% TS_DRIVE_MODULES)

  # The real registry: long = TRUE is what gives `running` a producer, since the
  # job is synchronous.
  gd <- shiny::reactiveValues()
  gd$drive_registry <- new.env(parent = emptyenv())
  counter <- shiny::reactiveVal(0L)
  expect_true(ts_drive_publish_token(gd, TS_DRIVE_SC_PATHWAYS_BUTTON, counter,
                                     ready = function() TRUE, long = TRUE))
  entry <- shiny::isolate(gd$drive_registry)[[TS_DRIVE_SC_PATHWAYS_BUTTON]]
  expect_true(isTRUE(entry$long))
  expect_identical(ts_drive_token_of(gd, TS_DRIVE_SC_PATHWAYS_BUTTON), counter)

  # The job API the module's close callback relies on, against its real contract.
  ts_drive_job_clear()
  on.exit(try(ts_drive_job_clear(), silent = TRUE), add = TRUE)
  job <- ts_drive_job_begin(1L, "sc_pathways", "run_pathway",
                            TS_DRIVE_SC_PATHWAYS_BUTTON)
  expect_true(is.list(job))
  expect_identical(job$status, "running")
  expect_false(ts_drive_job_finish("bulk-de-run_de", status = "done", job_id = job$job_id))
  expect_true(ts_drive_job_finish(TS_DRIVE_SC_PATHWAYS_BUTTON, status = "done",
                                  job_id = job$job_id))
  expect_identical(ts_drive_job_pending()$status, "done")
  ts_drive_job_clear()
  expect_false(isTRUE(ts_drive_job_busy()))
})

# -----------------------------------------------------------------------------
# THE READER-SIDE CONTRACT: both triggers write `shared_rv$pathway_results` and
# `shared_rv$pathway_db`, and the module's own sync observer renders from them.
# Here both triggers are executed and their stored objects compared.
# -----------------------------------------------------------------------------
test_that("the human observer and the drive action write the SAME two slots", {
  saved <- .scp_mock_domain(record = .scp_table(4L))
  saved[["showNotification"]] <- .scp_patch("showNotification", function(...) invisible(NULL))
  saved[["Progress"]] <- .scp_patch("Progress", list(
    new = function(...) list(set = function(...) invisible(NULL),
                             close = function() invisible(NULL))))
  # renderDT / renderPlot / renderPlotly are ATTACHED in the app (DT, plotly),
  # not sourced: the module's table and plot outputs only resolve because of
  # that. Stub them at the same seam the app-level attachment occupies — the
  # outputs are not this test's subject, the shared slots are.
  saved[["renderDT"]] <- .scp_patch("renderDT", function(...) NULL)
  saved[["renderPlot"]] <- .scp_patch("renderPlot", function(...) NULL)
  saved[["renderPlotly"]] <- .scp_patch("renderPlotly", function(...) NULL)
  on.exit(.scp_restore(saved), add = TRUE)

  # --- 1. the HUMAN path, through the module's own observer ------------------
  h <- .scp_state()
  shiny::testServer(mod_sc_pathways_server,
                    args = list(global_data = h$gd, shared_rv = h$rv), {
                      session$setInputs(run_pathway = 1, pathway_source = "markers",
                                        pathway_db = "GOBP", pathway_org = "human",
                                        pathway_pval = 0.05)
                    })
  human <- shiny::isolate(h$rv$pathway_results)

  # --- 2. the DRIVE path, on its own state -----------------------------------
  d <- .scp_state()
  shiny::isolate(.sc_pathways_run_drive(d$gd, d$rv, function(status, error = NULL) NULL))
  driven <- shiny::isolate(d$rv$pathway_results)

  expect_true(is.data.frame(human))
  expect_true(is.data.frame(driven))
  expect_identical(names(human), names(driven))
  expect_identical(nrow(human), nrow(driven))
  expect_identical(shiny::isolate(h$rv$pathway_db), "GOBP")
  expect_identical(shiny::isolate(d$rv$pathway_db), "GOBP")
  # Both stored the SAME observed pathway count (4), from the same mock record.
  expect_identical(nrow(human), 4L)
})

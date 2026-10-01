# =============================================================================
# test-mod-bulk-network-drive.R — Drive live control for the Bulk PCSF
# sub-network action `bulk-network-run_network` (module `bulk_network`).
# =============================================================================
# Phase E (docs/mcp_propagation.md §1.6/§8), rewritten from the Phase-F WIP
# backup against the MERGED implementation's own naming: the action reads the
# FROZEN set `.BULK_NETWORK_DRIVE_INPUTS()` (7 widget defaults, UPPER_CASE) and
# the session's DE thresholds (`shared_rv$padj_thresh` / `$lfc_thresh`, with
# `%||%` defaults) — the WIP's frozen-threshold pair is deliberately NOT the
# merged behaviour, and the closed-set pin below keeps it from coming back.
# `R/bulk/bulk_network.R` is untouched: its exported surface is frozen by
# test-bulk-network-contract-freeze.R (`bulk_network_public_api()`), so the
# wrapper lives in the module layer.
#
# What this file has to prove, and why each one is here:
#   - the frozen set is CLOSED: no contrast, no session threshold in it;
#   - readiness names each missing prerequisite instead of returning FALSE;
#   - `n_results` is an OBSERVED node count read from the record's QC, not the
#     number of input prizes;
#   - a failure publishes no count, so a stale sub-network is never read as an
#     outcome (the Phase B rule, and the Phase C fix);
#   - the human observer and the drive action store the SAME shape, asserted from
#     the READER's side.
# =============================================================================

source_project_file("config/defaults.R")
source_project_file("config/thresholds.R")
source_project_file("R/core/io_helpers.R")            # %||%
source_project_file("R/core/error_state.R")           # ts_error_state
source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")
source_project_file("R/bulk/bulk_network.R")          # the frozen domain
source_project_file("modules/bulk/mod_bulk_network.R")  # the code under test

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
# 8 synthetic samples, two groups, two batches.
.bnd_meta <- function() {
  data.frame(
    condition = factor(rep(c("A", "B"), each = 4L)),
    batch     = factor(rep(c("b1", "b2"), times = 4L)),
    row.names = paste0("S", seq_len(8L)),
    stringsAsFactors = FALSE
  )
}

.bnd_mat <- function(n_genes = 40L, n_samples = 8L) {
  set.seed(515)
  matrix(rnorm(n_genes * n_samples, mean = 6, sd = 1),
         nrow = n_genes, ncol = n_samples,
         dimnames = list(sprintf("G%02d", seq_len(n_genes)),
                         paste0("S", seq_len(n_samples))))
}

# A contrast with 14 significant genes, so the "at least one" gate passes.
.bnd_de <- function(n_sig = 14L) {
  data.frame(
    gene           = sprintf("G%02d", seq_len(30L)),
    log2FoldChange = c(rep(2.5, n_sig), rep(0.1, 30L - n_sig)),
    # The prime is -log10(padj): 0.001 -> 3.0, which clears the frozen threshold
    # of 1.3 on the prize scale.
    padj           = c(rep(0.001, n_sig), rep(0.9, 30L - n_sig)),
    baseMean       = rep(100, 30L),
    stringsAsFactors = FALSE
  )
}

# A PCSF record shaped like the real one: `n_nodes` is what `n_results` must read,
# and it is deliberately DIFFERENT from the 14 input prizes.
.bnd_record <- function(n_nodes = 6L, n_prizes = 14L) {
  nodes <- data.frame(
    node   = sprintf("N%02d", seq_len(n_nodes)),
    symbol = sprintf("G%02d", seq_len(n_nodes)),
    prize  = seq_len(n_nodes) * 0.5,
    role   = c(rep("terminal", min(n_nodes, 4L)),
               rep("relay", max(0L, n_nodes - 4L))),
    stringsAsFactors = FALSE
  )
  # A chain, so the edge table is well-formed for ANY n_nodes including 0.
  from <- nodes$node[-length(nodes$node)]
  to   <- nodes$node[-1L]
  list(
    type = "bulk_network_pcsf", status = "valid",
    nodes = nodes,
    edges = data.frame(from = from, to = to, stringsAsFactors = FALSE),
    prizes    = stats::setNames(seq_len(n_prizes) * 0.2, sprintf("G%02d", seq_len(n_prizes))),
    node_role = nodes$role,
    species   = "hsapiens", source_db = "Reactome", source_version = "offline",
    id_type   = "SYMBOL", map_rate = 1,
    parameters = list(threshold = 1.3, omega = 10, beta = 1, mu = 1,
                      convert_ids = TRUE),
    qc = list(algorithm = "PCSF-heuristic", threshold = 1.3,
              n_prizes_input = n_prizes, n_prizes_non_positive = 0L,
              n_prizes_below_threshold = 0L, n_prizes_outside_network = 0L,
              n_terminals = min(n_nodes, 4L), n_relay = max(0L, n_nodes - 4L),
              n_nodes = n_nodes, n_edges = max(0L, n_nodes - 1L), n_trees = 1L,
              total_prize = n_nodes * 0.5, score = 1,
              max_join_hops = 1, network_nodes = 20000L, network_edges = 100000L),
    warnings = character(0), provenance = list(), analysis_id = "bulk-network-pcsf",
    timestamp_utc = "2026-09-25T00:00:00Z"
  )
}

# --- Global patching ---------------------------------------------------------
.bnd_patch <- function(name, value) {
  existed <- exists(name, envir = globalenv(), inherits = FALSE)
  old <- if (existed) get(name, envir = globalenv()) else NULL
  assign(name, value, envir = globalenv())
  list(value = old, existed = existed)
}

.bnd_restore <- function(saved) {
  for (nm in names(saved)) {
    e <- saved[[nm]]
    if (isTRUE(e$existed)) assign(nm, e$value, envir = globalenv())
    else if (exists(nm, envir = globalenv(), inherits = FALSE)) rm(list = nm, envir = globalenv())
  }
}

# Install the domain mocks. `seen` records what the domain actually received, which
# is the only way to prove the FROZEN values reached it. The patch records are
# MERGED WITH [[<- , never with c(): a record whose `value` is NULL is DROPPED by
# c(), which would silently break the restore.
.bnd_mock_domain <- function(record = NULL, fail = NULL, seen = NULL) {
  out <- list()
  out[["load_bulk_network"]] <- .bnd_patch("load_bulk_network",
    function(species = "hsapiens", ...) {
      if (!is.null(seen)) seen$species_loaded <- species
      list(kind = "network", n_nodes = 20000L, n_edges = 100000L)
    })
  out[["run_bulk_network_pcsf"]] <- .bnd_patch("run_bulk_network_pcsf",
    function(prizes, network, species, threshold, params, convert_ids = TRUE, ...) {
      if (!is.null(seen)) {
        seen$prizes <- prizes
        seen$species <- species
        seen$threshold <- threshold
        seen$params <- params
        seen$convert_ids <- convert_ids
      }
      if (!is.null(fail)) stop(fail, call. = FALSE)
      if (!is.null(record)) return(record)
      .bnd_record(n_prizes = length(prizes))
    })
  out[["assert_bulk_network_result"]] <- .bnd_patch("assert_bulk_network_result",
    function(result, context = "") invisible(TRUE))
  out
}

.bnd_state <- function(with_de = TRUE) {
  gd <- shiny::reactiveValues()
  gd$bulk_obj <- list(counts = .bnd_mat(), metadata = .bnd_meta())
  gd$language <- "fr"
  rv <- shiny::reactiveValues()
  rv$vst_mat <- .bnd_mat()
  if (with_de) {
    rv$contrasts <- list("B_vs_A" = .bnd_de())
    rv$active_contrast <- "B_vs_A"
  }
  list(gd = gd, rv = rv)
}

# -----------------------------------------------------------------------------
test_that("the network drive input set is frozen and carries no session value", {
  inputs <- .BULK_NETWORK_DRIVE_INPUTS()

  # The name set is pinned: this is what stops a session-derived parameter from
  # being added silently later.
  expect_setequal(names(inputs),
                  c("species", "source", "prize", "threshold", "omega", "beta", "mu"))
  expect_identical(inputs$species, "hsapiens")
  expect_identical(inputs$source, "all_sig")
  expect_identical(inputs$prize, "padj")
  expect_identical(inputs$threshold, 1.3)
  expect_identical(inputs$omega, 10)
  expect_identical(inputs$beta, 1)
  expect_identical(inputs$mu, 1)

  # The contrast is a PREREQUISITE, never a parameter: no input may name it, or a
  # remote caller could choose a contrast the DE never produced.
  expect_false(any(grepl("contrast", names(inputs), fixed = TRUE)))

  # The DE thresholds are NOT frozen here (that was the WIP's answer): the merged
  # implementation reads the session's step-2 thresholds with `%||%` defaults, so
  # the frozen set must not carry them (its own `threshold` is the PCSF prize
  # threshold, a different knob).
  expect_false("padj_thresh" %in% names(inputs))
  expect_false("lfc_thresh" %in% names(inputs))
})

# -----------------------------------------------------------------------------
test_that("readiness names each missing prerequisite instead of returning FALSE", {
  # no dataset
  s <- .bnd_state(); s$gd$bulk_obj <- NULL
  expect_match(.bulk_network_drive_ready(s$rv, s$gd), "bulk_obj is NULL", fixed = TRUE)
  # no VST
  s <- .bnd_state(); s$rv$vst_mat <- NULL
  expect_match(.bulk_network_drive_ready(s$rv, s$gd), "VST matrix", fixed = TRUE)
  # no active contrast
  s <- .bnd_state(with_de = FALSE)
  expect_match(.bulk_network_drive_ready(s$rv, s$gd), "active contrast", fixed = TRUE)
  # and the state probe publishes not_ready, not a false TRUE
  st <- .bulk_network_drive_state(s$rv, s$gd, function() "done", "ran")
  expect_identical(st$status, "not_ready")
  expect_false(st$ready)
  # ready when everything is present
  s <- .bnd_state()
  expect_true(isTRUE(.bulk_network_drive_ready(s$rv, s$gd)))
})

# -----------------------------------------------------------------------------
test_that("the drive action builds the sub-network on the frozen inputs", {
  seen <- new.env(parent = emptyenv())
  saved <- .bnd_mock_domain(seen = seen)
  on.exit(.bnd_restore(saved), add = TRUE)

  s <- .bnd_state()
  closed <- character(0)
  res <- shiny::isolate(.bulk_network_run_drive(s$rv,
    function(status, error = NULL) closed <<- c(closed, status)))

  expect_identical(res$status, "done")
  expect_identical(res$step, "ran")
  expect_identical(closed, "done")
  # n_results is the OBSERVED node count (6) read from the record's QC, while the
  # run took 14 input prizes: the two numbers cannot be confused.
  expect_identical(res$n_results, 6L)
  expect_false(identical(res$n_results, 14L))

  # The frozen values reached the domain call.
  expect_identical(seen$species, "hsapiens")
  expect_identical(seen$species_loaded, "hsapiens")
  expect_identical(seen$threshold, 1.3)
  expect_identical(seen$params, list(omega = 10, beta = 1, mu = 1))
  expect_true(isTRUE(seen$convert_ids))
  # The prime is -log10(padj) = 3 for the significant genes, which clears 1.3; a
  # gene whose |log2FC| is under the session threshold is not in the prize vector.
  expect_true(is.numeric(seen$prizes))
  expect_length(seen$prizes, 14L)
  expect_true(all(seen$prizes > 1.3))

  # The whole record is stored for the readers.
  stored <- shiny::isolate(s$rv$network_result)
  expect_true(is.list(stored))
  expect_true(is.data.frame(stored$nodes))
  expect_identical(stored$qc$n_nodes, 6L)
})

# -----------------------------------------------------------------------------
test_that("the significance thresholds come from the session, with honest defaults", {
  seen <- new.env(parent = emptyenv())
  saved <- .bnd_mock_domain(seen = seen)
  on.exit(.bnd_restore(saved), add = TRUE)

  # No session thresholds: the `%||%` defaults (0.05 / 1) apply, so all 14
  # significant genes enter the prize vector.
  s <- .bnd_state()
  shiny::isolate(.bulk_network_run_drive(s$rv, function(status, error = NULL) NULL))
  expect_length(seen$prizes, 14L)

  # A session threshold of 0.05 excludes the padj = 0.9 genes... which the default
  # already did. To see the SESSION value bite, tighten padj to 0.01: a gene at
  # padj = 0.03 passes the default but not the session value.
  de2 <- .bnd_de(14L)
  de2$padj[14L] <- 0.03
  s2 <- .bnd_state()
  s2$rv$contrasts <- list("B_vs_A" = de2)
  s2$rv$padj_thresh <- 0.01
  seen$prizes <- NULL
  shiny::isolate(.bulk_network_run_drive(s2$rv, function(status, error = NULL) NULL))
  expect_length(seen$prizes, 13L)

  # And the LFC half: session lfc_thresh = 3 drops every gene (|log2FC| = 2.5).
  s3 <- .bnd_state()
  s3$rv$lfc_thresh <- 3
  closed <- character(0)
  res <- shiny::isolate(.bulk_network_run_drive(s3$rv,
    function(status, error = NULL) closed <<- c(closed, status)))
  expect_identical(res$status, "error")
  expect_identical(res$step, "error")
  expect_null(shiny::isolate(s3$rv$network_result))
})

# -----------------------------------------------------------------------------
test_that("no significant gene is an honest error, not a thin success", {
  saved <- .bnd_mock_domain()
  on.exit(.bnd_restore(saved), add = TRUE)

  s <- .bnd_state()
  s$rv$contrasts <- list("B_vs_A" = .bnd_de(n_sig = 0L))
  closed <- list()
  res <- shiny::isolate(.bulk_network_run_drive(s$rv,
    function(status, error = NULL) closed[[1L]] <<- list(status = status, error = error)))

  expect_identical(res$status, "error")
  expect_identical(res$step, "error")
  expect_identical(res$n_results, 0L)
  expect_identical(closed[[1L]]$status, "error")
  expect_true(nzchar(closed[[1L]]$error))
  expect_null(shiny::isolate(s$rv$network_result))
})

# -----------------------------------------------------------------------------
test_that("a failure publishes no count, even over a previous sub-network", {
  s <- .bnd_state()
  s$rv$network_result <- .bnd_record(n_nodes = 6L)
  saved <- .bnd_mock_domain(fail = "simulated PCSF failure")
  on.exit(.bnd_restore(saved), add = TRUE)

  closed <- list()
  res <- shiny::isolate(.bulk_network_run_drive(s$rv,
    function(status, error = NULL) closed[[1L]] <<- list(status = status, error = error)))

  expect_identical(res$status, "error")
  expect_identical(res$n_results, 0L)
  expect_identical(closed[[1L]]$status, "error")
  # The state probe counts the STALE record, which is why the verdict must not
  # carry a count: the caller reads `status`, not a number.
  st <- .bulk_network_drive_state(s$rv, s$gd, function() "error", "error")
  expect_identical(st$status, "error")
  expect_identical(st$steps$network, "error")
})

# -----------------------------------------------------------------------------
test_that("an empty sub-network is `empty`, and its job still closes as done", {
  # The JOB vocabulary has no `empty`: done/error/invalid/timeout/session_lost. A
  # heuristic that retained no node is a job that COMPLETED.
  saved <- .bnd_mock_domain(record = .bnd_record(n_nodes = 0L))
  on.exit(.bnd_restore(saved), add = TRUE)

  s <- .bnd_state()
  closed <- character(0)
  res <- shiny::isolate(.bulk_network_run_drive(s$rv,
    function(status, error = NULL) closed <<- c(closed, status)))
  expect_identical(res$status, "empty")
  expect_identical(res$n_results, 0L)
  expect_identical(closed, "done")
})

# -----------------------------------------------------------------------------
test_that("the state probe recounts the stored record and never remembers a count", {
  s <- .bnd_state()
  s$rv$network_result <- .bnd_record(n_nodes = 6L)
  st <- .bulk_network_drive_state(s$rv, s$gd, function() "done", "ran")
  expect_identical(st$n_results, 6L)
  expect_identical(st$steps$network, "ran")
  expect_identical(st$module, "bulk_network")
  expect_identical(st$action, "run_network")

  # Re-importing wipes the slot: the count goes with it, so no stale number can
  # survive a new dataset.
  s$rv$network_result <- NULL
  expect_identical(.bulk_network_drive_state(s$rv, s$gd, function() "idle")$n_results, 0L)

  # A missing step label is `skipped`; an unknown one is refused into `error`
  # rather than published verbatim.
  st2 <- .bulk_network_drive_state(s$rv, s$gd, function() "done")
  expect_identical(st2$steps$network, "skipped")
  st3 <- .bulk_network_drive_state(s$rv, s$gd, function() "done", "nonsense")
  expect_identical(st3$steps$network, "error")
})

# -----------------------------------------------------------------------------
test_that("the token is published as a long job and no DOM button is bound", {
  src <- readLines(file.path(ts_project_root(), "modules", "bulk", "mod_bulk_network.R"),
                   warn = FALSE, encoding = "UTF-8")
  expect_length(grep('ts_drive_publish_token\\(.*"bulk-network-run_network"', src), 1L)
  expect_length(grep('ts_drive_bind_button\\(.*"bulk-network-run_network"', src), 0L)
  # The state probe is wired through the EXISTING `state =` seam.
  expect_length(grep("state = net_drive_state", src, fixed = TRUE), 1L)
  expect_identical(TS_DRIVE_BULK_NETWORK_BUTTON, "bulk-network-run_network")
  expect_identical(TS_DRIVE_BULK_NETWORK_MODULE, "bulk_network")
  expect_identical(.BULK_NETWORK_DRIVE_BUTTON, TS_DRIVE_BULK_NETWORK_BUTTON)
  expect_identical(.BULK_NETWORK_DRIVE_MODULE, TS_DRIVE_BULK_NETWORK_MODULE)
  expect_true(TS_DRIVE_BULK_NETWORK_BUTTON %in% TS_DRIVE_BUTTONS)
  expect_true(TS_DRIVE_BULK_NETWORK_MODULE %in% TS_DRIVE_MODULES)

  # The real registry: long = TRUE is what gives `running` a producer, since the
  # job is synchronous.
  gd <- shiny::reactiveValues()
  gd$drive_registry <- new.env(parent = emptyenv())
  counter <- shiny::reactiveVal(0L)
  expect_true(ts_drive_publish_token(gd, TS_DRIVE_BULK_NETWORK_BUTTON, counter,
                                     ready = function() TRUE, long = TRUE))
  entry <- shiny::isolate(gd$drive_registry)[[TS_DRIVE_BULK_NETWORK_BUTTON]]
  expect_true(isTRUE(entry$long))
  expect_identical(ts_drive_token_of(gd, TS_DRIVE_BULK_NETWORK_BUTTON), counter)

  # The job API the module's close callback relies on, against its real contract.
  ts_drive_job_clear()
  on.exit(try(ts_drive_job_clear(), silent = TRUE), add = TRUE)
  job <- ts_drive_job_begin(1L, "bulk_network", "run_network",
                            TS_DRIVE_BULK_NETWORK_BUTTON)
  expect_true(is.list(job))
  expect_identical(job$status, "running")
  expect_false(ts_drive_job_finish("bulk-de-run_de", status = "done", job_id = job$job_id))
  expect_true(ts_drive_job_finish(TS_DRIVE_BULK_NETWORK_BUTTON, status = "done",
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
  saved <- .bnd_mock_domain(record = .bnd_record(n_nodes = 6L))
  saved[["showNotification"]] <- .bnd_patch("showNotification", function(...) invisible(NULL))
  saved[["Progress"]] <- .bnd_patch("Progress", list(
    new = function(...) list(set = function(...) invisible(NULL),
                             close = function() invisible(NULL))))
  # renderDT/renderPlot are ATTACHED in the app (DT), not sourced: the module's
  # table and plot outputs only resolve because of that. Stub them at the same
  # seam the app-level attachment occupies — the outputs are not this test's
  # subject, the stored record's shape is.
  saved[["renderDT"]] <- .bnd_patch("renderDT", function(...) NULL)
  saved[["renderPlot"]] <- .bnd_patch("renderPlot", function(...) NULL)
  on.exit(.bnd_restore(saved), add = TRUE)

  # --- 1. the HUMAN path, through the module's own observer ------------------
  h <- .bnd_state()
  shiny::testServer(mod_bulk_network_server,
                    args = list(global_data = h$gd, shared_rv = h$rv), {
                      session$setInputs(run_network = 1, network_species = "hsapiens",
                                        network_source = "all_sig", network_prize = "padj",
                                        network_threshold = 1.3, network_omega = 10,
                                        network_beta = 1, network_mu = 1)
                    })
  human <- shiny::isolate(h$rv$network_result)

  # --- 2. the DRIVE path, on its own state -----------------------------------
  d <- .bnd_state()
  shiny::isolate(.bulk_network_run_drive(d$rv, function(status, error = NULL) NULL))
  driven <- shiny::isolate(d$rv$network_result)

  expect_true(is.list(human))
  expect_true(is.list(driven))
  expect_setequal(if (is.list(human)) names(human) else character(0),
                  if (is.list(driven)) names(driven) else character(0))

  # The exact expressions the four readers evaluate.
  for (obj in list(human, driven)) {
    expect_true(is.data.frame(obj$nodes))
    expect_true(all(c("node", "symbol", "prize", "role") %in% colnames(obj$nodes)))
    expect_true(is.data.frame(obj$edges))
    expect_true(all(c("from", "to") %in% colnames(obj$edges)))
    # `qc` is a LIST of named scalars; network_qc reads it BY NAME, so a flattened
    # vector would break the QC table.
    expect_true(is.list(obj$qc))
    expect_true(all(c("n_nodes", "n_terminals", "n_relay", "n_edges", "n_trees",
                      "algorithm", "threshold", "max_join_hops") %in% names(obj$qc)))
  }
  # The status line reads the same QC names, so they must be scalars.
  expect_true(is.numeric(human$qc$n_nodes) && is.numeric(driven$qc$n_nodes))
  # Both paths stored the SAME observed node count (6), from the same mock record.
  expect_identical(human$qc$n_nodes, driven$qc$n_nodes)
})

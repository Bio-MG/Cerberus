# =============================================================================
# test-mod-bulk-network-export.R — Slice 2.3: the `bulk_network` drive export
# route (the PCSF node table)
# =============================================================================
# The Bulk-NETWORK twin of the other export suites. The artefact is the node
# table the human `dl_network` writes via `build_bulk_network_table_export()`
# — the SAME builder the exporter calls, whose in-subgraph degree is DERIVED
# from the result's edges, so the fixture carries a real edges frame and the
# degree values are asserted, not assumed. The full result object (prizes, QC,
# parameters) stays out of the route: one route, one table.
# =============================================================================

source_project_file("config/defaults.R")
source_project_file("R/core/io_helpers.R")            # %||%
source_project_file("R/core/error_state.R")
source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")
source_project_file("R/bulk/bulk_network.R")
source_project_file("modules/bulk/mod_bulk_network_export.R")

if (!exists(".tr", envir = globalenv()))
  assign(".tr", function(key) key, envir = globalenv())

# --- Fixtures ----------------------------------------------------------------
# A canonical bulk_network_pcsf result: 4 nodes, 3 edges. Node N1 is linked
# twice -> in-subgraph degree 2; N4 is isolated -> degree 0. Every contract
# field must be present (assert_bulk_network_result checks the SET).
.net_nodes <- 4L

.net_result <- function() {
  list(
    type            = "bulk_network_pcsf",
    status          = "valid",
    nodes           = data.frame(node = sprintf("ENSG%08d", seq_len(.net_nodes)),
                                 symbol = c("TP53", "EGFR", "MYC", "AKT1"),
                                 role = c("terminal", "relay", "terminal", "relay"),
                                 prize = c(2.5, 1.0, 3.0, 0.5),
                                 stringsAsFactors = FALSE),
    edges           = data.frame(from = c("ENSG00000001", "ENSG00000001", "ENSG00000003"),
                                 to   = c("ENSG00000002", "ENSG00000003", "ENSG00000002"),
                                 stringsAsFactors = FALSE),
    prizes          = NULL,
    node_role       = NULL,
    species         = "human",
    source_db       = "string",
    source_version  = "v12",
    id_type         = "ensembl_gene",
    map_rate        = 0.9,
    parameters      = list(beta = 1),
    qc              = list(),
    warnings        = character(0),
    provenance      = "fixture",
    analysis_id     = "fixture-net-123",
    timestamp_utc   = "2026-10-03T00:00:00Z"
  )
}

.net_state <- function(with_result = TRUE) {
  gd <- shiny::reactiveValues()
  gd$language <- "fr"
  rv <- shiny::reactiveValues()
  if (with_result) rv$network_result <- .net_result()
  list(gd = gd, rv = rv)
}

.net_exporter <- function() get0("bulk_network_export_nodes_csv", envir = globalenv())
.net_next_path <- function() get0("bulk_network_export_next_path", envir = globalenv())
.net_stem <- function() get0("TS_DRIVE_EXPORT_STEM_BULK_NETWORK", envir = globalenv())
.net_prune <- function() get0("ts_drive_export_prune", envir = globalenv())

.net_need <- function(fn, what) {
  if (is.function(fn) && length(formals(fn)) == 0L) {
    r <- tryCatch(fn(), error = function(e) NULL)
    if (is.function(r)) fn <- r
  }
  if (!is.function(fn)) {
    testthat::expect_true(is.function(fn), info = paste(what, "must exist as a callable"))
    return(NULL)
  }
  fn
}

# =============================================================================
# 1. The exporter REFUSES honestly when there is nothing to export
# =============================================================================
test_that("the bulk_network exporter returns an INVALID verdict, never a throw", {
  x <- .net_need(.net_exporter(), "bulk_network_export_nodes_csv()")
  if (is.null(x)) return(invisible(NULL))

  st <- .net_state(with_result = FALSE)
  r <- x(st$rv, st$gd, tempfile("ts-net-"))
  expect_false(isTRUE(r$ok))
  expect_identical(r$status, "invalid")
  expect_null(r$descriptor)
  expect_match(paste(r$errors, collapse = " "), "bulk-network-run_network",
               fixed = TRUE,
               info = "the refusal must name the action that produces the artefact")

  # A non-canonical stored object: the BUILDER's verdict, mapped to invalid.
  st2 <- .net_state()
  st2$rv$network_result <- list(type = "something_else")
  r2 <- x(st2$rv, st2$gd, tempfile("ts-net-"))
  expect_false(isTRUE(r2$ok))
  expect_identical(r2$status, "invalid")
  expect_match(paste(r2$errors, collapse = " "), "not canonical", fixed = TRUE)
})

# =============================================================================
# 2. The export writes the REAL table — degree derived, equal to the builder's
# =============================================================================
test_that("the bulk_network export writes a parseable table equal to the builder's", {
  x <- .net_need(.net_exporter(), "bulk_network_export_nodes_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- tempfile("ts-net-dir-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .net_state()

  r <- x(st$rv, st$gd, d)
  expect_true(isTRUE(r$ok))
  expect_identical(r$status, "done")

  p <- file.path(d, r$descriptor$file)
  expect_true(file.exists(p))
  expect_gt(file.size(p), 0L)

  want <- build_bulk_network_table_export(.net_result())
  back <- utils::read.csv(p, stringsAsFactors = FALSE)
  expect_identical(names(back), c("node", "symbol", "role", "prize", "degree",
                                  "species", "source_db"))
  expect_identical(nrow(back), .net_nodes)
  expect_identical(back$node, want$node)
  # The in-subgraph degree is the builder's DERIVED column: N1 has two edges,
  # N4 none. Asserted here so the fixture proves the derivation, not luck.
  expect_identical(as.integer(want$degree), c(2L, 2L, 2L, 0L))
  expect_identical(back$degree, want$degree)
  # And the declared contract says exactly these seven columns.
  ct <- get0("TS_DRIVE_EXPORT_COLUMNS", envir = globalenv())
  expect_identical(r$descriptor$columns, ct$bulk_network$fixed)
})

# =============================================================================
# 3. The filename is app-derived and stem-pinned
# =============================================================================
test_that("the bulk_network export filename is app-derived and stem-pinned", {
  x <- .net_need(.net_exporter(), "bulk_network_export_nodes_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- tempfile("ts-net-name-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .net_state()

  r <- x(st$rv, st$gd, d)
  expect_true(isTRUE(r$ok))
  f <- r$descriptor$file
  expect_match(f, "\\.csv$")
  expect_false(grepl("/", f, fixed = TRUE))
  expect_true(file.exists(file.path(d, f)))

  stem <- .net_stem()
  if (!is.null(stem)) {
    expect_identical(stem, "bulk_network_nodes")
    expect_match(f, paste0("^", stem, "_[0-9]+\\.csv$"))
  }

  np <- .net_need(.net_next_path(), "bulk_network_export_next_path()")
  expect_match(basename(np(d)), paste0("^", stem, "_2\\.csv$"))
})

# =============================================================================
# 4. Repeated exports are distinct and BOUNDED — the stem is ours
# =============================================================================
test_that("repeated bulk_network exports are distinct and the pruner owns the stem", {
  x <- .net_need(.net_exporter(), "bulk_network_export_nodes_csv()")
  if (is.null(x)) return(invisible(NULL))
  prune <- .net_need(.net_prune(), "ts_drive_export_prune()")
  d <- tempfile("ts-net-bounded-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .net_state()

  first <- x(st$rv, st$gd, d)
  expect_true(isTRUE(first$ok))
  again <- x(st$rv, st$gd, d)
  expect_true(isTRUE(again$ok))
  expect_false(identical(again$descriptor$file, first$descriptor$file),
               info = "each export must be its own file, or the retention cap is dead code")

  foreign <- file.path(d, "operator_note.csv")
  writeLines("keep me", foreign)
  t0 <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  for (i in seq_len(10L)) {
    p <- sprintf(file.path(d, "bulk_network_nodes_%d.csv"), 100L + i)
    writeLines("x", p)
    Sys.setFileTime(p, t0 + 10 + i)
  }
  Sys.setFileTime(foreign, t0 + 2)
  Sys.setFileTime(file.path(d, first$descriptor$file), t0)
  Sys.setFileTime(file.path(d, again$descriptor$file), t0 + 1)
  pruned <- prune(d, cap = 8L)
  expect_identical(as.integer(pruned), 4L)
  expect_true(file.exists(foreign),
              info = "the pruner must never delete a file it did not write")
})

# =============================================================================
# test-mod-bulk-pattern-export.R — Slice 2.3: the `bulk_pattern` drive export
# route (the gene/cluster table)
# =============================================================================
# The Bulk-PATTERN twin of the other export suites. The artefact is the
# gene/cluster table the human `dl_pattern` writes via
# `build_pattern_table_export()` — the SAME builder the exporter calls, and
# the builder's own assert_bulk_pattern_result() is the canonical-shape
# verdict (mapped to an honest invalid, never a throw through the poller).
# The human filename embeds the session's `k`; the drive filename must not.
# =============================================================================

source_project_file("config/defaults.R")
source_project_file("R/core/io_helpers.R")            # %||%
source_project_file("R/core/error_state.R")
source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")
source_project_file("R/bulk/bulk_pattern.R")
source_project_file("modules/bulk/mod_bulk_pattern_export.R")

if (!exists(".tr", envir = globalenv()))
  assign(".tr", function(key) key, envir = globalenv())

# --- Fixtures ----------------------------------------------------------------
.pat_n <- 12L

.pat_result <- function() {
  list(
    type     = "bulk_pattern_clusters",
    status   = "valid",
    k        = 3L,
    clusters = data.frame(gene = sprintf("G%04d", seq_len(.pat_n)),
                          cluster = sprintf("c%d", seq_len(.pat_n) %% 3L),
                          stringsAsFactors = FALSE)
  )
}

.pat_state <- function(with_result = TRUE) {
  gd <- shiny::reactiveValues()
  gd$language <- "fr"
  rv <- shiny::reactiveValues()
  if (with_result) rv$pattern_result <- .pat_result()
  list(gd = gd, rv = rv)
}

.pat_exporter <- function() get0("bulk_pattern_export_clusters_csv", envir = globalenv())
.pat_next_path <- function() get0("bulk_pattern_export_next_path", envir = globalenv())
.pat_stem <- function() get0("TS_DRIVE_EXPORT_STEM_BULK_PATTERN", envir = globalenv())
.pat_prune <- function() get0("ts_drive_export_prune", envir = globalenv())

.pat_need <- function(fn, what) {
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
test_that("the bulk_pattern exporter returns an INVALID verdict, never a throw", {
  x <- .pat_need(.pat_exporter(), "bulk_pattern_export_clusters_csv()")
  if (is.null(x)) return(invisible(NULL))

  st <- .pat_state(with_result = FALSE)
  r <- x(st$rv, st$gd, tempfile("ts-pat-"))
  expect_false(isTRUE(r$ok))
  expect_identical(r$status, "invalid")
  expect_null(r$descriptor)
  expect_match(paste(r$errors, collapse = " "), "bulk-pattern-run_pattern",
               fixed = TRUE,
               info = "the refusal must name the action that produces the artefact")

  # A non-canonical stored object: the BUILDER's verdict, mapped to invalid.
  st2 <- .pat_state()
  st2$rv$pattern_result <- list(type = "something_else")
  r2 <- x(st2$rv, st2$gd, tempfile("ts-pat-"))
  expect_false(isTRUE(r2$ok))
  expect_identical(r2$status, "invalid")
  expect_match(paste(r2$errors, collapse = " "), "not canonical", fixed = TRUE)
})

# =============================================================================
# 2. The export writes the REAL table, equal to the builder's
# =============================================================================
test_that("the bulk_pattern export writes a parseable table equal to the builder's", {
  x <- .pat_need(.pat_exporter(), "bulk_pattern_export_clusters_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- tempfile("ts-pat-dir-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .pat_state()

  r <- x(st$rv, st$gd, d)
  expect_true(isTRUE(r$ok))
  expect_identical(r$status, "done")

  p <- file.path(d, r$descriptor$file)
  expect_true(file.exists(p))
  expect_gt(file.size(p), 0L)

  want <- build_pattern_table_export(.pat_result())
  back <- utils::read.csv(p, stringsAsFactors = FALSE)
  expect_identical(names(back), c("gene", "cluster"))
  expect_identical(nrow(back), .pat_n)
  expect_identical(back$gene, want$gene)
  expect_identical(back$cluster, want$cluster)
  # And the declared contract says exactly these two columns.
  ct <- get0("TS_DRIVE_EXPORT_COLUMNS", envir = globalenv())
  expect_identical(r$descriptor$columns, ct$bulk_pattern$fixed)
})

# =============================================================================
# 3. The filename leaks NOTHING the caller chose — the k value included
# =============================================================================
test_that("the bulk_pattern export filename is app-derived and carries no k value", {
  x <- .pat_need(.pat_exporter(), "bulk_pattern_export_clusters_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- tempfile("ts-pat-name-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .pat_state()

  r <- x(st$rv, st$gd, d)
  expect_true(isTRUE(r$ok))
  f <- r$descriptor$file
  # The HUMAN filename embeds `pattern_clusters_k<k>_<date>` — k is a session
  # value a drive run itself selects, so the drive file must not carry it.
  expect_false(grepl("k3", f, fixed = TRUE))
  expect_match(f, "\\.csv$")

  stem <- .pat_stem()
  if (!is.null(stem)) {
    expect_identical(stem, "bulk_pattern_clusters")
    expect_match(f, paste0("^", stem, "_[0-9]+\\.csv$"))
  }

  np <- .pat_need(.pat_next_path(), "bulk_pattern_export_next_path()")
  expect_match(basename(np(d)), paste0("^", stem, "_2\\.csv$"))
})

# =============================================================================
# 4. Repeated exports are distinct and BOUNDED — the stem is ours
# =============================================================================
test_that("repeated bulk_pattern exports are distinct and the pruner owns the stem", {
  x <- .pat_need(.pat_exporter(), "bulk_pattern_export_clusters_csv()")
  if (is.null(x)) return(invisible(NULL))
  prune <- .pat_need(.pat_prune(), "ts_drive_export_prune()")
  d <- tempfile("ts-pat-bounded-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .pat_state()

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
    p <- sprintf(file.path(d, "bulk_pattern_clusters_%d.csv"), 100L + i)
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

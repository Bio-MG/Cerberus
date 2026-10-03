# =============================================================================
# test-mod-bulk-de-export.R — S2b: the `bulk_de` drive export route
# =============================================================================
# The Bulk-DE twin of test-mod-spatial-qc-export.R, and deliberately its mirror.
# One route, ONE artefact (the ACTIVE contrast's DE table), and the client still
# chooses nothing: not the destination, not the filename, not the format. The
# route is bound to the live session's own `shared_rv$contrasts`, it writes only
# into an application-controlled bounded temporary directory, and it returns a
# REDACTED descriptor.
#
# The one DELIBERATE divergence from the spatial_qc descriptor is the ABSENCE of
# `n_sig`: for the hotspot table that field is a threshold-free count of the
# app's own calls, while a DE "significant" count depends on `padj_thresh` and
# `lfc_thresh` — values the caller does not see. A count whose definition the
# reader cannot know is the `done` lie this protocol exists to prevent, so the
# field is simply not published.
# =============================================================================

source_project_file("config/defaults.R")
source_project_file("R/core/io_helpers.R")            # %||%
source_project_file("R/core/error_state.R")
source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")
source_project_file("modules/bulk_de/mod_bulk_de_export.R")

if (!exists(".tr", envir = globalenv()))
  assign(".tr", function(key) key, envir = globalenv())

# --- Fixtures ----------------------------------------------------------------
# A normalised DE table: the exact shape `.normalize_de_cols()` guarantees
# (mod_bulk_de_run.R stores it; bulk_helpers.R defines the guarantee).
.de_n <- 50L

.de_record <- function(n = .de_n) {
  set.seed(4242)
  data.frame(gene = sprintf("G%04d", seq_len(n)),
             baseMean = stats::runif(n, 1, 2000),
             log2FoldChange = stats::rnorm(n, 0, 2),
             pvalue = stats::runif(n, 0, 1),
             padj = stats::runif(n, 0, 1),
             stringsAsFactors = FALSE)
}

#' The state a live `bulk_de` session holds after `bulk-de-run_de`: ONE active
#' contrast, stored under a caller-choosable LABEL. The label is the thing the
#' drive filename must not leak; that is asserted, not assumed.
.de_state <- function(with_result = TRUE) {
  gd <- shiny::reactiveValues()
  gd$language <- "fr"
  rv <- shiny::reactiveValues()
  if (with_result) {
    rv$contrasts <- list("CoV2_vs_mock" = .de_record())
    rv$active_contrast <- "CoV2_vs_mock"
  }
  list(gd = gd, rv = rv)
}

.de_exporter <- function() get0("bulk_de_export_results_csv", envir = globalenv())
.de_next_path <- function() get0("bulk_de_export_next_path", envir = globalenv())
.de_stem <- function() get0("TS_DRIVE_EXPORT_STEM_BULK_DE", envir = globalenv())
.de_project <- function() get0("ts_drive_project_descriptor", envir = globalenv())
.de_keep <- function() get0("ts_drive_export_descriptor", envir = globalenv())
.de_prune <- function() get0("ts_drive_export_prune", envir = globalenv())

.de_need <- function(fn, what) {
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
test_that("the bulk_de exporter returns an INVALID verdict, never a throw", {
  x <- .de_need(.de_exporter(), "bulk_de_export_results_csv()")
  if (is.null(x)) return(invisible(NULL))

  # No contrast at all: the session is live, nothing has been run.
  st <- .de_state(with_result = FALSE)
  r <- x(st$rv, st$gd, tempfile("ts-de-"))
  expect_false(isTRUE(r$ok))
  expect_identical(r$status, "invalid")
  expect_null(r$descriptor)
  expect_match(paste(r$errors, collapse = " "), "bulk-de-run_de",
               fixed = TRUE,
               info = "the refusal must name the action that produces the artefact")

  # A stored object that is NOT a normalised table: refused, not written.
  st2 <- .de_state()
  st2$rv$contrasts <- list("CoV2_vs_mock" = data.frame(foo = 1:3))
  r2 <- x(st2$rv, st2$gd, tempfile("ts-de-"))
  expect_false(isTRUE(r2$ok))
  expect_identical(r2$status, "invalid")
  expect_match(paste(r2$errors, collapse = " "),
               "not a normalised contrasts table", fixed = TRUE)
})

# =============================================================================
# 2. The export writes the REAL table, into the bounded directory
# =============================================================================
test_that("the bulk_de export writes a parseable table equal to the stored one", {
  x <- .de_need(.de_exporter(), "bulk_de_export_results_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- tempfile("ts-de-dir-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .de_state()

  r <- x(st$rv, st$gd, d)
  expect_true(isTRUE(r$ok))
  expect_identical(r$status, "done")

  p <- file.path(d, r$descriptor$file)
  expect_true(file.exists(p))
  expect_gt(file.size(p), 0L)
  expect_identical(as.integer(file.size(p)), as.integer(r$descriptor$bytes))

  # PARSEABLE, and equal to the stored table — the assertion that catches an
  # error page being reported as a successful download.
  back <- utils::read.csv(p, stringsAsFactors = FALSE)
  expect_identical(nrow(back), .de_n)
  expect_setequal(names(back), c("gene", "baseMean", "log2FoldChange", "pvalue", "padj"))
  expect_identical(back$gene, shiny::isolate(st$rv$contrasts[["CoV2_vs_mock"]]$gene))

  # The descriptor is REDACTED and HONEST: every key is inside the wire's frozen
  # keep-set, and there is NO `n_sig` — a threshold-dependent count the reader
  # could not define. Asserted as an ABSENCE, not left implicit.
  expect_setequal(names(r$descriptor),
                  c("format", "file", "bytes", "n_rows", "n_cols", "columns"))
  project <- .de_need(.de_project(), "ts_drive_project_descriptor()")
  wire <- project(r$descriptor, c("format", "file", "bytes", "n_rows",
                                  "n_cols", "n_sig", "columns"))
  # On the wire, `columns` goes through the sanitiser BY DESIGN ("a column name
  # can be user-supplied" — drive_watcher.R): the long engine-schema names are
  # token-shaped, so they arrive redacted, exactly like the badge's own label
  # once did. PINNED, not hidden: this is what the agent reads, and it is the
  # reason the file itself is the artefact.
  expect_identical(wire$columns,
                   c("gene", "<redacted>", "<redacted>", "pvalue", "padj"))
  # The projection COERCES numerics to double (its numeric branch), so the
  # remaining fields compare EQUAL, not identical — same values, wire types.
  expect_equal(wire[names(wire) != "columns"],
               r$descriptor[names(r$descriptor) != "columns"])
})

# =============================================================================
# 3. The filename leaks NOTHING the caller chose
# =============================================================================
test_that("the bulk_de export filename is app-derived and carries no contrast label", {
  x <- .de_need(.de_exporter(), "bulk_de_export_results_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- tempfile("ts-de-name-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .de_state()

  r <- x(st$rv, st$gd, d)
  expect_true(isTRUE(r$ok))
  f <- r$descriptor$file
  # The label the HUMAN filename embeds (mod_bulk_de_viz.R) must not appear.
  expect_false(grepl("CoV2_vs_mock", f, fixed = TRUE))
  expect_match(f, "\\.csv$")
  expect_false(grepl("/", f, fixed = TRUE))
  expect_false(grepl("\\\\", f, fixed = TRUE))
  expect_true(file.exists(file.path(d, f)))

  # PINNED TO THE DERIVED PATTERN, on the spatial_qc rule: a fixed string that
  # merely does not contain the label would still pass with any other stem.
  stem <- .de_stem()
  if (!is.null(stem)) {
    expect_identical(stem, "bulk_de_results")
    expect_match(f, paste0("^", stem, "_[0-9]+\\.csv$"))
    expect_false(grepl("%", stem, fixed = TRUE))
  }

  # And the next-path resolver is its stateless twin: index one above the
  # highest present, never a counter.
  np <- .de_need(.de_next_path(), "bulk_de_export_next_path()")
  expect_match(basename(np(d)), paste0("^", stem, "_2\\.csv$"))
})

# =============================================================================
# 4. Repeated exports are distinct, byte-identical, and BOUNDED — ACROSS routes
# =============================================================================
test_that("repeated bulk_de exports are distinct and the pruner unions BOTH stems", {
  x <- .de_need(.de_exporter(), "bulk_de_export_results_csv()")
  if (is.null(x)) return(invisible(NULL))
  prune <- .de_need(.de_prune(), "ts_drive_export_prune()")
  d <- tempfile("ts-de-bounded-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .de_state()

  first <- x(st$rv, st$gd, d)
  expect_true(isTRUE(first$ok))
  f1 <- file.path(d, first$descriptor$file)
  b1 <- readBin(f1, "raw", n = file.size(f1))

  again <- x(st$rv, st$gd, d)
  expect_true(isTRUE(again$ok))
  expect_false(identical(again$descriptor$file, first$descriptor$file),
               info = "each export must be its own file, or the retention cap is dead code")
  expect_identical(again$descriptor$bytes, first$descriptor$bytes)
  expect_identical(readBin(file.path(d, again$descriptor$file), "raw",
                           n = file.size(file.path(d, again$descriptor$file))), b1)

  # The pruner must recognise files of BOTH route stems: a pruner keyed to
  # spatial_qc alone would let bulk_de grow the directory past the cap forever,
  # and vice versa. The cap itself is pinned in the spatial_qc suite; here only
  # the union is asserted. Mtimes are SET explicitly, because the victims are
  # chosen by modification time and ties would make the verdict ambiguous.
  foreign <- file.path(d, "operator_note.csv")
  writeLines("keep me", foreign)
  t0 <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  Sys.setFileTime(f1, t0)                       # oldest, OURS (spatial stem? no: bulk_de _1)
  Sys.setFileTime(file.path(d, again$descriptor$file), t0 + 1)  # OURS
  Sys.setFileTime(foreign, t0 + 2)              # NOT ours
  for (i in seq_len(10L)) {
    p <- sprintf(file.path(d, "bulk_de_results_%d.csv"), 100L + i)
    writeLines("x", p)
    Sys.setFileTime(p, t0 + 10 + i)             # all NEWER than everything above
  }
  # 13 files (12 ours + 1 foreign), cap 8 -> the 5 OLDEST are victims: the two
  # real exports, the foreign note and the two oldest fakes. The ours-filter
  # then spares the foreign note: 4 pruned, and the operator's file survives.
  pruned <- prune(d, cap = 8L)
  expect_identical(as.integer(pruned), 4L,
                   info = "12 ours + 1 foreign - 8 cap = 5 victims, of which 4 are OURS")
  expect_true(file.exists(foreign),
              info = "the pruner must never delete a file it did not write")
})

# =============================================================================
# test-sc-markers-export.R — S2c: the `sc_markers` drive export route
# =============================================================================
# The SC-MARKERS twin of test-mod-bulk-pathways-export.R, and deliberately its
# mirror. One route, ONE artefact (the marker table stored under
# `shared_rv$markers_data`, written by BOTH this module's button and the SC
# auto-pipeline), and the client still chooses nothing: not the destination,
# not the filename, not the format. The route writes only into an
# application-controlled bounded temporary directory and returns a REDACTED
# descriptor.
#
# One DELIBERATE divergence from the spatial_qc descriptor, pinned here like in
# the bulk suites: NO `n_sig` — a "significant marker" depends on a `p_val_adj`
# cutoff the caller does not see. The agent counts from the table it received.
#
# The wire-projection pin also covers the marker columns VERBATIM: none of
# `gene` / `cluster` / `avg_log2FC` / `p_val_adj` / `pct.1` / `pct.2` contains
# an 8+-character alphanumeric run, so the sanitiser has nothing to redact —
# unlike the bulk_de columns, where `baseMean` arrived as `<redacted>`. That
# asymmetry is the sanitiser's rule working, not an exemption.
# =============================================================================

source_project_file("config/defaults.R")
source_project_file("R/core/io_helpers.R")            # %||%
source_project_file("R/core/error_state.R")
source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")
source_project_file("modules/sc/mod_sc_markers_export.R")

if (!exists(".tr", envir = globalenv()))
  assign(".tr", function(key) key, envir = globalenv())

# --- Fixtures ----------------------------------------------------------------
# A normalised marker table: the exact shape .normalize_marker_cols() guarantees
# (mod_sc_markers.R:31) — six columns, ordered as the producers store them.
.mk_n <- 60L

.mk_record <- function(n = .mk_n) {
  set.seed(4242)
  data.frame(gene = sprintf("G%04d", seq_len(n)),
             cluster = sprintf("c%d", seq_len(n) %% 4L),
             avg_log2FC = stats::rnorm(n, 1, 0.8),
             p_val_adj = stats::runif(n, 0, 0.05),
             pct.1 = stats::runif(n, 0.1, 1),
             pct.2 = stats::runif(n, 0, 0.5),
             stringsAsFactors = FALSE)
}

#' The state a live `sc_markers` session holds after a markers run.
.mk_state <- function(with_result = TRUE) {
  gd <- shiny::reactiveValues()
  gd$language <- "fr"
  rv <- shiny::reactiveValues()
  if (with_result) rv$markers_data <- .mk_record()
  list(gd = gd, rv = rv)
}

.mk_exporter <- function() get0("sc_markers_export_table_csv", envir = globalenv())
.mk_next_path <- function() get0("sc_markers_export_next_path", envir = globalenv())
.mk_stem <- function() get0("TS_DRIVE_EXPORT_STEM_SC_MARKERS", envir = globalenv())
.mk_project <- function() get0("ts_drive_project_descriptor", envir = globalenv())
.mk_prune <- function() get0("ts_drive_export_prune", envir = globalenv())

.mk_need <- function(fn, what) {
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
test_that("the sc_markers exporter returns an INVALID verdict, never a throw", {
  x <- .mk_need(.mk_exporter(), "sc_markers_export_table_csv()")
  if (is.null(x)) return(invisible(NULL))

  # No markers run yet: the session is live, nothing has been stored.
  st <- .mk_state(with_result = FALSE)
  r <- x(st$rv, st$gd, tempfile("ts-mk-"))
  expect_false(isTRUE(r$ok))
  expect_identical(r$status, "invalid")
  expect_null(r$descriptor)
  # The refusal names BOTH producers: the button AND the auto-pipeline can
  # create the artefact, so pointing at only one would mislead an operator.
  expect_match(paste(r$errors, collapse = " "), "sc-markers-run_markers",
               fixed = TRUE)
  expect_match(paste(r$errors, collapse = " "), "auto-pipeline", fixed = TRUE)

  # A stored object that is NOT a normalised marker table: refused, not written.
  st2 <- .mk_state()
  st2$rv$markers_data <- data.frame(foo = 1:3)
  r2 <- x(st2$rv, st2$gd, tempfile("ts-mk-"))
  expect_false(isTRUE(r2$ok))
  expect_identical(r2$status, "invalid")
  expect_match(paste(r2$errors, collapse = " "),
               "not a normalised marker table", fixed = TRUE)
})

# =============================================================================
# 2. The export writes the REAL table, into the bounded directory
# =============================================================================
test_that("the sc_markers export writes a parseable table equal to the stored one", {
  x <- .mk_need(.mk_exporter(), "sc_markers_export_table_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- tempfile("ts-mk-dir-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .mk_state()

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
  expect_identical(nrow(back), .mk_n)
  expect_setequal(names(back),
                  c("gene", "cluster", "avg_log2FC", "p_val_adj", "pct.1", "pct.2"))
  expect_identical(back$gene, shiny::isolate(st$rv$markers_data)$gene)
  # A CSV round-trip is lossy for doubles (15 significant digits), so the
  # numeric column compares EQUAL, not identical.
  expect_equal(back$avg_log2FC, shiny::isolate(st$rv$markers_data)$avg_log2FC)

  # The descriptor is REDACTED and HONEST: every key is inside the wire's frozen
  # keep-set, and there is NO `n_sig` — a cutoff-dependent count the reader
  # could not define. Asserted as an ABSENCE, not left implicit.
  expect_setequal(names(r$descriptor),
                  c("format", "file", "bytes", "n_rows", "n_cols", "columns"))
  project <- .mk_need(.mk_project(), "ts_drive_project_descriptor()")
  wire <- project(r$descriptor, c("format", "file", "bytes", "n_rows",
                                  "n_cols", "n_sig", "columns"))
  # None of the marker columns contains an 8+-character alphanumeric run, so
  # the sanitiser returns every one VERBATIM — the bulk_de suite pins the
  # opposite case (`baseMean` -> `<redacted>`); together the two pins bracket
  # the rule instead of trusting it.
  expect_identical(wire$columns,
                   c("gene", "cluster", "avg_log2FC", "p_val_adj", "pct.1", "pct.2"))
  # The projection COERCES numerics to double (its numeric branch), so the
  # remaining fields compare EQUAL, not identical — same values, wire types.
  expect_equal(wire[names(wire) != "columns"],
               r$descriptor[names(r$descriptor) != "columns"])
})

# =============================================================================
# 3. The filename leaks NOTHING the caller chose, and is PINNED to the stem
# =============================================================================
test_that("the sc_markers export filename is app-derived and stem-pinned", {
  x <- .mk_need(.mk_exporter(), "sc_markers_export_table_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- tempfile("ts-mk-name-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .mk_state()

  r <- x(st$rv, st$gd, d)
  expect_true(isTRUE(r$ok))
  f <- r$descriptor$file
  expect_match(f, "\\.csv$")
  expect_false(grepl("/", f, fixed = TRUE))
  expect_false(grepl("\\\\", f, fixed = TRUE))
  expect_true(file.exists(file.path(d, f)))

  # PINNED TO THE DERIVED PATTERN, on the spatial_qc rule: a fixed string that
  # merely avoids leaks would still pass with any other stem.
  stem <- .mk_stem()
  if (!is.null(stem)) {
    expect_identical(stem, "sc_markers_table")
    expect_match(f, paste0("^", stem, "_[0-9]+\\.csv$"))
    expect_false(grepl("%", stem, fixed = TRUE))
  }

  # And the next-path resolver is its stateless twin: index one above the
  # highest present, never a counter.
  np <- .mk_need(.mk_next_path(), "sc_markers_export_next_path()")
  expect_match(basename(np(d)), paste0("^", stem, "_2\\.csv$"))
})

# =============================================================================
# 4. Repeated exports are distinct, byte-identical, and BOUNDED — ACROSS routes
# =============================================================================
test_that("repeated sc_markers exports are distinct and the pruner unions ALL FOUR stems", {
  x <- .mk_need(.mk_exporter(), "sc_markers_export_table_csv()")
  if (is.null(x)) return(invisible(NULL))
  prune <- .mk_need(.mk_prune(), "ts_drive_export_prune()")
  d <- tempfile("ts-mk-bounded-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .mk_state()

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

  # The pruner must recognise files of ALL FOUR route stems: a pruner keyed to
  # the first three would let sc_markers grow the directory past the cap
  # forever. The cap itself is pinned in the spatial_qc suite; here the FOURTH
  # stem's membership in the ours-filter is what is asserted. Mtimes are SET
  # explicitly, because the victims are chosen by modification time and ties
  # would make the verdict ambiguous.
  foreign <- file.path(d, "operator_note.csv")
  writeLines("keep me", foreign)
  t0 <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  Sys.setFileTime(f1, t0)                       # oldest, OURS (sc_markers _1)
  Sys.setFileTime(file.path(d, again$descriptor$file), t0 + 1)  # OURS
  Sys.setFileTime(foreign, t0 + 2)              # NOT ours
  for (i in seq_len(10L)) {
    p <- sprintf(file.path(d, "sc_markers_table_%d.csv"), 100L + i)
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

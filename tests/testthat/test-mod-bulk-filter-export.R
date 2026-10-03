# =============================================================================
# test-mod-bulk-filter-export.R — Slice 2.3: the `bulk_filter` drive export
# route (the VST matrix)
# =============================================================================
# The Bulk-FILTER twin of the other export suites — with one DELIBERATE
# divergence pinned here: this route is NOT the mirror of a downloadHandler
# (the module offers no CSV download of its matrices). The artefact is the
# VST matrix both store paths write, written `gene`-first so import_bulk can
# read it straight back. Sample names are preserved VERBATIM in the file
# (`check.names = FALSE`), and a sample named `gene` is refused (duplicate
# header = a file no reader can parse).
# =============================================================================

source_project_file("config/defaults.R")
source_project_file("R/core/io_helpers.R")            # %||%
source_project_file("R/core/error_state.R")
source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")
source_project_file("modules/bulk/mod_bulk_filter_export.R")

if (!exists(".tr", envir = globalenv()))
  assign(".tr", function(key) key, envir = globalenv())

# --- Fixtures ----------------------------------------------------------------
# A named genes x samples matrix, exactly what get_vst_matrix() returns. The
# sample names carry a hyphen and an 8+-character run on purpose: the file
# must keep them verbatim (the WIRE sanitises them, the file must not).
.bf_genes <- 15L
.bf_samples <- 4L

.bf_matrix <- function() {
  set.seed(4242)
  m <- matrix(stats::rnorm(.bf_genes * .bf_samples, 5, 1),
              nrow = .bf_genes, ncol = .bf_samples,
              dimnames = list(sprintf("G%04d", seq_len(.bf_genes)),
                              c("ctrl-1", "ctrl-2", "CoV2-6h", "CoV2-6h_rep2")))
  m
}

.bf_state <- function(with_result = TRUE, m = .bf_matrix()) {
  gd <- shiny::reactiveValues()
  gd$language <- "fr"
  rv <- shiny::reactiveValues()
  if (with_result) rv$vst_mat <- m
  list(gd = gd, rv = rv)
}

.bf_exporter <- function() get0("bulk_filter_export_vst_matrix_csv", envir = globalenv())
.bf_next_path <- function() get0("bulk_filter_export_next_path", envir = globalenv())
.bf_stem <- function() get0("TS_DRIVE_EXPORT_STEM_BULK_FILTER", envir = globalenv())
.bf_prune <- function() get0("ts_drive_export_prune", envir = globalenv())

.bf_need <- function(fn, what) {
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
test_that("the bulk_filter exporter returns an INVALID verdict, never a throw", {
  x <- .bf_need(.bf_exporter(), "bulk_filter_export_vst_matrix_csv()")
  if (is.null(x)) return(invisible(NULL))

  st <- .bf_state(with_result = FALSE)
  r <- x(st$rv, st$gd, tempfile("ts-bf-"))
  expect_false(isTRUE(r$ok))
  expect_identical(r$status, "invalid")
  expect_null(r$descriptor)
  expect_match(paste(r$errors, collapse = " "), "bulk-filter-run_filter_norm",
               fixed = TRUE,
               info = "the refusal must name the action that produces the artefact")

  # NOT a named matrix: refused, not written.
  st2 <- .bf_state(m = data.frame(a = 1:3))
  r2 <- x(st2$rv, st2$gd, tempfile("ts-bf-"))
  expect_false(isTRUE(r2$ok))
  expect_identical(r2$status, "invalid")
  expect_match(paste(r2$errors, collapse = " "),
               "not a named genes x samples matrix", fixed = TRUE)

  # A sample literally named `gene` would duplicate the first column's header:
  # refused, honestly, instead of writing a reader-breaking file.
  m <- .bf_matrix(); colnames(m)[1] <- "gene"
  st3 <- .bf_state(m = m)
  r3 <- x(st3$rv, st3$gd, tempfile("ts-bf-"))
  expect_false(isTRUE(r3$ok))
  expect_identical(r3$status, "invalid")
  expect_match(paste(r3$errors, collapse = " "), "duplicate header", fixed = TRUE)
})

# =============================================================================
# 2. The export writes the REAL matrix, gene-first, names verbatim
# =============================================================================
test_that("the bulk_filter export writes a parseable matrix equal to the stored one", {
  x <- .bf_need(.bf_exporter(), "bulk_filter_export_vst_matrix_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- tempfile("ts-bf-dir-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .bf_state()

  r <- x(st$rv, st$gd, d)
  expect_true(isTRUE(r$ok))
  expect_identical(r$status, "done")

  p <- file.path(d, r$descriptor$file)
  expect_true(file.exists(p))
  expect_gt(file.size(p), 0L)
  expect_identical(as.integer(file.size(p)), as.integer(r$descriptor$bytes))

  # PARSEABLE — and the sample names survive the file VERBATIM (the check.names
  # pin): the wire sanitises them, the file must not.
  back <- utils::read.csv(p, check.names = FALSE, stringsAsFactors = FALSE)
  expect_identical(nrow(back), .bf_genes)
  expect_identical(names(back),
                   c("gene", "ctrl-1", "ctrl-2", "CoV2-6h", "CoV2-6h_rep2"))
  expect_identical(back$gene, rownames(.bf_matrix()))
  # The VALUES must round-trip, gene-first. Row identity travels in the `gene`
  # column, not in row names, so the comparison is unnamed on both sides.
  expect_equal(unname(as.matrix(back[, -1])), unname(.bf_matrix()),
               info = "the values must round-trip, gene-first")

  # The descriptor describes the FILE: gene column + one per sample. The wire
  # keeps `gene` and the short names; the 8+-char sample name is the sanitiser's
  # business (pinned in the contract suites) — here only the shape is pinned.
  expect_setequal(names(r$descriptor),
                  c("format", "file", "bytes", "n_rows", "n_cols", "columns"))
  expect_identical(r$descriptor$n_rows, .bf_genes)
  expect_identical(r$descriptor$n_cols, .bf_samples + 1L)
  expect_identical(r$descriptor$columns[[1]], "gene")
})

# =============================================================================
# 3. The filename is app-derived and stem-pinned
# =============================================================================
test_that("the bulk_filter export filename is app-derived and stem-pinned", {
  x <- .bf_need(.bf_exporter(), "bulk_filter_export_vst_matrix_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- tempfile("ts-bf-name-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .bf_state()

  r <- x(st$rv, st$gd, d)
  expect_true(isTRUE(r$ok))
  f <- r$descriptor$file
  expect_match(f, "\\.csv$")
  expect_false(grepl("/", f, fixed = TRUE))
  expect_true(file.exists(file.path(d, f)))

  stem <- .bf_stem()
  if (!is.null(stem)) {
    expect_identical(stem, "bulk_filter_vst_matrix")
    expect_match(f, paste0("^", stem, "_[0-9]+\\.csv$"))
  }

  np <- .bf_need(.bf_next_path(), "bulk_filter_export_next_path()")
  expect_match(basename(np(d)), paste0("^", stem, "_2\\.csv$"))
})

# =============================================================================
# 4. Repeated exports are distinct and BOUNDED — the NINTH stem is ours
# =============================================================================
test_that("repeated bulk_filter exports are distinct and the pruner owns the stem", {
  x <- .bf_need(.bf_exporter(), "bulk_filter_export_vst_matrix_csv()")
  if (is.null(x)) return(invisible(NULL))
  prune <- .bf_need(.bf_prune(), "ts_drive_export_prune()")
  d <- tempfile("ts-bf-bounded-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .bf_state()

  first <- x(st$rv, st$gd, d)
  expect_true(isTRUE(first$ok))
  again <- x(st$rv, st$gd, d)
  expect_true(isTRUE(again$ok))
  expect_false(identical(again$descriptor$file, first$descriptor$file),
               info = "each export must be its own file, or the retention cap is dead code")

  # The pruner must recognise THIS route's stem (the ninth): fakes of the stem
  # are ours, a foreign file is not. Mtimes SET explicitly — victims are chosen
  # by modification time and ties would make the verdict ambiguous.
  foreign <- file.path(d, "operator_note.csv")
  writeLines("keep me", foreign)
  t0 <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  for (i in seq_len(10L)) {
    p <- sprintf(file.path(d, "bulk_filter_vst_matrix_%d.csv"), 100L + i)
    writeLines("x", p)
    Sys.setFileTime(p, t0 + 10 + i)             # all NEWER than the exports
  }
  Sys.setFileTime(foreign, t0 + 2)              # NOT ours, older than the fakes
  Sys.setFileTime(file.path(d, first$descriptor$file), t0)      # OURS, oldest
  Sys.setFileTime(file.path(d, again$descriptor$file), t0 + 1)  # OURS
  # 13 files (12 ours + 1 foreign), cap 8 -> the 5 OLDEST are victims: the two
  # real exports, the foreign note and the two oldest fakes; the ours-filter
  # spares the foreign note: 4 pruned.
  pruned <- prune(d, cap = 8L)
  expect_identical(as.integer(pruned), 4L)
  expect_true(file.exists(foreign),
              info = "the pruner must never delete a file it did not write")
})

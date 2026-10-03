# =============================================================================
# test-mod-bulk-signatures-export.R — Slice 2.3: the `bulk_signatures` drive
# export route (the signature-scores grid)
# =============================================================================
# The Bulk-SIGNATURES twin of the other export suites. The artefact is the
# long signature x sample grid the human `dl_sig_csv` writes via
# `build_signature_scores_export()` — the SAME builder the exporter calls, so
# the §M3 disclaimer column travels with the drive export too (pinned, not
# assumed). The RDS download (full result with QC) is a different artefact and
# gets no route.
# =============================================================================

source_project_file("config/defaults.R")
source_project_file("R/core/io_helpers.R")            # %||%
source_project_file("R/core/error_state.R")
source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")
source_project_file("R/bulk/bulk_signatures.R")
source_project_file("modules/bulk/mod_bulk_signatures_export.R")

if (!exists(".tr", envir = globalenv()))
  assign(".tr", function(key) key, envir = globalenv())

# --- Fixtures ----------------------------------------------------------------
# A canonical bulk_signature_scores result: 3 signatures x 4 samples, with the
# fields the builder reads. Sample/signature labels carry hyphens on purpose
# (the file keeps them verbatim; only the wire sanitises).
.sig_n <- 3L
.sig_s <- 4L

.sig_result <- function() {
  set.seed(4242)
  list(
    type        = "bulk_signature_scores",
    status      = "valid",
    scores      = matrix(stats::rnorm(.sig_n * .sig_s), nrow = .sig_n,
                         dimnames = list(c("T_cell_sig", "B_cell_sig", "macro_Sig"),
                                         c("ctrl-1", "ctrl-2", "CoV2-6h", "CoV2-6h_rep2"))),
    method      = "ssgsea",
    analysis_id = "fixture-abc-123",
    disclaimer  = "scores are exploratory; not a clinical measure",
    stringsAsFactors = FALSE
  )
}

.sig_state <- function(with_result = TRUE) {
  gd <- shiny::reactiveValues()
  gd$language <- "fr"
  rv <- shiny::reactiveValues()
  if (with_result) rv$signature_scores <- .sig_result()
  list(gd = gd, rv = rv)
}

.sig_exporter <- function() get0("bulk_signatures_export_scores_csv", envir = globalenv())
.sig_next_path <- function() get0("bulk_signatures_export_next_path", envir = globalenv())
.sig_stem <- function() get0("TS_DRIVE_EXPORT_STEM_BULK_SIGNATURES", envir = globalenv())
.sig_project <- function() get0("ts_drive_project_descriptor", envir = globalenv())
.sig_prune <- function() get0("ts_drive_export_prune", envir = globalenv())

.sig_need <- function(fn, what) {
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
test_that("the bulk_signatures exporter returns an INVALID verdict, never a throw", {
  x <- .sig_need(.sig_exporter(), "bulk_signatures_export_scores_csv()")
  if (is.null(x)) return(invisible(NULL))

  st <- .sig_state(with_result = FALSE)
  r <- x(st$rv, st$gd, tempfile("ts-sig-"))
  expect_false(isTRUE(r$ok))
  expect_identical(r$status, "invalid")
  expect_null(r$descriptor)
  expect_match(paste(r$errors, collapse = " "), "bulk-signatures-run_signatures",
               fixed = TRUE,
               info = "the refusal must name the action that produces the artefact")

  # A non-canonical stored object: the BUILDER's verdict, mapped to an honest
  # invalid — never a thrown error through the poller.
  st2 <- .sig_state()
  st2$rv$signature_scores <- list(type = "something_else")
  r2 <- x(st2$rv, st2$gd, tempfile("ts-sig-"))
  expect_false(isTRUE(r2$ok))
  expect_identical(r2$status, "invalid")
  expect_match(paste(r2$errors, collapse = " "), "not canonical", fixed = TRUE)
})

# =============================================================================
# 2. The export writes the REAL grid, disclaimer included
# =============================================================================
test_that("the bulk_signatures export writes a parseable grid equal to the builder's", {
  x <- .sig_need(.sig_exporter(), "bulk_signatures_export_scores_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- tempfile("ts-sig-dir-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .sig_state()

  r <- x(st$rv, st$gd, d)
  expect_true(isTRUE(r$ok))
  expect_identical(r$status, "done")

  p <- file.path(d, r$descriptor$file)
  expect_true(file.exists(p))
  expect_gt(file.size(p), 0L)

  # Equal to the BUILDER's output — the exporter does not have its own schema.
  want <- build_signature_scores_export(.sig_result())
  back <- utils::read.csv(p, check.names = FALSE, stringsAsFactors = FALSE)
  expect_identical(nrow(back), .sig_n * .sig_s)
  expect_identical(names(back), c("signature", "sample", "score", "method",
                                  "analysis_id", "disclaimer"))
  expect_identical(back$signature, want$signature)
  expect_equal(back$score, want$score)

  # The §M3 DISCLAIMER travels with the drive export: it is in the FILE, and
  # on the wire it arrives redacted (10-char alnum run) — pinned, not hidden.
  # `signature` redacts (9-char run) but `analysis_id` survives: the token rule
  # needs a word boundary AFTER the 8+-char run, and the "_" of analysis_id is
  # itself a word character — the same asymmetry the sc_markers suite pins for
  # p_val_adj.
  expect_true(all(nzchar(back$disclaimer)))
  project <- .sig_need(.sig_project(), "ts_drive_project_descriptor()")
  wire <- project(r$descriptor, c("format", "file", "bytes", "n_rows",
                                  "n_cols", "columns"))
  expect_identical(wire$columns,
                   c("<redacted>", "sample", "score", "method",
                     "analysis_id", "<redacted>"))
})

# =============================================================================
# 3. The filename leaks NOTHING the caller chose — the method label included
# =============================================================================
test_that("the bulk_signatures export filename is app-derived and carries no method label", {
  x <- .sig_need(.sig_exporter(), "bulk_signatures_export_scores_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- tempfile("ts-sig-name-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .sig_state()

  r <- x(st$rv, st$gd, d)
  expect_true(isTRUE(r$ok))
  f <- r$descriptor$file
  # The HUMAN filename embeds `signature_scores_<method>_<date>` — the method
  # is a session value a drive run itself selects, so the drive file must not.
  expect_false(grepl("ssgsea", f, fixed = TRUE))
  expect_match(f, "\\.csv$")

  stem <- .sig_stem()
  if (!is.null(stem)) {
    expect_identical(stem, "bulk_signatures_scores")
    expect_match(f, paste0("^", stem, "_[0-9]+\\.csv$"))
  }

  np <- .sig_need(.sig_next_path(), "bulk_signatures_export_next_path()")
  expect_match(basename(np(d)), paste0("^", stem, "_2\\.csv$"))
})

# =============================================================================
# 4. Repeated exports are distinct and BOUNDED — the stem is ours
# =============================================================================
test_that("repeated bulk_signatures exports are distinct and the pruner owns the stem", {
  x <- .sig_need(.sig_exporter(), "bulk_signatures_export_scores_csv()")
  if (is.null(x)) return(invisible(NULL))
  prune <- .sig_need(.sig_prune(), "ts_drive_export_prune()")
  d <- tempfile("ts-sig-bounded-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .sig_state()

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
    p <- sprintf(file.path(d, "bulk_signatures_scores_%d.csv"), 100L + i)
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

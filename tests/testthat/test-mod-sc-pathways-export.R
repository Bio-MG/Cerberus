# =============================================================================
# test-mod-sc-pathways-export.R — S2c: the `sc_pathways` drive export route
# =============================================================================
# The SC-PATHWAYS twin of test-mod-bulk-pathways-export.R, and deliberately its
# mirror. One route, ONE artefact (the ORA enrichment table stored under the SC
# shared state's `shared_rv$pathway_results`, written by BOTH the panel's
# button and the drive run path), and the client still chooses nothing.
#
# Two facts pinned here, not assumed:
#   * the drive filename must NOT embed the pathway database label
#     (GOBP / KEGG / Reactome) the human filename embeds — it is a UI choice a
#     drive run itself selects;
#   * bulk and SC hold DIFFERENT shared states, so this route can never export
#     the bulk module's table: the store read here is the SC one.
# =============================================================================

source_project_file("config/defaults.R")
source_project_file("R/core/io_helpers.R")            # %||%
source_project_file("R/core/error_state.R")
source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")
source_project_file("modules/sc/mod_sc_pathways_export.R")

if (!exists(".tr", envir = globalenv()))
  assign(".tr", function(key) key, envir = globalenv())

# --- Fixtures ----------------------------------------------------------------
# An ORA-shaped enrichment table — SC runs run_pathway_enrichment() only
# (R/core/pathway_helpers.R column contract).
.spw_n <- 25L

.spw_record <- function(n = .spw_n) {
  set.seed(4242)
  df <- data.frame(ID = sprintf("KEGG%05d", seq_len(n)),
                   Description = sprintf("pathway %d", seq_len(n)),
                   GeneRatio = sprintf("%d/%d", 5L + seq_len(n) %% 5L, 300L),
                   BgRatio = sprintf("%d/%d", 300L + seq_len(n) %% 20L, 8000L),
                   pvalue = stats::runif(n, 0, 0.05),
                   p.adjust = stats::runif(n, 0, 0.05),
                   qvalue = stats::runif(n, 0, 0.05),
                   geneID = sprintf("G%04d/G%04d", seq_len(n), 1000L + seq_len(n)),
                   Count = 5L + seq_len(n) %% 5L,
                   stringsAsFactors = FALSE)
  attr(df, "enrich_obj") <- list(fake = TRUE)
  df
}

.spw_state <- function(with_result = TRUE) {
  gd <- shiny::reactiveValues()
  gd$language <- "fr"
  rv <- shiny::reactiveValues()
  if (with_result) rv$pathway_results <- .spw_record()
  list(gd = gd, rv = rv)
}

.spw_exporter <- function() get0("sc_pathways_export_enrichment_csv", envir = globalenv())
.spw_next_path <- function() get0("sc_pathways_export_next_path", envir = globalenv())
.spw_stem <- function() get0("TS_DRIVE_EXPORT_STEM_SC_PATHWAYS", envir = globalenv())
.spw_project <- function() get0("ts_drive_project_descriptor", envir = globalenv())
.spw_prune <- function() get0("ts_drive_export_prune", envir = globalenv())

.spw_need <- function(fn, what) {
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
test_that("the sc_pathways exporter returns an INVALID verdict, never a throw", {
  x <- .spw_need(.spw_exporter(), "sc_pathways_export_enrichment_csv()")
  if (is.null(x)) return(invisible(NULL))

  st <- .spw_state(with_result = FALSE)
  r <- x(st$rv, st$gd, tempfile("ts-spw-"))
  expect_false(isTRUE(r$ok))
  expect_identical(r$status, "invalid")
  expect_null(r$descriptor)
  expect_match(paste(r$errors, collapse = " "), "sc-pathways-run_pathway",
               fixed = TRUE,
               info = "the refusal must name the action that produces the artefact")

  st2 <- .spw_state()
  st2$rv$pathway_results <- data.frame(foo = 1:3)
  r2 <- x(st2$rv, st2$gd, tempfile("ts-spw-"))
  expect_false(isTRUE(r2$ok))
  expect_identical(r2$status, "invalid")
  expect_match(paste(r2$errors, collapse = " "),
               "not a pathway results table", fixed = TRUE)
})

# =============================================================================
# 2. The export writes the REAL table, into the bounded directory
# =============================================================================
test_that("the sc_pathways export writes a parseable table equal to the stored one", {
  x <- .spw_need(.spw_exporter(), "sc_pathways_export_enrichment_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- tempfile("ts-spw-dir-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .spw_state()

  r <- x(st$rv, st$gd, d)
  expect_true(isTRUE(r$ok))
  expect_identical(r$status, "done")

  p <- file.path(d, r$descriptor$file)
  expect_true(file.exists(p))
  expect_gt(file.size(p), 0L)
  expect_identical(as.integer(file.size(p)), as.integer(r$descriptor$bytes))

  back <- utils::read.csv(p, stringsAsFactors = FALSE)
  expect_identical(nrow(back), .spw_n)
  expect_setequal(names(back),
                  c("ID", "Description", "GeneRatio", "BgRatio", "pvalue",
                    "p.adjust", "qvalue", "geneID", "Count"))
  expect_identical(back$ID, shiny::isolate(st$rv$pathway_results)$ID)
  # A CSV round-trip is lossy for doubles (15 significant digits), so the
  # numeric column compares EQUAL, not identical.
  expect_equal(back$p.adjust, shiny::isolate(st$rv$pathway_results)$p.adjust)

  expect_setequal(names(r$descriptor),
                  c("format", "file", "bytes", "n_rows", "n_cols", "columns"))
  project <- .spw_need(.spw_project(), "ts_drive_project_descriptor()")
  wire <- project(r$descriptor, c("format", "file", "bytes", "n_rows",
                                  "n_cols", "n_sig", "columns"))
  # Same wire shape as the bulk_pathways route: "Description" and "GeneRatio"
  # arrive redacted (8+-char alphanumeric runs), everything else verbatim.
  expect_identical(wire$columns,
                   c("ID", "<redacted>", "<redacted>", "BgRatio", "pvalue",
                     "p.adjust", "qvalue", "geneID", "Count"))
  expect_equal(wire[names(wire) != "columns"],
               r$descriptor[names(r$descriptor) != "columns"])
})

# =============================================================================
# 3. The filename leaks NOTHING the caller chose — PINNED, including the DB
# =============================================================================
test_that("the sc_pathways export filename is app-derived and carries no database label", {
  x <- .spw_need(.spw_exporter(), "sc_pathways_export_enrichment_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- tempfile("ts-spw-name-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .spw_state()

  r <- x(st$rv, st$gd, d)
  expect_true(isTRUE(r$ok))
  f <- r$descriptor$file
  # The label the HUMAN filename embeds (mod_sc_pathways.R) is a UI choice a
  # drive run itself selects — it must not appear.
  expect_false(grepl("GOBP", f, fixed = TRUE))
  expect_false(grepl("KEGG", f, fixed = TRUE))
  expect_false(grepl("Reactome", f, fixed = TRUE))
  expect_match(f, "\\.csv$")
  expect_false(grepl("/", f, fixed = TRUE))
  expect_false(grepl("\\\\", f, fixed = TRUE))
  expect_true(file.exists(file.path(d, f)))

  # PINNED TO THE DERIVED PATTERN, on the spatial_qc rule.
  stem <- .spw_stem()
  if (!is.null(stem)) {
    expect_identical(stem, "sc_pathways_enrichment")
    expect_match(f, paste0("^", stem, "_[0-9]+\\.csv$"))
    expect_false(grepl("%", stem, fixed = TRUE))
  }

  # And the next-path resolver is its stateless twin.
  np <- .spw_need(.spw_next_path(), "sc_pathways_export_next_path()")
  expect_match(basename(np(d)), paste0("^", stem, "_2\\.csv$"))
})

# =============================================================================
# 4. Repeated exports are distinct, byte-identical, and BOUNDED — ACROSS routes
# =============================================================================
test_that("repeated sc_pathways exports are distinct and the pruner unions ALL FIVE stems", {
  x <- .spw_need(.spw_exporter(), "sc_pathways_export_enrichment_csv()")
  if (is.null(x)) return(invisible(NULL))
  prune <- .spw_need(.spw_prune(), "ts_drive_export_prune()")
  d <- tempfile("ts-spw-bounded-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .spw_state()

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

  # The pruner must recognise files of ALL FIVE route stems; here the FIFTH
  # stem's membership in the ours-filter is what is asserted. Mtimes are SET
  # explicitly, because the victims are chosen by modification time and ties
  # would make the verdict ambiguous.
  foreign <- file.path(d, "operator_note.csv")
  writeLines("keep me", foreign)
  t0 <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  Sys.setFileTime(f1, t0)                       # oldest, OURS (sc_pathways _1)
  Sys.setFileTime(file.path(d, again$descriptor$file), t0 + 1)  # OURS
  Sys.setFileTime(foreign, t0 + 2)              # NOT ours
  for (i in seq_len(10L)) {
    p <- sprintf(file.path(d, "sc_pathways_enrichment_%d.csv"), 100L + i)
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

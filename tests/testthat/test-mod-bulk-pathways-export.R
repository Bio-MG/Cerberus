# =============================================================================
# test-mod-bulk-pathways-export.R — S2c: the `bulk_pathways` drive export route
# =============================================================================
# The Bulk-PATHWAYS twin of test-mod-bulk-de-export.R, and deliberately its
# mirror. One route, ONE artefact (the enrichment table the module stored after
# whichever of ORA / GSEA ran last), and the client still chooses nothing: not
# the destination, not the filename, not the format, not the MODE. The route is
# bound to the live session's own `shared_rv$pathway_results`, it writes only
# into an application-controlled bounded temporary directory, and it returns a
# REDACTED descriptor.
#
# Two DELIBERATE facts pinned here, not assumed:
#   * the descriptor has NO `n_sig` (the sibling exporters' rule — a
#     "significant pathway" count depends on a cutoff the caller does not see);
#   * ONE route covers BOTH modes: the shape guard pins the INTERSECTION of the
#     two column contracts (ID/Description/pvalue/p.adjust), and the GSEA-shaped
#     table exports through the same code path as the ORA-shaped one.
# =============================================================================

source_project_file("config/defaults.R")
source_project_file("R/core/io_helpers.R")            # %||%
source_project_file("R/core/error_state.R")
source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")
source_project_file("modules/bulk/mod_bulk_pathways_export.R")

if (!exists(".tr", envir = globalenv()))
  assign(".tr", function(key) key, envir = globalenv())

# --- Fixtures ----------------------------------------------------------------
# An ORA-shaped enrichment table: the exact shape run_pathway_enrichment()
# returns (pathway_helpers.R: as.data.frame(enrich_result), 9 columns), plus
# the `enrich_obj` attribute the module stores with it — the export writes the
# TABLE, and the attribute must ride along as nothing.
.pw_n <- 30L

.pw_ora_record <- function(n = .pw_n) {
  set.seed(4242)
  df <- data.frame(ID = sprintf("GO:%07d", seq_len(n)),
                   Description = sprintf("pathway %d", seq_len(n)),
                   GeneRatio = sprintf("%d/%d", 5L + seq_len(n) %% 5L, 200L),
                   BgRatio = sprintf("%d/%d", 200L + seq_len(n) %% 20L, 12000L),
                   pvalue = stats::runif(n, 0, 0.05),
                   p.adjust = stats::runif(n, 0, 0.05),
                   qvalue = stats::runif(n, 0, 0.05),
                   geneID = sprintf("G%04d/G%04d", seq_len(n), 1000L + seq_len(n)),
                   Count = 5L + seq_len(n) %% 5L,
                   stringsAsFactors = FALSE)
  # The module stores the raw clusterProfiler object as an attribute
  # (pathway_helpers.R). A plain list stands in for the S4 here; the point is
  # that attributes exist and must not reach the file.
  attr(df, "enrich_obj") <- list(fake = TRUE)
  df
}

# A GSEA-shaped table: run_gsea_enrichment() adds setSize/enrichmentScore/NES
# and ALIASES Count/GeneRatio in (pathway_helpers.R). One route covers it too.
.pw_gsea_record <- function(n = .pw_n) {
  set.seed(4242)
  df <- data.frame(ID = sprintf("GO:%07d", seq_len(n)),
                   Description = sprintf("pathway %d", seq_len(n)),
                   setSize = 40L + seq_len(n) %% 10L,
                   enrichmentScore = stats::runif(n, -0.8, 0.8),
                   NES = stats::rnorm(n, 0, 1.5),
                   pvalue = stats::runif(n, 0, 0.05),
                   p.adjust = stats::runif(n, 0, 0.05),
                   stringsAsFactors = FALSE)
  df$Count     <- df$setSize
  df$GeneRatio <- paste0(df$setSize, "/", 12000L)
  attr(df, "gsea_obj") <- list(fake = TRUE)
  df
}

#' The state a live `bulk_pathways` session holds after
#' `bulk-pathways-run_pathway`: ONE stored enrichment table. There is no
#' caller-choosable label in the artefact's identity (the module resets the
#' store before each run), so the filename pin is the STEM pattern itself.
.pw_state <- function(record = .pw_ora_record()) {
  gd <- shiny::reactiveValues()
  gd$language <- "fr"
  rv <- shiny::reactiveValues()
  rv$pathway_results <- record
  list(gd = gd, rv = rv)
}

.pw_exporter <- function() get0("bulk_pathways_export_enrichment_csv", envir = globalenv())
.pw_next_path <- function() get0("bulk_pathways_export_next_path", envir = globalenv())
.pw_stem <- function() get0("TS_DRIVE_EXPORT_STEM_BULK_PATHWAYS", envir = globalenv())
.pw_project <- function() get0("ts_drive_project_descriptor", envir = globalenv())
.pw_prune <- function() get0("ts_drive_export_prune", envir = globalenv())

.pw_need <- function(fn, what) {
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
test_that("the bulk_pathways exporter returns an INVALID verdict, never a throw", {
  x <- .pw_need(.pw_exporter(), "bulk_pathways_export_enrichment_csv()")
  if (is.null(x)) return(invisible(NULL))

  # No enrichment run at all: the session is live, nothing has been stored.
  st <- .pw_state(record = NULL)
  r <- x(st$rv, st$gd, tempfile("ts-pw-"))
  expect_false(isTRUE(r$ok))
  expect_identical(r$status, "invalid")
  expect_null(r$descriptor)
  expect_match(paste(r$errors, collapse = " "), "bulk-pathways-run_pathway",
               fixed = TRUE,
               info = "the refusal must name the action that produces the artefact")

  # A stored object that is NOT a pathway results table: refused, not written.
  st2 <- .pw_state(record = data.frame(foo = 1:3))
  r2 <- x(st2$rv, st2$gd, tempfile("ts-pw-"))
  expect_false(isTRUE(r2$ok))
  expect_identical(r2$status, "invalid")
  expect_match(paste(r2$errors, collapse = " "),
               "not a pathway results table", fixed = TRUE)

  # The ZERO-ROW frame pathway_helpers.R builds for an empty ORA result would
  # fail the same guard (it lacks `pvalue`) — and a 0-row file is no artefact
  # anyway. The module stores NULL for that case, but the guard holds on its
  # own terms too.
  st3 <- .pw_state(record = data.frame(ID = character(0), Description = character(0),
                                       p.adjust = numeric(0), Count = integer(0),
                                       GeneRatio = character(0)))
  r3 <- x(st3$rv, st3$gd, tempfile("ts-pw-"))
  expect_false(isTRUE(r3$ok))
  expect_identical(r3$status, "invalid")
})

# =============================================================================
# 2. The export writes the REAL table — in BOTH modes
# =============================================================================
test_that("the bulk_pathways export writes a parseable ORA table equal to the stored one", {
  x <- .pw_need(.pw_exporter(), "bulk_pathways_export_enrichment_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- tempfile("ts-pw-dir-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .pw_state()

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
  expect_identical(nrow(back), .pw_n)
  expect_setequal(names(back),
                  c("ID", "Description", "GeneRatio", "BgRatio", "pvalue",
                    "p.adjust", "qvalue", "geneID", "Count"))
  expect_identical(back$ID, shiny::isolate(st$rv$pathway_results)$ID)
  # A CSV round-trip is lossy for doubles (15 significant digits), so the
  # numeric column compares EQUAL, not identical — same values to print precision.
  expect_equal(back$p.adjust, shiny::isolate(st$rv$pathway_results)$p.adjust)

  # The descriptor is REDACTED and HONEST: every key is inside the wire's frozen
  # keep-set, and there is NO `n_sig` — a cutoff-dependent count the reader
  # could not define. Asserted as an ABSENCE, not left implicit.
  expect_setequal(names(r$descriptor),
                  c("format", "file", "bytes", "n_rows", "n_cols", "columns"))
  project <- .pw_need(.pw_project(), "ts_drive_project_descriptor()")
  wire <- project(r$descriptor, c("format", "file", "bytes", "n_rows",
                                  "n_cols", "n_sig", "columns"))
  # On the wire, `columns` goes through the sanitiser BY DESIGN ("a column name
  # can be user-supplied" — drive_watcher.R): "Description" and "GeneRatio" are
  # 8+-char alphanumeric runs, so they arrive redacted. PINNED, not hidden.
  expect_identical(wire$columns,
                   c("ID", "<redacted>", "<redacted>", "BgRatio", "pvalue",
                     "p.adjust", "qvalue", "geneID", "Count"))
  # The projection COERCES numerics to double (its numeric branch), so the
  # remaining fields compare EQUAL, not identical — same values, wire types.
  expect_equal(wire[names(wire) != "columns"],
               r$descriptor[names(r$descriptor) != "columns"])
})

test_that("the SAME route exports a GSEA-shaped table — one route, both modes", {
  x <- .pw_need(.pw_exporter(), "bulk_pathways_export_enrichment_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- tempfile("ts-pw-gsea-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .pw_state(record = .pw_gsea_record())

  r <- x(st$rv, st$gd, d)
  expect_true(isTRUE(r$ok))
  expect_identical(r$status, "done")
  p <- file.path(d, r$descriptor$file)
  back <- utils::read.csv(p, stringsAsFactors = FALSE)
  expect_identical(nrow(back), .pw_n)
  expect_setequal(names(back),
                  c("ID", "Description", "setSize", "enrichmentScore", "NES",
                    "pvalue", "p.adjust", "Count", "GeneRatio"))
})

# =============================================================================
# 3. The filename leaks NOTHING the caller chose, and is PINNED to the stem
# =============================================================================
test_that("the bulk_pathways export filename is app-derived and stem-pinned", {
  x <- .pw_need(.pw_exporter(), "bulk_pathways_export_enrichment_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- tempfile("ts-pw-name-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .pw_state()

  r <- x(st$rv, st$gd, d)
  expect_true(isTRUE(r$ok))
  f <- r$descriptor$file
  expect_match(f, "\\.csv$")
  expect_false(grepl("/", f, fixed = TRUE))
  expect_false(grepl("\\\\", f, fixed = TRUE))
  expect_true(file.exists(file.path(d, f)))

  # PINNED TO THE DERIVED PATTERN, on the spatial_qc rule: a fixed string that
  # merely avoids leaks would still pass with any other stem.
  stem <- .pw_stem()
  if (!is.null(stem)) {
    expect_identical(stem, "bulk_pathways_enrichment")
    expect_match(f, paste0("^", stem, "_[0-9]+\\.csv$"))
    expect_false(grepl("%", stem, fixed = TRUE))
  }

  # And the next-path resolver is its stateless twin: index one above the
  # highest present, never a counter.
  np <- .pw_need(.pw_next_path(), "bulk_pathways_export_next_path()")
  expect_match(basename(np(d)), paste0("^", stem, "_2\\.csv$"))
})

# =============================================================================
# 4. Repeated exports are distinct, byte-identical, and BOUNDED — ACROSS routes
# =============================================================================
test_that("repeated bulk_pathways exports are distinct and the pruner unions the route stems", {
  x <- .pw_need(.pw_exporter(), "bulk_pathways_export_enrichment_csv()")
  if (is.null(x)) return(invisible(NULL))
  prune <- .pw_need(.pw_prune(), "ts_drive_export_prune()")
  d <- tempfile("ts-pw-bounded-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .pw_state()

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

  # The pruner must recognise files of ALL THREE route stems: a pruner keyed to
  # the first two would let bulk_pathways grow the directory past the cap
  # forever. The cap itself is pinned in the spatial_qc suite; here the THIRD
  # stem's membership in the ours-filter is what is asserted. Mtimes are SET
  # explicitly, because the victims are chosen by modification time and ties
  # would make the verdict ambiguous.
  foreign <- file.path(d, "operator_note.csv")
  writeLines("keep me", foreign)
  t0 <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  Sys.setFileTime(f1, t0)                       # oldest, OURS (bulk_pathways _1)
  Sys.setFileTime(file.path(d, again$descriptor$file), t0 + 1)  # OURS
  Sys.setFileTime(foreign, t0 + 2)              # NOT ours
  for (i in seq_len(10L)) {
    p <- sprintf(file.path(d, "bulk_pathways_enrichment_%d.csv"), 100L + i)
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

# =============================================================================
# test-drive-export-columns.R — the per-route column CONTRACT (S2c follow-up,
# option 1 of the 2026-10-03 redaction assessment)
# =============================================================================
# The export descriptor's `columns` arrives on the wire through the app's
# sanitiser, which redacts every 8+-character alphanumeric name ("a column name
# can be user-supplied" — drive_watcher.R). The file is the artefact, but for a
# caller that cannot read it, the descriptor is the only schema knowledge — so
# the app now DECLARES each route's column contract as frozen data
# (`TS_DRIVE_EXPORT_COLUMNS`, drive_allowlist.R), the MCP server mirrors it in
# the export tool description, and `--check` cross-checks the two copies.
#
# THIS FILE pins the declared contract against what the exporters actually
# produce, so the declaration cannot drift from the artefact:
#   * `fixed` / `mode_*` routes (deterministic schema): the descriptor's
#     columns EQUAL the declared vector, in order;
#   * `guaranteed` routes (engine extras ride along, order varies): every
#     declared name is present, and NO full list is promised — the descriptor
#     keeps the stored order.
# The wire redaction itself is pinned in the per-route export suites.
# =============================================================================

source_project_file("config/defaults.R")
source_project_file("R/core/io_helpers.R")            # %||%
source_project_file("R/core/error_state.R")
source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")
source_project_file("modules/bulk_de/mod_bulk_de_export.R")
source_project_file("modules/bulk/mod_bulk_pathways_export.R")
source_project_file("modules/sc/mod_sc_markers_export.R")
source_project_file("modules/sc/mod_sc_pathways_export.R")
source_project_file("R/bulk/bulk_signatures.R")
source_project_file("R/bulk/bulk_pattern.R")
source_project_file("R/bulk/bulk_network.R")
source_project_file("modules/bulk/mod_bulk_filter_export.R")
source_project_file("modules/bulk/mod_bulk_signatures_export.R")
source_project_file("modules/bulk/mod_bulk_pattern_export.R")
source_project_file("modules/bulk/mod_bulk_network_export.R")

if (!exists(".tr", envir = globalenv()))
  assign(".tr", function(key) key, envir = globalenv())

#' Resolver, not a bare reference: a missing constant must read as a MISSING
#' CONTRACT, not as a broken test file (same reason as the S1.5/S2 resolvers).
.contract <- function() get0("TS_DRIVE_EXPORT_COLUMNS", envir = globalenv())

# --- Fixtures ----------------------------------------------------------------
# Each fixture is RUNTIME-shaped: the columns and order a real producer stores
# (extract_deseq2_contrast, clusterProfiler's enrichResult/gseaResult frames,
# Seurat's FindAllMarkers). Where the contract promises a FULL list, the
# fixture carries exactly that list; where it promises only a guaranteed set,
# the fixture carries the extras the engine would add, and the assertion is
# membership, not equality.

.de_fix <- function() {
  set.seed(4242)
  n <- 12L
  # DESeq2 frame as extract_deseq2_contrast stores it: as.data.frame(results())
  # (baseMean, log2FoldChange, lfcSE, stat, pvalue, padj), then `gene` appended,
  # then ordered by padj. The engine extras (lfcSE, stat) are why bulk_de may
  # only promise a guaranteed SET.
  data.frame(baseMean = stats::runif(n, 1, 2000),
             log2FoldChange = stats::rnorm(n, 0, 2),
             lfcSE = stats::runif(n, 0.1, 1),
             stat = stats::rnorm(n, 0, 3),
             pvalue = stats::runif(n, 0, 1),
             padj = stats::runif(n, 0, 1),
             gene = sprintf("G%04d", seq_len(n)),
             stringsAsFactors = FALSE)
}

.pw_ora_fix <- function() {
  n <- 8L
  data.frame(ID = sprintf("GO:%07d", seq_len(n)),
             Description = sprintf("pathway %d", seq_len(n)),
             GeneRatio = sprintf("%d/%d", 5L, 200L),
             BgRatio = sprintf("%d/%d", 210L, 12000L),
             pvalue = stats::runif(n, 0, 0.05),
             p.adjust = stats::runif(n, 0, 0.05),
             qvalue = stats::runif(n, 0, 0.05),
             geneID = sprintf("G%04d/G%04d", seq_len(n), 1000L + seq_len(n)),
             Count = 5L,
             stringsAsFactors = FALSE)
}

.pw_gsea_fix <- function() {
  n <- 8L
  df <- data.frame(ID = sprintf("GO:%07d", seq_len(n)),
                   Description = sprintf("pathway %d", seq_len(n)),
                   setSize = 40L + seq_len(n),
                   enrichmentScore = stats::runif(n, -0.8, 0.8),
                   NES = stats::rnorm(n, 0, 1.5),
                   pvalue = stats::runif(n, 0, 0.05),
                   p.adjust = stats::runif(n, 0, 0.05),
                   qvalue = stats::runif(n, 0, 0.05),
                   rank = seq_len(n),
                   core_enrichment = rep(c("yes", "no"), length.out = n),
                   stringsAsFactors = FALSE)
  # run_gsea_enrichment() ALIASES these two in for the table consumers.
  df$Count     <- df$setSize
  df$GeneRatio <- paste0(df$setSize, "/", 12000L)
  df
}

.mk_fix <- function() {
  n <- 12L
  # Seurat FindAllMarkers frame as .normalize_marker_cols() leaves it: the
  # standard names, engine order. Order is deliberately NOT promised by the
  # contract — only the guaranteed set is.
  data.frame(p_val = stats::runif(n, 0, 0.1),
             avg_log2FC = stats::rnorm(n, 1, 0.8),
             pct.1 = stats::runif(n, 0.1, 1),
             pct.2 = stats::runif(n, 0, 0.5),
             p_val_adj = stats::runif(n, 0, 0.05),
             cluster = sprintf("c%d", seq_len(n) %% 4L),
             gene = sprintf("G%04d", seq_len(n)),
             stringsAsFactors = FALSE)
}

.state <- function(df, slot) {
  gd <- shiny::reactiveValues()
  gd$language <- "fr"
  rv <- shiny::reactiveValues()
  if (identical(slot, "de")) {
    rv$contrasts <- list(fixture_contrast = df)
    rv$active_contrast <- "fixture_contrast"
  } else {
    rv[[slot]] <- df
  }
  list(gd = gd, rv = rv)
}

.need <- function(fn, what) {
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
# 1. The contract table exists, covers EVERY route, and is well-formed
# =============================================================================
test_that("the column contract table covers every route and is well-formed", {
  ct <- .contract()
  if (is.null(ct)) {
    testthat::fail("TS_DRIVE_EXPORT_COLUMNS must exist — the per-route column contract is declared data")
    return(invisible(NULL))
  }
  expect_setequal(names(ct), names(TS_DRIVE_EXPORT_ROUTES))
  testthat::expect_true(setequal(names(ct), names(TS_DRIVE_EXPORT_ROUTES)),
    info = "every export route must declare its column contract")
  for (rt in names(ct)) {
    e <- ct[[rt]]
    expect_true(is.list(e), info = sprintf("%s: contract must be a list", rt))
    has_full <- !is.null(e$fixed) || !is.null(e$mode_ora) || !is.null(e$mode_gsea)
    expect_true(has_full || !is.null(e$guaranteed),
                info = sprintf("%s: contract must declare `fixed`/`mode_*` and/or `guaranteed`", rt))
    for (k in names(e)) {
      expect_type(e[[k]], "character")
      expect_true(length(e[[k]]) > 0L, info = sprintf("%s$%s: empty contract", rt, k))
      expect_false(anyNA(e[[k]]), info = sprintf("%s$%s: NA in contract", rt, k))
    }
    # A route that declares BOTH a guaranteed set and full lists must have the
    # guaranteed set INSIDE every full list — the full list without the identity
    # columns would be a contract that its own shape guard refutes.
    if (!is.null(e$guaranteed) && !is.null(e$mode_ora)) {
      expect_true(all(e$guaranteed %in% e$mode_ora),
                  info = sprintf("%s: guaranteed columns must be inside mode_ora", rt))
    }
    if (!is.null(e$guaranteed) && !is.null(e$mode_gsea)) {
      expect_true(all(e$guaranteed %in% e$mode_gsea),
                  info = sprintf("%s: guaranteed columns must be inside mode_gsea", rt))
    }
  }
})

# =============================================================================
# 2. The deterministic routes: descriptor columns EQUAL the declared contract
# =============================================================================
test_that("bulk_pathways descriptor columns equal the declared contract, per mode", {
  ct <- .contract()
  if (is.null(ct)) {
    testthat::fail("TS_DRIVE_EXPORT_COLUMNS must exist")
    return(invisible(NULL))
  }
  x <- .need(get0("bulk_pathways_export_enrichment_csv", envir = globalenv()),
             "bulk_pathways_export_enrichment_csv()")
  if (is.null(x)) return(invisible(NULL))

  d <- tempfile("ts-ct-pw-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)

  st_ora <- .state(.pw_ora_fix(), "pathway_results")
  r_ora <- x(st_ora$rv, st_ora$gd, d)
  expect_true(isTRUE(r_ora$ok))
  expect_identical(r_ora$descriptor$columns, ct$bulk_pathways$mode_ora,
                   info = "ORA mode: the wire-order contract must equal the stored order")

  st_gsea <- .state(.pw_gsea_fix(), "pathway_results")
  r_gsea <- x(st_gsea$rv, st_gsea$gd, d)
  expect_true(isTRUE(r_gsea$ok))
  expect_identical(r_gsea$descriptor$columns, ct$bulk_pathways$mode_gsea,
                   info = "GSEA mode: the wire-order contract must equal the stored order")
})

test_that("sc_pathways descriptor columns equal the declared contract", {
  ct <- .contract()
  if (is.null(ct)) {
    testthat::fail("TS_DRIVE_EXPORT_COLUMNS must exist")
    return(invisible(NULL))
  }
  x <- .need(get0("sc_pathways_export_enrichment_csv", envir = globalenv()),
             "sc_pathways_export_enrichment_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- tempfile("ts-ct-spw-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .state(.pw_ora_fix(), "pathway_results")
  r <- x(st$rv, st$gd, d)
  expect_true(isTRUE(r$ok))
  expect_identical(r$descriptor$columns, ct$sc_pathways$mode_ora)
})

# =============================================================================
# 3. The engine-variable routes: guaranteed set present, ORDER NOT PROMISED
# =============================================================================
test_that("bulk_de descriptor carries the guaranteed set and keeps the stored order", {
  ct <- .contract()
  if (is.null(ct)) {
    testthat::fail("TS_DRIVE_EXPORT_COLUMNS must exist")
    return(invisible(NULL))
  }
  x <- .need(get0("bulk_de_export_results_csv", envir = globalenv()),
             "bulk_de_export_results_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- tempfile("ts-ct-de-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .state(.de_fix(), "de")

  r <- x(st$rv, st$gd, d)
  expect_true(isTRUE(r$ok))
  cols <- r$descriptor$columns
  # Every guaranteed name present, wherever the engine put it...
  expect_true(all(ct$bulk_de$guaranteed %in% cols),
              info = "the guaranteed identity columns must survive every engine")
  # ...and the descriptor keeps the STORED order (the exporter's own contract:
  # as.character(names(df)) — the redaction happens on the wire, not here).
  expect_identical(cols, names(.de_fix()))
  # And bulk_de declares NO full list: with three engines the order is not
  # pinnable, so the contract must not promise one.
  expect_null(ct$bulk_de$fixed)
  expect_null(ct$bulk_de$mode_ora)
})

test_that("sc_markers descriptor carries the guaranteed set and keeps the stored order", {
  ct <- .contract()
  if (is.null(ct)) {
    testthat::fail("TS_DRIVE_EXPORT_COLUMNS must exist")
    return(invisible(NULL))
  }
  x <- .need(get0("sc_markers_export_table_csv", envir = globalenv()),
             "sc_markers_export_table_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- tempfile("ts-ct-mk-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .state(.mk_fix(), "markers_data")

  r <- x(st$rv, st$gd, d)
  expect_true(isTRUE(r$ok))
  cols <- r$descriptor$columns
  expect_true(all(ct$sc_markers$guaranteed %in% cols),
              info = "the normaliser's guaranteed columns must survive")
  expect_identical(cols, names(.mk_fix()))
  # Same rule as bulk_de: extras depend on the Seurat frame, so no full list.
  expect_null(ct$sc_markers$fixed)
})

# =============================================================================
# 4. Slice 2.3 routes: same discipline, four more artefacts
# =============================================================================
test_that("bulk_filter descriptor guarantees the gene column and keeps sample names", {
  ct <- .contract()
  if (is.null(ct)) {
    testthat::fail("TS_DRIVE_EXPORT_COLUMNS must exist")
    return(invisible(NULL))
  }
  x <- .need(get0("bulk_filter_export_vst_matrix_csv", envir = globalenv()),
             "bulk_filter_export_vst_matrix_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- tempfile("ts-ct-bf-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  m <- matrix(stats::rnorm(6 * 3), nrow = 6, ncol = 3,
              dimnames = list(sprintf("G%04d", 1:6),
                              c("ctrl-1", "ctrl-2", "CoV2-6h")))
  rv <- shiny::reactiveValues(); rv$vst_mat <- m
  gd <- shiny::reactiveValues()
  r <- x(rv, gd, d)
  expect_true(isTRUE(r$ok))
  # The file is `gene` + one column PER SAMPLE: only `gene` is guaranteed...
  expect_identical(r$descriptor$columns[[1]], "gene")
  expect_true(all(ct$bulk_filter$guaranteed %in% r$descriptor$columns))
  # ...and the sample columns keep the session's own names verbatim here.
  expect_identical(r$descriptor$columns[-1], colnames(m))
  # No full list is promised: the sample names are the session's, not a schema.
  expect_null(ct$bulk_filter$fixed)
})

test_that("bulk_signatures descriptor columns equal the declared contract", {
  ct <- .contract()
  if (is.null(ct)) {
    testthat::fail("TS_DRIVE_EXPORT_COLUMNS must exist")
    return(invisible(NULL))
  }
  x <- .need(get0("bulk_signatures_export_scores_csv", envir = globalenv()),
             "bulk_signatures_export_scores_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- tempfile("ts-ct-sig-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  res <- list(type = "bulk_signature_scores", status = "valid",
              scores = matrix(1:6, nrow = 2,
                              dimnames = list(c("sigA", "sigB"), c("s1", "s2", "s3")))[, 1:2, drop = FALSE],
              method = "ssgsea", analysis_id = "ct", disclaimer = "d")
  rv <- shiny::reactiveValues(); rv$signature_scores <- res
  gd <- shiny::reactiveValues()
  r <- x(rv, gd, d)
  expect_true(isTRUE(r$ok))
  expect_identical(r$descriptor$columns, ct$bulk_signatures$fixed)
})

test_that("bulk_pattern and bulk_network descriptor columns equal the declared contracts", {
  ct <- .contract()
  if (is.null(ct)) {
    testthat::fail("TS_DRIVE_EXPORT_COLUMNS must exist")
    return(invisible(NULL))
  }
  d <- tempfile("ts-ct-pn-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  gd <- shiny::reactiveValues()

  # bulk_pattern: the builder’s exactly-two-column contract.
  xp <- .need(get0("bulk_pattern_export_clusters_csv", envir = globalenv()),
              "bulk_pattern_export_clusters_csv()")
  if (is.null(xp)) return(invisible(NULL))
  rv_p <- shiny::reactiveValues()
  rv_p$pattern_result <- list(type = "bulk_pattern_clusters", status = "valid",
                              clusters = data.frame(gene = c("G1", "G2"),
                                                    cluster = c("c1", "c2"),
                                                    stringsAsFactors = FALSE))
  rp <- xp(rv_p, gd, d)
  expect_true(isTRUE(rp$ok))
  expect_identical(rp$descriptor$columns, ct$bulk_pattern$fixed)

  # bulk_network: the seven-column node table, degree DERIVED from edges —
  # the last node has NO edge on purpose (the tabulate/nb regression).
  xn <- .need(get0("bulk_network_export_nodes_csv", envir = globalenv()),
              "bulk_network_export_nodes_csv()")
  if (is.null(xn)) return(invisible(NULL))
  rv_n <- shiny::reactiveValues()
  rv_n$network_result <- list(
    type = "bulk_network_pcsf", status = "valid",
    nodes = data.frame(node = c("a", "b", "c", "d"),
                       symbol = c("T", "E", "M", "A"),
                       role = c("terminal", "relay", "terminal", "relay"),
                       prize = c(2.5, 1, 3, 0.5), stringsAsFactors = FALSE),
    edges = data.frame(from = c("a", "a", "c"), to = c("b", "c", "b"),
                       stringsAsFactors = FALSE),
    prizes = NULL, node_role = NULL, species = "human", source_db = "string",
    source_version = "v12", id_type = "ensembl_gene", map_rate = 0.9,
    parameters = list(beta = 1), qc = list(), warnings = character(0),
    provenance = "fixture", analysis_id = "ct", timestamp_utc = "t")
  rn <- xn(rv_n, gd, d)
  expect_true(isTRUE(rn$ok))
  expect_identical(rn$descriptor$columns, ct$bulk_network$fixed)
})

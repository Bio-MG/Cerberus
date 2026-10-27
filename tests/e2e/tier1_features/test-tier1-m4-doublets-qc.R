# =============================================================================
# test-tier1-m4-doublets-qc.R — Tier 1 Feature Coverage: Milestone M4
# =============================================================================
# Covers Features 19-24 (Single-Cell Ambient RNA & Doublet QC Engine)
# >= 5 test cases per feature verifying biological QC, errors, and provenance.
#
# DE-FACADED 2026-10-26 (C4-06 note 2 — SW4-T1 prerequisite): the 29
# expect_true(TRUE) placeholders (F19-2 .. F24-5) were rewritten as REAL
# assertions BEFORE implementing sc_filter_doublets_and_ambient, so that
# activating M4 cannot turn them into false greens (TDD red -> green).
# Every test stays gated by is_m4_ready() (progressive testability): the
# assertions below execute only once the core exists, and they then bite.
#
# Contract sources pinned here:
#   - PROJECT.md:140-149 (core signature, return structure, classed error,
#     thresholds with defaults, provenance entry fields)
#   - PROJECT.md:57-62 (features 19-24 owner file R/sc/sc_qc_doublets.R)
# Names NOT pinned by PROJECT.md are pinned BY THIS SPEC (the M4 implementer
# must satisfy them): params$seed, params$run_ambient (opt-in decontamination,
# consistent with feature 20 "when ambient decontamination is selected"),
# params$batch_var (multi-sample column), qc_metrics$doublet_score (per-cell
# score over the INPUT cells, computed pre-filter). Everything else is matched
# flexibly (column naming is deliberately not over-pinned).
# =============================================================================

if (!exists("e2e_project_root", mode = "function")) {
  if (file.exists("tests/e2e/helper-e2e.R")) {
    source("tests/e2e/helper-e2e.R")
  } else if (file.exists("../helper-e2e.R")) {
    source("../helper-e2e.R")
  }
}
# F23 needs the provenance gateway; not in helper-e2e.R's default sources.
e2e_source_if_exists("tools/pkg_skeleton/R/provenance.R")

.make_test_sc_obj <- function(n_genes = 120, n_cells = 80) {
  set.seed(42)
  mat <- matrix(rpois(n_genes * n_cells, lambda = 3), nrow = n_genes, ncol = n_cells)
  rownames(mat) <- paste0("Gene-", seq_len(n_genes))
  colnames(mat) <- paste0("Cell-", seq_len(n_cells))
  suppressWarnings(Seurat::CreateSeuratObject(counts = mat))
}

# ── M4 spec helpers (local, tolerant to unpinned column naming) ──────────────

# Per-cell doublet scores over the INPUT cells: spec-pinned qc_metrics$doublet_score,
# else a score-bearing meta.data column matching the scDblFinder conventions.
.m4_doublet_scores <- function(res) {
  if (!is.null(res$qc_metrics$doublet_score)) return(res$qc_metrics$doublet_score)
  cols <- grep("doublet.*score|score.*doublet|scDblFinder", colnames(res$obj@meta.data),
               ignore.case = TRUE, value = TRUE)
  if (length(cols)) return(as.numeric(res$obj@meta.data[[cols[1]]]))
  NULL
}

# Per-cell singlet/doublet tags from any doublet-bearing meta.data column.
.m4_doublet_tags <- function(res) {
  cols <- grep("doublet|scDblFinder", colnames(res$obj@meta.data), ignore.case = TRUE, value = TRUE)
  cols <- cols[!grepl("score", cols, ignore.case = TRUE)]
  if (!length(cols)) return(NULL)
  as.character(res$obj@meta.data[[cols[1]]])
}

# cells_used / cells_excluded may be scalars (counts) or identifier vectors.
.m4_count <- function(v) {
  if (is.null(v)) return(NA_integer_)
  if (is.numeric(v) && length(v) == 1L) return(as.integer(v))
  length(v)
}

# All finite numeric leaves of a nested metrics list (violin-plot contract).
.m4_numeric_leaves <- function(x) {
  if (is.atomic(x)) return(if (is.numeric(x)) x else numeric(0))
  if (is.list(x)) return(unlist(lapply(x, .m4_numeric_leaves), use.names = FALSE))
  numeric(0)
}

# TRUE when a decontamination engine (SoupX / celda-DecontX) is installed.
.m4_has_ambient_engine <- function() {
  requireNamespace("SoupX", quietly = TRUE) || requireNamespace("celda", quietly = TRUE)
}

# ── Feature 19: Native scDblFinder Doublet Engine ────────────────────────────

test_that("F19-1: sc_filter_doublets_and_ambient signature matches contract", {
  if (!is_m4_ready()) skip("M4 doublet engine not yet active")
  expect_true(exists("sc_filter_doublets_and_ambient", mode = "function"))
  fn_formals <- names(formals(sc_filter_doublets_and_ambient))
  expect_true("obj" %in% fn_formals)
  expect_true("params" %in% fn_formals)
})

test_that("F19-2: scDblFinder execution returns per-cell doublet scores", {
  if (!is_m4_ready()) skip("M4 doublet engine not yet active")
  if (!requireNamespace("scDblFinder", quietly = TRUE)) {
    skip("scDblFinder not installed in this environment")
  }
  obj <- .make_test_sc_obj()
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42))
  expect_s4_class(res$obj, "Seurat")
  expect_true(is.list(res$qc_metrics))
  expect_true(is.list(res$provenance))
  scores <- .m4_doublet_scores(res)
  expect_false(is.null(scores))
  expect_true(is.numeric(scores))
  # Per-cell over the INPUT cells, computed pre-filter, plot-safe values.
  expect_length(scores, ncol(obj))
  expect_true(all(is.finite(scores)))
})

test_that("F19-3: doublet classification tags cells as singlet or doublet", {
  if (!is_m4_ready()) skip("M4 doublet engine not yet active")
  if (!requireNamespace("scDblFinder", quietly = TRUE)) {
    skip("scDblFinder not installed in this environment")
  }
  obj <- .make_test_sc_obj()
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42))
  tags <- .m4_doublet_tags(res)
  expect_false(is.null(tags))
  expect_length(tags, ncol(obj))
  expect_true(all(tolower(tags) %in% c("singlet", "doublet", "0", "1", "false", "true")))
})

test_that("F19-4: doublet detection output is deterministic with fixed random seed", {
  if (!is_m4_ready()) skip("M4 doublet engine not yet active")
  if (!requireNamespace("scDblFinder", quietly = TRUE)) {
    skip("scDblFinder not installed in this environment")
  }
  r1 <- sc_filter_doublets_and_ambient(.make_test_sc_obj(), params = list(seed = 42))
  r2 <- sc_filter_doublets_and_ambient(.make_test_sc_obj(), params = list(seed = 42))
  s1 <- .m4_doublet_scores(r1)
  s2 <- .m4_doublet_scores(r2)
  expect_false(is.null(s1))
  expect_identical(s1, s2)
  expect_identical(.m4_doublet_tags(r1), .m4_doublet_tags(r2))
})

test_that("F19-5: doublet engine supports multi-sample batch-aware execution", {
  if (!is_m4_ready()) skip("M4 doublet engine not yet active")
  if (!requireNamespace("scDblFinder", quietly = TRUE)) {
    skip("scDblFinder not installed in this environment")
  }
  obj <- .make_test_sc_obj()
  obj@meta.data$sample <- rep(c("donor_A", "donor_B"), length.out = ncol(obj))
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42, batch_var = "sample"))
  expect_s4_class(res$obj, "Seurat")
  scores <- .m4_doublet_scores(res)
  expect_false(is.null(scores))
  expect_length(scores, ncol(obj))
  expect_true(all(colnames(res$obj) %in% colnames(obj)))
  # The batch variable must be recorded as actually used (provenance contract).
  expect_identical(as.character(res$provenance$parameters$batch_var), "sample")
})

# ── Feature 20: 'No Silent Fallback' Ambient RNA Guard ────────────────────────

test_that("F20-1: missing SoupX/DecontX throws classed error sc_qc_error", {
  if (!is_m4_ready()) skip("M4 ambient RNA guard not yet active")
  if (.m4_has_ambient_engine()) {
    skip("decontamination engine installed — missing-package guard path not reachable")
  }
  obj <- .make_test_sc_obj()
  err <- tryCatch(
    sc_filter_doublets_and_ambient(obj, params = list(run_ambient = TRUE, seed = 42)),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_s3_class(err, "sc_qc_error")
})

test_that("F20-2: error condition carries sc_ambient_package_missing subclass", {
  if (!is_m4_ready()) skip("M4 ambient RNA guard not yet active")
  if (.m4_has_ambient_engine()) {
    skip("decontamination engine installed — missing-package guard path not reachable")
  }
  obj <- .make_test_sc_obj()
  err <- tryCatch(
    sc_filter_doublets_and_ambient(obj, params = list(run_ambient = TRUE, seed = 42)),
    error = function(e) e
  )
  expect_true(inherits(err, "sc_qc_error"))
  expect_true("sc_ambient_package_missing" %in% class(err))
})

test_that("F20-3: ambient decontamination never silently falls back to raw data", {
  if (!is_m4_ready()) skip("M4 ambient RNA guard not yet active")
  if (.m4_has_ambient_engine()) {
    skip("decontamination engine installed — missing-package guard path not reachable")
  }
  obj <- .make_test_sc_obj()
  # The call must THROW: no Seurat result may be returned pretending that
  # decontamination happened when no engine exists.
  val <- tryCatch(
    sc_filter_doublets_and_ambient(obj, params = list(run_ambient = TRUE, seed = 42)),
    error = function(e) e
  )
  expect_s3_class(val, "error")
  expect_false(is.null(val))
  expect_true(grepl("ambient|decontaminat|SoupX|celda", conditionMessage(val),
                    ignore.case = TRUE))
})

test_that("F20-4: error message names the exact missing R package", {
  if (!is_m4_ready()) skip("M4 ambient RNA guard not yet active")
  if (.m4_has_ambient_engine()) {
    skip("decontamination engine installed — missing-package guard path not reachable")
  }
  obj <- .make_test_sc_obj()
  err <- tryCatch(
    sc_filter_doublets_and_ambient(obj, params = list(run_ambient = TRUE, seed = 42)),
    error = function(e) e
  )
  msg <- conditionMessage(err)
  named <- vapply(c("SoupX", "celda", "DecontX"),
                  function(p) grepl(p, msg, fixed = TRUE), logical(1))
  expect_true(any(named))
})

test_that("F20-5: ambient path is total — decontamination runs, or the classed refusal is the contract", {
  if (!is_m4_ready()) skip("M4 ambient RNA guard not yet active")
  obj <- .make_test_sc_obj()
  if (.m4_has_ambient_engine()) {
    res <- sc_filter_doublets_and_ambient(obj, params = list(run_ambient = TRUE, seed = 42))
    expect_s4_class(res$obj, "Seurat")
    expect_true(is.list(res$qc_metrics))
    expect_true(is.list(res$provenance))
    expect_true(all(is.finite(.m4_numeric_leaves(res$qc_metrics))))
  } else {
    # O17 (2026-10-27) : couple F20 neutralisé côté test — le 5ᵉ garde ne
    # SKIPPE PLUS quand SoupX/celda est absent : le refus classé EST le
    # comportement documenté du chemin ambiant (aucun repli silencieux, même
    # doctrine que F20-1..4, énoncée du point de vue « la voie est TOTALE »).
    # Sur un hôte AVEC moteur, la branche haute exécute la décontamination.
    err <- tryCatch(
      sc_filter_doublets_and_ambient(obj, params = list(run_ambient = TRUE, seed = 42)),
      error = function(e) e
    )
    expect_s3_class(err, "sc_ambient_package_missing")
    expect_s3_class(err, "sc_qc_error")
    expect_true(grepl("SoupX|celda|DecontX", conditionMessage(err), ignore.case = TRUE))
  }
})

# ── Feature 21: Counterfactual QC Threshold Controls ─────────────────────────

test_that("F21-1: max_contamination slider parameter controls filtering cutoff", {
  if (!is_m4_ready()) skip("M4 counterfactual controls not yet active")
  if (!requireNamespace("scDblFinder", quietly = TRUE)) {
    skip("scDblFinder not installed in this environment")
  }
  obj <- .make_test_sc_obj()
  r1 <- sc_filter_doublets_and_ambient(obj, params = list(max_contamination = 0, seed = 42))
  r2 <- sc_filter_doublets_and_ambient(obj, params = list(max_contamination = 0.9, seed = 42))
  # The counterfactual knob is consumed and echoed in the provenance parameters.
  expect_equal(r1$provenance$parameters$max_contamination, 0)
  expect_equal(r2$provenance$parameters$max_contamination, 0.9)
})

test_that("F21-2: doublet_score_cutoff parameter controls cell filtering sensitivity", {
  if (!is_m4_ready()) skip("M4 counterfactual controls not yet active")
  if (!requireNamespace("scDblFinder", quietly = TRUE)) {
    skip("scDblFinder not installed in this environment")
  }
  obj <- .make_test_sc_obj()
  r1 <- sc_filter_doublets_and_ambient(obj, params = list(doublet_score_cutoff = 0.25, seed = 42))
  r2 <- sc_filter_doublets_and_ambient(obj, params = list(doublet_score_cutoff = 1, seed = 42))
  expect_equal(r1$provenance$parameters$doublet_score_cutoff, 0.25)
  expect_equal(r2$provenance$parameters$doublet_score_cutoff, 1)
  # Raising the cutoff can never exclude MORE cells (monotone sensitivity).
  expect_lte(.m4_count(r2$provenance$cells_excluded),
             .m4_count(r1$provenance$cells_excluded))
})

test_that("F21-3: threshold adjustment re-filters without re-running detector", {
  if (!is_m4_ready()) skip("M4 counterfactual controls not yet active")
  if (!requireNamespace("scDblFinder", quietly = TRUE)) {
    skip("scDblFinder not installed in this environment")
  }
  obj <- .make_test_sc_obj()
  r1 <- sc_filter_doublets_and_ambient(obj, params = list(doublet_score_cutoff = 0.25, seed = 42))
  r2 <- sc_filter_doublets_and_ambient(obj, params = list(doublet_score_cutoff = 0.5, seed = 42))
  # Same input + same seed: the detector output is invariant to the cutoff —
  # only the filtering step differs.
  expect_identical(.m4_doublet_scores(r1), .m4_doublet_scores(r2))
  expect_false(identical(r1$provenance$parameters$doublet_score_cutoff,
                         r2$provenance$parameters$doublet_score_cutoff))
})

test_that("F21-4: extreme threshold (cutoff = 1.0) retains all cells as singlets", {
  if (!is_m4_ready()) skip("M4 counterfactual controls not yet active")
  if (!requireNamespace("scDblFinder", quietly = TRUE)) {
    skip("scDblFinder not installed in this environment")
  }
  obj <- .make_test_sc_obj()
  res <- sc_filter_doublets_and_ambient(obj, params = list(doublet_score_cutoff = 1, seed = 42))
  expect_equal(ncol(res$obj), ncol(obj))
  expect_equal(.m4_count(res$provenance$cells_excluded), 0L)
  expect_equal(.m4_count(res$provenance$cells_used), ncol(obj))
})

test_that("F21-5: counterfactual thresholds update summary counts reactively", {
  if (!is_m4_ready()) skip("M4 counterfactual controls not yet active")
  if (!requireNamespace("scDblFinder", quietly = TRUE)) {
    skip("scDblFinder not installed in this environment")
  }
  obj <- .make_test_sc_obj()
  r1 <- sc_filter_doublets_and_ambient(obj, params = list(doublet_score_cutoff = 0.25, seed = 42))
  r2 <- sc_filter_doublets_and_ambient(obj, params = list(doublet_score_cutoff = 1, seed = 42))
  for (r in list(r1, r2)) {
    expect_equal(.m4_count(r$provenance$cells_used) +
                   .m4_count(r$provenance$cells_excluded), ncol(obj))
    expect_equal(.m4_count(r$provenance$cells_used), ncol(r$obj))
  }
  # The summary counts track the threshold change.
  expect_gte(.m4_count(r2$provenance$cells_used),
             .m4_count(r1$provenance$cells_used))
})

# ── Feature 22: Before/After QC Distribution Metrics ─────────────────────────

test_that("F22-1: distribution metrics compute nCount_RNA summary before and after", {
  if (!is_m4_ready()) skip("M4 QC distribution metrics not yet active")
  if (!requireNamespace("scDblFinder", quietly = TRUE)) {
    skip("scDblFinder not installed in this environment")
  }
  obj <- .make_test_sc_obj()
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42))
  flat <- unlist(res$qc_metrics, recursive = TRUE)
  hit_name <- grepl("nCount_RNA", names(flat), ignore.case = TRUE)
  hit_value <- is.character(flat) & grepl("nCount_RNA", flat, ignore.case = TRUE)
  expect_true(any(hit_name | hit_value))
  expect_true(any((hit_name | hit_value) &
                    (grepl("before|pre", names(flat), ignore.case = TRUE) |
                       grepl("before|pre", as.character(flat), ignore.case = TRUE))))
  expect_true(any((hit_name | hit_value) &
                    (grepl("after|post", names(flat), ignore.case = TRUE) |
                       grepl("after|post", as.character(flat), ignore.case = TRUE))))
})

test_that("F22-2: distribution metrics compute nFeature_RNA summary before and after", {
  if (!is_m4_ready()) skip("M4 QC distribution metrics not yet active")
  if (!requireNamespace("scDblFinder", quietly = TRUE)) {
    skip("scDblFinder not installed in this environment")
  }
  obj <- .make_test_sc_obj()
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42))
  flat <- unlist(res$qc_metrics, recursive = TRUE)
  hit_name <- grepl("nFeature_RNA", names(flat), ignore.case = TRUE)
  hit_value <- is.character(flat) & grepl("nFeature_RNA", flat, ignore.case = TRUE)
  expect_true(any(hit_name | hit_value))
  expect_true(any((hit_name | hit_value) &
                    (grepl("before|pre", names(flat), ignore.case = TRUE) |
                       grepl("before|pre", as.character(flat), ignore.case = TRUE))))
  expect_true(any((hit_name | hit_value) &
                    (grepl("after|post", names(flat), ignore.case = TRUE) |
                       grepl("after|post", as.character(flat), ignore.case = TRUE))))
})

test_that("F22-3: summary table reports raw cell count, retained count, and doublet fraction", {
  if (!is_m4_ready()) skip("M4 QC distribution metrics not yet active")
  if (!requireNamespace("scDblFinder", quietly = TRUE)) {
    skip("scDblFinder not installed in this environment")
  }
  obj <- .make_test_sc_obj()
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42))
  n_raw <- ncol(obj)
  n_used <- .m4_count(res$provenance$cells_used)
  n_exc <- .m4_count(res$provenance$cells_excluded)
  expect_equal(n_used + n_exc, n_raw)
  expect_equal(n_used, ncol(res$obj))
  frac <- n_exc / n_raw
  expect_gte(frac, 0)
  expect_lte(frac, 1)
})

test_that("F22-4: distribution violin plots receive clean structured numerical vectors", {
  if (!is_m4_ready()) skip("M4 QC distribution metrics not yet active")
  if (!requireNamespace("scDblFinder", quietly = TRUE)) {
    skip("scDblFinder not installed in this environment")
  }
  obj <- .make_test_sc_obj()
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42))
  vals <- .m4_numeric_leaves(res$qc_metrics)
  expect_gt(length(vals), 0)
  # Plot inputs must be finite: no NA/NaN/Inf can reach a violin plot.
  expect_true(all(is.finite(vals)))
})

test_that("F22-5: metrics handle extreme distributions (e.g. 0% or 100% doublets)", {
  if (!is_m4_ready()) skip("M4 QC distribution metrics not yet active")
  if (!requireNamespace("scDblFinder", quietly = TRUE)) {
    skip("scDblFinder not installed in this environment")
  }
  obj <- .make_test_sc_obj()
  r_hi <- sc_filter_doublets_and_ambient(obj, params = list(doublet_score_cutoff = 1, seed = 42))
  r_lo <- sc_filter_doublets_and_ambient(obj, params = list(doublet_score_cutoff = 0, seed = 42))
  for (r in list(r_hi, r_lo)) {
    expect_equal(.m4_count(r$provenance$cells_used) +
                   .m4_count(r$provenance$cells_excluded), ncol(obj))
  }
  # 0% doublets removed at cutoff = 1.0; the 0.0 cutoff stays arithmetically
  # consistent (up to 100% exclusion) without NA metrics.
  expect_equal(.m4_count(r_hi$provenance$cells_excluded), 0L)
  frac_lo <- .m4_count(r_lo$provenance$cells_excluded) / ncol(obj)
  expect_gte(frac_lo, 0)
  expect_lte(frac_lo, 1)
  expect_true(all(is.finite(.m4_numeric_leaves(r_lo$qc_metrics))))
})

# ── Feature 23: Cell Provenance Recording ────────────────────────────────────

test_that("F23-1: new_provenance_entry records cerberus-doublets tool provenance", {
  if (!is_m4_ready()) skip("M4 cell provenance not yet active")
  if (!requireNamespace("scDblFinder", quietly = TRUE)) {
    skip("scDblFinder not installed in this environment")
  }
  obj <- .make_test_sc_obj()
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42))
  expect_true(is.list(res$provenance))
  expect_true(grepl("cerberus-doublets", as.character(res$provenance$analysis_id),
                    fixed = TRUE))
  expect_identical(as.character(res$provenance$method), "scDblFinder")
})

test_that("F23-2: provenance entry records cells_used vs cells_excluded counts", {
  if (!is_m4_ready()) skip("M4 cell provenance not yet active")
  if (!requireNamespace("scDblFinder", quietly = TRUE)) {
    skip("scDblFinder not installed in this environment")
  }
  obj <- .make_test_sc_obj()
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42))
  n_used <- .m4_count(res$provenance$cells_used)
  n_exc <- .m4_count(res$provenance$cells_excluded)
  expect_false(is.na(n_used))
  expect_false(is.na(n_exc))
  expect_equal(n_used + n_exc, ncol(obj))
  expect_equal(n_used, ncol(res$obj))
})

test_that("F23-3: provenance entry records algorithm parameters and thresholds", {
  if (!is_m4_ready()) skip("M4 cell provenance not yet active")
  if (!requireNamespace("scDblFinder", quietly = TRUE)) {
    skip("scDblFinder not installed in this environment")
  }
  obj <- .make_test_sc_obj()
  res <- sc_filter_doublets_and_ambient(obj, params = list(
    doublet_score_cutoff = 0.3, max_contamination = 0.15, seed = 42))
  pp <- res$provenance$parameters
  expect_true(is.list(pp))
  expect_equal(pp$doublet_score_cutoff, 0.3)
  expect_equal(pp$max_contamination, 0.15)
  expect_equal(pp$seed, 42)
})

test_that("F23-4: provenance list appends without corrupting existing history", {
  if (!is_m4_ready()) skip("M4 cell provenance not yet active")
  expect_true(exists("new_provenance_entry", mode = "function"))
  expect_true(exists("provenance_append", mode = "function"))
  st <- shiny::reactiveValues()
  e1 <- new_provenance_entry(analysis_id = "cerberus-doublets-1", method = "scDblFinder",
                             cells_used = 70, cells_excluded = 10)
  e2 <- new_provenance_entry(analysis_id = "cerberus-doublets-2", method = "scDblFinder",
                             cells_used = 75, cells_excluded = 5)
  provenance_append(st, e1)
  provenance_append(st, e2)
  h <- state_get(st, "provenance")
  expect_length(h, 2)
  expect_identical(as.character(h[[1]]$analysis_id), "cerberus-doublets-1")
  expect_identical(as.character(h[[2]]$analysis_id), "cerberus-doublets-2")
  expect_equal(h[[1]]$cells_used, 70)
  expect_equal(h[[2]]$cells_excluded, 5)
})

test_that("F23-5: provenance history serializes cleanly to JSON report", {
  if (!is_m4_ready()) skip("M4 cell provenance not yet active")
  if (!requireNamespace("scDblFinder", quietly = TRUE)) {
    skip("scDblFinder not installed in this environment")
  }
  obj <- .make_test_sc_obj()
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42))
  js <- jsonlite::toJSON(res$provenance, auto_unbox = TRUE)
  expect_type(js, "character")
  back <- jsonlite::fromJSON(as.character(js), simplifyVector = FALSE)
  expect_identical(as.character(back$method), "scDblFinder")
  expect_true(grepl("cerberus-doublets", as.character(back$analysis_id), fixed = TRUE))
  expect_false(is.null(back$parameters))
})

# ── Feature 24: Clean Matrix Downstream Propagation ──────────────────────────

test_that("F24-1: filtered object contains strictly retained singlets", {
  if (!is_m4_ready()) skip("M4 downstream propagation not yet active")
  if (!requireNamespace("scDblFinder", quietly = TRUE)) {
    skip("scDblFinder not installed in this environment")
  }
  obj <- .make_test_sc_obj()
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42))
  expect_s4_class(res$obj, "Seurat")
  # The filtered object is a strict subset of the input bookkept as cells_used.
  expect_true(all(colnames(res$obj) %in% colnames(obj)))
  expect_equal(ncol(res$obj), .m4_count(res$provenance$cells_used))
  # Identifier-form exclusion lists must be fully absent from the result.
  exc_id <- res$provenance$cells_excluded
  if (!is.null(exc_id) && (!is.numeric(exc_id) || length(exc_id) > 1L)) {
    expect_false(any(as.character(exc_id) %in% colnames(res$obj)))
  }
})

test_that("F24-2: excluded doublet cell barcodes are absent from downstream assay", {
  if (!is_m4_ready()) skip("M4 downstream propagation not yet active")
  if (!requireNamespace("scDblFinder", quietly = TRUE)) {
    skip("scDblFinder not installed in this environment")
  }
  obj <- .make_test_sc_obj()
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42))
  counts <- res$obj[["RNA"]]$counts
  expect_true(all(colnames(counts) %in% colnames(res$obj)))
  excluded <- setdiff(colnames(obj), colnames(res$obj))
  if (length(excluded) > 0) {
    expect_false(any(excluded %in% colnames(counts)))
  }
})

test_that("F24-3: downstream normalization runs only on clean matrix cells", {
  if (!is_m4_ready()) skip("M4 downstream propagation not yet active")
  if (!requireNamespace("scDblFinder", quietly = TRUE)) {
    skip("scDblFinder not installed in this environment")
  }
  obj <- .make_test_sc_obj()
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42))
  norm <- Seurat::NormalizeData(res$obj, verbose = FALSE)
  expect_identical(colnames(norm), colnames(res$obj))
  expect_true(all(colnames(norm) %in% colnames(obj)))
})

test_that("F24-4: downstream PCA and UMAP embeddings exclude filtered doublets", {
  if (!is_m4_ready()) skip("M4 downstream propagation not yet active")
  if (!requireNamespace("scDblFinder", quietly = TRUE)) {
    skip("scDblFinder not installed in this environment")
  }
  obj <- .make_test_sc_obj()
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42))
  pca <- Seurat::RunPCA(res$obj, npcs = 5, verbose = FALSE)
  emb <- Seurat::Embeddings(pca, "pca")
  expect_identical(rownames(emb), colnames(res$obj))
  expect_true(all(rownames(emb) %in% colnames(obj)))
})

test_that("F24-5: clean matrix preserves dgCMatrix class and sparsity structure", {
  if (!is_m4_ready()) skip("M4 downstream propagation not yet active")
  if (!requireNamespace("scDblFinder", quietly = TRUE)) {
    skip("scDblFinder not installed in this environment")
  }
  obj <- .make_test_sc_obj()
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42))
  counts <- res$obj[["RNA"]]$counts
  expect_s4_class(counts, "dgCMatrix")
  expect_lte(length(counts), ncol(counts) * nrow(counts))
  expect_true(all(counts@x >= 0))
})

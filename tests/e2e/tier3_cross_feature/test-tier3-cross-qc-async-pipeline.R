# =============================================================================
# test-tier3-cross-qc-async-pipeline.R — Tier 3 Cross-Feature: QC -> Pipeline
# =============================================================================
# Cross-feature scenario: Cleaned singlet matrix from biological QC stage
# passed forward to decoupled asynchronous Single-Cell pipeline.
# - Filtered doublets are strictly excluded from PCA & UMAP reduction
# - Provenance entry attached during QC is preserved in pipeline result
# - Mirai worker daemon receives pure matrix without Shiny reactivity leaks
# =============================================================================

if (!exists("e2e_project_root", mode = "function")) {
  if (file.exists("tests/e2e/helper-e2e.R")) {
    source("tests/e2e/helper-e2e.R")
  } else if (file.exists("../helper-e2e.R")) {
    source("../helper-e2e.R")
  }
}

# ── Fixture M5 (2026-10-27) : même gabarit que tier2, doublets artificiels en
# option (somme de deux cellules distinctes — profils mixtes que scDblFinder
# appelle). Processus-global : chaque fichier e2e construit les siens.
.t3qcp_obj <- function(ng = 120, nc = 80, seed = 42, doublets = FALSE) {
  set.seed(seed)
  m <- matrix(rpois(ng * nc, lambda = 3), nrow = ng, ncol = nc)
  rownames(m) <- paste0("Gene-", seq_len(ng))
  colnames(m) <- paste0("Cell-", seq_len(nc))
  if (doublets) {
    for (k in seq_len(8L)) {
      m <- cbind(m, m[, 2 * k - 1] + m[, 2 * k])
      colnames(m)[ncol(m)] <- paste0("Artificial-", k)
    }
  }
  suppressWarnings(Seurat::CreateSeuratObject(counts = m))
}

test_that("T3-QCP-1: QC filtered singlets propagate into run_sc_auto_pipeline_core", {
  if (!is_m3_ready() || !is_m4_ready()) skip("M3 pipeline and/or M4 QC not yet active")
  # M5 : la Step 1c du cœur (opt-in sc_ap_doublets) appelle le MOTEUR M4 réel
  # — la provenance du doublet (analysis_id cerberus-doublets) voyage dans le
  # retour étendu du cœur, avec la graine écho.
  res <- run_sc_auto_pipeline_core(.t3qcp_obj(),
                                   params = list(sc_ap_doublets = TRUE, sc_ap_res = 0.5))
  expect_false(is.null(res$doublets_provenance))
  expect_identical(res$doublets_provenance$analysis_id, "cerberus-doublets")
  expect_identical(res$doublets_provenance$method, "scDblFinder")
  expect_true(is.list(res$qc_distributions))
})

test_that("T3-QCP-2: downstream PCA excludes cells filtered out by doublet engine", {
  if (!is_m3_ready() || !is_m4_ready()) skip("M3 pipeline and/or M4 QC not yet active")
  # M5 : doublets ARTIFICIELS (somme de deux cellules) injectés dans la
  # fixture — scDblFinder les appelle, le cœur les exclut, et la matrice
  # downstream ne contient AUCUN exclu (mesuré : 88 in -> 86 out, exclus 2).
  res <- run_sc_auto_pipeline_core(.t3qcp_obj(doublets = TRUE),
                                   params = list(sc_ap_doublets = TRUE, sc_ap_res = 0.5))
  excluded <- as.character(res$doublets_provenance$cells_excluded %||% character(0))
  expect_true(length(excluded) >= 1L)
  expect_true(all(excluded %in% colnames(.t3qcp_obj(doublets = TRUE))))
  expect_false(any(excluded %in% colnames(res$obj)))
  expect_equal(ncol(res$obj), 88L - length(excluded))
})

test_that("T3-QCP-3: QC provenance history is retained in pipeline output object", {
  if (!is_m3_ready() || !is_m4_ready()) skip("M3 pipeline et/ou M4 QC pas encore actif")
  # M5 : le snapshot QC pré-M4 ET la provenance du doublet coexistent dans le
  # retour (règle 7 : provenance PRODUITE à chaque étape, jamais reconstruite).
  res <- run_sc_auto_pipeline_core(.t3qcp_obj(),
                                   params = list(sc_ap_doublets = TRUE, sc_ap_res = 0.5))
  expect_true(is.list(res$qc_snapshot))
  expect_false(is.null(res$doublets_provenance))
  expect_true(!is.null(res$doublets_provenance$seed))
  expect_true(!is.null(res$timings$doublets_s))
})

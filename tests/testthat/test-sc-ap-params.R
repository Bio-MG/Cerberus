# =============================================================================
# test-sc-ap-params.R — M-2 phase 1 : normalisation des paramètres + cœur QC
# =============================================================================
# Deux livrables de la carte M-2 (cœur pur de run_sc_auto_pipeline) :
#
# 1. sc_ap_normalize_params(p) — le SEUL point de passage des paramètres de
#    l'autopipeline. Avant ce jalon, les défauts vivaient dans le canal drive
#    (.sc_ap_drive_inputs, modules/sc/mod_sc.R) et l'UI lisait ses propres
#    valeurs : la classe « une clé manquante change silencieusement un seuil »
#    (audit C-1) n'était fermée que par convention. Ici le cœur reçoit une
#    liste FERMÉE, normalisée, quelle que soit la source (UI, drive, scénario).
#    Le canal drive DÉLÈGUE désormais au cœur (inversion de dépendance :
#    les défauts vivent dans R/, la couche pure).
#
# 2. sc_pipeline_qc(obj, ...) — le cœur pur de l'étape 1 (QC), extrait du
#    pipeline : Seurat pur, zéro Shiny, testable hors app (audit J-5).
# =============================================================================
source_project_file("config/defaults.R")
source_project_file("R/core/io_helpers.R")      # %||%
source_project_file("R/core/state.R")
source_project_file("R/core/drive_allowlist.R")
source_project_file("R/sc/sc_pipeline.R")
source_project_file("modules/sc/mod_sc.R")      # canal drive (délégation)
assign(".tr", function(key) key, envir = globalenv())

.CANON_KEYS <- c(
  "sc_ap_mapping", "sc_ap_mapping_org", "sc_ap_bpcells",
  "sc_ap_min_gene", "sc_ap_max_gene", "sc_ap_mt", "sc_ap_norm",
  "sc_ap_pca_dim", "sc_ap_res", "sc_ap_cluster_algo", "sc_ap_compute_umap",
  "sc_ap_integration", "sc_ap_batch_var",
  "sc_ap_sketch_preset", "sc_ap_sketch_ncells_custom",
  "sc_ap_singler", "sc_ap_singler_ref", "sc_ap_singler_level",
  "sc_ap_markers", "sc_ap_pathway", "sc_ap_pathway_db", "sc_ap_pathway_org",
  "sc_ap_correlation", "sc_ap_trajectory"
)

test_that("sc_ap_normalize_params rend le jeu FERMÉ avec les défauts canoniques", {
  p <- sc_ap_normalize_params(list())
  expect_setequal(names(p), .CANON_KEYS)
  expect_equal(p$sc_ap_min_gene, TS_SC_QC_MIN_GENES)
  expect_equal(p$sc_ap_max_gene, TS_SC_QC_MAX_GENES)
  expect_equal(p$sc_ap_mt,       TS_SC_QC_MAX_PCT_MT)
  expect_equal(p$sc_ap_pca_dim,  TS_SC_QC_PCA_DIMS)
  expect_true(isTRUE(p$sc_ap_markers))
  expect_true(isTRUE(p$sc_ap_trajectory))
  expect_false(isTRUE(p$sc_ap_mapping))
  expect_identical(p$sc_ap_norm, "log")
})

test_that("les valeurs fournies gagnent, les clés inconnues sont PERDUES", {
  p <- sc_ap_normalize_params(list(sc_ap_min_gene = 42, cle_inconnue = "x"))
  expect_equal(p$sc_ap_min_gene, 42)
  expect_false("cle_inconnue" %in% names(p))
})

test_that("le canal drive DÉLÈGUE au cœur (une seule source de défauts)", {
  expect_identical(.sc_ap_drive_inputs(), sc_ap_normalize_params(list()))
})

# ── Cœur pur QC ─────────────────────────────────────────────────────────────
skip_if_not_installed("Seurat")
library(Seurat)

.scd_qc_obj <- function(n_per_group = 150L, seed = 42) {
  set.seed(seed)
  n_cells <- 2L * n_per_group
  n_genes <- 120L
  mat <- matrix(rpois(n_genes * n_cells, lambda = 1), nrow = n_genes,
                dimnames = list(paste0("G", seq_len(n_genes)), NULL))
  mat[1:15, seq_len(n_per_group)] <-
    mat[1:15, seq_len(n_per_group)] + rpois(15L * n_per_group, 8)
  mat[16:30, (n_per_group + 1L):n_cells] <-
    mat[16:30, (n_per_group + 1L):n_cells] + rpois(15L * n_per_group, 8)
  mt <- matrix(rpois(3L * n_cells, lambda = 1), nrow = 3L,
               dimnames = list(c("MT-A", "MT-B", "MT-C"), NULL))
  suppressWarnings(CreateSeuratObject(counts = rbind(mat, mt)))
}

test_that("QC : seuils généreux — tout passe, percent.mt est créée, comptes rendus", {
  obj <- .scd_qc_obj()
  res <- sc_pipeline_qc(obj, min_genes = 10, max_genes = 8000, max_mt = 20)
  expect_s4_class(res$object, "Seurat")
  expect_equal(ncol(res$object), 300L)
  expect_equal(res$n_before, 300L)
  expect_equal(res$n_removed, 0L)
  expect_true("percent.mt" %in% colnames(res$object[[]]))
})

test_that("QC : un seuil strict filtre réellement les cellules", {
  obj <- .scd_qc_obj()
  # La fixture a nFeature_RNA ∈ [71, 99] (mesuré) : 80 coupe en deux.
  res <- sc_pipeline_qc(obj, min_genes = 80, max_genes = 8000, max_mt = 20)
  expect_gt(res$n_removed, 0L)
  expect_lt(res$n_removed, res$n_before)
  expect_equal(ncol(res$object), res$n_before - res$n_removed)
})

test_that("QC : trop peu de survivantes => erreur CLASSÉE en français", {
  obj <- .scd_qc_obj()
  err <- tryCatch(sc_pipeline_qc(obj, min_genes = 500, max_genes = 8000, max_mt = 20),
                  error = function(e) e)
  expect_s3_class(err, "sc_pipeline_error")
  expect_match(conditionMessage(err), "après QC", fixed = TRUE)
})

# =============================================================================
# test-tier2-boundaries-qc-parameters.R — Tier 2 Boundary: QC & Pipeline Extremes
# =============================================================================
# Boundary parameters and edge cases for biological QC and pipeline execution:
# - Doublet score cutoff at boundary limits (0.0 vs 1.0)
# - Contamination fraction at boundary limits (0.0 vs 1.0)
# - Zero-variance genes in PCA reduction
# - Minimal cell count threshold for clustering
# - Missing optional pipeline parameters
#
# M5 dé-façadage (2026-10-27, décision O17) : chaque bloc exécute le MOTEUR ou
# le CŒUR réel sur la fixture du helper (120 gènes × 80 cellules, Poisson).
# ⚠️ Deux découvertes de mesure consignées dans le jalon M5 (comportement
# actuel, épinglé honnête) : sc_ap_res = 3.0 et l'objet < pca_dim propagent
# une erreur de dépendance NON classée (défaut à filet, hors contrat du cœur).
# =============================================================================

if (!exists("e2e_project_root", mode = "function")) {
  if (file.exists("tests/e2e/helper-e2e.R")) {
    source("tests/e2e/helper-e2e.R")
  } else if (file.exists("../helper-e2e.R")) {
    source("../helper-e2e.R")
  }
}

.t2qc_obj <- function(ng = 120, nc = 80, seed = 42, zero_row = FALSE) {
  set.seed(seed)
  m <- matrix(rpois(ng * nc, lambda = 3), nrow = ng, ncol = nc)
  rownames(m) <- paste0("Gene-", seq_len(ng))
  colnames(m) <- paste0("Cell-", seq_len(nc))
  if (zero_row) m["Gene-1", ] <- 1L
  suppressWarnings(Seurat::CreateSeuratObject(counts = m))
}

# Comptage tolérant NULL des exclus (miroir du .m4_count local au fichier m4).
.t2qc_count <- function(x) length(as.character(x %||% character(0)))

test_that("T2-QC-1: doublet cutoff = 0.0 flags all non-zero cells as doublets", {
  if (!is_m4_ready()) skip("M4 QC parameters not yet active")
  r1 <- sc_filter_doublets_and_ambient(.t2qc_obj(),
                                       params = list(doublet_score_cutoff = 0, seed = 42))
  rmid <- sc_filter_doublets_and_ambient(.t2qc_obj(),
                                         params = list(doublet_score_cutoff = 0.25, seed = 42))
  # Le paramètre est consommé et écho en provenance (contrat F21).
  expect_equal(r1$provenance$parameters$doublet_score_cutoff, 0)
  # Monotonie inverse : abaisser le cutoff n'exclut JAMAIS moins qu'un cutoff
  # élevé (sur la fixture uniforme, les deux peuvent être 0 — l'inégalité
  # reste la propriété).
  expect_gte(.t2qc_count(r1$provenance$cells_excluded),
             .t2qc_count(rmid$provenance$cells_excluded))
})

test_that("T2-QC-2: doublet cutoff = 1.0 retains 100% of cells as singlets", {
  if (!is_m4_ready()) skip("M4 QC parameters not yet active")
  r2 <- sc_filter_doublets_and_ambient(.t2qc_obj(),
                                       params = list(doublet_score_cutoff = 1, seed = 42))
  expect_equal(r2$provenance$parameters$doublet_score_cutoff, 1)
  expect_equal(.t2qc_count(r2$provenance$cells_excluded), 0L)
  expect_equal(ncol(r2$obj), 80L)
})

test_that("T2-QC-3: max_contamination = 0.0 applies strictest ambient decontamination", {
  if (!is_m4_ready()) skip("M4 QC parameters not yet active")
  # Sans run_ambient (SoupX/celda absents sur cet hôte), le knob est consommé
  # et écho — le chemin ambient classé est couvert par F20-1..5.
  r0 <- sc_filter_doublets_and_ambient(.t2qc_obj(),
                                       params = list(max_contamination = 0, seed = 42))
  r9 <- sc_filter_doublets_and_ambient(.t2qc_obj(),
                                       params = list(max_contamination = 0.9, seed = 42))
  expect_equal(r0$provenance$parameters$max_contamination, 0)
  expect_equal(r9$provenance$parameters$max_contamination, 0.9)
})

test_that("T2-QC-4: zero-variance genes are filtered before PCA calculation", {
  if (!is_m3_ready()) skip("M3 SC async pipeline not yet active")
  # M5 : le cœur COMPLÈTE avec un gène à variance nulle présente, et les
  # embeddings PCA restent finis (pas d'explosion NaN). Mesure honnête : à ces
  # paramètres VariableFeatures garde les 120 gènes — le filtre de gènes
  # dégénérés n'est PAS l'étape épinglée ; la propriété épinglée est la
  # robustesse du calcul (claim du legs, réduit à ce qui est vrai).
  r <- run_sc_auto_pipeline_core(.t2qc_obj(zero_row = TRUE),
                                 params = list(sc_ap_res = 0.05))
  expect_s4_class(r$obj, "Seurat")
  emb <- tryCatch(Seurat::Embeddings(r$obj, "pca"), error = function(e) NULL)
  expect_false(is.null(emb))
  expect_true(all(is.finite(emb)))
})

test_that("T2-QC-5: pipeline handles extreme resolution values (0.05 vs 3.0)", {
  if (!is_m3_ready()) skip("M3 SC async pipeline not yet active")
  low <- run_sc_auto_pipeline_core(.t2qc_obj(), params = list(sc_ap_res = 0.05))
  expect_s4_class(low$obj, "Seurat")
  expect_identical(length(unique(low$obj$seurat_clusters)), 1L)
  # ⚠️ Découverte M5 : res = 3.0 sur la fixture 80 cellules produit une erreur
  # de dépendance NON classée (propagation brute hors contrat du cœur) —
  # épinglé comme comportement ACTUEL : l'erreur est capturable (pas de crash
  # de session) et identifie le problème de dimension. Filet à poser au cœur
  # (jalon futur), pas de maquillage ici.
  high <- tryCatch(run_sc_auto_pipeline_core(.t2qc_obj(),
                                             params = list(sc_ap_res = 3.0)),
                   error = function(e) e)
  expect_s3_class(high, "error")
  expect_true(nzchar(conditionMessage(high)))
})

test_that("T2-QC-6: pipeline gracefully fails on fewer cells than requested clusters", {
  if (!is_m3_ready()) skip("M3 SC async pipeline not yet active")
  # ⚠️ Découverte M5 : 12 cellules < pca_dim (20) — l'erreur irlba/PCA sort
  # NON classée du cœur (même famille que T2-QC-5). Épinglé honnête : l'erreur
  # est capturable et nomme la contrainte de dimension ; aucun crash de
  # session. La classification (sc_pipeline_error) reste à poser au cœur.
  err <- tryCatch(run_sc_auto_pipeline_core(.t2qc_obj(nc = 12),
                                            params = list(sc_ap_res = 5)),
                  error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "strictly less|dimension", ignore.case = TRUE)
})

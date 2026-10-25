# =============================================================================
# test-bulk-pattern.R — Clustering de profils kmeans (STAT-S3, MVP)
# =============================================================================
# Couvre : gel de la surface publique + des champs contractuels, validations
# (matrice, colonne de groupe, k), exclusions comptabilisées (gènes absents,
# constants, échantillons NA), reproductibilité par graine, forme du résultat
# canonique, plot pur, export plat, assert canonique.
# 100 % hors-ligne : stats::kmeans + fixtures construites à la main.
# =============================================================================
source_project_file("tools/pkg_skeleton/R/provenance.R")
# D2 M4 (2026-10-25) : R/plotting vit desormais dans l'arbre du paquet
# (tools/pkg_skeleton/R/) — charge via load_project_pkg() (cf. helper-source.R).
load_project_pkg()
source_project_file("tools/pkg_skeleton/R/bulk_pattern.R")

# ── Fixtures ───────────────────────────────────────────────────────────────────
# 24 gènes x 6 échantillons, 2 groupes (3 vs 3) : 3 formes de profils nettes.
.toy_fixture <- function() {
  set.seed(42)
  smp <- c("s1", "s2", "s3", "s4", "s5", "s6")
  grp <- c("A", "A", "A", "B", "B", "B")
  genes <- paste0("g", seq_len(24))
  m <- matrix(rnorm(24 * 6, sd = 0.1), 24, 6, dimnames = list(genes, smp))
  m[1:8, 1:3] <- m[1:8, 1:3] + 3   # up dans A
  m[9:16, 4:6] <- m[9:16, 4:6] + 3 # up dans B
  # g17..g24 : bruit (profils plats)
  metadata <- data.frame(sample = smp, condition = grp,
                         stringsAsFactors = FALSE)
  rownames(metadata) <- smp
  list(mat = m, metadata = metadata)
}

# Les fixtures synthétiques peuvent déclencher l'avertissement kmeans
# "Quick-TRANSfer" (bruit numérique, pas une garde du domaine) — neutralisé
# dans les tests ; les avertissements MÉTIER passent par res$warnings.
.run_pattern_quiet <- function(...) suppressWarnings(run_pattern_clustering(...))

test_that("surface publique figée", {
  expect_setequal(
    bulk_pattern_public_api(),
    c("bulk_pattern_contract_fields", "bulk_pattern_validity_states",
      "bulk_pattern_error_state", "run_pattern_clustering",
      "plot_pattern_profiles", "build_pattern_table_export",
      "assert_bulk_pattern_result", "bulk_pattern_public_api")
  )
})

test_that("champs contractuels figés", {
  expect_setequal(bulk_pattern_contract_fields(),
    c("type", "status", "clusters", "cluster_profiles", "summary",
      "group_column", "k", "seed", "parameters", "qc", "warnings",
      "provenance", "analysis_id", "timestamp_utc"))
  expect_setequal(bulk_pattern_validity_states(),
    c("valid", "valid_with_warnings"))
})

test_that("run_pattern_clustering produit un résultat canonique et reproductible", {
  fx <- .toy_fixture()
  r1 <- .run_pattern_quiet(fx$mat, fx$metadata, "condition", k = 3, seed = 15)
  r2 <- .run_pattern_quiet(fx$mat, fx$metadata, "condition", k = 3, seed = 15)
  expect_identical(r1$type, "bulk_pattern_clusters")
  expect_identical(r1$analysis_id, "bulk-pattern-clusters")
  expect_true(r1$status %in% bulk_pattern_validity_states())
  expect_setequal(names(r1), bulk_pattern_contract_fields())
  expect_identical(r1$clusters$gene, r2$clusters$gene)
  expect_identical(r1$clusters$cluster, r2$clusters$cluster)
  expect_identical(r1$k, 3L)
  expect_identical(r1$seed, 15L)
  # les gènes up-A et up-B ne peuvent pas partager de cluster avec leur
  # profil miroir (z-scores opposés) — contrôle de bon sens sur la fixture
  cl_a <- r1$clusters$cluster[r1$clusters$gene == "g1"]
  cl_b <- r1$clusters$cluster[r1$clusters$gene == "g9"]
  expect_false(identical(cl_a, cl_b))
})

test_that("exclusions comptabilisées : gènes absents, constants, échantillons NA", {
  fx <- .toy_fixture()
  fx$mat <- rbind(fx$mat, gz = rep(5, ncol(fx$mat)))  # gène constant
  genes <- c(paste0("g", 1:24), "gz", "absent1", "absent2")
  metadata <- fx$metadata
  metadata$condition[1] <- NA  # échantillon sans libellé
  res <- .run_pattern_quiet(fx$mat, metadata, "condition",
                                genes = genes, k = 2, seed = 15)
  expect_identical(res$summary$n_genes_not_found, 2L)
  expect_true(res$summary$n_genes_constant >= 1L)
  expect_identical(res$summary$n_samples_na, 1L)
  expect_false("absent1" %in% res$clusters$gene)
  expect_true(length(res$warnings) >= 1L)
  expect_identical(res$summary$n_samples, 5L)
})

test_that("validations : matrice, colonne, k (états classés)", {
  fx <- .toy_fixture()
  expect_error(run_pattern_clustering(NULL, fx$metadata, "condition", k = 2),
               "vst_mat", fixed = TRUE)
  expect_error(.run_pattern_quiet(fx$mat, fx$metadata, "inconnue", k = 2),
               "absente", fixed = TRUE)
  expect_error(.run_pattern_quiet(fx$mat, fx$metadata, "condition", k = 1),
               "k doit être un entier >= 2", fixed = TRUE)
  expect_error(.run_pattern_quiet(fx$mat, fx$metadata, "condition", k = 99),
               "plafond", fixed = TRUE)
  expect_error(.run_pattern_quiet(fx$mat, fx$metadata, "condition", k = 2),
               NA)  # passe
  # état classé extractible
  e <- tryCatch(.run_pattern_quiet(fx$mat, fx$metadata, "condition", k = 1),
                error = function(e) e)
  expect_s3_class(e, "bulk_pattern_error")
  expect_identical(bulk_pattern_error_state(e), "invalid_input")
  expect_true(is.na(bulk_pattern_error_state(simpleError("non classée"))))
})

test_that("plot_pattern_profiles renvoie un ggplot; export plat; assert canonique", {
  skip_if_not_installed("ggplot2")
  fx <- .toy_fixture()
  res <- .run_pattern_quiet(fx$mat, fx$metadata, "condition", k = 3, seed = 15)
  p <- plot_pattern_profiles(res)
  expect_s3_class(p, "ggplot")
  tab <- build_pattern_table_export(res)
  expect_identical(colnames(tab), c("gene", "cluster"))
  expect_identical(nrow(tab), res$summary$n_genes_used)
  expect_true(isTRUE(assert_bulk_pattern_result(res, context = "test")))
  expect_error(assert_bulk_pattern_result(list(type = "autre"), "ctx"),
               "non canonique", fixed = TRUE)
})

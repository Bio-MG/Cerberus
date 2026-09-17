# =============================================================================
# test-helpers_sc.R — pure-function tests for helpers_sc.R (QA-2, Cerberus 1.0)
# =============================================================================
# Scope: resolve_sketch_preset() (pure, zero extra deps), calculate_pseudotime()
# (needs RANN + igraph, skip-guarded), subsample_seurat_for_analysis() (needs
# Seurat/SeuratObject, skip-guarded, tiny synthetic object -- no real counts
# data or Bioconductor annotation package required).
#
# OUT OF SCOPE: robust_find_clusters()'s fallback path only fires once
# Seurat::FindClusters() fails on a REAL neighbor graph (FindNeighbors()
# output) -- not worth a fragile full-pipeline fixture for a unit test. Plot
# builders (plot_trajectory(), plot_genes_vs_pseudotime(), ...), Slingshot,
# and the RNA Velocity validators are likewise out of scope here -- see RF-1
# in the Cerberus 1.0 sprint plan (extract Velocity to its own
# helpers_sc_velocity.R + a dedicated test file).
# =============================================================================

source_project_file("R/core/io_helpers.R")   # defines %||%, sourced first in app.R
source_project_file("R/plotting/palettes.R")   # required by sc_trajectory (scale helpers)
source_project_file("R/plotting/datatable.R")   # PLOT-S3 : build_markers_dt() -> ts_datatable()
source_project_file("R/sc/sc_helpers.R")
source_project_file("R/sc/sc_trajectory.R")   # calculate_pseudotime moved here (Block 7 refactor)

# ---------------------------------------------------------------------------
# resolve_sketch_preset()
# ---------------------------------------------------------------------------
test_that("resolve_sketch_preset caps ncells at n_total_cells for every named preset", {
  for (p in c("fast", "light", "medium", "standard", "high")) {
    out <- resolve_sketch_preset(p, n_total_cells = 1000)
    expect_lte(out$ncells, 1000)
    expect_true(out$npcs > 0)
    expect_true(out$max_per_cluster > 0)
  }
})

test_that("resolve_sketch_preset 'max' uses the full dataset size", {
  out <- resolve_sketch_preset("max", n_total_cells = 12345)
  expect_equal(out$ncells, 12345)
})

test_that("resolve_sketch_preset 'custom' uses custom_ncells, capped at n_total_cells", {
  out <- resolve_sketch_preset("custom", n_total_cells = 5000, custom_ncells = 3000)
  expect_equal(out$ncells, 3000)
  out2 <- resolve_sketch_preset("custom", n_total_cells = 2000, custom_ncells = 3000)
  expect_equal(out2$ncells, 2000)   # capped even for a custom request above dataset size
})

test_that("resolve_sketch_preset 'custom' defaults to 20000 when custom_ncells is NULL", {
  out <- resolve_sketch_preset("custom", n_total_cells = 100000, custom_ncells = NULL)
  expect_equal(out$ncells, 20000)
})

test_that("resolve_sketch_preset falls back to 'standard' for an unknown preset name", {
  expect_equal(resolve_sketch_preset("not_a_real_preset", n_total_cells = 60000),
               resolve_sketch_preset("standard", n_total_cells = 60000))
})

# ---------------------------------------------------------------------------
# calculate_pseudotime() -- needs RANN + igraph
# ---------------------------------------------------------------------------
test_that("calculate_pseudotime orders cells monotonically along a 1D line", {
  skip_if_not_installed("RANN")
  skip_if_not_installed("igraph")
  set.seed(1)
  emb <- matrix(seq(0, 10, length.out = 50), ncol = 1)
  rownames(emb) <- paste0("cell", seq_len(50))
  res <- calculate_pseudotime(emb, k = 5, root_cells = 1, root_method = "manual")
  expect_length(res$pseudotime, 50)
  expect_equal(unname(res$pseudotime[1]), 0)   # root cell -> pseudotime 0 after min-max scaling
  expect_gt(stats::cor(seq_len(50), unname(res$pseudotime), method = "spearman"), 0.95)
})

test_that("calculate_pseudotime errors on too few cells", {
  skip_if_not_installed("RANN")
  skip_if_not_installed("igraph")
  emb <- matrix(1:4, ncol = 2)
  expect_error(calculate_pseudotime(emb, k = 2), "at least 3 cells")
})

test_that("calculate_pseudotime errors on non-finite embedding values", {
  skip_if_not_installed("RANN")
  skip_if_not_installed("igraph")
  emb <- matrix(c(1, 2, NA, 4, 5, 6), ncol = 2)
  expect_error(calculate_pseudotime(emb, k = 2), "NA, NaN, or infinite")
})

test_that("calculate_pseudotime errors on an out-of-range manual root cell", {
  skip_if_not_installed("RANN")
  skip_if_not_installed("igraph")
  emb <- matrix(seq(0, 10, length.out = 30), ncol = 1)
  expect_error(calculate_pseudotime(emb, k = 5, root_cells = 999), "Invalid root cell index")
})

# ---------------------------------------------------------------------------
# subsample_seurat_for_analysis() -- needs Seurat/SeuratObject
# ---------------------------------------------------------------------------
.load_seurat_pkgs <- function() {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("SeuratObject")
  suppressPackageStartupMessages({
    library(SeuratObject)
    library(Seurat)
  })
}

.toy_seurat <- function(n_cells = 60, n_groups = 3) {
  counts <- matrix(stats::rpois(n_cells * 20, lambda = 2), nrow = 20,
                   dimnames = list(paste0("gene", 1:20), paste0("cell", 1:n_cells)))
  obj <- SeuratObject::CreateSeuratObject(counts = counts)
  obj$seurat_clusters <- factor(rep(seq_len(n_groups), length.out = n_cells))
  obj
}

test_that("subsample_seurat_for_analysis caps cells per group and reports the split", {
  .load_seurat_pkgs()
  obj <- .toy_seurat(n_cells = 90, n_groups = 3)   # 30 cells/cluster
  res <- subsample_seurat_for_analysis(obj, max_per_group = 10, group_col = "seurat_clusters")
  expect_true(res$was_subsampled)
  expect_equal(res$n_before, 90)
  expect_equal(ncol(res$object), 30)   # 3 clusters x 10 cells/cluster cap
})

test_that("subsample_seurat_for_analysis is a no-op when the cap disables subsampling", {
  .load_seurat_pkgs()
  obj <- .toy_seurat(n_cells = 40, n_groups = 2)
  for (cap in list(Inf, NA, 0, -5)) {
    res <- subsample_seurat_for_analysis(obj, max_per_group = cap, group_col = "seurat_clusters")
    expect_false(res$was_subsampled)
    expect_equal(ncol(res$object), 40)
  }
})

test_that("subsample_seurat_for_analysis falls back to a flat subsample when group_col is absent", {
  .load_seurat_pkgs()
  obj <- .toy_seurat(n_cells = 50, n_groups = 2)
  res <- subsample_seurat_for_analysis(obj, max_per_group = 15, group_col = "not_a_real_column")
  expect_true(res$was_subsampled)
  expect_equal(ncol(res$object), 15)
})

# ---------------------------------------------------------------------------
# C10 — les erreurs de sc_helpers.R portent la classe `sc_helpers_error`
# ---------------------------------------------------------------------------
# Dette de conventions, 7e increment. Le fichier compte 35 `stop()` : 34 sont
# non classes, le 35e est le re-leve nu `stop(e)` (ligne ~1205), forme que C10
# EXEMPTE explicitement et qui est laissee telle quelle.
#
# Classe NOMMEE PAR LE FICHIER, pas par un domaine : ce fichier est un
# fourre-tout heterogene (recherche de genes, remapping d'IDs, heatmap, densite
# 2D, 3D) — aucun domaine d'analyse unique ne le decrit. `sc_error` serait trop
# large (il cohabite avec sc_multi_error / sccoda_error / milo_error) et
# pretendrait designer "l'erreur single-cell" generique.
#
# ATTENTION — un site a un message en PLUSIEURS arguments
# (`stop("Agregation par groupe impossible : ", conditionMessage(e2))`, ligne
# ~1362) : il est enveloppe dans `paste0()`. Il est INJOIGNABLE ici (double
# `tryCatch` exigeant plus de 5000 cellules ET deux echecs d'AverageExpression),
# donc il n'est PAS couvert a l'execution : il est couvert par le verrou source
# ci-dessous, par l'assertion du script de conversion (+1 `paste0(`) et par la
# regle C16 — trois gardes statiques independantes.
#
# Deux niveaux de preuve, aucun ne suffisant seul :
#   1. SOURCE    — le fichier ne contribue plus AUCUN signalement C10 (couvre
#      les 19 sites injoignables : gardes "paquet manquant", branches internes).
#   2. EXECUTION — les 15 sites joignables levent VRAIMENT une condition classee.
source_project_file("tools/check_conventions.R")

#' Capture l'erreur levee par `expr` ; NULL si `expr` retourne normalement.
.sc_error_of <- function(expr) {
  tryCatch({
    suppressMessages(suppressWarnings(force(expr)))
    NULL
  }, error = function(e) e)
}

test_that("C10 : sc_helpers.R ne contribue plus aucun signalement", {
  path <- file.path(ts_project_root(), "R/sc/sc_helpers.R")
  .REPORT$warns <- list()
  check_c10_error_style(path)
  expect_length(.REPORT$warns, 0L)
})

test_that("C10 : les erreurs joignables de sc_helpers.R portent sc_helpers_error", {
  .load_seurat_pkgs()
  skip_if_not_installed("ggplot2")
  obj <- .toy_seurat()

  # 1-2. feature absente (les deux emplacements) — la valeur recue est citee
  e <- .sc_error_of(plot_enhanced_scatter(obj, "nope1", "nope2"))
  expect_s3_class(e, "sc_helpers_error")
  expect_match(conditionMessage(e), "nope1", fixed = TRUE)

  e <- .sc_error_of(plot_enhanced_scatter(obj, "gene1", "nope2"))
  expect_s3_class(e, "sc_helpers_error")
  expect_match(conditionMessage(e), "nope2", fixed = TRUE)

  # 3. aucun gene valide
  e <- .sc_error_of(plot_violin_enhanced(obj, features = "nope"))
  expect_s3_class(e, "sc_helpers_error")
  expect_match(conditionMessage(e), "valide trouv", fixed = TRUE)

  # 4-5. gene absent, puis trop peu d'echantillons
  e <- .sc_error_of(plot_multi_sample(obj, "nope"))
  expect_s3_class(e, "sc_helpers_error")
  expect_match(conditionMessage(e), "nope", fixed = TRUE)

  e <- .sc_error_of(plot_multi_sample(obj, "gene1"))
  expect_s3_class(e, "sc_helpers_error")
  expect_match(conditionMessage(e), "chantillons requis", fixed = TRUE)

  # 6. gene cible absent
  e <- .sc_error_of(find_correlated_genes(obj, "nope"))
  expect_s3_class(e, "sc_helpers_error")
  expect_match(conditionMessage(e), "nope", fixed = TRUE)

  # 6b. moins de 2 genes exploitables pour la matrice de correlation.
  #     ⚠️ Ne PAS confondre avec `find_correlated_genes()` : appelee avec un gene
  #     VALIDE sur cet objet de test, elle leve « no 'dimnames' attribute for
  #     array ». Cause MESUREE : `.get_norm_matrix()` y rend une matrice 0x0 sans
  #     dimnames, parce que `.toy_seurat()` n'est PAS normalise (pas de couche
  #     `data`). Ce n'est donc PAS un defaut produit — juste un message peu
  #     explicite sur un objet non normalise. Hors perimetre, non couvert ici.
  e <- .sc_error_of(plot_correlation_matrix(obj, features = "nope"))
  expect_s3_class(e, "sc_helpers_error")
  expect_match(conditionMessage(e), "Au moins 2 g", fixed = TRUE)

  # 7. from_type invalide
  e <- .sc_error_of(remap_seurat_ids_to_symbol(obj, from_type = "nope"))
  expect_s3_class(e, "sc_helpers_error")
  expect_match(conditionMessage(e), "from_type doit etre", fixed = TRUE)

  # 8. mapping ENSEMBL insuffisant — message construit par sprintf(), donc
  #    multi-arguments a la SOURCE : on verifie un fragment de la FIN.
  skip_if_not_installed("org.Hs.eg.db")
  skip_if_not_installed("AnnotationDbi")
  mat <- matrix(1, nrow = 5, ncol = 2,
                dimnames = list(c("ENSG00000141510", "ENSG00000012048", "ENSG00000146648",
                                  "ENSG00000136997", "ENSG00000157764"), c("c1", "c2")))
  e <- .sc_error_of(map_ensembl_matrix_to_symbol(mat))
  expect_s3_class(e, "sc_helpers_error")
  expect_match(conditionMessage(e), "5/5", fixed = TRUE)          # non tronque
  expect_match(conditionMessage(e), "organisme 'human'", fixed = TRUE)

  # 9-11. heatmap hierarchique : objet, colonne de groupe, nombre de genes
  skip_if_not_installed("ComplexHeatmap")
  e <- .sc_error_of(build_sc_hierarchical_heatmap(NULL, c("gene1", "gene2")))
  expect_s3_class(e, "sc_helpers_error")
  expect_match(conditionMessage(e), "Aucun objet Single-Cell", fixed = TRUE)

  e <- .sc_error_of(build_sc_hierarchical_heatmap(obj, c("gene1", "gene2"), group_by = "nope"))
  expect_s3_class(e, "sc_helpers_error")
  expect_match(conditionMessage(e), "Colonne de groupe", fixed = TRUE)
  expect_match(conditionMessage(e), "nope", fixed = TRUE)

  e <- .sc_error_of(build_sc_hierarchical_heatmap(obj, c("gene1"), group_by = "seurat_clusters"))
  expect_s3_class(e, "sc_helpers_error")
  expect_match(conditionMessage(e), "Au moins 2 genes valides", fixed = TRUE)

  # 12-13. densite 2D : objet absent, puis feature absente
  skip_if_not_installed("MASS")
  e <- .sc_error_of(plot_sc_expression_density_2d(NULL, "gene1"))
  expect_s3_class(e, "sc_helpers_error")
  expect_match(conditionMessage(e), "Aucun objet Single-Cell", fixed = TRUE)

  e <- .sc_error_of(plot_sc_expression_density_2d(obj, NULL))
  expect_s3_class(e, "sc_helpers_error")
  expect_match(conditionMessage(e), "Aucun gene/feature", fixed = TRUE)

  # 14. reduction 3D : objet absent
  skip_if_not_installed("plotly")
  e <- .sc_error_of(plot_sc_reduction_3d(NULL))
  expect_s3_class(e, "sc_helpers_error")
  expect_match(conditionMessage(e), "Aucun objet Single-Cell", fixed = TRUE)
})

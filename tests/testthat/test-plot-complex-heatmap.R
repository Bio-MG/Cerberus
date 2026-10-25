# =============================================================================
# test-plot-complex-heatmap.R — PLOT-S4 : coeur ComplexHeatmap unifie
# =============================================================================
# Objectif central : ZERO CHANGEMENT DE COMPORTEMENT sur les 3 sites migres.
#
# ⚠️ Les 3 fonctions (plot_heatmap_bulk, plot_sample_correlation_heatmap,
# build_sc_hierarchical_heatmap) n'avaient AUCUN test avant PLOT-S4. Ce fichier
# est donc leur premier filet de securite. Il compare, propriete par propriete,
# la sortie du coeur unifie a la construction ComplexHeatmap HISTORIQUE,
# reconstruite telle quelle dans le test (section 4).
#
# Proprietes comparees : matrice, name (legende), column_title, affichage des
# noms de lignes/colonnes, couleurs de rampe (echantillonnees), et presence du
# clustering + de l'annotation.
# =============================================================================

suppressWarnings(suppressPackageStartupMessages({
  library(ComplexHeatmap)
}))

# D2 M4 (2026-10-25) : R/plotting vit desormais dans l'arbre du paquet
# (tools/pkg_skeleton/R/) — charge via load_project_pkg() (cf. helper-source.R).
load_project_pkg()
source_project_file("R/bulk/bulk_helpers.R")

# --- Fixtures / utilitaires -------------------------------------------------

.hm_fixture <- function() {
  set.seed(42)
  mat <- matrix(stats::rnorm(200), nrow = 20,
                dimnames = list(paste0("g", 1:20), paste0("s", 1:10)))
  meta <- data.frame(condition = factor(rep(c("A", "B"), each = 5)),
                     row.names = colnames(mat))
  list(mat = mat, meta = meta)
}

# draw() renvoie un HeatmapList : on deroule vers le Heatmap sous-jacent.
.hm_unwrap <- function(x) if (inherits(x, "HeatmapList")) x@ht_list[[1]] else x

# draw() exige un device graphique ; en testthat il n'y en a pas forcement.
.hm_with_device <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(expr)
}

# Echantillonne la rampe de couleurs d'un Heatmap (les closures colorRamp2
# creees separement ne sont jamais identical(), on compare les couleurs).
.hm_ramp_colors <- function(ht, n = 7L) {
  rng <- range(ht@matrix, na.rm = TRUE)
  ht@matrix_color_mapping@col_fun(seq(rng[1], rng[2], length.out = n))
}

# --- 1. Surface publique figee ----------------------------------------------

test_that("surface publique figee", {
  expect_setequal(
    ts_complex_heatmap_public_api(),
    c("ts_complex_heatmap", "ts_heatmap_max_names", "ts_heatmap_distances",
      "ts_heatmap_methods", "ts_heatmap_ramps", "ts_complex_heatmap_public_api")
  )
  expect_false(anyDuplicated(ts_complex_heatmap_public_api()) > 0)
  expect_setequal(ts_heatmap_distances(), c("euclidean", "pearson", "spearman"))
  expect_setequal(ts_heatmap_methods(), c("complete", "ward.D2", "average"))
  expect_identical(ts_heatmap_max_names(), 60L)
})

# --- 2. Validation ----------------------------------------------------------

test_that("matrice invalide => invalid_matrix", {
  for (bad in list(NULL, "x", 1:5, list(a = 1))) {
    e <- tryCatch(ts_complex_heatmap(bad, draw = FALSE), error = function(e) e)
    expect_s3_class(e, "plot_heatmap_error")
    expect_identical(e$state, "invalid_matrix")
  }
})

test_that("matrice non numerique ou trop petite => invalid_matrix", {
  m_chr <- matrix(letters[1:4], nrow = 2)
  e <- tryCatch(ts_complex_heatmap(m_chr, draw = FALSE), error = function(e) e)
  expect_s3_class(e, "plot_heatmap_error")
  expect_identical(e$state, "invalid_matrix")

  e2 <- tryCatch(ts_complex_heatmap(matrix(1:4, nrow = 1), draw = FALSE),
                 error = function(e) e)
  expect_s3_class(e2, "plot_heatmap_error")
  expect_identical(e2$state, "invalid_matrix")
})

test_that("rampe inconnue => invalid_ramp", {
  fx <- .hm_fixture()
  e <- tryCatch(ts_complex_heatmap(fx$mat, ramp = "arc-en-ciel", draw = FALSE),
                error = function(e) e)
  expect_s3_class(e, "plot_heatmap_error")
})

test_that("k_row invalide => invalid_k_row", {
  fx <- .hm_fixture()
  for (bad in list(0, 1, NA, "3", c(2, 3))) {
    e <- tryCatch(ts_complex_heatmap(fx$mat, k_row = bad, draw = FALSE),
                  error = function(e) e)
    expect_s3_class(e, "plot_heatmap_error")
    expect_identical(e$state, "invalid_k_row")
  }
  # k_row > nrow(mat)
  e2 <- tryCatch(ts_complex_heatmap(fx$mat, k_row = nrow(fx$mat) + 1L, draw = FALSE),
                 error = function(e) e)
  expect_s3_class(e2, "plot_heatmap_error")
  expect_identical(e2$state, "invalid_k_row")
  # k_row exige cluster_rows = TRUE
  e3 <- tryCatch(ts_complex_heatmap(fx$mat, k_row = 2L, cluster_rows = FALSE, draw = FALSE),
                 error = function(e) e)
  expect_s3_class(e3, "plot_heatmap_error")
  expect_identical(e3$state, "invalid_k_row")
})

test_that("annotations desalignees => meta_mismatch", {
  fx <- .hm_fixture()
  bad_col <- list(group = rep("A", ncol(fx$mat) - 1L))
  e <- tryCatch(ts_complex_heatmap(fx$mat, col_meta = bad_col, draw = FALSE),
                error = function(e) e)
  expect_s3_class(e, "plot_heatmap_error")
  expect_identical(e$state, "meta_mismatch")

  bad_row <- list(group = rep("A", nrow(fx$mat) - 1L))
  e2 <- tryCatch(ts_complex_heatmap(fx$mat, row_meta = bad_row, draw = FALSE),
                 error = function(e) e)
  expect_s3_class(e2, "plot_heatmap_error")
  expect_identical(e2$state, "meta_mismatch")
})

test_that("annotation non nommee => invalid_meta", {
  fx <- .hm_fixture()
  e <- tryCatch(ts_complex_heatmap(fx$mat, col_meta = list(rep("A", ncol(fx$mat))),
                                   draw = FALSE),
                error = function(e) e)
  expect_s3_class(e, "plot_heatmap_error")
  expect_identical(e$state, "invalid_meta")
})

# --- 3. Comportement du coeur ------------------------------------------------

test_that("draw = FALSE rend un Heatmap visible ; draw = TRUE le dessine invisiblement", {
  fx <- .hm_fixture()

  ht <- ts_complex_heatmap(fx$mat, draw = FALSE)
  expect_s4_class(ht, "Heatmap")

  r <- withVisible(.hm_with_device(ts_complex_heatmap(fx$mat, draw = TRUE)))
  expect_false(r$visible)
  expect_s4_class(r$value, "HeatmapList")
})

test_that("regle automatique des noms : <= 60 affiche, > 60 masque", {
  small <- matrix(stats::rnorm(10 * 10), nrow = 10,
                  dimnames = list(paste0("g", 1:10), paste0("s", 1:10)))
  big <- matrix(stats::rnorm(70 * 70), nrow = 70,
                dimnames = list(paste0("g", 1:70), paste0("s", 1:70)))

  ht_s <- ts_complex_heatmap(small, draw = FALSE)
  expect_true(ht_s@row_names_param$show)
  expect_true(ht_s@column_names_param$show)

  ht_b <- ts_complex_heatmap(big, draw = FALSE)
  expect_false(ht_b@row_names_param$show)
  expect_false(ht_b@column_names_param$show)

  # les logiques explicites gagnent sur la regle automatique
  ht_o <- ts_complex_heatmap(big, show_row_names = TRUE, show_column_names = FALSE,
                             draw = FALSE)
  expect_true(ht_o@row_names_param$show)
  expect_false(ht_o@column_names_param$show)
})

test_that("ramp = 'diverging' vs 'sequential' produisent des rampes differentes", {
  fx <- .hm_fixture()
  div <- ts_complex_heatmap(fx$mat, ramp = "diverging", draw = FALSE)
  seq_ <- ts_complex_heatmap(fx$mat, ramp = "sequential", draw = FALSE)
  expect_false(identical(.hm_ramp_colors(div), .hm_ramp_colors(seq_)))
})

test_that("plusieurs annotations de colonnes sont acceptees (data.frame)", {
  fx <- .hm_fixture()
  df <- data.frame(group = fx$meta$condition,
                   batch = factor(rep(c("b1", "b2"), length.out = ncol(fx$mat))),
                   row.names = colnames(fx$mat))
  ht <- ts_complex_heatmap(fx$mat, col_meta = df, draw = FALSE)
  expect_setequal(names(ht@top_annotation@anno_list), c("group", "batch"))
})

test_that("k_row decoupe les lignes en k groupes", {
  fx <- .hm_fixture()
  # Le decoupage (row_split) n'est calcule qu'au moment du draw() : on dessine
  # puis on inspecte l'objet reellement dessine.
  ht <- .hm_unwrap(.hm_with_device(
    ts_complex_heatmap(fx$mat, k_row = 3L, draw = TRUE)
  ))
  expect_s4_class(ht, "Heatmap")
  expect_length(ht@row_order_list, 3L)
})

test_that("clustering_distance/method sont acceptes et appliques", {
  fx <- .hm_fixture()
  for (d in ts_heatmap_distances()) {
    for (m in ts_heatmap_methods()) {
      ht <- ts_complex_heatmap(fx$mat, clustering_distance = d,
                               clustering_method = m, draw = FALSE)
      expect_s4_class(ht, "Heatmap")
    }
  }
})

# --- 4. Equivalence avec la construction HISTORIQUE --------------------------

test_that("plot_heatmap_bulk : sortie equivalente a la construction historique", {
  fx <- .hm_fixture()
  tr <- function(x) x
  new <- .hm_unwrap(.hm_with_device(
    plot_heatmap_bulk(fx$mat, rownames(fx$mat), fx$meta,
                      annotation_col = "condition", tr = tr)
  ))

  # --- construction HISTORIQUE (avant PLOT-S4), recopiee telle quelle ---
  genes <- intersect(rownames(fx$mat), rownames(fx$mat))
  m <- fx$mat[genes, , drop = FALSE]
  m <- t(scale(t(m)))
  grp_vals <- fx$meta[colnames(m), "condition"]
  ann <- ComplexHeatmap::HeatmapAnnotation(group = grp_vals)
  old <- ComplexHeatmap::Heatmap(
    m, name = "Z-score", top_annotation = ann,
    col = bulk_diverging_ramp(range(m, na.rm = TRUE), palette = "default"),
    show_row_names = nrow(m) <= 60,
    column_title = "Heatmap — Gènes Différentiels")
  # ---------------------------------------------------------------------

  expect_identical(new@matrix, old@matrix)
  expect_identical(new@name, old@name)
  expect_identical(new@column_title, old@column_title)
  expect_identical(new@row_names_param$show, old@row_names_param$show)
  expect_identical(new@column_names_param$show, old@column_names_param$show)
  expect_identical(.hm_ramp_colors(new), .hm_ramp_colors(old))
  expect_identical(class(new@top_annotation), class(old@top_annotation))
  expect_identical(names(new@top_annotation@anno_list),
                   names(old@top_annotation@anno_list))
  expect_true(!is.null(new@row_dend_param$cluster))
  expect_true(!is.null(new@column_dend_param$cluster))
})

test_that("plot_sample_correlation_heatmap : sortie equivalente (draw = FALSE conserve)", {
  fx <- .hm_fixture()
  tr <- function(x) x
  new <- plot_sample_correlation_heatmap(fx$mat, fx$meta,
                                         annotation_col = "condition", tr = tr)
  # draw = FALSE => l'objet Heatmap est rendu DIRECTEMENT (pas de HeatmapList)
  expect_s4_class(new, "Heatmap")

  # --- construction HISTORIQUE, recopiee telle quelle ---
  cor_mat <- cor(fx$mat, method = "pearson", use = "pairwise.complete.obs")
  grp_vals <- fx$meta[colnames(cor_mat), "condition"]
  ann <- ComplexHeatmap::HeatmapAnnotation(group = grp_vals)
  old <- ComplexHeatmap::Heatmap(
    cor_mat, name = "Pearson", top_annotation = ann,
    col = bulk_sequential_ramp(c(min(cor_mat), 1), palette = "default"),
    show_row_names = TRUE, show_column_names = TRUE,
    column_title = "Corrélation Inter-Échantillons (QC)",
    cell_fun = function(j, i, x, y, width, height, fill) {
      grid::grid.text(sprintf("%.2f", cor_mat[i, j]), x, y,
                      gp = grid::gpar(fontsize = 8))
    })
  # ------------------------------------------------------

  expect_identical(new@matrix, old@matrix)
  expect_identical(new@name, old@name)
  expect_identical(new@column_title, old@column_title)
  expect_identical(new@row_names_param$show, old@row_names_param$show)
  expect_identical(new@column_names_param$show, old@column_names_param$show)
  expect_identical(.hm_ramp_colors(new), .hm_ramp_colors(old))
  expect_true(!is.null(new@matrix_param$cell_fun))
  expect_true(!is.null(old@matrix_param$cell_fun))
})

test_that("build_sc_hierarchical_heatmap : proprietes historiques preservees", {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("SeuratObject")
  suppressWarnings(suppressPackageStartupMessages({
    library(Seurat)
    library(SeuratObject)
  }))
  load_project_pkg()   # D2 M4 : theme.R vit dans le paquet
  source_project_file("R/sc/sc_helpers.R")

  set.seed(7)
  cnt <- matrix(stats::rpois(60 * 20, lambda = 3), nrow = 20,
                dimnames = list(paste0("gene", 1:20), paste0("cell", 1:60)))
  obj <- SeuratObject::CreateSeuratObject(counts = cnt)
  obj$seurat_clusters <- factor(rep(seq_len(3), length.out = 60))
  obj <- Seurat::NormalizeData(obj, verbose = FALSE)

  ht <- .hm_unwrap(.hm_with_device(
    build_sc_hierarchical_heatmap(obj, features = paste0("gene", 1:10),
                                  group_by = "seurat_clusters")
  ))

  expect_s4_class(ht, "Heatmap")
  # name EN DUR "Z-score" (non traduit) : comportement historique de ce site
  expect_identical(ht@name, "Z-score")
  expect_match(ht@column_title, "^Heatmap Hierarchique -- ")
  expect_true(ht@row_names_param$show)      # 10 lignes <= 60
  expect_true(ht@column_names_param$show)   # 60 colonnes <= 60
  # annotation unique nommee "Groupe" (vs "group" cote Bulk)
  expect_identical(names(ht@top_annotation@anno_list), "Groupe")
  # clustering explicite euclidean/complete (equivalent aux defauts)
  expect_true(!is.null(ht@row_dend_param$cluster))
  expect_true(!is.null(ht@column_dend_param$cluster))
})

# =============================================================================
# test-sc-batch-metrics.R — métrique de mélange des batchs (roadmap 4.3)
# =============================================================================
source_project_file("R/sc/sc_batch_metrics.R")

.make_batch_obj <- function(separated = TRUE, n = 60) {
  # 2 batchs x n cellules, embedding 2D contrôlé
  if (separated) {
    emb <- rbind(cbind(-10 + stats::rnorm(n, 0, .1), stats::rnorm(n, 0, .1)),
                 cbind( 10 + stats::rnorm(n, 0, .1), stats::rnorm(n, 0, .1)))
  } else {
    emb <- rbind(cbind(stats::rnorm(n), stats::rnorm(n)),
                 cbind(stats::rnorm(n), stats::rnorm(n)))
  }
  colnames(emb) <- c("PC_1", "PC_2")
  rownames(emb) <- paste0("c", seq_len(2 * n))
  mat <- matrix(stats::rpois(10 * 2 * n, 1), nrow = 10,
                dimnames = list(paste0("G", seq_len(10)), rownames(emb)))
  obj <- Seurat::CreateSeuratObject(counts = mat)
  obj[["pca"]] <- Seurat::CreateDimReducObject(
    embeddings = emb, key = "PC_", assay = "RNA")
  obj$orig.ident <- factor(rep(c("b1", "b2"), each = n))
  obj
}

test_that("score batchs séparés < score batchs mélangés", {
  skip_if_not_installed("Seurat")
  library(Seurat)
  s_sep <- sc_batch_mixing_score(.make_batch_obj(separated = TRUE))
  s_mix <- sc_batch_mixing_score(.make_batch_obj(separated = FALSE))
  expect_true(s_sep$score < 0.2)                 # batchs isolés : mélange quasi nul
  expect_true(s_sep$score < s_mix$score)
  expect_identical(s_sep$n_cells_used, 120L)
  expect_identical(s_sep$k, 30L)
  expect_setequal(names(s_sep$per_batch), c("b1", "b2"))
})

test_that("déterminisme et erreurs classées", {
  skip_if_not_installed("Seurat")
  library(Seurat)
  obj <- .make_batch_obj(TRUE)
  expect_identical(sc_batch_mixing_score(obj)$score,
                   sc_batch_mixing_score(obj)$score)
  expect_error(sc_batch_mixing_score(obj, batch_col = "absente"),
               class = "sc_batch_metrics_error")
  expect_error(sc_batch_mixing_score(obj, reduction = "umap"),
               class = "sc_batch_metrics_error")
  mono <- obj
  mono$orig.ident <- factor(rep("b1", ncol(mono)))
  expect_error(sc_batch_mixing_score(mono), class = "sc_batch_metrics_error")
})

# =============================================================================
# test-bulk-batch-metrics.R — métrique de mélange des batchs bulk (STAT-S1)
# =============================================================================
# Parité SC (R/sc/sc_batch_metrics.R, roadmap 4.3) transposée au niveau
# échantillon (handoff §4) : le score complète les PCA avant/après ComBat-seq
# par un chiffre. 0 = lots séparés, 1 = mélange parfait.
# =============================================================================
source_project_file("R/bulk/bulk_batch_metrics.R")

.bm_batches <- function(n_per = 6L) rep(c("B1", "B2"), each = n_per)

# Counts simulés (positifs — contrat de la métrique : log1p interne exige >= 0)
.bm_mat <- function(separated = TRUE, n_genes = 60L, n_per = 6L) {
  set.seed(42)
  m <- matrix(stats::rpois(n_genes * 2L * n_per, lambda = 10), nrow = n_genes)
  dimnames(m) <- list(paste0("g", seq_len(n_genes)),
                      paste0("s", seq_len(2L * n_per)))
  # Effet de lot massif sur TOUTES les dimensions : l'ACP sépare les lots.
  if (separated) m[, .bm_batches(n_per) == "B2"] <- m[, .bm_batches(n_per) == "B2"] + 40
  m
}

test_that("batchs séparés ≪ batchs mélangés (score = 1 - pureté kNN)", {
  sep <- bulk_batch_mixing_score(.bm_mat(TRUE),  .bm_batches())
  mix <- bulk_batch_mixing_score(.bm_mat(FALSE), .bm_batches())
  expect_true(sep$score < 0.2,
              info = sprintf("séparé : %s (attendu < 0.2)", sep$score))
  expect_true(mix$score > 0.3,
              info = sprintf("mélangé : %s (attendu > 0.3)", mix$score))
  for (sc in c(sep$score, mix$score)) {
    expect_true(sc >= 0 && sc <= 1)
  }
  expect_identical(names(sep$per_batch), c("B1", "B2"))
  expect_identical(sep$n_samples_used, 12L)
})

test_that("la métrique est déterministe (aucun RNG interne)", {
  a <- bulk_batch_mixing_score(.bm_mat(TRUE), .bm_batches())
  b <- bulk_batch_mixing_score(.bm_mat(TRUE), .bm_batches())
  expect_identical(a, b)
})

test_that("k est plafonné au nombre d'échantillons disponibles", {
  res <- bulk_batch_mixing_score(.bm_mat(TRUE), .bm_batches(), k = 100L)
  expect_identical(res$k, 11L)  # n - 1
})

test_that("erreurs classées bulk_batch_metrics_error (entrées invalides)", {
  expect_error(
    bulk_batch_mixing_score(NULL, .bm_batches()),
    class = "bulk_batch_metrics_error")
  expect_error(
    bulk_batch_mixing_score(data.frame(x = 1:3), .bm_batches()),
    class = "bulk_batch_metrics_error")
  m_na <- .bm_mat(TRUE); m_na[1, 1] <- NA
  expect_error(
    bulk_batch_mixing_score(m_na, .bm_batches()),
    class = "bulk_batch_metrics_error")
  expect_error(
    bulk_batch_mixing_score(.bm_mat(TRUE)[, 1:2, drop = FALSE],
                            c("B1", "B2")),
    class = "bulk_batch_metrics_error")
  m_neg <- .bm_mat(TRUE); m_neg[1, 1] <- -5
  expect_error(
    bulk_batch_mixing_score(m_neg, .bm_batches()),
    class = "bulk_batch_metrics_error")
  expect_error(
    bulk_batch_mixing_score(.bm_mat(TRUE), c("B1", "B2")),
    class = "bulk_batch_metrics_error")
  expect_error(
    bulk_batch_mixing_score(.bm_mat(TRUE), rep("B1", 12L)),
    class = "bulk_batch_metrics_error")
  expect_error(
    bulk_batch_mixing_score(.bm_mat(TRUE), c(.bm_batches(), "B3")),
    class = "bulk_batch_metrics_error")
})

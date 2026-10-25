# =============================================================================
# test-bulk-dose-response.R — Dose-réponse / time-course (NEW-1, MVP drc)
# =============================================================================
# Couvre : gel de la surface publique + des champs contractuels, validations
# (colonne de dose, positivité stricte, nombre de doses distinctes, modèle),
# ajustement RÉEL drc::drm sur une courbe de Hill synthétique (EC50 connu),
# échecs d'ajustement comptabilisés, structure des courbes, plot pur, export,
# assert canonique. Hors-ligne : drc est un calcul local.
# =============================================================================
source_project_file("tools/pkg_skeleton/R/provenance.R")
source_project_file("R/bulk/dose_response.R")

# ── Fixture : courbes de Hill synthétiques, EC50 = 1 connu ────────────────────
# 5 doses x 2 réplicats, réponse montante (g1..g4) et descendante (g5..g8),
# gène plat (g9). resp = 1 + 4/(1+dose) pour b=1, e=1 (LL.4 : c=1, d=5).
.toy_dose_fixture <- function(seed = 123) {
  set.seed(seed)
  doses <- c(0.1, 0.1, 0.3, 0.3, 1, 1, 3, 3, 10, 10)
  smp <- paste0("s", seq_along(doses))
  genes <- paste0("g", seq_len(9))
  m <- matrix(NA_real_, length(genes), length(doses),
              dimnames = list(genes, smp))
  for (g in seq_along(genes)) {
    slope <- if (g <= 4) 1 else if (g <= 8) -1 else 0
    resp <- 3 + slope * (2 * doses / (1 + doses))  # c=1,d=5 (slope=1) / miroir / plat
    m[g, ] <- resp + rnorm(length(doses), sd = 0.03)
  }
  metadata <- data.frame(sample = smp, dose = doses,
                         condition = c("A", "B"),
                         stringsAsFactors = FALSE)
  rownames(metadata) <- smp
  list(mat = m, metadata = metadata)
}

.run_dose_quiet <- function(...) suppressWarnings(run_dose_response(...))

test_that("surface publique figée", {
  expect_setequal(
    bulk_dose_public_api(),
    c("bulk_dose_models", "bulk_dose_contract_fields", "bulk_dose_validity_states",
      "bulk_dose_error_state", "run_dose_response", "plot_dose_response_curve",
      "build_dose_table_export", "assert_bulk_dose_result", "bulk_dose_public_api")
  )
})

test_that("champs contractuels figés", {
  expect_setequal(bulk_dose_models(), c("LL.4", "W1.4", "W2.4", "BC.4"))
  expect_setequal(bulk_dose_contract_fields(),
    c("type", "status", "fits", "curves", "data", "summary",
      "dose_column", "model", "parameters", "qc", "warnings",
      "provenance", "analysis_id", "timestamp_utc"))
  expect_setequal(bulk_dose_validity_states(), c("valid", "valid_with_warnings"))
})

test_that("ajustement réel : EC50 retrouvé autour de 1, r2 élevé, courbes complètes", {
  skip_if_not_installed("drc")
  fx <- .toy_dose_fixture()
  res <- .run_dose_quiet(fx$mat, fx$metadata, "dose",
                         genes = paste0("g", 1:8), model = "LL.4")
  expect_identical(res$type, "bulk_dose_response")
  expect_identical(res$analysis_id, "bulk-dose-response")
  expect_setequal(names(res), bulk_dose_contract_fields())
  expect_true(res$status %in% bulk_dose_validity_states())
  # montants : EC50 ~ 1 (tolérance large sur un demi-decade)
  up <- res$fits[res$fits$gene == "g1", ]
  expect_true(up$fit_ok)
  expect_true(abs(log10(up$ec50)) < 0.5)
  expect_true(is.finite(up$r2) && up$r2 > 0.9)
  # descendants : EC50 aussi ~1
  dn <- res$fits[res$fits$gene == "g5", ]
  expect_true(dn$fit_ok && abs(log10(dn$ec50)) < 0.5)
  # structure des courbes
  cu <- res$curves[["g1"]]
  expect_true(is.data.frame(cu) && all(c("dose", "fit", "lower", "upper") %in% colnames(cu)))
  expect_true(all(cu$lower <= cu$fit + 1e-8) && all(cu$fit <= cu$upper + 1e-8))
  expect_identical(res$summary$n_fit_ok, 8L)
})

test_that("échecs comptabilisés : gène avec Inf -> fit_ok FALSE + warning", {
  skip_if_not_installed("drc")
  fx <- .toy_dose_fixture()
  fx$mat <- rbind(fx$mat, gz = c(rep(3, 4), Inf, rep(3, 5)))  # Inf -> drm échoue
  res <- .run_dose_quiet(fx$mat, fx$metadata, "dose",
                         genes = c(paste0("g", 1:9), "gz"), model = "LL.4")
  expect_identical(res$summary$n_genes_used, 10L)
  expect_identical(res$summary$n_fit_ok, 9L)   # g1..g8 (Hill) + g9 (plat, dégénéré mais convergent)
  expect_identical(res$summary$n_fit_failed, 1L)
  expect_identical(res$status, "valid_with_warnings")
  bad <- res$fits[res$fits$gene == "gz", ]
  expect_false(bad$fit_ok)
  expect_true(is.na(bad$ec50))
  expect_true(length(res$warnings) >= 1L)
  expect_null(res$curves[["gz"]])
  expect_true(is.data.frame(res$curves[["g1"]]))
})

test_that("validations : colonne, positivité, nb de doses, modèle, gènes (états classés)", {
  skip_if_not_installed("drc")
  fx <- .toy_dose_fixture()
  expect_error(.run_dose_quiet(fx$mat, fx$metadata, "condition",
                               genes = "g1", model = "LL.4"),
               "exploitables", fixed = TRUE)  # condition = A/B -> coercition numérique NA
  expect_error(.run_dose_quiet(fx$mat, fx$metadata, "absente",
                               genes = "g1", model = "LL.4"),
               "absente", fixed = TRUE)
  # colonne numérique qualitative : 2 doses distinctes seulement
  meta2 <- fx$metadata
  meta2$dose2 <- rep(c(1, 2), 5)
  expect_error(.run_dose_quiet(fx$mat, meta2, "dose2",
                               genes = "g1", model = "LL.4"),
               "distinct", fixed = TRUE)
  # dose = 0 refusée
  meta0 <- fx$metadata
  meta0$dose[1] <- 0
  expect_error(.run_dose_quiet(fx$mat, meta0, "dose",
                               genes = "g1", model = "LL.4"),
               "STRICTEMENT POSITIVES", fixed = TRUE)
  # modèle hors liste
  expect_error(.run_dose_quiet(fx$mat, fx$metadata, "dose",
                               genes = "g1", model = "LL.5"),
               "non supporté", fixed = TRUE)
  # gènes introuvables
  expect_error(.run_dose_quiet(fx$mat, fx$metadata, "dose",
                               genes = c("absent1", "absent2"), model = "LL.4"),
               "aucun des gènes", fixed = TRUE)
  # état classé extractible
  e <- tryCatch(.run_dose_quiet(fx$mat, fx$metadata, "dose",
                                genes = "absent1", model = "LL.4"),
                error = function(e) e)
  expect_s3_class(e, "bulk_dose_error")
  expect_identical(bulk_dose_error_state(e), "invalid_input")
})

test_that("plot, export et assert canonique", {
  skip_if_not_installed("drc")
  skip_if_not_installed("ggplot2")
  fx <- .toy_dose_fixture()
  res <- .run_dose_quiet(fx$mat, fx$metadata, "dose",
                         genes = paste0("g", 1:8), model = "LL.4")
  p <- plot_dose_response_curve(res, "g1")
  expect_s3_class(p, "ggplot")
  expect_error(plot_dose_response_curve(res, "inconnu"), "pas de courbe", fixed = TRUE)
  tab <- build_dose_table_export(res)
  expect_true(all(c("gene", "ec50", "fit_ok") %in% colnames(tab)))
  expect_identical(nrow(tab), 8L)
  expect_true(isTRUE(assert_bulk_dose_result(res, context = "test")))
  expect_error(assert_bulk_dose_result(list(type = "autre"), "ctx"),
               "non canonique", fixed = TRUE)
})

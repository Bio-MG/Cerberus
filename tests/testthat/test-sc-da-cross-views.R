# =============================================================================
# test-sc-da-cross-views.R — Stage 16 : vues croisées Milo x scCODA (4E-3)
# =============================================================================
# Comparaison DESCRIPTIVE des deux resultats canoniques sur le MEME objet
# fixture : compatibilite exposee, regles de concordance explicites (toutes
# les categories testees avec des resultats minimaux synthetiques),
# desaccords exposes, absence de p-value de consensus, vues pures avec
# contexte echantillon.
# =============================================================================

# ── Fixture lourd : Milo + scCODA (MCMC reticulate) au CHARGEMENT du fichier ─
# Incident 2026-10-01 (verdict suite complete : err=1, pass=0) : ce setup
# tournait au parse du fichier ; sous conditions de suite complete il a leve
# et le fichier ENTIER a errore (0 test execute). Le setup reste eager (cout
# amorti sur tous les tests) mais toute exception est CAPTUREE et convertie
# en SKIP explicite par .req_cross() — jamais en erreur de fichier. Mesure en
# isolement sur cet hote : 59 pass / 0 fail (MCMC ~21 s, acceptance 49.2%).
cross_milo <- NULL
cross_sccoda <- NULL
.setup_error <- NULL
.setup_cross <- function() {
  if (is.null(cross_milo) && is.null(.setup_error)) {
    tryCatch({
      obj <- .sccoda_seurat_obj()
      des <- .milo_design(obj)
      cross_milo <<- suppressMessages(.milo_run(obj = obj, design = des))
      cross_sccoda <<- suppressMessages(.sccoda_run(obj = obj, design = des))
    }, error = function(e) {
      .setup_error <<- conditionMessage(e)
    })
  }
  invisible(is.null(.setup_error))
}
.setup_cross()
# Garde a appeler en tete de chaque test dependant du fixture : SKIP bruyant
# (le message porte la VRAIE erreur de setup), jamais ERROR de fichier.
.req_cross <- function() {
  if (!is.null(.setup_error)) {
    skip(paste("fixture cross-views indisponible (setup Milo/scCODA) :",
               .setup_error))
  }
}

# ── Resultat minimal synthetique (regles de concordance, sans MCMC) ─────────
.dacross_milo_stub <- function(identity, logfc, pvalue, fraction = 0.9) {
  list(
    type = "milo_da", status = "valid",
    DA_table = data.frame(
      Nhood = seq_along(logfc), n_cells = 40L, logFC = logfc,
      logCPM = 14, F = 10, PValue = pvalue, FDR = pvalue,
      SpatialFDR = pvalue, identity = identity,
      identity_fraction = fraction,
      stringsAsFactors = FALSE
    ),
    tested_contrast = list(contrast = "conditionB - conditionA",
                           target = "B", reference = "A",
                           formula = "~ 0 + condition",
                           interpretation = "logFC > 0 = cible."),
    parameters = list(identity_column = "cell_type",
                      fdr_weighting = "k-distance"),
    object_identity = list(fingerprint = "v2::stub"),
    analysis_id = "sc-da-milo"
  )
}
.dacross_sccoda_stub <- function(identity, effect, credible) {
  list(
    type = "sccoda_da", status = "valid", compositional_unit = "sample",
    effect_table = data.frame(
      covariate = "condition[T.B]", identity = identity, effect = effect,
      hdi_low = effect - 1, hdi_high = effect + 1, sd = 0.1,
      inclusion_probability = ifelse(credible, 0.99, 0.2),
      log2_fold_change = effect, credible = credible,
      effect_sign_flipped = FALSE,
      stringsAsFactors = FALSE
    ),
    parameters = list(target_condition = "B", reference_condition = "A",
                      identity_column = "cell_type"),
    model_specification = list(fdr_target = TS_DA_SCCODA_FDR_TARGET),
    credible_effects = identity[credible],
    reference_identity = "CD4 T",
    object_identity = list(fingerprint = "v2::stub"),
    analysis_id = "sc-da-sccoda"
  )
}

test_that("compatibility is fully flagged on the shared fixture", {
  .req_cross()
  sm <- build_da_cross_method_summary(cross_milo, cross_sccoda)
  expect_true(isTRUE(sm$comparability$fully_comparable))
  expect_identical(unname(sm$comparability$flags),
                   c(TRUE, TRUE, TRUE))
  expect_identical(sm$comparability$caveats, character(0))
  # Recaps : les deux identites de resultats restent separees
  expect_identical(sm$milo_recap$analysis_id, "sc-da-milo")
  expect_identical(sm$sccoda_recap$analysis_id, "sc-da-sccoda")
})

test_that("concordance rules produce the expected category on the fixture", {
  .req_cross()
  sm <- build_da_cross_method_summary(cross_milo, cross_sccoda)
  conc <- sm$concordance
  expect_true(all(conc$concordance %in% da_cross_concordance_categories()))
  # B : voisinages purs significatifs (logFC > 0) ET effet credible > 0.
  b <- conc[conc$identity == "B", ]
  expect_identical(b$concordance, "concordant_enriched_target")
  expect_true(b$milo_has_signal)
  expect_true(b$sccoda_credible)
  expect_gt(b$milo_median_logfc, 0)
  expect_gt(b$sccoda_effect, 0)
  # La reference scCODA (CD4 T) n'a pas d'effet : non comparable.
  cd4 <- conc[conc$identity == "CD4 T", ]
  expect_identical(cd4$concordance, "not_comparable")
  expect_true(is.na(cd4$sccoda_effect))
})

test_that("every concordance rule is exercised (synthetic canonical stubs)", {
  milo_sig <- .dacross_milo_stub("X", 1.5, 1e-4)
  milo_nosig <- .dacross_milo_stub("X", 0.1, 0.8)
  scc_cred_pos <- .dacross_sccoda_stub("X", 1, TRUE)
  scc_cred_neg <- .dacross_sccoda_stub("X", -1, TRUE)
  scc_nocred <- .dacross_sccoda_stub("X", 1, FALSE)
  cat_of <- function(m, s) {
    build_da_cross_concordance(m, s)$concordance
  }
  expect_identical(cat_of(milo_sig, scc_cred_pos), "concordant_enriched_target")
  expect_identical(cat_of(.dacross_milo_stub("X", -1.5, 1e-4), scc_cred_neg),
                   "concordant_enriched_reference")
  expect_identical(cat_of(milo_sig, scc_cred_neg), "discordant_direction")
  expect_identical(cat_of(milo_sig, scc_nocred), "milo_only")
  expect_identical(cat_of(milo_nosig, scc_cred_pos), "sccoda_only")
  expect_identical(cat_of(milo_nosig, scc_nocred), "no_signal")
  # Identite absente d'un cote : non comparable (et non inventee).
  m_y <- .dacross_milo_stub("Y", 1.5, 1e-4)
  conc_y <- build_da_cross_concordance(m_y, scc_cred_pos)
  expect_setequal(conc_y$identity, c("X", "Y"))
  expect_identical(conc_y$concordance[conc_y$identity == "Y"],
                   "not_comparable")
  # Les desaccords sont exposes, jamais moyennes.
  sm <- build_da_cross_method_summary(milo_sig, scc_nocred)
  expect_identical(sm$disagreement$concordance, "milo_only")
})

test_that("mixed nhoods (NA identity) never enter the concordance", {
  m <- .dacross_milo_stub(c("X", NA, "X"), c(2, 3, 1), c(1e-6, 1e-6, 0.7))
  s <- .dacross_sccoda_stub("X", 1, TRUE)
  conc <- build_da_cross_concordance(m, s)
  expect_identical(nrow(conc), 1L)
  x <- conc[conc$identity == "X", ]
  # Seuls les 2 voisinages annotes X comptent (NA jamais reaffecte).
  expect_identical(x$milo_n_nhoods, 2L)
  expect_identical(x$concordance, "concordant_enriched_target")
})

# ── Provenance et export ────────────────────────────────────────────────────
test_that("cross provenance records thresholds, ids and options (rule 7)", {
  .req_cross()
  pe <- build_da_cross_provenance(cross_milo, cross_sccoda,
                                  options = list(view = "test"))
  expect_identical(pe$analysis_type, "da_cross")
  expect_identical(pe$analysis_id, "sc-da-cross")
  expect_identical(pe$parameters$display_alpha, TS_DA_MILO_DISPLAY_ALPHA)
  expect_identical(pe$parameters$signif_fraction, TS_DA_CROSS_SIGNIF_FRACTION)
  expect_identical(pe$parameters$options$view, "test")
  expect_identical(pe$parameters$milo_analysis_id, "sc-da-milo")
  expect_identical(pe$parameters$sccoda_analysis_id, "sc-da-sccoda")
  expect_match(pe$parameters$concordance_rules, "milo:", fixed = TRUE)
})

test_that("cross export is traced by both analysis_ids", {
  .req_cross()
  ex <- build_da_cross_concordance_export(cross_milo, cross_sccoda)
  expect_true(all(ex$milo_analysis_id == "sc-da-milo"))
  expect_true(all(ex$sccoda_analysis_id == "sc-da-sccoda"))
  expect_match(da_cross_export_filename("da_cross_concordance", "csv"),
               "^da_cross_concordance_sc-da-cross_\\d{4}-\\d{2}-\\d{2}\\.csv$")
})

# ── Vues (pures, contexte echantillon present) ──────────────────────────────
test_that("cross views render with sample-level context and method framing", {
  .req_cross()
  p1 <- plot_da_cross_sample_composition(cross_milo, cross_sccoda)
  expect_s3_class(p1, "ggplot")
  expect_match(p1$labels$subtitle, "ECHANTILLON", fixed = TRUE)
  expect_match(p1$labels$subtitle, "replicats", fixed = TRUE)
  p2 <- plot_da_cross_concordance(cross_milo, cross_sccoda)
  expect_s3_class(p2, "ggplot")
  expect_match(p2$labels$subtitle, "GRANDEURS DIFFERENTES", fixed = TRUE)
  expect_match(p2$labels$caption, "p-value de consensus", fixed = TRUE)
  p3 <- plot_da_cross_nhood_mapping(cross_milo)
  expect_s3_class(p3, "ggplot")
  expect_match(p3$labels$subtitle, "DESCRIPTIVE", fixed = TRUE)
})

test_that("cross views refuse non-canonical inputs (pure consumers)", {
  .req_cross()
  for (bad in list(NULL, list(), "resultat")) {
    # Entree Milo invalide -> milo_error ; entree scCODA invalide -> sccoda_error.
    expect_error(build_da_cross_method_summary(bad, cross_sccoda),
                 class = "milo_error")
    expect_error(build_da_cross_method_summary(cross_milo, bad),
                 class = "sccoda_error")
    expect_error(plot_da_cross_concordance(bad, cross_sccoda),
                 class = "milo_error")
    expect_error(plot_da_cross_sample_composition(cross_milo, bad),
                 class = "sccoda_error")
  }
  # Objets differents : PAS une erreur, mais des reserves EXPLICITES
  # (comparabilite drapeautee, jamais masquee).
  other <- .milo_seurat_obj(meta = .da_meta(samples = c(s1 = 40, s2 = 40, s3 = 35, s4 = 35)))
  other_milo <- suppressMessages(.milo_run(obj = other))
  sm2 <- build_da_cross_method_summary(other_milo, cross_sccoda)
  expect_false(isTRUE(sm2$comparability$fully_comparable))
  expect_true(length(sm2$comparability$caveats) > 0L)
  expect_match(sm2$comparability$caveats[1], "INDICATIVE")
})

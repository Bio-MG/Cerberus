# =============================================================================
# test-bulk-survival.R — Survie & associations cliniques (Bulk V2 M5)
# =============================================================================
# Couvre les gardes de la mission : temps numérique > 0, statut 0/1 (codage
# 1/2 explicite), minimum 10 événements, découpes médiane/quartiles avec
# REFUS du cutpoint optimal, Cox univariés triés par p + ajustement BH,
# candidats temps/statut, figure KM (survminer ou repli ggplot).
# =============================================================================
source_project_file("tools/pkg_skeleton/R/validation.R")
source_project_file("tools/pkg_skeleton/R/provenance.R")
# D2 M4 (2026-10-25) : R/plotting vit desormais dans l'arbre du paquet
# (tools/pkg_skeleton/R/) — charge via load_project_pkg() (cf. helper-source.R).
load_project_pkg()
source_project_file("tools/pkg_skeleton/R/bulk_survival.R")

.meta_m5 <- function(n = 40, seed = 1) {
  set.seed(seed)
  data.frame(row.names = paste0("s", seq_len(n)),
             os_days = sample(200:3000, n, replace = TRUE),
             os_event = sample(c(0, 1), n, replace = TRUE, prob = c(0.4, 0.6)),
             age = sample(40:80, n, replace = TRUE),
             site = rep(c("A", "B", "C"), length.out = n))
}

test_that("surface publique figée", {
  expect_setequal(
    bulk_survival_public_api(),
    c("bulk_survival_public_api", "bulk_survival_candidates",
      "bulk_survival_validate_metadata", "bulk_survival_split_groups",
      "bulk_survival_km", "bulk_survival_cox", "plot_survival_km",
      "build_survival_export")
  )
})

test_that("candidats temps/statut : heuristique nominale pour l'UI", {
  cand <- bulk_survival_candidates(.meta_m5())
  expect_true("os_days" %in% cand$time_cols)
  expect_true("age" %in% cand$time_cols)          # numérique > 0 (nominale)
  expect_true("os_event" %in% cand$status_cols)
  expect_false("site" %in% cand$status_cols)      # 3 niveaux non numériques
  expect_identical(bulk_survival_candidates(NULL)$time_cols, character(0))
})

test_that("validation : temps numérique, statut 0/1, codage 1/2 explicite, >= 10 événements", {
  meta <- .meta_m5()
  val <- bulk_survival_validate_metadata(meta, "os_days", "os_event")
  expect_identical(val$n_obs, 40L)
  expect_gte(val$n_events, 10L)
  expect_true(all(val$status %in% c(0, 1)))
  expect_setequal(val$samples, rownames(meta))

  # temps non numérique
  meta_bad <- meta; meta_bad$os_days <- as.character(meta_bad$os_days)
  e1 <- tryCatch(bulk_survival_validate_metadata(meta_bad, "os_days", "os_event"),
                 error = function(e) e)
  expect_s3_class(e1, "bulk_survival_error")
  expect_identical(e1$state, "invalid_time")

  # statut hors codage
  meta_bad2 <- meta; meta_bad2$os_event[1:2] <- 5
  e2 <- tryCatch(bulk_survival_validate_metadata(meta_bad2, "os_days", "os_event"),
                 error = function(e) e)
  expect_identical(e2$state, "invalid_status")

  # codage 1/2 déclaré -> accepté et re-calibré
  meta12 <- meta; meta12$os_event <- ifelse(meta12$os_event == 1, 2, 1)
  val12 <- bulk_survival_validate_metadata(meta12, "os_days", "os_event",
                                           status_coding = "1/2")
  expect_identical(val12$n_events, val$n_events)
  # codage 1/2 NON déclaré -> erreur (valeurs hors 0/1)
  e3 <- tryCatch(bulk_survival_validate_metadata(meta12, "os_days", "os_event"),
                 error = function(e) e)
  expect_identical(e3$state, "invalid_status")

  # moins de 10 événements (garde mission)
  meta_low <- meta; meta_low$os_event <- 0; meta_low$os_event[1] <- 1
  e4 <- tryCatch(bulk_survival_validate_metadata(meta_low, "os_days", "os_event"),
                 error = function(e) e)
  expect_identical(e4$state, "min_events")
  expect_match(conditionMessage(e4), "10")
})

test_that("découpes : médiane, quartiles (milieu exclu), cutpoint optimal REFUSÉ", {
  x <- c(1:19, 1000)
  g1 <- bulk_survival_split_groups(x, split = "median")
  expect_setequal(levels(g1$groups), c("Low", "High"))
  expect_identical(g1$split, "median")
  expect_identical(sum(is.na(g1$groups)), 0L)

  g2 <- bulk_survival_split_groups(1:40, split = "quartile")
  expect_identical(g2$n_excluded, 20L)   # 50 % du milieu exclus, jamais caché
  expect_identical(sum(g2$groups == "Low", na.rm = TRUE), 10L)
  expect_identical(sum(g2$groups == "High", na.rm = TRUE), 10L)

  e <- tryCatch(bulk_survival_split_groups(x, split = "optimal"), error = function(e) e)
  expect_s3_class(e, "bulk_survival_error")
  expect_identical(e$state, "invalid_input")
  expect_match(conditionMessage(e), "cutpoint")
})

test_that("KM : survfit + log-rank, provenance sans cutpoint", {
  meta <- .meta_m5()
  val <- bulk_survival_validate_metadata(meta, "os_days", "os_event")
  feat <- rnorm(40, 5, 1); names(feat) <- rownames(meta)
  km <- bulk_survival_km(feat, val, split = "median", feature_label = "GENE1")
  expect_identical(km$type, "bulk_survival_km")
  expect_s3_class(km$fit, "survfit")
  expect_true(is.finite(km$logrank_p))
  expect_true(km$logrank_p >= 0 && km$logrank_p <= 1)
  expect_identical(km$provenance$parameters$cutpoint_search, FALSE)
  # figure
  p <- plot_survival_km(km)
  expect_true(inherits(p, "ggsurvplot") || inherits(p, "ggplot"))
})

test_that("Cox univariés : tri par p, HR cohérent, BH dès 2 variables", {
  meta <- .meta_m5(seed = 2)
  val <- bulk_survival_validate_metadata(meta, "os_days", "os_event")
  # gène pronostique : expression liée au risque simulé
  risk <- (meta$os_days < 1000) * 1
  good <- (risk + 1) * rnorm(40, 5, 0.3)
  noise <- rnorm(40, 5, 1)
  feat <- rbind(GOOD = good, NOISE = noise)
  colnames(feat) <- rownames(meta)
  cox_df <- bulk_survival_cox(feat, val)
  expect_identical(cox_df$feature[1], "GOOD")   # trié par p
  expect_gt(cox_df$hr[cox_df$feature == "GOOD"], 1)
  expect_true(all(cox_df$hr_lower <= cox_df$hr & cox_df$hr <= cox_df$hr_upper))
  expect_true(all(cox_df$p >= 0 & cox_df$p <= 1))
  expect_false(anyNA(cox_df$p_adj_BH))
  expect_match(attr(cox_df, "warnings"), "BH")
  # échantillons absents de la matrice -> erreur classée
  e <- tryCatch(bulk_survival_cox(feat[, 1:2, drop = FALSE], val),
                error = function(e) e)
  expect_identical(e$state, "invalid_input")
  expect_error(bulk_survival_cox(NULL, val), class = "bulk_survival_error")
})

test_that("export Cox : note de multi-testing attachée", {
  cox_df <- data.frame(feature = c("A", "B"), hr = c(2, 0.5), p = c(0.01, 0.2),
                       p_adj_BH = c(0.02, 0.2), stringsAsFactors = FALSE)
  attr(cox_df, "warnings") <- "2 variables testées — p ajustées (BH) disponibles."
  ex <- build_survival_export(cox_df)
  expect_identical(nrow(ex), 2L)
  expect_match(attr(ex, "multiple_testing_note"), "BH")
  expect_error(build_survival_export(data.frame(x = 1)), class = "bulk_survival_error")
})

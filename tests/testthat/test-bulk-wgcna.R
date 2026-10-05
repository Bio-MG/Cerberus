# =============================================================================
# test-bulk-wgcna.R — WGCNA safe-mode (Bulk V2 M4)
# =============================================================================
# Couvre les gardes de la mission : arrêt dur N < 15 (testé avec N = 6),
# counts bruts refusés, pré-filtrage HVG borné [2000, 5000], choix du power
# (cible R2 >= 0.80, repli meilleur R2 documenté), pipeline réel WGCNA
# (power + modules + bicor MEs/traits), plots pures, export.
# =============================================================================
source_project_file("R/core/validation.R")
source_project_file("R/core/provenance.R")
source_project_file("R/plotting/theme.R")
source_project_file("R/bulk/bulk_batch_qc.R")
source_project_file("R/bulk/bulk_wgcna.R")

test_that("surface publique figée", {
  expect_setequal(
    bulk_wgcna_public_api(),
    c("bulk_wgcna_public_api", "bulk_wgcna_select_hvg", "bulk_wgcna_choose_power",
      "bulk_wgcna_pick_power", "bulk_wgcna_build_modules", "bulk_wgcna_prepare_traits",
      "bulk_wgcna_module_trait", "plot_wgcna_soft_threshold",
      "plot_wgcna_trait_heatmap", "build_wgcna_export")
  )
})

test_that("GARDE MISSION : N = 6 -> arrêt dur, message citant N et le plancher", {
  m6 <- matrix(rnorm(300 * 6), 300, 6,
               dimnames = list(paste0("g", 1:300), paste0("s", 1:6))) + 8
  err <- tryCatch(bulk_wgcna_pick_power(m6), error = function(e) e)
  expect_s3_class(err, "bulk_wgcna_error")
  expect_identical(err$state, "samples_min")
  expect_match(conditionMessage(err), "6")
  expect_match(conditionMessage(err), "15")
  err2 <- tryCatch(bulk_wgcna_build_modules(m6, power = 2), error = function(e) e)
  expect_identical(err2$state, "samples_min")
})

test_that("GARDE MISSION : counts bruts refusés (garde QC batch re-classée)", {
  set.seed(2)
  raw <- matrix(rpois(500 * 20, lambda = 300), 500, 20,
                dimnames = list(paste0("g", 1:500), paste0("s", 1:20)))
  err <- tryCatch(bulk_wgcna_select_hvg(raw), error = function(e) e)
  expect_s3_class(err, "bulk_wgcna_error")
  expect_identical(err$state, "raw_counts_rejected")
})

test_that("pré-filtrage HVG : bornes [2000, 5000] en dur, déterministe", {
  set.seed(3)
  m3000 <- matrix(rnorm(3000 * 20), 3000, 20,
                  dimnames = list(paste0("g", 1:3000), paste0("s", 1:20))) + 8
  sel <- bulk_wgcna_select_hvg(m3000, n_top = 2500)
  expect_identical(sel$n_used, 2500L)
  expect_length(sel$warnings, 1L)
  sel2 <- bulk_wgcna_select_hvg(m3000, n_top = 10)
  expect_identical(sel2$n_used, 2000L)   # plancher mission
  sel3 <- bulk_wgcna_select_hvg(m3000, n_top = 99999)
  expect_identical(sel3$n_used, 3000L)   # sous le plafond : tout conservé
  sel4 <- bulk_wgcna_select_hvg(m3000, n_top = 2500)
  expect_identical(rownames(sel$mat), rownames(sel4$mat))  # déterministe
  m30 <- m3000[1:30, ]
  e <- tryCatch(bulk_wgcna_select_hvg(m30), error = function(e) e)
  expect_identical(e$state, "too_few_genes")
})

test_that("choix du power : premier R2 >= cible, sinon meilleur R2 + avertissement", {
  pt <- data.frame(Power = c(1, 2, 3, 4, 5), SFT.R.sq = c(0.3, 0.5, 0.85, 0.9, 0.95),
                   mean.k. = c(100, 60, 30, 15, 8))
  ch <- bulk_wgcna_choose_power(pt)
  expect_identical(ch$power, 3)
  expect_true(ch$target_reached)
  expect_true(is.na(ch$warning))
  pt2 <- data.frame(Power = c(1, 2), SFT.R.sq = c(0.4, 0.6), mean.k. = c(90, 50))
  ch2 <- bulk_wgcna_choose_power(pt2)
  expect_false(ch2$target_reached)
  expect_identical(ch2$power, 2)
  expect_match(ch2$warning, "Aucun power")
  expect_error(bulk_wgcna_choose_power(data.frame(x = 1)), class = "bulk_wgcna_error")
})

test_that("préparation des traits : numériques gardés, binaires codés 0/1, multi-niveaux écartés", {
  meta <- data.frame(row.names = paste0("s", 1:20),
                     age = seq(30, 68, by = 2),
                     sex = rep(c("F", "M"), 10),
                     batch3 = rep(c("b1", "b2", "b3"), length.out = 20),
                     note = c("a", "b", NA, "", "d"))
  prep <- bulk_wgcna_prepare_traits(meta)
  expect_setequal(colnames(prep$traits), c("age", "sex"))
  expect_setequal(prep$dropped, c("batch3", "note"))
  expect_identical(sort(unique(prep$traits$sex)), c(0, 1))
  e <- tryCatch(bulk_wgcna_prepare_traits(meta[, "note", drop = FALSE]),
                error = function(e) e)
  expect_s3_class(e, "bulk_wgcna_error")
  expect_identical(e$state, "no_traits")
  expect_error(bulk_wgcna_prepare_traits(NULL), class = "bulk_wgcna_error")
})

test_that("p-value de corrélation : formule Student asymptotique pure", {
  cm <- matrix(c(0.9, -0.3), 1, 2)
  pv <- .bulk_wgcna_cor_pvalue(cm, 20)
  expect_lt(pv[1], 0.001)
  expect_gt(pv[2], 0.1)
  expect_true(all(pv >= 0 & pv <= 1))
})

test_that("pipeline réel WGCNA : power -> modules -> bicor MEs/traits", {
  skip_if_not_installed("WGCNA")
  set.seed(2)
  sim <- matrix(rnorm(2500 * 20), 2500, 20,
                dimnames = list(paste0("g", 1:2500), paste0("s", 1:20))) + 8
  pw <- bulk_wgcna_pick_power(sim, powers = c(1, 2, 4, 6))
  expect_identical(pw$type, "bulk_wgcna_power")
  expect_identical(nrow(pw$power_table), 4L)
  expect_true(all(c("Power", "SFT.R.sq", "mean.k.") %in% colnames(pw$power_table)))
  expect_true(is.numeric(pw$chosen$power))
  expect_match(pw$provenance$method, "pickSoftThreshold")

  mods <- bulk_wgcna_build_modules(sim, power = 6, n_top = 2000)
  expect_identical(mods$type, "bulk_wgcna_modules")
  expect_identical(length(mods$colors), mods$n_genes_used)
  expect_false(any(is.na(mods$colors)))
  expect_false(is.null(mods$dendro))
  expect_false(is.null(mods$dendro_colors))
  # The names are the SELECTED GENE ids (rownames of the VST matrix): the HVG
  # step keeps the top-varied rows, so assert membership/count, not a prefix.
  expect_length(names(mods$colors), mods$n_genes_used)
  expect_false(any(duplicated(names(mods$colors))))
  expect_true(all(names(mods$colors) %in% rownames(sim)))

  traits_df <- data.frame(row.names = paste0("s", 1:20),
                          x = seq(1, 20), y = rep(c(0, 1), 10))
  mt <- bulk_wgcna_module_trait(mods, traits_df)
  expect_identical(mt$method, "bicor")
  expect_identical(dim(mt$cor), c(ncol(mods$MEs), 2L))
  expect_true(all(mt$pval >= 0 & mt$pval <= 1))
  expect_true(mt$n_samples == 20L)
})

test_that("F7 (réel 2026-10-05) : names(colors) sont des GÈNES de la VST, jamais des échantillons", {
  skip_if_not_installed("WGCNA")
  set.seed(7)
  # Noms DISTINCTS par nature : GENE* en lignes, MW_sample_* en colonnes —
  # toute fuite de colnames dans les noms de colors devient visible.
  sim <- matrix(rnorm(2500 * 18), 2500, 18,
                dimnames = list(paste0("GENE", 1:2500),
                                paste0("MW_sample_", 1:18))) + 8
  mods <- bulk_wgcna_build_modules(sim, power = 6, n_top = 2000)
  # Niveau moteur : les noms de `colors` sont les ids de gènes SÉLECTIONNÉS,
  # jamais les noms d'échantillons, jamais NA (le `names<-` de R complete par
  # NA au lieu d'erreur — le défaut réel mesuré sur GSE164073).
  expect_false(any(is.na(names(mods$colors))))
  expect_true(all(names(mods$colors) %in% rownames(sim)))
  expect_length(names(mods$colors), mods$n_genes_used)
  expect_false(any(duplicated(names(mods$colors))))
  expect_false(any(is.na(names(mods$dendro_colors))))
  expect_true(all(names(mods$dendro_colors) %in% rownames(sim)))
  # Niveau export : une ligne par gène utilisé, chaque `gene` un id présent
  # dans la matrice VST, aucun NA.
  ex <- build_wgcna_export(mods)
  expect_identical(nrow(ex), as.integer(mods$n_genes_used))
  expect_false(any(is.na(ex$gene)))
  expect_true(all(ex$gene %in% rownames(sim)))
  expect_false(any(duplicated(ex$gene)))
})

test_that("plots et export : consommateurs purs", {
  pw <- list(power_table = data.frame(Power = c(1, 2, 4), SFT.R.sq = c(0.4, 0.82, 0.9),
                                      mean.k. = c(90, 40, 20)),
             chosen = list(power = 2, r2 = 0.82))
  plots <- plot_wgcna_soft_threshold(pw)
  expect_s3_class(plots$fit, "ggplot")
  expect_s3_class(plots$connectivity, "ggplot")
  cm <- matrix(c(0.7, -0.2, 0.1, 0.4), 2, 2, dimnames = list(c("ME1", "ME2"), c("age", "sex")))
  mt <- list(cor = cm, pval = .bulk_wgcna_cor_pvalue(cm, 20), method = "bicor", n_samples = 20)
  expect_s3_class(plot_wgcna_trait_heatmap(mt), "ggplot")
  mods <- list(type = "bulk_wgcna_modules",
               colors = c(g1 = "grey", g2 = "blue", g3 = "blue"))
  ex <- build_wgcna_export(mods)
  expect_setequal(colnames(ex), c("gene", "module"))
  expect_identical(nrow(ex), 3L)
  expect_error(build_wgcna_export(list()), class = "bulk_wgcna_error")
  expect_error(plot_wgcna_soft_threshold(list()), class = "bulk_wgcna_error")
})

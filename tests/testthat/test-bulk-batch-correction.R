# =============================================================================
# test-bulk-batch-correction.R — STAT-S1 : correction de batch (ComBat-seq)
# =============================================================================
# Couvre la logique PURE de tools/pkg_skeleton/R/batch_correction.R hors Shiny :
#   - garde counts bruts (symétrique de bulk_assert_transformed_matrix) ;
#   - contrôle du plan (réutilise bulk_batch_design_check) ;
#   - libellé de provenance ;
#   - exécution réelle de ComBat-seq : le lot est corrigé, la condition est
#     PRÉSERVÉE (c'est le critère d'acceptation de STAT-S1) ;
#   - limite connue documentée (un décalage uniforme = effet de profondeur) ;
#   - chemins d'erreur classés + préservation des dimnames.
#
# ⚠️ FIXTURE : counts NB sur-dispersés (rnbinom, size = 10) et effet lot
#    MULTIPLICATIF PAR GÈNE. Un décalage additif uniforme sur tous les gènes
#    n'est PAS un effet lot : c'est un effet de profondeur de séquençage, que
#    ComBat-seq absorbe par l'offset de bibliothèque et ne corrige donc pas
#    (comportement attendu, gelé par le dernier test de ce fichier). Un
#    fixture Poisson sans sur-dispersion dégénère le modèle NB de sva et ne
#    mesure rien — les deux pièges ont été rencontrés puis corrigés.
# =============================================================================
source_project_file("tools/pkg_skeleton/R/io_helpers.R")
source_project_file("R/core/validation.R")
source_project_file("tools/pkg_skeleton/R/bulk_batch_qc.R")
source_project_file("tools/pkg_skeleton/R/batch_correction.R")

# ── Fixture : effet lot multiplicatif par gène + effet condition réel ───────
.bc_fixture <- function(seed = 42L, n_genes = 400L, n_per_batch = 3L) {
  set.seed(seed)
  samples <- paste0("s", seq_len(2L * n_per_batch))
  batch   <- rep(c("B1", "B2"), each = n_per_batch)
  # Condition volontairement NON alignée sur le lot (A et B dans CHAQUE lot)
  cond    <- rep(c("A", "B"), length.out = 2L * n_per_batch)
  genes   <- paste0("g", seq_len(n_genes))

  mu  <- rgamma(n_genes, shape = 4, scale = 25)
  cnt <- matrix(0, n_genes, length(samples), dimnames = list(genes, samples))
  for (j in seq_along(samples)) cnt[, j] <- rnbinom(n_genes, mu = mu, size = 10)

  # Effet lot : facteur multiplicatif PAR GÈNE (jamais uniforme — cf. en-tête)
  bf <- exp(rnorm(n_genes, 0, 0.8))
  b2 <- which(batch == "B2")
  cnt[, b2] <- matrix(rnbinom(n_genes * length(b2), mu = mu * bf, size = 10),
                      n_genes, length(b2))

  # Effet condition : x2 sur la moitié des gènes
  sig <- seq_len(n_genes %/% 2L)
  cb  <- which(cond == "B")
  cnt[sig, cb] <- matrix(rnbinom(length(sig) * length(cb), mu = mu[sig] * 2, size = 10),
                         length(sig), length(cb))

  list(counts = cnt, batch = batch, cond = cond, sig = sig,
       n_per_batch = n_per_batch)
}

# Écart moyen entre 2 groupes, moyenné sur les gènes.
.bc_group_gap <- function(mat, grouping, genes = NULL) {
  m  <- if (is.null(genes)) mat else mat[genes, , drop = FALSE]
  lv <- unique(grouping)
  mean(rowMeans(m[, grouping == lv[1], drop = FALSE]) -
       rowMeans(m[, grouping == lv[2], drop = FALSE]))
}

# Force de l'effet lot, par gène, sur l'échelle log2 (indépendante de l'échelle).
.bc_log2_batch_effect <- function(mat, batch) {
  b2 <- which(batch == unique(batch)[2L]); b1 <- which(batch == unique(batch)[1L])
  mean(abs(log2((mat[, b2, drop = FALSE] + 1) / (mat[, b1, drop = FALSE] + 1))))
}

# Critère de la roadmap : séparation des lots sur PC1 (R² du lot sur PC1).
.bc_batch_pc1_r2 <- function(mat, batch) {
  p <- stats::prcomp(t(log2(mat + 1)), scale. = TRUE)
  summary(stats::lm(p$x[, 1L] ~ factor(batch)))$r.squared
}

# ── Garde counts bruts ─────────────────────────────────────────────────────
test_that("bulk_assert_raw_counts accepts counts and rejects transformed input", {
  fx <- .bc_fixture()
  expect_invisible(bulk_assert_raw_counts(fx$counts))

  # Matrice VST-like : continue -> refusée, état dédié
  vst <- log2(fx$counts + 1) + 0.123456
  err <- tryCatch(bulk_assert_raw_counts(vst), error = function(e) e)
  expect_s3_class(err, "bulk_batch_correction_error")
  expect_identical(err$state, "not_raw_counts")
  expect_match(conditionMessage(err), "VST")

  # Symétrie des deux gardes : ce que l'une accepte, l'autre le refuse.
  expect_error(bulk_assert_transformed_matrix(fx$counts), class = "bulk_batch_qc_error")
  expect_error(bulk_assert_raw_counts(vst), class = "bulk_batch_correction_error")

  expect_identical(tryCatch(bulk_assert_raw_counts(NULL), error = function(e) e)$state,
                   "invalid_input")
  expect_identical(tryCatch(bulk_assert_raw_counts("x"), error = function(e) e)$state,
                   "invalid_input")

  neg <- fx$counts; neg[1, 1] <- -5
  expect_identical(tryCatch(bulk_assert_raw_counts(neg), error = function(e) e)$state,
                   "invalid_input")

  na_m <- fx$counts; na_m[1, 1] <- NA
  expect_identical(tryCatch(bulk_assert_raw_counts(na_m), error = function(e) e)$state,
                   "invalid_input")

  expect_identical(tryCatch(bulk_assert_raw_counts(fx$counts[, 1, drop = FALSE]),
                            error = function(e) e)$state,
                   "invalid_input")
})

# ── PLOT-S6c (P0) : une matrice PRESQUE entière n'est plus tolérée ─────────
test_that("bulk_assert_raw_counts refuse une matrice presque entière (P0)", {
  fx <- .bc_fixture()
  # 1 % de valeurs continues. L'ANCIENNE règle (>= 95 % d'entiers) laissait
  # passer, puis build_dds() arrondissait CES valeurs avec un simple warning —
  # les chiffres analysés n'étaient plus ceux fournis, sans trace.
  mixed <- fx$counts
  idx <- seq_len(max(1L, floor(0.01 * length(mixed))))
  mixed[idx] <- mixed[idx] + 0.37

  err <- tryCatch(bulk_assert_raw_counts(mixed), error = function(e) e)
  expect_s3_class(err, "bulk_batch_correction_error")
  expect_identical(err$state, "not_raw_counts")
  expect_match(conditionMessage(err), "non enti")
  expect_match(conditionMessage(err), "allow_non_integer")

  # L'échappatoire doit être DÉCLARÉE explicitement (et reste l'ancienne règle)
  expect_invisible(bulk_assert_raw_counts(mixed, allow_non_integer = TRUE))

  # Une matrice franchement continue reste refusée même avec l'échappatoire
  expect_error(bulk_assert_raw_counts(log2(fx$counts + 1) + 0.123456,
                                      allow_non_integer = TRUE),
               class = "bulk_batch_correction_error")
})

# ── Contrôle du plan ───────────────────────────────────────────────────────
test_that("bulk_batch_correction_design flags feasibility and group usage", {
  fx <- .bc_fixture()
  meta_ok <- data.frame(row.names = paste0("s", 1:6),
                        batch = c("B1", "B1", "B1", "B2", "B2", "B2"),
                        cond  = c("A", "B", "A", "B", "A", "B"))
  d <- bulk_batch_correction_design(meta_ok, "batch", "cond")
  expect_true(d$can_apply)
  expect_true(d$use_group)
  expect_identical(d$n_batch_levels, 2L)
  expect_identical(d$min_samples_per_batch, 3L)
  expect_length(d$blocking_messages, 0L)
  expect_true("cross_table" %in% names(d$design_check))   # réutilisation, pas duplication
  expect_true("B1" %in% d$batch_levels && "B2" %in% d$batch_levels)

  # Sans condition : pas de group=, mais correction possible
  d_blind <- bulk_batch_correction_design(meta_ok, "batch")
  expect_true(d_blind$can_apply)
  expect_false(d_blind$use_group)

  # Lot unique -> blocage
  meta_one <- data.frame(row.names = paste0("s", 1:4), batch = rep("B1", 4))
  d_one <- bulk_batch_correction_design(meta_one, "batch")
  expect_false(d_one$can_apply)
  expect_match(paste(d_one$blocking_messages, collapse = " "), "un seul niveau")

  # Lot à 1 échantillon -> blocage (plancher = TS_BULK_BATCH_MIN_SAMPLES_PER_BATCH)
  meta_small <- data.frame(row.names = paste0("s", 1:6),
                           batch = c("B1", "B1", "B1", "B2", "B2", "B3"))
  d_small <- bulk_batch_correction_design(meta_small, "batch")
  expect_false(d_small$can_apply)
  expect_identical(d_small$min_samples_per_batch, 1L)
  expect_identical(d_small$min_samples_required, TS_BULK_BATCH_MIN_SAMPLES_PER_BATCH)

  # Collinéarité totale lot x condition -> blocage, et surtout PAS de group=
  meta_conf <- data.frame(row.names = paste0("s", 1:6),
                          batch = c("B1", "B1", "B1", "B2", "B2", "B2"),
                          cond  = c("A", "A", "A", "B", "B", "B"))
  d_conf <- bulk_batch_correction_design(meta_conf, "batch", "cond")
  expect_false(d_conf$can_apply)
  expect_false(d_conf$use_group)
  expect_match(paste(d_conf$blocking_messages, collapse = " "), "collinéaires")

  # Colonne inexistante -> erreur classée (déléguée au contrôle réutilisé)
  expect_error(bulk_batch_correction_design(meta_ok, "inexistant"),
               class = "bulk_batch_qc_error")
})

# ── Libellé de provenance ──────────────────────────────────────────────────
test_that("bulk_batch_correction_label preserves the previous normalization", {
  expect_identical(bulk_batch_correction_label("batch"), "ComBat-seq (batch : batch)")
  expect_identical(bulk_batch_correction_label("batch", "condition"),
                   "ComBat-seq (batch : batch ; groupe : condition)")
  # La normalisation déclarée est PRÉSERVÉE (préfixée), jamais écrasée.
  expect_identical(bulk_batch_correction_label("b", NULL, "Counts bruts"),
                   "Counts bruts + ComBat-seq (batch : b)")
  expect_identical(bulk_batch_correction_label("b", "c", "VST (DESeq2)"),
                   "VST (DESeq2) + ComBat-seq (batch : b ; groupe : c)")
  # NA / vide / NULL -> libellé nu
  expect_identical(bulk_batch_correction_label("b", NULL, NA), "ComBat-seq (batch : b)")
  expect_identical(bulk_batch_correction_label("b", "", ""), "ComBat-seq (batch : b)")
})

# ── Exécution réelle : critère d'acceptation STAT-S1 ───────────────────────
test_that("run_combat_seq mixes batches on PC1 and preserves the condition", {
  skip_if_not_installed("sva")
  fx <- .bc_fixture()

  r2_before  <- .bc_batch_pc1_r2(fx$counts, fx$batch)
  eff_before <- .bc_log2_batch_effect(fx$counts, fx$batch)
  gap_cond_before <- .bc_group_gap(fx$counts, fx$cond, fx$sig)

  corrected <- run_combat_seq(fx$counts, fx$batch, group = fx$cond)

  # Contrat de forme
  expect_true(is.matrix(corrected))
  expect_identical(dim(corrected), dim(fx$counts))
  expect_identical(dimnames(corrected), dimnames(fx$counts))
  expect_false(anyNA(corrected))
  # La correction a bien modifié les données (sinon on ne teste rien)
  expect_gt(max(abs(corrected - fx$counts)), 0)

  r2_after  <- .bc_batch_pc1_r2(corrected, fx$batch)
  eff_after <- .bc_log2_batch_effect(corrected, fx$batch)
  gap_cond_after <- .bc_group_gap(corrected, fx$cond, fx$sig)

  # 1) CRITÈRE DE LA ROADMAP : les lots sont mélangés sur PC1 après correction.
  expect_lt(r2_after, r2_before * 0.5)
  # 2) La signature lot mesurée par gène diminue aussi.
  expect_lt(eff_after, eff_before * 0.85)
  # 3) La condition est PRÉSERVÉE : le signal biologique n'est pas effacé.
  expect_gt(abs(gap_cond_after), abs(gap_cond_before) * 0.7)

  # Sans group=, l'appel reste valide et de même forme.
  blind <- run_combat_seq(fx$counts, fx$batch)
  expect_identical(dim(blind), dim(fx$counts))
})

# ── Limite connue, GELÉE : un décalage uniforme n'est pas un effet lot ──────
test_that("a uniform additive shift is a depth effect, not a batch effect", {
  skip_if_not_installed("sva")
  set.seed(1)
  n <- 300
  m <- matrix(rpois(n * 6, 100), n, 6,
              dimnames = list(paste0("g", 1:n), paste0("s", 1:6)))
  batch <- rep(c("B1", "B2"), each = 3)
  m[, 4:6] <- m[, 4:6] + 150   # décalage uniforme = profondeur de séquençage

  eff_before <- .bc_log2_batch_effect(m, batch)
  out        <- suppressMessages(run_combat_seq(m, batch))
  eff_after  <- .bc_log2_batch_effect(out, batch)

  # ComBat-seq NE corrige PAS un décalage uniforme : il l'absorbe par l'offset
  # de bibliothèque. C'est le comportement attendu — on le gèle pour que toute
  # évolution de sva qui changerait cette sémantique soit visible.
  expect_gt(eff_after, eff_before * 0.9)
})

test_that("run_combat_seq reports classed errors on bad input", {
  fx <- .bc_fixture()

  # Mauvais nombre de lots
  expect_identical(
    tryCatch(run_combat_seq(fx$counts, fx$batch[1:3]), error = function(e) e)$state,
    "invalid_input")
  # Un seul niveau de lot
  expect_identical(
    tryCatch(run_combat_seq(fx$counts, rep("B1", 6)), error = function(e) e)$state,
    "degenerate_batch")
  # Lot à 1 échantillon
  expect_identical(
    tryCatch(run_combat_seq(fx$counts, c("B1", "B1", "B1", "B2", "B2", "B3")),
             error = function(e) e)$state,
    "degenerate_batch")
  # Matrice transformée refusée AVANT d'appeler sva
  expect_identical(
    tryCatch(run_combat_seq(log2(fx$counts + 1) + 0.1, fx$batch), error = function(e) e)$state,
    "not_raw_counts")
  # Mauvais nombre de conditions
  expect_identical(
    tryCatch(run_combat_seq(fx$counts, fx$batch, group = fx$cond[1:2]),
             error = function(e) e)$state,
    "invalid_input")
  # Toutes les erreurs portent la classe du domaine
  err <- tryCatch(run_combat_seq(NULL, fx$batch), error = function(e) e)
  expect_s3_class(err, "bulk_batch_correction_error")
})

# ── Diagnostic avant / après ───────────────────────────────────────────────
test_that("plot_batch_correction_pca combines two PCAs and rejects NULL", {
  skip_if_not_installed("patchwork")
  p1 <- ggplot2::ggplot(data.frame(x = 1:6, y = 1:6), ggplot2::aes(x, y)) + ggplot2::geom_point()
  p2 <- ggplot2::ggplot(data.frame(x = 1:6, y = 6:1), ggplot2::aes(x, y)) + ggplot2::geom_point()

  combined <- plot_batch_correction_pca(p1, p2)
  expect_true(inherits(combined, "patchwork") || inherits(combined, "gg"))

  expect_error(plot_batch_correction_pca(p1, NULL),
               class = "bulk_batch_correction_error")
  expect_error(plot_batch_correction_pca(NULL, NULL),
               class = "bulk_batch_correction_error")
})

# ── Le fichier reste PUR (aucune réactivité Shiny) ─────────────────────────
test_that("tools/pkg_skeleton/R/batch_correction.R stays pure R", {
  src <- paste(readLines(file.path(ts_project_root(), "tools/pkg_skeleton/R/batch_correction.R")),
               collapse = "\n")
  expect_false(grepl("shiny::|reactive\\(|renderPlot|observeEvent\\(|showNotification\\(|moduleServer",
                     src))
  # Réutilise le contrôle de plan existant, ne le réimplémente pas.
  expect_match(src, "bulk_batch_design_check(", fixed = TRUE)
  # Import paresseux : sva n'est jamais chargé au source.
  expect_match(src, "requireNamespace(\"sva\"", fixed = TRUE)
})

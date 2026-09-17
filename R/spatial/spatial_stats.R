# =============================================================================
# R/utils_spatial_stats.R — Spatial statistics (Phase 6 / vague 4-5)
# =============================================================================
# CONFIRMED CANONICAL (vague 5): this is the file actually preloaded by
# init_spatial_daemons() (R/utils_spatial_async.R's source_files) and the
# one .verify_spatial_daemons() checks against (has_enrichment/has_hotspots/
# has_diffcomp/has_ripley). If your running app still reports "has_enrichment;
# has_ripley" missing, the daemon pool was started with an EARLIER/partial
# copy of this file -- click "Reinitialiser les daemons" (onglet Spatial >
# menu "Session") after replacing the file on disk; init_spatial_daemons()
# itself will NOT re-source an already-`daemons_ready` pool (idempotent by
# design), only a full reset respawns workers with the new file content.
#
# Four functions, two execution profiles:
#   - spatial_neighborhood_enrichment() [B1] and ripley_k_random_labeling()
#     [B6] run PERMUTATIONS (n_perm draws) -- genuinely async-worthy, called
#     from inside a mirai daemon (see mod_spatial_niche.R).
#   - compute_getis_ord_hotspots() [B4] and compute_composition_differential()
#     [B3] are cheap, closed-form (no permutation) -- called SYNCHRONOUSLY
#     from the main Shiny process, same convention as compute_qc_metrics_fast()
#     / mod_spatial_deconv.R's own colocalisation heatmap.
#
# All four reuse the SAME physical k-NN pattern already used by
# compute_spatial_niches()/BANKSY-lite (RANN::nn2 in (x,y) space) -- no new
# heavy dependency beyond RANN (already required, see global.R). spdep is
# NOT used (compute_getis_ord_hotspots() is a zero-dependency
# reimplementation of Getis-Ord Gi*, not a spdep::localG() wrapper).
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Spatial neighborhood enrichment / co-occurrence (squidpy nhood_enrichment-lite)
#'
#' Tests whether pairs of groups (clusters or dominant cell types) occur as
#' spatial neighbors more/less often than expected under label permutation
#' (topology fixed, node labels reshuffled) -- reveals spatial associations
#' between populations a per-spot clustering/deconvolution alone can't show.
#'
#' @param coords data.frame(id, x, y, ...).
#' @param group_labels Named character/factor vector (names = ids).
#' @param k_neighbors Integer, spatial k-NN size.
#' @param n_perm Integer, permutations for the null distribution.
#' @param log_file Optional path for write_mirai_log() progress lines.
#' @return list(enrichment = data.frame(from, to, observed, z_score),
#'   matrix = z-score matrix (levels x levels), levels =, k_neighbors =, n_perm =)
spatial_neighborhood_enrichment <- function(coords, group_labels, k_neighbors = 30,
                                            n_perm = 200, log_file = NULL) {
  .log <- function(msg, step, total) if (!is.null(log_file)) write_mirai_log(log_file, msg, step, total)
  if (!requireNamespace("RANN", quietly = TRUE)) stop(errorCondition("Package 'RANN' requis (install.packages('RANN')).", class = "spatial_stats_error"))
  
  .log("Alignement coordonnees / labels...", 1, 4)
  group_labels <- group_labels[!is.na(group_labels) & nzchar(as.character(group_labels))]
  common_ids <- intersect(coords$id, names(group_labels))
  if (length(common_ids) < 10) stop(errorCondition("Moins de 10 elements communs entre coordonnees et regroupement.", class = "spatial_stats_error"))
  cd  <- coords[match(common_ids, coords$id), c("id", "x", "y")]
  cd  <- cd[stats::complete.cases(cd[, c("x", "y")]), , drop = FALSE]
  grp <- as.character(group_labels[cd$id])
  lv  <- sort(unique(grp)); L <- length(lv)
  if (L < 2) stop(errorCondition("Le regroupement choisi n'a qu'une seule categorie.", class = "spatial_stats_error"))
  
  n <- nrow(cd)
  k_eff <- max(2, min(k_neighbors, n - 1))
  .log(sprintf("Voisinage spatial (k=%d, RANN)...", k_eff), 2, 4)
  nn <- RANN::nn2(as.matrix(cd[, c("x", "y")]), k = min(k_eff + 1, n))
  neighbor_idx <- nn$nn.idx[, -1, drop = FALSE]
  kk <- ncol(neighbor_idx)
  lab_int <- match(grp, lv)
  
  .log("Comptage des voisinages observes...", 3, 4)
  from_vec <- rep(lab_int, times = kk)
  to_vec   <- lab_int[as.vector(neighbor_idx)]
  combined <- (from_vec - 1L) * L + to_vec
  observed <- t(matrix(tabulate(combined, nbins = L * L), nrow = L, ncol = L))
  dimnames(observed) <- list(lv, lv)
  
  .log(sprintf("Permutations (n=%d)...", n_perm), 4, 4)
  perm_sum <- matrix(0, L, L); perm_sumsq <- matrix(0, L, L)
  for (p in seq_len(n_perm)) {
    lab_perm <- sample(lab_int)
    from_p <- rep(lab_perm, times = kk)
    to_p   <- lab_perm[as.vector(neighbor_idx)]
    mat_p  <- t(matrix(tabulate((from_p - 1L) * L + to_p, nbins = L * L), nrow = L, ncol = L))
    perm_sum <- perm_sum + mat_p; perm_sumsq <- perm_sumsq + mat_p^2
    if (!is.null(log_file) && p %% 40 == 0) write_mirai_log(log_file, sprintf("  ...permutation %d/%d", p, n_perm))
  }
  perm_mean <- perm_sum / n_perm
  perm_sd   <- sqrt(pmax(perm_sumsq / n_perm - perm_mean^2, 0))
  z <- (observed - perm_mean) / pmax(perm_sd, 1e-6)
  
  long <- expand.grid(from = lv, to = lv, stringsAsFactors = FALSE)
  idx  <- cbind(match(long$from, lv), match(long$to, lv))
  long$observed <- as.vector(observed[idx])
  long$z_score  <- as.vector(z[idx])
  
  list(enrichment = long, matrix = z, levels = lv, k_neighbors = k_eff, n_perm = n_perm)
}

#' Local Getis-Ord Gi* hotspot statistic (self-included, binary k-NN weights)
#'
#' Zero-dependency reimplementation (no 'spdep') -- for each point, compares
#' the sum of a numeric metric over its k nearest neighbors (+itself) to
#' what would be expected if values were randomly distributed. Standard
#' formula (Getis & Ord 1992/1995, ESRI "Hot Spot Analysis").
#'
#' @param coords data.frame(id, x, y, ...).
#' @param values Named numeric vector (names = ids matching coords$id).
#' @param k_neighbors Integer, spatial k-NN size (self is added automatically).
#' @return data.frame(id, value, gi_star, p_value, hotspot).
compute_getis_ord_hotspots <- function(coords, values, k_neighbors = 30) {
  if (!requireNamespace("RANN", quietly = TRUE)) stop(errorCondition("Package 'RANN' requis (install.packages('RANN')).", class = "spatial_stats_error"))
  common_ids <- intersect(coords$id, names(values))
  if (length(common_ids) < 10) stop(errorCondition("Moins de 10 elements communs entre coordonnees et valeurs.", class = "spatial_stats_error"))
  cd <- coords[match(common_ids, coords$id), c("id", "x", "y")]
  x  <- as.numeric(values[cd$id])
  keep <- stats::complete.cases(cd[, c("x", "y")]) & is.finite(x)
  cd <- cd[keep, , drop = FALSE]; x <- x[keep]
  n <- nrow(cd)
  if (n < 10) stop(errorCondition("Moins de 10 elements valides (coordonnees + valeur finie).", class = "spatial_stats_error"))
  
  k_eff <- max(1, min(k_neighbors, n - 1))
  nn <- RANN::nn2(as.matrix(cd[, c("x", "y")]), k = k_eff + 1)  # col 1 = self (distance 0)
  x_nbr <- matrix(x[nn$nn.idx], nrow = n, ncol = k_eff + 1)
  Wsum <- k_eff + 1
  weighted_sum <- rowSums(x_nbr)
  Xbar <- mean(x); S <- sqrt(mean(x^2) - Xbar^2)
  if (!is.finite(S) || S == 0) stop(errorCondition("Variance nulle pour cette metrique -- Getis-Ord non calculable.", class = "spatial_stats_error"))
  
  numerator <- weighted_sum - Xbar * Wsum
  denom <- S * sqrt((n * Wsum - Wsum^2) / (n - 1))
  gi_star <- numerator / denom
  p_value <- 2 * stats::pnorm(-abs(gi_star))
  hotspot <- ifelse(gi_star > 1.96 & p_value < 0.05, "Hotspot (chaud)",
                    ifelse(gi_star < -1.96 & p_value < 0.05, "Coldspot (froid)", "NS"))
  
  data.frame(id = cd$id, value = x, gi_star = gi_star, p_value = p_value,
             hotspot = hotspot, row.names = NULL, stringsAsFactors = FALSE)
}

# -----------------------------------------------------------------------------
# Composition differentielle inter-echantillons (B3)
# -----------------------------------------------------------------------------

#' Differential composition test across samples (clusters per dataset)
#'
#' Chi-carre d'independance (dataset x cluster) sur
#' global_data$spatial_multi_integration$embeddings (colonnes dataset,
#' cluster -- voir integrate_spatial_sketches()). Repli automatique sur
#' chisq.test(simulate.p.value=TRUE, B=2000) si un effectif attendu < 5
#' (frequent avec peu d'echantillons / beaucoup de clusters) plutot que de
#' faire confiance a un p asymptotique non fiable. Les residus standardises
#' par cellule (dataset,cluster) sont la sortie biologiquement actionnable --
#' le p global dit seulement "quelque chose differe".
#'
#' @param embeddings data.frame avec au moins les colonnes `dataset` et `cluster`.
#' @return list(contingency=, chisq=list(statistic=,p_value=,method=),
#'   residuals=data.frame(dataset,cluster,std_resid),
#'   proportions=data.frame(dataset,cluster,proportion,n)).
compute_composition_differential <- function(embeddings) {
  if (!all(c("dataset", "cluster") %in% colnames(embeddings))) {
    stop(errorCondition("embeddings doit contenir les colonnes 'dataset' et 'cluster'.", class = "spatial_stats_error"))
  }
  df <- embeddings[!is.na(embeddings$dataset) & !is.na(embeddings$cluster), ]
  if (length(unique(as.character(df$dataset))) < 2) {
    stop(errorCondition("Au moins 2 echantillons requis pour un test de composition differentielle.", class = "spatial_stats_error"))
  }
  
  tab <- table(dataset = df$dataset, cluster = df$cluster)
  test_res <- suppressWarnings(stats::chisq.test(tab))
  if (any(test_res$expected < 5)) {
    test_res  <- stats::chisq.test(tab, simulate.p.value = TRUE, B = 2000)
    method_lbl <- "Chi-carre (p simule, B=2000 -- effectifs attendus faibles)"
  } else {
    method_lbl <- "Chi-carre (asymptotique)"
  }
  
  resid_df <- as.data.frame(as.table(test_res$stdres))
  colnames(resid_df) <- c("dataset", "cluster", "std_resid")
  
  prop_df <- as.data.frame(as.table(prop.table(tab, margin = 1)))
  colnames(prop_df) <- c("dataset", "cluster", "proportion")
  n_df <- as.data.frame(as.table(tab)); colnames(n_df) <- c("dataset", "cluster", "n")
  prop_df$n <- n_df$n[match(paste(prop_df$dataset, prop_df$cluster), paste(n_df$dataset, n_df$cluster))]
  
  list(contingency = tab,
       chisq = list(statistic = unname(test_res$statistic), p_value = test_res$p.value, method = method_lbl),
       residuals = resid_df, proportions = prop_df)
}

# -----------------------------------------------------------------------------
# Ripley's K par etiquetage aleatoire (bonus statistique -- pattern spatial)
# -----------------------------------------------------------------------------

#' Ripley's K/L-style clustering test via random labeling (zero dependance)
#'
#' Teste si UN niveau choisi (cluster/niche/type cellulaire) est plus
#' AGREGE ou plus DISPERSE spatialement que le hasard, a plusieurs echelles
#' r. Hypothese nulle : etiquetage aleatoire -- on reshuffle QUI porte le
#' label cible parmi les MEMES positions observees (n_perm fois), plutot que
#' de modeliser explicitement une fenetre/frontiere de tissu (machinerie
#' d'edge-correction classique de spatstat, non pertinente ici puisque la
#' forme du tissu est arbitraire et qu'aucun contrat de "fenetre" n'existe
#' ailleurs dans cette app). Meme philosophie de permutation que
#' spatial_neighborhood_enrichment() ci-dessus.
#'
#' @param coords data.frame(id, x, y, ...).
#' @param group_labels Named character/factor vector (names = ids).
#' @param target_level Character, le SEUL niveau de group_labels a tester.
#' @param n_perm Integer, permutations pour l'enveloppe.
#' @param max_total,max_target Integer, plafonds RAM/vitesse (distances
#'   O(n^2)) -- sous-echantillonnage stratifie si depasses (garde jusqu'a
#'   max_target points cibles, complete avec du fond jusqu'a max_total).
#' @param log_file Optionnel, chemin pour write_mirai_log().
#' @return list(curve=data.frame(r,k_observed,k_perm_mean,k_perm_lo,
#'   k_perm_hi,signif), target_level=, n_target=, n_total=, n_perm=,
#'   subsampled=logical).
ripley_k_random_labeling <- function(coords, group_labels, target_level,
                                     n_perm = 199, max_total = 6000L,
                                     max_target = 2000L, log_file = NULL) {
  .log <- function(msg, step, total) if (!is.null(log_file)) write_mirai_log(log_file, msg, step, total)
  
  .log("Alignement coordonnees / labels...", 1, 4)
  group_labels <- group_labels[!is.na(group_labels) & nzchar(as.character(group_labels))]
  common_ids <- intersect(coords$id, names(group_labels))
  if (length(common_ids) < 20) stop(errorCondition("Moins de 20 elements communs entre coordonnees et regroupement.", class = "spatial_stats_error"))
  cd  <- coords[match(common_ids, coords$id), c("id", "x", "y")]
  cd  <- cd[stats::complete.cases(cd[, c("x", "y")]), , drop = FALSE]
  grp <- as.character(group_labels[cd$id])
  if (!target_level %in% grp) stop(errorCondition(sprintf("Niveau cible '%s' introuvable.", target_level), class = "spatial_stats_error"))
  
  is_target <- grp == target_level
  if (sum(is_target) < 10) stop(errorCondition("Moins de 10 elements dans le groupe cible -- test non fiable.", class = "spatial_stats_error"))
  
  subsampled <- FALSE
  if (length(is_target) > max_total || sum(is_target) > max_target) {
    subsampled <- TRUE
    set.seed(1)
    idx_t <- which(is_target); if (length(idx_t) > max_target) idx_t <- sample(idx_t, max_target)
    idx_bg <- which(!is_target); budget_bg <- max(0, max_total - length(idx_t))
    if (length(idx_bg) > budget_bg) idx_bg <- sample(idx_bg, budget_bg)
    keep <- sort(c(idx_t, idx_bg))
    cd <- cd[keep, , drop = FALSE]; is_target <- is_target[keep]
  }
  
  n_total <- nrow(cd); n_target <- sum(is_target)
  coords_mat <- as.matrix(cd[, c("x", "y")])
  scale_ref <- min(diff(range(cd$x)), diff(range(cd$y)))
  if (!is.finite(scale_ref) || scale_ref <= 0) stop(errorCondition("Etendue spatiale nulle -- test impossible.", class = "spatial_stats_error"))
  r_values <- seq(scale_ref * 0.02, scale_ref * 0.3, length.out = 8)
  
  compute_k <- function(is_t) {
    pts <- coords_mat[is_t, , drop = FALSE]; nt <- nrow(pts)
    if (nt < 2) return(rep(0, length(r_values)))
    d <- stats::dist(pts)
    vapply(r_values, function(r) 2 * sum(d <= r) / nt, numeric(1))
  }
  
  .log(sprintf("Calcul K observe (n_cible=%d / n_total=%d)...", n_target, n_total), 2, 4)
  k_obs <- compute_k(is_target)
  
  .log(sprintf("Permutations (n=%d, etiquetage aleatoire)...", n_perm), 3, 4)
  k_perm <- matrix(NA_real_, nrow = n_perm, ncol = length(r_values))
  for (p in seq_len(n_perm)) {
    k_perm[p, ] <- compute_k(sample(is_target))
    if (!is.null(log_file) && p %% 40 == 0) write_mirai_log(log_file, sprintf("  ...permutation %d/%d", p, n_perm))
  }
  
  .log("Termine.", 4, 4)
  k_mean <- colMeans(k_perm); k_lo <- apply(k_perm, 2, stats::quantile, probs = 0.025)
  k_hi <- apply(k_perm, 2, stats::quantile, probs = 0.975)
  curve <- data.frame(
    r = r_values, k_observed = k_obs, k_perm_mean = k_mean, k_perm_lo = k_lo, k_perm_hi = k_hi,
    signif = ifelse(k_obs > k_hi, "Agregation", ifelse(k_obs < k_lo, "Dispersion", "NS")),
    row.names = NULL
  )
  list(curve = curve, target_level = target_level, n_target = n_target,
       n_total = n_total, n_perm = n_perm, subsampled = subsampled)
}
# -----------------------------------------------------------------------------
# B5. Score d'interaction ligand-recepteur spatial (leger)
# -----------------------------------------------------------------------------

#' Spatial ligand-receptor interaction scoring (lightweight, local-first)
#'
#' Pour chaque paire L-R : score par spot = L(spot) * R_moyenne(voisinage) +
#' R(spot) * L_moyenne(voisinage), en reutilisant EXACTEMENT le meme trick
#' matrice-indicatrice creuse que BANKSY-lite (nbr_mat) et les niches --
#' PAS un modele statistique complet type CellPhoneDB/CellChat. Significativite
#' par permutation (meme logique que spatial_neighborhood_enrichment()) :
#' le graphe spatial (W) reste fixe, on permute QUEL spot porte quel profil
#' d'expression, n_perm fois.
#'
#' @param coords data.frame(id, x, y).
#' @param expr_mat Matrice genes x spots normalisee (dense ou sparse), ex.
#'   SeuratObject::LayerData(sketch, layer="data") -- SOUS-ECHANTILLONNEZ aux
#'   genes de lr_pairs AVANT l'appel pour rester leger.
#' @param lr_pairs data.frame(ligand, receptor) -- symboles de genes.
#' @param k_neighbors Integer, voisinage spatial k-NN (defaut 30).
#' @param n_perm Integer, permutations (0 = scores seuls, pas de p-value).
#' @param log_file Optionnel, chemin write_mirai_log().
#' @return list(pair_scores=data.frame(ligand,receptor,mean_score,z_score,
#'   p_value), spot_scores=data.frame(id, <top 20 paires>), skipped=data.frame(ligand,receptor,reason)).
spatial_lr_score <- function(coords, expr_mat, lr_pairs, k_neighbors = 30,
                             n_perm = 100, log_file = NULL) {
  .log <- function(msg, step, total) if (!is.null(log_file)) write_mirai_log(log_file, msg, step, total)
  if (!requireNamespace("RANN", quietly = TRUE)) stop(errorCondition("Package 'RANN' requis.", class = "spatial_stats_error"))
  
  .log("Alignement coordonnees / expression...", 1, 5)
  common_ids <- intersect(coords$id, colnames(expr_mat))
  if (length(common_ids) < 10) stop(errorCondition("Moins de 10 elements communs entre coordonnees et matrice d'expression.", class = "spatial_stats_error"))
  cd <- coords[match(common_ids, coords$id), c("id", "x", "y")]
  cd <- cd[stats::complete.cases(cd[, c("x", "y")]), , drop = FALSE]
  expr_mat <- expr_mat[, cd$id, drop = FALSE]
  n <- nrow(cd)
  
  avail <- rownames(expr_mat)
  ok <- lr_pairs$ligand %in% avail & lr_pairs$receptor %in% avail
  skipped <- data.frame(ligand = lr_pairs$ligand[!ok], receptor = lr_pairs$receptor[!ok],
                        reason = "gene absent de la matrice", stringsAsFactors = FALSE)
  lr_pairs <- lr_pairs[ok, , drop = FALSE]
  if (nrow(lr_pairs) == 0) stop(errorCondition("Aucune paire ligand-recepteur exploitable (genes absents).", class = "spatial_stats_error"))
  
  k_eff <- max(2, min(k_neighbors, n - 1))
  .log(sprintf("Graphe spatial (k=%d, RANN)...", k_eff), 2, 5)
  nn <- RANN::nn2(as.matrix(cd[, c("x", "y")]), k = min(k_eff + 1, n))
  neighbor_idx <- nn$nn.idx[, -1, drop = FALSE]
  kk <- ncol(neighbor_idx)
  W <- Matrix::sparseMatrix(i = rep(seq_len(n), each = kk), j = as.vector(t(neighbor_idx)),
                            x = 1 / kk, dims = c(n, n))
  
  genes_needed <- unique(c(lr_pairs$ligand, lr_pairs$receptor))
  own_mat <- t(as.matrix(expr_mat[genes_needed, cd$id, drop = FALSE]))
  nbr_mat <- as.matrix(W %*% own_mat); dimnames(nbr_mat) <- dimnames(own_mat)
  
  .log(sprintf("Score par paire (%d paires)...", nrow(lr_pairs)), 3, 5)
  score_mat <- vapply(seq_len(nrow(lr_pairs)), function(i) {
    L <- lr_pairs$ligand[i]; R <- lr_pairs$receptor[i]
    (own_mat[, L] * nbr_mat[, R] + own_mat[, R] * nbr_mat[, L]) / 2
  }, numeric(n))
  colnames(score_mat) <- paste(lr_pairs$ligand, lr_pairs$receptor, sep = "-")
  mean_score <- colMeans(score_mat)
  
  z_score <- rep(NA_real_, ncol(score_mat)); p_value <- rep(NA_real_, ncol(score_mat))
  if (n_perm > 0) {
    .log(sprintf("Permutations (n=%d) pour la significativite...", n_perm), 4, 5)
    perm_means <- matrix(NA_real_, nrow = n_perm, ncol = ncol(score_mat))
    for (p in seq_len(n_perm)) {
      perm_idx <- sample(n)
      own_p <- own_mat[perm_idx, , drop = FALSE]
      nbr_p <- as.matrix(W %*% own_p)
      perm_means[p, ] <- vapply(seq_len(nrow(lr_pairs)), function(i) {
        L <- lr_pairs$ligand[i]; R <- lr_pairs$receptor[i]
        mean((own_p[, L] * nbr_p[, R] + own_p[, R] * nbr_p[, L]) / 2)
      }, numeric(1))
    }
    pm <- colMeans(perm_means); psd <- apply(perm_means, 2, stats::sd)
    psd[psd == 0 | !is.finite(psd)] <- NA_real_
    z_score <- (mean_score - pm) / psd
    p_value <- 2 * stats::pnorm(-abs(z_score))
  }
  
  .log("Termine.", 5, 5)
  pair_df <- data.frame(ligand = lr_pairs$ligand, receptor = lr_pairs$receptor, mean_score = mean_score,
                        z_score = z_score, p_value = p_value, row.names = NULL, stringsAsFactors = FALSE)
  pair_df <- pair_df[order(-abs(ifelse(is.na(pair_df$z_score), pair_df$mean_score, pair_df$z_score))), ]
  top_n <- min(20, ncol(score_mat))
  top_cols <- match(utils::head(paste(pair_df$ligand, pair_df$receptor, sep = "-"), top_n), colnames(score_mat))
  spot_df <- data.frame(id = cd$id, score_mat[, top_cols, drop = FALSE], row.names = NULL, check.names = FALSE)
  
  list(pair_scores = pair_df, spot_scores = spot_df, skipped = skipped)
}
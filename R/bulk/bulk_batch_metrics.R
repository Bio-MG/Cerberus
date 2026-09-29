# =============================================================================
# R/bulk/bulk_batch_metrics.R — Métrique de mélange des batchs (STAT-S1)
# =============================================================================
# Pure domain logic (aucun symbole Shiny). Parité du domaine SC :
# R/sc/sc_batch_metrics.R (roadmap 4.3, audit 2026-09-27) transposé au niveau
# ÉCHANTILLON (handoff §4) — en bulk, les points mesurés sont les échantillons,
# pas les cellules.
#
# Score = 1 - pureté kNN moyenne des étiquettes de lot : chaque échantillon
# est projeté dans l'ACP de sa matrice (log1p, gènes les plus variables), et
# on mesure la fraction de ses k plus proches voisins partageant le même lot.
# 0 = lots parfaitement séparés (aucun mélange), 1 = mélange parfait.
#
# Usage : comparer le score AVANT et APRÈS ComBat-seq — une correction
# déclarée doit s'accompagner d'un GAIN de mélange. Le jugement « à l'œil »
# sur les PCA côte à côte est complété par un chiffre.
#
# Déterminisme total : aucun tirage aléatoire (sélection par variance,
# ACP et kNN sont déterministes) — pas de graine à déclarer ici.
# =============================================================================

#' Score de mélange des batchs (niveau échantillon)
#'
#' @param mat Matrice numérique (gènes x échantillons) — counts (bruts ou
#'   corrigés ComBat-seq) ; log1p est appliqué en interne, symétriquement
#'   avant et après correction, pour que la comparaison reste équitable.
#' @param batch Vecteur de lots, longueur = ncol(mat).
#' @param n_pcs Nombre de composantes principales utilisées (plafonné).
#' @param k Nombre de voisins évalués par échantillon.
#' @param max_genes Plafond de gènes entrant dans l'ACP (les plus variables).
#' @return list(score = [0,1] (1 = mélange parfait),
#'              knn_same_batch_purity, per_batch (named numeric),
#'              k, n_samples_used, n_pcs, n_genes_used).
bulk_batch_mixing_score <- function(mat, batch, n_pcs = 10L, k = 3L,
                                    max_genes = 2000L) {
  .stop <- function(msg) {
    errorCondition(msg, class = "bulk_batch_metrics_error")
  }
  if (is.null(mat) || !is.matrix(mat) || !is.numeric(mat)) {
    stop(.stop(sprintf(
      "Échec métrique de mélange : une matrice numérique est requise (reçu : %s).",
      if (is.null(mat)) "NULL" else paste(class(mat), collapse = "/"))))
  }
  if (anyNA(mat)) {
    stop(.stop("Échec métrique de mélange : la matrice contient des valeurs manquantes (NA)."))
  }
  if (any(mat < 0)) {
    stop(.stop("Échec métrique de mélange : la matrice contient des valeurs négatives — des counts sont attendus (le log1p interne les rendrait indéfinis). Une matrice VST / normalisée n'a pas de sens ici."))
  }
  if (ncol(mat) < 3L) {
    stop(.stop("Échec métrique de mélange : au moins 3 échantillons requis."))
  }
  batch <- as.character(batch)
  if (length(batch) != ncol(mat)) {
    stop(.stop(sprintf(
      "Échec métrique de mélange : longueur du vecteur de lots (%d) différente du nombre d'échantillons (%d).",
      length(batch), ncol(mat))))
  }
  keep_s <- !is.na(batch) & nzchar(batch)
  if (length(unique(batch[keep_s])) < 2L) {
    stop(.stop("Échec métrique de mélange : moins de 2 lots — le score n'est pas défini."))
  }
  mat <- mat[, keep_s, drop = FALSE]
  batch <- batch[keep_s]

  # Gènes les plus variables (déterministe) — l'ACP sur tous les gènes serait
  # dominée par le bruit ; le plafond borne aussi le coût de SVD.
  if (nrow(mat) > max_genes) {
    vars <- matrixStats::rowVars(mat)
    top <- order(vars, decreasing = TRUE, method = "radix")[seq_len(max_genes)]
    mat <- mat[top, , drop = FALSE]
  }
  if (nrow(mat) < 2L) {
    stop(.stop("Échec métrique de mélange : moins de 2 gènes exploitables."))
  }

  # Échantillons = observations, gènes = variables ; log1p symétrique avant/après.
  pca <- stats::prcomp(t(log1p(mat)), center = TRUE, scale. = TRUE)
  n_dims <- min(n_pcs, ncol(pca$x))
  if (n_dims < 2L) {
    stop(.stop("Échec métrique de mélange : moins de 2 composantes disponibles."))
  }
  emb <- pca$x[, seq_len(n_dims), drop = FALSE]

  k_eff <- min(k, nrow(emb) - 1L)
  if (k_eff < 1L) {
    stop(.stop("Échec métrique de mélange : pas assez d'échantillons pour évaluer les k voisins."))
  }
  dm <- as.matrix(stats::dist(emb))
  diag(dm) <- Inf
  same_frac <- vapply(seq_len(nrow(emb)), function(i) {
    o <- order(dm[i, ], na.last = NA)[seq_len(k_eff)]
    mean(batch[o] == batch[i])
  }, numeric(1))

  purity <- mean(same_frac)
  per_batch <- vapply(sort(unique(batch)), function(b) {
    as.numeric(mean(same_frac[batch == b]))
  }, numeric(1))
  list(
    score                 = round(1 - purity, 4),
    knn_same_batch_purity = round(purity, 4),
    per_batch             = per_batch,
    k                     = k_eff,
    n_samples_used        = length(batch),
    n_pcs                 = n_dims,
    n_genes_used          = nrow(mat)
  )
}

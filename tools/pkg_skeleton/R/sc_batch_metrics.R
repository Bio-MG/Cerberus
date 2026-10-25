# =============================================================================
# R/sc/sc_batch_metrics.R — Métrique de mélange des batchs (roadmap 4.3)
# =============================================================================
# Pure domain logic (aucun symbole Shiny).
#
# Score = 1 - pureté kNN moyenne des étiquettes de batch : pour chaque
# cellule (sous-échantillon déterministe pour la RAM), fraction de ses k
# plus proches voisins (dans l'espace de l'embedding) partageant le même
# batch. 0 = batchs parfaitement séparés (aucun mélange), 1 = mélange
# parfait. Approximation d'iLISI sans dépendance additionnelle (stats::dist).
#
# Usage : comparer le score AVANT (pca) et APRÈS (harmony) la correction —
# une correction déclarée doit s'accompagner d'un GAIN de mélange. Le jugement
# « à l'œil » sur l'UMAP est remplacé par un chiffre journalisé.
# =============================================================================

#' Score de mélange des batchs sur une réduction
#'
#' @param obj Objet Seurat portant la réduction et la colonne de batch.
#' @param reduction Nom de la réduction à évaluer ("pca", "harmony"...).
#' @param batch_col Colonne de métadonnées définissant les batchs.
#' @param dims Nombre de dimensions de l'embedding utilisées (défaut 30, plafonné).
#' @param max_cells Plafond de sous-échantillonnage (matrice de distances k·k).
#' @param k Nombre de voisins évalués par cellule.
#' @param seed Graine du sous-échantillonnage (déterminisme déclaré).
#' @return list(score = [0,1] (1 = mélange parfait),
#'              knn_same_batch_purity, per_batch (named numeric),
#'              k, n_cells_used, reduction, batch_col, seed).
sc_batch_mixing_score <- function(obj, reduction = "pca",
                                  batch_col = "orig.ident",
                                  dims = NULL, max_cells = 2000L,
                                  k = 30L, seed = 989L) {
  .stop <- function(msg) {
    errorCondition(msg, class = "sc_batch_metrics_error")
  }
  if (!inherits(obj, "Seurat")) stop(.stop("Un objet Seurat est requis."))
  if (!reduction %in% names(obj@reductions)) {
    stop(.stop(sprintf("Réduction « %s » absente de l'objet.", reduction)))
  }
  if (!batch_col %in% colnames(obj@meta.data)) {
    stop(.stop(sprintf("Colonne de batch « %s » absente des métadonnées.", batch_col)))
  }
  emb <- Seurat::Embeddings(obj, reduction = reduction)
  n_dims <- if (is.null(dims)) min(ncol(emb), 30L) else min(dims, ncol(emb))
  if (n_dims < 2L) stop(.stop("Moins de 2 dimensions disponibles sur la réduction."))
  batches <- as.character(obj@meta.data[[batch_col]])
  if (length(unique(batches[!is.na(batches) & nzchar(batches)])) < 2L) {
    stop(.stop("Moins de 2 batchs — le score de mélange n'est pas défini."))
  }

  # Sous-échantillon déterministe (withr restaure le RNG).
  idx <- seq_len(nrow(emb))
  if (length(idx) > max_cells) {
    idx <- sort(withr::with_seed(seed, sample(idx, max_cells)))
  }
  emb_s <- emb[idx, seq_len(n_dims), drop = FALSE]
  bat_s <- batches[idx]
  keep <- !is.na(bat_s) & nzchar(bat_s)
  emb_s <- emb_s[keep, , drop = FALSE]
  bat_s <- bat_s[keep]
  k_eff <- min(k, nrow(emb_s) - 1L)
  if (k_eff < 1L) stop(.stop("Pas assez de cellules pour évaluer les k voisins."))

  dm <- as.matrix(stats::dist(emb_s))
  diag(dm) <- Inf
  same_frac <- vapply(seq_len(nrow(emb_s)), function(i) {
    o <- order(dm[i, ], na.last = NA)[seq_len(k_eff)]
    mean(bat_s[o] == bat_s[i])
  }, numeric(1))

  purity <- mean(same_frac)
  per_batch <- vapply(sort(unique(bat_s)), function(b) {
    as.numeric(mean(same_frac[bat_s == b]))
  }, numeric(1))
  list(
    score                 = round(1 - purity, 4),
    knn_same_batch_purity = round(purity, 4),
    per_batch             = per_batch,
    k                     = k_eff,
    n_cells_used          = length(bat_s),
    reduction             = reduction,
    batch_col             = batch_col,
    seed                  = seed
  )
}

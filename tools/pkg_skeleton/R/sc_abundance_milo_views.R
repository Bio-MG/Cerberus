# =============================================================================
# R/sc/sc_abundance_milo_views.R — Abondance differentielle 4E-1 : vues Milo
# (Stage 14)
# =============================================================================
# Consommateurs PURES du resultat canonique (assert_milo_result) :
# signal DA sur embedding, distribution des statistiques par voisinage,
# contexte de composition au niveau ECHANTILLON.
#
# REGLES (Stage 14) :
#   - une vue ne cree, n'infere et ne repare JAMAIS un resultat ; elle lit
#     l'objet canonique et n'ecrit rien dedans ;
#   - le signal DA est NIVEAU VOISINAGE : le mappage cellule (mean_logFC des
#     voisinages contenant la cellule) est un AFFICHAGE descriptif — les
#     cellules hors voisinage sont en gris, et la legende le dit explicitement ;
#   - le contraste teste ET la methode de correction (SpatialFDR) sont
#     affiches sur CHAQUE figure (sous-titre) ;
#   - l'identite d'un voisinage (fraction >= TS_DA_MILO_IDENTITY_FRACTION_MIN)
#     est descriptive : un voisinage mixte (NA) n'est jamais reaffecte ;
#   - la composition echantillon reste au niveau ECHANTILLON — les cellules
#     ne sont PAS des replicats biologiques (aucun test par cellule).
#
# Sourced in app.R AFTER R/sc/sc_abundance_milo.R. Pure ggplot2.
#
# ── API PUBLIQUE FIGEE (Stage 14) ───────────────────────────────────────────
# milo_views_public_api() enumere la surface ; le test de freeze
# (tests/testthat/test-milo-contract-freeze.R) la verifie. Helpers internes :
#   .milo_views_stop()       — erreur classee milo_error
#   .milo_empty_view_plot()  — message explicite au lieu d'un graphe vide
#   .milo_view_subtitle()    — sous-titre commun (contraste + FDR + niveau)
# =============================================================================

#' Surface publique figee de R/sc/sc_abundance_milo_views.R (Stage 14)
#'
#' @return Vecteur character des noms de fonctions publiques.
#' @export
milo_views_public_api <- function() {
  c(
    "milo_views_public_api",
    "plot_milo_da_embedding",
    "plot_milo_da_distribution",
    "plot_milo_sample_composition"
  )
}

#' Erreur d'une vue Milo (classe du domaine, etat structure)
.milo_views_stop <- function(message) {
  stop(errorCondition(
    message,
    state = "invalid_input",
    class = "milo_error"
  ))
}

#' Message explicite au lieu d'un graphe vide
.milo_empty_view_plot <- function(message) {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text", x = 0.5, y = 0.5, label = message, size = 4.2
    ) +
    ggplot2::theme_void()
}

#' Sous-titre commun : contraste + methode de correction + niveau du signal
.milo_view_subtitle <- function(milo_result) {
  tc <- milo_result$tested_contrast %||% list()
  p <- milo_result$parameters %||% list()
  paste0(
    "Contraste : ", tc$contrast %||% NA_character_,
    " (", tc$interpretation %||% "logFC > 0 = enrichi dans la cible.", ")",
    " — correction : SpatialFDR (", p$fdr_weighting %||% "k-distance", ")",
    " — signal PAR VOISINAGE, pas par type cellulaire"
  )
}

#' Signal DA des voisinages projete sur l'embedding (affichage descriptif)
#'
#' Chaque cellule est coloree par la moyenne des logFC des voisinages qui la
#' contiennent (resultat$nhood_assignment) ; les cellules hors voisinage sont
#' en gris (NA). Le score est DEScriptif — aucun test par cellule n'est
#' effectue ici ni ailleurs.
#'
#' @param milo_result Resultat canonique Milo (assert_milo_result).
#' @param seurat_obj Objet Seurat courant (fournit les coordonnees).
#' @param reduction Reduction dont les 2 premieres dimensions sont affichees.
#' @param alpha Seuil SpatialFDR cite en sous-titre (TS_DA_MILO_DISPLAY_ALPHA).
#' @param pt_size Taille des points.
#' @param palette Palette divergente (nom, voir palettes.R) ou NULL.
#' @return ggplot.
#' @export
plot_milo_da_embedding <- function(milo_result, seurat_obj, reduction,
                                   alpha = TS_DA_MILO_DISPLAY_ALPHA,
                                   pt_size = 1.1) {
  r <- assert_milo_result(milo_result, seurat_obj = seurat_obj,
                          context = "vue embedding Milo")
  assert_reduction(seurat_obj, reduction, context = "vue embedding Milo")
  na_tab <- r$nhood_assignment %||% NULL
  if (is.null(na_tab) || !is.data.frame(na_tab) || nrow(na_tab) == 0L) {
    return(.milo_empty_view_plot(
      "Aucun mappage cellule -> voisinages disponible : relancez l'analyse Milo."
    ))
  }
  emb <- SeuratObject::Embeddings(seurat_obj, reduction)[, 1:2, drop = FALSE]
  df <- data.frame(
    emb[na_tab$cell_id, , drop = FALSE],
    mean_logFC = na_tab$mean_logFC,
    in_nhood = na_tab$n_neighbourhoods > 0L,
    stringsAsFactors = FALSE
  )
  lim <- max(abs(df$mean_logFC), na.rm = TRUE)
  if (!is.finite(lim) || lim == 0) lim <- 1
  ggplot2::ggplot(df, ggplot2::aes(x = .data[[colnames(df)[1]]],
                                   y = .data[[colnames(df)[2]]],
                                   color = mean_logFC)) +
    ggplot2::geom_point(size = pt_size, alpha = 0.9) +
    ggplot2::scale_color_gradient2(
      low = "#2166AC", mid = "white", high = "#B2182B",
      midpoint = 0, limits = c(-lim, lim), na.value = "grey65",
      name = "logFC moyen\n(voisinages)"
    ) +
    ggplot2::labs(
      title = "Milo — signal d'abundance differentielle sur l'embedding",
      subtitle = .milo_view_subtitle(r),
      caption = paste0(
        "Gris : cellules hors voisinage (descriptif, aucun test par cellule). ",
        "Voisinages significatifs : SpatialFDR < ", alpha, "."
      ),
      x = paste0(reduction, "_1"), y = paste0(reduction, "_2")
    ) +
    ggplot2::theme_classic()
}

#' Distribution des logFC par voisinage (Stage 14)
#'
#' Histogramme des logFC des voisinages ; les voisinages significatifs
#' (SpatialFDR < alpha) sont distingues. Les voisinages sans p-value valide
#' sont exclus du graphe et COMPTEES (jamais imputes).
#'
#' @param milo_result Resultat canonique Milo (assert_milo_result).
#' @param alpha Seuil SpatialFDR (TS_DA_MILO_DISPLAY_ALPHA).
#' @param bins Nombre de classes de l'histogramme.
#' @return ggplot.
#' @export
plot_milo_da_distribution <- function(milo_result,
                                      alpha = TS_DA_MILO_DISPLAY_ALPHA,
                                      bins = 30) {
  r <- assert_milo_result(milo_result, context = "vue distribution Milo")
  da <- r$DA_table %||% NULL
  if (is.null(da) || !is.data.frame(da) || nrow(da) == 0L) {
    return(.milo_empty_view_plot(
      "Aucun voisinage testable dans le resultat Milo."
    ))
  }
  n_na <- sum(!is.finite(da$PValue) | is.na(da$PValue))
  da_ok <- da[is.finite(da$PValue) & !is.na(da$PValue), , drop = FALSE]
  if (nrow(da_ok) == 0L) {
    return(.milo_empty_view_plot(sprintf(
      "Aucun voisinage avec une p-value valide (%d voisinage(s) exclus, jamais imputes).",
      n_na
    )))
  }
  da_ok$sig <- da_ok$SpatialFDR < alpha
  ggplot2::ggplot(da_ok, ggplot2::aes(x = logFC, fill = sig)) +
    ggplot2::geom_histogram(bins = bins, color = "grey25", linewidth = 0.2) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
    ggplot2::scale_fill_manual(
      values = c(`FALSE` = "grey60", `TRUE` = "#B2182B"),
      labels = c(`FALSE` = paste0("SpatialFDR >= ", alpha),
                 `TRUE` = paste0("SpatialFDR < ", alpha)),
      name = NULL
    ) +
    ggplot2::labs(
      title = "Milo — distribution des logFC par voisinage",
      subtitle = .milo_view_subtitle(r),
      caption = sprintf(
        paste0("%d voisinage(s) sans p-value valide exclus du graphe ",
               "(jamais imputes)."),
        n_na
      ),
      x = "logFC (cible - reference)", y = "Nombre de voisinages"
    ) +
    ggplot2::theme_classic()
}

#' Contexte de composition au niveau ECHANTILLON (Stage 14)
#'
#' Barres empilees par echantillon : effectif total et effectif couvert par
#' les voisinages, colorees par condition. Contexte de lecture du DA — les
#' cellules ne sont PAS des replicats biologiques.
#'
#' @param milo_result Resultat canonique Milo (assert_milo_result).
#' @return ggplot.
#' @export
plot_milo_sample_composition <- function(milo_result) {
  r <- assert_milo_result(milo_result, context = "vue composition echantillon Milo")
  sc <- r$sample_composition %||% NULL
  if (is.null(sc) || !is.data.frame(sc) || nrow(sc) == 0L) {
    return(.milo_empty_view_plot(
      "Aucun contexte echantillon disponible dans le resultat Milo."
    ))
  }
  # Format long : total vs couvert par voisinage (deux segments par barre).
  df <- data.frame(
    sample = rep(sc$sample, 2),
    condition = rep(sc$condition, 2),
    coverage = rep(c("hors voisinage", "dans un voisinage"), each = nrow(sc)),
    n_cells = c(sc$n_cells_total - sc$n_cells_in_nhoods, sc$n_cells_in_nhoods),
    stringsAsFactors = FALSE
  )
  df <- df[df$n_cells > 0L, , drop = FALSE]
  if (nrow(df) == 0L) {
    return(.milo_empty_view_plot("Aucune cellule a afficher."))
  }
  ggplot2::ggplot(df, ggplot2::aes(x = sample, y = n_cells, fill = condition,
                                   alpha = coverage)) +
    ggplot2::geom_col(color = "grey25", linewidth = 0.2) +
    ggplot2::scale_alpha_manual(
      values = c("dans un voisinage" = 1, "hors voisinage" = 0.35), name = NULL
    ) +
    ggplot2::labs(
      title = "Milo — contexte de composition par echantillon",
      subtitle = .milo_view_subtitle(r),
      caption = paste0(
        "Niveau ECHANTILLON (unite statistique) — les cellules ne sont pas ",
        "des replicats biologiques. Opacite : couverture par les voisinages."
      ),
      x = NULL, y = "Nombre de cellules"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

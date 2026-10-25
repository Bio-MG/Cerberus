# =============================================================================
# R/sc/sc_abundance_sccoda_views.R — Abondance differentielle 4E-2 : vues
# scCODA (Stage 15)
# =============================================================================
# Consommatrices PURES du resultat canonique (assert_sccoda_result) :
# composition par echantillon, effets credibles (cible vs reference),
# incertitude (largeur des HDI).
#
# REGLES (Stage 15) :
#   - une vue ne cree, n'infere et ne repare JAMAIS un resultat ;
#   - l'unite est l'ECHANTILLON — les cellules ne sont PAS des replicats ;
#   - les effets sont des INTERVALLES DE CREDIBILITE bayesiens (prior
#     spike-and-slab) — PAS des p-values ; "credible" = approche de
#     probabilite posterieure directe au seuil fdr_target enregistre ;
#   - la reference (identite et condition de base) est affichee sur chaque
#     figure : un effet se lit TOUJOURS relativement a la reference declaree ;
#   - les effets de batch sont secondaires (contexte), jamais la lecture
#     principale.
#
# Sourced in app.R AFTER R/sc/sc_abundance_sccoda.R. Pure ggplot2.
#
# ── API PUBLIQUE FIGEE (Stage 15) ───────────────────────────────────────────
# sccoda_views_public_api() ; le test de freeze la verifie. Helpers internes :
#   .sccoda_views_stop()      — erreur classee sccoda_error
#   .sccoda_empty_view_plot() — message explicite au lieu d'un graphe vide
# =============================================================================

#' Surface publique figee de R/sc/sc_abundance_sccoda_views.R (Stage 15)
#'
#' @return Vecteur character des noms de fonctions publiques.
#' @export
sccoda_views_public_api <- function() {
  c(
    "sccoda_views_public_api",
    "plot_sccoda_composition",
    "plot_sccoda_effects",
    "plot_sccoda_uncertainty"
  )
}

#' Erreur d'une vue scCODA (classe du domaine, etat structure)
.sccoda_views_stop <- function(message) {
  stop(errorCondition(
    message,
    state = "invalid_input",
    class = "sccoda_error"
  ))
}

#' Message explicite au lieu d'un graphe vide
.sccoda_empty_view_plot <- function(message) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = message, size = 4.2) +
    ggplot2::theme_void()
}

#' Composition par echantillon (niveau ECHANTILLON — jamais par cellule)
#'
#' Barres empilees des comptages par identite, colorees par condition.
#'
#' @param sccoda_result Resultat canonique (assert_sccoda_result).
#' @return ggplot.
#' @export
plot_sccoda_composition <- function(sccoda_result) {
  r <- assert_sccoda_result(sccoda_result,
                            context = "vue composition scCODA")
  ct <- r$composition_table %||% NULL
  if (is.null(ct) || !is.data.frame(ct) || nrow(ct) == 0L) {
    return(.sccoda_empty_view_plot(
      "Aucune matrice de composition disponible dans le resultat scCODA."
    ))
  }
  ident_cols <- setdiff(colnames(ct), c("sample", "condition", "batch"))
  long <- stats::reshape(
    ct[, c("sample", "condition", ident_cols), drop = FALSE],
    direction = "long", varying = ident_cols,
    times = ident_cols, timevar = "identity", idvar = "sample_id",
    v.names = "n_cells"
  )
  long$condition <- as.character(long$condition)
  ggplot2::ggplot(long, ggplot2::aes(x = sample, y = n_cells, fill = identity)) +
    ggplot2::geom_col(color = "grey25", linewidth = 0.2) +
    ggplot2::facet_wrap(~condition, scales = "free_x") +
    ggplot2::labs(
      title = "scCODA — composition cellulaire par echantillon",
      subtitle = sprintf(
        paste0("Unite de composition : ECHANTILLON (les cellules ne sont pas ",
               "des replicats). Reference d'identite : %s — les effets se ",
               "lisent relativement a cette reference."),
        r$reference_identity %||% NA_character_
      ),
      caption = sprintf(
        "Matrice analysee par le modele (formula : %s).",
        r$model_specification$formula %||% NA_character_
      ),
      x = NULL, y = "Nombre de cellules", fill = "Identite"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' Effets condition cible vs reference (intervalles de credibilite)
#'
#' Effet posterieur (prior spike-and-slab) par identite pour la covariable
#' condition, avec HDI ; les effets credibles (seuil fdr_target) sont
#' distingues. logFC > 0 = enrichi dans la cible (apres reorientation
#' explicite du resultat).
#'
#' @param sccoda_result Resultat canonique (assert_sccoda_result).
#' @param include_batch Inclure les lignes de la covariable batch (contexte
#'   secondaire, style distinct).
#' @return ggplot.
#' @export
plot_sccoda_effects <- function(sccoda_result, include_batch = FALSE) {
  r <- assert_sccoda_result(sccoda_result, context = "vue effets scCODA")
  et <- r$effect_table %||% NULL
  if (is.null(et) || !is.data.frame(et) || nrow(et) == 0L) {
    return(.sccoda_empty_view_plot(
      "Aucune table d'effets disponible dans le resultat scCODA."
    ))
  }
  et$condition_cov <- grepl("^condition\\[", et$covariate)
  et$batch_cov <- grepl("^batch\\[", et$covariate)
  shown <- if (isTRUE(include_batch)) et else et[et$condition_cov, , drop = FALSE]
  if (nrow(shown) == 0L) {
    return(.sccoda_empty_view_plot(
      "Aucun effet de condition dans le resultat scCODA."
    ))
  }
  shown <- shown[order(shown$credible, shown$effect), , drop = FALSE]
  shown$identity <- factor(shown$identity, levels = unique(shown$identity))
  p <- ggplot2::ggplot(shown, ggplot2::aes(x = effect, y = identity)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = hdi_low, xmax = hdi_high),
                           orientation = "y", width = 0.22, color = "grey35",
                           linewidth = 0.5) +
    ggplot2::geom_point(ggplot2::aes(color = credible), size = 3) +
    ggplot2::scale_color_manual(
      values = c(`FALSE` = "grey55", `TRUE` = "#B2182B"),
      labels = c(`FALSE` = paste0("non credible (fdr ",
                                  r$model_specification$fdr_target %||% 0.05, ")"),
                 `TRUE` = "credible"),
      name = NULL
    ) +
    ggplot2::labs(
      title = "scCODA — effets d'abondance compositionnelle",
      subtitle = sprintf(
        paste0("Condition cible '%s' vs reference '%s' (effet > 0 = enrichi ",
               "dans la cible). Reference d'identite : %s. Intervalles de ",
               "CREDIBILITE bayesiens — pas des p-values."),
        r$parameters$target_condition %||% NA_character_,
        r$parameters$reference_condition %||% NA_character_,
        r$reference_identity %||% NA_character_
      ),
      caption = sprintf(
        paste0("Seuil FDR : %s ; MCMC HMC %d resultats / %d burnin ; ",
               "graine %s. \"Credible\" = probabilite posterieure directe, ",
               "PAS un test de significativite."),
        r$model_specification$fdr_target %||% NA_character_,
        r$model_specification$num_results %||% NA_integer_,
        r$model_specification$num_burnin %||% NA_integer_,
        r$model_specification$seed %||% NA_character_
      ),
      x = "Effet (cible - reference)", y = NULL
    ) +
    ggplot2::theme_classic()
  if (isTRUE(include_batch)) {
    p <- p + ggplot2::facet_grid(batch_cov ~ ., scales = "free_y",
                                 space = "free_y",
                                 labeller = ggplot2::labeller(
                                   batch_cov = c(`TRUE` = "batch (contexte)",
                                                 `FALSE` = "condition"))
    )
  }
  p
}

#' Incertitude des effets (largeur des HDI par identite)
#'
#' Visualisation d'incertitude : demi-largeur des intervalles de credibilite
#' pour la covariable condition — plus l'intervalle est etroit, plus
#' l'effet posterieur est contraint.
#'
#' @param sccoda_result Resultat canonique (assert_sccoda_result).
#' @return ggplot.
#' @export
plot_sccoda_uncertainty <- function(sccoda_result) {
  r <- assert_sccoda_result(sccoda_result, context = "vue incertitude scCODA")
  et <- r$effect_table %||% NULL
  if (is.null(et) || !is.data.frame(et) || nrow(et) == 0L) {
    return(.sccoda_empty_view_plot(
      "Aucune table d'effets disponible dans le resultat scCODA."
    ))
  }
  cond <- et[grepl("^condition\\[", et$covariate), , drop = FALSE]
  if (nrow(cond) == 0L) {
    return(.sccoda_empty_view_plot(
      "Aucun effet de condition dans le resultat scCODA."
    ))
  }
  cond$hdi_width <- cond$hdi_high - cond$hdi_low
  cond$identity <- factor(cond$identity,
                          levels = cond$identity[order(cond$hdi_width)])
  ggplot2::ggplot(cond, ggplot2::aes(x = hdi_width, y = identity,
                                     fill = credible)) +
    ggplot2::geom_col(color = "grey25", linewidth = 0.2) +
    ggplot2::scale_fill_manual(
      values = c(`FALSE` = "grey60", `TRUE` = "#B2182B"),
      labels = c(`FALSE` = "non credible", `TRUE` = "credible"),
      name = NULL
    ) +
    ggplot2::labs(
      title = "scCODA — incertitude des effets (largeur des HDI)",
      subtitle = sprintf(
        paste0("Demi-largeur de l'intervalle de credibilite pour '%s' vs ",
               "'%s' — plus la barre est courte, plus l'effet est contraint."),
        r$parameters$target_condition %||% NA_character_,
        r$parameters$reference_condition %||% NA_character_
      ),
      caption = "Incertitude posterieure descriptive — aucune inference par cellule.",
      x = "Largeur de l'intervalle de credibilite (HDI)", y = NULL
    ) +
    ggplot2::theme_classic()
}

# =============================================================================
# R/bulk/dose_response.R — Dose–réponse / time-course (NEW-1, MVP drc)
# =============================================================================
# Fiche ROADMAP_presentation_stats.md §6 (NEW-1), re-mesurée le 2026-09-13 :
# `shared_rv$vst_mat` + métadonnées numériques (dose ou temps déclarés) +
# listes DE (même pattern que mod_bulk_pathways / mod_bulk_pattern).
#
# DÉPENDANCE JUSTIFIÉE (amendement 2026-09-13) : `drc` 3.0-1 (CRAN, pur R,
# léger) — ajustement Hill/log-logistique/Weibull par gène, EC50/pente,
# courbes ajustées + IC. `renv.lock` mis à jour au même commit (6 entrées :
# drc + multcomp, mvtnorm, plotrix, sandwich, TH.data). Aucune régression :
# ajout pur, aucun appel existant modifié.
#
# PORTÉE v1 : sous-ensemble de gènes (top N DE), JAMAIS un balayage exhaustif
# (fiche NEW-1). Le modèle est CHOISI (LL.4 par défaut) ; les doses/temps
# doivent être STRICTEMENT POSITIFS (tous les modèles de drc 4-paramètres
# utilisent log(dose)) — limité et documenté au contrat §7.
#
# Contrat gelé : docs/contracts/BULK_DOSE_RESPONSE_CONTRACT.md (+ freeze test).
# Erreurs classées `bulk_dose_error` (français, errorCondition, state).
# =============================================================================

#' Modèles de dose-réponse supportés (fonctions drc 4 paramètres)
#' @export
bulk_dose_models <- function() {
  c("LL.4", "W1.4", "W2.4", "BC.4")
}

#' Champs contractuels du résultat dose-réponse
#' @export
bulk_dose_contract_fields <- function() {
  c("type", "status", "fits", "curves", "data", "summary",
    "dose_column", "model", "parameters", "qc", "warnings",
    "provenance", "analysis_id", "timestamp_utc")
}

#' États de validité
#' @export
bulk_dose_validity_states <- function() {
  c("valid", "valid_with_warnings")
}

.bulk_dose_stop <- function(state, message) {
  stop(errorCondition(message, class = "bulk_dose_error", state = state))
}

#' État d'une erreur classée bulk_dose_error
#' @export
# §14.1 : délégation à l'accesseur générique. Famille F2 MESURÉE — la classe
# n'est PAS vérifiée : l'état est rendu dès qu'il existe.
bulk_dose_error_state <- function(e) ts_error_state(e)

#' Surface publique figée (le freeze test refuse toute fonction non listée)
#' @export
bulk_dose_public_api <- function() {
  c("bulk_dose_models", "bulk_dose_contract_fields", "bulk_dose_validity_states",
    "bulk_dose_error_state", "run_dose_response", "plot_dose_response_curve",
    "build_dose_table_export", "assert_bulk_dose_result", "bulk_dose_public_api")
}

#' Ajustement dose-réponse par gène (drc::drm, MVP NEW-1)
#'
#' @param vst_mat matrice transformée (gènes x échantillons).
#' @param metadata data.frame d'échantillons (aligné sur les colonnes, ou
#'   joint par rownames si elles correspondent).
#' @param dose_column colonne numérique de dose/temps — DÉCLARÉE, jamais
#'   déduite ; valeurs strictement positives exigées.
#' @param genes gènes à ajuster (ex. top N DE) ; absents de la matrice =
#'   exclus et comptabilisés ; plafonné à TS_BULK_DOSE_MAX_GENES.
#' @param model fonction drc 4 paramètres (bulk_dose_models()).
#' @return liste canonique : type "bulk_dose_response", fits (data.frame par
#'   gène : b/c/d/e_logec50/ec50/r2/fit_ok), curves (grilles prédites + IC),
#'   data (réponses brutes par gène), summary, provenance.
#' @export
run_dose_response <- function(vst_mat, metadata, dose_column, genes,
                              model = "LL.4",
                              min_doses = .bulk_dose_config("TS_BULK_DOSE_MIN_DOSES", 4L),
                              max_genes = .bulk_dose_config("TS_BULK_DOSE_MAX_GENES", 200L),
                              curve_points = .bulk_dose_config("TS_BULK_DOSE_CURVE_POINTS", 100L)) {

  if (!model %in% bulk_dose_models()) {
    .bulk_dose_stop("invalid_input",
      sprintf("bulk_dose : modèle '%s' non supporté (autorisés : %s).",
              model, paste(bulk_dose_models(), collapse = ", ")))
  }
  if (!requireNamespace("drc", quietly = TRUE)) {
    .bulk_dose_stop("compute_failed",
      "bulk_dose : le package 'drc' est requis (renv::install('drc') ou renv::restore()).")
  }

  # -- gardes matrice / metadata (mêmes conventions que bulk_pattern) ----------
  if (is.null(vst_mat) || !is.matrix(vst_mat) || !is.numeric(vst_mat) ||
      is.null(colnames(vst_mat)) || is.null(rownames(vst_mat))) {
    .bulk_dose_stop("invalid_input",
      "bulk_dose : 'vst_mat' doit être une matrice numérique nommée (gènes x échantillons) — lancez d'abord l'étape 1 (Filtrage & VST).")
  }
  if (is.null(metadata) || !is.data.frame(metadata)) {
    .bulk_dose_stop("invalid_input", "bulk_dose : 'metadata' doit être un data.frame.")
  }
  if (is.null(dose_column) || length(dose_column) != 1L || is.na(dose_column) ||
      !nzchar(dose_column) || !dose_column %in% colnames(metadata)) {
    .bulk_dose_stop("invalid_input",
      sprintf("bulk_dose : colonne de dose '%s' absente des métadonnées.",
              ifelse(is.null(dose_column), "", dose_column)))
  }

  if (!is.null(rownames(metadata)) &&
      setequal(rownames(metadata), colnames(vst_mat))) {
    metadata <- metadata[colnames(vst_mat), , drop = FALSE]
  } else if (nrow(metadata) != ncol(vst_mat)) {
    .bulk_dose_stop("invalid_input",
      sprintf("bulk_dose : %d échantillons dans la matrice VST vs %d lignes de métadonnées — impossible d'aligner.",
              ncol(vst_mat), nrow(metadata)))
  }

  dose_raw <- suppressWarnings(as.numeric(metadata[[dose_column]]))
  keep_smp <- !is.na(dose_raw) & is.finite(dose_raw)
  n_samples_na <- sum(!keep_smp)
  doses <- dose_raw[keep_smp]
  if (length(doses) < min_doses) {
    .bulk_dose_stop("invalid_input",
      sprintf("bulk_dose : %d valeur(s) de '%s' exploitables — minimum %d.",
              length(doses), dose_column, min_doses))
  }
  if (any(doses <= 0)) {
    .bulk_dose_stop("invalid_input",
      paste0("bulk_dose : les valeurs de '", dose_column,
             "' doivent être STRICTEMENT POSITIVES (modèles log-logistique/Weibull : log(dose)). ",
             "Pour un temps commençant à 0, déclarez une colonne décalée (ex. temps + 1)."))
  }
  n_doses <- length(unique(doses))
  if (n_doses < min_doses) {
    .bulk_dose_stop("invalid_input",
      sprintf("bulk_dose : %d dose(s)/temps distinct(s) — minimum %d pour un ajustement 4 paramètres.",
              n_doses, min_doses))
  }

  # -- sélection des gènes ------------------------------------------------------
  n_genes_input <- nrow(vst_mat)
  genes <- unique(as.character(genes))
  genes <- genes[nzchar(genes)]
  n_not_found <- sum(!genes %in% rownames(vst_mat))
  genes <- intersect(genes, rownames(vst_mat))
  if (length(genes) == 0) {
    .bulk_dose_stop("invalid_input",
      "bulk_dose : aucun des gènes fournis n'est présent dans la matrice VST (vérifiez les identifiants).")
  }
  warnings <- character(0)
  if (n_not_found > 0L) {
    warnings <- c(warnings, sprintf("%d gène(s) sur %d fournis absents de la matrice VST — exclus.",
                                    n_not_found, n_not_found + length(genes)))
  }
  if (length(genes) > max_genes) {
    warnings <- c(warnings, sprintf("Liste tronquée aux %d premiers gènes (plafond TS_BULK_DOSE_MAX_GENES).",
                                    max_genes))
    genes <- head(genes, max_genes)
  }

  # -- grille commune (log-régulière : les modèles sont en log(dose)) -----------
  grid <- exp(seq(log(min(doses)), log(max(doses)), length.out = curve_points))

  # -- ajustement gène par gène -------------------------------------------------
  smp <- colnames(vst_mat)[keep_smp]
  # drc::drm attend l'OBJET modèle (résultat de LL.4()), pas la fonction :
  # lui passer la fonction brute déclenche un échec de parse interne (vec2mat).
  fct <- getExportedValue("drc", model)()
  fits_list <- vector("list", length(genes))
  curves <- vector("list", length(genes)); names(curves) <- genes
  rawdat <- vector("list", length(genes)); names(rawdat) <- genes
  n_ok <- 0L
  for (i in seq_along(genes)) {
    g <- genes[i]
    resp <- as.numeric(vst_mat[g, smp])
    df <- data.frame(dose = doses, resp = resp)
    rawdat[[g]] <- df
    fit_ok <- TRUE; msg <- NA_character_
    cf <- rep(NA_real_, 4); r2 <- NA_real_; curve <- NULL
    fit <- tryCatch(
      # drm émet fréquemment des avertissements numériques bénins (convergence,
      # variance non finie sur la grille) : mufflés ici — seul l'objet compte.
      # Un échec réel (error) est traité comme fit_ok = FALSE.
      withCallingHandlers(
        drc::drm(resp ~ dose, data = df, fct = fct),
        warning = function(w) invokeRestart("muffleWarning")
      ),
      error = function(e) e
    )
    if (inherits(fit, "error")) {
      fit_ok <- FALSE
      msg <- conditionMessage(fit)
    } else {
      cf <- tryCatch(as.numeric(stats::coefficients(fit)), error = function(e) rep(NA_real_, 4))
      if (length(cf) < 4 || any(!is.finite(cf))) {
        fit_ok <- FALSE; msg <- "coefficients non finis"
      } else {
        pr <- tryCatch(
          withCallingHandlers(
            stats::predict(fit, newdata = data.frame(dose = grid),
                           interval = "confidence"),
            warning = function(w) invokeRestart("muffleWarning")
          ),
          error = function(e) NULL
        )
        if (is.null(pr) || is.null(dim(pr)) ||
            !all(c("Prediction", "Lower", "Upper") %in% colnames(pr))) {
          fit_ok <- FALSE; msg <- "prédiction de courbe impossible"
        } else {
          fitted_vals <- tryCatch(
            withCallingHandlers(stats::fitted(fit),
                                warning = function(w) invokeRestart("muffleWarning")),
            error = function(e) rep(NA_real_, length(resp))
          )
          r2 <- tryCatch(stats::cor(fitted_vals, resp, use = "complete.obs")^2,
                         error = function(e) NA_real_)
          curve <- data.frame(dose = grid,
                              fit = as.numeric(pr[, "Prediction"]),
                              lower = as.numeric(pr[, "Lower"]),
                              upper = as.numeric(pr[, "Upper"]),
                              stringsAsFactors = FALSE)
          n_ok <- n_ok + 1L
        }
      }
    }
    fits_list[[i]] <- data.frame(
      gene = g, model = model,
      b_slope = cf[1], c_lower = cf[2], d_upper = cf[3],
      e_logec50 = cf[4], ec50 = if (is.finite(cf[4])) exp(cf[4]) else NA_real_,
      r2 = r2, fit_ok = fit_ok, message = msg,
      stringsAsFactors = FALSE)
    curves[[g]] <- curve
  }
  fits <- do.call(rbind, fits_list)
  if (n_ok == 0L) {
    .bulk_dose_stop("compute_failed",
      sprintf("bulk_dose : aucun ajustement %s n'a convergé (%d gène(s)) — vérifiez la dynamique de réponse.",
              model, nrow(fits)))
  }
  if (n_ok < nrow(fits)) {
    failed <- fits$gene[!fits$fit_ok]
    warnings <- c(warnings, sprintf("%d gène(s) sans ajustement convergent : %s.",
                                    length(failed), paste(utils::head(failed, 8), collapse = ", ")))
  }

  status <- if (length(warnings) > 0L) "valid_with_warnings" else "valid"

  provenance <- new_provenance_entry(
    analysis_id = "bulk-dose-response",
    method      = paste0("drc::drm (", model, ") par gène"),
    parameters  = list(dose_column = dose_column, model = model,
                       n_genes_input = n_genes_input,
                       min_doses = min_doses, max_genes = max_genes),
    dataset     = vst_mat,
    warnings    = warnings
  )

  list(
    type        = "bulk_dose_response",
    status      = status,
    fits        = fits,
    curves      = curves,
    data        = rawdat,
    dose_column = dose_column,
    model       = model,
    summary = list(
      n_genes_input = n_genes_input,
      n_genes_used  = nrow(fits),
      n_genes_not_found = n_not_found,
      n_fit_ok  = n_ok,
      n_fit_failed = nrow(fits) - n_ok,
      n_doses   = n_doses,
      dose_min  = min(doses),
      dose_max  = max(doses),
      n_samples = length(doses),
      n_samples_na = n_samples_na
    ),
    parameters = list(dose_column = dose_column, model = model,
                      min_doses = min_doses, max_genes = max_genes,
                      curve_points = curve_points),
    qc = list(n_genes_used = nrow(fits), n_samples = length(doses),
              n_samples_na = n_samples_na, n_doses = n_doses),
    warnings      = warnings,
    provenance    = provenance,
    analysis_id   = "bulk-dose-response",
    timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

.bulk_dose_config <- function(name, fallback) {
  if (exists(name, inherits = TRUE)) get(name) else fallback
}

#' Courbe ajustée + IC pour un gène (descriptif)
#'
#' @param dose_result résultat canonique de run_dose_response().
#' @param gene gène à tracer (doit avoir un ajustement convergent).
#' @param tr fonction de traduction optionnelle.
#' @return ggplot (points bruts + courbe ajustée + ruban d'IC).
#' @export
plot_dose_response_curve <- function(dose_result, gene, tr = NULL) {
  tr <- tr %||% function(x) x
  assert_bulk_dose_result(dose_result, context = "plot dose-réponse")
  if (!gene %in% names(dose_result$curves) ||
      is.null(dose_result$curves[[gene]])) {
    .bulk_dose_stop("invalid_input",
      sprintf("bulk_dose : pas de courbe disponible pour '%s' (ajustement absent ou non convergent).", gene))
  }
  curve <- dose_result$curves[[gene]]
  raw <- dose_result$data[[gene]]
  fits_row <- dose_result$fits[dose_result$fits$gene == gene, , drop = FALSE]

  ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = curve,
                         ggplot2::aes(x = .data$dose, ymin = .data$lower, ymax = .data$upper),
                         fill = "grey70", alpha = 0.4) +
    ggplot2::geom_line(data = curve,
                       ggplot2::aes(x = .data$dose, y = .data$fit),
                       color = "#18BC9C", linewidth = 1) +
    ggplot2::geom_point(data = raw,
                        ggplot2::aes(x = .data$dose, y = .data$resp),
                        size = 2.2, alpha = 0.8) +
    ggplot2::labs(
      title = sprintf("%s — %s", tr("Courbe dose-réponse"), gene),
      subtitle = sprintf("%s = %s, EC50 = %.3g, R2 = %.2f (%s)",
                         tr("modèle"), fits_row$model,
                         fits_row$ec50, fits_row$r2, tr("descriptif")),
      x = dose_result$dose_column,
      y = tr("Expression VST")) +
    ggplot2::scale_x_log10() +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14))
}

#' Table d'export plate (une ligne par gène)
#'
#' @param dose_result résultat canonique.
#' @return data.frame des ajustements (sans les courbes).
#' @export
build_dose_table_export <- function(dose_result) {
  assert_bulk_dose_result(dose_result, context = "export dose-réponse")
  dose_result$fits
}

#' Refuse un objet qui n'est pas un résultat canonique dose-réponse
#'
#' @param dose_result objet à valider.
#' @param context contexte inclus dans le message d'erreur.
#' @return invisible(TRUE), ou stop() classé.
#' @export
assert_bulk_dose_result <- function(dose_result, context = "") {
  ctx <- if (nzchar(context)) paste0(" (", context, ")") else ""
  if (is.null(dose_result) || !is.list(dose_result) ||
      !identical(dose_result$type, "bulk_dose_response")) {
    .bulk_dose_stop("invalid_input",
      paste0("assert_bulk_dose_result", ctx, " : objet non canonique ",
             "(type attendu 'bulk_dose_response')."))
  }
  if (!dose_result$status %in% bulk_dose_validity_states()) {
    .bulk_dose_stop("invalid_input",
      paste0("assert_bulk_dose_result", ctx, " : status '",
             dose_result$status, "' inconnu."))
  }
  if (!is.data.frame(dose_result$fits) ||
      !all(c("gene", "ec50", "fit_ok") %in% colnames(dose_result$fits))) {
    .bulk_dose_stop("invalid_input",
      paste0("assert_bulk_dose_result", ctx, " : 'fits' doit être un data.frame avec gene/ec50/fit_ok."))
  }
  invisible(TRUE)
}

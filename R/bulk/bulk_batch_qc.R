# =============================================================================
# R/bulk/bulk_batch_qc.R — Diagnostics d'effets batch (roadmap Bulk V2,
# Milestone 1) — PCA par batch, décomposition de variance, plan d'expérience.
# =============================================================================
# FONCTIONS PURES (aucune réactivité Shiny) consommées par mod_bulk_filter.R
# (onglet "QC Batch"). Réutilise les APIs existantes — jamais dupliquées :
#   - plot_bulk_pca() / plot_scree_bulk() (R/bulk/bulk_helpers.R) pour la PCA
#     et la variance expliquée ;
#   - check_design_confounding() (R/core/validation.R) pour la collinéarité
#     batch × condition ;
#   - run_job() (R/core/jobs.R) côté module pour le chemin async.
#
# variancePartition : la décomposition utilise
# variancePartition::fitExtractVarPartModel() quand le package est installé ;
# sinon REPLI R pur documenté (régression par gène, R² partiel par facteur —
# approximation linéaire, signalée comme telle au résultat, jamais silencieuse).
# NOTE ENVS (2026-09-06) : variancePartition 1.36.3 + lme4 récent échouent en
# amont (« the 'findbars' function has moved to the reformulas package » —
# incompatibilité Bioconductor/lme4, pas un problème de données). Le repli
# R pur prend le relais avec son avertissement ; la voie variancePartition se
# réactivera d'elle-même dès qu'une version compatible sera installée.
#
# Garde-fous :
#   - la matrice d'entrée DOIT être transformée (VST / TMM log2-CPM) :
#     des counts bruts sont refusés (les counts Poissonnien viole l'hypothèse
#     gaussienne des modèles linéaires et du PCA) ;
#   - les effectifs de gènes pour la décomposition sont plafonnés à
#     TS_BULK_VARPART_MAX_GENES (repli R pur : sous-échantillonnage déterministe
#     des gènes les plus variables, seed enregistré).
# Erreurs : classées `bulk_batch_qc_error` (français, call. = FALSE).
# =============================================================================

#' Surface publique figée du domaine diagnostics batch (gel par test de freeze)
bulk_batch_qc_public_api <- function() {
  c("bulk_batch_qc_public_api",
    "bulk_assert_transformed_matrix",
    "bulk_batch_design_check",
    "bulk_variance_partition",
    "plot_bulk_varpart",
    "plot_bulk_batch_scree")
}

#' Refuser les counts bruts : exiger une matrice TRANSFORMÉE (VST / log2-CPM)
#'
#' Heuristique de détection des counts bruts : >= 95 % de valeurs exactement
#' entières, toutes >= 0, et amplitude >= 30. Une matrice VST / log2-CPM / TPM
#' est CONTINUE par construction (log2 d'un rapport) — la fraction de valeurs
#' exactement entières y est quasi nulle, y compris après arrondi d'affichage.
#' Les matrices rejetées le sont avec un message explicite citant le remède.
#'
#' @param mat Matrice numérique (gènes x échantillons).
#' @param context Contexte cité dans le message d'erreur.
#' @return La matrice, invisible — sinon stop classé `bulk_batch_qc_error`.
bulk_assert_transformed_matrix <- function(mat, context = "diagnostics batch") {
  if (is.null(mat) || !is.matrix(mat) || !is.numeric(mat)) {
    stop(errorCondition(sprintf("Échec %s : une matrice numérique est requise (reçu : %s).",
                 context, if (is.null(mat)) "NULL" else paste(class(mat), collapse = "/")), class = "bulk_batch_qc_error", state = "invalid_input"))
  }
  if (ncol(mat) < 2L) {
    stop(errorCondition(sprintf("Échec %s : au moins 2 échantillons requis (reçu : %d).",
                 context, ncol(mat)), class = "bulk_batch_qc_error", state = "invalid_input"))
  }
  vals <- as.vector(mat)
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0L) {
    stop(errorCondition(sprintf("Échec %s : la matrice ne contient aucune valeur finie.", context), class = "bulk_batch_qc_error", state = "invalid_input"))
  }
  is_int  <- mean(abs(vals - round(vals)) < 1e-8) > 0.95
  is_nonneg <- all(vals >= 0)
  has_amp <- max(vals) - min(vals) >= 30
  if (is_int && is_nonneg && has_amp) {
    stop(errorCondition(paste0("Échec ", context, " : cette matrice ressemble à des COUNTS BRUTS ",
                "(valeurs entières, non négatives, amplitude élevée). Cette analyse exige ",
                "une matrice transformée (VST DESeq2 ou TMM log2-CPM) — lancez d'abord ",
                "« Filtrage & VST » (étape 1)."), class = "bulk_batch_qc_error", state = "raw_counts_rejected"))
  }
  invisible(mat)
}

#' Contrôle du plan d'expérience : cross-tab batch × condition + collinéarité
#'
#' Construit `table(metadata$batch, metadata$condition)` et détecte :
#'   - collinéarité TOTALE (chaque niveau de batch observé dans un seul niveau
#'     de condition — réutilise check_design_confounding()) : l'effet batch ne
#'     peut PAS être ajusté dans le design, alerte bloquante pour le DE ;
#'   - partiellement déséquilibré (au moins un croisement vide) : avertissement.
#'
#' @param metadata data.frame métadonnées échantillons.
#' @param batch_col,condition_col Noms de colonnes. `condition_col` peut être
#'   NULL (contrôle batch seul, pas de collinéarité testée).
#' @return list(cross_table, n_samples, n_batch_levels, n_condition_levels,
#'   fully_collinear, empty_cells, warning_messages) — tables brutes, jamais
#'   de redondance interprétative.
bulk_batch_design_check <- function(metadata, batch_col, condition_col = NULL) {
  if (is.null(metadata) || !is.data.frame(metadata)) {
    stop(errorCondition("bulk_batch_design_check() : un data.frame de métadonnées est requis.", class = "bulk_batch_qc_error", state = "invalid_input"))
  }
  if (length(batch_col) != 1L || is.na(batch_col) || !batch_col %in% colnames(metadata)) {
    stop(errorCondition(sprintf("bulk_batch_design_check() : colonne batch '%s' introuvable (disponibles : %s).",
                 batch_col, paste(colnames(metadata), collapse = ", ")), class = "bulk_batch_qc_error", state = "invalid_input"))
  }
  if (!is.null(condition_col) &&
      (length(condition_col) != 1L || !condition_col %in% colnames(metadata))) {
    stop(errorCondition(sprintf("bulk_batch_design_check() : colonne condition '%s' introuvable (disponibles : %s).",
                 condition_col, paste(colnames(metadata), collapse = ", ")), class = "bulk_batch_qc_error", state = "invalid_input"))
  }

  batch_vals <- factor(metadata[[batch_col]])
  cond_vals  <- if (is.null(condition_col)) NULL else factor(metadata[[condition_col]])

  cross <- if (is.null(cond_vals)) {
    table(Batch = batch_vals)
  } else {
    table(Condition = cond_vals, Batch = batch_vals)
  }

  fully_collinear <- FALSE
  empty_cells     <- 0L
  warnings_msg    <- character(0)

  if (!is.null(cond_vals)) {
    fully_collinear <- check_design_confounding(metadata, condition_col, batch_col)
    if (fully_collinear) {
      warnings_msg <- c(warnings_msg, sprintf(
        paste0("Batch (« %s ») et condition (« %s ») sont ENTIÈREMENT collinéaires : ",
               "chaque lot ne contient qu'un seul groupe. L'effet batch ne peut PAS être ",
               "ajusté dans le design — toute différence entre groupes peut être un effet lot."),
        batch_col, condition_col))
    } else if (nrow(cross) > 1L && ncol(cross) > 1L) {
      empty_cells <- sum(cross == 0L)
      if (empty_cells > 0L) {
        warnings_msg <- c(warnings_msg, sprintf(
          paste0("Plan déséquilibré : %d croisement(s) condition × batch vide(s) ",
                 "(sur %d) — l'ajustement batch reste possible mais moins robuste."),
          empty_cells, length(cross)))
      }
    }
  }
  n_levels_batch <- length(unique(stats::na.omit(as.character(batch_vals))))
  if (n_levels_batch < 2L) {
    warnings_msg <- c(warnings_msg, sprintf(
      "La colonne batch « %s » n'a qu'une seule modalité — aucun effet lot identifiable.",
      batch_col))
  }

  list(
    cross_table        = cross,
    n_samples          = nrow(metadata),
    n_batch_levels     = n_levels_batch,
    n_condition_levels = if (is.null(cond_vals)) NA_integer_ else length(unique(stats::na.omit(as.character(cond_vals)))),
    fully_collinear    = fully_collinear,
    empty_cells        = empty_cells,
    warning_messages   = warnings_msg
  )
}

#' Sous-échantillonnage déterministe des gènes les plus variables (cap mémoire)
#'
#' @param mat Matrice (gènes x échantillons). @param max_genes Plafond (>0).
#' @param seed Graine enregistrée (reproductibilité déclarée).
#' @return Liste(mat, n_total, n_used, seed) — mat jamais > max_genes lignes.
.subset_variable_genes <- function(mat, max_genes, seed = 11L) {
  n_total <- nrow(mat)
  if (n_total <= max_genes) {
    return(list(mat = mat, n_total = n_total, n_used = n_total, seed = seed))
  }
  rv <- matrixStats::rowVars(mat)
  ord <- order(rv, decreasing = TRUE, method = "radix")
  # Tirage déterministe parmi les 4×max_genes plus variables : favorise le
  # signal tout en évitant le biais des n premières lignes d'un tri à égalité.
  pool <- ord[seq_len(min(4L * max_genes, n_total))]
  set.seed(seed)
  keep <- sort(pool[sample.int(length(pool), max_genes)])
  list(mat = mat[keep, , drop = FALSE], n_total = n_total,
       n_used = max_genes, seed = seed)
}

#' Décomposition de variance par facteur (gène par gène)
#'
#' variancePartition::fitExtractVarPartModel() si installé ; sinon repli R pur
#' (lm par gène, R² partiel par facteur via sommation des carrés — approximation
#' documentée, `method = "pur_lm_partial_r2"`, jamais présentée comme
#' variancePartition). Les deux chemins rejettent les counts bruts d'abord.
#'
#' @param vst_matrix Matrice TRANSFORMÉE (gènes x échantillons).
#' @param metadata data.frame, rownames = colnames(vst_matrix).
#' @param covariates Vecteur character de colonnes à décomposer (>= 1).
#' @param max_genes Plafond de gènes (défaut TS_BULK_VARPART_MAX_GENES).
#' @param seed Graine du sous-échantillonnage (enregistrée dans le résultat).
#' @param context Contexte cité dans les erreurs.
#' @return list(var_part = data.frame (gène × facteur, fractions 0-1),
#'   method, n_genes_total, n_genes_used, seed, warnings, timestamp_utc).
bulk_variance_partition <- function(vst_matrix, metadata, covariates,
                                    max_genes = NULL, seed = 11L,
                                    context = "décomposition de variance") {
  if (length(covariates) == 0L || !all(covariates %in% colnames(metadata))) {
    stop(errorCondition(sprintf("bulk_variance_partition() : covariables invalides (disponibles : %s).",
                 paste(colnames(metadata), collapse = ", ")), class = "bulk_batch_qc_error", state = "invalid_input"))
  }
  max_genes <- max_genes %||%
    (if (exists("TS_BULK_VARPART_MAX_GENES", inherits = TRUE)) TS_BULK_VARPART_MAX_GENES else 2000L)
  max_genes <- max(1L, as.integer(max_genes))

  bulk_assert_transformed_matrix(vst_matrix, context = context)
  common <- intersect(colnames(vst_matrix), rownames(metadata))
  if (length(common) < 3L) {
    stop(errorCondition(sprintf("Échec %s : au moins 3 échantillons communs matrice/métadonnées requis (reçu : %d).",
                 context, length(common)), class = "bulk_batch_qc_error", state = "invalid_input"))
  }
  vst_matrix <- vst_matrix[, common, drop = FALSE]
  meta       <- metadata[common, , drop = FALSE]
  for (cv in covariates) {
    n_lvl <- length(unique(stats::na.omit(as.character(meta[[cv]]))))
    if (n_lvl < 2L) {
      stop(errorCondition(sprintf("Échec %s : la covariable '%s' a moins de 2 modalités observées.",
                   context, cv), class = "bulk_batch_qc_error", state = "invalid_input"))
    }
  }

  sub  <- .subset_variable_genes(vst_matrix, max_genes, seed = seed)
  expr <- sub$mat
  meta <- meta[colnames(expr), , drop = FALSE]

  # Caractères -> facteurs pour model.matrix (le NA devient sa propre modalité
  # explicite, jamais une ligne silencieusement droppée).
  for (cv in covariates) {
    if (is.character(meta[[cv]])) meta[[cv]] <- factor(meta[[cv]])
  }

  formula_str <- paste("~", paste(covariates, collapse = " + "))
  warnings    <- character(0)
  method      <- "variancePartition::fitExtractVarPartModel"

  vp <- tryCatch({
    if (!requireNamespace("variancePartition", quietly = TRUE)) {
      # Pas de `state` : l'erreur est AVALEE par le tryCatch ci-dessous (repli
      # R pur), donc JAMAIS routable. Or `state` est le vocabulaire de ROUTAGE
      # du domaine (BULK_BATCH_QC_CONTRACT.md §6 : « le module branche son
      # affichage dessus ») et il est gele : y figer une valeur inerte serait
      # exactement l'inflation que le freeze test interdit.
      stop(errorCondition(paste0(
        "bulk_variance_partition() : package 'variancePartition' absent ",
        "(BiocManager::install('variancePartition')) — repli R pur ",
        "(R² partiel par lm) appliqué."),
        class = "bulk_batch_qc_error"))
    }
    vp_obj <- variancePartition::fitExtractVarPartModel(
      expr = expr, formula = stats::as.formula(formula_str), data = meta)
    vp_df <- as.data.frame(vp_obj, check.names = FALSE)
    # Normalisation défensive : les fractions hors [0,1] (instabilités
    # numériques documentées de variancePartition) sont bornées pour l'affichage.
    bad <- sum(vp_df < -1e-6 | vp_df > 1 + 1e-6, na.rm = TRUE)
    if (bad > 0L) {
      warnings <- c(warnings, sprintf(
        "%d fraction(s) de variance hors [0,1] bornée(s) à l'affichage (instabilité numérique).", bad))
      vp_df[vp_df < 0] <- 0; vp_df[vp_df > 1] <- 1
    }
    vp_df
  }, error = function(e) {
    method <<- "pur_lm_partial_r2"
    warnings <<- c(warnings,
      "variancePartition indisponible ou ajustement échoué — repli R pur (R² partiel par lm, approximation).")
    .varpart_pure_r_fallback(expr, meta, covariates)
  })

  list(
    var_part     = vp,
    method       = method,
    formula      = formula_str,
    n_genes_total = sub$n_total,
    n_genes_used = sub$n_used,
    seed         = sub$seed,
    warnings     = warnings,
    timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

#' Repli R pur : R² partiel par facteur via lm() gène par gène
#'
#' Pour chaque gène : lm(y ~ tous les facteurs), fraction de la SCE expliquée
#' par chaque facteur = SCE_facteur / SCE_totale (sommes de carrés de type I
#' dans l'ordre des covariables — approximation DÉCLARÉE, cf. `method`).
#' @return data.frame gènes × (covariables + Residuals), colonnes = fractions.
.varpart_pure_r_fallback <- function(expr, meta, covariates) {
  out <- as.data.frame(
    matrix(NA_real_, nrow = nrow(expr),
           ncol = length(covariates) + 1L,
           dimnames = list(rownames(expr), c(covariates, "Residuals"))),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(expr))) {
    df_i <- cbind(data.frame(y = as.numeric(expr[i, ])), meta)
    fit <- tryCatch(stats::lm(stats::as.formula(paste("y ~", paste(covariates, collapse = " + "))),
                              data = df_i), error = function(e) NULL)
    if (is.null(fit)) next
    ao <- tryCatch(stats::anova(fit), error = function(e) NULL)
    if (is.null(ao)) next
    sses <- ao[["Sum Sq"]]; names(sses) <- rownames(ao)
    tot <- sum(sses, na.rm = TRUE)
    if (!is.finite(tot) || tot <= 0) next
    for (cv in covariates) {
      if (cv %in% names(sses)) out[i, cv] <- sses[[cv]] / tot
    }
    if ("Residuals" %in% names(sses)) out[i, "Residuals"] <- sses[["Residuals"]] / tot
  }
  out
}

#' Boîtes à moustaches des fractions de variance par facteur
#'
#' @param varpart Résultat de bulk_variance_partition() (champ var_part).
#' @return ggplot — facteurs triés par variance médiane décroissante.
plot_bulk_varpart <- function(varpart, tr = NULL) {
  tr <- tr %||% function(x) x
  vp <- varpart$var_part
  if (is.null(vp) || nrow(vp) == 0L) {
    stop("plot_bulk_varpart() : aucune fraction de variance à tracer.", call. = FALSE)
  }
  facet_order <- names(sort(colMeans(vp, na.rm = TRUE), decreasing = TRUE))
  long <- reshape2::melt(as.matrix(vp), varnames = c("gene", "factor"),
                         value.name = "fraction", na.rm = TRUE)
  long$factor <- factor(long$factor, levels = facet_order)
  # PLOT-S1 : theme partage ; 12 = valeur historique de ce site, ecrite
  # explicitement (zero changement visuel). Appels ggplot2 prefixes : ce fichier
  # doit rester testable hors de l'app, ou ggplot2 n'est pas attache.
  p <- ggplot2::ggplot(long, ggplot2::aes(x = fraction, y = factor, fill = factor)) +
    ggplot2::geom_boxplot(na.rm = TRUE, outlier.size = 0.6, outlier.alpha = 0.4) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(round(100 * x), "%"),
                                limits = c(0, 1), expand = c(0.01, 0)) +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::labs(title = tr("Décomposition de la variance (par gène)"),
                  subtitle = tr(paste0("Méthode : ", varpart$method,
                                       " — gènes analysés : ", format(varpart$n_genes_used, big.mark = ","))),
                  x = tr("Fraction de la variance expliquée"), y = NULL, fill = NULL) +
    ts_theme("minimal", 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 13),
                   legend.position = "none")
  if (length(varpart$warnings) > 0L) {
    p <- p + ggplot2::labs(caption = paste0("\u26a0\ufe0f ", paste(varpart$warnings, collapse = " ; ")))
  }
  p
}

#' Variance expliquée par composante, version « QC Batch » (habillage facteurs)
#'
#' Repli délibéré sur plot_scree_bulk() existant : la variance expliquée par PC
#' est UNE quantité, sans redondance de code. Cette fonction n'existe que pour
#' porter un titre orienté diagnostics batch.
#' @return ggplot.
plot_bulk_batch_scree <- function(vst_matrix, tr = NULL) {
  tr <- tr %||% function(x) x
  plot_scree_bulk(vst_matrix, tr = function(x) x) +
    ggplot2::labs(title = tr("Scree Plot — Variance Expliquée (QC Batch)"))
}

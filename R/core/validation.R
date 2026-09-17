# =============================================================================
# R/core/validation.R — Domain-agnostic validation guards
# =============================================================================
# CHRYSALIS PHASE — contrat d'état & frontières testables (ROADMAP.txt).
# Generic guards with NO domain-specific imports (Seurat/DESeq2) except where
# explicitly noted. Sourced in app.R BEFORE any domain file so every domain
# (Bulk, SC, Spatial) can call these without namespace prefix.
#
#   - check_design_confounding  (moved from R/bulk/bulk_helpers.R)
#   - validate_bulk_design      (moved from R/bulk/bulk_helpers.R)
#   - validate_seurat_reduction (extracted from R/sc/sc_helpers.R pattern)
#
# R/bulk/bulk_helpers.R retains IDENTICAL copies for standalone test sourcing
# (tests/testthat/test-bulk-helpers.R sources bulk_helpers directly). Canonical
# source of truth is THIS file; app.R loads it first, so the second definition
# never overwrites with divergent logic. Keep in sync if bulk_helpers copy changes.
# =============================================================================

#' Check if a covariate is fully confounded with the main condition
#'
#' DESeq2's design matrix loses full rank when a covariate level maps to
#' exactly one condition level — produces cryptic "model matrix is not full rank".
#' This check runs BEFORE fitting so the UI can show a clear message.
#'
#' @param metadata Sample metadata data.frame.
#' @param condition_col Character, main condition column.
#' @param covariate_col Character, covariate column to check.
#' @return Logical — TRUE if covariate_col is fully confounded with condition_col.
check_design_confounding <- function(metadata, condition_col, covariate_col) {
  if (!condition_col %in% colnames(metadata) || !covariate_col %in% colnames(metadata)) {
    return(FALSE)
  }
  tbl <- table(
    factor(metadata[[condition_col]]),
    factor(metadata[[covariate_col]])
  )
  if (nrow(tbl) < 2 || ncol(tbl) < 2) return(FALSE)
  # Confounded when every covariate level is observed in only one condition level
  nonzero_per_covariate_level <- colSums(tbl > 0)
  all(nonzero_per_covariate_level == 1)
}

#' Measure the residual degrees of freedom of a design (n samples vs p coefficients)
#'
#' A design is SATURATED when `n <= p`: the model has no residual degrees of
#' freedom, so nothing can estimate the dispersion. Measured behaviour of the
#' three engines (2026-09-17, renv project) — they all fail, but differently:
#'   - DESeq2: `estimateDispersionsGeneEst()` refuses outright
#'     ("The design matrix has the same number of samples and coefficients to fit").
#'   - edgeR: `estimateDisp()` does NOT fail — it warns "No residual df: setting
#'     dispersion to NA", and `glmQLFit()` then dies with "NA dispersions not allowed".
#'   - limma-voom: `voom()` warns and `eBayes()` fails with
#'     "No residual degrees of freedom in linear model fits".
#' So a single up-front test is needed; waiting for the engine to complain gives
#' three different, cryptic messages.
#'
#' `p` is read off the model matrix, NOT guessed: it is 1 (intercept) plus
#' `nlevels - 1` per factor term plus 1 per numeric term, which is exactly what
#' the engines will build.
#'
#' @param metadata Sample metadata data.frame.
#' @param condition_col Character, main condition column.
#' @param covariates Character vector of additional design columns (may be empty).
#' @return list(saturated = logical, n = integer effective samples, p = integer coefficients).
#'   `saturated` is FALSE whenever the design cannot be evaluated at all
#'   (missing column, < 2 complete cases, unparseable formula) — being unable
#'   to prove saturation must never block a design that might be fine.
design_saturation <- function(metadata, condition_col, covariates = character(0)) {
  out <- list(saturated = FALSE, n = NA_integer_, p = NA_integer_)
  if (!is.data.frame(metadata)) return(out)
  if (length(condition_col) != 1L || is.na(condition_col)) return(out)
  if (!condition_col %in% colnames(metadata)) return(out)

  cols <- unique(c(condition_col, intersect(covariates, colnames(metadata))))
  d    <- metadata[, cols, drop = FALSE]
  # Only complete cases reach the model matrix: an NA row is dropped by every
  # engine, so it lowers n without lowering p.
  d    <- d[stats::complete.cases(d), , drop = FALSE]
  if (nrow(d) < 2L) return(out)

  mm <- tryCatch(stats::model.matrix(stats::reformulate(cols), data = d),
                 error = function(e) NULL)
  if (is.null(mm) || ncol(mm) < 1L) return(out)

  out$n <- nrow(mm)
  out$p <- ncol(mm)
  out$saturated <- out$n <= out$p
  out
}

#' Is the design saturated (no residual degrees of freedom)?
#'
#' Thin boolean wrapper around `design_saturation()` — see it for the
#' engine-by-engine failure modes this predicts.
#'
#' @param metadata Sample metadata data.frame.
#' @param condition_col Character, main condition column.
#' @param covariates Character vector of additional design columns (may be empty).
#' @return Logical — TRUE when the design cannot estimate a dispersion.
check_design_saturated <- function(metadata, condition_col, covariates = character(0)) {
  design_saturation(metadata, condition_col, covariates)$saturated
}

#' Validate a full design (condition + optional covariates) before fitting
#'
#' @param metadata Sample metadata data.frame.
#' @param condition_col Character, main condition column.
#' @param covariates Character vector of additional covariate columns (may be empty).
#' @return Character vector of human-readable problem descriptions (empty if none).
validate_bulk_design <- function(metadata, condition_col, covariates = character(0)) {
  problems <- character(0)

  if (!condition_col %in% colnames(metadata)) return(problems)

  sat <- design_saturation(metadata, condition_col, covariates)
  if (sat$saturated) {
    problems <- c(problems, sprintf(
      paste0("Plan sans réplicat : %d échantillon(s) pour %d coefficient(s) (~ %s) — aucun degré de liberté ",
             "résiduel, donc aucune dispersion estimable. DESeq2 refusera, edgeR rendra une dispersion NA, ",
             "limma échouera dans eBayes(). Ajoutez des réplicats, ou activez le mode exploratoire ",
             "sans réplicat (dispersion imposée) en connaissance de cause."),
      sat$n, sat$p, paste(unique(c(condition_col, covariates)), collapse = " + ")
    ))
  }

  n_na <- sum(is.na(metadata[[condition_col]]))
  if (n_na > 0) {
    problems <- c(problems, sprintf(
      "%d échantillon(s) ont une valeur manquante (NA) dans '%s'.", n_na, condition_col
    ))
  }

  grp_counts <- table(metadata[[condition_col]])
  if (any(grp_counts < 2)) {
    problems <- c(problems, sprintf(
      "Groupe(s) avec un seul réplicat dans '%s' (sur '%s' seule) : %s — résultats statistiquement peu fiables.",
      condition_col, condition_col, paste(names(grp_counts)[grp_counts < 2], collapse = ", ")
    ))
  }

  # -- Effective (complete-case) replicate check -------------------------
  covariates <- intersect(covariates, colnames(metadata))
  if (length(covariates) > 0) {
    design_cols <- unique(c(condition_col, covariates))
    complete    <- stats::complete.cases(metadata[, design_cols, drop = FALSE])
    n_dropped   <- sum(!complete)

    if (n_dropped > 0) {
      grp_counts_cc <- table(metadata[[condition_col]][complete])
      if (any(grp_counts_cc < 2)) {
        problems <- c(problems, sprintf(
          paste0("Groupe(s) avec un seul réplicat APRÈS exclusion de %d échantillon(s) ayant un NA ",
                 "dans une covariable (%s) : %s. Vérifiez vos métadonnées (coldata GEO mal parsé ?)."),
          n_dropped, paste(covariates, collapse = ", "),
          paste(names(grp_counts_cc)[grp_counts_cc < 2], collapse = ", ")
        ))
      }
    }
  }

  # -- Single-level covariate ----------------------------------------------
  for (cov in covariates) {
    n_levels <- length(unique(metadata[[cov]][!is.na(metadata[[cov]])]))
    if (n_levels < 2) {
      only_val <- if (n_levels == 1) as.character(unique(stats::na.omit(metadata[[cov]]))) else "aucune valeur exploitable"
      problems <- c(problems, sprintf(
        paste0("La covariable '%s' n'a qu'une seule modalité observée (%s) - elle n'apporte aucune ",
               "information, sera ignorée silencieusement par le design (~ %s), et ne corrige donc PAS ",
               "pour cet effet comme attendu. Retirez-la du design."),
        cov, only_val, paste(unique(c(covariates, condition_col)), collapse = " + ")
      ))
    }
  }

  for (cov in covariates) {
    if (check_design_confounding(metadata, condition_col, cov)) {
      problems <- c(problems, sprintf(
        "La covariable '%s' est entièrement confondue avec '%s' - son effet ne peut pas être estimé séparément (design non plein rang). Retirez-la ou vérifiez votre plan d'expérience.",
        cov, condition_col
      ))
    }
  }

  problems
}

#' Validate Seurat object reduction availability
#'
#' Domain-agnostic guard: Seurat is optional at source-time (checked via
#' inherits/requireNamespace at call-time only), so sourcing this file never
#' hard-requires Seurat to be installed.
#'
#' @param obj Seurat object (or NULL).
#' @param reduction Character, reduction name (e.g. "umap", "pca").
#' @return invisible(TRUE) if valid, otherwise stops with a clear message.
validate_seurat_reduction <- function(obj, reduction) {
  if (is.null(obj) || !inherits(obj, "Seurat")) {
    stop("Object must be a valid Seurat object.", call. = FALSE)
  }
  if (!reduction %in% names(obj@reductions)) {
    stop(sprintf("Reduction '%s' not found in Seurat object.", reduction), call. = FALSE)
  }
  invisible(TRUE)
}

# =============================================================================
# Famille assert_* (CHRYSALIS 2B) — gardes defensives generiques, pipables.
#
# Contrat commun : stop() avec message francais clair + call. = FALSE (style
# assert_seurat_object() de R/core/io_helpers.R), et retour INVISIBLE de
# l'objet valide pour pouvoir piper. Toute erreur cite le `context` fourni
# par l'appelant afin que le message localise l'etape fautive.
#
# NB : assertseuratobject()/assert_seurat_object() (io_helpers.R) reste en
# place pour ne pas changer le comportement en cours de chrysalide —
# candidate a une consolidation ULTERIEURE vers cette famille.
# =============================================================================

#' Verifier que l'objet est un Seurat valide
#'
#' @param obj Objet quelconque (NULL accepte syntaxiquement, refuse).
#' @param context Contexte d'appel cite dans le message d'erreur.
#' @return L'objet, invisible.
assert_seurat <- function(obj, context = "analyse") {
  if (is.null(obj) || !inherits(obj, "Seurat")) {
    stop(sprintf(
      "Echec %s : un objet Seurat valide est requis (recu : %s).",
      context, if (is.null(obj)) "NULL" else paste(class(obj), collapse = "/")
    ), call. = FALSE)
  }
  invisible(obj)
}

#' Verifier la presence d'un assay dans un objet Seurat
#'
#' @param obj Objet Seurat.
#' @param assay_name Nom de l'assay attendu (ex. "RNA").
#' @param context Contexte d'appel.
#' @return L'objet, invisible.
assert_assay <- function(obj, assay_name, context = "analyse") {
  assert_seurat(obj, context = context)
  if (!assay_name %in% names(obj@assays)) {
    stop(sprintf(
      "Echec %s : l'assay '%s' est introuvable dans l'objet Seurat (disponibles : %s).",
      context, assay_name,
      if (length(names(obj@assays))) paste(names(obj@assays), collapse = ", ") else "aucun"
    ), call. = FALSE)
  }
  invisible(obj)
}

#' Verifier la presence d'une reduction dans un objet Seurat
#'
#' @param obj Objet Seurat.
#' @param reduction_name Nom de la reduction attendue (ex. "umap").
#' @param context Contexte d'appel.
#' @return L'objet, invisible.
assert_reduction <- function(obj, reduction_name, context = "analyse") {
  assert_seurat(obj, context = context)
  if (!reduction_name %in% names(obj@reductions)) {
    stop(sprintf(
      "Echec %s : la reduction '%s' est introuvable dans l'objet Seurat (disponibles : %s).",
      context, reduction_name,
      if (length(names(obj@reductions))) paste(names(obj@reductions), collapse = ", ") else "aucune"
    ), call. = FALSE)
  }
  invisible(obj)
}

#' Verifier que des identifiants de cellules existent tous dans l'objet
#'
#' @param obj Objet Seurat.
#' @param cell_ids Vecteur d'identifiants de cellules.
#' @param context Contexte d'appel.
#' @return L'objet, invisible.
assert_cells <- function(obj, cell_ids, context = "analyse") {
  assert_seurat(obj, context = context)
  missing_cells <- setdiff(as.character(cell_ids), colnames(obj))
  if (length(missing_cells) > 0) {
    stop(sprintf(
      "Echec %s : %d cellule(s) absente(s) de l'objet Seurat (ex. : %s).",
      context, length(missing_cells),
      paste(utils::head(missing_cells, 3), collapse = ", ")
    ), call. = FALSE)
  }
  invisible(obj)
}

#' Verifier la presence d'une colonne de metadonnees
#'
#' @param obj Objet Seurat.
#' @param column_name Nom de la colonne attendue dans obj@meta.data.
#' @param context Contexte d'appel.
#' @return L'objet, invisible.
assert_metadata_column <- function(obj, column_name, context = "analyse") {
  assert_seurat(obj, context = context)
  if (!column_name %in% colnames(obj@meta.data)) {
    stop(sprintf(
      "Echec %s : la colonne de metadonnees '%s' est introuvable (disponibles : %s).",
      context, column_name,
      if (length(colnames(obj@meta.data))) paste(utils::head(colnames(obj@meta.data), 10), collapse = ", ") else "aucune"
    ), call. = FALSE)
  }
  invisible(obj)
}

#' Verifier qu'un objet est une matrice numerique (dense)
#'
#' @param mat Objet quelconque.
#' @param context Contexte d'appel.
#' @return La matrice, invisible.
assert_numeric_matrix <- function(mat, context = "analyse") {
  if (is.null(mat) || !is.matrix(mat)) {
    stop(sprintf(
      "Echec %s : une matrice est requise (recu : %s).",
      context, if (is.null(mat)) "NULL" else paste(class(mat), collapse = "/")
    ), call. = FALSE)
  }
  if (!is.numeric(mat)) {
    stop(sprintf(
      "Echec %s : la matrice doit etre de mode numerique (mode actuel : %s).",
      context, mode(mat)
    ), call. = FALSE)
  }
  invisible(mat)
}

#' Verifier la table de mapping echantillons -> condition (Bulk)
#'
#' Garde STRUCTURELLE (existence de la colonne, absence de NA) — les
#' problemes statistiques (replicats, confondance) relevent de
#' validate_bulk_design(), deja couverte par tests/testthat/test-bulk-helpers.R.
#'
#' @param metadata data.frame de metadonnees echantillons.
#' @param sample_col Nom de la colonne cle (condition/batch).
#' @param context Contexte d'appel.
#' @return Le data.frame, invisible.
assert_sample_mapping <- function(metadata, sample_col, context = "analyse") {
  if (is.null(metadata) || !is.data.frame(metadata)) {
    stop(sprintf(
      "Echec %s : un data.frame de metadonnees est requis (recu : %s).",
      context, if (is.null(metadata)) "NULL" else paste(class(metadata), collapse = "/")
    ), call. = FALSE)
  }
  if (!sample_col %in% colnames(metadata)) {
    stop(sprintf(
      "Echec %s : la colonne '%s' est introuvable dans les metadonnees (disponibles : %s).",
      context, sample_col, paste(colnames(metadata), collapse = ", ")
    ), call. = FALSE)
  }
  n_na <- sum(is.na(metadata[[sample_col]]))
  if (n_na > 0) {
    stop(sprintf(
      "Echec %s : %d valeur(s) manquante(s) (NA) dans '%s'.",
      context, n_na, sample_col
    ), call. = FALSE)
  }
  invisible(metadata)
}

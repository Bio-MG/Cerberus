# =============================================================================
# helpers_bulk.R — Bulk RNA-seq differential expression engine + plots
# =============================================================================
# Extracted from global.R (refactor, session post-v1.0 Bulk) — pure functions
# used by the mod_bulk_* family (mod_bulk_filter.R, mod_bulk_de.R,
# mod_bulk_report.R). No Shiny reactivity here.
#
# Contents:
#   - Filtering        : filter_bulk_counts()
#   - DESeq2 engine     : build_dds(), check_design_confounding(),
#                         validate_bulk_design(), extract_deseq2_contrast()
#   - Fallback engines  : run_edger_de(), run_limma_voom_de(),
#                         run_bulk_de_dispatch(), .normalize_de_cols()
#   - Plots             : get_vst_matrix(), plot_bulk_pca(), plot_volcano_bulk(),
#                         plot_ma_bulk(), plot_heatmap_bulk(),
#                         plot_sample_correlation_heatmap()
#   - DT tables         : build_de_results_dt()
#
# Pathway enrichment (ORA/GSEA) is shared with single-cell — see
# helpers_pathway.R, NOT duplicated here even though it's used by
# mod_bulk_pathways.R too.
# Depends on: DESeq2/edgeR/limma (one of, checked via has_deseq2/has_edger/
# has_limma flags set in global.R), ComplexHeatmap (optional, ggplot fallback),
# ggplot2, dplyr.
# =============================================================================







#' Filter low-count genes from a bulk counts matrix

#'

#' @param counts_matrix Numeric matrix, genes in rows, samples in columns.

#' @param min_count Minimum total count across all samples to keep a gene.

#' @param min_samples Minimum number of samples reaching `min_count_per_sample`.

#' @param min_count_per_sample Per-sample count threshold used with `min_samples`.

#' @return Filtered numeric matrix (same column order, fewer rows).

filter_bulk_counts <- function(counts_matrix, min_count = 10,

                                min_samples = 1, min_count_per_sample = 1) {

  keep_total <- rowSums(counts_matrix, na.rm = TRUE) >= min_count

  keep_n     <- rowSums(counts_matrix >= min_count_per_sample, na.rm = TRUE) >= min_samples

  out <- counts_matrix[keep_total & keep_n, , drop = FALSE]

  if (nrow(out) == 0) stop(errorCondition("Aucun gène ne passe le filtre — seuils trop stricts.", class = "bulk_de_error"))

  out

}



#' Build a DESeqDataSet and (optionally) fit DESeq()

#'

#' @param counts_matrix Integer-like counts matrix (genes x samples).

#' @param metadata Sample metadata; rownames must match colnames(counts_matrix).

#' @param design_formula Character, e.g. "~ condition" or "~ batch + condition".

#' @param run_deseq Logical — if FALSE, returns the unfit DDS (used for blind VST).

#' @return A DESeqDataSet (fit or unfit depending on `run_deseq`).

build_dds <- function(counts_matrix, metadata, design_formula = "~ condition",

                       run_deseq = TRUE, allow_non_integer = FALSE) {

  if (!requireNamespace("DESeq2", quietly = TRUE)) {

    stop(errorCondition("Le package 'DESeq2' est requis pour cette analyse.", class = "bulk_de_error"))

  }

  common <- intersect(colnames(counts_matrix), rownames(metadata))

  if (length(common) < 2) {

    stop(errorCondition("Moins de 2 échantillons communs entre la matrice de counts et les métadonnées.", class = "bulk_de_error"))

  }

  counts_matrix <- counts_matrix[, common, drop = FALSE]

  metadata      <- metadata[common, , drop = FALSE]



  # Convert character design columns to factor explicitly, BEFORE

  # DESeqDataSetFromMatrix — silences the "converting to factors" warning

  # and gives us control over level ordering (first level = reference).

  design_terms <- all.vars(stats::as.formula(design_formula))

  for (term in design_terms) {

    if (term %in% colnames(metadata) && is.character(metadata[[term]])) {

      metadata[[term]] <- factor(metadata[[term]])

    }

  }



  if (any(counts_matrix < 0, na.rm = TRUE)) {

    stop(errorCondition("La matrice de counts contient des valeurs négatives — DESeq2 requiert des counts bruts.", class = "bulk_de_error"))

  }

  # PLOT-S6c (P0, audit §2an) — GARDE DE COUNTS ENFIN CÂBLÉE SUR LE CHEMIN DE.
  # Avant : build_dds() arrondissait les valeurs non entières avec un simple
  # warning, et les branches edgeR/limma le faisaient SANS aucun avertissement
  # (`DGEList(counts = round(...))`) : les chiffres analysés n'étaient plus
  # ceux fournis. La garde canonique du dépôt (domaine correction de batch)
  # refuse désormais par défaut ; l'arrondi reste possible mais doit être
  # DÉCLARÉ (allow_non_integer = TRUE) — et il est alors annoncé ci-dessous.
  bulk_assert_raw_counts(counts_matrix, "analyse différentielle (counts)",
                         allow_non_integer = allow_non_integer)

  if (any(counts_matrix != round(counts_matrix), na.rm = TRUE)) {

    warning("Valeurs non-entières détectées — arrondi automatique (DESeq2 requiert des counts entiers).")

    counts_matrix <- round(counts_matrix)

  }



  dds <- DESeq2::DESeqDataSetFromMatrix(

    countData = counts_matrix,

    colData   = metadata,

    design    = stats::as.formula(design_formula)

  )

  if (run_deseq) dds <- DESeq2::DESeq(dds, quiet = TRUE)

  dds

}



# CHRYSALIS PHASE — canonical guards MOVED to R/core/validation.R.
# This file NO LONGER defines check_design_confounding / validate_bulk_design.
# They are re-exported here via a thin shim that sources the canonical file
# on demand, avoiding a 15+ line duplication that would trip
# tests/testthat/test-check_duplication.R. In production app.R already
# sourced R/core/validation.R BEFORE this file, so the shim is a no-op.
if (!exists("check_design_confounding", envir = .GlobalEnv) ||
    !exists("validate_bulk_design", envir = .GlobalEnv)) {
  # Resolve project root relative to this file's location (works both in app.R
  # and when test-bulk-helpers.R sources this file directly via helper-source.R)
  .bulk_helpers_validation_candidates <- c(
    "R/core/validation.R",
    file.path(dirname(dirname(normalizePath("R/bulk/bulk_helpers.R", mustWork = FALSE))), "R", "core", "validation.R"),
    file.path(getwd(), "R", "core", "validation.R")
  )
  for (.p in .bulk_helpers_validation_candidates) {
    if (file.exists(.p)) { source(.p, local = FALSE); break }
  }
}



#' Extract a (shrunk) contrast result table from a fitted DESeqDataSet

#'

#' @param dds Fitted DESeqDataSet (post-DESeq()).

#' @param condition_col Column name used for the contrast.

#' @param group_target Numerator level ("treatment").

#' @param group_ref Denominator level ("reference").

#' @param shrink Apply apeglm/normal LFC shrinkage when possible.

#' @param alpha Significance threshold passed to results().

#' @param p_adjust_method Méthode de correction pour tests multiples ("BH",
#'   "BY", "bonferroni", "holm", ... — voir stats::p.adjust.methods).
#'   Défaut "BH" : comportement historique strictement inchangé.

#' @return data.frame: gene, baseMean, log2FoldChange, pvalue, padj, ...

extract_deseq2_contrast <- function(dds, condition_col, group_target, group_ref,

                                     shrink = TRUE, alpha = 0.05,
                                     p_adjust_method = "BH") {

  # STAT-Q1 : la correction pour tests multiples est appliquée ICI, par
  # results(). Conséquence voulue : re-appeler cette fonction sur un dds DÉJÀ
  # ajusté suffit à recalculer padj avec une autre méthode SANS relancer
  # DESeq() — c'est le mécanisme du recalcul « live » de l'onglet DE.
  p_adjust_method <- match.arg(p_adjust_method, stats::p.adjust.methods)

  contrast_vec <- c(condition_col, group_target, group_ref)

  res <- DESeq2::results(dds, contrast = contrast_vec, alpha = alpha,
                         pAdjustMethod = p_adjust_method)



  if (shrink) {

    coef_name    <- paste0(condition_col, "_", group_target, "_vs_", group_ref)

    avail_coefs  <- DESeq2::resultsNames(dds)

    res <- tryCatch({

      if (coef_name %in% avail_coefs) {

        DESeq2::lfcShrink(dds, coef = coef_name, res = res, type = "apeglm", quiet = TRUE)

      } else {

        DESeq2::lfcShrink(dds, contrast = contrast_vec, res = res, type = "normal", quiet = TRUE)

      }

    }, error = function(e) res)  # fallback silencieux sur le résultat non-shrunk

  }



  out <- as.data.frame(res)

  out$gene <- rownames(out)

  out <- out[order(out$padj), ]

  rownames(out) <- NULL

  out

}



#' Build the edgeR contrast vector for "group_target vs group_ref"
#'
#' The design is always `~ grp_keep` (+ optional covariates), so a level is
#' either a column named `grp_keep<level>` or — when it is the FIRST level —
#' the intercept, which has no column. Both cases are handled: a level sitting
#' on the intercept simply contributes 0 to the contrast, and the other one
#' carries the sign.
#'
#' @param design Model matrix (from `stats::model.matrix(~ grp_keep, ...)`).
#' @param group_target Character, target level.
#' @param group_ref Character, reference level.
#' @return Numeric vector of length `ncol(design)`.
.edger_contrast_vector <- function(design, group_target, group_ref) {
  nm <- colnames(design)
  .find_level_col <- function(level) {
    cand <- c(make.names(paste0("grp_keep", level)), paste0("grp_keep", level))
    for (c in cand) {
      hit <- which(nm == c)
      if (length(hit) == 1L) return(hit)
    }
    NA_integer_
  }
  i_target <- .find_level_col(group_target)
  i_ref    <- .find_level_col(group_ref)
  if (is.na(i_target) && is.na(i_ref)) {
    stop(errorCondition(
      sprintf("Contraste edgeR introuvable : '%s' / '%s' absents des colonnes du design (%s).",
              group_target, group_ref, paste(nm, collapse = ", ")),
      class = "bulk_de_error", state = "invalid_contrast"))
  }
  v <- rep(0, ncol(design))
  if (!is.na(i_target)) v[i_target] <- v[i_target] + 1
  if (!is.na(i_ref))    v[i_ref]    <- v[i_ref]    - 1
  if (all(v == 0)) {
    stop(errorCondition(
      sprintf("Contraste edgeR nul : '%s' et '%s' désignent le même niveau.", group_target, group_ref),
      class = "bulk_de_error", state = "invalid_contrast"))
  }
  v
}

#' edgeR fallback differential expression (2-group comparison)
#'
#' @param fixed_dispersion Optional numeric scalar > 0. When supplied, the model
#'   is fitted with this dispersion IMPOSED instead of estimated
#'   (`glmFit(dispersion = ...)` + `glmLRT(contrast = ...)`), and ALL levels of
#'   `condition_col` are kept in the design instead of only the two compared
#'   groups. This is the "no-replicate exploratory mode": it exists because a
#'   saturated design (`n <= p`, see `design_saturation()`) has no residual
#'   degrees of freedom, so `estimateDisp()` returns NA and `glmQLFit()` dies.
#'   The resulting p-values are conditional on the dispersion the caller chose —
#'   they are NOT inferential. Measured 2026-09-17 on 4 samples x 4 conditions:
#'   the same data gives 1197 / 2 / 0 genes at FDR<0.05 for BCV 0.1 / 0.4 / 0.8.
run_edger_de <- function(counts_matrix, metadata, condition_col, group_target, group_ref,
                         covariates = character(0), p_adjust_method = "BH",
                         fixed_dispersion = NULL) {

  p_adjust_method <- match.arg(p_adjust_method, stats::p.adjust.methods)

  if (!requireNamespace("edgeR", quietly = TRUE)) stop(errorCondition("Package 'edgeR' requis.", class = "bulk_de_error"))

  impose_disp <- !is.null(fixed_dispersion)
  if (impose_disp &&
      (!is.numeric(fixed_dispersion) || length(fixed_dispersion) != 1L ||
       !is.finite(fixed_dispersion) || fixed_dispersion <= 0)) {
    stop(errorCondition(
      sprintf("run_edger_de() : fixed_dispersion doit être un scalaire numérique fini > 0 (reçu : %s).",
              paste(format(fixed_dispersion), collapse = ", ")),
      class = "bulk_de_error", state = "invalid_input"))
  }

  common <- intersect(colnames(counts_matrix), rownames(metadata))

  counts_matrix <- counts_matrix[, common, drop = FALSE]

  metadata      <- metadata[common, , drop = FALSE]



  # Mode dispersion imposée : on garde TOUS les niveaux pour que la
  # normalisation (calcNormFactors) et l'ajustement portent sur tous les
  # échantillons disponibles ; le contraste voulu est extrait ensuite.
  # Mode normal : on se limite aux 2 groupes comparés (comportement historique).
  grp  <- if (impose_disp) {
    factor(metadata[[condition_col]])
  } else {
    factor(metadata[[condition_col]], levels = c(group_ref, group_target))
  }

  covariates <- intersect(covariates, colnames(metadata))

  keep <- !is.na(grp)

  for (cov in covariates) keep <- keep & !is.na(metadata[[cov]])

  if (sum(keep) < if (impose_disp) 2L else 4L) {
    stop(errorCondition(sprintf("Trop peu d'échantillons valides pour edgeR (minimum %s).",
                 if (impose_disp) "2 en mode dispersion imposée" else "4 recommandé"), class = "bulk_de_error"))
  }



  meta_keep <- droplevels(metadata[keep, , drop = FALSE])

  grp_keep  <- droplevels(grp[keep])

  design <- if (length(covariates) > 0) {

    stats::model.matrix(stats::as.formula(paste("~ grp_keep +", paste(covariates, collapse = " + "))),

                        data = data.frame(grp_keep = grp_keep, meta_keep[, covariates, drop = FALSE]))

  } else stats::model.matrix(~grp_keep)



  y      <- edgeR::DGEList(counts = round(counts_matrix[, keep, drop = FALSE]), group = grp_keep)

  y      <- edgeR::calcNormFactors(y)

  # Deux chemins MUTUELLEMENT EXCLUSIFS — ne jamais les mélanger :
  #   - normal        : dispersion ESTIMEE (estimateDisp) puis test QL.
  #   - imposée (n=p) : dispersion FOURNIE, pas d'estimation possible, test LRT
  #     sur un contraste explicite (coef = 2 n'a pas de sens quand le design
  #     porte tous les niveaux).
  if (impose_disp) {

    fit    <- edgeR::glmFit(y, design, dispersion = fixed_dispersion)

    qlf    <- edgeR::glmLRT(fit, contrast = .edger_contrast_vector(design, group_target, group_ref))

  } else {

    y      <- edgeR::estimateDisp(y, design)

    fit    <- edgeR::glmQLFit(y, design)

    qlf    <- edgeR::glmQLFTest(fit, coef = 2)

  }



  res <- edgeR::topTags(qlf, n = Inf, adjust.method = p_adjust_method)$table

  res$gene <- rownames(res)

  colnames(res)[colnames(res) == "logFC"]  <- "log2FoldChange"

  colnames(res)[colnames(res) == "PValue"] <- "pvalue"

  # edgeR nomme la colonne ajustée "FDR" pour BH/BY mais "FWER" pour
  # bonferroni/holm/hochberg/hommel (vérifié empiriquement le 2026-09-10).
  # Les deux doivent être ramenées à "padj" : sinon .normalize_de_cols()
  # ajouterait une colonne padj entièrement NA et le résultat non-BH
  # paraîtrait silencieusement « non testé ».
  colnames(res)[colnames(res) == "FDR"]    <- "padj"

  colnames(res)[colnames(res) == "FWER"]   <- "padj"

  res[order(res$padj), ]

}



#' limma-voom fallback differential expression (2-group comparison)

run_limma_voom_de <- function(counts_matrix, metadata, condition_col, group_target, group_ref,
                              covariates = character(0), p_adjust_method = "BH") {

  # STAT-Q1 : limma::topTable(adjust.method=) accepte toute valeur de
  # stats::p.adjust.methods ; la colonne ajustée reste "adj.P.Val" quel que
  # soit le choix (contrairement à edgeR, voir run_edger_de()).
  p_adjust_method <- match.arg(p_adjust_method, stats::p.adjust.methods)

  missing_pkgs <- c(

    if (!requireNamespace("limma", quietly = TRUE)) "limma",

    if (!requireNamespace("edgeR", quietly = TRUE)) "edgeR"

  )

  if (length(missing_pkgs) > 0) {

    stop(errorCondition(paste0("Package(s) manquant(s) pour limma-voom : ", paste(missing_pkgs, collapse = ", "),

         ". Vérifiez .libPaths() — installé mais peut-être dans une autre librairie R."), class = "bulk_de_error"))

  }

  common <- intersect(colnames(counts_matrix), rownames(metadata))

  counts_matrix <- counts_matrix[, common, drop = FALSE]

  metadata      <- metadata[common, , drop = FALSE]



  grp  <- factor(metadata[[condition_col]], levels = c(group_ref, group_target))

  covariates <- intersect(covariates, colnames(metadata))

  keep <- !is.na(grp)

  for (cov in covariates) keep <- keep & !is.na(metadata[[cov]])

  if (sum(keep) < 4) stop(errorCondition("Trop peu d'échantillons valides pour limma-voom (minimum 4 recommandé).", class = "bulk_de_error"))



  meta_keep <- droplevels(metadata[keep, , drop = FALSE])

  grp_keep  <- droplevels(grp[keep])

  design <- if (length(covariates) > 0) {

    stats::model.matrix(stats::as.formula(paste("~ grp_keep +", paste(covariates, collapse = " + "))),

                        data = data.frame(grp_keep = grp_keep, meta_keep[, covariates, drop = FALSE]))

  } else stats::model.matrix(~grp_keep)



  y      <- edgeR::DGEList(counts = round(counts_matrix[, keep, drop = FALSE]))

  y      <- edgeR::calcNormFactors(y)

  v      <- limma::voom(y, design)

  fit    <- limma::eBayes(limma::lmFit(v, design))



  res <- limma::topTable(fit, coef = 2, number = Inf, sort.by = "P",
                         adjust.method = p_adjust_method)

  res$gene <- rownames(res)

  colnames(res)[colnames(res) == "logFC"]     <- "log2FoldChange"

  colnames(res)[colnames(res) == "P.Value"]   <- "pvalue"

  colnames(res)[colnames(res) == "adj.P.Val"] <- "padj"

  res[order(res$padj), ]

}



#' Dispatch DE engine call

run_bulk_de_dispatch <- function(engine, counts_matrix, metadata, condition_col,

                                  group_target, group_ref, dds = NULL, shrink = TRUE,

                                  covariates = character(0), p_adjust_method = "BH",

                                  allow_non_integer = FALSE, fixed_dispersion = NULL) {

  # PLOT-S6c (P0, audit §2an) — les moteurs edgeR et limma faisaient
  # `round(counts)` SANS le moindre avertissement (DGEList(counts = round(...))).
  # La garde est posée ici, une seule fois, pour les trois moteurs : le moteur
  # deseq2 consomme un dds déjà construit par build_dds(), qui applique la même
  # garde — les deux chemins sont donc couverts.
  bulk_assert_raw_counts(counts_matrix, paste0("analyse différentielle (", engine, ")"),
                         allow_non_integer = allow_non_integer)

  switch(engine,

    deseq2 = {

      if (is.null(dds)) stop(errorCondition("DESeqDataSet manquant pour le moteur DESeq2.", class = "bulk_de_error"))

      extract_deseq2_contrast(dds, condition_col, group_target, group_ref, shrink = shrink,
                              p_adjust_method = p_adjust_method)

    },

    edger = run_edger_de(counts_matrix, metadata, condition_col, group_target, group_ref,
                         covariates = covariates, p_adjust_method = p_adjust_method,
                         fixed_dispersion = fixed_dispersion),

    limma = run_limma_voom_de(counts_matrix, metadata, condition_col, group_target, group_ref, covariates = covariates, p_adjust_method = p_adjust_method),

    stop(errorCondition(paste0("Moteur DE non supporté : ", engine), class = "bulk_de_error"))

  )

}



#' Normalize DE result column names + fill baseMean if missing (edgeR/limma)

.normalize_de_cols <- function(df, counts_for_basemean = NULL) {

  if (is.null(df) || nrow(df) == 0) return(df)

  if (!"gene" %in% colnames(df))           df$gene <- rownames(df)

  if (!"log2FoldChange" %in% colnames(df)) df$log2FoldChange <- NA_real_

  if (!"pvalue" %in% colnames(df))         df$pvalue <- NA_real_

  if (!"padj" %in% colnames(df))           df$padj   <- NA_real_

  if (!"baseMean" %in% colnames(df)) {

    df$baseMean <- if (!is.null(counts_for_basemean)) {

      rowMeans(counts_for_basemean[df$gene, , drop = FALSE], na.rm = TRUE)

    } else NA_real_

  }

  df

}



#' Run the SAME contrast through DESeq2 + edgeR + limma-voom (when available)
#'
#' Multi-method comparison helper: dispatches the same group_target vs
#' group_ref contrast across all available engines and returns normalized
#' results per method, so downstream code can compute rank consensus or
#' compare significant gene sets via Venn/UpSet across METHODS (as opposed
#' to across CONTRASTS — see `build_contrast_gene_sets()` for that). Reuses
#' the exact same `run_bulk_de_dispatch()` + `.normalize_de_cols()` already
#' used by the single-engine path, so results are byte-for-byte identical
#' to running each engine individually — no parallel code path to drift.
#' limma-voom is skipped if `has_limma` is FALSE (flag set in global.R), or
#' silently dropped if its fit errors (e.g. too few replicates).
#'
#' @param counts_matrix Filtered counts matrix (genes x samples).
#' @param metadata Sample metadata data.frame.
#' @param condition_col Name of the grouping column.
#' @param group_target,group_ref Levels of `condition_col` being compared.
#' @param dds_full Pre-built DESeqDataSet (real design) — reused as-is.
#' @param shrink Passed to the DESeq2 engine only (apeglm LFC shrinkage).
#' @param p_adjust_method Multiple-testing correction passed to EVERY engine
#'   (DESeq2 results(), edgeR topTags(), limma topTable()). Default "BH" —
#'   behaviour strictly unchanged for existing callers.
#' @return Named list of normalized data.frames: any of `deseq2`/`edger`/
#'   `limma` that succeeded (length >= 1; methods that errored are omitted,
#'   with a `warning()` raised for each — caller decides whether to surface it).
getAllDE <- function(counts_matrix, metadata, condition_col, group_target, group_ref,
                     dds_full, shrink = TRUE, covariates = character(0),
                     p_adjust_method = "BH") {
  out <- list()

  out$deseq2 <- tryCatch({
    res <- run_bulk_de_dispatch("deseq2", counts_matrix, metadata, condition_col,
                                group_target, group_ref, dds = dds_full, shrink = shrink,
                                p_adjust_method = p_adjust_method)
    .normalize_de_cols(res, counts_for_basemean = counts_matrix)
  }, error = function(e) { warning("DESeq2 a échoué : ", conditionMessage(e)); NULL })

  out$edger <- tryCatch({
    res <- run_bulk_de_dispatch("edger", counts_matrix, metadata, condition_col,
                                group_target, group_ref, covariates = covariates,
                                p_adjust_method = p_adjust_method)
    .normalize_de_cols(res, counts_for_basemean = counts_matrix)
  }, error = function(e) { warning("edgeR a échoué : ", conditionMessage(e)); NULL })

  if (isTRUE(has_limma)) {
    out$limma <- tryCatch({
      res <- run_bulk_de_dispatch("limma", counts_matrix, metadata, condition_col,
                                  group_target, group_ref, covariates = covariates,
                                  p_adjust_method = p_adjust_method)
      .normalize_de_cols(res, counts_for_basemean = counts_matrix)
    }, error = function(e) { warning("limma-voom a échoué : ", conditionMessage(e)); NULL })
  }

  out[!vapply(out, is.null, logical(1))]
}

#' Rank-aggregation consensus across DE methods (mean-rank, dependency-free)
#'
#' For each gene tested by ALL supplied methods, ranks genes by p-value
#' WITHIN each method (rank 1 = most significant), then averages the
#' per-method ranks — a simple, transparent, well-understood aggregation
#' scheme (equivalent to a Borda count), deliberately NOT using an extra
#' package (e.g. RobustRankAggreg) per the project's RAM/CPU-only /
#' minimal-dependency guidance. Genes a given method didn't report at all
#' (NA padj — can happen with DESeq2's automatic outlier filtering) are
#' excluded from the common gene set across methods.
#'
#' @param de_list Named list of normalized DE data.frames — typically the
#'   output of `getAllDE()`. Must contain >= 2 methods.
#' @param lfc_thresh,padj_thresh Significance thresholds, used only to
#'   compute `n_methods_sig` / `consistent_sign` — does NOT filter rows out.
#' @return data.frame ordered by `mean_rank` ascending (most consistently
#'   significant first), with columns: gene, mean_rank, n_methods_sig,
#'   mean_log2FC, consistent_sign, plus per-method log2FC_<m>/padj_<m>/rank_<m>.
rankConsensus <- function(de_list, lfc_thresh = 1, padj_thresh = 0.05) {
  de_list <- de_list[!vapply(de_list, is.null, logical(1))]
  if (length(de_list) < 2) stop(errorCondition("Au moins 2 méthodes requises pour un consensus de rang.", class = "bulk_de_error"))

  common_genes <- Reduce(intersect, lapply(de_list, function(d) d$gene[!is.na(d$padj)]))
  if (length(common_genes) == 0) {
    stop(errorCondition("Aucun gène commun (avec p-adj valide) entre les méthodes sélectionnées.", class = "bulk_de_error"))
  }

  per_method <- lapply(names(de_list), function(m) {
    d <- de_list[[m]]
    d <- d[match(common_genes, d$gene), c("gene", "log2FoldChange", "padj")]
    d$rank <- rank(d$padj, ties.method = "average", na.last = "keep")
    d$sig  <- !is.na(d$padj) & d$padj < padj_thresh & abs(d$log2FoldChange) > lfc_thresh
    setNames(d, c("gene", paste0("log2FC_", m), paste0("padj_", m), paste0("rank_", m), paste0("sig_", m)))
  })

  merged <- Reduce(function(a, b) merge(a, b, by = "gene"), per_method)

  rank_cols <- grep("^rank_",   colnames(merged))
  sig_cols  <- grep("^sig_",    colnames(merged))
  lfc_cols  <- grep("^log2FC_", colnames(merged))

  merged$mean_rank     <- rowMeans(merged[, rank_cols, drop = FALSE], na.rm = TRUE)
  merged$n_methods_sig <- rowSums(merged[, sig_cols, drop = FALSE], na.rm = TRUE)
  merged$mean_log2FC   <- rowMeans(merged[, lfc_cols, drop = FALSE], na.rm = TRUE)
  merged$consistent_sign <- apply(merged[, lfc_cols, drop = FALSE], 1, function(x) {
    x <- x[!is.na(x)]
    length(x) > 0 && (all(x > 0) || all(x < 0))
  })

  merged[order(merged$mean_rank), ]
}


#' Variance-stabilizing matrix for PCA/heatmap (falls back to log2 for small n)

get_vst_matrix <- function(dds) {

  # Guard 1: too few samples for blind dispersion estimation.

  if (ncol(dds) < 4) {

    return(log2(DESeq2::counts(dds, normalized = TRUE) + 1))

  }

  # Guard 2: vst() requires >= nsub genes (default 1000) for the

  # subsampling-based dispersion fit. Small gene panels (e.g. targeted

  # capture panels, custom panels < 1000 genes) must use the direct

  # variance-stabilizing transformation instead (slower, but correct

  # regardless of gene count — no subsampling involved).

  nsub <- min(1000, nrow(dds))

  if (nrow(dds) < 1000) {

    vsd <- tryCatch(

      DESeq2::varianceStabilizingTransformation(dds, blind = TRUE),

      error = function(e) {

        warning("VST direct a échoué (", conditionMessage(e),

                "), repli sur log2(counts normalisés + 1).")

        NULL

      }

    )

    if (is.null(vsd)) return(log2(DESeq2::counts(dds, normalized = TRUE) + 1))

    return(SummarizedExperiment::assay(vsd))

  }

  vsd <- DESeq2::vst(dds, blind = TRUE, nsub = nsub)

  SummarizedExperiment::assay(vsd)

}



# -----------------------------------------------------------------------------
# Palette helpers REMOVED (generalisation) : bulk_color_scale(),
# .default_manual_colors(), manual_color_picker_ui(), bulk_annotation_colors()
# et bulk_role_colors() vivaient en DOUBLE ici et dans R/palettes.R. app.R
# source R/palettes.R APRES ce fichier, donc la copie de palettes.R ecrasait
# silencieusement celle-ci a chaque lancement. Une seule source de verite
# desormais (R/palettes.R), etendue aux resolvers partages SC/Bulk/Spatial :
# diverging_ramp_colors() / sequential_ramp_colors() / role_colors_generic().
# -----------------------------------------------------------------------------


#' Safe plot render guard — "figure margins too large" (Bug C)
.safe_plot_render <- function(session, output_id, plot_fn, min_px = 30) {
  w <- session$clientData[[paste0("output_", output_id, "_width")]]
  h <- session$clientData[[paste0("output_", output_id, "_height")]]
  if (isTRUE(w < min_px) || isTRUE(h < min_px)) {
    grid::grid.newpage()
    grid::grid.text("Redimensionnement en cours...", gp = grid::gpar(col = "grey40", fontsize = 11))
    return(invisible(NULL))
  }
  tryCatch(plot_fn(), error = function(e) {
    if (grepl("figure margins too large", conditionMessage(e))) {
      grid::grid.newpage()
      grid::grid.text("Conteneur trop petit.", gp = grid::gpar(col = "grey40", fontsize = 11))
      return(invisible(NULL))
    }
    stop(e)
  })
}


#' Bulk PCA plot colored/shaped by metadata

plot_bulk_pca <- function(vst_matrix, metadata, color_by = NULL, shape_by = NULL, ntop = 500,
                          palette = "default", manual_colors = NULL,
                          theme_choice = TS_THEME_DEFAULT, base_size = TS_BASE_SIZE_DEFAULT, tr = NULL) {

  tr <- tr %||% function(x) x

  rv   <- apply(vst_matrix, 1, var)

  ntop <- min(ntop, nrow(vst_matrix))

  sel  <- order(rv, decreasing = TRUE)[seq_len(ntop)]

  pca  <- prcomp(t(vst_matrix[sel, , drop = FALSE]), scale. = FALSE)

  pct  <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)



  df <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2], sample = colnames(vst_matrix))



  # Defensive normalisation: treat NULL/NA/""/length!=1 all the same way, and

  # never let an ambiguous condition reach an `if`/`&&` — R >= 4.3 errors hard

  # ("missing value where TRUE/FALSE needed" or "length(0) in coercion to

  # 'logical(1)'") on exactly this class of bug, e.g. `shape_by != color_by`

  # when one side is NULL, or a stray length-0/NA value slips through from

  # an upstream selectInput.

  .clean_scalar <- function(x) {

    if (is.null(x) || length(x) != 1 || is.na(x) || !nzchar(x)) NULL else x

  }

  color_by <- .clean_scalar(color_by)

  shape_by <- .clean_scalar(shape_by)



  has_color <- !is.null(color_by) && isTRUE(color_by %in% colnames(metadata))

  has_shape <- !is.null(shape_by) && isTRUE(shape_by %in% colnames(metadata)) &&

               isTRUE(is.null(color_by) || shape_by != color_by)



  if (has_color) df$color <- as.character(metadata[df$sample, color_by])

  if (has_shape) df$shape <- as.character(metadata[df$sample, shape_by])



  p <- ggplot(df, aes(x = PC1, y = PC2, label = sample))

  if (has_color && has_shape) {

    p <- p + geom_point(aes(color = color, shape = shape), size = 3.5, alpha = 0.85)

  } else if (has_color) {

    p <- p + geom_point(aes(color = color), size = 3.5, alpha = 0.85)

  } else {

    p <- p + geom_point(size = 3.5, alpha = 0.85, color = "#2C3E50")

  }

  if (has_color) {
    sc <- bulk_color_scale(palette, manual_colors)
    if (!is.null(sc)) p <- p + sc
  }

  p + geom_text(size = 3, vjust = -1, check_overlap = TRUE) +

    labs(title = tr("PCA — Échantillons Bulk RNA-seq"),

         x = paste0("PC1 (", pct[1], "%)"), y = paste0("PC2 (", pct[2], "%)"),

         color = if (has_color) color_by else NULL,

         shape = if (has_shape) shape_by else NULL) +

    ts_theme(theme_choice, base_size) +

    theme(plot.title = element_text(face = "bold", size = 14))

}

#' Volcano plot for bulk DE results
#'
#' @param engine Optional DE engine name (e.g. "DESeq2", "edgeR") surfaced in
#'   the statistical subtitle. NULL omits the engine from the subtitle.
plot_volcano_bulk <- function(res_df, lfc_thresh = 1, padj_thresh = 0.05, top_label = 10,
                              up_color = "#E74C3C", down_color = "#2980B9", ns_color = "#BDC3C7",
                              engine = NULL, theme_choice = TS_THEME_DEFAULT,
                              base_size = TS_BASE_SIZE_DEFAULT, tr = NULL) {

  tr <- tr %||% function(x) x

  res_df <- res_df[!is.na(res_df$padj), ]

  res_df$status <- dplyr::case_when(

    res_df$padj < padj_thresh & res_df$log2FoldChange >  lfc_thresh ~ "Up",

    res_df$padj < padj_thresh & res_df$log2FoldChange < -lfc_thresh ~ "Down",

    TRUE ~ "NS"

  )

  ord      <- order(res_df$padj)

  up_lbl   <- head(res_df$gene[ord][res_df$status[ord] == "Up"],   top_label)

  down_lbl <- head(res_df$gene[ord][res_df$status[ord] == "Down"], top_label)

  res_df$label <- ifelse(res_df$gene %in% c(up_lbl, down_lbl), res_df$gene, NA)

  # Statistical subtitle (PLOT-Q3) — counts reflect the LIVE thresholds passed
  # by the caller (reactive), so the subtitle doubles as the "which cutoffs?"
  # answer in a meeting. `engine` is optional: callers that know the DE engine
  # pass it; otherwise the subtitle simply omits it.
  n_up    <- sum(res_df$status == "Up")
  n_down  <- sum(res_df$status == "Down")
  n_tot   <- nrow(res_df)
  eng_fmt <- if (is.null(engine) || !nzchar(engine %||% "")) "%d Up · %d Down · %d testés"
             else sprintf("%%d Up · %%d Down · %%d testés — %s", engine)
  stat_subtitle <- sprintf(paste0(eng_fmt, " (padj<%.2g, |log2FC|>%.2g)"),
                           n_up, n_down, n_tot, padj_thresh, lfc_thresh)

  p <- ggplot(res_df, aes(x = log2FoldChange, y = -log10(padj), color = status)) +
    geom_point(alpha = 0.7, size = 1.6) +
    scale_color_manual(values = c(Up = up_color, Down = down_color, NS = ns_color)) +
    geom_vline(xintercept = c(-lfc_thresh, lfc_thresh), linetype = "dashed", color = "grey40") +
    geom_hline(yintercept = -log10(padj_thresh), linetype = "dashed", color = "grey40")

  # PLOT-Q1 — repel gene labels instead of overlapping geom_text(); the
  # >15-labelled-genes acceptance case is exactly the one geom_text() failed.
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    p <- p + ggrepel::geom_text_repel(aes(label = label), size = 2.8, na.rm = TRUE,
                                      max.overlaps = 20, box.padding = 0.4,
                                      segment.size = 0.3, show.legend = FALSE,
                                      min.segment.length = 0)
  } else {
    # Graceful degradation — ggrepel is declared in global.R required_packages;
    # if it is somehow absent the volcano must still render (no hard stop).
    p <- p + geom_text(aes(label = label), size = 2.8, na.rm = TRUE,
                       vjust = -0.6, show.legend = FALSE)
  }

  p +
    labs(title = tr("Volcano Plot — Analyse Différentielle"),
         subtitle = tr(stat_subtitle),
         x = tr("Log2 Fold Change"), y = tr("-Log10(P-adj)"), color = tr("Statut")) +
    ts_theme(theme_choice, base_size) +
    theme(plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(size = 10, color = "grey35"))

}



#' MA-plot for bulk DE results
#'
#' @param engine Optional DE engine name surfaced in the statistical subtitle.
plot_ma_bulk <- function(res_df, lfc_thresh = 1, padj_thresh = 0.05,
                         sig_color = "#E74C3C", ns_color = "#BDC3C7",
                         engine = NULL, theme_choice = TS_THEME_DEFAULT,
                         base_size = TS_BASE_SIZE_DEFAULT, tr = NULL) {

  tr <- tr %||% function(x) x

  res_df <- res_df[!is.na(res_df$padj) & !is.na(res_df$baseMean), ]

  res_df$sig <- res_df$padj < padj_thresh & abs(res_df$log2FoldChange) > lfc_thresh

  # Statistical subtitle (PLOT-Q3) — same Up/Down/tested + thresholds contract
  # as the volcano, so the two panels of the same contrast cannot disagree.
  n_sig   <- sum(res_df$sig)
  n_tot   <- nrow(res_df)
  eng_fmt <- if (is.null(engine) || !nzchar(engine %||% "")) "%d significatifs · %d testés"
             else sprintf("%%d significatifs · %%d testés — %s", engine)
  stat_subtitle <- sprintf(paste0(eng_fmt, " (padj<%.2g, |log2FC|>%.2g)"),
                           n_sig, n_tot, padj_thresh, lfc_thresh)

  ggplot(res_df, aes(x = log10(baseMean + 1), y = log2FoldChange, color = sig)) +

    geom_point(alpha = 0.6, size = 1.4) +

    scale_color_manual(values = c(`TRUE` = sig_color, `FALSE` = ns_color), guide = "none") +

    geom_hline(yintercept = 0, color = "grey30") +

    labs(title = tr("MA-Plot"), subtitle = tr(stat_subtitle),
         x = tr("Log10(Expression Moyenne + 1)"), y = tr("Log2 Fold Change")) +

    ts_theme(theme_choice, base_size) +

    theme(plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(size = 10, color = "grey35"))

}



#' Heatmap of selected genes (ComplexHeatmap if available, ggplot fallback otherwise)
#'
#' @param subtitle Optional statistical subtitle (PLOT-Q3) shown as a second
#'   column title line. Ignored by the ggplot fallback path.
#' @param clustering_distance,clustering_method PLOT-S4 — transmis au coeur
#'   unifie `ts_complex_heatmap()`. Les defauts ("euclidean"/"complete")
#'   reproduisent exactement le comportement anterieur (defauts de
#'   ComplexHeatmap).
#' @param k_row PLOT-S4 — NULL (defaut) = pas de decoupage. Entier >= 2 =
#'   decoupage du dendrogramme de lignes en k groupes.
plot_heatmap_bulk <- function(vst_matrix, genes, metadata, annotation_col = NULL, scale_rows = TRUE,

                              palette = "default", manual_colors = NULL, subtitle = NULL,
                              theme_choice = TS_THEME_DEFAULT, base_size = TS_BASE_SIZE_DEFAULT,
                              clustering_distance = "euclidean", clustering_method = "complete",
                              k_row = NULL, tr = NULL) {

  tr <- tr %||% function(x) x

  genes <- intersect(genes, rownames(vst_matrix))

  if (length(genes) < 2) stop(errorCondition("Au moins 2 gènes requis pour la heatmap.", class = "bulk_de_error"))

  mat <- vst_matrix[genes, , drop = FALSE]

  if (scale_rows) mat <- t(scale(t(mat)))



  if (requireNamespace("ComplexHeatmap", quietly = TRUE)) {

    col_meta <- NULL

    col_ann_colors <- NULL

    if (!is.null(annotation_col) && annotation_col %in% colnames(metadata)) {

      grp_vals   <- metadata[colnames(mat), annotation_col]

      ann_colors <- bulk_annotation_colors(grp_vals, palette, manual_colors)

      col_meta   <- list(group = grp_vals)

      if (!is.null(ann_colors)) col_ann_colors <- list(group = ann_colors)

    }

    # PLOT-S4 : delegue au coeur unifie (R/plotting/complex_heatmap.R).
    # - draw = TRUE : dessin explicite a marge sure, comme avant (le bug
    #   "figure margins too large" corrige ici en PLOT-Q n'est pas reintroduit).
    # - show_column_names = TRUE est EXPLICITE : le defaut du coeur applique la
    #   regle "<= 60", que ce site n'appliquait PAS (ComplexHeatmap : TRUE).
    # - `manual_colors` n'est PAS transmis a la rampe : le code historique ne
    #   l'utilise que pour l'annotation (via bulk_annotation_colors). Le passer
    #   changerait la rampe en mode "manual".
    return(ts_complex_heatmap(

      mat,

      name                  = tr("Z-score"),

      column_title          = tr("Heatmap — Gènes Différentiels"),

      col_meta              = col_meta,

      col_annotation_colors = col_ann_colors,

      ramp                  = "diverging",

      palette               = palette,

      show_row_names        = nrow(mat) <= 60,

      show_column_names     = TRUE,

      clustering_distance   = clustering_distance,

      clustering_method     = clustering_method,

      k_row                 = k_row,

      tr                    = tr

    ))

  }

  melted <- reshape2::melt(mat, varnames = c("gene", "sample"))

  ggplot(melted, aes(x = sample, y = gene, fill = value)) +

    geom_tile() +

    scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0, name = tr("Z-score")) +

    ts_theme(theme_choice, base_size) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

}



#' Sample-to-sample correlation heatmap (QC diagnostic, computed on VST matrix)

#'

#' High diagnostic value, near-zero extra cost: detects mislabeled samples,

#' batch outliers, or unexpected duplicates BEFORE the user spends time

#' interpreting a differential expression result built on bad input.

#' Reuses the already-computed VST matrix — no extra heavy computation.

#'

#' @param vst_matrix Matrix, VST-transformed expression (genes x samples).

#' @param metadata Sample metadata, for optional column annotation.

#' @param annotation_col Character, metadata column used to annotate samples (optional).

#' @param method Correlation method, "pearson" (default, standard for VST data) or "spearman".

#' @return A ComplexHeatmap object if available, otherwise a ggplot fallback.

plot_sample_correlation_heatmap <- function(vst_matrix, metadata = NULL,

                                             annotation_col = NULL, method = "pearson",

                                             palette = "default", manual_colors = NULL,
                                             theme_choice = TS_THEME_DEFAULT,
                                             base_size = TS_BASE_SIZE_DEFAULT, tr = NULL) {

  tr <- tr %||% function(x) x

  if (ncol(vst_matrix) < 2) stop(errorCondition("Au moins 2 échantillons requis pour la corrélation.", class = "bulk_de_error"))

  cor_mat <- cor(vst_matrix, method = method, use = "pairwise.complete.obs")



  if (requireNamespace("ComplexHeatmap", quietly = TRUE)) {

    col_meta <- NULL

    col_ann_colors <- NULL

    if (!is.null(metadata) && !is.null(annotation_col) && annotation_col %in% colnames(metadata)) {

      grp_vals   <- metadata[colnames(cor_mat), annotation_col]

      ann_colors <- bulk_annotation_colors(grp_vals, palette, manual_colors)

      col_meta   <- list(group = grp_vals)

      if (!is.null(ann_colors)) col_ann_colors <- list(group = ann_colors)

    }

    # PLOT-S4 : delegue au coeur unifie. draw = FALSE CONSERVE exactement le
    # comportement historique : l'objet Heatmap est rendu (pas dessine) et
    # c'est print(), chez l'appelant, qui le dessine.
    # ⚠️ ECART CONNU, volontairement PRESERVE (zero changement) : ce site n'est
    # donc PAS protege contre "figure margins too large", contrairement a
    # plot_heatmap_bulk()/build_sc_hierarchical_heatmap(). A corriger dans un
    # jalon dedie — cf. docs/contracts/PLOT_HEATMAP_CONTRACT.md §Ecart connu.
    return(ts_complex_heatmap(

      cor_mat,

      name                  = paste0(toupper(substring(method, 1, 1)), substring(method, 2)),

      column_title          = tr("Corrélation Inter-Échantillons (QC)"),

      col_meta              = col_meta,

      col_annotation_colors = col_ann_colors,

      ramp                  = "sequential",

      ramp_domain           = c(min(cor_mat), 1),

      palette               = palette,

      show_row_names        = TRUE,

      show_column_names     = TRUE,

      cell_fun = function(j, i, x, y, width, height, fill) {

        grid::grid.text(sprintf("%.2f", cor_mat[i, j]), x, y, gp = grid::gpar(fontsize = 8))

      },

      draw                  = FALSE,

      tr                    = tr

    ))

  }



  melted <- reshape2::melt(cor_mat, varnames = c("sample1", "sample2"))

  ggplot(melted, aes(x = sample1, y = sample2, fill = value)) +

    geom_tile() +

    geom_text(aes(label = sprintf("%.2f", value)), size = 2.8, color = "black") +

    scale_fill_gradient(low = "white", high = "#2C3E50", name = toupper(method)) +

    labs(title = tr("Corrélation Inter-Échantillons (QC)"), x = NULL, y = NULL) +

    ts_theme(theme_choice, base_size) +

    theme(axis.text.x = element_text(angle = 45, hjust = 1),

          plot.title  = element_text(face = "bold", size = 14))

}



#' Compte les genes exclus du test differentiel (STAT-Q3)

#'

#' DESeq2 exclut des genes du test sans que l'utilisateur le voie : `pvalue`

#' mise a NA pour les outliers de distance de Cook (un echantillon aberrant

#' domine l'ajustement) et pour les genes non exprimes (`baseMean` nul) ; `padj`

#' mise a NA par le filtrage independant (genes de trop faible expression pour

#' que le test ait une chance d'etre significatif APRES correction).

#'

#' L'information est deja presente dans l'objet DESeqResults — cette fonction ne

#' fait que la rendre lisible. Volontairement PURE (aucune dependance a

#' DESeq2 ni a Shiny) : elle travaille sur le data.frame de resultats deja

#' stocke dans `shared_rv$contrasts`, donc utilisable aussi bien par l'UI que par

#' le rapport.

#'

#' Distinction Cook / non exprime : un gene dont `baseMean > 0` et dont la

#' `pvalue` est NA a ete ecarte PAR le filtrage Cook ; un gene de `baseMean` nul

#' n'est pas un outlier, il n'etait simplement pas testable. Confondre les deux

#' ferait afficher « outliers Cook » pour des genes jamais exprimes.

#'

#' @param res_df data.frame de resultats DE (colonnes `pvalue`, `padj` ;

#'   `baseMean` optionnelle).

#' @return liste de compteurs entiers : `n_total`, `n_tested`, `n_excluded`

#'   (= `n_cooks` + `n_zero`), `n_cooks`, `n_zero`, `n_filtered`.

de_exclusion_counts <- function(res_df) {
  if (is.null(res_df) || nrow(res_df) == 0) {
    return(list(n_total = 0L, n_tested = 0L, n_excluded = 0L,
                n_cooks = 0L, n_zero = 0L, n_filtered = 0L))
  }

  pv <- res_df$pvalue
  bm <- if ("baseMean" %in% colnames(res_df)) res_df$baseMean else rep(NA_real_, nrow(res_df))
  pa <- if ("padj"     %in% colnames(res_df)) res_df$padj     else rep(NA_real_, nrow(res_df))

  na_p <- is.na(pv)

  # Cook : pvalue NA alors que le gene EST exprime.
  n_cooks <- sum(na_p & !is.na(bm) & bm > 0)
  # Non exprime : baseMean nul ou absent => hors test, pas un outlier.
  n_zero  <- sum(na_p & (is.na(bm) | bm == 0))
  # Filtrage independant : pvalue calculee mais padj retiree.
  n_filtered <- sum(!na_p & is.na(pa))

  list(n_total    = nrow(res_df),
       n_tested   = sum(!na_p),
       n_excluded = sum(na_p),
       n_cooks    = as.integer(n_cooks),
       n_zero     = as.integer(n_zero),
       n_filtered = as.integer(n_filtered))
}

#' Ecrit une table en Excel si possible, sinon en CSV (STAT-Q4)

#'

#' Export partage. `openxlsx` est OPTIONNEL : un downloadHandler ne doit jamais

#' echouer en silence, et un CSV s'ouvre parfaitement dans Excel. Mieux vaut donc

#' livrer un CSV qu'une erreur — c'est le comportement historique de l'export DE,

#' factorise ici pour etre reutilise par l'export pathways.

#'

#' L'EXTENSION du fichier n'est PAS geree ici : c'est l'appelant qui la choisit

#' dans son `filename =`, en testant `requireNamespace("openxlsx")` de la meme

#' facon. Ainsi le nom du fichier annonce toujours le format reellement ecrit.

#'

#' @param df data.frame a exporter. `NULL` est refuse (rien a ecrire) ; une table

#'   de 0 ligne est acceptee et produit un fichier vide mais valide.

#' @param file chemin de destination fourni par `downloadHandler`.

#' @return `file`, invisiblement.

write_table_excel_or_csv <- function(df, file) {
  if (is.null(df)) stop("Aucune table \u00e0 exporter.", call. = FALSE)
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    openxlsx::write.xlsx(df, file)
  } else {
    utils::write.csv(df, file, row.names = FALSE)
  }
  invisible(file)
}

#' Standardized DE results DT table (shared by mod_bulk.R Shiny render AND the

#' bulk HTML/PDF report). Centralizing this fixes a real bug: the report's

#' previous inline call to DT::datatable() omitted rownames = FALSE, so the

#' first column displayed auto-numbered row indices (1..n) instead of gene

#' symbols (which live in the "gene" column, since extract_deseq2_contrast()

#' resets rownames to NULL). One function, used everywhere, prevents this

#' divergence from reappearing.

#'

#' @param df DE results data.frame (must have gene, baseMean, log2FoldChange, pvalue, padj).
#'   An optional `lfcSE` column is surfaced as-is when present (DESeq2 only —
#'   STAT-Q2); edgeR/limma results simply render without it.

#' @return A DT::datatable object.

build_de_results_dt <- function(df) {

  df_display <- data.frame(

    Gene     = df$gene,

    BaseMean = round(df$baseMean, 1),

    Log2FC   = round(df$log2FoldChange, 3),

    stringsAsFactors = FALSE

  )

  # STAT-Q2 : lfcSE (erreur standard du log2FC) est fourni par DESeq2 — par
  # results() ET par lfcShrink() — mais il est ABSENT des sorties edgeR
  # (topTags) et limma (topTable). On ne l'affiche donc que lorsque la colonne
  # existe réellement : dégradation gracieuse, jamais une colonne entièrement
  # NA qui laisserait croire à un échec du calcul.
  if ("lfcSE" %in% colnames(df)) df_display[["lfcSE"]] <- round(df$lfcSE, 3)

  df_display[["PValue"]] <- format(df$pvalue, scientific = TRUE, digits = 3)

  df_display[["Padj"]]   <- format(df$padj,   scientific = TRUE, digits = 3)

  ts_datatable(df_display, page_length = 15L, filename_base = "bulk_degenes") %>%

    DT::formatStyle("Padj", color = DT::styleInterval(c(0.001, 0.01, 0.05),

                                                       c("darkgreen", "green", "orange", "red")))

}


# =============================================================================
# Multi-contrast comparison: Venn / UpSet (added post-v1.0, roadmap item #3)
# =============================================================================
# Compares the SIGNIFICANT-gene sets across 2+ contrasts already stored in
# shared_rv$contrasts (single-pair runs and/or pairwise-auto). Deliberately
# takes lfc_thresh/padj_thresh as PLAIN ARGUMENTS rather than reading
# shared_rv directly: the caller (mod_bulk_de.R) wraps this in a reactive()
# keyed on the live input$lfc_thresh/input$padj_thresh, so the sets are
# always recomputed against the CURRENT threshold — not frozen at whatever
# the threshold happened to be when each contrast was originally computed.
# Uses ComplexHeatmap::make_comb_mat()/UpSet() (already a hard dependency —
# NOT the separate "ComplexUpset" package, which adds nothing here and isn't
# in any install list). Venn uses the optional 'VennDiagram' package, only
# for 2-4 sets (unreadable beyond that — UpSet is the right tool there).

#' Build significant-gene sets from a named list of DE result data.frames
#'
#' @param contrasts Named list of data.frames (each needs log2FoldChange + padj,
#'   and either a "gene" column or rownames as gene identifiers).
#' @param lfc_thresh,padj_thresh Significance thresholds, evaluated NOW.
#' @param direction_aware If TRUE, splits each contrast into "<name> (Up)" /
#'   "<name> (Down)" sets instead of one set per contrast.
#' @return Named list of character vectors (gene IDs per set).
build_contrast_gene_sets <- function(contrasts, lfc_thresh = 1, padj_thresh = 0.05,
                                     direction_aware = FALSE) {
  stopifnot(length(contrasts) >= 2)
  sets <- list()
  for (nm in names(contrasts)) {
    df    <- contrasts[[nm]]
    genes <- if ("gene" %in% colnames(df)) df$gene else rownames(df)
    sig   <- !is.na(df$padj) & df$padj < padj_thresh & abs(df$log2FoldChange) > lfc_thresh
    if (direction_aware) {
      sets[[paste0(nm, " (Up)")]]   <- genes[sig & df$log2FoldChange > 0]
      sets[[paste0(nm, " (Down)")]] <- genes[sig & df$log2FoldChange < 0]
    } else {
      sets[[nm]] <- genes[sig]
    }
  }
  sets
}

#' UpSet plot from gene sets — works for any number of sets >= 2.
#' @return A drawn ComplexHeatmap object (call inside renderPlot, no extra draw() needed by caller besides this).
plot_upset_contrasts <- function(gene_sets, min_comb_size = 1) {
  if (length(unlist(gene_sets)) == 0) {
    stop(errorCondition("Aucun gène significatif dans les contrastes sélectionnés avec ces seuils.", class = "bulk_de_error"))
  }
  # Tighter margins lower the minimum device size plot.new()-based layout
  # needs before throwing "figure margins too large" (verified: default
  # ~200px floor vs ~50px with mar=c(1,1,1,1)) — restored on exit regardless
  # of success/failure so it never leaks into other plots on the same device.
  old_mar <- graphics::par("mar")
  on.exit(graphics::par(mar = old_mar), add = TRUE)
  graphics::par(mar = c(1, 1, 1, 1))

  m <- ComplexHeatmap::make_comb_mat(gene_sets)
  m <- m[ComplexHeatmap::comb_size(m) >= min_comb_size]
  ComplexHeatmap::draw(ComplexHeatmap::UpSet(
    m,
    comb_order       = order(-ComplexHeatmap::comb_size(m)),
    top_annotation   = ComplexHeatmap::upset_top_annotation(m, add_numbers = TRUE),
    right_annotation = ComplexHeatmap::upset_right_annotation(m, add_numbers = TRUE)
  ))
}

#' Venn diagram from gene sets — readable for 2 to 4 sets only.
#' @return invisible(NULL); draws directly onto the current grid device (call inside renderPlot).
plot_venn_contrasts <- function(gene_sets, palette = "default") {
  n <- length(gene_sets)
  if (n < 2 || n > 4) {
    stop(errorCondition(paste0("Le diagramme de Venn n'est lisible que pour 2 à 4 contrastes (vous en avez ",
        n, ") -- utilisez UpSet au-delà."), class = "bulk_de_error"))
  }
  venn_load_error <- tryCatch({
    loadNamespace("VennDiagram")
    NULL
  }, error = function(e) conditionMessage(e))
  if (!is.null(venn_load_error)) {
    stop(errorCondition(sprintf(
      paste0("Impossible de charger le package 'VennDiagram' : %s\n",
            "(VennDiagram dépend de 'futile.logger' — si VennDiagram est bien installe mais ",
            "cette erreur persiste, c'est probablement futile.logger/futile.options/lambda.r ",
            "qui manque, pas VennDiagram lui-même. Vérifiez aussi que .libPaths() dans cette ",
            "session R correspond bien à là où vous avez installé les packages.)"),
      venn_load_error
    ), class = "bulk_de_error"))
  }
  if (length(unlist(gene_sets)) == 0) {
    stop(errorCondition("Aucun gène significatif dans les contrastes sélectionnés avec ces seuils.", class = "bulk_de_error"))
  }
  # Same margin safety as plot_upset_contrasts() — NOTE: VennDiagram has no
  # "margin" parameter on venn.diagram()/draw.*.venn() (checked against the
  # actual installed package: passing one is silently swallowed by ... and
  # does nothing). par(mar=) on the device is the real, verified lever here.
  old_mar <- graphics::par("mar")
  on.exit(graphics::par(mar = old_mar), add = TRUE)
  graphics::par(mar = c(1, 1, 1, 1))

  # VennDiagram writes a stray "VennDiagram*.log" file to the working
  # directory on every call unless this logger is silenced first — long
  # documented quirk of the package, not specific to this app.
  if (requireNamespace("futile.logger", quietly = TRUE)) {
    futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
  }
  grid::grid.newpage()
  fill_cols <- if (identical(palette %||% "default", "default")) {
    grDevices::rainbow(n, alpha = 0.5)
  } else {
    cols <- bulk_annotation_colors(names(gene_sets), palette) %||% .default_manual_colors(n)
    scales::alpha(unname(cols)[seq_len(n)], 0.5)
  }
  v <- VennDiagram::venn.diagram(
    x = gene_sets, filename = NULL,
    fill = fill_cols,
    main = "", cex = 1, cat.cex = 0.9
  )
  grid::grid.draw(v)
  invisible(NULL)
}

#' Build a downloadable per-gene/per-intersection table from gene sets.
#' One row per gene, listing every set it belongs to and the intersection
#' code — lets the user actually pull "give me the genes in A AND B but not C".
#' @return data.frame(gene, sets, n_sets, comb_code)
build_contrast_intersection_dt <- function(gene_sets) {
  m <- ComplexHeatmap::make_comb_mat(gene_sets)
  combs <- ComplexHeatmap::comb_name(m)
  set_names <- names(gene_sets)

  rows <- lapply(combs, function(code) {
    genes <- ComplexHeatmap::extract_comb(m, code)
    if (length(genes) == 0) return(NULL)
    bits <- as.integer(strsplit(code, "")[[1]])
    sets_here <- paste(set_names[bits == 1], collapse = " & ")
    data.frame(gene = genes, sets = sets_here, n_sets = sum(bits),
              comb_code = code, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  if (is.null(out)) {
    return(data.frame(gene = character(0), sets = character(0),
                      n_sets = integer(0), comb_code = character(0)))
  }
  out[order(-out$n_sets, out$sets), ]
}


# =============================================================================
# Up/Down summary + Scree plot (BingleSeq-inspired additions, Step-2 session)
# =============================================================================

#' Per-contrast Up/Down/Significant summary table (active thresholds, live)
#'
#' Single source of truth for the "n_sig/n_up/n_down per contrast" summary,
#' shared by the HTML/PDF report (mod_bulk_report.R) and the "Resume Up/Down"
#' tab (mod_bulk_de.R) — avoids the two diverging the way the report-only
#' inline version used to, before this was extracted.
#'
#' @param contrasts Named list of DE result data.frames (log2FoldChange, padj).
#' @param lfc_thresh,padj_thresh Significance thresholds, evaluated NOW.
#' @param active_contrast Optional name flagged TRUE in the `actif` column.
#' @return data.frame(Contraste, n_testes, n_sig, n_up, n_down, actif)
summarize_contrasts_updown <- function(contrasts, lfc_thresh = 1, padj_thresh = 0.05,
                                       active_contrast = NULL) {
  contrasts <- contrasts %||% list()
  if (length(contrasts) == 0) {
    return(data.frame(Contraste = character(0), n_testes = integer(0),
                      n_sig = integer(0), n_up = integer(0), n_down = integer(0),
                      actif = logical(0)))
  }
  do.call(rbind, lapply(names(contrasts), function(nm) {
    r   <- contrasts[[nm]]
    sig <- !is.na(r$padj) & r$padj < padj_thresh & abs(r$log2FoldChange) > lfc_thresh
    data.frame(
      Contraste = nm,
      n_testes  = nrow(r),
      n_sig     = sum(sig),
      n_up      = sum(sig & r$log2FoldChange > 0),
      n_down    = sum(sig & r$log2FoldChange < 0),
      actif     = identical(nm, active_contrast),
      stringsAsFactors = FALSE
    )
  }))
}

#' Up/Down summary barchart — one or several contrasts side by side
#' @param summary_df Output of `summarize_contrasts_updown()`.
#' @return ggplot object (grouped bar chart, counts labelled).
plot_updown_barchart <- function(summary_df, tr = NULL, palette = "default", manual_colors = NULL,
                                 theme_choice = TS_THEME_DEFAULT, base_size = 12) {
  tr <- tr %||% function(x) x
  if (is.null(summary_df) || nrow(summary_df) == 0) {
    stop(errorCondition("Aucun contraste calculé.", class = "bulk_de_error"))
  }
  long_df <- data.frame(
    Contraste = rep(summary_df$Contraste, 2),
    Direction = rep(c("Up", "Down"), each = nrow(summary_df)),
    Count     = c(summary_df$n_up, summary_df$n_down)
  )
  long_df$Contraste <- factor(long_df$Contraste, levels = summary_df$Contraste)
  long_df$Direction <- factor(long_df$Direction, levels = c("Up", "Down"))

  role_cols <- bulk_role_colors(palette, manual_colors)

  ggplot(long_df, aes(x = Contraste, y = Count, fill = Direction)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_text(aes(label = Count), position = position_dodge(width = 0.7),
              vjust = -0.3, size = 3.4) +
    scale_fill_manual(values = c(Up = role_cols[["Up"]], Down = role_cols[["Down"]])) +
    labs(title = tr("Gènes significatifs Up / Down par contraste"),
         x = NULL, y = tr("Nombre de gènes"), fill = NULL) +
    ts_theme(theme_choice, base_size) +
    theme(plot.title = element_text(face = "bold", size = 14),
          axis.text.x = element_text(angle = if (nrow(summary_df) > 4) 30 else 0, hjust = 1))
}

#' Scree plot (variance expliquée par composante) — PCA companion
#'
#' Reuses the exact same top-variable-gene selection as `plot_bulk_pca()`
#' (ntop = 500, scale. = FALSE) so the % variance shown here matches what
#' PC1/PC2 actually represent on the PCA tab.
#'
#' @param vst_matrix Matrix, VST-transformed expression (genes x samples).
#' @param ntop Number of most-variable genes used (must match plot_bulk_pca()).
#' @param max_pc Maximum number of components to display.
#' @return ggplot object (bars = % variance per PC, line = cumulative %).
plot_scree_bulk <- function(vst_matrix, ntop = 500, max_pc = 10,
                            theme_choice = TS_THEME_DEFAULT, base_size = 12, tr = NULL) {
  tr <- tr %||% function(x) x
  rv   <- apply(vst_matrix, 1, var)
  ntop <- min(ntop, nrow(vst_matrix))
  sel  <- order(rv, decreasing = TRUE)[seq_len(ntop)]
  pca  <- prcomp(t(vst_matrix[sel, , drop = FALSE]), scale. = FALSE)

  n_pc <- min(max_pc, length(pca$sdev))
  pct  <- 100 * pca$sdev[seq_len(n_pc)]^2 / sum(pca$sdev^2)
  df <- data.frame(PC = factor(paste0("PC", seq_len(n_pc)), levels = paste0("PC", seq_len(n_pc))),
                   pct = pct, cum_pct = cumsum(pct))

  ggplot(df, aes(x = PC)) +
    geom_col(aes(y = pct), fill = "#18BC9C", width = 0.6) +
    geom_line(aes(y = cum_pct, group = 1), color = "#2C3E50", linewidth = 0.8) +
    geom_point(aes(y = cum_pct), color = "#2C3E50", size = 2) +
    geom_text(aes(y = pct, label = paste0(round(pct, 1), "%")), vjust = -0.6, size = 3) +
    labs(title = tr("Scree Plot — Variance Expliquée"), x = NULL,
         y = tr("% Variance (barres) / % Cumulée (ligne)")) +
    ts_theme(theme_choice, base_size) +
    theme(plot.title = element_text(face = "bold", size = 13))
}

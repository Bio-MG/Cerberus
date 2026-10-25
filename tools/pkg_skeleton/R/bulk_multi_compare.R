# tools/pkg_skeleton/R/bulk_multi_compare.R — MD-2 : comparaison multi-jeux bulk
# =============================================================================
# Contrat gelé : docs/contracts/BULK_MULTI_CONTRACT.md §10
# Test de gel  : tests/testthat/test-bulk-multi-compare-contract-freeze.R
#
# Pure domain logic (no Shiny symbols — guarded by the freeze test).
# REUSE, never re-implement (guard §10.2.4):
#   - build_contrast_gene_sets()        — significant genes per result df
#   - build_contrast_intersection_dt()  — intersection table
#   - plot_upset_contrasts()            — UpSet for N >= 2 sets (module)
#   - plot_volcano_bulk()               — per-dataset volcano (panel wrapper)
# Errors are classed `bulk_multi_error` (same class as the MD-1 storage
# layer); the comparison states live in bulk_multi_error_states().
# =============================================================================

#' Public API surface (frozen by the contract freeze test)
#' @return Character vector of exported function names.
bulk_multi_compare_public_api <- function() {
  c(
    "bulk_multi_compare_public_api", "bulk_multi_entry_contrasts",
    "bulk_multi_common_contrasts", "bulk_multi_deg_gene_sets",
    "bulk_multi_volcano_scales", "bulk_multi_volcano_panel",
    "bulk_multi_concordance", "bulk_multi_run_comparison"
  )
}

#' Classed error constructor (bulk_multi_error — same class as MD-1)
#' @param msg French error message.
#' @param state One of bulk_multi_error_states().
.bulk_multi_compare_stop <- function(msg, state) {
  errorCondition(msg, class = "bulk_multi_error", state = state)
}

#' Validate the named `entries` argument (>= 2 container entries)
#' @param entries Named list of bulk_datasets entries.
#' @return The entries (invisible).
.bulk_multi_compare_check_entries <- function(entries) {
  if (!is.list(entries) || length(entries) < 2L ||
      is.null(names(entries)) || any(!nzchar(names(entries)))) {
    stop(.bulk_multi_compare_stop(
      "Comparaison multi-jeux : au moins 2 datasets enregistrés (avec un état pipeline) sont requis.",
      state = "insufficient_datasets"))
  }
  invisible(entries)
}

#' Contrast names available in one container entry
#' @param entry A bulk_datasets entry (list with optional $pipeline$contrasts).
#' @return Character vector of contrast names (empty when absent).
bulk_multi_entry_contrasts <- function(entry) {
  if (!is.list(entry) || is.null(entry$pipeline) ||
      is.null(entry$pipeline$contrasts)) return(character(0))
  nms <- names(entry$pipeline$contrasts)
  if (is.null(nms)) character(0) else nms
}

#' Contrast names common to ALL provided entries
#' @param entries Named list of bulk_datasets entries.
#' @return Character vector (possibly empty) of common contrast names.
bulk_multi_common_contrasts <- function(entries) {
  if (!is.list(entries) || length(entries) == 0L) return(character(0))
  per_entry <- lapply(entries, bulk_multi_entry_contrasts)
  Reduce(intersect, per_entry)
}

#' Extract the result data.frame of one entry for a contrast (validated)
#' @param entry Container entry.
#' @param contrast Contrast name.
#' @return The data.frame (invisible).
.bulk_multi_compare_res_df <- function(entry, contrast) {
  df <- entry$pipeline$contrasts[[contrast]]
  if (is.null(df) || !is.data.frame(df) ||
      !all(c("gene", "log2FoldChange", "padj") %in% colnames(df))) {
    stop(.bulk_multi_compare_stop(
      sprintf("Contraste « %s » : résultat absent ou sans colonnes requises (gene, log2FoldChange, padj).",
              contrast),
      state = "no_common_contrast"))
  }
  invisible(df)
}

#' Significant-gene sets per dataset for one common contrast
#' @param entries Named list of container entries (>= 2).
#' @param contrast Common contrast name.
#' @param lfc_thresh,padj_thresh Shared thresholds (same for all datasets).
#' @param direction_aware Return "<name> (Up)"/"<name> (Down)" sets.
#' @return Named list of gene vectors (via build_contrast_gene_sets).
bulk_multi_deg_gene_sets <- function(entries, contrast, lfc_thresh,
                                     padj_thresh, direction_aware = FALSE) {
  .bulk_multi_compare_check_entries(entries)
  if (!is.character(contrast) || length(contrast) != 1L || !nzchar(contrast)) {
    stop(.bulk_multi_compare_stop(
      "Contraste invalide : chaîne de caractères non vide attendue.",
      state = "invalid_input"))
  }
  missing_in <- names(entries)[vapply(
    entries, function(e) !contrast %in% bulk_multi_entry_contrasts(e),
    logical(1))]
  if (length(missing_in) > 0L) {
    stop(.bulk_multi_compare_stop(
      sprintf("Contraste « %s » absent du ou des datasets : %s.",
              contrast, paste(missing_in, collapse = ", ")),
      state = "no_common_contrast"))
  }
  res_dfs <- lapply(entries, function(e) .bulk_multi_compare_res_df(e, contrast))
  build_contrast_gene_sets(res_dfs, lfc_thresh = lfc_thresh,
                           padj_thresh = padj_thresh,
                           direction_aware = direction_aware)
}

#' Shared volcano axis limits across datasets (contract §10.2.3)
#' @param res_dfs Named list of result data.frames (log2FoldChange, padj).
#' @return list(x = c(-m, m), y = c(0, M)) — ceiling-rounded global limits.
bulk_multi_volcano_scales <- function(res_dfs) {
  if (!is.list(res_dfs) || length(res_dfs) == 0L) {
    stop(.bulk_multi_compare_stop(
      "Échelle volcano : au moins un data.frame de résultats est requis.",
      state = "invalid_input"))
  }
  for (df in res_dfs) {
    if (!is.data.frame(df) ||
        !all(c("log2FoldChange", "padj") %in% colnames(df))) {
      stop(.bulk_multi_compare_stop(
        "Échelle volcano : colonnes log2FoldChange et padj requises.",
        state = "invalid_obj"))
    }
  }
  x_all <- unlist(lapply(res_dfs, function(df) df$log2FoldChange))
  x_max <- suppressWarnings(ceiling(max(abs(x_all[!is.na(x_all)]), na.rm = TRUE)))
  if (!is.finite(x_max) || x_max < 1L) x_max <- 1L
  # padj nul (== 0) donne -log10 -> Inf : exclu du calcul d'échelle (contrat
  # §10.2.3), le point sera écrêté par coord_cartesian.
  y_vals <- unlist(lapply(res_dfs, function(df) {
    padj <- df$padj
    -log10(padj[!is.na(padj) & padj > 0])
  }))
  y_max <- suppressWarnings(ceiling(max(y_vals, na.rm = TRUE)))
  if (!is.finite(y_max) || y_max < 1L) y_max <- 1L
  list(x = c(-x_max, x_max), y = c(0, y_max))
}

#' One volcano panel for the multi-dataset grid (shared scale, titled)
#' @param res_df Result data.frame (gene, log2FoldChange, padj).
#' @param label Panel title (dataset label).
#' @param lfc_thresh,padj_thresh Shared thresholds.
#' @param scales Output of bulk_multi_volcano_scales().
#' @param engine Optional DE engine label for the subtitle.
#' @param tr Optional translation function.
#' @return A ggplot object.
bulk_multi_volcano_panel <- function(res_df, label, lfc_thresh, padj_thresh,
                                     scales, engine = NULL, tr = NULL) {
  if (!is.data.frame(res_df) ||
      !all(c("gene", "log2FoldChange", "padj") %in% colnames(res_df))) {
    stop(.bulk_multi_compare_stop(
      "Volcano multi-jeux : colonnes gene, log2FoldChange et padj requises.",
      state = "invalid_obj"))
  }
  if (!is.list(scales) || !all(c("x", "y") %in% names(scales))) {
    stop(.bulk_multi_compare_stop(
      "Volcano multi-jeux : scales invalide (list(x = c(...), y = c(...))) attendu.",
      state = "invalid_input"))
  }
  p <- plot_volcano_bulk(res_df, lfc_thresh = lfc_thresh,
                         padj_thresh = padj_thresh, engine = engine, tr = tr)
  p + ggplot2::labs(title = label) +
    ggplot2::coord_cartesian(xlim = scales$x, ylim = scales$y)
}

#' Pairwise direction concordance across datasets (contract §10.4)
#' @param up_down_sets Output of bulk_multi_deg_gene_sets(direction_aware = TRUE).
#' @return data.frame (one row per dataset pair).
bulk_multi_concordance <- function(up_down_sets) {
  nm <- names(up_down_sets)
  up_nms <- nm[grepl("\\(Up\\)$", nm)]
  downs <- sub(" \\(Up\\)$", "", up_nms)
  if (length(up_nms) < 2L ||
      !all(paste0(downs, " (Down)") %in% nm)) {
    stop(.bulk_multi_compare_stop(
      "Concordance : ensembles direction-aware « X (Up) » / « X (Down) » requis pour au moins 2 datasets.",
      state = "insufficient_datasets"))
  }
  ups <- lapply(up_nms, function(k) as.character(up_down_sets[[k]]))
  dns <- lapply(paste0(downs, " (Down)"),
                function(k) as.character(up_down_sets[[k]]))
  names(ups) <- downs
  names(dns) <- downs
  pairs <- utils::combn(downs, 2L, simplify = FALSE)
  rows <- lapply(pairs, function(pr) {
    a <- pr[1L]; b <- pr[2L]
    .jac <- function(x, y) {
      u <- length(union(x, y))
      if (u == 0L) NA_real_ else length(intersect(x, y)) / u
    }
    sig_a <- union(ups[[a]], dns[[a]])
    sig_b <- union(ups[[b]], dns[[b]])
    common <- intersect(sig_a, sig_b)
    n_common <- length(common)
    same <- length(union(intersect(ups[[a]], ups[[b]]),
                         intersect(dns[[a]], dns[[b]])))
    data.frame(
      dataset_a = a, dataset_b = b,
      n_up_a = length(ups[[a]]), n_down_a = length(dns[[a]]),
      n_up_b = length(ups[[b]]), n_down_b = length(dns[[b]]),
      jaccard_up = .jac(ups[[a]], ups[[b]]),
      jaccard_down = .jac(dns[[a]], dns[[b]]),
      n_common_sig = n_common,
      pct_same_direction = if (n_common == 0L) NA_real_ else 100 * same / n_common,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Run the full cross-dataset comparison for one common contrast (pure)
#' @param entries Named list of container entries (>= 2, contrast common).
#' @param contrast Common contrast name.
#' @param lfc_thresh,padj_thresh Shared thresholds (same for all datasets).
#' @return The flat result list of contract §10.4 (invisible).
bulk_multi_run_comparison <- function(entries, contrast, lfc_thresh,
                                      padj_thresh) {
  .bulk_multi_compare_check_entries(entries)
  if (!is.numeric(lfc_thresh) || length(lfc_thresh) != 1L ||
      is.na(lfc_thresh) || lfc_thresh < 0) {
    stop(.bulk_multi_compare_stop(
      "Seuil |log2FC| invalide : numérique >= 0 attendu.",
      state = "invalid_input"))
  }
  if (!is.numeric(padj_thresh) || length(padj_thresh) != 1L ||
      is.na(padj_thresh) || padj_thresh <= 0 || padj_thresh > 1) {
    stop(.bulk_multi_compare_stop(
      "Seuil p-adj invalide : numérique dans ]0 ; 1] attendu.",
      state = "invalid_input"))
  }
  if (!is.character(contrast) || length(contrast) != 1L || !nzchar(contrast)) {
    stop(.bulk_multi_compare_stop(
      "Contraste invalide : chaîne de caractères non vide attendue.",
      state = "invalid_input"))
  }

  res_dfs <- lapply(entries, function(e) .bulk_multi_compare_res_df(e, contrast))
  deg_sets <- bulk_multi_deg_gene_sets(entries, contrast, lfc_thresh,
                                       padj_thresh, direction_aware = FALSE)
  if (all(lengths(deg_sets) == 0L)) {
    stop(.bulk_multi_compare_stop(
      sprintf("Aucun gène significatif (padj < %.2g, |log2FC| > %.2g) dans les datasets comparés — élargissez les seuils.",
              padj_thresh, lfc_thresh),
      state = "no_significant_genes"))
  }
  up_down_sets <- bulk_multi_deg_gene_sets(entries, contrast, lfc_thresh,
                                           padj_thresh, direction_aware = TRUE)

  per_dataset <- do.call(rbind, lapply(names(entries), function(nm) {
    df <- res_dfs[[nm]]
    sig <- !is.na(df$padj) & df$padj < padj_thresh &
      abs(df$log2FoldChange) > lfc_thresh
    e <- entries[[nm]]
    data.frame(
      label = nm,
      n_tested = nrow(df),
      n_up = sum(sig & df$log2FoldChange > 0),
      n_down = sum(sig & df$log2FoldChange < 0),
      stored_lfc_thresh = e$pipeline$lfc_thresh,
      stored_padj_thresh = e$pipeline$padj_thresh,
      stringsAsFactors = FALSE
    )
  }))

  invisible(list(
    datasets       = names(entries),
    contrast       = contrast,
    lfc_thresh     = lfc_thresh,
    padj_thresh    = padj_thresh,
    deg_sets       = deg_sets,
    up_down_sets   = up_down_sets,
    intersection_dt = build_contrast_intersection_dt(deg_sets),
    concordance    = bulk_multi_concordance(up_down_sets),
    volcano_scales = bulk_multi_volcano_scales(res_dfs),
    per_dataset    = per_dataset,
    ran_at         = Sys.time()
  ))
}

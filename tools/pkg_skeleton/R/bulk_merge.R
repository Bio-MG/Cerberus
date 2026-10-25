# =============================================================================
# tools/pkg_skeleton/R/bulk_merge.R — NEW-2 : fusion de jeux bulk ("Merge Data")
# =============================================================================
# Contrat gelé : docs/contracts/BULK_MERGE_CONTRACT.md
# Test de gel  : tests/testthat/test-bulk-merge-contract-freeze.R
#
# Fiche NEW-2 (ROADMAP_presentation_stats.md §6) — ÉCART ASSUMÉ, documenté au
# contrat §1.2 : la fiche proposait d'étendre modules/import/mod_import_bulk.R,
# mais elle a été écrite AVANT MD-1..MD-4. L'arbre actuel dispose du conteneur
# `bulk_datasets` (jeux nommés, validés par bulk_multi_check_obj()) et de la
# famille de panneaux « Multi-jeux » (datasets MD-1, comparaison MD-2). La
# fusion consomme donc >= 2 jeux ENREGISTRÉS dans le conteneur et son produit
# est chargé comme jeu ACTIF (global_data$bulk_obj), exactement comme un
# import : les étages du pipeline (Filtrage -> DE) se recalculent quand
# l'utilisateur relance chaque étape. Aucune écriture sur le conteneur ni sur
# shared_rv ; le module ne reçoit même pas shared_rv (garde du test de gel).
#
# RÉUTILISATION STRICTE (garde §2 du contrat — jamais réimplémenté ici) :
#   - bulk_multi_check_obj()         (MD-1)  : validation de chaque jeu
#   - bulk_multi_check_label()       (MD-1)  : libellé de fusion
#   - run_combat_seq()               (STAT-S1) : harmonisation ComBat-seq
#   - bulk_batch_correction_design() (STAT-S1) : faisabilité lot x condition
#   - bulk_batch_correction_label()  (STAT-S1) : libellé de provenance
#   - bulk_update_provenance()       (M1)      : manifeste de provenance
#
# Alignement des gènes : INTERSECTION EXACTE des noms de lignes (aucune
# conversion Ensembl/symbole — passer par l'étape Mapping IDs AVANT
# enregistrement si les jeux n'utilisent pas les mêmes identifiants).
#
# Renommage des échantillons en collision (règle déclarée, contrat §4.3) :
# si un nom d'échantillon apparaît dans > 1 jeu, TOUS les échantillons des
# jeux concernés sont préfixés « <label>__ » ; les renommages sont comptés et
# restitués (jamais silencieux).
#
# Erreurs : classées `bulk_merge_error` (français, call. = FALSE), états :
#   invalid_input | insufficient_datasets | no_common_genes |
#   invalid_metadata | design_not_applicable
# Les erreurs ComBat-seq propagent TELLES QUELLES (bulk_batch_correction_error,
# états STAT-S1 gelés) — elles sont re-classées par le module appelant.
# =============================================================================

#' Surface publique figée du domaine fusion (gel par test de freeze)
#' @return Character vector of exported function names.
bulk_merge_public_api <- function() {
  c(
    "bulk_merge_public_api", "bulk_merge_error_states",
    "bulk_merge_check_inputs", "bulk_merge_common_meta_columns",
    "bulk_merge_align_genes",
    "bulk_merge_preview_metadata", "bulk_merge_combine",
    "bulk_merge_run", "bulk_merge_to_bulk_obj"
  )
}

#' Frozen error states (contract §8)
#' @return Character vector of `state` attribute values.
bulk_merge_error_states <- function() {
  c("invalid_input", "insufficient_datasets", "no_common_genes",
    "invalid_metadata", "design_not_applicable")
}

#' Classed error constructor (bulk_merge_error, contract §8)
#' @param msg French error message.
#' @param state One of bulk_merge_error_states().
#' @return An errorCondition with class `bulk_merge_error`.
.bulk_merge_stop <- function(msg, state) {
  stop(errorCondition(msg, class = "bulk_merge_error", state = state))
}

#' Validate the merge inputs (contract §3)
#'
#' Reuses `bulk_multi_check_obj()` (MD-1) on every entry — the container
#' already guarantees this shape at registration time, the merge re-checks it
#' at consumption time (defense in depth, zero re-implementation).
#'
#' @param datasets Named list of >= 2 bulk_obj-shaped entries (obj$counts
#'   matrix with rownames + colnames, obj$metadata data.frame or NULL).
#' @param condition_col NULL or the name of a metadata column present in ALL
#'   entries (the biological condition to preserve through ComBat-seq).
#' @return TRUE invisibly; classed error otherwise
#'   (bulk_merge_error, or bulk_multi_error from the reused validator).
bulk_merge_check_inputs <- function(datasets, condition_col = NULL) {
  if (!is.list(datasets) || length(datasets) == 0L ||
      is.null(names(datasets)) || any(!nzchar(names(datasets)))) {
    .bulk_merge_stop(
      "Fusion impossible : une liste nommée de jeux bulk_datasets est attendue.",
      state = "invalid_input")
  }
  if (length(datasets) < 2L) {
    .bulk_merge_stop(
      sprintf("Fusion impossible : au moins 2 jeux enregistrés sont requis (reçu : %d). Sélectionnez-les dans « Multi-jeux — Datasets enregistrés » ou enregistrez-les à l'import.",
              length(datasets)),
      state = "insufficient_datasets")
  }
  if (anyDuplicated(names(datasets)) > 0L) {
    .bulk_merge_stop(
      "Fusion impossible : labels de jeux dupliqués dans la sélection.",
      state = "invalid_input")
  }
  for (nm in names(datasets)) {
    e <- datasets[[nm]]
    if (is.null(e) || !is.list(e)) {
      .bulk_merge_stop(
        sprintf("Jeu « %s » invalide : entrée introuvable ou mal formée dans le conteneur bulk_datasets.", nm),
        state = "invalid_input")
    }
    bulk_multi_check_obj(e)  # MD-1 validator, reused (may raise bulk_multi_error)
    if (is.null(rownames(e$counts))) {
      .bulk_merge_stop(
        sprintf("Jeu « %s » invalide : les counts n'ont pas de noms de gènes (rownames) — l'alignement des gènes est impossible.", nm),
        state = "invalid_input")
    }
  }
  if (!is.null(condition_col)) {
    if (!is.character(condition_col) || length(condition_col) != 1L ||
        is.na(condition_col) || !nzchar(trimws(condition_col))) {
      .bulk_merge_stop(
        "Colonne de condition invalide : attendu un nom de colonne unique, ou NULL.",
        state = "invalid_input")
    }
    for (nm in names(datasets)) {
      meta <- datasets[[nm]]$metadata
      if (is.null(meta) || !condition_col %in% colnames(meta)) {
        .bulk_merge_stop(
          sprintf("Colonne de condition « %s » absente des métadonnées du jeu « %s » — choisissez une colonne commune à tous les jeux, ou laissez le champ vide.",
                  condition_col, nm),
          state = "invalid_metadata")
      }
    }
  }
  invisible(TRUE)
}

#' Common metadata columns across datasets (contract §3 — UI choices)
#'
#' Intersection of metadata column names across ALL entries; a NULL metadata
#' contributes an empty set (a dataset without metadata carries no shared
#' column). Feeds the condition-column selector — the merge NEVER invents a
#' condition, the declared choice must exist everywhere (checked again by
#' bulk_merge_check_inputs at run time).
#'
#' @param datasets Named list of >= 2 bulk_obj-shaped entries.
#' @return Character vector of common column names (possibly empty).
bulk_merge_common_meta_columns <- function(datasets) {
  bulk_merge_check_inputs(datasets)
  cols_list <- lapply(datasets, function(e) {
    if (is.null(e$metadata)) character(0) else colnames(e$metadata)
  })
  Reduce(intersect, cols_list)
}

#' Align the common genes across datasets (contract §4.1)
#'
#' EXACT intersection of rownames (no identifier conversion — see contract
#' §11.1). Order follows the FIRST dataset's rownames (deterministic).
#'
#' @param datasets Named list of >= 2 bulk_obj-shaped entries.
#' @return list(common_genes, per_dataset, n_common_genes) — per_dataset is a
#'   data.frame (label, n_genes_total, n_genes_common, pct_common).
bulk_merge_align_genes <- function(datasets) {
  bulk_merge_check_inputs(datasets)
  gene_sets <- lapply(datasets, function(e) rownames(e$counts))
  common <- Reduce(intersect, gene_sets)
  per_ds <- do.call(rbind, lapply(names(datasets), function(nm) {
    total <- length(gene_sets[[nm]])
    kept  <- length(common)
    data.frame(
      label          = nm,
      n_genes_total  = total,
      n_genes_common = kept,
      pct_common     = if (total > 0L) round(100 * kept / total, 1) else 0,
      stringsAsFactors = FALSE
    )
  }))
  if (length(common) == 0L) {
    .bulk_merge_stop(
      paste0("Fusion impossible : aucun gène commun aux jeux sélectionnés (intersection vide). ",
             "Vérifiez que tous les jeux utilisent les mêmes identifiants de gènes ",
             "(symboles vs Ensembl — passez par l'étape Mapping IDs avant enregistrement). ",
             "Détail : ", paste(sprintf("%s : %d gènes", per_ds$label, per_ds$n_genes_total),
                                collapse = " ; "), "."),
      state = "no_common_genes")
  }
  list(common_genes = common, per_dataset = per_ds,
       n_common_genes = length(common))
}

#' Build the combined metadata WITHOUT the counts (contract §4.2-4.3)
#'
#' Used by the module for the live design preview (ComBat feasibility on the
#' merged metadata) and internally by `bulk_merge_combine()` — ONE builder,
#' never two. Metadata rows are matched to counts columns BY ROWNAMES
#' (setequal required, reordered to match); a NULL metadata falls back to the
#' import default (sample column). Colliding sample names rename ALL samples
#' of every dataset that owns a collision (« <label>__<origine> »), counted in
#' `renames` (never silent). A `sample` column, if present, is re-synced to
#' the final names.
#'
#' @param datasets Named list of >= 2 bulk_obj-shaped entries.
#' @param batch_col Name of the dataset-origin column to append.
#' @return list(metadata, renames (data.frame label/original/final),
#'   n_renamed_datasets).
bulk_merge_preview_metadata <- function(datasets, batch_col = "dataset_origin") {
  bulk_merge_check_inputs(datasets)
  if (!is.character(batch_col) || length(batch_col) != 1L ||
      is.na(batch_col) || !nzchar(trimws(batch_col))) {
    .bulk_merge_stop(
      "Nom de colonne lot invalide : une chaîne non vide est attendue (batch_col).",
      state = "invalid_input")
  }

  # ── Per-dataset metadata, aligned to counts columns by rownames ──────────
  metas <- lapply(names(datasets), function(nm) {
    e    <- datasets[[nm]]
    cols <- colnames(e$counts)
    meta <- e$metadata
    if (is.null(meta)) {
      meta <- data.frame(sample = cols, row.names = cols,
                         stringsAsFactors = FALSE)
      return(meta)
    }
    rn <- rownames(meta)
    if (is.null(rn) || !setequal(rn, cols)) {
      .bulk_merge_stop(
        sprintf("Jeu « %s » : les noms de lignes des métadonnées ne correspondent pas aux noms d'échantillons des counts — alignement impossible (fusion refusée).", nm),
        state = "invalid_metadata")
    }
    meta[cols, , drop = FALSE]
  })

  # ── Sample-name collisions -> prefix ALL samples of the owning datasets ──
  all_samples <- unlist(lapply(datasets, function(e) colnames(e$counts)),
                        use.names = FALSE)
  colliding <- names(table(all_samples))[table(all_samples) > 1L]
  renames <- data.frame(label = character(0), original = character(0),
                        final = character(0), stringsAsFactors = FALSE)
  renamed_datasets <- character(0)
  final_cols <- vector("list", length(datasets))
  names(final_cols) <- names(datasets)
  for (nm in names(datasets)) {
    cols <- colnames(datasets[[nm]]$counts)
    if (any(cols %in% colliding)) {
      renamed_datasets <- c(renamed_datasets, nm)
      new_cols <- paste(nm, cols, sep = "__")
      renames <- rbind(renames, data.frame(
        label = nm, original = cols, final = new_cols,
        stringsAsFactors = FALSE))
      final_cols[[nm]] <- new_cols
    } else {
      final_cols[[nm]] <- cols
    }
  }
  final_all <- unlist(final_cols, use.names = FALSE)
  if (anyDuplicated(final_all) > 0L) {
    .bulk_merge_stop(
      "Fusion impossible : les noms d'échantillons ne peuvent pas être rendus uniques (collision après préfixage) — renommez vos échantillons ou vos jeux.",
      state = "invalid_metadata")
  }

  # ── Union-fill rbind (first-seen column order; missing -> NA) ────────────
  col_order <- unique(unlist(lapply(metas, colnames), use.names = FALSE))
  metas <- lapply(seq_along(metas), function(i) {
    m  <- metas[[i]]
    miss <- setdiff(col_order, colnames(m))
    if (length(miss) > 0L) {
      for (cl in miss) m[[cl]] <- NA
    }
    m[, col_order, drop = FALSE]
  })
  meta_all <- do.call(rbind, metas)
  rownames(meta_all) <- final_all

  # ── Dataset-origin column (batch = jeu d'origine) ────────────────────────
  for (nm in names(datasets)) {
    m <- datasets[[nm]]$metadata
    if (!is.null(m) && batch_col %in% colnames(m)) {
      .bulk_merge_stop(
        sprintf("Jeu « %s » : ses métadonnées contiennent déjà une colonne « %s » — choisissez un autre nom de colonne lot (batch_col).", nm, batch_col),
        state = "invalid_metadata")
    }
  }
  origin <- unlist(lapply(seq_along(datasets), function(i) {
    rep(names(datasets)[i], length(final_cols[[i]]))
  }), use.names = FALSE)
  meta_all[[batch_col]] <- as.character(origin)

  # ── Resync the `sample` column if any dataset carried one ────────────────
  if ("sample" %in% colnames(meta_all)) {
    meta_all$sample <- final_all
  }

  list(metadata = meta_all, renames = renames,
       n_renamed_datasets = length(renamed_datasets))
}

#' Combine the aligned counts and the merged metadata (contract §4-§5)
#'
#' @param datasets Named list of >= 2 bulk_obj-shaped entries.
#' @param common_genes Character vector as returned by
#'   bulk_merge_align_genes()$common_genes (validated as a subset of every
#'   dataset's rownames).
#' @param batch_col Name of the dataset-origin column to append.
#' @return list(counts, metadata, renames, batch_col).
bulk_merge_combine <- function(datasets, common_genes,
                               batch_col = "dataset_origin") {
  bulk_merge_check_inputs(datasets)
  if (!is.character(common_genes) || length(common_genes) == 0L) {
    .bulk_merge_stop(
      "Fusion impossible : liste de gènes communs vide ou invalide (passez par bulk_merge_align_genes()).",
      state = "invalid_input")
  }
  for (nm in names(datasets)) {
    if (!all(common_genes %in% rownames(datasets[[nm]]$counts))) {
      .bulk_merge_stop(
        sprintf("Jeu « %s » : des gènes communs sont absents de ses counts — relancez bulk_merge_align_genes().", nm),
        state = "invalid_input")
    }
  }
  meta_res <- bulk_merge_preview_metadata(datasets, batch_col)

  # Columns are taken in each dataset's ORIGINAL order; the merged dimnames
  # are then asserted to the FINAL (post-rename) sample order so that counts
  # and metadata stay aligned by construction.
  blocks <- lapply(names(datasets), function(nm) {
    cols <- colnames(datasets[[nm]]$counts)
    datasets[[nm]]$counts[common_genes, cols, drop = FALSE]
  })
  merged <- do.call(cbind, blocks)
  dimnames(merged) <- list(common_genes, rownames(meta_res$metadata))
  list(counts = merged, metadata = meta_res$metadata,
       renames = meta_res$renames, batch_col = batch_col)
}

#' Run the full merge (contract §6) — optional ComBat-seq harmonization
#'
#' Chain: check -> align -> combine -> [design check + run_combat_seq()].
#' ComBat-seq (STAT-S1) is applied with batch = dataset origin; the declared
#' condition column is preserved via `group=` when the design allows it.
#' ComBat errors propagate as `bulk_batch_correction_error` (STAT-S1 frozen
#' states, e.g. missing_dependency) — the module catches both classes.
#'
#' @param datasets Named list of >= 2 bulk_obj-shaped entries.
#' @param condition_col NULL or a common metadata column (biological condition
#'   to preserve). No default: declaring it is the user's decision.
#' @param apply_combat FALSE by default — without it, the merge is a plain
#'   gene-aligned concatenation.
#' @param batch_col Name of the dataset-origin column (default
#'   "dataset_origin", contract §5.2).
#' @return list(counts, counts_pre_combat (merged raw counts, only when ComBat
#'   was applied — consumed by the before/after PCA), metadata, batch_col,
#'   condition_col, combat_applied, labels, gene_alignment, n_common_genes,
#'   n_samples, renames, per_dataset, provenance_normalization, ran_at).
bulk_merge_run <- function(datasets, condition_col = NULL,
                           apply_combat = FALSE,
                           batch_col = "dataset_origin") {
  t_start <- Sys.time()
  al  <- bulk_merge_align_genes(datasets)
  cmb <- bulk_merge_combine(datasets, al$common_genes, batch_col)

  counts_out   <- cmb$counts
  counts_pre   <- cmb$counts
  combat       <- FALSE
  prov_label   <- NULL
  cond_resolved <- NULL

  if (isTRUE(apply_combat)) {
    dsgn <- bulk_batch_correction_design(cmb$metadata, batch_col, condition_col)
    if (!isTRUE(dsgn$can_apply)) {
      .bulk_merge_stop(
        paste0("Harmonisation ComBat-seq impossible : ",
               paste(dsgn$blocking_messages, collapse = " "),
               " Décochez l'harmonisation pour fusionner sans correction."),
        state = "design_not_applicable")
    }
    group_vec <- if (isTRUE(dsgn$use_group) && !is.null(condition_col)) {
      as.character(cmb$metadata[[condition_col]])
    } else {
      NULL
    }
    cond_resolved <- if (isTRUE(dsgn$use_group)) condition_col else NULL
    counts_out <- run_combat_seq(
      cmb$counts,
      batch = as.character(cmb$metadata[[batch_col]]),
      group = group_vec,
      context = "fusion de jeux (ComBat-seq)")
    combat <- TRUE
    prov_label <- bulk_batch_correction_label(
      batch_col, cond_resolved, previous_normalization = NULL)
  } else if (!is.null(condition_col)) {
    cond_resolved <- condition_col
  }

  per_ds <- al$per_dataset
  per_ds$n_samples <- vapply(names(datasets), function(nm) {
    ncol(datasets[[nm]]$counts)
  }, integer(1), USE.NAMES = FALSE)
  per_ds <- per_ds[, c("label", "n_samples", "n_genes_total",
                       "n_genes_common", "pct_common"), drop = FALSE]

  list(
    counts                   = counts_out,
    counts_pre_combat        = if (combat) counts_pre else NULL,
    metadata                 = cmb$metadata,
    batch_col                = batch_col,
    condition_col            = cond_resolved,
    combat_applied           = combat,
    labels                   = names(datasets),
    gene_alignment           = al$per_dataset,
    n_common_genes           = al$n_common_genes,
    n_samples                = ncol(counts_out),
    renames                  = cmb$renames,
    per_dataset              = per_ds,
    provenance_normalization = prov_label,
    ran_at                   = t_start
  )
}

#' Shape a merge result as an active bulk_obj (contract §7)
#'
#' The product is loaded by the module into `global_data$bulk_obj` — exactly
#' like an import (type "bulk", project, timestamp, import_mode = "merge").
#' When ComBat-seq was applied, the provenance manifest records the
#' normalization label PRODUCED at merge time (bulk_batch_correction_label);
#' without ComBat, no manifest is created (same as a fresh import — the
#' pipeline stages write their own provenance). `merge_info` keeps the
#' traceability of the fusion itself (datasets, common genes, renames).
#'
#' @param result Output of bulk_merge_run().
#' @param project_name Free text label for the merged dataset (trimmed; a
#'   fallback default is applied when empty).
#' @return A bulk_obj-shaped list (invisible).
bulk_merge_to_bulk_obj <- function(result, project_name = "") {
  if (!is.list(result) || is.null(result$counts) || is.null(result$metadata)) {
    .bulk_merge_stop(
      "Résultat de fusion invalide : counts et metadata sont requis (passez par bulk_merge_run()).",
      state = "invalid_input")
  }
  proj <- trimws(as.character(if (is.null(project_name)) "" else project_name))
  if (!nzchar(proj)) {
    n_ds <- if (!is.null(result$labels)) length(result$labels) else 0L
    proj <- sprintf("Fusion multi-jeux (%d)", n_ds)
  }
  obj <- list(
    counts      = result$counts,
    metadata    = result$metadata,
    project     = proj,
    type        = "bulk",
    timestamp   = Sys.time(),
    import_mode = "merge",
    merge_info  = list(
      datasets       = result$labels,
      batch_col      = result$batch_col,
      condition_col  = result$condition_col,
      combat_applied = isTRUE(result$combat_applied),
      n_common_genes = result$n_common_genes,
      n_samples      = result$n_samples,
      n_renames      = if (!is.null(result$renames)) nrow(result$renames) else 0L,
      ran_at         = result$ran_at
    )
  )
  if (!is.null(result$provenance_normalization) &&
      nzchar(result$provenance_normalization)) {
    obj <- bulk_update_provenance(
      obj, normalization = result$provenance_normalization)
  }
  invisible(obj)
}

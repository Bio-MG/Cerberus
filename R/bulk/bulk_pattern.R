# =============================================================================
# R/bulk/bulk_pattern.R — Clustering de profils d'expression (STAT-S3, MVP)
# =============================================================================
# Fiche ROADMAP_presentation_stats.md §3 (STAT-S3), re-mesurée le 2026-09-13 :
# `shared_rv$vst_mat` (étape 1) + métadonnées `global_data$bulk_obj$metadata`
# + listes de gènes DE (`shared_rv$contrasts`) — même pattern que
# mod_bulk_pathways. NOTE d'adaptation : la fiche plaçait le moteur dans
# R/core/ ; l'arbre actuel place les moteurs bulk dans R/bulk/ (bulk_wgcna.R,
# bulk_gsva.R, bulk_survival.R) — alignement sur l'existant.
#
# MVP kmeans, ZÉRO dépendance nouvelle :
#   1. moyenne VST par groupe (gènes x groupes) ;
#   2. z-score PAR GÈNE à travers les groupes (chaque gène contribue également,
#      quelles que soient son abondance et sa variance absolues) ;
#   3. stats::kmeans() sur les profils standardisés.
# La V2 floue (Mfuzz, fuzzy c-means) est une OPTION non retenue dans ce jalon.
#
# « Profil » = forme relative d'expression entre groupes : DESCRIPTIF, aucune
# affirmation différentielle (aucune p-value n'est produite ici).
#
# Contrat gelé : docs/contracts/BULK_PATTERN_CONTRACT.md (+ freeze test).
# Erreurs classées `bulk_pattern_error` (français, errorCondition, state).
# =============================================================================

#' Champs contractuels du résultat de clustering de profils
#'
#' @return vecteur character des champs canoniques (gelé par le freeze test).
#' @export
bulk_pattern_contract_fields <- function() {
  c("type", "status", "clusters", "cluster_profiles", "summary",
    "group_column", "k", "seed", "parameters", "qc", "warnings",
    "provenance", "analysis_id", "timestamp_utc")
}

#' États de validité
#' @export
bulk_pattern_validity_states <- function() {
  c("valid", "valid_with_warnings")
}

.bulk_pattern_stop <- function(state, message) {
  stop(errorCondition(message, class = "bulk_pattern_error", state = state))
}

#' État d'une erreur classée bulk_pattern_error
#'
#' @param e condition attrapée.
#' @return character(1) : le champ `state`, ou NA_character_ si non classée.
#' @export
# §14.1 : délégation à l'accesseur générique. Famille F2 MESURÉE — la classe
# n'est PAS vérifiée : l'état est rendu dès qu'il existe.
bulk_pattern_error_state <- function(e) ts_error_state(e)

#' Surface publique figée (le freeze test refuse toute fonction non listée)
#' @export
bulk_pattern_public_api <- function() {
  c("bulk_pattern_contract_fields", "bulk_pattern_validity_states",
    "bulk_pattern_error_state", "run_pattern_clustering",
    "plot_pattern_profiles", "build_pattern_table_export",
    "assert_bulk_pattern_result", "bulk_pattern_public_api")
}

#' Clustering de profils d'expression par kmeans (MVP STAT-S3)
#'
#' @param vst_mat matrice transformée (gènes x échantillons), colonnes = échantillons.
#' @param metadata data.frame d'échantillons (lignes alignées sur les colonnes
#'   de `vst_mat`, ou jointe par rownames si elles correspondent).
#' @param group_column colonne de `metadata` définissant les groupes (facteur
#'   ou character) — déclarée, jamais déduite.
#' @param genes vecteur de gènes à clusteriser (ex. liste DE) ; les gènes
#'   absents de `vst_mat` sont exclus ET comptabilisés ; NULL = tous les gènes.
#' @param k nombre de clusters — CHOIX DECLARÉ, entier 2..kmax.
#' @param seed graine du générateur (reproductibilité, tracée dans le résultat).
#' @param kmax plafond de k (défaut : TS_PATTERN_KMEANS_MAX_K si déclaré).
#' @param nstart relances de kmeans (défaut : TS_PATTERN_KMEANS_NSTART si déclaré).
#' @param itermax iter.max de kmeans (défaut : TS_PATTERN_KMEANS_ITERMAX si déclaré).
#' @return liste canonique (voir docs/contracts/BULK_PATTERN_CONTRACT.md) :
#'   type "bulk_pattern_clusters", clusters (data.frame gene/cluster),
#'   cluster_profiles (data.frame group/cluster/mean_z), summary, qc,
#'   provenance, timestamp_utc.
#' @export
run_pattern_clustering <- function(vst_mat, metadata, group_column, genes = NULL,
                                   k, seed = .bulk_pattern_seed_default(),
                                   kmax = .bulk_pattern_config("TS_PATTERN_KMEANS_MAX_K", 12L),
                                   nstart = .bulk_pattern_config("TS_PATTERN_KMEANS_NSTART", 10L),
                                   itermax = .bulk_pattern_config("TS_PATTERN_KMEANS_ITERMAX", 50L)) {

  # -- garde matrice -----------------------------------------------------------
  if (is.null(vst_mat) || !is.matrix(vst_mat) || !is.numeric(vst_mat)) {
    .bulk_pattern_stop("invalid_input",
      paste0("bulk_pattern : 'vst_mat' doit être une matrice numérique (reçu : ",
             if (is.null(vst_mat)) "NULL" else class(vst_mat)[1], "). ",
             "Lancez d'abord l'étape 1 (Filtrage & VST)."))
  }
  if (is.null(colnames(vst_mat)) || is.null(rownames(vst_mat))) {
    .bulk_pattern_stop("invalid_input",
      "bulk_pattern : 'vst_mat' doit avoir des dimnames (gènes x échantillons).")
  }
  if (nrow(vst_mat) < 2 || ncol(vst_mat) < 2) {
    .bulk_pattern_stop("invalid_input",
      sprintf("bulk_pattern : matrice trop petite (%d gènes x %d échantillons).",
              nrow(vst_mat), ncol(vst_mat)))
  }

  # -- garde metadata / groupe -------------------------------------------------
  if (is.null(metadata) || !is.data.frame(metadata)) {
    .bulk_pattern_stop("invalid_input",
      paste0("bulk_pattern : 'metadata' doit être un data.frame (reçu : ",
             if (is.null(metadata)) "NULL" else class(metadata)[1], ")."))
  }
  if (is.null(group_column) || length(group_column) != 1L || is.na(group_column) ||
      !nzchar(group_column) || !group_column %in% colnames(metadata)) {
    .bulk_pattern_stop("invalid_input",
      sprintf("bulk_pattern : colonne de groupe '%s' absente des métadonnées (colonnes disponibles : %s).",
              ifelse(is.null(group_column), "", group_column),
              paste(head(colnames(metadata), 12), collapse = ", ")))
  }

  # -- alignement échantillons (rownames si possible, sinon longueur égale) -----
  if (!is.null(rownames(metadata)) &&
      setequal(rownames(metadata), colnames(vst_mat))) {
    metadata <- metadata[colnames(vst_mat), , drop = FALSE]
  } else if (nrow(metadata) != ncol(vst_mat)) {
    .bulk_pattern_stop("invalid_input",
      sprintf("bulk_pattern : %d échantillons dans la matrice VST vs %d lignes de métadonnées — impossible d'aligner.",
              ncol(vst_mat), nrow(metadata)))
  }

  grp_raw <- metadata[[group_column]]
  keep_smp <- !is.na(grp_raw) & nzchar(trimws(as.character(grp_raw)))
  n_samples_na <- sum(!keep_smp)
  grp <- droplevels(as.factor(grp_raw[keep_smp]))
  n_groups <- nlevels(grp)
  if (n_groups < 2) {
    .bulk_pattern_stop("invalid_input",
      sprintf("bulk_pattern : la colonne '%s' doit définir au moins 2 groupes non vides (reçu : %d).",
              group_column, n_groups))
  }

  # -- sélection des gènes ------------------------------------------------------
  n_genes_input <- nrow(vst_mat)
  if (!is.null(genes)) {
    genes <- unique(as.character(genes))
    genes <- genes[nzchar(genes)]
    n_not_found <- sum(!genes %in% rownames(vst_mat))
    genes <- intersect(genes, rownames(vst_mat))
    if (length(genes) == 0) {
      .bulk_pattern_stop("invalid_input",
        "bulk_pattern : aucun des gènes fournis n'est présent dans la matrice VST (vérifiez les identifiants).")
    }
    warnings <- sprintf("%d gène(s) sur %d fournis absents de la matrice VST — exclus.",
                        n_not_found, length(genes) + n_not_found)
  } else {
    genes <- rownames(vst_mat)
    n_not_found <- 0L
    warnings <- character(0)
  }

  mat <- vst_mat[genes, colnames(vst_mat)[keep_smp], drop = FALSE]

  # gènes à variance nulle (ou NA) entre échantillons : inexploitables, exclus
  sd_rows <- apply(mat, 1L, stats::sd)
  bad_rows <- !is.finite(sd_rows) | sd_rows <= 0
  mat <- mat[!bad_rows, , drop = FALSE]
  n_genes_constant <- sum(bad_rows)
  if (nrow(mat) < 2) {
    .bulk_pattern_stop("invalid_input",
      "bulk_pattern : moins de 2 gènes exploitables après exclusion des gènes constants/NA.")
  }

  # -- profils : moyenne par groupe, puis z-score par gène ----------------------
  lvls <- levels(grp)
  idx <- lapply(lvls, function(l) which(grp == l))
  profiles_mean <- sapply(idx, function(ii) rowMeans(mat[, ii, drop = FALSE]))
  dimnames(profiles_mean) <- list(rownames(mat), lvls)

  profiles_z <- t(scale(t(profiles_mean)))
  bad_z <- rowSums(is.finite(profiles_z)) < ncol(profiles_z)
  n_genes_constant <- n_genes_constant + sum(bad_z)
  profiles_z <- profiles_z[!bad_z, , drop = FALSE]
  if (nrow(profiles_z) < 2) {
    .bulk_pattern_stop("invalid_input",
      "bulk_pattern : moins de 2 profils exploitables après standardisation (gènes constants entre groupes).")
  }

  # -- garde k ------------------------------------------------------------------
  if (is.null(k) || length(k) != 1L || is.na(k) || !is.numeric(k) || k < 2 ||
      k != floor(k)) {
    .bulk_pattern_stop("invalid_input",
      sprintf("bulk_pattern : k doit être un entier >= 2 (reçu : %s).",
              paste(format(k), collapse = ",")))
  }
  k <- as.integer(k)
  if (k > kmax) {
    .bulk_pattern_stop("invalid_input",
      sprintf("bulk_pattern : k = %d dépasse le plafond déclaré (%d) — au-delà, les profils ne sont plus lisibles.",
              k, kmax))
  }
  if (k > nrow(profiles_z)) {
    .bulk_pattern_stop("invalid_input",
      sprintf("bulk_pattern : k = %d supérieur au nombre de gènes exploitables (%d).",
              k, nrow(profiles_z)))
  }

  # -- kmeans -------------------------------------------------------------------
  # Graine déclarée (paramètre seed) + restauration du RNG par withr : deux
  # exécutions identiques donnent les mêmes clusters, sans fuiter la graine
  # vers l'analyse suivante (parité withr des pipelines SC, audit §1.8).
  km <- withr::with_seed(seed,
    stats::kmeans(profiles_z, centers = k, nstart = nstart, iter.max = itermax))

  clusters <- data.frame(
    gene    = rownames(profiles_z),
    cluster = as.integer(km$cluster),
    stringsAsFactors = FALSE
  )

  prof_long <- do.call(rbind, lapply(seq_along(lvls), function(j) {
    data.frame(group = lvls[j],
               cluster = as.integer(km$cluster),
               mean_z = as.numeric(profiles_z[, j]),
               stringsAsFactors = FALSE)
  }))
  cluster_profiles <- stats::aggregate(mean_z ~ group + cluster,
                                       data = prof_long, FUN = mean)
  cluster_profiles$group <- factor(cluster_profiles$group, levels = lvls)

  provenance <- new_provenance_entry(
    analysis_id = "bulk-pattern-clusters",
    method      = "stats::kmeans (moyenne VST par groupe + z-score par gène)",
    parameters  = list(group_column = group_column, k = k, seed = seed,
                       nstart = nstart, itermax = itermax,
                       n_genes_input = n_genes_input),
    dataset     = vst_mat,
    warnings    = warnings
  )

  list(
    type             = "bulk_pattern_clusters",
    status           = "valid",
    clusters         = clusters,
    cluster_profiles = cluster_profiles,
    group_column     = group_column,
    k                = k,
    seed             = as.integer(seed),
    summary = list(
      n_genes_input     = n_genes_input,
      n_genes_used      = nrow(profiles_z),
      n_genes_not_found = n_not_found,
      n_genes_constant  = n_genes_constant,
      n_samples         = sum(keep_smp),
      n_samples_na      = n_samples_na,
      n_groups          = n_groups,
      group_levels      = lvls
    ),
    parameters = list(group_column = group_column, k = k, seed = seed,
                      nstart = nstart, itermax = itermax),
    qc = list(n_genes_input = n_genes_input, n_genes_used = nrow(profiles_z),
              n_samples_na = n_samples_na, n_groups = n_groups),
    warnings      = warnings,
    provenance    = provenance,
    analysis_id   = "bulk-pattern-clusters",
    timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

.bulk_pattern_config <- function(name, fallback) {
  if (exists(name, inherits = TRUE)) get(name) else fallback
}

.bulk_pattern_seed_default <- function() {
  .bulk_pattern_config("TS_PATTERN_KMEANS_SEED", 15L)
}

#' Profils moyens par cluster (courbe par cluster, descriptif)
#'
#' @param pattern_result résultat canonique de run_pattern_clustering().
#' @param tr fonction de traduction optionnelle.
#' @param palette palette discrète (moteur partagé R/plotting/palettes.R).
#' @param manual_colors couleurs manuelles optionnelles.
#' @return ggplot.
#' @export
plot_pattern_profiles <- function(pattern_result, tr = NULL,
                                  palette = "default", manual_colors = NULL) {
  tr <- tr %||% function(x) x
  assert_bulk_pattern_result(pattern_result, context = "plot profils")

  df <- pattern_result$cluster_profiles
  df$cluster <- factor(df$cluster, levels = sort(unique(df$cluster)))

  ggplot2::ggplot(df, ggplot2::aes(x = .data$group, y = .data$mean_z,
                                   group = .data$cluster, color = .data$cluster)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    ggplot2::geom_line(linewidth = 1.1, alpha = 0.9) +
    ggplot2::geom_point(size = 2.4) +
    sc_discrete_scale(palette, manual_colors, aesthetic = "color") +
    ggplot2::labs(
      title = tr("Profils d'expression par cluster (kmeans)"),
      subtitle = sprintf("%s = %d, %s = %s, %s = %d",
                         tr("k"), pattern_result$k,
                         tr("groupe"), pattern_result$group_column,
                         tr("graine"), pattern_result$seed),
      x = tr("Groupe"), y = tr("Expression VST (z-score par gène)"),
      color = tr("Cluster")) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14),
                   legend.position = "right")
}

#' Export plat gène -> cluster
#'
#' @param pattern_result résultat canonique.
#' @return data.frame (gene, cluster) prêt pour write.csv.
#' @export
build_pattern_table_export <- function(pattern_result) {
  assert_bulk_pattern_result(pattern_result, context = "export profils")
  data.frame(gene = pattern_result$clusters$gene,
             cluster = pattern_result$clusters$cluster,
             stringsAsFactors = FALSE)
}

#' Refuse un objet qui n'est pas un résultat canonique de clustering de profils
#'
#' @param pattern_result objet à valider.
#' @param context contexte inclus dans le message d'erreur.
#' @return invisible(TRUE), ou stop() classé.
#' @export
assert_bulk_pattern_result <- function(pattern_result, context = "") {
  ctx <- if (nzchar(context)) paste0(" (", context, ")") else ""
  if (is.null(pattern_result) || !is.list(pattern_result) ||
      !identical(pattern_result$type, "bulk_pattern_clusters")) {
    .bulk_pattern_stop("invalid_input",
      paste0("assert_bulk_pattern_result", ctx, " : objet non canonique ",
             "(type attendu 'bulk_pattern_clusters')."))
  }
  if (!pattern_result$status %in% bulk_pattern_validity_states()) {
    .bulk_pattern_stop("invalid_input",
      paste0("assert_bulk_pattern_result", ctx, " : status '",
             pattern_result$status, "' inconnu."))
  }
  if (!is.data.frame(pattern_result$clusters) ||
      !all(c("gene", "cluster") %in% colnames(pattern_result$clusters))) {
    .bulk_pattern_stop("invalid_input",
      paste0("assert_bulk_pattern_result", ctx, " : 'clusters' doit être un data.frame gene/cluster."))
  }
  invisible(TRUE)
}

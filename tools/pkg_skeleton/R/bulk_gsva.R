# =============================================================================
# tools/pkg_skeleton/R/bulk_gsva.R — Scores de voies PAR ÉCHANTILLON (GSVA / ssGSEA / PLAGE /
# z-score) — roadmap Bulk V2, Milestone 2 (chantier Flux E).
# =============================================================================
# PRINCIPE : contrairement à l'ORA/GSEA de mod_bulk_pathways.R (qui teste
# l'enrichissement d'une LISTE de gènes différentiels), cette analyse attribue
# à CHAQUE échantillon un score par voie — la matrice de scores (voies x
# échantillons) est stockée dans bulk_obj$pathways$per_sample et alimente
# PCA / heatmaps par voie, y compris sans contraste.
#
# GARDES (mission Bulk V2 §2) :
#   - la matrice d'entrée DOIT être transformée (VST ou TMM log2-CPM) :
#     GSVA/ssGSEA sur counts bruts est invalide (kcdf gaussien requis) ;
#   - identifiants nettoyés (suffixes Ensembl `.1`/`.2` supprimés) AVANT
#     l'appariement contre les jeux de gènes ;
#   - porte de recouvrement : un jeu de gènes dont < TS_BULK_GSVA_OVERLAP_MIN
#     des gènes sont retrouvés dans la matrice est rejeté AVEC la liste
#     (mauvaise source d'identifiants presque toujours) ;
#   - tailles min/max appliquées localement (transparence) ET au constructeur.
#
# PARALLÉLISME (STATUS.md §2h) : BPPARAM est passé à GSVA::gsva() et NON au
# constructeur ; sous Windows -> SerialParam (jamais MulticoreParam, jamais
# fork) ; sinon MulticoreParam plafonné à TS_BULK_MAX_WORKERS.
#
# Fonctions pures, testables hors Shiny. Dépendance GSVA vérifiée par
# requireNamespace au moment de l'appel (jamais au source). Erreurs classées
# `bulk_gsva_error` (français, errorCondition, attribut state).
# =============================================================================

#' Surface publique figée du domaine scores par échantillon (gel)
bulk_gsva_public_api <- function() {
  c("bulk_gsva_public_api",
    "bulk_gsva_methods",
    "bulk_clean_gene_ids",
    "bulk_parse_gmt",
    "bulk_filter_gene_sets",
    "bulk_bpparam",
    "compute_pathway_scores",
    "build_pathway_scores_export",
    "plot_pathway_scores_pca",
    "plot_pathway_scores_heatmap")
}

#' Méthodes de scoring exposées dans l'UI
bulk_gsva_methods <- function() {
  c("gsva", "ssgsea", "plage", "zscore")
}

#' Nettoyer des identifiants de gènes (suffixes de version Ensembl)
#'
#' Supprime les suffixes de version `.1`, `.12`, etc. (Convention Ensembl
#' `ENSG00000141510.17`). Idempotent : un symbole ou un Ensembl déjà nu passe
#' inchangé. Ne modifie JAMAIS autre chose (pas de casse, pas d'espaces).
#'
#' @param ids Vecteur character d'identifiants.
#' @return Vecteur character nettoyé (mêmes positions).
bulk_clean_gene_ids <- function(ids) {
  out <- as.character(ids)
  out <- gsub("\\.[0-9]+$", "", out)
  # B1-C (O21, 2026-10-28) : les composites ENSG…_SYMBOL qui survivent au
  # strip de version sont ramenés à l'accession (même règle que le mapping).
  out <- strip_composite_gene_ids(out)
  out
}

#' Parseur GMT (Gene Matrix Transposed)
#'
#' Chaque ligne : `nom<TAB>description<TAB>gène1<TAB>gène2...`. Lignes vides et
#' jeux sans gène ignorés (les jeux trop petits sont rejetés plus tard, par la
#' porte de taille — pas ici, pour garder le parseur muet et testable).
#'
#' @param path Chemin du fichier .gmt.
#' @return Liste nommée (nom du set -> vecteur character de gènes) ; erreur
#'   classée si le fichier est vide ou illisible.
bulk_parse_gmt <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    stop(errorCondition(
      "bulk_parse_gmt() : fichier .gmt introuvable.",
      class = "bulk_gsva_error", state = "invalid_input"))
  }
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) NULL)
  if (is.null(lines)) {
    stop(errorCondition(sprintf("bulk_parse_gmt() : lecture impossible (%s).", path),
                        class = "bulk_gsva_error", state = "invalid_input"))
  }
  sets <- lapply(lines, function(l) {
    f <- strsplit(l, "\t", fixed = TRUE)[[1]]
    if (length(f) < 3L) return(NULL)
    genes <- f[3:length(f)]
    genes <- genes[nzchar(genes)]
    if (length(genes) == 0L) return(NULL)
    genes
  })
  names(sets) <- vapply(lines, function(l) {
    f <- strsplit(l, "\t", fixed = TRUE)[[1]]
    if (length(f) >= 1L) f[1] else ""
  }, character(1))
  keep <- !vapply(sets, is.null, logical(1)) & nzchar(names(sets))
  sets <- sets[keep]; names(sets) <- names(sets)[keep]
  if (length(sets) == 0L) {
    stop(errorCondition(
      "bulk_parse_gmt() : aucun jeu de gènes exploitable (format attendu : nom<TAB>description<TAB>gènes...).",
      class = "bulk_gsva_error", state = "invalid_input"))
  }
  # Doublons de noms : index numérique ajouté (GSEA ne le fait pas, et un
  # nom dupliqué casserait la matrice de scores).
  dup <- duplicated(names(sets))
  if (any(dup)) names(sets)[dup] <- paste0(names(sets)[dup], ".", seq_along(names(sets))[dup])
  sets
}

#' Porte d'appariement et de taille des jeux de gènes
#'
#' Les identifiants des sets et de la matrice sont nettoyés (bulk_clean_gene_ids)
#' AVANT l'appariement. Un set est rejeté si :
#'   - moins de TS_BULK_GSVA_OVERLAP_MIN (défaut 20 %) de ses gènes sont
#'     retrouvés dans la matrice — signature d'un décalage d'identifiants ;
#'   - sa taille APPARIÉE sort de [min_size, max_size].
#'
#' @param gene_sets Liste nommée de vecteurs de gènes.
#' @param matrix_genes Vecteur des identifiants (rownames) de la matrice.
#' @param min_size,max_size Bornes de taille appariée.
#' @param overlap_min Fraction minimale de gènes retrouvés (0-1).
#' @return list(sets = jeux retenus (gènes APPARIÉS uniquement),
#'   dropped = data.frame(set, n_genes, n_matched, matched_fraction, reason),
#'   n_input_sets).
bulk_filter_gene_sets <- function(gene_sets, matrix_genes, min_size = 10L, max_size = 500L,
                                  overlap_min = 0.20) {
  if (!is.list(gene_sets) || length(gene_sets) == 0L) {
    stop(errorCondition("bulk_filter_gene_sets() : liste de jeux de gènes vide.",
                        class = "bulk_gsva_error", state = "invalid_input"))
  }
  min_size <- max(1L, as.integer(min_size)); max_size <- max(min_size, as.integer(max_size))
  overlap_min <- min(1, max(0, as.numeric(overlap_min)))

  mat_clean <- unique(bulk_clean_gene_ids(matrix_genes))
  mat_lookup <- mat_clean

  used <- list(); dropped <- list()
  for (nm in names(gene_sets)) {
    genes_raw <- unique(as.character(gene_sets[[nm]]))
    genes_clean <- bulk_clean_gene_ids(genes_raw)
    matched <- unique(intersect(genes_clean, mat_lookup))
    frac <- if (length(genes_clean) > 0L) length(matched) / length(genes_clean) else 0
    reason <- if (length(genes_clean) == 0L) {
      "vide"
    } else if (frac < overlap_min) {
      "recouvrement"
    } else if (length(matched) < min_size) {
      "trop_petit"
    } else if (length(matched) > max_size) {
      "trop_grand"
    }
    if (is.null(reason)) {
      used[[nm]] <- matched
    } else {
      dropped[[length(dropped) + 1L]] <- data.frame(
        set = nm, n_genes = length(genes_clean), n_matched = length(matched),
        matched_fraction = round(frac, 3), reason = reason,
        stringsAsFactors = FALSE)
    }
  }
  list(
    sets = used,
    dropped = if (length(dropped)) do.call(rbind, dropped) else
      data.frame(set = character(0), n_genes = integer(0), n_matched = integer(0),
                 matched_fraction = numeric(0), reason = character(0),
                 stringsAsFactors = FALSE),
    n_input_sets = length(gene_sets)
  )
}

#' Paramètre BiocParallel conforme aux garde-fous du dépôt
#'
#' Sous Windows : SerialParam (jamais MulticoreParam — pas de fork fiable sous
#' Windows, cf. STATUS.md §2h). Sous Unix : MulticoreParam plafonné à
#' TS_BULK_MAX_WORKERS (mémoire 32 Go). `workers` <= 1 force le séquentiel.
#'
#' @param workers Nombre de workers demandé (entier >= 1).
#' @return Objet BiocParallelParam.
bulk_bpparam <- function(workers = 1L) {
  workers <- max(1L, as.integer(workers %||% 1L))
  cap <- if (exists("TS_BULK_MAX_WORKERS", inherits = TRUE)) TS_BULK_MAX_WORKERS else 4L
  workers <- min(workers, cap)
  if (.Platform$OS.type == "windows" || workers <= 1L) {
    BiocParallel::SerialParam()
  } else {
    BiocParallel::MulticoreParam(workers = workers)
  }
}

#' Calculer les scores de voies par échantillon
#'
#' Enveloppe pure de l'API Bioconductor moderne (GSVA 2.x : gsvaParam /
#' ssgseaParam / plageParam / zscoreParam). La matrice d'entrée doit être
#' TRANSFORMÉE (VST / TMM log2-CPM) — les counts bruts sont refusés. Les jeux
#' de gènes sont filtrés (bulk_filter_gene_sets) avant le calcul ; seuls les
#' sets retenus sont passés à GSVA.
#'
#' @param expr_matrix Matrice transformée (gènes x échantillons).
#' @param gene_sets Liste nommée de vecteurs de gènes (GMT parsé ou ressource).
#' @param method Une de bulk_gsva_methods().
#' @param min_size,max_size Bornes de taille appariée d'un set.
#' @param overlap_min Fraction minimale de recouvrement (porte d'identifiants).
#' @param workers Workers BPPARAM (plafonnés, SerialParam sous Windows).
#' @param analysis_id Identifiant d'analyse pour la provenance.
#' @return list(type = "bulk_pathway_scores", status, analysis_id, method,
#'   params, scores (matrice voies x échantillons), gene_sets (retenus),
#'   qc (n_input_sets, n_used_sets, dropped, n_genes_input, n_genes_matched),
#'   warnings, provenance (entrée new_provenance_entry), timestamp_utc).
compute_pathway_scores <- function(expr_matrix, gene_sets, method = "ssgsea",
                                   min_size = NULL, max_size = NULL, overlap_min = NULL,
                                   workers = 1L, analysis_id = "bulk-pathway-scores") {
  # Effet de bord tiers neutralisé (même leçon que STATUS.md §2k) :
  # GSVA::.onLoad() fait options(Matrix.warnDeprecatedCoerce = 2) SANS le
  # restaurer — charger GSVA ici laissait l'option derrière lui pour toute la
  # session, escaladant en erreur fatale toute dépréciation Matrix ULTÉRIEURE
  # d'autres domaines (casse réelle mesurée : les 16 échecs Milo du §2k sont
  # réapparus dès que ce domaine a chargé GSVA en direct). L'option est
  # relevée avant le chargement et restaurée à l'identique à la sortie du
  # calcul (absente -> retirée) ; le namespace GSVA, lui, reste chargé.
  mwd_before <- getOption("Matrix.warnDeprecatedCoerce")
  on.exit(options(Matrix.warnDeprecatedCoerce = mwd_before), add = TRUE)
  if (!requireNamespace("GSVA", quietly = TRUE)) {
    stop(errorCondition(paste0(
      "compute_pathway_scores() : le package 'GSVA' est requis (BiocManager::install('GSVA')). ",
      "L'application démarre sans lui ; cette fonctionnalité reste indisponible tant qu'il manque."),
      class = "bulk_gsva_error", state = "missing_dependency"))
  }
  if (!method %in% bulk_gsva_methods()) {
    stop(errorCondition(sprintf(
      "compute_pathway_scores() : méthode '%s' inconnue (disponibles : %s).",
      method, paste(bulk_gsva_methods(), collapse = ", ")),
      class = "bulk_gsva_error", state = "invalid_input"))
  }
  min_size <- min_size %||%
    (if (exists("TS_BULK_GSVA_MIN_SIZE", inherits = TRUE)) TS_BULK_GSVA_MIN_SIZE else 10L)
  max_size <- max_size %||%
    (if (exists("TS_BULK_GSVA_MAX_SIZE", inherits = TRUE)) TS_BULK_GSVA_MAX_SIZE else 500L)
  overlap_min <- overlap_min %||%
    (if (exists("TS_BULK_GSVA_OVERLAP_MIN", inherits = TRUE)) TS_BULK_GSVA_OVERLAP_MIN else 0.20)

  # Garde anti-counts-bruts réutilisée du domaine QC batch — re-classée dans
  # CE domaine (même message, même state) pour que le module n'attrape qu'une
  # classe d'erreur par domaine.
  tryCatch(
    bulk_assert_transformed_matrix(expr_matrix, context = "scores de voies par échantillon"),
    error = function(e) {
      stop(errorCondition(conditionMessage(e),
                          class = "bulk_gsva_error",
                          state = e$state %||% "invalid_input"))
    }
  )

  filt <- bulk_filter_gene_sets(gene_sets, rownames(expr_matrix),
                                min_size = min_size, max_size = max_size,
                                overlap_min = overlap_min)
  warnings <- character(0)
  n_drop_rec <- sum(filt$dropped$reason == "recouvrement")
  if (n_drop_rec > 0L) {
    warnings <- c(warnings, sprintf(
      paste0("%d jeu(x) rejeté(s) : moins de %.0f %% de leurs gènes retrouvés dans la matrice ",
             "(décalage d'identifiants probable) — voir la table des rejets."),
      n_drop_rec, 100 * overlap_min))
  }
  n_drop_size <- sum(filt$dropped$reason %in% c("trop_petit", "trop_grand"))
  if (n_drop_size > 0L) {
    warnings <- c(warnings, sprintf(
      "%d jeu(x) rejeté(s) hors bornes de taille [%d, %d].", n_drop_size, min_size, max_size))
  }
  if (length(filt$sets) == 0L) {
    stop(errorCondition(paste0(
      "compute_pathway_scores() : aucun jeu de gènes ne survit aux filtres ",
      "(recouvrement >= ", sprintf("%.0f %%", 100 * overlap_min),
      ", taille [", min_size, ", ", max_size, "]) — vérifiez le format des identifiants ",
      "(symboles vs Ensembl) et la taille des sets."),
      class = "bulk_gsva_error", state = "no_gene_sets"))
  }

  scores <- tryCatch({
    par <- switch(method,
      gsva   = GSVA::gsvaParam(exprData = expr_matrix, geneSets = filt$sets,
                               minSize = min_size, maxSize = max_size,
                               kcdf = "Gaussian"),
      ssgsea = GSVA::ssgseaParam(exprData = expr_matrix, geneSets = filt$sets,
                                 minSize = min_size, maxSize = max_size),
      plage  = GSVA::plageParam(exprData = expr_matrix, geneSets = filt$sets,
                                minSize = min_size, maxSize = max_size),
      zscore = GSVA::zscoreParam(exprData = expr_matrix, geneSets = filt$sets,
                                 minSize = min_size, maxSize = max_size)
    )
    # BPPARAM sur gsva() — JAMAIS sur le constructeur (STATUS.md §2h).
    GSVA::gsva(par, BPPARAM = bulk_bpparam(workers), verbose = FALSE)
  }, error = function(e) {
    stop(errorCondition(paste0("compute_pathway_scores() : GSVA a échoué — ",
                               conditionMessage(e)),
                        class = "bulk_gsva_error", state = "compute_failed"))
  })

  # Normalisation défensive du format : matrice voies x échantillons, dimnames.
  if (is.data.frame(scores)) scores <- as.matrix(scores)
  if (is.null(dim(scores)) || any(dim(scores) == 0L)) {
    stop(errorCondition("compute_pathway_scores() : GSVA a renvoyé une matrice vide.",
                        class = "bulk_gsva_error", state = "compute_failed"))
  }
  if (ncol(scores) == nrow(expr_matrix) && nrow(scores) == ncol(expr_matrix)) {
    # GSVA 2.x peut renvoyer échantillons x voies selon la classe d'entrée :
    # le contrat impose voies (lignes) x échantillons (colonnes).
    scores <- t(scores)
  }
  colnames(scores) <- colnames(expr_matrix)

  n_genes_matched <- length(unique(unlist(filt$sets)))
  provenance <- new_provenance_entry(
    analysis_id = analysis_id,
    method      = paste0("GSVA::", method),
    parameters  = list(min_size = min_size, max_size = max_size,
                       overlap_min = overlap_min, workers = as.integer(workers),
                       n_gene_sets = length(filt$sets)),
    dataset     = expr_matrix,
    warnings    = warnings
  )

  list(
    type          = "bulk_pathway_scores",
    status        = "valid",
    analysis_id   = analysis_id,
    method        = method,
    params        = list(min_size = min_size, max_size = max_size,
                         overlap_min = overlap_min,
                         matrix = if (isTRUE(all.equal(rownames(expr_matrix),
                                                       bulk_clean_gene_ids(rownames(expr_matrix))))) "symboles/nettoyés" else "bruts"),
    scores        = scores,
    gene_sets     = filt$sets,
    qc            = list(n_input_sets = filt$n_input_sets,
                         n_used_sets = length(filt$sets),
                         dropped = filt$dropped,
                         n_genes_input = nrow(expr_matrix),
                         n_genes_matched = n_genes_matched,
                         n_samples = ncol(expr_matrix)),
    warnings      = warnings,
    provenance    = provenance,
    timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

#' Export plat des scores (une ligne par voie x échantillon)
#'
#' @param result Résultat compute_pathway_scores().
#' @return data.frame(pathway, sample, score, method, analysis_id).
build_pathway_scores_export <- function(result) {
  if (!is.list(result) || !identical(result$type, "bulk_pathway_scores")) {
    stop(errorCondition("build_pathway_scores_export() : résultat de scores invalide.",
                        class = "bulk_gsva_error", state = "invalid_input"))
  }
  s <- result$scores
  grid <- expand.grid(pathway = rownames(s), sample = colnames(s),
                      stringsAsFactors = FALSE)
  grid$score    <- as.vector(s[cbind(match(grid$pathway, rownames(s)),
                                     match(grid$sample, colnames(s)))])
  grid$method   <- result$method
  grid$analysis_id <- result$analysis_id
  grid[order(grid$pathway, grid$sample), ]
}

#' PCA de la matrice de scores (échantillons dans l'espace des voies)
#'
#' @param result Résultat compute_pathway_scores().
#' @param metadata Métadonnées échantillons (rownames = échantillons).
#' @param color_by Colonne de coloration (NULL autorisé).
#' @param tr Fonction de traduction.
#' @return ggplot.
plot_pathway_scores_pca <- function(result, metadata = NULL, color_by = NULL, tr = NULL) {
  tr <- tr %||% function(x) x
  s <- result$scores
  if (ncol(s) < 3L) {
    stop(errorCondition("plot_pathway_scores_pca() : au moins 3 échantillons requis pour une PCA.",
                        class = "bulk_gsva_error", state = "invalid_input"))
  }
  pca <- prcomp(t(s), scale. = TRUE)
  pct <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
  df <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2], sample = colnames(s),
                   stringsAsFactors = FALSE)
  has_color <- !is.null(color_by) && !is.null(metadata) &&
    color_by %in% colnames(metadata) && any(df$sample %in% rownames(metadata))
  if (has_color) df$color <- as.character(metadata[df$sample, color_by])

  p <- if (has_color) {
    ggplot2::ggplot(df, ggplot2::aes(PC1, PC2, color = color)) +
      ggplot2::geom_point(size = 3.2, alpha = 0.85)
  } else {
    ggplot2::ggplot(df, ggplot2::aes(PC1, PC2)) +
      ggplot2::geom_point(size = 3.2, alpha = 0.85, color = "#2C3E50")
  }
  p + ggplot2::geom_text(ggplot2::aes(label = sample), size = 2.8, vjust = -1,
                         check_overlap = TRUE, show.legend = FALSE,
                         color = "grey20") +
    ggplot2::labs(
      title = tr("PCA des scores de voies (par échantillon)"),
      subtitle = tr(sprintf("Méthode : %s — %d voies", result$method, nrow(s))),
      x = paste0("PC1 (", pct[1], "%)"), y = paste0("PC2 (", pct[2], "%)"),
      color = if (has_color) color_by else NULL) +
    ts_theme("minimal", 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 13))
}

#' Heatmap clusterisée des scores de voies (top voies par variance)
#'
#' @param result Résultat compute_pathway_scores().
#' @param top_n Nombre de voies (les plus variables) affichées.
#' @param scale_rows Centrer-réduire les voies (recommandé).
#' @param tr Fonction de traduction.
#' @return Objet ComplexHeatmap (si disponible) sinon ggplot de repli.
plot_pathway_scores_heatmap <- function(result, top_n = 50, scale_rows = TRUE, tr = NULL) {
  tr <- tr %||% function(x) x
  s <- result$scores
  if (scale_rows && nrow(s) > 1L) {
    s <- t(scale(t(s)))
    s[!is.finite(s)] <- 0
  }
  if (nrow(s) > top_n) {
    rv <- matrixStats::rowVars(result$scores)
    s <- s[order(rv, decreasing = TRUE)[seq_len(top_n)], , drop = FALSE]
  }
  if (requireNamespace("ComplexHeatmap", quietly = TRUE)) {
    ComplexHeatmap::Heatmap(
      s, name = "Score",
      col = circlize::colorRamp2(c(min(s), 0, max(s)),
                                 c("#2166AC", "white", "#B2182B")),
      show_row_names = nrow(s) <= 60,
      column_title = tr(sprintf("Scores de voies — %s (%d voies)",
                                result$method, nrow(s)))
    )
  } else {
    melted <- reshape2::melt(s, varnames = c("pathway", "sample"))
    ggplot2::ggplot(melted, ggplot2::aes(sample, pathway, fill = value)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                                    midpoint = 0, name = tr("Score")) +
      ggplot2::labs(title = tr("Scores de voies")) +
      ts_theme("minimal", 11) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }
}

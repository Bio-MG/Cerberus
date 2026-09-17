# =============================================================================
# helpers_sc.R — Single-cell plotting & analysis helpers (Seurat)
# =============================================================================
# Extracted from global.R (refactor, session post-v1.0 Bulk) — pure functions
# used by the mod_sc_* family (mod_sc_viz.R, mod_sc_corr.R, mod_sc_markers.R,
# mod_sc_trajectory.R). No Shiny reactivity here.
#
# Contents:
#   - Plots      : plot_enhanced_scatter(), plot_violin_enhanced(),
#                  plot_multi_sample(), plot_correlation_matrix(),
#                  plot_trajectory(), plot_slingshot_trajectory(),
#                  plot_gene_correlation_network(),
#                  plot_pseudotime_distribution(), plot_genes_vs_pseudotime()
#   - Analysis   : find_correlated_genes(), calculate_pseudotime()
#                  (exploratory kNN graph), calculate_slingshot_pseudotime()
#                  (Slingshot lineage inference — distinct method),
#                  subsample_seurat_for_analysis()
#   - DT tables  : build_markers_dt(), build_corr_dt()
#   - Internal   : .get_norm_matrix() (Seurat v4/v5-safe assay extraction)
#
# Pathway enrichment (ORA/GSEA) is shared with bulk — see helpers_pathway.R,
# NOT duplicated here even though it's used by mod_sc_pathways.R too.
#
# Step-3.7 note: the SC visualization builder build_sc_viz_plot(obj, cfg,
# sc_palette, manual_colors) lives ONLY in mod_sc_viz.R now. A duplicate
# 2-arg build_sc_viz_plot(obj, cfg) used to live here too — since app.R
# source()s helpers_sc.R BEFORE mod_sc_viz.R, the later definition always
# silently shadowed this one (same global env), making it 100% dead code.
# Removed to avoid confusion/drift between the two copies.
#
# Depends on: Seurat, ggplot2, igraph, viridis, scales, DT, tidyr.
# =============================================================================







plot_enhanced_scatter <- function(seurat_obj, feature1, feature2,

                                   group.by = NULL, method = "pearson",

                                   add_smooth = TRUE, pt.size = 1, tr = NULL) {
  tr <- tr %||% function(x) x

  

  if(!feature1 %in% rownames(seurat_obj) && !feature1 %in% colnames(seurat_obj@meta.data)) {

    stop(errorCondition(paste("Feature non trouvée:", feature1), class = "sc_helpers_error"))

  }

  if(!feature2 %in% rownames(seurat_obj) && !feature2 %in% colnames(seurat_obj@meta.data)) {

    stop(errorCondition(paste("Feature non trouvée:", feature2), class = "sc_helpers_error"))

  }

  

  if(feature1 %in% rownames(seurat_obj)) {

    data1 <- FetchData(seurat_obj, vars = feature1)[,1]

  } else {

    data1 <- seurat_obj@meta.data[[feature1]]

  }

  

  if(feature2 %in% rownames(seurat_obj)) {

    data2 <- FetchData(seurat_obj, vars = feature2)[,1]

  } else {

    data2 <- seurat_obj@meta.data[[feature2]]

  }

  

  plot_df <- data.frame(

    x = data1,

    y = data2,

    stringsAsFactors = FALSE

  )

  

  if(!is.null(group.by) && group.by %in% colnames(seurat_obj@meta.data)) {

    plot_df$group <- seurat_obj@meta.data[[group.by]]

  } else {

    plot_df$group <- "All Cells"

  }

  

  cor_test <- cor.test(plot_df$x, plot_df$y, method = method)

  cor_value <- round(cor_test$estimate, 3)

  p_value <- format(cor_test$p.value, scientific = TRUE, digits = 3)

  

  stat_label <- paste0(

    toupper(method), " r = ", cor_value,

    "\n", tr("p-value = "), p_value

  )

  

  p <- ggplot(plot_df, aes(x = x, y = y, color = group)) +

    geom_point(size = pt.size, alpha = 0.6) +

    labs(

      x = feature1,

      y = feature2,

      title = paste(paste0(tr("Corrélation:"), " "), feature1, tr("vs"), feature2),

      color = if(!is.null(group.by)) group.by else NULL

    ) +

    theme_minimal() +

    theme(

      plot.title = element_text(face = "bold", size = 14),

      legend.position = "right",

      panel.grid.minor = element_blank()

    )

  

  if(add_smooth) {

    p <- p + geom_smooth(method = "lm", se = TRUE, color = "red", 

                         linetype = "dashed", linewidth = 0.8,

                         aes(group = 1))

  }

  

  p <- p + annotate("text", 

                    x = min(plot_df$x) + 0.1 * diff(range(plot_df$x)),

                    y = max(plot_df$y) - 0.05 * diff(range(plot_df$y)),

                    label = stat_label,

                    hjust = 0, vjust = 1,

                    size = 4, fontface = "bold",

                    color = "black")

  

  return(p)

}



plot_violin_enhanced <- function(seurat_obj, features, group.by = "seurat_clusters",

                                 add_boxplot = TRUE, split.by = NULL, tr = NULL) {
  tr <- tr %||% function(x) x

  

  valid_features <- intersect(features, rownames(seurat_obj))

  if(length(valid_features) == 0) {

    stop(errorCondition("Aucun gène valide trouvé", class = "sc_helpers_error"))

  }

  

  features_use <- head(valid_features, 6)

  

  p <- VlnPlot(

    seurat_obj, 

    features = features_use,

    group.by = group.by,

    split.by = split.by,

    pt.size = 0,

    ncol = min(3, length(features_use))

  )

  

  if(add_boxplot && length(features_use) == 1) {

    data_plot <- FetchData(seurat_obj, vars = c(features_use[1], group.by))

    colnames(data_plot) <- c("expression", "group")

    

    p <- ggplot(data_plot, aes(x = group, y = expression, fill = group)) +

      geom_violin(trim = FALSE, alpha = 0.7, scale = "width") +

      geom_boxplot(width = 0.15, fill = "white", outlier.shape = NA, 

                   alpha = 0.8, coef = 0) +

      labs(title = paste(tr("Expression de"), features_use[1]),

           x = group.by, y = tr("Expression Normalisée")) +

      theme_minimal() +

      theme(

        axis.text.x = element_text(angle = 45, hjust = 1),

        legend.position = "none",

        plot.title = element_text(face = "bold")

      )

  }

  

  return(p)

}



plot_multi_sample <- function(seurat_obj, gene, plot_type = "violin", tr = NULL) {
  tr <- tr %||% function(x) x

  

  if(!gene %in% rownames(seurat_obj)) {

    stop(errorCondition(paste("Gène non trouvé:", gene), class = "sc_helpers_error"))

  }

  

  n_samples <- length(unique(seurat_obj$orig.ident))

  if(n_samples < 2) {

    stop(errorCondition("Au moins 2 échantillons requis pour la comparaison", class = "sc_helpers_error"))

  }

  

  plot_data <- data.frame(

    expression = FetchData(seurat_obj, vars = gene)[,1],

    sample = seurat_obj$orig.ident,

    cluster = Idents(seurat_obj)

  )

  

  p <- ggplot(plot_data, aes(x = sample, y = expression, fill = sample)) +

    labs(title = paste(tr("Expression de"), gene, tr("par Échantillon")),

         x = tr("Échantillon"), y = tr("Expression Normalisée")) +

    theme_minimal() +

    theme(

      axis.text.x = element_text(angle = 45, hjust = 1),

      legend.position = "none",

      plot.title = element_text(face = "bold", size = 14)

    )

  

  if(plot_type == "violin") {

    p <- p + geom_violin(trim = FALSE, alpha = 0.7) +

      geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA)

  } else if(plot_type == "box") {

    p <- p + geom_boxplot(outlier.alpha = 0.3)

  } else if(plot_type == "jitter") {

    p <- p + geom_jitter(width = 0.2, alpha = 0.4, size = 0.5)

  }

  

  return(p)

}



#' Safe normalized matrix extraction (Seurat v4/v5 compatible)

.get_norm_matrix <- function(obj) {

  assay_use <- DefaultAssay(obj)

  mat <- tryCatch(

    GetAssayData(obj, layer = "data", assay = assay_use),

    error = function(e) GetAssayData(obj, slot = "data", assay = assay_use)

  )

  if (inherits(mat, "dgCMatrix")) mat <- as.matrix(mat)

  mat

}






#' Find Correlated Genes

#' @param seurat_obj Objet Seurat

#' @param target_gene Gène cible

#' @param method Méthode de corrélation ("pearson", "spearman")

#' @param threshold Seuil de corrélation (absolue)

#' @param top_n Nombre maximum de gènes à retourner

#' @return data.frame avec gènes corrélés et statistiques

find_correlated_genes <- function(seurat_obj, target_gene, 

                                  method = "pearson", 

                                  threshold = 0.3, 

                                  top_n = 50) {

  

  # Validation

  if(!target_gene %in% rownames(seurat_obj)) {

    stop(errorCondition(paste("Gène non trouvé:", target_gene), class = "sc_helpers_error"))

  }

  

  # Extraire expression normalisée

  expr_matrix <- .get_norm_matrix(seurat_obj)

  

  # Convertir en matrice dense si sparse

  if(inherits(expr_matrix, "dgCMatrix")) {

    expr_matrix <- as.matrix(expr_matrix)

  }

  

  # Expression du gène cible

  target_expr <- as.numeric(expr_matrix[target_gene, ])

  

  # Calculer corrélations avec tous les autres gènes

  all_genes <- rownames(expr_matrix)

  all_genes <- setdiff(all_genes, target_gene)

  

  # Pour performance, limiter aux gènes variables si > 5000 gènes

  if(length(all_genes) > 5000) {

    var_genes <- VariableFeatures(seurat_obj)

    if(length(var_genes) > 0) {

      all_genes <- intersect(all_genes, var_genes)

    }

  }

  

  # FIX: Calcul avec gestion des ex-aequos

  cor_results <- lapply(all_genes, function(g) {

    gene_expr <- as.numeric(expr_matrix[g, ])

    

    # Éviter gènes avec variance nulle

    if(sd(gene_expr) == 0 || sd(target_expr) == 0) {

      return(data.frame(cor = 0, pval = 1))

    }

    

    # Utiliser exact=FALSE pour éviter warning avec ex-aequos

    test <- tryCatch({

      cor.test(target_expr, gene_expr, method = method, exact = FALSE)

    }, error = function(e) {

      list(estimate = 0, p.value = 1)

    })

    

    data.frame(

      cor = as.numeric(test$estimate),

      pval = as.numeric(test$p.value)

    )

  })

  

  # Combiner résultats

  cor_df <- do.call(rbind, cor_results)

  cor_df$gene <- all_genes

  cor_df$abs_correlation <- abs(cor_df$cor)

  colnames(cor_df)[1:2] <- c("correlation", "p_value")

  

  # Filtrer par seuil

  cor_df <- cor_df[cor_df$abs_correlation >= threshold, ]

  

  # Ajuster p-values

  if(nrow(cor_df) > 0) {

    cor_df$p_adj <- p.adjust(cor_df$p_value, method = "BH")

  } else {

    return(data.frame(

      gene = character(0),

      correlation = numeric(0),

      p_value = numeric(0),

      abs_correlation = numeric(0),

      p_adj = numeric(0)

    ))

  }

  

  # Trier et limiter

  cor_df <- cor_df[order(-cor_df$abs_correlation), ]

  cor_df <- head(cor_df, top_n)

  

  return(cor_df)

}



#' Correlation Matrix Plot

#' @param seurat_obj Objet Seurat

#' @param features Vecteur de gènes (max 50)

#' @param method Méthode corrélation

#' @return ggplot heatmap

plot_correlation_matrix <- function(seurat_obj, features, method = "pearson",

                                     low_color = "#2166AC", mid_color = "white",

                                     high_color = "#B2182B", tr = NULL) {
  tr <- tr %||% function(x) x

  

  # Validation

  features <- intersect(features, rownames(seurat_obj))

  if(length(features) < 2) {

    stop(errorCondition("Au moins 2 gènes requis", class = "sc_helpers_error"))

  }

  

  # Limiter à 50 gènes pour performance

  if(length(features) > 50) {

    warning("Limitation à 50 gènes pour performance")

    features <- head(features, 50)

  }

  

  # Extraire expression

  expr_matrix <- .get_norm_matrix(seurat_obj)

  

  # Sous-ensemble et transposer

  expr_subset <- as.matrix(t(expr_matrix[features, ]))

  

  # Calculer matrice corrélation

  cor_matrix <- cor(expr_subset, method = method, use = "pairwise.complete.obs")

  

  # Convertir pour ggplot

  cor_melt <- reshape2::melt(cor_matrix)

  colnames(cor_melt) <- c("Gene1", "Gene2", "Correlation")

  

  # Plot

  p <- ggplot(cor_melt, aes(x = Gene1, y = Gene2, fill = Correlation)) +

    geom_tile(color = "white", size = 0.5) +

    geom_text(aes(label = sprintf("%.2f", Correlation)), 

              size = 3, color = "black") +

    scale_fill_gradient2(

      low = low_color, mid = mid_color, high = high_color,

      midpoint = 0, limits = c(-1, 1),

      name = tr("Corrélation")

    ) +

    labs(

      title = paste("Matrice de Corrélation (", method, ")", sep = ""),

      x = NULL, y = NULL

    ) +

    theme_minimal() +

    theme(

      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),

      axis.text.y = element_text(size = 9),

      plot.title = element_text(face = "bold", size = 14),

      panel.grid = element_blank(),

      legend.position = "right"

    ) +

    coord_fixed()

  

  return(p)

}






#' Calculate Slingshot lineage pseudotime
#'
#' Runs Slingshot on a reduced embedding matrix and cluster labels.
#' This is distinct from exploratory weighted kNN pseudotime:
#' calculate_pseudotime() is an exploratory graph ordering that infers NO
#' lineage, whereas this helper delegates to the actual Slingshot
#' lineage-inference algorithm (slingshot::slingshot()).
#'
#' @param embeddings Numeric matrix with cells in rows.
#' @param cluster_labels Cluster labels, one per cell.
#' @param start_cluster Optional starting cluster.
#' @param end_clusters Optional terminal clusters.
#' @param reduction Character reduction name for provenance.
#'
#' @return A list containing pseudotime, pseudotime_matrix, curve_weights,
#' lineages, curves, computation_reduction, method, root_method,
#' root_cluster, n_cells.
#' @export







#' Plot Slingshot pseudotime
#'
#' Cells coloured by the DEFAULT Slingshot lineage pseudotime (first
#' lineage — a compatibility default, not a biologically validated
#' selection). Deliberately separate from plot_trajectory(): no kNN graph
#' edges and no root-cell marker are drawn here.
#'
#' @param embeddings Display coordinates.
#' @param pseudotime Numeric pseudotime vector.
#' @param curves Optional Slingshot curves. Accepted for API completeness;
#'   curve overlays are only drawn when every curve exposes a numeric
#'   coordinate matrix compatible with the display space, otherwise they
#'   are silently skipped (no fabricated curves).
#'
#' @return A ggplot object.
#' @export




#' Gene correlation network plot (igraph), factored out of mod_sc_corr.R for

#' reuse by the Single-Cell report — one source of truth for the network

#' rendering so the in-app plot and the exported report never diverge.

#'

#' @param corr_df data.frame from find_correlated_genes() (gene, correlation, abs_correlation).

#' @param target_gene Character, the reference gene at the center of the network.

#' @param top_n Number of top-correlated genes to display as nodes.

plot_gene_correlation_network <- function(corr_df, target_gene, top_n = 20, tr = NULL) {
  tr <- tr %||% function(x) x

  top <- head(corr_df, top_n)

  edges <- data.frame(

    from        = target_gene,

    to          = top$gene,

    weight      = top$abs_correlation,

    correlation = top$correlation

  )

  g <- igraph::graph_from_data_frame(edges, directed = FALSE)



  edge_colors <- ifelse(edges$correlation > 0, "#27AE60", "#E74C3C")

  edge_widths <- scales::rescale(edges$weight, to = c(1, 5))

  layout      <- igraph::layout_with_fr(g)

  node_sizes  <- ifelse(igraph::V(g)$name == target_gene, 15, 8)

  node_colors <- ifelse(igraph::V(g)$name == target_gene, "#3498DB", "#95A5A6")



  par(mar = c(0, 0, 2, 0))

  plot(

    g,

    layout              = layout,

    vertex.size         = node_sizes,

    vertex.color        = node_colors,

    vertex.label.cex    = 0.7,

    vertex.label.color  = "black",

    vertex.label.family = "sans",

    edge.color          = edge_colors,

    edge.width          = edge_widths,

    main                = paste(paste0(tr("Reseau de Correlation -"), " "), target_gene),

    edge.curved         = 0.2

  )

  legend("bottomleft",

         legend = c(tr("Correlation positive"), tr("Correlation negative"), tr("Gene cible")),

         col    = c("#27AE60", "#E74C3C", "#3498DB"),

         pch    = c(15, 15, 19), pt.cex = c(2, 2, 2.5),

         bty = "n", cex = 0.9)

}



#' Standardized FindAllMarkers results DT table (shared by mod_sc_markers.R

#' and the future Single-Cell HTML/PDF report — same rationale as

#' build_de_results_dt: one source of truth for table formatting).

#'

#' @param df Markers data.frame (gene, cluster, avg_log2FC, p_val_adj, pct.1, pct.2).

#' @return A DT::datatable object.

build_markers_dt <- function(df) {

  df_display <- data.frame(

    Gene    = df$gene,

    Cluster = df$cluster,

    Log2FC  = round(df$avg_log2FC, 3),

    P_adj   = format(df$p_val_adj, scientific = TRUE, digits = 3),

    Pct.1   = round(df$pct.1 * 100, 1),

    Pct.2   = round(df$pct.2 * 100, 1),

    stringsAsFactors = FALSE

  )

  ts_datatable(df_display, page_length = 15L, filename_base = "sc_markers_table") %>%

    DT::formatStyle(

      "Log2FC",

      backgroundColor = DT::styleColorBar(range(df_display$Log2FC), "lightblue"),

      backgroundSize  = "98% 88%", backgroundRepeat = "no-repeat", backgroundPosition = "center"

    ) %>%

    DT::formatStyle("P_adj", color = DT::styleInterval(c(0.001, 0.01, 0.05),

                                                        c("darkgreen", "green", "orange", "red")))

}



#' Standardized gene-correlation results DT table (Step-3.7) — mirrors

#' build_markers_dt(). Shared by mod_sc_corr.R's live table AND the new

#' "Table des Corrélations" report section (sc_report_template.Rmd), so the
#' exported table always matches what was shown live.

#'

#' @param df data.frame from find_correlated_genes() (gene, correlation, p_value, abs_correlation, p_adj).

#' @return A DT::datatable object.

build_corr_dt <- function(df) {

  df_display <- data.frame(

    Gene        = df$gene,

    Correlation = round(df$correlation, 3),

    `|r|`       = round(df$abs_correlation, 3),

    P_value     = format(df$p_value, scientific = TRUE, digits = 2),

    P_adj       = format(df$p_adj, scientific = TRUE, digits = 2),

    check.names = FALSE

  )

  ts_datatable(df_display, page_length = 15L, filename_base = "sc_corr_table",

                selection = list(mode = "none")) %>%

    DT::formatStyle("Correlation",

                    background = DT::styleColorBar(range(df_display$Correlation), "lightblue"),

                    backgroundSize = "98% 88%", backgroundRepeat = "no-repeat",

                    backgroundPosition = "center") %>%

    DT::formatStyle("P_adj", color = DT::styleInterval(c(0.001, 0.01, 0.05),

                                                        c("darkgreen", "green", "orange", "red")))

}



#' Pseudotime distribution plot (density per cluster) — Step-3.7: factored

#' out of mod_sc_trajectory.R so the live plot, its PNG/PDF export, AND the

#' HTML/PDF report all call the exact same code (previous version had 3

#' near-identical ggplot blocks copy-pasted across renderPlot()/downloadHandler,

#' and the downloadHandler ones were broken — they called `output$xxx()` which

#' is not how you retrieve a renderPlot's underlying ggplot object).

#'

#' @param seurat_obj Seurat object with a $pseudotime + $seurat_clusters column.

#' @return ggplot object.




#' Gene expression vs pseudotime plot (Step-3.7) — factored out of

#' mod_sc_trajectory.R, single source of truth reused by the live render

#' (no more "Tracer" button — updates as soon as genes are picked), its

#' PNG/PDF export, AND the new "Gènes vs Pseudotemps" report section.

#'

#' @param seurat_obj Seurat object with a $pseudotime column already computed.

#' @param genes Character vector of gene symbols to trace (max 8 kept).

#' @param smooth_method "loess", "gam", or "lm".

#' @return ggplot object (faceted, one panel per gene).




#' Cap the number of cells used for a heavy per-cell computation
#' (FindAllMarkers, find_correlated_genes, ...) — RAM/CPU safety net for large
#' Single-Cell objects on a 32Go CPU-only machine.
#'
#' Stratifies by `group_col` (default: cluster) rather than a flat random
#' subsample of the whole object, so rare cell populations are not wiped out
#' by a global cap. Falls back to a flat/global subsample if `group_col`
#' doesn't exist in the object's metadata (e.g. correlation subsampling by
#' "orig.ident" on a single-sample object).
#'
#' @param obj Seurat object.
#' @param max_per_group Max cells to keep per level of `group_col`.
#'   `Inf`, `NA`, or `<= 0` disables subsampling entirely (returns obj as-is).
#' @param group_col Metadata column to stratify by (default "seurat_clusters").
#' @param seed Random seed, for reproducibility across re-runs of the same step.
#' @return list(object, n_before, n_after, was_subsampled)
subsample_seurat_for_analysis <- function(obj, max_per_group = Inf,
                                          group_col = "seurat_clusters", seed = 1) {
  n_before <- ncol(obj)

  if (is.null(max_per_group) || is.na(max_per_group) || !is.finite(max_per_group) || max_per_group <= 0) {
    return(list(object = obj, n_before = n_before, n_after = n_before, was_subsampled = FALSE))
  }

  set.seed(seed)

  if (group_col %in% colnames(obj@meta.data)) {
    groups <- obj@meta.data[[group_col]]
    keep_cells <- unlist(lapply(split(Cells(obj), groups), function(cells) {
      if (length(cells) > max_per_group) sample(cells, max_per_group) else cells
    }), use.names = FALSE)
  } else {
    all_cells  <- Cells(obj)
    keep_cells <- if (length(all_cells) > max_per_group) sample(all_cells, max_per_group) else all_cells
  }

  n_after <- length(keep_cells)
  if (n_after >= n_before) {
    return(list(object = obj, n_before = n_before, n_after = n_before, was_subsampled = FALSE))
  }

  list(object = subset(obj, cells = keep_cells),
       n_before = n_before, n_after = n_after, was_subsampled = TRUE)
}



#' Remap a Seurat object's RNA assay rownames (Ensembl/Entrez) to gene symbols
#'
#' Mirrors remap_gene_ids_to_symbol() (helpers_io.R, used for Bulk) but is
#' RAM-safe for large sparse SC matrices: duplicate-symbol collapsing is done
#' via sparse indicator-matrix multiplication (t(G) %*% counts), never
#' densifying the (genes x cells) matrix -- unlike the Bulk version which
#' densifies (`mode(mat) <- "numeric"`), acceptable there because bulk
#' matrices are small. Rebuilds a FRESH Seurat object from raw counts only;
#' any existing normalization/PCA/UMAP/clusters must be recomputed after
#' (same constraint as Bulk Step-0 mapping vs Step-1 filter/VST) -- this MUST
#' run BEFORE "1. Pipeline".
#'
#' @param obj Seurat object (RNA assay, raw counts).
#' @param from_type "ensembl" or "entrez".
#' @param organism "human" or "mouse".
#' @param collapse_method "sum" (recommended) or "max_mean".
#' @param strip_version Strip Ensembl ".N" version suffix before mapping.
#' @return list(object, n_mapped, n_unmapped, n_collapsed)
remap_seurat_ids_to_symbol <- function(obj, from_type = "ensembl", organism = "human",
                                       collapse_method = "sum", strip_version = TRUE) {
  if (!from_type %in% c("ensembl", "entrez")) {
    stop(errorCondition("from_type doit etre 'ensembl' ou 'entrez' pour Single-Cell.", class = "sc_helpers_error"))
  }
  orgdb <- if (organism == "human") {
    if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) stop(errorCondition("Package 'org.Hs.eg.db' requis.", class = "sc_helpers_error"))
    org.Hs.eg.db::org.Hs.eg.db
  } else {
    if (!requireNamespace("org.Mm.eg.db", quietly = TRUE)) stop(errorCondition("Package 'org.Mm.eg.db' requis.", class = "sc_helpers_error"))
    org.Mm.eg.db::org.Mm.eg.db
  }
  from_key <- switch(from_type, ensembl = "ENSEMBL", entrez = "ENTREZID")
  
  counts <- tryCatch(
    GetAssayData(obj, assay = "RNA", layer = "counts"),
    error = function(e) GetAssayData(obj, assay = "RNA", slot = "counts")
  )
  ids_clean <- trimws(rownames(counts))
  if (strip_version && from_type == "ensembl") ids_clean <- gsub("\\.[0-9]+$", "", ids_clean)

  # Step-3.8B: cross-check the SELECTED organism against what the IDs
  # actually look like, and auto-correct (loud warning, not a silent swap)
  # BEFORE the pre-flight pattern check below. The old pre-flight pattern
  # ("^ENS(MUS)?G...") matched BOTH species agnostically, so a wrong
  # `organism` (e.g. UI default "human" on a real ENSMUSG mouse dataset)
  # sailed straight through it and only failed downstream inside
  # AnnotationDbi::select() with an opaque "not valid keys" error -- this was
  # the actual root cause on the 1.3M-neurons mouse test.
  if (from_type == "ensembl") {
    detected_org <- tryCatch(detect_organism_from_ids(ids_clean), error = function(e) NA_character_)
    if (!is.na(detected_org) && detected_org != organism) {
      warning(sprintf(
        "Organisme '%s' selectionne mais les identifiants ressemblent a '%s' (ex: %s) -- organisme corrige automatiquement.",
        organism, detected_org, paste(head(ids_clean, 3), collapse = ", ")))
      organism <- detected_org
      orgdb <- if (organism == "human") {
        if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) stop(errorCondition("Package 'org.Hs.eg.db' requis.", class = "sc_helpers_error"))
        org.Hs.eg.db::org.Hs.eg.db
      } else {
        if (!requireNamespace("org.Mm.eg.db", quietly = TRUE)) stop(errorCondition("Package 'org.Mm.eg.db' requis.", class = "sc_helpers_error"))
        org.Mm.eg.db::org.Mm.eg.db
      }
    }
  }

  expected_pattern <- switch(from_type,
    ensembl = if (organism == "mouse") "^ENSMUSG[0-9]{6,}" else "^ENSG[0-9]{6,}",
    entrez  = "^[0-9]+$")
  sample_ids <- head(ids_clean[!is.na(ids_clean)], 200)
  pct_match  <- if (length(sample_ids) > 0) mean(grepl(expected_pattern, sample_ids)) else 0
  if (pct_match < 0.05) {
    stop(errorCondition(sprintf(
      paste0("Vos identifiants ne ressemblent pas a des ID '%s' pour l'organisme '%s' ",
             "(%.0f%% correspondent au format attendu %s). Exemples : %s."),
      from_type, organism, pct_match * 100, expected_pattern,
      paste(head(sample_ids, 5), collapse = ", ")
    ), class = "sc_helpers_error"))
  }
  
  map_df <- tryCatch(
    AnnotationDbi::select(orgdb, keys = ids_clean, keytype = from_key, columns = "SYMBOL"),
    error = function(e) stop(errorCondition(sprintf(
      "Echec du mapping d'identifiants (organisme '%s') : %s\nExemples testes : %s.",
      organism, conditionMessage(e), paste(head(sample_ids, 5), collapse = ", ")), class = "sc_helpers_error"))
  )
  map_df <- map_df[!is.na(map_df$SYMBOL), ]
  map_df <- map_df[!duplicated(map_df[[from_key]]), ]
  id_to_symbol <- setNames(map_df$SYMBOL, map_df[[from_key]])
  
  mapped_symbols <- id_to_symbol[ids_clean]
  keep       <- !is.na(mapped_symbols)
  n_mapped   <- sum(keep)
  n_unmapped <- sum(!keep)
  if (n_mapped == 0) stop(errorCondition(sprintf(
    "Aucun gene n'a pu etre converti en symbole (organisme '%s', %d IDs testes). Exemples : %s.",
    organism, length(ids_clean), paste(head(sample_ids, 5), collapse = ", ")), class = "sc_helpers_error"))
  
  mat  <- counts[keep, , drop = FALSE]
  syms <- unname(mapped_symbols[keep])
  n_before_collapse <- nrow(mat)
  uniq_syms <- unique(syms)
  
  if (collapse_method == "sum") {
    # Sparse-safe collapse: G is a (n_genes x n_unique_symbols) 0/1 indicator;
    # t(G) %*% mat sums duplicate-symbol rows without ever densifying `mat`.
    G <- Matrix::sparseMatrix(
      i = seq_along(syms), j = match(syms, uniq_syms), x = 1,
      dims = c(length(syms), length(uniq_syms))
    )
    new_mat <- Matrix::t(G) %*% mat
    rownames(new_mat) <- uniq_syms
  } else {
    mean_expr  <- Matrix::rowMeans(mat)
    ord        <- order(syms, -mean_expr)
    mat_ord    <- mat[ord, , drop = FALSE]
    syms_ord   <- syms[ord]
    keep_first <- !duplicated(syms_ord)
    new_mat    <- mat_ord[keep_first, , drop = FALSE]
    rownames(new_mat) <- syms_ord[keep_first]
  }
  
  meta_keep <- obj@meta.data[, intersect("orig.ident", colnames(obj@meta.data)), drop = FALSE]
  new_obj   <- CreateSeuratObject(counts = new_mat, meta.data = meta_keep, project = Project(obj))
  if ("orig.ident" %in% colnames(meta_keep)) new_obj$orig.ident <- meta_keep$orig.ident
  
  list(object = new_obj, n_mapped = n_mapped, n_unmapped = n_unmapped,
       n_collapsed = n_before_collapse - nrow(new_mat))
}

# =============================================================================
# Step-3.8A — Sketch presets (large-dataset speed/precision tradeoff)
# =============================================================================

#' Resolve a sketch-size preset into concrete SketchData/RunPCA parameters
#'
#' @param preset One of "fast","light","medium","standard","high","max","custom".
#' @param n_total_cells Total cells in the object (post-QC), used to cap ncells
#'   and to fall back to "no sketch" when the dataset is already small.
#' @param custom_ncells Used only when preset == "custom".
#' @return List with ncells, max_per_cluster, npcs.
resolve_sketch_preset <- function(preset, n_total_cells, custom_ncells = NULL) {
  presets <- list(
    fast     = list(ncells = 5000,   max_per_cluster = 300,  npcs = 15),
    light    = list(ncells = 10000,  max_per_cluster = 500,  npcs = 20),
    medium   = list(ncells = 25000,  max_per_cluster = 1000, npcs = 20),
    standard = list(ncells = 50000,  max_per_cluster = 1000, npcs = 20),
    high     = list(ncells = 100000, max_per_cluster = 2000, npcs = 30),
    max      = list(ncells = n_total_cells, max_per_cluster = 3000, npcs = 30)
  )

  if (identical(preset, "custom")) {
    ncells <- custom_ncells %||% 20000
    return(list(
      ncells          = min(ncells, n_total_cells),
      max_per_cluster = max(300, round(ncells / 25)),
      npcs            = if (ncells < 15000) 15 else if (ncells < 60000) 20 else 30
    ))
  }

  params <- presets[[preset]] %||% presets[["standard"]]
  params$ncells <- min(params$ncells, n_total_cells)
  params
}

#' FindClusters() with automatic fallback to Louvain (algorithm=1)
#'
#' Step-3.8B: algorithms 2 (Louvain multilevel) / 3 (SLM) have been observed
#' to crash on BPCells-backed objects; algorithm 4 (Leiden) additionally
#' requires a working reticulate/leidenalg Python environment that may not be
#' installed. Rather than aborting the whole auto-pipeline run, fall back to
#' standard Louvain with a logged warning -- same pattern already used in the
#' standalone "1. Pipeline" module (mod_sc_pipeline.R), factored out here so
#' the auto-pipeline (mod_sc.R) can share it instead of duplicating it.
#'
#' @param obj Seurat object, post FindNeighbors().
#' @param resolution Clustering resolution.
#' @param algo Algorithm code as passed by the UI selectInput ("1"-"4",
#'   character or numeric) -- coerced to integer, defaults to 1 if invalid.
#' @param log_fn Optional function(character) called with a message if a
#'   fallback to Louvain occurred (caller decides where it goes: log panel,
#'   showNotification, ...).
#' @return Seurat object with $seurat_clusters set.
robust_find_clusters <- function(obj, resolution, algo = 1L, log_fn = NULL) {
  algo <- suppressWarnings(as.integer(algo %||% 1L))
  if (is.na(algo)) algo <- 1L
  tryCatch(
    Seurat::FindClusters(obj, resolution = resolution, algorithm = algo, verbose = FALSE),
    error = function(e) {
      if (algo != 1L) {
        algo_name <- c("2"="Louvain multilevel","3"="SLM","4"="Leiden")[as.character(algo)]
        if (!is.null(log_fn))
          log_fn(sprintf("%s indisponible/a \u00e9chou\u00e9 (%s) \u2014 repli sur Louvain standard.",
                         algo_name %||% "Algorithme", conditionMessage(e)))
        Seurat::FindClusters(obj, resolution = resolution, algorithm = 1L, verbose = FALSE)
      } else stop(e)
    }
  )
}

#' Standardize Seurat reduction names after a sketch-based ProjectData() call
#'
#' Seurat's sketch workflow (SketchData -> analyze on "sketch" assay ->
#' ProjectData) writes the full-size projected embeddings under whatever names
#' were passed to `full.reduction=`/`umap.model=`, not automatically "pca"/
#' "umap". The rest of TranscriptoShiny (Viz, Annotation, Trajectory...)
#' assumes reductions are named "pca"/"umap", so this copies them into place.
#' Fully defensive: never errors out, no-op if names already match or if the
#' expected reduction can't be identified (Seurat version differences).
#'
#' @param obj Seurat object just returned by ProjectData().
#' @param full_pca_name Name used as `full.reduction=` in ProjectData().
#' @return The Seurat object with "pca"/"umap" reductions pointing at the
#'   full-size projected embeddings (best effort).
standardize_sketch_reductions <- function(obj, full_pca_name = "pca.full") {
  tryCatch({
    reds <- names(obj@reductions)

    if (full_pca_name %in% reds && !"pca" %in% reds) {
      obj[["pca"]] <- obj[[full_pca_name]]
    }

    if (!"umap" %in% reds) {
      # ProjectData() typically names the projected UMAP "ref.umap"; fall back
      # to any "*umap*" reduction whose embedding row count matches ncol(obj).
      umap_candidates <- reds[grepl("umap", reds, ignore.case = TRUE)]
      full_n <- ncol(obj)
      for (cand in umap_candidates) {
        if (nrow(Embeddings(obj[[cand]])) == full_n) {
          obj[["umap"]] <- obj[[cand]]
          break
        }
      }
    }
  }, error = function(e) {
    warning(paste("standardize_sketch_reductions() non-bloquant :", conditionMessage(e)))
  })
  obj
}

#' Map Ensembl gene IDs to gene symbols in an expression matrix, collapsing duplicates
#'
#' Used for the SingleR cluster-aggregate ("pseudobulk") path: applied to the
#' small per-cluster profile matrix, never to the full cell x gene matrix.
#'
#' @param mat Numeric/dgCMatrix with genes (Ensembl IDs) as rownames.
#' @param organism "human" or "mouse".
#' @return Matrix with unique gene-symbol rownames (duplicates summed via
#'   base rowsum() on a densified pseudobulk matrix -- see Step-3.8B fix note
#'   inside the function body).
map_ensembl_matrix_to_symbol <- function(mat, organism = c("human", "mouse")) {
  organism  <- match.arg(organism)
  orgdb_pkg <- if (organism == "human") "org.Hs.eg.db" else "org.Mm.eg.db"

  if (!requireNamespace("AnnotationDbi", quietly = TRUE) ||
      !requireNamespace(orgdb_pkg, quietly = TRUE)) {
    stop(errorCondition(sprintf(
      "Package Bioconductor '%s' (+ AnnotationDbi) requis pour la conversion ENSEMBL -> Symbol.",
      orgdb_pkg), class = "sc_helpers_error"))
  }

  orgdb     <- getExportedValue(orgdb_pkg, orgdb_pkg)
  clean_ids <- trimws(sub("\\..*$", "", rownames(mat)))  # drop Ensembl version suffix

  symbols <- AnnotationDbi::mapIds(orgdb, keys = clean_ids, keytype = "ENSEMBL",
                                    column = "SYMBOL", multiVals = "first")

  keep <- !is.na(symbols) & nzchar(symbols)
  if (sum(keep) < 200L) {
    stop(errorCondition(sprintf(
      paste0("Mapping ENSEMBL -> Symbol insuffisant (%d/%d genes mappes, organisme '%s'). ",
             "Exemples d'identifiants testes : %s. V\u00e9rifiez l'organisme."),
      sum(keep), length(clean_ids), organism,
      paste(head(clean_ids, 5), collapse = ", ")), class = "sc_helpers_error"))
  }

  mat <- mat[keep, , drop = FALSE]
  rownames(mat) <- unname(symbols[keep])

  # Step-3.8B fix: `Matrix::rowsum()` does not exist (rowsum is a base
  # function -- Matrix does not export/override it) -- this line crashed
  # 100% of the time on real data ("'rowsum' n'est un objet exporte depuis
  # 'namespace:Matrix'"), silently aborting the whole ENSEMBL->Symbol
  # pseudobulk conversion (caught by the caller's tryCatch, so SingleR then
  # ran on unconverted ENSEMBL rownames against a Symbol-keyed reference ->
  # "no common genes"). This matrix is always tiny at this point (genes x
  # n_clusters, e.g. ~30-40k x <100) -- safe to densify and use base rowsum().
  mat_dense <- as.matrix(mat)
  rowsum(mat_dense, group = rownames(mat_dense), reorder = FALSE)
}

#' Build a hierarchical single-cell heatmap (ComplexHeatmap)
#'
#' Scaled expression of selected genes, at single-cell resolution or
#' AGGREGATED (mean per group, via Seurat::AverageExpression) when the
#' object exceeds `max_cells` -- never densifies a genes x cells matrix
#' beyond that cap. Mirrors helpers_bulk.R::plot_heatmap_bulk()'s explicit
#' draw()-inside-renderPlot() convention.
#'
#' @param seurat_obj Seurat object.
#' @param features Character vector of genes.
#' @param group_by Metadata column for column annotation / aggregation.
#' @param max_features Max genes kept (head(), no re-ranking).
#' @param max_cells Cells shown at single-cell resolution before falling
#'   back to per-group aggregation.
#' @param assay Assay name, or NULL for DefaultAssay().
#' @param palette "default"|"okabeito"|"viridis"|"set2"|"manual".
#' @param manual_colors Named vector (level -> hex), palette=="manual" only.
#' @return Invisibly, the drawn ComplexHeatmap object (call inside renderPlot()).
#' @export
build_sc_hierarchical_heatmap <- function(seurat_obj, features, group_by = "seurat_clusters",
                                          max_features = 50L, max_cells = 5000L, assay = NULL,
                                          palette = "default", manual_colors = NULL,
                                          manual_gradient = NULL,
                                          clustering_distance = "euclidean",
                                          clustering_method = "complete",
                                          k_row = NULL) {
  if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) {
    stop(errorCondition("Package 'ComplexHeatmap' requis pour la heatmap hierarchique.", class = "sc_helpers_error"))
  }
  if (is.null(seurat_obj)) stop(errorCondition("Aucun objet Single-Cell charge.", class = "sc_helpers_error"))
  assay <- assay %||% Seurat::DefaultAssay(seurat_obj)
  if (!group_by %in% colnames(seurat_obj@meta.data)) {
    stop(errorCondition(sprintf("Colonne de groupe '%s' introuvable dans les metadonnees.", group_by), class = "sc_helpers_error"))
  }

  valid_features <- intersect(features, rownames(seurat_obj))
  n_dropped <- length(features) - length(valid_features)
  if (length(valid_features) < 2L) stop(errorCondition("Au moins 2 genes valides sont requis pour la heatmap hierarchique.", class = "sc_helpers_error"))
  if (n_dropped > 0) warning(sprintf("%d gene(s) demande(s) introuvable(s) -- ignore(s).", n_dropped))
  if (length(valid_features) > max_features) {
    warning(sprintf("Selection reduite a %d genes (sur %d) -- max_features=%d.",
                    max_features, length(valid_features), max_features))
    valid_features <- head(valid_features, max_features)
  }

  set.seed(1L)  # reproducible subsampling/order across re-renders
  grp_vec <- as.character(seurat_obj@meta.data[[group_by]])
  n_cells <- ncol(seurat_obj)

  if (n_cells > max_cells) {
    warning(sprintf(
      "%d cellules > max_cells (%d) -- expression agregee (moyenne) par '%s' (%d groupes).",
      n_cells, max_cells, group_by, length(unique(grp_vec))
    ))
    Seurat::Idents(seurat_obj) <- factor(grp_vec)
    avg <- tryCatch(
      Seurat::AverageExpression(seurat_obj, features = valid_features, assays = assay,
                                layer = "data", verbose = FALSE)[[assay]],
      error = function(e) tryCatch(
        Seurat::AverageExpression(seurat_obj, features = valid_features, assays = assay,
                                  slot = "data", verbose = FALSE)[[assay]],
        error = function(e2) stop(errorCondition(paste0("Agregation par groupe impossible : ", conditionMessage(e2)), class = "sc_helpers_error"))
      )
    )
    mat <- as.matrix(avg)
    mat <- mat[valid_features[valid_features %in% rownames(mat)], , drop = FALSE]
    mat_scaled <- t(scale(t(mat)))
    mat_scaled[!is.finite(mat_scaled)] <- 0
    ann_levels <- colnames(mat_scaled)
    ann_colors <- sc_discrete_colors(ann_levels, palette, manual_colors)
    # PLOT-S4 : on prepare les arguments du coeur unifie (l'appel est unique,
    # apres les deux branches) au lieu de construire la HeatmapAnnotation ici.
    col_meta       <- list(Groupe = ann_levels)
    col_ann_colors <- if (!is.null(ann_colors)) {
      list(Groupe = stats::setNames(ann_colors, ann_levels))
    } else NULL
    subtitle <- sprintf("Moyenne agregee par groupe (%d cellules -> %d colonnes)", n_cells, ncol(mat_scaled))
  } else {
    ord <- order(grp_vec, colnames(seurat_obj))
    cell_ids <- colnames(seurat_obj)[ord]
    mat <- tryCatch(
      as.matrix(SeuratObject::LayerData(seurat_obj, assay = assay, layer = "data")[valid_features, cell_ids, drop = FALSE]),
      error = function(e) as.matrix(Seurat::GetAssayData(seurat_obj, assay = assay, slot = "data")[valid_features, cell_ids, drop = FALSE])
    )
    mat_scaled <- t(scale(t(mat)))
    mat_scaled[!is.finite(mat_scaled)] <- 0
    ann_vec <- grp_vec[ord]
    ann_levels <- sort(unique(ann_vec))
    ann_colors <- sc_discrete_colors(ann_levels, palette, manual_colors)
    col_meta       <- list(Groupe = ann_vec)
    col_ann_colors <- if (!is.null(ann_colors)) {
      list(Groupe = stats::setNames(ann_colors, ann_levels))
    } else NULL
    subtitle <- sprintf("%d cellules x %d genes (resolution cellule)", ncol(mat_scaled), nrow(mat_scaled))
  }

  # PLOT-S4 : delegue au coeur unifie (R/plotting/complex_heatmap.R).
  # - `name` reste "Z-score" EN DUR (non traduit) : comportement historique de
  #   ce site, different de Bulk qui le passe par tr().
  # - `manual_gradient` alimente la RAMPE (comme avant) ; les couleurs
  #   d'annotation viennent de sc_discrete_colors() — resolveur DISTINCT de
  #   bulk_annotation_colors() en mode "manual", donc non fusionnes.
  # - draw = TRUE : dessin explicite a marge sure, comme avant.
  return(ts_complex_heatmap(
    mat_scaled,
    name                  = "Z-score",
    column_title          = paste0("Heatmap Hierarchique -- ", subtitle),
    col_meta              = col_meta,
    col_annotation_colors = col_ann_colors,
    ramp                  = "diverging",
    ramp_domain           = range(mat_scaled, na.rm = TRUE),
    palette               = palette,
    manual_colors         = manual_gradient,
    show_column_names     = ncol(mat_scaled) <= 60,
    show_row_names        = nrow(mat_scaled) <= 60,
    clustering_distance   = clustering_distance,
    clustering_method     = clustering_method,
    k_row                 = k_row
  ))
}

#' Build a two-dimensional expression density plot (Nebulosa-like)
#'
#' NOT the Nebulosa package. Weighted 2D KDE (MASS::kde2d) of a gene's
#' expression over a 2D reduction -- purely descriptive visualization, not
#' a statement about lineage/abundance significance.
#'
#' @param seurat_obj Seurat object.
#' @param feature Gene symbol or metadata column.
#' @param reduction Reduction name (>= 2 dims).
#' @param bandwidth Optional length-2 numeric (kde2d `h`); NULL = MASS default.
#' @param max_cells Deterministic subsample cap.
#' @return A ggplot object.
#' @export
plot_sc_expression_density_2d <- function(seurat_obj, feature, reduction = "umap",
                                          bandwidth = NULL, max_cells = 50000L,
                                          palette = "default", manual_gradient = NULL) {
  if (!requireNamespace("MASS", quietly = TRUE)) stop(errorCondition("Package 'MASS' requis pour la densite 2D (MASS::kde2d).", class = "sc_helpers_error"))
  if (is.null(seurat_obj)) stop(errorCondition("Aucun objet Single-Cell charge.", class = "sc_helpers_error"))
  if (is.null(feature) || !nzchar(feature %||% "")) stop(errorCondition("Aucun gene/feature selectionne.", class = "sc_helpers_error"))
  if (!reduction %in% names(seurat_obj@reductions)) stop(errorCondition(sprintf("Reduction '%s' non calculee.", reduction), class = "sc_helpers_error"))
  emb <- Seurat::Embeddings(seurat_obj, reduction = reduction)
  if (ncol(emb) < 2L) stop(errorCondition(sprintf("La reduction '%s' a moins de 2 dimensions.", reduction), class = "sc_helpers_error"))

  fetch_var <- if (feature %in% rownames(seurat_obj)) feature
              else if (feature %in% colnames(seurat_obj@meta.data)) feature
              else stop(errorCondition(sprintf("Feature '%s' introuvable (ni gene ni colonne de metadonnees).", feature), class = "sc_helpers_error"))

  df <- data.frame(dim1 = emb[, 1], dim2 = emb[, 2],
                   value = as.numeric(Seurat::FetchData(seurat_obj, vars = fetch_var)[, 1]))
  df <- df[stats::complete.cases(df), , drop = FALSE]
  if (nrow(df) == 0L) stop(errorCondition("Aucune cellule avec une valeur exploitable pour ce feature.", class = "sc_helpers_error"))

  n_before <- nrow(df)
  if (n_before > max_cells) { set.seed(1L); df <- df[sample.int(n_before, max_cells), , drop = FALSE] }

  subtitle <- "Densite ponderee par l'expression -- visualisation descriptive uniquement (pas une inference biologique)"
  expr_pos <- df[df$value > 0, , drop = FALSE]

  p <- ggplot2::ggplot(df, ggplot2::aes(x = dim1, y = dim2)) +
    ggplot2::geom_point(color = "grey85", size = 0.4, alpha = 0.5)  # background: every sampled cell

  if (nrow(expr_pos) == 0L) {
    subtitle <- "Gene non detecte (expression nulle) dans les cellules affichees"
  } else if (nrow(expr_pos) < 5L) {
    subtitle <- "Trop peu de cellules expressives pour une densite fiable (<5) -- points bruts affiches"
    p <- p + ggplot2::geom_point(data = expr_pos, ggplot2::aes(color = value), size = 1.1) +
      expression_continuous_scale(palette, "color", manual_gradient, base_option = "plasma") +
      ggplot2::labs(color = "Expression")
  } else {
    kde_bw <- bandwidth %||% c(MASS::bandwidth.nrd(expr_pos$dim1), MASS::bandwidth.nrd(expr_pos$dim2))
    kde_bw[!is.finite(kde_bw) | kde_bw <= 0] <- 1e-3
    dens <- MASS::kde2d(expr_pos$dim1, expr_pos$dim2, h = kde_bw, n = 100,
                        lims = c(range(df$dim1), range(df$dim2)))
    dens_df <- data.frame(x = rep(dens$x, times = length(dens$y)),
                          y = rep(dens$y, each = length(dens$x)), z = as.vector(dens$z))
    p <- p +
      ggplot2::geom_raster(data = dens_df, ggplot2::aes(x = x, y = y, fill = z), interpolate = TRUE) +
      ggplot2::geom_point(data = expr_pos, ggplot2::aes(x = dim1, y = dim2), color = "black", size = 0.15, alpha = 0.25) +
      expression_continuous_scale(palette, "fill", manual_gradient, base_option = "plasma") +
      ggplot2::labs(fill = "Densite\n(ponderee expr.)")
  }

  p + ggplot2::coord_fixed() + ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(title = sprintf("Densite d'expression 2D -- %s (%s)", feature, toupper(reduction)),
                 subtitle = subtitle, x = paste0(toupper(reduction), "_1"), y = paste0(toupper(reduction), "_2")) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 13),
                  plot.subtitle = ggplot2::element_text(size = 9, color = "grey40"))
}

#' Build an interactive 3D reduction plot (plotly)
#'
#' Requires >= 3 embedding dimensions -- PCA usually qualifies; UMAP/t-SNE
#' are computed 2D by default in this app's pipelines and will raise a
#' clear French error instead of a cryptic index error.
#'
#' @param seurat_obj Seurat object.
#' @param reduction Reduction name.
#' @param color_by Metadata column.
#' @param max_cells Deterministic subsample cap.
#' @return A plotly widget.
#' @export
plot_sc_reduction_3d <- function(seurat_obj, reduction = "umap",
                                 color_by = "seurat_clusters", max_cells = 50000L,
                                 palette = "default", manual_gradient = NULL, manual_colors = NULL) {
  if (!requireNamespace("plotly", quietly = TRUE)) stop(errorCondition("Le package plotly est requis pour la visualisation 3D.", class = "sc_helpers_error"))
  if (is.null(seurat_obj)) stop(errorCondition("Aucun objet Single-Cell charge.", class = "sc_helpers_error"))
  if (!reduction %in% names(seurat_obj@reductions)) stop(errorCondition(sprintf("Reduction '%s' non calculee.", reduction), class = "sc_helpers_error"))
  emb <- Seurat::Embeddings(seurat_obj, reduction = reduction)
  if (ncol(emb) < 3L) {
    stop(errorCondition(sprintf(
      "La reduction '%s' n'a que %d dimension(s) -- 3 minimum requises. UMAP/t-SNE sont calcules en 2D par defaut ; essayez 'pca'.",
      reduction, ncol(emb)), class = "sc_helpers_error"))
  }
  if (!color_by %in% colnames(seurat_obj@meta.data)) stop(errorCondition(sprintf("Colonne '%s' introuvable.", color_by), class = "sc_helpers_error"))

  df <- data.frame(dim1 = emb[, 1], dim2 = emb[, 2], dim3 = emb[, 3],
                   id = colnames(seurat_obj), value = seurat_obj@meta.data[[color_by]])
  n_before <- nrow(df)
  subsampled <- n_before > max_cells
  if (subsampled) { set.seed(1L); df <- df[sample.int(n_before, max_cells), , drop = FALSE] }

  is_num <- is.numeric(df$value)
  p <- if (is_num) {
    colorscale_arg <- if (identical(palette, "manual") && !is.null(manual_gradient)) {
      list(c(0, manual_gradient$low %||% "#2166AC"), c(1, manual_gradient$high %||% "#B2182B"))
    } else "Viridis"
    plotly::plot_ly(df, x = ~dim1, y = ~dim2, z = ~dim3, type = "scatter3d", mode = "markers",
                    marker = list(size = 2.5, color = ~value, colorscale = colorscale_arg, showscale = TRUE,
                                 colorbar = list(title = color_by)),
                    text = ~paste0("ID: ", id, "<br>", color_by, ": ", round(value, 3)), hoverinfo = "text")
   } else {
     lv   <- sort(unique(stats::na.omit(as.character(df$value))))
     cols <- sc_discrete_colors(lv, palette = palette, manual_colors = manual_colors)
     plotly::plot_ly(df, x = ~dim1, y = ~dim2, z = ~dim3, type = "scatter3d", mode = "markers",
                     color = ~as.character(value), colors = cols, marker = list(size = 2.5),
                     text = ~paste0("ID: ", id, "<br>", color_by, ": ", value), hoverinfo = "text")
   }
   plotly::layout(p,
     title = sprintf("Reduction 3D -- %s%s", toupper(reduction),
                     if (subsampled) sprintf(" (echantillon %s/%s)", format(nrow(df), big.mark=" "), format(n_before, big.mark=" ")) else ""),
     scene = list(xaxis = list(title = paste0(toupper(reduction), "_1")),
                 yaxis = list(title = paste0(toupper(reduction), "_2")),
                 zaxis = list(title = paste0(toupper(reduction), "_3"))))
 }

# =============================================================================
# RNA Velocity — Phase 3A Hardening (strict validation, no inference)
# =============================================================================








# Internal: readLines with transparent gzip support (barcodes/features files).







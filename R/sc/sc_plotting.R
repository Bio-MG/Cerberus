# =============================================================================
# R/sc/sc_plotting.R — Single-Cell visualization builder
# =============================================================================
# Extracted from modules/sc/mod_sc_viz.R (Block 5 refactor).
# Pure plotting function: no Shiny reactivity, no module namespace.
# Called by mod_sc_viz_server() and sc_report_template.Rmd.
#
# Depends on: R/palettes.R (sc_discrete_scale, expression_continuous_scale,
#             bulk_diverging_ramp), Seurat, ggplot2, tidyr.
# Sourced in app.R AFTER R/palettes.R, BEFORE modules/sc/*.R.
# =============================================================================
#' Build a SC visualization ggplot from a config snapshot.
#'
#' Single source of truth used by BOTH the live render functions (which then
#' wrap with ggplotly or display static) AND the HTML/PDF report (via
#' params$saved_viz_list). Palette is applied via sc_discrete_scale() /
#' sc_continuous_scale() — both NULL-safe (NULL = Seurat/ggplot defaults,
#' which is what "default" palette produces).
#'
#' For the Volcano type, two extra attributes are attached to the returned
#' ggplot so the caller can construct a richer native plotly without re-running
#' FindMarkers:
#'   attr(p, "volcano_markers")  — the markers data.frame
#'   attr(p, "volcano_title")    — the plot title string
#'
#' @param obj     Seurat object.
#' @param cfg     Named list of input values (see .current_cfg() in server).
#' @param sc_palette Character palette name.
#' @param manual_colors Named character vector (level→hex), manual palette only.
#' @return ggplot or patchwork object.
build_sc_viz_plot <- function(obj, cfg, sc_palette = "default", manual_colors = NULL,
                              manual_gradient = NULL, manual_volcano_cols = NULL) {
  type    <- cfg$type     %||% "dim"
  pt_size <- as.numeric(cfg$pt_size %||% 0.5)
  grp     <- cfg$group_by %||% "seurat_clusters"

  # PLOT-S1 — shared resolver (R/plotting/theme.R). The default base_size (11)
  # is ggplot2's own default, i.e. EXACTLY what the bare theme_*() calls this
  # switch used before, so the rendering is unchanged. SC does not expose
  # base_size yet -> defensive fallback to the declared default.
  theme_fn <- ts_theme(cfg$plot_theme %||% TS_THEME_DEFAULT,
                       cfg$base_size %||% TS_BASE_SIZE_DEFAULT)

  pal_disc <- sc_discrete_scale(sc_palette, manual_colors, "color")
  pal_fill <- sc_discrete_scale(sc_palette, manual_colors, "fill")
  pal_cont <- sc_continuous_scale(sc_palette, "color", manual_gradient)

  # NULL-safe scale adder: suppressWarnings silences ggplot2's "replacing
  # existing scale" message (expected when overriding Seurat's built-in scale).
  .add <- function(p, s) if (is.null(s)) p else suppressWarnings(p + s)

  # Step-3.8B: past ~50k points, plain ggplot geom_point (what DimPlot/
  # FeaturePlot use by default) makes plotly conversion crawl or hang the
  # browser -- observed on the 1.3M-neurons dataset ("previews non
  # interactives / trop lentes" even though export PNG/PDF worked fine).
  # Seurat's raster=TRUE routes through scattermore (already a project
  # dependency, see global.R) -- point count no longer drives render cost.
  .raster_large <- ncol(obj) > 50000L

  # 1. DimPlot ----------------------------------------------------------------
  if (type == "dim") {
    red <- cfg$reduction %||% "umap"
    if (!red %in% names(obj@reductions)) stop(errorCondition(paste0("Réduction non calculée : ", red), class = "sc_plotting_error"))
    p <- DimPlot(obj, reduction = red, group.by = grp,
                 label = TRUE, pt.size = pt_size, raster = .raster_large) +
         theme_fn + ggtitle(paste(toupper(red), "\u2014", grp))
    return(.add(p, pal_disc))
  }

  # 2. FeaturePlot ------------------------------------------------------------
  if (type == "feature") {
    valid <- intersect(cfg$feat_sel %||% character(0), rownames(obj))
    if (!length(valid)) stop(errorCondition("Aucun gène valide", class = "sc_plotting_error"))
    # multi-gene returns patchwork: scale only safe for single gene
    if (length(valid) == 1L) {
      p <- FeaturePlot(obj, features = valid[1], pt.size = pt_size, raster = .raster_large) + theme_fn
      return(.add(p, pal_cont))
    }
    return(FeaturePlot(obj, features = head(valid, 4), ncol = 2,
                       pt.size = pt_size, raster = .raster_large) + theme_fn)
  }

  # 3. Scatter (enhanced) -----------------------------------------------------
  if (type == "scatter") {
    p <- plot_enhanced_scatter(
           obj, cfg$scatter_gene1, cfg$scatter_gene2, group.by = grp,
           method     = cfg$scatter_cor_method %||% "pearson",
           add_smooth = isTRUE(cfg$scatter_smooth),
           pt.size    = pt_size) + theme_fn
    return(.add(p, pal_disc))
  }

  # 4. Violin -----------------------------------------------------------------
  if (type == "violin") {
    valid <- intersect(cfg$feat_sel %||% character(0), rownames(obj))
    if (!length(valid)) stop(errorCondition("Aucun gène valide", class = "sc_plotting_error"))
    if (length(valid) == 1L) {
      p <- plot_violin_enhanced(obj, valid[1], grp,
                                add_boxplot = isTRUE(cfg$violin_boxplot)) + theme_fn
      return(.add(p, pal_fill))
    }
    plots <- lapply(head(valid, 4), function(g)
      .add(VlnPlot(obj, features = g, group.by = grp, pt.size = 0) +
           theme_fn + ggtitle(g), pal_fill))
    return(wrap_plots(plots, ncol = 2))
  }

  # 5. Stacked Violin ---------------------------------------------------------
  if (type == "stacked_violin") {
    valid <- intersect(cfg$feat_sel %||% character(0), rownames(obj))
    if (!length(valid)) stop(errorCondition("Aucun gène valide", class = "sc_plotting_error"))
    plots <- lapply(head(valid, 8), function(g)
      .add(VlnPlot(obj, features = g, group.by = grp, pt.size = 0) +
           theme(legend.position = "none",
                 axis.title.x   = element_blank(),
                 axis.text.x    = element_blank()) + ggtitle(g), pal_fill))
    return(wrap_plots(plots, ncol = 1) + theme_fn)
  }

  # 6. Ridge ------------------------------------------------------------------
  if (type == "ridge") {
    valid <- intersect(cfg$feat_sel %||% character(0), rownames(obj))
    if (!length(valid)) stop(errorCondition("Aucun gène valide", class = "sc_plotting_error"))
    p <- RidgePlot(obj, features = head(valid, 6), ncol = 2) + theme_fn
    return(.add(p, pal_fill))
  }

  # 7. DotPlot ----------------------------------------------------------------
  if (type == "dot") {
    valid <- intersect(cfg$feat_sel %||% character(0), rownames(obj))
    if (!length(valid)) stop(errorCondition("Aucun gène valide", class = "sc_plotting_error"))
    p <- DotPlot(obj, features = head(valid, 20), group.by = grp) + theme_fn +
         theme(axis.text.x = element_text(angle = 45, hjust = 1))
    # DotPlot: color encodes avg expression (continuous), size encodes pct expressed.
    # Route through sc_continuous_scale() so i18n$t("Manuel") (sequential gradient) is
    # respected here too, same as FeaturePlot; size scale untouched.
    return(suppressWarnings(p + sc_continuous_scale(sc_palette, "color", manual_gradient)))
  }

  # 8. Heatmap ------------------------------------------------------------------
  # Step-3.7 FIX: DoHeatmap's group color bar is set via group.colors=, NOT via
  # a `+ scale_color_manual(...)` layer (unlike every other plot type here) —
  # so the selected palette (manual/Okabe-Ito/Set2/Viridis) used to be silently
  # ignored on the top annotation strip while the blue/red expression gradient
  # (unaffected, driven separately by DoHeatmap's internal fill scale) looked
  # correct. sc_discrete_colors() supplies the matching concrete color vector;
  # NULL (palette=="default") preserves the previous/original behaviour exactly.
  if (type == "heatmap") {
    valid <- intersect(cfg$feat_sel %||% character(0), rownames(obj))
    if (!length(valid)) stop(errorCondition("Aucun gène valide", class = "sc_plotting_error"))
    valid <- head(valid, 30)
    # Step-3.7A: smart_scale_data()/ScaleData() upstream now restricts scale.data
    # to VariableFeatures by default (RAM-safety) — marker genes picked here that
    # fall outside that set need scale.data extended on demand, without ever
    # re-scaling the whole assay.
    obj <- tryCatch(ensure_genes_scaled(obj, valid), error = function(e) obj)
    lvls     <- sort(unique(na.omit(as.character(obj@meta.data[[grp]]))))
    grp_cols <- tryCatch(sc_discrete_colors(lvls, sc_palette, manual_colors),
                         error = function(e) NULL)
    p <- if (!is.null(grp_cols))
      DoHeatmap(obj, features = valid, group.by = grp, group.colors = grp_cols)
    else
      DoHeatmap(obj, features = valid, group.by = grp)
    div_scale <- sc_diverging_scale(sc_palette, "fill", manual_gradient)
    if (!is.null(div_scale)) p <- suppressWarnings(p + div_scale)
    return(p + theme_fn)
  }

  # 9. Correlation Matrix -----------------------------------------------------
  if (type == "correlation_matrix") {
    valid <- intersect(cfg$feat_sel %||% character(0), rownames(obj))
    if (length(valid) < 2) stop(errorCondition("Au moins 2 gènes requis", class = "sc_plotting_error"))
    rc <- diverging_ramp_colors(sc_palette, manual_gradient)
    return(plot_correlation_matrix(obj, head(valid, 30), method = "pearson",
                                   low_color = rc$low, mid_color = rc$mid, high_color = rc$high) + theme_fn)
  }

  # 10. Multi-Sample ----------------------------------------------------------
  if (type == "multi_sample") {
    if (length(unique(obj$orig.ident)) < 2) stop(errorCondition("Au moins 2 échantillons requis", class = "sc_plotting_error"))
    p <- plot_multi_sample(obj, cfg$multi_gene %||% rownames(obj)[1],
                           cfg$multi_plot_type %||% "violin") + theme_fn
    return(.add(p, pal_fill))
  }

  # 11. Volcano (ggplot — also carries markers as attr for native plotly) ------
  if (type == "volcano") {
    grp_col <- grp
    if (!grp_col %in% colnames(obj@meta.data)) stop(errorCondition("Colonne de groupe introuvable", class = "sc_plotting_error"))
    Idents(obj) <- factor(as.character(obj@meta.data[[grp_col]]))
    ident1 <- cfg$volcano_group1 %||% levels(Idents(obj))[1]
    ident2 <- if (is.null(cfg$volcano_group2) ||
                  cfg$volcano_group2 == "rest") NULL else cfg$volcano_group2
    if (!ident1 %in% levels(Idents(obj))) stop(errorCondition(paste0("Groupe 1 invalide: ", ident1), class = "sc_plotting_error"))

    markers <- tryCatch(
      FindMarkers(obj, ident.1 = ident1, ident.2 = ident2,
                  only.pos = FALSE, min.pct = 0.1, logfc.threshold = 0,
                  verbose = FALSE),
      error = function(e) stop(errorCondition(paste0("FindMarkers: ", e$message), class = "sc_plotting_error")))
    if (!nrow(markers)) stop(errorCondition("Aucun marqueur trouvé", class = "sc_plotting_error"))
    markers$gene <- rownames(markers)

    lfc  <- as.numeric(cfg$volcano_logfc %||% 0.25)
    pval <- as.numeric(cfg$volcano_pval  %||% 0.05)
    markers$status <- dplyr::case_when(
      markers$avg_log2FC >  lfc & markers$p_val_adj < pval ~ "Up",
      markers$avg_log2FC < -lfc & markers$p_val_adj < pval ~ "Down",
      TRUE ~ "NS")

    color_map <- bulk_role_colors(
      sc_palette,
      manual_colors = if (identical(sc_palette, "manual")) c(
        Up   = manual_volcano_cols[["up"]]   %||% "#E74C3C",
        Down = manual_volcano_cols[["down"]] %||% "#2980B9",
        NS   = manual_volcano_cols[["ns"]]   %||% "#BDC3C7"
      ) else NULL
    )
    show_lbl  <- isTRUE(cfg$volcano_show_labels %||% TRUE)
    top_genes <- c(head(markers$gene[markers$status == "Up"],   10),
                   head(markers$gene[markers$status == "Down"], 10))
    markers$label <- ifelse(show_lbl & markers$gene %in% top_genes, markers$gene, "")

    vtitle <- paste0("Volcano: ", ident1, " vs ", ident2 %||% "rest")
    p <- ggplot(markers, aes(x = avg_log2FC,
                              y = -log10(p_val_adj + 1e-300),
                              color = status)) +
         geom_point(alpha = 0.75, size = pt_size) +
         scale_color_manual(values = color_map) +
         geom_vline(xintercept = c(-lfc, lfc), linetype = "dotted", color = "grey40") +
         geom_hline(yintercept = -log10(pval),  linetype = "dotted", color = "grey40") +
         labs(title = vtitle, x = "Log2 Fold Change",
              y = "-log10(P.adj)", color = i18n$t("Statut")) + theme_fn
    if (show_lbl)
      # PLOT-Q1 — repel marker labels (same rationale as plot_volcano_bulk);
      # graceful fallback if ggrepel is unavailable.
      if (requireNamespace("ggrepel", quietly = TRUE)) {
        p <- p + ggrepel::geom_text_repel(aes(label = label), size = 2.8, na.rm = TRUE,
                                          max.overlaps = 20, box.padding = 0.4,
                                          segment.size = 0.3, show.legend = FALSE,
                                          min.segment.length = 0)
      } else {
        p <- p + geom_text(aes(label = label), size = 2.8, na.rm = TRUE,
                           vjust = -0.6, show.legend = FALSE)
      }

    # Attach markers + title as attributes so the caller can build native plotly
    # without re-running FindMarkers.
    attr(p, "volcano_markers") <- markers
    attr(p, "volcano_title")   <- vtitle
    return(p)
  }

  # 12. Heatmap Hierarchique -- returns a DRAWN ComplexHeatmap, NOT a ggplot.
  # Caller MUST route via the static render path, never ggsave()/ggplotly().
  if (type == "heatmap_hier") {
    valid <- intersect(cfg$feat_sel %||% character(0), rownames(obj))
    if (!length(valid)) stop(errorCondition("Aucun gène valide sélectionné pour la heatmap", class = "sc_plotting_error"))
    # PLOT-S4 — reglages de clustering. k < 2 (ou vide) = pas de decoupage,
    # ce qui reproduit exactement le rendu anterieur au jalon.
    k_row <- suppressWarnings(as.integer(cfg$hier_k_row %||% 0))
    if (length(k_row) != 1L || is.na(k_row) || k_row < 2L) k_row <- NULL
    return(build_sc_hierarchical_heatmap(
      obj, features = valid, group_by = grp, max_features = 50L,
      max_cells = as.integer(cfg$hier_max_cells %||% 5000L),
      palette = sc_palette, manual_colors = manual_colors, manual_gradient = manual_gradient,
      clustering_distance = cfg$hier_clust_distance %||% "euclidean",
      clustering_method   = cfg$hier_clust_method %||% "complete",
      k_row               = k_row
    ))
  }

  # 13. Densite d'Expression 2D (Nebulosa-like) --------------------------------
  if (type == "density_2d") {
    gene <- cfg$density_gene
    if (is.null(gene) || !nzchar(gene %||% "")) stop(errorCondition("Aucun gène sélectionné pour la densité", class = "sc_plotting_error"))
    red <- cfg$density_reduction %||% "umap"
    if (!red %in% names(obj@reductions)) stop(errorCondition(paste0("Réduction non calculée : ", red), class = "sc_plotting_error"))
    return(plot_sc_expression_density_2d(obj, feature = gene, reduction = red,
                                         max_cells = as.integer(cfg$density_max_cells %||% 50000L),
                                         palette = sc_palette, manual_gradient = manual_gradient) + theme_fn)
  }

  stop(errorCondition(paste0("Type de visualisation non supporté: ", type), class = "sc_plotting_error"))
}


# =============================================================================
# R/utils_spatial_export.R — Spatial "paquet complet" (zip) + script R reproductible
# =============================================================================
# v2 (vague 5 — Phase 6 stats + "exporter tout ce qui est en memoire") :
#    build_spatial_export_bundle() gagne 3 nouvelles sections, memes
#    conventions que les sections existantes (CSV + ligne README, controlee
#    par `sections`) :
#      - "enrichment" : results$enrichment_result$enrichment (data.frame
#        from/to/observed/z_score, B1, mod_spatial_niche.R).
#      - "hotspots"   : results$hotspot_result (data.frame id/value/gi_star/
#        p_value/hotspot, B4, mod_spatial_qc.R).
#      - "ripley"     : results$ripley_result$curve (data.frame r/k_observed/
#        k_perm_mean/k_perm_lo/k_perm_hi/signif, B6, mod_spatial_niche.R).
#    Toutes trois NULL-safe (section simplement omise si le calcul
#    correspondant n'a jamais ete lance) -- meme philosophie "LE MAXIMUM
#    D'EXPORT, meme si non affiche, juste calcule" que le reste de ce
#    fichier. mod_spatial_export.R expose maintenant aussi un lien "Tout
#    selectionner" sur la liste de sections, pour repondre directement au
#    besoin "un moyen d'exporter tous les fichiers/plots deja calcules en
#    memoire" sans devoir cocher chaque section une par une.
#
# Companion to modules/spatial/mod_spatial_export.R (moyen terme, voir
# handoff_spatial_bio-mg.md, points a/b). Pure functions only (no Shiny
# reactivity) so they stay testable/callable outside the app -- same
# convention as R/utils_spatial_multi.R / R/utils_spatial_niche.R. Unlike
# those two, these are NEVER called from inside a mirai daemon (export only
# reads results ALREADY computed and sitting in shared_rv -- no new heavy
# computation happens here), so they do NOT need to be added to
# R/utils_spatial_async.R's daemon preload list.
#
# Mirrors the existing Bulk report/script pattern
# (modules/bulk/mod_bulk_report.R's .bulk_r_script_text()): a reproducible
# script is generated as plain R text (paste0()/sprintf()), bundled in a
# .zip alongside whatever small companion data it needs to run standalone
# (zip::zip(mode="cherry-pick"), already a project dependency). The one
# structural difference from Bulk: Spatial's primary data (BPCells counts)
# lives on DISK, not an in-memory matrix small enough to bundle (see app.R's
# own session-save warning about this) -- so the generated script
# references `bpcells_dir` as a PATH the user must keep available
# (documented explicitly in the script header), and only the small `coords`
# data.frame travels inside the bundle (context.rds), exactly like every
# mirai task in this app already does (never the live Seurat/BPCells object).
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Minimal, histology-free static spatial map (PNG-safe, no Shiny/Plotly)
#'
#' Deliberately simpler than mod_spatial_viz.R's build_raster_plot(): no
#' histology overlay (that logic is tightly coupled to the viz module's own
#' reactive helpers) -- point-only maps are enough for an export bundle's
#' "quick look" plots; the interactive/histology-backed view remains the
#' app itself (onglet "4. Visualisation").
#'
#' @param df data.frame(x, y, value).
#' @param title Character, plot title.
#' @param palette "default"|"okabeito"|"viridis"|"set2"|"manual" (Tier 5 :
#'   suit la palette globale de l'onglet Visualisation quand elle est passee
#'   par l'appelant ; "default" preserve le rendu historique a l'identique).
#' @param manual_gradient Optional list(low=, high=), palette=="manual".
#' @param manual_discrete Optional named vector (level -> hex), palette=="manual".
#' @return A ggplot object.
render_spatial_static_map <- function(df, title = NULL, palette = "default",
                                      manual_gradient = NULL, manual_discrete = NULL,
                                      histology = NULL) {
  df <- df[stats::complete.cases(df[, c("x", "y")]), , drop = FALSE]
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = -y, color = value))
  # Optional tissue background (report/export parity with the app views).
  .hist_bounds <- NULL
  .ras <- NULL
  if (!is.null(histology) && !is.null(histology$bounds) &&
      !is.null(histology$rgba)) {
    .rgba <- histology$rgba
    .d3 <- dim(.rgba)
    .npix <- if (length(.d3) >= 2L) as.double(.d3[1L]) * as.double(.d3[2L]) else NA_real_
    if (length(.d3) == 3L && !is.na(.npix) && .npix > 0 && .npix <= 25000000) {
      .ras <- tryCatch({
        .alpha <- if (.d3[3L] >= 4L) as.vector(pmin(1, pmax(0, .rgba[, , 4L])))
                  else rep(1, .d3[1L] * .d3[2L])
        matrix(grDevices::as.raster(grDevices::rgb(
          red   = as.vector(pmin(1, pmax(0, .rgba[, , 1L]))),
          green = as.vector(pmin(1, pmax(0, .rgba[, , 2L]))),
          blue  = as.vector(pmin(1, pmax(0, .rgba[, , 3L]))),
          alpha = .alpha
        )), nrow = .d3[1L], ncol = .d3[2L])
      }, error = function(e) NULL)
      if (!is.null(.ras)) .hist_bounds <- histology$bounds
    }
  }
  if (!is.null(.ras)) {
    p <- p + ggplot2::annotation_raster(
      raster = .ras,
      xmin = .hist_bounds$x[1L], xmax = .hist_bounds$x[2L],
      ymin = -.hist_bounds$y[2L], ymax = -.hist_bounds$y[1L],
      interpolate = TRUE
    )
  }
  p <- if (requireNamespace("scattermore", quietly = TRUE)) {
    p + scattermore::geom_scattermore(pointsize = 3, alpha = 0.85)
  } else {
    p + ggplot2::geom_point(size = 0.6, alpha = 0.85)
  }
  .xlim <- if (!is.null(.hist_bounds)) range(c(range(df$x), .hist_bounds$x)) else NULL
  .ylim <- if (!is.null(.hist_bounds)) range(c(range(-df$y), -.hist_bounds$y)) else NULL
  p <- p + ggplot2::coord_fixed(xlim = .xlim, ylim = .ylim) + ts_theme("void", 12) +
    ggplot2::labs(title = title, color = NULL)
  if (is.numeric(df$value)) {
    p + sc_continuous_scale(palette = palette, aesthetic = "color", gradient = manual_gradient, na.value = "#CCCCCC")
  } else {
    lv  <- sort(unique(stats::na.omit(as.character(df$value))))
    pal <- sc_discrete_colors(lv, palette = palette, manual_colors = manual_discrete)
    if (is.null(pal)) pal <- stats::setNames(grDevices::hcl.colors(max(length(lv), 1), palette = "Dark 3"), lv)
    p + ggplot2::scale_color_manual(values = pal, na.value = "#CCCCCC")
  }
}

#' Format a parameter list as a short human-readable string (internal)
#' @param params A named list, or NULL.
#' @return Character scalar.
.spatial_params_to_text <- function(params) {
  if (is.null(params) || length(params) == 0) return("parametres non enregistres")
  paste(sprintf("%s=%s", names(params),
                vapply(params, function(x) paste(x %||% "-", collapse = "/"), character(1))),
        collapse = ", ")
}

#' Build a complete export bundle (CSVs + PNG maps + README) for one dataset
#'
#' @param spatial_obj The active `global_data$spatial_obj` list (coords,
#'   sketch, project, technology, n_total, bpcells_dir).
#' @param results A plain list SNAPSHOT of the relevant shared_rv fields --
#'   qc_metrics, qc_pass_idx, qc_params, cluster_labels, cluster_params,
#'   cluster_markers, deconv_props, deconv_params, moran_results,
#'   moran_params, niche_labels, niche_composition, niche_params,
#'   enrichment_result, enrichment_params, hotspot_result, hotspot_params,
#'   ripley_result, ripley_params. Passed as a plain list (NOT the
#'   reactiveValues object itself) so this function has zero Shiny
#'   dependency and stays unit-testable.
#' @param sections Character vector, subset of c("qc","cluster","deconv",
#'   "niche","moran","maps","custom_viz","enrichment","hotspots","ripley")
#'   to include.
#' @param out_dir Character path to an existing, EMPTY directory to write
#'   files into.
#' @return Character vector of file paths written (for the caller to zip).
build_spatial_export_bundle <- function(spatial_obj, results, sections, out_dir, multi_integration = NULL) {
  written <- character(0)
  add <- function(path) written <<- c(written, path)
  plots_dir <- file.path(out_dir, "graphiques_png")
  .ensure_plots_dir <- function() { if (!dir.exists(plots_dir)) dir.create(plots_dir, showWarnings = FALSE); plots_dir }
  .save_plot <- function(p, name) {
    if (is.null(p)) return(invisible(NULL))
    dir <- .ensure_plots_dir()
    path <- file.path(dir, sprintf("%s.png", name))
    ok <- tryCatch({ ts_export_plot(path, p, width = 8, height = 6, dpi = 300, bg = "white"); TRUE }, error = function(e) FALSE)
    if (isTRUE(ok) && file.exists(path)) add(path)
  }
  
  coords <- spatial_obj$coords
  sk_ids <- if (!is.null(spatial_obj$sketch)) colnames(spatial_obj$sketch) else NULL
  
  readme_lines <- c(
    "Cerberus -- Export Spatial (paquet complet)",
    sprintf("Echantillon     : %s", spatial_obj$project %||% "?"),
    sprintf("Technologie     : %s", spatial_obj$technology %||% "?"),
    sprintf("Genere le       : %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    sprintf("Elements (total): %s", format(spatial_obj$n_total %||% NA, big.mark = ",")),
    "", "Contenu :"
  )
  
  if ("qc" %in% sections && !is.null(results$qc_metrics)) {
    p <- file.path(out_dir, "qc_metrics.csv"); utils::write.csv(results$qc_metrics, p, row.names = FALSE); add(p)
    readme_lines <- c(readme_lines, sprintf(
      "  - qc_metrics.csv : nCount/nFeature/%%MT/%%ribo par element (%d lignes).%s",
      nrow(results$qc_metrics),
      if (!is.null(results$qc_pass_idx)) sprintf(" %d/%d passent les seuils (%s).", length(results$qc_pass_idx),
                                                 nrow(results$qc_metrics), .spatial_params_to_text(results$qc_params)) else ""))
    df <- results$qc_metrics
    p1 <- ggplot2::ggplot(df, ggplot2::aes(x = nCount)) + ggplot2::geom_histogram(bins = 50, fill = "#2C3E50") + ts_theme("minimal") + ggplot2::labs(title = "nCount")
    p2 <- ggplot2::ggplot(df, ggplot2::aes(x = nFeature)) + ggplot2::geom_histogram(bins = 50, fill = "#18BC9C") + ts_theme("minimal") + ggplot2::labs(title = "nFeature")
    p3 <- ggplot2::ggplot(df, ggplot2::aes(x = pct_mt)) + ggplot2::geom_histogram(bins = 50, fill = "#E74C3C") + ts_theme("minimal") + ggplot2::labs(title = "%MT")
    .save_plot(tryCatch(patchwork::wrap_plots(p1, p2, p3, ncol = 3), error = function(e) NULL), "qc_histogrammes")
    .save_plot(tryCatch(ggplot2::ggplot(df, ggplot2::aes(x = nCount, y = nFeature, color = pct_mt)) +
                          ggplot2::geom_point(alpha = 0.5, size = 1) + ggplot2::scale_color_viridis_c(option = "inferno", direction = -1) +
                          ts_theme("minimal") + ggplot2::labs(title = "nCount vs nFeature"), error = function(e) NULL), "qc_scatter")
  }
  
  if ("cluster" %in% sections && !is.null(results$cluster_labels)) {
    p <- file.path(out_dir, "cluster_labels.csv")
    utils::write.csv(data.frame(id = names(results$cluster_labels), cluster = unname(results$cluster_labels)), p, row.names = FALSE); add(p)
    readme_lines <- c(readme_lines, sprintf("  - cluster_labels.csv : %d elements, %d clusters (%s).",
                                            length(results$cluster_labels), length(unique(results$cluster_labels)), .spatial_params_to_text(results$cluster_params)))
    if (!is.null(results$cluster_markers) && nrow(results$cluster_markers) > 0) {
      p2 <- file.path(out_dir, "cluster_markers.csv"); utils::write.csv(results$cluster_markers, p2, row.names = FALSE); add(p2)
      readme_lines <- c(readme_lines, sprintf("  - cluster_markers.csv : %d lignes.", nrow(results$cluster_markers)))
    }
  }
  
  if ("deconv" %in% sections && !is.null(results$deconv_props)) {
    p <- file.path(out_dir, "deconv_proportions.csv"); utils::write.csv(results$deconv_props, p, row.names = FALSE); add(p)
    readme_lines <- c(readme_lines, sprintf("  - deconv_proportions.csv : %d elements x %d types (%s).",
                                            nrow(results$deconv_props), ncol(results$deconv_props) - 1, .spatial_params_to_text(results$deconv_params)))
    long <- reshape2::melt(results$deconv_props, id.vars = "id", variable.name = "cell_type", value.name = "proportion")
    ids_show <- utils::head(unique(long$id), 60)
    .save_plot(tryCatch(ggplot2::ggplot(long[long$id %in% ids_show, ], ggplot2::aes(x = id, y = proportion, fill = cell_type)) +
                          ggplot2::geom_col() + ts_theme("minimal") + ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
                          ggplot2::labs(title = "Proportions par type (60 premiers)"), error = function(e) NULL), "deconv_proportions")
    cts <- setdiff(colnames(results$deconv_props), "id")
    if (length(cts) >= 2) {
      cor_mat <- stats::cor(results$deconv_props[, cts, drop = FALSE], use = "pairwise.complete.obs")
      long_c <- reshape2::melt(cor_mat, varnames = c("Type1", "Type2"), value.name = "correlation")
      .save_plot(tryCatch(ggplot2::ggplot(long_c, ggplot2::aes(x = Type1, y = Type2, fill = correlation)) + ggplot2::geom_tile() +
                            ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0, limits = c(-1, 1)) +
                            ts_theme("minimal") + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                            ggplot2::labs(title = "Colocalisation"), error = function(e) NULL), "deconv_colocalisation")
    }
  }
  
  if ("moran" %in% sections && !is.null(results$moran_results)) {
    p <- file.path(out_dir, "moran_svg.csv"); utils::write.csv(results$moran_results, p, row.names = FALSE); add(p)
    readme_lines <- c(readme_lines, sprintf("  - moran_svg.csv : %d genes (%s).",
                                            nrow(results$moran_results), .spatial_params_to_text(results$moran_params)))
  }
  
  if ("niche" %in% sections && !is.null(results$niche_labels)) {
    p <- file.path(out_dir, "niche_labels.csv")
    utils::write.csv(data.frame(id = names(results$niche_labels), niche = unname(results$niche_labels)), p, row.names = FALSE); add(p)
    if (!is.null(results$niche_composition)) {
      p2 <- file.path(out_dir, "niche_composition.csv"); utils::write.csv(results$niche_composition, p2, row.names = FALSE); add(p2)
      long <- reshape2::melt(results$niche_composition, id.vars = "niche", variable.name = "groupe", value.name = "proportion")
      .save_plot(tryCatch(ggplot2::ggplot(long, ggplot2::aes(x = groupe, y = niche, fill = proportion)) + ggplot2::geom_tile() +
                            ggplot2::scale_fill_viridis_c(option = "magma", limits = c(0, 1)) + ts_theme("minimal") +
                            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) + ggplot2::labs(title = "Composition par niche"),
                          error = function(e) NULL), "niche_composition")
    }
    readme_lines <- c(readme_lines, sprintf("  - niche_labels.csv / niche_composition.csv : %d niches (%s).",
                                            length(unique(results$niche_labels)), .spatial_params_to_text(results$niche_params)))
  }
  
  if ("enrichment" %in% sections && !is.null(results$enrichment_result)) {
    p <- file.path(out_dir, "enrichment_zscore.csv"); utils::write.csv(results$enrichment_result$enrichment, p, row.names = FALSE); add(p)
    df <- results$enrichment_result$enrichment
    .save_plot(tryCatch(ggplot2::ggplot(df, ggplot2::aes(x = to, y = from, fill = z_score)) + ggplot2::geom_tile() +
                          ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", z_score)), size = 3) +
                          ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0) +
                          ts_theme("minimal") + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                          ggplot2::labs(title = "Enrichissement de voisinage"), error = function(e) NULL), "enrichment_heatmap")
    readme_lines <- c(readme_lines, sprintf("  - enrichment_zscore.csv : %d niveaux (%s).",
                                            length(results$enrichment_result$levels), .spatial_params_to_text(results$enrichment_params)))
  }
  
  if ("hotspots" %in% sections && !is.null(results$hotspot_result)) {
    p <- file.path(out_dir, "hotspots_gi.csv"); utils::write.csv(results$hotspot_result, p, row.names = FALSE); add(p)
    n_hot <- sum(results$hotspot_result$hotspot == "Hotspot (chaud)")
    n_cold <- sum(results$hotspot_result$hotspot == "Coldspot (froid)")
    .save_plot(tryCatch(ggplot2::ggplot(results$hotspot_result, ggplot2::aes(x = gi_star, fill = hotspot)) +
                          ggplot2::geom_histogram(bins = 50) +
                          ggplot2::scale_fill_manual(values = c("Hotspot (chaud)" = "#D55E00", "Coldspot (froid)" = "#0072B2", "NS" = "#CCCCCC")) +
                          ts_theme("minimal") + ggplot2::labs(title = "Distribution du Gi*"), error = function(e) NULL), "hotspots_distribution")
    readme_lines <- c(readme_lines, sprintf("  - hotspots_gi.csv : %d hotspots, %d coldspots sur %d (%s).",
                                            n_hot, n_cold, nrow(results$hotspot_result), .spatial_params_to_text(results$hotspot_params)))
  }
  
  if ("ripley" %in% sections && !is.null(results$ripley_result)) {
    p <- file.path(out_dir, "ripley_k_curve.csv"); utils::write.csv(results$ripley_result$curve, p, row.names = FALSE); add(p)
    df <- results$ripley_result$curve
    .save_plot(tryCatch(ggplot2::ggplot(df, ggplot2::aes(x = r)) +
                          ggplot2::geom_ribbon(ggplot2::aes(ymin = k_perm_lo, ymax = k_perm_hi), fill = "grey80", alpha = 0.6) +
                          ggplot2::geom_line(ggplot2::aes(y = k_observed), color = "#D55E00", linewidth = 1) + ts_theme("minimal") +
                          ggplot2::labs(title = sprintf("Ripley's K -- '%s'", results$ripley_result$target_level)), error = function(e) NULL), "ripley_k")
    readme_lines <- c(readme_lines, sprintf("  - ripley_k_curve.csv : cible '%s' (%s, %s).",
                                            results$ripley_result$target_level, if (isTRUE(results$ripley_result$subsampled)) "sous-echantillonne" else "complet",
                                            .spatial_params_to_text(results$ripley_params)))
  }
  
  if ("maps" %in% sections && !is.null(coords) && !is.null(sk_ids)) {
    maps_dir <- file.path(out_dir, "cartes_png"); dir.create(maps_dir, showWarnings = FALSE)
    base_df <- coords[match(sk_ids, coords$id), c("id", "x", "y")]
    map_specs <- list()
    if (!is.null(results$qc_metrics)) { m <- match(base_df$id, results$qc_metrics$id); map_specs[["nCount"]] <- results$qc_metrics$nCount[m] }
    if (!is.null(results$cluster_labels)) map_specs[["cluster"]] <- as.character(results$cluster_labels[base_df$id])
    if (!is.null(results$niche_labels))   map_specs[["niche"]]   <- as.character(results$niche_labels[base_df$id])
    if (!is.null(results$hotspot_result)) { m <- match(base_df$id, results$hotspot_result$id); map_specs[["hotspot"]] <- results$hotspot_result$hotspot[m] }
    for (nm in names(map_specs)) {
      df_map <- data.frame(x = base_df$x, y = base_df$y, value = map_specs[[nm]])
      if (all(is.na(df_map$value))) next
      png_path <- file.path(maps_dir, sprintf("carte_%s.png", nm))
      p_gg <- tryCatch(render_spatial_static_map(df_map, title = nm), error = function(e) NULL)
      if (!is.null(p_gg)) {
        tryCatch(ts_export_plot(png_path, p_gg, width = 7, height = 7, dpi = 300, bg = "white"), error = function(e) NULL)
        if (file.exists(png_path)) add(png_path)
      }
    }
    if (length(map_specs) > 0) readme_lines <- c(readme_lines, sprintf("  - cartes_png/ : %s.", paste(names(map_specs), collapse = ", ")))
  }
  
  if ("custom_viz" %in% sections && !is.null(coords) && length(results$saved_viz_list %||% list()) > 0) {
    maps_dir <- file.path(out_dir, "cartes_png"); dir.create(maps_dir, showWarnings = FALSE)
    n_saved <- 0L
    for (nm in names(results$saved_viz_list)) {
      cfg <- results$saved_viz_list[[nm]]
      df_v <- tryCatch(build_saved_viz_df(spatial_obj$sketch, coords, cfg, results), error = function(e) NULL)
      if (is.null(df_v)) next
      png_path <- file.path(maps_dir, sprintf("vue_%s.png", gsub("[^A-Za-z0-9_-]+", "_", nm)))
      p_gg <- tryCatch(render_spatial_static_map(df_v, title = nm), error = function(e) NULL)
      if (!is.null(p_gg)) {
        tryCatch(ts_export_plot(png_path, p_gg, width = 7, height = 7, dpi = 300, bg = "white"), error = function(e) NULL)
        if (file.exists(png_path)) { add(png_path); n_saved <- n_saved + 1L }
      }
    }
    if (n_saved > 0) readme_lines <- c(readme_lines, sprintf("  - cartes_png/vue_*.png : %d vue(s) sauvegardee(s).", n_saved))
  }
  
  if ("multi" %in% sections && !is.null(multi_integration)) {
    p <- file.path(out_dir, "multi_embeddings.csv"); utils::write.csv(multi_integration$embeddings, p, row.names = FALSE); add(p)
    .save_plot(tryCatch(ggplot2::ggplot(multi_integration$embeddings, ggplot2::aes(x = dim1, y = dim2, color = dataset)) +
                          ggplot2::geom_point(size = 0.7, alpha = 0.7) + ts_theme("minimal") + ggplot2::labs(title = "UMAP conjoint -- par echantillon"),
                        error = function(e) NULL), "multi_umap_by_dataset")
    .save_plot(tryCatch(ggplot2::ggplot(multi_integration$embeddings, ggplot2::aes(x = dim1, y = dim2, color = cluster)) +
                          ggplot2::geom_point(size = 0.7, alpha = 0.7) + ts_theme("minimal") + ggplot2::labs(title = "UMAP conjoint -- par cluster"),
                        error = function(e) NULL), "multi_umap_by_cluster")
    readme_lines <- c(readme_lines, sprintf("  - multi_embeddings.csv : %s, %d elements, echantillons : %s.",
                                            multi_integration$reduction_used %||% "?", nrow(multi_integration$embeddings),
                                            paste(multi_integration$datasets %||% character(0), collapse = ", ")))
    
    diffcomp <- tryCatch(compute_composition_differential(multi_integration$embeddings), error = function(e) NULL)
    if (!is.null(diffcomp)) {
      p3 <- file.path(out_dir, "multi_composition_differentielle.csv"); utils::write.csv(diffcomp$residuals, p3, row.names = FALSE); add(p3)
      .save_plot(tryCatch(ggplot2::ggplot(diffcomp$residuals, ggplot2::aes(x = cluster, y = dataset, fill = std_resid)) + ggplot2::geom_tile() +
                            ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", std_resid)), size = 3) +
                            ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0) + ts_theme("minimal") +
                            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) + ggplot2::labs(title = "Composition differentielle"),
                          error = function(e) NULL), "multi_composition_differentielle")
      readme_lines <- c(readme_lines, sprintf("  - multi_composition_differentielle.csv : Chi2 = %.1f, p = %.4g (%s).",
                                              diffcomp$chisq$statistic, diffcomp$chisq$p_value, diffcomp$chisq$method))
    }
  }
  
  readme_lines <- c(readme_lines, "", "Note : cet export reflete l'etat des onglets au moment du clic.")
  readme_path <- file.path(out_dir, "README.txt"); writeLines(readme_lines, readme_path); add(readme_path)
  written
}
#' Rebuild one saved custom view's (x, y, value) data.frame
#'
#' Companion to mod_spatial_viz.R's "Ajouter au rapport" button: that button
#' saves only a small CONFIG (color_by + the relevant sub-selection), never
#' an image -- this function re-derives the actual (x, y, value) points from
#' whatever results are CURRENTLY cached for the dataset, mirroring
#' mod_spatial_viz.R's own plot_df() logic (histology-free, matching every
#' other report/export plot -- see render_spatial_static_map()'s header).
#' Returns NULL (not an error) when the underlying data no longer exists
#' (e.g. the config references a gene, but the sketch doesn't have it, or
#' references deconvolution but it was never computed for this dataset) --
#' the caller shows a one-line "unavailable" note instead of failing the
#' whole report.
#'
#' @param sketch A Seurat object (this dataset's RAM sketch) or NULL.
#' @param coords data.frame(id, x, y, ...) or NULL.
#' @param cfg One entry of shared_rv$saved_viz_list -- list(color_by=,
#'   qc_metric=, gene=, deconv_celltype=, show_cluster_labels=).
#' @param results The dataset's results snapshot (same shape as elsewhere
#'   in this file) -- qc_metrics, cluster_labels, deconv_props, niche_labels.
#' @return data.frame(id, x, y, value) or NULL.
build_saved_viz_df <- function(sketch, coords, cfg, results = list()) {
  if (is.null(sketch) || is.null(coords) || is.null(cfg)) return(NULL)
  sk_ids <- tryCatch(colnames(sketch), error = function(e) character(0))
  if (length(sk_ids) == 0) return(NULL)
  df <- coords[match(sk_ids, coords$id), c("id", "x", "y")]

  value <- tryCatch(switch(cfg$color_by %||% "qc",
    "cluster" = {
      if (is.null(results$cluster_labels)) return(NULL)
      as.character(results$cluster_labels[df$id])
    },
    "niche" = {
      if (is.null(results$niche_labels)) return(NULL)
      as.character(results$niche_labels[df$id])
    },
    "deconv" = {
      if (is.null(results$deconv_props) || is.null(cfg$deconv_celltype) ||
          !cfg$deconv_celltype %in% colnames(results$deconv_props)) return(NULL)
      m <- match(df$id, results$deconv_props$id)
      results$deconv_props[[cfg$deconv_celltype]][m]
    },
    "qc" = {
      if (is.null(results$qc_metrics)) return(NULL)
      metric <- cfg$qc_metric %||% "nCount"
      if (!metric %in% colnames(results$qc_metrics)) return(NULL)
      m <- match(df$id, results$qc_metrics$id)
      results$qc_metrics[[metric]][m]
    },
    "gene" = {
      g <- cfg$gene
      if (is.null(g) || !nzchar(g) || !g %in% rownames(sketch)) return(NULL)
      sk <- sketch
      if (!"data" %in% SeuratObject::Layers(sk)) sk <- Seurat::NormalizeData(sk, verbose = FALSE)
      as.numeric(SeuratObject::LayerData(sk, layer = "data")[g, df$id])
    },
    NULL
  ), error = function(e) NULL)

  if (is.null(value)) return(NULL)
  df$value <- value
  df <- df[stats::complete.cases(df[, c("x", "y")]), ]
  if (nrow(df) == 0) return(NULL)
  df
}

#' Mirrors modules/bulk/mod_bulk_report.R's `.bulk_r_script_text()` pattern:
#' plain R code assembled via paste0()/sprintf(), meant to run OUTSIDE Shiny
#' via `Rscript` or `source()`. Only steps ALREADY run in the app (i.e. with
#' a non-NULL `*_params` entry) are included -- everything else is written
#' as a clearly-commented, skipped section rather than guessed at.
#'
#' @param spatial_obj Active spatial_obj list (bpcells_dir, project, technology, n_total).
#' @param params list(qc=, cluster=, deconv=, niche=) -- each either NULL
#'   (step omitted from the script) or the matching *_params list captured
#'   at click time (see shared_rv$qc_params / cluster_params / deconv_params
#'   / niche_params, mirrored by each tab's own observeEvent).
#' @return Character scalar, the full R script text.
generate_spatial_pipeline_script <- function(spatial_obj, params) {
  date    <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  qc      <- params$qc
  cluster <- params$cluster
  deconv  <- params$deconv
  niche   <- params$niche

  qc_block <- if (!is.null(qc)) sprintf('
# -- 1. QC ---------------------------------------------------------------------
qc_metrics <- compute_qc_metrics_fast(bpcells_dir)
pass <- with(qc_metrics, nCount >= %s & nFeature >= %s & (is.na(pct_mt) | pct_mt <= %s))
pass_idx <- which(pass)
cat(sprintf("QC : %%d/%%d elements conserves.\\n", length(pass_idx), nrow(qc_metrics)))
', qc$min_count %||% 100, qc$min_features %||% 200, qc$max_pct_mt %||% 20) else '
# -- 1. QC ---------------------------------------------------------------------
# Aucun seuil QC applique dans l\'app au moment de l\'export -- tous les
# elements sont conserves ici (pass_idx = NULL).
qc_metrics <- compute_qc_metrics_fast(bpcells_dir)
pass_idx <- NULL
'

  cluster_block <- if (!is.null(cluster)) sprintf('
# -- 2. Clustering spatial (BANKSY-lite) ----------------------------------------
# Reimplementation directe (pas de dependance Banksy/SeuratWrappers) -- voir
# modules/spatial/mod_spatial_cluster.R du projet pour la version commentee
# en detail.
run_banksy_lite <- function(bpcells_dir, pass_idx, coords, lambda, k_geom, npcs, resolution) {
  mat <- BPCells::open_matrix_dir(bpcells_dir)
  if (!is.null(pass_idx)) mat <- mat[, pass_idx, drop = FALSE]
  obj <- Seurat::CreateSeuratObject(counts = mat)
  obj <- Seurat::NormalizeData(obj, verbose = FALSE)
  obj <- Seurat::FindVariableFeatures(obj, verbose = FALSE)
  var_feat <- Seurat::VariableFeatures(obj)
  coords_df <- coords[match(colnames(obj), coords$id), c("x", "y")]; rownames(coords_df) <- colnames(obj)
  keep <- stats::complete.cases(coords_df); obj <- obj[, keep]
  coords_mat <- as.matrix(coords_df[keep, , drop = FALSE])
  nn <- RANN::nn2(coords_mat, k = min(k_geom + 1, nrow(coords_mat)))
  neighbor_idx <- nn$nn.idx[, -1, drop = FALSE]
  own_mat <- t(as.matrix(SeuratObject::LayerData(obj, layer = "data")[var_feat, , drop = FALSE]))
  n <- nrow(own_mat); kk <- ncol(neighbor_idx)
  W <- Matrix::sparseMatrix(i = rep(seq_len(n), each = kk), j = as.vector(t(neighbor_idx)), x = 1 / kk, dims = c(n, n))
  nbr_mat <- as.matrix(W %%*%% own_mat); dimnames(nbr_mat) <- dimnames(own_mat)
  own_s <- scale(own_mat); own_s[!is.finite(own_s)] <- 0
  nbr_s <- scale(nbr_mat); nbr_s[!is.finite(nbr_s)] <- 0
  augmented <- cbind(sqrt(1 - lambda) * own_s, sqrt(lambda) * nbr_s)
  n_pc <- max(2, min(npcs, ncol(augmented) - 1, nrow(augmented) - 1))
  pca <- if (requireNamespace("irlba", quietly = TRUE)) {
    irlba::prcomp_irlba(augmented, n = n_pc, center = FALSE, scale. = FALSE)
  } else stats::prcomp(augmented, rank. = n_pc, center = FALSE, scale. = FALSE)
  emb <- pca$x; rownames(emb) <- colnames(obj); colnames(emb) <- paste0("BANKSYPCA_", seq_len(ncol(emb)))
  obj[["BANKSY_PCA"]] <- Seurat::CreateDimReducObject(embeddings = emb, key = "BANKSYPCA_", assay = Seurat::DefaultAssay(obj))
  obj <- Seurat::FindNeighbors(obj, reduction = "BANKSY_PCA", dims = seq_len(n_pc), verbose = FALSE)
  obj <- tryCatch(Seurat::FindClusters(obj, resolution = resolution, algorithm = 4, verbose = FALSE),
                  error = function(e) Seurat::FindClusters(obj, resolution = resolution, algorithm = 1, verbose = FALSE))
  setNames(as.character(obj$seurat_clusters), colnames(obj))
}
cluster_labels <- run_banksy_lite(bpcells_dir, pass_idx, coords, lambda = %s, k_geom = %s, npcs = %s, resolution = %s)
cat(sprintf("Clustering : %%d clusters.\\n", length(unique(cluster_labels))))
', cluster$lambda %||% 0.8, cluster$k_geom %||% 18, cluster$npcs %||% 30, cluster$resolution %||% 0.8) else '
# -- 2. Clustering spatial -------------------------------------------------------
# Non calcule dans l\'app au moment de l\'export -- section omise.
cluster_labels <- NULL
'

  deconv_block <- if (!is.null(deconv) && identical(deconv$mode, "rctd") && !is.null(deconv$ref_path)) sprintf('
# -- 3. Deconvolution (RCTD) ------------------------------------------------------
# Necessite l\'artefact de reference prepare par l\'app (R/utils_spatial_reference.R
# ::prepare_reference_artifact()) -- le chemin ci-dessous n\'est valide que sur
# la machine/session d\'origine (%s) ; re-preparez la reference si besoin.
ref_manifest_path <- "%s"
if (file.exists(ref_manifest_path)) {
  manifest <- readRDS(ref_manifest_path)
  ref_counts <- if (identical(manifest$backend, "bpcells")) {
    methods::as(BPCells::open_matrix_dir(manifest$counts_path), "dgCMatrix")
  } else readRDS(manifest$counts_path)
  mat <- BPCells::open_matrix_dir(bpcells_dir)
  if (!is.null(pass_idx)) mat <- mat[, pass_idx, drop = FALSE]
  coords_df <- coords[match(colnames(mat), coords$id), c("x", "y")]; rownames(coords_df) <- colnames(mat)
  keep <- stats::complete.cases(coords_df); mat <- mat[, keep, drop = FALSE]; coords_df <- coords_df[keep, , drop = FALSE]
  reference <- spacexr::Reference(counts = ref_counts, cell_types = manifest$cell_types)
  puck <- spacexr::SpatialRNA(coords = coords_df, counts = methods::as(mat, "dgCMatrix"))
  rctd <- spacexr::create.RCTD(puck, reference, max_cores = 1)
  rctd <- spacexr::run.RCTD(rctd, doublet_mode = "full")
  w <- as.matrix(rctd@results$weights); w <- sweep(w, 1, rowSums(w), "/")
  deconv_props <- data.frame(id = rownames(w), w, row.names = NULL, check.names = FALSE)
  write.csv(deconv_props, "deconv_proportions.csv", row.names = FALSE)
  cat("Deconvolution (RCTD) terminee -- deconv_proportions.csv ecrit.\\n")
} else {
  message("Reference introuvable a : ", ref_manifest_path, " -- etape deconvolution ignoree.")
  deconv_props <- NULL
}
', deconv$ref_source_label %||% "reference locale", deconv$ref_path) else '
# -- 3. Deconvolution --------------------------------------------------------------
# Non calculee (ou methode != RCTD -- voir modules/spatial/mod_spatial_deconv.R
# pour Label Transfer / STdeconvolve) dans l\'app au moment de l\'export.
deconv_props <- NULL
'

  niche_block <- if (!is.null(niche) && !is.null(cluster)) sprintf('
# -- 4. Niches spatiales -------------------------------------------------------------
niche_res <- compute_spatial_niches(coords = coords, group_labels = cluster_labels,
                                     k_neighbors = %s, n_niches = %s)
write.csv(niche_res$assignments, "niche_labels.csv", row.names = FALSE)
write.csv(niche_res$niche_composition, "niche_composition.csv", row.names = FALSE)
cat(sprintf("Niches : %%d niches.\\n", length(unique(niche_res$assignments$niche))))
', niche$k_neighbors %||% 30, niche$n_niches %||% 5) else '
# -- 4. Niches spatiales ---------------------------------------------------------------
# Non calculees dans l\'app au moment de l\'export (necessite un clustering) --
# section omise.
niche_res <- NULL
'

  paste0(
'# =============================================================================
# Script R Reproductible -- Cerberus (module Spatial)
# Genere le : ', date, '
# Echantillon : ', spatial_obj$project %||% "?", ' (', spatial_obj$technology %||% "?", ')
# =============================================================================
#
# PREREQUIS :
#   - Memes packages R que l\'application (Seurat v5, BPCells, RANN, irlba,
#     Matrix, et spacexr si la section Deconvolution est presente).
#   - Le dossier BPCells sur DISQUE, au meme chemin que dans l\'app (adaptez
#     `bpcells_dir` ci-dessous sinon) -- ce script ne le reconstruit pas.
#   - Le fichier "context.rds" fourni A COTE de ce script dans l\'archive
#     (coordonnees spatiales completes).
#
# Sourcez R/utils_spatial_io.R et R/utils_spatial_niche.R du projet AVANT ce
# script pour disposer de compute_qc_metrics_fast() / compute_spatial_niches().
# Sourcez aussi R/utils_spatial_stats.R si vous voulez reproduire manuellement
# l\'enrichissement de voisinage / les hotspots / Ripley\'s K (non inclus dans
# ce script auto-genere -- voir enrichment_zscore.csv / hotspots_gi.csv /
# ripley_k_curve.csv dans le paquet .zip pour les resultats DEJA calcules).

suppressPackageStartupMessages({
  library(Seurat); library(BPCells); library(Matrix); library(RANN)
})

# -- 0. Contexte -------------------------------------------------------------------
bpcells_dir <- "', spatial_obj$bpcells_dir %||% "<CHEMIN_BPCELLS_A_COMPLETER>", '"   # ADAPTEZ SI DEPLACE
context <- readRDS("context.rds")
coords  <- context$coords
', qc_block, '
', cluster_block, '
', deconv_block, '
', niche_block, '
# -- 5. Sauvegarde -------------------------------------------------------------------
saveRDS(list(qc_metrics = qc_metrics, pass_idx = pass_idx, cluster_labels = cluster_labels,
             deconv_props = deconv_props, niche = niche_res),
        file = "resultats_pipeline_spatial.rds")
cat("Termine -- resultats dans \'resultats_pipeline_spatial.rds\'.\\n")
'
  )
}

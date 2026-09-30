# R/palettes.R
# Shared palette helpers for TranscriptoShiny — single source of truth for
# discrete/continuous/diverging color palettes, used by SC (mod_sc_viz.R,
# mod_sc_annotation.R, mod_sc_trajectory.R, mod_sc_pathways.R, helpers_sc.R),
# Bulk (helpers_bulk.R, mod_bulk_pathways.R) and Spatial (mod_spatial_*).
# Sourced LAST in app.R -> always the canonical copy.

`%||%` <- function(a, b) if (is.null(a)) b else a

# Okabe-Ito colorblind-safe palette
.sc_okabe <- c("#E69F00","#56B4E9","#009E73","#F0E442",
               "#0072B2","#D55E00","#CC79A7","#999999")

.default_manual_colors <- function(n) {
  base <- c("#E69F00","#56B4E9","#009E73","#F0E442",
            "#0072B2","#D55E00","#CC79A7","#999999")
  rep_len(base, n)
}

manual_color_picker_ui <- function(ns, ids, labels, defaults) {
  tagList(
    div(
      style = "display:flex;flex-wrap:wrap;gap:14px;align-items:center;padding:8px 0;",
      lapply(seq_along(ids), function(i) {
        full_id <- ns(ids[i])
        div(
          style = "display:flex;align-items:center;gap:6px;",
          tags$input(
            type = "color", id = full_id, value = defaults[i],
            style = "width:34px;height:28px;border:1px solid #ccc;border-radius:4px;padding:0;cursor:pointer;",
            onchange = sprintf("Shiny.setInputValue('%s', this.value)", full_id)
          ),
          tags$span(labels[i], style = "font-size:0.85em;")
        )
      })
    )
  )
}

# ---------------------------------------------------------------------------
# Raw hex resolvers (single source of truth for BOTH ggplot scales AND
# ComplexHeatmap/leaflet ramps, which cannot accept a ggplot scale object).
# ---------------------------------------------------------------------------

#' Diverging low/mid/high hex triplet for a given palette preset
diverging_ramp_colors <- function(palette = "default", manual_colors = NULL) {
  if (identical(palette, "manual") && !is.null(manual_colors)) {
    return(list(low  = manual_colors$low  %||% "#2166AC",
                mid  = manual_colors$mid  %||% "white",
                high = manual_colors$high %||% "#B2182B"))
  }
  switch(palette %||% "default",
    okabeito = list(low = "#0072B2", mid = "white", high = "#D55E00"),
    viridis  = if (requireNamespace("viridisLite", quietly = TRUE)) {
      v <- viridisLite::viridis(3); list(low = v[1], mid = "white", high = v[3])
    } else list(low = "#2166AC", mid = "white", high = "#B2182B"),
    set2     = if (requireNamespace("RColorBrewer", quietly = TRUE)) {
      s <- RColorBrewer::brewer.pal(3, "Set2"); list(low = s[2], mid = "white", high = s[1])
    } else list(low = "#2166AC", mid = "white", high = "#B2182B"),
    list(low = "#2166AC", mid = "white", high = "#B2182B")   # default / manual-no-override / unknown
  )
}

#' Sequential low/high hex pair (correlation-style heatmaps)
sequential_ramp_colors <- function(palette = "default", manual_colors = NULL) {
  if (identical(palette, "manual") && !is.null(manual_colors)) {
    return(list(low = manual_colors$low %||% "#FFFFFF", high = manual_colors$high %||% "#2C3E50"))
  }
  switch(palette %||% "default",
    okabeito = list(low = "#FFFFFF", high = "#D55E00"),
    viridis  = if (requireNamespace("viridisLite", quietly = TRUE)) {
      v <- viridisLite::viridis(2); list(low = v[1], high = v[2])
    } else list(low = "#FFFFFF", high = "#2C3E50"),
    set2     = if (requireNamespace("RColorBrewer", quietly = TRUE)) {
      s <- RColorBrewer::brewer.pal(3, "Set2"); list(low = "#FFFFFF", high = s[1])
    } else list(low = "#FFFFFF", high = "#2C3E50"),
    list(low = "#FFFFFF", high = "#2C3E50")
  )
}

# ---------------------------------------------------------------------------
# Generic ggplot scales (discrete / continuous / diverging) — engine reused
# by SC, Bulk and Spatial resolvers below.
# ---------------------------------------------------------------------------

sc_discrete_scale <- function(palette = "default", manual_colors = NULL, aesthetic = "color") {
  # NB: scale_color_viridis_d/_c sont des exports ggplot2 (>= 3.0), PAS du
  # package viridis -- l'ancien `viridis::scale_color_viridis_d` plantait
  # ("n'est pas un objet exporte") avec viridis >= 0.6.x.
  fn_m <- if (aesthetic == "color") ggplot2::scale_color_manual else ggplot2::scale_fill_manual
  fn_v <- if (aesthetic == "color") ggplot2::scale_color_viridis_d else ggplot2::scale_fill_viridis_d
  fn_b <- if (aesthetic == "color") ggplot2::scale_color_brewer   else ggplot2::scale_fill_brewer

  if (identical(palette, "manual") && length(manual_colors) > 0)
    return(fn_m(values = manual_colors))

  switch(palette %||% "default",
    okabeito = fn_m(values = .sc_okabe),
    viridis  = fn_v(option = "turbo"),
    set2     = fn_b(palette = "Set2"),
    NULL)
}

#' Continuous scale — "viridis"-family default (FeaturePlot/DotPlot/spatial maps)
sc_continuous_scale <- function(palette = "default", aesthetic = "color", gradient = NULL,
                                limits = NULL, na.value = "grey50") {
  fn_g <- if (aesthetic == "color") ggplot2::scale_color_gradient else ggplot2::scale_fill_gradient
  fn_v <- if (aesthetic == "color") ggplot2::scale_color_viridis_c else ggplot2::scale_fill_viridis_c

  if (identical(palette, "manual") && !is.null(gradient))
    return(fn_g(low = gradient$low %||% "#2166AC", high = gradient$high %||% "#B2182B",
               limits = limits, na.value = na.value))

  opt <- switch(palette %||% "default",
    okabeito = "plasma", viridis = "plasma", set2 = "viridis", "viridis")
  fn_v(option = opt, limits = limits, na.value = na.value)
}

#' Continuous scale — "plasma"-family default (pathway enrichment, pseudotime,
#' 2D expression density). Kept SEPARATE from sc_continuous_scale() so the
#' historical default look of these plots (plasma) is never silently swapped
#' to viridis for users who never touch the palette selector.
expression_continuous_scale <- function(palette = "default", aesthetic = "color", gradient = NULL,
                                        base_option = "plasma", direction = 1, na.value = "grey50") {
  fn_g <- if (aesthetic == "color") ggplot2::scale_color_gradient else ggplot2::scale_fill_gradient
  fn_v <- if (aesthetic == "color") ggplot2::scale_color_viridis_c else ggplot2::scale_fill_viridis_c

  if (identical(palette, "manual") && !is.null(gradient))
    return(fn_g(low = gradient$low %||% "#2166AC", high = gradient$high %||% "#B2182B", na.value = na.value))

  switch(palette %||% "default",
    okabeito = fn_g(low = "#FFFFFF", high = "#D55E00", na.value = na.value),
    set2     = fn_v(option = "viridis", direction = direction, na.value = na.value),
    fn_v(option = base_option, direction = direction, na.value = na.value)   # default/viridis: unchanged
  )
}

#' Diverging scale — returns NULL for "default" so callers keep their own
#' historical hardcoded fallback (Heatmap/correlation matrix blue-white-red).
sc_diverging_scale <- function(palette = "default", aesthetic = "fill", gradient = NULL,
                               na.value = "grey50", limits = NULL) {
  if (identical(palette %||% "default", "default")) return(NULL)
  fn_g2 <- if (aesthetic == "color") ggplot2::scale_color_gradient2 else ggplot2::scale_fill_gradient2
  cc <- diverging_ramp_colors(palette, gradient)
  fn_g2(low = cc$low, mid = cc$mid, high = cc$high, midpoint = 0, na.value = na.value, limits = limits)
}

sc_discrete_colors <- function(levels, palette = "default", manual_colors = NULL) {
  n <- length(levels)
  if (n == 0) return(NULL)
  if (identical(palette, "manual")) {
    # Manual mode: keep the Dark-3 base for levels without an explicit
    # override instead of flattening every untouched level to #999999
    # as soon as one color is customized.
    base <- grDevices::hcl.colors(n, palette = "Dark 3")
    if (length(manual_colors) > 0) {
      hit <- levels %in% names(manual_colors)
      base[hit] <- unname(manual_colors[levels[hit]])
    }
    return(stats::setNames(base, levels))
  }
  switch(palette %||% "default",
    okabeito = rep(.sc_okabe, length.out = n),
    viridis  = {
      if (requireNamespace("viridis", quietly = TRUE)) viridis::viridis(n, option = "turbo")
      else if (requireNamespace("viridisLite", quietly = TRUE)) viridisLite::viridis(n)
      else rep(.sc_okabe, length.out = n)
    },
    set2     = {
      if (requireNamespace("scales", quietly = TRUE) && requireNamespace("RColorBrewer", quietly = TRUE))
        scales::brewer_pal(palette = "Set2")(n)
      else rep(.sc_okabe, length.out = n)
    },
    NULL)
}

# ---------------------------------------------------------------------------
# Bulk palette helpers
# ---------------------------------------------------------------------------

bulk_color_scale <- function(palette = "default", manual_colors = NULL) {
  if (identical(palette, "manual") && !is.null(manual_colors) && length(manual_colors) > 0)
    return(ggplot2::scale_color_manual(values = manual_colors))
  switch(palette %||% "default",
    okabeito = ggplot2::scale_color_manual(values = .default_manual_colors(8)),
    viridis  = ggplot2::scale_color_viridis_d(),
    set2     = ggplot2::scale_color_brewer(palette = "Set2"),
    NULL
  )
}

bulk_annotation_colors <- function(levels, palette = "default", manual_colors = NULL) {
  palette <- palette %||% "default"
  if (identical(palette, "default")) return(NULL)
  levels <- unique(as.character(levels))
  n <- length(levels)
  if (n == 0) return(NULL)
  if (identical(palette, "manual")) {
    defaults <- .default_manual_colors(n)
    vals <- vapply(seq_along(levels), function(i) {
      v <- if (levels[i] %in% names(manual_colors)) manual_colors[[levels[i]]] else NA_character_
      if (is.null(v) || is.na(v) || !nzchar(v)) defaults[i] else v
    }, character(1))
    return(setNames(vals, levels))
  }
  cols <- switch(palette,
    okabeito = .default_manual_colors(n),
    viridis  = if (requireNamespace("viridisLite", quietly = TRUE)) viridisLite::viridis(n) else .default_manual_colors(n),
    set2     = if (requireNamespace("RColorBrewer", quietly = TRUE)) RColorBrewer::brewer.pal(max(3, n), "Set2")[seq_len(n)] else .default_manual_colors(n),
    .default_manual_colors(n)
  )
  setNames(cols, levels)
}

#' Semantic 3-level role colors (Up/Down/NS) — the canonical engine reused
#' by Bulk volcano/MA/Up-Down, SC volcano, and (via role_colors_generic())
#' Spatial hotspots/Ripley's K.
bulk_role_colors <- function(palette = "default", manual_colors = NULL) {
  presets <- list(
    default  = c(Up = "#E74C3C", Down = "#2980B9", NS = "#BDC3C7"),
    okabeito = c(Up = "#D55E00", Down = "#0072B2", NS = "#999999"),
    viridis  = if (requireNamespace("viridisLite", quietly = TRUE)) {
      v <- viridisLite::viridis(3); c(Up = v[3], Down = v[1], NS = "#BDC3C7")
    } else c(Up = "#E74C3C", Down = "#2980B9", NS = "#BDC3C7"),
    set2     = if (requireNamespace("RColorBrewer", quietly = TRUE)) {
      s <- RColorBrewer::brewer.pal(3, "Set2"); c(Up = s[1], Down = s[2], NS = "#BDC3C7")
    } else c(Up = "#E74C3C", Down = "#2980B9", NS = "#BDC3C7")
  )
  base <- presets[[palette %||% "default"]] %||% presets$default
  if (identical(palette, "manual") && !is.null(manual_colors)) {
    for (role in intersect(names(manual_colors), names(base))) {
      v <- manual_colors[[role]]
      if (!is.null(v) && nzchar(v)) base[[role]] <- v
    }
  }
  base
}

#' Generic re-labeled wrapper around bulk_role_colors() for any OTHER
#' domain whose 3-way semantic split uses different display labels
#' (Spatial hotspots: Hotspot/Coldspot/NS ; Ripley's K: Agregation/Dispersion/NS).
role_colors_generic <- function(pos_label, neg_label, neutral_label,
                                palette = "default", manual_colors = NULL) {
  manual_ud <- NULL
  if (identical(palette, "manual") && !is.null(manual_colors) && length(manual_colors) >= 3) {
    manual_ud <- stats::setNames(unname(manual_colors)[1:3], c("Up", "Down", "NS"))
  }
  base <- bulk_role_colors(palette, manual_ud)
  stats::setNames(c(base[["Up"]], base[["Down"]], base[["NS"]]),
                  c(pos_label, neg_label, neutral_label))
}

# ---------------------------------------------------------------------------
# ComplexHeatmap ramps (circlize::colorRamp2) — domain-agnostic despite the
# "bulk_" prefix (historical naming); reused by SC's build_sc_hierarchical_heatmap().
# ---------------------------------------------------------------------------

bulk_sequential_ramp <- function(domain, palette = "default", manual_colors = NULL) {
  if (!requireNamespace("circlize", quietly = TRUE)) return(NULL)
  cc <- sequential_ramp_colors(palette, manual_colors)
  lo <- suppressWarnings(min(domain, na.rm = TRUE)); hi <- suppressWarnings(max(domain, na.rm = TRUE))
  if (!is.finite(lo) || !is.finite(hi) || lo == hi) { lo <- 0; hi <- 1 }
  circlize::colorRamp2(c(lo, hi), c(cc$low, cc$high))
}

bulk_diverging_ramp <- function(domain, palette = "default", manual_colors = NULL, midpoint = 0) {
  if (!requireNamespace("circlize", quietly = TRUE)) return(NULL)
  cc <- diverging_ramp_colors(palette, manual_colors)
  lo <- suppressWarnings(min(domain, na.rm = TRUE)); hi <- suppressWarnings(max(domain, na.rm = TRUE))
  if (!is.finite(lo) || !is.finite(hi) || lo == hi) { lo <- midpoint - 1; hi <- midpoint + 1 }
  circlize::colorRamp2(c(lo, midpoint, hi), c(cc$low, cc$mid, cc$high))
}

# ---------------------------------------------------------------------------
# Spatial-wide palette resolvers (shared_rv$color_palette / manual_gradient /
# manual_discrete, set centrally by mod_spatial_viz.R).
# ---------------------------------------------------------------------------

spatial_discrete_colors <- function(levels, shared_rv, kind = "cluster") {
  levels <- sort(unique(stats::na.omit(as.character(levels))))
  if (length(levels) == 0) return(NULL)
  pal <- state_get(shared_rv, "color_palette") %||% "default"
  manual <- (state_get(shared_rv, "manual_discrete") %||% list())[[kind]]
  cols <- sc_discrete_colors(levels, palette = pal, manual_colors = manual)
  if (is.null(cols)) cols <- grDevices::hcl.colors(length(levels), palette = "Dark 3")
  stats::setNames(cols, levels)
}

spatial_continuous_scale <- function(shared_rv, aesthetic = "color", limits = NULL) {
  sc_continuous_scale(palette = state_get(shared_rv, "color_palette") %||% "default", aesthetic = aesthetic,
                      gradient = state_get(shared_rv, "manual_gradient"), limits = limits)
}

spatial_diverging_scale <- function(shared_rv, aesthetic = "fill", na.value = "grey50", limits = NULL) {
  sc <- sc_diverging_scale(palette = state_get(shared_rv, "color_palette") %||% "default", aesthetic = aesthetic,
                           gradient = state_get(shared_rv, "manual_gradient"), na.value = na.value, limits = limits)
  if (!is.null(sc)) return(sc)
  fn <- if (aesthetic == "color") ggplot2::scale_color_gradient2 else ggplot2::scale_fill_gradient2
  fn(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0, na.value = na.value, limits = limits)
}

#' 256-stop hex ramp for leaflet::colorNumeric() — factors out logic
#' previously duplicated between mod_spatial_viz.R and mod_spatial_multi.R.
spatial_color_ramp_hex <- function(shared_rv) {
  pal  <- state_get(shared_rv, "color_palette") %||% "default"
  grad <- state_get(shared_rv, "manual_gradient")
  switch(pal,
    manual   = grDevices::colorRampPalette(c(grad$low %||% "#2166AC", grad$high %||% "#B2182B"))(256),
    okabeito = grDevices::colorRampPalette(c("#FFFFFF", "#D55E00"))(256),
    set2     = if (requireNamespace("RColorBrewer", quietly = TRUE))
                 grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(256) else "viridis",
    "viridis"
  )
}

spatial_hotspot_colors <- function(shared_rv) {
  role_colors_generic("Hotspot (chaud)", "Coldspot (froid)", "NS",
                      palette = state_get(shared_rv, "color_palette") %||% "default")
}

spatial_ripley_colors <- function(shared_rv) {
  role_colors_generic("Agregation", "Dispersion", "NS",
                      palette = state_get(shared_rv, "color_palette") %||% "default")
}

dynamic_manual_color_picker_ui <- function(ns, kind, levels, current_colors = NULL, defaults = NULL) {
  if (length(levels) == 0) return(NULL)
  if (is.null(defaults)) defaults <- stats::setNames(grDevices::hcl.colors(length(levels), palette = "Dark 3"), levels)
  input_id <- paste0("manual_discrete_", kind)
  div(style = "display:flex;flex-wrap:wrap;gap:10px;align-items:center;padding:6px 0;max-height:220px;overflow-y:auto;",
      lapply(levels, function(lv) {
        full_id <- ns(paste0(input_id, "__", gsub("[^A-Za-z0-9]+", "_", lv)))
        # Named-vector [[ ]] raises "subscript out of bounds" on missing
        # levels (e.g. new cluster set), so look up by name explicitly.
        .pick <- function(x) if (!is.null(x) && lv %in% names(x)) x[[lv]] else NULL
        val <- .pick(current_colors) %||% .pick(defaults) %||% "#999999"
        div(style = "display:flex;align-items:center;gap:5px;",
            tags$input(type = "color", id = full_id, value = val,
                       style = "width:30px;height:24px;border:1px solid #ccc;border-radius:4px;padding:0;cursor:pointer;",
                       onchange = sprintf(
                         "Shiny.setInputValue('%s', {level: '%s', color: this.value, ts: Date.now()}, {priority:'event'})",
                         ns(paste0(input_id, "_change")), gsub("'", "\\\\'", lv)
                       )),
            tags$span(lv, style = "font-size:0.78em;"))
      })
  )
}

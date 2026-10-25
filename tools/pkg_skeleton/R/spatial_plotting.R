# =============================================================================
# tools/pkg_skeleton/R/spatial_plotting.R — Spatial visualization rendering helpers
# =============================================================================
# Extracted from modules/spatial/mod_spatial_viz.R (Block 6 refactor).
# Pure rendering functions: no Shiny reactivity, no module namespace.
# Called by mod_spatial_viz_server() and potentially by report/export code.
#
# Depends on: R/palettes.R (spatial_discrete_colors, spatial_continuous_scale,
#             spatial_diverging_scale, spatial_color_ramp_hex),
#             ggplot2, plotly, scattermore (optional).
# Sourced in app.R AFTER R/palettes.R and tools/pkg_skeleton/R/spatial_io.R,
# BEFORE modules/spatial/*.R.
# =============================================================================

get_available_resolutions <- function(hist_data) {
  if (is.null(hist_data)) return(character(0))
  
  # Cas 1 : structure avec une liste "images"
  if (!is.null(hist_data$images) && is.list(hist_data$images)) {
    return(names(hist_data$images))
  }
  
  # Cas 2 : éléments de premier niveau qui contiennent un raster ou rgba
  candidates <- names(hist_data)[
    vapply(hist_data, function(x) {
      is.list(x) && any(c("raster", "rgba") %in% names(x))
    }, logical(1))
  ]
  if (length(candidates) > 0) return(candidates)
  
  # Cas 3 : noms connus (fallback)
  known <- c("lowres", "hires")
  known <- known[known %in% names(hist_data)]
  if (length(known) > 0) return(known)
  
  character(0)
}

make_plotly_histology_image <- function(hist_ov, opacity = 0.7) {
  if (
    is.null(hist_ov) ||
    is.null(hist_ov$data_uri) ||
    !nzchar(hist_ov$data_uri) ||
    is.null(hist_ov$bounds)
  ) {
    return(NULL)
  }
  
  b <- hist_ov$bounds
  
  xmin <- b$x[1L]
  xmax <- b$x[2L]
  ymin <- b$y[1L]
  ymax <- b$y[2L]
  
  # Les points utilisent y_display = -y.
  # Les coordonnées de l'image Plotly doivent couvrir [-ymax, -ymin].
  # Avec yanchor = "bottom", y est le bord inférieur de l'image.
  list(
    source = hist_ov$data_uri,
    xref = "x",
    yref = "y",
    x = xmin,
    y = -ymax,
    sizex = xmax - xmin,
    sizey = ymax - ymin,
    xanchor = "left",
    yanchor = "bottom",
    sizing = "stretch",
    opacity = opacity,
    layer = "below"
  )
}

compute_spatial_ranges <- function(df_all, hist_ov = NULL, show_hist = FALSE) {
  x_vals <- df_all$x
  y_vals <- -df_all$y
  
  if (
    isTRUE(show_hist) &&
    !is.null(hist_ov) &&
    !is.null(hist_ov$bounds)
  ) {
    xmin <- hist_ov$bounds$x[1L]
    xmax <- hist_ov$bounds$x[2L]
    ymin <- hist_ov$bounds$y[1L]
    ymax <- hist_ov$bounds$y[2L]
    
    # FIX (audit step 3.12): a resolution switch can momentarily produce
    # non-finite or degenerate bounds (0-width/height) while the new
    # histology_overlay() is recomputing -- feeding those into
    # coord_fixed() crashed the static PNG preview (graphics::plot.new
    # error reported by user). Skip the histology contribution to the
    # range for that one frame rather than propagating garbage; the
    # points' own bounds are always used as a safe minimum.
    bounds_ok <- all(is.finite(c(xmin, xmax, ymin, ymax))) && xmax > xmin && ymax > ymin
    if (bounds_ok) {
      x_vals <- c(x_vals, xmin, xmax)
      y_vals <- c(y_vals, -ymax, -ymin)
    }
  }
  
  list(
    x = range(x_vals, na.rm = TRUE),
    y = range(y_vals, na.rm = TRUE)
  )
}

is_valid_histology_overlay <- function(hist_ov) {
  if (is.null(hist_ov) || is.null(hist_ov$bounds) || is.null(hist_ov$rgba)) {
    return(FALSE)
  }
  
  b <- hist_ov$bounds
  rgba_dim <- dim(hist_ov$rgba)
  
  if (
    length(rgba_dim) != 3L ||
    rgba_dim[1L] < 1L ||
    rgba_dim[2L] < 1L ||
    rgba_dim[3L] < 3L
  ) {
    return(FALSE)
  }
  
  if (
    is.null(b$x) ||
    is.null(b$y) ||
    length(b$x) != 2L ||
    length(b$y) != 2L
  ) {
    return(FALSE)
  }
  
  all(is.finite(c(b$x, b$y))) &&
    b$x[2L] > b$x[1L] &&
    b$y[2L] > b$y[1L]
}

safe_static_histology_raster <- function(hist_ov, max_pixels = 25000000L) {
  if (!is_valid_histology_overlay(hist_ov)) {
    return(NULL)
  }
  
  rgba <- hist_ov$rgba
  image_dim <- dim(rgba)
  n_pixels <- as.double(image_dim[1L]) * as.double(image_dim[2L])
  
  # Avoid allocating a very large character raster in RAM.
  if (!is.finite(n_pixels) || n_pixels < 1 || n_pixels > max_pixels) {
    return(NULL)
  }
  
  tryCatch({
    red <- pmin(1, pmax(0, rgba[, , 1L]))
    green <- pmin(1, pmax(0, rgba[, , 2L]))
    blue <- pmin(1, pmax(0, rgba[, , 3L]))
    
    alpha <- if (image_dim[3L] >= 4L) {
      pmin(1, pmax(0, rgba[, , 4L]))
    } else {
      matrix(1, nrow = image_dim[1L], ncol = image_dim[2L])
    }
    
    grDevices::as.raster(
      grDevices::rgb(
        red = as.vector(red),
        green = as.vector(green),
        blue = as.vector(blue),
        alpha = as.vector(alpha)
      )
    ) |>
      matrix(nrow = image_dim[1L], ncol = image_dim[2L])
  }, error = function(e) {
    NULL
  })
}

safe_plot_range <- function(x, fallback = c(0, 1)) {
  out <- suppressWarnings(range(x[is.finite(x)], na.rm = TRUE))
  
  if (length(out) != 2L || !all(is.finite(out))) {
    return(fallback)
  }
  
  if (identical(out[1L], out[2L])) {
    delta <- max(abs(out[1L]) * 0.02, 1)
    out <- out + c(-delta, delta)
  }
  
  out
}

build_histology_debug_text <- function(hist_ov) {
  if (is.null(hist_ov)) return("Histology overlay = NULL")
  d <- hist_ov$diag
  paste0(
    "Histology debug | data_uri: ", !is.null(hist_ov$data_uri),
    " | raster dims: ", paste(dim(hist_ov$raster_obj), collapse = " x "),
    " | x=[", paste(round(hist_ov$bounds$x, 2), collapse = ", "), "]",
    " | y=[", paste(round(hist_ov$bounds$y, 2), collapse = ", "), "]",
    if (!is.null(d)) {
      sprintf(
        " | pct_near_white=%.1f%% | n_unique_colors=%d | mean_rgb=[%s]",
        d$pct_near_white, d$n_unique_colors, paste(d$mean_rgb, collapse = ",")
      )
    } else ""
  )
}

.esc_js <- function(x) gsub("'", "\\'", x, fixed = TRUE)

scale_alpha_by_value <- function(v, alpha_range = c(0.15, 1)) {
  rng <- suppressWarnings(range(v[is.finite(v)]))
  if (!all(is.finite(rng)) || diff(rng) == 0) return(rep(alpha_range[2], length(v)))
  norm <- (v - rng[1]) / diff(rng)
  norm[!is.finite(norm)] <- 0
  alpha_range[1] + norm * diff(alpha_range)
}

sort_cluster_labels <- function(x) {
  x <- unique(stats::na.omit(as.character(x)))
  if (length(x) == 0) return(x)
  if (all(grepl("^[0-9]+$", x))) x[order(as.integer(x))] else sort(x)
}

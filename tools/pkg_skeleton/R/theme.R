# =============================================================================
# R/plotting/theme.R — Shared ggplot theme resolver (PLOT-S1)
# =============================================================================
# Single source of truth for the app's ggplot themes + configurable base_size.
# Consumers: Bulk (R/bulk/bulk_helpers.R), SC (R/sc/sc_plotting.R) and Spatial
# (R/spatial/spatial_export.R, modules/spatial/*). It replaces:
#   * the local `switch()` that lived in R/sc/sc_plotting.R:37, and
#   * the ~20 hardcoded theme_*() calls scattered across the repo.
#
# Zero-visual-change contract (frozen by tests/testthat/test-plot-theme.R):
#   * the 4 choices are exactly minimal / classic / bw / void;
#   * an unknown / NA / length-0 choice falls back to "minimal" and NEVER
#     errors — a bad selectInput value must not blank every plot of a module;
#   * the default base_size is 11, which IS ggplot2's own default (verified on
#     ggplot2 4.0.3: theme_minimal()$text$size == 11). Therefore every bare
#     theme_*() call migrated to ts_theme() renders exactly as before;
#   * each migrated site KEEPS its historical base_size (12/13/15) explicitly —
#     the repo was already inconsistent, so a silent normalisation would change
#     the rendering of 16+ sites (see docs/archive/ROADMAP_HANDOFF_NEXT.md section 3).
#
# Sourced in app.R right after R/plotting/palettes.R.
# =============================================================================

#' Shared ggplot theme resolver — single source of truth.
#'
#' @param theme_choice One of "minimal" (default), "classic", "bw", "void".
#'   Any other value (unknown, NA, NULL, length != 1) falls back to "minimal".
#' @param base_size Base font size in points. Default 11 — ggplot2's own
#'   default, so `ts_theme("minimal")` renders exactly like `theme_minimal()`.
#'   NULL / NA / length != 1 also falls back to 11.
#' @return A ggplot2 theme object (never NULL, never an error).
ts_theme <- function(theme_choice = "minimal", base_size = 11) {
  # Defensive normalisation: NULL / NA / length-0 must never reach `switch()`.
  if (is.null(theme_choice) || length(theme_choice) != 1L || is.na(theme_choice)) {
    theme_choice <- "minimal"
  }
  if (is.null(base_size) || length(base_size) != 1L || is.na(base_size)) {
    base_size <- 11
  }
  base_size <- as.numeric(base_size)

  switch(as.character(theme_choice),
    classic = ggplot2::theme_classic(base_size = base_size),
    bw      = ggplot2::theme_bw(base_size = base_size),
    void    = ggplot2::theme_void(base_size = base_size),
    minimal = ggplot2::theme_minimal(base_size = base_size),
    # Unknown choice -> minimal (graceful degradation, never a hard stop).
    ggplot2::theme_minimal(base_size = base_size)
  )
}

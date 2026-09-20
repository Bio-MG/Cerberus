# =============================================================================
# R/spatial/spatial_report.R — Spatial HTML/PDF report: dataset snapshot builder
# =============================================================================
# Companion to modules/spatial/mod_spatial_report.R +
# modules/spatial/spatial_report_template.Rmd (+ its child template). Pure
# function only (no Shiny reactivity) -- called on the main Shiny process
# right before rmarkdown::render(), never inside a mirai daemon (same
# convention as R/spatial/spatial_export.R).
# ⚠️ En-tête corrigé en §2df : il annonçait `R/utils_spatial_report.R`, chemin
# qui n'existe PLUS depuis le déménagement vers `R/spatial/`. ⚠️ Ce défaut de
# prose est SYSTÉMIQUE : **10** autres fichiers de `R/spatial/` portent encore
# un en-tête `R/utils_*.R` (mesuré, non corrigés ici — hors périmètre).
#
# The report itself NEVER receives a live Seurat/BPCells object (same hard
# rule as the rest of this app, see R/spatial/spatial_io.R header) -- only
# small, already-computed pieces (coords, qc_metrics, cluster_labels, ...)
# that were already sitting in shared_rv/global_data. "LE MAXIMUM D'EXPORT,
# meme si non affiche, juste calcule" (feedback biologiste) is satisfied by
# the Rmd rendering every section for which a RESULT already exists,
# regardless of which UI tab the user currently has open -- not by adding a
# separate plot-image cache subsystem (Spatial has no per-plot "add to
# report" builder the way mod_sc_viz.R does for Single-Cell; every plot here
# is cheaply re-derived from small cached data.frames at render time).
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Build one dataset's report snapshot (light, serializable, Rmd-ready)
#'
#' @param spatial_obj The dataset's spatial_obj list (either the currently
#'   active `global_data$spatial_obj`, or one entry of
#'   `global_data$spatial_datasets` for a non-active dataset in a
#'   multi-sample report).
#' @param results A plain list snapshot of the relevant shared_rv fields --
#'   qc_metrics/qc_pass_idx/qc_params, cluster_labels/cluster_params/
#'   cluster_markers, deconv_props/deconv_params, moran_results/
#'   moran_params, niche_labels/niche_composition/niche_params, umap_df.
#'   Pass shared_rv's LIVE values for the active dataset, or
#'   `global_data$spatial_results_cache[[name]]` for any other one (see
#'   mod_spatial_report.R server for the exact wiring) -- NULL/missing
#'   entries are handled gracefully throughout (section simply omitted).
#' @return A plain list: project, technology, n_total, coords, sketch_ids,
#'   sketch, results (passed through unchanged).
build_spatial_report_dataset <- function(spatial_obj, results = list()) {
  list(
    project    = spatial_obj$project %||% "?",
    technology = spatial_obj$technology %||% "?",
    n_total    = spatial_obj$n_total %||% NA_integer_,
    coords     = spatial_obj$coords,
    sketch_ids = if (!is.null(spatial_obj$sketch)) colnames(spatial_obj$sketch) else character(0),
    # The RAM sketch itself (plain Seurat object, not reactive) -- needed by
    # build_saved_viz_df() (R/spatial/spatial_export.R) to re-derive gene
    # expression for a saved custom view's "Gene" color mode. rmarkdown::
    # render() runs in-process here (mod_spatial_report.R never spawns a
    # subprocess), so passing this through `params` costs no extra
    # serialization -- it's the same bounded (<= max_sketch cells) object
    # already resident in global_data$spatial_datasets.
    sketch     = spatial_obj$sketch,
    # Tissue background for report maps (bounds + rgba + plotly data URI).
    # NULL-safe: datasets imported without images render spots-only.
    # 🔴 Le corps ci-dessous est VOLONTAIREMENT une succession de `if` qui
    # RENDENT `NULL`, et JAMAIS un `return(NULL)` : ce `tryCatch` est un
    # ARGUMENT d'un appel `list(...)`, et en R `return()` remonte au cadre de
    # la FONCTION (pas du `tryCatch`) ⇒ il sortait de
    # `build_spatial_report_dataset()` ENTIÈREMENT, qui rendait donc `NULL`
    # au lieu du snapshot documenté, pour tout dataset SANS histologie — le
    # cas ordinaire. Voir tests/testthat/test-spatial-report.R, section 0.
    histology_overlay = tryCatch({
      .hi <- if (is.null(spatial_obj$histology)) NULL else
        get_histology_raster(hist_data = spatial_obj$histology,
                             resolution = "hires")
      .ok <- !is.null(.hi) && !is.null(.hi$rgba) && !is.null(.hi$dim) &&
        length(.hi$dim) >= 2L
      .npix <- if (.ok) {
        as.double(dim(.hi$rgba)[1L]) * as.double(dim(.hi$rgba)[2L])
      } else 0
      if (!.ok || !is.finite(.npix) || .npix <= 0 || .npix > 40000000) {
        NULL
      } else {
        .sf <- .hi$scale_factor %||% 1
        if (!is.finite(.sf) || .sf <= 0) .sf <- 1
        .uri <- tryCatch({
          .raw <- png::writePNG(.hi$rgba)
          .u <- if (requireNamespace("base64enc", quietly = TRUE))
            base64enc::dataURI(.raw, mime = "image/png") else NULL
          if (!is.null(.u)) .u <- gsub("[\r\n[:space:]]+", "", .u)
          .u
        }, error = function(e) NULL)
        list(
          rgba     = .hi$rgba,
          data_uri = .uri,
          bounds   = list(x = c(0, .hi$dim[2L] / .sf),
                          y = c(0, .hi$dim[1L] / .sf))
        )
      }
    }, error = function(e) NULL),
    results    = results %||% list()
  )
}

#' Locate spatial_report_template.Rmd robustly
#'
#' Same defensive multi-candidate pattern as
#' modules/bulk/mod_bulk_report.R's own `.find_bulk_report_template()` --
#' working directory at render time isn't guaranteed identical to the
#' project root for every launch method.
#'
#' @return Character path to the template file.
#' @keywords internal
find_spatial_report_template <- function() {
  candidates <- unique(c(
    file.path("reports", "spatial_report_template.Rmd"),
    file.path(getwd(), "reports", "spatial_report_template.Rmd"),
    file.path("modules", "spatial", "spatial_report_template.Rmd"),
    file.path(getwd(), "modules", "spatial", "spatial_report_template.Rmd")
  ))
  hit <- Filter(file.exists, candidates)
  if (length(hit) == 0) {
    stop(
      "\u274c Template 'spatial_report_template.Rmd' introuvable. Chemins essayes :\n  - ",
      paste(candidates, collapse = "\n  - "),
      "\nVerifiez que l'application est lancee depuis la racine du projet ",
      "(shiny::runApp() depuis le dossier contenant app.R).",
      call. = FALSE
    )
  }
  hit[[1]]
}

# =============================================================================
# mod_sc_trajectory_export.R — Drive export route for SC trajectory pseudotime
# =============================================================================
# Counterpart of mod_sc_markers_export.R for route `sc_trajectory_pseudotime_csv`
# (declared in TS_DRIVE_EXPORT_ROUTES).
#
# WHAT THE ARTEFACT IS:
# Per-cell pseudotime and lineage ordering table.
# Guaranteed columns: cell_id, pseudotime.
# Additional trajectory metrics and multi-lineage curves ride along when present.
#
# Returns a VERDICT, never throws.
# =============================================================================

#' The next unused export path under `dir` for SC trajectory pseudotime.
#' @param dir Directory to write into.
#' @return File path string.
sc_trajectory_export_next_path <- function(dir) {
  stem <- TS_DRIVE_EXPORT_STEM_SC_TRAJECTORY
  present <- list.files(dir, pattern = paste0("^", stem, "_[0-9]+\\.csv$"))
  idx <- suppressWarnings(as.integer(sub(paste0("^", stem, "_"), "",
                                         sub("\\.csv$", "", present))))
  idx <- idx[!is.na(idx)]
  n <- if (length(idx)) max(idx) + 1L else 1L
  file.path(dir, sprintf("%s_%d.csv", stem, n))
}

#' Export the trajectory pseudotime through the drive route.
#'
#' @param shared_rv The module's reactive values.
#' @param global_data The app-wide reactiveValues (reads `sc_obj`).
#' @param dir Destination override (for tests only).
#' @return list(ok, status, errors, descriptor)
sc_trajectory_export_pseudotime_csv <- function(shared_rv, global_data, dir = NULL) {
  bad <- function(status, msg) {
    list(ok = FALSE, status = status, errors = msg, descriptor = NULL)
  }
  obj <- tryCatch(
    shiny::isolate(global_data$sc_obj),
    error = function(e) NULL
  )
  if (is.null(obj)) {
    obj <- tryCatch(shiny::isolate(shared_rv$sc_obj), error = function(e) NULL)
  }
  if (is.null(obj) || !inherits(obj, "Seurat")) {
    return(bad("invalid", "this session has produced no Seurat object yet; load data or run pipeline first"))
  }
  meta <- obj@meta.data

  pt_cols <- c("pseudotime", grep("^pseudotime", colnames(meta), value = TRUE))
  pt_cols <- unique(pt_cols[pt_cols %in% colnames(meta)])
  if (length(pt_cols) == 0L) {
    return(bad("invalid", "no trajectory or pseudotime has been computed yet; run trajectory analysis first"))
  }
  pt_val <- meta[[pt_cols[1]]]
  if (!is.numeric(pt_val) || all(is.na(pt_val))) {
    return(bad("invalid", "stored pseudotime column contains no numeric values"))
  }

  df <- data.frame(
    cell_id = rownames(meta),
    pseudotime = as.numeric(pt_val),
    stringsAsFactors = FALSE
  )

  # Include lineage/component columns if present
  for (extra in c("traj_method", "in_root_component", "traj_in_root_component", "traj_root_cluster")) {
    if (extra %in% colnames(meta)) df[[extra]] <- meta[[extra]]
  }

  # Include additional curve pseudotimes if slingshot multi-lineage
  for (c_col in pt_cols[-1]) {
    df[[c_col]] <- meta[[c_col]]
  }

  ts_drive_export_write_table(df, sc_trajectory_export_next_path, dir)
}

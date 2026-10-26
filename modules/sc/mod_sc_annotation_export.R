# =============================================================================
# mod_sc_annotation_export.R — Drive export route for SC cell metadata
# =============================================================================
# Counterpart of mod_sc_markers_export.R for route `sc_cell_metadata_csv`
# (declared in TS_DRIVE_EXPORT_ROUTES).
#
# WHAT THE ARTEFACT IS:
# Per-cell annotation and metadata table. Guaranteed columns: cell_id, cluster.
# Additional annotations (SingleR labels, QC metrics, cell type) ride along
# when present.
#
# Returns a VERDICT, never throws.
# =============================================================================

#' The next unused export path under `dir` for SC cell metadata.
#' @param dir Directory to write into.
#' @return File path string.
sc_annotation_export_next_path <- function(dir) {
  stem <- TS_DRIVE_EXPORT_STEM_SC_ANNOTATION
  present <- list.files(dir, pattern = paste0("^", stem, "_[0-9]+\\.csv$"))
  idx <- suppressWarnings(as.integer(sub(paste0("^", stem, "_"), "",
                                         sub("\\.csv$", "", present))))
  idx <- idx[!is.na(idx)]
  n <- if (length(idx)) max(idx) + 1L else 1L
  file.path(dir, sprintf("%s_%d.csv", stem, n))
}

#' Export the cell metadata through the drive route.
#'
#' @param shared_rv The module's reactive values.
#' @param global_data The app-wide reactiveValues (reads `sc_obj`).
#' @param dir Destination override (for tests only).
#' @return list(ok, status, errors, descriptor)
sc_annotation_export_cell_metadata_csv <- function(shared_rv, global_data, dir = NULL) {
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
  if (!is.data.frame(meta) || nrow(meta) == 0L) {
    return(bad("invalid", "object metadata is empty"))
  }

  cell_id <- rownames(meta)
  cluster <- if ("seurat_clusters" %in% colnames(meta)) {
    as.character(meta$seurat_clusters)
  } else if ("cluster" %in% colnames(meta)) {
    as.character(meta$cluster)
  } else if ("active_ident" %in% colnames(meta)) {
    as.character(meta$active_ident)
  } else {
    as.character(Seurat::Idents(obj))
  }
  if (length(cluster) != length(cell_id)) {
    cluster <- rep("0", length(cell_id))
  }

  df <- data.frame(
    cell_id = cell_id,
    cluster = cluster,
    stringsAsFactors = FALSE
  )

  # Include annotation columns if present
  annot_cols <- grep("^(SingleR_|cell_type|celltype|annotation|predicted_)", colnames(meta), value = TRUE)
  for (col in annot_cols) {
    df[[col]] <- meta[[col]]
  }

  # Include QC metrics if present
  for (qc_col in c("nFeature_RNA", "nCount_RNA", "percent.mt", "percent_mito")) {
    if (qc_col %in% colnames(meta) && !qc_col %in% colnames(df)) {
      df[[qc_col]] <- meta[[qc_col]]
    }
  }

  ts_drive_export_write_table(df, sc_annotation_export_next_path, dir)
}

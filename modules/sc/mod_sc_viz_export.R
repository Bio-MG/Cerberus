# =============================================================================
# mod_sc_viz_export.R — Drive export route for SC 2D embeddings
# =============================================================================
# Counterpart of mod_sc_markers_export.R for route `sc_embeddings_csv`
# (declared in TS_DRIVE_EXPORT_ROUTES).
#
# WHAT THE ARTEFACT IS:
# 2D cell coordinate matrix from the active/primary dimension reduction
# (UMAP -> t-SNE -> PCA). Guaranteed column: cell_id.
# Full columns per mode:
#   mode_umap: cell_id, UMAP_1, UMAP_2
#   mode_tsne: cell_id, tSNE_1, tSNE_2
#   mode_pca:  cell_id, PC_1, PC_2
#
# Returns a VERDICT, never throws.
# =============================================================================

#' The next unused export path under `dir` for SC embeddings.
#' @param dir Directory to write into.
#' @param stem Stem to match against (default: TS_DRIVE_EXPORT_STEM_SC_EMBEDDINGS).
#' @return File path string.
sc_viz_export_next_path <- function(dir, stem = TS_DRIVE_EXPORT_STEM_SC_EMBEDDINGS) {
  present <- list.files(dir, pattern = paste0("^", stem, "_[0-9]+\\.csv$"))
  idx <- suppressWarnings(as.integer(sub(paste0("^", stem, "_"), "",
                                         sub("\\.csv$", "", present))))
  idx <- idx[!is.na(idx)]
  n <- if (length(idx)) max(idx) + 1L else 1L
  file.path(dir, sprintf("%s_%d.csv", stem, n))
}

#' Export the 2D cell embeddings through the drive route.
#'
#' @param shared_rv The module's reactive values.
#' @param global_data The app-wide reactiveValues (reads `sc_obj`).
#' @param dir Destination override (for tests only).
#' @return list(ok, status, errors, descriptor)
sc_viz_export_embeddings_csv <- function(shared_rv, global_data, dir = NULL) {
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

  reductions <- names(obj@reductions)
  red_use <- NULL
  mode_name <- NULL
  stem_use <- TS_DRIVE_EXPORT_STEM_SC_EMBEDDINGS

  if ("umap" %in% tolower(reductions)) {
    red_use <- reductions[tolower(reductions) == "umap"][1]
    mode_name <- "mode_umap"
    stem_use <- "sc_embeddings_umap_2d"
  } else if (any(grepl("tsne", tolower(reductions)))) {
    red_use <- reductions[grepl("tsne", tolower(reductions))][1]
    mode_name <- "mode_tsne"
    stem_use <- "sc_embeddings_tsne_2d"
  } else if ("pca" %in% tolower(reductions)) {
    red_use <- reductions[tolower(reductions) == "pca"][1]
    mode_name <- "mode_pca"
    stem_use <- "sc_embeddings_pca_2d"
  } else {
    return(bad("invalid", "the Seurat object has no 2D dimension reductions (UMAP, t-SNE, or PCA)"))
  }

  emb <- Seurat::Embeddings(obj, reduction = red_use)
  if (is.null(emb) || nrow(emb) == 0L || ncol(emb) < 2L) {
    return(bad("invalid", "reduction embeddings are empty or have fewer than 2 dimensions"))
  }

  col1_name <- if (identical(mode_name, "mode_umap")) "UMAP_1" else if (identical(mode_name, "mode_tsne")) "tSNE_1" else "PC_1"
  col2_name <- if (identical(mode_name, "mode_umap")) "UMAP_2" else if (identical(mode_name, "mode_tsne")) "tSNE_2" else "PC_2"

  df <- data.frame(
    cell_id = rownames(emb),
    stringsAsFactors = FALSE
  )
  df[[col1_name]] <- as.numeric(emb[, 1])
  df[[col2_name]] <- as.numeric(emb[, 2])

  if ("seurat_clusters" %in% colnames(obj@meta.data)) {
    df$cluster <- as.character(obj@meta.data$seurat_clusters)
  }

  next_path_fn <- function(d) sc_viz_export_next_path(d, stem = stem_use)
  ts_drive_export_write_table(df, next_path_fn, dir)
}

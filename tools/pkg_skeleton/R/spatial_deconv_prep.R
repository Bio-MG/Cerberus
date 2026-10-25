# =============================================================================
# tools/pkg_skeleton/R/spatial_deconv_prep.R — RAM-safety gene capping before deconvolution
# =============================================================================
# Backlog #6 (HANDOFF_TranscriptoShiny.txt): STdeconvolve's restrictCorpus()
# received the FULL filtered gene panel via as.matrix(methods::as(mat,
# "dgCMatrix")) -- a real dense-matrix RAM spike on a large Visium HD/
# Slide-seq dataset (e.g. ~20k genes x 50k+ spots). RCTD's Reference()/
# SpatialRNA() had no gene cap at all either. Both now go through a shared
# top-N HVG pre-filter, computed WITHOUT ever densifying the full matrix
# (NormalizeData()+FindVariableFeatures() stream over a BPCells
# IterableMatrix -- same pattern already used by mod_spatial_cluster.R and
# mod_spatial_qc.R to pick HVGs on-disk).
#
# Pure functions, no Shiny reactivity -- called from inside a mirai daemon
# (via run_spatial_deconv_body(), tools/pkg_skeleton/R/spatial_deconv_tasks.R, itself
# called by mod_spatial_deconv.R and mod_spatial_pipeline.R), so this file
# MUST be added to init_spatial_daemons()'s source_files
# (tools/pkg_skeleton/R/spatial_async.R).
#
# No-op for small gene panels (Xenium/CosMx, typically < n_hvg genes):
# select_hvg_for_deconv() returns every gene unchanged in that case.
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Default HVG cap shared by every deconvolution entry point.
DECONV_DEFAULT_N_HVG <- if (exists("TS_DECONV_DEFAULT_N_HVG")) TS_DECONV_DEFAULT_N_HVG else 2000L

#' Select top-N highly variable genes from a (possibly disk-backed) counts matrix
#'
#' RAM-safe: NormalizeData()/FindVariableFeatures() stream over a BPCells
#' IterableMatrix without ever densifying it.
#'
#' @param mat Genes x spots/cells matrix (BPCells IterableMatrix or dgCMatrix).
#' @param n_hvg Integer, number of variable genes to keep.
#' @return Character vector of gene names (length <= n_hvg). Falls back to
#'   ALL genes if variable-feature selection fails for any reason -- never
#'   blocks the caller.
select_hvg_for_deconv <- function(mat, n_hvg = DECONV_DEFAULT_N_HVG) {
  n_hvg <- max(50L, as.integer(n_hvg %||% DECONV_DEFAULT_N_HVG))
  if (nrow(mat) <= n_hvg) return(rownames(mat))
  tryCatch({
    obj <- Seurat::CreateSeuratObject(counts = mat)
    obj <- Seurat::NormalizeData(obj, verbose = FALSE)
    obj <- Seurat::FindVariableFeatures(obj, nfeatures = n_hvg, verbose = FALSE)
    hvg <- Seurat::VariableFeatures(obj)
    if (length(hvg) == 0) rownames(mat) else hvg
  }, error = function(e) rownames(mat))
}

#' Cap a genes x spots matrix to its top-N HVG (single matrix, no reference
#' to intersect with -- see run_spatial_deconv_body()'s RCTD branch for the
#' reference-intersection variant, done inline there instead).
#'
#' @param mat Genes x spots/cells matrix (BPCells IterableMatrix or dgCMatrix).
#' @param n_hvg Integer, number of variable genes to keep.
#' @return list(matrix=, genes=, was_capped=, n_before=, n_after=).
cap_matrix_to_hvg <- function(mat, n_hvg = DECONV_DEFAULT_N_HVG) {
  n_before <- nrow(mat)
  hvg <- select_hvg_for_deconv(mat, n_hvg)
  idx <- match(hvg, rownames(mat))
  idx <- idx[!is.na(idx)]
  list(matrix = mat[idx, , drop = FALSE], genes = rownames(mat)[idx],
       was_capped = length(idx) < n_before, n_before = n_before, n_after = length(idx))
}

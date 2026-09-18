# =============================================================================
# R/utils_spatial_multi.R — Phase 4: "Working with multiple slices" (vignette
# parity) — multi-sample sketch integration
# =============================================================================
# Pure function (no Shiny reactivity) — called from inside a mirai daemon by
# modules/spatial/mod_spatial_multi.R, same pattern as the existing
# single-dataset sketch UMAP task (mod_spatial_viz.R): each dataset's
# $sketch is saveRDS()'d to a tempfile by the caller, and only PATHS travel
# into mirai::mirai() (hard rule: no live Shiny/reactive state, no
# BPCells/disk object, crosses into the daemon).
#
# DELIBERATELY SKETCH-ONLY (never the full BPCells-backed matrices): this
# project's hard constraint is a 32GB CPU-only workstation, and every other
# heavy multi-cell computation already scopes to the sketch (sketch UMAP,
# Top-SVG grid, "Vue combinee") — merging N FULL-resolution spatial
# matrices would instantly be the single most RAM-hungry operation in the
# whole app. A biologist who needs a full-resolution joint analysis should
# export each dataset's ROI (materialize_seurat_subset(), R/utils_spatial_io.R)
# and continue in a dedicated command-line pipeline instead.
#
# Deviates from the Seurat Spatial Vignette's own 2-slice example in two
# deliberate ways:
#   1. LogNormalize by default (not SCTransform) — same CPU-speed rationale
#      already applied to Label Transfer (mod_spatial_deconv.R v4); HVGs are
#      recomputed on the merged object rather than carried forward per
#      dataset (simpler, standard for a LogNormalize workflow).
#   2. Harmony batch correction is OPT-IN (checkbox in mod_spatial_multi.R,
#      default ON) rather than absent — the vignette's own 2-slice example
#      needs none (anterior/posterior halves of the SAME physical section,
#      minimal batch effect), but this app already tells users elsewhere
#      that Harmony is the recommended path for "plusieurs echantillons"
#      (see app.R help modal, Single-Cell workflow) — real multi-SAMPLE
#      comparisons (different tissue blocks/subjects/conditions) generally
#      need it for an honest joint analysis.
#
# merge(..., add.cell.ids=) is used defensively even though the vignette's
# own example omits it: 10x barcodes are drawn from a fixed whitelist, so
# two independently-uploaded Visium sections silently sharing identical
# barcodes (e.g. "AAACAAGTATCTCCCA-1" in both) is a realistic collision
# risk for arbitrary user data, unlike the vignette's own curated dataset.
# =============================================================================

#' Merge multiple spatial sketches and run a joint PCA/(Harmony)/UMAP/cluster
#'
#' @param sketch_paths Named list/character vector: dataset name -> path to
#'   an RDS file holding that dataset's $sketch Seurat object.
#' @param npcs Integer, PCA/Harmony dimensions (also used for
#'   FindNeighbors/RunUMAP `dims`).
#' @param resolution Numeric, Leiden/Louvain clustering resolution.
#' @param use_harmony Logical, run harmony::RunHarmony(group.by.vars="dataset")
#'   before UMAP/clustering. Falls back to plain PCA (with a warning) if the
#'   package is missing or the call errors — never aborts the whole task for
#'   this reason alone.
#' @param log_file Optional path for write_mirai_log() progress lines (NULL
#'   = silent, e.g. for unit testing outside a daemon).
#' @return list(
#'   embeddings     = data.frame(id, dataset, dim1, dim2, cluster),
#'   n_per_dataset  = named integer vector (cells contributed per dataset),
#'   reduction_used = "harmony" or "pca" (whichever UMAP/clustering ran on)
#' )
integrate_spatial_sketches <- function(sketch_paths, npcs = 30, resolution = 0.8,
                                        use_harmony = TRUE, log_file = NULL) {
  .log <- function(msg, step, total) {
    if (!is.null(log_file)) write_mirai_log(log_file, msg, step, total)
  }

  if (length(sketch_paths) < 2) {
    stop(errorCondition("Selectionnez au moins 2 echantillons pour l'integration multi-coupes.", class = "spatial_multi_error"))
  }

  .log("Chargement des sketches...", 1, 6)
  ds_names <- names(sketch_paths)
  objs <- lapply(ds_names, function(nm) {
    sk <- readRDS(sketch_paths[[nm]])
    # Standardize to a single raw-counts starting point regardless of each
    # dataset's own import-time normalization choice (LogNormalize vs SCT,
    # see mod_import_spatial.R's norm_method) -- every dataset must start
    # from the SAME processing state for a fair joint analysis, and a
    # brand-new RNA assay avoids dragging along a possibly-SCT-named assay
    # with per-dataset model parameters that would not survive merge().
    counts <- SeuratObject::LayerData(sk, layer = "counts")
    obj <- Seurat::CreateSeuratObject(counts = counts)
    obj$dataset <- nm
    obj
  })
  names(objs) <- ds_names

  .log("Fusion des echantillons (merge, ids prefixes par securite)...", 2, 6)
  merged <- if (length(objs) == 2) {
    merge(objs[[1]], objs[[2]], add.cell.ids = ds_names)
  } else {
    merge(objs[[1]], objs[-1], add.cell.ids = ds_names)
  }
  merged <- SeuratObject::JoinLayers(merged)

  .log("Normalisation + PCA conjointe (LogNormalize)...", 3, 6)
  merged <- Seurat::NormalizeData(merged, verbose = FALSE)
  merged <- Seurat::FindVariableFeatures(merged, verbose = FALSE)
  merged <- Seurat::ScaleData(merged, verbose = FALSE)
  n_pc <- max(2, min(npcs, ncol(merged) - 1, nrow(merged) - 1))
  merged <- Seurat::RunPCA(merged, npcs = n_pc, verbose = FALSE)

  reduction_use <- "pca"
  if (isTRUE(use_harmony)) {
    if (!requireNamespace("harmony", quietly = TRUE)) {
      .log("Package 'harmony' absent -- correction de batch ignoree (PCA brute utilisee).", 4, 6)
    } else {
      .log("Correction de batch (Harmony, groupe = echantillon)...", 4, 6)
      merged <- tryCatch({
        m <- harmony::RunHarmony(merged, group.by.vars = "dataset",
                                  dims.use = seq_len(n_pc), verbose = FALSE)
        reduction_use <<- "harmony"
        m
      }, error = function(e) {
        warning("Harmony a echoue (", conditionMessage(e), ") -- UMAP/clustering sur la PCA non corrigee.")
        merged
      })
    }
  } else {
    .log("Correction de batch (Harmony) desactivee -- PCA brute utilisee.", 4, 6)
  }

  .log(sprintf("UMAP + clustering (Leiden, reduction=%s)...", reduction_use), 5, 6)
  merged <- Seurat::RunUMAP(merged, reduction = reduction_use, dims = seq_len(n_pc), verbose = FALSE)
  merged <- Seurat::FindNeighbors(merged, reduction = reduction_use, dims = seq_len(n_pc), verbose = FALSE)
  merged <- tryCatch(
    Seurat::FindClusters(merged, resolution = resolution, algorithm = 4, verbose = FALSE),
    error = function(e) Seurat::FindClusters(merged, resolution = resolution, algorithm = 1, verbose = FALSE)
  )

  .log("Termine.", 6, 6)
  emb <- as.data.frame(Seurat::Embeddings(merged, "umap"))
  colnames(emb)[1:2] <- c("dim1", "dim2")
  emb$id      <- rownames(emb)
  emb$dataset <- merged$dataset
  emb$cluster <- as.character(merged$seurat_clusters)
  # merge(add.cell.ids=) prefixes every barcode as "<dataset>_<barcode>" --
  # strip it back off so callers can re-join these results against each
  # dataset's OWN $coords$id (which never had the prefix) for the
  # per-section spatial maps (mod_spatial_multi.R).
  emb$original_id <- substring(emb$id, nchar(emb$dataset) + 2)

  list(
    embeddings     = emb[, c("id", "original_id", "dataset", "dim1", "dim2", "cluster")],
    n_per_dataset  = table(merged$dataset),
    reduction_used = reduction_use
  )
}

# =============================================================================
# R/utils_spatial_reference.R — scRNA-seq reference I/O for spatial deconvolution
# =============================================================================
# v2 (round-2 fixes from real-data testing):
#   1. sanitize_celltype_labels() -- spacexr::check_cell_types() rejects any
#      level containing "/" ("Reference: levels(cell_types) contains a cell
#      type with name containing prohibited character /"). Real references
#      routinely have this (Allen cortex 'subclass' == "L2/3 IT"). Sanitized
#      ONCE here, shared by RCTD + Label Transfer, so both backends produce
#      the same column names.
#   2. prepare_reference_artifact(): merging rare types into "Autre" could
#      previously still leave "Autre" itself below min_cells_per_type when
#      there was only one (or a few, summing to too few) rare type(s) --
#      relabeling alone does not fix that. Cells whose merged bucket is
#      STILL below the minimum are now excluded entirely (reported back via
#      n_dropped_rare), so ticking "merge rare types" always results in a
#      reference RCTD can actually run on.
#   3. New `max_cells_per_type` parameter -- optional stratified per-type
#      subsampling (same spirit as helpers_sc.R::subsample_seurat_for_analysis())
#      applied before writing the artifact. Reported real-world timeout was
#      on a 73k-cell / ~20-cell-type reference; capping per type is a much
#      better RAM/speed lever than shrinking the whole reference uniformly
#      (rare types stay represented, common types get capped).
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# -----------------------------------------------------------------------------
# 1. Multi-format reference reader (main process only) -- unchanged from v1
# -----------------------------------------------------------------------------

#' Read a single-cell reference file (.rds / .h5ad / .h5 / .loom)
#' @param path Character path to the staged reference file.
#' @return A Seurat object, OR list(counts=, meta=).
read_reference_scrna <- function(path) {
  if (!file.exists(path)) stop(errorCondition(paste0("Fichier de reference introuvable : ", path), class = "spatial_reference_error"))
  ext <- tolower(tools::file_ext(path))

  if (identical(ext, "rds")) {
    obj <- readRDS(path)
    if (inherits(obj, "Seurat")) return(obj)
    if (is.list(obj) && !is.null(obj$counts)) {
      return(list(counts = obj$counts, meta = obj$meta %||% obj$metadata %||% NULL))
    }
    if (methods::is(obj, "Matrix") || is.matrix(obj)) {
      return(list(counts = obj, meta = NULL))
    }
    stop(errorCondition(".rds ne contient ni objet Seurat, ni liste counts/meta, ni matrice exploitable.", class = "spatial_reference_error"))
  }

  # .rda/.RData — "Inspecter d'abord, importer ensuite" (contrat
  # docs/contracts/RDATA_IMPORT_CONTRACT.md) : env isolé, objet unique
  # extrait et validé ici ; workspace multi-objets -> message orientant vers
  # l'aperçu d'import single-cell (export d'un objet -> .RData/.rds dédié).
  if (rdata_is_supported_file(path)) {
    env <- rdata_load_env(path)
    on.exit(rdata_free(env), add = TRUE)
    nms <- ls(envir = env)
    if (length(nms) > 1L) {
      stop(paste0(
        "Ce fichier .RData contient ", length(nms),
        " objets. Exportez la reference seule (objet unique) vers un .rds ",
        "ou un .RData dedie — par exemple depuis l'aperçu d'import Single-Cell ",
        "(« Exporter la sélection ») — puis reimportez-la."),
        call. = FALSE)
    }
    obj <- rdata_extract_object(env, nms[1])
    if (inherits(obj, "Seurat")) return(obj)
    if (is.list(obj) && !is.null(obj$counts)) {
      return(list(counts = obj$counts, meta = obj$meta %||% obj$metadata %||% NULL))
    }
    if (methods::is(obj, "Matrix") || is.matrix(obj)) {
      return(list(counts = obj, meta = NULL))
    }
    stop(errorCondition(".RData ne contient ni objet Seurat, ni liste counts/meta, ni matrice exploitable.", class = "spatial_reference_error"))
  }

  if (identical(ext, "h5ad")) {
    if (requireNamespace("schard", quietly = TRUE)) {
      return(schard::h5ad2seurat(path))
    }
    if (requireNamespace("SeuratDisk", quietly = TRUE)) {
      h5seurat <- sub("\\.h5ad$", ".h5seurat", path, ignore.case = TRUE)
      SeuratDisk::Convert(path, dest = h5seurat, overwrite = TRUE, verbose = FALSE)
      return(SeuratDisk::LoadH5Seurat(h5seurat))
    }
    stop(errorCondition(paste0("Aucun lecteur .h5ad disponible -- installez 'schard' ",
         "(remotes::install_github('cellgeni/schard'), recommande) ou 'SeuratDisk'."), class = "spatial_reference_error"))
  }

  if (identical(ext, "h5")) {
    if (!requireNamespace("hdf5r", quietly = TRUE)) {
      stop(errorCondition("Package 'hdf5r' requis pour lire les fichiers .h5.", class = "spatial_reference_error"))
    }
    counts <- Seurat::Read10X_h5(path)
    if (is.list(counts)) counts <- counts[[1]]
    return(list(counts = counts, meta = NULL))
  }

  if (identical(ext, "loom")) {
    if (requireNamespace("SeuratDisk", quietly = TRUE)) {
      lfile <- SeuratDisk::Connect(filename = path, mode = "r")
      on.exit(tryCatch(lfile$close_all(), error = function(e) NULL), add = TRUE)
      return(Seurat::as.Seurat(lfile))
    }
    stop(errorCondition(paste0("Aucun lecteur .loom disponible -- installez 'SeuratDisk' ",
         "(remotes::install_github('mojaveazure/seurat-disk'))."), class = "spatial_reference_error"))
  }

  stop(errorCondition(sprintf("Format de reference non supporte : '.%s' (attendu : .rds, .h5ad, .h5, .loom).", ext), class = "spatial_reference_error"))
}

#' Normalize a read_reference_scrna() result into a clean, single-assay Seurat object
prepare_reference_seurat <- function(raw_ref, project_name = "Reference") {
  if (inherits(raw_ref, "Seurat")) {
    obj <- raw_ref
    if (grepl("^5", as.character(utils::packageVersion("Seurat")))) {
      obj <- tryCatch(SeuratObject::JoinLayers(obj), error = function(e) obj)
    }
    return(obj)
  }
  if (is.list(raw_ref) && !is.null(raw_ref$counts)) {
    obj <- Seurat::CreateSeuratObject(counts = raw_ref$counts, project = project_name)
    if (!is.null(raw_ref$meta) && is.data.frame(raw_ref$meta)) {
      common <- intersect(rownames(raw_ref$meta), colnames(obj))
      if (length(common) > 0) {
        for (cn in colnames(raw_ref$meta)) obj@meta.data[common, cn] <- raw_ref$meta[common, cn]
      }
    }
    return(obj)
  }
  stop(errorCondition("Format de reference non reconnu apres lecture (ni objet Seurat, ni liste counts/meta).", class = "spatial_reference_error"))
}

# -----------------------------------------------------------------------------
# 2. Cell-type label sanitization (RCTD/Label Transfer compatibility)
# -----------------------------------------------------------------------------

#' Sanitize cell-type labels for RCTD/Label Transfer compatibility
#'
#' spacexr::check_cell_types() rejects any level containing "/" -- confirmed
#' real-world error: "Reference: levels(cell_types) contains a cell type
#' with name containing prohibited character /". Applied once, shared by
#' both RCTD and Label Transfer, so their output columns stay consistent
#' with each other.
#'
#' @param x Character vector of cell-type labels.
#' @return Character vector, same length, "/" and "\\" replaced with "-".
sanitize_celltype_labels <- function(x) {
  # "/" est rejete par spacexr::check_cell_types() (RCTD). "_" est
  # silencieusement remplace par "-" par la validation interne de Seurat
  # quand Label Transfer construit son Assay de prediction
  # (TransferData(prediction.assay=TRUE)) -- la matrice de poids brute de
  # RCTD ne passe PAS par cette validation, donc sans ce fix, le MEME type
  # cellulaire logique finit sous deux noms DIFFERENTS selon la methode
  # utilisee (ex: "T_CD4+_1" via RCTD vs "T-CD4+-1" via Label Transfer),
  # rendant toute comparaison directe des deux methodes impossible.
  # Sanitiser "_" ici aussi, une seule fois, a la source commune, rend la
  # conversion de Seurat un no-op et garantit des noms de colonnes
  # identiques quelle que soit la methode choisie.
  gsub("[/_\\\\]", "-", x)
}

# -----------------------------------------------------------------------------
# 3. Self-contained on-disk artifact for the daemon (base R + BPCells only)
# -----------------------------------------------------------------------------

#' Prepare a self-contained on-disk artifact for a scRNA-seq reference
#'
#' @param ref_obj Seurat object (already parsed/validated on the main process).
#' @param celltype_col Character, metadata column holding the cell-type label.
#' @param merge_rare_types Logical, merge types with < min_cells_per_type
#'   cells into "Autre". If the merged "Autre" bucket (or any other bucket)
#'   is STILL below min_cells_per_type afterwards, those cells are dropped
#'   entirely (not just relabeled) -- see v2 changelog.
#' @param min_cells_per_type Integer threshold for the merge/drop above.
#' @param max_cells_per_type Integer or NA. If set (and >0, finite), caps
#'   each cell type at this many cells via stratified random subsampling
#'   (RAM/speed safety net for large real-world references). NA/Inf/<=0
#'   disables (default: no cap).
#' @param bpcells_threshold Integer cell-count above which counts are written
#'   as an on-disk BPCells matrix instead of an in-memory dgCMatrix RDS.
#' @return list(path=, n_cells=, n_genes=, backend=, n_dropped_rare=,
#'   celltype_counts=).
prepare_reference_artifact <- function(ref_obj, celltype_col, merge_rare_types = TRUE,
                                        min_cells_per_type = 25L, max_cells_per_type = NA_integer_,
                                        bpcells_threshold = 40000L) {
  if (!inherits(ref_obj, "Seurat")) stop(errorCondition("prepare_reference_artifact() attend un objet Seurat.", class = "spatial_reference_error"))
  if (!celltype_col %in% colnames(ref_obj@meta.data)) {
    stop(errorCondition(sprintf("Colonne '%s' absente des metadonnees de la reference.", celltype_col), class = "spatial_reference_error"))
  }

  cell_types_raw <- as.character(ref_obj@meta.data[[celltype_col]])
  names(cell_types_raw) <- colnames(ref_obj)
  cell_types_raw <- sanitize_celltype_labels(cell_types_raw)
  cell_types_raw[!nzchar(cell_types_raw %||% "")] <- NA_character_

  keep <- !is.na(cell_types_raw)
  if (sum(keep) < 10L) {
    stop(errorCondition("Moins de 10 cellules annotees (non-NA) dans la colonne choisie -- reference inexploitable.", class = "spatial_reference_error"))
  }
  cell_types_raw <- cell_types_raw[keep]

  n_dropped_rare <- 0L
  if (isTRUE(merge_rare_types)) {
    tab  <- table(cell_types_raw)
    rare <- names(tab)[tab < min_cells_per_type]
    if (length(rare) > 0) cell_types_raw[cell_types_raw %in% rare] <- "Autre"

    # Relabeling alone does not guarantee "Autre" itself clears the
    # threshold (e.g. a single 7-cell rare type merged into "Autre" is
    # still a 7-cell "Autre"). Drop whatever remains too small instead of
    # handing RCTD a bucket doomed to fail its own preflight check.
    tab2 <- table(cell_types_raw)
    still_too_small <- names(tab2)[tab2 < min_cells_per_type]
    if (length(still_too_small) > 0) {
      n_dropped_rare <- sum(cell_types_raw %in% still_too_small)
      cell_types_raw <- cell_types_raw[!(cell_types_raw %in% still_too_small)]
    }
  }

  if (length(cell_types_raw) < 10L) {
    stop(errorCondition("Moins de 10 cellules restantes apres filtrage des types trop rares -- reference inexploitable.", class = "spatial_reference_error"))
  }

  if (!is.null(max_cells_per_type) && !is.na(max_cells_per_type) &&
      is.finite(max_cells_per_type) && max_cells_per_type > 0) {
    set.seed(1)
    keep_names <- unlist(lapply(split(names(cell_types_raw), cell_types_raw), function(ids) {
      if (length(ids) > max_cells_per_type) sample(ids, max_cells_per_type) else ids
    }), use.names = FALSE)
    cell_types_raw <- cell_types_raw[keep_names]
  }

  cells_keep <- names(cell_types_raw)
  cell_types <- factor(unname(cell_types_raw))
  names(cell_types) <- cells_keep

  counts <- tryCatch(
    SeuratObject::LayerData(ref_obj, layer = "counts"),
    error = function(e) SeuratObject::GetAssayData(ref_obj, slot = "counts")
  )
  counts <- counts[, cells_keep, drop = FALSE]

  artifact_dir <- file.path(tempdir(),
                             paste0("ts_ref_", format(Sys.time(), "%Y%m%d%H%M%S"), "_",
                                    sample.int(1e6, 1)))
  dir.create(artifact_dir, showWarnings = FALSE, recursive = TRUE)

  use_bpcells <- ncol(counts) > bpcells_threshold && requireNamespace("BPCells", quietly = TRUE)
  if (use_bpcells) {
    bp_dir <- file.path(artifact_dir, "counts_bpcells")
    BPCells::write_matrix_dir(counts, bp_dir, overwrite = TRUE)
    backend     <- "bpcells"
    counts_path <- bp_dir
  } else {
    counts_path <- file.path(artifact_dir, "counts.rds")
    saveRDS(methods::as(counts, "dgCMatrix"), counts_path, compress = TRUE)
    backend <- "dgCMatrix"
  }

  manifest <- list(
    backend        = backend,
    counts_path    = counts_path,
    cell_types     = cell_types,
    genes          = rownames(counts),
    n_cells        = ncol(counts),
    n_genes        = nrow(counts),
    n_dropped_rare = n_dropped_rare,
    created_at     = Sys.time()
  )
  manifest_path <- file.path(artifact_dir, "manifest.rds")
  saveRDS(manifest, manifest_path)

  list(path = manifest_path, n_cells = manifest$n_cells, n_genes = manifest$n_genes,
       backend = backend, n_dropped_rare = n_dropped_rare,
       celltype_counts = table(cell_types))
}

# =============================================================================
# helpers_io.R — Multi-format data loading + gene-ID / metadata mapping
# =============================================================================
# Extracted from global.R (refactor, session post-v1.0 Bulk) — pure I/O and
# identifier-mapping helpers shared across the Import modules
# (mod_import_sc.R, mod_import_bulk.R, mod_import_spatial.R). No Shiny
# reactivity here: every function below is a plain R function operating on
# paths/data.frames, callable and testable outside the app.
#
# Contents:
#   - Spatial      : is_visium_hd_flat_dir(), is_visium_hd_dir(),
#                     get_visium_import_mode(), list_visium_hd_bin_sizes(),
#                     load_spatial_visium(), load_spatial_visium_hd(),
#                     prepare_spatial_object()
#   - Bulk metadata: infer_metadata_from_names(), preview_metadata_split(),
#                    parse_geo_series_matrix()
#   - Gene IDs     : detect_gene_id_type(), detect_organism_from_ids(),
#                    remap_gene_ids_to_symbol()
#
# NOTE: load_single_cell_data() / prepare_seurat_object() (single-cell
# import, used by mod_import_sc.R AND by mod_spatial_deconv.R's reference
# loader) are NOT in this file — they live in helpers_sc.R. Both files must
# be sourced (a) on the main Shiny process (app.R already does this) AND
# (b) inside every mirai daemon that calls them — see
# R/utils_spatial_async.R's `source_files` argument to init_spatial_daemons().
# Historically only utils_spatial_io.R/_multi.R/_niche.R were preloaded into
# daemons, which is why deconvolution (RCTD/Label Transfer, both of which
# call load_single_cell_data()/prepare_seurat_object() from inside the
# daemon to re-read the reference) failed with "could not find function" —
# see mod_spatial_deconv.R's own changelog for the full diagnosis.
#
# =============================================================================
# REWRITE NOTICE (this version): the previous revision of this file had
# is_visium_hd_dir(), list_visium_hd_bin_sizes() and load_spatial_visium_hd()
# EACH DEFINED TWICE — a correct/robust version near the top, silently
# SHADOWED by an older, incomplete copy-pasted lower in the same file (R
# keeps only the LAST definition of a name when a file is source()'d
# top-to-bottom). The active code in production was therefore always the
# inferior duplicate: is_visium_hd_dir() no longer detected the "flat"
# Visium HD layout at all, and load_spatial_visium_hd() had no
# flat-HD handling and no 'arrow' guard. This is why prior targeted fixes
# "worked in the diff" but were never observed in production — they were
# overwritten a few hundred lines later in the very same file. This
# rewrite keeps exactly ONE definition of every function.
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# =============================================================================
# Spatial helpers — robust Visium / Visium HD import
# =============================================================================

#' Detect flat/feature-slice Visium HD exports
#'
#' Some 10x Visium HD datasets are distributed as a single root .h5 file
#' (10x's "feature slice" / imaging export, e.g. "*_spatial.h5") plus a
#' spatial/ folder, without binned_outputs/. Confirmed (community reports,
#' e.g. satijalab/seurat#9649) that this .h5 uses a DIFFERENT internal
#' schema than the spot/bin count matrices Seurat::Load10X_Spatial() reads
#' — it is not a partially-supported edge case, it is simply not the right
#' kind of file. See .hd_flat_unsupported_message() below.
#'
#' @param visium_dir Character path.
#' @return Logical.
is_visium_hd_flat_dir <- function(visium_dir) {
  if (!dir.exists(visium_dir)) return(FALSE)
  h5_files <- list.files(visium_dir, pattern = "\\.h5$", full.names = FALSE, ignore.case = TRUE)
  has_spatial <- dir.exists(file.path(visium_dir, "spatial"))
  if (!has_spatial || length(h5_files) == 0) return(FALSE)
  any(grepl("visium[_-]?hd", h5_files, ignore.case = TRUE)) ||
    any(grepl("_spatial\\.h5$", h5_files, ignore.case = TRUE)) ||
    any(grepl("feature[_-]?slice", h5_files, ignore.case = TRUE))
}

#' Detect whether the selected directory looks like any Visium HD layout
#' (binned OR flat/feature-slice).
#'
#' @param visium_dir Character path.
#' @return Logical.
is_visium_hd_dir <- function(visium_dir) {
  dir.exists(file.path(visium_dir, "binned_outputs")) || is_visium_hd_flat_dir(visium_dir)
}

#' Return the Visium import mode — SINGLE source of truth used by both the
#' UI (mod_import_spatial.R) and the loaders below.
#'
#' @param visium_dir Character path.
#' @return One of "visium", "visium_hd_binned", "visium_hd_flat".
get_visium_import_mode <- function(visium_dir) {
  if (dir.exists(file.path(visium_dir, "binned_outputs"))) return("visium_hd_binned")
  if (is_visium_hd_flat_dir(visium_dir)) return("visium_hd_flat")
  "visium"
}

#' List available Visium HD bin sizes (um) that contain at least one .h5
#'
#' 2um is intentionally never returned here (RAM safety on a 32 Go CPU-only
#' workstation) — see load_spatial_visium_hd()'s own guard.
#'
#' @param visium_dir Character path to the Visium HD root folder.
#' @return Integer vector.
list_visium_hd_bin_sizes <- function(visium_dir) {
  binned_dir <- file.path(visium_dir, "binned_outputs")
  if (!dir.exists(binned_dir)) return(integer(0))
  bin_dirs <- list.dirs(binned_dir, full.names = FALSE, recursive = FALSE)
  bin_dirs <- bin_dirs[grepl("^square_[0-9]+um$", bin_dirs)]
  sizes <- as.integer(sub("^square_0*([0-9]+)um$", "\\1", bin_dirs))
  has_h5 <- vapply(
    bin_dirs,
    function(d) length(list.files(file.path(binned_dir, d), pattern = "\\.h5$", recursive = TRUE)) > 0L,
    logical(1)
  )
  sort(unique(sizes[has_h5 & sizes %in% c(8L, 16L)]))
}

#' Return the first matching QC column in meta.data
#'
#' @param obj Seurat object.
#' @param prefix Prefix to match, e.g. "nCount_" or "nFeature_".
#' @return Character scalar or NA_character_.
get_first_qc_col <- function(obj, prefix) {
  cols <- grep(paste0("^", prefix), colnames(obj@meta.data), value = TRUE)
  if (length(cols) == 0) return(NA_character_)
  cols[1]
}

#' Validate that a loaded object is a Seurat object
#'
#' @param obj Any object returned by a loader.
#' @param context Character, error context for the message.
#' @return The same object if valid.
assert_seurat_object <- function(obj, context = "import spatial") {
  if (is.null(obj) || inherits(obj, "error") || !inherits(obj, "Seurat")) {
    stop(sprintf("Echec %s : l'objet charge n'est pas un objet Seurat valide.", context), call. = FALSE)
  }
  obj
}

#' User-facing message for a flat/feature-slice Visium HD export
#'
#' Single string reused by both loaders below AND mod_import_spatial.R's
#' pre-import banner — one place to edit if the wording ever needs updating.
.hd_flat_unsupported_message <- function() {
  paste0(
    "Ce fichier .h5 utilise un schema interne (\"feature slice\"/imagerie) non pris en charge ",
    "par cette application ni par Seurat::Load10X_Spatial() — ce n'est pas une matrice de comptage ",
    "par spot/bin, meme si le nom du fichier ressemble a un export Visium HD standard. ",
    "Reexportez au format Space Ranger standard : un dossier 'outs/' contenant soit ",
    "'binned_outputs/square_008um|016um/' (Visium HD), soit 'filtered_feature_bc_matrix.h5' + ",
    "'spatial/' (Visium classique)."
  )
}

# ── Arrow-free HD-binned fallback ───────────────────────────────────────────

#' Read a Visium HD tissue_positions file (CSV preferred, Parquet fallback)
#'
#' Space Ranger's own HD output usually ships tissue_positions.parquet under
#' spatial/, which Seurat's own loader reads via `arrow::read_parquet()` —
#' 'arrow' has no CRAN binary for every Windows/R combination and can fail
#' to compile from source without a working Rtools toolchain. Some HD
#' exports also ship a CSV sibling (tissue_positions.csv /
#' tissue_positions_list.csv) — read that first, zero extra dependency. If
#' only the Parquet file exists, try 'arrow' then the much lighter
#' 'nanoparquet' (CRAN, no Arrow C++ toolchain, single small compiled
#' package — generally easier to obtain a Windows binary for).
#'
#' @param spatial_dir Character path to a .../spatial/ folder.
#' @return data.frame, lower-case column names.
read_hd_tissue_positions <- function(spatial_dir) {
  csv_candidates <- list.files(spatial_dir, pattern = "^tissue_positions(_list)?\\.csv$",
                                full.names = TRUE, ignore.case = TRUE)
  if (length(csv_candidates) > 0) {
    df <- utils::read.csv(csv_candidates[1], header = TRUE, stringsAsFactors = FALSE)
    colnames(df) <- tolower(colnames(df))
    return(df)
  }

  parquet_candidates <- list.files(spatial_dir, pattern = "^tissue_positions\\.parquet$",
                                    full.names = TRUE, ignore.case = TRUE)
  if (length(parquet_candidates) == 0) {
    stop("Ni tissue_positions.csv/tissue_positions_list.csv ni tissue_positions.parquet ",
         "trouve dans ", spatial_dir, ".", call. = FALSE)
  }

  if (requireNamespace("arrow", quietly = TRUE)) {
    df <- as.data.frame(arrow::read_parquet(parquet_candidates[1]))
  } else if (requireNamespace("nanoparquet", quietly = TRUE)) {
    df <- nanoparquet::read_parquet(parquet_candidates[1])
  } else {
    stop(
      "Aucun lecteur Parquet disponible (ni 'arrow', ni 'nanoparquet') pour lire ",
      basename(parquet_candidates[1]), ". Installez l'un des deux : ",
      "install.packages('nanoparquet') (leger, pas de dependance C++ Arrow, binaires CRAN ",
      "generalement disponibles sous Windows) ou ",
      "install.packages('arrow', repos = c('https://apache.r-universe.dev', getOption('repos'))) ",
      "si aucun binaire CRAN standard n'existe pour votre version de R.",
      call. = FALSE
    )
  }
  colnames(df) <- tolower(colnames(df))
  df
}

.PNG_SIGNATURE <- as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A))

#' Cheap PNG validity check (magic-byte header only, no image library needed)
.is_valid_png <- function(path) {
  if (!file.exists(path)) return(FALSE)
  con <- tryCatch(file(path, "rb"), error = function(e) NULL)
  if (is.null(con)) return(FALSE)
  on.exit(close(con), add = TRUE)
  header <- tryCatch(readBin(con, "raw", n = 8L), error = function(e) raw(0))
  length(header) == 8L && identical(header, .PNG_SIGNATURE)
}

#' Repair broken Visium HD tissue-image symlinks before Load10X_Spatial()
#' ever touches them
#'
#' Space Ranger writes `binned_outputs/square_0XXum/spatial/tissue_*_image.png`
#' as SYMLINKS back to the canonical `outs/spatial/` copy. These links have
#' been reported broken/unreadable on real exports (satijalab/seurat
#' issues #9533, #9688 — "file is not in PNG format" from inside
#' Load10X_Spatial()'s own image loader). Best-effort, silent on failure:
#' never blocks import, just leaves the underlying error to surface
#' normally if the repair itself can't help.
#'
#' @param bin_spatial_dir Character path to binned_outputs/square_0XXum/spatial/.
#' @param root_spatial_dir Character path to the root outs/spatial/.
#' @return invisible(TRUE) if anything was repaired, invisible(FALSE) otherwise.
repair_hd_symlinked_images <- function(bin_spatial_dir, root_spatial_dir) {
  if (!dir.exists(bin_spatial_dir) || !dir.exists(root_spatial_dir)) return(invisible(FALSE))
  repaired <- FALSE
  for (fname in c("tissue_lowres_image.png", "tissue_hires_image.png")) {
    bin_file  <- file.path(bin_spatial_dir, fname)
    root_file <- file.path(root_spatial_dir, fname)
    if (file.exists(root_file) && !.is_valid_png(bin_file)) {
      ok <- tryCatch({
        if (file.exists(bin_file)) unlink(bin_file)
        file.copy(root_file, bin_file, overwrite = TRUE)
      }, error = function(e) FALSE)
      if (isTRUE(ok)) repaired <- TRUE
    }
  }
  invisible(repaired)
}

#' Degraded Visium HD binned loader — used ONLY when 'arrow' is unavailable
#'
#' Bypasses Seurat::Load10X_Spatial(bin.size=) entirely (it hard-requires
#' 'arrow' for the .parquet coordinates on most real HD exports) and
#' assembles the object by hand from pieces that need no Arrow C++ install:
#'   - counts   : Seurat::Read10X_h5() (hdf5r — already a hard requirement)
#'   - positions: read_hd_tissue_positions() (CSV first, then arrow/
#'                nanoparquet only if a CSV sibling isn't shipped)
#'
#' Trade-off, stated plainly rather than hidden: the resulting object has
#' NO Seurat image slot (Images(obj) is empty) — this app's own histology
#' overlay does not need it (extract_histology_image() reads PNG/JSON
#' straight off disk from `raw_dir`, see R/utils_spatial_io.R and its
#' relaxed guard for `ts_manual_hd_loader`), but any OTHER code calling
#' Seurat::SpatialFeaturePlot() etc. directly on this object will not have
#' a background image. Coordinates are exposed both as standard metadata
#' columns (coord_x/coord_y, pixel-space) for this app's own
#' get_spatial_coords() fallback, AND the object is fully usable for
#' clustering/Moran/deconvolution (none of which read the image slot).
#'
#' @inheritParams load_spatial_visium_hd
#' @return Seurat object, attr(obj, "ts_manual_hd_loader") == TRUE.
load_spatial_visium_hd_manual <- function(visium_dir, bin_size, sample_name,
                                          min_counts, min_features) {
  bin_dir      <- file.path(visium_dir, "binned_outputs", sprintf("square_%03dum", bin_size))
  spatial_dir  <- file.path(bin_dir, "spatial")
  if (!dir.exists(spatial_dir)) {
    stop("Dossier 'spatial' introuvable sous ", bin_dir, ".", call. = FALSE)
  }

  h5_candidates <- list.files(bin_dir, pattern = "^filtered_feature_bc_matrix\\.h5$",
                               full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  if (length(h5_candidates) == 0) {
    h5_candidates <- list.files(bin_dir, pattern = "\\.h5$",
                                 full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  }
  if (length(h5_candidates) == 0) {
    stop("Aucun fichier .h5 de comptage trouve sous ", bin_dir, ".", call. = FALSE)
  }

  counts <- Seurat::Read10X_h5(h5_candidates[1])
  if (is.list(counts)) counts <- counts[[1]]  # multi-modal h5 safety net

  pos <- read_hd_tissue_positions(spatial_dir)
  needed <- c("barcode", "pxl_row_in_fullres", "pxl_col_in_fullres")
  missing_cols <- setdiff(needed, colnames(pos))
  if (length(missing_cols) > 0) {
    stop("Colonnes manquantes dans tissue_positions (", paste(missing_cols, collapse = ", "),
         ") — fichier de positions non standard.", call. = FALSE)
  }
  rownames(pos) <- pos$barcode

  common <- intersect(colnames(counts), pos$barcode)
  if (length(common) < 10) {
    stop("Moins de 10 bins communs entre la matrice de comptage et tissue_positions — ",
         "fichiers incoherents pour ce dossier.", call. = FALSE)
  }
  counts <- counts[, common, drop = FALSE]
  pos    <- pos[common, , drop = FALSE]

  obj <- Seurat::CreateSeuratObject(counts = counts, assay = "Spatial", project = sample_name)
  obj$orig.ident <- sample_name
  # imagecol/imagerow convention (matches get_spatial_coords()'s own naming).
  obj$coord_x <- as.numeric(pos$pxl_col_in_fullres)
  obj$coord_y <- as.numeric(pos$pxl_row_in_fullres)

  if ("in_tissue" %in% colnames(pos)) {
    keep_tissue <- pos$in_tissue %in% c(1, "1", TRUE)
    if (any(keep_tissue)) obj <- subset(obj, cells = colnames(obj)[keep_tissue])
  }

  count_col <- get_first_qc_col(obj, "nCount_")
  feat_col  <- get_first_qc_col(obj, "nFeature_")
  if (!is.na(count_col) && !is.na(feat_col)) {
    keep <- obj@meta.data[[count_col]] >= min_counts & obj@meta.data[[feat_col]] >= min_features
    obj <- subset(obj, cells = colnames(obj)[keep])
  }

  attr(obj, "ts_spatial_mode")     <- "visium_hd_binned"
  attr(obj, "ts_manual_hd_loader") <- TRUE
  obj
}

#' Load a Visium (classic) dataset
#'
#' Linear flow: validate inputs -> resolve import mode (single source of
#' truth: get_visium_import_mode()) -> refuse immediately if flat/unsupported
#' -> delegate to load_spatial_visium_hd() if binned -> otherwise load ->
#' QC filter -> return. One stop() per distinct failure cause.
#'
#' @param visium_dir Character path to the selected directory.
#' @param sample_name Character sample name.
#' @param min_counts Minimum count threshold for spot filtering.
#' @param min_features Minimum feature threshold for spot filtering.
#' @return Seurat object.
load_spatial_visium <- function(visium_dir, sample_name = "Spatial_Sample",
                                min_counts = 100, min_features = 200) {
  visium_dir <- normalizePath(enc2utf8(visium_dir), winslash = "/", mustWork = FALSE)

  if (!dir.exists(visium_dir)) {
    stop("Le dossier specifie n'existe pas : ", visium_dir, call. = FALSE)
  }
  if (!dir.exists(file.path(visium_dir, "spatial"))) {
    stop("Dossier 'spatial' introuvable dans ", visium_dir, call. = FALSE)
  }
  if (!requireNamespace("hdf5r", quietly = TRUE)) {
    stop("Le package 'hdf5r' est requis pour lire les fichiers 10X .h5.", call. = FALSE)
  }

  import_mode <- get_visium_import_mode(visium_dir)

  # Explicit, immediate refusal — never attempt Load10X_Spatial() on a
  # schema it cannot read (previously: silent fallback into a doomed call,
  # surfacing only as a raw "feature_slices/data does not exist" error).
  if (identical(import_mode, "visium_hd_flat")) {
    stop(.hd_flat_unsupported_message(), call. = FALSE)
  }

  # Routed here on a binned HD folder (e.g. a direct/console call bypassing
  # the UI's own dedicated bin-size selector) -- redirect rather than
  # picking an arbitrary .h5 among several bin resolutions.
  if (identical(import_mode, "visium_hd_binned")) {
    return(load_spatial_visium_hd(visium_dir, sample_name = sample_name,
                                   min_counts = min_counts, min_features = min_features))
  }

  h5_files <- list.files(visium_dir, pattern = "\\.h5$", full.names = FALSE, ignore.case = TRUE)
  h5_filename <- if (length(h5_files) == 0) {
    "filtered_feature_bc_matrix.h5"
  } else if (any(grepl("^filtered_feature_bc_matrix", h5_files, ignore.case = TRUE))) {
    h5_files[grepl("^filtered_feature_bc_matrix", h5_files, ignore.case = TRUE)][1]
  } else if (any(grepl("^raw_feature_bc_matrix", h5_files, ignore.case = TRUE))) {
    warning(
      "Seul un fichier 'raw_feature_bc_matrix.h5' a ete trouve (pas de version filtree) — ",
      "les spots hors-tissu ne seront pas exclus automatiquement.",
      call. = FALSE
    )
    h5_files[grepl("^raw_feature_bc_matrix", h5_files, ignore.case = TRUE)][1]
  } else {
    h5_files[1]
  }

  spatial_obj <- tryCatch({
    Seurat::Load10X_Spatial(
      data.dir = visium_dir, filename = h5_filename,
      assay = "Spatial", slice = sample_name, filter.matrix = TRUE
    )
  }, error = function(e) {
    if (dir.exists(file.path(visium_dir, "filtered_feature_bc_matrix"))) {
      tryCatch(
        Seurat::Load10X_Spatial(data.dir = visium_dir, assay = "Spatial",
                                slice = sample_name, filter.matrix = TRUE),
        error = function(e2) {
          stop("Impossible de charger les donnees Visium (fichier tente : '", h5_filename,
               "') : ", conditionMessage(e2), call. = FALSE)
        }
      )
    } else {
      stop("Impossible de charger les donnees Visium (fichier tente : '", h5_filename,
           "') : ", conditionMessage(e), call. = FALSE)
    }
  })

  spatial_obj <- assert_seurat_object(spatial_obj, context = paste("import", import_mode))
  spatial_obj$orig.ident <- sample_name
  attr(spatial_obj, "ts_spatial_mode") <- import_mode

  count_col <- get_first_qc_col(spatial_obj, "nCount_")
  feat_col  <- get_first_qc_col(spatial_obj, "nFeature_")
  if (!is.na(count_col) && !is.na(feat_col)) {
    keep <- spatial_obj@meta.data[[count_col]] >= min_counts &
      spatial_obj@meta.data[[feat_col]] >= min_features
    spatial_obj <- subset(spatial_obj, cells = colnames(spatial_obj)[keep])
  } else {
    warning("Colonnes QC nCount_/nFeature_ introuvables — filtrage QC ignore.", call. = FALSE)
  }

  spatial_obj
}

#' Load the RAW (unfiltered, all barcodes incl. background) Visium matrix
#'
#' Companion to load_spatial_visium() — loads raw_feature_bc_matrix.h5
#' (every barcode under the capture area, including empty/background spots)
#' SEPARATELY from the filtered matrix. Backs the optional "filtered + raw
#' simultane" import (see mod_import_spatial.R, checkbox "Importer aussi la
#' matrice brute"). Primary use case: a future ambient-RNA correction step
#' (DecontX or similar), which needs the background/empty-droplet profile
#' that the filtered matrix alone cannot provide (see long-term backlog,
#' handoff_spatial_bio-mg.md). No QC filtering, no image loading — counts
#' only. Classic (non-HD) Visium only; HD's bin.size-aware loader does not
#' expose an equivalent unfiltered matrix through the same code path.
#'
#' @param visium_dir Character path to the Visium root folder.
#' @return Seurat object (raw counts, "Spatial" assay), or NULL if no
#'   raw_feature_bc_matrix.h5 is found or it fails to read (never fatal —
#'   the primary filtered-only import still proceeds without it).
load_spatial_visium_raw <- function(visium_dir) {
  visium_dir <- normalizePath(enc2utf8(visium_dir), winslash = "/", mustWork = FALSE)
  h5_files <- list.files(visium_dir, pattern = "^raw_feature_bc_matrix\\.h5$",
                        full.names = TRUE, ignore.case = TRUE)
  if (length(h5_files) == 0) return(NULL)
  if (!requireNamespace("hdf5r", quietly = TRUE)) {
    warning("Package 'hdf5r' absent — matrice brute (raw) non chargee.", call. = FALSE)
    return(NULL)
  }
  counts <- tryCatch(Seurat::Read10X_h5(h5_files[1]), error = function(e) {
    warning("Lecture de raw_feature_bc_matrix.h5 echouee : ", conditionMessage(e), call. = FALSE)
    NULL
  })
  if (is.null(counts)) return(NULL)
  if (is.list(counts)) counts <- counts[[1]]  # multi-modal h5 safety net
  tryCatch(
    Seurat::CreateSeuratObject(counts = counts, assay = "Spatial", project = "raw"),
    error = function(e) {
      warning("Construction de l'objet raw echouee : ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
}

#' Load one Visium HD dataset (binned) or refuse a flat/feature-slice export
#'
#' Linear flow: validate inputs -> resolve mode -> refuse flat immediately ->
#' delegate to load_spatial_visium() if not actually HD -> validate bin_size
#' -> repair known-broken tissue-image symlinks (best-effort) -> load
#' (Seurat native if 'arrow' present, degraded manual loader otherwise) ->
#' QC filter (skipped if the manual loader already applied it) -> return.
#'
#' @param visium_dir Character path.
#' @param bin_size Integer, 8 or 16 for binned HD (2um intentionally not
#'   offered — see list_visium_hd_bin_sizes()).
#' @param sample_name Character sample name.
#' @param min_counts Minimum count threshold.
#' @param min_features Minimum feature threshold.
#' @return Seurat object.
load_spatial_visium_hd <- function(visium_dir, bin_size = 8L, sample_name = "Spatial_HD_Sample",
                                   min_counts = 100, min_features = 200) {
  visium_dir <- normalizePath(enc2utf8(visium_dir), winslash = "/", mustWork = FALSE)

  if (!dir.exists(visium_dir)) {
    stop("Le dossier specifie n'existe pas : ", visium_dir, call. = FALSE)
  }
  if (!requireNamespace("hdf5r", quietly = TRUE)) {
    stop("Le package 'hdf5r' est requis pour lire les fichiers 10X .h5.", call. = FALSE)
  }

  import_mode <- get_visium_import_mode(visium_dir)

  if (identical(import_mode, "visium_hd_flat")) {
    stop(.hd_flat_unsupported_message(), call. = FALSE)
  }

  if (!identical(import_mode, "visium_hd_binned")) {
    return(load_spatial_visium(visium_dir, sample_name = sample_name,
                               min_counts = min_counts, min_features = min_features))
  }

  if (!bin_size %in% c(8L, 16L)) {
    stop("bin_size doit etre 8 ou 16 (2um non propose : trop volumineux pour 32 Go CPU-only).",
         call. = FALSE)
  }

  available <- list_visium_hd_bin_sizes(visium_dir)
  if (!bin_size %in% available) {
    stop(sprintf(
      "Bin %dum introuvable sous binned_outputs/ (detectes : %s).",
      bin_size, if (length(available)) paste(available, collapse = ", ") else "aucun"
    ), call. = FALSE)
  }

  bin_dir          <- file.path(visium_dir, "binned_outputs", sprintf("square_%03dum", bin_size))
  bin_spatial_dir  <- file.path(bin_dir, "spatial")
  root_spatial_dir <- file.path(visium_dir, "spatial")
  # Best-effort, silent: see repair_hd_symlinked_images() header. A wrapping
  # tryCatch() guarantees this NEVER blocks the import even if e.g. file
  # permissions prevent the copy.
  tryCatch(repair_hd_symlinked_images(bin_spatial_dir, root_spatial_dir), error = function(e) NULL)

  has_arrow <- requireNamespace("arrow", quietly = TRUE)

  spatial_obj <- if (has_arrow) {
    tryCatch({
      Seurat::Load10X_Spatial(
        data.dir = visium_dir, bin.size = bin_size,
        assay = "Spatial", slice = sample_name, filter.matrix = TRUE
      )
    }, error = function(e) {
      stop("Impossible de charger les donnees Visium HD (bin ", bin_size, "um) : ",
           conditionMessage(e), call. = FALSE)
    })
  } else {
    message(sprintf(
      "[spatial] Package 'arrow' absent -- lecteur HD binned degrade utilise pour le bin %dum ",
      bin_size
    ), "(coordonnees + comptages uniquement ; le fond histologique reste disponible via le ",
    "dossier racine 'spatial/', voir R/utils_spatial_io.R).")
    tryCatch({
      load_spatial_visium_hd_manual(visium_dir = visium_dir, bin_size = bin_size,
                                    sample_name = sample_name,
                                    min_counts = min_counts, min_features = min_features)
    }, error = function(e) {
      stop(
        "Impossible de charger les donnees Visium HD (bin ", bin_size, "um) sans le package ",
        "'arrow' : ", conditionMessage(e), " Installez 'arrow' (install.packages('arrow'), ou ",
        "install.packages('arrow', repos = c('https://apache.r-universe.dev', getOption('repos'))) ",
        "si aucun binaire CRAN n'existe pour votre version de R) ou l'alternative legere ",
        "'nanoparquet' (install.packages('nanoparquet')) pour activer le chargement complet.",
        call. = FALSE
      )
    })
  }

  spatial_obj <- assert_seurat_object(spatial_obj, context = "import Visium HD")
  spatial_obj$orig.ident <- sample_name
  attr(spatial_obj, "ts_spatial_mode") <- "visium_hd_binned"

  # The manual loader already applied min_counts/min_features QC itself
  # (and sets ts_manual_hd_loader) -- do not re-filter an already-filtered
  # object, and do not clobber that attribute.
  if (!isTRUE(attr(spatial_obj, "ts_manual_hd_loader"))) {
    count_col <- get_first_qc_col(spatial_obj, "nCount_")
    feat_col  <- get_first_qc_col(spatial_obj, "nFeature_")
    if (!is.na(count_col) && !is.na(feat_col)) {
      keep <- spatial_obj@meta.data[[count_col]] >= min_counts &
        spatial_obj@meta.data[[feat_col]] >= min_features
      spatial_obj <- subset(spatial_obj, cells = colnames(spatial_obj)[keep])
    } else {
      warning("Colonnes QC nCount_/nFeature_ introuvables — filtrage QC ignore.", call. = FALSE)
    }
  }

  spatial_obj
}

#' Locate a Slide-seq bead-location file among a candidate file list (internal)
#'
#' Tried in priority order (most canonical Slide-seq/Macosko-lab naming
#' first) so that, if several matches exist, the most likely-correct one
#' wins deterministically:
#'   1. "BeadLocationsForR.csv[.gz]" / "BeadLocation.csv[.gz]" (classic
#'      Slide-seq v1/v2 Broad/Macosko naming).
#'   2. "*_alignedXYCoords.csv|tsv[.gz]" (Slide-seq v2 Macosko naming —
#'      frequently HEADERLESS, handled by .read_delimited_table()'s
#'      header-sniffing below).
#'   3. "coords.csv|tsv[.gz]" / "positions.csv|tsv[.gz]" (generic).
#'   4. "*_spatial.csv|tsv[.gz]" (seen on some real exports, e.g. MBASS-style
#'      "<sample>_spatial.csv" — lower priority than the more specific
#'      "coords"/"positions" names since "spatial" is a more generic word).
#'   5. Any filename containing "location"/"bead" as a last resort.
#'
#' BUG FIX (real dataset feedback, PUCK_01 export): patterns are NOT anchored
#' to the start of the filename (no leading `^`) — real-world exports
#' routinely prefix every file with a sample/puck identifier (e.g.
#' "Puck_Num_01_alignedXYCoords.tsv"), which a `^`-anchored pattern would
#' reject even though the file is perfectly usable.
#'
#' @param files Character vector of full file paths (as from list.files(...,
#'   full.names = TRUE)).
#' @return Character scalar (a path from `files`), or NA_character_ if none matched.
.find_slideseq_location_file <- function(files) {
  bn <- basename(files)
  patterns <- c(
    "BeadLocationsForR\\.csv(\\.gz)?$",
    "BeadLocation\\.csv(\\.gz)?$",
    "_alignedXYCoords\\.(csv|tsv)(\\.gz)?$",
    "coords?\\.(csv|tsv)(\\.gz)?$",
    "positions?\\.(csv|tsv)(\\.gz)?$",
    "_spatial\\.(csv|tsv)(\\.gz)?$",
    "bead.*location|location.*bead"
  )
  for (pat in patterns) {
    hit <- files[grepl(pat, bn, ignore.case = TRUE)]
    if (length(hit) > 0) return(hit[1])
  }
  NA_character_
}

#' Locate a Slide-seq counts source among a candidate file list (internal)
#'
#' Prefers the 10x-style sparse triplet (matrix.mtx + barcodes.tsv +
#' features.tsv/genes.tsv — increasingly common for curated/re-processed
#' public Slide-seq datasets, e.g. GEO re-exports) over a dense DGE table
#' (MappedDGEForR.csv, dge_matrix.csv/tsv, or any "*dge*"/"*expression*"
#' .csv|.tsv[.gz] file — the classic Slide-seq pipeline output, genes x
#' beads, can be large but is handled the same way as Bulk's dense tables).
#'
#' BUG FIX (real dataset feedback, PUCK_01 export "Puck_Num_01_expression_matrix.mtx"
#' / "Puck_Num_01_barcodes.tsv" / "Puck_Num_01_genes.tsv"): the previous
#' version anchored each pattern to the START of the filename (`^matrix\\.mtx$`
#' etc.), which rejected every real-world export that prefixes triplet files
#' with a sample/puck name -- the loader fell through to the DGE branch and
#' errored with "Aucune matrice de comptage trouvee" even though a perfectly
#' good triplet was sitting right there. Patterns now only anchor the
#' SUFFIX (`matrix\\.mtx$`, `barcodes\\.tsv$`, ...), matching any prefix.
#'
#' Also now DIRECTORY-AWARE: when a folder contains more than one candidate
#' matrix.mtx (e.g. an uncompressed copy at the top level PLUS a compressed
#' duplicate under a nested "compress/" subfolder — a real layout seen in
#' testing), pairing barcodes/features BLINDLY (just "first match") risked
#' silently combining files from two different directories. This groups
#' candidates by directory, keeps only directories that have all three
#' components together, and prefers the SHALLOWEST one (fewest path
#' separators) — i.e. the top-level copy over an archived/nested duplicate.
#'
#' @param files Character vector of full file paths.
#' @return list(kind = "mtx", mtx=, barcodes=, features=) or
#'   list(kind = "dge", path=). Errors (call.=FALSE) if nothing usable found.
.find_slideseq_counts <- function(files) {
  bn <- basename(files)
  dn <- dirname(files)

  mtx_idx <- grepl("matrix\\.mtx(\\.gz)?$", bn, ignore.case = TRUE)
  bc_idx  <- grepl("barcodes\\.tsv(\\.gz)?$", bn, ignore.case = TRUE)
  ft_idx  <- grepl("(features|genes)\\.tsv(\\.gz)?$", bn, ignore.case = TRUE)

  if (any(mtx_idx)) {
    mtx_dirs <- unique(dn[mtx_idx])
    valid_dirs <- mtx_dirs[vapply(mtx_dirs, function(d) {
      any(mtx_idx & dn == d) && any(bc_idx & dn == d) && any(ft_idx & dn == d)
    }, logical(1))]

    if (length(valid_dirs) > 0) {
      depth <- lengths(gregexpr("[/\\\\]", valid_dirs))
      chosen_dir <- valid_dirs[order(depth)][1]
      pick_in_dir <- function(idx) files[idx & dn == chosen_dir][1]
      return(list(kind = "mtx", mtx = pick_in_dir(mtx_idx),
                  barcodes = pick_in_dir(bc_idx), features = pick_in_dir(ft_idx)))
    }

    # A "*matrix.mtx"-like file exists but no directory has BOTH its
    # barcodes AND features/genes siblings alongside it -- surface a
    # precise error rather than silently falling through to the (almost
    # certainly wrong) DGE branch below.
    stop("Fichier '*matrix.mtx' trouve mais ses fichiers 'barcodes.tsv' et 'features.tsv'/",
         "'genes.tsv' correspondants sont introuvables ENSEMBLE dans le meme dossier.",
         call. = FALSE)
  }

  dge_patterns <- c("^MappedDGEForR\\.(csv|tsv)(\\.gz)?$", "^dge_?matrix\\.(csv|tsv)(\\.gz)?$",
                    "dge.*\\.(csv|tsv)(\\.gz)?$", "expression.*\\.(csv|tsv)(\\.gz)?$")
  for (pat in dge_patterns) {
    hit <- files[grepl(pat, bn, ignore.case = TRUE)]
    if (length(hit) > 0) return(list(kind = "dge", path = hit[1]))
  }

  stop("Aucune matrice de comptage trouvee (ni triplet matrix.mtx/barcodes.tsv/features.tsv|genes.tsv, ",
       "ni fichier DGE dense 'MappedDGEForR.csv', 'dge_matrix.csv/tsv' ou similaire).", call. = FALSE)
}

#' Read a small delimited table with auto-detected separator AND header
#' (internal) — handles the real-world Slide-seq location-file variability:
#' comma vs tab, plain vs .gz, and header vs HEADERLESS (Macosko-lab
#' "*_alignedXYCoords" files are typically 3 headerless columns).
#'
#' @param path Character path (may end in .csv, .tsv, .txt, optionally .gz).
#' @return data.frame. If no header was detected, columns are named V1..Vn.
.read_delimited_table <- function(path) {
  ext_stripped <- sub("\\.gz$", "", path, ignore.case = TRUE)
  sep <- if (grepl("\\.tsv$|\\.txt$", ext_stripped, ignore.case = TRUE)) "\t" else ","

  first_line <- tryCatch({
    con <- if (grepl("\\.gz$", path, ignore.case = TRUE)) gzfile(path, "rt") else file(path, "rt")
    on.exit(close(con), add = TRUE)
    readLines(con, n = 1)
  }, error = function(e) "")

  # Extension-based separator guess can be wrong (some exports use .txt for
  # comma-delimited data) -- if the guessed separator isn't even present on
  # the first line, try the other common one before giving up.
  if (nzchar(first_line) && !grepl(sep, first_line, fixed = TRUE)) {
    sep <- if (identical(sep, "\t")) "," else "\t"
  }

  fields1 <- if (nzchar(first_line)) strsplit(first_line, sep, fixed = TRUE)[[1]] else character(0)
  looks_numeric <- function(v) !is.na(suppressWarnings(as.numeric(v)))
  has_header <- !(length(fields1) >= 3 && (looks_numeric(fields1[2]) || looks_numeric(fields1[3])))

  df <- utils::read.table(path, header = has_header, sep = sep, stringsAsFactors = FALSE,
                          check.names = FALSE, quote = "\"", comment.char = "")
  if (!has_header) colnames(df) <- paste0("V", seq_len(ncol(df)))
  df
}

#' Detect a Slide-seq / Slide-seqV2 dataset directory (BETA)
#'
#' Recognizes either of the two common Slide-seq export layouts (see
#' .find_slideseq_counts()) PLUS the extended set of bead-location filename
#' conventions in .find_slideseq_location_file() (BeadLocationsForR,
#' *_alignedXYCoords, coords/positions, .csv or .tsv, optionally .gz). A
#' bead-location file is REQUIRED in both cases — Slide-seq has no
#' histology image; coordinates are the only spatial information available.
#' Not wired into any auto-detection banner (unlike Visium HD) — kept as an
#' explicit "Slide-seq" radio choice in mod_import_spatial.R to avoid any
#' collision with the already-intricate Visium/HD detection logic.
#'
#' @param dir_path Character path.
#' @return Logical.
is_slideseq_dir <- function(dir_path) {
  if (!dir.exists(dir_path)) return(FALSE)
  files <- list.files(dir_path, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  has_locations <- !is.na(.find_slideseq_location_file(files))
  has_counts <- tryCatch({ .find_slideseq_counts(files); TRUE }, error = function(e) FALSE)
  has_locations && has_counts
}

#' Load a Slide-seq / Slide-seqV2 puck (BETA)
#'
#' No histology image (Slide-seq has none) — produces a Seurat object whose
#' downstream spatial_obj (via convert_to_bpcells_and_fov()) has NULL
#' histology, same code path as the Visium-HD arrow-free manual loader (see
#' extract_histology_image()'s relaxed guard, R/utils_spatial_io.R). Bead
#' coordinates are exposed as coord_x/coord_y meta.data columns, reusing
#' get_spatial_coords()'s EXISTING fallback — zero changes needed anywhere
#' downstream (BANKSY-lite, Moran's I, ROI, multi-sample all read $coords
#' the same way regardless of technology; RCTD/Label Transfer in
#' mod_spatial_deconv.R are ALSO already technology-agnostic — no new code
#' needed there for Slide-seq to work with either).
#'
#' v2 (real-dataset feedback, Slide-seq v2 mouse hippocampus puck): broadened
#' format coverage (see .find_slideseq_location_file()/.find_slideseq_counts())
#' and added a barcode-intersection retry (common "-1"/".1" suffix mismatch
#' between the counts matrix and the location file) plus a diagnostic error
#' message (overlap %, example barcodes from both sides) when beads still
#' don't match after the retry.
#'
#' @param dir_path Character path to the Slide-seq puck folder.
#' @param sample_name Character sample name.
#' @param min_counts,min_features QC thresholds (same convention as Visium).
#' @return Seurat object, attr(obj, "ts_spatial_mode") == "slideseq".
#' 
#' #' Detect the valid feature column in a Slide-seq genes/features TSV file
#'
#' Slide-seq exports may contain only one gene-symbol column, whereas
#' standard 10x feature files usually contain at least two columns.
#'
#' @param features_path Path to genes.tsv/features.tsv, optionally gzipped.
#' @return Integer column index accepted by Seurat::ReadMtx().
.detect_slideseq_feature_column <- function(features_path) {
  if (!file.exists(features_path)) {
    stop(
      "Fichier de genes/features introuvable : ",
      features_path,
      call. = FALSE
    )
  }
  
  con <- if (grepl("\\.gz$", features_path, ignore.case = TRUE)) {
    gzfile(features_path, open = "rt")
  } else {
    file(features_path, open = "rt")
  }
  on.exit(close(con), add = TRUE)
  
  first_line <- readLines(con, n = 1L, warn = FALSE)
  
  if (length(first_line) != 1L || !nzchar(trimws(first_line))) {
    stop(
      "Fichier de genes/features vide ou illisible : ",
      features_path,
      call. = FALSE
    )
  }
  
  n_columns <- length(strsplit(first_line, "\t", fixed = TRUE)[[1L]])
  
  if (n_columns < 1L) {
    stop(
      "Impossible de determiner le nombre de colonnes dans : ",
      features_path,
      call. = FALSE
    )
  }
  
  if (n_columns >= 2L) 2L else 1L
}
#' 
#' 
#' 
#' 
load_spatial_slideseq <- function(dir_path, sample_name = "SlideSeq_Sample",
                                  min_counts = 100, min_features = 200) {
  dir_path <- normalizePath(enc2utf8(dir_path), winslash = "/", mustWork = FALSE)
  if (!dir.exists(dir_path)) stop("Le dossier specifie n'existe pas : ", dir_path, call. = FALSE)

  files <- list.files(dir_path, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) stop("Dossier Slide-seq vide : ", dir_path, call. = FALSE)

  # --- 1. Bead locations (required) ---------------------------------------
  loc_file <- .find_slideseq_location_file(files)
  if (is.na(loc_file)) {
    stop("Fichier de localisation des beads introuvable. Formats reconnus : ",
         "'BeadLocationsForR.csv', 'BeadLocation.csv', '*_alignedXYCoords.csv/.tsv', ",
         "'coords.csv/.tsv', 'positions.csv/.tsv' (.gz accepte pour tous).", call. = FALSE)
  }
  loc <- tryCatch(.read_delimited_table(loc_file), error = function(e) {
    stop("Lecture du fichier de localisation ('", basename(loc_file), "') echouee : ",
         conditionMessage(e), call. = FALSE)
  })
  colnames(loc) <- tolower(colnames(loc))
  bc_col <- intersect(c("barcode", "barcodes", "bead_barcode", "v1"), colnames(loc))[1]
  x_col  <- intersect(c("xcoord", "x", "x_coord", "coord_x", "v2"), colnames(loc))[1]
  y_col  <- intersect(c("ycoord", "y", "y_coord", "coord_y", "v3"), colnames(loc))[1]
  if (is.na(bc_col) && ncol(loc) >= 3) bc_col <- colnames(loc)[1]  # positional fallback
  if (is.na(x_col)  && ncol(loc) >= 3) x_col  <- colnames(loc)[2]
  if (is.na(y_col)  && ncol(loc) >= 3) y_col  <- colnames(loc)[3]
  if (is.na(bc_col) || is.na(x_col) || is.na(y_col)) {
    stop("Colonnes barcode/x/y non reconnues dans '", basename(loc_file), "' — colonnes trouvees : ",
         paste(colnames(loc), collapse = ", "), call. = FALSE)
  }
  loc <- data.frame(barcode = as.character(loc[[bc_col]]),
                    x = as.numeric(loc[[x_col]]), y = as.numeric(loc[[y_col]]),
                    stringsAsFactors = FALSE)
  loc <- loc[!is.na(loc$barcode) & nzchar(loc$barcode), , drop = FALSE]

  # --- 2. Counts: 10x-style triplet preferred, dense DGE table fallback ---
  counts_src <- .find_slideseq_counts(files)
  if (identical(counts_src$kind, "mtx")) {
    feature_column <- .detect_slideseq_feature_column(counts_src$features)
    
    counts <- Seurat::ReadMtx(
      mtx = counts_src$mtx,
      cells = counts_src$barcodes,
      features = counts_src$features,
      feature.column = feature_column
    )
  } else {
    sep <- if (grepl("\\.tsv(\\.gz)?$", counts_src$path, ignore.case = TRUE)) "\t" else ","
    dge <- utils::read.table(counts_src$path, header = TRUE, sep = sep, row.names = 1,
                             check.names = FALSE, stringsAsFactors = FALSE, quote = "\"")
    counts <- methods::as(as.matrix(dge), "dgCMatrix")
  }

  # --- 3. Barcode intersection, with one automatic retry + clear diagnostics
  common <- intersect(colnames(counts), loc$barcode)
  if (length(common) < 10) {
    # Common real-world mismatch: one side carries a 10x-style "-1"/".1"
    # numeric suffix, the other doesn't. Retry once with suffixes stripped
    # from BOTH sides before giving up.
    strip_suffix <- function(x) sub("[-.][0-9]+$", "", x)
    counts_bc_stripped <- strip_suffix(colnames(counts))
    loc_bc_stripped     <- strip_suffix(loc$barcode)
    common_stripped <- intersect(counts_bc_stripped, loc_bc_stripped)
    if (length(common_stripped) >= 10 && length(common_stripped) > length(common)) {
      colnames(counts) <- counts_bc_stripped
      loc$barcode <- loc_bc_stripped
      common <- common_stripped
    }
  }
  if (length(common) < 10) {
    pct <- if (length(unique(loc$barcode)) > 0) {
      round(100 * length(common) / length(unique(loc$barcode)), 1)
    } else 0
    stop(sprintf(
      paste0("Seulement %d bead(s) commun(s) entre la matrice de comptage et le fichier de ",
             "localisation (~%s%% de recouvrement, minimum requis : 10), meme apres tentative ",
             "de normalisation des suffixes ('-1'/'.1'). Exemples barcodes (matrice) : %s. ",
             "Exemples barcodes (localisation) : %s. Verifiez que les deux fichiers ",
             "proviennent bien du meme echantillon."),
      length(common), pct,
      paste(utils::head(colnames(counts), 3), collapse = ", "),
      paste(utils::head(loc$barcode, 3), collapse = ", ")
    ), call. = FALSE)
  }
  counts <- counts[, common, drop = FALSE]
  rownames(loc) <- loc$barcode
  loc <- loc[common, , drop = FALSE]

  obj <- Seurat::CreateSeuratObject(counts = counts, assay = "Spatial", project = sample_name)
  obj$orig.ident <- sample_name
  obj$coord_x <- as.numeric(loc$x)
  obj$coord_y <- as.numeric(loc$y)

  count_col <- get_first_qc_col(obj, "nCount_")
  feat_col  <- get_first_qc_col(obj, "nFeature_")
  if (!is.na(count_col) && !is.na(feat_col)) {
    keep <- obj@meta.data[[count_col]] >= min_counts & obj@meta.data[[feat_col]] >= min_features
    obj <- subset(obj, cells = colnames(obj)[keep])
  }

  attr(obj, "ts_spatial_mode") <- "slideseq"
  obj
}

#' Prepare a spatial Seurat object for downstream use
#'
#' @param obj A Seurat object.
#' @return A Seurat object.
prepare_spatial_object <- function(obj) {
  if (!inherits(obj, "Seurat")) {
    stop("L'objet doit etre un objet Seurat.", call. = FALSE)
  }
  if (!"Spatial" %in% names(obj@assays) && length(obj@images) == 0) {
    warning("L'objet ne contient pas de donnees spatiales detectees.", call. = FALSE)
  }
  if (grepl("^5", packageVersion("Seurat"))) {
    tryCatch({ obj <- Seurat::JoinLayers(obj) }, error = function(e) NULL)
  }
  obj
}

# =============================================================================
# Bulk / GEO metadata helpers (unchanged)
# =============================================================================

#' Infer sample-level metadata by splitting sample names on a delimiter
#'
#' GEO almost never ships clean tabular metadata — but sample names usually
#' encode the experimental design (e.g. "MW1_cornea_mock_1"). This replaces
#' manual/LLM-assisted metadata reconstruction with a deterministic,
#' inspectable split.
#'
#' @param sample_names Character vector of sample/column names from the counts matrix.
#' @param delimiter Regex delimiter to split on (default underscore or hyphen).
#' @param col_names Character vector naming each resulting segment. Length must match
#'   the number of segments produced by the split (after trimming to the minimum
#'   segment count across all samples, to handle slightly irregular naming).
#' @return data.frame, rownames = sample_names, one column per segment.
infer_metadata_from_names <- function(sample_names, delimiter = "[_-]", col_names = NULL) {
  segments <- strsplit(sample_names, delimiter)
  n_seg <- sapply(segments, length)

  if (length(unique(n_seg)) > 1) {
    min_n <- min(n_seg)
    warning(sprintf(
      "Nombre de segments inégal entre échantillons (min=%d, max=%d). Troncature à %d segments — vérifiez le résultat.",
      min_n, max(n_seg), min_n
    ))
    segments <- lapply(segments, function(s) head(s, min_n))
    n_seg <- min_n
  } else {
    n_seg <- n_seg[1]
  }

  mat <- do.call(rbind, segments)
  mat <- as.data.frame(mat, stringsAsFactors = FALSE)

  if (is.null(col_names)) {
    col_names <- paste0("segment_", seq_len(n_seg))
  } else if (length(col_names) != n_seg) {
    stop(errorCondition(sprintf("col_names doit avoir %d éléments (segments détectés), reçu %d.", n_seg, length(col_names)), class = "io_helpers_error"))
  }
  colnames(mat) <- col_names
  rownames(mat) <- sample_names

  # Auto-detect purely numeric segments (e.g. replicate number) and convert
  for (cn in colnames(mat)) {
    if (all(grepl("^[0-9]+$", mat[[cn]]))) mat[[cn]] <- as.integer(mat[[cn]])
  }
  mat
}

#' Preview a metadata-from-names split without committing — feeds the live UI preview
#'
#' @param sample_names Character vector of sample names.
#' @param delimiter Regex delimiter.
#' @param n_preview Number of samples to preview.
#' @return List: segments (list of character vectors), n_seg_consistent (logical), n_seg (integer vector).
preview_metadata_split <- function(sample_names, delimiter = "[_-]", n_preview = 5) {
  segments <- strsplit(head(sample_names, n_preview), delimiter)
  n_seg    <- sapply(segments, length)
  list(segments = segments, n_seg_consistent = length(unique(n_seg)) == 1, n_seg = n_seg)
}

#' Detect the likely gene ID type from a vector of row identifiers
#'
#' @param gene_ids Character vector (rownames of the counts matrix).
#' @return One of "symbol", "ensembl", "entrez", "affy_probe", "unknown".
detect_gene_id_type <- function(gene_ids) {
  sample_ids <- trimws(head(gene_ids[!is.na(gene_ids)], 50))
  if (length(sample_ids) == 0) return("unknown")

  pct_match <- function(pattern) mean(grepl(pattern, sample_ids))

  if (pct_match("^ENSG[0-9]{11}") > 0.7 || pct_match("^ENSMUSG[0-9]{11}") > 0.7) return("ensembl")
  if (pct_match("^[0-9]+$") > 0.7) return("entrez")
  if (pct_match("^[0-9]+(_[a-z]_)?_at$") > 0.5) return("affy_probe")
  if (pct_match("^[A-Za-z0-9.-]+$") > 0.7 && pct_match("^[0-9]+$") < 0.3) return("symbol")
  "unknown"
}

#' Detect species (human/mouse) from Ensembl gene ID prefixes
#'
#' Step-3.8A: used to auto-pick the correct org.*.eg.db package instead of
#' relying on the currently-selected reference/UI default (which defaults to
#' "human" and silently breaks mouse ENSMUSG... datasets).
#'
#' @param gene_ids Character vector (rownames of the counts/expression matrix).
#' @return "human", "mouse", or NA_character_ (ambiguous/unknown).
#'
#' Step-3.8B fix: previously returned the STRING "unknown" for the ambiguous
#' case. Every call site checks `is.na(detected_org)` (mod_sc_annotation.R,
#' mod_sc.R, remap_seurat_ids_to_symbol()) -- is.na("unknown") is FALSE, so
#' "unknown" silently passed every one of those checks as if it were a real
#' detected organism, defeating the whole auto-detect mechanism. Root cause of
#' the real-world mouse dataset defaulting to organism="human".
detect_organism_from_ids <- function(gene_ids) {
  gene_ids <- head(stats::na.omit(as.character(gene_ids)), 200)
  if (length(gene_ids) == 0L) return(NA_character_)

  gene_ids <- trimws(gene_ids)
  pct_mouse <- mean(grepl("^ENSMUSG[0-9]+", gene_ids))
  pct_human <- mean(grepl("^ENSG[0-9]+", gene_ids))

  if (pct_mouse >= 0.50) return("mouse")
  if (pct_human >= 0.50) return("human")
  NA_character_
}

#' Parse a GEO "series_matrix.txt" file into a clean sample x variable metadata table
#'
#' GEO series_matrix.txt files (downloaded directly from a GSE page, no GEOquery/
#' internet access needed) embed per-sample metadata as repeated
#' "!Sample_characteristics_chN" tab-separated lines (one line per characteristic,
#' one quoted "key: value" cell per sample) plus a "!Sample_geo_accession" line
#' giving the GSM IDs in the same column order as the expression/counts matrix.
#' This replaces manual/LLM-assisted extraction (see metadata_GSE52778.csv-style
#' files) with a deterministic local parse — biologist drops the raw file GEO
#' gives them, no curation step required.
#'
#' @param filepath Path to a *_series_matrix.txt file.
#' @return data.frame, one row per sample (rownames = GSM accession), one column
#'   per "!Sample_characteristics_chN" key (+ "title" if present).
parse_geo_series_matrix <- function(filepath) {
  raw_lines <- readLines(filepath, encoding = "UTF-8", warn = FALSE)

  split_quoted_tsv <- function(line) {
    parts <- strsplit(line, "\t")[[1]][-1]            # drop the "!Sample_xxx" tag itself
    trimws(gsub('^"|"$', "", parts))
  }

  geo_line   <- raw_lines[startsWith(raw_lines, "!Sample_geo_accession")]
  title_line <- raw_lines[startsWith(raw_lines, "!Sample_title")]
  char_lines <- raw_lines[grepl("^!Sample_characteristics_ch[0-9]+", raw_lines)]

  if (length(geo_line) == 0) {
    stop(errorCondition(paste0(
      "Pas une ligne '!Sample_geo_accession' trouvee. Ce fichier n'est probablement ",
      "pas un series_matrix.txt GEO valide (ou c'est en fait la matrice de counts — ",
      "elle s'importe via 'Option B/C', pas ici)."
    ), class = "io_helpers_error"))
  }

  geo_acc <- split_quoted_tsv(geo_line[1])
  n_samples <- length(geo_acc)
  if (n_samples == 0) stop(errorCondition("Aucun echantillon (GSM) trouve dans ce series_matrix.txt.", class = "io_helpers_error"))

  meta <- data.frame(row.names = geo_acc)

  if (length(title_line) > 0) {
    meta$title <- split_quoted_tsv(title_line[1])[seq_len(n_samples)]
  }

  used_keys <- character(0)
  for (ln in char_lines) {
    vals <- split_quoted_tsv(ln)[seq_len(n_samples)]
    has_kv <- grepl(":", vals)
    key <- if (any(has_kv)) trimws(sub(":.*$", "", vals[has_kv][1])) else "characteristic"
    vals_clean <- ifelse(has_kv, trimws(sub("^[^:]*:", "", vals)), vals)

    key <- make.unique(c(used_keys, key))[length(used_keys) + 1]
    used_keys <- c(used_keys, key)
    meta[[key]] <- vals_clean
  }

  meta
}

#' Convert counts matrix row identifiers to gene symbols
#'
#' @param counts_matrix Matrix, genes in rows (any supported ID type).
#' @param from_type One of "ensembl", "entrez", "affy_probe" (output of detect_gene_id_type()).
#' @param organism "human" or "mouse".
#' @param collapse_method How to merge counts when multiple original IDs map to the
#'   same symbol: "sum" (recommended for counts) or "max_mean" (keep the ID with the
#'   highest mean expression, discard the rest — useful for probe-level redundancy).
#' @return List: matrix (remapped, deduplicated), n_mapped, n_unmapped, n_collapsed.
remap_gene_ids_to_symbol <- function(counts_matrix, from_type, organism = "human",
                                     collapse_method = "sum") {
  if (!from_type %in% c("ensembl", "entrez", "affy_probe")) {
    stop(errorCondition("from_type doit être 'ensembl', 'entrez' ou 'affy_probe'.", class = "io_helpers_error"))
  }

  orgdb <- if (organism == "human") {
    if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) stop(errorCondition("Package 'org.Hs.eg.db' requis.", class = "io_helpers_error"))
    org.Hs.eg.db::org.Hs.eg.db
  } else {
    if (!requireNamespace("org.Mm.eg.db", quietly = TRUE)) stop(errorCondition("Package 'org.Mm.eg.db' requis.", class = "io_helpers_error"))
    org.Mm.eg.db::org.Mm.eg.db
  }

  from_key <- switch(from_type,
                     ensembl    = "ENSEMBL",
                     entrez     = "ENTREZID",
                     affy_probe = if (organism == "human") "PROBEID" else stop(errorCondition("Mapping de probes Affymetrix non supporté pour la souris dans ce module — fournissez un fichier d'annotation de plateforme dédié.", class = "io_helpers_error"))
  )

  ids_clean <- rownames(counts_matrix)

  if (from_type == "ensembl") {
    ids_clean <- sub("[_.].*", "", ids_clean)   # retire "_TSPAN6", ".1", etc.
  }
  
  # ── Pre-flight sanity check ────────────────────────────────────────────────
  # AnnotationDbi::select() throws a low-level, untranslated error ("None of
  # the keys entered are valid keys for 'ENSEMBL'") when NONE of the supplied
  # identifiers look anything like the chosen from_type — typically because
  # the data isn't gene-level at all (e.g. "eye_count"-style custom row IDs),
  # or the wrong from_type was left selected. Catch this BEFORE the call with
  # an actionable French message instead of the raw Bioconductor text.
  expected_pattern <- switch(from_type,
                             ensembl    = "^ENS(MUS)?G[0-9]{6,}",
                             entrez     = "^[0-9]+$",
                             affy_probe = "^[0-9]+(_[a-z]_)?_at$"
  )
  sample_ids <- head(ids_clean[!is.na(ids_clean)], 200)
  pct_match  <- if (length(sample_ids) > 0) mean(grepl(expected_pattern, sample_ids)) else 0

  if (pct_match < 0.05) {
    stop(sprintf(
      paste0(
        "Vos identifiants ne ressemblent pas à des ID '%s' (seulement %.0f%% correspondent au format attendu, ex: %s). ",
        "Causes probables : (1) vos données n'utilisent pas d'identifiants de gènes standards — dans ce cas, ",
        "ignorez cette étape facultative ; ou (2) le type source sélectionné ne correspond pas à vos données. ",
        "Exemple d'identifiant trouvé dans vos données : '%s'."
      ),
      from_type, pct_match * 100, expected_pattern,
      if (length(sample_ids) > 0) sample_ids[1] else "?"
    ), call. = FALSE)
  }

  map_df <- tryCatch({
    AnnotationDbi::select(orgdb, keys = ids_clean, keytype = from_key, columns = "SYMBOL")
  }, error = function(e) {
    if (grepl("not valid keys|None of the keys", conditionMessage(e), ignore.case = TRUE)) {
      stop(sprintf(
        paste0(
          "Aucun de vos identifiants n'est reconnu comme '%s' chez l'organisme '%s'. ",
          "Causes probables : (1) l'organisme sélectionné ne correspond pas à vos données — essayez l'autre option ; ",
          "ou (2) vos données n'utilisent pas d'identifiants de gènes standards, auquel cas ignorez cette étape facultative."
        ),
        from_type, organism
      ), call. = FALSE)
    }
    stop("Échec du mapping d'identifiants : ", conditionMessage(e),
         ". Vérifiez que l'organisme sélectionné correspond bien à vos données.", call. = FALSE)
  })

  map_df <- map_df[!is.na(map_df$SYMBOL), ]
  map_df <- map_df[!duplicated(map_df[[from_key]]), ]  # one symbol per original ID
  id_to_symbol <- setNames(map_df$SYMBOL, map_df[[from_key]])

  mapped_symbols <- id_to_symbol[ids_clean]
  n_unmapped <- sum(is.na(mapped_symbols))
  n_mapped   <- sum(!is.na(mapped_symbols))

  keep <- !is.na(mapped_symbols)
  mat  <- counts_matrix[keep, , drop = FALSE]
  syms <- mapped_symbols[keep]

  n_before_collapse <- nrow(mat)
  if (collapse_method == "sum") {
    # rowsum() is vastly faster than aggregate() on large matrices (no
    # data.frame round-trip) — same fix pattern as smart_read()'s duplicate
    # GeneID collapse in mod_import_bulk.R.
    mat_num <- mat
    suppressWarnings(mode(mat_num) <- "numeric")
    mat <- rowsum(mat_num, group = syms, reorder = FALSE, na.rm = TRUE)
  } else {
    # max_mean: keep highest-expressed probe/ID per symbol, discard the rest
    mean_expr <- rowMeans(mat)
    ord <- order(syms, -mean_expr)
    mat <- mat[ord, , drop = FALSE]
    syms_ord <- syms[ord]
    keep_first <- !duplicated(syms_ord)
    mat <- mat[keep_first, , drop = FALSE]
    rownames(mat) <- syms_ord[keep_first]
  }

  list(
    matrix      = mat,
    n_mapped    = n_mapped,
    n_unmapped  = n_unmapped,
    n_collapsed = n_before_collapse - nrow(mat)
  )
}

# =============================================================================
# Identite d'objet pour les contrats de resultat (Stages 8+, TREE CERBERUS)
# =============================================================================
# Bloc object_identity PARTAGE par tous les finalisateurs de resultats
# canoniques (velocity, communication, DA design...) : l'empreinte est celle
# de velocity_object_fingerprint() (v2, deterministe, sans adresse memoire),
# REUTILISEE jamais dupliquee. R/sc/sc_velocity.R doit etre source avant
# l'appel (ordre garanti par app.R).
#
#' Bloc object_identity partage des resultats canoniques (empreinte v2)
#'
#' @param obj Objet quelconque a dimnames (NULL accepte : empreinte NULL).
#' @return list(fingerprint, method, seurat_dims) — contrat commun des
#'   resultats canoniques (velocity, communication, DA design).
#' @export
build_object_identity_v2 <- function(obj) {
  if (is.null(obj)) {
    return(list(
      fingerprint = NULL,
      method = paste0(
        "v2 (velocity_object_fingerprint reutilise) : dimensions + 20 ",
        "premiers/derniers barcodes de cellules et identifiants de genes ",
        "(deterministe, sans adresse memoire)"
      ),
      seurat_dims = c(seurat_genes = NA_integer_, seurat_cells = NA_integer_)
    ))
  }
  if (!exists("velocity_object_fingerprint", mode = "function")) {
    stop(
      paste0(
        "build_object_identity_v2() : R/sc/sc_velocity.R doit etre source ",
        "avant usage (velocity_object_fingerprint est reutilise, jamais ",
        "duplique)."
      ),
      call. = FALSE
    )
  }
  list(
    fingerprint = velocity_object_fingerprint(obj),
    method = paste0(
      "v2 (velocity_object_fingerprint reutilise) : dimensions + 20 ",
      "premiers/derniers barcodes de cellules et identifiants de genes ",
      "(deterministe, sans adresse memoire)"
    ),
    seurat_dims = c(seurat_genes = as.integer(nrow(obj)),
                    seurat_cells = as.integer(ncol(obj)))
  )
}

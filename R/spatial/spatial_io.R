# =============================================================================
# R/utils_spatial_io.R — BPCells conversion + FOV standardization
# =============================================================================
# Pure functions (no Shiny reactivity). Called once by
# modules/import/mod_import_spatial.R right after a raw object is loaded
# (helpers_io.R::load_spatial_visium() / Seurat::LoadXenium() /
# Seurat::LoadNanostring()). Produces the on-disk BPCells matrix + the
# lightweight list that gets stored in global_data$spatial_obj.
#
# v6.1 (Chantier 1 refonte — HD import robustness, paired with the
# helpers_io.R rewrite): two small, targeted additions, both defensive/
# additive (zero behavior change for any dataset that already worked):
#   1. get_spatial_coords() now falls back to obj$coord_x / obj$coord_y
#      meta.data columns when Seurat::GetTissueCoordinates() errors or
#      returns NULL. This is the coordinate contract produced by
#      helpers_io.R::load_spatial_visium_hd_manual() — the arrow-free
#      degraded Visium HD binned loader, used only when the 'arrow' package
#      is unavailable and a native Seurat HD object (with a proper image/
#      coordinate slot) could not be built.
#   2. extract_histology_image()'s early-return used to require
#      Seurat::Images(seurat_obj) to be non-empty. An object built by
#      load_spatial_visium_hd_manual() has NO registered image at all (by
#      design — see that function's header), which used to make histology
#      silently unavailable for that path even though every actual pixel
#      read in this function already goes through `raw_dir` directly (see
#      v5/v6 history below), never through the object's own image slot.
#      The guard now also accepts a usable `raw_dir` as an alternative to a
#      registered image name.
#
# v5 (audit step 3.9c — histology rotation / background selection):
#
#   1. FIX (background selection): get_visium_spatial_dir() tried to locate
#      the Visium spatial/ folder from the VisiumV1 object's own @image /
#      @misc slots. Root cause of "impossible de selectionner tous les
#      fonds disponibles" -- a standard VisiumV1 object (Load10X_Spatial())
#      stores the RASTER ARRAY itself in @image (not a character path), and
#      has NO @misc slot at all. get_visium_spatial_dir() therefore always
#      returned NULL, the directory scan in extract_histology_image() never
#      ran, and only the 2 Seurat-native rasters (lowres/hires) ever made it
#      into the dropdown -- explaining "toujours les memes 2 fonds, memes
#      dimensions". Fix: extract_histology_image() now accepts an explicit
#      `raw_dir` (the folder the user actually picked in the import UI,
#      already available there) and uses it FIRST; the old slot-sniffing
#      stays as a best-effort fallback for callers that don't pass raw_dir.
#      Also de-duplicates: a scanned "tissue_lowres_image.*"/"tissue_hires_image.*"
#      is now skipped when the Seurat-native lowres/hires entry already
#      covers it (previously created a confusing "lowres_file" duplicate
#      with identical dimensions right next to "lowres").
#
#   2. FIX (90-degree rotation): GetTissueCoordinates.VisiumV1()'s column
#      ORDER convention (cols=c('imagerow','imagecol') vs
#      c('imagecol','imagerow')) has demonstrably changed between Seurat
#      releases (verified against both an older CRAN doc snapshot and the
#      current satijalab.org reference -- they disagree). get_spatial_coords()
#      already resolves columns BY NAME to be as version-robust as possible,
#      but a wrong result for a given installed version can still happen and
#      is not reliably guessable from static code alone. Rather than ship a
#      3rd unverified automatic heuristic, this adds an explicit, one-click,
#      visually-verifiable MANUAL correction (apply_coord_orientation() ---
#      swap_xy / flip_x / flip_y, wired up from 3 checkboxes in
#      mod_import_spatial.R) applied ONCE at import time to $coords. Because
#      every downstream consumer (histology overlay, BANKSY-lite's physical
#      k-NN graph, Moran's I, ROI, multi-sample maps) reads the SAME $coords,
#      a wrong orientation is not just cosmetic -- fixing it once here fixes
#      it everywhere, and getting it right matters for BANKSY's neighbor
#      graph, not just the picture.
#
# v4 (multi-resolution histology patch): unified extract_histology_image(),
# get_histology_raster(), raster_to_rgba_array() with no transposition/flip
# inside the raster converter — all geometry alignment is delegated to the
# rendering layer (ggplot2::annotation_raster() and Plotly images layout).
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Root cache directory for all persisted BPCells matrices
#'
#' Uses tools::R_user_dir() rather than tempdir() so imported datasets
#' survive across R sessions / app restarts. 
#' @return Character path, created if missing.
bpcells_cache_root <- function() {
  root <- tools::R_user_dir("TranscriptoShiny", which = "cache")
  dir.create(root, showWarnings = FALSE, recursive = TRUE)
  root
}

# -----------------------------------------------------------------------------
# Histology extraction (multi‑resolution) — Visium only
# -----------------------------------------------------------------------------

#' Extract Visium histology backgrounds at low and high resolution
#' and scan the spatial directory for additional image files.
#'
#' @param seurat_obj A Visium Seurat object.
#' @param technology Technology name.
#' @param raw_dir Optional character path to the ROOT folder the user picked
#'   in the import UI (the "outs" folder, or directly its "spatial"
#'   subfolder). Passed explicitly by the caller (mod_import_spatial.R
#'   already has this path from shinyFiles) because a standard VisiumV1
#'   object does NOT reliably carry it anywhere internally accessible (see
#'   file header, v5 fix #1) -- this is the PRIMARY way spatial_dir gets
#'   resolved now; the old slot-sniffing (get_visium_spatial_dir()) is kept
#'   only as a best-effort fallback for callers that omit raw_dir.
#' @return NULL or a multi-resolution histology list with an $images slot.
extract_histology_image <- function(seurat_obj, technology, raw_dir = NULL) {
  if (!identical(technology, "visium")) {
    return(NULL)
  }
  
  img_name <- tryCatch(
    Seurat::Images(seurat_obj)[1L],
    error = function(e) NA_character_
  )
  
  has_registered_image <- !is.na(img_name) && nzchar(img_name)
  has_raw_dir <- !is.null(raw_dir) && nzchar(raw_dir) && dir.exists(raw_dir)
  
  # v6.1: an object built by helpers_io.R::load_spatial_visium_hd_manual()
  # (arrow-free degraded HD loader) has NO registered image at all, by
  # design. Every pixel this function actually reads comes from `raw_dir`
  # directly (see the disk-first v6 rewrite below) -- a missing image slot
  # is only a real dead end when raw_dir is ALSO unusable.
  if (!has_registered_image && !has_raw_dir) {
    return(NULL)
  }
  
  read_histology_file <- function(path) {
    ext <- tolower(tools::file_ext(path))
    
    tryCatch({
      if (ext %in% c("png", "jpg", "jpeg")) {
        if (!requireNamespace("png", quietly = TRUE) && ext == "png") {
          stop(errorCondition("Package 'png' requis pour lire les fichiers PNG.", class = "spatial_io_error"))
        }
        
        if (!requireNamespace("jpeg", quietly = TRUE) && ext %in% c("jpg", "jpeg")) {
          stop(errorCondition("Package 'jpeg' requis pour lire les fichiers JPEG.", class = "spatial_io_error"))
        }
        
        if (ext == "png") {
          return(png::readPNG(path))
        }
        
        return(jpeg::readJPEG(path))
      }
      
      if (ext %in% c("tif", "tiff", "jp2", "j2k", "jpf")) {
        if (!requireNamespace("magick", quietly = TRUE)) {
          stop(errorCondition(paste0("Package 'magick' requis pour lire les images TIFF/JPEG2000/JPF. ", "Installez-le avec install.packages('magick')."), class = "spatial_io_error"))
        }
        
        img <- magick::image_read(path)
        img <- magick::image_convert(img, format = "png")
        img_raw <- magick::image_write(img, format = "png")
        return(png::readPNG(img_raw))
      }
      
      NULL
    }, error = function(e) {
      warning(
        sprintf(
          "Lecture impossible du fond histologique '%s' : %s",
          basename(path),
          conditionMessage(e)
        ),
        call. = FALSE
      )
      NULL
    })
  }
  
  # FIX (v5, best-effort fallback only -- see file header): a standard
  # VisiumV1 object generally does NOT expose a usable path via @image
  # (that slot holds the raster array itself) or @misc (slot doesn't
  # exist), so this almost always returns NULL in practice. Kept only for
  # callers/objects that happen to carry this info; extract_histology_image()
  # prefers the caller-supplied `raw_dir` instead (see below).
  get_visium_spatial_dir <- function(obj, image_obj) {
    candidates <- character(0)
    
    image_path <- tryCatch(
      methods::slot(image_obj, "image"),
      error = function(e) NULL
    )
    
    if (is.character(image_path) && length(image_path) == 1L) {
      candidates <- c(
        candidates,
        dirname(image_path),
        file.path(dirname(image_path), "spatial")
      )
    }
    
    image_dir <- tryCatch(
      methods::slot(image_obj, "misc")$spatial_dir,
      error = function(e) NULL
    )
    
    if (is.character(image_dir) && length(image_dir) == 1L) {
      candidates <- c(candidates, image_dir)
    }
    
    candidates <- unique(normalizePath(
      candidates[file.exists(candidates)],
      winslash = "/",
      mustWork = FALSE
    ))
    
    if (length(candidates) == 0L) {
      return(NULL)
    }
    
    spatial_dirs <- candidates[basename(candidates) == "spatial"]
    
    if (length(spatial_dirs) > 0L) {
      return(spatial_dirs[1L])
    }
    
    candidates[1L]
  }
  
  tryCatch({
    img_obj <- if (has_registered_image) seurat_obj[[img_name]] else NULL
    
    # FIX (v5): prefer the caller-supplied raw_dir (the folder the user
    # actually picked at import) over slot-sniffing, which structurally
    # cannot work for a standard VisiumV1 object (see header).
    spatial_dir <- NULL
    if (!is.null(raw_dir) && nzchar(raw_dir)) {
      candidate_spatial <- file.path(raw_dir, "spatial")
      if (dir.exists(candidate_spatial)) {
        spatial_dir <- candidate_spatial
      } else if (dir.exists(raw_dir) &&
                 length(list.files(raw_dir, pattern = "\\.(png|jpg|jpeg|tif|tiff|jp2|j2k|jpf)$",
                                    ignore.case = TRUE)) > 0L) {
        # user pointed shinyFiles directly at the spatial/ folder itself
        spatial_dir <- raw_dir
      }
    }
    if (is.null(spatial_dir) && has_registered_image) {
      spatial_dir <- get_visium_spatial_dir(seurat_obj, img_obj)
    }
    
    # v6 (audit step 3.12 -- background resolution/alignment): user testing
    # found that ONLY tissue_hires_image.png aligned correctly with the
    # spots; tissue_lowres_image.png (and anything else) appeared undersized
    # / offset. Root cause: Seurat::GetImage()/Seurat::ScaleFactors() go
    # through the object's own internal raster storage, which does not
    # reliably match the ORIGINAL file on disk (dimensions/scale) for every
    # Seurat version -- same family of version-dependent quirk already found
    # for GetTissueCoordinates() (see get_spatial_coords()). Reading
    # scalefactors_json.json + the PNG files DIRECTLY from disk sidesteps
    # this entirely, and is what actually worked in testing. This is now the
    # PRIMARY path whenever spatial_dir is known; Seurat-native extraction
    # (GetImage()/ScaleFactors()) is kept only as a fallback for datasets/
    # callers without a resolvable raw_dir (and requires a registered image
    # to even attempt).
    json_scale_factors <- NULL
    if (!is.null(spatial_dir)) {
      json_path <- file.path(spatial_dir, "scalefactors_json.json")
      if (file.exists(json_path)) {
        json_scale_factors <- tryCatch({
          if (requireNamespace("jsonlite", quietly = TRUE)) {
            jsonlite::fromJSON(json_path)
          } else {
            warning("Package 'jsonlite' absent : lecture de scalefactors_json.json ignoree ",
                    "(repli sur Seurat::ScaleFactors(), potentiellement moins fiable). ",
                    "Installez-le via install.packages('jsonlite').", call. = FALSE)
            NULL
          }
        }, error = function(e) NULL)
      }
    }
    
    read_disk_image <- function(name_pattern, scale_factor) {
      if (is.null(spatial_dir)) return(NULL)
      f <- list.files(spatial_dir, pattern = name_pattern, full.names = TRUE, ignore.case = TRUE)
      if (length(f) == 0L) return(NULL)
      raster_file <- read_histology_file(f[1L])
      if (is.null(raster_file)) return(NULL)
      list(
        raster = raster_file,
        scale_factor = scale_factor %||% 1,
        dim = dim(raster_file),
        source = normalizePath(f[1L], winslash = "/", mustWork = FALSE)
      )
    }
    
    images <- list()
    if (!is.null(json_scale_factors)) {
      hi <- read_disk_image("^tissue_hires_image\\.", json_scale_factors$tissue_hires_scalef)
      lo <- read_disk_image("^tissue_lowres_image\\.", json_scale_factors$tissue_lowres_scalef)
      if (!is.null(hi)) images$hires  <- hi
      if (!is.null(lo)) images$lowres <- lo
    }
    
    # FALLBACK: Seurat-native extraction, only for whichever of lowres/hires
    # wasn't already populated from disk above (no raw_dir, JSON missing, or
    # non-standard folder layout) -- and only possible at all when the
    # object actually has a registered image to query.
    scale_factors <- if (has_registered_image) {
      tryCatch(Seurat::ScaleFactors(img_obj), error = function(e) NULL)
    } else NULL
    if (is.null(images$lowres) && has_registered_image) {
      raster_lowres <- tryCatch(Seurat::GetImage(img_obj, mode = "raster"), error = function(e) NULL)
      if (!is.null(raster_lowres)) {
        images$lowres <- list(
          raster = raster_lowres, scale_factor = scale_factors$lowres %||% 1,
          dim = dim(raster_lowres),
          source = "Seurat::GetImage(mode='raster') [repli -- source disque preferee si disponible]"
        )
      }
    }
    if (is.null(images$hires) && has_registered_image) {
      raster_hires <- tryCatch({
        if ("image" %in% methods::slotNames(img_obj)) {
          image_slot <- methods::slot(img_obj, "image")
          if (is.list(image_slot) && !is.null(image_slot$hires)) image_slot$hires
          else Seurat::GetImage(img_obj, mode = "hires")
        } else {
          Seurat::GetImage(img_obj, mode = "hires")
        }
      }, error = function(e) NULL)
      if (!is.null(raster_hires)) {
        images$hires <- list(
          raster = raster_hires, scale_factor = scale_factors$hires %||% NA_real_,
          dim = dim(raster_hires),
          source = "Seurat::GetImage(mode='hires') [repli]"
        )
      }
    }
    
    if (length(images) == 0L) {
      warning(
        "Aucune image histologique exploitable n'a ete extraite (ni depuis le dossier, ni depuis l'objet Seurat).",
        call. = FALSE
      )
      return(NULL)
    }
    if (is.null(images$lowres) && !is.null(images$hires)) {
      images$lowres <- images$hires  # legacy consumers (this app) expect $lowres to always exist
    }
    
    # --- Fichiers additionnels dans spatial/ (fiducials, detected_tissue,
    # custom) -- lowres/hires standards deja couverts ci-dessus (disque ou
    # repli Seurat), jamais dupliques ici.
    if (!is.null(spatial_dir) && dir.exists(spatial_dir)) {
      image_files <- list.files(
        spatial_dir,
        pattern = "\\.(png|jpg|jpeg|tif|tiff|jp2|j2k|jpf)$",
        ignore.case = TRUE,
        full.names = TRUE
      )
      
      for (path in image_files) {
        file_name <- basename(path)
        file_stem <- tools::file_path_sans_ext(file_name)
        
        is_lowres <- grepl("^tissue_lowres_image", file_stem, ignore.case = TRUE)
        is_hires  <- grepl("^tissue_hires_image", file_stem, ignore.case = TRUE)
        if (is_lowres || is_hires) next
        
        key <- paste0("file_", gsub("[^A-Za-z0-9_]+", "_", tolower(file_stem)))
        if (key %in% names(images)) {
          key <- paste0(key, "_", sprintf("%02d", sum(startsWith(names(images), key)) + 1L))
        }
        
        raster_file <- read_histology_file(path)
        if (is.null(raster_file)) next
        
        images[[key]] <- list(
          raster = raster_file,
          scale_factor = 1,  # repere full-resolution par defaut pour les fichiers non-standards
          dim = dim(raster_file),
          source = normalizePath(path, winslash = "/", mustWork = FALSE)
        )
      }
    }
    
    list(
      images = images,
      lowres = images$lowres,
      hires = images$hires %||% NULL,
      scale_factors = json_scale_factors %||% scale_factors,
      # v6: hires devient la resolution PAR DEFAUT (voir diagnostic ci-dessus)
      # -- lowres reste selectionnable manuellement pour un apercu rapide.
      active_resolution = if ("hires" %in% names(images)) "hires" else "lowres",
      spatial_dir = spatial_dir
    )
  }, error = function(e) {
    warning(
      "Extraction histologique echouee : ",
      conditionMessage(e),
      call. = FALSE
    )
    NULL
  })
}

#' Convert a histology raster into a PNG-compatible RGBA array
#'
#' No transpose and no flip are applied here. Spatial placement and axis
#' orientation are handled only by the rendering layer.
#'
#' @param r Character raster matrix, SpatRaster, Raster* object, or RGB array.
#' @return Numeric RGBA array with dimensions height x width x 4, values in [0, 1].
raster_to_rgba_array <- function(r) {
  if (is.null(r)) return(NULL)
  
  if (is.matrix(r) && is.character(r)) {
    hex <- as.vector(r)
    rgb <- grDevices::col2rgb(hex, alpha = FALSE)
    
    out <- array(1, dim = c(nrow(r), ncol(r), 4L))
    out[, , 1L] <- matrix(rgb[1L, ] / 255, nrow = nrow(r), ncol = ncol(r))
    out[, , 2L] <- matrix(rgb[2L, ] / 255, nrow = nrow(r), ncol = ncol(r))
    out[, , 3L] <- matrix(rgb[3L, ] / 255, nrow = nrow(r), ncol = ncol(r))
    
    return(out)
  }
  
  if (inherits(r, "SpatRaster")) {
    arr <- terra::as.array(r)
  } else if (inherits(r, c("RasterBrick", "RasterStack", "RasterLayer"))) {
    arr <- raster::as.array(r)
  } else if (is.array(r) && length(dim(r)) == 3L) {
    arr <- r
  } else {
    stop(errorCondition(paste0("Format raster non reconnu : ", class(r)[1L]), class = "spatial_io_error"))
  }
  
  if (length(dim(arr)) == 2L) {
    arr <- array(arr, dim = c(nrow(arr), ncol(arr), 1L))
  }
  
  if (max(arr, na.rm = TRUE) > 1) {
    arr <- arr / 255
  }
  
  out <- array(1, dim = c(dim(arr)[1L], dim(arr)[2L], 4L))
  n_channels <- min(3L, dim(arr)[3L])
  out[, , seq_len(n_channels)] <- arr[, , seq_len(n_channels), drop = FALSE]
  
  out
}

#' Diagnostic de la qualité et des valeurs du raster d'histologie
diagnose_histology_raster <- function(r) {
  if (is.null(r)) return(NULL)
  arr <- raster_to_rgba_array(r)
  if (is.null(arr)) return(NULL)
  
  rgb_vals <- arr[,, 1:3]
  near_white <- (rgb_vals[,,1] > 0.95 & rgb_vals[,,2] > 0.95 & rgb_vals[,,3] > 0.95)
  pct_white <- mean(near_white) * 100
  
  unique_cols <- if (is.matrix(r) && is.character(r)) length(unique(as.vector(r))) else NA
  
  list(
    pct_near_white = round(pct_white, 2),
    n_unique_colors = unique_cols,
    mean_rgb = round(c(mean(rgb_vals[,,1]), mean(rgb_vals[,,2]), mean(rgb_vals[,,3])), 3)
  )
}

#' Get one histology image and its spatial scale
#'
#' @param hist_data Histology object returned by extract_histology_image().
#' @param resolution Requested resolution: "hires" (default, confirmed
#'   reliable) or "lowres" (quick preview only -- see
#'   extract_histology_image() header, audit step 3.12, for why lowres is no
#'   longer the default) or any key from hist_data$images.
#' @return NULL or list(raw_raster, rgba, scale_factor, dim, resolution).
get_histology_raster <- function(hist_data, resolution = "hires") {
  if (is.null(hist_data)) {
    return(NULL)
  }
  
  available_images <- if (
    !is.null(hist_data$images) &&
    is.list(hist_data$images)
  ) {
    hist_data$images
  } else {
    candidates <- names(hist_data)[
      vapply(
        hist_data,
        function(x) is.list(x) && !is.null(x$raster),
        logical(1)
      )
    ]
    
    stats::setNames(
      lapply(candidates, function(name) hist_data[[name]]),
      candidates
    )
  }
  
  if (length(available_images) == 0L) {
    return(NULL)
  }
  
  if (!resolution %in% names(available_images)) {
    resolution <- if ("hires" %in% names(available_images)) {
      "hires"
    } else if ("lowres" %in% names(available_images)) {
      "lowres"
    } else {
      names(available_images)[1L]
    }
  }
  
  target <- available_images[[resolution]]
  
  if (is.null(target) || is.null(target$raster)) {
    return(NULL)
  }
  
  list(
    raw_raster = target$raster,
    rgba = raster_to_rgba_array(target$raster),
    scale_factor = target$scale_factor %||% 1,
    dim = target$dim %||% dim(target$raster),
    resolution = resolution,
    source = target$source %||% NA_character_
  )
}

# -----------------------------------------------------------------------------
# Spatial coordinates (full resolution)
# -----------------------------------------------------------------------------

#' Extraction sécurisée des coordonnées spatiales Visium (Seurat v4 et v5)
#'
#' FIX (root cause of the reported 90° rotation): `Seurat::GetTissueCoordinates()`
#' column ORDER/NAMING for VisiumV1 has genuinely differed between Seurat
#' versions. This function reads columns by NAME instead of position.
#'
#' NOTE (audit step 3.9c): this name-based resolution is the most robust
#' static guess we can make, but Seurat's own column-order convention for
#' GetTissueCoordinates.VisiumV1() is CONFIRMED to have changed between
#' releases (cols=c('imagerow','imagecol') in some, c('imagecol','imagerow')
#' in others -- verified against two disagreeing official doc snapshots). If
#' it still comes out wrong for a given installed version, use
#' apply_coord_orientation() below (wired to 3 checkboxes in
#' mod_import_spatial.R) rather than editing this function's guess again --
#' see that function's header for why.
#'
#' v6.1: added a fallback for objects with NO Seurat image/coordinate slot
#' at all -- helpers_io.R::load_spatial_visium_hd_manual() (the arrow-free
#' degraded Visium HD binned loader) exposes coordinates as plain
#' obj$coord_x / obj$coord_y meta.data columns instead, since it never
#' builds a Seurat image object (see that function's header for why). Used
#' ONLY when GetTissueCoordinates() genuinely has nothing to offer -- zero
#' behavior change for every dataset that already had a working image slot.
#'
#' @param spatial_obj Objet Seurat spatial (Visium/Xenium/CosMx).
#' @return data.frame avec id, x (imagecol / horizontal), y (imagerow /
#'   vertical), fov — ou NULL si aucune source de coordonnees n'est disponible.
get_spatial_coords <- function(spatial_obj) {
  tc <- tryCatch(Seurat::GetTissueCoordinates(spatial_obj, scale = NULL), error = function(e) NULL)
  
  if (is.null(tc) || nrow(tc) == 0) {
    # v6.1 fallback: manual-loader coordinate contract (coord_x/coord_y).
    meta <- spatial_obj@meta.data
    if (all(c("coord_x", "coord_y") %in% colnames(meta))) {
      return(data.frame(
        id  = colnames(spatial_obj),
        x   = as.numeric(meta$coord_x),
        y   = as.numeric(meta$coord_y),
        fov = NA_character_,
        stringsAsFactors = FALSE
      ))
    }
    return(NULL)
  }
  
  colnames_tc <- tolower(colnames(tc))
  
  col_x <- if ("imagecol" %in% colnames_tc) {
    colnames(tc)[which(colnames_tc == "imagecol")]
  } else if ("x" %in% colnames_tc) {
    colnames(tc)[which(colnames_tc == "x")]
  } else {
    colnames(tc)[2] # fallback Seurat v4 (deuxième colonne = imagecol)
  }
  
  col_y <- if ("imagerow" %in% colnames_tc) {
    colnames(tc)[which(colnames_tc == "imagerow")]
  } else if ("y" %in% colnames_tc) {
    colnames(tc)[which(colnames_tc == "y")]
  } else {
    colnames(tc)[1] # fallback Seurat v4 (première colonne = imagerow)
  }
  
  df <- data.frame(
    id = rownames(tc) %||% tc$cell %||% paste0("spot_", seq_len(nrow(tc))),
    x = as.numeric(tc[[col_x]]),
    y = as.numeric(tc[[col_y]]),
    fov = if ("fov" %in% colnames_tc) tc[[colnames(tc)[which(colnames_tc == "fov")]]] else NA_character_,
    stringsAsFactors = FALSE
  )
  
  df
}

#' Manually correct spot coordinate orientation (swap / mirror)
#'
#' See file header (v5, fix #2) and get_spatial_coords()'s note for the full
#' rationale: Seurat's own GetTissueCoordinates.VisiumV1() column-order
#' convention is confirmed to differ between releases, so rather than ship
#' another unverified automatic heuristic, this exposes the correction as an
#' explicit, one-click, visually-verifiable MANUAL control (3 checkboxes in
#' mod_import_spatial.R: swap_xy / flip_x / flip_y).
#'
#' Applied ONCE at import time to $coords (see convert_to_bpcells_and_fov()),
#' NOT per-viz-session -- every downstream consumer (histology overlay,
#' BANKSY-lite's physical k-NN graph in mod_spatial_cluster.R, Moran's I,
#' ROI, multi-sample section maps) reads the SAME $coords, so a wrong
#' orientation silently corrupts spatial clustering too, not just the
#' picture. Fixing it once here fixes it everywhere.
#'
#' @param coords data.frame(id, x, y, fov) as returned by get_spatial_coords().
#' @param swap_xy Logical, swap the x and y columns (corrects a 90-degree-type mismatch).
#' @param flip_x Logical, mirror horizontally.
#' @param flip_y Logical, mirror vertically.
#' @param img_width,img_height Optional numeric, full-resolution histology
#'   image dimensions in pixels. When available, mirroring reflects around
#'   the IMAGE extent rather than just the point cloud's own bounding box
#'   (more correct when the tissue doesn't fill the whole capture area).
#'   Falls back to the point cloud's own range when NULL (e.g. Xenium/CosMx,
#'   or Visium with no histology extracted).
#' @return coords, geometry corrected. Unchanged if all three flags are FALSE.
apply_coord_orientation <- function(coords, swap_xy = FALSE, flip_x = FALSE, flip_y = FALSE,
                                     img_width = NULL, img_height = NULL) {
  if (is.null(coords) || nrow(coords) == 0 || !(isTRUE(swap_xy) || isTRUE(flip_x) || isTRUE(flip_y))) {
    return(coords)
  }
  
  if (isTRUE(swap_xy)) {
    tmp <- coords$x
    coords$x <- coords$y
    coords$y <- tmp
    tmp_dim <- img_width
    img_width <- img_height
    img_height <- tmp_dim
  }
  if (isTRUE(flip_x)) {
    w <- img_width %||% diff(range(coords$x, na.rm = TRUE))
    coords$x <- w - coords$x
  }
  if (isTRUE(flip_y)) {
    h <- img_height %||% diff(range(coords$y, na.rm = TRUE))
    coords$y <- h - coords$y
  }
  coords
}

#' Sanity-check that spot coordinates and the histology raster share the
#' same pixel scale (defensive regression guard)
#'
#' @param coords data.frame(id, x, y, ...) full-resolution coordinates.
#' @param hist_img list as returned by extract_histology_image().
#' @return invisible(TRUE) if aligned within tolerance.
check_histology_coord_alignment <- function(coords, hist_img) {
  # Support des deux structures (ancienne et nouvelle API)
  sf_lowres <- if (!is.null(hist_img$lowres$scale_factor)) {
    hist_img$lowres$scale_factor
  } else if (!is.null(hist_img$scale_factors$lowres)) {
    hist_img$scale_factors$lowres
  } else {
    1
  }
  
  img_dim <- if (!is.null(hist_img$lowres$dim)) {
    hist_img$lowres$dim
  } else {
    hist_img$dim
  }
  
  if (is.null(img_dim) || any(is.na(img_dim)) || length(img_dim) < 2) return(invisible(TRUE))
  
  img_w <- img_dim[2] / (sf_lowres %||% 1)
  img_h <- img_dim[1] / (sf_lowres %||% 1)
  
  spot_w <- diff(range(coords$x, na.rm = TRUE))
  spot_h <- diff(range(coords$y, na.rm = TRUE))
  
  ratio_w <- spot_w / img_w
  ratio_h <- spot_h / img_h
  
  ok <- is.finite(ratio_w) && is.finite(ratio_h) &&
    ratio_w > 0.02 && ratio_w < 5 &&
    ratio_h > 0.02 && ratio_h < 5
  
  if (is.na(ok) || !ok) {
    warning(
      "Incoherence ou verification impossible entre les coordonnees des spots et l'image histologique.",
      call. = FALSE
    )
  }
  invisible(identical(ok, TRUE))
}

# -----------------------------------------------------------------------------
# Main conversion pipeline
# -----------------------------------------------------------------------------

#' Convert a raw Seurat spatial object to an on-disk BPCells-backed assay
#'
#' @param seurat_obj Seurat object as returned by load_spatial_visium()
#'   / Seurat::LoadXenium() / Seurat::LoadNanostring().
#' @param dataset_id Character, unique slug for this dataset.
#' @param technology One of "visium", "xenium", "cosmx", "slideseq".
#' @param assay Character, assay holding raw counts.
#' @param simplify_tol Numeric tolerance passed to Seurat::Simplify().
#' @param max_sketch Integer, max cells/spots kept in the in-RAM sketch.
#' @param norm_method "lognorm" (default) or "sct".
#' @param raw_dir Optional character, the ROOT folder the user picked in the
#'   import UI -- passed through to extract_histology_image() so it can
#'   reliably find spatial/ (see that function's header, v5 fix #1).
#' @param swap_xy,flip_x,flip_y Logical, manual coordinate-orientation
#'   correction -- see apply_coord_orientation() (v5 fix #2).
#' @param raw_bg_obj Optional Seurat object, the UNFILTERED/background
#'   matrix (e.g. Visium's raw_feature_bc_matrix.h5, ALL barcodes incl.
#'   empty/background spots — see helpers_io.R::load_spatial_visium_raw()).
#'   Purely additive: when provided, written to its OWN BPCells directory
#'   (returned as $raw_bpcells_dir) alongside the primary filtered matrix.
#'   NULL (default) = filtered-only import, unchanged behavior. Reserved
#'   for a future ambient-RNA correction step; no existing consumer reads
#'   this field.
#' @return List — see file header CONTRACT.
convert_to_bpcells_and_fov <- function(seurat_obj, dataset_id,
                                       technology = c("visium", "xenium", "cosmx", "slideseq"),
                                       assay = NULL,
                                       simplify_tol = 20,
                                       max_sketch = 50000,
                                       norm_method = c("lognorm", "sct"),
                                       raw_dir = NULL,
                                       swap_xy = FALSE, flip_x = FALSE, flip_y = FALSE,
                                       raw_bg_obj = NULL) {
  technology  <- match.arg(technology)
  norm_method <- match.arg(norm_method)
  if (!requireNamespace("BPCells", quietly = TRUE)) {
    stop(errorCondition(paste0("Package 'BPCells' requis pour l'import spatial (stockage sur disque). ", "Installez via remotes::install_github('bnprks/BPCells/r')."), class = "spatial_io_error"))
  }
  if (!inherits(seurat_obj, "Seurat")) stop(errorCondition("seurat_obj doit etre un objet Seurat.", class = "spatial_io_error"))
  
  assay <- assay %||% Seurat::DefaultAssay(seurat_obj)
  dataset_id <- gsub("[^A-Za-z0-9_-]", "_", dataset_id)
  bpcells_dir <- file.path(bpcells_cache_root(), dataset_id, "counts_bpcells")
  dir.create(dirname(bpcells_dir), showWarnings = FALSE, recursive = TRUE)
  if (dir.exists(bpcells_dir)) unlink(bpcells_dir, recursive = TRUE)
  
  # --- 1. Counts -> disk (BPCells).
  counts <- SeuratObject::LayerData(seurat_obj, assay = assay, layer = "counts")
  BPCells::write_matrix_dir(counts, bpcells_dir, overwrite = TRUE)
  bpcells_mat <- BPCells::open_matrix_dir(bpcells_dir)
  SeuratObject::LayerData(seurat_obj, assay = assay, layer = "counts") <- bpcells_mat
  
  # --- 1b. OPTIONAL raw/background (unfiltered) matrix -> its OWN disk
  # BPCells dir. Purely additive: existing consumers of spatial_obj
  # (QC/cluster/deconv/viz) never read raw_bpcells_dir -- reserved for a
  # future ambient-RNA correction step (DecontX or similar), which needs
  # the background/empty-droplet profile the filtered matrix alone cannot
  # provide (see handoff_spatial_bio-mg.md long-term backlog). Stays NULL
  # unless the caller explicitly passes raw_bg_obj (see mod_import_spatial.R
  # "Importer aussi la matrice brute" checkbox); never fatal on failure.
  raw_bpcells_dir <- NULL
  n_raw_total <- NULL
  if (!is.null(raw_bg_obj) && inherits(raw_bg_obj, "Seurat")) {
    raw_bpcells_dir <- file.path(bpcells_cache_root(), dataset_id, "counts_bpcells_raw")
    dir.create(dirname(raw_bpcells_dir), showWarnings = FALSE, recursive = TRUE)
    if (dir.exists(raw_bpcells_dir)) unlink(raw_bpcells_dir, recursive = TRUE)
    tryCatch({
      raw_assay  <- Seurat::DefaultAssay(raw_bg_obj)
      raw_counts <- SeuratObject::LayerData(raw_bg_obj, assay = raw_assay, layer = "counts")
      BPCells::write_matrix_dir(raw_counts, raw_bpcells_dir, overwrite = TRUE)
      n_raw_total <- ncol(raw_bg_obj)
    }, error = function(e) {
      warning("Ecriture BPCells de la matrice brute (raw) echouee : ", conditionMessage(e),
              " — l'import se poursuit avec la matrice filtree seule.", call. = FALSE)
      raw_bpcells_dir <<- NULL
    })
  }
  
  # --- 2. Imaging-only: simplify segmentation polygons.
  if (technology %in% c("xenium", "cosmx")) {
    seurat_obj <- .simplify_all_fovs(seurat_obj, tol = simplify_tol)
  }
  
  # --- 3. Histology image (multi-resolution). Extracted BEFORE coordinates
  # (order swapped in v5) so a manual orientation fix below can mirror
  # around the actual image extent rather than just the point cloud's own
  # bounding box.
  hist_img <- extract_histology_image(seurat_obj, technology, raw_dir = raw_dir)
  
  # --- 4. Full-resolution coordinates.
  coords <- tryCatch(
    get_spatial_coords(seurat_obj),
    error = function(e) {
      warning("GetTissueCoordinates() a echoue : ", conditionMessage(e),
              " — le clustering spatial (BANKSY) sera indisponible pour ce jeu de donnees.")
      NULL
    }
  )
  if (is.null(coords)) {
    warning("Extraction des coordonnees spatiales impossible (get_spatial_coords() a retourne NULL) — ",
            "le clustering spatial (BANKSY) sera indisponible pour ce jeu de donnees.")
  }
  
  # --- 4b. Manual orientation correction (v5 fix #2) — applied ONCE here so
  # clustering/Moran/viz/multi-sample all share the same corrected geometry.
  if (!is.null(coords) && (isTRUE(swap_xy) || isTRUE(flip_x) || isTRUE(flip_y))) {
    img_w <- NULL; img_h <- NULL
    ref_img <- hist_img$lowres %||% (if (!is.null(hist_img$images) && length(hist_img$images) > 0) hist_img$images[[1]] else NULL)
    if (!is.null(ref_img) && !is.null(ref_img$dim)) {
      sf <- ref_img$scale_factor %||% 1
      img_w <- ref_img$dim[2] / sf
      img_h <- ref_img$dim[1] / sf
    }
    coords <- apply_coord_orientation(coords, swap_xy = swap_xy, flip_x = flip_x, flip_y = flip_y,
                                      img_width = img_w, img_height = img_h)
  }
  
  if (!is.null(coords) && !is.null(hist_img)) {
    check_histology_coord_alignment(coords, hist_img)
  }
  
  # --- 5. In-RAM sketch.
  sketch <- build_sketch(seurat_obj, max_cells = max_sketch, assay = assay,
                         norm_method = norm_method)
  
  list(
    sketch      = sketch,
    bpcells_dir = bpcells_dir,
    raw_bpcells_dir = raw_bpcells_dir,   # NEW (additive): NULL unless raw_bg_obj was provided
    n_raw_total     = n_raw_total,       # NEW (additive): barcode count of the raw/background matrix
    coords      = coords,
    histology   = hist_img,
    technology  = technology,
    n_total     = ncol(seurat_obj),
    images      = tryCatch(Seurat::Images(seurat_obj), error = function(e) character(0)),
    project     = dataset_id,
    created_at  = Sys.time()
  )
}

#' Simplify polygon segmentation boundaries on every FOV of an object
#'
#' Best-effort: any single boundary that fails is skipped with a warning.
#'
#' @param obj Seurat object with one or more imaging FOVs.
#' @param tol Numeric, SeuratObject::Simplify() tolerance.
#' @return The Seurat object, each FOV's segmentation simplified in place.
.simplify_all_fovs <- function(obj, tol = 20) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    warning("Package 'sf' manquant : simplification des polygones ignoree (Simplify() en depend).")
    return(obj)
  }
  fov_names <- tryCatch(Seurat::Images(obj), error = function(e) character(0))
  for (fv in fov_names) {
    fov_obj <- obj[[fv]]
    if (!methods::is(fov_obj, "FOV")) next
    
    bnames <- tryCatch(SeuratObject::Boundaries(fov_obj), error = function(e) character(0))
    boundaries_slot <- tryCatch(methods::slot(fov_obj, "boundaries"), error = function(e) NULL)
    if (is.null(boundaries_slot)) next
    
    for (bn in bnames) {
      seg <- tryCatch(fov_obj[[bn]], error = function(e) NULL)
      if (is.null(seg)) next
      seg_simple <- tryCatch(
        SeuratObject::Simplify(coords = seg, tol = tol),
        error = function(e) {
          warning(sprintf("Simplify() a echoue sur '%s'/'%s' : %s — polygone non simplifie.",
                          fv, bn, conditionMessage(e)))
          NULL
        }
      )
      if (is.null(seg_simple)) next
      idx <- which(names(boundaries_slot) == bn)
      if (length(idx) == 1) boundaries_slot[[idx]] <- seg_simple
    }
    tryCatch({
      methods::slot(fov_obj, "boundaries") <- boundaries_slot
      obj[[fv]] <- fov_obj
    }, error = function(e) {
      warning(sprintf("Impossible d'ecrire les polygones simplifies pour le FOV '%s' : %s",
                      fv, conditionMessage(e)))
    })
  }
  obj
}

#' Crop an object's FOV to a bounding box for zoomed-in rendering
#'
#' Thin, defensive wrapper around SeuratObject::Crop().
#'
#' @param obj Seurat object (or a single FOV object).
#' @param fov Character, FOV/image name (ignored if `obj` is already a FOV).
#' @param x,y Numeric length-2 vectors, bounding box in tissue coordinates.
#' @return A cropped FOV object, or NULL on failure.
crop_fov_bbox <- function(obj, fov, x, y) {
  tryCatch({
    target <- if (inherits(obj, "Seurat")) obj[[fov]] else obj
    SeuratObject::Crop(target, x = x, y = y, coords = "plot")
  }, error = function(e) {
    warning("Crop() a echoue : ", conditionMessage(e))
    NULL
  })
}

# -----------------------------------------------------------------------------
# Sketch construction
# -----------------------------------------------------------------------------

#' Build (or rebuild) the in-RAM plotting sketch for a spatial object
#'
#' Primary path: Seurat's own `SketchData(method = "Uniform")`.
#' Falls back to uniform `sample()` if SketchData fails.
#'
#' @param obj Seurat object (full, BPCells-backed or not).
#' @param max_cells Integer, sketch size cap.
#' @param assay Character, assay to materialize into RAM for the sketch.
#' @param norm_method "lognorm" (default) or "sct".
#' @return Seurat object, <= max_cells cells/spots, in-RAM Assay5.
build_sketch <- function(obj, max_cells = 50000, assay = NULL,
                         norm_method = c("lognorm", "sct")) {
  norm_method <- match.arg(norm_method)
  assay <- assay %||% Seurat::DefaultAssay(obj)
  
  sk <- tryCatch({
    # Initial LogNormalize so SketchData has a "data" layer.
    obj_norm <- Seurat::NormalizeData(obj, assay = assay, verbose = FALSE)
    obj_norm <- Seurat::SketchData(obj_norm, assay = assay, ncells = max_cells,
                                   sketched.assay = "sketch", method = "Uniform",
                                   cast = "dgCMatrix", verbose = FALSE)
    sketch_cells <- SeuratObject::Cells(obj_norm[["sketch"]])
    sk <- subset(obj_norm, cells = sketch_cells)
    sk <- Seurat::DietSeurat(sk, assays = "sketch")
    Seurat::DefaultAssay(sk) <- "sketch"
    sk
  }, error = function(e) {
    warning("Seurat::SketchData() a echoue (", conditionMessage(e),
            ") — repli sur un sous-echantillonnage aleatoire simple.")
    n <- ncol(obj)
    idx <- if (n <= max_cells) seq_len(n) else { set.seed(42); sort(sample(seq_len(n), max_cells)) }
    sk <- obj[, idx]
    sk_counts <- methods::as(SeuratObject::LayerData(sk, assay = assay, layer = "counts"), "dgCMatrix")
    sk[["sketch"]] <- SeuratObject::CreateAssay5Object(counts = sk_counts)
    Seurat::DefaultAssay(sk) <- "sketch"
    sk
  })
  
  # User-facing normalization (SCT opt‑in) on the small sketch only.
  if (identical(norm_method, "sct")) {
    sk <- tryCatch({
      run_sct <- function() {
        old_plan <- future::plan()
        on.exit(future::plan(old_plan), add = TRUE)
        future::plan("sequential")
        Seurat::SCTransform(sk, assay = "sketch", new.assay.name = "SCT",
                            variable.features.n = 3000, verbose = FALSE)
      }
      run_sct()
    }, error = function(e) {
      warning("SCTransform() a echoue sur le sketch (", conditionMessage(e),
              ") — repli sur NormalizeData (LogNormalize) standard.")
      Seurat::NormalizeData(sk, assay = "sketch", verbose = FALSE)
    })
    Seurat::DefaultAssay(sk) <- if ("SCT" %in% SeuratObject::Assays(sk)) "SCT" else "sketch"
  }
  
  # Guarantee a populated "data" layer.
  has_data <- tryCatch({
    d <- SeuratObject::LayerData(sk, layer = "data")
    !is.null(d) && length(d) > 0 && nrow(d) > 0
  }, error = function(e) FALSE)
  if (!has_data) {
    sk <- tryCatch(Seurat::NormalizeData(sk, verbose = FALSE),
                   error = function(e) { warning("Normalisation du sketch echouee : ", conditionMessage(e)); sk })
  }
  sk
}

# -----------------------------------------------------------------------------
# Fast QC metrics from BPCells
# -----------------------------------------------------------------------------

#' Fast, streamed QC stats directly from the on-disk BPCells matrix
#'
#' @param bpcells_dir Character path (global_data$spatial_obj$bpcells_dir).
#' @param mt_pattern Regex for mitochondrial genes.
#' @param ribo_pattern Regex for ribosomal genes.
#' @return data.frame(id, nCount, nFeature, pct_mt, pct_ribo, log_nCount).
compute_qc_metrics_fast <- function(bpcells_dir, mt_pattern = "^MT-", ribo_pattern = "^RP[SL]") {
  if (!requireNamespace("BPCells", quietly = TRUE)) stop(errorCondition("Package 'BPCells' requis.", class = "spatial_io_error"))
  mat <- BPCells::open_matrix_dir(bpcells_dir)
  
  n_count   <- Matrix::colSums(mat)
  n_feature <- Matrix::colSums(mat > 0)
  
  gene_names <- rownames(mat)
  mt_idx   <- grepl(mt_pattern, gene_names)
  ribo_idx <- grepl(ribo_pattern, gene_names)
  
  pct_mt   <- if (any(mt_idx))   100 * Matrix::colSums(mat[mt_idx, , drop = FALSE])   / pmax(n_count, 1) else rep(NA_real_, ncol(mat))
  pct_ribo <- if (any(ribo_idx)) 100 * Matrix::colSums(mat[ribo_idx, , drop = FALSE]) / pmax(n_count, 1) else rep(NA_real_, ncol(mat))
  
  data.frame(
    id        = colnames(mat),
    nCount    = as.numeric(n_count),
    nFeature  = as.numeric(n_feature),
    pct_mt    = as.numeric(pct_mt),
    pct_ribo  = as.numeric(pct_ribo),
    # log_nCount: equivalent of the vignette's log_nCount_Spatial (base-10,
    # +1 pseudocount) — generic across ALL technologies, not Slide-seq
    # specific, since large-dynamic-range puck/bead data (Slide-seq) reads
    # far more legibly on a log scale than raw counts, and it costs nothing
    # for Visium/Xenium/CosMx either. Surfaced as a QC-metric choice in
    # mod_spatial_qc.R's histogram grid and mod_spatial_viz.R's map viewer.
    log_nCount = log10(as.numeric(n_count) + 1),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# ROI materialization
# -----------------------------------------------------------------------------

#' Materialize a small, on-RAM Seurat object for a subset of full-resolution ids
#'
#' @param spatial_obj The full global_data$spatial_obj LIST.
#' @param cell_ids Character vector of ids to keep.
#' @param project Character, Seurat project name.
#' @return A Seurat object (raw counts, single "RNA" assay).
materialize_seurat_subset <- function(spatial_obj, cell_ids, project = NULL) {
  if (!requireNamespace("BPCells", quietly = TRUE)) stop(errorCondition("Package 'BPCells' requis.", class = "spatial_io_error"))
  if (length(cell_ids) == 0) stop(errorCondition("Aucun identifiant fourni pour la ROI.", class = "spatial_io_error"))
  bpcells_dir <- spatial_obj$bpcells_dir
  if (is.null(bpcells_dir) || !dir.exists(bpcells_dir)) {
    stop(errorCondition("bpcells_dir introuvable sur disque pour ce jeu de donnees.", class = "spatial_io_error"))
  }
  
  mat <- BPCells::open_matrix_dir(bpcells_dir)
  cell_ids <- intersect(cell_ids, colnames(mat))
  if (length(cell_ids) == 0) stop(errorCondition("Aucun des identifiants de la ROI n'est present dans la matrice BPCells.", class = "spatial_io_error"))
  
  mat_sub <- mat[, cell_ids, drop = FALSE]
  counts_dense <- methods::as(mat_sub, "dgCMatrix")
  obj <- Seurat::CreateSeuratObject(counts = counts_dense, project = project %||% spatial_obj$project %||% "ROI")
  
  coords <- spatial_obj$coords
  if (!is.null(coords)) {
    m <- match(colnames(obj), coords$id)
    obj$roi_x <- coords$x[m]
    obj$roi_y <- coords$y[m]
  }
  obj
}

# -----------------------------------------------------------------------------
# Debug helper
# -----------------------------------------------------------------------------

debug_histology <- function(global_data) {
  hist_data <- state_get(global_data, "spatial_obj")$histology
  cat("=== Histology Debug ===\n")
  cat("histology exists:", !is.null(hist_data), "\n")
  if (!is.null(hist_data)) {
    cat("names:", paste(names(hist_data), collapse=", "), "\n")
    for (res in c("lowres", "hires")) {
      if (!is.null(hist_data[[res]])) {
        cat(res, "raster class:", class(hist_data[[res]]$raster)[1], "\n")
        if (!is.null(hist_data[[res]]$dim)) {
          cat(res, "dim:", paste(hist_data[[res]]$dim, collapse=" x "), "\n")
        }
        cat(res, "scale_factor:", hist_data[[res]]$scale_factor, "\n")
      }
    }
    if (!is.null(hist_data$images)) {
      cat("Additional images in $images:", paste(names(hist_data$images), collapse=", "), "\n")
    }
  }
  cat("========================\n")
}

# =============================================================================
# R/utils_spatial_niche.R — Spatial niche detection ("BuildNicheAssay-lite")
# =============================================================================
# NEW (Phase 5 — "niche" analysis, seurat5_spatial_vignette_2.Rmd parity —
# section "Niche analysis", BuildNicheAssay()). Not previously covered
# anywhere in this app: every existing tab groups spots/cells by their OWN
# expression (BANKSY-lite clustering) or their OWN predicted identity
# (deconvolution/label transfer) -- niches instead group them by the
# COMPOSITION of their spatial NEIGHBORHOOD, revealing regions defined by
# which populations co-occur nearby (e.g. a tumor/stroma interface, a
# germinal-center border) rather than by any single spot's own identity.
#
# Reimplements BuildNicheAssay()'s core idea directly, same spirit as
# mod_spatial_cluster.R's BANKSY-lite reimplementation: rather than
# constructing a full Seurat/FOV object just to call BuildNicheAssay()
# (which expects an actual FOV/segmentation-aware object), this computes the
# same statistic straight from (coords, group_labels) already available in
# this app's lightweight spatial_obj contract (BPCells + coords, never a
# live Seurat/FOV object -- see R/utils_spatial_io.R header):
#   1. k-NN in PHYSICAL (x,y) space (RANN::nn2 -- identical approach to
#      BANKSY-lite's own physical neighbor graph).
#   2. For each spot/cell, the COMPOSITION of its k neighbors' group labels
#      (cluster id, or dominant cell type) -- one-hot encode + the SAME
#      sparse "indicator matrix" multiply trick already used in this project
#      (mod_spatial_cluster.R's own neighbor-averaging step,
#      helpers_sc.R::remap_seurat_ids_to_symbol()'s duplicate collapse)
#      instead of a per-row loop.
#   3. k-means on the composition vectors -> niche assignment. This is
#      exactly BuildNicheAssay()'s own approach ("we then use k-means
#      clustering to group cells that have similar neighborhoods together,
#      into spatial niches" -- seurat5_spatial_vignette_2.Rmd).
#
# No expression matrix is needed at all here (only coords + a small
# categorical vector), so this is cheap even at FULL resolution (not just
# the <=50k-cell RAM sketch) and needs no BPCells/Seurat object shipped into
# the mirai daemon -- `coords` and `group_labels` are both small and travel
# in directly, same pattern as moran_task's `coords` argument.
#
# `group_labels` is deliberately generic: the caller (mod_spatial_niche.R)
# can pass EITHER shared_rv$cluster_labels (BANKSY-lite clusters) OR the
# result of dominant_group_labels(shared_rv$deconv_props) below (argmax
# cell type per spot from RCTD/Label Transfer/STdeconvolve) -- niches over
# "who is spatially next to whom" make sense for either kind of identity.
# =============================================================================

#' Compute spatial niches from a categorical group label (BuildNicheAssay-lite)
#'
#' @param coords data.frame(id, x, y, ...) — full-resolution spatial
#'   coordinates (global_data$spatial_obj$coords).
#' @param group_labels Named character/factor vector (names = ids matching
#'   `coords$id`), e.g. a cluster assignment or a dominant-cell-type call.
#'   Must have >= 2 distinct non-NA levels.
#' @param k_neighbors Integer, spatial k-NN size (Seurat's BuildNicheAssay()
#'   default: neighbors.k = 30).
#' @param n_niches Integer, number of niches / k-means k (Seurat's default:
#'   niches.k = 5).
#' @param log_file Optional path for write_mirai_log() progress lines (NULL
#'   = silent, e.g. for unit testing outside a daemon).
#' @return list(
#'   assignments       = data.frame(id, niche) — niche is "N1".."N<n_niches>".
#'   niche_composition = data.frame(niche, <one column per group level>) —
#'                        mean neighborhood composition per niche, with the
#'                        EXACT group-level names preserved as column names
#'                        (check.names = FALSE) so labels like "L2/3 IT" are
#'                        not mangled -- ready for direct display/labeling.
#' )
compute_spatial_niches <- function(coords, group_labels, k_neighbors = 30,
                                    n_niches = 5, log_file = NULL) {
  .log <- function(msg, step, total) {
    if (!is.null(log_file)) write_mirai_log(log_file, msg, step, total)
  }
  if (!requireNamespace("RANN", quietly = TRUE)) {
    stop(errorCondition("Package 'RANN' requis (install.packages('RANN')).", class = "spatial_niche_error"))
  }

  .log("Alignement coordonnees / labels...", 1, 4)
  group_labels <- group_labels[!is.na(group_labels) & nzchar(as.character(group_labels))]
  common_ids <- intersect(coords$id, names(group_labels))
  if (length(common_ids) < 10) {
    stop(errorCondition(paste0("Moins de 10 elements communs entre les coordonnees et le regroupement choisi ", "(cluster ou deconvolution) — recalculez ce regroupement si necessaire."), class = "spatial_niche_error"))
  }

  cd <- coords[match(common_ids, coords$id), c("id", "x", "y")]
  cd <- cd[stats::complete.cases(cd[, c("x", "y")]), , drop = FALSE]
  grp <- as.character(group_labels[cd$id])
  lv  <- sort(unique(grp))
  if (length(lv) < 2) {
    stop(errorCondition(paste0("Le regroupement choisi n'a qu'une seule categorie — impossible de calculer ", "une composition de voisinage informative."), class = "spatial_niche_error"))
  }

  n <- nrow(cd)
  k_eff <- max(2, min(k_neighbors, n - 1))
  .log(sprintf("Voisinage spatial (k=%d, RANN)...", k_eff), 2, 4)
  coords_mat <- as.matrix(cd[, c("x", "y")])
  nn <- RANN::nn2(coords_mat, k = min(k_eff + 1, n))
  neighbor_idx <- nn$nn.idx[, -1, drop = FALSE]  # drop self (1st column, distance 0)

  .log("Composition des voisinages (one-hot x moyenne)...", 3, 4)
  # One-hot encoded BY HAND (not stats::model.matrix()) to preserve exact
  # group-level names as column names -- model.matrix() would mangle names
  # containing spaces/slashes (e.g. a cell-type label like "L2/3 IT").
  onehot <- vapply(lv, function(l) as.numeric(grp == l), numeric(n))
  colnames(onehot) <- lv

  kk <- ncol(neighbor_idx)
  W <- Matrix::sparseMatrix(
    i = rep(seq_len(n), each = kk), j = as.vector(t(neighbor_idx)),
    x = 1 / kk, dims = c(n, n)
  )
  composition <- as.matrix(W %*% onehot)  # n x length(lv), rows sum to ~1
  dimnames(composition) <- list(cd$id, lv)

  .log(sprintf("Clustering des niches (k-means, k=%d)...", n_niches), 4, 4)
  n_niches_eff <- max(2, min(n_niches, n - 1))
  km <- tryCatch(
    stats::kmeans(composition, centers = n_niches_eff, nstart = 10, iter.max = 100),
    error = function(e) {
      stop(errorCondition(paste0("k-means a echoue sur la composition de voisinage (", conditionMessage(e), ") — reduisez le nombre de niches ou augmentez neighbors.k."), class = "spatial_niche_error"))
    }
  )
  niche_vec <- paste0("N", km$cluster)

  # rowsum() preserves matrix column names exactly (no data.frame round-trip
  # mangling) -- same pattern already used in
  # helpers_io.R::remap_gene_ids_to_symbol()'s duplicate-ID collapse.
  niche_sums   <- rowsum(composition, group = niche_vec, reorder = TRUE)
  niche_counts <- as.numeric(table(niche_vec)[rownames(niche_sums)])
  niche_means  <- niche_sums / niche_counts

  list(
    assignments = data.frame(id = cd$id, niche = niche_vec,
                              row.names = NULL, stringsAsFactors = FALSE),
    niche_composition = data.frame(niche = rownames(niche_means), niche_means,
                                    check.names = FALSE, row.names = NULL)
  )
}

#' Derive a dominant-group label per spot/cell from a deconvolution result
#'
#' Simple argmax over the proportion/score columns of a
#' shared_rv$deconv_props-shaped data.frame (id + one column per cell type —
#' see mod_spatial_deconv.R, same contract for RCTD/Label Transfer/
#' STdeconvolve). Used as an alternative `group_labels` input for
#' compute_spatial_niches() when niches based on ASSIGNED CELL TYPE (rather
#' than an unsupervised spatial cluster) are of more direct biological
#' interest, or when no clustering has been run yet.
#'
#' @param deconv_props data.frame(id, <cell type columns>...).
#' @return Named character vector: id -> dominant cell type (column name
#'   with the highest proportion/score for that row).
dominant_group_labels <- function(deconv_props) {
  cols <- setdiff(colnames(deconv_props), "id")
  if (length(cols) == 0) stop(errorCondition("Aucune colonne de type cellulaire dans deconv_props.", class = "spatial_niche_error"))
  mat <- as.matrix(deconv_props[, cols, drop = FALSE])
  idx <- max.col(mat, ties.method = "first")
  stats::setNames(cols[idx], deconv_props$id)
}

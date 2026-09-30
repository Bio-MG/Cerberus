# =============================================================================
# test-sc-velocity-loom.R — lecteurs .loom / .h5ad (roadmap SC FUNCTION_TEST M0)
# =============================================================================
# read_velocity_loom() / read_velocity_h5ad() (R/sc/sc_velocity.R) : lecture
# stricte de couches velocity PRE-COMPUTES (aucune inference), sortie dans le
# même format whitelisté que read_velocity_rds(), validée par la même
# métavalidation. Fixtures HDF5 minimales écrites avec hdf5r ; tests skippés
# si le paquet est absent (dépendance déclarée dans renv.lock).
# =============================================================================

.vel_has_hdf5r <- requireNamespace("hdf5r", quietly = TRUE)

# Fixture .loom : couches denses compressées, orientation (cells x genes)
# stockée (layout mesuré sur velocyto.py), + Clusters/_X/_Y.
.vel_loom_fixture <- function(path, genes_x_cells = FALSE, with_ambiguous = TRUE,
                              dup_genes = FALSE) {
  genes <- c("GeneA", "GeneB", "GeneC")
  if (dup_genes) genes <- c(genes, "GeneA")       # 1 symbole dupliqué
  cells <- paste0("cell", 1:4)
  n_g <- length(genes); n_c <- length(cells)
  base <- matrix(1:(n_g * n_c), nrow = n_g, ncol = n_c)   # (genes x cells) logique
  f <- hdf5r::h5file(path, mode = "w")
  # hdf5r ne crée PAS les groupes intermédiaires à l'écriture (mesuré :
  # H5Gtraverse 'object already exists') -> création explicite.
  f$create_group("row_attrs"); f$create_group("col_attrs"); f$create_group("layers")
  # L'écriture reproduit le layout MESURÉ : couches (cells x genes) sauf si
  # genes_x_cells demandé (couverture des deux orientations de lecture).
  store <- if (genes_x_cells) base else t(base)
  f[["layers/spliced"]] <- store
  f[["layers/unspliced"]] <- store * 2L
  if (with_ambiguous) f[["layers/ambiguous"]] <- store
  f[["row_attrs/Gene"]] <- genes
  f[["col_attrs/CellID"]] <- cells
  f[["col_attrs/Clusters"]] <- rep(c(0L, 1L), length.out = n_c)
  f[["col_attrs/_X"]] <- seq_len(n_c) + 0.5
  f[["col_attrs/_Y"]] <- seq_len(n_c) - 0.5
  f$close_all()
  path
}

# Fixture .h5ad : layout old-anndata mesuré (composé /obs,/var ; couches CSR
# data/indices/indptr — cellules en lignes ; /obsm/X_umap en (dims x cells)).
.vel_h5ad_fixture <- function(path) {
  genes <- paste0("G", 1:3)
  cells <- paste0("c", 1:4)
  mat <- matrix(c(1, 0, 2,
                  0, 0, 0,
                  3, 0, 4,
                  0, 5, 0), nrow = 4, ncol = 3, byrow = TRUE)  # (cells x genes)
  f <- hdf5r::h5file(path, mode = "w")
  f$create_group("layers"); f$create_group("obsm")
  for (layer in c("spliced", "unspliced")) {
    v <- if (layer == "spliced") mat else mat * 2L
    # CSR = parcours LIGNE par ligne (cellule majeure, v est cells x genes) :
    # which()/v[...] extraient en COLUMN-major R -> tri par ligne puis colonne.
    nz  <- which(v != 0, arr.ind = TRUE)
    ord <- order(nz[, 1], nz[, 2])
    f$create_group(file.path("layers", layer))
    f[[file.path("layers", layer, "data")]] <- as.numeric(v[v != 0])[ord]
    f[[file.path("layers", layer, "indices")]] <- nz[ord, 2] - 1L
    f[[file.path("layers", layer, "indptr")]] <-
      c(0L, cumsum(rowSums(v != 0)))   # CSR : cellules en lignes
  }
  obs <- data.frame(index = cells, clusters = c(1L, 1L, 2L, 2L))
  var <- data.frame(index = genes, hv = c(0L, -1L, 0L))
  f[["obs"]] <- obs
  f[["var"]] <- var
  f[["obsm/X_umap"]] <- rbind(seq_along(cells) + 0.5, seq_along(cells) - 0.5)
  f$close_all()
  path
}

test_that("read_velocity_loom : couches sparsifiées, attrs, métavalidation (layout cells x genes)", {
  skip_if_not(.vel_has_hdf5r, "hdf5r absent")
  path <- .vel_loom_fixture(tempfile(fileext = ".loom"))

  vi <- read_velocity_loom(path)
  expect_s4_class(vi$spliced, "dgCMatrix")
  expect_identical(dim(vi$spliced), c(3L, 4L))          # genes x cells
  expect_identical(dimnames(vi$spliced), list(c("GeneA", "GeneB", "GeneC"),
                                              paste0("cell", 1:4)))
  # transposition correcte : spliced[gene i, cell j] = i + (j-1)*3
  expect_identical(as.numeric(vi$spliced[2, 3]), 8)
  expect_identical(as.numeric(vi$unspliced[2, 3]), 16)
  expect_identical(vi$cell_names, paste0("cell", 1:4))
  expect_identical(vi$gene_names, c("GeneA", "GeneB", "GeneC"))
  expect_identical(vi$clusters, c("0", "1", "0", "1"))
  expect_identical(dim(vi$umap_embedding), c(4L, 2L))
  expect_identical(vi$velocity_source, "loom")
  expect_identical(vi$orientation, "genes_x_cells")
  # métavalidation stricte partagée avec le mode RDS (contrat inchangé)
  expect_silent(validate_velocity_rds_metadata(vi))

  # la chaîne module complète passe : validate + finalize (état valid*)
  # (.vel_validate_and_enrich épingle SES gènes fixture — ici on aligne sur
  # les nôtres, même appel validate_velocity_matrices que le module)
  validated <- validate_velocity_matrices(
    spliced = vi$spliced, unspliced = vi$unspliced, ambiguous = vi$ambiguous,
    seurat_cells = paste0("cell", 1:4), seurat_genes = c("GeneA", "GeneB", "GeneC"),
    orientation = "genes_x_cells", strip_cell_suffix = FALSE,
    strip_gene_version = FALSE, allow_low_overlap = FALSE
  )
  canonical <- finalize_velocity_result(validated, input_mode = "loom")
  expect_match(canonical$status, "^valid")
  assert_velocity_result(canonical)
})

test_that("read_velocity_loom : orientation stockée genes x cells également supportée", {
  skip_if_not(.vel_has_hdf5r, "hdf5r absent")
  path <- .vel_loom_fixture(tempfile(fileext = ".loom"), genes_x_cells = TRUE)
  vi <- read_velocity_loom(path)
  expect_identical(as.numeric(vi$spliced[2, 3]), 8)
  expect_identical(as.numeric(vi$unspliced[2, 3]), 16)
})

test_that("read_velocity_loom : symboles dupliqués rendus uniques avec avertissement", {
  skip_if_not(.vel_has_hdf5r, "hdf5r absent")
  path <- .vel_loom_fixture(tempfile(fileext = ".loom"), dup_genes = TRUE)
  expect_warning(vi <- read_velocity_loom(path), "dupliqués rendus uniques")
  expect_identical(anyDuplicated(vi$gene_names), 0L)
  expect_match(vi$gene_names[4], "^GeneA\\.1$")
})

test_that("read_velocity_loom : erreurs classee invalid_input (fichier/noeud/CellID)", {
  skip_if_not(.vel_has_hdf5r, "hdf5r absent")
  # fichier absent
  e1 <- tryCatch(read_velocity_loom(tempfile("absent_")), error = function(e) e)
  expect_true(inherits(e1, "error") && grepl("introuvable", conditionMessage(e1)))
  # classe du domaine R/sc (distincte de sc_velocity_error, côté modules/)
  expect_s3_class(e1, "velocity_validation_error")

  # noeud layers/unspliced absent
  p2 <- tempfile(fileext = ".loom")
  f <- hdf5r::h5file(p2, mode = "w")
  f$create_group("row_attrs"); f$create_group("col_attrs"); f$create_group("layers")
  f[["layers/spliced"]] <- matrix(1L, 1, 1)
  f[["row_attrs/Gene"]] <- "G1"; f[["col_attrs/CellID"]] <- "c1"; f$close_all()
  e2 <- tryCatch(read_velocity_loom(p2), error = function(e) e)
  expect_match(conditionMessage(e2), "layers/unspliced", fixed = TRUE)

  # CellID dupliqués — clé d'alignement : refus (jamais renommés en silence)
  p3 <- tempfile(fileext = ".loom")
  f <- hdf5r::h5file(p3, mode = "w")
  f$create_group("row_attrs"); f$create_group("col_attrs"); f$create_group("layers")
  f[["layers/spliced"]] <- matrix(1:2, 1, 2)
  f[["layers/unspliced"]] <- matrix(1:2, 1, 2)
  f[["row_attrs/Gene"]] <- "G1"; f[["col_attrs/CellID"]] <- c("c1", "c1")
  f$close_all()
  e3 <- tryCatch(read_velocity_loom(p3), error = function(e) e)
  expect_match(conditionMessage(e3), "CellID dupliques", fixed = TRUE)
})

test_that("read_velocity_h5ad : CSR (cells en lignes) -> genes x cells, compound obs/var", {
  skip_if_not(.vel_has_hdf5r, "hdf5r absent")
  path <- .vel_h5ad_fixture(tempfile(fileext = ".h5ad"))
  vi <- read_velocity_h5ad(path)

  expect_s4_class(vi$spliced, "dgCMatrix")
  expect_identical(dim(vi$spliced), c(3L, 4L))
  expect_identical(dimnames(vi$spliced), list(paste0("G", 1:3), paste0("c", 1:4)))
  # vi est (genes x cells) => vi[g, c] = mat[c, g] :
  # mat[1,1]=1 ; mat[3,1]=3 -> vi[1,3] ; mat[4,2]=5 -> vi[2,4]
  expect_identical(as.numeric(vi$spliced[1, 1]), 1)
  expect_identical(as.numeric(vi$spliced[1, 3]), 3)
  expect_identical(as.numeric(vi$spliced[2, 4]), 5)
  expect_identical(as.numeric(vi$unspliced[2, 4]), 10)
  expect_identical(vi$cell_names, paste0("c", 1:4))
  expect_identical(vi$clusters, c("1", "1", "2", "2"))
  expect_identical(dim(vi$umap_embedding), c(4L, 2L))
  expect_silent(validate_velocity_rds_metadata(vi))

  validated <- validate_velocity_matrices(
    spliced = vi$spliced, unspliced = vi$unspliced, ambiguous = NULL,
    seurat_cells = paste0("c", 1:4), seurat_genes = paste0("G", 1:3),
    orientation = "genes_x_cells", strip_cell_suffix = FALSE,
    strip_gene_version = FALSE, allow_low_overlap = FALSE
  )
  canonical <- finalize_velocity_result(validated, input_mode = "loom")
  expect_match(canonical$status, "^valid")
  assert_velocity_result(canonical)
})

test_that("read_velocity_h5ad : couches absentes -> invalid_input classee", {
  skip_if_not(.vel_has_hdf5r, "hdf5r absent")
  p <- tempfile(fileext = ".h5ad")
  f <- hdf5r::h5file(p, mode = "w")
  f$create_group("layers")
  f[["obs"]] <- data.frame(index = paste0("c", 1:2))
  f[["var"]] <- data.frame(index = "G1")
  f$close_all()
  e <- tryCatch(read_velocity_h5ad(p), error = function(e) e)
  expect_s3_class(e, "velocity_validation_error")
  expect_match(conditionMessage(e), "layers/spliced", fixed = TRUE)
})

# Nettoyage : les fixtures temporelles sont dans tempfile() — R nettoie.

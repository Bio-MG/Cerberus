# =============================================================================
# test-core-validation.R — gardes R/core/validation.R (CHRYSALIS 2B)
# =============================================================================
# Couvre la famille assert_* (pass ET fail, messages francais cites avec le
# contexte) + validate_seurat_reduction (native de ce fichier, jamais testee).
# check_design_confounding()/validate_bulk_design() sont deja couvertes par
# tests/testthat/test-bulk-helpers.R (qui source ce fichier canonique) —
# non dupliquees ici volontairement.

source_project_file("tools/pkg_skeleton/R/io_helpers.R")
source_project_file("tools/pkg_skeleton/R/validation.R")

.load_seurat_pkgs <- function() {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("SeuratObject")
  suppressPackageStartupMessages({
    library(SeuratObject)
    library(Seurat)
  })
}

.toy_seurat_obj <- function() {
  counts <- matrix(stats::rpois(20 * 60, lambda = 2), nrow = 20,
                   dimnames = list(paste0("gene", 1:20), paste0("cell", 1:60)))
  obj <- SeuratObject::CreateSeuratObject(counts = counts)
  obj$group <- factor(rep(c("A", "B"), length.out = 60))
  # Reduction synthetique deterministe (evite RunPCA et sa variabilite)
  emb <- matrix(stats::rnorm(120), nrow = 60,
                dimnames = list(paste0("cell", 1:60), c("PC_1", "PC_2")))
  obj[["pca"]] <- SeuratObject::CreateDimReducObject(
    embeddings = emb, key = "PC_", assay = "RNA"
  )
  obj
}

# ---------------------------------------------------------------------------
# assert_seurat()
# ---------------------------------------------------------------------------
test_that("assert_seurat passes and returns the object invisibly", {
  .load_seurat_pkgs()
  obj <- .toy_seurat_obj()
  expect_invisible(assert_seurat(obj))
  expect_identical(assert_seurat(obj), obj)
})

test_that("assert_seurat fails on NULL and non-Seurat with the context cited", {
  expect_error(assert_seurat(NULL, context = "etape trajet"), "etape trajet")
  expect_error(assert_seurat(list(a = 1), context = "etape trajet"), "Seurat")
})

# ---------------------------------------------------------------------------
# assert_assay()
# ---------------------------------------------------------------------------
test_that("assert_assay passes for the default RNA assay", {
  .load_seurat_pkgs()
  obj <- .toy_seurat_obj()
  expect_identical(assert_assay(obj, "RNA"), obj)
})

test_that("assert_assay fails clearly on a missing assay, listing available ones", {
  .load_seurat_pkgs()
  obj <- .toy_seurat_obj()
  expect_error(assert_assay(obj, "SCT", context = "integration"), "'SCT'")
  expect_error(assert_assay(obj, "SCT"), "RNA")
  expect_error(assert_assay(obj, "SCT", context = "integration"), "integration")
})

# ---------------------------------------------------------------------------
# assert_reduction()
# ---------------------------------------------------------------------------
test_that("assert_reduction passes for the synthetic pca reduction", {
  .load_seurat_pkgs()
  obj <- .toy_seurat_obj()
  expect_identical(assert_reduction(obj, "pca"), obj)
})

test_that("assert_reduction fails on a missing reduction, listing available ones", {
  .load_seurat_pkgs()
  obj <- .toy_seurat_obj()
  expect_error(assert_reduction(obj, "umap"), "'umap'")
  expect_error(assert_reduction(obj, "umap"), "pca")
})

# ---------------------------------------------------------------------------
# assert_cells()
# ---------------------------------------------------------------------------
test_that("assert_cells passes when all ids exist", {
  .load_seurat_pkgs()
  obj <- .toy_seurat_obj()
  expect_identical(assert_cells(obj, c("cell1", "cell2", "cell60")), obj)
})

test_that("assert_cells fails citing the count and an example of missing ids", {
  .load_seurat_pkgs()
  obj <- .toy_seurat_obj()
  expect_error(assert_cells(obj, c("cell1", "ghost_cell")), "ghost_cell")
  expect_error(assert_cells(obj, "ghost_cell", context = "subset viz"), "subset viz")
})

# ---------------------------------------------------------------------------
# assert_metadata_column()
# ---------------------------------------------------------------------------
test_that("assert_metadata_column passes for an existing column", {
  .load_seurat_pkgs()
  obj <- .toy_seurat_obj()
  expect_identical(assert_metadata_column(obj, "group"), obj)
  expect_identical(assert_metadata_column(obj, "orig.ident"), obj)
})

test_that("assert_metadata_column fails on a missing column", {
  .load_seurat_pkgs()
  obj <- .toy_seurat_obj()
  expect_error(assert_metadata_column(obj, "nope", context = "palette sc"), "nope")
  expect_error(assert_metadata_column(obj, "nope"), "group")
})

# ---------------------------------------------------------------------------
# assert_numeric_matrix()
# ---------------------------------------------------------------------------
test_that("assert_numeric_matrix passes and returns the matrix invisibly", {
  m <- matrix(1:4, nrow = 2)
  expect_invisible(assert_numeric_matrix(m))
  expect_identical(assert_numeric_matrix(m, context = "velocity"), m)
  expect_identical(assert_numeric_matrix(matrix(c(1.5, 2.5), ncol = 1)), 
                   matrix(c(1.5, 2.5), ncol = 1))
})

test_that("assert_numeric_matrix fails on NULL, data.frame and character matrices", {
  expect_error(assert_numeric_matrix(NULL), "NULL")
  expect_error(assert_numeric_matrix(data.frame(x = 1:2)), "data.frame")
  expect_error(assert_numeric_matrix(matrix(c("a", "b"), ncol = 1)), "numerique")
  expect_error(assert_numeric_matrix(data.frame(x = 1), context = "bulk import"),
               "bulk import")
})

# ---------------------------------------------------------------------------
# assert_sample_mapping()
# ---------------------------------------------------------------------------
test_that("assert_sample_mapping passes for a clean metadata table", {
  meta <- data.frame(condition = c("A", "A", "B", "B"),
                     batch = c("b1", "b2", "b1", "b2"), row.names = paste0("S", 1:4))
  expect_invisible(assert_sample_mapping(meta, "condition"))
  expect_identical(assert_sample_mapping(meta, "batch"), meta)
})

test_that("assert_sample_mapping fails on non-data.frame, missing column and NAs", {
  meta <- data.frame(condition = c("A", "B", NA, "B"))
  expect_error(assert_sample_mapping(list(), "condition"), "data.frame")
  expect_error(assert_sample_mapping(meta, "batch"), "'batch'")
  expect_error(assert_sample_mapping(meta, "condition"), "1 valeur")
  expect_error(assert_sample_mapping(meta, "condition", context = "deseq2 design"),
               "deseq2 design")
})

# ---------------------------------------------------------------------------
# validate_seurat_reduction() — native de validation.R (pre-existante)
# ---------------------------------------------------------------------------
test_that("validate_seurat_reduction returns TRUE invisibly on success", {
  .load_seurat_pkgs()
  obj <- .toy_seurat_obj()
  expect_invisible(validate_seurat_reduction(obj, "pca"))
  expect_true(validate_seurat_reduction(obj, "pca"))
})

test_that("validate_seurat_reduction keeps its historical English messages", {
  expect_error(validate_seurat_reduction(NULL, "umap"), "valid Seurat object")
  .load_seurat_pkgs()
  obj <- .toy_seurat_obj()
  expect_error(validate_seurat_reduction(obj, "tsne"), "not found")
})

# =============================================================================
# test-rda-spatial-bulk.R — Intégrations .rda Commit 3 : référence spatiale
# (R/spatial/spatial_reference.R::read_reference_scrna) + slots Bulk
# =============================================================================
# read_reference_scrna : objet unique .rda accepté (Seurat / liste counts+meta
# / matrice) ; workspace multi-objets -> erreur française orientante.
# =============================================================================

suppressPackageStartupMessages({
  library(Seurat)
})
source_project_file("tools/pkg_skeleton/R/io_helpers.R")
source_project_file("R/core/rdata_io.R")
source_project_file("R/spatial/spatial_reference.R")

.tmpdir <- tempfile(pattern = "rda_spatial_")
dir.create(.tmpdir, showWarnings = FALSE)

ref_mat <- matrix(1:20, 4, 5, dimnames = list(paste0("g", 1:4), paste0("c", 1:5)))
single_mat_path <- file.path(.tmpdir, "ref_mat.rda")
save(ref_mat, file = single_mat_path)

fake_ref_seurat <- structure(list(assays = list()), class = c("Seurat", "S3"))
single_seurat_path <- file.path(.tmpdir, "ref_seurat.rda")
save(fake_ref_seurat, file = single_seurat_path)

ref_list <- list(counts = ref_mat, meta = data.frame(type = c("A", "B")))
single_list_path <- file.path(.tmpdir, "ref_list.rda")
save(ref_list, file = single_list_path)

bmeta <- data.frame(x = 1)
multi_path <- file.path(.tmpdir, "multi.rda")
save(ref_mat, bmeta, file = multi_path)

# ---------------------------------------------------------------------------
# read_reference_scrna — branche .rda
# ---------------------------------------------------------------------------
test_that("read_reference_scrna(.rda) accepte un objet unique exploitable", {
  res <- read_reference_scrna(single_seurat_path)
  expect_true(inherits(res, "Seurat"))

  res_mat <- read_reference_scrna(single_mat_path)
  expect_identical(res_mat$counts, ref_mat)
  expect_null(res_mat$meta)

  res_list <- read_reference_scrna(single_list_path)
  expect_identical(res_list$counts, ref_mat)
  expect_identical(res_list$meta$type, c("A", "B"))
})

test_that("read_reference_scrna(.rda) refuse un workspace multi-objets avec message orientant", {
  err <- tryCatch(read_reference_scrna(multi_path), error = function(e) e)
  expect_match(conditionMessage(err), "2 objets", fixed = TRUE)
  expect_match(conditionMessage(err), "Exporter la sélection", fixed = TRUE)
})

test_that("prepare_reference_seurat consomme le résultat .rda (matrice -> Seurat)", {
  raw <- read_reference_scrna(single_mat_path)
  obj <- prepare_reference_seurat(raw, project_name = "TestRef")
  expect_true(inherits(obj, "Seurat"))
})

unlink(.tmpdir, recursive = TRUE)

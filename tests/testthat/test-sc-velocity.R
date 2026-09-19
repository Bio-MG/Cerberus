# Tests for R/sc/sc_velocity.R
source_project_file("R/sc/sc_velocity.R")

test_that("detect_velocity_orientation detects genes_x_cells", {
  skip_if_not_installed("Matrix")
  mat <- Matrix::Matrix(0, nrow = 10, ncol = 5, sparse = TRUE)
  rownames(mat) <- paste0("gene", 1:10)
  colnames(mat) <- paste0("cell", 1:5)
  result <- detect_velocity_orientation(mat,
    seurat_cells = paste0("cell", 1:5),
    seurat_genes = paste0("gene", 1:10),
    orientation = "auto_strict")
  expect_equal(result, "genes_x_cells")
})

test_that("normalize_velocity_cell_barcodes strips suffix", {
  ids <- c("AAAC-1", "AAAG-1", "AAAT-1")
  result <- normalize_velocity_cell_barcodes(ids, strip_suffix = TRUE)
  expect_equal(result, c("AAAC", "AAAG", "AAAT"))
})

test_that("normalize_velocity_cell_barcodes detects collisions", {
  ids <- c("AAAC-1", "AAAC-2")
  expect_error(normalize_velocity_cell_barcodes(ids, strip_suffix = TRUE),
               "collision")
})

# ---------------------------------------------------------------------------
# build_velocity_provenance_export() : garde « gene absent » (chantier C10)
# ---------------------------------------------------------------------------
# Le site historique etait un stop() NU a DEUX arguments :
#     stop("Gene absent des matrices velocity : ", gene)
# Il est desormais route par .velocity_stop() : classe contractuelle
# velocity_validation_error + state "invalid_input" (VELOCITY_RESULT_CONTRACT.md
# « Fatal states »). Le message etant reconstruit par paste0(), la QUEUE du
# message est assertee : c'est le detecteur de troncature (errorCondition()
# n'AGREGE PAS ses arguments — regle C16).

.vel_prov_result <- function() {
  list(spliced = .vel_mat(), unspliced = .vel_mat() * 2L)
}

test_that("build_velocity_provenance_export : temoin (gene present)", {
  skip_if_not_installed("Matrix")
  df <- build_velocity_provenance_export(.vel_prov_result(), "gene3")
  expect_s3_class(df, "data.frame")
  expect_identical(nrow(df), length(.vel_cells))
  expect_true(all(df$gene == "gene3"))
})

test_that("build_velocity_provenance_export : gene absent => classe contractuelle", {
  skip_if_not_installed("Matrix")
  e <- tryCatch(
    build_velocity_provenance_export(.vel_prov_result(), "gene_absent"),
    error = function(e) e
  )
  expect_s3_class(e, "velocity_validation_error")
  expect_s3_class(e, "error")
  expect_identical(velocity_error_state(e), "invalid_input")
  # Queue du message = le gene : detecteur de troncature (C16).
  expect_true(grepl("gene_absent$", conditionMessage(e)))
})

test_that("build_velocity_provenance_export : la garde est CONDITIONNELLE", {
  skip_if_not_installed("Matrix")
  res <- .vel_prov_result()
  expect_no_error(
    build_velocity_provenance_export(res, rownames(res$spliced)[1L])
  )
  expect_error(build_velocity_provenance_export(res, "absent_xyz"))
})

test_that("R/sc/sc_velocity.R ne porte plus de stop() non classe (C10)", {
  expect_identical(ts_c10_sites("R/sc/sc_velocity.R"), integer(0))
})

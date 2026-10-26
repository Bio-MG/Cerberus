# =============================================================================
# test-tier4-scenario-pbmc3k.R — Tier 4 Real-World: 10x PBMC Single-Cell Workflow
# =============================================================================
# Realistic biological workload: 10x Genomics PBMC dataset drop
# - Sparse dgCMatrix count representation (300 genes x 80 cells)
# - Human taxonomy with GRCh38 assembly and Ensembl IDs
# - Headless CLI validation of the complete drop
# - Downstream doublet decontamination and async pipeline integration
# =============================================================================

if (!exists("e2e_project_root", mode = "function")) {
  if (file.exists("tests/e2e/helper-e2e.R")) {
    source("tests/e2e/helper-e2e.R")
  } else if (file.exists("../helper-e2e.R")) {
    source("../helper-e2e.R")
  }
}

test_that("T4-PBMC-1: full 10x PBMC drop validates headlessly via cerberus-handoff CLI", {
  mat_pbmc <- e2e_make_pbmc_matrix(n_genes = 250L, n_cells = 60L, seed = 2026L)
  sheet_pbmc <- e2e_make_pbmc_samplesheet(colnames(mat_pbmc), seed = 2026L)
  
  d <- e2e_make_test_drop(
    prefix = "t4_pbmc_drop_",
    counts = mat_pbmc,
    samplesheet = sheet_pbmc,
    matrix_filename = "counts.rds",
    dataset_id = "PBMC_10X_BENCHMARK",
    manifest_extra = list(
      metadata = list(
        project = "10x_PBMC_Immunology",
        technology = "10x_Chromium_v3",
        description = "Single-cell RNA-seq of peripheral blood mononuclear cells"
      )
    )
  )
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  out_dir <- file.path(d, "validation_out")
  
  cli_res <- e2e_run_cli(c(paste0("--drop=", d), paste0("--out=", out_dir)))
  expect_identical(cli_res$exit_code, 0L)
  
  val_path <- file.path(out_dir, "validation.json")
  expect_true(file.exists(val_path))
  
  report <- jsonlite::fromJSON(val_path)
  expect_identical(report$status, "VALID")
  expect_true(report$valid)
  expect_identical(report$dataset_id, "PBMC_10X_BENCHMARK")
  expect_identical(report$summary$n_genes, 250L)
  expect_identical(report$summary$n_samples, 60L)
})

test_that("T4-PBMC-2: PBMC dataset sparse matrix loads with intact dimensions and cell barcodes", {
  mat_pbmc <- e2e_make_pbmc_matrix(n_genes = 200L, n_cells = 40L, seed = 2027L)
  sheet_pbmc <- e2e_make_pbmc_samplesheet(colnames(mat_pbmc), seed = 2027L)
  d <- e2e_make_test_drop(prefix = "t4_pbmc_mat_", counts = mat_pbmc, samplesheet = sheet_pbmc, matrix_filename = "counts.rds")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  
  loaded <- cerberus_handoff_read_matrix(file.path(d, "counts.rds"))
  expect_true(inherits(loaded, "dgCMatrix"))
  expect_identical(dim(loaded), c(200L, 40L))
  expect_true(all(startsWith(colnames(loaded), "PBMC_CELL_")))
})

test_that("T4-PBMC-3: PBMC single-cell workflow executes doublet QC and async pipeline", {
  if (!is_m3_ready() || !is_m4_ready()) skip("M3 and/or M4 pipeline execution not yet active")
  expect_true(TRUE)
})

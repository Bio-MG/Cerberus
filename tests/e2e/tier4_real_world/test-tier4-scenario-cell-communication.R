# =============================================================================
# test-tier4-scenario-cell-communication.R — Tier 4 Real-World: CCC Workflow
# =============================================================================
# Realistic biological workload: Cell-Cell Communication (CellChat & LIANA)
# - Drop manifest declaring cell_cell_communication readiness block
# - Samplesheet carrying population_column cell type annotations
# - End-to-end CLI validation and metadata contract preservation
# =============================================================================

if (!exists("e2e_project_root", mode = "function")) {
  if (file.exists("tests/e2e/helper-e2e.R")) {
    source("tests/e2e/helper-e2e.R")
  } else if (file.exists("../helper-e2e.R")) {
    source("../helper-e2e.R")
  }
}

test_that("T4-CCC-1: drop with cell_cell_communication block validates successfully via CLI", {
  mat_pbmc <- e2e_make_pbmc_matrix(n_genes = 200L, n_cells = 50L, seed = 3001L)
  sheet_pbmc <- e2e_make_pbmc_samplesheet(colnames(mat_pbmc), seed = 3001L)
  
  ccc_config <- list(
    cellchat_ready = TRUE,
    liana_ready = TRUE,
    population_column = "cell_type"
  )
  
  d <- e2e_make_test_drop(
    prefix = "t4_ccc_drop_",
    counts = mat_pbmc,
    samplesheet = sheet_pbmc,
    matrix_filename = "counts.rds",
    dataset_id = "CCC_INTERACTION_DROP",
    manifest_extra = list(cell_cell_communication = ccc_config)
  )
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  out_dir <- file.path(d, "out")
  
  cli_res <- e2e_run_cli(c(paste0("--drop=", d), paste0("--out=", out_dir)))
  expect_identical(cli_res$exit_code, 0L)
  
  report <- jsonlite::fromJSON(file.path(out_dir, "validation.json"))
  expect_identical(report$status, "VALID")
  expect_true(report$valid)
})

test_that("T4-CCC-2: population column specified in manifest exists in samplesheet", {
  mat_pbmc <- e2e_make_pbmc_matrix(n_genes = 150L, n_cells = 30L, seed = 3002L)
  sheet_pbmc <- e2e_make_pbmc_samplesheet(colnames(mat_pbmc), seed = 3002L)
  d <- e2e_make_test_drop(
    prefix = "t4_ccc_pop_",
    counts = mat_pbmc,
    samplesheet = sheet_pbmc,
    matrix_filename = "counts.rds",
    manifest_extra = list(
      cell_cell_communication = list(population_column = "cell_type")
    )
  )
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  
  sheet <- cerberus_handoff_read_samplesheet(file.path(d, "samplesheet.tsv"))
  expect_true("cell_type" %in% colnames(sheet))
  expect_true(length(unique(sheet$cell_type)) >= 2L)
})

test_that("T4-CCC-3: cell-cell communication downstream routing preserves population annotations", {
  if (!is_m3_ready()) skip("M3 pipeline integration not yet active")
  # M5 : le cœur async PRÉSERVE les annotations de population du meta.data
  # d'entrée (clé de voûte M6 : les colonnes de design survivent au pipeline —
  # sinon CCC/DA perdent leurs identités).
  set.seed(11)
  m <- matrix(rpois(120 * 60, lambda = 3), nrow = 120, ncol = 60)
  rownames(m) <- paste0("Gene-", seq_len(120))
  colnames(m) <- paste0("Cell-", seq_len(60))
  obj <- suppressWarnings(Seurat::CreateSeuratObject(counts = m))
  obj$population <- factor(rep(c("Tcell", "Bcell", "Mono"), length.out = 60))
  obj$disease_stage <- "baseline"
  res <- run_sc_auto_pipeline_core(obj, params = list(sc_ap_res = 0.5))
  for (col in c("population", "disease_stage", "orig.ident")) {
    expect_true(col %in% colnames(res$obj@meta.data),
                info = paste("colonne de population perdue :", col))
  }
  expect_true(!is.null(res$timings) && is.list(res$timings))
})

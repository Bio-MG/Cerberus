# =============================================================================
# test-tier4-scenario-multisample-batch.R — Tier 4 Real-World: Multi-Sample Batch
# =============================================================================
# Realistic biological workload: Multi-sample case-control study with batch
# - 4 biological samples across 2 conditions (Control vs Treated)
# - Unconfounded batch and donor covariates
# - Strict CLI validation under --strict flag
# - Invariant verification: replicates >= 2 and zero design confounding
# =============================================================================

if (!exists("e2e_project_root", mode = "function")) {
  if (file.exists("tests/e2e/helper-e2e.R")) {
    source("tests/e2e/helper-e2e.R")
  } else if (file.exists("../helper-e2e.R")) {
    source("../helper-e2e.R")
  }
}

test_that("T4-MSB-1: multi-sample batch drop passes strict headless CLI validation", {
  s_ids <- paste0("sample_", seq_len(4L))
  set.seed(4001L)
  counts <- matrix(as.integer(rpois(150L * 4L, lambda = 400L)),
                   nrow = 150L, ncol = 4L,
                   dimnames = list(sprintf("ENSG%011d", seq_len(150L)), s_ids))
  
  sheet <- data.frame(
    sample = s_ids,
    condition = c("control", "control", "treated", "treated"),
    batch = c("batch_1", "batch_2", "batch_1", "batch_2"), # Orthogonal to condition!
    donor = c("d1", "d2", "d1", "d2"),                     # Repeated across conditions!
    stringsAsFactors = FALSE
  )
  
  d <- e2e_make_test_drop(
    prefix = "t4_msb_drop_",
    counts = counts,
    samplesheet = sheet,
    dataset_id = "MULTISAMPLE_BATCH_COHORT"
  )
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  out_dir <- file.path(d, "out")
  
  # Must pass even in strict mode because batch is orthogonal
  cli_res <- e2e_run_cli(c(paste0("--drop=", d), paste0("--out=", out_dir), "--strict"))
  expect_identical(cli_res$exit_code, 0L)
  
  val_path <- file.path(out_dir, "validation.json")
  expect_true(file.exists(val_path))
  
  report <- jsonlite::fromJSON(val_path)
  expect_identical(report$status, "VALID")
  expect_identical(report$summary$n_errors, 0L)
  expect_identical(report$summary$n_warnings, 0L)
})

test_that("T4-MSB-2: multi-sample design invariants guarantee differential expression power", {
  s_ids <- paste0("sample_", seq_len(4L))
  sheet <- data.frame(
    sample = s_ids,
    condition = c("control", "control", "treated", "treated"),
    batch = c("b1", "b2", "b1", "b2"),
    stringsAsFactors = FALSE
  )
  res <- cerberus_handoff_validate_samplesheet(sheet, mat_colnames = s_ids)
  expect_true(res$valid)
  expect_identical(length(res$conditions), 2L)
  expect_identical(length(res$confounded_covariates), 0L)
})

test_that("T4-MSB-3: downstream multi-sample batch-aware doublet filtering and correction", {
  if (!is_m4_ready()) skip("M4 batch-aware doublet QC not yet active")
  # M5 : le moteur M4 consomme batch_var réel (2 lots orig.ident) — le
  # paramètre est écho en provenance et l'objet downstream-ready rendu
  # (F19-5 côté mono ; ici le scénario multi-échantillons de bout en bout).
  set.seed(13)
  m <- matrix(rpois(120 * 80, lambda = 3), nrow = 120, ncol = 80)
  rownames(m) <- paste0("Gene-", seq_len(120))
  colnames(m) <- paste0("Cell-", seq_len(80))
  obj <- suppressWarnings(Seurat::CreateSeuratObject(counts = m))
  obj$orig.ident <- factor(rep(c("SampleA", "SampleB"), length.out = 80))
  res <- sc_filter_doublets_and_ambient(obj, params = list(batch_var = "orig.ident",
                                                           seed = 42))
  expect_identical(res$provenance$parameters$batch_var, "orig.ident")
  expect_equal(length(as.character(res$provenance$cells_used)), 80L)
  expect_s4_class(res$obj, "Seurat")
})

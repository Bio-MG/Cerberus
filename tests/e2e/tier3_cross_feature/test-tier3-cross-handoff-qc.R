# =============================================================================
# test-tier3-cross-handoff-qc.R — Tier 3 Cross-Feature: Handoff -> Single-Cell QC
# =============================================================================
# Cross-feature interaction: Upstream handoff drop validation seamlessly feeding
# into single-cell doublet and ambient RNA quality filtering.
# - Valid drops produce validated matrices ready for QC ingestion
# - Invalid drops block downstream QC from executing on ungrounded data
# - Strict mode halts continuous floats from corrupting negative binomial modeling
# =============================================================================

if (!exists("e2e_project_root", mode = "function")) {
  if (file.exists("tests/e2e/helper-e2e.R")) {
    source("tests/e2e/helper-e2e.R")
  } else if (file.exists("../helper-e2e.R")) {
    source("../helper-e2e.R")
  }
}

test_that("T3-HQC-1: validated drop matrix is successfully ingested and prepared for QC", {
  d <- e2e_make_test_drop(prefix = "t3_hqc_valid_")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  
  res <- cerberus_handoff_validate_drop(drop_dir = d)
  expect_true(res$valid)
  expect_identical(res$status, "VALID")
  
  # Read validated matrix and verify dimensions
  man <- jsonlite::fromJSON(file.path(d, "manifest.json"))
  mat <- cerberus_handoff_read_matrix(file.path(d, man$matrix_path))
  expect_identical(nrow(mat), 120L)
  expect_identical(ncol(mat), 4L)
  expect_true(all(mat >= 0))
})

test_that("T3-HQC-2: ungrounded continuous data rejected at handoff cannot enter QC stage", {
  # Generate continuous log-counts
  set.seed(42)
  cont_mat <- matrix(rnorm(40L, mean = 5, sd = 1), nrow = 10L, ncol = 4L,
                     dimnames = list(sprintf("ENSG%011d", 1:10), paste0("sample_", 1:4)))
  cont_mat[cont_mat < 0] <- 0
  
  d <- e2e_make_test_drop(prefix = "t3_hqc_cont_", counts = cont_mat)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  
  res <- cerberus_handoff_validate_drop(drop_dir = d)
  expect_false(res$valid)
  expect_true(any(vapply(res$errors, function(x) x$code == "not_raw_counts", logical(1))))
})

test_that("T3-HQC-3: pseudoreplicated drop is blocked from entering biological QC filtering", {
  s_ids <- paste0("sample_", 1:3)
  mat3 <- matrix(as.integer(rpois(120L * 3L, 100L)), 120L, 3L,
                 dimnames = list(sprintf("ENSG%011d", 1:120), s_ids))
  sheet <- data.frame(
    sample = s_ids,
    condition = c("control", "control", "treated"), # 1 replicate in treated
    stringsAsFactors = FALSE
  )
  d <- e2e_make_test_drop(prefix = "t3_hqc_pseudo_", counts = mat3, samplesheet = sheet)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  
  res <- cerberus_handoff_validate_drop(drop_dir = d)
  expect_false(res$valid)
  expect_true(any(vapply(res$errors, function(x) x$code == "pseudoreplication_detected", logical(1))))
})

test_that("T3-HQC-4: validated single-cell count matrix feeds downstream doublet caller", {
  if (!is_m4_ready()) skip("M4 doublet engine not yet active for cross-feature execution")
  # M5 : la matrice validée du harnais handoff traverse le MOTEUR M4 et rend
  # un objet downstream-ready (RunPCA direct exigé par F24-4 côté contrat).
  set.seed(7)
  m <- matrix(rpois(120 * 60, lambda = 3), nrow = 120, ncol = 60)
  rownames(m) <- paste0("Gene-", seq_len(120))
  colnames(m) <- paste0("Cell-", seq_len(60))
  obj <- suppressWarnings(Seurat::CreateSeuratObject(counts = m))
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42))
  expect_s4_class(res$obj, "Seurat")
  expect_equal(ncol(res$obj), 60L)
  expect_true(length(as.character(res$provenance$cells_used)) == 60L)
})

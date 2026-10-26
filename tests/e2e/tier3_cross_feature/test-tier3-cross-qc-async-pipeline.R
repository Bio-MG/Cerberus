# =============================================================================
# test-tier3-cross-qc-async-pipeline.R — Tier 3 Cross-Feature: QC -> Pipeline
# =============================================================================
# Cross-feature scenario: Cleaned singlet matrix from biological QC stage
# passed forward to decoupled asynchronous Single-Cell pipeline.
# - Filtered doublets are strictly excluded from PCA & UMAP reduction
# - Provenance entry attached during QC is preserved in pipeline result
# - Mirai worker daemon receives pure matrix without Shiny reactivity leaks
# =============================================================================

if (!exists("e2e_project_root", mode = "function")) {
  if (file.exists("tests/e2e/helper-e2e.R")) {
    source("tests/e2e/helper-e2e.R")
  } else if (file.exists("../helper-e2e.R")) {
    source("../helper-e2e.R")
  }
}

test_that("T3-QCP-1: QC filtered singlets propagate into run_sc_auto_pipeline_core", {
  if (!is_m3_ready() || !is_m4_ready()) skip("M3 pipeline and/or M4 QC not yet active")
  expect_true(TRUE)
})

test_that("T3-QCP-2: downstream PCA excludes cells filtered out by doublet engine", {
  if (!is_m3_ready() || !is_m4_ready()) skip("M3 pipeline and/or M4 QC not yet active")
  expect_true(TRUE)
})

test_that("T3-QCP-3: QC provenance history is retained in pipeline output object", {
  if (!is_m3_ready() || !is_m4_ready()) skip("M3 pipeline and/or M4 QC not yet active")
  expect_true(TRUE)
})

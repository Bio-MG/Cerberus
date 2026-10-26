# =============================================================================
# test-tier2-boundaries-metadata-design.R — Tier 2 Boundary: Metadata & Design
# =============================================================================
# Adversarial experimental design and metadata boundary conditions:
# - Single experimental condition (no contrast available)
# - Groups with single replicate (< TS_DA_MIN_REPLICATES_PER_CONDITION)
# - Duplicate sample IDs in samplesheet
# - Blank, whitespace, or NA sample IDs
# - Asymmetric sample sets between matrix and samplesheet
# - Multiple covariates with partial vs complete confounding
# =============================================================================

if (!exists("e2e_project_root", mode = "function")) {
  if (file.exists("tests/e2e/helper-e2e.R")) {
    source("tests/e2e/helper-e2e.R")
  } else if (file.exists("../helper-e2e.R")) {
    source("../helper-e2e.R")
  }
}

test_that("T2-DES-1: single condition experimental design triggers pseudoreplication_detected", {
  sheet <- data.frame(
    sample = paste0("s", 1:4),
    condition = rep("control", 4L), # Only 1 distinct condition!
    stringsAsFactors = FALSE
  )
  err <- tryCatch(
    cerberus_handoff_validate_samplesheet(sheet, mat_colnames = paste0("s", 1:4)),
    cerberus_handoff_error = function(e) e
  )
  expect_true(inherits(err, "cerberus_handoff_error"))
  expect_identical(err$state, "pseudoreplication_detected")
})

test_that("T2-DES-2: one condition with 10 replicates and second condition with 1 replicate triggers pseudoreplication", {
  samples <- paste0("s", 1:11)
  sheet <- data.frame(
    sample = samples,
    condition = c(rep("control", 10L), "treated"), # 'treated' has only 1 replicate (< 2L)
    stringsAsFactors = FALSE
  )
  err <- tryCatch(
    cerberus_handoff_validate_samplesheet(sheet, mat_colnames = samples),
    cerberus_handoff_error = function(e) e
  )
  expect_true(inherits(err, "cerberus_handoff_error"))
  expect_identical(err$state, "pseudoreplication_detected")
})

test_that("T2-DES-3: duplicate sample IDs in samplesheet trigger samplesheet_matrix_mismatch", {
  sheet <- data.frame(
    sample = c("s1", "s1", "s2", "s3"), # 's1' duplicated
    condition = c("c1", "c1", "c2", "c2"),
    stringsAsFactors = FALSE
  )
  err <- tryCatch(
    cerberus_handoff_validate_samplesheet(sheet, mat_colnames = c("s1", "s2", "s3", "s4")),
    cerberus_handoff_error = function(e) e
  )
  expect_true(inherits(err, "cerberus_handoff_error"))
  expect_identical(err$state, "samplesheet_matrix_mismatch")
})

test_that("T2-DES-4: whitespace-only sample ID triggers samplesheet_matrix_mismatch", {
  sheet <- data.frame(
    sample = c("s1", "   ", "s2", "s3"),
    condition = c("c1", "c1", "c2", "c2"),
    stringsAsFactors = FALSE
  )
  err <- tryCatch(
    cerberus_handoff_validate_samplesheet(sheet, mat_colnames = c("s1", "s2", "s3", "s4")),
    cerberus_handoff_error = function(e) e
  )
  expect_true(inherits(err, "cerberus_handoff_error"))
  expect_identical(err$state, "samplesheet_matrix_mismatch")
})

test_that("T2-DES-5: sample in matrix but omitted from samplesheet triggers samplesheet_matrix_mismatch", {
  sheet <- data.frame(
    sample = c("s1", "s2", "s3"), # 3 samples in sheet
    condition = c("c1", "c1", "c2"),
    stringsAsFactors = FALSE
  )
  err <- tryCatch(
    cerberus_handoff_validate_samplesheet(sheet, mat_colnames = c("s1", "s2", "s3", "s4")), # 4 in matrix
    cerberus_handoff_error = function(e) e
  )
  expect_true(inherits(err, "cerberus_handoff_error"))
  expect_identical(err$state, "samplesheet_matrix_mismatch")
})

test_that("T2-DES-6: multiple covariates isolate exactly the confounded covariate", {
  sheet <- data.frame(
    sample = paste0("s", 1:4),
    condition = c("c1", "c1", "c2", "c2"),
    batch = c("b1", "b1", "b2", "b2"),     # Confounded 100%
    donor = c("d1", "d2", "d1", "d2"),     # Not confounded (orthogonal)
    stringsAsFactors = FALSE
  )
  res <- cerberus_handoff_validate_samplesheet(sheet, mat_colnames = paste0("s", 1:4))
  expect_true(res$valid)
  expect_identical(res$confounded_covariates, "batch")
})

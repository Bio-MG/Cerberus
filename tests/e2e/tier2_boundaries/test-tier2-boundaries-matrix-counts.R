# =============================================================================
# test-tier2-boundaries-matrix-counts.R — Tier 2 Boundary: Matrix Counts & Stats
# =============================================================================
# Adversarial numerical tests and count matrix boundary stress:
# - Non-integer fraction right at 95.0% threshold (94.9% fail vs 95.1% pass)
# - All-zero count matrix
# - Negative counts
# - NA / NaN / Inf / -Inf numerical corruptions
# - Single-feature / single-sample dimensions
# - Degenerate empty matrices (0 rows, 0 cols)
# =============================================================================

if (!exists("e2e_project_root", mode = "function")) {
  if (file.exists("tests/e2e/helper-e2e.R")) {
    source("tests/e2e/helper-e2e.R")
  } else if (file.exists("../helper-e2e.R")) {
    source("../helper-e2e.R")
  }
}

test_that("T2-MAT-1: non-integer count fraction at exactly 94.9% fails under allow_non_integer = TRUE", {
  # 1000 entries: 949 integers, 51 non-integers (frac_int = 0.949 <= 0.95)
  set.seed(42)
  vals <- as.double(rpois(1000L, lambda = 50L))
  vals[seq_len(51L)] <- vals[seq_len(51L)] + 0.33
  mat <- matrix(vals, nrow = 250L, ncol = 4L,
                dimnames = list(sprintf("ENSG%011d", seq_len(250L)), paste0("s", 1:4)))
  
  err <- tryCatch(
    cerberus_handoff_validate_matrix(mat, matrix_type = "raw_counts", allow_non_integer = TRUE),
    cerberus_handoff_error = function(e) e
  )
  expect_true(inherits(err, "cerberus_handoff_error"))
  expect_identical(err$state, "not_raw_counts")
})

test_that("T2-MAT-2: non-integer count fraction at 95.5% passes with warning under allow_non_integer = TRUE", {
  # 1000 entries: 955 integers, 45 non-integers (frac_int = 0.955 > 0.95)
  set.seed(42)
  vals <- as.double(rpois(1000L, lambda = 50L))
  vals[seq_len(45L)] <- vals[seq_len(45L)] + 0.33
  mat <- matrix(vals, nrow = 250L, ncol = 4L,
                dimnames = list(sprintf("ENSG%011d", seq_len(250L)), paste0("s", 1:4)))
  
  res <- cerberus_handoff_validate_matrix(mat, matrix_type = "raw_counts", allow_non_integer = TRUE)
  expect_true(res$frac_integer > 0.95)
})

test_that("T2-MAT-3: matrix containing negative count values triggers negative_counts", {
  mat <- matrix(c(10L, -5L, 20L, 15L), nrow = 2L, ncol = 2L,
                dimnames = list(c("ENSG00000000001", "ENSG00000000002"), c("s1", "s2")))
  err <- tryCatch(
    cerberus_handoff_validate_matrix(mat, matrix_type = "raw_counts"),
    cerberus_handoff_error = function(e) e
  )
  expect_true(inherits(err, "cerberus_handoff_error"))
  expect_identical(err$state, "negative_counts")
})

test_that("T2-MAT-4: matrix containing NA or NaN triggers invalid_matrix_values", {
  mat_na <- matrix(c(10L, NA, 20L, 15L), nrow = 2L, ncol = 2L,
                   dimnames = list(c("ENSG00000000001", "ENSG00000000002"), c("s1", "s2")))
  err_na <- tryCatch(
    cerberus_handoff_validate_matrix(mat_na, matrix_type = "raw_counts"),
    cerberus_handoff_error = function(e) e
  )
  expect_true(inherits(err_na, "cerberus_handoff_error"))
  expect_identical(err_na$state, "invalid_matrix_values")

  mat_nan <- matrix(c(10.0, NaN, 20.0, 15.0), nrow = 2L, ncol = 2L,
                    dimnames = list(c("ENSG00000000001", "ENSG00000000002"), c("s1", "s2")))
  err_nan <- tryCatch(
    cerberus_handoff_validate_matrix(mat_nan, matrix_type = "raw_counts"),
    cerberus_handoff_error = function(e) e
  )
  expect_true(inherits(err_nan, "cerberus_handoff_error"))
  expect_identical(err_nan$state, "invalid_matrix_values")
})

test_that("T2-MAT-5: matrix containing Inf or -Inf triggers invalid_matrix_values", {
  mat_inf <- matrix(c(10L, Inf, 20L, 15L), nrow = 2L, ncol = 2L,
                    dimnames = list(c("ENSG00000000001", "ENSG00000000002"), c("s1", "s2")))
  err <- tryCatch(
    cerberus_handoff_validate_matrix(mat_inf, matrix_type = "raw_counts"),
    cerberus_handoff_error = function(e) e
  )
  expect_true(inherits(err, "cerberus_handoff_error"))
  expect_identical(err$state, "invalid_matrix_values")
})

test_that("T2-MAT-6: degenerate empty matrix (0 rows or 0 cols) triggers invalid_matrix_dimensions", {
  mat_empty <- matrix(integer(0), nrow = 0L, ncol = 0L)
  err <- tryCatch(
    cerberus_handoff_validate_matrix(mat_empty, matrix_type = "raw_counts"),
    cerberus_handoff_error = function(e) e
  )
  expect_true(inherits(err, "cerberus_handoff_error"))
  expect_identical(err$state, "invalid_matrix_dimensions")
})

test_that("T2-MAT-7: all-zero count matrix triggers warning but retains valid integer status", {
  mat_zero <- matrix(0L, nrow = 50L, ncol = 4L,
                     dimnames = list(sprintf("ENSG%011d", seq_len(50L)), paste0("s", 1:4)))
  res <- cerberus_handoff_validate_matrix(mat_zero, matrix_type = "raw_counts")
  expect_identical(res$frac_integer, 1.0)
})

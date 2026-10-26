# =============================================================================
# test-tier2-boundaries-manifest-fs.R — Tier 2 Boundary: Manifest & File System
# =============================================================================
# Adversarial boundary conditions for manifests, relative paths, and file I/O:
# - Zero-byte manifest.json
# - Corrupted JSON syntax (unclosed braces, trailing commas)
# - Missing required keys vs extraneous unrecognized keys
# - Path traversal attempts in matrix_path and samplesheet_path
# - Non-existent drop directory and missing referenced files
# =============================================================================

if (!exists("e2e_project_root", mode = "function")) {
  if (file.exists("tests/e2e/helper-e2e.R")) {
    source("tests/e2e/helper-e2e.R")
  } else if (file.exists("../helper-e2e.R")) {
    source("../helper-e2e.R")
  }
}

test_that("T2-FS-1: zero-byte manifest.json triggers manifest_malformed error state", {
  d <- e2e_make_test_drop(prefix = "t2_zerobyte_man_", write_manifest = FALSE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  file.create(file.path(d, "manifest.json")) # 0-byte file
  
  res <- cerberus_handoff_validate_drop(drop_dir = d)
  expect_false(res$valid)
  expect_identical(res$status, "INVALID")
  expect_true(any(vapply(res$errors, function(x) x$code == "manifest_malformed", logical(1))))
})

test_that("T2-FS-2: syntax-corrupted JSON manifest triggers manifest_malformed error state", {
  d <- e2e_make_test_drop(prefix = "t2_corrupt_json_", write_manifest = FALSE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  writeLines("{ \"schema_version\": \"cerberus-exchange/v1\", INVALID_JSON...", file.path(d, "manifest.json"))
  
  res <- cerberus_handoff_validate_drop(drop_dir = d)
  expect_false(res$valid)
  expect_identical(res$status, "INVALID")
  expect_true(any(vapply(res$errors, function(x) x$code == "manifest_malformed", logical(1))))
})

test_that("T2-FS-3: manifest referencing non-existent matrix path triggers file_not_found", {
  d <- e2e_make_test_drop(prefix = "t2_missing_mat_", manifest_extra = list(matrix_path = "non_existent_matrix.tsv"))
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  
  res <- cerberus_handoff_validate_drop(drop_dir = d)
  expect_false(res$valid)
  expect_identical(res$status, "INVALID")
  expect_true(any(vapply(res$errors, function(x) x$code == "file_not_found", logical(1))))
})

test_that("T2-FS-4: manifest referencing non-existent samplesheet path triggers file_not_found", {
  d <- e2e_make_test_drop(prefix = "t2_missing_sheet_", manifest_extra = list(samplesheet_path = "missing_sheet.tsv"))
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  
  res <- cerberus_handoff_validate_drop(drop_dir = d)
  expect_false(res$valid)
  expect_identical(res$status, "INVALID")
  expect_true(any(vapply(res$errors, function(x) x$code == "file_not_found", logical(1))))
})

test_that("T2-FS-5: completely non-existent drop directory triggers file_not_found", {
  non_dir <- file.path(tempdir(), "definitely_non_existent_drop_dir_xyz")
  res <- cerberus_handoff_validate_drop(drop_dir = non_dir)
  expect_false(res$valid)
  expect_identical(res$status, "INVALID")
  expect_true(any(vapply(res$errors, function(x) x$code == "file_not_found", logical(1))))
})

test_that("T2-FS-6: extraneous unrecognized manifest keys do not corrupt validation", {
  extra_keys <- list(
    extra_field_1 = "unrecognized_value",
    extra_field_2 = list(nested = 42L, items = c("a", "b"))
  )
  d <- e2e_make_test_drop(prefix = "t2_extra_keys_", manifest_extra = extra_keys)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  
  res <- cerberus_handoff_validate_drop(drop_dir = d)
  expect_true(res$valid)
  expect_identical(res$status, "VALID")
})

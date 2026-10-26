# =============================================================================
# test-tier2-boundaries-species-assembly.R — Tier 2 Boundary: Taxa & IDs
# =============================================================================
# Adversarial species, assembly, and identifier namespace boundary tests:
# - Case-insensitivity and whitespace trimming on species strings
# - Species/assembly cross-pairing violations
# - Ensembl organism cross-contamination
# - Versioned Ensembl identifiers
# - Entrez numeric strictness
# - Namespace declaration discordance (e.g. declared symbol, received Ensembl)
# =============================================================================

if (!exists("e2e_project_root", mode = "function")) {
  if (file.exists("tests/e2e/helper-e2e.R")) {
    source("tests/e2e/helper-e2e.R")
  } else if (file.exists("../helper-e2e.R")) {
    source("../helper-e2e.R")
  }
}

test_that("T2-SP-1: species with leading/trailing whitespace or upper case is normalized", {
  mat <- matrix(1L:10L, nrow = 10L, ncol = 1L,
                dimnames = list(sprintf("ENSG%011d", seq_len(10L)), "s1"))
  res <- cerberus_handoff_validate_identifiers(mat = mat, species = "  HUMAN  ", feature_id_type = "ensembl")
  expect_identical(res$detected_organism, "human")
})

test_that("T2-SP-2: empty or blank species string triggers invalid_species", {
  mat <- matrix(1L:2L, nrow = 2L, ncol = 1L, dimnames = list(c("g1", "g2"), "s1"))
  err <- tryCatch(
    cerberus_handoff_validate_identifiers(mat = mat, species = "   ", feature_id_type = "symbol"),
    cerberus_handoff_error = function(e) e
  )
  expect_true(inherits(err, "cerberus_handoff_error"))
  expect_identical(err$state, "invalid_species")
})

test_that("T2-SP-3: versioned Ensembl IDs (ENSG... .15) are correctly recognized as ensembl", {
  versioned_ids <- sprintf("ENSG%011d.%d", seq_len(10L), seq_len(10L))
  mat <- matrix(1L:10L, nrow = 10L, ncol = 1L, dimnames = list(versioned_ids, "s1"))
  res <- cerberus_handoff_validate_identifiers(mat = mat, species = "human", feature_id_type = "ensembl")
  expect_identical(res$detected_type, "ensembl")
  expect_identical(res$detected_organism, "human")
})

test_that("T2-SP-4: declared symbol namespace with Ensembl IDs triggers invalid_feature_ids", {
  ensembl_ids <- sprintf("ENSG%011d", seq_len(10L))
  mat <- matrix(1L:10L, nrow = 10L, ncol = 1L, dimnames = list(ensembl_ids, "s1"))
  err <- tryCatch(
    cerberus_handoff_validate_identifiers(mat = mat, species = "human", feature_id_type = "symbol"),
    cerberus_handoff_error = function(e) e
  )
  expect_true(inherits(err, "cerberus_handoff_error"))
  expect_identical(err$state, "invalid_feature_ids")
})

test_that("T2-SP-5: declared entrez namespace with non-numeric gene IDs triggers invalid_feature_ids", {
  non_numeric_entrez <- c("101", "102", "NOT_A_NUMBER", "104")
  mat <- matrix(1L:4L, nrow = 4L, ncol = 1L, dimnames = list(non_numeric_entrez, "s1"))
  err <- tryCatch(
    cerberus_handoff_validate_identifiers(mat = mat, species = "human", feature_id_type = "entrez"),
    cerberus_handoff_error = function(e) e
  )
  expect_true(inherits(err, "cerberus_handoff_error"))
  expect_identical(err$state, "invalid_feature_ids")
})

test_that("T2-SP-6: species assembly mismatch in manifest triggers species_assembly_mismatch", {
  d <- e2e_make_test_drop(prefix = "t2_sp_mismatch_", species = "mouse", genome_assembly = "GRCh38")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  man <- jsonlite::fromJSON(file.path(d, "manifest.json"))
  err <- tryCatch(
    cerberus_handoff_validate_manifest(man, drop_dir = d),
    cerberus_handoff_error = function(e) e
  )
  expect_true(inherits(err, "cerberus_handoff_error"))
  expect_identical(err$state, "species_assembly_mismatch")
})

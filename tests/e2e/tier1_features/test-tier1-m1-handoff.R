# =============================================================================
# test-tier1-m1-handoff.R — Tier 1 Feature Coverage: Milestone M1
# =============================================================================
# Covers Features 1-6 (cerberus-handoff upstream exchange contract & CLI)
# >= 5 test cases per feature verifying primary behavior, contracts, and flags.
# =============================================================================

# Source shared E2E helper
if (!exists("e2e_project_root", mode = "function")) {
  if (file.exists("tests/e2e/helper-e2e.R")) {
    source("tests/e2e/helper-e2e.R")
  } else if (file.exists("../helper-e2e.R")) {
    source("../helper-e2e.R")
  }
}

# ── Feature 1: cerberus-exchange/v1 Schema Contract ──────────────────────────

test_that("F01-1: valid manifest with all required fields validates successfully", {
  d <- e2e_make_test_drop(prefix = "f01_valid_")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  man <- jsonlite::fromJSON(file.path(d, "manifest.json"))
  res <- cerberus_handoff_validate_manifest(man, drop_dir = d)
  expect_true(res)
})

test_that("F01-2: manifest with missing schema_version triggers manifest_invalid_field", {
  d <- e2e_make_test_drop(prefix = "f01_no_schema_", write_manifest = FALSE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  man <- list(
    dataset_id = "DS_TEST",
    species = "human",
    genome_assembly = "GRCh38",
    feature_id_type = "ensembl",
    matrix_path = "counts.tsv",
    matrix_type = "raw_counts",
    samplesheet_path = "samplesheet.tsv"
  )
  err <- tryCatch(
    cerberus_handoff_validate_manifest(man, drop_dir = d),
    cerberus_handoff_error = function(e) e
  )
  expect_true(inherits(err, "cerberus_handoff_error"))
  expect_identical(err$state, "manifest_invalid_field")
})

test_that("F01-3: manifest with unsupported schema version triggers manifest_invalid_field", {
  d <- e2e_make_test_drop(prefix = "f01_bad_ver_", write_manifest = FALSE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  man <- list(
    schema_version = "cerberus-exchange/v999",
    dataset_id = "DS_TEST",
    species = "human",
    genome_assembly = "GRCh38",
    feature_id_type = "ensembl",
    matrix_path = "counts.tsv",
    matrix_type = "raw_counts",
    samplesheet_path = "samplesheet.tsv"
  )
  err <- tryCatch(
    cerberus_handoff_validate_manifest(man, drop_dir = d),
    cerberus_handoff_error = function(e) e
  )
  expect_true(inherits(err, "cerberus_handoff_error"))
  expect_identical(err$state, "manifest_invalid_field")
})

test_that("F01-4: manifest with missing or empty dataset_id triggers manifest_invalid_field", {
  d <- e2e_make_test_drop(prefix = "f01_bad_id_")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  man <- jsonlite::fromJSON(file.path(d, "manifest.json"))
  man$dataset_id <- ""
  err <- tryCatch(
    cerberus_handoff_validate_manifest(man, drop_dir = d),
    cerberus_handoff_error = function(e) e
  )
  expect_true(inherits(err, "cerberus_handoff_error"))
  expect_identical(err$state, "manifest_invalid_field")
})

test_that("F01-5: manifest preserves and validates optional cell_cell_communication block", {
  extra <- list(
    cell_cell_communication = list(
      cellchat_ready = TRUE,
      liana_ready = TRUE,
      population_column = "cell_type"
    )
  )
  d <- e2e_make_test_drop(prefix = "f01_ccc_", manifest_extra = extra)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  man <- jsonlite::fromJSON(file.path(d, "manifest.json"))
  res <- cerberus_handoff_validate_manifest(man, drop_dir = d)
  expect_true(res)
  expect_true(is.list(man$cell_cell_communication))
  expect_true(man$cell_cell_communication$cellchat_ready)
})

# ── Feature 2: bulk_assert_raw_counts Ingestion Extension ────────────────────

test_that("F02-1: pure integer count matrix passes strict raw count assertion", {
  set.seed(1)
  mat <- matrix(as.integer(rpois(50L * 4L, lambda = 100L)), nrow = 50L, ncol = 4L,
                dimnames = list(sprintf("ENSG%011d", seq_len(50L)), paste0("s", 1:4)))
  res <- cerberus_handoff_validate_matrix(mat, matrix_type = "raw_counts", allow_non_integer = FALSE)
  expect_identical(res$n_genes, 50L)
  expect_identical(res$frac_integer, 1.0)
})

test_that("F02-2: continuous log-normalized counts rejected with not_raw_counts", {
  set.seed(2)
  mat <- matrix(rnorm(50L * 4L, mean = 5, sd = 1), nrow = 50L, ncol = 4L,
                dimnames = list(sprintf("ENSG%011d", seq_len(50L)), paste0("s", 1:4)))
  mat[mat < 0] <- 0
  err <- tryCatch(
    cerberus_handoff_validate_matrix(mat, matrix_type = "raw_counts", allow_non_integer = FALSE),
    cerberus_handoff_error = function(e) e
  )
  expect_true(inherits(err, "cerberus_handoff_error"))
  expect_identical(err$state, "not_raw_counts")
})

test_that("F02-3: floating point numbers within machine precision 1e-8 treated as integer", {
  mat <- matrix(c(10.00000000000001, 20.0, 30.00000000000002, 40.0), nrow = 2L, ncol = 2L,
                dimnames = list(c("ENSG00000000001", "ENSG00000000002"), c("s1", "s2")))
  res <- cerberus_handoff_validate_matrix(mat, matrix_type = "raw_counts", allow_non_integer = FALSE)
  expect_identical(res$frac_integer, 1.0)
})

test_that("F02-4: allow_non_integer = TRUE emits warning when frac_int > 0.95", {
  set.seed(4)
  # 98% integers, 2% non-integers
  vals <- as.double(rpois(100L, lambda = 50L))
  vals[c(1, 2)] <- c(10.5, 20.25)
  mat <- matrix(vals, nrow = 25L, ncol = 4L,
                dimnames = list(sprintf("ENSG%011d", seq_len(25L)), paste0("s", 1:4)))
  res <- cerberus_handoff_validate_matrix(mat, matrix_type = "raw_counts", allow_non_integer = TRUE)
  expect_true(res$frac_integer > 0.95)
})

test_that("F02-5: allow_non_integer = TRUE rejects matrix when frac_int <= 0.95", {
  set.seed(5)
  # 90% integers, 10% non-integers
  vals <- as.double(rpois(100L, lambda = 50L))
  vals[seq_len(10L)] <- seq(1.1, 10.1, by = 1.0)
  mat <- matrix(vals, nrow = 25L, ncol = 4L,
                dimnames = list(sprintf("ENSG%011d", seq_len(25L)), paste0("s", 1:4)))
  err <- tryCatch(
    cerberus_handoff_validate_matrix(mat, matrix_type = "raw_counts", allow_non_integer = TRUE),
    cerberus_handoff_error = function(e) e
  )
  expect_true(inherits(err, "cerberus_handoff_error"))
  expect_identical(err$state, "not_raw_counts")
})

# ── Feature 3: Sample Metadata Design Invariants ─────────────────────────────

test_that("F03-1: samplesheet with valid 2 conditions x 2 replicates passes", {
  sheet <- data.frame(
    sample = c("s1", "s2", "s3", "s4"),
    condition = c("control", "control", "treated", "treated"),
    stringsAsFactors = FALSE
  )
  res <- cerberus_handoff_validate_samplesheet(sheet, mat_colnames = c("s1", "s2", "s3", "s4"))
  expect_true(res$valid)
  expect_identical(length(res$conditions), 2L)
})

test_that("F03-2: samplesheet missing required sample or condition column fails", {
  sheet <- data.frame(
    sample_id = c("s1", "s2"),
    batch = c("b1", "b2"),
    stringsAsFactors = FALSE
  )
  err <- tryCatch(
    cerberus_handoff_validate_samplesheet(sheet, mat_colnames = c("s1", "s2")),
    cerberus_handoff_error = function(e) e
  )
  expect_true(inherits(err, "cerberus_handoff_error"))
  expect_identical(err$state, "samplesheet_missing_columns")
})

test_that("F03-3: samplesheet with sample IDs differing from matrix column names fails", {
  sheet <- data.frame(
    sample = c("s1", "s2", "s3", "s4"),
    condition = c("control", "control", "treated", "treated"),
    stringsAsFactors = FALSE
  )
  err <- tryCatch(
    cerberus_handoff_validate_samplesheet(sheet, mat_colnames = c("s1", "s2", "s3", "s_other")),
    cerberus_handoff_error = function(e) e
  )
  expect_true(inherits(err, "cerberus_handoff_error"))
  expect_identical(err$state, "samplesheet_matrix_mismatch")
})

test_that("F03-4: pseudoreplication with single replicate condition is detected and rejected", {
  sheet <- data.frame(
    sample = c("s1", "s2", "s3"),
    condition = c("control", "control", "treated"), # 'treated' has only 1 replicate (< 2L)
    stringsAsFactors = FALSE
  )
  err <- tryCatch(
    cerberus_handoff_validate_samplesheet(sheet, mat_colnames = c("s1", "s2", "s3")),
    cerberus_handoff_error = function(e) e
  )
  expect_true(inherits(err, "cerberus_handoff_error"))
  expect_identical(err$state, "pseudoreplication_detected")
})

test_that("F03-5: covariate 100% confounded with condition triggers confounded_covariates entry", {
  sheet <- data.frame(
    sample = c("s1", "s2", "s3", "s4"),
    condition = c("control", "control", "treated", "treated"),
    batch = c("batch_A", "batch_A", "batch_B", "batch_B"), # 100% confounded with condition
    stringsAsFactors = FALSE
  )
  res <- cerberus_handoff_validate_samplesheet(sheet, mat_colnames = c("s1", "s2", "s3", "s4"))
  expect_true(res$valid)
  expect_true("batch" %in% res$confounded_covariates)
})

# ── Feature 4: Species & Identifier Validation ───────────────────────────────

test_that("F04-1: valid human species with GRCh38 and Ensembl prefix ENSG passes", {
  mat <- matrix(1L:10L, nrow = 10L, ncol = 1L,
                dimnames = list(sprintf("ENSG%011d", seq_len(10L)), "s1"))
  res <- cerberus_handoff_validate_identifiers(mat = mat, species = "human", feature_id_type = "ensembl")
  expect_identical(res$detected_type, "ensembl")
  expect_identical(res$detected_organism, "human")
})

test_that("F04-2: valid mouse species with GRCm39 and Ensembl prefix ENSMUSG passes", {
  mat <- matrix(1L:10L, nrow = 10L, ncol = 1L,
                dimnames = list(sprintf("ENSMUSG%011d", seq_len(10L)), "s1"))
  res <- cerberus_handoff_validate_identifiers(mat = mat, species = "mouse", feature_id_type = "ensembl")
  expect_identical(res$detected_type, "ensembl")
  expect_identical(res$detected_organism, "mouse")
})

test_that("F04-3: unsupported or null species triggers invalid_species", {
  mat <- matrix(1L:2L, nrow = 2L, ncol = 1L, dimnames = list(c("g1", "g2"), "s1"))
  err <- tryCatch(
    cerberus_handoff_validate_identifiers(mat = mat, species = "zebrafish", feature_id_type = "ensembl"),
    cerberus_handoff_error = function(e) e
  )
  expect_true(inherits(err, "cerberus_handoff_error"))
  expect_identical(err$state, "invalid_species")
})

test_that("F04-4: species-assembly mismatch (human + GRCm39) triggers species_assembly_mismatch", {
  d <- e2e_make_test_drop(prefix = "f04_mismatch_", species = "human", genome_assembly = "GRCm39")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  man <- jsonlite::fromJSON(file.path(d, "manifest.json"))
  err <- tryCatch(
    cerberus_handoff_validate_manifest(man, drop_dir = d),
    cerberus_handoff_error = function(e) e
  )
  expect_true(inherits(err, "cerberus_handoff_error"))
  expect_identical(err$state, "species_assembly_mismatch")
})

test_that("F04-5: cross-species ensembl prefix discordance triggers species_feature_mismatch", {
  # Human species declared, but mouse ENSMUSG identifiers passed
  mat <- matrix(1L:10L, nrow = 10L, ncol = 1L,
                dimnames = list(sprintf("ENSMUSG%011d", seq_len(10L)), "s1"))
  err <- tryCatch(
    cerberus_handoff_validate_identifiers(mat = mat, species = "human", feature_id_type = "ensembl"),
    cerberus_handoff_error = function(e) e
  )
  expect_true(inherits(err, "cerberus_handoff_error"))
  expect_identical(err$state, "species_feature_mismatch")
})

# ── Feature 5: cerberus-handoff Headless CLI ─────────────────────────────────

test_that("F05-1: CLI invocation on valid drop exits 0 and writes validation.json", {
  d <- e2e_make_test_drop(prefix = "f05_cli_valid_")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  out_dir <- file.path(d, "cli_out")

  cli_res <- e2e_run_cli(c(paste0("--drop=", d), paste0("--out=", out_dir)))
  expect_identical(cli_res$exit_code, 0L)
  val_file <- file.path(out_dir, "validation.json")
  expect_true(file.exists(val_file))

  v_json <- jsonlite::fromJSON(val_file)
  expect_identical(v_json$status, "VALID")
  expect_true(v_json$valid)
})

test_that("F05-2: CLI invocation on invalid drop exits 1 and emits status INVALID", {
  # Create invalid drop with negative counts
  bad_mat <- matrix(c(-10L, 5L, 20L, 15L), nrow = 2L, ncol = 2L,
                    dimnames = list(c("g1", "g2"), c("s1", "s2")))
  sheet <- data.frame(sample = c("s1", "s2"), condition = c("c1", "c2"), stringsAsFactors = FALSE)
  d <- e2e_make_test_drop(prefix = "f05_cli_bad_", counts = bad_mat, samplesheet = sheet)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  out_dir <- file.path(d, "cli_out")

  cli_res <- e2e_run_cli(c(paste0("--drop=", d), paste0("--out=", out_dir)))
  expect_identical(cli_res$exit_code, 1L)
  val_file <- file.path(out_dir, "validation.json")
  expect_true(file.exists(val_file))

  v_json <- jsonlite::fromJSON(val_file)
  expect_identical(v_json$status, "INVALID")
  expect_false(v_json$valid)
})

test_that("F05-3: CLI --strict flag turns covariate_confounded warning into exit 1", {
  # Confounded batch with condition
  sheet <- data.frame(
    sample = paste0("sample_", seq_len(4L)),
    condition = c("c1", "c1", "c2", "c2"),
    batch = c("b1", "b1", "b2", "b2"),
    stringsAsFactors = FALSE
  )
  d <- e2e_make_test_drop(prefix = "f05_cli_strict_", samplesheet = sheet)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  out_dir <- file.path(d, "cli_out")

  # Without --strict -> exit 0 with warning
  res_lenient <- e2e_run_cli(c(paste0("--drop=", d), paste0("--out=", out_dir)))
  expect_identical(res_lenient$exit_code, 0L)

  # With --strict -> exit 1
  res_strict <- e2e_run_cli(c(paste0("--drop=", d), paste0("--out=", out_dir), "--strict"))
  expect_identical(res_strict$exit_code, 1L)
  val_file <- file.path(out_dir, "validation.json")
  expect_true(file.exists(val_file))
  v_strict <- jsonlite::fromJSON(val_file)
  expect_identical(v_strict$status, "INVALID")
})

test_that("F05-4: CLI --help option prints usage and exits 0", {
  cli_res <- e2e_run_cli("--help")
  expect_identical(cli_res$exit_code, 0L)
  expect_true(grepl("Usage: Rscript tools/cerberus_handoff.R", cli_res$stdout_text, fixed = TRUE))
})

test_that("F05-5: CLI with missing mandatory arguments exits 1 with usage hint", {
  cli_res <- e2e_run_cli(character(0))
  expect_identical(cli_res$exit_code, 1L)
  expect_true(grepl("Erreur", cli_res$stdout_text) || grepl("Usage", cli_res$stdout_text))
})

# ── Feature 6: Contract Freeze Test Suite for Handoff ────────────────────────

test_that("F06-1: all 19 public API functions in cerberus_handoff.R are exported", {
  expected_api <- cerberus_handoff_public_api()
  expect_identical(length(expected_api), 19L)
  for (fn in expected_api) {
    expect_true(exists(fn, mode = "function"), info = paste("Missing API function:", fn))
  }
})

test_that("F06-2: all 16 frozen error states exist in cerberus_handoff_states()", {
  states <- cerberus_handoff_states()
  expect_identical(length(states), 16L)
  expect_true("not_raw_counts" %in% states)
  expect_true("pseudoreplication_detected" %in% states)
  expect_true("species_assembly_mismatch" %in% states)
  expect_true("samplesheet_matrix_mismatch" %in% states)
})

test_that("F06-3: cerberus_handoff_schema_version returns cerberus-exchange/v1", {
  expect_identical(cerberus_handoff_schema_version(), "cerberus-exchange/v1")
})

test_that("F06-4: tools/pkg_skeleton/R/cerberus_handoff.R maintains pure R purity (no Shiny symbols)", {
  code_path <- file.path(e2e_project_root(), "tools/pkg_skeleton/R/cerberus_handoff.R")
  lines <- readLines(code_path, warn = FALSE)
  code_only <- paste(lines[!grepl("^\\s*#", lines)], collapse = " ")
  shiny_symbols <- c("reactiveVal", "reactiveValues", "observeEvent", "isolate", "renderPlot", "uiOutput")
  for (sym in shiny_symbols) {
    expect_false(grepl(paste0("\\b", sym, "\\b"), code_only), info = paste("Found Shiny symbol:", sym))
  }
})

test_that("F06-5: supported biological taxonomies and assemblies are frozen", {
  expect_setequal(cerberus_handoff_supported_species(), c("human", "mouse"))
  assemblies <- cerberus_handoff_supported_assemblies()
  expect_setequal(assemblies, c("GRCh38", "GRCm39"))
  expect_setequal(cerberus_handoff_supported_matrix_types(), c("raw_counts", "normalized"))
  expect_setequal(cerberus_handoff_supported_feature_types(), c("ensembl", "symbol", "entrez"))
})

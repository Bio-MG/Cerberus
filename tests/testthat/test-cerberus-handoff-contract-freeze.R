# =============================================================================
# test-cerberus-handoff-contract-freeze.R — gel du contrat
# docs/contracts/CERBERUS_HANDOFF_CONTRACT.md (Proposition 4D-3)
# =============================================================================
source_project_file("tools/pkg_skeleton/R/io_helpers.R")
source_project_file("tools/pkg_skeleton/R/error_state.R")
if (file.exists(file.path(ts_project_root(), "tools/pkg_skeleton/R/batch_correction.R"))) {
  source_project_file("tools/pkg_skeleton/R/batch_correction.R")
} else if (file.exists(file.path(ts_project_root(), "R/bulk/batch_correction.R"))) {
  source_project_file("R/bulk/batch_correction.R")
}
source_project_file("R/core/cerberus_handoff.R")

.ch_top_level_names <- function(relpath) {
  exprs <- parse(file.path(ts_project_root(), relpath), keep.source = FALSE)
  nms <- character(0)
  for (e in exprs) {
    if (is.call(e) && identical(deparse(e[[1L]]), "<-") && is.symbol(e[[2L]])) {
      nms <- c(nms, as.character(e[[2L]]))
    }
  }
  unique(nms)
}

test_that("public API surface is frozen (names + signatures)", {
  expect_setequal(
    cerberus_handoff_public_api(),
    c(
      "cerberus_handoff_error_class",
      "cerberus_handoff_error_state",
      "cerberus_handoff_public_api",
      "cerberus_handoff_read_manifest",
      "cerberus_handoff_read_matrix",
      "cerberus_handoff_read_samplesheet",
      "cerberus_handoff_schema_version",
      "cerberus_handoff_states",
      "cerberus_handoff_supported_assemblies",
      "cerberus_handoff_supported_feature_types",
      "cerberus_handoff_supported_matrix_types",
      "cerberus_handoff_supported_species",
      "cerberus_handoff_validate_drop",
      "cerberus_handoff_validate_identifiers",
      "cerberus_handoff_validate_manifest",
      "cerberus_handoff_validate_matrix",
      "cerberus_handoff_validate_samplesheet",
      "cerberus_handoff_write_validation",
      "cerberus_validate_drop"
    )
  )

  expected_api <- list(
    cerberus_handoff_error_class             = list(),
    cerberus_handoff_error_state             = list(e = NULL),
    cerberus_handoff_public_api              = list(),
    cerberus_handoff_read_manifest           = list(drop_dir = NULL),
    cerberus_handoff_read_matrix             = list(matrix_path = NULL),
    cerberus_handoff_read_samplesheet        = list(samplesheet_path = NULL),
    cerberus_handoff_schema_version          = list(),
    cerberus_handoff_states                  = list(),
    cerberus_handoff_supported_assemblies    = list(),
    cerberus_handoff_supported_feature_types = list(),
    cerberus_handoff_supported_matrix_types  = list(),
    cerberus_handoff_supported_species       = list(),
    cerberus_handoff_validate_drop           = list(drop_dir = NULL, out_dir = NULL,
                                                    allow_non_integer = FALSE),
    cerberus_handoff_validate_identifiers    = list(mat = NULL, species = "human",
                                                    feature_id_type = "ensembl"),
    cerberus_handoff_validate_manifest       = list(manifest = NULL, drop_dir = NULL),
    cerberus_handoff_validate_matrix         = list(mat = NULL, matrix_type = "raw_counts",
                                                    feature_id_type = "ensembl", species = "human",
                                                    allow_non_integer = FALSE),
    cerberus_handoff_validate_samplesheet    = list(samplesheet = NULL, mat_colnames = NULL),
    cerberus_handoff_write_validation        = list(validation_result = NULL, out_path = NULL),
    cerberus_validate_drop                   = list(drop_dir = NULL, out_dir = NULL,
                                                    allow_non_integer = FALSE)
  )

  for (fn_name in names(expected_api)) {
    expect_true(exists(fn_name, where = globalenv(), inherits = FALSE),
                info = paste("fonction publique manquante :", fn_name))
    fn <- get(fn_name, envir = globalenv())
    expect_true(is.function(fn), info = fn_name)
    got <- names(formals(fn))
    if (is.null(got)) got <- character(0)
    want <- names(expected_api[[fn_name]])
    if (is.null(want)) want <- character(0)
    expect_identical(got, want, info = paste("signature de", fn_name))
  }

  defined <- .ch_top_level_names("R/core/cerberus_handoff.R")
  public <- setdiff(defined, grep("^\\.", defined, value = TRUE))
  expect_setequal(public, cerberus_handoff_public_api())
})

test_that("constants and error states are frozen", {
  expect_identical(cerberus_handoff_schema_version(), "cerberus-exchange/v1")
  expect_setequal(cerberus_handoff_supported_species(), c("human", "mouse"))
  expect_setequal(cerberus_handoff_supported_assemblies(), c("GRCh38", "GRCm39"))
  expect_setequal(cerberus_handoff_supported_feature_types(), c("ensembl", "symbol", "entrez"))
  expect_setequal(cerberus_handoff_supported_matrix_types(), c("raw_counts", "normalized"))
  expect_identical(cerberus_handoff_error_class(), "cerberus_handoff_error")

  expected_states <- c(
    "file_corrupted",
    "file_not_found",
    "invalid_feature_ids",
    "invalid_matrix_dimensions",
    "invalid_matrix_values",
    "invalid_species",
    "manifest_invalid_field",
    "manifest_malformed",
    "manifest_missing",
    "negative_counts",
    "not_raw_counts",
    "pseudoreplication_detected",
    "samplesheet_matrix_mismatch",
    "samplesheet_missing_columns",
    "species_assembly_mismatch",
    "species_feature_mismatch"
  )
  expect_setequal(cerberus_handoff_states(), expected_states)

  src <- paste(readLines(file.path(ts_project_root(), "R/core/cerberus_handoff.R"), warn = FALSE), collapse = "\n")
  states_found <- unique(regmatches(src, gregexpr('state = "[a-z_]+"', src))[[1]])
  expected_state_calls <- paste0('state = "', expected_states, '"')
  expect_setequal(states_found, expected_state_calls)
})

test_that("R/core/cerberus_handoff.R is pure (no Shiny symbols)", {
  src <- paste(readLines(file.path(ts_project_root(), "R/core/cerberus_handoff.R"), warn = FALSE), collapse = "\n")
  for (sym in c("reactiveVal", "reactiveValues", "observeEvent", "output\\$",
                "input\\$", "moduleServer", "showNotification",
                "isolate\\(", "reactive\\(", "renderPlot")) {
    expect_false(grepl(sym, src, perl = TRUE),
                 info = paste("symbole Shiny interdit trouve :", sym))
  }
})

test_that("contract document is in sync with the frozen code", {
  doc_path <- file.path(ts_project_root(), "docs", "contracts", "CERBERUS_HANDOFF_CONTRACT.md")
  skip_if_not(file.exists(doc_path), .ts_contract_skip_msg(doc_path))
  doc <- paste(.ts_contract_readlines(doc_path, encoding = "UTF-8"), collapse = "\n")
  for (f in cerberus_handoff_public_api()) {
    expect_match(doc, paste0("`", f, "`"), fixed = TRUE,
                 info = paste("fonction absente du contrat :", f))
  }
  for (st in cerberus_handoff_states()) {
    expect_match(doc, st, fixed = TRUE, info = paste("etat absent du contrat :", st))
  }
  expect_match(doc, "cerberus-exchange/v1", fixed = TRUE)
  expect_match(doc, "TS_DA_MIN_REPLICATES_PER_CONDITION", fixed = TRUE)
  expect_match(doc, "bulk_assert_raw_counts", fixed = TRUE)
})

test_that("warning codes and covariate confounding are frozen and mapped", {
  expected_warnings <- c(
    "duplicate_sample_ids",
    "duplicate_feature_ids",
    "low_feature_count",
    "covariate_confounded",
    "non_integer_counts_tolerated"
  )

  # 1. AST check: ensure R/core/cerberus_handoff.R actively emits covariate_confounded
  src <- paste(readLines(file.path(ts_project_root(), "R/core/cerberus_handoff.R"), warn = FALSE), collapse = "\n")
  warn_calls <- unique(regmatches(src, gregexpr('add_warn\\("[a-z_]+"', src))[[1]])
  warn_codes_found <- sub('^add_warn\\("', '', sub('"$', '', warn_calls))
  expect_true("covariate_confounded" %in% warn_codes_found,
              info = "add_warn('covariate_confounded', ...) absent de R/core/cerberus_handoff.R")

  # 2. Contract document sync: verify that CERBERUS_HANDOFF_CONTRACT.md contains all warning codes
  doc_path <- file.path(ts_project_root(), "docs", "contracts", "CERBERUS_HANDOFF_CONTRACT.md")
  skip_if_not(file.exists(doc_path), .ts_contract_skip_msg(doc_path))
  doc <- paste(.ts_contract_readlines(doc_path, encoding = "UTF-8"), collapse = "\n")
  for (w in expected_warnings) {
    expect_match(doc, w, fixed = TRUE, info = paste("Code d'avertissement absent du contrat :", w))
  }
})

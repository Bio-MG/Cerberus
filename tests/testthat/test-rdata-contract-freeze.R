# =============================================================================
# test-rdata-contract-freeze.R — Gel du contrat RDATA_IMPORT_CONTRACT
# =============================================================================
# Verrouille : codes de classification, API publique (noms + signatures),
# existence et synchronisation de docs/contracts/RDATA_IMPORT_CONTRACT.md,
# parametres config TS_IMPORT_RDA_*. Toute evolution = code + ce test + doc
# SIMULTANEMENT (regle contract-first du depot).
# =============================================================================

source_project_file("R/core/rdata_io.R")

test_that("les codes de classification sont gelés", {
  expect_setequal(
    .RDATA_TYPE_CODES,
    c("seurat", "sce", "cellchat", "velocity", "metadata", "matrix", "image", "other")
  )
  expect_setequal(.RDATA_VELOCITY_SIGNATURE, c("spliced", "unspliced"))
  # tout code retourne appartient au gel
  probe <- list(
    structure(list(), class = "Seurat"),
    structure(list(), class = "SingleCellExperiment"),
    structure(list(), class = "CellChat"),
    list(spliced = matrix(1), unspliced = matrix(1)),
    data.frame(x = 1),
    matrix(1),
    grid::rectGrob(),
    "autre"
  )
  for (p in probe) {
    expect_true(rdata_classify_object(p) %in% .RDATA_TYPE_CODES)
  }
})

test_that("l'API publique du contrat est gelée (noms + signatures)", {
  expected_api <- list(
    rdata_supported_extensions = list(),
    rdata_is_supported_file    = list(path = NULL),
    rdata_is_explorable_file   = list(path = NULL),
    rdata_load_env             = list(path = NULL),
    rdata_read_file_env        = list(path = NULL),
    rdata_safe_name            = list(path = NULL),
    rdata_classify_object      = list(obj = NULL),
    rdata_describe_objects     = list(env = NULL),
    rdata_flatten_env          = list(env = NULL, max_depth = NULL, max_rows = NULL),
    rdata_extract_object       = list(env = NULL, object_name = NULL),
    rdata_extract_path         = list(env = NULL, path = NULL),
    rdata_assert_class         = list(obj = NULL, expected = NULL, context = NULL),
    rdata_export_selection     = list(env = NULL, object_names = NULL, file = NULL),
    rdata_export_paths         = list(env = NULL, paths = NULL, file = NULL),
    rdata_free                 = list(env = NULL)
  )
  for (fn_name in names(expected_api)) {
    expect_true(exists(fn_name, where = globalenv(), inherits = FALSE),
                info = paste("fonction publique manquante :", fn_name))
    fn <- get(fn_name, envir = globalenv())
    expect_true(is.function(fn), info = fn_name)
    expect_setequal(as.character(names(formals(fn))), as.character(names(expected_api[[fn_name]])))
    cat("  signature OK :", fn_name, "\n")
  }
})

test_that("les erreurs sont classees rdata_import_error (sante du contrat)", {
  expect_error(rdata_load_env("inexistant.rda"), class = "rdata_import_error")
  env <- new.env(parent = emptyenv())
  expect_error(rdata_extract_object(env, "x"), class = "rdata_import_error")
})

test_that("le contrat documente est synchronise avec le code", {
  doc <- file.path(ts_project_root(), "docs", "contracts", "RDATA_IMPORT_CONTRACT.md")
  skip_if_not(file.exists(doc), .ts_contract_skip_msg(doc))
  txt <- paste(.ts_contract_readlines(doc, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  for (fn_name in c("rdata_load_env", "rdata_classify_object",
                    "rdata_describe_objects", "rdata_extract_object",
                    "rdata_assert_class", "rdata_export_selection",
                    "rdata_flatten_env", "rdata_extract_path",
                    "rdata_read_file_env", "rdata_export_paths",
                    "rdata_free")) {
    expect_match(txt, fn_name, fixed = TRUE,
                 info = paste("fonction absente du contrat :", fn_name))
  }
  expect_match(txt, "rdata_import_error", fixed = TRUE)
  expect_match(txt, "RDATA_IMPORT_CONTRACT", fixed = TRUE)
})

test_that("les parametres config TS_IMPORT_RDA_* sont declares", {
  defaults <- file.path(ts_project_root(), "config", "defaults.R")
  expect_true(file.exists(defaults))
  txt <- paste(readLines(defaults, warn = FALSE), collapse = "\n")
  expect_match(txt, "TS_IMPORT_RDA_EXTENSIONS", fixed = TRUE)
  expect_match(txt, "TS_IMPORT_RDA_WARN_MB", fixed = TRUE)
})

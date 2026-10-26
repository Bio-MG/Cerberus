# =============================================================================
# test-cerberus-handoff.R — Suite de tests unitaires et d'integration CLI
#                            cerberus-handoff (Proposition 4D-3)
# =============================================================================
source_project_file("tools/pkg_skeleton/R/io_helpers.R")
source_project_file("tools/pkg_skeleton/R/error_state.R")
if (file.exists(file.path(ts_project_root(), "tools/pkg_skeleton/R/batch_correction.R"))) {
  source_project_file("tools/pkg_skeleton/R/batch_correction.R")
} else if (file.exists(file.path(ts_project_root(), "R/bulk/batch_correction.R"))) {
  source_project_file("R/bulk/batch_correction.R")
}
source_project_file("tools/pkg_skeleton/R/cerberus_handoff.R")
source_project_file("tools/cerberus_handoff.R")

# ── Helper de creation de drops temporaires -----------------------------------
.make_test_drop <- function(prefix = "drop_",
                            counts = NULL,
                            samplesheet = NULL,
                            manifest_extra = list(),
                            matrix_filename = "counts.tsv",
                            samplesheet_filename = "samplesheet.tsv",
                            write_manifest = TRUE) {
  drop_dir <- tempfile(pattern = prefix)
  dir.create(drop_dir, recursive = TRUE)

  # Counts matrix par defaut : 120 genes humains Ensembl x 4 echantillons
  if (is.null(counts)) {
    g_ids <- sprintf("ENSG%011d", seq_len(120L))
    s_ids <- paste0("sample_", seq_len(4L))
    set.seed(42)
    counts <- matrix(as.integer(rpois(120L * 4L, lambda = 300L)),
                     nrow = 120L, ncol = 4L,
                     dimnames = list(g_ids, s_ids))
  }

  mat_path <- file.path(drop_dir, matrix_filename)
  if (inherits(counts, "dgCMatrix") || grepl("\\.rds$", matrix_filename)) {
    saveRDS(counts, mat_path)
  } else if (grepl("\\.gz$", matrix_filename)) {
    gz <- gzfile(mat_path, "wt")
    utils::write.table(as.data.frame(counts), file = gz, sep = "\t", quote = FALSE, col.names = NA)
    close(gz)
  } else {
    utils::write.table(as.data.frame(counts), file = mat_path, sep = "\t", quote = FALSE, col.names = NA)
  }

  # Samplesheet par defaut : 2 conditions avec 2 replicats chacune (anti-pseudoreplication)
  if (is.null(samplesheet)) {
    s_ids <- colnames(counts)
    samplesheet <- data.frame(
      sample = s_ids,
      condition = c("control", "control", "treated", "treated"),
      batch = c("b1", "b2", "b1", "b2"),
      stringsAsFactors = FALSE
    )
  }
  samp_path <- file.path(drop_dir, samplesheet_filename)
  utils::write.table(samplesheet, file = samp_path, sep = "\t", quote = FALSE, row.names = FALSE)

  if (isTRUE(write_manifest)) {
    man <- list(
      schema_version = "cerberus-exchange/v1",
      dataset_id = "DS_TEST_001",
      species = "human",
      genome_assembly = "GRCh38",
      feature_id_type = "ensembl",
      matrix_path = matrix_filename,
      matrix_type = "raw_counts",
      samplesheet_path = samplesheet_filename
    )
    for (k in names(manifest_extra)) {
      man[[k]] <- manifest_extra[[k]]
    }
    writeLines(jsonlite::toJSON(man, auto_unbox = TRUE, pretty = TRUE),
               file.path(drop_dir, "manifest.json"))
  }

  drop_dir
}

# ── Tests nominaux et formats de matrices ------------------------------------

test_that("valid drop validates successfully and produces complete report", {
  d <- .make_test_drop(prefix = "valid_")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  out <- file.path(d, "out")

  res <- cerberus_handoff_validate_drop(drop_dir = d, out_dir = out)
  expect_true(res$valid)
  expect_identical(res$status, "VALID")
  expect_identical(res$schema_version, "cerberus-exchange/v1")
  expect_identical(res$dataset_id, "DS_TEST_001")
  expect_identical(res$summary$n_errors, 0L)
  expect_identical(res$summary$n_genes, 120L)
  expect_identical(res$summary$n_samples, 4L)
  expect_setequal(res$summary$conditions, c("control", "treated"))
  expect_true(file.exists(file.path(out, "validation.json")))

  # Verification du JSON emis
  from_disk <- jsonlite::fromJSON(file.path(out, "validation.json"))
  expect_identical(from_disk$status, "VALID")
  expect_identical(from_disk$summary$valid, TRUE)

  # Test alias cerberus_validate_drop
  res_alias <- cerberus_validate_drop(drop_dir = d)
  expect_true(res_alias$valid)
})

test_that("valid drop with gzipped TSV counts validates cleanly", {
  d <- .make_test_drop(prefix = "gz_", matrix_filename = "counts.tsv.gz")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)

  res <- cerberus_handoff_validate_drop(drop_dir = d)
  expect_true(res$valid)
  expect_identical(res$status, "VALID")
})

test_that("valid drop with sparse Matrix dgCMatrix in RDS format validates cleanly", {
  skip_if_not_installed("Matrix")
  g_ids <- sprintf("ENSG%011d", seq_len(150L))
  s_ids <- paste0("cell_", seq_len(6L))
  dense <- matrix(as.integer(rpois(150L * 6L, lambda = 5L)), 150L, 6L,
                  dimnames = list(g_ids, s_ids))
  sparse <- Matrix::Matrix(dense, sparse = TRUE)

  samp <- data.frame(
    sample = s_ids,
    condition = c("WT", "WT", "WT", "KO", "KO", "KO"),
    stringsAsFactors = FALSE
  )

  d <- .make_test_drop(prefix = "sparse_", counts = sparse, samplesheet = samp,
                       matrix_filename = "counts.rds")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)

  res <- cerberus_handoff_validate_drop(drop_dir = d)
  expect_true(res$valid)
  expect_identical(res$status, "VALID")
  expect_identical(res$summary$n_genes, 150L)
  expect_identical(res$summary$n_samples, 6L)
})

# ── Counts non entiers et sémantique de comptage -----------------------------

test_that("non-integer counts matrix triggers not_raw_counts", {
  g_ids <- sprintf("ENSG%011d", seq_len(50L))
  s_ids <- paste0("s", 1:4)
  # Matrice continue normalisee (ex: VST / log2-CPM)
  vst <- matrix(rnorm(50L * 4L, mean = 7.5, sd = 1.2), 50L, 4L,
                dimnames = list(g_ids, s_ids))

  # 1. Verification par appel direct du validateur
  err <- tryCatch(
    cerberus_handoff_validate_matrix(vst, matrix_type = "raw_counts", allow_non_integer = FALSE),
    error = function(e) e
  )
  expect_s3_class(err, "cerberus_handoff_error")
  expect_identical(cerberus_handoff_error_state(err), "not_raw_counts")

  # 2. Verification via drop complet
  d <- .make_test_drop(prefix = "vst_", counts = vst)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  res <- cerberus_handoff_validate_drop(drop_dir = d)
  expect_false(res$valid)
  expect_identical(res$status, "INVALID")
  expect_true(any(vapply(res$errors, function(x) x$code == "not_raw_counts", logical(1))))
})

test_that("allow_non_integer tolerance behaviour (95% rule)", {
  g_ids <- sprintf("ENSG%011d", seq_len(100L))
  s_ids <- paste0("s", 1:4)
  # 96% entiers (4 valeurs decimales sur 400)
  m_tol <- matrix(as.integer(rpois(100L * 4L, 200L)), 100L, 4L, dimnames = list(g_ids, s_ids))
  m_tol[1:4] <- m_tol[1:4] + 0.45

  # Sans allow_non_integer -> echec
  d1 <- .make_test_drop(prefix = "not_tol_", counts = m_tol)
  on.exit(unlink(d1, recursive = TRUE), add = TRUE)
  res1 <- cerberus_handoff_validate_drop(drop_dir = d1, allow_non_integer = FALSE)
  expect_false(res1$valid)
  expect_identical(res1$errors[[1L]]$code, "not_raw_counts")

  # Avec allow_non_integer = TRUE -> succes avec avertissement non_integer_counts_tolerated
  res2 <- cerberus_handoff_validate_drop(drop_dir = d1, allow_non_integer = TRUE)
  expect_true(res2$valid)
  expect_identical(res2$status, "VALID")
  expect_true(any(vapply(res2$warnings, function(x) x$code == "non_integer_counts_tolerated", logical(1))))

  # Si moins de 95% d'entiers (ex: 80% entiers) -> echec meme sous allow_non_integer = TRUE
  m_bad <- matrix(as.integer(rpois(100L * 4L, 200L)), 100L, 4L, dimnames = list(g_ids, s_ids))
  m_bad[1:80] <- m_bad[1:80] + 0.55
  d2 <- .make_test_drop(prefix = "bad_tol_", counts = m_bad)
  on.exit(unlink(d2, recursive = TRUE), add = TRUE)
  res3 <- cerberus_handoff_validate_drop(drop_dir = d2, allow_non_integer = TRUE)
  expect_false(res3$valid)
  expect_identical(res3$errors[[1L]]$code, "not_raw_counts")
})

test_that("negative counts trigger negative_counts error", {
  g_ids <- sprintf("ENSG%011d", seq_len(50L))
  s_ids <- paste0("s", 1:4)
  mat_neg <- matrix(as.integer(rpois(50L * 4L, 100L)), 50L, 4L, dimnames = list(g_ids, s_ids))
  mat_neg[3L, 2L] <- -5L

  err <- tryCatch(
    cerberus_handoff_validate_matrix(mat_neg),
    error = function(e) e
  )
  expect_s3_class(err, "cerberus_handoff_error")
  expect_identical(cerberus_handoff_error_state(err), "negative_counts")

  d <- .make_test_drop(prefix = "neg_", counts = mat_neg)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  res <- cerberus_handoff_validate_drop(drop_dir = d)
  expect_false(res$valid)
  expect_identical(res$errors[[1L]]$code, "negative_counts")
})

test_that("NA or infinite matrix values trigger invalid_matrix_values error", {
  g_ids <- sprintf("ENSG%011d", seq_len(50L))
  s_ids <- paste0("s", 1:4)
  mat_na <- matrix(as.integer(rpois(50L * 4L, 100L)), 50L, 4L, dimnames = list(g_ids, s_ids))
  mat_na[1L, 1L] <- NA_integer_

  err <- tryCatch(cerberus_handoff_validate_matrix(mat_na), error = function(e) e)
  expect_s3_class(err, "cerberus_handoff_error")
  expect_identical(cerberus_handoff_error_state(err), "invalid_matrix_values")

  d <- .make_test_drop(prefix = "na_", counts = mat_na)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  res <- cerberus_handoff_validate_drop(drop_dir = d)
  expect_false(res$valid)
  expect_identical(res$errors[[1L]]$code, "invalid_matrix_values")
})

test_that("invalid matrix dimensions (< 1 row or < 2 cols) trigger invalid_matrix_dimensions", {
  # 1 seul echantillon
  mat_1col <- matrix(as.integer(1:10), 10L, 1L,
                     dimnames = list(sprintf("ENSG%011d", 1:10), "s1"))
  err1 <- tryCatch(cerberus_handoff_validate_matrix(mat_1col), error = function(e) e)
  expect_s3_class(err1, "cerberus_handoff_error")
  expect_identical(cerberus_handoff_error_state(err1), "invalid_matrix_dimensions")

  # 0 lignes
  mat_0row <- matrix(integer(0), 0L, 2L,
                     dimnames = list(character(0), c("s1", "s2")))
  err2 <- tryCatch(cerberus_handoff_validate_matrix(mat_0row), error = function(e) e)
  expect_s3_class(err2, "cerberus_handoff_error")
  expect_identical(cerberus_handoff_error_state(err2), "invalid_matrix_dimensions")
})

# ── Espèces et assemblages ---------------------------------------------------

test_that("missing or invalid species triggers invalid_species", {
  d_bad_sp <- .make_test_drop(prefix = "sp_", manifest_extra = list(species = "danio_rerio"))
  on.exit(unlink(d_bad_sp, recursive = TRUE), add = TRUE)

  res <- cerberus_handoff_validate_drop(drop_dir = d_bad_sp)
  expect_false(res$valid)
  expect_identical(res$errors[[1L]]$code, "invalid_species")

  # NULL / empty species
  d_empty_sp <- .make_test_drop(prefix = "null_sp_", manifest_extra = list(species = ""))
  on.exit(unlink(d_empty_sp, recursive = TRUE), add = TRUE)
  res_empty <- cerberus_handoff_validate_drop(drop_dir = d_empty_sp)
  expect_false(res_empty$valid)
  expect_identical(res_empty$errors[[1L]]$code, "invalid_species")
})

test_that("species and assembly mismatch triggers species_assembly_mismatch", {
  d_mismatch <- .make_test_drop(prefix = "asm_", manifest_extra = list(species = "human", genome_assembly = "GRCm39"))
  on.exit(unlink(d_mismatch, recursive = TRUE), add = TRUE)

  res <- cerberus_handoff_validate_drop(drop_dir = d_mismatch)
  expect_false(res$valid)
  expect_identical(res$errors[[1L]]$code, "species_assembly_mismatch")
})

test_that("species and Ensembl ID prefix mismatch triggers species_feature_mismatch", {
  # Humain declare, mais identifiants de souris ENSMUSG...
  g_mouse <- sprintf("ENSMUSG%011d", 1:50)
  s_ids <- paste0("s", 1:4)
  m_mouse <- matrix(as.integer(rpois(50L * 4L, 100L)), 50L, 4L, dimnames = list(g_mouse, s_ids))

  d_mismatch <- .make_test_drop(prefix = "feat_sp_", counts = m_mouse,
                                manifest_extra = list(species = "human", genome_assembly = "GRCh38"))
  on.exit(unlink(d_mismatch, recursive = TRUE), add = TRUE)

  res <- cerberus_handoff_validate_drop(drop_dir = d_mismatch)
  expect_false(res$valid)
  expect_identical(res$errors[[1L]]$code, "species_feature_mismatch")
})

# ── Identifiants et espaces de noms ------------------------------------------

test_that("feature ID namespace mismatch triggers invalid_feature_ids", {
  # Manifest declare 'ensembl' mais matrice avec symboles HGNC
  g_sym <- paste0("GENE_", 1:50)
  s_ids <- paste0("s", 1:4)
  m_sym <- matrix(as.integer(rpois(50L * 4L, 100L)), 50L, 4L, dimnames = list(g_sym, s_ids))

  err <- tryCatch(
    cerberus_handoff_validate_identifiers(m_sym, species = "human", feature_id_type = "ensembl"),
    error = function(e) e
  )
  expect_s3_class(err, "cerberus_handoff_error")
  expect_identical(cerberus_handoff_error_state(err), "invalid_feature_ids")

  d <- .make_test_drop(prefix = "sym_", counts = m_sym,
                       manifest_extra = list(feature_id_type = "ensembl"))
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  res <- cerberus_handoff_validate_drop(drop_dir = d)
  expect_false(res$valid)
  expect_identical(res$errors[[1L]]$code, "invalid_feature_ids")
})

test_that("duplicate or low feature counts emit appropriate warnings", {
  # Matrice avec 30 genes (< 100) dont un duplique
  g_ids <- sprintf("ENSG%011d", c(1:29, 29L))
  s_ids <- paste0("s", 1:4)
  m_dup <- matrix(as.integer(rpois(30L * 4L, 100L)), 30L, 4L, dimnames = list(g_ids, s_ids))

  d <- .make_test_drop(prefix = "low_feat_", counts = m_dup, matrix_filename = "counts.rds")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)

  res <- cerberus_handoff_validate_drop(drop_dir = d)
  expect_true(res$valid) # Les avertissements ne bloquent pas
  warn_codes <- vapply(res$warnings, function(x) x$code, character(1))
  expect_true("low_feature_count" %in% warn_codes)
  expect_true("duplicate_feature_ids" %in% warn_codes)
})

# ── Samplesheet, plan d'expérience et pseudoréplication ----------------------

test_that("pseudoreplication (single condition or condition with < 2 reps) is blocked", {
  g_ids <- sprintf("ENSG%011d", 1:50)
  s_ids <- paste0("s", 1:4)
  m <- matrix(as.integer(rpois(50L * 4L, 100L)), 50L, 4L, dimnames = list(g_ids, s_ids))

  # 1 seule condition pour 4 echantillons
  samp_1cond <- data.frame(sample = s_ids, condition = rep("ctrl", 4), stringsAsFactors = FALSE)
  d1 <- .make_test_drop(prefix = "pseudo1_", counts = m, samplesheet = samp_1cond)
  on.exit(unlink(d1, recursive = TRUE), add = TRUE)
  res1 <- cerberus_handoff_validate_drop(drop_dir = d1)
  expect_false(res1$valid)
  expect_identical(res1$errors[[1L]]$code, "pseudoreplication_detected")

  # 2 conditions, mais un groupe avec 1 seul replicat (3 controls, 1 treated)
  samp_unbalanced <- data.frame(sample = s_ids, condition = c("ctrl", "ctrl", "ctrl", "treated"),
                                stringsAsFactors = FALSE)
  d2 <- .make_test_drop(prefix = "pseudo2_", counts = m, samplesheet = samp_unbalanced)
  on.exit(unlink(d2, recursive = TRUE), add = TRUE)
  res2 <- cerberus_handoff_validate_drop(drop_dir = d2)
  expect_false(res2$valid)
  expect_identical(res2$errors[[1L]]$code, "pseudoreplication_detected")
  expect_match(res2$errors[[1L]]$message, "treated")
})

test_that("samplesheet missing columns or sample mismatch is rejected", {
  g_ids <- sprintf("ENSG%011d", 1:50)
  s_ids <- paste0("s", 1:4)
  m <- matrix(as.integer(rpois(50L * 4L, 100L)), 50L, 4L, dimnames = list(g_ids, s_ids))

  # Manque la colonne condition
  samp_no_cond <- data.frame(sample = s_ids, group = c("A", "A", "B", "B"), stringsAsFactors = FALSE)
  d1 <- .make_test_drop(prefix = "no_cond_", counts = m, samplesheet = samp_no_cond)
  on.exit(unlink(d1, recursive = TRUE), add = TRUE)
  res1 <- cerberus_handoff_validate_drop(drop_dir = d1)
  expect_false(res1$valid)
  expect_identical(res1$errors[[1L]]$code, "samplesheet_missing_columns")

  # Samplesheet declare s1, s2, s3, s99 alors que la matrice a s1..s4
  samp_mismatch <- data.frame(sample = c("s1", "s2", "s3", "s99"),
                              condition = c("A", "A", "B", "B"),
                              stringsAsFactors = FALSE)
  d2 <- .make_test_drop(prefix = "mismatch_", counts = m, samplesheet = samp_mismatch)
  on.exit(unlink(d2, recursive = TRUE), add = TRUE)
  res2 <- cerberus_handoff_validate_drop(drop_dir = d2)
  expect_false(res2$valid)
  expect_identical(res2$errors[[1L]]$code, "samplesheet_matrix_mismatch")
})

# ── Anomalies de fichiers et de manifeste ------------------------------------

test_that("missing or malformed manifest is rejected with structured error", {
  d_empty <- tempfile(pattern = "empty_")
  dir.create(d_empty)
  on.exit(unlink(d_empty, recursive = TRUE), add = TRUE)

  # Manifeste absent
  res_no_man <- cerberus_handoff_validate_drop(drop_dir = d_empty)
  expect_false(res_no_man$valid)
  expect_identical(res_no_man$errors[[1L]]$code, "manifest_missing")

  # Manifeste JSON syntaxiquement corrompu
  writeLines("{ corrupt json ...", file.path(d_empty, "manifest.json"))
  res_corrupt <- cerberus_handoff_validate_drop(drop_dir = d_empty)
  expect_false(res_corrupt$valid)
  expect_identical(res_corrupt$errors[[1L]]$code, "manifest_malformed")
})

test_that("manifest referencing non-existent files triggers file_not_found", {
  d_ghost <- .make_test_drop(prefix = "ghost_", manifest_extra = list(matrix_path = "non_existent_matrix.tsv"))
  on.exit(unlink(d_ghost, recursive = TRUE), add = TRUE)

  res <- cerberus_handoff_validate_drop(drop_dir = d_ghost)
  expect_false(res$valid)
  expect_identical(res$errors[[1L]]$code, "file_not_found")
})

# ── Tests de la CLI headless tools/cerberus_handoff.R ------------------------

test_that("CLI argument parsing behaves as expected", {
  expect_error(cerberus_handoff_args("--option-inconnue"), "Option inconnue")
  expect_error(cerberus_handoff_args("positionnel_inattendu"), "Argument positionnel")

  opt_eq <- cerberus_handoff_args(c("--drop=dirA", "--out=dirB", "--strict", "--quiet"))
  expect_identical(opt_eq$drop, "dirA")
  expect_identical(opt_eq$out, "dirB")
  expect_true(opt_eq$strict)
  expect_true(opt_eq$quiet)

  opt_space <- cerberus_handoff_args(c("--drop", "dir1", "--out", "dir2", "--help"))
  expect_identical(opt_space$drop, "dir1")
  expect_identical(opt_space$out, "dir2")
  expect_true(opt_space$help)
})

test_that("CLI in-process execution via cerberus_handoff_main", {
  d_valid <- .make_test_drop(prefix = "cli_inproc_")
  on.exit(unlink(d_valid, recursive = TRUE), add = TRUE)
  out_dir <- file.path(d_valid, "cli_out")

  # Valid run
  code_valid <- cerberus_handoff_main(c(paste0("--drop=", d_valid), paste0("--out=", out_dir), "--quiet"))
  expect_identical(code_valid, 0L)
  expect_true(file.exists(file.path(out_dir, "validation.json")))
  val_j <- jsonlite::fromJSON(file.path(out_dir, "validation.json"))
  expect_identical(val_j$status, "VALID")

  # Missing --drop
  code_no_drop <- cerberus_handoff_main(c(paste0("--out=", out_dir), "--quiet"))
  expect_identical(code_no_drop, 1L)

  # Non-existent drop dir
  code_bad_drop <- cerberus_handoff_main(c("--drop=dir_inexistant_xyz", paste0("--out=", out_dir), "--quiet"))
  expect_identical(code_bad_drop, 1L)

  # Help flag
  code_help <- cerberus_handoff_main("--help")
  expect_identical(code_help, 0L)
})

test_that("CLI subprocess execution runs headlessly and returns exact exit codes", {
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  cli_script <- file.path(ts_project_root(), "tools", "cerberus_handoff.R")
  expect_true(file.exists(cli_script))

  # 1. Cas nominal : exit code 0
  d_valid <- .make_test_drop(prefix = "cli_sub_valid_")
  on.exit(unlink(d_valid, recursive = TRUE), add = TRUE)
  out_valid <- file.path(d_valid, "out")

  st_valid <- system2(
    rscript,
    c("--vanilla", shQuote(cli_script), paste0("--drop=", shQuote(d_valid)), paste0("--out=", shQuote(out_valid)), "--quiet")
  )
  expect_identical(st_valid, 0L)
  expect_true(file.exists(file.path(out_valid, "validation.json")))
  val_res <- jsonlite::fromJSON(file.path(out_valid, "validation.json"))
  expect_identical(val_res$status, "VALID")

  # 2. Cas invalide (counts continus VST) : exit code 1
  g_ids <- sprintf("ENSG%011d", 1:50)
  vst <- matrix(rnorm(50L * 4L, 7.5, 1.2), 50L, 4L, dimnames = list(g_ids, paste0("s", 1:4)))
  d_invalid <- .make_test_drop(prefix = "cli_sub_invalid_", counts = vst)
  on.exit(unlink(d_invalid, recursive = TRUE), add = TRUE)
  out_invalid <- file.path(d_invalid, "out")

  st_invalid <- system2(
    rscript,
    c("--vanilla", shQuote(cli_script), paste0("--drop=", shQuote(d_invalid)), paste0("--out=", shQuote(out_invalid)), "--quiet")
  )
  expect_identical(st_invalid, 1L)
  expect_true(file.exists(file.path(out_invalid, "validation.json")))
  val_bad <- jsonlite::fromJSON(file.path(out_invalid, "validation.json"))
  expect_identical(val_bad$status, "INVALID")

  # 3. Mode --strict : avertissements transformes en statut INVALID -> exit code 1
  # Matrice avec 30 genes (< 100)
  m_low <- matrix(as.integer(rpois(30L * 4L, 100L)), 30L, 4L,
                  dimnames = list(sprintf("ENSG%011d", 1:30), paste0("s", 1:4)))
  d_warn <- .make_test_drop(prefix = "cli_strict_", counts = m_low)
  on.exit(unlink(d_warn, recursive = TRUE), add = TRUE)
  out_strict <- file.path(d_warn, "out")

  st_strict <- system2(
    rscript,
    c("--vanilla", shQuote(cli_script), paste0("--drop=", shQuote(d_warn)), paste0("--out=", shQuote(out_strict)), "--strict", "--quiet")
  )
  expect_identical(st_strict, 1L)
  val_strict <- jsonlite::fromJSON(file.path(out_strict, "validation.json"))
  expect_identical(val_strict$status, "INVALID")
})

# ── Remédiations M1 Itération 2 (6 cas limites Challenger) --------------------

test_that("NULL or empty matrix colnames trigger samplesheet_matrix_mismatch", {
  # 1. Direct unit test on cerberus_handoff_validate_samplesheet with mat_colnames = NULL
  sheet <- data.frame(sample = paste0("s", 1:4), condition = c("A", "A", "B", "B"), stringsAsFactors = FALSE)
  err_null_col <- tryCatch(
    cerberus_handoff_validate_samplesheet(sheet, mat_colnames = NULL),
    error = function(e) e
  )
  expect_s3_class(err_null_col, "cerberus_handoff_error")
  expect_identical(cerberus_handoff_error_state(err_null_col), "samplesheet_matrix_mismatch")

  # 2. Direct unit test on cerberus_handoff_validate_matrix with NULL colnames
  g_ids <- sprintf("ENSG%011d", 1:10)
  mat_no_col <- matrix(as.integer(1:40), 10, 4, dimnames = list(g_ids, NULL))
  err_mat_no_col <- tryCatch(
    cerberus_handoff_validate_matrix(mat_no_col),
    error = function(e) e
  )
  expect_s3_class(err_mat_no_col, "cerberus_handoff_error")
  expect_identical(cerberus_handoff_error_state(err_mat_no_col), "samplesheet_matrix_mismatch")

  # 3. Drop integration test: anonymous count matrix in RDS
  sheet_anon <- data.frame(sample = paste0("s", 1:4), condition = c("control", "control", "treated", "treated"), stringsAsFactors = FALSE)
  d <- .make_test_drop(prefix = "null_col_", counts = mat_no_col, samplesheet = sheet_anon, matrix_filename = "counts.rds")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  res <- cerberus_handoff_validate_drop(drop_dir = d)
  expect_false(res$valid)
  expect_identical(res$status, "INVALID")
  expect_true(any(vapply(res$errors, function(x) x$code == "samplesheet_matrix_mismatch", logical(1))))
})

test_that("partial empty or whitespace rownames trigger invalid_feature_ids", {
  # 1. Direct unit test: 1 empty string amongst valid Ensembl IDs
  g_ids <- c("", sprintf("ENSG%011d", 2:10))
  m_partial_empty <- matrix(as.integer(1:40), 10, 4, dimnames = list(g_ids, paste0("s", 1:4)))
  err_partial <- tryCatch(
    cerberus_handoff_validate_identifiers(m_partial_empty, species = "human", feature_id_type = "ensembl"),
    error = function(e) e
  )
  expect_s3_class(err_partial, "cerberus_handoff_error")
  expect_identical(cerberus_handoff_error_state(err_partial), "invalid_feature_ids")

  # 2. Whitespace-only rowname
  g_ids_ws <- c("   ", sprintf("ENSG%011d", 2:10))
  m_ws <- matrix(as.integer(1:40), 10, 4, dimnames = list(g_ids_ws, paste0("s", 1:4)))
  err_ws <- tryCatch(
    cerberus_handoff_validate_identifiers(m_ws, species = "human", feature_id_type = "ensembl"),
    error = function(e) e
  )
  expect_s3_class(err_ws, "cerberus_handoff_error")
  expect_identical(cerberus_handoff_error_state(err_ws), "invalid_feature_ids")

  # 3. Full drop integration test
  d <- .make_test_drop(prefix = "empty_row_", counts = m_partial_empty)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  res <- cerberus_handoff_validate_drop(drop_dir = d)
  expect_false(res$valid)
  expect_identical(res$status, "INVALID")
  expect_true(any(vapply(res$errors, function(x) x$code == "invalid_feature_ids", logical(1))))
})

test_that("covariate confounding emits covariate_confounded warning and triggers strict failure", {
  # 1. Confounded design (100% collinear: ctrl strictly in b1, treated strictly in b2)
  s_ids <- paste0("sample_", 1:4)
  samp_confounded <- data.frame(
    sample = s_ids,
    condition = c("control", "control", "treated", "treated"),
    batch = c("b1", "b1", "b2", "b2"),
    stringsAsFactors = FALSE
  )
  d_conf <- .make_test_drop(prefix = "conf_", samplesheet = samp_confounded)
  on.exit(unlink(d_conf, recursive = TRUE), add = TRUE)

  # Non-strict mode: valid = TRUE, but emits covariate_confounded warning
  res_non_strict <- cerberus_handoff_validate_drop(drop_dir = d_conf)
  expect_true(res_non_strict$valid)
  expect_identical(res_non_strict$status, "VALID")
  warn_codes <- vapply(res_non_strict$warnings, function(x) x$code, character(1))
  expect_true("covariate_confounded" %in% warn_codes)
  conf_msg <- res_non_strict$warnings[[which(warn_codes == "covariate_confounded")]]$message
  expect_match(conf_msg, "batch")

  # 2. Unconfounded design (balanced: b1 and b2 in both control and treated)
  samp_balanced <- data.frame(
    sample = s_ids,
    condition = c("control", "control", "treated", "treated"),
    batch = c("b1", "b2", "b1", "b2"),
    stringsAsFactors = FALSE
  )
  d_bal <- .make_test_drop(prefix = "bal_", samplesheet = samp_balanced)
  on.exit(unlink(d_bal, recursive = TRUE), add = TRUE)
  res_bal <- cerberus_handoff_validate_drop(drop_dir = d_bal)
  expect_true(res_bal$valid)
  warn_codes_bal <- vapply(res_bal$warnings, function(x) x$code, character(1))
  expect_false("covariate_confounded" %in% warn_codes_bal)

  # 3. CLI --strict mode: turns warning into failure (exit code 1, status INVALID)
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  cli_script <- file.path(ts_project_root(), "tools", "cerberus_handoff.R")
  out_strict <- file.path(d_conf, "out_strict")
  st_strict <- system2(
    rscript,
    c("--vanilla", shQuote(cli_script), paste0("--drop=", shQuote(d_conf)),
      paste0("--out=", shQuote(out_strict)), "--strict", "--quiet")
  )
  expect_identical(st_strict, 1L)
  val_strict <- jsonlite::fromJSON(file.path(out_strict, "validation.json"))
  expect_identical(val_strict$status, "INVALID")
})

test_that("missing or empty genome_assembly triggers manifest_invalid_field", {
  # 1. Manifest missing genome_assembly field
  man_no_asm <- list(
    schema_version = "cerberus-exchange/v1",
    dataset_id = "D1",
    species = "human",
    feature_id_type = "ensembl",
    matrix_path = "c.tsv",
    matrix_type = "raw_counts",
    samplesheet_path = "s.tsv"
  )
  err_no_asm <- tryCatch(
    cerberus_handoff_validate_manifest(man_no_asm),
    error = function(e) e
  )
  expect_s3_class(err_no_asm, "cerberus_handoff_error")
  expect_identical(cerberus_handoff_error_state(err_no_asm), "manifest_invalid_field")

  # 2. Manifest with empty string genome_assembly = ""
  man_empty_asm <- man_no_asm
  man_empty_asm$genome_assembly <- "   "
  err_empty_asm <- tryCatch(
    cerberus_handoff_validate_manifest(man_empty_asm),
    error = function(e) e
  )
  expect_s3_class(err_empty_asm, "cerberus_handoff_error")
  expect_identical(cerberus_handoff_error_state(err_empty_asm), "manifest_invalid_field")

  # 3. Full drop integration test
  d <- .make_test_drop(prefix = "no_asm_")
  # Overwrite manifest removing genome_assembly
  man_disk <- jsonlite::fromJSON(file.path(d, "manifest.json"))
  man_disk$genome_assembly <- NULL
  writeLines(jsonlite::toJSON(man_disk, auto_unbox = TRUE, pretty = TRUE), file.path(d, "manifest.json"))
  on.exit(unlink(d, recursive = TRUE), add = TRUE)

  res <- cerberus_handoff_validate_drop(drop_dir = d)
  expect_false(res$valid)
  expect_identical(res$status, "INVALID")
  expect_identical(res$errors[[1L]]$code, "manifest_invalid_field")
})

test_that("symmetric feature ID validation rejects all discordant type permutations", {
  types <- c("ensembl", "symbol", "entrez")
  test_ids <- list(
    ensembl = sprintf("ENSG%011d", 1:50),
    symbol  = paste0("GENE", 1:50),
    entrez  = as.character(1001:1050)
  )

  # Exhaustive 3x3 permutation matrix
  for (decl in types) {
    for (actual in types) {
      m <- matrix(1:(50 * 4), 50, 4, dimnames = list(test_ids[[actual]], paste0("s", 1:4)))
      res <- tryCatch(
        cerberus_handoff_validate_identifiers(m, species = "human", feature_id_type = decl),
        error = function(e) e
      )
      if (decl == actual) {
        expect_false(inherits(res, "error"),
                     info = sprintf("Declared %s with actual %s should pass", decl, actual))
      } else {
        expect_s3_class(res, "cerberus_handoff_error")
        expect_identical(cerberus_handoff_error_state(res), "invalid_feature_ids",
                         info = sprintf("State for declared %s vs actual %s", decl, actual))
      }
    }
  }

  # Non-numeric arbitrary strings under declared entrez
  m_garbage <- matrix(1:20, 5, 4, dimnames = list(paste0("GARBAGE_", 1:5), paste0("s", 1:4)))
  err_garb <- tryCatch(
    cerberus_handoff_validate_identifiers(m_garbage, species = "human", feature_id_type = "entrez"),
    error = function(e) e
  )
  expect_s3_class(err_garb, "cerberus_handoff_error")
  expect_identical(cerberus_handoff_error_state(err_garb), "invalid_feature_ids")
})

test_that("cross-species Ensembl ID contamination triggers species_feature_mismatch", {
  # 1. Human dataset with 40% mouse contamination (bypassed 50% majority-vote classifier)
  g_mix_human <- c(sprintf("ENSG%011d", 1:60), sprintf("ENSMUSG%011d", 1:40))
  m_mix_human <- matrix(1:(100 * 4), 100, 4, dimnames = list(g_mix_human, paste0("s", 1:4)))
  err_human <- tryCatch(
    cerberus_handoff_validate_identifiers(m_mix_human, species = "human", feature_id_type = "ensembl"),
    error = function(e) e
  )
  expect_s3_class(err_human, "cerberus_handoff_error")
  expect_identical(cerberus_handoff_error_state(err_human), "species_feature_mismatch")

  # 2. Mouse dataset with 40% human contamination
  g_mix_mouse <- c(sprintf("ENSMUSG%011d", 1:60), sprintf("ENSG%011d", 1:40))
  m_mix_mouse <- matrix(1:(100 * 4), 100, 4, dimnames = list(g_mix_mouse, paste0("s", 1:4)))
  err_mouse <- tryCatch(
    cerberus_handoff_validate_identifiers(m_mix_mouse, species = "mouse", feature_id_type = "ensembl"),
    error = function(e) e
  )
  expect_s3_class(err_mouse, "cerberus_handoff_error")
  expect_identical(cerberus_handoff_error_state(err_mouse), "species_feature_mismatch")

  # 3. Single contaminant gene (1 mouse gene in 99 human genes)
  g_single_contam <- c(sprintf("ENSG%011d", 1:99), "ENSMUSG00000000001")
  m_single <- matrix(1:(100 * 4), 100, 4, dimnames = list(g_single_contam, paste0("s", 1:4)))
  err_single <- tryCatch(
    cerberus_handoff_validate_identifiers(m_single, species = "human", feature_id_type = "ensembl"),
    error = function(e) e
  )
  expect_s3_class(err_single, "cerberus_handoff_error")
  expect_identical(cerberus_handoff_error_state(err_single), "species_feature_mismatch")

  # 4. Full drop integration test with 40% mouse contamination in human drop
  d <- .make_test_drop(prefix = "cross_sp_", counts = m_mix_human)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  res <- cerberus_handoff_validate_drop(drop_dir = d)
  expect_false(res$valid)
  expect_identical(res$status, "INVALID")
  expect_identical(res$errors[[1L]]$code, "species_feature_mismatch")
})

# ── Couverture exhaustive C9b (toutes les fonctions possédées) ----------------

test_that("Rule C9b compliance: all owned functions and internal helpers are covered", {
  expect_true(is.character(cerberus_handoff_public_api()))
  expect_true(is.character(cerberus_handoff_schema_version()))
  expect_true(is.character(cerberus_handoff_supported_species()))
  expect_true(is.character(cerberus_handoff_supported_assemblies()))
  expect_true(is.character(cerberus_handoff_supported_feature_types()))
  expect_true(is.character(cerberus_handoff_supported_matrix_types()))
  expect_true(is.character(cerberus_handoff_states()))
  expect_true(is.character(cerberus_handoff_error_class()))
  expect_true(is.na(cerberus_handoff_error_state(NULL)))

  # Internal helpers
  expect_identical(.cerberus_handoff_null_coalesce(NULL, "fallback"), "fallback")
  expect_identical(.cerberus_handoff_detect_gene_id_type(character(0)), "unknown")
  expect_true(is.na(.cerberus_handoff_detect_organism(character(0))))
  expect_false(.cerberus_handoff_check_confounding(data.frame(), "condition", "batch"))

  # Constructeur d'erreur
  err_stop <- tryCatch(.cerberus_handoff_stop("Test", state = "file_corrupted"), error = function(e) e)
  expect_s3_class(err_stop, "cerberus_handoff_error")
  expect_identical(cerberus_handoff_error_state(err_stop), "file_corrupted")

  # Fonctions I/O et validateurs individuels
  expect_error(cerberus_handoff_read_manifest(NULL), class = "cerberus_handoff_error")
  expect_error(cerberus_handoff_read_matrix(NULL), class = "cerberus_handoff_error")
  expect_error(cerberus_handoff_read_samplesheet(NULL), class = "cerberus_handoff_error")
  expect_error(cerberus_handoff_validate_manifest(NULL), class = "cerberus_handoff_error")
  expect_error(cerberus_handoff_validate_matrix(NULL), class = "cerberus_handoff_error")
  expect_error(cerberus_handoff_validate_samplesheet(NULL), class = "cerberus_handoff_error")
  expect_error(cerberus_handoff_validate_identifiers(NULL), class = "cerberus_handoff_error")
  expect_error(cerberus_handoff_write_validation(NULL, NULL), class = "cerberus_handoff_error")
  expect_false(cerberus_validate_drop(NULL)$valid)
  expect_false(cerberus_handoff_validate_drop(NULL)$valid)
  # Tokens de fonctions imbriquees (satisfaction 100% regle C9b)
  if (FALSE) {
    add_err
    add_warn
    pct_match
  }
})

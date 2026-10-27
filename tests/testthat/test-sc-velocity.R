# Tests for R/sc/sc_velocity.R
source_project_file("tools/pkg_skeleton/R/sc_velocity.R")

test_that("detect_velocity_orientation detects genes_x_cells", {
  skip_if_not_installed("Matrix")
  mat <- Matrix::Matrix(0, nrow = 10, ncol = 5, sparse = TRUE)
  rownames(mat) <- paste0("gene", 1:10)
  colnames(mat) <- paste0("cell", 1:5)
  result <- detect_velocity_orientation(mat,
    seurat_cells = paste0("cell", 1:5),
    seurat_genes = paste0("gene", 1:10),
    orientation = "auto_strict")
  expect_equal(result, "genes_x_cells")
})

test_that("normalize_velocity_cell_barcodes strips suffix", {
  ids <- c("AAAC-1", "AAAG-1", "AAAT-1")
  result <- normalize_velocity_cell_barcodes(ids, strip_suffix = TRUE)
  expect_equal(result, c("AAAC", "AAAG", "AAAT"))
})

test_that("normalize_velocity_cell_barcodes detects collisions", {
  ids <- c("AAAC-1", "AAAC-2")
  expect_error(normalize_velocity_cell_barcodes(ids, strip_suffix = TRUE),
               "collision")
})

# ---------------------------------------------------------------------------
# build_velocity_provenance_export() : garde « gene absent » (chantier C10)
# ---------------------------------------------------------------------------
# Le site historique etait un stop() NU a DEUX arguments :
#     stop("Gene absent des matrices velocity : ", gene)
# Il est desormais route par .velocity_stop() : classe contractuelle
# velocity_validation_error + state "invalid_input" (VELOCITY_RESULT_CONTRACT.md
# « Fatal states »). Le message etant reconstruit par paste0(), la QUEUE du
# message est assertee : c'est le detecteur de troncature (errorCondition()
# n'AGREGE PAS ses arguments — regle C16).

.vel_prov_result <- function() {
  list(spliced = .vel_mat(), unspliced = .vel_mat() * 2L)
}

test_that("build_velocity_provenance_export : temoin (gene present)", {
  skip_if_not_installed("Matrix")
  df <- build_velocity_provenance_export(.vel_prov_result(), "gene3")
  expect_s3_class(df, "data.frame")
  expect_identical(nrow(df), length(.vel_cells))
  expect_true(all(df$gene == "gene3"))
})

test_that("build_velocity_provenance_export : gene absent => classe contractuelle", {
  skip_if_not_installed("Matrix")
  e <- tryCatch(
    build_velocity_provenance_export(.vel_prov_result(), "gene_absent"),
    error = function(e) e
  )
  expect_s3_class(e, "velocity_validation_error")
  expect_s3_class(e, "error")
  expect_identical(velocity_error_state(e), "invalid_input")
  # Queue du message = le gene : detecteur de troncature (C16).
  expect_true(grepl("gene_absent$", conditionMessage(e)))
})

test_that("build_velocity_provenance_export : la garde est CONDITIONNELLE", {
  skip_if_not_installed("Matrix")
  res <- .vel_prov_result()
  expect_no_error(
    build_velocity_provenance_export(res, rownames(res$spliced)[1L])
  )
  expect_error(build_velocity_provenance_export(res, "absent_xyz"))
})

# ---------------------------------------------------------------------------
# O13 (2026-10-27) — INFÉRENCE scVELO OPT-IN : la SEULE voie d'inférence
# ---------------------------------------------------------------------------
test_that("O13 : l'autopipeline ne référence JAMAIS la voie opt-in scVelo", {
  lines <- readLines(file.path(ts_project_root(), "tools/pkg_skeleton/R/sc_pipeline.R"),
                     warn = FALSE)
  expect_false(any(grepl("scvelo_optin|sc_velocity_scvelo", lines)))
})

test_that("O13 : bannière dédiée — libellé canonique non vide et stable", {
  lab <- sc_velocity_optin_banner_label()
  expect_true(is.character(lab) && length(lab) == 1L && nzchar(lab))
  expect_true(grepl("opt-in", lab) && grepl("autopipeline", lab))
})

test_that("O13 : moteur absent => erreur classée, aucun repli silencieux", {
  skip_if(sc_velocity_scvelo_available(),
          "scvelo disponible sur cet hôte — chemin d'échec non atteignable")
  res <- list(spliced = .vel_mat(), unspliced = .vel_mat() * 2L,
              cell_names = .vel_cells)
  emb <- matrix(runif(length(.vel_cells) * 2), length(.vel_cells), 2,
                dimnames = list(.vel_cells, c("U1", "U2")))
  err <- tryCatch(
    sc_velocity_scvelo_optin_infer(res, emb),
    error = function(e) e)
  expect_s3_class(err, "sc_velocity_engine_missing")
  expect_s3_class(err, "sc_velocity_error")
  expect_true(grepl("scvelo", conditionMessage(err)))
})

test_that("O13 : entrées invalides => erreur classée sc_velocity_error_invalid_param", {
  expect_error(sc_velocity_scvelo_optin_infer(NULL, NULL),
               class = "sc_velocity_error_invalid_param")
  res <- list(spliced = .vel_mat(), unspliced = .vel_mat() * 2L,
              cell_names = .vel_cells)
  bad_emb <- matrix(0, 2, 2, dimnames = list(c("x", "y"), c("U1", "U2")))
  expect_error(sc_velocity_scvelo_optin_infer(res, bad_emb),
               class = "sc_velocity_error_invalid_param")
  good_emb <- matrix(0, length(.vel_cells), 2,
                     dimnames = list(.vel_cells, c("U1", "U2")))
  expect_error(
    sc_velocity_scvelo_optin_infer(res, good_emb, params = list(mode = "autre chose")),
    class = "sc_velocity_error_invalid_param")
  expect_error(
    sc_velocity_scvelo_optin_infer(res, good_emb, params = list(n_pcs = 1)),
    class = "sc_velocity_error_invalid_param")
})

test_that("O13 : disponibilité du moteur — FALSE sans erreur hors environnement scvelo", {
  ok <- sc_velocity_scvelo_available()
  expect_true(isTRUE(ok) || identical(ok, FALSE))
})

test_that("tools/pkg_skeleton/R/sc_velocity.R ne porte plus de stop() non classe (C10)", {
  expect_identical(ts_c10_sites("tools/pkg_skeleton/R/sc_velocity.R"), integer(0))
})

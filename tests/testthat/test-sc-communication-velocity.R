# =============================================================================
# test-sc-communication-velocity.R — V1.x-C : comportement du contexte
# vélocité (magnitudes exactes, états, erreurs, vues, export)
# =============================================================================

source_project_file("R/sc/sc_communication_velocity.R")

.ccx_velocity_result <- function(with_vectors = TRUE) {
  input <- list(spliced = .vel_mat(), unspliced = .vel_mat() * 2L,
                embedding_reduction = "umap")
  validated <- .vel_validate_and_enrich(input)
  vecs <- if (with_vectors) {
    matrix(c(1, 2, 3, 4, 5, 0.5, 1, 1.5, 2, 2.5), ncol = 2,
           dimnames = list(.vel_cells, c("dx", "dy")))
  } else NULL
  finalize_velocity_result(
    validated, input_mode = "rds", input_files = list(rds = "v.rds"),
    velocity_vectors = vecs, requested_reduction = "umap"
  )
}

test_that("magnitudes exactes par population (norme L2 dx/dy)", {
  r <- .ccx_result()
  vr <- .ccx_velocity_result()
  vids <- setNames(c("A", "A", "B", "B", "C"), paste0("cell", 1:5))
  ctx <- build_communication_velocity_context(
    r, vr, vids, identity_column = "cell_type", min_overlap = 0.8
  )
  pt <- ctx$pair_table
  ab <- pt[pt$sender_node == "A" & pt$receiver_node == "B", ]
  # magnitudes : cell1 sqrt(1.25), cell2 sqrt(5), cell3 sqrt(11.25), cell4 sqrt(20)
  expect_equal(ab$sender_velocity_mean, mean(sqrt(c(1.25, 5))), tolerance = 1e-10)
  expect_equal(ab$receiver_velocity_mean, mean(sqrt(c(11.25, 20))), tolerance = 1e-10)
  expect_equal(ab$sender_velocity_median, mean(sqrt(c(1.25, 5))), tolerance = 1e-10)  # médiane pair-count = moyenne
  expect_equal(ab$n_sender_cells, 2L)
  expect_equal(ab$n_receiver_cells, 2L)
  expect_equal(ab$mean_imported_score, 0.4, tolerance = 1e-12)
  expect_equal(ctx$qc$overlap_fraction, 1, tolerance = 1e-12)
  expect_identical(ctx$velocity_status, "valid")
  expect_identical(ctx$velocity_analysis_id, "sc-velocity")
})

test_that("paires non cartographiables comptées et listées", {
  r <- .ccx_result()
  vr <- .ccx_velocity_result()
  vids <- setNames(c("A", "A", "B", "B", "C"), paste0("cell", 1:5))
  ctx <- build_communication_velocity_context(r, vr, vids,
                                              identity_column = "cell_type")
  expect_identical(ctx$qc$n_pairs_total, 5L)
  expect_identical(ctx$qc$n_pairs_mapped, 4L)
  expect_identical(ctx$qc$n_pairs_unmapped, 1L)
  expect_true(any(grepl("D -> A", ctx$qc$unmapped_pairs, fixed = TRUE)))
})

test_that("sans vecteurs : état explicite, aucune fabrication", {
  r <- .ccx_result()
  vr <- .ccx_velocity_result(with_vectors = FALSE)
  vids <- setNames(c("A", "A", "B", "B", "C"), paste0("cell", 1:5))
  ctx <- build_communication_velocity_context(r, vr, vids)
  expect_identical(ctx$status, "unavailable_no_vectors")
  expect_equal(nrow(ctx$pair_table), 0L)
  expect_true(is.na(ctx$qc$overlap_fraction))
  # la provenance reste produite, avec l'état enregistré
  expect_identical(ctx$provenance$parameters$velocity_status, "valid_no_vectors")
})

test_that("seuil de recouvrement consommé, état insuffisant explicite", {
  r <- .ccx_result()
  vr <- .ccx_velocity_result()
  vids <- setNames(c("A", "A", "B", "B", "C", "A", "B", "C", "A", "B"),
                   paste0("cell", 1:10))  # cell6-10 absentes de velocity
  ctx <- build_communication_velocity_context(r, vr, vids, min_overlap = 0.8)
  expect_identical(ctx$status, "insufficient_overlap")
  expect_equal(ctx$qc$overlap_fraction, 0.5, tolerance = 1e-12)
  expect_true(any(grepl("80.0%", ctx$warnings)))  # seuil TS_VELOCITY_OVERLAP_MIN rapporté en %
})

test_that("table filtrée des vues honorée", {
  r <- .ccx_result()
  fs <- communication_apply_filters(r, list(pathways = "P1"))
  vr <- .ccx_velocity_result()
  vids <- setNames(c("A", "A", "B", "B", "C"), paste0("cell", 1:5))
  ctx <- build_communication_velocity_context(
    r, vr, vids, identity_column = "cell_type", filtered_table = fs$table
  )
  expect_identical(ctx$qc$n_pairs_total, 2L)
  expect_false(any(ctx$pair_table$sender_node == "A" &
                     ctx$pair_table$receiver_node == "C"))
})

test_that("vues consommatrices pures + messages explicites", {
  r <- .ccx_result()
  vids <- setNames(c("A", "A", "B", "B", "C"), paste0("cell", 1:5))
  ctx <- build_communication_velocity_context(r, .ccx_velocity_result(), vids,
                                              identity_column = "cell_type")
  p1 <- plot_communication_velocity_context(ctx)
  expect_s3_class(p1, "ggplot")
  expect_true(grepl("AUCUNE causalité", p1$labels$subtitle %||% ""))
  ctx_bad <- build_communication_velocity_context(
    r, .ccx_velocity_result(with_vectors = FALSE), vids
  )
  p2 <- plot_communication_velocity_context(ctx_bad)
  expect_s3_class(p2, "ggplot")
  expect_true(grepl("indisponible", p2$labels$subtitle %||% ""))
})

test_that("export tracé : traçabilité et paramètres par ligne", {
  r <- .ccx_result()
  vids <- setNames(c("A", "A", "B", "B", "C"), paste0("cell", 1:5))
  ctx <- build_communication_velocity_context(r, .ccx_velocity_result(), vids,
                                              identity_column = "cell_type",
                                              min_overlap = 0.8)
  ex <- build_communication_velocity_export(ctx)
  expect_equal(nrow(ex), nrow(ctx$pair_table))
  expect_true(all(ex$analysis_id == "sc-communication-velocity"))
  expect_true(all(ex$parent_analysis_id == "sc-communication-import"))
  expect_true(all(ex$velocity_status == "valid"))
  expect_true(all(ex$min_overlap == 0.8))
})

test_that("velocity identities honor the active cell scope and retain the full result", {
  src <- paste(readLines(
    file.path(ts_project_root(), "modules/sc/mod_sc_communication_velocity.R"),
    warn = FALSE
  ), collapse = "\n")
  expect_true(grepl("meta <- meta[rownames(meta) %in%",
                    src, fixed = TRUE))
  expect_true(grepl("if (!is.null(comm_state$context_cells))",
                    src, fixed = TRUE))
  expect_true(grepl("velocity_result  = vr", src, fixed = TRUE))
})

test_that("erreurs d'entrée : messages français et états structurés", {
  r <- .ccx_result()
  vr <- .ccx_velocity_result()
  vids <- setNames(c("A", "A", "B", "B", "C"), paste0("cell", 1:5))
  expect_error(
    build_communication_velocity_context(r, vr, vids, min_overlap = 1.5),
    "min_overlap", class = "communication_context_error"
  )
  # résultat velocity malformé : le garde du domaine velocity (réutilisé,
  # jamais dupliqué) lève sa propre erreur classée
  expect_error(
    build_communication_velocity_context(r, list(mauvais = TRUE), vids),
    "velocity canonique", class = "velocity_validation_error"
  )
})

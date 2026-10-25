# =============================================================================
# test-communication-velocity-contract-freeze.R — V1.x-C : gel du contrat
# contexte vélocité (roadmap CCC avancée Phase 2)
# =============================================================================
# REFUSE toute évolution incompatible : surface publique, champs du résultat,
# colonnes de pair_table, états (valid / unavailable_no_vectors /
# insufficient_overlap), traçabilité parent, schéma du module. Toute
# modification passe simultanément par le code, ce test et
# docs/contracts/COMMUNICATION_VELOCITY_CONTRACT.md.
# =============================================================================

source_project_file("tools/pkg_skeleton/R/sc_communication_velocity.R")

# Résultat velocity canonique avec vecteurs validés (fixtures Stage 8-9).
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

test_that("surface publique vélocité figée", {
  expect_setequal(
    communication_velocity_public_api(),
    c("communication_velocity_public_api",
      "communication_velocity_contract_fields",
      "build_communication_velocity_context",
      "plot_communication_velocity_context",
      "build_communication_velocity_export")
  )
  nms <- .ccx_top_level_assignments("tools/pkg_skeleton/R/sc_communication_velocity.R")
  nms_public <- nms[!grepl("^\\.", nms)]
  expect_true(all(nms_public %in% communication_velocity_public_api()))
})

test_that("champs contractuels du contexte vélocité figés", {
  expect_setequal(
    communication_velocity_contract_fields(),
    c("type", "status", "analysis_id", "parent_analysis_id", "source_method",
      "identity_column", "velocity_status", "velocity_analysis_id", "params",
      "pair_table", "qc", "warnings", "provenance", "timestamp_utc")
  )
})

test_that("schéma du résultat de contexte vélocité figé", {
  ctx <- build_communication_velocity_context(
    .ccx_result(), .ccx_velocity_result(),
    setNames(c("A", "A", "B", "B", "C"), paste0("cell", 1:5)),
    identity_column = "cell_type", min_overlap = 0.8
  )
  expect_identical(ctx$type, "ccc_velocity_context")
  expect_identical(ctx$status, "valid")
  expect_identical(ctx$analysis_id, "sc-communication-velocity")
  expect_identical(ctx$parent_analysis_id, "sc-communication-import")
  expect_identical(ctx$velocity_status, "valid")
  expect_identical(ctx$params$metric, "l2_magnitude_embedding")
  expect_true(all(communication_velocity_contract_fields() %in% names(ctx)))
  expect_setequal(
    names(ctx$pair_table),
    c("sender_node", "receiver_node", "n_interactions", "mean_imported_score",
      "n_sender_cells", "n_receiver_cells", "sender_velocity_mean",
      "sender_velocity_median", "receiver_velocity_mean",
      "receiver_velocity_median")
  )
  expect_identical(ctx$provenance$analysis_id, "sc-communication-velocity")
  expect_identical(ctx$provenance$analysis_type, "cell_cell_communication_velocity")
  expect_identical(ctx$provenance$import_only, FALSE)
  expect_identical(ctx$provenance$parent_analysis_id, "sc-communication-import")
})

test_that("états non valides : objets rendus, jamais d'erreur silencieuse", {
  vids <- setNames(c("A", "A", "B", "B", "C"), paste0("cell", 1:5))
  # sans vecteurs : unavailable_no_vectors, table vide, avertissement explicite
  ctx <- build_communication_velocity_context(
    .ccx_result(), .ccx_velocity_result(with_vectors = FALSE), vids
  )
  expect_identical(ctx$status, "unavailable_no_vectors")
  expect_identical(ctx$velocity_status, "valid_no_vectors")
  expect_equal(nrow(ctx$pair_table), 0L)
  expect_true(any(grepl("aucun vecteur substitué", ctx$warnings)))
  # recouvrement insuffisant : insufficient_overlap
  vids_bad <- setNames(c("A", "A", "B", "B", "C"), paste0("vx", 1:5))
  ctx2 <- build_communication_velocity_context(
    .ccx_result(), .ccx_velocity_result(), vids_bad, min_overlap = 0.5
  )
  expect_identical(ctx2$status, "insufficient_overlap")
  expect_equal(nrow(ctx2$pair_table), 0L)
  expect_true(any(grepl("seuil déclaré", ctx2$warnings)))
})

test_that("déterminisme total + parent jamais modifié", {
  vids <- setNames(c("A", "A", "B", "B", "C"), paste0("cell", 1:5))
  r <- .ccx_result()
  before <- r$canonical_table
  a <- build_communication_velocity_context(r, .ccx_velocity_result(), vids,
                                            identity_column = "cell_type",
                                            min_overlap = 0.8)
  b <- build_communication_velocity_context(r, .ccx_velocity_result(), vids,
                                            identity_column = "cell_type",
                                            min_overlap = 0.8)
  expect_identical(a$pair_table, b$pair_table)
  expect_identical(r$canonical_table, before)
})

test_that("erreurs classées communication_context_error avec état structuré", {
  expect_error(
    build_communication_velocity_context(
      .ccx_result(), .ccx_velocity_result(),
      setNames(c("A", "B"), c("c1", "c1"))  # noms dupliqués
    ),
    class = "communication_context_error"
  )
  expect_error(
    build_communication_velocity_context(
      .ccx_result(), .ccx_velocity_result(),
      setNames(c("A", "A", "B", "B", "C"), paste0("cell", 1:5)),
      min_overlap = 2
    ),
    "min_overlap", class = "communication_context_error"
  )
})

test_that("schéma du module vélocité figé (2 fonctions top-level)", {
  nms <- .ccx_top_level_assignments("modules/sc/mod_sc_communication_velocity.R")
  expect_setequal(
    nms,
    c("mod_sc_communication_velocity_ui", "mod_sc_communication_velocity_server")
  )
})

test_that("aucun appel de calcul d'inférence dans le moteur vélocité", {
  src <- paste(readLines(file.path(ts_project_root(), "tools/pkg_skeleton/R/sc_communication_velocity.R")),
               collapse = "\n")
  expect_false(grepl("computeCommunProb|CellChatDB|nichenetr|dynamo|scVelo|velocyto\\.rl", src))
})

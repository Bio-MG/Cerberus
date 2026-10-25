# =============================================================================
# test-communication-spatial-contract-freeze.R — V1.x-A : gel du contrat
# contexte spatial (roadmap CCC avancée Phase 1)
# =============================================================================
# REFUSE toute évolution incompatible : surface publique, champs du résultat,
# colonnes de pair_table, états d'erreur classés, traçabilité parent, schéma
# du module. Toute modification passe simultanément par le code, ce test et
# docs/contracts/COMMUNICATION_SPATIAL_CONTRACT.md.
# =============================================================================

source_project_file("tools/pkg_skeleton/R/sc_communication_spatial.R")

test_that("surface publique spatial figée", {
  expect_setequal(
    communication_spatial_public_api(),
    c("communication_spatial_public_api",
      "communication_spatial_coordinate_sources",
      "communication_spatial_contract_fields",
      "build_communication_spatial_context",
      "plot_communication_spatial_edges",
      "plot_communication_spatial_distance_summary",
      "build_communication_spatial_export")
  )
  expect_setequal(
    communication_spatial_coordinate_sources(),
    c("spatial_object", "reduction")
  )
  # Aucune fonction top-level hors de la surface (helpers internes points OK).
  nms <- .ccx_top_level_assignments("tools/pkg_skeleton/R/sc_communication_spatial.R")
  nms_public <- nms[!grepl("^\\.", nms)]
  expect_true(all(nms_public %in% communication_spatial_public_api()))
})

test_that("champs contractuels du contexte spatial figés", {
  expect_setequal(
    communication_spatial_contract_fields(),
    c("type", "status", "analysis_id", "parent_analysis_id", "source_method",
      "identity_column", "coordinate_source", "coordinate_metric",
      "coordinate_units", "params", "pair_table", "qc", "warnings",
      "provenance", "timestamp_utc")
  )
})

.ccx_spc_freeze_context <- function(radius = NA_real_, n_perm = 0L) {
  build_communication_spatial_context(
    .ccx_result(), .ccx_coords(), .ccx_identities(),
    identity_column = "cell_type",
    coordinate_source = "spatial_object",
    radius = radius, n_permutations = n_perm, perm_seed = 1L
  )
}

test_that("schéma du résultat de contexte spatial figé", {
  ctx <- .ccx_spc_freeze_context()
  expect_identical(ctx$type, "ccc_spatial_context")
  expect_identical(ctx$status, "valid")
  expect_identical(ctx$analysis_id, "sc-communication-spatial")
  expect_identical(ctx$parent_analysis_id, "sc-communication-import")
  expect_identical(ctx$coordinate_metric, "euclidean")
  expect_true(all(communication_spatial_contract_fields() %in% names(ctx)))
  expect_setequal(
    names(ctx$pair_table),
    c("sender_node", "receiver_node", "n_interactions", "mean_imported_score",
      "n_with_score", "n_sender_cells", "n_receiver_cells", "centroid_distance",
      "mean_recv_to_sender_nn", "median_recv_to_sender_nn",
      "mean_send_to_recv_nn", "median_send_to_recv_nn",
      "frac_receiver_within_radius", "frac_sender_within_radius",
      "z_score", "p_perm")
  )
  expect_identical(ctx$provenance$analysis_id, "sc-communication-spatial")
  expect_identical(ctx$provenance$analysis_type, "cell_cell_communication_spatial")
  expect_identical(ctx$provenance$import_only, FALSE)
  expect_identical(ctx$provenance$parent_analysis_id, "sc-communication-import")
})

test_that("colonnes de pair_table sans permutation : z/p restent NA", {
  ctx <- .ccx_spc_freeze_context(radius = 3)
  expect_true(all(is.na(ctx$pair_table$z_score)))
  expect_true(all(is.na(ctx$pair_table$p_perm)))
  expect_identical(ctx$params$n_permutations, 0L)
  expect_identical(ctx$params$perm_seed, 1L)
})

test_that("déterminisme total du contexte spatial", {
  a <- .ccx_spc_freeze_context(radius = 3, n_perm = 7L)
  b <- .ccx_spc_freeze_context(radius = 3, n_perm = 7L)
  expect_identical(a$pair_table, b$pair_table)
  expect_identical(a$qc, b$qc)
})

test_that("le résultat canonique parent n'est jamais modifié", {
  r <- .ccx_result()
  before <- r$canonical_table
  .ccx_spc_freeze_context()
  expect_identical(r$canonical_table, before)
})

test_that("erreurs classées communication_context_error avec état structuré", {
  expect_error(
    build_communication_spatial_context(.ccx_result(), .ccx_coords(),
                                        unname(.ccx_identities())),
    class = "communication_context_error"
  )
  e <- tryCatch(
    build_communication_spatial_context(
      .ccx_result(), .ccx_coords(),
      setNames(rep("X", 12), .ccx_cells)  # aucune paire cartographiable
    ),
    error = function(err) err
  )
  expect_s3_class(e, "communication_context_error")
  expect_identical(e$state, "invalid_identity_mapping")
  expect_true(grepl("aucune paire", conditionMessage(e), fixed = TRUE))
})

test_that("schéma du module spatial figé (2 fonctions top-level)", {
  nms <- .ccx_top_level_assignments("modules/sc/mod_sc_communication_spatial.R")
  expect_setequal(
    nms,
    c("mod_sc_communication_spatial_ui", "mod_sc_communication_spatial_server")
  )
})

test_that("aucun appel de calcul d'inférence dans le moteur spatial", {
  src <- paste(readLines(file.path(ts_project_root(), "tools/pkg_skeleton/R/sc_communication_spatial.R")),
               collapse = "\n")
  expect_false(grepl("computeCommunProb|CellChatDB|nichenetr|cellphonedb\\(", src))
})

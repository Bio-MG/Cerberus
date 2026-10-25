# =============================================================================
# test-communication-trajectory-contract-freeze.R — V1.x-B : gel du contrat
# contexte trajectoire (roadmap CCC avancée Phase 3)
# =============================================================================
# REFUSE toute évolution incompatible : surface publique, champs du résultat,
# colonnes de pair_bin_table/pair_summary, états d'erreur, traçabilité parent,
# schéma du module. Toute modification passe simultanément par le code, ce
# test et docs/contracts/COMMUNICATION_TRAJECTORY_CONTRACT.md.
# =============================================================================

source_project_file("tools/pkg_skeleton/R/sc_communication_trajectory.R")

test_that("surface publique trajectoire figée", {
  expect_setequal(
    communication_trajectory_public_api(),
    c("communication_trajectory_public_api",
      "communication_trajectory_contract_fields",
      "communication_fetch_expression_matrix",
      "build_communication_trajectory_context",
      "plot_communication_trajectory_curves",
      "plot_communication_trajectory_heatmap",
      "build_communication_trajectory_export")
  )
  nms <- .ccx_top_level_assignments("tools/pkg_skeleton/R/sc_communication_trajectory.R")
  nms_public <- nms[!grepl("^\\.", nms)]
  expect_true(all(nms_public %in% communication_trajectory_public_api()))
})

test_that("champs contractuels du contexte trajectoire figés", {
  expect_setequal(
    communication_trajectory_contract_fields(),
    c("type", "status", "analysis_id", "parent_analysis_id", "source_method",
      "identity_column", "params", "pair_bin_table", "pair_summary", "qc",
      "warnings", "provenance", "timestamp_utc")
  )
})

.ccx_trj_freeze_context <- function(lineage = NULL, n_bins = 5L) {
  build_communication_trajectory_context(
    .ccx_result(), .ccx_expr(), .ccx_identities(), .ccx_pseudotime(),
    lineage = lineage, identity_column = "cell_type", n_bins = n_bins
  )
}

test_that("schéma du résultat de contexte trajectoire figé", {
  ctx <- .ccx_trj_freeze_context()
  expect_identical(ctx$type, "ccc_trajectory_context")
  expect_identical(ctx$status, "valid")
  expect_identical(ctx$analysis_id, "sc-communication-trajectory")
  expect_identical(ctx$parent_analysis_id, "sc-communication-import")
  expect_true(all(communication_trajectory_contract_fields() %in% names(ctx)))
  expect_setequal(
    names(ctx$pair_bin_table),
    c("sender_node", "receiver_node", "lineage", "bin", "bin_from", "bin_to",
      "bin_mid", "n_cells_in_bin", "n_sender_cells", "n_receiver_cells",
      "frac_sender_of_bin", "frac_receiver_of_bin",
      "frac_sender_of_population", "frac_receiver_of_population",
      "mean_ligand_expression_senders", "mean_receptor_expression_receivers")
  )
  expect_setequal(
    names(ctx$pair_summary),
    c("sender_node", "receiver_node", "lineage", "n_sender_cells",
      "n_receiver_cells", "ligand_genes", "receptor_genes",
      "n_ligand_genes_missing", "n_receptor_genes_missing",
      "mean_imported_score")
  )
  expect_identical(ctx$provenance$analysis_id, "sc-communication-trajectory")
  expect_identical(ctx$provenance$analysis_type, "cell_cell_communication_trajectory")
  expect_identical(ctx$provenance$import_only, FALSE)
  expect_identical(ctx$provenance$parent_analysis_id, "sc-communication-import")
  expect_identical(ctx$params$lineage_mode, "global")
})

test_that("mode par lignée : jamais de collapse, qc en combinaisons", {
  ctx <- .ccx_trj_freeze_context(lineage = .ccx_lineage())
  expect_identical(ctx$params$lineage_mode, "per_lineage")
  expect_setequal(unique(ctx$pair_bin_table$lineage), c("L1", "L2"))
  expect_identical(ctx$qc$n_combinations_total, 10L)  # 5 paires x 2 lignées
  expect_identical(ctx$qc$n_combinations_mapped, 4L)  # L1: A->C, C->C ; L2: C->B, C->C
  expect_identical(ctx$qc$n_combinations_unmapped, 6L)
  expect_false(any(ctx$pair_bin_table$sender_node == "A" &
                     ctx$pair_bin_table$lineage == "L2"))
})

test_that("déterminisme total du contexte trajectoire", {
  a <- .ccx_trj_freeze_context(n_bins = 7L)
  b <- .ccx_trj_freeze_context(n_bins = 7L)
  expect_identical(a$pair_bin_table, b$pair_bin_table)
  expect_identical(a$qc, b$qc)
})

test_that("le résultat canonique parent n'est jamais modifié", {
  r <- .ccx_result()
  before <- r$canonical_table
  .ccx_trj_freeze_context()
  expect_identical(r$canonical_table, before)
})

test_that("erreurs classées communication_context_error avec état structuré", {
  expect_error(
    .ccx_trj_freeze_context(n_bins = 1L),
    class = "communication_context_error"
  )
  e <- tryCatch(
    build_communication_trajectory_context(
      .ccx_result(), .ccx_expr(), .ccx_identities(),
      setNames(rep(NA_real_, 12), .ccx_cells)  # aucun pseudo-temps fini
    ),
    error = function(err) err
  )
  expect_s3_class(e, "communication_context_error")
  expect_identical(e$state, "invalid_identity_mapping")
})

test_that("schéma du module trajectoire figé (2 fonctions top-level)", {
  nms <- .ccx_top_level_assignments("modules/sc/mod_sc_communication_trajectory.R")
  expect_setequal(
    nms,
    c("mod_sc_communication_trajectory_ui", "mod_sc_communication_trajectory_server")
  )
})

test_that("aucun appel de calcul d'inférence dans le moteur trajectoire", {
  src <- paste(readLines(file.path(ts_project_root(), "tools/pkg_skeleton/R/sc_communication_trajectory.R")),
               collapse = "\n")
  expect_false(grepl("computeCommunProb|CellChatDB|nichenetr|monocle|slingshot\\(", src))
})

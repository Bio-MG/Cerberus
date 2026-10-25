# =============================================================================
# test-communication-perturbation-contract-freeze.R — V1.x-D : gel du contrat
# perturbation IN SILICO (roadmap CCC avancée Phase 4)
# =============================================================================
# REFUSE toute évolution incompatible : surface publique, champs du résultat,
# colonnes delta/node, GARDE « IN SILICO PERTURBATION » (étiquette présente
# dans le résultat, les figures et l'export), traçabilité parent, schéma du
# module. Toute modification passe simultanément par le code, ce test et
# docs/contracts/COMMUNICATION_PERTURBATION_CONTRACT.md.
# =============================================================================

source_project_file("tools/pkg_skeleton/R/sc_communication_perturbation.R")

test_that("surface publique perturbation figée", {
  expect_setequal(
    communication_perturbation_public_api(),
    c("communication_perturbation_public_api",
      "communication_perturbation_targets",
      "communication_perturbation_contract_fields",
      "build_communication_perturbation",
      "plot_communication_perturbation_delta",
      "plot_communication_perturbation_nodes",
      "build_communication_perturbation_export")
  )
  expect_setequal(
    communication_perturbation_targets(),
    c("ligand", "receptor", "interaction", "sender", "receiver")
  )
  nms <- .ccx_top_level_assignments("tools/pkg_skeleton/R/sc_communication_perturbation.R")
  nms_public <- nms[!grepl("^\\.", nms)]
  expect_true(all(nms_public %in% communication_perturbation_public_api()))
})

test_that("champs contractuels de la perturbation figés", {
  expect_setequal(
    communication_perturbation_contract_fields(),
    c("type", "status", "analysis_id", "parent_analysis_id", "source_method",
      "target", "value", "mode", "params", "baseline_summary",
      "perturbed_summary", "delta_table", "node_table", "qc", "warnings",
      "provenance", "timestamp_utc")
  )
})

.ccx_pert_freeze <- function(...) {
  build_communication_perturbation(.ccx_result(), ...)
}

test_that("schéma du résultat de perturbation figé + garde in silico", {
  ctx <- .ccx_pert_freeze(target = "ligand", value = "L1", mode = "remove")
  expect_identical(ctx$type, "ccc_perturbation")
  expect_identical(ctx$status, "valid")
  expect_identical(ctx$analysis_id, "sc-communication-perturbation")
  expect_identical(ctx$parent_analysis_id, "sc-communication-import")
  expect_identical(ctx$params$label, "IN SILICO PERTURBATION")
  expect_identical(ctx$params$order, "first_order_no_propagation")
  expect_true(all(communication_perturbation_contract_fields() %in% names(ctx)))
  expect_setequal(
    names(ctx$delta_table),
    c("sender_node", "receiver_node", "n_interactions_baseline",
      "n_interactions_perturbed", "score_total_baseline",
      "score_total_perturbed", "delta_score_total", "delta_fraction",
      "affected")
  )
  expect_true(any(grepl("IN SILICO", ctx$warnings)))
  expect_true(any(grepl("PREMIER ORDRE", ctx$warnings)))
  expect_identical(ctx$provenance$analysis_type, "cell_cell_communication_perturbation")
  expect_identical(ctx$provenance$import_only, FALSE)
  expect_identical(ctx$provenance$parent_analysis_id, "sc-communication-import")
  expect_identical(ctx$provenance$parameters$label, "in_silico_not_ko")
})

test_that("déterminisme + parent jamais modifié", {
  r <- .ccx_result()
  before <- r$canonical_table
  a <- .ccx_pert_freeze(target = "receptor", value = "R1",
                        mode = "attenuate", factor = 0.5)
  b <- .ccx_pert_freeze(target = "receptor", value = "R1",
                        mode = "attenuate", factor = 0.5)
  expect_identical(a$delta_table, b$delta_table)
  expect_identical(a$node_table, b$node_table)
  expect_identical(r$canonical_table, before)
})

test_that("erreurs classées communication_context_error", {
  expect_error(.ccx_pert_freeze(target = "pathway", value = "X"),
               class = "communication_context_error")
  expect_error(.ccx_pert_freeze(target = "ligand", value = "ZZZ"),
               "absente", class = "communication_context_error")
  expect_error(.ccx_pert_freeze(target = "ligand", value = "L1",
                                mode = "attenuate", factor = 1.5),
               class = "communication_context_error")
})

test_that("les figures et l'export portent le garde in silico", {
  ctx <- .ccx_pert_freeze(target = "ligand", value = "L1")
  g1 <- plot_communication_perturbation_delta(ctx)
  g2 <- plot_communication_perturbation_nodes(ctx)
  expect_true(grepl("IN SILICO", g1$labels$subtitle %||% ""))
  expect_true(grepl("IN SILICO", g2$labels$subtitle %||% ""))
  ex <- build_communication_perturbation_export(ctx)
  expect_true(all(ex$label == "IN SILICO PERTURBATION"))
})

test_that("schéma du module perturbation figé (2 fonctions top-level)", {
  nms <- .ccx_top_level_assignments("modules/sc/mod_sc_communication_perturbation.R")
  expect_setequal(
    nms,
    c("mod_sc_communication_perturbation_ui", "mod_sc_communication_perturbation_server")
  )
})

test_that("aucun calcul d'inférence ni propagation modélisée", {
  src <- paste(readLines(file.path(ts_project_root(), "tools/pkg_skeleton/R/sc_communication_perturbation.R")),
               collapse = "\n")
  expect_false(grepl("computeCommunProb|CellChatDB|nichenetr|propagate|randomWalk|diffusion", src))
})

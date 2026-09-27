# =============================================================================
# test-sc-results-purge.R — invalidation des résultats au changement de jeu SC
# =============================================================================
# Audit 2026-09-27 §1.5 : shared_rv n'était jamais purgé quand
# global_data$sc_obj était remplacé — le rapport (mod_sc.R:1330-1365) pouvait
# mélanger le jeu B avec markers/pathways/velocity/da_*/rarity du jeu A.
# La purge est déclenchée par global_data$sc_obj_epoch (incrémentée UNIQUEMENT
# par les remplacements de dataset — les re-commits de même lignée du pipeline
# ne purgent pas, cf. R/core/state.R).
# =============================================================================
source_project_file("R/core/state.R")

test_that("sc_shared_result_fields couvre tout slot résultat écrit par un module", {
  expect_setequal(
    sc_shared_result_fields(),
    c("markers_data", "correlated_genes", "corr_target_gene",
      "pathway_results", "pathway_db", "qc_snapshot",
      "traj_reduction", "traj_method",
      "velocity_result", "da_design_result", "da_milo_result",
      "da_sccoda_result", "population_rarity_result")
  )
})

test_that("create_sc_shared_state déclare chaque champ résultat (plus de champs hors schéma)", {
  s <- create_sc_shared_state()
  nms <- shiny::isolate(names(s))
  missing <- setdiff(sc_shared_result_fields(), nms)
  expect_length(missing, 0L)
})

test_that("sc_purge_shared_results met à NULL chaque champ résultat — et uniquement eux", {
  s <- create_sc_shared_state()
  s$markers_data          <- data.frame(gene = "X", p_val_adj = 0.01)
  s$pathway_results       <- data.frame(ID = "GO:1")
  s$population_rarity_result <- list(status = "ok")
  s$velocity_result       <- list()
  s$da_milo_result        <- list()
  s$active_tab            <- "tab_viz"          # état d'UI : ne doit PAS être purgé
  s$selected_genes        <- c("CD4", "CD8A")   # préférence viz : ne doit PAS être purgé
  s$report_viz_list       <- list(a = 1)        # panier utilisateur : ne doit PAS être purgé

  sc_purge_shared_results(s)

  for (f in sc_shared_result_fields()) {
    expect_null(state_get(s, f), info = f)
  }
  expect_identical(state_get(s, "active_tab"), "tab_viz")
  expect_identical(state_get(s, "selected_genes"), c("CD4", "CD8A"))
  expect_identical(state_get(s, "report_viz_list"), list(a = 1))
})

test_that("sc_purge_shared_results est idempotent et invisible", {
  s <- create_sc_shared_state()
  expect_invisible(sc_purge_shared_results(s))
  expect_invisible(sc_purge_shared_results(s))
  for (f in sc_shared_result_fields()) expect_null(state_get(s, f))
})

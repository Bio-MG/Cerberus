# =============================================================================
# R/sc/sc_state.R — SC shared reactive state factory (LEGACY RE-EXPORT)
# =============================================================================
# CHRYSALIS PHASE — canonical definition moved to R/core/state.R.
# This file is retained ONLY for backward compatibility: tests or external
# code that source("R/sc/sc_state.R") directly must still get create_sc_shared_state().
# app.R no longer sources this file as canonical; it sources R/core/state.R first.
# If R/core/state.R was already sourced (normal app startup), this file is a no-op.
# =============================================================================

# Ensure canonical definitions are loaded if this file is sourced standalone (e.g. tests)
if (!exists("create_sc_shared_state", envir = .GlobalEnv) ||
    !exists("create_spatial_shared_state", envir = .GlobalEnv)) {
  core_state_path <- file.path(dirname(dirname(normalizePath("R/sc/sc_state.R", mustWork = FALSE))), "R", "core", "state.R")
  # Fallback: try relative path from project root when normalizePath above fails in some envs
  if (!file.exists(core_state_path)) {
    for (cand in c("R/core/state.R", file.path(getwd(), "R/core/state.R"))) {
      if (file.exists(cand)) { core_state_path <- cand; break }
    }
  }
  if (file.exists(core_state_path)) source(core_state_path, local = FALSE)
}

# If canonical file was not found (should never happen in app), define inline fallback
if (!exists("create_sc_shared_state", envir = .GlobalEnv)) {
  create_sc_shared_state <- function() {
    shiny::reactiveValues(
      markers_data = NULL, correlated_genes = NULL, corr_target_gene = NULL,
      pathway_results = NULL, pathway_db = NULL, selected_genes = character(0),
      active_tab = NULL, report_viz_list = list(), traj_reduction = NULL,
      traj_method = NULL, traj_genes = character(0), max_cells_heavy = Inf,
      sc_palette = "default", sc_manual_colors = NULL,
      sc_manual_gradient = NULL, sc_manual_volcano_colors = NULL,
      communication_result = NULL, communication_collection = NULL,
      active_communication_sample = NULL
    )
  }
}

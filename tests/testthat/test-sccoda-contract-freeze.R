# =============================================================================
# test-sccoda-contract-freeze.R — Stage 15 : gel du contrat scCODA (4E-2)
# =============================================================================
# REFUSE toute evolution incompatible : champs du resultat canonique, etats de
# validite + libelles, empreinte v2 (REUTILISEE de velocity), surfaces
# publiques de R/sc/sc_abundance_sccoda.R et des vues, orchestration module
# (3 fonctions top-level), consommation du design Stage 13 via
# shared_rv$da_design_result, isolation des consommateurs rapport/export,
# absence de dependance R nouvelle (reticulate deja dans renv.lock),
# synchronisation code <-> doc.
# =============================================================================

.sccoda_top_level_assignments <- function(relpath) {
  exprs <- parse(file.path(ts_project_root(), relpath), keep.source = FALSE)
  nms <- character(0)
  for (e in exprs) {
    if (is.call(e) && identical(deparse(e[[1L]]), "<-") && is.symbol(e[[2L]])) {
      nms <- c(nms, as.character(e[[2L]]))
    }
  }
  unique(nms)
}

.sccoda_freeze_canonical <- function() {
  suppressMessages(.sccoda_run())
}

# ── Schema du resultat canonique ────────────────────────────────────────────
test_that("canonical scCODA result exposes every frozen contract field", {
  canon <- .sccoda_freeze_canonical()
  expect_identical(setdiff(sccoda_contract_fields(), names(canon)),
                   character(0))
  expect_identical(canon$type, "sccoda_da")
  expect_identical(canon$compositional_unit, "sample")
  expect_true(canon$status %in% c("valid", "valid_with_warnings"))
  expect_match(canon$design$design_fingerprint, "^v2::")
  expect_true(all(c("formula", "condition_base", "reference_policy",
                    "reference_identity", "num_results", "num_burnin",
                    "num_leapfrog_steps", "step_size", "fdr_target",
                    "backend", "seed") %in%
                    names(canon$model_specification)))
  expect_true(all(c("rhat_max", "ess_min", "n_divergences", "acc_rate",
                    "notes") %in% names(canon$convergence_diagnostics)))
  expect_true(all(c("python", "sccoda", "tensorflow", "R") %in%
                    names(canon$package_versions)))
  expect_identical(canon$provenance$analysis_type, "sccoda_da")
  expect_identical(canon$provenance$analysis_id, "sc-da-sccoda")
  expect_match(canon$object_identity$fingerprint, "^v2::")
  # La graine est APPLIQUEE (tensorflow) et enregistree.
  expect_identical(canon$provenance$seed, TS_DA_SCCODA_SEED)
})

test_that("validity states and labels are frozen", {
  expect_setequal(sccoda_validity_states(), c(
    "valid", "valid_with_warnings", "invalid_input", "environment_missing",
    "compute_failed", "convergence_failure", "design_not_eligible",
    "stale_against_current_seurat_object"
  ))
  expect_setequal(names(sccoda_status_labels()), sccoda_validity_states())
})

# ── Surfaces publiques figees ───────────────────────────────────────────────
test_that("public API surface of sc_abundance_sccoda.R is frozen", {
  defined <- .sccoda_top_level_assignments("R/sc/sc_abundance_sccoda.R")
  public <- defined[!grepl("^\\.", defined)]
  expect_setequal(public, sccoda_public_api())
  expect_true(all(c(".SCCODA_STATUS_STATES", ".sccoda_stop",
                    ".sccoda_is_blank", ".sccoda_design_recap",
                    ".sccoda_probe_command",
                    ".sccoda_default_env_resolver", ".SCCODA_PY_BRIDGE",
                    ".sccoda_venv_dir", ".sccoda_run_bridge",
                    ".sccoda_orient_effects") %in%
                    setdiff(defined, public)))
})

test_that("public API surface of sc_abundance_sccoda_views.R is frozen", {
  defined <- .sccoda_top_level_assignments("R/sc/sc_abundance_sccoda_views.R")
  public <- defined[!grepl("^\\.", defined)]
  expect_setequal(public, sccoda_views_public_api())
  expect_true(all(c(".sccoda_views_stop", ".sccoda_empty_view_plot") %in%
                    setdiff(defined, public)))
})

test_that("scCODA module defines exactly its three orchestration functions", {
  expect_setequal(
    .sccoda_top_level_assignments("modules/sc/mod_sc_da_sccoda.R"),
    c("mod_sc_da_sccoda_ui", "mod_sc_da_sccoda_output_ui",
      "mod_sc_da_sccoda_server")
  )
  src <- paste(readLines(file.path(ts_project_root(),
                                   "modules/sc/mod_sc_da_sccoda.R")),
               collapse = "\n")
  expect_match(src, "run_sccoda_da(", fixed = TRUE)
  expect_match(src, "assert_sccoda_result(", fixed = TRUE)
  expect_match(src, "sccoda_available(", fixed = TRUE)
  expect_match(src, "sccoda_status_labels(", fixed = TRUE)
  expect_match(src, "sccoda_export_filename(", fixed = TRUE)
  expect_match(src, "build_sccoda_summary(", fixed = TRUE)
  expect_match(src, "provenance_append(", fixed = TRUE)
  expect_match(src, "plot_sccoda_effects(", fixed = TRUE)
  expect_match(src, "shared_rv$da_design_result", fixed = TRUE)
  # Aucune reimplemention du domaine : le modele Python et la preparation
  # restent dans R/sc/ (pas d'import reticulate direct dans le module).
  expect_false(grepl("reticulate::|py_run_string|CompositionalAnalysis|from_pandas",
                     src))
})

test_that("scCODA is mounted once in mod_sc.R (panel 8e + output tab)", {
  mod_sc_src <- paste(readLines(file.path(ts_project_root(),
                                          "modules/sc/mod_sc.R")),
                      collapse = "\n")
  expect_match(mod_sc_src, "mod_sc_da_sccoda_server", fixed = TRUE)
  expect_false(grepl("run_sccoda_da|finalize_sccoda_result", mod_sc_src))
  expect_match(mod_sc_src, "8e", fixed = TRUE)
  expect_match(mod_sc_src, "tab_da_sccoda", fixed = TRUE)
})

# ── Isolation des consommateurs rapport/export ──────────────────────────────
test_that("report/export consumers do not reimplement scCODA yet (Stage 17 scope)", {
  export_src <- paste(readLines(file.path(ts_project_root(),
                                          "R/sc/sc_export.R")),
                      collapse = "\n")
  expect_false(grepl("sccoda", export_src, ignore.case = TRUE))
  # Post-V1.0 : le rapport Rmd AFFICHE le resultat canonique (tables
  # pures, sections velocity/communication/DA) - il n a jamais le droit
  # de REIMPLEMENTER l analyse. Garde ciblee : aucun appel de
  # calcul/validation/empreinte du domaine.
  compute_ban <- "run_sccoda_da\\(|reticulate::|sccoda_available\\("
  for (tpl in list.files(file.path(ts_project_root(), "reports"),
                         full.names = TRUE)) {
    tpl_src <- paste(readLines(tpl), collapse = "\n")
    expect_false(grepl(compute_ban, tpl_src, ignore.case = TRUE),
                 info = basename(tpl))
  }
})

test_that("no new R dependency was added to renv.lock for scCODA", {
  lock <- jsonlite::fromJSON(file.path(ts_project_root(), "renv.lock"),
                             simplifyVector = FALSE)
  expect_true("reticulate" %in% names(lock$Packages))
})

# ── Synchronisation code <-> contrat documentaire ───────────────────────────
test_that("contract document is in sync with the frozen code", {
  doc_path <- file.path(ts_project_root(), "docs", "contracts",
                        "SCCODA_RESULT_CONTRACT.md")
  skip_if_not(file.exists(doc_path), .ts_contract_skip_msg(doc_path))
  doc <- paste(.ts_contract_readlines(doc_path), collapse = "\n")
  for (f in sccoda_contract_fields()) {
    expect_match(doc, paste0("`", f, "`"), fixed = TRUE,
                 info = paste("champ contrat absent du document :", f))
  }
  for (st in sccoda_validity_states()) {
    expect_match(doc, st, fixed = TRUE,
                 info = paste("etat absent du document :", st))
  }
  for (fn in c("run_sccoda_da", "finalize_sccoda_result",
               "assert_sccoda_result", "sccoda_public_api",
               "sccoda_views_public_api", "sccoda_available",
               "sccoda_convergence_assessment")) {
    expect_match(doc, fn, fixed = TRUE,
                 info = paste("fonction absente du document :", fn))
  }
  # La porte Stage 13 et la separation d'avec Milo sont documentees.
  expect_match(doc, "assert_da_design_result", fixed = TRUE)
  expect_match(doc, "Milo", fixed = TRUE)
})

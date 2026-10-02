# =============================================================================
# test-milo-contract-freeze.R — Stage 14 : gel du contrat de resultat Milo (4E-1)
# =============================================================================
# REFUSE toute evolution incompatible : champs du resultat canonique, etats de
# validite + libelles, empreinte v2 (REUTILISEE de velocity), surfaces
# publiques de R/sc/sc_abundance_milo.R et R/sc/sc_abundance_milo_views.R,
# orchestration module (3 fonctions top-level), portage du design Stage 13 via
# shared_rv$da_design_result, isolation des consommateurs rapport/export,
# dependance miloR declaree dans renv.lock, synchronisation code <-> doc.
# Extraction des affectations top-level reecrite localement (pas de
# duplication ligne a ligne des autres tests de freeze).
# =============================================================================

.milo_top_level_assignments <- function(relpath) {
  exprs <- parse(file.path(ts_project_root(), relpath), keep.source = FALSE)
  nms <- character(0)
  for (e in exprs) {
    if (is.call(e) && identical(deparse(e[[1L]]), "<-") && is.symbol(e[[2L]])) {
      nms <- c(nms, as.character(e[[2L]]))
    }
  }
  unique(nms)
}

.milo_freeze_canonical_full <- function() {
  suppressMessages(.milo_run())
}

.milo_freeze_canonical_warned <- function() {
  suppressMessages(.milo_run(
    obj = .milo_seurat_obj(meta = .da_meta(samples = c(s1 = 8, s2 = 50, s3 = 50, s4 = 50)))
  ))
}

# ── Schema du resultat canonique ────────────────────────────────────────────
test_that("canonical Milo result exposes every frozen contract field", {
  for (canon in list(.milo_freeze_canonical_full(), .milo_freeze_canonical_warned())) {
    expect_identical(setdiff(milo_contract_fields(), names(canon)),
                     character(0))
  }
})

test_that("frozen per-field shapes on the canonical Milo result", {
  canon <- .milo_freeze_canonical_full()
  expect_identical(canon$type, "milo_da")
  expect_true(canon$status %in% c("valid", "valid_with_warnings"))
  expect_identical(canon$design$design_analysis_id, "sc-da-design")
  expect_match(canon$design$design_fingerprint, "^v2::")
  expect_identical(canon$parameters$composition_unit, "sample")
  expect_true(isTRUE(canon$provenance$parameters$composition_unit == "sample"))
  expect_true(is.data.frame(canon$neighbourhood_summary))
  expect_identical(nrow(canon$neighbourhood_summary), 1L)
  expect_identical(
    colnames(canon$DA_table),
    c("Nhood", "n_cells", "logFC", "logCPM", "F", "PValue", "FDR",
      "SpatialFDR", "identity", "identity_fraction")
  )
  expect_identical(names(canon$tested_contrast),
                   c("formula", "contrast", "target", "reference", "interpretation"))
  expect_true(all(c("design_cols", "robust", "min.mean", "fdr_weighting",
                    "n_design_samples") %in%
                    names(canon$model_specification)))
  expect_true(all(c("miloR", "edgeR", "BiocNeighbors", "SeuratObject", "R") %in%
                    names(canon$package_versions)))
  expect_identical(canon$provenance$analysis_type, "milo_da")
  expect_identical(canon$provenance$analysis_id, "sc-da-milo")
  expect_match(canon$object_identity$fingerprint, "^v2::")
  expect_match(canon$timestamp_utc, "^\\d{4}-\\d{2}-\\d{2}T")
  # La graine est APPLIQUEE (makeNhoods est aleatoire) et enregistree.
  expect_identical(canon$provenance$seed, TS_DA_MILO_SEED)
  expect_identical(canon$parameters$seed, TS_DA_MILO_SEED)
})

test_that("validity states and labels are frozen", {
  expect_setequal(milo_validity_states(), c(
    "valid", "valid_with_warnings", "invalid_input", "compute_failed",
    "design_not_eligible", "stale_against_current_seurat_object"
  ))
  expect_setequal(names(milo_status_labels()), milo_validity_states())
})

test_that("object identity is pinned to the v2 fingerprint reused from velocity", {
  canon <- .milo_freeze_canonical_full()
  expect_match(canon$object_identity$fingerprint, "^v2::")
  expect_match(canon$object_identity$method, "velocity_object_fingerprint",
               fixed = TRUE)
})

# ── Surfaces publiques figees ───────────────────────────────────────────────
test_that("public API surface of sc_abundance_milo.R is frozen", {
  defined <- .milo_top_level_assignments("R/sc/sc_abundance_milo.R")
  public <- defined[!grepl("^\\.", defined)]
  expect_setequal(public, milo_public_api())
  expect_true(all(c(".MILO_STATUS_STATES", ".milo_stop", ".milo_is_blank",
                    ".milo_design_recap", ".milo_build_sce",
                    ".milo_cell_display_scores") %in%
                    setdiff(defined, public)))
})

test_that("public API surface of sc_abundance_milo_views.R is frozen", {
  defined <- .milo_top_level_assignments("R/sc/sc_abundance_milo_views.R")
  public <- defined[!grepl("^\\.", defined)]
  expect_setequal(public, milo_views_public_api())
  expect_true(all(c(".milo_views_stop", ".milo_empty_view_plot",
                    ".milo_view_subtitle") %in% setdiff(defined, public)))
})

test_that("milo module defines exactly its three orchestration functions", {
  expect_setequal(
    .milo_top_level_assignments("modules/sc/mod_sc_da_milo.R"),
    c("mod_sc_da_milo_ui", "mod_sc_da_milo_output_ui", "mod_sc_da_milo_server")
  )
  # Le module consomme les accesseurs documentes — aucune reimplemention du
  # calcul (miloR et le modele restent dans le domaine) ni du design.
  src <- paste(readLines(file.path(ts_project_root(),
                                   "modules/sc/mod_sc_da_milo.R")),
               collapse = "\n")
  expect_match(src, "run_milo_da(", fixed = TRUE)
  expect_match(src, "assert_milo_result(", fixed = TRUE)
  expect_match(src, "milo_status_labels(", fixed = TRUE)
  expect_match(src, "milo_export_filename(", fixed = TRUE)
  expect_match(src, "build_milo_summary(", fixed = TRUE)
  expect_match(src, "build_milo_da_table_export(", fixed = TRUE)
  expect_match(src, "provenance_append(", fixed = TRUE)
  expect_match(src, "plot_milo_da_embedding(", fixed = TRUE)
  expect_match(src, "shared_rv$da_design_result", fixed = TRUE)
  expect_false(grepl("miloR::|testNhoods\\(|model\\.matrix|validate_da_design\\(",
                     src))
})

test_that("the Stage 13 design result is exposed to consumers by panel 8c", {
  src <- paste(readLines(file.path(ts_project_root(),
                                   "modules/sc/mod_sc_da_design.R")),
               collapse = "\n")
  expect_match(src, "shared_rv$da_design_result <- canonical", fixed = TRUE)
})

test_that("milo is mounted once in mod_sc.R (panel 8d + output tab)", {
  mod_sc_src <- paste(readLines(file.path(ts_project_root(),
                                          "modules/sc/mod_sc.R")),
                      collapse = "\n")
  expect_match(mod_sc_src, "mod_sc_da_milo_server", fixed = TRUE)
  expect_false(grepl("run_milo_da|finalize_milo_result", mod_sc_src))
  expect_match(mod_sc_src, "8d", fixed = TRUE)
  expect_match(mod_sc_src, "tab_da_milo", fixed = TRUE)
})

# ── Isolation des consommateurs rapport/export ──────────────────────────────
test_that("report/export consumers do not reimplement Milo yet (Stage 17 scope)", {
  export_src <- paste(readLines(file.path(ts_project_root(),
                                          "R/sc/sc_export.R")),
                      collapse = "\n")
  expect_false(grepl("milo", export_src, ignore.case = TRUE))
  # Post-V1.0 : le rapport Rmd AFFICHE le resultat canonique (tables
  # pures, sections velocity/communication/DA) - il n a jamais le droit
  # de REIMPLEMENTER l analyse. Garde ciblee : aucun appel de
  # calcul/validation/empreinte du domaine.
  compute_ban <- "run_milo_da\\(|testNhoods\\(|makeNhoods\\(|buildGraph\\("
  for (tpl in list.files(file.path(ts_project_root(), "reports"),
                         full.names = TRUE)) {
    tpl_src <- paste(readLines(tpl), collapse = "\n")
    expect_false(grepl(compute_ban, tpl_src, ignore.case = TRUE),
                 info = basename(tpl))
  }
})

test_that("miloR dependency is declared in renv.lock", {
  lock <- jsonlite::fromJSON(file.path(ts_project_root(), "renv.lock"),
                             simplifyVector = FALSE)
  expect_true("miloR" %in% names(lock$Packages))
  expect_identical(lock$Packages$miloR$Source, "Bioconductor")
})

# ── Synchronisation code <-> contrat documentaire ───────────────────────────
test_that("contract document is in sync with the frozen code", {
  doc_path <- file.path(ts_project_root(), "docs", "contracts",
                        "MILO_RESULT_CONTRACT.md")
  skip_if_not(file.exists(doc_path), .ts_contract_skip_msg(doc_path))
  doc <- paste(.ts_contract_readlines(doc_path), collapse = "\n")
  for (f in milo_contract_fields()) {
    expect_match(doc, paste0("`", f, "`"), fixed = TRUE,
                 info = paste("champ contrat absent du document :", f))
  }
  for (st in milo_validity_states()) {
    expect_match(doc, st, fixed = TRUE,
                 info = paste("etat absent du document :", st))
  }
  for (fn in c("run_milo_da", "finalize_milo_result", "assert_milo_result",
               "milo_public_api", "milo_views_public_api")) {
    expect_match(doc, fn, fixed = TRUE,
                 info = paste("fonction absente du document :", fn))
  }
  # La porte Stage 13 est documentee.
  expect_match(doc, "assert_da_design_result", fixed = TRUE)
})

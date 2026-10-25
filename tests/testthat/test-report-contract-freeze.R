# =============================================================================
# test-report-contract-freeze.R — FREEZE du contrat rapport consolidé (Stage 17)
# =============================================================================
# Contrat : docs/contracts/CONSOLIDATED_REPORT_CONTRACT.md (figé).
# Toute évolution de la surface doit passer SIMULTANÉMENT par : le code
# (tools/pkg_skeleton/R/report_*.R), ce test et le document de contrat.
#
# D2 M5 LOT 2 (2026-10-25) : le domaine R/reports ENTIER vit dans le paquet
# (tools/pkg_skeleton/R/ — cartographie docs/audits/d2_reports_cartography.md
# §1). Ce fichier de gel bascule sur load_project_pkg() : ses vérifications
# sont de SURFACE (symboles figés + littéraux de chemin du code source), aucun
# appel profond au collecteur — les fournisseurs R/sc ne sont PAS requis.
# Les 2 autres fichiers stricts du domaine (test-release-hardening.R,
# test-sc-report-contract.R) GARDENT source_project_file() au NOUVEAU chemin :
# ils appellent collect_consolidated_report_input() en profondeur, et un
# namespace ne résout PAS via globalenv (leçon M4) — tant que les 9
# fournisseurs R/sc ne sont pas dans le paquet, leur exécution globalenv est
# l'état conçu (double chemin de chargement, cf. test-plotting-pkg-pilot.R).
# =============================================================================

CONTRACT_REPORT <- file.path("docs", "contracts", "CONSOLIDATED_REPORT_CONTRACT.md")

.rep_pkg_dir <- file.path("tools", "pkg_skeleton", "R")

# Le contrat est fige : on verifie la surface REELLEMENT chargee (pas
# seulement le texte source) — depuis le NAMESPACE du paquet (M5 lot 2).
load_project_pkg()

.rep_top_level_assignments <- function(relpath) {
  src <- readLines(file.path(ts_project_root(), relpath), warn = FALSE)
  hits <- grep("^[A-Za-z.][A-Za-z0-9._]*\\s*<-", src, value = TRUE)
  unique(sub("^([A-Za-z.][A-Za-z0-9._]*)\\s*<-.*$", "\\1", hits))
}

.rep_report_src <- function(relpath) {
  if (grepl("^docs/contracts/", relpath) &&
      !file.exists(file.path(ts_project_root(), relpath))) {
    skip(.ts_contract_skip_msg(relpath))
  }
  paste(readLines(file.path(ts_project_root(), relpath), warn = FALSE), collapse = "\n")
}

# ── Surface publique figée ───────────────────────────────────────────────────
test_that("report public API is defined, frozen and all functions exist", {
  api <- report_public_api()
  expect_match(api$contract, "CONSOLIDATED_REPORT_CONTRACT.md", fixed = TRUE)
  expect_setequal(api$functions, c(
    "report_public_api", "collect_consolidated_report_input",
    "consolidated_report_analyses", "consolidated_report_input_recap",
    "validate_consolidated_report_input",
    "build_consolidated_report_html", "write_consolidated_report_html",
    "build_report_bundle", "consolidated_report_export_filename",
    "consolidated_report_validation_states"
  ))
  # M5 lot 2 : la surface vit dans le NAMESPACE du paquet (forme du garde
  # pilote M4 — exists(..., globalenv) ne tient pas pour un paquet attaché).
  ns <- asNamespace("TranscriptoShiny")
  for (fn in api$functions) {
    expect_true(exists(fn, envir = ns, mode = "function", inherits = FALSE),
                info = paste("fonction publique absente du namespace :", fn))
  }
})

test_that("the four report files define exactly their frozen symbols", {
  expect_setequal(
    .rep_top_level_assignments(file.path(.rep_pkg_dir, "report_collector.R")),
    c(".report_analysis_domains", ".report_contract_domains",
      ".report_legacy_domains", ".report_global_domains",
      ".report_config_keys", ".rep_stop",
      ".report_kv_df", ".report_domain_summary",
      "collect_consolidated_report_input", "consolidated_report_analyses",
      "consolidated_report_input_recap", "report_public_api")
  )
  expect_setequal(
    .rep_top_level_assignments(file.path(.rep_pkg_dir, "report_validator.R")),
    c("consolidated_report_validation_states", ".report_val_stop",
      ".report_state_labels", "validate_consolidated_report_input")
  )
  expect_setequal(
    .rep_top_level_assignments(file.path(.rep_pkg_dir, "report_render.R")),
    c(".report_render_stop", ".report_domain_labels", ".report_banner_colors",
      ".report_css", ".ts_report_const", ".report_html_table", ".report_banner",
      "build_consolidated_report_html", "write_consolidated_report_html")
  )
  expect_setequal(
    .rep_top_level_assignments(file.path(.rep_pkg_dir, "report_bundle.R")),
    c(".report_bundle_stop", "consolidated_report_export_filename",
      ".report_bundle_tables", ".report_bundle_readme", "build_report_bundle")
  )
})

test_that("the frozen domain list and validation states are stable", {
  expect_setequal(consolidated_report_analyses(), c(
    "markers", "pseudobulk", "correlation", "pathways", "trajectory",
    "velocity", "communication",
    "da_design", "da_milo", "da_sccoda", "da_cross",
    "bulk_multi_comparison"))
  expect_setequal(consolidated_report_validation_states(), c(
    "absent", "valid", "valid_legacy", "stale", "invalid", "unknown",
    "blocked"))
})

# ── 4F-EXT : domaine global bulk_multi_comparison (contrat §2) ──────────────
test_that("the global domain bulk_multi_comparison reads global_data additively", {
  # Catégorie "global" figée : ni contrat (provenance), ni legacy SC.
  expect_setequal(.report_global_domains, "bulk_multi_comparison")
  src_col <- .rep_report_src(file.path(.rep_pkg_dir, "report_collector.R"))
  # M-3/C17 (2026-09-30) : l'accès direct `global_data$bulk_multi_comparison`
  # passe désormais par l'accesseur — l'INTENTION du gel (lecture additive de
  # l'état, jamais de ré-exécution) est inchangée.
  expect_match(src_col, 'state_get(global_data, "bulk_multi_comparison")', fixed = TRUE)
  expect_match(src_col, "global_data = NULL", fixed = TRUE)
  # Le module 9b transmet global_data au collecteur.
  src_mod <- .rep_report_src("modules/sc/mod_sc_report_consolidated.R")
  expect_match(src_mod, "global_data = global_data", fixed = TRUE)
  # Le bundle exporte les 3 tables du domaine.
  src_bdl <- .rep_report_src(file.path(.rep_pkg_dir, "report_bundle.R"))
  for (f in c("bulk_multi_per_dataset.csv", "bulk_multi_concordance.csv",
              "bulk_multi_intersection.csv")) {
    expect_match(src_bdl, f, fixed = TRUE, info = f)
  }
  # Le collecteur n'écrit JAMAIS dans global_data (lecture seule, garde §2).
  expect_false(grepl("global_data\\$bulk_multi_comparison\\s*<-", src_col,
                     perl = TRUE))
})

test_that("LIANA report integration reads the collection and never executes analysis", {
  src_col <- .rep_report_src(file.path(.rep_pkg_dir, "report_collector.R"))
  expect_match(src_col, 'state_get(shared_rv, "communication_collection")',
               fixed = TRUE)
  expect_match(src_col, 'state_get(shared_rv, "active_communication_sample")',
               fixed = TRUE)
  expect_match(src_col, "assert_liana_collection(collection, sc_obj)",
               fixed = TRUE)
  expect_match(src_col, "build_liana_collection_table(collection)", fixed = TRUE)
  expect_false(grepl("run_liana\\(|run_liana_by_sample\\(", src_col))
  expect_false(grepl("communication_collection\\s*<-|active_communication_sample\\s*<-",
                     src_col, perl = TRUE))
  expect_false(grepl("run_liana\\(|run_liana_by_sample\\(",
                     .rep_report_src(file.path(.rep_pkg_dir, "report_render.R"))))
  expect_false(grepl("run_liana\\(|run_liana_by_sample\\(",
                     .rep_report_src(file.path(.rep_pkg_dir, "report_bundle.R"))))
})

test_that("render and bundle expose the three LIANA tables without changing source neutrality", {
  src_render <- .rep_report_src(file.path(.rep_pkg_dir, "report_render.R"))
  expect_match(src_render, 'communication = "Communication cellulaire"',
               fixed = TRUE)
  expect_match(src_render, "l'échantillon biologique est ", fixed = TRUE)
  expect_match(src_render, "les cellules ne sont pas des réplicats", fixed = TRUE)
  src_bundle <- .rep_report_src(file.path(.rep_pkg_dir, "report_bundle.R"))
  for (file in c("communication_canonical.csv",
                 "communication_sample_manifest.csv",
                 "communication_condition_summary.csv",
                 "communication_by_sample.csv")) {
    expect_match(src_bundle, file, fixed = TRUE)
  }
})

# ── Garde-fous scientifiques figés ──────────────────────────────────────────
test_that("the collector REUSES the v2 fingerprint and never re-implements it", {
  src <- .rep_report_src(file.path(.rep_pkg_dir, "report_collector.R"))
  expect_match(src, "build_object_identity_v2(", fixed = TRUE)
  expect_false(grepl("digest::digest|xxhash64", src))
})

test_that("the render layer is pandoc-free (htmltools compiler, no re-execution)", {
  src <- .rep_report_src(file.path(.rep_pkg_dir, "report_render.R"))
  expect_false(grepl("rmarkdown::|knitr::|pandoc|render\\(", src))
  # Aucune re-execution d'analyse dans le rendu.
  expect_false(grepl("run_milo_da\\(|run_sccoda_da\\(|testNhoods\\(|FindAllMarkers",
                     src))
})

test_that("validator states and blocking rules match the contract wording", {
  src <- .rep_report_src(file.path(.rep_pkg_dir, "report_validator.R"))
  expect_match(src, '"invalid_design"', fixed = TRUE)
  expect_match(src, "aucune reconstruction", fixed = TRUE)
})

test_that("the bundle never exports raw data (nhood_assignment excluded)", {
  src <- .rep_report_src(file.path(.rep_pkg_dir, "report_bundle.R"))
  expect_false(grepl("nhood_assignment|n_cells_in_nhoods|spliced", src))
})

# ── Module : orchestration seule, aucun calcul ──────────────────────────────
test_that("report module defines exactly its three orchestration functions", {
  expect_setequal(
    .rep_top_level_assignments("modules/sc/mod_sc_report_consolidated.R"),
    c("mod_sc_report_consolidated_ui", "mod_sc_report_consolidated_output_ui",
      "mod_sc_report_consolidated_server")
  )
  src <- .rep_report_src("modules/sc/mod_sc_report_consolidated.R")
  # Le module consomme les fonctions documentees du domaine rapport.
  expect_match(src, "collect_consolidated_report_input(", fixed = TRUE)
  expect_match(src, "validate_consolidated_report_input(", fixed = TRUE)
  expect_match(src, "write_consolidated_report_html(", fixed = TRUE)
  expect_match(src, "build_report_bundle(", fixed = TRUE)
  expect_match(src, "consolidated_report_export_filename(", fixed = TRUE)
  # AUCUN calcul scientifique dans le module.
  expect_false(grepl("run_milo_da\\(|run_sccoda_da\\(|testNhoods\\(|reticulate::|FindAllMarkers",
                     src))
})

test_that("mod_sc.R mounts panel 9b + tab without touching the legacy panel 9", {
  mod_sc_src <- .rep_report_src("modules/sc/mod_sc.R")
  expect_match(mod_sc_src, "9b. Rapport consolidé (4F)", fixed = TRUE)
  expect_match(mod_sc_src, "tab_report_consolide", fixed = TRUE)
  expect_match(mod_sc_src, "mod_sc_report_consolidated_server", fixed = TRUE)
  # Le panneau 9 historique (rapport Rmd par domaine) reste en place.
  expect_match(mod_sc_src, "sc_report_template.Rmd", fixed = TRUE)
  expect_match(mod_sc_src, "9. Rapport Complet", fixed = TRUE)
})

test_that("app.R sources the report domain AFTER all domain files", {
  # M5 lot 2 : les 4 fichiers vivent dans tools/pkg_skeleton/R/ — app.R le
  # source() TOUJOURS, au MEME ordre de position (bloc 08, manifeste 77-80).
  # M5 lot 4 : spatial_report.R suit le domaine spatial dans le paquet — la
  # position de reference suit le chemin (meme bloc 07, meme ordre).
  app_src <- .rep_report_src("app.R")
  pos_spatial <- regexpr('source("tools/pkg_skeleton/R/spatial_report.R")', app_src, fixed = TRUE)
  pos_collector <- regexpr('source("tools/pkg_skeleton/R/report_collector.R")',
                           app_src, fixed = TRUE)
  pos_module <- regexpr('source("modules/sc/mod_sc_report_consolidated.R")',
                        app_src, fixed = TRUE)
  expect_gt(pos_spatial, 0)
  expect_gt(pos_collector, pos_spatial)
  expect_gt(pos_module, pos_collector)
  for (f in c("report_validator.R", "report_render.R", "report_bundle.R")) {
    expect_match(app_src, paste0('source("tools/pkg_skeleton/R/', f, '")'),
                 fixed = TRUE)
  }
})

# ── Config + contrat : synchronisation code / doc ────────────────────────────
test_that("config declares the TS_REPORT_* constants consumed by the contract", {
  cfg <- .rep_report_src("config/defaults.R")
  expect_match(cfg, "TS_REPORT_MAX_TABLE_ROWS", fixed = TRUE)
  expect_match(cfg, "TS_REPORT_MAX_PROVENANCE_ROWS", fixed = TRUE)
  src <- .rep_report_src(file.path(.rep_pkg_dir, "report_render.R"))
  expect_match(src, "TS_REPORT_MAX_TABLE_ROWS", fixed = TRUE)
  expect_match(src, "TS_REPORT_MAX_PROVENANCE_ROWS", fixed = TRUE)
  # M5 lot 2 — « carry + pin » (précédent .ts_dt_const/.ts_export_const) :
  # les sites :82/:238 n'ont AUCUN fallback exists() sur la constante nue ;
  # le paquet fournit .ts_report_const() dont les replis sont ÉPINGLÉS contre
  # config/defaults.R:157-158. Le gel tient des DEUX côtés : la config porte
  # les valeurs courantes, le paquet porte les mêmes en repli.
  expect_match(src, '.ts_report_const("TS_REPORT_MAX_TABLE_ROWS", 200L',
               fixed = TRUE)
  expect_match(src, '.ts_report_const("TS_REPORT_MAX_PROVENANCE_ROWS", 500L',
               fixed = TRUE)
  expect_identical(get("TS_REPORT_MAX_TABLE_ROWS", envir = globalenv()), 200L)
  expect_identical(get("TS_REPORT_MAX_PROVENANCE_ROWS", envir = globalenv()), 500L)
})

test_that("the contract document stays in sync with the frozen surface", {
  skip_if_not(file.exists(file.path(ts_project_root(), CONTRACT_REPORT)), .ts_contract_skip_msg(file.path(ts_project_root(), CONTRACT_REPORT)))
  doc <- .rep_report_src(CONTRACT_REPORT)
  expect_match(doc, "consolidated_report_input", fixed = TRUE)
  expect_match(doc, "sc-report-consolide", fixed = TRUE)
  expect_match(doc, "COMPILATEUR", fixed = TRUE)
  expect_match(doc, "aucune analyse ré-exécutée", ignore.case = TRUE)
  expect_match(doc, "mod_sc_report_consolidated", fixed = TRUE)
  expect_match(doc, "9b", fixed = TRUE)
  expect_match(doc, "TS_REPORT_MAX_TABLE_ROWS", fixed = TRUE)
  # 4F-EXT : domaine global documenté (code <-> doc simultanés).
  expect_match(doc, "bulk_multi_comparison", fixed = TRUE)
  expect_match(doc, "global_data$bulk_multi_comparison", fixed = TRUE)
  expect_match(doc, "12 domaines figés", fixed = TRUE)
  expect_match(doc, "communication_collection", fixed = TRUE)
  expect_match(doc, "active_communication_sample", fixed = TRUE)
  expect_match(doc, "communication_sample_manifest.csv", fixed = TRUE)
  expect_match(doc, "communication_condition_summary.csv", fixed = TRUE)
  expect_match(doc, "communication_by_sample.csv", fixed = TRUE)
  expect_match(doc, "l'échantillon biologique est l'unité de réplication",
               fixed = TRUE)
  expect_match(doc, "7 états **figés**", fixed = TRUE)
  # Les 7 etats de validation sont documentes
  for (st in consolidated_report_validation_states()) {
    expect_match(doc, paste0("`", st, "`"), fixed = TRUE)
  }
})

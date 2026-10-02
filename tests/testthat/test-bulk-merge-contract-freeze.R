# =============================================================================
# test-bulk-merge-contract-freeze.R — NEW-2 : gel du contrat
# docs/contracts/BULK_MERGE_CONTRACT.md (fusion de jeux bulk)
# =============================================================================
# Verrouille : surface publique (noms + signatures), états d'erreur (code <>
# contrat), PURETÉ Shiny du moteur, gardes de RÉUTILISATION (STAT-S1 / MD-1
# cités, jamais redéfinis), module sans shared_rv et sans écriture sur le
# conteneur, ancres app.R + mod_bulk.R, AUCUNE constante TS_ nouvelle dans le
# moteur, sync code <-> contrat, clés i18n.
# =============================================================================
source_project_file("R/core/io_helpers.R")         # %||%
source_project_file("R/bulk/bulk_multi.R")
source_project_file("R/bulk/bulk_batch_qc.R")
source_project_file("R/bulk/bulk_provenance.R")
source_project_file("R/bulk/batch_correction.R")
source_project_file("R/bulk/bulk_merge.R")

.ts_top_level_names <- function(relpath) {
  exprs <- parse(file.path(ts_project_root(), relpath))
  nms <- character(0)
  for (e in exprs) {
    if (is.call(e) && identical(e[[1L]], as.name("<-")) && is.name(e[[2L]])) {
      nms <- c(nms, as.character(e[[2L]]))
    }
  }
  unique(nms)
}

.ts_read <- function(relpath) {
  if (grepl("^docs/contracts/", relpath) &&
      !file.exists(file.path(ts_project_root(), relpath))) {
    skip(.ts_contract_skip_msg(relpath))
  }
  paste(readLines(file.path(ts_project_root(), relpath), warn = FALSE),
        collapse = "\n")
}

.engine_src <- .ts_read("R/bulk/bulk_merge.R")
.module_src <- .ts_read("modules/bulk/mod_bulk_merge.R")
.contract   <- .ts_read("docs/contracts/BULK_MERGE_CONTRACT.md")
.app_src    <- .ts_read("app.R")
.parent_src <- .ts_read("modules/bulk/mod_bulk.R")

# ── Surface publique gelée (contrat §2, §8) ────────────────────────────────
test_that("public API surface is frozen (names + signatures)", {
  expect_setequal(
    bulk_merge_public_api(),
    c("bulk_merge_public_api", "bulk_merge_error_states",
      "bulk_merge_check_inputs", "bulk_merge_common_meta_columns",
      "bulk_merge_align_genes", "bulk_merge_preview_metadata",
      "bulk_merge_combine", "bulk_merge_run", "bulk_merge_to_bulk_obj")
  )
  expect_setequal(
    bulk_merge_public_api(),
    intersect(bulk_merge_public_api(), .ts_top_level_names("R/bulk/bulk_merge.R"))
  )
  expected_args <- list(
    bulk_merge_public_api             = list(),
    bulk_merge_error_states           = list(),
    bulk_merge_check_inputs           = list(datasets = NULL, condition_col = NULL),
    bulk_merge_common_meta_columns    = list(datasets = NULL),
    bulk_merge_align_genes            = list(datasets = NULL),
    bulk_merge_preview_metadata       = list(datasets = NULL, batch_col = "dataset_origin"),
    bulk_merge_combine                = list(datasets = NULL, common_genes = NULL,
                                             batch_col = "dataset_origin"),
    bulk_merge_run                    = list(datasets = NULL, condition_col = NULL,
                                             apply_combat = FALSE,
                                             batch_col = "dataset_origin"),
    bulk_merge_to_bulk_obj            = list(result = NULL, project_name = "")
  )
  for (fn_name in names(expected_args)) {
    expect_true(exists(fn_name, where = globalenv(), inherits = FALSE),
                info = paste("fonction publique manquante :", fn_name))
    fn <- get(fn_name, envir = globalenv())
    expect_true(is.function(fn), info = fn_name)
    expect_identical(names(formals(fn)), names(expected_args[[fn_name]]),
                     info = fn_name)
  }
})

# ── États d'erreur figés (contrat §8) ──────────────────────────────────────
test_that("error states are frozen in code AND contract (no silent inflation)", {
  states_code <- bulk_merge_error_states()
  expect_setequal(states_code,
                  c("invalid_input", "insufficient_datasets",
                    "no_common_genes", "invalid_metadata",
                    "design_not_applicable"))
  emitted <- unique(regmatches(.engine_src,
                               gregexpr('(?<=state = ")[^"]+', .engine_src,
                                        perl = TRUE))[[1L]])
  expect_true(all(emitted %in% states_code),
              info = paste("états hors ensemble gelé :",
                           paste(setdiff(emitted, states_code), collapse = ", ")))
  for (st in states_code) {
    expect_match(.contract, st, fixed = TRUE,
                 info = paste("état absent du contrat :", st))
  }
  # Classe d'erreur dédiée : bulk_merge_error (jamais une classe étrangère)
  expect_match(.engine_src, 'class = "bulk_merge_error"', fixed = TRUE)
})

# ── Pureté Shiny du moteur (contrat §2, R/ = logique pure) ─────────────────
test_that("engine is pure R (no Shiny symbols)", {
  shiny_patterns <- c("input\\$", "output\\$", "renderPlot", "renderUI",
                      "renderDT", "observeEvent", "observe\\(", "reactive\\(",
                      "reactiveVal", "reactiveValues", "moduleServer",
                      "showNotification", "NS\\(", "session\\$", "req\\(",
                      "isolate\\(", "Progress\\$")
  for (pat in shiny_patterns) {
    expect_false(grepl(pat, .engine_src),
                 info = paste("symbole Shiny détecté dans le moteur :", pat))
  }
})

# ── Réutilisation stricte (contrat §2) : cité, jamais redéfini ─────────────
test_that("engine reuses frozen domains and never redefines them", {
  for (sym in c("bulk_multi_check_obj", "run_combat_seq",
                "bulk_batch_correction_design", "bulk_batch_correction_label",
                "bulk_update_provenance")) {
    expect_match(.engine_src, sym, fixed = TRUE,
                 info = paste("symbole réutilisé absent du moteur :", sym))
  }
  for (sym in c("bulk_multi_check_obj", "run_combat_seq",
                "bulk_batch_correction_design", "bulk_batch_correction_label",
                "bulk_update_provenance", "bulk_batch_design_check")) {
    expect_false(grepl(paste0(sym, "\\s*<-\\s*function"), .engine_src),
                 info = paste("redéfinition interdite dans le moteur :", sym))
  }
  # Le module réutilise le moteur + la chaîne canonique de tracé/transformation
  for (sym in c("bulk_merge_run", "bulk_merge_to_bulk_obj",
                "bulk_merge_align_genes", "bulk_merge_preview_metadata",
                "bulk_merge_common_meta_columns", "bulk_batch_correction_design",
                "build_dds", "get_vst_matrix", "plot_bulk_pca",
                "plot_batch_correction_pca", "ts_datatable")) {
    expect_match(.module_src, sym, fixed = TRUE,
                 info = paste("symbole réutilisé absent du module :", sym))
  }
  for (sym in c("bulk_merge_run", "bulk_batch_correction_design",
                "plot_bulk_pca", "plot_batch_correction_pca")) {
    expect_false(grepl(paste0(sym, "\\s*<-\\s*function"), .module_src),
                 info = paste("redéfinition interdite dans le module :", sym))
  }
})

# ── Module : comportement d'un import (contrat §7) ─────────────────────────
test_that("module writes ONLY global_data$bulk_obj (never shared_rv / container)", {
  expect_false(grepl("shared_rv", .module_src),
               info = "le module ne doit même pas recevoir shared_rv")
  expect_match(.module_src, "global_data$bulk_obj <- obj", fixed = TRUE)
  expect_false(grepl("global_data\\$bulk_datasets\\s*<-", .module_src),
               info = "écriture interdite sur le conteneur bulk_datasets")
  expect_false(grepl("bulk_multi_comparison", .module_src),
               info = "écriture interdite sur bulk_multi_comparison")
  # Aucun accès direct aux données d'un jeu : tout passe par le moteur
  expect_false(grepl("\\$counts\\[", .module_src),
               info = "indexation de counts dans le module — logique à extraire")
})

# ── Ancres de câblage (contrat §9) ─────────────────────────────────────────
test_that("app.R and mod_bulk.R anchors are present", {
  expect_match(.app_src, 'source("R/bulk/bulk_merge.R")', fixed = TRUE)
  expect_match(.app_src, 'source("modules/bulk/mod_bulk_merge.R")', fixed = TRUE)
  expect_match(.parent_src, 'value = "panel_merge"', fixed = TRUE)
  expect_match(.parent_src, 'value = "tab_bulk_merge"', fixed = TRUE)
  expect_match(.parent_src, "mod_bulk_merge_ui(ns(\"merge\"))", fixed = TRUE)
  expect_match(.parent_src, "mod_bulk_merge_output_ui(ns(\"merge\"))", fixed = TRUE)
  expect_match(.parent_src, 'mod_bulk_merge_server("merge", global_data)',
               fixed = TRUE)
})

# ── Aucun nouveau seuil config (contrat §10) ───────────────────────────────
test_that("engine introduces no new TS_ threshold (constraints come from STAT-S1)", {
  expect_false(grepl("TS_", .engine_src),
               info = "constante TS_ détectée dans le moteur — passer par config/ si réellement nécessaire")
  expect_match(.ts_read("config/thresholds.R"),
               "TS_BULK_BATCH_MIN_SAMPLES_PER_BATCH", fixed = TRUE)
})

# ── Colonne lot déclarée (contrat §5.2) ────────────────────────────────────
test_that("batch column name dataset_origin is frozen in code and contract", {
  expect_match(.engine_src, '"dataset_origin"', fixed = TRUE)
  expect_match(.contract, "dataset_origin", fixed = TRUE)
  expect_match(.module_src, '"dataset_origin"', fixed = TRUE)
  # Le produit est façonné en bulk_obj avec import_mode = "merge" (contrat §7)
  expect_match(.engine_src, 'import_mode = "merge"', fixed = TRUE)
  expect_match(.contract, 'import_mode = "merge"', fixed = TRUE)
  # counts_pre_combat : les counts bruts fusionnés (PCA « avant »)
  expect_match(.engine_src, "counts_pre_combat", fixed = TRUE)
  expect_match(.contract, "counts_pre_combat", fixed = TRUE)
})

# ── Sync code <> contrat (sections clés) ───────────────────────────────────
test_that("contract document is in sync (key sections present)", {
  skip_if_not(file.exists(file.path(ts_project_root(), "docs/contracts/BULK_MERGE_CONTRACT.md")), .ts_contract_skip_msg("docs/contracts/BULK_MERGE_CONTRACT.md"))
  for (token in c("bulk_merge_check_inputs", "bulk_merge_align_genes",
                  "bulk_merge_preview_metadata", "bulk_merge_combine",
                  "bulk_merge_run", "bulk_merge_to_bulk_obj",
                  "bulk_merge_common_meta_columns",
                  "ComBat-seq", "no_common_genes", "design_not_applicable",
                  "bulk_merge_error", "Multi-jeux — Fusion de jeux",
                  "Fusion de jeux")) {
    expect_match(.contract, token, fixed = TRUE,
                 info = paste("token absent du contrat :", token))
  }
  # Écart assumé documenté (fiche NEW-2 vs arbre MD-1..MD-4)
  expect_match(.contract, "mod_import_bulk", fixed = TRUE)
  expect_match(.contract, "Écart assumé", fixed = TRUE)
})

# ── Clés i18n (contrat §9) ─────────────────────────────────────────────────
test_that("i18n keys used by the module exist in translation.json", {
  json <- jsonlite::fromJSON(
    file.path(ts_project_root(), "i18n/translation.json"),
    simplifyVector = FALSE)$translation
  fr_keys <- vapply(json, function(x) x$fr %||% "", character(1))
  needed <- c(
    "Multi-jeux — Fusion de jeux", "Fusion de jeux",
    "Datasets à fusionner (>= 2)", "Colonne condition commune (optionnel)",
    "Harmonisation ComBat-seq (lot = jeu d'origine)", "Nom du jeu fusionné",
    "Fusionner et charger comme jeu actif",
    "Le jeu actif courant sera remplacé (comme un import). Enregistrez-le d'abord via « Enregistrer l'état courant » si nécessaire.",
    "Sélectionnez au moins 2 datasets enregistrés.",
    "Fusion impossible : %s",
    "sans correction", "PCA avant / après", "Gènes par dataset",
    "Résultat de la fusion"
  )
  for (k in needed) {
    expect_true(k %in% fr_keys, info = paste("clé i18n manquante :", k))
  }
})

# =============================================================================
# test-sc-multi-contract-freeze.R — MD-4 : gel du contrat
# docs/contracts/SC_MULTI_CONTRACT.md (conteneur sc_datasets & double jeu SC)
# =============================================================================
# Verrouille : surface publique, signatures, états d'erreur, relations
# déclarées, PURETÉ Shiny de la logique, seuil config + repli, ancres app.R
# (init / snapshot / restore / reset), câblage des deux producteurs,
# ZÉRO référence à sc_datasets dans les fichiers SC préexistants (garde §2.2),
# sync code <-> contrat, clés i18n.
# =============================================================================
source_project_file("R/sc/sc_multi.R")
.local_source_if_exists("config/thresholds.R")

.ts_read <- function(relpath) {
  paste(readLines(file.path(ts_project_root(), relpath), warn = FALSE),
        collapse = "\n")
}

# ── Surface publique gelée ─────────────────────────────────────────────────
test_that("public API surface is frozen (names + signatures)", {
  expect_setequal(
    sc_multi_public_api(),
    c("sc_multi_public_api", "sc_multi_error_states", "sc_multi_relations",
      "sc_multi_check_label", "sc_multi_check_obj", "sc_multi_check_relation",
      "sc_multi_register", "sc_multi_remove", "sc_multi_get",
      "sc_multi_summary")
  )
  for (fn_name in sc_multi_public_api()) {
    expect_true(exists(fn_name, where = globalenv(), inherits = FALSE),
                info = paste("fonction absente :", fn_name))
  }
  expected_args <- list(
    sc_multi_public_api     = list(),
    sc_multi_error_states   = list(),
    sc_multi_relations      = list(),
    sc_multi_check_label    = list(label = NULL),
    sc_multi_check_obj      = list(obj = NULL),
    sc_multi_check_relation = list(relation = NULL),
    sc_multi_register       = list(datasets = NULL, label = NULL, obj = NULL,
                                   relation = "standalone",
                                   producer = "pipeline_save",
                                   overwrite = FALSE, max_datasets = NULL),
    sc_multi_remove         = list(datasets = NULL, label = NULL),
    sc_multi_get            = list(datasets = NULL, label = NULL),
    sc_multi_summary        = list(datasets = NULL)
  )
  for (fn_name in names(expected_args)) {
    fmls <- names(formals(get(fn_name, envir = globalenv())))
    expect_identical(fmls, names(expected_args[[fn_name]]),
                     info = paste("signature de", fn_name))
  }
})

# ── États d'erreur + relations gelés ───────────────────────────────────────
test_that("error states and declared relations are frozen", {
  expect_setequal(
    sc_multi_error_states(),
    c("invalid_input", "invalid_label", "invalid_obj", "invalid_relation",
      "duplicate_label", "unknown_label", "capacity_exceeded")
  )
  expect_setequal(
    sc_multi_relations(),
    c("standalone", "shared_params", "distinct_params")
  )
  # Toute erreur émise appartient à l'ensemble gelé (invariant).
  bad <- tryCatch(sc_multi_check_relation("merged"), error = function(e) e)
  expect_true(bad$state %in% sc_multi_error_states())
  # Le contrat cite les relations et les modes de la décision 5.
  doc <- .ts_read("docs/contracts/SC_MULTI_CONTRACT.md")
  for (anchor in c("standalone", "shared_params", "distinct_params",
                   "mode 1", "mode 2", "TS_SC_MULTI_MAX_DATASETS",
                   "global_data$sc_datasets", "overwrite = TRUE",
                   "registered_at", "updated_at", "sc_multi_summary()",
                   "ts_datatable", "page_length = 6", "BULK_MULTI_CONTRACT.md")) {
    expect_match(doc, anchor, fixed = TRUE, info = anchor)
  }
})

# ── Pureté Shiny (contrat §2.5) ────────────────────────────────────────────
test_that("R/sc/sc_multi.R is pure (no Shiny symbols)", {
  src <- .ts_read("R/sc/sc_multi.R")
  for (sym in c("reactiveVal", "reactiveValues", "observeEvent", "output\\$",
                "input\\$", "moduleServer", "showNotification",
                "isolate\\(", "reactive\\(")) {
    expect_false(grepl(sym, src, perl = TRUE),
                 info = paste("symbole Shiny interdit trouvé :", sym))
  }
})

# ── Seuil config + repli (contrat §9) ──────────────────────────────────────
test_that("capacity threshold is declared in config and consumed with fallback", {
  expect_true(exists("TS_SC_MULTI_MAX_DATASETS", inherits = TRUE))
  # 2026-09-27 : relevé de 5 à 20 (feature 6×10X — les 6 réplicats doivent
  # tenir dans le conteneur ; aligné sur TS_BULK_MULTI_MAX_DATASETS).
  expect_identical(TS_SC_MULTI_MAX_DATASETS, 20L)
  # repli si la config n'est pas sourcée : la valeur de repli est dans le
  # source de la logique pure (20L, même pattern que TS_BULK_MULTI_MAX_DATASETS).
  src <- .ts_read("R/sc/sc_multi.R")
  expect_match(src, "TS_SC_MULTI_MAX_DATASETS", fixed = TRUE)
  expect_match(src, "      20L", fixed = TRUE)
})

# ── Producteur 1 : import SC (contrat §6) ──────────────────────────────────
test_that("mod_import_sc.R registers via the pure API (producer = import)", {
  imp <- .ts_read("modules/import/mod_import_sc.R")
  expect_match(imp, "multi_label", fixed = TRUE)
  expect_match(imp, "multi_relation", fixed = TRUE)
  expect_match(imp, ".register_sc_multi_dataset", fixed = TRUE)
  # Exactement 4 points d'appel (picker .rda + options A/B/C).
  calls <- length(gregexpr(".register_sc_multi_dataset(", imp,
                           fixed = TRUE)[[1L]])
  expect_identical(calls, 4L)
  expect_match(imp, 'producer = "import"', fixed = TRUE)
  expect_match(imp, 'inherits(res, "sc_multi_error")', fixed = TRUE)
  # L'échec d'enregistrement est une ALERTE, pas une interruption : le corps
  # du helper ne contient aucun stop() (l'import ne doit jamais être bloqué).
  lines <- readLines(file.path(ts_project_root(),
                               "modules/import/mod_import_sc.R"),
                     warn = FALSE)
  start <- grep(".register_sc_multi_dataset <- function(obj) {", lines,
                fixed = TRUE)
  expect_length(start, 1L)
  closes <- which(lines[(start + 1L):length(lines)] == "    }")
  expect_true(length(closes) > 0L,
              info = "fin du helper .register_sc_multi_dataset introuvable")
  body_txt <- paste(lines[start:(start + closes[1L])], collapse = "\n")
  expect_false(grepl("stop(", body_txt, fixed = TRUE),
               info = "l'enregistrement import ne doit jamais stopper l'import")
  expect_match(body_txt, "sc_multi_register(", fixed = TRUE)
})

# ── Producteur 2 : gestion (contrat §6) ────────────────────────────────────
test_that("mod_sc_datasets.R is a thin consumer of the pure API", {
  mbd <- .ts_read("modules/sc/mod_sc_datasets.R")
  for (call in c("sc_multi_check_label(", "sc_multi_register(",
                 "sc_multi_remove(", "sc_multi_summary(")) {
    expect_match(mbd, call, fixed = TRUE, info = call)
  }
  expect_match(mbd, 'producer = "pipeline_save"', fixed = TRUE)
  expect_match(mbd, "overwrite = TRUE", fixed = TRUE)
  # Roadmap 4.1 (2026-09-27) : l'ACTIVATION d'un dataset enregistré écrit
  # global_data$sc_obj via sc_multi_get — SEULE écriture autorisée (contrat
  # SC_MULTI_CONTRACT.md §6, amended). Toute autre écriture reste interdite.
  mbd_no_activation <- gsub("global_data\\$sc_obj <- entry\\$obj", "", mbd,
                            perl = TRUE)
  expect_false(grepl("global_data\\$sc_obj\\s*(<-|\\[\\[\\]\\s*<-)",
                     mbd_no_activation, perl = TRUE),
               info = "écriture interdite sur global_data$sc_obj hors activation")
  expect_match(mbd, "sc_multi_get(", fixed = TRUE)
  expect_match(mbd, "sc_obj_epoch", fixed = TRUE)
})

# ── Câblage mod_sc.R (contrat §6) ──────────────────────────────────────────
test_that("mod_sc.R wires the datasets panel (UI + server, additive)", {
  msc <- .ts_read("modules/sc/mod_sc.R")
  expect_match(msc, "mod_sc_datasets_ui(ns(\"sc_datasets\"))", fixed = TRUE)
  expect_match(msc, "mod_sc_datasets_server(  \"sc_datasets\", global_data)",
               fixed = TRUE)
})

# ── Ancres app.R (contrat §6/§7) ───────────────────────────────────────────
test_that("app.R anchors: source, init, snapshot, restore, reset", {
  app <- .ts_read("app.R")
  expect_match(app, 'source("R/sc/sc_multi.R")', fixed = TRUE)
  expect_match(app, 'source("modules/sc/mod_sc_datasets.R")', fixed = TRUE)
  expect_match(app, "sc_datasets = list(),", fixed = TRUE)
  expect_match(app, "sc_datasets = global_data$sc_datasets,", fixed = TRUE)
  expect_match(app, "global_data$sc_datasets <- snapshot$sc_datasets %||% list()",
               fixed = TRUE)
  expect_match(app, "global_data$sc_datasets <- list()", fixed = TRUE)
})

# ── Garde de non-régression (contrat §2.2) ─────────────────────────────────
test_that("NO pre-existing SC file references sc_datasets (zero behavior change)", {
  existing <- c(
    "R/sc/sc_helpers.R", "R/sc/sc_pipeline.R", "R/sc/sc_state.R",
    "R/sc/sc_plotting.R", "R/sc/sc_export.R", "R/sc/sc_trajectory.R",
    "R/sc/sc_velocity.R", "R/sc/sc_bpcells.R",
    "R/sc/sc_abundance_design.R", "R/sc/sc_abundance_milo.R",
    "R/sc/sc_abundance_sccoda.R", "R/sc/sc_abundance_cross_views.R",
    "R/sc/sc_abundance_milo_views.R", "R/sc/sc_abundance_sccoda_views.R",
    "R/sc/sc_communication.R", "R/sc/sc_communication_input.R",
    "R/sc/sc_communication_perturbation.R", "R/sc/sc_communication_spatial.R",
    "R/sc/sc_communication_trajectory.R", "R/sc/sc_communication_velocity.R",
    "R/sc/sc_communication_views.R",
    "modules/sc/mod_sc_pipeline.R", "modules/sc/mod_sc_markers.R",
    "modules/sc/mod_sc_annotation.R", "modules/sc/mod_sc_viz.R",
    "modules/sc/mod_sc_pathways.R", "modules/sc/mod_sc_trajectory.R",
    "modules/sc/mod_sc_velocity.R", "modules/sc/mod_sc_corr.R",
    "modules/sc/mod_sc_pseudobulk.R", "modules/sc/mod_sc_mapping.R",
    "modules/sc/mod_sc_report_consolidated.R"
  )
  for (f in existing) {
    expect_false(grepl("sc_datasets", .ts_read(f), fixed = TRUE),
                 info = paste(f, "ne doit pas référencer sc_datasets —",
                              "hors fichiers câblage MD-4 déclarés au contrat"))
  }
  # Les fichiers de câblage MD-4, eux, le référencent.
  expect_true(grepl("sc_datasets", .ts_read("R/sc/sc_multi.R"), fixed = TRUE))
  expect_true(grepl("sc_datasets", .ts_read("modules/sc/mod_sc_datasets.R"),
                    fixed = TRUE))
})

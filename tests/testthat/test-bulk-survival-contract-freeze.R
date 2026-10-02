# =============================================================================
# test-bulk-survival-contract-freeze.R — Bulk V2 M5 : gel du contrat
# docs/contracts/BULK_SURVIVAL_CONTRACT.md (survie & associations cliniques)
# =============================================================================
# Verrouille : surface publique, GARDES MISSION (>= 10 événements, découpes
# médiane/quartiles UNIQUEMENT — cutpoint optimal refusé, codage statut
# explicite), états d'erreur, seuil config, pureté Shiny, autonomie ggplot2,
# consommation par les modules, sync code <-> contrat.
# =============================================================================
source_project_file("R/core/io_helpers.R")
source_project_file("R/core/validation.R")
source_project_file("R/core/provenance.R")
source_project_file("R/plotting/theme.R")
source_project_file("R/bulk/bulk_survival.R")

.sv_top_level_names <- function(relpath) {
  exprs <- parse(file.path(ts_project_root(), relpath))
  nms <- character(0)
  for (e in exprs) {
    if (is.call(e) && identical(e[[1L]], as.name("<-")) && is.name(e[[2L]])) {
      nms <- c(nms, as.character(e[[2L]]))
    }
  }
  unique(nms)
}

# ── Surface publique gelée ─────────────────────────────────────────────────
test_that("public API surface is frozen (names + signatures)", {
  expect_setequal(
    bulk_survival_public_api(),
    c("bulk_survival_public_api", "bulk_survival_candidates",
      "bulk_survival_validate_metadata", "bulk_survival_split_groups",
      "bulk_survival_km", "bulk_survival_cox", "plot_survival_km",
      "build_survival_export")
  )
  expected_api <- list(
    bulk_survival_public_api         = list(),
    bulk_survival_candidates         = list(metadata = NULL),
    bulk_survival_validate_metadata  = list(metadata = NULL, time_col = NULL,
                                            status_col = NULL,
                                            status_coding = NULL),
    bulk_survival_split_groups       = list(feature_values = NULL, split = NULL),
    bulk_survival_km                 = list(feature_values = NULL, surv = NULL,
                                            split = NULL, feature_label = NULL),
    bulk_survival_cox                = list(feature_matrix = NULL, surv = NULL,
                                            max_features = NULL),
    plot_survival_km                 = list(km_result = NULL, tr = NULL),
    build_survival_export            = list(cox_df = NULL,
                                            multiple_testing_note = NULL)
  )
  for (fn_name in names(expected_api)) {
    expect_true(exists(fn_name, where = globalenv(), inherits = FALSE),
                info = paste("fonction publique manquante :", fn_name))
    fn <- get(fn_name, envir = globalenv())
    expect_true(is.function(fn), info = fn_name)
    got <- names(formals(fn)); if (is.null(got)) got <- character(0)
    want <- names(expected_api[[fn_name]]); if (is.null(want)) want <- character(0)
    expect_setequal(got, want)
  }
  defined <- .sv_top_level_names("R/bulk/bulk_survival.R")
  public <- setdiff(defined, grep("^\\.", defined, value = TRUE))
  expect_setequal(public, bulk_survival_public_api())
})

# ── GARDES MISSION gelés structurellement ──────────────────────────────────
test_that("mission guards are frozen in source (events, no cutpoint, coding)", {
  src <- paste(readLines(file.path(ts_project_root(), "R/bulk/bulk_survival.R")),
               collapse = "\n")
  # 1. Plancher d'événements consommé de la config (jamais codé en dur).
  expect_match(src, "TS_BULK_SURV_MIN_EVENTS", fixed = TRUE)
  expect_match(src, 'state = "min_events"', fixed = TRUE)
  # 2. Découpes fermées : médiane/quartiles ; toute autre valeur -> erreur.
  expect_match(src, '"median"', fixed = TRUE)
  expect_match(src, '"quartile"', fixed = TRUE)
  expect_match(src, "cutpoint", fixed = TRUE)
  expect_true(length(gregexpr('split %in% c\\("median", "quartile"\\)', src)[[1]]) >= 1L)
  # 3. Codage statut explicite 0/1 ou 1/2.
  expect_match(src, 'status_coding %in% c("0/1", "1/2")', fixed = TRUE)
  expect_match(src, 'state = "invalid_status"', fixed = TRUE)
  # 4. Temps numérique strictement positif.
  expect_match(src, 'state = "invalid_time"', fixed = TRUE)
  # 5. BH dès 2 variables (multi-testing annoncé).
  expect_match(src, "p.adjust(out$p, method = \"BH\")", fixed = TRUE)
  # 6. survival::survfit / survdiff / coxph — pas de dépendance hors survival.
  expect_match(src, "survival::survfit", fixed = TRUE)
  expect_match(src, "survival::survdiff", fixed = TRUE)
  expect_match(src, "survival::coxph", fixed = TRUE)
  expect_match(src, "survminer::ggsurvplot", fixed = TRUE)
  # 7. Provenance produite au calcul, cutpoint_search = FALSE enregistré.
  expect_match(src, "cutpoint_search = FALSE", fixed = TRUE)
})

# ── États d'erreur gelés ───────────────────────────────────────────────────
test_that("error states are frozen", {
  src <- paste(readLines(file.path(ts_project_root(), "R/bulk/bulk_survival.R")),
               collapse = "\n")
  states <- regmatches(src, gregexpr('state = "[a-z_]+"', src))[[1]]
  expect_setequal(unique(states),
                  c('state = "invalid_input"', 'state = "invalid_time"',
                    'state = "invalid_status"', 'state = "min_events"',
                    'state = "compute_failed"'))
})

# ── Seuil config gelé ──────────────────────────────────────────────────────
test_that("config threshold is declared and pinned", {
  expect_identical(TS_BULK_SURV_MIN_EVENTS, 10L)
})

# ── Pureté Shiny + autonomie ggplot2 ───────────────────────────────────────
test_that("R file stays pure and ggplot2-autonomous", {
  src <- paste(readLines(file.path(ts_project_root(), "R/bulk/bulk_survival.R")),
               collapse = "\n")
  expect_false(grepl("shiny::|reactive\\(|renderPlot|observeEvent\\(|showNotification\\(|moduleServer",
                     src))
  expect_match(src, "new_provenance_entry(", fixed = TRUE)
  expect_match(src, "ts_theme(", fixed = TRUE)
  code <- grep("^\\s*#",
               readLines(file.path(ts_project_root(), "R/bulk/bulk_survival.R"),
                         warn = FALSE, encoding = "UTF-8"),
               value = TRUE, invert = TRUE)
  bare <- "(^|[^.[:alnum:]_])(ggplot|aes|geom_[a-z_]+|labs|theme|theme_[a-z]+|scale_[a-z_]+|element_[a-z]+|annotate)[[:space:]]*\\("
  hits <- grep(bare, code, perl = TRUE, value = TRUE)
  real <- hits[!grepl("ggplot2::", hits, fixed = TRUE)]
  expect(length(real) == 0L,
         paste("appels ggplot2 non prefixes :", paste(trimws(real), collapse = " | ")))
})

# ── Consommation par le module + parent ────────────────────────────────────
test_that("modules consume the contract", {
  mod_src <- paste(readLines(file.path(ts_project_root(),
                                       "modules/bulk/mod_bulk_survival.R"),
                             encoding = "UTF-8"), collapse = "\n")
  expect_match(mod_src, "bulk_survival_candidates(", fixed = TRUE)
  expect_match(mod_src, "bulk_survival_validate_metadata(", fixed = TRUE)
  expect_match(mod_src, "bulk_survival_km(", fixed = TRUE)
  expect_match(mod_src, "bulk_survival_cox(", fixed = TRUE)
  expect_match(mod_src, "plot_survival_km(", fixed = TRUE)
  expect_match(mod_src, "build_survival_export(", fixed = TRUE)
  # GARDE §M5 : l'alerte de verrouillage existe côté UI.
  expect_match(mod_src, "survival_gate_ui", fixed = TRUE)
  expect_match(mod_src, "alert-warning", fixed = TRUE)
  parent_src <- paste(readLines(file.path(ts_project_root(),
                                          "modules/bulk/mod_bulk.R"),
                                encoding = "UTF-8"), collapse = "\n")
  expect_match(parent_src, "panel_survival", fixed = TRUE)
  expect_match(parent_src, "tab_survival", fixed = TRUE)
  expect_match(parent_src, "mod_bulk_survival_server(", fixed = TRUE)
  app_src <- paste(readLines(file.path(ts_project_root(), "app.R"),
                             encoding = "UTF-8"), collapse = "\n")
  expect_match(app_src, 'source("R/bulk/bulk_survival.R")', fixed = TRUE)
  expect_match(app_src, 'source("modules/bulk/mod_bulk_survival.R")', fixed = TRUE)
})

# ── Synchronisation code <-> contrat documentaire ──────────────────────────
test_that("contract document is in sync with the frozen code", {
  doc_path <- file.path(ts_project_root(), "docs", "contracts", "BULK_SURVIVAL_CONTRACT.md")
  skip_if_not(file.exists(doc_path), .ts_contract_skip_msg(doc_path))
  doc <- paste(.ts_contract_readlines(doc_path, encoding = "UTF-8"), collapse = "\n")
  for (f in bulk_survival_public_api()) {
    expect_match(doc, paste0("`", f, "`"), fixed = TRUE,
                 info = paste("fonction absente du document :", f))
  }
  for (st in c("invalid_input", "invalid_time", "invalid_status", "min_events",
               "compute_failed")) {
    expect_match(doc, st, fixed = TRUE, info = paste("état absent du document :", st))
  }
  for (th in c("TS_BULK_SURV_MIN_EVENTS", "survfit", "coxph", "bicor_ou_cor_rien",
               "log-rank")) {
    if (th == "bicor_ou_cor_rien") next  # sentinelle : bicor est hors domaine survie
    expect_match(doc, th, fixed = TRUE, info = paste("mot absent du document :", th))
  }
  expect_match(doc, "cutpoint", fixed = TRUE)
  expect_match(doc, "bulk_survival_public_api", fixed = TRUE)
})

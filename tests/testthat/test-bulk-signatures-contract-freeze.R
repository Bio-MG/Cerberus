# =============================================================================
# test-bulk-signatures-contract-freeze.R — Bulk V2 M3 : gel du contrat
# docs/contracts/BULK_SIGNATURES_CONTRACT.md (signatures cellulaires)
# =============================================================================
# Verrouille : surface publique, GARDE §M3 (disclaimer dans le résultat,
# l'export et l'UI), ressources locales (jamais réseau), réutilisation du
# moteur M2, re-classage des erreurs, stockage bulk_obj$pathways$signatures,
# pureté Shiny du fichier R/, autonomie ggplot2, sync code <-> contrat.
# =============================================================================
source_project_file("R/core/io_helpers.R")
source_project_file("R/core/validation.R")
source_project_file("R/core/provenance.R")
source_project_file("R/plotting/theme.R")
source_project_file("R/bulk/bulk_batch_qc.R")
source_project_file("R/bulk/bulk_gsva.R")
source_project_file("R/bulk/bulk_gene_sets.R")
source_project_file("R/bulk/bulk_signatures.R")

.bsig_top_level_names <- function(relpath) {
  exprs <- parse(file.path(ts_project_root(), relpath))
  nms <- character(0)
  for (e in exprs) {
    if (is.call(e) && identical(e[[1L]], as.name("<-")) && is.name(e[[2L]])) {
      nms <- c(nms, as.character(e[[2L]]))
    }
  }
  unique(nms)
}

# ── Surface publique gelée (noms + signatures) ─────────────────────────────
test_that("public API surface is frozen (names + signatures)", {
  expect_setequal(
    bulk_signatures_public_api(),
    c("bulk_signatures_public_api", "bulk_signature_resources",
      "bulk_load_signatures", "bulk_score_signatures",
      "build_signature_scores_export")
  )
  expected_api <- list(
    bulk_signatures_public_api    = list(),
    bulk_signature_resources      = list(),
    bulk_load_signatures          = list(resource = NULL, organism = NULL,
                                         rds_path = NULL),
    bulk_score_signatures         = list(expr_matrix = NULL, gene_sets = NULL,
                                         method = NULL, ... = quote(expr = )),
    build_signature_scores_export = list(result = NULL)
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
  defined <- .bsig_top_level_names("R/bulk/bulk_signatures.R")
  public <- setdiff(defined, grep("^\\.", defined, value = TRUE))
  expect_setequal(public, c(bulk_signatures_public_api(),
                            "BULK_SIGNATURES_DISCLAIMER"))
})

# ── GARDE §M3 : disclaimer porté partout ───────────────────────────────────
test_that("relative-scores disclaimer is frozen in result, export and UI", {
  expect_match(BULK_SIGNATURES_DISCLAIMER, "relatifs", fixed = TRUE)
  expect_match(BULK_SIGNATURES_DISCLAIMER, "cytom", fixed = TRUE)
  rds <- file.path(tempdir(), paste0("fz_", Sys.getpid(), ".rds"))
  on.exit(unlink(rds), add = TRUE)
  saveRDS(list(SIG_A = paste0("GENE", 1:30)), rds)
  sets <- bulk_load_signatures("rds_local", rds_path = rds)
  m <- matrix(rnorm(60 * 6) + 8, 60, 6,
              dimnames = list(paste0("GENE", 1:60), paste0("s", 1:6)))
  res <- bulk_score_signatures(m, sets, method = "ssgsea", min_size = 5, max_size = 100)
  expect_identical(res$disclaimer, BULK_SIGNATURES_DISCLAIMER)
  ex <- build_signature_scores_export(res)
  expect_true(all(ex$disclaimer == BULK_SIGNATURES_DISCLAIMER))
  # UI : le disclaimer est présent dans le module (alerte permanente + carte)
  mod_src <- paste(readLines(file.path(ts_project_root(),
                                       "modules/bulk/mod_bulk_signatures.R"),
                             encoding = "UTF-8"), collapse = "\n")
  expect_match(mod_src, "alert alert-warning", fixed = TRUE)
  expect_true(length(gregexpr("Scores de signatures relatifs", mod_src,
                              fixed = TRUE)[[1]]) >= 2L)
  # la colonne disclaimer est appelée dans le downloadHandler CSV
  expect_match(mod_src, "build_signature_scores_export(", fixed = TRUE)
})

# ── Ressources LOCALES (jamais réseau) ─────────────────────────────────────
test_that("resources are local-only (message cites no download; no url fetch)", {
  src <- paste(readLines(file.path(ts_project_root(), "R/bulk/bulk_signatures.R"),
                         encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("download\\.file|url\\(|curl::|httr::|download_file", src),
               info = "appel réseau dans le domaine signatures")
  # Le source stocke les accents en échappements unicode (\u00e9) — match FIXE
  # sur la forme littérale du message d'indisponibilité.
  expect_match(src, "aucun t\\u00e9l\\u00e9chargement", fixed = TRUE,
               info = "message d'indisponibilité doit citer l'absence de téléchargement")
  tbl <- bulk_signature_resources()
  expect_setequal(tbl$resource, c("hallmark", "progeny", "dorothea", "rds_local"))
})

# ── Réutilisation du moteur M2 + erreurs re-classées ───────────────────────
test_that("reuses M2 engine and re-classes errors (no duplication)", {
  src <- paste(readLines(file.path(ts_project_root(), "R/bulk/bulk_signatures.R"),
                         encoding = "UTF-8"), collapse = "\n")
  expect_match(src, "compute_pathway_scores(", fixed = TRUE)
  expect_match(src, "bulk_filter_gene_sets(", fixed = TRUE)
  expect_match(src, "bulk_assert_transformed_matrix(", fixed = TRUE)
  expect_match(src, "new_provenance_entry(", fixed = TRUE)
  expect_match(src, 'e$state %||% "invalid_input"', fixed = TRUE)
  expect_match(src, "decoupleR::run_ulm(", fixed = TRUE)
})

# ── États d'erreur gelés ───────────────────────────────────────────────────
test_that("error states are frozen", {
  src <- paste(readLines(file.path(ts_project_root(), "R/bulk/bulk_signatures.R")),
               collapse = "\n")
  states <- regmatches(src, gregexpr('state = "[a-z_]+"', src))[[1]]
  expect_setequal(unique(states),
                  c('state = "invalid_input"', 'state = "missing_dependency"',
                    'state = "no_gene_sets"', 'state = "compute_failed"'))
})

# ── Schéma du résultat gelé ────────────────────────────────────────────────
test_that("result schema is frozen (disclaimer included)", {
  rds <- file.path(tempdir(), paste0("fz2_", Sys.getpid(), ".rds"))
  on.exit(unlink(rds), add = TRUE)
  saveRDS(list(SIG_A = paste0("GENE", 1:30), SIG_B = paste0("GENE", 21:40)), rds)
  sets <- bulk_load_signatures("rds_local", rds_path = rds)
  m <- matrix(rnorm(60 * 6) + 8, 60, 6,
              dimnames = list(paste0("GENE", 1:60), paste0("s", 1:6)))
  res <- bulk_score_signatures(m, sets, method = "ssgsea", min_size = 5, max_size = 100)
  expect_setequal(names(res),
                  c("type", "status", "analysis_id", "method", "scores",
                    "gene_sets", "qc", "warnings", "disclaimer", "provenance",
                    "timestamp_utc"))
  expect_identical(res$type, "bulk_signature_scores")
  expect_identical(dim(res$scores), c(2L, 6L))
  ex <- build_signature_scores_export(res)
  expect_setequal(colnames(ex), c("signature", "sample", "score", "method",
                                  "analysis_id", "disclaimer"))
})

# ── Seuils config réutilisés (pas de nouveau seuil signatures) ─────────────
test_that("config thresholds are reused from M2", {
  expect_true(exists("TS_BULK_GSVA_MIN_SIZE", inherits = TRUE))
  expect_true(exists("TS_BULK_GSVA_MAX_SIZE", inherits = TRUE))
})

# ── Pureté Shiny du fichier R/ ─────────────────────────────────────────────
test_that("R file stays pure (no Shiny reactivity)", {
  src <- paste(readLines(file.path(ts_project_root(), "R/bulk/bulk_signatures.R")),
               collapse = "\n")
  expect_false(grepl("shiny::|reactive\\(|renderPlot|observeEvent\\(|showNotification\\(|moduleServer",
                     src))
})

# ── Consommation par le module + parent ────────────────────────────────────
test_that("modules consume the contract", {
  mod_src <- paste(readLines(file.path(ts_project_root(),
                                       "modules/bulk/mod_bulk_signatures.R"),
                             encoding = "UTF-8"), collapse = "\n")
  expect_match(mod_src, "bulk_load_signatures(", fixed = TRUE)
  expect_match(mod_src, "bulk_score_signatures(", fixed = TRUE)
  # Stockage contractuel bulk_obj$pathways$signatures (mission §M3).
  expect_match(mod_src, "bo$pathways$signatures <- res$scores", fixed = TRUE)
  parent_src <- paste(readLines(file.path(ts_project_root(),
                                          "modules/bulk/mod_bulk.R"),
                                encoding = "UTF-8"), collapse = "\n")
  expect_match(parent_src, "panel_signatures", fixed = TRUE)
  expect_match(parent_src, "tab_signatures", fixed = TRUE)
  expect_match(parent_src, "mod_bulk_signatures_server(", fixed = TRUE)
  app_src <- paste(readLines(file.path(ts_project_root(), "app.R"),
                             encoding = "UTF-8"), collapse = "\n")
  expect_match(app_src, 'source("R/bulk/bulk_signatures.R")', fixed = TRUE)
  expect_match(app_src, 'source("modules/bulk/mod_bulk_signatures.R")', fixed = TRUE)
})

# ── Autonomie ggplot2 ──────────────────────────────────────────────────────
test_that("R/bulk/bulk_signatures.R has no bare ggplot2 calls", {
  path <- file.path(ts_project_root(), "R/bulk/bulk_signatures.R")
  code <- readLines(path, warn = FALSE, encoding = "UTF-8")
  code <- grep("^\\s*#", code, value = TRUE, invert = TRUE)
  bare <- "(^|[^.[:alnum:]_])(ggplot|aes|geom_[a-z_]+|labs|theme|theme_[a-z]+|scale_[a-z_]+|element_[a-z]+|annotate)[[:space:]]*\\("
  hits <- grep(bare, code, perl = TRUE, value = TRUE)
  real <- hits[!grepl("ggplot2::", hits, fixed = TRUE)]
  expect(length(real) == 0L,
         paste("appels ggplot2 non prefixes :", paste(trimws(real), collapse = " | ")))
})

# ── Synchronisation code <-> contrat documentaire ──────────────────────────
test_that("contract document is in sync with the frozen code", {
  doc_path <- file.path(ts_project_root(), "docs", "contracts",
                        "BULK_SIGNATURES_CONTRACT.md")
  skip_if_not(file.exists(doc_path), .ts_contract_skip_msg(doc_path))
  doc <- paste(.ts_contract_readlines(doc_path, encoding = "UTF-8"), collapse = "\n")
  for (f in c(bulk_signatures_public_api(), "BULK_SIGNATURES_DISCLAIMER")) {
    expect_match(doc, paste0("`", f, "`"), fixed = TRUE,
                 info = paste("fonction absente du document :", f))
  }
  for (st in c("invalid_input", "missing_dependency", "no_gene_sets",
               "compute_failed", "raw_counts_rejected")) {
    expect_match(doc, st, fixed = TRUE, info = paste("état absent du document :", st))
  }
  for (fld in c("disclaimer", "scores", "qc", "provenance", "signatures")) {
    expect_match(doc, fld, fixed = TRUE, info = paste("champ absent du document :", fld))
  }
  expect_match(doc, "bulk_signatures_public_api", fixed = TRUE)
})

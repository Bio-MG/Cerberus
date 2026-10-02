# =============================================================================
# test-bulk-multi-compare-contract-freeze.R — MD-2 : gel du contrat
# docs/contracts/BULK_MULTI_CONTRACT.md §10 (comparaison multi-jeux)
# =============================================================================
# Verrouille : surface publique, états MD-2 (⊆ ensemble gelé du domaine),
# PURETÉ Shiny, RÉUTILISATION stricte des helpers existants (jamais de
# ré-implémentation — garde §10.2.4), ancres app.R (snapshot/restore/reset),
# câblage module, sync code <-> contrat, clés i18n.
# =============================================================================
source_project_file("R/core/io_helpers.R")   # %||% (sourcé en premier par app.R)
source_project_file("R/core/validation.R")
source_project_file("R/bulk/bulk_helpers.R") # build_contrast_gene_sets & co
source_project_file("R/bulk/bulk_multi.R")
source_project_file("R/bulk/bulk_multi_compare.R")
source_project_file("R/plotting/theme.R")

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

# ── Surface publique gelée ─────────────────────────────────────────────────
test_that("public API surface is frozen (names + signatures)", {
  expect_setequal(
    bulk_multi_compare_public_api(),
    c("bulk_multi_compare_public_api", "bulk_multi_entry_contrasts",
      "bulk_multi_common_contrasts", "bulk_multi_deg_gene_sets",
      "bulk_multi_volcano_scales", "bulk_multi_volcano_panel",
      "bulk_multi_concordance", "bulk_multi_run_comparison")
  )
  expect_setequal(
    bulk_multi_compare_public_api(),
    intersect(bulk_multi_compare_public_api(),
              .ts_top_level_names("R/bulk/bulk_multi_compare.R"))
  )
  expected_args <- list(
    bulk_multi_compare_public_api  = list(),
    bulk_multi_entry_contrasts     = list(entry = NULL),
    bulk_multi_common_contrasts    = list(entries = NULL),
    bulk_multi_deg_gene_sets       = list(entries = NULL, contrast = NULL,
                                          lfc_thresh = NULL, padj_thresh = NULL,
                                          direction_aware = FALSE),
    bulk_multi_volcano_scales      = list(res_dfs = NULL),
    bulk_multi_volcano_panel       = list(res_df = NULL, label = NULL,
                                          lfc_thresh = NULL, padj_thresh = NULL,
                                          scales = NULL, engine = NULL,
                                          tr = NULL),
    bulk_multi_concordance         = list(up_down_sets = NULL),
    bulk_multi_run_comparison      = list(entries = NULL, contrast = NULL,
                                          lfc_thresh = NULL, padj_thresh = NULL)
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

# ── États MD-2 : émis ⊆ ensemble gelé du domaine, et les 3 nouveaux présents
test_that("MD-2 error states belong to the frozen domain set (no silent inflation)", {
  states <- bulk_multi_error_states()
  src <- .ts_read("R/bulk/bulk_multi_compare.R")
  emitted <- unique(regmatches(src,
                               gregexpr('(?<=state = ")[^"]+', src,
                                        perl = TRUE))[[1L]])
  expect_true(all(emitted %in% states),
              info = paste("états hors ensemble gelé :",
                           paste(setdiff(emitted, states), collapse = ", ")))
  for (st in c("insufficient_datasets", "no_common_contrast",
               "no_significant_genes")) {
    expect_true(st %in% emitted,
                info = paste("état MD-2 attendu non émis :", st))
  }
  # Le contrat cite les 3 états MD-2 dans le tableau §5.
  doc <- .ts_read("docs/contracts/BULK_MULTI_CONTRACT.md")
  for (st in emitted) {
    expect_match(doc, paste0("`", st, "`"), fixed = TRUE,
                 info = paste("état absent du contrat :", st))
  }
})

# ── PURETÉ Shiny (contrat §10.2.6) ─────────────────────────────────────────
test_that("R/bulk/bulk_multi_compare.R contains no Shiny symbols (pure logic)", {
  src <- .ts_read("R/bulk/bulk_multi_compare.R")
  forbidden <- c(
    "reactiveVal(?!ues)", "reactive\\(", "observeEvent", "observe\\(",
    "output\\$", "input\\$", "moduleServer", "showNotification",
    "session\\$", "\\bNS\\(", "renderDT"
  )
  for (sym in forbidden) {
    expect_false(grepl(sym, src, perl = TRUE),
                 info = paste("symbole Shiny interdit trouvé :", sym))
  }
})

# ── RÉUTILISATION stricte (garde §10.2.4) ──────────────────────────────────
test_that("compare engine reuses existing helpers, never re-implements", {
  src <- .ts_read("R/bulk/bulk_multi_compare.R")
  for (helper in c("build_contrast_gene_sets(", "build_contrast_intersection_dt(",
                   "plot_volcano_bulk(")) {
    expect_match(src, helper, fixed = TRUE, info = helper)
  }
  mod <- .ts_read("modules/bulk/mod_bulk_multi.R")
  expect_match(mod, "plot_upset_contrasts(", fixed = TRUE)
  # Les fonctions clés ne sont PAS redéfinies dans le module.
  for (fn in c("build_contrast_gene_sets <- function",
               "build_contrast_intersection_dt <- function",
               "plot_volcano_bulk <- function")) {
    expect_false(grepl(fn, mod, fixed = TRUE),
                 info = paste("ré-implémentation interdite :", fn))
  }
})

# ── Ancres app.R (contrat §10.4) ───────────────────────────────────────────
test_that("app.R anchors: source, session snapshot, restore, reset", {
  app <- .ts_read("app.R")
  expect_match(app, 'source("R/bulk/bulk_multi_compare.R")', fixed = TRUE)
  expect_match(app, 'source("modules/bulk/mod_bulk_multi.R")', fixed = TRUE)
  expect_match(app, "bulk_multi_comparison = global_data$bulk_multi_comparison,",
               fixed = TRUE)
  expect_match(app, "global_data$bulk_multi_comparison <- snapshot$bulk_multi_comparison %||% NULL",
               fixed = TRUE)
  expect_match(app, "global_data$bulk_multi_comparison <- NULL", fixed = TRUE)
})

# ── Câblage module Bulk (contrat §10.5) ────────────────────────────────────
test_that("mod_bulk.R wires the comparison tab and server", {
  mb <- .ts_read("modules/bulk/mod_bulk.R")
  expect_match(mb, 'mod_bulk_multi_ui(ns("multi"))', fixed = TRUE)
  expect_match(mb, 'mod_bulk_multi_server("multi", global_data)', fixed = TRUE)
})

# ── Sync code <-> contrat §10 ──────────────────────────────────────────────
test_that("contract §10 cites the API, structure and key guards", {
  doc <- .ts_read("docs/contracts/BULK_MULTI_CONTRACT.md")
  for (fn in bulk_multi_compare_public_api()) {
    expect_match(doc, fn, fixed = TRUE,
                 info = paste("fonction absente du contrat :", fn))
  }
  for (anchor in c("coord_cartesian", "patchwork::wrap_plots",
                   "global_data$bulk_multi_comparison",
                   "spatial_multi_integration", "bulk_multi_volcano_scales",
                   "jaccard_up", "pct_same_direction", "stored_")) {
    expect_match(doc, anchor, fixed = TRUE, info = anchor)
  }
  src <- .ts_read("R/bulk/bulk_multi_compare.R")
  expect_match(src, "docs/contracts/BULK_MULTI_CONTRACT.md", fixed = TRUE)
})

# ── Clés i18n MD-2 présentes ───────────────────────────────────────────────
test_that("MD-2 i18n keys exist in translation.json (no FR leak in EN UI)", {
  json <- jsonlite::fromJSON(
    file.path(ts_project_root(), "i18n", "translation.json"),
    simplifyVector = FALSE)
  fr_keys <- vapply(json$translation, function(x) x$fr %||% "", character(1))
  keys <- c(
    "Comparaison multi-jeux",
    "Comparez des jeux enregistrés (import nommé ou bouton « Enregistrer l'état courant ») partageant un même nom de contraste. Les seuils choisis s'appliquent à TOUS les jeux comparés.",
    "Datasets à comparer (>= 2)",
    "Sélectionnez au moins 2 datasets traités (via « Multi-jeux — Datasets enregistrés » ou l'import nommé).",
    "Aucun nom de contraste commun aux datasets sélectionnés.",
    "Contraste commun",
    "Lancer la comparaison",
    "Comparaison impossible : %s",
    "✓ Comparaison calculée (contraste « %s », %d datasets).",
    "Un dataset comparé a été supprimé depuis le calcul — relancez la comparaison.",
    "Moins de 2 datasets avec des gènes significatifs — pas d'UpSet.",
    "Aucun gène dans les intersections avec ces seuils.",
    "Résultats de la comparaison",
    "Volcanos côte à côte",
    "Recouvrement des DEGs",
    "Gènes par intersection",
    "Concordance de direction",
    "Jaccard = recouvrement des ensembles Up (resp. Down) entre deux jeux. « % même direction » = proportion des gènes significatifs communs qui varient dans le même sens.",
    "Détail des datasets",
    "En attente — sélectionnez >= 2 datasets puis lancez la comparaison.",
    "✓ %s — %d datasets : %s"
  )
  for (k in keys) {
    expect_true(k %in% fr_keys, info = paste("clé i18n absente :", k))
  }
})

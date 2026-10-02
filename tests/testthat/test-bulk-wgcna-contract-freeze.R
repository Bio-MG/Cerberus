# =============================================================================
# test-bulk-wgcna-contract-freeze.R — Bulk V2 M4 : gel du contrat
# docs/contracts/BULK_WGCNA_CONTRACT.md (WGCNA safe-mode)
# =============================================================================
# Verrouille : surface publique, GARDES MISSION (N >= 15 arrêt dur, HVG
# [2000, 5000], maxBlockSize borné, threads DÉSACTIVÉS — enableWGCNAThreads
# interdit, quirk d'attache WGCNA avec détachement systématique), états
# d'erreur, schémas power/modules, seuils config, pureté Shiny, autonomie
# ggplot2, consommation par les modules, sync code <-> contrat.
# =============================================================================
source_project_file("R/core/io_helpers.R")
source_project_file("R/core/validation.R")
source_project_file("R/core/provenance.R")
source_project_file("R/plotting/theme.R")
source_project_file("R/bulk/bulk_batch_qc.R")
source_project_file("R/bulk/bulk_wgcna.R")

.wg_top_level_names <- function(relpath) {
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
    bulk_wgcna_public_api(),
    c("bulk_wgcna_public_api", "bulk_wgcna_select_hvg", "bulk_wgcna_choose_power",
      "bulk_wgcna_pick_power", "bulk_wgcna_build_modules", "bulk_wgcna_prepare_traits",
      "bulk_wgcna_module_trait", "plot_wgcna_soft_threshold",
      "plot_wgcna_trait_heatmap", "build_wgcna_export")
  )
  expected_api <- list(
    bulk_wgcna_public_api      = list(),
    bulk_wgcna_select_hvg      = list(vst_matrix = NULL, n_top = NULL),
    bulk_wgcna_choose_power    = list(power_table = NULL, r2_min = NULL),
    bulk_wgcna_pick_power      = list(vst_matrix = NULL, n_top = NULL,
                                      powers = NULL),
    bulk_wgcna_build_modules   = list(vst_matrix = NULL, power = NULL,
                                      n_top = NULL, deep_split = NULL),
    bulk_wgcna_prepare_traits  = list(metadata = NULL, candidate_cols = NULL),
    bulk_wgcna_module_trait    = list(module_result = NULL, traits = NULL),
    plot_wgcna_soft_threshold  = list(power_result = NULL, tr = NULL),
    plot_wgcna_trait_heatmap   = list(trait_cor = NULL, tr = NULL),
    build_wgcna_export         = list(modules_result = NULL, trait_cor = NULL)
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
  defined <- .wg_top_level_names("R/bulk/bulk_wgcna.R")
  public <- setdiff(defined, grep("^\\.", defined, value = TRUE))
  expect_setequal(public, c(bulk_wgcna_public_api(), "labels2colors_safe"))
})

# ── GARDES MISSION gelés structurellement ──────────────────────────────────
test_that("mission guards are frozen in source (N>=15, HVG bounds, TOM, threads)", {
  src <- paste(readLines(file.path(ts_project_root(), "R/bulk/bulk_wgcna.R")),
               collapse = "\n")
  # 1. Arrêt dur N < TS_BULK_WGCNA_MIN_SAMPLES — cité dans les DEUX compute.
  expect_match(src, "TS_BULK_WGCNA_MIN_SAMPLES", fixed = TRUE)
  expect_true(length(gregexpr('state = "samples_min"', src, fixed = TRUE)[[1]]) >= 2L)
  # 2. HVG bornées.
  expect_match(src, "TS_BULK_WGCNA_MIN_GENES", fixed = TRUE)
  expect_match(src, "TS_BULK_WGCNA_MAX_GENES", fixed = TRUE)
  expect_match(src, 'state = "too_few_genes"', fixed = TRUE)
  # 3. TOM borné (maxBlockSize jamais Inf).
  expect_match(src, "maxBlockSize = max_bs", fixed = TRUE)
  expect_match(src, "TS_BULK_WGCNA_MAX_BLOCKSIZE", fixed = TRUE)
  # 4. Threads : disable présent, enable ABSENT (contrôle sur le CODE,
  #    commentaires exclus — l'en-tête documente l'interdit).
  expect_match(src, "WGCNA::disableWGCNAThreads()", fixed = TRUE)
  code_nocomment <- paste(grep("^\\s*#",
                               readLines(file.path(ts_project_root(),
                                                   "R/bulk/bulk_wgcna.R"),
                                         warn = FALSE),
                               value = TRUE, invert = TRUE),
                          collapse = "\n")
  expect_false(grepl("enableWGCNAThreads|allowWGCNAThreads", code_nocomment),
               info = "enableWGCNAThreads/allowWGCNAThreads interdit")
  # 5. Quirk d'attache : WGCNA attaché PUIS détaché systématiquement.
  expect_match(src, 'detach("package:WGCNA", unload = TRUE)', fixed = TRUE)
  expect_match(src, "require(\"WGCNA\", quietly = TRUE, character.only = TRUE)",
               fixed = TRUE)
  # 6. Garde matrice transformée réutilisée (jamais dupliquée).
  expect_match(src, "bulk_assert_transformed_matrix(", fixed = TRUE)
  expect_match(src, 'e$state %||% "invalid_input"', fixed = TRUE)
  # 7. bicor avec repli cor documenté.
  expect_match(src, "WGCNA::bicor(", fixed = TRUE)
  expect_match(src, '"bicor"', fixed = TRUE)
})

# ── États d'erreur gelés ───────────────────────────────────────────────────
test_that("error states are frozen", {
  src <- paste(readLines(file.path(ts_project_root(), "R/bulk/bulk_wgcna.R")),
               collapse = "\n")
  states <- regmatches(src, gregexpr('state = "[a-z_]+"', src))[[1]]
  # raw_counts_rejected est HÉRITÉ du garde M1 via re-classage (e$state) —
  # le wrapper doit rester présent (garde anti-duplication).
  expect_setequal(unique(states),
                  c('state = "invalid_input"', 'state = "samples_min"',
                    'state = "too_few_genes"', 'state = "missing_dependency"',
                    'state = "compute_failed"', 'state = "no_traits"'))
  expect_match(src, 'e$state %||% "invalid_input"', fixed = TRUE)
  expect_match(src, "bulk_assert_transformed_matrix(", fixed = TRUE)
})

# ── Schémas des résultats gelés ────────────────────────────────────────────
test_that("power and modules result schemas are frozen", {
  set.seed(5)
  sim <- matrix(rnorm(2000 * 18), 2000, 18,
                dimnames = list(paste0("g", 1:2000), paste0("s", 1:18))) + 8
  skip_if_not_installed("WGCNA")
  pw <- bulk_wgcna_pick_power(sim, powers = c(1, 2, 4))
  expect_setequal(names(pw),
                  c("type", "status", "power_table", "chosen", "n_samples",
                    "n_genes_input", "n_genes_used", "warnings", "provenance",
                    "timestamp_utc"))
  expect_identical(pw$type, "bulk_wgcna_power")
  expect_setequal(names(pw$chosen),
                  c("power", "r2", "mean_k", "target_reached", "warning"))
  mods <- bulk_wgcna_build_modules(sim, power = 4, n_top = 2000)
  expect_setequal(names(mods),
                  c("type", "status", "power", "colors", "dendro",
                    "dendro_colors", "MEs", "n_modules", "module_sizes",
                    "n_samples", "n_genes_input", "n_genes_used", "warnings",
                    "provenance", "timestamp_utc"))
  expect_identical(mods$type, "bulk_wgcna_modules")
  ex <- build_wgcna_export(mods)
  expect_setequal(colnames(ex), c("gene", "module"))
  ex2 <- build_wgcna_export(mods, list(method = "bicor"))
  expect_setequal(colnames(ex2), c("gene", "module", "trait_cor_method"))
  expect_identical(ex2$trait_cor_method[1], "bicor")
})

# ── Seuils config gelés ────────────────────────────────────────────────────
test_that("config thresholds are declared and pinned", {
  expect_identical(TS_BULK_WGCNA_MIN_SAMPLES, 15L)
  expect_identical(TS_BULK_WGCNA_MIN_GENES, 2000L)
  expect_identical(TS_BULK_WGCNA_MAX_GENES, 5000L)
  expect_identical(TS_BULK_WGCNA_R2_MIN, 0.80)
  expect_identical(TS_BULK_WGCNA_MAX_BLOCKSIZE, 5000L)
  expect_identical(TS_BULK_WGCNA_MIN_MODULE, 30L)
})

# ── Pureté Shiny + autonomie ggplot2 ───────────────────────────────────────
test_that("R file stays pure and ggplot2-autonomous", {
  src <- paste(readLines(file.path(ts_project_root(), "R/bulk/bulk_wgcna.R")),
               collapse = "\n")
  expect_false(grepl("shiny::|reactive\\(|renderPlot|observeEvent\\(|showNotification\\(|moduleServer",
                     src))
  expect_match(src, "new_provenance_entry(", fixed = TRUE)
  expect_match(src, "ts_theme(", fixed = TRUE)
  code <- grep("^\\s*#",
               readLines(file.path(ts_project_root(), "R/bulk/bulk_wgcna.R"),
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
                                       "modules/bulk/mod_bulk_wgcna.R"),
                             encoding = "UTF-8"), collapse = "\n")
  expect_match(mod_src, "bulk_wgcna_pick_power(", fixed = TRUE)
  expect_match(mod_src, "bulk_wgcna_build_modules(", fixed = TRUE)
  expect_match(mod_src, "bulk_wgcna_prepare_traits(", fixed = TRUE)
  expect_match(mod_src, "bulk_wgcna_module_trait(", fixed = TRUE)
  expect_match(mod_src, "plot_wgcna_soft_threshold(", fixed = TRUE)
  expect_match(mod_src, "plot_wgcna_trait_heatmap(", fixed = TRUE)
  expect_match(mod_src, "build_wgcna_export(", fixed = TRUE)
  parent_src <- paste(readLines(file.path(ts_project_root(),
                                          "modules/bulk/mod_bulk.R"),
                                encoding = "UTF-8"), collapse = "\n")
  expect_match(parent_src, "panel_wgcna", fixed = TRUE)
  expect_match(parent_src, "tab_wgcna", fixed = TRUE)
  expect_match(parent_src, "mod_bulk_wgcna_server(", fixed = TRUE)
  app_src <- paste(readLines(file.path(ts_project_root(), "app.R"),
                             encoding = "UTF-8"), collapse = "\n")
  expect_match(app_src, 'source("R/bulk/bulk_wgcna.R")', fixed = TRUE)
  expect_match(app_src, 'source("modules/bulk/mod_bulk_wgcna.R")', fixed = TRUE)
})

# ── Synchronisation code <-> contrat documentaire ──────────────────────────
test_that("contract document is in sync with the frozen code", {
  doc_path <- file.path(ts_project_root(), "docs", "contracts", "BULK_WGCNA_CONTRACT.md")
  skip_if_not(file.exists(doc_path), .ts_contract_skip_msg(doc_path))
  doc <- paste(.ts_contract_readlines(doc_path, encoding = "UTF-8"), collapse = "\n")
  for (f in bulk_wgcna_public_api()) {
    expect_match(doc, paste0("`", f, "`"), fixed = TRUE,
                 info = paste("fonction absente du document :", f))
  }
  for (st in c("invalid_input", "raw_counts_rejected", "samples_min",
               "too_few_genes", "missing_dependency", "compute_failed",
               "no_traits")) {
    expect_match(doc, st, fixed = TRUE, info = paste("état absent du document :", st))
  }
  for (th in c("TS_BULK_WGCNA_MIN_SAMPLES", "TS_BULK_WGCNA_MIN_GENES",
               "TS_BULK_WGCNA_MAX_GENES", "TS_BULK_WGCNA_R2_MIN",
               "TS_BULK_WGCNA_MAX_BLOCKSIZE", "TS_BULK_WGCNA_MIN_MODULE")) {
    expect_match(doc, th, fixed = TRUE)
  }
  expect_match(doc, "bicor", fixed = TRUE)
  expect_match(doc, "bulk_wgcna_public_api", fixed = TRUE)
})

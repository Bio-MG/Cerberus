# =============================================================================
# test-bulk-batch-qc-contract-freeze.R — Bulk V2 M1 : gel du contrat
# docs/contracts/BULK_BATCH_QC_CONTRACT.md
# =============================================================================
# Verrouille : surface publique (noms + signatures), états d'erreur classés,
# seuils de l'heuristique anti-counts-bruts, sémantique design-check et
# variance-partition (méthodes, Residuals, seed/pla fond), seuils config,
# pureté Shiny du fichier R/, consommation par mod_bulk_filter.R, et
# synchronisation code <-> contrat documentaire.
# Toute évolution = code + ce test + doc SIMULTANÉMENT (règle contract-first).
# =============================================================================
source_project_file("R/core/io_helpers.R")
source_project_file("R/core/validation.R")
source_project_file("R/plotting/theme.R")      # ts_theme() — resolveur de theme partage
source_project_file("R/bulk/bulk_helpers.R")
source_project_file("R/bulk/bulk_batch_qc.R")

.bqc_top_level_names <- function(relpath) {
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
    bulk_batch_qc_public_api(),
    c("bulk_batch_qc_public_api", "bulk_assert_transformed_matrix",
      "bulk_batch_design_check", "bulk_variance_partition",
      "plot_bulk_varpart", "plot_bulk_batch_scree")
  )
  expected_api <- list(
    bulk_batch_qc_public_api     = list(),
    bulk_assert_transformed_matrix = list(mat = NULL, context = NULL),
    bulk_batch_design_check      = list(metadata = NULL, batch_col = NULL,
                                        condition_col = NULL),
    bulk_variance_partition      = list(vst_matrix = NULL, metadata = NULL,
                                        covariates = NULL, max_genes = NULL,
                                        seed = NULL, context = NULL),
    plot_bulk_varpart            = list(varpart = NULL, tr = NULL),
    plot_bulk_batch_scree        = list(vst_matrix = NULL, tr = NULL)
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
  # Helpers internes préfixés d'un point, jamais publics.
  defined <- .bqc_top_level_names("R/bulk/bulk_batch_qc.R")
  public <- setdiff(defined, grep("^\\.", defined, value = TRUE))
  expect_setequal(public, bulk_batch_qc_public_api())
  expect_true(all(c(".subset_variable_genes",
                    ".varpart_pure_r_fallback") %in% defined))
})

# ── Erreurs classées gelées ────────────────────────────────────────────────
test_that("error class and states are frozen", {
  set.seed(11)
  raw_m <- matrix(rpois(50 * 12, lambda = 300), 50, 12)
  err_raw <- tryCatch(bulk_assert_transformed_matrix(raw_m, "gel"),
                      error = function(e) e)
  expect_s3_class(err_raw, "bulk_batch_qc_error")
  expect_identical(err_raw$state, "raw_counts_rejected")
  expect_match(conditionMessage(err_raw), "VST")

  err_in <- tryCatch(bulk_assert_transformed_matrix(NULL), error = function(e) e)
  expect_s3_class(err_in, "bulk_batch_qc_error")
  expect_identical(err_in$state, "invalid_input")

  meta <- data.frame(row.names = paste0("s", 1:6),
                     batch = rep(c("b1", "b2"), 3),
                     condition = rep(c("A", "B"), each = 3))
  err_col <- tryCatch(bulk_batch_design_check(meta, "inexistant"),
                      error = function(e) e)
  expect_s3_class(err_col, "bulk_batch_qc_error")
  expect_identical(err_col$state, "invalid_input")

  # Seuls deux états existent (garde contre l'inflation silencieuse).
  src <- paste(readLines(file.path(ts_project_root(), "R/bulk/bulk_batch_qc.R")),
               collapse = "\n")
  expect_match(src, 'state = "invalid_input"', fixed = TRUE)
  expect_match(src, 'state = "raw_counts_rejected"', fixed = TRUE)
  states_found <- regmatches(src, gregexpr('state = "[a-z_]+"', src))[[1]]
  expect_setequal(unique(states_found),
                  c('state = "invalid_input"', 'state = "raw_counts_rejected"'))
})

# ── Heuristique anti-counts-bruts gelée ────────────────────────────────────
test_that("raw-counts heuristic thresholds are frozen", {
  src <- paste(readLines(file.path(ts_project_root(), "R/bulk/bulk_batch_qc.R")),
               collapse = "\n")
  expect_match(src, "0.95", fixed = TRUE)
  expect_match(src, "1e-8", fixed = TRUE)
  expect_match(src, ">= 30", fixed = TRUE)
})

# ── Sémantique design-check gelée ──────────────────────────────────────────
test_that("design-check return fields are frozen", {
  meta_bal <- data.frame(row.names = paste0("s", 1:12),
                         batch = rep(c("b1", "b2"), 6),
                         condition = rep(c("A", "B"), each = 6))
  chk <- bulk_batch_design_check(meta_bal, "batch", "condition")
  expect_setequal(names(chk),
                  c("cross_table", "n_samples", "n_batch_levels",
                    "n_condition_levels", "fully_collinear", "empty_cells",
                    "warning_messages"))
  expect_false(chk$fully_collinear)
  expect_identical(chk$empty_cells, 0L)

  meta_conf <- data.frame(row.names = paste0("s", 1:12),
                          batch = rep(c("b1", "b2"), each = 6),
                          condition = rep(c("A", "B"), each = 6))
  conf <- bulk_batch_design_check(meta_conf, "batch", "condition")
  expect_true(conf$fully_collinear)
})

# ── Sémantique variance-partition gelée ────────────────────────────────────
test_that("variance-partition methods and fields are frozen", {
  set.seed(1)
  m <- matrix(rnorm(20 * 12) + 10, 20, 12,
              dimnames = list(paste0("g", 1:20), paste0("s", 1:12)))
  meta <- data.frame(row.names = paste0("s", 1:12),
                     batch = rep(c("b1", "b2"), 6),
                     condition = rep(c("A", "B"), each = 6))
  vp <- bulk_variance_partition(m, meta, c("batch", "condition"))
  expect_setequal(names(vp),
                  c("var_part", "method", "formula", "n_genes_total",
                    "n_genes_used", "seed", "warnings", "timestamp_utc"))
  expect_true(vp$method %in% c("variancePartition::fitExtractVarPartModel",
                               "pur_lm_partial_r2"))
  expect_match(vp$formula, "^~ batch \\+ condition$")
  expect_identical(vp$seed, 11L)
  expect_true("Residuals" %in% colnames(vp$var_part))
  # Repli pur exposé et cohérent (somme par gène <= 1).
  fb <- .varpart_pure_r_fallback(m[1:8, ], meta, c("batch", "condition"))
  expect_identical(colnames(fb), c("batch", "condition", "Residuals"))
  expect_true(all(rowSums(fb) <= 1 + 1e-8))
})

# ── Seuils config gelés ────────────────────────────────────────────────────
test_that("config thresholds are declared and pinned", {
  expect_true(exists("TS_BULK_VARPART_MAX_GENES", inherits = TRUE))
  expect_identical(TS_BULK_VARPART_MAX_GENES, 2000L)
})

# ── Pureté Shiny + réutilisation (pas de duplication) ──────────────────────
test_that("R file stays pure (no Shiny reactivity, no reimplemented helpers)", {
  src <- paste(readLines(file.path(ts_project_root(), "R/bulk/bulk_batch_qc.R")),
               collapse = "\n")
  # Motifs de CODE réactif (les commentaires citent "réactivité Shiny" et les
  # modules — d'où l'ancrage sur des tokens de code, pas des mots libres).
  expect_false(grepl("shiny::|reactive\\(|renderPlot|observeEvent\\(|showNotification\\(|moduleServer",
                      src))
  # Réutilise les APIs existantes, jamais dupliquées.
  expect_match(src, "check_design_confounding(", fixed = TRUE)
  expect_match(src, "plot_scree_bulk(", fixed = TRUE)
})

# ── Consommation par le module ─────────────────────────────────────────────
test_that("mod_bulk_filter consumes the contract (QC Batch tab)", {
  mod_path <- file.path(ts_project_root(), "modules/bulk/mod_bulk_filter.R")
  expect_true(file.exists(mod_path))
  mod_src <- paste(readLines(mod_path), collapse = "\n")
  expect_match(mod_src, "bulk_batch_design_check(", fixed = TRUE)
  expect_match(mod_src, "bulk_variance_partition(", fixed = TRUE)
  expect_match(mod_src, "plot_bulk_varpart(", fixed = TRUE)
  expect_match(mod_src, "plot_bulk_batch_scree(", fixed = TRUE)
  expect_match(mod_src, "bulk_assert_transformed_matrix", fixed = TRUE)
  # Boîte d'alerte collinéarité.
  expect_match(mod_src, "alert-danger", fixed = TRUE)
})

# ── Autonomie ggplot2 : le fichier doit tourner SANS ggplot2 attache ───────
# Regression vecue : plot_bulk_varpart() appelait ggplot()/labs()/theme() nus.
# Ca marche dans l'app (global.R attache ggplot2) mais CASSE dans tout contexte
# externe — dont le harnais de test — avec "could not find function ggplot".
test_that("R/bulk/bulk_batch_qc.R prefixes every ggplot2 call", {
  path <- file.path(ts_project_root(), "R/bulk/bulk_batch_qc.R")
  code <- readLines(path, warn = FALSE, encoding = "UTF-8")
  code <- grep("^\\s*#", code, value = TRUE, invert = TRUE)   # on ignore les commentaires

  # appels nus = le mot-cle NON precede de "ggplot2::"
  bare <- "(^|[^.[:alnum:]_])(ggplot|aes|geom_[a-z_]+|labs|theme|theme_[a-z]+|scale_[a-z_]+|element_[a-z]+|facet_[a-z]+|coord_[a-z]+|guide_[a-z]+)[[:space:]]*\\("
  hits <- grep(bare, code, perl = TRUE, value = TRUE)
  # on ne garde que les lignes ou le motif n'est PAS prefixe par ggplot2::
  real <- hits[!grepl("ggplot2::", hits, fixed = TRUE)]
  expect(length(real) == 0L,
         paste("appels ggplot2 non prefixes (le fichier n'est pas autonome) :",
               paste(trimws(real), collapse = " | ")))
})

# ── Synchronisation code <-> contrat documentaire ──────────────────────────
test_that("contract document is in sync with the frozen code", {
  doc_path <- file.path(ts_project_root(), "docs", "contracts",
                        "BULK_BATCH_QC_CONTRACT.md")
  skip_if_not(file.exists(doc_path), .ts_contract_skip_msg(doc_path))
  doc <- paste(.ts_contract_readlines(doc_path, encoding = "UTF-8"), collapse = "\n")
  for (f in bulk_batch_qc_public_api()) {
    expect_match(doc, paste0("`", f, "`"), fixed = TRUE,
                 info = paste("fonction absente du document :", f))
  }
  for (st in c("invalid_input", "raw_counts_rejected")) {
    expect_match(doc, st, fixed = TRUE,
                 info = paste("état absent du document :", st))
  }
  for (fld in c("cross_table", "fully_collinear", "empty_cells",
                "warning_messages", "var_part", "Residuals")) {
    expect_match(doc, fld, fixed = TRUE,
                 info = paste("champ absent du document :", fld))
  }
  expect_match(doc, "TS_BULK_VARPART_MAX_GENES", fixed = TRUE)
  expect_match(doc, "bulk_batch_qc_public_api", fixed = TRUE)
})

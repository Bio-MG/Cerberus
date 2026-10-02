# =============================================================================
# test-bulk-batch-correction-contract-freeze.R — STAT-S1 : gel du contrat
# docs/contracts/BATCH_CORRECTION_CONTRACT.md
# =============================================================================
# Verrouille : surface publique (noms + signatures), états d'erreur classés,
# seuil config, symétrie de la garde counts-bruts avec bulk_batch_qc.R,
# pureté Shiny du fichier R/, import paresseux de sva, préfixage ggplot2 /
# patchwork, consommation par mod_bulk_filter.R, et synchronisation
# code <-> contrat documentaire.
# Toute évolution = code + ce test + doc SIMULTANÉMENT (règle contract-first).
# =============================================================================
source_project_file("R/core/io_helpers.R")
source_project_file("R/core/validation.R")
source_project_file("R/bulk/bulk_batch_qc.R")
source_project_file("R/bulk/batch_correction.R")

.bc_src <- function() {
  paste(readLines(file.path(ts_project_root(), "R/bulk/batch_correction.R")),
        collapse = "\n")
}

.bc_top_level_names <- function(relpath) {
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
    bulk_batch_correction_public_api(),
    c("bulk_batch_correction_public_api", "bulk_assert_raw_counts",
      "bulk_batch_correction_design", "bulk_batch_correction_label",
      "run_combat_seq", "plot_batch_correction_pca")
  )
  expected_api <- list(
    bulk_batch_correction_public_api = list(),
    bulk_assert_raw_counts           = list(mat = NULL, context = NULL,
                                            allow_non_integer = NULL),
    bulk_batch_correction_design     = list(metadata = NULL, batch_col = NULL,
                                            condition_col = NULL),
    bulk_batch_correction_label      = list(batch_col = NULL, condition_col = NULL,
                                            previous_normalization = NULL),
    run_combat_seq                   = list(counts_matrix = NULL, batch = NULL,
                                            group = NULL, context = NULL),
    plot_batch_correction_pca        = list(pca_before = NULL, pca_after = NULL,
                                            tr = NULL)
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
  # Piège de signature hérité (PLOT-S1/S2) : tr reste EN DERNIER.
  expect_identical(tail(names(formals(plot_batch_correction_pca)), 1L), "tr")
  # Helpers internes préfixés d'un point, jamais publics.
  defined <- .bc_top_level_names("R/bulk/batch_correction.R")
  public <- setdiff(defined, grep("^\\.", defined, value = TRUE))
  expect_setequal(public, bulk_batch_correction_public_api())
})

# ── Erreurs classées gelées : exactement 5 états ───────────────────────────
test_that("error class and the five states are frozen", {
  fx_counts <- matrix(as.integer(rpois(50 * 6, 300)), 50, 6,
                      dimnames = list(paste0("g", 1:50), paste0("s", 1:6)))

  err_null <- tryCatch(bulk_assert_raw_counts(NULL), error = function(e) e)
  expect_s3_class(err_null, "bulk_batch_correction_error")
  expect_identical(err_null$state, "invalid_input")

  err_vst <- tryCatch(bulk_assert_raw_counts(log2(fx_counts + 1) + 0.123),
                      error = function(e) e)
  expect_s3_class(err_vst, "bulk_batch_correction_error")
  expect_identical(err_vst$state, "not_raw_counts")

  err_deg <- tryCatch(run_combat_seq(fx_counts, rep("B1", 6)),
                      error = function(e) e)
  expect_s3_class(err_deg, "bulk_batch_correction_error")
  expect_identical(err_deg$state, "degenerate_batch")

  # Les 5 états, et eux seuls (garde contre l'inflation silencieuse).
  src <- .bc_src()
  states_found <- unique(regmatches(src, gregexpr('state = "[a-z_]+"', src))[[1]])
  expect_setequal(states_found,
                  c('state = "invalid_input"',
                    'state = "not_raw_counts"',
                    'state = "degenerate_batch"',
                    'state = "missing_dependency"',
                    'state = "compute_failed"'))
})

# ── Symétrie de la garde counts bruts <-> matrice transformée ──────────────
test_that("the raw-counts guard is the exact mirror of the transformed guard", {
  set.seed(3)
  raw <- matrix(as.integer(rpois(40 * 6, 250)), 40, 6)
  vst <- log2(raw + 1) + 0.123456
  # Ce que l'une accepte, l'autre le refuse — et réciproquement.
  expect_invisible(bulk_assert_raw_counts(raw))
  expect_error(bulk_assert_raw_counts(vst), class = "bulk_batch_correction_error")
  expect_invisible(bulk_assert_transformed_matrix(vst))
  expect_error(bulk_assert_transformed_matrix(raw), class = "bulk_batch_qc_error")
  # Même seuil de 95 % appliqué dans les deux sens.
  expect_match(.bc_src(), "0.95", fixed = TRUE)
  qc_src <- paste(readLines(file.path(ts_project_root(), "R/bulk/bulk_batch_qc.R")),
                  collapse = "\n")
  expect_match(qc_src, "0.95", fixed = TRUE)
})

# ── Seuil config gelé ──────────────────────────────────────────────────────
test_that("config threshold is declared and pinned", {
  expect_true(exists("TS_BULK_BATCH_MIN_SAMPLES_PER_BATCH", inherits = TRUE))
  expect_identical(TS_BULK_BATCH_MIN_SAMPLES_PER_BATCH, 2L)
  expect_match(.bc_src(), "TS_BULK_BATCH_MIN_SAMPLES_PER_BATCH", fixed = TRUE)
})

# ── Pureté Shiny + réutilisation (pas de duplication) ──────────────────────
test_that("R file stays pure and reuses existing APIs", {
  src <- .bc_src()
  expect_false(grepl("shiny::|reactive\\(|renderPlot|observeEvent\\(|showNotification\\(|moduleServer",
                     src))
  # Réutilise le contrôle de plan existant, ne le réimplémente pas.
  expect_match(src, "bulk_batch_design_check(", fixed = TRUE)
  # Import PARESSEUX de sva : jamais au source.
  expect_match(src, "requireNamespace(\"sva\"", fixed = TRUE)
  expect_false(grepl("library\\(sva\\)|require\\(sva\\)", src))
})

# ── Autonomie ggplot2 / patchwork : appels toujours préfixés ───────────────
test_that("R/bulk/batch_correction.R prefixes every ggplot2 and patchwork call", {
  code <- readLines(file.path(ts_project_root(), "R/bulk/batch_correction.R"),
                    warn = FALSE, encoding = "UTF-8")
  code <- grep("^\\s*#", code, value = TRUE, invert = TRUE)   # on ignore les commentaires
  bare <- "(^|[^.[:alnum:]_])(ggplot|aes|geom_[a-z_]+|labs|theme|theme_[a-z]+|scale_[a-z_]+|element_[a-z]+|wrap_plots|plot_annotation)[[:space:]]*\\("
  hits <- grep(bare, code, perl = TRUE, value = TRUE)
  real <- hits[!grepl("ggplot2::", hits, fixed = TRUE) &
               !grepl("patchwork::", hits, fixed = TRUE)]
  expect(length(real) == 0L,
         paste("appels non prefixes (le fichier n'est pas autonome) :",
               paste(trimws(real), collapse = " | ")))
})

# ── Consommation par le module ─────────────────────────────────────────────
test_that("mod_bulk_filter consumes the contract (QC Batch tab)", {
  mod_path <- file.path(ts_project_root(), "modules/bulk/mod_bulk_filter.R")
  expect_true(file.exists(mod_path))
  mod_src <- paste(readLines(mod_path), collapse = "\n")
  expect_match(mod_src, "bulk_batch_correction_design(", fixed = TRUE)
  expect_match(mod_src, "run_combat_seq(", fixed = TRUE)
  expect_match(mod_src, "bulk_batch_correction_label(", fixed = TRUE)
  expect_match(mod_src, "plot_batch_correction_pca(", fixed = TRUE)
  expect_match(mod_src, "bulk_update_provenance(", fixed = TRUE)
  # Idempotence : la copie pristine est locale au module.
  expect_match(mod_src, "bc_pristine", fixed = TRUE)
  # Le pipeline ne change que sur clic explicite.
  expect_match(mod_src, "input$run_batch_correction", fixed = TRUE)
})

# ── Déclaration de la dépendance dans global.R ─────────────────────────────
test_that("sva is declared as a Bioconductor dependency, not a required one", {
  g <- paste(readLines(file.path(ts_project_root(), "global.R")), collapse = "\n")
  expect_match(g, "bioc_packages", fixed = TRUE)
  expect_match(g, "\"sva\"", fixed = TRUE)
  # Volontairement PAS dans required_packages : l'app doit demarrer sans sva.
  req_block <- sub("(?s).*required_packages <- c\\((.*?)\\)\\n.*", "\\1", g, perl = TRUE)
  expect_false(grepl("\"sva\"", req_block, fixed = TRUE))
})

# ── Synchronisation code <-> contrat documentaire ──────────────────────────
test_that("contract document is in sync with the frozen code", {
  doc_path <- file.path(ts_project_root(), "docs", "contracts",
                        "BATCH_CORRECTION_CONTRACT.md")
  skip_if_not(file.exists(doc_path), .ts_contract_skip_msg(doc_path))
  doc <- paste(.ts_contract_readlines(doc_path, encoding = "UTF-8"), collapse = "\n")
  for (f in bulk_batch_correction_public_api()) {
    expect_match(doc, paste0("`", f, "`"), fixed = TRUE,
                 info = paste("fonction absente du document :", f))
  }
  for (st in c("invalid_input", "not_raw_counts", "degenerate_batch",
               "missing_dependency", "compute_failed")) {
    expect_match(doc, st, fixed = TRUE,
                 info = paste("état absent du document :", st))
  }
  expect_match(doc, "TS_BULK_BATCH_MIN_SAMPLES_PER_BATCH", fixed = TRUE)
  expect_match(doc, "bulk_batch_correction_public_api", fixed = TRUE)
  expect_match(doc, "bulk_batch_design_check(", fixed = TRUE)
  expect_match(doc, "plot_bulk_pca(", fixed = TRUE)
})

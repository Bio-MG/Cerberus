# =============================================================================
# test-bulk-gsva-contract-freeze.R — Bulk V2 M2 : gel du contrat
# docs/contracts/BULK_GSVA_CONTRACT.md (scores de voies par échantillon)
# =============================================================================
# Verrouille : surface publique (noms + signatures), états d'erreur classés,
# gardes de la mission (counts bruts refusés, BPPARAM jamais MulticoreParam
# sous Windows, BPPARAM passé à gsva() et non au constructeur, porte de
# recouvrement, bornes de taille), schéma du résultat, stockage contractuel
# bulk_obj$pathways$per_sample côté module, seuils config, pureté Shiny du
# fichier R/, autonomie ggplot2, et synchronisation code <-> contrat.
# Toute évolution = code + ce test + doc SIMULTANÉMENT (contract-first).
# =============================================================================
source_project_file("R/core/io_helpers.R")
source_project_file("R/core/validation.R")
source_project_file("R/core/provenance.R")
source_project_file("R/plotting/theme.R")
source_project_file("R/bulk/bulk_batch_qc.R")
source_project_file("R/bulk/bulk_gsva.R")

.bgsva_top_level_names <- function(relpath) {
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
    bulk_gsva_public_api(),
    c("bulk_gsva_public_api", "bulk_gsva_methods", "bulk_clean_gene_ids",
      "bulk_parse_gmt", "bulk_filter_gene_sets", "bulk_bpparam",
      "compute_pathway_scores", "build_pathway_scores_export",
      "plot_pathway_scores_pca", "plot_pathway_scores_heatmap")
  )
  expected_api <- list(
    bulk_gsva_public_api          = list(),
    bulk_gsva_methods             = list(),
    bulk_clean_gene_ids           = list(ids = NULL),
    bulk_parse_gmt                = list(path = NULL),
    bulk_filter_gene_sets         = list(gene_sets = NULL, matrix_genes = NULL,
                                         min_size = NULL, max_size = NULL,
                                         overlap_min = NULL),
    bulk_bpparam                  = list(workers = NULL),
    compute_pathway_scores        = list(expr_matrix = NULL, gene_sets = NULL,
                                         method = NULL, min_size = NULL,
                                         max_size = NULL, overlap_min = NULL,
                                         workers = NULL, analysis_id = NULL),
    build_pathway_scores_export   = list(result = NULL),
    plot_pathway_scores_pca       = list(result = NULL, metadata = NULL,
                                         color_by = NULL, tr = NULL),
    plot_pathway_scores_heatmap   = list(result = NULL, top_n = NULL,
                                         scale_rows = NULL, tr = NULL)
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
  defined <- .bgsva_top_level_names("R/bulk/bulk_gsva.R")
  public <- setdiff(defined, grep("^\\.", defined, value = TRUE))
  expect_setequal(public, bulk_gsva_public_api())
})

# ── Erreurs classées gelées ────────────────────────────────────────────────
test_that("error class and states are frozen", {
  set.seed(2)
  raw_m <- matrix(rpois(50 * 8, lambda = 300), 50, 8,
                  dimnames = list(paste0("G", 1:50), paste0("s", 1:8)))
  e_raw <- tryCatch(compute_pathway_scores(raw_m, list(S = paste0("G", 1:20))),
                    error = function(e) e)
  expect_s3_class(e_raw, "bulk_gsva_error")
  expect_identical(e_raw$state, "raw_counts_rejected")

  m <- matrix(rnorm(80 * 8) + 8, 80, 8,
              dimnames = list(paste0("GENE", 1:80), paste0("s", 1:8)))
  e_meth <- tryCatch(compute_pathway_scores(m, list(S = paste0("GENE", 1:20)),
                                             method = "nope"), error = function(e) e)
  expect_identical(e_meth$state, "invalid_input")

  e_sets <- tryCatch(compute_pathway_scores(m, list(SEUL = paste0("MISSING", 1:100))),
                     error = function(e) e)
  expect_identical(e_sets$state, "no_gene_sets")

  e_gmt <- tryCatch(bulk_parse_gmt("absent.gmt"), error = function(e) e)
  expect_s3_class(e_gmt, "bulk_gsva_error")
  expect_identical(e_gmt$state, "invalid_input")

  # Seuls les états gelés existent (garde anti-inflation silencieuse).
  # NB : raw_counts_rejected est HÉRITÉ du domaine QC batch via le
  # re-classage e$state (la garde bulk_assert_transformed_matrix est
  # réutilisée, jamais dupliquée) — le wrapper doit rester présent.
  src <- paste(readLines(file.path(ts_project_root(), "R/bulk/bulk_gsva.R")),
               collapse = "\n")
  states_found <- regmatches(src, gregexpr('state = "[a-z_]+"', src))[[1]]
  expect_setequal(unique(states_found),
                  c('state = "invalid_input"', 'state = "missing_dependency"',
                    'state = "no_gene_sets"', 'state = "compute_failed"'))
  expect_match(src, 'e$state %||% "invalid_input"', fixed = TRUE)
  expect_match(src, "bulk_assert_transformed_matrix(", fixed = TRUE)
})

# ── Garde mission : BPPARAM sur gsva(), jamais MulticoreParam sous Windows ─
test_that("BPPARAM discipline is frozen (call site + Windows serial)", {
  src <- paste(readLines(file.path(ts_project_root(), "R/bulk/bulk_gsva.R")),
               collapse = "\n")
  # BPPARAM est passé à GSVA::gsva() — jamais au constructeur (STATUS.md §2h).
  expect_match(src, "GSVA::gsva(par, BPPARAM = bulk_bpparam(workers), verbose = FALSE)",
               fixed = TRUE)
  expect_false(grepl("Param\\([^)]*BPPARAM", src),
               info = "BPPARAM passé à un constructeur")
  # Sous Windows : SerialParam, jamais MulticoreParam/fork.
  expect_match(src, ".Platform$OS.type == \"windows\"", fixed = TRUE)
  expect_match(src, "BiocParallel::SerialParam()", fixed = TRUE)
  bp <- bulk_bpparam(8)
  if (.Platform$OS.type == "windows") {
    expect_identical(class(bp)[1], "SerialParam")
  }
  # Plafond mémoire : jamais plus de TS_BULK_MAX_WORKERS.
  expect_match(src, "TS_BULK_MAX_WORKERS", fixed = TRUE)
})

# ── Porte de recouvrement + nettoyage d'identifiants gelés ─────────────────
test_that("overlap gate and Ensembl version stripping are frozen", {
  expect_match(
    paste(readLines(file.path(ts_project_root(), "R/bulk/bulk_gsva.R")),
          collapse = "\n"),
    'gsub("\\\\.[0-9]+$", ""', fixed = TRUE)
  m <- matrix(rnorm(30 * 6) + 8, 30, 6,
              dimnames = list(paste0("GENE", 1:30), paste0("s", 1:6)))
  filt <- bulk_filter_gene_sets(list(PARTIEL = paste0("GENE", 1:10),
                                     absent = paste0("ABSENT", 1:10)),
                                rownames(m), overlap_min = 0.20)
  expect_identical(filt$dropped$reason[filt$dropped$set == "absent"], "recouvrement")
  expect_setequal(names(filt$dropped),
                  c("set", "n_genes", "n_matched", "matched_fraction", "reason"))
})

# ── Schéma du résultat gelé ────────────────────────────────────────────────
test_that("result schema is frozen", {
  m <- matrix(rnorm(60 * 6) + 8, 60, 6,
              dimnames = list(paste0("GENE", 1:60), paste0("s", 1:6)))
  sets <- list(A = paste0("GENE", 1:20), B = paste0("GENE", 21:40))
  res <- compute_pathway_scores(m, sets, method = "ssgsea")
  expect_setequal(names(res),
                  c("type", "status", "analysis_id", "method", "params",
                    "scores", "gene_sets", "qc", "warnings", "provenance",
                    "timestamp_utc"))
  expect_identical(res$type, "bulk_pathway_scores")
  expect_setequal(names(res$qc),
                  c("n_input_sets", "n_used_sets", "dropped", "n_genes_input",
                    "n_genes_matched", "n_samples"))
  expect_identical(nrow(res$scores), 2L)
  expect_identical(ncol(res$scores), 6L)  # voies (lignes) x échantillons (colonnes)
  expect_true(all(is.finite(res$scores)))
  ex <- build_pathway_scores_export(res)
  expect_setequal(colnames(ex), c("pathway", "sample", "score", "method", "analysis_id"))
})

# ── Seuils config gelés ────────────────────────────────────────────────────
test_that("config thresholds are declared and pinned", {
  expect_true(exists("TS_BULK_GSVA_MIN_SIZE", inherits = TRUE))
  expect_identical(TS_BULK_GSVA_MIN_SIZE, 10L)
  expect_identical(TS_BULK_GSVA_MAX_SIZE, 500L)
  expect_identical(TS_BULK_GSVA_OVERLAP_MIN, 0.20)
})

# ── Pureté Shiny + réutilisation ───────────────────────────────────────────
test_that("R file stays pure (no Shiny reactivity, reused guards only)", {
  src <- paste(readLines(file.path(ts_project_root(), "R/bulk/bulk_gsva.R")),
               collapse = "\n")
  expect_false(grepl("shiny::|reactive\\(|renderPlot|observeEvent\\(|showNotification\\(|moduleServer",
                     src))
  # La garde anti-counts-bruts est RÉUTILISÉE du domaine QC batch (jamais dupliquée).
  expect_match(src, "bulk_assert_transformed_matrix(", fixed = TRUE)
  expect_match(src, "new_provenance_entry(", fixed = TRUE)
  expect_match(src, "ts_theme(", fixed = TRUE)
})

# ── Consommation par le module ─────────────────────────────────────────────
test_that("mod_bulk_pathways consumes the contract (Scores par échantillon)", {
  mod_path <- file.path(ts_project_root(), "modules/bulk/mod_bulk_pathways.R")
  expect_true(file.exists(mod_path))
  mod_src <- paste(readLines(mod_path, encoding = "UTF-8"), collapse = "\n")
  expect_match(mod_src, "compute_pathway_scores(", fixed = TRUE)
  expect_match(mod_src, "bulk_parse_gmt(", fixed = TRUE)
  expect_match(mod_src, "build_pathway_scores_export(", fixed = TRUE)
  expect_match(mod_src, "plot_pathway_scores_pca(", fixed = TRUE)
  expect_match(mod_src, "plot_pathway_scores_heatmap(", fixed = TRUE)
  # Stockage contractuel : bulk_obj$pathways$per_sample (mission Bulk V2 §M2).
  expect_match(mod_src, "bo$pathways$per_sample <- res$scores", fixed = TRUE)
  expect_match(mod_src, "run_scores", fixed = TRUE)
  expect_match(mod_src, "tab_scores", fixed = TRUE)
})

# ── Autonomie ggplot2 : le fichier doit tourner SANS ggplot2 attaché ───────
test_that("R/bulk/bulk_gsva.R prefixes every ggplot2 call", {
  path <- file.path(ts_project_root(), "R/bulk/bulk_gsva.R")
  code <- readLines(path, warn = FALSE, encoding = "UTF-8")
  code <- grep("^\\s*#", code, value = TRUE, invert = TRUE)
  bare <- "(^|[^.[:alnum:]_])(ggplot|aes|geom_[a-z_]+|labs|theme|theme_[a-z]+|scale_[a-z_]+|element_[a-z]+|facet_[a-z]+|coord_[a-z]+|guide_[a-z]+|annotate)[[:space:]]*\\("
  hits <- grep(bare, code, perl = TRUE, value = TRUE)
  real <- hits[!grepl("ggplot2::", hits, fixed = TRUE)]
  expect(length(real) == 0L,
         paste("appels ggplot2 non prefixes (le fichier n'est pas autonome) :",
               paste(trimws(real), collapse = " | ")))
})

# ── Synchronisation code <-> contrat documentaire ──────────────────────────
test_that("contract document is in sync with the frozen code", {
  doc_path <- file.path(ts_project_root(), "docs", "contracts", "BULK_GSVA_CONTRACT.md")
  skip_if_not(file.exists(doc_path), .ts_contract_skip_msg(doc_path))
  doc <- paste(.ts_contract_readlines(doc_path, encoding = "UTF-8"), collapse = "\n")
  for (f in bulk_gsva_public_api()) {
    expect_match(doc, paste0("`", f, "`"), fixed = TRUE,
                 info = paste("fonction absente du document :", f))
  }
  for (st in c("invalid_input", "raw_counts_rejected", "missing_dependency",
               "no_gene_sets", "compute_failed")) {
    expect_match(doc, st, fixed = TRUE, info = paste("état absent du document :", st))
  }
  for (fld in c("scores", "qc", "provenance", "n_genes_matched", "matched_fraction",
                "per_sample")) {
    expect_match(doc, fld, fixed = TRUE, info = paste("champ absent du document :", fld))
  }
  for (th in c("TS_BULK_GSVA_MIN_SIZE", "TS_BULK_GSVA_MAX_SIZE", "TS_BULK_GSVA_OVERLAP_MIN")) {
    expect_match(doc, th, fixed = TRUE)
  }
  expect_match(doc, "bulk_gsva_public_api", fixed = TRUE)
})

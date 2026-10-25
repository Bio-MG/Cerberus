# =============================================================================
# test-bulk-multi-pseudobulk-bridge.R — MD-3 : pont pseudobulk -> bulk_datasets
# =============================================================================
# Le pont lui-même est du câblage UI (mod_sc_pseudobulk.R) : ce fichier teste
# la COMPOSITION PURE qu'il effectue, à savoir :
#   1. construire le pipeline capturé via bulk_multi_capture_pipeline() sur une
#      liste partielle (contraste unique du run DE pseudobulk + seuils) ;
#   2. enregistrer l'entrée avec bulk_multi_register(producer = "pseudobulk")
#      — producteur réservé MD-3 dans le contrat BULK_MULTI_CONTRACT.md §2.8 ;
#   3. vérifier que l'entrée est ÉLIGIBLE et COMPARABLE par le moteur MD-2
#      (bulk_multi_entry_contrasts / bulk_multi_common_contrasts /
#      bulk_multi_run_comparison) aux côtés d'une entrée bulk classique.
# =============================================================================
source_project_file("tools/pkg_skeleton/R/io_helpers.R")
source_project_file("R/core/validation.R")
source_project_file("tools/pkg_skeleton/R/bulk_helpers.R")
source_project_file("tools/pkg_skeleton/R/bulk_multi.R")
source_project_file("tools/pkg_skeleton/R/bulk_multi_compare.R")
# D2 M4 (2026-10-25) : R/plotting vit desormais dans l'arbre du paquet
# (tools/pkg_skeleton/R/) — charge via load_project_pkg() (cf. helper-source.R).
load_project_pkg()

suppressPackageStartupMessages(library(ggplot2))

# ── Fixtures ─────────────────────────────────────────────────────────────────

# Sortie d'aggregate_pseudobulk_counts() + resolve_pseudobulk_condition() :
# comptages entiers gènes x pseudo-échantillons, metadata sample/condition/n_cells.
.pseudobulk_counts <- function(n_genes = 40, n_samples = 6, seed = 2) {
  set.seed(seed)
  counts <- matrix(round(abs(rnorm(n_genes * n_samples, 80, 25))), nrow = n_genes)
  dimnames(counts) <- list(paste0("GENE", seq_len(n_genes)),
                           paste0("pb_s", seq_len(n_samples)))
  storage.mode(counts) <- "integer"
  counts
}

.pseudobulk_metadata <- function(counts) {
  data.frame(row.names = colnames(counts),
             sample = colnames(counts),
             condition = rep(c("ctrl", "T2"), length.out = ncol(counts)),
             n_cells = rep(250L, ncol(counts)),
             stringsAsFactors = FALSE)
}

# Table DE pseudobulk normalisée (.normalize_de_cols) — mêmes colonnes que
# shared_rv$contrasts côté bulk.
.pseudobulk_de <- function(n_genes = 40, seed = 3) {
  set.seed(seed)
  data.frame(
    gene = paste0("GENE", seq_len(n_genes)),
    baseMean = abs(rnorm(n_genes, 80, 25)),
    log2FoldChange = c(rep(2.5, 5), rep(-2, 4), rnorm(n_genes - 9, 0, 0.3)),
    padj = c(rep(0.001, 9), runif(n_genes - 9, 0.2, 1)),
    stringsAsFactors = FALSE
  )
}

# Entrée bulk "classique" (producteur pipeline_save), même patron que
# test-bulk-multi-compare.R — partage le contraste T2_vs_ctrl.
.bulk_entry <- function(label, n_up = 5, n_down = 4, lfc = 1, padj = 0.05) {
  genes <- paste0("GENE", seq_len(40))
  res <- data.frame(
    gene = genes,
    baseMean = rep(80, 40),
    log2FoldChange = c(rep(2.5, n_up), rep(-2, n_down), rnorm(40 - n_up - n_down, 0, 0.3)),
    padj = c(rep(0.001, n_up + n_down), runif(40 - n_up - n_down, 0.2, 1)),
    stringsAsFactors = FALSE
  )
  list(
    label = label, producer = "pipeline_save",
    registered_at = Sys.time(), updated_at = Sys.time(),
    obj = list(
      counts = matrix(1:240, 40, 6, dimnames = list(genes, paste0("s", 1:6))),
      metadata = data.frame(row.names = paste0("s", 1:6),
                            condition = rep(c("ctrl", "T2"), 3)),
      project = label, type = "bulk", import_mode = "merged_matrix"
    ),
    pipeline = list(lfc_thresh = lfc, padj_thresh = padj,
                    contrasts = list(T2_vs_ctrl = res))
  )
}

# Réplique EXACTE de la composition du module (mod_sc_pseudobulk.R,
# observeEvent(input$pb_send)) — si le câblage diverge de ce patron, ce test
# doit échouer pour le signaler.
.bridge_register <- function(datasets, label, counts, metadata, de_result,
                             contrast_name = "T2_vs_ctrl",
                             lfc_thresh = 1, padj_thresh = 0.05) {
  pipeline <- bulk_multi_capture_pipeline(list(
    contrasts   = setNames(list(de_result), contrast_name),
    lfc_thresh  = lfc_thresh,
    padj_thresh = padj_thresh
  ))
  bulk_multi_register(
    datasets, label,
    obj = list(counts = counts, metadata = metadata),
    pipeline_state = pipeline,
    producer = "pseudobulk", overwrite = TRUE)
}

# ── Tests ────────────────────────────────────────────────────────────────────

test_that("l'enregistrement pseudobulk produit une entrée conforme au contrat", {
  counts <- .pseudobulk_counts()
  meta <- .pseudobulk_metadata(counts)
  de <- .pseudobulk_de()
  ds <- .bridge_register(NULL, "pseudobulk_T2_vs_ctrl", counts, meta, de)

  expect_setequal(names(ds), "pseudobulk_T2_vs_ctrl")
  e <- ds[["pseudobulk_T2_vs_ctrl"]]
  expect_identical(e$label, "pseudobulk_T2_vs_ctrl")
  expect_identical(e$producer, "pseudobulk")           # producteur réservé MD-3
  expect_s3_class(e$registered_at, "POSIXct")
  expect_s3_class(e$updated_at, "POSIXct")
  expect_identical(e$obj$counts, counts)               # copie, counts intacts
  expect_identical(e$obj$metadata, meta)
  # pipeline capturé = champs figés, contraste unique présent
  expect_setequal(names(e$pipeline), bulk_multi_pipeline_fields())
  expect_identical(names(e$pipeline$contrasts), "T2_vs_ctrl")
  expect_true(is.data.frame(e$pipeline$contrasts$T2_vs_ctrl))
  expect_identical(e$pipeline$lfc_thresh, 1)
  expect_identical(e$pipeline$padj_thresh, 0.05)
  # champs absents de la liste partielle -> NULL mais PRÉSENTS (noms conservés)
  expect_null(e$pipeline$filtered_counts)
  expect_null(e$pipeline$pathway_results)
})

test_that("le résumé du conteneur reflète l'entrée pseudobulk", {
  counts <- .pseudobulk_counts()
  ds <- .bridge_register(NULL, "pseudobulk_T2_vs_ctrl", counts,
                         .pseudobulk_metadata(counts), .pseudobulk_de())
  s <- bulk_multi_summary(ds)
  expect_identical(s$label, "pseudobulk_T2_vs_ctrl")
  expect_identical(s$producer, "pseudobulk")
  expect_identical(s$n_genes, 40L)
  expect_identical(s$n_samples, 6L)
  expect_false(s$has_filtered)
  expect_identical(s$n_contrasts, 1L)
  expect_false(s$has_pathways)
  expect_true(is.na(s$import_mode))   # pas d'import_mode sur un jeu pseudobulk
})

test_that("l'entrée pseudobulk est éligible et comparable par le moteur MD-2", {
  counts <- .pseudobulk_counts()
  ds <- .bridge_register(NULL, "pseudobulk_T2_vs_ctrl", counts,
                         .pseudobulk_metadata(counts), .pseudobulk_de())
  # second jeu, producteur classique, résultats DE différents mais même contraste
  ds <- bulk_multi_register(ds, "Jeu_Bulk_A",
                            obj = .bulk_entry("Jeu_Bulk_A")$obj,
                            pipeline_state = bulk_multi_capture_pipeline(
                              .bulk_entry("Jeu_Bulk_A")$pipeline),
                            producer = "pipeline_save")

  # éligibilité (critère du sélecteur mod_bulk_multi.R : >= 1 contraste)
  expect_true(length(bulk_multi_entry_contrasts(ds[["pseudobulk_T2_vs_ctrl"]])) > 0L)
  expect_true(length(bulk_multi_entry_contrasts(ds[["Jeu_Bulk_A"]])) > 0L)
  expect_setequal(bulk_multi_common_contrasts(ds), "T2_vs_ctrl")

  res <- bulk_multi_run_comparison(ds, "T2_vs_ctrl", lfc_thresh = 1,
                                   padj_thresh = 0.05)
  expect_setequal(res$datasets, c("pseudobulk_T2_vs_ctrl", "Jeu_Bulk_A"))
  expect_identical(res$contrast, "T2_vs_ctrl")
  expect_setequal(names(res$deg_sets),
                  c("pseudobulk_T2_vs_ctrl", "Jeu_Bulk_A"))
  expect_identical(nrow(res$concordance), 1L)
  expect_setequal(names(res$per_dataset),
                  c("label", "n_tested", "n_up", "n_down",
                    "stored_lfc_thresh", "stored_padj_thresh"))
  # seuils stockés du jeu pseudobulk = ceux passés à la capture
  expect_identical(
    res$per_dataset$stored_lfc_thresh[res$per_dataset$label == "pseudobulk_T2_vs_ctrl"],
    1)
})

test_that("overwrite explicite met à jour le producteur et conserve registered_at", {
  counts <- .pseudobulk_counts()
  meta <- .pseudobulk_metadata(counts)
  ds <- .bridge_register(NULL, "pseudobulk_T2_vs_ctrl", counts, meta, .pseudobulk_de())
  first <- ds[["pseudobulk_T2_vs_ctrl"]]$registered_at
  # re-push après un nouveau run DE (résultat différent) — même label
  ds <- .bridge_register(ds, "pseudobulk_T2_vs_ctrl", counts, meta,
                         .pseudobulk_de(seed = 99))
  e <- ds[["pseudobulk_T2_vs_ctrl"]]
  expect_identical(e$producer, "pseudobulk")
  expect_identical(e$registered_at, first)
  expect_true(e$updated_at >= first)
  expect_identical(names(e$pipeline$contrasts), "T2_vs_ctrl")
})

test_that("obj sans counts -> invalid_obj (aucun résultat DE à envoyer)", {
  counts <- .pseudobulk_counts()
  meta <- .pseudobulk_metadata(counts)
  pipeline <- bulk_multi_capture_pipeline(list(
    contrasts = list(T2_vs_ctrl = .pseudobulk_de()),
    lfc_thresh = 1, padj_thresh = 0.05))
  err <- tryCatch(
    bulk_multi_register(NULL, "pseudobulk_x", obj = list(metadata = meta),
                        pipeline_state = pipeline, producer = "pseudobulk"),
    error = function(e) e)
  expect_true(inherits(err, "bulk_multi_error"))
  expect_identical(err$state, "invalid_obj")
})

test_that("isolation par copie : muter pb$ après l'envoi ne change pas l'entrée", {
  counts <- .pseudobulk_counts()
  meta <- .pseudobulk_metadata(counts)
  de <- .pseudobulk_de()
  ds <- .bridge_register(NULL, "pseudobulk_T2_vs_ctrl", counts, meta, de)
  # le module continue de vivre (nouveau run DE -> pb$de_result remplacé)
  de$log2FoldChange[1] <- 999
  counts[1, 1] <- -5L
  e <- ds[["pseudobulk_T2_vs_ctrl"]]
  expect_false(identical(e$obj$counts[1, 1], -5L))
  expect_false(identical(e$pipeline$contrasts$T2_vs_ctrl$log2FoldChange[1], 999))
})

# =============================================================================
# test-bulk-multi-compare.R — MD-2 : comparaison multi-jeux bulk
# =============================================================================
# Couvre le contrat BULK_MULTI_CONTRACT.md §10 : contrastes communs, ensembles
# de gènes significatifs (réutilisation build_contrast_gene_sets), échelle
# volcano partagée (garde padj nul -> Inf), concordance de direction (Jaccard
# + % même direction, valeurs calculées à la main), run_comparison de bout en
# bout (structure plate §10.4, états d'erreur, zéro mutation des entrées).
# =============================================================================
source_project_file("tools/pkg_skeleton/R/io_helpers.R")
source_project_file("tools/pkg_skeleton/R/validation.R")
source_project_file("tools/pkg_skeleton/R/bulk_helpers.R")
source_project_file("tools/pkg_skeleton/R/bulk_multi.R")
source_project_file("tools/pkg_skeleton/R/bulk_multi_compare.R")
# D2 M4 (2026-10-25) : R/plotting vit desormais dans l'arbre du paquet
# (tools/pkg_skeleton/R/) — charge via load_project_pkg() (cf. helper-source.R).
load_project_pkg()

# plot_volcano_bulk (helper préexistant) appelle ggplot()/geom_*() nus —
# ggplot2 doit être ATTACHÉ (même convention que test-bulk-batch-qc.R).
suppressPackageStartupMessages(library(ggplot2))

.make_res_df <- function(up = c("G1", "G2"), down = c("G3", "G4"),
                         other = c("G5", "G6"), lfcv = 3,
                         padj_sig = 0.001, zero_padj = FALSE) {
  genes <- c(up, down, other)
  padj <- c(rep(padj_sig, length(up)), rep(padj_sig, length(down)),
            rep(0.9, length(other)))
  if (isTRUE(zero_padj)) padj[1L] <- 0
  data.frame(
    gene = genes,
    log2FoldChange = c(rep(lfcv, length(up)),
                       rep(-lfcv, length(down)),
                       rep(0.1, length(other))),
    padj = padj,
    stringsAsFactors = FALSE
  )
}

.make_entry <- function(label, res_list, lfc = 1, padj = 0.05) {
  list(
    label = label, producer = "pipeline_save",
    registered_at = Sys.time(), updated_at = Sys.time(),
    obj = list(
      counts = matrix(1:12, 3, 4,
                      dimnames = list(paste0("G", 1:3), paste0("s", 1:4))),
      metadata = data.frame(row.names = paste0("s", 1:4),
                            condition = c("A", "A", "B", "B")),
      project = label, type = "bulk", import_mode = "merged_matrix"
    ),
    pipeline = list(lfc_thresh = lfc, padj_thresh = padj, contrasts = res_list)
  )
}

.expect_state <- function(expr, state) {
  err <- tryCatch(force(expr), error = function(e) e)
  expect_true(inherits(err, "bulk_multi_error"),
              info = paste("erreur bulk_multi_error attendue, état :", state))
  expect_identical(err$state, state)
}

.entries <- list(
  Jeu_A = .make_entry("Jeu_A", list(T2_vs_T1 = .make_res_df(),
                                    T3_vs_T1 = .make_res_df())),
  Jeu_B = .make_entry("Jeu_B", list(T2_vs_T1 = .make_res_df(up = c("G1", "G2", "G7")),
                                    Autre = .make_res_df()), lfc = 1.5, padj = 0.1)
)

test_that("surface publique figée", {
  expect_setequal(
    bulk_multi_compare_public_api(),
    c("bulk_multi_compare_public_api", "bulk_multi_entry_contrasts",
      "bulk_multi_common_contrasts", "bulk_multi_deg_gene_sets",
      "bulk_multi_volcano_scales", "bulk_multi_volcano_panel",
      "bulk_multi_concordance", "bulk_multi_run_comparison")
  )
})

test_that("contrastes : par entrée, communs (intersection), cas vides", {
  expect_setequal(bulk_multi_entry_contrasts(.entries$Jeu_A),
                  c("T2_vs_T1", "T3_vs_T1"))
  expect_identical(bulk_multi_entry_contrasts(list(label = "vide")),
                   character(0))
  expect_setequal(bulk_multi_common_contrasts(.entries), "T2_vs_T1")
  expect_identical(bulk_multi_common_contrasts(list()), character(0))
  no_common <- list(Jeu_A = .entries$Jeu_A,
                    Jeu_C = .make_entry("Jeu_C", list(X_vs_Y = .make_res_df())))
  expect_identical(bulk_multi_common_contrasts(no_common), character(0))
})

test_that("ensembles de gènes : réutilisation build_contrast_gene_sets (2 directions)", {
  sets <- bulk_multi_deg_gene_sets(.entries, "T2_vs_T1", 1, 0.05)
  expect_setequal(names(sets), c("Jeu_A", "Jeu_B"))
  expect_setequal(sets$Jeu_A, c("G1", "G2", "G3", "G4"))
  expect_setequal(sets$Jeu_B, c("G1", "G2", "G3", "G4", "G7"))

  ud <- bulk_multi_deg_gene_sets(.entries, "T2_vs_T1", 1, 0.05,
                                 direction_aware = TRUE)
  expect_setequal(names(ud), c("Jeu_A (Up)", "Jeu_A (Down)",
                               "Jeu_B (Up)", "Jeu_B (Down)"))
  expect_setequal(ud[["Jeu_A (Up)"]], c("G1", "G2"))
  expect_setequal(ud[["Jeu_B (Up)"]], c("G1", "G2", "G7"))
})

test_that("ensembles : états insufficient_datasets / no_common_contrast / invalid_input", {
  one <- list(Jeu_A = .entries$Jeu_A)
  .expect_state(bulk_multi_deg_gene_sets(one, "T2_vs_T1", 1, 0.05),
                "insufficient_datasets")
  .expect_state(bulk_multi_deg_gene_sets(.entries, "Inconnu", 1, 0.05),
                "no_common_contrast")
  .expect_state(bulk_multi_deg_gene_sets(.entries, c("a", "b"), 1, 0.05),
                "invalid_input")
  .expect_state(bulk_multi_deg_gene_sets(.entries, "", 1, 0.05),
                "invalid_input")
})

test_that("échelle volcano partagée : limites plafonnées, padj nul exclu", {
  sc <- bulk_multi_volcano_scales(list(A = .make_res_df(), B = .make_res_df()))
  expect_identical(sc$x, c(-3, 3))
  expect_identical(sc$y, c(0, 3))   # -log10(0.001) = 3

  # padj nul -> -log10 = Inf : exclu du calcul (garde §10.2.3)
  sc0 <- bulk_multi_volcano_scales(list(A = .make_res_df(zero_padj = TRUE)))
  expect_identical(sc0$y, c(0, 3))

  sc1 <- bulk_multi_volcano_scales(list(A = .make_res_df(lfcv = 0.5)))
  expect_identical(sc1$x, c(-1, 1))  # plancher 1

  .expect_state(bulk_multi_volcano_scales(list()), "invalid_input")
  .expect_state(bulk_multi_volcano_scales(list(A = data.frame(x = 1))),
                "invalid_obj")
})

test_that("panneau volcano : titre = label du dataset + coord_cartesian partagée", {
  sc <- bulk_multi_volcano_scales(list(A = .make_res_df()))
  p <- bulk_multi_volcano_panel(.make_res_df(), "Jeu_A", 1, 0.05, sc)
  expect_s3_class(p, "ggplot")
  expect_identical(p$labels$title, "Jeu_A")
  expect_identical(class(p$coordinates)[1L], "CoordCartesian")

  .expect_state(bulk_multi_volcano_panel(data.frame(x = 1), "L", 1, 0.05, sc),
                "invalid_obj")
  .expect_state(bulk_multi_volcano_panel(.make_res_df(), "L", 1, 0.05, "pas une liste"),
                "invalid_input")
})

test_that("concordance : Jaccard et % même direction (valeurs calculées à la main)", {
  ud <- list(
    "A (Up)" = c("G1", "G2", "G5"), "A (Down)" = c("G3"),
    "B (Up)" = c("G1", "G2"),       "B (Down)" = c("G3", "G4"),
    "C (Up)" = c("G9"),             "C (Down)" = character(0)
  )
  cd <- bulk_multi_concordance(ud)
  expect_identical(nrow(cd), 3L)  # A-B, A-C, B-C
  ab <- cd[cd$dataset_a == "A" & cd$dataset_b == "B", ]
  expect_identical(ab$jaccard_up, 2 / 3)       # |{G1,G2}| / |{G1,G2,G5}|
  expect_identical(ab$jaccard_down, 1 / 2)     # |{G3}| / |{G3,G4}|
  expect_identical(ab$n_common_sig, 3L)        # {G1,G2,G3}
  expect_identical(ab$pct_same_direction, 100) # G1,G2 up/up ; G3 down/down

  ac <- cd[cd$dataset_a == "A" & cd$dataset_b == "C", ]
  expect_identical(ac$n_common_sig, 0L)
  expect_true(is.na(ac$pct_same_direction))

  .expect_state(bulk_multi_concordance(list(A = c("G1"), B = c("G1"))),
                "insufficient_datasets")
})

test_that("run_comparison : structure plate §10.4 complète, invisible, non mutable", {
  before <- .entries
  res <- bulk_multi_run_comparison(.entries, "T2_vs_T1", 1, 0.05)
  expect_invisible(bulk_multi_run_comparison(.entries, "T2_vs_T1", 1, 0.05))
  expect_setequal(
    names(res),
    c("datasets", "contrast", "lfc_thresh", "padj_thresh", "deg_sets",
      "up_down_sets", "intersection_dt", "concordance", "volcano_scales",
      "per_dataset", "ran_at")
  )
  expect_identical(res$contrast, "T2_vs_T1")
  expect_identical(res$datasets, c("Jeu_A", "Jeu_B"))
  expect_identical(res$volcano_scales$x, c(-3, 3))
  expect_s3_class(res$intersection_dt, "data.frame")
  expect_s3_class(res$per_dataset, "data.frame")
  expect_identical(res$per_dataset$stored_lfc_thresh, c(1, 1.5))
  expect_identical(res$per_dataset$stored_padj_thresh, c(0.05, 0.1))
  expect_identical(res$per_dataset$n_up, c(2L, 3L))
  expect_identical(res$per_dataset$n_down, c(2L, 2L))
  # Zéro mutation des entrées (garde §10.2.1)
  expect_identical(.entries, before)
})

test_that("run_comparison : états invalid_input / insufficient / no_significant", {
  .expect_state(bulk_multi_run_comparison(.entries, "T2_vs_T1", -1, 0.05),
                "invalid_input")
  .expect_state(bulk_multi_run_comparison(.entries, "T2_vs_T1", 1, 1.5),
                "invalid_input")
  .expect_state(bulk_multi_run_comparison(list(Jeu_A = .entries$Jeu_A),
                                          "T2_vs_T1", 1, 0.05),
                "insufficient_datasets")
  .expect_state(bulk_multi_run_comparison(.entries, "T2_vs_T1", 100, 0.05),
                "no_significant_genes")
})

# =============================================================================
# test-bulk-gsva.R — Scores de voies par échantillon (Bulk V2 M2)
# =============================================================================
# Couvre les gardes de la mission : matrice transformée exigée (counts bruts
# refusés), nettoyage d'identifiants, porte de recouvrement, tailles min/max,
# BPPARAM jamais MulticoreParam sous Windows, les 4 méthodes, export, plots.
# =============================================================================
source_project_file("tools/pkg_skeleton/R/validation.R")
source_project_file("tools/pkg_skeleton/R/provenance.R")
# D2 M4 (2026-10-25) : R/plotting vit desormais dans l'arbre du paquet
# (tools/pkg_skeleton/R/) — charge via load_project_pkg() (cf. helper-source.R).
load_project_pkg()
source_project_file("tools/pkg_skeleton/R/bulk_batch_qc.R")
source_project_file("tools/pkg_skeleton/R/bulk_gsva.R")

.vst_m2 <- function(genes = 80, samples = 8, seed = 1) {
  set.seed(seed)
  matrix(rnorm(genes * samples), genes, samples,
         dimnames = list(paste0("GENE", seq_len(genes)),
                         paste0("s", seq_len(samples)))) + 8
}

.sets_m2 <- function() {
  list(SET_A = paste0("GENE", 1:30),
       SET_B = paste0("GENE", 21:50),
       SET_BAD = paste0("MISSING", 1:15),
       SET_TINY = paste0("GENE", 1:5))
}

test_that("surface publique figée", {
  expect_setequal(
    bulk_gsva_public_api(),
    c("bulk_gsva_public_api", "bulk_gsva_methods", "bulk_clean_gene_ids",
      "bulk_parse_gmt", "bulk_filter_gene_sets", "bulk_bpparam",
      "compute_pathway_scores", "build_pathway_scores_export",
      "plot_pathway_scores_pca", "plot_pathway_scores_heatmap")
  )
  expect_setequal(bulk_gsva_methods(), c("gsva", "ssgsea", "plage", "zscore"))
})

test_that("nettoyage d'identifiants : suffixes Ensembl, idempotent", {
  expect_identical(
    bulk_clean_gene_ids(c("ENSG00000141510.17", "TP53", "ENSG00000012048.1", "GENE.2X")),
    c("ENSG00000141510", "TP53", "ENSG00000012048", "GENE.2X"))
  expect_identical(bulk_clean_gene_ids("TP53"), bulk_clean_gene_ids(bulk_clean_gene_ids("TP53")))
})

test_that("parseur GMT : format nominal, lignes incomplètes ignorées, doublons indexés", {
  gmt <- file.path(tempdir(), paste0("t", Sys.getpid(), ".gmt"))
  on.exit(unlink(gmt), add = TRUE)
  writeLines(c(
    "SET_X\tdescription\tGENE1\tGENE2\tGENE3",
    "BAD_ONLY2\tdescription\tGENE1",
    "EMPTY\tdescription",
    "\t\tGENE1\tGENE2",
    ""),
    gmt)
  gs <- bulk_parse_gmt(gmt)
  expect_identical(names(gs), c("SET_X", "BAD_ONLY2"))
  expect_identical(gs$SET_X, c("GENE1", "GENE2", "GENE3"))
  # doublons de noms rendus uniques
  writeLines(c("DUP\td\tG1", "DUP\td\tG2"), gmt)
  gs2 <- bulk_parse_gmt(gmt)
  expect_identical(anyDuplicated(names(gs2)), 0L)
  expect_identical(length(gs2), 2L)
  # erreur classée sur fichier vide/absent
  expect_error(bulk_parse_gmt(file.path(tempdir(), "absent.gmt")),
               class = "bulk_gsva_error")
  writeLines(c("", ""), gmt)
  e <- tryCatch(bulk_parse_gmt(gmt), error = function(e) e)
  expect_identical(e$state, "invalid_input")
})

test_that("porte de recouvrement + tailles : rejets tracés, sets retenus appariés", {
  m <- .vst_m2()
  filt <- bulk_filter_gene_sets(.sets_m2(), rownames(m), min_size = 10, max_size = 500,
                                overlap_min = 0.20)
  expect_setequal(names(filt$sets), c("SET_A", "SET_B"))
  expect_identical(nrow(filt$dropped), 2L)
  expect_setequal(filt$dropped$reason, c("recouvrement", "trop_petit"))
  expect_identical(filt$n_input_sets, 4L)
  expect_true(all(filt$dropped$matched_fraction >= 0))
  # les sets retenus ne contiennent que des gènes de la matrice
  expect_true(all(unlist(filt$sets) %in% rownames(m)))
  # bornes de taille
  f2 <- bulk_filter_gene_sets(list(SET = paste0("GENE", 1:60)), rownames(m),
                              min_size = 10, max_size = 50, overlap_min = 0)
  expect_identical(f2$dropped$reason, "trop_grand")
  # entrée vide -> erreur classée
  expect_error(bulk_filter_gene_sets(list(), rownames(m)), class = "bulk_gsva_error")
})

test_that("GARDE MISSION : counts bruts refusés avec erreur classée et message VST", {
  set.seed(2)
  counts <- matrix(rpois(50 * 8, lambda = 300), 50, 8,
                   dimnames = list(paste0("G", 1:50), paste0("s", 1:8)))
  err <- tryCatch(compute_pathway_scores(counts, .sets_m2(), method = "ssgsea"),
                  error = function(e) e)
  expect_s3_class(err, "bulk_gsva_error")
  expect_identical(err$state, "raw_counts_rejected")
  expect_match(conditionMessage(err), "VST")
})

test_that("GARDE MISSION : Windows ne produit JAMAIS MulticoreParam", {
  bp <- bulk_bpparam(8)
  if (.Platform$OS.type == "windows") {
    expect_false(grepl("Multicore", class(bp)[1]))
    expect_identical(class(bp)[1], "SerialParam")
  } else {
    expect_identical(class(bp)[1], "MulticoreParam")
    expect_lte(BiocParallel::bpnworkers(bp),
               if (exists("TS_BULK_MAX_WORKERS")) TS_BULK_MAX_WORKERS else 4L)
  }
  expect_identical(class(bulk_bpparam(1))[1], "SerialParam")  # workers <= 1 -> séquentiel
})

test_that("calcul : les 4 méthodes renvoient voies x échantillons, contrats respectés", {
  m <- .vst_m2()
  for (meth in bulk_gsva_methods()) {
    res <- compute_pathway_scores(m, .sets_m2(), method = meth)
    expect_identical(res$type, "bulk_pathway_scores")
    expect_identical(res$status, "valid")
    expect_identical(dim(res$scores), c(2L, ncol(m)))
    expect_identical(colnames(res$scores), colnames(m))
    expect_setequal(names(res$gene_sets), c("SET_A", "SET_B"))
    expect_match(res$provenance$method, "^GSVA::")
    expect_true(all(is.finite(res$scores)))
  }
})

test_that("GARDE §2k : charger GSVA via compute ne laisse AUCUNE option globale derrière", {
  # GSVA::.onLoad() pose Matrix.warnDeprecatedCoerce = 2 sans le restaurer —
  # un chargement direct dans CE domaine réintroduisait les 16 échecs Milo
  # (option = 2 -> dépréciations Matrix fatales ailleurs). Gelé par test.
  before <- getOption("Matrix.warnDeprecatedCoerce")
  compute_pathway_scores(.vst_m2(), .sets_m2(), method = "ssgsea")
  expect_identical(getOption("Matrix.warnDeprecatedCoerce"), before)
})

test_that("calcul : erreurs classées (méthode inconnue, aucun set, dépendance)", {
  m <- .vst_m2()
  e1 <- tryCatch(compute_pathway_scores(m, .sets_m2(), method = "nope"), error = function(e) e)
  expect_identical(e1$state, "invalid_input")
  e2 <- tryCatch(compute_pathway_scores(m, list(SEUL = paste0("MISSING", 1:100))),
                 error = function(e) e)
  expect_s3_class(e2, "bulk_gsva_error")
  expect_identical(e2$state, "no_gene_sets")
  e3 <- tryCatch(compute_pathway_scores(m[1, , drop = FALSE], .sets_m2()),
                 error = function(e) e)
  expect_s3_class(e3, "bulk_gsva_error")  # < 2 échantillons
})

test_that("export plat : une ligne par voie x échantillon, colonnes stables", {
  res <- compute_pathway_scores(.vst_m2(), .sets_m2(), method = "ssgsea")
  ex <- build_pathway_scores_export(res)
  expect_identical(nrow(ex), nrow(res$scores) * ncol(res$scores))
  expect_setequal(colnames(ex), c("pathway", "sample", "score", "method", "analysis_id"))
  expect_true(all(is.finite(ex$score)))
  # ordre déterministe
  expect_identical(ex, ex[order(ex$pathway, ex$sample), ])
  expect_error(build_pathway_scores_export(list()), class = "bulk_gsva_error")
})

test_that("plots : PCA ggplot, heatmap ComplexHeatmap ou repli ggplot", {
  res <- compute_pathway_scores(.vst_m2(80, 8, seed = 3), .sets_m2(), method = "ssgsea")
  meta <- data.frame(row.names = paste0("s", 1:8), group = rep(c("A", "B"), 4))
  expect_s3_class(plot_pathway_scores_pca(res, metadata = meta, color_by = "group"), "ggplot")
  expect_s3_class(plot_pathway_scores_pca(res), "ggplot")
  h <- plot_pathway_scores_heatmap(res, top_n = 2)
  expect_true(inherits(h, "ggplot") || inherits(h, "Heatmap"))
  # PCA impossible sous 3 échantillons
  r2 <- compute_pathway_scores(.vst_m2(80, 2, seed = 4), .sets_m2(), method = "ssgsea")
  expect_error(plot_pathway_scores_pca(r2), class = "bulk_gsva_error")
})

# ── B1-C (O21, 2026-10-28) : bulk_clean_gene_ids couvre aussi les composites ─
test_that("B1-C câblage: bulk_clean_gene_ids passe par strip_composite_gene_ids", {
  src <- paste(readLines(file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "bulk_gsva.R"), warn = FALSE), collapse = "\n")
  expect_true(grepl("out <- strip_composite_gene_ids(out)", src, fixed = TRUE))
  expect_true(is.function(strip_composite_gene_ids))
})

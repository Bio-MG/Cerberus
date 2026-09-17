# =============================================================================
# test-helpers_pathway.R — tests for helpers_pathway.R's plot/table builders
# =============================================================================
# Scope: plot_pathway_barplot(), plot_pathway_dotplot(), build_pathway_dt() —
# the only functions in this file with no hard dependency on clusterProfiler/
# org.Hs.eg.db/org.Mm.eg.db/ReactomePA (those need a live Bioconductor
# annotation stack + network access to bitr()/enrichGO() and are integration-
# tested manually against a real dataset, not unit-tested here).
#
# run_pathway_enrichment() / run_gsea_enrichment() themselves are NOT
# covered: every code path needs clusterProfiler + an org.*.eg.db package
# before it can even validate its inputs (see each function's own early
# requireNamespace() guards) — there's no meaningful pure-logic slice to
# isolate without those.
# =============================================================================

source_project_file("R/plotting/palettes.R")
source_project_file("R/plotting/datatable.R")   # PLOT-S3 : build_pathway_dt() -> ts_datatable()
source_project_file("R/core/pathway_helpers.R")
# ggplot2 is required for plot builders but not auto-loaded by helper-source.R
if (requireNamespace("ggplot2", quietly = TRUE)) suppressPackageStartupMessages(library(ggplot2))

.toy_pathway_df <- function(n = 5) {
  data.frame(
    ID          = paste0("GO:", sprintf("%07d", seq_len(n))),
    Description = paste("Pathway", LETTERS[seq_len(n)]),
    p.adjust    = seq(0.001, by = 0.01, length.out = n),
    Count       = rev(seq_len(n)) * 3,
    GeneRatio   = paste0(rev(seq_len(n)) * 3, "/100"),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# plot_pathway_barplot()
# ---------------------------------------------------------------------------
test_that("plot_pathway_barplot returns a ggplot object sized to top_n", {
  skip_if_not_installed("ggplot2")
  df <- .toy_pathway_df(10)
  p <- plot_pathway_barplot(df, db_label = "GOBP", top_n = 5)
  expect_s3_class(p, "gg")
  expect_equal(nrow(p$data), 5)
})

test_that("plot_pathway_barplot handles fewer rows than top_n gracefully", {
  skip_if_not_installed("ggplot2")
  df <- .toy_pathway_df(3)
  p <- plot_pathway_barplot(df, db_label = "GOBP", top_n = 15)
  expect_s3_class(p, "gg")
  expect_equal(nrow(p$data), 3)
})

# ---------------------------------------------------------------------------
# plot_pathway_dotplot()
# ---------------------------------------------------------------------------
test_that("plot_pathway_dotplot parses GeneRatio strings ('k/n') into a numeric ratio", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(ID = "GO:1", Description = "Pathway A", p.adjust = 0.01,
                   Count = 3, GeneRatio = "3/50", stringsAsFactors = FALSE)
  p <- plot_pathway_dotplot(df, db_label = "GOBP", top_n = 5)
  expect_equal(p$data$GeneRatioNum, 3 / 50)
})

test_that("plot_pathway_dotplot falls back to Count/max(Count) when GeneRatio is absent", {
  skip_if_not_installed("ggplot2")
  df <- data.frame(ID = c("GO:1","GO:2"), Description = c("A","B"),
                   p.adjust = c(0.01, 0.02), Count = c(5, 10), stringsAsFactors = FALSE)
  p <- plot_pathway_dotplot(df, db_label = "GOBP", top_n = 5)
  expect_equal(p$data$GeneRatioNum, c(0.5, 1))
})

# ---------------------------------------------------------------------------
# build_pathway_dt()
# ---------------------------------------------------------------------------
test_that("build_pathway_dt renames the 5 standard columns to their French labels", {
  skip_if_not_installed("DT")
  df <- .toy_pathway_df(4)
  dt <- build_pathway_dt(df)
  expect_s3_class(dt, "datatables")
  # rownames = FALSE is passed explicitly -> no leading row-number column.
  expect_identical(colnames(dt$x$data), c("ID", "Description", "P-adj", "Nb G\u00e8nes", "Ratio"))
})

test_that("build_pathway_dt only keeps columns actually present in the input (defensive intersect)", {
  skip_if_not_installed("DT")
  # NOTE (finding, not asserted as a bug here): build_pathway_dt() renames
  # via a POSITIONAL vector after intersect()-ing the expected column names.
  # This is only correct when any missing columns are TRAILING in the
  # canonical order (ID, Description, p.adjust, Count, GeneRatio) — which
  # matches how this file's own callers (run_pathway_enrichment(),
  # run_gsea_enrichment()) always populate all 5. A future caller supplying
  # a partial data.frame with a MIDDLE column missing (e.g. Count without
  # GeneRatio) would get its trailing column mislabeled. Covered here only
  # for the trailing-omission case, which is what actually occurs today.
  df <- .toy_pathway_df(3)[, c("ID", "Description", "p.adjust")]  # trailing cols omitted
  dt <- build_pathway_dt(df)
  expect_identical(colnames(dt$x$data), c("ID", "Description", "P-adj"))
})

# ---------------------------------------------------------------------------
# plot_pathway_network() — STAT-S2
# ---------------------------------------------------------------------------
# Unlike run_pathway_enrichment()/run_gsea_enrichment(), the network plot only
# needs the RAW enrichResult object + enrichplot — both offline. The happy path
# below therefore builds a REAL enrichGO result (org.Hs.eg.db, no network
# access) instead of a mock, which also regression-covers the additive
# `enrich_obj` attribute contract introduced by STAT-S2.

.enrich_obj_fixture <- function() {
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("org.Hs.eg.db")
  suppressPackageStartupMessages({
    library(clusterProfiler)
    library(org.Hs.eg.db)
    library(AnnotationDbi)
  })
  # Gene set built straight from the annotation (cell cycle GO:0007049) —
  # offline, no DOSE dataset lazy-load quirks, and guaranteed to enrich its own term.
  all_ids <- AnnotationDbi::keys(org.Hs.eg.db, "ENTREZID")
  cc <- intersect(AnnotationDbi::get("GO:0007049", org.Hs.egGO2ALLEGS), all_ids)
  ego <- clusterProfiler::enrichGO(
    gene = cc, universe = all_ids, OrgDb = org.Hs.eg.db,
    keyType = "ENTREZID", ont = "BP",
    pvalueCutoff = 0.05, qvalueCutoff = 0.2, readable = TRUE)
  skip_if(is.null(ego) || nrow(as.data.frame(ego)) < 5,
          "enrichGO n'a renvoyé aucune voie enrichie")
  ego
}

test_that("plot_pathway_network refuses a result without the raw enrich_obj attribute", {
  expect_error(
    plot_pathway_network(.toy_pathway_df(), top_n = 10),
    "enrich_obj", fixed = TRUE
  )
})

test_that("plot_pathway_network validates mode and top_n", {
  expect_error(plot_pathway_network(.toy_pathway_df(), top_n = 1), "top_n")
  expect_error(plot_pathway_network(.toy_pathway_df(), top_n = NA), "top_n")
  expect_error(plot_pathway_network(.toy_pathway_df(), mode = "nope"), "'arg'")
})

test_that("plot_pathway_network refuses fewer than two enriched pathways", {
  skip_if_not_installed("enrichplot")
  ego <- .enrich_obj_fixture()
  ego1 <- ego[1]  # single-pathway enrichResult (S4 subset)
  df <- as.data.frame(ego1)
  attr(df, "enrich_obj") <- ego1
  expect_error(plot_pathway_network(df, top_n = 10), "Au moins deux voies", fixed = TRUE)
})

test_that("plot_pathway_network builds emap and cnet networks from a real enrichResult", {
  skip_if_not_installed("enrichplot")
  ego <- .enrich_obj_fixture()
  df <- as.data.frame(ego)
  attr(df, "enrich_obj") <- ego
  p_emap <- plot_pathway_network(df, db_label = "GOBP", top_n = 10, mode = "emap")
  expect_s3_class(p_emap, "ggplot")
  p_cnet <- plot_pathway_network(df, db_label = "GOBP", top_n = 10, mode = "cnet")
  expect_s3_class(p_cnet, "ggplot")
})

# ---------------------------------------------------------------------------
# C10 — les erreurs de pathway_helpers.R portent la classe `pathway_error`
# ---------------------------------------------------------------------------
# Dette de conventions, 6ᵉ incrément. Le fichier compte 22 `stop()` : 4
# portaient DEJA `call. = FALSE` (forme que C10 exempte — leur unification sur
# la classe est une decision OUVERTE, cf. docs/STATUS.md) et 18 etaient non
# classes. Ces 18 passent a `stop(errorCondition(<msg>, class = "pathway_error"))`.
#
# ATTENTION — les sites dont le message tient en PLUSIEURS arguments sont
# enveloppes dans `paste0()` : `errorCondition()` ne concatene pas ses `...`
# (il en fait des CHAMPS de la condition), il TRONQUE au premier argument.
# Defaut mesure en §2bn, garde desormais par la regle C16.
#
# Deux niveaux de preuve, aucun ne suffisant seul :
#   1. SOURCE    — le fichier ne contribue plus AUCUN signalement C10. Ce verrou
#      couvre les 18 sites, y compris les 9 NON joignables (paquets absents).
#   2. EXECUTION — les 9 sites joignables levent VRAIMENT une condition classee.
#      Un `class=` pose au mauvais endroit resterait invisible au niveau source :
#      c'est exactement ce que §2bn a montre.
source_project_file("tools/check_conventions.R")

#' Un data.frame DE minimal, valide pour les controles en amont de GSEA.
.pathway_toy_de <- function(n = 20, genes = paste0("GENE", seq_len(n))) {
  data.frame(
    gene             = genes,
    log2FoldChange   = rep(c(1, -1), length.out = n),
    pvalue           = rep(c(0.001, 0.02), length.out = n),
    stringsAsFactors = FALSE
  )
}

#' Capture l'erreur levee par `expr` ; NULL si `expr` retourne normalement.
.pathway_error_of <- function(expr) {
  tryCatch({
    suppressMessages(suppressWarnings(force(expr)))
    NULL
  }, error = function(e) e)
}

# Les messages sont cites par des fragments ASCII (les accents du source
# dependraient de l'encodage de lecture) et ceux places TARD dans le message
# servent aussi de preuve de NON-TRONCATURE.

test_that("C10 : pathway_helpers.R ne contribue plus aucun signalement", {
  path <- file.path(ts_project_root(), "R/core/pathway_helpers.R")
  .REPORT$warns <- list()
  check_c10_error_style(path)
  expect_length(.REPORT$warns, 0L)
})

test_that("C10 : les erreurs joignables de pathway_helpers.R portent pathway_error", {
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("org.Hs.eg.db")

  # 1. organisme non supporte — run_pathway_enrichment()
  e <- .pathway_error_of(run_pathway_enrichment(genes = "TP53", organism = "rat"))
  expect_s3_class(e, "pathway_error")
  expect_match(conditionMessage(e), "Organisme non support", fixed = TRUE)

  # 2. aucun gene valide apres nettoyage
  e <- .pathway_error_of(run_pathway_enrichment(genes = c("", NA, "   "), organism = "human"))
  expect_s3_class(e, "pathway_error")
  expect_match(conditionMessage(e), "valide fourni", fixed = TRUE)

  # 3. aucun gene converti en Entrez ID
  e <- .pathway_error_of(run_pathway_enrichment(genes = c("ZZZNOTAGENE1", "ZZZNOTAGENE2"),
                                                organism = "human"))
  expect_s3_class(e, "pathway_error")
  expect_match(conditionMessage(e), "Entrez ID", fixed = TRUE)

  # 4. base de donnees non supportee (ORA) — exige une conversion reussie
  e <- .pathway_error_of(run_pathway_enrichment(genes = c("TP53", "BRCA1", "EGFR"),
                                                organism = "human", database = "NOPE"))
  expect_s3_class(e, "pathway_error")
  expect_match(conditionMessage(e), "Base de donn", fixed = TRUE)

  # 5. organisme non supporte — run_gsea_enrichment()
  e <- .pathway_error_of(run_gsea_enrichment(de_results = .pathway_toy_de(), organism = "rat"))
  expect_s3_class(e, "pathway_error")
  expect_match(conditionMessage(e), "Organisme non support", fixed = TRUE)

  # 6. colonnes manquantes — message MULTI-ARGUMENTS -> paste0()
  e <- .pathway_error_of(run_gsea_enrichment(de_results = data.frame(x = 1:20), organism = "human"))
  expect_s3_class(e, "pathway_error")
  expect_match(conditionMessage(e), "Colonnes manquantes", fixed = TRUE)
  expect_match(conditionMessage(e), "log2FoldChange", fixed = TRUE)  # non tronque

  # 7. trop peu de genes — message MULTI-ARGUMENTS -> paste0()
  e <- .pathway_error_of(run_gsea_enrichment(de_results = .pathway_toy_de(3), organism = "human"))
  expect_s3_class(e, "pathway_error")
  expect_match(conditionMessage(e), "valides (3)", fixed = TRUE)     # non tronque

  # 8. conversion des genes impossible — message MULTI-ARGUMENTS -> paste0()
  e <- .pathway_error_of(run_gsea_enrichment(
    de_results = .pathway_toy_de(20, paste0("ZZZNOTAGENE", 1:20)), organism = "human"))
  expect_s3_class(e, "pathway_error")
  expect_match(conditionMessage(e), "conversion des g", fixed = TRUE)
  expect_match(conditionMessage(e), "valid keys", fixed = TRUE)      # non tronque

  # 9. base de donnees non supportee (GSEA)
  e <- .pathway_error_of(run_gsea_enrichment(
    de_results = .pathway_toy_de(20, c("TP53", "BRCA1", "EGFR", "MYC", "KRAS", "PTEN", "RB1",
                                       "APC", "VHL", "BRAF", "PIK3CA", "AKT1", "MTOR", "CDKN2A",
                                       "SMAD4", "NOTCH1", "CTNNB1", "JAK2", "STAT3", "TGFBR2")),
    organism = "human", database = "NOPE"))
  expect_s3_class(e, "pathway_error")
  expect_match(conditionMessage(e), "Base de donn", fixed = TRUE)
})

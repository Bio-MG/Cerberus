# =============================================================================
# test-pathway-network-interactive.R — STAT-S2 V2 (réseau interactif)
# =============================================================================
# Contrat : docs/contracts/PATHWAY_NETWORK_CONTRACT.md (contract-first :
# code + test de gel + document SIMULTANÉS).
#
# Portée : build_pathway_network_data() / pathway_network_layout() /
# plot_pathway_network_interactive() — R pur (le constructeur n'appelle PAS
# enrichplot : similarité = Jaccard ordinaire), testable sans Bioconductor,
# même philosophie que test-pathway-helpers.R pour la V1 statique.
# =============================================================================

source_project_file("R/core/pathway_helpers.R")

# --- fixture ----------------------------------------------------------------

.toy_enrich_df <- function() {
  # 4 termes, jeux de gènes choisis pour des Jaccards CONNUS :
  #   J(T1,T2) = 3/5 = 0.6  -> arête (>= TS_PATHWAY_NET_MIN_SIM)
  #   J(T1,T3) = 0/7 = 0    -> pas d'arête
  #   J(T2,T3) = 0/7 = 0    -> pas d'arête
  #   J(T1,T4) = 4/4 = 1.0  -> arête ; J(T2,T4) = 3/5 = 0.6 -> arête
  d <- data.frame(
    ID          = c("T1", "T2", "T3", "T4"),
    Description = c("Voie un", "Voie deux", "Voie trois", "Voie quatre"),
    Count       = c(4L, 4L, 3L, 4L),
    p.adjust    = c(0.001, 0.002, 0.003, 0.004),
    GeneRatio   = c("4/100", "4/100", "3/100", "4/100"),
    geneID      = c("g1/g2/g3/g4", "g1/g2/g3/g5", "g9/g10/g11",
                    "g1/g2/g3/g4"),
    stringsAsFactors = FALSE
  )
  .network_df(d)
}
.network_df <- function(enrich) {
  df <- enrich[, c("ID", "Description", "p.adjust", "Count", "GeneRatio")]
  attr(df, "enrich_obj") <- enrich
  df
}

.tnet_error_class <- function(expr) {
  err <- tryCatch(expr, error = function(e) e)
  expect_s3_class(err, "error")
  expect_true("pathway_error" %in% class(err),
              info = paste("classe inattendue :", paste(class(err), collapse = ",")))
  invisible(err)
}

# --- 1. structure figée ------------------------------------------------------

test_that("le constructeur rend la structure figée du contrat", {
  net <- build_pathway_network_data(.toy_enrich_df(), top_n = 30, mode = "emap")
  expect_s3_class(net, "pathway_network_data")
  expect_identical(names(net), c("nodes", "edges", "meta"))
  expect_identical(names(net$nodes),
                   c("id", "label", "kind", "count", "p.adjust"))
  expect_identical(names(net$edges), c("from", "to", "weight"))
  expect_identical(names(net$meta),
                   c("mode", "top_n", "n_edges", "min_similarity",
                     "gene_separator"))
  expect_true(all(net$nodes$kind == "term"))          # emap : que des termes
  expect_identical(net$meta$gene_separator, "/")
})

# --- 2. emap : arêtes de Jaccard ---------------------------------------------

test_that("emap : les arêtes suivent le Jaccard et le seuil figé", {
  net <- build_pathway_network_data(.toy_enrich_df(), top_n = 30, mode = "emap")
  expect_identical(net$meta$min_similarity, TS_PATHWAY_NET_MIN_SIM)
  expect_identical(net$meta$n_edges, 3L)
  expect_setequal(
    paste(net$edges$from, net$edges$to),
    c("T1 T2", "T1 T4", "T2 T4")
  )
  # from < to : arêtes non orientées normalisées
  expect_true(all(net$edges$from < net$edges$to))
  w <- setNames(net$edges$weight, paste(net$edges$from, net$edges$to))
  expect_equal(w[["T1 T2"]], 0.6)
  expect_equal(w[["T1 T4"]], 1)
  expect_equal(w[["T2 T4"]], 0.6)
})

# --- 3. cnet : bipartite termes ↔ gènes --------------------------------------

test_that("cnet : nœuds gènes ajoutés, arêtes strictement bipartites", {
  net <- build_pathway_network_data(.toy_enrich_df(), top_n = 30, mode = "cnet")
  expect_true("g1" %in% net$nodes$id && all(net$nodes$kind %in% c("term", "gene")))
  genes <- net$nodes[net$nodes$kind == "gene", ]
  expect_setequal(genes$id, c("g1", "g2", "g3", "g4", "g5", "g9", "g10", "g11"))
  expect_true(all(is.na(genes$count)) && all(is.na(genes$p.adjust)))
  kind_of <- setNames(net$nodes$kind, net$nodes$id)
  expect_true(all(kind_of[net$edges$from] == "term"))
  expect_true(all(kind_of[net$edges$to] == "gene"))
  expect_true(all(net$edges$weight == 1))
})

# --- 4. plafond --------------------------------------------------------------

test_that("top_n est plafonné à TS_PATHWAY_NETWORK_MAX_TERMS sans refus", {
  n <- 350L
  d <- data.frame(
    ID = paste0("T", seq_len(n)), Description = paste("V", seq_len(n)),
    Count = 1L, p.adjust = seq_len(n) / n, GeneRatio = "1/100",
    geneID = "g1", stringsAsFactors = FALSE
  )
  net <- build_pathway_network_data(.network_df(d), top_n = 10000, mode = "emap")
  expect_identical(net$meta$top_n, TS_PATHWAY_NETWORK_MAX_TERMS)
  expect_identical(nrow(net$nodes), TS_PATHWAY_NETWORK_MAX_TERMS)
})

# --- 5. refus du contrat (classe pathway_error) -------------------------------

test_that("les refus sont des pathway_error avec les ancres du contrat", {
  # pas d'attribut enrich_obj
  bare <- data.frame(ID = "T1", p.adjust = 0.1)
  .tnet_error_class(build_pathway_network_data(bare))
  # top_n invalide
  .tnet_error_class(build_pathway_network_data(.toy_enrich_df(), top_n = 1))
  .tnet_error_class(build_pathway_network_data(.toy_enrich_df(), top_n = "x"))
  # moins de deux voies
  one <- .toy_enrich_df()
  attr(one, "enrich_obj") <- attr(one, "enrich_obj")[1, ]
  .tnet_error_class(build_pathway_network_data(one))
  # colonne manquante
  bad <- .toy_enrich_df()
  eb <- attr(bad, "enrich_obj"); eb$geneID <- NULL; attr(bad, "enrich_obj") <- eb
  err <- .tnet_error_class(build_pathway_network_data(bad))
  expect_match(conditionMessage(err), "Colonnes manquantes", fixed = TRUE)
  # rendu alimenté par un objet étranger
  .tnet_error_class(plot_pathway_network_interactive(list(a = 1)))
})

# --- 6. déterminisme de la disposition ----------------------------------------

test_that("la disposition est DÉTERMINISTE (graine figée)", {
  net <- build_pathway_network_data(.toy_enrich_df(), top_n = 30, mode = "emap")
  l1 <- pathway_network_layout(net)
  l2 <- pathway_network_layout(net)
  expect_identical(l1$x, l2$x)
  expect_identical(l1$y, l2$y)
  expect_true(all(is.finite(l1$x)) && all(is.finite(l1$y)))
})

# --- 6bis. helpers purs d'arêtes (couverture réelle, pas seulement via le constructeur)

test_that("les helpers purs emap/cnet produisent les arêtes attendues", {
  terms <- c("T1", "T2", "T3", "T4")
  sets <- list(c("g1", "g2", "g3", "g4"), c("g1", "g2", "g3", "g5"),
               c("g9", "g10", "g11"), c("g1", "g2", "g3", "g4"))
  e <- .pathway_emap_edges(terms, sets)
  expect_identical(nrow(e), 3L)
  expect_setequal(paste(e$from, e$to), c("T1 T2", "T1 T4", "T2 T4"))
  # cas dégénéré : un seul terme -> arête vide, pas d'erreur
  expect_identical(nrow(.pathway_emap_edges("T1", list(c("g1", "g2")))), 0L)
  cn <- .pathway_cnet_edges(c("T1", "T2"), list(c("g1", "g2"), c("g2", "g3")))
  expect_identical(nrow(cn), 4L)
  expect_setequal(cn$to, c("g1", "g2", "g2", "g3"))
  expect_true(all(cn$weight == 1))
})

# --- 7. rendu plotly ----------------------------------------------------------

test_that("le rendu plotly porte les 3 traces et les infobulles du contrat", {
  net <- build_pathway_network_data(.toy_enrich_df(), top_n = 30, mode = "emap")
  p <- plot_pathway_network_interactive(net, title = "Test")
  expect_s3_class(p, "plotly")
  expect_s3_class(p, "htmlwidget")
  tr_attrs <- p$x$attrs
  expect_true(any(vapply(tr_attrs, function(a) identical(a[["mode"]], "lines"), TRUE)))
  expect_true(any(vapply(tr_attrs, function(a) identical(a[["type"]], "scatter") &&
                                           identical(a[["mode"]], "markers"), TRUE)))
  # infobulle terme : libellé + effectif + p.adjust (text vit dans x$data,
  # miroir dans x$attrs ; certains traces légitimes n'en portent pas)
  texts <- unlist(c(
    lapply(p$x$data, function(tr) if (is.null(tr$text)) character(0) else tr$text),
    lapply(p$x$attrs, function(a) if (is.null(a[["text"]])) character(0) else a[["text"]])
  ), use.names = FALSE)
  expect_true(any(grepl("Voie un", texts, fixed = TRUE)))
  expect_true(any(grepl("p.adjust", texts, fixed = TRUE)))
})

# --- 8. synchronisation code <-> contrat --------------------------------------

test_that("le contrat documente les jetons gelés", {
  doc_path <- file.path(ts_project_root(), "docs", "contracts",
                        "PATHWAY_NETWORK_CONTRACT.md")
  skip_if_not(file.exists(doc_path), .ts_contract_skip_msg(doc_path))
  doc <- paste(.ts_contract_readlines(doc_path, warn = FALSE), collapse = "\n")
  for (token in c("pathway_network_data", "build_pathway_network_data",
                  "pathway_network_layout", "plot_pathway_network_interactive",
                  "TS_PATHWAY_NETWORK_MAX_TERMS", "TS_PATHWAY_NET_MIN_SIM",
                  "TS_PATHWAY_NETWORK_LAYOUT_SEED",
                  "R\u00e9seau interactif (survol des n\u0153uds)",
                  "plotly_click", "network_interactive")) {
    expect_match(doc, token, fixed = TRUE,
                 info = paste("jeton absent du contrat :", token))
  }
})

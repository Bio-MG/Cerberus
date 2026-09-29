# =============================================================================
# helpers_pathway.R — Pathway enrichment (ORA + GSEA), shared sc + bulk
# =============================================================================
# Extracted from global.R (refactor, session post-v1.0 Bulk) — single source
# of truth for pathway analysis, called by BOTH mod_sc_pathways.R and
# mod_bulk_pathways.R so the two domains never diverge in methodology.
#
# Contents:
#   - run_pathway_enrichment() : ORA (GO/KEGG/Reactome) via clusterProfiler
#   - run_gsea_enrichment()    : pre-ranked GSEA (GO/KEGG/Reactome)
#   - plot_pathway_barplot(), plot_pathway_dotplot(), build_pathway_dt()
#
# Depends on: clusterProfiler, org.Hs.eg.db / org.Mm.eg.db, ReactomePA
# (KEGG/Reactome only), KEGGREST (optional), ggplot2, viridis, DT.
# =============================================================================




#' Pathway Enrichment Analysis (Robust v3)

#' @param genes Vecteur de gènes (Symbols)

#' @param organism Organisme ("human", "mouse")

#' @param database Base de données ("GOBP", "KEGG", "Reactome")

#' @param pval_cutoff Seuil p-value

#' @param p_adjust_method Méthode de correction pour tests multiples ("BH",
#'   "BY", "bonferroni", "holm", ... — voir stats::p.adjust.methods).
#'   Défaut "BH" : comportement historique strictement inchangé.

#' @return data.frame avec pathways enrichis

run_pathway_enrichment <- function(genes, organism = "human",

                                   database = "GOBP",

                                   pval_cutoff = 0.05, universe = NULL,
                                   p_adjust_method = "BH") {

  

  # STAT-Q1 : méthode de correction exposée à l'utilisateur. Validation contre
  # stats::p.adjust.methods (liste de référence de R) plutôt que contre
  # TS_PADJ_METHODS, qui n'est que le sous-ensemble offert dans l'UI — un
  # appelant programmatique reste libre d'utiliser "none" ou "hommel".
  p_adjust_method <- match.arg(p_adjust_method, stats::p.adjust.methods)

  # 1. Check Core Dependencies

  if (!requireNamespace("clusterProfiler", quietly = TRUE))

    stop(errorCondition("Package 'clusterProfiler' requis. Installez-le via BiocManager.", class = "pathway_error"))

  

  library(clusterProfiler)

  

  # 2. Prepare Organism Database & Convert Genes

  gene_entrez <- NULL

  

  if (organism == "human") {

    if (!requireNamespace("org.Hs.eg.db", quietly = TRUE))

      stop(errorCondition("Package 'org.Hs.eg.db' requis pour l'analyse humaine.", class = "pathway_error"))

    library(org.Hs.eg.db)

    orgdb <- org.Hs.eg.db

    

  } else if (organism == "mouse") {

    if (!requireNamespace("org.Mm.eg.db", quietly = TRUE))

      stop(errorCondition("Package 'org.Mm.eg.db' requis pour l'analyse souris.", class = "pathway_error"))

    library(org.Mm.eg.db)

    orgdb <- org.Mm.eg.db

    

  } else {

    stop(errorCondition("Organisme non supporté (choisir 'human' ou 'mouse')", class = "pathway_error"))

  }

  

  # Clean input genes: remove NAs, empty strings, trim whitespace

  genes_clean <- unique(trimws(genes[!is.na(genes) & nchar(trimws(genes)) > 0]))

  

  if (length(genes_clean) == 0) {

    stop(errorCondition("Aucun gène valide fourni après nettoyage.", class = "pathway_error"))

  }

  

  # Attempt conversion Symbol -> Entrez ID

  # Note: bitr can fail if keys are invalid. We wrap it in tryCatch.

  tryCatch({

    gene_entrez <- bitr(

      genes_clean, 

      fromType = "SYMBOL", 

      toType   = "ENTREZID", 

      OrgDb    = orgdb

    )

  }, error = function(e) {

    # If bitr fails completely, return empty

    warning(paste("Erreur lors de la conversion des gènes:", e$message))

    gene_entrez <<- data.frame(SYMBOL=character(0), ENTREZID=character(0))

  })

  

  # Check if any genes were successfully converted

  if (is.null(gene_entrez) || nrow(gene_entrez) == 0) {

    stop(errorCondition("Aucun gène n'a pu être converti en Entrez ID. Vérifiez que les noms de gènes sont des symboles officiels (ex: 'TP53', 'Actb') et correspondent à l'organisme sélectionné.", class = "pathway_error"))

  }

  

  ids <- gene_entrez$ENTREZID

  universe_entrez <- NULL
  if (!is.null(universe)) {
    universe_clean <- unique(trimws(universe[!is.na(universe) & nchar(trimws(universe)) > 0]))
    if (length(universe_clean) > 0) {
      universe_map <- tryCatch(bitr(universe_clean, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = orgdb),
                               error = function(e) NULL)
      if (!is.null(universe_map) && nrow(universe_map) > 0) {
        universe_entrez <- union(unique(universe_map$ENTREZID), ids)
      } else {
        warning("Univers fourni mais aucun ID converti — repli sur le fond par défaut.")
      }
    }
  } 

  # 3. Run Enrichment based on Database

  enrich_result <- NULL

  

  if (database == "GOBP") {

    enrich_result <- enrichGO(

      gene          = ids,

      OrgDb         = orgdb,

      ont           = "BP",

      pAdjustMethod = p_adjust_method,

      pvalueCutoff  = pval_cutoff,

      qvalueCutoff  = 0.2,

      readable      = FALSE, # Keep as Entrez for consistency, or TRUE for Symbols

      universe      = universe_entrez

    )

    

  } else if (database == "KEGG") {

    # KEGG requires organism code (hsa/mmu)

    org_code <- if (organism == "human") "hsa" else "mmu"

    

    # Check if KEGGREST is available for newer clusterProfiler versions

    if (!requireNamespace("KEGGREST", quietly = TRUE)) {

      warning("KEGGREST recommandé pour KEGG. Installation suggérée.")

    }

    

    enrich_result <- enrichKEGG(

      gene          = ids,

      organism      = org_code,

      pAdjustMethod = p_adjust_method,

      pvalueCutoff  = pval_cutoff,

      universe      = universe_entrez

    )

    

  } else if (database == "Reactome") {

    if (!requireNamespace("ReactomePA", quietly = TRUE))

      stop(errorCondition("Package 'ReactomePA' requis pour l'analyse Reactome.", class = "pathway_error"))

    

    library(ReactomePA)

    

    # ReactomePA::enrichPathway expects organism name exactly as per its DB

    # Usually "human" or "mouse" works if the DB is loaded.

    enrich_result <- enrichPathway(

      gene          = ids,

      organism      = organism, 

      pAdjustMethod = p_adjust_method,

      pvalueCutoff  = pval_cutoff,

      universe      = universe_entrez

    )

  } else {

    stop(errorCondition("Base de données non supportée (GOBP/KEGG/Reactome)", class = "pathway_error"))

  }

  

  # 4. Format Output

  if (is.null(enrich_result) || nrow(as.data.frame(enrich_result)) == 0) {

    return(data.frame(

      ID          = character(0),

      Description = character(0),

      p.adjust    = numeric(0),

      Count       = integer(0),

      GeneRatio   = character(0)

    ))

  }

  

  res_df <- as.data.frame(enrich_result)

  # STAT-S2 : attach the raw enrichResult S4 object as an attribute (additive —
  # mirrors the `gsea_obj` pattern of run_gsea_enrichment()). enrichplot network
  # views (emapplot/cnetplot + pairwise_termsim) need this raw object (gene
  # sets, GeneRatio, geneID); consumers that only need the table can ignore it —
  # the data.frame contract for existing callers is unchanged.
  attr(res_df, "enrich_obj") <- enrich_result

  return(res_df)

}



#' Gene Set Enrichment Analysis (GSEA, pre-ranked) — complements run_pathway_enrichment()

#'

#' Unlike ORA (run_pathway_enrichment), GSEA does not require an arbitrary

#' significance threshold: it ranks ALL tested genes by a continuous score

#' (here, signed -log10(p) * sign(log2FC), the standard ranking metric) and

#' tests for enrichment along that full ranking. More statistically robust

#' when few genes pass a hard significance cutoff.

#'

#' @param de_results data.frame with at least columns: gene, log2FoldChange, pvalue.

#' @param organism "human" or "mouse".

#' @param database "GOBP", "KEGG", or "Reactome".

#' @param pval_cutoff p-value cutoff passed to the GSEA call.

#' @param p_adjust_method Méthode de correction pour tests multiples ("BH",
#'   "BY", "bonferroni", "holm", ... — voir stats::p.adjust.methods).
#'   Défaut "BH" : comportement historique strictement inchangé.

#' @return data.frame: ID, Description, setSize, enrichmentScore, NES, pvalue, p.adjust, ...

run_gsea_enrichment <- function(de_results, organism = "human",

                                 database = "GOBP", pval_cutoff = 0.05,
                                 p_adjust_method = "BH") {

  # STAT-Q1 : cf. run_pathway_enrichment() — même validation, même défaut.
  p_adjust_method <- match.arg(p_adjust_method, stats::p.adjust.methods)



  if (!requireNamespace("clusterProfiler", quietly = TRUE))

    stop(errorCondition("Package 'clusterProfiler' requis. Installez-le via BiocManager.", class = "pathway_error"))

  library(clusterProfiler)



  if (organism == "human") {

    if (!requireNamespace("org.Hs.eg.db", quietly = TRUE))

      stop(errorCondition("Package 'org.Hs.eg.db' requis pour l'analyse humaine.", class = "pathway_error"))

    library(org.Hs.eg.db)

    orgdb <- org.Hs.eg.db

  } else if (organism == "mouse") {

    if (!requireNamespace("org.Mm.eg.db", quietly = TRUE))

      stop(errorCondition("Package 'org.Mm.eg.db' requis pour l'analyse souris.", class = "pathway_error"))

    library(org.Mm.eg.db)

    orgdb <- org.Mm.eg.db

  } else {

    stop(errorCondition("Organisme non supporté (choisir 'human' ou 'mouse')", class = "pathway_error"))

  }



  required_cols <- c("gene", "log2FoldChange", "pvalue")

  missing_cols <- setdiff(required_cols, colnames(de_results))

  if (length(missing_cols) > 0) {

    stop(errorCondition(paste0("Colonnes manquantes dans les résultats DE pour GSEA : ", paste(missing_cols, collapse = ", ")), class = "pathway_error"))

  }



  de_clean <- de_results[!is.na(de_results$pvalue) & !is.na(de_results$log2FoldChange), ]

  de_clean <- de_clean[!is.na(de_clean$gene) & nchar(trimws(de_clean$gene)) > 0, ]

  if (nrow(de_clean) < 10) {

    stop(errorCondition(paste0("Trop peu de gènes valides (", nrow(de_clean), ") après nettoyage pour GSEA."), class = "pathway_error"))

  }



  gene_entrez <- tryCatch({

    AnnotationDbi::select(orgdb, keys = unique(de_clean$gene), keytype = "SYMBOL", columns = "ENTREZID")

  }, error = function(e) {

    stop(errorCondition(paste0("Erreur lors de la conversion des gènes pour GSEA : ", conditionMessage(e)), class = "pathway_error"))

  })

  gene_entrez <- gene_entrez[!is.na(gene_entrez$ENTREZID), ]

  gene_entrez <- gene_entrez[!duplicated(gene_entrez$SYMBOL), ]



  de_merged <- merge(de_clean, gene_entrez, by.x = "gene", by.y = "SYMBOL")

  if (nrow(de_merged) < 10) {

    stop(errorCondition("Aucun gène n'a pu être converti en Entrez ID pour GSEA. Vérifiez l'organisme sélectionné.", class = "pathway_error"))

  }



  # Standard pre-ranking metric: signed -log10(p), tie-broken by averaging

  # duplicated Entrez IDs (can happen when multiple symbols map to one gene).

  de_merged$rank_metric <- -log10(pmax(de_merged$pvalue, 1e-300)) * sign(de_merged$log2FoldChange)

  de_merged <- stats::aggregate(rank_metric ~ ENTREZID, data = de_merged, FUN = mean)



  ranked <- sort(setNames(de_merged$rank_metric, de_merged$ENTREZID), decreasing = TRUE)



  gsea_result <- if (database == "GOBP") {

    clusterProfiler::gseGO(geneList = ranked, OrgDb = orgdb, ont = "BP",

                           pvalueCutoff = pval_cutoff, pAdjustMethod = p_adjust_method, verbose = FALSE)

  } else if (database == "KEGG") {

    org_code <- if (organism == "human") "hsa" else "mmu"

    clusterProfiler::gseKEGG(geneList = ranked, organism = org_code,

                             pvalueCutoff = pval_cutoff, pAdjustMethod = p_adjust_method, verbose = FALSE)

  } else if (database == "Reactome") {

    if (!requireNamespace("ReactomePA", quietly = TRUE))

      stop(errorCondition("Package 'ReactomePA' requis pour l'analyse Reactome.", class = "pathway_error"))

    library(ReactomePA)

    ReactomePA::gsePathway(geneList = ranked, organism = organism,

                          pvalueCutoff = pval_cutoff, pAdjustMethod = p_adjust_method, verbose = FALSE)

  } else {

    stop(errorCondition("Base de données non supportée pour GSEA (GOBP/KEGG/Reactome)", class = "pathway_error"))

  }



  res_df <- as.data.frame(gsea_result)

  if (nrow(res_df) == 0) {

    return(data.frame(ID = character(0), Description = character(0), setSize = integer(0),

                      enrichmentScore = numeric(0), NES = numeric(0),

                      pvalue = numeric(0), p.adjust = numeric(0)))

  }

  # Normalize column naming to stay compatible with build_pathway_dt() /

  # plot_pathway_barplot() / plot_pathway_dotplot(), which expect "Count"

  # and "GeneRatio" — GSEA uses "setSize" instead, so we alias it.

  res_df$Count     <- res_df$setSize

  res_df$GeneRatio <- paste0(res_df$setSize, "/", length(ranked))



  # Attach the raw gseaResult S4 object as an attribute (additive — does not

  # change the data.frame contract for existing callers). enrichplot::gseaplot2()

  # needs this raw object (with @geneList, @result, etc.) to draw the running

  # enrichment-score curve; consumers that only need the table can ignore it.

  attr(res_df, "gsea_obj") <- gsea_result

  res_df

}








#' Top-N pathway barplot (shared by mod_sc_pathways.R and mod_bulk.R)

plot_pathway_barplot <- function(df, db_label = "", top_n = 15, tr = NULL,
                                 palette = "default", manual_gradient = NULL) {

  tr <- tr %||% function(x) x

  df_top <- head(df, top_n)

  df_top$Description <- factor(df_top$Description, levels = rev(df_top$Description))

  ggplot(df_top, aes(x = Count, y = Description, fill = -log10(p.adjust))) +

    geom_bar(stat = "identity", width = 0.7) +

    expression_continuous_scale(palette, "fill", manual_gradient, base_option = "plasma", direction = -1) +

    labs(title = paste(tr("Top"), top_n, tr("Pathways"), "-", db_label),

         x = tr("Nombre de gènes"), y = NULL, fill = "-log10(P-adj)") +

    theme_minimal(base_size = 12) +

    theme(axis.text.y = element_text(size = 10), plot.title = element_text(face = "bold", size = 14),

          legend.position = "right", panel.grid.major.y = element_blank())

}



#' Pathway dotplot (shared)

plot_pathway_dotplot <- function(df, db_label = "", top_n = 20, tr = NULL,
                                 palette = "default", manual_gradient = NULL) {

  tr <- tr %||% function(x) x

  df_top <- head(df, top_n)

  df_top$Description <- factor(df_top$Description, levels = rev(df_top$Description))

  if ("GeneRatio" %in% colnames(df_top)) {

    df_top$GeneRatioNum <- sapply(df_top$GeneRatio, function(r) {

      parts <- strsplit(as.character(r), "/")[[1]]

      if (length(parts) == 2) as.numeric(parts[1]) / as.numeric(parts[2]) else NA

    })

  } else {

    df_top$GeneRatioNum <- df_top$Count / max(df_top$Count, na.rm = TRUE)

  }

  ggplot(df_top, aes(x = GeneRatioNum, y = Description, color = -log10(p.adjust), size = Count)) +

    geom_point(alpha = 0.85) +

    expression_continuous_scale(palette, "color", manual_gradient, base_option = "magma", direction = -1) +

    labs(title = paste(tr("Dotplot Pathways"), "-", db_label),

         x = tr("Ratio de gènes"), y = NULL, color = "-log10(P-adj)", size = tr("Nombre de gènes")) +

    theme_minimal(base_size = 12) +

    theme(axis.text.y = element_text(size = 10), plot.title = element_text(face = "bold", size = 14),

          legend.position = "right")

}



#' Pathway enrichment network (emapplot / cnetplot) — STAT-S2
#'
#' Pure consumer of the RAW enrichment object (enrichResult/gseaResult) stored
#' as attribute `enrich_obj` on the results data.frame by
#' `run_pathway_enrichment()` / `run_gsea_enrichment()` — the same additive
#' pattern as `gsea_obj`. The network is DESCRIPTIVE: edges encode gene
#' similarity between pathways (emap) or pathway↔gene membership (cnet),
#' never causality.
#'
#' @param df results data.frame carrying the `enrich_obj` attribute.
#' @param db_label label of the enrichment database, shown in the title.
#' @param top_n maximum number of pathways shown (selected on p.adjust).
#' @param mode "emap" (pathway–pathway similarity network) or "cnet"
#'   (pathway–gene bipartite network).
#' @param tr optional translation function.
#' @return a ggplot object.
#'
#' @export
plot_pathway_network <- function(df, db_label = "", top_n = 30,
                                 mode = c("emap", "cnet"), tr = NULL) {

  tr <- tr %||% function(x) x
  mode <- match.arg(mode)

  if (!is.numeric(top_n) || length(top_n) != 1L || is.na(top_n) || top_n < 2) {
    stop("top_n doit être un nombre >= 2 (un réseau d'une seule voie n'a pas de sens).", call. = FALSE)
  }

  enrich_obj <- attr(df, "enrich_obj")

  if (is.null(enrich_obj)) {
    stop("Aucun objet d'enrichissement brut attaché à ce résultat (attribut 'enrich_obj' absent). Relancez l'enrichissement pour produire le réseau.", call. = FALSE)
  }

  if (!requireNamespace("enrichplot", quietly = TRUE)) {
    stop("Package 'enrichplot' requis. Installez-le via BiocManager.", call. = FALSE)
  }

  if (nrow(as.data.frame(enrich_obj)) < 2) {
    stop("Au moins deux voies enrichies sont nécessaires pour tracer un réseau.", call. = FALSE)
  }

  if (mode == "emap") {
    obj <- enrichplot::pairwise_termsim(enrich_obj)
    p <- enrichplot::emapplot(obj, showCategory = top_n)
    subtitle <- tr("Voies reliées par similarité de gènes (descriptif)")
  } else {
    p <- enrichplot::cnetplot(enrich_obj, showCategory = top_n)
    subtitle <- tr("Voies reliées à leurs gènes (descriptif)")
  }

  p + ggplot2::labs(
    title = paste(tr("Réseau d'enrichissement"), "-", db_label),
    subtitle = subtitle
  )

}


# ── STAT-S2 V2 : réseau d'enrichissement INTERACTIF (plotly + igraph) ───────
# Cadrage §2bd.5 #3 (2026-09-16) : la V1 (`plot_pathway_network()`) est
# statique ; la V2 est un réseau INTERACTIF. Choix tranché le 2026-09-28
# (décision utilisateur) : plotly + igraph — ZÉRO dépendance nouvelle (toutes
# deux déjà au renv.lock et installées ; précédent PLOT-S6 pour le toggle
# plotly). visNetwork est ÉCARTÉ pour cette version (dépendance nouvelle —
# évaluation documentée dans `docs/mcp_propagation.md` §12 et le contrat).

#' Enrichment network data — STAT-S2 V2 (structure figée)
#'
#' Construit la structure nœuds/arêtes consommée par
#' `plot_pathway_network_interactive()`. R PUR, sans enrichplot : la
#' similarité terme–terme (mode "emap") est un Jaccard ordinaire sur les jeux
#' de gènes, ce qui rend le constructeur testable sans Bioconductor et
#' reproductible indépendamment du rendu.
#' Contrat : `docs/contracts/PATHWAY_NETWORK_CONTRACT.md`.
#'
#' @param df résultats portant l'attribut `enrich_obj` (même convention que
#'   `plot_pathway_network()`).
#' @param top_n nombre maximal de termes (triés sur `p.adjust`), plafonné à
#'   `TS_PATHWAY_NETWORK_MAX_TERMS`.
#' @param mode "emap" (termes ↔ termes) ou "cnet" (termes ↔ gènes).
#' @return une liste de classe `pathway_network_data` : `nodes`
#'   (id/label/kind/count/p.adjust), `edges` (from/to/weight), `meta`.
#'
#' @export
build_pathway_network_data <- function(df, top_n = 30, mode = c("emap", "cnet")) {

  mode <- match.arg(mode)

  if (!is.data.frame(df)) {
    stop(errorCondition(paste0("df doit être un data.frame de résultats de voies (reçu : ", class(df)[1], ")."),
                        class = "pathway_error"))
  }

  enrich_obj <- attr(df, "enrich_obj")
  if (is.null(enrich_obj)) {
    stop(errorCondition("Aucun objet d'enrichissement brut attaché à ce résultat (attribut 'enrich_obj' absent). Relancez l'enrichissement pour produire le réseau.",
                        class = "pathway_error"))
  }

  if (!is.numeric(top_n) || length(top_n) != 1L || is.na(top_n) || top_n < 2) {
    stop(errorCondition("top_n doit être un nombre >= 2 (un réseau d'une seule voie n'a pas de sens).",
                        class = "pathway_error"))
  }
  top_n <- min(as.integer(top_n), TS_PATHWAY_NETWORK_MAX_TERMS)

  res <- as.data.frame(enrich_obj)
  if (nrow(res) < 2) {
    stop(errorCondition("Au moins deux voies enrichies sont nécessaires pour tracer un réseau.",
                        class = "pathway_error"))
  }

  needed <- c("ID", "Description", "Count", "p.adjust", "geneID")
  if (!all(needed %in% names(res))) {
    stop(errorCondition(paste0("Colonnes manquantes dans l'objet d'enrichissement : ",
                               paste(setdiff(needed, names(res)), collapse = ", "), "."),
                        class = "pathway_error"))
  }

  terms <- res[order(res$p.adjust), ][seq_len(min(top_n, nrow(res))), ]

  gene_sets <- strsplit(terms$geneID, "/", fixed = TRUE)
  term_nodes <- data.frame(
    id = as.character(terms$ID),
    label = ifelse(is.na(terms$Description) | !nzchar(terms$Description),
                   as.character(terms$ID), as.character(terms$Description)),
    kind = "term",
    count = as.integer(terms$Count),
    p.adjust = as.numeric(terms$p.adjust),
    stringsAsFactors = FALSE
  )

  if (mode == "emap") {
    edges <- .pathway_emap_edges(term_nodes$id, gene_sets)
    nodes <- term_nodes
    min_sim <- TS_PATHWAY_NET_MIN_SIM
  } else {
    edges <- .pathway_cnet_edges(term_nodes$id, gene_sets)
    gene_ids <- sort(unique(edges$to))
    nodes <- rbind(term_nodes,
                   data.frame(id = gene_ids, label = gene_ids, kind = "gene",
                              count = NA_integer_, p.adjust = NA_real_,
                              stringsAsFactors = FALSE))
    min_sim <- NA_real_
  }

  structure(
    list(
      nodes = nodes,
      edges = edges,
      meta = list(mode = mode, top_n = nrow(term_nodes),
                  n_edges = nrow(edges), min_similarity = min_sim,
                  gene_separator = "/")
    ),
    class = "pathway_network_data"
  )

}

#' Jaccard term–term edges (emap) — helper pur de `build_pathway_network_data`
.pathway_emap_edges <- function(term_ids, gene_sets) {
  n <- length(term_ids)
  if (n < 2L) {
    return(data.frame(from = character(0), to = character(0),
                      weight = numeric(0), stringsAsFactors = FALSE))
  }
  from <- character(0); to <- character(0); w <- numeric(0)
  for (i in seq_len(n - 1L)) {
    for (j in seq(i + 1L, n)) {
      inter <- length(intersect(gene_sets[[i]], gene_sets[[j]]))
      if (inter == 0L) next
      sim <- inter / length(union(gene_sets[[i]], gene_sets[[j]]))
      if (sim >= TS_PATHWAY_NET_MIN_SIM) {
        pair <- sort(c(term_ids[[i]], term_ids[[j]]))
        from <- c(from, pair[[1]]); to <- c(to, pair[[2]]); w <- c(w, sim)
      }
    }
  }
  data.frame(from = from, to = to, weight = w, stringsAsFactors = FALSE)
}

#' Term–gene membership edges (cnet) — helper pur de `build_pathway_network_data`
.pathway_cnet_edges <- function(term_ids, gene_sets) {
  from <- unlist(Map(rep, term_ids, vapply(gene_sets, length, integer(1))), use.names = FALSE)
  to <- unlist(gene_sets, use.names = FALSE)
  data.frame(from = from, to = to, weight = 1,
             stringsAsFactors = FALSE)
}

#' Deterministic network layout (Fruchterman-Reingold, graine figée)
#'
#' Séparé du rendu pour être testable : deux appels avec la même entrée
#' rendent les MÊMES coordonnées (garantie par le contrat).
#'
#' @param net sortie de `build_pathway_network_data()`.
#' @return le data.frame `nodes` de `net`, augmenté de `x`/`y`.
#' @export
pathway_network_layout <- function(net) {

  if (!all(c("nodes", "edges") %in% names(net)) ||
      !is.data.frame(net$nodes) || !is.data.frame(net$edges)) {
    stop(errorCondition("net doit être la sortie de build_pathway_network_data().",
                        class = "pathway_error"))
  }

  g <- igraph::graph_from_data_frame(net$edges, directed = FALSE,
                                     vertices = net$nodes)
  coords <- withr::with_seed(
    TS_PATHWAY_NETWORK_LAYOUT_SEED,
    igraph::layout_with_fr(g)
  )
  net$nodes$x <- coords[, 1]
  net$nodes$y <- coords[, 2]
  net$nodes
}

#' Interactive enrichment network — STAT-S2 V2 (plotly)
#'
#' Rendu plotly de la structure `pathway_network_data` : survol d'un terme →
#' libellé, effectif et p.adjust ; survol d'un gène (mode cnet) →
#' identifiant. La barre de mode plotly fournit l'export PNG. Le CLIC
#' (filtrage du tableau par un terme cliqué) est l'item ouvert du contrat —
#' non câblé dans cette version.
#'
#' @param net sortie de `build_pathway_network_data()`.
#' @param title titre du tracé (peut être pré-traduit par l'appelant).
#' @param tr fonction de traduction optionnelle.
#' @return un widget plotly.
#' @export
plot_pathway_network_interactive <- function(net, title = "", tr = NULL) {

  tr <- tr %||% function(x) x

  if (!inherits(net, "pathway_network_data")) {
    stop(errorCondition("net doit être la sortie de build_pathway_network_data().",
                        class = "pathway_error"))
  }

  nodes <- pathway_network_layout(net)
  edges <- net$edges

  # arêtes : un seul trace de segments (x/y alternés, NA sépare les segments)
  edge_x <- as.vector(t(cbind(nodes$x[match(edges$from, nodes$id)],
                              nodes$x[match(edges$to, nodes$id)], NA)))
  edge_y <- as.vector(t(cbind(nodes$y[match(edges$from, nodes$id)],
                              nodes$y[match(edges$to, nodes$id)], NA)))

  terms <- nodes[nodes$kind == "term", ]
  genes <- nodes[nodes$kind == "gene", ]

  term_hover <- paste0(
    "<b>", terms$label, "</b><br>",
    terms$count, " ", tr("g\u00e8nes"), "<br>",
    "p.adjust = ", format.pval(terms$p.adjust, digits = 3)
  )

  p <- plotly::plot_ly(type = "scatter", mode = "lines")
  if (nrow(edges) > 0) {
    p <- plotly::add_trace(p, x = edge_x, y = edge_y, mode = "lines",
                           line = list(color = "#cccccc", width = 1),
                           hoverinfo = "none", showlegend = FALSE)
  }
  if (nrow(genes) > 0) {
    p <- plotly::add_trace(p, data = genes, x = ~x, y = ~y, type = "scatter",
                           mode = "markers", text = genes$label,
                           hoverinfo = "text",
                           marker = list(color = "#999999", size = 7),
                           showlegend = FALSE)
  }
  p <- plotly::add_trace(p, data = terms, x = ~x, y = ~y, type = "scatter",
                         mode = "markers", text = term_hover,
                         hoverinfo = "text",
                         marker = list(
                           color = "#2c7fb8",
                           size = pmax(8, 3.5 * sqrt(pmax(terms$count, 1)))
                         ),
                         showlegend = FALSE)
  p <- plotly::layout(
    p,
    title = title,
    showlegend = FALSE,
    xaxis = list(visible = FALSE),
    yaxis = list(visible = FALSE),
    hoverlabel = list(bgcolor = "white"),
    margin = list(t = 40)
  )
  p
}



#' Pathway results DT table (shared)

build_pathway_dt <- function(df, tr = NULL) {

  tr <- tr %||% function(x) x

  cols_available <- intersect(c("ID", "Description", "p.adjust", "Count", "GeneRatio"), colnames(df))

  df_display <- df[, cols_available, drop = FALSE]

  colnames(df_display) <- c("ID", "Description", "P-adj", tr("Nb Gènes"), "Ratio")[seq_along(cols_available)]

  ts_datatable(df_display, page_length = 15L, filename_base = "pathways") %>%

    DT::formatStyle("P-adj", color = DT::styleInterval(c(0.001, 0.01, 0.05),

                                                        c("darkgreen", "green", "orange", "red")))

}

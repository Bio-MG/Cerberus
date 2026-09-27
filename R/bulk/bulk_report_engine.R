# =============================================================================
# R/bulk/bulk_report_engine.R — Bulk report generation engine
# (extracted from mod_bulk_report.R, Block 9 refactor)
# =============================================================================
# Pure functions: no Shiny reactivity. Called by mod_bulk_report_server().
# Depends on: helpers_bulk.R (filter_bulk_counts, build_dds, etc.)
# =============================================================================

#' Locate bulk_report_template.Rmd robustly (Step-3.6 fix)
#'
#' The previous version hardcoded a single relative path
#' (`modules/bulk/bulk_report_template.Rmd`), which only resolves when the
#' app's working directory is EXACTLY the project root at render time (true
#' for the normal `shiny::runApp()` from the project folder, but not
#' guaranteed for every deployment/launch method — e.g. Shiny Server app
#' directories, `rsconnect`, or running from a different `wd`). A missing
#' template previously surfaced as a cryptic `file.copy()`/`rmarkdown::render()`
#' error with no actionable message. This tries a small set of realistic
#' candidates and fails with a clear, French, actionable message otherwise.
#'
#' @return Character path to the template file.
.find_bulk_report_template <- function() {
  candidates <- unique(c(
    file.path("reports", "bulk_report_template.Rmd"),
    file.path(getwd(), "reports", "bulk_report_template.Rmd"),
    file.path("modules", "bulk", "bulk_report_template.Rmd"),
    file.path(getwd(), "modules", "bulk", "bulk_report_template.Rmd")
  ))
  hit <- Filter(file.exists, candidates)
  if (length(hit) == 0) {
    stop(
       "\u274c Template 'bulk_report_template.Rmd' introuvable. Chemins essay\u00e9s :\n  - ",
      paste(candidates, collapse = "\n  - "),
      "\nV\u00e9rifiez que l'application est lanc\u00e9e depuis la racine du projet ",
      "(shiny::runApp() depuis le dossier contenant app.R).",
      call. = FALSE
    )
  }
  hit[[1]]
}

bulk_r_script_text <- function(n_genes, n_samp, lfc, padj,
                                 contrast_name, condition_col,
                                 group_target, group_ref, palette_colors,
                                 all_contrast_names = NULL, pathway_mode = "ora",
                                 padj_method = "BH", pathway_padj_method = NULL) {
  # STAT-Q1 : padj_method est INTERPOLÉE dans le script R généré (voir
  # PADJ_METHOD ci-dessous) — on la valide AVANT interpolation pour qu'aucune
  # valeur inattendue ne puisse atterrir dans du code exécutable.
  padj_method <- match.arg(padj_method, stats::p.adjust.methods)
  # Le panneau Pathways a son propre sélecteur (STAT-Q1) ; à défaut on retombe
  # sur la méthode DE, pour ne jamais produire un script incohérent.
  pathway_padj_method <- pathway_padj_method %||% padj_method
  pathway_padj_method <- match.arg(pathway_padj_method, stats::p.adjust.methods)
  rc   <- palette_colors %||% c(Up = "#E74C3C", Down = "#2980B9", NS = "#BDC3C7")
  date <- format(Sys.Date(), "%Y-%m-%d")
  cond <- condition_col %||% "condition"

  # ── Pairwise block (only when the app has >1 stored contrast) ────────────
  other_names <- setdiff(all_contrast_names %||% character(0), contrast_name)
  pairwise_block <- ""
  if (length(other_names) > 0) {
    pair_list_lines <- vapply(other_names, function(nm) {
      parts_nm <- strsplit(nm, "_vs_")[[1]]
      tgt <- paste(parts_nm[-length(parts_nm)], collapse = "_")
      rf  <- parts_nm[length(parts_nm)]
      sprintf('  list(target = "%s", ref = "%s")', tgt, rf)
    }, character(1))
    pairwise_block <- paste0(
'
# \u2500\u2500 3bis. Pairwise \u2014 autres contrastes calcul\u00e9s dans l\'app (', length(other_names), ' paire(s) suppl.) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
# R\u00e9utilise le M\u00cAME ajustement DESeq2 "dds" (design ~ ', cond, ') pour chaque paire \u2014
# results()/lfcShrink() seulement ; DESeq() n\'est PAS relanc\u00e9.
other_pairs <- list(
', paste(pair_list_lines, collapse = ",\n"), '
)
all_de <- list()
all_de[[paste0(GROUP_TARGET, "_vs_", GROUP_REF)]] <- res_df
for (pr in other_pairs) {
  coef_p <- paste0(CONDITION_COL, "_", pr$target, "_vs_", pr$ref)
  # ── SHRINKAGE : porte + AVERTISSEMENT + drapeau ──
  # `apeglm` est une dependance OPTIONNELLE de DESeq2 (SUGGERE, pas requis).
  # MESURE : sur un projet ou il nest pas installe, le repli renvoyait le
  # resultat NON RETRECI sans rien dire : meme tableau, meme volcano, meme
  # compte de genes significatifs, et des LFC que personne n avait demandes.
  # PAS DE SUBSTITUTION par type="normal" : ce serait changer lestimateur en
  # silence, cest-à-dire la meme malhonnetete que le repli silencieux.
  shrunk_p <- FALSE
  r0 <- DESeq2::results(dds, contrast = c(CONDITION_COL, pr$target, pr$ref), pAdjustMethod = PADJ_METHOD)
  if (requireNamespace("apeglm", quietly=TRUE)) {
    res_p <- tryCatch({
      shrunk_p <<- TRUE
      if (coef_p %in% DESeq2::resultsNames(dds)) {
        DESeq2::lfcShrink(dds, coef = coef_p, res = r0, type = "apeglm", quiet = TRUE)
      } else {
        DESeq2::lfcShrink(dds, contrast = c(CONDITION_COL, pr$target, pr$ref), res = r0, type = "normal", quiet = TRUE)
      }
    }, error = function(e) { shrunk_p <<- FALSE; r0 })
  } else {
    res_p <- r0
  }
  if (!shrunk_p) {
    warning(paste0("Shrinkage LFC non applique pour ", pr$target, " vs ", pr$ref,
                   " : paquet optionnel apeglm absent (ou shrinkage en echec). ",
                   "Les log2FoldChange affiches sont ceux de results(), NON retrecis."))
  }
  df_p <- as.data.frame(res_p); df_p$gene <- rownames(df_p); df_p <- df_p[order(df_p$padj), ]
  attr(df_p, "shrunk") <- shrunk_p
  all_de[[paste0(pr$target, "_vs_", pr$ref)]] <- df_p
  write.csv(df_p, paste0("DE_", pr$target, "_vs_", pr$ref, "_", Sys.Date(), ".csv"), row.names = FALSE)
}

# R\u00e9sum\u00e9 Up/Down \u2014 TOUS les contrastes (actif + pairwise)
summary_rows <- lapply(names(all_de), function(nm) {
  d <- all_de[[nm]]
  sig <- !is.na(d$padj) & d$padj < PADJ_THRESH & abs(d$log2FoldChange) > LFC_THRESH
  data.frame(Contraste = nm, n_testes = nrow(d), n_sig = sum(sig),
             n_up = sum(sig & d$log2FoldChange > 0), n_down = sum(sig & d$log2FoldChange < 0))
})
summary_df <- do.call(rbind, summary_rows)
write.csv(summary_df, paste0("summary_updown_pairwise_", Sys.Date(), ".csv"), row.names = FALSE)
print(summary_df)
'
    )
  }

  # ── Pathway section: ORA (significant genes) or GSEA (full ranked list) ──
  pathway_section <- if (identical(pathway_mode, "gsea")) {
'# \u2500\u2500 7. Pathway GSEA (optionnel, sans seuil) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
# BiocManager::install(c("clusterProfiler","org.Hs.eg.db","enrichplot"))
if (requireNamespace("clusterProfiler",quietly=TRUE) && requireNamespace("org.Hs.eg.db",quietly=TRUE)) {
  library(clusterProfiler); library(org.Hs.eg.db)
  de_gsea <- res_df[!is.na(res_df$padj) & !is.na(res_df$log2FoldChange) & !is.na(res_df$pvalue), ]
  gene_entrez <- tryCatch(
    AnnotationDbi::select(org.Hs.eg.db, keys=unique(de_gsea$gene), keytype="SYMBOL", columns="ENTREZID"),
    error=function(e) NULL)
  if (!is.null(gene_entrez)) {
    gene_entrez <- gene_entrez[!is.na(gene_entrez$ENTREZID) & !duplicated(gene_entrez$SYMBOL), ]
    de_m <- merge(de_gsea, gene_entrez, by.x="gene", by.y="SYMBOL")
    de_m$rank_metric <- -log10(pmax(de_m$pvalue, 1e-300)) * sign(de_m$log2FoldChange)
    de_m <- stats::aggregate(rank_metric ~ ENTREZID, data=de_m, FUN=mean)
    ranked <- sort(setNames(de_m$rank_metric, de_m$ENTREZID), decreasing=TRUE)
    if (length(ranked) >= 10) {
      gsea_res <- gseGO(geneList=ranked, OrgDb=org.Hs.eg.db, ont="BP",
                        pvalueCutoff=0.05, pAdjustMethod=PADJ_METHOD_PW, verbose=FALSE)
      if (!is.null(gsea_res) && nrow(as.data.frame(gsea_res)) > 0) {
        write.csv(as.data.frame(gsea_res), paste0("pathways_GSEA_GOBP_",Sys.Date(),".csv"), row.names=FALSE)
        if (requireNamespace("enrichplot", quietly=TRUE))
          print(enrichplot::gseaplot2(gsea_res, geneSetID=1, title=as.data.frame(gsea_res)$Description[1]))
      }
    }
  }
}
'
  } else {
'# \u2500\u2500 7. Pathway ORA (optionnel) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
# BiocManager::install(c("clusterProfiler","org.Hs.eg.db"))
if (requireNamespace("clusterProfiler",quietly=TRUE) && requireNamespace("org.Hs.eg.db",quietly=TRUE)) {
  library(clusterProfiler); library(org.Hs.eg.db)
  sig_g  <- res_df$gene[!is.na(res_df$padj) & res_df$padj < PADJ_THRESH &
                           abs(res_df$log2FoldChange) > LFC_THRESH]
  entrez <- tryCatch(bitr(sig_g,fromType="SYMBOL",toType="ENTREZID",OrgDb=org.Hs.eg.db),
                     error=function(e) NULL)
  if (!is.null(entrez) && nrow(entrez) >= 10) {
    # ── Le FOND (universe) : les gènes TESTÉS, pas les gènes significatifs ──
    # ⚠️ AUCUN APOSTROPHE dans ce commentaire : ce bloc est une chaîne R entre
    # guillemets simples, donc un apostrophe le terminerait. Cest pour cela que
    # les commentaires voisins de ce script n en contiennent aucun. Le test
    # `parse(text = )` est le seul à voir ce genre de faute.
    #
    # Un ORA se réfère toujours à un univers. Sans `universe`, clusterProfiler
    # prend TOUS les gènes annotés de lespèce (environ 193 000 symboles) comme
    # référence, alors que l app utilise lensemble de gènes réellement détecté
    # par l assay (4 sites d appel : sc_pipeline.R:309, mod_bulk.R:506,
    # mod_sc_pathways.R:173/427 ; voir run_pathway_enrichment(),
    # pathway_helpers.R:195/229/261). Les deux ne sont donc pas la meme analyse.
    #
    # MESURÉ (symboles réels, un terme GO BP réel, panel de 8 000 gènes) :
    #   fond = gènes testés      -> 618 termes, meilleur p.adjust 3.9e-233
    #   fond = défaut clusterProfiler -> 294 termes, meilleur p.adjust 2.3e-07
    #   rapport médian des p.adjust script/app : 375
    # Le SENS de lécart dépend de la composition du jeu de données, donc aucune
    # direction nest revendiquée ici : ce qui est établi, cest que les deux
    # chemins ne concordent pas. Le bon fond est lensemble testé, car cest
    # lensemble sur lequel la correction des tests multiples a été appliquée.
    #
    # `res_df$gene` et NON `sig_g` : lunivers doit contenir la requête, sinon
    # arithmétiquement rien ne peut être sur-représenté. D’où le `union()`, le
    # meme ordre que pathway_helpers.R:164.
    uni_all <- tryCatch(
      bitr(unique(as.character(res_df$gene[!is.na(res_df$gene)])),
           fromType="SYMBOL", toType="ENTREZID", OrgDb=org.Hs.eg.db),
      error=function(e) NULL)
    uni_ids <- if (!is.null(uni_all) && nrow(uni_all) > 0)
      union(unique(uni_all$ENTREZID), unique(entrez$ENTREZID))
    else
      unique(entrez$ENTREZID)
    ora <- enrichGO(gene=entrez$ENTREZID, OrgDb=org.Hs.eg.db, ont="BP",
                    pAdjustMethod=PADJ_METHOD_PW, pvalueCutoff=0.05, readable=TRUE,
                    universe=uni_ids)
    if (!is.null(ora) && nrow(as.data.frame(ora)) > 0) {
      barplot(ora, showCategory=15, title="GO BP (ORA)")
      write.csv(as.data.frame(ora), paste0("pathways_GOBP_",Sys.Date(),".csv"), row.names=FALSE)
    }
  }
}
'
  }

  paste0(
'# =============================================================================
# Script R Reproductible \u2014 TranscriptoShiny
# G\u00e9n\u00e9r\u00e9 le : ', date, '
# Contraste  : ', contrast_name, ' (', group_target, ' vs ', group_ref, ')
# Dataset    : ', n_genes, ' g\u00e8nes \u00d7 ', n_samp, ' \u00e9chantillons
# Condition  : ', cond, '
# Correction : ', padj_method, ' (tests multiples, DE + enrichissement)
# =============================================================================

library(DESeq2); library(ggplot2); library(dplyr)

# \u2500\u2500 0. Donn\u00e9es \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
counts   <- readRDS("counts_raw.rds")   # matrix: genes x samples (raw integers)
metadata <- readRDS("metadata.rds")     # data.frame: rownames = colnames(counts)

# \u2500\u2500 1. Filtrage \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
filtered <- counts[rowSums(counts, na.rm=TRUE) >= 10 & rowSums(counts >= 1, na.rm=TRUE) >= 1, ]
cat(sprintf("%d genes retained\\n", nrow(filtered)))

# \u2500\u2500 2. VST + PCA \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
dds_b   <- DESeq2::DESeqDataSetFromMatrix(filtered, metadata, design = ~1)
dds_b   <- DESeq2::estimateSizeFactors(dds_b)
vst_mat <- SummarizedExperiment::assay(DESeq2::vst(dds_b, blind = TRUE))

rv  <- apply(vst_mat, 1, var)
sel <- order(rv, decreasing = TRUE)[seq_len(min(500L, nrow(vst_mat)))]
pca <- prcomp(t(vst_mat[sel, ]), scale. = FALSE)
pct <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
df_pca <- data.frame(PC1=pca$x[,1], PC2=pca$x[,2], sample=rownames(pca$x))
ggplot(df_pca, aes(PC1, PC2, label=sample)) +
  geom_point(size=3) + geom_text(size=3, vjust=-0.8) +
  labs(title="PCA", x=paste0("PC1 (",pct[1],"%)"), y=paste0("PC2 (",pct[2],"%)")) +
  theme_minimal()

# QC \u2014 corr\u00e9lation inter-\u00e9chantillons
if (requireNamespace("pheatmap", quietly=TRUE))
  pheatmap::pheatmap(cor(vst_mat), main="QC \u2014 Corr\u00e9lation inter-\u00e9chantillons",
                     color=colorRampPalette(c("white","#2C3E50"))(50), fontsize=8)

# \u2500\u2500 3. DESeq2 \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
CONDITION_COL <- "', cond, '"
GROUP_TARGET  <- "', group_target, '"
GROUP_REF     <- "', group_ref, '"
LFC_THRESH    <- ', sprintf("%.3f", lfc), '
PADJ_THRESH   <- ', sprintf("%.4f", padj), '
PADJ_METHOD   <- "', padj_method, '"   # correction tests multiples choisie dans l app
PADJ_METHOD_PW <- "', pathway_padj_method, '"   # idem pour l enrichissement (section 7)

metadata[[CONDITION_COL]] <- relevel(factor(metadata[[CONDITION_COL]]), ref=GROUP_REF)
dds <- DESeq2::DESeqDataSetFromMatrix(filtered, metadata, design=as.formula(paste("~", CONDITION_COL)))
dds <- DESeq2::DESeq(dds, quiet=TRUE)
coef_name <- paste0(CONDITION_COL, "_", GROUP_TARGET, "_vs_", GROUP_REF)
# results() porte la correction PADJ_METHOD ; lfcShrink() re\u00e7oit CE r\u00e9sultat
# via res= au lieu de recalculer ses propres p-values en BH \u2014 sans quoi le
# script ne reproduirait pas la m\u00e9thode choisie dans l\'application.
res0 <- DESeq2::results(dds, contrast=c(CONDITION_COL, GROUP_TARGET, GROUP_REF),
                        pAdjustMethod=PADJ_METHOD)
# ── SHRINKAGE : porte + AVERTISSEMENT + drapeau (meme contrat que l app) ──
# Ce site est le PIRE des deux : il demande `type="apeglm"` SANS branche
# `normal`, donc sur un projet sans apeglm il ne pouvait que degrader en silence.
# Le repli reste un repli (une table non retrecie vaut mieux qu un arret), mais
# il est desormais ANONCE et MARQUE.
shrunk <- FALSE
if (requireNamespace("apeglm", quietly=TRUE)) {
  res <- tryCatch({
    shrunk <<- TRUE
    DESeq2::lfcShrink(dds, coef=coef_name, res=res0, type="apeglm", quiet=TRUE)
  }, error=function(e) { shrunk <<- FALSE; res0 })
} else {
  res <- res0
}
if (!shrunk) {
  warning(paste0("Shrinkage LFC non applique : paquet optionnel apeglm absent ",
                 "(ou shrinkage en echec). Les log2FoldChange affiches sont ceux ",
                 "de results(), NON retrecis."))
}
res_df       <- as.data.frame(res); res_df$gene <- rownames(res_df)
res_df       <- res_df[order(res_df$padj), ]
attr(res_df, "shrunk") <- shrunk
cat(sprintf("Shrinkage LFC : %s\\n", if (shrunk) "applique (apeglm)" else "NON applique (apeglm absent)"))
cat(sprintf("%d significant genes\\n",
    sum(res_df$padj < PADJ_THRESH & abs(res_df$log2FoldChange) > LFC_THRESH, na.rm=TRUE)))
', pairwise_block, '
# \u2500\u2500 4. Volcano \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
res_v <- res_df[!is.na(res_df$padj), ]
res_v$status <- dplyr::case_when(
  res_v$padj < PADJ_THRESH & res_v$log2FoldChange >  LFC_THRESH ~ "Up",
  res_v$padj < PADJ_THRESH & res_v$log2FoldChange < -LFC_THRESH ~ "Down",
  TRUE ~ "NS")
lbl <- c(head(res_v$gene[res_v$status=="Up"],10), head(res_v$gene[res_v$status=="Down"],10))
res_v$label <- ifelse(res_v$gene %in% lbl, res_v$gene, NA_character_)
n_up_v <- sum(res_v$status=="Up"); n_down_v <- sum(res_v$status=="Down")
ggplot(res_v, aes(log2FoldChange, -log10(padj+1e-300), color=status)) +
  geom_point(alpha=0.7, size=1.6) +
  scale_color_manual(values=c(Up="', rc["Up"], '", Down="', rc["Down"], '", NS="', rc["NS"], '")) +
  ggrepel::geom_text_repel(aes(label=label), size=2.8, na.rm=TRUE, show.legend=FALSE,
                           max.overlaps=20, box.padding=0.4, segment.size=0.3, min.segment.length=0) +
  geom_vline(xintercept=c(-LFC_THRESH,LFC_THRESH), linetype="dashed", color="grey40") +
  geom_hline(yintercept=-log10(PADJ_THRESH), linetype="dashed", color="grey40") +
  labs(title="Volcano",
       subtitle=sprintf("%d Up | %d Down | %d testes (padj<%.2g, |log2FC|>%.2g)",
                        n_up_v, n_down_v, nrow(res_v), PADJ_THRESH, LFC_THRESH),
       x="Log2FC", y="-log10(padj)", color="Status") + theme_minimal(base_size=13)

# \u2500\u2500 5. MA-Plot \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
res_ma <- res_df[!is.na(res_df$padj) & !is.na(res_df$baseMean), ]
res_ma$sig <- res_ma$padj < PADJ_THRESH & abs(res_ma$log2FoldChange) > LFC_THRESH
ggplot(res_ma, aes(log10(baseMean+1), log2FoldChange, color=sig)) +
  geom_point(alpha=0.6, size=1.4) +
  scale_color_manual(values=c("TRUE"="', rc["Up"], '","FALSE"="', rc["NS"], '"), guide="none") +
  geom_hline(yintercept=0, color="grey30") +
  labs(title="MA-Plot", x="Log10(BaseMean+1)", y="Log2FC") + theme_minimal()

# \u2500\u2500 6. Heatmap top g\u00e8nes \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
# install.packages("pheatmap")  si n\u00e9cessaire
top_genes <- intersect(head(res_df$gene, 30), rownames(vst_mat))
if (length(top_genes) >= 2 && requireNamespace("pheatmap", quietly=TRUE)) {
  mat_z <- t(scale(t(vst_mat[top_genes, ])))
  ann   <- metadata[colnames(mat_z), CONDITION_COL, drop=FALSE]
  pheatmap::pheatmap(mat_z, annotation_col=ann, scale="none",
                     main=paste("Top", length(top_genes), "g\u00e8nes (p-adj)"), fontsize_row=8)
}


', pathway_section, '

# \u2500\u2500 7bis. edgeR / limma-voom (optionnel, m\u00eame contraste) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
# D\u00e9comment\u00e9 sur demande \u2014 relance le M\u00eame contraste avec un moteur diff\u00e9rent,
# pour comparaison manuelle avec les r\u00e9sultats DESeq2 ci-dessus.
# ---------------------------------------------------------------------------
# if (requireNamespace("edgeR", quietly=TRUE)) {
#   grp    <- factor(metadata[[CONDITION_COL]], levels=c(GROUP_REF, GROUP_TARGET))
#   y      <- edgeR::DGEList(counts=round(filtered), group=grp)
#   y      <- edgeR::calcNormFactors(y)
#   design <- stats::model.matrix(~grp)
#   y      <- edgeR::estimateDisp(y, design)
#   fit    <- edgeR::glmQLFit(y, design)
#   qlf    <- edgeR::glmQLFTest(fit, coef=2)
#   res_edger <- edgeR::topTags(qlf, n=Inf, adjust.method=PADJ_METHOD)$table
#   res_edger$gene <- rownames(res_edger)
#   write.csv(res_edger, paste0("DE_edgeR_", CONDITION_COL, "_", Sys.Date(), ".csv"), row.names=FALSE)
# }
# if (requireNamespace("limma", quietly=TRUE) && requireNamespace("edgeR", quietly=TRUE)) {
#   grp2    <- factor(metadata[[CONDITION_COL]], levels=c(GROUP_REF, GROUP_TARGET))
#   y2      <- edgeR::calcNormFactors(edgeR::DGEList(counts=round(filtered)))
#   design2 <- stats::model.matrix(~grp2)
#   v       <- limma::voom(y2, design2)
#   fit2    <- limma::eBayes(limma::lmFit(v, design2))
#   res_limma <- limma::topTable(fit2, coef=2, number=Inf, sort.by="P", adjust.method=PADJ_METHOD)
#   res_limma$gene <- rownames(res_limma)
#   write.csv(res_limma, paste0("DE_limma_", CONDITION_COL, "_", Sys.Date(), ".csv"), row.names=FALSE)
# }

# \u2500\u2500 8. Export CSV \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
out <- paste0("DE_",CONDITION_COL,"_",GROUP_TARGET,"_vs_",GROUP_REF,"_",Sys.Date(),".csv")
write.csv(res_df, out, row.names=FALSE)
cat("Results saved to:", out, "\\n")
')
}

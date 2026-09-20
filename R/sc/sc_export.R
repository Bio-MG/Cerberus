# =============================================================================
# R/sc/sc_export.R — Reproducible R-script generator for Single-Cell
# =============================================================================
# Extracted from R/sc/sc_pipeline.R (Phase 11a, RF-2 completion).
# Original location was modules/sc/mod_sc.R (.sc_r_script_text) then
# R/sc/sc_pipeline.R (sc_r_script_text) — now standalone pure function.
# Pure function: paste0()/sprintf() text assembly only.
# Called by: mod_sc_server() output$dl_sc_r_script downloadHandler.
# Sourced in app.R BEFORE modules/sc/mod_sc.R.
# =============================================================================

# =============================================================================
# .sc_r_script_text — reproducible SC analysis script (introspects Seurat obj)
# =============================================================================
sc_r_script_text <- function(obj, shared_rv = NULL) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
  meta         <- obj@meta.data
  n_cells      <- ncol(obj)
  n_genes      <- nrow(obj)
  date         <- format(Sys.Date(), "%Y-%m-%d")
  has_umap     <- "umap"            %in% names(obj@reductions)
  has_pca      <- "pca"             %in% names(obj@reductions)
  has_mt       <- "percent.mt"      %in% colnames(meta)
  has_clusters <- "seurat_clusters" %in% colnames(meta)
  singler_cols <- grep("^SingleR_", colnames(meta), value=TRUE)
  has_singler  <- length(singler_cols) > 0
  has_markers  <- !is.null(shared_rv) && !is.null(shared_rv$markers_data) &&
                  nrow(shared_rv$markers_data) > 0
  has_corr     <- !is.null(shared_rv) && !is.null(shared_rv$corr_target_gene)
  has_traj     <- "pseudotime" %in% colnames(meta)
  # ⚠️ PREFIXE OBLIGATOIRE : `Embeddings()` est re-exporte par Seurat ET
  # SeuratObject, mais AUCUN `library(Seurat)` inconditionnel n'existe au boot
  # de l'app (spatial_async.R:222 ne l'attache que dans une garde paresseuse).
  # Un appel nu echoue sur `could not find function "Embeddings"` (§2dd, §2dg).
  pca_dims     <- if (has_pca) min(ncol(Seurat::Embeddings(obj,"pca")), 50) else 20
  n_clusters   <- if (has_clusters) length(levels(factor(meta$seurat_clusters))) else "?"

  paste0(
'# =============================================================================
# Script R Reproductible \u2014 TranscriptoShiny (Single-Cell)
# Généré le : ', date, '
# Dataset    : ', n_genes, ' gènes \u00d7 ', n_cells, ' cellules
# Pipeline   : PCA=', if(has_pca)"oui" else "non",
  ', UMAP=', if(has_umap)"oui" else "non",
  ', clusters=', if(has_clusters) n_clusters else "non",
  ', SingleR=', if(has_singler) paste(singler_cols,collapse=",") else "non",
  ', Corr=', if(has_corr) shared_rv$corr_target_gene else "non",
  ', Traj=', if(has_traj) "oui" else "non", '
# =============================================================================

library(Seurat); library(ggplot2); library(patchwork)

# \u2500\u2500 0. Charger l\'objet ────────────────────────────────────────────────────────
obj <- readRDS("sc_obj.rds")
cat(sprintf("Objet : %d cellules, %d gènes\\n", ncol(obj), nrow(obj)))

# \u2500\u2500 1. QC ──────────────────────────────────────────────────────────────────────
',
if (!has_mt) '
mt_pat <- if (any(grepl("^MT-",rownames(obj)))) { "^MT-"
          } else if (any(grepl("^mt-",rownames(obj)))) { "^mt-" } else NULL
if (!is.null(mt_pat)) obj[["percent.mt"]] <- PercentageFeatureSet(obj, pattern=mt_pat)
' else '# percent.mt déjà calculé dans l\'objet exporté.',
'
VlnPlot(obj, features=c("nFeature_RNA","nCount_RNA"',
if(has_mt) ',"percent.mt"' else '','), ncol=', if(has_mt) 3 else 2, ', pt.size=0)

MIN_GENES <- 100; MAX_GENES <- 8000; MAX_MT <- 20
# obj <- subset(obj, subset=nFeature_RNA>MIN_GENES & nFeature_RNA<MAX_GENES & percent.mt<MAX_MT)

# \u2500\u2500 2. Normalisation ────────────────────────────────────────────────────────────
DefaultAssay(obj) <- "RNA"
obj <- NormalizeData(obj); obj <- FindVariableFeatures(obj, nfeatures=2000); obj <- ScaleData(obj)
# Alternative : obj <- SCTransform(obj, verbose=FALSE, vst.flavor="v2")

# \u2500\u2500 3. PCA + Clustering + UMAP/t-SNE ────────────────────────────────────────────
PCA_DIMS  <- ', pca_dims, '
CLUST_RES <- 0.5
obj <- RunPCA(obj, npcs=PCA_DIMS, verbose=FALSE)
obj <- FindNeighbors(obj, dims=1:PCA_DIMS); obj <- FindClusters(obj, resolution=CLUST_RES)
obj <- RunUMAP(obj, dims=1:PCA_DIMS, verbose=FALSE)
obj <- RunTSNE(obj, dims=1:PCA_DIMS, verbose=FALSE)   # secondaire, dispo dans l\'app aux côtés d\'UMAP
p_umap <- DimPlot(obj, reduction="umap", label=TRUE, pt.size=0.5)
print(p_umap)
ggsave(paste0("umap_clusters_',date,'.png"), p_umap, width=8, height=6, dpi=300)

# \u2500\u2500 4. Marqueurs ────────────────────────────────────────────────────────────────
',
if (has_markers) paste0('# ', nrow(shared_rv$markers_data), ' marqueurs dans l\'app (recalculés ci-dessous, sur',
  ' l\'objet complet — l\'app peut avoir sous-échantillonné pour accélérer le calcul) :') else '',
'
Idents(obj) <- obj$seurat_clusters
markers <- FindAllMarkers(obj, only.pos=TRUE, min.pct=0.1, logfc.threshold=0.25, verbose=FALSE)
write.csv(markers, paste0("markers_', date, '.csv"), row.names=FALSE)
top5 <- markers |> dplyr::group_by(cluster) |> dplyr::slice_min(p_val_adj, n=5)
print(DotPlot(obj, features=unique(top5$gene)) + theme(axis.text.x=element_text(angle=45,hjust=1)))

# \u2500\u2500 5. Annotation SingleR (optionnel) ────────────────────────────────────────────
# BiocManager::install(c("SingleR","celldex"))
',
if (has_singler) paste0(
'# Déjà annoté (colonne : ', tail(singler_cols,1), ') :
print(DimPlot(obj, group.by="', tail(singler_cols,1), '", label=TRUE, repel=TRUE))'
) else
'# if (requireNamespace("SingleR",quietly=TRUE)) {
#   ref  <- celldex::HumanPrimaryCellAtlasData()
#   sce  <- as.SingleCellExperiment(obj)
#   pred <- SingleR::SingleR(test=sce, ref=ref, labels=ref$label.main)
#   obj[["SingleR"]] <- pred$labels
# }',
'

# \u2500\u2500 6. Gene Correlation ──────────────────────────────────────────────────────────
',
if (has_corr) paste0(
'# Gène cible utilisé dans l\'app : ', shared_rv$corr_target_gene, '
TARGET_GENE <- "', shared_rv$corr_target_gene, '"
# Corrélation (top 50 gènes les plus corrélés) :
# corr_df <- find_correlated_genes(obj, TARGET_GENE, method="pearson", threshold=0.3, top_n=50)
# print(plot_gene_correlation_network(corr_df, TARGET_GENE, top_n=20))'
) else
'# Lancez l\'étape 5 (Gene Correlation) dans l\'app pour générer le code de corrélation.',
'

# \u2500\u2500 7. Trajectory / Pseudotemps ────────────────────────────────────────────────
',
if (has_traj) paste0(
'# Pseudotemps déjà calculé dans l\'objet exporté.
ggplot(data.frame(pseudotime=obj$pseudotime, cluster=obj$seurat_clusters),
       aes(x=pseudotime, fill=cluster)) +
  geom_density(alpha=0.6) + scale_fill_viridis_d(option="turbo") +
  labs(title="Distribution Pseudotemps", x="Pseudotemps", y="Densité") + theme_minimal()'
) else
'# Step-3.9 : nouvelle signature (matrice embeddings -> liste de resultats) :
# emb <- Seurat::Embeddings(obj, reduction = "umap")
# attr(emb, "reduction") <- "umap"
# res <- calculate_pseudotime(embeddings = emb, k = 15, root_cells = NULL)
# obj$pseudotime <- res$pseudotime
# ggplot(data.frame(pseudotime=obj$pseudotime, cluster=obj$seurat_clusters),
#        aes(x=pseudotime, fill=cluster)) +
#   geom_density(alpha=0.6) + scale_fill_viridis_d(option="turbo") + theme_minimal()',
'

# \u2500\u2500 8. Sauvegarde ────────────────────────────────────────────────────────────────
saveRDS(obj, paste0("sc_obj_processed_', date, '.rds"))
cat("Objet sauvegardé.\\n")
'
  )
}

# =============================================================================
# R/sc/sc_pipeline.R — SC auto-pipeline execution & R-script export
# (extracted from mod_sc.R, Block 8 refactor)
# =============================================================================
# Pure logic: called from mod_sc_server() inside observeEvent().
# Depends on: helpers_sc.R (resolve_sketch_preset, robust_find_clusters,
#             standardize_sketch_reductions, subsample_seurat_for_analysis,
#             remap_seurat_ids_to_symbol), helpers_pathway.R,
#             R/sc/sc_trajectory.R
# =============================================================================

# ── M-2 phase 1 (2026-09-30) : normalisation des paramètres + cœur QC pur ──

#' Paramètres normalisés de l'autopipeline — LE point de passage unique
#'
#' Prend une liste brute (UI convertie, scénario drive, appel direct) et rend
#' un jeu FERMÉ de 22 clés : défauts canoniques appliqués aux clés absentes,
#' clés inconnues perdues. La classe « une clé manquante change silencieusement
#' un seuil » (audit C-1) est fermée STRUCTURELLEMENT : le cœur ne voit jamais
#' de NULL. Les défauts vivent ICI (couche pure) — le canal drive délègue.
#'
#' @param p Named list (ou vide). Les reactiveValues doivent être convertis
#'   par l'appelant (`shiny::reactiveValuesToList(input)`).
#' @return Named list de 22 clés `sc_ap_*`.
sc_ap_normalize_params <- function(p = list()) {
  d <- list(
    sc_ap_mapping              = FALSE,
    sc_ap_mapping_org          = "human",
    sc_ap_bpcells              = FALSE,
    sc_ap_min_gene             = TS_SC_QC_MIN_GENES,
    sc_ap_max_gene             = TS_SC_QC_MAX_GENES,
    sc_ap_mt                   = TS_SC_QC_MAX_PCT_MT,
    sc_ap_norm                 = "log",
    sc_ap_pca_dim              = TS_SC_QC_PCA_DIMS,
    sc_ap_res                  = 0.5,
    sc_ap_cluster_algo         = "1",
    sc_ap_integration          = "none",
    sc_ap_batch_var            = "orig.ident",
    sc_ap_compute_umap         = TRUE,
    sc_ap_sketch_preset        = "max",
    sc_ap_sketch_ncells_custom = NA,
    sc_ap_singler              = FALSE,
    sc_ap_singler_ref          = "hpca",
    sc_ap_singler_level        = "main",
    sc_ap_markers              = TRUE,
    sc_ap_pathway              = FALSE,
    sc_ap_pathway_db           = "GOBP",
    sc_ap_pathway_org          = "human",
    sc_ap_correlation          = FALSE,
    sc_ap_trajectory           = TRUE
  )
  for (k in names(d)) d[[k]] <- p[[k]] %||% d[[k]]
  d
}

#' Cœur PUR de l'étape 1 (QC) de l'autopipeline — zéro Shiny
#'
#' Détection du pattern mito (humain `MT-` / souris `mt-`), calcul de
#' `percent.mt`, filtrage par seuils, garde de survie. Utilisable hors app
#' (tests, CLI) — c'est le premier pas de l'extraction du cœur pur (M-2).
#'
#' @param obj Seurat object brut.
#' @param min_genes,max_genes,max_mt Seuils QC (paramètres normalisés).
#' @param min_cells Plancher de survie (défaut 10, comportement historique).
#' @return List `object` (Seurat filtré), `n_before`, `n_removed`.
#' @raises Erreur classée `sc_pipeline_error` (message français) si moins de
#'   `min_cells` cellules survivent.
sc_pipeline_qc <- function(obj, min_genes, max_genes, max_mt, min_cells = 10) {
  mt_pat <- if (any(grepl("^MT-", rownames(obj)))) "^MT-"
            else if (any(grepl("^mt-", rownames(obj)))) "^mt-" else NULL
  obj[["percent.mt"]] <- if (!is.null(mt_pat))
    PercentageFeatureSet(obj, pattern = mt_pat) else 0
  n_before <- ncol(obj)
  # ⚠️ subset() lève sa propre erreur brute (« No cells found ») quand AUCUNE
  # cellule ne survit — convertie ici en erreur classée française (sinon la
  # garde min_cells ci-dessous n'est jamais atteinte ; trouvé par le test).
  obj <- tryCatch(
    subset(obj,
           subset = nFeature_RNA > min_genes &
                    nFeature_RNA < max_genes &
                    percent.mt   < max_mt),
    error = function(e) stop(errorCondition(sprintf(
      .tr("Seulement %d cellule(s) après QC (départ: %d). Réduisez les seuils."), 0L, n_before),
      class = "sc_pipeline_error")))
  if (ncol(obj) < min_cells) stop(errorCondition(sprintf(
    .tr("Seulement %d cellule(s) après QC (départ: %d). Réduisez les seuils."), ncol(obj), n_before), class = "sc_pipeline_error"))
  list(object = obj, n_before = n_before, n_removed = n_before - ncol(obj))
}

# Graine déclarée des étapes stochastiques (audit 2026-09-27 §1.8) — withr
# restaure l'état du RNG après chaque appel : deux exécutions identiques
# produisent les mêmes embeddings, sans fuiter la graine vers le reste de l'app.
.SC_AP_SEED <- if (exists("TS_SC_PIPELINE_SEED")) TS_SC_PIPELINE_SEED else 989L

.seeded_ap <- function(expr) {
  withr::with_seed(.SC_AP_SEED, force(expr))
}

#' Execute the SC auto-pipeline (called inside observeEvent)
#' @param input Shiny input object
#' @param global_data Global reactiveValues
#' @param shared_rv SC shared state
#' @param session Shiny session (for showNotification, log)
#' @param sc_log_rv reactiveVal for log
run_sc_auto_pipeline <- function(input, global_data, shared_rv, session, sc_log_rv) {
      # ── M-2 phase 1 (2026-09-30) : les paramètres passent par le normalisateur PUR —
      # liste fermée, défauts canoniques, clés inconnues perdues (la classe « une clé
      # manquante change silencieusement un seuil » — audit C-1 — est fermée
      # structurellement).
      params <- sc_ap_normalize_params(
        if (is.list(input)) input else shiny::reactiveValuesToList(input))
      removeModal()
      req(state_get(global_data, "sc_obj"))

      ll <- character(0)
      log_sc <- function(msg) {
        ll <<- c(ll, paste0("[", format(Sys.time(),"%H:%M:%S"), "] ", msg))
        sc_log_rv(paste(ll, collapse="\n"))
      }

      p <- shiny::Progress$new(); on.exit(p$close())

      # ── Préflight RAM (M-4, 2026-09-30) : ALERTE dans le log, JAMAIS
      # bloquante — le refus risquerait de bloquer des analyses légitimes
      # (MODE C, direction 2). Facteur à calibrer par benchmark (§2dr).
      ram_sys <- ts_system_ram_mb()
      ram_budget <- ts_ram_budget_check(
        object_bytes = as.numeric(object.size(state_get(global_data, "sc_obj"))),
        factor = TS_RAM_PREFLIGHT_FACTOR,
        available_mb = ram_sys$available_mb,
        total_mb = ram_sys$total_mb)
      if (ram_budget$level != "none") log_sc(ram_budget$message)

      tryCatch({
        obj <- state_get(global_data, "sc_obj")

        # ── Step 0: Mapping IDs ─────────────────────────────────────────────
        if (isTRUE(params$sc_ap_mapping)) {
          detected <- tryCatch(detect_gene_id_type(rownames(obj)),
                               error=function(e) "unknown")
          if (detected %in% c("ensembl","entrez")) {
            p$set(0.02,.tr("Mapping IDs...")); log_sc(sprintf(.tr("Mapping IDs (%s)..."), detected))
            map_res <- tryCatch(
              withCallingHandlers(
                remap_seurat_ids_to_symbol(obj,
                  from_type        = detected,
                  organism         = params$sc_ap_mapping_org %||% "human",
                  collapse_method  = "sum"),
                warning = function(w) { log_sc(paste("\u2139\ufe0f", conditionMessage(w))); invokeRestart("muffleWarning") }
              ),
              error=function(e) { log_sc(paste(.tr("⚠️ Mapping ignoré:"), e$message)); NULL }
            )
            if (!is.null(map_res)) {
              obj <- map_res$object
              log_sc(sprintf(.tr("✓ Mapping : %d gènes finaux (%d mappés, %d non-mappés)"),
                             nrow(obj), map_res$n_mapped, map_res$n_unmapped))
            }
          } else {
            log_sc(.tr("Mapping IDs : symboles déjà détectés (ou type inconnu) — ignoré."))
          }
        }

        # ── Step 1: QC (cœur pur extrait — M-2 phase 1) ───────────────────
        p$set(0.05,.tr("QC...")); log_sc(.tr("QC..."))
        qcr <- sc_pipeline_qc(obj,
                              min_genes = params$sc_ap_min_gene,
                              max_genes = params$sc_ap_max_gene,
                              max_mt    = params$sc_ap_mt)
        obj <- qcr$object
        log_sc(sprintf(.tr("✓ QC : %d cellules (retirées: %d)"), ncol(obj), qcr$n_removed))

        # ── Step 1b: Backend disque (BPCells) — Step-3.7A ────────────────────
        if (isTRUE(params$sc_ap_bpcells) && ncol(obj) > .BPCELLS_AUTO_THRESHOLD &&
            sc_backend_status(obj) == "memory") {
          if (!.bpcells_available()) {
            log_sc(.tr("⚠️ BPCells non installé — pipeline exécuté en RAM."))
          } else {
            conv <- tryCatch(convert_seurat_to_bpcells(obj),
                             error=function(e){ log_sc(paste(.tr("⚠️ BPCells:"), e$message)); NULL })
            if (!is.null(conv)) {
              obj <- conv$object
              if (!isTRUE(conv$already_disk)) {
                session$onSessionEnded(function() unlink(conv$dir, recursive = TRUE))
                log_sc(sprintf(.tr("✓ Backend disque (BPCells) activé — %s cellules"),
                               format(conv$n_cells, big.mark=" ")))
              }
            }
          }
        }

        # ── Step 2-5: Normalisation / PCA / Clustering / UMAP (Step-3.8A) ────
        # Sketch workflow (Seurat v5): analyse sur un sous-ensemble représentatif
        # (LeverageScore) puis projection sur le dataset complet via ProjectData().
        # Voir resolve_sketch_preset()/standardize_sketch_reductions() (helpers_sc.R).
        n_total_cells <- ncol(obj)
        sketch_params <- resolve_sketch_preset(
          params$sc_ap_sketch_preset %||% "standard", n_total_cells,
          params$sc_ap_sketch_ncells_custom)
        use_sketch <- !identical(params$sc_ap_norm, "sct") &&
                      sketch_params$ncells < n_total_cells
        pca_dim <- params$sc_ap_pca_dim  # fallback / full-dataset path
        # Intégration multi-échantillons (audit 2026-09-27 §1.3) : la
        # correction de batch s'applique au dataset COMPLET, avant clustering
        # — incompatible avec le sketch, qui est donc ignoré si demandée.
        if (!identical(input$sc_ap_integration %||% "none", "none") &&
            isTRUE(use_sketch)) {
          use_sketch <- FALSE
          log_sc(.tr("ℹ️ Intégration multi-échantillons demandée — sketch ignoré (incompatible), pipeline sur dataset complet."))
        }

        if (sc_backend_status(obj) == "disk") {
          .ap_old_plan <- future::plan()
          on.exit(future::plan(.ap_old_plan), add = TRUE)
          future::plan("sequential")
          log_sc(.tr("ℹ️ Backend disque : future séquentiel forcé (Normalisation → Clustering) pour éviter un crash 'globals size'."))
        }

        if (isTRUE(use_sketch)) {
          # ── Sketch: analyse sur sous-ensemble ────────────────────────────
          .t_sketch <- Sys.time()
          p$set(0.15,.tr("Sketch...")); log_sc(sprintf(
            .tr("Sketch : %s / %s cellules (preset '%s')..."),
            format(sketch_params$ncells, big.mark=" "), format(n_total_cells, big.mark=" "),
            params$sc_ap_sketch_preset))
          DefaultAssay(obj) <- "RNA"
          obj <- NormalizeData(obj, verbose=FALSE)
          obj <- FindVariableFeatures(obj, nfeatures=2000, verbose=FALSE)
          obj <- .seeded_ap(SketchData(object=obj, ncells=sketch_params$ncells,
                            method="LeverageScore", sketched.assay="sketch"))
          DefaultAssay(obj) <- "sketch"
          log_sc(sprintf(.tr("✓ Sketch OK (%.0fs)"), as.numeric(difftime(Sys.time(), .t_sketch, units="secs"))))

          p$set(0.30,.tr("Normalisation (sketch)...")); log_sc(.tr("Normalisation (sketch)..."))
          obj <- FindVariableFeatures(obj, nfeatures=2000, verbose=FALSE)
          obj <- ScaleData(obj, verbose=FALSE)
          log_sc(.tr("✓ Normalisation OK"))

          p$set(0.40,.tr("PCA (sketch)..."))
          obj <- RunPCA(obj, npcs=sketch_params$npcs, verbose=FALSE)
          log_sc(sprintf(.tr("✓ PCA (%d dims, sketch)"), sketch_params$npcs))

          p$set(0.55,.tr("Clustering (sketch)..."))
          obj <- FindNeighbors(obj, dims=1:sketch_params$npcs, verbose=FALSE)
          obj <- robust_find_clusters(obj, resolution=params$sc_ap_res, algo=params$sc_ap_cluster_algo,
                                      log_fn=function(m) log_sc(paste("\u26a0\ufe0f", m)))
          log_sc(sprintf(.tr("✓ Clustering sketch OK (res %.1f)"), params$sc_ap_res))

          # Step-3.8B: UMAP is the slowest step by far on large sketches --
          # skippable for fast debug iteration. When skipped, ProjectData()
          # below simply omits umap.model= (PCA-only projection); trajectory
          # (Step 9) and any live/report preview fall back to PCA automatically.
          compute_umap_sketch <- isTRUE(params$sc_ap_compute_umap)
          if (compute_umap_sketch) {
            p$set(0.63,.tr("UMAP (sketch)..."))
            obj <- .seeded_ap(RunUMAP(obj, dims=1:sketch_params$npcs, reduction="pca",
                           return.model=TRUE, verbose=FALSE))
            log_sc(.tr("✓ UMAP sketch OK"))
          } else {
            log_sc(.tr("ℹ️ UMAP désactivé (mode PCA seul, debug rapide) — previews/trajectoire utiliseront PCA."))
          }

          # ── Projection sketch → dataset complet ──────────────────────────
          .t_project <- Sys.time()
          p$set(0.68,.tr("Projection sur le dataset complet..."))
          log_sc(.tr("Projection (ProjectData) sur le dataset complet..."))
          project_args <- list(
            object=obj, assay="RNA", sketched.assay="sketch",
            sketched.reduction="pca", full.reduction="pca.full",
            dims=1:sketch_params$npcs,
            refdata=list(seurat_clusters="seurat_clusters"))
          if (compute_umap_sketch) project_args$umap.model <- "umap"
          obj <- do.call(ProjectData, project_args)
          obj <- standardize_sketch_reductions(obj, full_pca_name="pca.full")
          DefaultAssay(obj) <- "RNA"
          pca_dim <- sketch_params$npcs
          log_sc(sprintf(.tr("✓ Projection OK — %s cellules (%.0fs)"),
                         format(ncol(obj), big.mark=" "), as.numeric(difftime(Sys.time(), .t_project, units="secs"))))

        } else {
          # ── Dataset complet (comportement existant, inchangé) ────────────
          if (identical(params$sc_ap_norm, "sct"))
            log_sc(.tr("ℹ️ Sketch non supporté avec SCTransform — pipeline sur dataset complet."))
          else
            log_sc(.tr("ℹ️ Sketch ignoré : preset ≥ taille du dataset — pipeline sur dataset complet."))

          p$set(0.20,.tr("Normalisation...")); log_sc(.tr("Normalisation..."))
          if (params$sc_ap_norm=="sct") {
            obj <- SCTransform(obj, verbose=FALSE, vst.flavor="v2")
          } else {
            DefaultAssay(obj) <- "RNA"
            obj <- NormalizeData(obj, verbose=FALSE)
            obj <- FindVariableFeatures(obj, nfeatures=2000, verbose=FALSE)
            obj <- smart_scale_data(obj)   # Step-3.7A: RAM-safe (VariableFeatures only)
          }
          log_sc(.tr("✓ Normalisation OK"))

          p$set(0.40,.tr("PCA..."))
          obj <- RunPCA(obj, verbose=FALSE, npcs=pca_dim)
          log_sc(sprintf(.tr("✓ PCA (%d dims)"), pca_dim))

          # ── Step 4b: Intégration multi-échantillons — AVANT le clustering ──
          # (audit 2026-09-27 §1.3 : l'auto-pipeline n'avait AUCUNE
          # intégration — tout jeu multi-échantillons importé en un seul objet
          # était clusterisé sans correction de batch, silencieusement.)
          integ      <- input$sc_ap_integration %||% "none"
          batch_var  <- input$sc_ap_batch_var %||% "orig.ident"
          clust_red  <- "pca"
          clust_dims <- pca_dim
          if (identical(integ, "harmony")) {
            if (!requireNamespace("harmony", quietly=TRUE)) {
              log_sc(.tr("⚠️ Package 'harmony' non installé — clustering sur PCA brute."))
            } else if (!batch_var %in% colnames(obj@meta.data) ||
                       length(unique(obj@meta.data[[batch_var]])) < 2) {
              log_sc(.tr("⚠️ Harmony non appliquée : 0 ou 1 batch — clustering sur PCA brute."))
            } else {
              p$set(0.48,.tr("Harmony (intégration avant clustering)..."))
              log_sc(sprintf(.tr("Harmony (variable de batch : %s)..."), batch_var))
              obj <- .seeded_ap(RunHarmony(obj, group.by.vars=batch_var,
                                           dims.use=1:min(30, pca_dim), verbose=FALSE))
              clust_red  <- "harmony"
              clust_dims <- min(pca_dim, ncol(Seurat::Embeddings(obj, "harmony")))
              log_sc(sprintf(.tr("✓ Harmony appliquée (%d dims) — clustering sur l'espace intégré"), clust_dims))
              # Roadmap 4.3 : mélange des batchs chiffré avant/après.
              mix_msg <- tryCatch({
                before <- sc_batch_mixing_score(obj, reduction = "pca", batch_col = batch_var)
                after  <- sc_batch_mixing_score(obj, reduction = "harmony", batch_col = batch_var)
                sprintf(.tr("Mélange des batchs (0 = séparés, 1 = mélangés) : PCA brute %.0f%% → Harmony %.0f%%."),
                        100 * before$score, 100 * after$score)
              }, error = function(e) NULL)
              if (!is.null(mix_msg)) log_sc(mix_msg)
            }
          } else if (identical(integ, "sct")) {
            log_sc(.tr("⚠️ Intégration SCT-anchor non implémentée dans l'auto-pipeline — clustering sur PCA brute."))
          }

          p$set(0.55,.tr("Clustering..."))
          obj <- FindNeighbors(obj, dims=1:pca_dim, verbose=FALSE)
          obj <- robust_find_clusters(obj, resolution=params$sc_ap_res, algo=params$sc_ap_cluster_algo,
                                      log_fn=function(m) log_sc(paste("\u26a0\ufe0f", m)))
          log_sc(sprintf(.tr("✓ %d clusters (res %.1f)"), length(unique(obj$seurat_clusters)), params$sc_ap_res))

          if (isTRUE(params$sc_ap_compute_umap)) {
            p$set(0.68,.tr("UMAP..."))
            obj <- .seeded_ap(RunUMAP(obj, reduction=clust_red, dims=1:clust_dims, verbose=FALSE))
            log_sc(.tr("✓ UMAP OK"))
          } else {
            log_sc(.tr("ℹ️ UMAP désactivé (mode PCA seul, debug rapide)."))
          }
        }

        n_cl <- length(unique(obj$seurat_clusters))
        if (isTRUE(use_sketch)) log_sc(sprintf(.tr("✓ %d clusters (projetés sur dataset complet)"), n_cl))

        # ── Step 5b: t-SNE secondaire (Step-3.7) ──────────────────────────────
        # Toujours calculé (si dataset raisonnable) pour être disponible aux
        # côtés de PCA/UMAP dans le picker "Réduction à visualiser" — même
        # constante de garde que le module "1. Pipeline" (.AUTO_TSNE_MAX_CELLS).
        if (!isTRUE(params$sc_ap_compute_umap)) {
          log_sc(.tr("ℹ️ t-SNE secondaire ignoré (UMAP désactivé, mode PCA seul)."))
        } else {
          p$set(0.72,.tr("t-SNE (secondaire)..."))
          if (ncol(obj) > .AUTO_TSNE_MAX_CELLS) {
            log_sc(sprintf(.tr("⚠️ t-SNE secondaire ignoré (%s cellules > %s max)."),
                           format(ncol(obj), big.mark=" "), format(.AUTO_TSNE_MAX_CELLS, big.mark=" ")))
          } else {
            obj <- tryCatch(.seeded_ap(RunTSNE(obj, dims=1:pca_dim, verbose=FALSE)),
                            error=function(e){ log_sc(paste(.tr("⚠️ t-SNE secondaire ignoré:"), e$message)); obj })
            log_sc(.tr("✓ t-SNE secondaire OK"))
          }
        }

        # ── Step 6: SingleR (optional) ───────────────────────────────────────
        if (isTRUE(params$sc_ap_singler)) {
          if (!requireNamespace("SingleR",quietly=TRUE) ||
              !requireNamespace("celldex",quietly=TRUE)) {
            log_sc(.tr("⚠️ SingleR/celldex non installés — annotation ignorée."))
          } else {
            .t_singler <- Sys.time()
            p$set(0.76,.tr("Annotation SingleR..."))
            result <- tryCatch(
              withCallingHandlers(
                .run_singler_safe(obj, params$sc_ap_singler_ref, params$sc_ap_singler_level),
                warning=function(w) {
                  log_sc(paste(.tr("⚠️"), conditionMessage(w)))
                  invokeRestart("muffleWarning")
                }),
              error=function(e) { log_sc(paste(.tr("⚠️ SingleR:"), e$message)); NULL }
            )
            if (!is.null(result)) {
              col_name <- paste0("SingleR_", params$sc_ap_singler_ref, "_", params$sc_ap_singler_level)
              obj[[col_name]] <- result$labels
              log_sc(sprintf(.tr("✓ Annoté [%s] — %d types (%.0fs)"),
                             result$method, length(unique(result$labels)),
                             as.numeric(difftime(Sys.time(), .t_singler, units="secs"))))
            }
          }
        }

        # ── Step 7: FindAllMarkers (optional, also needed for correlation) ───
        # Step-3.7: runs on a RAM-safety-capped subsample (shared_rv$max_cells_heavy,
        # set in "1. Pipeline") — `obj` itself (UMAP/t-SNE/clusters) stays full-size.
        if (isTRUE(params$sc_ap_markers) || isTRUE(params$sc_ap_correlation)) {
          p$set(0.82,.tr("FindAllMarkers..."))
          cap_m   <- state_get(shared_rv, "max_cells_heavy") %||% Inf
          sub_res <- subsample_seurat_for_analysis(obj, max_per_group = cap_m, group_col = "seurat_clusters")
          if (sub_res$was_subsampled)
            log_sc(sprintf(.tr("ℹ️ Sous-échantillonnage marqueurs : %d → %d cellules (max %d/cluster)"),
                           sub_res$n_before, sub_res$n_after, cap_m))
          log_sc(.tr("FindAllMarkers..."))
          markers <- tryCatch({
            Idents(sub_res$object) <- sub_res$object$seurat_clusters
            FindAllMarkers(sub_res$object, only.pos=TRUE, min.pct=0.1,
                           logfc.threshold=0.25, verbose=FALSE)
          }, error=function(e) { log_sc(paste(.tr("⚠️ Markers:"), e$message)); NULL })

          if (!is.null(markers) && nrow(markers) > 0) {
            markers <- as.data.frame(markers); rownames(markers) <- NULL
            if (!"gene"       %in% colnames(markers)) markers$gene       <- rownames(markers)
            if (!"avg_log2FC" %in% colnames(markers)) markers$avg_log2FC <- markers$avg_logFC %||% 0
            # Roadmap 5.3 (audit 2026-09-27 §1.4) : ne PAS fabriquer
            # p_val_adj <- 1 — un classement arbitraire alimentait l'ORA et la
            # corrélation sans avertissement. Sans la colonne : log explicite
            # + ORA/corrélation sautés (table marqueurs conservée telle quelle).
            has_padj <- "p_val_adj" %in% colnames(markers)
            if (!has_padj) {
              log_sc(.tr("⚠️ p_val_adj absent des marqueurs — ORA et corrélation sautés (aucun classement arbitraire fabriqué)."))
              markers$p_val_adj <- NA_real_
            }
            if (!"cluster"    %in% colnames(markers)) markers$cluster    <- "Unknown"
            if (!"pct.1"      %in% colnames(markers)) markers$pct.1      <- NA_real_
            if (!"pct.2"      %in% colnames(markers)) markers$pct.2      <- NA_real_
            state_set(shared_rv, "markers_data", markers)
            log_sc(sprintf(.tr("✓ %d marqueurs"), nrow(markers)))

            # Step 7b: Pathway ORA on top markers (optional)
            if (isTRUE(params$sc_ap_pathway)) {
              .t_pathway <- Sys.time()
              log_sc(.tr("Pathway ORA..."))
              pathway_org <- params$sc_ap_pathway_org %||% "human"
              top_g_raw   <- head(markers$gene[order(markers$p_val_adj)], 100)
              # Step-3.8B: .remap_if_ensg() (mod_sc_pathways.R, globally
              # available -- sourced before this module in app.R) converts
              # ENSEMBL marker IDs to symbols before bitr(); a no-op if
              # markers are already symbols (e.g. Step 0 mapping succeeded).
              # Without this, sketch/auto-pipeline runs where mapping was
              # skipped or failed always produced "Aucun gene converti".
              top_g <- .remap_if_ensg(top_g_raw, pathway_org,
                                      notify_fn = function(msg, ...) log_sc(paste(.tr("ℹ️"), msg)))
              if (length(top_g) == 0) {
                log_sc(sprintf(.tr("⚠️ Pathway ignoré : 0/%d gènes convertibles (organisme '%s'). Exemples : %s."),
                               length(top_g_raw), pathway_org, paste(head(top_g_raw, 5), collapse=", ")))
              } else {
                pw <- tryCatch(
                  run_pathway_enrichment(top_g,
                                         organism = pathway_org,
                                         database = params$sc_ap_pathway_db %||% "GOBP",
                                         pval_cutoff = 0.05,
                                         universe = rownames(obj)),
                  error=function(e) { log_sc(paste(.tr("⚠️ Pathway:"), e$message,
                                                    .tr("— exemples testés :"),
                                                    paste(head(top_g, 5), collapse=", "))); NULL }
                )
                if (!is.null(pw) && nrow(pw) > 0) {
                  state_set(shared_rv, "pathway_results", pw)
                  state_set(shared_rv, "pathway_db", params$sc_ap_pathway_db %||% "GOBP")
                  log_sc(sprintf(.tr("✓ %d pathways (%d/%d gènes convertis, %.0fs)"), nrow(pw), length(top_g), length(top_g_raw),
                                 as.numeric(difftime(Sys.time(), .t_pathway, units="secs"))))
                }
              }
            }

          } else {
            log_sc(.tr("⚠️ Aucun marqueur trouvé."))
          }
        }

        # ── Step 8: Gene Correlation (optional) — top significant marker ─────
        # Step-3.7: also subsampled (stratified by orig.ident) with the same cap.
        if (isTRUE(params$sc_ap_correlation)) {
          p$set(0.90,.tr("Corrélation...")); log_sc(.tr("Gene Correlation..."))
          target_gene <- NULL
          markers_now <- state_get(shared_rv, "markers_data")
            if (!is.null(markers_now) && nrow(markers_now) > 0) {
            ranked      <- markers_now[order(markers_now$p_val_adj), ]
            target_gene <- ranked$gene[1]
          }
          if (is.null(target_gene)) {
            log_sc(.tr("⚠️ Corrélation ignorée : aucun marqueur disponible (cochez 'Marqueurs')."))
          } else {
            cap_c     <- state_get(shared_rv, "max_cells_heavy") %||% Inf
            sub_res_c <- subsample_seurat_for_analysis(obj, max_per_group = cap_c, group_col = "orig.ident")
            if (sub_res_c$was_subsampled)
              log_sc(sprintf(.tr("ℹ️ Sous-échantillonnage corrélation : %d → %d cellules (max %d/échantillon)"),
                             sub_res_c$n_before, sub_res_c$n_after, cap_c))
            corr_res <- tryCatch(
              find_correlated_genes(sub_res_c$object, target_gene=target_gene,
                                    method="pearson", threshold=0.3, top_n=50),
              error=function(e) { log_sc(paste(.tr("⚠️ Corrélation:"), e$message)); NULL }
            )
            if (!is.null(corr_res) && nrow(corr_res) > 0) {
              state_set(shared_rv, "correlated_genes", corr_res)
              state_set(shared_rv, "corr_target_gene", target_gene)
              log_sc(sprintf(.tr("✓ %d gènes corrélés avec %s (top marqueur)"),
                             nrow(corr_res), target_gene))
            } else {
              log_sc(sprintf(.tr("⚠️ Aucun gène corrélé pour %s (seuil |r|≥0.3)."), target_gene))
            }
          }
        }

        # ── Step 9: Trajectory (optional) ────────────────────────────────────
        # Auto-pipeline stays EXPLICITLY exploratory (weighted kNN graph):
        # Slingshot is opt-in only from the Trajectory module UI. No method
        # switch ever happens silently here.
        if (isTRUE(params$sc_ap_trajectory)) {
          p$set(0.95,.tr("Trajectoire...")); log_sc(.tr("Trajectory / Pseudotime..."))
          if (ncol(obj) > .MAX_TRAJECTORY_CELLS) {
            log_sc(sprintf(.tr("⚠️ Trajectoire ignorée : dataset trop grand (%d > %d)."),
                           ncol(obj), .MAX_TRAJECTORY_CELLS))
          } else {
            # Step-3.8B: fall back to PCA if UMAP was skipped ("PCA seul" mode)
            traj_red_use <- if ("umap" %in% names(obj@reductions)) "umap" else "pca"
            # Step-3.9: embeddings-based signature — extract the matrix, tag it
            # with the reduction name for provenance, then write the result list
            # fields back into meta.data (no Seurat object returned anymore).
            traj_embedding <- Seurat::Embeddings(obj, reduction = traj_red_use)
            attr(traj_embedding, "reduction") <- traj_red_use
            traj_res <- tryCatch(
              calculate_pseudotime(embeddings = traj_embedding, k = 15,
                                   root_cells = NULL, root_method = "diameter"),
              error=function(e) { log_sc(paste(.tr("⚠️ Trajectoire:"), e$message)); NULL }
            )
            if (!is.null(traj_res)) {
              obj@meta.data$pseudotime        <- traj_res$pseudotime
              obj@meta.data$traj_in_component <- traj_res$in_root_component
              # Provenance persistence (mirrors mod_sc_trajectory.R) so the
              # report/exports know exactly how this pseudotime was made.
              obj@meta.data$traj_method                <- rep("exploratory_knn", ncol(obj))
              obj@meta.data$traj_computation_reduction <- rep(traj_red_use, ncol(obj))
              obj@meta.data$traj_root_method           <- rep(traj_res$root_method, ncol(obj))
              obj@meta.data$traj_root_cell             <- rep(traj_res$root_cell, ncol(obj))
              obj@meta.data$traj_root_cluster          <- rep(NA_character_, ncol(obj))
              obj@meta.data$traj_root_component_size   <- rep(traj_res$root_component_size, ncol(obj))
              state_set(shared_rv, "traj_reduction", traj_red_use)
              state_set(shared_rv, "traj_method", "exploratory_knn")
              log_sc(sprintf(.tr("✓ Pseudotemps calculé (exploratoire kNN, racine auto/diamètre, réduction: %s)"), toupper(traj_red_use)))
            }
          }
        }

        # ── Commit ───────────────────────────────────────────────────────────
        state_set(global_data, "sc_obj", obj)
        state_set(shared_rv, "active_tab", "tab_viz")
        showNotification(
          sprintf(.tr("✓ Pipeline SC : %d cellules, %d clusters"), ncol(obj), n_cl),
          type="message", duration=6)

      }, error=function(e) {
        log_sc(paste(.tr("❌ Erreur:"), e$message))
        showNotification(paste(.tr("Erreur pipeline SC:"), e$message), type="error", duration=10)
        # Roadmap 5.2 (audit 2026-09-27 §1.9) : l'erreur est journalisée ET
        # notifiée, puis RELANCÉE — la classe sc_pipeline_error devient
        # observable (le verrou source qui interdisait le stop est réécrit
        # dans test-sc-pipeline.R). Les appelants UI enveloppent dans
        # tryCatch (mod_sc.R) / .sc_ap_run_drive pour ne pas casser la session.
        stop(e)
      })
}

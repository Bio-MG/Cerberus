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
#' un jeu FERMÉ de 28 clés : défauts canoniques appliqués aux clés absentes,
#' clés inconnues perdues. La classe « une clé manquante change silencieusement
#' un seuil » (audit C-1) est fermée STRUCTURELLEMENT : le cœur ne voit jamais
#' de NULL. Les défauts vivent ICI (couche pure) — le canal drive délègue.
#'
#' @param p Named list (ou vide). Les reactiveValues doivent être convertis
#'   par l'appelant (`shiny::reactiveValuesToList(input)`).
#' @return Named list de 28 clés `sc_ap_*` (M4 2026-10-26 : +4 clés doublets/ambiant).
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
    sc_ap_trajectory           = TRUE,
    # M4 (2026-10-26, jalon CERBERUS) : QC doublets & ambiant. Défauts = source
    # unique defaults.R (convention QW-2). sc_ap_doublets est OPT-IN (FALSE) :
    # l'appel moteur est PARESSEUX (Step 1c) et le cœur reste exécutable sans
    # scDblFinder ; le canal drive hérite du même jeu fermé (délégation).
    sc_ap_doublets             = FALSE,
    sc_ap_run_ambient          = FALSE,
    sc_ap_doublet_score_cutoff = TS_SC_QC_DOUBLET_SCORE_CUTOFF,
    sc_ap_max_contamination    = TS_SC_QC_MAX_CONTAMINATION
  )
  for (k in names(d)) d[[k]] <- p[[k]] %||% d[[k]]
  d
}

.sc_tr <- function(msg, ...) {
  tr_fn <- if (exists(".tr", mode = "function", inherits = TRUE)) {
    get(".tr", mode = "function", inherits = TRUE)
  } else {
    NULL
  }
  raw <- if (is.function(tr_fn)) {
    tryCatch(tr_fn(msg), error = function(e) msg)
  } else {
    msg
  }
  if (...length() > 0L) sprintf(raw, ...) else raw
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
    error = function(e) stop(errorCondition(
      .sc_tr("Seulement %d cellule(s) après QC (départ: %d). Réduisez les seuils.", 0L, n_before),
      class = "sc_pipeline_error")))
  if (ncol(obj) < min_cells) stop(errorCondition(
    .sc_tr("Seulement %d cellule(s) après QC (départ: %d). Réduisez les seuils.", ncol(obj), n_before),
    class = "sc_pipeline_error"))
  list(object = obj, n_before = n_before, n_removed = n_before - ncol(obj))
}

# Graine déclarée des étapes stochastiques (audit 2026-09-27 §1.8) — withr
# restaure l'état du RNG après chaque appel : deux exécutions identiques
# produisent les mêmes embeddings, sans fuiter la graine vers le reste de l'app.
.SC_AP_SEED <- if (exists("TS_SC_PIPELINE_SEED")) TS_SC_PIPELINE_SEED else 989L

.seeded_ap <- function(expr) {
  withr::with_seed(.SC_AP_SEED, force(expr))
}

# ── Jalon filets (2026-10-27) : filet de classification Step 2-5 ─────────────
# Les erreurs de dépendance BRUTES (Seurat/irlba/umap-rgl…) ne franchissent
# plus la frontière du cœur : converties en `sc_pipeline_error`, message
# d'origine préservé in extenso. Une condition DÉJÀ classée par le dépôt
# (`<domaine>_error` / `<domaine>_missing` — doctrine C5, erreurs M4 comme
# sc_ambient_package_missing) est RELANCÉE telle quelle (F20 : jamais de
# re-classement silencieux). Défauts mesurés : T2-QC-5 (FindClusters à haute
# résolution, « invalid first argument ») et T2-QC-6 (irlba, npcs >= cellules).
.ap_is_domain_classed <- function(e) {
  any(grepl("_(error|missing)$", class(e)))
}

.ap_classed_step <- function(expr) {
  tryCatch(
    expr,
    error = function(e) {
      if (.ap_is_domain_classed(e)) stop(e)
      stop(errorCondition(
        .sc_tr("Échec de l'autopipeline SC (normalisation/PCA/clustering) : %s",
               conditionMessage(e)),
        class = "sc_pipeline_error"))
    }
  )
}

#' Pure headless Single-Cell auto-pipeline core
#'
#' @param obj Seurat object (validated via assert_seurat)
#' @param params Named list of parameters (flexible aliases supported)
#' @param log_file Optional path to log file for inter-process progress streaming
#' @return list(obj = obj, metrics = metrics, timings = timings, log = log)
run_sc_auto_pipeline_core <- function(obj, params = list(), log_file = NULL) {
  # 1. Input assertions
  assert_seurat(obj, context = "run_sc_auto_pipeline_core")

  t_start <- Sys.time()
  step_times <- list(
    mapping_s    = 0,
    qc_s         = 0,
    doublets_s   = 0,
    norm_s       = 0,
    pca_s        = 0,
    harmony_s    = 0,
    clustering_s = 0,
    umap_s       = 0,
    tsne_s       = 0,
    markers_s    = 0,
    pathway_s    = 0,
    correlation_s= 0,
    trajectory_s = 0,
    total_s      = 0
  )
  log_lines <- character(0)
  log_fn <- function(msg) {
    line <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg)
    log_lines <<- c(log_lines, line)
    if (!is.null(log_file) && is.character(log_file) && nzchar(log_file)) {
      tryCatch(cat(line, "\n", file = log_file, append = TRUE), error = function(e) NULL)
    }
  }

  # 2. Extract parameters with alias resolution
  p_norm <- sc_ap_normalize_params(params)

  pca_dim      <- as.integer(params$n_dims %||% params$dims %||% params$pca_dim %||% p_norm$sc_ap_pca_dim %||% 30L)
  resolution   <- as.numeric(params$resolution %||% params$res %||% params$clust_res %||% p_norm$sc_ap_res %||% 0.5)
  cluster_algo <- as.character(params$cluster_algo %||% params$algorithm %||% p_norm$sc_ap_cluster_algo %||% "louvain")
  norm_method  <- as.character(params$norm_method %||% params$norm %||% p_norm$sc_ap_norm %||% "log")
  nfeatures    <- as.integer(params$nfeatures %||% params$n_features %||% 2000L)
  assay        <- as.character(params$assay %||% Seurat::DefaultAssay(obj) %||% "RNA")
  integration  <- as.character(params$integration %||% params$sc_ap_integration %||% p_norm$sc_ap_integration %||% "none")
  batch_var    <- as.character(params$batch_var %||% params$sc_ap_batch_var %||% p_norm$sc_ap_batch_var %||% "orig.ident")
  compute_umap <- isTRUE(params$compute_umap %||% p_norm$sc_ap_compute_umap)
  compute_tsne <- if (!is.null(params$compute_tsne)) isTRUE(params$compute_tsne) else compute_umap
  run_qc       <- if (!is.null(params$run_qc)) isTRUE(params$run_qc) else if (!is.null(params$skip_qc)) !isTRUE(params$skip_qc) else TRUE
  seed         <- as.integer(params$seed %||% .SC_AP_SEED %||% 989L)

  n_cells_initial <- ncol(obj)
  n_genes_initial <- nrow(obj)
  qc_removed <- 0L
  qc_snapshot <- NULL

  # ── Step 0: ID Mapping (optional) ──────────────────────────────────────────
  if (isTRUE(p_norm$sc_ap_mapping)) {
    t0 <- Sys.time()
    detected <- tryCatch(detect_gene_id_type(rownames(obj)), error = function(e) "unknown")
    if (detected %in% c("ensembl", "entrez")) {
      log_fn(sprintf("Mapping IDs (%s)...", detected))
      map_res <- tryCatch(
        remap_seurat_ids_to_symbol(obj, from_type = detected,
                                   organism = p_norm$sc_ap_mapping_org %||% "human",
                                   collapse_method = "sum"),
        error = function(e) { log_fn(paste("Mapping ignoré:", e$message)); NULL }
      )
      if (!is.null(map_res)) {
        obj <- map_res$object
        log_fn(sprintf("✓ Mapping : %d gènes finaux", nrow(obj)))
      }
    } else {
      log_fn("Mapping IDs : symboles déjà détectés (ou type inconnu) — ignoré.")
    }
    step_times$mapping_s <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  }

  # ── Step 1: QC ─────────────────────────────────────────────────────────────
  if (isTRUE(run_qc)) {
    t_qc <- Sys.time()
    log_fn("QC...")
    qcr <- sc_pipeline_qc(obj,
                          min_genes = p_norm$sc_ap_min_gene,
                          max_genes = p_norm$sc_ap_max_gene,
                          max_mt    = p_norm$sc_ap_mt,
                          min_cells = as.integer(params$min_cells %||% 10L))
    obj <- qcr$object
    qc_removed <- qcr$n_removed
    qc_snapshot <- list(min_genes = p_norm$sc_ap_min_gene, max_genes = p_norm$sc_ap_max_gene,
                        max_mt = p_norm$sc_ap_mt, n_removed = qc_removed, n_remaining = ncol(obj))
    log_fn(sprintf("✓ QC : %d cellules (retirées: %d)", ncol(obj), qc_removed))
    step_times$qc_s <- as.numeric(difftime(Sys.time(), t_qc, units = "secs"))
  } else {
    log_fn("QC ignoré (matrice pré-filtrée fournie).")
    step_times$qc_s <- 0
  }

  # ── Step 1c (M4, 2026-10-26) : QC doublets & ambiant — appel PARESSEUX ──────
  # scDblFinder est OPTIONNEL : le cœur doit rester exécutable sans le package
  # (log + skip quand le moteur n'est pas résident). Quand le moteur tourne,
  # ses erreurs classées (ex. sc_ambient_package_missing si run_ambient sans
  # SoupX/celda) se propagent TELLES QUELLES — jamais de repli silencieux
  # (feature 20). L'appel est AVANT la conversion BPCells (Step 1b) : la
  # détection exige des counts en mémoire. Provenance + distributions
  # rejoignent la liste de retour (consommables par le rapport).
  m4_provenance <- NULL
  m4_distributions <- NULL
  if (isTRUE(p_norm$sc_ap_doublets)) {
    if (exists("sc_filter_doublets_and_ambient", mode = "function")) {
      t_m4 <- Sys.time()
      log_fn("QC doublets & ambiant (scDblFinder)...")
      m4_res <- sc_filter_doublets_and_ambient(obj, params = list(
        seed = seed,
        batch_var = batch_var,
        doublet_score_cutoff = p_norm$sc_ap_doublet_score_cutoff,
        max_contamination = p_norm$sc_ap_max_contamination,
        run_ambient = isTRUE(p_norm$sc_ap_run_ambient)
      ))
      obj <- m4_res$obj
      m4_provenance <- m4_res$provenance
      m4_distributions <- m4_res$qc_metrics$distributions
      log_fn(sprintf("✓ QC doublets : %d cellules conservées (exclues: %d)",
                     ncol(obj),
                     length(as.character(m4_res$provenance$cells_excluded))))
      step_times$doublets_s <- as.numeric(difftime(Sys.time(), t_m4, units = "secs"))
    } else {
      log_fn("QC doublets ignoré : moteur sc_filter_doublets_and_ambient indisponible (scDblFinder optionnel).")
      step_times$doublets_s <- 0
    }
  }

  # ── Step 1b: Backend disque (BPCells) ──────────────────────────────────────
  if (isTRUE(p_norm$sc_ap_bpcells) && ncol(obj) > 10000L &&
      exists("sc_backend_status", mode = "function") && sc_backend_status(obj) == "memory") {
    if (exists(".bpcells_available", mode = "function") && .bpcells_available() &&
        exists("convert_seurat_to_bpcells", mode = "function")) {
      conv <- tryCatch(convert_seurat_to_bpcells(obj), error = function(e) NULL)
      if (!is.null(conv) && !is.null(conv$object)) {
        obj <- conv$object
        log_fn(sprintf("✓ Backend disque (BPCells) activé — %s cellules", format(ncol(obj), big.mark = " ")))
      }
    }
  }

  # ── Step 2-5: Normalisation / PCA / Clustering / UMAP ───────────────────────
  t_norm <- Sys.time()
  n_total_cells <- ncol(obj)
  sketch_params <- resolve_sketch_preset(
    p_norm$sc_ap_sketch_preset %||% "standard", n_total_cells,
    p_norm$sc_ap_sketch_ncells_custom)
  use_sketch <- !identical(norm_method, "sct") &&
                sketch_params$ncells < n_total_cells &&
                identical(integration, "none")

  # ── Garde de dimension (jalon filets, 2026-10-27) ──────────────────────────
  # T2-QC-6 (M5) : 12 cellules < pca_dim (20) faisaient sortir l'erreur irlba
  # brute (« max(nu, nv) must be strictly less than min(nrow(A), ncol(A)) »).
  # Refus CLASSÉ et actionnable avant tout calcul : les deux knobs nommés.
  if (!isTRUE(use_sketch) && n_total_cells <= pca_dim) {
    stop(errorCondition(
      .sc_tr("Pas assez de cellules (%d) pour %d dimensions PCA (pca_dim). Fournissez plus de cellules ou réduisez pca_dim.",
             n_total_cells, pca_dim),
      class = "sc_pipeline_error"))
  }

  if (exists("sc_backend_status", mode = "function") && sc_backend_status(obj) == "disk") {
    .ap_old_plan <- future::plan()
    on.exit(future::plan(.ap_old_plan), add = TRUE)
    future::plan("sequential")
    log_fn("Backend disque : future séquentiel forcé.")
  }

  clust_dims <- pca_dim
  clust_red <- "pca"
  mixing_before <- NULL
  mixing_after <- NULL

  # Filet de classification autour de TOUTE la branche sketch/complete (le
  # block est évalué dans le frame de l'appelant — même mécanisme que
  # .seeded_ap) : voir .ap_classed_step ci-dessus.
  .ap_classed_step({
  if (isTRUE(use_sketch)) {
    # ── Sketch Branch ────────────────────────────────────────────────────────
    log_fn(sprintf("Sketch : %s / %s cellules...", format(sketch_params$ncells, big.mark=" "), format(n_total_cells, big.mark=" ")))
    Seurat::DefaultAssay(obj) <- assay
    obj <- Seurat::NormalizeData(obj, verbose = FALSE)
    obj <- Seurat::FindVariableFeatures(obj, nfeatures = nfeatures, verbose = FALSE)
    obj <- withr::with_seed(seed, Seurat::SketchData(object = obj, ncells = sketch_params$ncells,
                                                    method = "LeverageScore", sketched.assay = "sketch"))
    Seurat::DefaultAssay(obj) <- "sketch"
    obj <- Seurat::FindVariableFeatures(obj, nfeatures = nfeatures, verbose = FALSE)
    obj <- smart_scale_data(obj)
    step_times$norm_s <- as.numeric(difftime(Sys.time(), t_norm, units = "secs"))

    t_pca <- Sys.time()
    obj <- Seurat::RunPCA(obj, npcs = sketch_params$npcs, verbose = FALSE)
    step_times$pca_s <- as.numeric(difftime(Sys.time(), t_pca, units = "secs"))
    log_fn(sprintf("✓ PCA (%d dims, sketch)", sketch_params$npcs))

    t_clust <- Sys.time()
    obj <- Seurat::FindNeighbors(obj, dims = 1:sketch_params$npcs, verbose = FALSE)
    obj <- robust_find_clusters(obj, resolution = resolution, algo = cluster_algo, log_fn = log_fn)
    step_times$clustering_s <- as.numeric(difftime(Sys.time(), t_clust, units = "secs"))
    log_fn(sprintf("✓ Clustering sketch OK (res %.1f)", resolution))

    if (isTRUE(compute_umap)) {
      t_umap <- Sys.time()
      obj <- withr::with_seed(seed, Seurat::RunUMAP(obj, dims = 1:sketch_params$npcs, reduction = "pca",
                                                   return.model = TRUE, verbose = FALSE))
      step_times$umap_s <- as.numeric(difftime(Sys.time(), t_umap, units = "secs"))
      log_fn("✓ UMAP sketch OK")
    }

    t_proj <- Sys.time()
    project_args <- list(
      object = obj, assay = assay, sketched.assay = "sketch",
      sketched.reduction = "pca", full.reduction = "pca.full",
      dims = 1:sketch_params$npcs,
      refdata = list(seurat_clusters = "seurat_clusters")
    )
    if (isTRUE(compute_umap)) project_args$umap.model <- "umap"
    obj <- do.call(Seurat::ProjectData, project_args)
    obj <- standardize_sketch_reductions(obj, full_pca_name = "pca.full")
    Seurat::DefaultAssay(obj) <- assay
    clust_red <- "pca"
    clust_dims <- sketch_params$npcs
    log_fn(sprintf("✓ Projection OK — %s cellules", format(ncol(obj), big.mark = " ")))

  } else {
    # ── Full Dataset Branch ──────────────────────────────────────────────────
    log_fn("Normalisation...")
    if (identical(norm_method, "sct")) {
      obj <- Seurat::SCTransform(obj, verbose = FALSE, variable.features.n = nfeatures, vst.flavor = "v2")
    } else {
      Seurat::DefaultAssay(obj) <- assay
      obj <- Seurat::NormalizeData(obj, verbose = FALSE)
      obj <- Seurat::FindVariableFeatures(obj, nfeatures = nfeatures, verbose = FALSE)
      obj <- smart_scale_data(obj)
    }
    step_times$norm_s <- as.numeric(difftime(Sys.time(), t_norm, units = "secs"))
    log_fn("✓ Normalisation OK")

    t_pca <- Sys.time()
    obj <- Seurat::RunPCA(obj, verbose = FALSE, npcs = pca_dim)
    step_times$pca_s <- as.numeric(difftime(Sys.time(), t_pca, units = "secs"))
    log_fn(sprintf("✓ PCA (%d dims)", pca_dim))

    # Harmony Integration
    if (identical(integration, "harmony")) {
      t_harm <- Sys.time()
      if (requireNamespace("harmony", quietly = TRUE) &&
          batch_var %in% colnames(obj@meta.data) &&
          length(unique(obj@meta.data[[batch_var]])) >= 2) {
        log_fn(sprintf("Harmony (variable de batch : %s)...", batch_var))
        obj <- withr::with_seed(seed, harmony::RunHarmony(obj, group.by.vars = batch_var,
                                                          dims.use = 1:min(30, pca_dim), verbose = FALSE))
        clust_red <- "harmony"
        clust_dims <- min(pca_dim, ncol(Seurat::Embeddings(obj, "harmony")))
        mixing_before <- tryCatch(sc_batch_mixing_score(obj, reduction = "pca", batch_col = batch_var)$score, error = function(e) NULL)
        mixing_after  <- tryCatch(sc_batch_mixing_score(obj, reduction = "harmony", batch_col = batch_var)$score, error = function(e) NULL)
        log_fn(sprintf("✓ Harmony appliquée (%d dims)", clust_dims))
      } else {
        log_fn("Harmony non appliquée : conditions non remplies.")
      }
      step_times$harmony_s <- as.numeric(difftime(Sys.time(), t_harm, units = "secs"))
    }

    t_clust <- Sys.time()
    obj <- Seurat::FindNeighbors(obj, dims = 1:clust_dims, reduction = clust_red, verbose = FALSE)
    obj <- robust_find_clusters(obj, resolution = resolution, algo = cluster_algo, log_fn = log_fn)
    step_times$clustering_s <- as.numeric(difftime(Sys.time(), t_clust, units = "secs"))
    log_fn(sprintf("✓ %d clusters (res %.1f)", length(unique(obj$seurat_clusters)), resolution))

    if (isTRUE(compute_umap)) {
      t_umap <- Sys.time()
      obj <- withr::with_seed(seed, Seurat::RunUMAP(obj, reduction = clust_red, dims = 1:clust_dims, verbose = FALSE))
      step_times$umap_s <- as.numeric(difftime(Sys.time(), t_umap, units = "secs"))
      log_fn("✓ UMAP OK")
    }

    if (isTRUE(compute_tsne)) {
      t_tsne <- Sys.time()
      max_tsne_cells <- if (exists(".AUTO_TSNE_MAX_CELLS")) .AUTO_TSNE_MAX_CELLS else 30000L
      if (ncol(obj) <= max_tsne_cells) {
        obj <- tryCatch(withr::with_seed(seed, Seurat::RunTSNE(obj, dims = 1:pca_dim, verbose = FALSE)),
                        error = function(e) { log_fn(paste("t-SNE ignoré:", e$message)); obj })
        log_fn("✓ t-SNE secondaire OK")
      } else {
        log_fn(sprintf("t-SNE secondaire ignoré (%d > %d cellules).", ncol(obj), max_tsne_cells))
      }
      step_times$tsne_s <- as.numeric(difftime(Sys.time(), t_tsne, units = "secs"))
    }
  }
  })

  n_cl <- length(unique(obj$seurat_clusters))

  # ── Step 6: SingleR (optional) ─────────────────────────────────────────────
  if (isTRUE(p_norm$sc_ap_singler)) {
    if (requireNamespace("SingleR", quietly = TRUE) && requireNamespace("celldex", quietly = TRUE) &&
        exists(".run_singler_safe", mode = "function")) {
      t_singler <- Sys.time()
      log_fn("Annotation SingleR...")
      result <- tryCatch(
        .run_singler_safe(obj, p_norm$sc_ap_singler_ref, p_norm$sc_ap_singler_level),
        error = function(e) { log_fn(paste("SingleR:", e$message)); NULL }
      )
      if (!is.null(result)) {
        col_name <- paste0("SingleR_", p_norm$sc_ap_singler_ref, "_", p_norm$sc_ap_singler_level)
        obj[[col_name]] <- result$labels
        log_fn(sprintf("✓ Annoté [%s] — %d types", result$method, length(unique(result$labels))))
      }
    } else {
      log_fn("SingleR/celldex non installés ou non disponibles — annotation ignorée.")
    }
  }

  # ── Step 7: FindAllMarkers & Pathways (optional) ───────────────────────────
  markers <- NULL
  pw <- NULL
  if (isTRUE(p_norm$sc_ap_markers) || isTRUE(p_norm$sc_ap_correlation)) {
    t_mark <- Sys.time()
    log_fn("FindAllMarkers...")
    cap_m <- as.integer(params$max_cells_heavy %||% 500L)
    sub_res <- subsample_seurat_for_analysis(obj, max_per_group = cap_m, group_col = "seurat_clusters")
    markers <- tryCatch({
      Seurat::Idents(sub_res$object) <- sub_res$object$seurat_clusters
      Seurat::FindAllMarkers(sub_res$object, only.pos = TRUE, min.pct = 0.1,
                             logfc.threshold = 0.25, verbose = FALSE)
    }, error = function(e) { log_fn(paste("Markers:", e$message)); NULL })

    if (!is.null(markers) && nrow(markers) > 0) {
      markers <- as.data.frame(markers)
      rownames(markers) <- NULL
      if (!"gene" %in% colnames(markers)) markers$gene <- rownames(markers)
      if (!"avg_log2FC" %in% colnames(markers)) markers$avg_log2FC <- markers$avg_logFC %||% 0
      has_padj <- "p_val_adj" %in% colnames(markers)
      if (!has_padj) {
        markers$p_val_adj <- NA_real_
      }
      if (!"cluster" %in% colnames(markers)) markers$cluster <- "Unknown"
      if (!"pct.1" %in% colnames(markers)) markers$pct.1 <- NA_real_
      if (!"pct.2" %in% colnames(markers)) markers$pct.2 <- NA_real_
      log_fn(sprintf("✓ %d marqueurs", nrow(markers)))
      step_times$markers_s <- as.numeric(difftime(Sys.time(), t_mark, units = "secs"))

      # Pathway ORA
      if (isTRUE(p_norm$sc_ap_pathway)) {
        t_pw <- Sys.time()
        log_fn("Pathway ORA...")
        pathway_org <- p_norm$sc_ap_pathway_org %||% "human"
        top_g_raw <- head(markers$gene[order(markers$p_val_adj)], 100)
        top_g <- if (exists(".remap_if_ensg", mode = "function")) {
          .remap_if_ensg(top_g_raw, pathway_org, notify_fn = function(m, ...) log_fn(m))
        } else {
          top_g_raw
        }
        if (length(top_g) > 0) {
          pw <- tryCatch(
            run_pathway_enrichment(top_g, organism = pathway_org,
                                   database = p_norm$sc_ap_pathway_db %||% "GOBP",
                                   pval_cutoff = 0.05,
                                   universe = rownames(obj)),
            error = function(e) { log_fn(paste("Pathway:", e$message)); NULL }
          )
          if (!is.null(pw) && nrow(pw) > 0) {
            log_fn(sprintf("✓ %d pathways", nrow(pw)))
          }
        }
        step_times$pathway_s <- as.numeric(difftime(Sys.time(), t_pw, units = "secs"))
      }
    } else {
      log_fn("Aucun marqueur trouvé.")
    }
  }

  # ── Step 8: Gene Correlation (optional) ────────────────────────────────────
  corr_res <- NULL
  if (isTRUE(p_norm$sc_ap_correlation)) {
    t_corr <- Sys.time()
    target_gene <- NULL
    if (!is.null(markers) && nrow(markers) > 0 && any(!is.na(markers$p_val_adj))) {
      ranked <- markers[order(markers$p_val_adj), ]
      target_gene <- ranked$gene[1]
    }
    if (!is.null(target_gene)) {
      log_fn(sprintf("Corrélation avec %s...", target_gene))
      cap_c <- as.integer(params$max_cells_heavy %||% 500L)
      sub_res_c <- subsample_seurat_for_analysis(obj, max_per_group = cap_c, group_col = "orig.ident")
      corr_res <- tryCatch(
        find_correlated_genes(sub_res_c$object, target_gene = target_gene,
                              method = "pearson", threshold = 0.3, top_n = 50),
        error = function(e) { log_fn(paste("Corrélation:", e$message)); NULL }
      )
      if (!is.null(corr_res) && nrow(corr_res) > 0) {
        log_fn(sprintf("✓ %d gènes corrélés avec %s", nrow(corr_res), target_gene))
      }
    }
    step_times$correlation_s <- as.numeric(difftime(Sys.time(), t_corr, units = "secs"))
  }

  # ── Step 9: Trajectory (optional) ──────────────────────────────────────────
  if (isTRUE(p_norm$sc_ap_trajectory)) {
    t_traj <- Sys.time()
    max_traj_cells <- if (exists(".MAX_TRAJECTORY_CELLS")) .MAX_TRAJECTORY_CELLS else 50000L
    if (ncol(obj) <= max_traj_cells) {
      log_fn("Trajectoire...")
      traj_red_use <- if ("umap" %in% names(obj@reductions)) "umap" else "pca"
      traj_embedding <- Seurat::Embeddings(obj, reduction = traj_red_use)
      attr(traj_embedding, "reduction") <- traj_red_use
      traj_res <- tryCatch(
        calculate_pseudotime(embeddings = traj_embedding, k = 15,
                             root_cells = NULL, root_method = "diameter"),
        error = function(e) { log_fn(paste("Trajectoire:", e$message)); NULL }
      )
      if (!is.null(traj_res)) {
        obj@meta.data$pseudotime        <- traj_res$pseudotime
        obj@meta.data$traj_in_component <- traj_res$in_root_component
        obj@meta.data$traj_method                <- rep("exploratory_knn", ncol(obj))
        obj@meta.data$traj_computation_reduction <- rep(traj_red_use, ncol(obj))
        obj@meta.data$traj_root_method           <- rep(traj_res$root_method, ncol(obj))
        obj@meta.data$traj_root_cell             <- rep(traj_res$root_cell, ncol(obj))
        obj@meta.data$traj_root_cluster          <- rep(NA_character_, ncol(obj))
        obj@meta.data$traj_root_component_size   <- rep(traj_res$root_component_size, ncol(obj))
        log_fn(sprintf("✓ Pseudotemps calculé (exploratoire kNN, racine auto/diamètre, réduction: %s)", toupper(traj_red_use)))
      }
    } else {
      log_fn(sprintf("Trajectoire ignorée (%d > %d cellules).", ncol(obj), max_traj_cells))
    }
    step_times$trajectory_s <- as.numeric(difftime(Sys.time(), t_traj, units = "secs"))
  }

  t_end <- Sys.time()
  step_times$total_s <- as.numeric(difftime(t_end, t_start, units = "secs"))

  # 3. Structured return
  metrics <- list(
    n_cells_initial     = n_cells_initial,
    n_cells_final       = ncol(obj),
    n_genes_initial     = n_genes_initial,
    n_genes_final       = nrow(obj),
    n_clusters          = n_cl,
    pca_dims            = clust_dims,
    resolution          = resolution,
    reductions          = names(obj@reductions),
    integration         = integration,
    batch_mixing_before = mixing_before,
    batch_mixing_after  = mixing_after,
    qc_removed          = qc_removed
  )

  list(
    obj              = obj,
    metrics          = metrics,
    timings          = step_times,
    log              = log_lines,
    markers_data     = markers,
    pathway_results  = pw,
    correlated_genes = corr_res,
    qc_snapshot      = qc_snapshot,
    doublets_provenance = m4_provenance,   # M4 : entrée new_provenance_entry (NULL si step désactivé)
    qc_distributions    = m4_distributions # M4 : distributions nCount/nFeature avant/après (NULL si step désactivé)
  )
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
        p$set(0.1, .tr("Calcul en cours..."))

        res <- run_sc_auto_pipeline_core(obj, params)

        for (l in res$log) {
          log_sc(sub("^\\[[0-9:]+\\]\\s*", "", l))
        }

        obj <- res$obj
        state_set(global_data, "sc_obj", obj)
        if (!is.null(res$markers_data)) state_set(shared_rv, "markers_data", res$markers_data)
        if (!is.null(res$pathway_results)) {
          state_set(shared_rv, "pathway_results", res$pathway_results)
          state_set(shared_rv, "pathway_db", params$sc_ap_pathway_db %||% "GOBP")
        }
        if (!is.null(res$correlated_genes)) {
          state_set(shared_rv, "correlated_genes", res$correlated_genes)
          if (!is.null(res$markers_data) && nrow(res$markers_data) > 0) {
            state_set(shared_rv, "corr_target_gene", res$markers_data$gene[1])
          }
        }
        if ("pseudotime" %in% colnames(obj@meta.data)) {
          traj_red_use <- if ("umap" %in% names(obj@reductions)) "umap" else "pca"
          state_set(shared_rv, "traj_reduction", traj_red_use)
          state_set(shared_rv, "traj_method", "exploratory_knn")
        }

        state_set(shared_rv, "active_tab", "tab_viz")
        showNotification(
          sprintf(.tr("✓ Pipeline SC : %d cellules, %d clusters"), ncol(obj), res$metrics$n_clusters),
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

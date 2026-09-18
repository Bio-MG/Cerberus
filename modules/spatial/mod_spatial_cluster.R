# =============================================================================
# modules/spatial/mod_spatial_cluster.R — Spatial clustering ("BANKSY-lite")
# =============================================================================
# v2 (Phase 3 — regional differential expression): added FindMarkers/
# FindAllMarkers "par cluster" (vignette equivalent: FindMarkers() between
# regions/clusters). Runs in its own mirai daemon, reopening the BPCells
# matrix from disk and subsetting to the SAME (bpcells column) ids the
# clustering itself used (pass_idx) — deliberately NOT run on the sketch:
# the sketch is an INDEPENDENT random subsample of the full dataset, so its
# ids only partially overlap with shared_rv$cluster_labels (whose names are
# full-resolution bpcells ids) -- reusing bpcells_dir+pass_idx guarantees
# perfect alignment between expression data and cluster identity, same
# pattern as moran_task/deconv_task below/elsewhere in this module family.
#
# v3 (moyen terme — export/auto-pipeline, voir handoff_spatial_bio-mg.md) :
#    shared_rv$cluster_params ecrit au moment du clic sur "Lancer Clustering
#    Spatial" (miroir des parametres UI utilises) -- purement additif, lu
#    uniquement par mod_spatial_export.R (script R reproductible) ; zero
#    changement de comportement pour cet onglet.
#
# REWRITE (post-test-3): SeuratWrappers::RunBanksy() / Banksy::computeHarmonics()
# tries to spawn nested parallel worker processes from inside the mirai
# daemon (parallel::makeCluster-style) -- fragile in general, and observed to
# hang indefinitely on a Windows project path containing spaces/brackets.
# Rather than keep chasing Banksy/SeuratWrappers version compatibility, this
# reimplements BANKSY's core idea directly, with zero nested parallelism:
#   1. k-NN in PHYSICAL (x,y) space (RANN::nn2 — single-threaded C++, no
#      cluster spawning).
#   2. For each spot, average its neighbors' (log-normalized) expression —
#      done as ONE sparse-matrix multiply (same "indicator matrix" trick
#      already used elsewhere in this project, e.g. helpers_sc.R's
#      remap_seurat_ids_to_symbol()), not a per-row loop.
#   3. Augmented feature matrix = [sqrt(1-lambda)*scale(own),
#      sqrt(lambda)*scale(neighbor_mean)] — BANKSY's own augmentation
#      formula (own expression vs. neighborhood mean, weighted by lambda),
#      minus the optional azimuthal-Gabor-filter term (a refinement, not the
#      core mechanism).
#   4. PCA (irlba, truncated/fast) on the augmented matrix, then Seurat's
#      OWN FindNeighbors()/FindClusters(algorithm=4, Leiden) — mature,
#      already used successfully elsewhere in this app (mod_sc_pipeline.R),
#      no external nested-parallel dependency.
#
# Install: install.packages(c("RANN", "irlba")) — both lightweight, no
# GitHub-only / version-fragile dependency chain anymore.
#
# Entirely async: the daemon reopens the BPCells matrix from disk and
# receives only the small `coords` data.frame + scalar parameters — never
# the Seurat/BPCells object itself (hard rule). A MIRAI_TASK_TIMEOUT_MS
# ceiling (R/utils_spatial_async.R) guarantees this never hangs forever.
# =============================================================================

mod_spatial_cluster_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = i18n$t("Clustering spatial (BANKSY-lite)"), width = 350,

      div(class = "alert alert-light", style = "font-size:0.8rem;",
          bsicons::bs_icon("info-circle"),
          " ",
          i18n$t("Augmente l'expression de chaque spot/cellule avec la moyenne de son voisinage spatial avant le clustering — utile pour detecter des domaines tissulaires, pas seulement des types cellulaires.")),

      sliderInput(ns("lambda"), i18n$t("Lambda (poids spatial)"), 0, 1, 0.8, step = 0.05),
      numericInput(ns("k_geom"), i18n$t("Voisins geometriques (k_geom)"), 18, min = 4, max = 100, step = 1),
      numericInput(ns("npcs"), i18n$t("Composantes PCA"), 30, min = 5, max = 100, step = 5),
      numericInput(ns("resolution"), i18n$t("Resolution (Leiden)"), 0.8, min = 0.1, max = 3, step = 0.1),

      div(class = "alert alert-light", style = "font-size:0.75rem;",
          i18n$t("Le clustering s'applique aux elements passant les seuils QC (onglet 1) — reimportez ou ajustez les seuils si necessaire.")),

      bslib::input_task_button(ns("btn_cluster"), i18n$t("Lancer Clustering Spatial"),
                                icon = icon("shapes")),
      verbatimTextOutput(ns("cluster_progress_text"), placeholder = TRUE),

      hr(),
      h6(i18n$t("Marqueurs differentiels par cluster"), style = "font-weight:bold;"),
      div(class = "alert alert-light", style = "font-size:0.75rem;",
          bsicons::bs_icon("cpu"),
          " ",
          i18n$t("Equivalent de FindMarkers() applique a chaque cluster contre tous les autres (test de Wilcoxon) — necessite un clustering deja calcule ci-dessus. Asynchrone (mirai), sur les memes elements (QC-filtres) que le clustering.")),
      numericInput(ns("logfc_threshold"), i18n$t("Seuil log2FC minimum"), 0.25, min = 0, max = 5, step = 0.05),
      numericInput(ns("min_pct"), i18n$t("% cellules exprimant le gene (min)"), 0.1, min = 0, max = 1, step = 0.05),
      bslib::input_task_button(ns("btn_find_markers"), i18n$t("Rechercher les marqueurs"),
                                icon = icon("magnifying-glass-chart")),
      verbatimTextOutput(ns("markers_progress_text"), placeholder = TRUE)
    ),

    navset_card_underline(
      nav_panel(i18n$t("Resume clustering"),
                uiOutput(ns("cluster_summary")),
                DT::DTOutput(ns("cluster_sizes_table"))),
      nav_panel(i18n$t("Marqueurs par cluster (FindMarkers)"),
                div(class = "alert alert-light small mb-2",
                    i18n$t("Top marqueurs (triés par log2FC decroissant, p-adj < 0.05) par cluster — utilisez ces genes pour interpreter biologiquement chaque domaine spatial.")),
                card(full_screen = TRUE, DT::DTOutput(ns("markers_table"))))
    )
  )
}

mod_spatial_cluster_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Session-scoped scalar translation (plain strings, never HTML spans).
    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # i18n: push translated labels on every language change (values never
    # change; task buttons & static HTML switch via the client-side JS shim,
    # same convention as mod_spatial_qc.R).
    observeEvent(global_data$language, {
      updateSliderInput(session, "lambda",
                        label = .tr("Lambda (poids spatial)"))
      updateNumericInput(session, "k_geom",
                         label = .tr("Voisins geometriques (k_geom)"))
      updateNumericInput(session, "npcs", label = .tr("Composantes PCA"))
      updateNumericInput(session, "resolution", label = .tr("Resolution (Leiden)"))
      updateNumericInput(session, "logfc_threshold",
                         label = .tr("Seuil log2FC minimum"))
      updateNumericInput(session, "min_pct",
                         label = .tr("% cellules exprimant le gene (min)"))
    }, ignoreInit = TRUE)

    log_file <- spatial_log_path(session, "cluster")
    tracker  <- create_reactive_tracker(session, log_file)

    cluster_task <- ExtendedTask$new(function(bpcells_dir, pass_idx, coords,
                                               lambda, k_geom, npcs, resolution, log_file) {
      mirai::mirai(
        {
          if (!requireNamespace("RANN", quietly = TRUE)) {
            stop(errorCondition("Package 'RANN' requis (install.packages('RANN')).",
                                class = "spatial_cluster_error"))
          }

          write_mirai_log(log_file, "Ouverture de la matrice BPCells...", 1, 8)
          mat <- BPCells::open_matrix_dir(bpcells_dir)
          if (!is.null(pass_idx)) mat <- mat[, pass_idx, drop = FALSE]

          write_mirai_log(log_file, "Normalisation + selection des HVG...", 2, 8)
          obj <- Seurat::CreateSeuratObject(counts = mat)
          obj <- Seurat::NormalizeData(obj, verbose = FALSE)
          obj <- Seurat::FindVariableFeatures(obj, verbose = FALSE)
          var_feat <- Seurat::VariableFeatures(obj)

          write_mirai_log(log_file, "Alignement des coordonnees spatiales...", 3, 8)
          coords_df <- coords[match(colnames(obj), coords$id), c("x", "y")]
          rownames(coords_df) <- colnames(obj)
          keep <- stats::complete.cases(coords_df)
          obj <- obj[, keep]
          coords_mat <- as.matrix(coords_df[keep, , drop = FALSE])

          write_mirai_log(log_file, sprintf("Voisinage spatial (k=%d, RANN)...", k_geom), 4, 8)
          nn <- RANN::nn2(coords_mat, k = min(k_geom + 1, nrow(coords_mat)))
          neighbor_idx <- nn$nn.idx[, -1, drop = FALSE]  # drop self (1st column, distance 0)

          write_mirai_log(log_file, "Augmentation BANKSY-lite (moyenne du voisinage)...", 5, 8)
          own_mat <- t(as.matrix(SeuratObject::LayerData(obj, layer = "data")[var_feat, , drop = FALSE]))
          n  <- nrow(own_mat)
          kk <- ncol(neighbor_idx)
          # ONE sparse-matrix multiply instead of a per-row loop (same trick
          # as helpers_sc.R::remap_seurat_ids_to_symbol()'s duplicate collapse).
          W <- Matrix::sparseMatrix(
            i = rep(seq_len(n), each = kk), j = as.vector(t(neighbor_idx)),
            x = 1 / kk, dims = c(n, n)
          )
          nbr_mat <- as.matrix(W %*% own_mat)
          dimnames(nbr_mat) <- dimnames(own_mat)

          own_scaled <- scale(own_mat); own_scaled[!is.finite(own_scaled)] <- 0
          nbr_scaled <- scale(nbr_mat); nbr_scaled[!is.finite(nbr_scaled)] <- 0
          augmented  <- cbind(sqrt(1 - lambda) * own_scaled, sqrt(lambda) * nbr_scaled)

          write_mirai_log(log_file, "PCA sur la matrice augmentee...", 6, 8)
          n_pc <- max(2, min(npcs, ncol(augmented) - 1, nrow(augmented) - 1))
          pca <- if (requireNamespace("irlba", quietly = TRUE)) {
            irlba::prcomp_irlba(augmented, n = n_pc, center = FALSE, scale. = FALSE)
          } else {
            stats::prcomp(augmented, rank. = n_pc, center = FALSE, scale. = FALSE)
          }
          emb <- pca$x
          rownames(emb) <- colnames(obj)
          colnames(emb) <- paste0("BANKSYPCA_", seq_len(ncol(emb)))
          obj[["BANKSY_PCA"]] <- Seurat::CreateDimReducObject(
            embeddings = emb, key = "BANKSYPCA_", assay = Seurat::DefaultAssay(obj)
          )

          write_mirai_log(log_file, "Recherche des voisins (graphe)...", 7, 8)
          obj <- Seurat::FindNeighbors(obj, reduction = "BANKSY_PCA", dims = seq_len(n_pc), verbose = FALSE)

          write_mirai_log(log_file, "Clustering Leiden...", 8, 8)
          obj <- tryCatch(
            Seurat::FindClusters(obj, resolution = resolution, algorithm = 4, verbose = FALSE),
            error = function(e) {
              write_mirai_log(log_file, paste("Leiden indisponible (", conditionMessage(e),
                                               ") - repli sur Louvain."), 8, 8)
              Seurat::FindClusters(obj, resolution = resolution, algorithm = 1, verbose = FALSE)
            }
          )

          write_mirai_log(log_file, "Termine.", 8, 8)
          setNames(as.character(obj$seurat_clusters), colnames(obj))
        },
        bpcells_dir = bpcells_dir, pass_idx = pass_idx, coords = coords,
        lambda = lambda, k_geom = k_geom, npcs = npcs, resolution = resolution,
        log_file = log_file, .timeout = MIRAI_TASK_TIMEOUT_MS
      )
    })
    bslib::bind_task_button(cluster_task, "btn_cluster")

    observeEvent(input$btn_cluster, {
      req(global_data$spatial_obj$bpcells_dir, global_data$spatial_obj$coords)
      pass_idx <- safe_pass_idx(shared_rv$qc_pass_idx, global_data$active_spatial_dataset,
                                global_data$spatial_obj$n_total)
      if (is.null(pass_idx) && !is.null(shared_rv$qc_pass_idx)) {
        showNotification(.tr("Seuils QC obsoletes pour cet echantillon -- clustering lance sans filtre QC."),
                         type = "warning", duration = 8)
      }
      reset_log(log_file)
      # v3 (export/script reproductible) : miroir des parametres UTILISES,
      # lu uniquement par mod_spatial_export.R -- purement additif.
      shared_rv$cluster_params <- list(lambda = input$lambda, k_geom = input$k_geom,
                                       npcs = input$npcs, resolution = input$resolution)
      cluster_task$invoke(
        bpcells_dir = global_data$spatial_obj$bpcells_dir,
        pass_idx    = pass_idx,
        coords      = global_data$spatial_obj$coords,
        lambda      = input$lambda, k_geom = input$k_geom,
        npcs        = input$npcs, resolution = input$resolution,
        log_file    = log_file
      )
    })
    observeEvent(cluster_task$status(), {
      if (cluster_task$status() == "success") {
        shared_rv$cluster_labels <- cluster_task$result()
        showNotification(.t_fmt(.tr("Clustering termine : {n} clusters trouves."),
                                  n = length(unique(shared_rv$cluster_labels))),
                          type = "message", duration = 5)
      } else if (cluster_task$status() == "error") {
        showNotification(
          .tr("Erreur (ou depassement du delai de 20 min) pendant le clustering — voir le log. Essayez 'Reinitialiser les daemons' dans l'entete Spatial puis relancez."),
          type = "error", duration = 12)
      }
    })

    # Full log history (not just the last line) — fast tasks on small
    # datasets can otherwise complete between two 1000ms polls, hiding every
    # intermediate step.
    output$cluster_progress_text <- renderText({
      global_data$language  # i18n: re-render on language switch
      lines <- tracker()
      if (length(lines) == 0) return(.tr("En attente..."))
      paste(lines, collapse = "\n")
    })

    output$cluster_summary <- renderUI({
      global_data$language  # i18n: re-render on language switch
      req(shared_rv$cluster_labels)
      div(class = "alert alert-success",
          .t_fmt(.tr("{n} clusters sur {total} elements."),
                  n = length(unique(shared_rv$cluster_labels)),
                  total = length(shared_rv$cluster_labels)))
    })

    output$cluster_sizes_table <- DT::renderDT({
      global_data$language  # i18n: re-render on language switch
      req(shared_rv$cluster_labels)
      tab <- as.data.frame(table(cluster = shared_rv$cluster_labels), stringsAsFactors = FALSE)
      colnames(tab) <- c(.tr("Cluster"), .tr("Effectif"))
      ts_datatable(tab, page_length = 15L, filename_base = "spatial_cluster_sizes",
                   filter = "none", scroll_x = FALSE)
    })

    # ── Regional differential expression: FindAllMarkers, one cluster vs rest ──
    markers_log_file <- spatial_log_path(session, "cluster_markers")
    markers_tracker  <- create_reactive_tracker(session, markers_log_file)

    markers_task <- ExtendedTask$new(function(bpcells_dir, pass_idx, cluster_labels,
                                               logfc_threshold, min_pct, log_file) {
      mirai::mirai(
        {
          write_mirai_log(log_file, "Ouverture de la matrice BPCells...", 1, 4)
          mat <- BPCells::open_matrix_dir(bpcells_dir)
          if (!is.null(pass_idx)) mat <- mat[, pass_idx, drop = FALSE]

          write_mirai_log(log_file, "Alignement des labels de cluster...", 2, 4)
          common_ids <- intersect(colnames(mat), names(cluster_labels))
          if (length(common_ids) < 10) {
            stop(errorCondition(paste0(
              "Trop peu d'elements communs entre la matrice QC-filtree et les labels de cluster ",
              "(reimportez ou relancez le clustering si les seuils QC ont change entre-temps)."),
              class = "spatial_cluster_error"))
          }
          mat <- mat[, common_ids, drop = FALSE]

          obj <- Seurat::CreateSeuratObject(counts = mat)
          obj <- Seurat::NormalizeData(obj, verbose = FALSE)
          Seurat::Idents(obj) <- factor(cluster_labels[common_ids])

          write_mirai_log(log_file, sprintf("FindAllMarkers (Wilcoxon, %d clusters)...",
                                             length(unique(cluster_labels[common_ids]))), 3, 4)
          markers <- Seurat::FindAllMarkers(
            obj, only.pos = FALSE, logfc.threshold = logfc_threshold, min.pct = min_pct,
            test.use = "wilcox", verbose = FALSE
          )

          write_mirai_log(log_file, "Termine.", 4, 4)
          if (nrow(markers) == 0) {
            return(data.frame(cluster = character(0), gene = character(0), avg_log2FC = numeric(0),
                               pct.1 = numeric(0), pct.2 = numeric(0), p_val_adj = numeric(0)))
          }
          markers <- markers[order(markers$cluster, -markers$avg_log2FC), ]
          markers[, c("cluster", "gene", "avg_log2FC", "pct.1", "pct.2", "p_val", "p_val_adj")]
        },
        bpcells_dir = bpcells_dir, pass_idx = pass_idx, cluster_labels = cluster_labels,
        logfc_threshold = logfc_threshold, min_pct = min_pct, log_file = log_file,
        .timeout = MIRAI_TASK_TIMEOUT_MS
      )
    })
    bslib::bind_task_button(markers_task, "btn_find_markers")

    observeEvent(input$btn_find_markers, {
      req(global_data$spatial_obj$bpcells_dir, shared_rv$cluster_labels)
      pass_idx <- safe_pass_idx(shared_rv$qc_pass_idx, global_data$active_spatial_dataset,
                                global_data$spatial_obj$n_total)
      reset_log(markers_log_file)
      markers_task$invoke(
        bpcells_dir     = global_data$spatial_obj$bpcells_dir,
        pass_idx        = pass_idx,
        cluster_labels  = shared_rv$cluster_labels,
        logfc_threshold = input$logfc_threshold,
        min_pct         = input$min_pct,
        log_file        = markers_log_file
      )
    })
    
    observeEvent(markers_task$status(), {
      if (markers_task$status() == "success") {
        shared_rv$cluster_markers <- markers_task$result()
        showNotification(.t_fmt(.tr("Marqueurs trouves : {n} genes (tous clusters confondus)."),
                                  n = nrow(shared_rv$cluster_markers)), type = "message", duration = 5)
      } else if (markers_task$status() == "error") {
        showNotification(
          .tr("Erreur (ou depassement du delai) pendant la recherche de marqueurs — voir le log. Essayez 'Reinitialiser les daemons' puis relancez."),
          type = "error", duration = 12)
      }
    })

    output$markers_progress_text <- renderText({
      global_data$language  # i18n: re-render on language switch
      lines <- markers_tracker()
      if (length(lines) == 0) return(.tr("En attente..."))
      paste(lines, collapse = "\n")
    })

    output$markers_table <- DT::renderDT({
      req(shared_rv$cluster_markers)
      ts_datatable(shared_rv$cluster_markers, page_length = 15L,
                   filename_base = "spatial_cluster_markers") |>
        DT::formatRound(c("avg_log2FC", "pct.1", "pct.2"), 3) |>
        DT::formatSignif(c("p_val", "p_val_adj"), 3)
    })
  })
}

# =============================================================================
# modules/spatial/deconv/mod_spatial_deconv_refviz.R — Reference UMAP/PCA preview
# =============================================================================
# Owns: ref_viz_task (mirai), ref_viz_result, progress text, static + plotly
# outputs for the reference preview panel.
#
# Plain function called from the orchestrator's moduleServer().
#
# i18n: mirai task body stays COMPLETELY UNCHANGED (daemons have no
# global_data/i18n — write_mirai_log strings remain French).
# =============================================================================

.deconv_refviz_server <- function(input, output, session, ns, global_data, shared_rv, ref_state, input_ref_celltype_col) {

  # Session-scoped scalar translation (plain strings, never HTML spans).
  .tr <- function(key) {
    tr <- global_data$i18n
    if (is.null(tr)) return(key)
    tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
  }

  # discrete_palette_colors_fallback() supprimee : spatial_discrete_colors()
  # (R/palettes.R) suit la palette globale de l'onglet Visualisation.

  ref_viz_log_file <- spatial_log_path(session, "ref_viz")
  ref_viz_tracker  <- create_reactive_tracker(session, ref_viz_log_file)

  ref_viz_task <- ExtendedTask$new(function(mode, ref_obj_path, ref_manifest_path, celltype_col,
                                             reduction, max_cells, log_file) {
    mirai::mirai(
      {
        write_mirai_log(log_file, "Chargement de la reference...", 1, 4)
        if (identical(mode, "local")) {
          obj <- readRDS(ref_obj_path)
          if (!celltype_col %in% colnames(obj@meta.data)) {
            stop(errorCondition("Colonne 'type cellulaire' introuvable.",
                                class = "spatial_deconv_refviz_error"))
          }
          cell_types <- as.character(obj@meta.data[[celltype_col]])
          names(cell_types) <- colnames(obj)
        } else {
          manifest <- readRDS(ref_manifest_path)
          counts <- if (identical(manifest$backend, "bpcells")) {
            BPCells::open_matrix_dir(manifest$counts_path)
          } else readRDS(manifest$counts_path)
          obj <- Seurat::CreateSeuratObject(counts = counts)
          cell_types <- as.character(manifest$cell_types)[match(colnames(obj), names(manifest$cell_types))]
          names(cell_types) <- colnames(obj)
        }

        if (ncol(obj) > max_cells) {
          write_mirai_log(log_file, sprintf("Sous-echantillonnage (%d -> %d cellules, apercu rapide)...",
                                             ncol(obj), max_cells), 2, 4)
          set.seed(1)
          keep <- sample(colnames(obj), max_cells)
          obj <- obj[, keep]
          cell_types <- cell_types[keep]
        }

        has_red <- reduction %in% names(obj@reductions)
        if (has_red) {
          write_mirai_log(log_file, sprintf("Reduction '%s' deja presente -- reutilisee.", reduction), 3, 4)
        } else {
          write_mirai_log(log_file, "Normalisation + PCA...", 2, 4)
          obj <- Seurat::NormalizeData(obj, verbose = FALSE)
          obj <- Seurat::FindVariableFeatures(obj, verbose = FALSE)
          obj <- Seurat::ScaleData(obj, verbose = FALSE)
          obj <- Seurat::RunPCA(obj, npcs = 30, verbose = FALSE)
          if (identical(reduction, "umap")) {
            write_mirai_log(log_file, "UMAP...", 3, 4)
            obj <- Seurat::RunUMAP(obj, dims = 1:30, verbose = FALSE)
          }
        }

        write_mirai_log(log_file, "Termine.", 4, 4)
        emb <- as.data.frame(Seurat::Embeddings(obj, reduction)[, 1:2])
        colnames(emb) <- c("dim1", "dim2")
        emb$id <- rownames(emb)
        emb$cell_type <- cell_types[emb$id]
        emb
      },
      mode = mode, ref_obj_path = ref_obj_path, ref_manifest_path = ref_manifest_path,
      celltype_col = celltype_col, reduction = reduction, max_cells = max_cells, log_file = log_file,
      .timeout = MIRAI_TASK_TIMEOUT_MS
    )
  })
  bslib::bind_task_button(ref_viz_task, "btn_ref_viz")

  observeEvent(input$btn_ref_viz, {
    reset_log(ref_viz_log_file)
    # "shared" mode: use global_data$spatial_reference manifest
    if (!is.null(global_data$spatial_reference) &&
        (is.null(input$ref_source) || identical(input$ref_source, "shared"))) {
      req(global_data$spatial_reference$path)
      ref_viz_task$invoke(mode = "shared", ref_obj_path = NULL,
                          ref_manifest_path = global_data$spatial_reference$path,
                          celltype_col = global_data$spatial_reference$celltype_col %||% "",
                          reduction = input$ref_viz_reduction, max_cells = 20000,
                          log_file = ref_viz_log_file)
    } else {
      # "local" mode: use the ref_state$obj uploaded in this session
      if (is.null(ref_state$obj) || is.null(input_ref_celltype_col())) {
        showNotification(.tr("Chargez d'abord une reference et choisissez la colonne 'type cellulaire'."),
                         type = "warning", duration = 8)
        return()
      }
      tmp <- tempfile(fileext = ".rds")
      saveRDS(ref_state$obj, tmp)
      ref_viz_task$invoke(mode = "local", ref_obj_path = tmp, ref_manifest_path = NULL,
                          celltype_col = input_ref_celltype_col(), reduction = input$ref_viz_reduction,
                          max_cells = 20000, log_file = ref_viz_log_file)
    }
  })

  ref_viz_result <- reactiveVal(NULL)
  observeEvent(ref_viz_task$status(), {
    if (ref_viz_task$status() == "success") {
      ref_viz_result(ref_viz_task$result())
    } else if (ref_viz_task$status() == "error") {
      showNotification(.tr("Erreur pendant le calcul UMAP/PCA de la reference -- voir le journal."),
                       type = "error", duration = 10)
    }
  })

  output$ref_viz_progress_text <- renderText({
    global_data$language  # i18n: re-render on language switch
    lines <- ref_viz_tracker()
    if (length(lines) == 0) return(.tr("En attente..."))
    paste(lines, collapse = "\n")
  })

  output$ref_viz_plot <- renderPlot({
    global_data$language  # i18n: re-render on language switch
    req(ref_viz_result())
    emb <- ref_viz_result()
    lv  <- sort(unique(stats::na.omit(as.character(emb$cell_type))))
    pal <- spatial_discrete_colors(lv, shared_rv, kind = "celltype")
    centroids <- stats::aggregate(cbind(dim1, dim2) ~ cell_type, data = emb, FUN = stats::median)
    ggplot2::ggplot(emb, ggplot2::aes(x = dim1, y = dim2, color = cell_type)) +
      ggplot2::geom_point(size = 0.6, alpha = 0.7) +
      ggplot2::scale_color_manual(values = pal, na.value = "#CCCCCC") +
      ggplot2::geom_text(data = centroids, ggplot2::aes(label = cell_type), color = "black",
                         size = 3.2, fontface = "bold") +
      ts_theme("minimal") +
      ggplot2::labs(x = paste0(toupper(input$ref_viz_reduction), "_1"),
                    y = paste0(toupper(input$ref_viz_reduction), "_2"),
                    title = .tr("Reference scRNA-seq")) +
      ggplot2::theme(legend.position = "none")
  })

  output$ref_viz_plotly <- plotly::renderPlotly({
    global_data$language  # i18n: re-render on language switch
    req(ref_viz_result())
    emb <- ref_viz_result()
    lv  <- sort(unique(stats::na.omit(as.character(emb$cell_type))))
    pal <- spatial_discrete_colors(lv, shared_rv, kind = "celltype")
    plotly::plot_ly(emb, x = ~dim1, y = ~dim2, color = ~cell_type, colors = pal,
                    type = "scattergl", mode = "markers",
                    marker = list(size = 5, opacity = 0.75),
                    text = ~paste0(.tr("Type: "), cell_type), hoverinfo = "text") |>
      plotly::layout(
        xaxis = list(title = paste0(toupper(input$ref_viz_reduction), "_1")),
        yaxis = list(title = paste0(toupper(input$ref_viz_reduction), "_2")),
        margin = list(l = 20, r = 20, t = 20, b = 20)
      )
  })
}

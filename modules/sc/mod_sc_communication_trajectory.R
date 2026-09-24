# =============================================================================
# mod_sc_communication_trajectory.R — onglet « Contexte trajectoire » du
# panneau Communication (roadmap avancée Phase 3 / V1.x-B)
# =============================================================================
# Orchestration uniquement : la science residue dans
# R/sc/sc_communication_trajectory.R (contrat
# COMMUNICATION_TRAJECTORY_CONTRACT). Le module choisit la colonne de
# pseudo-temps, le mode lignees (slingshot : premiere colonne finie par
# cellule — jamais de collapse), le nombre de bins ; l'expression est
# extraite en donnees normalisees ('data'), gènes nécessaires uniquement.
# Provenance PRODUITE au calcul, appended a l'etat partage.
# =============================================================================

mod_sc_communication_trajectory_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-warning", style = "font-size:0.85em;padding:6px;",
        i18n$t("Pseudo-temps = ordonnement relatif d'une trajectoire inférée, PAS un temps réel. Le score importé d'une paire est constant — seules les populations et l'expression varient le long du pseudo-temps.")),
    fluidRow(
      column(6, selectInput(ns("trj_pt_col"), i18n$t("Colonne de pseudo-temps (méta.data)"),
                            choices = character(0), width = "100%")),
      column(6, selectInput(ns("trj_assay"), i18n$t("Assay"),
                            choices = "RNA", width = "100%"))),
    fluidRow(
      column(6, radioButtons(ns("trj_lineage_mode"), i18n$t("Lignées (slingshot)"),
                             choices = setNames(c("global", "per_lineage"),
                                                c(.tr_plain("Aucune (toutes cellules)"),
                                                  .tr_plain("Par lignée (pseudotime_slingshot_*)"))),
                             selected = "global", width = "100%")),
      column(6, numericInput(ns("trj_nbins"), i18n$t("Bins de pseudo-temps"),
                             value = 10, min = 2, max = 100, step = 1, width = "100%"))),
    div(class = "small text-muted mb-2",
        i18n$t("Mode par lignée : chaque cellule est rattachée à la première colonne pseudotime_slingshot_* finie (point de branchement = première lignée). Les lignées ne sont jamais collapsées.")),
    actionButton(ns("trj_compute"), i18n$t("Calculer le contexte trajectoire"),
                 class = "btn-primary w-100", icon = icon("chart-line")),
    div(class = "small text-muted mt-1", textOutput(ns("trj_status"))),
    hr(),
    fluidRow(
      column(6, selectizeInput(ns("trj_pair"), i18n$t("Paire sender → receiver"),
                               choices = character(0), width = "100%")),
      column(6, selectizeInput(ns("trj_lineage_pick"), i18n$t("Lignée"),
                               choices = character(0), width = "100%"))),
    fluidRow(
      column(6, plotOutput(ns("trj_curves"), height = "340px"),
             fluidRow(
               column(6, downloadButton(ns("dl_trj_curves_png"), i18n$t("PNG"), class = "btn-sm btn-outline-secondary")),
               column(6, downloadButton(ns("dl_trj_curves_pdf"), i18n$t("PDF"), class = "btn-sm btn-outline-secondary")))),
      column(6, plotOutput(ns("trj_heatmap"), height = "340px"),
             fluidRow(
               column(6, downloadButton(ns("dl_trj_heat_png"), i18n$t("PNG"), class = "btn-sm btn-outline-secondary")),
               column(6, downloadButton(ns("dl_trj_heat_pdf"), i18n$t("PDF"), class = "btn-sm btn-outline-secondary"))))),
    fluidRow(
      column(6, DT::dataTableOutput(ns("trj_pair_summary"), height = "260px")),
      column(6, downloadButton(ns("dl_trj_csv"), i18n$t("Exporter bins (CSV)"), class = "btn-sm btn-info w-100 mt-4")))
  )
}
mod_sc_communication_trajectory_server <- function(id, comm_state, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    .tr <- function(key) {
      tr <- isolate(global_data$i18n)
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }
    .error_plot <- function(e) {
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = conditionMessage(e), size = 5, colour = "red") +
        ggplot2::theme_void()
    }
    trj_ctx <- reactiveVal(NULL)

    .guard_stale <- function() {
      req(global_data$sc_obj, comm_state$result, comm_state$object_fingerprint)
      if (!identical(velocity_object_fingerprint(global_data$sc_obj),
                     comm_state$object_fingerprint)) {
        shiny::validate(shiny::need(FALSE, .tr("Résultat de communication périmé : réimportez.")))
      }
    }

    observeEvent(comm_state$result, { trj_ctx(NULL) }, ignoreInit = TRUE)

    # Colonnes de pseudo-temps + assays disponibles sur l'objet courant.
    observeEvent(global_data$sc_obj, {
      obj <- global_data$sc_obj
      if (is.null(obj)) {
        updateSelectInput(session, "trj_pt_col", choices = character(0))
        updateSelectInput(session, "trj_assay", choices = "RNA")
        return()
      }
      meta_cols <- colnames(obj@meta.data)
      pt_cols <- meta_cols[grepl("^pseudotime", meta_cols)]
      updateSelectInput(session, "trj_pt_col", choices = pt_cols,
                        selected = if ("pseudotime" %in% pt_cols) "pseudotime" else pt_cols[1])
      assays <- names(obj@assays)
      updateSelectInput(session, "trj_assay", choices = assays,
                        selected = if ("RNA" %in% assays) "RNA" else assays[1])
    }, ignoreInit = FALSE)

    # Rattachement des lignées : première colonne pseudotime_slingshot_*
    # finie par cellule (convention documentée dans l'UI).
    .lineage_vector <- function() {
      obj <- global_data$sc_obj
      meta <- obj@meta.data
      if (!is.null(comm_state$context_cells)) {
        meta <- meta[rownames(meta) %in% as.character(comm_state$context_cells), , drop = FALSE]
      }
      lin_cols <- grep("^pseudotime_slingshot_", colnames(meta), value = TRUE)
      if (!length(lin_cols)) return(NULL)
      vals <- meta[, lin_cols, drop = FALSE]
      lab <- paste0("L", seq_along(lin_cols))
      out <- rep(NA_character_, nrow(meta))
      assigned <- rep(FALSE, nrow(meta))
      for (k in seq_along(lin_cols)) {
        idx <- which(!assigned & is.finite(vals[[lin_cols[k]]]))
        out[idx] <- lab[k]
        assigned[idx] <- TRUE
      }
      setNames(out, rownames(meta))
    }

    .identities <- function() {
      obj <- global_data$sc_obj
      col <- comm_state$result$identity_column %||% NA_character_
      if (is.null(obj) || is.na(col) || !col %in% colnames(obj@meta.data)) return(NULL)
      meta <- obj@meta.data
      if (!is.null(comm_state$context_cells)) {
        meta <- meta[rownames(meta) %in% as.character(comm_state$context_cells), , drop = FALSE]
      }
      setNames(as.character(meta[[col]]), rownames(meta))
    }

    observeEvent(input$trj_compute, {
      .guard_stale()
      tryCatch({
        col <- input$trj_pt_col
        if (is.null(col) || !nzchar(col) || !col %in% colnames(global_data$sc_obj@meta.data)) {
          stop("Choisissez d'abord une colonne de pseudo-temps (lancez d'abord Trajectoire 8.4).", call. = FALSE)
        }
        meta <- global_data$sc_obj@meta.data
        if (!is.null(comm_state$context_cells)) {
          meta <- meta[rownames(meta) %in% as.character(comm_state$context_cells), , drop = FALSE]
        }
        pt <- setNames(as.numeric(meta[[col]]), rownames(meta))
        idents <- .identities()
        if (is.null(idents)) {
          stop(sprintf("Colonne d'identités '%s' introuvable — réimportez la communication.",
                       comm_state$result$identity_column %||% NA), call. = FALSE)
        }
        lineage <- if (identical(input$trj_lineage_mode, "per_lineage")) .lineage_vector() else NULL
        if (identical(input$trj_lineage_mode, "per_lineage") &&
            (is.null(lineage) || all(is.na(lineage)))) {
          stop("Aucune colonne pseudotime_slingshot_* exploitable : relancez Trajectoire en mode Slingshot.", call. = FALSE)
        }
        table_use <- comm_state$result$canonical_table
        genes_needed <- unique(c(stats::na.omit(table_use$ligand),
                                 stats::na.omit(table_use$receptor)))
        expr <- communication_fetch_expression_matrix(
          global_data$sc_obj, genes = genes_needed, assay = input$trj_assay %||% "RNA"
        )
        if (!is.null(comm_state$context_cells)) {
          expr <- expr[, colnames(expr) %in% as.character(comm_state$context_cells), drop = FALSE]
        }
        ctx <- build_communication_trajectory_context(
          comm_state$result,
          expression_matrix = expr,
          cell_identities   = idents,
          pseudotime        = pt,
          lineage           = lineage,
          identity_column   = comm_state$result$identity_column,
          n_bins            = as.integer(input$trj_nbins %||% 10L)
        )
        trj_ctx(ctx)
        if (!is.null(shared_rv)) provenance_append(shared_rv, ctx$provenance)
        pairs <- paste(ctx$pair_bin_table$sender_node, ctx$pair_bin_table$receiver_node, sep = "\r")
        pair_labels <- ifelse(is.na(ctx$pair_bin_table$lineage), pairs,
                              paste0(pairs, " [", ctx$pair_bin_table$lineage, "]"))
        uniq <- !duplicated(pair_labels)
        updateSelectizeInput(session, "trj_pair",
                             choices = setNames(pair_labels[uniq], gsub("\r", " → ", pair_labels[uniq])),
                             selected = if (any(uniq)) pair_labels[uniq][1] else character(0))
        lins <- sort(unique(stats::na.omit(ctx$pair_bin_table$lineage)))
        updateSelectizeInput(session, "trj_lineage_pick", choices = lins,
                             selected = if (length(lins)) lins[1] else character(0))
        showNotification(
          sprintf(.tr("Contexte trajectoire : %d combinaison(s) paire/lignée calculée(s) sur %d."),
                  ctx$qc$n_combinations_mapped, ctx$qc$n_combinations_total),
          type = "message", duration = 5
        )
      }, error = function(e) {
        trj_ctx(NULL)
        showNotification(paste(.tr("Erreur contexte trajectoire :"), conditionMessage(e)),
                         type = "error", duration = 10)
      })
    })

    output$trj_status <- renderText({
      ctx <- trj_ctx()
      req(ctx)
      sprintf(.tr("Statut : %s — %d cellule(s) appariée(s) ; %d bins (effectif) ; %d combinaison(s) non calculable(s)."),
              ctx$status, ctx$qc$n_cells_matched, ctx$qc$n_bins_effective,
              ctx$qc$n_combinations_unmapped)
    })

    .selected_pair_lineage <- function() {
      ctx <- trj_ctx()
      k <- input$trj_pair
      if (is.null(ctx) || is.null(k) || !nzchar(k)) return(NULL)
      lab <- k
      lin <- input$trj_lineage_pick
      base <- sub(" \\[.*\\]$", "", lab)
      sp <- strsplit(base, "\r", fixed = TRUE)[[1L]]
      list(sender = sp[1L], receiver = sp[2L],
           lineage = if (is.null(lin) || !nzchar(lin)) NULL else lin)
    }

    output$trj_curves <- renderPlot({
      ctx <- trj_ctx()
      req(ctx)
      pr <- .selected_pair_lineage()
      req(pr)
      tryCatch(
        plot_communication_trajectory_curves(ctx, pr$sender, pr$receiver,
                                             lineage = pr$lineage),
        error = function(e) .error_plot(e)
      )
    })

    output$trj_heatmap <- renderPlot({
      ctx <- trj_ctx()
      req(ctx)
      tryCatch(plot_communication_trajectory_heatmap(ctx), error = function(e) .error_plot(e))
    })

    output$trj_pair_summary <- DT::renderDataTable({
      ctx <- trj_ctx()
      req(ctx)
      ts_datatable(ctx$pair_summary, page_length = 15L,
                   filename_base = "sc_comm_trajectory_pair_summary", filter = "none")
    })

    output$dl_trj_csv <- downloadHandler(
      filename = function() {
        ctx <- trj_ctx()
        if (is.null(ctx)) return("communication_trajectory_context.csv")
        paste0("communication_trajectory_context_", ctx$analysis_id, "_", ctx$timestamp_utc, ".csv")
      },
      content = function(file) {
        ctx <- trj_ctx()
        req(ctx)
        .guard_stale()
        utils::write.csv(build_communication_trajectory_export(ctx), file, row.names = FALSE)
      }
    )

    .fig_download <- function(output_id, kind, plot_fn) {
      output[[output_id]] <- downloadHandler(
        filename = function() {
          ctx <- trj_ctx()
          if (is.null(ctx)) return(paste0(kind, ".png"))
          paste0(kind, "_", ctx$analysis_id, "_", ctx$timestamp_utc, ".",
                 tolower(tools::file_ext(output_id)))
        },
        content = function(file) {
          ctx <- trj_ctx()
          req(ctx)
          .guard_stale()
          ts_export_plot(file, plot_fn(), width = 8, height = 6, dpi = 300)
        }
      )
    }
    .fig_download("dl_trj_curves_png", "communication_trajectory_curves", function() {
      pr <- .selected_pair_lineage(); req(pr)
      plot_communication_trajectory_curves(trj_ctx(), pr$sender, pr$receiver, lineage = pr$lineage)
    })
    .fig_download("dl_trj_curves_pdf", "communication_trajectory_curves", function() {
      pr <- .selected_pair_lineage(); req(pr)
      plot_communication_trajectory_curves(trj_ctx(), pr$sender, pr$receiver, lineage = pr$lineage)
    })
    .fig_download("dl_trj_heat_png", "communication_trajectory_heatmap", function() {
      plot_communication_trajectory_heatmap(trj_ctx())
    })
    .fig_download("dl_trj_heat_pdf", "communication_trajectory_heatmap", function() {
      plot_communication_trajectory_heatmap(trj_ctx())
    })

    invisible(NULL)
  })
}

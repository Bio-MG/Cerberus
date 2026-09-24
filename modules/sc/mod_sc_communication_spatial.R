# =============================================================================
# mod_sc_communication_spatial.R — onglet « Contexte spatial » du panneau
# Communication (roadmap avancée Phase 1 / V1.x-A)
# =============================================================================
# Orchestration uniquement : la science residue dans
# R/sc/sc_communication_spatial.R (contrat COMMUNICATION_SPATIAL_CONTRACT).
# Le module DECLAR explicitement la source de coordonnees (objet spatial
# courant = unites natives, ou reduction 2D = distances de projection NON
# physiques), le rayon (jamais infere) et les permutations (opt-in, seed
# fixee) ; la provenance est PRODUITE au calcul et appended a l'etat partage.
# Aucun recalcul de score, aucune modification du resultat canonique.
# =============================================================================

mod_sc_communication_spatial_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-warning", style = "font-size:0.85em;padding:6px;",
        i18n$t("La distance est une contrainte spatiale — une compatibilité spatiale n'établit aucune communication biologique. ")),
    fluidRow(
      column(6, radioButtons(ns("spc_source"), i18n$t("Source de coordonnées"),
                             choices = setNames(c("reduction", "spatial_object"),
                                                c(.tr_plain("Projection 2D (réduction — distances NON physiques)"),
                                                  .tr_plain("Coordonnées spatiales (bundle spatial courant)"))),
                             selected = "reduction", width = "100%")),
      column(6,
             conditionalPanel(
               condition = "input.spc_source == 'reduction'", ns = ns,
               selectInput(ns("spc_reduction"), i18n$t("Réduction 2D"),
                           choices = character(0), width = "100%")),
             conditionalPanel(
               condition = "input.spc_source == 'spatial_object'", ns = ns,
               div(class = "small text-muted mt-4",
                   textOutput(ns("spc_spatial_info")))))),
    fluidRow(
      column(4, numericInput(ns("spc_radius"), i18n$t("Rayon (NA = non calculé)"),
                             value = NA, min = 0, width = "100%")),
      column(4, numericInput(ns("spc_nperm"), i18n$t("Permutations (0 = aucune)"),
                             value = 0, min = 0, max = 1000, step = 1, width = "100%")),
      column(4, div(class = "small text-muted mt-4",
                    i18n$t("Seed fixée : 1 (déterministe).")))),
    actionButton(ns("spc_compute"), i18n$t("Calculer le contexte spatial"),
                 class = "btn-primary w-100", icon = icon("ruler")),
    div(class = "small text-muted mt-1", textOutput(ns("spc_status"))),
    hr(),
    DT::dataTableOutput(ns("spc_table"), height = "320px"),
    fluidRow(
      column(6, selectizeInput(ns("spc_pair"), i18n$t("Paire sender → receiver"),
                               choices = character(0), width = "100%")),
      column(6, downloadButton(ns("dl_spc_csv"), i18n$t("CSV"), class = "btn-sm btn-outline-secondary mt-4"))),
    fluidRow(
      column(6, plotOutput(ns("spc_edges"), height = "360px"),
             fluidRow(
               column(6, downloadButton(ns("dl_spc_edges_png"), i18n$t("PNG"), class = "btn-sm btn-outline-secondary")),
               column(6, downloadButton(ns("dl_spc_edges_pdf"), i18n$t("PDF"), class = "btn-sm btn-outline-secondary")))),
      column(6, plotOutput(ns("spc_distances"), height = "360px"),
             fluidRow(
               column(6, downloadButton(ns("dl_spc_dist_png"), i18n$t("PNG"), class = "btn-sm btn-outline-secondary")),
               column(6, downloadButton(ns("dl_spc_dist_pdf"), i18n$t("PDF"), class = "btn-sm btn-outline-secondary")))))
  )
}

mod_sc_communication_spatial_server <- function(id, comm_state, global_data, shared_rv) {
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
    spc_ctx <- reactiveVal(NULL)

    # Garde de peremption (resultat canonique vs objet courant) — garde legere,
    # le portage complet reste dans le module parent.
    .guard_stale <- function() {
      req(global_data$sc_obj, comm_state$result, comm_state$object_fingerprint)
      if (!identical(velocity_object_fingerprint(global_data$sc_obj),
                     comm_state$object_fingerprint)) {
        shiny::validate(shiny::need(FALSE, .tr("Résultat de communication périmé : réimportez.")))
      }
    }

    # Nouveau resultat importe -> contexte precedant perime (jamais affiche).
    observeEvent(comm_state$result, { spc_ctx(NULL) }, ignoreInit = TRUE)

    # Redutions 2D disponibles (>= 2 colonnes), defaut umap.
    observeEvent(global_data$sc_obj, {
      obj <- global_data$sc_obj
      reds <- if (!is.null(obj)) names(obj@reductions) else character(0)
      reds2 <- reds[vapply(reds, function(rd) {
        em <- tryCatch(SeuratObject::Embeddings(obj, rd), error = function(e) NULL)
        !is.null(em) && ncol(em) >= 2L
      }, logical(1))]
      updateSelectInput(session, "spc_reduction", choices = reds2,
                        selected = if ("umap" %in% reds2) "umap" else reds2[1])
    }, ignoreInit = FALSE)

    output$spc_spatial_info <- renderText({
      b <- global_data$spatial_obj
      n <- if (!is.null(b) && !is.null(b$coords)) nrow(b$coords) else 0L
      if (n == 0L) {
        .tr("Aucune coordonnée disponible dans le bundle spatial courant.")
      } else {
        sprintf(.tr("Bundle spatial courant : %d spot(s)/cellule(s) (coordonnées d'import, colonnes id/x/y)."), n)
      }
    })

    # Identites nommees depuis la colonne D'IMPORT (enregistree dans le
    # resultat canonique) sur l'objet courant.
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

    .coordinates <- function() {
      if (identical(input$spc_source, "spatial_object")) {
        b <- global_data$spatial_obj
        coords <- b$coords %||% NULL
        if (is.null(coords) || !all(c("id", "x", "y") %in% names(coords))) {
          stop("Aucune coordonnée id/x/y disponible dans le bundle spatial courant.", call. = FALSE)
        }
        coords <- coords[, c("id", "x", "y"), drop = FALSE]
        if (!is.null(comm_state$context_cells)) {
          coords <- coords[as.character(coords$id) %in%
                            as.character(comm_state$context_cells), , drop = FALSE]
        }
        coords
      } else {
        obj <- global_data$sc_obj
        red <- input$spc_reduction
        if (is.null(obj) || is.null(red) || !nzchar(red)) {
          stop("Choisissez d'abord une réduction 2D.", call. = FALSE)
        }
        em <- SeuratObject::Embeddings(obj, red)[, 1:2, drop = FALSE]
        if (!is.null(comm_state$context_cells)) {
          em <- em[rownames(em) %in% as.character(comm_state$context_cells), , drop = FALSE]
        }
        colnames(em) <- c("x", "y")
        as.data.frame(em)
      }
    }

    observeEvent(input$spc_compute, {
      .guard_stale()
      tryCatch({
        idents <- .identities()
        if (is.null(idents)) {
          stop(sprintf(
            "Colonne d'identités '%s' introuvable sur l'objet courant — réimportez la communication.",
            comm_state$result$identity_column %||% NA_character_
          ), call. = FALSE)
        }
        ctx <- build_communication_spatial_context(
          comm_state$result,
          coordinates       = .coordinates(),
          cell_identities   = idents,
          identity_column   = comm_state$result$identity_column,
          coordinate_source = input$spc_source,
          coordinate_units  = if (identical(input$spc_source, "spatial_object")) "unités natives" else NA_character_,
          radius            = input$spc_radius %||% NA_real_,
          n_permutations    = as.integer(input$spc_nperm %||% 0L),
          perm_seed         = 1L
        )
        spc_ctx(ctx)
        # Provenance PRODUITE au calcul (regle 7) — appended, jamais reconstruite.
        if (!is.null(shared_rv)) provenance_append(shared_rv, ctx$provenance)
        pairs <- paste(ctx$pair_table$sender_node, ctx$pair_table$receiver_node, sep = "\u2192")
        updateSelectizeInput(session, "spc_pair", choices = setNames(
          paste(ctx$pair_table$sender_node, ctx$pair_table$receiver_node, sep = "\r"), pairs
        ), selected = if (length(pairs)) pairs[1] else character(0))
        showNotification(
          sprintf(.tr("Contexte spatial : %d paire(s) cartographiée(s) sur %d."),
                  ctx$qc$n_pairs_mapped, ctx$qc$n_pairs_total),
          type = "message", duration = 5
        )
      }, error = function(e) {
        spc_ctx(NULL)
        showNotification(paste(.tr("Erreur contexte spatial :"), conditionMessage(e)),
                         type = "error", duration = 10)
      })
    })

    output$spc_status <- renderText({
      ctx <- spc_ctx()
      req(ctx)
      sprintf(.tr("Statut : %s — %d cellule(s) cartographiée(s) ; %d paire(s) non cartographiée(s) (listées dans les avertissements)."),
              ctx$status, ctx$qc$n_cells_matched, ctx$qc$n_pairs_unmapped)
    })

    output$spc_table <- DT::renderDataTable({
      ctx <- spc_ctx()
      req(ctx)
      ts_datatable(ctx$pair_table, page_length = 15L,
                   filename_base = "sc_comm_spatial_pair_table", filter = "none")
    })

    .selected_pair <- function() {
      ctx <- spc_ctx()
      k <- input$spc_pair
      if (is.null(ctx) || is.null(k) || !nzchar(k)) return(NULL)
      sp <- strsplit(k, "\r", fixed = TRUE)[[1L]]
      list(sender = sp[1L], receiver = sp[2L])
    }

    output$spc_edges <- renderPlot({
      ctx <- spc_ctx()
      req(ctx)
      pr <- .selected_pair()
      req(pr)
      tryCatch(
        plot_communication_spatial_edges(
          ctx, pr$sender, pr$receiver,
          coordinates = .coordinates(), cell_identities = .identities()
        ),
        error = function(e) .error_plot(e)
      )
    })

    output$spc_distances <- renderPlot({
      ctx <- spc_ctx()
      req(ctx)
      tryCatch(plot_communication_spatial_distance_summary(ctx), error = function(e) .error_plot(e))
    })

    output$dl_spc_csv <- downloadHandler(
      filename = function() {
        ctx <- spc_ctx()
        if (is.null(ctx)) return("communication_spatial_context.csv")
        paste0("communication_spatial_context_", ctx$analysis_id, "_", ctx$timestamp_utc, ".csv")
      },
      content = function(file) {
        ctx <- spc_ctx()
        req(ctx)
        .guard_stale()
        utils::write.csv(build_communication_spatial_export(ctx), file, row.names = FALSE)
      }
    )

    .fig_download <- function(output_id, kind, plot_fn) {
      output[[output_id]] <- downloadHandler(
        filename = function() {
          ctx <- spc_ctx()
          if (is.null(ctx)) return(paste0(kind, ".png"))
          paste0(kind, "_", ctx$analysis_id, "_", ctx$timestamp_utc, ".", tolower(tools::file_ext(output_id)))
        },
        content = function(file) {
          ctx <- spc_ctx()
          req(ctx)
          .guard_stale()
          ts_export_plot(file, plot_fn(ctx), width = 8, height = 6, dpi = 300)
        }
      )
    }
    .fig_download("dl_spc_edges_png", "communication_spatial_edges", function(ctx) {
      pr <- .selected_pair(); req(pr)
      plot_communication_spatial_edges(ctx, pr$sender, pr$receiver,
                                       coordinates = .coordinates(),
                                       cell_identities = .identities())
    })
    .fig_download("dl_spc_edges_pdf", "communication_spatial_edges", function(ctx) {
      pr <- .selected_pair(); req(pr)
      plot_communication_spatial_edges(ctx, pr$sender, pr$receiver,
                                       coordinates = .coordinates(),
                                       cell_identities = .identities())
    })
    .fig_download("dl_spc_dist_png", "communication_spatial_distances", function(ctx) {
      plot_communication_spatial_distance_summary(ctx)
    })
    .fig_download("dl_spc_dist_pdf", "communication_spatial_distances", function(ctx) {
      plot_communication_spatial_distance_summary(ctx)
    })

    invisible(NULL)
  })
}

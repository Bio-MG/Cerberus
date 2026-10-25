# =============================================================================
# mod_bulk_multi.R — MD-2 : comparaison multi-jeux bulk (côte à côte +
# recouvrement DEGs + concordance de direction) — UI + orchestration.
# =============================================================================
# Logique pure : tools/pkg_skeleton/R/bulk_multi_compare.R (contrat gelé
# BULK_MULTI_CONTRACT.md §10). Ce module ne fait que LIRE
# global_data$bulk_datasets : aucune écriture sur le jeu actif ni sur le
# conteneur. Résultat écrit en plat dans
# global_data$bulk_multi_comparison (pattern spatial_multi_integration).
#
# Réutilisation stricte (garde §10.2.4) : build_contrast_gene_sets(),
# build_contrast_intersection_dt(), plot_upset_contrasts(),
# plot_volcano_bulk(), ts_datatable() — jamais ré-implémentés ici.
# =============================================================================

mod_bulk_multi_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = i18n$t("Comparaison multi-jeux"), width = 360,
      div(class = "alert alert-light", style = "font-size:0.8rem;",
          bsicons::bs_icon("info-circle"),
          " ", i18n$t("Comparez des jeux enregistrés (import nommé ou bouton « Enregistrer l'état courant ») partageant un même nom de contraste. Les seuils choisis s'appliquent à TOUS les jeux comparés.")),
      selectInput(ns("datasets"), i18n$t("Datasets à comparer (>= 2)"),
                  choices = NULL, multiple = TRUE),
      uiOutput(ns("contrast_ui")),
      numericInput(ns("lfc_thresh"), i18n$t("Seuil |Log2FC|"), 1,
                   min = 0, step = 0.25),
      numericInput(ns("padj_thresh"), i18n$t("Seuil p-adj"), 0.05,
                   min = 1e-04, max = 1, step = 0.01),
      actionButton(ns("run"), i18n$t("Lancer la comparaison"),
                   icon = icon("layer-group"), class = "btn-primary w-100 mb-2"),
      verbatimTextOutput(ns("status"), placeholder = TRUE)
    ),
    navset_card_underline(
      id = ns("multi_tabs"), title = i18n$t("Résultats de la comparaison"),
      nav_panel(i18n$t("Volcanos côte à côte"), value = "tab_volcanos",
                plotOutput(ns("volcano_grid"), height = "640px")),
      nav_panel(i18n$t("Recouvrement des DEGs"), value = "tab_overlap",
                plotOutput(ns("upset"), height = "480px"),
                h6(i18n$t("Gènes par intersection"), style = "font-weight:bold; margin-top:12px;"),
                DTOutput(ns("intersection_table"))),
      nav_panel(i18n$t("Concordance de direction"), value = "tab_concordance",
                div(class = "alert alert-light", style = "font-size:0.78rem;",
                    i18n$t("Jaccard = recouvrement des ensembles Up (resp. Down) entre deux jeux. « % même direction » = proportion des gènes significatifs communs qui varient dans le même sens.")),
                DTOutput(ns("concordance_table"))),
      nav_panel(i18n$t("Détail des datasets"), value = "tab_detail",
                DTOutput(ns("per_dataset_table")))
    )
  )
}

mod_bulk_multi_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    last_result <- reactiveVal(NULL)

    # ── Eligible datasets: entries with a captured, non-empty pipeline ──
    eligible <- reactive({
      ds <- global_data$bulk_datasets
      keep <- names(ds)[vapply(ds, function(e) {
        length(bulk_multi_entry_contrasts(e)) > 0L
      }, logical(1))]
      sort(keep)
    })

    observeEvent(eligible(), {
      prev <- intersect(input$datasets %||% character(0), eligible())
      updateSelectInput(session, "datasets", choices = eligible(),
                        selected = prev)
    })

    # Default thresholds from the FIRST selected dataset's stored pipeline
    # (predictable: re-applied on every selection change, adjustable after).
    observeEvent(input$datasets, {
      ds <- global_data$bulk_datasets
      first <- input$datasets[1L]
      if (is.na(first) || is.null(first) || !first %in% names(ds)) return()
      pipe <- ds[[first]]$pipeline
      if (is.null(pipe)) return()
      if (is.numeric(pipe$lfc_thresh) && length(pipe$lfc_thresh) == 1L) {
        updateNumericInput(session, "lfc_thresh", value = pipe$lfc_thresh)
      }
      if (is.numeric(pipe$padj_thresh) && length(pipe$padj_thresh) == 1L &&
          pipe$padj_thresh > 0 && pipe$padj_thresh <= 1) {
        updateNumericInput(session, "padj_thresh", value = pipe$padj_thresh)
      }
    })

    # ── Common contrasts across selected datasets ────────────────────────
    common <- reactive({
      req(length(input$datasets) >= 1L)
      bulk_multi_common_contrasts(
        global_data$bulk_datasets[input$datasets])
    })

    output$contrast_ui <- renderUI({
      global_data$language  # i18n
      if (length(input$datasets) < 2L) {
        return(tags$div(class = "alert alert-warning", style = "font-size:0.8rem;",
                        .tr("Sélectionnez au moins 2 datasets traités (via « Multi-jeux — Datasets enregistrés » ou l'import nommé).")))
      }
      cm <- common()
      if (length(cm) == 0L) {
        return(tags$div(class = "alert alert-warning", style = "font-size:0.8rem;",
                        .tr("Aucun nom de contraste commun aux datasets sélectionnés.")))
      }
      selectInput(ns("contrast"), .tr("Contraste commun"), choices = cm)
    })

    # ── Run (synchronous — bulk V1.0 convention; light computation) ─────
    observeEvent(input$run, {
      global_data$language  # i18n
      req(input$contrast, length(input$datasets) >= 2L)
      ds <- global_data$bulk_datasets
      entries <- ds[input$datasets]
      entries <- entries[!vapply(entries, is.null, logical(1))]
      res <- tryCatch(
        bulk_multi_run_comparison(entries, input$contrast,
                                  input$lfc_thresh, input$padj_thresh),
        bulk_multi_error = function(e) e)
      if (inherits(res, "bulk_multi_error")) {
        showNotification(sprintf(.tr("Comparaison impossible : %s"),
                                 res$message),
                         type = "error", duration = 8)
        return()
      }
      global_data$bulk_multi_comparison <- res
      last_result(res)
      showNotification(
        sprintf(.tr("✓ Comparaison calculée (contraste « %s », %d datasets)."),
                res$contrast, length(res$datasets)),
        type = "message", duration = 5)
    })

    # ── Volcano grid: re-reads contrasts from bulk_datasets at render time
    # (no duplication in the stored result — contrat §10.2.5). If a dataset
    # was deleted since the run, fail softly with a message.
    output$volcano_grid <- renderPlot({
      global_data$language  # i18n
      res <- req(last_result())
      ds <- global_data$bulk_datasets
      missing <- setdiff(res$datasets, names(ds))
      validate(need(length(missing) == 0L,
                    .tr("Un dataset comparé a été supprimé depuis le calcul — relancez la comparaison.")))
      plots <- lapply(res$datasets, function(nm) {
        df <- ds[[nm]]$pipeline$contrasts[[res$contrast]]
        bulk_multi_volcano_panel(df, nm, res$lfc_thresh, res$padj_thresh,
                                 res$volcano_scales, tr = .tr)
      })
      patchwork::wrap_plots(plots, ncol = min(length(plots), 2L))
    }, height = function() {
      res <- last_result()
      if (is.null(res)) return(400)
      380 * ceiling(length(res$datasets) /
                      max(min(length(res$datasets), 2L), 1L))
    })

    output$upset <- renderPlot({
      global_data$language  # i18n
      res <- req(last_result())
      sets <- Filter(function(s) length(s) > 0L, res$deg_sets)
      validate(need(length(sets) >= 2L,
                    .tr("Moins de 2 datasets avec des gènes significatifs — pas d'UpSet.")))
      tryCatch(plot_upset_contrasts(sets),
               error = function(e) {
                 validate(need(FALSE, conditionMessage(e)))
               })
    })

    output$intersection_table <- renderDT({
      global_data$language  # i18n
      res <- req(last_result())
      validate(need(nrow(res$intersection_dt) > 0,
                    .tr("Aucun gène dans les intersections avec ces seuils.")))
      ts_datatable(res$intersection_dt, page_length = 15L,
                   filename_base = "bulk_multi_intersections")
    })

    output$concordance_table <- renderDT({
      global_data$language  # i18n
      res <- req(last_result())
      ts_datatable(res$concordance, page_length = 15L,
                   filename_base = "bulk_multi_concordance")
    })

    output$per_dataset_table <- renderDT({
      global_data$language  # i18n
      res <- req(last_result())
      ts_datatable(res$per_dataset, page_length = 15L,
                   filename_base = "bulk_multi_per_dataset")
    })

    output$status <- renderText({
      res <- last_result()
      if (is.null(res)) return(.tr("En attente — sélectionnez >= 2 datasets puis lancez la comparaison."))
      sprintf(.tr("✓ %s — %d datasets : %s"),
              res$contrast, length(res$datasets),
              paste(res$datasets, collapse = ", "))
    })
  })
}

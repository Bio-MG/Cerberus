# =============================================================================
# modules/bulk/mod_bulk_dose_response.R — Dose–réponse / time-course (NEW-1)
# =============================================================================
# UI + orchestration UNIQUEMENT : tout le calcul vit dans tools/pkg_skeleton/R/dose_response.R
# (contrat gelé : docs/contracts/BULK_DOSE_RESPONSE_CONTRACT.md).
#
# Sources de gènes : résultat DE du contraste actif (up / down / all_sig),
# top N déclarés — même pattern que mod_bulk_pathways / mod_bulk_pattern.
# Colonne de dose/temps : NUMÉRIQUE, déclarée (jamais déduite).
# Dépendance drc (justification renv.lock au commit NEW-1).
# =============================================================================

mod_bulk_dose_response_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-light", style = "font-size:0.9em;border-left:3px solid #E74C3C;",
        i18n$t("Ajuste une courbe dose-réponse (Hill / log-logistique) par gène et extrait EC50, pente et intervalle de confiance.")),
    selectInput(ns("dose_column"), i18n$t("Colonne de dose/temps (numérique)"), choices = NULL),
    radioButtons(ns("dose_source"), i18n$t("Source de gènes"),
                 choices = setNames(c("up", "down", "all_sig"),
                                    c(.tr_plain("Surexprimés (up)"), .tr_plain("Sous-exprimés (down)"), .tr_plain("Tous les significatifs")))),
    fluidRow(
      column(6, numericInput(ns("dose_top_n"), i18n$t("Top N gènes (par p.adjust)"),
                             value = 20, min = 1, max = 200, step = 1)),
      column(6, selectInput(ns("dose_model"), i18n$t("Modèle"),
                            choices = setNames(bulk_dose_models(),
                                               c(.tr_plain("Log-logistique (Hill, LL.4)"), .tr_plain("Weibull type 1 (W1.4)"),
                                                 .tr_plain("Weibull type 2 (W2.4)"), .tr_plain("Log-logistique biface (BC.4)")))))
    ),
    actionButton(ns("run_dose"), i18n$t("Lancer les ajustements"), class = "btn-warning w-100", icon = icon("chart-area")),
    hr(),
    div(class = "small text-muted", textOutput(ns("dose_status")))
  )
}

mod_bulk_dose_response_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header(div(style = "display:flex;justify-content:space-between;align-items:center;",
                    h5(i18n$t("Dose–réponse (descriptif)"), class = "mb-0"),
                    downloadButton(ns("dl_dose"), i18n$t("Export CSV"), class = "btn-sm btn-info"))),
    navset_tab(
      nav_panel(i18n$t("Courbe"),
                fluidRow(
                  column(8, selectizeInput(ns("dose_gene"), i18n$t("Gène (tapez pour rechercher)"),
                                           choices = NULL, options = list(placeholder = .tr_plain("Gène (tapez pour rechercher)")))),
                  column(4, div(style = "margin-top:25px;",
                                downloadButton(ns("dl_dose_png"), i18n$t("Export PNG"), class = "btn-sm btn-secondary w-100")))
                ),
                plotOutput(ns("dose_plot"), height = "560px")),
      nav_panel(i18n$t("Table EC50"), DTOutput(ns("dose_table")))
    )
  )
}

mod_bulk_dose_response_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {

    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # ── i18n push on language switch ─────────────────────────────────────
    observeEvent(global_data$language, {
      updateSelectInput(session, "dose_column", label = .tr("Colonne de dose/temps (numérique)"))
      updateRadioButtons(session, "dose_source", label = .tr("Source de gènes"),
        choices = setNames(c("up", "down", "all_sig"),
                           c(.tr("Surexprimés (up)"), .tr("Sous-exprimés (down)"), .tr("Tous les significatifs"))),
        selected = isolate(input$dose_source) %||% "up")
      updateNumericInput(session, "dose_top_n", label = .tr("Top N gènes (par p.adjust)"))
      updateSelectInput(session, "dose_model", label = .tr("Modèle"))
      updateActionButton(session, "run_dose", label = .tr("Lancer les ajustements"))
      updateSelectizeInput(session, "dose_gene", label = .tr("Gène (tapez pour rechercher)"))
    }, ignoreInit = TRUE)

    # Colonnes de dose/temps candidates : numériques/logiques, ou character
    # entièrement convertible en numérique
    observeEvent(global_data$bulk_obj, {
      req(global_data$bulk_obj$metadata)
      meta <- global_data$bulk_obj$metadata
      ok <- vapply(names(meta), function(cl) {
        x <- meta[[cl]]
        if (is.numeric(x) || is.logical(x)) return(TRUE)
        if (!is.character(x)) return(FALSE)
        nx <- suppressWarnings(as.numeric(stats::na.omit(x)))
        length(nx) > 0 && all(is.finite(nx))
      }, logical(1))
      updateSelectInput(session, "dose_column", choices = names(meta)[ok])
    }, ignoreNULL = TRUE)

    observe({
      shinyjs::toggleState("run_dose", condition = !is.null(shared_rv$vst_mat))
    })

    .active_de_results <- function() {
      ac <- shared_rv$active_contrast
      if (is.null(ac) || !ac %in% names(shared_rv$contrasts)) return(NULL)
      shared_rv$contrasts[[ac]]
    }

    output$dose_status <- renderText({
      global_data$language
      res <- shared_rv$dose_result
      if (is.null(res)) .tr("En attente — lancez d'abord le Filtrage & VST (étape 1) et l'analyse différentielle (étape 2).")
      else .t_fmt(.tr("\u2713 {g}/{t} ajustements convergés ({m}, colonne '{c}')."),
                  g = res$summary$n_fit_ok, t = res$summary$n_genes_used,
                  m = res$model, c = res$dose_column)
    })

    observeEvent(input$run_dose, {
      req(shared_rv$vst_mat)
      res_de <- .active_de_results()
      if (is.null(res_de)) {
        showNotification(.tr("⚠️ Lancez d'abord l'étape 2 (Analyse Différentielle)."), type = "warning")
        return()
      }
      sig <- res_de$padj < (shared_rv$padj_thresh %||% 0.05) &
        abs(res_de$log2FoldChange) > (shared_rv$lfc_thresh %||% 1)
      sig[is.na(sig)] <- FALSE
      o <- order(res_de$padj)  # tri par p.adjust croissant
      genes <- switch(input$dose_source,
                      up      = res_de$gene[o][sig[o] & res_de$log2FoldChange[o] > 0],
                      down    = res_de$gene[o][sig[o] & res_de$log2FoldChange[o] < 0],
                      all_sig = res_de$gene[o][sig[o]])
      genes <- head(genes, input$dose_top_n %||% 20)
      if (length(genes) < 1) {
        showNotification(.tr("⚠️ Aucun gène significatif — élargissez la source ou les seuils."), type = "warning")
        return()
      }
      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message = .tr("Ajustements dose-réponse (drc)..."), value = 0.3)
      tryCatch({
        res <- run_dose_response(
          vst_mat     = shared_rv$vst_mat,
          metadata    = global_data$bulk_obj$metadata,
          dose_column = input$dose_column,
          genes       = genes,
          model       = input$dose_model
        )
        assert_bulk_dose_result(res, context = "module dose-réponse")
        shared_rv$dose_result <- res
        ok <- res$fits$fit_ok
        updateSelectizeInput(session, "dose_gene",
                             choices = setNames(res$fits$gene[ok],
                                                sprintf("%s (EC50=%.3g, R2=%.2f)",
                                                        res$fits$gene[ok], res$fits$ec50[ok], res$fits$r2[ok])),
                             selected = res$fits$gene[ok][1], server = TRUE)
        showNotification(.t_fmt(.tr("\u2713 {g} courbe(s) ajustée(s) sur {t} gène(s)."),
                                 g = res$summary$n_fit_ok, t = res$summary$n_genes_used),
                         type = "message")
        shared_rv$active_tab <- "tab_dose"
      }, error = function(e) {
        showNotification(paste(.tr("Erreur dose-réponse:"), conditionMessage(e)),
                         type = "error", duration = 10)
        shared_rv$dose_result <- NULL
      })
    })

    output$dose_plot <- renderPlot({
      global_data$language
      req(shared_rv$dose_result, input$dose_gene)
      tryCatch(
        plot_dose_response_curve(shared_rv$dose_result, input$dose_gene, tr = .tr),
        error = function(e) {
          ggplot() +
            annotate("text", x = 1, y = 1, label = paste(.tr("Erreur:"), conditionMessage(e)), color = "red") +
            theme_void()
        }
      )
    })

    output$dose_table <- renderDT({
      req(shared_rv$dose_result)
      tab <- build_dose_table_export(shared_rv$dose_result)
      tab$message[is.na(tab$message)] <- ""
      ts_datatable(tab, page_length = 15L, filename_base = "dose_response")
    })

    output$dl_dose <- downloadHandler(
      filename = function() {
        req(shared_rv$dose_result)
        paste0("dose_response_", shared_rv$dose_result$model, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(shared_rv$dose_result)
        utils::write.csv(build_dose_table_export(shared_rv$dose_result), file, row.names = FALSE)
      }
    )

    .dose_plot_fn <- function() {
      req(shared_rv$dose_result, input$dose_gene)
      plot_dose_response_curve(shared_rv$dose_result, input$dose_gene, tr = .tr)
    }
    output$dl_dose_png <- downloadHandler(
      filename = function() paste0("dose_curve_", input$dose_gene, "_", Sys.Date(), ".png"),
      content  = function(file) {
        png(file, width = 9, height = 7, units = "in", res = 300)
        print(.dose_plot_fn())
        dev.off()
      }
    )
  })
}

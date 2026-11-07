# =============================================================================
# mod_bulk_de_summary.R — Bulk Child 2: Resume Up/Down tab
# (Step-3.6 refactor — extracted from the monolithic mod_bulk_de.R)
# =============================================================================
# Barchart + table of significant Up/Down genes across ALL computed
# contrasts. Reuses the same helper as the HTML/PDF report
# (summarize_contrasts_updown(), helpers_bulk.R) — single source of truth —
# and recomputes LIVE on input$lfc_thresh/padj_thresh, with no DE recalculation.
# =============================================================================

.de_summary_server <- function(input, output, session, ns, global_data, shared_rv) {

  .tr <- function(key) {
    tr <- global_data$i18n
    if (is.null(tr)) return(key)
    tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
  }


  updown_summary <- reactive({
    req(length(shared_rv$contrasts) > 0)
    summarize_contrasts_updown(shared_rv$contrasts, lfc_thresh = input$lfc_thresh,
                               padj_thresh = input$padj_thresh,
                               active_contrast = shared_rv$active_contrast)
  })

  updown_plot <- reactive({
    global_data$language                     # i18n trigger
    plot_updown_barchart(updown_summary(), tr = .tr_fn(global_data),
                         palette = shared_rv$bulk_palette %||% "default",
                         manual_colors = shared_rv$volcano_role_colors)
  })

  output$plot_updown <- renderPlot({
    validate(need(length(shared_rv$contrasts) > 0,
                  .tr("Aucun contraste calculé — lancez l'Étape 2 (DE) d'abord.")))
    updown_plot()
  })

  output$dl_updown_png <- downloadHandler(
    filename = function() paste0("updown_summary_", Sys.Date(), ".png"),
    content  = function(file) ts_export_plot(file, updown_plot(), width = 8, height = 5.5, dpi = 300)
  )

  output$table_updown <- renderDT({
    global_data$language  # i18n
    # R-3bis (fix 2026-11-06) : lecture du miroir d'onglet (mod_bulk.R) —
    # ré-exécute ce render au moment où l'onglet devient VISIBLE, ce qui
    # débloque la DT « empoisonnée » par une invalidation pendant cachée
    # (mécanisme mesuré en app minimale : Shiny 1.14 + DT 0.34 + bslib).
    force(shared_rv$main_tabs_current)
    req(updown_summary())
    df <- updown_summary()
    df$Actif <- ifelse(df$actif, .tr("\u2192 actif"), "")
    df$actif <- NULL
    colnames(df) <- c(.tr("Contraste"), .tr("G\u00e8nes test\u00e9s"), .tr("Significatifs"),
                      "Up", "Down", .tr("Actif"))
    ts_datatable(df, page_length = 15L, filename_base = "bulk_de_summary",
                 filter = "none")
  })

  output$dl_updown_csv <- downloadHandler(
    filename = function() paste0("updown_summary_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(updown_summary(), file, row.names = FALSE)
  )

  # R-3 (fix 2026-11-06, clan BULK-7 « jamais rendu ») : mesuré live (probe
  # DOM), la DT Up/Down n'a JAMAIS reçu son payload (aucun script[data-for])
  # quand son invalidation a eu lieu dans un onglet inactif — la reprise de
  # suspension ne la re-flushe pas, alors que le plot du même reactive rend.
  # Idiome Shiny standard : désactiver la suspension de CETTE sortie — elle
  # calcule et envoie son payload même cachée ; le binding DT dessine (et
  # redessine à l'activation via les hooks de redimensionnement bslib).
  outputOptions(output, "table_updown", suspendWhenHidden = FALSE)
}

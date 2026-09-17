# =============================================================================
# mod_bulk_de_multimethod.R — Bulk Child 2: multi-method comparison
# (Step-3.6 refactor — extracted from the monolithic mod_bulk_de.R)
# =============================================================================
# Runs the SAME contrast (Cible/Référence from the Step 2 sidebar) through
# DESeq2 + edgeR + limma-voom via getAllDE(), then computes a rank consensus
# via rankConsensus() — both in helpers_bulk.R. Also owns the Venn/UpSet
# comparison ACROSS METHODS (distinct from mod_bulk_de_venn.R, which compares
# ACROSS CONTRASTS for a single method).
#
# Step-3.6 BUG FIX: the pre-refactor version stored results in local
# `reactiveVal()`s (multi_de_rv/consensus_rv) that were NEVER copied to
# shared_rv, so mod_bulk_report.R always received multimethod_de=NULL —
# the report's "Multi-methodes" section could never show real data even
# after a successful comparison. A later, incomplete patch then wrote BOTH
# the local reactiveVal()s AND shared_rv (redundant, with a confusing
# dead-code branch in the rankConsensus error handler). This version keeps
# state in EXACTLY ONE place — shared_rv$multimethod_de /
# shared_rv$multimethod_consensus — read directly by every output below AND
# by mod_bulk_report.R across the module boundary. multimethod_status_rv
# stays as a local reactiveVal since it is pure UI text, not report state.
#
# Depends on helpers_bulk.R: check_design_confounding(), build_dds(),
#   getAllDE(), rankConsensus(), build_contrast_gene_sets(),
#   plot_upset_contrasts(), plot_venn_contrasts().
# Depends on: helpers$design_str() — see mod_bulk_de_engine.R::.de_make_helpers().
#
# State contract (shared_rv):
#   WRITE : shared_rv$multimethod_de         — named list (deseq2/edger/limma)
#           shared_rv$multimethod_consensus  — rankConsensus() data.frame or NULL
#           shared_rv$dds_full               — reused/refit as needed
# =============================================================================

.de_multimethod_server <- function(input, output, session, ns, global_data, shared_rv, helpers) {

  .tr <- function(key) {
    tr <- global_data$i18n
    if (is.null(tr)) return(key)
    tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
  }


  multimethod_status_rv <- reactiveVal(NULL)

  observeEvent(input$run_multimethod, {
    req(shared_rv$filtered_counts, input$condition_col, input$group_ref, input$group_target)

    if (input$group_ref == input$group_target) {
      showNotification(.tr("⚠️ Le groupe Référence et le groupe Cible doivent être différents."),
                       type = "warning"); return()
    }
    meta <- global_data$bulk_obj$metadata

    # Same hard blocks as the single-engine run (confounding / single-level
    # covariate) — duplicated check, not factored out, to avoid touching the
    # already-tested single-engine path while adding this new one.
    covariates_in_use <- input$covariates %||% character(0)
    confounded <- Filter(function(cov) check_design_confounding(meta, input$condition_col, cov),
                         covariates_in_use)
    if (length(confounded) > 0) {
      showNotification(.t_fmt(.tr("\u274c Covariable(s) confondue(s) avec '{col}' : {covs}. Retirez-la(les) du design ou revoyez votre plan d'exp\u00e9rience."),
                              col = input$condition_col, covs = paste(confounded, collapse = ", ")),
                       type = "error", duration = 10); return()
    }

    # HARD BLOCK : plan saturé (n = p). Le consensus multi-méthodes exige les
    # trois moteurs, et AUCUN ne peut estimer une dispersion ici. Le mode
    # exploratoire reste réservé au contraste unique (onglet DE / pipeline auto).
    sat <- design_saturation(meta, input$condition_col, covariates_in_use)
    if (sat$saturated) {
      showNotification(
        .t_fmt(.tr("\u274c Plan sans r\u00e9plicat : {n} \u00e9chantillon(s) pour {p} coefficient(s). La comparaison multi-m\u00e9thodes exige une dispersion estimable \u2014 elle est impossible ici (utilisez un contraste unique en mode exploratoire)."),
               n = sat$n, p = sat$p),
        type = "error", duration = 12); return()
    }

    p <- shiny::Progress$new(); on.exit(p$close())
    p$set(message = .tr("Comparaison multi-méthodes..."), value = 0.15)

    tryCatch({
      design_str <- helpers$design_str()

      # Reuse the existing dds_full ONLY if it already matches this exact
      # design (cheap heuristic: same design formula string) — otherwise
      # refit. Avoids re-running DESeq() needlessly when the user just
      # clicked "Comparer" right after "Lancer l'Analyse Différentielle"
      # with the deseq2 engine already selected (the common case).
      dds_full <- shared_rv$dds_full
      needs_fit <- is.null(dds_full) ||
        !isTRUE(identical(attr(dds_full, "design_str_cache"), design_str))
      if (needs_fit) {
        p$set(0.3, .tr("Ajustement DESeq2 (requis pour le consensus)..."))
        dds_full <- build_dds(shared_rv$filtered_counts, meta, design_formula = design_str, run_deseq = TRUE)
        attr(dds_full, "design_str_cache") <- design_str
        shared_rv$dds_full <- dds_full
      }

      p$set(0.5, .tr("DESeq2 + edgeR + limma-voom..."))
      de_list <- getAllDE(shared_rv$filtered_counts, meta, input$condition_col,
                          input$group_target, input$group_ref,
                          dds_full = dds_full, shrink = input$shrink_lfc,
                          covariates = input$covariates %||% character(0),
                          # STAT-Q1 : la MÊME méthode est appliquée aux 3 moteurs,
                          # sinon le consensus de rang comparerait des padj
                          # corrigés différemment selon la méthode.
                          p_adjust_method = input$padj_method %||% TS_PADJ_METHOD_DEFAULT)

      if (length(de_list) < 2) {
        stop(.t_fmt(.tr("Au moins 2 m\u00e9thodes doivent r\u00e9ussir pour comparer ({n} r\u00e9ussies). V\u00e9rifiez que edgeR/limma sont install\u00e9s."),
                    n = length(de_list)))
      }

      p$set(0.85, .tr("Consensus de rang..."))
      cons <- tryCatch(
        rankConsensus(de_list, input$lfc_thresh, input$padj_thresh),
        error = function(e) { warning(conditionMessage(e)); NULL }
      )

      # Single source of truth — read directly by this module's own outputs
      # below AND by mod_bulk_report.R (across the module boundary).
      shared_rv$multimethod_de        <- de_list
      shared_rv$multimethod_consensus <- cons

      # i18n-safe STATE: language-neutral data only — the display sentence is
      # rebuilt at render time in the CURRENT language (see
      # output$multimethod_status_ui below), mirroring the mapping module's
      # mapping_summary_data pattern.
      multimethod_status_rv(list(
        n_methods = length(de_list),
        methods   = paste(names(de_list), collapse = ", "),
        target    = input$group_target,
        ref       = input$group_ref
      ))
      showNotification(.t_fmt(.tr("Comparaison multi-méthodes terminée ({methods})"),
                               methods = paste(names(de_list), collapse = ", ")),
                       type = "message", duration = 6)
      shared_rv$active_tab <- "tab_multimethod"

    }, error = function(e) {
      multimethod_status_rv(NULL)
      shared_rv$multimethod_de        <- NULL
      shared_rv$multimethod_consensus <- NULL
      showNotification(paste(.tr("Erreur comparaison multi-méthodes:"), e$message),
                       type = "error", duration = 10)
    })
  })

  output$multimethod_status_ui <- renderUI({
    global_data$language
    .trl <- function(key) { tr <- global_data$i18n; if (is.null(tr)) return(key)
                            tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key) }

    st <- multimethod_status_rv()
    if (is.null(st)) {
      div(class = "alert alert-info", style = "font-size:0.85em;",
          .trl("Cliquez \"🔬 Comparer DESeq2 / edgeR / limma-voom\" dans le panneau Step 2."))
    } else {
      # Rebuild the sentence in the CURRENT language from language-neutral data
      div(class = "alert alert-success", style = "font-size:0.85em;",
          .t_fmt(.trl("✓ {n} méthode(s) comparée(s) : {m} ({a} vs {b})."),
                 n = st$n_methods, m = st$methods, a = st$target, b = st$ref))
    }
  })

  # LIVE recompute on threshold change, same pattern as venn_gene_sets in
  # mod_bulk_de_venn.R.
  mm_gene_sets <- reactive({
    de_list <- shared_rv$multimethod_de
    req(length(de_list) >= 2)
    build_contrast_gene_sets(de_list, lfc_thresh = input$lfc_thresh, padj_thresh = input$padj_thresh)
  })

  output$mm_venn_plot <- renderPlot({
    global_data$language                     # i18n trigger (diagrams are language-neutral)
    w <- session$clientData[[paste0("output_", "mm_venn_plot", "_width")]]
    h <- session$clientData[[paste0("output_", "mm_venn_plot", "_height")]]
    if (isTRUE(w < 30) || isTRUE(h < 30)) {
      grid::grid.newpage()
      grid::grid.text(.tr("Conteneur trop petit pour afficher le diagramme."),
                      gp = grid::gpar(col = "grey40", fontsize = 12))
      return(invisible(NULL))
    }
    sets <- tryCatch(mm_gene_sets(), error = function(e) NULL)
    validate(need(!is.null(sets), .tr("Lancez d'abord la comparaison multi-m\u00e9thodes.")))
    tryCatch({
      if (input$mm_venn_type == "venn") plot_venn_contrasts(sets, palette = shared_rv$bulk_palette %||% "default") else plot_upset_contrasts(sets)
    }, error = function(e) {
      grid::grid.newpage()
      grid::grid.text(paste0("Erreur : ", conditionMessage(e), "\n(ou conteneur trop petit — agrandissez la fenêtre/onglet)"),
                      gp = grid::gpar(col = "firebrick", fontsize = 11))
    })
  })

  output$dl_mm_venn_png <- downloadHandler(
    filename = function() paste0("venn_methodes_", Sys.Date(), ".png"),
    content  = function(file) {
      sets <- mm_gene_sets()
      png(file, width = 9, height = 7, units = "in", res = 300)
      if (input$mm_venn_type == "venn") plot_venn_contrasts(sets, palette = shared_rv$bulk_palette %||% "default") else plot_upset_contrasts(sets)
      dev.off()
    }
  )

  output$mm_consensus_table <- renderDT({
    df <- shared_rv$multimethod_consensus
    req(df)
    df_display <- df
    num_cols <- setdiff(colnames(df_display), c("gene", "consistent_sign"))
    for (cl in num_cols) df_display[[cl]] <- round(df_display[[cl]], 4)
    ts_datatable(df_display, page_length = 15L,
                 filename_base = "bulk_de_multimethod_consensus") %>%
      formatStyle("n_methods_sig",
                  background = styleColorBar(range(df_display$n_methods_sig), "#F39C12"),
                  backgroundSize = "98% 88%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })

  output$dl_mm_consensus_csv <- downloadHandler(
    filename = function() paste0("consensus_rang_", Sys.Date(), ".csv"),
    content  = function(file) { req(shared_rv$multimethod_consensus); write.csv(shared_rv$multimethod_consensus, file, row.names = FALSE) }
  )
}

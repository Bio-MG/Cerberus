# =============================================================================
# mod_bulk_merge.R — NEW-2 : fusion de jeux bulk ("Merge Data") — UI +
# orchestration. Panneau « Multi-jeux — Fusion de jeux » + onglet de sortie.
# =============================================================================
# Logique pure : R/bulk/bulk_merge.R (contrat gelé BULK_MERGE_CONTRACT.md).
# Le module écrit UNIQUEMENT global_data$bulk_obj (comportement d'un import —
# contrat §7) : il ne reçoit pas l'état réactif du pipeline (shared state),
# n'écrit jamais le conteneur bulk_datasets. Les étages (Filtrage -> DE) se
# recalculent quand l'utilisateur relance chaque étape.
#
# Réutilisation stricte (garde du test de gel) : bulk_merge_* (moteur),
# bulk_batch_correction_design() (STAT-S1, aperçu du plan), build_dds()/
# get_vst_matrix() (chaîne de transformation canonique), plot_bulk_pca() +
# plot_batch_correction_pca() (tracés), ts_datatable() (table).
# =============================================================================

mod_bulk_merge_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-light", style = "font-size:0.78rem;padding:5px;margin-bottom:5px;",
        bsicons::bs_icon("info-circle"),
        " ", i18n$t("Fusionne >= 2 jeux enregistrés (Multi-jeux — Datasets enregistrés) : gènes communs en intersection exacte, harmonisation ComBat-seq optionnelle (lot = jeu d'origine). Le produit est chargé comme jeu actif, comme un import.")),
    selectizeInput(ns("merge_datasets"), i18n$t("Datasets à fusionner (>= 2)"),
                   choices = NULL, multiple = TRUE,
                   options = list(placeholder = .tr_plain("Sélectionnez des datasets..."))),
    selectizeInput(ns("merge_condition"), i18n$t("Colonne condition commune (optionnel)"),
                   choices = NULL,
                   options = list(placeholder = .tr_plain("Aucune — correction à l'aveugle si ComBat"))),
    checkboxInput(ns("merge_combat"),
                  i18n$t("Harmonisation ComBat-seq (lot = jeu d'origine)"),
                  value = FALSE),
    textInput(ns("merge_project"), i18n$t("Nom du jeu fusionné"),
              placeholder = .tr_plain("ex : Fusion_GSE123_GSE456")),
    uiOutput(ns("merge_status")),
    actionButton(ns("merge_run"), i18n$t("Fusionner et charger comme jeu actif"),
                 icon = icon("object-group"), class = "btn-primary w-100 mb-2"),
    div(class = "alert alert-warning", style = "font-size:0.78rem;padding:5px;",
        icon("triangle-exclamation"), " ",
        i18n$t("Le jeu actif courant sera remplacé (comme un import). Enregistrez-le d'abord via « Enregistrer l'état courant » si nécessaire."))
  )
}

mod_bulk_merge_output_ui <- function(id) {
  ns <- NS(id)
  navset_card_underline(
    id = ns("merge_tabs"), title = i18n$t("Résultat de la fusion"),
    nav_panel(i18n$t("Résumé"), value = "tab_merge_summary",
              verbatimTextOutput(ns("merge_status_line"), placeholder = TRUE),
              h6(i18n$t("Gènes par dataset"), style = "font-weight:bold; margin-top:12px;"),
              DTOutput(ns("merge_summary")),
              uiOutput(ns("merge_renames_ui"))),
    nav_panel(i18n$t("PCA avant / après"), value = "tab_merge_pca",
              uiOutput(ns("merge_pca_help")),
              plotOutput(ns("merge_pca"), height = "560px"))
  )
}

mod_bulk_merge_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    last_merge <- reactiveVal(NULL)

    # ── Eligible datasets: entries with a counts matrix ────────────────────
    eligible <- reactive({
      ds <- global_data$bulk_datasets
      keep <- names(ds)[vapply(ds, function(e) {
        !is.null(e) && is.list(e) && !is.null(e$obj) && !is.null(e$obj$counts)
      }, logical(1))]
      sort(keep)
    })

    observeEvent(eligible(), {
      prev <- intersect(input$merge_datasets %||% character(0), eligible())
      updateSelectizeInput(session, "merge_datasets", choices = eligible(),
                           selected = prev, server = FALSE)
    })

    # ── Condition column choices: intersection across the SELECTION ────────
    observeEvent(input$merge_datasets, {
      global_data$language  # i18n
      sel <- input$merge_datasets %||% character(0)
      if (length(sel) < 2L) {
        updateSelectizeInput(session, "merge_condition", choices = NULL,
                             server = FALSE)
        return()
      }
      ds <- global_data$bulk_datasets[sel]
      common_cols <- tryCatch(bulk_merge_common_meta_columns(ds),
                              error = function(e) character(0))
      prev <- if (!is.null(input$merge_condition) &&
                  input$merge_condition %in% common_cols) {
        input$merge_condition
      } else {
        NULL
      }
      updateSelectizeInput(session, "merge_condition", choices = common_cols,
                           selected = prev, server = FALSE)
    })

    # ── Live previews BEFORE running: gene alignment + ComBat design ───────
    selected_ds <- reactive({
      sel <- input$merge_datasets %||% character(0)
      if (length(sel) < 2L) return(NULL)
      global_data$bulk_datasets[sel]
    })

    alignment_preview <- reactive({
      ds <- selected_ds()
      if (is.null(ds)) return(NULL)
      tryCatch(bulk_merge_align_genes(ds), error = function(e) e)
    })

    combat_design <- reactive({
      if (!isTRUE(input$merge_combat)) return(NULL)
      ds <- selected_ds()
      if (is.null(ds)) return(NULL)
      tryCatch({
        meta_prev <- bulk_merge_preview_metadata(
          ds, batch_col = "dataset_origin")
        bulk_batch_correction_design(
          meta_prev$metadata, "dataset_origin",
          if (nzchar(input$merge_condition %||% "")) input$merge_condition else NULL)
      }, error = function(e) e)
    })

    output$merge_status <- renderUI({
      global_data$language  # i18n
      if (length(input$merge_datasets %||% character(0)) < 2L) {
        return(div(class = "alert alert-warning", style = "font-size:0.8rem;",
                   .tr("Sélectionnez au moins 2 datasets enregistrés.")))
      }
      al <- alignment_preview()
      if (inherits(al, "error")) {
        return(div(class = "alert alert-danger", style = "font-size:0.8rem;",
                   tags$strong(.tr("Alignement impossible : ")),
                   conditionMessage(al)))
      }
      parts <- sprintf(.tr("Gènes communs : %d (%s)."),
                       al$n_common_genes,
                       paste(sprintf("%s %d/%d", al$per_dataset$label,
                                     al$per_dataset$n_genes_common,
                                     al$per_dataset$n_genes_total),
                             collapse = ", "))
      ren <- selected_ds()
      n_renamed <- tryCatch({
        m <- bulk_merge_preview_metadata(ren, batch_col = "dataset_origin")
        nrow(m$renames)
      }, error = function(e) -1L)
      if (n_renamed > 0L) {
        parts <- paste(parts, sprintf(.tr("%d échantillon(s) seront renommés (collision de noms)."), n_renamed))
      }
      ui <- div(class = "alert alert-info", style = "font-size:0.8rem;", parts)
      cd <- combat_design()
      if (!is.null(cd)) {
        if (inherits(cd, "error")) {
          ui <- tagList(ui, div(class = "alert alert-danger",
                                style = "font-size:0.8rem;",
                                paste0("⚠️ ", conditionMessage(cd))))
        } else if (!isTRUE(cd$can_apply)) {
          ui <- tagList(ui, div(class = "alert alert-danger",
                                style = "font-size:0.8rem;",
                                tags$strong(.tr("Harmonisation impossible : ")),
                                paste(cd$blocking_messages, collapse = " ")))
        } else {
          ui <- tagList(ui, div(class = "alert alert-success",
                                style = "font-size:0.8rem;",
                                .tr("✓ Harmonisation ComBat-seq possible."),
                                " ",
                                if (isTRUE(cd$use_group))
                                  .tr("La condition déclarée sera préservée.")
                                else
                                  .tr("Aucune condition commune déclarée : correction à l'aveugle.")))
        }
      }
      ui
    })

    # ── Run (synchronous — bulk V1.0 convention) ────────────────────────────
    observeEvent(input$merge_run, {
      global_data$language  # i18n
      ds <- selected_ds()
      if (is.null(ds)) {
        showNotification(.tr("Sélectionnez au moins 2 datasets enregistrés."),
                         type = "warning", duration = 6)
        return()
      }
      cond <- if (nzchar(input$merge_condition %||% "")) {
        input$merge_condition
      } else {
        NULL
      }

      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message = .tr("Fusion des jeux..."), value = 0.2)

      res <- tryCatch(
        bulk_merge_run(ds, condition_col = cond,
                       apply_combat = isTRUE(input$merge_combat)),
        bulk_merge_error = function(e) e,
        bulk_multi_error = function(e) e,
        bulk_batch_correction_error = function(e) e)
      if (inherits(res, "error")) {
        showNotification(sprintf(.tr("Fusion impossible : %s"), conditionMessage(res)),
                         type = "error", duration = 10)
        return()
      }

      # Diagnostic PCA matrices — canonical transform chain (same as the
      # Filtering stage and the STAT-S1 batch-correction diagnostic).
      p$set(0.6, .tr("Transformation VST (diagnostic) ..."))
      .vst_of <- function(counts) {
        dds <- build_dds(counts, res$metadata, design_formula = "~1",
                         run_deseq = FALSE)
        dds <- DESeq2::estimateSizeFactors(dds)
        get_vst_matrix(dds)
      }
      res$before_vst <- tryCatch(.vst_of(res$counts_pre_combat %||% res$counts),
                                 error = function(e) e)
      res$after_vst  <- if (isTRUE(res$combat_applied)) {
        tryCatch(.vst_of(res$counts), error = function(e) e)
      } else {
        NULL
      }

      p$set(0.9, .tr("Chargement du jeu fusionné..."))
      obj <- tryCatch(bulk_merge_to_bulk_obj(res, input$merge_project),
                      bulk_merge_error = function(e) e)
      if (inherits(obj, "error")) {
        showNotification(sprintf(.tr("Fusion impossible : %s"), conditionMessage(obj)),
                         type = "error", duration = 8)
        return()
      }
      global_data$bulk_obj <- obj
      global_data$bulk_obj_epoch <- (global_data$bulk_obj_epoch %||% 0L) + 1L  # purge résultats partagés (jeu remplacé)
      last_merge(res)

      msg <- sprintf(.tr("✓ Fusion chargée : %d gènes communs × %d échantillons (%s)."),
                     res$n_common_genes, res$n_samples,
                     paste(res$labels, collapse = " + "))
      if (isTRUE(res$combat_applied)) {
        msg <- paste(msg, .tr("Harmonisation ComBat-seq appliquée (lot = jeu d'origine)."))
      }
      showNotification(msg, type = "message", duration = 8)
    })

    # ── Summary outputs ─────────────────────────────────────────────────────
    output$merge_status_line <- renderText({
      global_data$language  # i18n
      res <- last_merge()
      if (is.null(res)) {
        return(.tr("En attente — sélectionnez >= 2 datasets puis lancez la fusion."))
      }
      sprintf("%s — %d gènes communs × %d échantillons [%s]",
              paste(res$labels, collapse = " + "), res$n_common_genes,
              res$n_samples,
              if (isTRUE(res$combat_applied)) "ComBat-seq" else .tr("sans correction"))
    })

    output$merge_summary <- renderDT({
      global_data$language  # i18n
      res <- last_merge()
      validate(need(!is.null(res),
                    .tr("En attente — lancez d'abord la fusion.")))
      ts_datatable(res$per_dataset, page_length = 15L,
                   filename_base = "bulk_merge_per_dataset")
    })

    output$merge_renames_ui <- renderUI({
      global_data$language  # i18n
      res <- last_merge()
      req(res)
      if (is.null(res$renames) || nrow(res$renames) == 0L) {
        return(tags$div(class = "small text-muted",
                        .tr("Aucun renommage d'échantillon (noms uniques entre les jeux).")))
      }
      tagList(
        h6(i18n$t("Échantillons renommés (collision de noms)"),
           style = "font-weight:bold; margin-top:12px;"),
        DTOutput(ns("merge_renames"))
      )
    })

    output$merge_renames <- renderDT({
      global_data$language  # i18n
      res <- last_merge()
      req(res, !is.null(res$renames), nrow(res$renames) > 0L)
      ts_datatable(res$renames, page_length = 15L,
                   filename_base = "bulk_merge_renames")
    })

    output$merge_pca_help <- renderUI({
      global_data$language  # i18n
      res <- last_merge()
      if (is.null(res)) {
        return(tags$div(class = "alert alert-light", style = "font-size:0.8rem;",
                        .tr("Diagnostic descriptif — lancez d'abord la fusion. A : avant correction (jeux attendus séparés) ; B : après ComBat-seq (jeux attendus mélangés).")))
      }
      if (isTRUE(res$combat_applied) && !inherits(res$after_vst, "error")) {
        return(NULL)
      }
      tags$div(class = "alert alert-light", style = "font-size:0.8rem;",
               .tr("PCA descriptive colorée par jeu d'origine (pas de correction appliquée, ou diagnostic indisponible)."))
    })

    output$merge_pca <- renderPlot({
      global_data$language  # i18n
      res <- last_merge()
      req(res)
      validate(need(!inherits(res$before_vst, "error"),
                    .tr("Transformation VST impossible sur la fusion (diagnostic indisponible).")))
      before <- plot_bulk_pca(res$before_vst, res$metadata,
                              color_by = res$batch_col, tr = .tr)
      if (isTRUE(res$combat_applied) && !is.null(res$after_vst) &&
          !inherits(res$after_vst, "error")) {
        after <- plot_bulk_pca(res$after_vst, res$metadata,
                               color_by = res$batch_col, tr = .tr)
        plot_batch_correction_pca(before, after, tr = .tr)
      } else {
        before
      }
    })

    # ── i18n push on language switch ────────────────────────────────────────
    observeEvent(global_data$language, {
      updateSelectizeInput(session, "merge_datasets",
                           label = .tr("Datasets à fusionner (>= 2)"))
      updateSelectizeInput(session, "merge_condition",
                           label = .tr("Colonne condition commune (optionnel)"))
      updateCheckboxInput(session, "merge_combat",
                          label = .tr("Harmonisation ComBat-seq (lot = jeu d'origine)"))
      updateTextInput(session, "merge_project",
                      label = .tr("Nom du jeu fusionné"),
                      placeholder = .tr("ex : Fusion_GSE123_GSE456"))
      updateActionButton(session, "merge_run",
                         label = .tr("Fusionner et charger comme jeu actif"))
    })
  })
}

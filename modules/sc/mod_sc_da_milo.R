# =============================================================================
# mod_sc_da_milo.R — Abondance differentielle 4E-1 : Milo (Stage 14)
# =============================================================================
# Le module ORCHESTRE uniquement : la porte de design (assert_da_design_result
# dans run_milo_da), le calcul (run_milo_da), le resultat canonique, les vues
# pures (sc_abundance_milo_views.R), la provenance et les exports residuent
# dans R/sc/. Le design Stage 13 (panel 8c) est consomme via
# shared_rv$da_design_result — Milo est IMPOSSIBLE sans design valide.
# Affichage : le signal est NIVEAU VOISINAGE, jamais un DA par type cellulaire.
# =============================================================================

mod_sc_da_milo_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-light",
        style = "font-size:0.9em;border-left:3px solid #C0392B;",
        i18n$t("Milo teste l'abondance différentielle PAR VOISINAGE (régions d'un graphe kNN) — ce n'est PAS un test par type cellulaire. "),
        i18n$t("Le design du panel 8c est OBLIGATOIRE : un design bloqué ou périmé est refusé.")),

    selectInput(ns("milo_reduction"), i18n$t("Réduction de l'espace latent (graphe)"),
                choices = character(0), width = "100%"),
    fluidRow(
      column(6, selectInput(ns("milo_target"), i18n$t("Condition cible (logFC > 0)"),
                            choices = character(0), width = "100%")),
      column(6, selectInput(ns("milo_reference"), i18n$t("Condition référence"),
                            choices = character(0), width = "100%"))
    ),
    selectInput(ns("milo_identity"), i18n$t("Annotation d'identité des voisinages (descriptive)"),
                choices = c("(définie par le design)" = "", "(aucune)" = "__none__"),
                selected = "", width = "100%"),
    checkboxInput(ns("milo_use_batch"), i18n$t("Inclure le terme batch (design) dans le modèle"), value = TRUE),
    fluidRow(
      column(3, numericInput(ns("milo_k"), "k", value = TS_DA_MILO_K, min = 3, step = 1)),
      column(3, numericInput(ns("milo_prop"), "prop", value = TS_DA_MILO_PROP, min = 0.01, max = 1, step = 0.05)),
      column(3, numericInput(ns("milo_d"), "d", value = TS_DA_MILO_D, min = 2, step = 1)),
      column(3, numericInput(ns("milo_seed"), i18n$t("Graine"), value = TS_DA_MILO_SEED, step = 1))
    ),
    actionButton(ns("milo_run"), i18n$t("Lancer Milo"),
                 class = "btn-primary w-100", icon = icon("project-diagram")),
    div(class = "small text-muted mt-1", textOutput(ns("milo_status"))),
    hr(),
    downloadButton(ns("dl_milo_summary"), i18n$t("Exporter résumé Milo (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_milo_da_table"), i18n$t("Exporter table DA par voisinage (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_milo_assignment"), i18n$t("Exporter mappage cellules → voisinages (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_milo_composition"), i18n$t("Exporter contexte échantillons (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_milo_rds"), i18n$t("Exporter résultat (RDS)"), class = "btn-sm btn-info w-100 mt-1")
  )
}

mod_sc_da_milo_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header(i18n$t("Abondance différentielle — Milo (voisinages)")),
    navset_tab(
      nav_panel(i18n$t("Signal sur embedding"),
                plotOutput(ns("milo_embed"), height = "520px"),
                div(class = "m-1",
                    downloadButton(ns("dl_milo_embed_png"), "PNG", class = "btn-sm btn-outline-secondary"),
                    downloadButton(ns("dl_milo_embed_pdf"), "PDF", class = "btn-sm btn-outline-secondary"))),
      nav_panel(i18n$t("Distribution logFC"),
                plotOutput(ns("milo_dist"), height = "520px"),
                div(class = "m-1",
                    downloadButton(ns("dl_milo_dist_png"), "PNG", class = "btn-sm btn-outline-secondary"),
                    downloadButton(ns("dl_milo_dist_pdf"), "PDF", class = "btn-sm btn-outline-secondary"))),
      nav_panel(i18n$t("Composition échantillons"),
                plotOutput(ns("milo_compo"), height = "520px"),
                div(class = "m-1",
                    downloadButton(ns("dl_milo_compo_png"), "PNG", class = "btn-sm btn-outline-secondary"),
                    downloadButton(ns("dl_milo_compo_pdf"), "PDF", class = "btn-sm btn-outline-secondary"))),
      nav_panel(i18n$t("Table DA (voisinages)"),
                DT::dataTableOutput(ns("milo_table"), height = "460px")),
      nav_panel(i18n$t("Limites"),
                div(class = "alert alert-danger m-3", style = "font-size:0.9em;",
                    i18n$t("Le signal Milo est NIVEAU VOISINAGE (région du graphe) : l'identité d'un voisinage (fraction ≥ 0.7) est descriptive et ne constitue PAS un DA par type cellulaire.")),
                div(class = "alert alert-warning m-3", style = "font-size:0.9em;",
                    i18n$t("Les cellules ne sont pas des réplicats biologiques : le contexte de composition reste au niveau échantillon.")),
                div(class = "alert alert-light m-3", style = "font-size:0.9em;",
                    uiOutput(ns("milo_contrast_note"))),
                div(class = "alert alert-light m-3", style = "font-size:0.9em;",
                    i18n$t("Milo n'interprète PAS la composition d'un type cellulaire — pour la DA compositionnelle par échantillon, voir scCODA (4E-2, à venir).")))
    )
  )
}

mod_sc_da_milo_server <- function(id, global_data, shared_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    # ── i18n proxy ──────────────────────────────────────────────────────────
    .tr <- function(key) {
      tr <- isolate(global_data$i18n)
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    .status_label <- function(st) {
      lab <- milo_status_labels()
      if (is.character(st) && length(st) == 1L && st %in% names(lab)) lab[[st]] else st
    }

    milo_state <- reactiveValues(result = NULL, object_fingerprint = NULL)
    milo_status_rv <- reactiveVal(.tr("En attente : validez d'abord le design (panel 8c), puis lancez Milo."))

    output$milo_status <- renderText({ milo_status_rv() })

    .check_fingerprint <- function() {
      req(global_data$sc_obj)
      req(milo_state$object_fingerprint)
      current_fp <- velocity_object_fingerprint(global_data$sc_obj)
      if (!identical(milo_state$object_fingerprint, current_fp)) {
        stale_msg <- .status_label("stale_against_current_seurat_object")
        showNotification(stale_msg, type = "error", duration = 8)
        shiny::validate(shiny::need(FALSE, stale_msg))
      }
      invisible(TRUE)
    }

    # Design Stage 13 : consomme le resultat canonique expose par le panel 8c
    # (shared_rv$da_design_result) — aucune revalidation locale, aucun calcul
    # de design duplique.
    .design_result <- reactive({
      if (is.null(shared_rv)) return(NULL)
      shared_rv$da_design_result
    })

    # UN SEUL observateur par declencheur (garde anti-duplication) : reset +
    # synchronisation des choix (reduction depuis l'objet ; conditions depuis
    # le design valide).
    observeEvent(c(global_data$sc_obj, .design_result()), {
      milo_state$result <- NULL
      milo_state$object_fingerprint <- NULL
      obj <- global_data$sc_obj
      des <- .design_result()
      if (is.null(obj)) {
        updateSelectInput(session, "milo_reduction", choices = character(0))
      } else {
        reds <- names(obj@reductions)
        updateSelectInput(session, "milo_reduction", choices = reds,
                          selected = if ("pca" %in% reds) "pca" else reds[1])
      }
      if (is.null(des) || is.null(des$condition_summary) ||
          nrow(des$condition_summary) == 0L) {
        updateSelectInput(session, "milo_target", choices = character(0))
        updateSelectInput(session, "milo_reference", choices = character(0))
        milo_status_rv(.tr("En attente : validez d'abord le design (panel 8c), puis lancez Milo."))
        return()
      }
      conds <- as.character(des$condition_summary$condition)
      updateSelectInput(session, "milo_target", choices = conds,
                        selected = conds[length(conds)])
      updateSelectInput(session, "milo_reference", choices = conds,
                        selected = conds[1])
      meta_cols <- if (!is.null(obj)) colnames(obj@meta.data) else character(0)
      id_choice <- des$config$identity %||% NA_character_
      id_choices <- setNames(c("", "__none__", meta_cols),
                             c(.tr("(définie par le design)"), .tr("(aucune)"), meta_cols))
      updateSelectInput(session, "milo_identity", choices = id_choices,
                        selected = if (!is.na(id_choice) && nzchar(id_choice) &&
                                       id_choice %in% meta_cols) id_choice else "")
      milo_status_rv(.tr("Design valide disponible : choisissez le contraste puis lancez Milo."))
    }, ignoreInit = FALSE)

    # ── Lancement Milo (orchestration uniquement) ──────────────────────────
    observeEvent(input$milo_run, {
      req(global_data$sc_obj)
      des <- .design_result()
      if (is.null(des)) {
        showNotification(.tr("Aucun design validé : passez d'abord par le panel 8c (validation du plan expérimental)."),
                         type = "error", duration = 10)
        return()
      }
      tryCatch({
        obj <- assert_seurat(global_data$sc_obj, context = "Milo")
        if (is.null(input$milo_reduction) || !nzchar(input$milo_reduction)) {
          stop("Choisissez d'abord une réduction de l'espace latent.", call. = FALSE)
        }
        if (is.null(input$milo_target) || is.null(input$milo_reference) ||
            !nzchar(input$milo_target) || !nzchar(input$milo_reference) ||
            identical(input$milo_target, input$milo_reference)) {
          stop("Choisissez deux conditions DISTINCTES (cible ≠ référence).", call. = FALSE)
        }
        ident_choice <- input$milo_identity
        ident_col <- if (is.null(ident_choice) || ident_choice == "") NULL
                     else if (ident_choice == "__none__") "" else ident_choice

        # ── 4E-4 : le calcul part dans le POOL APPLICATIF partage ──────────
        # run_job() est le wrapper async UNIQUE du depot (regle 8 : aucun
        # second mecanisme de workers ; jamais MulticoreParam, jamais
        # enableWGCNAThreads). Le pool est celui de spatial_async.R, rendu
        # applicatif (APP_DAEMON_SOURCE_FILES) : il preloade la fermeture Milo,
        # sans quoi le daemon ne resout pas run_milo_da() (mesure : « could not
        # find function »).
        # Le repli synchrone de run_job() est DECLARE ici (statut + notification)
        # et non seulement journalise : l'utilisateur doit savoir que l'interface
        # va rester bloquee. scCODA reste hors du perimetre async (reticulate /
        # TensorFlow = un interpreteur Python par daemon).
        pool_ok <- isTRUE(ensure_app_daemons()) &&
                   requireNamespace("mirai", quietly = TRUE) &&
                   isTRUE(mirai::daemons_set())
        if (!pool_ok) {
          showNotification(
            .tr("Pool de calcul indisponible : l'analyse Milo tourne en synchrone — l'interface restera bloquée pendant le calcul."),
            type = "warning", duration = 10
          )
        }
        milo_status_rv(.tr("Calcul Milo en cours..."))

        res <- run_job(
          run_milo_da,
          seurat_obj          = obj,
          da_design_result    = des,
          reduction           = input$milo_reduction,
          target_condition    = input$milo_target,
          reference_condition = input$milo_reference,
          use_batch           = isTRUE(input$milo_use_batch),
          identity_col        = ident_col,
          k    = max(3L, as.integer(input$milo_k %||% TS_DA_MILO_K)),
          prop = min(max(as.numeric(input$milo_prop %||% TS_DA_MILO_PROP), 0.01), 1),
          d    = max(2L, as.integer(input$milo_d %||% TS_DA_MILO_D)),
          seed = as.integer(input$milo_seed %||% TS_DA_MILO_SEED),
          async      = TRUE,
          timeout_ms = TS_DA_MILO_TIMEOUT_MS
        )
        # Le resultat canonique est deja valide par construction ; la garde
        # explicite documente la consommation (contrat Stage 14).
        res <- assert_milo_result(res, seurat_obj = obj, context = "Milo")
        milo_state$result <- res
        milo_state$object_fingerprint <- velocity_object_fingerprint(obj)
        if (!is.null(shared_rv)) shared_rv$da_milo_result <- res

        provenance_append(shared_rv, res$provenance)

        verdict <- if (identical(res$status, "valid")) "✅" else "⚠️"
        msg <- sprintf(
          "%s %s — %d voisinage(s), contraste %s (cible '%s' vs référence '%s').",
          verdict, .status_label(res$status),
          nrow(res$DA_table %||% data.frame()),
          res$tested_contrast$contrast,
          res$tested_contrast$target, res$tested_contrast$reference
        )
        milo_status_rv(msg)
        showNotification(.tr("Analyse Milo terminée."), type = "message", duration = 4)

      }, error = function(e) {
        milo_state$result <- NULL
        milo_state$object_fingerprint <- NULL
        milo_status_rv(paste(.tr("Erreur Milo :"), conditionMessage(e)))
        showNotification(
          paste(.tr("Erreur Milo :"), conditionMessage(e)),
          type = "error", duration = 12
        )
      })
    })

    # ── Vues (consommatrices pures du resultat canonique) ──────────────────
    # L'embedding affiche la reduction ENREGISTREE au moment du calcul
    # (parameters$reduction) — changer le select apres coup ne reinterprete
    # pas le resultat.
    .embed_plot <- function() {
      plot_milo_da_embedding(
        milo_state$result, global_data$sc_obj,
        milo_state$result$parameters$reduction %||% input$milo_reduction
      )
    }
    .dist_plot <- function() plot_milo_da_distribution(milo_state$result)
    .compo_plot <- function() plot_milo_sample_composition(milo_state$result)

    output$milo_embed <- renderPlot({
      if (!.export_guard(milo_state$result)) return()
        .check_fingerprint()
      .embed_plot()
    })
    output$milo_dist <- renderPlot({
      if (!.export_guard(milo_state$result)) return()
        .check_fingerprint()
      .dist_plot()
    })
    output$milo_compo <- renderPlot({
      if (!.export_guard(milo_state$result)) return()
        .check_fingerprint()
      .compo_plot()
    })

    output$milo_table <- DT::renderDataTable({
      req(milo_state$result)
      .check_fingerprint()
      ts_datatable(milo_state$result$DA_table, page_length = 15L,
                   filename_base = "milo_da_table", filter = "none") |>
        DT::formatSignif(columns = c("logFC", "logCPM", "F", "PValue", "FDR",
                                     "SpatialFDR", "identity_fraction"),
                         digits = 4)
    })

    output$milo_contrast_note <- renderUI({
      req(milo_state$result)
      tc <- milo_state$result$tested_contrast %||% list()
      tags$span(sprintf(
        "%s — formule : %s ; correction SpatialFDR (%s) ; graine : %s.",
        tc$interpretation %||% "",
        tc$formula %||% NA_character_,
        milo_state$result$parameters$fdr_weighting %||% "k-distance",
        milo_state$result$parameters$seed %||% NA_integer_
      ))
    })

    # ── Exports (traces par analysis_id ; figures = vues pures) ────────────
    .export_figure <- function(plot_fn, file) {
      .check_fingerprint()
      ts_export_plot(file, plot_fn(), width = 8, height = 6.5, dpi = 300)
    }

    output$dl_milo_embed_png <- downloadHandler(
      filename = function() milo_export_filename(milo_state$result, "milo_embedding", "png"),
      content = function(file) { if (!.export_guard(milo_state$result)) return(); .export_figure(.embed_plot, file) }
    )
    output$dl_milo_embed_pdf <- downloadHandler(
      filename = function() milo_export_filename(milo_state$result, "milo_embedding", "pdf"),
      content = function(file) { if (!.export_guard(milo_state$result)) return(); .export_figure(.embed_plot, file) }
    )
    output$dl_milo_dist_png <- downloadHandler(
      filename = function() milo_export_filename(milo_state$result, "milo_logfc_distribution", "png"),
      content = function(file) { if (!.export_guard(milo_state$result)) return(); .export_figure(.dist_plot, file) }
    )
    output$dl_milo_dist_pdf <- downloadHandler(
      filename = function() milo_export_filename(milo_state$result, "milo_logfc_distribution", "pdf"),
      content = function(file) { if (!.export_guard(milo_state$result)) return(); .export_figure(.dist_plot, file) }
    )
    output$dl_milo_compo_png <- downloadHandler(
      filename = function() milo_export_filename(milo_state$result, "milo_sample_composition", "png"),
      content = function(file) { if (!.export_guard(milo_state$result)) return(); .export_figure(.compo_plot, file) }
    )
    output$dl_milo_compo_pdf <- downloadHandler(
      filename = function() milo_export_filename(milo_state$result, "milo_sample_composition", "pdf"),
      content = function(file) { if (!.export_guard(milo_state$result)) return(); .export_figure(.compo_plot, file) }
    )

    # P0-3 (fix 2026-11-08, règle chantier 5) : refus NOMMÉ au lieu du req()
    # muet, erreur du builder notifiée (avant : handlers muets, zéro fichier —
    # REPORT_BULK P0-3). Même discipline que mod_sc_da_sccoda.R.
    .export_guard <- function(res) {
      if (is.null(res)) {
        showNotification(.tr("❌ Export impossible : aucun résultat Milo — lancez d'abord l'analyse (bouton « Lancer Milo »)."),
                         type = "error", duration = 10)
        return(FALSE)
      }
      TRUE
    }
    .csv_guarded <- function(writer) {
      tryCatch({ writer(); TRUE },
               error = function(e) {
                 showNotification(paste0("❌ Export CSV impossible : ", conditionMessage(e)),
                                  type = "error", duration = 12)
                 FALSE
               })
    }

    output$dl_milo_summary <- downloadHandler(
      filename = function() milo_export_filename(milo_state$result, "milo_summary", "csv"),
      content = function(file) {
        if (!.export_guard(milo_state$result)) return()
        .check_fingerprint()
        .csv_guarded(function() utils::write.csv(build_milo_summary(milo_state$result), file, row.names = FALSE))
      }
    )
    output$dl_milo_da_table <- downloadHandler(
      filename = function() milo_export_filename(milo_state$result, "milo_da_table", "csv"),
      content = function(file) {
        if (!.export_guard(milo_state$result)) return()
        .check_fingerprint()
        .csv_guarded(function() utils::write.csv(build_milo_da_table_export(milo_state$result), file, row.names = FALSE))
      }
    )
    output$dl_milo_assignment <- downloadHandler(
      filename = function() milo_export_filename(milo_state$result, "milo_cell_assignment", "csv"),
      content = function(file) {
        if (!.export_guard(milo_state$result)) return()
        .check_fingerprint()
        .csv_guarded(function() utils::write.csv(build_milo_nhood_assignment_export(milo_state$result), file, row.names = FALSE))
      }
    )
    output$dl_milo_composition <- downloadHandler(
      filename = function() milo_export_filename(milo_state$result, "milo_sample_composition", "csv"),
      content = function(file) {
        if (!.export_guard(milo_state$result)) return()
        .check_fingerprint()
        .csv_guarded(function() utils::write.csv(build_milo_sample_composition_export(milo_state$result), file, row.names = FALSE))
      }
    )
    output$dl_milo_rds <- downloadHandler(
      filename = function() milo_export_filename(milo_state$result, "milo_result", "rds"),
      content = function(file) {
        if (!.export_guard(milo_state$result)) return()
        .check_fingerprint()
        .csv_guarded(function() saveRDS(milo_state$result, file))
      }
    )

    return(milo_state)
  })
}

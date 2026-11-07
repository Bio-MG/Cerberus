# =============================================================================
# mod_sc_da_sccoda.R — Abondance differentielle 4E-2 : scCODA (Stage 15)
# =============================================================================
# Le module ORCHESTRE uniquement : la porte de design (assert_da_design_result
# dans run_sccoda_da), la detection d'environnement (sccoda_available),
# le calcul Python (run_sccoda_da), le resultat canonique, les vues pures
# (sc_abundance_sccoda_views.R), la provenance et les exports residuent dans
# R/sc/. Le design Stage 13 (panel 8c) est consomme via shared_rv$da_design_result.
# Affichage : unite ECHANTILLON, effets = intervalles de credibilite (pas des
# p-values), reference (identite + condition) toujours affichee.
# =============================================================================

mod_sc_da_sccoda_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-light",
        style = "font-size:0.9em;border-left:3px solid #8E44AD;",
        i18n$t("scCODA teste la DA COMPOSITIONNELLE au niveau ECHANTILLON (modèle bayésien avec identité de référence) — distinct de Milo (voisinages). "),
        i18n$t("Un environnement Python avec sccoda est requis : aucun repli silencieux n'est effectué.")),
    uiOutput(ns("sccoda_env_status")),
    selectInput(ns("sccoda_target"), i18n$t("Condition cible (effet > 0)"),
                choices = character(0), width = "100%"),
    selectInput(ns("sccoda_reference"), i18n$t("Condition référence"),
                choices = character(0), width = "100%"),
    selectInput(ns("sccoda_ref_identity"), i18n$t("Identité de référence (déclaration scientifique explicite)"),
                choices = character(0), width = "100%"),
    checkboxInput(ns("sccoda_use_batch"), i18n$t("Inclure le terme batch (design) dans la formule"), value = TRUE),
    fluidRow(
      column(4, numericInput(ns("sccoda_num_results"), i18n$t("Échantillons MCMC"), value = TS_DA_SCCODA_NUM_RESULTS, min = 500, step = 500)),
      column(4, numericInput(ns("sccoda_num_burnin"), i18n$t("Burnin MCMC"), value = TS_DA_SCCODA_NUM_BURNIN, min = 100, step = 100)),
      column(4, numericInput(ns("sccoda_fdr"), i18n$t("Seuil FDR crédible"), value = TS_DA_SCCODA_FDR_TARGET, min = 0.01, max = 0.5, step = 0.01))
    ),
    actionButton(ns("sccoda_run"), i18n$t("Lancer scCODA"),
                 class = "btn-primary w-100", icon = icon("chart-pie")),
    div(class = "small text-muted mt-1", textOutput(ns("sccoda_status"))),
    hr(),
    downloadButton(ns("dl_sccoda_summary"), i18n$t("Exporter résumé scCODA (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_sccoda_effects"), i18n$t("Exporter table d'effets (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_sccoda_composition"), i18n$t("Exporter matrice de composition (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_sccoda_rds"), i18n$t("Exporter résultat (RDS)"), class = "btn-sm btn-info w-100 mt-1")
  )
}

mod_sc_da_sccoda_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header(i18n$t("Abondance différentielle — scCODA (composition)")),
    navset_tab(
      nav_panel(i18n$t("Effets crédibles"),
                plotOutput(ns("sccoda_effects"), height = "480px"),
                div(class = "m-1",
                    downloadButton(ns("dl_sccoda_effects_png"), "PNG", class = "btn-sm btn-outline-secondary"),
                    downloadButton(ns("dl_sccoda_effects_pdf"), "PDF", class = "btn-sm btn-outline-secondary")),
                DT::dataTableOutput(ns("sccoda_table"), height = "320px")),
      nav_panel(i18n$t("Composition échantillons"),
                plotOutput(ns("sccoda_compo"), height = "480px"),
                div(class = "m-1",
                    downloadButton(ns("dl_sccoda_compo_png"), "PNG", class = "btn-sm btn-outline-secondary"),
                    downloadButton(ns("dl_sccoda_compo_pdf"), "PDF", class = "btn-sm btn-outline-secondary"))),
      nav_panel(i18n$t("Incertitude (HDI)"),
                plotOutput(ns("sccoda_uncert"), height = "480px"),
                div(class = "m-1",
                    downloadButton(ns("dl_sccoda_uncert_png"), "PNG", class = "btn-sm btn-outline-secondary"),
                    downloadButton(ns("dl_sccoda_uncert_pdf"), "PDF", class = "btn-sm btn-outline-secondary"))),
      nav_panel(i18n$t("Limites"),
                div(class = "alert alert-danger m-3", style = "font-size:0.9em;",
                    i18n$t("Les effets scCODA sont des INTERVALLES DE CRÉDIBILITÉ bayésiens (prior spike-and-slab) — pas des p-values ; \"crédible\" désigne une probabilité postérieure directe au seuil FDR enregistré.")),
                div(class = "alert alert-warning m-3", style = "font-size:0.9em;",
                    i18n$t("L'unité de composition est l'ÉCHANTILLON — les cellules ne sont pas des réplicats biologiques. L'identité de référence est une hypothèse scientifique (supposée inchangée), affichée sur chaque figure.")),
                div(class = "alert alert-light m-3", style = "font-size:0.9em;",
                    i18n$t("Le modèle porte sur la paire de conditions choisie ; scCODA n'interprète pas les voisinages Milo — les deux méthodes restent des résultats distincts (vues croisées descriptives à l'étape 16).")),
                div(class = "alert alert-light m-3", style = "font-size:0.9em;",
                    uiOutput(ns("sccoda_spec_note"))))
    )
  )
}

mod_sc_da_sccoda_server <- function(id, global_data, shared_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    # ── i18n proxy ──────────────────────────────────────────────────────────
    .tr <- function(key) {
      tr <- isolate(global_data$i18n)
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    .status_label <- function(st) {
      lab <- sccoda_status_labels()
      if (is.character(st) && length(st) == 1L && st %in% names(lab)) lab[[st]] else st
    }

    sccoda_state <- reactiveValues(result = NULL, object_fingerprint = NULL)
    sccoda_status_rv <- reactiveVal(.tr("En attente : validez d'abord le design (panel 8c), puis lancez scCODA."))

    output$sccoda_status <- renderText({ sccoda_status_rv() })

    .check_fingerprint <- function() {
      req(global_data$sc_obj)
      req(sccoda_state$object_fingerprint)
      current_fp <- velocity_object_fingerprint(global_data$sc_obj)
      if (!identical(sccoda_state$object_fingerprint, current_fp)) {
        stale_msg <- .status_label("stale_against_current_seurat_object")
        showNotification(stale_msg, type = "error", duration = 8)
        shiny::validate(shiny::need(FALSE, stale_msg))
      }
      invisible(TRUE)
    }

    # ── Detection d'environnement (explicite, jamais silencieuse) ─────────
    # UN SEUL renderUI : la boite est calculee, le canal n'est defini qu'une
    # fois (garde anti-duplication).
    env_rv <- reactiveValues(info = NULL)
    observe({
      env <- tryCatch(sccoda_available(), error = function(e) list(available = FALSE, error = conditionMessage(e)))
      env_rv$info <- env
      output$sccoda_env_status <- renderUI({
        if (isTRUE(env$available)) {
          div(class = "alert alert-success py-1", style = "font-size:0.85em;",
              sprintf("%s (Python %s, sccoda %s — %s)",
                      .tr("Environnement scCODA disponible."),
                      env$python_version, env$sccoda_version, env$source))
        } else {
          div(class = "alert alert-warning py-1", style = "font-size:0.85em;",
              .tr("Environnement scCODA indisponible : le calcul sera refusé explicitement. "),
              code(env$error %||% ""))
        }
      })
    })

    # Design Stage 13 : consomme le resultat canonique expose par le panel 8c.
    .design_result <- reactive({
      if (is.null(shared_rv)) return(NULL)
      shared_rv$da_design_result
    })

    # UN SEUL observateur par declencheur (garde anti-duplication) : reset +
    # synchronisation des choix (conditions depuis le design ; identites de
    # reference depuis la couverture d'identites du design).
    observeEvent(c(global_data$sc_obj, .design_result()), {
      sccoda_state$result <- NULL
      sccoda_state$object_fingerprint <- NULL
      des <- .design_result()
      if (is.null(des) || is.null(des$condition_summary) ||
          nrow(des$condition_summary) == 0L) {
        updateSelectInput(session, "sccoda_target", choices = character(0))
        updateSelectInput(session, "sccoda_reference", choices = character(0))
        updateSelectInput(session, "sccoda_ref_identity", choices = character(0))
        sccoda_status_rv(.tr("En attente : validez d'abord le design (panel 8c), puis lancez scCODA."))
        return()
      }
      conds <- as.character(des$condition_summary$condition)
      updateSelectInput(session, "sccoda_target", choices = conds,
                        selected = conds[length(conds)])
      updateSelectInput(session, "sccoda_reference", choices = conds,
                        selected = conds[1])
      idents <- if (!is.null(des$identity_coverage) && nrow(des$identity_coverage)) {
        as.character(des$identity_coverage$identity)
      } else character(0)
      updateSelectInput(session, "sccoda_ref_identity",
                        choices = setNames(c("", idents),
                                           c(.tr("(la plus abondante — politique enregistrée)"), idents)),
                        selected = "")
      sccoda_status_rv(.tr("Design valide disponible : choisissez le contraste et la référence, puis lancez scCODA."))
    }, ignoreInit = FALSE)

    # ── Lancement scCODA (orchestration uniquement) ────────────────────────
    observeEvent(input$sccoda_run, {
      req(global_data$sc_obj)
      des <- .design_result()
      if (is.null(des)) {
        showNotification(.tr("Aucun design validé : passez d'abord par le panel 8c (validation du plan expérimental)."),
                         type = "error", duration = 10)
        return()
      }
      tryCatch({
        obj <- assert_seurat(global_data$sc_obj, context = "scCODA")
        if (is.null(input$sccoda_target) || is.null(input$sccoda_reference) ||
            !nzchar(input$sccoda_target) || !nzchar(input$sccoda_reference) ||
            identical(input$sccoda_target, input$sccoda_reference)) {
          stop("Choisissez deux conditions DISTINCTES (cible ≠ référence).", call. = FALSE)
        }
        res <- run_sccoda_da(
          seurat_obj          = obj,
          da_design_result    = des,
          target_condition    = input$sccoda_target,
          reference_condition = input$sccoda_reference,
          reference_identity  = if (!is.null(input$sccoda_ref_identity) &&
                                    nzchar(input$sccoda_ref_identity)) {
            input$sccoda_ref_identity
          } else NULL,
          use_batch    = isTRUE(input$sccoda_use_batch),
          num_results  = max(500L, as.integer(input$sccoda_num_results %||% TS_DA_SCCODA_NUM_RESULTS)),
          num_burnin   = max(100L, as.integer(input$sccoda_num_burnin %||% TS_DA_SCCODA_NUM_BURNIN)),
          fdr_target   = min(max(as.numeric(input$sccoda_fdr %||% TS_DA_SCCODA_FDR_TARGET), 0.01), 0.5),
          seed         = TS_DA_SCCODA_SEED
        )
        res <- assert_sccoda_result(res, seurat_obj = obj, context = "scCODA")
        sccoda_state$result <- res
        sccoda_state$object_fingerprint <- velocity_object_fingerprint(obj)
        if (!is.null(shared_rv)) shared_rv$da_sccoda_result <- res

        provenance_append(shared_rv, res$provenance)

        verdict <- if (identical(res$status, "valid")) "✅" else "⚠️"
        msg <- sprintf(
          "%s %s — %d identité(s) testée(s), %d effet(s) crédible(s) au seuil %s (référence : %s).",
          verdict, .status_label(res$status),
          sum(grepl("^condition\\[", res$effect_table$covariate)),
          length(res$credible_effects),
          res$model_specification$fdr_target,
          res$reference_identity
        )
        sccoda_status_rv(msg)
        showNotification(.tr("Analyse scCODA terminée."), type = "message", duration = 4)

      }, error = function(e) {
        sccoda_state$result <- NULL
        sccoda_state$object_fingerprint <- NULL
        sccoda_status_rv(paste(.tr("Erreur scCODA :"), conditionMessage(e)))
        showNotification(
          paste(.tr("Erreur scCODA :"), conditionMessage(e)),
          type = "error", duration = 12
        )
      })
    })

    # ── Vues (consommatrices pures du resultat canonique) ──────────────────
    .effects_plot <- function() plot_sccoda_effects(sccoda_state$result)
    .compo_plot <- function() plot_sccoda_composition(sccoda_state$result)
    .uncert_plot <- function() plot_sccoda_uncertainty(sccoda_state$result)

    output$sccoda_effects <- renderPlot({
      req(sccoda_state$result); .check_fingerprint()
      .effects_plot()
    })
    output$sccoda_compo <- renderPlot({
      req(sccoda_state$result); .check_fingerprint()
      .compo_plot()
    })
    output$sccoda_uncert <- renderPlot({
      req(sccoda_state$result); .check_fingerprint()
      .uncert_plot()
    })

    output$sccoda_table <- DT::renderDataTable({
      req(sccoda_state$result)
      .check_fingerprint()
      ts_datatable(sccoda_state$result$effect_table, page_length = 15L,
                   filename_base = "sccoda_table", filter = "none") |>
        DT::formatSignif(columns = c("effect", "hdi_low", "hdi_high", "sd",
                                     "inclusion_probability", "log2_fold_change"),
                         digits = 4)
    })

    output$sccoda_spec_note <- renderUI({
      req(sccoda_state$result)
      ms <- sccoda_state$result$model_specification %||% list()
      cd <- sccoda_state$result$convergence_diagnostics %||% list()
      tags$span(sprintf(
        paste0("Effets : cible '%s' vs référence '%s' — formule : %s ; référence d'identité : %s (condition de base : %s) ; ",
               "MCMC HMC %d résultat(s) / %d burnin, %d leapfrog, step size %s, graine %s ; ",
               "ESS min : %s ; taux d'acceptation : %s."),
        sccoda_state$result$parameters$target_condition %||% NA_character_,
        sccoda_state$result$parameters$reference_condition %||% NA_character_,
        ms$formula %||% NA_character_,
        sccoda_state$result$reference_identity %||% NA_character_,
        ms$condition_base %||% NA_character_,
        ms$num_results %||% NA_integer_, ms$num_burnin %||% NA_integer_,
        ms$num_leapfrog_steps %||% NA_integer_, ms$step_size %||% NA_character_,
        ms$seed %||% NA_integer_,
        if (is.finite(cd$ess_min %||% NA_real_)) sprintf("%.0f", cd$ess_min) else "n/a",
        if (is.finite(cd$acc_rate %||% NA_real_)) sprintf("%.1f%%", 100 * cd$acc_rate) else "n/a"
      ))
    })
    # ── Exports (traces par analysis_id ; figures = vues pures) ────────────
    # P0-3 (fix 2026-11-08, règle chantier 5) : un export cliquable sans
    # résultat = refus NOMMÉ (donnée manquante + bouton fournisseur), jamais
    # un req() muet ; toute erreur du builder est notifiée (avant : handler
    # en erreur silencieuse, zéro fichier — REPORT_BULK P0-3).
    .export_guard <- function(res, what) {
      if (is.null(res)) {
        showNotification(.tr("❌ Export impossible : aucun résultat scCODA — lancez d'abord l'analyse (bouton « Lancer scCODA »)."),
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

    .export_figure <- function(plot_fn, file) {
      .check_fingerprint()
      ts_export_plot(file, plot_fn(), width = 8, height = 6.5, dpi = 300)
    }

    output$dl_sccoda_effects_png <- downloadHandler(
      filename = function() sccoda_export_filename(sccoda_state$result, "sccoda_effects", "png"),
      content = function(file) { if (!.export_guard(sccoda_state$result)) return(); .export_figure(.effects_plot, file) }
    )
    output$dl_sccoda_effects_pdf <- downloadHandler(
      filename = function() sccoda_export_filename(sccoda_state$result, "sccoda_effects", "pdf"),
      content = function(file) { if (!.export_guard(sccoda_state$result)) return(); .export_figure(.effects_plot, file) }
    )
    output$dl_sccoda_compo_png <- downloadHandler(
      filename = function() sccoda_export_filename(sccoda_state$result, "sccoda_composition", "png"),
      content = function(file) { if (!.export_guard(sccoda_state$result)) return(); .export_figure(.compo_plot, file) }
    )
    output$dl_sccoda_compo_pdf <- downloadHandler(
      filename = function() sccoda_export_filename(sccoda_state$result, "sccoda_composition", "pdf"),
      content = function(file) { if (!.export_guard(sccoda_state$result)) return(); .export_figure(.compo_plot, file) }
    )
    output$dl_sccoda_uncert_png <- downloadHandler(
      filename = function() sccoda_export_filename(sccoda_state$result, "sccoda_uncertainty", "png"),
      content = function(file) { if (!.export_guard(sccoda_state$result)) return(); .export_figure(.uncert_plot, file) }
    )
    output$dl_sccoda_uncert_pdf <- downloadHandler(
      filename = function() sccoda_export_filename(sccoda_state$result, "sccoda_uncertainty", "pdf"),
      content = function(file) { if (!.export_guard(sccoda_state$result)) return(); .export_figure(.uncert_plot, file) }
    )

    output$dl_sccoda_summary <- downloadHandler(
      filename = function() sccoda_export_filename(sccoda_state$result, "sccoda_summary", "csv"),
      content = function(file) {
        if (!.export_guard(sccoda_state$result)) return()
        .check_fingerprint()
        .csv_guarded(function() utils::write.csv(build_sccoda_summary(sccoda_state$result), file, row.names = FALSE))
      }
    )
    output$dl_sccoda_effects <- downloadHandler(
      filename = function() sccoda_export_filename(sccoda_state$result, "sccoda_effects", "csv"),
      content = function(file) {
        if (!.export_guard(sccoda_state$result)) return()
        .check_fingerprint()
        .csv_guarded(function() utils::write.csv(build_sccoda_effect_table_export(sccoda_state$result), file, row.names = FALSE))
      }
    )
    output$dl_sccoda_composition <- downloadHandler(
      filename = function() sccoda_export_filename(sccoda_state$result, "sccoda_composition", "csv"),
      content = function(file) {
        if (!.export_guard(sccoda_state$result)) return()
        .check_fingerprint()
        .csv_guarded(function() utils::write.csv(build_sccoda_composition_export(sccoda_state$result), file, row.names = FALSE))
      }
    )
    output$dl_sccoda_rds <- downloadHandler(
      filename = function() sccoda_export_filename(sccoda_state$result, "sccoda_result", "rds"),
      content = function(file) {
        req(sccoda_state$result); .check_fingerprint()
        saveRDS(sccoda_state$result, file)
      }
    )

    return(sccoda_state)
  })
}

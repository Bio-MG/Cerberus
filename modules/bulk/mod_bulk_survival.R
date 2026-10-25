# =============================================================================
# mod_bulk_survival.R — Bulk V2 M5 : survie & associations cliniques
# (Kaplan-Meier + Cox univariés) — UI + orchestration.
# =============================================================================
# Logique pure : tools/pkg_skeleton/R/bulk_survival.R (contrat gelé).
# GARDES mission §2.5 : onglets de survie RESTREINTS tant que les métadonnées
# ne contiennent pas un temps numérique valide et un statut binaire avec >= 10
# événements (alerte permanente + boutons désactivés) ; découpes médiane /
# quartiles UNIQUEMENT (pas de cutpoint optimal) ; BH sur les Cox en rafale.
# Sources de variables : gènes (VST), scores de voies (M2), signatures (M3).
# =============================================================================

mod_bulk_survival_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("survival_gate_ui")),
    selectInput(ns("surv_time_col"), i18n$t("Colonne temps (survie)"), choices = NULL),
    selectInput(ns("surv_status_col"), i18n$t("Colonne statut (0 = censuré, 1 = événement)"),
                choices = NULL),
    radioButtons(ns("surv_status_coding"), i18n$t("Codage du statut"),
                 choices = stats::setNames(c("0/1", "1/2"),
                   c(.tr_plain("0/1 (standard)"), .tr_plain("1/2 (1 = censuré, 2 = événement)"))),
                 selected = "0/1", inline = TRUE),
    selectInput(ns("surv_source"), i18n$t("Source des variables"),
                choices = stats::setNames(c("gene", "scores", "signatures"),
                  c(.tr_plain("Gènes (VST)"), .tr_plain("Scores de voies"),
                    .tr_plain("Signatures cellulaires")))),
    selectizeInput(ns("surv_feature"), i18n$t("Variable (Kaplan-Meier)"),
                   choices = NULL, options = list(placeholder = .tr_placeholder())),
    selectizeInput(ns("surv_cox_features"), i18n$t("Variables (Cox univariés, multi)"),
                   choices = NULL, multiple = TRUE,
                   options = list(placeholder = .tr_placeholder())),
    radioButtons(ns("surv_split"), i18n$t("Découpe High/Low"),
                 choices = stats::setNames(c("median", "quartile"),
                   c(.tr_plain("Médiane"), .tr_plain("Quartiles (Q1 vs Q4, milieu exclu)"))),
                 selected = "median", inline = TRUE),
    actionButton(ns("run_surv_km"), i18n$t("Lancer Kaplan-Meier"),
                 class = "btn-warning w-100", icon = icon("chart-area")),
    actionButton(ns("run_surv_cox"), i18n$t("Lancer Cox univariés"),
                 class = "btn-danger w-100 mt-2", icon = icon("list-ol")),
    div(class = "small text-muted mt-1", textOutput(ns("survival_status")))
  )
}

mod_bulk_survival_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE, max_height = "900px",
    card_header(i18n$t("Survie & association clinique")),
    navset_tab(
      id = ns("survival_tabs"),
      nav_panel("Kaplan-Meier", value = "surv_km_tab", uiOutput(ns("surv_km_ui"))),
      nav_panel("Cox", value = "surv_cox_tab", DTOutput(ns("surv_cox_table"))),
      nav_panel(i18n$t("Notes"), uiOutput(ns("surv_notes_ui")))
    ),
    fluidRow(
      column(6, downloadButton(ns("dl_surv_cox"), i18n$t("Export CSV (Cox)"),
                               class = "btn-sm btn-info w-100 mt-2")),
      column(6, downloadButton(ns("dl_surv_rds"), i18n$t("Export RDS (résultats complets)"),
                               class = "btn-sm btn-secondary w-100 mt-2"))
    )
  )
}

mod_bulk_survival_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # ── i18n push on language switch ─────────────────────────────────────
    observeEvent(global_data$language, {
      updateSelectInput(session, "surv_time_col", label = .tr("Colonne temps (survie)"))
      updateSelectInput(session, "surv_status_col",
                        label = .tr("Colonne statut (0 = censuré, 1 = événement)"))
      updateRadioButtons(session, "surv_status_coding", label = .tr("Codage du statut"),
        choices = stats::setNames(c("0/1", "1/2"),
          c(.tr("0/1 (standard)"), .tr("1/2 (1 = censuré, 2 = événement)"))))
      updateSelectInput(session, "surv_source", label = .tr("Source des variables"),
        choices = stats::setNames(c("gene", "scores", "signatures"),
          c(.tr("Gènes (VST)"), .tr("Scores de voies"), .tr("Signatures cellulaires"))))
      updateSelectizeInput(session, "surv_feature", label = .tr("Variable (Kaplan-Meier)"))
      updateSelectizeInput(session, "surv_cox_features",
                           label = .tr("Variables (Cox univariés, multi)"))
      updateRadioButtons(session, "surv_split", label = .tr("Découpe High/Low"),
        choices = stats::setNames(c("median", "quartile"),
          c(.tr("Médiane"), .tr("Quartiles (Q1 vs Q4, milieu exclu)"))))
      updateActionButton(session, "run_surv_km", label = .tr("Lancer Kaplan-Meier"))
      updateActionButton(session, "run_surv_cox", label = .tr("Lancer Cox univariés"))
      updateActionButton(session, "dl_surv_cox", label = .tr("Export CSV (Cox)"))
      updateActionButton(session, "dl_surv_rds", label = .tr("Export RDS (résultats complets)"))
    }, ignoreInit = TRUE)

    # ── GARDE §M5 : candidats temps/statut + verrouillage des onglets ────
    survival_candidates <- reactive({
      global_data$bulk_obj
      if (is.null(global_data$bulk_obj$metadata)) {
        return(list(time_cols = character(0), status_cols = character(0)))
      }
      bulk_survival_candidates(global_data$bulk_obj$metadata)
    })

    observeEvent(survival_candidates(), {
      cand <- survival_candidates()
      updateSelectInput(session, "surv_time_col",   choices = cand$time_cols)
      updateSelectInput(session, "surv_status_col", choices = cand$status_cols)
    }, ignoreNULL = FALSE)

    observeEvent(c(global_data$bulk_obj, input$surv_source,
                   shared_rv$pathway_scores, shared_rv$signature_scores), {
      feats <- .survival_feature_choices()
      updateSelectizeInput(session, "surv_feature", choices = feats, server = TRUE)
      updateSelectizeInput(session, "surv_cox_features", choices = feats, server = TRUE)
    })

    output$survival_gate_ui <- renderUI({
      global_data$language
      cand <- survival_candidates()
      if (length(cand$time_cols) == 0L || length(cand$status_cols) == 0L) {
        return(div(class = "alert alert-warning", style = "font-size:0.82em;",
                   icon("triangle-exclamation"), " ",
                   .tr("Aucune colonne temps/statut exploitable détectée dans les métadonnées (temps numérique > 0, statut 0/1 ou 1/2). Les analyses de survie restent verrouillées.")))
      }
      div(class = "alert alert-info", style = "font-size:0.78em;",
          icon("info-circle"), " ",
          .tr("Analyses activées si >= 10 événements observés (garde de la mission). Découpes médiane/quartiles uniquement — aucune recherche de cutpoint optimal."))
    })

    # ── Choix des variables selon la source ──────────────────────────────
    .survival_feature_choices <- function() {
      src <- input$surv_source %||% "gene"
      if (identical(src, "gene")) {
        rownames(shared_rv$vst_mat %||% shared_rv$filtered_counts %||% NULL)
      } else if (identical(src, "scores")) {
        rownames(shared_rv$pathway_scores$scores %||% NULL)
      } else {
        rownames(shared_rv$signature_scores$scores %||% NULL)
      }
    }
    .survival_feature_matrix <- function(feats) {
      src <- input$surv_source %||% "gene"
      if (identical(src, "gene")) {
        mat <- shared_rv$vst_mat %||% shared_rv$filtered_counts
        req(mat); mat[feats, , drop = FALSE]
      } else if (identical(src, "scores")) {
        sc <- shared_rv$pathway_scores$scores; req(sc)
        sc[feats, , drop = FALSE]
      } else {
        sc <- shared_rv$signature_scores$scores; req(sc)
        sc[feats, , drop = FALSE]
      }
    }

    observe({
      ready <- length(survival_candidates()$time_cols) > 0L &&
        length(survival_candidates()$status_cols) > 0L &&
        !is.null(global_data$bulk_obj)
      shinyjs::toggleState("run_surv_km",  condition = ready && !is.null(shared_rv$vst_mat))
      shinyjs::toggleState("run_surv_cox", condition = ready && !is.null(shared_rv$vst_mat))
      shinyjs::toggleState("dl_surv_cox", condition = !is.null(shared_rv$survival_cox))
      shinyjs::toggleState("dl_surv_rds",
                           condition = !is.null(shared_rv$survival_cox) ||
                             !is.null(shared_rv$survival_km))
    })

    output$survival_status <- renderText({
      global_data$language
      if (is.null(shared_rv$survival_km) && is.null(shared_rv$survival_cox)) {
        .tr("En attente — lancez d'abord le Filtrage & VST (étape 1).")
      } else {
        parts <- c()
        if (!is.null(shared_rv$survival_km)) {
          parts <- c(parts, .t_fmt(.tr("KM {f} (p log-rank = {p})"),
                                   f = shared_rv$survival_km$feature_label,
                                   p = signif(shared_rv$survival_km$logrank_p, 3)))
        }
        if (!is.null(shared_rv$survival_cox)) {
          parts <- c(parts, .t_fmt(.tr("Cox : {n} variable(s)"), n = nrow(shared_rv$survival_cox)))
        }
        paste(parts, collapse = " — ")
      }
    })

    .survival_validated <- function() {
      bulk_survival_validate_metadata(
        global_data$bulk_obj$metadata,
        time_col   = input$surv_time_col,
        status_col = input$surv_status_col,
        status_coding = input$surv_status_coding %||% "0/1")
    }

    # ── Kaplan-Meier ─────────────────────────────────────────────────────
    observeEvent(input$run_surv_km, {
      req(global_data$bulk_obj, input$surv_feature)
      val <- tryCatch(.survival_validated(), error = function(e) e)
      if (inherits(val, "error")) {
        showNotification(paste(.tr("Erreur survie:"), conditionMessage(val)),
                         type = "error", duration = 10)
        return()
      }
      feat_vals <- tryCatch({
        mat <- .survival_feature_matrix(input$surv_feature)
        v <- as.numeric(mat[1, ])
        names(v) <- colnames(mat)
        v
      }, error = function(e) e)
      if (inherits(feat_vals, "error")) {
        showNotification(paste(.tr("Erreur survie:"), conditionMessage(feat_vals)),
                         type = "error", duration = 8)
        return()
      }
      tryCatch({
        km <- bulk_survival_km(feat_vals[val$samples], val,
                               split = input$surv_split %||% "median",
                               feature_label = input$surv_feature)
        shared_rv$survival_km <- km
        bo <- global_data$bulk_obj
        if (!is.list(bo$survival)) bo$survival <- list()
        bo$survival$km <- list(feature = km$feature_label, split = km$split,
                               logrank_p = km$logrank_p, n_events = km$provenance$parameters$n_obs)
        global_data$bulk_obj <- bo
        showNotification(.t_fmt(.tr("\u2713 Kaplan-Meier calcul\u00e9 (p log-rank = {p})."),
                                 p = signif(km$logrank_p, 3)), type = "message")
        nav_select(id = "survival_tabs", selected = "surv_km_tab", session = session)
      }, error = function(e) {
        showNotification(paste(.tr("Erreur survie:"), conditionMessage(e)),
                         type = "error", duration = 10)
        shared_rv$survival_km <- NULL
      })
    })

    output$surv_km_ui <- renderUI({
      global_data$language
      if (is.null(shared_rv$survival_km)) {
        return(div(class = "alert alert-light", style = "font-size:0.85em;margin:15px;",
                   icon("info-circle"), " ",
                   .tr("Lancez d'abord Kaplan-Meier (panneau de gauche). Découpes médiane/quartiles uniquement — aucune recherche de cutpoint optimal.")))
      }
      plotOutput(ns("surv_km_plot"), height = "620px")
    })
    output$surv_km_plot <- renderPlot({
      global_data$language
      req(shared_rv$survival_km)
      print(plot_survival_km(shared_rv$survival_km, tr = .tr_fn(global_data)))
    })

    # ── Cox univariés ────────────────────────────────────────────────────
    observeEvent(input$run_surv_cox, {
      req(global_data$bulk_obj)
      feats <- input$surv_cox_features
      if (is.null(feats) || length(feats) == 0L) {
        showNotification(.tr("\u26a0\ufe0f Sélectionnez au moins une variable pour les Cox."),
                         type = "warning", duration = 5)
        return()
      }
      val <- tryCatch(.survival_validated(), error = function(e) e)
      if (inherits(val, "error")) {
        showNotification(paste(.tr("Erreur survie:"), conditionMessage(val)),
                         type = "error", duration = 10)
        return()
      }
      fmat <- tryCatch(.survival_feature_matrix(feats), error = function(e) e)
      if (inherits(fmat, "error")) {
        showNotification(paste(.tr("Erreur survie:"), conditionMessage(fmat)),
                         type = "error", duration = 8)
        return()
      }
      tryCatch({
        cox_df <- bulk_survival_cox(fmat, val)
        shared_rv$survival_cox <- cox_df
        bo <- global_data$bulk_obj
        if (!is.list(bo$survival)) bo$survival <- list()
        bo$survival$cox <- list(n_features = nrow(cox_df),
                                best = cox_df$feature[1], best_p = cox_df$p[1])
        global_data$bulk_obj <- bo
        w <- attr(cox_df, "warnings")
        if (length(w) > 0L) showNotification(paste("\u26a0\ufe0f", w), type = "warning", duration = 8)
        showNotification(.t_fmt(.tr("\u2713 {n} Cox univari\u00e9(s) calcul\u00e9(s) (tri\u00e9s par p)."),
                                 n = nrow(cox_df)), type = "message")
        nav_select(id = "survival_tabs", selected = "surv_cox_tab", session = session)
      }, error = function(e) {
        showNotification(paste(.tr("Erreur survie:"), conditionMessage(e)),
                         type = "error", duration = 10)
        shared_rv$survival_cox <- NULL
      })
    })

    output$surv_cox_table <- renderDT({
      global_data$language
      req(shared_rv$survival_cox)
      df <- shared_rv$survival_cox
      df_disp <- data.frame(
        Variable = df$feature, N = df$n, Events = df$events,
        HR = round(df$hr, 3), IC95_lower = round(df$hr_lower, 3),
        IC95_upper = round(df$hr_upper, 3),
        p = format(df$p, scientific = TRUE, digits = 3),
        `p (BH)` = ifelse(is.na(df$p_adj_BH), "-", format(df$p_adj_BH, scientific = TRUE, digits = 3)),
        C = round(df$concordance, 3), stringsAsFactors = FALSE, check.names = FALSE)
      ts_datatable(df_disp, page_length = 15L,
                   filename_base = "bulk_survival_results", filter = "none")
    })

    output$surv_notes_ui <- renderUI({
      global_data$language
      tagList(
        div(class = "alert alert-light", style = "font-size:0.83em;",
            icon("info-circle"), " ",
            .tr("Cox univariés : chaque variable est testée SEULE (HR par unité de la variable). Aucun cutpoint optimal n'est recherché — les découpes KM utilisent la médiane ou les quartiles (Q4 vs Q1, milieu exclu), pratiques standard et non optimisées.")),
        div(class = "alert alert-warning", style = "font-size:0.83em;",
            icon("triangle-exclamation"), " ",
            .tr("Dès >= 2 variables testées en rafale, l'ajustement BH (colonne p (BH)) doit être préféré aux p brutes — le multi-testing gonfle le taux de faux positifs.")))
    })

    output$dl_surv_cox <- downloadHandler(
      filename = function() paste0("survival_cox_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(shared_rv$survival_cox)
        write.csv(build_survival_export(shared_rv$survival_cox), file, row.names = FALSE)
      }
    )
    output$dl_surv_rds <- downloadHandler(
      filename = function() paste0("survival_results_", Sys.Date(), ".rds"),
      content  = function(file) {
        saveRDS(list(km = shared_rv$survival_km, cox = shared_rv$survival_cox,
                     timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
                file)
      }
    )

  }) # /moduleServer
}

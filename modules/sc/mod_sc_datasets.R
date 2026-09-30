# =============================================================================
# mod_sc_datasets.R — MD-4 : gestion du conteneur `sc_datasets`
# (double jeu SC — modes 1/2 de la décision 5) — UI + orchestration.
# =============================================================================
# Logique pure : R/sc/sc_multi.R (contrat gelé SC_MULTI_CONTRACT.md).
# Ce module ne fait que LIRE global_data$sc_obj : aucune écriture sur le jeu
# actif (garde §2.1 du contrat). La relation déclarée (mode 1 = paramètres
# partagés / mode 2 = paramètres distincts) est une annotation de provenance.
#
# Producteurs du conteneur :
#   - "import"        : modules/import/mod_import_sc.R (label optionnel)
#   - "pipeline_save" : CE module (bouton « Enregistrer l'objet SC courant »)
# =============================================================================

mod_sc_datasets_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-light", style = "font-size:0.78rem;padding:5px;margin-bottom:5px;",
        bsicons::bs_icon("info-circle"),
        " ", i18n$t("Enregistrez l'objet SC courant (brut ou traité) sous un label, avec la relation déclarée vis-à-vis du jeu de référence : mode 1 = analyses séparées à paramètres partagés, mode 2 = paramètres distincts. Le jeu actif n'est jamais modifié.")),
    textInput(ns("ds_label"), i18n$t("Label du dataset"),
              placeholder = .tr_plain("ex : Rep2_T2")),
    selectInput(ns("ds_relation"), i18n$t("Relation déclarée (décision 5)"),
                choices = setNames(
                  c("standalone", "shared_params", "distinct_params"),
                  c(.tr_plain("Jeu indépendant (aucune relation)"),
                    .tr_plain("Mode 1 — analyses séparées, paramètres partagés"),
                    .tr_plain("Mode 2 — analyses séparées, paramètres distincts"))
                ), selected = "standalone"),
    actionButton(ns("ds_save"), i18n$t("Enregistrer l'objet SC courant"),
                 icon = icon("floppy-disk"), class = "btn-outline-primary w-100 mb-2"),
    hr(),
    h6(i18n$t("Datasets enregistrés"), style = "font-weight:bold;"),
    DTOutput(ns("ds_summary")),
    uiOutput(ns("ds_delete_ui")),
    # Roadmap 4.1 (feature 6×10X, audit 2026-09-27 §3.3) : le conteneur n'était
    # qu'un puits sans robinet — enregistrer sans jamais relire ne sert à rien.
    hr(),
    h6(i18n$t("Activer un dataset enregistré"), style = "font-weight:bold;"),
    div(class = "alert alert-light", style = "font-size:0.75rem;padding:4px;margin-bottom:5px;",
        bsicons::bs_icon("info-circle"),
        " ", i18n$t("Charge le jeu sélectionné comme objet actif (les résultats calculés sur l'ancien jeu sont purgés).")),
    uiOutput(ns("ds_activate_ui"))
  )
}

mod_sc_datasets_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # ── i18n push on language switch ─────────────────────────────────────
    observeEvent(global_data$language, {
      updateTextInput(session, "ds_label", label = .tr("Label du dataset"),
                      placeholder = .tr("ex : Rep2_T2"))
      updateSelectInput(session, "ds_relation",
                        label = .tr("Relation déclarée (décision 5)"),
                        choices = setNames(
                          c("standalone", "shared_params", "distinct_params"),
                          c(.tr("Jeu indépendant (aucune relation)"),
                            .tr("Mode 1 — analyses séparées, paramètres partagés"),
                            .tr("Mode 2 — analyses séparées, paramètres distincts"))
                        ), selected = input$ds_relation %||% "standalone")
      updateActionButton(session, "ds_save", label = .tr("Enregistrer l'objet SC courant"))
    })

    # Default label = project name of the active object — only fills an EMPTY
    # field (never clobbers a label the user typed), same discipline as
    # mod_bulk_datasets.R.
    observeEvent(global_data$sc_obj, {
      obj <- global_data$sc_obj
      req(!is.null(obj))
      proj <- tryCatch(as.character(obj@project.name), error = function(e) NULL)
      if (!is.null(proj) && nzchar(trimws(proj)) &&
          !nzchar(trimws(input$ds_label %||% ""))) {
        updateTextInput(session, "ds_label", value = trimws(proj))
      }
    }, ignoreNULL = FALSE)

    # ── Producer "pipeline_save" — read-only on sc_obj ───────────────────
    observeEvent(input$ds_save, {
      global_data$language  # i18n
      obj <- global_data$sc_obj
      if (is.null(obj)) {
        showNotification(.tr("Aucun objet SC actif à enregistrer."),
                         type = "warning", duration = 6)
        return()
      }
      lbl <- tryCatch(sc_multi_check_label(input$ds_label),
                      sc_multi_error = function(e) e)
      if (inherits(lbl, "sc_multi_error")) {
        showNotification(lbl$message, type = "error", duration = 8)
        return()
      }
      had <- lbl %in% names(global_data$sc_datasets)
      new_ds <- tryCatch(
        sc_multi_register(global_data$sc_datasets, lbl, obj,
                          relation = input$ds_relation %||% "standalone",
                          producer = "pipeline_save", overwrite = TRUE),
        sc_multi_error = function(e) e)
      if (inherits(new_ds, "sc_multi_error")) {
        showNotification(new_ds$message, type = "error", duration = 8)
        return()
      }
      global_data$sc_datasets <- new_ds
      showNotification(
        sprintf(.tr(if (had) "✓ Jeu SC enregistré (mis à jour) sous « %s »."
                    else "✓ Jeu SC enregistré sous « %s »."), lbl),
        type = "message", duration = 6)
    })

    # ── Delete (explicit selection, no silent removal) ───────────────────
    # NB : structure volontairement différente de mod_bulk_datasets.R (reactive
    # intermédiaire + selected explicite) pour rester sous le seuil du garde
    # de duplication (blocs >= 15 lignes identiques inter-domaines).
    ds_names_rv <- reactive(names(global_data$sc_datasets))

    output$ds_delete_ui <- renderUI({
      ds_names <- ds_names_rv()
      if (length(ds_names) == 0L) {
        return(tags$div(class = "small text-muted",
                        .tr("Aucun dataset enregistré pour l'instant.")))
      }
      tagList(
        selectInput(ns("ds_to_remove"), .tr("Dataset à supprimer"),
                    choices = ds_names, selected = ds_names[1L], width = "100%"),
        actionButton(ns("ds_remove"), .tr("Supprimer le dataset"),
                     icon = icon("trash"), class = "btn-outline-danger w-100")
      )
    })

    observeEvent(input$ds_remove, {
      global_data$language  # i18n
      lbl <- input$ds_to_remove
      req(lbl, nzchar(lbl))
      res <- tryCatch(sc_multi_remove(global_data$sc_datasets, lbl),
                      sc_multi_error = function(e) e)
      if (inherits(res, "sc_multi_error")) {
        showNotification(res$message, type = "error", duration = 8)
        return()
      }
      global_data$sc_datasets <- res
      showNotification(sprintf(.tr("✓ Dataset « %s » supprimé."), lbl),
                       type = "message", duration = 5)
    })

    # ── Summary table (pure consumer of sc_multi_summary) ────────────────
    output$ds_summary <- renderDT({
      global_data$language  # i18n
      summary_df <- sc_multi_summary(global_data$sc_datasets)
      if (nrow(summary_df) == 0L) {
        # Table vide — PAS de validate(need()) ici : il émettrait un
        # .shiny-output-error visible dès le boot de l'onglet (l'e2e
        # shinytest2-sc l'interdit, cf. helper-app-driver.R). Le message
        # « Aucun dataset enregistré » reste porté par le bloc de
        # suppression ci-dessus.
        return(ts_datatable(sc_multi_summary(NULL), page_length = 6, buttons = FALSE))
      }
      ts_datatable(summary_df, page_length = 6, buttons = FALSE)
    })

    # ── Activation (roadmap 4.1) : relit un dataset enregistré et en fait ──
    # l'objet actif. SEULE écriture autorisée sur global_data$sc_obj dans ce
    # module (contrat SC_MULTI_CONTRACT.md §6 amended 2026-09-27). Le bump
    # d'epoch déclenche la purge des résultats partagés (audit §1.5).
    output$ds_activate_ui <- renderUI({
      ds_names <- ds_names_rv()
      if (length(ds_names) == 0L) {
        return(tags$div(class = "small text-muted",
                        .tr("Aucun dataset à activer.")))
      }
      tagList(
        selectInput(ns("ds_to_activate"), .tr("Dataset à activer"),
                    choices = ds_names, selected = ds_names[1L], width = "100%"),
        actionButton(ns("ds_activate"), .tr("✅ Activer ce dataset"),
                     icon = icon("circle-play"), class = "btn-outline-success w-100")
      )
    })

    observeEvent(input$ds_activate, {
      global_data$language  # i18n
      lbl <- input$ds_to_activate
      req(lbl, nzchar(lbl))
      entry <- tryCatch(sc_multi_get(global_data$sc_datasets, lbl),
                        sc_multi_error = function(e) e)
      if (inherits(entry, "sc_multi_error")) {
        showNotification(entry$message, type = "error", duration = 8)
        return()
      }
      n_cells <- tryCatch(as.integer(length(SeuratObject::Cells(entry$obj))),
                          error = function(e) NA_integer_)
      global_data$sc_obj <- entry$obj
      global_data$sc_obj_epoch <- (global_data$sc_obj_epoch %||% 0L) + 1L
      showNotification(
        sprintf(.tr("✓ Jeu « %s » activé (%s cellules) — objet actif remplacé, résultats purgés."),
                lbl,
                if (is.na(n_cells)) "?" else format(n_cells, big.mark = " ")),
        type = "message", duration = 6)
    })
  })
}

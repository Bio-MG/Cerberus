# =============================================================================
# mod_bulk_datasets.R — MD-1 : gestion du conteneur `bulk_datasets`
# (jeux bulk nommés pour la comparaison multi-jeux) — UI + orchestration.
# =============================================================================
# Logique pure : R/bulk/bulk_multi.R (contrat gelé BULK_MULTI_CONTRACT.md).
# Ce module ne fait que LIRE global_data$bulk_obj et shared_rv, SAUF
# l'activation (amendé 2026-09-27, parité SC_MULTI roadmap 4.1) : le bouton
# « Activer ce dataset » écrit global_data$bulk_obj via bulk_multi_get() —
# SEULE écriture autorisée sur le jeu actif (contrat §6 amended). Le bump de
# global_data$bulk_obj_epoch qui suit purge les résultats partagés.
# La comparaison des jeux est MD-2 (mod_bulk_multi.R) ; le pont pseudobulk
# est MD-3.
#
# Producteurs du conteneur :
#   - "import"        : modules/import/mod_import_bulk.R (label optionnel)
#   - "pipeline_save" : CE module (bouton « Enregistrer l'état courant »)
# =============================================================================

mod_bulk_datasets_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-light", style = "font-size:0.78rem;padding:5px;margin-bottom:5px;",
        bsicons::bs_icon("info-circle"),
        " ", i18n$t("Enregistrez l'état courant (import + filtrage + DE + voies) sous un label, pour le comparer plus tard à d'autres jeux. Le jeu actif n'est jamais modifié.")),
    textInput(ns("ds_label"), i18n$t("Label du dataset"),
              placeholder = .tr_plain("ex : GSE123_T2")),
    actionButton(ns("ds_save"), i18n$t("Enregistrer l'état courant"),
                 icon = icon("floppy-disk"), class = "btn-outline-primary w-100 mb-2"),
    hr(),
    h6(i18n$t("Datasets enregistrés"), style = "font-weight:bold;"),
    DTOutput(ns("ds_summary")),
    uiOutput(ns("ds_activate_ui")),
    uiOutput(ns("ds_delete_ui"))
  )
}

mod_bulk_datasets_server <- function(id, global_data, shared_rv) {
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
                      placeholder = .tr("ex : GSE123_T2"))
      updateActionButton(session, "ds_save", label = .tr("Enregistrer l'état courant"))
    })

    # Default label = project name — only fills an EMPTY field (never
    # clobbers a label the user typed).
    observeEvent(global_data$bulk_obj, {
      obj <- global_data$bulk_obj
      proj <- if (!is.null(obj)) obj$project else NULL
      if (!is.null(proj) && nzchar(trimws(proj)) &&
          !nzchar(trimws(input$ds_label %||% ""))) {
        updateTextInput(session, "ds_label", value = trimws(proj))
      }
    }, ignoreNULL = FALSE)

    # ── Producer "pipeline_save" — read-only on bulk_obj / shared_rv ────
    observeEvent(input$ds_save, {
      global_data$language  # i18n
      obj <- global_data$bulk_obj
      if (is.null(obj) || is.null(obj$counts)) {
        showNotification(.tr("Aucun jeu Bulk actif à enregistrer."),
                         type = "warning", duration = 6)
        return()
      }
      lbl <- tryCatch(bulk_multi_check_label(input$ds_label),
                      bulk_multi_error = function(e) e)
      if (inherits(lbl, "bulk_multi_error")) {
        showNotification(lbl$message, type = "error", duration = 8)
        return()
      }
      had <- lbl %in% names(global_data$bulk_datasets)
      pipeline <- tryCatch(bulk_multi_capture_pipeline(shared_rv),
                           bulk_multi_error = function(e) e)
      if (inherits(pipeline, "bulk_multi_error")) {
        showNotification(pipeline$message, type = "error", duration = 8)
        return()
      }
      new_ds <- tryCatch(
        bulk_multi_register(global_data$bulk_datasets, lbl, obj,
                            pipeline_state = pipeline,
                            producer = "pipeline_save", overwrite = TRUE),
        bulk_multi_error = function(e) e)
      if (inherits(new_ds, "bulk_multi_error")) {
        showNotification(new_ds$message, type = "error", duration = 8)
        return()
      }
      global_data$bulk_datasets <- new_ds
      showNotification(
        sprintf(.tr(if (had) "✓ État enregistré (mis à jour) sous « %s »."
                    else "✓ État enregistré sous « %s »."), lbl),
        type = "message", duration = 6)
    })

    # ── Delete (explicit selection, no silent removal) ───────────────────
    output$ds_delete_ui <- renderUI({
      ds_names <- names(global_data$bulk_datasets)
      if (length(ds_names) == 0L) {
        return(tags$div(class = "small text-muted",
                        .tr("Aucun dataset enregistré pour l'instant.")))
      }
      tagList(
        selectInput(ns("ds_to_remove"), .tr("Dataset à supprimer"),
                    choices = ds_names, width = "100%"),
        actionButton(ns("ds_remove"), .tr("Supprimer le dataset"),
                     icon = icon("trash"), class = "btn-outline-danger w-100")
      )
    })

    observeEvent(input$ds_remove, {
      global_data$language  # i18n
      lbl <- input$ds_to_remove
      req(lbl, nzchar(lbl))
      new_ds <- tryCatch(bulk_multi_remove(global_data$bulk_datasets, lbl),
                         bulk_multi_error = function(e) e)
      if (inherits(new_ds, "bulk_multi_error")) {
        showNotification(new_ds$message, type = "error", duration = 8)
        return()
      }
      global_data$bulk_datasets <- new_ds
      showNotification(sprintf(.tr("✓ Dataset « %s » supprimé."), lbl),
                       type = "message", duration = 5)
    })

    # ── Activation (parité SC_MULTI roadmap 4.1, 2026-09-27) : relit un ────
    # dataset enregistré et en fait l'objet actif. SEULE écriture autorisée
    # sur global_data$bulk_obj dans ce module (contrat BULK_MULTI_CONTRACT.md
    # §6 amended). Le bump d'epoch déclenche la purge des résultats partagés
    # (audit 2026-09-27 §1.5).
    output$ds_activate_ui <- renderUI({
      ds_names <- names(global_data$bulk_datasets)
      if (length(ds_names) == 0L) {
        return(tags$div(class = "small text-muted",
                        .tr("Aucun dataset à activer.")))
      }
      tagList(
        selectInput(ns("ds_to_activate"), .tr("Dataset à activer"),
                    choices = ds_names, selected = ds_names[1L], width = "100%"),
        actionButton(ns("ds_activate"), .tr("✅ Activer ce dataset"),
                     icon = icon("circle-play"), class = "btn-outline-success w-100 mb-1"),
        helpText(style = "font-size:0.76em;", .tr(
          "L'activation remplace le jeu actif : filtrage, contrastes et voies du jeu précédent sont purgés."))
      )
    })

    observeEvent(input$ds_activate, {
      global_data$language  # i18n
      lbl <- input$ds_to_activate
      req(lbl, nzchar(lbl))
      entry <- tryCatch(bulk_multi_get(global_data$bulk_datasets, lbl),
                        bulk_multi_error = function(e) e)
      if (inherits(entry, "bulk_multi_error")) {
        showNotification(entry$message, type = "error", duration = 8)
        return()
      }
      dims <- tryCatch(
        sprintf("%s gènes × %s échantillons",
                format(nrow(entry$obj$counts), big.mark = " "),
                format(ncol(entry$obj$counts), big.mark = " ")),
        error = function(e) "?")
      global_data$bulk_obj <- entry$obj
      global_data$bulk_obj_epoch <- (global_data$bulk_obj_epoch %||% 0L) + 1L
      showNotification(
        sprintf(.tr("✓ Jeu « %s » activé (%s) — objet actif remplacé, résultats purgés."),
                lbl, dims),
        type = "message", duration = 6)
    })

    # ── Summary table (pure consumer of bulk_multi_summary) ─────────────
    output$ds_summary <- renderDT({
      global_data$language  # i18n
      summary_df <- bulk_multi_summary(global_data$bulk_datasets)
      validate(need(nrow(summary_df) > 0,
                    .tr("Aucun dataset enregistré pour l'instant.")))
      ts_datatable(summary_df, page_length = 6, buttons = FALSE)
    })
  })
}

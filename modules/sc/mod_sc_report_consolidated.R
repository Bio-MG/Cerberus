# =============================================================================
# mod_sc_report_consolidated.R — Rapport consolidé 4F (Stage 17)
# =============================================================================
# UI + orchestration UNIQUEMENT : la logique domaine vit dans le paquet (tools/pkg_skeleton/R/, ex-R/reports/ — D2 M5 lot 2)
# (collecteur / validateur / rendu / bundle — fonctions pures). Ce module :
#   - expose les options de rapport (titre, sous-titre, notes, tables) ;
#   - affiche l'APERCU des verdicts de validation par section (réactif) ;
#   - branche deux téléchargements : rapport HTML autonome + bundle .zip.
#
# ZÉRO calcul scientifique : collect_consolidated_report_input() ne fait que
# LIRE l'état existant ; validate_consolidated_report_input() ne fait que
# qualifier ; le rendu ne fait que mettre en forme. Les analyses absentes
# sont signalées gracieusement, les sections sans provenance sont refusées.
# =============================================================================

mod_sc_report_consolidated_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-light", style = "font-size:0.85em;border-left:3px solid #18BC9C;",
        bsicons::bs_icon("clipboard-check"),
        i18n$t("Compile l'état canonique + la provenance en un rapport HTML autonome et un bundle d'export projet. Aucune analyse n'est ré-exécutée.")),
    textInput(ns("rep_title"), i18n$t("Titre"), value = "Rapport consolidé du projet"),
    textInput(ns("rep_subtitle"), i18n$t("Sous-titre (optionnel)")),
    textAreaInput(ns("rep_notes"), i18n$t("Notes"), rows = 3),
    checkboxInput(ns("rep_tables"), i18n$t("Inclure les tables de résumés"), value = TRUE),
    downloadButton(ns("dl_consolide_html"), i18n$t("📄 Rapport HTML compilé"),
                   class = "btn-dark w-100 mt-2"),
    downloadButton(ns("dl_consolide_bundle"), i18n$t("🧳 Bundle d'export projet (.zip)"),
                   class = "btn-outline-secondary w-100 mt-1"),
    div(class = "small text-muted mt-1", textOutput(ns("rep_consolide_status")))
  )
}

mod_sc_report_consolidated_output_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header(i18n$t("Rapport consolidé — aperçu des verdicts")),
    uiOutput(ns("rep_verdicts_ui")),
    hr(),
    uiOutput(ns("rep_provenance_count_ui"))
  )
}

mod_sc_report_consolidated_server <- function(id, global_data, shared_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    .tr <- function(key) {
      tr <- isolate(global_data$i18n)
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # ── Collecte + validation (lecture seule de l'état — zéro calcul) ────────
    .collect_and_validate <- function() {
      obj <- global_data$sc_obj
      if (is.null(obj)) return(NULL)
      opts <- list(
        title = input$rep_title %||% "Rapport consolidé du projet",
        subtitle = input$rep_subtitle %||% "",
        notes = input$rep_notes %||% "",
        language = isolate(global_data$language) %||% "fr",
        include_tables = isTRUE(input$rep_tables)
      )
      ri <- collect_consolidated_report_input(obj, shared_rv, options = opts,
                                              global_data = global_data)
      list(ri = ri, val = validate_consolidated_report_input(ri))
    }

    state_label_color <- function(state) {
      switch(state,
             valid = "success", valid_legacy = "success",
             stale = "warning", unknown = "warning",
             invalid = "danger", blocked = "danger",
             "secondary")  # absent
    }

    # ── Aperçu des verdicts (onglet de sortie) ────────────────────────────────
    output$rep_verdicts_ui <- renderUI({
      cv <- .collect_and_validate()
      if (is.null(cv)) {
        return(div(class = "alert alert-info m-3",
                   .tr("Objet SC requis : importez et traitez un objet d'abord.")))
      }
      rows <- lapply(cv$val$verdicts, function(v) {
        tags$tr(
          tags$td(tags$strong(v$section)),
          tags$td(tags$span(class = paste0("badge bg-",
                                            state_label_color(v$state)), v$state)),
          tags$td(paste(v$analysis_ids, collapse = ", ")),
          tags$td(style = "font-size:0.82em;", v$label)
        )
      })
      tagList(
        tags$table(class = "table table-sm table-hover",
                   tags$thead(tags$tr(
                     tags$th(.tr("Section")), tags$th(.tr("État")),
                     tags$th("analysis_id"), tags$th(.tr("Verdict")))),
                   tags$tbody(rows))
      )
    })

    output$rep_provenance_count_ui <- renderUI({
      cv <- .collect_and_validate()
      if (is.null(cv)) return(NULL)
      tagList(
        h6(paste(.tr("Provenance consolidée :"), nrow(cv$ri$provenance_df),
                 .tr("entrée(s) produite(s) par les analyses"))),
        p(class = "small text-muted", consolidated_report_input_recap(cv$ri))
      )
    })

    output$rep_consolide_status <- renderText({
      if (is.null(global_data$sc_obj)) return(.tr("Objet SC requis."))
      cv <- .collect_and_validate()
      if (is.null(cv)) return("")
      blocked <- cv$val$blocked_sections
      if (length(blocked)) {
        paste(.tr("⚠ sections refusées (provenance absente) :"), paste(blocked, collapse = ", "))
      } else {
        paste(.tr("Prêt —"), sum(vapply(cv$val$verdicts, function(v) v$state != "absent", logical(1))),
              .tr("section(s) avec résultat."))
      }
    })

    # ── Téléchargement : rapport HTML autonome ────────────────────────────────
    output$dl_consolide_html <- downloadHandler(
      filename = function() consolidated_report_export_filename("rapport_consolide", "html"),
      content = function(file) {
        req(global_data$sc_obj)
        cv <- tryCatch(.collect_and_validate(), error = function(e) e)
        if (inherits(cv, "error")) {
          showNotification(paste0("✗ ", .tr("Échec du rapport consolidé : "),
                                  conditionMessage(cv)), type = "error", duration = 12)
          return(invisible(NULL))
        }
        withProgress(message = .tr_plain("Génération du rapport consolidé..."), value = 0.5, {
          tryCatch({
            write_consolidated_report_html(cv$ri, cv$val, file)
            showNotification(paste0("✓ ", .tr("Rapport consolidé généré.")),
                             type = "message", duration = 4)
          }, error = function(e) {
            showNotification(paste0("✗ ", .tr("Échec du rapport consolidé : "),
                                    conditionMessage(e)), type = "error", duration = 12)
          })
        })
      }
    )

    # ── Téléchargement : bundle d'export projet (.zip) ────────────────────────
    output$dl_consolide_bundle <- downloadHandler(
      filename = function() consolidated_report_export_filename("bundle_consolide", "zip"),
      content = function(file) {
        req(global_data$sc_obj)
        cv <- tryCatch(.collect_and_validate(), error = function(e) e)
        if (inherits(cv, "error")) {
          showNotification(paste0("✗ ", .tr("Échec du rapport consolidé : "),
                                  conditionMessage(cv)), type = "error", duration = 12)
          return(invisible(NULL))
        }
        withProgress(message = .tr_plain("Assemblage du bundle d'export..."), value = 0.4, {
          tmp_dir <- tempfile("report_bundle_")
          on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
          tryCatch({
            bundle <- build_report_bundle(tmp_dir, cv$ri, cv$val)
            files_abs <- file.path(bundle$bundle_dir, bundle$files)
            zip::zip(file, files = files_abs, mode = "cherry-pick")
            showNotification(paste0("✓ ", .tr("Bundle d'export généré.")),
                             type = "message", duration = 4)
          }, error = function(e) {
            showNotification(paste0("✗ ", .tr("Échec du bundle d'export : "),
                                    conditionMessage(e)), type = "error", duration = 12)
          })
        })
      }
    )
  })
}

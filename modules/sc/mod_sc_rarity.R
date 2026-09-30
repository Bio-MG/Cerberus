# =============================================================================
# modules/sc/mod_sc_rarity.R — Rarete par population annotee (CCC 9, question 1)
# =============================================================================
# UI + orchestration UNIQUEMENT : tout le calcul vit dans
# R/sc/sc_population_rarity.R (logique pure). Ce module ne recompte JAMAIS :
# il appelle compute_population_rarity() et affiche le resultat canonique.
#
# Jalon DESCRIPTIF et MONO-CONDITION : aucune comparaison entre conditions,
# aucun test -> la porte Stage 13 (assert_da_design_result) ne s'applique pas.
# Voir docs/contracts/POPULATION_RARITY_CONTRACT.md §1.1.
#
# Le seuil de rarete est un CHOIX DECLARE : le numericInput part VIDE (value =
# NA) et le domaine refuse un appel sans seuil — aucun defaut implicite n'est
# fabrique par l'UI.
# =============================================================================

mod_sc_rarity_ui <- function(id) {
  ns <- NS(id)
  # Le VOCABULAIRE des regles vient du domaine (population_rarity_rule_types) :
  # l'UI ne re-code jamais la liste autorisee — le contrat reste la source de
  # verite unique (verifie par le test de freeze). Seuls les libelles sont ici.
  rules <- population_rarity_rule_types()
  rule_labels <- c(
    absolute_n_cells  = .tr_plain("Nombre de cellules sous le seuil"),
    relative_fraction = .tr_plain("Fraction du jeu sous le seuil")
  )
  tagList(
    div(class = "alert alert-light",
        style = "font-size:0.9em;border-left:3px solid #18BC9C;",
        i18n$t("Compte les cellules par population annotée et signale celles qui passent sous un seuil que vous déclarez."),
        tags$br(),
        tags$small(i18n$t("Analyse descriptive, mono-condition : aucune comparaison entre conditions, aucun test."))),
    selectInput(ns("rarity_identity"), i18n$t("Colonne d'identité (populations)"),
                choices = NULL),
    radioButtons(ns("rarity_rule"), i18n$t("Règle de rareté"),
                 choices = setNames(rules, unname(rule_labels[rules])),
                 inline = FALSE),
    # Aucune valeur par defaut : le seuil est DECLARE (contrat §1.3).
    numericInput(ns("rarity_threshold"), i18n$t("Seuil déclaré"),
                 value = NA, min = 0, step = 1),
    selectInput(ns("rarity_sample"), i18n$t("Colonne d'échantillon (optionnel)"),
                choices = NULL),
    actionButton(ns("rarity_run"), i18n$t("Calculer la rareté"),
                 class = "btn-info w-100", icon = icon("magnifying-glass-chart")),
    hr(),
    div(class = "small text-muted", textOutput(ns("rarity_status")))
  )
}

mod_sc_rarity_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header(div(style = "display:flex;justify-content:space-between;align-items:center;",
                    h5(i18n$t("Rareté par population (descriptif)"), class = "mb-0"),
                    downloadButton(ns("dl_rarity"), "Export CSV",
                                   class = "btn-sm btn-info"))),
    layout_columns(col_widths = c(12),
      card(card_header(i18n$t("Populations et rareté déclarée")),
           DTOutput(ns("rarity_table"))),
      card(card_header(i18n$t("Règle déclarée et compteurs")),
           tableOutput(ns("rarity_summary"))),
      card(card_header(i18n$t("Composition par population (descriptif)")),
           plotOutput(ns("rarity_plot"), height = "340px")))
  )
}

mod_sc_rarity_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    # ── i18n proxy ──────────────────────────────────────────────────────────
    .tr <- function(key) {
      tr <- isolate(global_data$i18n)
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    rarity_state <- reactiveValues(result = NULL)

    # UN SEUL observateur par declencheur (garde anti-duplication) : reset +
    # synchronisation des choix depuis l'objet Seurat courant.
    observeEvent(global_data$sc_obj, {
      rarity_state$result <- NULL
      if (!is.null(shared_rv)) shared_rv$population_rarity_result <- NULL  # purge champ partagé (audit §1.5 : résultat périmé exporté sinon)
      obj <- global_data$sc_obj
      if (is.null(obj)) {
        updateSelectInput(session, "rarity_identity", choices = character(0))
        updateSelectInput(session, "rarity_sample", choices = character(0))
        return()
      }
      meta_cols <- colnames(obj@meta.data)
      updateSelectInput(session, "rarity_identity", choices = meta_cols,
                        selected = if ("seurat_clusters" %in% meta_cols)
                          "seurat_clusters" else meta_cols[1])
      updateSelectInput(session, "rarity_sample",
                        choices = c(setNames("", .tr("(aucune)")), meta_cols),
                        selected = "")
    }, ignoreInit = FALSE)

    output$rarity_status <- renderText({
      r <- rarity_state$result
      if (is.null(r)) {
        return(.tr("En attente d'un calcul. Choisissez la colonne d'identité, la règle et le seuil déclaré."))
      }
      label <- population_rarity_status_labels()[[r$status]] %||% r$status
      if (identical(r$status, "unavailable_single_population")) {
        return(sprintf("⚠️ %s", label))
      }
      sprintf("✅ %s — %d population(s), %d rare(s) (%s).",
              label,
              r$summary$n_populations %||% NA_integer_,
              r$summary$n_rare %||% NA_integer_,
              r$summary$declared_rule_label %||% "")
    })

    # ── Calcul (orchestration uniquement) ───────────────────────────────────
    observeEvent(input$rarity_run, {
      req(global_data$sc_obj)
      tryCatch({
        obj <- global_data$sc_obj
        ident_col <- input$rarity_identity
        if (is.null(ident_col) || !nzchar(ident_col)) {
          stop("Choisissez d'abord la colonne de métadonnées décrivant les populations.",
               call. = FALSE)
        }
        rule <- input$rarity_rule %||% NULL
        # Le seuil est DECLARE : l'UI part vide, et un champ vide n'est pas
        # converti en valeur par defaut — le domaine refuse (invalid_input).
        thr <- input$rarity_threshold
        if (is.null(thr) || length(thr) != 1L || is.na(thr)) thr <- NULL

        sample_col <- input$rarity_sample
        if (is.null(sample_col) || !nzchar(sample_col)) sample_col <- NULL

        res <- compute_population_rarity(
          meta            = obj@meta.data,
          identity_column = ident_col,
          rule_type       = rule,
          threshold       = thr,
          sample_column   = sample_col,
          seurat_obj      = obj
        )
        assert_population_rarity_result(res, context = "module rareté")
        rarity_state$result <- res
        if (!is.null(shared_rv)) shared_rv$population_rarity_result <- res
        provenance_append(shared_rv, res$provenance)
        showNotification(.tr("Calcul de rareté terminé."),
                         type = "message", duration = 4)
      }, error = function(e) {
        rarity_state$result <- NULL
        if (!is.null(shared_rv)) shared_rv$population_rarity_result <- NULL  # purge champ partagé aussi en cas d'échec
        showNotification(
          paste(.tr_plain("Erreur rareté :"), conditionMessage(e)),
          type = "error", duration = 10)
      })
    })

    # ── Affichages (lecture du canonique, aucune deduction) ─────────────────
    output$rarity_table <- renderDT({
      req(rarity_state$result)
      tab <- build_population_rarity_table_export(rarity_state$result)
      ts_datatable(tab, page_length = 15L, filename_base = "population_rarity")
    })

    output$rarity_summary <- renderTable({
      req(rarity_state$result)
      smry <- build_population_rarity_summary(rarity_state$result)
      data.frame(
        Champ = c("analysis_id", "identity_column", "rule_type", "threshold",
                  "declared_rule_label", "n_populations", "n_rare",
                  "n_cells_total", "n_cells_counted", "n_labels_na",
                  "n_levels_empty", "descriptive_only"),
        Valeur = c(smry$analysis_id, smry$identity_column, smry$rule_type,
                   smry$threshold, smry$declared_rule_label,
                   smry$n_populations, smry$n_rare, smry$n_cells_total,
                   smry$n_cells_counted, smry$n_labels_na, smry$n_levels_empty,
                   smry$descriptive_only),
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, bordered = TRUE)

    # Figure DESCRIPTIVE (jamais causale) : composition du jeu analyse.
    # width/height clamps : l'ouverture du device PNG par Shiny appelle
    # plot.new() AVANT d'evaluer l'expression — si l'output est rendu dans un
    # conteneur replie/masque (taille rapportee ~0), le device est trop petit
    # pour les marges par defaut et Shiny logge "figure margins too large".
    # Les fonctions de taille imposent un plancher (200px) : l'erreur
    # disparait, le comportement a l'ecran reste identique (tailles >= plancher
    # utilisees telles quelles), et req() suspend l'output sans donnees.
    output$rarity_plot <- renderPlot(
      {
        req(rarity_state$result)
        r <- rarity_state$result
        tab <- r$population_table
        tab$rarete <- ifelse(is.na(tab$is_rare), .tr("non qualifiée"),
                             ifelse(tab$is_rare, .tr("rare"), .tr("non rare")))
        ggplot2::ggplot(tab, ggplot2::aes(x = stats::reorder(population, -n_cells),
                                          y = n_cells, fill = rarete)) +
          ggplot2::geom_col() +
          ggplot2::labs(
            title = .tr("Composition par population (descriptif)"),
            subtitle = sprintf("%s — %s", r$summary$declared_rule_label,
                               .tr("règle déclarée par l'utilisateur")),
            x = r$identity_column, y = .tr("Nombre de cellules"),
            fill = .tr("Rareté déclarée")) +
          ts_theme() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      },
      width = function() {
        w <- session$clientData[["output_sc-rarity-rarity_plot_width"]]
        max(w %||% 0, 200)
      },
      height = function() {
        h <- session$clientData[["output_sc-rarity-rarity_plot_height"]]
        max(h %||% 0, 200)
      }
    )

    output$dl_rarity <- downloadHandler(
      filename = function() {
        req(rarity_state$result)
        population_rarity_export_filename(rarity_state$result, ext = "csv")
      },
      content = function(file) {
        req(rarity_state$result)
        utils::write.csv(
          build_population_rarity_table_export(rarity_state$result),
          file, row.names = FALSE)
      }
    )
  })
}

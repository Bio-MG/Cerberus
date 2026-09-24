# =============================================================================
# mod_sc_communication.R — Communication cellule-cellule 4D-1 + 4D-2
# (Stage 11 : import et validation / Stage 12 : exploration, vues, exports)
# =============================================================================
# Stage 11 : le module ORCHESTRE uniquement — parseurs par source (CellChat
# table, CellChat objet .rds, CellPhoneDB means/pvalues), harmonisation des
# identites (exact match), QC d'import, statut canonique, provenance et
# exports residuent dans R/sc/sc_communication.R.
# Stage 12 : les vues (DotPlot, heatmap pathways, reseau circulaire,
# centralite) sont des CONSOMMATRICES pures du resultat canonique via
# R/sc/sc_communication_views.R ; les filtres sont des operations d'affichage
# (la table canonique n'est jamais modifiee) et sont capturés dans la
# provenance a l'export. Le type de score et la methode source sont affiches
# sur chaque vue — les scores de sources differentes ne sont jamais compares.
# Aucun calcul CellPhoneDB, aucune inference sur les imports, aucun matching
# flou. Depuis CC-5 le module peut aussi CALCULER (source « Calculer dans
# l'application ») : c'est le moteur R/sc/sc_communication_engine.R qui fait le
# calcul — le module ne fait qu'orchestrer, et le resultat est depose
# EXACTEMENT comme un import (regle des deux voies : meme forme, donc memes
# vues et memes exports, sans qu'aucun n'ait eu a changer).
# =============================================================================

mod_sc_communication_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-light",
        style = "font-size:0.9em;border-left:3px solid #2980B9;",
         i18n$t("Communication cellule-cellule — import de resultats externes (CellChat / CellPhoneDB / LIANA) ou calcul CellChat / LIANA dans l'application. "),
        i18n$t("Aucun score recompose. Les scores de sources differentes ne sont pas comparables.")),

    # Le moteur natif (CC-5) n'est pas un repli mais la voie NORMALE : il est
    # donc propose EN TETE et SELECTIONNE par defaut (decision utilisateur du
    # 2026-09-16). Les quatre sources d'import restent disponibles, dans leur
    # ordre d'origine. ⚠️ Le defaut est DECLARE ici ET repris par le repli
    # serveur (`input$comm_source %||% ...`) : les deux doivent rester
    # d'accord — garde test-sc-communication-engine-ui.R.
    radioButtons(ns("comm_source"), i18n$t("Source des resultats"),
                 choices = setNames(
                   c("cellchat_engine", "cellchat", "cellchat_object", "cellphonedb", "liana", "liana_engine"),
                   c(.tr_plain("Calculer dans l'application (CellChat)"),
                     .tr_plain("CellChat (table exportee)"),
                     .tr_plain("Objet CellChat (.rds, resultats deja calcules)"),
                     .tr_plain("CellPhoneDB (means.txt)"),
                     .tr_plain("LIANA (rangs agreges)"),
                     .tr_plain("Calculer dans l'application (LIANA)"))),
                 selected = "cellchat_engine"),

    conditionalPanel(
      condition = "input.comm_source == 'cellchat'", ns = ns,
      fileInput(ns("comm_cellchat_file"), i18n$t("Table CellChat exportee (CSV/TSV)"),
                accept = c(".csv", ".tsv", ".txt"), width = "100%"),
      div(class = "small text-muted mb-2",
          i18n$t("Colonnes attendues : source, target, ligand, receptor, prob (pathway/pval optionnels)."))
    ),

    conditionalPanel(
      condition = "input.comm_source == 'cellchat_object'", ns = ns,
      fileInput(ns("comm_cellchat_obj"), i18n$t("Objet CellChat sauvegarde (.rds)"),
                accept = c(".rds", ".RDS", ".rda", ".RData"), width = "100%"),
      div(class = "small text-muted mb-2",
          i18n$t("Extraction des resultats deja calcules (net$prob) — aucune methode CellChat n'est relancee. La colonne pathway reste vide pour cette route."))
    ),

    conditionalPanel(
      condition = "input.comm_source == 'cellphonedb'", ns = ns,
      fileInput(ns("comm_cpdb_means"), i18n$t("CellPhoneDB means.txt (requis)"),
                accept = c(".txt", ".tsv", ".csv"), width = "100%"),
      fileInput(ns("comm_cpdb_pvalues"), i18n$t("CellPhoneDB pvalues.txt (optionnel)"),
                accept = c(".txt", ".tsv", ".csv"), width = "100%"),
      div(class = "small text-muted mb-2",
          i18n$t("Format v2 : colonne interacting_pair (ligand|receptor) + une colonne par paire sender|receiver."))
    ),

    # ── CCC 7-8 route (b) : import de RANGS LIANA produits hors de l'app ────
    # Aucune inference : l'app lit une table agregee deja calculee. Deux choix
    # EXPLICITES sont exiges (mode d'agregation + colonne de rang) car ni l'un
    # ni l'autre n'est deductible du contenu du fichier.
    conditionalPanel(
      condition = "input.comm_source == 'liana'", ns = ns,
      fileInput(ns("comm_liana_file"), i18n$t("Table LIANA agregee (CSV/TSV)"),
                accept = c(".csv", ".tsv", ".txt"), width = "100%"),
      radioButtons(ns("comm_liana_mode"), i18n$t("Mode d'agregation LIANA (requis)"),
                   choices = setNames(c("specificity", "magnitude"),
                                      c(.tr_plain("Specificite — l'interaction est-elle specifique de ces types cellulaires ?"),
                                        .tr_plain("Magnitude — l'interaction est-elle abondante ?"))),
                   selected = character(0)),
      selectInput(ns("comm_liana_rank"), i18n$t("Colonne de rang a importer (requis)"),
                  choices = character(0), width = "100%"),
      div(class = "small text-muted mb-2",
          i18n$t("Colonnes de rang : mean_rank, aggregate_rank ou {methode}.rank. Dans LIANA, rang 1 = meilleur (inverse de prob) : aucun score n'est reconstitue, et les modes specificite/magnitude ne sont pas comparables entre eux."))
    ),

    # ── CC-5 : Path B — calculer CellChat DANS l'application ──────────────────
    # Le moteur (R/sc/sc_communication_engine.R) produit EXACTEMENT la meme forme
    # qu'un import (les 12 champs canoniques du contrat Stage 11) et le resultat
    # est depose au MEME endroit : toutes les vues et tous les exports sont donc
    # conserves SANS modification. Regle des deux voies : la moindre divergence
    # de forme entre import et calcul serait un BUG, pas une variante.
    conditionalPanel(
      condition = "input.comm_source == 'cellchat_engine'", ns = ns,
      selectInput(ns("comm_engine_species"), i18n$t("Espece (base ligand-recepteur)"),
                  choices = setNames(c("human", "mouse"),
                                     c(.tr_plain("Humain (CellChatDB.human)"),
                                       .tr_plain("Souris (CellChatDB.mouse)"))),
                  selected = character(0), width = "100%"),
      div(class = "small text-muted mb-2",
          i18n$t("Choix OBLIGATOIRE : la base ligand-recepteur en decoule, elle ne se devine pas a partir des donnees.")),
      numericInput(ns("comm_engine_seed"), i18n$t("Graine"),
                   value = TS_CELLCHAT_SEED_DEFAULT, min = 1, step = 1, width = "100%"),
      numericInput(ns("comm_engine_nboot"), i18n$t("Nombre de permutations"),
                   value = TS_CELLCHAT_NBOOT_DEFAULT, min = 1, step = 1, width = "100%"),
      div(class = "small text-muted mb-2",
          i18n$t("Les p-values viennent d'une permutation : la graine est un parametre du calcul, elle est tracee dans la provenance. Plus de permutations = calcul plus long."))
    ),

    conditionalPanel(
      condition = "input.comm_source == 'liana_engine'", ns = ns,
      div(class = "small text-muted mb-2",
          i18n$t("LIANA natif : les résultats sont calculés par échantillon biologique.")),
      selectInput(ns("comm_liana_sample_col"), i18n$t("Colonne d'echantillon (LIANA)"),
                  choices = character(0), selected = character(0), width = "100%"),
      selectInput(ns("comm_liana_condition_col"), i18n$t("Colonne de condition (LIANA)"),
                  choices = character(0), selected = character(0), width = "100%"),
      div(class = "small text-muted mb-2",
          i18n$t("La colonne d'identites utilise le selecteur commun.")),
      selectInput(ns("comm_liana_method"), i18n$t("Methode LIANA"),
                  choices = setNames(TS_LIANA_METHOD_DEFAULT, TS_LIANA_METHOD_DEFAULT),
                  selected = TS_LIANA_METHOD_DEFAULT, width = "100%"),
      selectInput(ns("comm_liana_resource"), i18n$t("Ressource LIANA"),
                  choices = setNames(TS_LIANA_RESOURCE_DEFAULT, TS_LIANA_RESOURCE_DEFAULT),
                  selected = TS_LIANA_RESOURCE_DEFAULT, width = "100%"),
      numericInput(ns("comm_liana_seed"), i18n$t("Graine LIANA"),
                   value = TS_LIANA_SEED_DEFAULT, min = 1, step = 1, width = "100%"),
      numericInput(ns("comm_liana_min_cells"), i18n$t("Cellules minimum par echantillon"),
                   value = TS_LIANA_MIN_CELLS_DEFAULT, min = 1, step = 1, width = "100%"),
      div(class = "small text-muted mb-2",
          i18n$t("La graine et le minimum de cellules sont des paramètres du calcul et sont conservés dans la provenance.")),
      actionButton(ns("comm_compute_liana"), i18n$t("Lancer le calcul LIANA"),
                   class = "btn-primary w-100", icon = icon("cogs")),
      selectInput(ns("comm_active_sample"), i18n$t("Echantillon actif (LIANA)"),
                  choices = character(0), width = "100%"),
      div(class = "small text-muted mt-1", textOutput(ns("comm_collection_status")))
    ),

    hr(),
    selectInput(ns("comm_identity_column"),
                i18n$t("Colonne d'identites cellulaires (Seurat)"),
                choices = character(0), width = "100%"),
    div(class = "small text-muted mb-2",
        i18n$t("Harmonisation par exact match uniquement : aucun renommage de populations, labels sans correspondance listes integralement.")),
    # Le bouton d'import n'a pas de sens pour la source « calcul dans
    # l'application » : il lancerait la branche import sur une entree absente.
    conditionalPanel(
      condition = "input.comm_source == 'cellchat' || input.comm_source == 'cellchat_object' || input.comm_source == 'cellphonedb' || input.comm_source == 'liana'", ns = ns,
      actionButton(ns("comm_import"), i18n$t("Importer et valider"),
                   class = "btn-primary w-100", icon = icon("check"))
    ),
    conditionalPanel(
      condition = "input.comm_source == 'cellchat_engine'", ns = ns,
      actionButton(ns("comm_compute"), i18n$t("Lancer le calcul CellChat"),
                   class = "btn-primary w-100", icon = icon("cogs"))
    ),
    div(class = "small text-muted mt-1", textOutput(ns("comm_status"))),
    hr(),
    downloadButton(ns("dl_comm_table"), i18n$t("Exporter table canonique (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_comm_filtered"), i18n$t("Exporter table filtree (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_comm_mapping"), i18n$t("Exporter harmonisation identites (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_comm_summary"), i18n$t("Exporter resume d'import (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_comm_liana_manifest"), i18n$t("Exporter manifeste des echantillons (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_comm_liana_condition_summary"), i18n$t("Exporter resume des conditions (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_comm_centrality"), i18n$t("Exporter centralite (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_comm_rds"), i18n$t("Exporter resultat valide (RDS)"), class = "btn-sm btn-info w-100 mt-1")
  )
}

mod_sc_communication_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header(i18n$t("Communication cellule-cellule — resultats importes")),
    # ── Filtres d'affichage (operations d'affichage, captures en provenance
    # a l'export — la table canonique du resultat n'est jamais modifiee) ────
    div(class = "border rounded p-2 m-2", style = "background:#f8f9fa;",
        fluidRow(
          column(3, numericInput(ns("comm_f_score_min"), i18n$t("Score minimum (importé)"),
                                 value = NA, min = 0, width = "100%")),
          column(3, numericInput(ns("comm_f_pmax"), i18n$t("P-value maximum (importée)"),
                                 value = NA, min = 0, max = 1, width = "100%")),
          column(6, selectizeInput(ns("comm_f_pathways"), i18n$t("Pathways"),
                                   choices = character(0), multiple = TRUE,
                                   options = list(placeholder = .tr_plain("Tous"))))),
        fluidRow(
          column(3, numericInput(ns("comm_f_rank_max"), i18n$t("Rang maximum (importé)"),
                                 value = NA, min = 0, width = "100%"))),
        fluidRow(
          column(4, selectizeInput(ns("comm_f_senders"), i18n$t("Senders"),
                                   choices = character(0), multiple = TRUE,
                                   options = list(placeholder = .tr_plain("Tous")))),
          column(4, selectizeInput(ns("comm_f_receivers"), i18n$t("Receivers"),
                                   choices = character(0), multiple = TRUE,
                                   options = list(placeholder = .tr_plain("Tous")))),
          column(4, checkboxInput(ns("comm_f_include_self"),
                                  i18n$t("Inclure les auto-interactions"), value = TRUE))),
        div(class = "small text-muted", textOutput(ns("comm_filter_summary")))),
    navset_tab(
      nav_panel(i18n$t("Table interactions"),
                DT::dataTableOutput(ns("comm_table"), height = "520px")),
      nav_panel(i18n$t("Table (filtres)"),
                DT::dataTableOutput(ns("comm_table_filtered"), height = "520px")),
      nav_panel(i18n$t("DotPlot"),
                plotOutput(ns("comm_dotplot"), height = "480px"),
                fluidRow(
                  column(6, downloadButton(ns("dl_comm_dotplot_png"), i18n$t("PNG"), class = "btn-sm btn-outline-secondary")),
                  column(6, downloadButton(ns("dl_comm_dotplot_pdf"), i18n$t("PDF"), class = "btn-sm btn-outline-secondary")))),
      nav_panel(i18n$t("Heatmap pathways"),
                plotOutput(ns("comm_heatmap"), height = "480px"),
                fluidRow(
                  column(6, downloadButton(ns("dl_comm_heatmap_png"), i18n$t("PNG"), class = "btn-sm btn-outline-secondary")),
                  column(6, downloadButton(ns("dl_comm_heatmap_pdf"), i18n$t("PDF"), class = "btn-sm btn-outline-secondary")))),
      nav_panel(i18n$t("Réseau"),
                plotOutput(ns("comm_circle"), height = "520px"),
                fluidRow(
                  column(6, downloadButton(ns("dl_comm_circle_png"), i18n$t("PNG"), class = "btn-sm btn-outline-secondary")),
                  column(6, downloadButton(ns("dl_comm_circle_pdf"), i18n$t("PDF"), class = "btn-sm btn-outline-secondary")))),
      nav_panel(i18n$t("Centralité"),
                div(class = "small text-muted m-2",
                    i18n$t("Centralité dérivée du réseau, descriptive — ce n'est pas un contrôle biologique.")),
                DT::dataTableOutput(ns("comm_centrality"), height = "420px")),
      # ── Contextes V1.x (roadmap CCC avancée : contrainte/ordre/contexte —
      # consommateurs purs, aucun recalcul ; docs/ROADMAP_CCC_ADVANCED.md) ──
      nav_panel(i18n$t("Contexte spatial"),
                mod_sc_communication_spatial_ui(ns("spatial"))),
      nav_panel(i18n$t("Contexte trajectoire"),
                mod_sc_communication_trajectory_ui(ns("trajectory"))),
      nav_panel(i18n$t("Contexte vélocité"),
                mod_sc_communication_velocity_ui(ns("velocity_ctx"))),
      nav_panel(i18n$t("Perturbation (in silico)"),
                mod_sc_communication_perturbation_ui(ns("perturbation"))),
      nav_panel(i18n$t("Schema source"),
                DT::dataTableOutput(ns("comm_schema"), height = "380px"),
                div(class = "small text-muted mt-2", textOutput(ns("comm_schema_note")))),
      nav_panel(i18n$t("Harmonisation identites"),
                DT::dataTableOutput(ns("comm_mapping"), height = "380px")),
      nav_panel(i18n$t("QC import"),
                uiOutput(ns("comm_qc"))),
      nav_panel(i18n$t("Limites"),
                div(class = "alert alert-warning m-3", style = "font-size:0.9em;",
                    i18n$t("Validite technique uniquement : un import valide n'implique aucune validite biologique.")),
                div(class = "alert alert-info m-3", style = "font-size:0.9em;",
                    i18n$t("Les scores ne sont pas compares entre sources differentes (CellChat vs CellPhoneDB).")),
                div(class = "alert alert-info m-3", style = "font-size:0.9em;",
                    i18n$t("Une association ligand-receptor n'etablit aucune causalite.")),
                div(class = "alert alert-light m-3", style = "font-size:0.9em;",
                    i18n$t("Les labels sans correspondance exacte dans la colonne choisie sont conserves tels quels et listes — jamais renommes.")),
                div(class = "alert alert-light m-3", style = "font-size:0.9em;",
                    i18n$t("Objet CellChat (.rds) : extraction de resultats deja calcules uniquement — le mapping ligand-receptor vers pathway n'est pas reconstitue.")))
    )
  )
}

mod_sc_communication_server <- function(id, global_data, shared_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    # ── i18n proxy ──────────────────────────────────────────────────────────
    .tr <- function(key) {
      tr <- isolate(global_data$i18n)
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # Libelle francais d'un etat de validite (fallback = code d'etat).
    .status_label <- function(st) {
      lab <- communication_status_labels()
      if (is.character(st) && length(st) == 1L && st %in% names(lab)) lab[[st]] else st
    }

    # Plot d'erreur standardise (message de validation exploitable).
    .error_plot <- function(e) {
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = conditionMessage(e), size = 5, colour = "red") +
        ggplot2::theme_void()
    }

    comm_state <- reactiveValues(
      result = NULL,
      object_fingerprint = NULL,
      collection = NULL,
      active_sample = NULL,
      context_cells = NULL
    )
    comm_status_rv <- reactiveVal(.tr("En attente d'import de resultats de communication..."))

    output$comm_status <- renderText({ comm_status_rv() })

    output$comm_collection_status <- renderText({
      collection <- comm_state$collection
      if (is.null(collection)) return(.tr("Aucune collection LIANA."))
      manifest <- liana_collection_sample_manifest(collection)
      conditions <- liana_collection_condition_summary(collection)
      .t_fmt(
        .tr("Collection LIANA : statut {status} — {n} echantillon(s), {c} condition(s)."),
        status = as.character(collection$status %||% NA_character_),
        n = nrow(manifest), c = nrow(conditions)
      )
    })

    # ── Lecture CSV/TSV explicite (check.names = FALSE : les colonnes de
    # paires CellPhoneDB "A|B" ne doivent jamais etre mangles en "A.B") ────
    .read_table_auto <- function(path) {
      ext <- tolower(tools::file_ext(path))
      read_fn <- switch(ext,
        "csv" = function(p) utils::read.csv(p, check.names = FALSE, stringsAsFactors = FALSE, comment.char = ""),
        function(p) utils::read.delim(p, check.names = FALSE, stringsAsFactors = FALSE, comment.char = "")
      )
      tryCatch(read_fn(path), error = function(e) {
        stop(sprintf(
          "Lecture du fichier impossible (%s) : %s",
          basename(path), conditionMessage(e)
        ), call. = FALSE)
      })
    }

    # Garde de peremption avant tout rendu/export (etat derive du contrat).
    .check_fingerprint <- function() {
      req(global_data$sc_obj)
      req(comm_state$object_fingerprint)
      current_fp <- velocity_object_fingerprint(global_data$sc_obj)
      if (!identical(comm_state$object_fingerprint, current_fp)) {
        stale_msg <- .status_label("stale_against_current_seurat_object")
        showNotification(stale_msg, type = "error", duration = 8)
        shiny::validate(shiny::need(FALSE, stale_msg))
      }
      invisible(TRUE)
    }

    # ── Choix de la colonne d'identites, synchronise avec l'objet Seurat ───
    # UN SEUL observateur par declencheur (garde anti-duplication) : reset du
    # resultat (aucun import ne survit a un changement d'objet — resultat
    # perime par construction) + resynchronisation des choix de colonne.
    observeEvent(global_data$sc_obj, {
      .clear_result_state()
      comm_status_rv(.tr("Objet Seurat modifie : reimportez la table de communication."))
      updateSelectInput(session, "comm_identity_column", choices = character(0),
                        selected = character(0))
      updateSelectInput(session, "comm_liana_sample_col", choices = character(0),
                        selected = character(0))
      updateSelectInput(session, "comm_liana_condition_col", choices = character(0),
                        selected = character(0))
      obj <- global_data$sc_obj
      if (is.null(obj)) return()
      meta_cols <- colnames(obj@meta.data)
      updateSelectInput(session, "comm_identity_column",
                        choices = meta_cols,
                        selected = if ("seurat_clusters" %in% meta_cols) "seurat_clusters" else meta_cols[1])
      updateSelectInput(session, "comm_liana_sample_col", choices = meta_cols,
                        selected = character(0))
      updateSelectInput(session, "comm_liana_condition_col", choices = meta_cols,
                        selected = character(0))
    }, ignoreInit = TRUE)

    # ── Colonnes de rang LIANA : proposees depuis l'en-tete du fichier ─────
    # La liste est PROPOSEE, jamais imposee : comme le mode d'agregation, le
    # choix reste explicite (aucun element preselectionne). On met en avant les
    # colonnes qui ressemblent a des rangs ; si aucune n'est reconnue, on
    # propose TOUTES les colonnes — le parseur diagnostiquera un choix
    # inexploitable plutot que d'interdire l'import.
    observeEvent(input$comm_liana_file, {
      f <- input$comm_liana_file
      tab <- if (is.null(f)) NULL else
        tryCatch(.read_table_auto(f$datapath), error = function(e) NULL)
      nms <- if (is.null(tab)) character(0) else colnames(tab)
      rank_like <- grep("(^mean_rank$)|(^aggregate_rank$)|(\\.rank$)",
                        nms, value = TRUE, ignore.case = TRUE)
      updateSelectInput(session, "comm_liana_rank",
                        choices = if (length(rank_like)) rank_like else nms,
                        selected = character(0))
    }, ignoreInit = TRUE)

    .view_object <- function() {
      if (is.null(comm_state$collection)) global_data$sc_obj else NULL
    }

    .liana_ui_stop <- function(state, message) {
      stop(errorCondition(
        paste0("Moteur LIANA — ", message),
        class = c("liana_engine_error", "error", "condition"),
        state = state
      ))
    }

    .import_ui_stop <- function(state, message) {
      stop(errorCondition(
        paste0("Import communication — ", message),
        class = c("communication_import_error", "error", "condition"),
        state = state
      ))
    }

    .clear_result_state <- function() {
      comm_state$result <- NULL
      comm_state$object_fingerprint <- NULL
      comm_state$collection <- NULL
      comm_state$active_sample <- NULL
      comm_state$context_cells <- NULL
      shared_rv$communication_result <- NULL
      shared_rv$communication_collection <- NULL
      shared_rv$active_communication_sample <- NULL
      updateNumericInput(session, "comm_f_score_min", value = NA)
      updateNumericInput(session, "comm_f_pmax", value = NA)
      updateNumericInput(session, "comm_f_rank_max", value = NA)
      updateSelectizeInput(session, "comm_f_pathways", choices = character(0),
                           selected = character(0))
      updateSelectizeInput(session, "comm_f_senders", choices = character(0),
                           selected = character(0))
      updateSelectizeInput(session, "comm_f_receivers", choices = character(0),
                           selected = character(0))
      updateSelectInput(session, "comm_active_sample", choices = character(0),
                        selected = character(0))
      invisible(NULL)
    }

    # ── Filtres reactifs (operation d'affichage uniquement) ────────────────
    .num_filter <- function(x) {
      if (is.null(x) || length(x) != 1L || is.na(x)) NA_real_ else as.numeric(x)
    }
    comm_filtered <- reactive({
      req(comm_state$result)
      communication_apply_filters(comm_state$result, list(
        score_min    = .num_filter(input$comm_f_score_min),
        rank_max     = .num_filter(input$comm_f_rank_max),
        p_value_max  = .num_filter(input$comm_f_pmax),
        pathways     = input$comm_f_pathways %||% character(0),
        senders      = input$comm_f_senders %||% character(0),
        receivers    = input$comm_f_receivers %||% character(0),
        include_self = isTRUE(input$comm_f_include_self)
      ))
    })

    output$comm_filter_summary <- renderText({
      req(comm_state$result)
      fs <- comm_filtered()$summary
      sprintf(
        .tr("%d interaction(s) affichée(s) sur %d — retirées : score %d, rang %d, p-value %d, pathway %d, sender %d, receiver %d, auto %d"),
        fs$n_after, fs$n_before, fs$dropped_score, fs$dropped_rank,
        fs$dropped_p_value, fs$dropped_pathway, fs$dropped_sender,
        fs$dropped_receiver, fs$dropped_self
      )
    })

    # Depot d'un resultat canonique : IDENTIQUE pour les deux voies (import =
    # Path A, calcul = Path B). Factorise parce que la regle des deux voies
    # exige que les deux chemins deposent le resultat au MEME endroit — sinon
    # elles divergeraient, et une divergence est un bug.
    .store_result <- function(canonical, obj, append_provenance = TRUE,
                              collection = NULL, active_sample = NULL) {
      context_cells <- NULL
      if (!is.null(collection) && length(active_sample) == 1L &&
          !is.na(active_sample) && nzchar(as.character(active_sample))) {
        manifest <- liana_collection_sample_manifest(collection)
        sample_index <- which(as.character(manifest$sample_key) ==
                              as.character(active_sample))
        if (length(sample_index) == 1L) {
          sample_id <- as.character(manifest$sample_id[[sample_index]])
          sample_col <- as.character(collection$sample_col)
          meta <- obj@meta.data
          if (length(sample_col) == 1L && !is.na(sample_col) &&
              sample_col %in% colnames(meta)) {
            context_cells <- rownames(meta)[
              trimws(as.character(meta[[sample_col]])) == sample_id
            ]
          }
          used_cells <- canonical$engine$cells_used_ids
          if (length(used_cells)) context_cells <- as.character(used_cells)
        }
      }
      if (!is.null(collection) && length(active_sample) == 1L &&
          !is.na(active_sample) && nzchar(as.character(active_sample))) {
        collection$active_sample_key <- as.character(active_sample)
      }
      comm_state$result <- canonical
      comm_state$object_fingerprint <- velocity_object_fingerprint(obj)
      comm_state$collection <- collection
      comm_state$active_sample <- active_sample
      comm_state$context_cells <- context_cells
      shared_rv$communication_result <- canonical
      shared_rv$communication_collection <- collection
      shared_rv$active_communication_sample <- active_sample
      if (isTRUE(append_provenance)) {
        provenance_append(shared_rv, canonical$provenance)
      }
      updateNumericInput(session, "comm_f_score_min", value = NA)
      updateNumericInput(session, "comm_f_pmax", value = NA)
      updateNumericInput(session, "comm_f_rank_max", value = NA)
      t0 <- canonical$canonical_table
      sender_nodes <- ifelse(!is.na(t0$sender_mapped), t0$sender_mapped, t0$sender)
      receiver_nodes <- ifelse(!is.na(t0$receiver_mapped), t0$receiver_mapped, t0$receiver)
      updateSelectizeInput(session, "comm_f_pathways",
                           choices = sort(unique(stats::na.omit(t0$pathway))),
                           selected = character(0))
      updateSelectizeInput(session, "comm_f_senders",
                           choices = sort(unique(sender_nodes)),
                           selected = character(0))
      updateSelectizeInput(session, "comm_f_receivers",
                           choices = sort(unique(receiver_nodes)),
                           selected = character(0))
    }

    # ── Import + validation (orchestration uniquement) ─────────────────────
    observeEvent(input$comm_import, {
      req(global_data$sc_obj)
      tryCatch({
        src <- input$comm_source %||% "cellchat_engine"
        if (identical(src, "cellchat_engine") || identical(src, "liana_engine")) {
           .import_ui_stop(
             "invalid_schema",
             .tr("Les sources natives ne sont pas importables : utilisez « Lancer le calcul CellChat » ou « Lancer le calcul LIANA ».")
           )
        }
        # CHRYSALIS 2E : garde d'entree explicite (message FR dans le canal
        # d'erreur au lieu d'un req() silencieux).
        obj <- assert_seurat(global_data$sc_obj, context = "communication import")

        identity_col <- input$comm_identity_column
        if (is.null(identity_col) || !nzchar(identity_col)) {
           .import_ui_stop(
             "invalid_input",
             "Choisissez d'abord la colonne de metadonnees Seurat decrivant les identites des populations."
           )
        }
        assert_metadata_column(obj, identity_col, context = "communication import")

        warnings_all <- character(0)

        if (identical(src, "cellchat")) {
          req(input$comm_cellchat_file)
          tab <- .read_table_auto(input$comm_cellchat_file$datapath)
          parsed <- parse_cellchat_import(tab, source_file = input$comm_cellchat_file$name)
          files <- list(table = input$comm_cellchat_file$name)
        } else if (identical(src, "cellchat_object")) {
          req(input$comm_cellchat_obj)
          parsed <- parse_cellchat_object(input$comm_cellchat_obj$datapath,
                                          source_file = input$comm_cellchat_obj$name)
          files <- list(object = input$comm_cellchat_obj$name)
        } else if (identical(src, "cellphonedb")) {
          req(input$comm_cpdb_means)
          means <- .read_table_auto(input$comm_cpdb_means$datapath)
          pvals <- NULL
          files <- list(means = input$comm_cpdb_means$name)
          if (!is.null(input$comm_cpdb_pvalues)) {
            pvals <- .read_table_auto(input$comm_cpdb_pvalues$datapath)
            files$pvalues <- input$comm_cpdb_pvalues$name
          }
          parsed <- parse_cellphonedb_import(means, pvals, source_file = input$comm_cpdb_means$name)
        } else if (identical(src, "liana")) {
          # ── LIANA (CCC 7-8 route (b)) : import de RANGS produits hors de
          # l'app. Les deux choix ci-dessous sont EXIGES — ni le mode
          # d'agregation ni la colonne de rang ne sont deductibles du fichier,
          # et un defaut implicite produirait un artefact d'analyse.
          req(input$comm_liana_file)
          if (is.null(input$comm_liana_mode) || !nzchar(input$comm_liana_mode)) {
             .import_ui_stop(
               "invalid_input",
               paste0(
                 "Choisissez le mode d'agregation LIANA (specificite ou magnitude) : ",
                 "les deux repondent a des questions differentes et ne sont pas ",
                 "comparables entre elles."
               )
             )
          }
          rank_col <- input$comm_liana_rank
          if (is.null(rank_col) || !nzchar(rank_col)) {
             .import_ui_stop(
               "invalid_input",
               paste0(
                 "Choisissez la colonne de rang LIANA a importer (mean_rank, ",
                 "aggregate_rank ou {methode}.rank) — la route (b) importe des ",
                 "rangs : sans rang, il n'y a pas de mesure."
               )
             )
          }
          liana_tab <- .read_table_auto(input$comm_liana_file$datapath)
          parsed <- parse_liana_import(
            liana_tab, rank_column = rank_col,
            aggregation_mode = input$comm_liana_mode,
            source_file = input$comm_liana_file$name
          )
          files <- list(table = input$comm_liana_file$name)
        } else {
           .import_ui_stop(
             "invalid_schema",
             sprintf(.tr("Source d'import inconnue : %s."), as.character(src)[1L])
           )
        }
        warnings_all <- c(warnings_all, parsed$warnings)

        # Harmonisation des identites : vecteur d'identites extrait de la
        # colonne EXPLICITEMENT choisie (exact match, jamais de renommage).
        identities <- unique(as.character(obj@meta.data[[identity_col]]))
        harm <- harmonize_communication_identities(
          parsed$table, identities, identity_col, context = "communication import"
        )
        warnings_all <- c(warnings_all, harm$warnings)

        # QC d'import : comptages avant/apres, duplications, auto-interactions.
        qcr <- communication_import_qc(harm$table)
        warnings_all <- c(warnings_all, qcr$warnings)

        if (nrow(qcr$table) == 0L) {
           .import_ui_stop(
             "invalid_input",
             "Toutes les lignes ont ete supprimees au QC (sender/receiver/ligand/receptor vides) : aucun resultat canonique produit."
           )
        }

        # Resultat canonique — contrat documente dans
        # docs/contracts/COMMUNICATION_RESULT_CONTRACT.md. Provenance PRODUITE
        # ici puis appendee a l'etat partage (regle 7 AGENTS.md).
        canonical <- finalize_communication_result(
          canonical_table = qcr$table,
          source_method   = unique(qcr$table$source_method)[1L],
          source_files    = files,
          identity_column = identity_col,
          identity_mapping = harm$mapping,
          identity_summary = harm$summary,
          column_mapping  = parsed$column_mapping,
          qc              = qcr$counts,
          n_input_rows    = parsed$n_input_rows,
          seurat_obj      = obj,
          extra_warnings  = warnings_all,
          # Route LIANA : la table porte un agregat inter-methodes calcule par
          # LIANA (mean_rank/aggregate_rank). L'app n'agrege rien — elle MARQUE
          # (NULL pour les autres sources -> FALSE).
          external_consensus = isTRUE(parsed$external_consensus),
          analysis_id     = "sc-communication-import"
        )
        .store_result(canonical, obj)

        msg <- sprintf(
          paste0("Import OK : %d lignes canoniques (%d lignes source). ",
                 "Source : %s. Colonne d'identites : %s. ",
                 "Labels harmonises : %d/%d."),
          canonical$input_summary$n_rows_canonical,
          canonical$input_summary$n_rows_input,
          unique(qcr$table$source_method)[1L], identity_col,
          harm$summary$n_labels_matched, harm$summary$n_labels
        )
        if (harm$summary$n_labels_unmatched > 0L) {
          msg <- paste0(msg, "\n", sprintf(
            "%d label(s) sans correspondance (conserves tels quels) : %s",
            harm$summary$n_labels_unmatched,
            paste(utils::head(harm$summary$unmatched_labels, 5), collapse = ", ")
          ))
        }
        comm_status_rv(paste0("[", .status_label(canonical$status), "]\n", msg))
        showNotification(.tr("Import communication valide."), type = "message", duration = 4)

       }, error = function(e) {
         .clear_result_state()
         st <- communication_error_state(e)
         detail <- if (is.na(st)) conditionMessage(e) else
           paste0(conditionMessage(e), " [etat : ", st, "]")
         comm_status_rv(paste(.tr("Erreur import communication :"), detail))
         showNotification(
           paste(.tr("Erreur import communication :"), detail),
           type = "error", duration = 10
         )
       })
    })

    # ── CC-5 : calcul CellChat DANS l'application (Path B) ──────────────────
    # Meme depot, memes vues, memes exports que l'import : le resultat du
    # moteur EST un resultat canonique (regle des deux voies). Aucune vue ni
    # aucun export n'a eu a changer — c'est la preuve que la forme est bonne.
    observeEvent(input$comm_compute, {
      req(global_data$sc_obj)
      tryCatch({
        obj <- assert_seurat(global_data$sc_obj, context = "communication engine")

        identity_col <- input$comm_identity_column
        if (is.null(identity_col) || !nzchar(identity_col)) {
          stop(errorCondition(
            "Choisissez d'abord la colonne de metadonnees Seurat decrivant les identites des populations.",
            class = c("cellchat_engine_error", "error", "condition"),
            state = "invalid_input"
          ))
        }
        if (!identity_col %in% colnames(obj@meta.data)) {
          stop(errorCondition(
            sprintf("Colonne d'identites absente : %s.", identity_col),
            class = c("cellchat_engine_error", "error", "condition"),
            state = "invalid_input"
          ))
        }

        species <- input$comm_engine_species
        if (is.null(species) || !nzchar(species)) {
          stop(errorCondition(
            paste0("Choisissez l'espece : la base ligand-recepteur en ",
                   "decoule et ne se devine pas a partir des donnees."),
            class = c("cellchat_engine_error", "error", "condition"),
            state = "invalid_input"
          ))
        }

        # Dependance PARESSeUSE : l'absence de CellChat est un etat PREVU. On
        # garde la main sur le texte (avec le remede) plutot que de laisser
        # l'erreur du moteur arriver telle quelle dans la notification.
        if (!cellchat_engine_available()) {
          stop(errorCondition(
            paste0(
              "Le package 'CellChat' n'est pas installe : le calcul dans ",
              "l'application est impossible. Il est distribue uniquement sur ",
              "GitHub — remotes::install_github(\"jinworks/CellChat\"). L'import ",
              "de resultats externes reste disponible sans cette dependance."
            ),
            class = c("cellchat_engine_error", "error", "condition"),
            state = "missing_dependency"
          ))
        }

        cc_input <- build_cellchat_input(obj, group_by = identity_col,
                                         species = species)

        result <- withProgress(message = .tr("Calcul CellChat en cours..."),
                               value = 0, {
          run_cellchat(cc_input,
                       seed  = as.integer(input$comm_engine_seed),
                       nboot = as.integer(input$comm_engine_nboot),
                       seurat_obj = obj,
                       on_progress = function(msg) incProgress(0, detail = msg))
        })

        .store_result(result, obj)

        eng <- result$engine
        msg <- sprintf(
          paste0("Calcul OK : %s interactions, %s populations, %s voies ",
                 "significatives. Moteur %s %s — base %s (%s) — graine %s, ",
                 "%s permutations."),
          format(eng$n_interactions), format(eng$n_populations),
          format(eng$n_pathways_significant), eng$engine, eng$engine_version,
          eng$database, eng$database_version, format(eng$seed), format(eng$nboot)
        )
        comm_status_rv(paste0("[", .status_label(result$status), "]\n", msg))
        showNotification(.tr("Calcul CellChat termine."), type = "message", duration = 4)

      }, error = function(e) {
        .clear_result_state()
        # Les erreurs du moteur sont CLASSEES (6 etats) : on affiche l'etat EN
        # PLUS du texte — « no_interactions » ou « invalid_parameters » ne se
        # devinent pas dans un message generique.
        st <- cellchat_engine_error_state(e)
        detail <- if (is.na(st)) conditionMessage(e) else
          paste0(conditionMessage(e), " [etat : ", st, "]")
        comm_status_rv(paste(.tr("Erreur calcul communication :"), detail))
        showNotification(paste(.tr("Erreur calcul communication :"), detail),
                         type = "error", duration = 10)
      })
    })

    observeEvent(input$comm_compute_liana, {
      req(global_data$sc_obj)
      tryCatch({
        obj <- assert_seurat(global_data$sc_obj, context = "moteur LIANA")

        identity_col <- input$comm_identity_column
        if (is.null(identity_col) || !nzchar(identity_col)) {
          .liana_ui_stop(
            "invalid_input",
            .tr("Choisissez d'abord la colonne de metadonnees Seurat decrivant les identites des populations.")
          )
        }
        if (!identity_col %in% colnames(obj@meta.data)) {
          .liana_ui_stop("invalid_input", sprintf("Colonne d'identites absente : %s.", identity_col))
        }

        sample_col <- input$comm_liana_sample_col
        if (is.null(sample_col) || !nzchar(sample_col)) {
          .liana_ui_stop("invalid_input", .tr("Choisissez la colonne d'echantillon LIANA."))
        }
        if (!sample_col %in% colnames(obj@meta.data)) {
          .liana_ui_stop("invalid_input", sprintf("Colonne d'echantillon absente : %s.", sample_col))
        }

        condition_col <- input$comm_liana_condition_col
        if (is.null(condition_col) || !nzchar(condition_col)) {
          .liana_ui_stop("invalid_input", .tr("Choisissez la colonne de condition LIANA."))
        }
        if (!condition_col %in% colnames(obj@meta.data)) {
          .liana_ui_stop("invalid_input", sprintf("Colonne de condition absente : %s.", condition_col))
        }

        if (!liana_engine_available()) {
          stop(errorCondition(
            .tr("Le moteur LIANA natif n'est pas disponible : installez les paquets requis."),
            class = c("liana_engine_error", "error", "condition"),
            state = "missing_dependency"
          ))
        }

        method <- input$comm_liana_method %||% TS_LIANA_METHOD_DEFAULT
        resource <- input$comm_liana_resource %||% TS_LIANA_RESOURCE_DEFAULT
        collection <- withProgress(
          message = .tr("Calcul LIANA par echantillon en cours..."),
          value = 0,
          {
            run_liana_by_sample(
              obj,
              sample_col = sample_col,
              condition_col = condition_col,
              idents_col = identity_col,
              method = method,
              resource = resource,
              seed = as.integer(input$comm_liana_seed),
              min_cells = as.integer(input$comm_liana_min_cells),
              on_progress = function(msg) incProgress(0, detail = msg)
            )
          }
        )

        manifest <- liana_collection_sample_manifest(collection)
        keys <- as.character(manifest$sample_key)
        if (!length(keys)) {
          .liana_ui_stop("no_interactions", .tr("La collection LIANA ne contient aucun echantillon."))
        }
        first_key <- keys[[1L]]
        first_result <- liana_collection_active(collection, first_key)
        .store_result(first_result, obj, collection = collection,
                      active_sample = first_key)
        if (length(keys) > 1L) {
          for (key in keys[-1L]) {
            sample_result <- liana_collection_active(collection, key)
            provenance_append(shared_rv, sample_result$provenance)
          }
        }
        provenance_append(shared_rv, collection$provenance)

        updateSelectInput(session, "comm_active_sample", choices = keys,
                          selected = first_key)
        condition_summary <- liana_collection_condition_summary(collection)
        msg <- .t_fmt(
          .tr("Collection LIANA : statut {status} — {n} echantillon(s), {c} condition(s)."),
          status = as.character(collection$status %||% NA_character_),
          n = length(keys), c = nrow(condition_summary)
        )
        comm_status_rv(paste0("[", as.character(collection$status %||% NA_character_), "]\n", msg))
        showNotification(.tr("Calcul LIANA termine."), type = "message", duration = 4)
      }, error = function(e) {
        .clear_result_state()
        st <- liana_engine_error_state(e)
        detail <- if (is.na(st)) conditionMessage(e) else
          paste0(conditionMessage(e), " [etat : ", st, "]")
        comm_status_rv(paste(.tr("Erreur calcul LIANA :"), detail))
        showNotification(paste(.tr("Erreur calcul LIANA :"), detail),
                         type = "error", duration = 10)
      })
    })

    observeEvent(input$comm_active_sample, {
      req(global_data$sc_obj, comm_state$collection, input$comm_active_sample)
      key <- as.character(input$comm_active_sample)
      tryCatch({
        keys <- as.character(liana_collection_sample_manifest(comm_state$collection)$sample_key)
        if (!key %in% keys) {
          stop(.tr("Echantillon actif absent de la collection LIANA."), call. = FALSE)
        }
        active_result <- liana_collection_active(comm_state$collection, key)
        .store_result(active_result, global_data$sc_obj,
                      append_provenance = FALSE,
                      collection = comm_state$collection,
                      active_sample = key)
        comm_status_rv(.t_fmt(.tr("Echantillon actif : {name}"), name = key))
      }, error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 8)
      })
    }, ignoreInit = TRUE)

    # ── Vues (consommatrices pures du resultat canonique) ──────────────────
    output$comm_table <- DT::renderDataTable({
      req(comm_state$result)
      .check_fingerprint()
      ts_datatable(
        comm_state$result$canonical_table,
        page_length = 15L,
        filename_base = "sc_comm_canonical_table"
      )
    })

    output$comm_table_filtered <- DT::renderDataTable({
      req(comm_state$result)
      .check_fingerprint()
      ts_datatable(
        comm_filtered()$table,
        page_length = 15L,
        filename_base = "sc_comm_filtered_table",
        filter = "none"
      )
    })

    output$comm_dotplot <- renderPlot({
      req(comm_state$result)
      .check_fingerprint()
      tryCatch(
        plot_communication_dotplot(comm_state$result, comm_filtered()$table,
                                   seurat_obj = .view_object()),
        error = function(e) .error_plot(e)
      )
    })

    output$comm_heatmap <- renderPlot({
      req(comm_state$result)
      .check_fingerprint()
      tryCatch(
        plot_communication_pathway_heatmap(comm_state$result, comm_filtered()$table,
                                           seurat_obj = .view_object()),
        error = function(e) .error_plot(e)
      )
    })

    output$comm_circle <- renderPlot({
      req(comm_state$result)
      .check_fingerprint()
      tryCatch(
        plot_communication_circle(comm_state$result, comm_filtered()$table,
                                  seurat_obj = .view_object()),
        error = function(e) .error_plot(e)
      )
    })

    output$comm_centrality <- DT::renderDataTable({
      req(comm_state$result)
      .check_fingerprint()
      ts_datatable(
        build_communication_centrality(comm_state$result, comm_filtered()$table),
        page_length = 15L,
        filename_base = "sc_comm_centrality",
        filter = "none"
      )
    })

    output$comm_schema <- DT::renderDataTable({
      req(comm_state$result)
      cm <- comm_state$result$column_mapping %||% list()
      df <- data.frame(
        champ_canonique = names(cm),
        colonne_source = vapply(cm, paste, character(1), collapse = " / "),
        stringsAsFactors = FALSE
      )
      ts_datatable(df, page_length = 15L, filename_base = "sc_comm_column_mapping",
                   filter = "none", scroll_x = FALSE)
    })

    output$comm_schema_note <- renderText({
      req(comm_state$result)
      .tr("Mapping resolu de facon deterministe a l'import — aucune conversion supposee entre schemas incompatibles.")
    })

    output$comm_mapping <- DT::renderDataTable({
      req(comm_state$result)
      req(comm_state$result$identity_mapping)
      ts_datatable(
        comm_state$result$identity_mapping,
        page_length = 15L,
        filename_base = "sc_comm_identity_mapping"
      )
    })

    output$comm_qc <- renderUI({
      req(comm_state$result)
      r <- comm_state$result
      qc <- r$qc %||% list()
      ids <- r$identity_summary %||% list()

      .row <- function(k, v) tags$tr(tags$th(style = "width:65%;", k), tags$td(v))
      rows <- list(
        .row(.tr("Lignes source (avant conversion)"), as.character(r$input_summary$n_rows_input)),
        .row(.tr("Lignes canoniques (apres QC)"), as.character(r$input_summary$n_rows_canonical)),
        .row(.tr("Lignes supprimees (champs requis vides)"), as.character(qc$n_dropped_required_fields %||% 0L)),
        .row(.tr("Auto-interactions (sender = receiver)"), as.character(qc$n_self_interactions %||% 0L)),
        .row(.tr("Interactions dupliquees (conservees et flagees)"), as.character(qc$n_duplicate_interactions %||% 0L)),
        .row(.tr("Pathways manquants"), as.character(qc$n_pathway_missing %||% 0L)),
        .row(.tr("P-values hors [0,1]"), as.character(qc$n_p_value_out_of_range %||% 0L)),
        .row(.tr("Labels sources distincts"), as.character(ids$n_labels %||% NA_integer_)),
        .row(.tr("Labels harmonises (exact match)"), as.character(ids$n_labels_matched %||% NA_integer_)),
        .row(.tr("Labels sans correspondance"), as.character(ids$n_labels_unmatched %||% NA_integer_))
      )
      warn <- r$warnings %||% character(0)
      tagList(
        div(class = "m-3",
            tags$table(class = "table table-sm table-bordered",
                       tags$tbody(lapply(rows, function(rw) rw)))),
        if (length(warn)) div(class = "m-3", h6(.tr("Avertissements produits a l'import")),
                              tags$ul(lapply(warn, function(w) tags$li(w))))
      )
    })

    # ── Exports (traces par analysis_id ; filtres captures en provenance) ──
    .export_figure <- function(plot_fn, file) {
      .check_fingerprint()
      p <- plot_fn()
      ts_export_plot(file, p, width = 8, height = 6.5, dpi = 300)
    }

    # Chaque export filtre PRODUIT une entree de provenance (filtres figes
    # dans les parametres) appended a l'etat partage — jamais reconstruite.
    .append_filter_provenance <- function() {
      fs <- comm_filtered()
      provenance_append(
        shared_rv,
        build_communication_filter_provenance(comm_state$result, fs)
      )
    }

    output$dl_comm_table <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_import_table", "csv"),
      content = function(file) {
        req(comm_state$result)
        .check_fingerprint()
        utils::write.csv(comm_state$result$canonical_table, file, row.names = FALSE)
      }
    )

    output$dl_comm_filtered <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_filtered_table", "csv"),
      content = function(file) {
        req(comm_state$result)
        .check_fingerprint()
        fs <- comm_filtered()
        .append_filter_provenance()
        df <- build_communication_filtered_export(
          comm_state$result, fs$table, fs$description
        )
        utils::write.csv(df, file, row.names = FALSE)
      }
    )

    output$dl_comm_centrality <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_centrality", "csv"),
      content = function(file) {
        req(comm_state$result)
        .check_fingerprint()
        .append_filter_provenance()
        df <- build_communication_centrality(comm_state$result, comm_filtered()$table)
        utils::write.csv(df, file, row.names = FALSE)
      }
    )

    output$dl_comm_dotplot_png <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_dotplot", "png"),
      content = function(file) .export_figure(function() {
        plot_communication_dotplot(comm_state$result, comm_filtered()$table,
                                   seurat_obj = .view_object())
      }, file)
    )
    output$dl_comm_dotplot_pdf <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_dotplot", "pdf"),
      content = function(file) .export_figure(function() {
        plot_communication_dotplot(comm_state$result, comm_filtered()$table,
                                   seurat_obj = .view_object())
      }, file)
    )
    output$dl_comm_heatmap_png <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_pathway_heatmap", "png"),
      content = function(file) .export_figure(function() {
        plot_communication_pathway_heatmap(comm_state$result, comm_filtered()$table,
                                           seurat_obj = .view_object())
      }, file)
    )
    output$dl_comm_heatmap_pdf <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_pathway_heatmap", "pdf"),
      content = function(file) .export_figure(function() {
        plot_communication_pathway_heatmap(comm_state$result, comm_filtered()$table,
                                           seurat_obj = .view_object())
      }, file)
    )
    output$dl_comm_circle_png <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_circle_network", "png"),
      content = function(file) .export_figure(function() {
        plot_communication_circle(comm_state$result, comm_filtered()$table,
                                  seurat_obj = .view_object())
      }, file)
    )
    output$dl_comm_circle_pdf <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_circle_network", "pdf"),
      content = function(file) .export_figure(function() {
        plot_communication_circle(comm_state$result, comm_filtered()$table,
                                  seurat_obj = .view_object())
      }, file)
    )

    output$dl_comm_mapping <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_identity_mapping", "csv"),
      content = function(file) {
        req(comm_state$result)
        .check_fingerprint()
        df <- build_communication_identity_mapping_export(comm_state$result)
        utils::write.csv(df, file, row.names = FALSE)
      }
    )

    output$dl_comm_summary <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_import_summary", "csv"),
      content = function(file) {
        req(comm_state$result)
        .check_fingerprint()
        df <- build_communication_import_summary(comm_state$result)
        utils::write.csv(df, file, row.names = FALSE)
      }
    )

    output$dl_comm_liana_manifest <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "liana_sample_manifest", "csv"),
      content = function(file) {
        req(comm_state$collection)
        .check_fingerprint()
        utils::write.csv(liana_collection_sample_manifest(comm_state$collection),
                         file, row.names = FALSE)
      }
    )

    output$dl_comm_liana_condition_summary <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "liana_condition_summary", "csv"),
      content = function(file) {
        req(comm_state$collection)
        .check_fingerprint()
        utils::write.csv(liana_collection_condition_summary(comm_state$collection),
                         file, row.names = FALSE)
      }
    )

    output$dl_comm_rds <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_result", "rds"),
       content = function(file) {
         req(comm_state$result)
         .check_fingerprint()
         payload <- comm_state$result
         if (!is.null(comm_state$collection)) {
           payload <- list(
             type = "liana_collection",
             collection = comm_state$collection,
             active_sample = comm_state$active_sample,
             active_result = comm_state$result
           )
         }
         saveRDS(payload, file)
       }
     )

    # ── Contextes V1.x (roadmap CCC avancée) : sous-modules montés dans le
    # panneau existant — consommateurs du résultat canonique (comm_state est
    # partagé par référence), aucun recalcul de score, aucun nouveau panneau
    # latéral (friction UX n°2). ────────────────────────────────────────────
    mod_sc_communication_spatial_server("spatial", comm_state, global_data, shared_rv)
    mod_sc_communication_trajectory_server("trajectory", comm_state, global_data, shared_rv)
    mod_sc_communication_velocity_server("velocity_ctx", comm_state, global_data, shared_rv)
    mod_sc_communication_perturbation_server("perturbation", comm_state, global_data, shared_rv)

    # Expose state for tests (optional)
    return(comm_state)
  })
}

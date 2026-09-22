# =============================================================================
# mod_bulk_filter.R  —  Bulk Child 1: Filtering + VST, PCA & Sample-QC
# i18n Phase 3.1 — static labels via i18n$t()/.tr_plain() + server push via update*Input
# =============================================================================
# Entry point of the bulk pipeline: raw counts -> filtered counts ->
# DESeqDataSet(design = ~1) -> VST matrix. Everything downstream (DE, heatmap,
# pathways, report) reads shared_rv$vst_mat / shared_rv$filtered_counts.
#
# Depends on helpers_bulk.R (sourced by global.R, not defined there):
#   filter_bulk_counts(), build_dds(), get_vst_matrix(),
#   plot_bulk_pca(), plot_sample_correlation_heatmap(),
#   bulk_color_scale(), manual_color_picker_ui(), plot_scree_bulk()
#
# State contract (shared_rv):
#   READ  : shared_rv$counts_mapped    — written by mod_bulk_mapping (Step 0,
#                                         optional); used instead of the raw
#                                         import when present (see %||% below)
#   WRITE : shared_rv$filtered_counts  — matrix, post-filter counts. Peut être
#                                         CORRIGÉ des effets lot (STAT-S1,
#                                         ComBat-seq) : dans ce cas c'est la
#                                         matrice corrigée, et dds_blind /
#                                         vst_mat sont reconstruits depuis elle.
#                                         La copie PRISTINE reste locale au
#                                         module (bc_pristine), donc aucune clé
#                                         d'état partagé n'a été ajoutée.
#           shared_rv$dds_blind        — DESeqDataSet (design ~1), exploration only
#           shared_rv$vst_mat          — matrix, VST-transformed counts
#           shared_rv$contrasts        — RESET to list() if user re-filters
#                                         (stale gene set would no longer match
#                                         existing DE results / heatmap)
#           shared_rv$active_contrast  — RESET to NULL alongside contrasts
#           shared_rv$pca_color_by, shared_rv$pca_shape_by
#                                       — mirrored so mod_bulk_report can read
#                                         them without crossing module namespaces
#
# UI split:
#   mod_bulk_filter_ui(id)      -> sidebar accordion body (Step 1 controls)
#   mod_bulk_filter_pca_ui(id)  -> main panel "PCA" tab
#   mod_bulk_filter_batch_ui(id) -> main panel "QC Batch" tab (Bulk V2 M1)
#   mod_bulk_filter_qc_ui(id)   -> main panel "QC Échantillons" tab
# =============================================================================


# ── UI: sidebar controls ──────────────────────────────────────────────────────

mod_bulk_filter_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "display:flex;align-items:center;gap:6px;",
        tags$label(i18n$t("Counts totaux minimum / g\u00e8ne"), class = "control-label", style = "margin-bottom:0;"),
        tooltip(bsicons::bs_icon("info-circle"),
                i18n$t("Un g\u00e8ne doit avoir au moins ce nombre de reads, cumul\u00e9s sur tous les \u00e9chantillons, pour \u00eatre conserv\u00e9. \u00c9limine le bruit de fond sans biaiser l'analyse diff\u00e9rentielle."))),
    numericInput(ns("min_count"), NULL, 10, min = 0, step = 1),
    numericInput(ns("min_samples"), i18n$t("Nb \u00e9chantillons min. au-dessus du seuil"), 1, min = 1, step = 1),
    numericInput(ns("min_count_per_sample"), i18n$t("Seuil par \u00e9chantillon"), 1, min = 0, step = 1),

    helpText(i18n$t("La transformation VST (variance-stabilizing) est utilis\u00e9e pour la PCA et la heatmap. Si < 4 \u00e9chantillons : repli automatique sur log2(counts normalis\u00e9s + 1).")),

    actionButton(ns("run_filter_norm"),
                 tagList(icon("play"), i18n$t("Lancer Filtrage & VST")),
                 class = "btn-danger w-100"),

    div(class = "small text-muted mt-2", textOutput(ns("filter_status"))),

    hr(),
    div(style = "display:flex;align-items:center;gap:6px;",
        tags$label(tagList("\U0001f3a8", i18n$t("Palette de couleurs (PCA)")), class = "control-label", style = "margin-bottom:0;"),
        tooltip(bsicons::bs_icon("info-circle"),
                i18n$t("Couleurs utilis\u00e9es pour les groupes sur la PCA. Okabe-Ito = s\u00fbre pour daltoniens."))),
    selectInput(ns("palette_choice"), NULL,
               choices = stats::setNames(
                 c("default","okabeito","viridis","set2","manual"),
                 c(.tr_plain("D\u00e9faut (ggplot)"), .tr_plain("Okabe-Ito (daltonien)"), "Viridis",
                   .tr_plain("Set2 (ColorBrewer)"), .tr_plain("Manuel (choisir chaque couleur)"))))
  )
}


# ── UI: PCA tab ────────────────────────────────────────────────────────────────

mod_bulk_filter_pca_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header("PCA"),
    fluidRow(
      column(4, selectizeInput(ns("pca_color_by"), i18n$t("Colorer par"), choices = NULL,
                               options = list(placeholder = .tr_placeholder(), allowEmptyOption = TRUE))),
      column(4, selectizeInput(ns("pca_shape_by"), i18n$t("Forme par (optionnel)"), choices = NULL,
                               options = list(placeholder = .tr_placeholder(), allowEmptyOption = TRUE)))
    ),
    uiOutput(ns("manual_palette_ui")),
    checkboxInput(ns("pca_interactive"), i18n$t("Interactif (Plotly \u2014 survol pour identifier l'\u00e9chantillon)"), value = FALSE),
    div(style = "height:620px;overflow-y:auto;", uiOutput(ns("pca_container"))),
    downloadButton(ns("dl_pca_png"), i18n$t("Export PNG (statique)"), class = "btn-sm btn-secondary mt-2"),

    hr(),
    h6(i18n$t("Scree Plot \u2014 Variance Expliqu\u00e9e"), style = "font-weight:bold;"),
    helpText(i18n$t("Combien de composantes principales faut-il regarder ? Une chute nette (\"coude\") indique o\u00f9 le signal biologique s'arr\u00eate et o\u00f9 le bruit commence.")),
    div(style = "height:400px;overflow-y:auto;", plotOutput(ns("plot_scree"), height = "380px")),
    downloadButton(ns("dl_scree_png"), i18n$t("Export PNG"), class = "btn-sm btn-secondary mt-2")
  )
}


# ── UI: QC Échantillons tab ────────────────────────────────────────────────────

mod_bulk_filter_qc_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header(i18n$t("QC \u00c9chantillons")),
    div(class = "alert alert-light", style = "font-size:0.85em;",
        bsicons::bs_icon("info-circle"),
        " ", i18n$t("D\u00e9tecte les \u00e9chantillons mal \u00e9tiquet\u00e9s, les outliers ou doublons inattendus AVANT de lancer l'analyse diff\u00e9rentielle. Des \u00e9chantillons d'un m\u00eame groupe devraient corr\u00e9ler fortement entre eux (cellules sombres group\u00e9es).")),
    fluidRow(
      column(4, selectizeInput(ns("qc_corr_annot"), i18n$t("Annotation"), choices = NULL,
                               options = list(placeholder = .tr_placeholder(), allowEmptyOption = TRUE))),
      column(4, selectInput(ns("qc_corr_method"), i18n$t("M\u00e9thode"),
                            choices = c("Pearson" = "pearson", "Spearman" = "spearman")))
    ),
    uiOutput(ns("qc_manual_palette_ui")),
    div(style = "height:640px;overflow-y:auto;", plotOutput(ns("plot_sample_corr"), height = "620px")),
    downloadButton(ns("dl_sample_corr_png"), i18n$t("Export PNG"), class = "btn-sm btn-secondary mt-2")
  )
}

# tiny helper for selectize placeholders (returns French by default; JS shim
# does not touch placeholders, so keep them neutral/short).
.tr_placeholder <- function() "\u2014"


# ---------------------------------------------------------------------------
# UI: QC Batch tab (Bulk V2 M1, contrat BULK_BATCH_QC_CONTRACT.md)
# Consomme UNIQUEMENT R/bulk/bulk_batch_qc.R (garde anti-counts-bruts,
# design-check, variance partition) sur shared_rv$vst_mat. Aucune validation
# locale : toute garde scientifique vient du contrat.
# ---------------------------------------------------------------------------

mod_bulk_filter_batch_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header(i18n$t("QC Batch")),
    div(class = "alert alert-light", style = "font-size:0.85em;",
        bsicons::bs_icon("info-circle"),
        " ", i18n$t("V\u00e9rifie que l'effet lot (batch) n'est pas confondu avec la condition biologique AVANT l'analyse diff\u00e9rentielle. Matrice exig\u00e9e : VST (produite \u00e0 l'\u00e9tape 1) \u2014 les counts bruts sont refus\u00e9s.")),
    fluidRow(
      column(4, selectizeInput(ns("batch_col"), i18n$t("Colonne batch"), choices = NULL,
                               options = list(placeholder = .tr_placeholder(), allowEmptyOption = TRUE))),
      column(4, selectizeInput(ns("batch_cond_col"), i18n$t("Colonne condition (optionnel)"), choices = NULL,
                               options = list(placeholder = .tr_placeholder(), allowEmptyOption = TRUE))),
      column(4, div(style = "padding-top:32px;",
                    actionButton(ns("run_varpart"),
                                 tagList(icon("play"), i18n$t("D\u00e9composition de variance")),
                                 class = "btn-outline-primary w-100 btn-sm")))
    ),
    uiOutput(ns("batch_alert")),
    h6(i18n$t("Table de contingence (condition x batch)"), style = "font-weight:bold;"),
    tableOutput(ns("batch_crosstab")),

    # ── STAT-S1 — correction de batch ComBat-seq (BATCH_CORRECTION_CONTRACT.md)
    # Section repliable NATIVE (<details>, aucun JS) et ouverte par défaut.
    # Rien ne touche le pipeline tant que l'utilisateur ne clique pas
    # « Appliquer » : l'étage est strictement optionnel (règle dure n°1).
    tags$details(
      open = NA,
      tags$summary(style = "cursor:pointer;font-weight:bold;padding:6px 0;",
                   i18n$t("Correction de batch (optionnel) \u2014 ComBat-seq")),
      div(class = "alert alert-light", style = "font-size:0.85em;",
          bsicons::bs_icon("info-circle"), " ",
          i18n$t("Corrige l'effet lot sur les COUNTS BRUTS (jamais sur la matrice VST), en pr\u00e9servant la condition biologique. Le VST est recalcul\u00e9 apr\u00e8s correction et les contrastes d\u00e9j\u00e0 calcul\u00e9s sont invalid\u00e9s.")),
      uiOutput(ns("batch_correction_status")),
      actionButton(ns("run_batch_correction"),
                   i18n$t("Appliquer la correction de batch"),
                   class = "btn-outline-danger w-100 btn-sm"),
      div(style = "height:520px;overflow-y:auto;margin-top:10px;",
          plotOutput(ns("plot_batch_correction"), height = "500px"))
    ),

    hr(),
    h6(i18n$t("Scree Plot \u2014 Variance Expliqu\u00e9e (QC Batch)"), style = "font-weight:bold;"),
    div(style = "height:400px;overflow-y:auto;", plotOutput(ns("plot_batch_scree"), height = "380px")),
    hr(),
    h6(i18n$t("D\u00e9composition de la variance par facteur"), style = "font-weight:bold;"),
    helpText(i18n$t("Part de variance expliqu\u00e9e par le batch vs la condition, g\u00e8ne par g\u00e8ne (plafond m\u00e9moire d\u00e9terministe, voir contrat).")),
    verbatimTextOutput(ns("varpart_method")),
    div(style = "height:420px;overflow-y:auto;", plotOutput(ns("plot_varpart"), height = "400px")),
    downloadButton(ns("dl_varpart_csv"), i18n$t("Export CSV (fractions)"), class = "btn-sm btn-secondary mt-2")
  )
}


# ── Server ────────────────────────────────────────────────────────────────────

mod_bulk_filter_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # ── i18n: push translated labels/choices on language switch ──────────
    observeEvent(global_data$language, {
      updateNumericInput(session, "min_samples",         label = .tr("Nb \u00e9chantillons min. au-dessus du seuil"))
      updateNumericInput(session, "min_count_per_sample",label = .tr("Seuil par \u00e9chantillon"))
      updateSelectInput(session, "palette_choice",
        choices = stats::setNames(c("default","okabeito","viridis","set2","manual"),
          c(.tr("D\u00e9faut (ggplot)"), .tr("Okabe-Ito (daltonien)"), "Viridis",
            .tr("Set2 (ColorBrewer)"), .tr("Manuel (choisir chaque couleur)"))))
      updateSelectizeInput(session, "pca_color_by",  label = .tr("Colorer par"))
      updateSelectizeInput(session, "pca_shape_by",  label = .tr("Forme par (optionnel)"))
      updateCheckboxInput(session, "pca_interactive", label = .tr("Interactif (Plotly \u2014 survol pour identifier l'\u00e9chantillon)"))
      updateSelectizeInput(session, "qc_corr_annot", label = .tr("Annotation"))
      updateSelectizeInput(session, "batch_col", label = .tr("Colonne batch"))
      updateSelectizeInput(session, "batch_cond_col", label = .tr("Colonne condition (optionnel)"))
      updateSelectInput(session, "qc_corr_method", label = .tr("M\u00e9thode"))
      updateActionButton(session, "run_filter_norm",
                         label = paste0("\U0001f680 ", .tr("Lancer Filtrage & VST")))
      updateActionButton(session, "run_batch_correction",
                         label = .tr("Appliquer la correction de batch"))
    }, ignoreInit = TRUE)

    # ── Refresh metadata-driven choices when bulk_obj changes ────────────────
    observeEvent(global_data$bulk_obj, {
      req(global_data$bulk_obj, global_data$bulk_obj$metadata)
      meta <- global_data$bulk_obj$metadata
      cat_cols <- names(meta)[sapply(meta, function(x) is.character(x) || is.factor(x))]
      cat_cols <- if (length(cat_cols) == 0) names(meta) else cat_cols

      updateSelectizeInput(session, "pca_color_by",  choices = cat_cols, server = FALSE)
      updateSelectizeInput(session, "pca_shape_by",  choices = cat_cols, server = FALSE)
      updateSelectizeInput(session, "qc_corr_annot", choices = cat_cols, server = FALSE)
      updateSelectizeInput(session, "batch_col", choices = cat_cols, server = FALSE)
      updateSelectizeInput(session, "batch_cond_col", choices = cat_cols, server = FALSE)
    }, ignoreNULL = TRUE)

    # ── Mirror PCA inputs + palette choice to shared_rv (read by mod_bulk_report) ─
    observe({
      shared_rv$pca_color_by    <- input$pca_color_by
      shared_rv$pca_shape_by    <- input$pca_shape_by
      shared_rv$bulk_palette    <- input$palette_choice
      shared_rv$pca_manual_colors <- if (identical(input$palette_choice, "manual")) manual_palette_vec() else NULL
    })

    # ── Polish UI: disable the run button until an import actually exists ───
    observe({
      shinyjs::toggleState("run_filter_norm", condition = !is.null(global_data$bulk_obj))
    })

    # =========================================================================
    # STAT-S1 — copie PRISTINE des counts filtrés, gardée CÔTÉ MODULE (aucune
    # clé d'état partagé ajoutée). Elle rend « Appliquer la correction de
    # batch » IDEMPOTENT : re-cliquer re-corrige à partir des counts d'origine,
    # jamais à partir d'une matrice déjà corrigée. Remise à zéro par chaque
    # exécution du Filtrage & VST (nouveau jeu de gènes).
    bc_pristine <- reactiveVal(NULL)

    # ── DRIVE LIVE CONTROL (docs/DRIVE_LIVE_CONTROL_PLAN.md, G3) ─────────────
    # Step 1 is the ONLY producer of `shared_rv$filtered_counts` a scenario can
    # reach, and every downstream panel gates on it — so this bind is what makes
    # DE, pathways and the rest reachable AT ALL. It is a BIND, not a second
    # implementation: the observer below is the existing human path, and the
    # human click is still one of its two triggers.
    #
    # MEASURED, live, before this existed: `run_pipeline` on `bulk_de` answered
    # `invalid — not ready: no bulk object loaded (shared_rv$filtered_counts is
    # NULL)` while `snapshot` answered `has_data=TRUE genes=17925 samples=18` in
    # the SAME session. Both statements are true — `global_data$bulk_obj` is the
    # IMPORTED dataset, `shared_rv$filtered_counts` is this stage's OUTPUT, and
    # nothing could fill the second one.
    #
    # READINESS GUARD. The observer opens with `req(global_data$bulk_obj)`.
    # Without a guard the poller would fire the token, that `req()` would abort
    # in SILENCE, and `result.json` would report `done` for a filtering that
    # never ran. The guard mirrors the precondition measured from the line
    # below — it is not guessed.
    #
    # STATE PROBE. An agent must be able to tell "Step 1 ran" from "Step 1 did
    # not", and `global_data$bulk_obj` cannot tell it: it reports the IMPORTED
    # dimensions either way (17925 x 18 before AND after). So the module
    # publishes what only IT can see. `shiny::isolate()` keeps these reads out
    # of the poller's dependency set (reactivity is the module's business, C2).
    # Dimensions and sample names only — never the matrices themselves.
    drive_counter <- shiny::reactiveVal(0L)
    drive_ready <- function() {
      if (is.null(shiny::isolate(global_data$bulk_obj))) {
        "no bulk object loaded (global_data$bulk_obj is NULL)"
      } else TRUE
    }
    drive_state <- function() {
      .dims <- function(m) {
        if (is.null(m)) return(NULL)
        list(n_genes = nrow(m), n_samples = ncol(m), samples = colnames(m))
      }
      list(
        filtered_counts = .dims(shiny::isolate(shared_rv$filtered_counts)),
        vst_mat         = .dims(shiny::isolate(shared_rv$vst_mat))
      )
    }
    ts_drive_publish_token(global_data, "bulk-filter-run_filter_norm",
                           drive_counter, ready = drive_ready, state = drive_state)
    drive_trigger <- shiny::reactive(list(drive_counter(), input$run_filter_norm))

    # STEP 1 — Filtering + VST
    # =========================================================================
    observeEvent(drive_trigger(), {
      # A `reactive` trigger fires once at init with both values at 0. Without
      # this line the observer would run its full body on a session where
      # nothing was asked of it — the `req()` below happens to abort today, but
      # that is the SAME silent-abort class the readiness guard exists to
      # remove, so the two conditions are stated instead of relied upon.
      req(input$run_filter_norm > 0 || shiny::isolate(drive_counter()) > 0)
      req(global_data$bulk_obj)
      # Picks up the Step-0 gene-ID-mapped matrix if one was applied
      # (mod_bulk_mapping.R), otherwise falls back to the raw import.
      counts <- shared_rv$counts_mapped %||% global_data$bulk_obj$counts
      meta   <- global_data$bulk_obj$metadata

      if (ncol(counts) > 500 || nrow(counts) > 60000) {
        showNotification(
          .tr("Matrice volumineuse \u2014 le filtrage va r\u00e9duire la taille avant VST/PCA."),
          type = "warning", duration = 5
        )
      }

      # SAFETY: re-filtering invalidates any previously computed contrast,
      # since gene sets between vst_mat and contrasts would otherwise diverge
      # (this caused a heatmap crash when filtering was re-run after a DE pass).
      if (length(shared_rv$contrasts) > 0) {
        showNotification(
          .tr("Les contrastes calcul\u00e9s pr\u00e9c\u00e9demment seront invalid\u00e9s par ce nouveau filtrage."),
          type = "warning", duration = 6
        )
        shared_rv$contrasts       <- list()
        shared_rv$active_contrast <- NULL
      }

      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message = .tr("Filtrage & VST..."), value = 0.2)

      tryCatch({
        filtered <- filter_bulk_counts(
          counts, min_count = input$min_count, min_samples = input$min_samples,
          min_count_per_sample = input$min_count_per_sample
        )
        p$set(0.5, .tr("Construction DESeqDataSet (design ~1)..."))
        dds_blind <- build_dds(filtered, meta, design_formula = "~1", run_deseq = FALSE)
        dds_blind <- DESeq2::estimateSizeFactors(dds_blind)

        p$set(0.8, .tr("Transformation VST..."))
        vst_mat <- get_vst_matrix(dds_blind)

        shared_rv$filtered_counts <- filtered
        shared_rv$dds_blind       <- dds_blind
        shared_rv$vst_mat         <- vst_mat
        # STAT-S1 : nouvelle référence pristine. Toute correction de batch
        # précédente devient caduque (elle portait sur un autre jeu de gènes).
        bc_pristine(list(counts = filtered, vst = vst_mat))

        showNotification(.t_fmt(.tr("\u2713 {n} g\u00e8nes conserv\u00e9s sur {m} \u00e9chantillons"),
                                n = nrow(filtered), m = ncol(filtered)),
                         type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste(.tr("Erreur filtrage/VST:"), e$message), type = "error", duration = 8)
      })
    })

    output$filter_status <- renderText({
      global_data$language
      if (is.null(shared_rv$filtered_counts)) .tr("En attente du filtrage...")
      else .t_fmt(.tr("\u2713 {n} g\u00e8nes \u00d7 {m} \u00e9chantillons"),
                  n = nrow(shared_rv$filtered_counts), m = ncol(shared_rv$filtered_counts))
    })

    # =========================================================================
    # PCA
    # =========================================================================
    # ── Manual palette: dynamic color pickers for the active PCA grouping ───
    # Re-evaluates whenever pca_color_by changes (new set of levels) — the
    # picker UI itself is rebuilt by output$manual_palette_ui below.
    manual_pca_levels <- reactive({
      req(global_data$bulk_obj$metadata, input$pca_color_by)
      req(nzchar(input$pca_color_by))
      lvls <- sort(unique(stats::na.omit(as.character(global_data$bulk_obj$metadata[[input$pca_color_by]]))))
      req(length(lvls) > 0)
      lvls
    })

    output$manual_palette_ui <- renderUI({
      global_data$language  # i18n
      if (!identical(input$palette_choice, "manual")) return(NULL)
      if (!nzchar(input$pca_color_by %||% "")) {
        return(div(class = "alert alert-warning", style = "font-size:0.8em;",
                   .tr("S\u00e9lectionnez d'abord une variable \"Colorer par\" pour personnaliser ses couleurs.")))
      }
      lvls <- tryCatch(manual_pca_levels(), error = function(e) character(0))
      if (length(lvls) == 0) return(NULL)
      ids <- paste0("manual_color_", seq_along(lvls))
      div(
        class = "border rounded p-2 mb-2", style = "background:#f8f9fa;",
        h6(.t_fmt(.tr("Couleurs manuelles \u2014 {var}"), var = input$pca_color_by),
           style = "font-size:0.85em;font-weight:bold;margin-bottom:6px;"),
        manual_color_picker_ui(ns, ids, lvls, .default_manual_colors(length(lvls)))
      )
    })

    manual_palette_vec <- reactive({
      if (!identical(input$palette_choice, "manual")) return(NULL)
      lvls <- tryCatch(manual_pca_levels(), error = function(e) character(0))
      if (length(lvls) == 0) return(NULL)
      defaults <- .default_manual_colors(length(lvls))
      vals <- vapply(seq_along(lvls), function(i) {
        v <- input[[paste0("manual_color_", i)]]
        if (is.null(v) || !nzchar(v)) defaults[i] else v
      }, character(1))
      setNames(vals, lvls)
    })

    pca_plot <- reactive({
      global_data$language                     # i18n trigger
      req(shared_rv$vst_mat)
      pal <- input$palette_choice %||% "default"
      plot_bulk_pca(shared_rv$vst_mat, global_data$bulk_obj$metadata,
                    color_by = if (nzchar(input$pca_color_by %||% "")) input$pca_color_by else NULL,
                    shape_by = if (nzchar(input$pca_shape_by %||% "")) input$pca_shape_by else NULL,
                    palette  = pal,
                    manual_colors = if (identical(pal, "manual")) manual_palette_vec() else NULL,
                    tr = .tr_fn(global_data))
    })
    output$plot_pca <- renderPlot({
      .safe_plot_render(session, "plot_pca", function() pca_plot())
    })

    output$pca_container <- renderUI({
      if (isTRUE(input$pca_interactive)) plotlyOutput(ns("plot_pca_ly"), height = "620px")
      else plotOutput(ns("plot_pca"), height = "620px")
    })

    output$plot_pca_ly <- renderPlotly({
      req(pca_plot())
      tryCatch(
        suppressWarnings(ggplotly(pca_plot(), tooltip = c("x", "y", "colour", "shape", "label"))),
        error = function(e) plotly_empty()
      )
    })

    output$dl_pca_png <- downloadHandler(
      filename = function() paste0("pca_bulk_", Sys.Date(), ".png"),
      content  = function(file) ts_export_plot(file, pca_plot(), width = 8, height = 6, dpi = 300)
    )

    # =========================================================================
    # SCREE PLOT — PCA companion, reuses shared_rv$vst_mat (no extra heavy
    # computation; same ntop=500 variable-gene selection as plot_bulk_pca()).
    # =========================================================================
    scree_plot <- reactive({
      global_data$language                     # i18n trigger
      req(shared_rv$vst_mat)
      plot_scree_bulk(shared_rv$vst_mat, tr = .tr_fn(global_data))
    })
    output$plot_scree <- renderPlot({
      .safe_plot_render(session, "plot_scree", function() scree_plot())
    })
    output$dl_scree_png <- downloadHandler(
      filename = function() paste0("scree_plot_bulk_", Sys.Date(), ".png"),
      content  = function(file) ts_export_plot(file, scree_plot(), width = 7, height = 5, dpi = 300)
    )

    # =========================================================================
    # QC: Sample correlation heatmap (reuses shared_rv$vst_mat — zero extra
    # heavy computation)
    # =========================================================================
    # Manual palette: own picker, keyed to qc_corr_annot's levels — kept
    # SEPARATE from the PCA picker above because qc_corr_annot may point to
    # a different metadata column (e.g. "batch" for QC vs "treatment" for PCA).
    manual_qc_levels <- reactive({
      req(global_data$bulk_obj$metadata, input$qc_corr_annot)
      req(nzchar(input$qc_corr_annot))
      lvls <- sort(unique(stats::na.omit(as.character(global_data$bulk_obj$metadata[[input$qc_corr_annot]]))))
      req(length(lvls) > 0)
      lvls
    })

    output$qc_manual_palette_ui <- renderUI({
      global_data$language  # i18n
      if (!identical(input$palette_choice, "manual")) return(NULL)
      if (!nzchar(input$qc_corr_annot %||% "")) {
        return(div(class = "alert alert-warning", style = "font-size:0.8em;",
                   .tr("S\u00e9lectionnez d'abord une \"Annotation\" pour personnaliser ses couleurs.")))
      }
      lvls <- tryCatch(manual_qc_levels(), error = function(e) character(0))
      if (length(lvls) == 0) return(NULL)
      ids <- paste0("qc_manual_color_", seq_along(lvls))
      div(
        class = "border rounded p-2 mb-2", style = "background:#f8f9fa;",
        h6(.t_fmt(.tr("Couleurs manuelles \u2014 {var}"), var = input$qc_corr_annot),
           style = "font-size:0.85em;font-weight:bold;margin-bottom:6px;"),
        manual_color_picker_ui(ns, ids, lvls, .default_manual_colors(length(lvls)))
      )
    })

    qc_manual_colors <- reactive({
      if (!identical(input$palette_choice, "manual")) return(NULL)
      lvls <- tryCatch(manual_qc_levels(), error = function(e) character(0))
      if (length(lvls) == 0) return(NULL)
      defaults <- .default_manual_colors(length(lvls))
      vals <- vapply(seq_along(lvls), function(i) {
        v <- input[[paste0("qc_manual_color_", i)]]
        if (is.null(v) || !nzchar(v)) defaults[i] else v
      }, character(1))
      setNames(vals, lvls)
    })

    sample_corr_plot_fn <- function() {
      global_data$language                     # i18n trigger
      req(shared_rv$vst_mat)
      annot <- if (nzchar(input$qc_corr_annot %||% "")) input$qc_corr_annot else NULL
      pal   <- input$palette_choice %||% "default"
      plot_sample_correlation_heatmap(
        shared_rv$vst_mat, global_data$bulk_obj$metadata,
        annotation_col = annot, method = input$qc_corr_method %||% "pearson",
        palette = pal, manual_colors = if (identical(pal, "manual")) qc_manual_colors() else NULL,
        tr = .tr_fn(global_data))
    }
    output$plot_sample_corr <- renderPlot({
      req(shared_rv$vst_mat)
      .safe_plot_render(session, "plot_sample_corr", function() print(sample_corr_plot_fn()))
    })
    output$dl_sample_corr_png <- downloadHandler(
      filename = function() paste0("sample_correlation_qc_", Sys.Date(), ".png"),
      content = function(file) {
        png(file, width = 9, height = 8, units = "in", res = 300)
        print(sample_corr_plot_fn())
        dev.off()
      }
    )

    # =========================================================================
    # QC BATCH (Bulk V2 M1 — contrat BULK_BATCH_QC_CONTRACT.md)
    # Design-check (alerte collinearite) + scree + variance partition, le tout
    # sur shared_rv$vst_mat. Aucune validation locale : les gardes viennent
    # de R/bulk/bulk_batch_qc.R (erreurs classees bulk_batch_qc_error).
    # =========================================================================
    batch_design_check <- reactive({
      req(global_data$bulk_obj, global_data$bulk_obj$metadata)
      req(nzchar(input$batch_col %||% ""))
      cond <- if (nzchar(input$batch_cond_col %||% "")) input$batch_cond_col else NULL
      tryCatch(
        bulk_batch_design_check(global_data$bulk_obj$metadata, input$batch_col, cond),
        error = function(e) e
      )
    })

    output$batch_alert <- renderUI({
      global_data$language  # i18n
      if (is.null(shared_rv$vst_mat)) {
        return(div(class = "alert alert-warning", style = "font-size:0.85em;",
                   .tr("Lancez d'abord le Filtrage & VST (\u00e9tape 1) pour activer les diagnostics batch.")))
      }
      chk <- tryCatch(batch_design_check(), error = function(e) e)
      if (inherits(chk, "error")) {
        return(div(class = "alert alert-warning", style = "font-size:0.85em;",
                   paste0("\u26a0\ufe0f ", conditionMessage(chk))))
      }
      if (isTRUE(chk$fully_collinear)) {
        div(class = "alert alert-danger", style = "font-size:0.85em;",
            tags$strong(.tr("Batch et condition enti\u00e8rement collin\u00e9aires : ")),
            chk$warning_messages[1])
      } else if (length(chk$warning_messages) > 0) {
        div(class = "alert alert-warning", style = "font-size:0.85em;",
            paste0("\u26a0\ufe0f ", paste(chk$warning_messages, collapse = " ")))
      } else {
        div(class = "alert alert-success", style = "font-size:0.85em;",
            .tr("\u2713 Plan \u00e9quilibr\u00e9 : l'effet batch peut \u00eatre ajust\u00e9 dans le design."))
      }
    })

    output$batch_crosstab <- renderTable({
      chk <- batch_design_check()
      if (inherits(chk, "error")) return(NULL)
      as.data.frame.matrix(chk$cross_table)
    }, rownames = TRUE)

    batch_scree_plot <- reactive({
      global_data$language                     # i18n trigger
      req(shared_rv$vst_mat)
      # Garde contrat : refuse les counts bruts (defense-in-depth — la VST
      # produit un continu, ce stop ne doit jamais se declencher en pratique).
      bulk_assert_transformed_matrix(shared_rv$vst_mat, "diagnostics batch")
      plot_bulk_batch_scree(shared_rv$vst_mat, tr = .tr_fn(global_data))
    })
    output$plot_batch_scree <- renderPlot({
      .safe_plot_render(session, "plot_batch_scree", function() batch_scree_plot())
    })

    # eventReactive (pas observeEvent) : aucun nouveau trigger dupliqué.
    varpart_res <- eventReactive(input$run_varpart, {
      req(shared_rv$vst_mat)
      req(nzchar(input$batch_col %||% ""))
      meta <- global_data$bulk_obj$metadata
      cond <- if (nzchar(input$batch_cond_col %||% "")) input$batch_cond_col else NULL
      covs <- c(input$batch_col, cond)
      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message = .tr("D\u00e9composition de variance..."), value = 0.3)
      tryCatch({
        bulk_variance_partition(shared_rv$vst_mat, meta, covs)
      }, error = function(e) {
        showNotification(paste(.tr("Erreur variance partition :"), conditionMessage(e)),
                         type = "error", duration = 8)
        NULL
      })
    })

    output$varpart_method <- renderText({
      global_data$language  # i18n
      vp <- varpart_res()
      if (is.null(vp)) {
        .tr("Cliquez sur D\u00e9composition de variance pour lancer le calcul.")
      } else {
        paste0("M\u00e9thode : ", vp$method, " — ",
               vp$n_genes_used, " g\u00e8nes analys\u00e9s")
      }
    })
    output$plot_varpart <- renderPlot({
      req(varpart_res())
      .safe_plot_render(session, "plot_varpart", function() {
        global_data$language
        plot_bulk_varpart(varpart_res(), tr = .tr_fn(global_data))
      })
    })
    output$dl_varpart_csv <- downloadHandler(
      filename = function() paste0("variance_partition_batch_", Sys.Date(), ".csv"),
      content = function(file) {
        vp <- varpart_res()
        req(vp)
        out <- cbind(gene = rownames(vp$var_part), vp$var_part)
        utils::write.csv(out, file, row.names = FALSE)
      }
    )

    # =========================================================================
    # STAT-S1 — CORRECTION DE BATCH (ComBat-seq)
    # Contrat : docs/contracts/BATCH_CORRECTION_CONTRACT.md
    # Consomme R/bulk/batch_correction.R (pur). Aucune validation locale : les
    # gardes viennent du contrat (erreurs classees bulk_batch_correction_error).
    # L'etage est OPTIONNEL : rien ne change tant que l'utilisateur ne clique
    # pas « Appliquer ».
    # =========================================================================
    batch_correction_design <- reactive({
      req(global_data$bulk_obj, global_data$bulk_obj$metadata)
      req(nzchar(input$batch_col %||% ""))
      cond <- if (nzchar(input$batch_cond_col %||% "")) input$batch_cond_col else NULL
      tryCatch(
        bulk_batch_correction_design(global_data$bulk_obj$metadata, input$batch_col, cond),
        error = function(e) e
      )
    })

    output$batch_correction_status <- renderUI({
      global_data$language  # i18n
      if (is.null(bc_pristine())) {
        return(div(class = "alert alert-warning", style = "font-size:0.85em;",
                   .tr("Lancez d'abord le Filtrage & VST (\u00e9tape 1) pour activer la correction de batch.")))
      }
      d <- tryCatch(batch_correction_design(), error = function(e) e)
      if (inherits(d, "error")) {
        return(div(class = "alert alert-warning", style = "font-size:0.85em;",
                   paste0("\u26a0\ufe0f ", conditionMessage(d))))
      }
      if (!isTRUE(d$can_apply)) {
        return(div(class = "alert alert-danger", style = "font-size:0.85em;",
                   tags$strong(.tr("Correction impossible : ")),
                   paste(d$blocking_messages, collapse = " ")))
      }
      div(class = "alert alert-success", style = "font-size:0.85em;",
          .tr("\u2713 Correction possible."), " ",
          if (isTRUE(d$use_group))
            .tr("La condition d\u00e9clar\u00e9e sera pass\u00e9e en « groupe » et pr\u00e9serv\u00e9e par la correction.")
          else
            .tr("Aucune condition d\u00e9clar\u00e9e : la correction sera appliqu\u00e9e sans variable biologique \u00e0 pr\u00e9server."))
    })

    # eventReactive : aucun nouveau trigger dupliqué (même choix que varpart).
    batch_correction_res <- eventReactive(input$run_batch_correction, {
      pristine <- bc_pristine()
      req(pristine)
      d <- batch_correction_design()
      req(isTRUE(d$can_apply))

      # SAFETY (même règle que le re-filtrage) : la correction modifie la
      # matrice effective -> tout contraste déjà calculé devient incohérent.
      if (length(shared_rv$contrasts) > 0) {
        showNotification(
          .tr("Les contrastes calcul\u00e9s pr\u00e9c\u00e9demment seront invalid\u00e9s par la correction de batch."),
          type = "warning", duration = 6)
        shared_rv$contrasts       <- list()
        shared_rv$active_contrast <- NULL
      }

      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message = .tr("Correction de batch (ComBat-seq)..."), value = 0.2)

      tryCatch({
        meta      <- global_data$bulk_obj$metadata
        batch_vec <- as.character(meta[[d$batch_col]])
        group_vec <- if (isTRUE(d$use_group)) as.character(meta[[d$condition_col]]) else NULL

        corrected <- run_combat_seq(pristine$counts, batch_vec, group = group_vec)

        p$set(0.6, .tr("Reconstruction DESeqDataSet (design ~1)..."))
        dds <- build_dds(corrected, meta, design_formula = "~1", run_deseq = FALSE)
        dds <- DESeq2::estimateSizeFactors(dds)

        p$set(0.8, .tr("Transformation VST..."))
        vst_corrected <- get_vst_matrix(dds)

        shared_rv$filtered_counts <- corrected
        shared_rv$dds_blind       <- dds
        shared_rv$vst_mat         <- vst_corrected

        # Provenance (AGENTS.md règle 6) : PRODUITE ici, jamais reconstruite
        # après coup. Le libellé PRÉSERVE la normalisation déjà déclarée (il la
        # préfixe) ; l'historique de provenance enregistre la transition.
        global_data$bulk_obj <- bulk_update_provenance(
          global_data$bulk_obj,
          normalization = bulk_batch_correction_label(
            d$batch_col,
            if (isTRUE(d$use_group)) d$condition_col else NULL,
            global_data$bulk_obj$provenance$normalization))

        showNotification(
          .t_fmt(.tr("\u2713 Correction de batch appliqu\u00e9e \u2014 {n} g\u00e8nes \u00d7 {m} \u00e9chantillons"),
                 n = nrow(corrected), m = ncol(corrected)),
          type = "message", duration = 6)

        list(before_vst = pristine$vst, after_vst = vst_corrected)
      }, error = function(e) {
        showNotification(paste(.tr("Erreur correction de batch :"), conditionMessage(e)),
                         type = "error", duration = 8)
        NULL
      })
    })

    # Diagnostic avant / après : deux appels à plot_bulk_pca() (API existante,
    # aucune logique de tracé dupliquée) composés par plot_batch_correction_pca().
    output$plot_batch_correction <- renderPlot({
      res <- batch_correction_res()
      req(res)
      .safe_plot_render(session, "plot_batch_correction", function() {
        global_data$language
        meta <- global_data$bulk_obj$metadata
        pal  <- input$palette_choice %||% "default"
        man  <- if (identical(pal, "manual")) manual_palette_vec() else NULL
        before <- plot_bulk_pca(res$before_vst, meta, color_by = input$batch_col,
                                palette = pal, manual_colors = man,
                                tr = .tr_fn(global_data))
        after  <- plot_bulk_pca(res$after_vst, meta, color_by = input$batch_col,
                                palette = pal, manual_colors = man,
                                tr = .tr_fn(global_data))
        plot_batch_correction_pca(before, after, tr = .tr_fn(global_data))
      })
    })

  }) # /moduleServer
}

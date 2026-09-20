# =============================================================================
# mod_sc_viz.R  —  Child 3: Visualisation (11 types) + Export + Palette SC
# =============================================================================
# Changes vs prev version:
#   [1] sc_discrete_scale() / sc_continuous_scale() — mirror Bulk palette helpers
#   [2] build_sc_viz_plot() — UNIFIED builder: single truth for live + report export
#       (removes duplication that existed between renderPlotly inline code and the
#       previous session's separate helpers_sc.R version)
#   [3] Palette UI: selectInput "Défaut / Okabe-Ito / Viridis / Set2 / Manuel"
#       + dynamic manual colour pickers per group_by level (mirrors Bulk pattern)
#   [4] i18n$t("📌 Ajouter au Rapport") → shared_rv$report_viz_list (consumed by mod_sc.R)
#   [5] Volcano pickers: fixed stale isolate bug (new reactive on list(sc_obj, group_by))
#
# Step-3.7 changes:
#   [6] sc_discrete_colors() — NEW: returns a concrete hex color VECTOR (not a
#       ggplot scale layer) for a given palette/levels. Needed because DoHeatmap()
#       takes group.colors= directly rather than accepting `+ scale_color_manual(...)`
#       like every other Seurat plot function used here — this is why the manual/
#       Okabe-Ito/Set2/Viridis palette was silently ignored on the Heatmap's group
#       color bar while the expression gradient (fill) was fine.
#   [7] .current_cfg() now also captures sc_palette/sc_manual_colors, and the SC
#       report (sc_report_template.Rmd) reads them back instead of hardcoding
#       "default" — so i18n$t("📌 Ajouter au Rapport") plots match the live palette both
#       on screen AND in the exported HTML/PDF.
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1L && is.na(a)) ||
                              identical(a, "")) b else a

# ── Colour palette helpers ────────────────────────────────────────────────────
# REMOVED (palette generalisation) : sc_discrete_scale / sc_continuous_scale /
# sc_diverging_scale / sc_discrete_colors / .sc_okabe vivaient en DOUBLE ici et
# dans R/palettes.R. R/palettes.R est source APRES ce fichier par app.R, donc
# sa copie ecrasait silencieusement celle-ci a chaque lancement -- une seule
# source de verite desormais (voir R/palettes.R), avec en plus
# diverging_ramp_colors()/expression_continuous_scale()/bulk_role_colors()
# partages SC/Bulk/Spatial.



# ── UI: sidebar controls ──────────────────────────────────────────────────────

mod_sc_viz_ui <- function(id) {
  ns <- NS(id)
  tagList(

    div(class = "alert alert-light", style = "font-size:0.78em;border-left:3px solid #2C3E50;",
        bsicons::bs_icon("diagram-3"), tags$strong(" Bloc 1 — Exploration Core"),
        i18n$t(" : réductions, expression, profils, heatmaps, densité 2D, 3D.")),

    # Step 5 (UI reorg, Phase 1): grouped <optgroup> choices instead of a flat
    # list -- purely presentational (native <select> optgroup rendering),
    # input$viz_type keeps its EXACT contract (same values, same server-side
    # switches downstream) -- zero reactivity change.
    selectInput(
      ns("viz_type"), i18n$t("Style de Visualisation"),
      choices = setNames(
        list(
          setNames(c("dim"), .tr_plain("Réduction Dimensionnelle (UMAP/PCA/t-SNE)")),
          setNames(c("feature", "dot"), c(.tr_plain("Feature Plot (Expression sur Réduction)"), .tr_plain("DotPlot"))),
          setNames(c("violin", "stacked_violin", "ridge", "scatter", "correlation_matrix", "multi_sample", "volcano"),
                   c(.tr_plain("Distribution (Violin)"), .tr_plain("Stacked Violin Plot"), .tr_plain("Densité (Ridge Plot)"),
                     .tr_plain("Corrélation Gènes (Scatter Amélioré)"), .tr_plain("Matrice Corrélation"),
                     .tr_plain("Comparaison Multi-Échantillons"), .tr_plain("Volcano Plot"))),
          setNames(c("heatmap", "heatmap_hier"), c(.tr_plain("Heatmap (DoHeatmap)"), .tr_plain("Heatmap Hiérarchique (ComplexHeatmap)"))),
          setNames(c("density_2d"), .tr_plain("Densité d'Expression 2D")),
          setNames(c("reduction_3d"), .tr_plain("Réduction 3D (interactif)"))
        ),
        c(.tr_plain("Réductions"), .tr_plain("Expression"), .tr_plain("Profils"), .tr_plain("Heatmap"), .tr_plain("Densité 2D"), .tr_plain("Visualisation 3D"))
      )
    ),

    # Conditional: DimPlot reduction picker
    conditionalPanel(
      condition = "input.viz_type == 'dim'", ns = ns,
      selectInput(ns("viz_reduction"), i18n$t("Reduction a Visualiser"),
                  choices  = c("UMAP"="umap","PCA"="pca","t-SNE"="tsne","Diffusion Map"="dm"),
                  selected = "umap")
    ),

    # Conditional: Scatter gene X / Y
    conditionalPanel(
      condition = "input.viz_type == 'scatter'", ns = ns,
      div(style="border-bottom:1px solid #ddd;padding-bottom:10px;margin-bottom:10px;",
          h6(i18n$t("Gene / Feature X"), style="font-weight:bold;color:#2C3E50;"),
          selectizeInput(ns("scatter_gene1"), NULL, choices = NULL, multiple = FALSE,
                         options = list(placeholder = "Ex: CD4"))),
      div(style="border-bottom:1px solid #ddd;padding-bottom:10px;margin-bottom:10px;",
          h6(i18n$t("Gene / Feature Y"), style="font-weight:bold;color:#2C3E50;"),
          selectizeInput(ns("scatter_gene2"), NULL, choices = NULL, multiple = FALSE,
                         options = list(placeholder = "Ex: CD8A"))),
      div(style="background:#f8f9fa;padding:10px;border-radius:5px;",
          radioButtons(ns("scatter_cor_method"), .tr("Méthode"),
                       choices = c("Pearson"="pearson","Spearman"="spearman"),
                       selected = "pearson", inline = TRUE),
          checkboxInput(ns("scatter_smooth"), i18n$t("Ligne de tendance"), value = TRUE))
    ),

    # Conditional: Multi-sample
    conditionalPanel(
      condition = "input.viz_type == 'multi_sample'", ns = ns,
      selectizeInput(ns("multi_gene"), i18n$t("Gene a Comparer"), choices = NULL, multiple = FALSE),
      radioButtons(ns("multi_plot_type"), i18n$t("Type"),
                   choices = c("Violin"="violin","Box"="box","Jitter"="jitter"),
                   inline = TRUE)
    ),

    # Group-by
    div(style="display:flex;align-items:center;justify-content:space-between;",
        tags$label(i18n$t("Grouper/Colorer par:"), class="control-label"),
        tooltip(bsicons::bs_icon("info-circle"), i18n$t("Variable de métadonnées pour groupement"))),
    selectInput(ns("group_by"), NULL, choices = NULL),

    # Gene basket (hidden for scatter / multi_sample / volcano)
    conditionalPanel(
      condition = "input.viz_type != 'scatter' && input.viz_type != 'multi_sample' && input.viz_type != 'volcano' && input.viz_type != 'density_2d' && input.viz_type != 'reduction_3d'",
      ns = ns,
      div(
        class = "border-bottom pb-2 mb-2",
        selectizeInput(ns("feat_sel"), i18n$t("Genes a Visualiser"),
                       choices = NULL, multiple = TRUE,
                       options = list(maxOptions=5000, placeholder="Ex: CD4, PTPRC", maxItems=10)),
        div(class="small text-muted mb-2", textOutput(ns("gene_selection_status"))),
        actionButton(ns("clear_viz_genes"), i18n$t("Vider"), class="btn-outline-danger btn-sm w-100")
      )
    ),

    # Violin: boxplot overlay
    conditionalPanel(
      condition = "input.viz_type == 'violin'", ns = ns,
      checkboxInput(ns("violin_boxplot"), i18n$t("Superposer Boxplot"), value = FALSE)
    ),

    # Volcano settings
    conditionalPanel(
      condition = "input.viz_type == 'volcano'", ns = ns,
      div(style="background:#f8f9fa;padding:10px;border-radius:5px;margin-bottom:10px;",
          h6(i18n$t("Groupes de Comparaison"), style="font-weight:bold;"),
          selectInput(ns("volcano_group1"), i18n$t("Group 1 (Test)"),      choices = NULL),
          selectInput(ns("volcano_group2"), i18n$t("Group 2 (Reference)"), choices = c("All other cells"="rest")),
          helpText("Compare Group 1 vs Group 2 (ou 'rest' pour vs tous les autres)")),
      fluidRow(
        column(6, numericInput(ns("volcano_logfc"), i18n$t("Log2FC Threshold"), value=0.25, step=0.05)),
        column(6, numericInput(ns("volcano_pval"),  i18n$t("P-adj Threshold"),  value=0.05, step=0.001))
      ),
      checkboxInput(ns("volcano_show_labels"), i18n$t("Afficher labels gènes sig."), value=TRUE),
      actionButton(ns("volcano_add_sig"), i18n$t("-> Add Sig. Genes to Viz"),
                   class="btn-sm btn-success w-100")
    ),

    conditionalPanel(
      condition = "input.viz_type == 'heatmap_hier'", ns = ns,
      div(style="background:#f8f9fa;padding:10px;border-radius:5px;margin-bottom:10px;",
          h6(i18n$t("Heatmap Hierarchique"), style="font-weight:bold;"),
          helpText("Genes du panier ci-dessus (max 50). Clustering hierarchique lignes/colonnes."),
          numericInput(ns("hier_max_cells"), i18n$t("Max cellules avant agregation par groupe"),
                       value = 5000, min = 200, max = 20000, step = 500),
          # PLOT-S4 — memes reglages de clustering que cote Bulk DE.
          fluidRow(
            column(6, selectInput(ns("hier_clust_distance"), i18n$t("Distance de clustering"),
                                  choices = stats::setNames(TS_HEATMAP_DISTANCES, TS_HEATMAP_DISTANCES),
                                  selected = "euclidean")),
            column(6, selectInput(ns("hier_clust_method"), i18n$t("Méthode de clustering"),
                                  choices = stats::setNames(TS_HEATMAP_METHODS, TS_HEATMAP_METHODS),
                                  selected = "complete"))
          ),
          numericInput(ns("hier_k_row"), i18n$t("Découpage en k groupes de lignes (0 = aucun)"),
                       value = 0, min = 0, max = 20, step = 1))
    ),

    conditionalPanel(
      condition = "input.viz_type == 'density_2d'", ns = ns,
      div(style="background:#f8f9fa;padding:10px;border-radius:5px;margin-bottom:10px;",
          h6(i18n$t("Densite d'Expression 2D"), style="font-weight:bold;"),
          selectizeInput(ns("density_gene"), i18n$t("Gene"), choices = NULL, multiple = FALSE,
                         options = list(placeholder = "Ex: CD3D")),
          selectInput(ns("density_reduction"), i18n$t("Reduction (2D)"),
                      choices = c("UMAP"="umap","PCA"="pca","t-SNE"="tsne"), selected = "umap"),
          numericInput(ns("density_max_cells"), i18n$t("Max cellules (estimation densite)"),
                       value = 50000, min = 1000, max = 200000, step = 1000),
          div(class="text-muted", style="font-size:0.72em;",
              i18n$t("Visualisation descriptive -- ne constitue pas une inference de lignage.")))
    ),

    conditionalPanel(
      condition = "input.viz_type == 'reduction_3d'", ns = ns,
      div(style="background:#f8f9fa;padding:10px;border-radius:5px;margin-bottom:10px;",
          h6(i18n$t("Reduction 3D"), style="font-weight:bold;"),
          selectInput(ns("reduction_3d_pick"), i18n$t("Reduction (>= 3 dimensions)"), choices = NULL),
          numericInput(ns("reduction_3d_max_cells"), i18n$t("Max cellules affichees"),
                       value = 50000, min = 1000, max = 200000, step = 1000),
          div(class="text-muted", style="font-size:0.72em;",
              "Utilise 'Grouper/Colorer par' ci-dessous. Si vide, choisissez PCA (UMAP/t-SNE sont 2D par defaut)."))
    ),

    sliderInput(ns("pt_size"), i18n$t("Taille points"), 0.1, 3, 0.5, 0.1),

    hr(),

    # ── Palette ─────────────────────────────────────────────────────────────
    div(style="display:flex;align-items:center;gap:6px;",
         tags$label(i18n$t("🎨 Palette couleur"), class="control-label", style="margin-bottom:0;"),
         tooltip(bsicons::bs_icon("info-circle"),
                 i18n$t("Appliqué aux groupes/clusters colorés. Okabe-Ito = sûre pour daltoniens."))),
    selectInput(ns("sc_palette"), NULL,
                choices = setNames(
                  c("default", "okabeito", "viridis", "set2", "manual"),
                  c(.tr_plain("Défaut (Seurat/ggplot)"), .tr_plain("Okabe-Ito (daltonien)"), "Viridis", "Set2 (ColorBrewer)", .tr_plain("Manuel"))
                )
    ), # <-- FIX syntax error (closing parenthesis was missing)
    uiOutput(ns("sc_manual_palette_ui")),
    uiOutput(ns("sc_manual_gradient_ui")),
    uiOutput(ns("sc_manual_volcano_ui")),
    
    hr(),
    div(class = "alert alert-light", style = "font-size:0.72em;opacity:0.55;",
        bsicons::bs_icon("hourglass-split"), tags$strong(" Bloc 2 — Régulation & Fonction"),
        i18n$t(" (à venir — régulons, cartes d'enrichissement).")),
    # Bloc 3 : fonctionnalites livrees (panels 8 a 8f + 9b) — carte "disponible"
    # (plus un placeholder "a venir") ; decision UX 2026-09-03, mission 4F.
    div(class = "alert alert-light", style = "font-size:0.72em;margin-bottom:0;",
        bsicons::bs_icon("check-circle-fill"), tags$strong(" Bloc 3 — Dynamique & Écosystème"),
        i18n$t(" Disponible : vélocité ARN (8), communication (8b), Milo (8d), scCODA (8e), vues croisées (8f) — rapport consolidé (9b)."))
    
    
  )
}


# ── UI: output panel ──────────────────────────────────────────────────────────

mod_sc_viz_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    max_height  = "750px",
    div(
      class = "card-header bg-light",
      div(
        style = "display:flex;justify-content:space-between;align-items:center;",
        h5(i18n$t("Visualisation Interactive"), class="card-title mb-0"),
        div(
          # "Add to report" basket button
          actionButton(ns("add_to_report"), i18n$t("📌 Ajouter au Rapport"),
                       class="btn-sm btn-outline-dark me-1"),
          # Export popover
          popover(
            trigger = actionButton(ns("plot_settings"), i18n$t("Personnaliser"),
                                   class="btn-sm btn-outline-primary"),
            title = "Export",
            selectInput(ns("plot_theme"), i18n$t("Thème"),
                        choices = stats::setNames(
                          TS_THEME_CHOICES,
                          c(.tr_plain("Minimal"), .tr_plain("Classique"),
                            .tr_plain("BW"), .tr_plain("Vide")))),
            numericInput(ns("plot_width"),  i18n$t("Largeur"), 800, min=400, max=2000),
            numericInput(ns("plot_height"), i18n$t("Hauteur"), 600, min=300, max=1500),
            selectInput(ns("export_format"), i18n$t("Format"),
                        choices = ts_export_format_choices_ui()),
            uiOutput(ns("export_fidelity_note")),
            downloadButton(ns("export_plot"), i18n$t("Exporter"))
          )
        )
      )
    ),
    uiOutput(ns("preview_badge")),
    uiOutput(ns("viz_category_badge")),
    div(style="height:650px;overflow:auto;",
        uiOutput(ns("plot_container")))
  )
}


# ── Server ────────────────────────────────────────────────────────────────────

mod_sc_viz_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    # ── i18n proxy ──────────────────────────────────────────────────────────
    .tr <- function(key) {
      tr <- isolate(global_data$i18n)
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    observeEvent(global_data$language, {
      updateSelectInput(session, "viz_type",
        label = .tr("Style de Visualisation"),
        choices = setNames(
          list(
            setNames(c("dim"), .tr("Réduction Dimensionnelle (UMAP/PCA/t-SNE)")),
            setNames(c("feature", "dot"), c(.tr("Feature Plot (Expression sur Réduction)"), .tr("DotPlot"))),
            setNames(c("violin", "stacked_violin", "ridge", "scatter", "correlation_matrix", "multi_sample", "volcano"),
                     c(.tr("Distribution (Violin)"), .tr("Stacked Violin Plot"), .tr("Densité (Ridge Plot)"),
                       .tr("Corrélation Gènes (Scatter Amélioré)"), .tr("Matrice Corrélation"),
                       .tr("Comparaison Multi-Échantillons"), .tr("Volcano Plot"))),
            setNames(c("heatmap", "heatmap_hier"), c(.tr("Heatmap (DoHeatmap)"), .tr("Heatmap Hiérarchique (ComplexHeatmap)"))),
            setNames(c("density_2d"), .tr("Densité d'Expression 2D")),
            setNames(c("reduction_3d"), .tr("Réduction 3D (interactif)"))
          ),
          c(.tr("Réductions"), .tr("Expression"), .tr("Profils"), .tr("Heatmap"), .tr("Densité 2D"), .tr("Visualisation 3D"))
        ),
        selected = isolate(input$viz_type) %||% "dim"
      )
      updateSelectInput(session, "sc_palette",
        label = NULL,
        choices = setNames(
          c("default", "okabeito", "viridis", "set2", "manual"),
          c(.tr("Défaut (Seurat/ggplot)"), .tr("Okabe-Ito (daltonien)"), "Viridis", "Set2 (ColorBrewer)", .tr("Manuel"))
        ),
        selected = isolate(input$sc_palette) %||% "default"
      )
      updateSliderInput(session, "pt_size", label = .tr("Taille points"))
    }, ignoreInit = TRUE)

    ns <- session$ns

    # ── Reactive: ggplot for export ──────────────────────────────────────────
    current_plot <- reactiveVal(NULL)

    # ── Step-3.8B (section E): live preview vs high-fidelity export ─────────
    # Per-cell plot types (dim/feature/scatter/violin) get expensive on very
    # large objects -- not RAM (Seurat/ggplot don't densify), but disk I/O:
    # FetchData() on a BPCells-backed 1.3M-cell object streams from disk for
    # every single render, and the resulting SVG/plotly point count also
    # chokes the browser (see raster=TRUE fix in build_sc_viz_plot() above).
    # Live preview renders on a capped, cluster-stratified subsample (reusing
    # subsample_seurat_for_analysis(), already used for markers/correlation
    # RAM-safety); export re-renders build_sc_viz_plot() on the FULL object
    # on demand so the exported file is always full-fidelity regardless of
    # what the live preview showed.
    .PREVIEW_MAX_CELLS  <- 50000L
    .PREVIEW_CELL_TYPES <- c("dim", "feature", "scatter", "violin")
    preview_subsampled  <- reactiveVal(FALSE)

    .preview_obj <- function(obj, type, grp_col) {
      if (!type %in% .PREVIEW_CELL_TYPES || ncol(obj) <= .PREVIEW_MAX_CELLS) {
        preview_subsampled(FALSE)
        return(obj)
      }
      max_per_grp <- max(500L, round(.PREVIEW_MAX_CELLS / max(1L, length(unique(obj@meta.data[[grp_col]] %||% "all")))))
      sub <- tryCatch(
        subsample_seurat_for_analysis(obj, max_per_group = max_per_grp, group_col = grp_col),
        error = function(e) list(object = obj, was_subsampled = FALSE)
      )
      preview_subsampled(isTRUE(sub$was_subsampled))
      sub$object
    }

    output$preview_badge <- renderUI({
      if (!isTRUE(preview_subsampled())) return(NULL)
      div(class="alert alert-info py-1 px-2 mb-1", style="font-size:0.78em;",
          bsicons::bs_icon("info-circle"),
          sprintf(" Aper\u00e7u sous-\u00e9chantillonn\u00e9 (max %s cellules) pour la fluidit\u00e9 \u2014 l'export utilise le dataset complet.",
                 format(.PREVIEW_MAX_CELLS, big.mark=" ")))
    })

    # Step 5 (UI reorg): maps the flat viz_type value back to its conceptual
    # group for display -- purely informational, no new reactive contract.
    output$viz_category_badge <- renderUI({
      req(input$viz_type)
      # Make badge reactive to language switch
      global_data$language
      .viz_category_map <- c(
        dim = .tr("Réductions"), reduction_3d = .tr("Visualisation 3D"),
        feature = .tr("Expression"), dot = .tr("Expression"),
        violin = .tr("Profils"), stacked_violin = .tr("Profils"), ridge = .tr("Profils"),
        scatter = .tr("Profils"), correlation_matrix = .tr("Profils"),
        multi_sample = .tr("Profils"), volcano = .tr("Profils"),
        heatmap = .tr("Heatmap"), heatmap_hier = .tr("Heatmap"),
        density_2d = .tr("Densité 2D")
      )
      cat_label <- .viz_category_map[[input$viz_type]] %||% "Exploration"
      div(class = "text-muted", style = "font-size:0.75em;margin-bottom:4px;",
          bsicons::bs_icon("diagram-3"),
          sprintf(" Bloc 1 — Exploration Core / %s", cat_label))
    })

    # ── Helper: config snapshot (used ONLY for report basket — NOT for renders
    #    to avoid retriggerring expensive computations like FindMarkers when
    #    unrelated inputs change) ─────────────────────────────────────────────
    # Step-3.7: now also captures sc_palette/sc_manual_colors so the exported
    # report reproduces the exact palette the user configured live (BUG2 fix).
    .current_cfg <- function() {
      list(
        type               = input$viz_type,
        reduction          = input$viz_reduction,
        feat_sel           = input$feat_sel,
        group_by           = input$group_by,
        scatter_gene1      = input$scatter_gene1,
        scatter_gene2      = input$scatter_gene2,
        scatter_cor_method = input$scatter_cor_method,
        scatter_smooth     = input$scatter_smooth,
        multi_gene         = input$multi_gene,
        multi_plot_type    = input$multi_plot_type,
        violin_boxplot     = input$violin_boxplot,
        volcano_group1     = input$volcano_group1,
        volcano_group2     = input$volcano_group2,
        volcano_logfc      = input$volcano_logfc,
        volcano_pval       = input$volcano_pval,
        volcano_show_labels= input$volcano_show_labels,
        pt_size            = input$pt_size,
        plot_theme         = input$plot_theme,
        sc_palette         = input$sc_palette,
        sc_manual_colors   = if (identical(input$sc_palette, "manual")) sc_manual_colors_vec() else NULL,
        sc_gradient        = if (identical(input$sc_palette, "manual")) sc_gradient_vec() else NULL,
        sc_volcano_colors  = if (identical(input$sc_palette, "manual")) sc_volcano_colors_vec() else NULL,
        hier_max_cells         = input$hier_max_cells,
        hier_clust_distance    = input$hier_clust_distance,
        hier_clust_method      = input$hier_clust_method,
        hier_k_row             = input$hier_k_row,
        density_gene           = input$density_gene,
        density_reduction      = input$density_reduction,
        density_max_cells      = input$density_max_cells,
        reduction_3d_pick      = input$reduction_3d_pick,
        reduction_3d_max_cells = input$reduction_3d_max_cells
      )
    }

    # ── Helper: add genes to viz basket ─────────────────────────────────────
    add_genes_to_viz <- function(genes_input) {
      req(global_data$sc_obj)
      clean <- trimws(unlist(strsplit(genes_input, "[, \n\t]+")))
      clean <- clean[nchar(clean) > 0]
      if (!length(clean)) { showNotification("No valid genes provided", type="warning", duration=3); return(invisible(NULL)) }
      valid   <- intersect(clean, rownames(global_data$sc_obj))
      invalid <- setdiff(clean, valid)
      current <- isolate(input$feat_sel) %||% character(0)
      new_sel <- unique(c(current, valid))
      updateSelectizeInput(session, "feat_sel", selected=new_sel,
                           choices=rownames(global_data$sc_obj), server=TRUE)
      state_set(shared_rv, "selected_genes", new_sel)
      if (length(valid) > 0) {
        showNotification(sprintf("Added %d gene(s) (%d invalid skipped)", length(valid), length(invalid)),
                         type="message", duration=5)
        state_set(shared_rv, "active_tab", "tab_viz")
      } else {
        showNotification(paste("No valid genes. Invalid:", paste(invalid, collapse=", ")),
                         type="error", duration=5)
      }
      invisible(valid)
    }

    # ── Palette: group levels for manual picker ──────────────────────────────
    sc_group_levels <- reactive({
      req(global_data$sc_obj, input$group_by)
      meta <- global_data$sc_obj@meta.data
      if (!input$group_by %in% names(meta)) return(character(0))
      sort(unique(na.omit(as.character(meta[[input$group_by]]))))
    })

    output$sc_manual_palette_ui <- renderUI({
      if (!identical(input$sc_palette, "manual")) return(NULL)
      lvls <- tryCatch(sc_group_levels(), error = function(e) character(0))
      if (!length(lvls)) {
        return(div(class="alert alert-warning", style="font-size:0.8em;",
                   .tr("Sélectionnez une variable 'Grouper par' pour personnaliser les couleurs.")))
      }
      ids <- paste0("sc_color_", seq_along(lvls))
      div(
        class = "border rounded p-2 mb-2", style = "background:#f8f9fa;",
        h6(.t_fmt(.tr("Couleurs manuelles — {var}"), var = input$group_by),
           style = "font-size:0.85em;font-weight:bold;margin-bottom:6px;"),
        manual_color_picker_ui(ns, ids, lvls, .default_manual_colors(length(lvls)))
      )
    })

    sc_manual_colors_vec <- reactive({
      if (!identical(input$sc_palette, "manual")) return(NULL)
      lvls <- tryCatch(sc_group_levels(), error = function(e) character(0))
      if (!length(lvls)) return(NULL)
      defaults <- .default_manual_colors(length(lvls))
      vals <- vapply(seq_along(lvls), function(i) {
        v <- input[[paste0("sc_color_", i)]]
        if (is.null(v) || !nzchar(v)) defaults[i] else v
      }, character(1))
      setNames(vals, lvls)
    })

    # ── Step-3.7 (restored) — continuous gradient picker: sequential for
    #    Feature/DotPlot, diverging for Heatmap/Correlation Matrix. Only
    #    shown when palette=="manual" AND the current viz_type actually uses
    #    a continuous color scale. ─────────────────────────────────────────
    output$sc_manual_gradient_ui <- renderUI({
      if (!identical(input$sc_palette, "manual")) return(NULL)
      type <- input$viz_type
      if (!isTRUE(type %in% c("feature", "dot", "heatmap", "correlation_matrix", "heatmap_hier"))) return(NULL)
      if (type %in% c("feature", "dot")) {
        ids      <- c("sc_grad_low", "sc_grad_high")
        labels   <- c("Bas (expression min)", "Haut (expression max)")
        defaults <- c("#2166AC", "#B2182B")
      } else {
        ids      <- c("sc_grad_low", "sc_grad_mid", "sc_grad_high")
        labels   <- c("Bas (-1)", "Milieu (0)", "Haut (+1)")
        defaults <- c("#2166AC", "#FFFFFF", "#B2182B")
      }
      div(
        class = "border rounded p-2 mb-2", style = "background:#f8f9fa;",
        h6(.tr("Dégradé manuel (expression)"), style = "font-size:0.85em;font-weight:bold;margin-bottom:6px;"),
        manual_color_picker_ui(ns, ids, labels, defaults)
      )
    })

    sc_gradient_vec <- reactive({
      if (!identical(input$sc_palette, "manual")) return(NULL)
      type <- input$viz_type
      if (type %in% c("feature", "dot")) {
        list(low  = input$sc_grad_low  %||% "#2166AC",
             high = input$sc_grad_high %||% "#B2182B")
      } else if (type %in% c("heatmap", "correlation_matrix", "heatmap_hier")) {
        list(low  = input$sc_grad_low  %||% "#2166AC",
             mid  = input$sc_grad_mid  %||% "#FFFFFF",
             high = input$sc_grad_high %||% "#B2182B")
      } else NULL
    })

    # ── Volcano Up/Down/NS manual colors — same rationale, separate from the
    #    group-level discrete picker (Volcano's "groups" are a fixed 3-level
    #    status field, not the group_by metadata column). ───────────────────
    output$sc_manual_volcano_ui <- renderUI({
      if (!identical(input$sc_palette, "manual") || !identical(input$viz_type, "volcano")) return(NULL)
      ids      <- c("sc_volc_up", "sc_volc_down", "sc_volc_ns")
      labels   <- c("Up (significatif +)", "Down (significatif -)", "NS (non significatif)")
      defaults <- c("#E74C3C", "#2980B9", "#BDC3C7")
      div(
        class = "border rounded p-2 mb-2", style = "background:#f8f9fa;",
        h6(.tr("Couleurs manuelles — Volcano"), style = "font-size:0.85em;font-weight:bold;margin-bottom:6px;"),
        manual_color_picker_ui(ns, ids, labels, defaults)
      )
    })

    sc_volcano_colors_vec <- reactive({
      if (!identical(input$sc_palette, "manual")) return(NULL)
      c(up   = input$sc_volc_up   %||% "#E74C3C",
        down = input$sc_volc_down %||% "#2980B9",
        ns   = input$sc_volc_ns   %||% "#BDC3C7")
    })

    # ── Palette PARTAGEE SC (miroir vers shared_rv, meme patron que Spatial
    # vague 6) : Annotation/Trajectoire/Pathways/rapport lisent shared_rv
    # au lieu de recalculer leur propre palette locale. ─────────────────
    observe({
      state_set(shared_rv, "sc_palette", input$sc_palette %||% "default")
      state_set(shared_rv, "sc_manual_colors", if (identical(input$sc_palette, "manual")) sc_manual_colors_vec() else NULL)
      state_set(shared_rv, "sc_manual_gradient", if (identical(input$sc_palette, "manual")) sc_gradient_vec() else NULL)
      state_set(shared_rv, "sc_manual_volcano_colors", if (identical(input$sc_palette, "manual")) sc_volcano_colors_vec() else NULL)
    })

    # ── Sync gene basket from sibling modules ────────────────────────────────
    observeEvent(state_get(shared_rv, "selected_genes"), {
      req(global_data$sc_obj)
      genes <- state_get(shared_rv, "selected_genes")
      if (length(genes) > 0)
        updateSelectizeInput(session, "feat_sel", selected=genes,
                             choices=rownames(global_data$sc_obj), server=TRUE)
    }, ignoreInit = TRUE)

    # ── Refresh UI choices on new sc_obj ────────────────────────────────────
    .safe_setnames <- function(values, labels) {
      labels <- as.character(labels); values <- as.character(values)
      n <- length(values)
      if (length(labels) != n) labels <- labels[seq_len(n)]
      setNames(values, labels)
    }

    observeEvent(global_data$sc_obj, {
      req(global_data$sc_obj)
      obj  <- global_data$sc_obj
      meta <- obj@meta.data

      # Group-by
      valid_cols <- names(meta)[vapply(meta, function(x) is.factor(x)||is.character(x), logical(1))]
      valid_cols <- unique(c("seurat_clusters","orig.ident", valid_cols))
      valid_cols <- valid_cols[valid_cols %in% names(meta)]
      cur_grp    <- isolate(input$group_by)
      sel_grp    <- if (!is.null(cur_grp) && cur_grp %in% valid_cols) cur_grp else "seurat_clusters"
      updateSelectInput(session, "group_by", choices=valid_cols, selected=sel_grp)

      # Gene pickers
      var_features <- tryCatch(VariableFeatures(obj), error=function(e) character(0))
      gene_choices <- c(var_features, setdiff(rownames(obj), var_features))
      updateSelectizeInput(session, "feat_sel",      choices=gene_choices, server=TRUE)
      updateSelectizeInput(session, "scatter_gene1", choices=rownames(obj), server=TRUE)
      updateSelectizeInput(session, "scatter_gene2", choices=rownames(obj), server=TRUE)
      updateSelectizeInput(session, "multi_gene",    choices=gene_choices,  server=TRUE)

      # Reduction picker
      avail_red  <- names(obj@reductions)
      pref_red   <- c("umap","umap_harmony","pca","tsne","dm")
      red_choices <- avail_red[avail_red %in% pref_red]
      if (!length(red_choices)) red_choices <- avail_red
      updateSelectInput(session, "viz_reduction",
                        choices  = red_choices,
                        selected = if ("umap" %in% red_choices) "umap" else red_choices[1])

      updateSelectizeInput(session, "density_gene", choices = gene_choices, server = TRUE)

      red_3d <- avail_red[vapply(avail_red, function(r) {
        tryCatch(ncol(Seurat::Embeddings(obj, r)) >= 3L, error = function(e) FALSE)
      }, logical(1))]
      updateSelectInput(session, "reduction_3d_pick", choices = red_3d,
                        selected = if ("pca" %in% red_3d) "pca" else if (length(red_3d)) red_3d[1] else character(0))
    }, ignoreInit = TRUE)

    # ── FIX: Volcano group pickers — separate reactive on BOTH sc_obj AND
    #    group_by. The previous version used isolate(input$group_by) inside
    #    the sc_obj-only observer, which read a stale value (the picker was
    #    never properly populated on first load or after group_by changes). ──
    observeEvent(list(global_data$sc_obj, input$group_by), {
      req(global_data$sc_obj, input$group_by)
      meta    <- global_data$sc_obj@meta.data
      grp_col <- input$group_by
      if (!grp_col %in% names(meta)) return(invisible(NULL))

      idents_levels <- tryCatch({
        lvls <- levels(factor(as.character(meta[[grp_col]])))
        lvls[nchar(lvls) > 0]
      }, error = function(e) character(0))

      if (length(idents_levels) < 2) return(invisible(NULL))

      cur_g1 <- isolate(input$volcano_group1)
      updateSelectInput(session, "volcano_group1", choices = idents_levels,
                        selected = if (!is.null(cur_g1) && cur_g1 %in% idents_levels) cur_g1
                                   else idents_levels[1])

      v2_choices <- c(setNames("rest", .tr("All other cells")),
                      .safe_setnames(idents_levels, paste("Group:", idents_levels)))
      cur_g2 <- isolate(input$volcano_group2)
      updateSelectInput(session, "volcano_group2", choices = v2_choices,
                        selected = if (!is.null(cur_g2) && cur_g2 %in% unname(v2_choices)) cur_g2
                                   else "rest")
    }, ignoreInit = TRUE)

    # ── Plot container router ────────────────────────────────────────────────
    output$plot_container <- renderUI({
      if (isTRUE(input$viz_type %in% c("heatmap", "stacked_violin", "heatmap_hier", "density_2d")))
        plotOutput(ns("plot_static"), height="600px")
      else if (identical(input$viz_type, "reduction_3d"))
        plotlyOutput(ns("plot_3d"), height="600px")
      else
        plotlyOutput(ns("plot_interactive"), height="600px")
    })

    # ── Static render (heatmap / stacked_violin) ─────────────────────────────
    output$plot_static <- renderPlot({
      req(global_data$sc_obj)
      obj  <- global_data$sc_obj
      type <- input$viz_type
      if (!type %in% c("heatmap", "stacked_violin", "heatmap_hier", "density_2d")) return(NULL)

      cfg <- list(type=type, feat_sel=input$feat_sel, group_by=input$group_by,
                  pt_size=input$pt_size, plot_theme=input$plot_theme,
                  hier_max_cells=input$hier_max_cells,
                  hier_clust_distance=input$hier_clust_distance,
                  hier_clust_method=input$hier_clust_method,
                  hier_k_row=input$hier_k_row,
                  density_gene=input$density_gene, density_reduction=input$density_reduction,
                  density_max_cells=input$density_max_cells)

      p <- tryCatch(
        build_sc_viz_plot(obj, cfg, input$sc_palette, sc_manual_colors_vec(), sc_gradient_vec()),
        error = function(e)
          ggplot() + annotate("text",x=1,y=1,label=paste(.tr("Erreur:"), e$message)) + theme_void()
      )
      # ComplexHeatmap n'est pas un ggplot -- jamais dans current_plot() (ggsave sinon)
      if (!identical(type, "heatmap_hier")) current_plot(p)
      p
    })

    # ── Interactive render (all other types) ─────────────────────────────────
    output$plot_interactive <- renderPlotly({
      req(global_data$sc_obj, input$group_by)
      obj  <- global_data$sc_obj
      type <- input$viz_type

      if (type %in% c("heatmap","stacked_violin","heatmap_hier","density_2d")) return(plotly_empty())

      # Deduplicated: reuse .current_cfg() (check_duplication guard, 15-line threshold)
      base_cfg <- .current_cfg()
      cfg <- base_cfg[c("type","reduction","feat_sel","group_by","scatter_gene1","scatter_gene2","scatter_cor_method","scatter_smooth","multi_gene","multi_plot_type","violin_boxplot","volcano_group1","volcano_group2","volcano_logfc","volcano_pval","volcano_show_labels","pt_size","plot_theme")]
      cfg$type <- type

      # Step-3.8B: fast preview on large objects (see .preview_obj() above) --
      # export_plot() re-renders on the untouched `obj`/full dataset separately.
      obj <- .preview_obj(obj, type, cfg$group_by %||% "seurat_clusters")

      p_gg <- tryCatch(
        build_sc_viz_plot(obj, cfg, input$sc_palette, sc_manual_colors_vec(),
                          sc_gradient_vec(), sc_volcano_colors_vec()),
        error = function(e)
          ggplot() + annotate("text",x=1,y=1,
                              label=paste(.tr("Erreur:"), e$message), color="red") + theme_void()
      )
      current_plot(p_gg)   # always set ggplot for export, regardless of display mode

      # Volcano: native plotly for richer hover (reuses markers from attr to
      # avoid a second FindMarkers call)
      if (type == "volcano") {
        markers <- attr(p_gg, "volcano_markers")
        vtitle  <- attr(p_gg, "volcano_title") %||% "Volcano"
        if (!is.null(markers)) {
          lfc       <- as.numeric(input$volcano_logfc %||% 0.25)
          pval      <- as.numeric(input$volcano_pval  %||% 0.05)
          pt_sz     <- as.numeric(input$pt_size %||% 0.5) * 5
          color_map <- bulk_role_colors(
            input$sc_palette,
            manual_colors = if (identical(input$sc_palette, "manual")) c(
              "Up"   = sc_volcano_colors_vec()[["up"]]   %||% "#E74C3C",
              "Down" = sc_volcano_colors_vec()[["down"]] %||% "#2980B9",
              "NS"   = sc_volcano_colors_vec()[["ns"]]   %||% "#BDC3C7"
            ) else NULL
          )
          x_rng     <- range(markers$avg_log2FC, na.rm=TRUE)
          return(
            plot_ly(data=markers, x=~avg_log2FC, y=~-log10(p_val_adj+1e-300),
                    type="scatter", mode="markers+text",
                    marker=list(color=~color_map[status], size=pt_sz, opacity=0.75,
                                line=list(width=0)),
                    text=~label, textposition="top center",
                    hovertext=~paste0("<b>",gene,"</b><br>Log2FC: ",round(avg_log2FC,3),
                                      "<br>-log10(padj): ",round(-log10(p_val_adj+1e-300),2),
                                      "<br>Status: ",status),
                    hoverinfo="text") |>
              layout(title=vtitle,
                     xaxis=list(title="Log2 Fold Change", zeroline=TRUE),
                     yaxis=list(title="-log10(P.adj)"),
                     shapes=list(
                       list(type="line",x0=lfc,x1=lfc,y0=0,y1=1,yref="paper",
                            line=list(dash="dot",color="#E74C3C",width=1)),
                       list(type="line",x0=-lfc,x1=-lfc,y0=0,y1=1,yref="paper",
                            line=list(dash="dot",color="#2980B9",width=1)),
                       list(type="line",x0=x_rng[1],x1=x_rng[2],
                            y0=-log10(pval),y1=-log10(pval),
                            line=list(dash="dot",color="#7F8C8D",width=1))
                     ), showlegend=FALSE)
          )
        }
        # Fallback to ggplotly if markers attr missing
        return(tryCatch(suppressWarnings(ggplotly(p_gg, tooltip="text")),
                        error=function(e) plotly_empty()))
      }

      tryCatch(suppressWarnings(ggplotly(p_gg, tooltip="text")),
               error = function(e) plotly_empty())
    })

    output$plot_3d <- renderPlotly({
      req(global_data$sc_obj, input$reduction_3d_pick)
      tryCatch(
        plot_sc_reduction_3d(global_data$sc_obj, reduction = input$reduction_3d_pick,
                             color_by = input$group_by %||% "seurat_clusters",
                             max_cells = as.integer(input$reduction_3d_max_cells %||% 50000L),
                             palette = input$sc_palette, manual_gradient = sc_gradient_vec(),
                             manual_colors = sc_manual_colors_vec()),
        error = function(e) {
          showNotification(paste("Erreur 3D:", conditionMessage(e)), type = "error", duration = 8)
          plotly_empty()
        }
      )
    })

    output$export_fidelity_note <- renderUI({
      req(global_data$sc_obj)
      type <- input$viz_type %||% "dim"
      if (!type %in% .PREVIEW_CELL_TYPES || ncol(global_data$sc_obj) <= .PREVIEW_MAX_CELLS) return(NULL)
      div(class="alert alert-warning py-1 px-2 mb-2", style="font-size:0.76em;",
          bsicons::bs_icon("hourglass-split"),
          sprintf(" Export haute fid\u00e9lit\u00e9 (%s cellules, dataset complet, jamais sous-\u00e9chantillonn\u00e9) : ",
                 format(ncol(global_data$sc_obj), big.mark=" ")),
          tags$strong("peut prendre plusieurs minutes (voire plus sur donn\u00e9es sur disque/BPCells)."))
    })

    # ── Export ───────────────────────────────────────────────────────────────
    # Step-3.8B: re-renders build_sc_viz_plot() from scratch on the FULL,
    # untouched global_data$sc_obj -- deliberately does NOT reuse current_plot(),
    # which may be the subsampled live-preview render (see .preview_obj() /
    # plot_interactive above). Export is always full-fidelity; if that's slow
    # on a very large object the UI warns beforehand (export_fidelity_note)
    # and shows an in-progress message here -- it is never silently downsampled.
    output$export_plot <- downloadHandler(
      filename = function() paste0("plot_", Sys.Date(), ".", input$export_format %||% "png"),
      content  = function(file) {
        req(global_data$sc_obj)
        cfg  <- .current_cfg()
        type <- cfg$type %||% "dim"

        if (identical(type, "reduction_3d")) {
          showNotification("Export non disponible pour la 3D (interactif uniquement).", type = "warning", duration = 8)
          req(FALSE)
        }

        n_cells <- ncol(global_data$sc_obj)
        is_slow <- type %in% .PREVIEW_CELL_TYPES && n_cells > .PREVIEW_MAX_CELLS

        withProgress(
          message = if (is_slow)
            sprintf("Export haute fid\u00e9lit\u00e9 (%s cellules) \u2014 peut prendre plusieurs minutes...",
                    format(n_cells, big.mark=" "))
          else "Export...",
          value = 0.3, {
            if (identical(type, "heatmap_hier")) {
              incProgress(0.3, detail = "Rendu de la heatmap...")
              # PLOT-S5 — device ouvert a la main (ComplexHeatmap n'est pas un
              # ggplot). `svg` rejoint `pdf` (vectoriel, dimensions en pouces) ;
              # `png` reste en pixels. Branches png/pdf inchangees.
              fmt <- input$export_format %||% "png"
              if (identical(fmt, "svg")) {
                svglite::svglite(file, width = (input$plot_width %||% 800)/100, height = (input$plot_height %||% 600)/100)
              } else if (identical(fmt, "pdf")) {
                pdf(file, width = (input$plot_width %||% 800)/100, height = (input$plot_height %||% 600)/100)
              } else {
                png(file, width = input$plot_width %||% 800, height = input$plot_height %||% 600, res = 100)
              }
              tryCatch({
                build_sc_viz_plot(global_data$sc_obj, cfg, cfg$sc_palette %||% input$sc_palette,
                                  cfg$sc_manual_colors, cfg$sc_gradient, cfg$sc_volcano_colors)
              }, error = function(e) {
                showNotification(paste("Erreur export heatmap:", conditionMessage(e)), type="error", duration=10)
              }, finally = { grDevices::dev.off() })
            } else {
              p <- tryCatch(
                build_sc_viz_plot(global_data$sc_obj, cfg, cfg$sc_palette %||% input$sc_palette,
                                  cfg$sc_manual_colors, cfg$sc_gradient, cfg$sc_volcano_colors),
                error = function(e) {
                  showNotification(paste("Erreur export:", conditionMessage(e)), type="error", duration=10)
                  stop(e)
                }
              )
              incProgress(0.6, detail = "Écriture du fichier...")
              ts_export_plot(file, p,
                     width  = (input$plot_width  %||% 800) / 100,
                     height = (input$plot_height %||% 600) / 100,
                     dpi    = 300,
                     format = input$export_format %||% "png")
            }
          }
        )
      }
    )

    # ── .tr("📌 Ajouter au Rapport") ──────────────────────────────────────────────
    observeEvent(input$add_to_report, {
      req(global_data$sc_obj)
      cfg <- .current_cfg()
      if (cfg$type %in% c("heatmap_hier", "reduction_3d")) {
        showNotification(
          "Ce type de visualisation n'est pas encore integrable au rapport HTML/PDF (live uniquement).",
          type = "warning", duration = 8)
        return()
      }
      title <- paste0(cfg$type, "_", format(Sys.time(), "%H%M%S"))
      current <- state_get(shared_rv, "report_viz_list") %||% list()
      current[[title]] <- cfg
      state_set(shared_rv, "report_viz_list", current)
      showNotification(paste("📌 Ajouté au rapport:", cfg$type),
                       type="message", duration=3)
    })

    # ── Volcano: add significant genes to viz basket ──────────────────────────
    observeEvent(input$volcano_add_sig, {
      req(current_plot())
      markers <- attr(current_plot(), "volcano_markers")
      if (is.null(markers)) {
        showNotification(.tr("Générez d'abord le Volcano Plot."), type="warning"); return()
      }
      lfc  <- as.numeric(input$volcano_logfc %||% 0.25)
      pval <- as.numeric(input$volcano_pval  %||% 0.05)
      sig  <- markers$gene[markers$p_val_adj < pval & abs(markers$avg_log2FC) > lfc]
      if (!length(sig)) {
        showNotification("Aucun gène significatif.", type="warning"); return()
      }
      current <- state_get(shared_rv, "selected_genes") %||% character(0)
      state_set(shared_rv, "selected_genes", unique(c(current, sig)))
      state_set(shared_rv, "active_tab", "tab_viz")
      showNotification(paste(length(sig), .tr("gènes significatifs ajoutés.")), type="message")
    })

    # ── Gene count status ────────────────────────────────────────────────────
    output$gene_selection_status <- renderText({
      n <- length(input$feat_sel %||% character(0))
      if (n == 0) .tr("Aucun gène sélectionné") else paste(n, .tr("gène(s) sélectionné(s)"))
    })

    # ── Clear gene selection ─────────────────────────────────────────────────
    observeEvent(input$clear_viz_genes, {
      req(global_data$sc_obj)
      obj  <- global_data$sc_obj
      var_features <- tryCatch(VariableFeatures(obj), error=function(e) character(0))
      gene_choices <- c(var_features, setdiff(rownames(obj), var_features))
      updateSelectizeInput(session, "feat_sel", choices=gene_choices,
                           selected=character(0), server=TRUE)
    })

  }) # /moduleServer
}

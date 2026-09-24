# =============================================================================
# mod_sc.R  —  Parent Router Module (Step-3.7)
# =============================================================================
# Step-3.6 changes (recap): report_viz_list + traj_reduction in reactiveValues,
# "0. Mapping IDs" panel, saved-viz basket, extended auto-pipeline, pipeline
# status bar + Résumé Pipeline tab.
#
# Step-3.7 changes:
#   [1] Auto-pipeline: secondary t-SNE always computed after UMAP (capped via
#       .AUTO_TSNE_MAX_CELLS, defined in mod_sc_pipeline.R) — same rationale
#       as the standalone "1. Pipeline" module: PCA/UMAP/t-SNE all available
#       in the Viz "Réduction à visualiser" picker without an extra manual run.
#   [2] Auto-pipeline: FindAllMarkers / Gene Correlation steps now run on a
#       RAM-safety-capped subsample (shared_rv$max_cells_heavy, set in
#       "1. Pipeline") instead of always the full object.
#   [3] render_params$traj_genes forwarded to the report (mirrors
#       shared_rv$traj_genes, written live by mod_sc_trajectory.R) so the new
#       "Gènes vs Pseudotemps" report section renders the same genes the user
#       was looking at live.
# =============================================================================

mod_sc_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 420, title = i18n$t("Single-Cell Workflow"),
      # LOT 1 (V1.x UX): paradigm badge — auto-pipeline exists, manual steps stay optional.
      div(class = "alert alert-success", style = "font-size:0.78rem;padding:5px;margin-bottom:5px;",
          icon("bolt"), " ", i18n$t("Auto-pipeline disponible — les étapes manuelles restent optionnelles")),
      div(class = "alert alert-info", style = "font-size:0.8rem;padding:5px;",
          bsicons::bs_icon("info-circle"), " ", i18n$t("Étapes séquentielles recommandées.")),
      # Mutualized auto-pipeline paradigm (V1.x UX): in EVERY domain the 1-click
      # auto pipeline is the FIRST accordion panel (0.), manual steps follow.
      accordion(
        id = ns("acc_workflow"), open = "grp_prep",
        # ── LOT 2 (V1.x UX): 5 parent sections replace the flat 17-panel list.
        # Section bodies are nested accordions; every historical panel value is
        # preserved (0_mapping, 1_pipeline, ..., 9b_report_consolide).
        # ── 1. Préparation ────────────────────────────────────────────────
        accordion_panel(i18n$t("Préparation"), icon = icon("layer-group"), value = "grp_prep",
          accordion(id = ns("acc_prep"), open = "1_pipeline",
            # Mutualized paradigm (V1.x UX): auto pipeline first, same panel
            # title/position as Bulk and Spatial (ids unchanged).
            accordion_panel(i18n$t("Pipeline auto (1 clic)"), icon = icon("bolt"),
                            value = "0_autopipeline",
                            actionButton(ns("btn_auto_pipeline_sc"), i18n$t("▶ Lancer Pipeline Complet (SC)"),
                                         icon = icon("play-circle"), class = "btn-outline-success w-100 mb-1"),
                            verbatimTextOutput(ns("sc_auto_log")),
                            uiOutput(ns("sc_pipeline_status_bar"))),
            accordion_panel(i18n$t("0. Mapping IDs (Optionnel)"), icon = icon("arrows-rotate"),
                            value = "0_mapping",
                            mod_sc_mapping_ui(ns("mapping"))),
            accordion_panel(i18n$t("1. Pipeline"), icon = icon("cogs"),
                            value = "1_pipeline",
                            mod_sc_pipeline_ui(ns("pipeline"))),
            # MD-4 (décision 5) : gestion du conteneur sc_datasets — enregistrer
            # l'objet SC courant (brut ou traité) sous un label avec relation
            # déclarée (mode 1 = paramètres partagés, mode 2 = distincts).
            accordion_panel(i18n$t("Datasets SC enregistrés (double jeu)"), icon = icon("database"),
                            value = "0_sc_datasets",
                            mod_sc_datasets_ui(ns("sc_datasets")))
          )),
        # ── 2. Analyse ────────────────────────────────────────────────────
        accordion_panel(i18n$t("Analyse"), icon = icon("chart-area"), value = "grp_analyse",
          accordion(id = ns("acc_analyse"),
            accordion_panel(i18n$t("2. Annotation"), icon = icon("user-tag"),
                            value = "2_annotation",
                            mod_sc_annotation_ui(ns("annotation"))),
            # CCC 9 (Q1) : rareté par population annotée — DESCRIPTIF et
            # mono-condition. Onglet du panneau SC existant, jamais un nouveau
            # panneau latéral (point de friction UX n°2).
            accordion_panel(i18n$t("2b. Rareté par population"), icon = icon("magnifying-glass-chart"),
                            value = "2b_rarity",
                            mod_sc_rarity_ui(ns("rarity"))),
            accordion_panel(i18n$t("3. Visualisation"), icon = icon("chart-area"),
                            value = "3_viz",
                            mod_sc_viz_ui(ns("viz"))),
            accordion_panel(i18n$t("4. Marqueurs"), icon = icon("magnifying-glass-chart"),
                            value = "4_markers",
                            mod_sc_markers_ui(ns("markers"))),
            accordion_panel(i18n$t("5. Gene Correlation"), icon = icon("project-diagram"),
                            value = "5_corr",
                            mod_sc_corr_ui(ns("corr"))),
            accordion_panel(i18n$t("6. Pathway Enrichment"), icon = icon("sitemap"),
                            value = "6_pathway",
                            mod_sc_pathways_ui(ns("pathways")))
          )),
        # ── 3. Dynamique ──────────────────────────────────────────────────
        accordion_panel(i18n$t("Dynamique"), icon = icon("route"), value = "grp_dynamique",
          accordion(id = ns("acc_dynamique"),
            accordion_panel(i18n$t("7. Trajectory Analysis"), icon = icon("route"),
                            value = "7_trajectory",
                            mod_sc_trajectory_ui(ns("trajectory"))),
            # LOT 5 (V1.x UX glossary): dual label — what RNA velocity means.
            accordion_panel(i18n$t("Vélocité ARN — orientation des trajectoires"), icon = icon("wind"),
                            value = "8_velocity",
                            mod_sc_velocity_ui(ns("velocity"))),
            accordion_panel(i18n$t("8b. Communication (import)"), icon = icon("satellite-dish"),
                            value = "8b_communication",
                            mod_sc_communication_ui(ns("communication")))
          )),
        # ── 4. Abondance cellulaire ───────────────────────────────────────
        accordion_panel(i18n$t("Abondance cellulaire"), icon = icon("balance-scale"), value = "grp_abondance",
          accordion(id = ns("acc_abondance"),
            # LOT 5 (V1.x UX glossary): dual label — what "pseudobulk" means.
            accordion_panel(i18n$t("Pseudobulk — agrégation des cellules par échantillon"), icon = icon("layer-group"),
                            value = "4b_pseudobulk",
                            mod_sc_pseudobulk_ui(ns("pseudobulk"))),
            # LOT 2 (mandatory DA nesting): the four flat panels 8c/8d/8e/8f
            # become ONE accordion panel whose body is an inner navset_pill.
            # A = design gatekeeper (8c); B = methods, Milo & scCODA side by
            # side (8d/8e) — existing UIs nested, NO backend merge; C = cross
            # views (8f). Gating of C stays where it is today: mod_sc_da_cross
            # already req()s both results internally (no new gate invented).
            accordion_panel(i18n$t("Abondance différentielle — design, méthodes & vues croisées"),
                            icon = icon("balance-scale"), value = "8_da_merged",
                            navset_pill(
                              id = ns("da_tabs"),
                              nav_panel(i18n$t("A. Plan expérimental (design)"), value = "da_design",
                                        mod_sc_da_design_ui(ns("da_design"))),
                              nav_panel(i18n$t("B. Méthodes (Milo / scCODA)"), value = "da_methods",
                                        navset_pill(
                                          id = ns("da_methods_tabs"),
                                          # LOT 5 (V1.x UX): tooltips demystify the method names.
                                          nav_panel(tagList("Milo ", bslib::tooltip(icon("circle-info"), i18n$t("Milo — abondance par voisinages de cellules"))),
                                                    value = "da_milo",
                                                    mod_sc_da_milo_ui(ns("da_milo"))),
                                          nav_panel(tagList("scCODA ", bslib::tooltip(icon("circle-info"), i18n$t("scCODA — composition cellulaire (modèle bayésien)"))),
                                                    value = "da_sccoda",
                                                    mod_sc_da_sccoda_ui(ns("da_sccoda")))
                                        )),
                              nav_panel(i18n$t("C. Vues croisées"), value = "da_cross",
                                        mod_sc_da_cross_ui(ns("da_cross")))
                            ))
          )),
        # ── 5. Livrables (LOT 3A: reports are the workflow's outcome) ─────
        accordion_panel(i18n$t("Livrables"), icon = icon("file-export"), value = "grp_livrables",
          accordion(id = ns("acc_livrables"),
            accordion_panel(
              i18n$t("9. Rapport Complet"), icon = icon("file-export"),
              value = "9_report",
              div(class = "alert alert-light", style = "font-size:0.85em;border-left:3px solid #2C3E50;",
                  i18n$t("Rapport autonome (QC, Réduction, Annotation, Marqueurs, Pathways, Trajectoire).")),
              textInput(ns("report_title"), i18n$t("Titre"), value = "Analyse Single-Cell"),
              textInput(ns("report_subtitle"), i18n$t("Sous-titre (optionnel)")),
              textAreaInput(ns("report_notes"), i18n$t("Notes"), rows = 3),
              checkboxGroupInput(ns("report_sections"), i18n$t("Sections"),
                choices = setNames(
                  c("qc", "dim", "annotation", "rarity", "markers", "correlation", "pathway", "trajectory", "velocity", "communication", "da", "custom_viz"),
                  c(.tr_plain("QC"), .tr_plain("Réduction Dimensionnelle"), .tr_plain("Annotation"),
                    .tr_plain("Rareté par population"),
                    .tr_plain("Marqueurs"), .tr_plain("Réseau Corrélation"), .tr_plain("Pathway Enrichment"),
                    .tr_plain("Trajectoire"), .tr_plain("Vitesse ARN"), .tr_plain("Communication cellulaire"),
                    .tr_plain("Abondance différentielle"), .tr_plain("Visualisations Sauvegardées"))),
                selected = c("qc", "dim", "annotation", "markers", "pathway")),
              div(class = "border rounded p-2 mb-2", style = "background:#f8f9fa;",
                  h6(i18n$t("📌 Visualisations sauvegardées"), style = "font-size:0.85em;font-weight:bold;"),
                  uiOutput(ns("saved_viz_list_ui")),
                  actionButton(ns("clear_saved_viz"), i18n$t("🗑️ Vider la liste"),
                               class = "btn-outline-danger btn-sm w-100 mt-1")),
              radioButtons(ns("report_format"), i18n$t("Format"),
                choices = setNames(c("html", "pdf", "both"),
                                   c(.tr_plain("HTML interactif"), .tr_plain("PDF statique"), .tr_plain("Les deux (.zip)"))),
                selected = "html"),
              conditionalPanel(condition = "input.report_format != 'pdf'", ns = ns,
                checkboxInput(ns("report_interactive"), i18n$t("Graphiques interactifs (HTML)"), value = TRUE)),
              div(class = "small text-muted", i18n$t("PDF requiert tinytex::install_tinytex().")),
              downloadButton(ns("dl_report"), i18n$t("📄 Générer le Rapport"), class = "btn-dark w-100 mt-2"),
              hr(),
              div(class = "alert alert-light", style = "font-size:0.82em;border-left:3px solid #18BC9C;",
                  bsicons::bs_icon("code-slash"),
                  i18n$t(" Script R reproductible (.zip) + objet Seurat traité.")),
              downloadButton(ns("dl_sc_r_script"), i18n$t("🧾 Export Script R (.zip)"),
                             class = "btn-outline-secondary w-100"),
              div(class = "small text-muted mt-1", textOutput(ns("report_status")))
            ),
            # Stage 17 (4F) : rapport consolide — compile l'etat canonique + la
            # provenance (aucune re-execution d'analyse ; le panneau 9 historique
            # reste le rapport Rmd par domaine, inchangé).
            accordion_panel(i18n$t("9b. Rapport consolidé (4F)"), icon = icon("clipboard-check"),
                            value = "9b_report_consolide",
                            mod_sc_report_consolidated_ui(ns("report_consolide")))
          ))
      )
    ),
    navset_card_underline(
      id = ns("main_tabs"), title = i18n$t("Résultats"),
      nav_panel(i18n$t("Graphiques"), value = "tab_viz", mod_sc_viz_output_ui(ns("viz"))),
      # MAJ 2
      nav_panel(i18n$t("Table Marqueurs"), value = "tab_table", mod_sc_markers_output_ui(ns("markers"))),
      nav_panel(i18n$t("Pseudobulk DE"), value = "tab_pseudobulk", mod_sc_pseudobulk_output_ui(ns("pseudobulk"))),
      nav_panel(i18n$t("Annotation"), value = "tab_annotation", mod_sc_annotation_output_ui(ns("annotation"))),
      nav_panel(i18n$t("Rareté par population"), value = "tab_rarity", mod_sc_rarity_output_ui(ns("rarity"))),
      nav_panel(i18n$t("Gènes Corrélés"), value = "tab_correlation", mod_sc_corr_output_ui(ns("corr"))),
      nav_panel(i18n$t("Pathways"), value = "tab_pathway", mod_sc_pathways_output_ui(ns("pathways"))),
      nav_panel(i18n$t("Trajectory"), value = "tab_trajectory", mod_sc_trajectory_output_ui(ns("trajectory"))),
      nav_panel(i18n$t("Velocity"), value = "tab_velocity", mod_sc_velocity_output_ui(ns("velocity"))),
      nav_panel(i18n$t("Communication"), value = "tab_communication", mod_sc_communication_output_ui(ns("communication"))),
      nav_panel(i18n$t("DA design"), value = "tab_da_design", mod_sc_da_design_output_ui(ns("da_design"))),
      nav_panel(i18n$t("Milo DA"), value = "tab_da_milo", mod_sc_da_milo_output_ui(ns("da_milo"))),
      nav_panel(i18n$t("scCODA DA"), value = "tab_da_sccoda", mod_sc_da_sccoda_output_ui(ns("da_sccoda"))),
      nav_panel(i18n$t("DA croisées"), value = "tab_da_cross", mod_sc_da_cross_output_ui(ns("da_cross"))),
      nav_panel(i18n$t("Rapport consolidé"), value = "tab_report_consolide", mod_sc_report_consolidated_output_ui(ns("report_consolide"))),
      nav_panel(i18n$t("QC"), value = "tab_qc",
        card(max_height = 750,
          div(class = "card-header bg-light", h5(i18n$t("Contrôle Qualité"), class = "card-title mb-0")),
          plotOutput(ns("plot_qc"), height = "650px"),
          # V1.x UX multi-échantillons : sous-panneau léger, visible seulement
          # quand >= 2 échantillons (orig.ident) — barplots méta.data +
          # projections sur réductions EXISTANTES (aucun calcul nouveau).
          uiOutput(ns("multisample_overview_ui")))),
      nav_panel(i18n$t("Résumé Pipeline"), value = "tab_summary",
        card(card_header(i18n$t("Résumé du Pipeline Single-Cell")),
             uiOutput(ns("pipeline_summary_panel"))))
    )
  )
}


mod_sc_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {
    # OBLIGATOIRE : `ns` n'est lie que dans la fonction UI (NS(id)), pas dans le
    # serveur. Sans cette ligne, un renderUI qui appelle ns() leve « impossible
    # de trouver la fonction "ns" » — et seulement quand la branche s'affiche,
    # donc invisible au demarrage. Garde : test-release-hardening.R.
    ns <- session$ns

    .tr <- function(key) { tr <- isolate(global_data$i18n); if (is.null(tr)) return(key); tryCatch(.strip_i18n_html(tr$t(key)), error=function(e) key) }

    shared_rv <- create_sc_shared_state()

    # ── i18n: update report section choices on language switch ──────────────
    observeEvent(global_data$language, {
      updateCheckboxGroupInput(session, "report_sections",
        label = .tr("Sections"),
        choices = stats::setNames(
          c("qc", "dim", "annotation", "rarity", "markers", "correlation", "pathway", "trajectory", "velocity", "communication", "da", "custom_viz"),
          c(.tr("QC"), .tr("Réduction Dimensionnelle"), .tr("Annotation"),
            .tr("Rareté par population"),
            .tr("Marqueurs"), .tr("Réseau Corrélation"), .tr("Pathway Enrichment"),
            .tr("Trajectoire"), .tr("Vitesse ARN"), .tr("Communication cellulaire"),
            .tr("Abondance différentielle"), .tr("Visualisations Sauvegardées"))))

      updateRadioButtons(session, "report_format",
        label = .tr("Format"),
        choices = stats::setNames(c("html", "pdf", "both"),
          c(.tr("HTML interactif"), .tr("PDF statique"), .tr("Les deux (.zip)"))))

      updateTextInput(session, "report_title", label = .tr("Titre"))
      updateTextInput(session, "report_subtitle", label = .tr("Sous-titre (optionnel)"))
      updateTextAreaInput(session, "report_notes", label = .tr("Notes"))
      updateCheckboxInput(session, "report_interactive",
        label = .tr("Graphiques interactifs (HTML)"))
      updateActionButton(session, "dl_report",
        label = paste("📄", .tr("Générer le Rapport")))
    }, ignoreInit = TRUE)

    observeEvent(state_get(shared_rv, "active_tab"), {
      req(state_get(shared_rv, "active_tab"))
      nav_select(id="main_tabs", selected=state_get(shared_rv, "active_tab"), session=session)
    })

    # ── QC plot ──────────────────────────────────────────────────────────────
    output$plot_qc <- renderPlot({
      req(global_data$sc_obj)
      VlnPlot(global_data$sc_obj,
              features=c("nFeature_RNA","nCount_RNA","percent.mt"), ncol=3, pt.size=0)
    })

    # ── V1.x UX multi-échantillons : vue d'ensemble (≥ 2 orig.ident) ─────────
    # Lectures pures de l'état existant : barplots depuis méta.data, projets
    # 2D depuis une réduction DÉJÀ calculée (aucun recalcul, aucune
    # densification). Visible uniquement quand >= 2 échantillons.
    .ms_pick_reduction <- function(obj) {
      for (r in c("umap", "umap_harmony", "tsne", "pca"))
        if (r %in% names(obj@reductions)) return(r)
      NULL
    }
    .ms_dim_plot <- function(obj, group_by, red, title) {
      tryCatch(
        Seurat::DimPlot(obj, reduction = red, group.by = group_by,
                        raster = ncol(obj) > 50000) +
          ggplot2::ggtitle(title),
        error = function(e) NULL)
    }
    output$multisample_overview_ui <- renderUI({
      global_data$language
      obj <- global_data$sc_obj
      if (is.null(obj) || is.null(obj$orig.ident)) return(NULL)
      if (length(unique(obj$orig.ident)) < 2) return(NULL)
      has_cond <- "condition" %in% colnames(obj@meta.data)
      red      <- .ms_pick_reduction(obj)
      tagList(
        hr(),
        h5(.tr("Vue d'ensemble multi-échantillons"), class = "mb-1"),
        div(class = "alert alert-light", style = "font-size:0.82em;padding:6px;",
            bsicons::bs_icon("lightbulb"), " ",
            .tr("Séparation forte par échantillon ? Activez Harmony dans le Pipeline.")),
        plotOutput(ns("ms_plot_cells_sample"), height = "280px"),
        if (has_cond) plotOutput(ns("ms_plot_cells_condition"), height = "280px"),
        if (!is.null(red)) {
          tagList(
            plotOutput(ns("ms_plot_dim_sample"), height = "380px"),
            if (has_cond) plotOutput(ns("ms_plot_dim_condition"), height = "380px"))
        } else {
          div(class = "alert alert-warning", style = "font-size:0.82em;padding:6px;",
              .tr("Aucune réduction 2D disponible : lancez le Pipeline pour afficher les projections par échantillon."))
        }
      )
    })
    output$ms_plot_cells_sample <- renderPlot({
      global_data$language
      obj <- global_data$sc_obj
      req(obj, obj$orig.ident)
      op <- par(mar = c(9, 4, 2, 1)); on.exit(par(op), add = TRUE)
      barplot(table(obj$orig.ident), las = 2, col = "#2C3E50",
              main = .tr("Cellules par échantillon"), ylab = "n")
    })
    output$ms_plot_cells_condition <- renderPlot({
      global_data$language
      obj <- global_data$sc_obj
      req(obj)
      req("condition" %in% colnames(obj@meta.data))
      op <- par(mar = c(9, 4, 2, 1)); on.exit(par(op), add = TRUE)
      barplot(table(factor(obj@meta.data$condition)), las = 2, col = "#18BC9C",
              main = .tr("Cellules par condition"), ylab = "n")
    })
    output$ms_plot_dim_sample <- renderPlot({
      global_data$language
      obj <- global_data$sc_obj
      req(obj)
      red <- .ms_pick_reduction(obj)
      req(red)
      .ms_dim_plot(obj, "orig.ident", red, .tr("Projection par échantillon"))
    })
    output$ms_plot_dim_condition <- renderPlot({
      global_data$language
      obj <- global_data$sc_obj
      req(obj)
      req("condition" %in% colnames(obj@meta.data))
      red <- .ms_pick_reduction(obj)
      req(red)
      .ms_dim_plot(obj, "condition", red, .tr("Projection par condition"))
    })

    # ── Pipeline status bar ───────────────────────────────────────────────────
    output$sc_pipeline_status_bar <- renderUI({
      obj <- global_data$sc_obj
      if (is.null(obj)) return(NULL)
      meta <- obj@meta.data
      s_qc      <- if ("percent.mt"      %in% colnames(meta))                     "\u2705" else "\u26aa"
      s_norm    <- if (length(tryCatch(VariableFeatures(obj),error=function(e) character(0))) > 0) "\u2705" else "\u26aa"
      s_cluster <- if ("seurat_clusters" %in% colnames(meta))                     "\u2705" else "\u26aa"
      s_umap    <- if ("umap"            %in% names(obj@reductions))              "\u2705" else "\u26aa"
      s_annot   <- if (any(grepl("^SingleR_", colnames(meta))))                   "\u2705" else "\u26aa"
      s_markers <- if (!is.null(state_get(shared_rv, "markers_data")))                          "\u2705" else "\u26aa"
      div(style=paste0("display:flex;justify-content:space-around;font-size:0.72em;",
                       "background:#f8f9fa;border:1px solid #e3e6e8;border-radius:6px;",
                       "padding:4px 2px;margin-bottom:8px;"),
          tags$span(style="padding:2px 4px;", s_qc,      .tr(" QC")),
          tags$span(style="padding:2px 4px;", s_norm,    .tr(" Norm")),
          tags$span(style="padding:2px 4px;", s_cluster, .tr(" Cluster")),
          tags$span(style="padding:2px 4px;", s_umap,    .tr(" UMAP")),
          tags$span(style="padding:2px 4px;", s_annot,   .tr(" Annot")),
          tags$span(style="padding:2px 4px;", s_markers, .tr(" Marqueurs")))
    })

    # ── Pipeline summary panel ────────────────────────────────────────────────
    output$pipeline_summary_panel <- renderUI({
      global_data$language
      obj <- global_data$sc_obj
      if (is.null(obj))
        return(div(class="alert alert-info m-3", .tr("Aucun objet Single-Cell chargé.")))
      meta         <- obj@meta.data
      singler_cols <- grep("^SingleR_", colnames(meta), value=TRUE)
      reductions   <- names(obj@reductions)
      n_clusters   <- if ("seurat_clusters" %in% colnames(meta))
                        length(levels(factor(meta$seurat_clusters))) else NA
      rows <- list(
        c(.tr("Cellules"),              format(ncol(obj), big.mark=",")),
        c(.tr("Gènes"),                 format(nrow(obj), big.mark=",")),
        c(.tr("Réductions"),            if (length(reductions)) paste(reductions,collapse=", ") else "\u2014"),
        c(.tr("Clusters"),              if (!is.na(n_clusters)) as.character(n_clusters) else .tr("Non calculé")),
        c(.tr("Annotation SingleR"),    if (length(singler_cols)) paste(singler_cols,collapse=", ") else .tr("Non effectuée")),
        c(.tr("Gènes variables"),       if (length(tryCatch(VariableFeatures(obj),error=function(e) character(0))))
                                     format(length(VariableFeatures(obj)),big.mark=",") else "\u2014"),
        c(.tr("Marqueurs calculés"),    if (!is.null(state_get(shared_rv, "markers_data")))
                                     paste(nrow(state_get(shared_rv, "markers_data")),"marqueurs") else .tr("Non calculés")),
        c(.tr("Pathways"),              if (!is.null(state_get(shared_rv, "pathway_results")))
                                     paste(nrow(state_get(shared_rv, "pathway_results")),"pathways") else .tr("Non calculés")),
        c(.tr("Pseudotemps"),           if ("pseudotime" %in% colnames(meta)) .tr("Calculé") else .tr("Non calculé")),
        c(.tr("Backend stockage"),      if (sc_backend_status(obj) == "disk") "\U0001f4bd Disque (BPCells)" else "\U0001f9e0 RAM (standard)"),
        c(.tr("Sous-échant. (marqueurs/corr)"), if (is.finite(state_get(shared_rv, "max_cells_heavy") %||% Inf))
                                     paste0("max ", format(state_get(shared_rv, "max_cells_heavy"), big.mark=","), " ", .tr_plain("cellules/groupe"))
                                   else .tr("désactivé")),
        c(.tr("Viz. sauvegardées"),     paste0(length(state_get(shared_rv, "report_viz_list")), " ", .tr_plain("plot(s) dans le panier")))
      )
      tagList(
        div(class="m-3",
          tags$table(class="table table-sm table-bordered",
            tags$tbody(lapply(rows, function(r) {
              tags$tr(tags$th(style="width:40%;",r[1]), tags$td(r[2]))
            }))),
          div(class="small text-muted", paste(.tr("Mis à jour :"), format(Sys.time(),"%H:%M:%S")))
        )
      )
    })

    # ── Saved viz basket UI ───────────────────────────────────────────────────
    output$saved_viz_list_ui <- renderUI({
      lst <- state_get(shared_rv, "report_viz_list") %||% list()
      if (!length(lst))
        return(div(class="text-muted small", paste0(.tr("Aucune visualisation sauvegardée. "), .tr("Utilisez '📌 Ajouter au Rapport' dans l'onglet Graphiques."))))
      tags$ul(style="font-size:0.8em;margin-bottom:0;",
              lapply(names(lst), function(nm) tags$li(nm)))
    })

    observeEvent(input$clear_saved_viz, {
      state_set(shared_rv, "report_viz_list", list())
      showNotification(.tr("🗑️ Liste de visualisations vidée."), type="message", duration=3)
    })

    # ── Child servers ─────────────────────────────────────────────────────────
    mod_sc_mapping_server(   "mapping",   global_data)
    mod_sc_pipeline_server(  "pipeline",  global_data, shared_rv)
    mod_sc_datasets_server(  "sc_datasets", global_data)  # MD-4 : conteneur sc_datasets (lecture seule de sc_obj)
    mod_sc_annotation_server("annotation",global_data, shared_rv)
    mod_sc_rarity_server("rarity", global_data, shared_rv)
    mod_sc_viz_server(       "viz",       global_data, shared_rv)
    # maj 3
    mod_sc_markers_server(   "markers",   global_data, shared_rv)
    mod_sc_pseudobulk_server("pseudobulk",global_data, shared_rv)
    mod_sc_corr_server(      "corr",      global_data, shared_rv)
    mod_sc_pathways_server(  "pathways",  global_data, shared_rv)
    mod_sc_trajectory_server("trajectory",global_data, shared_rv)
    mod_sc_velocity_server("velocity", global_data, shared_rv)
    mod_sc_communication_server("communication", global_data, shared_rv)
    mod_sc_da_design_server("da_design", global_data, shared_rv)
    mod_sc_da_milo_server("da_milo", global_data, shared_rv)
    mod_sc_da_sccoda_server("da_sccoda", global_data, shared_rv)
    mod_sc_da_cross_server("da_cross", global_data, shared_rv)
    mod_sc_report_consolidated_server("report_consolide", global_data, shared_rv)  # Stage 17 (4F)

    # ── traj_reduction / traj_genes mirrors (written by mod_sc_trajectory_server)

    # =========================================================================
    # AUTO-PIPELINE MODAL
    # =========================================================================
    sc_log_rv <- reactiveVal("")
    output$sc_auto_log <- renderText({ sc_log_rv() })

    # ── Step-3.8A: sketch preset hint + PCA-dims sync ──────────────────────
    output$sc_ap_sketch_hint <- renderUI({
      req(global_data$sc_obj, input$sc_ap_sketch_preset)
      n_total <- ncol(global_data$sc_obj)
      params  <- resolve_sketch_preset(input$sc_ap_sketch_preset, n_total,
                                        input$sc_ap_sketch_ncells_custom)
      will_sketch <- params$ncells < n_total
      div(class="small", style=paste0("color:", if (will_sketch) "#18BC9C" else "#666", ";"),
          .t_fmt(.tr("{n} / {total} cellules — npcs suggéré : {npcs} ({sketch})"),
                 n     = format(params$ncells, big.mark = " "),
                 total = format(n_total, big.mark = " "),
                 npcs  = params$npcs,
                 sketch = if (will_sketch) .tr("sketch actif") else .tr("pas de sketch")))
    })

    observeEvent(input$sc_ap_sketch_preset, {
      req(global_data$sc_obj)
      params <- resolve_sketch_preset(input$sc_ap_sketch_preset, ncol(global_data$sc_obj),
                                       input$sc_ap_sketch_ncells_custom)
      updateSliderInput(session, "sc_ap_pca_dim", value = params$npcs)
    }, ignoreInit = TRUE)

    observeEvent(input$btn_auto_pipeline_sc, {
      req(global_data$sc_obj)
      ns_m <- session$ns
      detected_map_org <- tryCatch(detect_organism_from_ids(rownames(global_data$sc_obj)),
                                   error = function(e) NA_character_)
      mapping_org_selected <- if (!is.na(detected_map_org)) detected_map_org else "human"
      showModal(modalDialog(
        title=paste("\u25b6", .tr("Pipeline SC — Paramètres")), size="m", easyClose=TRUE,

        # ── Step 0: Mapping ─────────────────────────────────────────────────
        checkboxInput(ns_m("sc_ap_mapping"),
                      paste("\U0001f504", .tr("Mapping IDs → Symbol (auto-détecté, avant QC)")), value=TRUE),
        conditionalPanel(
          condition=sprintf("input['%s'] == true", ns_m("sc_ap_mapping")),
          selectInput(ns_m("sc_ap_mapping_org"), .tr("Organisme (mapping)"),
                      stats::setNames(c("human","mouse"), c(.tr("Humain"), .tr("Souris"))),
                      selected = mapping_org_selected)),
        checkboxInput(ns_m("sc_ap_bpcells"),
                      .t_fmt(.tr("💽 Backend disque (BPCells) si > {n} cellules"),
                             n = format(.BPCELLS_AUTO_THRESHOLD, big.mark = " ")),
                      value = TRUE),
        hr(),

        # ── Step 1: QC ──────────────────────────────────────────────────────
        fluidRow(
          column(6,
            h6(.tr("QC"), style="font-weight:bold;"),
            numericInput(ns_m("sc_ap_min_gene"), .tr("Min gènes/cellule"),   100, min=0),
            numericInput(ns_m("sc_ap_max_gene"), .tr("Max gènes/cellule"), 8000, min=0),
            sliderInput(ns_m("sc_ap_mt"), .tr("% Mito max"), 0, 50, 20, step=1)
          ),
          column(6,
            h6(.tr("Normalisation & Réduction"), style="font-weight:bold;"),
            radioButtons(ns_m("sc_ap_norm"), .tr("Normalisation"),
                         stats::setNames(c("log","sct"),
                                         c(.tr("LogNormalize"), .tr("SCTransform")))),
            sliderInput(ns_m("sc_ap_pca_dim"), .tr("Dims PCA"), 5, 50, 20),
            numericInput(ns_m("sc_ap_res"), .tr("Résolution clustering"), 0.5, min=0.1, step=0.1),
            selectInput(ns_m("sc_ap_cluster_algo"), .tr("Algorithme de clustering"),
                       choices = stats::setNames(c("1","2","3","4"),
                                                 c(.tr("Louvain (standard)"),
                                                   .tr("Louvain (multilevel refinement)"),
                                                   .tr("SLM (Smart Local Moving)"),
                                                   .tr("Leiden (nécessite reticulate + leidenalg)"))),
                       selected="1")
          )
        ),
        checkboxInput(ns_m("sc_ap_compute_umap"),
                     paste("\u2713", .tr("Calculer UMAP (décochez pour PCA seul — bien plus rapide, mode debug)")),
                     value = TRUE),
        div(class="small text-muted mb-2",
            .tr("Si coché : UMAP + t-SNE secondaire (si dataset raisonnable) sont calculés (le plus lent du pipeline). Si décoché : PCA seul — previews/trajectoire se rabattent automatiquement sur PCA, rien ne plante.")),
        hr(),

        # ── Sketch ─────────────────────────────────────────────────────────
        h6(.tr("Sketch — gros datasets"), style="font-weight:bold;"),
        div(class="small text-muted mb-1",
            .tr("PCA/Clustering/UMAP tournent sur un sous-ensemble représentatif (LeverageScore), puis sont projetés sur toutes les cellules. Accélère fortement les gros datasets (ex: 1,3M cellules) sans perdre les clusters rares. Ignoré si SCTransform est choisi.")),
        fluidRow(
          column(7, selectInput(ns_m("sc_ap_sketch_preset"), .tr("Preset sketch"),
            choices = stats::setNames(
              c("fast","light","medium","standard","high","max","custom"),
              c(.tr("Rapide (test, 5 000 cellules)"),
                .tr("Léger (10 000 cellules)"),
                .tr("Moyen (25 000 cellules)"),
                .tr("Standard (50 000 cellules)"),
                .tr("Élevé (100 000 cellules)"),
                .tr("Max (dataset complet)"),
                .tr("Personnalisé"))),
            selected = "standard")),
          column(5, conditionalPanel(
            condition = sprintf("input['%s'] == 'custom'", ns_m("sc_ap_sketch_preset")),
            numericInput(ns_m("sc_ap_sketch_ncells_custom"), .tr("N cellules"),
                         value = 20000, min = 1000, max = 500000, step = 1000)))
        ),
        uiOutput(ns_m("sc_ap_sketch_hint")),
        hr(),

        # ── Steps 2-7 optional ──────────────────────────────────────────────
        h6(.tr("Options supplémentaires"), style="font-weight:bold;"),
        checkboxInput(ns_m("sc_ap_singler"), paste("\U0001f9ec", .tr("Annotation SingleR")), value=FALSE),
        conditionalPanel(
          condition=sprintf("input['%s'] == true", ns_m("sc_ap_singler")),
          fluidRow(
            column(6, selectInput(ns_m("sc_ap_singler_ref"), .tr("Référence"),
                       stats::setNames(c("hpca","blueprint","immgen","dice"),
                                       c(.tr("Human Primary Cell Atlas"), .tr("Blueprint Encode"),
                                         .tr("ImmGen (Mouse)"), .tr("DICE Immune"))))),
            column(6, radioButtons(ns_m("sc_ap_singler_level"), .tr("Niveau"),
                       stats::setNames(c("main","fine"),
                                       c(.tr("Main (General)"), .tr("Fine (Specifique)"))), inline=TRUE))
          )
        ),
        checkboxInput(ns_m("sc_ap_markers"),
                      paste("\U0001f9ec", .tr("FindAllMarkers après clustering")), value=FALSE),
        checkboxInput(ns_m("sc_ap_pathway"),
                      paste("\U0001f9ec", .tr("Pathway ORA sur top marqueurs")), value=FALSE),
        conditionalPanel(
          condition=sprintf("input['%s'] == true", ns_m("sc_ap_pathway")),
          fluidRow(
            column(6, selectInput(ns_m("sc_ap_pathway_db"), .tr("Base"),
                       stats::setNames(c("GOBP","KEGG","Reactome"),
                                       c(.tr("GO BP"), .tr("KEGG"), .tr("Reactome"))))),
            column(6, selectInput(ns_m("sc_ap_pathway_org"), .tr("Organisme"),
                       stats::setNames(c("human","mouse"),
                                       c(.tr("Humain"), .tr("Souris")))))
          )
        ),
        checkboxInput(ns_m("sc_ap_correlation"),
                      paste("\U0001f9ec", .tr("Gene Correlation (auto: gène le plus significatif)")), value=FALSE),
        helpText(style="font-size:0.78em;color:#666;",
                 .tr("Requiert 'Marqueurs' coché. Corrèle le gène à p-adj minimal avec tous les autres.")),
        checkboxInput(ns_m("sc_ap_trajectory"),
                      paste("\U0001f9ec", .tr("Trajectory / Pseudotemps (UMAP, racine auto)")), value=FALSE),

        footer=tagList(
          modalButton(.tr("Annuler")),
          actionButton(ns_m("sc_ap_confirm"), paste("\u25b6", .tr("Lancer")), class="btn-success"))
      ))
    })

    # =========================================================================
    # AUTO-PIPELINE SERVER
    # =========================================================================
    observeEvent(input$sc_ap_confirm, {
      removeModal()
      req(global_data$sc_obj)
      run_sc_auto_pipeline(input, global_data, shared_rv, session, sc_log_rv)
    })

    # =========================================================================
    # REPORT STATUS
    # =========================================================================
    output$report_status <- renderText({
      if (is.null(global_data$sc_obj)) .tr("Importez et traitez un objet SC.")
      else sprintf(.tr("Prêt — %d viz. sauvegardée(s) dans le panier."),
                   length(state_get(shared_rv, "report_viz_list") %||% list()))
    })

    # =========================================================================
    # HTML / PDF REPORT
    # =========================================================================
    output$dl_report <- downloadHandler(
      filename = function() {
        ext <- switch(input$report_format, html="html", pdf="pdf", both="zip")
        paste0("rapport_singlecell_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".", ext)
      },
      content = function(file) {
        req(global_data$sc_obj)
        template_path <- file.path("reports","sc_report_template.Rmd")
        if (!file.exists(template_path)) stop(errorCondition("Template introuvable : reports/sc_report_template.Rmd", class = "mod_sc_error"))
        tmp_rmd <- file.path(tempdir(), "sc_report_template.Rmd")
        file.copy(template_path, tmp_rmd, overwrite=TRUE)

        # NULL-guard corr params
        corr_genes  <- if (!is.null(state_get(shared_rv, "correlated_genes")) &&
                           is.data.frame(state_get(shared_rv, "correlated_genes")) &&
                           nrow(state_get(shared_rv, "correlated_genes")) > 0) state_get(shared_rv, "correlated_genes") else NULL
        corr_target <- if (!is.null(state_get(shared_rv, "corr_target_gene")) &&
                           nchar(state_get(shared_rv, "corr_target_gene") %||% "") > 0) state_get(shared_rv, "corr_target_gene") else NULL

        # i18n Phase 6 : build translation map for report (French keys -> current language)
        lang <- isolate(global_data$language) %||% "fr"
        i18n_strings <- tryCatch({
          json_path <- file.path("i18n", "translation.json")
          if (file.exists(json_path)) {
            j <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)
            trans <- j$translation
            if (!is.null(trans)) {
              s <- setNames(
                vapply(trans, function(x) if (lang == "en") x$en %||% x$fr else x$fr, character(1)),
                vapply(trans, function(x) x$fr, character(1))
              )
              as.list(s)
            } else NULL
          } else NULL
        }, error = function(e) NULL)

        render_params <- list(
          sc_obj           = global_data$sc_obj,
          markers_data     = state_get(shared_rv, "markers_data"),
          pathway_results  = state_get(shared_rv, "pathway_results"),
          pathway_db       = state_get(shared_rv, "pathway_db"),
          correlated_genes = corr_genes,
          corr_target_gene = corr_target,
          sections         = input$report_sections %||% character(0),
          reduction        = "umap",
          traj_reduction   = state_get(shared_rv, "traj_reduction") %||% "umap",
          traj_method      = state_get(shared_rv, "traj_method"),   # transient fallback; obj@meta.data provenance wins in the report
          traj_genes       = state_get(shared_rv, "traj_genes") %||% character(0),  # Step-3.7
          # Post-V1.0 (mandat utilisateur) : resultats canoniques exposes au
          # rapport Rmd — restitues TELS QUELS (tables, aucune re-execution).
          velocity_result      = state_get(shared_rv, "velocity_result"),
          communication_result = state_get(shared_rv, "communication_result"),
          communication_collection = state_get(shared_rv, "communication_collection"),
          active_communication_sample = state_get(shared_rv, "active_communication_sample"),
          da_design_result     = state_get(shared_rv, "da_design_result"),
          da_milo_result       = state_get(shared_rv, "da_milo_result"),
          da_sccoda_result     = state_get(shared_rv, "da_sccoda_result"),
          # CCC 9 (Q1) : rareté par population — restituée TELLE QUELLE
          # (table descriptive, aucune re-exécution).
          population_rarity_result = state_get(shared_rv, "population_rarity_result"),
          saved_viz_list   = if (length(state_get(shared_rv, "report_viz_list"))) state_get(shared_rv, "report_viz_list") else NULL,
          group_by         = "seurat_clusters",
          sc_palette         = state_get(shared_rv, "sc_palette") %||% "default",
          sc_manual_colors   = state_get(shared_rv, "sc_manual_colors"),
          sc_manual_gradient = state_get(shared_rv, "sc_manual_gradient"),
          report_title     = input$report_title    %||% "Analyse Single-Cell",
          report_subtitle  = input$report_subtitle %||% "",
          report_notes     = input$report_notes    %||% "",
          interactive      = isTRUE(input$report_interactive) && input$report_format != "pdf",
          i18n_strings     = i18n_strings,
          report_language  = lang
        )

        withProgress(message=.tr_plain("Génération du rapport..."), value=0.2, {
          formats_needed <- switch(input$report_format,
            html="html_document", pdf="pdf_document",
            both=c("html_document","pdf_document"))
          out_files <- character(0)
          for (fmt in formats_needed) {
            incProgress(0.3, detail=paste("Rendu", fmt))
            ext_i    <- if (fmt=="html_document") "html" else "pdf"
            out_path <- tempfile(pattern=paste0("sc_report_",ext_i,"_"),
                                 fileext=paste0(".",ext_i))
            res <- tryCatch(
              rmarkdown::render(input=tmp_rmd, output_format=fmt, output_file=out_path,
                                params=render_params, envir=new.env(parent=globalenv()),
                                quiet=TRUE),
              error=function(e) {
                showNotification(paste0("\u274c ", fmt, ": ", conditionMessage(e)),
                                 type="error", duration=12); NULL })
            if (!is.null(res)) out_files <- c(out_files, res)
          }
          if (!length(out_files)) stop(errorCondition(.tr_plain("Aucun format généré."), class = "mod_sc_error"))
          else if (length(out_files)==1) file.copy(out_files[1], file, overwrite=TRUE)
          else zip::zip(file, files=out_files, mode="cherry-pick")
        })
      }
    )

    # =========================================================================
    # SC REPRODUCIBLE R SCRIPT
    # =========================================================================
    output$dl_sc_r_script <- downloadHandler(
      filename = function() paste0("analyse_sc_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".zip"),
      content  = function(file) {
        req(global_data$sc_obj)
        obj     <- global_data$sc_obj
        tmp_dir <- tempfile("sc_script_"); dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive=TRUE), add=TRUE)
        stamp       <- format(Sys.time(),"%Y%m%d_%H%M%S")
        script_path <- file.path(tmp_dir, paste0("analyse_sc_",stamp,".R"))
        rds_path    <- file.path(tmp_dir, "sc_obj.rds")
        writeLines(sc_r_script_text(obj, shared_rv), script_path)
        saveRDS(obj, rds_path)
        zip::zip(file, files=c(script_path, rds_path), mode="cherry-pick")
        showNotification("\u2713 Script R généré.", type="message", duration=4)
      }
    )

  }) # /moduleServer
}


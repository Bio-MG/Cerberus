# mod_spatial_viz.R
# Module d'exploration spatiale avec fond histologique multi-résolution
#
# v2 (audit step 3.9c): "Carte spatiale" tab now shows the interactive
# Plotly map and the static PNG-preview plot SIDE BY SIDE (layout_columns,
# same pattern as the "Vue combinee" tab) instead of stacked, and the
# preview defaults to VISIBLE (was hidden behind an unchecked checkbox) —
# fixes "le plot de previsualisation PNG ne s'affiche pas a cote du cluster
# spatial plotly pour comparer". The checkbox is kept so people with very
# large sketches can hide it again to save render time.

mod_spatial_viz_ui <- function(id) {
  ns <- NS(id)
  
  layout_sidebar(
    sidebar = sidebar(
      title = i18n$t("Visualisation"), width = 320,
      
      uiOutput(ns("sketch_norm_status_ui")),
      
      selectInput(
        ns("color_by"), .tr_plain("Colorer par"),
        choices = stats::setNames(
          c("qc", "cluster", "deconv", "niche", "gene"),
          c(.tr_plain("Metrique QC"),
            .tr_plain("Cluster spatial"),
            .tr_plain("Type cellulaire (deconvolution)"),
            .tr_plain("Niche spatiale"),
            .tr_plain("Gene"))
        ),
        selected = "qc"
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'qc'", ns("color_by")),
        selectInput(
          ns("qc_metric"), NULL,
          choices = c("nCount", "nFeature", "pct_mt", "pct_ribo", "log_nCount")
        )
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'gene'", ns("color_by")),
        selectizeInput(
          ns("gene"), NULL, choices = NULL,
          options = list(
            maxOptions = 3000,
            placeholder = .tr_plain("Rechercher un gene...")
          )
        ),
        # NEW: quick-pick from the Moran's I results (onglet "1. QC &
        # Autocorrelation" > "Genes spatialement variables") -- the small
        # facet grid there is useful to scan many genes at once, but picking
        # ONE gene to inspect full-size/interactively belongs here. Purely
        # a convenience selectInput that feeds the same `gene` field above;
        # NULL/hidden until Moran's I has been run at least once.
        uiOutput(ns("moran_quickpick_ui"))
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'gene'", ns("color_by")),
        checkboxInput(
          ns("scale_alpha_by_expr"),
          i18n$t("Opacite proportionnelle a l'expression (SpatialFeaturePlot)"),
          value = TRUE
        ),
        conditionalPanel(
          condition = sprintf("input['%s']", ns("scale_alpha_by_expr")),
          sliderInput(
            ns("alpha_range"), i18n$t("Plage d'opacite (min-max)"),
            0, 1, c(0.15, 1), step = 0.05
          )
        )
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'deconv'", ns("color_by")),
        uiOutput(ns("deconv_celltype_ui"))
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'cluster'", ns("color_by")),
        checkboxInput(
          ns("show_cluster_labels"),
          i18n$t("Afficher les labels de cluster sur la carte (vignette: DimPlot label=TRUE)"),
          value = FALSE
        )
      ),
      
      sliderInput(ns("pt_radius"), i18n$t("Taille des points"), 1, 20, 6, step = 1),
      sliderInput(ns("pt_opacity"), i18n$t("Opacite des points (hors mode Gene)"), 0.1, 1, 0.85, step = 0.05),

      hr(),
      tags$details(
        tags$summary(style = "cursor:pointer; font-weight:bold; font-size:0.85rem;", i18n$t("Palette de couleurs")),
        div(class = "mt-2",
            selectInput(ns("color_palette"), i18n$t("Jeu de couleurs"),
                        choices = stats::setNames(
                          c("default", "okabeito", "viridis", "set2", "manual"),
                          c(.tr_plain("Defaut"), .tr_plain("Okabe-Ito (daltonien)"),
                            "Viridis", "Set2", .tr_plain("Manuel"))
                        ),
                        selected = "default"),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'manual'", ns("color_palette")),
              div(class = "text-muted", style = "font-size:0.7rem;", i18n$t("Degrade (metriques continues : QC, gene, Z-score...)")),
              manual_color_picker_ui(ns, c("grad_low", "grad_high"), c(i18n$t("Bas"), i18n$t("Haut")), c("#2166AC", "#B2182B")),
              div(class = "text-muted mt-2", style = "font-size:0.7rem;", i18n$t("Couleurs discretes (categorie affichee actuellement) :")),
              uiOutput(ns("manual_discrete_picker_ui"))
            ),
            checkboxInput(ns("fixed_scale"), i18n$t("Echelle de couleur fixe (point fixe min/max)"), value = FALSE),
            conditionalPanel(
              condition = sprintf("input['%s']", ns("fixed_scale")),
              div(style = "display:flex; gap:8px;",
                  numericInput(ns("fixed_scale_min"), "Min", value = 0, width = "100px"),
                  numericInput(ns("fixed_scale_max"), "Max", value = 100, width = "100px"))
            )
        )
      ),
      hr(),
      
      checkboxInput(ns("show_histology"), i18n$t("Afficher l'image histologique (fond de coupe)"), value = TRUE),
      conditionalPanel(
        condition = sprintf("input['%s'] == true", ns("show_histology")),
        selectInput(
          ns("histology_resolution"), i18n$t("Résolution du fond"),
          choices = NULL,          # sera mis à jour dynamiquement côté serveur
          selected = NULL
        ),
        sliderInput(ns("histology_opacity"), i18n$t("Opacité de l'image"), 0, 1, 0.7, step = 0.05)
      ),
      
      uiOutput(ns("histology_status_ui")),
      conditionalPanel(
        condition = sprintf("input['%s'] == true", ns("show_histology")),
        downloadButton(
          ns("dl_histology_raw"), i18n$t("\U0001F41B Fond brut (debug PNG)"),
          class = "btn-sm btn-outline-secondary w-100 mb-1"
        )
      ),
      
      div(
        class = "border-top pt-2 mt-2",
        checkboxInput(
          ns("show_static_export_preview"),
          i18n$t("Afficher l'apercu PNG (a cote de la carte interactive)"),
          value = TRUE
        ),
        div(
          class = "text-muted mb-2",
          style = "font-size: 0.7rem;",
          i18n$t("Meme moteur que le bouton Export PNG ci-dessous. Decochez pour donner toute la largeur a la carte interactive Plotly.")
        ),
        downloadButton(ns("dl_png"), i18n$t("Export PNG"), class = "btn-sm btn-outline-secondary w-100 mb-1"),
        downloadButton(ns("dl_csv"), i18n$t("Export CSV (donnees affichees)"), class = "btn-sm btn-outline-secondary w-100")
      ),

      # Feedback biologiste ("ajouter la fonction d'ajout au rapport") :
      # sauvegarde la vue ACTUELLE (mode de coloration + parametres) pour
      # qu'elle apparaisse dans l'onglet "7. Export & Rapport" > "Rapport
      # HTML/PDF" > section "Visualisations sauvegardees", MEME apres avoir
      # change d'onglet ou d'echantillon (shared_rv$saved_viz_list est
      # cacheable comme le reste, voir mod_spatial.R). Seule la CONFIG est
      # sauvegardee (pas une image) -- reconstruite a la volee au moment du
      # rendu du rapport via build_saved_viz_df() (R/utils_spatial_report.R),
      # donc toujours a jour avec les derniers calculs si vous relancez une
      # etape ensuite.
      div(
        class = "border-top pt-2 mt-2",
        h6(i18n$t("Ajouter au rapport"), style = "font-weight: bold; font-size:0.85rem;"),
        div(class = "text-muted", style = "font-size:0.7rem;",
            i18n$t("Sauvegarde la vue ACTUELLE (coloration + parametres ci-dessus) pour l'onglet \"7. Export & Rapport\", meme apres avoir change d'onglet ou d'echantillon.")),
        textInput(ns("viz_save_label"), NULL, placeholder = .tr_plain("Nom de cette vue (ex: Cluster 3 vs stroma)")),
        actionButton(ns("btn_add_to_report"), paste("\u2795", i18n$t("Ajouter cette vue au rapport")),
                     class = "btn-sm btn-outline-primary w-100"),
        uiOutput(ns("saved_viz_list_ui"))
      ),
      
      hr(),
      # v4 (audit step 3.12): DEBUG-ONLY tool, collapsed by default. The
      # real root cause turned out to be the LOWRES background itself
      # (undersized/misaligned depending on Seurat version -- see
      # R/utils_spatial_io.R::extract_histology_image()), now fixed by
      # reading tissue_hires_image.png + scalefactors_json.json directly
      # from disk and defaulting to it. This toggle is kept only as an
      # escape hatch for a dataset/version where that still isn't enough --
      # it is NOT a permanent correction and should not need to be used in
      # normal operation. Scoped to the Plotly-rendering outputs only
      # (spatial_preview_plot, combined_spatial_plot, roi_zoom_plot,
      # roi_context_plot); PNG export, BANKSY clustering and Moran's I are
      # never affected by it.
      tags$details(
        tags$summary(style = "cursor:pointer; font-size:0.72rem; color:#888;",
                     i18n$t("Outil de diagnostic (debug) : orientation Plotly")),
        div(class = "mt-1",
            checkboxInput(
              ns("plotly_orient_fix"),
              i18n$t("Corriger l'orientation dans la vue interactive Plotly"),
              value = FALSE
            ),
            div(class = "text-muted", style = "font-size:0.65rem;",
                i18n$t("A utiliser seulement si le fond hires choisi ci-dessus semble encore tourne. N'affecte JAMAIS l'export PNG ni le clustering/Moran."))
        )
      ),
      
      hr(),
      bslib::input_task_button(
        ns("btn_compute_umap"),
        i18n$t("Calculer PCA + UMAP (sketch)"),
        icon = icon("chart-line")
      ),
      verbatimTextOutput(ns("umap_progress_text"), placeholder = TRUE)
    ),
    
    navset_card_underline(
      nav_panel(
        i18n$t("Carte spatiale"),
        uiOutput(ns("carte_spatiale_layout_ui"))
      ),
      
      nav_panel(
        i18n$t("UMAP (non-spatial, sketch)"),
        card(
          full_screen = TRUE,
          style = "min-height: 78vh;",
          plotly::plotlyOutput(
            ns("umap_plot"),
            height = "calc(100vh - 220px)"
          )
        )
      ),
      
      nav_panel(
        i18n$t("Vue combinee (mode expert)"),
        div(
          class = "alert alert-light mb-2",
          style = "font-size:0.82rem;",
          uiOutput(ns("combined_help_ui")),
          uiOutput(ns("highlight_debug_ui"))
        ),
        div(
          class = "d-flex flex-wrap gap-2 align-items-end mb-3",
          style = "row-gap: 0.5rem;",
          div(
            style = "min-width: 320px; flex: 1 1 320px;",
            selectizeInput(
              ns("highlight_clusters"),
              i18n$t("Isoler cluster(s)"),
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = .tr_plain("Tous les clusters"),
                plugins = list("remove_button")
              )
            )
          ),
          div(
            style = "min-width: 220px;",
            actionButton(
              ns("btn_clear_selection"),
              i18n$t("Effacer la selection lasso"),
              class = "btn btn-outline-secondary"
            )
          ),
          div(
            style = "min-width: 220px;",
            actionButton(
              ns("btn_isolate_roi"),
              i18n$t("\U0001F50D Isoler cette selection (ROI)"),
              class = "btn btn-primary"
            )
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            full_screen = TRUE,
            style = "min-height: 78vh;",
            card_header(i18n$t("Carte spatiale liee")),
            plotly::plotlyOutput(
              ns("combined_spatial_plot"),
              height = "72vh"
            )
          ),
          card(
            full_screen = TRUE,
            style = "min-height: 78vh;",
            card_header(i18n$t("UMAP lie")),
            plotly::plotlyOutput(
              ns("combined_umap_plot"),
              height = "72vh"
            )
          )
        )
      ),
      
      nav_panel(
        i18n$t("5. ROI isolee"),
        
        div(
          class = "mb-3",
          uiOutput(ns("roi_summary_ui"))
        ),
        
        div(
          class = "container-fluid px-0",
          
          div(
            class = "row g-3 align-items-stretch mb-3",
            
            div(
              class = "col-12 col-xl-6 d-flex",
              card(
                full_screen = TRUE,
                class = "w-100",
                style = "height: min(68vh, 720px); min-height: 520px;",
                card_header(i18n$t("Vue zoomee (ROI)")),
                card_body(
                  class = "p-0",
                  style = "height: calc(min(68vh, 720px) - 52px); min-height: 468px; overflow: hidden;",
                  plotly::plotlyOutput(
                    ns("roi_zoom_plot"),
                    height = "100%"
                  )
                )
              )
            ),
            
            div(
              class = "col-12 col-xl-6 d-flex",
              card(
                full_screen = TRUE,
                class = "w-100",
                style = "height: min(68vh, 720px); min-height: 520px;",
                card_header(i18n$t("Contexte (carte complete)")),
                card_body(
                  class = "p-0",
                  style = "height: calc(min(68vh, 720px) - 52px); min-height: 468px; overflow: hidden;",
                  plotly::plotlyOutput(
                    ns("roi_context_plot"),
                    height = "100%"
                  )
                )
              )
            )
          ),
          
          div(
            class = "row g-3",
            div(
              class = "col-12",
              card(
                card_header(i18n$t("Marqueurs differentiels (ROI vs Reste)")),
                card_body(
                  div(
                    class = "alert alert-light small mb-3",
                    bsicons::bs_icon("info-circle"),
                    " ",
                    i18n$t("Selectionnez une ROI depuis la Vue combinee, puis lancez le test Wilcoxon asynchrone contre le reste des spots.")
                  ),
                  
                  div(
                    class = "d-flex flex-wrap align-items-center gap-2 mb-3",
                    bslib::input_task_button(
                      ns("btn_roi_markers"),
                      i18n$t("Chercher les marqueurs"),
                      icon = icon("magnifying-glass-chart")
                    ),
                    downloadButton(
                      ns("dl_roi_object"),
                      i18n$t("\U0001F4E5 Telecharger le sous-objet (.rds)"),
                      class = "btn-outline-secondary"
                    ),
                    actionButton(
                      ns("btn_clear_roi"),
                      i18n$t("Vider la ROI"),
                      class = "btn-outline-danger"
                    )
                  ),
                  
                  div(
                    class = "bg-light border rounded p-2 mb-3",
                    style = "max-height: 110px; overflow-y: auto; white-space: pre-wrap; font-family: monospace; font-size: 0.75rem;",
                    verbatimTextOutput(
                      ns("roi_markers_progress_text"),
                      placeholder = TRUE
                    )
                  ),
                  
                  DT::DTOutput(ns("roi_markers_table"))
                )
              )
            )
          )
        )
      )
    )
  )
}

mod_spatial_viz_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Session-scoped scalar translation (plain strings, never HTML spans).
    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }
    
    # i18n: push translated labels/choices for build-time-frozen inputs on
    # every language change (values NEVER change; selection is preserved).
    observeEvent(global_data$language, {
      updateSelectInput(session, "color_by",
        label = .tr("Colorer par"),
        choices = stats::setNames(
          c("qc", "cluster", "deconv", "niche", "gene"),
          c(.tr("Metrique QC"), .tr("Cluster spatial"),
            .tr("Type cellulaire (deconvolution)"), .tr("Niche spatiale"),
            .tr("Gene"))))
      updateSliderInput(session, "alpha_range",
        label = .tr("Plage d'opacite (min-max)"))
      updateCheckboxInput(session, "scale_alpha_by_expr",
        label = .tr("Opacite proportionnelle a l'expression (SpatialFeaturePlot)"))
      updateCheckboxInput(session, "show_cluster_labels",
        label = .tr("Afficher les labels de cluster sur la carte (vignette: DimPlot label=TRUE)"))
      updateSliderInput(session, "pt_radius", label = .tr("Taille des points"))
      updateSliderInput(session, "pt_opacity",
        label = .tr("Opacite des points (hors mode Gene)"))
      updateSelectInput(session, "color_palette",
        label = .tr("Jeu de couleurs"),
        choices = stats::setNames(
          c("default", "okabeito", "viridis", "set2", "manual"),
          c(.tr("Defaut"), .tr("Okabe-Ito (daltonien)"),
            "Viridis", "Set2", .tr("Manuel"))))
      updateCheckboxInput(session, "fixed_scale",
        label = .tr("Echelle de couleur fixe (point fixe min/max)"))
      updateCheckboxInput(session, "show_histology",
        label = .tr("Afficher l'image histologique (fond de coupe)"))
      updateSelectInput(session, "histology_resolution",
        label = .tr("Résolution du fond"))
      updateSliderInput(session, "histology_opacity",
        label = .tr("Opacité de l'image"))
      updateCheckboxInput(session, "show_static_export_preview",
        label = .tr("Afficher l'apercu PNG (a cote de la carte interactive)"))
      updateTextInput(session, "viz_save_label",
        placeholder = .tr("Nom de cette vue (ex: Cluster 3 vs stroma)"))
      updateCheckboxInput(session, "plotly_orient_fix",
        label = .tr("Corriger l'orientation dans la vue interactive Plotly"))
      updateActionButton(session, "btn_add_to_report",
        label = paste("\u2795", .tr("Ajouter cette vue au rapport")))
      updateSelectizeInput(session, "highlight_clusters",
        label = .tr("Isoler cluster(s)"),
        options = list(placeholder = .tr("Tous les clusters")))
      updateActionButton(session, "btn_clear_selection",
        label = .tr("Effacer la selection lasso"))
      updateActionButton(session, "btn_isolate_roi",
        label = .tr("\U0001F50D Isoler cette selection (ROI)"))
      updateActionButton(session, "btn_clear_roi", label = .tr("Vider la ROI"))
    }, ignoreInit = TRUE)
    
    # --------------------------------------------------------------------------
    # Helper pour extraire les résolutions disponibles
    # --------------------------------------------------------------------------
    
    # --------------------------------------------------------------------------
    # Mise à jour dynamique des choix de résolution
    # --------------------------------------------------------------------------
    observe({
      hist_data <- global_data$spatial_obj$histology
      req(hist_data)
      
      res_choices <- get_available_resolutions(hist_data)
      if (length(res_choices) == 0) {
        # Si aucune résolution trouvée, on garde les valeurs par défaut
        res_choices <- c("hires", "lowres")
      }
      
      # FIX (audit step 3.12) : hires en premier / par defaut -- confirme
      # fiable en test utilisateur (lowres apparaissait systematiquement
      # trop petit/decale, voir R/utils_spatial_io.R::extract_histology_image()
      # pour le diagnostic complet). lowres reste choisissable manuellement.
      if ("hires" %in% res_choices) {
        res_choices <- c("hires", setdiff(res_choices, "hires"))
      }
      
      # Créer des noms lisibles (première lettre en majuscule)
      names(res_choices) <- tools::toTitleCase(res_choices)
      
      updateSelectInput(
        session,
        "histology_resolution",
        choices = res_choices,
        selected = if (input$histology_resolution %in% res_choices) {
          input$histology_resolution
        } else {
          res_choices[1]
        }
      )
    })
    
    # --------------------------------------------------------------------------
    # Layout reactif "Carte spatiale" (feedback UI) : la carte Plotly recupere
    # toute la largeur quand l'apercu PNG (case a cocher, deplacee dans la
    # sidebar) est decoche, au lieu de garder une colonne vide a 5/12. La case
    # elle-meme ne fait plus partie de cette carte -- voir la sidebar.
    # --------------------------------------------------------------------------
    output$carte_spatiale_layout_ui <- renderUI({
      global_data$language  # i18n: re-render on language switch
      show_png <- isTRUE(input$show_static_export_preview)
      plotly_card <- card(
        full_screen = TRUE,
        style = "min-height: 72vh;",
        card_header(.tr("Carte interactive (Plotly)")),
        card_body(
          class = "p-0",
          plotly::plotlyOutput(ns("spatial_preview_plot"), height = if (show_png) "72vh" else "80vh")
        )
      )
      if (!show_png) return(plotly_card)

      layout_columns(
        col_widths = c(7, 5),
        plotly_card,
        card(
          full_screen = TRUE,
          style = "min-height: 72vh;",
          card_header(.tr("Aperçu dynamique — export PNG")),
          card_body(
            div(
              class = "text-muted mb-2",
              style = "font-size: 0.72rem;",
              .tr("Même moteur que le bouton Export PNG. Le fond est temporairement masqué, sans crash, pendant un changement de résolution invalide.")
            ),
            plotOutput(ns("static_export_preview"), height = "calc(72vh - 65px)")
          )
        )
      )
    })

    # --------------------------------------------------------------------------
    # Statut de la normalisation du sketch
    # --------------------------------------------------------------------------
    output$sketch_norm_status_ui <- renderUI({
      global_data$language  # i18n: re-render on language switch
      req(global_data$spatial_obj$sketch)
      norm_used <- tryCatch(
        Seurat::DefaultAssay(global_data$spatial_obj$sketch),
        error = function(e) NA
      )
      label <- if (identical(norm_used, "SCT")) "SCTransform" else "LogNormalize"
      div(
        class = "alert alert-light",
        style = "font-size:0.7rem;padding:2px 8px;",
        .t_fmt(.tr("Normalisation du sketch : {norm}"), norm = label)
      )
    })
    
    # --------------------------------------------------------------------------
    # Overlay histologique (nouvelle version multi-résolution)
    # --------------------------------------------------------------------------
    histology_overlay <- reactive({
      hist_data <- global_data$spatial_obj$histology
      if (is.null(hist_data)) return(NULL)
      
      requested_resolution <- input$histology_resolution %||% "hires"
      
      hist_img <- tryCatch(
        get_histology_raster(
          hist_data = hist_data,
          resolution = requested_resolution
        ),
        error = function(e) {
          warning(
            "Lecture du fond histologique echouee : ",
            conditionMessage(e),
            call. = FALSE
          )
          NULL
        }
      )
      
      if (
        is.null(hist_img) ||
        is.null(hist_img$raw_raster) ||
        is.null(hist_img$rgba) ||
        is.null(hist_img$dim) ||
        length(hist_img$dim) < 2L
      ) {
        return(NULL)
      }
      
      scale_factor <- hist_img$scale_factor %||% 1
      if (!is.finite(scale_factor) || scale_factor <= 0) scale_factor <- 1
      
      bounds <- list(
        x = c(0, hist_img$dim[2L] / scale_factor),
        y = c(0, hist_img$dim[1L] / scale_factor)
      )
      
      data_uri <- tryCatch({
        raw_png <- png::writePNG(hist_img$rgba)
        
        if (requireNamespace("base64enc", quietly = TRUE)) {
          uri <- base64enc::dataURI(raw_png, mime = "image/png")
        } else if (requireNamespace("jsonlite", quietly = TRUE)) {
          uri <- paste0(
            "data:image/png;base64,",
            jsonlite::base64_enc(raw_png)
          )
        } else {
          NULL
        }
        
        # Nettoyage des retours à la ligne pour Plotly
        if (!is.null(uri)) {
          uri <- gsub("[\r\n[:space:]]+", "", uri)
        }
        
        uri
      }, error = function(e) {
        warning(
          "Encodage data URI histologique echoue : ",
          conditionMessage(e),
          call. = FALSE
        )
        NULL
      })
      
      list(
        raster_obj = hist_img$raw_raster,
        rgba = hist_img$rgba,
        data_uri = data_uri,
        bounds = bounds,
        resolution = hist_img$resolution,
        scale_factor = scale_factor,
        diag = tryCatch(
          diagnose_histology_raster(hist_img$raw_raster),
          error = function(e) NULL
        )
      )
    })
    
    # --------------------------------------------------------------------------
    # Statut de l'histologie (UI)
    # --------------------------------------------------------------------------
    output$histology_status_ui <- renderUI({
      global_data$language  # i18n: re-render on language switch
      req(global_data$spatial_obj)
      
      hist_data <- global_data$spatial_obj$histology
      if (is.null(hist_data)) {
        return(
          div(
            class = "alert alert-light",
            style = "font-size:0.7rem;padding:2px 6px;",
            .tr("Image histologique indisponible.")
          )
        )
      }
      
      ov <- histology_overlay()
      
      if (is.null(ov) || is.null(ov$raster_obj)) {
        return(
          div(
            class = "alert alert-warning",
            style = "font-size:0.7rem;padding:2px 6px;",
            .tr("Le fond histologique n'a pas pu etre prepare pour cette resolution.")
          )
        )
      }
      
      if (is.null(ov$data_uri)) {
        return(
          div(
            class = "alert alert-warning",
            style = "font-size:0.7rem;padding:2px 6px;",
            .tr("Fond disponible pour l'export PNG, mais echec de l'encodage Plotly.")
          )
        )
      }
      
      div(
        class = "text-muted",
        style = "font-size:0.65rem;padding:2px 6px;",
        sprintf(
          .tr("Fond histologique OK — %s — %d x %d px — %.0f Ko"),
          ov$resolution,
          dim(ov$raster_obj)[2L],
          dim(ov$raster_obj)[1L],
          nchar(ov$data_uri) / 1024
        )
      )
    })
    
    # --------------------------------------------------------------------------
    # Téléchargement du fond brut (PNG)
    # --------------------------------------------------------------------------
    output$dl_histology_raw <- downloadHandler(
      filename = function() {
        paste0(
          "fond_histologique_",
          input$histology_resolution %||% "hires",
          "_",
          Sys.Date(),
          ".png"
        )
      },
      content = function(file) {
        hist_data <- global_data$spatial_obj$histology
        
        hist_img <- get_histology_raster(
          hist_data = hist_data,
          resolution = input$histology_resolution %||% "hires"
        )
        
        validate(
          need(
            !is.null(hist_img) && !is.null(hist_img$rgba),
            .tr("Aucune image histologique disponible.")
          )
        )
        
        png::writePNG(hist_img$rgba, file)
      }
    )
    
    # --------------------------------------------------------------------------
    # Fonction de construction d'une image Plotly à partir de l'overlay
    # Renvoie un objet image unique (list) ou NULL
    # --------------------------------------------------------------------------
    # ========================================================================
    #  make_plotly_histology_image (remplacé)
    # ========================================================================
    
    # ========================================================================
    #  compute_spatial_ranges (remplacé)
    # ========================================================================
    
    # --------------------------------------------------------------------------
    # Correctif d'orientation SCOPE PLOTLY UNIQUEMENT (audit step 3.11)
    # --------------------------------------------------------------------------
    # Applies ONLY where called explicitly below (spatial_preview_plot,
    # combined_spatial_df -> combined_spatial_plot/roi_zoom_plot/
    # roi_context_plot). Does NOT touch plot_df() itself, so build_raster_plot()
    # (static_export_preview + dl_png) and $coords (BANKSY/Moran, read
    # directly from global_data$spatial_obj$coords, never from this module)
    # are unaffected -- see sidebar checkbox comment for the diagnosis that
    # led here.
    swap_for_plotly <- function(df) {
      if (!isTRUE(input$plotly_orient_fix)) return(df)
      tmp <- df$x
      df$x <- df$y
      df$y <- tmp
      df
    }
    
    histology_overlay_plotly <- reactive({
      ov <- histology_overlay()
      if (is.null(ov) || !isTRUE(input$plotly_orient_fix)) return(ov)
      b <- ov$bounds
      ov$bounds <- list(x = b$y, y = b$x)
      ov
    })
    
    
    
    # --------------------------------------------------------------------------
    # Safe histology helpers for static ggplot rendering
    # --------------------------------------------------------------------------
    
    
    #' Convert an RGBA image to a ggplot-compatible raster safely
    #'
    #' @param hist_ov Histology overlay returned by histology_overlay().
    #' @param max_pixels Maximum number of pixels accepted for static rendering.
    #' @return A raster object or NULL.
    
    #' Guarantee finite non-degenerate coordinate ranges for ggplot/Plotly
    #'
    #' @param x Numeric vector.
    #' @param fallback Numeric length-two fallback range.
    #' @return Numeric length-two range.
    
    # --------------------------------------------------------------------------
    # Fonction de diagnostic (déjà présente)
    # --------------------------------------------------------------------------
    
    # --------------------------------------------------------------------------
    # Mise à jour de la liste des gènes
    # --------------------------------------------------------------------------
    observeEvent(global_data$spatial_obj, {
      req(global_data$spatial_obj$sketch)
      updateSelectizeInput(
        session,
        "gene",
        choices = rownames(global_data$spatial_obj$sketch),
        server = TRUE
      )
    }, ignoreInit = TRUE)

    # --------------------------------------------------------------------------
    # Raccourci "genes spatialement variables (Moran's I)" -- alimente le
    # champ de recherche de gene ci-dessus a partir des resultats calcules
    # dans l'onglet "1. QC & Autocorrelation". Cache tant que Moran's I n'a
    # jamais ete lance pour l'echantillon actif (shared_rv$moran_results
    # est deja reinitialise au changement d'echantillon par mod_spatial.R).
    # --------------------------------------------------------------------------
    output$moran_quickpick_ui <- renderUI({
      global_data$language  # i18n: re-render on language switch
      req(shared_rv$moran_results, nrow(shared_rv$moran_results) > 0)
      top <- shared_rv$moran_results[order(-shared_rv$moran_results$moran_i), ]
      top <- top[!is.na(top$gene), , drop = FALSE]
      choices <- stats::setNames(
        top$gene,
        sprintf("%s (I = %.2f)", top$gene, top$moran_i)
      )
      tagList(
        selectInput(
          ns("moran_gene_pick"),
          .tr("Raccourci : gene spatialement variable (indice de Moran)"),
          choices = c(stats::setNames("", .tr("\u2014 choisir parmi le top Moran's I \u2014")),
                     utils::head(choices, 100))
        ),
        div(class = "text-muted", style = "font-size:0.68rem; margin-top:-8px;",
            .tr("Classes par indice de Moran decroissant (calcule onglet 1)."))
      )
    })

    observeEvent(input$moran_gene_pick, {
      req(nzchar(input$moran_gene_pick), global_data$spatial_obj$sketch)
      g <- input$moran_gene_pick
      all_genes <- rownames(global_data$spatial_obj$sketch)
      if (!g %in% all_genes) {
        showNotification(.t_fmt(.tr("Gene '{g}' introuvable dans le sketch actuel."), g = g),
                         type = "warning", duration = 6)
        return()
      }
      # ROBUSTESSE (feedback biologiste : "certains genes ne sont pas
      # synchronises") : pour un selectizeInput server-side, appeler
      # updateSelectizeInput() avec seulement `selected=` ne garantit pas que
      # le client affiche correctement une valeur qu'il n'a encore JAMAIS vue
      # via une recherche ajax (limitation connue de selectize en mode
      # server=TRUE). Re-fournir le jeu de choix COMPLET avec `server=TRUE`
      # a chaque appel enregistre `g` de facon fiable tout en preservant la
      # recherche libre ensuite (meme liste qu'a l'initialisation).
      updateSelectizeInput(session, "gene", choices = all_genes, selected = g, server = TRUE)
      # Le champ "gene" n'etait utile que si color_by == "gene" -- sans ce
      # changement, choisir un gene ici ne se "voyait" nulle part tant que
      # l'utilisateur ne changeait pas MANUELLEMENT "Colorer par" -> "Gene".
      updateSelectInput(session, "color_by", selected = "gene")
    }, ignoreInit = TRUE)

    # --------------------------------------------------------------------------
    # "Ajouter au rapport" -- sauvegarde la CONFIG de la vue actuelle (pas une
    # image) dans shared_rv$saved_viz_list, cacheable/restaurable par
    # mod_spatial.R comme tout le reste. Rendue par le rapport (onglet 7) via
    # build_saved_viz_df() (R/utils_spatial_report.R).
    # --------------------------------------------------------------------------
    observeEvent(input$btn_add_to_report, {
      label <- trimws(input$viz_save_label %||% "")
      if (!nzchar(label)) label <- sprintf("Vue %s", format(Sys.time(), "%H:%M:%S"))
      cfg <- list(
        color_by = input$color_by, qc_metric = input$qc_metric %||% "nCount",
        gene = input$gene, deconv_celltype = input$deconv_celltype,
        show_cluster_labels = isTRUE(input$show_cluster_labels), created_at = Sys.time()
      )
      current <- shared_rv$saved_viz_list %||% list()
      current[[label]] <- cfg
      shared_rv$saved_viz_list <- current
      showNotification(.t_fmt(.tr("Vue '{label}' ajoutee au rapport."), label = label),
                       type = "message", duration = 4)
      updateTextInput(session, "viz_save_label", value = "")
    })


    output$saved_viz_list_ui <- renderUI({
      global_data$language  # i18n: re-render on language switch
      lst <- shared_rv$saved_viz_list %||% list()
      if (length(lst) == 0) {
        return(div(class = "text-muted", style = "font-size:0.7rem; margin-top:4px;",
                   .tr("Aucune vue sauvegardee pour cet echantillon.")))
      }
      tagList(
        tags$ul(
          style = "font-size:0.72rem; padding-left:0.2rem; margin-top:4px; list-style:none;",
          lapply(names(lst), function(nm) {
            tags$li(style = "display:flex; justify-content:space-between; align-items:center; gap:6px; padding:1px 0;",
                    tags$span(nm),
                    tags$a(href = "#", class = "text-danger", title = .tr("Supprimer cette vue"),
                           style = "text-decoration:none; font-weight:bold;",
                           onclick = sprintf(
                             "Shiny.setInputValue('%s', {name: '%s', ts: Date.now()}, {priority:'event'}); return false;",
                             ns("viz_delete_target"), .esc_js(nm)
                           ),
                           "\u2715"))
          })
        ),
        actionLink(ns("btn_clear_saved_viz"), .tr("Vider toutes les vues sauvegardees"), style = "font-size:0.7rem;")
      )
    })

    # (b) Un seul input partage (JS passe {name:}) -- pas d'observer
    # dynamique par vue, donc pas de fuite d'observers au fil des ajouts.
    observeEvent(input$viz_delete_target, {
      target <- input$viz_delete_target$name
      req(nzchar(target %||% ""))
      current <- shared_rv$saved_viz_list %||% list()
      current[[target]] <- NULL
      shared_rv$saved_viz_list <- current
    })

    observeEvent(input$btn_clear_saved_viz, {
      shared_rv$saved_viz_list <- list()
    })
    
    # ── Palette PARTAGEE app-wide (vague 6) : ce module reste le panneau de
    # controle central ; toute autre vue Spatial lit shared_rv$color_palette /
    # manual_gradient / manual_discrete au lieu de recalculer sa propre palette.
    observeEvent(list(input$color_palette, input$grad_low, input$grad_high), {
      shared_rv$color_palette <- input$color_palette %||% "default"
      shared_rv$manual_gradient <- list(low = input$grad_low %||% "#2166AC", mid = "white",
                                        high = input$grad_high %||% "#B2182B")
    }, ignoreInit = FALSE)
    
    .current_discrete_kind <- reactive({
      switch(input$color_by %||% "qc", "cluster" = "cluster", "niche" = "niche", "deconv" = "celltype", NULL)
    })
    
    output$manual_discrete_picker_ui <- renderUI({
      kind <- .current_discrete_kind()
      req(kind)
      levels <- switch(kind,
                       "cluster" = if (!is.null(shared_rv$cluster_labels)) shared_rv$cluster_labels else NULL,
                       "niche"   = if (!is.null(shared_rv$niche_labels)) shared_rv$niche_labels else NULL,
                       "celltype" = if (!is.null(shared_rv$deconv_props)) setdiff(colnames(shared_rv$deconv_props), "id") else NULL
      )
      req(levels)
      levels <- sort(unique(as.character(levels)))
      defaults <- spatial_discrete_colors(levels, shared_rv, kind = kind)
      dynamic_manual_color_picker_ui(ns, kind, levels,
                                     current_colors = (shared_rv$manual_discrete %||% list())[[kind]],
                                     defaults = defaults)
    })
    
    lapply(c("cluster", "niche", "celltype", "dataset"), function(kind) {
      observeEvent(input[[paste0("manual_discrete_", kind, "_change")]], {
        ev <- input[[paste0("manual_discrete_", kind, "_change")]]
        req(ev$level, ev$color)
        md <- shared_rv$manual_discrete %||% list()
        cur <- md[[kind]] %||% stats::setNames(character(0), character(0))
        cur[ev$level] <- ev$color
        md[[kind]] <- cur
        shared_rv$manual_discrete <- md
      }, ignoreInit = TRUE)
    })
    
    # --------------------------------------------------------------------------
    # UI du type cellulaire pour la déconvolution
    # --------------------------------------------------------------------------
    output$deconv_celltype_ui <- renderUI({
      req(shared_rv$deconv_props)
      cts <- setdiff(colnames(shared_rv$deconv_props), "id")
      selectInput(ns("deconv_celltype"), NULL, choices = cts)
    })
    
    # FIX (crash "cannot coerce type 'closure'..."): relancer la deconvolution
    # avec une AUTRE methode change les noms de colonnes (RCTD garde les noms
    # bruts "T_CD4+_1" ; TransferData() les fait passer par la convention
    # Seurat qui remplace "_" par "-", ex "T-CD4+-1") -- input$deconv_celltype
    # cote serveur peut rester bloque sur l'ancienne valeur le temps d'un
    # aller-retour navigateur. Force une selection valide des que les
    # colonnes disponibles changent.
    observeEvent(shared_rv$deconv_props, {
      cts <- setdiff(colnames(shared_rv$deconv_props), "id")
      if (length(cts) == 0) return()
      if (is.null(input$deconv_celltype) || !input$deconv_celltype %in% cts) {
        updateSelectInput(session, "deconv_celltype", choices = cts, selected = cts[1])
      }
    }, ignoreInit = TRUE)
    
    # --------------------------------------------------------------------------
    # Données à afficher (plot_df)
    # --------------------------------------------------------------------------
    plot_df <- reactive({
      req(global_data$spatial_obj$sketch, global_data$spatial_obj$coords)
      sk_ids <- colnames(global_data$spatial_obj$sketch)
      coords <- global_data$spatial_obj$coords
      df <- coords[match(sk_ids, coords$id), c("id", "x", "y")]
      
      df$value <- tryCatch(
        switch(input$color_by,
               "cluster" = {
                 req(shared_rv$cluster_labels)
                 as.character(shared_rv$cluster_labels[df$id])
               },
               #ajout niche labels
               "niche" = {
                 req(shared_rv$niche_labels)
                 as.character(shared_rv$niche_labels[df$id])
               },
               "deconv" = {
                 req(shared_rv$deconv_props, input$deconv_celltype)
                 req(input$deconv_celltype %in% colnames(shared_rv$deconv_props))
                 m <- match(df$id, shared_rv$deconv_props$id)
                 shared_rv$deconv_props[[input$deconv_celltype]][m]
               },
               "qc" = {
                 req(shared_rv$qc_metrics)
                 m <- match(df$id, shared_rv$qc_metrics$id)
                 shared_rv$qc_metrics[[input$qc_metric]][m]
               },
               "gene" = {
                 req(input$gene)
                 sk <- global_data$spatial_obj$sketch
                 if (!"data" %in% SeuratObject::Layers(sk)) {
                   sk <- Seurat::NormalizeData(sk, verbose = FALSE)
                 }
                 as.numeric(SeuratObject::LayerData(sk, layer = "data")[input$gene, df$id])
               }
        ),
        error = function(e) {
          if (inherits(e, "shiny.silent.error")) stop(e)
          rep(NA_real_, nrow(df))
        }
      )
      
      df[stats::complete.cases(df[, c("x", "y")]), ]
    })
    
    # --------------------------------------------------------------------------
    # Échelle d'opacité pour l'expression génique
    # --------------------------------------------------------------------------
    
    # --------------------------------------------------------------------------
    # Tri des labels de clusters
    # --------------------------------------------------------------------------
    
    # Couleur discrete partagee cluster/niche/generique. "default" renvoie
    # NULL depuis sc_discrete_colors() (R/palettes.R) -> repli Dark 3
    # inchange si l'utilisateur ne touche pas au selecteur.
    # (La 1ere definition morte de discrete_palette_colors() a ete supprimee :
    # la seconde, ci-dessous, ecrasait deja la premiere a chaque lancement.)
    cluster_palette <- function(cluster_vec) spatial_discrete_colors(cluster_vec, shared_rv, kind = "cluster")
    discrete_palette_colors <- function(lv) spatial_discrete_colors(lv, shared_rv, kind = if (identical(input$color_by, "niche")) "niche" else if (identical(input$color_by, "deconv")) "celltype" else "cluster")
    color_values <- function(df) {
      n <- nrow(df)
      if (n == 0L) return(character(0))

      add_alpha <- function(cols, alpha) {
        cols <- as.character(cols)
        alpha <- as.numeric(alpha)
        if (length(alpha) != length(cols)) alpha <- rep(1, length(cols))
        cols[is.na(cols) | !nzchar(cols)] <- "#CCCCCC"
        alpha[!is.finite(alpha)] <- 1
        alpha <- pmax(0, pmin(1, alpha))
        rgb <- grDevices::col2rgb(cols, alpha = FALSE)
        grDevices::rgb(red = rgb[1, ], green = rgb[2, ], blue = rgb[3, ],
                       alpha = round(alpha * 255), maxColorValue = 255)
      }

      if (is.numeric(df$value)) {
        valid <- is.finite(df$value)
        if (!any(valid)) return(rep("#CCCCCC", n))

        domain <- if (isTRUE(input$fixed_scale) &&
                      is.finite(input$fixed_scale_min %||% NA_real_) &&
                      is.finite(input$fixed_scale_max %||% NA_real_) &&
                      input$fixed_scale_max > input$fixed_scale_min) {
          c(input$fixed_scale_min, input$fixed_scale_max)
        } else range(df$value[valid])

        # Rampe 256 stops factorisee dans R/palettes.R::spatial_color_ramp_hex()
        # (meme logique, meme repli "viridis" pour la palette Defaut).
        pal <- leaflet::colorNumeric(palette = spatial_color_ramp_hex(shared_rv), domain = domain, na.color = "#CCCCCC")
        cols <- as.character(pal(df$value))
        cols[!valid | is.na(cols) | !nzchar(cols)] <- "#CCCCCC"

        if (identical(input$color_by, "gene") && isTRUE(input$scale_alpha_by_expr)) {
          alpha <- scale_alpha_by_value(df$value, input$alpha_range %||% c(0.15, 1))
          return(add_alpha(cols, alpha))
        }
        return(cols)
      }

      vals <- as.character(df$value)
      vals[is.na(vals) | !nzchar(vals)] <- NA_character_

      if (identical(input$color_by, "cluster")) {
        pal <- cluster_palette(vals)
        cols <- unname(pal[vals])
        cols[is.na(cols) | !nzchar(cols)] <- "#CCCCCC"
        return(cols)
      }

      lv <- sort(unique(stats::na.omit(vals)))
      if (length(lv) == 0L) return(rep("#CCCCCC", n))
      pal <- discrete_palette_colors(lv)
      cols <- unname(pal[vals])
      cols[is.na(cols) | !nzchar(cols)] <- "#CCCCCC"
      cols
    }
    
    # --------------------------------------------------------------------------
    # Carte spatiale interactive (Plotly)
    # --------------------------------------------------------------------------
    output$spatial_preview_plot <- plotly::renderPlotly({
      df <- plot_df()
      req(nrow(df) > 0)
      df <- swap_for_plotly(df)
      
      df_plot <- df
      df_plot$y_display <- -df_plot$y
      df_plot$colour <- color_values(df_plot)
      
      hist_ov <- histology_overlay_plotly()
      show_hist <- !is.null(hist_ov) && !is.null(hist_ov$data_uri) && isTRUE(input$show_histology)
      
      max_preview_cells <- if (isTRUE(show_hist)) 40000L else 150000L
      if (nrow(df_plot) > max_preview_cells) {
        set.seed(1)
        df_plot <- df_plot[sample.int(nrow(df_plot), max_preview_cells), , drop = FALSE]
      }
      
      img_single <- if (show_hist) {
        make_plotly_histology_image(
          hist_ov = hist_ov,
          opacity = input$histology_opacity %||% 0.7
        )
      } else {
        NULL
      }
      
      plot_images <- if (!is.null(img_single)) list(img_single) else NULL
      
      spatial_ranges <- compute_spatial_ranges(
        df_all = df,
        hist_ov = hist_ov,
        show_hist = show_hist
      )
      
      trace_type <- if (isTRUE(show_hist)) "scatter" else "scattergl"
      
      p <- plotly::plot_ly(
        data = df_plot,
        x = ~x,
        y = ~y_display,
        key = ~id,
        type = trace_type,
        mode = "markers",
        marker = list(
          color = ~colour,
          size = input$pt_radius,
          opacity = input$pt_opacity %||% 0.85
        ),
        text = ~paste0(
          "ID: ", id,
          "<br>x: ", round(x, 1),
          "<br>y: ", round(-y_display, 1),
          "<br>Valeur: ", ifelse(is.na(value), "NA",
                                 if (is.numeric(value)) round(value, 3) else as.character(value)
          )
        ),
        hoverinfo = "text",
        source = ns("spatial_preview_src")
      ) |>
        plotly::layout(
          dragmode = "pan",
          margin = list(l = 10, r = 10, t = 10, b = 10),
          images = plot_images,
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)",
          xaxis = list(
            title = "",
            zeroline = FALSE,
            showgrid = FALSE,
            range = spatial_ranges$x
          ),
          yaxis = list(
            title = "",
            zeroline = FALSE,
            showgrid = FALSE,
            scaleanchor = "x",
            scaleratio = 1,
            range = spatial_ranges$y
          )
        )
      
      if (
        identical(input$color_by, "cluster") &&
        isTRUE(input$show_cluster_labels) &&
        is.character(df_plot$value)
      ) {
        centroids <- stats::aggregate(cbind(x, y_display) ~ value, data = df_plot, FUN = mean)
        p <- p |>
          plotly::add_annotations(
            x = centroids$x,
            y = centroids$y_display,
            text = centroids$value,
            showarrow = FALSE,
            font = list(size = 13, color = "#1a1a1a"),
            bgcolor = "rgba(255,255,255,0.65)",
            bordercolor = "#666",
            borderwidth = 0.5
          )
      }
      
      p <- plotly::event_register(p, "plotly_selected")
      p
    })
    
    # --------------------------------------------------------------------------
    # Construction du rendu statique (ggplot) – corrigé pour utiliser le raster
    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # Shared static plot builder: preview and exported PNG use the same object
    # --------------------------------------------------------------------------
    
    build_raster_plot <- function(df) {
      validate(
        shiny::need(is.data.frame(df) && nrow(df) > 0L, .tr("Aucune coordonnée spatiale exploitable.")),
        shiny::need(
          all(c("x", "y", "value") %in% colnames(df)),
          .tr("Les colonnes x, y ou value sont absentes.")
        )
      )
      
      df <- df[
        is.finite(df$x) & is.finite(df$y),
        ,
        drop = FALSE
      ]
      
      validate(
        shiny::need(nrow(df) > 0L, .tr("Aucune coordonnée spatiale finie à afficher."))
      )
      
      use_alpha_scale <- identical(input$color_by, "gene") &&
        isTRUE(input$scale_alpha_by_expr) &&
        is.numeric(df$value)
      
      p <- if (use_alpha_scale) {
        ggplot2::ggplot(
          df,
          ggplot2::aes(x = x, y = -y, color = value, alpha = value)
        )
      } else {
        ggplot2::ggplot(
          df,
          ggplot2::aes(x = x, y = -y, color = value)
        )
      }
      
      hist_ov <- histology_overlay()
      static_raster <- if (isTRUE(input$show_histology)) {
        safe_static_histology_raster(hist_ov)
      } else {
        NULL
      }
      
      show_hist <- !is.null(static_raster) && is_valid_histology_overlay(hist_ov)
      
      if (isTRUE(input$show_histology) && !show_hist) {
        # Deliberately do not stop the preview: spots remain visible while a
        # resolution is being recomputed or when a raster is too large for RAM.
        showNotification(
          .tr("Fond histologique temporairement indisponible pour l'aperçu PNG ; les spots restent affichés."),
          type = "warning",
          duration = 4
        )
      }
      
      if (show_hist) {
        b <- hist_ov$bounds
        
        p <- p + ggplot2::annotation_raster(
          raster = static_raster,
          xmin = b$x[1L],
          xmax = b$x[2L],
          ymin = -b$y[2L],
          ymax = -b$y[1L],
          interpolate = TRUE
        )
      }
      
      point_alpha <- input$pt_opacity %||% 0.85
      
      if (requireNamespace("scattermore", quietly = TRUE)) {
        p <- if (use_alpha_scale) {
          p + scattermore::geom_scattermore(
            pointsize = input$pt_radius
          )
        } else {
          p + scattermore::geom_scattermore(
            pointsize = input$pt_radius,
            alpha = point_alpha
          )
        }
      } else {
        p <- if (use_alpha_scale) {
          p + ggplot2::geom_point(
            size = input$pt_radius / 4
          )
        } else {
          p + ggplot2::geom_point(
            size = input$pt_radius / 4,
            alpha = point_alpha
          )
        }
      }
      
      if (use_alpha_scale) {
        p <- p + ggplot2::scale_alpha_continuous(
          range = input$alpha_range %||% c(0.15, 1),
          guide = "none"
        )
      }
      
      spatial_ranges <- compute_spatial_ranges(
        df_all = df,
        hist_ov = if (show_hist) hist_ov else NULL,
        show_hist = show_hist
      )
      
      spatial_ranges$x <- safe_plot_range(spatial_ranges$x)
      spatial_ranges$y <- safe_plot_range(spatial_ranges$y)
      
      p <- p +
        ggplot2::coord_fixed(
          xlim = spatial_ranges$x,
          ylim = spatial_ranges$y,
          expand = FALSE
        ) +
        ts_theme("void")
      
      if (is.numeric(df$value)) {
        limits <- if (isTRUE(input$fixed_scale) &&
                      is.finite(input$fixed_scale_min %||% NA_real_) &&
                      is.finite(input$fixed_scale_max %||% NA_real_) &&
                      input$fixed_scale_max > input$fixed_scale_min) {
          c(input$fixed_scale_min, input$fixed_scale_max)
        } else NULL

        if (identical(input$color_palette %||% "default", "manual")) {
          p + ggplot2::scale_color_gradient(
            low = input$grad_low %||% "#2166AC", high = input$grad_high %||% "#B2182B",
            limits = limits, na.value = "#CCCCCC"
          )
        } else {
          p + ggplot2::scale_color_viridis_c(na.value = "#CCCCCC", limits = limits)
        }
      } else {
        labels <- sort_cluster_labels(df$value)
        palette <- discrete_palette_colors(labels)
        p + ggplot2::scale_color_manual(values = palette, na.value = "#CCCCCC")
      }
    }
    
    # --------------------------------------------------------------------------
    # Shared static plot reactive
    # --------------------------------------------------------------------------
    
    static_export_plot <- reactive({
      req(isTRUE(input$show_static_export_preview))
      
      df <- plot_df()
      req(nrow(df) > 0L)
      
      tryCatch(
        build_raster_plot(df),
        error = function(e) {
          if (!inherits(e, "shiny.silent.error")) {
            showNotification(
              paste(
                .tr("Aperçu PNG non généré :"),
                conditionMessage(e)
              ),
              type = "warning",
              duration = 6
            )
          }
          
          NULL
        }
      )
    })
    
    # --------------------------------------------------------------------------
    # Static PNG preview
    # --------------------------------------------------------------------------
    
    output$static_export_preview <- renderPlot(
      {
        global_data$language  # i18n: re-render on language switch
        p <- static_export_plot()
        
        validate(
          shiny::need(
            !is.null(p),
            .tr("Préparation de l'aperçu PNG…")
          )
        )
        
        print(p)
      },
      res = 110,
      bg = "white",
      execOnResize = FALSE
    )
    
    # --------------------------------------------------------------------------
    # PNG export: same rendering engine as the on-screen preview
    # --------------------------------------------------------------------------
    
    output$dl_png <- downloadHandler(
      filename = function() {
        paste0(
          "carte_spatiale_",
          input$color_by,
          "_",
          input$histology_resolution %||% "sans_fond",
          "_",
          Sys.Date(),
          ".png"
        )
      },
      content = function(file) {
        df <- plot_df()
        
        validate(
          shiny::need(nrow(df) > 0L, .tr("Aucune donnée à exporter."))
        )
        
        p <- build_raster_plot(df)
        
        ts_export_plot(
          filename = file,
          plot = p,
          width = 8,
          height = 8,
          dpi = 200,
          bg = "white"
        )
      }
    )
    
    # --------------------------------------------------------------------------
    # Téléchargement CSV
    # --------------------------------------------------------------------------
    output$dl_csv <- downloadHandler(
      filename = function() paste0("carte_spatiale_", input$color_by, "_", Sys.Date(), ".csv"),
      content = function(file) {
        df <- plot_df()
        validate(need(nrow(df) > 0, .tr("Aucune donnee a exporter.")))
        write.csv(df, file, row.names = FALSE)
      }
    )
    
    # --------------------------------------------------------------------------
    # UMAP asynchrone
    # --------------------------------------------------------------------------
    log_file <- spatial_log_path(session, "sketch_umap")
    tracker <- create_reactive_tracker(session, log_file)
    
    umap_task <- ExtendedTask$new(function(sketch_path, log_file) {
      mirai::mirai(
        {
          write_mirai_log(log_file, "Chargement du sketch...", 1, 5)
          sk <- readRDS(sketch_path)
          
          already_sct <- identical(Seurat::DefaultAssay(sk), "SCT")
          
          if (already_sct) {
            write_mirai_log(log_file, "Sketch deja normalise (SCTransform) — HVG/scale.data reutilises.", 2, 5)
          } else {
            write_mirai_log(log_file, "Normalisation + selection des HVG...", 2, 5)
            if (!"data" %in% SeuratObject::Layers(sk)) sk <- Seurat::NormalizeData(sk, verbose = FALSE)
            sk <- Seurat::FindVariableFeatures(sk, verbose = FALSE)
            sk <- Seurat::ScaleData(sk, verbose = FALSE)
          }
          
          write_mirai_log(log_file, "PCA...", 3, 5)
          sk <- Seurat::RunPCA(sk, npcs = 30, verbose = FALSE)
          
          write_mirai_log(log_file, "UMAP...", 4, 5)
          sk <- Seurat::RunUMAP(sk, dims = 1:30, verbose = FALSE)
          
          write_mirai_log(log_file, "Termine.", 5, 5)
          emb <- as.data.frame(Seurat::Embeddings(sk, "umap"))
          colnames(emb)[1:2] <- c("dim1", "dim2")
          emb$id <- rownames(emb)
          emb
        },
        sketch_path = sketch_path,
        log_file = log_file,
        .timeout = MIRAI_TASK_TIMEOUT_MS
      )
    })
    bslib::bind_task_button(umap_task, "btn_compute_umap")
    
    observeEvent(input$btn_compute_umap, {
      req(global_data$spatial_obj$sketch)
      reset_log(log_file)
      tmp <- tempfile(fileext = ".rds")
      saveRDS(global_data$spatial_obj$sketch, tmp)
      umap_task$invoke(sketch_path = tmp, log_file = log_file)
    })
    
    observeEvent(umap_task$status(), {
      if (umap_task$status() == "success") {
        shared_rv$umap_df <- umap_task$result()
        showNotification(.tr("UMAP calcule."), type = "message", duration = 3)
      } else if (umap_task$status() == "error") {
        showNotification(.tr("Erreur pendant le calcul UMAP."), type = "error", duration = 10)
      }
    })
    
    output$umap_progress_text <- renderText({
      global_data$language  # i18n: re-render on language switch
      lines <- tracker()
      if (length(lines) == 0) return(.tr("En attente..."))
      paste(lines, collapse = "\n")
    })
    
    # --------------------------------------------------------------------------
    # Graphique UMAP
    # --------------------------------------------------------------------------
    output$umap_plot <- plotly::renderPlotly({
      req(shared_rv$umap_df)
      emb <- shared_rv$umap_df
      emb$cluster <- if (!is.null(shared_rv$cluster_labels)) {
        as.character(shared_rv$cluster_labels[emb$id])
      } else {
        "sketch"
      }
      
      plotly::plot_ly(
        emb,
        x = ~dim1,
        y = ~dim2,
        color = ~cluster,
        colors = cluster_palette(emb$cluster),
        type = "scattergl",
        mode = "markers",
        marker = list(size = 5, opacity = 0.7)
      ) |>
        plotly::layout(
          margin = list(l = 20, r = 20, t = 20, b = 20),
          autosize = TRUE
        )
    })
    
    # --------------------------------------------------------------------------
    # Mode expert : sélection liée
    # --------------------------------------------------------------------------
    observeEvent(shared_rv$cluster_labels, {
      req(shared_rv$cluster_labels)
      updateSelectizeInput(
        session,
        "highlight_clusters",
        choices = sort_cluster_labels(shared_rv$cluster_labels),
        server = FALSE
      )
    }, ignoreInit = FALSE)
    
    linked_selection <- reactiveVal(character(0))
    
    selected_event_ids <- function(source_id) {
      ev <- tryCatch(
        suppressWarnings(plotly::event_data(
          event = "plotly_selected",
          source = source_id,
          session = session,
          priority = "event"
        )),
        error = function(e) NULL
      )
      
      if (is.null(ev) || nrow(ev) == 0L || !"key" %in% colnames(ev)) {
        return(character(0))
      }
      
      unique(as.character(ev$key[!is.na(ev$key)]))
    }
    
    observeEvent(
      suppressWarnings(plotly::event_data(
        event = "plotly_selected",
        source = ns("combined_spatial_src"),
        session = session,
        priority = "event"
      )),
      {
        req(output$combined_spatial_plot)

        ids <- selected_event_ids(ns("combined_spatial_src"))
        if (length(ids) > 0L) linked_selection(ids)
      },
      ignoreInit = TRUE
    )
    
    observeEvent(
      suppressWarnings(plotly::event_data(
        event = "plotly_selected",
        source = ns("combined_umap_src"),
        session = session,
        priority = "event"
      )),
      {
        req(output$combined_umap_plot)

        ids <- selected_event_ids(ns("combined_umap_src"))
        if (length(ids) > 0L) linked_selection(ids)
      },
      ignoreInit = TRUE
    )
    
    observeEvent(input$highlight_clusters, {
      selected_clusters <- as.character(input$highlight_clusters %||% character(0))
      
      if (length(selected_clusters) == 0L) {
        linked_selection(character(0))
        return()
      }
      
      req(shared_rv$cluster_labels)
      
      ids <- names(shared_rv$cluster_labels)[
        as.character(shared_rv$cluster_labels) %in% selected_clusters
      ]
      
      linked_selection(unique(as.character(ids)))
    }, ignoreInit = TRUE)
    
    observeEvent(input$btn_clear_selection, {
      linked_selection(character(0))
      updateSelectizeInput(
        session,
        "highlight_clusters",
        selected = character(0),
        server = FALSE
      )
    })
    
    highlighted_ids <- reactive({
      unique(as.character(linked_selection() %||% character(0)))
    })
    
    apply_highlight_alpha <- function(df) {
      ids <- highlighted_ids()
      
      if (length(ids) == 0L) {
        return(rep(0.85, nrow(df)))
      }
      
      ifelse(df$id %in% ids, 0.95, 0.08)
    }
    
    combined_spatial_df <- reactive({
      df <- plot_df()
      req(nrow(df) > 0L)
      df <- swap_for_plotly(df)  # every consumer of this reactive is Plotly-only
      
      df$cluster <- if (!is.null(shared_rv$cluster_labels)) {
        as.character(shared_rv$cluster_labels[df$id])
      } else {
        NA_character_
      }
      
      df
    })
    
    combined_umap_df <- reactive({
      req(shared_rv$umap_df)
      emb <- shared_rv$umap_df
      emb$cluster <- if (!is.null(shared_rv$cluster_labels)) {
        as.character(shared_rv$cluster_labels[emb$id])
      } else {
        NA_character_
      }
      emb
    })
    
    output$highlight_debug_ui <- renderUI({
      global_data$language  # i18n: re-render on language switch
      hl <- highlighted_ids()
      if (length(hl) == 0) return(NULL)
      
      n_spatial <- sum(combined_spatial_df()$id %in% hl)
      n_umap <- if (!is.null(shared_rv$umap_df)) sum(combined_umap_df()$id %in% hl) else NA
      
      div(
        class = "text-info style-sm mt-1",
        style = "font-size:0.75rem;",
        sprintf(
          .tr("\u2139\ufe0f %d cellule(s) selectionnee(s) / isolee(s) (Spatial: %s, UMAP: %s)"),
          length(hl),
          paste0(n_spatial, " pts"),
          if (is.na(n_umap)) .tr("non calcule") else paste0(n_umap, " pts")
        )
      )
    })
    
    # --------------------------------------------------------------------------
    # Vue combinée : spatial
    # --------------------------------------------------------------------------
    output$combined_spatial_plot <- plotly::renderPlotly({
      df <- combined_spatial_df()
      req(nrow(df) > 0L)
      
      df$y_display <- -df$y
      df$colour <- color_values(df)
      
      alpha_vec <- apply_highlight_alpha(df)
      
      add_alpha <- function(hex_col, alpha) {
        hex_col <- ifelse(is.na(hex_col) | !nzchar(hex_col), "#CCCCCC", hex_col)
        rgb_mat <- grDevices::col2rgb(hex_col, alpha = FALSE)
        
        grDevices::rgb(
          red = rgb_mat[1L, ],
          green = rgb_mat[2L, ],
          blue = rgb_mat[3L, ],
          alpha = pmax(0, pmin(255, round(alpha * 255))),
          maxColorValue = 255
        )
      }
      
      df$colour <- add_alpha(df$colour, alpha_vec)
      
      hist_ov <- histology_overlay_plotly()
      show_hist <- !is.null(hist_ov) &&
        !is.null(hist_ov$data_uri) &&
        isTRUE(input$show_histology)
      
      # Construction de l'image unique, puis empaquetage dans une liste
      hist_image <- if (show_hist) {
        make_plotly_histology_image(
          hist_ov = hist_ov,
          opacity = input$histology_opacity %||% 0.7
        )
      } else {
        NULL
      }
      
      img <- if (!is.null(hist_image)) list(hist_image) else NULL
      
      ranges <- compute_spatial_ranges(
        df_all = df,
        hist_ov = hist_ov,
        show_hist = show_hist
      )
      
      trace_type <- if (show_hist) "scatter" else "scattergl"
      
      p <- plotly::plot_ly(
        data = df,
        x = ~x,
        y = ~y_display,
        key = ~id,
        type = trace_type,
        mode = "markers",
        marker = list(
          color = ~colour,
          size = input$pt_radius,
          opacity = 1
        ),
        text = ~paste0(
          "ID : ", id,
          "<br>Valeur : ",
          ifelse(
            is.na(value),
            "NA",
            if (is.numeric(value)) round(value, 3) else as.character(value)
          ),
          "<br>Cluster : ",
          ifelse(is.na(cluster), "NA", cluster)
        ),
        hoverinfo = "text",
        source = ns("combined_spatial_src")
      ) |>
        plotly::layout(
          dragmode = "lasso",
          margin = list(l = 10, r = 10, t = 10, b = 10),
          images = img,
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)",
          xaxis = list(
            title = "",
            zeroline = FALSE,
            showgrid = FALSE,
            range = ranges$x
          ),
          yaxis = list(
            title = "",
            zeroline = FALSE,
            showgrid = FALSE,
            scaleanchor = "x",
            scaleratio = 1,
            range = ranges$y
          )
        )
      
      if (
        identical(input$color_by, "cluster") &&
        isTRUE(input$show_cluster_labels) &&
        any(!is.na(df$cluster))
      ) {
        centroids <- stats::aggregate(
          cbind(x, y_display) ~ cluster,
          data = df[!is.na(df$cluster), , drop = FALSE],
          FUN = mean
        )
        
        p <- p |>
          plotly::add_annotations(
            x = centroids$x,
            y = centroids$y_display,
            text = centroids$cluster,
            showarrow = FALSE,
            font = list(size = 11, color = "#1A1A1A"),
            bgcolor = "rgba(255,255,255,0.65)",
            bordercolor = "#666666",
            borderwidth = 0.5
          )
      }
      
      plotly::event_register(p, "plotly_selected")
    })
    
    # --------------------------------------------------------------------------
    # Vue combinée : UMAP
    # --------------------------------------------------------------------------
    output$combined_umap_plot <- plotly::renderPlotly({
      df <- combined_umap_df()
      req(nrow(df) > 0)
      
      pal <- cluster_palette(df$cluster)
      alpha_vec <- apply_highlight_alpha(df)
      
      encode_rgba <- function(hex_col, alpha) {
        hex_col <- ifelse(is.na(hex_col) | !nzchar(hex_col), "#CCCCCC", hex_col)
        rgb_mat <- grDevices::col2rgb(hex_col, alpha = FALSE)
        sprintf(
          "rgba(%d,%d,%d,%.3f)",
          rgb_mat[1, 1], rgb_mat[2, 1], rgb_mat[3, 1], alpha
        )
      }
      
      df$colour <- mapply(function(cl, a) {
        hex <- if (!is.na(cl) && cl %in% names(pal)) pal[[cl]] else "#CCCCCC"
        encode_rgba(hex, a)
      }, df$cluster, alpha_vec)
      
      p <- plotly::plot_ly(
        source = ns("combined_umap_src")
      ) |>
        plotly::layout(
          dragmode = "lasso",
          margin = list(l = 10, r = 10, t = 10, b = 10),
          xaxis = list(title = "UMAP_1", zeroline = FALSE, showgrid = FALSE),
          yaxis = list(title = "UMAP_2", zeroline = FALSE, showgrid = FALSE)
        )
      
      for (cl in sort_cluster_labels(df$cluster)) {
        df_cl <- df[df$cluster == cl, , drop = FALSE]
        p <- p |>
          plotly::add_trace(
            data = df_cl,
            x = ~dim1,
            y = ~dim2,
            key = ~id,
            type = "scattergl",
            mode = "markers",
            name = cl,
            legendgroup = cl,
            marker = list(color = ~colour, size = 5),
            text = ~paste0("ID: ", id, "<br>Cluster: ", cluster),
            hoverinfo = "text"
          )
      }
      
      p <- plotly::event_register(p, "plotly_selected")
      p
    })
    
    # --------------------------------------------------------------------------
    # ROI
    # --------------------------------------------------------------------------
    roi_ids <- reactiveVal(NULL)
    
    # FIX (audit step 3.12 -- multi-echantillons dans l'onglet 4): switching
    # the active dataset (mod_spatial.R's selector) already resets shared_rv
    # (QC/clusters/deconv/ROI) and now ALSO restores/clears shared_rv$umap_df
    # centrally (see mod_spatial.R's .cacheable_fields -- feedback biologiste:
    # le sketch-UMAP se perdait a chaque changement d'echantillon). This
    # module's OWN local state -- roi_ids (indices into the PREVIOUS
    # dataset's coordinates) -- is NOT part of that cache (ROI stays tightly
    # coupled to this module's own local reactive state, not safely
    # restorable, see roi_ids below) and is still reset here explicitly.
    observeEvent(global_data$active_spatial_dataset, {
      roi_ids(NULL)
      shared_rv$roi_ids <- NULL
    }, ignoreInit = TRUE)
    
    observeEvent(input$btn_isolate_roi, {
      hl <- highlighted_ids()
      if (length(hl) == 0) {
        showNotification(.tr("Aucune selection a isoler. Utilisez le lasso."), type = "warning")
        return()
      }
      roi_ids(hl)
      showNotification(.t_fmt(.tr("ROI isolee ({n} cellules)."), n = length(hl)), type = "message")
    })
    
    observeEvent(input$btn_clear_roi, {
      roi_ids(NULL)
      shared_rv$roi_ids <- NULL
    })
    
    roi_df <- reactive({
      ids <- roi_ids()
      req(ids, length(ids) > 0)
      df <- combined_spatial_df()
      df[df$id %in% ids, , drop = FALSE]
    })
    
    output$roi_summary_ui <- renderUI({
      global_data$language  # i18n: re-render on language switch
      ids <- roi_ids()
      if (is.null(ids) || length(ids) == 0) {
        return(
          div(
            class = "alert alert-warning mb-3",
            bsicons::bs_icon("exclamation-triangle"),
            " ",
            .tr("Aucune ROI isolee.")
          )
        )
      }
      
      total <- if (!is.null(global_data$spatial_obj$sketch)) ncol(global_data$spatial_obj$sketch) else NA
      pct <- if (!is.na(total) && total > 0) round(length(ids) / total * 100, 1) else NA
      
      div(
        class = "alert alert-success mb-3 d-flex justify-content-between align-items-center",
        div(
          strong(.t_fmt(.tr("ROI active : {n} spots"), n = length(ids))),
          if (!is.na(pct)) sprintf(" — %.1f%%", pct) else ""
        )
      )
    })
    
    output$roi_zoom_plot <- plotly::renderPlotly({
      df_roi <- roi_df()
      req(nrow(df_roi) > 0)
      df_roi$y_display <- -df_roi$y
      
      hist_ov <- histology_overlay_plotly()
      show_hist <- !is.null(hist_ov) && !is.null(hist_ov$data_uri) && isTRUE(input$show_histology)
      trace_type <- if (isTRUE(show_hist)) "scatter" else "scattergl"
      
      img_single <- if (show_hist) {
        make_plotly_histology_image(hist_ov = hist_ov, opacity = input$histology_opacity %||% 0.7)
      } else {
        NULL
      }
      plot_images <- if (!is.null(img_single)) list(img_single) else NULL
      
      xr <- range(df_roi$x, na.rm = TRUE)
      yr <- range(df_roi$y_display, na.rm = TRUE)
      xdiff <- max(diff(xr), 10)
      ydiff <- max(diff(yr), 10)
      x_range <- c(xr[1] - xdiff * 0.1, xr[2] + xdiff * 0.1)
      y_range <- c(yr[1] - ydiff * 0.1, yr[2] + ydiff * 0.1)
      
      pal <- cluster_palette(df_roi$cluster)
      
      p <- plotly::plot_ly() |>
        plotly::layout(
          margin = list(l = 10, r = 10, t = 10, b = 10),
          images = plot_images,
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)",
          xaxis = list(title = "", zeroline = FALSE, showgrid = FALSE, range = x_range),
          yaxis = list(title = "", zeroline = FALSE, showgrid = FALSE, scaleanchor = "x", scaleratio = 1, range = y_range)
        )
      
      for (cl in sort_cluster_labels(df_roi$cluster)) {
        sub_df <- df_roi[df_roi$cluster == cl, , drop = FALSE]
        hex_col <- if (!is.na(cl) && cl %in% names(pal)) pal[[cl]] else "#CCCCCC"
        p <- p |>
          plotly::add_trace(
            data = sub_df,
            x = ~x,
            y = ~y_display,
            key = ~id,
            type = trace_type,
            mode = "markers",
            name = cl,
            legendgroup = cl,
            marker = list(color = hex_col, size = input$pt_radius),
            text = ~paste0("ID: ", id, "<br>Cluster: ", cluster),
            hoverinfo = "text"
          )
      }
      p
    })
    
    output$roi_context_plot <- plotly::renderPlotly({
      global_data$language  # i18n: re-render on language switch
      df_all <- combined_spatial_df()
      req(nrow(df_all) > 0)
      ids_roi <- roi_ids()
      req(ids_roi)
      
      df_all$y_display <- -df_all$y
      df_all$is_roi <- df_all$id %in% ids_roi
      
      hist_ov <- histology_overlay_plotly()
      show_hist <- !is.null(hist_ov) && !is.null(hist_ov$data_uri) && isTRUE(input$show_histology)
      trace_type <- if (isTRUE(show_hist)) "scatter" else "scattergl"
      
      img_single <- if (show_hist) {
        make_plotly_histology_image(hist_ov = hist_ov, opacity = input$histology_opacity %||% 0.7)
      } else {
        NULL
      }
      plot_images <- if (!is.null(img_single)) list(img_single) else NULL
      
      spatial_ranges <- compute_spatial_ranges(df_all = df_all, hist_ov = hist_ov, show_hist = show_hist)
      
      p <- plotly::plot_ly() |>
        plotly::layout(
          margin = list(l = 10, r = 10, t = 10, b = 10),
          images = plot_images,
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)",
          xaxis = list(title = "", zeroline = FALSE, showgrid = FALSE, range = spatial_ranges$x),
          yaxis = list(title = "", zeroline = FALSE, showgrid = FALSE, scaleanchor = "x", scaleratio = 1, range = spatial_ranges$y)
        )
      
      df_bg <- df_all[!df_all$is_roi, , drop = FALSE]
      if (nrow(df_bg) > 0) {
        p <- p |>
          plotly::add_trace(
            data = df_bg,
            x = ~x,
            y = ~y_display,
            type = trace_type,
            mode = "markers",
            name = .tr("Reste"),
            marker = list(color = "rgba(180, 180, 180, 0.3)", size = max(1, input$pt_radius - 2)),
            hoverinfo = "none"
          )
      }
      
      df_fg <- df_all[df_all$is_roi, , drop = FALSE]
      if (nrow(df_fg) > 0) {
        pal <- cluster_palette(df_fg$cluster)
        for (cl in sort_cluster_labels(df_fg$cluster)) {
          sub_df <- df_fg[df_fg$cluster == cl, , drop = FALSE]
          hex_col <- if (!is.na(cl) && cl %in% names(pal)) pal[[cl]] else "#e41a1c"
          p <- p |>
            plotly::add_trace(
              data = sub_df,
              x = ~x,
              y = ~y_display,
              key = ~id,
              type = trace_type,
              mode = "markers",
              name = paste0("ROI: ", cl),
              marker = list(color = hex_col, size = input$pt_radius + 1),
              text = ~paste0("ID: ", id, "<br>Cluster: ", cluster),
              hoverinfo = "text"
            )
        }
      }
      p
    })
    
    # --------------------------------------------------------------------------
    # Marqueurs ROI (asynchrones)
    # --------------------------------------------------------------------------
    roi_log_file <- spatial_log_path(session, "roi_markers")
    roi_tracker <- create_reactive_tracker(session, roi_log_file)
    
    roi_markers_task <- ExtendedTask$new(function(sketch_path, roi_ids, log_file) {
      mirai::mirai(
        {
          write_mirai_log(log_file, "Chargement du sketch...", 1, 4)
          sk <- readRDS(sketch_path)
          
          write_mirai_log(log_file, "Preparation du test...", 2, 4)
          if (!"data" %in% SeuratObject::Layers(sk)) {
            sk <- Seurat::NormalizeData(sk, verbose = FALSE)
          }
          
          sk$roi_group <- ifelse(colnames(sk) %in% roi_ids, "ROI", "Reste")
          sk$roi_group <- factor(sk$roi_group, levels = c("ROI", "Reste"))
          
          write_mirai_log(log_file, "Calcul FindMarkers...", 3, 4)
          Seurat::Idents(sk) <- "roi_group"
          res <- Seurat::FindMarkers(
            sk,
            ident.1 = "ROI",
            ident.2 = "Reste",
            test.use = "wilcox",
            logfc.threshold = 0.25,
            min.pct = 0.1,
            verbose = FALSE
          )
          
          write_mirai_log(log_file, "Termine.", 4, 4)
          res$gene <- rownames(res)
          res
        },
        sketch_path = sketch_path,
        roi_ids = roi_ids,
        log_file = log_file,
        .timeout = MIRAI_TASK_TIMEOUT_MS
      )
    })
    bslib::bind_task_button(roi_markers_task, "btn_roi_markers")
    
    observeEvent(input$btn_roi_markers, {
      ids <- roi_ids()
      req(ids, length(ids) > 0, global_data$spatial_obj$sketch)
      reset_log(roi_log_file)
      tmp <- tempfile(fileext = ".rds")
      saveRDS(global_data$spatial_obj$sketch, tmp)
      roi_markers_task$invoke(sketch_path = tmp, roi_ids = ids, log_file = roi_log_file)
    })
    
    roi_markers_res <- reactiveVal(NULL)
    
    observeEvent(roi_markers_task$status(), {
      if (identical(roi_markers_task$status(), "success")) {
        result <- roi_markers_task$result()
        roi_markers_res(result)
        
        if (is.null(result) || nrow(result) == 0L) {
          showNotification(
            .tr("Calcul termine, mais aucun gene ne passe les seuils definis."),
            type = "warning",
            duration = 8
          )
        } else {
          showNotification(
            .t_fmt(.tr("{n} marqueurs ROI calcules."), n = nrow(result)),
            type = "message",
            duration = 4
          )
        }
      }
      
      if (identical(roi_markers_task$status(), "error")) {
        showNotification(
          .tr("Erreur lors du calcul des marqueurs ROI. Consultez le journal."),
          type = "error",
          duration = 10
        )
      }
    })
    
    output$roi_markers_progress_text <- renderText({
      global_data$language  # i18n: re-render on language switch
      lines <- roi_tracker()
      if (length(lines) == 0) return(.tr("En attente..."))
      paste(lines, collapse = "\n")
    })
    
    output$roi_markers_table <- DT::renderDT({
      global_data$language  # i18n: re-render on language switch
      res <- roi_markers_res()
      
      validate(
        need(
          !is.null(res) && nrow(res) > 0L,
          .tr("Aucun marqueur a afficher. Lancez le calcul ou ajustez les seuils.")
        )
      )
      
      p_cols <- intersect(c("p_val", "p_val_adj"), colnames(res))
      effect_cols <- intersect(c("avg_log2FC", "avg_logFC", "pct.1", "pct.2"), colnames(res))
      
      order_col <- match("p_val_adj", colnames(res))
      if (is.na(order_col)) order_col <- 1L
      
      tbl <- ts_datatable(
        res,
        page_length = 15L,
        filename_base = "spatial_roi_markers",
        class = "compact stripe hover",
        filter = "none",
        extra_options = list(
          lengthMenu = c(10, 15, 25, 50),
          autoWidth  = TRUE,
          order      = list(list(order_col - 1L, "asc"))
        )
      )

      if (length(effect_cols) > 0L) {
        tbl <- DT::formatRound(tbl, columns = effect_cols, digits = 3)
      }
      
      if (length(p_cols) > 0L) {
        tbl <- DT::formatSignif(tbl, columns = p_cols, digits = 3)
      }
      
      tbl
    })
    
    output$dl_roi_object <- downloadHandler(
      filename = function() paste0("spatial_roi_subset_", Sys.Date(), ".rds"),
      content = function(file) {
        ids <- roi_ids()
        validate(need(!is.null(ids) && length(ids) > 0, .tr("Aucune ROI selectionnee.")))
        sub_obj <- materialize_seurat_subset(global_data$spatial_obj, cell_ids = ids)
        saveRDS(sub_obj, file)
      }
    )
    
  })
}

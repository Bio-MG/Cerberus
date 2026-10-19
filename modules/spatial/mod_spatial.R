# =============================================================================
# modules/spatial/mod_spatial.R — Parent Module (router)
# =============================================================================
# i18n Phase 5 : tous les labels statiques passent par i18n$t() (HTML, traduits
#    cote client par le shim JS), le contenu dynamique serveur passe par le
#    proxy .tr() (traducteur de session) + .t_fmt() pour l'interpolation.
#    Chaque renderUI lit global_data$language en premiere ligne pour se
#    re-rendre au changement de langue.
#
# v13 (vague 5 — Phase 6 stats) : shared_rv gagne 6 nouveaux champs pour les
#    sous-onglets B1 (enrichissement de voisinage) et B6 (Ripley's K) de
#    mod_spatial_niche.R : enrichment_result/enrichment_params,
#    hotspot_result/hotspot_params (B4, mod_spatial_qc.R),
#    ripley_result/ripley_params (B6, mod_spatial_niche.R) -- ajoutes a
#    .cacheable_fields pour beneficier GRATUITEMENT du meme cache par-
#    echantillon que qc_metrics/cluster_labels/etc. (voir v9/v10 ci-dessous).
#    B3 (composition differentielle, mod_spatial_multi.R) N'EST PAS ajoute
#    ici deliberement : comme l'integration multi-echantillons elle-meme
#    (global_data$spatial_multi_integration), c'est un resultat CROSS-
#    dataset, pas un resultat de l'echantillon actif -- il vit dans son
#    propre reactive local a mod_spatial_multi.R et n'a pas besoin d'etre
#    snapshot/restaure au changement d'echantillon actif.
#
# v11 (moyen terme — export/auto-pipeline, voir handoff_spatial_bio-mg.md) :
#    2 nouveaux onglets additifs, zero changement de logique existante :
#      - "Pipeline auto" (mod_spatial_pipeline.R) : QC -> Clustering ->
#        Deconvolution (si reference partagee) -> Niches, en 1 clic, memes
#        resultats/memes champs shared_rv que si chaque etape avait ete
#        lancee manuellement depuis son propre onglet.
#      - "7. Export" (mod_spatial_export.R) : paquet .zip (CSV/PNG deja
#        calcules) + script R reproductible.
#    shared_rv gagne 5 champs *_params (miroir des parametres utilises pour
#    chaque resultat, ecrits par chaque onglet au moment du clic sur son
#    propre bouton) -- lus par mod_spatial_export.R pour le script
#    reproductible ; caches/restaures comme le reste au changement
#    d'echantillon (voir .cacheable_fields ci-dessous).
#
# v12 (feedback biologiste, session suivante) :
#    - "8. Rapport" (mod_spatial_report.R) : rapport HTML/PDF multi-
#      echantillons (voir handoff pour details).
#    - shared_rv$umap_df AJOUTE et rendu cacheable : le sketch-UMAP (onglet
#      4) etait un reactiveVal LOCAL a mod_spatial_viz.R, perdu a chaque
#      changement d'echantillon en multi-echantillons -- deplace ici pour
#      beneficier du meme cache par-echantillon que le reste.
#    - Selecteur "Actif" : fix CSS (debordement sur le menu "Session" avec
#      un nom d'echantillon long).
#
# v10 (backlog court-terme #2, session "puck_01 bugfix + backlog" — voir
#    handoff_spatial_bio-mg.md) : le cache par-echantillon (v9 ci-dessous)
#    est deplace de reactiveVal(list()) local vers
#    global_data$spatial_results_cache -- app.R n'a aucun acces direct au
#    reactiveVal INTERNE d'un module Shiny, alors que global_data est deja
#    le bus partage que app.R serialise dans save_session_btn/restaure dans
#    load_session_file. Zero changement de logique de cache, uniquement de
#    "conteneur" -- rend une session Spatial multi-echantillon veritablement
#    resumable apres sauvegarde/rechargement (voir app.R pour la partie
#    serialisation).
#
# v9 (Chantier 3 round-2 — per-dataset result cache). Switching the active
# dataset used to unconditionally NULL every derived result (QC/clusters/
# deconv/niche) -- correct the first time a dataset is visited, but wasteful
# every time the user just wants to flip BACK to a sample already analyzed
# earlier in the session (reported: "le clustering doit etre recalcule quand
# on a un multi echantillon quand on veut visualiser"). results_cache() now
# snapshots shared_rv into an in-memory, SESSION-SCOPED cache keyed by
# dataset name right before switching away from it, and restores it if
# present when switching back -- a fresh dataset (never analyzed) still
# gets the same clean-slate NULLs as before. ROI state (roi_ids/roi_bbox/
# roi_markers) is deliberately NEVER cached/restored: it is tightly coupled
# to mod_spatial_viz.R's own local reactive state (umap_df, linked
# selections), which is not safely restorable from here, so ROI is always
# reset on a dataset switch, same as before this change. This cache does
# NOT persist across app restarts / session save-load (out of scope for
# this round -- see handoff note).
#
# v8 (Chantier 3 refonte — daemon badge honesty): 3-state daemon badge
# (ready/degraded/inactive) + diagnostics.
#
# v6 (Phase 4 — "Working with multiple slices"): 5th top-level tab,
# "Multi-echantillons" (mod_spatial_multi.R), active-dataset switcher.
#
# v6.1 (audit step 3.9b — multi-sample bugfix): `ns <- session$ns` fix.
#
# v5 (Phase 3 — ROI/crop): shared_rv gained roi_ids / roi_bbox / roi_markers.
#
# v4 (UX feedback): daemon status/reset row AND dataset banner folded into
# the tab strip itself.
#
# v2 (post-test-3): "Reinitialiser les daemons" button.
#
# v7 (Phase 5 — niches spatiales): 6th top-level tab, "Niches spatiales".
# =============================================================================

mod_spatial_ui <- function(id) {
  ns <- NS(id)
  # ── V1.x UX: standard container (LEFT = workflow steps, RIGHT = results) ──
  # The old top-level horizontal navset_card_underline of peer stages is
  # replaced by the project-wide skeleton: layout_sidebar(sidebar = numbered
  # accordion, navset_card_underline = results).
  #
  # FOLLOW-UP (known leftover, zero regression): QC / Cluster / Deconv /
  # Visualisation / Multi-echantillons / Niches child UIs still ship their own
  # layout_sidebar (controls + results together). They are reparented AS-IS
  # into the right navset (like the previous horizontal tabs) — their internals
  # are out of scope for this lot. Only the pipeline module is split: controls
  # in the accordion panel, summary in the right navset.
  layout_sidebar(
    sidebar = sidebar(
      width = 340,
      title = i18n$t("Workflow Spatial"),
      # LOT 1 (V1.x UX): paradigm badge — async auto-pipeline (mirai).
      div(class = "alert alert-success", style = "font-size:0.78rem;padding:5px;margin-bottom:5px;",
          icon("bolt"), " ", i18n$t("Pipeline automatique asynchrone (mirai) — le statut reste visible ci-dessous")),
      # Status placement (v4 status row relocated from the tab strip): dataset
      # selector + daemon status stay ALWAYS VISIBLE in the sidebar header.
      uiOutput(ns("active_dataset_ui")),
      uiOutput(ns("daemon_status_ui")),
      actionButton(ns("btn_reset_daemons"), i18n$t("Reinitialiser les daemons"),
                   class = "btn-outline-warning btn-sm w-100 mt-1", icon = icon("rotate")),
      hr(),
      accordion(
        id = ns("steps"),
        open = "panel_pipeline",
        # multiple = FALSE: one workflow step open at a time (same semantics as
        # the old horizontal navset). bslib's accordion input binding returns a
        # vector of ALL open panels when multiple=TRUE, which crashes single-
        # value indexing in the sync observer below (crash reported 2026-09-05).
        multiple = FALSE,

        accordion_panel(i18n$t("0. Pipeline auto"), icon = icon("bolt"),
                        value = "panel_pipeline",
                        mod_spatial_pipeline_ui(ns("pipeline"))),

        # LOT 5 (V1.x UX): tooltip demystifies "Moran" on the QC step.
        accordion_panel(tagList(i18n$t("1. QC & Autocorrelation"), " ",
                                bslib::tooltip(icon("circle-info"),
                                               i18n$t("Autocorrélation spatiale (Moran) — gènes spatialement variables"))),
                        icon = icon("filter"),
                        value = "panel_qc",
                        p(class = "small text-muted mb-0",
                          i18n$t("Les contrôles et les résultats de cette étape s'affichent dans le panneau de droite."))),

        accordion_panel(i18n$t("2. Clustering (BANKSY-lite)"), icon = icon("shapes"),
                        value = "panel_cluster",
                        p(class = "small text-muted mb-0",
                          i18n$t("Les contrôles et les résultats de cette étape s'affichent dans le panneau de droite."))),

        accordion_panel(i18n$t("3. Deconvolution"), icon = icon("puzzle-piece"),
                        value = "panel_deconv",
                        p(class = "small text-muted mb-0",
                          i18n$t("Les contrôles et les résultats de cette étape s'affichent dans le panneau de droite."))),

        accordion_panel(i18n$t("4. Visualisation"), icon = icon("eye"),
                        value = "panel_viz",
                        p(class = "small text-muted mb-0",
                          i18n$t("Les contrôles et les résultats de cette étape s'affichent dans le panneau de droite."))),

        accordion_panel(i18n$t("5. Multi-echantillons"), icon = icon("layer-group"),
                        value = "panel_multi",
                        p(class = "small text-muted mb-0",
                          i18n$t("Les contrôles et les résultats de cette étape s'affichent dans le panneau de droite."))),

        # LOT 5 (V1.x UX glossary): dual label — what "spatial niches" means.
        accordion_panel(i18n$t("Niches spatiales — micro-environnements cellulaires"), icon = icon("diagram-project"),
                        value = "panel_niche",
                        p(class = "small text-muted mb-0",
                          i18n$t("Les contrôles et les résultats de cette étape s'affichent dans le panneau de droite."))),

        accordion_panel(i18n$t("7. Export & Rapport"), icon = icon("file-export"),
                        value = "panel_export",
                        p(class = "small text-muted mb-0",
                          i18n$t("Les contrôles et les résultats de cette étape s'affichent dans le panneau de droite.")))
      )
    ),
    navset_card_underline(
      id = ns("results"), title = i18n$t("Résultats"),

      # Pipeline step: results/summary on the right (controls live in the
      # accordion). Same "spatial-pipeline" namespace => server untouched.
      nav_panel(i18n$t("Résumé Pipeline"), value = "results_pipeline",
                mod_spatial_pipeline_summary_ui(ns("pipeline"))),

      nav_panel(i18n$t("1. QC & Autocorrelation"), icon = icon("filter"),
                value = "results_qc",
                mod_spatial_qc_ui(ns("qc"))),

      nav_panel(i18n$t("2. Clustering (BANKSY-lite)"), icon = icon("shapes"),
                value = "results_cluster",
                mod_spatial_cluster_ui(ns("cluster"))),

      nav_panel(i18n$t("3. Deconvolution"), icon = icon("puzzle-piece"),
                value = "results_deconv",
                mod_spatial_deconv_ui(ns("deconv"))),

      nav_panel(i18n$t("4. Visualisation"), icon = icon("eye"),
                value = "results_viz",
                mod_spatial_viz_ui(ns("viz"))),

      nav_panel(i18n$t("5. Multi-echantillons"), icon = icon("layer-group"),
                value = "results_multi",
                mod_spatial_multi_ui(ns("multi"))),

      nav_panel(i18n$t("Niches spatiales — micro-environnements cellulaires"), icon = icon("diagram-project"),
                value = "results_niche",
                mod_spatial_niche_ui(ns("niche"))),

      nav_panel(i18n$t("7. Export & Rapport"), icon = icon("file-export"),
                value = "results_export_report",
                navset_pill(
                  nav_panel(i18n$t("Paquet & Script"), value = "export",
                            mod_spatial_export_ui(ns("export"))),
                  nav_panel(i18n$t("Rapport HTML/PDF"), value = "report",
                            mod_spatial_report_ui(ns("report")))
                ))
    )
  )
}

mod_spatial_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── i18n proxy ──────────────────────────────────────────────────────────
    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # Chantier #5 : NE PAS passer de compte ici. `n_daemons = 6` court-circuitait
    # TS_MIRAI_N_DAEMONS / options(ts.mirai.n_daemons) : le pool démarrait à 6
    # quel que soit le réglage. Le défaut de init_spatial_daemons() résout la
    # config (option > constante > 6L).
    if (!spatial_daemons_ready()) init_spatial_daemons()

    shared_rv <- reactiveValues(
      active_tab       = "qc",
      qc_metrics       = NULL,
      qc_pass_idx      = NULL,
      qc_params        = NULL,
      moran_results    = NULL,
      moran_params     = NULL,
      cluster_labels   = NULL,
      cluster_params   = NULL,
      deconv_props     = NULL,
      deconv_params    = NULL,
      cluster_markers  = NULL,
      current_fov_crop = NULL,
      roi_ids          = NULL,
      roi_bbox          = NULL,
      roi_markers       = NULL,
      niche_labels      = NULL,
      niche_composition = NULL,
      niche_params      = NULL,
      umap_df           = NULL,
      saved_viz_list    = list(),
      # v13 (vague 5 — Phase 6 stats) : voir changelog en tete de fichier.
      #   - enrichment_result = list(enrichment=, matrix=, levels=,
      #     k_neighbors=, n_perm=) — mod_spatial_niche.R (B1).
      #   - hotspot_result = data.frame(id,value,gi_star,p_value,hotspot)
      #     — mod_spatial_qc.R (B4).
      #   - ripley_result = list(curve=, target_level=, n_target=, n_total=,
      #     n_perm=, subsampled=) — mod_spatial_niche.R (B6).
      enrichment_result = NULL,
      enrichment_params = NULL,
      hotspot_result     = NULL,
      hotspot_params     = NULL,
      ripley_result      = NULL,
      ripley_params      = NULL,
      # Palette PARTAGEE (vague 6) — reglages UTILISATEUR, pas des resultats
      # de calcul : volontairement HORS .cacheable_fields (persiste entre
      # changements d'echantillon actif, contrairement a qc_metrics/etc.).
      color_palette   = "default",
      manual_gradient = list(low = "#2166AC", mid = "white", high = "#B2182B"),
      manual_discrete = list()   # list(cluster=c(...), niche=c(...), celltype=c(...), dataset=c(...))
    )

    # ── v10 (backlog court-terme #2): per-dataset result cache, backed by
    # global_data$spatial_results_cache (see v10 changelog above).
    .get_cache <- function() global_data$spatial_results_cache %||% list()
    .set_cache <- function(cache) { global_data$spatial_results_cache <- cache }

    .cacheable_fields <- c("qc_metrics", "qc_pass_idx", "qc_params", "moran_results", "moran_params",
                           "cluster_labels", "cluster_params", "deconv_props", "deconv_params",
                           "cluster_markers", "niche_labels", "niche_composition", "niche_params",
                           "umap_df", "saved_viz_list",
                           # v13 (vague 5 — Phase 6 stats)
                           "enrichment_result", "enrichment_params",
                           "hotspot_result", "hotspot_params",
                           "ripley_result", "ripley_params")

    .snapshot_shared_rv <- function() {
      stats::setNames(lapply(.cacheable_fields, function(f) shared_rv[[f]]), .cacheable_fields)
    }
    .restore_shared_rv <- function(snap) {
      for (f in .cacheable_fields) shared_rv[[f]] <- snap[[f]] %||% NULL
    }
    .clear_shared_rv <- function() {
      for (f in .cacheable_fields) shared_rv[[f]] <- NULL
    }

    # ── Daemon status (i18n-aware) ─────────────────────────────────────────
    output$daemon_status_ui <- renderUI({
      global_data$language  # re-render trigger
      input$btn_reset_daemons
      status <- tryCatch(spatial_daemon_status(), error = function(e) "inactive")
      label <- switch(status,
                      "ready"    = .tr("\u2705 daemons actifs (verifies)"),
                      "degraded" = .tr("\u26a0\ufe0f daemons actifs -- preload degrade"),
                      .tr("\u26aa daemons inactifs")
      )
      css_class <- switch(status, "ready" = "text-success", "degraded" = "text-warning", "text-muted")
      tagList(
        tags$span(class = paste("small", css_class), style = "align-self:center;", label),
        if (identical(status, "degraded")) {
          tags$details(
            style = "font-size:0.68rem; margin-top:2px;",
            tags$summary("Diagnostic"),
            tags$pre(style = "white-space:pre-wrap; max-width:260px;",
                     tryCatch(spatial_daemon_diagnostics_text(), error = function(e) conditionMessage(e)))
          )
        }
      )
    })

    observeEvent(input$btn_reset_daemons, {
      global_data$language  # notification language
      ok <- tryCatch(reset_spatial_daemons(6), error = function(e) FALSE)
      if (isTRUE(ok)) {
        st <- tryCatch(spatial_daemon_status(), error = function(e) "inactive")
        msg <- if (identical(st, "ready")) {
          .tr("\U0001F504 Daemons mirai reinitialises et verifies — relancez votre tache.")
        } else {
          .tr("\U0001F504 Daemons mirai reinitialises, mais la verification reste en echec — voir le diagnostic.")
        }
        showNotification(msg, type = if (identical(st, "ready")) "message" else "warning", duration = 6)
      } else {
        showNotification(.tr("Echec de la reinitialisation des daemons — voir la console R."),
                         type = "error", duration = 8)
      }
    })

    # ── Active dataset selector ─────────────────────────────────────────────
    output$active_dataset_ui <- renderUI({
      global_data$language
      ds_names <- names(global_data$spatial_datasets)
      if (length(ds_names) < 2) return(NULL)
      tags$span(
        style = "display:flex; align-items:center; gap:6px; max-width:240px; min-width:0;",
        tags$style(HTML(sprintf(
          "#%s { text-overflow: ellipsis; overflow: hidden; white-space: nowrap; }",
          ns("active_dataset_select")
        ))),
        tags$span(class = "small text-muted", style = "flex-shrink:0;", .tr("Actif :")),
        div(style = "min-width:0; flex:1 1 auto;",
            title = global_data$active_spatial_dataset %||% ds_names[1],
            selectInput(ns("active_dataset_select"), NULL, choices = ds_names,
                        selected = global_data$active_spatial_dataset %||% ds_names[1],
                        width = "100%"))
      )
    })

    observeEvent(input$active_dataset_select, {
      req(input$active_dataset_select %in% names(global_data$spatial_datasets))
      if (identical(input$active_dataset_select, global_data$active_spatial_dataset)) return()
      global_data$active_spatial_dataset <- input$active_dataset_select
    })

    observeEvent(list(global_data$active_spatial_dataset, global_data$spatial_datasets), {
      nm <- global_data$active_spatial_dataset
      if (!is.null(nm) && nm %in% names(global_data$spatial_datasets)) {
        global_data$spatial_obj <- global_data$spatial_datasets[[nm]]
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    prev_active_dataset <- reactiveVal(NULL)

    observeEvent(global_data$active_spatial_dataset, {
      global_data$language  # for notification language
      new_name <- global_data$active_spatial_dataset
      old_name <- prev_active_dataset()

      if (!is.null(old_name) && !identical(old_name, new_name) &&
          old_name %in% names(global_data$spatial_datasets)) {
        cache <- .get_cache()
        cache[[old_name]] <- .snapshot_shared_rv()
        .set_cache(cache)
      }

      shared_rv$roi_ids     <- NULL
      shared_rv$roi_bbox    <- NULL
      shared_rv$roi_markers <- NULL

      if (!is.null(new_name)) {
        cached <- .get_cache()[[new_name]]
        if (!is.null(cached)) {
          .restore_shared_rv(cached)
          if (!is.null(old_name)) {
            showNotification(
              .t_fmt(.tr("Echantillon actif : {name} (resultats precedents restaures)."), name = new_name),
              type = "message", duration = 4)
          }
        } else {
          .clear_shared_rv()
          if (!is.null(old_name)) {
            showNotification(
              .t_fmt(.tr("Echantillon actif : {name}"), name = new_name),
              type = "message", duration = 4)
          }
        }
      }

      prev_active_dataset(new_name)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    observeEvent(global_data$spatial_reimport_signal, {
      global_data$language
      sig <- global_data$spatial_reimport_signal
      req(sig, identical(sig$name, global_data$active_spatial_dataset))
      .clear_shared_rv()
      cache <- .get_cache()
      cache[[sig$name]] <- NULL
      .set_cache(cache)
      showNotification(
        .t_fmt(.tr("Echantillon '{name}' re-importe (deja actif) : resultats precedents reinitialises."),
               name = sig$name),
        type = "message", duration = 5)
    }, ignoreInit = TRUE)

    # ── Sub-module servers (unchanged wiring) ──────────────────────────────
    mod_spatial_qc_server("qc", global_data, shared_rv)
    mod_spatial_cluster_server("cluster", global_data, shared_rv)
    mod_spatial_deconv_server("deconv", global_data, shared_rv)
    mod_spatial_viz_server("viz", global_data, shared_rv)
    mod_spatial_multi_server("multi", global_data, shared_rv)
    mod_spatial_niche_server("niche", global_data, shared_rv)
    mod_spatial_pipeline_server("pipeline", global_data, shared_rv)
    mod_spatial_export_server("export", global_data, shared_rv)
    mod_spatial_report_server("report", global_data, shared_rv)

    # ── V1.x UX container: accordion steps ↔ right results navset sync ──────
    # The old horizontal nav (input$spatial_nav) is gone. Opening an accordion
    # step selects the matching right panel; shared_rv$active_tab keeps its
    # historical values ("pipeline", "qc", ..., "export_report") so any
    # existing reader sees the same semantics.
    .step_results <- c(
      panel_pipeline = "results_pipeline", panel_qc      = "results_qc",
      panel_cluster  = "results_cluster",  panel_deconv  = "results_deconv",
      panel_viz      = "results_viz",      panel_multi   = "results_multi",
      panel_niche    = "results_niche",    panel_export  = "results_export_report"
    )
    .step_tab <- c(
      panel_pipeline = "pipeline", panel_qc = "qc",       panel_cluster = "cluster",
      panel_deconv   = "deconv",   panel_viz = "viz",     panel_multi   = "multi",
      panel_niche    = "niche",    panel_export = "export_report"
    )
    observeEvent(input$steps, {
      # Defensive: bslib returns a vector of ALL open panels (or NULL). Take
      # the LAST one (most recently opened step) so a multiple-open regression
      # can never crash single-value indexing again.
      panels <- input$steps
      if (is.null(panels) || length(panels) == 0) return(invisible(NULL))
      panel <- utils::tail(panels, 1)
      if (length(panel) != 1 || is.na(panel)) return(invisible(NULL))
      sel <- .step_results[[panel]]
      if (!is.null(sel)) try(nav_select(id = "results", selected = sel, session = session), silent = TRUE)
      shared_rv$active_tab <- .step_tab[[panel]] %||% panel
    }, ignoreNULL = TRUE)
  })
}

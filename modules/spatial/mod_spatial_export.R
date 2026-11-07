# =============================================================================
# modules/spatial/mod_spatial_export.R — Export ("paquet complet" + script R)
# =============================================================================
# i18n Phase 5 : labels statiques via i18n$t()/.tr_plain(), contenu dynamique
#    serveur via le proxy .tr() + .t_fmt() ; renderUI lit global_data$language.
#
# v2 (vague 5 — Phase 6 stats + "exporter tout ce qui est en memoire") :
#   1. 3 nouvelles sections cochables : "Enrichissement de voisinage" (B1),
#      "Hotspots (Getis-Ord)" (B4), "Ripley's K" (B6) -- memes conventions
#      que les sections existantes (voir R/utils_spatial_export.R). NULL-safe
#      : une section reste grisee/vide si le calcul correspondant n'a jamais
#      ete lance pour l'echantillon actif.
#   2. Liens "Tout selectionner" / "Tout deselectionner" au-dessus de la
#      liste de sections -- reponse directe au retour "un moyen d'exporter
#      tous les fichiers/plots deja calcules en memoire" sans devoir cocher
#      chaque section une par une.
#
# NEW (moyen terme a/b, voir handoff_spatial_bio-mg.md) : sur le modele de
# modules/bulk/mod_bulk_report.R (paquet .zip + script R reproductible),
# decline pour le module Spatial. Voir R/utils_spatial_export.R pour les
# fonctions pures sous-jacentes (build_spatial_export_bundle(),
# generate_spatial_pipeline_script()).
#
# 100% synchrone (pas de mirai) : n'exporte que des resultats DEJA calcules,
# assis dans shared_rv -- aucun nouveau calcul lourd ici, donc aucun besoin
# d'ExtendedTask ni de preload daemon pour ce module.
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

.SPATIAL_EXPORT_ALL_SECTIONS <- c("qc", "cluster", "deconv", "niche", "moran",
                                  "enrichment", "hotspots", "ripley", "multi", "maps", "custom_viz")


mod_spatial_export_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = i18n$t("Export"), width = 360,

      div(class = "alert alert-light", style = "font-size:0.8rem;",
          bsicons::bs_icon("archive"),
          i18n$t("Regroupe les resultats DEJA CALCULES (QC, clustering, deconvolution, niches, statistiques spatiales avancees, cartes) pour l'echantillon actif dans une archive .zip, ou genere un script R autonome (+ contexte) pour reproduire le pipeline hors Shiny.")),

      div(class = "d-flex gap-2 mb-1",
          actionLink(ns("btn_select_all"), i18n$t("Tout selectionner"), style = "font-size:0.75rem;"),
          tags$span("\u00b7", class = "text-muted"),
          actionLink(ns("btn_select_none"), i18n$t("Tout deselectionner"), style = "font-size:0.75rem;")),
      checkboxGroupInput(ns("sections"), i18n$t("Sections a inclure dans le paquet complet"),
        choices = stats::setNames(
          c("qc", "cluster", "deconv", "niche", "moran",
            "enrichment", "hotspots", "ripley", "maps", "multi", "custom_viz"),
          c(.tr_plain("QC"), .tr_plain("Clustering"), .tr_plain("Deconvolution"),
            .tr_plain("Niches"), .tr_plain("Moran's I (SVGs)"),
            .tr_plain("Enrichissement de voisinage"),
            .tr_plain("Hotspots (Getis-Ord Gi*)"),
            .tr_plain("Ripley's K"),
            .tr_plain("Cartes (PNG)"),
            .tr_plain("Multi-echantillons (integration + composition diff.)"),
            .tr_plain("Vues sauvegardees (onglet 4)"))),
        selected = .SPATIAL_EXPORT_ALL_SECTIONS),
      downloadButton(ns("dl_bundle"), i18n$t("\U0001F4E6 Paquet complet (.zip)"),
                     class = "btn-success w-100 mb-3"),

      hr(),
      div(class = "alert alert-light", style = "font-size:0.75rem;",
          bsicons::bs_icon("code-slash"),
          i18n$t("Le script reproduit UNIQUEMENT les etapes DEJA LANCEES dans l'app (QC/clustering/deconvolution RCTD/niches) avec les MEMES parametres. Necessite que le dossier BPCells reste accessible au meme chemin (voir README/commentaires inclus dans le script). Les statistiques spatiales avancees (enrichissement/hotspots/Ripley's K) ne sont PAS re-executees par ce script -- leurs resultats DEJA calcules restent disponibles en CSV dans le paquet complet ci-dessus.")),
      downloadButton(ns("dl_script"), i18n$t("\U0001F9FE Script R reproductible (.zip)"),
                     class = "btn-outline-secondary w-100")
    ),
    uiOutput(ns("export_preview_ui"))
  )
}

mod_spatial_export_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {

    # ── i18n proxy ──────────────────────────────────────────────────────────
    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    observeEvent(input$btn_select_all, {
      updateCheckboxGroupInput(session, "sections", selected = .SPATIAL_EXPORT_ALL_SECTIONS)
    })
    observeEvent(input$btn_select_none, {
      updateCheckboxGroupInput(session, "sections", selected = character(0))
    })

    output$export_preview_ui <- renderUI({
      global_data$language  # re-render on language switch
      obj <- global_data$spatial_obj
      if (is.null(obj)) {
        return(div(class = "alert alert-danger",
                    .tr("Aucune donnee spatiale chargee. Allez dans l'onglet 'Import Donnees > Spatial'.")))
      }
      row <- function(label, value) tags$tr(tags$td(label), tags$td(value))
      tagList(
        h6(.tr("Etat actuel de l'echantillon (ce qui sera exporte)"), style = "font-weight:bold;"),
        tags$table(class = "table table-sm table-striped",
          row(.tr("Echantillon actif"), obj$project %||% "-"),
          row(.tr("QC applique"), if (!is.null(shared_rv$qc_pass_idx)) .t_fmt(.tr("{n} elements retenus"), n = length(shared_rv$qc_pass_idx)) else .tr("Non")),
          row(.tr("Clustering"), if (!is.null(shared_rv$cluster_labels)) .t_fmt(.tr("{n} clusters"), n = length(unique(shared_rv$cluster_labels))) else .tr("Non calcule")),
          row(.tr("Deconvolution"), if (!is.null(shared_rv$deconv_props)) .t_fmt(.tr("{n} types cellulaires"), n = ncol(shared_rv$deconv_props) - 1) else .tr("Non calculee")),
          row(.tr("Niches"), if (!is.null(shared_rv$niche_labels)) .t_fmt(.tr("{n} niches"), n = length(unique(shared_rv$niche_labels))) else .tr("Non calculees")),
          row(.tr("Moran's I"), if (!is.null(shared_rv$moran_results)) .t_fmt(.tr("{n} genes testes"), n = nrow(shared_rv$moran_results)) else .tr("Non calcule")),
          row(.tr("Enrichissement de voisinage"), if (!is.null(shared_rv$enrichment_result)) .t_fmt(.tr("{n} niveaux"), n = length(shared_rv$enrichment_result$levels)) else .tr("Non calcule")),
          row(.tr("Hotspots (Getis-Ord)"), if (!is.null(shared_rv$hotspot_result)) .t_fmt(.tr("{n} elements testes"), n = nrow(shared_rv$hotspot_result)) else .tr("Non calcules")),
          row(.tr("Ripley's K"), if (!is.null(shared_rv$ripley_result)) .t_fmt(.tr("cible '{target}'"), target = shared_rv$ripley_result$target_level) else .tr("Non calcule"))
        )
      )
    })

    .snapshot_results <- function() {
      list(
        qc_metrics = shared_rv$qc_metrics, qc_pass_idx = shared_rv$qc_pass_idx, qc_params = shared_rv$qc_params,
        cluster_labels = shared_rv$cluster_labels, cluster_params = shared_rv$cluster_params,
        cluster_markers = shared_rv$cluster_markers,
        deconv_props = shared_rv$deconv_props, deconv_params = shared_rv$deconv_params,
        moran_results = shared_rv$moran_results, moran_params = shared_rv$moran_params,
        niche_labels = shared_rv$niche_labels, niche_composition = shared_rv$niche_composition,
        niche_params = shared_rv$niche_params, saved_viz_list = shared_rv$saved_viz_list,
        # v2 (vague 5 — Phase 6 stats)
        enrichment_result = shared_rv$enrichment_result, enrichment_params = shared_rv$enrichment_params,
        hotspot_result = shared_rv$hotspot_result, hotspot_params = shared_rv$hotspot_params,
        ripley_result = shared_rv$ripley_result, ripley_params = shared_rv$ripley_params
      )
    }

    output$dl_bundle <- downloadHandler(
      filename = function() {
        nm <- global_data$spatial_obj$project %||% "spatial"
        paste0("export_spatial_", nm, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
      },
      content = function(file) {
        # P1-21 (fix 2026-11-06) : req() nu = abort INVISIBLE (SP11 : aucun
        # download, aucun message). Notifier la cause + l'endroit qui la
        # lève — ici un objet spatial absent de CETTE session (multi-onglets :
        # l'objet vit dans la session où l'import a été fait).
        if (is.null(global_data$spatial_obj)) {
          showNotification(.tr("❌ Export impossible : aucun objet spatial dans cette session — importez un échantillon (onglet « Import Données > Spatial ») ; en session multi-onglets, l'objet appartient à la session où l'import a été fait."), type = "error", duration = 20)
          validate(.tr("Aucun objet spatial chargé."))
          return()
        }
        nid <- showNotification(.tr("⏳ Préparation du paquet complet (.zip) en cours…"), type = "message", duration = NULL, closeButton = FALSE)
        on.exit(try(removeNotification(nid), silent = TRUE), add = TRUE)
        tmp_dir <- tempfile("spatial_export_"); dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

        withProgress(message = .tr("Preparation du paquet complet..."), value = 0.2, {
          written <- tryCatch(
            build_spatial_export_bundle(
              spatial_obj = global_data$spatial_obj,
              results     = .snapshot_results(),
              sections    = input$sections %||% character(0),
              out_dir     = tmp_dir,
              multi_integration = global_data$spatial_multi_integration
            ),
            error = function(e) {
              showNotification(paste(.tr("Erreur export :"), conditionMessage(e)), type = "error", duration = 10)
              character(0)
            }
          )
          incProgress(0.6, detail = .tr("Compression (.zip)..."))
          if (length(written) == 0) {
            fallback <- file.path(tmp_dir, "README.txt")
            writeLines("Aucune section disponible/selectionnee -- rien a exporter.", fallback)
            written <- fallback
          }
          zip::zip(file, files = written, mode = "cherry-pick")
        })
      }
    )

    output$dl_script <- downloadHandler(
      filename = function() {
        nm <- global_data$spatial_obj$project %||% "spatial"
        paste0("script_spatial_", nm, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
      },
      content = function(file) {
        # P1-21 (fix 2026-11-06) : même discipline que dl_bundle — abort
        # invisible remplacé par notification explicite (règle chantier 5).
        if (is.null(global_data$spatial_obj$coords)) {
          showNotification(.tr("❌ Script impossible : aucun objet spatial (coordonnées) dans cette session — importez un échantillon spatial d'abord (onglet « Import Données > Spatial »)."), type = "error", duration = 20)
          validate(.tr("Aucun objet spatial (coordonnées) chargé."))
          return()
        }
        nid <- showNotification(.tr("⏳ Génération du script reproductible (.zip) en cours…"), type = "message", duration = NULL, closeButton = FALSE)
        on.exit(try(removeNotification(nid), silent = TRUE), add = TRUE)
        tmp_dir <- tempfile("spatial_script_"); dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

        script_text <- generate_spatial_pipeline_script(
          spatial_obj = global_data$spatial_obj,
          params = list(qc = shared_rv$qc_params, cluster = shared_rv$cluster_params,
                        deconv = shared_rv$deconv_params, niche = shared_rv$niche_params)
        )
        script_path <- file.path(tmp_dir, sprintf("pipeline_spatial_%s.R", format(Sys.time(), "%Y%m%d_%H%M%S")))
        writeLines(script_text, script_path)

        context_path <- file.path(tmp_dir, "context.rds")
        saveRDS(list(coords = global_data$spatial_obj$coords,
                     project = global_data$spatial_obj$project,
                     technology = global_data$spatial_obj$technology), context_path)

        zip::zip(file, files = c(script_path, context_path), mode = "cherry-pick")
      }
    )
  })
}

# =============================================================================
# modules/spatial/mod_spatial_pipeline.R — Pipeline automatique (1 clic)
# =============================================================================
# v3 (vague 5 — Phase 6 stats) : 3 nouvelles etapes OPTIONNELLES, ajoutees a
#    la fin de la chaine, meme convention que UMAP/Moran (v2) -- checkbox
#    decochee par defaut (cout supplementaire), meme champs shared_rv que si
#    l'etape avait ete lancee manuellement depuis son propre onglet :
#      7. Enrichissement de voisinage (B1, mod_spatial_niche.R) — base =
#         cluster_labels (toujours disponible a ce stade, etape 2 obligatoire).
#      8. Hotspots locaux (B4, mod_spatial_qc.R) — SYNCHRONE (pas de mirai,
#         voir R/utils_spatial_stats.R : compute_getis_ord_hotspots() est
#         volontairement bon marche), sur une metrique QC (metrique par
#         defaut : log_nCount, toujours disponible des l'etape 1).
#      9. Ripley's K (B6, mod_spatial_niche.R) — cible AUTO-SELECTIONNEE =
#         le cluster le plus peuple (deterministe, calcule juste apres
#         l'etape 2) ; pas de selection manuelle possible ici (le pipeline
#         auto n'a pas d'etape intermediaire pour proposer un choix a
#         l'utilisateur) -- pour tester un AUTRE cluster/label, utilisez
#         l'onglet 6 directement.
#    Numerotation des etapes : 6 -> 9 partout (messages de log, table de
#    resume). Les 3 nouvelles etapes reutilisent DIRECTEMENT les fonctions
#    pures de R/utils_spatial_stats.R (deja preloadees dans les daemons,
#    voir R/utils_spatial_async.R), memes noms/signatures que confirmes par
#    l'utilisateur : spatial_neighborhood_enrichment(), ripley_k_random_labeling(),
#    compute_getis_ord_hotspots().
#
# v2 (feedback biologiste — "plus de personnalisation de l'auto-pipeline") :
#   1. Deconvolution : choix de methode complet (RCTD / Label Transfer /
#      STdeconvolve(LDA) / Aucune) au lieu de RCTD uniquement -- memes
#      sous-parametres que l'onglet 3 (mod_spatial_deconv.R). RCTD et Label
#      Transfer restent conditionnes a une reference partagee disponible
#      (Import > Spatial) ; STdeconvolve ne necessite aucune reference.
#   2. 2 etapes optionnelles ajoutees en fin de chaine : PCA+UMAP (sketch,
#      memes resultats que le bouton "Calculer PCA + UMAP" de l'onglet 4,
#      ecrit shared_rv$umap_df) et indice de Moran (memes resultats que le
#      bouton de l'onglet 1, methode "moransi" fixe ici pour rester simple
#      -- markvariogram reste reserve a l'onglet 1 pour l'utilisateur avance).
#
# NEW (moyen terme c, voir handoff_spatial_bio-mg.md) : enchaine QC -> 
# Clustering spatial (BANKSY-lite) -> Deconvolution (methode au choix) ->
# Niches spatiales -> [UMAP] -> [Moran's I] -> [Enrichissement] -> [Hotspots]
# -> [Ripley's K], avec des parametres par defaut identiques a ceux des
# onglets individuels.
#
# Architecture : contrairement au pipeline Single-Cell (mod_sc_pipeline.R,
# synchrone avec shiny::Progress), ce module reste 100% ExtendedTask/mirai
# pour toute etape non-triviale, comme CHAQUE calcul lourd du module Spatial
# (regle du projet : jamais d'objet Seurat/BPCells vivant hors d'un daemon --
# voir R/utils_spatial_io.R header). L'exception assumee est le QC (etape 1)
# et desormais les Hotspots (etape 8), tous deux documentes comme
# volontairement synchrones/bon marche dans leurs fichiers sources
# respectifs (R/utils_spatial_io.R::compute_qc_metrics_fast(),
# R/utils_spatial_stats.R::compute_getis_ord_hotspots()).
#
# Duplication assumee (pas de refactor des onglets existants) : chaque corps
# de tache est copie depuis le module correspondant plutot que factorise
# dans une fonction partagee -- meme philosophie que le pipeline/rapport
# Bulk. Les niches ET les 3 nouvelles etapes stats reutilisent en revanche
# DIRECTEMENT leurs fonctions pures respectives (deja pures, deja
# preloadees dans tous les daemons) plutot que de dupliquer leur corps.
#
# Etat ecrit : chaque etape ecrit dans les MEMES champs shared_rv que les
# onglets 1/2/3/4/6 -- un run du pipeline auto est donc indiscernable, pour
# tout le reste de l'app (onglet 4 Visualisation, Export, Rapport, cache
# par-echantillon), d'un enchainement manuel des boutons individuels.
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

mod_spatial_pipeline_ui <- function(id) {
  ns <- NS(id)
  # ── V1.x UX (spatial container): CONTROLS ONLY. ──────────────────────────
  # This module used to ship a full page (layout_sidebar + results card).
  # The parent container (mod_spatial.R) now owns the layout: these controls
  # are rendered inside the "0. Pipeline auto" accordion panel, and the
  # results card moved to mod_spatial_pipeline_summary_ui() below (right
  # navset). Same namespace/ids => mod_spatial_pipeline_server() unchanged.
  tagList(

      div(class = "alert alert-light", style = "font-size:0.8rem;",
          bsicons::bs_icon("magic"),
          i18n$t(" Enchaine automatiquement, avec des parametres par defaut identiques a ceux des onglets individuels. Chaque etape ecrit ses resultats au MEME endroit que si vous l'aviez lancee manuellement depuis son propre onglet numerote -- vous pouvez ensuite affiner n'importe quelle etape individuellement sans tout relancer.")),

      h6(i18n$t("1. QC"), style = "font-weight:bold;"),
      numericInput(ns("qc_min_count"), i18n$t("nCount minimum"), 100, min = 0, step = 10),
      numericInput(ns("qc_min_features"), i18n$t("nFeature minimum"), 200, min = 0, step = 10),
      sliderInput(ns("qc_max_pct_mt"), i18n$t("% Mitochondrial max"), 0, 100, 20, step = 1),

      hr(),
      h6(i18n$t("2. Clustering (BANKSY-lite)"), style = "font-weight:bold;"),
      sliderInput(ns("lambda"), i18n$t("Lambda (poids spatial)"), 0, 1, 0.8, step = 0.05),
      numericInput(ns("resolution"), i18n$t("Resolution (Leiden)"), 0.8, min = 0.1, max = 3, step = 0.1),

      hr(),
      h6(i18n$t("3. Deconvolution"), style = "font-weight:bold;"),
      uiOutput(ns("deconv_status_ui")),
      radioButtons(ns("deconv_mode"), i18n$t("Methode"),
                   choices = stats::setNames(
                     c("rctd", "labeltransfer", "stdeconvolve", "none"),
                     c(.tr_plain("RCTD (reference partagee)"),
                       .tr_plain("Transfert d'ancres (Label Transfer, reference partagee)"),
                       .tr_plain("Sans reference (LDA, type STdeconvolve)"),
                       .tr_plain("Aucune (ignorer cette etape)"))),
                   selected = "rctd"),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'labeltransfer'", ns("deconv_mode")),
        radioButtons(ns("lt_norm_method"), i18n$t("Normalisation"),
                     choices = stats::setNames(c("lognorm", "sct"),
                                               c(.tr_plain("LogNormalize (rapide)"),
                                                 .tr_plain("SCTransform (lent)"))),
                     selected = "lognorm"),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'sct'", ns("lt_norm_method")),
          numericInput(ns("lt_ncells"), i18n$t("Cellules SCTransform (ncells)"), 3000, min = 500, max = 10000, step = 500)
        ),
        numericInput(ns("lt_npcs"), i18n$t("Composantes PCA"), 30, min = 5, max = 50, step = 5)
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'stdeconvolve'", ns("deconv_mode")),
        numericInput(ns("n_topics"), i18n$t("Nombre de types cellulaires (K)"), 6, min = 2, max = 30, step = 1),
        numericInput(ns("n_top_od"), i18n$t("Genes surdisperses maximum"), 1000, min = 200, max = 3000, step = 100)
      ),

      hr(),
      h6(i18n$t("4. Niches spatiales"), style = "font-weight:bold;"),
      numericInput(ns("n_niches"), i18n$t("Nombre de niches"), 5, min = 2, max = 20, step = 1),

      hr(),
      h6(i18n$t("Analyses complementaires (optionnel)"), style = "font-weight:bold;"),
      checkboxInput(ns("compute_umap"), i18n$t("PCA + UMAP (sketch, onglet 4)"), value = TRUE),
      checkboxInput(ns("compute_moran"), i18n$t("Indice de Moran / genes spatialement variables (onglet 1)"), value = FALSE),
      conditionalPanel(
        condition = sprintf("input['%s']", ns("compute_moran")),
        numericInput(ns("n_hvg_moran"), i18n$t("Nombre de genes (HVG)"), 1000, min = 100, max = 5000, step = 100)
      ),

      hr(),
      h6(i18n$t("Statistiques spatiales avancees (optionnel, onglets 1/6)"), style = "font-weight:bold;"),
      div(class = "text-muted", style = "font-size:0.7rem;",
          i18n$t("Ajoutent chacune un peu de temps de calcul -- decochees par defaut. Consultez les onglets \"1. QC\" et \"6. Niches spatiales\" pour l'explication de chaque test.")),
      checkboxInput(ns("compute_enrichment"), i18n$t("Enrichissement de voisinage (co-occurrence, base = clusters)"), value = FALSE),
      conditionalPanel(
        condition = sprintf("input['%s']", ns("compute_enrichment")),
        numericInput(ns("k_neighbors_enrich"), i18n$t("Voisins spatiaux (k)"), 30, min = 5, max = 200, step = 5),
        numericInput(ns("n_perm_enrich"), i18n$t("Permutations"), 200, min = 50, max = 1000, step = 50)
      ),
      checkboxInput(ns("compute_hotspots"), i18n$t("Hotspots locaux (Getis-Ord Gi*, metrique QC)"), value = FALSE),
      conditionalPanel(
        condition = sprintf("input['%s']", ns("compute_hotspots")),
        selectInput(ns("hotspot_metric"), i18n$t("Metrique"),
                    choices = c("nCount", "nFeature", "pct_mt", "pct_ribo", "log_nCount"),
                    selected = "log_nCount"),
        numericInput(ns("k_neighbors_hotspot"), i18n$t("Voisins spatiaux (k)"), 30, min = 5, max = 200, step = 5)
      ),
      checkboxInput(ns("compute_ripley"), i18n$t("Ripley's K (etiquetage aleatoire, cible = cluster majoritaire)"), value = FALSE),
      conditionalPanel(
        condition = sprintf("input['%s']", ns("compute_ripley")),
        numericInput(ns("n_perm_ripley"), i18n$t("Permutations"), 199, min = 49, max = 499, step = 10),
        div(class = "text-muted", style = "font-size:0.68rem;",
            i18n$t("Cible choisie automatiquement = le cluster le plus peuple apres l'etape 2. Pour tester un autre cluster/niche/type cellulaire, utilisez l'onglet 6 directement."))
      ),

      hr(),
      actionButton(ns("btn_run_all"), i18n$t("\U0001F680 Lancer le pipeline complet"),
                   class = "btn-danger w-100", icon = icon("bolt")),
      div(class = "mt-2", uiOutput(ns("pipeline_status_ui"))),
      div(class = "bg-light border rounded p-2 mt-2",
          style = "max-height:220px; overflow-y:auto; white-space:pre-wrap; font-family:monospace; font-size:0.72rem;",
          verbatimTextOutput(ns("pipeline_log_text"), placeholder = TRUE))
  )
}

# ── Summary card (V1.x UX spatial container) ─────────────────────────────────
# Rendered in the parent container's right navset ("Résumé Pipeline"). Uses the
# SAME namespace as mod_spatial_pipeline_ui() (call with ns("pipeline")), so the
# existing output$pipeline_summary_ui binding in mod_spatial_pipeline_server()
# drives it without any server change.
mod_spatial_pipeline_summary_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header(i18n$t("Resultats du pipeline")),
    uiOutput(ns("pipeline_summary_ui")),
    div(class = "alert alert-light small mt-2",
        bsicons::bs_icon("compass"),
        i18n$t(" Consultez les onglets numerotes (1 a 6) pour explorer/affiner chaque resultat en detail, ou les onglets \"7. Export\" / \"8. Rapport\" pour tout regrouper dans un paquet .zip / script R reproductible / rapport HTML-PDF."))
  )
}

mod_spatial_pipeline_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {

    # Session-scoped scalar translation (plain strings, never HTML spans).
    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # i18n: push translated labels/choices for build-time-frozen inputs on
    # every language change (values NEVER change; selection is preserved).
    observeEvent(global_data$language, {
      updateNumericInput(session, "qc_min_count", label = .tr("nCount minimum"))
      updateNumericInput(session, "qc_min_features", label = .tr("nFeature minimum"))
      updateSliderInput(session, "qc_max_pct_mt", label = .tr("% Mitochondrial max"))
      updateSliderInput(session, "lambda", label = .tr("Lambda (poids spatial)"))
      updateNumericInput(session, "resolution", label = .tr("Resolution (Leiden)"))
      updateRadioButtons(session, "deconv_mode",
        label = .tr("Methode"),
        choices = stats::setNames(
          c("rctd", "labeltransfer", "stdeconvolve", "none"),
          c(.tr("RCTD (reference partagee)"),
            .tr("Transfert d'ancres (Label Transfer, reference partagee)"),
            .tr("Sans reference (LDA, type STdeconvolve)"),
            .tr("Aucune (ignorer cette etape)"))))
      updateRadioButtons(session, "lt_norm_method",
        label = .tr("Normalisation"),
        choices = stats::setNames(c("lognorm", "sct"),
                                  c(.tr("LogNormalize (rapide)"), .tr("SCTransform (lent)"))))
      updateNumericInput(session, "lt_ncells", label = .tr("Cellules SCTransform (ncells)"))
      updateNumericInput(session, "lt_npcs", label = .tr("Composantes PCA"))
      updateNumericInput(session, "n_topics", label = .tr("Nombre de types cellulaires (K)"))
      updateNumericInput(session, "n_top_od", label = .tr("Genes surdisperses maximum"))
      updateNumericInput(session, "n_niches", label = .tr("Nombre de niches"))
      updateCheckboxInput(session, "compute_umap", label = .tr("PCA + UMAP (sketch, onglet 4)"))
      updateCheckboxInput(session, "compute_moran",
        label = .tr("Indice de Moran / genes spatialement variables (onglet 1)"))
      updateNumericInput(session, "n_hvg_moran", label = .tr("Nombre de genes (HVG)"))
      updateCheckboxInput(session, "compute_enrichment",
        label = .tr("Enrichissement de voisinage (co-occurrence, base = clusters)"))
      updateNumericInput(session, "k_neighbors_enrich", label = .tr("Voisins spatiaux (k)"))
      updateNumericInput(session, "n_perm_enrich", label = .tr("Permutations"))
      updateCheckboxInput(session, "compute_hotspots",
        label = .tr("Hotspots locaux (Getis-Ord Gi*, metrique QC)"))
      updateSelectInput(session, "hotspot_metric", label = .tr("Metrique"),
                        choices = c("nCount", "nFeature", "pct_mt", "pct_ribo", "log_nCount"),
                        selected = input$hotspot_metric %||% "log_nCount")
      updateNumericInput(session, "k_neighbors_hotspot", label = .tr("Voisins spatiaux (k)"))
      updateCheckboxInput(session, "compute_ripley",
        label = .tr("Ripley's K (etiquetage aleatoire, cible = cluster majoritaire)"))
      updateNumericInput(session, "n_perm_ripley", label = .tr("Permutations"))
      updateActionButton(session, "btn_run_all", label = .tr("\U0001F680 Lancer le pipeline complet"))
    }, ignoreInit = TRUE)

    log_file <- spatial_log_path(session, "auto_pipeline")
    tracker  <- create_reactive_tracker(session, log_file)
    pipeline_state <- reactiveVal("idle")   # idle | running | done | error
    deconv_mode_decided <- reactiveVal("none")
    TOTAL_STEPS <- 9L

    output$deconv_status_ui <- renderUI({
      global_data$language  # re-render on language switch
      if (!is.null(global_data$spatial_reference)) {
        div(class = "alert alert-success", style = "font-size:0.75rem;",
            bsicons::bs_icon("check-circle"),
            sprintf(.tr(" Reference partagee disponible (%s cellules) \u2014 utilisable par RCTD/Label Transfer."),
                    format(global_data$spatial_reference$n_cells %||% 0, big.mark = ",")))
      } else {
        div(class = "alert alert-warning", style = "font-size:0.75rem;",
            bsicons::bs_icon("exclamation-triangle"),
            .tr(" Aucune reference partagee (onglet Import > Spatial) \u2014 RCTD/Label Transfer seront ignores meme si selectionnes ; choisissez \"Sans reference (LDA)\" ou \"Aucune\"."))
      }
    })

    output$pipeline_status_ui <- renderUI({
      global_data$language  # re-render on language switch
      switch(pipeline_state(),
        "idle"    = tags$span(class = "text-muted", .tr("En attente.")),
        "running" = tags$span(class = "text-info", .tr("\u23f3 Pipeline en cours... (voir le journal ci-dessous)")),
        "done"    = tags$span(class = "text-success", .tr("\u2705 Pipeline termine.")),
        "error"   = tags$span(class = "text-danger", .tr("\u274c Erreur \u2014 voir le journal ci-dessous.")),
        tags$span(.tr("En attente."))
      )
    })

    output$pipeline_log_text <- renderText({
      global_data$language  # re-render on language switch
      lines <- tracker()
      if (length(lines) == 0) return(.tr("En attente..."))
      paste(lines, collapse = "\n")
    })

    # ── Etape 2 : Clustering (BANKSY-lite) -- duplication assumee, voir header ──
    cluster_task <- ExtendedTask$new(function(bpcells_dir, pass_idx, coords,
                                               lambda, k_geom, npcs, resolution, log_file) {
      mirai::mirai(
        {
          if (!requireNamespace("RANN", quietly = TRUE)) {
            stop(errorCondition("Package 'RANN' requis.",
                                class = "spatial_pipeline_error"))
          }
          write_mirai_log(log_file, "[2/9] Ouverture BPCells...", 1, 4)
          mat <- BPCells::open_matrix_dir(bpcells_dir)
          if (!is.null(pass_idx)) mat <- mat[, pass_idx, drop = FALSE]
          obj <- Seurat::CreateSeuratObject(counts = mat)
          obj <- Seurat::NormalizeData(obj, verbose = FALSE)
          obj <- Seurat::FindVariableFeatures(obj, verbose = FALSE)
          var_feat <- Seurat::VariableFeatures(obj)
          coords_df <- coords[match(colnames(obj), coords$id), c("x", "y")]
          rownames(coords_df) <- colnames(obj)
          keep <- stats::complete.cases(coords_df)
          obj <- obj[, keep]
          coords_mat <- as.matrix(coords_df[keep, , drop = FALSE])
          write_mirai_log(log_file, "[2/9] Voisinage spatial (RANN)...", 2, 4)
          nn <- RANN::nn2(coords_mat, k = min(k_geom + 1, nrow(coords_mat)))
          neighbor_idx <- nn$nn.idx[, -1, drop = FALSE]
          own_mat <- t(as.matrix(SeuratObject::LayerData(obj, layer = "data")[var_feat, , drop = FALSE]))
          n <- nrow(own_mat); kk <- ncol(neighbor_idx)
          W <- Matrix::sparseMatrix(i = rep(seq_len(n), each = kk), j = as.vector(t(neighbor_idx)),
                                     x = 1 / kk, dims = c(n, n))
          nbr_mat <- as.matrix(W %*% own_mat); dimnames(nbr_mat) <- dimnames(own_mat)
          own_scaled <- scale(own_mat); own_scaled[!is.finite(own_scaled)] <- 0
          nbr_scaled <- scale(nbr_mat); nbr_scaled[!is.finite(nbr_scaled)] <- 0
          augmented <- cbind(sqrt(1 - lambda) * own_scaled, sqrt(lambda) * nbr_scaled)
          write_mirai_log(log_file, "[2/9] PCA...", 3, 4)
          n_pc <- max(2, min(npcs, ncol(augmented) - 1, nrow(augmented) - 1))
          pca <- if (requireNamespace("irlba", quietly = TRUE)) {
            irlba::prcomp_irlba(augmented, n = n_pc, center = FALSE, scale. = FALSE)
          } else stats::prcomp(augmented, rank. = n_pc, center = FALSE, scale. = FALSE)
          emb <- pca$x; rownames(emb) <- colnames(obj); colnames(emb) <- paste0("BANKSYPCA_", seq_len(ncol(emb)))
          obj[["BANKSY_PCA"]] <- Seurat::CreateDimReducObject(embeddings = emb, key = "BANKSYPCA_",
                                                               assay = Seurat::DefaultAssay(obj))
          obj <- Seurat::FindNeighbors(obj, reduction = "BANKSY_PCA", dims = seq_len(n_pc), verbose = FALSE)
          write_mirai_log(log_file, "[2/9] Clustering Leiden...", 4, 4)
          obj <- tryCatch(Seurat::FindClusters(obj, resolution = resolution, algorithm = 4, verbose = FALSE),
                          error = function(e) Seurat::FindClusters(obj, resolution = resolution, algorithm = 1, verbose = FALSE))
          setNames(as.character(obj$seurat_clusters), colnames(obj))
        },
        bpcells_dir = bpcells_dir, pass_idx = pass_idx, coords = coords,
        lambda = lambda, k_geom = k_geom, npcs = npcs, resolution = resolution,
        log_file = log_file, .timeout = MIRAI_TASK_TIMEOUT_MS
      )
    })

    # ── Etape 3 : Deconvolution (RCTD / Label Transfer / STdeconvolve) --
    # duplication complete du corps de mod_spatial_deconv.R::deconv_task
    # (memes 3 branches, memes timeouts dedies). ────────────────────────────
    deconv_task <- ExtendedTask$new(function(bpcells_dir, pass_idx, coords, mode, ref_path,
                                             n_topics, n_top_od, lt_npcs, lt_norm_method,
                                             lt_ncells, min_shared_genes, log_file) {
      mirai::mirai(
        {
          run_spatial_deconv_body(
            bpcells_dir = bpcells_dir, pass_idx = pass_idx, coords = coords,
            mode = mode, ref_path = ref_path, n_topics = n_topics,
            n_top_od = n_top_od, lt_npcs = lt_npcs, lt_norm_method = lt_norm_method,
            lt_ncells = lt_ncells, min_shared_genes = min_shared_genes,
            log_file = log_file
          )
        },
        bpcells_dir = bpcells_dir, pass_idx = pass_idx, coords = coords, mode = mode,
        ref_path = ref_path, n_topics = n_topics, n_top_od = n_top_od, lt_npcs = lt_npcs,
        lt_norm_method = lt_norm_method, lt_ncells = lt_ncells, min_shared_genes = min_shared_genes,
        log_file = log_file,
        .timeout = switch(mode, "labeltransfer" = LABEL_TRANSFER_TIMEOUT_MS,
                          "rctd" = RCTD_TIMEOUT_MS, MIRAI_TASK_TIMEOUT_MS)
      )
    })
    # ── Etape 4 : Niches -- reutilise DIRECTEMENT la fonction pure existante ──
    niche_task <- ExtendedTask$new(function(coords, group_labels, n_niches, log_file) {
      mirai::mirai(
        {
          write_mirai_log(log_file, "[4/9] Calcul des niches...", 1, 1)
          compute_spatial_niches(coords = coords, group_labels = group_labels,
                                  k_neighbors = 30, n_niches = n_niches, log_file = NULL)
        },
        coords = coords, group_labels = group_labels, n_niches = n_niches, log_file = log_file,
        .timeout = MIRAI_TASK_TIMEOUT_MS
      )
    })

    # ── Etape 5 (optionnelle) : PCA + UMAP (sketch) -- duplication du corps
    # de mod_spatial_viz.R::umap_task. ───────────────────────────────────────
    umap_task <- ExtendedTask$new(function(sketch_path, log_file) {
      mirai::mirai(
        {
          write_mirai_log(log_file, "[5/9] Chargement du sketch...", 1, 4)
          sk <- readRDS(sketch_path)
          already_sct <- identical(Seurat::DefaultAssay(sk), "SCT")
          if (!already_sct) {
            if (!"data" %in% SeuratObject::Layers(sk)) sk <- Seurat::NormalizeData(sk, verbose = FALSE)
            sk <- Seurat::FindVariableFeatures(sk, verbose = FALSE)
            sk <- Seurat::ScaleData(sk, verbose = FALSE)
          }
          write_mirai_log(log_file, "[5/9] PCA...", 2, 4)
          sk <- Seurat::RunPCA(sk, npcs = 30, verbose = FALSE)
          write_mirai_log(log_file, "[5/9] UMAP...", 3, 4)
          sk <- Seurat::RunUMAP(sk, dims = 1:30, verbose = FALSE)
          write_mirai_log(log_file, "[5/9] Termine.", 4, 4)
          emb <- as.data.frame(Seurat::Embeddings(sk, "umap"))
          colnames(emb)[1:2] <- c("dim1", "dim2")
          emb$id <- rownames(emb)
          emb
        },
        sketch_path = sketch_path, log_file = log_file, .timeout = MIRAI_TASK_TIMEOUT_MS
      )
    })

    # ── Etape 6 (optionnelle) : Indice de Moran -- duplication du corps de
    # mod_spatial_qc.R::moran_task (methode "moransi" fixe ici, voir header). ──
    moran_task <- ExtendedTask$new(function(bpcells_dir, pass_idx, coords, n_hvg, log_file) {
      mirai::mirai(
        {
          write_mirai_log(log_file, "[6/9] Ouverture BPCells...", 1, 4)
          mat <- BPCells::open_matrix_dir(bpcells_dir)
          if (!is.null(pass_idx)) mat <- mat[, pass_idx, drop = FALSE]
          obj <- Seurat::CreateSeuratObject(counts = mat)
          obj <- Seurat::NormalizeData(obj, verbose = FALSE)
          obj <- Seurat::FindVariableFeatures(obj, nfeatures = n_hvg, verbose = FALSE)
          hvgs <- Seurat::VariableFeatures(obj)
          write_mirai_log(log_file, "[6/9] Alignement coordonnees...", 2, 4)
          coords_df <- coords[match(colnames(obj), coords$id), c("x", "y")]
          rownames(coords_df) <- colnames(obj)
          keep <- stats::complete.cases(coords_df)
          coords_df <- coords_df[keep, , drop = FALSE]
          obj <- obj[, rownames(coords_df)]
          write_mirai_log(log_file, sprintf("[6/9] Indice de Moran sur %d genes...", length(hvgs)), 3, 4)
          assay_res <- Seurat::FindSpatiallyVariableFeatures(
            object = obj[["RNA"]], layer = "data", features = hvgs, spatial.location = coords_df,
            selection.method = "moransi", nfeatures = length(hvgs), verbose = FALSE
          )
          info <- SeuratObject::SVFInfo(assay_res, method = "moransi")
          write_mirai_log(log_file, "[6/9] Termine.", 4, 4)
          obs_col <- grep("observed$", colnames(info), value = TRUE)[1]
          pv_col  <- grep("p\\.value$|pvalue$", colnames(info), value = TRUE)[1]
          data.frame(gene = rownames(info),
                    moran_i = if (!is.na(obs_col)) info[[obs_col]] else NA_real_,
                    p_value = if (!is.na(pv_col)) info[[pv_col]] else NA_real_,
                    row.names = NULL, stringsAsFactors = FALSE)
        },
        bpcells_dir = bpcells_dir, pass_idx = pass_idx, coords = coords, n_hvg = n_hvg,
        log_file = log_file, .timeout = MIRAI_TASK_TIMEOUT_MS
      )
    })

    # ── Etape 7 (optionnelle, vague 5) : Enrichissement de voisinage (B1) --
    # reutilise DIRECTEMENT spatial_neighborhood_enrichment() (deja pure,
    # preloadee dans les daemons). Base = cluster_labels (etape 2). ─────────
    enrichment_task <- ExtendedTask$new(function(coords, group_labels, k_neighbors, n_perm, log_file) {
      mirai::mirai(
        {
          write_mirai_log(log_file, "[7/9] Enrichissement de voisinage...", 1, 1)
          spatial_neighborhood_enrichment(coords = coords, group_labels = group_labels,
                                          k_neighbors = k_neighbors, n_perm = n_perm, log_file = NULL)
        },
        coords = coords, group_labels = group_labels, k_neighbors = k_neighbors, n_perm = n_perm,
        log_file = log_file, .timeout = MIRAI_TASK_TIMEOUT_MS
      )
    })

    # ── Etape 9 (optionnelle, vague 5) : Ripley's K (B6) -- reutilise
    # DIRECTEMENT ripley_k_random_labeling(). Cible auto-selectionnee = le
    # cluster le plus peuple (voir .launch_ripley_or_finish() ci-dessous). ──
    ripley_task <- ExtendedTask$new(function(coords, group_labels, target_level, n_perm, log_file) {
      mirai::mirai(
        {
          write_mirai_log(log_file, "[9/9] Ripley's K (etiquetage aleatoire)...", 1, 1)
          ripley_k_random_labeling(coords = coords, group_labels = group_labels,
                                   target_level = target_level, n_perm = n_perm, log_file = NULL)
        },
        coords = coords, group_labels = group_labels, target_level = target_level, n_perm = n_perm,
        log_file = log_file, .timeout = MIRAI_TASK_TIMEOUT_MS
      )
    })

    .launch_niche <- function() {
      write_mirai_log(log_file, "Etape 4/9 : Niches spatiales (basees sur le clustering)...", 4, TOTAL_STEPS)
      niche_task$invoke(coords = global_data$spatial_obj$coords, group_labels = shared_rv$cluster_labels,
                        n_niches = input$n_niches, log_file = log_file)
    }

    .launch_umap_or_moran <- function() {
      if (isTRUE(input$compute_umap)) {
        write_mirai_log(log_file, "Etape 5/9 : PCA + UMAP (sketch)...", 5, TOTAL_STEPS)
        tmp <- tempfile(fileext = ".rds")
        saveRDS(global_data$spatial_obj$sketch, tmp)
        umap_task$invoke(sketch_path = tmp, log_file = log_file)
      } else {
        write_mirai_log(log_file, "Etape 5/9 : UMAP ignore (non coche).", 5, TOTAL_STEPS)
        .launch_moran()
      }
    }

    .launch_moran <- function() {
      if (isTRUE(input$compute_moran)) {
        write_mirai_log(log_file, "Etape 6/9 : Indice de Moran...", 6, TOTAL_STEPS)
        moran_task$invoke(
          bpcells_dir = global_data$spatial_obj$bpcells_dir, pass_idx = shared_rv$qc_pass_idx,
          coords = global_data$spatial_obj$coords, n_hvg = input$n_hvg_moran %||% 1000, log_file = log_file
        )
      } else {
        write_mirai_log(log_file, "Etape 6/9 : Moran ignore (non coche).", 6, TOTAL_STEPS)
        .launch_enrichment()
      }
    }

    .launch_enrichment <- function() {
      if (isTRUE(input$compute_enrichment)) {
        write_mirai_log(log_file, "Etape 7/9 : Enrichissement de voisinage...", 7, TOTAL_STEPS)
        enrichment_task$invoke(
          coords = global_data$spatial_obj$coords, group_labels = shared_rv$cluster_labels,
          k_neighbors = input$k_neighbors_enrich %||% 30, n_perm = input$n_perm_enrich %||% 200,
          log_file = log_file
        )
      } else {
        write_mirai_log(log_file, "Etape 7/9 : Enrichissement ignore (non coche).", 7, TOTAL_STEPS)
        .launch_hotspots()
      }
    }

    .launch_hotspots <- function() {
      # SYNCHRONE (voir header) -- pas de mirai/ExtendedTask, log + resultat
      # immediats, on enchaine directement sur l'etape suivante.
      if (isTRUE(input$compute_hotspots)) {
        write_mirai_log(log_file, "Etape 8/9 : Hotspots locaux (Getis-Ord Gi*)...", 8, TOTAL_STEPS)
        req(shared_rv$qc_metrics)
        metric <- input$hotspot_metric %||% "log_nCount"
        values <- stats::setNames(shared_rv$qc_metrics[[metric]], shared_rv$qc_metrics$id)
        res <- tryCatch(
          compute_getis_ord_hotspots(coords = global_data$spatial_obj$coords, values = values,
                                     k_neighbors = input$k_neighbors_hotspot %||% 30),
          error = function(e) {
            write_mirai_log(log_file, paste("Etape 8/9 : Hotspots echoues --", conditionMessage(e)), 8, TOTAL_STEPS)
            NULL
          }
        )
        if (!is.null(res)) {
          shared_rv$hotspot_result <- res
          shared_rv$hotspot_params <- list(source = "qc", metric = metric, k_neighbors = input$k_neighbors_hotspot %||% 30)
          write_mirai_log(log_file, "Etape 8/9 : Hotspots termines.", 8, TOTAL_STEPS)
        }
      } else {
        write_mirai_log(log_file, "Etape 8/9 : Hotspots ignores (non coche).", 8, TOTAL_STEPS)
      }
      .launch_ripley_or_finish()
    }

    .launch_ripley_or_finish <- function() {
      if (isTRUE(input$compute_ripley)) {
        cl <- shared_rv$cluster_labels
        if (is.null(cl) || length(cl) == 0) {
          write_mirai_log(log_file, "Etape 9/9 : Ripley's K ignore (aucun cluster disponible).", 9, TOTAL_STEPS)
          pipeline_state("done")
          showNotification(.tr("\u2705 Pipeline automatique termine."), type = "message", duration = 6)
          return()
        }
        target_level <- names(sort(table(cl), decreasing = TRUE))[1]
        write_mirai_log(log_file, sprintf("Etape 9/9 : Ripley's K (cible auto = cluster '%s')...", target_level),
                        9, TOTAL_STEPS)
        ripley_task$invoke(
          coords = global_data$spatial_obj$coords, group_labels = cl, target_level = target_level,
          n_perm = input$n_perm_ripley %||% 199, log_file = log_file
        )
      } else {
        write_mirai_log(log_file, "Etape 9/9 : Ripley's K ignore (non coche). Pipeline termine.", 9, TOTAL_STEPS)
        pipeline_state("done")
        showNotification(.tr("\u2705 Pipeline automatique termine."), type = "message", duration = 6)
      }
    }

    observeEvent(input$btn_run_all, {
      req(global_data$spatial_obj$bpcells_dir, global_data$spatial_obj$coords)
      if (identical(pipeline_state(), "running")) return()   # re-entrance guard

      reset_log(log_file)
      pipeline_state("running")

      write_mirai_log(log_file, "Etape 1/9 : QC (seuils appliques)...", 1, TOTAL_STEPS)
      qc_metrics <- tryCatch(compute_qc_metrics_fast(global_data$spatial_obj$bpcells_dir),
                             error = function(e) NULL)
      if (is.null(qc_metrics)) {
        write_mirai_log(log_file, "Erreur QC -- pipeline interrompu.", 1, TOTAL_STEPS)
        pipeline_state("error")
        showNotification(.tr("Erreur lors du calcul QC (pipeline auto)."), type = "error", duration = 8)
        return()
      }
      shared_rv$qc_metrics <- qc_metrics
      pass <- with(qc_metrics, nCount >= input$qc_min_count & nFeature >= input$qc_min_features &
                     (is.na(pct_mt) | pct_mt <= input$qc_max_pct_mt))
      pass_idx <- which(pass)
      shared_rv$qc_pass_idx <- pass_idx
      shared_rv$qc_params <- list(min_count = input$qc_min_count, min_features = input$qc_min_features,
                                   max_pct_mt = input$qc_max_pct_mt)
      write_mirai_log(log_file, sprintf("QC : %d/%d elements conserves.", length(pass_idx), nrow(qc_metrics)),
                      1, TOTAL_STEPS)

      mode_req <- input$deconv_mode %||% "none"
      deconv_mode_decided(
        if (mode_req %in% c("rctd", "labeltransfer") && is.null(global_data$spatial_reference)) "none"
        else mode_req
      )

      write_mirai_log(log_file, "Etape 2/9 : Clustering spatial (BANKSY-lite)...", 2, TOTAL_STEPS)
      cluster_task$invoke(
        bpcells_dir = global_data$spatial_obj$bpcells_dir, pass_idx = pass_idx,
        coords = global_data$spatial_obj$coords, lambda = input$lambda, k_geom = 18,
        npcs = 30, resolution = input$resolution, log_file = log_file
      )
    })

    observeEvent(cluster_task$status(), {
      req(identical(pipeline_state(), "running"))
      st <- cluster_task$status()
      if (identical(st, "success")) {
        shared_rv$cluster_labels <- cluster_task$result()
        shared_rv$cluster_params <- list(lambda = input$lambda, k_geom = 18, npcs = 30, resolution = input$resolution)
        write_mirai_log(log_file, sprintf("Clustering termine : %d clusters.",
                                          length(unique(shared_rv$cluster_labels))), 2, TOTAL_STEPS)
        mode <- deconv_mode_decided()
        if (!identical(mode, "none")) {
          write_mirai_log(log_file, sprintf("Etape 3/9 : Deconvolution (%s)...", mode), 3, TOTAL_STEPS)
          deconv_task$invoke(
            bpcells_dir = global_data$spatial_obj$bpcells_dir, pass_idx = shared_rv$qc_pass_idx,
            coords = global_data$spatial_obj$coords, mode = mode,
            ref_path = if (mode %in% c("rctd", "labeltransfer")) global_data$spatial_reference$path else NULL,
            n_topics = input$n_topics %||% 6, n_top_od = input$n_top_od %||% 1000,
            lt_npcs = input$lt_npcs %||% 30, lt_norm_method = input$lt_norm_method %||% "lognorm",
            lt_ncells = input$lt_ncells %||% 3000, min_shared_genes = LABEL_TRANSFER_MIN_SHARED_GENES,
            log_file = log_file
          )
        } else {
          write_mirai_log(log_file, "Etape 3/9 : Deconvolution ignoree (aucune methode / pas de reference).", 3, TOTAL_STEPS)
          .launch_niche()
        }
      } else if (identical(st, "error")) {
        write_mirai_log(log_file, "Erreur pendant le clustering -- pipeline interrompu.", 2, TOTAL_STEPS)
        pipeline_state("error")
        showNotification(.tr("Erreur pendant le clustering (pipeline auto) -- voir le journal."), type = "error", duration = 10)
      }
    })

    observeEvent(deconv_task$status(), {
      req(identical(pipeline_state(), "running"), !identical(deconv_mode_decided(), "none"))
      st <- deconv_task$status()
      if (identical(st, "success")) {
        shared_rv$deconv_props <- deconv_task$result()
        shared_rv$deconv_params <- list(
          mode = deconv_mode_decided(),
          ref_path = if (deconv_mode_decided() %in% c("rctd", "labeltransfer")) global_data$spatial_reference$path else NULL,
          ref_source_label = if (deconv_mode_decided() %in% c("rctd", "labeltransfer")) "reference partagee (Import > Spatial)" else NULL,
          n_topics = input$n_topics, n_top_od = input$n_top_od,
          lt_npcs = input$lt_npcs, lt_norm_method = input$lt_norm_method, lt_ncells = input$lt_ncells
        )
        write_mirai_log(log_file, "Deconvolution terminee.", 3, TOTAL_STEPS)
        .launch_niche()
      } else if (identical(st, "error")) {
        write_mirai_log(log_file, "Deconvolution echouee -- poursuite sans elle (niches basees sur le clustering).", 3, TOTAL_STEPS)
        .launch_niche()
      }
    })

    observeEvent(niche_task$status(), {
      req(identical(pipeline_state(), "running"))
      st <- niche_task$status()
      if (identical(st, "success")) {
        res <- niche_task$result()
        shared_rv$niche_labels      <- stats::setNames(res$assignments$niche, res$assignments$id)
        shared_rv$niche_composition <- res$niche_composition
        shared_rv$niche_params <- list(group_by = "cluster", k_neighbors = 30, n_niches = input$n_niches)
        write_mirai_log(log_file, "Niches terminees.", 4, TOTAL_STEPS)
        .launch_umap_or_moran()
      } else if (identical(st, "error")) {
        write_mirai_log(log_file, "Erreur pendant le calcul des niches.", 4, TOTAL_STEPS)
        pipeline_state("error")
        showNotification(.tr("Erreur pendant le calcul des niches (pipeline auto) -- voir le journal."), type = "error", duration = 10)
      }
    })

    observeEvent(umap_task$status(), {
      req(identical(pipeline_state(), "running"), isTRUE(input$compute_umap))
      st <- umap_task$status()
      if (identical(st, "success")) {
        shared_rv$umap_df <- umap_task$result()
        write_mirai_log(log_file, "UMAP termine.", 5, TOTAL_STEPS)
        .launch_moran()
      } else if (identical(st, "error")) {
        write_mirai_log(log_file, "UMAP echoue -- poursuite sans lui.", 5, TOTAL_STEPS)
        .launch_moran()
      }
    })

    observeEvent(moran_task$status(), {
      req(identical(pipeline_state(), "running"), isTRUE(input$compute_moran))
      st <- moran_task$status()
      if (identical(st, "success")) {
        shared_rv$moran_results <- moran_task$result()
        shared_rv$moran_params <- list(n_hvg = input$n_hvg_moran %||% 1000, x_cuts = 0, y_cuts = 0, method = "moransi")
        write_mirai_log(log_file, "Moran termine.", 6, TOTAL_STEPS)
      } else if (identical(st, "error")) {
        write_mirai_log(log_file, "Indice de Moran echoue.", 6, TOTAL_STEPS)
      }
      .launch_enrichment()
    })

    observeEvent(enrichment_task$status(), {
      req(identical(pipeline_state(), "running"), isTRUE(input$compute_enrichment))
      st <- enrichment_task$status()
      if (identical(st, "success")) {
        shared_rv$enrichment_result <- enrichment_task$result()
        shared_rv$enrichment_params <- list(group_by = "cluster", k_neighbors = input$k_neighbors_enrich %||% 30,
                                            n_perm = input$n_perm_enrich %||% 200)
        write_mirai_log(log_file, "Enrichissement termine.", 7, TOTAL_STEPS)
      } else if (identical(st, "error")) {
        write_mirai_log(log_file, "Enrichissement de voisinage echoue.", 7, TOTAL_STEPS)
      }
      .launch_hotspots()
    })

    observeEvent(ripley_task$status(), {
      req(identical(pipeline_state(), "running"), isTRUE(input$compute_ripley))
      st <- ripley_task$status()
      if (identical(st, "success")) {
        shared_rv$ripley_result <- ripley_task$result()
        shared_rv$ripley_params <- list(group_by = "cluster", target = shared_rv$ripley_result$target_level,
                                        n_perm = input$n_perm_ripley %||% 199)
        write_mirai_log(log_file, "Ripley's K termine.", 9, TOTAL_STEPS)
      } else if (identical(st, "error")) {
        write_mirai_log(log_file, "Ripley's K echoue.", 9, TOTAL_STEPS)
      }
      pipeline_state("done")
      showNotification(.tr("\u2705 Pipeline automatique termine."), type = "message", duration = 6)
    })

    output$pipeline_summary_ui <- renderUI({
      global_data$language  # re-render on language switch
      row <- function(label, value) tags$tr(tags$td(strong(label)), tags$td(value))
      tags$table(class = "table table-sm table-striped",
        row(.tr("1. QC"),
            if (!is.null(shared_rv$qc_pass_idx)) .t_fmt(.tr("{n} elements retenus"), n = length(shared_rv$qc_pass_idx)) else "-"),
        row(.tr("2. Clustering"),
            if (!is.null(shared_rv$cluster_labels)) .t_fmt(.tr("{n} clusters"), n = length(unique(shared_rv$cluster_labels))) else .tr("Non calcule")),
        row(.tr("3. Deconvolution"),
            if (!is.null(shared_rv$deconv_props)) sprintf(.tr("%d types cellulaires (%s)"), ncol(shared_rv$deconv_props) - 1, shared_rv$deconv_params$mode %||% "?") else .tr("Non calculee / ignoree")),
        row(.tr("4. Niches"),
            if (!is.null(shared_rv$niche_labels)) .t_fmt(.tr("{n} niches"), n = length(unique(shared_rv$niche_labels))) else .tr("Non calculees")),
        row(.tr("5. UMAP"),
            if (!is.null(shared_rv$umap_df)) sprintf(.tr("%d points (sketch)"), nrow(shared_rv$umap_df)) else .tr("Non calcule / ignore")),
        row(.tr("6. Moran's I"),
            if (!is.null(shared_rv$moran_results)) .t_fmt(.tr("{n} genes testes"), n = nrow(shared_rv$moran_results)) else .tr("Non calcule / ignore")),
        row(.tr("7. Enrichissement"),
            if (!is.null(shared_rv$enrichment_result)) sprintf(.tr("%d niveaux (%d permutations)"), length(shared_rv$enrichment_result$levels), shared_rv$enrichment_result$n_perm) else .tr("Non calcule / ignore")),
        row(.tr("8. Hotspots (Gi*)"),
            if (!is.null(shared_rv$hotspot_result)) .t_fmt(.tr("{n} elements testes"), n = nrow(shared_rv$hotspot_result)) else .tr("Non calcule / ignore")),
        row(.tr("9. Ripley's K"),
            if (!is.null(shared_rv$ripley_result)) sprintf(.tr("cible '%s' (%s)"), shared_rv$ripley_result$target_level, if (isTRUE(shared_rv$ripley_result$subsampled)) .tr("sous-echantillonne") else .tr("complet")) else .tr("Non calcule / ignore"))
      )
    })
  })
}

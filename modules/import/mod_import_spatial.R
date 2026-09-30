# =============================================================================
# modules/import/mod_import_spatial.R — Spatial Import (Visium / Xenium / CosMx)
# =============================================================================
# v9 (backlog court-terme #1 — voir handoff_spatial_bio-mg.md) : nouveau
#    panneau "Reference scRNA-seq partagee" (carte dediee, sous le resume de
#    l'objet spatial) : upload + preparation UNE FOIS, reutilisable par RCTD
#    ET Label Transfer (mod_spatial_deconv.R) sans re-upload/re-parse, tant
#    que ce module reste ouvert dans l'onglet Import (l'artefact prepare vit
#    dans global_data$spatial_reference, capture par save_session_btn). Le
#    module de deconvolution garde son propre uploader LOCAL inchange comme
#    repli/override (radio "Source de la reference", visible uniquement
#    quand une reference partagee existe).
#
# v10 (feedback biologiste — fix cache multi-echantillons, voir
#    handoff_spatial_bio-mg.md) : cet import ne pose plus DIRECTEMENT
#    global_data$spatial_obj -- seuls spatial_datasets[[nom]] et
#    active_spatial_dataset sont ecrits ici desormais. C'est
#    mod_spatial.R qui, en reagissant a active_spatial_dataset (observer
#    CENTRALISE, quel que soit qui a change cette valeur), se charge de
#    synchroniser spatial_obj -- ET, plus important, de snapshot les
#    resultats de l'echantillon SORTANT dans le cache par-echantillon avant
#    de basculer. Avant ce correctif, cet import ecrasait spatial_obj EN
#    COURT-CIRCUITANT ce mecanisme : importer un 2e echantillon perdait
#    irremediablement les resultats deja calcules sur le 1er (jamais
#    snapshotes) ET les faisait apparaitre, a tort, comme appartenant au
#    nouvel echantillon (shared_rv n'etait jamais reinitialise). Voir
#    mod_spatial.R pour le detail du nouvel observer centralise.
#
# v6 (Chantier 1 refonte — HD import robustness): the previous version
# derived "is this an HD folder / which bin sizes exist" from
# is_visium_hd_dir() + list_visium_hd_bin_sizes(), and separately whether
# the *flat* HD layout existed anywhere. Because of a since-fixed
# duplicate-definition bug in helpers_io.R, is_visium_hd_dir() had silently
# regressed to ONLY detecting the binned_outputs/ layout — the flat/
# feature-slice layout was invisible to this module even though the
# detection logic below LOOKED complete. This module now reads a SINGLE
# reactive, `visium_mode()`, driven by helpers_io.R::get_visium_import_mode()
# — the same single source of truth the loaders themselves use — so the UI
# and the actual import call can never disagree about which of the 3 modes
# ("visium" / "visium_hd_binned" / "visium_hd_flat") applies:
#   - "visium_hd_flat" now shows its OWN clear red banner (not "binning
#     introuvable", which was misleading for this genuinely-unsupported
#     format) and the Importer button, if clicked, fails immediately with
#     the same clear message (helpers_io.R refuses this mode itself before
#     ever attempting Load10X_Spatial() — defense in depth, not just a UI
#     warning that could be bypassed by a stale render).
#   - "visium_hd_binned" keeps the existing arrow-availability banner + bin
#     size selector, now also mentioning the lightweight 'nanoparquet'
#     alternative and this app's own degraded arrow-free loader (still
#     works without arrow, coordinates + counts only, see
#     helpers_io.R::load_spatial_visium_hd_manual()).
#
# v5 (Phase 5 — Visium HD): auto-detects a Visium HD dataset (presence of a
# `binned_outputs/` folder under the picked directory, see
# helpers_io.R::is_visium_hd_dir()) as soon as a folder is chosen with
# "Visium" selected, and — if detected — swaps in a bin-size selector
# (8um/16um only, see load_spatial_visium_hd()'s header for why 2um is
# intentionally NOT offered) and routes the import through
# load_spatial_visium_hd() instead of the classic load_spatial_visium().
# Deliberately NOT a new top-level "technology" radio choice: detection is
# automatic (one less decision for a non-expert biologist), and HD still
# produces the exact same spatial_obj CONTRACT (sketch + bpcells_dir +
# coords + histology) as classic Visium — QC/Clustering/Deconvolution/
# Visualisation/Multi-echantillons needed ZERO changes.
#
# v8 (Slide-seq boost — feedback reel dataset hippocampe souris, voir
#    handoff_spatial_bio-mg.md) : le loader Slide-seq lui-meme (formats,
#    detection, diagnostics barcodes) est dans helpers_io.R (v2 des fonctions
#    .find_slideseq_location_file()/.find_slideseq_counts()/load_spatial_slideseq()).
#    Ce fichier ne change que le panneau d'info (liste exhaustive des formats
#    reconnus + rappel de ce qui fonctionne deja "gratuitement" pour Slide-seq
#    sans code specifique : SCTransform, BANKSY-lite a lambda=0, Moran's I
#    avec x.cuts/y.cuts, RCTD/Label Transfer, multi-pucks).
#
# v7 (backlog #4, "carte blanche" session — voir handoff_spatial_bio-mg.md) :
#   1. "Importer aussi la matrice brute (raw)" — checkbox Visium classique
#      (non-HD) uniquement : charge EN PLUS raw_feature_bc_matrix.h5 (tous
#      les barcodes, y compris hors-tissu) dans SA PROPRE matrice BPCells
#      (helpers_io.R::load_spatial_visium_raw() ->
#      convert_to_bpcells_and_fov(raw_bg_obj=)). Purement additif : les
#      champs $raw_bpcells_dir/$n_raw_total ne sont lus par aucun module
#      existant aujourd'hui — reserve a une future correction de bruit
#      ambiant (DecontX ou equivalent).
#   2. Slide-seq (BETA) — nouveau choix de technologie, sans image
#      histologique (aucune pour cette technologie), voir
#      helpers_io.R::load_spatial_slideseq().
#
# Histology for HD: passes `raw_dir` = the ROOT "outs" folder (the SAME
# folder the user picked — unchanged from the classic path) rather than the
# bin-specific `binned_outputs/square_0XXum/` subfolder. This is
# deliberate, not an oversight: 10x's own Visium HD output makes
# `binned_outputs/square_0XXum/spatial/tissue_*_image.png` a SYMLINK back
# to the root `outs/spatial/` copy, and that symlink has been reported
# broken/unreadable in several real-world exports (see e.g.
# satijalab/seurat issue #9533/#9688) — Load10X_Spatial()'s own image
# loader inherits that fragility (helpers_io.R's loader now also
# auto-repairs this before calling Load10X_Spatial(), see
# repair_hd_symlinked_images()). Reading directly from the root
# `outs/spatial/` folder (this app's own disk-first histology path, see
# R/utils_spatial_io.R's v5/v6 history) sidesteps the symlink entirely: the
# root copy is the canonical, non-symlinked source, and its scale factors
# apply to the same underlying microscopy image regardless of which bin
# size was loaded.
#
# v4 (audit step 3.9c — histology rotation / background selection): added
# 3 manual orientation checkboxes (swap X/Y, mirror horizontal, mirror
# vertical) for Visium, wired to R/utils_spatial_io.R::apply_coord_orientation()
# via convert_to_bpcells_and_fov(). Also now passes `raw_dir = dir_path()`
# through so extract_histology_image() can actually find the spatial/
# folder — a standard VisiumV1 object does not reliably expose that path
# internally (root cause of "impossible de selectionner tous les fonds
# disponibles", see utils_spatial_io.R header for the full diagnosis). Both
# fixes apply ONCE at import time (not per-viz-session) so clustering/
# Moran/viz/multi-sample all read the same corrected $coords and the same
# full set of discovered histology backgrounds.
#
# v3 (Phase 4 — multi-echantillons): each successful import now ADDS to
# global_data$spatial_datasets (named list, key = sample name) instead of
# silently overwriting any previous import, and becomes the "active" dataset
# (global_data$active_spatial_dataset — see mod_spatial.R's centralized
# active-dataset observer, v10 above). Re-importing under an EXISTING sample
# name still replaces that one entry (with a warning), matching the previous
# re-import-overwrites behavior for a single dataset.
#
# v2 (vignette coverage — Phase 3): added the sketch normalization choice
# (LogNormalize / SCTransform, opt-in) — see R/utils_spatial_io.R::build_sketch()
# for where this is actually applied (bounded to the <= max_sketch cells,
# never the full disk-backed dataset).
#
# Loads raw spatial data then immediately hands off to
# R/utils_spatial_io.R::convert_to_bpcells_and_fov() so global_data$spatial_obj
# is ALWAYS the lightweight list contract (sketch + bpcells_dir + coords),
# never a raw in-RAM Seurat object — see utils_spatial_io.R header.
#
# Visium reuses the existing helpers_io.R::load_spatial_visium() (QC-filtered
# on load, same as before this module existed) — "reutiliser les modules
# existants" project rule. Xenium/CosMx use Seurat's own loaders.
#
# Import itself stays synchronous (withProgress spinner, like
# mod_import_sc.R) — only the heavy downstream analyses (clustering,
# deconvolution, Moran's I, multi-sample integration) go through mirai/
# ExtendedTask. SCTransform (when selected) also runs synchronously here, on
# the sketch only — see the warning shown in the UI when it's selected.
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ── UI ────────────────────────────────────────────────────────────────────
mod_import_spatial_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        width = 400, title = i18n$t("Import Spatial"),
        
        radioButtons(ns("technology"), i18n$t("Technologie"),
                     choices = stats::setNames(c("visium", "xenium", "cosmx", "slideseq"),
                                               c(.tr_plain("Visium (spots)"),
                                                 .tr_plain("Xenium (subcellulaire)"),
                                                 .tr_plain("CosMx (subcellulaire)"),
                                                 .tr_plain("Slide-seq (beads, beta)"))),
                     selected = "visium"),
        
        div(class = "alert alert-light", style = "font-size:0.8rem;",
            bsicons::bs_icon("lightbulb"),
            " ", i18n$t("Selectionnez le dossier racine contenant les fichiers bruts (ex: dossier 'outs' pour Visium/Xenium 10X, ou dossier CosMx AtoMx). Visium HD (dossier contenant 'binned_outputs/') est detecte automatiquement. Chaque import s'ajoute a la liste des echantillons (onglet Spatial > \"5. Multi-echantillons\") plutot que de remplacer le precedent.")),
        
        textInput(ns("sample_name"), i18n$t("Nom de l'echantillon"), placeholder = "Ex: Tumor_slice1"),
        
        # renderUI + global_data$language : shinyDirButton n'a pas d'update*
        # equivalent -> le bouton est reconstruit a chaque changement de langue.
        uiOutput(ns("dir_select_ui")),
        verbatimTextOutput(ns("path_display"), placeholder = TRUE),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'visium'", ns("technology")),
          hr(),

          uiOutput(ns("hd_mode_banner_ui")),

          numericInput(ns("min_counts"), i18n$t("nCount_Spatial minimum"), 100, min = 0, step = 10),
          numericInput(ns("min_features"), i18n$t("nFeature_Spatial minimum"), 200, min = 0, step = 10),

          checkboxInput(ns("load_raw_also"),
                        i18n$t("Importer aussi la matrice brute (raw, spots hors-tissu inclus)"),
                        value = FALSE),
          conditionalPanel(
            condition = sprintf("input['%s']", ns("load_raw_also")),
            div(class = "alert alert-light", style = "font-size:0.72rem;",
                bsicons::bs_icon("info-circle"),
                " ", i18n$t("Charge EN PLUS 'raw_feature_bc_matrix.h5' (tous les barcodes, y compris hors-tissu) dans une matrice BPCells separee — reservee a un futur usage (ex: correction de bruit ambiant). N'affecte AUCUN calcul actuel (QC/clustering/deconvolution continuent d'utiliser uniquement la matrice filtree). Non disponible pour Visium HD."))
          ),
          
          hr(),
          div(class = "alert alert-light", style = "font-size:0.75rem;",
              bsicons::bs_icon("compass"),
              " ", i18n$t("Si le fond histologique apparait tourne/inverse par rapport aux spots apres import (probleme connu, cause = convention de colonnes GetTissueCoordinates() qui a change entre versions de Seurat), corrigez ici puis re-importez — verifiez visuellement dans l'onglet \"4. Visualisation\" > Carte spatiale.")),
          checkboxInput(ns("orient_swap_xy"), i18n$t("Inverser X / Y (corrige une rotation de 90\u00b0)"), value = FALSE),
          checkboxInput(ns("orient_flip_x"), i18n$t("Miroir horizontal"), value = FALSE),
          checkboxInput(ns("orient_flip_y"), i18n$t("Miroir vertical"), value = FALSE)
        ),

        conditionalPanel(
          condition = sprintf("input['%s'] == 'slideseq'", ns("technology")),
          hr(),
          div(class = "alert alert-warning", style = "font-size:0.78rem;",
              bsicons::bs_icon("lightbulb"),
              " ", i18n$t("Slide-seq (BETA) : counts (MTX ou DGE) + coordonnees des beads (CSV/TSV), sans image histologique (technologie sans imagerie associee).")),
          tags$details(
            tags$summary(style = "cursor:pointer; font-size:0.72rem; color:#666;",
                         i18n$t("Formats de fichiers reconnus")),
            tags$ul(style = "font-size:0.72rem; padding-left:1.1rem;",
              tags$li(tags$b(i18n$t("Comptages")), i18n$t("— triplet 10x :"), tags$code("matrix.mtx[.gz]"),
                      " + ", tags$code("barcodes.tsv[.gz]"), " + ",
                      tags$code("features.tsv[.gz]"), "/", tags$code("genes.tsv[.gz]"),
                      ", ", i18n$t("OU une table dense :"), " ", tags$code("MappedDGEForR.csv"), ", ",
                      tags$code("dge_matrix.csv/tsv"), i18n$t("(ou tout fichier '*dge*'/'*expression*', .csv ou .tsv, .gz accepte).")),
              tags$li(tags$b(i18n$t("Localisation des beads")), " \u2014 ", tags$code("BeadLocationsForR.csv"),
                      ", ", tags$code("BeadLocation.csv"), ", ",
                      tags$code("*_alignedXYCoords.csv/.tsv"), i18n$t("(Slide-seq v2, souvent SANS en-tete — detecte automatiquement),"), tags$code("coords.csv/.tsv"),
                      ", ", tags$code("positions.csv/.tsv"), i18n$t("(.gz accepte pour tous).")),
              tags$li(i18n$t("Un recoupement de barcodes trop faible entre comptages et localisation"),
                      i18n$t("(ex: suffixe '-1' present d'un cote seulement) est corrige automatiquement"),
                      i18n$t("si possible, sinon signale avec des exemples de barcodes des deux cotes."))
            )
          ),
          div(class = "alert alert-light", style = "font-size:0.7rem;",
              bsicons::bs_icon("check2-circle"),
              " ", i18n$t("Deja disponibles apres import, sans code supplementaire (ces onglets sont generiques a toutes les technologies) : SCTransform (case ci-dessous) ; clustering spatial onglet 2 (Lambda = 0 \u2192 equivalent PCA/UMAP/clusters \"classique\", sans terme de voisinage) ; indice de Moran onglet 1 (options avancees x.cuts/y.cuts pour accelerer sur un gros puck) ; deconvolution RCTD et transfert d'annotations scRNA-seq onglet 3 ; multi-pucks onglet 5.")),
          numericInput(ns("min_counts_ss"), i18n$t("nCount minimum"), 100, min = 0, step = 10),
          numericInput(ns("min_features_ss"), i18n$t("nFeature minimum"), 200, min = 0, step = 10)
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'xenium' || input['%s'] == 'cosmx'",
                              ns("technology"), ns("technology")),
          hr(),
          sliderInput(ns("simplify_tol"), i18n$t("Tolerance de simplification des polygones"),
                      1, 100, 20, step = 1)
        ),
        
        hr(),
        numericInput(ns("max_sketch"), i18n$t("Taille max. du sketch (RAM)"),
                     50000, min = 5000, max = 100000, step = 5000),
        
        radioButtons(ns("norm_method"), i18n$t("Normalisation du sketch"),
                     choices = stats::setNames(c("lognorm", "sct"),
                                               c(.tr_plain("LogNormalize (rapide, defaut)"),
                                                 .tr_plain("SCTransform (vignette Seurat, plus lourd)"))),
                     selected = "lognorm"),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'sct'", ns("norm_method")),
          div(class = "alert alert-warning", style = "font-size:0.75rem;",
              bsicons::bs_icon("exclamation-triangle"),
              " ", i18n$t("SCTransform est significativement plus lourd que LogNormalize et s'execute de maniere SYNCHRONE pendant l'import (pas de mirai a cette etape) — reduisez la taille du sketch ci-dessus (ex: 10 000-20 000) si l'import devient trop long. N'affecte que le sketch (visualisation gene/UMAP) : le clustering spatial (BANKSY-lite) et l'indice de Moran restent en LogNormalize rapide sur les donnees completes, inchanges."))
        ),
        
        actionButton(ns("btn_import"), i18n$t("\U0001F680 Importer + convertir (BPCells)"),
                     class = "btn-success w-100 mt-2", icon = icon("play"))
      ),
      
      card(
        card_header(i18n$t("Resume de l'objet spatial charge")),
        layout_columns(
          value_box(title = i18n$t("Spots / Cellules (total disque)"), value = textOutput(ns("nb_total")),
                    showcase = bsicons::bs_icon("grid-3x3"), theme = "primary"),
          value_box(title = i18n$t("Sketch (RAM)"), value = textOutput(ns("nb_sketch")),
                    showcase = bsicons::bs_icon("cpu"), theme = "secondary"),
          value_box(title = i18n$t("Genes"), value = textOutput(ns("nb_genes")),
                    showcase = bsicons::bs_icon("diagram-3"), theme = "info"),
          value_box(title = i18n$t("Statut"), value = textOutput(ns("status_obj")),
                    showcase = bsicons::bs_icon("check-circle"), theme = "light")
        ),
        card_body(h5(i18n$t("Console de Log"), class = "text-muted"),
                  verbatimTextOutput(ns("console_log"), placeholder = TRUE))
      ),

      # v7 (backlog court-terme #1 — voir handoff_spatial_bio-mg.md) :
      # reference scRNA-seq PARTAGEE, preparee une fois ici et reutilisable
      # directement par RCTD ET Label Transfer (onglet Spatial > "3.
      # Deconvolution") sans re-upload/re-parse a chaque session ou a
      # chaque nouvel echantillon spatial importe. mod_spatial_deconv.R
      # garde son propre uploader LOCAL (session-only) inchange -- ceci est
      # une option SUPPLEMENTAIRE, proposee automatiquement des qu'une
      # reference partagee existe (voir son radio "Source de la reference").
      card(
        card_header(i18n$t("Reference scRNA-seq partagee (RCTD / Label Transfer)")),
        div(class = "alert alert-light", style = "font-size:0.78rem;",
            bsicons::bs_icon("info-circle"),
            " ", i18n$t("Chargez et preparez UNE FOIS une reference scRNA-seq annotee ici pour la reutiliser directement dans l'onglet \"Spatial > 3. Deconvolution\" (RCTD ET Label Transfer), sans avoir a la re-uploader/re-preparer a chaque echantillon ou a chaque session (tant que l'artefact reste sur ce disque — voir sauvegarde de session dans le panneau lateral).")),
        fileInput(ns("shared_ref_file"), i18n$t("Fichier reference (.rds Seurat, .h5ad, .h5, .loom)"),
                  accept = c(".rds", ".h5ad", ".h5", ".loom", ".rda", ".RData")),
        uiOutput(ns("shared_ref_status_badge_ui")),
        uiOutput(ns("shared_ref_celltype_col_ui")),
        uiOutput(ns("shared_ref_artifact_status_ui"))
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────────────────────
mod_import_spatial_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # Bouton dossier : reconstruit a chaque changement de langue (pas
    # d'update*Input pour shinyDirButton). En-session, .tr() renvoie deja
    # une chaine brute (pas de span), donc plus de balise visible.
    output$dir_select_ui <- renderUI({
      global_data$language
      shinyFiles::shinyDirButton(ns("dir_select"),
                                 .tr("\U0001F4C1 Choisir le dossier"),
                                 .tr("Selectionner le dossier de donnees spatiales"),
                                 class = "btn-secondary w-100", icon = icon("folder-open"))
    })

    # ── i18n: push translated labels/choices on EVERY language change ──────
    # Values NEVER change; only display names move (update*Input preserves
    # the current selection). fileInput has no update* function in Shiny.
    observeEvent(global_data$language, {
      updateRadioButtons(session, "technology",
        label = .tr("Technologie"),
        choices = stats::setNames(c("visium", "xenium", "cosmx", "slideseq"),
                                  c(.tr("Visium (spots)"), .tr("Xenium (subcellulaire)"),
                                    .tr("CosMx (subcellulaire)"), .tr("Slide-seq (beads, beta)"))))
      updateTextInput(session, "sample_name", label = .tr("Nom de l'echantillon"))
      updateNumericInput(session, "min_counts", label = .tr("nCount_Spatial minimum"))
      updateNumericInput(session, "min_features", label = .tr("nFeature_Spatial minimum"))
      updateCheckboxInput(session, "load_raw_also",
        label = .tr("Importer aussi la matrice brute (raw, spots hors-tissu inclus)"))
      updateCheckboxInput(session, "orient_swap_xy",
        label = .tr("Inverser X / Y (corrige une rotation de 90\u00b0)"))
      updateCheckboxInput(session, "orient_flip_x", label = .tr("Miroir horizontal"))
      updateCheckboxInput(session, "orient_flip_y", label = .tr("Miroir vertical"))
      updateNumericInput(session, "min_counts_ss", label = .tr("nCount minimum"))
      updateNumericInput(session, "min_features_ss", label = .tr("nFeature minimum"))
      updateSliderInput(session, "simplify_tol",
        label = .tr("Tolerance de simplification des polygones"))
      updateNumericInput(session, "max_sketch", label = .tr("Taille max. du sketch (RAM)"))
      updateRadioButtons(session, "norm_method",
        label = .tr("Normalisation du sketch"),
        choices = stats::setNames(c("lognorm", "sct"),
                                  c(.tr("LogNormalize (rapide, defaut)"),
                                    .tr("SCTransform (vignette Seurat, plus lourd)"))))
      updateActionButton(session, "btn_import",
        label = .tr("\U0001F680 Importer + convertir (BPCells)"))
    }, ignoreInit = TRUE)

    logs <- reactiveVal("En attente d'import...")
    add_log <- function(msg) logs(paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg, "\n", logs()))
    
    volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())
    shinyFiles::shinyDirChoose(input, "dir_select", roots = volumes, session = session)
    
    dir_path <- reactiveVal(NULL)
    observeEvent(input$dir_select, {
      path <- shinyFiles::parseDirPath(volumes, input$dir_select)
      if (length(path) > 0) { dir_path(path); add_log(paste(.tr("Dossier:"), path)) }
    })
    output$path_display <- renderText({
      global_data$language  # i18n: re-render on language switch
      if (is.null(dir_path())) .tr("Aucun dossier selectionne") else dir_path()
    })

    # ── DRIVE LIVE CONTROL (Phase F) : the bound confirm button ───────────────
    # `btn_import` is a REAL `actionButton`, so — unlike the seven dispatched
    # actions — it needs no counter of its own and no declared parameter set: the
    # folder and the technology are already in the module. `ts_drive_bind_button()`
    # instruments it so a session a HUMAN populated through the native picker
    # becomes drivable by an agent afterwards, which is the half of the flow that
    # needs no `import_file` at all.
    #
    # With no `arm.json` the counter never moves: the binding is inert, exactly
    # like `import_bulk`'s.
    #
    # READINESS GUARD, same reasoning as `import_bulk`'s: the observer below opens
    # with `req(dir_path())` and would abort in SILENCE with no folder, so the
    # poller would report `done` for an import that never happened. The guard
    # tests the module's own PICKED PATH, never a path string from a scenario, and
    # it stays cheap — it reads a `reactiveVal`, it does not stat a 10X tree.
    drive_btn <- ts_drive_bind_button(TS_DRIVE_SPATIAL_IMPORT_BUTTON)
    drive_counter <- shiny::reactiveVal(0L)
    drive_ready <- function() {
      if (is.null(shiny::isolate(dir_path()))) {
        "no data folder selected (the native picker is empty)"
      } else TRUE
    }
    # ── STATE PROBE (gap 1) ───────────────────────────────────────────────
    # Same gap, same fix, same bound as `mod_import_bulk.R`: without this the
    # module published NO `snapshot.modules` entry, so an `import_file` that
    # reported `done` was indistinguishable from one that loaded nothing. The
    # `technology` field is the importer-specific one — it is the value that
    # SELECTS the loader (`switch()` with no fallthrough), so an agent cannot
    # infer which loader ran from the dimensions alone.
    #
    # Every reactive read is `isolate()`d: the probe runs inside the poller's
    # beat, and an un-isolated read would enrol this module's data in the
    # poller's dependency set (the rule `ts_drive_module_states()` states).
    #
    # The dimensions are read from `$sketch` and NOT from a `$counts` field: the
    # Spatial object carries `sketch` / `coords` / `bpcells_dir` / `n_total` and no
    # counts matrix (:805-807 reads the same three), so a probe written against
    # `obj$counts` would publish NULL forever and look like a working probe that
    # reports nothing. `n_total` is the on-disk spot count and `n_samples` the
    # spots in the sketch: two DIFFERENT numbers by design.
    drive_state <- function() {
      obj  <- tryCatch(shiny::isolate(global_data$spatial_obj), error = function(e) NULL)
      sk   <- tryCatch(obj$sketch, error = function(e) NULL)
      list(module = "import_spatial", has_data = !is.null(obj),
           n_genes = if (is.null(sk)) NULL else suppressWarnings(as.integer(nrow(sk))),
           n_samples = if (is.null(sk)) NULL else suppressWarnings(as.integer(ncol(sk))),
           # ⚠️ `as.integer(NULL)` is `integer(0)`, which serialises as `[]` — a
           # SECOND shape for one field, the exact trap `ts_drive_module_states()`
           # documents for `columns` (drive_watcher.R:1174-1180).
           n_total = if (is.null(obj) || is.null(obj$n_total)) NULL else
             suppressWarnings(as.integer(obj$n_total)),
           technology = tryCatch(shiny::isolate(input$technology),
                                 error = function(e) NULL))
    }
    # The button id is a LITERAL here on purpose: test-drive-watcher.R greps every
    # TS_DRIVE_BUTTONS entry as a quoted literal beside a `ts_drive_publish_token(`
    # call, and that shared check is what proves an allowlist entry is really
    # wired. The assertions keep the literal and the constant from drifting.
    ts_drive_publish_token(global_data, "import_spatial-btn_import",
                           drive_counter, ready = drive_ready, state = drive_state)
    # A `reactive()` is the trigger type that respects BOTH a reactiveVal counter
    # and an actionButton counter, so the human's click and the drive's click share
    # ONE observer. Same idiom as `mod_import_bulk.R:326`, and the reason is not
    # stylistic: two observers on the same button would mean two bodies to keep in
    # step, and `check_duplication.R` counts repeated `observeEvent` triggers in one
    # file as a copy-paste risk.
    drive_trigger <- shiny::reactive(list(drive_counter(), input$btn_import))

    # ── Visium import mode: SINGLE source of truth (Chantier 1 refonte) ───
    # get_visium_import_mode() (helpers_io.R) is the exact same function the
    # loaders themselves call before deciding what to do — the UI can never
    # show a banner for a mode different from the one that will actually be
    # loaded. Re-evaluated any time the picked folder OR technology changes.
    visium_mode <- reactive({
      req(dir_path(), identical(input$technology, "visium"))
      get_visium_import_mode(dir_path())
    })

    hd_bins <- reactive({
      req(identical(visium_mode(), "visium_hd_binned"))
      list_visium_hd_bin_sizes(dir_path())
    })

    output$hd_mode_banner_ui <- renderUI({
      global_data$language  # i18n: re-render on language switch
      mode <- tryCatch(visium_mode(), error = function(e) NA_character_)
      if (is.na(mode) || identical(mode, "visium")) return(NULL)

      if (identical(mode, "visium_hd_flat")) {
        return(div(
          class = "alert alert-danger", style = "font-size:0.78rem;",
          bsicons::bs_icon("exclamation-octagon"),
          " ", .tr("Format non pris en charge : ce dossier contient un .h5 \"feature slice\"/imagerie Visium HD (ex: '..._spatial.h5'), pas une matrice de comptage par spot/bin — l'import echouera si vous cliquez sur \"Importer\". Reexportez au format Space Ranger standard ('outs/binned_outputs/square_0XXum/' pour du HD, ou 'filtered_feature_bc_matrix.h5' + 'spatial/' pour du Visium classique).")
        ))
      }

      # mode == "visium_hd_binned"
      bins <- hd_bins()
      has_arrow <- requireNamespace("arrow", quietly = TRUE)
      has_nanoparquet <- requireNamespace("nanoparquet", quietly = TRUE)

      tagList(
        div(class = "alert alert-info", style = "font-size:0.78rem;",
            bsicons::bs_icon("grid-3x3-gap"),
            " ", .tr("Visium HD detecte ('binned_outputs/'). Choisissez UNE resolution a importer — reimportez ce meme dossier pour ajouter l'autre resolution comme echantillon distinct (onglet Spatial > \"5. Multi-echantillons\").")),

        if (length(bins) == 0) {
          div(class = "alert alert-warning", style = "font-size:0.75rem;",
              bsicons::bs_icon("exclamation-triangle"),
              " ", .tr("Dossier 'binned_outputs/' detecte mais aucun binning 8um/16um exploitable n'y a ete trouve (fichier .h5 manquant sous square_008um/ ou square_016um/)."))
        },

        if (!has_arrow && !has_nanoparquet) {
          div(class = "alert alert-danger", style = "font-size:0.75rem;",
              bsicons::bs_icon("exclamation-octagon"),
              " ", .tr("Ni 'arrow' ni 'nanoparquet' installes — necessaires pour lire les positions de bins (tissue_positions.parquet). L'import utilisera un chargeur degrade (coordonnees + comptages uniquement, PAS de fond histologique) si un fichier .csv de positions est aussi present ; sinon il echouera avec un message clair. Pour un import complet, installez l'un des deux : "),
              tags$code("install.packages('nanoparquet')"),
              " ", .tr("(leger, recommande sous Windows) ou "),
              tags$code("install.packages('arrow', repos = c('https://apache.r-universe.dev', getOption('repos')))"),
              ".")
        } else if (!has_arrow && has_nanoparquet) {
          div(class = "alert alert-warning", style = "font-size:0.72rem;",
              bsicons::bs_icon("info-circle"),
              " ", .tr("Package 'arrow' absent mais 'nanoparquet' present : import complet toujours possible, via un lecteur Parquet plus leger."))
        },

        if (length(bins) > 0) {
          selectInput(ns("hd_bin_size"), .tr("Resolution de binning"),
                      choices = stats::setNames(bins, paste0(bins, " \u00b5m")),
                      selected = if (16 %in% bins) 16 else bins[1])
        }
      )
    })
    
    # ── THE ONE IMPORT WRITER (Phase F) ────────────────────────────────────
    # Extracted verbatim from what used to be the body of the confirm-button
    # observer, with exactly THREE substitutions: `dir_path()` -> `path`,
    # `input$technology` -> `technology`, and every remaining widget read wrapped in
    # `shiny::isolate()`.
    #
    # ⚠️ The comment above deliberately does NOT quote the trigger expression
    # verbatim. `check_duplication.R` counts `observeEvent(<same trigger>, ...)`
    # occurrences per file, and it matches inside COMMENTS — so a comment naming
    # the old trigger made the gate report a duplicate observer that does not
    # exist. A warning that describes the code inaccurately costs the next reader
    # more than the sentence was worth.
    #
    # WHY (spec G3 / S5, the same rule `import_bulk` follows): the folder this
    # module imports is the ONE dataset input Shiny offers no way to set from the
    # server — `shinyDirButton` has no `update*` equivalent, and the only widget
    # that could carry it is a `webkitdirectory` input behind a native dialog an
    # automated client cannot answer. Faking the widget is forbidden, and
    # duplicating 180 lines of loader + BPCells conversion so a second caller can
    # reach it is how the two copies drift. So the body became a FUNCTION both
    # callers use, and the drive's `import_file` reaches it by name.
    #
    # `shiny::isolate()` is load-bearing in BOTH directions and is NOT a no-op in
    # the reactive case: it is what makes the same expression legal from a
    # non-reactive context (the importer) without changing the reactive one. The
    # observer only ever re-ran on the confirm click, and it still
    # does — the body no longer contributes dependencies to the observer, so the
    # handler cannot be dragged into re-running by a widget it merely reads.
    #
    # @return list(ok, status, error, name, technology, n_total, n_sketch).
    #   A verdict rather than an exception, because the two callers translate it
    #   differently: the UI shows a notification and moves on, while the drive
    #   must tell `invalid` (a refusal) from `error` (a loader failure).
    run_spatial_import <- function(path, sample_name, technology) {
      # ⚠️ THE NEVER-RAISE CONTRACT. Everything below — including this guard —
      # returns a verdict instead of raising, and the path guard sits INSIDE the
      # `tryCatch` precisely so that it does too. The first version raised it, which
      # made the writer the ONE caller that could throw, and the drive importer had
      # to wrap it in a second `tryCatch` purely to convert an exception into the
      # `error` verdict its sibling already produced. Two error channels for one
      # operation is exactly the ambiguity the verdict contract exists to remove.
      min_counts   <- shiny::isolate(input$min_counts)   %||% 100
      min_features <- shiny::isolate(input$min_features) %||% 200
      max_sketch   <- shiny::isolate(input$max_sketch)   %||% 50000
      norm_method  <- shiny::isolate(input$norm_method)  %||% "lognorm"
      simplify_tol <- shiny::isolate(input$simplify_tol) %||% 20
      load_raw_also <- isTRUE(shiny::isolate(input$load_raw_also))
      orient_swap_xy <- isTRUE(shiny::isolate(input$orient_swap_xy))
      orient_flip_x  <- isTRUE(shiny::isolate(input$orient_flip_x))
      orient_flip_y  <- isTRUE(shiny::isolate(input$orient_flip_y))
      hd_bin_size    <- shiny::isolate(input$hd_bin_size)
      min_counts_ss  <- shiny::isolate(input$min_counts_ss)   %||% 100
      min_features_ss <- shiny::isolate(input$min_features_ss) %||% 200

      mode <- if (identical(technology, "visium")) {
        tryCatch(get_visium_import_mode(path), error = function(e) "visium")
      } else {
        NA_character_
      }
      is_hd <- identical(mode, "visium_hd_binned")

      withProgress(message = .tr("Import spatial..."), value = 0, {
        tryCatch({
          # The one guard that belongs to the WRITER rather than to a caller: an
          # empty path can only come from a caller that skipped its own `req()`.
          if (is.null(path) || !nzchar(path)) {
            stop(.tr("Aucun dossier selectionne"), call. = FALSE)
          }
          # Fail fast, before touching any progress UI, for the one mode we
          # know in advance can never succeed -- same message the loader
          # itself would raise, but skips a pointless withProgress cycle.
          if (identical(mode, "visium_hd_flat")) {
            stop(.hd_flat_unsupported_message(), call. = FALSE)
          }
          if (is_hd && (is.null(hd_bin_size) || !nzchar(hd_bin_size))) {
            stop(.tr("Aucune resolution de binning selectionnee (voir le panneau Visium HD ci-dessus)."),
                 call. = FALSE)
          }

          incProgress(0.1, detail = .tr("Lecture des fichiers bruts..."))
          raw_obj <- switch(technology,
                            "visium" = {
                              if (is_hd) {
                                add_log(sprintf("  Visium HD detecte -- import du binning %sum.", hd_bin_size))
                                load_spatial_visium_hd(path, bin_size = as.integer(hd_bin_size),
                                                       sample_name = sample_name,
                                                       min_counts = min_counts,
                                                       min_features = min_features)
                              } else {
                                load_spatial_visium(path, sample_name = sample_name,
                                                    min_counts = min_counts,
                                                    min_features = min_features)
                              }
                            },
                            "xenium" = Seurat::LoadXenium(path, fov = "fov"),
                            "cosmx"  = Seurat::LoadNanostring(path, fov = "fov", assay = "Nanostring"),
                            "slideseq" = load_spatial_slideseq(path, sample_name = sample_name,
                                                               min_counts = min_counts_ss,
                                                               min_features = min_features_ss),
                            stop(errorCondition(.tr("Technologie inconnue."), class = "spatial_import_error"))
          )
          if (isTRUE(attr(raw_obj, "ts_manual_hd_loader"))) {
            add_log(paste0(
              "  ⚠ Chargeur HD degrade utilise (pas de package Parquet complet) : ",
              "coordonnees et comptages disponibles, PAS de fond histologique natif ",
              "pour cet echantillon (le fond depuis 'spatial/' racine reste tente separement)."
            ))
          }
          add_log(sprintf("  ✓ Objet brut charge : %d genes x %d %s%s",
                          nrow(raw_obj), ncol(raw_obj),
                          if (technology == "visium") "spots" else "cellules",
                          if (is_hd) sprintf(" (bin %sum)", hd_bin_size) else ""))

          # ── v7 (backlog #4) : matrice brute (raw) OPTIONNELLE, Visium
          # classique (non-HD) uniquement — chargee EN PLUS, jamais a la
          # place, de la matrice filtree ci-dessus. Best-effort : un echec
          # ici ne bloque jamais l'import filtre.
          raw_bg_obj <- NULL
          if (identical(technology, "visium") && !is_hd && load_raw_also) {
            incProgress(0.05, detail = .tr("Lecture de la matrice brute (raw)..."))
            raw_bg_obj <- tryCatch(load_spatial_visium_raw(path), error = function(e) {
              add_log(paste("  \u26a0 Lecture matrice brute (raw) echouee :", conditionMessage(e)))
              NULL
            })
            if (!is.null(raw_bg_obj)) {
              add_log(sprintf("  \u2713 Matrice brute (raw) lue : %d barcodes (avant filtrage tissu).",
                              ncol(raw_bg_obj)))
            } else {
              add_log("  \u26a0 Aucune matrice brute (raw_feature_bc_matrix.h5) trouvee ou lecture impossible.")
            }
          }
          
          incProgress(0.3, detail = .tr("Conversion BPCells (disque)..."))
          norm_label <- if (norm_method == "sct") "SCTransform" else "LogNormalize"
          add_log(sprintf("  Normalisation du sketch : %s", norm_label))
          if (norm_method == "sct") {
            incProgress(0.1, detail = .tr("SCTransform sur le sketch (synchrone, peut prendre du temps)..."))
          }
          
          # FIX (audit step 3.9c): pass the ACTUAL folder the user picked
          # (raw_dir) so extract_histology_image() can find spatial/ itself
          # -- a standard VisiumV1 object does not reliably carry this path
          # internally, see utils_spatial_io.R header for the full
          # diagnosis. Also thread the manual orientation correction
          # through (Visium only -- these inputs don't exist in the DOM for
          # xenium/cosmx, so the reads below are FALSE there, and isTRUE(NULL)
          # safely defaults to FALSE). For Visium HD, raw_dir is STILL the
          # root "outs" folder (not the bin-specific subfolder) — see this
          # file's header for why (symlinked per-bin histology images are a
          # documented Visium HD fragility point, the root copy is canonical).
          orient_applied <- orient_swap_xy || orient_flip_x || orient_flip_y
          if (orient_applied) {
            add_log(sprintf("  Correction manuelle d'orientation : swap_xy=%s, flip_x=%s, flip_y=%s",
                            orient_swap_xy, orient_flip_x, orient_flip_y))
          }
          
          spatial_pkg <- convert_to_bpcells_and_fov(
            raw_obj, dataset_id = sample_name, technology = technology,
            simplify_tol = simplify_tol,
            max_sketch = max_sketch,
            norm_method = norm_method,
            raw_dir = path,
            swap_xy = orient_swap_xy,
            flip_x  = orient_flip_x,
            flip_y  = orient_flip_y,
            raw_bg_obj = raw_bg_obj
          )
          add_log(sprintf("  ✓ BPCells: %s", spatial_pkg$bpcells_dir))
          if (!is.null(spatial_pkg$raw_bpcells_dir)) {
            add_log(sprintf("  \u2713 BPCells (brut/raw) : %s (%d barcodes)",
                            spatial_pkg$raw_bpcells_dir, spatial_pkg$n_raw_total %||% NA))
          }
          add_log(sprintf("  ✓ Sketch RAM: %d/%d elements (normalisation: %s)",
                          ncol(spatial_pkg$sketch), spatial_pkg$n_total, norm_label))
          if (!is.null(spatial_pkg$histology)) {
            n_fonds <- length(spatial_pkg$histology$images %||% list())
            add_log(sprintf("  \u2713 %d fond(s) histologique(s) detecte(s) (voir selecteur de resolution, onglet Visualisation).",
                            n_fonds))
          } else if (is_hd) {
            add_log(paste0(
              "  \u26a0 Aucun fond histologique detecte pour cet echantillon HD (verifiez que le ",
              "dossier racine contient bien 'spatial/')."
            ))
          }
          
          incProgress(0.9, detail = .tr("Finalisation..."))
          
          # Phase 4 (multi-echantillons) : AJOUTE au conteneur plutot que
          # d'ecraser silencieusement un import precedent d'un AUTRE
          # echantillon. Re-importer sous un nom DEJA utilise remplace
          # toujours cette entree specifique (comportement inchange pour un
          # seul echantillon), avec un avertissement explicite.
          was_already_active <- identical(sample_name, global_data$active_spatial_dataset)
          if (sample_name %in% names(global_data$spatial_datasets)) {
            add_log(sprintf("  \u26a0 Un echantillon nomme '%s' existait deja — remplace.", sample_name))
            showNotification(sprintf("\u26a0\ufe0f Echantillon '%s' deja existant — remplace.", sample_name),
                             type = "warning", duration = 6)
          }
          # (c) Purge le cache par-echantillon pour CE nom -- un import frais
          # rend tout resultat precedemment cache obsolete, actif ou non.
          if (!is.null(global_data$spatial_results_cache[[sample_name]])) {
            cache <- global_data$spatial_results_cache
            cache[[sample_name]] <- NULL
            global_data$spatial_results_cache <- cache
          }
          global_data$spatial_datasets[[sample_name]] <- spatial_pkg
          # v10 (fix cache multi-echantillons) : NE PLUS ecrire
          # global_data$spatial_obj directement ici -- seul
          # active_spatial_dataset est mis a jour ; c'est l'observer
          # CENTRALISE de mod_spatial.R (qui reagit a active_spatial_dataset,
          # peu importe qui l'a change) qui synchronise spatial_obj ET
          # snapshot/restaure shared_rv au passage. Voir le header de ce
          # fichier (v10) pour le bug que ce court-circuit provoquait.
          global_data$active_spatial_dataset <- sample_name
          # (c) Reassigner a la MEME valeur ne re-declenche pas l'observer
          # de mod_spatial.R (Shiny n'invalide pas sur reassignation
          # identique) -- signal explicite separe pour ce cas precis.
          if (was_already_active) {
            global_data$spatial_reimport_signal <- list(name = sample_name, at = Sys.time())
          }
          
          add_log(.t_fmt(.tr("✅ Import termine : {name} ({tech}{hd}) — {n} echantillon(s) au total"),
                          name = sample_name, tech = technology,
                          hd = if (is_hd) sprintf(" HD %sum", hd_bin_size) else "",
                          n = length(global_data$spatial_datasets)))
          showNotification(.t_fmt(.tr("✅ Import spatial reussi : {n} elements ({sk} en sketch RAM, {norm}){extra}"),
                                  n = spatial_pkg$n_total, sk = ncol(spatial_pkg$sketch), norm = norm_label,
                                  extra = if (length(global_data$spatial_datasets) > 1) {
                                    sprintf(.tr(" — %d echantillons charges, voir 'Multi-echantillons'"),
                                            length(global_data$spatial_datasets))
                                  } else ""),
                           type = "message", duration = 6)
          # The verdict the drive importer returns. Deliberately SCALARS only: it
          # is published through `result.json`, and the module's own `n_results`
          # projection reads the same shape. `n_total` counts disk elements and
          # `n_sketch` the RAM sketch, which are the two numbers the module's own
          # value boxes show — so an agent reads the same figures a human does.
          list(ok = TRUE, status = "applied", error = NULL,
               name = sample_name, technology = technology,
               n_total = as.integer(spatial_pkg$n_total),
               n_sketch = as.integer(ncol(spatial_pkg$sketch)),
               n_datasets = length(global_data$spatial_datasets))
        }, error = function(e) {
          msg <- paste(.tr("❌ Erreur import spatial:"), conditionMessage(e))
          add_log(msg); showNotification(msg, type = "error", duration = 10)
          # `error`, not `invalid`: the payload was ACCEPTED (the watcher already
          # validated the folder against the allowlisted roots) and the LOADER is
          # what failed. The two must stay distinguishable, exactly as they are for
          # the Bulk importer.
          list(ok = FALSE, status = "error", error = conditionMessage(e),
               name = sample_name, technology = technology,
               n_total = NA_integer_, n_sketch = NA_integer_,
               n_datasets = length(global_data$spatial_datasets))
        })
      })
    }

    # ── The UI's own click path, now a two-line adapter over the shared writer ──
    # The folder and the name come from the widgets; the writer takes them as
    # ARGUMENTS precisely so the drive path never has to fake them.
    #
    # ONE observer, TWO triggers (`drive_trigger`). Declared HERE, after the
    # writer, not beside the binding: an observer body naming a function defined
    # further down would be correct by accident, and only until someone reordered
    # the two blocks. `req(dir_path())` is the human's guard — with no folder
    # picked, a drive click aborts in silence rather than dispatching an import
    # that can only fail.
    observeEvent(drive_trigger(), {
      req(dir_path())
      run_spatial_import(
        dir_path(),
        if (nchar(trimws(input$sample_name))) trimws(input$sample_name) else basename(dir_path()),
        input$technology
      )
    })

    # ── DRIVE LIVE CONTROL (Phase F) : the Spatial importer for `import_file` ───
    # Same contract as modules/import/mod_import_bulk.R:620, and the same rule —
    # reuse the loader the UI uses, never fake a widget. `dir_path` arrived as
    # DATA, already confined to the allowlisted roots and already `dir.exists()`
    # by `ts_drive_validate_import_dir()` (spec S11), and `technology` /
    # `sample_name` arrived as validated scalars, so nothing here has to be
    # re-checked or guessed.
    #
    # The parameters the widgets own (min_counts, max_sketch, norm_method, …) are
    # read by `run_spatial_import()` through `isolate()`, so the drive honours the
    # widget state a human may have set, and an agent that never opened the panel
    # gets the declared defaults. They are DEFAULTS, not derived values: no
    # session-derived parameter enters this function.
    #
    # No `tryCatch` around the writer: it RETURNS its verdict (see the never-raise
    # contract on `run_spatial_import`) and this is a straight translation of it. An
    # exception here would have to be mapped onto the same `error` status anyway, so
    # the wrapper bought nothing but a second place for the two channels to
    # disagree.
    ts_drive_publish_importer(global_data, TS_DRIVE_SPATIAL_IMPORT_MODULE, function(request) {
      res <- run_spatial_import(request$dir_path, request$sample_name, request$technology)
      if (!isTRUE(res$ok)) {
        # `error`, not `invalid`: the payload was ACCEPTED (the watcher already
        # validated the folder against the allowlisted roots) and the LOADER is
        # what failed. The two must stay distinguishable, exactly as they are for
        # the Bulk importer — an `invalid` here would tell the agent to send a
        # different payload, and no different payload would help.
        return(list(ok = FALSE, status = res$status %||% "error",
                    errors = res$error %||% "the Spatial import failed",
                    warnings = character(0)))
      }
      list(ok = TRUE, status = "applied", errors = character(0), warnings = character(0))
    })
    
    # ── Outputs ────────────────────────────────────────────────────────
    output$nb_total  <- renderText({ if (is.null(global_data$spatial_obj)) "-" else format(global_data$spatial_obj$n_total, big.mark = ",") })
    output$nb_sketch  <- renderText({ if (is.null(global_data$spatial_obj)) "-" else format(ncol(global_data$spatial_obj$sketch), big.mark = ",") })
    output$nb_genes   <- renderText({ if (is.null(global_data$spatial_obj)) "-" else format(nrow(global_data$spatial_obj$sketch), big.mark = ",") })
    output$status_obj <- renderText({
      global_data$language  # i18n: re-render on language switch
      if (is.null(global_data$spatial_obj)) .tr("⚪ Inactif")
      else {
        # DefaultAssay of the sketch tells us which normalization actually
        # ended up being used (SCTransform can silently fall back to
        # LogNormalize on failure — see build_sketch()) — reflect the truth,
        # not just what was requested.
        norm_used  <- tryCatch(Seurat::DefaultAssay(global_data$spatial_obj$sketch), error = function(e) NA)
        norm_label <- if (identical(norm_used, "SCT")) "SCT" else "LogNorm"
        paste0("🟢 ", global_data$spatial_obj$technology, " (", norm_label, ")")
      }
    })
    output$console_log <- renderText({
      global_data$language  # i18n: re-render on language switch
      txt <- logs()
      if (identical(txt, "En attente d'import...")) .tr("En attente d'import...") else txt
    })

    # ── Reference scRNA-seq PARTAGEE (backlog court-terme #1) ──────────────
    # Reuses the SAME underlying functions as mod_spatial_deconv.R's local
    # uploader (R/utils_spatial_reference.R -- read_reference_scrna(),
    # prepare_reference_seurat(), prepare_reference_artifact()), but writes
    # the prepared artifact into global_data$spatial_reference instead of a
    # module-local reactiveVal, so it survives across dataset imports (and,
    # via app.R's save/load session, across sessions) and can be offered as
    # a ready-to-use option by mod_spatial_deconv.R (its "Source de la
    # reference" radio) without any re-parsing.
    shared_ref_state         <- reactiveValues(obj = NULL, status = "empty", message = NULL)
    shared_ref_meta_cols     <- reactiveVal(character(0))

    observeEvent(input$shared_ref_file, {
      req(input$shared_ref_file)
      orig_ext <- tolower(tools::file_ext(input$shared_ref_file$name))
      shared_ref_state$obj     <- NULL
      shared_ref_state$status  <- "loading"
      shared_ref_state$message <- NULL
      shared_ref_meta_cols(character(0))

      withProgress(message = .tr("Lecture de la reference partagee..."), value = 0, {
        incProgress(0.15, detail = sprintf("Lecture (.%s)...", if (nzchar(orig_ext)) orig_ext else "?"))
        staged_path <- tryCatch({
          if (nzchar(orig_ext)) {
            p <- tempfile(fileext = paste0(".", orig_ext))
            file.copy(input$shared_ref_file$datapath, p, overwrite = TRUE)
            p
          } else input$shared_ref_file$datapath
        }, error = function(e) NULL)
        if (is.null(staged_path)) {
          shared_ref_state$status  <- "error"
          shared_ref_state$message <- "Copie du fichier uploade impossible."
          return(invisible(FALSE))
        }

        raw_ref <- tryCatch(read_reference_scrna(staged_path), error = function(e) {
          shared_ref_state$status  <- "error"
          shared_ref_state$message <- paste("Lecture :", conditionMessage(e))
          showNotification(paste(.tr("Reference partagee — erreur de lecture :"), conditionMessage(e)),
                           type = "error", duration = 12)
          NULL
        })
        if (is.null(raw_ref)) return(invisible(FALSE))

        incProgress(0.5, detail = "Conversion en objet Seurat...")
        ref_obj <- tryCatch(prepare_reference_seurat(raw_ref, project_name = "SharedReference"),
                            error = function(e) {
          shared_ref_state$status  <- "error"
          shared_ref_state$message <- paste("Conversion :", conditionMessage(e))
          showNotification(paste(.tr("Reference partagee — erreur de conversion :"), conditionMessage(e)),
                           type = "error", duration = 12)
          NULL
        })
        if (is.null(ref_obj) || !inherits(ref_obj, "Seurat")) return(invisible(FALSE))

        shared_ref_state$obj     <- ref_obj
        shared_ref_state$status  <- "loaded"
        shared_ref_state$message <- NULL
        shared_ref_meta_cols(colnames(ref_obj@meta.data))
        incProgress(0.35, detail = .tr("Termine."))
        showNotification(.t_fmt(.tr("Reference partagee lue : {cells} cellules x {genes} genes."),
                                cells = ncol(ref_obj), genes = nrow(ref_obj)),
                         type = "message", duration = 5)
      })
    })

    output$shared_ref_status_badge_ui <- renderUI({
      global_data$language  # i18n: re-render on language switch
      st <- shared_ref_state$status
      switch(st,
        "empty" = div(class = "alert alert-light", style = "font-size:0.75rem;",
                     bsicons::bs_icon("circle"), " ", .tr("Aucune reference partagee chargee.")),
        "loading" = div(class = "alert alert-info", style = "font-size:0.75rem;",
                        bsicons::bs_icon("hourglass-split"), " ", .tr("Chargement en cours...")),
        "error" = div(class = "alert alert-danger", style = "font-size:0.75rem;",
                      bsicons::bs_icon("x-circle"), " ",
                      .t_fmt(.tr("Erreur : {msg}"),
                             msg = shared_ref_state$message %||% .tr("cause inconnue"))),
        "loaded" = div(class = "alert alert-success", style = "font-size:0.75rem;",
                       bsicons::bs_icon("check-circle"), " ",
                       .t_fmt(.tr("Reference lue : {cells} cellules x {genes} genes — choisissez la colonne 'type cellulaire' puis preparez-la ci-dessous."),
                              cells = ncol(shared_ref_state$obj), genes = nrow(shared_ref_state$obj))),
        NULL
      )
    })

    output$shared_ref_celltype_col_ui <- renderUI({
      global_data$language  # i18n: re-render on language switch
      req(shared_ref_state$obj, length(shared_ref_meta_cols()) > 0)
      ref_obj <- shared_ref_state$obj
      cols <- shared_ref_meta_cols()
      n_levels <- vapply(cols, function(cn) length(unique(ref_obj@meta.data[[cn]])), integer(1))
      useful <- cols[n_levels >= 2 & n_levels <= 200]
      useful <- useful[order(-n_levels[useful])]
      choices <- if (length(useful) > 0) useful else cols
      tagList(
        selectInput(ns("shared_ref_celltype_col"), .tr("Colonne 'type cellulaire'"), choices = choices),
        checkboxInput(ns("shared_merge_rare_types"),
                      .tr("Fusionner/exclure les types rares (< 25 cellules)"), value = TRUE),
        checkboxInput(ns("shared_cap_ref_cells"),
                      .tr("Limiter le nombre de cellules par type (RAM/vitesse)"), value = TRUE),
        conditionalPanel(
          condition = sprintf("input['%s']", ns("shared_cap_ref_cells")),
          numericInput(ns("shared_max_cells_per_type"), .tr("Max cellules par type"), 500, min = 25, max = 5000, step = 25)
        ),
        actionButton(ns("btn_prepare_shared_ref"), .tr("Preparer + partager cette reference"),
                     icon = icon("share-fill"), class = "btn-sm btn-outline-success w-100 mt-2")
      )
    })

    observeEvent(input$btn_prepare_shared_ref, {
      req(shared_ref_state$obj, input$shared_ref_celltype_col)
      withProgress(message = .tr("Preparation de l'artefact partage..."), value = 0.1, {
        incProgress(0.2, detail = .tr("Filtrage / fusion des types rares..."))
        result <- tryCatch({
          prepare_reference_artifact(
            ref_obj            = shared_ref_state$obj,
            celltype_col       = input$shared_ref_celltype_col,
            merge_rare_types   = isTRUE(input$shared_merge_rare_types),
            min_cells_per_type = 25L,
            max_cells_per_type = if (isTRUE(input$shared_cap_ref_cells)) input$shared_max_cells_per_type else NA_integer_
          )
        }, error = function(e) {
          showNotification(paste(.tr("Erreur preparation reference partagee :"), conditionMessage(e)),
                           type = "error", duration = 10)
          NULL
        })
        if (!is.null(result)) {
          incProgress(0.7, detail = .tr("Termine."))
          global_data$spatial_reference <- list(
            path = result$path, n_cells = result$n_cells, n_genes = result$n_genes,
            backend = result$backend, celltype_col = input$shared_ref_celltype_col,
            source_label = input$shared_ref_file$name %||% "reference",
            n_dropped_rare = result$n_dropped_rare, created_at = Sys.time()
          )
          showNotification(.t_fmt(.tr("Reference partagee prete : {cells} cellules x {genes} genes — disponible dans Spatial > 3. Deconvolution (RCTD/Label Transfer)."),
                                   cells = result$n_cells, genes = result$n_genes
          ), type = "message", duration = 8)
        }
      })
    })

    output$shared_ref_artifact_status_ui <- renderUI({
      global_data$language  # i18n: re-render on language switch
      info <- global_data$spatial_reference
      if (is.null(info)) {
        return(div(class = "text-muted", style = "font-size:0.7rem;",
                   .tr("Pas encore de reference partagee active.")))
      }
      div(class = "alert alert-success", style = "font-size:0.72rem;",
          bsicons::bs_icon("share-fill"),
          " ", .t_fmt(.tr("Reference partagee ACTIVE : {cells} cellules x {genes} genes (colonne '{col}') — "),
                      cells = format(info$n_cells, big.mark = ","), genes = format(info$n_genes, big.mark = ","),
                      col = info$celltype_col),
          .tr("utilisable directement dans Spatial > 3. Deconvolution."),
          actionLink(ns("btn_clear_shared_ref"), paste(" ", .tr("Retirer")),
                     style = "font-size:0.7rem; margin-left:6px;"))
    })

    observeEvent(input$btn_clear_shared_ref, {
      global_data$spatial_reference <- NULL
      showNotification(.tr("Reference partagee retiree."), type = "message", duration = 4)
    })
  })
}

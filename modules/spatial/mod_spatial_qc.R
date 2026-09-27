# =============================================================================
# modules/spatial/mod_spatial_qc.R — QC & Spatial Autocorrelation (Moran's I)
# =============================================================================
# v5 (vague 5 — Phase 6 stats) : nouveau sous-onglet "Hotspots locaux
#    (Getis-Ord Gi*)" (B4), SYNCHRONE (pas de mirai) -- appelle directement
#    R/utils_spatial_stats.R::compute_getis_ord_hotspots(), qui est
#    volontairement bon marche (pas de permutation, O(n*k)), meme convention
#    que compute_qc_metrics_fast() ci-dessous. Deux sources de metrique :
#    une colonne QC deja en RAM (shared_rv$qc_metrics) ou une proportion de
#    deconvolution deja en RAM (shared_rv$deconv_props) -- toutes deux sans
#    reouverture de la matrice BPCells. L'expression d'un gene n'est PAS
#    proposee ici (necessiterait de rouvrir BPCells/normaliser -- casserait
#    la conception synchrone documentee dans utils_spatial_stats.R) ; a
#    faire plus tard si besoin reel (onglet 4 reste la reference pour
#    l'expression genique).
#
# v4 (UX feedback): new "Apercu du jeu de donnees" tab, first in the strip —
# absorbs the dataset banner that used to sit above ALL tabs in mod_spatial.R
# (project/technology/counts/sketch size), now shown where it's contextually
# relevant instead of permanently pinned, plus a metadata table (sketch
# meta.data) that didn't exist anywhere before. Also added: %ribo histogram
# (was computed by compute_qc_metrics_fast() already but never plotted) and
# an nCount-vs-nFeature scatter colored by %MT (the classic QC diagnostic,
# complements the 4 histograms with the actual joint relationship).
#
# v3 (vignette coverage — Phase 2): added the "Top SVGs" small-multiples grid
# (facet_wrap of the top-N Moran's-I genes over the spatial sketch) — purely
# a visualization of results already computed by moran_task below, so it
# runs synchronously off global_data$spatial_obj$sketch (<=50k cells, no
# BPCells/disk access, no mirai needed — consistent with this project's
# convention of reserving async for genuinely heavy compute only).
#
# v2 (moyen terme — export/auto-pipeline, voir handoff_spatial_bio-mg.md) :
#    shared_rv$qc_params / shared_rv$moran_params ecrits au moment du clic
#    sur leurs boutons respectifs (miroir des parametres UI utilises) --
#    purement additif, lu uniquement par mod_spatial_export.R (script R
#    reproductible) ; zero changement de comportement pour cet onglet.
#
# Cost profiles, per spec:
#   1. QC metrics (nCount/nFeature/%MT/%ribo) — cheap, streamed straight off
#      the on-disk BPCells matrix (R/utils_spatial_io.R::compute_qc_metrics_fast()),
#      runs synchronously on the main thread.
#   2. Moran's I spatial autocorrelation on the top ~1000 HVGs — genuinely
#      heavy, so it goes through ExtendedTask + mirai, isolated in a daemon
#      that reopens the BPCells matrix from disk (never receives the Seurat
#      object itself).
#   3. Hotspots (Getis-Ord Gi*, B4) — cheap, closed-form, synchronous (see
#      v5 above).
#
# Reuses Seurat's own FindSpatiallyVariableFeatures(selection.method="moransi")
# / SVFInfo() (verified against SeuratObject/Seurat source — see comments
# inline) rather than reimplementing Moran's I.
# =============================================================================

# =============================================================================
# MCP drive action `spatial-qc-btn_hotspots` (Phase E).
#
# Same module-layer wrapper pattern as the five other actions. The statistic
# itself lives in R/spatial/spatial_stats.R and is NOT touched.
#
# WHY THIS BUTTON AND NOT ANOTHER: it is the only Spatial action that is a plain
# `actionButton`, SYNCHRONOUS, and free of the two hard prerequisites the rest of
# this tab has - no on-disk BPCells reopen and no mirai daemon. The clustering and
# marker buttons need `global_data$spatial_obj$bpcells_dir` and inline their whole
# algorithm in the worker body; Moran's I is an `input_task_button`. This one reads
# two in-RAM tables, so a driven run is reproducible and cheap.
#
# THE ONE VALUE THAT CANNOT BE FROZEN IS THE METRIC, resolved by RULE
# (.spatial_hotspot_metric) exactly as `bulk-pattern-run_pattern` resolves its
# group column. The human path lets the user pick a QC column or a deconvolution
# proportion; a remote caller cannot see either widget, so the rule declares a
# preference order over the QC metrics and refuses with `not_ready` when none is
# usable. The deconvolution branch is deliberately NOT reachable from the drive
# action: it needs a prior deconvolution, which is not on the drive surface, and
# silently substituting a QC metric for a requested proportion would be a lie.
#
# `n_results` COUNTS SIGNIFICANT SPOTS, not rows of the result table. The table has
# one row per spatial element, so `nrow()` would report ~1000 for a run that found
# two hotspots - the same "a number that does not mean what it says" failure the
# `empty` distinction exists to prevent. A run with no significant spot is `empty`,
# and that outcome is legitimate rather than an error.
# =============================================================================

.SPATIAL_QC_DRIVE_MODULE <- "spatial_qc"
.SPATIAL_QC_DRIVE_BUTTON <- "spatial-qc-btn_hotspots"

#' Closed step vocabulary for this action, same as the other actions.
.SPATIAL_QC_DRIVE_STEP_STATES <- c("skipped", "running", "ran", "ignored", "error")

#' The QC metrics this action may use, in preference order.
#'
#' `nCount` first because it is what the human `selectInput` picks by default (it is
#' the first choice, and no `selected` is given), so a drive run and a human click
#' agree on the common case. `log_nCount` follows as the monotone alternative. The
#' rule never picks a metric that is not present.
.spatial_hotspot_metric_order <- function() {
  c("nCount", "log_nCount", "nFeature", "pct_mt", "pct_ribo")
}

#' Frozen input set for the drive action.
#'
#' `hotspot_k` is the widget default (30 neighbours, self included) and the frozen
#' `hotspot_source = "qc"` is the DECLARED branch; the deconvolution branch is out of
#' reach by design. `hotspot_alpha` is stated rather than left implicit: the domain
#' hard-codes 1.96 / 0.05 in its own classification
#' (R/spatial/spatial_stats.R), so the value is recorded for the report and MUST
#' stay 0.05 — a frozen input that disagreed with the domain would be a lie in the
#' provenance block.
.spatial_qc_drive_inputs <- function() {
  list(
    hotspot_source = "qc",
    hotspot_k      = 30L,
    hotspot_alpha  = 0.05
  )
}

#' Availability of the one optional package this action needs.
#'
#' `compute_getis_ord_hotspots()` guards `RANN` itself
#' (R/spatial/spatial_stats.R), so this pre-check does not remove a guard, it only
#' moves the failure from "halfway through a run" to "before a job is declared" so
#' the refusal can carry `state = missing_dependency`.
.spatial_hotspot_dependency_check <- function() {
  if (isTRUE(requireNamespace("RANN", quietly = TRUE))) {
    return(list(ok = TRUE, missing = character(0)))
  }
  list(ok = FALSE, missing = "RANN")
}

#' Resolve the metric by RULE, never by guessing.
#'
#' A metric is "usable" when it is a NUMERIC column of `shared_rv$qc_metrics` with
#' at least one finite value. The returned field is named, never bare, for the same
#' reason as `.bulk_pattern_group_column()`: a reason is a length-1 character too,
#' and an earlier version there let the caller report `ready` while holding a
#' sentence.
#'
#' @return `list(metric = <one column name>)` or `list(reason = <short reason>)`.
.spatial_hotspot_metric <- function(shared_rv) {
  qc <- tryCatch(shiny::isolate(shared_rv$qc_metrics), error = function(e) NULL)
  if (!is.data.frame(qc) || !nrow(qc)) {
    return(list(reason = "the QC step has not produced a metric table (shared_rv$qc_metrics is NULL)"))
  }
  order <- .spatial_hotspot_metric_order()
  for (m in order) {
    if (!m %in% names(qc)) next
    v <- qc[[m]]
    if (is.numeric(v) && any(is.finite(v))) return(list(metric = m))
  }
  return(list(reason = sprintf(
    "no numeric QC metric among the %d declared (%s)", length(order),
    paste(order, collapse = ", "))))
}

#' THE single writer for the hotspot slots, shared by THREE callers: the human
#' observer, the drive action, and the Spatial pipeline's stage 8.
#'
#' S1.5 renamed this from a dot-prefixed private name. A leading dot claims a
#' privacy that stopped being true when a second module started writing the same
#' two slots, and the name is load-bearing in the other direction too: a private
#' helper is one a second module would be tempted to inline rather than call, which
#' is exactly the duplication this function exists to prevent.
#'
#' It is deliberately still a MODULE-LEVEL function in `modules/`, not a pure
#' function in `R/`. `R/` is the pure domain layer and must not touch a Shiny
#' reactiveValues (rule C2); this function's whole job is to write a store, so
#' `modules/` is where it belongs. Keeping it here is also why S1.5 needed no
#' extraction: it was already reachable from any module and from an offline test.
#'
#' `hotspot_params` and `hotspot_result` are read together by the status panel, the
#' map, the histogram, the table and the CSV export, so they move together or not
#' at all, and PARAMS ARE WRITTEN FIRST. `metric` is validated to be a scalar
#' BEFORE anything is written, because an empty params block is what a half-written
#' pair looks like. The `metric` VALUE is the caller's business and is stored as
#' given: the QC rule resolves `nCount` and the pipeline declares `log_nCount`, and
#' this function must not become the place where that difference is quietly erased.
spatial_hotspot_store <- function(res, params, shared_rv) {
  if (!is.null(res) && !is.data.frame(res)) {
    stop(errorCondition(
      "spatial hotspots: the result to store must be the Getis-Ord table or NULL.",
      class = "spatial_stats_error", state = "invalid_input"))
  }
  if (!is.null(params)) {
    if (!is.list(params) || is.null(params$metric) || length(params$metric) != 1L ||
        is.na(params$metric)) {
      stop(errorCondition(
        "spatial hotspots: the parameters to store must name exactly one metric.",
        class = "spatial_stats_error", state = "invalid_input"))
    }
    shared_rv$hotspot_params <- params
  }
  shared_rv$hotspot_result <- res
  invisible(res)
}

#' S2 — the ONE serialiser for the hotspot table. Shared by the human
#' `downloadHandler` and the drive export route so the two files are
#' byte-identical; two serialisations would drift invisibly.
spatial_hotspot_csv_write <- function(df, file) {
  utils::write.csv(df, file, row.names = FALSE)
  invisible(file)
}

#' Thin wrapper: the same call as the human `eventReactive`, on the frozen `k` and
#' the rule-resolved metric.
#'
#' @return list(ok, n_results, record, params) where `n_results` is the number of
#'   SIGNIFICANT spots (hot or cold). `n_elements` carries the table length, so a
#'   caller that wants the other number does not have to guess.
run_spatial_hotspots <- function(coords, qc_metrics,
                                 inputs = .spatial_qc_drive_inputs()) {
  if (!is.list(inputs)) inputs <- .spatial_qc_drive_inputs()
  fail <- function(state, message) {
    stop(errorCondition(message, class = "spatial_stats_error", state = state))
  }
  check <- .spatial_hotspot_dependency_check()
  if (!isTRUE(check$ok)) {
    fail("missing_dependency", sprintf(
      "spatial hotspots: the local package '%s' is required; nothing is downloaded.",
      paste(check$missing, collapse = ", ")))
  }
  if (!is.data.frame(coords) || !all(c("id", "x", "y") %in% names(coords)) || !nrow(coords)) {
    fail("invalid_input", "spatial hotspots: the loaded dataset carries no spatial coordinates.")
  }
  m <- .spatial_hotspot_metric(list(qc_metrics = qc_metrics))
  if (!is.null(m$reason)) fail("not_ready", as.character(m$reason))
  metric <- m$metric
  if (!"id" %in% names(qc_metrics)) {
    fail("invalid_input", "spatial hotspots: the QC metric table has no 'id' column.")
  }
  values <- stats::setNames(qc_metrics[[metric]], as.character(qc_metrics$id))
  params <- list(source = as.character(inputs$hotspot_source)[1L],
                 metric = metric,
                 k_neighbors = as.integer(inputs$hotspot_k))
  res <- compute_getis_ord_hotspots(coords = coords, values = values,
                                    k_neighbors = as.integer(inputs$hotspot_k))
  if (!is.data.frame(res) || !nrow(res)) {
    fail("invalid_input", "spatial hotspots: the domain returned no scored element.")
  }
  n_sig <- sum(res$hotspot != "NS", na.rm = TRUE)
  list(ok = TRUE, n_results = as.integer(n_sig),
       n_elements = as.integer(nrow(res)), record = res, params = params)
}

#' Readiness: a loaded dataset with coordinates, and a usable QC metric.
#' Returning the REASON is what lets the poller publish `not_ready` instead of
#' dispatching a job that must fail.
.spatial_qc_drive_ready <- function(shared_rv, global_data) {
  coords <- tryCatch(shiny::isolate(global_data$spatial_obj$coords), error = function(e) NULL)
  if (!is.data.frame(coords) || !all(c("id", "x", "y") %in% names(coords)) || !nrow(coords)) {
    return("no spatial dataset loaded (global_data$spatial_obj$coords is NULL)")
  }
  m <- .spatial_hotspot_metric(shared_rv)
  if (!is.null(m$reason)) return(as.character(m$reason))
  TRUE
}

.spatial_qc_drive_view <- function(status, elapsed_s = 0, seq = 0L,
                                   n_results = 0L, has_data = FALSE,
                                   ready = FALSE, step = NULL) {
  step <- if (is.null(step)) "skipped" else as.character(step)
  if (length(step) != 1L || is.na(step) || !step %in% .SPATIAL_QC_DRIVE_STEP_STATES) {
    step <- "error"
  }
  list(
    module = .SPATIAL_QC_DRIVE_MODULE,
    action = "run_pipeline",
    status = as.character(status),
    elapsed_s = as.numeric(elapsed_s),
    seq = as.integer(seq),
    n_results = as.integer(n_results),
    has_data = isTRUE(has_data),
    ready = isTRUE(ready),
    steps = list(hotspots = step)
  )
}

#' State probe. `n_results` counts the significant spots of the last successful run,
#' RECOMPUTED from the stored table rather than remembered, so a stale count cannot
#' survive a re-import: `shiny::isolate()` keeps these reads out of the poller's
#' dependency set.
.spatial_qc_drive_state <- function(shared_rv, global_data, run_state,
                                    last_step = NULL) {
  job <- tryCatch(ts_drive_job_state(), error = function(e) NULL)
  pending <- tryCatch(ts_drive_job_pending(), error = function(e) NULL)
  ready <- isTRUE(.spatial_qc_drive_ready(shared_rv, global_data))
  obj <- tryCatch(shiny::isolate(global_data$spatial_obj), error = function(e) NULL)
  res <- tryCatch(shiny::isolate(shared_rv$hotspot_result), error = function(e) NULL)
  n_results <- if (is.data.frame(res) && nrow(res) && "hotspot" %in% names(res)) {
    as.integer(sum(res$hotspot != "NS", na.rm = TRUE))
  } else 0L
  current <- if (is.function(run_state)) {
    shiny::isolate(run_state())
  } else {
    as.character(run_state)
  }
  if (length(current) != 1L || is.na(current)) current <- "idle"
  .spatial_qc_drive_view(
    status = if (ready) current else "not_ready",
    elapsed_s = if (is.null(pending)) 0 else as.numeric(pending$elapsed_s),
    seq = if (is.null(job)) 0L else as.integer(job$seq),
    n_results = n_results,
    has_data = !is.null(obj),
    ready = ready,
    step = last_step
  )
}

#' Runs the wrapper, publishes through the shared writer, and reports the terminal
#' state. A failure publishes NO result count, so a stale hotspot map can never be
#' read next to an error.
.spatial_qc_run_drive <- function(global_data, shared_rv, close_job) {
  coords <- tryCatch(shiny::isolate(global_data$spatial_obj$coords), error = function(e) NULL)
  qc <- tryCatch(shiny::isolate(shared_rv$qc_metrics), error = function(e) NULL)
  res <- tryCatch(
    run_spatial_hotspots(coords, qc, .spatial_qc_drive_inputs()),
    error = function(e) e
  )
  if (inherits(res, "condition")) {
    close_job("error", conditionMessage(res))
    return(list(status = "error", n_results = 0L, step = "error"))
  }
  spatial_hotspot_store(res$record, res$params, shared_rv)
  n_results <- as.integer(res$n_results %||% 0L)
  if (length(n_results) != 1L || is.na(n_results) || n_results < 0L) n_results <- 0L
  # The JOB vocabulary is narrower than the VIEW vocabulary: `done` / `error` /
  # `invalid` / `timeout` / `session_lost` only. A metric with no significant spot
  # is a job that COMPLETED, so it closes as `done` while the view reports `empty`.
  close_job("done", NULL)
  list(status = if (n_results == 0L) "empty" else "done",
       n_results = n_results, step = "ran")
}

mod_spatial_qc_ui <- function(id) {  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = i18n$t("QC & filtres"), width = 350,

      div(class = "alert alert-light", style = "font-size:0.8rem;",
          bsicons::bs_icon("info-circle"),
          " ",
          i18n$t("Les seuils ci-dessous ne modifient pas les donnees sur disque : ils definissent quels spots/cellules sont inclus dans le clustering et la deconvolution. Ajustables a tout moment.")),

      numericInput(ns("min_features"), i18n$t("nFeature minimum"), 200, min = 0, step = 10),
      numericInput(ns("min_count"), i18n$t("nCount minimum"), 100, min = 0, step = 10),
      sliderInput(ns("max_pct_mt"), i18n$t("% Mitochondrial max"), 0, 100, 20, step = 1),

      actionButton(ns("btn_apply_qc"), i18n$t("Appliquer les seuils"),
                   class = "btn-danger w-100 mt-2", icon = icon("filter")),
      uiOutput(ns("qc_pass_summary")),

      hr(),
      h6(i18n$t("Autocorrelation spatiale (Indice de Moran)"), style = "font-weight:bold;"),
      div(class = "alert alert-light", style = "font-size:0.8rem;",
          bsicons::bs_icon("cpu"),
          " ",
          i18n$t("Calcul asynchrone (mirai) sur les 1000 genes les plus variables — n'interrompt pas votre session.")),
      numericInput(ns("n_hvg_moran"), i18n$t("Nombre de genes (HVG)"), 1000, min = 100, max = 5000, step = 100),

      # Long terme (carte blanche, voir handoff_spatial_bio-mg.md) : methode
      # alternative a l'indice de Moran. Mark variogram est l'autre methode
      # native de Seurat::FindSpatiallyVariableFeatures() (approche
      # Trendsceek-like) -- x.cuts/y.cuts (grille) ne s'appliquent qu'a
      # "moransi" cote Seurat, ignores automatiquement sinon.
      radioButtons(ns("svg_method"), i18n$t("Methode de detection"),
                   choices = stats::setNames(c("moransi", "markvariogram"),
                                             c(.tr_plain("Indice de Moran (rapide, recommande)"),
                                               .tr_plain("Mark variogram (alternative, plus lent)"))),
                   selected = "moransi"),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'markvariogram'", ns("svg_method")),
        div(class = "alert alert-warning", style = "font-size:0.72rem;",
            bsicons::bs_icon("exclamation-triangle"),
            " ",
            i18n$t("Methode alternative -- le nom des colonnes internes de Seurat differe de 'moransi' et n'est pas garanti stable entre versions ; le score affiche est detecte de facon defensive (generique) plutot que suppose. En cas de doute, preferez l'indice de Moran (par defaut)."))
      ),

      tags$details(
        tags$summary(style = "cursor:pointer; font-size:0.75rem; color:#666;",
                     i18n$t("Options avancees (gros jeux de donnees : Visium HD, Slide-seq)")),
        div(class = "mt-2",
            div(class = "text-muted", style = "font-size:0.7rem;",
                i18n$t("Regroupe les elements sur une grille avant le calcul de Moran's I — accelere fortement le calcul sur un puck Slide-seq (dizaines de milliers de beads) ou du Visium HD, au prix d'une resolution spatiale legerement reduite. Laissez a 0 pour le comportement standard (calcul point-par-point, adapte a Visium classique).")),
            numericInput(ns("moran_x_cuts"), i18n$t("x.cuts (0 = desactive)"), 0, min = 0, max = 500, step = 10),
            numericInput(ns("moran_y_cuts"), i18n$t("y.cuts (0 = desactive)"), 0, min = 0, max = 500, step = 10)
        )
      ),

      bslib::input_task_button(ns("btn_moran"), i18n$t("Lancer l'autocorrelation spatiale"),
                                icon = icon("wave-square")),
      verbatimTextOutput(ns("moran_progress_text"), placeholder = TRUE),

      hr(),
      h6(i18n$t("Hotspots locaux (Getis-Ord Gi*)"), style = "font-weight:bold;"),
      div(class = "alert alert-light", style = "font-size:0.78rem;",
          bsicons::bs_icon("fire"),
          " ",
          i18n$t("Detecte les regions ou une metrique est significativement plus ELEVEE (hotspot, rouge) ou plus BASSE (coldspot, bleu) que la moyenne globale, en tenant compte du voisinage spatial. Calcul rapide (pas de permutation), synchrone -- pas de barre de progression necessaire.")),
      radioButtons(ns("hotspot_source"), i18n$t("Metrique"),
                   choices = stats::setNames(c("qc", "deconv"),
                                             c(.tr_plain("Metrique QC"),
                                               .tr_plain("Proportion (deconvolution, onglet 3)"))),
                   selected = "qc"),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'qc'", ns("hotspot_source")),
        selectInput(ns("hotspot_qc_metric"), NULL,
                    choices = c("nCount", "nFeature", "pct_mt", "pct_ribo", "log_nCount"))
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'deconv'", ns("hotspot_source")),
        uiOutput(ns("hotspot_deconv_celltype_ui"))
      ),
      numericInput(ns("hotspot_k"), i18n$t("Voisins spatiaux (k, self inclus)"), 30, min = 5, max = 200, step = 5),
      actionButton(ns("btn_hotspots"), i18n$t("Detecter les hotspots"),
                   class = "btn-outline-primary w-100", icon = icon("fire")),
      uiOutput(ns("hotspot_status_ui"))
    ),

    # S1. `id` + `selected` are NEW, and they are what made the hotspots panel
    # reachable at all. Declared without an `id`, bslib builds the tab list with
    # an auto-generated key that nothing — not a human click, not
    # `ts_drive_perform_nav()` — can address, so the panel 4-of-4 was shown to
    # nobody and its four outputs stayed suspended under Shiny's default
    # `suspendWhenHidden = TRUE`. `selected` is pinned to the first panel so the
    # default view is unchanged and the addition is purely additive.
    # The id is the module's own, and it is declared as DATA in
    # R/core/drive_allowlist.R (TS_DRIVE_SPATIAL_QC_SUB_TABS_ID) so the UI and
    # the drive's navigation plan cannot drift apart.
    navset_card_underline(
      id = ns("qc_results"), selected = "overview",
      nav_panel(i18n$t("Apercu du jeu de donnees"), value = "overview",
                uiOutput(ns("dataset_overview_ui")),
                hr(),
                h6(i18n$t("Metadata (sketch)"), style = "font-weight:bold;"),
                div(class = "text-muted small mb-2",
                    i18n$t("Colonnes disponibles dans les metadonnees de l'objet Seurat (orig.ident, annotations importees, etc.) pour les elements du sketch en RAM.")),
                DT::DTOutput(ns("metadata_table"))),

      nav_panel(i18n$t("Distributions QC"),
                card(full_screen = TRUE,
                     plotOutput(ns("qc_hist_plot"), height = "560px")),
                card(full_screen = TRUE,
                     card_header(i18n$t("nCount vs nFeature (couleur = %MT)")),
                     plotOutput(ns("qc_scatter_plot"), height = "480px"))),

      nav_panel(i18n$t("Genes spatialement variables (Moran's I)"), value = "moran",
                div(class = "alert alert-light", style = "font-size:0.78rem;",
                    i18n$t("Grille des genes les plus spatialement structures (rang Moran's I) — necessite l'autocorrelation ci-contre (calculee au moins une fois).")),
                fluidRow(
                  column(6, numericInput(ns("n_top_svg"), i18n$t("Nombre de genes (grille)"), 9, min = 4, max = 30, step = 1)),
                  column(6, actionButton(ns("btn_svg_grid"), i18n$t("Afficher la grille des top SVGs"),
                                          class = "btn-sm btn-outline-primary mt-4", icon = icon("table-cells")))
                ),
                card(full_screen = TRUE, uiOutput(ns("svg_grid_plot_ui"))),
                hr(),
                DT::DTOutput(ns("moran_table"))),

      nav_panel(i18n$t("Hotspots locaux (Getis-Ord Gi*)"), value = "hotspots",
                div(class = "alert alert-light small mb-2",
                    i18n$t("Rouge = hotspot (voisinage significativement eleve, p < 0.05) ; bleu = coldspot ; gris = non significatif.")),
                layout_columns(
                  col_widths = c(7, 5),
                  card(full_screen = TRUE, card_header(i18n$t("Carte des hotspots")),
                       plotOutput(ns("hotspot_map"), height = "520px")),
                  card(full_screen = TRUE, card_header(i18n$t("Distribution du Gi*")),
                       plotOutput(ns("hotspot_hist"), height = "520px"))
                ),
                DT::DTOutput(ns("hotspot_table")),
                # ── Export (Phase F) ───────────────────────────────────────
                # The ONE thing this panel was missing. `mod_spatial_viz.R`
                # exports its table (`dl_csv`) and `mod_spatial_export.R` exports
                # a session bundle, but the hotspot result — the artefact the
                # `spatial-qc-btn_hotspots` drive action produces — lived only in
                # `shared_rv$hotspot_result` and in the DOM, so an agent that ran
                # the action had no way to OBTAIN the numbers. Additive only: a
                # button and a handler, no change to the computation, the table or
                # its classification.
                div(class = "mt-2",
                    downloadButton(ns("dl_hotspot_csv"),
                                   label = .tr_plain("\U0001F4CE Exporter la table des hotspots (CSV)"),
                                   class = "btn-sm btn-outline-secondary")))
    )
  )
}

mod_spatial_qc_server <- function(id, global_data, shared_rv) {
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
      updateRadioButtons(session, "svg_method",
        label = .tr("Methode de detection"),
        choices = stats::setNames(c("moransi", "markvariogram"),
                                  c(.tr("Indice de Moran (rapide, recommande)"),
                                    .tr("Mark variogram (alternative, plus lent)"))))
      updateRadioButtons(session, "hotspot_source",
        label = .tr("Metrique"),
        choices = stats::setNames(c("qc", "deconv"),
                                  c(.tr("Metrique QC"),
                                    .tr("Proportion (deconvolution, onglet 3)"))))
    }, ignoreInit = TRUE)

    # ── Apercu du jeu de donnees (ex-bandeau vert, deplace ici) ───────────
    output$dataset_overview_ui <- renderUI({
      global_data$language  # re-render on language switch
      if (is.null(global_data$spatial_obj)) {
        return(div(class = "alert alert-danger",
                    bsicons::bs_icon("exclamation-triangle"),
                    " ",
                    .tr("Aucune donnee spatiale chargee. Allez dans l'onglet 'Import Donnees > Spatial'.")))
      }
      obj <- global_data$spatial_obj
      disk_ok <- !is.null(obj$bpcells_dir) && dir.exists(obj$bpcells_dir)
      norm_used  <- tryCatch(Seurat::DefaultAssay(obj$sketch), error = function(e) NA)
      norm_label <- if (identical(norm_used, "SCT")) "SCTransform" else "LogNormalize"

      tagList(
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          value_box(title = .tr("Echantillon"), value = obj$project %||% "-",
                     showcase = bsicons::bs_icon("bookmark"), theme = "primary"),
          value_box(title = .tr("Technologie"), value = obj$technology,
                     showcase = bsicons::bs_icon("diagram-3"), theme = "secondary"),
          value_box(title = .tr("Elements (total disque)"), value = format(obj$n_total, big.mark = ","),
                     showcase = bsicons::bs_icon("grid-3x3"), theme = "info"),
          value_box(title = .tr("Sketch (RAM)"),
                     value = sprintf("%s (%s)", format(ncol(obj$sketch), big.mark = ","), norm_label),
                     showcase = bsicons::bs_icon("cpu"), theme = "light")
        ),
        if (!disk_ok) {
          div(class = "alert alert-warning mt-2",
              bsicons::bs_icon("exclamation-triangle"),
              " ",
              .tr("Donnees BPCells introuvables sur disque — le sketch reste utilisable pour la visualisation, mais reimportez pour relancer clustering/deconvolution/Moran."))
        }
      )
    })

    output$metadata_table <- DT::renderDT({
      global_data$language  # re-render on language switch
      req(global_data$spatial_obj$sketch)
      meta <- global_data$spatial_obj$sketch@meta.data
      validate(need(ncol(meta) > 0, .tr("Aucune metadata disponible pour ce jeu de donnees.")))
      ts_datatable(meta, page_length = 15L, filename_base = "spatial_qc_metadata",
                   filter = "none", rownames = TRUE)
    })

    # ── Fast, synchronous QC metrics (recomputed whenever the object changes) ──
    observeEvent(global_data$spatial_obj, {
      req(global_data$spatial_obj$bpcells_dir)
      shared_rv$qc_metrics <- tryCatch(
        compute_qc_metrics_fast(global_data$spatial_obj$bpcells_dir),
        error = function(e) {
          showNotification(paste(.tr("Erreur calcul QC :"), conditionMessage(e)), type = "error")
          NULL
        }
      )
    }, ignoreInit = TRUE)

    output$qc_hist_plot <- renderPlot({
      global_data$language  # re-render on language switch
      req(shared_rv$qc_metrics)
      df <- shared_rv$qc_metrics
      p1 <- ggplot2::ggplot(df, ggplot2::aes(x = nCount)) +
        ggplot2::geom_histogram(bins = 50, fill = "#2C3E50") +
        ggplot2::geom_vline(xintercept = input$min_count, color = "red", linetype = "dashed") +
        ggplot2::labs(title = "nCount") + ts_theme("minimal")
      p2 <- ggplot2::ggplot(df, ggplot2::aes(x = nFeature)) +
        ggplot2::geom_histogram(bins = 50, fill = "#18BC9C") +
        ggplot2::geom_vline(xintercept = input$min_features, color = "red", linetype = "dashed") +
        ggplot2::labs(title = "nFeature") + ts_theme("minimal")
      p3 <- ggplot2::ggplot(df, ggplot2::aes(x = pct_mt)) +
        ggplot2::geom_histogram(bins = 50, fill = "#E74C3C") +
        ggplot2::geom_vline(xintercept = input$max_pct_mt, color = "red", linetype = "dashed") +
        ggplot2::labs(title = .tr("% Mitochondrial")) + ts_theme("minimal")
      p4 <- ggplot2::ggplot(df, ggplot2::aes(x = pct_ribo)) +
        ggplot2::geom_histogram(bins = 50, fill = "#8E44AD") +
        ggplot2::labs(title = .tr("% Ribosomal")) + ts_theme("minimal")
      p5 <- ggplot2::ggplot(df, ggplot2::aes(x = log_nCount)) +
        ggplot2::geom_histogram(bins = 50, fill = "#F39C12") +
        ggplot2::labs(title = "log10(nCount + 1)") + ts_theme("minimal")
      patchwork::wrap_plots(p1, p2, p3, p4, p5, ncol = 5)
    })

    output$qc_scatter_plot <- renderPlot({
      global_data$language  # re-render on language switch
      req(shared_rv$qc_metrics)
      df <- shared_rv$qc_metrics
      ggplot2::ggplot(df, ggplot2::aes(x = nCount, y = nFeature, color = pct_mt)) +
        ggplot2::geom_point(alpha = 0.6, size = 1.3) +
        ggplot2::geom_vline(xintercept = input$min_count, color = "red", linetype = "dashed") +
        ggplot2::geom_hline(yintercept = input$min_features, color = "red", linetype = "dashed") +
        ggplot2::scale_color_viridis_c(option = "inferno", direction = -1, na.value = "grey70") +
        ggplot2::labs(x = "nCount", y = "nFeature", color = .tr("% MT")) +
        ts_theme("minimal", 12)
    })

    observeEvent(input$btn_apply_qc, {
      req(shared_rv$qc_metrics)
      df <- shared_rv$qc_metrics
      pass <- with(df, nCount >= input$min_count & nFeature >= input$min_features &
                     (is.na(pct_mt) | pct_mt <= input$max_pct_mt))
      shared_rv$qc_pass_idx <- which(pass)
      shared_rv$qc_params <- list(min_count = input$min_count, min_features = input$min_features,
                                   max_pct_mt = input$max_pct_mt)
      showNotification(.t_fmt(.tr("Seuils appliques : {kept}/{total} elements conserves."),
                              kept = sum(pass), total = length(pass)), type = "message", duration = 4)
    })

    output$qc_pass_summary <- renderUI({
      global_data$language  # re-render on language switch
      req(shared_rv$qc_pass_idx, shared_rv$qc_metrics)
      div(class = "alert alert-success", style = "font-size:0.8rem;",
          .t_fmt(.tr("{kept} / {total} elements retenus."),
                 kept = length(shared_rv$qc_pass_idx), total = nrow(shared_rv$qc_metrics)))
    })

    # ── Async: Moran's I on top HVGs (ExtendedTask + mirai) ────────────────
    log_file <- spatial_log_path(session, "moran")
    tracker  <- create_reactive_tracker(session, log_file)

    moran_task <- ExtendedTask$new(function(bpcells_dir, pass_idx, coords, n_hvg,
                                            x_cuts, y_cuts, method, log_file) {
      mirai::mirai(
        {
          write_mirai_log(log_file, "Ouverture de la matrice BPCells...", 1, 5)
          mat <- BPCells::open_matrix_dir(bpcells_dir)
          if (!is.null(pass_idx)) mat <- mat[, pass_idx, drop = FALSE]

          write_mirai_log(log_file, "Normalisation + selection des HVG...", 2, 5)
          obj <- Seurat::CreateSeuratObject(counts = mat)
          obj <- Seurat::NormalizeData(obj, verbose = FALSE)
          obj <- Seurat::FindVariableFeatures(obj, nfeatures = n_hvg, verbose = FALSE)
          hvgs <- Seurat::VariableFeatures(obj)

          write_mirai_log(log_file, "Alignement des coordonnees spatiales...", 3, 5)
          coords_df <- coords[match(colnames(obj), coords$id), c("x", "y")]
          rownames(coords_df) <- colnames(obj)
          keep <- stats::complete.cases(coords_df)
          coords_df <- coords_df[keep, , drop = FALSE]
          obj <- obj[, rownames(coords_df)]

          use_cuts <- identical(method, "moransi") &&
            is.finite(x_cuts) && is.finite(y_cuts) && x_cuts > 0 && y_cuts > 0
          write_mirai_log(log_file, sprintf(
            "Calcul de l'autocorrelation spatiale (%s) sur %d genes%s...",
            if (identical(method, "markvariogram")) "mark variogram" else "indice de Moran",
            length(hvgs), if (use_cuts) sprintf(" (grille %dx%d)", x_cuts, y_cuts) else ""
          ), 4, 5)
          svf_args <- list(
            object = obj[["RNA"]], layer = "data", features = hvgs,
            spatial.location = coords_df, selection.method = method,
            nfeatures = length(hvgs), verbose = FALSE
          )
          if (use_cuts) { svf_args$x.cuts <- x_cuts; svf_args$y.cuts <- y_cuts }
          assay_res <- do.call(Seurat::FindSpatiallyVariableFeatures, svf_args)

          ranked <- tryCatch(Seurat::SpatiallyVariableFeatures(assay_res, selection.method = method),
                             error = function(e) character(0))
          info <- SeuratObject::SVFInfo(assay_res, method = method)

          if (identical(method, "moransi")) {
            obs_col <- grep("observed$", colnames(info), value = TRUE)[1]
            pv_col  <- grep("p\\.value$|pvalue$", colnames(info), value = TRUE)[1]
          } else {
            numeric_cols <- colnames(info)[vapply(info, is.numeric, logical(1))]
            obs_col <- if (length(numeric_cols) > 0) numeric_cols[1] else NA_character_
            pv_col  <- grep("p\\.value$|pvalue$", colnames(info), value = TRUE)[1]
          }

          write_mirai_log(log_file, "Termine.", 5, 5)
          out <- data.frame(
            gene     = rownames(info),
            moran_i  = if (!is.na(obs_col)) info[[obs_col]] else NA_real_,
            p_value  = if (!is.na(pv_col))  info[[pv_col]]  else NA_real_,
            row.names = NULL, stringsAsFactors = FALSE
          )
          if (length(ranked) > 0) {
            ord <- match(ranked, out$gene)
            ord <- ord[!is.na(ord)]
            out <- out[c(ord, setdiff(seq_len(nrow(out)), ord)), ]
          }
          out
        },
        bpcells_dir = bpcells_dir, pass_idx = pass_idx, coords = coords,
        n_hvg = n_hvg, x_cuts = x_cuts, y_cuts = y_cuts, method = method,
        log_file = log_file, .timeout = MIRAI_TASK_TIMEOUT_MS
      )
    })
    bslib::bind_task_button(moran_task, "btn_moran")

    observeEvent(input$btn_moran, {
      req(global_data$spatial_obj$bpcells_dir, global_data$spatial_obj$coords)
      reset_log(log_file)
      shared_rv$moran_params <- list(n_hvg = input$n_hvg_moran,
                                     x_cuts = input$moran_x_cuts %||% 0,
                                     y_cuts = input$moran_y_cuts %||% 0,
                                     method = input$svg_method %||% "moransi")
      moran_task$invoke(
        bpcells_dir = global_data$spatial_obj$bpcells_dir,
        pass_idx    = shared_rv$qc_pass_idx,
        coords      = global_data$spatial_obj$coords,
        n_hvg       = input$n_hvg_moran,
        x_cuts      = input$moran_x_cuts %||% 0,
        y_cuts      = input$moran_y_cuts %||% 0,
        method      = input$svg_method %||% "moransi",
        log_file    = log_file
      )
    })

    observeEvent(moran_task$status(), {
      if (moran_task$status() == "success") {
        shared_rv$moran_results <- moran_task$result()
        showNotification(.tr("Autocorrelation spatiale terminee."), type = "message", duration = 4)
      } else if (moran_task$status() == "error") {
        showNotification(
          .tr("Erreur (ou depassement du delai) pendant le calcul de Moran — voir le log. Essayez 'Reinitialiser les daemons' puis relancez."),
          type = "error", duration = 10)
      }
    })

    output$moran_progress_text <- renderText({
      global_data$language  # re-render on language switch
      lines <- tracker()
      if (length(lines) == 0) return(.tr("En attente..."))
      paste(lines, collapse = "\n")
    })

    output$moran_table <- DT::renderDT({
      req(shared_rv$moran_results)
      ts_datatable(shared_rv$moran_results, page_length = 15L, filename_base = "spatial_qc_moran",
                   filter = "none", scroll_x = FALSE) |>
        DT::formatRound(c("moran_i", "p_value"), 4)
    })

    # ── Top SVGs grid ─────────────────────────────────────────────────────
    svg_grid_long <- eventReactive(input$btn_svg_grid, {
      req(shared_rv$moran_results, global_data$spatial_obj$sketch, global_data$spatial_obj$coords)

      ord <- order(-shared_rv$moran_results$moran_i)
      top_genes <- utils::head(shared_rv$moran_results$gene[ord], input$n_top_svg)
      top_genes <- intersect(top_genes, rownames(global_data$spatial_obj$sketch))
      validate(need(length(top_genes) > 0,
                    .tr("Aucun des genes les mieux classes (Moran's I) n'est present dans le sketch (RAM). Relancez l'autocorrelation ou reduisez N.")))

      sk <- global_data$spatial_obj$sketch
      if (!"data" %in% SeuratObject::Layers(sk)) sk <- Seurat::NormalizeData(sk, verbose = FALSE)
      expr_mat <- as.matrix(SeuratObject::LayerData(sk, layer = "data")[top_genes, , drop = FALSE])

      coords  <- global_data$spatial_obj$coords
      base_df <- coords[match(colnames(sk), coords$id), c("id", "x", "y")]

      long <- do.call(rbind, lapply(top_genes, function(g) {
        data.frame(base_df, gene = g, expr = as.numeric(expr_mat[g, base_df$id]))
      }))
      long <- long[stats::complete.cases(long[, c("x", "y")]), ]
      long$gene <- factor(long$gene, levels = top_genes)
      long
    })

    output$svg_grid_plot <- renderPlot({
      global_data$language  # re-render on language switch
      long <- svg_grid_long()
      n_facet_col <- 3
      p <- ggplot2::ggplot(long, ggplot2::aes(x = x, y = -y, color = expr))
      if (requireNamespace("scattermore", quietly = TRUE)) {
        p <- p + scattermore::geom_scattermore(pointsize = 4.5)
      } else {
        p <- p + ggplot2::geom_point(size = 0.8)
      }
      method_lbl <- if (identical(shared_rv$moran_params$method, "markvariogram")) {
        .tr("mark variogram")
      } else .tr("indice de Moran")
      p + ggplot2::facet_wrap(~gene, ncol = n_facet_col) +
        ggplot2::scale_color_viridis_c(option = "plasma") +
        ggplot2::coord_fixed() + ts_theme("void", 15) +
        ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", size = 15),
                       legend.text = ggplot2::element_text(size = 12),
                       legend.title = ggplot2::element_text(size = 13),
                       plot.title = ggplot2::element_text(size = 17, face = "bold"),
                       panel.spacing = ggplot2::unit(1, "lines")) +
        ggplot2::labs(color = .tr("Expression"),
                      title = sprintf(.tr("Top %d genes spatialement variables (%s)"),
                                       length(unique(long$gene)), method_lbl))
    })

    output$svg_grid_plot_ui <- renderUI({
      n_genes <- input$n_top_svg %||% 9
      n_rows  <- ceiling(n_genes / 3)
      height_px <- max(650, n_rows * 340)
      plotOutput(ns("svg_grid_plot"), height = paste0(height_px, "px"))
    })

    # =========================================================================
    # B4 (vague 5) — Hotspots locaux (Getis-Ord Gi*), SYNCHRONE
    # compute_getis_ord_hotspots(coords, values, k_neighbors) ->
    # data.frame(id, value, gi_star, p_value, hotspot). No log_file/mirai --
    # see file header for why (cheap, closed-form, no permutation).
    # =========================================================================
    output$hotspot_deconv_celltype_ui <- renderUI({
      req(shared_rv$deconv_props)
      cts <- setdiff(colnames(shared_rv$deconv_props), "id")
      selectInput(ns("hotspot_deconv_celltype"), NULL, choices = cts)
    })

    hotspot_result <- eventReactive(input$btn_hotspots, {
      req(global_data$spatial_obj$coords)
      values <- if (identical(input$hotspot_source, "deconv")) {
        req(shared_rv$deconv_props, input$hotspot_deconv_celltype)
        stats::setNames(shared_rv$deconv_props[[input$hotspot_deconv_celltype]], shared_rv$deconv_props$id)
      } else {
        req(shared_rv$qc_metrics, input$hotspot_qc_metric)
        stats::setNames(shared_rv$qc_metrics[[input$hotspot_qc_metric]], shared_rv$qc_metrics$id)
      }
      # The SAME writer the drive path uses: one shape, written once. The drive
      # action cannot reuse this eventReactive (it reads `input$`, which a driven
      # run has none of), so the two paths converge on the writer, not on the
      # reactive. The parameter block is recorded BEFORE the computation, as it
      # always was, so a failed computation still leaves the provenance of what was
      # attempted.
      spatial_hotspot_store(NULL,
        list(source = input$hotspot_source,
             metric = if (identical(input$hotspot_source, "deconv")) input$hotspot_deconv_celltype else input$hotspot_qc_metric,
             k_neighbors = input$hotspot_k),
        shared_rv)
      res <- tryCatch(
        compute_getis_ord_hotspots(coords = global_data$spatial_obj$coords, values = values,
                                    k_neighbors = input$hotspot_k),
        error = function(e) {
          showNotification(paste(.tr("Erreur hotspots :"), conditionMessage(e)), type = "error", duration = 8)
          NULL
        }
      )
      spatial_hotspot_store(res,
        list(source = input$hotspot_source,
             metric = if (identical(input$hotspot_source, "deconv")) input$hotspot_deconv_celltype else input$hotspot_qc_metric,
             k_neighbors = input$hotspot_k),
        shared_rv)
      req(res)
      res
    })

    # S1 — `hotspot_result` IS LAZY, and it used to have exactly ONE dependent:
    # `output$hotspot_map` below. A Shiny reactive that nobody reads is never
    # evaluated, so the human click on `btn_hotspots` was only ever executed
    # *because a plot happened to read the result*. Re-pointing the map at the
    # store — the correct fix for the drive, which cannot click the button — would
    # therefore have silently killed the human path as well: the observer would
    # invalidate, nothing would evaluate, and the store would stay empty.
    # Measured, not theorised: with the map moved to the store and nothing here,
    # `shared_rv$hotspot_result` stayed NULL after a real `btn_hotspots` click.
    #
    # One line, and it makes the event's OWN execution explicit instead of an
    # accident of who happens to render. The drive path already had this shape —
    # its own observer on `spqc_drive_counter`, further down this server — so the
    # human path now has its counterpart. `ignoreNULL = FALSE` because the result
    # is a data.frame, not a value, and a `NULL` return from a failed computation
    # is precisely the case that must not be skipped.
    #
    # NB the comment above deliberately spells the drive observer in prose rather
    # than as a literal call. `tools/check_duplication.R` extracts an
    # `observeEvent` trigger by scanning LINES and does not skip comments, so
    # quoting the call here registered a second "repeated trigger" and pushed the
    # warning ceiling from 3 to 4 for a line of English. The ceiling is a debt
    # ceiling and must not grow on a comment.
    observeEvent(input$btn_hotspots, hotspot_result(), ignoreNULL = FALSE)

    # ── DRIVE (MCP) — spatial-qc-btn_hotspots ────────────────────────────────
    # The human observer above is NOT re-wired and NOT duplicated: the drive action
    # calls `run_spatial_hotspots()`, which makes the same domain call on the
    # FROZEN k and the rule-resolved metric, then publishes through the same
    # `spatial_hotspot_store()`. `spqc_drive_counter` is the only trigger, so no
    # `input$` is read here - that is what makes a frozen input set possible at
    # all, and it is also why the metric needs a RULE.
    spqc_drive_counter <- shiny::reactiveVal(0L)
    spqc_drive_run_state <- shiny::reactiveVal("idle")
    spqc_drive_last_step <- new.env(parent = emptyenv())
    spqc_drive_last_step$value <- NULL
    spqc_drive_job <- new.env(parent = emptyenv())
    spqc_drive_job$id <- NULL

    spqc_drive_ready <- function() {
      .spatial_qc_drive_ready(shared_rv, global_data)
    }
    spqc_drive_state <- function() {
      .spatial_qc_drive_state(shared_rv, global_data,
                              spqc_drive_run_state,
                              spqc_drive_last_step$value)
    }

    # `long = TRUE` is declared even though Getis-Ord is cheap: the guarantee it
    # buys is that `running` has a real producer, and the job is SYNCHRONOUS, so
    # no tick would otherwise report `running` at all. No `timeout_s` is declared,
    # matching the other long jobs, so the poller's default ceiling applies.
    #
    # The button id is a LITERAL here on purpose: test-drive-watcher.R greps every
    # TS_DRIVE_BUTTONS entry as a quoted literal beside a `ts_drive_publish_token(`
    # call, and that shared check is what proves an allowlist entry is really
    # wired. The assertions keep the literal and the constant from drifting.
    ts_drive_publish_token(global_data, "spatial-qc-btn_hotspots",
      spqc_drive_counter, ready = spqc_drive_ready,
      state = spqc_drive_state, long = TRUE)

    close_spqc_drive_job <- function(status, error = NULL) {
      if (is.null(spqc_drive_job$id)) return(invisible(FALSE))
      job <- tryCatch(ts_drive_job_state(), error = function(e) NULL)
      if (is.null(job) || !identical(job$job_id, spqc_drive_job$id)) {
        return(invisible(FALSE))
      }
      ts_drive_job_finish(.SPATIAL_QC_DRIVE_BUTTON, status = status,
                          error = error, job_id = spqc_drive_job$id)
    }

    observeEvent(spqc_drive_counter(), {
      if (!isTRUE(spqc_drive_ready())) return()
      spqc_drive_job$id <- NULL
      job <- tryCatch(ts_drive_job_state(), error = function(e) NULL)
      if (!is.null(job) && isTRUE(ts_drive_job_busy()) &&
          identical(job$button, .SPATIAL_QC_DRIVE_BUTTON)) {
        spqc_drive_job$id <- job$job_id
      }
      spqc_drive_run_state("running")
      res <- .spatial_qc_run_drive(global_data, shared_rv, close_spqc_drive_job)
      spqc_drive_last_step$value <- res$step
      spqc_drive_run_state(res$status)
    }, ignoreInit = TRUE)

    output$hotspot_status_ui <- renderUI({
      global_data$language  # re-render on language switch
      req(shared_rv$hotspot_result)
      n_hot <- sum(shared_rv$hotspot_result$hotspot == "Hotspot (chaud)")
      n_cold <- sum(shared_rv$hotspot_result$hotspot == "Coldspot (froid)")
      div(class = "alert alert-success", style = "font-size:0.75rem;",
          .t_fmt(.tr("{hot} hotspot(s), {cold} coldspot(s) sur {total} elements (p < 0.05)."),
                 hot = n_hot, cold = n_cold, total = nrow(shared_rv$hotspot_result)))
    })

    .hotspot_palette <- c("Hotspot (chaud)" = "#D55E00", "Coldspot (froid)" = "#0072B2", "NS" = "#CCCCCC")

    output$hotspot_map <- renderPlot({
      global_data$language  # re-render on language switch
      # S1. This used to read `hotspot_result()`, the `eventReactive(input$btn_hotspots)`
      # declared above, and that was the defect: the drive action dispatches
      # `spqc_drive_counter()` and the Spatial pipeline writes
      # `shared_rv$hotspot_result` directly, so NEITHER ever set the button and the
      # map could not re-evaluate after either. A human click happened to satisfy
      # the eventReactive, which is why the human path was never the failing one
      # and why the panel looked broken only where it was actually driven.
      # It now reads the STORE, like its three siblings at :880, :913 and :942.
      # The value is identical: the eventReactive's own last act is
      # `spatial_hotspot_store(res, ...)` at :809, so both sources are the same
      # table. The `req()` is the siblings' guard, and it is what makes a cleared
      # store clear the plot instead of leaving a stale frame on screen.
      req(shared_rv$hotspot_result)
      df <- shared_rv$hotspot_result
      coords <- global_data$spatial_obj$coords
      m <- match(df$id, coords$id)
      df$x <- coords$x[m]; df$y <- coords$y[m]
      df <- df[stats::complete.cases(df[, c("x", "y")]), ]
      p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = -y, color = hotspot))
      if (requireNamespace("scattermore", quietly = TRUE)) {
        p <- p + scattermore::geom_scattermore(pointsize = 3, alpha = 0.85)
      } else {
        p <- p + ggplot2::geom_point(size = 0.7, alpha = 0.85)
      }
      p + ggplot2::scale_color_manual(values = .hotspot_palette,
                                      labels = c("Hotspot (chaud)" = .tr("Hotspot (chaud)"),
                                                 "Coldspot (froid)" = .tr("Coldspot (froid)"),
                                                 "NS" = .tr("NS"))) +
        ggplot2::coord_fixed() + ts_theme("void", 12) +
        ggplot2::labs(color = NULL, title = .tr("Hotspots locaux (Getis-Ord Gi*)"))
    })

    output$hotspot_hist <- renderPlot({
      global_data$language  # re-render on language switch
      req(shared_rv$hotspot_result)
      
      df <- shared_rv$hotspot_result
      
      ggplot2::ggplot(
        df,
        ggplot2::aes(x = gi_star, fill = hotspot)
      ) +
        ggplot2::geom_histogram(bins = 50) +
        ggplot2::geom_vline(
          xintercept = c(-1.96, 1.96),
          color = "grey30",
          linetype = "dashed"
        ) +
        ggplot2::scale_fill_manual(values = .hotspot_palette,
                                   labels = c("Hotspot (chaud)" = .tr("Hotspot (chaud)"),
                                              "Coldspot (froid)" = .tr("Coldspot (froid)"),
                                              "NS" = .tr("NS"))) +
        ts_theme("minimal", 12) +
        ggplot2::labs(
          x = "Gi* (z-score)",
          y = .tr("Effectif"),
          fill = NULL,
          title = .tr("Distribution du Gi*"),
          subtitle = .tr("Pointilles = seuil p < 0.05 (|z| > 1.96)")
        )
    })
    
    output$hotspot_table <- DT::renderDT({
      req(shared_rv$hotspot_result)
      
      ts_datatable(
        shared_rv$hotspot_result,
        page_length = 15L,
        filename_base = "spatial_qc_hotspots"
      ) |>
        DT::formatRound(c("value", "gi_star", "p_value"), 3)
    })

    # ── Export CSV des hotspots (Phase F) ──────────────────────────────────
    # The handler reads the SAME slot the table renders, so the file and the DOM
    # cannot disagree — a second computation here would be a second chance to
    # export something the panel never showed.
    #
    # The dataset name is part of the FILENAME only. It comes from the module's own
    # active dataset, never from a scenario, and it is not published on the drive:
    # a biological sample name has no business in `result.json`, which is read by a
    # remote caller. The agent reads the numbers, not the sample.
    # S2 — the ONE serialiser for the hotspot table.
    #
    # The human `downloadHandler` below and the drive export route
    # (`spatial_qc_export_hotspot_csv`) BOTH call this. Two serialisations would
    # drift, and the drift would be invisible: the agent would receive a file the
    # analyst's own download does not match, and nothing in either path would
    # report a difference. `row.names = FALSE` for the reason the handler already
    # gave — an exported file whose first column is a meaningless row index is a
    # file an analyst has to clean before use.
    #
    # MODULE LEVEL, and that is load-bearing rather than incidental. A function
    # declared inside `moduleServer()` is a closure: no offline test and no second
    # caller can reach it, which is the same trap `load_single_cell_data()` set for
    # the SC importer. Both S2 helpers take the stores as ARGUMENTS precisely so
    # they can live here, next to `spatial_hotspot_store()` and
    # `.spatial_qc_drive_state()`.
    output$dl_hotspot_csv <- downloadHandler(
      filename = function() {
        sprintf("spatial_qc_hotspots_%s_%s.csv",
                gsub("[^A-Za-z0-9._-]", "_",
                     as.character(global_data$active_spatial_dataset %||% "dataset")),
                Sys.Date())
      },
      content = function(file) {
        df <- shared_rv$hotspot_result
        validate(need(!is.null(df) && nrow(df) > 0,
                      .tr("Aucun resultat de hotspots a exporter.")))
        # Through the shared serialiser, so this file and the drive export route's
        # file are byte-identical.
        spatial_hotspot_csv_write(df, file)
      }
    )

    # Published as a closure over THIS module's own `shared_rv`, and published at
    # all rather than bound to a button: an export is a READ of a result that
    # already exists, and a fake clickable control plus an extra entry in the
    # closed `spatial-` set would buy nothing.
    #
    # The closure taking NO ARGUMENTS is the binding. The route is welded to the
    # store that produced the result, so nothing upstream — a scenario field, a
    # second module, a different `shared_rv` — can point the export at another
    # state. It also means the app-side seam needs no `shared_rv` at all, which
    # does not exist at app scope.
    ts_drive_publish_export(global_data, TS_DRIVE_SPATIAL_QC_MODULE, function() {
      spatial_qc_export_hotspot_csv(shared_rv, global_data)
    })
  })
}

# ── S2: the ONE drive export route ───────────────────────────────────────────
# Bound to THIS module and to the result THIS session already produced. The caller
# supplies no handler, no outputId, no destination, no filename and no format; the
# destination is `ts_drive_export_dir()` and the basename is
# `ts_drive_export_next_path()` over the shared `TS_DRIVE_EXPORT_STEM`, both
# app-controlled.
#
# The basename deliberately does NOT embed `active_spatial_dataset`, which the
# human filename does. That name is a sample name, and this descriptor is read by
# a remote caller: the agent gets counts, never the sample.
#
# Returns a VERDICT, never throws, for the reason `run_spatial_import()` gives: a
# `tryCatch` in the wrapper would buy a second place for the two channels to
# disagree. `status = "invalid"` means "this session has nothing to export";
# `status = "error"` means the write itself failed.
spatial_qc_export_hotspot_csv <- function(shared_rv, global_data, dir = NULL) {
  bad <- function(status, msg) {
    list(ok = FALSE, status = status, errors = msg, descriptor = NULL)
  }
  df <- tryCatch(shiny::isolate(shared_rv$hotspot_result), error = function(e) NULL)
  if (is.null(df)) {
    return(bad("invalid",
      paste("this session has produced no hotspot result yet; run",
            "spatial-qc-btn_hotspots (or the panel's own button) first")))
  }
  # The same shape the readers assume. The single writer's guard normally makes
  # this unreachable; the export is the one place that must not hand a
  # reader-breaking value to a FILE either.
  if (!is.data.frame(df) || !nrow(df) ||
      !all(c("id", "value", "gi_star", "p_value", "hotspot") %in% names(df))) {
    return(bad("invalid", "the stored hotspot result is not the Getis-Ord table"))
  }
  if (is.null(dir) || !nzchar(dir)) dir <- ts_drive_export_dir()
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  # MEASURED DESIGN DECISION, and the first version was wrong. A single fixed
  # basename made `TS_DRIVE_EXPORT_MAX_FILES` and `ts_drive_export_prune()`
  # UNREACHABLE — the route always overwrote one file, so the directory could
  # never exceed one entry and the cap was dead code, which is the same defect the
  # C9/C9b work exists to eliminate. Each export is therefore a DISTINCT file,
  # named by the app from what is already on disk: one more than the highest
  # index present. No caller input, no session field, no sample name, and no new
  # mutable state — the count is derived, so a second export in the same session
  # cannot collide with the first.
  file <- spatial_qc_export_next_path(dir)
  wrote <- tryCatch({
    spatial_hotspot_csv_write(df, file)
    TRUE
  }, error = function(e) conditionMessage(e))
  if (!isTRUE(wrote)) {
    return(bad("error", paste("the export could not be written:", wrote)))
  }
  ts_drive_export_prune(dir)
  list(ok = TRUE, status = "done", errors = character(0), warnings = character(0),
       descriptor = list(
         format = "csv",
         file = basename(file),
         bytes = as.integer(file.size(file)),
         n_rows = as.integer(nrow(df)),
         n_cols = as.integer(ncol(df)),
         n_sig = as.integer(sum(df$hotspot != "NS", na.rm = TRUE)),
         columns = as.character(names(df))
       ))
}

#' The next unused export path under `dir`, derived from what is already there.
#'
#' `TS_DRIVE_EXPORT_STEM` plus an index one above the highest present. Deriving
#' the index rather than keeping a counter means the function is stateless, so it
#' cannot disagree with the directory after a restart, a prune, or two exports in
#' the same session.
spatial_qc_export_next_path <- function(dir) {
  stem <- TS_DRIVE_EXPORT_STEM
  present <- list.files(dir, pattern = paste0("^", stem, "_[0-9]+\\.csv$"))
  idx <- suppressWarnings(as.integer(sub(paste0("^", stem, "_"), "",
                                         sub("\\.csv$", "", present))))
  idx <- idx[!is.na(idx)]
  n <- if (length(idx)) max(idx) + 1L else 1L
  file.path(dir, sprintf("%s_%d.csv", stem, n))
}

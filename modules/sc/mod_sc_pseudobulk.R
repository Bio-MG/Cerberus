# =============================================================================
# mod_sc_pseudobulk.R — Pseudobulk differential expression (Single-Cell)
# =============================================================================
# Cerberus 1.0 / Step 4.0 -- SG-1 (CRITICAL scientific gap): mod_sc_markers.R
# only exposes FindAllMarkers() (cluster-vs-rest, CELL-level) -- no test
# exists that compares CONDITIONS (treated/control, genotype, ...) while
# avoiding pseudo-replication (treating cells from the same sample/animal as
# independent replicates inflates significance -- Squair et al. 2021).
#
# Reuses the Bulk DE engine (helpers_bulk.R: build_dds(), run_bulk_de_dispatch(),
# validate_bulk_design(), .normalize_de_cols(), plot_bulk_pca(),
# plot_volcano_bulk(), plot_ma_bulk(), build_de_results_dt(), filter_bulk_counts())
# on counts SUMMED per (sample [, split_by]) -- one row per biological
# replicate, exactly like an ordinary Bulk RNA-seq design -- instead of
# building a second, parallel DE engine. "Reutiliser les modules existants."
#
# Two-step flow (mirrors mod_bulk_filter.R -> mod_bulk_de.R):
#   1. Aggregate: pick a sample/replicate column (+ optional split-by column,
#      e.g. a cluster/cell-type, for cell-type-specific pseudobulk), a
#      condition column, and a min-cells-per-group floor -> pseudobulk counts
#      + metadata, with a PCA of the pseudo-samples for QC (mislabeled
#      sample / outlier detection) before any contrast is even chosen.
#   2. Differential expression: pick target/reference condition levels + a
#      statistical engine (DESeq2/edgeR/limma-voom) -> DE table, volcano,
#      MA-plot -- same visuals as Bulk RNA-seq, same interpretation.
#
# FindAllMarkers() (mod_sc_markers.R) is untouched and remains the
# cluster-level/exploratory tool; this module is the condition-level test.
#
# MD-3 (ROADMAP_MULTI_DATASET.md) — additive bridge to the bulk multi-dataset
# container: a "Envoyer vers comparaison Bulk" button registers the pseudobulk
# run (aggregated counts + metadata + the single DE contrast) into
# global_data$bulk_datasets via bulk_multi_register(producer = "pseudobulk"),
# making it selectable in mod_bulk_multi.R's "Comparaison multi-jeux" tab.
# Pure logic lives in R/bulk/bulk_multi.R (frozen contract
# BULK_MULTI_CONTRACT.md §6, producer 3); this module only READS its own pb$
# state — bulk_obj / shared_rv are never touched here.
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# -----------------------------------------------------------------------------
# Pure helpers (no Shiny reactivity) -- unit-testable in isolation.
# -----------------------------------------------------------------------------

#' Aggregate single-cell raw counts into a pseudobulk matrix
#'
#' Sums raw counts per (sample_id) or per (sample_id x split_by) group,
#' producing a genes x pseudo-samples count matrix ready for
#' helpers_bulk.R's DESeq2/edgeR/limma engine -- avoids the pseudo-
#' replication trap of testing condition differences on individual cells.
#'
#' Sparse-safe: sums via an indicator-matrix multiply (same pattern already
#' used in helpers_sc.R::remap_seurat_ids_to_symbol()'s duplicate-symbol
#' collapse), never densifying the full genes x cells matrix. If the object
#' is disk-backed (BPCells), counts are coerced to an in-memory sparse
#' matrix first -- a bounded, ONE-TIME cost regardless of dataset size,
#' since the OUTPUT (genes x a handful of pseudo-samples) is always small.
#'
#' @param obj Seurat object (raw counts in the default assay).
#' @param sample_col Metadata column identifying the biological
#'   sample/replicate (e.g. "orig.ident").
#' @param split_by Optional metadata column (e.g. a cluster/cell-type
#'   column) to aggregate SEPARATELY per (sample, split_by) pair --
#'   standard "pseudobulk per cell type" design. NULL = one profile per
#'   sample (whole-sample pseudobulk).
#' @param min_cells_per_group Minimum cells required for a (sample[,split])
#'   group to be kept -- groups below this are dropped (too few cells to
#'   trust a summed profile), reported in `dropped`.
#' @return list(
#'   counts   = genes x pseudo-samples integer matrix,
#'   metadata = data.frame (rownames = pseudo-sample id): sample, split (if
#'              split_by was used), n_cells,
#'   dropped  = data.frame(group, n_cells) for groups excluded by the floor
#' )
aggregate_pseudobulk_counts <- function(obj, sample_col, split_by = NULL,
                                        min_cells_per_group = 10L) {
  if (!sample_col %in% colnames(obj@meta.data)) {
    stop(errorCondition(sprintf("Colonne d'echantillon '%s' introuvable dans les metadonnees.", sample_col), class = "sc_pseudobulk_error"))
  }
  counts <- tryCatch(
    SeuratObject::LayerData(obj, layer = "counts"),
    error = function(e) Seurat::GetAssayData(obj, slot = "counts")
  )
  if (!inherits(counts, c("dgCMatrix", "matrix"))) {
    counts <- methods::as(counts, "CsparseMatrix")
  }

  sample_vec <- as.character(obj@meta.data[[sample_col]])
  if (!is.null(split_by)) {
    if (!split_by %in% colnames(obj@meta.data)) {
      stop(errorCondition(sprintf("Colonne de regroupement '%s' introuvable dans les metadonnees.", split_by), class = "sc_pseudobulk_error"))
    }
    split_vec <- as.character(obj@meta.data[[split_by]])
    group_vec <- paste(sample_vec, split_vec, sep = "__")
  } else {
    group_vec <- sample_vec
  }

  tab <- table(group_vec)
  keep_groups <- names(tab)[tab >= min_cells_per_group]
  dropped <- data.frame(group = names(tab)[tab < min_cells_per_group],
                        n_cells = as.integer(tab[tab < min_cells_per_group]),
                        stringsAsFactors = FALSE)
  if (length(keep_groups) < 2) {
    stop(errorCondition(paste0("Moins de 2 groupes (echantillon", if (!is.null(split_by)) "/regroupement" else "",
         ") avec au moins ", min_cells_per_group, " cellules -- pseudobulk impossible."),
         class = "sc_pseudobulk_error"))
  }

  keep_cells <- group_vec %in% keep_groups   # NA sample/split values -> FALSE, silently dropped
  counts <- counts[, keep_cells, drop = FALSE]
  group_vec <- group_vec[keep_cells]

  lv <- sort(unique(group_vec))
  G <- Matrix::sparseMatrix(i = seq_along(group_vec), j = match(group_vec, lv),
                            x = 1, dims = c(length(group_vec), length(lv)))
  pb_counts <- as.matrix(counts %*% G)
  dimnames(pb_counts) <- list(rownames(counts), lv)
  storage.mode(pb_counts) <- "integer"

  meta <- data.frame(row.names = lv)
  if (!is.null(split_by)) {
    parts <- strsplit(lv, "__", fixed = TRUE)
    meta$sample <- vapply(parts, `[`, character(1), 1)
    meta$split  <- vapply(parts, `[`, character(1), 2)
  } else {
    meta$sample <- lv
  }
  meta$n_cells <- as.integer(tab[lv])

  list(counts = pb_counts, metadata = meta, dropped = dropped)
}

#' Attach a per-cell condition column to pseudobulk metadata
#'
#' A CONDITION is a property of the sample/animal, not of the cell -- this
#' resolves it once per sample and reports (never silently swallows) any
#' sample whose cells disagree on the condition value, which is very
#' likely mislabeled metadata rather than a legitimate mixed sample.
#'
#' @param obj Seurat object (source of per-cell metadata).
#' @param pb_meta Pseudobulk metadata data.frame from
#'   aggregate_pseudobulk_counts() ($metadata), with a $sample column.
#' @param sample_col Metadata column matching pb_meta$sample.
#' @param condition_col Metadata column to attach (must be constant per sample).
#' @return list(
#'   metadata     = pb_meta with a new $condition column,
#'   inconsistent = character vector of sample ids with > 1 condition value
#'                  (majority value used for those, but callers should stop()
#'                  rather than silently proceed when this is non-empty)
#' )
resolve_pseudobulk_condition <- function(obj, pb_meta, sample_col, condition_col) {
  if (!condition_col %in% colnames(obj@meta.data)) {
    stop(errorCondition(sprintf("Colonne de condition '%s' introuvable dans les metadonnees.", condition_col), class = "sc_pseudobulk_error"))
  }
  meta <- obj@meta.data
  by_sample <- split(as.character(meta[[condition_col]]), as.character(meta[[sample_col]]))
  n_levels  <- vapply(by_sample, function(x) length(unique(x[!is.na(x)])), integer(1))
  inconsistent <- names(n_levels)[n_levels > 1]

  cond_per_sample <- vapply(by_sample, function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_character_)
    names(sort(table(x), decreasing = TRUE))[1]   # majority value (reported if inconsistent)
  }, character(1))

  pb_meta$condition <- unname(cond_per_sample[pb_meta$sample])
  list(metadata = pb_meta, inconsistent = inconsistent)
}

# -----------------------------------------------------------------------------
# UI: sidebar controls
# -----------------------------------------------------------------------------
mod_sc_pseudobulk_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-light", style = "font-size:0.85em;border-left:3px solid #8E44AD;",
        bsicons::bs_icon("exclamation-diamond"),
        i18n$t(" Test au niveau ECHANTILLON (pseudobulk), pas au niveau cellule -- evite la pseudo-replication de FindAllMarkers/FindMarkers pour comparer des CONDITIONS (traite/controle, etc.). Necessite au moins 2 echantillons biologiques par condition.")),

    h6(i18n$t("1. Agregation Pseudobulk"), style = "font-weight:bold;"),
    selectInput(ns("sample_col"), i18n$t("Colonne 'echantillon' (replicat biologique)"), choices = NULL),
    selectInput(ns("condition_col"), i18n$t("Colonne 'condition' a comparer"), choices = NULL),
    selectInput(ns("split_by"), i18n$t("Regrouper aussi par (optionnel, ex: type cellulaire)"),
                choices = c("Aucun (pseudobulk par echantillon entier)" = "none")),
    numericInput(ns("min_cells_per_group"), i18n$t("Min cellules par groupe"), value = 10, min = 1, step = 1),
    actionButton(ns("run_aggregate"), i18n$t("Agreger en pseudobulk"),
                 class = "btn-primary w-100", icon = icon("layer-group")),
    div(class = "small text-muted mt-1", textOutput(ns("aggregate_status"))),

    hr(),
    h6(i18n$t("2. Analyse Differentielle (pseudobulk)"), style = "font-weight:bold;"),
    selectInput(ns("group_target"), i18n$t("Groupe Cible"), choices = NULL),
    selectInput(ns("group_ref"), i18n$t("Groupe Référence"), choices = NULL),
    radioButtons(ns("engine"), i18n$t("Moteur Statistique"),
                 choices = setNames(c("deseq2", "edger", "limma"),
                                    c(.tr_plain("DESeq2 (recommandé)"), "edgeR", "limma-voom")),
                 selected = "deseq2"),
    uiOutput(ns("pb_no_rep_ui")),
    fluidRow(
      column(6, numericInput(ns("lfc_thresh"), i18n$t("Seuil |Log2FC|"), value = 1, step = 0.1)),
      column(6, numericInput(ns("padj_thresh"), i18n$t("Seuil p-adj"), value = 0.05, step = 0.01))
    ),
    actionButton(ns("run_de"), i18n$t("Lancer l'Analyse Différentielle"),
                 class = "btn-success w-100", icon = icon("chart-line")),
    hr(),
    h6(i18n$t("3. Envoi vers la comparaison multi-jeux Bulk"), style = "font-weight:bold;"),
    div(class = "small text-muted mb-1",
        i18n$t("Enregistre le résultat DE pseudobulk (comptages agrégés + contraste) dans le conteneur bulk_datasets pour le comparer aux jeux Bulk (onglet « Comparaison multi-jeux » du module Bulk). Le jeu actif n'est jamais modifié.")),
    textInput(ns("pb_send_label"), i18n$t("Label du dataset"),
              placeholder = .tr_plain("ex : pseudobulk_T2_vs_ctrl")),
    actionButton(ns("pb_send"), i18n$t("Envoyer vers comparaison Bulk"),
                 class = "btn-outline-primary w-100", icon = icon("paper-plane")),
    div(class = "small text-muted mt-1", textOutput(ns("pb_send_status"))),
    hr(),
    downloadButton(ns("dl_pb_csv"), i18n$t("Export CSV"), class = "btn-sm btn-info w-100"),
    div(class = "small text-muted mt-2", textOutput(ns("de_status")))
  )
}

# -----------------------------------------------------------------------------
# UI: output panel
# -----------------------------------------------------------------------------
mod_sc_pseudobulk_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header(i18n$t("Pseudobulk DE — Comparaison de Conditions")),
    navset_tab(
      nav_panel(i18n$t("QC Pseudobulk (PCA)"),
                div(class = "small text-muted mt-2",
                    i18n$t("Chaque point = un pseudo-echantillon (cellules sommees par echantillon). Verifiez qu'il n'y a pas d'outlier/mauvais etiquetage avant d'interpreter le contraste.")),
                plotOutput(ns("pb_pca_plot"), height = "450px"),
                DTOutput(ns("pb_summary_table"))),
      nav_panel(i18n$t("Volcano + MA-Plot"),
                plotOutput(ns("pb_volcano"), height = "420px"),
                plotOutput(ns("pb_ma"), height = "380px")),
      nav_panel(i18n$t("Table DE"), DTOutput(ns("pb_de_table")))
    )
  )
}

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
mod_sc_pseudobulk_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    .tr <- function(key) {
      tr <- isolate(global_data$i18n)
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    observeEvent(global_data$language, {
      updateSelectInput(session, "sample_col", label = .tr("Colonne 'echantillon' (replicat biologique)"))
      updateSelectInput(session, "condition_col", label = .tr("Colonne 'condition' a comparer"))
      updateSelectInput(session, "split_by", label = .tr("Regrouper aussi par (optionnel, ex: type cellulaire)"))
      updateNumericInput(session, "min_cells_per_group", label = .tr("Min cellules par groupe"))
      updateActionButton(session, "run_aggregate", label = .tr("Agreger en pseudobulk"))
      updateSelectInput(session, "group_target", label = .tr("Groupe Cible"))
      updateSelectInput(session, "group_ref", label = .tr("Groupe Référence"))
      updateRadioButtons(session, "engine", label = .tr("Moteur Statistique"))
      updateNumericInput(session, "lfc_thresh", label = .tr("Seuil |Log2FC|"))
      updateNumericInput(session, "padj_thresh", label = .tr("Seuil p-adj"))
      updateActionButton(session, "run_de", label = .tr("Lancer l'Analyse Différentielle"))
      updateTextInput(session, "pb_send_label", label = .tr("Label du dataset"),
                      placeholder = .tr("ex : pseudobulk_T2_vs_ctrl"))
      updateActionButton(session, "pb_send", label = .tr("Envoyer vers comparaison Bulk"))
    }, ignoreInit = TRUE)

    pb <- reactiveValues(counts = NULL, metadata = NULL, dropped = NULL, de_result = NULL)

    # ── Refresh metadata-column choices when a new SC object is loaded ─────
    observeEvent(global_data$sc_obj, {
      req(global_data$sc_obj)
      meta <- global_data$sc_obj@meta.data
      cat_cols <- names(meta)[vapply(meta, function(x) is.factor(x) || is.character(x), logical(1))]
      cat_cols <- unique(c("orig.ident", cat_cols))
      cat_cols <- cat_cols[cat_cols %in% names(meta)]
      updateSelectInput(session, "sample_col", choices = cat_cols,
                        selected = if ("orig.ident" %in% cat_cols) "orig.ident" else cat_cols[1])
      cond_default <- setdiff(cat_cols, "orig.ident")
      updateSelectInput(session, "condition_col", choices = cat_cols,
                        selected = if (length(cond_default)) cond_default[1] else cat_cols[1])
      split_choices <- c(setNames("none", .tr("Aucun (pseudobulk par echantillon entier)")),
                        setNames(cat_cols, cat_cols))
      updateSelectInput(session, "split_by", choices = split_choices, selected = "none")
    }, ignoreInit = TRUE)

    # ── Step 1: aggregate ───────────────────────────────────────────────────
    agg_status_rv <- reactiveVal("En attente d'agregation...")
    output$aggregate_status <- renderText({ agg_status_rv() })

    observeEvent(input$run_aggregate, {
      req(global_data$sc_obj, input$sample_col, input$condition_col)
      obj <- global_data$sc_obj

      tryCatch({
        split_by <- if (identical(input$split_by, "none") || is.null(input$split_by)) NULL else input$split_by

        res <- aggregate_pseudobulk_counts(obj, sample_col = input$sample_col,
                                           split_by = split_by,
                                           min_cells_per_group = input$min_cells_per_group %||% 10L)

        cond <- resolve_pseudobulk_condition(obj, res$metadata, sample_col = input$sample_col,
                                             condition_col = input$condition_col)
        if (length(cond$inconsistent) > 0) {
          stop(errorCondition(sprintf(
            "La condition '%s' n'est pas constante au sein de l'echantillon(s) suivant(s) : %s -- verifiez vos metadonnees (une CONDITION doit etre une propriete de l'echantillon/animal, pas de la cellule).",
            input$condition_col, paste(cond$inconsistent, collapse = ", ")),
            class = "sc_pseudobulk_error"))
        }
        pb_meta <- cond$metadata
        pb_meta$condition <- factor(pb_meta$condition)
        if (nlevels(pb_meta$condition) < 2) {
          stop(errorCondition("La colonne condition n'a qu'un seul niveau apres agregation -- impossible de comparer.", class = "sc_pseudobulk_error"))
        }

        pb$counts    <- res$counts
        pb$metadata  <- pb_meta
        pb$dropped   <- res$dropped
        pb$de_result <- NULL   # invalidate any previous DE result

        updateSelectInput(session, "group_target", choices = levels(pb_meta$condition))
        updateSelectInput(session, "group_ref", choices = levels(pb_meta$condition),
                          selected = levels(pb_meta$condition)[min(2, nlevels(pb_meta$condition))])

        n_dropped <- if (!is.null(res$dropped)) nrow(res$dropped) else 0L
        agg_status_rv(sprintf(
          "OK -- %d pseudo-echantillon(s) (%d gene(s)), %d groupe(s) trop petit(s) exclu(s) (< %s cellules).",
          ncol(pb$counts), nrow(pb$counts), n_dropped, input$min_cells_per_group %||% 10L))
        showNotification(sprintf("Pseudobulk OK : %d pseudo-echantillons.", ncol(pb$counts)),
                         type = "message", duration = 5)
      }, error = function(e) {
        agg_status_rv(paste("Erreur:", conditionMessage(e)))
        showNotification(paste("Erreur agregation pseudobulk:", conditionMessage(e)),
                         type = "error", duration = 10)
      })
    })

    output$pb_summary_table <- renderDT({
      req(pb$metadata)
      ts_datatable(pb$metadata, page_length = 15L,
                   filename_base = "sc_pseudobulk_metadata", rownames = TRUE,
                   filter = "none")
    })

    output$pb_pca_plot <- renderPlot({
      req(pb$counts, pb$metadata)
      validate(need(ncol(pb$counts) >= 3, "Au moins 3 pseudo-echantillons requis pour une PCA lisible."))
      vst_mat <- tryCatch({
        counts_f <- filter_bulk_counts(pb$counts, min_count = 10, min_samples = 1, min_count_per_sample = 1)
        dds_qc <- build_dds(counts_f, pb$metadata, design_formula = "~ condition", run_deseq = FALSE)
        dds_qc <- DESeq2::estimateSizeFactors(dds_qc)
        get_vst_matrix(dds_qc)
      }, error = function(e) NULL)
      validate(need(!is.null(vst_mat), "VST indisponible pour la PCA QC (package 'DESeq2' requis)."))
      plot_bulk_pca(vst_mat, pb$metadata, color_by = "condition")
    })

    # ── Step 2: differential expression ─────────────────────────────────────
    de_status_rv <- reactiveVal("En attente de l'analyse...")
    output$de_status <- renderText({ de_status_rv() })

    # ── Plan sans réplicat (n = p) : même diagnostic que le Bulk ────────────
    # Le pseudobulk peut parfaitement produire un pseudo-échantillon par
    # condition (4 conditions -> 4 pseudo-échantillons) : le design est alors
    # saturé et aucun moteur ne peut estimer une dispersion. Voir
    # mod_bulk_de_run.R pour le détail mesuré des trois moteurs.
    output$pb_no_rep_ui <- renderUI({
      req(pb$metadata)
      if (!"condition" %in% colnames(pb$metadata)) return(NULL)
      sat <- design_saturation(pb$metadata, "condition")
      if (!sat$saturated) return(NULL)

      on <- isTRUE(input$pb_no_rep_enable)
      tagList(
        div(class="alert alert-danger", style="font-size:0.76em;padding:6px 10px;",
            tags$strong("Plan sans r\u00e9plicat d\u00e9tect\u00e9"),
            tags$div(sprintf("%d pseudo-\u00e9chantillon(s) pour %d coefficient(s) : aucun degr\u00e9 de libert\u00e9 r\u00e9siduel. Le calcul sera bascul\u00e9 sur edgeR avec une dispersion impos\u00e9e.",
                             sat$n, sat$p))),
        checkboxInput(ns("pb_no_rep_enable"),
                      "Activer le mode exploratoire sans r\u00e9plicat (dispersion impos\u00e9e)",
                      value = on),
        if (on) tagList(
          numericInput(ns("pb_no_rep_bcv"), "BCV impos\u00e9 (dispersion = BCV\u00b2)",
                       value = 0.4, min = 0.01, max = 2, step = 0.05),
          checkboxInput(ns("pb_no_rep_attest"),
                        "J'atteste comprendre que ces p-values reposent sur une dispersion impos\u00e9e et ne sont PAS inf\u00e9rentielles.",
                        value = isTRUE(input$pb_no_rep_attest))
        )
      )
    })

    observeEvent(input$run_de, {
      req(pb$counts, pb$metadata, input$group_target, input$group_ref)
      if (identical(input$group_target, input$group_ref)) {
        showNotification("Le groupe Cible et le groupe Reference doivent etre differents.",
                         type = "warning", duration = 6)
        return()
      }

      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message = "Analyse differentielle pseudobulk...", value = 0.2)

      tryCatch({
        counts_f <- filter_bulk_counts(pb$counts, min_count = 10, min_samples = 1, min_count_per_sample = 1)

        problems <- validate_bulk_design(pb$metadata, "condition")
        if (length(problems) > 0) {
          showNotification(paste(problems, collapse = " | "), type = "warning", duration = 12)
        }

        # HARD BLOCK : plan saturé (n = p) — aucune dispersion estimable.
        sat    <- design_saturation(pb$metadata, "condition")
        bypass <- FALSE
        if (sat$saturated) {
          if (!isTRUE(input$pb_no_rep_enable) || !isTRUE(input$pb_no_rep_attest)) {
            showNotification(sprintf(
              "Plan sans replicat : %d pseudo-echantillon(s) pour %d coefficient(s) - aucune dispersion estimable. Activez le mode exploratoire ET cochez l'attestation pour continuer.",
              sat$n, sat$p), type = "error", duration = 12)
            return()
          }
          bypass <- TRUE
        }
        fixed_disp <- if (bypass) (input$pb_no_rep_bcv %||% 0.4)^2 else NULL

        p$set(0.4, "Ajustement du modele...")
        dds <- if (!bypass && identical(input$engine, "deseq2")) {
          build_dds(counts_f, pb$metadata, design_formula = "~ condition", run_deseq = TRUE)
        } else NULL

        p$set(0.7, "Extraction du contraste...")
        res_df <- if (bypass) {
          run_bulk_de_dispatch("edger", counts_f, pb$metadata, "condition",
                               input$group_target, input$group_ref,
                               fixed_dispersion = fixed_disp)
        } else {
          run_bulk_de_dispatch(input$engine, counts_f, pb$metadata, "condition",
                               input$group_target, input$group_ref, dds = dds, shrink = TRUE)
        }
        res_df <- .normalize_de_cols(res_df, counts_for_basemean = counts_f)

        pb$de_result <- res_df

        n_sig <- sum(!is.na(res_df$padj) & res_df$padj < (input$padj_thresh %||% 0.05) &
                      abs(res_df$log2FoldChange) > (input$lfc_thresh %||% 1))

        # Post-V1.0 (mandat utilisateur) : exposition additive du resultat DE
        # pseudobulk au rapport consolide (lecture seule ; pb$ reste la
        # reference du panel 4b — aucun changement de comportement du panneau).
        shared_rv$pseudobulk_result <- list(
          type = "sc_pseudobulk_de",
          engine = if (bypass) "edger" else input$engine,
          target = input$group_target,
          reference = input$group_ref,
          n_genes = nrow(res_df),
          n_significant = n_sig,
          de_table = res_df,
          # Contournement scientifique : doit voyager avec le résultat, sinon
          # le rapport consolidé le présenterait comme une DE ordinaire.
          no_replicate_bypass = if (bypass) {
            list(bcv = sqrt(fixed_disp), dispersion = fixed_disp,
                 n = sat$n, p = sat$p, attested = TRUE, timestamp = Sys.time())
          } else NULL
        )
        if (bypass) {
          de_status_rv(paste(de_status_rv(),
                             sprintf("-- MODE EXPLORATOIRE : dispersion imposee (BCV = %.2f), p-values NON inferentielles.",
                                     sqrt(fixed_disp))))
        }

        de_status_rv(sprintf("OK [%s] -- %d/%d genes significatifs (%s vs %s).",
                             toupper(input$engine), n_sig, nrow(res_df),
                             input$group_target, input$group_ref))
        showNotification(sprintf("DE pseudobulk terminee : %d genes significatifs.", n_sig),
                         type = "message", duration = 6)
      }, error = function(e) {
        de_status_rv(paste("Erreur:", conditionMessage(e)))
        showNotification(paste("Erreur DE pseudobulk:", conditionMessage(e)),
                         type = "error", duration = 10)
      })
    })

    # ── MD-3: bridge to the bulk multi-dataset container ────────────────────
    send_status_rv <- reactiveVal("")
    output$pb_send_status <- renderText({ send_status_rv() })

    # Default label only fills an EMPTY field (never clobbers a typed label),
    # same discipline as mod_bulk_datasets.R.
    observeEvent(pb$de_result, {
      req(pb$de_result, input$group_target, input$group_ref)
      if (!nzchar(trimws(input$pb_send_label %||% ""))) {
        updateTextInput(session, "pb_send_label",
                        value = paste0("pseudobulk_", input$group_target,
                                       "_vs_", input$group_ref))
      }
    })

    observeEvent(input$pb_send, {
      global_data$language  # i18n
      if (is.null(pb$de_result) || is.null(pb$counts) || is.null(pb$metadata)) {
        showNotification(.tr("Aucun résultat DE pseudobulk à envoyer — lancez d'abord l'analyse différentielle (étape 2)."),
                         type = "warning", duration = 6)
        return()
      }
      lbl <- tryCatch(bulk_multi_check_label(input$pb_send_label),
                      bulk_multi_error = function(e) e)
      if (inherits(lbl, "bulk_multi_error")) {
        showNotification(lbl$message, type = "error", duration = 8)
        return()
      }
      contrast_name <- paste0(input$group_target, "_vs_", input$group_ref)
      pipeline <- tryCatch(
        bulk_multi_capture_pipeline(list(
          contrasts   = setNames(list(pb$de_result), contrast_name),
          lfc_thresh  = input$lfc_thresh %||% 1,
          padj_thresh = input$padj_thresh %||% 0.05
        )),
        bulk_multi_error = function(e) e)
      if (inherits(pipeline, "bulk_multi_error")) {
        showNotification(pipeline$message, type = "error", duration = 8)
        return()
      }
      had <- lbl %in% names(global_data$bulk_datasets)
      new_ds <- tryCatch(
        bulk_multi_register(
          global_data$bulk_datasets, lbl,
          obj = list(counts = pb$counts, metadata = pb$metadata),
          pipeline_state = pipeline,
          producer = "pseudobulk", overwrite = TRUE),
        bulk_multi_error = function(e) e)
      if (inherits(new_ds, "bulk_multi_error")) {
        showNotification(new_ds$message, type = "error", duration = 8)
        return()
      }
      global_data$bulk_datasets <- new_ds
      send_status_rv(sprintf(.tr(if (had)
        "✓ Jeu « %s » mis à jour dans bulk_datasets (producteur pseudobulk)."
        else "✓ Jeu « %s » enregistré dans bulk_datasets (producteur pseudobulk)."), lbl))
      showNotification(sprintf(
        .tr(if (had)
          "✓ Pseudobulk mis à jour sous « %s » — comparable dans l'onglet « Comparaison multi-jeux » du module Bulk."
          else "✓ Pseudobulk enregistré sous « %s » — comparable dans l'onglet « Comparaison multi-jeux » du module Bulk."), lbl),
        type = "message", duration = 6)
    })

    output$pb_volcano <- renderPlot({
      req(pb$de_result)
      plot_volcano_bulk(pb$de_result, lfc_thresh = input$lfc_thresh %||% 1,
                        padj_thresh = input$padj_thresh %||% 0.05)
    })
    output$pb_ma <- renderPlot({
      req(pb$de_result)
      plot_ma_bulk(pb$de_result, lfc_thresh = input$lfc_thresh %||% 1,
                   padj_thresh = input$padj_thresh %||% 0.05)
    })
    output$pb_de_table <- renderDT({
      req(pb$de_result)
      build_de_results_dt(pb$de_result)
    })

    output$dl_pb_csv <- downloadHandler(
      filename = function() paste0("pseudobulk_DE_", input$group_target, "_vs_", input$group_ref, "_", Sys.Date(), ".csv"),
      content = function(file) {
        req(pb$de_result)
        write.csv(pb$de_result, file, row.names = FALSE)
      }
    )
  })
}

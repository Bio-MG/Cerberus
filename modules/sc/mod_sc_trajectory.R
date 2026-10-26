# =============================================================================
# mod_sc_trajectory.R — Child 7: Trajectory analysis (two distinct methods)
# =============================================================================
# Scientific disclaimer:
# This module offers TWO scientifically DISTINCT trajectory methods:
#
#   1. "exploratory_knn" — exploratory weighted kNN-graph ordering of cells
#      (calculate_pseudotime(), helpers_sc.R). It is NOT Slingshot, Monocle,
#      diffusion pseudotime, or a branching lineage-inference method.
#      Pseudotime is unitless and depends on the selected computation space,
#      kNN parameter k, root definition, and graph connectivity.
#
#   2. "slingshot" — REAL Slingshot lineage inference via
#      calculate_slingshot_pseudotime() -> slingshot::slingshot(). Requires
#      cluster labels and the Bioconductor package 'Slingshot'. If Slingshot
#      is unavailable the app shows a clear French error and NEVER silently
#      falls back to the exploratory method.
#
# Depends on helpers_sc.R (sourced by app.R, not defined there):
#   calculate_pseudotime(embeddings, k, root_cells, root_method)
#     -> list(pseudotime, graph, root_cell, in_root_component, ...)
#   calculate_slingshot_pseudotime(embeddings, cluster_labels,
#       start_cluster, end_clusters, reduction)
#     -> list(pseudotime, pseudotime_matrix, curve_weights, lineages, ...)
#   plot_trajectory(embeddings, pseudotime, graph, root_cell, show_edges)
#   plot_slingshot_trajectory(embeddings, pseudotime, curves)
#   plot_pseudotime_distribution(seurat_obj)                  [Step-3.7]
#   plot_genes_vs_pseudotime(seurat_obj, genes, smooth_method) [Step-3.7]
#
# Step-3.9 changes (methodology fix):
#   - calculate_pseudotime() now takes an embedding MATRIX and returns a
#     result LIST (no longer mutates/returns a Seurat object). The raw
#     result is kept in traj_result() so the plot can overlay the root
#     cell and the report can check reduction provenance.
#   - Edges are weighted by Euclidean distance (RANN::nn2), not hop count;
#     no n x n dist() matrix is ever built (100k-cell guard is now real).
#   - Auto-root = double-sweep BFS diameter endpoint (not closeness max);
#     manual root is validated against the actual cell count.
#   - Default computation/display reduction is PCA (was UMAP); the chosen
#     reduction is mirrored to shared_rv$traj_reduction for the report.
#   - ONE gene picker only (traj_genes, Tab 3) feeds plots + CSV/PNG
#     exports; the old traj_genes_export duplicate was removed.
#   - Exports carry an explicit in_root_component flag (NA = outside the
#     root connected component, never silently filtered).
#
# Slingshot integration (current pass):
#   - Method selector (input$traj_method) branches the calculation observer;
#     each branch persists its own provenance into obj@meta.data:
#       traj_method ("exploratory_knn" | "slingshot"),
#       traj_computation_reduction, traj_root_method, traj_root_cell,
#       traj_root_cluster (Slingshot start cluster), traj_root_component_size
#       (exploratory: cells connected to root; Slingshot: cells with finite
#       pseudotime on the default lineage),
#       plus pseudotime_slingshot_1..k simple numeric columns per lineage.
#   - Connectivity flag: exploratory writes traj_in_component (root
#     connected component); Slingshot writes traj_in_root_component which
#     means "has finite pseudotime on the selected default lineage" — NOT a
#     connected-component flag. Stale columns from the other method are
#     removed on every run so exports never mix provenance.
#   - Multi-lineage pseudotimes stay available in traj_result()
#     (pseudotime_matrix, curve_weights, lineages); complex curve objects
#     are never written into meta.data.
#
# Safety block:
#   Hard ceiling at MAX_TRAJECTORY_CELLS = 100 000. Shows a clear UI
#   warning instead of attempting the calculation (mirrors global.R guard).
#
# State contract (shared_rv):
#   WRITE : shared_rv$active_tab     -> "tab_trajectory" after successful run
#           shared_rv$traj_reduction -> mirrors reduction used (report title/reduction sync)
#           shared_rv$traj_genes     -> mirrors current gene picker (report export)
#           shared_rv$traj_method    -> mirrors method used (report provenance)
#
# UI split:
#   mod_sc_trajectory_ui(id)         -> sidebar accordion body
#   mod_sc_trajectory_output_ui(id)  -> main panel i18n$t("Trajectory") tab
# =============================================================================

.MAX_TRAJECTORY_CELLS <- if (exists("TS_MAX_TRAJECTORY_CELLS")) TS_MAX_TRAJECTORY_CELLS else 100000L   # mirrors the guard in global.R


# ── UI: sidebar controls ──────────────────────────────────────────────────────

mod_sc_trajectory_ui <- function(id) {
  ns <- NS(id)
  tagList(

    div(class="d-flex align-items-center mb-2",
        span(i18n$t("Two distinct methods available: Exploratory Pseudotime (kNN graph) and Slingshot (lineage curves)."),
             style="font-size:0.9em;"),
        bslib::tooltip(
          bsicons::bs_icon("info-circle", class="ms-2 text-primary", style="cursor:help;"),
          i18n$t("Exploratory pseudotime orders cells along a weighted kNN graph (it does not infer valid lineages). Slingshot infers lineage curves from reduced space and clusters. Always interpret alongside known biology, marker expression, sample conditions, and independent lineage evidence."),
          placement="right"
        )
    ),

    div(
      class = "alert alert-light",
      style = "font-size:0.9em;border-left:3px solid #3498DB;",
      i18n$t("Analyse de trajectoire et pseudotemps.")
    ),

    uiOutput(ns("cell_count_badge")),   # live warning for large datasets

    # Trajectory METHOD selector — two scientifically distinct methods,
    # never interchangeable and never silently substituted.
    selectInput(
      ns("traj_method"),
      i18n$t("Méthode de trajectoire"),
      choices = setNames(
        c("exploratory_knn", "slingshot"),
        c(.tr_plain("Pseudotemps exploratoire — graphe kNN pondéré"),
          .tr_plain("Slingshot — inférence de lignées"))
      ),
      selected = "exploratory_knn"
    ),

    uiOutput(ns("traj_method_help")),

    selectInput(ns("traj_reduction"), i18n$t("Reduction a utiliser (calcul + affichage)"),
                choices  = c("PCA" = "pca", "UMAP" = "umap"),
                selected = "pca"),

    # Slingshot-only controls (cluster column + optional start/terminal
    # clusters); rendered only when method == "slingshot".
    uiOutput(ns("slingshot_controls")),

    # Exploratory-only controls (root detection + optional graph edges).
    conditionalPanel(
      condition = "input.traj_method == 'exploratory_knn'",
      ns = ns,
      checkboxInput(ns("traj_auto_root"), i18n$t("Detection auto racine"), value = TRUE),

      conditionalPanel(
        condition = "!input.traj_auto_root",
        ns = ns,
        numericInput(ns("traj_root_cell"), i18n$t("Index cellule racine"),
                     value = 1, min = 1, max = 1, step = 1)
      ),

      checkboxInput(ns("traj_show_edges"), i18n$t("Afficher le graphe (aretes echantillonnees)"),
                    value = FALSE)
    ),

    actionButton(ns("calc_trajectory"), i18n$t("Calculer Trajectoire"),
                 class = "btn-info w-100", icon = icon("project-diagram")),

    hr(),

    h6(i18n$t("Visualisation"), style = "font-weight:bold;"),

    selectInput(
      ns("traj_color"),
      i18n$t("Colorer par"),
      choices = setNames(c("pseudotime", "seurat_clusters"),
                         c(.tr_plain("Pseudotime"), .tr_plain("Clusters")))
    ),

    actionButton(ns("plot_trajectory_btn"), i18n$t("Actualiser Plot"),
                 class = "btn-outline-primary btn-sm w-100 mt-1"),

    hr(),
    downloadButton(ns("dl_pseudotime"), i18n$t("Export pseudotemps CSV"),
                   class = "btn-sm btn-info w-100"),

    hr(),
    div(class = "small text-muted", textOutput(ns("trajectory_status")))
  )
}


# ── UI: output panel ──────────────────────────────────────────────────────────

mod_sc_trajectory_output_ui <- function(id) {
  ns <- NS(id)
    card(
    full_screen = TRUE,
    card_header(i18n$t("Trajectory Analysis")),
    navset_tab(
      nav_panel(
        i18n$t("Plot Trajectoire"),
        plotOutput(ns("trajectory_plot"), height = "550px"),

        downloadButton(ns("dl_trajectory_png"), i18n$t("Export Plot"), class = "btn-sm btn-secondary w-100 mt-2"),

        hr(),
        h6(i18n$t("Analyses complémentaires"), style = "font-weight:bold; color:#3498DB"),
        div(class = "small text-muted mb-2",
            i18n$t("La sélection des gènes (Gènes vs Pseudotemps) se fait uniquement dans l'onglet Genes vs Pseudotemps ci-dessous — sélecteur unique.")),
        plotOutput(ns("plot_pseudotime_dist"), height = "280px"),
        fluidRow(
          column(4, downloadButton(ns("dl_pseudotime_dist"),
                                   i18n$t("Export Distribution CSV"), class = "btn-sm btn-info w-100")),
          column(4, downloadButton(ns("dl_genes_pseudotime"),
                                   i18n$t("Export Gènes/Pseudo CSV"), class = "btn-sm btn-info w-100")),
          column(4, selectInput(ns("traj_export_fmt"), i18n$t("Format export plots (PNG/PDF/SVG)"),
                                choices = ts_export_format_choices_ui(), selected = "png", width = "100%"))
        )
      ),
      nav_panel(
        i18n$t("Distribution Pseudotemps"),
        plotOutput(ns("pseudotime_distribution"), height = "400px"),
        downloadButton(ns("dl_dist_png"), i18n$t("Export Distribution"), class = "btn-sm btn-secondary w-100 mt-2")
      ),
      nav_panel(
        i18n$t("Genes vs Pseudotemps"),
        fluidRow(
          column(6,
            selectizeInput(ns("traj_genes"), i18n$t("Genes a tracer"),
                           choices = NULL, multiple = TRUE,
                           options = list(maxItems = 8,
                                         placeholder = "Ex: CD3D, MS4A1"))
          ),
          column(6,
            radioButtons(ns("traj_smooth"), i18n$t("Lissage"),
                         choices = c("LOESS" = "loess", "GAM" = "gam",
                                     "Lineaire" = "lm"),
                         inline = TRUE, selected = "loess")
          )
        ),
        div(class = "small text-muted mb-2",
            i18n$t("Le plot se met à jour automatiquement dès qu'un gène est sélectionné.")),
        plotOutput(ns("gene_pseudotime_plot"), height = "450px"),

        downloadButton(ns("dl_genes_png"), i18n$t("Export Gènes/Pseudo"), class = "btn-sm btn-secondary w-100 mt-2")
      )
    )
  )
}


# ── Server ────────────────────────────────────────────────────────────────────

mod_sc_trajectory_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    # ── i18n proxy ──────────────────────────────────────────────────────────
    .tr <- function(key) {
      tr <- isolate(global_data$i18n)
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }


    ns <- session$ns
    traj_status_rv <- reactiveVal(.tr("En attente du calcul..."))
    # Raw result of the LAST trajectory run — either method:
    #   - exploratory : calculate_pseudotime() list (graph, root_cell,
    #     in_root_component, computation_reduction)
    #   - slingshot   : calculate_slingshot_pseudotime() list
#     (pseudotime_matrix, curve_weights, lineages, curves, ...)
    # Feeds plot overlay/branching (method_type) and exports.
    traj_result    <- reactiveVal(NULL)

    observeEvent(global_data$language, {
      updateSelectInput(session, "traj_method",
        label = .tr("Méthode de trajectoire"),
        choices = setNames(
          c("exploratory_knn", "slingshot"),
          c(.tr("Pseudotemps exploratoire — graphe kNN pondéré"), .tr("Slingshot — inférence de lignées"))
        ),
        selected = isolate(input$traj_method) %||% "exploratory_knn"
      )
      updateSelectInput(session, "traj_reduction",
        label = .tr("Reduction a utiliser (calcul + affichage)"),
        choices = setNames(c("pca", "umap"), c("PCA", "UMAP")),
        selected = isolate(input$traj_reduction) %||% "pca"
      )
      updateSelectInput(session, "traj_color", label = .tr("Colorer par"))
      updateActionButton(session, "calc_trajectory", label = .tr("Calculer Trajectoire"))
      updateActionButton(session, "plot_trajectory_btn", label = .tr("Actualiser Plot"))
      updateSelectInput(session, "traj_smooth", label = .tr("Lissage"))
      updateCheckboxInput(session, "traj_auto_root", label = .tr("Detection auto racine"))
      updateCheckboxInput(session, "traj_show_edges", label = .tr("Afficher le graphe (aretes echantillonnees)"))
    }, ignoreInit = TRUE)
    # ── Live cell-count badge ──────────────────────────────────────────────
    output$cell_count_badge <- renderUI({
      obj <- global_data$sc_obj
      if (is.null(obj)) return(NULL)
      n <- ncol(obj)
      if (n > .MAX_TRAJECTORY_CELLS) {
        div(class = "alert alert-danger alert-sm", style = "font-size:0.85em;padding:6px;",
            icon("exclamation-triangle"),
            sprintf("Dataset trop grand (%s cellules > %s max). La trajectoire est désactivée.",
                    format(n, big.mark = " "), format(.MAX_TRAJECTORY_CELLS, big.mark = " ")))
      } else {
        div(class = "alert alert-success alert-sm", style = "font-size:0.85em;padding:6px;",
            icon("check-circle"),
            sprintf("%s cellules — trajectoire disponible.", format(n, big.mark = " ")))
      }
    })

    # ── Update choices after pipeline ──────────────────────────────────────
    observeEvent(global_data$sc_obj, {
      req(global_data$sc_obj)
      obj  <- global_data$sc_obj
      meta <- obj@meta.data
      valid_cols <- c("seurat_clusters", "orig.ident",
                      names(meta)[sapply(meta, function(x) is.factor(x) || is.character(x))])
      traj_choices <- setNames(c("pseudotime", "seurat_clusters"),
                                 c(.tr("Pseudotime"), .tr("Clusters")))
      if ("pseudotime" %in% colnames(meta)) {
        traj_choices <- c(traj_choices, setNames(valid_cols, valid_cols))
      }
      updateSelectInput(session, "traj_color", choices = unique(traj_choices))

      var_genes <- VariableFeatures(obj)
      all_genes <- c(var_genes, setdiff(rownames(obj), var_genes))
      updateSelectizeInput(session, "traj_genes", choices = all_genes, server = TRUE)
    })

    # ── 0bis. Method help + Slingshot controls (Slingshot integration) ─────

    # French explanation below the method selector — makes the scientific
    # distinction between the two methods explicit in the UI.
      output$traj_method_help <- renderUI({
      if (identical(input$traj_method, "slingshot")) {
        tags$p(class = "small text-muted", .tr("Slingshot infère des courbes de lignées à partir d'un espace réduit et de clusters. Les résultats doivent être interprétés avec la biologie connue."))
      } else {
        tags$p(class = "small text-muted", .tr("Ordonnancement exploratoire fondé sur un graphe kNN pondéré. Cette méthode n'infère pas de lignées."))
      }
    })

    # Categorical metadata columns usable as cluster labels (seurat_clusters
    # offered FIRST; other factor/character columns are listed so the user
    # can explicitly select them — never silently substituted).
    cluster_cols_rv <- reactiveVal(c("seurat_clusters" = "seurat_clusters"))
    observeEvent(global_data$sc_obj, {
      req(global_data$sc_obj)
      meta <- global_data$sc_obj@meta.data
      cat_cols <- names(meta)[vapply(meta, function(x) is.factor(x) || is.character(x), logical(1))]
      cols <- if ("seurat_clusters" %in% cat_cols) {
        c("seurat_clusters", setdiff(cat_cols, "seurat_clusters"))
      } else {
        cat_cols
      }
      if (!length(cols)) cols <- "seurat_clusters"
      cluster_cols_rv(setNames(cols, cols))
    })

    # Slingshot-only controls: rendered ONLY when method == "slingshot".
    # If seurat_clusters is missing, an explicit French warning is shown and
    # the user must consciously pick another column (no silent fallback).
    output$slingshot_controls <- renderUI({
      req(identical(input$traj_method, "slingshot"))
      obj  <- global_data$sc_obj
      cols <- cluster_cols_rv()
      has_default <- "seurat_clusters" %in% colnames(if (!is.null(obj)) obj@meta.data else data.frame())
      tagList(
        selectInput(ns("traj_cluster_col"), .tr("Colonne de clusters"),
                    choices = cols,
                    selected = isolate(input$traj_cluster_col) %||% "seurat_clusters"),
        selectizeInput(ns("traj_start_cluster"),
                       .tr("Cluster de départ (optionnel)"),
                       choices = NULL,
                       selected = isolate(input$traj_start_cluster),
                        multiple = FALSE,
                        options = list(placeholder = .tr_plain("Laisser vide pour le défaut Slingshot"))),
        selectizeInput(ns("traj_end_clusters"),
                       .tr("Clusters terminaux (optionnel)"),
                       choices = NULL,
                       selected = isolate(input$traj_end_clusters),
                       multiple = TRUE,
                       options = list(maxItems = 10)),
        if (!has_default) {
          div(class = "alert alert-warning alert-sm", style = "font-size:0.85em;padding:6px;",
              icon("exclamation-triangle"),
               .tr("'seurat_clusters' est absent des métadonnées. Lancez le pipeline de clustering, ou choisissez explicitement une colonne de clusters valide ci-dessus."))
        }
      )
    })

    # Cluster LEVELS for the start/terminal-cluster selectors follow the
    # chosen cluster column (and the loaded object).
    observeEvent(c(global_data$sc_obj, input$traj_cluster_col), {
      req(global_data$sc_obj, input$traj_cluster_col)
      col  <- input$traj_cluster_col
      meta <- global_data$sc_obj@meta.data
      levels_chr <- if (col %in% colnames(meta)) {
        sort(unique(as.character(meta[[col]])))
      } else character(0)
      updateSelectizeInput(session, "traj_start_cluster", choices = levels_chr,
                           selected = isolate(input$traj_start_cluster))
      updateSelectizeInput(session, "traj_end_clusters", choices = levels_chr,
                           selected = intersect(isolate(input$traj_end_clusters) %||% character(0), levels_chr))
    })

    # ── 0. Manual root validation + dynamic max (Step-3.9) ─────────────────

    #' Validate a manual root cell index against the current object size
    #' @param root_cell Raw numeric input value.
    #' @param n_cells Integer, ncol(seurat_obj).
    #' @return Validated integer root cell index, or triggers shiny::validate().
    validate_manual_root <- function(root_cell, n_cells) {
      root_cell <- suppressWarnings(as.integer(root_cell))
      if (length(root_cell) != 1L || is.na(root_cell) || root_cell < 1L || root_cell > n_cells) {
        shiny::validate(shiny::need(FALSE, sprintf(.tr("Index de cellule racine invalide. Choisissez une valeur entre 1 et %d."), n_cells)))
      }
      root_cell
    }

    # Keep the numericInput ceiling in sync with the loaded object so an
    # out-of-range root index can never be typed in silently.
    observeEvent(global_data$sc_obj, {
      req(global_data$sc_obj)
      updateNumericInput(session, "traj_root_cell",
                         max = ncol(global_data$sc_obj),
                         value = min(isolate(input$traj_root_cell) %||% 1, ncol(global_data$sc_obj)))
    }, ignoreInit = FALSE)

    # ── 1. Calculate trajectory ────────────────────────────────────────────
    # Both methods persist provenance into obj@meta.data so reports/exports
    # survive session save/load without transient state:
    #   traj_method, traj_computation_reduction, traj_root_method,
    #   traj_root_cell, traj_root_cluster, traj_root_component_size,
    #   pseudotime + (exploratory) traj_in_component /
    #   (slingshot) traj_in_root_component + pseudotime_slingshot_* columns.
    observeEvent(input$calc_trajectory, {
      req(global_data$sc_obj)
      obj <- global_data$sc_obj

      if (ncol(obj) > .MAX_TRAJECTORY_CELLS) {
        showNotification(.tr("Dataset trop grand pour la trajectoire."), type = "error", duration = 6)
        traj_status_rv(.tr("BLOQUE: dataset trop grand."))
        return()
      }

      reduction_used <- input$traj_reduction
      if (!reduction_used %in% names(obj@reductions)) {
        showNotification(.tr("Réduction non trouvée. Lancez le pipeline d'abord."), type = "error", duration = 6)
        return()
      }

      embedding <- Seurat::Embeddings(obj, reduction = reduction_used)
      attr(embedding, "reduction") <- reduction_used

      method_key <- input$traj_method %||% "exploratory_knn"

      p <- shiny::Progress$new(); on.exit(p$close())

      tryCatch({
        # Stale multi-lineage columns must never survive a new run (they
        # would silently leak outdated provenance into exports/reports).
        stale_slingshot_cols <- grep("^pseudotime_slingshot_", colnames(obj@meta.data), value = TRUE)

        if (identical(method_key, "slingshot")) {
          # ══════════════ Slingshot branch (real lineage inference) ════════
          # Explicit French error when Slingshot is unavailable — NEVER a
          # silent fallback to the exploratory graph method.
          if (!requireNamespace("slingshot", quietly = TRUE)) {
            stop(.tr("Le package Bioconductor slingshot est requis pour l'option Slingshot : inférence de lignées. Installez-le via BiocManager::install('slingshot') puis relancez le calcul. (Aucune bascule automatique vers la méthode exploratoire.)"),
                 call. = FALSE)
          }
          p$set(message = "Calcul Slingshot...", value = 0.4)

          cluster_col <- input$traj_cluster_col %||% "seurat_clusters"
          if (!cluster_col %in% colnames(obj@meta.data)) {
            stop(sprintf(.tr("Colonne de clusters '%s' introuvable dans les métadonnées. Choisissez une colonne de clusters existante (aucune substitution silencieuse)."), cluster_col),
                 call. = FALSE)
          }
          cluster_labels <- as.character(obj@meta.data[[cluster_col]])
          # Cluster labels must be aligned EXACTLY with the embedding rows.
          if (!identical(rownames(embedding), rownames(obj@meta.data))) {
            idx <- match(rownames(embedding), rownames(obj@meta.data))
            if (anyNA(idx)) {
              stop(.tr("Alignement cellules/clusters impossible."), call. = FALSE)
            }
            cluster_labels <- cluster_labels[idx]
          }
          if (anyNA(cluster_labels) || any(!nzchar(cluster_labels))) {
            stop(.tr("La colonne de clusters sélectionnée contient des valeurs manquantes ou vides."),
                 call. = FALSE)
          }
          # Empty optional selectize values -> NULL (Slingshot defaults).
          start_clus <- input$traj_start_cluster
          end_clus   <- input$traj_end_clusters
          start_clus <- if (length(start_clus) == 1L && nzchar(start_clus)) start_clus else NULL
          end_clus   <- if (length(end_clus) > 0L) end_clus else NULL

          result <- calculate_slingshot_pseudotime(
            embeddings     = embedding,
            cluster_labels = cluster_labels,
            start_cluster  = start_clus,
            end_clusters   = end_clus,
            reduction      = reduction_used
          )
          result$method_type <- "slingshot"

          obj@meta.data[stale_slingshot_cols] <- NULL  # no-op on first run
          # Default exported/displayed pseudotime = FIRST lineage only
          # (compatibility default, not a biologically validated choice).
          obj@meta.data$pseudotime             <- result$pseudotime
          # For Slingshot this flag means "has finite pseudotime on the
          # selected default lineage" — NOT a connected-component flag.
          obj@meta.data$traj_in_root_component <- !is.na(result$pseudotime)
          if ("traj_in_component" %in% colnames(obj@meta.data)) {
            obj@meta.data$traj_in_component <- NULL
          }
          # Full multi-lineage information stays available as simple
          # numeric metadata columns AND inside traj_result().
          for (j in seq_len(ncol(result$pseudotime_matrix))) {
            obj@meta.data[[sprintf("pseudotime_slingshot_%d", j)]] <-
              result$pseudotime_matrix[, j]
          }
          n_finite <- sum(is.finite(result$pseudotime))
          obj@meta.data$traj_method                <- rep("slingshot", ncol(obj))
          obj@meta.data$traj_computation_reduction <- rep(reduction_used, ncol(obj))
          obj@meta.data$traj_root_method           <- rep(result$root_method, ncol(obj))
          obj@meta.data$traj_root_cell             <- rep(NA_integer_, ncol(obj))
          obj@meta.data$traj_root_cluster          <- rep(as.character(result$root_cluster %||% NA_character_), ncol(obj))
          # For Slingshot this counts cells with finite pseudotime on the
          # default lineage — NOT a connected-component size.
          obj@meta.data$traj_root_component_size   <- rep(n_finite, ncol(obj))

          status_msg <- sprintf(paste0("OK Slingshot (%d lignee(s), espace %s). ",
                                       "Pseudotemps affiche : premiere lignee (defaut de compatibilite). ",
                                       "%d/%d cellules avec pseudotemps fini."),
                                ncol(result$pseudotime_matrix), reduction_used,
                                n_finite, result$n_cells)
        } else {
          # ══════════ Exploratory weighted kNN graph branch ════════════════
          p$set(message = .tr("Calcul trajectoire..."), value = 0.4)

          # Manual root validated INSIDE tryCatch so shiny::validate()'s
          # condition lands in the status text + notification instead of
          # bubbling up uncaught (no crash on out-of-range index).
          root_cells <- NULL
          if (!input$traj_auto_root) {
            root_cells <- validate_manual_root(input$traj_root_cell, nrow(embedding))
          }

          result <- calculate_pseudotime(
            embeddings = embedding,
            k = 15,
            root_cells = root_cells,
            root_method = if (is.null(root_cells)) "diameter" else "manual"
          )
          result$method_type <- "exploratory_knn"

          obj@meta.data[stale_slingshot_cols] <- NULL
          if ("traj_in_root_component" %in% colnames(obj@meta.data)) {
            obj@meta.data$traj_in_root_component <- NULL
          }
          obj@meta.data$pseudotime        <- result$pseudotime
          obj@meta.data$traj_in_component <- result$in_root_component
          obj@meta.data$traj_method                <- rep("exploratory_knn", ncol(obj))
          obj@meta.data$traj_computation_reduction <- rep(reduction_used, ncol(obj))
          obj@meta.data$traj_root_method           <- rep(result$root_method, ncol(obj))
          obj@meta.data$traj_root_cell             <- rep(result$root_cell, ncol(obj))
          obj@meta.data$traj_root_cluster          <- rep(NA_character_, ncol(obj))
          obj@meta.data$traj_root_component_size   <- rep(result$root_component_size, ncol(obj))

          pct <- round(100 * result$root_component_size / result$n_cells, 1)
          status_msg <- sprintf(
            "OK Pseudotemps calcule (graphe kNN pondere, espace %s). %d/%d cellules (%.1f%%) connectees a la racine.",
            reduction_used, result$root_component_size, result$n_cells, pct
          )
        }

        global_data$sc_obj <- obj

        traj_result(result)
        state_set(shared_rv, "active_tab", "tab_trajectory")
        state_set(shared_rv, "traj_reduction", reduction_used)
        state_set(shared_rv, "traj_method", method_key)

        # CHRYSALIS 2E : provenance PRODUITE a cette etape (regle 7 AGENTS.md)
        # — seuls les parametres deja calcules/disponibles sont enregistres ;
        # la consolidation est reservee au rapport (macro-step 4F).
        provenance_append(shared_rv, new_provenance_entry(
          analysis_id = "sc-trajectory",
          method      = method_key,
          parameters  = list(
            reduction   = reduction_used,
            root_method = result$root_method,
            auto_root   = isTRUE(input$traj_auto_root)
          ),
          dataset    = obj,
          cells_used = result$n_cells
        ))

        traj_status_rv(status_msg)
        showNotification(
          if (identical(method_key, "slingshot")) .tr("Trajectoire Slingshot calculée")
          else .tr("Trajectoire calculee"),
          type = "message", duration = 4)

        meta <- obj@meta.data
        valid_cols <- names(meta)[sapply(meta, function(x) is.factor(x) || is.character(x))]
        # traj_* provenance columns are constants — never useful color groups.
        valid_cols <- setdiff(valid_cols, grep("^traj_", valid_cols, value = TRUE))
        traj_choices <- c(setNames(c("pseudotime", "seurat_clusters"),
                                   c(.tr("Pseudotime"), .tr("Clusters"))),
                          setNames(valid_cols, valid_cols))
        updateSelectInput(session, "traj_color", choices = unique(traj_choices), selected = "pseudotime")
      }, error = function(e) {
        traj_status_rv(paste(.tr("Erreur:"), e$message))
        showNotification(paste(.tr("Erreur trajectoire:"), e$message), type = "error", duration = 8)
      })
    })

    # ── 1b. Mirror gene picker to shared_rv (Step-3.7 — read by the SC report
    #    to render the same "Gènes vs Pseudotemps" plot on export) ─────────
    observeEvent(input$traj_genes, {
      state_set(shared_rv, "traj_genes", input$traj_genes)
    }, ignoreNULL = FALSE)

    # ── 2. Main trajectory plot ────────────────────────────────────────────
    output$trajectory_plot <- renderPlot({
      input$plot_trajectory_btn
      req(global_data$sc_obj, traj_result())
      obj    <- global_data$sc_obj
      result <- traj_result()
      colorby <- input$traj_color

      if (colorby == "pseudotime" && !"pseudotime" %in% colnames(obj@meta.data)) {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                   label = .tr("Pseudotemps non calculé."), size = 6, hjust = 0.5) +
                 theme_void())
      }

      display_embedding <- Seurat::Embeddings(obj, reduction = input$traj_reduction)

      # NOTE: deliberately separate rendering paths.
      #  - exploratory "pseudotime" -> plot_trajectory(): continuous viridis
      #    + root marker + optional subsampled edge overlay (needs traj_result()).
      #  - slingshot "pseudotime" -> plot_slingshot_trajectory(): pseudotime
      #    scatter of the default lineage; NO kNN graph edges and NO root
      #    marker are ever drawn on a Slingshot result.
      #  - any other (categorical) metadata -> Seurat::DimPlot on the same
      #    display reduction; scale_colour_viridis_c in plot_trajectory()
      #    requires a continuous variable so factors must not go through it.
      tryCatch({
        if (identical(colorby, "pseudotime")) {
          if (identical(result$method_type %||% "exploratory_knn", "slingshot")) {
            plot_slingshot_trajectory(
              embeddings = display_embedding,
              pseudotime = obj@meta.data$pseudotime,
              curves     = NULL,   # curves stay available in traj_result(); no fabricated overlay
              palette         = state_get(shared_rv, "sc_palette") %||% "default",
              manual_gradient = state_get(shared_rv, "sc_manual_gradient")
            )
          } else {
            plot_trajectory(
              embeddings  = display_embedding,
              pseudotime  = obj@meta.data$pseudotime,
              graph       = result$graph,
              root_cell   = result$root_cell,
              show_edges  = isTRUE(input$traj_show_edges),
              palette         = state_get(shared_rv, "sc_palette") %||% "default",
              manual_gradient = state_get(shared_rv, "sc_manual_gradient")
            )
          }
        } else {
          Seurat::DimPlot(obj, reduction = input$traj_reduction,
                          group.by = colorby, pt.size = 0.7) +
            ggplot2::theme_classic()
        }
      },
      error = function(e) ggplot() + annotate("text", x = 0.5, y = 0.5,
                                              label = paste("Erreur plot:", e$message),
                                              size = 5, color = "red", hjust = 0.5) + theme_void())
    })

    # ── 3. Pseudotime distribution (Step-3.7: shared helper) ───────────────
    output$pseudotime_distribution <- renderPlot({
      req(global_data$sc_obj)
      validate(need("pseudotime" %in% colnames(global_data$sc_obj@meta.data), .tr("Pseudotemps non calculé.")))
      tryCatch(plot_pseudotime_distribution(global_data$sc_obj,
                                            palette = state_get(shared_rv, "sc_palette") %||% "default",
                                            manual_colors = state_get(shared_rv, "sc_manual_colors")),
               error = function(e) { validate(need(FALSE, e$message)) })
    })

    output$plot_pseudotime_dist <- renderPlot({
      req(global_data$sc_obj)
      validate(need("pseudotime" %in% colnames(global_data$sc_obj@meta.data), .tr("Pseudotemps non calculé.")))
      tryCatch(plot_pseudotime_distribution(global_data$sc_obj,
                                            palette = state_get(shared_rv, "sc_palette") %||% "default",
                                            manual_colors = state_get(shared_rv, "sc_manual_colors")),
               error = function(e) { validate(need(FALSE, e$message)) })
    })

    # ── 4. Genes vs pseudotime — Step-3.7: NO MORE "Tracer" button, updates
    #    dynamically as soon as genes/lissage change (shared helper). ──────
    output$gene_pseudotime_plot <- renderPlot({
      req(global_data$sc_obj, input$traj_genes)
      validate(need("pseudotime" %in% colnames(global_data$sc_obj@meta.data), .tr("Pseudotemps non calculé.")))
      tryCatch(plot_genes_vs_pseudotime(global_data$sc_obj, input$traj_genes, input$traj_smooth %||% "loess",
                                        palette = state_get(shared_rv, "sc_palette") %||% "default",
                                        manual_colors = state_get(shared_rv, "sc_manual_colors")),
               error = function(e) { validate(need(FALSE, e$message)) })
    })

    # Step-3.9: output$plot_genes_pseudotime removed — traj_genes_export picker
    # is gone; Tab 3 (input$traj_genes) is the single source for gene plots.

    # ── 5. Status ──────────────────────────────────────────────────────────
    output$trajectory_status <- renderText({ traj_status_rv() })

    # ── 6. CSV Exports (rows are NEVER filtered — NA/out-of-component/
    #    outside-lineage cells always stay visible) ─────────────────────────
    # Connectivity flag semantics:
    #   - exploratory : traj_in_component = membership in the root
    #     connected component of the weighted kNN graph.
    #   - slingshot   : traj_in_root_component = has FINITE PSEUDOTIME on
    #     the default (first) lineage — NOT a connected-component flag.
    .export_flag_col <- function(meta) {
      if ("traj_in_component" %in% colnames(meta)) {
        meta$traj_in_component
      } else if ("traj_in_root_component" %in% colnames(meta)) {
        meta$traj_in_root_component
      } else {
        NA  # computed before any flag existed: explicit, never silent
      }
    }
    output$dl_pseudotime <- downloadHandler(
      filename = function() paste0("pseudotime_", Sys.Date(), ".csv"),
      content = function(file) {
        req(global_data$sc_obj, "pseudotime" %in% colnames(global_data$sc_obj@meta.data))
        meta <- global_data$sc_obj@meta.data
        out <- data.frame(
          cell_barcode = rownames(meta),
          pseudotime = meta$pseudotime,
          in_root_component = .export_flag_col(meta),
          stringsAsFactors = FALSE
        )
        # Provenance columns persisted at calculation time (NA for objects
        # predating this integration — backward compatible, no guessing).
        for (cn in c("traj_method", "traj_computation_reduction",
                     "traj_root_method", "traj_root_cell", "traj_root_cluster")) {
          out[[cn]] <- if (cn %in% colnames(meta)) meta[[cn]] else NA
        }
        out$seurat_clusters <- if ("seurat_clusters" %in% colnames(meta)) meta$seurat_clusters else NA
        # All available Slingshot lineage pseudotimes (exploratory runs
        # simply have none of these columns).
        for (cn in grep("^pseudotime_slingshot_", colnames(meta), value = TRUE)) {
          out[[cn]] <- meta[[cn]]
        }
        write.csv(out, file, row.names = FALSE)
      }
    )
    output$dl_pseudotime_dist <- downloadHandler(
      filename = function() paste0("pseudotime_distribution_", Sys.Date(), ".csv"),
      content = function(file) {
        req(global_data$sc_obj, "pseudotime" %in% colnames(global_data$sc_obj@meta.data))
        meta <- global_data$sc_obj@meta.data
        out <- data.frame(cell_barcode = rownames(meta),
                          pseudotime = meta$pseudotime,
                          in_root_component = .export_flag_col(meta),
                          stringsAsFactors = FALSE)
        for (cn in c("traj_method", "traj_computation_reduction",
                     "traj_root_method", "traj_root_cell", "traj_root_cluster")) {
          out[[cn]] <- if (cn %in% colnames(meta)) meta[[cn]] else NA
        }
        out$seurat_clusters <- if ("seurat_clusters" %in% colnames(meta)) meta$seurat_clusters else NA
        for (cn in grep("^pseudotime_slingshot_", colnames(meta), value = TRUE)) {
          out[[cn]] <- meta[[cn]]
        }
        write.csv(out, file, row.names = FALSE)
      }
    )
    output$dl_genes_pseudotime <- downloadHandler(
      filename = function() paste0("genes_pseudotime_", input$traj_smooth %||% "loess", "_", Sys.Date(), ".csv"),
      content = function(file) {
        # Single gene picker (Tab 3, input$traj_genes exclusively).
        # Smoothing method is recorded in the filename AND in a
        # traj_smooth_method column — CSV rows carry raw expression only.
        req(global_data$sc_obj, input$traj_genes, input$traj_smooth)
        obj  <- global_data$sc_obj
        meta <- obj@meta.data
        valid_genes <- intersect(input$traj_genes, rownames(obj))
        req(length(valid_genes) > 0, "pseudotime" %in% colnames(meta))
        expr_df <- tryCatch(
          Seurat::FetchData(obj, vars = c("pseudotime", valid_genes), clean = FALSE),
          error = function(e) {
            # Older Seurat versions without the `clean` argument: retry
            # WITHOUT it rather than filtering rows manually.
            Seurat::FetchData(obj, vars = c("pseudotime", valid_genes))
          }
        )
        # Positional mapping keeps cell identity and requested order intact:
        # first requested variable = pseudotime, remaining = selected genes.
        n_gene_cols <- max(0L, ncol(expr_df) - 1L)
        out <- data.frame(
          cell_barcode = rownames(expr_df),
          pseudotime   = as.numeric(expr_df[[1L]]),
          stringsAsFactors = FALSE
        )
        out$in_root_component <- .export_flag_col(meta)[match(rownames(out), rownames(meta))]
        for (cn in c("traj_method", "traj_computation_reduction",
                     "traj_root_method", "traj_root_cell", "traj_root_cluster")) {
          out[[cn]] <- if (cn %in% colnames(meta)) meta[rownames(out), cn] else NA
        }
        out$traj_smooth_method <- rep(input$traj_smooth, nrow(out))
        if (n_gene_cols > 0L) {
          gene_block <- expr_df[, seq_len(n_gene_cols) + 1L, drop = FALSE]
          colnames(gene_block) <- valid_genes[seq_len(n_gene_cols)]
          out <- cbind(out, gene_block)
        }
        # ALL cells exported — NA-pseudotime rows are preserved as-is.
        write.csv(out, file, row.names = FALSE)
      }
    )

    # ── 7. Plot Exports (Step-3.7: fixed + format-aware PNG/PDF/SVG via
    #    input$traj_export_fmt, all routed through the shared plot helpers) ──
    output$dl_trajectory_png <- downloadHandler(
      filename = function() paste0("trajectory_plot_", Sys.Date(), ".", input$traj_export_fmt %||% "png"),
      content = function(file) {
        req(global_data$sc_obj, traj_result())
        obj    <- global_data$sc_obj
        result <- traj_result()
        p <- tryCatch({
          if (identical(input$traj_color %||% "pseudotime", "pseudotime")) {
            if (identical(result$method_type %||% "exploratory_knn", "slingshot")) {
              plot_slingshot_trajectory(
                embeddings = Seurat::Embeddings(obj, reduction = input$traj_reduction),
                pseudotime = obj@meta.data$pseudotime,
                curves     = NULL,
                palette         = state_get(shared_rv, "sc_palette") %||% "default",
                manual_gradient = state_get(shared_rv, "sc_manual_gradient")
              )
            } else {
              plot_trajectory(
                embeddings = Seurat::Embeddings(obj, reduction = input$traj_reduction),
                pseudotime = obj@meta.data$pseudotime,
                graph      = result$graph,
                root_cell  = result$root_cell,
                show_edges = isTRUE(input$traj_show_edges),
                palette         = state_get(shared_rv, "sc_palette") %||% "default",
                manual_gradient = state_get(shared_rv, "sc_manual_gradient")
              )
            }
          } else {
            Seurat::DimPlot(obj, reduction = input$traj_reduction,
                            group.by = input$traj_color, pt.size = 0.7) +
              ggplot2::theme_classic()
          }
        }, error = function(e) NULL)
        req(p)
        ts_export_plot(file, p, width = 8, height = 6, dpi = 300,
               format = input$traj_export_fmt %||% "png")
      }
    )
    output$dl_dist_png <- downloadHandler(
      filename = function() paste0("pseudotime_dist_", Sys.Date(), ".", input$traj_export_fmt %||% "png"),
      content = function(file) {
        req(global_data$sc_obj, "pseudotime" %in% colnames(global_data$sc_obj@meta.data))
        p <- tryCatch(plot_pseudotime_distribution(global_data$sc_obj,
                                                   palette = state_get(shared_rv, "sc_palette") %||% "default",
                                                   manual_colors = state_get(shared_rv, "sc_manual_colors")),
                      error = function(e) NULL)
        req(p)
        ts_export_plot(file, p, width = 7, height = 5, dpi = 300,
               format = input$traj_export_fmt %||% "png")
      }
    )
    output$dl_genes_png <- downloadHandler(
      filename = function() paste0("genes_pseudotime_", Sys.Date(), ".", input$traj_export_fmt %||% "png"),
      content = function(file) {
        req(global_data$sc_obj, input$traj_genes, "pseudotime" %in% colnames(global_data$sc_obj@meta.data))
        p <- tryCatch(plot_genes_vs_pseudotime(global_data$sc_obj, input$traj_genes,
                                               input$traj_smooth %||% "loess",
                                               palette = state_get(shared_rv, "sc_palette") %||% "default",
                                               manual_colors = state_get(shared_rv, "sc_manual_colors")),
                      error = function(e) NULL)
        req(p)
        ts_export_plot(file, p, width = 8, height = 6, dpi = 300,
               format = input$traj_export_fmt %||% "png")
      }
    )

    # M3 / Feature 18: publish sc_trajectory export route
    ts_drive_publish_export(global_data, "sc_trajectory", function() {
      sc_trajectory_export_pseudotime_csv(shared_rv, global_data)
    })
  }) # /moduleServer
}

# =============================================================================
# mod_sc_velocity.R — RNA Velocity Phase 3A Hardening + Stage 8 (3B-1) + 9 (3B-2)
# =============================================================================
# Strict RDS/MTX import, matrix validation, orientation, cell/gene alignment,
# precomputed UMAP vector validation, phase portraits, provenance export,
# reset when Seurat object changes. No inference, no loom/h5ad, no projection.
# Stage 8 : le module ORCHESTRE uniquement — la validation, le statut canonique
# (velocity_status_labels), la provenance et les exports residuent dans
# R/sc/sc_velocity.R. Validite technique affichee explicitement ; aucune
# validite biologique n'est suggeree.
# Stage 9 : les visualisations sont des CONSOMMATRICES pures du resultat
# canonique (assert_velocity_result) — fleches uniquement depuis des vecteurs
# valides, portee de donnees (preview/full) affichee, exports traces par
# analysis_id. Une figure ne cree, n'infere et ne repare jamais un resultat.
# =============================================================================

mod_sc_velocity_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-light", style = "font-size:0.9em;border-left:3px solid #2980B9;",
        i18n$t("RNA Velocity — import strict des matrices spliced/unspliced alignees sur l'objet Seurat. "),
        i18n$t("Aucune inference n'est effectuee dans l'application.")),

    radioButtons(ns("velocity_import_mode"), i18n$t("Source d'import"),
                  choices = setNames(c("rds", "mtx", "loom"),
                              c(.tr_plain("RDS combine (spliced + unspliced)"),
                                .tr_plain("Matrix Market (MTX + barcodes + features)"),
                                .tr_plain("Loom / H5AD (velocyto, scVelo)"))),
                  selected = "rds"),

    conditionalPanel(
      condition = "input.velocity_import_mode == 'rds'", ns = ns,
      fileInput(ns("velocity_rds_file"), i18n$t("Fichier RDS velocity (liste spliced/unspliced)"),
                accept = c(".rds", ".RDS", ".rda", ".RData"), width = "100%"),
      div(class = "small text-muted mb-2",
           i18n$t("Le RDS doit être une liste nommée avec spliced et unspliced. Noms inattendus bloqués."))
    ),

    conditionalPanel(
      condition = "input.velocity_import_mode == 'loom'", ns = ns,
      # Roadmap SC FUNCTION_TEST M0 : lecteur loom/h5ad (hdf5r, pre-computed).
      # Aucune inference : les couches spliced/unspliced du fichier sont
      # lues telles quelles et passent la meme validation que RDS/MTX.
      fileInput(ns("velocity_loom_file"), i18n$t("Fichier velocity .loom / .h5ad"),
                accept = c(".loom", ".h5ad"), width = "100%"),
      div(class = "small text-muted mb-2",
           i18n$t("Lecture stricte des couches spliced/unspliced (+ ambiguous si present). Aucune inference ; les symboles de gènes dupliqués sont rendus uniques (avertissement affiché)."))
    ),

    conditionalPanel(
      condition = "input.velocity_import_mode == 'mtx'", ns = ns,
      h6(i18n$t("Matrice spliced (MTX)"), style = "font-weight:bold;"),
      fileInput(ns("velocity_mtx_spliced"), i18n$t("Fichier .mtx (spliced)"), accept = c(".mtx", ".gz"), width = "100%"),
      fileInput(ns("velocity_mtx_spliced_barcodes"), i18n$t("Barcodes spliced"), accept = c(".tsv", ".txt", ".gz"), width = "100%"),
      fileInput(ns("velocity_mtx_spliced_features"), i18n$t("Features/genes spliced"), accept = c(".tsv", ".txt", ".gz"), width = "100%"),
      h6(i18n$t("Matrice unspliced (MTX)"), style = "font-weight:bold;"),
      fileInput(ns("velocity_mtx_unspliced"), i18n$t("Fichier .mtx (unspliced)"), accept = c(".mtx", ".gz"), width = "100%"),
      fileInput(ns("velocity_mtx_unspliced_barcodes"), i18n$t("Barcodes unspliced"), accept = c(".tsv", ".txt", ".gz"), width = "100%"),
      fileInput(ns("velocity_mtx_unspliced_features"), i18n$t("Features/genes unspliced"), accept = c(".tsv", ".txt", ".gz"), width = "100%"),
      h6(i18n$t("Matrice ambiguous (optionnel)"), style = "font-weight:bold;"),
      fileInput(ns("velocity_mtx_ambiguous"), i18n$t("Fichier .mtx (ambiguous)"), accept = c(".mtx", ".gz"), width = "100%"),
      fileInput(ns("velocity_mtx_ambiguous_barcodes"), i18n$t("Barcodes ambiguous"), accept = c(".tsv", ".txt", ".gz"), width = "100%"),
      fileInput(ns("velocity_mtx_ambiguous_features"), i18n$t("Features/genes ambiguous"), accept = c(".tsv", ".txt", ".gz"), width = "100%"),
      numericInput(ns("velocity_feature_column"),
                    i18n$t("Colonne d'identifiants dans features.tsv"),
                    value = 1, min = 1, step = 1, width = "100%"),
      div(class = "small text-muted mb-2",
           HTML(paste(i18n$t("Les fichiers 10x contiennent souvent gene_id (colonne 1) ET symbole "),
                 i18n$t("(colonne 2). La colonne choisie doit correspondre aux rownames de "),
                 i18n$t("l'objet Seurat — la premiere colonne n'est jamais supposee correcte."))))
    ),

    hr(),
    selectInput(
      ns("velocity_orientation"),
      i18n$t("Orientation des matrices"),
      choices = setNames(c("auto_strict", "genes_x_cells", "cells_x_genes"),
        c(.tr_plain("Détection automatique stricte"),
          .tr_plain("Genes x cellules"),
          .tr_plain("Cellules x genes"))),
      selected = "auto_strict"
    ),
    checkboxInput(
      ns("velocity_strip_cell_suffix"),
      i18n$t("Retirer les suffixes de barcodes (ex. -1, -2)"),
      value = FALSE
    ),
    checkboxInput(
      ns("velocity_strip_gene_version"),
      i18n$t("Retirer les suffixes de version Ensembl (ex. .1, .2)"),
      value = FALSE
    ),
    checkboxInput(
      ns("velocity_allow_low_overlap"),
      i18n$t("Forcer l'alignement malgre un recouvrement < 80%"),
      value = FALSE
    ),
    div(
      class = "small text-muted",
      i18n$t("Les corrections d'identifiants sont desactivees par defaut. "),
      " ",
      i18n$t("Une collision bloque la validation.")
    ),
    hr(),
    actionButton(ns("velocity_validate"), i18n$t("Valider les matrices velocity"),
                 class = "btn-primary w-100", icon = icon("check")),
    div(class = "small text-muted mt-1", textOutput(ns("velocity_status"))),
    hr(),
    selectInput(ns("velocity_gene"), i18n$t("Gene pour phase portrait"), choices = character(0)),
    selectInput(ns("velocity_reduction"), i18n$t("Reduction pour vecteurs"), choices = c("umap", "pca")),
    downloadButton(ns("dl_velocity_provenance"), i18n$t("Exporter provenance (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_velocity_summary"), i18n$t("Exporter resume validation (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_velocity_mapping"), i18n$t("Exporter alignement cellules/genes (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_velocity_result_rds"), i18n$t("Exporter resultat valide (RDS)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_velocity_phase_png"), i18n$t("Exporter phase portrait (PNG)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_velocity_phase_pdf"), i18n$t("Exporter phase portrait (PDF)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_velocity_embedding_png"), i18n$t("Exporter embedding (PNG)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_velocity_embedding_pdf"), i18n$t("Exporter embedding (PDF)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_velocity_cell_vectors"), i18n$t("Exporter vecteurs par cellule (CSV)"), class = "btn-sm btn-info w-100 mt-1")
  )
}

mod_sc_velocity_output_ui <- function(id) {
  ns <- NS(id)
    card(
    full_screen = TRUE,
    card_header(i18n$t("RNA Velocity — Phase portrait & vecteurs pre-calcules")),
    navset_tab(
      nav_panel(i18n$t("Phase portrait"),
                plotOutput(ns("velocity_phase_plot"), height = "500px")),
      nav_panel(i18n$t("Vecteurs UMAP"),
                plotOutput(ns("velocity_embedding_plot"), height = "550px"),
                div(class = "small text-muted", textOutput(ns("velocity_vector_status")))),
      nav_panel(i18n$t("Couverture vecteurs"),
                plotOutput(ns("velocity_coverage_plot"), height = "450px")),
      nav_panel(i18n$t("QC alignement"),
                plotOutput(ns("velocity_qc_plot"), height = "450px"))
    )
  )
}

mod_sc_velocity_server <- function(id, global_data, shared_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    # ── i18n proxy ──────────────────────────────────────────────────────────
    .tr <- function(key) {
      tr <- isolate(global_data$i18n)
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # ── helpers contrat Stage 8 ────────────────────────────────────────────
    # Libelle francais d'un etat de validite (fallback = code d'etat).
    .status_label <- function(st) {
      lab <- velocity_status_labels()
      if (is.character(st) && length(st) == 1L && st %in% names(lab)) lab[[st]] else st
    }

    # Fichiers sources declares (NOMS ORIGINAUX uniquement — jamais les
    # chemins locaux complets) + options de lecture, pour input_summary.
    .collect_input_files <- function(mode) {
      if (identical(mode, "rds")) {
        if (is.null(input$velocity_rds_file)) return(list())
        return(list(rds = input$velocity_rds_file$name))
      }
      if (identical(mode, "loom")) {
        if (is.null(input$velocity_loom_file)) return(list())
        return(list(loom = input$velocity_loom_file$name))
      }
      files <- list()
      for (slot in c("velocity_mtx_spliced", "velocity_mtx_spliced_barcodes",
                     "velocity_mtx_spliced_features", "velocity_mtx_unspliced",
                     "velocity_mtx_unspliced_barcodes", "velocity_mtx_unspliced_features",
                     "velocity_mtx_ambiguous", "velocity_mtx_ambiguous_barcodes",
                     "velocity_mtx_ambiguous_features")) {
        fi <- input[[slot]]
        if (!is.null(fi)) files[[slot]] <- fi$name
      }
      files$feature_column <- as.character(input$velocity_feature_column %||% 1L)
      files
    }

    # Plot d'erreur standardise (message de validation exploitable).
    .error_plot <- function(e) {
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = conditionMessage(e), size = 5, colour = "red") +
        ggplot2::theme_void()
    }

    # Coordonnees de base pour la vue embedding : embedding RDS partiel
    # prioritaire, sinon reduction Seurat restreinte aux cellules velocity
    # validees. NULL si introuvables (le demandeur affiche alors un message
    # exploitable — jamais de coordonnees substituees).
    .base_coords_for <- function(red) {
      if (!is.null(velocity_state$partial_embedding)) {
        return(velocity_state$partial_embedding)
      }
      obj <- global_data$sc_obj
      if (is.null(obj) || is.null(velocity_state$result) ||
          !red %in% names(obj@reductions)) {
        return(NULL)
      }
      emb <- Seurat::Embeddings(obj, reduction = red)
      common_cells <- intersect(velocity_state$result$cell_names, rownames(emb))
      if (!length(common_cells)) return(NULL)
      emb[common_cells, seq_len(min(2L, ncol(emb))), drop = FALSE]
    }



    velocity_state <- reactiveValues(
      inputs = NULL,
      validated = NULL,
      result = NULL,
      alignment = NULL,
      umap_vectors = NULL,
      partial_embedding = NULL,
      embedding_alignment = NULL,
      vector_validation = NULL,
      plot_cache = list(),
      export_cache = list(),
      object_fingerprint = NULL
    )

    observeEvent(global_data$language, {
      updateRadioButtons(session, "velocity_import_mode",
        label = .tr("Source d'import"),
        choices = setNames(
          c("rds", "mtx", "loom"),
          c(.tr("RDS combine (spliced + unspliced)"), .tr("Matrix Market (MTX + barcodes + features)"),
            .tr("Loom / H5AD (velocyto, scVelo)"))
        ),
        selected = isolate(input$velocity_import_mode) %||% "rds"
      )
      updateSelectInput(session, "velocity_orientation",
        label = .tr("Orientation des matrices"),
        choices = setNames(
          c("auto_strict", "genes_x_cells", "cells_x_genes"),
          c(.tr("Détection automatique stricte"), .tr("Genes x cellules"), .tr("Cellules x genes"))
        ),
        selected = isolate(input$velocity_orientation) %||% "auto_strict"
      )
      updateCheckboxInput(session, "velocity_strip_cell_suffix", label = .tr("Retirer les suffixes de barcodes (ex. -1, -2)"))
      updateCheckboxInput(session, "velocity_strip_gene_version", label = .tr("Retirer les suffixes de version Ensembl (ex. .1, .2)"))
      updateCheckboxInput(session, "velocity_allow_low_overlap", label = .tr("Forcer l'alignement malgre un recouvrement < 80%"))
      updateActionButton(session, "velocity_validate", label = .tr("Valider les matrices velocity"))
      updateSelectInput(session, "velocity_gene", label = .tr("Gene pour phase portrait"))
      updateSelectInput(session, "velocity_reduction", label = .tr("Reduction pour vecteurs"))
    }, ignoreInit = TRUE)
    velocity_status_rv <- reactiveVal(.tr("En attente d'import velocity..."))

    output$velocity_status <- renderText({ velocity_status_rv() })
    output$velocity_vector_status <- renderText({
      parts <- character(0)
      res <- velocity_state$result
      if (!is.null(res) && !is.null(res$status)) {
        parts <- c(parts, paste0("Statut : ", .status_label(res$status)))
      }
      ali <- velocity_state$embedding_alignment
      if (!is.null(ali)) {
        parts <- c(parts, sprintf(
          "Coordonnees UMAP : %d/%d cellules couvertes (%d manquantes).",
          ali$n_embedding_matched, ali$n_velocity_cells, ali$n_embedding_missing
        ))
      }
      if (!is.null(velocity_state$umap_vectors)) {
        parts <- c(parts, sprintf(
          "%d vecteurs valides (%s).",
          nrow(velocity_state$umap_vectors),
          velocity_state$vector_validation$field %||% "?"
        ))
      } else {
        parts <- c(parts,
          "Aucun vecteur velocity pre-calcule valide — fleches desactivees.")
      }
      paste(parts, collapse = " ")
    })

    # Keep reduction choices in sync with Seurat object
    observeEvent(global_data$sc_obj, {
      obj <- global_data$sc_obj
      if (is.null(obj)) {
        updateSelectInput(session, "velocity_reduction", choices = character(0))
        return()
      }
      reds <- names(obj@reductions)
      if (!length(reds)) reds <- c("umap", "pca")
      updateSelectInput(session, "velocity_reduction",
                        choices = reds,
                        selected = if ("umap" %in% reds) "umap" else reds[1])
    }, ignoreInit = TRUE)

    # Reset when Seurat object changes (BUG 9) — no velocity state may
    # survive from a previous Seurat object.
    observeEvent(global_data$sc_obj, {
      velocity_state$inputs <- NULL
      velocity_state$validated <- NULL
      velocity_state$result <- NULL
      velocity_state$alignment <- NULL
      velocity_state$umap_vectors <- NULL
      velocity_state$partial_embedding <- NULL
      velocity_state$embedding_alignment <- NULL
      velocity_state$vector_validation <- NULL
      velocity_state$plot_cache <- list()
      velocity_state$export_cache <- list()
      velocity_state$object_fingerprint <- NULL

      velocity_status_rv(
        .tr("Objet Seurat modifie : reimportez et validez les donnees velocity.")
      )

      updateSelectInput(
        session,
        "velocity_gene",
        choices = character(0),
        selected = character(0)
      )

      updateCheckboxInput(
        session,
        "velocity_strip_cell_suffix",
        value = FALSE
      )

      updateCheckboxInput(
        session,
        "velocity_strip_gene_version",
        value = FALSE
      )

      updateCheckboxInput(
        session,
        "velocity_allow_low_overlap",
        value = FALSE
      )

      updateSelectInput(
        session,
        "velocity_orientation",
        selected = "auto_strict"
      )
    }, ignoreInit = TRUE)

    # Helper to check fingerprint before plot/export — etat derive
    # stale_against_current_seurat_object (contrat Stage 8).
    .check_fingerprint <- function() {
      req(global_data$sc_obj)
      req(velocity_state$object_fingerprint)
      current_fp <- velocity_object_fingerprint(global_data$sc_obj)
      if (!identical(velocity_state$object_fingerprint, current_fp)) {
        stale_msg <- .status_label("stale_against_current_seurat_object")
        showNotification(stale_msg, type = "error", duration = 8)
        shiny::validate(shiny::need(FALSE, stale_msg))
      }
      invisible(TRUE)
    }

    # Validation button (BUG 1-8, 10)
    observeEvent(input$velocity_validate, {
      req(global_data$sc_obj)

      tryCatch({
        # CHRYSALIS 2E : garde d'entree explicite — le req() ci-dessus laisse
        # l'observateur silencieux si l'objet est absent/non-Seurat ; l'assert
        # produit desormais un message FR clair dans le canal d'erreur.
        assert_seurat(global_data$sc_obj, context = "velocity")

        # 1. Require Seurat object already ensured by req
        seurat_cells <- colnames(global_data$sc_obj)
        seurat_genes <- rownames(global_data$sc_obj)

        # 2. Read selected RDS/MTX inputs
        spliced <- NULL
        unspliced <- NULL
        ambiguous <- NULL
        rds_meta <- list()
        vector_field <- NULL
        vector_matrix <- NULL

        mode <- input$velocity_import_mode %||% "rds"

        if (identical(mode, "rds")) {
          req(input$velocity_rds_file)
          rds_path <- input$velocity_rds_file$datapath
          # read_velocity_rds() enforces the whitelist AND validates metadata
          velocity_input <- read_velocity_rds(rds_path)
          spliced <- velocity_input$spliced
          unspliced <- velocity_input$unspliced
          ambiguous <- velocity_input$ambiguous %||% NULL
          rds_meta <- velocity_input
          # BUG 3/BUG 8: ONLY these fields are dx/dy vectors in embedding
          # space. umap_embedding is coordinates and is NEVER routed here.
          for (f in c("umap_velocity", "embedding_velocity", "umap_vectors", "vectors")) {
            if (!is.null(velocity_input[[f]])) {
              vector_field <- f
              vector_matrix <- velocity_input[[f]]
              break
            }
          }
        } else if (identical(mode, "loom")) {
          # Roadmap SC FUNCTION_TEST M0 : lecture .loom (velocyto) / .h5ad
          # (scVelo, layout old-anndata). Meme chaine de validation que RDS :
          # validate_velocity_matrices() s'applique tel quel plus bas.
          req(input$velocity_loom_file)
          # l'upload anonymise l'extension -> dispatcher sur le nom original
          lname <- input$velocity_loom_file$name %||% ""
          velocity_input <- if (grepl("\\.h5ad$", lname, ignore.case = TRUE)) {
            read_velocity_h5ad(input$velocity_loom_file$datapath)
          } else {
            read_velocity_loom(input$velocity_loom_file$datapath)
          }
          spliced <- velocity_input$spliced
          unspliced <- velocity_input$unspliced
          ambiguous <- velocity_input$ambiguous %||% NULL
          rds_meta <- velocity_input
          for (f in c("umap_velocity", "embedding_velocity", "umap_vectors", "vectors")) {
            if (!is.null(velocity_input[[f]])) {
              vector_field <- f
              vector_matrix <- velocity_input[[f]]
              break
            }
          }
        } else {
          # MTX mode: require both spliced and unspliced triplets
          req(input$velocity_mtx_spliced, input$velocity_mtx_spliced_barcodes, input$velocity_mtx_spliced_features,
              input$velocity_mtx_unspliced, input$velocity_mtx_unspliced_barcodes, input$velocity_mtx_unspliced_features)
          fc <- input$velocity_feature_column %||% 1L
          spliced <- read_velocity_mtx(
            matrix_path = input$velocity_mtx_spliced$datapath,
            barcode_path = input$velocity_mtx_spliced_barcodes$datapath,
            feature_path = input$velocity_mtx_spliced_features$datapath,
            feature_column = fc
          )
          unspliced <- read_velocity_mtx(
            matrix_path = input$velocity_mtx_unspliced$datapath,
            barcode_path = input$velocity_mtx_unspliced_barcodes$datapath,
            feature_path = input$velocity_mtx_unspliced_features$datapath,
            feature_column = fc
          )
          # optional ambiguous triplet (only when all three files provided)
          if (!is.null(input$velocity_mtx_ambiguous) &&
              !is.null(input$velocity_mtx_ambiguous_barcodes) &&
              !is.null(input$velocity_mtx_ambiguous_features)) {
            ambiguous <- read_velocity_mtx(
              matrix_path = input$velocity_mtx_ambiguous$datapath,
              barcode_path = input$velocity_mtx_ambiguous_barcodes$datapath,
              feature_path = input$velocity_mtx_ambiguous_features$datapath,
              feature_column = fc
            )
          }
        }

        # 3. Read orientation, 4. suffix/version, 5. low overlap confirmation (BUG 5/6)
        orientation <- input$velocity_orientation %||% "auto_strict"
        strip_cell_suffix <- isTRUE(input$velocity_strip_cell_suffix)
        strip_gene_version <- isTRUE(input$velocity_strip_gene_version)
        allow_low_overlap <- isTRUE(input$velocity_allow_low_overlap)

        # 6. Validate matrices, 7. Align by barcode and gene ID.
        # Normalization applies to BOTH sides (velocity + Seurat barcodes).
        validated <- validate_velocity_matrices(
          spliced = spliced,
          unspliced = unspliced,
          ambiguous = ambiguous,
          seurat_cells = seurat_cells,
          seurat_genes = seurat_genes,
          orientation = orientation,
          strip_cell_suffix = strip_cell_suffix,
          strip_gene_version = strip_gene_version,
          allow_low_overlap = allow_low_overlap
        )

        # Zero aligned cells makes every downstream view (phase portrait,
        # embedding, export) empty without any visible explanation.
        if (validated$n_cells_matched == 0L) {
          stop(errorCondition(
            paste0(
              "Aucune cellule alignee entre les matrices velocity et ",
              "l'objet Seurat courant. Verifiez que l'objet charge ",
              "correspond au meme run Cell Ranger et activez le retrait ",
              "des suffixes (-1) si necessaire."
            ),
            class = "sc_velocity_error"
          ))
        }

        # BUG 10: preserve RDS metadata on the validated result (never in
        # Seurat meta.data — this stays module-local).
        validated$velocity_source <- rds_meta$velocity_source %||% "unknown"
        validated$velocity_method <- rds_meta$velocity_method %||% "precomputed"
        validated$input_orientation <- rds_meta$orientation %||% NA_character_
        validated$embedding_reduction <- rds_meta$embedding_reduction %||%
          rds_meta$stored_reduction %||%
          rds_meta$reduction %||% NA_character_
        validated$clusters <- rds_meta$clusters %||% NULL
        validated$umap_embedding <- rds_meta$umap_embedding %||% NULL

        # 8. Store validated result (structure plate, compatibilite)
        velocity_state$validated <- validated
        velocity_state$alignment <- list(cells = validated$cell_names, genes = validated$gene_names)

        # Reset per-import derived state before repopulating
        velocity_state$partial_embedding <- NULL
        velocity_state$embedding_alignment <- NULL
        velocity_state$vector_validation <- NULL
        velocity_state$umap_vectors <- NULL

        sel_red <- input$velocity_reduction %||% "umap"
        orchestration_warnings <- character(0)

        # BUG 3/BUG 4: partial coordinate display via align_velocity_embedding()
        if (!is.null(validated$umap_embedding)) {
          ali <- tryCatch(
            align_velocity_embedding(
              embedding = validated$umap_embedding,
              cell_names = validated$cell_names,
              selected_reduction = sel_red,
              stored_reduction = validated$embedding_reduction
            ),
            error = function(e) {
              orchestration_warnings <<- c(orchestration_warnings,
                paste0("Embedding UMAP ignore : ", conditionMessage(e)))
              showNotification(paste("Embedding UMAP ignore :", conditionMessage(e)),
                               type = "warning", duration = 8)
              NULL
            }
          )
          if (!is.null(ali)) {
            velocity_state$partial_embedding <- ali$embedding
            velocity_state$embedding_alignment <- ali
            validated$embedding_alignment <- ali
          }
        }

        # BUG 8: strict vector validation — no arrows on any failure
        if (!is.null(vector_matrix)) {
          vec_stored_red <- validated$embedding_reduction
          if (!identical(vector_field, "umap_velocity") &&
              !identical(vector_field, "embedding_velocity") &&
              !identical(vector_field, "umap_vectors") &&
              !identical(vector_field, "vectors")) {
            vector_matrix <- NULL
          }
          if (!is.null(vector_matrix)) {
            tryCatch({
              velocity_state$umap_vectors <- validate_precomputed_velocity_vectors(
                vectors = vector_matrix,
                cell_names = validated$cell_names,
                selected_reduction = sel_red,
                stored_reduction = vec_stored_red
              )
              velocity_state$vector_validation <- list(field = vector_field, ok = TRUE)
            }, error = function(e) {
              velocity_state$umap_vectors <- NULL
              velocity_state$vector_validation <- list(field = vector_field, ok = FALSE,
                                                       reason = conditionMessage(e))
              orchestration_warnings <<- c(orchestration_warnings,
                paste0("Vecteurs velocity rejetes : ", conditionMessage(e)))
              showNotification(paste("Vecteurs velocity refuses :", conditionMessage(e)),
                               type = "error", duration = 8)
            })
          }
        }

        # 9. Store current object fingerprint
        velocity_state$object_fingerprint <- velocity_object_fingerprint(global_data$sc_obj)

        # Stage 8 (3B-1) : resultat canonique — contrat documente dans
        # R/sc/sc_velocity.R. La provenance est PRODUITE dans l'objet (regle 7
        # AGENTS.md) puis appendee a l'etat partage ; consolidation reservee au
        # rapport (macro-step 4F).
        canonical <- finalize_velocity_result(
          validated           = validated,
          input_mode          = mode,
          input_files         = .collect_input_files(mode),
          seurat_obj          = global_data$sc_obj,
          assay_used          = NULL,
          requested_reduction = sel_red,
          embedding_alignment = velocity_state$embedding_alignment,
          velocity_vectors    = velocity_state$umap_vectors,
          vector_validation   = velocity_state$vector_validation,
          extra_warnings      = orchestration_warnings,
          analysis_id         = "sc-velocity"
        )
        velocity_state$result <- canonical

        # Stage 17 (4F) : exposition ADDITIVE du resultat canonique au rapport
        # consolide (lecture seule ; velocity_state reste la reference du
        # panel 8 — aucun changement de comportement du panneau).
        shared_rv$velocity_result <- canonical

        provenance_append(shared_rv, canonical$provenance)

        # 10. Populate gene selector
        gene_choices <- rownames(validated$spliced)
        updateSelectInput(session, "velocity_gene", choices = gene_choices, selected = gene_choices[1])

        # 11. French status summary : statut canonique explicite (+ BUG 3
        # message when only coordinates exist). Validite technique uniquement —
        # aucune validite biologique n'est impliquee.
        msg <- sprintf(
          paste0(
            "Validation OK : %d cellules, %d genes. ",
            "Orientation : %s. Alignement cellules : %s. ",
            "Alignement genes : %s."
          ),
          validated$n_cells_matched,
          validated$n_genes_matched,
          validated$orientation,
          validated$cell_match_mode,
          validated$gene_match_mode
        )
        if (is.null(vector_matrix) && !is.null(velocity_state$partial_embedding)) {
          msg <- paste(
            msg,
            "Coordonnees UMAP disponibles, mais aucun vecteur velocity pre-calcule n'est fourni.",
            sep = "\n"
          )
        }
        msg <- paste0("[", .status_label(canonical$status), "]\n", msg)
        velocity_status_rv(msg)
        showNotification(.tr("Validation velocity reussie."), type = "message", duration = 4)

      }, error = function(e) {
        velocity_status_rv(
          paste(.tr("Erreur validation velocity :"), conditionMessage(e))
        )
        showNotification(
          paste(.tr("Erreur validation velocity :"), conditionMessage(e)),
          type = "error",
          duration = 8
        )
      })
    })

    # Phase portrait (Stage 9 : consommateur pur du resultat canonique ;
    # apercu avec cap declare et affiche)
    output$velocity_phase_plot <- renderPlot({
      req(velocity_state$result)
      req(input$velocity_gene)
      .check_fingerprint()
      if (ncol(velocity_state$result$spliced) == 0L) {
        return(ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
            label = "Aucune cellule alignee : revalidez les donnees velocity.",
            size = 5, colour = "red") +
          ggplot2::theme_void())
      }
      tryCatch({
        plot_velocity_phase_portrait(
          velocity_state$result, input$velocity_gene,
          data_scope = "preview", seurat_obj = global_data$sc_obj
        )
      }, error = function(e) .error_plot(e))
    })

    # Embedding plot (Stage 9) : les fleches proviennent UNIQUEMENT des
    # vecteurs valides embarques dans le resultat canonique ; coordonnees
    # RDS partielles prioritaires, sinon reduction Seurat. Reduction absente
    # => message de validation exploitable, jamais de substitution.
    output$velocity_embedding_plot <- renderPlot({
      req(velocity_state$result)
      .check_fingerprint()
      red <- input$velocity_reduction %||% "umap"

      base_coords <- .base_coords_for(red)
      if (is.null(base_coords)) {
        shiny::validate(shiny::need(
          FALSE,
          sprintf(paste0("Reduction '%s' introuvable sur l'objet Seurat ou ",
                         "aucune cellule velocity dans l'embedding : calculez ",
                         "la reduction ou choisissez-en une autre."), red)
        ))
      }

      tryCatch(
        plot_velocity_embedding(
          velocity_state$result, base_coords,
          selected_reduction = red, data_scope = "preview",
          seurat_obj = global_data$sc_obj
        ),
        error = function(e) .error_plot(e)
      )
    })

    # Vue densite/couverture des vecteurs valides (Stage 9)
    output$velocity_coverage_plot <- renderPlot({
      req(velocity_state$result)
      .check_fingerprint()
      tryCatch(
        plot_velocity_coverage(velocity_state$result,
                               seurat_obj = global_data$sc_obj),
        error = function(e) .error_plot(e)
      )
    })

    # Vue QC d'alignement (Stage 9)
    output$velocity_qc_plot <- renderPlot({
      req(velocity_state$result)
      .check_fingerprint()
      tryCatch(
        plot_velocity_alignment_qc(velocity_state$result,
                                   seurat_obj = global_data$sc_obj),
        error = function(e) .error_plot(e)
      )
    })

    # Provenance export
    output$dl_velocity_provenance <- downloadHandler(
      filename = function() paste0("velocity_provenance_", format(Sys.Date(), "%Y-%m-%d"), ".csv"),
      content = function(file) {
        req(velocity_state$result)
        req(input$velocity_gene)
        .check_fingerprint()
        df <- build_velocity_provenance_export(
          velocity_result = velocity_state$result,
          gene = input$velocity_gene,
          selected_reduction = input$velocity_reduction %||% NA_character_
        )
        utils::write.csv(df, file, row.names = FALSE)
      }
    )

    # ── Exports contrat Stage 8 ─────────────────────────────────────────────
    # Resume de validation (1 ligne : analysis_id, statut, reglages, compteurs,
    # avertissements, versions).
    output$dl_velocity_summary <- downloadHandler(
      filename = function() paste0("velocity_validation_summary_", format(Sys.Date(), "%Y-%m-%d"), ".csv"),
      content = function(file) {
        req(velocity_state$result)
        .check_fingerprint()
        df <- build_velocity_validation_summary(velocity_state$result)
        utils::write.csv(df, file, row.names = FALSE)
      }
    )

    # Table d'alignement cellules/genes produite a la validation.
    output$dl_velocity_mapping <- downloadHandler(
      filename = function() paste0("velocity_alignment_mapping_", format(Sys.Date(), "%Y-%m-%d"), ".csv"),
      content = function(file) {
        req(velocity_state$result)
        .check_fingerprint()
        df <- build_velocity_alignment_mapping(velocity_state$result)
        utils::write.csv(df, file, row.names = FALSE)
      }
    )

    # Resultat canonique complet (RDS) — identite, statut, reglages et
    # avertissements embarques dans l'objet ; analysis_id dans le nom de fichier.
    output$dl_velocity_result_rds <- downloadHandler(
      filename = function() {
        aid <- velocity_state$result$analysis_id %||% "sc-velocity"
        paste0("velocity_result_", aid, "_", format(Sys.Date(), "%Y-%m-%d"), ".rds")
      },
      content = function(file) {
        req(velocity_state$result)
        .check_fingerprint()
        saveRDS(velocity_state$result, file)
      }
    )

    # ── Exports figures Stage 9 ─────────────────────────────────────────────
    # Figures PNG/PDF en portee COMPLETE ("full") — la portee est ecrite dans
    # le sous-titre de la figure ; nom de fichier trace par analysis_id.
    output$dl_velocity_phase_png <- downloadHandler(
      filename = function() velocity_export_filename(velocity_state$result, "velocity_phase_portrait", "png"),
      content = function(file) {
        req(velocity_state$result); req(input$velocity_gene)
        .check_fingerprint()
        p <- plot_velocity_phase_portrait(
          velocity_state$result, input$velocity_gene,
          data_scope = "full", seurat_obj = global_data$sc_obj
        )
        ts_export_plot(file, p, width = 8, height = 6, dpi = 300)
      }
    )

    output$dl_velocity_phase_pdf <- downloadHandler(
      filename = function() velocity_export_filename(velocity_state$result, "velocity_phase_portrait", "pdf"),
      content = function(file) {
        req(velocity_state$result); req(input$velocity_gene)
        .check_fingerprint()
        p <- plot_velocity_phase_portrait(
          velocity_state$result, input$velocity_gene,
          data_scope = "full", seurat_obj = global_data$sc_obj
        )
        ts_export_plot(file, p, width = 8, height = 6)
      }
    )

    # Figure embedding en portee COMPLETE — partagee PNG/PDF. Coordonnees
    # introuvables => notification + annulation explicite (jamais de
    # substitution silencieuse).
    .export_embedding_figure <- function(file) {
      red <- input$velocity_reduction %||% "umap"
      coords <- .base_coords_for(red)
      if (is.null(coords)) {
        showNotification(
          sprintf(paste0("Reduction '%s' introuvable ou aucune cellule ",
                         "velocity dans l'embedding : export annule."), red),
          type = "error", duration = 8
        )
        req(FALSE)
      }
      p <- plot_velocity_embedding(
        velocity_state$result, coords,
        selected_reduction = red, data_scope = "full",
        seurat_obj = global_data$sc_obj
      )
      ts_export_plot(file, p, width = 8, height = 6, dpi = 300)
    }

    output$dl_velocity_embedding_png <- downloadHandler(
      filename = function() velocity_export_filename(velocity_state$result, "velocity_embedding", "png"),
      content = function(file) {
        req(velocity_state$result)
        .check_fingerprint()
        .export_embedding_figure(file)
      }
    )

    output$dl_velocity_embedding_pdf <- downloadHandler(
      filename = function() velocity_export_filename(velocity_state$result, "velocity_embedding", "pdf"),
      content = function(file) {
        req(velocity_state$result)
        .check_fingerprint()
        .export_embedding_figure(file)
      }
    )

    # Vecteurs/alignement par cellule (CSV) — depuis le resultat canonique.
    output$dl_velocity_cell_vectors <- downloadHandler(
      filename = function() velocity_export_filename(velocity_state$result, "velocity_cell_vectors", "csv"),
      content = function(file) {
        req(velocity_state$result)
        .check_fingerprint()
        df <- build_velocity_cell_vectors_export(velocity_state$result)
        utils::write.csv(df, file, row.names = FALSE)
      }
    )

    # Expose state for tests (optional)
    return(velocity_state)
  })
}

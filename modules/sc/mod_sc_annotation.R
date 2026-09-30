# mod_sc_annotation.R  —  Child 2: SingleR
# Step-3.6: integrated user's working ENSG→Symbol fix (extended for mouse)
#   Standard path now always normalizes first, then auto-remaps if no gene overlap.
# Step-3.7: "Afficher les étiquettes" toggle for the UMAP plot (readability on
#   dense datasets with many fine-grained cell types).

.annot_is_big    <- function(obj) ncol(obj) > 100000L
.annot_is_ondisk <- function(obj) {
  # Step-3.7A: now delegates to the shared sc_backend_status() helper
  # (helpers_sc_bpcells.R) instead of its own inline IterableMatrix/
  # DelayedMatrix check, so every module agrees on what "on-disk" means.
  tryCatch(sc_backend_status(obj) == "disk", error = function(e) FALSE)
}

.load_ref <- function(code) {
  switch(code,
    hpca      = celldex::HumanPrimaryCellAtlasData(),
    blueprint = celldex::BlueprintEncodeData(),
    immgen    = celldex::ImmGenData(),
    dice      = celldex::DatabaseImmuneCellExpressionData(),
    stop(errorCondition(paste("Unknown reference:", code), class = "sc_annotation_error"))
  )
}

.refcode_to_organism <- function(code) if (code == "immgen") "mouse" else "human"

.run_singler_safe <- function(obj, refcode, labellevel, maxcells = 50000L) {
  label_col <- if (labellevel == "main") "label.main" else "label.fine"

  # Step-3.8A: organism is now detected from the actual gene IDs (Ensembl
  # prefix) rather than inferred solely from the chosen reference — fixes
  # systematic mismatches on mouse data (ENSMUSG... tested against org.Hs.eg.db
  # whenever a non-ImmGen reference is picked). Detected value wins; a warning
  # is raised if it conflicts with what the reference choice implies.
  ref_organism <- .refcode_to_organism(refcode)
  detected     <- detect_organism_from_ids(rownames(obj))
  organism     <- if (!is.na(detected)) detected else ref_organism

  if (!is.na(detected) && !identical(detected, ref_organism)) {
    warning(sprintf(
      paste0("Organisme d\u00e9tect\u00e9 depuis les identifiants g\u00e8nes ('%s') diff\u00e8re de la ",
             "r\u00e9f\u00e9rence choisie ('%s', organisme attendu '%s'). Conversion ID effectu\u00e9e ",
             "avec '%s' \u2014 v\u00e9rifiez que la r\u00e9f\u00e9rence SingleR correspond \u00e0 votre esp\u00e8ce."),
      detected, refcode, ref_organism, organism))
  }

  # ── Standard path ──────────────────────────────────────────────────────────
  if (!.annot_is_big(obj) && !.annot_is_ondisk(obj)) {
    ref <- .load_ref(refcode)

    # Always normalize to ensure logcounts (user-validated fix)
    DefaultAssay(obj) <- "RNA"
    obj <- NormalizeData(obj, verbose = FALSE)
    sce <- as.SingleCellExperiment(obj)

    test_ids <- rownames(sce)
    ref_ids  <- rownames(ref)

    # Auto-remap ENSG → Symbol when no overlap with reference
    if (!any(test_ids %in% ref_ids)) {
      orgdb_pkg <- if (organism == "human") "org.Hs.eg.db" else "org.Mm.eg.db"

      if (requireNamespace("AnnotationDbi", quietly = TRUE) &&
          requireNamespace(orgdb_pkg,       quietly = TRUE)) {

        orgdb <- getExportedValue(orgdb_pkg, orgdb_pkg)
        sym <- tryCatch(
          AnnotationDbi::mapIds(orgdb, keys = test_ids, keytype = "ENSEMBL",
                                column = "SYMBOL", multiVals = "first"),
          error = function(e) NULL
        )

        if (!is.null(sym)) {
          keep <- !is.na(sym) & nchar(sym) > 0
          if (sum(keep) > 0) {
            # D2: subset by `keep` FIRST, then rename — renamer d'abord
            # transformerait chaque NA non-mappe en chaine littérale "NA" que
            # le garde !duplicated() ci-dessous collapserait en UNE seule ligne
            # bogus. Discipline portée depuis map_ensembl_matrix_to_symbol()
            # (R/sc/sc_helpers.R) ; testée par .annot_subset_rename() ci-dessous.
            mat <- .annot_subset_rename(
              SummarizedExperiment::assay(sce, "logcounts"), sym
            )
            sce          <- SingleCellExperiment::SingleCellExperiment(
              assays  = list(logcounts = mat),
              colData = SummarizedExperiment::colData(sce)
            )
            warning(sprintf(
              "SingleR: %d IDs ENSEMBL remappés en Symboles (%s) avant annotation.",
              sum(keep), organism
            ))
          }
        }
      } else {
        # No AnnotationDbi / orgdb → clear actionable error
        stop(errorCondition(sprintf(
          paste0(
            "Aucun chevauchement entre votre objet (ex: %s) et la référence '%s' (attend des Symboles).\n",
            "Solution : installez %s via BiocManager::install('%s') pour la conversion automatique."
          ),
          head(test_ids, 1), refcode, orgdb_pkg, orgdb_pkg
        ), class = "sc_annotation_error"))
      }
    }

    pred <- SingleR::SingleR(test = sce, ref = ref, labels = ref[[label_col]])
    return(list(labels = pred$labels, method = "full", organism = organism))
  }

  # ── Cluster-aggregate path (large / on-disk) ───────────────────────────────
  warning("Large dataset: using cluster-level aggregation for SingleR")
  if (!"seurat_clusters" %in% colnames(obj@meta.data))
    stop(errorCondition("Lancez le clustering (Pipeline) avant l'annotation.", class = "sc_annotation_error"))

  ref      <- .load_ref(refcode)
  clusters <- unique(obj$seurat_clusters)

  cluster_profiles <- lapply(setNames(clusters, as.character(clusters)), function(clust) {
    cells <- Cells(obj)[obj$seurat_clusters == clust]
    if (length(cells) > maxcells) cells <- sample(cells, maxcells)
    tmp   <- subset(obj, cells = cells)
    DefaultAssay(tmp) <- "RNA"
    tmp   <- NormalizeData(tmp, verbose = FALSE)
    # Step-3.8A: GetAssayData(slot=) is DEFUNCT in SeuratObject 5.0 (crashed
    # this exact pseudobulk loop on the 1.3M-neurons dataset) -> LayerData().
    Matrix::rowMeans(SeuratObject::LayerData(tmp, assay = "RNA", layer = "data"))
  })
  profile_matrix <- do.call(cbind, cluster_profiles)

  # Step-3.8A: pseudobulk (per-cluster) ENSEMBL -> Symbol remap, organism
  # auto-detected above (detect_organism_from_ids), not just from `refcode`.
  ref_ids <- rownames(ref)
  if (!any(rownames(profile_matrix) %in% ref_ids)) {
    profile_matrix <- tryCatch(
      map_ensembl_matrix_to_symbol(profile_matrix, organism = organism),
      error = function(e) {
        warning(paste("Conversion ENSEMBL -> Symbol (pseudobulk) impossible :", conditionMessage(e)))
        profile_matrix
      })
  }

  # Step-3.8B: fail loud & actionable BEFORE handing off to SingleR::SingleR(),
  # which otherwise errors with an opaque "no common genes between 'test' and
  # 'ref'" that gives the user nothing to act on.
  n_common <- length(intersect(rownames(profile_matrix), ref_ids))
  if (n_common == 0) {
    test_id_type <- tryCatch(detect_gene_id_type(rownames(profile_matrix)), error = function(e) "unknown")
    ref_id_type  <- tryCatch(detect_gene_id_type(ref_ids), error = function(e) "unknown")
    stop(errorCondition(sprintf(paste0(
      "SingleR (pseudobulk) : 0 gene commun entre le test et la reference '%s'.\n",
      "  Organisme detecte (test) : %s\n",
      "  Type d'ID test (%s)      : %s\n",
      "  Type d'ID reference (%s) : %s\n",
      "  Exemples test : %s\n",
      "  Exemples ref  : %s"
    ), refcode, organism %||% "?",
    test_id_type, paste(head(rownames(profile_matrix), 5), collapse = ", "),
    ref_id_type,  paste(head(ref_ids, 5), collapse = ", "),
    paste(head(rownames(profile_matrix), 5), collapse = ", "),
    paste(head(ref_ids, 5), collapse = ", ")), class = "sc_annotation_error"))
  }

  pred           <- SingleR::SingleR(test=profile_matrix, ref=ref, labels=ref[[label_col]])
  cluster_labels <- setNames(as.character(pred$labels), colnames(profile_matrix))
  cell_labels    <- .annot_cluster_labels_to_cells(cluster_labels, obj)
  list(labels = cell_labels, method = "cluster_aggregate", organism = organism)
}

# D1 : les étiquettes par cluster ne portent que les ids de cluster ("0", "1",
# ...) comme noms — les barcodes sont ré-attachés PAR POSITION depuis
# obj$seurat_clusters (colonne meta.data, donc déjà dans l'ordre colnames(obj)).
# Jamais indexé par les noms : as.character() sur un facteur les perd.
.annot_cluster_labels_to_cells <- function(cluster_labels, obj) {
  cell_labels <- cluster_labels[as.character(obj$seurat_clusters)]
  setNames(unname(cell_labels), colnames(obj))
}

# D2 : subset par `keep` AVANT renommage — l'ordre inverse fabrique des noms
# "NA" littéraux que le garde !duplicated() collapse en une ligne unique
# (perte silencieuse de gènes réellement mappés).
.annot_subset_rename <- function(mat, sym) {
  keep <- !is.na(sym) & nchar(sym) > 0
  mat <- mat[keep, , drop = FALSE]
  rownames(mat) <- unname(sym[keep])
  mat
}

# ── UI ────────────────────────────────────────────────────────────────────────

mod_sc_annotation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class="alert alert-light",style="font-size:0.9em;border-left:3px solid #18BC9C;",
        i18n$t("Utilise SingleR pour annoter automatiquement les clusters."),
        tags$br(),
        tags$small(i18n$t("IDs Ensembl (ENSG…) convertis automatiquement si org.Hs.eg.db disponible."))),
    selectInput(ns("ref_singler"),i18n$t("Reference Cellulaire"),
      choices=setNames(c("hpca","blueprint","immgen","dice"),
                       c(.tr_plain("Human Primary Cell Atlas"), .tr_plain("Blueprint Encode"), .tr_plain("ImmGen (Mouse)"), .tr_plain("DICE Immune")))),
    radioButtons(ns("label_level"),i18n$t("Niveau de Detail"),
      choices=setNames(c("main","fine"),
                       c(.tr_plain("Main (General)"), .tr_plain("Fine (Specifique)"))), inline=TRUE),
    actionButton(ns("run_annot"),i18n$t("Annoter avec SingleR"),
                 class="btn-warning w-100",icon=icon("user-tag")),
    hr(),
    # Step-3.7: readability toggle for the UMAP plot (label + repel), independent
    # of re-running SingleR — purely a display setting for output$annot_umap_plot.
    checkboxInput(ns("annot_show_labels"),
                  i18n$t("Afficher les étiquettes de types cellulaires sur l'UMAP"),
                  value = TRUE),
    hr(),
    div(class="small text-muted",textOutput(ns("annot_status")))
  )
}

mod_sc_annotation_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen=TRUE,
    card_header(div(style="display:flex;justify-content:space-between;align-items:center;",
                    h5(i18n$t("Annotation Automatique (SingleR)"),class="mb-0"),
                    downloadButton(ns("dl_annotation"),"Export CSV",class="btn-sm btn-info"))),
    layout_columns(col_widths=c(12),
      card(card_header(i18n$t("UMAP — types cellulaires predits")),
           plotOutput(ns("annot_umap_plot"),height="380px")),
      card(card_header(i18n$t("Table — distribution par cluster")),
           DTOutput(ns("annot_table"))))
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_sc_annotation_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    # ── i18n proxy ──────────────────────────────────────────────────────────
    .tr <- function(key) {
      tr <- isolate(global_data$i18n)
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    observeEvent(global_data$language, {
      updateSelectInput(session, "ref_singler",
        label = .tr("Reference Cellulaire"),
        choices = setNames(
          c("hpca", "blueprint", "immgen", "dice"),
          c(.tr("Human Primary Cell Atlas"), .tr("Blueprint Encode"), .tr("ImmGen (Mouse)"), .tr("DICE Immune"))
        ),
        selected = isolate(input$ref_singler) %||% "hpca"
      )
      updateRadioButtons(session, "label_level",
        label = .tr("Niveau de Detail"),
        choices = setNames(c("main", "fine"), c(.tr("Main (General)"), .tr("Fine (Specifique)"))),
        selected = isolate(input$label_level) %||% "main"
      )
      updateActionButton(session, "run_annot", label = .tr("Annoter avec SingleR"))
      updateCheckboxInput(session, "annot_show_labels", label = .tr("Afficher les étiquettes de types cellulaires sur l'UMAP"))
    }, ignoreInit = TRUE)


    annot_status_rv <- reactiveVal(.tr("En attente de l'annotation..."))

    observeEvent(input$run_annot, ignoreInit=TRUE, {
      req(global_data$sc_obj)
      obj     <- isolate(global_data$sc_obj)
      refcode <- isolate(input$ref_singler)
      lv      <- isolate(input$label_level)

      p <- shiny::Progress$new(); on.exit(p$close())
      p$set(message=.tr("Annotation SingleR..."), value=0.1)

      tryCatch({
        p$set(0.35, .tr("Chargement référence + vérification IDs..."))
        result <- withCallingHandlers(
          .run_singler_safe(obj, refcode, lv),
          warning = function(w) {
            msg <- conditionMessage(w)
            type <- if (grepl("cluster-level|large", msg, ignore.case=TRUE)) "warning"
                    else if (grepl("ENSEMBL|remapp", msg, ignore.case=TRUE)) "message"
                    else "warning"
            showNotification(msg, type=type, duration=7)
            invokeRestart("muffleWarning")
          }
        )

        p$set(0.90, .tr("Mise à jour de l'objet..."))
        col_name        <- paste0("SingleR_", refcode, "_", lv)
        obj[[col_name]] <- result$labels
        Idents(obj)     <- result$labels

        global_data$sc_obj   <- obj
        shared_rv$active_tab <- "tab_viz"

        # Roadmap SC FUNCTION TEST (D1/D2 follow-up) : organisme détecté +
        # référence affichés — un mismatch humain/souris doit être visible
        # sans fouiller les logs.
        annot_status_rv(paste0("✓ [", result$method, "] — ",
                               length(unique(result$labels)), " types cellulaires",
                               " — organisme: ", result$organism %||% "?",
                               " / réf: ", refcode))
        showNotification(paste("✓ Annotation [", result$method, "] —",
                               length(unique(result$labels)), "types",
                               "—", result$organism %||% "?", "/", refcode),
                         type = "message", duration = 5)

      }, error=function(e) {
        annot_status_rv(paste(.tr("Erreur:"), e$message))
        showNotification(paste(.tr_plain("Erreur annotation:"), e$message), type="error", duration=10)
      })
    })

    output$annot_status <- renderText({ annot_status_rv() })

    output$annot_umap_plot <- renderPlot({
      req(global_data$sc_obj)
      obj  <- global_data$sc_obj
      meta <- obj@meta.data
      singler_cols <- grep("^SingleR_", colnames(meta), value=TRUE)
      validate(need(length(singler_cols)>0, .tr("Aucune annotation. Lancez 'Annoter avec SingleR'.")))
      col_use <- tail(singler_cols, 1)
      validate(need("umap" %in% names(obj@reductions), .tr("UMAP non calculé.")))
      show_lbl <- isTRUE(input$annot_show_labels)
      pal_scale <- sc_discrete_scale(shared_rv$sc_palette %||% "default", shared_rv$sc_manual_colors)
      p_annot <- DimPlot(obj, reduction="umap", group.by=col_use, label=show_lbl, repel=show_lbl, pt.size=0.5) +
        labs(title=paste("SingleR:", col_use)) + theme_minimal() +
        theme(plot.title=element_text(face="bold",size=13))
      if (!is.null(pal_scale)) p_annot <- suppressWarnings(p_annot + pal_scale)
      p_annot
    })

    output$annot_table <- renderDT({
      req(global_data$sc_obj)
      obj  <- global_data$sc_obj
      meta <- obj@meta.data
      singler_cols <- grep("^SingleR_", colnames(meta), value=TRUE)
      validate(need(length(singler_cols)>0, "Aucune annotation."))
      col_use <- tail(singler_cols, 1)
      tbl <- as.data.frame(table(Cluster=meta$seurat_clusters, CellType=meta[[col_use]]))
      tbl <- tbl[tbl$Freq > 0, ]
      tbl <- tbl[order(tbl$Cluster, -tbl$Freq), ]
      ts_datatable(tbl, page_length = 15L,
                   filename_base = "sc_annotation_table") %>%
        formatStyle("Freq", background=styleColorBar(range(tbl$Freq),"#18BC9C"),
                    backgroundSize="98% 88%", backgroundRepeat="no-repeat",
                    backgroundPosition="center")
    })

    output$dl_annotation <- downloadHandler(
      filename = function() paste0("annotation_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(global_data$sc_obj)
        meta         <- global_data$sc_obj@meta.data
        singler_cols <- grep("^SingleR_", colnames(meta), value=TRUE)
        validate(need(length(singler_cols)>0, "Aucune annotation."))
        write.csv(data.frame(cell_barcode=rownames(meta),
                             seurat_clusters=meta$seurat_clusters,
                             meta[, singler_cols, drop=FALSE],
                             check.names=FALSE),
                  file, row.names=FALSE)
      }
    )
  })
}

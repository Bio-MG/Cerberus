# modules/import/mod_import_bulk.R - Import RNA Bulk
# v3: ajout du mode "One file per sample" (import multi-fichiers bulk)
#     en plus du mode "Merged matrix" existant (inchangé).
#
# Depends on helpers_io.R (sourced by global.R, not defined there):
# infer_metadata_from_names(), preview_metadata_split(), detect_gene_id_type()

# ── Helpers mode per-sample ───────────────────────────────────────────────────

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || identical(x, "") ||
      (length(x) == 1 && is.na(x))) y else x
}

#' Guess delimiter from filename (.csv → comma, others → tab).
#' @param filename Uploaded filename.
#' @return Character delimiter.

#' Read one bulk sample file (csv/tsv/txt, optionally .gz).
#' @param path Temp path from fileInput.
#' @param filename Original uploaded filename.
#' @return data.frame with trimmed column names.

#' Fraction of numeric-like values in a vector.
#' @param x Vector.
#' @return Numeric in [0,1].

#' Infer sample ID from filename (stem, sanitized).
#' @param filename Uploaded filename.
#' @return Character sample ID.

#' Infer condition from filename stem (heuristic).
#' @param filename Uploaded filename.
#' @return Character or NA_character_.

#' Detect candidate gene-ID and count columns in one file.
#' @param df Raw data.frame.
#' @return Named list: gene_candidates, count_candidates, auto_gene, auto_count.

#' Standardize one file to a feature_id + one sample column.
#' @param df Raw data.frame.
#' @param sample_id Name to give the count column.
#' @param gene_col Gene ID column name.
#' @param count_col Count column name.
#' @param dup_threshold Max fraction of duplicated feature IDs allowed.
#' @return list(ok, status, data, id_type, integer_like)

#' Merge list of per-sample tables by feature_id (full join).
#' @param tbls List of data.frames.
#' @return Merged data.frame.

#' Validate design readiness (min 2 groups, >=2 replicates, sample match).
#' @param counts_mat Gene x sample matrix.
#' @param metadata data.frame with sample_id + condition columns.
#' @return list(ok, de_ok, messages, metadata)

# ─────────────────────────────────────────────────────────────────────────────
# UI
# ─────────────────────────────────────────────────────────────────────────────

mod_import_bulk_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        title = "Import RNA Bulk",
        
        div(class = "alert alert-info", style = "font-size: 0.85rem;",
            bsicons::bs_icon("info-circle"),
            " Importez des données RNA-Seq en Bulk (matrice de counts + métadonnées)."),

        # LOT 4A (V1.x UX): mapping stays in the analysis module (no UI move);
        # remind + native jump to the existing Bulk "0. Mapping IDs" panel.
        div(class = "alert alert-light", style = "font-size:0.78rem;",
            bsicons::bs_icon("info-circle"), " ",
            i18n$t("Identifiants non-symboles (Ensembl, Entrez, sondes Affymetrix) ? Le mapping d'IDs est disponible à l'étape 0 de l'analyse.")),
        actionButton(ns("goto_mapping"), i18n$t("Aller au mapping des IDs"),
                     class = "btn-outline-secondary btn-sm w-100 mb-2", icon = icon("arrow-right")),
        
        # ── Mode selector ──────────────────────────────────────────────────
        radioButtons(
          ns("bulk_import_mode"),
          label    = i18n$t("Mode d'import"),
          choices  = setNames(c("merged_matrix", "per_sample"),
                              c(.tr_plain("Merged matrix (un seul fichier)"), .tr_plain("One file per sample"))),
          selected = "merged_matrix",
          inline   = TRUE
        ),
        
        accordion(
          # ── Panel 1 : counts (mode merged_matrix) ───────────────────────
          accordion_panel(
            "1. Matrice de Counts",
            icon = icon("table"),
            
            conditionalPanel(
              condition = sprintf("input['%s'] === 'merged_matrix'", ns("bulk_import_mode")),
              
              fileInput(ns("counts_file"), "Fichier de Counts (CSV/TSV/TXT)",
                        accept = c(".csv", ".tsv", ".txt", ".xlsx",
                                   ".rda", ".RData", ".rds", ".RDS")),
              
              helpText("Format attendu : Lignes = Gènes, Colonnes = Échantillons. ",
                       "Un filtrage fin (par réplicat) est disponible à l'étape 1 du module d'analyse — ",
                       "le seuil ci-dessous (Options d'Import) n'est qu'un pré-nettoyage grossier."),
              
              radioButtons(ns("counts_format"), i18n$t("Format de la matrice"),
                           choices  = setNames(c("rows", "cols"),
                                              c(.tr_plain("Genes en lignes (standard)"), .tr_plain("Genes en colonnes (transposé)"))),
                           selected = "rows"),
              
              checkboxInput(ns("counts_has_header"),   "La 1ère ligne est un en-tête", value = TRUE),
              checkboxInput(ns("counts_has_rownames"), "La 1ère colonne est le nom des gènes", value = TRUE),
              
              h6("Aperçu de la matrice:", style = "font-weight: bold; margin-top: 10px;"),
              div(style = "max-height: 200px; overflow: auto; border: 1px solid #ddd; padding: 5px;",
                  tableOutput(ns("counts_preview"))),
              
              uiOutput(ns("detected_gene_id_banner")),

              # .rda/.RData "Inspect & Select" — objet = matrice de comptages
              rdata_picker_ui(ns("rdata_picker_bulk_counts"))
            )
          ),
          
          # ── Panel 2 : métadonnées (mode merged_matrix) ──────────────────
          accordion_panel(
            "2. Métadonnées (Optionnel)",
            icon = icon("tags"),
            
            conditionalPanel(
              condition = sprintf("input['%s'] === 'merged_matrix'", ns("bulk_import_mode")),
              
              fileInput(ns("metadata_file"), "Fichier de Métadonnées (CSV/TSV/TXT)",
                        accept = c(".csv", ".tsv", ".txt", ".xlsx",
                                   ".rda", ".RData", ".rds", ".RDS")),
              
              helpText("Format : Lignes = Échantillons, Colonnes = Variables (condition, batch, etc.)."),
              
              checkboxInput(ns("metadata_has_header"),   "La 1ère ligne est un en-tête", value = TRUE),
              checkboxInput(ns("metadata_has_rownames"), "La 1ère colonne est le nom des échantillons", value = TRUE),
              
              h6("Aperçu des métadonnées actives:", style = "font-weight: bold; margin-top: 10px;"),
              div(style = "max-height: 200px; overflow: auto; border: 1px solid #ddd; padding: 5px;",
                  tableOutput(ns("metadata_preview"))),

              # .rda/.RData "Inspect & Select" — objet = métadonnées
              rdata_picker_ui(ns("rdata_picker_bulk_meta")),

              hr(),
              
              div(
                class = "border rounded p-2",
                style = "background:#f8f9fa;",
                h6("Pas de fichier ? Inférer depuis les noms d'échantillons",
                   style = "font-weight: bold; font-size: 0.85em;"),
                helpText(style = "font-size:0.78em;",
                         "Ex: 'Ctrl_1', 'Ctrl_2', 'KO_1', 'KO_2' → colonnes condition + réplicat. ",
                         "Ignoré si un fichier de métadonnées est fourni ci-dessus."),
                
                textInput(ns("infer_delimiter"), "Délimiteur (regex)", value = "[_-]"),
                actionButton(ns("btn_infer_preview"), "👁 Prévisualiser le découpage",
                             class = "btn-outline-secondary btn-sm w-100"),
                
                uiOutput(ns("infer_preview_ui")),
                uiOutput(ns("infer_colnames_ui")),
                
                actionButton(ns("btn_infer_apply"), "✓ Utiliser ces métadonnées inférées",
                             class = "btn-outline-success btn-sm w-100 mt-2")
              )
            )
          ),
          
          # ── Panel 3 : options d'import ───────────────────────────────────
          accordion_panel(
            "3. Options d'Import",
            icon = icon("cogs"),
            
            textInput(ns("project_name"), "Nom du Projet",
                      value = "BulkRNA_Project", placeholder = "Ex: Study_2024"),

            # MD-1 : label optionnel — enregistre aussi le jeu importé dans
            # global_data$bulk_datasets (producteur "import") sans toucher
            # bulk_obj (contrat docs/contracts/BULK_MULTI_CONTRACT.md §6).
            textInput(ns("multi_label"), i18n$t("Label multi-datasets (optionnel)"),
                      placeholder = .tr_plain("ex : GSE123_T2")),
            helpText(i18n$t("Si renseigné, le jeu importé est aussi enregistré sous ce nom pour la comparaison multi-jeux (Bulk > Multi-jeux).")),

            conditionalPanel(
              condition = sprintf("input['%s'] === 'merged_matrix'", ns("bulk_import_mode")),
              numericInput(ns("min_counts"), "Counts minimum par gène (pré-filtre grossier)",
                           value = 10, min = 0, step = 1),
              helpText("Les gènes avec moins de counts seront filtrés dès l'import.")
            )
          ),
          
          # ── Panel 4 : NEW — one file per sample ─────────────────────────
          accordion_panel(
            "4. Import multi-fichiers (one file per sample)",
            icon = icon("copy"), # previous icon("files") = problem
            
            conditionalPanel(
              condition = sprintf("input['%s'] === 'per_sample'", ns("bulk_import_mode")),
              
              fileInput(
                ns("ps_files"),
                "Fichiers bulk (un fichier = un sample)",
                multiple = TRUE,
                accept   = c(".csv", ".tsv", ".txt", ".gz")
              ),
              
              numericInput(
                ns("ps_dup_threshold"),
                "Seuil max. gene IDs dupliqués (fraction)",
                value = 0.05, min = 0, max = 1, step = 0.01
              ),
              
              hr(),
              
              # Résolution ambiguïté colonnes (rendu dynamique)
              uiOutput(ns("ps_resolve_ui")),
              
              h6("📋 Aperçu des fichiers détectés",
                 style = "font-weight:bold; margin-top:8px;"),
              DT::DTOutput(ns("ps_preview_table")),
              
              hr(),
              
              checkboxInput(
                ns("ps_fill_zero"),
                "✅ J'accepte de remplacer les counts manquants (full join) par 0",
                value = FALSE
              ),
              
              h6("📝 Métadonnées auto-générées (éditables)",
                 style = "font-weight:bold; margin-top:8px;"),
              DT::DTOutput(ns("ps_metadata_dt")),
              
              uiOutput(ns("ps_status_banner"))
            )
          )
        ),
        
        hr(),
        # ── Bouton persistant (inchangé) ───────────────────────────────────
        div(
          style = "background:#eef2f5;padding:10px;border-radius:6px;border:1px solid #d6dde1;",
          uiOutput(ns("readiness_banner")),
          div(id = ns("load_button_container"), style = "margin-top:8px;",
              actionButton(ns("btn_load"), "🚀 Charger les Données",
                           class = "btn-success w-100", icon = icon("play"))
          )
        )
      ),
      
      card(
        card_header("Résumé des Données Bulk"),
        layout_columns(
          value_box(title = "Échantillons",
                    value   = textOutput(ns("nb_samples")),
                    showcase = bsicons::bs_icon("grid-3x3"),
                    theme   = "primary"),
          value_box(title = "Gènes",
                    value   = textOutput(ns("nb_genes")),
                    showcase = bsicons::bs_icon("diagram-3"),
                    theme   = "secondary"),
          value_box(title = "Variables Métadonnées",
                    value   = textOutput(ns("nb_metadata_vars")),
                    showcase = bsicons::bs_icon("tags"),
                    theme   = "info"),
          value_box(title = "Statut",
                    value   = textOutput(ns("status_obj")),
                    showcase = bsicons::bs_icon("check-circle"),
                    theme   = "light")
        ),
        card_body(
          h5("Console de Log", class = "text-muted"),
          verbatimTextOutput(ns("console_log"), placeholder = TRUE)
        )
      )
    )
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# SERVER
# ─────────────────────────────────────────────────────────────────────────────

mod_import_bulk_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── DRIVE LIVE CONTROL (docs/DRIVE_LIVE_CONTROL_PLAN.md, grade G2) ─────
    # `updateActionButton()` does NOT click an actionButton, and
    # `session$setInputs()` does not exist on a LIVE session. So the existing
    # `observeEvent(input$btn_load, ...)` below is given a SECOND trigger: a
    # counter the file-drop poller increments.
    #
    # The counter is created HERE (a `reactiveVal` is reactivity, and reactivity
    # belongs to modules/ — C2), then published into the session-scoped registry
    # that `app.R` gave us via `global_data$drive_registry`, so the poller can
    # increment it. With `tools/_drive/arm.json` absent the counter never moves
    # and the observer behaves exactly as before.
    drive_btn <- ts_drive_bind_button("import_bulk-btn_load")
    drive_counter <- shiny::reactiveVal(0L)
    # READINESS GUARD (G2). `btn_load`'s observer opens with
    # `req(counts_reactive())` — and `counts_reactive()` itself opens with
    # `req(input$counts_file)` — so with no file chosen the observer aborts in
    # SILENCE and the poller would report `done` for an import that never
    # happened. The guard tests the WIDGET's own value, never a path string
    # (spec S5: the fileInput is never faked), and it deliberately does NOT
    # call `counts_reactive()`: that PARSES the file, and a readiness probe
    # must stay cheap enough to run on every scenario.
    drive_ready <- function() {
      if (is.null(shiny::isolate(input$counts_file))) {
        "no counts file selected (fileInput `counts_file` is empty)"
      } else TRUE
    }
    ts_drive_publish_token(global_data, "import_bulk-btn_load", drive_counter,
                           ready = drive_ready)
    # A `reactive()` is the documented trigger type that respects both a
    # reactiveVal and an actionButton counter, so drag-and-click share one path.
    drive_trigger <- shiny::reactive(list(drive_counter(), input$btn_load))

    # ── i18n proxy ──────────────────────────────────────────────────────────
    .tr <- function(key) {
      tr <- isolate(global_data$i18n)
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # ── MD-1 : enregistrement optionnel du jeu importé dans bulk_datasets ───
    # Producteur "import" (contrat BULK_MULTI_CONTRACT.md §6) : lecture seule
    # de bulk_obj ; un échec (ex. label déjà pris) est un AVERTISSEMENT et ne
    # bloque jamais l'import.
    .register_multi_dataset <- function(obj) {
      lbl <- trimws(input$multi_label %||% "")
      if (!nzchar(lbl)) return(invisible(NULL))
      res <- tryCatch(
        bulk_multi_register(global_data$bulk_datasets, lbl, obj,
                            producer = "import"),
        bulk_multi_error = function(e) e)
      if (inherits(res, "bulk_multi_error")) {
        add_log(paste(" ⚠️ Dataset non enregistré (multi-jeux) :", res$message))
        showNotification(
          sprintf(.tr("⚠️ Dataset non enregistré (multi-jeux) : %s"), res$message),
          type = "warning", duration = 8)
        return(invisible(NULL))
      }
      global_data$bulk_datasets <- res
      add_log(paste(" 📦 Dataset enregistré pour la comparaison multi-jeux :", lbl))
      showNotification(
        sprintf(.tr("📦 Import enregistré pour la comparaison multi-jeux : « %s »."), lbl),
        type = "message", duration = 6)
      invisible(NULL)
    }

    # ── LOT 4A (V1.x UX): native jump to the EXISTING Bulk mapping panel ────
    # No UI duplication: select the analysis page (top-level page_navbar via
    # the root session handle) and open the "0. Mapping IDs" accordion panel.
    # Silent no-op if the handles are missing.
    observeEvent(input$goto_mapping, {
      sess <- global_data$session
      req(!is.null(sess))
      nav_select(id = "main_nav", selected = "tab_bulk", session = sess)
      try(accordion_panel_open(id = "bulk-acc_bulk", values = "panel_mapping", session = sess), silent = TRUE)
    })

    observeEvent(global_data$language, {
      updateRadioButtons(session, "bulk_import_mode",
        label = .tr("Mode d'import"),
        choices = setNames(
          c("merged_matrix", "per_sample"),
          c(.tr("Merged matrix (un seul fichier)"), .tr("One file per sample"))
        ),
        selected = isolate(input$bulk_import_mode) %||% "merged_matrix"
      )
      updateRadioButtons(session, "counts_format",
        label = .tr("Format de la matrice"),
        choices = setNames(
          c("rows", "cols"),
          c(.tr("Genes en lignes (standard)"), .tr("Genes en colonnes (transposé)"))
        ),
        selected = isolate(input$counts_format) %||% "rows"
      )
    }, ignoreInit = TRUE)

    # ── Logger (inchangé) ──────────────────────────────────────────────────
    logs <- reactiveVal("En attente d'import...")
    add_log <- function(msg) {
      timestamp <- format(Sys.time(), "%H:%M:%S")
      logs(paste0("[", timestamp, "] ", msg, "\n", isolate(logs())))
    }
    
    temp_data <- reactiveValues(is_loaded = FALSE)
    
    # ── Metadata auto-inference state (inchangé) ───────────────────────────
    infer_preview_rv  <- reactiveVal(NULL)
    inferred_metadata <- reactiveVal(NULL)
    
    # ── Per-sample reactive state ──────────────────────────────────────────
    rv_ps <- reactiveValues(
      metadata  = NULL,   # auto-generated metadata data.frame
      preview   = NULL    # preview table (one row per file)
    )
    
    # ── smart_read (inchangé) ──────────────────────────────────────────────
    smart_read <- function(filepath, has_header = TRUE, has_rownames = TRUE) {
      ext <- tolower(tools::file_ext(filepath))
      tryCatch({
        df <- switch(
          ext,
          "csv"  = read.csv(filepath, header = has_header,
                            row.names = if (has_rownames) 1 else NULL,
                            check.names = FALSE, stringsAsFactors = FALSE,
                            fileEncoding = "UTF-8-BOM"),
          "tsv"  = read.delim(filepath, header = has_header,
                              row.names = if (has_rownames) 1 else NULL,
                              sep = "\t", check.names = FALSE,
                              stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM"),
          "txt"  = read.delim(filepath, header = has_header,
                              row.names = if (has_rownames) 1 else NULL,
                              check.names = FALSE, stringsAsFactors = FALSE,
                              fileEncoding = "UTF-8-BOM"),
          "xlsx" = {
            if (!requireNamespace("readxl", quietly = TRUE))
              stop(errorCondition("Package 'readxl' nécessaire pour lire les fichiers .xlsx", class = "bulk_import_error"))
            tmp <- readxl::read_excel(filepath, col_names = has_header)
            tmp <- as.data.frame(tmp, check.names = FALSE, stringsAsFactors = FALSE)
            if (has_rownames) { rownames(tmp) <- tmp[[1]]; tmp <- tmp[, -1, drop = FALSE] }
            tmp
          },
          stop(errorCondition("Format de fichier non supporté", class = "bulk_import_error"))
        )
        if (!is.null(colnames(df))) colnames(df) <- trimws(sub("^\ufeff", "", colnames(df)))
        if (!is.null(rownames(df))) rownames(df) <- trimws(sub("^\ufeff", "", rownames(df)))
        df
      }, error = function(e) stop(errorCondition(paste("Erreur de lecture:", e$message), class = "bulk_import_error")))
    }
    
    # =========================================================================
    # MODE MERGED_MATRIX — inchangé
    # =========================================================================

    # ── .rda/.RData "Inspect & Select" (composant mutualisé) ───────────────
    # Deux instances du composant : l'une désigne la matrice de comptages,
    # l'autre les métadonnées (un .rda peut alimenter les deux slots —
    # déposer le même fichier dans les deux panneaux, sélectionner 1 objet
    # par instance). Contrat docs/contracts/RDATA_IMPORT_CONTRACT.md.
    rdata_counts_rv <- reactiveVal(NULL)
    rdata_meta_rv   <- reactiveVal(NULL)
    counts_rda_obj  <- reactiveVal(NULL)  # objet validé (comptages)
    metadata_rda_obj <- reactiveVal(NULL) # objet validé (métadonnées)

    rdata_picker_server(
      "rdata_picker_bulk_counts",
      file_rv      = rdata_counts_rv,
      commit_fn    = function(obj, obj_name) {
        if (is.data.frame(obj)) obj <- as.matrix(obj)
        counts_rda_obj(obj)
        add_log(paste("✓ Objet .RData sélectionné comme comptages :", obj_name))
      },
      expected     = c("matrix", "dgCMatrix", "dgTMatrix", "data.frame"),
      context      = "import bulk counts (.RData)",
      import_label = "Utiliser comme matrice de comptages",
      tr           = .tr,
      log_fn       = add_log
    )
    rdata_picker_server(
      "rdata_picker_bulk_meta",
      file_rv      = rdata_meta_rv,
      commit_fn    = function(obj, obj_name) {
        metadata_rda_obj(obj)
        add_log(paste("✓ Objet .RData sélectionné comme métadonnées :", obj_name))
      },
      expected     = c("data.frame", "matrix", "dgCMatrix", "dgTMatrix"),
      context      = "import bulk metadata (.RData)",
      import_label = "Utiliser comme métadonnées",
      tr           = .tr,
      log_fn       = add_log
    )

    counts_reactive <- reactive({
      req(input$counts_file)
      tryCatch({
        # Flux .rda/.RData/.rds : l'objet est déjà extrait/validé par le composant
        if (rdata_is_explorable_file(input$counts_file$datapath)) {
          obj <- counts_rda_obj()
          if (is.null(obj)) {
            add_log("ℹ Aperçu .RData actif : sélectionnez l'objet à utiliser comme comptages.")
            return(NULL)
          }
          if (input$counts_format == "cols") obj <- t(obj)
          df <- as.data.frame(obj)
          add_log(paste("✓ Matrice de counts .RData chargée:", nrow(df), "gènes ×", ncol(df), "échantillons"))
          return(df)
        }
        add_log("📂 Lecture du fichier de counts...")
        df <- smart_read(input$counts_file$datapath,
                         input$counts_has_header, input$counts_has_rownames)
        if (input$counts_format == "cols") {
          df <- as.data.frame(t(df))
          add_log(" ↻ Matrice transposée (genes étaient en colonnes)")
        }
        if (nrow(df) == 0 || ncol(df) == 0) {
          stop(errorCondition("Matrice vide après lecture. Vérifie les options d'en-tête/rownames.", class = "bulk_import_error"))
        } else {
          add_log(paste("✓ Matrice de counts chargée:", nrow(df), "gènes ×", ncol(df), "échantillons"))
        }
        df
      }, error = function(e) {
        err_msg <- e$message
        add_log(paste("⚠️ Erreur lecture counts:", err_msg))
        showNotification(paste("⚠️ Erreur lecture counts:", err_msg), type = "error", duration = 10)
        NULL
      })
    })
    
    metadata_file_reactive <- reactive({
      if (is.null(input$metadata_file)) return(NULL)
      tryCatch({
        # Flux .rda/.RData/.rds : objet extrait/validé par le composant
        if (rdata_is_explorable_file(input$metadata_file$datapath)) {
          obj <- metadata_rda_obj()
          if (is.null(obj)) {
            add_log("ℹ Aperçu .RData actif : sélectionnez l'objet à utiliser comme métadonnées.")
            return(NULL)
          }
          df <- as.data.frame(obj)
          add_log(paste("✓ Métadonnées .RData chargées:", nrow(df), "échantillons ×", ncol(df), "variables"))
          return(df)
        }
        add_log("📂 Lecture du fichier de métadonnées...")
        df <- smart_read(input$metadata_file$datapath,
                         input$metadata_has_header, input$metadata_has_rownames)
        add_log(paste("✓ Métadonnées chargées:", nrow(df), "échantillons ×", ncol(df), "variables"))
        df
      }, error = function(e) {
        add_log(paste("⚠️ Erreur lecture métadonnées:", e$message))
        showNotification(paste("Erreur métadonnées:", e$message), type = "warning", duration = 10)
        NULL
      })
    })
    
    effective_metadata <- reactive({ metadata_file_reactive() %||% inferred_metadata() })
    
    observe({
      shinyjs::toggleState("btn_infer_preview", condition = is.null(metadata_file_reactive()))
      shinyjs::toggleState("btn_infer_apply",   condition = is.null(metadata_file_reactive()))
    })
    
    observeEvent(input$btn_infer_preview, {
      req(counts_reactive())
      preview <- tryCatch(
        preview_metadata_split(colnames(counts_reactive()), delimiter = input$infer_delimiter),
        error = function(e) {
          showNotification(paste("Erreur découpage:", conditionMessage(e)), type = "error", duration = 8)
          NULL
        })
      infer_preview_rv(preview)
    })
    
    output$infer_preview_ui <- renderUI({
      preview <- infer_preview_rv()
      if (is.null(preview)) return(NULL)
      seg_mat <- do.call(rbind, lapply(preview$segments, function(s) {
        length(s) <- max(preview$n_seg); s }))
      rownames(seg_mat) <- NULL
      tagList(
        div(class = if (preview$n_seg_consistent) "alert alert-success" else "alert alert-warning",
            style = "font-size:0.78em;padding:6px;margin-top:6px;",
            if (preview$n_seg_consistent) {
              sprintf("✓ Découpage cohérent : %d segment(s) par échantillon.", preview$n_seg[1])
            } else {
              sprintf("⚠️ Nombre de segments incohérent (min=%d, max=%d).",
                      min(preview$n_seg), max(preview$n_seg))
            }),
        renderTable(as.data.frame(seg_mat, stringsAsFactors = FALSE))()
      )
    })
    
    output$infer_colnames_ui <- renderUI({
      preview <- infer_preview_rv()
      req(preview)
      n_seg <- if (preview$n_seg_consistent) preview$n_seg[1] else min(preview$n_seg)
      default_names <- if (n_seg == 2) c("condition", "replicate") else paste0("segment_", seq_len(n_seg))
      tagList(
        h6("Nom de chaque colonne:", style = "font-size:0.82em;font-weight:bold;margin-top:8px;"),
        lapply(seq_len(n_seg), function(i) {
          textInput(ns(paste0("seg_name_", i)), NULL, value = default_names[i],
                    placeholder = paste("Segment", i), width = "100%")
        })
      )
    })
    
    observeEvent(input$btn_infer_apply, {
      req(counts_reactive(), infer_preview_rv())
      samples <- colnames(counts_reactive())
      preview <- infer_preview_rv()
      n_seg   <- if (preview$n_seg_consistent) preview$n_seg[1] else min(preview$n_seg)
      col_names <- vapply(seq_len(n_seg), function(i) {
        v <- input[[paste0("seg_name_", i)]]
        if (is.null(v) || !nzchar(trimws(v))) paste0("segment_", i) else trimws(v)
      }, character(1))
      if (length(unique(col_names)) != length(col_names)) {
        showNotification("⚠️ Les noms de colonnes doivent être uniques.", type = "warning", duration = 6)
        return()
      }
      result <- withCallingHandlers(
        tryCatch(
          infer_metadata_from_names(samples, delimiter = input$infer_delimiter, col_names = col_names),
          error = function(e) {
            showNotification(paste("Erreur:", conditionMessage(e)), type = "error", duration = 8); NULL
          }),
        warning = function(w) {
          showNotification(paste("⚠️", conditionMessage(w)), type = "warning", duration = 8)
          invokeRestart("muffleWarning")
        })
      req(result)
      inferred_metadata(result)
      add_log(paste("✓ Métadonnées inférées:", paste(col_names, collapse = ", ")))
      showNotification("✓ Métadonnées inférées appliquées.", type = "message", duration = 5)
    })
    
    observeEvent(input$counts_file, {
      infer_preview_rv(NULL); inferred_metadata(NULL); temp_data$is_loaded <- FALSE
      # .rda/.RData/.rds : transfert au composant "Inspect & Select" (ou reset)
      f <- input$counts_file
      if (is.null(f) || !rdata_is_explorable_file(f$datapath)) {
        rdata_counts_rv(NULL); counts_rda_obj(NULL)
      } else {
        counts_rda_obj(NULL)
        rdata_counts_rv(list(datapath = f$datapath, name = f$name, size = f$size))
      }
    })
    observeEvent(input$metadata_file, {
      temp_data$is_loaded <- FALSE
      # .rda/.RData/.rds : transfert au composant "Inspect & Select" (ou reset)
      f <- input$metadata_file
      if (is.null(f) || !rdata_is_explorable_file(f$datapath)) {
        rdata_meta_rv(NULL); metadata_rda_obj(NULL)
      } else {
        metadata_rda_obj(NULL)
        rdata_meta_rv(list(datapath = f$datapath, name = f$name, size = f$size))
      }
    })
    
    output$detected_gene_id_banner <- renderUI({
      df <- counts_reactive(); req(df)
      detected <- tryCatch(detect_gene_id_type(rownames(df)), error = function(e) "unknown")
      if (detected %in% c("symbol", "unknown")) return(NULL)
      label <- switch(detected, ensembl = "Ensembl Gene ID", entrez = "Entrez ID",
                      affy_probe = "Probe Affymetrix", detected)
      div(class = "alert alert-light",
          style = "font-size:0.78em;border-left:3px solid #F39C12;margin-top:6px;",
          bsicons::bs_icon("lightbulb"),
          sprintf(" Identifiants détectés : %s. Convertissez-les via ", label),
          tags$strong("'0. Mapping IDs'"), " dans le module d'analyse RNA Bulk.")
    })
    
    output$counts_preview <- renderTable({
      df <- counts_reactive(); if (is.null(df)) return(NULL)
      head(df[, 1:min(5, ncol(df)), drop = FALSE], 10)
    }, rownames = TRUE)
    
    output$metadata_preview <- renderTable({
      df <- effective_metadata(); if (is.null(df)) return(NULL); head(df, 10)
    }, rownames = TRUE)
    
    # =========================================================================
    # MODE PER_SAMPLE — nouveau
    # =========================================================================
    
    # ── Parse all uploaded files reactively ───────────────────────────────
    ps_raw <- reactive({
      req(input$bulk_import_mode == "per_sample")
      files <- input$ps_files
      req(files)
      
      lapply(seq_len(nrow(files)), function(i) {
        df <- tryCatch(
          .read_per_sample_file(files$datapath[i], files$name[i]),
          error = function(e) NULL
        )
        list(
          file_index = i,
          filename   = files$name[i],
          datapath   = files$datapath[i],
          df         = df,
          sample_id  = .infer_sample_id(files$name[i]),
          detection  = if (!is.null(df)) .detect_columns_per_sample(df) else list(
            gene_candidates = character(0), count_candidates = character(0),
            auto_gene = NA_character_, auto_count = NA_character_)
        )
      })
    })
    
    # ── Dynamic UI for ambiguous column resolution ─────────────────────────
    output$ps_resolve_ui <- renderUI({
      req(input$bulk_import_mode == "per_sample")
      parsed <- ps_raw()
      
      blocks <- lapply(parsed, function(x) {
        need_gene  <- is.na(x$detection$auto_gene)  || length(x$detection$gene_candidates)  > 1
        need_count <- is.na(x$detection$auto_count) || length(x$detection$count_candidates) > 1
        
        if (!need_gene && !need_count) return(NULL)
        
        wellPanel(
          tags$strong(x$filename),
          if (need_gene && length(x$detection$gene_candidates) > 0) {
            selectInput(
              ns(paste0("ps_gene_col_", x$file_index)),
              "Colonne Gene ID",
              choices  = x$detection$gene_candidates,
              selected = x$detection$auto_gene %||% x$detection$gene_candidates[1]
            )
          },
          if (need_count && length(x$detection$count_candidates) > 0) {
            selectInput(
              ns(paste0("ps_count_col_", x$file_index)),
              "Colonne Count",
              choices  = x$detection$count_candidates,
              selected = x$detection$auto_count %||% x$detection$count_candidates[1]
            )
          }
        )
      })
      
      do.call(tagList, Filter(Negate(is.null), blocks))
    })
    
    # ── Preview table (one row per file) ───────────────────────────────────
    ps_preview <- reactive({
      req(input$bulk_import_mode == "per_sample")
      parsed <- ps_raw()
      
      rows <- lapply(parsed, function(x) {
        gene_col  <- input[[paste0("ps_gene_col_",  x$file_index)]] %||% x$detection$auto_gene
        count_col <- input[[paste0("ps_count_col_", x$file_index)]] %||% x$detection$auto_count
        
        status <- if (is.null(x$df))               "Erreur de lecture"
        else if (is.na(gene_col))         "Gene column non résolue"
        else if (is.na(count_col))        "Count column non résolue"
        else                               "OK"
        
        id_type <- if (!is.null(x$df) && !is.na(gene_col) && gene_col %in% names(x$df))
          tryCatch(detect_gene_id_type(x$df[[gene_col]]), error = function(e) "mixed/unknown")
        else "mixed/unknown"
        
        data.frame(
          filename       = x$filename,
          sample_id      = x$sample_id,
          gene_id_column = gene_col  %||% NA_character_,
          count_column   = count_col %||% NA_character_,
          id_type        = id_type,
          status         = status,
          stringsAsFactors = FALSE, check.names = FALSE
        )
      })
      
      prev <- dplyr::bind_rows(rows)
      rv_ps$preview <- prev
      prev
    })
    
    output$ps_preview_table <- DT::renderDT({
      req(input$bulk_import_mode == "per_sample")
      ts_datatable(
        ps_preview(),
        page_length = 8,
        buttons = FALSE,
        dom = "tip",
        filter = "none"
      )
    })
    
    # ── Auto-generate editable metadata whenever preview changes ──────────
    observe({
      req(input$bulk_import_mode == "per_sample")
      prev <- ps_preview()
      req(nrow(prev) > 0)
      
      new_md <- data.frame(
        sample_id = prev$sample_id,
        filename  = prev$filename,
        condition = vapply(prev$filename, .infer_condition, character(1)),
        batch     = NA_character_,
        stringsAsFactors = FALSE, check.names = FALSE
      )
      
      # Preserve existing manual edits on filename match
      old <- rv_ps$metadata
      if (!is.null(old) && nrow(old)) {
        idx  <- match(new_md$filename, old$filename)
        keep <- !is.na(idx)
        new_md$sample_id[keep]  <- old$sample_id[idx[keep]]
        new_md$condition[keep]  <- old$condition[idx[keep]]
        new_md$batch[keep]      <- old$batch[idx[keep]]
      }
      
      rv_ps$metadata <- new_md
    })
    
    output$ps_metadata_dt <- DT::renderDT({
      req(rv_ps$metadata)
      ts_datatable(
        rv_ps$metadata,
        editable = TRUE,
        page_length = 8,
        buttons = FALSE,
        filter = "none"
      )
    })
    
    observeEvent(input$ps_metadata_dt_cell_edit, {
      info <- input$ps_metadata_dt_cell_edit
      x <- rv_ps$metadata; req(x)
      x[info$row, info$col + 1L] <- info$value   # DT éditable 0-indexé côté JS
      rv_ps$metadata <- x
    })
    
    # ── Status / warnings banner ───────────────────────────────────────────
    output$ps_status_banner <- renderUI({
      req(input$bulk_import_mode == "per_sample")
      msgs <- character(0)
      
      prev <- rv_ps$preview
      if (!is.null(prev) && nrow(prev)) {
        bad_files <- prev$filename[prev$status != "OK" &
                                     !grepl("^OK", prev$status)]
        if (length(bad_files))
          msgs <- c(msgs, paste("Fichiers rejetés:", paste(bad_files, collapse = ", ")))
        
        id_types <- unique(prev$id_type[!is.na(prev$id_type) & prev$id_type != "mixed/unknown"])
        if (length(id_types) > 1)
          msgs <- c(msgs, paste("Types d'identifiants mixtes:", paste(id_types, collapse = ", ")))
      }
      
      md <- rv_ps$metadata
      if (!is.null(md) && nrow(md)) {
        cond <- as.character(md$condition)
        cond <- cond[!is.na(cond) & nzchar(trimws(cond))]
        tab  <- table(cond)
        if (length(tab) < 2)
          msgs <- c(msgs, "1 seule condition : QC/PCA actifs, DE/pathway désactivés.")
        else if (any(tab < 2))
          msgs <- c(msgs, "Au moins 1 groupe avec < 2 réplicats : DE/pathway désactivés.")
        if (!is.null(prev) && nrow(prev) > 0) {
          n_entrez <- sum(prev$id_type == "Entrez", na.rm = TRUE)
          if (n_entrez > 0)
            msgs <- c(msgs, sprintf("%d fichier(s) avec IDs Entrez — conversion possible via '0. Mapping IDs'.",
                                    n_entrez))
        }
      }
      
      if (!length(msgs)) return(NULL)
      div(class = "alert alert-warning", style = "font-size:0.82em;margin-top:6px;",
          lapply(msgs, tags$p))
    })
    
    # =========================================================================
    # BOUTON PERSISTANT : readiness_banner + btn_load
    # =========================================================================
    
    output$readiness_banner <- renderUI({
      if (input$bulk_import_mode == "merged_matrix") {
        counts_ok <- !is.null(counts_reactive())
        meta_ok   <- !is.null(effective_metadata())
        div(style = "font-size:0.8em;",
            if (counts_ok) span("✅ Counts chargés",     style = "color:#18BC9C;font-weight:bold;")
            else           span("⚪ Counts manquants",    style = "color:#999;"),
            " · ",
            if (meta_ok) span("✅ Métadonnées prêtes",   style = "color:#18BC9C;font-weight:bold;")
            else         span("⚪ Métadonnées (optionnel)", style = "color:#999;"))
      } else {
        files <- input$ps_files
        prev  <- rv_ps$preview
        n_ok  <- if (!is.null(prev)) sum(grepl("^OK", prev$status)) else 0
        n_tot <- if (!is.null(prev)) nrow(prev) else 0
        
        div(style = "font-size:0.8em;",
            if (!is.null(files)) span(sprintf("✅ %d/%d fichiers OK", n_ok, n_tot),
                                      style = "color:#18BC9C;font-weight:bold;")
            else span("⚪ Aucun fichier",       style = "color:#999;"),
            " · ",
            if (!is.null(rv_ps$metadata)) span("✅ Métadonnées prêtes", style = "color:#18BC9C;font-weight:bold;")
            else                          span("⚪ Métadonnées",         style = "color:#999;"))
      }
    })
    
    observe({
      ready <- if (input$bulk_import_mode == "merged_matrix") {
        !is.null(counts_reactive())
      } else {
        !is.null(input$ps_files) && !is.null(rv_ps$metadata) && nrow(rv_ps$metadata) > 0
      }
      shinyjs::toggleState("btn_load", condition = ready)
      shinyjs::html("btn_load",
                    html = if (isTRUE(temp_data$is_loaded)) "🔄 Mettre à jour les Données"
                    else "🚀 Charger les Données")
    })
    
    # =========================================================================
    # BTN_LOAD — dispatch merged_matrix / per_sample
    # =========================================================================
    
    # DRIVE (G2): `drive_trigger` is a `reactive()` on (driver counter, button).
    # Reading it in the trigger position keeps the ORIGINAL click semantics
    # (`ignoreNULL` default) while letting the file-drop poller fire the same
    # body. Nothing else in this observer changed.
    #
    # MEASURED RISK, not yet observed on a live session: on the FIRST flush a
    # `reactive()` can deliver its start value (`list(0L, NULL)`) before the
    # observer has a trigger, so the `req()` below must reject that value.
    # `input$btn_load > 0` is FALSE on NULL *and* the second branch relies on
    # `||` short-circuit, so the start value is rejected — but `x > 0` on a
    # LENGTH-0 `numeric(0)` returns `logical(0)`, which `req()` would also
    # refuse. The guard is therefore safe by two independent paths; it must
    # still be confirmed live (G0/G1 acceptance) rather than assumed.
    observeEvent(drive_trigger(), {
      req(input$btn_load > 0 || shiny::isolate(drive_counter()) > 0)
      
      # ── Branch merged_matrix (code original inchangé) ─────────────────
      if (input$bulk_import_mode == "merged_matrix") {
        req(counts_reactive())
        add_log("🔄 Préparation de l'objet RNA Bulk...")
        showNotification("Traitement des données...", type = "message", duration = NULL, id = ns("progress"))
        
        tryCatch({
          counts   <- counts_reactive()
          metadata <- effective_metadata()
          
          add_log(" → Conversion des valeurs en numérique...")
          counts <- as.data.frame(counts, check.names = FALSE, stringsAsFactors = FALSE)
          for (col in colnames(counts)) {
            if (is.character(counts[[col]]))
              counts[[col]] <- suppressWarnings(as.numeric(counts[[col]]))
          }
          if (anyNA(as.matrix(counts)))
            add_log(" ⚠️ Certaines valeurs ne sont pas numériques (NA après conversion)")
          
          counts_matrix <- as.matrix(counts)
          gene_counts   <- rowSums(counts_matrix, na.rm = TRUE)
          keep_genes    <- gene_counts >= input$min_counts
          counts_matrix <- counts_matrix[keep_genes, , drop = FALSE]
          add_log(paste(" → Gènes filtrés:", sum(!keep_genes), "retirés,",
                        sum(keep_genes), "conservés"))
          
          bulk_obj <- list(counts    = counts_matrix,
                           metadata  = NULL,
                           project   = input$project_name,
                           type      = "bulk",
                           timestamp = Sys.time())
          
          if (!is.null(metadata)) {
            sample_names   <- colnames(counts_matrix)
            metadata_names <- rownames(metadata)
            if (all(sample_names %in% metadata_names)) {
              bulk_obj$metadata <- metadata[sample_names, , drop = FALSE]
              add_log(paste(" ✓ Métadonnées alignées:", ncol(bulk_obj$metadata), "variables"))
            } else {
              bulk_obj$metadata <- data.frame(sample = sample_names, row.names = sample_names)
              add_log(" ⚠️ Métadonnées non alignées - création de métadonnées par défaut")
            }
          } else {
            bulk_obj$metadata <- data.frame(
              sample = colnames(counts_matrix), row.names = colnames(counts_matrix))
            add_log(" → Aucune métadonnée — colonne 'sample' par défaut créée")
          }
          
          global_data$bulk_obj <- bulk_obj
          temp_data$is_loaded  <- TRUE
          add_log(paste("✅ Import réussi!", nrow(counts_matrix), "gènes ×",
                        ncol(counts_matrix), "échantillons"))
          .register_multi_dataset(global_data$bulk_obj)  # MD-1 (label optionnel)
          removeNotification(id = ns("progress"))
          showNotification(paste("✅ Import réussi:", ncol(counts_matrix),
                                 "échantillons,", nrow(counts_matrix), "gènes"),
                           type = "message", duration = 5)
          
        }, error = function(e) {
          removeNotification(id = ns("progress"))
          add_log(paste("❌ Erreur lors du chargement:", e$message))
          showNotification(paste("Erreur:", e$message), type = "error", duration = 10)
          temp_data$is_loaded <- FALSE
        })
        
        # ── Branch per_sample ───────────────────────────────────────────────
      } else {
        
        parsed   <- ps_raw()
        prev     <- rv_ps$preview
        metadata <- rv_ps$metadata
        
        req(length(parsed) > 0, nrow(prev) == length(parsed), nrow(metadata) == length(parsed))
        
        if (anyDuplicated(metadata$sample_id)) {
          showNotification("❌ sample_id dupliqués dans la métadata.", type = "error")
          return(NULL)
        }
        
        add_log("🔄 Préparation import multi-fichiers bulk...")
        showNotification("Traitement des données...", type = "message",
                         duration = NULL, id = ns("progress"))
        
        result <- tryCatch({
          
          sample_tables <- vector("list", length(parsed))
          int_like_vec  <- logical(length(parsed))
          id_type_vec   <- character(length(parsed))
          
          for (i in seq_along(parsed)) {
            gene_col  <- prev$gene_id_column[i]
            count_col <- prev$count_column[i]
            
            if (is.null(parsed[[i]]$df)) {
              stop(errorCondition(sprintf("Fichier illisible : %s", parsed[[i]]$filename), class = "bulk_import_error"))
            }
            if (is.na(gene_col) || !nzchar(gene_col)) {
              stop(errorCondition(sprintf("Gene column non résolue : %s", parsed[[i]]$filename), class = "bulk_import_error"))
            }
            if (is.na(count_col) || !nzchar(count_col)) {
              stop(errorCondition(sprintf("Count column non résolue : %s", parsed[[i]]$filename), class = "bulk_import_error"))
            }
            
            res <- .prepare_one_sample(
              df            = parsed[[i]]$df,
              sample_id     = metadata$sample_id[i],
              gene_col      = gene_col,
              count_col     = count_col,
              dup_threshold = input$ps_dup_threshold %||% 0.05
            )
            
            if (!isTRUE(res$ok)) stop(errorCondition(sprintf("%s : %s", parsed[[i]]$filename, res$status), class = "bulk_import_error"))
            
            add_log(paste0(" ✓ ", parsed[[i]]$filename, " (", res$status, ")"))
            sample_tables[[i]] <- res$data
            int_like_vec[i]    <- isTRUE(res$integer_like)
            id_type_vec[i]     <- res$id_type
          }
          
          merged     <- .merge_per_sample_tables(sample_tables)
          count_cols <- setdiff(names(merged), "feature_id")
          has_na     <- anyNA(merged[, count_cols, drop = FALSE])
          
          if (has_na && !isTRUE(input$ps_fill_zero)) {
            stop(errorCondition("Valeurs manquantes après full join. Cochez la case de confirmation avant d'importer.", class = "bulk_import_error"))
          }
          if (has_na) {
            for (cc in count_cols) merged[[cc]][is.na(merged[[cc]])] <- 0L
            add_log(" ⚠️ Counts manquants remplacés par 0 (full join)")
          }
          
          counts_mat <- as.matrix(merged[, count_cols, drop = FALSE])
          rownames(counts_mat) <- merged$feature_id
          storage.mode(counts_mat) <- "double"
          
          # Warn for non-integer (DESeq2 mode)
          if (!all(int_like_vec)) {
            bad_samples <- metadata$sample_id[!int_like_vec]
            add_log(paste(" ⚠️ Counts non entiers :", paste(bad_samples, collapse = ", ")))
            showNotification(paste("⚠️ Counts non entiers pour :",
                                   paste(bad_samples, collapse = ", "),
                                   "— DESeq2 nécessite des entiers."),
                             type = "warning", duration = NULL)
          }
          
          # ID type
          unique_types <- unique(id_type_vec[!is.na(id_type_vec)])
          merged_id_type <- if (length(unique_types) == 1) unique_types else "mixed/unknown"
          if (length(unique_types) > 1)
            showNotification(paste("⚠️ Types d'ID mixtes :", paste(unique_types, collapse = ", ")),
                             type = "warning", duration = NULL)
          
          # Validate design
          meta_for_design <- data.frame(
            sample_id = metadata$sample_id,
            condition = metadata$condition,
            batch     = metadata$batch,
            filename  = metadata$filename,
            stringsAsFactors = FALSE, check.names = FALSE
          )
          validation <- .validate_design(counts_mat, meta_for_design)
          
          if (!validation$ok) stop(errorCondition(paste(validation$messages, collapse = " | "), class = "bulk_import_error"))
          
          # Align metadata rownames to match existing bulk_obj structure
          aligned_meta <- as.data.frame(validation$metadata, stringsAsFactors = FALSE)
          rownames(aligned_meta) <- aligned_meta$sample_id
          
          # Add any extra columns from rv_ps$metadata (e.g. GEO-enriched)
          extra_cols <- setdiff(names(metadata), names(aligned_meta))
          if (length(extra_cols)) {
            idx <- match(aligned_meta$sample_id, metadata$sample_id)
            for (cc in extra_cols) aligned_meta[[cc]] <- metadata[[cc]][idx]
          }
          
          list(
            counts_mat    = counts_mat,
            metadata      = aligned_meta,
            id_type       = merged_id_type,
            de_ok         = validation$de_ok,
            val_messages  = validation$messages
          )
        }, error = function(e) {
          removeNotification(id = ns("progress"))
          add_log(paste("❌", e$message))
          showNotification(paste("Erreur:", e$message), type = "error", duration = NULL)
          NULL
        })
        
        if (is.null(result)) { temp_data$is_loaded <- FALSE; return(NULL) }
        
        # ── Commit to global_data (same structure as merged_matrix) ────────
        global_data$bulk_obj <- list(
          counts    = result$counts_mat,
          metadata  = result$metadata,
          project   = input$project_name,
          type      = "bulk",
          timestamp = Sys.time(),
          # Extra slots used by downstream modules
          gene_id_type  = result$id_type,
          de_allowed    = result$de_ok,
          import_mode   = "per_sample"
        )

        .register_multi_dataset(global_data$bulk_obj)  # MD-1 (label optionnel)

        temp_data$is_loaded <- TRUE
        removeNotification(id = ns("progress"))
        add_log(paste("✅ Import multi-fichiers réussi —",
                      nrow(result$counts_mat), "gènes ×",
                      ncol(result$counts_mat), "échantillons"))
        
        if (length(result$val_messages)) {
          showNotification(paste(result$val_messages, collapse = " | "),
                           type = "warning", duration = 8)
        } else {
          showNotification(
            paste("✅ Import réussi:", ncol(result$counts_mat),
                  "échantillons,", nrow(result$counts_mat), "gènes"),
            type = "message", duration = 5)
        }
      }
    })
    
    # ── Outputs info (inchangés) ───────────────────────────────────────────
    output$nb_samples <- renderText({
      if (is.null(global_data$bulk_obj) || is.null(global_data$bulk_obj$counts)) "-"
      else ncol(global_data$bulk_obj$counts)
    })
    output$nb_genes <- renderText({
      if (is.null(global_data$bulk_obj) || is.null(global_data$bulk_obj$counts)) "-"
      else nrow(global_data$bulk_obj$counts)
    })
    output$nb_metadata_vars <- renderText({
      if (is.null(global_data$bulk_obj) || is.null(global_data$bulk_obj$metadata)) "-"
      else ncol(global_data$bulk_obj$metadata)
    })
    output$status_obj <- renderText({
      if (is.null(global_data$bulk_obj)) "⚪ Inactif"
      else paste("🟢 Chargé (", format(global_data$bulk_obj$timestamp, "%H:%M:%S"), ")")
    })
    output$console_log <- renderText({ logs() })
  })
}
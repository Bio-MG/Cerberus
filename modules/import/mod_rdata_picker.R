# =============================================================================
# modules/import/mod_rdata_picker.R — Composant mutualisé .rda/.RData
# "Inspecter d'abord, importer ensuite"
# =============================================================================
# Utilisé par les modules d'import (SC, Bulk, Spatial, Communication,
# Velocity, snapshot de session). Contrat :
# docs/contracts/RDATA_IMPORT_CONTRACT.md (gel : test-rdata-contract-freeze.R).
#
# Logique de sécurité :
#   - le workspace est chargé UNE fois par fichier dans un environnement
#     isolé (rdata_load_env, parent = emptyenv) — jamais dans globalenv ;
#   - objet unique compatible (>= seuil de confidence) => commit automatique
#     via commit_fn ; incompatible => carte de preview avec message français ;
#   - >= 2 objets => carte de preview (table DT multi-sélection) ;
#   - « Importer » : exactement 1 ligne sélectionnée -> assert de classe ->
#     commit_fn (l'hôte garde le contrôle du commit dans global_data) ;
#   - « Exporter la sélection » : bundle .RData téléchargé côté navigateur,
#     AUCUNE écriture dans global_data.
#
# API :
#   rdata_picker_ui(id)                          — carte conditionnelle à
#                                                  embedder dans le panel hôte
#   rdata_picker_server(id, file_rv, commit_fn,  — file_rv : reactiveVal hôte
#       expected, context, tr, log_fn)             (NULL ou liste datapath/
#                                                  name/size, cf. fileInput)
#
# commit_fn(obj, obj_name) : commit de l'objet validé dans l'état hôte.
#   Doit stop() avec un message français en cas d'échec (rattrapé ici et
#   affiché en notification) ; retour ignoré.
# =============================================================================

# Libellés i18n par code de classification (clé = chaîne FR, cf. contrat §5)
.RDATA_TYPE_KEYS <- c(
  seurat   = "Objet Seurat",
  sce      = "SingleCellExperiment",
  cellchat = "Objet CellChat",
  velocity = "Données de vélocité",
  metadata = "Table de données (métadonnées ?)",
  matrix   = "Matrice (comptages ?)",
  image    = "Image / graphique",
  other    = "Autre"
)

#' Carte de preview .RData (UI) — à embedder dans le panel du module hôte
rdata_picker_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("picker_card"))
}

#' Serveur du composant .RData mutualisé
#'
#' @param id Namespace du composant.
#' @param file_rv Reactive/ReactiveVal de l'hôte : NULL ou liste nommée
#'   avec datapath, name, size (mêmes champs qu'une ligne de fileInput).
#'   L'hôte ne pose une valeur QUE pour un fichier .rda/.RData
#'   (rdata_is_supported_file), après le contrôle d'intégrité d'upload.
#' @param commit_fn function(obj, obj_name) — commit hôte ; stop() en cas
#'   d'échec (message français).
#' @param expected Classes acceptées pour rdata_assert_class (NULL = tout
#'   objet accepté, l'hôte assume alors sa propre validation).
#' @param context Contexte pour les messages de validation de classe.
#' @param import_label Libellé du bouton d'import (ex. « Utiliser comme
#'   métadonnées » pour le slot métadonnées de Bulk).
#' @param tr Fonction de traduction de l'hôte (défaut : identité).
#' @param log_fn Fonction de log optionnelle (console de l'hôte).
rdata_picker_server <- function(id, file_rv, commit_fn, expected = NULL,
                                context = "import .RData",
                                import_label = "Importer l'objet sélectionné",
                                tr = function(key) key, log_fn = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    .log <- function(msg) if (!is.null(log_fn)) log_fn(msg)

    env_rv <- reactiveVal(NULL)    # environnement .RData chargé (1 seule fois)
    info_rv <- reactiveVal(NULL)   # table de description (preview)
    forced_rv <- reactiveVal(FALSE)  # preview forcée (objet unique incompatible)

    .notify <- function(msg, type = "error") {
      showNotification(msg, type = type, duration = 10)
    }

    # Validation + commit — retourne un message d'erreur (character) ou NULL
    .validate_and_commit <- function(obj, obj_name) {
      err <- tryCatch({
        rdata_assert_class(obj, expected, context = context)
        commit_fn(obj, obj_name)
        NULL
      }, error = function(e) conditionMessage(e))
      err
    }

    # ── Réaction au fichier posé par l'hôte ────────────────────────────────
    observeEvent(file_rv(), {
      old <- env_rv()
      if (!is.null(old)) rdata_free(old)
      env_rv(NULL); info_rv(NULL); forced_rv(FALSE)

      f <- file_rv()
      if (is.null(f) || !rdata_is_explorable_file(f$datapath)) return()

      warn_mb <- if (exists("TS_IMPORT_RDA_WARN_MB", inherits = TRUE))
        TS_IMPORT_RDA_WARN_MB else 500
      if (!is.null(f$size) && (f$size / 1024^2) > warn_mb) {
        .log(paste("⚠", tr("Fichier volumineux : l'inspection charge le workspace entier en mémoire.")))
      }

      .log(paste("🔍", tr("Aperçu du contenu .RData"), "—", f$name))
      env <- tryCatch(rdata_read_file_env(f$datapath), error = function(e) e)
      if (inherits(env, "condition")) {
        .log(paste("❌", conditionMessage(env)))
        .notify(conditionMessage(env))
        return()
      }
      env_rv(env)
      # Aplatissement exploratoire : les listes nommees (workspaces, objets
      # type data_humanSkin$data$...) produisent une ligne par chemin
      # "objet$enfant$..." — l'utilisateur choisit la feuille a importer.
      info <- rdata_flatten_env(env)
      info_rv(info)

      if (nrow(info) == 1L) {
        # Chemin rapide : import direct si l'objet unique est compatible
        obj <- tryCatch(rdata_extract_path(env, info$name[1]),
                        error = function(e) e)
        if (inherits(obj, "condition")) {
          .notify(conditionMessage(obj)); return()
        }
        err <- .validate_and_commit(obj, info$name[1])
        if (is.null(err)) {
          .log(paste("✅", tr("Importer l'objet sélectionné"), "—", info$name[1]))
        } else {
          # incompatible : on montre la carte (l'utilisateur choisit / exporte)
          forced_rv(TRUE)
          .log(paste("⚠", err))
          .notify(err)
        }
      } else {
        .log(paste("ℹ", nrow(info), tr("Plusieurs objets détectés. Sélectionnez-en un à importer, ou exportez-en plusieurs vers un fichier .RData.")))
      }
    })

    # ── Carte de preview (>= 2 objets, ou objet unique incompatible) ───────
    show_picker <- reactive({
      info <- info_rv()
      !is.null(info) && (nrow(info) >= 2L || isTRUE(forced_rv()))
    })

    # Table affichée : libellés i18n des types (scalaires, jamais vectoriels)
    .display_table <- function(info) {
      display <- info
      display$type <- vapply(info$type_code, function(tc) {
        key <- .RDATA_TYPE_KEYS[[tc]]
        tr(if (is.null(key)) "Autre" else key)
      }, character(1), USE.NAMES = FALSE)
      display <- display[, c("name", "class", "dimensions", "size_mb", "type")]
      names(display) <- c(tr("Nom"), tr("Classe"), tr("Dimensions"),
                          tr("Taille (Mo)"), tr("Type"))
      display
    }

    output$picker_card <- renderUI({
      if (!req(show_picker(), cancelOutput = TRUE)) return(NULL)
      info <- info_rv()
      n_sel <- length(input$preview_table_rows_selected %||% integer(0))

      tagList(
        if (nrow(info) >= 2L) div(
          class = "alert alert-warning p-2 mt-2", style = "font-size:0.85rem;",
          bsicons::bs_icon("exclamation-triangle"), " ",
          tr("Plusieurs objets détectés. Sélectionnez-en un à importer, ou exportez-en plusieurs vers un fichier .RData.")
        ),
        h6(tr("Contenu .RData — choisissez les objets à importer ou à sauvegarder"),
           class = "text-muted mt-2"),
        helpText(style = "font-size:0.78rem;",
                 tr("Les listes nommées sont explorées (chemins type objet$enfant) — sélectionnez la feuille à importer ou à sauvegarder.")),
        DT::DTOutput(ns("preview_table")),
        layout_columns(
          col_widths = c(4, 4, 4),
          actionButton(ns("import_btn"),
                       tr(import_label),
                       class = "btn-primary btn-sm w-100"),
          downloadButton(ns("export_btn"),
                         tr("Exporter la sélection (.RData)"),
                         class = "btn-info btn-sm w-100"),
          downloadButton(ns("export_rds_btn"),
                         tr("Exporter l'objet sélectionné (.rds)"),
                         class = "btn-outline-info btn-sm w-100")
        ),
        if (n_sel != 1L) helpText(
          style = "font-size:0.78rem;",
          tr("Sélectionnez exactement un objet à importer."),
          " ", tr("Sélectionnez au moins un objet à exporter."),
          " ", tr("Sélectionnez exactement un objet à exporter en .rds.")
        )
      )
    })

    output$preview_table <- DT::renderDT({
      req(show_picker())
      ts_datatable(
        .display_table(info_rv()),
        selection = list(mode = "multiple", selected = NULL),
        page_length = 10,
        buttons = FALSE,
        dom = "tip",
        filter = "none"
      )
    })

    # ── Import : exactement une ligne sélectionnée ─────────────────────────
    observeEvent(input$import_btn, {
      env <- env_rv(); req(env)
      sel <- input$preview_table_rows_selected
      if (is.null(sel) || length(sel) != 1L) {
        .notify(tr("Sélectionnez exactement un objet à importer."))
        return()
      }
      info <- info_rv()
      obj_name <- info$name[sel[1]]
      obj <- tryCatch(rdata_extract_path(env, obj_name), error = function(e) e)
      if (inherits(obj, "condition")) {
        .log(paste("❌", conditionMessage(obj)))
        .notify(conditionMessage(obj)); return()
      }
      err <- .validate_and_commit(obj, obj_name)
      if (is.null(err)) {
        .log(paste("✅", tr("Importer l'objet sélectionné"), "—", obj_name))
      } else {
        .log(paste("❌", err))
        .notify(err)
      }
    })

    # ── Export : bundle .RData téléchargé (hors application) ───────────────
    output$export_btn <- downloadHandler(
      filename = function() {
        paste0("Cerberus_selection_", format(Sys.time(), "%Y%m%d_%H%M%S"),
               ".RData")
      },
      content = function(file) {
        env <- env_rv()
        sel <- input$preview_table_rows_selected
        if (is.null(env) || is.null(sel) || length(sel) == 0L) {
          showNotification(tr("Sélectionnez au moins un objet à exporter."),
                           type = "warning", duration = 8)
          return()
        }
        info <- info_rv()
        nms <- rdata_export_paths(env, info$name[sel], file)
        showNotification(
          sprintf(tr("Export .RData réussi : %d objet(s) sauvegardé(s) hors de l'application."), length(nms)),
          type = "message", duration = 8
        )
        .log(paste("⬇", tr("Exporter la sélection (.RData)"), "—", paste(nms, collapse = ", ")))
      }
    )

    # ── Export : objet unique en .rds (monofile, hors application) ─────────
    output$export_rds_btn <- downloadHandler(
      filename = function() {
        sel <- input$preview_table_rows_selected
        base <- "objet"
        if (!is.null(sel) && length(sel) == 1L) {
          p <- info_rv()$name[sel[1]]
          base <- gsub("[^[:alnum:]_-]", "_", p)
        }
        paste0("Cerberus_", base, "_", format(Sys.time(), "%Y%m%d_%H%M%S"),
               ".rds")
      },
      content = function(file) {
        env <- env_rv()
        sel <- input$preview_table_rows_selected
        if (is.null(env) || is.null(sel) || length(sel) != 1L) {
          showNotification(tr("Sélectionnez exactement un objet à exporter en .rds."),
                           type = "warning", duration = 8)
          return()
        }
        info <- info_rv()
        path <- info$name[sel[1]]
        obj <- tryCatch(rdata_extract_path(env, path), error = function(e) e)
        if (inherits(obj, "condition")) {
          showNotification(conditionMessage(obj), type = "error", duration = 10)
          return()
        }
        saveRDS(obj, file)
        showNotification(
          paste(tr("Exporter l'objet sélectionné (.rds)"), "—", path),
          type = "message", duration = 8
        )
        .log(paste("⬇", tr("Exporter l'objet sélectionné (.rds)"), "—", path))
      }
    )

    list(
      info       = reactive(info_rv()),
      n_selected = reactive(length(input$preview_table_rows_selected %||% integer(0)))
    )
  })
}

# =============================================================================
# modules/spatial/deconv/mod_spatial_deconv_reference.R — Reference state & prep
# =============================================================================
# Owns: ref_state reactiveValues, file upload, reference parsing, artifact
# preparation, shared-reference radio, status badges, cell-type table.
#
# Plain function (NOT a nested Shiny module) called from the orchestrator's
# moduleServer() — same pattern as bulk_de/.de_*_server().
#
# State contract:
#   WRITES (module-local): ref_state, ref_path, ref_artifact_info, ref_meta_cols
#   EXPOSES via return: list(reference_is_ready, effective_ref_path, ref_state)
#
# i18n: ref_state$message stays a raw language-neutral diagnostic payload
# (same convention as mod_import_spatial.R's shared_ref_state) — translated
# only at display time via .tr()/.t_fmt() inside renderUI/notifications.
# =============================================================================

.deconv_reference_server <- function(input, output, session, ns, global_data, shared_rv) {

  # Session-scoped scalar translation (plain strings, never HTML spans).
  .tr <- function(key) {
    tr <- global_data$i18n
    if (is.null(tr)) return(key)
    tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
  }

  # ── Reactive state ──────────────────────────────────────────────────────
  ref_state <- reactiveValues(
    obj = NULL, staged_path = NULL, orig_ext = NULL,
    status = "empty", message = NULL
  )
  ref_path          <- reactiveVal(NULL)
  ref_artifact_info <- reactiveVal(NULL)
  ref_meta_cols     <- reactiveVal(character(0))

  # ── Helpers ─────────────────────────────────────────────────────────────
  .version_hint <- function(msg) {
    if (!grepl("GetAssayData|slot.*defunct|defunct.*slot", msg, ignore.case = TRUE)) return("")
    sprintf(" [Seurat %s | SeuratObject %s | schard %s | SeuratDisk %s]",
            tryCatch(as.character(utils::packageVersion("Seurat")), error = function(e) "?"),
            tryCatch(as.character(utils::packageVersion("SeuratObject")), error = function(e) "?"),
            tryCatch(as.character(utils::packageVersion("schard")), error = function(e) "absent"),
            tryCatch(as.character(utils::packageVersion("SeuratDisk")), error = function(e) "absent"))
  }

  run_reference_pipeline <- function(staged_path, orig_ext) {
    ref_state$obj         <- NULL
    ref_state$staged_path <- NULL
    ref_state$status      <- "loading"
    ref_state$message     <- NULL
    ref_meta_cols(character(0))
    ref_path(NULL)
    ref_artifact_info(NULL)

    withProgress(message = .tr("Preparation de la reference..."), value = 0, {
      incProgress(0.15, detail = sprintf(.tr("Lecture (.%s)..."), if (nzchar(orig_ext)) orig_ext else "?"))
      raw_ref <- tryCatch(read_reference_scrna(staged_path), error = function(e) {
        msg <- conditionMessage(e)
        ref_state$status  <- "error"
        ref_state$message <- sprintf("Lecture (.%s) : %s%s", orig_ext, msg, .version_hint(msg))
        showNotification(paste(.tr("Reference — erreur de lecture :"), msg), type = "error", duration = 15)
        NULL
      })
      if (is.null(raw_ref)) return(invisible(FALSE))

      incProgress(0.4, detail = .tr("Conversion en objet Seurat..."))
      ref_obj <- tryCatch(prepare_reference_seurat(raw_ref, project_name = "Reference"), error = function(e) {
        msg <- conditionMessage(e)
        ref_state$status  <- "error"
        ref_state$message <- sprintf("Conversion Seurat : %s%s", msg, .version_hint(msg))
        showNotification(paste(.tr("Reference — erreur de conversion :"), msg), type = "error", duration = 15)
        NULL
      })
      if (is.null(ref_obj)) return(invisible(FALSE))
      if (!inherits(ref_obj, "Seurat")) {
        ref_state$status  <- "error"
        ref_state$message <- "Le fichier n'a pas pu etre converti en objet Seurat."
        showNotification(.tr("Reference — le fichier n'a pas pu etre converti en objet Seurat."),
                          type = "error", duration = 10)
        return(invisible(FALSE))
      }

      incProgress(0.3, detail = .tr("Lecture des metadonnees..."))
      meta_cols <- tryCatch(colnames(ref_obj@meta.data), error = function(e) {
        ref_state$status  <- "error"
        ref_state$message <- paste("Lecture metadata :", conditionMessage(e))
        showNotification(paste(.tr("Reference — erreur de lecture metadata :"), conditionMessage(e)),
                          type = "error", duration = 10)
        NULL
      })
      if (is.null(meta_cols)) return(invisible(FALSE))

      ref_state$obj         <- ref_obj
      ref_state$staged_path <- staged_path
      ref_state$orig_ext    <- orig_ext
      ref_state$status      <- "loaded"
      ref_state$message     <- NULL
      ref_meta_cols(meta_cols)
      incProgress(0.15, detail = .tr("Termine."))
      showNotification(.t_fmt(.tr("Reference chargee : {cells} cellules x {genes} genes (.{ext})."),
                              cells = ncol(ref_obj), genes = nrow(ref_obj),
                              ext = if (nzchar(orig_ext)) orig_ext else "?"),
                        type = "message", duration = 5)
      invisible(TRUE)
    })
  }

  # ── File upload ─────────────────────────────────────────────────────────
  observeEvent(input$ref_file, {
    req(input$ref_file)
    orig_ext <- tolower(tools::file_ext(input$ref_file$name))
    staged_path <- tryCatch({
      if (nzchar(orig_ext)) {
        p <- tempfile(fileext = paste0(".", orig_ext))
        file.copy(input$ref_file$datapath, p, overwrite = TRUE)
        p
      } else {
        input$ref_file$datapath
      }
    }, error = function(e) {
      ref_state$status  <- "error"
      ref_state$message <- paste("Copie du fichier :", conditionMessage(e))
      showNotification(paste(.tr("Reference — erreur de copie du fichier :"), conditionMessage(e)),
                        type = "error", duration = 10)
      NULL
    })
    req(staged_path)
    run_reference_pipeline(staged_path, orig_ext)
  })

  observeEvent(input$btn_recheck_ref, {
    path <- ref_state$staged_path %||% NULL
    ext  <- ref_state$orig_ext %||% ""
    if (is.null(path)) {
      showNotification(.tr("Aucun fichier a revérifier — uploadez d'abord une reference."),
                        type = "warning", duration = 6)
      return()
    }
    run_reference_pipeline(path, ext)
  })

  # ── Shared reference (global_data$spatial_reference) ────────────────────
  using_shared_ref <- reactive({
    !is.null(global_data$spatial_reference) &&
      (is.null(input$ref_source) || identical(input$ref_source, "shared"))
  })

  reference_is_ready <- reactive({
    if (using_shared_ref()) return(!is.null(global_data$spatial_reference$path))
    identical(ref_state$status, "loaded") && !is.null(ref_path())
  })

  effective_ref_path <- reactive({
    if (using_shared_ref()) global_data$spatial_reference$path else ref_path()
  })

  output$ref_source_picker_ui <- renderUI({
    global_data$language  # i18n: re-render on language switch
    req(global_data$spatial_reference)
    radioButtons(ns("ref_source"), .tr("Source de la reference"),
                 choices = stats::setNames(
                   c("shared", "upload"),
                   c(.tr("Reference partagee (Import > Spatial)"),
                     .tr("Uploader une nouvelle reference (cette session)"))),
                 selected = "shared")
  })

  output$shared_ref_status_ui <- renderUI({
    global_data$language  # i18n: re-render on language switch
    req(global_data$spatial_reference)
    info <- global_data$spatial_reference
    div(class = "alert alert-success", style = "font-size:0.75rem;",
        bsicons::bs_icon("check-circle"),
        sprintf(.tr("%s cellules x %s genes (colonne '%s', backend: %s)%s — preparee le %s."),
                format(info$n_cells, big.mark = ","), format(info$n_genes, big.mark = ","),
                info$celltype_col %||% "?", info$backend %||% "?",
                if (!is.null(info$source_label) && nzchar(info$source_label)) {
                  sprintf(" [%s]", info$source_label)
                } else "",
                tryCatch(format(info$created_at, "%d/%m %H:%M"), error = function(e) "?")))
  })

  output$ref_status_badge_ui <- renderUI({
    global_data$language  # i18n: re-render on language switch
    st <- ref_state$status
    badge <- switch(st,
      "empty" = div(class = "alert alert-light", style = "font-size:0.75rem; margin-bottom:4px;",
                    bsicons::bs_icon("circle"), " ", .tr("Aucune reference chargee.")),
      "loading" = div(class = "alert alert-info", style = "font-size:0.75rem; margin-bottom:4px;",
                      bsicons::bs_icon("hourglass-split"), " ", .tr("Chargement de la reference en cours...")),
      "error" = div(class = "alert alert-danger", style = "font-size:0.75rem; margin-bottom:4px;",
                    bsicons::bs_icon("x-circle"), " ",
                    .t_fmt(.tr("Erreur : {msg}"),
                           msg = ref_state$message %||% .tr("cause inconnue"))),
      "loaded" = if (reference_is_ready()) {
        div(class = "alert alert-success", style = "font-size:0.75rem; margin-bottom:4px;",
            bsicons::bs_icon("check-circle"), " ",
            .t_fmt(.tr("Reference chargee : {cells} cellules x {genes} genes."),
                   cells = ncol(ref_state$obj), genes = nrow(ref_state$obj)))
      } else {
        div(class = "alert alert-warning", style = "font-size:0.75rem; margin-bottom:4px;",
            bsicons::bs_icon("exclamation-triangle"), " ",
            .tr(paste0("Reference chargee — choisissez la colonne 'type cellulaire' puis cliquez ",
                       "'1) Preparer la reference (disque)' ci-dessous.")))
      },
      div(class = "alert alert-light", style = "font-size:0.75rem;", .tr("Statut inconnu."))
    )
    tagList(
      badge,
      if (identical(st, "error") || identical(st, "loaded")) {
        actionLink(ns("btn_recheck_ref"),
                   paste("\U1F504", .tr("Revérifier la reference (sans re-upload)")),
                   style = "font-size:0.72rem;")
      }
    )
  })

  # ── Cell-type column + artifact preparation ─────────────────────────────
  # SPMV-1 (fiche SPATIAL_META_COL_VALIDATION_PROPOSAL.md §4c) : la fenêtre
  # muette 2-200 et son fallback silencieux « else cols » (§2d-1, ancien
  # l.225-228) sont supprimés — TOUTES les colonnes sont proposées, triées
  # par le verdict du composant unique : compatibles (ok) d'abord, puis à
  # vérifier (warn), puis incompatibles (danger). Tri STABLE au sein d'un
  # niveau (l'ordre meta.data fait foi) ; plus aucun tri silencieux par
  # cardinalité décroissante. Le composant VALIDE et EXPLIQUE, il ne
  # transforme aucune donnée (§5).
  # SPMV-2 (fiche §6 :140, §7 :145-149) — rendu de la validation au sélecteur :
  #   * chaque option porte son verdict visible : badge vert discret « N
  #     classes » (ok, §4b) ; badge orange/rouge + raison traduite du
  #     composant (warn/danger) ;
  #   * un bandeau récapitule n_ok / n_warn / n_danger AU-DESSUS du sélecteur ;
  #   * la colonne retenue warn/danger EXIGE la case « Je confirme » (rendue
  #     pour warn/danger seulement — ok ne l'exige pas) ; l'id de la case est
  #     propre à CHAQUE colonne : la confirmation est fraîche, jamais reportée
  #     d'une colonne à l'autre ;
  #   * le bouton « 1) Préparer la référence » est DÉSACTIVÉ tant que la
  #     colonne retenue est danger SANS confirmation (§7-2), état EXPLIQUÉ à
  #     l'écran (§7-4) — disable par primitive shiny tagAppendAttributes
  #     (disabled = NA), shinyjs proscrit ; une warn reste exécutable mais
  #     éclairée (§7-1).
  # NB rendu : selectInput ÉCHAPPE ses labels d'options inconditionnellement
  # (shiny:::selectOptions -> htmlEscape, mesuré) — les badges y seraient
  # affichés comme du texte brut. D'où un tags$select manuel (classe
  # « shiny-input-select » : binding shiny conservé, mesuré sous testServer)
  # dont les tags$option portent le HTML du badge. Primitives bslib/shiny
  # uniquement, 0 CSS www/.
  output$ref_celltype_col_ui <- renderUI({
    global_data$language  # i18n: re-render on language switch
    req(ref_state$obj, length(ref_meta_cols()) > 0)
    ref_obj <- ref_state$obj
    cols <- ref_meta_cols()
    verdicts <- lapply(cols, function(cn) {
      spatial_meta_col_validation(ref_obj@meta.data, cn)
    })
    names(verdicts) <- cols
    verdict_levels <- vapply(verdicts, function(v) v$level, character(1))
    choices <- cols[order(match(verdict_levels, c("ok", "warn", "danger")), method = "radix")]

    # Badge par option (§4b) — pictogramme + raison traduite (données et clés
    # i18n échappées via htmltools::htmlEscape — shiny ne l'exporte pas, mesuré :
    # les reason_keys SPMV-1, ASCII sans & < >, passent telles quelles).
    .opt_label <- function(cn) {
      v <- verdicts[[cn]]
      if (identical(v$level, "ok")) {
        paste0('<span class="badge bg-success">',
               .t_fmt(.tr("{n} classes"), n = v$detail$n_levels %||% ""),
               '</span> ', htmltools::htmlEscape(cn))
      } else {
        sym <- if (identical(v$level, "danger")) "\u26d4" else "\u26a0"
        cls <- if (identical(v$level, "danger")) "bg-danger text-white" else "bg-warning text-dark"
        paste0('<span class="badge ', cls, '">', sym, '</span> ', htmltools::htmlEscape(cn),
               " — ", htmltools::htmlEscape(.tr(v$reason_key)))
      }
    }

    sel <- input$ref_celltype_col
    if (is.null(sel) || !sel %in% choices) sel <- choices[1]
    sel_is_warn   <- identical(verdicts[[sel]]$level, "warn")
    sel_is_danger <- identical(verdicts[[sel]]$level, "danger")
    # Case « Je confirme » : id PROPRE à la colonne retenue (caractères
    # d'id HTML sanitizés) — confirmation fraîche par colonne.
    confirm_id    <- paste0("ref_col_confirm__", gsub("[^A-Za-z0-9_.-]", "_", sel))
    sel_confirmed <- isTRUE(input[[confirm_id]])
    block_prepare <- sel_is_danger && !sel_confirmed

    n_ok     <- sum(verdict_levels == "ok")
    n_warn   <- sum(verdict_levels == "warn")
    n_danger <- sum(verdict_levels == "danger")

    btn <- actionButton(ns("btn_prepare_ref"), .tr("1) Preparer la reference (disque)"),
                        icon = icon("database"), class = "btn-sm btn-outline-primary w-100 mt-2")
    if (block_prepare) {
      btn <- tagAppendAttributes(btn, disabled = NA, `aria-disabled` = "true")
    }

    tagList(
      # Bandeau récapitulatif — AU-DESSUS du sélecteur (§4c).
      div(class = "alert alert-light", style = "font-size:0.72rem; margin-bottom:4px;",
          bsicons::bs_icon("info-circle"), " ",
          .t_fmt(.tr("Colonnes evaluees : {n_ok} compatibles, {n_warn} a verifier, {n_danger} incompatibles."),
                 n_ok = n_ok, n_warn = n_warn, n_danger = n_danger)),
      tags$div(class = "form-group shiny-input-container",
               tags$label(class = "control-label", id = ns("ref_celltype_col-label"),
                          `for` = ns("ref_celltype_col"), .tr("Colonne 'type cellulaire'")),
               tags$select(id = ns("ref_celltype_col"), class = "shiny-input-select form-select",
                           lapply(choices, function(cn) {
                             tags$option(value = cn,
                                         selected = if (identical(cn, sel)) NA,
                                         HTML(.opt_label(cn)))
                           }))),
      # Colonne retenue warn/danger : avertissement visible + case « Je
      # confirme » (ok ne l'exige pas).
      if (sel_is_warn || sel_is_danger) {
        tagList(
          div(class = if (sel_is_danger) "alert alert-danger" else "alert alert-warning",
              style = "font-size:0.72rem; margin-bottom:4px;",
              bsicons::bs_icon(if (sel_is_danger) "x-octagon" else "exclamation-triangle"), " ",
              .tr(verdicts[[sel]]$reason_key)),
          checkboxInput(ns(confirm_id),
                        .tr("Je confirme : je veux utiliser cette colonne malgre l'avertissement"),
                        value = sel_confirmed)
        )
      },
      # Blocage §7-2 — jamais silencieux : l'état désactivé est expliqué (§7-4).
      if (block_prepare) {
        div(class = "alert alert-danger", style = "font-size:0.72rem; margin-bottom:4px;",
            bsicons::bs_icon("x-circle"), " ",
            .tr("Preparation bloquee : cochez la case de confirmation pour utiliser cette colonne."))
      },
      div(class = "text-muted", style = "font-size:0.68rem;",
          .tr("Colonnes triees : compatibles en premier (vert), a verifier (orange), incompatibles (rouge) — chaque colonne est evaluee par le composant de validation des metadonnees.")),
      checkboxInput(ns("merge_rare_types"),
                    .tr("Fusionner/exclure les types rares (< 25 cellules)"),
                    value = TRUE),
      checkboxInput(ns("cap_ref_cells"),
                    .tr("Limiter le nombre de cellules par type (RAM/vitesse)"), value = TRUE),
      conditionalPanel(
        condition = sprintf("input['%s']", ns("cap_ref_cells")),
        numericInput(ns("max_cells_per_type"), .tr("Max cellules par type"), 500, min = 25, max = 5000, step = 25)
      ),
      div(class = "text-muted", style = "font-size:0.68rem;",
          .tr(paste0("Sous-echantillonnage stratifie (chaque type garde sa proportion relative) — ",
                     "accelere nettement RCTD/Label Transfer sur une grosse reference (ex: >50k cellules)."))),
      btn,
      uiOutput(ns("ref_artifact_status_ui"))
    )
  })

  observeEvent(list(ref_state$obj, input$ref_celltype_col, input$merge_rare_types,
                     input$cap_ref_cells, input$max_cells_per_type), {
    ref_path(NULL)
    ref_artifact_info(NULL)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  observeEvent(input$btn_prepare_ref, {
    req(ref_state$obj, input$ref_celltype_col)
    ref_obj <- ref_state$obj
    withProgress(message = .tr("Preparation de l'artifact de reference..."), value = 0.1, {
      incProgress(0.2, detail = .tr("Filtrage / fusion des types rares..."))
      result <- tryCatch({
        artifact <- prepare_reference_artifact(
          ref_obj            = ref_obj,
          celltype_col       = input$ref_celltype_col,
          merge_rare_types   = isTRUE(input$merge_rare_types),
          min_cells_per_type = RCTD_CELL_MIN_INSTANCE,
          max_cells_per_type = if (isTRUE(input$cap_ref_cells)) input$max_cells_per_type else NA_integer_
        )
        incProgress(0.6, detail = .tr("Ecriture sur disque..."))
        artifact
      }, error = function(e) {
        showNotification(paste(.tr("Erreur preparation reference:"), conditionMessage(e)), type = "error", duration = 10)
        NULL
      })
      if (!is.null(result)) {
        ref_path(result$path)
        ref_artifact_info(result)
        incProgress(0.1, detail = .tr("Termine."))
        showNotification(.t_fmt(
          .tr("Reference preparee : {cells} cellules x {genes} genes (backend: {backend}).{extra}"),
          cells = result$n_cells, genes = result$n_genes, backend = result$backend,
          extra = if (result$n_dropped_rare > 0) sprintf(.tr(" %d cellule(s) trop rare(s) exclue(s)."),
                                                         result$n_dropped_rare) else ""
        ), type = "message", duration = 6)
      }
    })
  })

  output$ref_artifact_status_ui <- renderUI({
    global_data$language  # i18n: re-render on language switch
    info <- ref_artifact_info()
    if (is.null(info)) {
      return(div(class = "alert alert-light", style = "font-size:0.72rem; margin-top:4px;",
                 bsicons::bs_icon("info-circle"), " ",
                 .tr("Reference pas encore preparee pour la deconvolution — cliquez ci-dessus.")))
    }
    div(class = "alert alert-success", style = "font-size:0.72rem; margin-top:4px;",
        bsicons::bs_icon("check-circle"), " ",
        .t_fmt(.tr("Pret : {cells} cellules x {genes} genes (backend: {backend}).{extra}"),
               cells = info$n_cells, genes = info$n_genes, backend = info$backend,
               extra = if (info$n_dropped_rare > 0) sprintf(.tr(" %d cellule(s) exclue(s) (trop rares)."),
                                                            info$n_dropped_rare) else ""))
  })

  # ── Cell-type counts table ──────────────────────────────────────────────
  ref_celltype_counts <- reactive({
    req(ref_state$obj, input$ref_celltype_col)
    req(input$ref_celltype_col %in% colnames(ref_state$obj@meta.data))
    ct <- as.character(ref_state$obj@meta.data[[input$ref_celltype_col]])
    ct <- ct[!is.na(ct) & nzchar(ct)]
    validate(need(length(ct) > 0, .tr("Aucune valeur non vide dans cette colonne.")))
    tab_counts <- table(ct)
    tab <- data.frame(`Type cellulaire` = names(tab_counts),
                      Effectif          = as.integer(tab_counts),
                      check.names = FALSE, stringsAsFactors = FALSE)
    tab[order(-tab$Effectif), ]
  })

  output$ref_celltype_summary_ui <- renderUI({
    global_data$language  # i18n: re-render on language switch
    tab <- tryCatch(ref_celltype_counts(), error = function(e) NULL)
    req(tab)
    rare <- tab[tab$Effectif < RCTD_CELL_MIN_INSTANCE, , drop = FALSE]
    tagList(
      if (nrow(rare) > 0) {
        div(class = "alert alert-warning", style = "font-size:0.72rem;",
            bsicons::bs_icon("exclamation-triangle"), " ",
            .t_fmt(.tr("{n_types} type(s) sous {min_cells} cellules : {types_list}."),
                   n_types = nrow(rare), min_cells = RCTD_CELL_MIN_INSTANCE,
                   types_list = paste(sprintf("%s (%d)", rare$`Type cellulaire`, rare$Effectif), collapse = ", ")),
            " ",
            .tr(paste0("Avec 'Fusionner/exclure les types rares' coche, ces cellules seront regroupees ",
                       "sous 'Autre' puis, si ce groupe reste trop petit, exclues de la reference.")))
      },
      div(style = "max-height:160px; overflow-y:auto;",
          DT::DTOutput(ns("ref_celltype_table")))
    )
  })
  output$ref_celltype_table <- DT::renderDT({
    global_data$language  # i18n: re-render on language switch
    tab <- ref_celltype_counts()
    ts_datatable(stats::setNames(tab, c(.tr("Type cellulaire"), .tr("Effectif"))),
                 page_length = 6, buttons = FALSE, dom = "tp",
                 filter = "none", scroll_x = FALSE)
  })

  # ── Expose state to orchestrator ────────────────────────────────────────
  list(
    ref_state          = ref_state,
    ref_path           = ref_path,
    ref_artifact_info  = ref_artifact_info,
    ref_meta_cols      = ref_meta_cols,
    reference_is_ready = reference_is_ready,
    effective_ref_path = effective_ref_path,
    using_shared_ref   = using_shared_ref
  )
}
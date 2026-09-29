# =============================================================================
# mod_sc_metadata.R — 0.5 Métadonnées : déclaration du design expérimental
# (condition / réplicat par échantillon) — UI + orchestration.
# =============================================================================
# Logique pure : R/sc/sc_metadata.R (contrat gelé SC_METADATA_CONTRACT.md).
#
# Ce module AJOUTE des colonnes à meta.data sur commit explicite uniquement
# (condition, replicate). Il ne modifie ni les comptages ni les analyses :
# zéro changement de comportement tant que l'utilisateur ne commite pas.
#
# Sans colonne `condition`, tout le volet A-vs-B (plots par condition
# mod_sc.R:798-851, pseudobulk sc_abundance_design.R:376-389, Milo/scCODA
# mod_sc_da_design.R:152-157, communication par condition) reste
# inaccessible — c'est le déblocage central du design multi-réplicats.
# =============================================================================

mod_sc_metadata_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-light", style = "font-size:0.78rem;padding:5px;margin-bottom:5px;",
        bsicons::bs_icon("info-circle"),
        " ", i18n$t("Déclarez la condition et le réplicat de chaque échantillon. Une CONDITION est une propriété de l'échantillon (pas de la cellule). Sans elle, les analyses A-vs-B (pseudobulk, Milo/scCODA, plots par condition) restent inaccessibles.")),
    selectInput(ns("sample_col"), i18n$t("Colonne échantillon"), choices = NULL),
    fluidRow(
      column(7, selectInput(ns("cond_position"), i18n$t("Position de la condition dans le nom"),
                            choices = setNames(
                              c("first", "last"),
                              c(.tr_plain("Condition en préfixe — A_1 → condition A, réplicat 1"),
                                .tr_plain("Condition en suffixe — 1a → condition a, réplicat 1"))
                            ), selected = "first")),
      column(5, actionButton(ns("btn_parse"), i18n$t("Déduire depuis les noms"),
                             icon = icon("wand-magic-sparkles"), class = "btn-outline-info w-100 mt-4"))
    ),
    h6(i18n$t("Table design (éditable)"), style = "font-weight:bold;"),
    DT::DTOutput(ns("map_dt")),
    div(class = "small text-muted mb-2",
        i18n$t("Éditez directement les cellules condition / replicate dans le tableau. Réplicat vide = l'échantillon EST le réplicat.")),
    hr(),
    fileInput(ns("csv_file"), i18n$t("…ou joindre un CSV de design (colonnes : sample, condition[, replicate])"),
              accept = c(".csv", ".tsv", ".txt", ".csv2")),
    verbatimTextOutput(ns("design_recap")),
    actionButton(ns("btn_commit"), i18n$t("✅ Appliquer le design à l'objet SC"),
                 icon = icon("table"), class = "btn-success w-100")
  )
}

mod_sc_metadata_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    .notify_err <- function(e) {
      showNotification(paste(.tr_plain("Erreur métadonnées :"), conditionMessage(e)),
                       type = "error", duration = 10)
    }

    map_rv <- reactiveVal(data.frame(sample = character(0), condition = character(0),
                                     replicate = character(0), stringsAsFactors = FALSE))

    # ── i18n push on language switch ─────────────────────────────────────
    observeEvent(global_data$language, {
      updateSelectInput(session, "sample_col", label = .tr("Colonne échantillon"))
      updateSelectInput(session, "cond_position", label = .tr("Position de la condition dans le nom"))
      updateActionButton(session, "btn_parse", label = .tr("Déduire depuis les noms"))
      updateActionButton(session, "btn_commit", label = .tr("✅ Appliquer le design à l'objet SC"))
    }, ignoreInit = TRUE)

    # ── Resynchronisation depuis l'objet courant (UN SEUL observateur par
    #    déclencheur — garde anti-duplication, comme mod_sc_rarity.R) ──────
    observeEvent(global_data$sc_obj, {
      obj <- global_data$sc_obj
      if (is.null(obj)) {
        updateSelectInput(session, "sample_col", choices = character(0))
        map_rv(data.frame(sample = character(0), condition = character(0),
                          replicate = character(0), stringsAsFactors = FALSE))
        return()
      }
      meta <- obj@meta.data
      cat_cols <- colnames(meta)[vapply(colnames(meta), function(x) {
        is.factor(meta[[x]]) || is.character(meta[[x]])
      }, logical(1))]
      updateSelectInput(session, "sample_col",
                        choices = cat_cols,
                        selected = if ("orig.ident" %in% cat_cols) "orig.ident" else cat_cols[1])
      tbl <- tryCatch(
        sc_metadata_sample_table(meta,
                                 sample_col = if ("orig.ident" %in% cat_cols) "orig.ident" else cat_cols[1]),
        sc_metadata_error = function(e) e)
      if (inherits(tbl, "sc_metadata_error")) {
        map_rv(data.frame(sample = character(0), condition = character(0),
                          replicate = character(0), stringsAsFactors = FALSE))
        return()
      }
      # Reprendre le design déjà appliqué si l'objet le porte déjà.
      if ("condition" %in% colnames(meta) && "orig.ident" %in% colnames(meta)) {
        hit <- match(tbl$sample, as.character(meta$orig.ident))
        if (!is.na(hit)) tbl$condition[!is.na(hit)] <- as.character(meta$condition)[na.omit(hit)]
        if ("replicate" %in% colnames(meta)) {
          tbl$replicate[!is.na(hit)] <- as.character(meta$replicate)[na.omit(hit)]
        }
      }
      map_rv(tbl)
    }, ignoreInit = FALSE)

    observeEvent(input$sample_col, {
      obj <- global_data$sc_obj
      req(obj, input$sample_col, input$sample_col %in% colnames(obj@meta.data))
      tbl <- tryCatch(
        sc_metadata_sample_table(obj@meta.data, sample_col = input$sample_col),
        sc_metadata_error = function(e) e)
      if (inherits(tbl, "sc_metadata_error")) { .notify_err(tbl); return() }
      map_rv(tbl)
    }, ignoreInit = TRUE)

    # ── Mode parse_labels : déduction depuis les noms d'échantillons ─────
    observeEvent(input$btn_parse, {
      tbl <- map_rv()
      if (!nrow(tbl)) {
        showNotification(.tr("Aucun échantillon à traiter — importez d'abord un objet SC."),
                         type = "warning", duration = 6)
        return()
      }
      tbl <- tryCatch(
        sc_metadata_parse_sample_table(tbl, cond_position = input$cond_position %||% "first"),
        error = function(e) e)
      if (inherits(tbl, "sc_metadata_error")) { .notify_err(tbl); return() }
      map_rv(tbl)
      showNotification(.tr("Conditions/réplicats déduits des noms — vérifiez et éditez la table si besoin."),
                       type = "message", duration = 5)
    })

    # ── Table éditable (DT cell edit) ────────────────────────────────────
    output$map_dt <- DT::renderDT({
      ts_datatable(
        map_rv(), page_length = 10, buttons = FALSE, dom = "t",
        scroll_x = FALSE, editable = "cell",
        extra_options = list(scrollY = "220px",
                             language = list(emptyTable = .tr("Aucun échantillon")))
      )
    })

    map_proxy <- DT::dataTableProxy("map_dt", session)

    observeEvent(input$map_dt_cell_edit, {
      info <- input$map_dt_cell_edit
      req(info)
      tbl <- tryCatch(DT::editData(map_rv(), info, rownames = FALSE), error = function(e) e)
      if (inherits(tbl, "error")) { .notify_err(tbl); return() }
      map_rv(tbl)
      DT::replaceData(map_proxy, map_rv(), rownames = FALSE, resetPaging = FALSE)
    })

    # ── Mode csv : jointure du CSV de design dans la table (aperçu, pas de
    #    commit — le commit reste le bouton unique ci-dessous) ────────────
    observeEvent(input$csv_file, {
      f <- input$csv_file
      req(f)
      tbl <- map_rv()
      if (!nrow(tbl)) {
        showNotification(.tr("Aucun échantillon à traiter — importez d'abord un objet SC."),
                         type = "warning", duration = 6)
        return()
      }
      csv <- tryCatch(sc_metadata_read_csv(f$datapath), sc_metadata_error = function(e) e)
      if (inherits(csv, "sc_metadata_error")) { .notify_err(csv); return() }
      # La jointure valide la couverture des échantillons via meta.data —
      # ici on projette sur la TABLE design (aperçu) : réutiliser la logique
      # en construisant un meta minimal sample_col = sample.
      meta_min <- data.frame(orig.ident = tbl$sample, stringsAsFactors = FALSE)
      joined <- tryCatch(
        sc_metadata_join_csv(meta_min, csv,
                             key_col = if ("sample" %in% colnames(csv)) "sample" else colnames(csv)[1],
                             sample_col = "orig.ident"),
        sc_metadata_error = function(e) e)
      if (inherits(joined, "sc_metadata_error")) { .notify_err(joined); return() }
      hit <- match(tbl$sample, meta_min$orig.ident)
      tbl$condition <- as.character(joined$condition)[hit]
      if ("replicate" %in% colnames(joined)) tbl$replicate <- as.character(joined$replicate)[hit]
      map_rv(tbl)
      showNotification(.tr("CSV de design chargé en aperçu — vérifiez la table puis appliquez."),
                       type = "message", duration = 5)
    })

    # ── Récapitulatif design (avant commit) ──────────────────────────────
    output$design_recap <- renderPrint({
      tbl <- map_rv()
      if (!nrow(tbl)) return(cat(.tr("En attente d'un objet SC.")))
      obj <- global_data$sc_obj
      cells <- NULL
      if (!is.null(obj) && input$sample_col %in% colnames(obj@meta.data)) {
        cells <- table(trimws(as.character(obj@meta.data[[input$sample_col]])))
      }
      res <- sc_metadata_design_recap(tbl, cells_per_sample = cells)
      cat(.tr("Récapitulatif du design (avant application) :\n"))
      print(res$recap, row.names = FALSE)
      if (isTRUE(res$ok)) {
        cat(.tr("✓ Design exploitable pour une analyse A-vs-B.\n"))
      } else {
        cat(.tr("⚠️ Blocages :\n"))
        for (b in res$blockers) cat(" -", b, "\n")
      }
    })

    # ── Commit (le seul point d'écriture — zéro changement sinon) ────────
    observeEvent(input$btn_commit, {
      global_data$language  # i18n
      obj <- global_data$sc_obj
      if (is.null(obj)) {
        showNotification(.tr("Aucun objet SC actif — importez d'abord un jeu."),
                         type = "warning", duration = 6)
        return()
      }
      tbl <- map_rv()
      if (!nrow(tbl)) {
        showNotification(.tr("Table design vide — rien à appliquer."), type = "warning", duration = 6)
        return()
      }
      new_meta <- tryCatch(
        sc_metadata_apply(obj@meta.data, tbl, sample_col = input$sample_col),
        sc_metadata_error = function(e) e)
      if (inherits(new_meta, "sc_metadata_error")) { .notify_err(new_meta); return() }
      obj@meta.data <- new_meta
      global_data$sc_obj <- obj
      n_cond <- length(unique(na.omit(tbl$condition)))
      showNotification(
        sprintf(.tr("✓ Design appliqué : %d condition(s) déclarée(s) sur %d échantillon(s)."),
                n_cond, nrow(tbl)),
        type = "message", duration = 6)
    })
  })
}

# mod_sc_pathways.R  —  Child 6: GO / KEGG / Reactome enrichment
# Step-3.6: auto-remap ENSG→Symbol before enrichment when gene IDs are Ensembl
# Step-3.7: BUG1 fix — pathway_rv (+ the i18n$t("Base de donnees") selector) is now
#   synced from shared_rv$pathway_results / shared_rv$pathway_db, so results
#   written by the auto-pipeline (mod_sc.R) show up here immediately instead
#   of needing a manual i18n$t("Lancer Enrichissement") click (same class of bug as
#   mod_sc_markers.R / mod_sc_corr.R).

# ── Drive (docs/mcp_propagation.md §1.6/§9) ──────────────────────────────────
.SC_PATHWAYS_DRIVE_MODULE <- "sc_pathways"
.SC_PATHWAYS_DRIVE_BUTTON <- "sc-pathways-run_pathway"

# Paramètres FIGÉS (aucun paramètre non déclaré) : valeurs par défaut de l'UI.
.SC_PATHWAYS_DRIVE_INPUTS <- function() {
  list(source = "markers", db = "GOBP", org = "human", pval = 0.05)
}

#' Readiness, in the same order as the human path: an SC object, then the
#' marker table — the DECLARED prerequisite of the frozen "markers" source.
#' Returning the reason (not FALSE) is what lets the poller report `not_ready`
#' instead of dispatching a job that must fail.
.sc_pathways_drive_ready <- function(shared_rv, global_data) {
  if (is.null(tryCatch(shiny::isolate(global_data$sc_obj), error = function(e) NULL))) {
    return("no SC object loaded (global_data$sc_obj is NULL)")
  }
  md <- tryCatch(shiny::isolate(shared_rv$markers_data), error = function(e) NULL)
  if (is.null(md) || !is.data.frame(md) || !nrow(md)) {
    return("Step 4 has not produced a marker table (shared_rv$markers_data is NULL)")
  }
  TRUE
}

#' State probe. `n_results` is the number of ENRICHED pathways of the last
#' successful run (measured: 414 pathways on the live Phase E run, §9).
.sc_pathways_drive_state <- function(shared_rv, global_data, run_state,
                                     last_step = NULL) {
  job <- tryCatch(ts_drive_job_state(), error = function(e) NULL)
  pending <- tryCatch(ts_drive_job_pending(), error = function(e) NULL)
  ready <- isTRUE(.sc_pathways_drive_ready(shared_rv, global_data))
  obj <- tryCatch(shiny::isolate(global_data$sc_obj), error = function(e) NULL)
  res <- tryCatch(shiny::isolate(shared_rv$pathway_results), error = function(e) NULL)
  n_results <- if (is.data.frame(res)) nrow(res) else 0L
  current <- if (is.function(run_state)) {
    shiny::isolate(run_state())
  } else {
    as.character(run_state)
  }
  if (length(current) != 1L || is.na(current)) current <- "idle"
  step <- if (is.null(last_step)) "skipped" else as.character(last_step)
  if (length(step) != 1L || is.na(step) || !step %in% c("ran", "skipped", "error")) {
    step <- "error"
  }
  list(
    module = .SC_PATHWAYS_DRIVE_MODULE,
    action = "run_pathway",
    status = as.character(if (ready) current else "not_ready"),
    elapsed_s = if (is.null(pending)) 0 else as.numeric(pending$elapsed_s),
    seq = if (is.null(job)) 0L else as.integer(job$seq),
    n_results = as.integer(n_results),
    has_data = !is.null(obj),
    ready = ready,
    steps = list(pathways = step)
  )
}

#' Drive run: the SAME steps in the SAME order as the human observer, on the
#' FROZEN inputs (frozen source = markers), then the SAME two writes
#' (`shared_rv$pathway_results` / `shared_rv$pathway_db`). The record comes
#' back so the module-scoped observer can refresh its local table too.
.sc_pathways_run_drive <- function(global_data, shared_rv, close_job) {
  inp <- .SC_PATHWAYS_DRIVE_INPUTS()
  res <- tryCatch({
    md <- shiny::isolate(shared_rv$markers_data)
    if (is.null(md) || !is.data.frame(md) || !nrow(md)) {
      stop(errorCondition("no marker table — run Step 4 first",
                          class = "sc_pathways_drive_error"))
    }
    genes_to_test <- head(md$gene, 100)
    genes_to_test <- unique(trimws(genes_to_test[nchar(trimws(genes_to_test)) > 0]))
    genes_to_test <- .remap_if_ensg(genes_to_test, inp$org, notify_fn = NULL)
    genes_to_test <- unique(genes_to_test[nchar(genes_to_test) > 0])
    if (length(genes_to_test) < 10) {
      stop(errorCondition(sprintf("too few genes (%d); minimum 10", length(genes_to_test)),
                          class = "sc_pathways_drive_error"))
    }
    out <- run_pathway_enrichment(
      genes = genes_to_test, organism = inp$org, database = inp$db,
      pval_cutoff = inp$pval,
      universe = rownames(shiny::isolate(global_data$sc_obj)))
    if (is.data.frame(out) && nrow(out) == 0) {
      stop(errorCondition("no enriched pathway", class = "sc_pathways_drive_error"))
    }
    out
  }, error = function(e) e)
  if (inherits(res, "condition")) {
    close_job("error", conditionMessage(res))
    return(list(status = "error", n_results = 0L, step = "error", record = NULL))
  }
  shiny::isolate(shared_rv$pathway_results <- res)
  shiny::isolate(shared_rv$pathway_db <- inp$db)
  n_results <- nrow(res)
  close_job("done", NULL)
  list(status = if (n_results == 0L) "empty" else "done",
       n_results = as.integer(n_results), step = "ran", record = res)
}

# ── Helper: remap ENSG IDs → Symbols if detected ─────────────────────────────
.remap_if_ensg <- function(genes, organism = "human", notify_fn = NULL) {
  id_type <- tryCatch(detect_gene_id_type(genes), error = function(e) "unknown")
  if (id_type != "ensembl") return(genes)

  orgdb_pkg <- if (organism == "human") "org.Hs.eg.db" else "org.Mm.eg.db"
  if (!requireNamespace("AnnotationDbi", quietly = TRUE) ||
      !requireNamespace(orgdb_pkg,       quietly = TRUE)) {
    if (!is.null(notify_fn))
      notify_fn(paste0(.tr_plain("⚠️ IDs ENSEMBL détectés mais "), orgdb_pkg,
                       .tr_plain(" non installé — enrichissement peut échouer.")), type = "warning")
    return(genes)
  }

  orgdb     <- getExportedValue(orgdb_pkg, orgdb_pkg)
  ids_clean <- gsub("\\.[0-9]+$", "", genes)   # strip version suffix
  sym <- tryCatch(
    AnnotationDbi::mapIds(orgdb, keys = unique(ids_clean),
                          keytype = "ENSEMBL", column = "SYMBOL", multiVals = "first"),
    error = function(e) NULL
  )
  if (is.null(sym)) return(genes)

  mapped <- as.character(sym[!is.na(sym) & nchar(sym) > 0])
  if (length(mapped) == 0) return(genes)

  if (!is.null(notify_fn))
    notify_fn(sprintf("ℹ️ %d IDs ENSEMBL convertis en symboles avant enrichissement.", length(mapped)),
              type = "message", duration = 5)
  mapped
}

# ── UI ────────────────────────────────────────────────────────────────────────

mod_sc_pathways_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class="alert alert-light",style="font-size:0.9em;border-left:3px solid #9B59B6;",
        i18n$t("Analyse d'enrichissement de voies biologiques."),
        tags$br(),
        tags$small(i18n$t("Les IDs Ensembl (ENSG…) sont convertis automatiquement en symboles."))),
    selectInput(ns("pathway_source"), i18n$t("Source de genes"),
      choices=setNames(c("markers","correlated","manual"), c(.tr_plain("Marqueurs calculés"), .tr_plain("Genes correles"), .tr_plain("Selection manuelle")))),
    conditionalPanel(condition="input.pathway_source == 'manual'", ns=ns,
      selectizeInput(ns("pathway_genes"), "Genes", choices=NULL, multiple=TRUE,
                     options=list(placeholder="Selectionnez genes"))),
    fluidRow(
      column(6, selectInput(ns("pathway_db"), i18n$t("Base de donnees"),
               choices=setNames(c("GOBP","KEGG","Reactome"), c(.tr_plain("GO Biological Process"), .tr_plain("KEGG Pathways"), .tr_plain("Reactome"))))),
      column(6, selectInput(ns("pathway_org"), i18n$t("Organisme"),
               choices=setNames(c("human","mouse"), c(.tr_plain("Humain"), .tr_plain("Souris")))))
    ),
    numericInput(ns("pathway_pval"), i18n$t("P-value cutoff"), value=0.05, min=0.001, max=0.1, step=0.01),
    actionButton(ns("run_pathway"), i18n$t("Lancer Enrichissement"), class="btn-warning w-100", icon=icon("dna")),
    hr(),
    downloadButton(ns("dl_pathway"), "Export CSV", class="btn-sm btn-info w-100"),
    hr(),
    div(class="small text-muted", textOutput(ns("pathway_status")))
  )
}

mod_sc_pathways_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen=TRUE,
    card_header(div(style="display:flex;justify-content:space-between;align-items:center;",
                    h5("Pathway Enrichment", class="mb-0"),
                    downloadButton(ns("dl_pathway_header"), "Export", class="btn-sm btn-info"))),
    navset_tab(
      nav_panel(i18n$t("Barplot Top 15"), plotOutput(ns("pathway_barplot"), height="500px")),
      nav_panel(i18n$t("Dotplot"),        plotOutput(ns("pathway_dotplot"), height="500px")),
      # STAT-S2 — réseau d'enrichissement (descriptif : similarité de gènes / appartenance)
      nav_panel(i18n$t("Réseau"),         uiOutput(ns("network_ui"))),
      nav_panel(i18n$t("Table"),          DTOutput(ns("pathway_table")))
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

mod_sc_pathways_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    # OBLIGATOIRE : `ns` n'est lie que dans les fonctions UI (NS(id)), pas dans
    # le serveur. Sans cette ligne, un renderUI qui appelle ns() leve
    # « impossible de trouver la fonction "ns" » — et seulement quand la branche
    # s'affiche, donc invisible au demarrage. Garde : test-release-hardening.R.
    ns <- session$ns

    # ── i18n proxy ──────────────────────────────────────────────────────────
    .tr <- function(key) {
      tr <- isolate(global_data$i18n)
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }


    pathway_rv <- reactiveVal(NULL)

    observeEvent(global_data$sc_obj, {
      req(global_data$sc_obj)
      updateSelectizeInput(session, "pathway_genes",
                           choices=rownames(global_data$sc_obj), server=TRUE)
    })

    observeEvent(global_data$language, {
      updateSelectInput(session, "pathway_source",
        label = .tr("Source de genes"),
        choices = setNames(
          c("markers", "correlated", "manual"),
          c(.tr("Marqueurs calculés"), .tr("Genes correles"), .tr("Selection manuelle"))
        ),
        selected = isolate(input$pathway_source) %||% "markers"
      )
      updateSelectInput(session, "pathway_db",
        label = .tr("Base de donnees"),
        choices = setNames(
          c("GOBP", "KEGG", "Reactome"),
          c(.tr("GO Biological Process"), .tr("KEGG Pathways"), .tr("Reactome"))
        ),
        selected = isolate(input$pathway_db) %||% "GOBP"
      )
      updateSelectInput(session, "pathway_org",
        label = .tr("Organisme"),
        choices = setNames(c("human", "mouse"), c(.tr("Humain"), .tr("Souris"))),
        selected = isolate(input$pathway_org) %||% "human"
      )
      updateNumericInput(session, "pathway_pval", label = .tr("P-value cutoff"))
      updateActionButton(session, "run_pathway", label = .tr("Lancer Enrichissement"))
    }, ignoreInit = TRUE)
    # ── Step-3.7 BUG1 fix: sync local table + db selector from shared_rv,
    #    written by either this module's own button OR the auto-pipeline. ──────
    observeEvent(shared_rv$pathway_results, {
      pathway_rv(shared_rv$pathway_results)
    }, ignoreNULL = FALSE)

    observeEvent(shared_rv$pathway_db, {
      req(shared_rv$pathway_db)
      updateSelectInput(session, "pathway_db", selected = shared_rv$pathway_db)
    }, ignoreInit = TRUE)

    observeEvent(input$run_pathway, {
      req(global_data$sc_obj)
      genes_to_test <- NULL

      if (input$pathway_source == "markers") {
        df <- shared_rv$markers_data
        if (is.null(df) || nrow(df)==0) {
          showNotification(.tr("⚠️ Lancez d'abord l'étape 4 (Marqueurs)."), type="warning"); return()
        }
        genes_to_test <- head(df$gene, 100)

      } else if (input$pathway_source == "correlated") {
        df <- shared_rv$correlated_genes
        if (is.null(df) || nrow(df)==0) {
          showNotification(.tr("⚠️ Lancez d'abord l'étape 5 (Corrélation)."), type="warning"); return()
        }
        genes_to_test <- c(shared_rv$corr_target_gene, df$gene)

      } else if (input$pathway_source == "manual") {
        req(input$pathway_genes)
        genes_to_test <- input$pathway_genes
      }

      genes_to_test <- unique(trimws(genes_to_test[nchar(trimws(genes_to_test)) > 0]))

      # Step-3.6: auto-remap ENSG → Symbol before bitr
      genes_to_test <- .remap_if_ensg(
        genes_to_test, input$pathway_org,
        notify_fn = function(msg, ...) showNotification(msg, ...)
      )
      genes_to_test <- unique(genes_to_test[nchar(genes_to_test) > 0])

      if (length(genes_to_test) < 10) {
        showNotification(sprintf(.tr("⚠️ Trop peu de gènes (%d). Minimum 10."), length(genes_to_test)),
                         type="warning"); return()
      }

      # add = TRUE : le module DÉCLARE un job drive — un on.exit() nu
      # effacerait la déclaration (garde test-drive-watcher.R §additive).
      p <- shiny::Progress$new(); on.exit(p$close(), add = TRUE)
      p$set(message=.tr("Enrichissement..."), value=0.3)

      tryCatch({
        res <- run_pathway_enrichment(genes=genes_to_test, organism=input$pathway_org,
                                      database=input$pathway_db, pval_cutoff=input$pathway_pval,
                                      universe = rownames(global_data$sc_obj))
        if (nrow(res)==0) {
          showNotification(.tr("ℹ️ Aucun pathway enrichi."), type="warning")
          pathway_rv(NULL); return()
        }
        pathway_rv(res)
        shared_rv$pathway_results <- res
        shared_rv$pathway_db      <- input$pathway_db
        showNotification(paste("✅", nrow(res), "pathways enrichis"), type="message")
        shared_rv$active_tab <- "tab_pathway"
      }, error=function(e) {
        showNotification(paste0(.tr_plain("❌ Erreur pathway: "), as.character(e$message)[1]),
                         type="error", duration=5)
        pathway_rv(NULL)
      })
    })

    # ── Drive (docs/mcp_propagation.md §1.6/§9) : « sc-pathways-run_pathway » ─
    # La source de gènes est un PRÉREQUISITE DÉCLARÉ (la table des marqueurs,
    # portée par la readiness — jamais un paramètre) ; tout le reste tourne sur
    # les valeurs PAR DÉFAUT de l'UI, figées ICI une seule fois (GOBP : la seule
    # base sans package optionnel au-delà de clusterProfiler + org.* — cf.
    # R/core/drive_allowlist.R). Le chemin drive appelle les MÊMES fonctions
    # R/ dans le même ordre que l'observateur humain, puis applique les MÊMES
    # écritures — zéro ré-implémentation, zéro binding DOM.
    sc_pathways_drive_counter <- shiny::reactiveVal(0L)
    sc_pathways_drive_run_state <- shiny::reactiveVal("idle")
    sc_pathways_drive_last_step <- new.env(parent = emptyenv())
    sc_pathways_drive_last_step$value <- NULL
    sc_pathways_drive_job <- new.env(parent = emptyenv())
    sc_pathways_drive_job$id <- NULL

    sc_pathways_drive_ready <- function() {
      .sc_pathways_drive_ready(shared_rv, global_data)
    }
    sc_pathways_drive_state <- function() {
      .sc_pathways_drive_state(shared_rv, global_data,
                               sc_pathways_drive_run_state,
                               sc_pathways_drive_last_step$value)
    }

    close_sc_pathways_drive_job <- function(status, error = NULL) {
      if (is.null(sc_pathways_drive_job$id)) return(invisible(FALSE))
      job <- tryCatch(ts_drive_job_state(), error = function(e) NULL)
      if (is.null(job) || !identical(job$job_id, sc_pathways_drive_job$id)) {
        return(invisible(FALSE))
      }
      ts_drive_job_finish(.SC_PATHWAYS_DRIVE_BUTTON, status = status,
                          error = error)
      sc_pathways_drive_job$id <- NULL
      invisible(TRUE)
    }

    # 🔴 THE BUTTON ID IS A LITERAL HERE, not the constant:
    # test-drive-watcher.R greps every TS_DRIVE_BUTTONS entry as a quoted
    # literal next to a `ts_drive_publish_token(` call, with a readiness guard.
    ts_drive_publish_token(global_data, "sc-pathways-run_pathway",
      sc_pathways_drive_counter, ready = sc_pathways_drive_ready,
      state = sc_pathways_drive_state, long = TRUE)

    observeEvent(sc_pathways_drive_counter(), {
      if (!isTRUE(sc_pathways_drive_ready())) return()
      sc_pathways_drive_job$id <- NULL
      job <- tryCatch(ts_drive_job_state(), error = function(e) NULL)
      if (!is.null(job) && isTRUE(ts_drive_job_busy()) &&
          identical(job$button, .SC_PATHWAYS_DRIVE_BUTTON)) {
        sc_pathways_drive_job$id <- job$job_id
      }
      sc_pathways_drive_run_state("running")
      res <- .sc_pathways_run_drive(global_data, shared_rv,
                                    close_sc_pathways_drive_job)
      sc_pathways_drive_last_step$value <- res$step
      sc_pathways_drive_run_state(res$status)
      if (!is.null(res$record)) pathway_rv(res$record)
    }, ignoreInit = TRUE)

    output$pathway_status <- renderText({
      if (is.null(pathway_rv())) .tr("Aucune analyse en cours")
      else paste("✓", nrow(pathway_rv()), .tr("pathways ["), input$pathway_db, "]")
    })

    output$pathway_barplot <- renderPlot({
      req(pathway_rv())
      plot_pathway_barplot(pathway_rv(), db_label = input$pathway_db, top_n = 15, tr = .tr,
                           palette = shared_rv$sc_palette %||% "default",
                           manual_gradient = shared_rv$sc_manual_gradient)
    })
    output$pathway_dotplot <- renderPlot({
      req(pathway_rv())
      plot_pathway_dotplot(pathway_rv(), db_label = input$pathway_db, top_n = 20, tr = .tr,
                           palette = shared_rv$sc_palette %||% "default",
                           manual_gradient = shared_rv$sc_manual_gradient)
    })
    output$pathway_table <- renderDT({
      req(pathway_rv()); build_pathway_dt(pathway_rv(), tr = .tr)
    })

    # ── STAT-S2 : réseau d'enrichissement (emapplot / cnetplot) ─────────────
    output$network_ui <- renderUI({
      if (is.null(attr(pathway_rv(), "enrich_obj"))) {
        return(div(class="alert alert-light", style="font-size:0.85em;margin:15px;",
                   icon("info-circle"), " ",
                   .tr("Disponible uniquement après une analyse de voies — relancez l'enrichissement (panneau de gauche).")))
      }
      tagList(
        fluidRow(
          column(4, radioButtons(ns("network_mode"), .tr("Type de réseau"),
                                 choices = stats::setNames(
                                   c("emap", "cnet"),
                                   c(.tr("Voies ↔ voies (similarité de gènes)"),
                                     .tr("Voies ↔ gènes"))),
                                 inline = TRUE)),
          column(4, numericInput(ns("network_top_n"), .tr("Voies affichées (réseau)"),
                                 value = 30, min = 2, max = 100, step = 1)),
          column(4, div(style = "margin-top:25px;",
                        checkboxInput(ns("network_interactive"),
                                      .tr("Réseau interactif (survol des nœuds)"),
                                      value = FALSE)))
        ),
        # STAT-S2 V2 : bascule statique (V1) / plotly interactif — défaut =
        # statique, zéro changement de comportement à l'ouverture.
        uiOutput(ns("network_plot_ui"))
      )
    })

    output$network_plot_ui <- renderUI({
      if (isTRUE(input$network_interactive)) {
        plotly::plotlyOutput(ns("network_plot_ly"), height = "560px")
      } else {
        plotOutput(ns("network_plot"), height = "560px")
      }
    })

    output$network_plot <- renderPlot({
      req(pathway_rv())
      top_n <- input$network_top_n
      if (is.null(top_n) || is.na(top_n)) top_n <- 30
      tryCatch(
        plot_pathway_network(pathway_rv(), db_label = shared_rv$pathway_db %||% input$pathway_db,
                             top_n = top_n, mode = input$network_mode, tr = .tr),
        error = function(e) {
          ggplot() +
            annotate("text", x = 1, y = 1, label = paste(.tr("Erreur:"), conditionMessage(e)), color = "red") +
            theme_void()
        }
      )
    })

    output$network_plot_ly <- renderPlotly({
      req(pathway_rv())
      top_n <- input$network_top_n
      if (is.null(top_n) || is.na(top_n)) top_n <- 30
      tryCatch(
        plot_pathway_network_interactive(
          build_pathway_network_data(pathway_rv(), top_n = top_n,
                                     mode = input$network_mode),
          title = paste(.tr("Réseau d'enrichissement"), "-",
                        shared_rv$pathway_db %||% input$pathway_db),
          tr = .tr
        ),
        error = function(e) plotly::plot_ly(type = "scatter", mode = "markers") |>
          plotly::layout(annotations = list(
            text = paste(.tr("Erreur:"), conditionMessage(e)),
            showarrow = FALSE))
      )
    })

    .dl <- function() downloadHandler(
      filename = function() paste0("pathways_", input$pathway_db, "_", Sys.Date(), ".csv"),
      content  = function(file) { req(pathway_rv()); write.csv(pathway_rv(), file, row.names=FALSE) }
    )
    output$dl_pathway        <- .dl()
    output$dl_pathway_header <- .dl()
  })
}

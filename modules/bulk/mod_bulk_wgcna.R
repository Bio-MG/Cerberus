# =============================================================================
# mod_bulk_wgcna.R — Bulk V2 M4 : réseaux de co-expression WGCNA (safe-mode)
# — UI + orchestration.
# =============================================================================
# Logique pure : R/bulk/bulk_wgcna.R (contrat gelé). Deux étapes guidées :
#   1) « Analyse du power » (pickSoftThreshold — plots fit/connectivité) ;
#   2) « Construire les modules » (blockwiseModules, power retenu ou manuel)
#      puis corrélations MEs <-> traits (bicor).
# GARDES mission (gelés côté R/) : N >= 15 (arrêt dur), 2000-5000 gènes
# variables, maxBlockSize borné, threads désactivés, matrice transformée.
# =============================================================================

mod_bulk_wgcna_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-info", style = "font-size:0.8em;",
        icon("info-circle"), " ",
        i18n$t("WGCNA exige au moins 15 \u00e9chantillons et une matrice transform\u00e9e (VST, \u00e9tape 1). Le r\u00e9seau est construit sur 2000\u20135000 g\u00e8nes les plus variables (garde m\u00e9moire 32 Go, TOM born\u00e9).")),
    h6(i18n$t("1. Analyse du power (soft-thresholding)"), style = "font-weight:bold;"),
    sliderInput(ns("wgcna_n_genes"), i18n$t("G\u00e8nes les plus variables"),
                min = TS_BULK_WGCNA_MIN_GENES, max = TS_BULK_WGCNA_MAX_GENES,
                value = TS_BULK_WGCNA_MAX_GENES, step = 500),
    actionButton(ns("run_wgcna_power"), i18n$t("Analyser le power"),
                 class = "btn-warning w-100", icon = icon("chart-line")),
    div(class = "small text-muted mt-1", textOutput(ns("wgcna_power_status"))),

    hr(),
    h6(i18n$t("2. Modules et traits cliniques"), style = "font-weight:bold;"),
    numericInput(ns("wgcna_power_override"), i18n$t("Power (vide = retenu \u00e0 l'\u00e9tape 1)"),
                 value = NA, min = 1, max = 20, step = 1),
    selectizeInput(ns("wgcna_traits"), i18n$t("Traits cliniques (num\u00e9riques / binaires)"),
                   choices = NULL, multiple = TRUE,
                   options = list(placeholder = .tr_placeholder())),
    actionButton(ns("run_wgcna_modules"), i18n$t("Construire les modules"),
                 class = "btn-danger w-100", icon = icon("project-diagram")),
    div(class = "small text-muted mt-1", textOutput(ns("wgcna_status"))),

    hr(),
    downloadButton(ns("dl_wgcna_genes"), i18n$t("Export CSV (g\u00e8nes -> modules)"),
                   class = "btn-sm btn-info w-100"),
    downloadButton(ns("dl_wgcna_rds"), i18n$t("Export RDS (r\u00e9sultats complets)"),
                   class = "btn-sm btn-secondary w-100 mt-2")
  )
}

mod_bulk_wgcna_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE, max_height = "900px",
    card_header("WGCNA"),
    navset_tab(
      id = ns("wgcna_tabs"),
      nav_panel(i18n$t("Power"), plotOutput(ns("wgcna_power_fit"), height = "380px"),
                plotOutput(ns("wgcna_power_conn"), height = "380px")),
      nav_panel(i18n$t("Dendrogramme"), uiOutput(ns("wgcna_dendro_ui"))),
      nav_panel(i18n$t("Modules <-> Traits"), plotOutput(ns("wgcna_trait_plot"), height = "520px")),
      nav_panel(i18n$t("Table modules"), DTOutput(ns("wgcna_module_table")))
    )
  )
}

  mod_bulk_wgcna_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # ── DRIVE (Slice 4) : compteurs, gardes de readiness, sondes ────────────
    # Deux jetons, un par ÉTAPE : la paire est une chaîne de PRÉREQUIS déclaré
    # (l'étape 2 consomme `shared_rv$wgcna_power`), pas un couple
    # run/confirm — le handshake de confirmation du drive concerne les
    # entrées non-bouton injectées et n'est pas touché.
    drive_counter_power   <- shiny::reactiveVal(0L)
    drive_counter_modules <- shiny::reactiveVal(0L)

    # READINESS GUARD (G2) par étape, miroir des préconditions DES observers
    # ci-dessous (mesurées, pas devinées) : sans garde, le dispatch réponde
    # `done` pour un calcul jamais parti — le mensonge coûteux déjà mesuré
    # sur bulk_de. `shiny::isolate()` garde la lecture hors du battement du
    # poller (la réactivité est l'affaire du module).
    drive_ready_power <- function() {
      if (is.null(shiny::isolate(shared_rv$vst_mat))) {
        "no VST matrix (run the bulk Filtering & VST step first)"
      } else TRUE
    }
    drive_ready_modules <- function() {
      if (is.null(shiny::isolate(shared_rv$vst_mat))) {
        "no VST matrix (run the bulk Filtering & VST step first)"
      } else if (is.null(shiny::isolate(shared_rv$wgcna_power))) {
        "no power analysis yet (stage 2 consumes its result: run bulk-wgcna-run_wgcna_power first)"
      } else TRUE
    }

    # STATE PROBE (G3) — COMPTEURS SEULEMENT, chaque lecture isolate-guardée
    # (le probe tourne dans le battement du poller, spec §6 ; rien ici ne
    # renvoie jamais la matrice VST ni les couleurs brutes des modules).
    # « Le jeton a bougé » ne dit pas qu'un power est retenu ni que des
    # modules existent : la sonde rend ces deux faits OBSERVABLES au lieu
    # d'inférés, et la clé `ready` déclare la lisibilité de l'ÉTAPE 2 —
    # l'agent peut observer avant de dispatcher.
    drive_state <- function() {
      pw <- tryCatch(shiny::isolate(shared_rv$wgcna_power), error = function(e) NULL)
      md <- tryCatch(shiny::isolate(shared_rv$wgcna_modules), error = function(e) NULL)
      out <- list(
        ready         = FALSE,
        n_genes       = NULL,
        n_samples     = NULL,
        chosen_power  = NULL,
        chosen_r2     = NULL,
        n_modules     = NULL,
        modules_power = NULL,
        convention    = "power by scale-free fit R2 >= 0.80, best compromise otherwise"
      )
      if (!is.null(pw)) {
        out$ready        <- TRUE
        out$n_genes      <- pw$n_genes_used
        out$n_samples    <- pw$n_samples
        out$chosen_power <- pw$chosen$power
        out$chosen_r2    <- pw$chosen$r2
      }
      if (!is.null(md)) {
        out$n_modules     <- md$n_modules
        out$modules_power <- md$power
      }
      out
    }

    # VOCABULARY PROBE (Slice 3) — le domaine de `wgcna_traits`, publié par
    # INDEX. LA MÊME closure nourrit l'état publiée et la résolution à
    # l'apply (`vocab =`), donc la rev épinglée par l'agent et les choix
    # résolus ne peuvent pas dériver l'un de l'autre. Le filtre est le
    # MIROIR EXACT de l'observer qui alimente les choices du widget
    # (colonnes numériques >= 3 valeurs finies, ou exactement binaires) ;
    # `vocab_rev` est un compteur MONOTONE poussé par comparaison de
    # fingerprint — la sonde est la seule écrivaine, aucun observer
    # existant n'est touché, l'UI humaine ne change pas.
    drive_vocab_state <- new.env(parent = emptyenv())
    drive_vocab_state$rev <- 0L
    drive_vocab_state$fp  <- NULL
    drive_vocabulary <- function() {
      meta <- tryCatch(shiny::isolate(global_data$bulk_obj)$metadata,
                       error = function(e) NULL)
      traits <- character(0)
      if (!is.null(meta) && ncol(meta)) {
        ok <- vapply(names(meta), function(cl) {
          x <- meta[[cl]]
          (is.numeric(x) && sum(is.finite(x)) >= 3L) ||
            ((is.factor(x) || is.character(x) || is.logical(x)) &&
               length(unique(stats::na.omit(as.character(x)))) == 2L)
        }, logical(1))
        traits <- names(meta)[ok]
      }
      fp <- paste(c(length(traits), traits), collapse = "\u0001")
      if (!identical(fp, drive_vocab_state$fp)) {
        drive_vocab_state$rev <- drive_vocab_state$rev + 1L
        drive_vocab_state$fp  <- fp
      }
      list(
        traits    = traits,
        vocab_rev = drive_vocab_state$rev
      )
    }
    # La vocabulaire voyage DANS l'état (canal wire), projetée par
    # ts_drive_project_vocabulary (choix D1-verbatim, keep-set fermé).
    drive_state2 <- function() {
      s <- drive_state()
      s$vocabulary <- drive_vocabulary()
      s
    }

    drive_trigger_power   <- shiny::reactive(list(drive_counter_power(),   input$run_wgcna_power))
    drive_trigger_modules <- shiny::reactive(list(drive_counter_modules(), input$run_wgcna_modules))

    ts_drive_publish_token(global_data, "bulk-wgcna-run_wgcna_power", drive_counter_power,
                           ready = drive_ready_power, state = drive_state2, long = TRUE,
                           vocab = drive_vocabulary)
    ts_drive_publish_token(global_data, "bulk-wgcna-run_wgcna_modules", drive_counter_modules,
                           ready = drive_ready_modules, state = drive_state2, long = TRUE,
                           vocab = drive_vocabulary)

    # ── i18n push on language switch ─────────────────────────────────────
    observeEvent(global_data$language, {
      updateSliderInput(session, "wgcna_n_genes", label = .tr("Gènes les plus variables"))
      updateActionButton(session, "run_wgcna_power", label = .tr("Analyser le power"))
      updateNumericInput(session, "wgcna_power_override",
                         label = .tr("Power (vide = retenu à l'étape 1)"))
      updateSelectizeInput(session, "wgcna_traits",
                           label = .tr("Traits cliniques (numériques / binaires)"))
      updateActionButton(session, "run_wgcna_modules", label = .tr("Construire les modules"))
      updateActionButton(session, "dl_wgcna_genes", label = .tr("Export CSV (gènes -> modules)"))
      updateActionButton(session, "dl_wgcna_rds", label = .tr("Export RDS (résultats complets)"))
    }, ignoreInit = TRUE)

    # Traits candidats : colonnes numériques ou binaires des métadonnées
    # (les multi-niveaux sont écartés par bulk_wgcna_prepare_traits — on ne
    # les propose même pas, avec l'explication en helpText du rejet).
    observeEvent(global_data$bulk_obj, {
      req(global_data$bulk_obj$metadata)
      meta <- global_data$bulk_obj$metadata
      ok <- vapply(names(meta), function(cl) {
        x <- meta[[cl]]
        (is.numeric(x) && sum(is.finite(x)) >= 3L) ||
          (is.factor(x) || is.character(x) || is.logical(x)) &&
            length(unique(stats::na.omit(as.character(x)))) == 2L
      }, logical(1))
      updateSelectizeInput(session, "wgcna_traits", choices = names(meta)[ok],
                           server = FALSE)
    }, ignoreNULL = TRUE)

    observe({
      shinyjs::toggleState("run_wgcna_power",   condition = !is.null(shared_rv$vst_mat))
      shinyjs::toggleState("run_wgcna_modules", condition = !is.null(shared_rv$wgcna_power))
      shinyjs::toggleState("dl_wgcna_genes", condition = !is.null(shared_rv$wgcna_modules))
      shinyjs::toggleState("dl_wgcna_rds",
                           condition = !is.null(shared_rv$wgcna_power) ||
                             !is.null(shared_rv$wgcna_modules))
    })

    output$wgcna_power_status <- renderText({
      global_data$language
      pw <- shared_rv$wgcna_power
      if (is.null(pw)) .tr("En attente — lancez d'abord le Filtrage & VST (étape 1).")
      else .t_fmt(.tr("\u2713 Power retenu : {p} (R2 = {r}) — {g} g\u00e8nes, {n} \u00e9chantillons."),
                  p = format(pw$chosen$power), r = round(pw$chosen$r2, 3),
                  g = format(pw$n_genes_used, big.mark = ","), n = pw$n_samples)
    })
    output$wgcna_status <- renderText({
      global_data$language
      md <- shared_rv$wgcna_modules
      if (is.null(md)) .tr("Étape 2 en attente — analyse du power d'abord.")
      else .t_fmt(.tr("\u2713 {k} modules d\u00e9tect\u00e9s sur {g} g\u00e8nes (power = {p})."),
                  k = md$n_modules, g = format(md$n_genes_used, big.mark = ","),
                  p = format(md$power))
    })

    # ── Étape 1 : power analysis ─────────────────────────────────────────
    observeEvent(drive_trigger_power(), {
      # ── DECLARE THE JOB OVER (drive job contract, spec §5) ────────────────
      # Ce jeton est DÉCLARÉ long (pickSoftThreshold est synchrone et long) :
      # il doit fermer son job sur TOUTE sortie, y compris les `req()` qui
      # avortent en silence. L'on.exit est enregistré AVANT eux — c'est lui
      # qui transforme un clic à sec en verdict `invalid` honnête au lieu
      # d'un `running` sans fin (la panne la plus coûteuse du contrat long).
      outcome <- new.env(parent = emptyenv())
      outcome$v <- "refused"
      if (isTRUE(ts_drive_job_busy()) &&
          identical(ts_drive_job_state()$button, "bulk-wgcna-run_wgcna_power")) {
        # F4 (2026-10-06) : la raison du refus voyage sur le wire.
        on.exit(ts_drive_job_finish("bulk-wgcna-run_wgcna_power",
          status = switch(outcome$v, ok = "done", failed = "error", "invalid"),
          error = if (identical(outcome$v, "failed")) {
            "the power analysis raised — see the app notification"
          } else outcome$reason
        ), add = TRUE)
      }

      req(input$run_wgcna_power > 0 || shiny::isolate(drive_counter_power()) > 0)
      # F4 : un `req()` avorte en silence — la même absence, énoncée, devient
      # la raison du verdict `invalid` (cf. drive_ready_power, même miroir).
      if (is.null(shiny::isolate(shared_rv$vst_mat))) {
        outcome$reason <- "no VST matrix yet: run step 1 first"
        return(invisible(NULL))
      }
      p <- shiny::Progress$new(); on.exit(p$close(), add = TRUE)
      p$set(message = .tr("Analyse du power (soft-thresholding)..."), value = 0.2)
      tryCatch({
        pw <- bulk_wgcna_pick_power(shared_rv$vst_mat, n_top = input$wgcna_n_genes)
        shared_rv$wgcna_power <- pw
        # Stockage récapitulatif côté bulk_obj (traçabilité de session).
        bo <- global_data$bulk_obj
        if (!is.list(bo$wgcna)) bo$wgcna <- list()
        bo$wgcna$power <- list(chosen_power = pw$chosen$power,
                               r2 = pw$chosen$r2, n_genes_used = pw$n_genes_used)
        bo <- bulk_ensure_provenance(bo)
        global_data$bulk_obj <- bo
        if (!isTRUE(pw$chosen$target_reached)) {
          showNotification(.tr(paste0("\u26a0\ufe0f Aucun power n'atteint R2 = 0.80 — le meilleur ",
                                       "compromis est retenu (voir sous-titre du graphique).")),
                           type = "warning", duration = 8)
        }
        showNotification(.t_fmt(.tr("\u2713 Power retenu : {p} (R2 = {r})."),
                                 p = format(pw$chosen$power), r = round(pw$chosen$r2, 3)),
                         type = "message")
        nav_select(id = "wgcna_tabs", selected = "wgcna_power_tab", session = session)
        outcome$v <- "ok"
      }, error = function(e) {
        showNotification(paste(.tr("Erreur WGCNA:"), conditionMessage(e)),
                         type = "error", duration = 10)
        shared_rv$wgcna_power <- NULL
        outcome$v <- "failed"
      })
    })

    # ── Étape 2 : modules + traits ───────────────────────────────────────
    # ⚠️ PRÉREQUIS DÉCLARÉ : cette étape consomme `shared_rv$wgcna_power`
    # (l'étape 1). Sans étape 1, le dispatch est refusé par la garde de
    # readiness AVANT tout feu ; si l'état change entre le dispatch et le
    # battement (fenêtre TOCTOU), l'on.exit du contrat long transforme le
    # `req()` avorté en verdict `invalid` — jamais un `running` sans fin.
    observeEvent(drive_trigger_modules(), {
      # ── DECLARE THE JOB OVER (drive job contract, spec §5) ────────────────
      # Même contrat que l'observateur du power ci-dessus : blockwiseModules
      # est synchrone et long, le job doit être fermé sur TOUTE sortie.
      outcome <- new.env(parent = emptyenv())
      outcome$v <- "refused"
      if (isTRUE(ts_drive_job_busy()) &&
          identical(ts_drive_job_state()$button, "bulk-wgcna-run_wgcna_modules")) {
        # F4 (2026-10-06) : la raison du refus voyage sur le wire.
        on.exit(ts_drive_job_finish("bulk-wgcna-run_wgcna_modules",
          status = switch(outcome$v, ok = "done", failed = "error", "invalid"),
          error = if (identical(outcome$v, "failed")) {
            "the module construction raised — see the app notification"
          } else outcome$reason
        ), add = TRUE)
      }

      req(input$run_wgcna_modules > 0 || shiny::isolate(drive_counter_modules()) > 0)
      # F4 : les prérequis de l'étape 2, énoncés (miroir de drive_ready_modules)
      # au lieu d'un `req()` muet — le verdict `invalid` dit alors POURQUOI.
      if (is.null(shiny::isolate(shared_rv$vst_mat)) ||
          is.null(shiny::isolate(shared_rv$wgcna_power))) {
        outcome$reason <- "needs step 1: a VST matrix and the power result; run the power step first"
        return(invisible(NULL))
      }
      power <- if (!is.na(input$wgcna_power_override) && !is.null(input$wgcna_power_override)) {
        input$wgcna_power_override
      } else shared_rv$wgcna_power$chosen$power
      p <- shiny::Progress$new(); on.exit(p$close(), add = TRUE)
      p$set(message = .tr("Construction des modules (blockwiseModules)..."), value = 0.2)
      tryCatch({
        md <- bulk_wgcna_build_modules(shared_rv$vst_mat, power = power,
                                       n_top = input$wgcna_n_genes)
        shared_rv$wgcna_modules <- md
        bo <- global_data$bulk_obj
        if (!is.list(bo$wgcna)) bo$wgcna <- list()
        bo$wgcna$modules <- list(power = md$power, n_modules = md$n_modules,
                                 module_sizes = as.list(md$module_sizes))
        bo <- bulk_ensure_provenance(bo)
        global_data$bulk_obj <- bo
        showNotification(.t_fmt(.tr("\u2713 {k} modules d\u00e9tect\u00e9s."),
                                 k = md$n_modules), type = "message")
        nav_select(id = "wgcna_tabs", selected = "wgcna_trait_tab", session = session)
        outcome$v <- "ok"
      }, error = function(e) {
        showNotification(paste(.tr("Erreur WGCNA:"), conditionMessage(e)),
                         type = "error", duration = 10)
        shared_rv$wgcna_modules <- NULL
        outcome$v <- "failed"
      })
    })

    # ── Corrélations MEs <-> traits (recalculées à chaque sélection) ─────
    wgcna_trait_cor <- reactive({
      req(shared_rv$wgcna_modules)
      tr_sel <- input$wgcna_traits
      prep <- bulk_wgcna_prepare_traits(global_data$bulk_obj$metadata,
                                        candidate_cols = if (length(tr_sel)) tr_sel else NULL)
      bulk_wgcna_module_trait(shared_rv$wgcna_modules, prep$traits)
    }) %>% shiny::debounce(500)

    # ── Outputs ──────────────────────────────────────────────────────────
    output$wgcna_power_fit <- renderPlot({
      global_data$language
      req(shared_rv$wgcna_power)
      plots <- plot_wgcna_soft_threshold(shared_rv$wgcna_power, tr = .tr_fn(global_data))
      print(plots$fit)
    })
    output$wgcna_power_conn <- renderPlot({
      global_data$language
      req(shared_rv$wgcna_power)
      plots <- plot_wgcna_soft_threshold(shared_rv$wgcna_power, tr = .tr_fn(global_data))
      print(plots$connectivity)
    })
    output$wgcna_dendro_ui <- renderUI({
      global_data$language
      if (is.null(shared_rv$wgcna_modules)) {
        return(div(class = "alert alert-light", style = "font-size:0.85em;margin:15px;",
                   icon("info-circle"), " ",
                   .tr("Construisez d'abord les modules (étape 2 du panneau de gauche).")))
      }
      plotOutput(ns("wgcna_dendro"), height = "700px")
    })
    output$wgcna_dendro <- renderPlot({
      global_data$language
      md <- shared_rv$wgcna_modules; req(md)
      # Dendrogramme + bande de couleurs — dessin WGCNA (base graphics).
      tryCatch({
        cols <- md$dendro_colors %||% md$colors
        WGCNA::plotDendroAndColors(
          md$dendro, cols, groupLabels = .tr("Modules"),
          dendroLabels = FALSE, cex.dendroLabels = 0.6,
          cex.colorLabels = 0.8, marAll = c(2, 5, 2, 2))
      }, error = function(e) {
        plot.new(); text(0.5, 0.5, paste(.tr("Erreur:"), conditionMessage(e)), cex = 0.9)
      })
    })
    output$wgcna_trait_plot <- renderPlot({
      global_data$language
      md <- shared_rv$wgcna_modules; req(md)
      tryCatch(
        print(plot_wgcna_trait_heatmap(wgcna_trait_cor(), tr = .tr_fn(global_data))),
        error = function(e) {
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 1, y = 1,
                              label = paste(.tr("Erreur:"), conditionMessage(e)), color = "red") +
            ggplot2::theme_void()
        }
      )
    })
    output$wgcna_module_table <- renderDT({
      global_data$language
      md <- shared_rv$wgcna_modules; req(md)
      sizes <- as.data.frame(md$module_sizes)
      colnames(sizes) <- c("Module", "Gènes")
      sizes <- sizes[order(sizes$Module), ]
      ts_datatable(sizes, page_length = 15L,
                   filename_base = "wgcna_module_sizes", filter = "none")
    })

    output$dl_wgcna_genes <- downloadHandler(
      filename = function() paste0("wgcna_gene_modules_", Sys.Date(), ".csv"),
      content  = function(file) {
        md <- shared_rv$wgcna_modules; req(md)
        tc <- tryCatch(wgcna_trait_cor(), error = function(e) NULL)
        write.csv(build_wgcna_export(md, tc), file, row.names = FALSE)
      }
    )
    output$dl_wgcna_rds <- downloadHandler(
      filename = function() paste0("wgcna_results_", Sys.Date(), ".rds"),
      content  = function(file) {
        out <- list(power = shared_rv$wgcna_power, modules = shared_rv$wgcna_modules,
                    trait_cor = tryCatch(wgcna_trait_cor(), error = function(e) NULL),
                    timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
        saveRDS(out, file)
      }
    )

    # ── EXPORT ROUTE (Slice 4) — la table gène -> module ──────────────────
    # Le builder est EXACTEMENT celui du téléchargement humain
    # `dl_wgcna_genes` ci-dessus (règle S2 : jamais un second builder) ; la
    # clôture passe la reactive des corrélations isolée — le poller appelle
    # l'exporteur hors de tout contexte réactif. Les verdicts (rien à
    # exporter / résultat non canonique / écriture) vivent dans
    # mod_bulk_wgcna_export.R, atteignable hors Shiny par les tests.
    ts_drive_publish_export(global_data, "bulk_wgcna", function() {
      bulk_wgcna_export_genes_csv(
        shared_rv, global_data,
        trait_cor = tryCatch(shiny::isolate(wgcna_trait_cor()),
                             error = function(e) NULL))
    })

  }) # /moduleServer
}

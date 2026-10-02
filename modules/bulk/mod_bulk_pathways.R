# mod_bulk_pathways.R — Bulk Child 3 (i18n Phase 3.1)

mod_bulk_pathways_ui <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("enrich_mode"), i18n$t("M\u00e9thode d'enrichissement"),
                choices = stats::setNames(c("ora","gsea"),
                  c(.tr_plain("ORA \u2014 sur g\u00e8nes significatifs (classique)"),
                    .tr_plain("GSEA \u2014 sur tous les g\u00e8nes class\u00e9s (sans seuil)"))),
                selected = "ora"),
    div(class = "small text-muted mb-2",
        i18n$t("GSEA n'a pas besoin de seuil de significativit\u00e9 arbitraire \u2014 elle classe tous les g\u00e8nes par Log2FC et teste l'enrichissement cumul\u00e9. Plus robuste statistiquement, recommand\u00e9e si peu de g\u00e8nes passent vos seuils.")),

    conditionalPanel(
      condition = "input.enrich_mode == 'ora'", ns = ns,
      selectInput(ns("pathway_source"), i18n$t("Source de g\u00e8nes"),
                  choices = stats::setNames(c("up","down","all_sig","manual"),
                    c(.tr_plain("G\u00e8nes Up (significatifs)"), .tr_plain("G\u00e8nes Down (significatifs)"),
                      .tr_plain("Tous g\u00e8nes significatifs"), .tr_plain("S\u00e9lection manuelle")))),
      conditionalPanel(
        condition = "input.pathway_source == 'manual'", ns = ns,
        selectizeInput(ns("pathway_genes"), i18n$t("G\u00e8nes"), choices = NULL, multiple = TRUE)
      )
    ),

    fluidRow(
      column(6, selectInput(ns("pathway_db"), i18n$t("Base de donn\u00e9es"),
                            choices = c("GO Biological Process" = "GOBP",
                                        "KEGG Pathways"         = "KEGG",
                                        "Reactome"              = "Reactome"))),
      column(6, selectInput(ns("pathway_org"), i18n$t("Organisme"),
                            choices = stats::setNames(c("human","mouse"),
                              c(.tr_plain("Humain"), .tr_plain("Souris")))))
    ),
    numericInput(ns("pathway_pval"), i18n$t("P-value cutoff"), value = 0.05, min = 0.001, max = 0.1, step = 0.01),

    # STAT-Q1 — correction pour tests multiples de l'enrichissement. Choix
    # déclarés dans config/defaults.R (TS_PADJ_METHODS) ; "fdr" est
    # volontairement absent (alias de BH dans stats::p.adjust.methods).
    selectInput(ns("pathway_padj_method"), i18n$t("M\u00e9thode de correction (p-adj)"),
                choices  = stats::setNames(TS_PADJ_METHODS, TS_PADJ_METHODS),
                selected = TS_PADJ_METHOD_DEFAULT),

    actionButton(ns("run_pathway"), i18n$t("Lancer Enrichissement"),
                 class = "btn-warning w-100", icon = icon("dna")),

    downloadButton(ns("dl_pathway"), i18n$t("Export CSV"), class = "btn-sm btn-info w-100 mt-2"),

    # STAT-Q4 — meme table, en Excel. Le CSV ci-dessus reste inchange ; le repli
    # CSV du helper n'est utilise que si openxlsx est absent.
    downloadButton(ns("dl_pathway_excel"), i18n$t("Export Excel"), class = "btn-sm btn-success w-100 mt-2"),

    div(class = "small text-muted mt-1", textOutput(ns("pathway_status"))),

    # ── Bulk V2 M2 — scores de voies PAR ÉCHANTILLON (GSVA/ssGSEA/PLAGE/zscore).
    # Complément de l'ORA/GSEA ci-dessus : attribue un score par voie à CHAQUE
    # échantillon (aucun contraste requis). La matrice (voies x échantillons)
    # vit dans bulk_obj$pathways$per_sample + shared_rv$pathway_scores.
    hr(),
    h6(i18n$t("Scores par \u00e9chantillon (GSVA / ssGSEA)"), style = "font-weight:bold;"),
    div(class = "small text-muted mb-2",
        i18n$t("Attribue \u00e0 chaque \u00e9chantillon un score par voie \u2014 PCA et heatmaps par voie m\u00eame sans contraste. Exige la matrice VST (\u00e9tape 1). Les jeux de g\u00e8nes viennent d'une source NATIVE embarqu\u00e9e (MSigDB, PROGENy, DoRothEA \u2014 aucun fichier \u00e0 fournir) ou d'un .gmt fourni. Les jeux dont moins de 20 % des g\u00e8nes sont retrouv\u00e9s sont rejet\u00e9s (d\u00e9calage d'identifiants).")),
    # PLOT-S6b — source NATIVE des jeux de gènes (bulk_gene_sets.R). Le .gmt
    # reste disponible pour les jeux maison : le fileInput n'apparaît que pour
    # cette source (conditionalPanel), il n'est plus un prérequis.
    selectInput(ns("scores_source"), i18n$t("Source de jeux de g\u00e8nes"),
                choices  = bulk_gene_set_choices(.tr_plain),
                selected = "msigdb_hallmark"),
    selectInput(ns("scores_org"), i18n$t("Organisme"),
                choices = stats::setNames(c("human", "mouse"),
                  c(.tr_plain("Humain"), .tr_plain("Souris"))),
                selected = "human"),
    conditionalPanel(
      condition = "input.scores_source == 'file'", ns = ns,
      fileInput(ns("scores_gmt"), i18n$t("Fichier de jeux de g\u00e8nes (.gmt)"),
                accept = c(".gmt", ".txt"), buttonLabel = i18n$t("Parcourir..."))
    ),
    div(class = "small text-muted mb-2", textOutput(ns("scores_source_hint"))),
    selectInput(ns("scores_method"), i18n$t("M\u00e9thode de scoring"),
                choices = stats::setNames(c("ssgsea", "gsva", "plage", "zscore"),
                  c("ssGSEA", "GSVA", "PLAGE", "zscore")),
                selected = "ssgsea"),
    fluidRow(
      column(6, numericInput(ns("scores_min_size"), i18n$t("Taille min voie"),
                            value = TS_BULK_GSVA_MIN_SIZE, min = 1, step = 1)),
      column(6, numericInput(ns("scores_max_size"), i18n$t("Taille max voie"),
                            value = TS_BULK_GSVA_MAX_SIZE, min = 2, step = 1))
    ),
    actionButton(ns("run_scores"), i18n$t("Lancer Scores par \u00e9chantillon"),
                 class = "btn-warning w-100", icon = icon("layer-group")),
    # Les jeux natifs restent exportables en .gmt : reproductibilité hors
    # application, et réutilisation dans un autre outil.
    downloadButton(ns("dl_scores_gmt"), i18n$t("Exporter les jeux de g\u00e8nes (.gmt)"),
                   class = "btn-sm btn-secondary w-100 mt-2"),
    div(class = "small text-muted mt-1", textOutput(ns("scores_status")))
  )
}

mod_bulk_pathways_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE, max_height = "900px",
    card_header("Pathway Enrichment"),
    navset_tab(
      id = ns("pathways_tabs"),
      nav_panel(i18n$t("Barplot Top 15"), plotOutput(ns("pathway_barplot"), height = "580px")),
      nav_panel(i18n$t("Dotplot"),        plotOutput(ns("pathway_dotplot"), height = "580px")),
      # STAT-S2 — réseau d'enrichissement (descriptif : similarité de gènes / appartenance)
      nav_panel(i18n$t("Réseau"),         uiOutput(ns("network_ui"))),
      nav_panel(i18n$t("Table"),          DTOutput(ns("pathway_table"))),
      nav_panel(i18n$t("Courbe GSEA"),    uiOutput(ns("gsea_curve_ui"))),
      # Bulk V2 M2 — scores par échantillon (voies x échantillons)
      nav_panel(i18n$t("Scores par \u00e9chantillon"), value = "tab_scores",
        uiOutput(ns("scores_dropped_ui")),
        fluidRow(
          column(4, selectizeInput(ns("scores_color_by"), i18n$t("Colorer la PCA par"),
                                   choices = NULL,
                                   options = list(placeholder = .tr_placeholder(), allowEmptyOption = TRUE))),
          column(4, numericInput(ns("scores_heat_top_n"), i18n$t("Voies affichées (heatmap)"),
                                 value = 50, min = 5, max = 500, step = 5))
        ),
        plotOutput(ns("scores_pca"), height = "420px"),
        hr(),
        plotOutput(ns("scores_heatmap"), height = "620px"),
        hr(),
        fluidRow(
          column(6, downloadButton(ns("dl_scores_csv"), i18n$t("Export CSV (scores)"),
                                   class = "btn-sm btn-info w-100")),
          column(6, downloadButton(ns("dl_scores_rds"), i18n$t("Export RDS (résultat complet)"),
                                   class = "btn-sm btn-secondary w-100"))
        ),
        DTOutput(ns("scores_table"))
      )
    )
  )
}

mod_bulk_pathways_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── DRIVE LIVE CONTROL (docs/DRIVE_LIVE_CONTROL_PLAN.md, grade G2) ─────
    # Two click sites in this module are fired by the file-drop poller through
    # counters it publishes in the session-scoped registry. `updateActionButton()`
    # does NOT click; these second triggers do.
    drive_counter_pathway <- shiny::reactiveVal(0L)
    drive_counter_scores  <- shiny::reactiveVal(0L)
    # READINESS GUARDS (G2). Each observer below opens with its own `req()` —
    # `shared_rv$filtered_counts` for the enrichment, `shared_rv$vst_mat` for
    # the per-sample scores — and `req()` aborts in SILENCE. Without a guard
    # the poller would fire the token, the observer would quietly do nothing,
    # and `result.json` would report `done` for an analysis that never ran.
    # Each guard mirrors its OWN observer's precondition, and
    # `shiny::isolate()` keeps the read out of the poller's dependency set.
    ready_pathway <- function() {
      if (is.null(shiny::isolate(shared_rv$filtered_counts))) {
        "no bulk object loaded (shared_rv$filtered_counts is NULL)"
      } else TRUE
    }
    ready_scores <- function() {
      if (is.null(shiny::isolate(shared_rv$vst_mat))) {
        "Step 1 has not produced a VST matrix (shared_rv$vst_mat is NULL)"
      } else TRUE
    }

    # ── STATE PUBLISHER (jalon « pathway state observability », 2026-09-24) ──
    # 🔴 LE DÉFAUT QUE CE PROBE EXISTE POUR TUER, mesuré en session VIVANTE :
    # `run_pipeline` sur `bulk-pathways-run_pathway` répond `done` AUSSI BIEN
    # quand l'enrichissement a produit 2 voies que quand l'observateur sort par
    # son garde silencieux des `< 10` gènes (l. ~471, `return()` nu). `done` est
    # HONNÊTE sur ce qu'il mesure — « le token a bougé » (spec §2.3) — et MUET
    # sur ce qui compte. Preuve de la cécité : au même instant `result.json`
    # portait `status="done"` et `snapshot$modules` ne contenait AUCUNE entrée
    # `bulk_pathways`, alors que le DOM portait la vérité (`✓ 2 pathways…`).
    #
    # Un `done` de PROTOCOLE n'est pas un `done` de MÉTIER. Ce probe publie le
    # second, par la couture EXISTANTE (`state =`, cf. `bulk_filter`/`bulk_de`).
    #
    # ⚠️ CASSETTE, pas lecture de l'objet vivant. Les deux sorties qui comptent
    # ne se distinguent PAS sur `shared_rv$pathway_results` : le garde `< 10`
    # gènes laisse le slot INCHANGÉ (`return()` nu). Sans enregistreur, « a
    # tourné, trop peu de gènes » et « n'a jamais tourné » rendent le même
    # `NULL`. L'enregistreur est donc le plus petit témoin honnête.
    #
    # ⚠️ `running` N'A PAS DE PRODUCTEUR ICI, et n'en aura pas : le job est
    # SYNCHRONE, donc aucun tick ne tourne pendant qu'il s'exécute. Un drapeau
    # posé dans le corps de l'observateur serait INOBSERVABLE — un `running`
    # sans producteur est un MENSONGE, pas une observabilité. Le producteur est
    # le poller lui-même : `long = TRUE` fait écrire `running` au dispatch. Ce
    # probe se contente donc de LIRE l'état du job, il ne l'invente pas.
    #
    # ⚠️ Un ENVIRONNEMENT et non une liste : écrit depuis les observateurs et lu
    # depuis le probe, il doit avoir une sémantique de RÉFÉRENCE (une liste
    # serait copiée à chaque écriture, donc l'écriture serait perdue).
    drive_last <- local({
      d <- new.env(parent = emptyenv())
      d$action    <- NA_character_
      d$status    <- NA_character_
      d$n_results <- NA_integer_
      d$error     <- ""
      d$elapsed_s <- NA_real_
      d$started   <- NULL
      d$seq       <- NULL
      d
    })

    # Enregistre une issue RÉSOLUE. `started` est posé par l'observateur, pas
    # ici : la durée doit mesurer le TRAVAIL, pas le temps écoulé depuis le
    # dernier appel réussi. Ne JAMAIS y écrire `running` — voir plus haut.
    drive_record <- function(action, status, n_results = NA_integer_, error = "") {
      drive_last$action    <- action
      drive_last$status    <- status
      drive_last$n_results <- n_results
      drive_last$error     <- error
      drive_last$elapsed_s <- if (is.null(drive_last$started)) {
        NA_real_
      } else {
        as.numeric(Sys.time()) - drive_last$started
      }
      invisible(NULL)
    }

    # STATE PROBE (G3) — cf. `state` dans `ts_drive_publish_token()`.
    # Scalaire UNIQUEMENT : jamais la matrice de voies, jamais un tracé, jamais
    # la table biologique, jamais un log. Chaque lecture partagée est
    # isolate()-gardée : ce probe tourne dans le battement réactif du poller
    # (spec §6), donc une lecture nue enrôlerait les données du module dans son
    # jeu de dépendances.
    drive_state <- function() {
      out <- list(
        module    = "bulk_pathways",
        action    = NA_character_,
        seq       = NA_integer_,
        status    = "not_ready",
        elapsed_s = NULL,
        n_results = NULL,
        error     = ""
      )
      # 1) Un job en vol est le SEUL producteur de `running` (cf. plus haut).
      job <- ts_drive_job_state()
      if (!is.null(job)) {
        btn <- if (is.null(job$button)) "" else as.character(job$button)[1]
        out$action <- if (nzchar(btn)) sub("^bulk-pathways-", "", btn) else NA_character_
        out$seq    <- if (is.null(job$seq)) NA_integer_ else as.integer(job$seq)
        out$status <- "running"
        if (!is.null(job$started)) {
          out$elapsed_s <- round(as.numeric(Sys.time()) - job$started, 1L)
        }
        return(out)
      }
      # 2) Aucun objet de travail ⇒ `not_ready`, que l'objet n'ait jamais existé
      #    ou qu'il ait disparu. Un statut RÉSOLU ne survit pas à sa disparition :
      #    le module ne peut plus ni rejouer ni défendre ce résultat, donc il ne
      #    doit pas le présenter comme `done`. Les DEUX lectures sont isolées.
      ready <- !is.null(shiny::isolate(shared_rv$filtered_counts)) ||
               !is.null(shiny::isolate(shared_rv$vst_mat))
      st <- drive_last$status
      if (!isTRUE(ready) || is.null(st) || length(st) != 1L || is.na(st)) {
        return(out)
      }
      # 3) Issue RÉSOLUE, telle que l'enregistreur l'a MESURÉE. Liste
      #    RECONSTRUITE : `out$champ <- NULL` SUPPRIMERAIT le champ (mesuré), et
      #    le contrat de champs est fermé.
      return(list(
        module    = "bulk_pathways",
        action    = drive_last$action,
        seq       = if (is.null(drive_last$seq)) NA_integer_ else
                    as.integer(drive_last$seq),
        status    = as.character(st),
        elapsed_s = if (is.null(drive_last$elapsed_s) ||
                        is.na(drive_last$elapsed_s)) NULL else
                    round(as.numeric(drive_last$elapsed_s), 1L),
        n_results = if (is.null(drive_last$n_results) ||
                        is.na(drive_last$n_results)) NULL else
                    as.integer(drive_last$n_results),
        # Le seul champ LIBRE du contrat : il traverse le caviardage du badge
        # (un token ne doit pas fuir dans l'UI), et `known` ne lui est JAMAIS
        # appliqué — c'est là que vit la propriété de sécurité.
        error     = if (identical(as.character(st), "error")) {
          ts_drive_badge_sanitize(drive_last$error, 200L)
        } else ""
      ))
    }

    ts_drive_publish_token(global_data, "bulk-pathways-run_pathway", drive_counter_pathway,
                           ready = ready_pathway, state = drive_state, long = TRUE)
    ts_drive_publish_token(global_data, "bulk-pathways-run_scores",  drive_counter_scores,
                           ready = ready_scores, state = drive_state, long = TRUE)
    drive_trigger_pathway <- shiny::reactive(list(drive_counter_pathway(), input$run_pathway))
    drive_trigger_scores  <- shiny::reactive(list(drive_counter_scores(),  input$run_scores))

    # PLOT-S6b — jeux de gènes réellement chargés (source native ou .gmt).
    # Déclaré ICI, avant tout observe() qui le lit : un observe() évalue son
    # corps immédiatement, donc une définition plus bas serait introuvable.
    scores_sets_rv <- reactiveVal(NULL)

    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # ── i18n push on language switch ─────────────────────────────────────
    observeEvent(global_data$language, {
      updateRadioButtons(session, "enrich_mode", label = .tr("M\u00e9thode d'enrichissement"),
        choices = stats::setNames(c("ora","gsea"),
          c(.tr("ORA \u2014 sur g\u00e8nes significatifs (classique)"), .tr("GSEA \u2014 sur tous les g\u00e8nes class\u00e9s (sans seuil)"))))
      updateSelectInput(session, "pathway_source", label = .tr("Source de g\u00e8nes"),
        choices = stats::setNames(c("up","down","all_sig","manual"),
          c(.tr("G\u00e8nes Up (significatifs)"), .tr("G\u00e8nes Down (significatifs)"),
            .tr("Tous g\u00e8nes significatifs"), .tr("S\u00e9lection manuelle"))))
      updateSelectizeInput(session, "pathway_genes", label = .tr("G\u00e8nes"))
      updateSelectInput(session, "pathway_db",  label = .tr("Base de donn\u00e9es"))
      updateSelectInput(session, "pathway_org", label = .tr("Organisme"),
        choices = stats::setNames(c("human","mouse"), c(.tr("Humain"), .tr("Souris"))))
      updateNumericInput(session, "pathway_pval", label = .tr("P-value cutoff"))
      updateSelectInput(session, "pathway_padj_method", label = .tr("Méthode de correction (p-adj)"))
      updateActionButton(session, "run_pathway", label = .tr("Lancer Enrichissement"))
      # Bulk V2 M2 — libellés « scores par échantillon »
      # PLOT-S6b — la source de jeux de gènes est un selectInput : ses libellés
      # doivent être repoussés au changement de langue (les choix natifs sont
      # traduits à la construction de l'UI, cf. bulk_gene_set_choices(.tr_plain)).
      updateSelectInput(session, "scores_source", label = .tr("Source de jeux de gènes"),
        choices = bulk_gene_set_choices(.tr))
      updateSelectInput(session, "scores_org", label = .tr("Organisme"),
        choices = stats::setNames(c("human", "mouse"), c(.tr("Humain"), .tr("Souris"))))
      updateSelectInput(session, "scores_method", label = .tr("Méthode de scoring"),
        choices = stats::setNames(c("ssgsea", "gsva", "plage", "zscore"),
                                  c("ssGSEA", "GSVA", "PLAGE", "zscore")))
      updateNumericInput(session, "scores_min_size", label = .tr("Taille min voie"))
      updateNumericInput(session, "scores_max_size", label = .tr("Taille max voie"))
      updateActionButton(session, "run_scores", label = .tr("Lancer Scores par échantillon"))
      updateSelectizeInput(session, "scores_color_by", label = .tr("Colorer la PCA par"))
      updateNumericInput(session, "scores_heat_top_n", label = .tr("Voies affichées (heatmap)"))
      updateActionButton(session, "dl_scores_csv", label = .tr("Export CSV (scores)"))
      updateActionButton(session, "dl_scores_rds", label = .tr("Export RDS (résultat complet)"))
      updateActionButton(session, "dl_scores_gmt", label = .tr("Exporter les jeux de gènes (.gmt)"))
    }, ignoreInit = TRUE)

    # (unchanged) manual gene picker refresh + mirroring
    observeEvent(shared_rv$filtered_counts, {
      req(shared_rv$filtered_counts)
      updateSelectizeInput(session, "pathway_genes",
                           choices = rownames(shared_rv$filtered_counts), server = TRUE)
    })
    observe({
      shared_rv$pathway_db   <- input$pathway_db
      shared_rv$pathway_mode <- input$enrich_mode
      # STAT-Q1 : reprise dans le script R reproductible exporté
      shared_rv$pathway_padj_method <- input$pathway_padj_method %||% TS_PADJ_METHOD_DEFAULT
    })

    .active_de_results <- function() {
      ac <- shared_rv$active_contrast
      if (is.null(ac) || !ac %in% names(shared_rv$contrasts)) return(NULL)
      shared_rv$contrasts[[ac]]
    }

    observe({
      shinyjs::toggleState("run_pathway", condition = !is.null(shared_rv$filtered_counts))
      shinyjs::toggleState("dl_pathway",  condition = !is.null(shared_rv$pathway_results))
      # Bulk V2 M2 — le scoring exige la matrice VST (étape 1), pas les counts.
      shinyjs::toggleState("run_scores", condition = !is.null(shared_rv$vst_mat))
      shinyjs::toggleState("dl_scores_csv", condition = !is.null(shared_rv$pathway_scores))
      shinyjs::toggleState("dl_scores_rds", condition = !is.null(shared_rv$pathway_scores))
      # PLOT-S6b — l'export .gmt ne dépend que des jeux chargés (pas du scoring).
      shinyjs::toggleState("dl_scores_gmt", condition = !is.null(scores_sets_rv()))
    })

    # ── Bulk V2 M2 — scores de voies par échantillon ────────────────────────
    observeEvent(shared_rv$vst_mat, {
      req(global_data$bulk_obj$metadata)
      meta <- global_data$bulk_obj$metadata
      updateSelectizeInput(session, "scores_color_by", choices = colnames(meta), server = FALSE)
    }, ignoreNULL = TRUE)

    output$scores_status <- renderText({
      global_data$language
      sc <- shared_rv$pathway_scores
      if (is.null(sc)) .tr("En attente — lancez d'abord le Filtrage & VST (étape 1).")
      else .t_fmt(.tr("\u2713 {n} voies scorées x {m} échantillons [ {meth} ]"),
                  n = nrow(sc$scores), m = ncol(sc$scores), meth = sc$method)
    })

    # PLOT-S6b — jeux de gènes réellement chargés (natifs ou .gmt) : conservés
    # pour l'export .gmt, qui doit rendre EXACTEMENT ce qui a été scoré.
    # (le reactiveVal lui-même est déclaré en tête de serveur : lu par un
    #  observe() qui s'exécute avant cette section.)

    output$scores_source_hint <- renderText({
      global_data$language
      src <- input$scores_source %||% "msigdb_hallmark"
      if (identical(src, "file")) {
        return(.tr("Format attendu : nom<TAB>description<TAB>gènes..."))
      }
      cat_df <- bulk_gene_set_catalog()
      row <- cat_df[cat_df$source == src, , drop = FALSE]
      if (nrow(row) == 0L) return("")
      if (!isTRUE(row$available)) {
        return(.t_fmt(.tr("\u26a0\ufe0f Source indisponible — installez {pkg} (aucun téléchargement automatique)."),
                      pkg = row$requires))
      }
      .tr(row$description)
    })

    observeEvent(drive_trigger_scores(), {
      # ── DECLARE THE JOB OVER (drive job contract, spec §5) ────────────────
      # Même contrat que l'observateur d'enrichissement ci-dessus : ce jeton est
      # DÉCLARÉ long (le calcul GSVA/ssGSEA est synchrone et long), donc il doit
      # fermer son job sur TOUTE sortie, sinon le contrat B refuserait à jamais
      # le `run_pipeline` suivant et le module deviendrait impilotable — la
      # panne silencieuse la plus coûteuse.
      outcome <- new.env(parent = emptyenv())
      outcome$v <- "refused"
      if (isTRUE(ts_drive_job_busy()) &&
          identical(ts_drive_job_state()$button, "bulk-pathways-run_scores")) {
        on.exit(ts_drive_job_finish("bulk-pathways-run_scores",
          status = switch(outcome$v, ok = "done", failed = "error", "invalid"),
          error = if (identical(outcome$v, "failed")) {
            "the per-sample scoring raised — see the app notification"
          } else NULL
        ), add = TRUE)
      }

      req(input$run_scores > 0 || shiny::isolate(drive_counter_scores()) > 0)
      req(shared_rv$vst_mat)
      drive_last$seq     <- ts_drive_job_state()$seq
      drive_last$started <- as.numeric(Sys.time())
      # PLOT-S6b — la source décide : jeux NATIFS (aucun fichier à fournir) ou
      # .gmt fourni. Les deux rendent la même forme (nom -> gènes), donc tout
      # l'aval (compute_pathway_scores, exports, vues) est inchangé.
      src <- input$scores_source %||% "msigdb_hallmark"
      if (identical(src, "file")) {
        if (is.null(input$scores_gmt) || is.null(input$scores_gmt$datapath)) {
          showNotification(.tr("\u26a0\ufe0f Fournissez un fichier .gmt (jeux de gènes)."),
                           type = "warning", duration = 5)
          drive_record("run_scores", "not_ready")
          return()
        }
        sets <- tryCatch(bulk_parse_gmt(input$scores_gmt$datapath), error = function(e) e)
        if (inherits(sets, "error")) {
          showNotification(paste(.tr("Erreur GMT:"), conditionMessage(sets)),
                           type = "error", duration = 8)
          drive_record("run_scores", "error", error = conditionMessage(sets))
          return()
        }
      } else {
        sets <- tryCatch(bulk_load_gene_sets(src, input$scores_org %||% "human"),
                         error = function(e) e)
        if (inherits(sets, "error")) {
          showNotification(paste(.tr("Erreur jeux de gènes :"), conditionMessage(sets)),
                           type = "error", duration = 8)
          drive_record("run_scores", "error", error = conditionMessage(sets))
          return()
        }
      }
      scores_sets_rv(sets)
      # `add = TRUE` est PORTEUR : un `on.exit()` nu effacerait la déclaration
      # de job posée en tête d'observateur (cf. le commentaire long ci-dessus).
      p <- shiny::Progress$new(); on.exit(p$close(), add = TRUE)
      p$set(message = .tr("Scores de voies par échantillon..."), value = 0.2)
      tryCatch({
        res <- compute_pathway_scores(
          shared_rv$vst_mat, sets,
          method      = input$scores_method %||% "ssgsea",
          min_size    = input$scores_min_size %||% TS_BULK_GSVA_MIN_SIZE,
          max_size    = input$scores_max_size %||% TS_BULK_GSVA_MAX_SIZE,
          overlap_min = TS_BULK_GSVA_OVERLAP_MIN
        )
        shared_rv$pathway_scores <- res
        # Stockage contractuel : bulk_obj$pathways$per_sample (réaffectation
        # complète pour déclencher la réactivité de global_data).
        bo <- global_data$bulk_obj
        if (!is.list(bo$pathways)) bo$pathways <- list()
        bo$pathways$per_sample <- res$scores
        bo <- bulk_ensure_provenance(bo)
        global_data$bulk_obj <- bo
        if (nrow(res$qc$dropped) > 0L) {
          showNotification(.t_fmt(.tr("\u26a0\ufe0f {n} jeu(x) de gènes rejeté(s) — voir le détail dans l'onglet Scores."),
                                   n = nrow(res$qc$dropped)), type = "warning", duration = 6)
        }
        showNotification(.t_fmt(.tr("\u2713 {n} voies scorées (ssGSEA/GSVA) sur {m} échantillons."),
                                 n = nrow(res$scores), m = ncol(res$scores)), type = "message")
        nav_select(id = "pathways_tabs", selected = "tab_scores", session = session)
        # `empty` reste distinct de `done` : tous les jeux de gènes peuvent être
        # rejetés (recouvrement < seuil), et « 0 voie » n'est pas « N voies ».
        if (nrow(res$scores) == 0L) {
          drive_record("run_scores", "empty", 0L)
        } else {
          drive_record("run_scores", "done", nrow(res$scores))
        }
        outcome$v <- "ok"
      }, error = function(e) {
        showNotification(paste(.tr("Erreur scores:"), conditionMessage(e)),
                         type = "error", duration = 8)
        shared_rv$pathway_scores <- NULL
        drive_record("run_scores", "error", error = conditionMessage(e))
        outcome$v <- "failed"
      })
    })

    output$scores_dropped_ui <- renderUI({
      global_data$language
      sc <- shared_rv$pathway_scores
      if (is.null(sc) || nrow(sc$qc$dropped) == 0L) return(NULL)
      div(class = "alert alert-warning", style = "font-size:0.82em;",
          icon("triangle-exclamation"), " ",
          .t_fmt(.tr("{n} jeu(x) rejeté(s) — recouvrement < {pct} % ou taille hors bornes. Détail :"),
                 n = nrow(sc$qc$dropped), pct = round(100 * TS_BULK_GSVA_OVERLAP_MIN)),
          ts_datatable(sc$qc$dropped, page_length = 5, buttons = FALSE,
                       filter = "none", scroll_x = FALSE))
    })

    output$scores_pca <- renderPlot({
      global_data$language
      req(shared_rv$pathway_scores)
      color_by <- if (nzchar(input$scores_color_by %||% "")) input$scores_color_by else NULL
      tryCatch(
        plot_pathway_scores_pca(shared_rv$pathway_scores,
                                metadata = global_data$bulk_obj$metadata,
                                color_by = color_by, tr = .tr_fn(global_data)),
        error = function(e) {
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 1, y = 1,
                              label = paste(.tr("Erreur:"), conditionMessage(e)), color = "red") +
            ggplot2::theme_void()
        }
      )
    })

    output$scores_heatmap <- renderPlot({
      global_data$language
      req(shared_rv$pathway_scores)
      h <- plot_pathway_scores_heatmap(shared_rv$pathway_scores,
                                       top_n = input$scores_heat_top_n %||% 50,
                                       tr = .tr_fn(global_data))
      # ComplexHeatmap -> print() explicite ; ggplot de repli s'imprime pareil.
      print(h)
    })

    output$scores_table <- renderDT({
      global_data$language
      req(shared_rv$pathway_scores)
      s <- round(shared_rv$pathway_scores$scores, 4)
      ts_datatable(as.data.frame(s), page_length = 15L,
                   filename_base = "bulk_pathways_scores", filter = "none",
                   rownames = TRUE)
    })

    output$dl_scores_csv <- downloadHandler(
      filename = function() paste0("pathway_scores_", shared_rv$pathway_scores$method,
                                   "_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(shared_rv$pathway_scores)
        write.csv(build_pathway_scores_export(shared_rv$pathway_scores), file, row.names = FALSE)
      }
    )
    output$dl_scores_rds <- downloadHandler(
      filename = function() paste0("pathway_scores_", shared_rv$pathway_scores$method,
                                   "_", Sys.Date(), ".rds"),
      content  = function(file) {
        req(shared_rv$pathway_scores)
        saveRDS(shared_rv$pathway_scores, file)
      }
    )
    # PLOT-S6b — export .gmt des jeux RÉELLEMENT scorés : une source native
    # (MSigDB/PROGENy/DoRothEA) reste ainsi réutilisable hors application, et
    # le .gmt exporté se relit à l'identique (bulk_write_gmt -> bulk_parse_gmt).
    output$dl_scores_gmt <- downloadHandler(
      filename = function() {
        src <- input$scores_source %||% "msigdb_hallmark"
        paste0("gene_sets_", src, "_", Sys.Date(), ".gmt")
      },
      content  = function(file) {
        sets <- scores_sets_rv()
        req(sets)
        bulk_write_gmt(sets, file)
      }
    )

    # ── Enrichment (ORA + GSEA) — strings translated ─────────────────────
    observeEvent(drive_trigger_pathway(), {
      # ── DECLARE THE JOB OVER (drive job contract, spec §5) ────────────────
      # Producer TERMINAL que `long = TRUE` engage. Enregistré AVANT le premier
      # `req()` pour que TOUTE sortie rapporte quelque chose : l'abandon d'un
      # `req()`, le garde GSEA sans DE et le garde `< 10` gènes laissent
      # `outcome$v` à "refused" -> `invalid` (un refus n'est PAS un échec, même
      # raisonnement qu'ailleurs : un refus ne doit pas apprendre à ignorer un
      # badge rouge) ; le chemin nominal pose "ok" -> `done` ; le `catch` de la
      # computation pose "failed" -> `error`.
      #
      # ⚠️ `outcome` est un ENVIRONNEMENT, pas une variable locale. MESURÉ
      # (2026-09-24) : un `job_outcome <- "failed"` écrit DANS un handler
      # `error = function(e) …` se lie à la frame DU HANDLER et ne remonte
      # JAMAIS — la sonde a rendu `refused` sur un `stop()` réel. C'est le motif
      # exact de `mod_bulk_de_run.R` (l. 352), qui annonce donc `invalid` là où
      # la DE a levé. Une cellule d'environnement a une sémantique de RÉFÉRENCE
      # et traverse la frontière de closure.
      outcome <- new.env(parent = emptyenv())
      outcome$v <- "refused"
      # `add = TRUE` est PORTEUR, pas du style : un `on.exit()` nu REMPLACE
      # toute expression en attente, donc il effacerait cette déclaration — le
      # job resterait `running` à jamais et l'agent sonderait une complétion
      # qui ne serait jamais écrite.
      if (isTRUE(ts_drive_job_busy()) &&
          identical(ts_drive_job_state()$button, "bulk-pathways-run_pathway")) {
        on.exit(ts_drive_job_finish("bulk-pathways-run_pathway",
          status = switch(outcome$v, ok = "done", failed = "error", "invalid"),
          error = if (identical(outcome$v, "failed")) {
            "the pathway enrichment raised — see the app notification"
          } else NULL
        ), add = TRUE)
      }

      req(input$run_pathway > 0 || shiny::isolate(drive_counter_pathway()) > 0)
      req(shared_rv$filtered_counts)
      # `seq` appartient au PROTOCOLE, pas au module : il n'est lisible que tant
      # que le job est en vol. Capturé ici pour que l'issue résolue reste
      # corrélable à la commande qui l'a demandée.
      drive_last$seq     <- ts_drive_job_state()$seq
      drive_last$started <- as.numeric(Sys.time())
      p <- shiny::Progress$new(); on.exit(p$close(), add = TRUE)

      if (input$enrich_mode == "gsea") {
        res_de <- .active_de_results()
        if (is.null(res_de)) {
          showNotification(.tr("\u26a0\ufe0f Lancez d'abord l'\u00e9tape 2 (Analyse Diff\u00e9rentielle)."), type = "warning")
          # Refus : rien n'a tourné. `not_ready` est la réponse honnête — le
          # module ne peut pas produire de résultat faute d'amont.
          drive_record("run_pathway", "not_ready")
          return()
        }
        p$set(message = .tr("GSEA en cours..."), value = 0.3)
        tryCatch({
          res <- run_gsea_enrichment(res_de, organism = input$pathway_org,
                                     database = input$pathway_db, pval_cutoff = input$pathway_pval,
                                     p_adjust_method = input$pathway_padj_method %||% TS_PADJ_METHOD_DEFAULT)
          if (nrow(res) == 0) {
            showNotification(.tr("\u2139\ufe0f Aucun pathway enrichi trouv\u00e9 (GSEA)."), type = "warning")
            shared_rv$pathway_results <- NULL
            # `empty` et non `done` : l'analyse a TOURNÉ et n'a rien trouvé.
            drive_record("run_pathway", "empty", 0L)
            return()
          }
          shared_rv$pathway_results <- res
          showNotification(.t_fmt(.tr("\u2705 {n} pathways enrichis (GSEA)"), n = nrow(res)), type = "message")
          shared_rv$active_tab <- "tab_pathway"
          drive_record("run_pathway", "done", nrow(res))
          outcome$v <- "ok"
        }, error = function(e) {
          showNotification(.t_fmt(.tr("\u274c Erreur GSEA: {msg}"), msg = as.character(e$message)[1]),
                           type = "error", duration = 8)
          shared_rv$pathway_results <- NULL
          drive_record("run_pathway", "error", error = as.character(e$message)[1])
          outcome$v <- "failed"
        })
        return()
      }

      genes_to_test <- NULL
      if (input$pathway_source %in% c("up","down","all_sig")) {
        res <- .active_de_results()
        if (is.null(res)) {
          showNotification(.tr("\u26a0\ufe0f Lancez d'abord l'\u00e9tape 2 (Analyse Diff\u00e9rentielle)."), type = "warning")
          drive_record("run_pathway", "not_ready")
          return()
        }
        sig <- res$padj < (shared_rv$padj_thresh %||% 0.05) &
               abs(res$log2FoldChange) > (shared_rv$lfc_thresh %||% 1)
        sig[is.na(sig)] <- FALSE
        genes_to_test <- switch(input$pathway_source,
          up = res$gene[sig & res$log2FoldChange > 0],
          down = res$gene[sig & res$log2FoldChange < 0],
          all_sig = res$gene[sig])
      } else {
        req(input$pathway_genes); genes_to_test <- input$pathway_genes
      }
      genes_to_test <- unique(trimws(genes_to_test)); genes_to_test <- genes_to_test[nchar(genes_to_test) > 0]

      if (length(genes_to_test) < 10) {
        showNotification(.t_fmt(.tr("\u26a0\ufe0f Trop peu de g\u00e8nes ({n}). Minimum 10 requis."), n = length(genes_to_test)),
                         type = "warning", duration = 5)
        # 🔴 LE GARDE SILENCIEUX QUE CE JALON REND VISIBLE. Avant lui, cette
        # sortie et un enrichissement réussi de 2 voies rendaient le MÊME
        # `done` de protocole, et `shared_rv$pathway_results` restait INCHANGÉ
        # (d'où la cassette : l'objet vivant ne peut pas les distinguer).
        drive_record("run_pathway", "empty", 0L)
        return()
      }

      # Univers ORA = gènes réellement mesurés du jeu chargé (aligne Bulk sur le
      # chemin SC, qui passe déjà rownames(sc_obj)) ; NULL en saisie manuelle
      # sans objet -> comportement inchangé (enrichissement pan-génome).
      fc <- shiny::isolate(shared_rv$filtered_counts)
      universe_genes <- if (is.null(fc)) NULL else rownames(fc)
      p$set(message = .tr("Enrichissement..."), value = 0.3)
      tryCatch({
        res <- run_pathway_enrichment(genes = genes_to_test, organism = input$pathway_org,
                                      database = input$pathway_db, pval_cutoff = input$pathway_pval,
                                      p_adjust_method = input$pathway_padj_method %||% TS_PADJ_METHOD_DEFAULT,
                                      universe = universe_genes)
        if (nrow(res) == 0) {
          showNotification(.tr("\u2139\ufe0f Aucun pathway enrichi trouv\u00e9."), type = "warning")
          shared_rv$pathway_results <- NULL
          drive_record("run_pathway", "empty", 0L)
          return()
        }
        shared_rv$pathway_results <- res
        showNotification(.t_fmt(.tr("\u2705 {n} pathways enrichis"), n = nrow(res)), type = "message")
        shared_rv$active_tab <- "tab_pathway"
        drive_record("run_pathway", "done", nrow(res))
        outcome$v <- "ok"
      }, error = function(e) {
        showNotification(.t_fmt(.tr("\u274c Erreur pathway: {msg}"), msg = as.character(e$message)[1]),
                         type = "error", duration = 6)
        shared_rv$pathway_results <- NULL
        drive_record("run_pathway", "error", error = as.character(e$message)[1])
        outcome$v <- "failed"
      })
    })

    output$pathway_status <- renderText({
      global_data$language
      if (is.null(shared_rv$pathway_results)) .tr("Aucune analyse en cours")
      else .t_fmt(.tr("\u2713 {n} pathways trouv\u00e9s [ {db} ]"),
                  n = nrow(shared_rv$pathway_results), db = input$pathway_db)
    })

    output$pathway_barplot <- renderPlot({
      global_data$language                     # i18n trigger
      req(shared_rv$pathway_results)
      plot_pathway_barplot(shared_rv$pathway_results, db_label = input$pathway_db, top_n = 15,
                           tr = .tr_fn(global_data),
                           palette = shared_rv$bulk_palette %||% "default")
    })
    output$pathway_dotplot <- renderPlot({
      global_data$language                     # i18n trigger
      req(shared_rv$pathway_results)
      plot_pathway_dotplot(shared_rv$pathway_results, db_label = input$pathway_db, top_n = 20,
                           tr = .tr_fn(global_data),
                           palette = shared_rv$bulk_palette %||% "default")
    })
    output$pathway_table <- renderDT({
      global_data$language                     # i18n trigger
      req(shared_rv$pathway_results)
      build_pathway_dt(shared_rv$pathway_results, tr = .tr_fn(global_data))
    })
    output$dl_pathway <- downloadHandler(
      filename = function() paste0("pathways_bulk_", input$pathway_db, "_", Sys.Date(), ".csv"),
      content  = function(file) { req(shared_rv$pathway_results); write.csv(shared_rv$pathway_results, file, row.names = FALSE) }
    )
    # STAT-Q4 — export Excel de la MEME table. Meme source de verite, donc pas de
    # divergence possible entre le CSV et le classeur. L'extension annonce le
    # format reellement ecrit (CSV si openxlsx manque).
    output$dl_pathway_excel <- downloadHandler(
      filename = function() paste0("pathways_bulk_", input$pathway_db, "_", Sys.Date(),
                                   if (requireNamespace("openxlsx", quietly = TRUE)) ".xlsx" else ".csv"),
      content  = function(file) {
        req(shared_rv$pathway_results)
        write_table_excel_or_csv(shared_rv$pathway_results, file)
      }
    )

    # ── GSEA curve (strings translated) ──────────────────────────────────
    output$gsea_curve_ui <- renderUI({
      global_data$language
      gsea_obj <- attr(shared_rv$pathway_results, "gsea_obj")
      if (is.null(gsea_obj)) {
        return(div(class = "alert alert-light", style = "font-size:0.85em;margin:15px;",
                   icon("info-circle"), " ", .tr("Disponible uniquement pour les r\u00e9sultats GSEA \u2014 relancez l'enrichissement en mode GSEA (panneau de gauche).")))
      }
      df <- shared_rv$pathway_results
      choices <- setNames(df$ID, sprintf("%s (NES=%.2f, p.adj=%.1e)", df$Description, df$NES, df$p.adjust))
      tagList(
        fluidRow(
          column(8, selectizeInput(ns("gsea_curve_pathway"), .tr("Pathway (tapez pour rechercher)"),
                                   choices = choices, width = "100%",
                                   options = list(placeholder = .tr("Pathway (tapez pour rechercher)")))),
          column(4, div(style = "margin-top:25px;",
                       downloadButton(ns("dl_gsea_curve_png"), .tr("Export PNG"), class = "btn-sm btn-secondary w-100")))
        ),
        checkboxInput(ns("gsea_curve_pvalue_table"), .tr("Afficher la table p-value sur le graphique"), value = TRUE),
        plotOutput(ns("gsea_curve_plot"), height = "500px")
      )
    })

    .gsea_curve_plot_fn <- function() {
      gsea_obj <- attr(shared_rv$pathway_results, "gsea_obj")
      req(gsea_obj, input$gsea_curve_pathway)
      if (!requireNamespace("enrichplot", quietly = TRUE)) {
        stop(errorCondition(.tr("Package 'enrichplot' requis (BiocManager::install('enrichplot'))."),
                            class = "bulk_pathways_error"))
      }
      enrichplot::gseaplot2(gsea_obj, geneSetID = input$gsea_curve_pathway,
                            title = input$gsea_curve_pathway,
                            pvalue_table = isTRUE(input$gsea_curve_pvalue_table))
    }

    output$gsea_curve_plot <- renderPlot({
      global_data$language  # i18n
      tryCatch(
        .gsea_curve_plot_fn(),
        error = function(e) {
          ggplot() +
            annotate("text", x = 1, y = 1, label = paste(.tr("Erreur:"), conditionMessage(e)), color = "red") +
            theme_void()
        }
      )
    })

    output$dl_gsea_curve_png <- downloadHandler(
      filename = function() paste0("gsea_curve_", input$gsea_curve_pathway, "_", Sys.Date(), ".png"),
      content  = function(file) {
        png(file, width = 9, height = 7, units = "in", res = 300)
        print(.gsea_curve_plot_fn())
        dev.off()
      }
    )

    # ── STAT-S2 : réseau d'enrichissement (emapplot / cnetplot) ─────────────
    output$network_ui <- renderUI({
      global_data$language  # i18n
      if (is.null(attr(shared_rv$pathway_results, "enrich_obj"))) {
        return(div(class = "alert alert-light", style = "font-size:0.85em;margin:15px;",
                   icon("info-circle"), " ",
                   .tr("Disponible uniquement après une analyse de voies — relancez l'enrichissement (panneau de gauche).")))
      }
      tagList(
        fluidRow(
          column(3, radioButtons(ns("network_mode"), .tr("Type de réseau"),
                                 choices = stats::setNames(
                                   c("emap", "cnet"),
                                   c(.tr("Voies ↔ voies (similarité de gènes)"),
                                     .tr("Voies ↔ gènes"))),
                                 inline = TRUE)),
          column(3, numericInput(ns("network_top_n"), .tr("Voies affichées (réseau)"),
                                 value = 30, min = 2, max = 100, step = 1)),
          column(3, div(style = "margin-top:25px;",
                        checkboxInput(ns("network_interactive"),
                                      .tr("Réseau interactif (survol des nœuds)"),
                                      value = FALSE))),
          column(3, div(style = "margin-top:25px;",
                        downloadButton(ns("dl_network_png"), .tr("Export PNG"), class = "btn-sm btn-secondary w-100")))
        ),
        # STAT-S2 V2 : le rendu bascule statique (V1) / plotly interactif —
        # même routeur que pca_interactive (mod_bulk_filter.R) ; défaut =
        # statique, zéro changement de comportement à l'ouverture.
        uiOutput(ns("network_plot_ui"))
      )
    })

    output$network_plot_ui <- renderUI({
      if (isTRUE(input$network_interactive)) {
        plotly::plotlyOutput(ns("network_plot_ly"), height = "600px")
      } else {
        plotOutput(ns("network_plot"), height = "600px")
      }
    })

    .network_plot_fn <- function() {
      req(shared_rv$pathway_results)
      top_n <- input$network_top_n
      if (is.null(top_n) || is.na(top_n)) top_n <- 30
      plot_pathway_network(shared_rv$pathway_results,
                           db_label = input$pathway_db,
                           top_n = top_n, mode = input$network_mode, tr = .tr)
    }

    .network_net_fn <- function() {
      req(shared_rv$pathway_results)
      top_n <- input$network_top_n
      if (is.null(top_n) || is.na(top_n)) top_n <- 30
      build_pathway_network_data(shared_rv$pathway_results,
                                 top_n = top_n, mode = input$network_mode)
    }

    output$network_plot <- renderPlot({
      global_data$language  # i18n
      tryCatch(
        .network_plot_fn(),
        error = function(e) {
          ggplot() +
            annotate("text", x = 1, y = 1, label = paste(.tr("Erreur:"), conditionMessage(e)), color = "red") +
            theme_void()
        }
      )
    })

    output$network_plot_ly <- renderPlotly({
      global_data$language  # i18n
      tryCatch(
        plot_pathway_network_interactive(
          .network_net_fn(),
          title = paste(.tr("Réseau d'enrichissement"), "-", input$pathway_db),
          tr = .tr
        ),
        error = function(e) plotly::plot_ly(type = "scatter", mode = "markers") |>
          plotly::layout(annotations = list(
            text = paste(.tr("Erreur:"), conditionMessage(e)),
            showarrow = FALSE))
      )
    })

    output$dl_network_png <- downloadHandler(
      filename = function() paste0("pathway_network_", input$network_mode, "_", Sys.Date(), ".png"),
      content  = function(file) {
        png(file, width = 10, height = 8, units = "in", res = 300)
        print(.network_plot_fn())
        dev.off()
      }
    )

  }) # /moduleServer
}

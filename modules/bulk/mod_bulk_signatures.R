# =============================================================================
# mod_bulk_signatures.R — Bulk V2 M3 : scores de SIGNATURES CELLULAIRES
# (MSigDB Hallmark / PROGENy / DoRothEA / RDS local) — UI + orchestration.
# =============================================================================
# Logique pure : R/bulk/bulk_signatures.R (contrat gelé). Le moteur de scoring
# est réutilisé du M2 (compute_pathway_scores) ou decoupleR (ulm).
# GARDE MISSION §M3 : l'avertissement « scores relatifs » est AFFICHÉ en
# PERMANENCE dans l'UI et porté par le résultat + chaque export.
# =============================================================================

# =============================================================================
# MCP drive action `bulk-signatures-run_signatures` (Phase C).
#
# WHY HERE AND NOT IN R/bulk/bulk_signatures.R: that file's exported surface is
# FROZEN by tests/testthat/test-bulk-signatures-contract-freeze.R, which asserts
# that every non-dot top-level name equals `bulk_signatures_public_api()` plus the
# disclaimer. A wrapper added there would be a contract change; the private,
# dot-prefixed helpers below keep it out, and match the SC precedent where
# `run_annot()` / `run_markers()` also live in the module layer.
#
# The thin wrapper composes the two existing R/ calls in the SAME order the human
# observer uses (mod_bulk_signatures.R:122-174) and adds nothing else: no new
# statistical method, no new default, no new dependency.
# =============================================================================

.BULK_SIGNATURES_DRIVE_MODULE <- "bulk_signatures"
.BULK_SIGNATURES_DRIVE_BUTTON <- "bulk-signatures-run_signatures"

#' Closed step vocabulary for this action. A step that ran and produced nothing
#' is `ran` with `status = "empty"`, never `error`.
.BULK_SIG_DRIVE_STEP_STATES <- c("skipped", "running", "ran", "ignored", "error")

#' Frozen input set for the drive action.
#'
#' Every value is read from a widget default or a declared config constant, and
#' NONE of them is session-derived, so the same five values are valid for any
#' dataset. `sig_rds` is deliberately ABSENT: it is a `fileInput` PATH, and a
#' drive caller must never be able to hand a filesystem path to an action.
.bulk_signatures_drive_inputs <- function() {
  list(
    sig_resource = "hallmark",
    sig_organism = "human",
    sig_method   = "ssgsea",
    sig_min_size = TS_BULK_GSVA_MIN_SIZE,
    sig_max_size = TS_BULK_GSVA_MAX_SIZE
  )
}

#' Local availability of a signature resource, or FALSE plus what it would need.
#'
#' `bulk_signature_resources()$available` reflects LOCALLY installed packages
#' only, and no resource is ever downloaded, so an unavailable resource must be
#' refused BEFORE a job is declared rather than discovered mid-run.
.bulk_signatures_resource_check <- function(resource) {
  res <- tryCatch(bulk_signature_resources(), error = function(e) NULL)
  if (is.null(res) || !is.data.frame(res)) {
    return(list(ok = FALSE, requires = ""))
  }
  row <- res[res$resource == resource, , drop = FALSE]
  if (nrow(row) != 1L) return(list(ok = FALSE, requires = ""))
  list(ok = isTRUE(row$available[1L]), requires = as.character(row$requires[1L]))
}

#' Thin wrapper: availability pre-check, then load, then score.
#'
#' @return list(ok, n_results, scores, resource, method) where `n_results` counts
#'   SCORED SIGNATURES (rows of `scores`). Errors are the domain's classed
#'   `bulk_signatures_error` with a `state`, and never carry a filesystem path.
run_signatures <- function(vst_mat, inputs = .bulk_signatures_drive_inputs()) {
  if (!is.list(inputs)) inputs <- .bulk_signatures_drive_inputs()
  fail <- function(state, message) {
    stop(errorCondition(message, class = "bulk_signatures_error", state = state))
  }
  resource <- as.character(inputs$sig_resource)[1L]
  method   <- as.character(inputs$sig_method)[1L]
  if (is.na(resource) || !nzchar(resource)) {
    fail("invalid_input", "bulk signatures: empty resource.")
  }
  if (is.na(method) || !nzchar(method)) {
    fail("invalid_input", "bulk signatures: empty scoring method.")
  }
  # A path-bearing input is refused by CONSTRUCTION, and the refusal never echoes
  # the value it refuses, so a caller cannot leak a local path into a log.
  if (identical(resource, "rds_local") || !is.null(inputs$sig_rds)) {
    fail("invalid_input",
         "bulk signatures: a local .rds path is not an accepted input; the frozen resource is 'hallmark'.")
  }
  check <- .bulk_signatures_resource_check(resource)
  if (!isTRUE(check$ok)) {
    fail("missing_dependency", sprintf(
      "bulk signatures: resource '%s' is not available locally (requires: %s); nothing is downloaded.",
      resource, check$requires))
  }
  if (is.null(vst_mat) || !is.matrix(vst_mat) || !is.numeric(vst_mat) ||
      nrow(vst_mat) < 1L || ncol(vst_mat) < 1L) {
    fail("invalid_input",
         "bulk signatures: the expression matrix is missing or is not a numeric matrix.")
  }
  sets <- bulk_load_signatures(resource, organism = as.character(inputs$sig_organism)[1L])
  res  <- bulk_score_signatures(vst_mat, sets, method = method,
                                min_size = inputs$sig_min_size,
                                max_size = inputs$sig_max_size)
  # `record` is the WHOLE scoring result, `resource` attached exactly as the human
  # observer does it. It is what gets stored, because that is the shape the five
  # readers expect; `n_results` is derived from it so the two can never disagree.
  res$resource <- resource
  list(ok = TRUE, n_results = as.integer(nrow(res$scores)), record = res,
       resource = resource, method = method)
}

#' THE single writer for the two slots one signature run fills.
#'
#' Both the human observer and the MCP drive action go through here, so the shape
#' stored in `shared_rv$signature_scores` cannot drift from what the READERS
#' expect. There are five of them and they are not interchangeable: `sig_status`
#' reads `$scores` and `$method`, `sig_heatmap` and `sig_pca` pass the WHOLE
#' record to the plotting helpers, `sig_table` reads `$scores`, and both download
#' handlers read `$method` for the filename and the whole record for the body.
#'
#' 🔴 WHY THIS HELPER EXISTS — measured, not designed. Live validation on
#' 2026-09-25 found the drive path storing a bare MATRIX where this slot holds the
#' scoring RECORD. The drive state still reported `done` with a correct
#' `n_results`, while the panel raised `$ operator is invalid for atomic vectors`
#' for both `sig_status` and `sig_heatmap`, nothing rendered, and the CSV
#' filename resolved to `signature_scores_NULL`. The unit tests had been green
#' because they asserted the writer's own assumption. One writer, one shape.
#'
#' @param res The `bulk_score_signatures()` result, with `resource` attached.
#' @return `res`, invisibly, so the caller can report counts from it.
#'
#' ⚠️ The local names below are not cosmetic: `bo$pathways$signatures <- res$scores`
#' is pinned as a LITERAL by test-bulk-signatures-contract-freeze.R:165, so this
#' helper is written to reproduce that line byte for byte rather than to satisfy
#' the freeze test with a rename.
.bulk_signatures_store <- function(res, global_data, shared_rv) {
  if (!is.list(res) || is.null(res$scores)) {
    stop(errorCondition(
      "bulk signatures: the result to store must be the scoring record carrying $scores.",
      class = "bulk_signatures_error", state = "invalid_input"))
  }
  shared_rv$signature_scores <- res
  # Contractual storage in bulk_obj$pathways$signatures (mission §M3).
  # `global_data` is fully re-assigned so the reactive invalidation is clean.
  bo <- global_data$bulk_obj
  if (!is.list(bo$pathways)) bo$pathways <- list()
  bo$pathways$signatures <- res$scores
  global_data$bulk_obj <- bulk_ensure_provenance(bo)
  invisible(res)
}

.bulk_signatures_drive_view <- function(status, elapsed_s = 0, seq = 0L,
                                        n_results = 0L, has_data = FALSE,
                                        ready = FALSE, step = NULL) {
  step <- if (is.null(step)) "skipped" else as.character(step)
  if (length(step) != 1L || is.na(step) || !step %in% .BULK_SIG_DRIVE_STEP_STATES) {
    step <- "error"
  }
  list(
    module = .BULK_SIGNATURES_DRIVE_MODULE,
    action = "run_pipeline",
    status = as.character(status),
    elapsed_s = as.numeric(elapsed_s),
    seq = as.integer(seq),
    n_results = as.integer(n_results),
    has_data = isTRUE(has_data),
    ready = isTRUE(ready),
    steps = list(signatures = step)
  )
}

#' Readiness, in the same order as the human path: a dataset, then Step 1's VST
#' matrix, then the local availability of the FROZEN resource. Returning the
#' reason (not FALSE) is what lets the poller report `not_ready` instead of
#' dispatching a job that must fail.
.bulk_signatures_drive_ready <- function(shared_rv, global_data) {
  if (is.null(tryCatch(shiny::isolate(global_data$bulk_obj), error = function(e) NULL))) {
    return("no bulk object loaded (global_data$bulk_obj is NULL)")
  }
  if (is.null(tryCatch(shiny::isolate(shared_rv$vst_mat), error = function(e) NULL))) {
    return("Step 1 has not produced a VST matrix (shared_rv$vst_mat is NULL)")
  }
  resource <- .bulk_signatures_drive_inputs()$sig_resource
  check <- .bulk_signatures_resource_check(resource)
  if (!isTRUE(check$ok)) {
    return(sprintf("resource '%s' is not available locally (requires: %s); nothing is downloaded",
                   resource, check$requires))
  }
  TRUE
}

#' State probe. `n_results` is the count of signatures the LAST successful run
#' left in `signature_scores`; `shiny::isolate()` keeps these reads out of the
#' poller's dependency set.
.bulk_signatures_drive_state <- function(shared_rv, global_data, run_state,
                                         last_step = NULL) {
  job <- tryCatch(ts_drive_job_state(), error = function(e) NULL)
  pending <- tryCatch(ts_drive_job_pending(), error = function(e) NULL)
  ready <- isTRUE(.bulk_signatures_drive_ready(shared_rv, global_data))
  obj <- tryCatch(shiny::isolate(global_data$bulk_obj), error = function(e) NULL)
  sc <- tryCatch(shiny::isolate(shared_rv$signature_scores), error = function(e) NULL)
  # The slot holds the scoring RECORD; the count comes from its `$scores`. The
  # matrix fallback is deliberate and narrow: a divergent writer must still yield
  # a TRUE count here, so this probe cannot quietly report 0 while the UI breaks.
  # The canonical shape itself is pinned by the writer/reader regression test.
  scores <- if (is.list(sc)) sc$scores else sc
  n_results <- if (is.matrix(scores) || is.data.frame(scores)) nrow(scores) else 0L
  current <- if (is.function(run_state)) {
    shiny::isolate(run_state())
  } else {
    as.character(run_state)
  }
  if (length(current) != 1L || is.na(current)) current <- "idle"
  .bulk_signatures_drive_view(
    status = if (ready) current else "not_ready",
    elapsed_s = if (is.null(pending)) 0 else as.numeric(pending$elapsed_s),
    seq = if (is.null(job)) 0L else as.integer(job$seq),
    n_results = n_results,
    has_data = !is.null(obj),
    ready = ready,
    step = last_step
  )
}

#' Runs the wrapper and publishes the result the way the human path does, then
#' reports the terminal state. A failure publishes NO result count, so a stale
#' table can never be read next to an error.
.bulk_signatures_run_drive <- function(global_data, shared_rv, close_job) {
  res <- tryCatch(
    run_signatures(shiny::isolate(shared_rv$vst_mat), .bulk_signatures_drive_inputs()),
    error = function(e) e
  )
  if (inherits(res, "condition")) {
    close_job("error", conditionMessage(res))
    return(list(status = "error", n_results = 0L, step = "error"))
  }
  # ONE writer, shared with the human observer: the drive path stores the same
  # scoring RECORD, so the five readers cannot see a different shape depending on
  # who triggered the run.
  .bulk_signatures_store(res$record, global_data, shared_rv)
  n_results <- as.integer(res$n_results %||% 0L)
  if (length(n_results) != 1L || is.na(n_results) || n_results < 0L) n_results <- 0L
  # 🔴 THE JOB VOCABULARY IS NARROWER THAN THE VIEW VOCABULARY.
  # `ts_drive_job_set_pending()` accepts only done / error / invalid / timeout /
  # session_lost, and REFUSES anything else. So an empty result closes the job as
  # `done` — the work COMPLETED, it produced nothing — while the published view
  # reports `empty`. Passing "empty" here would be refused, the job would stay
  # `running` until its timeout, and the agent would read a hang as a slow run.
  close_job("done", NULL)
  list(status = if (n_results == 0L) "empty" else "done",
       n_results = n_results, step = "ran")
}

mod_bulk_signatures_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # GARDE §M3 — avertissement permanent, non conditionnel.
    div(class = "alert alert-warning", style = "font-size:0.82em;",
        icon("triangle-exclamation"), " ",
        i18n$t("Scores de signatures relatifs : estimation d'abondance relative ne rempla\u00e7ant pas une quantification cytom\u00e9trique.")),
    h6(i18n$t("Ressource de signatures"), style = "font-weight:bold;"),
    selectInput(ns("sig_resource"), i18n$t("Source"),
                choices = stats::setNames(
                  c("hallmark", "progeny", "dorothea", "rds_local"),
                  c(.tr_plain("MSigDB Hallmark (50 voies)"), "PROGENy", "DoRothEA",
                    .tr_plain("Fichier RDS local"))),
                selected = "hallmark"),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'rds_local'", ns("sig_resource")), ns = ns,
      fileInput(ns("sig_rds"), i18n$t("Fichier .rds (liste nommée ou data.frame signature/gene)"),
                accept = c(".rds", ".RDS")),
      div(class = "small text-muted",
          i18n$t("Liste nomm\u00e9e de vecteurs de g\u00e8nes, ou data.frame avec colonnes 'signature' et 'gene'."))
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] != 'rds_local'", ns("sig_resource")), ns = ns,
      selectInput(ns("sig_organism"), i18n$t("Organisme"),
                  choices = stats::setNames(c("human", "mouse"),
                                            c(.tr_plain("Humain"), .tr_plain("Souris"))))
    ),
    selectInput(ns("sig_method"), i18n$t("M\u00e9thode de scoring"),
                choices = stats::setNames(
                  c("ssgsea", "gsva", "zscore", "ulm_decoupleR"),
                  c("ssGSEA", "GSVA", "zscore",
                    .tr_plain("ULM (decoupleR)"))),
                selected = "ssgsea"),
    fluidRow(
      column(6, numericInput(ns("sig_min_size"), i18n$t("Taille min signature"),
                             value = TS_BULK_GSVA_MIN_SIZE, min = 1, step = 1)),
      column(6, numericInput(ns("sig_max_size"), i18n$t("Taille max signature"),
                             value = TS_BULK_GSVA_MAX_SIZE, min = 2, step = 1))
    ),
    actionButton(ns("run_signatures"), i18n$t("Lancer Scores de signatures"),
                 class = "btn-warning w-100", icon = icon("fingerprint")),
    div(class = "small text-muted mt-1", textOutput(ns("sig_status")))
  )
}

mod_bulk_signatures_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE, max_height = "900px",
    card_header(i18n$t("Signatures cellulaires")),
    # GARDE §M3 — l'avertissement voyage aussi avec les vues.
    div(class = "alert alert-light", style = "font-size:0.78em;margin-bottom:4px;",
        icon("info-circle"), " ", i18n$t("Scores de signatures relatifs : estimation d'abondance relative ne rempla\u00e7ant pas une quantification cytom\u00e9trique.")),
    navset_tab(
      id = ns("sig_tabs"),
      nav_panel(i18n$t("Heatmap"), plotOutput(ns("sig_heatmap"), height = "640px")),
      nav_panel("PCA",              plotOutput(ns("sig_pca"), height = "420px")),
      nav_panel(i18n$t("Table"),    DTOutput(ns("sig_table"))),
      nav_panel(i18n$t("Rejets"),   uiOutput(ns("sig_dropped_ui")))
    ),
    fluidRow(
      column(6, downloadButton(ns("dl_sig_csv"), i18n$t("Export CSV"),
                               class = "btn-sm btn-info w-100 mt-2")),
      column(6, downloadButton(ns("dl_sig_rds"), i18n$t("Export RDS (résultat complet)"),
                               class = "btn-sm btn-secondary w-100 mt-2"))
    )
  )
}

mod_bulk_signatures_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # ── i18n push on language switch ─────────────────────────────────────
    observeEvent(global_data$language, {
      updateSelectInput(session, "sig_resource", label = .tr("Source"),
        choices = stats::setNames(c("hallmark", "progeny", "dorothea", "rds_local"),
          c(.tr("MSigDB Hallmark (50 voies)"), "PROGENy", "DoRothEA",
            .tr("Fichier RDS local"))))
      updateSelectInput(session, "sig_organism", label = .tr("Organisme"),
        choices = stats::setNames(c("human", "mouse"), c(.tr("Humain"), .tr("Souris"))))
      updateSelectInput(session, "sig_method", label = .tr("Méthode de scoring"),
        choices = stats::setNames(c("ssgsea", "gsva", "zscore", "ulm_decoupleR"),
          c("ssGSEA", "GSVA", "zscore", .tr("ULM (decoupleR)"))))
      updateNumericInput(session, "sig_min_size", label = .tr("Taille min signature"))
      updateNumericInput(session, "sig_max_size", label = .tr("Taille max signature"))
      updateActionButton(session, "run_signatures", label = .tr("Lancer Scores de signatures"))
      updateActionButton(session, "dl_sig_csv", label = .tr("Export CSV"))
      updateActionButton(session, "dl_sig_rds", label = .tr("Export RDS (résultat complet)"))
    }, ignoreInit = TRUE)

    observe({
      shinyjs::toggleState("run_signatures", condition = !is.null(shared_rv$vst_mat))
      shinyjs::toggleState("dl_sig_csv", condition = !is.null(shared_rv$signature_scores))
      shinyjs::toggleState("dl_sig_rds", condition = !is.null(shared_rv$signature_scores))
    })

    output$sig_status <- renderText({
      global_data$language
      sc <- shared_rv$signature_scores
      if (is.null(sc)) .tr("En attente — lancez d'abord le Filtrage & VST (étape 1).")
      else .t_fmt(.tr("\u2713 {n} signatures scor\u00e9es x {m} \u00e9chantillons [ {meth} ]"),
                  n = nrow(sc$scores), m = ncol(sc$scores), meth = sc$method)
    })

    observeEvent(input$run_signatures, {
      req(shared_rv$vst_mat)
      resource <- input$sig_resource %||% "hallmark"
      sets <- NULL
      if (identical(resource, "rds_local")) {
        if (is.null(input$sig_rds) || is.null(input$sig_rds$datapath)) {
          showNotification(.tr("\u26a0\ufe0f Fournissez un fichier .rds de signatures."),
                           type = "warning", duration = 5)
          return()
        }
        sets <- tryCatch(
          bulk_load_signatures("rds_local", rds_path = input$sig_rds$datapath),
          error = function(e) e)
      } else {
        # ⚠️ `add = TRUE` is LOAD-BEARING in this file since Phase C: this module
        # now declares a drive job, and test-drive-watcher.R refuses any file that
        # calls `ts_drive_job_finish()` and also carries a BARE `on.exit()`, because
        # a bare call REPLACES the registered expressions and would discard the job
        # declaration. It was already `add = TRUE` on the second registration; the
        # first one now matches.
        p <- shiny::Progress$new(); on.exit(p$close(), add = TRUE)
        p$set(message = .tr("Chargement de la ressource..."), value = 0.1)
        sets <- tryCatch(
          bulk_load_signatures(resource, organism = input$sig_organism %||% "human"),
          error = function(e) e)
      }
      if (inherits(sets, "error")) {
        showNotification(paste(.tr("Erreur signatures:"), conditionMessage(sets)),
                         type = "error", duration = 8)
        return()
      }

      p <- shiny::Progress$new(); on.exit(p$close(), add = TRUE)
      p$set(message = .tr("Scores de signatures..."), value = 0.4)
      tryCatch({
        res <- bulk_score_signatures(
          shared_rv$vst_mat, sets,
          method   = input$sig_method %||% "ssgsea",
          min_size = input$sig_min_size %||% TS_BULK_GSVA_MIN_SIZE,
          max_size = input$sig_max_size %||% TS_BULK_GSVA_MAX_SIZE
        )
        res$resource <- resource
        # The SAME writer the drive path uses (see .bulk_signatures_store): the
        # slot shape is defined once, in one place, for both triggers.
        .bulk_signatures_store(res, global_data, shared_rv)
        showNotification(.t_fmt(.tr("\u2713 {n} signatures scor\u00e9es sur {m} \u00e9chantillons."),
                                 n = nrow(res$scores), m = ncol(res$scores)), type = "message")
        nav_select(id = "sig_tabs", selected = "sig_heatmap_tab", session = session)
      }, error = function(e) {
        showNotification(paste(.tr("Erreur signatures:"), conditionMessage(e)),
                         type = "error", duration = 8)
        shared_rv$signature_scores <- NULL
      })
    })

    # ── DRIVE (MCP) — bulk-signatures-run_signatures ────────────────────────
    # The human observer above is NOT re-wired and NOT duplicated: the drive
    # action calls `run_signatures()`, which composes the same two R/ calls in
    # the same order, and then applies the same two writes. `sig_drive_counter`
    # is the only trigger, so no `input$` is read here — that is what makes a
    # FROZEN input set possible at all.
    sig_drive_counter <- shiny::reactiveVal(0L)
    sig_drive_run_state <- shiny::reactiveVal("idle")
    sig_drive_last_step <- new.env(parent = emptyenv())
    sig_drive_last_step$value <- NULL
    sig_drive_job <- new.env(parent = emptyenv())
    sig_drive_job$id <- NULL

    sig_drive_ready <- function() {
      .bulk_signatures_drive_ready(shared_rv, global_data)
    }
    sig_drive_state <- function() {
      .bulk_signatures_drive_state(shared_rv, global_data,
                                   sig_drive_run_state,
                                   sig_drive_last_step$value)
    }

    # `long = TRUE` is what gives `running` a real producer: the job is
    # SYNCHRONOUS, so no tick runs during it and a flag set inside the observer
    # would be unobservable. The two existing Bulk long jobs (run_pathway,
    # run_scores) declare no `timeout_s` either, so the poller's default ceiling
    # applies here as well — adding one would be a new, unmeasured default.
    #
    # 🔴 THE BUTTON ID IS A LITERAL HERE, not the constant: test-drive-watcher.R
    # greps every TS_DRIVE_BUTTONS entry as a quoted literal next to a
    # `ts_drive_publish_token(` call, and that shared check is what proves each
    # allowlist entry is really wired. A constant would make the wiring
    # invisible to it. `expect_identical(.BULK_SIGNATURES_DRIVE_BUTTON,
    # TS_DRIVE_BULK_SIGNATURES_BUTTON)` keeps the two from drifting.
    ts_drive_publish_token(global_data, "bulk-signatures-run_signatures",
      sig_drive_counter, ready = sig_drive_ready,
      state = sig_drive_state, long = TRUE)

    close_sig_drive_job <- function(status, error = NULL) {
      if (is.null(sig_drive_job$id)) return(invisible(FALSE))
      job <- tryCatch(ts_drive_job_state(), error = function(e) NULL)
      if (is.null(job) || !identical(job$job_id, sig_drive_job$id)) {
        return(invisible(FALSE))
      }
      ts_drive_job_finish(.BULK_SIGNATURES_DRIVE_BUTTON, status = status,
                          error = error, job_id = sig_drive_job$id)
    }

    observeEvent(sig_drive_counter(), {
      if (!isTRUE(sig_drive_ready())) return()
      sig_drive_job$id <- NULL
      job <- tryCatch(ts_drive_job_state(), error = function(e) NULL)
      if (!is.null(job) && isTRUE(ts_drive_job_busy()) &&
          identical(job$button, .BULK_SIGNATURES_DRIVE_BUTTON)) {
        sig_drive_job$id <- job$job_id
      }
      sig_drive_run_state("running")
      res <- .bulk_signatures_run_drive(global_data, shared_rv, close_sig_drive_job)
      sig_drive_last_step$value <- res$step
      sig_drive_run_state(res$status)
    }, ignoreInit = TRUE)

    output$sig_heatmap <- renderPlot({
      global_data$language
      req(shared_rv$signature_scores)
      h <- plot_pathway_scores_heatmap(shared_rv$signature_scores, top_n = 60,
                                       tr = .tr_fn(global_data))
      print(h)
    })
    output$sig_pca <- renderPlot({
      global_data$language
      req(shared_rv$signature_scores)
      tryCatch(
        plot_pathway_scores_pca(shared_rv$signature_scores,
                                metadata = global_data$bulk_obj$metadata,
                                color_by = NULL, tr = .tr_fn(global_data)),
        error = function(e) {
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 1, y = 1,
                              label = paste(.tr("Erreur:"), conditionMessage(e)), color = "red") +
            ggplot2::theme_void()
        }
      )
    })
    output$sig_table <- renderDT({
      global_data$language
      req(shared_rv$signature_scores)
      ts_datatable(as.data.frame(round(shared_rv$signature_scores$scores, 4)),
                   page_length = 15L, filename_base = "bulk_signature_scores",
                   filter = "none", rownames = TRUE)
    })
    output$sig_dropped_ui <- renderUI({
      global_data$language
      sc <- shared_rv$signature_scores
      if (is.null(sc) || is.null(sc$qc$dropped) || nrow(sc$qc$dropped) == 0L) {
        return(div(class = "alert alert-light", style = "font-size:0.85em;",
                   icon("check-circle"), " ", .tr("Aucune signature rejet\u00e9e.")))
      }
      div(class = "alert alert-warning", style = "font-size:0.82em;",
          icon("triangle-exclamation"), " ",
          .t_fmt(.tr("{n} signature(s) rejet\u00e9e(s) :"), n = nrow(sc$qc$dropped)),
          ts_datatable(sc$qc$dropped, page_length = 5, buttons = FALSE,
                       filter = "none", scroll_x = FALSE))
    })

    output$dl_sig_csv <- downloadHandler(
      filename = function() paste0("signature_scores_", shared_rv$signature_scores$method,
                                   "_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(shared_rv$signature_scores)
        # GARDE §M3 : la colonne disclaimer voyage avec l'export.
        write.csv(build_signature_scores_export(shared_rv$signature_scores),
                  file, row.names = FALSE)
      }
    )
    output$dl_sig_rds <- downloadHandler(
      filename = function() paste0("signature_scores_", shared_rv$signature_scores$method,
                                   "_", Sys.Date(), ".rds"),
      content  = function(file) {
        req(shared_rv$signature_scores)
        saveRDS(shared_rv$signature_scores, file)
      }
    )

    # Slice 2.3: publish THIS module's drive export route — the scores table
    # built by build_signature_scores_export() (the same builder the human
    # dl_sig_csv writes, disclaimer column included). The exporter lives in
    # mod_bulk_signatures_export.R so the offline tests can reach it.
    ts_drive_publish_export(global_data, "bulk_signatures", function() {
      bulk_signatures_export_scores_csv(shared_rv, global_data)
    })

  }) # /moduleServer
}

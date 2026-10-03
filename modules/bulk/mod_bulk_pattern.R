# =============================================================================
# modules/bulk/mod_bulk_pattern.R — Clustering de profils d'expression (STAT-S3)
# =============================================================================
# UI + orchestration UNIQUEMENT : tout le calcul vit dans R/bulk/bulk_pattern.R
# (logique pure, contract-first — docs/contracts/BULK_PATTERN_CONTRACT.md).
#
# Sources de gènes : résultat DE du contraste actif (up / down / all_sig),
# même pattern que mod_bulk_pathways. Entrée matrice : shared_rv$vst_mat
# (étape 1), groupes : colonne DECLAREE de global_data$bulk_obj$metadata.
#
# MVP kmeans (zéro dépendance nouvelle) ; k et la graine sont déclarés par
# l'utilisateur. Résultat descriptif : aucune p-value produite.
# =============================================================================

# =============================================================================
# MCP drive action `bulk-pattern-run_pattern` (Phase D).
#
# The pure domain (`run_pattern_clustering()` and friends) is FROZEN and is not
# touched: everything below is the same module-layer wrapper pattern already used
# by `bulk-signatures-run_signatures`, and for the same reason - a public name in
# R/bulk/bulk_pattern.R would change `bulk_pattern_public_api()`, which
# test-bulk-pattern-contract-freeze.R pins.
#
# THE ONE PARAMETER THAT CANNOT BE FROZEN IS THE GROUP COLUMN, and it is handled
# by a RULE rather than a value (see .bulk_pattern_group_column). `run_pattern_clustering()`
# takes `group_column` as a REQUIRED argument and raises when it is not a metadata
# column (R/bulk/bulk_pattern.R:111-117), so a frozen literal would either error or
# silently pick whichever column happens to be first.
# =============================================================================

.BULK_PATTERN_DRIVE_MODULE <- "bulk_pattern"
.BULK_PATTERN_DRIVE_BUTTON <- "bulk-pattern-run_pattern"

#' Closed step vocabulary for this action, same as the other Bulk actions.
.BULK_PATTERN_DRIVE_STEP_STATES <- c("skipped", "running", "ran", "ignored", "error")

#' Frozen input set for the drive action.
#'
#' `pattern_source`, `pattern_k` and `pattern_seed` are the widget defaults.
#' `padj_thresh` / `lfc_thresh` are the SIGNIFICANCE thresholds this action
#' applies to the active contrast; they are declared here on purpose rather than
#' read from `shared_rv`, exactly as `bulk-de-run_de` freezes its own pair, so the
#' gene set a drive run selects never silently follows whatever thresholds the
#' session's DE happened to use. The one value that IS read from the session is
#' the group column, and only through the rule in .bulk_pattern_group_column().
.bulk_pattern_drive_inputs <- function() {
  list(
    pattern_source = "up",
    pattern_k      = 4L,
    pattern_seed   = 15L,
    padj_thresh    = 0.05,
    lfc_thresh     = 1
  )
}

#' THE single writer for `shared_rv$pattern_result`, shared by the human observer
#' and the drive action.
#'
#' The slot holds the WHOLE clustering record: `pattern_plot` passes it to
#' `plot_pattern_profiles()`, `pattern_table` and the download handler read its
#' `$k`, and the status line reads `$summary`. A drive run that stored anything
#' narrower would break those readers exactly the way the signature slot did on
#' 2026-09-25, so the shape is written once, here.
.bulk_pattern_store <- function(res, shared_rv) {
  if (!is.list(res) || is.null(res$clusters) || is.null(res$k)) {
    stop(errorCondition(
      "bulk pattern: the result to store must be the clustering record carrying $clusters.",
      class = "bulk_pattern_error", state = "invalid_input"))
  }
  shared_rv$pattern_result <- res
  shared_rv$active_tab     <- "tab_pattern"
  invisible(res)
}

#' Resolve the group column by RULE, never by guessing.
#'
#' Preference order:
#'   1. the group column the DE step recorded for the ACTIVE contrast
#'      (`shared_rv$active_condition_col`), when it is a usable metadata column;
#'   2. otherwise, the single usable metadata column, if there is exactly one;
#'   3. otherwise a reason, which the caller turns into `not_ready`.
#'
#' A column is "usable" by the SAME rule the UI uses to populate `pattern_group`:
#' a factor/character with at least two distinct values. When step 2 cannot
#' decide, the reason reports only the NUMBER of candidates, never their names.
#'
#' @return `list(column = <one column name>)` or `list(reason = <short reason>)`.
#'   The two are returned in DIFFERENT FIELDS on purpose: an earlier version
#'   returned a bare string, and since a reason is also a length-1 character the
#'   caller could not tell a column from a refusal - it reported `ready` while
#'   holding the sentence "step 2 recorded no group column and 2 metadata columns
#'   are candidates". A named field cannot be confused with a value.
.bulk_pattern_group_column <- function(shared_rv, global_data) {
  # `shiny::isolate()` on every read: these helpers are called BOTH from the
  # observer (a reactive context) and from the poller's state probe (which is not
  # one), and reading reactiveValues outside a context throws. A bare
  # `tryCatch(read)` would swallow that as "no metadata" and report a
  # not_ready reason that is simply false.
  meta <- tryCatch(shiny::isolate(global_data$bulk_obj$metadata),
                   error = function(e) NULL)
  if (!is.data.frame(meta) || !ncol(meta)) {
    return(list(reason = "the active dataset carries no metadata"))
  }
  usable <- vapply(names(meta), function(cl) {
    x <- meta[[cl]]
    (is.factor(x) || is.character(x)) &&
      length(unique(stats::na.omit(as.character(x)))) >= 2L
  }, logical(1))

  col <- tryCatch(shiny::isolate(shared_rv$active_condition_col),
                  error = function(e) NULL)
  if (!is.null(col) && length(col) == 1L && !is.na(col) && nzchar(as.character(col))) {
    col <- as.character(col)
    if (!col %in% names(meta)) {
      return(list(reason = "the group column recorded by step 2 is absent from the metadata"))
    }
    if (!isTRUE(usable[[col]])) {
      return(list(reason = "the group column recorded by step 2 does not define at least two groups"))
    }
    return(list(column = col))
  }

  n <- sum(usable)
  if (n == 1L) return(list(column = names(meta)[usable]))
  if (n == 0L) return(list(reason = "no metadata column defines at least two groups"))
  list(reason = sprintf("step 2 recorded no group column and %d metadata columns are candidates", n))
}

#' Availability pre-check: NONE is needed, and that is a measured statement.
#' The clustering is `stats::kmeans()` (base R) with `nstart`/`itermax` from
#' `TS_PATTERN_KMEANS_*`, so there is no optional package that could be missing
#' and nothing is ever downloaded. The readiness gate below is therefore about
#' DATA, not about dependencies - the contrast that must exist is checked in
#' .bulk_pattern_drive_ready().
#'
#' Errors are raised with the DOMAIN's own `.bulk_pattern_stop()`
#' (R/bulk/bulk_pattern.R:41) rather than a second copy: a same-named function in
#' two sourced files is a real hazard, because app.R sources in a FIXED order
#' (R/ at :123, this module at :193) and only the LAST definition is live. The
#' duplication gate flags it, and the failure would have been silent.
#' The helper is deliberately NOT added to `bulk_pattern_public_api()`, which
#' test-bulk-pattern-contract-freeze.R pins.

#' Thin wrapper: the same five steps as the human observer, in the same order,
#' with the frozen inputs and the resolved group column.
#'
#' @return list(ok, n_results, record, k, seed) where `n_results` is the number
#'   of clusters ACTUALLY produced - an observed outcome, not the frozen `k`.
run_pattern_profile <- function(vst_mat, metadata, de_result, group_column,
                                inputs = .bulk_pattern_drive_inputs()) {
  if (!is.list(inputs)) inputs <- .bulk_pattern_drive_inputs()
  if (is.null(vst_mat) || !is.matrix(vst_mat) || !is.numeric(vst_mat)) {
    .bulk_pattern_stop("invalid_input",
                       "bulk pattern: the VST matrix is missing or is not numeric.")
  }
  if (!is.data.frame(metadata) || !ncol(metadata)) {
    .bulk_pattern_stop("invalid_input", "bulk pattern: the metadata are missing.")
  }
  if (!is.data.frame(de_result) || !nrow(de_result)) {
    .bulk_pattern_stop("invalid_input",
                       "bulk pattern: the active contrast carries no result.")
  }
  if (!is.character(group_column) || length(group_column) != 1L || is.na(group_column)) {
    .bulk_pattern_stop("invalid_input", "bulk pattern: no usable group column.")
  }

  # Gene selection, identical to the human path, but on the FROZEN thresholds.
  sig <- de_result$padj < inputs$padj_thresh &
    abs(de_result$log2FoldChange) > inputs$lfc_thresh
  sig[is.na(sig)] <- FALSE
  genes <- switch(inputs$pattern_source,
                  up      = de_result$gene[sig & de_result$log2FoldChange > 0],
                  down    = de_result$gene[sig & de_result$log2FoldChange < 0],
                  all_sig = de_result$gene[sig])
  genes <- unique(trimws(genes[nchar(trimws(as.character(genes))) > 0]))
  genes <- genes[!is.na(genes)]
  # The same floor as the human path (mod_bulk_pattern.R:118): below it the
  # clustering is not worth running, and saying so beats returning a `done` that
  # clustered almost nothing.
  if (length(genes) < 10L) {
    .bulk_pattern_stop("no_gene_sets",
                       sprintf("bulk pattern: only %d gene(s) pass the frozen thresholds; widen the source or the thresholds.",
                               length(genes)))
  }

  res <- run_pattern_clustering(
    vst_mat      = vst_mat,
    metadata     = metadata,
    group_column = group_column,
    genes        = genes,
    k            = inputs$pattern_k,
    seed         = inputs$pattern_seed
  )
  assert_bulk_pattern_result(res, context = "drive clustering de profils")
  list(ok = TRUE, n_results = length(unique(res$clusters$cluster)),
       record = res, k = res$k, seed = res$seed)
}

#' Readiness, in the same order as the human path: a dataset, Step 1's VST
#' matrix, an active contrast from Step 2, and a resolvable group column.
#' Returning the REASON is what lets the poller publish `not_ready` instead of
#' dispatching a job that must fail.
.bulk_pattern_drive_ready <- function(shared_rv, global_data) {
  if (is.null(tryCatch(shiny::isolate(global_data$bulk_obj), error = function(e) NULL))) {
    return("no bulk object loaded (global_data$bulk_obj is NULL)")
  }
  if (is.null(tryCatch(shiny::isolate(shared_rv$vst_mat), error = function(e) NULL))) {
    return("Step 1 has not produced a VST matrix (shared_rv$vst_mat is NULL)")
  }
  ac <- tryCatch(shiny::isolate(shared_rv$active_contrast), error = function(e) NULL)
  contrasts <- tryCatch(shiny::isolate(shared_rv$contrasts), error = function(e) NULL)
  if (is.null(ac) || is.null(contrasts) || !ac %in% names(contrasts)) {
    return("Step 2 has not produced an active contrast")
  }
  g <- .bulk_pattern_group_column(shared_rv, global_data)
  if (!is.null(g$reason)) return(as.character(g$reason))
  TRUE
}

.bulk_pattern_drive_view <- function(status, elapsed_s = 0, seq = 0L,
                                    n_results = 0L, has_data = FALSE,
                                    ready = FALSE, step = NULL) {
  step <- if (is.null(step)) "skipped" else as.character(step)
  if (length(step) != 1L || is.na(step) || !step %in% .BULK_PATTERN_DRIVE_STEP_STATES) {
    step <- "error"
  }
  list(
    module = .BULK_PATTERN_DRIVE_MODULE,
    action = "run_pipeline",
    status = as.character(status),
    elapsed_s = as.numeric(elapsed_s),
    seq = as.integer(seq),
    n_results = as.integer(n_results),
    has_data = isTRUE(has_data),
    ready = isTRUE(ready),
    steps = list(pattern = step)
  )
}

#' State probe. `n_results` counts the clusters the last successful run recorded,
#' read from the stored record's `clusters` column; `shiny::isolate()` keeps these
#' reads out of the poller's dependency set.
.bulk_pattern_drive_state <- function(shared_rv, global_data, run_state,
                                      last_step = NULL) {
  job <- tryCatch(ts_drive_job_state(), error = function(e) NULL)
  pending <- tryCatch(ts_drive_job_pending(), error = function(e) NULL)
  ready <- isTRUE(.bulk_pattern_drive_ready(shared_rv, global_data))
  obj <- tryCatch(shiny::isolate(global_data$bulk_obj), error = function(e) NULL)
  res <- tryCatch(shiny::isolate(shared_rv$pattern_result), error = function(e) NULL)
  n_results <- if (is.list(res) && is.data.frame(res$clusters) && nrow(res$clusters)) {
    length(unique(res$clusters$cluster))
  } else 0L
  current <- if (is.function(run_state)) {
    shiny::isolate(run_state())
  } else {
    as.character(run_state)
  }
  if (length(current) != 1L || is.na(current)) current <- "idle"
  .bulk_pattern_drive_view(
    status = if (ready) current else "not_ready",
    elapsed_s = if (is.null(pending)) 0 else as.numeric(pending$elapsed_s),
    seq = if (is.null(job)) 0L else as.integer(job$seq),
    n_results = n_results,
    has_data = !is.null(obj),
    ready = ready,
    step = last_step
  )
}

#' Runs the wrapper, publishes through the shared writer, and reports the
#' terminal state. A failure publishes NO result count, so a stale clustering can
#' never be read next to an error.
.bulk_pattern_run_drive <- function(global_data, shared_rv, close_job) {
  vst <- tryCatch(shiny::isolate(shared_rv$vst_mat), error = function(e) NULL)
  meta <- tryCatch(global_data$bulk_obj$metadata, error = function(e) NULL)
  ac <- tryCatch(shiny::isolate(shared_rv$active_contrast), error = function(e) NULL)
  contrasts <- tryCatch(shiny::isolate(shared_rv$contrasts), error = function(e) NULL)
  de <- if (!is.null(ac) && !is.null(contrasts) && ac %in% names(contrasts)) {
    contrasts[[ac]]
  } else NULL
  g <- .bulk_pattern_group_column(shared_rv, global_data)
  col <- g$column

  res <- tryCatch(
    run_pattern_profile(vst, meta, de, col, .bulk_pattern_drive_inputs()),
    error = function(e) e
  )
  if (inherits(res, "condition")) {
    close_job("error", conditionMessage(res))
    return(list(status = "error", n_results = 0L, step = "error"))
  }
  .bulk_pattern_store(res$record, shared_rv)
  n_results <- as.integer(res$n_results %||% 0L)
  if (length(n_results) != 1L || is.na(n_results) || n_results < 0L) n_results <- 0L
  # The JOB vocabulary is narrower than the VIEW vocabulary: `done` / `error` /
  # `invalid` / `timeout` / `session_lost` only. An empty result is a job that
  # COMPLETED, so it closes as `done` while the view reports `empty`.
  close_job("done", NULL)
  list(status = if (n_results == 0L) "empty" else "done",
       n_results = n_results, step = "ran")
}


mod_bulk_pattern_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-light", style = "font-size:0.9em;border-left:3px solid #F39C12;",
        i18n$t("Regroupe les gènes par forme de profil d'expression entre groupes (moyenne VST par groupe, z-score par gène, kmeans).")),
    radioButtons(ns("pattern_source"), i18n$t("Source de gènes"),
                 choices = setNames(c("up", "down", "all_sig"),
                                    c(.tr_plain("Surexprimés (up)"), .tr_plain("Sous-exprimés (down)"), .tr_plain("Tous les significatifs")))),
    selectInput(ns("pattern_group"), i18n$t("Colonne de groupe (profils)"), choices = NULL),
    fluidRow(
      column(6, numericInput(ns("pattern_k"), i18n$t("Nombre de clusters (k)"),
                             value = 4, min = 2, max = 12, step = 1)),
      column(6, numericInput(ns("pattern_seed"), i18n$t("Graine (reproductibilité)"),
                             value = 15, min = 1, step = 1))
    ),
    actionButton(ns("run_pattern"), i18n$t("Lancer le clustering"), class = "btn-warning w-100", icon = icon("chart-line")),
    hr(),
    div(class = "small text-muted", textOutput(ns("pattern_status")))
  )
}

mod_bulk_pattern_output_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header(div(style = "display:flex;justify-content:space-between;align-items:center;",
                    h5(i18n$t("Clustering de profils (descriptif)"), class = "mb-0"),
                    downloadButton(ns("dl_pattern"), i18n$t("Export CSV"), class = "btn-sm btn-info"))),
    navset_tab(
      nav_panel(i18n$t("Profils"), plotOutput(ns("pattern_plot"), height = "560px")),
      nav_panel(i18n$t("Gènes par cluster"), DTOutput(ns("pattern_table")))
    )
  )
}

mod_bulk_pattern_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {

    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # ── i18n push on language switch ─────────────────────────────────────
    observeEvent(global_data$language, {
      updateRadioButtons(session, "pattern_source", label = .tr("Source de gènes"),
        choices = setNames(c("up", "down", "all_sig"),
                           c(.tr("Surexprimés (up)"), .tr("Sous-exprimés (down)"), .tr("Tous les significatifs"))),
        selected = isolate(input$pattern_source) %||% "up")
      updateSelectInput(session, "pattern_group", label = .tr("Colonne de groupe (profils)"))
      updateNumericInput(session, "pattern_k", label = .tr("Nombre de clusters (k)"))
      updateNumericInput(session, "pattern_seed", label = .tr("Graine (reproductibilité)"))
      updateActionButton(session, "run_pattern", label = .tr("Lancer le clustering"))
    }, ignoreInit = TRUE)

    # Colonnes de groupe candidates : facteur/character avec au moins 2 niveaux
    observeEvent(global_data$bulk_obj, {
      req(global_data$bulk_obj$metadata)
      meta <- global_data$bulk_obj$metadata
      ok <- vapply(names(meta), function(cl) {
        x <- meta[[cl]]
        (is.factor(x) || is.character(x)) &&
          length(unique(stats::na.omit(as.character(x)))) >= 2L
      }, logical(1))
      updateSelectInput(session, "pattern_group", choices = names(meta)[ok])
    }, ignoreNULL = TRUE)

    observe({
      shinyjs::toggleState("run_pattern", condition = !is.null(shared_rv$vst_mat))
    })

    # Contraste DE actif — même convention que mod_bulk_pathways
    .active_de_results <- function() {
      ac <- shared_rv$active_contrast
      if (is.null(ac) || !ac %in% names(shared_rv$contrasts)) return(NULL)
      shared_rv$contrasts[[ac]]
    }

    output$pattern_status <- renderText({
      global_data$language
      res <- shared_rv$pattern_result
      if (is.null(res)) .tr("En attente — lancez d'abord le Filtrage & VST (étape 1) et l'analyse différentielle (étape 2).")
      else .t_fmt(.tr("\u2713 {g} g\u00e8nes regroup\u00e9s en {k} clusters ({c} groupes, graine {s})."),
                  g = format(res$summary$n_genes_used, big.mark = ","),
                  k = res$k, c = res$summary$n_groups, s = res$seed)
    })

    observeEvent(input$run_pattern, {
      req(shared_rv$vst_mat)
      res_de <- .active_de_results()
      if (is.null(res_de)) {
        showNotification(.tr("⚠️ Lancez d'abord l'étape 2 (Analyse Différentielle)."), type = "warning")
        return()
      }
      sig <- res_de$padj < (shared_rv$padj_thresh %||% 0.05) &
        abs(res_de$log2FoldChange) > (shared_rv$lfc_thresh %||% 1)
      sig[is.na(sig)] <- FALSE
      genes <- switch(input$pattern_source,
                      up      = res_de$gene[sig & res_de$log2FoldChange > 0],
                      down    = res_de$gene[sig & res_de$log2FoldChange < 0],
                      all_sig = res_de$gene[sig])
      genes <- unique(trimws(genes[nchar(trimws(as.character(genes))) > 0]))
      if (length(genes) < 10) {
        showNotification(.t_fmt(.tr("⚠️ Trop peu de gènes ({n}) — élargissez la liste (source ou seuils)."),
                                n = length(genes)), type = "warning", duration = 6)
        return()
      }
      # ⚠️ `add = TRUE` is LOAD-BEARING in this file since Phase D: this module
      # now declares a drive job, and test-drive-watcher.R refuses any file that
      # calls `ts_drive_job_finish()` and also carries a BARE `on.exit()`, because
      # a bare call REPLACES the registered expressions and would discard the job
      # declaration. Same fix as mod_bulk_signatures.R.
      p <- shiny::Progress$new(); on.exit(p$close(), add = TRUE)
      p$set(message = .tr("Clustering des profils (kmeans)..."), value = 0.3)
      tryCatch({
        res <- run_pattern_clustering(
          vst_mat      = shared_rv$vst_mat,
          metadata     = global_data$bulk_obj$metadata,
          group_column = input$pattern_group,
          genes        = genes,
          k            = input$pattern_k,
          seed         = input$pattern_seed
        )
        assert_bulk_pattern_result(res, context = "module clustering de profils")
        # The SAME writer the drive path uses: one shape, written once.
        .bulk_pattern_store(res, shared_rv)
        showNotification(.t_fmt(.tr("\u2713 {g} g\u00e8nes regroup\u00e9s en {k} clusters."),
                                 g = format(res$summary$n_genes_used, big.mark = ","),
                                 k = res$k), type = "message")
      }, error = function(e) {
        showNotification(paste(.tr("Erreur clustering:"), conditionMessage(e)),
                         type = "error", duration = 10)
        shared_rv$pattern_result <- NULL
      })
    })

    # ── DRIVE (MCP) — bulk-pattern-run_pattern ───────────────────────────────
    # The human observer above is NOT re-wired and NOT duplicated: the drive
    # action calls `run_pattern_profile()`, which repeats the same five steps on
    # the FROZEN inputs and the rule-resolved group column, then publishes through
    # the same `.bulk_pattern_store()`. `pat_drive_counter` is the only trigger,
    # so no `input$` is read here - that is what makes a frozen input set
    # possible at all, and it is also why the group column needs a RULE.
    pat_drive_counter <- shiny::reactiveVal(0L)
    pat_drive_run_state <- shiny::reactiveVal("idle")
    pat_drive_last_step <- new.env(parent = emptyenv())
    pat_drive_last_step$value <- NULL
    pat_drive_job <- new.env(parent = emptyenv())
    pat_drive_job$id <- NULL

    pat_drive_ready <- function() {
      .bulk_pattern_drive_ready(shared_rv, global_data)
    }
    pat_drive_state <- function() {
      .bulk_pattern_drive_state(shared_rv, global_data,
                                pat_drive_run_state,
                                pat_drive_last_step$value)
    }

    # `long = TRUE` gives `running` a real producer: the job is SYNCHRONOUS, so no
    # tick runs during it. No `timeout_s` is declared, matching the other Bulk long
    # jobs, so the poller's default ceiling applies.
    #
    # The button id is a LITERAL here on purpose: test-drive-watcher.R greps every
    # TS_DRIVE_BUTTONS entry as a quoted literal beside a `ts_drive_publish_token(`
    # call, and that shared check is what proves an allowlist entry is really
    # wired. The assertions keep the literal and the constant from drifting.
    ts_drive_publish_token(global_data, "bulk-pattern-run_pattern",
      pat_drive_counter, ready = pat_drive_ready,
      state = pat_drive_state, long = TRUE)

    close_pat_drive_job <- function(status, error = NULL) {
      if (is.null(pat_drive_job$id)) return(invisible(FALSE))
      job <- tryCatch(ts_drive_job_state(), error = function(e) NULL)
      if (is.null(job) || !identical(job$job_id, pat_drive_job$id)) {
        return(invisible(FALSE))
      }
      ts_drive_job_finish(.BULK_PATTERN_DRIVE_BUTTON, status = status,
                          error = error, job_id = pat_drive_job$id)
    }

    observeEvent(pat_drive_counter(), {
      if (!isTRUE(pat_drive_ready())) return()
      pat_drive_job$id <- NULL
      job <- tryCatch(ts_drive_job_state(), error = function(e) NULL)
      if (!is.null(job) && isTRUE(ts_drive_job_busy()) &&
          identical(job$button, .BULK_PATTERN_DRIVE_BUTTON)) {
        pat_drive_job$id <- job$job_id
      }
      pat_drive_run_state("running")
      res <- .bulk_pattern_run_drive(global_data, shared_rv, close_pat_drive_job)
      pat_drive_last_step$value <- res$step
      pat_drive_run_state(res$status)
    }, ignoreInit = TRUE)

    output$pattern_plot <- renderPlot({
      global_data$language
      req(shared_rv$pattern_result)
      plot_pattern_profiles(shared_rv$pattern_result, tr = .tr,
                            palette = "default", manual_colors = NULL)
    })

    output$pattern_table <- renderDT({
      req(shared_rv$pattern_result)
      tab <- build_pattern_table_export(shared_rv$pattern_result)
      ts_datatable(tab, page_length = 15L, filename_base = "pattern_clusters")
    })

    output$dl_pattern <- downloadHandler(
      filename = function() {
        req(shared_rv$pattern_result)
        paste0("pattern_clusters_k", shared_rv$pattern_result$k, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(shared_rv$pattern_result)
        utils::write.csv(build_pattern_table_export(shared_rv$pattern_result),
                         file, row.names = FALSE)
      }
    )
    # Slice 2.3: publish THIS module's drive export route — the gene/cluster
    # table built by build_pattern_table_export() (the same builder the human
    # dl_pattern writes). The exporter lives in mod_bulk_pattern_export.R so
    # the offline tests can reach it.
    ts_drive_publish_export(global_data, "bulk_pattern", function() {
      bulk_pattern_export_clusters_csv(shared_rv, global_data)
    })
  })
}

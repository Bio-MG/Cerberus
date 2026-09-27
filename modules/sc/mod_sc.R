# =============================================================================
# mod_sc.R  —  Parent Router Module (Step-3.7)
# =============================================================================
# Step-3.6 changes (recap): report_viz_list + traj_reduction in reactiveValues,
# "0. Mapping IDs" panel, saved-viz basket, extended auto-pipeline, pipeline
# status bar + Résumé Pipeline tab.
#
# Step-3.7 changes:
#   [1] Auto-pipeline: secondary t-SNE always computed after UMAP (capped via
#       .AUTO_TSNE_MAX_CELLS, defined in mod_sc_pipeline.R) — same rationale
#       as the standalone "1. Pipeline" module: PCA/UMAP/t-SNE all available
#       in the Viz "Réduction à visualiser" picker without an extra manual run.
#   [2] Auto-pipeline: FindAllMarkers / Gene Correlation steps now run on a
#       RAM-safety-capped subsample (shared_rv$max_cells_heavy, set in
#       "1. Pipeline") instead of always the full object.
#   [3] render_params$traj_genes forwarded to the report (mirrors
#       shared_rv$traj_genes, written live by mod_sc_trajectory.R) so the new
#       "Gènes vs Pseudotemps" report section renders the same genes the user
#       was looking at live.
# =============================================================================

# =============================================================================
# Drive live control — SC auto-pipeline (ONE frozen action)
# =============================================================================
# `sc-btn_auto_pipeline_sc` is NOT drivable: it only calls showModal() (the
# pipeline runs from `sc_ap_confirm` inside that modal, see the observeEvent
# below). The protocol therefore owns its own id — `sc-pipeline-run_auto_pipeline`,
# mirrored in R/core/drive_allowlist.R — dispatched by a counter this module
# publishes. The human modal flow is untouched: zero behaviour change.
#
# These helpers are FILE-LEVEL on purpose. `mod_sc_server()` fans out to ~20
# sibling servers, so `testServer()` on it would need the whole application
# mocked; keeping the logic here lets tests/testthat/test-mod-sc-drive.R drive
# it directly, exactly as test-sc-auto-pipeline.R drives run_sc_auto_pipeline().

.SC_AP_DRIVE_MODULE <- "sc_pipeline"
.SC_AP_DRIVE_BUTTON <- "sc-pipeline-run_auto_pipeline"

# The step vocabulary, CLOSED. `ignored` is not a synonym for `skipped`: a step
# can be SELECTED, attempted, and declined by the pipeline itself (a size guard
# such as .AUTO_TSNE_MAX_CELLS) — that is neither a success nor a failure, and
# collapsing the three would be a lie in either direction.
.SC_AP_DRIVE_STEP_STATES <- c("skipped", "running", "ran", "ignored", "error")

.SC_AP_DRIVE_STEPS <- c("mapping", "qc", "norm", "pca", "clusters", "umap",
                        "tsne", "singler", "markers", "pathway",
                        "correlation", "trajectory")

#' The FROZEN, DECLARED parameter set of the drive action.
#'
#' Equal to the set tests/testthat/test-sc-auto-pipeline.R already runs
#' end-to-end, so the drive path performs a computation the repository has
#' proven rather than a new one. Two choices are deliberate:
#'   * `sc_ap_sketch_preset = "max"` — FULL dataset. The drive must never depend
#'     on a hidden sub-sampling decision.
#'   * `sc_ap_bpcells = FALSE` — the action performs no disk write, so the Drive
#'     channel stays the only thing this path touches outside the session.
#' Every key run_sc_auto_pipeline() reads is present: a missing one would arrive
#' as NULL and silently change a threshold.
.sc_ap_drive_inputs <- function() {
  list(
    sc_ap_mapping              = FALSE,
    sc_ap_mapping_org          = "human",
    sc_ap_bpcells              = FALSE,
    sc_ap_min_gene             = 10,
    sc_ap_max_gene             = 10000,
    sc_ap_mt                   = 50,
    sc_ap_norm                 = "log",
    sc_ap_pca_dim              = 10,
    sc_ap_res                  = 0.5,
    sc_ap_cluster_algo         = "1",
    sc_ap_compute_umap         = TRUE,
    sc_ap_sketch_preset        = "max",
    sc_ap_sketch_ncells_custom = NA,
    sc_ap_singler              = FALSE,
    sc_ap_singler_ref          = "hpca",
    sc_ap_singler_level        = "main",
    sc_ap_markers              = TRUE,
    sc_ap_pathway              = FALSE,
    sc_ap_pathway_db           = "GOBP",
    sc_ap_pathway_org          = "human",
    sc_ap_correlation          = FALSE,
    sc_ap_trajectory           = TRUE
  )
}

#' Which steps the frozen set asks for. The always-on stages (QC, normalisation,
#' PCA, clustering) have no toggle and are therefore always selected.
.sc_ap_drive_selection <- function() {
  out <- c(
    mapping     = isTRUE(.sc_ap_drive_inputs()$sc_ap_mapping),
    qc          = TRUE,
    norm        = TRUE,
    pca         = TRUE,
    clusters    = TRUE,
    umap        = isTRUE(.sc_ap_drive_inputs()$sc_ap_compute_umap),
    tsne        = isTRUE(.sc_ap_drive_inputs()$sc_ap_compute_umap),
    singler     = isTRUE(.sc_ap_drive_inputs()$sc_ap_singler),
    markers     = isTRUE(.sc_ap_drive_inputs()$sc_ap_markers),
    pathway     = isTRUE(.sc_ap_drive_inputs()$sc_ap_pathway),
    correlation = isTRUE(.sc_ap_drive_inputs()$sc_ap_correlation),
    trajectory  = isTRUE(.sc_ap_drive_inputs()$sc_ap_trajectory)
  )
  out[.SC_AP_DRIVE_STEPS]
}

#' A fresh step recorder.
#'
#' An ENVIRONMENT, never a list: a list is copied on every assignment, so a write
#' from inside a callback would land in a copy and be lost without a warning.
#' (Already paid once on the Spatial pipeline.)
.sc_ap_steps_new <- function(selection) {
  steps <- new.env(parent = emptyenv())
  for (s in .SC_AP_DRIVE_STEPS) {
    assign(s, if (isTRUE(selection[[s]])) "running" else "skipped", envir = steps)
  }
  steps
}

.sc_ap_step_set <- function(steps, step, value) {
  if (!is.character(step) || length(step) != 1L || !step %in% .SC_AP_DRIVE_STEPS) {
    stop("unknown SC auto-pipeline drive step", call. = FALSE)
  }
  if (!is.character(value) || length(value) != 1L ||
      !value %in% .SC_AP_DRIVE_STEP_STATES) {
    stop("unknown SC auto-pipeline drive step state", call. = FALSE)
  }
  assign(step, value, envir = steps)
  invisible(TRUE)
}

.sc_ap_steps_as_list <- function(steps) {
  out <- lapply(.SC_AP_DRIVE_STEPS, function(s) as.character(steps[[s]]))
  names(out) <- .SC_AP_DRIVE_STEPS
  out
}

#' The step record BEFORE any run: nothing has been attempted, so no step may
#' claim work. `.sc_ap_steps_new()` would say `running` for every selected step,
#' which is a lie when no job is in flight; the authoritative "nothing happened
#' yet" signal is the top-level `status = "idle"`, and `skipped` here means "no
#' work was done for this step".
.sc_ap_steps_idle <- function() {
  steps <- new.env(parent = emptyenv())
  for (s in .SC_AP_DRIVE_STEPS) assign(s, "skipped", envir = steps)
  steps
}

#' Derive the per-step outcome from OBSERVABLE state, never from the request.
#'
#' `run_sc_auto_pipeline()` returns nothing and swallows its error (decision B1:
#' the function and its source lock stay untouched), so the only honest record is
#' built from what the session can be seen to hold afterwards. A selected step
#' with no result is reported `error` — the conservative direction: it can never
#' claim a success that cannot be proven. A size guard that fired is `ignored`.
.sc_ap_step_outcomes <- function(obj, selection, produced = list(),
                                 n_genes_before = NA_integer_,
                                 tsne_max = 30000L, traj_max = 100000L) {
  steps  <- .sc_ap_steps_new(selection)
  is_obj <- inherits(obj, "Seurat")
  n_cells <- if (is_obj) ncol(obj) else 0L
  meta <- if (is_obj) obj@meta.data else data.frame()
  reds <- if (is_obj) names(obj@reductions) else character(0)
  has_col <- function(x) is_obj && x %in% colnames(meta)
  mark <- function(step, value) .sc_ap_step_set(steps, step, value)

  nvf <- if (is_obj) {
    length(tryCatch(Seurat::VariableFeatures(obj), error = function(e) character(0)))
  } else 0L

  mark("qc",       if (has_col("percent.mt")) "ran" else "error")
  mark("norm",     if (nvf > 0L) "ran" else "error")
  mark("pca",      if (any(c("pca", "pca.full") %in% reds)) "ran" else "error")
  mark("clusters", if (has_col("seurat_clusters")) "ran" else "error")

  # Mapping rewrites rownames in place; a collapsed duplicate set changes the
  # gene count. An unchanged count means the detector found symbols already, so
  # the step was a deliberate no-op.
  mark("mapping", if (!isTRUE(selection[["mapping"]])) "skipped"
       else if (is_obj && !is.na(n_genes_before) && nrow(obj) != n_genes_before) "ran"
       else "ignored")

  mark("umap", if (!isTRUE(selection[["umap"]])) "skipped"
       else if ("umap" %in% reds) "ran" else "error")
  mark("tsne", if (!isTRUE(selection[["tsne"]])) "skipped"
       else if ("tsne" %in% reds) "ran"
       else if (n_cells > tsne_max) "ignored" else "error")
  mark("singler", if (!isTRUE(selection[["singler"]])) "skipped"
       else if (any(grepl("^SingleR_", colnames(meta)))) "ran" else "error")
  mark("markers", if (!isTRUE(selection[["markers"]])) "skipped"
       else if (isTRUE(produced$markers)) "ran" else "error")
  mark("pathway", if (!isTRUE(selection[["pathway"]])) "skipped"
       else if (isTRUE(produced$pathway)) "ran" else "error")
  mark("correlation", if (!isTRUE(selection[["correlation"]])) "skipped"
       else if (isTRUE(produced$correlation)) "ran" else "error")
  mark("trajectory", if (!isTRUE(selection[["trajectory"]])) "skipped"
       else if (has_col("pseudotime")) "ran"
       else if (n_cells > traj_max) "ignored" else "error")

  .sc_ap_steps_as_list(steps)
}

#' Which optional result slots the shared state holds RIGHT NOW.
#'
#' Presence only — never the content. The gene, cell and sample names these
#' slots carry are exactly what the snapshot disclosure contract forbids, so
#' they are read as booleans and never projected.
.sc_ap_produced <- function(shared_rv) {
  has <- function(slot) {
    !is.null(tryCatch(shiny::isolate(state_get(shared_rv, slot)),
                      error = function(e) NULL))
  }
  list(markers = has("markers_data"),
       pathway = has("pathway_results"),
       correlation = has("correlated_genes"))
}

.sc_ap_count_results <- function(obj, produced) {
  n <- 0L
  if (inherits(obj, "Seurat")) {
    n <- n + sum(c("pca", "pca.full", "umap", "tsne") %in% names(obj@reductions))
    n <- n + sum(c("seurat_clusters", "pseudotime") %in% colnames(obj@meta.data))
  }
  n + sum(vapply(produced, isTRUE, logical(1)))
}

#' The published state, as a CLOSED contract.
#'
#' Eight scalars plus the `steps` slot: nothing here can carry a gene name, a
#' cell id, a coordinate, a cluster label or a path, because nothing here reads
#' one.
 .sc_ap_drive_view <- function(status, elapsed_s = 0, seq = 0L, n_results = 0L,
                                has_data = FALSE, ready = FALSE, steps = NULL,
                                descriptor = NULL, convention = NULL) {
    out <- list(
      module    = .SC_AP_DRIVE_MODULE,
    action    = "run_pipeline",
    status    = as.character(status),
    elapsed_s = as.numeric(elapsed_s),
    seq       = as.integer(seq),
    n_results = as.integer(n_results),
    has_data  = isTRUE(has_data),
    ready     = isTRUE(ready),
      steps     = if (is.null(steps)) {
        .sc_ap_steps_as_list(.sc_ap_steps_idle())
      } else {
        steps
      }
    )
    # See `.sc_markers_drive_view()`. The auto-pipeline counts SEVERAL artefacts,
    # so its convention has to say so, or its number is not comparable with a
    # single-table module's.
    if (!is.null(descriptor)) out$descriptor <- descriptor
    if (!is.null(convention)) out$convention <- as.character(convention)[1L]
    out
  }

#' Run the pipeline for a drive request and report what actually happened.
#'
#' Never throws: the job must reach a terminal state whatever happens, or the
#' agent is left with a `running` job it can never resolve. `close_job` is the
#' module's job-closing callback (it verifies the job id before writing, so a
#' human click can never close an agent's job).
.sc_ap_run_drive <- function(global_data, shared_rv, session, sc_log_rv, close_job) {
  obj_before <- tryCatch(shiny::isolate(global_data$sc_obj), error = function(e) NULL)
  n_genes_before <- if (inherits(obj_before, "Seurat")) nrow(obj_before) else NA_integer_
  log_before <- tryCatch(as.character(shiny::isolate(sc_log_rv())),
                         error = function(e) "")

  failed <- FALSE
  tryCatch(
    run_sc_auto_pipeline(.sc_ap_drive_inputs(), global_data, shared_rv,
                         session, sc_log_rv),
    error = function(e) failed <<- TRUE
  )

  # The swallowed error, recovered from the log. MEASURED in
  # i18n/translation.json: the marker `.tr("\u274c Erreur:")` carries the emoji
  # in BOTH the fr and en values, so it is language-independent — unlike the
  # word that follows it, which is exactly the part a translation may change.
  log_after <- tryCatch(as.character(shiny::isolate(sc_log_rv())),
                        error = function(e) "")
  if (nchar(log_after) > nchar(log_before)) {
    delta <- substr(log_after, nchar(log_before) + 1L, nchar(log_after))
    if (grepl("\u274c", delta, fixed = TRUE)) failed <- TRUE
  }

  obj <- tryCatch(shiny::isolate(global_data$sc_obj), error = function(e) NULL)
  produced <- .sc_ap_produced(shared_rv)
  steps <- .sc_ap_step_outcomes(
    obj, .sc_ap_drive_selection(), produced = produced,
    n_genes_before = n_genes_before
  )
  status <- if (isTRUE(failed)) "error" else "done"
  close_job(status,
            if (isTRUE(failed))
              "the SC auto-pipeline reported an error (see the module log)" else NULL)
  list(status = status, steps = steps,
       n_results = .sc_ap_count_results(obj, produced))
}

.SC_ANNOT_DRIVE_MODULE <- "sc_annotation"
.SC_ANNOT_DRIVE_BUTTON <- "sc-annotation-run_annot"

.sc_annot_drive_inputs <- function() {
  list(
    ref_singler = "hpca",
    label_level = "main",
    maxcells = 50000L
  )
}

run_annot <- function(input, global_data, shared_rv, session) {
  obj <- tryCatch(shiny::isolate(global_data$sc_obj), error = function(e) NULL)
  if (is.null(obj)) return(list(ok = FALSE, n_results = 0L))
  tryCatch({
    p <- shiny::Progress$new()
    on.exit(p$close(), add = TRUE)
    p$set(message = "Annotation SingleR...", value = 0.1)
    result <- withCallingHandlers(
      .run_singler_safe(obj, input$ref_singler, input$label_level,
                        maxcells = input$maxcells),
      warning = function(w) invokeRestart("muffleWarning")
    )
    col_name <- paste0("SingleR_", input$ref_singler, "_", input$label_level)
    obj[[col_name]] <- result$labels
    Idents(obj) <- result$labels
    global_data$sc_obj <- obj
    shared_rv$active_tab <- "tab_viz"
    list(ok = TRUE,
         n_results = as.integer(length(unique(result$labels))),
         method = as.character(result$method))
  }, error = function(e) list(ok = FALSE, n_results = 0L))
}

 .sc_annot_drive_view <- function(status, elapsed_s = 0, seq = 0L,
                                   n_results = 0L, has_data = FALSE,
                                   ready = FALSE, step = NULL,
                                   descriptor = NULL, convention = NULL) {
    step <- if (is.null(step)) "skipped" else as.character(step)
    if (length(step) != 1L || is.na(step) || !step %in% .SC_AP_DRIVE_STEP_STATES) {
      step <- "error"
    }
    out <- list(
      module = .SC_ANNOT_DRIVE_MODULE,
    action = "run_pipeline",
    status = as.character(status),
    elapsed_s = as.numeric(elapsed_s),
    seq = as.integer(seq),
    n_results = as.integer(n_results),
    has_data = isTRUE(has_data),
    ready = isTRUE(ready),
      steps = list(singler = step)
    )
    # See `.sc_markers_drive_view()`: the descriptor is what gives `n_results` a
    # referent and a convention. ABSENT, never empty, when there is no result.
    if (!is.null(descriptor)) out$descriptor <- descriptor
    if (!is.null(convention)) out$convention <- as.character(convention)[1L]
    out
  }

.sc_annot_drive_ready <- function(global_data) {
  if (is.null(tryCatch(shiny::isolate(global_data$sc_obj), error = function(e) NULL))) {
    return("sc_obj is not loaded")
  }
  TRUE
}

.sc_annot_drive_state <- function(global_data, run_state, last_step = NULL) {
  job <- tryCatch(ts_drive_job_state(), error = function(e) NULL)
  pending <- tryCatch(ts_drive_job_pending(), error = function(e) NULL)
  ready <- isTRUE(.sc_annot_drive_ready(global_data))
  obj <- tryCatch(shiny::isolate(global_data$sc_obj), error = function(e) NULL)
  n_results <- 0L
  annot_col <- NULL
  if (inherits(obj, "Seurat")) {
    cols <- grep("^SingleR_", colnames(obj@meta.data), value = TRUE)
    if (length(cols)) {
      annot_col <- tail(cols, 1L)
      n_results <- length(unique(obj@meta.data[[annot_col]]))
    }
  }
  current <- if (is.function(run_state)) {
    shiny::isolate(run_state())
  } else {
    as.character(run_state)
  }
  .sc_annot_drive_view(
    status = if (ready) current else "not_ready",
    elapsed_s = if (is.null(pending)) 0 else as.numeric(pending$elapsed_s),
    seq = if (is.null(job)) 0L else as.integer(job$seq),
    n_results = n_results,
    has_data = !is.null(obj),
    ready = ready,
    step = last_step,
    # Annotation has no table, so the referent for its level count is the COLUMN
    # the count came from. NULL when SingleR has not run, and then omitted.
    descriptor = if (is.null(annot_col)) NULL else ts_drive_table_descriptor(
      NULL, kind = "levels", n_levels = n_results, column = annot_col,
      convention = .SC_ANNOT_DRIVE_CONVENTION),
    convention = .SC_ANNOT_DRIVE_CONVENTION
  )
}

.sc_annot_run_drive <- function(global_data, shared_rv, session, close_job) {
  result <- tryCatch(
    run_annot(.sc_annot_drive_inputs(), global_data, shared_rv, session),
    error = function(e) list(ok = FALSE, n_results = 0L)
  )
  ok <- isTRUE(result$ok)
  n_results <- suppressWarnings(as.integer(result$n_results %||% 0L))
  if (length(n_results) != 1L || is.na(n_results) || n_results < 0L) n_results <- 0L
  if (!ok) n_results <- 0L
  status <- if (ok) "done" else "error"
  close_job(status, if (ok) NULL else "the SC annotation action reported an error")
  list(status = status, n_results = n_results,
       step = if (ok) "ran" else "error")
}

.SC_MARKERS_DRIVE_MODULE <- "sc_markers"
.SC_MARKERS_DRIVE_BUTTON <- "sc-markers-run_markers"

.sc_markers_drive_inputs <- function() {
  list(
    marker_test = "wilcox",
    marker_min_pct = 0.10,
    marker_logfc = 0.25,
    group_col = "seurat_clusters",
    max_per_group = 5000L,
    only_pos = TRUE,
    verbose = FALSE
  )
}

run_markers <- function(input, global_data, shared_rv, session) {
  obj <- tryCatch(shiny::isolate(global_data$sc_obj), error = function(e) NULL)
  if (is.null(obj)) return(list(ok = FALSE, n_results = 0L))
  tryCatch({
    p <- shiny::Progress$new()
    on.exit(p$close(), add = TRUE)
    p$set(message = "Recherche Marqueurs...", value = 0.3)
    groups <- obj@meta.data[[input$group_col]]
    if (length(unique(groups)) < 2L) {
      stop(errorCondition("Au moins 2 groupes nécessaires",
                          class = "sc_markers_error"))
    }
    Idents(obj) <- as.factor(groups)
    sub_res <- subsample_seurat_for_analysis(
      obj, max_per_group = input$max_per_group,
      group_col = input$group_col
    )
    obj_use <- sub_res$object
    Idents(obj_use) <- as.factor(obj_use@meta.data[[input$group_col]])
    bpc <- tryCatch(optimize_bpcells_for_markers(obj_use), error = function(e) NULL)
    if (!is.null(bpc) && isTRUE(bpc$transposed)) {
      obj_use <- bpc$object
      Idents(obj_use) <- as.factor(obj_use@meta.data[[input$group_col]])
      session$onSessionEnded(function() unlink(bpc$dir, recursive = TRUE))
    }
    p$set(0.6, "FindAllMarkers...")
    markers <- FindAllMarkers(
      obj_use,
      test.use = input$marker_test,
      min.pct = input$marker_min_pct,
      logfc.threshold = input$marker_logfc,
      only.pos = input$only_pos,
      verbose = input$verbose
    )
    if (is.null(markers) || nrow(markers) == 0L) {
      shared_rv$markers_data <- NULL
      return(list(ok = TRUE, n_results = 0L))
    }
    markers <- as.data.frame(markers)
    rownames(markers) <- NULL
    markers <- .normalize_marker_cols(markers)
    markers <- markers[order(markers$p_val_adj, -abs(markers$avg_log2FC)), ]
    shared_rv$markers_data <- markers
    shared_rv$active_tab <- "tab_table"
    list(ok = TRUE, n_results = as.integer(nrow(markers)))
  }, error = function(e) list(ok = FALSE, n_results = 0L))
}

#' The declared CONVENTION behind each SC module's `n_results`.
#'
#' A FIXED string per module, never derived from the data, so two modules' counts
#' are comparable and an agent can tell what the number means. `bulk_de` already
#' shipped one for its `n_significant` ("padj < 0.05 & |log2FoldChange| > 1"); SC
#' had none, which was the measured gap (2026-09-27) this slice closes.
#'
#' ⚠️ EACH constant lives in the file of the probe that uses it, not here.
#' `.SC_PATHWAYS_DRIVE_CONVENTION` was first declared here and the real pathways
#' state probe then died with "object not found" in three test blocks, because
#' `test-mod-sc-pathways-drive.R` sources `mod_sc_pathways.R` and not this file. A
#' shared block of conventions is the obvious place to put them and the wrong one.
#' @noRd
.SC_MARKERS_DRIVE_CONVENTION   <- "rows of the marker table (one row per gene x cluster)"
.SC_ANNOT_DRIVE_CONVENTION     <- "unique levels of the annotation column named in `column`"
.SC_AP_DRIVE_CONVENTION        <- "number of pipeline artefacts produced (not rows)"
 .sc_markers_drive_view <- function(status, elapsed_s = 0, seq = 0L,                                  n_results = 0L, has_data = FALSE,
                                  ready = FALSE, step = NULL,
                                  descriptor = NULL, convention = NULL) {
  step <- if (is.null(step)) "skipped" else as.character(step)
  if (length(step) != 1L || is.na(step) || !step %in% .SC_AP_DRIVE_STEP_STATES) {
    step <- "error"
  }
  out <- list(
    module = .SC_MARKERS_DRIVE_MODULE,
    action = "run_pipeline",
    status = as.character(status),
    elapsed_s = as.numeric(elapsed_s),
    seq = as.integer(seq),
    n_results = as.integer(n_results),
    has_data = isTRUE(has_data),
    ready = isTRUE(ready),
    steps = list(markers = step)
  )
  # The DESCRIPTOR. Without it `n_results` is a number with no subject: an agent
  # cannot tell WHICH markers, nor interpret the count, nor compare it with another
  # module's. MEASURED gap, 2026-09-27. Bounded by construction (see
  # `ts_drive_table_descriptor()`) and PROJECTED on the wire by
  # `ts_drive_module_states()`, which passes a probe's list WHOLE. Both fields are
  # ABSENT rather than empty when there is no result, so "no descriptor" is never
  # confused with "an empty result".
  if (!is.null(descriptor)) out$descriptor <- descriptor
  if (!is.null(convention)) out$convention <- as.character(convention)[1L]
  out
}

.sc_markers_drive_ready <- function(global_data) {
  if (is.null(tryCatch(shiny::isolate(global_data$sc_obj), error = function(e) NULL))) {
    return("sc_obj is not loaded")
  }
  TRUE
}

.sc_markers_drive_state <- function(global_data, shared_rv, run_state,
                                    last_step = NULL) {
  job <- tryCatch(ts_drive_job_state(), error = function(e) NULL)
  pending <- tryCatch(ts_drive_job_pending(), error = function(e) NULL)
  ready <- isTRUE(.sc_markers_drive_ready(global_data))
  obj <- tryCatch(shiny::isolate(global_data$sc_obj), error = function(e) NULL)
  markers <- tryCatch(shiny::isolate(shared_rv$markers_data), error = function(e) NULL)
  n_results <- if (is.data.frame(markers)) nrow(markers) else 0L
  current <- if (is.function(run_state)) {
    shiny::isolate(run_state())
  } else {
    as.character(run_state)
  }
  if (length(current) != 1L || is.na(current)) current <- "idle"
    .sc_markers_drive_view(
      status = if (ready) current else "not_ready",
      elapsed_s = if (is.null(pending)) 0 else as.numeric(pending$elapsed_s),
      seq = if (is.null(job)) 0L else as.integer(job$seq),
      n_results = n_results,
      has_data = !is.null(obj),
      ready = ready,
      step = last_step,
      # The schema of the table `n_results` counts, so the number has a referent.
      # NULL when there is no table, which the view then OMITS - a probe that never
      # ran must not publish a "0 rows, 0 columns" descriptor that reads like an
      # empty result.
      descriptor = ts_drive_table_descriptor(
        markers, convention = .SC_MARKERS_DRIVE_CONVENTION),
      convention = .SC_MARKERS_DRIVE_CONVENTION
    )
}

.sc_markers_run_drive <- function(global_data, shared_rv, session, close_job) {
  result <- tryCatch(
    run_markers(.sc_markers_drive_inputs(), global_data, shared_rv, session),
    error = function(e) list(ok = FALSE, n_results = 0L)
  )
  ok <- isTRUE(result$ok)
  n_results <- suppressWarnings(as.integer(result$n_results %||% 0L))
  if (length(n_results) != 1L || is.na(n_results) || n_results < 0L) n_results <- 0L
  if (!ok) n_results <- 0L
  status <- if (ok) "done" else "error"
  close_job(status, if (ok) NULL else "the SC marker action reported an error")
  list(status = status, n_results = n_results,
       step = if (ok) "ran" else "error")
}

mod_sc_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 420, title = i18n$t("Single-Cell Workflow"),
      # LOT 1 (V1.x UX): paradigm badge — auto-pipeline exists, manual steps stay optional.
      div(class = "alert alert-success", style = "font-size:0.78rem;padding:5px;margin-bottom:5px;",
          icon("bolt"), " ", i18n$t("Auto-pipeline disponible — les étapes manuelles restent optionnelles")),
      div(class = "alert alert-info", style = "font-size:0.8rem;padding:5px;",
          bsicons::bs_icon("info-circle"), " ", i18n$t("Étapes séquentielles recommandées.")),
      # Mutualized auto-pipeline paradigm (V1.x UX): in EVERY domain the 1-click
      # auto pipeline is the FIRST accordion panel (0.), manual steps follow.
      accordion(
        id = ns("acc_workflow"), open = "grp_prep",
        # ── LOT 2 (V1.x UX): 5 parent sections replace the flat 17-panel list.
        # Section bodies are nested accordions; every historical panel value is
        # preserved (0_mapping, 1_pipeline, ..., 9b_report_consolide).
        # ── 1. Préparation ────────────────────────────────────────────────
        accordion_panel(i18n$t("Préparation"), icon = icon("layer-group"), value = "grp_prep",
          accordion(id = ns("acc_prep"), open = "1_pipeline",
            # Mutualized paradigm (V1.x UX): auto pipeline first, same panel
            # title/position as Bulk and Spatial (ids unchanged).
            accordion_panel(i18n$t("Pipeline auto (1 clic)"), icon = icon("bolt"),
                            value = "0_autopipeline",
                            actionButton(ns("btn_auto_pipeline_sc"), i18n$t("▶ Lancer Pipeline Complet (SC)"),
                                         icon = icon("play-circle"), class = "btn-outline-success w-100 mb-1"),
                            verbatimTextOutput(ns("sc_auto_log")),
                            uiOutput(ns("sc_pipeline_status_bar"))),
            accordion_panel(i18n$t("0. Mapping IDs (Optionnel)"), icon = icon("arrows-rotate"),
                            value = "0_mapping",
                            mod_sc_mapping_ui(ns("mapping"))),
            accordion_panel(i18n$t("1. Pipeline"), icon = icon("cogs"),
                            value = "1_pipeline",
                            mod_sc_pipeline_ui(ns("pipeline"))),
            # MD-4 (décision 5) : gestion du conteneur sc_datasets — enregistrer
            # l'objet SC courant (brut ou traité) sous un label avec relation
            # déclarée (mode 1 = paramètres partagés, mode 2 = distincts).
            accordion_panel(i18n$t("Datasets SC enregistrés (double jeu)"), icon = icon("database"),
                            value = "0_sc_datasets",
                            mod_sc_datasets_ui(ns("sc_datasets")))
          )),
        # ── 2. Analyse ────────────────────────────────────────────────────
        accordion_panel(i18n$t("Analyse"), icon = icon("chart-area"), value = "grp_analyse",
          accordion(id = ns("acc_analyse"),
            accordion_panel(i18n$t("2. Annotation"), icon = icon("user-tag"),
                            value = "2_annotation",
                            mod_sc_annotation_ui(ns("annotation"))),
            # CCC 9 (Q1) : rareté par population annotée — DESCRIPTIF et
            # mono-condition. Onglet du panneau SC existant, jamais un nouveau
            # panneau latéral (point de friction UX n°2).
            accordion_panel(i18n$t("2b. Rareté par population"), icon = icon("magnifying-glass-chart"),
                            value = "2b_rarity",
                            mod_sc_rarity_ui(ns("rarity"))),
            accordion_panel(i18n$t("3. Visualisation"), icon = icon("chart-area"),
                            value = "3_viz",
                            mod_sc_viz_ui(ns("viz"))),
            accordion_panel(i18n$t("4. Marqueurs"), icon = icon("magnifying-glass-chart"),
                            value = "4_markers",
                            mod_sc_markers_ui(ns("markers"))),
            accordion_panel(i18n$t("5. Gene Correlation"), icon = icon("project-diagram"),
                            value = "5_corr",
                            mod_sc_corr_ui(ns("corr"))),
            accordion_panel(i18n$t("6. Pathway Enrichment"), icon = icon("sitemap"),
                            value = "6_pathway",
                            mod_sc_pathways_ui(ns("pathways")))
          )),
        # ── 3. Dynamique ──────────────────────────────────────────────────
        accordion_panel(i18n$t("Dynamique"), icon = icon("route"), value = "grp_dynamique",
          accordion(id = ns("acc_dynamique"),
            accordion_panel(i18n$t("7. Trajectory Analysis"), icon = icon("route"),
                            value = "7_trajectory",
                            mod_sc_trajectory_ui(ns("trajectory"))),
            # LOT 5 (V1.x UX glossary): dual label — what RNA velocity means.
            accordion_panel(i18n$t("Vélocité ARN — orientation des trajectoires"), icon = icon("wind"),
                            value = "8_velocity",
                            mod_sc_velocity_ui(ns("velocity"))),
            accordion_panel(i18n$t("8b. Communication (import)"), icon = icon("satellite-dish"),
                            value = "8b_communication",
                            mod_sc_communication_ui(ns("communication")))
          )),
        # ── 4. Abondance cellulaire ───────────────────────────────────────
        accordion_panel(i18n$t("Abondance cellulaire"), icon = icon("balance-scale"), value = "grp_abondance",
          accordion(id = ns("acc_abondance"),
            # LOT 5 (V1.x UX glossary): dual label — what "pseudobulk" means.
            accordion_panel(i18n$t("Pseudobulk — agrégation des cellules par échantillon"), icon = icon("layer-group"),
                            value = "4b_pseudobulk",
                            mod_sc_pseudobulk_ui(ns("pseudobulk"))),
            # LOT 2 (mandatory DA nesting): the four flat panels 8c/8d/8e/8f
            # become ONE accordion panel whose body is an inner navset_pill.
            # A = design gatekeeper (8c); B = methods, Milo & scCODA side by
            # side (8d/8e) — existing UIs nested, NO backend merge; C = cross
            # views (8f). Gating of C stays where it is today: mod_sc_da_cross
            # already req()s both results internally (no new gate invented).
            accordion_panel(i18n$t("Abondance différentielle — design, méthodes & vues croisées"),
                            icon = icon("balance-scale"), value = "8_da_merged",
                            navset_pill(
                              id = ns("da_tabs"),
                              nav_panel(i18n$t("A. Plan expérimental (design)"), value = "da_design",
                                        mod_sc_da_design_ui(ns("da_design"))),
                              nav_panel(i18n$t("B. Méthodes (Milo / scCODA)"), value = "da_methods",
                                        navset_pill(
                                          id = ns("da_methods_tabs"),
                                          # LOT 5 (V1.x UX): tooltips demystify the method names.
                                          nav_panel(tagList("Milo ", bslib::tooltip(icon("circle-info"), i18n$t("Milo — abondance par voisinages de cellules"))),
                                                    value = "da_milo",
                                                    mod_sc_da_milo_ui(ns("da_milo"))),
                                          nav_panel(tagList("scCODA ", bslib::tooltip(icon("circle-info"), i18n$t("scCODA — composition cellulaire (modèle bayésien)"))),
                                                    value = "da_sccoda",
                                                    mod_sc_da_sccoda_ui(ns("da_sccoda")))
                                        )),
                              nav_panel(i18n$t("C. Vues croisées"), value = "da_cross",
                                        mod_sc_da_cross_ui(ns("da_cross")))
                            ))
          )),
        # ── 5. Livrables (LOT 3A: reports are the workflow's outcome) ─────
        accordion_panel(i18n$t("Livrables"), icon = icon("file-export"), value = "grp_livrables",
          accordion(id = ns("acc_livrables"),
            accordion_panel(
              i18n$t("9. Rapport Complet"), icon = icon("file-export"),
              value = "9_report",
              div(class = "alert alert-light", style = "font-size:0.85em;border-left:3px solid #2C3E50;",
                  i18n$t("Rapport autonome (QC, Réduction, Annotation, Marqueurs, Pathways, Trajectoire).")),
              textInput(ns("report_title"), i18n$t("Titre"), value = "Analyse Single-Cell"),
              textInput(ns("report_subtitle"), i18n$t("Sous-titre (optionnel)")),
              textAreaInput(ns("report_notes"), i18n$t("Notes"), rows = 3),
              checkboxGroupInput(ns("report_sections"), i18n$t("Sections"),
                choices = setNames(
                  c("qc", "dim", "annotation", "rarity", "markers", "correlation", "pathway", "trajectory", "velocity", "communication", "da", "custom_viz"),
                  c(.tr_plain("QC"), .tr_plain("Réduction Dimensionnelle"), .tr_plain("Annotation"),
                    .tr_plain("Rareté par population"),
                    .tr_plain("Marqueurs"), .tr_plain("Réseau Corrélation"), .tr_plain("Pathway Enrichment"),
                    .tr_plain("Trajectoire"), .tr_plain("Vitesse ARN"), .tr_plain("Communication cellulaire"),
                    .tr_plain("Abondance différentielle"), .tr_plain("Visualisations Sauvegardées"))),
                selected = c("qc", "dim", "annotation", "markers", "pathway")),
              div(class = "border rounded p-2 mb-2", style = "background:#f8f9fa;",
                  h6(i18n$t("📌 Visualisations sauvegardées"), style = "font-size:0.85em;font-weight:bold;"),
                  uiOutput(ns("saved_viz_list_ui")),
                  actionButton(ns("clear_saved_viz"), i18n$t("🗑️ Vider la liste"),
                               class = "btn-outline-danger btn-sm w-100 mt-1")),
              radioButtons(ns("report_format"), i18n$t("Format"),
                choices = setNames(c("html", "pdf", "both"),
                                   c(.tr_plain("HTML interactif"), .tr_plain("PDF statique"), .tr_plain("Les deux (.zip)"))),
                selected = "html"),
              conditionalPanel(condition = "input.report_format != 'pdf'", ns = ns,
                checkboxInput(ns("report_interactive"), i18n$t("Graphiques interactifs (HTML)"), value = TRUE)),
              div(class = "small text-muted", i18n$t("PDF requiert tinytex::install_tinytex().")),
              downloadButton(ns("dl_report"), i18n$t("📄 Générer le Rapport"), class = "btn-dark w-100 mt-2"),
              hr(),
              div(class = "alert alert-light", style = "font-size:0.82em;border-left:3px solid #18BC9C;",
                  bsicons::bs_icon("code-slash"),
                  i18n$t(" Script R reproductible (.zip) + objet Seurat traité.")),
              downloadButton(ns("dl_sc_r_script"), i18n$t("🧾 Export Script R (.zip)"),
                             class = "btn-outline-secondary w-100"),
              div(class = "small text-muted mt-1", textOutput(ns("report_status")))
            ),
            # Stage 17 (4F) : rapport consolide — compile l'etat canonique + la
            # provenance (aucune re-execution d'analyse ; le panneau 9 historique
            # reste le rapport Rmd par domaine, inchangé).
            accordion_panel(i18n$t("9b. Rapport consolidé (4F)"), icon = icon("clipboard-check"),
                            value = "9b_report_consolide",
                            mod_sc_report_consolidated_ui(ns("report_consolide")))
          ))
      )
    ),
    navset_card_underline(
      id = ns("main_tabs"), title = i18n$t("Résultats"),
      nav_panel(i18n$t("Graphiques"), value = "tab_viz", mod_sc_viz_output_ui(ns("viz"))),
      # MAJ 2
      nav_panel(i18n$t("Table Marqueurs"), value = "tab_table", mod_sc_markers_output_ui(ns("markers"))),
      nav_panel(i18n$t("Pseudobulk DE"), value = "tab_pseudobulk", mod_sc_pseudobulk_output_ui(ns("pseudobulk"))),
      nav_panel(i18n$t("Annotation"), value = "tab_annotation", mod_sc_annotation_output_ui(ns("annotation"))),
      nav_panel(i18n$t("Rareté par population"), value = "tab_rarity", mod_sc_rarity_output_ui(ns("rarity"))),
      nav_panel(i18n$t("Gènes Corrélés"), value = "tab_correlation", mod_sc_corr_output_ui(ns("corr"))),
      nav_panel(i18n$t("Pathways"), value = "tab_pathway", mod_sc_pathways_output_ui(ns("pathways"))),
      nav_panel(i18n$t("Trajectory"), value = "tab_trajectory", mod_sc_trajectory_output_ui(ns("trajectory"))),
      nav_panel(i18n$t("Velocity"), value = "tab_velocity", mod_sc_velocity_output_ui(ns("velocity"))),
      nav_panel(i18n$t("Communication"), value = "tab_communication", mod_sc_communication_output_ui(ns("communication"))),
      nav_panel(i18n$t("DA design"), value = "tab_da_design", mod_sc_da_design_output_ui(ns("da_design"))),
      nav_panel(i18n$t("Milo DA"), value = "tab_da_milo", mod_sc_da_milo_output_ui(ns("da_milo"))),
      nav_panel(i18n$t("scCODA DA"), value = "tab_da_sccoda", mod_sc_da_sccoda_output_ui(ns("da_sccoda"))),
      nav_panel(i18n$t("DA croisées"), value = "tab_da_cross", mod_sc_da_cross_output_ui(ns("da_cross"))),
      nav_panel(i18n$t("Rapport consolidé"), value = "tab_report_consolide", mod_sc_report_consolidated_output_ui(ns("report_consolide"))),
      nav_panel(i18n$t("QC"), value = "tab_qc",
        card(max_height = 750,
          div(class = "card-header bg-light", h5(i18n$t("Contrôle Qualité"), class = "card-title mb-0")),
          plotOutput(ns("plot_qc"), height = "650px"),
          # V1.x UX multi-échantillons : sous-panneau léger, visible seulement
          # quand >= 2 échantillons (orig.ident) — barplots méta.data +
          # projections sur réductions EXISTANTES (aucun calcul nouveau).
          uiOutput(ns("multisample_overview_ui")))),
      nav_panel(i18n$t("Résumé Pipeline"), value = "tab_summary",
        card(card_header(i18n$t("Résumé du Pipeline Single-Cell")),
             uiOutput(ns("pipeline_summary_panel"))))
    )
  )
}


mod_sc_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {
    # OBLIGATOIRE : `ns` n'est lie que dans la fonction UI (NS(id)), pas dans le
    # serveur. Sans cette ligne, un renderUI qui appelle ns() leve « impossible
    # de trouver la fonction "ns" » — et seulement quand la branche s'affiche,
    # donc invisible au demarrage. Garde : test-release-hardening.R.
    ns <- session$ns

    .tr <- function(key) { tr <- isolate(global_data$i18n); if (is.null(tr)) return(key); tryCatch(.strip_i18n_html(tr$t(key)), error=function(e) key) }

    shared_rv <- create_sc_shared_state()

    # ── i18n: update report section choices on language switch ──────────────
    observeEvent(global_data$language, {
      updateCheckboxGroupInput(session, "report_sections",
        label = .tr("Sections"),
        choices = stats::setNames(
          c("qc", "dim", "annotation", "rarity", "markers", "correlation", "pathway", "trajectory", "velocity", "communication", "da", "custom_viz"),
          c(.tr("QC"), .tr("Réduction Dimensionnelle"), .tr("Annotation"),
            .tr("Rareté par population"),
            .tr("Marqueurs"), .tr("Réseau Corrélation"), .tr("Pathway Enrichment"),
            .tr("Trajectoire"), .tr("Vitesse ARN"), .tr("Communication cellulaire"),
            .tr("Abondance différentielle"), .tr("Visualisations Sauvegardées"))))

      updateRadioButtons(session, "report_format",
        label = .tr("Format"),
        choices = stats::setNames(c("html", "pdf", "both"),
          c(.tr("HTML interactif"), .tr("PDF statique"), .tr("Les deux (.zip)"))))

      updateTextInput(session, "report_title", label = .tr("Titre"))
      updateTextInput(session, "report_subtitle", label = .tr("Sous-titre (optionnel)"))
      updateTextAreaInput(session, "report_notes", label = .tr("Notes"))
      updateCheckboxInput(session, "report_interactive",
        label = .tr("Graphiques interactifs (HTML)"))
      updateActionButton(session, "dl_report",
        label = paste("📄", .tr("Générer le Rapport")))
    }, ignoreInit = TRUE)

    observeEvent(state_get(shared_rv, "active_tab"), {
      req(state_get(shared_rv, "active_tab"))
      nav_select(id="main_tabs", selected=state_get(shared_rv, "active_tab"), session=session)
    })

    # ── QC plot ──────────────────────────────────────────────────────────────
    output$plot_qc <- renderPlot({
      req(global_data$sc_obj)
      VlnPlot(global_data$sc_obj,
              features=c("nFeature_RNA","nCount_RNA","percent.mt"), ncol=3, pt.size=0)
    })

    # ── V1.x UX multi-échantillons : vue d'ensemble (≥ 2 orig.ident) ─────────
    # Lectures pures de l'état existant : barplots depuis méta.data, projets
    # 2D depuis une réduction DÉJÀ calculée (aucun recalcul, aucune
    # densification). Visible uniquement quand >= 2 échantillons.
    .ms_pick_reduction <- function(obj) {
      for (r in c("umap", "umap_harmony", "tsne", "pca"))
        if (r %in% names(obj@reductions)) return(r)
      NULL
    }
    .ms_dim_plot <- function(obj, group_by, red, title) {
      tryCatch(
        Seurat::DimPlot(obj, reduction = red, group.by = group_by,
                        raster = ncol(obj) > 50000) +
          ggplot2::ggtitle(title),
        error = function(e) NULL)
    }
    output$multisample_overview_ui <- renderUI({
      global_data$language
      obj <- global_data$sc_obj
      if (is.null(obj) || is.null(obj$orig.ident)) return(NULL)
      if (length(unique(obj$orig.ident)) < 2) return(NULL)
      has_cond <- "condition" %in% colnames(obj@meta.data)
      red      <- .ms_pick_reduction(obj)
      tagList(
        hr(),
        h5(.tr("Vue d'ensemble multi-échantillons"), class = "mb-1"),
        div(class = "alert alert-light", style = "font-size:0.82em;padding:6px;",
            bsicons::bs_icon("lightbulb"), " ",
            .tr("Séparation forte par échantillon ? Activez Harmony dans le Pipeline.")),
        plotOutput(ns("ms_plot_cells_sample"), height = "280px"),
        if (has_cond) plotOutput(ns("ms_plot_cells_condition"), height = "280px"),
        if (!is.null(red)) {
          tagList(
            plotOutput(ns("ms_plot_dim_sample"), height = "380px"),
            if (has_cond) plotOutput(ns("ms_plot_dim_condition"), height = "380px"))
        } else {
          div(class = "alert alert-warning", style = "font-size:0.82em;padding:6px;",
              .tr("Aucune réduction 2D disponible : lancez le Pipeline pour afficher les projections par échantillon."))
        }
      )
    })
    output$ms_plot_cells_sample <- renderPlot({
      global_data$language
      obj <- global_data$sc_obj
      req(obj, obj$orig.ident)
      op <- par(mar = c(9, 4, 2, 1)); on.exit(par(op), add = TRUE)
      barplot(table(obj$orig.ident), las = 2, col = "#2C3E50",
              main = .tr("Cellules par échantillon"), ylab = "n")
    })
    output$ms_plot_cells_condition <- renderPlot({
      global_data$language
      obj <- global_data$sc_obj
      req(obj)
      req("condition" %in% colnames(obj@meta.data))
      op <- par(mar = c(9, 4, 2, 1)); on.exit(par(op), add = TRUE)
      barplot(table(factor(obj@meta.data$condition)), las = 2, col = "#18BC9C",
              main = .tr("Cellules par condition"), ylab = "n")
    })
    output$ms_plot_dim_sample <- renderPlot({
      global_data$language
      obj <- global_data$sc_obj
      req(obj)
      red <- .ms_pick_reduction(obj)
      req(red)
      .ms_dim_plot(obj, "orig.ident", red, .tr("Projection par échantillon"))
    })
    output$ms_plot_dim_condition <- renderPlot({
      global_data$language
      obj <- global_data$sc_obj
      req(obj)
      req("condition" %in% colnames(obj@meta.data))
      red <- .ms_pick_reduction(obj)
      req(red)
      .ms_dim_plot(obj, "condition", red, .tr("Projection par condition"))
    })

    # ── Pipeline status bar ───────────────────────────────────────────────────
    output$sc_pipeline_status_bar <- renderUI({
      obj <- global_data$sc_obj
      if (is.null(obj)) return(NULL)
      meta <- obj@meta.data
      s_qc      <- if ("percent.mt"      %in% colnames(meta))                     "\u2705" else "\u26aa"
      s_norm    <- if (length(tryCatch(VariableFeatures(obj),error=function(e) character(0))) > 0) "\u2705" else "\u26aa"
      s_cluster <- if ("seurat_clusters" %in% colnames(meta))                     "\u2705" else "\u26aa"
      s_umap    <- if ("umap"            %in% names(obj@reductions))              "\u2705" else "\u26aa"
      s_annot   <- if (any(grepl("^SingleR_", colnames(meta))))                   "\u2705" else "\u26aa"
      s_markers <- if (!is.null(state_get(shared_rv, "markers_data")))                          "\u2705" else "\u26aa"
      div(style=paste0("display:flex;justify-content:space-around;font-size:0.72em;",
                       "background:#f8f9fa;border:1px solid #e3e6e8;border-radius:6px;",
                       "padding:4px 2px;margin-bottom:8px;"),
          tags$span(style="padding:2px 4px;", s_qc,      .tr(" QC")),
          tags$span(style="padding:2px 4px;", s_norm,    .tr(" Norm")),
          tags$span(style="padding:2px 4px;", s_cluster, .tr(" Cluster")),
          tags$span(style="padding:2px 4px;", s_umap,    .tr(" UMAP")),
          tags$span(style="padding:2px 4px;", s_annot,   .tr(" Annot")),
          tags$span(style="padding:2px 4px;", s_markers, .tr(" Marqueurs")))
    })

    # ── Pipeline summary panel ────────────────────────────────────────────────
    output$pipeline_summary_panel <- renderUI({
      global_data$language
      obj <- global_data$sc_obj
      if (is.null(obj))
        return(div(class="alert alert-info m-3", .tr("Aucun objet Single-Cell chargé.")))
      meta         <- obj@meta.data
      singler_cols <- grep("^SingleR_", colnames(meta), value=TRUE)
      reductions   <- names(obj@reductions)
      n_clusters   <- if ("seurat_clusters" %in% colnames(meta))
                        length(levels(factor(meta$seurat_clusters))) else NA
      rows <- list(
        c(.tr("Cellules"),              format(ncol(obj), big.mark=",")),
        c(.tr("Gènes"),                 format(nrow(obj), big.mark=",")),
        c(.tr("Réductions"),            if (length(reductions)) paste(reductions,collapse=", ") else "\u2014"),
        c(.tr("Clusters"),              if (!is.na(n_clusters)) as.character(n_clusters) else .tr("Non calculé")),
        c(.tr("Annotation SingleR"),    if (length(singler_cols)) paste(singler_cols,collapse=", ") else .tr("Non effectuée")),
        c(.tr("Gènes variables"),       if (length(tryCatch(VariableFeatures(obj),error=function(e) character(0))))
                                     format(length(VariableFeatures(obj)),big.mark=",") else "\u2014"),
        c(.tr("Marqueurs calculés"),    if (!is.null(state_get(shared_rv, "markers_data")))
                                     paste(nrow(state_get(shared_rv, "markers_data")),"marqueurs") else .tr("Non calculés")),
        c(.tr("Pathways"),              if (!is.null(state_get(shared_rv, "pathway_results")))
                                     paste(nrow(state_get(shared_rv, "pathway_results")),"pathways") else .tr("Non calculés")),
        c(.tr("Pseudotemps"),           if ("pseudotime" %in% colnames(meta)) .tr("Calculé") else .tr("Non calculé")),
        c(.tr("Backend stockage"),      if (sc_backend_status(obj) == "disk") "\U0001f4bd Disque (BPCells)" else "\U0001f9e0 RAM (standard)"),
        c(.tr("Sous-échant. (marqueurs/corr)"), if (is.finite(state_get(shared_rv, "max_cells_heavy") %||% Inf))
                                     paste0("max ", format(state_get(shared_rv, "max_cells_heavy"), big.mark=","), " ", .tr_plain("cellules/groupe"))
                                   else .tr("désactivé")),
        c(.tr("Viz. sauvegardées"),     paste0(length(state_get(shared_rv, "report_viz_list")), " ", .tr_plain("plot(s) dans le panier")))
      )
      tagList(
        div(class="m-3",
          tags$table(class="table table-sm table-bordered",
            tags$tbody(lapply(rows, function(r) {
              tags$tr(tags$th(style="width:40%;",r[1]), tags$td(r[2]))
            }))),
          div(class="small text-muted", paste(.tr("Mis à jour :"), format(Sys.time(),"%H:%M:%S")))
        )
      )
    })

    # ── Saved viz basket UI ───────────────────────────────────────────────────
    output$saved_viz_list_ui <- renderUI({
      lst <- state_get(shared_rv, "report_viz_list") %||% list()
      if (!length(lst))
        return(div(class="text-muted small", paste0(.tr("Aucune visualisation sauvegardée. "), .tr("Utilisez '📌 Ajouter au Rapport' dans l'onglet Graphiques."))))
      tags$ul(style="font-size:0.8em;margin-bottom:0;",
              lapply(names(lst), function(nm) tags$li(nm)))
    })

    observeEvent(input$clear_saved_viz, {
      state_set(shared_rv, "report_viz_list", list())
      showNotification(.tr("🗑️ Liste de visualisations vidée."), type="message", duration=3)
    })

    # ── Child servers ─────────────────────────────────────────────────────────
    mod_sc_mapping_server(   "mapping",   global_data)
    mod_sc_pipeline_server(  "pipeline",  global_data, shared_rv)
    mod_sc_datasets_server(  "sc_datasets", global_data)  # MD-4 : conteneur sc_datasets (lecture seule de sc_obj)
    mod_sc_annotation_server("annotation",global_data, shared_rv)
    mod_sc_rarity_server("rarity", global_data, shared_rv)
    mod_sc_viz_server(       "viz",       global_data, shared_rv)
    # maj 3
    mod_sc_markers_server(   "markers",   global_data, shared_rv)
    mod_sc_pseudobulk_server("pseudobulk",global_data, shared_rv)
    mod_sc_corr_server(      "corr",      global_data, shared_rv)
    mod_sc_pathways_server(  "pathways",  global_data, shared_rv)
    mod_sc_trajectory_server("trajectory",global_data, shared_rv)
    mod_sc_velocity_server("velocity", global_data, shared_rv)
    mod_sc_communication_server("communication", global_data, shared_rv)
    mod_sc_da_design_server("da_design", global_data, shared_rv)
    mod_sc_da_milo_server("da_milo", global_data, shared_rv)
    mod_sc_da_sccoda_server("da_sccoda", global_data, shared_rv)
    mod_sc_da_cross_server("da_cross", global_data, shared_rv)
    mod_sc_report_consolidated_server("report_consolide", global_data, shared_rv)  # Stage 17 (4F)

    # ── traj_reduction / traj_genes mirrors (written by mod_sc_trajectory_server)

    # =========================================================================
    # AUTO-PIPELINE MODAL
    # =========================================================================
    sc_log_rv <- reactiveVal("")
    output$sc_auto_log <- renderText({ sc_log_rv() })

    # ── Step-3.8A: sketch preset hint + PCA-dims sync ──────────────────────
    output$sc_ap_sketch_hint <- renderUI({
      req(global_data$sc_obj, input$sc_ap_sketch_preset)
      n_total <- ncol(global_data$sc_obj)
      params  <- resolve_sketch_preset(input$sc_ap_sketch_preset, n_total,
                                        input$sc_ap_sketch_ncells_custom)
      will_sketch <- params$ncells < n_total
      div(class="small", style=paste0("color:", if (will_sketch) "#18BC9C" else "#666", ";"),
          .t_fmt(.tr("{n} / {total} cellules — npcs suggéré : {npcs} ({sketch})"),
                 n     = format(params$ncells, big.mark = " "),
                 total = format(n_total, big.mark = " "),
                 npcs  = params$npcs,
                 sketch = if (will_sketch) .tr("sketch actif") else .tr("pas de sketch")))
    })

    observeEvent(input$sc_ap_sketch_preset, {
      req(global_data$sc_obj)
      params <- resolve_sketch_preset(input$sc_ap_sketch_preset, ncol(global_data$sc_obj),
                                       input$sc_ap_sketch_ncells_custom)
      updateSliderInput(session, "sc_ap_pca_dim", value = params$npcs)
    }, ignoreInit = TRUE)

    observeEvent(input$btn_auto_pipeline_sc, {
      req(global_data$sc_obj)
      ns_m <- session$ns
      detected_map_org <- tryCatch(detect_organism_from_ids(rownames(global_data$sc_obj)),
                                   error = function(e) NA_character_)
      mapping_org_selected <- if (!is.na(detected_map_org)) detected_map_org else "human"
      showModal(modalDialog(
        title=paste("\u25b6", .tr("Pipeline SC — Paramètres")), size="m", easyClose=TRUE,

        # ── Step 0: Mapping ─────────────────────────────────────────────────
        checkboxInput(ns_m("sc_ap_mapping"),
                      paste("\U0001f504", .tr("Mapping IDs → Symbol (auto-détecté, avant QC)")), value=TRUE),
        conditionalPanel(
          condition=sprintf("input['%s'] == true", ns_m("sc_ap_mapping")),
          selectInput(ns_m("sc_ap_mapping_org"), .tr("Organisme (mapping)"),
                      stats::setNames(c("human","mouse"), c(.tr("Humain"), .tr("Souris"))),
                      selected = mapping_org_selected)),
        checkboxInput(ns_m("sc_ap_bpcells"),
                      .t_fmt(.tr("💽 Backend disque (BPCells) si > {n} cellules"),
                             n = format(.BPCELLS_AUTO_THRESHOLD, big.mark = " ")),
                      value = TRUE),
        hr(),

        # ── Step 1: QC ──────────────────────────────────────────────────────
        fluidRow(
          column(6,
            h6(.tr("QC"), style="font-weight:bold;"),
            numericInput(ns_m("sc_ap_min_gene"), .tr("Min gènes/cellule"),   100, min=0),
            numericInput(ns_m("sc_ap_max_gene"), .tr("Max gènes/cellule"), 8000, min=0),
            sliderInput(ns_m("sc_ap_mt"), .tr("% Mito max"), 0, 50, 20, step=1)
          ),
          column(6,
            h6(.tr("Normalisation & Réduction"), style="font-weight:bold;"),
            radioButtons(ns_m("sc_ap_norm"), .tr("Normalisation"),
                         stats::setNames(c("log","sct"),
                                         c(.tr("LogNormalize"), .tr("SCTransform")))),
            sliderInput(ns_m("sc_ap_pca_dim"), .tr("Dims PCA"), 5, 50, 20),
            numericInput(ns_m("sc_ap_res"), .tr("Résolution clustering"), 0.5, min=0.1, step=0.1),
            selectInput(ns_m("sc_ap_cluster_algo"), .tr("Algorithme de clustering"),
                       choices = stats::setNames(c("1","2","3","4"),
                                                 c(.tr("Louvain (standard)"),
                                                   .tr("Louvain (multilevel refinement)"),
                                                   .tr("SLM (Smart Local Moving)"),
                                                   .tr("Leiden (nécessite reticulate + leidenalg)"))),
                       selected="1")
          )
        ),
        checkboxInput(ns_m("sc_ap_compute_umap"),
                     paste("\u2713", .tr("Calculer UMAP (décochez pour PCA seul — bien plus rapide, mode debug)")),
                     value = TRUE),
        div(class="small text-muted mb-2",
            .tr("Si coché : UMAP + t-SNE secondaire (si dataset raisonnable) sont calculés (le plus lent du pipeline). Si décoché : PCA seul — previews/trajectoire se rabattent automatiquement sur PCA, rien ne plante.")),
        hr(),

        # ── Sketch ─────────────────────────────────────────────────────────
        h6(.tr("Sketch — gros datasets"), style="font-weight:bold;"),
        div(class="small text-muted mb-1",
            .tr("PCA/Clustering/UMAP tournent sur un sous-ensemble représentatif (LeverageScore), puis sont projetés sur toutes les cellules. Accélère fortement les gros datasets (ex: 1,3M cellules) sans perdre les clusters rares. Ignoré si SCTransform est choisi.")),
        fluidRow(
          column(7, selectInput(ns_m("sc_ap_sketch_preset"), .tr("Preset sketch"),
            choices = stats::setNames(
              c("fast","light","medium","standard","high","max","custom"),
              c(.tr("Rapide (test, 5 000 cellules)"),
                .tr("Léger (10 000 cellules)"),
                .tr("Moyen (25 000 cellules)"),
                .tr("Standard (50 000 cellules)"),
                .tr("Élevé (100 000 cellules)"),
                .tr("Max (dataset complet)"),
                .tr("Personnalisé"))),
            selected = "standard")),
          column(5, conditionalPanel(
            condition = sprintf("input['%s'] == 'custom'", ns_m("sc_ap_sketch_preset")),
            numericInput(ns_m("sc_ap_sketch_ncells_custom"), .tr("N cellules"),
                         value = 20000, min = 1000, max = 500000, step = 1000)))
        ),
        uiOutput(ns_m("sc_ap_sketch_hint")),
        hr(),

        # ── Steps 2-7 optional ──────────────────────────────────────────────
        h6(.tr("Options supplémentaires"), style="font-weight:bold;"),
        checkboxInput(ns_m("sc_ap_singler"), paste("\U0001f9ec", .tr("Annotation SingleR")), value=FALSE),
        conditionalPanel(
          condition=sprintf("input['%s'] == true", ns_m("sc_ap_singler")),
          fluidRow(
            column(6, selectInput(ns_m("sc_ap_singler_ref"), .tr("Référence"),
                       stats::setNames(c("hpca","blueprint","immgen","dice"),
                                       c(.tr("Human Primary Cell Atlas"), .tr("Blueprint Encode"),
                                         .tr("ImmGen (Mouse)"), .tr("DICE Immune"))))),
            column(6, radioButtons(ns_m("sc_ap_singler_level"), .tr("Niveau"),
                       stats::setNames(c("main","fine"),
                                       c(.tr("Main (General)"), .tr("Fine (Specifique)"))), inline=TRUE))
          )
        ),
        checkboxInput(ns_m("sc_ap_markers"),
                      paste("\U0001f9ec", .tr("FindAllMarkers après clustering")), value=FALSE),
        checkboxInput(ns_m("sc_ap_pathway"),
                      paste("\U0001f9ec", .tr("Pathway ORA sur top marqueurs")), value=FALSE),
        conditionalPanel(
          condition=sprintf("input['%s'] == true", ns_m("sc_ap_pathway")),
          fluidRow(
            column(6, selectInput(ns_m("sc_ap_pathway_db"), .tr("Base"),
                       stats::setNames(c("GOBP","KEGG","Reactome"),
                                       c(.tr("GO BP"), .tr("KEGG"), .tr("Reactome"))))),
            column(6, selectInput(ns_m("sc_ap_pathway_org"), .tr("Organisme"),
                       stats::setNames(c("human","mouse"),
                                       c(.tr("Humain"), .tr("Souris")))))
          )
        ),
        checkboxInput(ns_m("sc_ap_correlation"),
                      paste("\U0001f9ec", .tr("Gene Correlation (auto: gène le plus significatif)")), value=FALSE),
        helpText(style="font-size:0.78em;color:#666;",
                 .tr("Requiert 'Marqueurs' coché. Corrèle le gène à p-adj minimal avec tous les autres.")),
        checkboxInput(ns_m("sc_ap_trajectory"),
                      paste("\U0001f9ec", .tr("Trajectory / Pseudotemps (UMAP, racine auto)")), value=FALSE),

        footer=tagList(
          modalButton(.tr("Annuler")),
          actionButton(ns_m("sc_ap_confirm"), paste("\u25b6", .tr("Lancer")), class="btn-success"))
      ))
    })

    # =========================================================================
    # AUTO-PIPELINE SERVER
    # =========================================================================
    observeEvent(input$sc_ap_confirm, {
      removeModal()
      req(global_data$sc_obj)
      run_sc_auto_pipeline(input, global_data, shared_rv, session, sc_log_rv)
    })

    # =========================================================================
    # DRIVE LIVE CONTROL — the SC auto-pipeline, one frozen action
    # =========================================================================
    # Thin by construction: the observer below only wires the protocol to the
    # file-level helpers, and every decision (readiness, parameters, per-step
    # outcomes, terminal status) lives in those helpers, where it is testable
    # without a session. `ignoreInit = TRUE` is load-bearing — a bare
    # observeEvent() on a reactiveVal fires once at module init, which would
    # launch the whole pipeline on every page load.
    drive_counter  <- shiny::reactiveVal(0L)
    drive_run_state <- shiny::reactiveVal("idle")
    drive_last_steps <- new.env(parent = emptyenv())
    drive_last_steps$value <- NULL
    drive_job <- new.env(parent = emptyenv())
    drive_job$id <- NULL

    .sc_ap_drive_ready <- function() {
      if (is.null(tryCatch(shiny::isolate(global_data$sc_obj), error = function(e) NULL))) {
        return("sc_obj is not loaded")
      }
      TRUE
    }

    .sc_ap_drive_state <- function() {
      job <- tryCatch(ts_drive_job_state(), error = function(e) NULL)
      pending <- tryCatch(ts_drive_job_pending(), error = function(e) NULL)
      ready <- isTRUE(.sc_ap_drive_ready())
      obj <- tryCatch(shiny::isolate(global_data$sc_obj), error = function(e) NULL)
      .sc_ap_drive_view(
        status = if (ready) drive_run_state() else "not_ready",
        elapsed_s = if (is.null(pending)) 0 else as.numeric(pending$elapsed_s),
        seq = if (is.null(job)) 0L else as.integer(job$seq),
        n_results = .sc_ap_count_results(obj, .sc_ap_produced(shared_rv)),
        has_data = !is.null(obj),
        ready = ready,
        steps = drive_last_steps$value,
        # The auto-pipeline produces SEVERAL artefacts, so its descriptor names
        # them rather than describing a frame: the referent for its count is the
        # set of steps that produced something.
        descriptor = ts_drive_table_descriptor(
          data.frame(step = names(.sc_ap_produced(shared_rv)),
                     stringsAsFactors = FALSE),
          convention = .SC_AP_DRIVE_CONVENTION),
        convention = .SC_AP_DRIVE_CONVENTION
      )
    }

    # The id is written as a LITERAL, not as `.SC_AP_DRIVE_BUTTON`: the wiring
    # guard in test-drive-watcher.R greps the module sources for
    # `ts_drive_publish_token(.*"<id>"` to prove a token is actually published —
    # a constant would make the binding a promise again, which is precisely what
    # that guard exists to prevent. The literal is pinned against the allowlist
    # constant in test-mod-sc-drive.R, so the duplication cannot drift.
    ts_drive_publish_token(global_data, "sc-pipeline-run_auto_pipeline", drive_counter,
      ready = .sc_ap_drive_ready, state = .sc_ap_drive_state, long = TRUE,
      timeout_s = TS_SC_AUTO_PIPELINE_TIMEOUT_S
    )

    .close_sc_ap_drive_job <- function(status, error = NULL) {
      if (is.null(drive_job$id)) return(invisible(FALSE))
      job <- tryCatch(ts_drive_job_state(), error = function(e) NULL)
      if (is.null(job) || !identical(job$job_id, drive_job$id)) {
        return(invisible(FALSE))
      }
      ts_drive_job_finish(.SC_AP_DRIVE_BUTTON, status = status, error = error,
                          job_id = drive_job$id)
    }

    observeEvent(drive_counter(), {
      # Fail closed: an unready module never starts a job it cannot finish.
      if (!isTRUE(.sc_ap_drive_ready())) return()
      drive_job$id <- NULL
      job <- tryCatch(ts_drive_job_state(), error = function(e) NULL)
      if (!is.null(job) && isTRUE(ts_drive_job_busy()) &&
          identical(job$button, .SC_AP_DRIVE_BUTTON)) {
        drive_job$id <- job$job_id
      }
      drive_run_state("running")
      res <- .sc_ap_run_drive(global_data, shared_rv, session, sc_log_rv,
                              .close_sc_ap_drive_job)
      drive_last_steps$value <- res$steps
      drive_run_state(res$status)
    }, ignoreInit = TRUE)

    annot_drive_counter <- shiny::reactiveVal(0L)
    annot_drive_run_state <- shiny::reactiveVal("idle")
    annot_drive_last_step <- new.env(parent = emptyenv())
    annot_drive_last_step$value <- NULL
    annot_drive_job <- new.env(parent = emptyenv())
    annot_drive_job$id <- NULL

    annot_drive_ready <- function() {
      .sc_annot_drive_ready(global_data)
    }

    annot_drive_state <- function() {
      .sc_annot_drive_state(global_data, annot_drive_run_state,
                             annot_drive_last_step$value)
    }

    ts_drive_publish_token(global_data, "sc-annotation-run_annot",
      annot_drive_counter, ready = annot_drive_ready,
      state = annot_drive_state, long = TRUE,
      timeout_s = TS_SC_ANNOTATION_TIMEOUT_S
    )

    close_annot_drive_job <- function(status, error = NULL) {
      if (is.null(annot_drive_job$id)) return(invisible(FALSE))
      job <- tryCatch(ts_drive_job_state(), error = function(e) NULL)
      if (is.null(job) || !identical(job$job_id, annot_drive_job$id)) {
        return(invisible(FALSE))
      }
      ts_drive_job_finish(.SC_ANNOT_DRIVE_BUTTON, status = status, error = error,
                          job_id = annot_drive_job$id)
    }

    observeEvent(annot_drive_counter(), {
      if (!isTRUE(annot_drive_ready())) return()
      annot_drive_job$id <- NULL
      job <- tryCatch(ts_drive_job_state(), error = function(e) NULL)
      if (!is.null(job) && isTRUE(ts_drive_job_busy()) &&
          identical(job$button, .SC_ANNOT_DRIVE_BUTTON)) {
        annot_drive_job$id <- job$job_id
      }
      annot_drive_run_state("running")
      res <- .sc_annot_run_drive(global_data, shared_rv, session,
                                 close_annot_drive_job)
      annot_drive_last_step$value <- res$step
      annot_drive_run_state(res$status)
    }, ignoreInit = TRUE)

    markers_drive_counter <- shiny::reactiveVal(0L)
    markers_drive_run_state <- shiny::reactiveVal("idle")
    markers_drive_last_step <- new.env(parent = emptyenv())
    markers_drive_last_step$value <- NULL
    markers_drive_job <- new.env(parent = emptyenv())
    markers_drive_job$id <- NULL

    markers_drive_ready <- function() {
      .sc_markers_drive_ready(global_data)
    }

    markers_drive_state <- function() {
      .sc_markers_drive_state(global_data, shared_rv,
                              markers_drive_run_state,
                              markers_drive_last_step$value)
    }

    ts_drive_publish_token(global_data, "sc-markers-run_markers",
      markers_drive_counter, ready = markers_drive_ready,
      state = markers_drive_state, long = TRUE,
      timeout_s = TS_SC_MARKERS_TIMEOUT_S
    )

    close_markers_drive_job <- function(status, error = NULL) {
      if (is.null(markers_drive_job$id)) return(invisible(FALSE))
      job <- tryCatch(ts_drive_job_state(), error = function(e) NULL)
      if (is.null(job) || !identical(job$job_id, markers_drive_job$id)) {
        return(invisible(FALSE))
      }
      ts_drive_job_finish(.SC_MARKERS_DRIVE_BUTTON, status = status, error = error,
                          job_id = markers_drive_job$id)
    }

    observeEvent(markers_drive_counter(), {
      if (!isTRUE(markers_drive_ready())) return()
      markers_drive_job$id <- NULL
      job <- tryCatch(ts_drive_job_state(), error = function(e) NULL)
      if (!is.null(job) && isTRUE(ts_drive_job_busy()) &&
          identical(job$button, .SC_MARKERS_DRIVE_BUTTON)) {
        markers_drive_job$id <- job$job_id
      }
      markers_drive_run_state("running")
      res <- .sc_markers_run_drive(global_data, shared_rv, session,
                                   close_markers_drive_job)
      markers_drive_last_step$value <- res$step
      markers_drive_run_state(res$status)
    }, ignoreInit = TRUE)

    # =========================================================================
    # REPORT STATUS
    # =========================================================================
    output$report_status <- renderText({
      if (is.null(global_data$sc_obj)) .tr("Importez et traitez un objet SC.")
      else sprintf(.tr("Prêt — %d viz. sauvegardée(s) dans le panier."),
                   length(state_get(shared_rv, "report_viz_list") %||% list()))
    })

    # =========================================================================
    # HTML / PDF REPORT
    # =========================================================================
    output$dl_report <- downloadHandler(
      filename = function() {
        ext <- switch(input$report_format, html="html", pdf="pdf", both="zip")
        paste0("rapport_singlecell_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".", ext)
      },
      content = function(file) {
        req(global_data$sc_obj)
        template_path <- file.path("reports","sc_report_template.Rmd")
        if (!file.exists(template_path)) stop(errorCondition("Template introuvable : reports/sc_report_template.Rmd", class = "mod_sc_error"))
        tmp_rmd <- file.path(tempdir(), "sc_report_template.Rmd")
        file.copy(template_path, tmp_rmd, overwrite=TRUE)

        # NULL-guard corr params
        corr_genes  <- if (!is.null(state_get(shared_rv, "correlated_genes")) &&
                           is.data.frame(state_get(shared_rv, "correlated_genes")) &&
                           nrow(state_get(shared_rv, "correlated_genes")) > 0) state_get(shared_rv, "correlated_genes") else NULL
        corr_target <- if (!is.null(state_get(shared_rv, "corr_target_gene")) &&
                           nchar(state_get(shared_rv, "corr_target_gene") %||% "") > 0) state_get(shared_rv, "corr_target_gene") else NULL

        # i18n Phase 6 : build translation map for report (French keys -> current language)
        lang <- isolate(global_data$language) %||% "fr"
        i18n_strings <- tryCatch({
          json_path <- file.path("i18n", "translation.json")
          if (file.exists(json_path)) {
            j <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)
            trans <- j$translation
            if (!is.null(trans)) {
              s <- setNames(
                vapply(trans, function(x) if (lang == "en") x$en %||% x$fr else x$fr, character(1)),
                vapply(trans, function(x) x$fr, character(1))
              )
              as.list(s)
            } else NULL
          } else NULL
        }, error = function(e) NULL)

        render_params <- list(
          sc_obj           = global_data$sc_obj,
          markers_data     = state_get(shared_rv, "markers_data"),
          pathway_results  = state_get(shared_rv, "pathway_results"),
          pathway_db       = state_get(shared_rv, "pathway_db"),
          correlated_genes = corr_genes,
          corr_target_gene = corr_target,
          sections         = input$report_sections %||% character(0),
          reduction        = "umap",
          traj_reduction   = state_get(shared_rv, "traj_reduction") %||% "umap",
          traj_method      = state_get(shared_rv, "traj_method"),   # transient fallback; obj@meta.data provenance wins in the report
          traj_genes       = state_get(shared_rv, "traj_genes") %||% character(0),  # Step-3.7
          # Post-V1.0 (mandat utilisateur) : resultats canoniques exposes au
          # rapport Rmd — restitues TELS QUELS (tables, aucune re-execution).
          velocity_result      = state_get(shared_rv, "velocity_result"),
          communication_result = state_get(shared_rv, "communication_result"),
          communication_collection = state_get(shared_rv, "communication_collection"),
          active_communication_sample = state_get(shared_rv, "active_communication_sample"),
          da_design_result     = state_get(shared_rv, "da_design_result"),
          da_milo_result       = state_get(shared_rv, "da_milo_result"),
          da_sccoda_result     = state_get(shared_rv, "da_sccoda_result"),
          # CCC 9 (Q1) : rareté par population — restituée TELLE QUELLE
          # (table descriptive, aucune re-exécution).
          population_rarity_result = state_get(shared_rv, "population_rarity_result"),
          saved_viz_list   = if (length(state_get(shared_rv, "report_viz_list"))) state_get(shared_rv, "report_viz_list") else NULL,
          group_by         = "seurat_clusters",
          sc_palette         = state_get(shared_rv, "sc_palette") %||% "default",
          sc_manual_colors   = state_get(shared_rv, "sc_manual_colors"),
          sc_manual_gradient = state_get(shared_rv, "sc_manual_gradient"),
          report_title     = input$report_title    %||% "Analyse Single-Cell",
          report_subtitle  = input$report_subtitle %||% "",
          report_notes     = input$report_notes    %||% "",
          interactive      = isTRUE(input$report_interactive) && input$report_format != "pdf",
          i18n_strings     = i18n_strings,
          report_language  = lang
        )

        withProgress(message=.tr_plain("Génération du rapport..."), value=0.2, {
          formats_needed <- switch(input$report_format,
            html="html_document", pdf="pdf_document",
            both=c("html_document","pdf_document"))
          out_files <- character(0)
          for (fmt in formats_needed) {
            incProgress(0.3, detail=paste("Rendu", fmt))
            ext_i    <- if (fmt=="html_document") "html" else "pdf"
            out_path <- tempfile(pattern=paste0("sc_report_",ext_i,"_"),
                                 fileext=paste0(".",ext_i))
            res <- tryCatch(
              rmarkdown::render(input=tmp_rmd, output_format=fmt, output_file=out_path,
                                params=render_params, envir=new.env(parent=globalenv()),
                                quiet=TRUE),
              error=function(e) {
                showNotification(paste0("\u274c ", fmt, ": ", conditionMessage(e)),
                                 type="error", duration=12); NULL })
            if (!is.null(res)) out_files <- c(out_files, res)
          }
          if (!length(out_files)) stop(errorCondition(.tr_plain("Aucun format généré."), class = "mod_sc_error"))
          else if (length(out_files)==1) file.copy(out_files[1], file, overwrite=TRUE)
          else zip::zip(file, files=out_files, mode="cherry-pick")
        })
      }
    )

    # =========================================================================
    # SC REPRODUCIBLE R SCRIPT
    # =========================================================================
    output$dl_sc_r_script <- downloadHandler(
      filename = function() paste0("analyse_sc_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".zip"),
      content  = function(file) {
        req(global_data$sc_obj)
        obj     <- global_data$sc_obj
        tmp_dir <- tempfile("sc_script_"); dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive=TRUE), add=TRUE)
        stamp       <- format(Sys.time(),"%Y%m%d_%H%M%S")
        script_path <- file.path(tmp_dir, paste0("analyse_sc_",stamp,".R"))
        rds_path    <- file.path(tmp_dir, "sc_obj.rds")
        writeLines(sc_r_script_text(obj, shared_rv), script_path)
        saveRDS(obj, rds_path)
        zip::zip(file, files=c(script_path, rds_path), mode="cherry-pick")
        showNotification("\u2713 Script R généré.", type="message", duration=4)
      }
    )

  }) # /moduleServer
}


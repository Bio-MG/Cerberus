# =============================================================================
# test-mod-sc-drive.R — Drive live control for the SC auto-pipeline
# =============================================================================
# Scope: the three drive actions `sc-pipeline-run_auto_pipeline`,
# `sc-annotation-run_annot` and `sc-markers-run_markers` (modules
# `sc_pipeline`, `sc_annotation` and `sc_markers`). The auto-pipeline keeps
# decision A1; the annotation and marker actions call `run_annot()` and
# `run_markers()` with frozen, declared input sets instead of binding DOM
# buttons.
#
# Decision B1: `R/sc/sc_pipeline.R` is NOT modified. Its outer `tryCatch` keeps
# swallowing the error (logged + notified, then returns), and the source lock in
# test-sc-pipeline.R:54-65 stays intact. Truthfulness is therefore bought on the
# DRIVE side: a per-step outcome record, derived from observable state, plus a
# log-delta probe for the swallowed error. Every value is one of
# `skipped` / `running` / `ran` / `ignored` / `error` — `ignored` exists because
# "selected, attempted, and the pipeline itself declined it" (a size guard) is
# NOT the same claim as "ran", and NOT the same as a failure.
#
# The drive helpers are FILE-LEVEL functions in `modules/sc/mod_sc.R` on purpose:
# `mod_sc_server()` fans out to ~20 sibling servers, so `testServer()` on it
# would need the whole application mocked. The observer is therefore a thin
# wrapper and the logic is exercised directly, exactly as
# `test-sc-auto-pipeline.R` exercises `run_sc_auto_pipeline()`.
# =============================================================================

source_project_file("config/defaults.R")
source_project_file("R/core/io_helpers.R")    # %||%
source_project_file("R/core/state.R")         # create_sc_shared_state
source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")
source_project_file("R/sc/sc_helpers.R")      # resolve_sketch_preset, robust_find_clusters
source_project_file("R/sc/sc_bpcells.R")      # smart_scale_data, sc_backend_status
source_project_file("R/sc/sc_trajectory.R")   # calculate_pseudotime
source_project_file("R/sc/sc_pipeline.R")     # run_sc_auto_pipeline (READ-ONLY, decision B1)
source_project_file("modules/sc/mod_sc_pipeline.R")
source_project_file("modules/sc/mod_sc_trajectory.R")
source_project_file("modules/sc/mod_sc_annotation.R")
source_project_file("modules/sc/mod_sc_markers.R")
source_project_file("modules/sc/mod_sc.R")     # the drive helpers under test

if (!exists(".tr", envir = globalenv()))
  assign(".tr", function(key) key, envir = globalenv())

# --- Fixture: a tiny deterministic Seurat object (2 clusters, 2 samples) ------
.scd_make_obj <- function(n_per_group = 150L, n_genes = 120L, seed = 42) {
  set.seed(seed)
  n_cells <- 2L * n_per_group
  mat <- matrix(rpois(n_genes * n_cells, lambda = 1),
                nrow = n_genes, dimnames = list(paste0("G", seq_len(n_genes)), NULL))
  mat[1:15, seq_len(n_per_group)] <-
    mat[1:15, seq_len(n_per_group)] + rpois(15L * n_per_group, 8)
  mat[16:30, (n_per_group + 1L):n_cells] <-
    mat[16:30, (n_per_group + 1L):n_cells] + rpois(15L * n_per_group, 8)
  mt <- matrix(rpois(3L * n_cells, lambda = 1), nrow = 3L,
               dimnames = list(c("MT-A", "MT-B", "MT-C"), NULL))
  mat <- rbind(mat, mt)
  # `CreateSeuratObject` coerces a base matrix and says so. The warning is
  # EXPECTED and unrelated to what is under test; left in place it would be
  # counted against this file on every run.
  obj <- suppressWarnings(CreateSeuratObject(counts = mat))
  obj$orig.ident <- factor(rep(c("s1", "s2"), each = n_per_group))
  obj
}

.scd_make_marker_obj <- function(n_per_group = 2L, n_genes = 40L, seed = 42) {
  obj <- .scd_make_obj(n_per_group = n_per_group, n_genes = n_genes, seed = seed)
  obj$seurat_clusters <- factor(rep(c("g1", "g2"), each = n_per_group))
  obj
}
# The step record is a NAMED list of length-1 strings (that shape is what the
# snapshot projector walks), so comparing it to a bare `rep("ran", 8L)` needs the
# names dropped — `unlist()` alone keeps them.
.scd_vals <- function(x) unname(unlist(x))

# Runs the drive wrapper exactly as the observer does, on a MockShinySession.
.scd_run_drive <- function(obj, seed_log = "", shared_rv = NULL, inputs = NULL) {
  gd <- shiny::reactiveValues(sc_obj = obj)
  if (is.null(shared_rv)) shared_rv <- create_sc_shared_state()
  sc_log_rv <- shiny::reactiveVal(seed_log)
  closed <- list()
  mock <- shiny::MockShinySession$new()
  # inputs = NULL → défauts drive canoniques ; sinon surcharge explicite
  # (QW-2 : la fixture ne survit pas aux défauts QC canoniques).
  if (is.null(inputs)) inputs <- .sc_ap_drive_inputs()
  res <- shiny::isolate(shiny:::withReactiveDomain(mock, {
    .sc_ap_run_drive(gd, shared_rv, mock, sc_log_rv,
                     function(status, error = NULL) {
                       closed[[length(closed) + 1L]] <<- list(status = status, error = error)
                       invisible(TRUE)
                     },
                     inputs = inputs)
  }))
  list(res = res, closed = closed, shared_rv = shared_rv, log = shiny::isolate(sc_log_rv()))
}

test_that("the drive input set is frozen, closed, and carries no modal-only key", {
  inputs <- .sc_ap_drive_inputs()
  # Every key the domain function reads is present: a missing one would reach
  # run_sc_auto_pipeline() as NULL and silently change a threshold.
  expect_setequal(names(inputs), c(
    "sc_ap_mapping", "sc_ap_mapping_org", "sc_ap_bpcells",
    "sc_ap_min_gene", "sc_ap_max_gene", "sc_ap_mt", "sc_ap_norm",
    "sc_ap_pca_dim", "sc_ap_res", "sc_ap_cluster_algo", "sc_ap_compute_umap",
    "sc_ap_sketch_preset", "sc_ap_sketch_ncells_custom",
    "sc_ap_singler", "sc_ap_singler_ref", "sc_ap_singler_level",
    "sc_ap_markers", "sc_ap_pathway", "sc_ap_pathway_db", "sc_ap_pathway_org",
    "sc_ap_correlation", "sc_ap_trajectory"
  ))
  expect_length(inputs, 22L)
  # Declared values. `sketch_preset = "max"` means FULL dataset: the drive never
  # depends on a hidden sub-sampling decision, and `bpcells = FALSE` keeps the
  # action free of any disk write.
  expect_identical(inputs$sc_ap_mapping, FALSE)
  expect_identical(inputs$sc_ap_bpcells, FALSE)
  expect_identical(inputs$sc_ap_sketch_preset, "max")
  expect_identical(inputs$sc_ap_compute_umap, TRUE)
  expect_identical(inputs$sc_ap_markers, TRUE)
  expect_identical(inputs$sc_ap_pathway, FALSE)
  expect_identical(inputs$sc_ap_singler, FALSE)
  expect_identical(inputs$sc_ap_correlation, FALSE)
  expect_identical(inputs$sc_ap_trajectory, TRUE)
  # QW-2 (2026-09-30) : les défauts drive lisent la source unique config/ —
  # les anciens 10/50 (plus permissifs que l'UI) créaient un fork de
  # reproductibilité. Le pin suit désormais la constante, pas un littéral.
  expect_identical(inputs$sc_ap_min_gene, TS_SC_QC_MIN_GENES)
  expect_identical(inputs$sc_ap_mt, TS_SC_QC_MAX_PCT_MT)
  # `sc_ap_confirm` is the MODAL's own button: driving it would be the two-step
  # A2 design this scope rejected.
  expect_false(any(grepl("confirm", names(inputs), fixed = TRUE)))

  sel <- .sc_ap_drive_selection()
  expect_identical(names(sel), .SC_AP_DRIVE_STEPS)
  expect_length(sel, 12L)
  expect_true(all(is.logical(sel)))
  expect_identical(unname(sel[c("qc", "norm", "pca", "clusters")]), rep(TRUE, 4L))
  expect_identical(unname(sel[c("mapping", "singler", "pathway", "correlation")]),
                   rep(FALSE, 4L))
})

test_that("the step recorder is an ENVIRONMENT and its vocabulary is closed", {
  sel <- .sc_ap_drive_selection()
  steps <- .sc_ap_steps_new(sel)
  # An environment, never a list: a list is copied on every write, so an
  # assignment from inside a callback would be written into a copy and lost in
  # silence. Measured trap, already paid once on the Spatial pipeline.
  expect_true(is.environment(steps))
  # Selected steps start as "running", unselected ones as "skipped".
  expect_identical(steps$markers, "running")
  expect_identical(steps$trajectory, "running")
  expect_identical(steps$mapping, "skipped")
  expect_identical(steps$pathway, "skipped")
  # A mutation is visible to the holder (the environment semantics, asserted).
  .sc_ap_step_set(steps, "markers", "ran")
  expect_identical(steps$markers, "ran")
  # Off-vocabulary writes and unknown step names are refused, not stored.
  expect_error(.sc_ap_step_set(steps, "markers", "fine"))
  expect_error(.sc_ap_step_set(steps, "not_a_step", "ran"))
  expect_true(all(.SC_AP_DRIVE_STEP_STATES %in%
                    c("skipped", "running", "ran", "ignored", "error")))
})

test_that("per-step outcomes are derived from observable state, not from the request", {
  skip_if_not_installed("Seurat")
  library(Seurat)
  library(shiny)

  gd <- shiny::reactiveValues(sc_obj = .scd_make_obj())
  shared_rv <- create_sc_shared_state()
  sc_log_rv <- shiny::reactiveVal("")
  mock <- shiny::MockShinySession$new()
  # QW-2 (2026-09-30) : les défauts drive canoniques (100 min gènes, 8000 max,
  # 20 % mito) écrasent la fixture (123 gènes, rpois λ=1). Ce test exerce la
  # MÉCANIQUE du pipeline, pas les défauts — épinglés plus haut et par
  # test-sc-qc-defaults-parity.R. Le scénario surcharge donc le QC, comme le
  # permet le contrat des entrées.
  inputs <- .sc_ap_drive_inputs()
  inputs$sc_ap_min_gene <- 10
  inputs$sc_ap_max_gene <- 10000
  inputs$sc_ap_mt <- 50
  inputs$sc_ap_pca_dim <- 10
  shiny::isolate(shiny:::withReactiveDomain(mock, {
    run_sc_auto_pipeline(inputs, gd, shared_rv, mock, sc_log_rv)
  }))
  obj <- shiny::isolate(gd$sc_obj)
  expect_s4_class(obj, "Seurat")

  produced <- list(
    markers = !is.null(state_get(shared_rv, "markers_data")),
    pathway = FALSE, correlation = FALSE
  )
  out <- .sc_ap_step_outcomes(obj, .sc_ap_drive_selection(), produced = produced,
                              n_genes_before = nrow(.scd_make_obj()))
  expect_identical(names(out), .SC_AP_DRIVE_STEPS)
  expect_true(all(unlist(out) %in% .SC_AP_DRIVE_STEP_STATES))
  # Selected AND produced a result.
  expect_identical(.scd_vals(out[c("qc", "norm", "pca", "clusters", "umap", "tsne",
                                "markers", "trajectory")]), rep("ran", 8L))
  # Not selected: never claimed.
  expect_identical(.scd_vals(out[c("mapping", "singler", "pathway", "correlation")]),
                   rep("skipped", 4L))
})

test_that("a selected step with no result is `error`, and a size guard is `ignored`", {
  skip_if_not_installed("Seurat")
  library(Seurat)
  # The RAW fixture: 300 cells, no reductions, no meta columns. Nothing ran, so
  # every selected step must read as a failure rather than a success.
  obj <- .scd_make_obj()
  sel <- .sc_ap_drive_selection()
  sel[] <- TRUE
  out <- .sc_ap_step_outcomes(obj, sel, produced = list(markers = FALSE,
                                                        pathway = FALSE,
                                                        correlation = FALSE),
                              n_genes_before = nrow(obj))
  expect_identical(.scd_vals(out[setdiff(names(out), "mapping")]), rep("error", 11L))
  # Mapping is the one deliberate exception: an unchanged gene count means the
  # detector found symbols already, so the step was a NO-OP, not a failure.
  expect_identical(.scd_vals(out["mapping"]), "ignored")

  # A size guard that fired is `ignored`, NOT `ran` and NOT `error`. The
  # thresholds are injected so the guard is exercised without building a 30k-cell
  # object; the production values are the module constants asserted below.
  guarded <- .sc_ap_step_outcomes(obj, sel, produced = list(markers = TRUE,
                                                           pathway = TRUE,
                                                           correlation = TRUE),
                                  n_genes_before = nrow(obj), tsne_max = 10L,
                                  traj_max = 10L)
  expect_identical(.scd_vals(guarded["tsne"]), "ignored")
  expect_identical(.scd_vals(guarded["trajectory"]), "ignored")
  expect_identical(.scd_vals(guarded[c("markers", "pathway", "correlation")]), rep("ran", 3L))
  # The guard must be reachable with the REAL thresholds too: below them the same
  # object is an `error`, never a silent `ran`.
  below <- .sc_ap_step_outcomes(obj, sel, produced = list(markers = TRUE),
                                n_genes_before = nrow(obj))
  expect_identical(.scd_vals(below["tsne"]), "error")
  expect_true(.AUTO_TSNE_MAX_CELLS == 30000L)
  expect_true(.MAX_TRAJECTORY_CELLS == 100000L)
})

test_that(".sc_ap_run_drive closes the job `done` and reports each step honestly", {
  skip_if_not_installed("Seurat")
  library(Seurat)
  library(shiny)

  out <- .scd_run_drive(.scd_make_obj(), inputs = {
    d <- .sc_ap_drive_inputs()
    d$sc_ap_min_gene <- 10; d$sc_ap_max_gene <- 10000
    d$sc_ap_mt <- 50; d$sc_ap_pca_dim <- 10
    d
  })
  expect_identical(out$res$status, "done")
  expect_length(out$closed, 1L)
  expect_identical(out$closed[[1]]$status, "done")
  expect_null(out$closed[[1]]$error)
  steps <- out$res$steps
  expect_identical(names(steps), .SC_AP_DRIVE_STEPS)
  expect_true(all(vapply(steps, function(x) length(x) == 1L, logical(1))))
  expect_identical(.scd_vals(steps[c("qc", "norm", "pca", "clusters", "umap", "tsne",
                                  "markers", "trajectory")]), rep("ran", 8L))
  expect_identical(.scd_vals(steps[c("mapping", "singler", "pathway", "correlation")]),
                   rep("skipped", 4L))
  expect_gt(out$res$n_results, 0L)
  expect_false(grepl("\u274c", out$log, fixed = TRUE))
})

test_that(".sc_ap_run_drive reports `error` when the pipeline swallows a failure", {
  skip_if_not_installed("Seurat")
  library(Seurat)
  library(shiny)

  # `req(global_data$sc_obj)` inside run_sc_auto_pipeline() stops silently
  # inside a reactive domain, so the wrapper sees a condition, not a return
  # value. Readiness normally prevents this; the point is that the WRAPPER
  # still refuses to claim success.
  out <- .scd_run_drive(NULL)
  expect_identical(out$res$status, "error")
  expect_length(out$closed, 1L)
  expect_identical(out$closed[[1]]$status, "error")
  expect_true(all(unlist(out$res$steps) %in% .SC_AP_DRIVE_STEP_STATES))
  expect_identical(out$res$n_results, 0L)
})

test_that("the log-delta probe catches the swallowed error the return value hides", {
  # The inner handler logs `.tr("\u274c Erreur:")`. MEASURED in
  # i18n/translation.json: the emoji is present in BOTH the fr and en values,
  # so the marker is language-independent — unlike the word after it, which is
  # exactly the part a translation may change. `translation` is an UNNAMED list
  # of {fr, en} pairs, hence the search rather than a keyed lookup.
  json <- jsonlite::fromJSON(file.path(ts_project_root(), "i18n", "translation.json"),
                             simplifyVector = FALSE)
  entries <- json$translation
  idx <- which(vapply(entries, function(x) identical(x$fr, "\u274c Erreur:"),
                      logical(1)))
  expect_length(idx, 1L)
  expect_match(entries[[idx]]$fr, "\u274c", fixed = TRUE)
  expect_match(entries[[idx]]$en, "\u274c", fixed = TRUE)
})

test_that("the published view is a CLOSED contract of scalars plus a steps slot", {
  skip_if_not_installed("Seurat")
  library(Seurat)
  library(shiny)

  steps <- .sc_ap_steps_as_list(.sc_ap_steps_new(.sc_ap_drive_selection()))
  view <- .sc_ap_drive_view("idle", 0, 0L, 0L, has_data = FALSE, ready = FALSE,
                            steps = steps)
  expect_setequal(names(view), c("module", "action", "status", "elapsed_s", "seq",
                                 "n_results", "has_data", "ready", "steps"))
  expect_identical(view$module, "sc_pipeline")
  expect_identical(view$action, "run_pipeline")
  expect_identical(view$status, "idle")
  expect_true(all(vapply(view[setdiff(names(view), "steps")],
                         function(x) length(x) == 1L, logical(1))))
  expect_identical(names(view$steps), .SC_AP_DRIVE_STEPS)

  # A view with no recorded run must not invent progress: nothing has run. The
  # NULL default is what the module's state probe passes before the first drive
  # action, and it must not say `running` for the selected steps.
  idle <- .sc_ap_drive_view("idle", 0, 0L, 0L)
  expect_identical(names(idle$steps), .SC_AP_DRIVE_STEPS)
  expect_true(all(unlist(idle$steps) == "skipped"))

  # REDACTION. Seed the shared state with the exact payloads the disclosure
  # contract forbids (gene name, cell id, sample label, absolute path) and
  # prove none of them can reach the projection.
  shared_rv <- create_sc_shared_state()
  shared_rv$markers_data <- data.frame(gene = "SENTINEL_GENE_NAME",
                                       cluster = "SENTINEL_CLUSTER")
  shared_rv$corr_target_gene <- "SENTINEL_GENE_NAME"
  shared_rv$correlated_genes <- data.frame(gene = "SENTINEL_GENE_NAME")
  shared_rv$pathway_results <- data.frame(pathway = "SENTINEL_PATHWAY")
  obj <- .scd_make_obj()
  produced <- .sc_ap_produced(shared_rv)
  expect_true(isTRUE(produced$markers))
  view2 <- .sc_ap_drive_view("done", 1.5, 7L,
                             .sc_ap_count_results(obj, produced),
                             has_data = TRUE, ready = TRUE, steps = steps)
  flat <- unlist(view2, use.names = TRUE)
  expect_false(any(grepl("SENTINEL", flat, fixed = TRUE)))
  expect_false(any(grepl("MT-A|/d/|C:/", flat)))
  expect_identical(view2$seq, 7L)
  expect_identical(view2$elapsed_s, 1.5)
  expect_gt(view2$n_results, 0L)
})

test_that("the declared timeout is set, larger than Spatial's, and used by the token", {
  expect_true(is.numeric(TS_SC_AUTO_PIPELINE_TIMEOUT_S))
  expect_length(TS_SC_AUTO_PIPELINE_TIMEOUT_S, 1L)
  expect_identical(TS_SC_AUTO_PIPELINE_TIMEOUT_S, 14400)
  expect_true(TS_SC_AUTO_PIPELINE_TIMEOUT_S > TS_SPATIAL_PIPELINE_TIMEOUT_S)

  # The published token carries it: this is the same call mod_sc.R makes, so a
  # regression in the wiring (long / timeout) is visible here.
  gd <- list(drive_registry = new.env(parent = emptyenv()))
  counter <- shiny::reactiveVal(0L)
  ok <- ts_drive_publish_token(gd, "sc-pipeline-run_auto_pipeline", counter,
                               ready = function() TRUE,
                               state = function() list(status = "idle"),
                               long = TRUE, timeout_s = TS_SC_AUTO_PIPELINE_TIMEOUT_S)
  expect_true(ok)
  entry <- gd$drive_registry[["sc-pipeline-run_auto_pipeline"]]
  expect_true(isTRUE(entry$long))
  expect_identical(entry$timeout_s, TS_SC_AUTO_PIPELINE_TIMEOUT_S)
  expect_true(is.function(entry$ready))
  expect_true(is.function(entry$state))
})

test_that("the published id is the allowlist id, and the wiring is greppable", {
  # The call site writes the id as a LITERAL (see the comment there): the
  # wiring guard in test-drive-watcher.R greps the module sources for it. This
  # test closes the other half of that trade — the literal cannot drift away
  # from the frozen constant.
  src <- readLines(file.path(ts_project_root(), "modules", "sc", "mod_sc.R"),
                   warn = FALSE, encoding = "UTF-8")
  hit <- grep('ts_drive_publish_token\\(.*"sc-pipeline-run_auto_pipeline"', src)
  expect_length(hit, 1L)
  window <- paste(src[hit[1]:min(length(src), hit[1] + 4L)], collapse = "\n")
  # A binding without a readiness guard turns a doomed run into `done`.
  expect_match(window, "ready")
  expect_match(window, "long = TRUE")
  expect_identical(TS_DRIVE_SC_BUTTON, "sc-pipeline-run_auto_pipeline")
  expect_identical(.SC_AP_DRIVE_BUTTON, TS_DRIVE_SC_BUTTON)
  expect_identical(.SC_AP_DRIVE_MODULE, TS_DRIVE_SC_MODULE)
  expect_identical(TS_DRIVE_SC_ANNOTATION_BUTTON, "sc-annotation-run_annot")
  expect_identical(.SC_ANNOT_DRIVE_BUTTON, TS_DRIVE_SC_ANNOTATION_BUTTON)
  expect_identical(.SC_ANNOT_DRIVE_MODULE, TS_DRIVE_SC_ANNOTATION_MODULE)
  expect_length(grep('ts_drive_publish_token\\(.*"sc-annotation-run_annot"', src), 1L)
  expect_length(grep('ts_drive_bind_button\\(.*"sc-annotation-run_annot"', src), 0L)
  # The DOM button that only opens the modal must not be published anywhere.
  expect_length(grep('ts_drive_publish_token\\(.*"sc-btn_auto_pipeline_sc"', src), 0L)
})

test_that("the SC annotation input set is frozen and run_annot updates shared state", {
  skip_if_not_installed("Seurat")
  library(Seurat)
  library(shiny)

  inputs <- .sc_annot_drive_inputs()
  expect_identical(inputs, list(ref_singler = "hpca", label_level = "main",
                               maxcells = 50000L))

  old <- get(".run_singler_safe", envir = globalenv())
  on.exit(assign(".run_singler_safe", old, envir = globalenv()), add = TRUE)
  assign(".run_singler_safe", function(obj, refcode, labellevel, maxcells = 50000L) {
    list(labels = factor(c("A", "A", "B", "B")), method = "fixture")
  }, envir = globalenv())

  gd <- shiny::reactiveValues(sc_obj = .scd_make_obj(n_per_group = 2L, n_genes = 40L))
  shared_rv <- create_sc_shared_state()
  mock <- shiny::MockShinySession$new()
  out <- shiny::isolate(shiny:::withReactiveDomain(mock, {
    run_annot(inputs, gd, shared_rv, mock)
  }))

  expect_true(out$ok)
  expect_identical(out$n_results, 2L)
  expect_true("SingleR_hpca_main" %in% colnames(shiny::isolate(gd$sc_obj)@meta.data))
  expect_identical(shiny::isolate(shared_rv$active_tab), "tab_viz")
})

test_that("the SC annotation drive view is closed and uses the five-state vocabulary", {
  expect_identical(TS_SC_ANNOTATION_TIMEOUT_S, TS_SC_AUTO_PIPELINE_TIMEOUT_S)
  view <- .sc_annot_drive_view("done", 2.5, 9L, 3L,
                               has_data = TRUE, ready = TRUE, step = "ran")
  expect_setequal(names(view), c("module", "action", "status", "elapsed_s", "seq",
                                 "n_results", "has_data", "ready", "steps"))
  expect_identical(view$module, "sc_annotation")
  expect_identical(view$action, "run_pipeline")
  expect_identical(view$steps, list(singler = "ran"))
  expect_identical(.sc_annot_drive_view("idle")$steps, list(singler = "skipped"))
  expect_identical(.sc_annot_drive_view("error", step = "not-a-state")$steps$singler,
                   "error")
  expect_true(all(c("ran", "skipped", "ignored", "error", "running") %in%
                    .SC_AP_DRIVE_STEP_STATES))
})

test_that("the SC marker input set is frozen and run_markers updates shared state", {
  skip_if_not_installed("Seurat")
  library(Seurat)
  library(shiny)

  inputs <- .sc_markers_drive_inputs()
  expect_identical(inputs, list(
    marker_test = "wilcox", marker_min_pct = 0.10, marker_logfc = 0.25,
    group_col = "seurat_clusters", max_per_group = 5000L,
    only_pos = TRUE, verbose = FALSE
  ))

  old <- get("FindAllMarkers", envir = globalenv())
  on.exit(assign("FindAllMarkers", old, envir = globalenv()), add = TRUE)
  assign("FindAllMarkers", function(obj, ...) {
    data.frame(gene = c("M1", "M2"), cluster = c("g1", "g2"),
               avg_log2FC = c(1.2, 0.8), p_val_adj = c(0.01, 0.02),
               pct.1 = c(0.8, 0.7), pct.2 = c(0.1, 0.2),
               stringsAsFactors = FALSE)
  }, envir = globalenv())

  gd <- shiny::reactiveValues(sc_obj = .scd_make_marker_obj())
  shared_rv <- create_sc_shared_state()
  mock <- shiny::MockShinySession$new()
  out <- shiny::isolate(shiny:::withReactiveDomain(mock, {
    run_markers(inputs, gd, shared_rv, mock)
  }))

  expect_true(out$ok)
  expect_identical(out$n_results, 2L)
  expect_identical(nrow(shiny::isolate(shared_rv$markers_data)), 2L)
  expect_identical(shiny::isolate(shared_rv$active_tab), "tab_table")
})

test_that("the SC annotation drive reports an error instead of claiming success", {
  old <- get("run_annot", envir = globalenv())
  on.exit(assign("run_annot", old, envir = globalenv()), add = TRUE)
  assign("run_annot", function(...) list(ok = FALSE, n_results = 99L),
         envir = globalenv())
  closed <- list()
  out <- .sc_annot_run_drive(list(), list(), NULL,
                             function(status, error = NULL) {
                               closed[[1L]] <<- list(status = status, error = error)
                               invisible(TRUE)
                             })
  expect_identical(out$status, "error")
  expect_identical(out$n_results, 0L)
  expect_identical(out$step, "error")
  expect_identical(closed[[1L]]$status, "error")
  expect_true(nzchar(closed[[1L]]$error))
})

test_that("the SC marker drive reports an error instead of claiming success", {
  old <- get("run_markers", envir = globalenv())
  on.exit(assign("run_markers", old, envir = globalenv()), add = TRUE)
  assign("run_markers", function(...) list(ok = FALSE, n_results = 99L),
         envir = globalenv())
  closed <- list()
  out <- .sc_markers_run_drive(list(), list(), NULL,
                               function(status, error = NULL) {
                                 closed[[1L]] <<- list(status = status, error = error)
                                 invisible(TRUE)
                               })
  expect_identical(out$status, "error")
  expect_identical(out$n_results, 0L)
  expect_identical(out$step, "error")
  expect_identical(closed[[1L]]$status, "error")
})

test_that("the SC marker drive view and token are closed and greppable", {
  expect_identical(TS_SC_MARKERS_TIMEOUT_S, TS_SC_AUTO_PIPELINE_TIMEOUT_S)
  view <- .sc_markers_drive_view("done", 3.5, 11L, 4L,
                                 has_data = TRUE, ready = TRUE, step = "ran")
  expect_setequal(names(view), c("module", "action", "status", "elapsed_s", "seq",
                                 "n_results", "has_data", "ready", "steps"))
  expect_identical(view$module, "sc_markers")
  expect_identical(view$action, "run_pipeline")
  expect_identical(view$steps, list(markers = "ran"))
  expect_identical(.sc_markers_drive_view("idle")$steps, list(markers = "skipped"))
  expect_identical(TS_DRIVE_SC_MARKERS_BUTTON, "sc-markers-run_markers")
  expect_identical(.SC_MARKERS_DRIVE_BUTTON, TS_DRIVE_SC_MARKERS_BUTTON)
  expect_identical(.SC_MARKERS_DRIVE_MODULE, TS_DRIVE_SC_MARKERS_MODULE)
  src <- readLines(file.path(ts_project_root(), "modules", "sc", "mod_sc.R"),
                   warn = FALSE, encoding = "UTF-8")
  expect_length(grep('ts_drive_publish_token\\(.*"sc-markers-run_markers"', src), 1L)
  expect_length(grep('ts_drive_bind_button\\(.*"sc-markers-run_markers"', src), 0L)
})

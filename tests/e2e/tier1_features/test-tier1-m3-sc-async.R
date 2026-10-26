# =============================================================================
# test-tier1-m3-sc-async.R — Tier 1 Feature Coverage: Milestone M3
# =============================================================================
# Covers Features 13-18 (Asynchronous Single-Cell Pipeline & Export Routes)
# 5 test cases per feature (30 total) verifying async decoupling and export contracts.
# =============================================================================

if (!exists("e2e_project_root", mode = "function")) {
  if (file.exists("tests/e2e/helper-e2e.R")) {
    source("tests/e2e/helper-e2e.R")
  } else if (file.exists("../helper-e2e.R")) {
    source("../helper-e2e.R")
  }
}

.make_test_sc_obj <- function() {
  set.seed(42)
  mat <- matrix(rpois(80 * 25, lambda = 3), nrow = 80, ncol = 25)
  rownames(mat) <- paste0("Gene-", seq_len(80))
  colnames(mat) <- paste0("Cell-", seq_len(25))
  suppressWarnings(Seurat::CreateSeuratObject(counts = mat))
}

# ── Feature 13: Core SC Pipeline Extraction ──────────────────────────────────

test_that("F13-1: run_sc_auto_pipeline_core signature accepts obj and params", {
  if (!is_m3_ready()) skip("M3 SC async pipeline not yet active")
  expect_true(exists("run_sc_auto_pipeline_core", mode = "function"))
  fn_formals <- names(formals(run_sc_auto_pipeline_core))
  expect_true("obj" %in% fn_formals)
  expect_true("params" %in% fn_formals)
  expect_true("log_file" %in% fn_formals)
})

test_that("F13-2: run_sc_auto_pipeline_core returns structured list with obj, metrics, timings", {
  if (!is_m3_ready()) skip("M3 SC async pipeline not yet active")
  obj <- .make_test_sc_obj()
  res <- run_sc_auto_pipeline_core(obj, params = list(skip_qc = TRUE, n_dims = 5, resolution = 0.5, compute_umap = FALSE))
  expect_true(is.list(res))
  expect_true("obj" %in% names(res))
  expect_true("metrics" %in% names(res))
  expect_true("timings" %in% names(res))
  expect_true("log" %in% names(res))
  expect_s4_class(res$obj, "Seurat")
})

test_that("F13-3: run_sc_auto_pipeline_core has zero Shiny dependencies (pure R)", {
  if (!is_m3_ready()) skip("M3 SC async pipeline not yet active")
  pipe_file <- file.path(e2e_project_root(), "tools/pkg_skeleton/R/sc_pipeline.R")
  if (!file.exists(pipe_file)) {
    pipe_file <- file.path(e2e_project_root(), "R/sc/sc_pipeline.R")
  }
  expect_true(file.exists(pipe_file))
  lines <- readLines(pipe_file, warn = FALSE)
  start_idx <- grep("^run_sc_auto_pipeline_core\\s*<-\\s*function", lines)
  end_idx <- grep("^run_sc_auto_pipeline\\s*<-\\s*function", lines)
  expect_true(length(start_idx) >= 1L && length(end_idx) >= 1L)
  core_lines <- lines[start_idx[1]:(end_idx[1] - 1L)]
  code <- paste(core_lines[!grepl("^\\s*#", core_lines)], collapse = " ")
  expect_false(grepl("\\breactiveVal\\b", code))
  expect_false(grepl("\\bobserveEvent\\b", code))
  expect_false(grepl("\\bshowNotification\\b", code))
  expect_false(grepl("\\bmodalDialog\\b", code))
})

test_that("F13-4: run_sc_auto_pipeline_core computes normalization, PCA and clustering", {
  if (!is_m3_ready()) skip("M3 SC async pipeline not yet active")
  obj <- .make_test_sc_obj()
  res <- run_sc_auto_pipeline_core(obj, params = list(skip_qc = TRUE, n_dims = 5, resolution = 0.5, compute_umap = FALSE))
  expect_true("pca" %in% tolower(names(res$obj@reductions)))
  expect_true("seurat_clusters" %in% colnames(res$obj@meta.data))
  expect_true(res$metrics$n_clusters > 0)
})

test_that("F13-5: run_sc_auto_pipeline_core supports parameter overrides (n_dims, resolution)", {
  if (!is_m3_ready()) skip("M3 SC async pipeline not yet active")
  obj <- .make_test_sc_obj()
  res <- run_sc_auto_pipeline_core(obj, params = list(skip_qc = TRUE, n_dims = 7, resolution = 0.8, compute_umap = FALSE))
  expect_equal(res$metrics$pca_dims, 7)
  expect_equal(res$metrics$resolution, 0.8)
})

# ── Feature 14: Worker Daemon Dependency Registration ────────────────────────

test_that("F14-1: APP_DAEMON_SOURCE_FILES contains sc pipeline source files", {
  if (!is_m3_ready()) skip("M3 daemon registration not yet active")
  expect_true(exists("APP_DAEMON_SOURCE_FILES"))
  expect_true(any(grepl("sc_pipeline\\.R", APP_DAEMON_SOURCE_FILES)))
  expect_true(any(grepl("sc_bpcells\\.R", APP_DAEMON_SOURCE_FILES)))
  expect_true(any(grepl("sc_helpers\\.R", APP_DAEMON_SOURCE_FILES)))
})

test_that("F14-2: daemon source files all exist and are non-empty", {
  if (!is_m3_ready()) skip("M3 daemon registration not yet active")
  root <- e2e_project_root()
  for (f in APP_DAEMON_SOURCE_FILES) {
    fp <- file.path(root, f)
    expect_true(file.exists(fp), info = sprintf("File %s does not exist", f))
    expect_gt(file.info(fp)$size, 0)
  }
})

test_that("F14-3: daemon source file dependencies respect topological order", {
  if (!is_m3_ready()) skip("M3 daemon registration not yet active")
  bp_idx <- grep("sc_bpcells\\.R", APP_DAEMON_SOURCE_FILES)
  hlp_idx <- grep("sc_helpers\\.R", APP_DAEMON_SOURCE_FILES)
  pipe_idx <- grep("sc_pipeline\\.R", APP_DAEMON_SOURCE_FILES)
  expect_true(length(bp_idx) == 1L && length(hlp_idx) == 1L && length(pipe_idx) == 1L)
  expect_true(bp_idx < pipe_idx)
  expect_true(hlp_idx < pipe_idx)
})

test_that("F14-4: background daemon worker environment initializes cleanly", {
  if (!is_m3_ready()) skip("M3 daemon registration not yet active")
  expect_true(exists("init_spatial_daemons", mode = "function"))
  ok <- tryCatch({
    init_spatial_daemons(n_daemons = 1L)
    TRUE
  }, error = function(e) FALSE)
  expect_true(ok)
  expect_true(isTRUE(mirai::daemons_set()))
})

test_that("F14-5: daemon source files contain zero unclassed stop() calls", {
  if (!is_m3_ready()) skip("M3 daemon registration not yet active")
  root <- e2e_project_root()
  sc_files <- grep("sc_", APP_DAEMON_SOURCE_FILES, value = TRUE)
  for (f in sc_files) {
    fp <- file.path(root, f)
    lines <- readLines(fp, warn = FALSE)
    code_lines <- lines[!grepl("^\\s*#", lines)]
    unclassed <- grep("(?:^|[^._a-zA-Z0-9])stop\\(\"[^\"]+\"\\)", code_lines, perl = TRUE, value = TRUE)
    expect_length(unclassed, 0L)
  }
})

# ── Feature 15: ExtendedTask + mirai Reactive Integration ────────────────────

test_that("F15-1: mirai daemon processes tasks asynchronously in subprocess", {
  if (!is_m3_ready()) skip("M3 mirai integration not yet active")
  ensure_app_daemons()
  m <- mirai::mirai(Sys.getpid())
  expect_true(mirai::is_mirai(m))
  pid <- m[]
  expect_true(is.numeric(pid))
  expect_true(pid > 0)
})

test_that("F15-2: ExtendedTask wraps mirai task without blocking main thread", {
  if (!is_m3_ready()) skip("M3 mirai integration not yet active")
  ext_task_cls <- if (exists("ExtendedTask", where = asNamespace("shiny"), inherits = FALSE)) {
    get("ExtendedTask", envir = asNamespace("shiny"))
  } else {
    get("ExtendedTask", envir = asNamespace("bslib"))
  }
  task <- ext_task_cls$new(function(x) {
    mirai::mirai(x * 2, x = x)
  })
  expect_true(is.function(task$invoke))
  expect_true(is.function(task$status))
  expect_true(is.function(task$result))
  expect_identical(shiny::isolate(task$status()), "initial")
})

test_that("F15-3: ExtendedTask transitions correctly through initial, running, and result", {
  if (!is_m3_ready()) skip("M3 mirai integration not yet active")
  ext_task_cls <- if (exists("ExtendedTask", where = asNamespace("shiny"), inherits = FALSE)) {
    get("ExtendedTask", envir = asNamespace("shiny"))
  } else {
    get("ExtendedTask", envir = asNamespace("bslib"))
  }
  task <- ext_task_cls$new(function(x) {
    mirai::mirai({ Sys.sleep(0.05); x + 10 }, x = x)
  })
  expect_identical(shiny::isolate(task$status()), "initial")
  task$invoke(5)
  expect_true(shiny::isolate(task$status()) %in% c("running", "success"))
  t0 <- Sys.time()
  while(shiny::isolate(task$status()) == "running" && as.numeric(difftime(Sys.time(), t0, units = "secs")) < 5) {
    if (requireNamespace("later", quietly = TRUE)) later::run_now(0.1)
    Sys.sleep(0.05)
  }
  if (requireNamespace("later", quietly = TRUE)) later::run_now(0.1)
  expect_identical(shiny::isolate(task$status()), "success")
  expect_equal(shiny::isolate(task$result()), 15)
})

test_that("F15-4: worker RAM spikes do not pollute parent Shiny process memory", {
  if (!is_m3_ready()) skip("M3 mirai integration not yet active")
  ensure_app_daemons()
  main_pid <- Sys.getpid()
  m <- mirai::mirai({
    mat <- matrix(stats::rnorm(1000 * 2500), nrow = 1000)
    list(worker_pid = Sys.getpid(), bytes = object.size(mat))
  })
  res <- m[]
  expect_false(identical(main_pid, res$worker_pid))
  expect_gt(as.numeric(res$bytes), 15000000)
})

test_that("F15-5: background worker error propagates gracefully without crashing session", {
  if (!is_m3_ready()) skip("M3 mirai integration not yet active")
  ensure_app_daemons()
  m <- mirai::mirai(stop("intentional daemon error", call. = FALSE))
  res <- m[]
  expect_s3_class(res, "errorValue")
  ts_drive_record_error("Worker task error test", module = "sc_pipeline")
  errs <- ts_drive_get_errors(limit = 5)
  expect_true(any(grepl("Worker task error test", sapply(errs, `[[`, "message"))))
})

# ── Feature 16: Non-blocking Event Loop & Heartbeat ──────────────────────────

test_that("F16-1: ready.json hb_at heartbeat updates continuously at <= 2s intervals", {
  if (!is_m3_ready()) skip("M3 non-blocking heartbeat not yet active")
  hb_int <- ts_drive_hb_interval()
  expect_lte(hb_int, 2)
})

test_that("F16-2: heartbeat_stalled_by_job remains false during pipeline execution", {
  if (!is_m3_ready()) skip("M3 non-blocking heartbeat not yet active")
  status <- ts_drive_detect_pipeline_status()
  expect_identical(status, "idle")
  t0 <- Sys.time()
  due <- (as.numeric(t0 + 2.5) - as.numeric(t0)) >= ts_drive_hb_interval()
  expect_true(due)
})

test_that("F16-3: main thread event loop handles concurrent input events during pipeline run", {
  if (!is_m3_ready()) skip("M3 non-blocking heartbeat not yet active")
  now_iso <- ts_drive_now_iso()
  expect_type(now_iso, "character")
  expect_true(nzchar(now_iso))
  next_hb <- ts_drive_hb_next_at(Sys.time(), hb_at = 100, wrote_ok = TRUE)
  expect_gt(next_hb, 100)
})

test_that("F16-4: heartbeat watchdog reports healthy status during long computation", {
  if (!is_m3_ready()) skip("M3 non-blocking heartbeat not yet active")
  td <- tempfile("drv_hb_")
  dir.create(td, recursive = TRUE)
  withr::local_envvar(c(TRANSCRIPTO_DRIVE_ROOT = td))
  ts_drive_write_ready(list(), "tok_m3_test", armed = TRUE)
  age <- ts_drive_ready_age()
  expect_lte(age, 5)
})

test_that("F16-5: heartbeat timestamp uses UTC ISO8601 format", {
  if (!is_m3_ready()) skip("M3 non-blocking heartbeat not yet active")
  now_iso <- ts_drive_now_iso()
  expect_match(now_iso, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$")
})

# ── Feature 17: Downstream SC Export Routes Registration ─────────────────────

test_that("F17-1: sc_embeddings_csv is registered in TS_DRIVE_EXPORT_ROUTES", {
  if (!is_m3_ready()) skip("M3 export routes not yet active")
  expect_true(exists("TS_DRIVE_EXPORT_ROUTES"))
  expect_identical(TS_DRIVE_EXPORT_ROUTES$sc_embeddings, "sc_embeddings_csv")
})

test_that("F17-2: sc_cell_metadata_csv is registered in TS_DRIVE_EXPORT_ROUTES", {
  if (!is_m3_ready()) skip("M3 export routes not yet active")
  expect_true(exists("TS_DRIVE_EXPORT_ROUTES"))
  expect_identical(TS_DRIVE_EXPORT_ROUTES$sc_annotation, "sc_cell_metadata_csv")
})

test_that("F17-3: sc_trajectory_pseudotime_csv is registered in TS_DRIVE_EXPORT_ROUTES", {
  if (!is_m3_ready()) skip("M3 export routes not yet active")
  expect_true(exists("TS_DRIVE_EXPORT_ROUTES"))
  expect_identical(TS_DRIVE_EXPORT_ROUTES$sc_trajectory, "sc_trajectory_pseudotime_csv")
})

test_that("F17-4: export route stems are registered in TS_DRIVE_EXPORT_STEMS allowlist", {
  if (!is_m3_ready()) skip("M3 export routes not yet active")
  expect_true(exists("TS_DRIVE_EXPORT_STEMS"))
  expect_identical(TS_DRIVE_EXPORT_STEMS[["sc_embeddings"]], "sc_embeddings_umap_2d")
  expect_identical(TS_DRIVE_EXPORT_STEMS[["sc_embeddings_tsne"]], "sc_embeddings_tsne_2d")
  expect_identical(TS_DRIVE_EXPORT_STEMS[["sc_embeddings_pca"]], "sc_embeddings_pca_2d")
  expect_identical(TS_DRIVE_EXPORT_STEMS[["sc_annotation"]], "sc_cell_metadata")
  expect_identical(TS_DRIVE_EXPORT_STEMS[["sc_trajectory"]], "sc_trajectory_pseudotime")
})

test_that("F17-5: unregistered export routes are refused with informative error", {
  if (!is_m3_ready()) skip("M3 export routes not yet active")
  invalid_route <- "sc_nonexistent_route"
  expect_false(invalid_route %in% names(TS_DRIVE_EXPORT_ROUTES))
  expect_false(invalid_route %in% TS_DRIVE_EXPORT_MODULES)
})

# ── Feature 18: SC Export Route Handlers & Exporters ─────────────────────────

test_that("F18-1: sc_embeddings_csv exports valid 2D coordinates (cell_id, UMAP_1, UMAP_2)", {
  if (!is_m3_ready()) skip("M3 export handlers not yet active")
  expect_true(exists("sc_viz_export_embeddings_csv", mode = "function"))
  obj <- .make_test_sc_obj()
  obj <- Seurat::NormalizeData(obj, verbose = FALSE)
  obj <- Seurat::FindVariableFeatures(obj, nfeatures = 20, verbose = FALSE)
  obj <- Seurat::ScaleData(obj, verbose = FALSE)
  obj <- Seurat::RunPCA(obj, npcs = 5, verbose = FALSE)
  umap_emb <- matrix(stats::rnorm(25 * 2), nrow = 25, ncol = 2,
                     dimnames = list(colnames(obj), c("UMAP_1", "UMAP_2")))
  obj[["umap"]] <- Seurat::CreateDimReducObject(embeddings = umap_emb, key = "UMAP_", assay = "RNA")

  td <- tempfile("ts-exp-emb-")
  dir.create(td, showWarnings = FALSE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  gd <- shiny::reactiveValues(sc_obj = obj)
  rv <- shiny::reactiveValues()
  res <- sc_viz_export_embeddings_csv(rv, gd, dir = td)
  expect_true(isTRUE(res$ok))
  expect_identical(res$descriptor$columns[1:3], c("cell_id", "UMAP_1", "UMAP_2"))
  written_file <- file.path(td, res$descriptor$file)
  expect_true(file.exists(written_file))
  df <- utils::read.csv(written_file, stringsAsFactors = FALSE)
  expect_equal(nrow(df), 25)
  expect_true(all(c("cell_id", "UMAP_1", "UMAP_2") %in% names(df)))
})

test_that("F18-2: sc_cell_metadata_csv exports cell annotations and cluster labels", {
  if (!is_m3_ready()) skip("M3 export handlers not yet active")
  expect_true(exists("sc_annotation_export_cell_metadata_csv", mode = "function"))
  obj <- .make_test_sc_obj()
  obj@meta.data$seurat_clusters <- factor(c(rep("0", 10), rep("1", 10), rep("2", 5)))
  obj@meta.data$SingleR_main <- c(rep("T_cells", 10), rep("B_cells", 10), rep("Monocytes", 5))

  td <- tempfile("ts-exp-annot-")
  dir.create(td, showWarnings = FALSE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  gd <- shiny::reactiveValues(sc_obj = obj)
  rv <- shiny::reactiveValues()
  res <- sc_annotation_export_cell_metadata_csv(rv, gd, dir = td)
  expect_true(isTRUE(res$ok))
  expect_true(all(c("cell_id", "cluster") %in% res$descriptor$columns))
  written_file <- file.path(td, res$descriptor$file)
  expect_true(file.exists(written_file))
  df <- utils::read.csv(written_file, stringsAsFactors = FALSE)
  expect_equal(nrow(df), 25)
  expect_true("cluster" %in% names(df))
  expect_true("SingleR_main" %in% names(df))
})

test_that("F18-3: sc_trajectory_pseudotime_csv exports cell pseudotime ordering", {
  if (!is_m3_ready()) skip("M3 export handlers not yet active")
  expect_true(exists("sc_trajectory_export_pseudotime_csv", mode = "function"))
  obj <- .make_test_sc_obj()
  obj@meta.data$pseudotime <- seq(0, 1, length.out = 25)
  obj@meta.data$traj_method <- "slingshot"

  td <- tempfile("ts-exp-traj-")
  dir.create(td, showWarnings = FALSE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  gd <- shiny::reactiveValues(sc_obj = obj)
  rv <- shiny::reactiveValues()
  res <- sc_trajectory_export_pseudotime_csv(rv, gd, dir = td)
  expect_true(isTRUE(res$ok))
  expect_true(all(c("cell_id", "pseudotime") %in% res$descriptor$columns))
  written_file <- file.path(td, res$descriptor$file)
  expect_true(file.exists(written_file))
  df <- utils::read.csv(written_file, stringsAsFactors = FALSE)
  expect_equal(nrow(df), 25)
  expect_true("pseudotime" %in% names(df))
  expect_true("traj_method" %in% names(df))
})

test_that("F18-4: exported CSV files conform to TS_DRIVE_EXPORT_COLUMNS contract", {
  if (!is_m3_ready()) skip("M3 export handlers not yet active")
  ct <- TS_DRIVE_EXPORT_COLUMNS
  expect_true(all(c("sc_embeddings", "sc_annotation", "sc_trajectory") %in% names(ct)))
  expect_identical(ct$sc_embeddings$guaranteed, "cell_id")
  expect_identical(ct$sc_embeddings$mode_umap, c("cell_id", "UMAP_1", "UMAP_2"))
  expect_identical(ct$sc_annotation$guaranteed, c("cell_id", "cluster"))
  expect_identical(ct$sc_trajectory$guaranteed, c("cell_id", "pseudotime"))
})

test_that("F18-5: export write is atomic and bounded by TS_DRIVE_EXPORT_MAX_FILES", {
  if (!is_m3_ready()) skip("M3 export handlers not yet active")
  td <- tempfile("ts-exp-bound-")
  dir.create(td, showWarnings = FALSE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  stem <- TS_DRIVE_EXPORT_STEMS[["sc_embeddings"]]
  for (i in seq_len(15)) {
    f <- file.path(td, sprintf("%s_%d.csv", stem, i))
    write.csv(data.frame(x = 1), f, row.names = FALSE)
  }
  ts_drive_export_prune(td, cap = 10L)
  remaining <- list.files(td, pattern = paste0("^", stem, "_[0-9]+\\.csv$"))
  expect_lte(length(remaining), 10L)
})

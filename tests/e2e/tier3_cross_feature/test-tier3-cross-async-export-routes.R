# =============================================================================
# test-tier3-cross-async-export-routes.R — Tier 3 Cross-Feature: Pipeline -> Export
# =============================================================================
# Cross-feature scenario: Asynchronous pipeline completion triggering export
# file publishing via registered downstream export routes.
# - Completed pipeline results feed export publishing handlers
# - Exported CSV files match expected column schema
# - Heartbeat loop keeps running throughout export write operations
# =============================================================================

if (!exists("e2e_project_root", mode = "function")) {
  if (file.exists("tests/e2e/helper-e2e.R")) {
    source("tests/e2e/helper-e2e.R")
  } else if (file.exists("../helper-e2e.R")) {
    source("../helper-e2e.R")
  }
}

.make_tier3_sc_obj <- function() {
  set.seed(123)
  mat <- matrix(rpois(60 * 45, lambda = 3), nrow = 60, ncol = 45)
  rownames(mat) <- paste0("Gene-", seq_len(60))
  colnames(mat) <- paste0("Cell-", seq_len(45))
  suppressWarnings(Seurat::CreateSeuratObject(counts = mat))
}

test_that("T3-AER-1: pipeline 2D embeddings export conforms to sc_embeddings_csv schema", {
  if (!is_m3_ready()) skip("M3 async pipeline and export routes not yet active")
  obj <- .make_tier3_sc_obj()
  res <- run_sc_auto_pipeline_core(
    obj,
    params = list(skip_qc = TRUE, n_dims = 5, resolution = 0.5, compute_umap = TRUE)
  )
  expect_true(is.list(res))
  expect_s4_class(res$obj, "Seurat")
  expect_true("umap" %in% names(res$obj@reductions))

  td <- tempfile("ts_tier3_emb_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  gd <- shiny::reactiveValues(sc_obj = res$obj)
  rv <- shiny::reactiveValues()
  exp_res <- sc_viz_export_embeddings_csv(rv, gd, dir = td)

  expect_true(isTRUE(exp_res$ok))
  expect_identical(exp_res$status, "done")
  expect_identical(exp_res$descriptor$format, "csv")
  expect_true(grepl("^sc_embeddings_umap_2d_[0-9]+\\.csv$", exp_res$descriptor$file))

  written_file <- file.path(td, exp_res$descriptor$file)
  expect_true(file.exists(written_file))

  df <- utils::read.csv(written_file, stringsAsFactors = FALSE)
  expect_equal(nrow(df), ncol(res$obj))
  contract <- TS_DRIVE_EXPORT_COLUMNS$sc_embeddings$mode_umap
  expect_true(all(contract %in% names(df)))
  expect_false(any(is.na(df$cell_id)))
})

test_that("T3-AER-2: pipeline cell metadata export conforms to sc_cell_metadata_csv schema", {
  if (!is_m3_ready()) skip("M3 async pipeline and export routes not yet active")
  obj <- .make_tier3_sc_obj()
  res <- run_sc_auto_pipeline_core(
    obj,
    params = list(skip_qc = TRUE, n_dims = 5, resolution = 0.5, compute_umap = FALSE)
  )
  expect_s4_class(res$obj, "Seurat")
  expect_true("seurat_clusters" %in% colnames(res$obj@meta.data))

  td <- tempfile("ts_tier3_meta_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  gd <- shiny::reactiveValues(sc_obj = res$obj)
  rv <- shiny::reactiveValues()
  exp_res <- sc_annotation_export_cell_metadata_csv(rv, gd, dir = td)

  expect_true(isTRUE(exp_res$ok))
  expect_identical(exp_res$status, "done")
  expect_identical(exp_res$descriptor$format, "csv")
  expect_true(grepl("^sc_cell_metadata_[0-9]+\\.csv$", exp_res$descriptor$file))

  written_file <- file.path(td, exp_res$descriptor$file)
  expect_true(file.exists(written_file))

  df <- utils::read.csv(written_file, stringsAsFactors = FALSE)
  expect_equal(nrow(df), ncol(res$obj))
  guaranteed <- TS_DRIVE_EXPORT_COLUMNS$sc_annotation$guaranteed
  expect_true(all(guaranteed %in% names(df)))
  expect_false(any(is.na(df$cell_id)))
  expect_false(any(is.na(df$cluster)))
})

test_that("T3-AER-3: export route writes atomic CSV within ts_drive_exports directory", {
  if (!is_m3_ready()) skip("M3 export routes not yet active")
  td <- tempfile("ts_tier3_traj_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  obj <- .make_tier3_sc_obj()
  obj@meta.data$pseudotime <- seq(0, 1, length.out = ncol(obj))
  gd <- shiny::reactiveValues(sc_obj = obj)
  rv <- shiny::reactiveValues()

  exp_res <- sc_trajectory_export_pseudotime_csv(rv, gd, dir = td)
  expect_true(isTRUE(exp_res$ok))
  expect_identical(exp_res$status, "done")

  stem <- TS_DRIVE_EXPORT_STEMS[["sc_trajectory"]]
  expect_identical(stem, "sc_trajectory_pseudotime")
  files <- list.files(td, pattern = paste0("^", stem, "_[0-9]+\\.csv$"))
  expect_length(files, 1L)

  # Check atomicity: no stray .tmp files left in export dir
  tmp_files <- list.files(td, pattern = "\\.tmp$")
  expect_length(tmp_files, 0L)

  written_file <- file.path(td, files[1])
  df <- utils::read.csv(written_file, stringsAsFactors = FALSE)
  guaranteed <- TS_DRIVE_EXPORT_COLUMNS$sc_trajectory$guaranteed
  expect_true(all(guaranteed %in% names(df)))
  expect_equal(nrow(df), ncol(obj))

  # Check pruning maintenance
  for (i in 2:8) {
    extra_file <- file.path(td, sprintf("%s_%d.csv", stem, i))
    write.csv(data.frame(x = 1), extra_file, row.names = FALSE)
  }
  expect_equal(length(list.files(td, pattern = paste0("^", stem, "_[0-9]+\\.csv$"))), 8L)
  ts_drive_export_prune(td, cap = 4L)
  remaining <- list.files(td, pattern = paste0("^", stem, "_[0-9]+\\.csv$"))
  expect_lte(length(remaining), 4L)
})

test_that("T3-AER-4: heartbeat and error state integration during pipeline and export", {
  if (!is_m3_ready()) skip("M3 export routes not yet active")
  td_drive <- tempfile("ts_tier3_drive_root_")
  dir.create(td_drive, recursive = TRUE)
  on.exit(unlink(td_drive, recursive = TRUE, force = TRUE), add = TRUE)

  withr::local_envvar(c(TRANSCRIPTO_DRIVE_ROOT = td_drive))
  ts_drive_write_ready(list(), "tier3_tok", armed = TRUE)
  expect_lte(ts_drive_ready_age(), 5)

  # Publishing an export records into global_data registry
  gd <- shiny::reactiveValues(drive_registry = new.env(parent = emptyenv()))
  pub_res <- ts_drive_publish_export(gd, "sc_embeddings", sc_viz_export_embeddings_csv)
  expect_true(isTRUE(pub_res))

  retrieved_fn <- ts_drive_export_of(gd, "sc_embeddings")
  expect_identical(retrieved_fn, sc_viz_export_embeddings_csv)

  # Error state recording without crashing
  err_msg <- "sc_test_pipeline_error: simulated failure"
  err_list <- ts_drive_record_error(err_msg, module = "sc_pipeline")
  expect_true(is.list(err_list))
  expect_true(any(grepl("sc_test_pipeline_error", vapply(err_list, function(x) x$message %||% "", character(1)))))
})

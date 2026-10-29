# =============================================================================
# test-challenge-m3-export-routes.R — Empirical Adversarial Stress Test Suite
# Milestone M3: Single-Cell Downstream Export Routes (Features 17 & 18)
# =============================================================================

if (!exists("e2e_project_root", mode = "function")) {
  cand <- c("tests/e2e/helper-e2e.R", "../e2e/helper-e2e.R", "../../tests/e2e/helper-e2e.R")
  for (f in cand) {
    if (file.exists(f)) {
      source(f)
      break
    }
  }
}

.make_base_sc_obj <- function(n_genes = 40, n_cells = 30) {
  set.seed(42)
  mat <- matrix(rpois(n_genes * n_cells, lambda = 3), nrow = n_genes, ncol = n_cells)
  rownames(mat) <- paste0("Gene-", seq_len(n_genes))
  colnames(mat) <- paste0("Cell-", seq_len(n_cells))
  suppressWarnings(Seurat::CreateSeuratObject(counts = mat))
}

.add_dummy_dimred <- function(obj, name, col_prefix, n_dim = 2) {
  n_cells <- ncol(obj)
  emb <- matrix(rnorm(n_cells * n_dim), nrow = n_cells, ncol = n_dim)
  rownames(emb) <- colnames(obj)
  colnames(emb) <- paste0(col_prefix, "_", seq_len(n_dim))
  dr <- suppressWarnings(Seurat::CreateDimReducObject(
    embeddings = emb,
    key = paste0(col_prefix, "_"),
    assay = Seurat::DefaultAssay(obj)
  ))
  obj[[name]] <- dr
  obj
}

# =============================================================================
# 1. Incomplete / Edge-Case Objects: Reductions & Fallbacks
# =============================================================================

test_that("ADV-1.1: sc_viz_export_embeddings_csv handles object with NO reductions gracefully", {
  obj <- .make_base_sc_obj()
  expect_length(names(obj@reductions), 0L)

  td <- tempfile("ts_adv_no_red_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  gd <- shiny::reactiveValues(sc_obj = obj)
  rv <- shiny::reactiveValues()

  res <- sc_viz_export_embeddings_csv(rv, gd, dir = td)
  expect_false(isTRUE(res$ok))
  expect_identical(res$status, "invalid")
  expect_true(grepl("no 2D dimension reductions", res$errors))
  expect_null(res$descriptor)

  # Verify no file written on invalid status
  expect_length(list.files(td), 0L)
})

test_that("ADV-1.2: sc_viz_export_embeddings_csv falls back to PCA stem and mode_pca columns when PCA only", {
  obj <- .make_base_sc_obj()
  obj <- .add_dummy_dimred(obj, name = "pca", col_prefix = "PC", n_dim = 2)
  expect_identical(names(obj@reductions), "pca")

  td <- tempfile("ts_adv_pca_only_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  gd <- shiny::reactiveValues(sc_obj = obj)
  rv <- shiny::reactiveValues()

  res <- sc_viz_export_embeddings_csv(rv, gd, dir = td)
  expect_true(isTRUE(res$ok))
  expect_identical(res$status, "done")
  expect_true(grepl("^sc_embeddings_pca_2d_[0-9]+\\.csv$", res$descriptor$file))

  written <- file.path(td, res$descriptor$file)
  expect_true(file.exists(written))

  df <- utils::read.csv(written, stringsAsFactors = FALSE)
  expect_equal(nrow(df), ncol(obj))

  expected_pca_cols <- TS_DRIVE_EXPORT_COLUMNS$sc_embeddings$mode_pca
  expect_identical(expected_pca_cols, c("cell_id", "PC_1", "PC_2"))
  expect_true(all(expected_pca_cols %in% names(df)))
  expect_false(any(is.na(df$cell_id)))
  expect_true(is.numeric(df$PC_1))
  expect_true(is.numeric(df$PC_2))
})

test_that("ADV-1.3: sc_viz_export_embeddings_csv falls back to t-SNE stem and mode_tsne columns when t-SNE only", {
  obj <- .make_base_sc_obj()
  obj <- .add_dummy_dimred(obj, name = "tsne", col_prefix = "tSNE", n_dim = 2)
  expect_identical(names(obj@reductions), "tsne")

  td <- tempfile("ts_adv_tsne_only_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  gd <- shiny::reactiveValues(sc_obj = obj)
  rv <- shiny::reactiveValues()

  res <- sc_viz_export_embeddings_csv(rv, gd, dir = td)
  expect_true(isTRUE(res$ok))
  expect_identical(res$status, "done")
  expect_true(grepl("^sc_embeddings_tsne_2d_[0-9]+\\.csv$", res$descriptor$file))

  written <- file.path(td, res$descriptor$file)
  expect_true(file.exists(written))

  df <- utils::read.csv(written, stringsAsFactors = FALSE)
  expect_equal(nrow(df), ncol(obj))

  expected_tsne_cols <- TS_DRIVE_EXPORT_COLUMNS$sc_embeddings$mode_tsne
  expect_identical(expected_tsne_cols, c("cell_id", "tSNE_1", "tSNE_2"))
  expect_true(all(expected_tsne_cols %in% names(df)))
  expect_false(any(is.na(df$cell_id)))
  expect_true(is.numeric(df$tSNE_1))
  expect_true(is.numeric(df$tSNE_2))
})

test_that("ADV-1.4: sc_viz_export_embeddings_csv prioritizes UMAP over t-SNE and PCA when multiple reductions present", {
  obj <- .make_base_sc_obj()
  obj <- .add_dummy_dimred(obj, name = "pca", col_prefix = "PC", n_dim = 2)
  obj <- .add_dummy_dimred(obj, name = "tsne", col_prefix = "tSNE", n_dim = 2)
  obj <- .add_dummy_dimred(obj, name = "umap", col_prefix = "UMAP", n_dim = 2)

  td <- tempfile("ts_adv_umap_prec_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  gd <- shiny::reactiveValues(sc_obj = obj)
  rv <- shiny::reactiveValues()

  res <- sc_viz_export_embeddings_csv(rv, gd, dir = td)
  expect_true(isTRUE(res$ok))
  expect_identical(res$status, "done")
  expect_true(grepl("^sc_embeddings_umap_2d_[0-9]+\\.csv$", res$descriptor$file))

  written <- file.path(td, res$descriptor$file)
  df <- utils::read.csv(written, stringsAsFactors = FALSE)
  expected_umap_cols <- TS_DRIVE_EXPORT_COLUMNS$sc_embeddings$mode_umap
  expect_identical(expected_umap_cols, c("cell_id", "UMAP_1", "UMAP_2"))
  expect_true(all(expected_umap_cols %in% names(df)))
})

test_that("ADV-1.5: sc_viz_export_embeddings_csv refuses reduction with <2 dimensions without crash", {
  obj <- .make_base_sc_obj()
  # 1D PCA
  obj <- .add_dummy_dimred(obj, name = "pca", col_prefix = "PC", n_dim = 1)

  td <- tempfile("ts_adv_1d_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  gd <- shiny::reactiveValues(sc_obj = obj)
  rv <- shiny::reactiveValues()

  res <- sc_viz_export_embeddings_csv(rv, gd, dir = td)
  expect_false(isTRUE(res$ok))
  expect_identical(res$status, "invalid")
  expect_true(grepl("fewer than 2 dimensions", res$errors))
  expect_null(res$descriptor)
})

# =============================================================================
# 2. Metadata & Clustering Edge Cases
# =============================================================================

test_that("ADV-2.1: sc_annotation_export_cell_metadata_csv handles object with NO clustering / metadata gracefully", {
  obj <- .make_base_sc_obj()
  # Strip any cluster columns from metadata
  obj@meta.data$seurat_clusters <- NULL
  obj@meta.data$cluster <- NULL
  obj@meta.data$active_ident <- NULL

  td <- tempfile("ts_adv_no_clust_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  gd <- shiny::reactiveValues(sc_obj = obj)
  rv <- shiny::reactiveValues()

  res <- sc_annotation_export_cell_metadata_csv(rv, gd, dir = td)
  expect_true(isTRUE(res$ok))
  expect_identical(res$status, "done")

  written <- file.path(td, res$descriptor$file)
  expect_true(file.exists(written))

  df <- utils::read.csv(written, stringsAsFactors = FALSE)
  guaranteed <- TS_DRIVE_EXPORT_COLUMNS$sc_annotation$guaranteed
  expect_identical(guaranteed, c("cell_id", "cluster"))
  expect_true(all(guaranteed %in% names(df)))
  expect_equal(nrow(df), ncol(obj))
  expect_false(any(is.na(df$cluster)))
  expect_false(any(is.na(df$cell_id)))
})

test_that("ADV-2.2: sc_viz_export_embeddings_csv handles object with missing seurat_clusters gracefully", {
  obj <- .make_base_sc_obj()
  obj <- .add_dummy_dimred(obj, name = "umap", col_prefix = "UMAP", n_dim = 2)
  obj@meta.data$seurat_clusters <- NULL

  td <- tempfile("ts_adv_viz_no_clust_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  gd <- shiny::reactiveValues(sc_obj = obj)
  rv <- shiny::reactiveValues()

  res <- sc_viz_export_embeddings_csv(rv, gd, dir = td)
  expect_true(isTRUE(res$ok))
  expect_identical(res$status, "done")

  df <- utils::read.csv(file.path(td, res$descriptor$file), stringsAsFactors = FALSE)
  expect_true(all(c("cell_id", "UMAP_1", "UMAP_2") %in% names(df)))
  # 'cluster' column is optional in sc_embeddings contract
  expect_equal(nrow(df), ncol(obj))
})

test_that("ADV-2.3: sc_annotation_export_cell_metadata_csv carries annotations and QC metrics when present", {
  obj <- .make_base_sc_obj()
  obj@meta.data$seurat_clusters <- factor(rep(c(0, 1, 2), length.out = ncol(obj)))
  obj@meta.data$SingleR_labels <- rep("T_cell", ncol(obj))
  obj@meta.data$cell_type <- rep("Immune", ncol(obj))
  obj@meta.data$percent.mt <- rep(2.5, ncol(obj))

  td <- tempfile("ts_adv_meta_rich_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  gd <- shiny::reactiveValues(sc_obj = obj)
  rv <- shiny::reactiveValues()

  res <- sc_annotation_export_cell_metadata_csv(rv, gd, dir = td)
  expect_true(isTRUE(res$ok))

  df <- utils::read.csv(file.path(td, res$descriptor$file), stringsAsFactors = FALSE)
  expect_true(all(c("cell_id", "cluster", "SingleR_labels", "cell_type", "percent.mt") %in% names(df)))
  expect_equal(nrow(df), ncol(obj))
})

# =============================================================================
# 3. Trajectory Pseudotime Edge Cases
# =============================================================================

test_that("ADV-3.1: sc_trajectory_export_pseudotime_csv returns status='invalid' on missing trajectory metadata", {
  obj <- .make_base_sc_obj()
  # Ensure no trajectory columns
  expect_false("pseudotime" %in% colnames(obj@meta.data))

  td <- tempfile("ts_adv_no_traj_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  gd <- shiny::reactiveValues(sc_obj = obj)
  rv <- shiny::reactiveValues()

  res <- sc_trajectory_export_pseudotime_csv(rv, gd, dir = td)
  expect_false(isTRUE(res$ok))
  expect_identical(res$status, "invalid")
  expect_true(grepl("no trajectory or pseudotime has been computed", res$errors))
  expect_null(res$descriptor)
  expect_length(list.files(td), 0L)
})

test_that("ADV-3.2: sc_trajectory_export_pseudotime_csv returns status='invalid' when pseudotime is all NA", {
  obj <- .make_base_sc_obj()
  obj@meta.data$pseudotime <- as.numeric(rep(NA, ncol(obj)))

  td <- tempfile("ts_adv_traj_all_na_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  gd <- shiny::reactiveValues(sc_obj = obj)
  rv <- shiny::reactiveValues()

  res <- sc_trajectory_export_pseudotime_csv(rv, gd, dir = td)
  expect_false(isTRUE(res$ok))
  expect_identical(res$status, "invalid")
  expect_true(grepl("stored pseudotime column contains no numeric values", res$errors))
  expect_null(res$descriptor)
})

test_that("ADV-3.3: sc_trajectory_export_pseudotime_csv returns status='invalid' when pseudotime is non-numeric string", {
  obj <- .make_base_sc_obj()
  obj@meta.data$pseudotime <- rep("undefined", ncol(obj))

  td <- tempfile("ts_adv_traj_non_num_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  gd <- shiny::reactiveValues(sc_obj = obj)
  rv <- shiny::reactiveValues()

  res <- sc_trajectory_export_pseudotime_csv(rv, gd, dir = td)
  expect_false(isTRUE(res$ok))
  expect_identical(res$status, "invalid")
  expect_true(grepl("stored pseudotime column contains no numeric values", res$errors))
  expect_null(res$descriptor)
})

test_that("ADV-3.4: sc_trajectory_export_pseudotime_csv exports valid pseudotime with partial NAs and auxiliary fields", {
  obj <- .make_base_sc_obj()
  pt <- seq(0, 1, length.out = ncol(obj))
  pt[c(1, 5)] <- NA # simulated unassigned cells
  obj@meta.data$pseudotime <- pt
  obj@meta.data$pseudotime_lineage2 <- pt * 1.5
  obj@meta.data$traj_method <- "slingshot"
  obj@meta.data$traj_root_cluster <- "0"

  td <- tempfile("ts_adv_traj_valid_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  gd <- shiny::reactiveValues(sc_obj = obj)
  rv <- shiny::reactiveValues()

  res <- sc_trajectory_export_pseudotime_csv(rv, gd, dir = td)
  expect_true(isTRUE(res$ok))
  expect_identical(res$status, "done")

  df <- utils::read.csv(file.path(td, res$descriptor$file), stringsAsFactors = FALSE)
  guaranteed <- TS_DRIVE_EXPORT_COLUMNS$sc_trajectory$guaranteed
  expect_identical(guaranteed, c("cell_id", "pseudotime"))
  expect_true(all(guaranteed %in% names(df)))
  expect_true("pseudotime_lineage2" %in% names(df))
  expect_true("traj_method" %in% names(df))
  expect_true("traj_root_cluster" %in% names(df))
  expect_equal(nrow(df), ncol(obj))
})

# =============================================================================
# 4. Malformed Objects & Session Disconnection
# =============================================================================

test_that("ADV-4.1: Exporters handle NULL and disconnected objects without uncaught exceptions", {
  td <- tempfile("ts_adv_null_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  test_cases <- list(
    list(gd = shiny::reactiveValues(sc_obj = NULL), rv = shiny::reactiveValues()),
    list(gd = shiny::reactiveValues(), rv = shiny::reactiveValues()),
    list(gd = list(), rv = list()),
    list(gd = NULL, rv = NULL)
  )

  for (tc in test_cases) {
    res1 <- tryCatch(sc_viz_export_embeddings_csv(tc$rv, tc$gd, dir = td), error = function(e) e)
    expect_false(inherits(res1, "error"))
    expect_identical(res1$status, "invalid")

    res2 <- tryCatch(sc_annotation_export_cell_metadata_csv(tc$rv, tc$gd, dir = td), error = function(e) e)
    expect_false(inherits(res2, "error"))
    expect_identical(res2$status, "invalid")

    res3 <- tryCatch(sc_trajectory_export_pseudotime_csv(tc$rv, tc$gd, dir = td), error = function(e) e)
    expect_false(inherits(res3, "error"))
    expect_identical(res3$status, "invalid")
  }
})

test_that("ADV-4.2: Exporters reject non-Seurat objects with status='invalid' without uncaught exceptions", {
  td <- tempfile("ts_adv_non_seurat_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  bad_objects <- list(
    matrix(1:10, 2, 5),
    data.frame(a = 1:5),
    "not_an_object",
    12345L,
    list(a = 1, b = 2)
  )

  for (bad_obj in bad_objects) {
    gd <- shiny::reactiveValues(sc_obj = bad_obj)
    rv <- shiny::reactiveValues()

    res1 <- tryCatch(sc_viz_export_embeddings_csv(rv, gd, dir = td), error = function(e) e)
    expect_false(inherits(res1, "error"))
    expect_identical(res1$status, "invalid")

    res2 <- tryCatch(sc_annotation_export_cell_metadata_csv(rv, gd, dir = td), error = function(e) e)
    expect_false(inherits(res2, "error"))
    expect_identical(res2$status, "invalid")

    res3 <- tryCatch(sc_trajectory_export_pseudotime_csv(rv, gd, dir = td), error = function(e) e)
    expect_false(inherits(res3, "error"))
    expect_identical(res3$status, "invalid")
  }
})

# =============================================================================
# 5. Bounded Pruning & Multi-Stem Pruning Verification
# =============================================================================

test_that("ADV-5.1: Export directory is strictly bounded by TS_DRIVE_EXPORT_MAX_FILES = 8", {
  expect_identical(TS_DRIVE_EXPORT_MAX_FILES, 8L)

  td <- tempfile("ts_adv_prune_bounded_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  obj <- .make_base_sc_obj()
  obj <- .add_dummy_dimred(obj, name = "umap", col_prefix = "UMAP", n_dim = 2)
  obj@meta.data$seurat_clusters <- factor(rep(0, ncol(obj)))
  obj@meta.data$pseudotime <- seq(0, 1, length.out = ncol(obj))

  gd <- shiny::reactiveValues(sc_obj = obj)
  rv <- shiny::reactiveValues()

  # Trigger 15 exports mixed across the 3 SC exporters
  for (i in 1:5) {
    sc_viz_export_embeddings_csv(rv, gd, dir = td)
    sc_annotation_export_cell_metadata_csv(rv, gd, dir = td)
    sc_trajectory_export_pseudotime_csv(rv, gd, dir = td)
  }

  all_csvs <- list.files(td, pattern = "\\.csv$")
  # After 15 exports with cap = 8, exactly 8 files must remain
  expect_lte(length(all_csvs), 8L)
  expect_equal(length(all_csvs), 8L)
})

test_that("ADV-5.2: Pruner protects foreign/unrelated files while strictly cleaning declared stems", {
  td <- tempfile("ts_adv_foreign_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  # Create foreign files that MUST NOT be touched
  foreign1 <- file.path(td, "operator_analysis.csv")
  foreign2 <- file.path(td, "custom_annotation.txt")
  write.csv(data.frame(x = 1), foreign1, row.names = FALSE)
  writeLines("keep me", foreign2)

  # Create 12 files matching SC export stems
  for (i in 1:6) {
    f_umap <- file.path(td, sprintf("sc_embeddings_umap_2d_%d.csv", i))
    write.csv(data.frame(a = 1), f_umap, row.names = FALSE)
    Sys.sleep(0.01) # ensure distinct mtime
    f_meta <- file.path(td, sprintf("sc_cell_metadata_%d.csv", i))
    write.csv(data.frame(b = 1), f_meta, row.names = FALSE)
    Sys.sleep(0.01)
  }

  # Total files before prune: 2 foreign + 12 = 14
  expect_equal(length(list.files(td)), 14L)

  # Run pruner with default cap (8)
  pruned_count <- ts_drive_export_prune(td, cap = 8L)
  expect_gt(pruned_count, 0L)

  # Foreign files MUST survive
  expect_true(file.exists(foreign1))
  expect_true(file.exists(foreign2))

  # Total remaining files must be <= cap + foreign files
  remaining <- list.files(td)
  expect_lte(length(remaining), 10L) # 8 cap + 2 foreign
})

test_that("ADV-5.3: Next path generator increments monotonically without filename collision", {
  td <- tempfile("ts_adv_monotonic_")
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

  stem <- "sc_embeddings_pca_2d"
  p1 <- sc_viz_export_next_path(td, stem = stem)
  expect_identical(basename(p1), paste0(stem, "_1.csv"))
  write.csv(data.frame(x = 1), p1, row.names = FALSE)

  p2 <- sc_viz_export_next_path(td, stem = stem)
  expect_identical(basename(p2), paste0(stem, "_2.csv"))
  write.csv(data.frame(x = 1), p2, row.names = FALSE)

  # Even if _1 is pruned, _3 is generated next
  unlink(p1)
  p3 <- sc_viz_export_next_path(td, stem = stem)
  expect_identical(basename(p3), paste0(stem, "_3.csv"))
})

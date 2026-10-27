# =============================================================================
# helper-e2e.R — Shared Fixtures and Utilities for E2E Test Suite (Cerberus)
# =============================================================================
# Provides project root discovery, hermetic environment bootstrap, drop
# generators, CLI subprocess runners, and progressive milestone capability
# detection for Tiers 1-4 opaque-box end-to-end tests.
# =============================================================================

# ── Project Root & Source Bootstrap ──────────────────────────────────────────

e2e_find_project_root <- function(start = getwd(), marker = "app.R", max_up = 8L) {
  d <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in seq_len(max_up)) {
    if (file.exists(file.path(d, marker))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  stop("Could not locate project root from '", start, "'", call. = FALSE)
}

e2e_project_root <- local({
  root <- NULL
  function() {
    if (is.null(root)) root <<- e2e_find_project_root()
    root
  }
})

e2e_source_project_file <- function(relpath) {
  full <- file.path(e2e_project_root(), relpath)
  if (!file.exists(full)) {
    stop("Project file not found: ", full, call. = FALSE)
  }
  sys.source(full, envir = globalenv())
  invisible(full)
}

e2e_source_if_exists <- function(relpath) {
  full <- file.path(e2e_project_root(), relpath)
  if (file.exists(full)) {
    sys.source(full, envir = globalenv())
    return(TRUE)
  }
  FALSE
}

if (requireNamespace("Seurat", quietly = TRUE)) {
  suppressPackageStartupMessages(library(Seurat))
}

# Source core runtime primitives
e2e_source_if_exists("tools/pkg_skeleton/R/defaults.R")
e2e_source_if_exists("tools/pkg_skeleton/R/thresholds.R")
e2e_source_if_exists("tools/pkg_skeleton/R/error_state.R")
e2e_source_if_exists("tools/pkg_skeleton/R/error_log.R")
e2e_source_if_exists("tools/pkg_skeleton/R/io_helpers.R")
e2e_source_if_exists("tools/pkg_skeleton/R/validation.R")
e2e_source_if_exists("tools/pkg_skeleton/R/state.R")
if (file.exists(file.path(e2e_project_root(), "tools/pkg_skeleton/R/batch_correction.R"))) {
  e2e_source_if_exists("tools/pkg_skeleton/R/batch_correction.R")
} else {
  e2e_source_if_exists("R/bulk/batch_correction.R")
}
e2e_source_if_exists("tools/pkg_skeleton/R/drive_allowlist.R")
e2e_source_if_exists("tools/pkg_skeleton/R/drive_watcher.R")
e2e_source_if_exists("tools/pkg_skeleton/R/spatial_async.R")
e2e_source_if_exists("tools/pkg_skeleton/R/cerberus_handoff.R")
e2e_source_if_exists("tools/cerberus_handoff.R")

# Progressive milestone sources (source if available)
e2e_source_if_exists("tools/pkg_skeleton/R/sc_helpers.R")
e2e_source_if_exists("tools/pkg_skeleton/R/sc_bpcells.R")
e2e_source_if_exists("tools/pkg_skeleton/R/sc_batch_metrics.R")
if (file.exists(file.path(e2e_project_root(), "tools/pkg_skeleton/R/sc_pipeline.R"))) {
  e2e_source_if_exists("tools/pkg_skeleton/R/sc_pipeline.R")
} else {
  e2e_source_if_exists("R/sc/sc_pipeline.R")
}
e2e_source_if_exists("modules/sc/mod_sc_viz_export.R")
e2e_source_if_exists("modules/sc/mod_sc_annotation_export.R")
e2e_source_if_exists("modules/sc/mod_sc_trajectory_export.R")
# M4 (2026-10-26) : le moteur doublets/QC vit dans l'arbre paquet (migration
# D2 M5) — le legs R/sc/sc_qc_doublets.R n'existe plus sur l'arbre. Même
# pattern de repli que sc_pipeline.R ci-dessus (lignes 75-79) : paquet
# prioritaire, legs sinon. Sans ce fallback, is_m4_ready() resterait FALSE.
if (file.exists(file.path(e2e_project_root(), "tools/pkg_skeleton/R/sc_qc_doublets.R"))) {
  e2e_source_if_exists("tools/pkg_skeleton/R/sc_qc_doublets.R")
} else {
  e2e_source_if_exists("R/sc/sc_qc_doublets.R")
}

# Continuous heartbeat configuration for M3
options(ts.drive.hb_interval = 2)

# ── Milestone Readiness & Capability Detection ──────────────────────────────

is_m1_ready <- function() {
  file.exists(file.path(e2e_project_root(), "tools/pkg_skeleton/R/cerberus_handoff.R")) &&
  file.exists(file.path(e2e_project_root(), "tools/cerberus_handoff.R")) &&
  exists("cerberus_handoff_validate_drop", mode = "function")
}

is_m2_ready <- function() {
  # M2 requires TRANSCRIPTO_DRIVE_ROOT env var resolution in ts_drive_root()
  # and passive state / error ring buffer
  if (!exists("ts_drive_root", mode = "function")) return(FALSE)
  old_env <- Sys.getenv("TRANSCRIPTO_DRIVE_ROOT", unset = NA)
  on.exit({
    if (is.na(old_env)) Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT") else Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = old_env)
  }, add = TRUE)
  test_probe <- tempfile(pattern = "probe_drv_")
  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = test_probe)
  root_resolved <- tryCatch(
    identical(normalizePath(ts_drive_root(), winslash = "/", mustWork = FALSE),
              normalizePath(test_probe, winslash = "/", mustWork = FALSE)),
    error = function(e) FALSE
  )
  has_state_func <- exists("ts_drive_write_state", mode = "function") ||
                    exists("ts_drive_state_digest", mode = "function") ||
                    exists("ts_drive_error_ring_push", mode = "function")
  root_resolved && has_state_func
}

is_m3_ready <- function() {
  exists("run_sc_auto_pipeline_core", mode = "function")
}

is_m4_ready <- function() {
  exists("sc_filter_doublets_and_ambient", mode = "function")
}

# ── Rscript CLI Subprocess Runner ────────────────────────────────────────────

e2e_rscript_bin <- function() {
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  if (file.exists(rscript)) return(rscript)
  explicit_bin <- "D:/Data_science/R-4.4.2/bin/Rscript.exe"
  if (file.exists(explicit_bin)) return(explicit_bin)
  "Rscript"
}

e2e_run_cli <- function(args, cwd = e2e_project_root()) {
  bin <- e2e_rscript_bin()
  cli_script <- file.path(e2e_project_root(), "tools", "cerberus_handoff.R")
  full_args <- c("--vanilla", shQuote(cli_script))
  for (a in args) {
    if (grepl("=", a, fixed = TRUE)) {
      parts <- strsplit(a, "=", fixed = TRUE)[[1]]
      flag <- parts[1]
      val <- paste(parts[-1], collapse = "=")
      full_args <- c(full_args, paste0(flag, "=", shQuote(val)))
    } else {
      full_args <- c(full_args, shQuote(a))
    }
  }
  out <- suppressWarnings(
    system2(bin, args = full_args, stdout = TRUE, stderr = TRUE)
  )
  code <- attr(out, "status")
  if (is.null(code)) code <- 0L
  list(
    exit_code = as.integer(code),
    output = out,
    stdout_text = paste(out, collapse = "\n")
  )
}

# ── Test Drop Fixture Generator ──────────────────────────────────────────────

e2e_make_test_drop <- function(prefix = "e2e_drop_",
                               counts = NULL,
                               samplesheet = NULL,
                               manifest_extra = list(),
                               matrix_filename = "counts.tsv",
                               samplesheet_filename = "samplesheet.tsv",
                               write_manifest = TRUE,
                               species = "human",
                               genome_assembly = "GRCh38",
                               feature_id_type = "ensembl",
                               matrix_type = "raw_counts",
                               dataset_id = "DS_E2E_001") {
  drop_dir <- tempfile(pattern = prefix)
  dir.create(drop_dir, recursive = TRUE)

  # Default count matrix: 120 human Ensembl genes x 4 samples
  if (is.null(counts)) {
    g_ids <- sprintf("ENSG%011d", seq_len(120L))
    s_ids <- paste0("sample_", seq_len(4L))
    set.seed(42)
    counts <- matrix(as.integer(rpois(120L * 4L, lambda = 350L)),
                     nrow = 120L, ncol = 4L,
                     dimnames = list(g_ids, s_ids))
  }

  mat_path <- file.path(drop_dir, matrix_filename)
  if (inherits(counts, "dgCMatrix") || grepl("\\.rds$", matrix_filename)) {
    saveRDS(counts, mat_path)
  } else if (grepl("\\.gz$", matrix_filename)) {
    gz <- gzfile(mat_path, "wt")
    utils::write.table(as.data.frame(counts), file = gz, sep = "\t", quote = FALSE, col.names = NA)
    close(gz)
  } else if (grepl("\\.csv$", matrix_filename)) {
    utils::write.table(as.data.frame(counts), file = mat_path, sep = ",", quote = FALSE, col.names = NA)
  } else {
    utils::write.table(as.data.frame(counts), file = mat_path, sep = "\t", quote = FALSE, col.names = NA)
  }

  # Default samplesheet: 2 conditions with 2 replicates each (>= TS_DA_MIN_REPLICATES_PER_CONDITION)
  if (is.null(samplesheet)) {
    s_ids <- colnames(counts)
    if (is.null(s_ids)) s_ids <- paste0("sample_", seq_len(ncol(counts)))
    n_half <- length(s_ids) %/% 2L
    conds <- c(rep("control", n_half), rep("treated", length(s_ids) - n_half))
    samplesheet <- data.frame(
      sample = s_ids,
      condition = conds,
      batch = rep(c("b1", "b2"), length.out = length(s_ids)),
      stringsAsFactors = FALSE
    )
  }
  samp_path <- file.path(drop_dir, samplesheet_filename)
  if (grepl("\\.csv$", samplesheet_filename)) {
    utils::write.table(samplesheet, file = samp_path, sep = ",", quote = FALSE, row.names = FALSE)
  } else {
    utils::write.table(samplesheet, file = samp_path, sep = "\t", quote = FALSE, row.names = FALSE)
  }

  # Write manifest
  if (isTRUE(write_manifest)) {
    man <- list(
      schema_version = "cerberus-exchange/v1",
      dataset_id = dataset_id,
      species = species,
      genome_assembly = genome_assembly,
      feature_id_type = feature_id_type,
      matrix_path = matrix_filename,
      matrix_type = matrix_type,
      samplesheet_path = samplesheet_filename,
      metadata = list(
        project = "E2E_Test_Project",
        technology = "RNA-seq",
        description = "Deterministic synthetic dataset for Cerberus E2E verification"
      )
    )
    for (k in names(manifest_extra)) {
      man[[k]] <- manifest_extra[[k]]
    }
    writeLines(jsonlite::toJSON(man, auto_unbox = TRUE, pretty = TRUE),
               file.path(drop_dir, "manifest.json"))
  }

  drop_dir
}

# ── Real-World Biological Fixture Generators ────────────────────────────────

e2e_make_pbmc_matrix <- function(n_genes = 200L, n_cells = 60L, seed = 101L) {
  set.seed(seed)
  # Sparse gene expression simulating 10x single-cell data
  g_ids <- sprintf("ENSG%011d", seq_len(n_genes))
  c_ids <- sprintf("PBMC_CELL_%04d-1", seq_len(n_cells))
  
  # Non-zero count probability ~ 15%
  vals <- rpois(n_genes * n_cells, lambda = 1.2)
  vals[runif(length(vals)) > 0.20] <- 0L
  mat <- matrix(as.integer(vals), nrow = n_genes, ncol = n_cells,
                dimnames = list(g_ids, c_ids))
  Matrix::Matrix(mat, sparse = TRUE)
}

e2e_make_pbmc_samplesheet <- function(cell_ids, seed = 101L) {
  set.seed(seed)
  types <- c("CD4_T", "CD8_T", "B_cell", "Monocyte", "NK")
  data.frame(
    sample = cell_ids,
    condition = rep(c("healthy", "stimulated"), each = length(cell_ids) %/% 2L),
    cell_type = sample(types, length(cell_ids), replace = TRUE),
    batch = rep(c("batch_1", "batch_2"), length.out = length(cell_ids)),
    stringsAsFactors = FALSE
  )
}

Sys.setenv(OMNIPATHR_LOGFILE = "none")

source_project_file("R/core/io_helpers.R")
source_project_file("R/core/provenance.R")
source_project_file("R/sc/sc_velocity.R")
source_project_file("R/sc/sc_communication.R")
source_project_file("R/sc/sc_communication_engine.R")

.liana_test_object <- function() {
  samples <- c("C01", "C02", "T01", "T02", "T03")
  condition <- c("CONTROL", "CONTROL", "TREATMENT", "TREATMENT", "TREATMENT")
  cells <- unlist(lapply(seq_along(samples), function(i) paste0(samples[i], "_", 1:2)))
  identities <- rep(c("A", "B"), length.out = length(cells))
  counts <- Matrix::Matrix(
    matrix(seq_len(20 * length(cells)) %% 7L,
           nrow = 20L,
           dimnames = list(paste0("G", 1:20), cells)),
    sparse = TRUE
  )
  data <- log1p(counts)
  obj <- SeuratObject::CreateSeuratObject(counts = counts, meta.data = data.frame(
    cell_type = identities,
    sample_id = rep(samples, each = 2L),
    condition = rep(condition, each = 2L),
    row.names = cells
  ))
  obj[["RNA"]]$data <- data
  obj
}

.liana_fake_table <- function() {
  data.frame(
    source = c("A", "B"),
    target = c("B", "A"),
    ligand.complex = c("L1", "L2"),
    ligand = c("L1", "L2"),
    receptor.complex = c("R1", "R2"),
    receptor = c("R1", "R2"),
    edge_specificity = c(0.8, 0.6),
    stringsAsFactors = FALSE
  )
}

test_that("LIANA dependencies are pinned to the benchmarked commits", {
  lock <- jsonlite::fromJSON(
    paste(readLines(file.path(ts_project_root(), "renv.lock"), warn = FALSE), collapse = "\n"),
    simplifyVector = FALSE
  )
  liana <- lock$Packages$liana
  omni <- lock$Packages$OmnipathR
  expect_false(is.null(liana))
  expect_false(is.null(omni))
  expect_identical(liana$Version, "0.1.14")
  expect_identical(liana$RemoteSha, "6cab46c54234f861ea176c3de77c4b8aa45ecb3d")
  expect_identical(omni$Version, "4.1.0")
  expect_identical(omni$RemoteSha, "3e1139172d3036ffd772ebb945bc5e338ac63fde")
  expect_identical(.liana_resource_md5(), "b807279afdb1b2f34fa83a3dd51bbcb5")
})

test_that("LIANA public API and states are frozen", {
  expected <- c(
    "assert_liana_collection",
    "build_liana_collection_table",
    "liana_collection_active",
    "liana_collection_condition_summary",
    "liana_collection_sample_manifest",
    "liana_engine_available",
    "liana_engine_error_state",
    "liana_engine_public_api",
    "liana_engine_states",
    "run_liana",
    "run_liana_by_sample"
  )
  expect_identical(liana_engine_public_api(), expected)
  expect_identical(
    liana_engine_states(),
    c("valid", "missing_dependency", "invalid_input", "invalid_parameters",
      "invalid_design", "engine_failure", "no_interactions")
  )
  expect_identical(liana_engine_error_state(simpleError("x")), NA_character_)
})

test_that("run_liana returns one canonical score result through an injected backend", {
  obj <- .liana_test_object()
  calls <- list()
  backend <- list(run = function(sce, ...) {
    calls[[length(calls) + 1L]] <<- sce
    .liana_fake_table()
  })
  result <- run_liana(
    obj,
    idents_col = "cell_type",
    method = "natmi",
    resource = "Consensus",
    seed = 42L,
    min_cells = 1L,
    backend = backend
  )
  expect_length(calls, 1L)
  expect_s4_class(calls[[1L]], "SingleCellExperiment")
  expect_equal(ncol(calls[[1L]]), ncol(obj))
  expect_identical(result$source_method, "liana")
  expect_false(result$provenance$import_only)
  expect_identical(result$provenance$computation_path, "B")
  expect_true(all(result$canonical_table$source_method == "liana"))
  expect_true(all(is.finite(result$canonical_table$score)))
  expect_length(intersect(communication_rank_fields(), colnames(result$canonical_table)), 0L)
  expect_identical(result$engine$method, "natmi")
  expect_identical(result$engine$resource, "Consensus")
  expect_identical(result$engine$seed, 42L)
  expect_false(any(vapply(result, inherits, logical(1), "SingleCellExperiment")))
})

test_that("run_liana validates explicit parameters and classifies failures", {
  obj <- .liana_test_object()
  backend <- list(run = function(sce, ...) .liana_fake_table())
  expect_error(
    run_liana(obj, "missing", "natmi", "Consensus", 1L, backend = backend),
    class = "liana_engine_error"
  )
  expect_error(
    run_liana(obj, "cell_type", "connectome", "Consensus", 1L, backend = backend),
    class = "liana_engine_error"
  )
  expect_error(
    run_liana(obj, "cell_type", "natmi", "CellPhoneDB", 1L, backend = backend),
    class = "liana_engine_error"
  )
  bad_seed <- tryCatch(
    run_liana(obj, "cell_type", "natmi", "Consensus", NA_integer_, backend = backend),
    error = function(e) e
  )
  expect_identical(liana_engine_error_state(bad_seed), "invalid_parameters")
  failed <- tryCatch(
    run_liana(
      obj, "cell_type", "natmi", "Consensus", 1L,
      backend = list(run = function(sce, ...) stop("boom"))
    ),
    error = function(e) e
  )
  expect_identical(liana_engine_error_state(failed), "engine_failure")
})

test_that("LIANA usage metadata reflects post-filter cells and identities", {
  expect_true(is.function(.liana_input_usage))
  obj <- .liana_test_object()
  sce <- .liana_build_sce(
    obj,
    .liana_validate_run_parameters(obj, "cell_type", "natmi", "Consensus", 42L, 2L)$meta,
    "RNA"
  )
  validated <- .liana_validate_run_parameters(
    obj, "cell_type", "natmi", "Consensus", 42L, 2L
  )
  resource_usage <- .liana_input_usage(sce, validated, apply_resource_filter = TRUE)
  expect_identical(resource_usage$n_cells_used, 0L)
  obj$cell_type <- c("rare", rep("B", 5L), rep("C", 4L))
  backend <- list(run = function(sce, ...) .liana_fake_table())
  result <- run_liana(
    obj,
    idents_col = "cell_type",
    method = "natmi",
    resource = "Consensus",
    seed = 42L,
    min_cells = 2L,
    backend = backend
  )
  expect_identical(result$engine$n_cells_input, 10L)
  expect_identical(result$engine$n_cells_used, 9L)
  expect_identical(result$engine$n_cells_excluded, 1L)
  expect_identical(result$engine$n_populations_used, 2L)
  expect_identical(result$provenance$cells_used, 9L)
  expect_identical(result$provenance$cells_excluded, 1L)
})

test_that("LIANA refuses a sample with fewer than two retained populations", {
  obj <- .liana_test_object()
  obj$cell_type <- c("rare", rep("B", 9L))
  backend <- list(run = function(sce, ...) .liana_fake_table())
  err <- tryCatch(
    run_liana(
      obj,
      idents_col = "cell_type",
      method = "natmi",
      resource = "Consensus",
      seed = 42L,
      min_cells = 2L,
      backend = backend
    ),
    error = function(e) e
  )
  expect_identical(liana_engine_error_state(err), "no_interactions")
})

test_that("run_liana_by_sample executes five biological samples atomically", {
  obj <- .liana_test_object()
  cell_counts <- integer()
  backend <- list(run = function(sce, ...) {
    cell_counts <<- c(cell_counts, ncol(sce))
    .liana_fake_table()
  })
  collection <- run_liana_by_sample(
    obj,
    sample_col = "sample_id",
    condition_col = "condition",
    idents_col = "cell_type",
    method = "natmi",
    resource = "Consensus",
    seed = 7L,
    min_cells = 1L,
    backend = backend
  )
  expect_identical(cell_counts, rep(2L, 5L))
  expect_invisible(assert_liana_collection(collection, obj))
  manifest <- liana_collection_sample_manifest(collection)
  expect_identical(
    manifest$sample_key,
    c("CONTROL::C01", "CONTROL::C02", "TREATMENT::T01",
      "TREATMENT::T02", "TREATMENT::T03")
  )
  expect_identical(manifest$n_cells, rep(2L, 5L))
  expect_identical(anyDuplicated(manifest$analysis_id), 0L)
  expect_identical(
    manifest$analysis_id,
    paste0(
      "sc-communication-liana-",
      c("CONTROL-C01", "CONTROL-C02", "TREATMENT-T01",
        "TREATMENT-T02", "TREATMENT-T03")
    )
  )
  summary <- liana_collection_condition_summary(collection)
  expect_identical(summary$condition, c("CONTROL", "TREATMENT"))
  expect_identical(summary$n_samples, c(2L, 3L))
  expect_identical(summary$n_cells, c(4L, 6L))
  expect_identical(nrow(build_liana_collection_table(collection)), 10L)
  expect_true(all(build_liana_collection_table(collection)$sample_key %in% manifest$sample_key))
  expect_identical(
    liana_collection_active(collection, "TREATMENT::T03")$provenance$parameters$sample_id,
    "T03"
  )
})

test_that("collection validation reconciles manifest counts and active pointer", {
  obj <- .liana_test_object()
  backend <- list(run = function(sce, ...) .liana_fake_table())
  collection <- run_liana_by_sample(
    obj, "sample_id", "condition", "cell_type",
    "natmi", "Consensus", 1L, backend = backend
  )
  tampered_count <- collection
  tampered_count$sample_manifest$n_interactions[1L] <-
    tampered_count$sample_manifest$n_interactions[1L] + 1L
  expect_error(
    assert_liana_collection(tampered_count, obj),
    class = "liana_engine_error"
  )
  tampered_active <- collection
  tampered_active$active_sample_key <- "missing"
  expect_error(
    assert_liana_collection(tampered_active, obj),
    class = "liana_engine_error"
  )
  tampered_obj <- obj
  tampered_obj@meta.data$condition[1L] <- "TREATMENT"
  expect_error(
    assert_liana_collection(collection, tampered_obj),
    class = "liana_engine_error"
  )
  tampered_identity_obj <- obj
  tampered_identity_obj@meta.data$cell_type[1L] <- " altered"
  expect_error(
    assert_liana_collection(collection, tampered_identity_obj),
    class = "liana_engine_error"
  )
})

test_that("invalid sample designs fail before the backend is called", {
  obj <- .liana_test_object()
  calls <- 0L
  backend <- list(run = function(sce, ...) {
    calls <<- calls + 1L
    .liana_fake_table()
  })
  obj$condition[1:2] <- NA_character_
  err <- tryCatch(
    run_liana_by_sample(obj, "sample_id", "condition", "cell_type",
                        "natmi", "Consensus", 1L, backend = backend),
    error = function(e) e
  )
  expect_identical(liana_engine_error_state(err), "invalid_input")
  obj <- .liana_test_object()
  obj$sample_id[1:2] <- "CONTROL"
  err2 <- tryCatch(
    run_liana_by_sample(obj, "sample_id", "condition", "cell_type",
                        "natmi", "Consensus", 1L, backend = backend),
    error = function(e) e
  )
  expect_identical(liana_engine_error_state(err2), "invalid_design")
  expect_identical(calls, 0L)
})

test_that("a failed sample aborts publication of the collection", {
  obj <- .liana_test_object()
  backend <- list(run = function(sce, ...) {
    sample_col <- colnames(sce)[1L]
    if (identical(SummarizedExperiment::colData(sce)$sample_id[1L], "T03")) {
      stop("T03 failed")
    }
    .liana_fake_table()
  })
  err <- tryCatch(
    run_liana_by_sample(obj, "sample_id", "condition", "cell_type",
                        "natmi", "Consensus", 1L, backend = backend),
    error = function(e) e
  )
  expect_identical(liana_engine_error_state(err), "engine_failure")
  expect_match(conditionMessage(err), "T03", fixed = TRUE)
})

test_that("the real pinned NATMI Consensus path produces a canonical result", {
  skip_on_cran()
  skip_if_not_installed("liana")
  skip_if_not_installed("OmnipathR")
  expect_true(liana_engine_available())
  obj <- readRDS(system.file("testdata", "input", "testdata.rds", package = "liana"))
  result <- suppressWarnings(run_liana(
    obj,
    idents_col = "seurat_annotations",
    method = "natmi",
    resource = "Consensus",
    seed = 42L,
    min_cells = 5L
  ))
  expect_identical(result$source_method, "liana")
  expect_gt(nrow(result$canonical_table), 0L)
  expect_true(any(is.finite(result$canonical_table$score)))
  expect_identical(result$engine$engine_version, "0.1.14")
  expect_identical(result$engine$resource_md5, "b807279afdb1b2f34fa83a3dd51bbcb5")
  expect_false(dir.exists("omnipathr-log"))
})

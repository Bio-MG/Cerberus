# =============================================================================
# test-mod-import-bulk-drive.R — the `import_bulk` STATE PROBE (gap 1)
# =============================================================================
# WHY THIS FILE EXISTS
#   Measured LIVE on 2026-09-28 (docs/mcp_propagation.md §13.5 item 1): an
#   `import_file` answered `done` and `snapshot.modules` carried NO entry for
#   `import_bulk`. Twelve modules published a probe and all three importers
#   published none, so `done` was indistinguishable from "an import that loaded
#   nothing" — the same defect §2dn closed for `bulk_pathways`.
#
#   This file is the module-level drive test for the Bulk importer (the
#   protocol-level coverage lives in `test-drive-watcher.R`, the error-class
#   coverage in `test-mod-import-bulk.R`), and it drives the REAL importer with
#   a REAL corpus rather than asserting on source text alone.
#
# WHAT IS PROVEN HERE
#   * the WIRING — the probe reaches the wire through the EXISTING `state =`
#     seam on the token the module already published, asserted on the SOURCE so
#     the test cannot survive the wiring being deleted;
#   * (a) the real importer entry point, on a real counts file, writing a real
#     `global_data$bulk_obj`;
#   * (b) the probe present and non-NULL on success, and `has_data = FALSE` with
#     NULL dimensions on an empty session — a probe returning `NULL` would be
#     reported as `probe_error`, i.e. as a CRASH, which is why "absent" is spelled
#     `has_data = FALSE` and never a missing list;
#   * (c) FALSIFICATION — the predicate that passes on the real record must reject
#     a MUTATED one.
# =============================================================================

source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")
source_project_file("R/core/io_helpers.R")
# The module itself: `smart_read()` and `assemble_bulk_obj()` are top-level there,
# and the importer closure calls both by name, so they must be the shipped ones.
source_project_file("modules/import/mod_import_bulk.R")

.TMIB_FILE <- "modules/import/mod_import_bulk.R"
.tmib_read <- function() paste(readLines(file.path(ts_project_root(), .TMIB_FILE),
                                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# The published importer is an ANONYMOUS function inside the
# `ts_drive_publish_importer()` call, so it is lifted out of the real source
# rather than retyped: a test that exercised a copy would measure the copy.
.tmib_importer <- function(module, envir) {
  p <- ts_ast_parse(.TMIB_FILE)
  found <- NULL
  for (i in seq_along(p)) {
    ts_ast_walk(p[[i]], function(x) {
      if (!ts_ast_is_call_to(x, "ts_drive_publish_importer")) return(FALSE)
      l <- as.list(x)
      if (length(l) < 4L) return(FALSE)
      mod <- tryCatch(as.character(l[[3]]), error = function(e) "")
      if (!identical(mod, module)) return(FALSE)
      found <<- eval(l[[4]], envir = envir)
      return(TRUE)
    })
    if (!is.null(found)) break
  }
  if (is.null(found)) stop("no published importer for ", module, call. = FALSE)
  found
}

# A real counts file: genes in rows, samples in columns, header + rownames, which
# is the shape `smart_read(path, TRUE, TRUE)` reads.
.tmib_fixture <- function() {
  d <- file.path(tempdir(), "tmib_fixture")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  set.seed(20260928L)
  ng <- 120; ns <- 6
  m <- matrix(rnbinom(ng * ns, mu = 100, size = 3), nrow = ng,
              dimnames = list(paste0("g", seq_len(ng)), paste0("s", seq_len(ns))))
  m[1:20, 1:3] <- m[1:20, 1:3] * 4L
  df <- data.frame(gene = rownames(m), m, check.names = FALSE)
  utils::write.csv(df, file.path(d, "counts.csv"), row.names = FALSE)
  d
}

.tmib_env <- function() {
  e <- new.env(parent = globalenv())
  # `smart_read()` and `assemble_bulk_obj()` are themselves CLOSURES inside
  # `mod_import_bulk_server()`, not top-level functions — so they are lifted out
  # of the real source the same way the probe is. Sourcing the module is not
  # enough, and MEASURED: the first version of this test did exactly that and the
  # importer answered `counts: impossible de trouver la fonction "smart_read"`.
  e$smart_read <- ts_ast_assignment(.TMIB_FILE, "smart_read", e)
  e$assemble_bulk_obj <- ts_ast_assignment(.TMIB_FILE, "assemble_bulk_obj", e)
  # Also a closure of `server()`, called by the importer on success. Extracted,
  # not stubbed: multi-dataset registration is part of the path the importer walks.
  e$.register_multi_dataset <- ts_ast_assignment(.TMIB_FILE, ".register_multi_dataset", e)
  # The importer's other free variables: a session flag it sets, and the UI
  # notification it fires. `showNotification` is the ONE stub here, and it is
  # legitimate: it is a Shiny UI side effect with no bearing on the probe, and
  # calling it outside a session would error. Nothing on the DATA path is stubbed.
  e$temp_data <- new.env(parent = emptyenv())
  e$showNotification <- function(...) invisible(NULL)
  e$global_data <- new.env(parent = emptyenv())
  e$input <- list(counts_format = "rows", min_counts = 10,
                  project_name = "Bulk", bulk_import_mode = "merged_matrix")
  e$add_log <- function(msg) invisible(NULL)
  e
}

test_that("import_bulk publishes a BOUNDED state probe, non-NULL on success and empty on nothing", {
  dir <- .tmib_fixture()
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)
  envir <- .tmib_env()

  # (a) the REAL importer entry point, on a REAL counts file.
  importer <- .tmib_importer("import_bulk", envir)
  expect_true(is.function(importer))
  res <- importer(list(counts_path = file.path(dir, "counts.csv"), mode = "merged_matrix"))
  expect_true(res$ok, info = paste(res$errors, collapse = "; "))
  # It really wrote into the ENVIRONMENT it was given: a list would have accepted
  # a copy here and every assertion below would have been theatre.
  obj <- envir$global_data$bulk_obj
  expect_false(is.null(obj))
  expect_true(is.matrix(obj$counts) || is.data.frame(obj$counts))

  # The WIRING, asserted on the source.
  expect_match(.tmib_read(), "state = drive_state", fixed = TRUE)

  probe <- ts_ast_assignment(.TMIB_FILE, "drive_state", envir)
  expect_true(is.function(probe))

  # (b) present and non-NULL on success, with the module's OWN numbers.
  st <- probe()
  expect_type(st, "list")
  expect_identical(st$module, "import_bulk")
  expect_true(st$has_data)
  expect_identical(st$n_genes, 120L)
  expect_identical(st$n_samples, 6L)
  expect_identical(st$import_mode, "merged_matrix")

  # BOUNDED: the collector projects only `descriptor` and passes every other
  # field through whole (drive_watcher.R:1164-1185), so keeping this small is the
  # module's obligation, not the collector's.
  expect_setequal(names(st), c("module", "has_data", "n_genes", "n_samples", "import_mode"))
  for (f in st) {
    expect_true(is.atomic(f) || is.null(f),
                info = "a probe field must be a scalar, never a matrix or a list")
  }

  # Empty session: has_data FALSE and NULL dimensions. NOT a NULL probe — that
  # would be published as `probe_error`, i.e. as a crash.
  envir$global_data$bulk_obj <- NULL
  st0 <- probe()
  expect_type(st0, "list")
  expect_false(st0$has_data)
  expect_null(st0$n_genes)
  expect_null(st0$n_samples)

  # (c) FALSIFICATION.
  ok <- function(s) isTRUE(s$has_data) && identical(s$n_genes, 120L) &&
    identical(s$import_mode, "merged_matrix")
  expect_true(ok(st))
  for (mut in list(
    function(s) { s$n_genes <- 1L; s },
    function(s) { s$has_data <- FALSE; s },
    function(s) { s$import_mode <- "per_sample"; s },
    function(s) { s$n_genes <- NULL; s }
  )) {
    expect_failure(expect_true(ok(mut(st))), label = "a mutated probe record was accepted")
  }
})

test_that("the Bulk probe reports an ERRORED import as empty, not as a phantom success", {
  dir <- .tmib_fixture()
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)
  envir <- .tmib_env()
  importer <- .tmib_importer("import_bulk", envir)
  probe <- ts_ast_assignment(.TMIB_FILE, "drive_state", envir)

  # A refused payload never reaches the writer, so the session holds nothing and
  # the probe must say so. This is the assertion that keeps a failed import from
  # inheriting the PREVIOUS session's dimensions.
  bad <- suppressWarnings(importer(list(counts_path = file.path(dir, "no_such_file.csv"))))
  expect_false(bad$ok)
  expect_null(envir$global_data$bulk_obj)
  st <- probe()
  expect_false(st$has_data)
  expect_null(st$n_genes)
})

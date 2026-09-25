# =============================================================================
# test-mod-bulk-signatures-drive.R — Drive live control for the Bulk signature
# scoring action `bulk-signatures-run_signatures` (module `bulk_signatures`).
# =============================================================================
# Phase C. The action calls the module-level `run_signatures()`, which composes
# the SAME two R/ calls as the human observer
# (modules/bulk/mod_bulk_signatures.R:122-174) with a FROZEN input set. The human
# observer is neither re-wired nor duplicated, and `R/bulk/bulk_signatures.R` is
# untouched: its exported surface is frozen by
# test-bulk-signatures-contract-freeze.R, so the wrapper lives in the module layer
# (the same decision as `run_annot()` / `run_markers()` in the SC domain).
#
# What is genuinely new here, and therefore what this file has to prove:
#   - the availability PRE-CHECK refuses a locally-missing resource BEFORE any
#     load or score is attempted (measured: on this host `progeny`, `dorothea` and
#     `decoupleR` are absent, `msigdbr` is present);
#   - a fileInput PATH can never reach the action, and the refusal does not echo
#     the value it refuses;
#   - `long = TRUE` gives `running` a real producer, and the terminal state is
#     published for `done` / `empty` / `error`;
#   - a failure publishes NO result count, so a stale table cannot be read next
#     to an error (the Phase B rule, asserted in the SC file too).
# =============================================================================

source_project_file("config/defaults.R")
source_project_file("config/thresholds.R")   # TS_BULK_GSVA_MIN_SIZE / MAX_SIZE
source_project_file("R/core/io_helpers.R")    # %||%
source_project_file("R/core/error_state.R")   # ts_error_state
source_project_file("R/bulk/bulk_provenance.R")  # bulk_ensure_provenance (store path)
source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")
source_project_file("R/bulk/bulk_signatures.R")
source_project_file("modules/bulk/mod_bulk_signatures.R")  # the code under test

if (!exists(".tr", envir = globalenv()))
  assign(".tr", function(key) key, envir = globalenv())

# `.t_fmt()` also lives in global.R (not in any R/ module file), and the human
# observer calls it to build its notification. Stubbed with the same
# named-placeholder substitution as global.R:275; a drift here could only change
# notification TEXT in this test, never the stored shape it measures.
if (!exists(".t_fmt", envir = globalenv()))
  assign(".t_fmt", function(template, ...) {
    vals <- list(...)
    for (nm in names(vals)) {
      template <- gsub(paste0("{", nm, "}"), format(vals[[nm]]), template, fixed = TRUE)
    }
    template
  }, envir = globalenv())

# --- Mocks -------------------------------------------------------------------
# `run_signatures()` and `.bulk_signatures_run_drive()` are top-level functions of
# the module file, so their enclosing environment is globalenv. Pointing that
# environment at a CHILD of globalenv makes the domain calls resolve to the mocks
# while every other name still falls through to the real definitions.
#
# 🔴 `environment(f) <- e` INSIDE A FUNCTION IS A NO-OP FOR A GLOBAL BINDING, and
# it fails silently: the replacement assigns the modified copy into the CALLER's
# frame, so the object the rest of the session sees is untouched. Measured here —
# `environment(run_signatures)` still reported globalenv, the mock was present in
# `e`, and the REAL `bulk_load_signatures()` ran anyway. The patched copy must
# therefore be published back with `assign()`, and the restoration is asserted.
.bsig_patch_env <- function(name, e) {
  old <- get(name, envir = globalenv())
  f <- old
  environment(f) <- e
  assign(name, f, envir = globalenv())
  old
}

.bsig_with_mocks <- function(resources = NULL, load = NULL, score = NULL,
                             provenance = NULL, code) {
  e <- new.env(parent = globalenv())
  if (!is.null(resources)) e$bulk_signature_resources <- resources
  if (!is.null(load)) e$bulk_load_signatures <- load
  if (!is.null(score)) e$bulk_score_signatures <- score
  if (!is.null(provenance)) e$bulk_ensure_provenance <- provenance
  # ⚠️ The availability pre-check lives in its OWN function. Patching only
  # `run_signatures()` leaves the check calling the real
  # `bulk_signature_resources()`, which reports Hallmark as available on a host
  # where msigdbr IS installed — so the mocked "missing" case sailed straight
  # through and the test asserted on a returned list instead of an error.
  # The store helper is one of the patched targets: the drive path reaches
  # `bulk_ensure_provenance` THROUGH it, so patching only the wrapper would leave
  # the real function in the chain.
  targets <- c("run_signatures", ".bulk_signatures_run_drive",
               ".bulk_signatures_resource_check", ".bulk_signatures_store")
  olds <- lapply(targets, .bsig_patch_env, e = e)
  names(olds) <- targets
  on.exit({
    for (nm in targets) assign(nm, olds[[nm]], envir = globalenv())
  }, add = TRUE)
  force(code)
}

.bsig_resources <- function(available = TRUE, resource = "hallmark",
                            requires = "msigdbr") {
  function() data.frame(
    resource   = c("hallmark", "progeny", "dorothea", "rds_local"),
    label      = c("MSigDB Hallmark", "PROGENy", "DoRothEA", "RDS local"),
    available  = c(available, TRUE, TRUE, TRUE),
    requires   = c(requires, "decoupleR + progeny", "decoupleR + dorothea", ""),
    description = c("d", "d", "d", "d"),
    stringsAsFactors = FALSE
  )
}

.bsig_mat <- function(n_genes = 40L, n_samples = 6L) {
  matrix(seq_len(n_genes * n_samples) / 7, nrow = n_genes, ncol = n_samples,
         dimnames = list(paste0("G", seq_len(n_genes)),
                         paste0("S", seq_len(n_samples))))
}

.bsig_scores <- function(n_sig = 3L, n_samples = 6L) {
  # A 0-row matrix is the `empty` case, and R refuses a dimnames entry whose
  # length does not match the extent when the data is empty: build it with a NULL
  # row name instead of `character(0)`.
  if (n_sig == 0L) {
    return(matrix(numeric(0), nrow = 0L, ncol = n_samples,
                  dimnames = list(NULL, paste0("S", seq_len(n_samples)))))
  }
  matrix(as.numeric(seq_len(n_sig * n_samples)), nrow = n_sig, ncol = n_samples,
         dimnames = list(paste0("SIG", seq_len(n_sig)),
                         paste0("S", seq_len(n_samples))))
}

.bsig_score <- function(n_sig = 3L, n_samples = 6L) {
  m <- .bsig_scores(n_sig, n_samples)
  function(expr_matrix, gene_sets, method = "ssgsea", ...) {
    list(type = "bulk_signature_scores", scores = m, method = method)
  }
}

# -----------------------------------------------------------------------------
test_that("the signature drive input set is frozen and carries no file path", {
  inputs <- .bulk_signatures_drive_inputs()

  # The name set is pinned: this is the assertion that keeps a future
  # session-derived parameter from being added silently.
  expect_setequal(names(inputs),
                  c("sig_resource", "sig_organism", "sig_method",
                    "sig_min_size", "sig_max_size"))
  expect_identical(inputs$sig_resource, "hallmark")
  expect_identical(inputs$sig_organism, "human")
  expect_identical(inputs$sig_method, "ssgsea")
  expect_identical(inputs$sig_min_size, TS_BULK_GSVA_MIN_SIZE)
  expect_identical(inputs$sig_max_size, TS_BULK_GSVA_MAX_SIZE)

  # `sig_rds` is a fileInput PATH and is deliberately absent.
  expect_null(inputs$sig_rds)
  expect_false("sig_rds" %in% names(inputs))
})

# -----------------------------------------------------------------------------
test_that("run_signatures loads Hallmark and scores the frozen set on the VST matrix", {
  seen <- new.env(parent = emptyenv())
  seen$load <- NULL
  seen$score <- NULL

  out <- .bsig_with_mocks(
    resources = .bsig_resources(available = TRUE),
    load = function(resource = "hallmark", organism = "human", ...) {
      seen$load <- list(resource = resource, organism = organism)
      list(HALLMARK_A = c("G1", "G2", "G3"), HALLMARK_B = c("G4", "G5"))
    },
    score = function(expr_matrix, gene_sets, method = "ssgsea", ...) {
      seen$score <- list(n = nrow(expr_matrix), p = ncol(expr_matrix),
                         method = method, dots = list(...))
      .bsig_score(n_sig = 3L, n_samples = ncol(expr_matrix))()
    },
    provenance = function(bo) bo,
    code = run_signatures(.bsig_mat(), .bulk_signatures_drive_inputs())
  )

  expect_true(out$ok)
  expect_identical(out$n_results, 3L)
  expect_identical(out$resource, "hallmark")
  expect_identical(out$method, "ssgsea")
  # The wrapper hands back the WHOLE record, because that is the shape the slot
  # holds and the readers read.
  expect_true(is.list(out$record))
  expect_true(is.matrix(out$record$scores))
  expect_identical(dim(out$record$scores), c(3L, 6L))

  # The wrapper must call the two domain functions with the FROZEN values, not
  # with anything re-read from the session.
  expect_identical(seen$load$resource, "hallmark")
  expect_identical(seen$load$organism, "human")
  expect_identical(seen$score$method, "ssgsea")
  expect_identical(seen$score$n, 40L)
  expect_identical(seen$score$dots$min_size, TS_BULK_GSVA_MIN_SIZE)
  expect_identical(seen$score$dots$max_size, TS_BULK_GSVA_MAX_SIZE)
})

# -----------------------------------------------------------------------------
test_that("a locally missing resource fails early with state = missing_dependency", {
  n_load <- 0L
  n_score <- 0L

  err <- tryCatch(
    .bsig_with_mocks(
      resources = .bsig_resources(available = FALSE, requires = "msigdbr"),
      load = function(...) { n_load <<- n_load + 1L; list() },
      score = function(...) { n_score <<- n_score + 1L; list() },
      provenance = function(bo) bo,
      code = run_signatures(.bsig_mat(), .bulk_signatures_drive_inputs())
    ),
    condition = function(e) e
  )

  expect_s3_class(err, "bulk_signatures_error")
  expect_identical(ts_error_state(err), "missing_dependency")
  # A non-condition here means the guard did NOT fire, so the assertions below
  # degrade to failures rather than ERRORING on `conditionMessage()` of a list:
  # a red that crashes says "the test is broken", a red that fails says "the
  # guard is missing".
  msg <- if (inherits(err, "condition")) conditionMessage(err) else ""
  expect_match(msg, "not available locally", fixed = TRUE)
  # "FAIL EARLY" is the whole point: neither domain call may have been reached.
  expect_identical(n_load, 0L)
  expect_identical(n_score, 0L)
  # The message names the resource and the missing PACKAGE, never a path.
  expect_match(msg, "msigdbr", fixed = TRUE)
  expect_false(grepl("[\\/]", msg))
})

# -----------------------------------------------------------------------------
test_that("a file path input is refused by construction and never echoed", {
  secret <- "C:/private/human/signatures.rds"
  err <- tryCatch(
    .bsig_with_mocks(
      resources = .bsig_resources(),
      load = function(...) stop("must not be reached"),
      score = function(...) stop("must not be reached"),
      provenance = function(bo) bo,
      code = run_signatures(.bsig_mat(), list(
        sig_resource = "rds_local", sig_organism = "human",
        sig_method = "ssgsea", sig_min_size = 10L, sig_max_size = 500L,
        sig_rds = secret))
    ),
    condition = function(e) e
  )

  expect_s3_class(err, "bulk_signatures_error")
  msg <- if (inherits(err, "condition")) conditionMessage(err) else ""
  expect_identical(ts_error_state(err), "invalid_input")
  expect_false(grepl(secret, msg, fixed = TRUE))
  expect_false(grepl("private", msg, fixed = TRUE))
})

# -----------------------------------------------------------------------------
test_that("the token is published as a long job and no DOM button is bound", {
  src <- readLines(file.path(ts_project_root(), "modules", "bulk",
                             "mod_bulk_signatures.R"),
                   warn = FALSE, encoding = "UTF-8")
  expect_length(grep('ts_drive_publish_token\\(.*"bulk-signatures-run_signatures"',
                     src), 1L)
  expect_length(grep('ts_drive_bind_button\\(.*"bulk-signatures-run_signatures"',
                     src), 0L)
  expect_identical(TS_DRIVE_BULK_SIGNATURES_BUTTON, "bulk-signatures-run_signatures")
  expect_identical(TS_DRIVE_BULK_SIGNATURES_MODULE, "bulk_signatures")
  expect_identical(.BULK_SIGNATURES_DRIVE_BUTTON, TS_DRIVE_BULK_SIGNATURES_BUTTON)
  expect_identical(.BULK_SIGNATURES_DRIVE_MODULE, TS_DRIVE_BULK_SIGNATURES_MODULE)

  # The REAL registry, not a grep: `long = TRUE` is what gives `running` a
  # producer, since the job is synchronous.
  gd <- shiny::reactiveValues()
  gd$drive_registry <- new.env(parent = emptyenv())
  counter <- shiny::reactiveVal(0L)
  expect_true(ts_drive_publish_token(gd, TS_DRIVE_BULK_SIGNATURES_BUTTON, counter,
                                     ready = function() TRUE, long = TRUE))
  entry <- shiny::isolate(gd$drive_registry)[[TS_DRIVE_BULK_SIGNATURES_BUTTON]]
  expect_true(isTRUE(entry$long))
  expect_identical(ts_drive_entry_ready(entry), "ready")
  expect_identical(ts_drive_token_of(gd, TS_DRIVE_BULK_SIGNATURES_BUTTON), counter)

  # The job API the module's close callback relies on, exercised against its REAL
  # contract (measured in drive_watcher.R, not assumed):
  #   - `ts_drive_job_begin()` returns the job record, not TRUE, and REFUSES when a
  #     job is already in flight — hence the clear() first;
  #   - `ts_drive_job_finish()` does NOT clear: it moves the job to a PENDING
  #     terminal state, which the poller consumes later with `ts_drive_job_clear()`;
  #   - the status vocabulary is done / error / invalid / timeout / session_lost.
  ts_drive_job_clear()
  on.exit(try(ts_drive_job_clear(), silent = TRUE), add = TRUE)

  job <- ts_drive_job_begin(1L, "bulk_signatures", "run_pipeline",
                            TS_DRIVE_BULK_SIGNATURES_BUTTON)
  expect_true(is.list(job))
  expect_identical(job$button, TS_DRIVE_BULK_SIGNATURES_BUTTON)
  expect_identical(job$status, "running")
  expect_true(isTRUE(ts_drive_job_busy()))
  # Nothing terminal has been DECLARED yet: pending is NULL, not "running".
  expect_null(ts_drive_job_pending())

  # A foreign button cannot close this job.
  expect_false(ts_drive_job_finish("bulk-de-run_de", status = "done",
                                   job_id = job$job_id))
  expect_null(ts_drive_job_pending())

  expect_true(ts_drive_job_finish(TS_DRIVE_BULK_SIGNATURES_BUTTON,
                                 status = "done", job_id = job$job_id))
  # `pending` is a RECORD, not a bare status string.
  expect_identical(ts_drive_job_pending()$status, "done")
  # Still busy until the poller consumes the pending state.
  expect_true(isTRUE(ts_drive_job_busy()))
  ts_drive_job_clear()
  expect_false(isTRUE(ts_drive_job_busy()))
})

# -----------------------------------------------------------------------------
test_that("readiness names the missing prerequisite instead of returning FALSE", {
  gd <- shiny::reactiveValues()
  rv <- shiny::reactiveValues()

  expect_match(.bulk_signatures_drive_ready(rv, gd), "bulk_obj is NULL", fixed = TRUE)

  gd$bulk_obj <- list(counts = .bsig_mat(), metadata = data.frame(x = 1))
  expect_match(.bulk_signatures_drive_ready(rv, gd), "VST matrix", fixed = TRUE)

  rv$vst_mat <- .bsig_mat()
  expect_true(isTRUE(.bulk_signatures_drive_ready(rv, gd)))

  # An unavailable FROZEN resource is a readiness answer, not a run failure.
  old <- .bulk_signatures_resource_check
  .bulk_signatures_resource_check <<- function(resource) {
    list(ok = FALSE, requires = "msigdbr")
  }
  on.exit(.bulk_signatures_resource_check <<- old, add = TRUE)
  # An unavailable FROZEN resource is a readiness answer, not a run failure. Read
  # it through `as.character()` so a falsified guard yields a FAILURE (TRUE is
  # not a character) instead of an error inside `expect_match()`.
  rdy <- .bulk_signatures_drive_ready(rv, gd)
  expect_true(is.character(rdy))
  expect_match(as.character(rdy), "not available locally", fixed = TRUE)
})

# -----------------------------------------------------------------------------
# THE READER-SIDE CONTRACT.
#
# Live validation on 2026-09-25 proved that 84 green assertions could sit on top
# of a broken panel: the drive path stored a bare MATRIX where the slot holds the
# scoring RECORD, so `sig_status` and `sig_heatmap` both raised
# `$ operator is invalid for atomic vectors` while the drive state still reported
# `done` with a correct `n_results`. The old test had asserted the writer's own
# assumption, so it agreed with the bug.
#
# This test therefore asserts from the READER's side, and it runs BOTH triggers:
# the real human observer (`testServer` on the module, so its own code path
# executes) and the real drive wrapper. It compares the two stored objects rather
# than either one's idea of the right shape, so any future divergence between the
# triggers - or between the writer and the readers - turns it red.
# -----------------------------------------------------------------------------
# Patches a name in globalenv and returns what to put back. A name that does not
# exist here is a legitimate case: the module calls `nav_select()` unqualified and
# only resolves it because `bslib` is ATTACHED in the app, which a test run does
# not do. `existed = FALSE` means "remove the binding again on restore".
.bsig_patch_global <- function(name, value) {
  existed <- exists(name, envir = globalenv(), inherits = FALSE)
  old <- if (existed) get(name, envir = globalenv()) else NULL
  assign(name, value, envir = globalenv())
  list(value = old, existed = existed)
}

.bsig_restore_globals <- function(saved) {
  for (nm in names(saved)) {
    entry <- saved[[nm]]
    if (isTRUE(entry$existed)) {
      assign(nm, entry$value, envir = globalenv())
    } else if (exists(nm, envir = globalenv(), inherits = FALSE)) {
      rm(list = nm, envir = globalenv())
    }
  }
}

test_that("the human observer and the drive action store the SAME shape", {
  scores <- .bsig_scores(n_sig = 3L, n_samples = 6L)

  # The score record the domain returns, with every field the readers touch.
  record <- function(method = "ssgsea") {
    list(type = "bulk_signature_scores", status = "ok", analysis_id = "x",
         method = method, resource = "hallmark", scores = scores,
         gene_sets = list(A = "G1"), qc = list(), warnings = character(0),
         disclaimer = "d", provenance = list(), timestamp_utc = "2026-01-01T00:00:00Z")
  }

  saved <- list(
    bulk_load_signatures = .bsig_patch_global("bulk_load_signatures",
      function(resource = "hallmark", organism = "human", ...) list(A = c("G1", "G2"))),
    bulk_score_signatures = .bsig_patch_global("bulk_score_signatures",
      function(expr_matrix, gene_sets, method = "ssgsea", ...) record(method)),
    bulk_ensure_provenance = .bsig_patch_global("bulk_ensure_provenance",
      function(bo) bo),
    # UI calls the test does not care about, and which only exist because a
    # package is ATTACHED in the app: `nav_select` (bslib) and `renderDT` (DT).
    # `renderDT` in particular is CALLED while the outputs are defined, so an
    # absent binding aborts testServer before any assertion runs.
    nav_select = .bsig_patch_global("nav_select", function(...) invisible(TRUE)),
    renderDT = .bsig_patch_global("renderDT", function(...) NULL)
  )
  on.exit(.bsig_restore_globals(saved), add = TRUE)

  new_state <- function() {
    gd <- shiny::reactiveValues()
    gd$bulk_obj <- list(counts = .bsig_mat(), metadata = data.frame(x = 1))
    gd$language <- "fr"
    rv <- shiny::reactiveValues()
    rv$vst_mat <- .bsig_mat()
    list(gd = gd, rv = rv)
  }

  # --- 1. the HUMAN path: the module's own observer, through testServer --------
  h <- new_state()
  shiny::testServer(mod_bulk_signatures_server,
                    args = list(global_data = h$gd, shared_rv = h$rv), {
                      session$setInputs(run_signatures = 1)
                    })
  human <- shiny::isolate(h$rv$signature_scores)

  # --- 2. the DRIVE path: the real wrapper, on its own state ------------------
  d <- new_state()
  close_log <- character(0)
  res <- shiny::isolate(.bulk_signatures_run_drive(d$gd, d$rv,
    function(status, error = NULL) { close_log <<- c(close_log, status) }))
  driven <- shiny::isolate(d$rv$signature_scores)

  expect_identical(res$status, "done")
  expect_identical(close_log, "done")

  # Both writes happened, and both are the RECORD. The name comparison is done
  # through a safe form so a divergent writer produces FAILURES here rather than an
  # error inside expect_setequal() - the `is.list` assertions above are what make
  # the divergence loud.
  expect_true(is.list(human))
  expect_true(is.list(driven))
  nm_h <- if (is.list(human)) names(human) else character(0)
  nm_d <- if (is.list(driven)) names(driven) else character(0)
  expect_setequal(nm_h, nm_d)

  # The exact expressions the five readers evaluate, on BOTH objects. This is the
  # assertion that failed live: `$` on a matrix. The `$` is reached through
  # `if (is.list(obj))`, so a divergent writer yields a FAILURE with the reader's
  # own error message quoted, instead of crashing the test on it.
  for (obj in list(human, driven)) {
    s <- if (is.list(obj)) obj$scores else NULL
    expect_true(is.list(obj))
    expect_true(is.matrix(s))
    expect_identical(nrow(s), 3L)
    expect_identical(ncol(s), 6L)
    expect_true(is.character(if (is.list(obj)) obj$method else NULL))
    expect_identical(as.character(if (is.list(obj)) obj$method else NULL), "ssgsea")
    expect_identical(as.character(if (is.list(obj)) obj$resource else NULL), "hallmark")
  }

  # The contractual second slot gets the MATRIX in both cases.
  expect_true(is.matrix(shiny::isolate(h$gd$bulk_obj$pathways$signatures)))
  expect_true(is.matrix(shiny::isolate(d$gd$bulk_obj$pathways$signatures)))
  expect_identical(dim(shiny::isolate(d$gd$bulk_obj$pathways$signatures)), c(3L, 6L))

  # And the drive state counts the stored scores, not the wrapper's own field.
  st <- .bulk_signatures_drive_state(d$rv, d$gd, function() "done", "ran")
  expect_identical(st$n_results, 3L)
})

test_that("the shared writer refuses anything that is not a scoring record", {
  gd <- shiny::reactiveValues()
  gd$bulk_obj <- list(counts = .bsig_mat(), metadata = data.frame(x = 1))
  rv <- shiny::reactiveValues()

  # A bare matrix is exactly what the live defect stored: it must be refused, not
  # silently written, so the failure surfaces here instead of in the panel.
  err <- tryCatch(.bulk_signatures_store(.bsig_scores(n_sig = 2L), gd, rv),
                  condition = function(e) e)
  expect_s3_class(err, "bulk_signatures_error")
  expect_identical(ts_error_state(err), "invalid_input")
  expect_null(shiny::isolate(rv$signature_scores))
})

# -----------------------------------------------------------------------------
test_that("the drive run publishes done, empty and error without a stale count", {
  close_log <- list()
  closer <- function(status, error = NULL) {
    close_log[[length(close_log) + 1L]] <<- list(status = status, error = error)
    invisible(TRUE)
  }

  gd <- shiny::reactiveValues()
  gd$bulk_obj <- list(counts = .bsig_mat(), metadata = data.frame(x = 1))
  rv <- shiny::reactiveValues()
  rv$vst_mat <- .bsig_mat()

  # done
  res <- .bsig_with_mocks(
    resources = .bsig_resources(), load = function(...) list(A = c("G1")),
    score = .bsig_score(n_sig = 3L), provenance = function(bo) bo,
    # The wrapper WRITES shared_rv and global_data, so it must be called the way
    # the module calls it: inside a reactive context. `isolate()` gives the test
    # that context without registering a consumer.
    code = shiny::isolate(.bulk_signatures_run_drive(gd, rv, closer)))
  expect_identical(res$status, "done")
  expect_identical(res$step, "ran")
  expect_identical(res$n_results, 3L)
  expect_identical(close_log[[1L]]$status, "done")
  expect_null(close_log[[1L]]$error)
  # The result is written the way the human path writes it: the RECORD in the
  # slot, and the MATRIX in the contractual bulk_obj slot.
  sc <- shiny::isolate(rv$signature_scores)
  expect_true(is.list(sc))
  expect_identical(dim(if (is.list(sc)) sc$scores else NULL), c(3L, 6L))
  expect_true(!is.null(shiny::isolate(gd$bulk_obj$pathways$signatures)))

  # empty is NOT done: the step ran, it produced nothing. The JOB still closes as
  # `done`, because `ts_drive_job_set_pending()` refuses "empty" — asserting the
  # view status and the job status separately is the only way to keep both honest.
  res <- .bsig_with_mocks(
    resources = .bsig_resources(), load = function(...) list(A = c("G1")),
    score = .bsig_score(n_sig = 0L), provenance = function(bo) bo,
    code = shiny::isolate(.bulk_signatures_run_drive(gd, rv, closer)))
  expect_identical(res$status, "empty")
  expect_identical(res$step, "ran")
  expect_identical(res$n_results, 0L)
  expect_identical(close_log[[2L]]$status, "done")

  # error: no result count, even though a previous run left 3 rows behind.
  res <- .bsig_with_mocks(
    resources = .bsig_resources(available = FALSE),
    load = function(...) list(A = c("G1")),
    score = .bsig_score(), provenance = function(bo) bo,
    code = shiny::isolate(.bulk_signatures_run_drive(gd, rv, closer)))
  expect_identical(res$status, "error")
  expect_identical(res$step, "error")
  expect_identical(res$n_results, 0L)
  expect_identical(close_log[[3L]]$status, "error")
  expect_true(nzchar(close_log[[3L]]$error))
})

# -----------------------------------------------------------------------------
test_that("the state probe and the view are closed and five-state", {
  view <- .bulk_signatures_drive_view("done", 2.5, 9L, 4L, has_data = TRUE,
                                     ready = TRUE, step = "ran")
  expect_setequal(names(view),
                  c("module", "action", "status", "elapsed_s", "seq",
                    "n_results", "has_data", "ready", "steps"))
  expect_identical(view$module, "bulk_signatures")
  expect_identical(view$action, "run_pipeline")
  expect_identical(view$steps, list(signatures = "ran"))
  expect_identical(.bulk_signatures_drive_view("idle")$steps,
                   list(signatures = "skipped"))
  # An unknown step is coerced to `error`, never published as-is.
  expect_identical(
    .bulk_signatures_drive_view("error", step = "not-a-state")$steps$signatures,
    "error")

  gd <- shiny::reactiveValues()
  gd$bulk_obj <- list(counts = .bsig_mat(), metadata = data.frame(x = 1))
  rv <- shiny::reactiveValues()
  rv$vst_mat <- .bsig_mat()
  # A MATRIX, which is what the human observer stores: `signature_scores` holds
  # `res$scores`, not the whole result record. A record here would silently make
  # the probe report 0 results.
  rv$signature_scores <- .bsig_scores(n_sig = 5L)

  st <- .bulk_signatures_drive_state(rv, gd, function() "done", "ran")
  expect_identical(st$status, "done")
  expect_identical(st$n_results, 5L)
  expect_true(st$ready)
  expect_true(st$has_data)

  # No VST matrix: not_ready, whatever the run state claims.
  rv2 <- shiny::reactiveValues()
  expect_identical(.bulk_signatures_drive_state(rv2, gd, function() "done")$status,
                   "not_ready")
})

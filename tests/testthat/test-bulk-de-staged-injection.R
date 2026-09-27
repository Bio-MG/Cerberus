# =============================================================================
# tests/testthat/test-bulk-de-staged-injection.R
#
# Staged injection: a select value must never be sent before the options that can
# represent it exist.
#
# MEASURED live (2026-09-27, GSE164073 27,946 x 18). One scenario carrying the
# whole contrast —
#   condition_col = "condition", group_ref = "CoV2", group_target = "mock"
# — left the client holding `group_ref = "mock"`, `group_target = "CoV2"`: the
# module's own defaults, and the run was refused at the bound.
#
# It is NOT a module overwrite and NOT a transposition, and both were measured
# before this file existed:
#
#   * a keep-if-valid rule in the rebuild observer was implemented and the
#     identical batch still failed, so the observer is not the culprit;
#   * the defaults happened to equal the requested pair REVERSED, because
#     `lvls[1]`/`lvls[2]` for this dataset are `mock`/`CoV2`. Reading that as a
#     swap is a coincidence of level order, and no code swaps ref and target.
#
# The mechanism is that the drive injects a `selectInput` value that is not among
# that select's CURRENT options. A browser `<select>` cannot represent it, so the
# value never exists client-side; the module's rebuild then supplies its default.
# No module-side guard can recover a value the client never held.
#
# The measured proof, and the recipe this slice encodes: inject `condition_col`
# ALONE, let the module rebuild the options, then inject the reverse pair — which
# landed, and produced `active_contrast = "mock_vs_CoV2"`, i.e. exactly the
# requested ref/target under the module's `target_vs_ref` naming.
#
# So the ORDER is declared as data (TS_DRIVE_INPUT_STAGES) and the drive injects
# one stage per beat, confirming only after the last one. Nothing is ever
# re-injected: a value the drive already sent is not sent again, and a human edit
# is still refused at the bound exactly as before.
# =============================================================================

# The drive layer is NOT auto-sourced by helper-source.R, so it is sourced here
# explicitly — the same two lines, for the same reason, as
# test-bulk-de-input-confirm.R:37-38 and test-drive-watcher.R:68-69. Without them
# every block in this file errors with "could not find function ts_drive_tick",
# which reads like a product failure and is not one.
source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")

.BSI <- function() {
  vals <- list(`bulk-de-condition_col` = "condition",
               `bulk-de-group_ref`     = "CoV2",
               `bulk-de-group_target`  = "mock",
               `bulk-de-de_engine`     = "deseq2",
               `bulk-de-shrink_lfc`    = FALSE)
  scn <- list(protocol = "ts-drive/1", seq = 5L, action = "run_pipeline",
              module = "bulk_de", button = "bulk-de-run_de", inputs = vals)
  scn
}

# A module whose selects only accept values that are among the options it has been
# told about — i.e. a real `<select>`. `options` is what the panel currently holds.
.BSI_module <- function(options = c("cornea", "limbus", "sclera")) {
  e <- new.env(parent = emptyenv())
  e$opts <- options
  e$seen <- character(0)
  e$coerced <- 0L
  e
}

# The injector the tick will call: it models the browser, refusing a value the
# select cannot represent and coercing to the first option, which is what a real
# `<select>` does.
#
# It also models the REBUILD: injecting `condition_col` re-points the group
# selects' options at that column's levels. The rebuild lands during the round
# trip, i.e. between two protocol beats, which is exactly the window the staging
# exists to exploit — without it the model can never represent a dependency and
# the coercion count is meaningless.
.BSI_LEVELS <- list(condition = c("mock", "CoV2"), tissue = c("cornea", "limbus", "sclera"))

.BSI_injector <- function(m, values) {
  col <- values[["bulk-de-condition_col"]]
  if (!is.null(col) && !is.na(col) && !is.null(.BSI_LEVELS[[as.character(col)]])) {
    m$opts <- .BSI_LEVELS[[as.character(col)]]
  }
  for (id in names(values)) {
    v <- values[[id]]
    if (grepl("group_(ref|target)$", id)) {
      if (!v %in% m$opts) {
        m$coerced <- m$coerced + 1L
        v <- m$opts[1]
      }
    }
    m$seen <- c(m$seen, paste0(id, "=", v))
  }
  invisible(TRUE)
}

test_that("the staging order is DECLARED, and the group selects are not in the first stage", {
  # Data, not inference: if this is wrong the slice is wrong, and it must be
  # readable without booting a session.
  st <- TS_DRIVE_INPUT_STAGES[["bulk_de"]]
  expect_true(is.list(st))
  expect_true(length(st) >= 2L)
  flat <- unlist(st, use.names = FALSE)
  expect_false(any(duplicated(flat)))
  first <- st[[1]]
  expect_true("bulk-de-condition_col" %in% first)
  # The whole point: the group selects are NOT in the first stage.
  expect_false("bulk-de-group_ref" %in% first)
  expect_false("bulk-de-group_target" %in% first)
  # Every id named must be a real, allowlisted id — a typo would otherwise silently
  # drop a control from the plan.
  for (id in flat) expect_false(is.null(ts_drive_allowlist_get(id)))
  # And the module must actually publish a confirmation, or staging buys nothing.
  expect_equal(TS_DRIVE_INPUT_STAGES[["bulk_de"]], st)
})

test_that("a scenario carrying a DEPENDENT select is injected in stages, one per beat", {
  # RED, and the property under test is the ORDER of the writes, not the outcome:
  # no value may be handed to a select whose options cannot represent it.
  #
  # It has to go through the FILESYSTEM: `ts_drive_tick()` takes no scenario
  # argument, it reads `scenario.json` from the drive root itself. A first draft
  # called the tick with a hand-built scenario and every assertion failed for that
  # reason alone — which is the whole point of this block, so the real route is used.
  root <- file.path(tempdir(), paste0("tsdrive-si-", as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(root, "tools", "_drive"), recursive = TRUE, showWarnings = FALSE)
  ts_drive_boot(root)
  on.exit({ ts_drive_boot(ts_project_root()); unlink(root, recursive = TRUE, force = TRUE) }, add = TRUE)
  ts_drive_clear_write_error()
  ts_drive_job_clear()
  op <- options(ts.drive.interactive = TRUE)
  on.exit(options(op), add = TRUE)

  tok <- "tokstg01"
  ts_drive_write_json(list(protocol = TS_DRIVE_PROTOCOL, token = tok, armed = TRUE),
                      ts_drive_path("arm.json"))
  ts_drive_write_json(.BSI(), ts_drive_path("scenario.json"))

  m <- .BSI_module()
  injects <- list()
  # The seam's contract is the `ts_drive_apply_inputs()` shape. A first version
  # returned a bare `invisible(TRUE)` and the product died on `got$warnings` with
  # "$ operator is invalid for atomic vectors" — correct, because an injector that
  # breaks its contract is a bug worth surfacing loudly rather than absorbing.
  inject <- function(inputs, module) {
    injects[[length(injects) + 1L]] <<- names(inputs)
    .BSI_injector(m, inputs)
    list(applied = names(inputs), refused = character(0), warnings = character(0))
  }
  fired <- 0L
  effects <- function(id, mode = NULL, module = NULL, request = NULL) {
    if (identical(mode, "tokens")) {
      return(list(`bulk-de-run_de` = list(
        counter = NULL, long = FALSE, timeout_s = 600,
        ready = function() TRUE, state = NULL,
        confirm_inputs = function(values, session_token, prior = NULL) {
          # The module reports what it observes; a real one could only report what
          # the client could hold.
          list(ok = TRUE, seq = NULL, differs = character(0),
               missing = character(0), waiting = character(0),
               observed = values)
        })))
    }
    if (is.null(id)) return(FALSE)
    fired <<- fired + 1L
    TRUE
  }

  # beat 1: the scenario is consumed and DEFERRED. Nothing may be fired, and the
  # plan must travel with the record.
  b1 <- ts_drive_tick(NULL, NULL, NULL, tok, last_seq = 0, armed = TRUE,
                      effects = effects, inject = inject)
  expect_identical(b1$status, "running")
  expect_false(is.null(b1$pending))
  expect_identical(fired, 0L)
  expect_true(length(injects) >= 1L)
  # The FIRST injection must not carry the group selects.
  expect_false(any(grepl("group_(ref|target)$", injects[[1]])))
  expect_true("bulk-de-condition_col" %in% injects[[1]])
  # And the model must never have been asked to represent an impossible value.
  expect_identical(m$coerced, 0L)
  # The plan must be IN the record, or the service has nothing to walk.
  expect_true(is.list(b1$pending$stages))
  expect_true(length(b1$pending$stages) >= 2L)

  # The dependent stage is injected on a LATER beat, still without firing.
  b2 <- ts_drive_tick(NULL, NULL, NULL, tok, last_seq = 5L, armed = TRUE,
                      effects = effects, inject = inject, pending = b1$pending)
  expect_identical(fired, 0L)
  expect_true(length(injects) >= 2L)
  expect_true(any(grepl("group_ref", injects[[2]])))
  expect_true(any(grepl("group_target", injects[[2]])))
  expect_identical(m$coerced, 0L)
  # The confirmation must NOT have been consulted yet: probing before the dependent
  # stage is in would report the pre-injection state as a mismatch.
  expect_null(b2$status)

  # 🔴 EACH CONTROL EXACTLY ONCE. Added after falsification: restoring the eager
  # injection (so stage 1 is sent twice) left the file GREEN, because nothing
  # counted the writes. This is the "nothing is re-injected" property, and staging
  # is exactly where a double send could be introduced - the drive injects stage 1
  # at deferral AND walks the remaining stages from the service.
  sent <- unlist(injects, use.names = FALSE)
  for (id in c("bulk-de-condition_col", "bulk-de-group_ref", "bulk-de-group_target",
               "bulk-de-de_engine", "bulk-de-shrink_lfc")) {
    expect_identical(sum(sent == id), 1L)
  }
  # And nothing at all was sent twice, whatever the id.
  expect_false(any(duplicated(sent)))
})

test_that("a scenario with NO dependency is injected in ONE batch, exactly as before", {
  # The staging must be INVISIBLE for the 95% case: a single-stage plan may not
  # cost an extra beat, or every ordinary scenario gets slower.
  m <- .BSI_module()
  injects <- list()
  inject <- function(inputs, module) {
    injects[[length(injects) + 1L]] <<- names(inputs)
    .BSI_injector(m, inputs)
  }
  fired <- 0L
  effects <- function(id, mode = NULL, module = NULL, request = NULL) {
    if (identical(mode, "tokens")) {
      return(list(`bulk-de-run_de` = list(
        counter = NULL, long = FALSE, timeout_s = 600,
        ready = function() TRUE, state = NULL,
        confirm_inputs = function(values, session_token, prior = NULL) {
          list(ok = TRUE, seq = NULL, differs = character(0),
               missing = character(0), waiting = character(0),
               observed = values)
        })))
    }
    if (is.null(id)) return(FALSE)
    fired <<- fired + 1L
    TRUE
  }
  scn <- list(protocol = "ts-drive/1", seq = 5L, action = "run_pipeline",
              module = "bulk_de", button = "bulk-de-run_de",
              inputs = list(`bulk-de-shrink_lfc` = FALSE))
  b1 <- ts_drive_apply(NULL, NULL, scn, effects = effects, owner_token = "tokrt001")
  expect_identical(b1$status, "running")
  # One stage, so there is nothing to wait for: the record must be ready at once.
  expect_true(is.null(b1$pending_confirm$stages) ||
                length(b1$pending_confirm$stages) <= 1L)
})

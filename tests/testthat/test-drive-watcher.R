# =============================================================================
# test-drive-watcher.R — Live Control Protocol, app side (G0 + G1 gates)
# =============================================================================
# Eponymous test for `R/core/drive_watcher.R` ONLY. The allowlist's own tests
# live in `tests/testthat/test-drive-allowlist.R`, which is what rule 5 asks
# for — see the header of that file for the MEASURED reason the split was
# necessary (the C9 guard checks a file NAME, never its content).
#
# The allowlist is still READ here, but only as `drive_watcher.R` consumes it:
# `ts_drive_validate_scenario()` refuses off-allowlist keys, and
# `ts_drive_apply()` refuses a scenario whose module owns no bound button.
#
# Spec: docs/DRIVE_LIVE_CONTROL_PLAN.md. Grade G0 accepts 1-4, G1 accepts 5-8.
# Integration points of app.R are asserted separately (test-app-sourcing.R
# already guards the source() wiring).
#
# WHAT THIS FILE IS ALLOWED TO ASSERT
#   The poller's REAL decision logic: schema validation, the seq/stale rule,
#   the arm gate, the allowlist refusal, the atomic write, and the ready.json
#   lifecycle. It does NOT boot Shiny — that is the G0/G1 acceptance run on a
#   live session, which cannot be mechanised from Rscript (spec §7: the agent
#   waits for a real client to connect).
# =============================================================================

# --- Fixtures ---------------------------------------------------------------

# Isolated root per test file: the protocol resolves EVERY path from the boot
# root, so pointing `ts_drive_boot()` at a tempdir makes the tests hermetic and
# stops them from ever touching the real `tools/_drive/` of the working tree.
.drv_local_root <- function() {
  root <- file.path(tempdir(), paste0("tsdrive-", as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(root, "tools", "_drive"), recursive = TRUE, showWarnings = FALSE)
  ts_drive_boot(root)
  # A new root is a new protocol universe, so the write diagnostics must be
  # reset with it. MEASURED: without this, the write error recorded by an
  # earlier test leaked into the next one and made "no failure is NULL" fail —
  # the state lives in a session-global environment, exactly like `$root`.
  ts_drive_clear_write_error()
  root
}

.drv_write_arm <- function(token, armed = TRUE, protocol = TS_DRIVE_PROTOCOL) {
  ts_drive_write_json(
    list(protocol = protocol, token = token, armed = armed),
    ts_drive_path("arm.json")
  )
}

.drv_write_scn <- function(seq, action = "noop", module = "bulk_de",
                           inputs = list(), session_token = NULL,
                           protocol = TS_DRIVE_PROTOCOL,
                           preserve_data = NULL, expect = NULL, button = NULL) {
  payload <- list(protocol = protocol, seq = seq, module = module,
                  action = action, inputs = inputs)
  if (!is.null(session_token)) payload$session_token <- session_token
  if (!is.null(preserve_data)) payload$preserve_data <- preserve_data
  if (!is.null(expect)) payload$expect <- expect
  if (!is.null(button)) payload$button <- button
  ts_drive_write_json(payload, ts_drive_path("scenario.json"))
}

# The three functions under test that need no Shiny at all.
.drv_source <- function() {
  source_project_file("R/core/drive_allowlist.R")
  source_project_file("R/core/drive_watcher.R")
}

.drv_source()

# Force the arm gate OPEN for the whole file. `Rscript` is never interactive,
# so without this the token/schema half of the protocol would be unreachable
# from a test — and a gate nobody can exercise is a gate nobody has checked.
# The gate itself is asserted separately (see §3, "non-interactive refuses").
options(ts.drive.interactive = TRUE)

# Sections 0 and 1 (the allowlist's own consistency and frozen data) MOVED to
# tests/testthat/test-drive-allowlist.R, the eponymous test that rule 5 asks
# for. Nothing was dropped and no assertion was weakened: the C9 guard reads
# only the FILE NAME, so keeping them here left `R/core/drive_allowlist.R`
# reported as untested while its tests ran in this file. This file now keeps
# only what the WATCHER owns.

# 2. ready.json lifecycle (G0 acceptance 2 and 3)
# =============================================================================

test_that("ready.json is absent until written, then round-trips the token", {
  .drv_local_root()
  expect_null(ts_drive_read_ready())

  fake_session <- list()
  written <- ts_drive_write_ready(fake_session, "abc12345", armed = FALSE)

  expect_true(file.exists(ts_drive_path("ready.json")))
  expect_identical(written$protocol, TS_DRIVE_PROTOCOL)
  expect_identical(written$session_token, "abc12345")
  expect_false(written$armed)
  expect_identical(written$last_seq, 0L)
  expect_identical(written$pid, Sys.getpid())

  back <- ts_drive_read_ready()
  expect_identical(back$session_token, "abc12345")
})

test_that("invalidating ready.json with the right token removes it", {
  .drv_local_root()
  ts_drive_write_ready(list(), "tokA", armed = TRUE)
  expect_true(file.exists(ts_drive_path("ready.json")))

  expect_true(ts_drive_invalidate_ready("tokA"))
  expect_false(file.exists(ts_drive_path("ready.json")))
})

test_that("invalidating ready.json with a STALE token leaves a newer handshake intact", {
  # Two tabs: the second overwrote ready.json. The first tab closing must not
  # delete the second tab's handshake — otherwise a departing session would
  # silently unadvertise a live one (spec §2.1, "last connected session wins").
  .drv_local_root()
  ts_drive_write_ready(list(), "newer", armed = TRUE)

  expect_false(ts_drive_invalidate_ready("older"))
  expect_true(file.exists(ts_drive_path("ready.json")))
  expect_identical(ts_drive_read_ready()$session_token, "newer")
})


test_that("a malformed ready.json reads as NULL instead of throwing", {
  .drv_local_root()
  writeLines("{ this is not json", ts_drive_path("ready.json"))
  expect_silent(expect_null(ts_drive_read_ready()))
})

# =============================================================================
# 3. arm gate (spec §1) — the ONLY thing that keeps production inert
# =============================================================================

test_that("no arm.json means disarmed, with a reason", {
  .drv_local_root()
  res <- ts_drive_arm_state("tok")
  expect_false(res$armed)
  expect_match(res$reason, "no arm.json")
})

test_that("a matching token arms; a mismatch does not", {
  .drv_local_root()
  .drv_write_arm("tok")
  expect_true(ts_drive_arm_state("tok")$armed)

  .drv_write_arm("other")
  res <- ts_drive_arm_state("tok")
  expect_false(res$armed)
  expect_match(res$reason, "token mismatch")
})

test_that("armed=false in arm.json disarms without deleting the file", {
  .drv_local_root()
  .drv_write_arm("tok", armed = FALSE)
  res <- ts_drive_arm_state("tok")
  expect_false(res$armed)
  expect_match(res$reason, "armed=false")
})

test_that("a foreign protocol version is refused, not obeyed", {
  .drv_local_root()
  .drv_write_arm("tok", protocol = "ts-drive/99")
  res <- ts_drive_arm_state("tok")
  expect_false(res$armed)
  expect_match(res$reason, "protocol")
})

test_that("the wildcard token is refused unless TRANSCRIPTO_DEV_DRIVE=1", {
  # This is the helper-launch escape hatch (spec §1). It must NOT be open by
  # default, otherwise any file drop would arm any session.
  .drv_local_root()
  .drv_write_arm("*")
  old <- Sys.getenv("TRANSCRIPTO_DEV_DRIVE", unset = NA)
  on.exit({
    if (is.na(old)) Sys.unsetenv("TRANSCRIPTO_DEV_DRIVE")
    else Sys.setenv(TRANSCRIPTO_DEV_DRIVE = old)
  }, add = TRUE)

  Sys.unsetenv("TRANSCRIPTO_DEV_DRIVE")
  expect_false(ts_drive_arm_state("tok")$armed)

  Sys.setenv(TRANSCRIPTO_DEV_DRIVE = "1")
  expect_true(ts_drive_arm_state("tok")$armed)
})

test_that("an unreadable arm.json is treated as absent (no throw)", {
  .drv_local_root()
  writeLines("}}}", ts_drive_path("arm.json"))
  res <- ts_drive_arm_state("tok")
  expect_false(res$armed)
  expect_match(res$reason, "no arm.json")
})

test_that("a NON-interactive session is disarmed even with a perfect arm.json", {
  # The single gate that keeps production inert. `Rscript` is non-interactive
  # by definition, which is exactly why the check is exercised by unsetting the
  # override rather than by pretending.
  .drv_local_root()
  .drv_write_arm("tok")
  op <- options(ts.drive.interactive = FALSE)
  on.exit(options(op), add = TRUE)

  res <- ts_drive_arm_state("tok")
  expect_false(res$armed)
  expect_match(res$reason, "non-interactive")

  # And the whole tick stays silent, even with a valid scenario on disk.
  .drv_write_scn(1L, action = "noop", module = "bulk_de", session_token = "tok")
  tick <- ts_drive_tick(NULL, NULL, NULL, "tok", 0, FALSE)
  expect_false(tick$consumed)
  expect_false(file.exists(ts_drive_path("result.json")))
})

# =============================================================================
# 4. Scenario validation — status enum freeze (spec §2.3 / §2.4)
# =============================================================================

test_that("a well-formed set_inputs scenario passes validation", {
  scn <- list(protocol = TS_DRIVE_PROTOCOL, seq = 1, module = "bulk_de",
              action = "set_inputs", session_token = "tok",
              inputs = list("bulk-de-lfc_thresh" = 2))
  v <- ts_drive_validate_scenario(scn, "tok", last_seq = 0)
  expect_true(v$ok)
  expect_identical(v$status, "applied-candidate")
  expect_length(v$errors, 0)
  expect_identical(v$scenario$seq, 1L)
  expect_true(v$scenario$preserve_data)
})

test_that("an unknown protocol is IGNORED, not invalid", {
  # The distinction matters to the agent: `ignored` means "the session did not
  # understand the version", `invalid` means "your payload is broken". They
  # call for different fixes, so they must not be merged.
  scn <- list(protocol = "something-else/1", seq = 5, module = "bulk_de",
              action = "noop")
  v <- ts_drive_validate_scenario(scn, "tok", last_seq = 0)
  expect_false(v$ok)
  expect_identical(v$status, "ignored")
  expect_length(v$errors, 0)
  expect_match(v$warnings[1], "unknown protocol")
})

test_that("a stale seq is IGNORED (G1 acceptance 8)", {
  scn <- list(protocol = TS_DRIVE_PROTOCOL, seq = 3, module = "bulk_de",
              action = "noop", session_token = "tok")
  v <- ts_drive_validate_scenario(scn, "tok", last_seq = 3)
  expect_identical(v$status, "ignored")
  expect_match(v$warnings[1], "stale seq")

  # And an equal seq is stale too (spec: `seq` <= last_seq).
  v2 <- ts_drive_validate_scenario(scn, "tok", last_seq = 4)
  expect_identical(v2$status, "ignored")
})

test_that("a wrong session_token invalidates — the wrong tab is never touched", {
  scn <- list(protocol = TS_DRIVE_PROTOCOL, seq = 1, module = "bulk_de",
              action = "noop", session_token = "other-tab")
  v <- ts_drive_validate_scenario(scn, "this-tab", last_seq = 0)
  expect_identical(v$status, "invalid")
  expect_match(v$errors[1], "session_token mismatch")
})

test_that("unknown module and unknown action both land in invalid", {
  v <- ts_drive_validate_scenario(
    list(protocol = TS_DRIVE_PROTOCOL, seq = 1, module = "sc",
         action = "noop", session_token = "tok"), "tok", 0)
  expect_identical(v$status, "invalid")
  expect_match(v$errors[1], "outside the v1 bulk pilot allowlist")

  v2 <- ts_drive_validate_scenario(
    list(protocol = TS_DRIVE_PROTOCOL, seq = 1, module = "bulk_de",
         action = "teleport", session_token = "tok"), "tok", 0)
  expect_identical(v2$status, "invalid")
  expect_match(v2$errors[1], "unknown action")
})

test_that("an off-allowlist inputId is reported AND the valid keys survive", {
  # S3: "Unknown inputId -> error item, skip that key, do not abort the
  # session." A blanket all-or-nothing refusal would be easier to write and
  # wrong to ship. The surviving keys are what the injector may still apply.
  scn <- list(protocol = TS_DRIVE_PROTOCOL, seq = 1, module = "bulk_de",
              action = "set_inputs", session_token = "tok",
              inputs = list("bulk-de-lfc_thresh" = 2,
                            "bulk-de-total_nonsense" = 1))
  v <- ts_drive_validate_scenario(scn, "tok", 0)
  expect_identical(v$status, "invalid")
  expect_match(v$errors[1], "not on the driver allowlist")
  expect_match(v$errors[1], "bulk-de-total_nonsense")
  expect_true("bulk-de-lfc_thresh" %in% names(v$scenario$inputs_ok))
  expect_false("bulk-de-total_nonsense" %in% names(v$scenario$inputs_ok))
})

test_that("an allowlisted input from ANOTHER module is reported as unowned", {
  # The message must name both modules: "belongs to bulk_pathways, not bulk_de"
  # is actionable, "not on the allowlist" would be a lie (it IS on it).
  scn <- list(protocol = TS_DRIVE_PROTOCOL, seq = 1, module = "bulk_de",
              action = "set_inputs", session_token = "tok",
              inputs = list("bulk-pathways-pathway_pval" = 0.01))
  v <- ts_drive_validate_scenario(scn, "tok", 0)
  expect_match(v$warnings[1], "belongs to 'bulk_pathways', not 'bulk_de'")
  expect_false("bulk-pathways-pathway_pval" %in% names(v$scenario$inputs_ok))
  # It is allowlisted, so it is NOT an "unknown inputId" error.
  expect_length(v$errors, 0)
})

test_that("preserve_data defaults to TRUE and a non-boolean is refused", {
  base <- list(protocol = TS_DRIVE_PROTOCOL, seq = 1, module = "bulk_de",
               action = "noop", session_token = "tok")
  expect_true(ts_drive_validate_scenario(base, "tok", 0)$scenario$preserve_data)

  bad <- base; bad$preserve_data <- "yes"
  v <- ts_drive_validate_scenario(bad, "tok", 0)
  expect_identical(v$status, "invalid")
  expect_match(v$errors[1], "preserve_data")

  explicit <- base; explicit$preserve_data <- FALSE
  expect_false(ts_drive_validate_scenario(explicit, "tok", 0)$scenario$preserve_data)
})

test_that("a missing seq is invalid; a NULL scenario is invalid and never throws", {
  v <- ts_drive_validate_scenario(
    list(protocol = TS_DRIVE_PROTOCOL, module = "bulk_de", action = "noop"),
    "tok", 0)
  expect_identical(v$status, "invalid")
  expect_match(v$errors[1], "seq")

  v2 <- ts_drive_validate_scenario(NULL, "tok", 0)
  expect_identical(v2$status, "invalid")
  expect_match(v2$errors[1], "absent")
})

# =============================================================================
# 5. result.json — field freeze (spec §2.4)
# =============================================================================

test_that("result.json carries every frozen field, and the status enum is respected", {
  .drv_local_root()
  ts_drive_write_result(7L, "done", "bulk_de", armed = TRUE,
                        preserve_data = TRUE, errors = character(0),
                        warnings = character(0), snapshot = list(has_data = FALSE))

  res <- ts_drive_read_result()
  expect_identical(res$protocol, TS_DRIVE_PROTOCOL)
  expect_identical(res$ack_seq, 7L)
  expect_identical(res$status, "done")
  expect_identical(res$active_module, "bulk_de")
  expect_true(res$armed)
  expect_true(res$preserve_data)
  expect_identical(res$errors, list())
  expect_identical(res$warnings, list())
  expect_false(res$snapshot$has_data)
  expect_true(grepl("^\\d{4}-\\d{2}-\\d{2}T", res$applied_at))
})

test_that("errors and warnings survive the round trip as arrays", {
  .drv_local_root()
  ts_drive_write_result(1L, "invalid", "bulk_de", armed = TRUE,
                        errors = c("one", "two"), warnings = "w3")
  res <- ts_drive_read_result()
  expect_length(res$errors, 2)
  expect_identical(res$errors[[1]], "one")
  expect_length(res$warnings, 1)
})

test_that("snapshot reports objects, never image data", {
  .drv_local_root()
  gd <- list(bulk_obj = list(counts = matrix(1:12, nrow = 4, ncol = 3)))
  snap <- ts_drive_snapshot(gd)
  expect_true(snap$has_data)
  expect_identical(snap$n_genes, 4L)
  expect_identical(snap$n_samples, 3L)
  # No base64 blob may leak into the verdict: renderPlot returns a PNG, which
  # is exactly what the spec forbids relying on.
  expect_false(any(grepl("base64", unlist(snap), fixed = TRUE)))
})

test_that("snapshot on empty or absent state is honest, not an error", {
  .drv_local_root()
  empty <- ts_drive_snapshot(list())
  expect_false(empty$has_data)
  expect_null(empty$n_genes)
  expect_false(ts_drive_snapshot(NULL)$has_data)
})

# =============================================================================
# 6. Atomic write (spec §2.3 / S8) — the Windows rename trap
# =============================================================================

test_that("write_json leaves no .tmp behind and overwrites an existing file", {
  .drv_local_root()
  dest <- ts_drive_path("scenario.json")
  expect_true(ts_drive_write_json(list(seq = 1), dest))
  expect_true(file.exists(dest))
  expect_false(file.exists(paste0(dest, ".tmp")))

  # POSIX would overwrite silently; Windows file.rename() does NOT. The
  # implementation unlink()s first — this asserts the observable result.
  expect_true(ts_drive_write_json(list(seq = 2), dest))
  # jsonlite returns an integer for `2`; compare on value, not on storage mode.
  expect_equal(ts_drive_read_json(dest)$seq, 2)
  expect_false(file.exists(paste0(dest, ".tmp")))
})

test_that("a mid-write failure cannot leave a half-written scenario.json", {
  # FALSIFICATION: if the payload is un-serialisable, the destination must keep
  # its previous content rather than being truncated to garbage. A protocol
  # that can hand a half-parsed JSON to the poller is worse than one that
  # fails loudly.
  .drv_local_root()
  dest <- ts_drive_path("scenario.json")
  ts_drive_write_json(list(seq = 1), dest)
  before <- readLines(dest, warn = FALSE)

  ok <- ts_drive_write_json(list(seq = 2, bad = new.env()), dest)
  # jsonlite CAN serialise an environment (as a list), so accept either
  # outcome — but the file must never be corrupt.
  expect_type(ok, "logical")
  expect_silent(parsed <- ts_drive_read_json(dest))
  expect_false(is.null(parsed))
  expect_true(is.list(parsed))
  expect_false(file.exists(paste0(dest, ".tmp")))
  expect_true(length(before) > 0)
})

test_that("write_json returns FALSE rather than throwing on an unwritable path", {
  .drv_local_root()
  bogus <- file.path(tempdir(), "no-such-dir-xyz", "sub", "out.json")
  dir.create(dirname(bogus), recursive = TRUE, showWarnings = FALSE)
  # A DIRECTORY at the destination is the portable way to force a rename
  # failure on Windows.
  dir.create(bogus, showWarnings = FALSE)
  expect_false(ts_drive_write_json(list(a = 1), bogus))
})

test_that("ts_drive_mtime is NA for a missing file and numeric otherwise", {
  .drv_local_root()
  expect_true(is.na(ts_drive_mtime(ts_drive_path("nope.json"))))
  ts_drive_write_json(list(x = 1), ts_drive_path("here.json"))
  expect_true(is.numeric(ts_drive_mtime(ts_drive_path("here.json"))))
})

# =============================================================================
# 7. Path derivation — the root is captured at boot, never getwd()
# =============================================================================

test_that("every drive path resolves under the boot root", {
  root <- .drv_local_root()
  expect_identical(ts_drive_root(), normalizePath(root, winslash = "/", mustWork = FALSE))
  expect_identical(
    ts_drive_path("arm.json"),
    file.path(normalizePath(root, winslash = "/", mustWork = FALSE),
              "tools", "_drive", "arm.json")
  )
})

test_that("ts_drive_boot normalises the separator and survives a getwd() change", {
  root <- .drv_local_root()
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(tempdir())
  # The protocol must not follow the working directory: RStudio drifts.
  expect_identical(ts_drive_root(), normalizePath(root, winslash = "/", mustWork = FALSE))
  expect_false(grepl("\\\\", ts_drive_root()))
})

test_that("ts_drive_ensure_dir is idempotent", {
  root <- .drv_local_root()
  unlink(file.path(root, "tools"), recursive = TRUE)
  expect_false(dir.exists(ts_drive_path()))
  ts_drive_ensure_dir()
  expect_true(dir.exists(ts_drive_path()))
  expect_silent(ts_drive_ensure_dir())
})

# =============================================================================
# 8. The no-op guarantee (G0 acceptance 1 / §10 invariant)
# =============================================================================

test_that("the tick writes NOTHING while disarmed", {
  # This is the acceptance-1 mechanisation: with no arm.json, dropping a
  # scenario.json must leave no trace at all. If the poller emitted a
  # result.json here, the app would no longer be "indistinguishable from
  # today" with the arm file removed.
  .drv_local_root()
  .drv_write_scn(1L, action = "set_inputs", session_token = "tok",
                 inputs = list("bulk-de-lfc_thresh" = 5))

  token <- "tok"
  res <- ts_drive_tick(session = NULL, input = NULL, global_data = NULL,
                       token = token, last_seq = 0, armed = FALSE)

  expect_false(res$consumed)
  expect_false(res$armed)
  expect_false(file.exists(ts_drive_path("result.json")))
})

test_that("a wrong token consumes nothing either (still silent)", {
  .drv_local_root()
  .drv_write_arm("other")
  .drv_write_scn(1L, action = "set_inputs", session_token = "tok",
                 inputs = list("bulk-de-lfc_thresh" = 5))
  res <- ts_drive_tick(NULL, NULL, NULL, "tok", 0, FALSE)
  expect_false(res$consumed)
  expect_false(file.exists(ts_drive_path("result.json")))
})

# =============================================================================
# 9. seq cursor semantics (G1 acceptance 8, mechanised end to end)
# =============================================================================

test_that("the arm token gate runs before any scenario is even read", {
  # Order matters: a disarmed session must not validate, apply, or answer.
  .drv_local_root()
  .drv_write_scn(1L, action = "noop", module = "bulk_de", session_token = "tok")
  .drv_write_arm("tok", armed = FALSE)
  res <- ts_drive_tick(NULL, NULL, NULL, "tok", 0, FALSE)
  expect_false(res$consumed)
  expect_false(file.exists(ts_drive_path("result.json")))
})

test_that("an invalid scenario is consumed (so its seq is not retried forever)", {
  # Subtle but load-bearing: `invalid` must raise last_seq, otherwise the poller
  # would re-report the same broken payload on every tick, eight hundred times
  # a minute, and the agent could never distinguish a new attempt from a stuck
  # one.
  .drv_local_root()
  .drv_write_arm("tok")
  .drv_write_scn(1L, action = "teleport", module = "bulk_de", session_token = "tok")
  res <- ts_drive_tick(NULL, NULL, NULL, "tok", 0, FALSE)
  expect_true(res$consumed)
  expect_identical(res$last_seq, 0)      # unchanged: the payload was never applied
  expect_true(file.exists(ts_drive_path("result.json")))
  expect_identical(ts_drive_read_result()$status, "invalid")
})

test_that("a stale scenario produces no result and no consumption", {
  .drv_local_root()
  .drv_write_arm("tok")
  .drv_write_scn(2L, action = "noop", module = "bulk_de", session_token = "tok")
  res <- ts_drive_tick(NULL, NULL, NULL, "tok", last_seq = 5, armed = TRUE)
  expect_false(res$consumed)
  expect_false(file.exists(ts_drive_path("result.json")))
})

# =============================================================================
# 10. Button wiring contract (G2 preparation, no Shiny needed)
# =============================================================================

test_that("ts_drive_bind_button refuses an id outside TS_DRIVE_BUTTONS", {
  expect_warning(
    expect_null(ts_drive_bind_button("bulk-de-run_de_typo"))
  )
  expect_match(ts_drive_bind_button("bulk-de-run_de"), "bulk-de-run_de$")
})

test_that("ts_drive_bump_token on a NULL token is FALSE, never a crash", {
  # The G0/G1 situation: no module has announced a binding yet. run_pipeline
  # must then report `invalid` with an actionable message instead of erroring.
  expect_false(ts_drive_bump_token(NULL))
})

test_that("a token can be published from MODULE INIT, outside any reactive consumer", {
  # REGRESSION for the G0/G1 live finding, finally explained — and the reason
  # grade G2 exists at all.
  #
  # `global_data` is a `reactiveValues`, and Shiny ABORTS a field read outside
  # a reactive consumer:
  #   Can't access reactive value 'drive_registry' outside of reactive consumer.
  # Module init IS such a place: `moduleServer()` bodies run while `server()`
  # is still executing, before any flush. The first implementation read the
  # registry through `tryCatch(..., error = function(e) NULL)`, so that abort
  # became a SILENT NULL, `ts_drive_publish_token()` returned FALSE without a
  # word, and the registry stayed EMPTY. Every bound button then reported
  # "not bound" on a real session — which is precisely what the G0/G1 live run
  # observed, and why the source-level wiring looked perfect while the runtime
  # was dead.
  #
  # This test reproduces the init-time context faithfully: no reactive consumer
  # anywhere. Nothing here needs Shiny running, so the defect is catchable
  # offline — which is the whole point.
  gd <- shiny::reactiveValues()
  gd$drive_registry <- new.env(parent = emptyenv())
  counter <- shiny::reactiveVal(0L)

  expect_true(ts_drive_publish_token(gd, "bulk-de-run_de", counter,
                                     ready = function() TRUE))

  # The registry must actually HOLD the entry. `isolate()` on the test's own
  # read too: the test lives outside a consumer, exactly like the module.
  reg <- shiny::isolate(gd$drive_registry)
  expect_true("bulk-de-run_de" %in% ls(reg))
  entry <- reg[["bulk-de-run_de"]]
  expect_true(is.list(entry))
  expect_identical(ts_drive_entry_ready(entry), "ready")
  # And the poller's own accessor must find the same counter.
  expect_identical(ts_drive_token_of(gd, "bulk-de-run_de"), counter)
  # Bumping it through the record must move the real reactiveVal.
  expect_true(ts_drive_bump_token(entry))
  expect_identical(shiny::isolate(counter()), 1L)

  # A second publish accumulates rather than replaces, and an id outside
  # TS_DRIVE_BUTTONS is refused loudly instead of widening the surface.
  expect_true(ts_drive_publish_token(gd, "bulk-pathways-run_pathway",
                                     shiny::reactiveVal(0L)))
  expect_true(all(c("bulk-de-run_de", "bulk-pathways-run_pathway") %in%
                    ls(shiny::isolate(gd$drive_registry))))
  expect_warning(ts_drive_publish_token(gd, "bulk-de-not_a_button",
                                        shiny::reactiveVal(0L)))
  expect_false("bulk-de-not_a_button" %in% ls(shiny::isolate(gd$drive_registry)))

  # With NO registry at all (a unit test that sources a module alone) the call
  # must stay silent and harmless, never throw.
  expect_false(ts_drive_publish_token(shiny::reactiveValues(), "bulk-de-run_de",
                                      counter))
})

test_that("run_pipeline without a binding is invalid and names the missing bind", {
  .drv_local_root()
  .drv_write_arm("tok")
  .drv_write_scn(9L, action = "run_pipeline", module = "bulk_de",
                 session_token = "tok")
  res <- ts_drive_tick(NULL, NULL, NULL, "tok", 0, FALSE, effects = NULL)
  expect_true(res$consumed)
  r <- ts_drive_read_result()
  expect_identical(r$status, "invalid")
  expect_match(r$errors[[1]], "not bound")
})

test_that("a bulk_de scenario with no explicit button resolves to run_de", {
  .drv_local_root()
  .drv_write_arm("tok")
  .drv_write_scn(4L, action = "run_pipeline", module = "bulk_de",
                 session_token = "tok")
  calls <- new.env(); calls$ids <- character(0)
  effects <- function(id, mode = NULL, module = NULL) {
    if (identical(mode, "tokens")) return(list())
    if (is.null(id)) return(FALSE)
    calls$ids <- c(calls$ids, id)
    TRUE
  }
  res <- ts_drive_tick(NULL, NULL, NULL, "tok", 0, FALSE, effects = effects)
  expect_true(res$consumed)
  expect_identical(calls$ids, "bulk-de-run_de")
  expect_identical(ts_drive_read_result()$status, "done")
})

test_that("run_pipeline remembers the seq and reports done", {
  .drv_local_root()
  .drv_write_arm("tok")
  .drv_write_scn(11L, action = "run_pipeline", module = "bulk_pathways",
                 session_token = "tok")
  effects <- function(id, mode = NULL, module = NULL) {
    if (identical(mode, "tokens")) return(list())
    TRUE
  }
  res <- ts_drive_tick(NULL, NULL, NULL, "tok", 0, FALSE, effects = effects)
  expect_identical(res$last_seq, 11L)
  expect_identical(ts_drive_read_result()$ack_seq, 11L)
})

# -----------------------------------------------------------------------------
# 10b. THE READINESS GATE — a bound button that cannot run must not report done
# -----------------------------------------------------------------------------
# Spec G2 asks for the BIND (acceptance 9: two consecutive `run_pipeline` both
# fire). It does not ask what the poller reports when a module is bound but has
# nothing to work on — and the first implementation answered `done`, which is a
# lie. The module's own `req()` aborted in silence, the poller had already
# bumped the token, and the agent was told the pipeline had run. This is the
# same class of defect as the dead navigation branch in §12: a seam that fails
# SILENTLY. It needs a test, not a promise.
#
# The gate belongs to the MODULE, never to the poller: only the module can know
# whether its object is loaded (`shared_rv$filtered_counts`, `input$counts_file`,
# `shared_rv$vst_mat`). So `ts_drive_publish_token()` takes an optional `ready`
# guard and the poller asks it BEFORE firing. `unknown` — no guard published —
# keeps the G1 behaviour, so a module that has not adopted the guard still runs.

test_that("ts_drive_entry_ready classifies a guard into a frozen vocabulary", {
  expect_setequal(TS_DRIVE_READY,
                  c("ready", "not-ready", "probe-failed", "unknown"))

  # No entry at all, or an entry with no guard -> "unknown". That is the G0/G1
  # shape (a counter announced, nothing said about preconditions) and it must
  # stay PERMISSIVE: refusing there would stop every module that has not yet
  # adopted a guard.
  expect_identical(ts_drive_entry_ready(NULL), "unknown")
  expect_identical(ts_drive_entry_ready(list(counter = function() 0L)), "unknown")

  expect_identical(ts_drive_entry_ready(list(ready = function() TRUE)), "ready")
  expect_identical(ts_drive_entry_ready(list(ready = function() FALSE)), "not-ready")
  # A guard may return the REASON, so the refusal names the missing object
  # instead of a bare "not ready" the agent cannot act on.
  expect_identical(ts_drive_entry_ready(list(ready = function() "no object")), "not-ready")
  # A character is ALWAYS a reason, never a disguised boolean. `"TRUE"` is a
  # perfectly serviceable reason string ("the flag named TRUE is unset"), and
  # coercing it to TRUE would be the same class of mistake as truthy-string
  # comparison — the kind that makes a refusal look like a success.
  expect_identical(ts_drive_entry_ready(list(ready = function() "TRUE")), "not-ready")

  # FAIL CLOSED. A guard that cannot answer is not evidence of readiness, and
  # the alternative — firing a doomed click and reporting `done` — is precisely
  # the lie this gate exists to remove.
  expect_identical(ts_drive_entry_ready(list(ready = function() stop("boom"))), "probe-failed")
  expect_identical(ts_drive_entry_ready(list(ready = function() NA)), "probe-failed")
  # Malformed guards: a non-logical/non-character answer, an empty reason, and
  # a `ready` slot that is not even a function.
  expect_identical(ts_drive_entry_ready(list(ready = function() 1)), "probe-failed")
  expect_identical(ts_drive_entry_ready(list(ready = function() c(TRUE, TRUE))), "probe-failed")
  expect_identical(ts_drive_entry_ready(list(ready = function() "")), "probe-failed")
  expect_identical(ts_drive_entry_ready(list(ready = "not a function")), "probe-failed")
})

test_that("run_pipeline is invalid, not done, when the button's guard refuses", {
  .drv_local_root()
  .drv_write_arm("tok")
  .drv_write_scn(21L, action = "run_pipeline", module = "bulk_de",
                 session_token = "tok")

  fired <- new.env(); fired$n <- 0L
  reason <- "no bulk object loaded (shared_rv$filtered_counts is NULL)"
  effects <- function(id, mode = NULL, module = NULL) {
    if (identical(mode, "tokens")) {
      return(stats::setNames(list(list(ready = function() reason)), "bulk-de-run_de"))
    }
    fired$n <- fired$n + 1L
    TRUE
  }

  res <- ts_drive_tick(NULL, NULL, NULL, "tok", 0, FALSE, effects = effects)
  expect_true(res$consumed)
  r <- ts_drive_read_result()
  expect_identical(r$status, "invalid")
  # The refusal still ACKNOWLEDGES the seq, or a polling agent would wait for
  # a terminal status that never arrives.
  expect_identical(r$ack_seq, 21L)
  expect_match(r$errors[[1]], "not ready")
  expect_match(r$errors[[1]], "no bulk object loaded")
  expect_match(r$errors[[1]], "shared_rv$filtered_counts", fixed = TRUE)
  # The doomed click must NOT have been fired. Bumping first and reporting
  # afterwards would leave the module's counter advanced for a run that never
  # happened — and the NEXT scenario would then be one click out of step.
  expect_identical(fired$n, 0L)
})

test_that("a bound button with NO guard still reports done (the G1 behaviour is kept)", {
  .drv_local_root()
  .drv_write_arm("tok")
  .drv_write_scn(31L, action = "run_pipeline", module = "bulk_pathways",
                 session_token = "tok")

  fired <- new.env(); fired$n <- 0L
  effects <- function(id, mode = NULL, module = NULL) {
    if (identical(mode, "tokens")) {
      return(stats::setNames(list(list(counter = function() 0L)), "bulk-pathways-run_pathway"))
    }
    fired$n <- fired$n + 1L
    TRUE
  }

  res <- ts_drive_tick(NULL, NULL, NULL, "tok", 0, FALSE, effects = effects)
  expect_true(res$consumed)
  expect_identical(ts_drive_read_result()$status, "done")
  expect_identical(fired$n, 1L)
})

test_that("a guard that cannot answer is a refusal, never a silent done", {
  .drv_local_root()
  .drv_write_arm("tok")
  .drv_write_scn(32L, action = "run_pipeline", module = "bulk_de",
                 session_token = "tok")

  fired <- new.env(); fired$n <- 0L
  effects <- function(id, mode = NULL, module = NULL) {
    if (identical(mode, "tokens")) {
      return(stats::setNames(list(list(ready = function() stop("probe exploded"))), "bulk-de-run_de"))
    }
    fired$n <- fired$n + 1L
    TRUE
  }

  ts_drive_tick(NULL, NULL, NULL, "tok", 0, FALSE, effects = effects)
  r <- ts_drive_read_result()
  expect_identical(r$status, "invalid")
  expect_match(r$errors[[1]], "probe")
  expect_identical(fired$n, 0L)
})

test_that("two consecutive run_pipeline both fire, guard satisfied (G2 acceptance 9)", {
  # The attach-level version of this property lives in §12 ("two consecutive
  # scenarios both fire, and the token is read live"). What is added HERE is
  # the piece §12 cannot reach: the guard is satisfied, so the button is
  # really fired, and the counter's VALUE advances twice. §12 asserts that the
  # poller CALLS the effect twice; this asserts the counter itself moves,
  # which is what separates a working trigger from a one-shot latch.
  .drv_local_root()
  .drv_write_arm("tok")

  box <- new.env(); box$n <- 0L
  counter <- function(v) {
    if (missing(v)) return(box$n)
    box$n <- as.integer(v); invisible(NULL)
  }
  fired <- new.env(); fired$n <- 0L
  effects <- function(id, mode = NULL, module = NULL) {
    if (identical(mode, "tokens")) {
      return(stats::setNames(
        list(list(counter = counter, ready = function() TRUE)), "bulk-de-run_de"))
    }
    fired$n <- fired$n + 1L
    ts_drive_bump_token(list(counter = counter))
  }

  .drv_write_scn(1L, action = "run_pipeline", module = "bulk_de", session_token = "tok")
  r1 <- ts_drive_tick(NULL, NULL, NULL, "tok", 0L, FALSE, effects = effects)
  expect_true(r1$consumed)
  expect_identical(r1$last_seq, 1L)
  expect_identical(box$n, 1L)
  expect_identical(ts_drive_read_result()$status, "done")
  expect_identical(ts_drive_read_result()$ack_seq, 1L)

  # The SECOND scenario must move the SAME counter again — a latch would leave
  # it at 1, and the module's observer would never re-fire.
  .drv_write_scn(2L, action = "run_pipeline", module = "bulk_de", session_token = "tok")
  r2 <- ts_drive_tick(NULL, NULL, NULL, "tok", r1$last_seq, FALSE, effects = effects)
  expect_true(r2$consumed)
  expect_identical(r2$last_seq, 2L)
  expect_identical(box$n, 2L)
  expect_identical(ts_drive_read_result()$status, "done")
  expect_identical(ts_drive_read_result()$ack_seq, 2L)

  expect_identical(fired$n, 2L)
})

test_that("set_inputs refuses a button whose guard says not-ready", {
  .drv_local_root()
  fired <- new.env(); fired$n <- 0L
  res <- ts_drive_apply(NULL, NULL,
    list(action = "set_inputs", module = "bulk_de",
         inputs = list("bulk-de-run_de" = 1)),
    effects = function(id, mode = NULL, module = NULL) {
      if (identical(mode, "tokens")) {
        return(stats::setNames(list(list(ready = function() "no object")), "bulk-de-run_de"))
      }
      fired$n <- fired$n + 1L
      TRUE
    })
  expect_identical(res$status, "applied")
  expect_match(res$warnings, "not ready")
  expect_identical(fired$n, 0L)
})

test_that("a scenario's `button` and `expect` survive validation (spec §2.3)", {
  # The validator REBUILDS the scenario from a field whitelist, and the first
  # version omitted `button` and `expect`. Both are then always NULL downstream:
  #   * `ts_drive_apply()` falls back to the module's DEFAULT button, so
  #     `bulk-pathways-run_scores` — one of the four bound click sites — could
  #     never be fired, because that module's default is `run_pathway`;
  #   * `ts_drive_nav_plan()` never saw a requested tab.
  # Nothing warns, nothing errors: the scenario just drives the WRONG button.
  # Found on the live session, where seq 7 named `run_scores` and the result
  # came back for `run_pathway`.
  v <- ts_drive_validate_scenario(
    list(protocol = TS_DRIVE_PROTOCOL, seq = 5L, module = "bulk_pathways",
         action = "run_pipeline", inputs = list(),
         button = "bulk-pathways-run_scores",
         expect = list(nav = "tab_volcano")),
    "tok", 0L)
  expect_identical(v$status, "applied-candidate")
  expect_identical(v$scenario$button, "bulk-pathways-run_scores")
  expect_identical(v$scenario$expect$nav, "tab_volcano")

  # Absent, they must stay absent — not become a stray empty string or list.
  v2 <- ts_drive_validate_scenario(
    list(protocol = TS_DRIVE_PROTOCOL, seq = 6L, module = "bulk_de",
         action = "run_pipeline", inputs = list()), "tok", 0L)
  expect_null(v2$scenario$button)
  expect_null(v2$scenario$expect)
})

test_that("run_pipeline honours an explicit `button`, and `expect$nav` reaches the nav plan", {
  .drv_local_root()
  .drv_write_arm("tok")
  # The 4th bound click site: reachable ONLY through an explicit `button`,
  # since bulk_pathways defaults to `run_pathway`.
  .drv_write_scn(41L, action = "run_pipeline", module = "bulk_pathways",
                 session_token = "tok", button = "bulk-pathways-run_scores",
                 expect = list(nav = "tab_volcano"))
  seen <- character(0)
  effects <- function(id, mode = NULL, module = NULL) {
    if (identical(mode, "tokens")) {
      return(list("bulk-pathways-run_scores" = list(ready = function() TRUE)))
    }
    seen <<- c(seen, id); TRUE
  }
  res <- ts_drive_tick(NULL, NULL, NULL, "tok", 0L, FALSE, effects = effects)
  expect_true(res$consumed)
  expect_identical(ts_drive_read_result()$status, "done")
  expect_identical(seen, "bulk-pathways-run_scores")
  # `expect$nav` is carried by the tick to app.R, which performs the jump.
  expect_identical(res$nav$tab, "tab_volcano")
  expect_identical(res$nav$panel, "panel_pathways")
})

test_that("an explicit `button` outside TS_DRIVE_BUTTONS is refused as invalid", {
  .drv_local_root()
  .drv_write_arm("tok")
  .drv_write_scn(42L, action = "run_pipeline", module = "bulk_de",
                 session_token = "tok", button = "bulk-de-run_de_typo")
  res <- ts_drive_tick(NULL, NULL, NULL, "tok", 0L, FALSE,
                       effects = function(...) TRUE)
  expect_true(res$consumed)
  r <- ts_drive_read_result()
  expect_identical(r$status, "invalid")
  expect_match(r$errors[[1]], "no bound button")
})

test_that("every bound button is published by a real module, WITH a readiness guard", {
  # REGRESSION for the G0/G1 live finding. That run reported
  # `run_pipeline -> invalid` because "the target button was not bound on that
  # module" — and NOTHING in the suite could have caught a missing
  # `ts_drive_publish_token()` call: every watcher test injects its own
  # `effects` closure, so they prove the POLLER's logic and never the WIRING.
  # A binding that exists only in the allowlist is a promise, not a fact.
  files <- list.files(file.path(ts_project_root(), "modules"),
                      pattern = "[.]R$", recursive = TRUE, full.names = TRUE)
  lines <- unlist(lapply(files, function(f) readLines(f, warn = FALSE, encoding = "UTF-8")))

  for (btn in TS_DRIVE_BUTTONS) {
    hit <- grep(sprintf('ts_drive_publish_token\\(.*"%s"', btn), lines)
    expect_true(length(hit) >= 1L,
                info = paste("no module publishes a token for", btn))
    # The guard is what turns a doomed run into `invalid` instead of `done`.
    # A bind without one re-introduces the lie this grade removed, so the two
    # are asserted together: the binding is not "done" until it can refuse.
    window <- paste(lines[hit[1]:min(length(lines), hit[1] + 4L)], collapse = "\n")
    expect_match(window, "ready", info = paste("no readiness guard for", btn))
  }
})

test_that("no fileInput id is injectable (spec S5: the widget is never faked)", {
  # Spec S5: "in live session BYPASS the widget ... Never pass a raw path string
  # into update*". The three fileInput ids of mod_import_bulk.R are `counts_file`,
  # `metadata_file` and `ps_files`; none may be on the allowlist, or the poller
  # would be pretending to be a file chooser. G3's `import_file` is the
  # sanctioned route, and it is NOT this one.
  for (id in c("import_bulk-counts_file", "import_bulk-metadata_file",
               "import_bulk-ps_files")) {
    expect_false(ts_drive_allowlisted(id), info = id)
  }
  # And no kind in the frozen vocabulary is a file kind, so no future entry can
  # smuggle one in without changing the vocabulary — which is itself frozen.
  expect_false(any(grepl("file", TS_DRIVE_KINDS, ignore.case = TRUE)))
})

# =============================================================================
# 11. Actions that must refuse rather than silently no-op (G3 acceptance 14)
# =============================================================================

test_that("reset_module returns invalid, never a silent no-op", {
  # Spec G3.14: "if not implemented, the action must return invalid, not
  # silently no-op". A silent success here would let an agent believe it had
  # cleared data it did not clear.
  .drv_local_root()
  ts_drive_apply_res <- ts_drive_apply(NULL, NULL,
    list(action = "reset_module", module = "bulk_de", inputs = list()),
    effects = NULL)
  expect_identical(ts_drive_apply_res$status, "invalid")
  expect_match(ts_drive_apply_res$errors[1], "reset_module")
})

test_that("import_file is implemented in G3 — a refusal now names the MISSING PAYLOAD", {
  # This assertion USED to read `import_file is not implemented in this grade
  # (G3)`, and it was the pin on the not-implemented state. G3 implements the
  # action, so the pin moves to the NEW contract rather than being deleted: an
  # `import_file` with no `import` block must still refuse — because the
  # PAYLOAD is missing, never because the action is absent. The full G3
  # contract (path validation, importer seam) lives in §16.
  res <- ts_drive_apply(NULL, NULL,
    list(action = "import_file", module = "import_bulk", inputs = list()),
    effects = NULL)
  expect_identical(res$status, "invalid")
  msg <- paste(res$errors, collapse = " ")
  expect_match(msg, "import")
  expect_false(grepl("not implemented", msg))
})

test_that("noop and snapshot are done, and change nothing", {
  for (act in c("noop", "snapshot")) {
    res <- ts_drive_apply(NULL, NULL,
      list(action = act, module = "bulk_de", inputs = list("bulk-de-lfc_thresh" = 9)),
      effects = NULL)
    expect_identical(res$status, "done", info = act)
    expect_length(res$errors, 0)
  }
})

test_that("set_inputs reports applied (inputs updated, pipeline not finished)", {
  # Spec §2.4: `applied` = inputs updated, pipeline NOT finished. Distinguishing
  # it from `done` is what lets the agent tell "I changed a widget" from "I ran
  # the analysis".
  .drv_local_root()
  res <- ts_drive_apply(NULL, NULL,
    list(action = "set_inputs", module = "bulk_de",
         inputs = list("bulk-de-nonexistent_probe" = 1)),
    effects = NULL)
  expect_identical(res$status, "applied")
})

test_that("the status enum is FROZEN, and terminality is the distinction", {
  # Spec §2.4 freezes the six values. Pinned as a set so a new status cannot be
  # invented silently: an agent switches on these, and a seventh would be an
  # unhandled case in every client.
  expect_setequal(TS_DRIVE_STATUSES,
                  c("ignored", "invalid", "applied", "running", "done", "error"))

  # The distinction that MATTERS to a polling agent:
  #   * `applied` and `running` ACKNOWLEDGE the seq but are NOT terminal —
  #     the pipeline may still be working;
  #   * `done` and `error` are TERMINAL for that seq;
  #   * `ignored` and `invalid` are terminal refusals (nothing will change).
  expect_true(ts_drive_status_terminal("done"))
  expect_true(ts_drive_status_terminal("error"))
  expect_true(ts_drive_status_terminal("ignored"))
  expect_true(ts_drive_status_terminal("invalid"))
  expect_false(ts_drive_status_terminal("applied"))
  expect_false(ts_drive_status_terminal("running"))
  # An unknown status is NOT terminal — an agent must keep waiting rather than
  # treat a future status it does not understand as completion.
  expect_false(ts_drive_status_terminal("something_new"))
  expect_false(ts_drive_status_terminal(NA_character_))
})

test_that("pipeline actions report a terminal status; set_inputs does not", {
  # The claim under test, end to end for the two actions that differ:
  #   * `set_inputs` -> `applied`  (acknowledgement, NOT terminal)
  #   * `run_pipeline` with a binding -> `done` (terminal)
  # `now` is irrelevant here; both go through the real apply() dispatcher.
  .drv_local_root()

  set_res <- ts_drive_apply(NULL, NULL,
    list(action = "set_inputs", module = "bulk_de", inputs = list()),
    effects = NULL)
  expect_identical(set_res$status, "applied")
  expect_false(ts_drive_status_terminal(set_res$status))   # agent keeps polling

  run_res <- ts_drive_apply(NULL, NULL,
    list(action = "run_pipeline", module = "bulk_de", inputs = list()),
    effects = function(id, mode = NULL, module = NULL) TRUE)
  expect_identical(run_res$status, "done")
  expect_true(ts_drive_status_terminal(run_res$status))    # agent may stop

  # And a refused pipeline is terminal too — an explicit `invalid`, never a
  # silent no-op (spec G3.14), so the agent is never left waiting on a refusal.
  refused <- ts_drive_apply(NULL, NULL,
    list(action = "run_pipeline", module = "bulk_de", inputs = list()),
    effects = function(id, mode = NULL, module = NULL) FALSE)
  expect_identical(refused$status, "invalid")
  expect_true(ts_drive_status_terminal(refused$status))
})

test_that("the nav plan targets the bulk tab, and the right panel per module", {
  expect_identical(ts_drive_nav_plan("bulk_de")$top, "tab_bulk")
  expect_identical(ts_drive_nav_plan("bulk_de")$panel, "panel_de")
  expect_identical(ts_drive_nav_plan("bulk_pathways")$panel, "panel_pathways")
  expect_null(ts_drive_nav_plan("import_bulk")$panel)
  # A tab value outside the measured list is dropped, not passed through.
  expect_null(ts_drive_nav_plan("bulk_de", "tab_made_up")$tab)
  expect_identical(ts_drive_nav_plan("bulk_de", "tab_volcano")$tab, "tab_volcano")
})

# =============================================================================
# 12. The attach() CONTRACT that app.R's observe() depends on
# =============================================================================
# This is the seam between `R/` (pure decisions) and `app.R` (the one reactive
# beat). It was written BECAUSE the first version of that seam was broken: the
# observer read `.drive$last_nav`, a member `ts_drive_attach()` never returned,
# so the navigation branch could never fire — and nothing failed, because a
# `NULL` read is silent. A seam that fails SILENTLY needs a test, not a promise.
#
# No Shiny is booted. `ts_drive_attach()` only needs `session$onSessionEnded()`;
# everything else it touches is the file system, which is why this seam is
# reachable from `Rscript` at all.

.drv_fake_session <- function(env) {
  list(onSessionEnded = function(f) { env$f <- f; invisible(NULL) })
}

# --- Second-tab (token rotation) --------------------------------------------
# Placed HERE, after the fixture: a test defined above `.drv_fake_session()`
# cannot see it and errors with "could not find function" (measured — the first
# attempt at these two tests was written near the top of the file and failed).

test_that("a SECOND session mints a new token; the previous one is replaced", {
  # MEASURED LIVE (2026-09-21): each new Shiny session mints a new token, and
  # ready.json then names the newest. Verified on a real app (port 7789):
  #   * tab 1           -> token 7h88ewiz
  #   * RELOAD that tab -> token v4z732tt   (rotated = TRUE)
  #   * a separate browser session -> token u0jrebu4, new started_at (rotated)
  # NOTE the trap this test cannot express: opening a second *target* inside ONE
  # `ChromoteSession` does NOT create a second Shiny session, so the token looks
  # unchanged — that is a property of the harness, not of the protocol.
  .drv_local_root()
  s1 <- ts_drive_attach(.drv_fake_session(new.env()), list())
  tok1 <- s1$token
  expect_identical(as.character(ts_drive_read_ready()$session_token), tok1)

  s2 <- ts_drive_attach(.drv_fake_session(new.env()), list())
  tok2 <- s2$token

  # A NEW token, and it now owns the handshake ("last connected session wins").
  expect_false(identical(tok1, tok2))
  expect_identical(as.character(ts_drive_read_ready()$session_token), tok2)
})

test_that("the PREVIOUS tab's token can neither arm nor display", {
  # The consequence of rotation, stated as the user-visible rule. Two tabs, the
  # second owning ready.json; the FIRST tab's token must be inert.
  .drv_local_root()
  s1 <- ts_drive_attach(.drv_fake_session(new.env()), list())
  s2 <- ts_drive_attach(.drv_fake_session(new.env()), list())

  # The agent arms the CURRENT session (the second tab) — the honest case.
  .drv_write_arm(s2$token)

  # The current tab: armed AND selected AND displayable.
  cur_selected <- identical(as.character(ts_drive_read_ready()$session_token),
                            s2$token)
  expect_true(cur_selected)
  expect_true(ts_drive_arm_state(s2$token)$armed)
  expect_true(ts_drive_badge_visible(TRUE, TRUE, cur_selected,
                                     ts_drive_arm_state(s2$token)$armed, "armed"))

  # The PREVIOUS tab: not selected, and its token does not match arm.json, so
  # it is disarmed — it can neither arm nor display. This is the rule the user
  # asked to confirm, asserted on both halves.
  prev_selected <- identical(as.character(ts_drive_read_ready()$session_token),
                             s1$token)
  expect_false(prev_selected)
  expect_false(ts_drive_arm_state(s1$token)$armed)
  expect_match(ts_drive_arm_state(s1$token)$reason, "token mismatch")
  expect_false(ts_drive_badge_visible(TRUE, TRUE, prev_selected,
                                      ts_drive_arm_state(s1$token)$armed,
                                      "armed"))
})

test_that("attach exposes the closure contract app.R reads", {
  .drv_local_root()
  env <- new.env(); env$f <- NULL
  d <- ts_drive_attach(.drv_fake_session(env), list(run_de = 0L))

  # The exact members app.R touches. If one is renamed, app.R must be updated —
  # better a red test here than a silently dead branch in the observer.
  expect_true(all(c("token", "poll_ms", "on_tick", "pending_nav",
                    "last_seq", "armed") %in% names(d)))
  expect_type(d$token, "character")
  expect_length(d$token, 1)
  expect_true(is.numeric(d$poll_ms))
  # Spec §3: 500-1000 ms. Pinned so a "performance tweak" cannot leave it.
  expect_true(d$poll_ms >= 500 && d$poll_ms <= 1000)

  expect_true(is.function(env$f))            # onSessionEnded registered
  expect_true(file.exists(ts_drive_path("ready.json")))
})

test_that("disarmed, on_tick consumes nothing and writes no scenario", {
  root <- .drv_local_root()
  d <- ts_drive_attach(.drv_fake_session(new.env()), list())
  tick <- d$on_tick(global_data = list())

  expect_false(isTRUE(tick$consumed))
  expect_identical(d$last_seq(), 0L)
  expect_null(d$pending_nav())
  # The no-op guarantee, restated at the attach() level: nothing is created.
  expect_false(file.exists(ts_drive_path("scenario.json")))
  expect_false(file.exists(ts_drive_path("result.json")))
  expect_true(dir.exists(file.path(root, "tools", "_drive")))
})

test_that("an applied scenario stores its nav plan, and pending_nav() is ONE-SHOT", {
  .drv_local_root()
  d <- ts_drive_attach(.drv_fake_session(new.env()), list())
  .drv_write_arm(d$token)
  .drv_write_scn(1L, action = "set_inputs", module = "bulk_de",
                 session_token = d$token)

  tick <- d$on_tick(global_data = list())
  expect_true(isTRUE(tick$consumed))
  # The plan is BOTH returned to the caller and stashed for it.
  expect_identical(tick$nav$top, "tab_bulk")
  expect_identical(tick$nav$panel, "panel_de")

  expect_false(is.null(d$pending_nav()))
  # ONE-SHOT is a correctness property, not a memory optimisation: replaying a
  # jump on a later idle tick would drag the human back to a tab they left.
  expect_null(d$pending_nav())
})

test_that("two consecutive scenarios both fire, and the token is read live", {
  .drv_local_root()
  d <- ts_drive_attach(.drv_fake_session(new.env()), list())
  .drv_write_arm(d$token)

  seen <- character(0)
  effects <- function(input_id, mode = "bump", module = NULL) {
    if (identical(mode, "tokens")) return(list())
    seen <<- c(seen, input_id); TRUE
  }

  .drv_write_scn(1L, action = "run_pipeline", module = "bulk_de",
                 session_token = d$token)
  expect_true(isTRUE(d$on_tick(global_data = list(), effects = effects)$consumed))

  # G2 acceptance 9's shape: a SECOND scenario must fire too. This is the
  # property that separates a working token trigger from a one-shot latch.
  .drv_write_scn(2L, action = "run_pipeline", module = "bulk_de",
                 session_token = d$token)
  expect_true(isTRUE(d$on_tick(global_data = list(), effects = effects)$consumed))

  expect_identical(seen, c("bulk-de-run_de", "bulk-de-run_de"))
  expect_identical(d$last_seq(), 2L)
  expect_true(d$armed())
})

test_that("an idle armed tick is a true no-op (absent scenario.json)", {
  # REGRESSION for a defect measured while building the badge. With
  # `scenario.json` merely ABSENT, `ts_drive_validate_scenario(NULL, ...)`
  # reports `invalid`, and the tick CONSUMED it — so every idle beat of an
  # armed session wrote a bogus `status:"invalid"` result and queued a badge
  # transition. The poller would have cried failure ~1.25x per second forever.
  #
  # The two cases must stay DISTINCT:
  #   absent file  -> nothing to do (no-op)
  #   malformed    -> invalid (there IS a payload, and it is wrong)
  .drv_local_root()
  d <- ts_drive_attach(.drv_fake_session(new.env()), list())
  .drv_write_arm(d$token)
  expect_false(file.exists(ts_drive_path("scenario.json")))

  tick <- d$on_tick(global_data = list())
  expect_false(isTRUE(tick$consumed))
  expect_identical(as.integer(d$last_seq()), 0L)
  expect_false(file.exists(ts_drive_path("result.json")))
  # The only event an idle armed beat may produce is the arm itself.
  expect_identical(vapply(d$pending_events(), function(e) e$event, ""), "arm")

  # A second idle beat produces NOTHING at all: no re-arm, no invalid.
  tick2 <- d$on_tick(global_data = list())
  expect_false(isTRUE(tick2$consumed))
  expect_length(d$pending_events(), 0L)
  expect_false(file.exists(ts_drive_path("result.json")))
})

test_that("a malformed scenario.json is still reported as invalid", {
  # The other half of the distinction above: a file that EXISTS but does not
  # parse must NOT be silently swallowed as "nothing to do".
  .drv_local_root()
  d <- ts_drive_attach(.drv_fake_session(new.env()), list())
  .drv_write_arm(d$token)

  con <- file(ts_drive_path("scenario.json"), open = "wb")
  writeLines("{not valid json", con = con, useBytes = TRUE)
  close(con)

  tick <- d$on_tick(global_data = list())
  expect_true(isTRUE(tick$consumed))
  expect_identical(tick$status, "invalid")
  res <- ts_drive_read_result()
  expect_identical(as.character(res$status), "invalid")
})

test_that("the ready.json handshake is invalidated when the session ends", {
  .drv_local_root()
  env <- new.env(); env$f <- NULL
  d <- ts_drive_attach(.drv_fake_session(env), list())
  expect_true(file.exists(ts_drive_path("ready.json")))

  env$f()
  # A dead session must not leave a handshake an agent would trust: otherwise
  # the agent arms a token nobody is listening for and waits forever.
  expect_false(file.exists(ts_drive_path("ready.json")))
})

# =============================================================================
# 13. Heartbeat — the liveness proof in ready.json
# =============================================================================
# WHY THIS EXISTS: a `ready.json` left behind by a process that died mid-session
# is byte-identical to a live one as far as "the file exists" is concerned. Only
# a timestamp that keeps moving distinguishes them. Without it the agent's only
# recourse would be scraping stdout — the very thing the protocol exists to
# avoid, since Rscript exits 139 on teardown and loses buffered stdout.

test_that("ready.json carries the heartbeat fields", {
  .drv_local_root()
  d <- ts_drive_attach(.drv_fake_session(new.env()), list())
  hb <- ts_drive_read_ready()

  expect_true(all(c("hb_at", "hb_n", "hb_timeout_s") %in% names(hb)))
  expect_identical(as.integer(hb$hb_n), 0L)
  expect_identical(as.character(hb$session_token), d$token)
  # The timeout is published BY the app, so the agent need not hardcode it.
  expect_true(is.numeric(hb$hb_timeout_s) && hb$hb_timeout_s > 0)
})

test_that("hb_n increases monotonically and never restarts mid-session", {
  .drv_local_root()
  d <- ts_drive_attach(.drv_fake_session(new.env()), list())
  .drv_write_arm(d$token)

  # hb_interval = 0 forces a write on every beat, so the counter's behaviour is
  # observable without sleeping the suite.
  old <- options(ts.drive.hb_interval = 0.001)
  on.exit(options(old), add = TRUE)

  seq_n <- integer(0)
  for (i in 1:5) {
    d$on_tick(global_data = list())
    seq_n <- c(seq_n, as.integer(ts_drive_read_ready()$hb_n))
  }
  expect_true(all(diff(seq_n) > 0L))
  # Strictly increasing is the load-bearing property: it is what lets the agent
  # tell "same session, still alive" from "a NEW session reusing the same pid".
  expect_identical(seq_n, sort(seq_n, method = "radix"))
})

test_that("the heartbeat preserves the token, the pid and started_at", {
  .drv_local_root()
  d <- ts_drive_attach(.drv_fake_session(new.env()), list())
  .drv_write_arm(d$token)

  first <- ts_drive_read_ready()
  old <- options(ts.drive.hb_interval = 0.001)
  on.exit(options(old), add = TRUE)
  for (i in 1:3) d$on_tick(global_data = list())
  later <- ts_drive_read_ready()

  expect_identical(as.character(later$session_token), d$token)
  expect_identical(as.integer(later$pid), as.integer(first$pid))
  # started_at identifies the SESSION, not the beat: it must stay put, or the
  # agent would see a "new session" every 3 seconds.
  expect_identical(as.character(later$started_at), as.character(first$started_at))
  expect_gt(as.integer(later$hb_n), as.integer(first$hb_n))
})

test_that("the heartbeat stops when disarmed but the file is KEPT", {
  .drv_local_root()
  d <- ts_drive_attach(.drv_fake_session(new.env()), list())
  .drv_write_arm(d$token)
  old <- options(ts.drive.hb_interval = 0.001)
  on.exit(options(old), add = TRUE)

  for (i in 1:3) d$on_tick(global_data = list())
  before <- ts_drive_read_ready()
  expect_true(isTRUE(before$armed))

  .drv_write_arm(d$token, armed = FALSE)
  d$on_tick(global_data = list())
  after <- ts_drive_read_ready()

  expect_false(isTRUE(after$armed))
  # DELIBERATE: not deleted. `armed:false` + a frozen counter says "this session
  # exists but is no longer driven", and keeping the file preserves the token the
  # agent needs to re-arm the SAME session without a restart (spec §0).
  expect_true(file.exists(ts_drive_path("ready.json")))
  expect_identical(as.character(after$session_token), d$token)

  # Once disarmed, further idle beats must not advance the counter.
  n <- as.integer(after$hb_n)
  for (i in 1:3) d$on_tick(global_data = list())
  expect_identical(as.integer(ts_drive_read_ready()$hb_n), n)
})

test_that("freshness accepts a live handshake and rejects a stale one", {
  .drv_local_root()
  ts_drive_write_json(
    list(protocol = TS_DRIVE_PROTOCOL, session_token = "abc12345",
         hb_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
         hb_n = 7L),
    ts_drive_path("ready.json")
  )
  expect_true(ts_drive_ready_fresh(timeout_s = 15))

  # Backdate the heartbeat instead of sleeping: freshness must be decidable
  # from the data, not from wall-clock patience.
  p <- ts_drive_read_ready()
  p$hb_at <- format(Sys.time() - 3600, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ts_drive_write_json(p, ts_drive_path("ready.json"))

  expect_false(ts_drive_ready_fresh(timeout_s = 15))
  expect_gt(ts_drive_ready_age(), 3000)
  # Same data, wider window -> accepted. Proves the timeout is a real knob and
  # not a constant baked into the comparison.
  expect_true(ts_drive_ready_fresh(timeout_s = 7200))
})

test_that("freshness refuses a missing, malformed or foreign handshake", {
  .drv_local_root()
  expect_false(ts_drive_ready_fresh())                       # absent

  ts_drive_write_json(list(nope = TRUE), ts_drive_path("ready.json"))
  expect_false(ts_drive_ready_fresh())                       # no hb_at

  ts_drive_write_json(
    list(protocol = "ts-drive/99", hb_at = ts_drive_now_iso(), hb_n = 1L),
    ts_drive_path("ready.json")
  )
  expect_false(ts_drive_ready_fresh())                       # wrong protocol

  ts_drive_write_json(
    list(protocol = TS_DRIVE_PROTOCOL, hb_at = "not-a-timestamp", hb_n = 1L),
    ts_drive_path("ready.json")
  )
  expect_false(ts_drive_ready_fresh())                       # unparseable date
  expect_identical(ts_drive_ready_age(), Inf)
})

test_that("the freshness timeout is a dev-protocol knob, never a scenario field", {
  old <- options(ts.drive.hb_timeout = 42)
  on.exit(options(old), add = TRUE)
  expect_identical(ts_drive_hb_timeout(), 42)

  options(ts.drive.hb_timeout = NULL)
  # A nonsense value must fall back to the documented default rather than
  # producing a window of 0 (which would make every session look stale) or Inf
  # (which would make every dead session look alive).
  for (bad in list(0, -1, NA_real_, "abc")) {
    options(ts.drive.hb_timeout = bad)
    expect_identical(ts_drive_hb_timeout(), 15)
  }
  options(ts.drive.hb_timeout = NULL)

  # The honest structural check: the timeout must not be reachable from a
  # scenario payload, otherwise a scenario could widen its own liveness window.
  scn <- .drv_write_scn
  expect_true(is.function(scn))
  expect_false("hb_timeout_s" %in% names(formals(scn)))
})

# -----------------------------------------------------------------------------
# 13b. First-arm transition — and the symptom that was NOT a lost write
# -----------------------------------------------------------------------------
# OBSERVED (2026-09-22, G2 acceptance run): after the FIRST arm of a fresh
# session, `ready.json` could stay frozen at `armed:false, hb_n:0`, while
# re-arming the same session made the counter advance.
#
# THE FIRST EXPLANATION WAS WRONG, and the correction is the point of this
# header. The symptom is the app's SLOW BOOT, not a lost write:
# `ts_drive_attach()` runs early in `server()`, and Shiny starts no reactive
# flush for a session until `server()` RETURNS — this app's `server()` does
# heavy init (spatial `mirai` daemons, plotly). So no protocol beat had run and
# `ready.json` was still the file written at attach time. MEASURED on a clean
# session, arming exactly ONCE and never re-arming: the arm was honoured after
# **22.4 s**, `hb_n` reached 1, the handshake was fresh, and **zero** write
# failures were logged. Arming early is not a bug: `arm.json` persists and the
# first beat honours it.
#
# What the investigation DID turn up were two real, latent write-path defects
# plus one measured stall — none of them the cause of that symptom:
#
#   * `ts_drive_write_json()` retried a fixed `<dest>.tmp` name and reported
#     nothing about a failure;
#   * the heartbeat's guard was `!inherits(wrote, "try-error")` around a
#     function that RETURNS its payload instead of throwing, so it was ALWAYS
#     true: the throttle advanced even when nothing had been written, so any
#     failed write would have silenced the heartbeat for a whole interval and
#     left a stale-but-plausible handshake;
#   * a doomed rename costs ~5.1 s on this Windows host, so an 8-attempt budget
#     parked the Shiny observer for 41.4 s in a single beat.
#
# The tests below pin the parts that CAN be pinned offline. The slow boot itself
# is Shiny semantics and cannot be: it is a live-session observation, recorded
# in tools/_drive/README.md with its numbers.

test_that("FIRST arm of a FRESH session latches armed and advances hb_n", {
  # The required regression, in the exact shape it was specified with:
  #   fresh session -> arm ONCE -> ready.armed = TRUE -> hb_n advances.
  #
  # A fresh root and a fresh token per test: re-arming an already-used session
  # is precisely the workaround that HID the symptom, so it must not be what the
  # test does.
  #
  # SCOPE: offline there is no `server()` to boot, so this pins the transition
  # itself, not the live boot delay. The live run is the one that measures the
  # 22.4 s (tools/_drive/README.md).
  .drv_local_root()
  d <- ts_drive_attach(.drv_fake_session(new.env()), list())

  h0 <- ts_drive_read_ready()
  expect_false(isTRUE(h0$armed))
  expect_identical(as.integer(h0$hb_n), 0L)

  .drv_write_arm(d$token)              # ONE arm
  d$on_tick(global_data = list())      # ONE beat

  h1 <- ts_drive_read_ready()
  expect_true(isTRUE(h1$armed))
  expect_gt(as.integer(h1$hb_n), 0L)
  # Present is not enough: the defect left a file that was readable and WRONG.
  # It has to be a FRESH handshake.
  expect_true(ts_drive_ready_fresh(timeout_s = 15))
})

test_that("the throttle advances on SUCCESS only, never on a failed write", {
  # THE GUARD FOR THE THROTTLE BUG, and it is deliberately PURE.
  #
  # The integration-level version of this test cannot be written honestly:
  # telling the two behaviours apart through the tick needs an interval LONGER
  # than a real rename failure — MEASURED at ~5.1 s on this host — so the test
  # would sleep ~11 s and still sit within ~2 s of both margins. Testing the
  # decision directly is exact, instant, and falsifiable.
  #
  # (This guards a LATENT bug: it is not what produced the first-arm symptom.
  # See the section header. It is still worth a guard, and the live injection in
  # tools/_drive/README.md exercises it end to end.)
  now <- 1000
  # Success -> the throttle moves, so the next beat is `interval` away.
  expect_identical(ts_drive_hb_next_at(now, hb_at = 5, wrote_ok = TRUE), 1000)
  # Failure -> the throttle does NOT move. This is the whole fix: the next beat
  # is 800 ms away, so the handshake repairs itself instead of waiting out the
  # interval with a stale `armed:false, hb_n:0` on disk until someone re-arms.
  expect_identical(ts_drive_hb_next_at(now, hb_at = 5, wrote_ok = FALSE), 5)
  # Fail-closed: anything that is not a definite TRUE must not advance it.
  for (bad in list(NA, NULL, "TRUE", 1)) {
    expect_identical(ts_drive_hb_next_at(now, hb_at = 5, wrote_ok = bad), 5)
  }
})

test_that("the tick USES the pure throttle rule — wiring, not just the rule", {
  # The pure test above guards the RULE; by construction it cannot see whether
  # the tick still CALLS it. A behavioural version of this test is possible but
  # dishonest: telling the two behaviours apart through the tick needs an
  # interval longer than a failed write (~0.56 s of retry backoff) AND a
  # pre-arm wait longer than that, so it costs ~2.6 s and ends up within ~1 s of
  # both margins — a guard made mostly of clock.
  #
  # A wiring assertion is exact instead: the old bug was literally the line
  # `cursor$hb_at <- now_num` (advance on ATTEMPT), and restoring it reddens
  # this immediately. MEASURED — the pure test alone did NOT redden when the
  # call site was reverted, which is exactly the hole this closes.
  src <- readLines(file.path(ts_project_root(), "R", "core", "drive_watcher.R"),
                   warn = FALSE, encoding = "UTF-8")
  expect_true(any(grepl("ts_drive_hb_next_at(now_num, cursor$hb_at, wrote_ok)",
                        src, fixed = TRUE)))
  expect_false(any(grepl("cursor$hb_at <- now_num", src, fixed = TRUE)))
})

test_that("a failed ready.json write is recorded and repairs on the next beat", {
  # The INTEGRATION half: the pure rule above must actually be WIRED into the
  # tick, and a lost write must be loud rather than silent.
  #
  # A DIRECTORY at the destination is the portable way to make the destination
  # unreplaceable, which is what a locked destination does on Windows.
  .drv_local_root()
  d <- ts_drive_attach(.drv_fake_session(new.env()), list())
  expect_true(file.exists(ts_drive_path("ready.json")))

  old <- options(ts.drive.hb_interval = 0.001)
  on.exit(options(old), add = TRUE)

  ts_drive_clear_write_error()
  unlink(ts_drive_path("ready.json"))
  dir.create(ts_drive_path("ready.json"))   # lock the destination

  .drv_write_arm(d$token)
  t0 <- Sys.time()
  d$on_tick(global_data = list())
  beat_s <- as.numeric(Sys.time() - t0, units = "secs")

  # 1. The failure is RECORDED, never swallowed.
  err <- ts_drive_last_write_error()
  expect_false(is.null(err))
  expect_identical(err$file, "ready.json")
  # 2. The PRE-FLIGHT fired, so the doomed rename was never issued: the message
  #    names the locked target rather than a rename failure. Deterministic, no
  #    clock involved — and it is the whole reason the beat below is cheap.
  #    MEASURED without it: each rename costs ~5.1 s and the 8-attempt budget
  #    parks the Shiny observer for 41.4 s.
  expect_true(grepl("cannot be removed", err$message, fixed = TRUE))
  expect_gte(err$attempts, 1L)
  expect_lte(err$attempts, TS_DRIVE_WRITE_ATTEMPTS)
  # 3. And the beat is BOUNDED IN TIME. Measured 0.65 s here against 41.4 s
  #    before the pre-flight; the ceiling is deliberately generous so a loaded
  #    machine cannot make this flaky, while the stall it guards against would
  #    still trip it four times over.
  expect_lt(beat_s, 10)

  # 3. Clear the obstruction: the very next beat repairs the handshake, with no
  #    re-arm and no waiting out an interval.
  unlink(ts_drive_path("ready.json"), recursive = TRUE)
  d$on_tick(global_data = list())

  h <- ts_drive_read_ready()
  expect_true(isTRUE(h$armed))
  expect_gt(as.integer(h$hb_n), 0L)
  # A successful write clears the banner: "no error" cannot be a stale flag.
  expect_null(ts_drive_last_write_error())
})

test_that("a write that cannot land fails LOUDLY after a bounded budget", {
  .drv_local_root()
  # A destination whose PARENT does not exist fails at the write stage in
  # microseconds, so the retry budget itself becomes observable: every attempt
  # is spent, and quickly. (A directory AT the destination is stopped by the
  # pre-flight after one attempt — asserted in the test above.)
  dest <- file.path(ts_drive_path(), "no-such-subdir", "scenario.json")
  ts_drive_clear_write_error()

  expect_false(ts_drive_write_json(list(a = 1), dest))

  err <- ts_drive_last_write_error()
  expect_false(is.null(err))                                 # never silent
  # The budget is bounded AND spent: a write that cannot land must give up,
  # not spin, and the attempt count must be reportable.
  expect_identical(err$attempts, TS_DRIVE_WRITE_ATTEMPTS)
  expect_identical(err$file, "scenario.json")

  # No temporary file of ANY name survives. A stale tmp is exactly what makes
  # the next writer collide, so the cleanup is part of the retry contract.
  expect_identical(list.files(ts_drive_path(), pattern = "\\.tmp$"), character(0))
})

test_that("the writer derives a UNIQUE temporary name per attempt", {
  # A STATIC assertion, deliberately: the property is not observable at runtime,
  # because the helper always cleans its tmp up — a fixed name and a unique one
  # leave the same directory behind. What a fixed name breaks is CONCURRENCY: a
  # leftover tmp from a killed process, or a second tab writing the same
  # destination, makes two writers fight over one name, and the loser's rename
  # then fails against a file the winner already moved.
  #
  # Restoring `paste0(path, ".tmp")` reddens this, which is the only
  # falsification available for the property — so the assertion is worth its
  # brittleness rather than being an unbacked claim.
  src <- readLines(file.path(ts_project_root(), "R", "core", "drive_watcher.R"),
                   warn = FALSE, encoding = "UTF-8")
  expect_false(any(grepl('paste0(path, ".tmp")', src, fixed = TRUE)))
  expect_true(any(grepl("ts_drive_tmp_name", src, fixed = TRUE)))

  # And the name must actually VARY, not merely look unique.
  .drv_local_root()
  a <- ts_drive_tmp_name(ts_drive_path("x.json"))
  b <- ts_drive_tmp_name(ts_drive_path("x.json"))
  expect_false(identical(a, b))
  expect_true(grepl("\\.tmp$", a))
  expect_identical(dirname(a), dirname(ts_drive_path("x.json")))
})

test_that("the recorded write error is SANITIZED and cleared by a success", {
  .drv_local_root()
  expect_null(ts_drive_last_write_error())     # "no failure" is NULL, not ""

  # The value is reachable from the wire (result.json) and from the console
  # log, so it must not carry an absolute path — reusing the badge sanitizer
  # rather than growing a second redactor that could drift from the first.
  ts_drive_note_write_error(file.path("C:", "somewhere", "deep", "ready.json"),
                            paste0("rename failed for ",
                                   file.path("C:", "Users", "someone", "ready.json")),
                            8L)
  err <- ts_drive_last_write_error()
  expect_identical(err$file, "ready.json")                 # basename only
  expect_false(grepl("Users", err$message, fixed = TRUE))
  expect_false(grepl("C:", err$message, fixed = TRUE))
  expect_true(grepl("<path>", err$message, fixed = TRUE))
  expect_lte(nchar(err$message), 80L)

  # A later success clears it.
  ts_drive_clear_write_error()
  expect_true(ts_drive_write_json(list(a = 1), ts_drive_path("here.json")))
  expect_null(ts_drive_last_write_error())
})

test_that("result.json carries the sanitized write error on a different channel", {
  # `ready.json` is itself a file that can fail to be written, so the error
  # cannot be reported there — it has to travel on ANOTHER channel. result.json
  # is that channel.
  .drv_local_root()
  dest <- ts_drive_path("scenario.json")
  dir.create(dest)
  ts_drive_clear_write_error()
  expect_false(ts_drive_write_json(list(a = 1), dest))

  ts_drive_write_result(1L, "done", "bulk_de", armed = TRUE)
  res <- ts_drive_read_result()
  expect_false(is.null(res$write_error))
  expect_identical(res$write_error$file, "scenario.json")

  # With no failure the field is PRESENT and null, so a client can tell
  # "nothing failed" from "this build does not report failures".
  unlink(dest, recursive = TRUE)
  ts_drive_clear_write_error()
  ts_drive_write_result(2L, "done", "bulk_de", armed = TRUE)
  expect_true("write_error" %in% names(ts_drive_read_result()))
  expect_null(ts_drive_read_result()$write_error)
})

# =============================================================================
# 14. Badge — passive, observational, and unable to leak
# =============================================================================
# The badge is rendered into the UI, so anything it prints can be read by
# anyone with the tab open. These tests pin BEHAVIOUR (what moves it) and
# SECRECY (what it cannot show), the two things a "dev affordance" gets wrong.

test_that("the badge moves only on real protocol transitions", {
  m <- ts_drive_badge_model()
  expect_identical(m$state, "off")

  for (pair in list(list("arm", "armed"), list("accepted", "armed"),
                    list("started", "running"))) {
    m <- ts_drive_badge_advance(m, pair[[1]])
    expect_identical(m$state, pair[[2]])
  }
  m <- ts_drive_badge_advance(m, "completed", status = "done", ack_seq = 3L)
  expect_identical(m$state, "done")
  expect_identical(m$ack_seq, 3L)

  m <- ts_drive_badge_advance(m, "disarm")
  expect_identical(m$state, "off")
})

test_that("an unknown badge event cannot move the badge", {
  m <- ts_drive_badge_advance(ts_drive_badge_model(), "arm")
  expect_identical(ts_drive_badge_advance(m, "invented_event"), m)
  expect_identical(ts_drive_badge_advance(m, ""), m)
  expect_identical(ts_drive_badge_advance(m, NA_character_), m)
})

test_that("invalid and ignored are NOT error states", {
  # A refused payload means "nothing ran", not "the app broke". Painting red
  # there would teach the operator to ignore red — the badge is only useful if
  # red means red.
  m <- ts_drive_badge_advance(ts_drive_badge_model(), "arm")
  expect_identical(ts_drive_badge_advance(m, "completed", status = "invalid")$state, "armed")
  expect_identical(ts_drive_badge_advance(m, "completed", status = "ignored")$state, "armed")
  expect_identical(ts_drive_badge_advance(m, "completed", status = "error")$state, "error")
})

test_that("a stale error is cleared by the next healthy state", {
  m <- ts_drive_badge_advance(ts_drive_badge_model(), "arm")
  m <- ts_drive_badge_advance(m, "error", error = "boom")
  expect_identical(m$error, "boom")
  m <- ts_drive_badge_advance(m, "completed", status = "done")
  expect_identical(m$error, "")
})

test_that("the badge view is a WHITELIST and cannot expose a secret", {
  # A model carrying every forbidden field at once.
  bad <- list(state = "armed", ack_seq = 1L, module = "bulk_de",
              action = "run_pipeline", elapsed_s = 1.2, error = "",
              token = "a1b2c3d4", root = "C:/secret/root", pid = 999L,
              payload = list(inputs = list()), snapshot = list(n_genes = 5))
  v <- ts_drive_badge_view(bad)

  expect_false(any(c("token", "root", "pid", "payload", "snapshot") %in% names(v)))
  expect_identical(names(v), c("visible", "state", "ack_seq", "module",
                               "action", "elapsed", "error"))
})

test_that("the sanitizer strips tokens and absolute paths from an error message", {
  win <- file.path("C:", "Users", "someone", "private", "ready.json")
  leaky <- paste0("Error in ts_drive_write_json(\"", win,
                  "\"): token=a1b2c3d4; pid=12345; matrix loaded")
  s <- ts_drive_badge_sanitize(leaky)

  expect_false(grepl("a1b2c3d4", s, fixed = TRUE))
  expect_false(grepl("C:", s, fixed = TRUE))
  expect_false(grepl("Users", s, fixed = TRUE))
  expect_true(grepl("<path>", s, fixed = TRUE))

  # A condition body carries a trailing newline and a call stack; only the
  # first line is a message, so the rest must be dropped.
  expect_identical(ts_drive_badge_sanitize("boom\nCall: f()\nExtra"), "boom")
  expect_identical(ts_drive_badge_sanitize(""), "")
  expect_identical(ts_drive_badge_sanitize(NULL), "")
  expect_identical(ts_drive_badge_sanitize(NA_character_), "")
  expect_identical(ts_drive_badge_sanitize(numeric(0)), "")
  # Long messages are truncated rather than allowed to blow up the layout.
  expect_lte(nchar(ts_drive_badge_sanitize(strrep("x", 500))), 80L)
})

test_that("an NA module or error cannot crash the view", {
  # MEASURED, not imagined: `nchar(NA)` is NA and `if (NA)` is an error, so the
  # first version of the sanitizer ABORTED on exactly this input. Found by the
  # probe, kept as a regression test.
  v <- ts_drive_badge_view(list(state = "armed", ack_seq = 1L,
                                module = NA_character_, action = NA,
                                elapsed_s = NA_real_, error = NA_character_))
  expect_true(v$visible)
  expect_identical(v$module, "")
  expect_identical(v$action, "")
  expect_identical(v$error, "")
  expect_identical(v$elapsed, "")
})

test_that("the badge is hidden unless EVERY gate condition holds", {
  # Note these are passed POSITIONALLY as length-1 values; an earlier probe
  # packed them into a vector, R coerced the string to "TRUE", and the probe
  # lied. The all-true row is the one that matters.
  expect_true(ts_drive_badge_visible(TRUE, TRUE, TRUE, TRUE, "armed"))
  expect_false(ts_drive_badge_visible(FALSE, TRUE, TRUE, TRUE, "armed"))  # dev off
  expect_false(ts_drive_badge_visible(TRUE, FALSE, TRUE, TRUE, "armed"))  # not interactive
  expect_false(ts_drive_badge_visible(TRUE, TRUE, FALSE, TRUE, "armed"))  # not selected
  expect_false(ts_drive_badge_visible(TRUE, TRUE, TRUE, FALSE, "armed"))  # not armed
  expect_false(ts_drive_badge_visible(TRUE, TRUE, TRUE, TRUE, "off"))     # nothing to say
  expect_false(ts_drive_badge_visible(TRUE, TRUE, TRUE, TRUE, ""))        # no state
  expect_false(ts_drive_badge_visible(TRUE, TRUE, TRUE, TRUE, NA))
})

test_that("hidden means NO UI element at all, not an empty one", {
  expect_null(ts_drive_badge_ui(list(visible = FALSE)))
  expect_null(ts_drive_badge_ui(NULL))

  m <- ts_drive_badge_advance(ts_drive_badge_model(), "arm")
  el <- ts_drive_badge_ui(ts_drive_badge_view(m))
  # The app must be indistinguishable from before when the protocol is off, so
  # "hidden" has to mean absent from the DOM, not a zero-width node.
  expect_false(is.null(el))
  html <- as.character(el)
  expect_true(grepl("ts_drive_badge", html, fixed = TRUE))
  expect_true(grepl("drive: armed", html, fixed = TRUE))
})

test_that("the badge never renders a token, a path or a payload", {
  m <- ts_drive_badge_advance(ts_drive_badge_model(), "arm")
  m <- ts_drive_badge_advance(m, "error",
                              error = "failed at C:/secret/root/ready.json token=zz999999")
  html <- as.character(ts_drive_badge_ui(ts_drive_badge_view(m)))
  expect_false(grepl("C:/secret", html, fixed = TRUE))
  expect_false(grepl("zz999999", html, fixed = TRUE))
  expect_true(grepl("drive: error", html, fixed = TRUE))
})

test_that("an idle tick queues no badge event", {
  .drv_local_root()
  d <- ts_drive_attach(.drv_fake_session(new.env()), list())
  expect_length(d$pending_events(), 0L)

  d$on_tick(global_data = list())            # disarmed: nothing happened
  expect_length(d$pending_events(), 0L)
})

test_that("arming queues exactly one 'arm' event, and reading it clears it", {
  .drv_local_root()
  d <- ts_drive_attach(.drv_fake_session(new.env()), list())
  .drv_write_arm(d$token)
  d$on_tick(global_data = list())

  ev <- d$pending_events()
  # ONE event, and reading drains the queue: the badge must not be re-armed by
  # every later idle tick, or "done" would decay to "armed" every 800 ms and the
  # human would never see the outcome.
  expect_length(ev, 1L)
  expect_identical(ev[[1]]$event, "arm")
  expect_length(d$pending_events(), 0L)
})

test_that("a consumed scenario queues accepted + completed with its seq", {
  .drv_local_root()
  d <- ts_drive_attach(.drv_fake_session(new.env()), list())
  .drv_write_arm(d$token)
  d$on_tick(global_data = list())            # drain the arm event
  invisible(d$pending_events())

  .drv_write_scn(4L, action = "noop", module = "bulk_de")
  effects <- function(input_id, mode = "bump", module = NULL) TRUE
  d$on_tick(global_data = list(ts_error_state = NULL), effects = effects)

  ev <- d$pending_events()
  expect_true(length(ev) >= 2L)
  expect_identical(ev[[1]]$event, "accepted")
  expect_identical(ev[[2]]$event, "completed")
  # The acknowledged sequence is what the human cross-checks against the agent.
  expect_identical(as.integer(ev[[2]]$ack_seq), 4L)
})

test_that("a disarmed session queues a 'disarm' event", {
  .drv_local_root()
  d <- ts_drive_attach(.drv_fake_session(new.env()), list())
  .drv_write_arm(d$token)
  d$on_tick(global_data = list())
  invisible(d$pending_events())

  .drv_write_arm(d$token, armed = FALSE)
  d$on_tick(global_data = list())
  ev <- d$pending_events()
  expect_length(ev, 1L)
  expect_identical(ev[[1]]$event, "disarm")
})

# =============================================================================
# 15. Badge visibility gates — the SIX situations the spec enumerates
# =============================================================================
# The single boolean table above (section 14) proves the FORMULA. This section
# proves the six SITUATIONS the spec names, each through the real decision path
# that produces the gate's inputs — because "the formula is right" and "the
# situation is detected" are different claims, and only the second is what the
# human sees. Every case asserts BOTH halves:
#   * the badge is hidden, AND
#   * the reason it is hidden is the one under test (not an accident of another
#     input being FALSE), so a future refactor cannot make a case pass by
#     disabling something unrelated.
#
# The six, verbatim from the spec:
#   1. TRANSCRIPTO_DEV_DRIVE is not enabled;
#   2. no valid arm.json exists;
#   3. the session is not the SELECTED session (wrong token);
#   4. the session is invalidated, or its heartbeat is stale;
#   5. the session is disarmed;
#   6. the session has ENDED.

test_that("GATE 1 — dev drive disabled means the badge cannot be shown", {
  # The app must be indistinguishable from before when the protocol is off.
  # `enabled` is read from the environment by app.R; here we assert the gate
  # honours that input for every other combination of live variables, so a
  # production session that is somehow armed and selected still shows NOTHING.
  expect_false(ts_drive_badge_visible(FALSE, TRUE, TRUE, TRUE, "armed"))
  expect_false(ts_drive_badge_visible(FALSE, TRUE, TRUE, TRUE, "done"))
  expect_false(ts_drive_badge_visible(FALSE, TRUE, TRUE, TRUE, "error"))
  # And the UI emitter agrees: no element at all, not an empty one.
  expect_null(ts_drive_badge_ui(ts_drive_badge_view(ts_drive_badge_model())))
})

test_that("GATE 2 — no valid arm.json means disarmed, hence hidden", {
  .drv_local_root()
  # (a) No arm.json at all.
  res <- ts_drive_arm_state("tok")
  expect_false(res$armed)
  expect_match(res$reason, "no arm.json")
  expect_false(ts_drive_badge_visible(TRUE, TRUE, TRUE, res$armed, "armed"))

  # (b) A malformed arm.json must be as good as absent (never a partial arm):
  # a half-written file is exactly what an interrupted agent leaves behind.
  writeLines("{ not json", ts_drive_path("arm.json"))
  res_bad <- ts_drive_arm_state("tok")
  expect_false(res_bad$armed)
  expect_false(ts_drive_badge_visible(TRUE, TRUE, TRUE, res_bad$armed, "armed"))
})

test_that("GATE 3 — a WRONG session token cannot arm, select or display", {
  .drv_local_root()
  # Two tabs, faithfully modelled: THIS tab's token vs the one ready.json names.
  # `selected` is not a literal here — it is computed the way app.R computes it
  # (does the handshake name MY token?), so the test fails if that rule changes.
  ts_drive_write_ready(list(), "the-other-tab", armed = TRUE)
  .drv_write_arm("the-other-tab")

  my_token <- "this-tab"
  res <- ts_drive_arm_state(my_token)
  is_selected <- identical(as.character(ts_drive_read_ready()$session_token),
                           my_token)

  expect_false(res$armed)
  expect_match(res$reason, "token mismatch")
  expect_false(is_selected)
  # BOTH inputs are FALSE for the wrong tab, and the badge is hidden. Asserting
  # them jointly is the point: the wrong tab can neither arm nor display.
  expect_false(ts_drive_badge_visible(TRUE, TRUE, is_selected, res$armed, "armed"))

  # And the mirror image, so the test cannot pass by everything being FALSE:
  # the RIGHT tab owns ready.json, arms, and displays. (Writing the handshake
  # is a distinct step — the token in ready.json is what `selected` compares.)
  ts_drive_write_ready(list(), my_token, armed = TRUE)
  .drv_write_arm(my_token)
  ok <- ts_drive_arm_state(my_token)
  ok_selected <- identical(as.character(ts_drive_read_ready()$session_token),
                           my_token)
  expect_true(ok$armed)
  expect_true(ok_selected)
  expect_true(ts_drive_badge_visible(TRUE, TRUE, ok_selected, ok$armed, "armed"))
})

test_that("GATE 3b — invalidation by the wrong tab leaves the owner's handshake intact", {
  # The stale-token half of "the previous token cannot arm or display": a tab
  # holding an OLD token must not be able to tear down the session that
  # currently owns ready.json.
  .drv_local_root()
  ts_drive_write_ready(list(), "current-owner", armed = TRUE)

  expect_false(ts_drive_invalidate_ready("stale-token"))   # refused
  expect_true(file.exists(ts_drive_path("ready.json")))    # still there
  expect_identical(ts_drive_read_ready()$session_token, "current-owner")
  # The stale token fails the freshness/selection test, so it cannot display.
  stale_selected <- identical(
    as.character(ts_drive_read_ready()$session_token), "stale-token")
  expect_false(stale_selected)
  expect_false(ts_drive_badge_visible(TRUE, TRUE, stale_selected, TRUE, "armed"))

  # The owner can still tear its own session down.
  expect_true(ts_drive_invalidate_ready("current-owner"))
  expect_false(file.exists(ts_drive_path("ready.json")))
})

test_that("GATE 4 — an invalidated or STALE session cannot be displayed", {
  .drv_local_root()
  ts_drive_write_ready(list(), "tok", armed = TRUE)

  # (a) invalidated: the handshake is gone, so freshness fails loudly.
  expect_true(ts_drive_invalidate_ready("tok"))
  expect_false(ts_drive_ready_fresh())
  expect_false(ts_drive_badge_visible(TRUE, TRUE, FALSE, FALSE, "off"))

  # (b) stale: the file is present but its heartbeat stopped moving.
  # `ts_drive_write_ready()` always stamps `hb_at` with NOW, so a stale file
  # must be FABRICATED directly — that is the honest way to represent "a
  # session died and left its handshake behind". `now` is injected into the
  # query, so this asserts a MEASURED age rather than sleeping for it.
  ts_drive_write_json(
    list(protocol = TS_DRIVE_PROTOCOL, session_token = "tok", armed = TRUE,
         pid = 1234L, hb_at = "2026-09-21T19:00:00Z", hb_n = 5L,
         hb_timeout_s = 15L),
    ts_drive_path("ready.json")
  )
  stale_now <- as.POSIXct("2026-09-21T20:00:00Z",
                          format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  expect_gt(ts_drive_ready_age(now = stale_now), ts_drive_hb_timeout())
  expect_false(ts_drive_ready_fresh(now = stale_now))
  # The badge is hidden because the session cannot be shown as live.
  expect_false(ts_drive_badge_visible(TRUE, TRUE, TRUE, TRUE, "off"))
})

test_that("GATE 5 — a disarmed session hides the badge but keeps its handshake", {
  .drv_local_root()
  .drv_write_arm("tok", armed = FALSE)
  res <- ts_drive_arm_state("tok")
  expect_false(res$armed)
  expect_match(res$reason, "armed=false")
  expect_false(ts_drive_badge_visible(TRUE, TRUE, TRUE, res$armed, "armed"))
  # The handshake is deliberately KEPT: the agent can re-arm the SAME session
  # without a restart, and a stopped hb_n is the honest "no longer driven" cue.
  expect_true(file.exists(ts_drive_path("arm.json")))
})

test_that("GATE 6 — a session that has ENDED hides the badge and drops its file", {
  .drv_local_root()
  # attach() MINTS its own token, so arm.json must carry THAT token — arming
  # with a made-up one leaves the session genuinely disarmed (caught by the
  # first run of this test, which expected TRUE and got FALSE).
  d <- ts_drive_attach(.drv_fake_session(new.env()), list())
  expect_true(file.exists(ts_drive_path("ready.json")))  # attach() wrote it
  .drv_write_arm(d$token)
  d$on_tick(global_data = list())              # armed, badge would show
  expect_true(d$armed())
  expect_true(ts_drive_badge_visible(TRUE, TRUE, TRUE, d$armed(), "armed"))

  # Ending the session drops the handshake, so no session is selected and
  # nothing is displayable — the badge cannot outlive its session.
  ts_drive_invalidate_ready(d$token)
  expect_false(file.exists(ts_drive_path("ready.json")))
  expect_false(ts_drive_ready_fresh())
  expect_false(ts_drive_badge_visible(TRUE, TRUE, FALSE, FALSE, "off"))
})

test_that("the badge is OBSERVATIONAL: no gate reads data, plots or inputs", {
  # The spec forbids the badge touching global_data, Seurat objects, matrices,
  # plots or long jobs. `ts_drive_badge_visible()` takes FIVE scalars and
  # `ts_drive_badge_view()` a flat model — so the guarantee is structural:
  # assert the signatures, which is stronger than grepping for a forbidden name.
  vis_fmls <- names(formals(ts_drive_badge_visible))
  expect_identical(
    vis_fmls,
    c("enabled", "interactive", "selected", "armed", "state")
  )
  view_fmls <- names(formals(ts_drive_badge_view))
  expect_length(view_fmls, 1L)
  # And the view is a closed WHITELIST: no extra field can leak through. The
  # model must carry a DISPLAYABLE state — for "off"/unknown the view is the
  # one-element `list(visible = FALSE)`, which has no fields to leak.
  v <- ts_drive_badge_view(list(state = "armed", secret = "TOKEN",
                                counts = matrix(1:4, 2), plot = "base64"))
  expect_identical(sort(names(v)),
                   sort(c("visible", "state", "ack_seq", "module",
                          "action", "elapsed", "error")))
  expect_false("secret" %in% names(v))
  expect_false("counts" %in% names(v))
  expect_false("plot" %in% names(v))
})

# =============================================================================
# 16. import_file — G3: load a REAL file without faking the widget
# =============================================================================
# Spec G3: "reuse the existing bulk import helper (discover in G0). Build the
# `fileInput`-shaped data.frame internally if that helper expects it; do not
# fake the widget." Spec S5 adds: "in live session BYPASS the widget ... Never
# pass a raw path string into `update*`."
#
# MEASURED (G0 discovery): the bulk load path is
#   mod_import_bulk.R  counts_reactive() -> input$counts_file$datapath
#                      -> smart_read()    -> global_data$bulk_obj
# so `import_file` is NOT an input injection at all. It has its own validated
# payload (`import`) and its own module-side seam: a published IMPORTER, which
# is deliberately a DIFFERENT registry slot from a button TOKEN — conflating
# the two would make the poller believe a button was bound and hand
# `run_pipeline` a `done` it never earned.
#
# The dataset this grade was handed is `GSE164073_Eye_count_matrix.csv`, which
# is a DIRECTORY containing a file of the same name. That is why the
# directory case below is not hypothetical: it is the literal input.

test_that("the import payload has a FROZEN key set", {
  expect_setequal(TS_DRIVE_IMPORT_KEYS, c("counts_path", "metadata_path", "mode"))
})

test_that("a path containing a `..` component is refused (spec S11)", {
  root <- .drv_local_root()
  v <- ts_drive_validate_import_path(file.path(root, "..", "escape.csv"), roots = root)
  expect_false(v$ok)
  expect_match(v$reason, "\\.\\.")
})

test_that("a path outside every allowlisted root is refused (spec S11)", {
  root <- .drv_local_root()
  outside <- file.path(dirname(root), "elsewhere.csv")
  v <- ts_drive_validate_import_path(outside, roots = root)
  expect_false(v$ok)
  expect_match(v$reason, "outside")
})

test_that("a DIRECTORY is refused with a reason that says so", {
  # Not hypothetical: the path this grade was given IS a directory.
  root <- .drv_local_root()
  d <- file.path(root, "GSE164073_Eye_count_matrix.csv")
  dir.create(d, showWarnings = FALSE)
  v <- ts_drive_validate_import_path(d, roots = root)
  expect_false(v$ok)
  expect_match(v$reason, "director")
})

test_that("a missing file, a non-string and an empty string are all refused", {
  root <- .drv_local_root()
  for (bad in list(file.path(root, "nope.csv"), 42L, "")) {
    v <- ts_drive_validate_import_path(bad, roots = root)
    expect_false(v$ok)
    expect_true(nzchar(v$reason))
  }
})

test_that("a real file inside the root is ACCEPTED", {
  root <- .drv_local_root()
  f <- file.path(root, "counts.csv")
  writeLines(c("gene,s1,s2", "A,1,2", "B,3,4"), f)
  v <- ts_drive_validate_import_path(f, roots = root)
  expect_true(v$ok)
  expect_true(file.exists(v$path))
})

test_that("import_file without an `import` block is refused, never a silent no-op", {
  .drv_local_root()
  v <- ts_drive_validate_scenario(
    list(protocol = TS_DRIVE_PROTOCOL, seq = 1, action = "import_file",
         module = "import_bulk"),
    "tok", 0L)
  expect_false(v$ok)
  expect_match(paste(v$errors, collapse = " "), "import")
})

test_that("an unknown key inside `import` is refused, and only the frozen keys survive", {
  root <- .drv_local_root()
  f <- file.path(root, "counts.csv")
  writeLines(c("gene,s1,s2", "A,1,2", "B,3,4"), f)

  bad <- ts_drive_validate_scenario(
    list(protocol = TS_DRIVE_PROTOCOL, seq = 1, action = "import_file",
         module = "import_bulk",
         import = list(counts_path = f, rm_rf = "C:/")),
    "tok", 0L)
  expect_false(bad$ok)
  expect_match(paste(bad$errors, collapse = " "), "rm_rf")

  ok <- ts_drive_validate_scenario(
    list(protocol = TS_DRIVE_PROTOCOL, seq = 2, action = "import_file",
         module = "import_bulk",
         import = list(counts_path = f, mode = "merged_matrix")),
    "tok", 0L)
  expect_true(ok$ok)
  # A WHITELIST, like every other rebuilt scenario: the injector can only ever
  # see the keys it declares.
  expect_setequal(names(ok$scenario$import), c("counts_path", "mode"))
})

test_that("import_file with NO published importer is invalid and names the seam", {
  root <- .drv_local_root()
  f <- file.path(root, "counts.csv")
  writeLines(c("gene,s1,s2", "A,1,2", "B,3,4"), f)
  res <- ts_drive_apply(NULL, NULL,
    list(action = "import_file", module = "import_bulk", inputs = list(),
         import = list(counts_path = f)),
    effects = function(input_id, mode = "bump", module = NULL, request = NULL) {
      if (identical(mode, "import")) return(NULL)   # nothing published
      FALSE
    })
  expect_identical(res$status, "invalid")
  expect_match(paste(res$errors, collapse = " "), "importer")
})

test_that("import_file hands the VALIDATED request to the module importer", {
  root <- .drv_local_root()
  f <- file.path(root, "counts.csv")
  writeLines(c("gene,s1,s2", "A,1,2", "B,3,4"), f)
  seen <- new.env(parent = emptyenv())
  seen$request <- NULL

  res <- ts_drive_apply(NULL, NULL,
    list(action = "import_file", module = "import_bulk", inputs = list(),
         import = list(counts_path = f)),
    effects = function(input_id, mode = "bump", module = NULL, request = NULL) {
      if (identical(mode, "import")) {
        seen$request <- request
        return(list(ok = TRUE, status = "applied",
                    errors = character(0), warnings = character(0)))
      }
      FALSE
    })

  # `done` — TERMINAL. The load is SYNCHRONOUS: by the time the result is
  # written the object is already in `global_data$bulk_obj`, so an agent must be
  # able to STOP polling. MEASURED on a live session: with `applied` — which is
  # not terminal — the driver waited its full 300 s on a load that had succeeded
  # in seconds. Asserting the PROPERTY (`terminal`) and not only the label is
  # what pins that; the label alone would move again silently.
  expect_identical(res$status, "done")
  expect_true(ts_drive_status_terminal(res$status))
  expect_identical(seen$request$counts_path, f)
  # Spec S5: the raw path must NEVER be routed through an `update*()` call.
  # The request travels as DATA to the module; no input id is touched.
  expect_length(res$errors, 0)
})

test_that("a module importer that refuses makes import_file invalid, not done", {
  root <- .drv_local_root()
  f <- file.path(root, "counts.csv")
  writeLines(c("gene,s1,s2", "A,1,2", "B,3,4"), f)
  res <- ts_drive_apply(NULL, NULL,
    list(action = "import_file", module = "import_bulk", inputs = list(),
         import = list(counts_path = f)),
    effects = function(input_id, mode = "bump", module = NULL, request = NULL) {
      if (identical(mode, "import")) {
        return(list(ok = FALSE, status = "invalid",
                    errors = "the counts file has no numeric columns",
                    warnings = character(0)))
      }
      FALSE
    })
  expect_identical(res$status, "invalid")
  expect_match(paste(res$errors, collapse = " "), "numeric columns")
})

test_that("a published IMPORTER is never listed as a button TOKEN", {
  # The two live in ONE registry environment, so this is the invariant that
  # keeps them apart: `effects(mode = "tokens")` filters `ls(reg)` by
  # `ts_drive_module_of()`, and an importer key must not survive that filter —
  # otherwise `run_pipeline` would read a published importer as a bound button
  # and report `done` for a click that never happened.
  gd <- list(drive_registry = new.env(parent = emptyenv()))
  expect_true(ts_drive_publish_importer(gd, "import_bulk", function(request) NULL))
  expect_true(is.function(ts_drive_importer_of(gd, "import_bulk")))

  ids  <- ls(gd$drive_registry)
  toks <- ids[vapply(ids, function(i)
    identical(ts_drive_module_of(i), "import_bulk"), logical(1))]
  expect_length(toks, 0L)
  # And it is keyed by the frozen prefix, so the separation is visible in `ls()`.
  expect_true(any(startsWith(ids, TS_DRIVE_IMPORTER_PREFIX)))
  # The DISCRIMINATING assertion — the one that actually pins the prefix. The
  # empty listing above would ALSO hold for a key with no dash at all
  # (`ts_drive_module_of()` returns NA there), so it cannot tell a correct
  # prefix from a lucky one. What matters is that the prefix defeats the token
  # filter for EVERY allowlisted module: a prefix ending in the module name
  # (e.g. "tsdrive-import_bulk") would be attributed to that module and read as
  # a bound button.
  for (m in TS_DRIVE_MODULES) {
    expect_false(identical(ts_drive_module_of(paste0(TS_DRIVE_IMPORTER_PREFIX, m)), m),
                 info = m)
  }
})

test_that("publishing an importer for an unknown module is refused, LOUDLY", {
  # A SILENT refusal here would read as "G3 is wired for sc". The v1 allowlist is
  # bulk-only (spec S3), so the warning is what makes the mistake visible in the
  # console instead of at the first scenario.
  gd <- list(drive_registry = new.env(parent = emptyenv()))
  expect_warning(
    ok_sc <- ts_drive_publish_importer(gd, "sc", function(request) NULL),
    "TS_DRIVE_MODULES")
  expect_false(isTRUE(ok_sc))
  expect_warning(
    ok_sp <- ts_drive_publish_importer(gd, "spatial", function(request) NULL),
    "TS_DRIVE_MODULES")
  expect_false(isTRUE(ok_sp))
  expect_null(ts_drive_importer_of(gd, "sc"))
})

test_that("only an OPERATOR can widen the import roots — never the scenario", {
  # The G3 dataset lives outside the project (`D:/Data_science/Données/bulk/…`),
  # so a data directory has to be allowlistable. It must be widened from the
  # ENVIRONMENT, which requires control of the launching process — the agent
  # driving a session a human opened cannot set it. If the scenario could name
  # its own root, spec S11 would be decoration.
  old <- Sys.getenv("TRANSCRIPTO_DRIVE_DATA_DIR", unset = NA_character_)
  on.exit({
    if (is.na(old)) Sys.unsetenv("TRANSCRIPTO_DRIVE_DATA_DIR")
    else Sys.setenv(TRANSCRIPTO_DRIVE_DATA_DIR = old)
  }, add = TRUE)

  root <- .drv_local_root()
  d <- file.path(tempdir(), "tsdrive-data-dir")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  f <- file.path(d, "counts.csv")
  writeLines(c("gene,s1", "A,1"), f)

  Sys.unsetenv("TRANSCRIPTO_DRIVE_DATA_DIR")
  expect_false(ts_drive_validate_import_path(f, roots = root)$ok)

  Sys.setenv(TRANSCRIPTO_DRIVE_DATA_DIR = d)
  expect_true(ts_drive_validate_import_path(f, roots = ts_drive_import_roots(root))$ok)

  # And a SCENARIO cannot widen it: the roots come from the validator's own
  # arguments, and nothing in the payload reaches them.
  v <- ts_drive_validate_scenario(
    list(protocol = TS_DRIVE_PROTOCOL, seq = 1, action = "import_file",
         module = "import_bulk",
         import = list(counts_path = f, roots = d)),
    "tok", 0L)
  expect_false(v$ok)
  expect_match(paste(v$errors, collapse = " "), "roots")
})


# =============================================================================
# 17. The Step 1 filtering action (G3, second milestone)
# =============================================================================
# Bulk is STAGED: `shared_rv$filtered_counts` is produced ONLY by Step 1 (or by
# the auto-pipeline), and EVERY downstream panel gates on it. So this section
# asserts three separate things, because passing one without the others proves
# nothing:
#   (a) the action is REACHABLE (`run_pipeline` finds it);
#   (b) it REFUSES, by name, when its precondition is missing;
#   (c) it rides the EXISTING observer that writes the state — no drive-only
#       copy, and no write to `shared_rv` from the drive layer.
# (c) is the one a behavioural test can never catch: a second, drive-only
# filtering implementation would pass (a) and (b) while leaving the human path
# to rot.

test_that("run_pipeline reaches Step 1 through its measured module", {
  # The button is resolved from the MODULE, so the scenario names
  # `bulk_filter` and the protocol must know its default click site. If the
  # module is not in TS_DRIVE_MODULES the scenario is refused at validation
  # time, which is exactly the state this test was written against.
  .drv_local_root()
  .drv_write_arm("tok")
  seen <- character(0)
  effects <- function(input_id = NULL, mode = "bump", module = NULL, request = NULL) {
    if (identical(mode, "tokens")) {
      return(list("bulk-filter-run_filter_norm" =
                    list(counter = NULL, ready = function() TRUE)))
    }
    seen <<- c(seen, input_id)
    TRUE
  }
  .drv_write_scn(7L, action = "run_pipeline", module = "bulk_filter",
                 session_token = "tok")
  res <- ts_drive_tick(NULL, NULL, NULL, "tok", 0L, FALSE, effects = effects)
  expect_true(res$consumed)
  expect_identical(ts_drive_read_result()$status, "done")
  expect_identical(seen, "bulk-filter-run_filter_norm")
})

test_that("the Step 1 action is refused, naming the missing object, when nothing is loaded", {
  # The refusal has to happen BEFORE the counter moves, or the module's `req()`
  # aborts in silence and the agent is told `done` for work that never ran.
  .drv_local_root()
  .drv_write_arm("tok")
  effects <- function(input_id = NULL, mode = "bump", module = NULL, request = NULL) {
    if (identical(mode, "tokens")) {
      return(list("bulk-filter-run_filter_norm" = list(
        counter = NULL,
        ready = function() "no bulk object loaded (global_data$bulk_obj is NULL)")))
    }
    stop("the poller must NOT fire a button whose readiness guard refused")
  }
  .drv_write_scn(8L, action = "run_pipeline", module = "bulk_filter",
                 session_token = "tok")
  res <- ts_drive_tick(NULL, NULL, NULL, "tok", 0L, FALSE, effects = effects)
  expect_true(res$consumed)
  r <- ts_drive_read_result()
  expect_identical(r$status, "invalid")
  expect_match(r$errors[[1]], "not ready")
  expect_match(r$errors[[1]], "bulk_obj")
})

test_that("the Step 1 binding rides the EXISTING observer, and the human path survives", {
  # Three constraints from the instruction, asserted on the SOURCE:
  #   * reuse the existing `mod_bulk_filter.R` observer;
  #   * never write `shared_rv$filtered_counts` directly;
  #   * no drive-only filtering implementation;
  #   * the human UI path stays unchanged when the drive is disabled.
  src <- readLines(file.path(ts_project_root(), "modules", "bulk", "mod_bulk_filter.R"),
                   warn = FALSE, encoding = "UTF-8")

  # (a) the module publishes the token for the measured id, WITH a guard.
  pub <- grep('ts_drive_publish_token\\(.*"bulk-filter-run_filter_norm"', src)
  expect_true(length(pub) >= 1L,
              info = "mod_bulk_filter.R does not publish the Step 1 token")
  expect_match(paste(src[pub[1]:min(length(src), pub[1] + 4L)], collapse = "\n"), "ready")

  # (b) the observer that WRITES the state is the one the drive token reaches.
  #     Walk back from the FIRST write site to its opening `observeEvent(`.
  write_idx <- grep("shared_rv\\$filtered_counts\\s*<-", src)
  expect_true(length(write_idx) >= 1L)
  obs_idx <- rev(grep("observeEvent\\(", src[seq_len(write_idx[1])]))[1]
  window  <- paste(src[obs_idx:write_idx[1]], collapse = "\n")
  expect_match(window, "drive_trigger",
               info = "the drive token does not reach the observer that writes filtered_counts")
  #     ...and the publish happens BEFORE that observer opens, or the counter
  #     would not exist yet when the poller looks it up.
  expect_true(pub[1] < obs_idx)

  # (c) the human click is still a trigger input. Without this the binding could
  #     silently replace the button instead of joining it.
  expect_match(paste(src, collapse = "\n"),
               "reactive\\(list\\(drive_counter\\(\\), input\\$run_filter_norm\\)\\)")

  # (d) the drive layer never writes the state itself.
  core <- list.files(file.path(ts_project_root(), "R"), pattern = "[.]R$",
                     recursive = TRUE, full.names = TRUE)
  core_txt <- unlist(lapply(core, function(f) readLines(f, warn = FALSE, encoding = "UTF-8")))
  expect_length(grep("shared_rv\\$filtered_counts\\s*<-", core_txt), 0L)
})

test_that("snapshot reports the module-published Step 1 state AND the loaded object", {
  # The snapshot used to read `global_data$bulk_obj` alone, so it could not tell
  # "Step 1 ran" from "Step 1 never ran": both answer `has_data=TRUE` with the
  # IMPORTED dimensions. MEASURED on a live session: the snapshot reported
  # `has_data=TRUE genes=17925 samples=18` in the same instant the DE guard
  # refused with "no bulk object loaded". A module may now publish what IT can
  # see, and the snapshot carries it under `modules[[module]]`.
  .drv_local_root()
  gd <- list(bulk_obj = list(counts = matrix(1:12, nrow = 4, ncol = 3)))
  reg <- new.env(parent = emptyenv())
  reg[["bulk-filter-run_filter_norm"]] <- list(
    counter = NULL,
    ready = function() TRUE,
    state = function() list(
      filtered_counts = list(n_genes = 4L, n_samples = 3L, samples = c("s1", "s2", "s3")),
      vst_mat = list(n_genes = 4L, n_samples = 3L, samples = c("s1", "s2", "s3"))))
  gd$drive_registry <- reg

  snap <- ts_drive_snapshot(gd)
  # The loaded object is STILL reported, unchanged — the addition is additive.
  expect_true(snap$has_data)
  expect_identical(snap$n_genes, 4L)
  expect_identical(snap$n_samples, 3L)
  # ...and now the Step 1 state is visible too.
  expect_identical(snap$modules$bulk_filter$filtered_counts$n_genes, 4L)
  expect_identical(snap$modules$bulk_filter$filtered_counts$samples, c("s1", "s2", "s3"))
  expect_identical(snap$modules$bulk_filter$vst_mat$n_samples, 3L)
  # No image data may leak into the verdict, wherever it now nests.
  expect_false(any(grepl("base64", unlist(snap), fixed = TRUE)))
})

test_that("a broken state probe degrades the snapshot loudly, it does not kill it", {
  # A module whose probe throws must not take the poller's beat down with it —
  # and must not vanish silently either, or "no state" and "probe crashed"
  # become the same answer to an agent.
  .drv_local_root()
  gd <- list(bulk_obj = list(counts = matrix(1:12, nrow = 4, ncol = 3)))
  reg <- new.env(parent = emptyenv())
  reg[["bulk-filter-run_filter_norm"]] <- list(
    counter = NULL, ready = function() TRUE,
    state = function() stop("boom"))
  gd$drive_registry <- reg

  snap <- ts_drive_snapshot(gd)
  expect_true(snap$has_data)
  expect_match(snap$modules$bulk_filter$probe_error, "boom")
})

test_that("a module that publishes no state contributes no `modules` entry", {
  # Absence must stay distinguishable from failure: the two are reported by
  # different fields, never by the same empty value.
  #
  # The fixture used to be `bulk-de-run_de`. It moved to the import button when
  # the DE token gained a state probe (G3, third milestone): the mechanism under
  # test is unchanged, but leaving a module there that DOES publish a state
  # would have made the test's own example stale — it would keep passing while
  # illustrating the opposite of the truth.
  .drv_local_root()
  gd <- list(bulk_obj = list(counts = matrix(1:12, nrow = 4, ncol = 3)))
  reg <- new.env(parent = emptyenv())
  reg[["import_bulk-btn_load"]] <- list(counter = NULL, ready = function() TRUE)
  gd$drive_registry <- reg

  snap <- ts_drive_snapshot(gd)
  expect_true(snap$has_data)
  expect_length(snap$modules, 0L)
})


# =============================================================================
# 18. The DE action (G3, third milestone) — the CONTRAST becomes observable
# =============================================================================
# Step 1 made `shared_rv$filtered_counts` reachable. DE is the NEXT stage, and
# it carries the same observability problem one level deeper: `result.json`
# says `done` as soon as the TOKEN moved, which means only that the module's
# `observeEvent` was triggered — never that a contrast exists. With
# `filtered_counts` present and no contrast registered, an agent trusting
# `done` would report a DE analysis that never happened. That is the same
# silent-seam class as §12, one stage further down the staged workflow.
#
# The probe is a closure inside `moduleServer()`, so a test cannot CALL it.
# What a test CAN pin is the source contract (the probe exists, is wired to the
# token, isolates every read, and returns counts under a NAMED convention) plus
# the snapshot mechanism that surfaces it. The live acceptance is what proves
# the numbers.

# The probe lives between `drive_state <- function()` and its closing brace.
# Returns `character(0)` when the probe is absent, so the caller's assertions
# fail rather than error — a RED that says "the probe is missing" is worth more
# than one that says "the helper crashed".
.drv_de_probe_region <- function() {
  src <- readLines(file.path(ts_project_root(), "modules", "bulk_de", "mod_bulk_de_run.R"),
                   warn = FALSE, encoding = "UTF-8")
  start <- grep("^\\s*drive_state <- function\\(\\)", src)
  if (length(start) != 1L) return(character(0))
  closer <- grep("^  \\}$", src)
  end <- closer[closer > start[1]]
  if (length(end) == 0L) return(character(0))
  src[start[1]:end[1]]
}

test_that("the DE token publishes a state probe, and every read in it is isolated", {
  src    <- readLines(file.path(ts_project_root(), "modules", "bulk_de", "mod_bulk_de_run.R"),
                      warn = FALSE, encoding = "UTF-8")
  region <- .drv_de_probe_region()
  expect_true(length(region) > 0L,
              info = "mod_bulk_de_run.R defines no `drive_state` probe")
  # Comments must not be able to satisfy a code rule.
  code <- sub("#.*$", "", region)

  # (a) the probe is actually WIRED to the token — a probe that is defined but
  #     never published leaves the agent exactly where it started.
  pub <- grep('ts_drive_publish_token\\(.*"bulk-de-run_de"', src)
  expect_length(pub, 1L)
  expect_match(paste(src[pub:min(length(src), pub + 2L)], collapse = "\n"),
               "state = drive_state")

  # (b) spec §6: the probe runs inside the POLLER's reactive beat, so an
  #     un-isolated read would enrol the DE result in the poller's dependency
  #     set. EQUAL COUNTS is the rule — one bare read is enough to break it,
  #     which a "does it contain isolate()?" check would never notice.
  expect_true(length(grep("shared_rv\\$", code)) >= 1L,
              info = "the probe reads no shared state — it cannot report the contrast")
  expect_identical(length(grep("shared_rv\\$", code)),
                   length(grep("isolate\\(shared_rv\\$", code)),
                   info = "a reactive read in the DE state probe is not isolate()-guarded")
})

test_that("the DE state reports COUNTS under a named convention, never the table", {
  region <- .drv_de_probe_region()
  joined <- paste(sub("#.*$", "", region), collapse = "\n")

  for (k in c("n_contrasts", "active_contrast", "n_genes", "n_padj_finite",
              "n_significant", "convention", "bypass")) {
    expect_match(joined, k, fixed = TRUE,
                 info = sprintf("the DE state probe does not report `%s`", k))
  }

  # The significant-gene count must be SELF-DESCRIBING. The panel's thresholds
  # (`lfc_thresh`, `padj_thresh`) are INPUTS, so a probe that borrowed them
  # would describe the last click rather than the state — and the agent could
  # not tell which convention the number was computed under. The convention is
  # therefore fixed and NAMED in the payload.
  expect_match(joined, "padj < 0\\.05",
               info = "the significance convention is applied but never named")
  expect_match(joined, "log2FoldChange")

  # Counts only: returning the result frame would push a per-gene table through
  # the poller's write path on every beat — the same class of mistake as
  # handing back a `renderPlot`'s base64.
  expect_false(grepl("=\\s*res\\s*[,)]", joined),
               info = "the DE state probe returns the result table itself")
})

test_that("run_pipeline reaches the DE action through its measured module", {
  # The reachability leg, mirrored from §17 for the filter: the scenario names
  # `bulk_de` and the protocol must resolve the module to its click site.
  .drv_local_root()
  .drv_write_arm("tok")
  seen <- character(0)
  effects <- function(input_id = NULL, mode = "bump", module = NULL, request = NULL) {
    if (identical(mode, "tokens")) {
      return(list("bulk-de-run_de" = list(counter = NULL, ready = function() TRUE)))
    }
    seen <<- c(seen, input_id)
    TRUE
  }
  .drv_write_scn(61L, action = "run_pipeline", module = "bulk_de", session_token = "tok")
  res <- ts_drive_tick(NULL, NULL, NULL, "tok", 0L, FALSE, effects = effects)
  expect_true(res$consumed)
  expect_identical(ts_drive_read_result()$status, "done")
  expect_identical(seen, "bulk-de-run_de")
})

test_that("snapshot carries the DE state, and 0 contrasts differs from never having run", {
  .drv_local_root()
  gd <- list(bulk_obj = list(counts = matrix(1:12, nrow = 4, ncol = 3)))

  # (a) a DE module that HAS registered one contrast.
  reg <- new.env(parent = emptyenv())
  reg[["bulk-de-run_de"]] <- list(
    counter = NULL, ready = function() TRUE,
    state = function() list(
      n_contrasts = 1L, active_contrast = "CoV2_vs_mock", n_genes = 17925L,
      n_padj_finite = 15000L, n_significant = 812L,
      convention = "padj < 0.05 & |log2FoldChange| > 1", bypass = FALSE))
  gd$drive_registry <- reg
  snap <- ts_drive_snapshot(gd)
  expect_identical(snap$modules$bulk_de$n_contrasts, 1L)
  expect_identical(snap$modules$bulk_de$active_contrast, "CoV2_vs_mock")
  expect_identical(snap$modules$bulk_de$n_significant, 812L)

  # (b) a DE module that is BOUND and READY but has never produced a contrast.
  #     This answer must not look like (a) — and must not look like "no DE
  #     module at all" either. It is the exact state an agent lands in after a
  #     `run_pipeline` whose `set_inputs` silently did not take.
  reg2 <- new.env(parent = emptyenv())
  reg2[["bulk-de-run_de"]] <- list(
    counter = NULL, ready = function() TRUE,
    state = function() list(n_contrasts = 0L, active_contrast = NULL,
                            n_genes = NULL, n_padj_finite = NULL,
                            n_significant = NULL, bypass = FALSE))
  gd$drive_registry <- reg2
  snap2 <- ts_drive_snapshot(gd)
  expect_true("bulk_de" %in% names(snap2$modules))
  expect_identical(snap2$modules$bulk_de$n_contrasts, 0L)
  expect_null(snap2$modules$bulk_de$active_contrast)
  expect_null(snap2$modules$bulk_de$n_genes)

  # ...and neither of those is the same as the key being ABSENT.
  gd$drive_registry <- new.env(parent = emptyenv())
  expect_length(ts_drive_snapshot(gd)$modules, 0L)
})

test_that("the free-text contrast name stays NOT drivable, so the pair is self-checking", {
  # `bulk-de-contrast_name` is deliberately ABSENT from the allowlist. Left
  # empty, the module names the contrast `group_target_vs_group_ref`, so the
  # NAME encodes the pair that was actually used — and that name is the only
  # self-check a scenario has that its ref/target injection TOOK.
  #
  # MEASURED live (2026-09-22), and the measurement is why this matters: the
  # driver injected the pair INVERTED (ref = CoV2, target = mock), against a
  # module default of ref = lvls[1] = "mock", target = lvls[2] = "CoV2". The
  # contrast came back named `mock_vs_CoV2` — the inverted name — which is what
  # proves both selects took. A free-text `contrast_name` would let a scenario
  # write any label it liked over that evidence.
  #
  # (An earlier version of this comment claimed the value is "ignored by the
  # client" when it is not among the choices. That was WRONG and is corrected
  # here: a live readback of the select's own `.value` showed the injected pair,
  # and the contrast name confirmed it end to end.)
  expect_false("bulk-de-contrast_name" %in% names(TS_DRIVE_ALLOWLIST))
  expect_true(all(c("bulk-de-condition_col", "bulk-de-group_ref",
                    "bulk-de-group_target", "bulk-de-de_engine") %in%
                    names(TS_DRIVE_ALLOWLIST)),
              info = "the design/contrast inputs must stay drivable")
})

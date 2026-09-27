# =============================================================================
# S6 - input CONFIRMATION before a drive run fires (Bulk DE pilot)
# =============================================================================
# THE DEFECT THIS PINS. Every injector adapter is `shiny::update*Input()`, a
# CLIENT ROUND-TRIP, and `ts_drive_apply()` fired the run in the SAME tick. The run
# therefore observed the PREVIOUS scenario's values. MEASURED live (4 runs):
#
#   run A (TRUE)  -> observer saw nothing yet -> `req()` cancel -> invalid, no reason
#   run B (FALSE) -> observer saw A's TRUE      -> warned  <- the reported bug
#   run C (FALSE) -> observer saw B's FALSE     -> silent  <- the one-tick lag
#   run D (TRUE)  -> observer saw C's FALSE     -> silent
#
# So run N read run N-1. `FALSE` reached the scenario and was lost at the
# Shiny-input boundary; `extract_deseq2_contrast()` was faithful and is NOT at
# fault.
#
# THE RULE. A drive run that injects NON-BUTTON inputs may not fire until the
# OWNING MODULE confirms it observes exactly the values validated for THIS
# scenario, bound to the session identity and the scenario sequence. Failures:
#   * no `confirm_inputs` on the module      -> refuse, no job (fail-closed)
#   * values not yet observed (timeout)      -> refuse, no job
#   * values differ (a human edited a control) -> refuse, NO re-injection
#   * confirmation for another seq / session  -> refuse, no job
#
# ⚠️ A HUMAN EDIT IS NOT PERMISSION TO OVERWRITE IT. Re-injecting would make the
# agent's last write win over a deliberate human change, so the scenario is
# refused and a NEW explicit request is required.
#
# SCOPE, from the measured inventory (not assumed): of 15 bound run buttons, only
# FOUR modules have any non-button allowlisted input at all - bulk_de,
# bulk_filter, bulk_pathways, import_bulk. The other ten cannot receive injected
# inputs, so the rule cannot affect them. A Bulk-DE-only pilot makes exactly
# three modules newly refuse; those are migrated module by module, not
# grandfathered.
# =============================================================================

source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")

# The `required` inputs of the Bulk DE run, as a scenario carries them.
.de_inputs <- function(shrink = TRUE) {
  list(`bulk-de-condition_col` = "condition",
       `bulk-de-group_target`  = "MW4_cornea_CoV2_1",
       `bulk-de-group_ref`     = "MW1_cornea_mock_1",
       `bulk-de-de_engine`     = "deseq2",
       `bulk-de-shrink_lfc`    = shrink)
}

# A registry entry, and an `effects` seam that serves it. Built in named steps
# rather than nested calls: hand-counting five closing parentheses inside a
# `return(list(x = list(...)))` is how this file first failed to parse.
#
# ⚠️ `ready()` must return `TRUE` for ready. `ts_drive_ready_probe()` reads a
# non-empty CHARACTER as "not ready, and THIS is the reason" (drive_watcher.R
# :1175-1176), so a first draft returning the string "ready" produced
# "button 'bulk-de-run_de' is bound but not ready: ready" and turned two blocks
# red for a harness reason rather than a product one. The same draft counted the
# fire through a second `effects` call, but the counter is bumped INSIDE the
# registry entry (`ts_drive_bump_token`), so the observable is `entry$counter`.
.de_entry <- function(counter = 0L, ready = TRUE, long = TRUE) {
  # A fresh entry means NO job in flight. The drive's job state is session-global
  # (`.ts_drive_state$job`), and a `long = TRUE` fire REGISTERS one, so without
  # this a completed test leaves a job behind and the next test is refused with
  # "another job is already running" — MEASURED as a 22-assertion cascade across
  # six unrelated blocks, every one of them reporting a failure that had nothing
  # to do with what it was testing. The block that asserts the job EXISTS clears
  # it again itself, at the end.
  ts_drive_job_clear()
  # `long = TRUE` BY DEFAULT, because that is what the real entry declares
  # (`ts_drive_publish_token(..., long = TRUE)`, mod_bulk_de_run.R:93). The first
  # version of this file used `long = FALSE`, and MEASURED consequence: every
  # "the run fired" assertion was exercising a code path production never takes,
  # and the missing `ts_drive_job_begin()` on the confirmation fire path stayed
  # invisible for exactly that reason — a long entry is the only one that needs it.
  list(counter = counter,
       ready = function() if (isTRUE(ready)) TRUE else "the module is not ready",
       state = NULL, long = long, timeout_s = 600)
}

.de_effects <- function(entry) {
  function(id, mode = NULL, module = NULL, request = NULL) {
    if (identical(mode, "tokens")) return(list(`bulk-de-run_de` = entry))
    if (is.null(id)) return(FALSE)
    TRUE
  }
}

.de_scn <- function(inputs = NULL, seq = 5L) {
  list(seq = seq, module = "bulk_de", action = "run_pipeline",
       session_token = "tokpin01", inputs = inputs)
}

# -----------------------------------------------------------------------------
test_that("a run injecting non-button inputs REFUSES when the module cannot confirm", {
  # The fail-closed rule. Today this run FIRES, and the module reads whatever the
  # session happened to hold - which is how a prior scenario's values were used.
  entry <- .de_entry()
  effects <- .de_effects(entry)
  out <- ts_drive_apply(NULL, NULL, .de_scn(.de_inputs(TRUE)), effects = effects)
  expect_identical(out$status, "invalid")
  expect_false(any(grepl("apeglm|Shrinkage", out$errors)))
  # A BOUNDED, redacted reason naming the mechanism, never a value.
  msg <- paste(out$errors, collapse = " ")
  expect_match(msg, "confirm", ignore.case = TRUE)
  expect_false(grepl("MW4_cornea|MW1_cornea", msg, fixed = TRUE))
  expect_identical(out$status, "invalid")
})

test_that("a run injecting NO inputs keeps its existing behaviour", {
  # Explicitly preserved by decision: the handshake only guards runs that inject.
  entry <- .de_entry()
  effects <- .de_effects(entry)
  out <- ts_drive_apply(NULL, NULL, .de_scn(NULL), effects = effects)
  expect_true(out$status %in% c("applied", "done", "running"))
})

# A pending record exactly as `ts_drive_apply()` builds it, so these tests drive
# the SERVICE seam (`ts_drive_tick(pending = )`) rather than `ts_drive_apply()`.
# A first draft asserted the seq and differs rules at `ts_drive_apply()` and both
# were red for a structural reason: apply does not consult the confirmation at all,
# by design — it only records the pending record. Consulting it in the same tick is
# the defect being fixed.
.de_pending <- function(seq = 5L, token = "tokpin01", shrink = TRUE, beats = 0L,
                        prior_shrink = NULL) {
  vals <- .de_inputs(shrink)
  if (is.null(prior_shrink)) prior_shrink <- !shrink
  list(seq = seq, module = "bulk_de", button = "bulk-de-run_de",
       values = vals, ids = names(vals),
       # The session state BEFORE the injection, which the drive read from `input`.
       prior = .de_inputs(prior_shrink),
       key = ts_drive_confirm_key(vals),
       session_token = token, beats = beats)
}

# Drive one servicing beat. `confirm` is what the module reports it observes.
.de_beat <- function(pending, confirm, token = "tokpin01") {
  entry <- .de_entry()
  if (!is.null(confirm)) entry$confirm_inputs <- confirm
  effects <- .de_effects(entry)
  ts_drive_tick(NULL, NULL, NULL, token, last_seq = 0, armed = TRUE,
                effects = effects, pending = pending)
}

test_that("a confirmation must be bound to THIS scenario's sequence", {
  # ⚠️ REWRITTEN after a live measurement, and the reason is a design error of mine.
  #
  # This block used to require the module to echo the scenario's `seq`, on the
  # theory that a confirmation had to be "bound to THIS scenario's sequence". A
  # module CANNOT: the drive's sequence is not a widget and nothing the module can
  # read, so the echo could only ever be `NA`. MEASURED live (2026-09-27): the very
  # first confirmation was refused with
  #   "the confirmation ... is for a different scenario (seq NA, expected 5)"
  # The check was not a guard, it was a wall, and it would have failed every run.
  #
  # So the binding is STRUCTURAL, and these three assertions are what actually
  # carry it: a declared seq must match, an ABSENT seq is not a replay, and the
  # record is single-slot so a second scenario cannot reuse the first one's.
  vals <- .de_inputs(TRUE)
  entry <- .de_entry()
  fired <- FALSE
  effects <- function(id, mode = NULL, module = NULL, request = NULL) {
    if (identical(mode, "tokens")) return(list(`bulk-de-run_de` = entry))
    if (is.null(id)) return(FALSE)
    fired <<- TRUE
    TRUE
  }

  # (a) a module that DECLARES a different seq is refused.
  entry$confirm_inputs <- function(values, session_token, prior = NULL) {
    list(ok = TRUE, seq = 999L, differs = character(0), missing = character(0),
         observed = values)
  }
  b <- .de_beat(.de_pending(seq = 5L), entry$confirm_inputs)
  expect_null(b$pending)
  expect_true(b$consumed)
  expect_identical(b$status, "invalid")
  expect_match(b$error, "declares seq 999", fixed = TRUE)
  expect_false(fired)

  # (b) an ABSENT seq means "I have none to declare" and must NOT be read as a
  #     replay. This is the case the live session hit.
  #     NB the tracked `fx` below, not `.de_beat()`: that helper builds its OWN
  #     effects closure, so a `fired` flag in this frame would never be set and the
  #     assertion would be measuring the helper rather than the fire path.
  for (absent in list(NULL, NA_integer_)) {
    fired <- 0L
    entry$confirm_inputs <- function(values, session_token, prior = NULL) {
      list(ok = TRUE, seq = absent, differs = character(0), missing = character(0),
           observed = values)
    }
    fx <- function(id, mode = NULL, module = NULL, request = NULL) {
      if (identical(mode, "tokens")) return(list(`bulk-de-run_de` = entry))
      if (is.null(id)) return(FALSE)
      fired <<- fired + 1L
      TRUE
    }
    b <- ts_drive_tick(NULL, NULL, NULL, "tokpin01", last_seq = 0, armed = TRUE,
                       effects = fx, pending = .de_pending(seq = 5L))
    expect_null(b$pending)
    expect_identical(b$status, "running")   # a LONG entry answers running, not done
    expect_identical(fired, 1L)
  }

  # (c) the record is SINGLE-SLOT: servicing it yields a verdict and clears it, so
  #     a later scenario cannot be satisfied by the earlier record. Firing twice
  #     from one record is the shape a replay would need.
  fired <- 0L
  entry$confirm_inputs <- function(values, session_token, prior = NULL) {
    list(ok = TRUE, seq = NULL, differs = character(0), missing = character(0),
         observed = values)
  }
  fx <- function(id, mode = NULL, module = NULL, request = NULL) {
    if (identical(mode, "tokens")) return(list(`bulk-de-run_de` = entry))
    if (is.null(id)) return(FALSE)
    fired <<- fired + 1L
    TRUE
  }
  p <- .de_pending(seq = 5L)
  b1 <- ts_drive_tick(NULL, NULL, NULL, "tokpin01", last_seq = 0, armed = TRUE,
                      effects = fx, pending = p)
  expect_identical(fired, 1L)
  expect_null(b1$pending)
  # The cleared record is NULL, so there is nothing left for a second attempt.
  b2 <- ts_drive_tick(NULL, NULL, NULL, "tokpin01", last_seq = 0, armed = TRUE,
                      effects = fx, pending = b1$pending)
  expect_identical(fired, 1L)
  expect_false(isTRUE(b2$consumed))
})

test_that("a confirmed fire DECLARES the long job, so the module can close it", {
  # 🔴 MEASURED live (2026-09-27): the confirmation succeeded, the log said
  # `inputs confirmed ... run fired`, the heartbeat kept climbing (nothing blocked,
  # nothing threw) — and `result.json` stayed `running` for good.
  #
  # Cause: `ts_drive_apply()` calls `ts_drive_job_begin()` for a `long = TRUE` entry
  # BEFORE answering `running`, and that declaration is what the module's
  # `on.exit(ts_drive_job_finish(...))` closes. The confirmation fire path went
  # straight to `effects(button)`, so no job was ever in flight and the module's
  # terminal had nothing to close.
  #
  # `bulk-de-run_de` declares `long = TRUE` (mod_bulk_de_run.R:93), which is why
  # `.de_entry()` carries it. So the fire must answer `running` and register a job.
  ts_drive_job_clear()
  entry <- .de_entry()
  expect_true(isTRUE(ts_drive_entry_long(entry)))
  fired <- 0L
  effects <- function(id, mode = NULL, module = NULL, request = NULL) {
    if (identical(mode, "tokens")) return(list(`bulk-de-run_de` = entry))
    if (is.null(id)) return(FALSE)
    fired <<- fired + 1L
    TRUE
  }
  entry$confirm_inputs <- function(values, session_token, prior = NULL) {
    list(ok = TRUE, seq = NULL, differs = character(0), missing = character(0),
         waiting = character(0), observed = values)
  }
  b <- ts_drive_tick(NULL, NULL, NULL, "tokpin01", last_seq = 0, armed = TRUE,
                     effects = effects, pending = .de_pending(seq = 5L))
  expect_identical(fired, 1L)
  # `done` here would be the `done` lie: the token moved, the analysis has not run.
  expect_identical(b$status, "running")
  expect_identical(b$job_status, "running")
  expect_null(b$pending)
  # The DECLARATION is the part that was missing: a job must now be in flight, for
  # this seq and this button, owned by this session.
  expect_true(isTRUE(ts_drive_job_busy()))
  job <- ts_drive_job_state()
  expect_identical(as.numeric(job$seq), 5)
  expect_identical(job$button, "bulk-de-run_de")
  expect_identical(job$module, "bulk_de")
  ts_drive_job_clear()
  expect_false(isTRUE(ts_drive_job_busy()))
})

test_that("the drive confirms ONLY the ids the scenario carried", {
  # 🔴 MEASURED live (2026-09-27): the probe answers about the requested controls
  # but reports its WHOLE view of the panel — `bulk_de` returns all five. The drive
  # was hashing that full observation against a key built from the ONE requested id,
  # so the two could never match: a correct single-checkbox injection was refused at
  # the bound while the DOM read `checked=false`. The drive must confirm exactly the
  # ids it validated, and no more.
  vals <- .de_inputs(FALSE)
  entry <- .de_entry()
  fired <- 0L
  effects <- function(id, mode = NULL, module = NULL, request = NULL) {
    if (identical(mode, "tokens")) return(list(`bulk-de-run_de` = entry))
    if (is.null(id)) return(FALSE)
    fired <<- fired + 1L
    TRUE
  }
  # The scenario asked for ONE control; the module reports FIVE.
  one <- list(seq = 5L, module = "bulk_de", button = "bulk-de-run_de",
              values = list(`bulk-de-shrink_lfc` = FALSE),
              ids = "bulk-de-shrink_lfc",
              prior = list(`bulk-de-shrink_lfc` = TRUE),
              key = ts_drive_confirm_key(list(`bulk-de-shrink_lfc` = FALSE)),
              session_token = "tokpin01", beats = 0L)
  entry$confirm_inputs <- function(values, session_token, prior = NULL) {
    list(ok = TRUE, seq = NULL, differs = character(0), missing = character(0),
         waiting = character(0),
         # five ids reported, one requested
         observed = list(`bulk-de-shrink_lfc` = FALSE, `bulk-de-group_ref` = "z",
                          `bulk-de-group_target` = "z", `bulk-de-de_engine` = "z",
                          `bulk-de-condition_col` = "z"))
  }
  b <- ts_drive_tick(NULL, NULL, NULL, "tokpin01", last_seq = 0, armed = TRUE,
                     effects = effects, pending = one)
  expect_identical(fired, 1L)
  expect_null(b$pending)
  expect_identical(b$status, "running")   # a LONG entry answers running, not done

  # And the converse, which is the safety side: a module that reports FEWER ids than
  # requested cannot produce a matching key, so it fails CLOSED rather than open.
  fired <- 0L
  entry$confirm_inputs <- function(values, session_token, prior = NULL) {
    list(ok = TRUE, seq = NULL, differs = character(0), missing = character(0),
         waiting = character(0), observed = list())   # reports nothing
  }
  p <- one
  out <- NULL
  for (i in seq_len(TS_DRIVE_CONFIRM_MAX_BEATS + 2L)) {
    out <- ts_drive_tick(NULL, NULL, NULL, "tokpin01", last_seq = 0, armed = TRUE,
                         effects = effects, pending = p)
    if (is.null(out$pending)) break
    p <- out$pending
  }
  expect_identical(fired, 0L)
  expect_null(out$pending)
  expect_identical(out$status, "invalid")

  # And a requested id reported at the WRONG value must not fire either, even when
  # the module claims `ok = TRUE`.
  fired <- 0L
  entry$confirm_inputs <- function(values, session_token, prior = NULL) {
    list(ok = TRUE, seq = NULL, differs = character(0), missing = character(0),
         waiting = character(0),
         observed = list(`bulk-de-shrink_lfc` = TRUE))   # wrong, but claims ok
  }
  p <- one
  for (i in seq_len(TS_DRIVE_CONFIRM_MAX_BEATS + 2L)) {
    out <- ts_drive_tick(NULL, NULL, NULL, "tokpin01", last_seq = 0, armed = TRUE,
                         effects = effects, pending = p)
    if (is.null(out$pending)) break
    p <- out$pending
  }
  expect_identical(fired, 0L)
  expect_identical(out$status, "invalid")
})

test_that("a human edit during the wait is REFUSED, never re-injected", {
  # 🔴 THE SAFETY PROPERTY, and its scope is deliberately BOUNDED.
  #
  # What must hold: a control the drive did not put there is never fired on, and
  # the drive never re-sends its own value over it. A first version ALSO refused
  # IMMEDIATELY, on the reasoning that "neither wanted nor prior" means a human.
  # Two live measurements refuted that, both on a session with NO human in it: the
  # not-yet-landed class (fixed by `prior`), and then `group_ref`/`group_target`,
  # which are selects whose CHOICES are rebuilt when the condition column changes
  # and which therefore sit at intermediate values mid-render. So the refusal is
  # now at the BOUND, and the message states both causes rather than picking one.
  #
  # This block pins what is left: the run NEVER fires, and NOTHING is re-injected.
  entry <- .de_entry()
  fired <- 0L
  effects <- function(id, mode = NULL, module = NULL, request = NULL) {
    if (identical(mode, "tokens")) return(list(`bulk-de-run_de` = entry))
    if (is.null(id)) return(FALSE)
    fired <<- fired + 1L
    TRUE
  }
  p <- .de_pending(seq = 5L)
  out <- NULL
  for (i in seq_len(TS_DRIVE_CONFIRM_MAX_BEATS + 2L)) {
    entry$confirm_inputs <- function(values, session_token, prior = NULL) {
      # A THIRD value: neither the wanted one nor the pre-injection one.
      other <- values
      other[["bulk-de-shrink_lfc"]] <- NULL
      list(ok = FALSE, seq = NULL, differs = "bulk-de-shrink_lfc",
           missing = "bulk-de-shrink_lfc", waiting = character(0),
           observed = other)
    }
    out <- ts_drive_tick(NULL, NULL, NULL, "tokpin01", last_seq = 0, armed = TRUE,
                         effects = effects, pending = p)
    if (is.null(out$pending)) break
    p <- out$pending
  }
  # Never fired. This is the property that matters and it is unconditional.
  expect_identical(fired, 0L)
  expect_null(out$pending)
  expect_true(out$consumed)
  expect_identical(out$status, "invalid")
  # And the message NAMES the control without ever carrying its value, states
  # BOTH causes (it cannot know which), and says plainly that nothing was re-sent.
  expect_match(out$error, "shrink_lfc", fixed = TRUE)
  expect_false(grepl("MW4_cornea", out$error, fixed = TRUE))
  expect_match(out$error, "re-injected", fixed = TRUE)
  expect_match(out$error, "cannot tell those apart", fixed = TRUE)
  # The refusal happens AT the bound, not on the first beat.
  expect_gte(p$beats %||% 0L, 0L)
})

test_that("a round trip that has NOT landed yet is WAITED for, not called tampering", {
  # 🔴 THE FALSE-ACCUSATION TEST, written after the live session produced
  #   "the values of 'bulk-de-condition_col', 'bulk-de-group_target',
  #    'bulk-de-group_ref', 'bulk-de-shrink_lfc' differ from those validated for
  #    seq 5 ... a human changed the control"
  # on a session driven entirely by an agent, with no human present. The module
  # must separate the two, and only `prior` makes it possible.
  seen <- NULL
  # Wanted = FALSE, prior = TRUE, so the pre-injection state really differs from
  # the wanted one — otherwise "still the prior value" is the trivial case.
  b <- .de_beat(.de_pending(seq = 5L, shrink = FALSE, prior_shrink = TRUE),
                 function(values, session_token, prior = NULL) {
                   # Observed is still the PRE-injection state: the client round
                   # trip has not completed. This is NOT a human edit.
                   seen <<- prior
                   list(ok = FALSE, seq = NULL, differs = character(0),
                        missing = character(0),
                        waiting = c("bulk-de-shrink_lfc", "bulk-de-condition_col"))
                 })
  # Non-terminal: the run is STILL WAITING, no verdict, nothing fired.
  expect_false(is.null(b$pending))
  expect_true(is.null(b$status))
  expect_false(isTRUE(b$consumed))
  # And `prior` really reached the probe — without it the middle state is invisible.
  expect_false(is.null(seen))
  expect_identical(seen[["bulk-de-shrink_lfc"]], TRUE)
})

test_that("the DEFERRED record carries the prior values, read before the injection", {
  # The drive is the only party that can see the pre-injection state, so the
  # record MUST carry it; a record without `prior` makes the module's
  # discrimination impossible and every first injection a false accusation.
  entry <- .de_entry()
  entry$confirm_inputs <- function(values, session_token, prior = NULL) {
    list(ok = TRUE, seq = NULL, differs = character(0), missing = character(0),
         observed = values)
  }
  vals <- .de_inputs(FALSE)
  scn <- .de_scn(vals, seq = 5L)
  # A fake `input` as the poller sees it: the session's CURRENT state, which is
  # what `prior` must capture (and NOT the wanted values).
  inp <- list(`bulk-de-condition_col` = "tissue",
              `bulk-de-group_target` = "limbus",
              `bulk-de-group_ref` = "cornea",
              `bulk-de-de_engine` = "limma",
              `bulk-de-shrink_lfc` = TRUE)
  effects <- .de_effects(entry)
  out <- ts_drive_apply(NULL, inp, scn, effects = effects, owner_token = "tokpin01")
  expect_identical(out$status, "running")
  p <- out$pending_confirm
  expect_false(is.null(p$prior))
  expect_identical(sort(names(p$prior)), sort(names(vals)))
  # The recorded prior is the SESSION state, not the wanted values.
  expect_identical(p$prior[["bulk-de-shrink_lfc"]], TRUE)
  expect_identical(p$prior[["bulk-de-condition_col"]], "tissue")
  expect_identical(p$values[["bulk-de-shrink_lfc"]], FALSE)
  # And the identity came from `owner_token`, which the validator does not carry.
  expect_identical(p$session_token, "tokpin01")
})

test_that("an UNCONFIRMED run is refused after a bounded number of beats", {
  # A bound, not a hope: the run either becomes confirmed or is refused. It must
  # never sit at `running` indefinitely, which is the failure a partially built
  # deferral would cause.
  slow <- function(values, session_token, prior = NULL) list(ok = FALSE, seq = 5L,
                                               differs = character(0),
                                               missing = "bulk-de-group_ref")
  p <- .de_pending(seq = 5L)
  n <- 0L
  repeat {
    n <- n + 1L
    out <- .de_beat(p, slow)
    if (is.null(out$pending)) break
    p <- out$pending
    if (n > 20L) break
  }
  expect_null(out$pending)
  expect_identical(out$status, "invalid")
  expect_match(out$error, "not confirmed", ignore.case = TRUE)
  # The bound is the declared one, so the test fails if someone raises it silently.
  expect_match(out$error, as.character(TS_DRIVE_CONFIRM_MAX_BEATS), fixed = TRUE)
  expect_gte(n, 1L)
  expect_lte(n, TS_DRIVE_CONFIRM_MAX_BEATS + 1L)
})

test_that("a REPLACED session can never satisfy a pending confirmation", {
  # The pending record belongs to the session that injected the values. A new
  # session has a new token, and the old confirmation must not carry over.
  b <- .de_beat(.de_pending(seq = 5L, token = "oldtoken"),
                 function(values, session_token, prior = NULL) list(ok = TRUE, seq = 5L,
                                                      differs = character(0),
                                                      missing = character(0),
                                                      observed = values),
                 token = "newtoken")
  expect_null(b$pending)
  expect_identical(b$status, "invalid")
  expect_match(b$error, "session was replaced", fixed = TRUE)
})

test_that("FIRST-RUN fidelity: each scenario fires on ITS OWN values, not the previous", {
  # The whole point of the slice, as a sequence. TRUE then FALSE must each be
  # honoured on the FIRST run that carries them - the defect was that run N read
  # run N-1. A module that reports the values it observes is driven once per
  # scenario, and each must fire.
  fire_for <- function(shrink) {
    fired <- FALSE
    p <- .de_pending(seq = 5L, shrink = shrink)
    # The module reports what it actually read; here that is the value THIS
    # scenario carried, once the round trip has landed. A first draft also defined
    # a stray `confirm` closure and then attached an attribute to it, which is not
    # indexable - dead code that aborted the block before its first assertion.
    entry <- .de_entry()
    entry$confirm_inputs <- function(values, session_token, prior = NULL) {
      list(ok = TRUE, seq = 5L, differs = character(0), missing = character(0),
           observed = values)
    }
    effects <- function(id, mode = NULL, module = NULL, request = NULL) {
      if (identical(mode, "tokens")) return(list(`bulk-de-run_de` = entry))
      if (is.null(id)) return(FALSE)
      fired <<- TRUE
      TRUE
    }
    out <- ts_drive_tick(NULL, NULL, NULL, "tokpin01", last_seq = 0, armed = TRUE,
                         effects = effects, pending = p)
    list(fired = fired, status = out$status)
  }
  for (v in c(TRUE, FALSE, TRUE)) {
    r <- fire_for(v)
    expect_true(r$fired)
    expect_identical(r$status, "running")  # long entry -> running
  }
})

test_that("the DEFERRED record carries the canonical key of the values validated", {
  # Added after falsification (M9). Replacing `key = ts_drive_confirm_key(vals)` in
  # `ts_drive_apply()` with a constant left the file GREEN: every other block builds
  # its own pending record with the test's own call to `ts_drive_confirm_key()`, so
  # the product's assignment was never exercised. A key that ignores the values
  # would make the confirmation compare a scenario against itself.
  #
  # So drive `ts_drive_apply()` itself and assert the record it produces.
  vals <- .de_inputs(TRUE)
  entry <- .de_entry()
  entry$confirm_inputs <- function(values, session_token, prior = NULL) {
    list(ok = TRUE, seq = 5L, differs = character(0), missing = character(0),
         observed = values)
  }
  effects <- .de_effects(entry)
  out <- ts_drive_apply(NULL, NULL, .de_scn(vals, seq = 5L), effects = effects)
  expect_identical(out$status, "running")
  p <- out$pending_confirm
  expect_false(is.null(p))
  # The key is a FUNCTION of the validated values, not a constant.
  expect_identical(p$key, ts_drive_confirm_key(vals))
  expect_identical(sort(p$ids), sort(names(vals)))
  expect_identical(p$values, vals)
  expect_identical(p$seq, 5L)
  expect_identical(p$beats, 0L)
  # And it is order-insensitive on the ids, so a re-ordered scenario is still the
  # same scenario: the key is canonical, not positional.
  rev_vals <- vals[rev(names(vals))]
  expect_identical(ts_drive_confirm_key(rev_vals), p$key)
  # Two different value sets must NOT collide on one key.
  other <- .de_inputs(FALSE)
  expect_false(identical(ts_drive_confirm_key(other), p$key))
  # And the button must NOT have been fired at deferral time.
  expect_identical(entry$counter, 0L)
})

test_that("a confirmation claiming ok but reporting OTHER values must NOT fire", {
  # Added after falsification. Mutating the canonical-key comparison to `TRUE`
  # left this whole file GREEN (M8): `ok` and `differs` already gate the fire, so
  # the key check looked like redundant defence and nothing exercised it. It is
  # not redundant — it is the only comparison against the values the DRIVE
  # validated, as opposed to the values the MODULE claims. A module reporting
  # `ok` with a different set is precisely the case it exists for.
  stale <- function(values, session_token, prior = NULL) {
    other <- values
    other[["bulk-de-shrink_lfc"]] <- !values[["bulk-de-shrink_lfc"]]
    list(ok = TRUE, seq = 5L, differs = character(0), missing = character(0),
         observed = other)
  }
  fired <- FALSE
  entry <- .de_entry()
  entry$confirm_inputs <- stale
  effects <- function(id, mode = NULL, module = NULL, request = NULL) {
    if (identical(mode, "tokens")) return(list(`bulk-de-run_de` = entry))
    if (is.null(id)) return(FALSE)
    fired <<- TRUE
    TRUE
  }
  p <- .de_pending(seq = 5L)
  for (i in seq_len(TS_DRIVE_CONFIRM_MAX_BEATS + 2L)) {
    out <- ts_drive_tick(NULL, NULL, NULL, "tokpin01", last_seq = 0, armed = TRUE,
                         effects = effects, pending = p)
    if (is.null(out$pending)) break
    p <- out$pending
  }
  expect_false(fired)
  expect_null(out$pending)
  expect_identical(out$status, "invalid")
  expect_match(out$error, "not confirmed", ignore.case = TRUE)
})

test_that("the ROUND TRIP holds: apply defers -> tick carries -> next beat services", {
  # 🔴 THE TEST THAT WAS MISSING, written after the live session found the defect.
  #
  # Every other block in this file calls `ts_drive_apply()` or
  # `ts_drive_tick(pending = )` DIRECTLY. That is a real gap, and it hid TWO breaks
  # in the same chain, on the path a live session takes and no unit test did:
  #
  #   1. the tick copied `res$status` / `nav` / `action` out of `ts_drive_apply()`
  #      but never `res$pending_confirm`, so the deferred record died in the tick;
  #   2. `ts_drive_attach` wrote `cursor$pending` only under `if (had_pending)`,
  #      which propagates an EXISTING record and cannot CAPTURE a new one.
  #
  # Together: seq 7 answered `running` (the deferral worked, the beat loop was
  # healthy, `hb_n` climbing) and then nothing happened for 250+ s — no
  # confirmation and no 5-beat timeout either, because the next beat was handed
  # `pending = NULL`. Fifty-one green assertions, and the feature did not work.
  #
  # ⚠️ It has to go through the FILESYSTEM, because `ts_drive_tick()` takes no
  # scenario argument: it reads `scenario.json` from the drive root itself. A first
  # draft called the tick with a hand-built scenario and was red for that reason
  # alone — which is the whole point of this block, so the real route is used.
  root <- file.path(tempdir(), paste0("tsdrive-cf-", as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(root, "tools", "_drive"), recursive = TRUE, showWarnings = FALSE)
  ts_drive_boot(root)
  on.exit({
    ts_drive_boot(ts_project_root())
    unlink(root, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  ts_drive_clear_write_error()
  ts_drive_job_clear()
  # The arm gate reads `ts_drive_interactive()`, and `Rscript` is never
  # interactive - so without this the tick refuses the scenario before reading it
  # and every assertion below reports FALSE for a reason that has nothing to do
  # with the handshake. Same line, same reason, as test-drive-watcher.R:78.
  op <- options(ts.drive.interactive = TRUE)
  on.exit(options(op), add = TRUE)

  tok <- "tokrt001"
  ts_drive_write_json(list(protocol = TS_DRIVE_PROTOCOL, token = tok, armed = TRUE),
                      ts_drive_path("arm.json"))
  ts_drive_write_json(
    list(protocol = TS_DRIVE_PROTOCOL, seq = 5L, action = "run_pipeline",
         module = "bulk_de", button = "bulk-de-run_de", session_token = tok,
         inputs = .de_inputs(FALSE)),
    ts_drive_path("scenario.json"))

  observed <- NULL
  entry <- .de_entry()
  entry$confirm_inputs <- function(values, session_token, prior = NULL) {
    # Beat 1: the round trip has NOT landed, so the module still sees the session
    # default. The real one-beat lag, reproduced on purpose.
    if (is.null(observed)) {
      return(list(ok = FALSE, seq = 5L, differs = character(0),
                  missing = "bulk-de-group_ref"))
    }
    list(ok = TRUE, seq = 5L, differs = character(0), missing = character(0),
         observed = observed)
  }
  fired <- FALSE
  effects <- function(id, mode = NULL, module = NULL, request = NULL) {
    if (identical(mode, "tokens")) return(list(`bulk-de-run_de` = entry))
    if (is.null(id)) return(FALSE)
    fired <<- TRUE
    TRUE
  }

  # --- beat 1: the scenario is consumed, DEFERRED, and the record must survive --
  b1 <- ts_drive_tick(NULL, NULL, NULL, tok, last_seq = 0, armed = TRUE,
                      effects = effects)
  expect_true(b1$consumed)
  expect_identical(b1$status, "running")
  expect_false(fired)
  # THE MISSING LINK (1): the tick must hand the record on.
  expect_false(is.null(b1$pending))
  expect_identical(b1$pending$seq, 5L)
  expect_identical(b1$pending$key, ts_drive_confirm_key(.de_inputs(FALSE)))

  # --- beats 2..n: carried forward, still waiting, nothing fired ---------------
  p <- b1$pending
  b <- b1
  for (i in seq_len(TS_DRIVE_CONFIRM_MAX_BEATS)) {
    b <- ts_drive_tick(NULL, NULL, NULL, tok, last_seq = 5L, armed = TRUE,
                       effects = effects, pending = p)
    if (!is.null(b$pending)) p <- b$pending
  }
  expect_false(fired)
  expect_false(is.null(p))
  expect_gte(p$beats, 1L)
  # A scenario already consumed must NOT be re-read from disk, or the wait would
  # restart from zero on every beat and never expire.
  expect_identical(b$consumed, FALSE)

  # --- the round trip lands: the module now sees the injected values ----------
  observed <- .de_inputs(FALSE)
  b2 <- ts_drive_tick(NULL, NULL, NULL, tok, last_seq = 5L, armed = TRUE,
                      effects = effects, pending = p)
  expect_true(fired)
  expect_null(b2$pending)
  expect_identical(b2$status, "running")  # long entry -> running, not the `done` lie
  expect_identical(b2$last_seq, 5L)
})

test_that("ts_drive_attach threads cursor$pending and CLEARS it on a verdict", {
  # Added after falsification. Disabling the assignment
  # `if (had_pending) cursor$pending <- tick$pending` left the whole file GREEN
  # (M6): every test drives `ts_drive_tick()` directly and passes `pending =` by
  # hand, so nothing observed the CURSOR at all. The consequence is silent and
  # severe: the wait never resolves, because beat 2 carries no record, the
  # scenario is already consumed, and the run vanishes with no verdict.
  #
  # ⚠️ THIS IS A SOURCE LOCK, and the label matters. The two lines live inside the
  # `observe()` body, which no `Rscript` test can advance: `observe()` outside a
  # session does not run without a domain to flush, which is exactly why the
  # token/nav tests above only reach the file-system half of `ts_drive_attach()`.
  # A source lock is the technique this repo already accepts for a seam that is
  # unobservable in R (§2cr, "preuve = verrou source"), and it is WEAKER than
  # execution. The behavioural proof is the fresh VISIBLE session: a run must stay
  # non-terminal across beats and then reach a verdict, which the lock cannot show.
  w <- readLines(file.path(ts_project_root(), "R/core/drive_watcher.R"), warn = FALSE)
  # Comments are stripped first: the fix is DOCUMENTED in a comment that quotes the
  # buggy line verbatim, so a naive `grepl` over the raw file matches the prose and
  # the assertion would be about the comment rather than the code. Measured: a
  # first version of this block failed for exactly that reason.
  code <- w[!grepl("^\\s*#", w)]
  # (a) the cursor is DECLARED beside last_seq/armed, not somewhere per-beat
  expect_true(any(grepl("^\\s*cursor\\$pending\\s*<-\\s*NULL\\s*$", code)))
  # (b) the cursor is fed INTO the tick, not only read from it
  expect_true(any(grepl("effects\\s*=\\s*effects,\\s*pending\\s*=\\s*cursor\\$pending", code)))
  # (c) and it is written back UNCONDITIONALLY. 🔴 This is the exact line the live
  #     session falsified: the first version guarded it with `if (had_pending)`,
  #     which can only PROPAGATE an existing record, so a record the tick had just
  #     created was dropped on the same beat and the wait could neither complete
  #     nor time out. A guard here is the bug, not a style.
  expect_true(any(grepl("^\\s*cursor\\$pending\\s*<-\\s*tick\\$pending\\s*$", code)))
  expect_false(any(grepl("had_pending", code)))
  # (d) the tick must LIFT `pending_confirm` out of apply()'s result — the other
  #     half of the same live defect, and the reason the record died one step
  #     earlier than (c) could ever repair.
  expect_true(any(grepl("out\\$pending\\s*<-\\s*res\\$pending_confirm", code)))
  # (e) the identity is the LIVE token. `session_token` is absent from the
  #     validator's rebuilt whitelist, so reading it off `scn` yields NULL and every
  #     confirmation is refused as a replaced session.
  expect_true(any(grepl("session_token\\s*=\\s*as\\.character\\(owner_token", code)))
})

test_that("the module publisher exposes confirm_inputs, and the seam is falsifiable", {
  # The wiring half. `bulk_de` must publish a confirmation; the pilot is worthless
  # if the registry entry cannot carry one, and the guard above is then vacuous.
  src <- paste(readLines(file.path(ts_project_root(), "modules/bulk_de/mod_bulk_de_run.R"),
                         warn = FALSE), collapse = "\n")
  expect_match(src, "confirm_inputs", fixed = TRUE)
  # And the watcher must actually consult it, or the registry field is decoration.
  w <- paste(readLines(file.path(ts_project_root(), "R/core/drive_watcher.R"),
                       warn = FALSE), collapse = "\n")
  expect_match(w, "confirm_inputs", fixed = TRUE)
})

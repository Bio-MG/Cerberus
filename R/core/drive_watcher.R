# =============================================================================
# R/core/drive_watcher.R — Live control protocol: file-drop poller
# =============================================================================
# Dev-only sibling of docs/DRIVE_LIVE_CONTROL_PLAN.md (BUILD THIS spec).
# Sourced by app.R as a SHINY-AWARE core file (like R/core/state.R): it is the
# one place in R/ allowed to carry Shiny reactivity, because the spec puts the
# whole protocol here (`app.R` gets 5-15 lines, not the logic).
#
# WHAT IT DOES
#   Attaches a cheap poller to the LIVE session the human is looking at
#   (RStudio Viewer or a localhost tab — same httpuv session). The agent never
#   attaches to R, never scrapes the Viewer, never sends keystrokes: it drops
#   JSON files under tools/_drive/ and the running session picks them up.
#
#   ready.json    app -> agent   (written at session start, removed at end)
#   arm.json      agent -> app   (arms the poller)
#   scenario.json agent -> app   (one action to apply)
#   result.json   app -> agent   (the VERDICT — never stdout)
#
# NO-OP BY CONSTRUCTION
#   With no arm.json the poller only ever does file.exists() + file.info().
#   `session$setInputs()` is testServer-only and is NOT used here.
#
# ONE STYLE ONLY (spec §6)
#   `shiny::invalidateLater()` in a single `observe()`. JSON is parsed only
#   when the mtime changed, so an idle tick stays in the sub-millisecond range.
# =============================================================================


# =============================================================================
# App root — captured at boot, NEVER getwd() (RStudio drifts mid-session)
# =============================================================================

# Populated by ts_drive_boot() from app.R, before server() exists.
.ts_drive_state <- new.env(parent = emptyenv())
.ts_drive_state$root <- NULL

# Write diagnostics, populated by ts_drive_write_json() on FAILURE only.
#
# `NULL` therefore means "no write has failed", which is distinguishable from
# "a write failed and nobody looked". Deliberately a plain environment and not
# a `reactiveVal`: `R/` must stay free of reactivity (C2), and the poller must
# be able to record a failure without a reactive context.
.ts_drive_state$write_error <- NULL
# Monotonic counter, used to make every temporary write name unique.
.ts_drive_state$write_attempts <- 0L
# The in-flight job, or NULL. See the "Job lifecycle" section for why this
# exists and what an agent may conclude from it.
.ts_drive_state$job <- NULL
.ts_drive_state$selected_token <- NULL
.ts_drive_state$job_serial <- 0L

#' Record the app root once, at source time.
#'
#' app.R runs with the project root as working directory (renv activates at
#' startup and `runApp` sources from there), but the SERVER may later run with
#' a different `getwd()` if any code calls `setwd()`. Capturing here means the
#' runtime IPC path cannot drift.
#'
#' @param root Absolute path to the app root.
ts_drive_boot <- function(root = getwd()) {
  .ts_drive_state$root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  .ts_drive_state$selected_token <- NULL
  invisible(.ts_drive_state$root)
}

#' App root used by every drive path.
#'
#' Falls back to `getwd()` only when `ts_drive_boot()` was never called (e.g.
#' a unit test sourcing this file alone). Never silently returns NA.
ts_drive_root <- function() {
  if (!is.null(.ts_drive_state$root)) return(.ts_drive_state$root)
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

#' Absolute path of one runtime IPC file (`ready.json`, `arm.json`, ...).
ts_drive_path <- function(...) {
  file.path(ts_drive_root(), TS_DRIVE_DIR, ...)
}

#' Create `tools/_drive/` if needed. Returns the directory, invisibly.
ts_drive_ensure_dir <- function() {
  dir.create(ts_drive_path(), showWarnings = FALSE, recursive = TRUE)
}


# =============================================================================
# JSON read / write — atomic only, jsonlite (already in the lockfile)
# =============================================================================

#' Read one JSON file, never throwing.
#'
#' @return The parsed list, or NULL when the file is absent/unreadable/not
#'   JSON. Callers treat NULL as "nothing to do" — a malformed file must not
#'   take the session down (spec S1: no `stop()` on the drive path).
ts_drive_read_json <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) { ts_log_swallow("drive_watcher.ts_drive_read_json", e); NULL }
  )
}

#' Hard cap on the number of attempts for one wire write.
#'
#' MEASURED, not guessed: an antivirus/indexer (or a concurrent reader) can hold
#' the just-written temporary file for a few milliseconds, so a single attempt is
#' not enough. Eight attempts with a linearly growing backoff span ~0.56 s, which
#' is far below the 15 s heartbeat timeout while still being a ceiling — a write
#' that cannot land must FAIL, not spin forever.
TS_DRIVE_WRITE_ATTEMPTS <- 8L

#' Wall-clock ceiling for the whole retry loop, in seconds.
#'
#' The attempt cap alone is NOT a bound on time, and that was MEASURED the hard
#' way: `file.rename()` onto an existing destination takes **~5.1 s to fail** on
#' this Windows host, so an 8-attempt budget spent **41.4 s** inside a single
#' protocol beat — inside the Shiny observer, where it freezes the app. The
#' budget makes the worst case one attempt; the cheap pre-flight check in
#' `ts_drive_write_attempt()` normally removes even that.
TS_DRIVE_WRITE_BUDGET_S <- 1.0

#' Backoff between write attempts, in seconds (multiplied by the attempt index).
TS_DRIVE_WRITE_BACKOFF_S <- 0.02

#' Record a sanitized write failure.
#'
#' Sanitized because this value is reachable from the wire (`result.json`) and
#' from the console log: an absolute path or a session token must not travel
#' with it. Reuses the badge sanitizer rather than growing a second redactor
#' that could drift from the first.
#'
#' @param path Destination that failed.
#' @param message Raw failure message (sanitized here, never stored raw).
#' @param attempts Number of attempts spent.
ts_drive_note_write_error <- function(path, message, attempts) {
  .ts_drive_state$write_error <- list(
    at       = ts_drive_now_iso(),
    file     = basename(path),
    message  = ts_drive_badge_sanitize(message),
    attempts = as.integer(attempts)
  )
  invisible(.ts_drive_state$write_error)
}

#' Forget the last write failure. Called on the first SUCCESS.
ts_drive_clear_write_error <- function() {
  .ts_drive_state$write_error <- NULL
  invisible(NULL)
}

#' Sanitized description of the last FAILED wire write, or `NULL` when the
#' session has had no write failure.
#'
#' This is the accessor the fix is required to expose: without it a failed
#' `ready.json` write is indistinguishable from an idle session, which is how
#' the first-arm defect stayed invisible.
ts_drive_last_write_error <- function() {
  .ts_drive_state$write_error
}

#' Unique temporary name for one write attempt.
#'
#' The name is unique PER ATTEMPT (pid + monotonic counter + random tag) rather
#' than the fixed `<dest>.tmp` used by the first version. A fixed name is shared
#' by every writer of that destination, so a leftover tmp from a killed process
#' — or a second tab writing the same file — makes two writers fight over one
#' name, and the loser's rename fails against a file the winner already moved.
#' Uniqueness removes the collision by construction instead of retrying into it.
ts_drive_tmp_name <- function(path) {
  .ts_drive_state$write_attempts <- .ts_drive_state$write_attempts + 1L
  tag <- paste(sample(c(letters, 0:9), 6L, replace = TRUE), collapse = "")
  sprintf("%s.%d.%d.%s.tmp", path, Sys.getpid(), .ts_drive_state$write_attempts, tag)
}

#' Normalise text for the read-back comparison.
#'
#' `writeLines()` terminates every line; `readLines()` drops the terminator and
#' any trailing empty line. Comparing the raw strings would therefore report a
#' mismatch on a perfectly good write, so both sides are normalised the same way.
ts_drive_norm_text <- function(x) {
  paste(strsplit(gsub("\r\n", "\n", as.character(x)), "\n", fixed = TRUE)[[1]],
        collapse = "\n")
}

#' One write attempt: unique tmp -> close -> unlink dest -> rename -> read back.
#'
#' Split out of `ts_drive_write_json()` so the retry loop has a single, testable
#' unit and the read-back verification has exactly one home.
#'
#' @param txt Serialized payload.
#' @param path Destination.
#' @param tmp Unique temporary path for THIS attempt.
#' @return list(ok = logical, error = character or NULL).
ts_drive_write_attempt <- function(txt, path, tmp) {
  # `con` is initialised in THIS frame, before the attempt. That is not
  # cosmetic. MEASURED 2026-09-23: when `file(tmp, open = "wb")` ITSELF throws
  # (an unwritable destination — the missing-parent case the tests exercise),
  # the local `con` is never assigned, so the handler's `close(con)` resolved
  # LEXICALLY, up through the writer's enclosing environments, and closed an
  # unrelated connection that merely shared the name. The victim was real:
  # `tools/run_full_suite.R` keeps its results connection in a global called
  # `con`, so one failed write closed it, the next `writeLines()` raised
  # "invalid connection", and the suite aborted at file 56 of 137 — while the
  # drive test file itself reported 570 passing assertions. Initialising here
  # makes the handler find a NULL it OWNS instead of a stranger's handle.
  # The rule this encodes: the writer may only close what the writer opened.
  con <- NULL
  # 1. Write to the temporary name.
  ok <- tryCatch({
    con <- file(tmp, open = "wb")
    # Close BEFORE the rename: on Windows the rename fails with "le processus
    # ne peut pas accéder au fichier car ce fichier est utilisé par un autre
    # processus" while the handle is open, and an `on.exit(close(con))` runs
    # AFTER the rename (the first version did exactly that and every write
    # failed silently).
    writeLines(txt, con = con, useBytes = TRUE)
    flush(con)
    close(con)
    TRUE
  }, error = function(e) {
    if (!is.null(con)) try(close(con), silent = TRUE)
    conditionMessage(e)
  })
  if (!isTRUE(ok)) {
    return(list(ok = FALSE, error = sprintf("write failed: %s", ok)))
  }

  # 2. The destination must be gone first: `file.rename()` does not overwrite an
  #    existing file on Windows, whereas POSIX would.
  #
  #    PRE-FLIGHT, and it is not an optimisation. MEASURED: `file.rename()` onto
  #    a destination that cannot be replaced (a directory, a read-only file, a
  #    file held open) spends **~5.1 s** before reporting failure — 41.4 s for
  #    the full 8-attempt budget, inside the Shiny observer. `unlink()` tells us
  #    the same thing in microseconds, so the doomed rename is never issued.
  if (file.exists(path)) {
    removed <- suppressWarnings(try(unlink(path), silent = TRUE))
    if (!identical(removed, 0L) && file.exists(path)) {
      return(list(ok = FALSE,
                  error = "target exists and cannot be removed (locked or read-only)"))
    }
  }
  reason <- NULL
  # The rename is still wrapped because on Windows it reports failure as a
  # WARNING, not as an error. That is not cosmetic: testthat 3e promotes a
  # warning to a failure, and the warning text carries the only useful
  # diagnostic this path can produce ("Accès refusé"). It is therefore CAPTURED
  # into the recorded error rather than muffled and forgotten.
  renamed <- withCallingHandlers(
    tryCatch(file.rename(tmp, path), error = function(e) FALSE),
    warning = function(w) {
      reason <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  if (!isTRUE(renamed)) {
    return(list(ok = FALSE,
                error = sprintf("rename failed: %s",
                                if (is.null(reason)) {
                                  "destination locked or not overwritable"
                                } else {
                                  reason
                                })))
  }

  # 3. VERIFY the destination by reading it back.
  #
  # A rename that reports success while the destination holds someone else's
  # bytes is precisely the silent failure this change exists to stop, and only
  # a read-back can see it. Without this step "written" means "we asked".
  back <- tryCatch(readLines(path, warn = FALSE), error = function(e) NULL)
  if (is.null(back)) {
    return(list(ok = FALSE, error = "read-back failed after rename"))
  }
  if (!identical(ts_drive_norm_text(txt),
                 ts_drive_norm_text(paste(back, collapse = "\n")))) {
    return(list(ok = FALSE, error = "read-back differs after rename"))
  }
  list(ok = TRUE, error = NULL)
}

#' Write one JSON file atomically (spec §2.3, S8).
#'
#' Full payload -> unique `<dest>.<pid>.<n>.<tag>.tmp` -> close -> unlink dest
#' -> `file.rename` -> read back.
#'
#' Two Windows rules, both measured (a first version got this wrong and every
#' write silently failed):
#'
#'  1. The temporary connection MUST be CLOSED BEFORE `file.rename()`. On
#'     Windows the rename fails with "le processus ne peut pas accéder au
#'     fichier car ce fichier est utilisé par un autre processus" while the
#'     handle is open. An `on.exit(close(con))` is too late — it runs after the
#'     rename. Close explicitly, then rename.
#'  2. `unlink()` the destination first: `file.rename()` does not overwrite an
#'     existing file on Windows, whereas POSIX would.
#'
#' A failure is NEVER silent: the attempt budget is bounded, the failure is
#' recorded (sanitized) by `ts_drive_note_write_error()`, and the caller can ask
#' for it through `ts_drive_last_write_error()`. The caller must then decline to
#' treat the write as done — see the heartbeat in `ts_drive_attach()`.
#'
#' @param obj Object to serialize.
#' @param path Destination.
#' @param attempts Hard cap on attempts (default 8).
#' @param budget_s Wall-clock ceiling for the whole loop (default 1 s).
#' @return TRUE when the destination verifiably holds the new payload.
ts_drive_write_json <- function(obj, path, attempts = TS_DRIVE_WRITE_ATTEMPTS,
                                budget_s = TS_DRIVE_WRITE_BUDGET_S) {
  ts_drive_ensure_dir()
  txt <- tryCatch(
    jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null", pretty = TRUE),
    error = function(e) NULL
  )
  if (is.null(txt)) {
    ts_drive_note_write_error(path, "payload could not be encoded to JSON", 0L)
    return(FALSE)
  }

  n <- suppressWarnings(as.integer(attempts))
  if (length(n) != 1L || is.na(n) || n < 1L) n <- TS_DRIVE_WRITE_ATTEMPTS
  budget <- suppressWarnings(as.numeric(budget_s))
  if (length(budget) != 1L || is.na(budget) || budget < 0) {
    budget <- TS_DRIVE_WRITE_BUDGET_S
  }

  t0 <- as.numeric(Sys.time())
  spent <- 0L
  last <- NULL
  # TWO bounds, because an attempt cap is not a bound on TIME: MEASURED, a
  # single doomed `file.rename()` costs ~5.1 s on this host, so 8 attempts spent
  # 41.4 s inside one protocol beat. The loop therefore stops as soon as the
  # wall-clock budget is exhausted, but always makes at least ONE attempt —
  # otherwise the budget could suppress the write entirely.
  for (i in seq_len(n)) {
    tmp <- ts_drive_tmp_name(path)
    res <- ts_drive_write_attempt(txt, path, tmp)
    spent <- spent + 1L
    # A failed attempt must not leave its tmp behind: a stale tmp is exactly
    # what makes a later writer collide, so cleanup is part of the retry
    # contract and not an afterthought. Wrapped because `unlink()` warns (not
    # errors) when it cannot remove.
    if (file.exists(tmp)) suppressWarnings(try(unlink(tmp), silent = TRUE))
    if (isTRUE(res$ok)) {
      ts_drive_clear_write_error()
      return(TRUE)
    }
    last <- res$error
    if (i < n && (as.numeric(Sys.time()) - t0) < budget) {
      Sys.sleep(TS_DRIVE_WRITE_BACKOFF_S * i)
    } else {
      break
    }
  }
  ts_drive_note_write_error(path, last, spent)
  FALSE
}

#' mtime of a file, or NA when absent. Cheap idle probe.
ts_drive_mtime <- function(path) {
  if (!file.exists(path)) return(NA_real_)
  as.numeric(file.info(path)$mtime)
}

#' UTC timestamp in the wire format used by ready.json / result.json.
ts_drive_now_iso <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' Per-session token: 8 lowercase alphanumerics (spec §2.1 example).
ts_drive_new_token <- function() {
  paste(sample(c(letters, 0:9), 8L, replace = TRUE), collapse = "")
}


# =============================================================================
# ready.json — app -> agent
# =============================================================================

#' The closed vocabulary of client-visibility modes.
#'
#' TWO supported modes share ONE IPC contract (spec §4): the same `ready.json`,
#' `arm.json`, `scenario.json` and `result.json`, the same session-selection and
#' heartbeat rules, the same passive badge. Only the CLIENT's visibility differs,
#' which is why this is the only field that may:
#'
#'   - `headless` — `launch.browser = FALSE`, a real httpuv server plus a real
#'     chromote client, no visible window: reproducible unattended runs.
#'   - `visible`  — RStudio Viewer or a visible localhost Chrome/Edge tab.
#'   - `unknown`  — nobody declared it, and `interactive()` could not decide.
#'
#' A declaration outside this set is reported as `unknown`, NEVER coerced:
#' telling an agent "headless" while a human is watching would be worse than
#' telling it nothing.
TS_DRIVE_VIEWERS <- c("headless", "visible", "unknown")

#' Which visibility mode is this session running under?
#'
#' Declared wins over derived, and the derived value uses the SAME gate that
#' decides whether a dev badge can ever render (`ts_drive_interactive()`), so the
#' two can never disagree about whether a human is present. Under `Rscript` that
#' gate is FALSE, i.e. `headless` — which is exactly the Mode 1 launch.
#'
#' `tools/launch_dev_drive.R` is the only supported way to DECLARE it.
#' Deliberately NOT readable from a scenario payload: this is a property of the
#' session, not of a request, and a scenario that could relabel itself would make
#' the field worthless to an agent deciding whether a human is watching.
ts_drive_viewer <- function() {
  v <- getOption("ts.drive.viewer", NULL)
  if (is.null(v)) v <- Sys.getenv("TRANSCRIPTO_DEV_DRIVE_VIEWER", "")
  if (is.character(v) && length(v) == 1L && nzchar(v)) {
    v <- tolower(trimws(v))
    return(if (v %in% TS_DRIVE_VIEWERS) v else "unknown")
  }
  if (isTRUE(interactive())) "visible" else "headless"
}

#' Write `ready.json` for this session.
#'
#' Last connected session wins: several tabs/sessions overwrite the same file,
#' and a scenario may therefore pin `session_token` to address one of them.
#'
#' ## Heartbeat
#'
#' `hb_at` + `hb_n` turn the handshake into a liveness proof. A `ready.json`
#' left behind by a process that died mid-session is **byte-identical to a live
#' one** from the agent's point of view; only a timestamp that keeps moving can
#' tell them apart. Without it the agent's only option would be scraping stdout,
#' which the whole protocol exists to avoid (Rscript exits 139 on teardown and
#' loses buffered stdout).
#'
#' Monotonicity is the load-bearing part: `hb_n` must never restart, so the
#' agent can distinguish "same session, still alive" from "a NEW session
#' reusing the same pid". `started_at` therefore stays fixed for the session's
#' life while `hb_at` moves.
#'
#' @param session The Shiny session.
#' @param token The session token.
#' @param armed Whether the session is armed.
#' @param last_seq Last scenario sequence consumed; mirrored for diagnostics.
#' @param hb_n Heartbeat counter; `NULL` starts at 0L, non-NULL preserves the
#'   caller's counter so it can only ever increase (never reset by a rewrite).
#' @param started_at Session start, preserved across heartbeat rewrites.
#' @return The payload written, invisibly.
ts_drive_write_ready <- function(session, token, armed = FALSE, last_seq = 0L, hb_n = NULL, started_at = NULL) {
  # NOTE ON PLACEMENT (convention C2): `session$` must be read while the guard
  # still recognises `session` as a FORMAL of this function. The guard derives
  # the enclosing scope from the SIGNATURE LINE — when a signature is split
  # across lines, the `{` on the continuation line is never attributed to the
  # definition, so the body is treated as file scope and every `session$` /
  # `input$` is reported as unbound. Keeping the signature on ONE line is the
  # fix; it is a property of the guard, not a style preference.
  port <- tryCatch(session$request$REMOTE_PORT, error = function(e) NULL)
  if (is.null(port)) port <- tryCatch(session$clientData_port(), error = function(e) NULL)

  prev <- if (is.null(hb_n) || is.null(started_at)) ts_drive_read_ready() else NULL

  n <- if (!is.null(hb_n)) {
    as.integer(hb_n)
  } else if (!is.null(prev) && !is.null(prev$hb_n) &&
             identical(as.character(prev$session_token), as.character(token))) {
    # Same session: continue its counter. A different token means a new
    # session owns the file, so the counter legitimately restarts.
    as.integer(prev$hb_n)
  } else {
    0L
  }
  if (is.na(n) || n < 0L) n <- 0L

  started <- if (!is.null(started_at)) {
    as.character(started_at)
  } else if (!is.null(prev) && !is.null(prev$started_at) &&
             identical(as.character(prev$session_token), as.character(token))) {
    as.character(prev$started_at)
  } else {
    ts_drive_now_iso()
  }

  payload <- list(
    protocol      = TS_DRIVE_PROTOCOL,
    armed         = isTRUE(armed),
    pid           = Sys.getpid(),
    port          = port,
    root          = ts_drive_root(),
    session_token = token,
    # WHICH of the two supported visibility modes this session is running
    # under. Was hardcoded "unknown" — present in every payload, carrying
    # nothing. See TS_DRIVE_VIEWERS.
    viewer        = ts_drive_viewer(),
    last_seq      = as.integer(last_seq),
    started_at    = started,
    hb_at         = ts_drive_now_iso(),
    hb_n          = n,
    hb_timeout_s  = ts_drive_hb_timeout()
  )
  ok <- ts_drive_write_json(payload, ts_drive_path("ready.json"))
  attr(payload, "written") <- isTRUE(ok)
  payload
}

#' Read `ready.json` back.
ts_drive_read_ready <- function() {
  ts_drive_read_json(ts_drive_path("ready.json"))
}

#' Heartbeat freshness timeout, in seconds.
#'
#' Configurable **only** through the development protocol — an option set in the
#' dev process, or the `TRANSCRIPTO_DEV_DRIVE_HB_TIMEOUT` env var. It is
#' deliberately NOT readable from a scenario payload: a scenario that could
#' widen its own liveness window would let a stale file look alive, which is the
#' exact failure the heartbeat exists to prevent.
#'
#' Default 15 s ≈ 3 missed beats at the 5 s cadence, so a single slow write does
#' not produce a false "stale" verdict.
ts_drive_hb_timeout <- function() {
  v <- getOption("ts.drive.hb_timeout", NULL)
  if (is.null(v)) v <- Sys.getenv("TRANSCRIPTO_DEV_DRIVE_HB_TIMEOUT", "")
  n <- suppressWarnings(as.numeric(v))
  if (length(n) != 1L || is.na(n) || n <= 0) return(15)
  n
}

#' Heartbeat write interval, in seconds.
#'
#' The poller beats every `poll_ms` (800 ms), but `ready.json` must NOT be
#' rewritten that often — the spec asks for ~2-5 s. Configurable through the
#' dev protocol only, for the same reason as the timeout.
ts_drive_hb_interval <- function() {
  v <- getOption("ts.drive.hb_interval", NULL)
  if (is.null(v)) v <- Sys.getenv("TRANSCRIPTO_DEV_DRIVE_HB_INTERVAL", "")
  n <- suppressWarnings(as.numeric(v))
  if (length(n) != 1L || is.na(n) || n <= 0) return(3)
  n
}

#' Advance the heartbeat throttle — the PURE rule, and the fix's real guard.
#'
#' The throttle exists so the poller does not rewrite `ready.json` every 800 ms;
#' `hb_at` is the time of the last beat it counted. The whole question is: **is
#' `hb_at` the last ATTEMPT or the last SUCCESS?**
#'
#' It used to be the last attempt, and by accident rather than intent: the
#' caller's guard was `!inherits(wrote, "try-error")` around a function that
#' RETURNS its payload instead of throwing, so the guard was **always true**.
#' Any failed write would therefore have silenced the heartbeat for a whole
#' interval and left a stale-but-plausible handshake on disk. Answering "the
#' last success" makes the next beat (800 ms) retry instead.
#'
#' HONEST SCOPE — this was a LATENT bug, not the cause of the first-arm symptom.
#' The symptom (a stale `armed:false, hb_n:0` right after session start) was
#' measured on a clean session to be the app's SLOW BOOT: `server()` does heavy
#' init and Shiny starts no reactive flush until it returns, so no beat had run
#' yet. The arm was honoured 22.4 s later with ZERO write failures logged. This
#' rule is still right, and it is now exercised live by deliberately obstructing
#' `ready.json`: the handshake repairs itself on the next beat (1.0 s) with no
#' re-arm.
#'
#' Kept as a pure function, like the badge transition table, so the property is
#' testable WITHOUT a wall-clock race — the alternative is a test that has to
#' sleep longer than a real rename failure (5.1 s, measured) to tell the two
#' behaviours apart.
#'
#' @param now Current time, numeric.
#' @param hb_at Throttle timestamp from the previous beat.
#' @param wrote_ok Did the write VERIFIABLY land?
#' @return The throttle timestamp for the next beat.
ts_drive_hb_next_at <- function(now, hb_at, wrote_ok) {
  if (isTRUE(wrote_ok)) as.numeric(now) else hb_at
}

#' How old is the handshake, in seconds? `Inf` when unreadable.
ts_drive_ready_age <- function(now = Sys.time()) {
  hb <- ts_drive_read_ready()
  if (is.null(hb) || is.null(hb$hb_at)) return(Inf)
  t0 <- tryCatch(as.POSIXct(as.character(hb$hb_at),
                            format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                 error = function(e) { ts_log_swallow("drive_watcher.heartbeat_age", e); NA })
  if (length(t0) != 1L || is.na(t0)) return(Inf)
  as.numeric(difftime(now, t0, units = "secs"))
}

#' Is the handshake alive, i.e. is its heartbeat recent enough?
#'
#' This is the AGENT-side gate (spec §7): a `ready.json` older than the timeout
#' is rejected outright, so a later scenario can never be applied to a session
#' that has already died (acceptance 18/19).
#'
#' @param now Injectable clock, so freshness is testable without sleeping.
#' @param timeout_s Override; defaults to the dev-protocol setting.
#' @return TRUE only when the file exists, parses, carries a protocol match and
#'   is within the timeout.
ts_drive_ready_fresh <- function(now = Sys.time(), timeout_s = ts_drive_hb_timeout()) {
  hb <- ts_drive_read_ready()
  if (is.null(hb)) return(FALSE)
  if (!identical(as.character(hb$protocol), TS_DRIVE_PROTOCOL)) return(FALSE)
  age <- ts_drive_ready_age(now = now)
  is.finite(age) && age >= -60 && age <= timeout_s
}

#' Remove `ready.json` at session end (spec §2.1 / G0 acceptance 3).
#'
#' A leftover `ready.json` from a dead pid must not be trusted: the agent
#' compares `pid` + `started_at` before arming, so removing the file is the
#' honest signal that no session is listening any more.
ts_drive_invalidate_ready <- function(token = NULL) {
  cur <- ts_drive_read_ready()
  if (!is.null(cur) && !is.null(cur$session_token) &&
      !is.null(token) && !identical(as.character(cur$session_token), token)) {
    # Another (newer) session owns the file — do not delete its handshake.
    return(invisible(FALSE))
  }
  try(unlink(ts_drive_path("ready.json")), silent = TRUE)
  invisible(TRUE)
}


# =============================================================================
# arm.json — agent -> app
# =============================================================================

#' Interactivity of the CURRENT R session.
#'
#' Overridable so the arm gate is testable: `Rscript` is never interactive, so
#' a non-overridable `interactive()` would make "the token matches" unreachable
#' from a test — and an untestable gate is not a gate. The production path
#' always reads `base::interactive()`.
ts_drive_interactive <- function() {
  ov <- getOption("ts.drive.interactive", NULL)
  if (!is.null(ov)) return(isTRUE(ov))
  isTRUE(interactive())
}

#' Arm-state decision for the current tick.
#'
#' All four conditions must hold (spec §1 + arm rule):
#'   1. `ts_drive_interactive()` is TRUE (Run App / local dev). This is the
#'      ONLY gate that keeps production inert — deliberately NOT
#'      `options(shiny.testmode)` and NOT an env var written into the committed
#'      `.Renviron`.
#'   2. `arm.json` exists and validates as `ts-drive/1`.
#'   3. `armed` is TRUE (writing `false` or deleting the file disarms).
#'   4. The token matches this session — OR the wildcard `"*"` is used AND
#'      `TRANSCRIPTO_DEV_DRIVE=1`, which only `tools/launch_dev_drive.R` sets.
#'
#' @return list(armed = logical, reason = character)
ts_drive_arm_state <- function(token) {
  if (!ts_drive_interactive()) {
    return(list(armed = FALSE, reason = "non-interactive session"))
  }
  arm <- ts_drive_read_json(ts_drive_path("arm.json"))
  if (is.null(arm)) return(list(armed = FALSE, reason = "no arm.json"))

  proto <- as.character(arm$protocol %||% "")
  if (!identical(proto, TS_DRIVE_PROTOCOL)) {
    return(list(armed = FALSE, reason = sprintf("protocol %s, expected %s", proto, TS_DRIVE_PROTOCOL)))
  }
  if (!isTRUE(arm$armed)) return(list(armed = FALSE, reason = "arm.json says armed=false"))

  arm_token <- as.character(arm$token %||% "")
  if (identical(arm_token, token)) return(list(armed = TRUE, reason = "token match"))

  wildcard_ok <- identical(arm_token, "*") &&
    identical(Sys.getenv("TRANSCRIPTO_DEV_DRIVE"), "1")
  if (wildcard_ok) return(list(armed = TRUE, reason = "wildcard token (dev launcher)"))

  list(armed = FALSE, reason = sprintf("token mismatch (%s)", arm_token))
}


# =============================================================================
# scenario.json — agent -> app
# =============================================================================

#' Allowed `action` values (spec §2.3, frozen).
#' R1: `read_export` joins the enum — the bounded read (option i, D1 approuvé).
#' The handler lands in R2; until then a dispatched read_export receives an
#' honest `invalid` verdict from the unhandled-action branch, never silence.
TS_DRIVE_ACTIONS <- c("noop", "set_inputs", "run_pipeline", "import_file",
                      "snapshot", "reset_module", "export_result",
                      "read_export")

#' The `result.json` status enum (spec §2.4, FROZEN).
#'
#' Declared once so the six values are checkable rather than spread as 27
#' literals through this file. An agent switches on these names; a seventh
#' would be an unhandled case in every client, so the set is pinned by a test.
TS_DRIVE_STATUSES <- c("ignored", "invalid", "applied", "running", "done",
                       "error")

#' Is a `result.json` status TERMINAL for its `seq`?
#'
#' This is the distinction a polling agent depends on (spec §2.4):
#'   * `applied` (inputs updated, pipeline NOT finished) and `running` (a job
#'     has started) ACKNOWLEDGE the seq but are NOT terminal — keep polling;
#'   * `done` / `error` are terminal for that seq;
#'   * `ignored` / `invalid` are terminal refusals — nothing further will happen.
#' An UNKNOWN status is deliberately NOT terminal: a client that does not
#' recognise a value must keep waiting rather than mistake it for completion.
#'
#' @param status A status string, possibly `NA`.
#' @return TRUE only for the four terminal values.
ts_drive_status_terminal <- function(status) {
  if (length(status) != 1L || is.na(status)) return(FALSE)
  as.character(status) %in% c("done", "error", "ignored", "invalid")
}

#' Validate one scenario payload against the frozen schema.
#'
#' Never throws (spec S1). Every rejection becomes an `errors[]` entry so the
#' agent can read WHY from `result.json` instead of guessing.
#'
#' @param scn Parsed `scenario.json` (list) or NULL.
#' @param token This session's token.
#' @param last_seq Highest seq already applied in this session.
#' @return list(ok = logical, status = "invalid"|"ignored"|"applied-candidate",
#'   errors = character(), warnings = character(), scenario = list-or-NULL)
ts_drive_validate_scenario <- function(scn, token, last_seq) {
  errors <- character(0); warnings <- character(0)

  if (is.null(scn) || !is.list(scn)) {
    return(list(ok = FALSE, status = "invalid",
                errors = "scenario.json absent or not a JSON object",
                warnings = warnings, scenario = NULL))
  }

  proto <- as.character(scn$protocol %||% "")
  if (!identical(proto, TS_DRIVE_PROTOCOL)) {
    # Unknown protocol => IGNORE (spec §2.3 bullet 3), not invalid.
    return(list(ok = FALSE, status = "ignored",
                errors = character(0),
                warnings = sprintf("unknown protocol '%s'", proto),
                scenario = NULL))
  }

  seq <- suppressWarnings(as.numeric(scn$seq %||% NA_real_))
  if (is.na(seq)) {
    errors <- c(errors, "missing or non-numeric `seq`")
  } else if (seq <= last_seq) {
    # Stale seq => IGNORE, log, result.status = ignored (G1 acceptance 8).
    return(list(ok = FALSE, status = "ignored", errors = character(0),
                warnings = sprintf("stale seq %s (<= last_seq %s)", seq, last_seq),
                scenario = NULL))
  }

  # `action` is resolved BEFORE the token comparison, because S2 makes the
  # comparison ACTION-DEPENDENT: `export_result` writes a file, so it must be
  # pinned to a session, while every other action may still be addressed without
  # a token (the one-shot scenario an operator drops in by hand, and the
  # human-driven path). Reading `action` after the comparison made the
  # distinction inexpressible.
  action <- as.character(scn$action %||% "")
  if (!action %in% TS_DRIVE_ACTIONS) {
    errors <- c(errors, sprintf("unknown action '%s' (allowed: %s)",
                                action, paste(TS_DRIVE_ACTIONS, collapse = ", ")))
  }

  declared_token <- as.character(scn$session_token %||% "")
  # The actions that MUTATE the session are pinned; see
  # `TS_DRIVE_TOKEN_PINNED_ACTIONS` for the measurement and for why the other five
  # deliberately keep the token-optional affordance.
  if (action %in% TS_DRIVE_TOKEN_PINNED_ACTIONS && !nzchar(declared_token)) {
    errors <- c(errors, sprintf(
      "`%s` changes this session, so it must carry the session_token of the session that owns it; a %s addressed to no session is refused",
      action, action))
  } else if (nzchar(declared_token) && !identical(declared_token, token)) {
    # The declared token is NOT echoed: it is a credential, and an error message is
    # copied into logs, transcripts and bug reports.
    errors <- c(errors, "session_token mismatch (the declared token is not this session's)")
  }

  module <- as.character(scn$module %||% "")
  # R1/R2: `read_export` is deliberately MODULE-LESS — it targets the current
  # export verdict and names no module (the agent could not echo a handle or a
  # route anyway: the stems carry 8+-character runs the sanitiser redacts).
  # Every other action still requires one; a module handed to read_export is
  # simply ignored downstream.
  if (!nzchar(module) && !identical(action, "read_export")) {
    errors <- c(errors, "missing `module`")
  } else if (nzchar(module) && !module %in% TS_DRIVE_MODULES) {
    errors <- c(errors, sprintf(
      "module '%s' outside the drive allowlist (allowed: %s)",
      module, paste(TS_DRIVE_MODULES, collapse = ", ")))
  }

  # preserve_data default TRUE — forbidding silent wipes is a construction
  # gate, not a nicety (spec §2.3). A non-boolean is refused loudly.
  preserve <- scn$preserve_data %||% TRUE
  if (!is.logical(preserve) || length(preserve) != 1L || is.na(preserve)) {
    errors <- c(errors, "`preserve_data` must be TRUE or FALSE")
  }

  inputs <- scn$inputs %||% list()
  if (!is.list(inputs)) {
    errors <- c(errors, "`inputs` must be a JSON object (inputId -> value)")
    inputs <- list()
  }

  # Allowlist check (spec S3/S4): each offending key is reported and skipped;
  # the session stays up and the remaining keys are still applied.
  bad_keys <- character(0); skipped <- character(0)
  for (k in names(inputs)) {
    entry <- ts_drive_allowlist_get(k)
    if (is.null(entry)) {
      bad_keys <- c(bad_keys, k)
      next
    }
    if (!identical(entry$module, module)) {
      skipped <- c(skipped, sprintf("%s belongs to '%s', not '%s'", k,
                                    entry$module, module))
    }
  }
  if (length(bad_keys)) {
    errors <- c(errors, sprintf(
      "unknown inputId(s) not on the driver allowlist: %s",
      paste(bad_keys, collapse = ", ")))
  }
  warnings <- c(warnings, skipped)

  # `import_file` carries a PATH, not a widget value (spec G3/S5), so it has its
  # own validated block. Validated HERE rather than in the injector for the same
  # reason `inputs` is: the refusal must reach `result.errors[]` before anything
  # is attempted, and a path is exactly the kind of value that must never be
  # handed downstream unexamined (spec S11).
  imp <- NULL
  if (identical(action, "import_file")) {
    # The MODULE routes the payload, because one flat key vector cannot describe
    # two importers with different shapes: `import_bulk` needs a counts FILE,
    # `import_spatial` needs a data FOLDER. Handing either the other's key would
    # be accepted by a global whitelist and then fail inside the loader.
    iv <- ts_drive_validate_import(scn$import, module = module)
    errors <- c(errors, iv$errors)
    if (length(iv$import)) imp <- iv$import
  } else if (identical(action, "export_result")) {
    # S2: `export_result` takes NO request field, and the refusal has to happen
    # HERE, on the seam the poller actually calls.
    #
    # It used to happen further down, in `ts_drive_apply()`, on `scn$import` -
    # but the whitelist above fills `imp` only for `import_file`, so for an
    # export the field was ALWAYS NULL, and the validator opens with
    # `if (is.null(req)) return(ok = TRUE)`. MEASURED consequence: the refusal
    # was unreachable dead code, and a caller's `path` was dropped by the
    # whitelist SILENTLY - which is the outcome the validator's own
    # documentation refuses to accept, because "I asked for
    # `filename: ../../x` and the tool said OK" is how an export ends up
    # somewhere nobody chose. The unit test missed it by calling the validator
    # directly, a path the live route never reaches with a non-NULL request.
    ev <- ts_drive_validate_export_request(scn$import)
    errors <- c(errors, ev$errors)
    if (ev$ok) imp <- ev$request
  }

  status <- if (length(errors)) "invalid" else "applied-candidate"
  # The rebuilt scenario is a WHITELIST, and that is deliberate: a field the
  # injector never reads must not reach it. The list below is therefore the
  # contract, and anything omitted here is silently unreachable downstream.
  #
  # `expect` (spec §2.3) and `button` were omitted by the first version, and
  # both omissions were SILENT — no error, no warning, just the wrong click
  # site. `ts_drive_apply()` fell back to the module's default button, so
  # `bulk-pathways-run_scores` could never be fired (that module defaults to
  # `run_pathway`), and `ts_drive_nav_plan()` never saw a requested tab. Found
  # on the live session: a scenario naming `run_scores` came back for
  # `run_pathway`. Both are carried through unchanged now, with only their
  # TYPE checked here; `button` is validated against TS_DRIVE_BUTTONS by the
  # injector, which owns that refusal.
  list(ok = !length(errors), status = status, errors = errors,
       warnings = warnings,
       scenario = list(
         seq = if (is.na(seq)) NULL else as.integer(seq),
         module = module,
         action = action,
         preserve_data = preserve,
         inputs = inputs,
         # Keys the injector WILL attempt: allowlisted AND owned by `module`.
         # An allowlisted key from another module is refused, never applied.
         inputs_ok = as.list(inputs[setdiff(names(inputs), c(bad_keys, sub(" .*$", "", skipped)))]),
         expect = if (is.list(scn$expect)) scn$expect else NULL,
         button = if (is.character(scn$button) && length(scn$button) == 1L &&
                     !is.na(scn$button) && nzchar(scn$button)) scn$button else NULL,
         # The WHITELISTED import block (G3), NULL for every other action.
         import = imp
       ))
}


# =============================================================================
# result.json — app -> agent (field freeze, spec §2.4)
# =============================================================================

#' Write a terminal result for one `seq`.
#'
#' @param seq The ack'd sequence number.
#' @param status One of the frozen enum.
#' @param active_module Module that handled the scenario.
#' @param armed Whether the poller is currently armed.
#' @param preserve_data Echo of the scenario flag.
#' @param errors,warnings Character vectors (empty = none).
#' @param snapshot Object snapshot, or NULL.
#' Project an export descriptor onto the REDACTED set that may travel the wire.
#'
#' PURE, and deliberately so: it takes a descriptor and returns a list, writing
#' nothing. `ts_drive_write_result()` calls it, and the offline tests call it too —
#' a test that exercised the descriptor by invoking the real writer would have to
#' write `tools/_drive/result.json`, which is the LIVE drive of a running session.
#' That is not a test artifact, and putting it in a unit test would have made the
#' suite unusable next to a live app.
#'
#' Two rules, and the second is defence in depth:
#'   1. only the DECLARED keys travel, so widening an exporter's return value
#'      cannot widen the wire;
#'   2. every string goes through `ts_drive_badge_sanitize()` even though the
#'      exporter is built not to carry a path — a field that is safe by
#'      construction today is one constructor change away from not being.
#' Project a descriptor onto the wire: DECLARED keys only, every string sanitised.
#'
#' The single implementation of the two rules, shared by the export descriptor and
#' the module-state descriptor. Split out rather than written twice for the reason
#' `.ts_drive_resolve_import_path()` was: "two copies of this function would be two
#' places where a future edit to the security rule could be applied to one caller
#' and forgotten in the other".
#'
#' @param descriptor The descriptor a producer built, or NULL.
#' @param keep Character vector of the keys allowed on the wire.
#' @return The projected list, or NULL when there is nothing to send.
#' @noRd
ts_drive_project_descriptor <- function(descriptor, keep, verbatim = character(0)) {
  if (is.null(descriptor) || !is.list(descriptor)) return(NULL)
  d <- as.list(descriptor)[intersect(keep, names(descriptor))]
  lapply(d, function(v) {
    if (is.character(v)) {
      # A DECLARED, FROZEN vocabulary is exempt; anything else is sanitised.
      #
      # MEASURED (2026-09-27, live): the module-state path declared a `convention` of
      # "number of pipeline artefacts produced (not rows)" and it came back on the
      # wire as "number of <redacted> <redacted> <redacted> (not rows)". The
      # sanitiser redacts every 8+-character alphanumeric run, and "pipeline",
      # "artefacts" and "produced" each qualify. So the field whose entire purpose
      # is to make a count INTERPRETABLE arrived unreadable - the same defect the
      # badge had with its own label "snapshot" (STATUS.md, DRIVE README), and the
      # same fix: a frozen set is returned verbatim, BEFORE the path/token rules.
      #
      # It is an OPT-IN per field, and deliberately narrow: `kind` and `convention`
      # are compile-time constants in the module's own source. `columns` and
      # `column` are NOT exempt - a column name can be user-supplied (an annotation
      # label), so it keeps the sanitiser.
      if (length(verbatim) && all(v %in% verbatim)) return(v)
      vapply(as.character(v), function(s) ts_drive_badge_sanitize(s, 200L), character(1),
             USE.NAMES = FALSE)
    } else if (is.numeric(v)) as.numeric(v) else NULL
  })
}

#' @noRd
ts_drive_export_descriptor <- function(descriptor) {
  ts_drive_project_descriptor(
    descriptor, c("format", "file", "bytes", "n_rows", "n_cols", "n_sig", "columns"))
}

#' Project a READ block onto the wire (R2): keep-set `TS_DRIVE_READ_KEYS` only,
#' structural strings through the sanitiser, preview cells RE-GUARDED, classes
#' mapped through the frozen declared set.
#'
#' Two rules, the family resemblance to `ts_drive_export_descriptor` is
#' deliberate:
#'   1. only the DECLARED keys travel, so widening the responder's block cannot
#'      widen the wire;
#'   2. every STRUCTURAL string goes through `ts_drive_badge_sanitize()` —
#'      `route`, `handle`, and the preview's `columns`. The preview CELL VALUES
#'      are the D1 exception, and they get the opposite treatment: they pass
#'      through `ts_drive_verbatim_guard()` AGAIN, so a cell that reaches the
#'      wire has passed the guard on BOTH sides of the process boundary. The
#'      guard is idempotent on its own output and on the declared markers
#'      (`<over-200-chars>` / `<guarded>` carry no 8+-character run), so a
#'      clean block is unchanged; a cell that somehow failed twice degrades to
#'      NA (JSON null), never to prose.
#' `col_summary$class` travels verbatim only within `TS_DRIVE_READ_CLASSES`
#' (precedent: `TS_DRIVE_DESCRIPTOR_VERBATIM`); anything else reports "other".
#'
#' PURE, like the export projector: takes a block, returns a list, writes
#' nothing.
ts_drive_read_descriptor <- function(descriptor) {
  if (is.null(descriptor) || !is.list(descriptor)) return(NULL)
  d <- descriptor[intersect(TS_DRIVE_READ_KEYS, names(descriptor))]
  for (k in c("route", "handle")) {
    if (!is.null(d[[k]])) {
      d[[k]] <- ts_drive_badge_sanitize(as.character(d[[k]]), 200L)
    }
  }
  if (!is.null(d$seq)) d$seq <- suppressWarnings(as.integer(d$seq))
  # The echoed EXPORT descriptor: projected through the SAME projector that
  # produced it — defence in depth on the one nested structure the read block
  # is allowed to carry.
  d$descriptor <- ts_drive_export_descriptor(d$descriptor)
  if (!is.null(d$preview)) {
    p <- d$preview
    if (!is.null(p$columns)) {
      p$columns <- vapply(as.character(p$columns),
                          function(s) ts_drive_badge_sanitize(s, 200L),
                          character(1), USE.NAMES = FALSE)
    }
    if (!is.null(p$rows)) {
      p$rows <- lapply(p$rows, function(r) {
        g <- ts_drive_verbatim_guard(as.character(r))
        as.list(ifelse(is.na(g), NA_character_, g))
      })
    }
    d$preview <- p
  }
  if (!is.list(d$col_summary)) {
    d$col_summary <- NULL
  } else {
    d$col_summary <- lapply(d$col_summary, function(cs) {
      cls <- as.character(cs$class %||% "other")
      list(name = ts_drive_badge_sanitize(as.character(cs$name %||% ""), 200L),
           class = if (length(cls) == 1L && cls %in% TS_DRIVE_READ_CLASSES) cls else "other",
           n_missing_in_preview = suppressWarnings(
             as.integer(cs$n_missing_in_preview %||% 0L)))
    })
  }
  d
}

#' Project a module's published VOCABULARY onto the wire (Slice 3).
#'
#' The keep-set `TS_DRIVE_VOCABULARY_KEYS[[module]]` + `vocab_rev` is CLOSED,
#' like every other projection: a probe that returns one extra key cannot widen
#' the wire. The CHOICES are the D1-verbatim exception — they go through
#' `ts_drive_verbatim_guard()` (the SECOND declared consumer, same function as
#' the read preview) — but a choice that fails the guard is replaced by NA
#' (JSON `null`), NEVER dropped: dropping would desynchronise the published
#' positions from the widget's real choices, and the index the agent chose
#' would resolve to the wrong value. `vocab_rev` travels as an integer.
#'
#' A list LONGER than `TS_DRIVE_VOCAB_MAX_CHOICES` empties the whole block and
#' names the reason in `vocab_error` (sanitised): fail closed, and never
#' TRUNCATE — a truncated list misaligns every index behind it.
#'
#' PURE: takes a block, returns a list, writes nothing.
ts_drive_project_vocabulary <- function(vocab, module) {
  if (is.null(vocab) || !is.list(vocab)) return(NULL)
  keys <- TS_DRIVE_VOCABULARY_KEYS[[module]]
  if (is.null(keys)) return(NULL)   # a module that declares no vocabulary publishes none
  keep <- c(keys, "vocab_rev", "vocab_error")
  out <- vocab[intersect(keep, names(vocab))]
  for (k in keys) {
    v <- out[[k]]
    if (is.null(v)) next
    v <- as.character(v)
    if (length(v) > TS_DRIVE_VOCAB_MAX_CHOICES) {
      out[[k]] <- character(0)
      # Chaque mot est choisi pour PASSER le sanitiseur (mesuré : "declared",
      # "vocabulary", "published" portent des runs de 8+ et partiraient en
      # <redacted>) — la raison doit rester lisible sur le wire.
      out$vocab_error <- ts_drive_badge_sanitize(sprintf(
        "%s: %d choices over the bound of %d — the list is not sent",
        k, length(v), TS_DRIVE_VOCAB_MAX_CHOICES), 200L)
      next
    }
    # Positions preserved: a guarded-out choice is NA, not removed.
    out[[k]] <- if (length(v)) ts_drive_verbatim_guard(v) else character(0)
  }
  if (!is.null(out$vocab_rev)) out$vocab_rev <- suppressWarnings(as.integer(out$vocab_rev))
  if (!is.null(out$vocab_error)) {
    out$vocab_error <- ts_drive_badge_sanitize(as.character(out$vocab_error), 200L)
  }
  out
}

#' Resolve SESSION-DERIVED inputs (Slice 3): index -> value, at apply time.
#'
#' The scenario carries `{index, vocab_rev}` per session-derived id (the server
#' validated the index against the PUBLISHED vocabulary before writing). The
#' app re-checks EVERYTHING here against its own probe — the app is the
#' authority, and the probe is the SAME closure that produced the published
#' block, so the rev and the choices cannot drift from each other:
#'   INPUT_NOT_READY     no probe, no vocabulary, or the key list is empty
#'                       (data not loaded) — fail closed with a reason;
#'   VOCAB_STALE         the payload's `vocab_rev` is not the CURRENT one —
#'                       the choices changed since the agent looked; re-snapshot;
#'   INDEX_OUT_OF_RANGE  an index outside 1..N of the choices as they stand at
#'                       the apply beat — the residual race after VOCAB_STALE;
#'   PAYLOAD_REFUSED     duplicates, more than `max_items`, an empty list where
#'                       none is allowed, or group_ref == group_target.
#' A value that is NOT the indexed shape (a plain string) passes through
#' UNCHANGED: it is the internal path (tests, internal scenarios) and the
#' existing behaviour is untouched — the INDEXED surface is the server's.
#'
#' The resolution happens BEFORE any injection or confirmation, so the adapters
#' and the confirm handshake keep working on real values — zero behaviour
#' change downstream.
#'
#' @param inputs The scenario's inputs (named list).
#' @param module The scenario module.
#' @param vocab_fn The module's vocabulary probe (a function), or NULL.
#' @return list(ok, values, errors). On failure `values` is untouched input.
ts_drive_resolve_session_inputs <- function(inputs, module, vocab_fn = NULL) {
  errs <- character(0)
  refuse <- function(...) list(ok = FALSE, values = inputs, errors = c(...))
  sess_ids <- intersect(names(inputs), names(TS_DRIVE_SESSION_INPUTS))
  if (!length(sess_ids)) return(list(ok = TRUE, values = inputs, errors = character(0)))

  vocab <- NULL
  if (!is.null(vocab_fn) && is.function(vocab_fn)) {
    vocab <- tryCatch(vocab_fn(), error = function(e) NULL)
  }
  if (is.null(vocab) || !is.list(vocab)) {
    return(refuse("INPUT_NOT_READY: the module published no vocabulary for this session."))
  }
  # The APPLIED vocabulary is the PROJECTED one — the exact block the wire
  # carries, so the positions the agent saw are the positions resolved here.
  vocab <- ts_drive_project_vocabulary(vocab, module)
  if (is.null(vocab)) {
    return(refuse("INPUT_NOT_READY: the module publishes no vocabulary."))
  }

  values <- inputs
  seen_groups <- list()
  for (id in sess_ids) {
    entry <- TS_DRIVE_SESSION_INPUTS[[id]]
    v <- inputs[[id]]
    if (!is.list(v) || is.null(v$index)) next  # plain-string value: internal path
    choices <- vocab[[entry$key]]
    if (is.null(choices) || !length(choices)) {
      errs <- c(errs, sprintf(
        "INPUT_NOT_READY: '%s' has an empty domain (its data is not loaded) — load it, then re-snapshot.",
        id))
      next
    }
    rev <- suppressWarnings(as.integer(v$vocab_rev %||% NA_integer_))
    if (length(rev) != 1L || is.na(rev) ||
        !identical(rev, suppressWarnings(as.integer(vocab$vocab_rev %||% NA_integer_)))) {
      errs <- c(errs, sprintf(
        "VOCAB_STALE: '%s' pins vocabulary revision %s, but the session is at %s — re-snapshot and retry.",
        id, if (length(rev) == 1L && !is.na(rev)) as.character(rev) else "(absent)",
        as.character(suppressWarnings(as.integer(vocab$vocab_rev %||% NA_integer_)))))
      next
    }
    # Un index JSON peut arriver en liste (tableau) : as.integer(list) LÈVE —
    # mesuré — donc la coercition est gardée, et l'échec est un refus, pas une
    # exception qui tuerait le battement du poller.
    idx <- tryCatch(suppressWarnings(as.integer(v$index)),
                    error = function(e) NA_integer_)
    if (!length(idx) || any(is.na(idx)) || any(idx < 1L)) {
      errs <- c(errs, sprintf("PAYLOAD_REFUSED: '%s' carries an index that is not a positive integer.", id))
      next
    }
    if (identical(entry$type, "index") && length(idx) != 1L) {
      errs <- c(errs, sprintf("PAYLOAD_REFUSED: '%s' takes exactly one index.", id))
      next
    }
    if (identical(entry$type, "index_list")) {
      if (length(idx) > (entry$max_items %||% 8L)) {
        errs <- c(errs, sprintf(
          "PAYLOAD_REFUSED: '%s' carries %d indices; the declared maximum is %d.",
          id, length(idx), entry$max_items %||% 8L))
        next
      }
      if (any(duplicated(idx))) {
        errs <- c(errs, sprintf("PAYLOAD_REFUSED: '%s' carries duplicate indices.", id))
        next
      }
      if (!length(idx) && !isTRUE(entry$allow_empty)) {
        errs <- c(errs, sprintf("PAYLOAD_REFUSED: '%s' cannot be empty.", id))
        next
      }
    }
    if (any(idx > length(choices))) {
      errs <- c(errs, sprintf(
        "INDEX_OUT_OF_RANGE: '%s' points past the %d choices the session holds at apply time — re-snapshot.",
        id, length(choices)))
      next
    }
    resolved <- choices[idx]
    if (identical(entry$key, "group_levels")) seen_groups[[id]] <- idx
    values[[id]] <- resolved
  }
  # The pair check is on the RESOLVED positions: ref == target is refused,
  # server-side when both travel together, and re-checked here (authority).
  if (length(seen_groups) >= 2L &&
      identical(seen_groups[["bulk-de-group_ref"]],
                seen_groups[["bulk-de-group_target"]])) {
    errs <- c(errs, "PAYLOAD_REFUSED: group_ref and group_target resolve to the same choice.")
  }
  if (length(errs)) return(list(ok = FALSE, values = inputs, errors = errs))
  list(ok = TRUE, values = values, errors = character(0))
}

#' The ONE generic `read_export` responder (R2). Zero per-module code: the
#' read is route-agnostic by construction — the module seams exist for the
#' EXPORT (each module knows what its artefact IS); the read only ever opens
#' what the export verdict already named.
#'
#' The target is derived STATELESSLY from result.json — the exact mirror of
#' the server's R1 derivation, re-checked here because two processes see the
#' same verdict at different instants (defence in depth, never a second rule:
#' the five deciding fields are the same five). The file basename comes from
#' the verdict's own descriptor; the stem check against
#' `TS_DRIVE_EXPORT_STEMS` is app-INTERNAL knowledge (it never produces a wire
#' code — a verdict whose file is not a declared artefact has no target).
#'
#' STREAMING, and this is the RAM contract (32 GB, VST matrices > 1 Gio): a
#' connection, ONE header line, then `read.csv(con, nrows = K)` — K records,
#' parsed record-wise (quoted multi-line fields stay correct), and one extra
#' record to know whether MORE rows exist. Nothing ever reads the file to its
#' end; the allocation is O(preview), never O(file).
#'
#' Bounds, applied in order: `EXPORT_GONE` (existence), `FILE_TOO_LARGE`
#' (`TS_DRIVE_READ_MAX_FILE_BYTES`), `READ_TOO_WIDE` (header width vs
#' `TS_DRIVE_READ_MAX_COLS`), `READ_IO_ERROR` (stream failure) — each as an
#' `errors[]` prefix on an `invalid` verdict, the app's honest refusal shape.
#' The 256 KiB ceiling is measured AFTER projection (guard + sanitiser
#' applied — the size of what the agent will receive), halving the preview
#' rows until it fits; `truncated_rows` reports every shrink honestly.
#'
#' @param seq The read scenario's own seq (echoed in the block).
#' @param max_rows The caller's preview height; clamped again here — the app
#'   is the authority.
#' @return An applier verdict: `done` with the PROJECTED read block as
#'   `descriptor`, or `invalid` with a declared `errors[]` prefix.
ts_drive_read_export_respond <- function(seq, max_rows = NULL) {
  refuse <- function(msg) {
    list(status = "invalid", errors = msg, warnings = character(0),
         active_module = NULL, nav = NULL)
  }
  # ── the stateless target derivation (mirror of R1, same five fields) ──────
  res <- ts_drive_read_result()
  if (is.null(res)) {
    return(refuse("NO_EXPORT_TARGET: result.json is absent — nothing has been exported in this session."))
  }
  if (!identical(as.character(res$protocol %||% ""), TS_DRIVE_PROTOCOL)) {
    return(refuse(sprintf(
      "NO_EXPORT_TARGET: result.json declares protocol '%s' (expected '%s').",
      as.character(res$protocol %||% "(absent)"), TS_DRIVE_PROTOCOL)))
  }
  if (!identical(as.character(res$status %||% ""), "done") ||
      is.null(res$descriptor) || !is.null(res$descriptor$preview)) {
    return(refuse(paste0(
      "NO_EXPORT_TARGET: the current verdict is not a completed export",
      if (!is.null(res$descriptor) && !is.null(res$descriptor$preview))
        " (already a read verdict — one read per export)" else "",
      "; export again to read again.")))
  }
  desc <- res$descriptor
  file_base <- as.character(desc$file %||% "")
  stem_ok <- nzchar(file_base) && !grepl("/|\\\\|~|[.]{2}", file_base) &&
    any(vapply(TS_DRIVE_EXPORT_STEMS,
               function(st) grepl(paste0("^", st, "_[0-9]+\\.csv$"), file_base),
               logical(1)))
  if (!stem_ok) {
    return(refuse("NO_EXPORT_TARGET: the verdict's file is not a declared export artefact."))
  }
  path <- file.path(ts_drive_export_dir(), file_base)
  if (!file.exists(path)) {
    return(refuse(paste0(
      "EXPORT_GONE: the exported file no longer exists in the app's bounded ",
      "export directory (pruned or temp cleaned); export again.")))
  }
  fsz <- suppressWarnings(file.size(path))
  if (is.na(fsz) || fsz > TS_DRIVE_READ_MAX_FILE_BYTES) {
    return(refuse(sprintf(
      "FILE_TOO_LARGE: the exported file is %s bytes; the declared read ceiling is %s.",
      format(as.numeric(fsz), scientific = FALSE, big.mark = ","),
      format(TS_DRIVE_READ_MAX_FILE_BYTES, scientific = FALSE, big.mark = ","))))
  }

  # ── streaming: header, width gate, then at most K records ─────────────────
  rows_want <- if (is.null(max_rows)) TS_DRIVE_READ_DEFAULT_ROWS else {
    r <- suppressWarnings(as.integer(max_rows))
    if (length(r) != 1L || is.na(r) || r < 1L) TS_DRIVE_READ_DEFAULT_ROWS else r
  }
  rows_want <- min(rows_want, TS_DRIVE_READ_MAX_ROWS)

  io_err <- NULL
  hdr_names <- character(0)
  hdr <- tryCatch({
    con <- file(path, "r")
    h <- readLines(con, n = 1L, warn = FALSE)
    close(con)
    if (length(h) == 0L) stop("the file has no header line")
    h
  }, error = function(e) {
    io_err <<- conditionMessage(e); NULL
  })
  if (is.null(hdr)) {
    return(refuse(sprintf("READ_IO_ERROR: the export could not be opened: %s.",
                          ts_drive_badge_sanitize(as.character(io_err), 200L))))
  }
  hdr_names <- tryCatch(
    names(utils::read.csv(text = hdr, header = TRUE, check.names = FALSE)),
    error = function(e) { io_err <<- conditionMessage(e); NULL })
  if (is.null(hdr_names)) {
    return(refuse(sprintf("READ_IO_ERROR: the export header could not be parsed: %s.",
                          ts_drive_badge_sanitize(as.character(io_err), 200L))))
  }
  if (length(hdr_names) > TS_DRIVE_READ_MAX_COLS) {
    return(refuse(sprintf(
      "READ_TOO_WIDE: the export has %s columns; the declared read bound is %s.",
      length(hdr_names), TS_DRIVE_READ_MAX_COLS)))
  }
  body <- tryCatch({
    con <- file(path, "r")
    h <- readLines(con, n = 1L, warn = FALSE)  # skip the header already parsed
    b <- utils::read.csv(con, header = FALSE, nrows = rows_want,
                         col.names = hdr_names, check.names = FALSE,
                         stringsAsFactors = FALSE, colClasses = "character",
                         na.strings = "")
    # ONE extra record: the honest truncated_rows signal — "more rows exist
    # than were returned" — without ever counting the file.
    more <- utils::read.csv(con, header = FALSE, nrows = 1L,
                            col.names = hdr_names, check.names = FALSE,
                            stringsAsFactors = FALSE, colClasses = "character",
                            na.strings = "")
    close(con)
    attr(b, "has_more") <- nrow(more) > 0L
    b
  }, error = function(e) {
    io_err <<- conditionMessage(e); NULL
  })
  if (is.null(body)) {
    return(refuse(sprintf("READ_IO_ERROR: the export could not be streamed: %s.",
                          ts_drive_badge_sanitize(as.character(io_err), 200L))))
  }

  # ── cells: verbatim guard, markers, honest counters ───────────────────────
  K <- nrow(body)
  # Classes INFERRED from the returned preview only (no full-file scan — the
  # separation is declared): integer/numeric/logical patterns on the preview's
  # own values, "character" otherwise, "other" is the projector's business.
  classify <- function(v) {
    x <- v[!is.na(v) & nzchar(v)]
    if (!length(x)) return("character")
    if (all(grepl("^[+-]?[0-9]+$", x))) return("integer")
    if (all(grepl("^[+-]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][+-]?[0-9]+)?$", x))) return("numeric")
    if (all(x %in% c("TRUE", "FALSE", "true", "false"))) return("logical")
    "character"
  }
  col_summary <- lapply(seq_along(body), function(j) list(
    name = names(body)[[j]],
    class = classify(body[[j]]),
    n_missing_in_preview = as.integer(sum(is.na(body[[j]])))))

  if (K == 0L) {
    cells <- matrix(character(0), nrow = 0L, ncol = length(hdr_names))
  } else {
    # cbind, NOT sapply: sapply simplifies single-value columns to an atomic
    # vector, and the ncol=1 fallback then reshaped a 1x2 table into 2x1 — a
    # silently TRANSPOSED preview (measured in the R2 smoke run). cbind keeps
    # the shape the file had.
    cells <- do.call(cbind, lapply(body, function(col) as.character(col)))
    dimnames(cells) <- NULL
  }
  in_na <- is.na(cells)
  n_chars <- suppressWarnings(nchar(cells, type = "chars", allowNA = TRUE))
  too_long <- !in_na & (is.na(n_chars) | n_chars > TS_DRIVE_READ_MAX_CELL_CHARS)
  # The guard is vectorised: one call over the flattened cells (column-major,
  # same order as every logical matrix subscript below).
  g_flat <- ts_drive_verbatim_guard(as.character(cells))
  guarded <- !in_na & !too_long & is.na(g_flat)
  cells_final <- cells
  cells_final[too_long] <- TS_DRIVE_READ_CELL_MARKERS[["truncated"]]
  cells_final[guarded] <- TS_DRIVE_READ_CELL_MARKERS[["guarded"]]
  cells_final[in_na] <- NA_character_

  # ── the block, and the byte ceiling measured AFTER projection ─────────────
  has_more <- isTRUE(attr(body, "has_more"))
  mk_block <- function(k) {
    list(
      route = as.character(res$active_module %||% ""),
      handle = file_base,
      seq = as.integer(seq),
      descriptor = desc,
      preview = list(
        rows_returned = as.integer(k),
        truncated_rows = isTRUE(has_more),
        columns = hdr_names,
        rows = if (k == 0L) list() else lapply(seq_len(k), function(i) {
          as.list(cells_final[i, ])
        }),
        cells_truncated = as.integer(if (k == 0L) 0L else sum(too_long[seq_len(k), , drop = TRUE])),
        cells_guarded = as.integer(if (k == 0L) 0L else sum(guarded[seq_len(k), , drop = TRUE]))),
      col_summary = col_summary)
  }
  K_cur <- K
  clamped <- FALSE
  proj <- NULL
  jsz <- NA_real_
  repeat {
    proj <- ts_drive_read_descriptor(mk_block(K_cur))
    jsz <- nchar(jsonlite::toJSON(proj, auto_unbox = TRUE, null = "null",
                                  pretty = TRUE), type = "bytes")
    if (jsz <= TS_DRIVE_READ_MAX_BYTES || K_cur <= 1L) break
    K_cur <- max(1L, floor(K_cur / 2L))
    clamped <- TRUE
  }
  # The block is rebuilt at the final height so the counters and rows_returned
  # describe EXACTLY what travels.
  proj <- ts_drive_read_descriptor(mk_block(K_cur))
  blk <- proj
  blk$preview$truncated_rows <- isTRUE(has_more) || isTRUE(clamped)

  list(status = "done", errors = character(0), warnings = character(0),
       active_module = NULL, nav = NULL, descriptor = blk)
}

#' The DECLARED vocabulary a state descriptor's `kind` and `convention` may take.
#'
#' Frozen, and returned VERBATIM by the projection: both are compile-time constants
#' in the modules' own source, so redacting them destroys meaning without removing
#' any risk. `columns` / `column` are deliberately absent - those can carry a
#' user-supplied annotation label, so they keep the sanitiser.
#' @noRd
TS_DRIVE_DESCRIPTOR_VERBATIM <- c(
  "table", "levels",
  "rows of the marker table (one row per gene x cluster)",
  "rows of the enrichment table (one row per term x database)",
  "unique levels of the annotation column named in `column`",
  "number of pipeline artefacts produced (not rows)")

#' The declared keys a MODULE STATE descriptor may carry.
#'
#' Separate from the export set on purpose: an export describes a FILE (so it has
#' `format` and `bytes`), while a state descriptor describes a RESULT (so it has
#' `kind` and `convention`). Sharing one list would let a state probe ship a
#' `file` field, which is exactly the widening the projection exists to stop.
#' @noRd
#' Bounds on a state descriptor's schema field. Named, so they are declared rather
#' than sprinkled as literals at each call site.
#' @noRd
TS_DRIVE_DESCRIPTOR_MAX_COLS <- 12L
TS_DRIVE_DESCRIPTOR_MAX_COL  <- 60L

#' @noRd
TS_DRIVE_STATE_DESCRIPTOR_KEYS <- c(
  "kind", "n_rows", "n_cols", "n_sig", "n_levels", "columns", "column",
  "columns_truncated", "convention")

#' A BOUNDED descriptor for a result a module produced.
#'
#' Schema and counts, never values. The gap it closes was MEASURED (2026-09-27): the
#' four SC probes published `n_results` with no referent, no schema and no
#' convention, so an agent could not act on the number.
#'
#' Two bounds, and both are the point:
#'   * at most `TS_DRIVE_DESCRIPTOR_MAX_COLS` column names, each truncated to
#'     `TS_DRIVE_DESCRIPTOR_MAX_COL`, so a 400-column table cannot write a kilobyte
#'     of schema into every snapshot;
#'   * no element is ever a data.frame, matrix or list — only scalars and short
#'     strings — so a cell can not reach the wire through here.
#'
#' @param x A data.frame/matrix to describe, or NULL.
#' @param kind `"table"` (default) or `"levels"` for a vector-shaped result.
#' @param convention A FIXED, declared string saying what the counts mean. It is
#'   never derived from the data, so two modules' numbers stay comparable.
#' @param n_levels For `kind = "levels"`: the number of levels.
#' @return The descriptor list, or NULL when there is nothing to describe.
#' @noRd
ts_drive_table_descriptor <- function(x, kind = "table", convention = NULL,
                                     n_levels = NULL, column = NULL) {
  conv <- if (is.null(convention)) NULL else
    substr(paste(as.character(convention), collapse = " "), 1L, 120L)
  col1 <- if (is.null(column)) NULL else
    substr(as.character(column)[1L], 1L, TS_DRIVE_DESCRIPTOR_MAX_COL)
  if (!is.null(n_levels)) {
    # A VECTOR-shaped result: there is no frame, so the referent is the COLUMN the
    # level count came from. Without it `n_levels = 12` is as anonymous as a bare
    # row count was.
    out <- list(kind = "levels", n_levels = as.numeric(n_levels))
    if (!is.null(col1)) out$column <- col1
    out$convention <- conv
    return(out)
  }
  if (is.null(x)) return(NULL)
  if (!is.data.frame(x) && !is.matrix(x)) return(NULL)
  if (nrow(x) == 0L && ncol(x) == 0L) return(NULL)
  nms <- colnames(x)
  if (is.null(nms)) nms <- character(0)
  cap <- TS_DRIVE_DESCRIPTOR_MAX_COLS
  keep <- utils::head(nms, cap)
  trunc <- length(nms) > cap ||
    any(nchar(keep) > TS_DRIVE_DESCRIPTOR_MAX_COL)
  out <- list(kind = kind,
              n_rows = as.numeric(nrow(x)),
              n_cols = as.numeric(ncol(x)),
              columns = substr(as.character(keep), 1L, TS_DRIVE_DESCRIPTOR_MAX_COL))
  if (trunc) out$columns_truncated <- TRUE
  out$convention <- conv
  out
}


#' @noRd
ts_drive_write_result <- function(seq, status, active_module, armed,
                                 preserve_data = TRUE, errors = character(0),
                                 warnings = character(0), snapshot = NULL,
                                 descriptor = NULL,
                                 descriptor_projector = ts_drive_export_descriptor) {
  payload <- list(
    protocol      = TS_DRIVE_PROTOCOL,
    ack_seq       = if (is.null(seq)) 0L else as.integer(seq),
    status        = status,
    applied_at    = ts_drive_now_iso(),
    active_module = active_module,
    armed         = isTRUE(armed),
    preserve_data = isTRUE(preserve_data),
    errors        = as.list(errors),
    warnings      = as.list(warnings),
    snapshot      = snapshot,
    job           = ts_drive_job_view(ts_drive_job_state()),
    # S2: the REDACTED export descriptor, or NULL. Projected by a PURE function so
    # the rule is testable without writing `result.json`, and so a `done` export is
    # never a verdict with nothing behind it. MEASURED on a live session: the export
    # wrote a real 204 485-byte file, reported `done`, and published no descriptor —
    # so the agent could not learn the filename, the row count or the columns.
    # R2: the projector is a PARAMETER, defaulting to the export projection —
    # every existing caller is unchanged byte for byte; the read verdict passes
    # ts_drive_read_descriptor so the read block is projected by its own rule.
    descriptor    = descriptor_projector(descriptor),
    # Sanitized diagnostics for the last FAILED wire write, or NULL.
    #
    # This is how a write failure becomes visible: `ready.json` is itself a
    # file that can fail to be written, so the error cannot be reported there
    # — it has to travel on a DIFFERENT channel. `result.json` is that channel
    # (and `ts_drive_last_write_error()` is the in-process accessor). Captured
    # BEFORE the write below, so the value survives even when this very write
    # is the one that fails.
    write_error   = ts_drive_last_write_error()
  )
  ok <- ts_drive_write_json(payload, ts_drive_path("result.json"))
  # Whether the payload VERIFIABLY landed, carried as an attribute rather than
  # as the return value: every existing caller reads the payload, and the job
  # lifecycle is the one caller that must not clear a pending terminal status
  # on a write that failed. `ts_drive_write_json()` already MEASURES this, so
  # the attribute forwards its verdict instead of re-deriving one. Same
  # convention as `ts_drive_write_ready()`.
  attr(payload, "written") <- isTRUE(ok)
  invisible(payload)
}

#' Read the previous result (used to resume `last_seq` after a reload).
ts_drive_read_result <- function() {
  ts_drive_read_json(ts_drive_path("result.json"))
}


# =============================================================================
# Object snapshot (spec §2.4: OBJECTS, not PNG)
# =============================================================================

#' Snapshot the bulk objects currently in memory.
#'
#' A `renderPlot` hands back base64 PNG data, which is useless to an agent;
#' the state below is what the UI itself reads. Field names are frozen in the
#' spec: has_data / object_class / n_genes / n_samples / error_state.
#'
#' @param global_data The app-wide `reactiveValues`.
ts_drive_snapshot <- function(global_data) {
  out <- list(has_data = FALSE, object_class = NULL, n_genes = NULL,
              n_samples = NULL, error_state = NULL, modules = list())
  if (is.null(global_data)) return(out)

  # What the MODULES can see, published by them (see `state` in
  # `ts_drive_publish_token()`). Collected BEFORE the `bulk_obj` early return,
  # so a session that has module state but no active object still reports it.
  #
  # WHY THIS EXISTS — measured, not speculative. `global_data$bulk_obj` and
  # `shared_rv$filtered_counts` are DIFFERENT slots with different lifetimes:
  # the first is the imported dataset, the second is the output of Step 1.
  # Reading only the first made the snapshot answer `has_data=TRUE
  # genes=17925 samples=18` in the same instant the DE guard refused with
  # "no bulk object loaded" — the agent could not tell "Step 1 ran" from
  # "Step 1 never ran", because both report the IMPORTED dimensions.
  out$modules <- ts_drive_module_states(global_data)

  obj <- state_get(global_data, "bulk_obj")
  if (is.null(obj)) return(out)

  counts <- tryCatch(obj$counts, error = function(e) { ts_log_swallow("drive_watcher.bulk_summary", e); NULL })
  out$has_data     <- TRUE
  out$object_class <- class(obj)[1]
  out$n_genes      <- tryCatch(nrow(counts), error = function(e) { ts_log_swallow("drive_watcher.bulk_summary", e); NULL })
  out$n_samples    <- tryCatch(ncol(counts), error = function(e) NULL)
  out$error_state  <- tryCatch(
    ts_error_state(NULL, class = NULL), error = function(e) { ts_log_swallow("drive_watcher.bulk_summary", e); NULL })
  out
}

#' Collect the per-module state published by the modules themselves.
#'
#' One entry per MODULE that published a `state` probe, keyed by module name
#' (`bulk_filter`, `bulk_de`, ...). A module publishes ONE state — if two of its
#' buttons announce one, the last writer wins, which is why the convention is
#' "the module's primary token carries the state".
#'
#' The probe is a module-side closure and MUST wrap its own reactive reads in
#' `shiny::isolate()`: it is called from inside the poller's reactive beat, and
#' an un-isolated read would silently enrol the module's data in the poller's
#' dependency set (same rule as the readiness guard, spec §6).
#'
#' A probe that throws is reported as `probe_error` rather than omitted:
#' "this module has no state" and "this module's probe crashed" must never be
#' the same answer, or a broken probe becomes invisible.
#'
#' @param global_data The app-wide `reactiveValues`.
#' @return Named list, possibly empty. Never NULL.
ts_drive_module_states <- function(global_data) {
  reg <- ts_drive_registry(global_data)
  if (is.null(reg)) return(list())
  out <- list()
  for (id in ls(reg)) {
    entry <- reg[[id]]
    probe <- if (is.list(entry)) entry$state else NULL
    if (!is.function(probe)) next
    # An IMPORTER entry is named `tsdrive-importer-<module>`, which is not an
    # allowlisted input id; the lexical fallback would answer
    # `tsdrive-importer-import`. The mapping is DATA (drive_allowlist.R).
    mod <- ts_drive_module_of_registry(id)
    if (is.na(mod)) next
    ans <- tryCatch(probe(), error = function(e) e)
    out[[mod]] <- if (inherits(ans, "condition")) {
      list(probe_error = ts_drive_badge_sanitize(
        sprintf("the state probe raised: %s", conditionMessage(ans)), 200L
      ))
    } else if (is.list(ans)) {
      # 🔴 THE PROJECTION, and it is not optional. MEASURED: this collector returned
      # a probe's list WHOLE, with no whitelist - unlike `ts_drive_export_descriptor()`.
      # A descriptor reaching the wire through here would therefore skip BOTH rules
      # (declared keys only, every string sanitised) and could ship whatever the
      # probe returned. The descriptor is projected here, where it enters the wire,
      # so a probe cannot widen it by returning one extra key.
      if (!is.null(ans$descriptor)) {
        ans$descriptor <- ts_drive_project_descriptor(
          ans$descriptor, TS_DRIVE_STATE_DESCRIPTOR_KEYS,
          verbatim = TS_DRIVE_DESCRIPTOR_VERBATIM)
        # 🔴 `columns` MUST serialise as an ARRAY at every width. MEASURED live: a
        # one-column descriptor published `"columns": "step"` while a four-column
        # one published a JSON array, because the writer auto-unboxes a length-1
        # vector. An agent then has to handle two shapes for one field, and the
        # difference is invisible until it parses the wrong one. Wrapping in
        # `as.list()` defeats the unboxing; the field is a list of names, not a
        # name.
        if (!is.null(ans$descriptor$columns)) {
          ans$descriptor$columns <- as.list(ans$descriptor$columns)
        }
      }
      # Slice 3: the published VOCABULARY is projected by ITS rule (closed
      # keep-set, D1-verbatim choices through the shared guard, positions
      # preserved). Same shape as the descriptor branch above — a probe cannot
      # widen the wire by returning one extra key.
      if (!is.null(ans$vocabulary)) {
        ans$vocabulary <- ts_drive_project_vocabulary(ans$vocabulary, mod)
      }
      ans
    } else {
      list(probe_error = "the state probe returned neither a list nor an error")
    }
  }
  out
}


# =============================================================================
# Button token binding (spec S5 / G2)
# =============================================================================

#' Bind a click site to a drive token.
#'
#' `updateActionButton()` does NOT click, and `session$setInputs()` does not
#' exist on a live session. The portable way to fire an existing
#' `observeEvent(input$<btn>, ...)` is to give it a SECOND trigger: a counter
#' read by the observer and incremented by the poller. The observer body becomes
#'
#'     observeEvent(c(input$run_de, ts_drive_read_token(ts_drive_bind_button("bulk-de-run_de"))), ...)
#'
#' which is a minimal, additive touch — the module is not refactored (spec G2).
#'
#' @param input_id Namespaced button id (must be in TS_DRIVE_BUTTONS).
#' @return An identifier the module can use as the `observeEvent()` trigger.
#'   The VALUE is a plain string on purpose: `R/` may not create a
#'   `reactiveVal` (C2 — reactivity belongs to `modules/`, and
#'   `R/core/state.R` is the only blessed factory). The module wraps this id in
#'   its own counter; see `ts_drive_read_token()` for the contract.
ts_drive_bind_button <- function(input_id) {
  if (!input_id %in% TS_DRIVE_BUTTONS) {
    warning(sprintf("ts_drive_bind_button(): '%s' is not in TS_DRIVE_BUTTONS — binding refused.", input_id))
    return(NULL)
  }
  TS_DRIVE_TOKEN_PREFIX <- "tsdrive-token-"
  paste0(TS_DRIVE_TOKEN_PREFIX, input_id)
}

#' Readiness vocabulary for a bound button (frozen).
#'
#' A binding says "this button CAN be fired". It says nothing about whether
#' firing it would DO anything — and that gap produced a real lie. A bound
#' button whose module had no object loaded was fired, the module's own `req()`
#' aborted in silence, and `result.json` reported `done` for a pipeline that
#' never ran. So `run_pipeline` asks the owning module FIRST.
#'
#'   ready        the module states it can run  -> fire
#'   not-ready    the module states it cannot   -> `invalid`, and NOTHING fires
#'   probe-failed the guard could not answer    -> `invalid`. FAIL CLOSED: an
#'                unanswerable probe is not evidence of readiness, and reading
#'                it as permission is exactly how the `done` lie comes back.
#'   unknown      no guard was published        -> fire. This is the G0/G1
#'                shape, kept so a module that has not adopted a guard still
#'                runs; it is the ONLY permissive case.
TS_DRIVE_READY <- c("ready", "not-ready", "probe-failed", "unknown")

#' Unwrap the counter a registry entry carries.
#'
#' Two shapes are accepted so the G1 call sites keep working unchanged:
#'   * a bare `reactiveVal` — which IS a function, and was the original shape;
#'   * a record `list(counter =, ready =)` — what `ts_drive_publish_token()`
#'     stores now that a button can also announce a readiness guard.
#' @noRd
ts_drive_entry_counter <- function(entry) {
  if (is.null(entry)) return(NULL)
  if (is.function(entry)) return(entry)
  if (is.list(entry)) return(entry$counter)
  NULL
}

#' Does one registry entry DECLARE a long job? (see `long` in
#' `ts_drive_publish_token()`.)
#'
#' Fails CLOSED on anything unexpected: a bare counter, a legacy record written
#' before `long` existed, `NULL`, or a non-logical all answer `FALSE`, so an
#' undeclared button keeps the documented `done` semantics rather than being
#' silently promoted into a job that must be closed.
ts_drive_entry_long <- function(entry) {
  if (!is.list(entry)) return(FALSE)
  isTRUE(entry$long)
}

ts_drive_entry_timeout <- function(entry) {
  if (!is.list(entry) || is.null(entry$timeout_s)) return(NULL)
  value <- suppressWarnings(as.numeric(entry$timeout_s))
  if (length(value) != 1L || is.na(value) || !is.finite(value) || value <= 0) {
    return(NULL)
  }
  value
}

#' Ask one registry entry's readiness guard, and classify the answer.
#'
#' A guard may return:
#'   * `TRUE`                -> `"ready"`
#'   * `FALSE`               -> `"not-ready"`, generic
#'   * a non-empty character -> `"not-ready"`, carrying the REASON, so the
#'     refusal can name the missing object instead of a bare "not ready" that
#'     an agent cannot act on.
#' Anything else — an error, `NA`, a non-logical — is `"probe-failed"`.
#'
#' The guard is called from inside the poller's reactive beat, so a module
#' should wrap its own reactive read in `shiny::isolate()`: reactivity is the
#' module's business (C2), and an un-isolated read would silently enrol the
#' module's object in the poller's dependency set.
#'
#' Returns the verdict AND the reason from ONE evaluation, so a guard with a
#' side effect (a log line) is not run twice by the caller that needs both.
#'
#' @param entry One registry entry, a bare counter, or `NULL`.
#' @return list(verdict = one of `TS_DRIVE_READY`, reason = character or NULL).
ts_drive_ready_probe <- function(entry) {
  if (is.null(entry) || is.function(entry) ||
      !is.list(entry) || is.null(entry$ready)) {
    return(list(verdict = "unknown", reason = NULL))
  }
  guard <- entry$ready
  if (!is.function(guard)) {
    return(list(verdict = "probe-failed",
                reason = "the readiness guard is not a function"))
  }
  ans <- tryCatch(guard(), error = function(e) e)
  if (inherits(ans, "condition")) {
    return(list(verdict = "probe-failed",
                reason = sprintf("the readiness guard raised: %s",
                                 conditionMessage(ans))))
  }
  if (isTRUE(ans)) return(list(verdict = "ready", reason = NULL))
  if (identical(ans, FALSE)) return(list(verdict = "not-ready", reason = NULL))
  if (is.character(ans) && length(ans) == 1L && !is.na(ans) && nzchar(ans)) {
    return(list(verdict = "not-ready", reason = ans))
  }
  list(verdict = "probe-failed",
       reason = "the readiness guard returned neither TRUE/FALSE nor a reason")
}

#' The verdict alone, for callers that only switch on it.
#' @noRd
ts_drive_entry_ready <- function(entry) ts_drive_ready_probe(entry)$verdict

#' Build the refusal message for a non-ready button.
#'
#' `not ready` and `probe failed` are kept as two distinct sentences on
#' purpose: the first is a normal refusal the agent can act on (load an
#' object), the second is a bug in the module's guard and must not be
#' mistaken for "the app is simply not ready yet".
#' @noRd
ts_drive_ready_refusal <- function(button_id, probe) {
  if (identical(probe$verdict, "probe-failed")) {
    return(sprintf(
      "button '%s' is bound but its readiness probe failed: %s",
      button_id, probe$reason %||% "no reason reported"))
  }
  if (is.null(probe$reason) || !nzchar(probe$reason)) {
    sprintf("button '%s' is bound but not ready (the module reports no object loaded)",
            button_id)
  } else {
    sprintf("button '%s' is bound but not ready: %s", button_id, probe$reason)
  }
}

#' How a module turns the trigger id into a reactive counter read.
#'
#' Contract, written once here so the four call sites stay identical:
#'
#'   drive_counter <- shiny::reactiveVal(0L)                    # created in modules/ (C2)
#'   ts_drive_publish_token(global_data, "bulk-de-run_de", drive_counter)
#'   drive_trigger <- shiny::reactive(list(drive_counter(), input$run_de))
#'   observeEvent(drive_trigger(), { ... })                     # the existing body
#'
#' Reading `drive_counter()` inside a `reactive()` makes it a reactive
#' dependency of the trigger, so incrementing it re-fires the observer exactly
#' as a click would. The body itself is untouched; only the trigger changed.
#' Everything is a no-op while `arm.json` is absent, because the counter then
#' never moves.
#'
#' @param counter The module's `reactiveVal`, or NULL.
#' @return The integer read, for direct use in a trigger vector.
ts_drive_read_token <- function(counter) {
  if (is.null(counter)) return(0L)
  as.integer(shiny::isolate(counter())) + 1L
}

#' Read the session-scoped drive registry — legally, from ANY context.
#'
#' `global_data` is a `reactiveValues`, and Shiny ABORTS a read of one of its
#' fields outside a reactive consumer:
#'
#'     Can't access reactive value 'drive_registry' outside of reactive consumer.
#'
#' Module INIT is such a place. A `moduleServer()` body runs while `server()`
#' is still executing, before any flush, so there is no active reactive
#' context. The first version of this file read the registry through
#' `tryCatch(..., error = function(e) NULL)`, which turned that abort into a
#' SILENT `NULL`; `ts_drive_publish_token()` then returned FALSE without a
#' word, the registry stayed empty, and on a REAL session every bound button
#' reported "not bound" — the exact symptom the G0/G1 live run recorded, with
#' source-level wiring that looked perfect. A seam that fails silently needs a
#' test, and this one now has it (`test-drive-watcher.R`, "a token can be
#' published from MODULE INIT").
#'
#' `shiny::isolate()` is the sanctioned way to read outside a consumer, and it
#' ALSO suppresses the dependency a tick-time read would otherwise register on
#' the poller — which is what we want in both directions.
#'
#' @param global_data The app-wide `reactiveValues`.
#' @return The registry environment, or `NULL` when there is none (a unit test
#'   that sources a module alone). Never throws.
#' @noRd
ts_drive_registry <- function(global_data) {
  reg <- state_get(global_data, "drive_registry")
  if (is.environment(reg)) reg else NULL
}

#' Publish one button's counter into the session-scoped registry.
#'
#' Called from a module (which owns the `reactiveVal`) so the poller can
#' increment it later. The registry lives on `global_data`, i.e. it is
#' per-SESSION — never a global environment, which would let two tabs share a
#' counter (spec §2.1: a scenario may pin one session).
#'
#' The stored value is a RECORD, `list(counter =, ready =)`, not the bare
#' counter: `run_pipeline` must be able to ask whether the button can actually
#' run before it fires (see `TS_DRIVE_READY`). `ts_drive_entry_counter()` keeps
#' the bare-`reactiveVal` shape readable, so nothing else had to change.
#'
#' The registry is reached through `ts_drive_registry()` — NOT with a bare
#' `$` read, which would abort here and be swallowed. See that function.
#'
#' Silently does nothing when no registry is present (e.g. a unit test that
#' sources a module alone), so the module keeps working outside the app.
#'
#' @param global_data The app-wide `reactiveValues`.
#' @param input_id Namespaced button id (must be in TS_DRIVE_BUTTONS).
#' @param counter The module's `reactiveVal`.
#' @param ready Optional zero-argument guard, owned by the module, returning
#'   `TRUE`, `FALSE`, or a character REASON. It answers "could this button run
#'   right now?" — e.g. `function() !is.null(shiny::isolate(shared_rv$filtered_counts))`.
#'   Omitted (`NULL`) means "no guard": the button stays fireable and the poller
#'   reports what it always did. A guard is deliberately NOT validated here —
#'   `ts_drive_ready_probe()` classifies whatever it is, and a non-function
#'   fails closed there rather than silently disappearing at publish time.
#' @param state Optional zero-argument probe, owned by the module, returning a
#'   LIST describing what the module can see (`shared_rv$filtered_counts`
#'   dimensions, sample names, …). It is surfaced under
#'   `snapshot$modules[[<module>]]` (see `ts_drive_snapshot()`), which is how an
#'   agent tells a stage that has RUN from one that has not. Like `ready`, it is
#'   called from the poller's beat, so the module MUST wrap its reactive reads
#'   in `shiny::isolate()`. Omitted means "this module publishes no state", and
#'   that absence stays distinguishable from a probe that crashes.
#' @param long Does firing this button start a job that OUTLIVES the reactive
#'   flush? Default `FALSE`, which keeps `run_pipeline`'s documented meaning
#'   ("the token moved", spec §2.3). A module that declares `TRUE` takes on the
#'   matching duty: `run_pipeline` answers `running` at dispatch, and the module
#'   must close the loop with `ts_drive_job_finish()` or the agent polls a job
#'   that never reports. Declared by the MODULE because only the module knows
#'   its own cost — the DE module cites its 852.7 s measurement.
ts_drive_publish_token <- function(global_data, input_id, counter, ready = NULL,
                                    state = NULL, long = FALSE, timeout_s = NULL,
                                    confirm_inputs = NULL, vocab = NULL) {
  if (!input_id %in% TS_DRIVE_BUTTONS) {
    warning(sprintf("ts_drive_publish_token(): '%s' is not in TS_DRIVE_BUTTONS — ignored.", input_id))
    return(invisible(FALSE))
  }
  reg <- ts_drive_registry(global_data)
  if (is.null(reg)) return(invisible(FALSE))
  reg[[input_id]] <- list(counter = counter, ready = ready, state = state,
                          long = isTRUE(long),
                          timeout_s = ts_drive_entry_timeout(list(timeout_s = timeout_s)),
                          # S6: the module's own report of what it OBSERVES, used by
                          # the confirmation handshake. `NULL` means the module
                          # cannot confirm, and a run injecting non-button inputs
                          # into it is refused rather than fired blind.
                          confirm_inputs = if (is.function(confirm_inputs)) confirm_inputs else NULL,
                          # Slice 3: the module's own VOCABULARY probe — the SAME
                          # closure whose output the state publishes — so the
                          # applier resolves an index against the choices that
                          # produced the published block, never against a copy.
                          # `NULL` means the module publishes no vocabulary and
                          # its session-derived inputs stay `INPUT_NOT_READY`.
                          vocab = if (is.function(vocab)) vocab else NULL)
  invisible(TRUE)
}

#' Read one published counter (used by the poller through the effects callback).
#'
#' Unwraps the registry record, so the returned value is the `reactiveVal`
#' itself — which is what the name promises and what every caller wants.
ts_drive_token_of <- function(global_data, input_id) {
  reg <- ts_drive_registry(global_data)
  if (is.null(reg)) return(NULL)
  ts_drive_entry_counter(reg[[input_id]])
}

#' Increment one bound button token (used by `run_pipeline`).
#'
#' The counter only ever increases: an `actionButton` fires on a CHANGE, and
#' a monotonically growing counter is what makes two consecutive
#' `run_pipeline` scenarios both fire (G2 acceptance 9). A latch — a counter
#' that stops moving after the first fire — would show up as the second
#' scenario never advancing it, which is why the test asserts the VALUE, not
#' just that a call happened.
#'
#' @param rv The registry entry for one button — either the record
#'   `list(counter =, ready =)` that `ts_drive_publish_token()` stores, or a
#'   bare `reactiveVal` (the G1 shape). It is passed explicitly (never looked
#'   up) because a `reactiveVal` is only readable inside an active reactive
#'   context: the value is both read and written by the caller in `modules/`,
#'   where reactivity belongs.
#' @return TRUE when a binding existed and was incremented.
ts_drive_bump_token <- function(rv) {
  rv <- ts_drive_entry_counter(rv)
  if (is.null(rv) || !is.function(rv)) return(FALSE)
  rv(shiny::isolate(rv()) + 1L)
  TRUE
}


# =============================================================================
# Injection — set_inputs / nav_select (spec S5)
# =============================================================================

#' Adapters, one per allowlisted widget kind.
#'
#' `input` is a LIVE `reactivevalues` snapshot (the poller reads with
#' `shiny::isolate`); `value` is the scenario's scalar. Returns TRUE when the
#' value was sent, FALSE when it was refused (a warning is appended either way)
#' so the caller can report per-key outcomes rather than a single boolean.
#' @noRd
.ts_drive_adapters <- list(
  select   = function(session, id, value) tryCatch({ shiny::updateSelectInput(session, id, selected = value); TRUE }, error = function(e) FALSE),
  radio    = function(session, id, value) tryCatch({ shiny::updateRadioButtons(session, id, selected = value); TRUE }, error = function(e) FALSE),
  checkbox = function(session, id, value) tryCatch({ shiny::updateCheckboxInput(session, id, value = isTRUE(value)); TRUE }, error = function(e) FALSE),
  numeric  = function(session, id, value) tryCatch({ shiny::updateNumericInput(session, id, value = as.numeric(value)); TRUE }, error = function(e) FALSE),
  text     = function(session, id, value) tryCatch({ shiny::updateTextInput(session, id, value = as.character(value)); TRUE }, error = function(e) FALSE)
)

#' Apply the allowlisted `inputs` block of a scenario.
#'
#' @param tokens Named list id -> registry entry (`list(counter =, ready =)`)
#'   announced by the module for THIS scenario's module (read through
#'   `ts_drive_tokens_for()`).
#' @return list(applied = character(), refused = character(), warnings = character())
ts_drive_apply_inputs <- function(session, inputs, module, tokens = list()) {
  applied <- character(0); refused <- character(0); warns <- character(0)

  for (id in names(inputs)) {
    entry <- ts_drive_allowlist_get(id)
    if (is.null(entry)) {
      refused <- c(refused, id)
      warns <- c(warns, sprintf("%s: not on the allowlist — skipped", id))
      next
    }
    if (!identical(ts_drive_module_of(id), module)) {
      refused <- c(refused, id)
      warns <- c(warns, sprintf("%s: belongs to module '%s', scenario targets '%s' — skipped",
                                id, ts_drive_module_of(id), module))
      next
    }
    kind <- entry$kind
    if (identical(kind, "button")) {
      # Buttons are fired, never set: an actionButton is a counter. And they
      # are fired only when the owning module says it can run — firing a
      # doomed click and reporting success is the `done` lie the readiness
      # gate exists to remove (see TS_DRIVE_READY).
      probe <- ts_drive_ready_probe(tokens[[id]])
      if (probe$verdict %in% c("not-ready", "probe-failed")) {
        refused <- c(refused, id)
        warns <- c(warns, sprintf("%s: %s — skipped", id,
                                  ts_drive_ready_refusal(id, probe)))
        next
      }
      ok <- ts_drive_bump_token(tokens[[id]])
      if (isTRUE(ok)) applied <- c(applied, id)
      else {
        refused <- c(refused, id)
        warns <- c(warns, sprintf("%s: button not bound (module did not announce a token) — skipped", id))
      }
      next
    }
    adapter <- if (identical(kind, "nav") || identical(kind, "nav_top")) {
      function(session, id, value) {
        tryCatch({ bslib::nav_select(id = TS_DRIVE_TOP_NAV_ID, selected = "tab_bulk", session = session); TRUE },
                 error = function(e) FALSE)
      }
    } else {
      .ts_drive_adapters[[kind]]
    }
    if (is.null(adapter)) {
      refused <- c(refused, id)
      warns <- c(warns, sprintf("%s: unsafe widget kind '%s' for v1 — skipped", id, kind))
      next
    }
    if (isTRUE(adapter(session, id, inputs[[id]]))) applied <- c(applied, id)
    else {
      refused <- c(refused, id)
      warns <- c(warns, sprintf("%s: adapter '%s' refused the value — skipped", id, kind))
    }
  }
  list(applied = applied, refused = refused, warnings = warns)
}

#' Put the human on the bulk tab (and optionally a bulk result tab / panel).
#'
#' `bslib::nav_select()` is used because the human must SEE the target tab in
#' the Viewer or the Chrome tab — a state change nobody watches is not a demo.
#'
#' @param target_tab Optional value of `bulk-main_tabs`.
#' @return TRUE on success.
ts_drive_nav_select_bulk <- function(session, target_tab = NULL) {
  ok <- tryCatch({
    bslib::nav_select(id = TS_DRIVE_TOP_NAV_ID, selected = "tab_bulk", session = session)
    TRUE
  }, error = function(e) FALSE)

  if (!is.null(target_tab) && target_tab %in% TS_DRIVE_BULK_TABS) {
    ok <- tryCatch({
      bslib::nav_select(id = TS_DRIVE_BULK_TABS_ID, selected = target_tab, session = session)
      TRUE
    }, error = function(e) ok)
  }
  ok
}

#' Open one bulk sidebar accordion panel (e.g. "panel_de", "panel_pathways").
ts_drive_open_panel <- function(session, panel) {
  if (!panel %in% TS_DRIVE_BULK_PANELS) return(FALSE)
  tryCatch({
    bslib::accordion_panel_open(TS_DRIVE_BULK_ACCORDION_ID, values = panel, session = session)
    TRUE
  }, error = function(e) FALSE)
}

#' Canonical form of an injected input block, for the confirmation handshake.
#'
#' @param values Named list of injected `id -> value`.
#' @return A single string, or `""` for an empty block.
#'
#' WHY A STRING AND NOT A HASH. The watcher and the module run in the SAME R
#' process, so the two sides can build the same canonical text and compare it
#' exactly. A digest would add a dependency and a second way to be wrong (encoding,
#' truncation) to solve a problem that string equality already solves. Order is
#' normalised by key so JSON key order cannot change the verdict, and values are
#' compared by their printed form so `TRUE` and `"TRUE"` are distinguishable from
#' `1` and `"1"`.
ts_drive_confirm_key <- function(values) {
  if (length(values) == 0L) return("")
  keys <- names(values)
  if (is.null(keys)) keys <- as.character(seq_along(values))
  ord <- order(keys)
  paste0(keys[ord], "=",
         vapply(values[ord], function(v) paste(as.character(v), collapse = ","), character(1)),
         collapse = "\n")
}

#' The non-button ids of an `inputs` block: the ones a run must be confirmed on.
#'
#' A `button` entry is FIRED rather than set, so it needs no round trip and must be
#' excluded: including it would make every ordinary button-only run wait for a
#' confirmation that can never arrive, stranding it at `running`.
ts_drive_nonbutton_inputs <- function(inputs) {
  if (length(inputs) == 0L) return(character(0))
  keep <- vapply(names(inputs), function(id) {
    e <- ts_drive_allowlist_get(id)
    !is.null(e) && !identical(as.character(e$kind), "button")
  }, logical(1))
  names(inputs)[keep]
}

#' Beats a pending confirmation may wait before it is refused.
#'
#' A bound, not a hope. At the 800 ms default poll this is ~4 s, which is several
#' client round trips; the point is that a run either becomes confirmed or is
#' REFUSED with a reason, and never sits at `running` indefinitely.
TS_DRIVE_CONFIRM_MAX_BEATS <- 5L

#' Service one pending confirmation.
#'
#' @param pending The record built by `ts_drive_apply()`.
#' @param token The LIVE session token, for the identity comparison.
#' @param effects The effect seam, used to fire the button once confirmed.
#' @param out The tick's result list.
#' @param inject Function(inputs, module) used to send a later injection stage.
#' @return The tick's result list, with `out$pending` set to the next record or to
#'   `NULL` once a verdict is reached.
#'
#' ⚠️ IT RETURNS `out` RATHER THAN MUTATING IT. A first version took `out` as a
#' parameter and assigned `out$error <- msg` inside a `refuse()` closure, which
#' modifies a COPY: R copies lists on assignment, so every verdict was silently
#' lost and the tests saw `error = NULL`. This is the "environment recorder vs
#' list" trap recorded for the S2 provenance work, reached again from the other
#' direction — there the copy lost a WRITE, here it lost a VERDICT.
#'
#' Every terminal branch publishes a verdict and creates NO job. A human edit is
#' REFUSED, never re-injected: the agent's earlier write does not get to win twice
#' over a deliberate human change, and a new scenario is required.
ts_drive_service_pending <- function(pending, token, effects, out, inject = NULL) {
  refuse <- function(msg) {
    out$consumed  <- TRUE
    out$status    <- "invalid"
    out$module    <- pending$module
    out$error     <- msg
    out$pending   <- NULL
    wrote <- ts_drive_write_result(pending$seq, "invalid", pending$module,
                                   out$armed, errors = msg)
    if (!isTRUE(attr(wrote, "written"))) {
      # Keep it pending so the refusal is RETRIED, exactly as a failed terminal
      # job write is. Returning the record, not a verdict, is what makes the
      # refusal durable across a transient IPC failure.
      out$consumed <- FALSE
      out$status   <- NULL
      out$error    <- NULL
      p <- pending
      p$beats <- pending$beats + 1L
      out$pending <- p
      return(out)
    }
    out$published <- TRUE
    out$last_seq  <- pending$seq
    out
  }
  beats <- pending$beats + 1L

  # ── STAGE THE REMAINING INJECTIONS, one per beat ──
  #
  # A `selectInput` cannot hold a value that is not among its current options, so
  # a dependent value must not be sent before the options exist. The order is
  # declared in `TS_DRIVE_INPUT_STAGES`; this walks it.
  #
  # It runs BEFORE the confirmation probe, and deliberately so: probing a stage
  # that has not been injected yet would report the pre-injection state as a
  # mismatch, which is the false accusation the `prior` comparison exists to
  # prevent. Nothing is ever sent twice — a control appears in exactly one stage.
  stages <- pending$stages
  if (is.list(stages) && length(stages) &&
      (is.null(pending$stage) || pending$stage < length(stages))) {
    nxt <- pending$stage %||% 0L
    if (nxt < length(stages)) {
      if (!is.function(inject)) {
        return(refuse(sprintf(
          "module '%s' needs its inputs injected in %d stages, but no injector is available; the run for seq %s is refused. No job was created.",
          pending$module, length(stages), pending$seq)))
      }
      got <- tryCatch(inject(stages[[nxt + 1L]], pending$module), error = function(e) e)
      if (inherits(got, "condition")) {
        return(refuse(sprintf(
          "injecting stage %d of %d for seq %s failed; the run is refused. No job was created.",
          nxt + 1L, length(stages), pending$seq)))
      }
      p <- pending
      p$stage  <- nxt + 1L
      p$beats  <- beats
      out$pending <- p
      return(out)
    }
  }

  # A replaced session must never be satisfied by the old one's confirmation.
  #
  # 🔑 ON SESSION IDENTITY: the comparison is on `session_token` ALONE, and
  # `started_at` is deliberately NOT part of it. `token <- ts_drive_new_token()`
  # mints 8 fresh random characters per session (see ts_drive_attach), so the
  # token IS the session identity — and it is already the identity notion the rest
  # of the file uses: ts_drive_write_ready() refuses to carry a heartbeat across a
  # token change (l. 462/474) and the pinned-action validator refuses a scenario
  # whose declared token is not this session's (l. 755). `started_at` is a
  # companion FIELD for humans reading ready.json, not a stronger key: threading
  # it into ts_drive_tick() would add a second identity to keep in sync for no
  # gain in discrimination. Stated here so it reads as a decision, not an omission.
  if (!identical(as.character(pending$session_token), as.character(token))) {
    return(refuse(sprintf(
      "the session was replaced while inputs for seq %s were pending confirmation; no run started",
      pending$seq)))
  }
  entry <- ts_drive_tokens_for(pending$module, effects)[[pending$button]]
  conf <- entry$confirm_inputs
  if (!is.function(conf)) {
    return(refuse(sprintf(
      "module '%s' no longer publishes confirm_inputs(); the injected run for seq %s is refused. No job was created.",
      pending$module, pending$seq)))
  }
  # The probe is called as `confirm_inputs(values, session_token, prior)`. A probe
  # that does not accept `prior` cannot tell a round trip that has not landed from
  # a human edit, so it is refused BY NAME rather than allowed to fail as an
  # "unused argument" error. MEASURED: the first version simply called it with
  # three arguments, and every two-argument probe surfaced as the opaque
  # "the confirmation probe of module 'x' failed" — true, and useless.
  cf <- names(formals(conf))
  if (!("prior" %in% cf) && !("..." %in% cf)) {
    return(refuse(sprintf(
      "module '%s' publishes a confirm_inputs() that does not accept `prior`; the drive needs the pre-injection values to tell a round trip that has not landed from a human edit, so the run for seq %s is refused. No job was created.",
      pending$module, pending$seq)))
  }
  r <- tryCatch(conf(pending$values, token, pending$prior), error = function(e) e)
  if (inherits(r, "condition")) {
    return(refuse(sprintf(
      "the confirmation probe of module '%s' failed for seq %s; no run started",
      pending$module, pending$seq)))
  }
  # A module that REPORTS a sequence must report THIS one. ⚠️ This check is
  # OPTIONAL and the reason is a design error found on a live session (2026-09-27).
  # The first version required `identical(r$seq, pending$seq)` unconditionally,
  # on the theory that the confirmation had to be "bound to this scenario's
  # sequence". It cannot be: the MODULE has no access to the drive's sequence - it
  # is not a widget, not an input, not anything the module can read - so it could
  # only ever answer `NA`, and the run was refused 100% of the time with
  #   "the confirmation ... is for a different scenario (seq NA, expected 5)"
  # i.e. the guard was not a guard, it was a wall.
  #
  # What actually binds a confirmation to its scenario is the RECORD, and it is
  # structural, not declared: the drive holds one pending record at a time, it
  # carries that scenario's own `seq`, `values` and `key`, it is matched against
  # the live session token, and it is CLEARED the moment it reaches a verdict. A
  # second scenario cannot reuse it, and a queued scenario is not even read until
  # the pending one has resolved. So a stale confirmation has nothing to satisfy.
  # A module is still free to return a `seq` and be held to it; it is simply not
  # required to, and `NA`/`NULL` means "I have no sequence to declare".
  if (!is.null(r$seq) && length(r$seq) == 1L && !is.na(r$seq) &&
      !identical(as.integer(r$seq), as.integer(pending$seq))) {
    return(refuse(sprintf(
      "the confirmation from module '%s' declares seq %s but the pending run is seq %s; it is refused. No run started",
      pending$module, as.character(r$seq), pending$seq)))
  }
  differs <- as.character(r$differs %||% character(0))
  waiting <- as.character(r$waiting %||% character(0))
  # 🔴 COMPARE ONLY THE REQUESTED IDS. `r$observed` is the module's whole view of
  # the panel (MEASURED: `bulk_de` reports all five controls), while `pending$key`
  # was built from the ids the SCENARIO carried — so hashing the full observation
  # against the pending key can never match once the two differ in size, and the
  # run waits out its bound and is refused even though every requested value was
  # correct. MEASURED live (2026-09-27): a scenario injecting only
  # `bulk-de-shrink_lfc` was refused at the bound with the DOM reading
  # `shrink_lfc: checked=false`.
  #
  # The drive validated `pending$values` and nothing else, so it must confirm
  # exactly those ids. A module that reports FEWER of them cannot produce a
  # matching key, which fails closed rather than open.
  obs <- r$observed %||% list()
  obs_ids <- if (length(obs)) intersect(pending$ids, names(obs)) else character(0)
  if (isTRUE(r$ok) && length(obs_ids) &&
      identical(ts_drive_confirm_key(obs[obs_ids]), pending$key)) {
    if (is.null(effects) || !isTRUE(effects(pending$button))) {
      return(refuse(sprintf(
        "the inputs for seq %s were confirmed but the button could not be fired; no run started",
        pending$seq)))
    }
    # 🔴 DECLARE THE JOB, or the run hangs at `running` forever.
    #
    # `ts_drive_apply()` calls `ts_drive_job_begin()` for a `long = TRUE` entry
    # BEFORE returning `running`, and that declaration is what the module's
    # `on.exit(ts_drive_job_finish(...))` closes. This fire path went straight to
    # `effects(button)`, so no job was ever in flight: the module's terminal was
    # refused (it may only declare the job that is in flight) and `result.json`
    # stayed `running` for good.
    #
    # MEASURED live (2026-09-27): the confirmation succeeded and the log said
    # `inputs confirmed ... run fired`, the heartbeat kept climbing (so nothing was
    # blocked and nothing had thrown), and the verdict never moved off `running`.
    # The button HAD been fired — only the job bookkeeping was missing.
    if (isTRUE(ts_drive_entry_long(entry))) {
      ts_drive_job_begin(pending$seq, pending$module, "run_pipeline",
                         pending$button, owner_token = token,
                         timeout_s = ts_drive_entry_timeout(entry))
    }
    out$consumed <- TRUE
    out$status   <- if (isTRUE(ts_drive_entry_long(entry))) "running" else "done"
    out$job_status <- if (isTRUE(out$status == "running")) "running" else NULL
    out$module   <- pending$module
    out$action   <- "run_pipeline"
    out$last_seq <- pending$seq
    out$pending  <- NULL
    message(sprintf("[drive] inputs confirmed for seq=%s module=%s (%s); run fired",
                    pending$seq, pending$module,
                    paste(pending$ids, collapse = ", ")))
    return(out)
  }
  if (beats > TS_DRIVE_CONFIRM_MAX_BEATS) {
    # 🔴 WHY THERE IS NO IMMEDIATE "A HUMAN CHANGED THIS" REFUSAL.
    #
    # The first version refused the moment a control's observed value was neither
    # the wanted one nor the pre-injection one, and said a human had changed it.
    # TWO live measurements killed it, both on a session with no human in it:
    #
    #   * "'bulk-de-condition_col', 'bulk-de-group_target', 'bulk-de-group_ref',
    #     'bulk-de-shrink_lfc' differ ... a human changed the control" — the round
    #     trip had simply not landed. `prior` fixes that class, and the count fell
    #     from four ids to two.
    #   * "'bulk-de-group_target', 'bulk-de-group_ref' differ" — those two are
    #     `selectInput`s whose CHOICES are rebuilt when the condition column
    #     changes, so they pass through intermediate values that are neither the
    #     wanted nor the prior one. A re-render is not tampering, and the drive
    #     cannot tell the two apart from `input` alone.
    #
    # So "neither" is not a verdict: the run WAITS — which creates no job and
    # re-injects nothing — and is refused only at the bound. The refusal names the
    # ids and states BOTH causes, because claiming to know which it was would be
    # the same false accusation one layer down.
    #
    # SAFETY IS UNCHANGED. A run whose values never reach what the drive validated
    # is never fired, and the drive never re-sends its own value over whatever it
    # finds: during a wait the only writer of these controls is the human.
    ids <- unique(c(waiting, differs, missing <- as.character(r$missing %||% character(0))))
    why <- if (length(missing) && !length(differs) && !length(waiting)) {
      "the session never held a value for the control(s) at all (an empty or not-yet-populated widget)"
    } else if (length(differs) && !length(waiting) && !length(missing)) {
      "a human edited the control, or the panel rebuilt its options while the values were being applied"
    } else if (length(waiting) && !length(differs) && !length(missing)) {
      "the values never reached the session"
    } else {
      "the controls ended the wait in more than one state (some never arrived, some still held their pre-injection value, some held neither)"
    }
    return(refuse(sprintf(
      "the inputs for seq %s were not confirmed by module '%s' within %d poll beats: %s never matched (%s). The cause is %s; the drive cannot tell those apart, so the run is REFUSED. NO job was created and NOTHING was re-injected - re-request the run explicitly.",
      pending$seq, pending$module, TS_DRIVE_CONFIRM_MAX_BEATS,
      if (length(ids)) sprintf("%d control(s)", length(ids)) else "one or more controls",
      paste(sprintf("'%s'", ids), collapse = ", "), why)))
  }
  p <- pending
  p$beats <- beats
  out$pending <- p
  out
}


# =============================================================================
# The tick — the whole reasoning of one poll cycle
# =============================================================================

#' Apply one already-validated scenario.
#'
#' Called from the poller, which lives in `modules/` (the reactivity layer).
#' `R/` only computes WHAT to do; the module owns the effects.
#'
#' @param effects Callback invoked by the poller to bump one button token:
#'   `effects(input_id)` -> TRUE when a bound token was incremented.
#' @return list(status, errors, warnings, active_module)
#' The injection PLAN for a set of controls: a list of stages, in order.
#'
#' Read from `TS_DRIVE_INPUT_STAGES`. A control the table does not mention is
#' assumed INDEPENDENT and goes in the first stage — that is the default, and it is
#' the historical behaviour, because a stage costs a protocol beat and an ordering
#' is therefore declared only where a dependency has been measured.
#'
#' @param module The module whose controls these are.
#' @param ids The control ids the scenario carried, after allowlist filtering.
#' @param values The named list of values, same names as `ids`.
#' @return A list of named lists. Length 1 means "inject it all at once".
#' @noRd
ts_drive_input_stages <- function(module, ids, values) {
  decl <- TS_DRIVE_INPUT_STAGES[[module]]
  if (is.null(decl) || !length(decl)) return(list(values))
  out <- list()
  used <- character(0)
  for (stage in decl) {
    take <- intersect(stage, ids)
    if (length(take)) {
      out[[length(out) + 1L]] <- values[take]
      used <- c(used, take)
    }
  }
  rest <- setdiff(ids, used)
  # Undeclared controls join the FIRST stage: the table is a statement about
  # dependencies, and a control absent from it has none that we know of.
  if (length(rest) && length(out)) out[[1]] <- c(out[[1]], values[rest])
  if (!length(out)) out <- list(values)
  out
}

#' The injector the poller hands to the confirmation service.
#'
#' A closure over the session and the effect seam, so the service can inject a
#' later stage without knowing anything about Shiny. Exported here so the unit
#' tests can substitute a recorder.
#' @noRd
ts_drive_injector <- function(session, effects) {
  function(inputs, module) {
    ts_drive_apply_inputs(session, inputs, module,
                          tokens = ts_drive_tokens_for(module, effects))
  }
}

ts_drive_apply <- function(session, input, scn, effects = NULL, owner_token = NULL,
                           inject = NULL) {
  warnings <- character(0)
  errors   <- character(0)
  action   <- scn$action
  module   <- scn$module

  if (identical(action, "noop")) {
    return(list(status = "done", errors = character(0), warnings = warnings,
                active_module = module, nav = NULL))
  }

  if (identical(action, "snapshot")) {
    return(list(status = "done", errors = character(0), warnings = warnings,
                active_module = module, nav = NULL))
  }
  # ── S2: export_result ─────────────────────────────────────────────────────
  # ONE artefact, and the caller chooses NOTHING. The route is named by
  # `TS_DRIVE_EXPORT_ROUTES`, the destination by `ts_drive_export_dir()`, the
  # the filename by the module's own `ts_drive_export_next_path()` over the
  # shared `TS_DRIVE_EXPORT_STEM`, the format by the module's own
  # serialiser, and the request carries no field at all. Everything a download
  # verb normally takes from its caller is therefore absent by construction
  # rather than by validation — which is the only arrangement in which "the client
  # cannot choose an arbitrary destination" is a property of the protocol instead
  # of a promise about its discipline.
  #
  # The freshness gate lives ABOVE this branch (the poller only dispatches to a
  # live session), so a stale handshake never reaches the exporter. That is why
  # this function does not re-check it: a second check would be a second rule, and
  # two rules about liveness drift.
  if (identical(action, "export_result")) {
    if (!module %in% TS_DRIVE_EXPORT_MODULES) {
      return(list(status = "invalid", errors = c(errors, sprintf(
        "module '%s' has no export route; the only exported artefact belongs to %s",
        module, paste(TS_DRIVE_EXPORT_MODULES, collapse = ", "))),
        warnings = warnings, active_module = module, nav = NULL))
    }
    v <- ts_drive_validate_export_request(scn$import)
    if (!isTRUE(v$ok)) {
      return(list(status = "invalid", errors = c(errors, v$errors),
                  warnings = warnings, active_module = module, nav = NULL))
    }
    out <- if (is.null(effects)) NULL else
      tryCatch(effects(NULL, mode = "export", module = module, request = v$request),
               error = function(e) e)
    if (inherits(out, "condition")) {
      return(list(status = "error", errors = c(errors, sprintf(
        "the exporter raised: %s", ts_drive_badge_sanitize(conditionMessage(out), 200L))),
        warnings = warnings, active_module = module, nav = NULL))
    }
    # MEASURED, and this guard exists because of it. The app-side `effects` seam
    # had no `export` branch on the first run, so an unknown `mode` fell through to
    # the registry path and returned atomic `FALSE`. The `out$ok` below then raised
    # "$ operator is invalid for atomic vectors", and because this function is
    # called from the ONE reactive beat of the poller, the error killed the
    # observer: the handshake stopped being rewritten and the session was reported
    # lost. A missing seam must degrade to a VERDICT, never to a dead poller.
    if (!is.null(out) && !is.list(out)) {
      return(list(status = "invalid", errors = c(errors, sprintf(
        "the app's effect seam returned a %s for mode 'export', not a verdict list - the seam is not wired",
        class(out)[[1L]])),
        warnings = warnings, active_module = module, nav = NULL))
    }
    if (is.null(out)) {
      return(list(status = "invalid", errors = c(errors, sprintf(
        "module '%s' published no exporter - its server() does not call ts_drive_publish_export()",
        module)), warnings = warnings, active_module = module, nav = NULL))
    }
    if (!isTRUE(out$ok)) {
      return(list(status = out$status %||% "invalid",
                  errors = c(errors, out$errors %||% "the exporter refused"),
                  warnings = c(warnings, out$warnings %||% character(0)),
                  active_module = module, nav = NULL))
    }
    return(list(status = "done", errors = character(0), warnings = warnings,
                active_module = module, nav = NULL,
                descriptor = out$descriptor))
  }

  # ── R2: read_export — the ONE bounded read ────────────────────────────────
  # Zero per-module code: the responder is GENERIC over the routes. It derives
  # the target STATELESSLY from result.json (mirror of the server's R1
  # derivation), streams the file (header + at most K records through a
  # connection — never a full read.csv), applies every declared bound, and
  # returns the read block as the verdict's descriptor, projected by
  # ts_drive_read_descriptor at the writer. `module` is deliberately absent
  # from the scenario (relaxed gate above); the block's `route` is the export
  # verdict's own module, for the agent's information only.
  if (identical(action, "read_export")) {
    return(ts_drive_read_export_respond(scn$seq %||% 0L, scn$max_rows))
  }

  if (identical(module, "spatial_pipeline") && identical(action, "set_inputs")) {
    return(list(
      status = "invalid",
      errors = "module 'spatial_pipeline' does not accept set_inputs",
      warnings = warnings, active_module = module, nav = NULL
    ))
  }

  # Same rule, same reason, for the SC auto-pipeline, SC annotation, SC marker,
  # SC pathway, Bulk signature, Bulk pattern and Bulk network actions: their
  # inputs are FROZEN, DECLARED sets, and an injected input could not be honoured
  # without silently changing the action boundary. For the signature action the
  # refusal also keeps a fileInput PATH (`sig_rds`) out of reach of a remote
  # caller.
  if (identical(action, "set_inputs") &&
      module %in% c(TS_DRIVE_SC_MODULE, TS_DRIVE_SC_ANNOTATION_MODULE,
                    TS_DRIVE_SC_MARKERS_MODULE, TS_DRIVE_SC_PATHWAYS_MODULE,
                    TS_DRIVE_BULK_SIGNATURES_MODULE,
                    TS_DRIVE_BULK_PATTERN_MODULE,
                    TS_DRIVE_BULK_NETWORK_MODULE,
                    TS_DRIVE_SPATIAL_QC_MODULE)) {
    return(list(
      status = "invalid",
      errors = sprintf("module '%s' does not accept set_inputs", module),
      warnings = warnings, active_module = module, nav = NULL
    ))
  }

  # The human must SEE the target tab (spec §2.3: the same httpuv session).
  # Collecting the navigation OUTCOME here lets the module perform it without
  # any `bslib::nav_select()` call living in R/.
  nav <- ts_drive_nav_plan(module, scn$expect$nav %||% NULL)

  applied <- list(applied = character(0), refused = character(0), warnings = character(0))

  # ── Slice 3: resolve SESSION-DERIVED inputs (index -> value) ──────────────
  # BEFORE any injection or confirmation, so the adapters and the confirm
  # handshake keep working on real values — zero behaviour change downstream.
  # The probe is the module's OWN vocabulary closure (published through the
  # same projection the wire sees), so the rev the agent pinned and the
  # choices resolved here come from ONE closure: a change in between bumps the
  # rev and the resolution refuses VOCAB_STALE. Plain-string values (the
  # internal path) pass through untouched.
  if (length(scn$inputs)) {
    sess <- intersect(names(scn$inputs), names(TS_DRIVE_SESSION_INPUTS))
    if (length(sess)) {
      entries <- ts_drive_tokens_for(module, effects)
      vocab_fn <- NULL
      for (nm in names(entries)) {
        e <- entries[[nm]]
        if (is.list(e) && is.function(e$vocab)) { vocab_fn <- e$vocab; break }
      }
      rv <- ts_drive_resolve_session_inputs(scn$inputs, module, vocab_fn)
      if (!isTRUE(rv$ok)) {
        return(list(status = "invalid", errors = rv$errors, warnings = warnings,
                    active_module = module, nav = nav))
      }
      scn$inputs <- rv$values
    }
  }

  # A `run_pipeline` that injects non-button inputs is DEFERRED below, and a
  # deferred run injects in STAGES, one per beat. Injecting it here would hand a
  # value to a `selectInput` whose options do not exist yet, and a browser select
  # cannot represent such a value: it is silently dropped and the module's own
  # default takes its place. MEASURED live (2026-09-27, GSE164073) — the pair came
  # out as the module's defaults and the run was refused at the bound.
  # Every other action injects here, exactly as before.
  staged_run <- identical(action, "run_pipeline") &&
    length(scn$inputs) && !is.null(TS_DRIVE_INPUT_STAGES[[module]])
  if (length(scn$inputs) && !staged_run) {
    # ONE seam for every injection, staged or eager. This used to call
    # `ts_drive_apply_inputs()` directly while the staged path went through
    # `inject`, so the two were different code paths: a double injection (restoring
    # the eager call for a deferred run) was real in production and INVISIBLE to
    # every test, because the recorder only saw the staged side. Measured.
    inj <- inject %||% (if (is.function(effects)) ts_drive_injector(session, effects) else NULL)
    if (!is.null(inj)) {
      applied <- inj(scn$inputs, module)
      warnings <- c(warnings, applied$warnings)
    }
  }

  if (identical(action, "set_inputs")) {
    return(list(status = "applied", errors = character(0), warnings = warnings,
                active_module = module, nav = nav))
  }

  if (identical(action, "run_pipeline")) {
    btn <- scn$button %||% switch(module,
      import_bulk   = "import_bulk-btn_load",
      bulk_filter   = "bulk-filter-run_filter_norm",
      bulk_de       = "bulk-de-run_de",
      bulk_pathways = "bulk-pathways-run_pathway",
      bulk_signatures = TS_DRIVE_BULK_SIGNATURES_BUTTON,
      bulk_pattern  = TS_DRIVE_BULK_PATTERN_BUTTON,
      bulk_network  = TS_DRIVE_BULK_NETWORK_BUTTON,
      spatial_pipeline = "spatial-pipeline-btn_run_all",
      spatial_qc    = TS_DRIVE_SPATIAL_QC_BUTTON,
      sc_pipeline   = TS_DRIVE_SC_BUTTON,
      sc_annotation = TS_DRIVE_SC_ANNOTATION_BUTTON,
      sc_markers    = TS_DRIVE_SC_MARKERS_BUTTON,
      sc_pathways   = TS_DRIVE_SC_PATHWAYS_BUTTON,
      NULL)
    if (is.null(btn) || !btn %in% TS_DRIVE_BUTTONS) {
      errors <- c(errors, sprintf("module '%s' has no bound button for run_pipeline", module))
      return(list(status = "invalid", errors = errors, warnings = warnings,
                  active_module = module, nav = nav))
    }
    if (!identical(ts_drive_button_module(btn), module)) {
      errors <- c(errors, sprintf(
        "button '%s' belongs to module '%s', not '%s'", btn,
        ts_drive_button_module(btn), module
      ))
      return(list(status = "invalid", errors = errors, warnings = warnings,
                  active_module = module, nav = nav))
    }

    # CONTRACT B — ONE job at a time (spec §5). A second `run_pipeline` while a
    # job is in flight is REFUSED, not queued: a queue would let an agent pile
    # up work it can never observe, and the module's own `observeEvent` would
    # run the second one on top of the first's state. `invalid` is the honest
    # status — nothing ran, and the message names the job that blocks.
    #
    # Checked BEFORE the readiness probe so the two refusals stay distinct: a
    # busy session is ready, it is simply occupied.
    if (ts_drive_job_busy()) {
      job <- ts_drive_job_state()
      errors <- c(errors, sprintf(
        "another job is already running (module '%s', seq %s, started %s) — wait for result.json to reach a terminal status",
        job$module, job$seq, job$started_at))
      return(list(status = "invalid", errors = errors, warnings = warnings,
                  active_module = module, nav = nav))
    }

    # ASK BEFORE FIRING. The owning module is the only side that can know
    # whether its button would do anything (see TS_DRIVE_READY), and the
    # refusal has to happen HERE — before the counter moves — or the module's
    # `req()` aborts in silence and the agent is told `done` for work that
    # never ran. An unpublished button answers `unknown` and falls through to
    # the "not bound" branch below, so the two failures stay distinguishable:
    #   not bound  -> the WIRING is missing (the G0/G1 live finding)
    #   not ready  -> the wiring is there and the module refused (no object)
    entry <- ts_drive_tokens_for(module, effects)[[btn]]
    probe <- ts_drive_ready_probe(entry)
    if (probe$verdict %in% c("not-ready", "probe-failed")) {
      errors <- c(errors, ts_drive_ready_refusal(btn, probe))
      return(list(status = "invalid", errors = errors, warnings = warnings,
                  active_module = module, nav = nav))
    }

    # ── S6: CONFIRM BEFORE FIRING, when inputs were injected ──────────────
    # Every adapter above is `shiny::update*Input()`, a CLIENT ROUND-TRIP, so a
    # value injected by this scenario is not yet in the server's `input` when this
    # same tick fires the button. MEASURED live over four runs: run N read run
    # N-1's values, so `shrink_lfc = FALSE` was ignored (the run warned as if it
    # were TRUE) and a bare run hit the observer's `req()` and was recorded
    # `invalid` with no reason.
    #
    # `session$setInputs()` is NOT the fix: it does not exist on a live session
    # (mod_import_bulk.R:298) and is testServer-only (this file's header). So the
    # run is DEFERRED across beats and fired only once the OWNING MODULE confirms
    # it observes the values validated for THIS scenario. Fail-closed: a module
    # that publishes no `confirm_inputs` cannot be confirmed, so a run that
    # injects non-button inputs into it is REFUSED rather than fired blind.
    nb <- ts_drive_nonbutton_inputs(scn$inputs)
    if (length(nb)) {
      conf <- entry$confirm_inputs
      if (!is.function(conf)) {
        errors <- c(errors, sprintf(
          "module '%s' cannot confirm injected inputs, so this run is refused: it publishes no confirm_inputs(). Without a confirmation the run would read whatever the session happened to hold. No job was created.",
          module))
        return(list(status = "invalid", errors = errors, warnings = warnings,
                    active_module = module, nav = nav, pending_confirm = NULL))
      }
      vals <- scn$inputs[nb]
      # The injection PLAN, and stage 1 goes in NOW. Injecting here rather than on
      # the next servicing beat is what keeps a single-stage scenario on the fast
      # path: it is fully injected at deferral time, exactly as before, and only a
      # DEPENDENT stage costs an extra beat.
      stages <- ts_drive_input_stages(module, nb, vals)
      # ONE seam for every stage. The poller's injector is preferred so that the
      # first stage and the later ones go through the same path — a first version
      # built a fresh injector here from `session` + `effects`, which meant the
      # caller's seam saw only stages 2..n and stage 1 was invisible to it.
      inject1 <- inject %||% (if (is.function(effects)) ts_drive_injector(session, effects) else NULL)
      if (!is.null(inject1)) {
        got <- inject1(stages[[1]], module)
        warnings <- c(warnings, got$warnings)
      }
      # 🔴 THE PRIOR VALUES, and the reason this handshake needs a third state.
      # `shiny::update*Input()` is a CLIENT ROUND-TRIP, so on the beat the button
      # would be fired the new values are not in `input` yet. The module then sees
      # the SESSION's previous values, and a "did the human change this?" test
      # cannot tell that apart from a deliberate human edit.
      # MEASURED live (2026-09-27), and the symptom is the worst kind: the drive
      # refused with "the values of 'bulk-de-condition_col', ... differ ... a human
      # changed the control" — on a session with NO human in it, blaming one for a
      # lag it caused. Three states, and only the drive can see all three:
      #   observed == wanted  -> confirmed
      #   observed == prior   -> the round trip has not landed yet; WAIT
      #   observed == neither -> a human edit; REFUSE
      # Without `prior` the middle state is unreachable and every first injection
      # is misreported as tampering.
      #
      # Read with `isolate()`: this runs inside the poller's observer, so a bare
      # read of `input` would enrol the protocol in every widget change and make
      # the observer re-run on each one. The fall back to the trailing id segment
      # covers a runtime that hands us an already-namespaced `input`.
      prior <- list()
      for (id in nb) {
        v <- tryCatch(shiny::isolate(input[[id]]), error = function(e) NULL)
        if (is.null(v)) {
          v <- tryCatch(shiny::isolate(input[[sub("^.*-", "", id)]]),
                        error = function(e) NULL)
        }
        prior[[id]] <- v
      }
      return(list(
        status = "running", errors = errors, warnings = warnings,
        active_module = module, nav = nav,
        pending_confirm = list(seq = scn$seq, module = module, button = btn,
                               values = vals, ids = nb, prior = prior,
                               # The staging plan. `stage` counts the stages already
                               # INJECTED, so `stage == length(stages)` means the whole
                               # plan is in and the confirmation may begin.
                               stages = stages, stage = 1L,
                               key = ts_drive_confirm_key(vals),
                               # 🔴 THE IDENTITY IS `owner_token`, NOT `scn$session_token`.
                               # `scn` here is the scenario REBUILT by the validator, and
                               # that whitelist (seq/module/action/preserve_data/inputs/
                               # inputs_ok/expect/button/import) does NOT carry
                               # `session_token` — the validator reads the declared
                               # token for its PIN CHECK and then drops it. So
                               # `scn$session_token` is always NULL, the record stored
                               # `""`, and the identity check rejected EVERY
                               # confirmation as "the session was replaced".
                               # MEASURED (unit, via the round-trip diagnostic): beat 2
                               # returned `invalid` with exactly that message, 100% of
                               # the time, on a healthy single session.
                               # `owner_token` IS the live consuming session, and the
                               # pin check has already proven the declared token equals
                               # it, so this is the same identity by another route — and
                               # it cannot drift from the session doing the work.
                               session_token = as.character(owner_token %||%
                                                             scn$session_token %||% ""),
                               beats = 0L)))
    }

    if (is.null(effects) || !isTRUE(effects(btn))) {
      errors <- c(errors, sprintf(
        "button '%s' is not bound — its observeEvent does not read ts_drive_bind_button()/ts_drive_button_token()",
        btn))
      return(list(status = "invalid", errors = errors, warnings = warnings,
                  active_module = module, nav = nav))
    }

    # CONTRACT A — a job DECLARED long is `running`, not `done`.
    #
    # The declaration is the MODULE's, through `long = TRUE` on
    # `ts_drive_publish_token()`, because only the module knows its own cost.
    # It is a declaration and not a guess: the DE module cites the 852.7 s
    # measurement that justifies it.
    #
    # `done` here would be TERMINAL for the seq and is written before the work
    # starts (see the block comment above `ts_drive_job_state()`), so a long
    # job answering `done` tells an agent to stop polling for a result that
    # does not exist yet. `running` is non-terminal, so the agent keeps
    # polling — and it is ALSO the proof of life while the synchronous job
    # blocks the event loop and the heartbeat stalls.
    if (isTRUE(ts_drive_entry_long(entry))) {
      ts_drive_job_begin(
        scn$seq, module, action, btn,
        owner_token = owner_token,
        timeout_s = ts_drive_entry_timeout(entry)
      )
      return(list(status = "running", errors = character(0), warnings = warnings,
                  active_module = module, nav = nav))
    }

    # What `done` means here, precisely: the token moved, so the module's
    # `observeEvent` HAS BEEN TRIGGERED. It has not necessarily finished. The
    # counter is set from inside the poller's own observer, and Shiny runs the
    # dependent observer on the NEXT step of the same flush — i.e. after this
    # tick returns. So `done` is terminal for the SEQ (nothing more will be
    # written for it), never a claim that the pipeline completed; the
    # pipeline's own outcome is read through the following `snapshot`.
    #
    # That is only honest while the work is SHORT enough to finish within the
    # flush's tail. A module whose job outlives it must declare `long = TRUE`
    # and close the loop with `ts_drive_job_finish()`.
    return(list(status = "done", errors = character(0), warnings = warnings,
                active_module = module, nav = nav))
  }

  if (identical(action, "import_file")) {
    # G3. The MODULE owns the load: `smart_read()`, the numeric coercion, the
    # `min_counts` filter and the write to `global_data$bulk_obj` all live in
    # modules/import/mod_import_bulk.R, and `R/` may touch none of them (C2, and
    # spec S6 "the watcher must not run DESeq2/Seurat"). So the request travels
    # as DATA through the same `effects` callback that fires buttons — never as
    # a string handed to `update*()` (spec S5).
    req <- scn$import
    # 🔴 THIS GUARD WAS BULK-ONLY AND SAID SO. It asked for `counts_path` by name,
    # so an `import_file` aimed at `import_spatial` — which carries a `dir_path` —
    # was refused with "`import_file` needs an `import` block carrying
    # `counts_path`" before the module was ever consulted. MEASURED on a live
    # session (Phase F, 2026-09-26), and it is the second of TWO independent
    # `counts_path` assumptions in this file; the other is the key schema, which
    # `ts_drive_validate_import()` now routes on the module. A second copy of a
    # rule is exactly what a grep for the obvious spelling misses.
    #
    # The required keys are therefore READ FROM THE SCHEMA rather than named
    # here, so a third importer needs no third edit in this function.
    #
    # ALL of them, not the first. `need` became a vector when `import_sc` arrived
    # (it carries `dir_path` AND `sample_name`), and `req[[need]]` with a vector
    # subscript would have addressed `req[[c("dir_path","sample_name")]]` — a
    # silent, total miss that would have let a half-populated payload reach the
    # importer. The schema is not the gate this function uses; it is a second one.
    need <- TS_DRIVE_IMPORT_SCHEMA[[module]]$required
    if (is.null(need)) {
      errors <- c(errors, sprintf(
        "`import_file` for '%s' needs an `import` block carrying `%s`", module,
        "<the module's required key>"))
      return(list(status = "invalid", errors = errors, warnings = warnings,
                  active_module = module, nav = nav))
    }
    if (is.null(req) || !is.list(req)) {
      errors <- c(errors, sprintf(
        "`import_file` for '%s' needs an `import` block carrying %s", module,
        paste0("`", need, "`", collapse = " and ")))
      return(list(status = "invalid", errors = errors, warnings = warnings,
                  active_module = module, nav = nav))
    }
    lack <- need[vapply(need, function(k) is.null(req[[k]]), logical(1))]
    if (length(lack)) {
      errors <- c(errors, sprintf(
        "`import_file` for '%s' needs an `import` block carrying %s; absent: %s",
        module, paste0("`", need, "`", collapse = " and "),
        paste0("`", lack, "`", collapse = ", ")))
      return(list(status = "invalid", errors = errors, warnings = warnings,
                  active_module = module, nav = nav))
    }

    out <- if (is.null(effects)) NULL else
      tryCatch(effects(NULL, mode = "import", module = module, request = req),
               error = function(e) e)

    if (inherits(out, "condition")) {
      # The importer THREW. That is an app-side failure, not a refusal, and the
      # two must stay distinguishable: `error` is terminal-with-a-cause,
      # `invalid` means the payload was declined.
      errors <- c(errors, sprintf("the importer raised: %s", conditionMessage(out)))
      return(list(status = "error", errors = errors, warnings = warnings,
                  active_module = module, nav = nav))
    }
    if (is.null(out)) {
      # The seam is missing, not the data. Naming it keeps "G3 not wired into
      # this module" from looking like "your file was rejected".
      errors <- c(errors, sprintf(
        "module '%s' published no importer — its server() does not call ts_drive_publish_importer()",
        module))
      return(list(status = "invalid", errors = errors, warnings = warnings,
                  active_module = module, nav = nav))
    }
    if (!isTRUE(out$ok)) {
      errors <- c(errors, out$errors %||% "the importer refused the request")
      return(list(status = out$status %||% "invalid", errors = errors,
                  warnings = c(warnings, out$warnings %||% character(0)),
                  active_module = module, nav = nav))
    }
    # `done`, and this is the ONE status that lets an agent STOP polling.
    #
    # MEASURED ON A LIVE SESSION, and my first choice was WRONG: I returned
    # `applied`, reasoning that a load is "acknowledged but maybe not finished".
    # But `applied` is by definition NOT terminal
    # (`ts_drive_status_terminal("applied")` is FALSE), while this load is
    # SYNCHRONOUS — the importer has already written `global_data$bulk_obj` by
    # the time this returns. So an agent doing exactly what the spec says
    # ("poll until `ack_seq == seq` and the status is terminal, or `timeout_s`")
    # waited its FULL timeout on a load that had succeeded seconds earlier: the
    # live driver burned 300.7 s and recorded
    # `TIMEOUT — no terminal result.json`, while a following `snapshot` was
    # already answering `has_data=TRUE, genes=17925, samples=18`.
    #
    # The unit tests could not catch this: they asserted `applied` because I
    # wrote them from the implementation. A refusal stays `invalid`/`error`.
    return(list(status = "done", errors = character(0),
                warnings = c(warnings, out$warnings %||% character(0)),
                active_module = module, nav = nav))
  }

  if (identical(action, "reset_module")) {
    errors <- c(errors, "reset_module is not implemented in this grade (G3)")
    return(list(status = "invalid", errors = errors, warnings = warnings,
                active_module = module, nav = nav))
  }

  errors <- c(errors, sprintf("action '%s' reached the injector without a handler", action))
  list(status = "invalid", errors = errors, warnings = warnings,
       active_module = module, nav = nav)
}

#' Fetch the tokens a module announced, through the poller callback.
#'
#' `effects` is the module-side closure: `effects(input_id)` bumps a button
#' token, `effects(ids_only = TRUE)` lists the announced token ids. One
#' callback, two uses — the module is the only side that can read them.
#' @noRd
ts_drive_tokens_for <- function(module, effects) {
  if (is.null(effects)) return(list())
  out <- tryCatch(effects(NULL, mode = "tokens", module = module), error = function(e) { ts_log_swallow("drive_watcher.ts_drive_tokens_for", e); list() })
  if (is.list(out)) out else list()
}

#' Compute the navigation plan for a module (data, no Shiny call).
#'
#' @param module Scenario module.
#' @param target_tab Optional value of `bulk-main_tabs` requested by `expect`.
#' @return list(top = "tab_bulk", tab = <value-or-NULL>, panel = <value-or-NULL>)
ts_drive_nav_plan <- function(module, target_tab = NULL) {
  if (identical(module, "spatial_pipeline")) {
    return(list(
      top = TS_DRIVE_SPATIAL_TOP_TAB,
      tab = TS_DRIVE_SPATIAL_TAB,
      panel = TS_DRIVE_SPATIAL_PANELS,
      tab_id = TS_DRIVE_SPATIAL_TABS_ID,
      accordion_id = TS_DRIVE_SPATIAL_ACCORDION_ID
    ))
  }
  # The Spatial QC child module, measured in mod_spatial.R: the results navset is
  # the SAME one (`spatial-results`), only the value differs, and the controls live
  # in the SAME accordion (`spatial-steps`). The pipeline branch above hard-codes
  # both values, so this one is a separate explicit branch rather than a parameter
  # of it — a shared branch would have meant guessing which of the two a new
  # Spatial module wanted.
  if (identical(module, TS_DRIVE_SPATIAL_QC_MODULE)) {
    return(list(
      top = TS_DRIVE_SPATIAL_TOP_TAB,
      tab = TS_DRIVE_SPATIAL_QC_TAB,
      panel = TS_DRIVE_SPATIAL_QC_PANELS,
      tab_id = TS_DRIVE_SPATIAL_TABS_ID,
      accordion_id = TS_DRIVE_SPATIAL_ACCORDION_ID,
      # S1: the FOURTH level. This module nests a navset of its own inside the
      # `spatial-results` one, and the hotspot map / histogram / table / CSV
      # button live in it. `sub_tab` is deliberately ABSENT for every other module,
      # and `ts_drive_perform_nav()` treats its absence as "three levels, as
      # before" rather than inventing one.
      sub_tab = TS_DRIVE_SPATIAL_QC_SUB_TAB,
      sub_tab_id = TS_DRIVE_SPATIAL_QC_SUB_TABS_ID
    ))
  }
  # The Spatial import lives on a DIFFERENT navbar page from every Spatial
  # analysis tab, and that page's value is not a stable id. MEASURED on a live
  # session: `nav_panel(i18n$t("Spatial"), ...)` (app.R:395) passes no `value=`,
  # so bslib uses the title TAG, and after clicking Import > Spatial the navbar
  # reads
  #   "<span class=\"i18n\" data-key=\"Spatial\">Spatial</span>"
  # A 47-character HTML fragment emitted by the i18n shim. Hard-coding it would
  # buy a navigation effect whose failure mode is a silent no-op the day the shim
  # markup changes, so the plan is DELIBERATELY empty: `top = NULL` means "do not
  # navigate", which is a documented outcome of this function and not a missing
  # branch. The import announces itself with a `showNotification`, which is how a
  # human watching sees it. `test-drive-watcher.R` pins the measured value so the
  # reasoning stays falsifiable.
  if (identical(module, TS_DRIVE_SPATIAL_IMPORT_MODULE)) {
    return(list(top = NULL, tab = NULL, panel = NULL,
                tab_id = NULL, accordion_id = NULL))
  }
  if (identical(module, TS_DRIVE_SC_PATHWAYS_MODULE)) {
    return(list(
      top = TS_DRIVE_SC_PATHWAYS_TOP_TAB,
      # S5: see the sc_markers branch — the barplot is a panel of `sc-main_tabs`.
      tab = TS_DRIVE_SC_RESULTS_TAB[[TS_DRIVE_SC_PATHWAYS_MODULE]],
      panel = TS_DRIVE_SC_PATHWAYS_PANELS,
      tab_id = TS_DRIVE_SC_MAIN_TABS_ID,
      accordion_id = TS_DRIVE_SC_PATHWAYS_ACCORDION_IDS
    ))
  }
  if (identical(module, TS_DRIVE_SC_MARKERS_MODULE)) {
    return(list(
      top = TS_DRIVE_SC_MARKERS_TOP_TAB,
      # S5: `tab`/`tab_id` were NULL. They opened the CONTROLS accordion and
      # stopped, but the reader is a panel of the `sc-main_tabs` results navset —
      # a SIBLING of the accordions (mod_sc.R:693) — and bslib does not render an
      # output in an unselected tab. The value comes from the shared table, which
      # `test-drive-watcher.R` cross-checks against the module's own
      # `nav_panel(value = )`, because a stale id is a silent no-op here.
      tab = TS_DRIVE_SC_RESULTS_TAB[[TS_DRIVE_SC_MARKERS_MODULE]],
      panel = TS_DRIVE_SC_MARKERS_PANELS,
      tab_id = TS_DRIVE_SC_MAIN_TABS_ID,
      accordion_id = TS_DRIVE_SC_MARKERS_ACCORDION_IDS
    ))
  }
  if (identical(module, TS_DRIVE_SC_ANNOTATION_MODULE)) {
    return(list(
      top = TS_DRIVE_SC_ANNOTATION_TOP_TAB,
      tab = TS_DRIVE_SC_RESULTS_TAB[[TS_DRIVE_SC_ANNOTATION_MODULE]],
      panel = TS_DRIVE_SC_ANNOTATION_PANELS,
      tab_id = TS_DRIVE_SC_MAIN_TABS_ID,
      accordion_id = TS_DRIVE_SC_ANNOTATION_ACCORDION_IDS
    ))
  }
  # No inner navset on the SC side, so `tab`/`tab_id` stay NULL: the panel lives
  # in a doubly nested accordion and BOTH ids must be opened. A scalar would open
  # the outer section and leave the auto-pipeline panel folded away, because
  # `acc_prep` opens on "1_pipeline" (mod_sc.R:41).
  if (identical(module, TS_DRIVE_SC_MODULE)) {
    return(list(
      top = TS_DRIVE_SC_TOP_TAB,
      tab = NULL,
      panel = TS_DRIVE_SC_PANELS,
      tab_id = NULL,
      accordion_id = TS_DRIVE_SC_ACCORDION_IDS
    ))
  }
  panel <- switch(module,
    bulk_de       = "panel_de",
    bulk_pathways = "panel_pathways",
    bulk_signatures = TS_DRIVE_BULK_SIGNATURES_PANELS,
    bulk_pattern  = TS_DRIVE_BULK_PATTERN_PANELS,
    bulk_network  = TS_DRIVE_BULK_NETWORK_PANELS,
    import_bulk   = NULL,
    NULL)
  list(
    top   = "tab_bulk",
    tab   = if (!is.null(target_tab) && target_tab %in% TS_DRIVE_BULK_TABS) target_tab else NULL,
    panel = panel,
    tab_id = TS_DRIVE_BULK_TABS_ID,
    accordion_id = TS_DRIVE_BULK_ACCORDION_ID
  )
}

#' One poll cycle.
#'
#' Returns `consumed = TRUE` when a scenario was applied (the caller then
#' raises `last_seq`). Everything is inside `tryCatch` so a malformed file can
#' never take the session down (spec S1).
#'
#' @param global_data The app-wide `reactiveValues`, read to build `snapshot`.
#' @param effects Module-side callback: `effects(input_id)` bumps one button
#'   token, `effects(NULL, mode = "tokens", module = m)` lists a module's
#'   announced tokens. `NULL` means no binding exists yet (G0/G1).
#' @param last_seq Highest applied seq so far.
#' @param armed Previously-known arm state, echoed into `result.json`.
#' @return list(consumed, last_seq, armed, error, nav)
  ts_drive_tick <- function(session, input, global_data, token, last_seq = 0,
                            armed = FALSE, effects = NULL, inject = NULL,
                            pending = NULL) {
    out <- list(consumed = FALSE, new_scenario = FALSE, published = FALSE,
                deferred = FALSE, selected = TRUE, last_seq = last_seq,
                armed = armed, error = NULL, nav = NULL, status = NULL,
                job_status = NULL, module = NULL, action = NULL, elapsed_s = NULL,
                pending = pending)
  selected <- .ts_drive_state$selected_token
  if (!is.null(selected) &&
      !identical(as.character(selected), as.character(token))) {
    out$selected <- FALSE
    out$armed <- FALSE
    return(out)
  }
    out$armed <- tryCatch(ts_drive_arm_state(token)$armed, error = function(e) FALSE)
    ts_drive_job_expire()

    # ── S6: service a PENDING confirmation before anything else ────────────
    # Same precedence discipline as the finished-job block below, for the same
    # reason: a confirmation resolved in the same beat as a queued scenario would
    # have the scenario's own outcome overwrite it.
    if (!is.null(pending)) {
      # The poller supplies the injector; a caller that omits it (an offline test)
      # gets one built from the session and the effect seam, so the staging path is
      # never silently inert.
      if (is.null(inject)) inject <- ts_drive_injector(session, effects)
      out <- ts_drive_service_pending(pending, token, effects, out, inject)
      # Still waiting: consume NOTHING this beat and let the next one look again.
      if (!is.null(out$pending)) return(out)
      if (isTRUE(out$consumed)) return(out)
    }


  # ── A job that FINISHED outranks every other concern this beat ────────────
  # Resolved BEFORE the arm gate and before `scenario.json` is even read, and
  # the beat then consumes NOTHING else. Both matter:
  #
  #   * before the arm gate, because a job dispatched while armed must still be
  #     reported if the session was disarmed while it ran — the agent is owed
  #     the terminal status of work it started;
  #   * consuming nothing else, because a queued scenario resolved in the SAME
  #     beat would overwrite `result.json` with its own outcome, and the
  #     terminal status would be lost before the agent could ever read it.
  #
  # A FAILED write keeps the job pending, so the next beat retries — the same
  # discipline as the heartbeat, which does not advance its throttle on a write
  # that did not land.
  pend <- ts_drive_job_pending()
  if (!is.null(pend)) {
    job <- ts_drive_job_state()
    wire_status <- pend$wire_status %||% pend$status
    errors <- if (identical(wire_status, "error")) {
      pend$error %||% "the job failed"
    } else {
      character(0)
    }
    wrote <- ts_drive_write_result(
      job$seq, wire_status, job$module, out$armed,
      errors   = errors,
      snapshot = ts_drive_snapshot(global_data)
    )
    if (isTRUE(attr(wrote, "written"))) {
      message(sprintf("[drive] job seq=%s module=%s finished status=%s elapsed=%.1fs",
                      job$seq, job$module, pend$status, pend$elapsed_s))
      ts_drive_job_clear()
      out$consumed   <- TRUE
      out$published  <- TRUE
      out$last_seq   <- job$seq
      out$status     <- wire_status
      out$job_status <- pend$status
      out$error      <- if (identical(wire_status, "error")) errors else NULL
      out$module     <- job$module
      out$action     <- job$action
      out$elapsed_s  <- pend$elapsed_s
    } else {
      message(sprintf("[drive] terminal write for seq=%s FAILED — retrying next beat",
                      job$seq))
    }
    return(out)
  }

  if (ts_drive_job_busy()) {
    out$deferred <- TRUE
    return(out)
  }

  if (!isTRUE(out$armed)) return(out)

  scn_raw <- ts_drive_read_json(ts_drive_path("scenario.json"))

  # AN ABSENT scenario.json IS NOT AN INVALID ONE.
  #
  # This distinction is load-bearing and was MEASURED, not reasoned: with the
  # file merely missing, `ts_drive_validate_scenario(NULL, ...)` returns
  # `invalid`, which the tick CONSUMES — so every single idle beat of an armed
  # session wrote a `status:"invalid"` result and queued a badge transition.
  # The poller would have reported a failure 1.25 times per second forever, and
  # the badge would have flickered through accepted/completed on a session where
  # nothing was ever asked of it. An idle tick must be a no-op (spec §10).
  #
  # A file that EXISTS but does not parse must stay `invalid`: there IS a
  # payload and it is malformed, and silently treating it as "nothing to do"
  # would hide a corrupted write from the agent forever.
  #
  # `ts_drive_read_json()` deliberately never throws, so it returns NULL for
  # BOTH "absent" and "unparseable". The existence test therefore has to be
  # made on the FILE, not on the parsed value — otherwise the two cases collapse
  # into one and a corrupted scenario.json is invisible.
  if (is.null(scn_raw) && !file.exists(ts_drive_path("scenario.json"))) return(out)

  raw_seq_value <- if (is.list(scn_raw)) scn_raw$seq else NULL
  raw_seq <- suppressWarnings(as.numeric(raw_seq_value %||% NA_real_))
  if (!is.na(raw_seq) && isTRUE(last_seq > 0) && raw_seq == last_seq) return(out)

  v <- ts_drive_validate_scenario(scn_raw, token, last_seq)

  if (identical(v$status, "invalid")) {
    ts_drive_write_result(if (is.null(v$scenario)) last_seq else v$scenario$seq,
                          "invalid", NA_character_, out$armed,
                          errors = v$errors, warnings = v$warnings,
                          snapshot = ts_drive_snapshot(global_data))
    out$consumed <- TRUE
    out$new_scenario <- TRUE
    # `invalid` is CONSUMED but deliberately NOT reported as an error badge:
    # "the payload was refused, nothing ran" is not an app failure, and a red
    # badge there would teach the operator to ignore red.
    out$status <- "invalid"
    return(out)
  }
  if (!identical(v$status, "applied-candidate")) return(out)

  scn      <- v$scenario
  preserve <- isTRUE(scn$preserve_data)

  # ONE scenario, ONE module (spec S7). The 32 GB budget forbids combining
  # sc + spatial + bulk, and the v1 allowlist is bulk-only anyway.
  t0  <- Sys.time()
  res <- ts_drive_apply(session, input, scn, effects = effects,
                        owner_token = token, inject = inject)

  message(sprintf("[drive] seq=%s module=%s action=%s status=%s preserve_data=%s",
                  scn$seq, scn$module, scn$action, res$status, preserve))

  ts_drive_write_result(
    scn$seq, res$status, res$active_module %||% scn$module, out$armed,
    preserve_data = preserve,
    errors   = c(v$errors, res$errors),
    warnings = c(v$warnings, res$warnings),
    snapshot = ts_drive_snapshot(global_data),
    # S2: the export DESCRIPTOR, so a `done` verdict is not a verdict with nothing
    # behind it. MEASURED on a live session: the export wrote a real 204 485-byte
    # file and reported `done`, and `result.json` carried no descriptor at all — an
    # agent could not learn the filename, the row count or the column names, so
    # "the export worked" was indistinguishable from "the export silently did
    # nothing". The descriptor is bounded scalars and short strings by
    # construction; it is projected through the same redaction as everything else.
    descriptor = res$descriptor,
    # R2: the read verdict's block is projected by ITS rule (guard + keep-set),
    # the export verdict's by the export rule — same writer, one parameter.
    descriptor_projector = if (identical(scn$action, "read_export")) {
      ts_drive_read_descriptor
    } else {
      ts_drive_export_descriptor
    }
  )

  out$consumed <- TRUE
  out$new_scenario <- TRUE
  out$last_seq <- scn$seq
  out$nav      <- res$nav
  # Badge-facing fields. They describe what HAPPENED, and are the only things
  # app.R may put on screen (module/action are short, static-ish labels).
  out$status    <- res$status
  out$job_status <- if (identical(res$status, "running")) "running" else NULL
  out$module    <- res$active_module %||% scn$module
  out$action    <- scn$action
  out$elapsed_s <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  # 🔴 S6: LIFT THE DEFERRED RECORD. `ts_drive_apply()` returns it as
  # `pending_confirm`, and this block copied `res$status`, `res$nav`,
  # `res$action` and `res$elapsed_s` — but NOT that field, so the record died
  # here. MEASURED on a live session (2026-09-27): seq 7 answered `running` (so
  # the deferral itself worked and the arm/beat loop was healthy, `hb_n`
  # climbing), and then NOTHING happened for 250+ s: no `[drive] inputs
  # confirmed`, and no 5-beat timeout refusal either. The wait could neither
  # complete nor expire, because the next beat was handed `pending = NULL`.
  #
  # ⚠️ This is the SECOND of two breaks in the same chain, and the offline suite
  # could not see either: every test drove `ts_drive_tick(pending = )` by hand and
  # asserted on the service directly, so the apply -> tick -> attach ROUND TRIP
  # was never executed. The unit tests were green on a path no live session takes.
  if (!is.null(res$pending_confirm)) out$pending <- res$pending_confirm
  out
}


# =============================================================================
# Badge — the reactive-state model (pure transition logic)
# =============================================================================
# The badge's reactive state lives in app.R (`drive_state`). What lives HERE is
# the pure decision: given the previous model and one protocol event, what is
# the next model? Keeping it in R/ means the transition table is testable
# without a browser, and app.R only assigns.
#
# Non-negotiable (spec): an IDLE tick must not move the badge. Only a real
# transition does — otherwise "done" would decay to "armed" every 800 ms and the
# human would never see the outcome.

#' Advance a badge model on ONE protocol event.
#'
#' @param model Previous model from `ts_drive_badge_model()` (or NULL).
#' @param event One of `TS_DRIVE_BADGE_EVENTS`.
#' @param status Scenario status when relevant (for `completed`).
#' @param ack_seq Sequence to acknowledge, if any.
#' @param module,action Short labels; sanitized later by `ts_drive_badge_view()`.
#' @param elapsed_s Seconds since the scenario started.
#' @param error Raw error message; sanitized later.
#' @return The next model. An unknown event returns `model` UNCHANGED, so a
#'   caller cannot move the badge by inventing an event name.
ts_drive_badge_advance <- function(model, event, status = NULL, ack_seq = NULL,
                                   module = NULL, action = NULL,
                                   elapsed_s = NULL, error = NULL) {
  if (is.null(model)) model <- ts_drive_badge_model()
  next_state <- ts_drive_badge_next(event, status = status)
  if (is.na(next_state)) return(model)

  out <- model
  out$state <- next_state
  out$armed <- next_state %in% c("armed", "running", "done", "error")
  if (!is.null(ack_seq)) out$ack_seq <- suppressWarnings(as.integer(ack_seq))
  if (!is.null(module))  out$module   <- ts_drive_badge_chr(module)
  if (!is.null(action))  out$action   <- ts_drive_badge_chr(action)
  if (!is.null(elapsed_s)) out$elapsed_s <- suppressWarnings(as.numeric(elapsed_s))
  # An error message is attached on `error` and CLEARED on every other event,
  # so a stale failure cannot linger next to a healthy state.
  out$error <- if (identical(next_state, "error")) ts_drive_badge_chr(error) else ""
  if (identical(next_state, "off")) {
    out$module <- ""; out$action <- ""
    out$elapsed_s <- NA_real_; out$error <- ""
  }
  out
}

#' Should the badge be shown at all?
#'
#' All four must hold (spec: "The badge must be absent when…"). Note this is the
#' DISPLAY gate, distinct from the arm gate: a session can be live, selected and
#' fresh yet simply not armed, and then nothing is shown.
#'
#' @param enabled `TRANSCRIPTO_DEV_DRIVE=1`?
#' @param interactive Live Shiny session?
#' @param selected Is this session the one `ready.json` names?
#' @param armed Is the session armed?
#' @param state Current badge state.
ts_drive_badge_visible <- function(enabled, interactive, selected, armed, state) {
  isTRUE(enabled) && isTRUE(interactive) && isTRUE(selected) && isTRUE(armed) &&
    nzchar(ts_drive_badge_chr(state)) && !identical(ts_drive_badge_chr(state), "off")
}


# =============================================================================
# Job lifecycle — what an agent polls while work is in flight (spec §5)
# =============================================================================
# WHY THIS EXISTS, and it was MEASURED rather than reasoned.
#
# `run_pipeline` used to answer `done` for every module, and `done` is TERMINAL
# (`ts_drive_status_terminal()`). For the DE module that answer is written
# BEFORE the work starts: this tick moves the button counter and returns, and
# Shiny runs the module's `observeEvent` on the next step of the SAME flush.
# MEASURED live 2026-09-22 (DESeq2, 17 925 genes x 18 samples): the token
# answered `done` in 2.1 s, and the contrast first became observable 852.7 s
# later. An agent that switches on the terminal status was therefore told, in
# 2.1 s, that a fourteen-minute job had FINISHED.
#
# `running` already existed in `TS_DRIVE_STATUSES`, in `TS_DRIVE_BADGE_STATES`
# and in the badge transition table — `ts_drive_badge_state_for("running")`
# maps it — but NOTHING ever produced it. The badge was built for this signal
# and the producer was missing; that is the whole of what "badge-only" meant.
#
# `running` is written to `result.json` at DISPATCH, so it travels the wire and
# is not merely a colour on screen. The terminal status is written when the
# MODULE DECLARES the job over — a measurement by the only side that knows,
# never an inference from a state fingerprint or a timer. Until a module
# declares itself long, `run_pipeline` keeps its documented meaning ("the token
# moved", spec §2.3) and answers `done` as before.
#
# RESIDUAL — MEASURED, AND DELIBERATELY NOT MASKED. The long computation is
# still SYNCHRONOUS, so it blocks the Shiny event loop for its whole duration.
# No tick runs meanwhile, so `ready.json`'s heartbeat stalls and
# `ts_drive_ready_fresh()` — the agent-side liveness gate, default 15 s — goes
# FALSE for the rest of the job. `result.json` carrying `running` is therefore
# ALSO the proof of life the heartbeat cannot give during that window: it is
# what separates "alive and busy" from "dead". Restoring the heartbeat needs
# the computation itself to stop blocking, i.e. to go through the existing
# asynchronous layer (`run_job()`, R/core/jobs.R) — a change to the MODULE, not
# to this protocol.

#' The in-flight job, or `NULL`.
#'
#' Process-wide, like `.ts_drive_state$write_error`, and for the same reason:
#' the protocol admits ONE driven session per process (`ready.json` names it),
#' so a session-scoped registry would only add a way for two tabs to disagree.
ts_drive_job_state <- function() {
  .ts_drive_state$job
}

#' Is a job in flight right now?
ts_drive_job_busy <- function() {
  !is.null(.ts_drive_state$job)
}

#' The terminal status a module declared and that is not yet on the wire.
ts_drive_job_pending <- function() {
  .ts_drive_state$job$pending
}

ts_drive_next_job_id <- function() {
  .ts_drive_state$job_serial <- .ts_drive_state$job_serial + 1L
  sprintf("job-%08d-%s", .ts_drive_state$job_serial, ts_drive_new_token())
}

ts_drive_job_result_status <- function(status) {
  if (status %in% c("timeout", "session_lost")) "error" else status
}

#' Seconds elapsed since a job started, or `NULL` when that is not knowable.
#'
#' @description
#' `ts_drive_job_view()` has always published `elapsed_s` — but only
#' `if (!is.null(pending))`, i.e. only once the job had a TERMINAL record. So the
#' field was `null` for exactly the window where an agent needs it.
#'
#' MEASURED live (2026-09-27): while a `long` job runs, the work blocks the
#' reactive loop, `ready.json` stops being rewritten, and `ready_fresh()` goes
#' FALSE on 5 consecutive samples (max age 19.5 s against a 15 s threshold) —
#' while `result.json` carries `running`. The written operational rule is then
#' "never conclude the session is dead from `ready_fresh() == FALSE` while a job
#' is in flight". With `elapsed_s` null the agent had NO way to tell "14 minutes
#' into a DESeq2 on 17 925 genes, normal" from "wedged": the rule forbade it to
#' conclude, and gave it nothing to conclude FROM.
#'
#' Three properties this function must hold, each a decision rather than a
#' convenience:
#'
#'   * `NULL` when `started` is unusable — never a fabricated `0`, which would
#'     read as "the job just began" and is the same NULL-is-not-FALSE property
#'     the descriptor and the packaging gate needed.
#'   * A CLOCK THAT WENT BACKWARDS yields `0`, never a negative. `Sys.time()` can
#'     step back (NTP, a timezone change), and a negative duration is not a fact
#'     about the job — it is an artefact of the observer, reported as if it were
#'     the former.
#'   * Rounded to a tenth of a second: a bounded scalar on the wire, not a
#'     precision that promises the microsecond.
#'
#' @param started Numeric epoch seconds, as stored by `ts_drive_job_begin()`.
#' @param now Injectable clock, so no test depends on the wall clock.
#' @return A single non-negative number, or `NULL`.
#' @noRd
ts_drive_job_elapsed <- function(started, now = Sys.time()) {
  # `started` is stored as a plain numeric by `ts_drive_job_begin()`, but a
  # POSIXct is a legitimate epoch too and refusing it would be a landmine: the
  # caller would get a bare NULL with no explanation. A CHARACTER epoch, on the
  # other hand, means a corrupted record, and is refused rather than coerced — an
  # `as.numeric()` that quietly accepted "1000" would turn corruption into a
  # plausible number.
  if (is.null(started) || length(started) != 1L || is.character(started)) {
    return(NULL)
  }
  s <- suppressWarnings(as.numeric(started))
  if (is.na(s)) return(NULL)
  # 🔴 `now` DEFAUT À `Sys.time()`, ET `is.numeric(Sys.time())` EST FAUX.
  # MESURÉ : `class(Sys.time())` = `POSIXct POSIXt` — un double CLASSÉ — et
  # `is.numeric()` renvoie FALSE pour tout objet classé. Un garde écrit
  # `!is.numeric(now)` rejetait donc l'horloge de production et renvoyait NULL à
  # CHAQUE appel, alors que le test unitaire — qui passait un double nu — restait
  # VERT. Le test et la production prenaient des CHEMINS DIFFÉRENTS, ce qui est
  # la seule façon dont cela passe inaperçu.
  # D'où la coercition AVANT la validation : c'est elle qui transforme
  # l'horloge légitime en nombre nu que le garde sait vérifier.
  n <- suppressWarnings(as.numeric(now))
  if (length(n) != 1L || is.na(n)) return(NULL)
  e <- n - s
  if (!is.finite(e)) return(NULL)
  if (e < 0) e <- 0
  round(e, 1L)
}

ts_drive_job_view <- function(job) {
  if (is.null(job)) return(NULL)
  pending <- job$pending
  # 🔴 LIVE, then terminal. `pending$elapsed_s` is the PRODUCER's value, measured
  # at closure, and stays authoritative — a consumer that has already read the
  # terminal number must not see it drift. The live branch fills the window
  # where there is no terminal value yet; it never overrides one.
  elapsed <- if (!is.null(pending)) {
    as.numeric(pending$elapsed_s)
  } else {
    ts_drive_job_elapsed(job$started)
  }
  list(
    job_id     = as.character(job$job_id),
    seq        = suppressWarnings(as.numeric(job$seq)),
    module     = as.character(job$module),
    action     = as.character(job$action),
    button     = as.character(job$button),
    status     = as.character(job$status),
    started_at = as.character(job$started_at),
    ended_at   = if (is.null(pending)) NULL else as.character(pending$ended_at),
    elapsed_s  = elapsed,
    timeout_s  = if (is.null(job$timeout_s)) NULL else as.numeric(job$timeout_s)
  )
}

ts_drive_job_owner_matches <- function(job, owner_token = NULL) {
  if (is.null(job$owner_token)) return(TRUE)
  candidate <- owner_token
  if (is.null(candidate)) candidate <- .ts_drive_state$selected_token
  if (is.null(candidate)) return(TRUE)
  identical(as.character(candidate), as.character(job$owner_token))
}

ts_drive_job_set_pending <- function(status, error = NULL) {
  job <- .ts_drive_state$job
  if (is.null(job) || !identical(job$status, "running") ||
      !is.null(job$pending)) {
    return(invisible(FALSE))
  }
  status <- as.character(status)
  valid <- c("done", "error", "invalid", "timeout", "session_lost")
  if (length(status) != 1L || is.na(status) || !status %in% valid) {
    return(invisible(FALSE))
  }
  if (is.null(error) && status %in% c("error", "timeout", "session_lost")) {
    error <- if (identical(status, "timeout")) {
      sprintf("the job exceeded its declared timeout of %s seconds",
              format(job$timeout_s %||% 0, trim = TRUE))
    } else if (identical(status, "session_lost")) {
      "the session that owned this job ended"
    } else {
      "the job failed"
    }
  }
  .ts_drive_state$job$status <- status
  .ts_drive_state$job$pending <- list(
    status      = status,
    wire_status = ts_drive_job_result_status(status),
    error       = if (identical(ts_drive_job_result_status(status), "error")) {
      as.character(error)
    } else {
      NULL
    },
    ended_at    = ts_drive_now_iso(),
    elapsed_s   = as.numeric(Sys.time()) - job$started
  )
  invisible(TRUE)
}

#' Record a job the tick has just DISPATCHED.
#'
#' @param seq Scenario sequence the job answers for.
#' @param module,action,button Where the job was fired from.
ts_drive_job_begin <- function(seq, module, action, button,
                               owner_token = NULL, timeout_s = NULL) {
  if (ts_drive_job_busy()) return(invisible(FALSE))
  timeout <- ts_drive_entry_timeout(list(timeout_s = timeout_s))
  owner <- owner_token %||% .ts_drive_state$selected_token
  .ts_drive_state$job <- list(
    job_id     = ts_drive_next_job_id(),
    seq        = suppressWarnings(as.numeric(seq)),
    module     = as.character(module),
    action     = as.character(action),
    button     = as.character(button),
    owner_token = if (is.null(owner)) NULL else as.character(owner),
    status     = "running",
    started_at = ts_drive_now_iso(),
    started    = as.numeric(Sys.time()),
    timeout_s  = timeout,
    pending    = NULL
  )
  invisible(.ts_drive_state$job)
}

#' A module DECLARES its job over.
#'
#' This is the terminal producer. It is called by the MODULE — the only side
#' that can know the work has stopped — and never by the watcher, which may not
#' run a pipeline and therefore may not guess that one finished.
#'
#' The declaration is only RECORDED here; the write happens on the next tick,
#' so `result.json` keeps exactly one writer. A declaration whose `button` is
#' not the one in flight is REFUSED rather than applied: two tabs, or a stale
#' observer, must not be able to close someone else's job.
#'
#' @param button The bound button id the job was dispatched from.
#' @return `TRUE` when the declaration was accepted.
ts_drive_job_finish <- function(button, status = "done", error = NULL,
                                job_id = NULL, owner_token = NULL) {
  job <- .ts_drive_state$job
  if (is.null(job) || !identical(job$status, "running") ||
      !is.null(job$pending)) {
    return(invisible(FALSE))
  }
  if (!identical(as.character(button), job$button)) return(invisible(FALSE))
  if (!is.null(job_id) && !identical(as.character(job_id), job$job_id)) {
    return(invisible(FALSE))
  }
  if (!ts_drive_job_owner_matches(job, owner_token)) return(invisible(FALSE))
  ts_drive_job_set_pending(status, error)
}

ts_drive_job_expire <- function(now = Sys.time()) {
  job <- .ts_drive_state$job
  if (is.null(job) || !identical(job$status, "running") ||
      is.null(job$timeout_s)) {
    return(invisible(FALSE))
  }
  if ((as.numeric(now) - job$started) < job$timeout_s) {
    return(invisible(FALSE))
  }
  ts_drive_job_set_pending("timeout")
}

ts_drive_job_session_lost <- function(owner_token = NULL) {
  job <- .ts_drive_state$job
  if (is.null(job) || !identical(job$status, "running")) {
    return(invisible(FALSE))
  }
  if (!is.null(owner_token) && !is.null(job$owner_token) &&
      !identical(as.character(owner_token), as.character(job$owner_token))) {
    return(invisible(FALSE))
  }
  ts_drive_job_set_pending("session_lost")
}

#' Forget the in-flight job. Called once its terminal status is ON THE WIRE.
ts_drive_job_clear <- function() {
  .ts_drive_state$job <- NULL
  invisible(NULL)
}


# =============================================================================
# Attach — the ONLY entry point app.R calls
# =============================================================================

#' Attach the drive poller to a live session.
#'
#' Prepares everything needed to drive a live session — writes the handshake,
#' derives the session token, and returns an `on_tick()` closure plus the
#' navigation performer. **No reactivity is created here**: `R/` may not carry
#' `observe()`/`reactiveVal()` (C2), so the single
#' `observe({ ... invalidateLater() ... })` lives in `app.R`'s `server()` and
#' calls `on_tick()` on each beat. `on_tick()` is a plain function doing the
#' whole protocol decision, which is where the logic belongs and where it stays
#' testable without a browser.
#'
#' Cheap and always safe: with `arm.json` absent, `on_tick()` returns after two
#' `file.info()` probes.
#'
#' @param session, input The Shiny server arguments.
#' @param poll_ms Idle poll period; 800 ms is inside the spec's 500-1000 ms.
#' @return list(token, poll_ms, on_tick, last_seq, armed, pending_nav, nav).
#'   `on_tick(global_data, effects)` runs one protocol beat and returns the tick
#'   invisibly; `pending_nav()` returns the navigation plan of the last applied
#'   scenario **and clears it**, so the caller performs each plan exactly once.
ts_drive_attach <- function(session, input, poll_ms = 800) {
  dir.create(ts_drive_path(), showWarnings = FALSE, recursive = TRUE)

  token <- ts_drive_new_token()
  prior_job <- ts_drive_job_state()
  if (!is.null(prior_job$owner_token) &&
      !identical(as.character(prior_job$owner_token), token)) {
    ts_drive_job_session_lost(prior_job$owner_token)
  }
  .ts_drive_state$selected_token <- token

  # `started_at` is fixed for the whole session and `hb_n` starts at 0; both are
  # handed to every later rewrite so neither can drift.
  started_at <- ts_drive_now_iso()
  # 🔴 THE FLOOR IS RESUMED, NOT INVENTED. `ts_drive_read_result()` was documented
  # as "used to resume `last_seq` after a reload" and had NO call site, so every
  # session started at 0 — and with a floor of 0 the `seq <= last_seq` guard is
  # FALSE for the PREVIOUS session's `scenario.json`, which is still on disk: the
  # first tick APPLIED a dead session's scenario. That behaviour was even frozen by
  # a test (`:3452`, where `d2` consumes the `seq = 72` scenario `d1` wrote).
  # MEASURED 2026-09-29. The agent-facing half matters as much as the safety half:
  # the floor is PUBLISHED in `ready.json`, so a first `seq` is read from the file
  # instead of guessed — and `0` stays exactly what it should mean, an EMPTY
  # channel, which is what the non-vacuity assertion pins.
  resumed_ack <- suppressWarnings(as.integer(ts_drive_read_result()$ack_seq %||% 0L))
  if (length(resumed_ack) != 1L || is.na(resumed_ack) || resumed_ack < 0L) {
    resumed_ack <- 0L
  }
  boot_ready <- ts_drive_write_ready(session, token, armed = FALSE,
                                     started_at = started_at, hb_n = 0L,
                                     last_seq = resumed_ack)
  # The FIRST write of a session is the one the agent needs to find the token,
  # so its failure is reported rather than assumed away. It used to be a bare
  # call whose result was discarded: a session whose handshake never landed
  # looked exactly like a session nobody had armed yet.
  if (!isTRUE(attr(boot_ready, "written"))) {
    boot_err <- ts_drive_last_write_error()
    message(sprintf("[drive] ready.json write FAILED at attach: %s",
                    if (is.null(boot_err)) "unknown" else boot_err$message))
  }

  # One console line (not stop(), not a modal) so a human/agent log can copy
  # the token straight out of the R console.
  message(sprintf("[TranscriptoShiny drive] session_token=%s root=%s protocol=%s",
                  token, ts_drive_root(), TS_DRIVE_PROTOCOL))

  session$onSessionEnded(function() {
    lost <- ts_drive_job_session_lost(token)
    if (identical(.ts_drive_state$selected_token, token)) {
      .ts_drive_state$selected_token <- NULL
    }
    if (isTRUE(lost) || !is.null(ts_drive_job_pending())) {
      try(ts_drive_tick(
        session, input, NULL, token,
        last_seq = cursor$last_seq,
        armed = cursor$armed,
        effects = NULL
      ), silent = TRUE)
    }
    try(ts_drive_invalidate_ready(token), silent = TRUE)
  })

  # Mutable cursor shared by the closure. Kept in the closure's environment,
  # not in a `reactiveVal`, precisely so `R/` stays free of reactivity.
    cursor <- new.env(parent = emptyenv())
    cursor$last_seq <- resumed_ack
    cursor$armed    <- FALSE
    cursor$nav      <- NULL
    # S6: a run waiting for its injected inputs to be confirmed by the owning
    # module. Lives in the closure's environment beside `last_seq`, so the tick
    # stays a pure function of its arguments and the state is per-session by
    # construction.
    cursor$pending  <- NULL
  # Heartbeat state. `hb_n` is monotonic for the session's life and `started_at`
  # is written once, so an agent can tell "same live session" from "a new
  # session reusing the pid" — see ts_drive_write_ready().
  cursor$hb_n       <- 0L
  cursor$started_at <- ts_drive_now_iso()
  cursor$hb_at      <- as.numeric(Sys.time())
  # Badge events pending delivery to app.R's reactive state.
  cursor$events     <- list()

  # TWO captures matter, and they are captured for opposite reasons:
  #
  #  1. `input`, in `local()` below. Shiny builds each session's `input` as a
  #     promise-like object; reading it OUTSIDE a reactive context is what
  #     triggers "Can't access reactive value 'x' outside of reactive consumer".
  #     Binding it to the local `input` argument at attach time makes every
  #     later read by `ts_drive_tick()` plain promise-forcing inside the
  #     observer — no capture escapes to session end.
  #
  #  2. `global_data`, NEVER captured. app.R may pass it as a
  #     `reactiveValues()` (whose `$` read is reactive) or as a plain `list()`
  #     (grade G0's inert probe). Capturing it here would freeze the object and
  #     silently break the `snapshot` of a partly-loaded matrix.
  local({
    tick_fn <- function(global_data = NULL, effects = NULL) {
      was_armed <- cursor$armed
      tick <- ts_drive_tick(session, input, global_data, token,
                              last_seq = cursor$last_seq, armed = cursor$armed,
                              effects = effects, pending = cursor$pending)
        if (isFALSE(tick$selected)) return(tick)
      if (!is.null(tick$error)) {
        message(sprintf("[drive] tick error: %s", tick$error))
      }
        if (isTRUE(tick$consumed) && !is.na(tick$last_seq)) {
          cursor$last_seq <- tick$last_seq
        }
        # A pending confirmation is carried beat to beat. This assignment is
        # UNCONDITIONAL, and getting that wrong is the second half of a defect
        # MEASURED live (2026-09-27): the first version read
        #   if (had_pending) cursor$pending <- tick$pending
        # which only PROPAGATES a record that already existed, and cannot CAPTURE
        # one the tick just created. So the record `ts_drive_apply()` deferred was
        # dropped on the same beat it was created: the next beat was handed
        # `pending = NULL`, the confirmation could neither complete nor hit its
        # 5-beat timeout, and the run hung at `running` indefinitely - with a
        # healthy heartbeat (`hb_n` climbing) and no error anywhere.
        #
        # `tick$pending` is the authoritative post-beat state, so one assignment
        # covers all three cases: still waiting (non-NULL, carried), resolved
        # (NULL, cleared), none (NULL).
        cursor$pending <- tick$pending
      cursor$armed <- isTRUE(tick$armed)
      if (isTRUE(tick$consumed)) {
        # A navigation plan is a ONE-SHOT effect: it is stored for the caller
        # (which owns `session`, hence bslib) and cleared on read, so a later
        # idle tick cannot replay a jump the human has since undone.
        cursor$nav <- tick$nav
      }

      # ── Badge events (only REAL transitions) ─────────────────────────────
      # The badge must not move on an idle tick. Two sources qualify:
      #   1. an arm/disarm transition of the gate itself;
      #   2. a scenario that was actually consumed (accepted → started →
      #      completed/error).
      # They are QUEUED, not applied, because the badge lives in a
      # `reactiveVal()` that only app.R (which owns session, hence reactivity)
      # may touch — R/ stays reactive-free (C2). Same one-shot discipline as
      # `pending_nav()`: read once, then cleared, so a late tick cannot double
      # count a sequence.
      now_num <- as.numeric(Sys.time())
      transition <- !identical(was_armed, cursor$armed)
      if (transition) {
        cursor$events[[length(cursor$events) + 1L]] <-
          list(event = if (cursor$armed) "arm" else "disarm")
      }
      if (isTRUE(tick$consumed)) {
        seq_now <- if (!is.na(tick$last_seq)) tick$last_seq else cursor$last_seq
        st <- as.character(tick$status %||% "applied")
        if (isTRUE(tick$new_scenario)) {
          cursor$events[[length(cursor$events) + 1L]] <-
            list(event = "accepted", ack_seq = seq_now,
                 module = tick$module %||% "", action = tick$action %||% "",
                 status = st)
        }
        if (identical(st, "error")) {
          cursor$events[[length(cursor$events) + 1L]] <-
            list(event = "error", ack_seq = seq_now, error = tick$error %||% "")
        } else {
          cursor$events[[length(cursor$events) + 1L]] <-
            list(event = "completed", ack_seq = seq_now, status = st,
                 module = tick$module %||% "", action = tick$action %||% "",
                 elapsed_s = tick$elapsed_s %||% NA_real_)
        }
      }

      # ── Heartbeat (spec: low-frequency, ~2-5 s, while ARMED) ─────────────
      # Written by THIS session (never the agent), through the same atomic
      # helper as every other wire write. Two separate concerns:
      #
      #   * an ARM/DISARM transition must be visible IMMEDIATELY, so it always
      #     forces a write regardless of the cadence;
      #   * otherwise only the counter moves, and it is throttled so the poller
      #     does not rewrite ready.json every 800 ms.
      #
      # On disarm the file is NOT deleted — `armed: false` plus a `hb_n` that
      # has stopped moving is the honest signal ("this session still exists, but
      # is no longer driven"), and deleting it would also erase the token the
      # agent needs to re-arm the SAME session without a restart.
      due <- (now_num - cursor$hb_at) >= ts_drive_hb_interval()
      if (isTRUE(tick$consumed) || transition || (cursor$armed && due)) {
        if (cursor$armed) cursor$hb_n <- cursor$hb_n + 1L
        wrote <- try(ts_drive_write_ready(session, token, armed = cursor$armed,
                                          last_seq = cursor$last_seq,
                                          hb_n = cursor$hb_n,
                                          started_at = started_at),
                     silent = TRUE)
        # A write counts as DONE only when the destination verifiably holds it.
        #
        # The previous guard was `!inherits(wrote, "try-error")`, and
        # `ts_drive_write_ready()` RETURNS its payload instead of throwing — so
        # `try()` never produced a `try-error` and the throttle advanced even
        # when nothing had been written. LATENT, not the cause of the first-arm
        # symptom (that was the app's slow boot — see ts_drive_hb_next_at()),
        # but a failed write would have silenced the heartbeat for a whole
        # interval and left a stale file behind.
        #
        # Not advancing `hb_at` on failure is the whole fix at this site: the
        # next beat is 800 ms away, so the handshake repairs itself instead of
        # waiting out the interval — and the failure is reported, never
        # swallowed.
        wrote_ok <- !inherits(wrote, "try-error") && isTRUE(attr(wrote, "written"))
        # The decision itself lives in a pure function so it can be tested
        # without racing the clock; see ts_drive_hb_next_at() for the measured
        # reason a failed write must NOT advance the throttle.
        cursor$hb_at <- ts_drive_hb_next_at(now_num, cursor$hb_at, wrote_ok)
        if (!isTRUE(wrote_ok)) {
          # Roll the counter back: `hb_n` mirrors what is ON DISK, so a beat
          # that never landed must not consume a number. This keeps the
          # sequence the agent reads gap-free (0,1,2,...) instead of skipping.
          if (cursor$armed && cursor$hb_n > 0L) cursor$hb_n <- cursor$hb_n - 1L
          hb_err <- ts_drive_last_write_error()
          message(sprintf("[drive] ready.json write FAILED (retrying next beat): %s",
                          if (is.null(hb_err)) "unknown" else hb_err$message))
        }
        if (transition) {
          message(sprintf("[drive] %s", if (cursor$armed) "armed" else "disarmed"))
        }
      }
      tick
    }

    list(
      token = token,
      poll_ms = poll_ms,

      #' Run exactly one protocol beat. Called from the `observe()` in app.R.
      #' @param global_data reactiveValues (or list) read by the snapshot.
      #' @param effects module-side token callback (see ts_drive_tick).
      #' @return the tick, INCLUDING `$nav` which the caller must perform.
      on_tick = tick_fn,

      #' Navigation plan of the last applied scenario, then `NULL`.
      pending_nav = function() {
        plan <- cursor$nav
        cursor$nav <- NULL
        plan
      },

      #' Badge events since the last read, then empty. One-shot like `nav`.
      pending_events = function() {
        ev <- cursor$events
        cursor$events <- list()
        ev
      },

      #' Last seq applied, for diagnostics.
      last_seq = function() cursor$last_seq,
      armed    = function() cursor$armed
    )
  })
}

#' Perform a navigation plan produced by `ts_drive_nav_plan()`.
#'
#' Kept in `R/` because it is a plain function; the caller (app.R's `server()`)
#' owns the `session`. `bslib::nav_select()` is used because the human must SEE
#' the target tab in the Viewer or the Chrome tab — a state change nobody
#' watches is not a demo (spec §2.3).
#'
#' @param session The Shiny session.
#' @param plan list(top, tab, panel) as returned by `ts_drive_nav_plan()`.
#' @return TRUE when at least the top-level jump succeeded.
#'
#' A plan whose `top` is NULL is a DOCUMENTED "do not navigate" outcome, not a
#' missing branch: `ts_drive_nav_plan()` returns one for a module that has no
#' stable navbar value (see the `import_spatial` branch). It is answered here
#' explicitly, so the empty plan is a decision and not an error swallowed by the
#' `tryCatch` below.
ts_drive_perform_nav <- function(session, plan) {
  if (is.null(plan)) return(invisible(FALSE))
  if (is.null(plan$top)) return(invisible(FALSE))
  ok <- tryCatch({
    bslib::nav_select(id = TS_DRIVE_TOP_NAV_ID, selected = plan$top, session = session)
    TRUE
  }, error = function(e) FALSE)

  if (!is.null(plan$tab)) {
    ok <- tryCatch({
      bslib::nav_select(id = plan$tab_id %||% TS_DRIVE_BULK_TABS_ID,
                        selected = plan$tab, session = session)
      TRUE
    }, error = function(e) ok)
  }
  if (!is.null(plan$panel)) {
    # `accordion_id` may be a VECTOR: the SC panel lives in a doubly nested
    # accordion, and opening only the outer one leaves the panel folded away.
    # Each id is opened independently and every failure is swallowed — the
    # navigation is there so a HUMAN can watch the run (spec 2.3), and a drive
    # click is dispatched through a counter, not through the DOM, so a cosmetic
    # failure here can never turn a completed job into a failed one.
    for (aid in plan$accordion_id %||% TS_DRIVE_BULK_ACCORDION_ID) {
      tryCatch(
        bslib::accordion_panel_open(aid, values = plan$panel, session = session),
        error = function(e) NULL
      )
    }
  }
  # S1: the OPTIONAL fourth level, a navset nested inside the module's own results
  # panel. It is performed LAST and only when the plan carries it, for two
  # reasons that are both load-bearing: the nested navset is not in the DOM until
  # the level above has selected its parent, and `bslib::nav_select()` on an id
  # that is not present is a silent no-op that would otherwise be invisible.
  # Same API and the same swallow-everything contract as the level above: the
  # navigation exists so a human can watch, and it can never turn a completed job
  # into a failed one.
  if (!is.null(plan$sub_tab)) {
    tryCatch(
      bslib::nav_select(id = plan$sub_tab_id, selected = plan$sub_tab, session = session),
      error = function(e) NULL
    )
  }
  invisible(ok)
}

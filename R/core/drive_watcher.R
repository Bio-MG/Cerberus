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
    error = function(e) NULL
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
                 error = function(e) NA)
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
TS_DRIVE_ACTIONS <- c("noop", "set_inputs", "run_pipeline", "import_file",
                      "snapshot", "reset_module")

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

  declared_token <- as.character(scn$session_token %||% "")
  if (nzchar(declared_token) && !identical(declared_token, token)) {
    errors <- c(errors, sprintf("session_token mismatch ('%s' != '%s')", declared_token, token))
  }

  action <- as.character(scn$action %||% "")
  if (!action %in% TS_DRIVE_ACTIONS) {
    errors <- c(errors, sprintf("unknown action '%s' (allowed: %s)",
                                action, paste(TS_DRIVE_ACTIONS, collapse = ", ")))
  }

  module <- as.character(scn$module %||% "")
  if (!nzchar(module)) {
    errors <- c(errors, "missing `module`")
  } else if (!module %in% TS_DRIVE_MODULES) {
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
    iv <- ts_drive_validate_import(scn$import)
    errors <- c(errors, iv$errors)
    if (length(iv$import)) imp <- iv$import
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
ts_drive_write_result <- function(seq, status, active_module, armed,
                                 preserve_data = TRUE, errors = character(0),
                                 warnings = character(0), snapshot = NULL) {
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

  obj <- tryCatch(global_data$bulk_obj, error = function(e) NULL)
  if (is.null(obj)) return(out)

  counts <- tryCatch(obj$counts, error = function(e) NULL)
  out$has_data     <- TRUE
  out$object_class <- class(obj)[1]
  out$n_genes      <- tryCatch(nrow(counts), error = function(e) NULL)
  out$n_samples    <- tryCatch(ncol(counts), error = function(e) NULL)
  out$error_state  <- tryCatch(
    ts_error_state(NULL, class = NULL), error = function(e) NULL)
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
    mod <- ts_drive_module_of(id)
    if (is.na(mod)) next
    ans <- tryCatch(probe(), error = function(e) e)
    out[[mod]] <- if (inherits(ans, "condition")) {
      list(probe_error = ts_drive_badge_sanitize(
        sprintf("the state probe raised: %s", conditionMessage(ans)), 200L
      ))
    } else if (is.list(ans)) {
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
  reg <- tryCatch(shiny::isolate(global_data$drive_registry),
                  error = function(e) NULL)
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
                                    state = NULL, long = FALSE, timeout_s = NULL) {
  if (!input_id %in% TS_DRIVE_BUTTONS) {
    warning(sprintf("ts_drive_publish_token(): '%s' is not in TS_DRIVE_BUTTONS — ignored.", input_id))
    return(invisible(FALSE))
  }
  reg <- ts_drive_registry(global_data)
  if (is.null(reg)) return(invisible(FALSE))
  reg[[input_id]] <- list(counter = counter, ready = ready, state = state,
                          long = isTRUE(long),
                          timeout_s = ts_drive_entry_timeout(list(timeout_s = timeout_s)))
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
ts_drive_apply <- function(session, input, scn, effects = NULL, owner_token = NULL) {
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

  if (identical(module, "spatial_pipeline") && identical(action, "set_inputs")) {
    return(list(
      status = "invalid",
      errors = "module 'spatial_pipeline' does not accept set_inputs",
      warnings = warnings, active_module = module, nav = NULL
    ))
  }

  # Same rule, same reason, for the SC auto-pipeline, SC annotation, SC marker
  # and Bulk signature actions: their inputs are FROZEN, DECLARED sets, and an
  # injected input could not be honoured without silently changing the action
  # boundary. For the signature action the refusal also keeps a fileInput PATH
  # (`sig_rds`) out of reach of a remote caller.
  if (identical(action, "set_inputs") &&
      module %in% c(TS_DRIVE_SC_MODULE, TS_DRIVE_SC_ANNOTATION_MODULE,
                    TS_DRIVE_SC_MARKERS_MODULE,
                    TS_DRIVE_BULK_SIGNATURES_MODULE)) {
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
  if (length(scn$inputs)) {
    applied <- ts_drive_apply_inputs(session, scn$inputs, module,
                                    tokens = ts_drive_tokens_for(module, effects))
    warnings <- c(warnings, applied$warnings)
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
      spatial_pipeline = "spatial-pipeline-btn_run_all",
      sc_pipeline   = TS_DRIVE_SC_BUTTON,
      sc_annotation = TS_DRIVE_SC_ANNOTATION_BUTTON,
      sc_markers    = TS_DRIVE_SC_MARKERS_BUTTON,
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
    if (is.null(req) || is.null(req$counts_path)) {
      errors <- c(errors, "`import_file` needs an `import` block carrying `counts_path`")
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
  out <- tryCatch(effects(NULL, mode = "tokens", module = module), error = function(e) list())
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
  if (identical(module, TS_DRIVE_SC_MARKERS_MODULE)) {
    return(list(
      top = TS_DRIVE_SC_MARKERS_TOP_TAB,
      tab = NULL,
      panel = TS_DRIVE_SC_MARKERS_PANELS,
      tab_id = NULL,
      accordion_id = TS_DRIVE_SC_MARKERS_ACCORDION_IDS
    ))
  }
  if (identical(module, TS_DRIVE_SC_ANNOTATION_MODULE)) {
    return(list(
      top = TS_DRIVE_SC_ANNOTATION_TOP_TAB,
      tab = NULL,
      panel = TS_DRIVE_SC_ANNOTATION_PANELS,
      tab_id = NULL,
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
                          armed = FALSE, effects = NULL) {
  out <- list(consumed = FALSE, new_scenario = FALSE, published = FALSE,
              deferred = FALSE, selected = TRUE, last_seq = last_seq,
              armed = armed, error = NULL, nav = NULL, status = NULL,
              job_status = NULL, module = NULL, action = NULL, elapsed_s = NULL)
  selected <- .ts_drive_state$selected_token
  if (!is.null(selected) &&
      !identical(as.character(selected), as.character(token))) {
    out$selected <- FALSE
    out$armed <- FALSE
    return(out)
  }
  out$armed <- tryCatch(ts_drive_arm_state(token)$armed, error = function(e) FALSE)
  ts_drive_job_expire()

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
                        owner_token = token)

  message(sprintf("[drive] seq=%s module=%s action=%s status=%s preserve_data=%s",
                  scn$seq, scn$module, scn$action, res$status, preserve))

  ts_drive_write_result(
    scn$seq, res$status, res$active_module %||% scn$module, out$armed,
    preserve_data = preserve,
    errors   = c(v$errors, res$errors),
    warnings = c(v$warnings, res$warnings),
    snapshot = ts_drive_snapshot(global_data)
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

ts_drive_job_view <- function(job) {
  if (is.null(job)) return(NULL)
  pending <- job$pending
  list(
    job_id     = as.character(job$job_id),
    seq        = suppressWarnings(as.numeric(job$seq)),
    module     = as.character(job$module),
    action     = as.character(job$action),
    button     = as.character(job$button),
    status     = as.character(job$status),
    started_at = as.character(job$started_at),
    ended_at   = if (is.null(pending)) NULL else as.character(pending$ended_at),
    elapsed_s  = if (is.null(pending)) NULL else as.numeric(pending$elapsed_s),
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
  boot_ready <- ts_drive_write_ready(session, token, armed = FALSE,
                                     started_at = started_at, hb_n = 0L)
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
  cursor$last_seq <- 0L
  cursor$armed    <- FALSE
  cursor$nav      <- NULL
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
                            effects = effects)
      if (isFALSE(tick$selected)) return(tick)
      if (!is.null(tick$error)) {
        message(sprintf("[drive] tick error: %s", tick$error))
      }
      if (isTRUE(tick$consumed) && !is.na(tick$last_seq)) {
        cursor$last_seq <- tick$last_seq
      }
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
ts_drive_perform_nav <- function(session, plan) {
  if (is.null(plan)) return(invisible(FALSE))
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
  invisible(ok)
}

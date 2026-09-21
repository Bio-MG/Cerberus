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

#' Write one JSON file atomically (spec §2.3, S8).
#'
#' Full payload -> `<dest>.tmp` -> unlink dest -> `file.rename`.
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
#' @return TRUE when the destination holds the new payload.
ts_drive_write_json <- function(obj, path) {
  ts_drive_ensure_dir()
  tmp <- paste0(path, ".tmp")
  txt <- tryCatch(
    jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null", pretty = TRUE),
    error = function(e) NULL
  )
  if (is.null(txt)) {
    try(unlink(tmp), silent = TRUE)
    return(FALSE)
  }

  ok <- tryCatch({
    con <- file(tmp, open = "wb")
    # Close BEFORE returning, so the handle is gone when rename() runs below.
    writeLines(txt, con = con, useBytes = TRUE)
    flush(con)
    close(con)
    TRUE
  }, error = function(e) {
    try(close(con), silent = TRUE)
    FALSE
  })
  if (!isTRUE(ok)) {
    try(unlink(tmp), silent = TRUE)
    return(FALSE)
  }

  if (file.exists(path)) try(unlink(path), silent = TRUE)
  if (isTRUE(file.rename(tmp, path))) return(TRUE)

  # Last resort: some antivirus / indexer can still hold the freshly written
  # tmp for a few milliseconds. Retry briefly rather than losing the handshake.
  for (i in seq_len(5L)) {
    Sys.sleep(0.05)
    if (file.exists(path)) try(unlink(path), silent = TRUE)
    if (isTRUE(file.rename(tmp, path))) return(TRUE)
  }
  try(unlink(tmp), silent = TRUE)
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
    viewer        = "unknown",
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
      "module '%s' outside the v1 bulk pilot allowlist (allowed: %s)",
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

  status <- if (length(errors)) "invalid" else "applied-candidate"
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
         inputs_ok = as.list(inputs[setdiff(names(inputs), c(bad_keys, sub(" .*$", "", skipped)))])
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
    snapshot      = snapshot
  )
  ts_drive_write_json(payload, ts_drive_path("result.json"))
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
              n_samples = NULL, error_state = NULL)
  if (is.null(global_data)) return(out)

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

#' Publish one button's counter into the session-scoped registry.
#'
#' Called from a module (which owns the `reactiveVal`) so the poller can
#' increment it later. The registry lives on `global_data`, i.e. it is
#' per-SESSION — never a global environment, which would let two tabs share a
#' counter (spec §2.1: a scenario may pin one session).
#'
#' Silently does nothing when no registry is present (e.g. a unit test that
#' sources a module alone), so the module keeps working outside the app.
#'
#' @param global_data The app-wide `reactiveValues`.
#' @param input_id Namespaced button id (must be in TS_DRIVE_BUTTONS).
#' @param counter The module's `reactiveVal`.
ts_drive_publish_token <- function(global_data, input_id, counter) {
  if (!input_id %in% TS_DRIVE_BUTTONS) {
    warning(sprintf("ts_drive_publish_token(): '%s' is not in TS_DRIVE_BUTTONS — ignored.", input_id))
    return(invisible(FALSE))
  }
  reg <- tryCatch(global_data$drive_registry, error = function(e) NULL)
  if (is.null(reg) || !is.environment(reg)) return(invisible(FALSE))
  reg[[input_id]] <- counter
  invisible(TRUE)
}

#' Read one published counter (used by the poller through the effects callback).
ts_drive_token_of <- function(global_data, input_id) {
  reg <- tryCatch(global_data$drive_registry, error = function(e) NULL)
  if (is.null(reg) || !is.environment(reg)) return(NULL)
  reg[[input_id]]
}

#' Increment one bound button token (used by `run_pipeline`).
#'
#' The counter only ever increases: an `actionButton` fires on a CHANGE, and
#' a monotonically growing counter is what makes two consecutive
#' `run_pipeline` scenarios both fire (G2 acceptance 9).
#'
#' @param rv The `reactiveVal` handed back by `ts_drive_bind_button()`. It is
#'   passed explicitly (never looked up) because a `reactiveVal` is only
#'   readable inside an active reactive context: the value is both read and
#'   written by the caller in `modules/`, where reactivity belongs.
#' @return TRUE when a binding existed and was incremented.
ts_drive_bump_token <- function(rv) {
  if (is.null(rv)) return(FALSE)
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
#' @param tokens Named list id -> `reactiveVal` announced by the module for
#'   THIS scenario's module (read through `ts_drive_read_tokens()`).
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
      # Buttons are fired, never set: an actionButton is a counter.
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
ts_drive_apply <- function(session, input, scn, effects = NULL) {
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
      bulk_de       = "bulk-de-run_de",
      bulk_pathways = "bulk-pathways-run_pathway",
      NULL)
    if (is.null(btn) || !btn %in% TS_DRIVE_BUTTONS) {
      errors <- c(errors, sprintf("module '%s' has no bound button for run_pipeline", module))
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
    # The real observeEvent now runs. It is synchronous for the bulk pipeline
    # (import -> DE), so by the time the tick returns the work is done and
    # `done` is the honest status; a job that finishes later is read through
    # the NEXT snapshot (spec §2.4: `running` is for mirai / long observers).
    return(list(status = "done", errors = character(0), warnings = warnings,
                active_module = module, nav = nav))
  }

  if (identical(action, "import_file")) {
    errors <- c(errors, "import_file is not implemented in this grade (G3)")
    return(list(status = "invalid", errors = errors, warnings = warnings,
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
  panel <- switch(module,
    bulk_de       = "panel_de",
    bulk_pathways = "panel_pathways",
    import_bulk   = NULL,
    NULL)
  list(
    top   = "tab_bulk",
    tab   = if (!is.null(target_tab) && target_tab %in% TS_DRIVE_BULK_TABS) target_tab else NULL,
    panel = panel
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
  out <- list(consumed = FALSE, last_seq = last_seq, armed = armed,
              error = NULL, nav = NULL, status = NULL, module = NULL,
              action = NULL, elapsed_s = NULL)
  out$armed <- tryCatch(ts_drive_arm_state(token)$armed, error = function(e) FALSE)

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

  v <- ts_drive_validate_scenario(scn_raw, token, last_seq)

  if (identical(v$status, "invalid")) {
    ts_drive_write_result(if (is.null(v$scenario)) last_seq else v$scenario$seq,
                          "invalid", NA_character_, out$armed,
                          errors = v$errors, warnings = v$warnings,
                          snapshot = ts_drive_snapshot(global_data))
    out$consumed <- TRUE
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
  res <- ts_drive_apply(session, input, scn, effects = effects)

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
  out$last_seq <- scn$seq
  out$nav      <- res$nav
  # Badge-facing fields. They describe what HAPPENED, and are the only things
  # app.R may put on screen (module/action are short, static-ish labels).
  out$status    <- res$status
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

  # `started_at` is fixed for the whole session and `hb_n` starts at 0; both are
  # handed to every later rewrite so neither can drift.
  started_at <- ts_drive_now_iso()
  ts_drive_write_ready(session, token, armed = FALSE, started_at = started_at,
                       hb_n = 0L)

  # One console line (not stop(), not a modal) so a human/agent log can copy
  # the token straight out of the R console.
  message(sprintf("[TranscriptoShiny drive] session_token=%s root=%s protocol=%s",
                  token, ts_drive_root(), TS_DRIVE_PROTOCOL))

  session$onSessionEnded(function() {
    # A dead session must not leave a handshake the agent would trust.
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
      if (!is.null(tick$error)) {
        message(sprintf("[drive] tick error: %s", tick$error))
        return(tick)
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
        cursor$events[[length(cursor$events) + 1L]] <-
          list(event = "accepted", ack_seq = seq_now,
               module = tick$module %||% "", action = tick$action %||% "",
               status = st)
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
        if (!inherits(wrote, "try-error")) cursor$hb_at <- now_num
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
      bslib::nav_select(id = TS_DRIVE_BULK_TABS_ID, selected = plan$tab, session = session)
      TRUE
    }, error = function(e) ok)
  }
  if (!is.null(plan$panel)) {
    tryCatch(
      bslib::accordion_panel_open(TS_DRIVE_BULK_ACCORDION_ID, values = plan$panel,
                                  session = session),
      error = function(e) NULL
    )
  }
  invisible(ok)
}

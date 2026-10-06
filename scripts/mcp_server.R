# =============================================================================
# scripts/mcp_server.R — TranscriptoShiny MCP server (stdio, NATIVE JSON-RPC)
# =============================================================================
# M1 — READ-ONLY drive tools. Native handler: NO mcptools, NO btw, NO ellmer.
# Only jsonlite (already in renv.lock) is used, and only for the protocol layer.
#
# Usage (from anywhere — `--no-init-file` is REQUIRED, see "stdout purity"):
#   Rscript --no-init-file scripts/mcp_server.R          # serve (blocks, stdio)
#   Rscript --no-init-file scripts/mcp_server.R --check  # diagnose, then exit
#
# WHY --no-init-file (MEASURED, not stylistic)
#   `.Rprofile` sources `renv/activate.R`, whose synchronized-check prints two
#   lines to STDOUT — i.e. straight into the MCP transport, before the first
#   frame. That happens during R startup, so it CANNOT be suppressed from
#   inside this script. Skipping `.Rprofile` (while keeping `.Renviron`) is the
#   only in-band fix; the launcher then puts the project library on
#   `.libPaths()` itself. Measured: `RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE` also
#   works, but it must be set in the client's environment, not here.
#
# WHAT IT DOES
#   Reads the live drive session's own files (`tools/_drive/ready.json`,
#   `tools/_drive/result.json`) and projects them through a whitelist. It never
#   talks to R, never touches the app, and NEVER WRITES — no `arm.json`, no
#   `scenario.json`, no `unlink()`. The drive protocol is reused, not extended.
#
# FRAMING — MCP-conformant NEWLINE-DELIMITED JSON (transport correction)
#   The MCP stdio transport specifies: "Messages are delimited by newlines, and
#   MUST NOT contain embedded newlines." One JSON-RPC message per line, and
#   nothing else on stdout.
#   This REPLACES the earlier LSP-style `Content-Length: N\r\n\r\n` framing,
#   which the reference client cannot read: @modelcontextprotocol/sdk splits the
#   stream on LF, so the literal header line was handed to JSON.parse and threw.
#   MEASURED: with Content-Length framing the SDK's `initialize` TIMED OUT; with
#   newline-delimited JSON it completes.
#   - Input: read bytes until LF, drop one trailing CR (CRLF clients), parse the
#     line. Blank lines are ignored; a malformed line is answered with -32700 and
#     the loop CONTINUES (recovery, not shutdown).
#   - Output: `cat()` the single-line body plus ONE newline, then `flush()`.
#     `cat()` is deliberate — `writeBin()`/`writeChar()` write NOTHING to stdout
#     on this host. Windows text mode turns that newline into CRLF; a trailing CR
#     is stripped by conformant NDJSON readers (the SDK does so explicitly).
#   - Bodies are single-line (pretty = FALSE); jsonlite escapes CR/LF inside
#     strings, and a `fixed` substitution guarantees no literal newline escapes.
# =============================================================================

# --- 0. Bootstrap: project root, library paths, jsonlite ---------------------

.ts_stderr <- function(...) cat(..., "\n", sep = "", file = stderr())

.cmd_args <- commandArgs(trailingOnly = FALSE)
.file_arg <- .cmd_args[grepl("^--file=", .cmd_args)]
if (length(.file_arg) == 0L) {
  stop("mcp_server: cannot determine the script path (no --file= argument). ",
       "Launch via Rscript, e.g. Rscript --no-init-file scripts/mcp_server.R",
       call. = FALSE)
}
.script_path <- normalizePath(sub("^--file=", "", .file_arg[1L]),
                              winslash = "/", mustWork = FALSE)
.script_dir <- dirname(.script_path)

.ts_find_root <- function(start_dir) {
  dir <- start_dir
  for (i in seq_len(5L)) {
    if (file.exists(file.path(dir, "app.R")) &&
        file.exists(file.path(dir, "renv", "activate.R"))) return(dir)
    parent <- dirname(dir)
    if (identical(parent, dir)) break
    dir <- parent
  }
  NULL
}

.project_root <- .ts_find_root(.script_dir)
if (is.null(.project_root)) {
  stop("mcp_server: project root not found (expected app.R + renv/activate.R above ",
       .script_dir, "). Do not move this script out of scripts/.", call. = FALSE)
}

# Put the project library on the search path WITHOUT activating renv (which is
# what pollutes stdout). Globs cover both `<os>/R-<ver>/<arch>` and `<os>/R-<ver>`.
.ts_renv_libs <- function(root) {
  cand <- unique(c(
    Sys.glob(file.path(root, "renv", "library", "*", "R-*", "*")),
    Sys.glob(file.path(root, "renv", "library", "*", "R-*"))
  ))
  cand[dir.exists(cand)]
}
.renv_libs <- .ts_renv_libs(.project_root)
if (length(.renv_libs)) .libPaths(c(.renv_libs, .libPaths()))

.ts_have_jsonlite <- requireNamespace("jsonlite", quietly = TRUE)

# --- 1. `--check`: diagnose, then exit (diagnostics go to STDERR) ------------
# Deliberately NOT gated behind a hard require: a check that dies when a
# dependency is missing cannot diagnose the missing dependency.

.args <- commandArgs(trailingOnly = TRUE)
if ("--check" %in% .args) {
  .ts_stderr("mcp_server check:")
  .ts_stderr("  project_root : ", .project_root)
  .ts_stderr("  renv_libs    : ", if (length(.renv_libs)) paste(.renv_libs, collapse = ", ") else "(none)")
  .ts_stderr("  jsonlite     : ", if (.ts_have_jsonlite) as.character(utils::packageVersion("jsonlite")) else "MISSING")
  .ts_stderr("  transport    : stdio (no network port)")
  .ts_stderr("  tools        : transcripto_drive_status, transcripto_drive_read_result (read-only)")
  .ts_stderr("                 transcripto_drive_snapshot (M3a: passive, read-only)")
  .ts_stderr("                 transcripto_drive_set_inputs (M3b: controlled write, scenario.json only)")
  .ts_stderr("                 transcripto_drive_run (M3c: controlled run, run_pipeline only)")
  .ts_stderr("                 transcripto_drive_wait (M4: bounded observation; <=1 snapshot, post-terminal)")
  .ts_stderr("                 transcripto_drive_set_armed (M2: arm/disarm; writes arm.json only)")
  .ts_stderr("                 transcripto_drive_export (S2: the ONE artefact; no caller-selectable field)")
  .ts_stderr("  export route : spatial_qc -> spatial_qc_hotspot_csv (app-chosen destination, filename, format)")
  .ts_stderr("  dependencies : mcptools/btw/ellmer NOT required")
  # The tool-schema and reader checks need the drive files SOURCED, so they run
  # in a SECOND phase further down (section 1b). Everything above is
  # bootstrap-only and can be answered before anything is sourced.
}

if (!.ts_have_jsonlite) {
  stop("mcp_server: 'jsonlite' is not available on .libPaths(). From the project ",
       "root run: renv::restore()", call. = FALSE)
}

# --- 2. Reuse the EXISTING drive readers (sourced, never duplicated) ---------
# Capture any stray stdout from `source()` so the transport stream stays pure.
.ts_source_quiet <- function(path) {
  capture.output(source(path, local = FALSE), type = "output")
  invisible(TRUE)
}
.ts_source_quiet(file.path(.project_root, "R", "core", "drive_allowlist.R"))
.ts_source_quiet(file.path(.project_root, "R", "core", "drive_watcher.R"))
ts_drive_boot(.project_root)

# --- 3. Framing: newline-delimited JSON (MCP stdio) -------------------------

#' Serialise one JSON-RPC message to a SINGLE line of JSON.
#' `pretty = FALSE` keeps the structure on one line, and jsonlite escapes CR/LF
#' inside strings; the two `fixed` substitutions are defence in depth so that no
#' literal newline can ever reach the wire, whatever a future payload contains.
.ts_json <- function(x) {
  s <- as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", pretty = FALSE))
  s <- gsub("\n", " ", s, fixed = TRUE)
  s <- gsub("\r", " ", s, fixed = TRUE)
  s
}

#' The empty JSON OBJECT `{}`. jsonlite renders `list()` as `[]`, but an MCP
#' result envelope is an object — `ping` must answer `{}`.
.ts_empty_object <- function() stats::setNames(list(), character(0))

#' Write one message: the JSON body, then ONE newline, then flush.
#' `cat()` is deliberate: `writeBin()`/`writeChar()` write NOTHING to stdout on
#' this host. Windows text mode turns the newline into CRLF, and a conformant
#' NDJSON reader strips a trailing CR (the official SDK does exactly that).
.ts_write_message <- function(payload) {
  cat(payload, "\n", sep = "", file = stdout())
  flush(stdout())
}

#' Read one message, byte by byte, until LF (the MCP stdio delimiter).
#' @return NULL on clean EOF with nothing buffered; list(text=, eof=FALSE) for a
#'   complete line; list(text=, eof=TRUE) for a final line with no trailing LF;
#'   list(text=, too_long=TRUE) when the line exceeds `max_bytes`.
.ts_read_message <- function(con, max_bytes = 1048576L) {
  buf <- raw(0)
  repeat {
    b <- readBin(con, "raw", n = 1L)
    if (length(b) == 0L) {
      if (length(buf) == 0L) return(NULL)
      return(list(text = rawToChar(buf), eof = TRUE))
    }
    if (identical(b, as.raw(10L))) return(list(text = rawToChar(buf), eof = FALSE))
    buf <- c(buf, b)
    if (length(buf) > max_bytes) return(list(text = rawToChar(buf), too_long = TRUE))
  }
}

#' Drop ONE trailing CR (so CRLF clients are accepted) without using a regex.
.ts_strip_cr <- function(text) {
  if (endsWith(text, "\r")) substr(text, 1L, nchar(text) - 1L) else text
}

# --- 4. JSON-RPC envelopes --------------------------------------------------
# PROTOCOL errors are JSON-RPC `error` objects (standard codes).
# DOMAIN errors are TOOL results with `isError: true` and a structured payload —
# the MCP tool-result schema has no `code` field, so domain codes live in the
# content. The two channels are never conflated.

.ts_result <- function(id, result) list(jsonrpc = "2.0", id = id, result = result)
.ts_error <- function(id, code, message) {
  list(jsonrpc = "2.0", id = id, error = list(code = code, message = message))
}

.ts_tool_ok <- function(structured, text) {
  list(content = list(list(type = "text", text = text)),
       structuredContent = structured, isError = FALSE)
}
.ts_tool_err <- function(code, message, hint = NULL, detail = NULL) {
  sc <- list(code = code, message = message, hint = hint)
  # `detail` is OPTIONAL and additive: every existing caller keeps the same
  # envelope, so M1/M2 answers are unchanged.
  if (!is.null(detail)) sc$detail <- detail
  list(content = list(list(type = "text", text = paste0(code, ": ", message))),
       structuredContent = sc,
       isError = TRUE)
}

.ts_domain_codes <- c("NO_SESSION", "STALE_SESSION", "RESULT_SESSION_MISMATCH",
                      "INVALID_PROTOCOL", "READ_FAILED",
                      # M2 — arm/disarm
                      "AMBIGUOUS_SESSION", "SESSION_MISMATCH", "ARM_WRITE_FAILED",
                      # M3b — controlled set_inputs. A WRITE needs a vocabulary a
                      # READ does not: who is allowed to be written to, what may
                      # be written, and whether the write landed.
                      "SESSION_ASSERTION_REQUIRED", "SESSION_NOT_ARMED",
                      "MODULE_NOT_ALLOWED", "INPUT_NOT_ALLOWED",
                      "INPUT_MODULE_MISMATCH", "VALUE_REFUSED",
                      "PAYLOAD_REFUSED", "PAYLOAD_TOO_LARGE", "SEQ_STALE",
                      "SCENARIO_WRITE_FAILED",
                      # M3c — controlled run
                      "ACTION_NOT_ALLOWED", "ACTION_AMBIGUOUS",
                      "BUTTON_MODULE_MISMATCH", "JOB_ALREADY_RUNNING",
                      # M4 — long-job observation
                      "SESSION_LOST", "OBSERVE_FAILED",
                      "OBSERVE_SKIPPED_NO_TERMINAL")
# M3a — the passive snapshot tool adds NO new code on purpose: it is a READ, so
# every refusal it can produce is one of the six above (NO_SESSION,
# STALE_SESSION, INVALID_PROTOCOL, READ_FAILED, RESULT_SESSION_MISMATCH,
# SESSION_MISMATCH). A read-only tool that needed a new error vocabulary would
# be a sign it was doing more than reading.

# --- 5. Sanitising and small helpers ---------------------------------------

.as_chr <- function(x) {
  if (is.null(x) || length(x) != 1L) return(NA_character_)
  v <- suppressWarnings(as.character(x))
  if (length(v) != 1L || is.na(v)) NA_character_ else v
}

#' Sanitise free text for the wire: drops everything after the first LF,
#' replaces paths and token-shaped runs, truncates.
#' `known` is LOAD-BEARING: without it the 8+-char token heuristic redacts the
#' protocol's own vocabulary (the measured "snapshot" trap).
.ts_clean <- function(x, max_chars = 200L) {
  ts_drive_badge_sanitize(x, max_chars,
                          known = c(TS_DRIVE_STATUSES, TS_DRIVE_MODULES, TS_DRIVE_ACTIONS,
                                    TS_DRIVE_VIEWERS))
}

.ts_parse_iso <- function(x) {
  s <- .as_chr(x)
  if (is.na(s)) return(NA)
  t <- tryCatch(as.POSIXct(s, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                error = function(e) NA)
  if (length(t) != 1L || is.na(t)) return(NA)
  as.numeric(t)
}

#' Best-effort OS liveness for a PID. `NA` means "could not determine" and is
#' NEVER reported as dead. Only called when the heartbeat is already stale —
#' that is when it distinguishes "the app exited" from "the app is alive but not
#' beating because a SYNCHRONOUS job is in flight".
.ts_pid_alive <- function(pid) {
  p <- suppressWarnings(as.integer(pid))
  if (length(p) != 1L || is.na(p)) return(NA)
  if (.Platform$OS.type != "windows") {
    return(tryCatch(file.exists(file.path("/proc", as.character(p))),
                    error = function(e) NA))
  }
  out <- tryCatch(suppressWarnings(
    system2("tasklist", c("/FO", "CSV", "/NH"), stdout = TRUE, stderr = FALSE)),
    error = function(e) NULL)
  if (is.null(out) || !length(out)) return(NA)
  any(grepl(paste0('","', p, '","'), out, fixed = TRUE))
}

# --- 6. Session validation (existing contract, reused) ----------------------

#' Resolve the current session, or a domain error describing why not.
#' @return list(ok = TRUE, hb =, fresh =, age =, pid_alive =) or
#'         list(ok = FALSE, result = <tool error>).
.ts_session <- function() {
  hb <- ts_drive_read_ready()
  if (is.null(hb)) {
    return(list(ok = FALSE, result = .ts_tool_err(
      "NO_SESSION",
      "No drive session is attached: tools/_drive/ready.json is absent.",
      "Start the app (tools/launch_dev_drive.R) and open the Viewer or a localhost tab.")))
  }
  proto <- .as_chr(hb$protocol)
  if (is.na(proto) || !identical(proto, TS_DRIVE_PROTOCOL)) {
    return(list(ok = FALSE, result = .ts_tool_err(
      "INVALID_PROTOCOL",
      sprintf("ready.json declares protocol '%s'; expected '%s'.",
              .ts_clean(proto %|NA|% "(absent)"), TS_DRIVE_PROTOCOL),
      "The handshake was written by a different build; do not trust it.")))
  }
  if (is.null(hb$pid) || is.null(hb$started_at)) {
    return(list(ok = FALSE, result = .ts_tool_err(
      "INVALID_PROTOCOL",
      "ready.json is missing the session identity fields (pid, started_at).",
      "Session identity is (pid, started_at); without them no result can be trusted.")))
  }
  age <- ts_drive_ready_age()
  fresh <- ts_drive_ready_fresh()
  alive <- NA
  if (!fresh) alive <- .ts_pid_alive(hb$pid)
  if (!fresh) {
    hint <- if (isTRUE(alive)) {
      paste0("The process is ALIVE but the heartbeat is older than ",
             ts_drive_hb_timeout(), " s. A long SYNCHRONOUS job blocks the event loop ",
             "and stops the beat: check the job state before concluding the session is dead.")
    } else if (identical(alive, FALSE)) {
      "The process is gone: this is a leftover handshake from a closed session."
    } else {
      "Process liveness could not be determined."
    }
    return(list(ok = FALSE, result = .ts_tool_err(
      "STALE_SESSION",
      sprintf("ready.json heartbeat is stale (age %s s > timeout %s s).",
              if (is.finite(age)) round(age, 1) else "Inf", ts_drive_hb_timeout()),
      hint)))
  }
  list(ok = TRUE, hb = hb, fresh = TRUE, age = age, pid_alive = alive)
}

`%|NA|%` <- function(a, b) if (is.null(a) || length(a) != 1L || is.na(a)) b else a

# --- 7. The two READ-ONLY tools --------------------------------------------

.ts_tool_status <- function() {
  s <- .ts_session()
  if (!s$ok) return(s$result)
  hb <- s$hb
  job <- tryCatch(ts_drive_job_state(), error = function(e) NULL)
  structured <- list(
    session = list(
      present            = TRUE,
      protocol           = TS_DRIVE_PROTOCOL,
      # The DERIVED correlation id: how a client NAMES this session (M3b pins it
      # in `expect.session_id`) without the raw token ever being published.
      session_id         = .ts_session_id(.as_chr(hb$pid), .as_chr(hb$started_at),
                                          .as_chr(hb$session_token)),
      viewer             = .ts_clean(.as_chr(hb$viewer) %|NA|% "unknown", 40L),
      pid                = suppressWarnings(as.integer(hb$pid)),
      pid_alive          = s$pid_alive,
      started_at         = .ts_clean(.as_chr(hb$started_at), 40L),
      heartbeat_age_s    = if (is.finite(s$age)) round(s$age, 1) else NA_real_,
      heartbeat_timeout_s = ts_drive_hb_timeout(),
      fresh              = TRUE,
      session_token      = "<redacted>",
      # The APP's published view (`ready.json.armed`) — NOT ts_drive_arm_state(),
      # which is gated by ts_drive_interactive() and therefore answers
      # "non-interactive session" -> FALSE forever under Rscript. The M2
      # arm/disarm tool already reads ready.json.armed; this makes the two
      # read-only tools AGREE instead of contradicting each other.
      armed              = isTRUE(hb$armed),
      last_seq           = suppressWarnings(as.integer(.as_chr(hb$last_seq) %|NA|% NA_integer_))
    ),
    job = list(
      busy  = isTRUE(ts_drive_job_busy()),
      state = .ts_clean(.as_chr(job$button) %|NA|% "", 60L)
    ),
    scope = list(read_only = TRUE,
                 modules = TS_DRIVE_MODULES,
                 buttons = TS_DRIVE_BUTTONS,
                 allowlisted_inputs = length(TS_DRIVE_ALLOWLIST))
  )
  text <- sprintf("drive session live: viewer=%s pid=%s age=%.1fs armed=%s job_busy=%s",
                  structured$session$viewer, structured$session$pid,
                  structured$session$heartbeat_age_s, structured$session$armed,
                  structured$job$busy)
  .ts_tool_ok(structured, text)
}

.ts_tool_read_result <- function() {
  s <- .ts_session()
  if (!s$ok) return(s$result)
  hb <- s$hb

  res <- ts_drive_read_result()
  if (is.null(res)) {
    # Absent is an EMPTY state, not a failure: the session is live, nothing has
    # been driven yet.
    return(.ts_tool_ok(
      list(present = FALSE, session = list(pid = suppressWarnings(as.integer(hb$pid)),
                                           started_at = .ts_clean(.as_chr(hb$started_at), 40L))),
      "no result.json yet: the session is live but no scenario has been applied"))
  }

  rproto <- .as_chr(res$protocol)
  if (is.na(rproto) || !identical(rproto, TS_DRIVE_PROTOCOL)) {
    return(.ts_tool_err(
      "INVALID_PROTOCOL",
      sprintf("result.json declares protocol '%s'; expected '%s'.",
              .ts_clean(rproto %|NA|% "(absent)"), TS_DRIVE_PROTOCOL),
      "The verdict was written by a different build."))
  }

  # Identity: never trust ack_seq alone. result.json carries NO session token,
  # so the only available cross-check is that it must not PREDATE this session.
  applied <- .ts_parse_iso(res$applied_at)
  started <- .ts_parse_iso(hb$started_at)
  if (is.na(applied)) {
    return(.ts_tool_err(
      "READ_FAILED",
      "result.json has no parseable applied_at, so its session cannot be established.",
      "A verdict without a timestamp cannot be attributed to the live session."))
  }
  if (!is.na(started) && applied < started) {
    return(.ts_tool_err(
      "RESULT_SESSION_MISMATCH",
      "result.json predates the live session: it is a leftover verdict from a previous session.",
      "Compare (pid, started_at) before trusting any result; ack_seq alone is not evidence."))
  }

  status <- .as_chr(res$status)
  errs <- as.list(res$errors %|NA|% list())
  warns <- as.list(res$warnings %|NA|% list())
  snap <- res$snapshot
  structured <- list(
    present       = TRUE,
    protocol      = TS_DRIVE_PROTOCOL,
    ack_seq       = suppressWarnings(as.integer(.as_chr(res$ack_seq) %|NA|% NA_integer_)),
    status        = .ts_clean(status %|NA|% "", 40L),
    status_terminal = tryCatch(isTRUE(ts_drive_status_terminal(status)),
                               error = function(e) NA),
    applied_at    = .ts_clean(.as_chr(res$applied_at), 40L),
    active_module = .ts_clean(.as_chr(res$active_module) %|NA|% "", 40L),
    armed         = isTRUE(res$armed),
    preserve_data = isTRUE(res$preserve_data),
    errors        = lapply(utils::head(errs, 5L), .ts_clean, max_chars = 200L),
    warnings      = lapply(utils::head(warns, 5L), .ts_clean, max_chars = 200L),
    snapshot      = if (is.null(snap)) NULL else list(
      has_data     = isTRUE(snap$has_data),
      object_class = .ts_clean(.as_chr(snap$object_class) %|NA|% "", 40L),
      n_genes      = suppressWarnings(as.integer(.as_chr(snap$n_genes) %|NA|% NA_integer_)),
      n_samples    = suppressWarnings(as.integer(.as_chr(snap$n_samples) %|NA|% NA_integer_))
    ),
    session = list(pid = suppressWarnings(as.integer(hb$pid)),
                   started_at = .ts_clean(.as_chr(hb$started_at), 40L),
                   identity_ok = TRUE)
  )
  text <- sprintf("drive result: status=%s seq=%s module=%s terminal=%s",
                  structured$status, structured$ack_seq, structured$active_module,
                  structured$status_terminal)
  .ts_tool_ok(structured, text)
}

# --- 7b. M2 — the ONE controlled arm/disarm tool ----------------------------
#
# Scope is deliberately narrow: write tools/_drive/arm.json and nothing else.
# No snapshot, no set_inputs, no run, no wait, no analysis. The app applies the
# arm on its own next poll tick; this server does not wait for it (waiting is
# explicitly out of scope for M2).
#
# The token written is ALWAYS the one the live session publishes in ready.json.
# A caller-supplied token is only ever an ASSERTION (`expect`), never a source.
# The wildcard "*" is therefore unreachable: a session publishing it is refused
# as ambiguous, and pinning it yields SESSION_MISMATCH.

.ts_expect_fields <- c("pid", "started_at", "session_token", "session_id")

#' Validate the live session, and an optional identity assertion.
#'
#' THE one place that answers "is there a live session I may write to, and is it
#' the one the caller thinks it is?". Extracted from the M2 arm/disarm tool so
#' that M3b cannot drift from it: a second, independently written copy of these
#' checks is exactly how two write tools come to disagree about what is live.
#'
#' @param expect Assertion object (or NULL). An ASSERTION, never a source.
#' @param require_assertion TRUE for a write that must NAME its session: then
#'   `expect$session_id` is mandatory. It is the DERIVED id (never the token),
#'   so it is obtainable through this server without publishing a secret, and it
#'   is non-wildcard by construction.
#' @param allow_stale TRUE lets a stale handshake through WITHOUT a pin — the
#'   `wait` tool's graded mode, where observing a stalled session is the job.
#' @param rescue_pinned_alive TRUE enables THE ARMING BOOTSTRAP: a stale
#'   handshake is accepted when the caller PINS the session identity
#'   (`session_id`, or `pid` + `started_at`) AND the pinned pid is ALIVE, and
#'   the pin is still validated below (a wrong pin fails closed as
#'   SESSION_MISMATCH). Only `set_armed` passes it: the poller beats only while
#'   ARMED (drive_watcher.R: `cursor$armed && due`), so an unarmed session goes
#'   stale within `ts_drive_hb_timeout()` and arming — the one write that
#'   RESTORES liveness — must remain reachable, or every session's first action
#'   is a hand-written `arm.json` that bypasses the tool. Writes that CHANGE
#'   analysis state (set_inputs, run, export) never get the rescue: a stale
#'   heartbeat is exactly when the session's true state is unknown.
#' @return list(ok, error, hb, pid, started, token, age). On refusal `ok` is
#'   FALSE and `error` is the ready-made tool result.
.ts_session_assert <- function(expect = NULL, require_assertion = FALSE,
                               allow_stale = FALSE, rescue_pinned_alive = FALSE) {
  refuse <- function(code, message, hint = NULL) {
    list(ok = FALSE, error = .ts_tool_err(code, message, hint))
  }

  # --- 1. session present ---------------------------------------------------
  hb <- ts_drive_read_ready()
  if (is.null(hb)) {
    return(refuse(
      "NO_SESSION",
      "No drive session is attached: tools/_drive/ready.json is absent.",
      "Start the app (tools/launch_dev_drive.R) and open the Viewer or a localhost tab."))
  }

  # --- 2. protocol ----------------------------------------------------------
  proto <- .as_chr(hb$protocol)
  if (is.na(proto) || !identical(proto, TS_DRIVE_PROTOCOL)) {
    return(refuse(
      "INVALID_PROTOCOL",
      sprintf("ready.json declares protocol '%s'; expected '%s'.",
              .ts_clean(proto %|NA|% "(absent)"), TS_DRIVE_PROTOCOL),
      "The handshake was written by a different build; refusing to use it."))
  }

  # --- 3. identity complete, and unambiguous --------------------------------
  pid <- suppressWarnings(as.integer(.as_chr(hb$pid)))
  started <- .as_chr(hb$started_at)
  token <- .as_chr(hb$session_token)
  missing <- character(0)
  if (is.na(pid)) missing <- c(missing, "pid")
  if (is.na(started) || !nzchar(started)) missing <- c(missing, "started_at")
  if (is.na(token) || !nzchar(token)) missing <- c(missing, "session_token")
  if (length(missing)) {
    return(refuse(
      "AMBIGUOUS_SESSION",
      sprintf("The live session does not declare a usable identity: %s missing or empty.",
              paste(missing, collapse = ", ")),
      "Without (pid, started_at, session_token) this session cannot be told apart from another."))
  }
  if (identical(token, "*")) {
    return(refuse(
      "AMBIGUOUS_SESSION",
      "The live session declares the wildcard token '*', which this server never accepts.",
      "A wildcard would address any session. Relaunch so the app publishes a real token."))
  }

  # --- 4. heartbeat freshness ----------------------------------------------
  age <- ts_drive_ready_age()
  fresh <- ts_drive_ready_fresh()
  if (!fresh) {
    alive <- .ts_pid_alive(pid)
    hint <- if (isTRUE(alive)) {
      paste0("The process is ALIVE but the heartbeat is older than ",
             ts_drive_hb_timeout(), " s. A long SYNCHRONOUS job stops the beat; so does an ",
             "app launched WITHOUT tools/launch_dev_drive.R (the arm gate then never fires).")
    } else if (identical(alive, FALSE)) {
      "The process is gone: this is a leftover handshake from a closed session."
    } else {
      "Process liveness could not be determined."
    }
    if (!isTRUE(allow_stale)) {
      # THE ARMING BOOTSTRAP. A stale handshake is survivable only for the one
      # write that restores liveness, and only under a pin the next section
      # validates. The liveness check is on the LIVE session's pid — the same
      # pid the pin must name, so a wrong pin cannot buy the rescue.
      rescued <- FALSE
      if (isTRUE(rescue_pinned_alive) && isTRUE(alive) && is.list(expect)) {
        pins_sid <- !is.null(expect$session_id) &&
          nzchar(.as_chr(expect$session_id) %|NA|% "")
        pins_pair <- !is.null(expect$pid) && !is.null(expect$started_at) &&
          nzchar(.as_chr(expect$started_at) %|NA|% "")
        rescued <- pins_sid || pins_pair
      }
      if (!rescued) {
        return(refuse(
          "STALE_SESSION",
          sprintf("ready.json heartbeat is stale (age %s s > timeout %s s).",
                  if (is.finite(age)) round(age, 1) else "Inf", ts_drive_hb_timeout()),
          hint))
      }
    }
  }

  # --- 5. identity assertion ------------------------------------------------
  sid <- .ts_session_id(pid, started, token)
  ex_token <- if (is.list(expect)) expect$session_token else NULL
  ex_sid <- if (is.list(expect)) expect$session_id else NULL
  if (require_assertion) {
    if (is.null(ex_sid) || is.na(.as_chr(ex_sid)) || !nzchar(.as_chr(ex_sid))) {
      return(refuse(
        "SESSION_ASSERTION_REQUIRED",
        "This write requires `expect.session_id`: it must NAME the session it writes to.",
        paste0("Read transcripto_drive_status (or transcripto_drive_snapshot) and pin its ",
               "`session_id`. The wildcard '*' is never accepted, and the raw token is ",
               "never published.")))
    }
    if (identical(.as_chr(ex_sid), "*")) {
      return(refuse(
        "AMBIGUOUS_SESSION",
        "The assertion pins the wildcard '*', which this server never accepts.",
        "A wildcard would address any session; pin the derived session_id."))
    }
  }
  if (!is.null(expect)) {
    if (!is.null(ex_sid)) {
      want <- .as_chr(ex_sid)
      if (is.na(want) || !identical(want, sid)) {
        return(refuse(
          "SESSION_MISMATCH",
          "The live session's derived id does not match the pinned value.",
          "The session you inspected is gone; re-read transcripto_drive_status."))
      }
    }
    if (!is.null(expect$pid)) {
      want <- suppressWarnings(as.integer(.as_chr(expect$pid)))
      if (is.na(want) || !identical(want, pid)) {
        return(refuse(
          "SESSION_MISMATCH",
          "The live session's pid does not match the pinned value.",
          "The session you inspected is gone; re-read transcripto_drive_status."))
      }
    }
    if (!is.null(expect$started_at)) {
      want <- .as_chr(expect$started_at)
      if (is.na(want) || !identical(want, started)) {
        return(refuse(
          "SESSION_MISMATCH",
          "The live session's started_at does not match the pinned value.",
          "A different session has taken over; re-read transcripto_drive_status."))
      }
    }
    if (!is.null(ex_token)) {
      want <- .as_chr(ex_token)
      if (is.na(want) || !identical(want, token)) {
        return(refuse(
          "SESSION_MISMATCH",
          "The live session's token does not match the pinned value.",
          "Never pin '*': this server always writes the session's own token."))
      }
    }
  }

  list(ok = TRUE, error = NULL, hb = hb, pid = pid, started = started,
       token = token, age = age, fresh = fresh)
}

.ts_tool_set_armed <- function(armed, expect = NULL) {
  # THE ARMING BOOTSTRAP: the only tool that may rescue a stale handshake (a
  # live pin required; see .ts_session_assert). Disarming a stalled session is
  # rescued by the same gate, which is what makes an abandoned session
  # reachable again without a restart.
  a <- .ts_session_assert(expect, rescue_pinned_alive = TRUE)
  if (!isTRUE(a$ok)) return(a$error)
  hb <- a$hb; pid <- a$pid; started <- a$started; token <- a$token; age <- a$age

  # --- 6. atomic write, reusing the drive's own writer ----------------------
  observed_before <- isTRUE(hb$armed)
  payload <- list(protocol = TS_DRIVE_PROTOCOL, token = token, armed = isTRUE(armed))
  wrote <- tryCatch(ts_drive_write_json(payload, ts_drive_path("arm.json")),
                    error = function(e) FALSE)
  if (!isTRUE(wrote)) {
    return(.ts_tool_err(
      "ARM_WRITE_FAILED",
      "The atomic write of tools/_drive/arm.json did not land.",
      "Another process may be holding the file; retry, or check tools/check_writers.R."))
  }

  structured <- list(
    action = if (isTRUE(armed)) "arm" else "disarm",
    armed = isTRUE(armed),
    wrote = TRUE,
    arm_file = "arm.json",            # basename only — never an absolute path
    wildcard_used = FALSE,
    rescued_stale = !isTRUE(a$fresh),
    session = list(
      pid = pid,
      started_at = .ts_clean(started, 40L),
      viewer = .ts_clean(.as_chr(hb$viewer) %|NA|% "unknown", 40L),
      heartbeat_age_s = if (is.finite(age)) round(age, 1) else NA_real_,
      heartbeat_timeout_s = ts_drive_hb_timeout(),
      session_token = "<redacted>"
    ),
    pinned = list(
      pid = !is.null(expect$pid),
      started_at = !is.null(expect$started_at),
      session_token = !is.null(expect$session_token)
    ),
    observed_armed_before = observed_before,
    note = paste0("arm.json is written; the app applies it on its next poll tick (~800 ms). ",
                  "This tool does not wait. A stale heartbeat is accepted ONLY under the ",
                  "arming bootstrap: the caller pins the session identity and the pinned ",
                  "pid is alive (rescued_stale = true).")
  )
  .ts_tool_ok(structured,
              sprintf("%s: arm.json written (armed=%s, pid %s)",
                      structured$action, isTRUE(armed), pid))
}

# --- 7d. M3a — the PASSIVE, READ-ONLY snapshot tool -------------------------
#
# WHAT M3a IS, AND WHAT IT DELIBERATELY IS NOT.
#   M3a is a PASSIVE observation: it reads the verdict the app ALREADY published
#   (`tools/_drive/result.json`) and projects it through a CLOSED allowlist. It
#   writes nothing, sets no Shiny input, fires no button, arms nothing, and never
#   polls.
#   An ACTIVE snapshot — writing a `snapshot` scenario and waiting for its
#   `ack_seq` — is a DIFFERENT operation (a controlled write plus a wait) and is
#   deliberately NOT here; it belongs to M4, where the synchronous long job makes
#   a fresh snapshot per poll the only honest way to observe progress.
#
# WHY THE RAW PAYLOAD IS NEVER REUSED.
#   The app's `snapshot` object is built for the APP, not for the wire. Measured:
#   `mod_bulk_filter.R` publishes `samples = colnames(counts)` — SAMPLE NAMES, i.e.
#   biological identifiers — and `mod_bulk_de_run.R` publishes `active_contrast`,
#   a user label. `error_state` / `probe_error` are free text that can embed a
#   path. The projection below is therefore built FIELD BY FIELD from an
#   allowlist: a field this code does not name cannot escape, whatever a module
#   starts publishing tomorrow. That is the difference between a projection and a
#   filter.

# Scalar field names that MAY cross the wire, at any level of the module state.
# Anything not listed is dropped and COUNTED — never silently ignored.
.ts_snapshot_module_allow <- c(
  # counters
  "n_genes", "n_samples", "n_contrasts", "n_padj_finite", "n_significant",
  "n_results", "has_data", "bypass", "ready",
  # bounded scalars
  "elapsed_s", "seq",
  # short enums / fixed strings (sanitised and truncated below)
  "status", "state", "module", "action", "convention", "probe_error",
  # per-step outcomes of the SC auto-pipeline, read INSIDE the `steps` slot.
  # Five states, no free text: `ran` / `skipped` (not selected) / `ignored`
  # (selected, attempted, declined by the pipeline's own size guard) / `error`
  # (selected, no result). The distinction is the whole point — collapsing
  # `ignored` into `ran` would claim a computation that never happened.
  "mapping", "qc", "norm", "pca", "clusters", "umap", "tsne", "singler",
  "markers", "pathway", "correlation", "trajectory"
)

# SLOT names that may be opened and recursed into. The measured shape of
# `bulk_filter`'s probe is `list(filtered_counts = list(...), vst_mat = list(...))`
# — its top level is slot NAMES, not counters — so without this the whole module
# would project to nothing. The set is CLOSED on purpose: a slot a module starts
# publishing is a disclosure decision, not a side effect. `steps` is the SC
# auto-pipeline's per-step record — scalars only, one level deep.
.ts_snapshot_slot_allow <- c("filtered_counts", "vst_mat", "steps")

# Bounds. Every one is enforced below and echoed in `redaction.limits`.
# max_modules = 10: the Bulk pattern action is the tenth drivable module, and a
# smaller cap would silently truncate a runnable module out of snapshots.
.ts_snapshot_max_modules <- 10L
.ts_snapshot_max_fields  <- 12L
.ts_snapshot_max_depth   <- 2L
.ts_snapshot_max_string  <- 120L
.ts_snapshot_max_bytes   <- 8192L

#' A scalar we are willing to publish: length 1, never NA, never non-finite.
.ts_snapshot_scalar <- function(x) {
  if (is.null(x) || length(x) != 1L) return(FALSE)
  if (is.logical(x)) return(!is.na(x))
  if (is.numeric(x)) return(is.finite(x))
  if (is.character(x)) return(!is.na(x))
  FALSE
}

#' Project ONE module's published state through the closed allowlist.
#'
#' Recurses at most `.ts_snapshot_max_depth` levels (the measured shape is
#' `list(<slot> = list(n_genes, n_samples, samples))`) and keeps SCALARS only.
#' A vector (`samples`), a matrix, or anything deeper is DROPPED.
#'
#' @return list(value = named list, dropped = integer)
.ts_snapshot_project_module <- function(state, depth = 1L) {
  out <- list(); dropped <- 0L
  if (!is.list(state)) return(list(value = out, dropped = 1L))
  nms <- names(state)
  for (i in seq_along(nms)) {
    if (length(out) >= .ts_snapshot_max_fields) { dropped <- dropped + 1L; next }
    nm <- nms[[i]]
    v <- state[[i]]
    if (.ts_snapshot_scalar(v)) {
      if (!nm %in% .ts_snapshot_module_allow) { dropped <- dropped + 1L; next }
      if (is.character(v)) v <- .ts_clean(v, .ts_snapshot_max_string)
      out[[nm]] <- v
    } else if (is.list(v) && length(v) && depth < .ts_snapshot_max_depth &&
               nm %in% .ts_snapshot_slot_allow) {
      # A SLOT: only the allowlisted scalars INSIDE it are published.
      sub <- .ts_snapshot_project_module(v, depth + 1L)
      dropped <- dropped + sub$dropped
      if (length(sub$value)) out[[nm]] <- sub$value
    } else {
      dropped <- dropped + 1L
    }
  }
  list(value = out, dropped = dropped)
}

#' The bounded, redacted projection of `result.json`'s `snapshot`.
#'
#' @return list(object, modules, dropped, truncated)
.ts_project_snapshot <- function(snap) {
  object <- list(has_data = FALSE, object_class = NULL, n_genes = NULL,
                 n_samples = NULL, error_state = NULL)
  modules <- list(); dropped <- 0L; truncated <- FALSE

  if (is.list(snap)) {
    object$has_data     <- isTRUE(snap$has_data)
    object$object_class <- .ts_clean(.as_chr(snap$object_class) %|NA|% "", 40L)
    object$n_genes      <- suppressWarnings(as.integer(.as_chr(snap$n_genes) %|NA|% NA_integer_))
    object$n_samples    <- suppressWarnings(as.integer(.as_chr(snap$n_samples) %|NA|% NA_integer_))
    object$error_state  <- .ts_clean(.as_chr(snap$error_state) %|NA|% "", 60L)

    mods <- snap$modules
    if (is.list(mods) && length(mods)) {
      # CLOSED set: a module name this server does not know is not published.
      known <- intersect(names(mods), TS_DRIVE_MODULES)
      dropped <- dropped + length(setdiff(names(mods), TS_DRIVE_MODULES))
      if (length(known) > .ts_snapshot_max_modules) {
        dropped <- dropped + (length(known) - .ts_snapshot_max_modules)
        known <- known[seq_len(.ts_snapshot_max_modules)]
        truncated <- TRUE
      }
      for (k in known) {
        pm <- .ts_snapshot_project_module(mods[[k]])
        dropped <- dropped + pm$dropped
        if (length(pm$value)) modules[[k]] <- pm$value
      }
    }
  }

  # Hard byte cap. Module detail is dropped rather than allowed to grow: the
  # object summary is what an agent polls for, and it must always fit.
  bytes <- nchar(.ts_json(list(object = object, modules = modules)), type = "bytes")
  if (bytes > .ts_snapshot_max_bytes && length(modules)) {
    dropped <- dropped + length(modules)
    modules <- list()
    truncated <- TRUE
  }
  list(object = object, modules = modules, dropped = dropped, truncated = truncated)
}

#' The disclosure contract, published WITH the answer so a reader can audit it.
.ts_redaction_note <- function(dropped, truncated) {
  list(
    policy = "snapshot-allowlist-v1",
    dropped_field_count = as.integer(dropped),
    truncated = isTRUE(truncated),
    limits = list(max_modules = .ts_snapshot_max_modules,
                  max_fields_per_module = .ts_snapshot_max_fields,
                  max_depth = .ts_snapshot_max_depth,
                  max_string_chars = .ts_snapshot_max_string,
                  max_output_bytes = .ts_snapshot_max_bytes),
    never_published = c("sample names", "gene names", "spot or cell IDs",
                        "coordinates", "cluster labels", "matrices",
                        "session token", "pid", "started_at",
                        "absolute paths", "raw log text")
  )
}

#' A DERIVED correlation id for the live session.
#'
#' Deliberately NOT a cryptographic primitive: it exists so two observations can
#' be correlated, and so an agent can PIN the session it inspected, without this
#' server ever publishing `pid`, `started_at` or the token. Bounded 31-bit
#' accumulators keep the arithmetic exact in doubles.
.ts_session_id <- function(pid, started_at, token) {
  b <- utf8ToInt(paste(pid, started_at, token, sep = "|"))
  h1 <- 5381; h2 <- 52711
  for (x in b) {
    h1 <- (h1 * 33 + x) %% 2147483647
    h2 <- (h2 * 65599 + x) %% 2147483647
  }
  sprintf("%08x%08x", as.integer(h1), as.integer(h2))
}

.ts_tool_snapshot <- function(expect = NULL) {
  s <- .ts_session()
  if (!s$ok) return(s$result)
  hb <- s$hb

  sid <- .ts_session_id(.as_chr(hb$pid), .as_chr(hb$started_at),
                        .as_chr(hb$session_token))

  # Optional pin, on the M2 pattern: an ASSERTION, never a source.
  if (!is.null(expect) && !is.null(expect$session_id)) {
    want <- .as_chr(expect$session_id)
    if (is.na(want) || !identical(want, sid)) {
      return(.ts_tool_err(
        "SESSION_MISMATCH",
        "The live session's derived id does not match the pinned value.",
        "The session you observed is gone; re-read transcripto_drive_status."))
    }
  }

  session <- list(
    session_id          = sid,
    viewer              = .ts_clean(.as_chr(hb$viewer) %|NA|% "unknown", 40L),
    armed               = isTRUE(hb$armed),
    identity_ok         = TRUE,
    fresh               = TRUE,
    heartbeat_age_s     = if (is.finite(s$age)) round(s$age, 1) else NA_real_,
    heartbeat_timeout_s = ts_drive_hb_timeout(),
    last_seq            = suppressWarnings(as.integer(.as_chr(hb$last_seq) %|NA|% NA_integer_))
  )

  res <- ts_drive_read_result()
  if (is.null(res)) {
    # An ABSENT verdict is an EMPTY observation, not a failure — and it is STALE
    # by definition: nothing has been published for this session yet.
    return(.ts_tool_ok(
      list(
        present        = FALSE,
        protocol       = TS_DRIVE_PROTOCOL,
        session        = session,
        observation    = list(
          ack_seq = NULL, age_s = NULL, stale = TRUE, non_terminal = TRUE,
          stale_reason = "no verdict has been published for this session"),
        protocol_state = list(status = NULL, terminal = FALSE, acknowledged = FALSE),
        business_state = list(
          active_module = NULL, completion_verified = FALSE,
          note = "no verdict published yet; nothing to observe"),
        object         = list(has_data = FALSE, object_class = NULL, n_genes = NULL,
                              n_samples = NULL, error_state = NULL),
        modules        = list(),
        redaction      = .ts_redaction_note(0L, FALSE)
      ),
      sprintf("drive snapshot: no verdict yet (session %s)", sid)))
  }

  rproto <- .as_chr(res$protocol)
  if (is.na(rproto) || !identical(rproto, TS_DRIVE_PROTOCOL)) {
    return(.ts_tool_err(
      "INVALID_PROTOCOL",
      sprintf("result.json declares protocol '%s'; expected '%s'.",
              .ts_clean(rproto %|NA|% "(absent)"), TS_DRIVE_PROTOCOL),
      "The verdict was written by a different build."))
  }

  applied <- .ts_parse_iso(res$applied_at)
  if (is.na(applied)) {
    return(.ts_tool_err(
      "READ_FAILED",
      "result.json has no parseable applied_at, so its session cannot be established.",
      "A verdict without a timestamp cannot be attributed to the live session."))
  }
  started <- .ts_parse_iso(hb$started_at)
  if (!is.na(started) && applied < started) {
    return(.ts_tool_err(
      "RESULT_SESSION_MISMATCH",
      "result.json predates the live session: it is a leftover verdict.",
      "Compare (pid, started_at) before trusting any verdict."))
  }

  status   <- .ts_clean(.as_chr(res$status) %|NA|% "", 40L)
  terminal <- tryCatch(isTRUE(ts_drive_status_terminal(status)), error = function(e) FALSE)
  age_s    <- as.numeric(difftime(Sys.time(), applied, units = "secs"))
  timeout  <- ts_drive_hb_timeout()

  # STALENESS — what a passive read CAN know, and nothing more. Two independent
  # reasons, both derived from the files themselves:
  #   * the verdict is explicitly NON-TERMINAL (`applied` / `running`), so it
  #     does not describe a settled state;
  #   * the verdict is older than the heartbeat timeout, so it may predate the
  #     session's current state.
  # It deliberately does NOT consult `ts_drive_job_busy()`: the job lives in the
  # APP's process, so from THIS process that call is a constant FALSE — the same
  # silent lie as `ts_drive_arm_state()`.
  stale_reasons <- character(0)
  if (!terminal) {
    stale_reasons <- c(stale_reasons,
                       sprintf("the verdict is not terminal (status '%s')", status))
  }
  if (is.finite(age_s) && age_s > timeout) {
    stale_reasons <- c(stale_reasons, sprintf(
      "the verdict is %.1f s old, beyond the %s s heartbeat timeout", age_s, timeout))
  }
  stale <- length(stale_reasons) > 0L

  proj <- .ts_project_snapshot(res$snapshot)

  structured <- list(
    present  = TRUE,
    protocol = TS_DRIVE_PROTOCOL,
    session  = session,
    observation = list(
      ack_seq      = suppressWarnings(as.integer(.as_chr(res$ack_seq) %|NA|% NA_integer_)),
      age_s        = if (is.finite(age_s)) round(age_s, 1) else NULL,
      stale        = stale,
      non_terminal = !terminal,
      stale_reason = if (length(stale_reasons)) paste(stale_reasons, collapse = "; ") else NULL
    ),
    # PROTOCOL state and BUSINESS state are published as two SEPARATE objects on
    # purpose: a protocol acknowledgement is not a business outcome, and merging
    # them is exactly how `done` came to be read as "the pipeline finished".
    protocol_state = list(
      status       = status,
      terminal     = terminal,
      acknowledged = TRUE
    ),
    business_state = list(
      active_module       = .ts_clean(.as_chr(res$active_module) %|NA|% "", 40L),
      completion_verified = FALSE,
      note = paste0("M3a is a PASSIVE read of a published verdict: it cannot verify ",
                    "that any pipeline finished. `terminal` is a statement about the ",
                    "PROTOCOL, not about the analysis.")
    ),
    object    = proj$object,
    modules   = proj$modules,
    redaction = .ts_redaction_note(proj$dropped, proj$truncated)
  )

  .ts_tool_ok(structured, sprintf(
    "drive snapshot: status=%s terminal=%s stale=%s modules=%d",
    status, terminal, stale, length(proj$modules)))
}

# --- 7e. M3b — the controlled `set_inputs` tool -----------------------------
#
# WHAT M3b IS.
#   It writes ONE `scenario.json` with `action = "set_inputs"` through the
#   drive's OWN atomic writer, then STOPS. It clicks nothing, runs no action,
#   waits for nothing, and never touches a Shiny input directly: the app's
#   poller applies the scenario on its next tick, and only while armed. The
#   answer is therefore an ACKNOWLEDGEMENT OF A WRITE — never a claim that an
#   input was applied.
#
# WHY A VALUE SCHEMA EXISTS HERE AT ALL.
#   The app's allowlist (`TS_DRIVE_ALLOWLIST`, reused from `R/core/` at boot)
#   says WHICH inputs exist and their widget `kind`, but it does NOT constrain
#   the VALUE. Measured (M3 audit, finding F-A): `updateSelectInput(selected =
#   <not a choice>)` does not raise, so the app answers `applied` for an input
#   that never changed. M3b closes that by validating the value HERE, BEFORE
#   anything is written.
#
# WHY FIVE ALLOWLISTED INPUTS ARE DELIBERATELY NOT EXPOSED.
#   `condition_col`, `covariates`, `group_ref`, `group_target` take their value
#   domain from the LIVE session's metadata, and `scores_source` from
#   `bulk_gene_set_choices()`. This server cannot read either, so it cannot
#   honour "reject unsupported values" for them. Fail closed: they are not
#   exposed, and `redaction.not_exposed` says so. Exposing an input whose
#   domain we cannot check would re-open exactly the hole this tool closes.

# --- the value schema (server-side policy, cross-checked against the app) ----
# `min`/`max` are GENEROUS server-side bounds, not an app contract: they exist
# to reject nonsense, not to second-guess a legitimate analysis setting.
TS_MCP_INPUT_SCHEMA <- list(
  # ── import_bulk ──────────────────────────────────────────────────────────
  "import_bulk-bulk_import_mode"    = list(type = "enum", values = c("merged_matrix", "per_sample")),
  "import_bulk-counts_format"       = list(type = "enum", values = c("rows", "cols")),
  "import_bulk-counts_has_header"   = list(type = "boolean"),
  "import_bulk-counts_has_rownames" = list(type = "boolean"),
  "import_bulk-metadata_has_header" = list(type = "boolean"),
  "import_bulk-metadata_has_rownames" = list(type = "boolean"),
  "import_bulk-ps_fill_zero"        = list(type = "boolean"),
  "import_bulk-infer_delimiter"     = list(type = "text", max_chars = 16L),
  "import_bulk-project_name"        = list(type = "text", max_chars = 120L),
  "import_bulk-multi_label"         = list(type = "text", max_chars = 120L),
  "import_bulk-min_counts"          = list(type = "number", min = 0, max = 1e7),
  "import_bulk-ps_dup_threshold"    = list(type = "number", min = 0, max = 1),
  # ── bulk_de (the four metadata-driven selects are NOT here, by design) ───
  "bulk-de-de_engine"               = list(type = "enum", values = c("deseq2", "edger", "limma")),
  "bulk-de-shrink_lfc"              = list(type = "boolean"),
  "bulk-de-lfc_thresh"              = list(type = "number", min = -100, max = 100),
  "bulk-de-padj_thresh"             = list(type = "number", min = 0, max = 1),
  "bulk-de-heatmap_top_n"           = list(type = "number", min = 1, max = 1000, integer = TRUE),
  # ── bulk_filter ──────────────────────────────────────────────────────────
  "bulk-filter-min_count"           = list(type = "number", min = 0, max = 1e7),
  "bulk-filter-min_samples"         = list(type = "number", min = 0, max = 1e6, integer = TRUE),
  "bulk-filter-min_count_per_sample" = list(type = "number", min = 0, max = 1e6),
  # ── bulk_pathways (scores_source is NOT here, by design) ─────────────────
  "bulk-pathways-enrich_mode"       = list(type = "enum", values = c("ora", "gsea")),
  "bulk-pathways-pathway_source"    = list(type = "enum", values = c("up", "down", "all_sig", "manual")),
  "bulk-pathways-pathway_db"        = list(type = "enum", values = c("GOBP", "KEGG", "Reactome")),
  "bulk-pathways-pathway_org"       = list(type = "enum", values = c("human", "mouse")),
  "bulk-pathways-pathway_pval"      = list(type = "number", min = 0, max = 1),
  "bulk-pathways-scores_org"        = list(type = "enum", values = c("human", "mouse")),
  "bulk-pathways-scores_method"     = list(type = "enum", values = c("ssgsea", "gsva", "plage", "zscore")),
  "bulk-pathways-scores_min_size"   = list(type = "number", min = 1, max = 100000, integer = TRUE),
  "bulk-pathways-scores_max_size"   = list(type = "number", min = 1, max = 100000, integer = TRUE)
)

# Payload bounds.
TS_MCP_MAX_INPUTS        <- 24L
TS_MCP_MAX_PAYLOAD_BYTES <- 4096L

#' Cross-check the value schema against the APP's own allowlist.
#'
#' The schema is a SECOND table, so it can drift. This makes the drift
#' detectable instead of silent: every id must exist in `TS_DRIVE_ALLOWLIST`,
#' its `type` must match the widget `kind`, and a BUTTON must never be settable.
#' Returns `character(0)` when consistent. Surfaced by `--check`.
.ts_mcp_schema_problems <- function() {
  problems <- character(0)
  for (id in names(TS_MCP_INPUT_SCHEMA)) {
    e <- ts_drive_allowlist_get(id)
    if (is.null(e)) {
      problems <- c(problems, sprintf("%s: not in TS_DRIVE_ALLOWLIST", id)); next
    }
    sch <- TS_MCP_INPUT_SCHEMA[[id]]
    want <- switch(e$kind,
                   checkbox = "boolean", numeric = "number", text = "text",
                   radio = "enum", select = "enum", button = "FORBIDDEN", "?")
    if (identical(want, "FORBIDDEN")) {
      problems <- c(problems, sprintf("%s: is a BUTTON and must never be settable", id))
    } else if (!identical(sch$type, want)) {
      problems <- c(problems, sprintf("%s: schema type '%s' != widget kind '%s' (expects '%s')",
                                      id, sch$type, e$kind, want))
    }
    if (identical(sch$type, "enum") && !length(sch$values)) {
      problems <- c(problems, sprintf("%s: enum with no values", id))
    }
    if (identical(sch$type, "number") &&
        (!is.numeric(sch$min) || !is.numeric(sch$max) || sch$min > sch$max)) {
      problems <- c(problems, sprintf("%s: number without a valid min/max", id))
    }
    if (identical(sch$type, "text") && (!is.numeric(sch$max_chars) || sch$max_chars < 1L)) {
      problems <- c(problems, sprintf("%s: text without max_chars", id))
    }
  }
  problems
}

#' Input ids the app allows but M3b does NOT expose. Named, never silent.
.ts_mcp_not_exposed <- function() {
  all_ids <- names(TS_DRIVE_ALLOWLIST)
  settable <- all_ids[vapply(all_ids, function(id) {
    e <- ts_drive_allowlist_get(id)
    !is.null(e) && !identical(e$kind, "button")
  }, logical(1))]
  setdiff(settable, names(TS_MCP_INPUT_SCHEMA))
}

#' Validate ONE value against its schema. Never throws, never echoes the value.
#' @return list(ok, reason)
.ts_input_value_ok <- function(id, value) {
  sch <- TS_MCP_INPUT_SCHEMA[[id]]
  if (is.null(sch)) return(list(ok = FALSE, reason = "no value schema for this input"))
  v <- value
  if (is.list(v) || length(v) != 1L) {
    return(list(ok = FALSE, reason = "must be a single scalar"))
  }
  if (identical(sch$type, "boolean")) {
    if (!is.logical(v) || is.na(v)) return(list(ok = FALSE, reason = "must be true or false"))
    return(list(ok = TRUE, reason = NULL))
  }
  if (identical(sch$type, "number")) {
    n <- suppressWarnings(as.numeric(v))
    if (is.na(n) || !is.finite(n)) return(list(ok = FALSE, reason = "must be a finite number"))
    if (isTRUE(sch$integer) && n != trunc(n)) return(list(ok = FALSE, reason = "must be an integer"))
    if (n < sch$min || n > sch$max) {
      return(list(ok = FALSE, reason = sprintf("out of the accepted range [%s, %s]", sch$min, sch$max)))
    }
    return(list(ok = TRUE, reason = NULL))
  }
  if (identical(sch$type, "text")) {
    if (!is.character(v) || is.na(v)) return(list(ok = FALSE, reason = "must be a string"))
    if (nchar(v) > sch$max_chars) {
      return(list(ok = FALSE, reason = sprintf("longer than %d characters", sch$max_chars)))
    }
    # Control characters via code point, so the source carries no escape sequence.
    if (grepl(intToUtf8(10L), v, fixed = TRUE) || grepl(intToUtf8(13L), v, fixed = TRUE)) {
      return(list(ok = FALSE, reason = "must not contain a line break"))
    }
    return(list(ok = TRUE, reason = NULL))
  }
  if (identical(sch$type, "enum")) {
    if (!is.character(v) || is.na(v)) return(list(ok = FALSE, reason = "must be a string"))
    if (!v %in% sch$values) {
      return(list(ok = FALSE,
                  reason = sprintf("must be one of: %s", paste(sch$values, collapse = ", "))))
    }
    return(list(ok = TRUE, reason = NULL))
  }
  list(ok = FALSE, reason = "unsupported schema type")
}

.ts_tool_set_inputs <- function(seq, module, inputs, preserve_data = TRUE, expect = NULL) {
  # A WRITE names its session: the assertion is mandatory and non-wildcard.
  a <- .ts_session_assert(expect, require_assertion = TRUE)
  if (!isTRUE(a$ok)) return(a$error)
  hb <- a$hb; pid <- a$pid; started <- a$started; token <- a$token; age <- a$age

  # --- arm state ------------------------------------------------------------
  # An unarmed session never consumes a scenario, so writing one would be a
  # SILENT no-op. Refuse instead of pretending.
  if (!isTRUE(hb$armed)) {
    return(.ts_tool_err(
      "SESSION_NOT_ARMED",
      "The live session is not armed, so it would never consume this scenario.",
      "Arm it with transcripto_drive_set_armed first; the write would otherwise be a silent no-op."))
  }

  # --- module ---------------------------------------------------------------
  if (!is.character(module) || length(module) != 1L || is.na(module) ||
      !module %in% TS_MCP_SET_INPUT_MODULES) {
    return(.ts_tool_err(
      "MODULE_NOT_ALLOWED",
      sprintf("module '%s' is outside the M3b input allowlist.", .ts_clean(module %|NA|% "(absent)")) ,
      sprintf("Allowed modules: %s.", paste(TS_MCP_SET_INPUT_MODULES, collapse = ", "))))
  }

  # --- inputs shape ---------------------------------------------------------
  if (!is.list(inputs) || !length(inputs)) {
    return(.ts_tool_err("PAYLOAD_REFUSED", "`inputs` must be a non-empty object.",
                        "Provide at least one allowlisted inputId."))
  }
  if (length(inputs) > TS_MCP_MAX_INPUTS) {
    return(.ts_tool_err(
      "PAYLOAD_TOO_LARGE",
      sprintf("`inputs` carries %d entries; the limit is %d.", length(inputs), TS_MCP_MAX_INPUTS),
      "Split the write into several scenarios."))
  }
  if (is.null(names(inputs)) || any(!nzchar(names(inputs)))) {
    return(.ts_tool_err("PAYLOAD_REFUSED", "Every entry of `inputs` must be named by its inputId.",
                        "An unnamed entry cannot be validated."))
  }

  # --- per-field validation (BEFORE anything is written) --------------------
  # Each refusal is classified so the caller gets a PRECISE code, and the
  # per-field reasons travel in `detail` — WITHOUT ever echoing a value.
  refused <- list(); kind <- character(0)
  for (id in names(inputs)) {
    if (!id %in% names(TS_MCP_INPUT_SCHEMA)) {
      refused[[id]] <- if (is.null(ts_drive_allowlist_get(id))) {
        "not on the app's allowlist"
      } else {
        "allowed by the app but NOT exposed by M3b (its value domain is not statically verifiable)"
      }
      kind[id] <- "INPUT_NOT_ALLOWED"
      next
    }
    owner <- ts_drive_module_of(id)
    if (!identical(owner, module)) {
      refused[[id]] <- sprintf("belongs to module '%s', not '%s'", owner, module)
      kind[id] <- "INPUT_MODULE_MISMATCH"
      next
    }
    v <- .ts_input_value_ok(id, inputs[[id]])
    if (!isTRUE(v$ok)) { refused[[id]] <- v$reason; kind[id] <- "VALUE_REFUSED"; next }
  }
  if (length(refused)) {
    code <- if ("INPUT_NOT_ALLOWED" %in% kind) "INPUT_NOT_ALLOWED"
            else if ("INPUT_MODULE_MISMATCH" %in% kind) "INPUT_MODULE_MISMATCH"
            else "VALUE_REFUSED"
    return(.ts_tool_err(
      code,
      sprintf("%d input(s) refused; NOTHING was written.", length(refused)),
      "Only allowlisted inputs of the named module, with an in-range value, are accepted.",
      detail = list(refused = refused)))
  }

  # --- sequence monotonicity ------------------------------------------------
  seq <- suppressWarnings(as.integer(seq))
  if (is.na(seq) || seq < 1L) {
    return(.ts_tool_err("PAYLOAD_REFUSED", "`seq` must be an integer >= 1.",
                        "The app ignores any scenario whose seq is not greater than its last_seq."))
  }
  last_seq <- suppressWarnings(as.integer(.as_chr(hb$last_seq) %|NA|% 0L))
  if (seq <= last_seq) {
    return(.ts_tool_err(
      "SEQ_STALE",
      sprintf("seq %s is not greater than the session's last_seq %s.", seq, last_seq),
      "A replayed or stale scenario is ignored by the app; re-read the session and use a higher seq."))
  }

  payload <- list(
    protocol      = TS_DRIVE_PROTOCOL,
    seq           = seq,
    session_token = token,
    module        = module,
    action        = "set_inputs",
    preserve_data = isTRUE(preserve_data),
    inputs        = inputs
  )
  # Payload byte bound, measured on the SERIALISED scenario (the real thing).
  payload_bytes <- nchar(.ts_json(payload), type = "bytes")
  if (payload_bytes > TS_MCP_MAX_PAYLOAD_BYTES) {
    return(.ts_tool_err(
      "PAYLOAD_TOO_LARGE",
      sprintf("the serialised scenario is %d bytes; the limit is %d.",
              payload_bytes, TS_MCP_MAX_PAYLOAD_BYTES),
      "Reduce the number or the length of the values."))
  }

  # Re-read the session IMMEDIATELY before writing: a second writer between the
  # check above and this line would otherwise be silently overwritten, and the
  # heartbeat could have gone stale in between.
  hb2 <- ts_drive_read_ready()
  if (is.null(hb2) || !identical(.as_chr(hb2$session_token), token)) {
    return(.ts_tool_err("SESSION_MISMATCH",
                        "The session changed between validation and the write.",
                        "Re-read transcripto_drive_status and retry."))
  }
  last2 <- suppressWarnings(as.integer(.as_chr(hb2$last_seq) %|NA|% 0L))
  if (seq <= last2) {
    return(.ts_tool_err(
      "SEQ_STALE",
      sprintf("another scenario was consumed meanwhile (last_seq is now %s).", last2),
      "Re-read the session and use a higher seq."))
  }
  if (!ts_drive_ready_fresh()) {
    return(.ts_tool_err("STALE_SESSION",
                        "The heartbeat went stale between validation and the write.",
                        "Nothing was written; re-read the session."))
  }

  # --- the existing atomic writer ------------------------------------------
  wrote <- tryCatch(ts_drive_write_json(payload, ts_drive_path("scenario.json")),
                    error = function(e) FALSE)
  if (!isTRUE(wrote)) {
    return(.ts_tool_err(
      "SCENARIO_WRITE_FAILED",
      "The atomic write of tools/_drive/scenario.json did not land.",
      "Another process may be holding the file; retry, or check tools/check_writers.R."))
  }

  structured <- list(
    accepted      = TRUE,
    wrote         = TRUE,
    scenario_file = "scenario.json",     # basename only — never an absolute path
    seq           = seq,
    module        = module,
    action        = "set_inputs",
    preserve_data = isTRUE(preserve_data),
    inputs_accepted = length(names(inputs)),
    input_ids     = names(inputs),       # IDS only — a value is NEVER echoed
    wildcard_used = FALSE,
    session = list(
      pid                 = pid,
      started_at          = .ts_clean(started, 40L),
      viewer              = .ts_clean(.as_chr(hb$viewer) %|NA|% "unknown", 40L),
      heartbeat_age_s     = if (is.finite(age)) round(age, 1) else NA_real_,
      heartbeat_timeout_s = ts_drive_hb_timeout(),
      armed               = TRUE,
      session_token       = "<redacted>"
    ),
    pinned = list(pid = !is.null(expect$pid),
                  started_at = !is.null(expect$started_at),
                  session_token = !is.null(expect$session_token)),
    applied = FALSE,
    redaction = list(
      policy = "set-inputs-v1",
      values_echoed = FALSE,
      limits = list(max_inputs = TS_MCP_MAX_INPUTS,
                    max_payload_bytes = TS_MCP_MAX_PAYLOAD_BYTES),
      not_exposed = .ts_mcp_not_exposed()
    ),
    note = paste0("scenario.json is written; the app applies it on its next poll tick (~800 ms). ",
                  "This tool does NOT wait and does NOT apply anything: `applied` stays false ",
                  "until a later observation shows it.")
  )
  .ts_tool_ok(structured, sprintf(
    "drive set_inputs: seq=%s module=%s inputs=%d written (not applied)",
    seq, module, length(names(inputs))))
}

# --- 7f. M3c — the controlled `run` tool -------------------------------------
#
# WHAT M3c IS.
#   It writes ONE `scenario.json` with `action = "run_pipeline"` through the
#   drive's OWN atomic writer, then STOPS. The app's poller fires the bound
#   button on its next tick. Nothing is clicked here, nothing is computed here,
#   nothing is waited for. `run_pipeline` is the ONLY run action M3c can write.
#
# WHAT M3c CANNOT KNOW, AND THEREFORE REFUSES TO GUESS.
#   Three refusals live in the APP and are deliberately NOT reproduced here:
#     * the module's own READINESS guard ("not ready: no bulk object loaded");
#     * whether the button is actually BOUND (`ts_drive_bind_button()`);
#     * the job's lifecycle once it starts.
#   Inventing any of them here would produce exactly the `done` lie the long-job
#   contract exists to prevent. They arrive in the VERDICT instead.
#
# WHAT M3c *CAN* SEE, AND THEREFORE CHECKS.
#   Contract B ("one job at a time") is enforced by the app against its own
#   in-process job state, which this server cannot read. But the app PUBLISHES
#   that state: a long job writes `status = "running"` to `result.json` at
#   dispatch. The FILE is therefore the signal, and a `running` verdict is
#   treated as a busy session. A job that never published `running` is invisible
#   to us — the app still refuses it, and that refusal arrives as `invalid`.

# The ONLY run action M3c may write. `TS_DRIVE_ACTIONS` also carries
# `import_file` (path-bearing), `snapshot` (M3a/M4), `reset_module`
# (unimplemented) and `set_inputs` (M3b) — none is a run action, and none is
# reachable from this tool.
TS_MCP_RUN_ACTIONS <- c("run_pipeline")
# Six modules are run-only by DECLARED DESIGN, not by omission: the Spatial
# pipeline, the SC auto-pipeline, SC annotation, SC marker and Bulk signature
# actions, and the Bulk pattern action whose one undeclared parameter is
# resolved by rule, publish frozen, non-negotiable parameter sets and do not
# read scenario inputs. `setdiff()` is kept so a future module is settable by default, and the
# refusal is named in the answer.
#
# 🔴 THIS LIST IS A REFUSAL, AND IT IS NOT COVERED BY A CONSTANT ON PURPOSE: the
# MCP server runs in its own process and cannot see the allowlist CONSTANTS
# (TS_DRIVE_SC_*), only the two vectors it sources. Naming them here keeps the
# server self-contained; the counterpart on the app side is the guard in
# ts_drive_apply_scenario() using the same constants. The only automated check
# that the two agree is test-mcp-sc-local.R, which must be re-run whenever either
# side changes — a module forgotten HERE would be settable, which is the one
# failure direction that matters.
TS_MCP_SET_INPUT_MODULES <- setdiff(
  TS_DRIVE_MODULES,
  c("spatial_pipeline", "spatial_qc", "sc_pipeline", "sc_annotation",
    "sc_markers", "sc_pathways", "bulk_signatures", "bulk_pattern",
    "bulk_network")
  )

# ── S2: the ONE export tool ──────────────────────────────────────────────────
# This tool has NO export parameters. Not a handler, not an outputId, not a
# destination, not a filename, not a format, not even a module: the route table
# has one entry and the app resolves the module from it. The only fields are the
# protocol envelope (`seq`, `expect`), which are counters and identity, not
# choices about the artefact.
#
# It writes a `scenario.json` of its own, exactly as `set_armed` writes an
# `arm.json` — which is what makes it usable from a tool at all. `import_file`
# needed a hand-placed scenario file for want of this, and the cost was that the
# one action an agent most wants (get the data in) was the one action it could not
# perform.
TS_MCP_EXPORT_MODULE <- "spatial_qc"

#' The scenario payload for `export_result`, as a REBUILT list.
#'
#' Built field by field rather than assembled from the caller's arguments, so a
#' field this server did not put there cannot reach the app. `import` is absent
#' entirely: the app-side validator refuses every key, so sending even an empty
#' block would be a refusal waiting to happen.
.ts_export_payload <- function(seq, token) {
  list(
    protocol      = TS_DRIVE_PROTOCOL,
    seq           = seq,
    session_token = token,
    module        = TS_MCP_EXPORT_MODULE,
    action        = "export_result"
  )
}

.ts_tool_export <- function(seq, expect = NULL) {
  if (!is.numeric(seq) || length(seq) != 1L || is.na(seq) || seq < 1 || seq != as.integer(seq)) {
    return(.ts_tool_err("PAYLOAD_REFUSED", "`seq` must be an integer >= 1.",
                        "The app ignores any scenario whose seq is not greater than its last_seq."))
  }
  seq <- as.integer(seq)
  asserted <- .ts_session_assert(expect, require_assertion = TRUE)
  if (!isTRUE(asserted$ok)) return(asserted$error)
  last_seq <- suppressWarnings(as.integer(.as_chr(asserted$hb$last_seq) %|NA|% 0L))
  if (seq <= last_seq) {
    return(.ts_tool_err("SEQ_STALE",
      sprintf("seq %s is not greater than the session's last_seq %s.", seq, last_seq),
      "A replayed or stale scenario is ignored by the app; re-read the session and use a higher seq."))
  }

  payload <- .ts_export_payload(seq, asserted$token)
  if (nchar(.ts_json(payload), type = "bytes") > TS_MCP_MAX_PAYLOAD_BYTES) {
    return(.ts_tool_err("PAYLOAD_TOO_LARGE", "the serialised scenario is too large.",
                        "An export carries no values; this should not happen."))
  }

  # The existing atomic writer, the same one `run` and `set_inputs` use.
  wrote <- tryCatch(ts_drive_write_json(payload, ts_drive_path("scenario.json")),
                    error = function(e) FALSE)
  if (!isTRUE(wrote)) {
    return(.ts_tool_err("SCENARIO_WRITE_FAILED",
      "The atomic write of tools/_drive/scenario.json did not land.",
      "Another process may be holding the file; retry, or check tools/check_writers.R."))
  }

  .ts_tool_ok(list(
    dispatched = "export_result",
    module = TS_MCP_EXPORT_MODULE,
    seq = seq,
    chosen_by_caller = list(handler = NULL, output_id = NULL, destination = NULL,
                            filename = NULL, format = NULL),
    note = paste("The app chose the route, the destination, the filename and the",
                 "format. Read the descriptor from transcripto_drive_read_result",
                 "once the app has acknowledged this seq."),
    disclosure = .ts_redaction_note(0L, FALSE)
  ), paste0("export_result dispatched for module '", TS_MCP_EXPORT_MODULE,
            "' at seq ", seq, ". The app picks the artefact; nothing was selectable."))
}

# module -> the buttons that ARE a run action for it. Closed on purpose.
TS_MCP_RUN_BUTTONS <- list(
  import_bulk   = "import_bulk-btn_load",
  import_spatial = "import_spatial-btn_import",
  bulk_filter   = "bulk-filter-run_filter_norm",
  bulk_de       = "bulk-de-run_de",
  bulk_pathways = c("bulk-pathways-run_pathway", "bulk-pathways-run_scores"),
  bulk_signatures = c("bulk-signatures-run_signatures"),
  bulk_pattern = c("bulk-pattern-run_pattern"),
  bulk_network = c("bulk-network-run_network"),
  spatial_pipeline = "spatial-pipeline-btn_run_all",
  spatial_qc   = "spatial-qc-btn_hotspots",
  sc_pipeline   = "sc-pipeline-run_auto_pipeline",
  sc_annotation = "sc-annotation-run_annot",
  sc_markers    = "sc-markers-run_markers",
  sc_pathways   = "sc-pathways-run_pathway"
)

#' Cross-check the run allowlist against the app's own button table.
#' Returns character(0) when consistent. Surfaced by `--check`.
.ts_mcp_run_problems <- function() {
  problems <- character(0)
  for (m in names(TS_MCP_RUN_BUTTONS)) {
    if (!m %in% TS_DRIVE_MODULES) {
      problems <- c(problems,
                    sprintf("run allowlist names module '%s', not in TS_DRIVE_MODULES", m))
      next
    }
    for (b in TS_MCP_RUN_BUTTONS[[m]]) {
      if (!b %in% TS_DRIVE_BUTTONS) {
        problems <- c(problems, sprintf("%s: button '%s' is not in TS_DRIVE_BUTTONS", m, b))
      } else if (!identical(ts_drive_button_module(b), m)) {
        problems <- c(problems, sprintf("%s: button '%s' actually belongs to module '%s'",
                                        m, b, ts_drive_button_module(b)))
      }
    }
  }
  # Every drivable module must be reachable. Two ways, not one:
  #   * it names a run button here, so `transcripto_drive_run` can click it; or
  #   * it is a declared IMPORTER, which `import_file` reaches directly and which
  #     therefore has no button to click.
  #
  # ⚠️ The importer exemption is new with `import_sc`, and it is not a loosening
  # done to silence a warning. The human SC entry point is a TWO-STEP flow
  # (`btn_add_sample` registers the pair, `btn_load_dir` consumes the list), so
  # `btn_load_dir` is meaningless to an agent — it `req()`s a `sample_list()` that
  # only a human click can fill. Binding it would put a button in
  # `TS_MCP_RUN_BUTTONS` whose every press fails, which is worse than an honest
  # "this module exposes no run action". The two existing importers keep their real
  # buttons, so the check below still holds them to the stricter rule.
  missing <- setdiff(TS_DRIVE_MODULES, names(TS_MCP_RUN_BUTTONS))
  orphans <- setdiff(missing, TS_DRIVE_IMPORT_MODULES)
  if (length(orphans)) {
    problems <- c(problems,
                  sprintf("module(s) with no run action and no importer: %s",
                          paste(orphans, collapse = ", ")))
  }
  problems
}

#' Resolve the run button of a module, or explain why it cannot be resolved.
#' An OMITTED button on a module with SEVERAL run actions is AMBIGUOUS, not a
#' silent default: picking one would run work the caller never named.
#' @return list(ok, button, reason)
.ts_mcp_resolve_button <- function(module, button) {
  allowed <- TS_MCP_RUN_BUTTONS[[module]]
  if (is.null(allowed)) {
    return(list(ok = FALSE, button = NULL,
                reason = sprintf("module '%s' exposes no run action in M3c", module)))
  }
  if (is.null(button)) {
    if (length(allowed) > 1L) {
      return(list(ok = FALSE, button = NULL, reason = sprintf(
        "module '%s' exposes %d run actions (%s); name one with `button`",
        module, length(allowed), paste(allowed, collapse = ", "))))
    }
    return(list(ok = TRUE, button = allowed[[1L]], reason = NULL))
  }
  if (!button %in% allowed) {
    return(list(ok = FALSE, button = NULL, reason = sprintf(
      "button '%s' is not a run action of module '%s' (allowed: %s)",
      button, module, paste(allowed, collapse = ", "))))
  }
  list(ok = TRUE, button = button, reason = NULL)
}

.ts_tool_run <- function(seq, module, button = NULL, preserve_data = TRUE, expect = NULL) {
  # The SAME non-secret assertion as M3b: a write names its session.
  a <- .ts_session_assert(expect, require_assertion = TRUE)
  if (!isTRUE(a$ok)) return(a$error)
  hb <- a$hb; pid <- a$pid; started <- a$started; token <- a$token; age <- a$age

  # --- precondition: armed --------------------------------------------------
  if (!isTRUE(hb$armed)) {
    return(.ts_tool_err(
      "SESSION_NOT_ARMED",
      "The live session is not armed, so it would never consume this scenario.",
      "Arm it with transcripto_drive_set_armed first; the write would otherwise be a silent no-op."))
  }

  # --- precondition: module ------------------------------------------------
  if (!is.character(module) || length(module) != 1L || is.na(module) ||
      !module %in% TS_DRIVE_MODULES) {
    return(.ts_tool_err(
      "MODULE_NOT_ALLOWED",
        sprintf("module '%s' is outside the drive allowlist.",
              .ts_clean(module %|NA|% "(absent)")),
      sprintf("Allowed modules: %s.", paste(TS_DRIVE_MODULES, collapse = ", "))))
  }

  # --- precondition: the action is the ONE allowlisted run action -----------
  # Defensive: the allowlist is a constant, but a future edit that widened it
  # must fail loudly rather than silently start writing other actions.
  if (!identical(TS_MCP_RUN_ACTIONS, "run_pipeline")) {
    return(.ts_tool_err(
      "ACTION_NOT_ALLOWED",
      "This build's run allowlist is not exactly 'run_pipeline'; refusing to write.",
      "M3c is authorised for the single controlled run action only."))
  }

  # --- precondition: action / module compatibility --------------------------
  if (!is.null(button) &&
      (!is.character(button) || length(button) != 1L || is.na(button) || !nzchar(button))) {
    return(.ts_tool_err("ACTION_NOT_ALLOWED", "`button` must be one non-empty string.",
                        sprintf("Allowed run actions: %s.",
                                paste(unlist(TS_MCP_RUN_BUTTONS), collapse = ", "))))
  }
  if (!is.null(button) && !button %in% TS_DRIVE_BUTTONS) {
    return(.ts_tool_err(
      "ACTION_NOT_ALLOWED",
      sprintf("unknown run action '%s'.", .ts_clean(button, 60L)),
      sprintf("Allowed run actions: %s.", paste(unlist(TS_MCP_RUN_BUTTONS), collapse = ", "))))
  }
  r <- .ts_mcp_resolve_button(module, button)
  if (!isTRUE(r$ok)) {
    code <- if (is.null(button)) "ACTION_AMBIGUOUS" else "BUTTON_MODULE_MISMATCH"
    return(.ts_tool_err(
      code, r$reason,
      if (is.null(button)) "Name the run action explicitly; M3c never picks one for you."
      else "The button must belong to the module named in the same call."))
  }
  btn <- r$button

  # --- precondition: Contract B, read from the WIRE -------------------------
  res <- ts_drive_read_result()
  last_status <- if (is.list(res)) .as_chr(res$status) else NA_character_
  last_ack <- if (is.list(res)) suppressWarnings(as.integer(.as_chr(res$ack_seq) %|NA|% NA_integer_)) else NA_integer_
  if (identical(last_status, "running")) {
    return(.ts_tool_err(
      "JOB_ALREADY_RUNNING",
      "The session's last published verdict is 'running': a job is already in flight.",
      paste0("Contract B refuses a second job rather than queueing it. Wait for the verdict to ",
             "reach a terminal status (done/error/invalid) before dispatching another run."),
      detail = list(last_ack_seq = last_ack, last_status = last_status)))
  }

  # --- precondition: monotonic sequence -------------------------------------
  seq <- suppressWarnings(as.integer(seq))
  if (is.na(seq) || seq < 1L) {
    return(.ts_tool_err("PAYLOAD_REFUSED", "`seq` must be an integer >= 1.",
                        "The app ignores any scenario whose seq is not greater than its last_seq."))
  }
  last_seq <- suppressWarnings(as.integer(.as_chr(hb$last_seq) %|NA|% 0L))
  if (seq <= last_seq) {
    return(.ts_tool_err(
      "SEQ_STALE",
      sprintf("seq %s is not greater than the session's last_seq %s.", seq, last_seq),
      "A replayed or stale scenario is ignored by the app; re-read the session and use a higher seq."))
  }

  payload <- list(
    protocol      = TS_DRIVE_PROTOCOL,
    seq           = seq,
    session_token = token,
    module        = module,
    action        = "run_pipeline",
    button        = btn,
    preserve_data = isTRUE(preserve_data)
  )
  payload_bytes <- nchar(.ts_json(payload), type = "bytes")
  if (payload_bytes > TS_MCP_MAX_PAYLOAD_BYTES) {
    return(.ts_tool_err(
      "PAYLOAD_TOO_LARGE",
      sprintf("the serialised scenario is %d bytes; the limit is %d.",
              payload_bytes, TS_MCP_MAX_PAYLOAD_BYTES),
      "A run action carries no values; this should not happen."))
  }

  # --- re-validate IMMEDIATELY before writing ------------------------------
  # Between the checks above and the write, the session can die, another writer
  # can move `last_seq`, or a job can start. All three are re-checked here.
  hb2 <- ts_drive_read_ready()
  if (is.null(hb2) || !identical(.as_chr(hb2$session_token), token)) {
    return(.ts_tool_err("SESSION_MISMATCH",
                        "The session changed between validation and the write.",
                        "Re-read transcripto_drive_status and retry."))
  }
  if (!isTRUE(hb2$armed)) {
    return(.ts_tool_err("SESSION_NOT_ARMED",
                        "The session was disarmed between validation and the write.",
                        "Nothing was written."))
  }
  last2 <- suppressWarnings(as.integer(.as_chr(hb2$last_seq) %|NA|% 0L))
  if (seq <= last2) {
    return(.ts_tool_err(
      "SEQ_STALE",
      sprintf("another scenario was consumed meanwhile (last_seq is now %s).", last2),
      "Re-read the session and use a higher seq."))
  }
  if (!ts_drive_ready_fresh()) {
    return(.ts_tool_err("STALE_SESSION",
                        "The heartbeat went stale between validation and the write.",
                        "Nothing was written; re-read the session."))
  }
  res2 <- ts_drive_read_result()
  if (is.list(res2) && identical(.as_chr(res2$status), "running")) {
    return(.ts_tool_err("JOB_ALREADY_RUNNING",
                        "A job started between validation and the write.",
                        "Contract B refuses a second job; nothing was written."))
  }

  # --- the existing atomic writer ------------------------------------------
  wrote <- tryCatch(ts_drive_write_json(payload, ts_drive_path("scenario.json")),
                    error = function(e) FALSE)
  if (!isTRUE(wrote)) {
    return(.ts_tool_err(
      "SCENARIO_WRITE_FAILED",
      "The atomic write of tools/_drive/scenario.json did not land.",
      "Another process may be holding the file; retry, or check tools/check_writers.R."))
  }

  structured <- list(
    # `accepted` is the ONLY state this tool can honestly report: the scenario is
    # on disk and the app has not consumed it yet.
    state    = "accepted",
    accepted = TRUE,
    wrote    = TRUE,
    scenario_file = "scenario.json",      # basename only — never an absolute path
    seq      = seq,
    module   = module,
    action   = "run_pipeline",
    button   = btn,
    preserve_data = isTRUE(preserve_data),
    state_enum = list("accepted", "applied", "running", "done", "error", "invalid"),
    preconditions = list(
      session_present        = TRUE,
      protocol               = TRUE,
      identity               = TRUE,
      assertion_matched      = TRUE,
      heartbeat_fresh        = TRUE,
      armed                  = TRUE,
      job_not_busy           = TRUE,
      seq_monotonic          = TRUE,
      module_action_compatible = TRUE,
      # Named, so a reader never mistakes "we checked" for "everything is fine".
      delegated_to_app = c("the module's readiness guard",
                           "whether the button is bound",
                           "the job lifecycle, and Contract B against in-process state")
    ),
    protocol_acknowledgement = list(
      written = TRUE,
      ack_seq = NULL,
      note = paste0("the app acknowledges on its next poll tick (~800 ms); ",
                    "this tool does not wait and cannot observe the ack")
    ),
    # PROTOCOL and BUSINESS stay separate objects, exactly as in M3a.
    business_state = list(
      completion_verified = FALSE,
      status = NULL,
      note = paste0("`accepted` is a WRITE acknowledgement, never a business outcome. The run's ",
                    "own state (applied | running | done | error | invalid) is published later in ",
                    "result.json and must be read with transcripto_drive_read_result or ",
                    "transcripto_drive_snapshot.")
    ),
    session = list(
      pid                 = pid,
      started_at          = .ts_clean(started, 40L),
      viewer              = .ts_clean(.as_chr(hb$viewer) %|NA|% "unknown", 40L),
      heartbeat_age_s     = if (is.finite(age)) round(age, 1) else NA_real_,
      heartbeat_timeout_s = ts_drive_hb_timeout(),
      armed               = TRUE,
      session_token       = "<redacted>"
    ),
    pinned = list(session_id = !is.null(expect$session_id),
                  pid = !is.null(expect$pid),
                  started_at = !is.null(expect$started_at)),
    redaction = list(
      policy = "run-v1",
      payload_echoed = FALSE,
      limits = list(max_payload_bytes = TS_MCP_MAX_PAYLOAD_BYTES),
      # Actions that exist in the protocol but are NOT reachable from M3c.
      not_reachable = c("import_file", "snapshot", "reset_module", "set_inputs", "wait")
    )
  )
  .ts_tool_ok(structured, sprintf(
    "drive run: seq=%s module=%s button=%s accepted (NOT applied, NOT done)",
    seq, module, btn))
}

# --- 7g. M4 — long-job observation: `transcripto_drive_wait` -----------------
#
# WHAT M4 IS.
#   ONE tool that TRACKS an already-authorised `run_pipeline`. It adds no action,
#   no module, no button and no write capability, EXCEPT one post-terminal
#   `snapshot` scenario taken on explicit request (`observe = TRUE`) — which is
#   the protocol's OWN fresh-observation mechanism.
#
# THE MEASURED CONSTRAINT THAT GOVERNS EVERYTHING (drive README, "RESIDUAL").
#   A long job is SYNCHRONOUS: it blocks the Shiny event loop, so while it runs
#   NO tick executes, the heartbeat stops BY DESIGN, and nothing can be
#   consumed. Therefore:
#     * progress observation DURING a job is impossible;
#     * a fresh snapshot during a job is BOTH useless (the loop is blocked) AND
#       dangerous — the tick resolves a pending terminal BEFORE consuming
#       anything, so a scenario queued mid-job is consumed on the NEXT beat and
#       can overwrite the very terminal the waiter is looking for.
#   M4 therefore NEVER writes while a run is in flight.
#
# `timeout` MEANS ONLY THAT THIS CALL ENDED. It is never a claim that the job
# failed or finished. `business_state.completion_verified` stays FALSE: no
# application-level completion contract exists, and inferring one from a
# protocol `done` is the exact lie M1–M3c exist to prevent.

TS_MCP_WAIT_DEFAULT_S  <- 120L
TS_MCP_WAIT_MAX_S      <- 600L
TS_MCP_WAIT_DEFAULT_MS <- 500L
TS_MCP_WAIT_MIN_MS     <- 200L
TS_MCP_WAIT_MAX_MS     <- 5000L
TS_MCP_WAIT_STALL      <- 3L
TS_MCP_WAIT_TIMELINE   <- 8L
TS_MCP_OBSERVE_S       <- 60L

# The states this tool can report. `stale` is a FLAG (orthogonal to the protocol
# state), documented as such in `state_semantics` — never confused with a
# terminal state.
TS_MCP_WAIT_STATES <- c("accepted", "applied", "running", "done", "error",
                        "invalid", "ignored", "timeout", "stale", "session_lost")
TS_MCP_WAIT_FIELDS <- c("seq", "module", "timeout_s", "poll_ms", "observe", "expect")
TS_MCP_WAIT_EXPECT_FIELDS <- c("session_id", "pid", "started_at")

.ts_wait_integer <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) &&
    x == trunc(x) && x <= .Machine$integer.max
}

.ts_wait_result <- function(started_at) {
  res <- ts_drive_read_result()
  if (is.null(res)) return(list(ok = TRUE, result = NULL))
  proto <- .as_chr(res$protocol)
  if (is.na(proto) || !identical(proto, TS_DRIVE_PROTOCOL)) {
    return(list(ok = FALSE, error = .ts_tool_err(
      "INVALID_PROTOCOL",
      sprintf("result.json declares protocol '%s'; expected '%s'.",
              .ts_clean(proto %|NA|% "(absent)"), TS_DRIVE_PROTOCOL),
      "The verdict was written by a different build.")))
  }
  started <- .ts_parse_iso(started_at)
  if (is.na(started)) {
    return(list(ok = FALSE, error = .ts_tool_err(
      "READ_FAILED",
      "The live session's started_at is not parseable, so result chronology cannot be checked.",
      "A verdict cannot be attributed to an unidentifiable session.")))
  }
  applied <- .ts_parse_iso(res$applied_at)
  if (is.na(applied)) {
    return(list(ok = FALSE, error = .ts_tool_err(
      "READ_FAILED",
      "result.json has no parseable applied_at, so its session cannot be established.",
      "A verdict without a timestamp cannot be attributed to the live session.")))
  }
  if (applied < started) {
    return(list(ok = FALSE, error = .ts_tool_err(
      "RESULT_SESSION_MISMATCH",
      "result.json predates the live session: it is a leftover verdict from a previous session.",
      "Compare (pid, started_at) before trusting any result; ack_seq alone is not evidence.")))
  }
  list(ok = TRUE, result = res)
}

#' An identity for one verdict, so an UNCHANGED re-read is recognisable.
.ts_wait_fingerprint <- function(res) {
  if (!is.list(res)) return("<none>")
  sprintf("%s|%s|%s",
          .as_chr(res$ack_seq) %|NA|% "NA",
          .as_chr(res$status) %|NA|% "NA",
          .as_chr(res$applied_at) %|NA|% "NA")
}

.ts_wait_terminal <- function(status) {
  if (length(status) != 1L || is.na(status)) return(FALSE)
  tryCatch(isTRUE(ts_drive_status_terminal(status)), error = function(e) FALSE)
}

#' ONE post-terminal fresh snapshot — the protocol's fresh-observation mechanism.
#'
#' Issued ONLY after a terminal verdict, so it can never overwrite the terminal
#' the caller already received. `last_seq` is re-read IMMEDIATELY before the
#' write, so a concurrent writer yields SEQ_STALE instead of a collision.
#' @return list(ok, code, seq, projection, status, ack_seq)
.ts_wait_observe <- function(module, token, run_seq, started_at) {
  hb <- ts_drive_read_ready()
  if (is.null(hb) || !identical(.as_chr(hb$session_token), token)) {
    return(list(ok = FALSE, code = "SESSION_MISMATCH", wrote = FALSE))
  }
  if (!isTRUE(hb$armed)) {
    return(list(ok = FALSE, code = "SESSION_NOT_ARMED", wrote = FALSE))
  }
  last_seq <- suppressWarnings(as.integer(.as_chr(hb$last_seq) %|NA|% 0L))
  obs_seq <- max(last_seq, suppressWarnings(as.integer(run_seq)) %|NA|% 0L) + 1L

  hb2 <- ts_drive_read_ready()
  if (is.null(hb2) || !identical(.as_chr(hb2$session_token), token)) {
    return(list(ok = FALSE, code = "SESSION_MISMATCH", wrote = FALSE))
  }
  last2 <- suppressWarnings(as.integer(.as_chr(hb2$last_seq) %|NA|% 0L))
  if (obs_seq <= last2) return(list(ok = FALSE, code = "SEQ_STALE", wrote = FALSE))

  payload <- list(protocol = TS_DRIVE_PROTOCOL, seq = obs_seq, session_token = token,
                  module = module, action = "snapshot", preserve_data = TRUE)
  wrote <- tryCatch(ts_drive_write_json(payload, ts_drive_path("scenario.json")),
                    error = function(e) FALSE)
  if (!isTRUE(wrote)) return(list(ok = FALSE, code = "SCENARIO_WRITE_FAILED", wrote = FALSE))

  deadline <- Sys.time() + TS_MCP_OBSERVE_S
  repeat {
    if (Sys.time() >= deadline) {
      return(list(ok = FALSE, code = "OBSERVE_FAILED", wrote = TRUE))
    }
    vr <- .ts_wait_result(started_at)
    if (!isTRUE(vr$ok)) {
      code <- "READ_FAILED"
      sc <- vr$error$structuredContent
      if (is.list(sc)) {
        candidate <- .as_chr(sc$code)
        if (!is.na(candidate)) code <- candidate
      }
      return(list(ok = FALSE, code = code, wrote = TRUE))
    }
    res <- vr$result
    if (is.list(res)) {
      ack <- suppressWarnings(as.integer(.as_chr(res$ack_seq) %|NA|% NA_integer_))
      st <- .as_chr(res$status)
      if (!is.na(ack) && identical(ack, obs_seq) && .ts_wait_terminal(st)) {
        return(list(ok = TRUE, code = NULL, seq = obs_seq, status = st, ack_seq = ack,
                    projection = .ts_project_snapshot(res$snapshot), wrote = TRUE))
      }
    }
    Sys.sleep(TS_MCP_WAIT_DEFAULT_MS / 1000)
  }
}

.ts_tool_wait <- function(seq, module, timeout_s, poll_ms, observe, expect = NULL) {
  # The SAME non-secret assertion as M3b/M3c.
  a <- .ts_session_assert(expect, require_assertion = TRUE, allow_stale = TRUE)
  if (!isTRUE(a$ok)) return(a$error)
  token <- a$token
  run_seq <- suppressWarnings(as.integer(seq))

  t0 <- Sys.time()
  deadline <- t0 + timeout_s
  interval <- poll_ms / 1000
  state <- "accepted"
  timeline <- list()
  unchanged <- 0L
  last_fp <- NULL
  last_change_at <- t0
  polls <- 0L
  session_lost <- FALSE
  last_res <- NULL
  last_status <- NA_character_
  last_ack <- NA_integer_
  last_age <- NA_real_

  repeat {
    polls <- polls + 1L

    # --- session identity: cheap, and the ONLY liveness check during a job ----
    hb_now <- ts_drive_read_ready()
    if (is.null(hb_now) || !identical(.as_chr(hb_now$session_token), token)) {
      session_lost <- TRUE
      break
    }
    last_seq_now <- suppressWarnings(as.integer(.as_chr(hb_now$last_seq) %|NA|% 0L))
    if (last_seq_now > run_seq) {
      return(.ts_tool_err(
        "SEQ_STALE",
        sprintf("seq %s is behind the live session's accepted sequence window (last_seq %s).",
                run_seq, last_seq_now),
        "Re-read the session and wait for the run sequence it is currently tracking."))
    }

    vr <- .ts_wait_result(a$started)
    if (!isTRUE(vr$ok)) return(vr$error)
    res <- vr$result
    last_res <- res
    fp <- .ts_wait_fingerprint(res)
    if (!identical(fp, last_fp)) {
      last_fp <- fp
      last_change_at <- Sys.time()
      unchanged <- 0L
      if (length(timeline) < TS_MCP_WAIT_TIMELINE) {
        timeline[[length(timeline) + 1L]] <- list(
          at_age_s = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1),
          ack_seq = if (is.list(res)) suppressWarnings(as.integer(.as_chr(res$ack_seq) %|NA|% NA_integer_)) else NA_integer_,
          status = if (is.list(res)) .ts_clean(.as_chr(res$status) %|NA|% "", 40L) else "")
      }
    } else {
      unchanged <- unchanged + 1L
    }

    last_status <- if (is.list(res)) .as_chr(res$status) else NA_character_
    last_ack <- if (is.list(res)) suppressWarnings(as.integer(.as_chr(res$ack_seq) %|NA|% NA_integer_)) else NA_integer_
    applied_at <- if (is.list(res)) .ts_parse_iso(res$applied_at) else as.POSIXct(NA)
    last_age <- if (inherits(applied_at, "POSIXct") && !is.na(applied_at)) {
      as.numeric(difftime(Sys.time(), applied_at, units = "secs"))
    } else NA_real_

    # --- classify OUR seq ----------------------------------------------------
    if (last_seq_now >= run_seq && !is.na(last_ack) && identical(last_ack, run_seq)) {
      state <- if (identical(last_status, "running")) "running"
               else if (identical(last_status, "applied")) "applied"
               else if (identical(last_status, "done")) "done"
               else if (identical(last_status, "error")) "error"
               else if (identical(last_status, "invalid")) "invalid"
               else if (identical(last_status, "ignored")) "ignored"
               # An UNKNOWN status is NOT terminal (drive README §4): keep waiting.
               else "accepted"
    } else {
      state <- "accepted"          # our scenario is not on the wire yet
    }
    if (.ts_wait_terminal(state)) break

    # --- liveness, WITHOUT punishing a job that must stall the heartbeat -----
    # A stalled heartbeat during `running` is the documented RESIDUAL, not a
    # fault: it never aborts the wait. It is only a signal when the session is
    # genuinely GONE (stale AND the pid is dead AND we are not the running seq).
    if (!identical(state, "running") && !ts_drive_ready_fresh()) {
      if (identical(.ts_pid_alive(.as_chr(hb_now$pid)), FALSE)) {
        session_lost <- TRUE
        break
      }
    }

    remaining <- as.numeric(difftime(deadline, Sys.time(), units = "secs"))
    if (remaining <= 0) { state <- "timeout"; break }
    if (unchanged >= TS_MCP_WAIT_STALL) {
      interval <- min(interval * 2, max(poll_ms / 1000 * 4, 2))
    }
    Sys.sleep(min(interval, remaining))
  }

  if (isTRUE(session_lost)) state <- "session_lost"

  waited_s <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  terminal <- state %in% c("done", "error", "invalid", "ignored", "timeout", "session_lost")

  # --- staleness, on the M3a rule (non-terminal OR older than the timeout) ---
  stale_reasons <- character(0)
  if (!.ts_wait_terminal(last_status) && !isTRUE(session_lost)) {
    stale_reasons <- c(stale_reasons, sprintf(
      "the verdict is not terminal (status '%s')", .ts_clean(last_status %|NA|% "", 40L)))
  }
  if (is.finite(last_age) && last_age > ts_drive_hb_timeout()) {
    stale_reasons <- c(stale_reasons, sprintf(
      "the verdict is %.1f s old, beyond the %s s heartbeat timeout",
      last_age, ts_drive_hb_timeout()))
  }
  stale <- length(stale_reasons) > 0L

  # --- the ONE optional post-terminal fresh observation ---------------------
  obs <- NULL; obs_code <- NULL; writes <- 0L
  if (isTRUE(observe)) {
    if (!state %in% c("done", "error", "invalid", "ignored")) {
      # EXPLICIT condition: never write before a terminal protocol state.
      obs_code <- "OBSERVE_SKIPPED_NO_TERMINAL"
    } else {
      o <- .ts_wait_observe(module, token, run_seq, a$started)
      if (isTRUE(o$wrote)) writes <- 1L
      if (isTRUE(o$ok)) obs <- o else obs_code <- o$code
    }
  }

  structured <- list(
    state    = state,
    terminal = terminal,
    state_enum = as.list(TS_MCP_WAIT_STATES),
    state_semantics = list(
      accepted  = "the scenario is not on the wire yet; nothing has been acknowledged",
      applied   = "inputs were set; the pipeline has NOT finished",
      running   = "a job is in flight; the heartbeat stalling here is BY DESIGN",
      done      = "protocol terminal for this seq; NOT a claim the analysis finished",
      error     = "protocol terminal, with a cause",
      invalid   = "the app refused the scenario; nothing ran",
      ignored   = "the app ignored it (stale protocol or seq)",
      timeout   = "THIS CALL ended; it says nothing about the job",
      stale     = "reported as observation.stale, orthogonal to state",
      session_lost = "the session disappeared or its token changed"),
    seq    = run_seq,
    module = module,
    waited_s = waited_s,
    polls    = polls,
    observation = list(
      ack_seq            = last_ack,
      status             = .ts_clean(last_status %|NA|% "", 40L),
      age_s              = if (is.finite(last_age)) round(last_age, 1) else NULL,
      changed            = unchanged == 0L,
      unchanged_polls    = unchanged,
      last_change_age_s  = round(as.numeric(difftime(Sys.time(), last_change_at, units = "secs")), 1),
      stalled            = unchanged >= TS_MCP_WAIT_STALL,
      stale              = stale,
      stale_reason       = if (length(stale_reasons)) paste(stale_reasons, collapse = "; ") else NULL,
      heartbeat_stalled_by_job = identical(last_status, "running"),
      note = paste0("an UNCHANGED verdict is classified as stalled, never as progress; ",
                    "during a long job the verdict is static BY DESIGN")
    ),
    timeline = timeline,
    protocol_state = list(
      status   = .ts_clean(last_status %|NA|% "", 40L),
      terminal = .ts_wait_terminal(last_status)
    ),
    business_state = list(
      # NO application-level completion contract exists, so this stays FALSE.
      completion_verified = FALSE,
      source = if (!is.null(obs)) "post-terminal snapshot (module-published state)" else NULL,
      observed_after_terminal = if (!is.null(obs)) TRUE else NULL,
      observe_seq = if (!is.null(obs)) obs$seq else NULL,
      observe_status = if (!is.null(obs)) .ts_clean(obs$status, 40L) else NULL,
      modules = if (!is.null(obs)) obs$projection$modules else list(),
      note = paste0("M4 never claims an analysis finished: a protocol `done` means the token moved ",
                    "or the module declared its job over. With observe=TRUE the module's own ",
                    "published state is returned for the caller to judge.")
    ),
    observe_error = obs_code,
    session = list(
      session_id          = .ts_session_id(.as_chr(a$hb$pid), .as_chr(a$hb$started_at), token),
      viewer              = .ts_clean(.as_chr(a$hb$viewer) %|NA|% "unknown", 40L),
      heartbeat_age_s     = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1),
      heartbeat_timeout_s = ts_drive_hb_timeout(),
      session_token       = "<redacted>"
    ),
    redaction = list(
      policy = "wait-v1",
      limits = list(timeout_max_s = TS_MCP_WAIT_MAX_S,
                    poll_ms_min = TS_MCP_WAIT_MIN_MS,
                    poll_ms_max = TS_MCP_WAIT_MAX_MS,
                    timeline_max = TS_MCP_WAIT_TIMELINE),
      writes = writes,
      note = "at most ONE write (the post-terminal snapshot); never during a running job"
    )
  )
  .ts_tool_ok(structured, sprintf(
    "drive wait: seq=%s state=%s terminal=%s waited=%.1fs polls=%s",
    run_seq, state, terminal, waited_s, polls))
}

# --- 8. Tool registry and dispatch -----------------------------------------

.ts_tools <- function() {
  empty_obj <- stats::setNames(list(), character(0))
  list(
    # S2 — deliberately the SHORTEST tool description in the inventory, because
    # the tool's whole contract is that there is nothing to choose. Every extra
    # parameter here would be a decision handed to a remote caller, and the
    # artefact is written by the app into a directory the app controls.
    list(name = "transcripto_drive_export",
         description = paste0(
           "Export the ONE artefact the protocol exposes: the Spatial hotspot ",
           "table this session has already produced. Takes no handler, no ",
           "outputId, no destination, no filename and no format - the app ",
           "chooses all five, writes into an application-controlled bounded ",
           "temporary directory, and returns a redacted descriptor (format, ",
           "file basename, bytes, n_rows, n_cols, n_sig, column names). No ",
           "sample name, no absolute path and no row data. Refuses a session ",
           "with no result (INVALID), and refuses any field beyond `seq` and ",
           "`expect`."),
         inputSchema = list(
           type = "object",
           properties = list(
             seq = list(
               type = "integer",
               minimum = 1,
               description = paste0(
                 "Protocol counter, must be greater than the session's last_seq. ",
                 "Required. It is a sequence number, not a choice about the file.")),
             expect = list(
               type = "object",
               description = paste0(
                 "Required pin. Each field provided must match the live session ",
                 "EXACTLY, otherwise SESSION_MISMATCH. An export writes into the ",
                 "live session's own temporary directory, so it must name it."),
               properties = list(
                 pid = list(type = "integer"),
                 started_at = list(type = "string"),
                 session_token = list(type = "string"),
                 session_id = list(type = "string")),
               additionalProperties = FALSE)),
           required = c("seq", "expect"),
           additionalProperties = FALSE)),
    list(name = "transcripto_drive_status",
         description = paste0(
           "Read-only. Report the live TranscriptoShiny drive session: protocol, ",
           "viewer mode, pid, started_at, heartbeat freshness and job state. ",
           "Returns a domain error code (NO_SESSION, STALE_SESSION, ",
           "INVALID_PROTOCOL, READ_FAILED) in structuredContent when the session ",
           "is absent, stale or untrustworthy. Never writes and never runs code."),
         inputSchema = list(type = "object", properties = empty_obj,
                            additionalProperties = FALSE)),
    list(name = "transcripto_drive_read_result",
         description = paste0(
           "Read-only. Read the drive verdict (tools/_drive/result.json) of the ",
           "live session: status, ack_seq, applied_at, active_module, errors, ",
           "warnings and the object snapshot. The verdict is only trusted when ",
           "its session identity matches the live session; a leftover verdict ",
           "yields RESULT_SESSION_MISMATCH. Never writes and never runs code."),
         inputSchema = list(type = "object", properties = empty_obj,
                            additionalProperties = FALSE)),
    list(name = "transcripto_drive_snapshot",
         description = paste0(
           "Read-only and PASSIVE. Report the object snapshot published by the live ",
           "session's LAST verdict (tools/_drive/result.json), through a closed, ",
           "bounded projection: has_data, object_class, n_genes, n_samples, ",
           "error_state and per-module counters. Sample names, gene names, matrices, ",
           "absolute paths, the session token and raw logs are NEVER published. ",
           "Protocol state is reported separately from business state; the answer ",
           "marks whether the observation is stale and NEVER claims an analysis ",
           "finished. This tool issues no scenario and does not wait: it observes ",
           "what the app already published. Refusals: NO_SESSION, STALE_SESSION, ",
           "INVALID_PROTOCOL, READ_FAILED, RESULT_SESSION_MISMATCH, SESSION_MISMATCH."),
         inputSchema = list(
           type = "object",
           properties = list(
             expect = list(
               type = "object",
               description = paste0(
                 "Optional pin. If `session_id` is given it must match the live ",
                 "session's DERIVED id exactly, otherwise SESSION_MISMATCH. Use it to ",
                 "observe the session you inspected and fail loudly if another has ",
                 "taken over."),
               properties = list(session_id = list(type = "string")),
               additionalProperties = FALSE)),
           additionalProperties = FALSE)),
    list(name = "transcripto_drive_set_inputs",
         description = paste0(
           "CONTROLLED WRITE. Set allowlisted Shiny inputs for ONE module by writing ",
           "ONE tools/_drive/scenario.json with action=set_inputs, then stop. Values ",
           "are validated HERE (type, enum, bounds, length, payload size) BEFORE the ",
           "write, because the app accepts an out-of-range select value silently. ",
           "Requires a live, ARMED session and a non-wildcard `expect.session_token`. ",
           "Refuses unknown modules and inputs, module/input mismatches, stale or ",
           "replayed seq, and an oversized payload. Never clicks a button, never runs ",
           "an action, never waits, never touches a Shiny input directly, and never ",
           "echoes a value back. Refusals: NO_SESSION, STALE_SESSION, INVALID_PROTOCOL, ",
           "READ_FAILED, AMBIGUOUS_SESSION, SESSION_MISMATCH, ",
           "SESSION_ASSERTION_REQUIRED, SESSION_NOT_ARMED, MODULE_NOT_ALLOWED, ",
           "INPUT_NOT_ALLOWED, INPUT_MODULE_MISMATCH, VALUE_REFUSED, PAYLOAD_REFUSED, ",
           "PAYLOAD_TOO_LARGE, SEQ_STALE, SCENARIO_WRITE_FAILED."),
         inputSchema = list(
           type = "object",
           properties = list(
             seq = list(
               type = "integer", minimum = 1L,
               description = paste0(
                 "Monotonic scenario sequence. MUST be greater than the session's ",
                 "last_seq, otherwise SEQ_STALE.")),
             module = list(
                type = "string", enum = as.list(TS_MCP_SET_INPUT_MODULES),
                description = "The one module every input must belong to."),
             inputs = list(
               type = "object",
               description = paste0(
                 "inputId -> value. Every id must be exposed by M3b and owned by ",
                 "`module`; values are validated against a server-side schema.")),
             preserve_data = list(
               type = "boolean",
               description = "Defaults to true. False is refused by design in this grade."),
             expect = list(
               type = "object",
               description = paste0(
                 "REQUIRED. A write must NAME its session: `session_id` (the DERIVED ",
                 "id published by transcripto_drive_status) is mandatory and the ",
                 "wildcard '*' is never accepted. The raw token is never published."),
               properties = list(
                 session_id = list(type = "string"),
                 pid = list(type = "integer"),
                 started_at = list(type = "string")),
               required = list("session_id"),
               additionalProperties = FALSE)),
           required = list("seq", "module", "inputs", "expect"),
           additionalProperties = FALSE)),
    list(name = "transcripto_drive_run",
         description = paste0(
           "CONTROLLED WRITE. Start ONE allowlisted run action for ONE module by ",
           "writing ONE tools/_drive/scenario.json with action=run_pipeline, then ",
           "stop. Requires a live, ARMED session and a non-wildcard ",
           "`expect.session_id`. Refuses an unknown or ambiguous action, a button ",
           "belonging to another module, a busy session (Contract B, read from the ",
           "published verdict), a stale or replayed seq, and a non-monotonic seq. ",
           "Reports `accepted` and NOTHING more: the run's own state ",
           "(applied|running|done|error|invalid) is published later in result.json. ",
           "Never clicks a button, never evaluates R, never calls Shiny/Seurat/DESeq2 ",
           "or a pathway function, never waits. Refusals: NO_SESSION, STALE_SESSION, ",
           "INVALID_PROTOCOL, READ_FAILED, AMBIGUOUS_SESSION, SESSION_MISMATCH, ",
           "SESSION_ASSERTION_REQUIRED, SESSION_NOT_ARMED, MODULE_NOT_ALLOWED, ",
           "ACTION_NOT_ALLOWED, ACTION_AMBIGUOUS, BUTTON_MODULE_MISMATCH, ",
           "JOB_ALREADY_RUNNING, PAYLOAD_REFUSED, PAYLOAD_TOO_LARGE, SEQ_STALE, ",
           "SCENARIO_WRITE_FAILED."),
         inputSchema = list(
           type = "object",
           properties = list(
             seq = list(
               type = "integer", minimum = 1L,
               description = paste0(
                 "Monotonic scenario sequence. MUST be greater than the session's ",
                 "last_seq, otherwise SEQ_STALE.")),
             module = list(
               type = "string", enum = as.list(TS_DRIVE_MODULES),
               description = "The one module the run action belongs to."),
             button = list(
               type = "string",
               enum = as.list(unique(unlist(TS_MCP_RUN_BUTTONS))),
               description = paste0(
                 "The run action to fire. OPTIONAL only when the module exposes ",
                 "exactly one; with several, omitting it yields ACTION_AMBIGUOUS.")),
             preserve_data = list(type = "boolean"),
             expect = list(
               type = "object",
               description = paste0(
                 "REQUIRED. A write must NAME its session: `session_id` (the DERIVED ",
                 "id published by transcripto_drive_status) is mandatory and the ",
                 "wildcard '*' is never accepted. The raw token is never published."),
               properties = list(
                 session_id = list(type = "string"),
                 pid = list(type = "integer"),
                 started_at = list(type = "string")),
               required = list("session_id"),
               additionalProperties = FALSE)),
           required = list("seq", "module", "expect"),
           additionalProperties = FALSE)),
    list(name = "transcripto_drive_wait",
         description = paste0(
           "Bounded observation of ONE already-authorised run_pipeline seq. Polls ",
           "tools/_drive/result.json until ack_seq == seq reaches a TERMINAL status, ",
           "or timeout_s elapses. An UNCHANGED verdict is classified as stalled, ",
           "never as progress; during a long SYNCHRONOUS job the verdict is static ",
           "and the heartbeat stalls BY DESIGN, which never aborts the wait. ",
           "`timeout` means only that THIS CALL ended: never that the job failed or ",
           "finished. Requires a non-wildcard `expect.session_id`. With ",
           "observe=true, and ONLY after a terminal verdict, it writes exactly ONE ",
           "snapshot scenario and returns the module's published state; it never ",
           "writes while a run is in flight. `business_state.completion_verified` is ",
           "always false: no application-level completion contract exists. Refusals: ",
           "NO_SESSION, INVALID_PROTOCOL, READ_FAILED, SESSION_MISMATCH, ",
           "SESSION_ASSERTION_REQUIRED, SESSION_LOST, SEQ_STALE, SCENARIO_WRITE_FAILED, ",
           "OBSERVE_FAILED, OBSERVE_SKIPPED_NO_TERMINAL."),
         inputSchema = list(
           type = "object",
           properties = list(
             seq = list(type = "integer", minimum = 1L,
                        description = "The seq of the run_pipeline scenario to track."),
             module = list(type = "string", enum = as.list(TS_DRIVE_MODULES),
                           description = "The module the run action belonged to."),
             timeout_s = list(type = "integer", minimum = 1L,
                              maximum = TS_MCP_WAIT_MAX_S,
                              description = "Wall-clock bound for THIS call. Default 120."),
             poll_ms = list(type = "integer", minimum = TS_MCP_WAIT_MIN_MS,
                            maximum = TS_MCP_WAIT_MAX_MS,
                            description = "Poll interval; backs off while stalled. Default 500."),
             observe = list(type = "boolean",
                            description = paste0(
                              "Take exactly ONE fresh snapshot AFTER a terminal verdict and ",
                              "return the module's published state. Never during a running job.")),
             expect = list(
               type = "object",
               description = paste0(
                 "REQUIRED. `session_id` (the DERIVED id published by ",
                 "transcripto_drive_status) is mandatory; the wildcard '*' is never ",
                 "accepted and the raw token is never published."),
               properties = list(
                 session_id = list(type = "string"),
                 pid = list(type = "integer"),
                 started_at = list(type = "string")),
               required = list("session_id"),
               additionalProperties = FALSE)),
           required = list("seq", "module", "expect"),
           additionalProperties = FALSE)),
    list(name = "transcripto_drive_set_armed",
         description = paste0(
           "Controlled arm/disarm of the LIVE drive poller. Writes ONLY ",
           "tools/_drive/arm.json, atomically, using the live session's own token. ",
           "Validates protocol, session identity (pid, started_at, session_token), ",
           "heartbeat freshness and selected-session metadata before writing, and ",
           "refuses stale, mismatched, malformed or ambiguous sessions ",
           "(NO_SESSION, STALE_SESSION, INVALID_PROTOCOL, READ_FAILED, ",
           "AMBIGUOUS_SESSION, SESSION_MISMATCH, ARM_WRITE_FAILED). THE ARMING ",
           "BOOTSTRAP: because the poller beats only while ARMED, an unarmed ",
           "session's handshake goes stale within the heartbeat timeout; this is ",
           "the ONE tool that may act on a stale handshake, and only when the ",
           "caller pins the session identity (session_id, or pid + started_at) ",
           "and the pinned pid is alive — reported as rescued_stale = true. A ",
           "wrong pin still fails closed (SESSION_MISMATCH). Never accepts the ",
           "wildcard token, never waits, never mutates Shiny inputs, never runs ",
           "code."),
         inputSchema = list(
           type = "object",
           properties = list(
             armed = list(
               type = "boolean",
               description = "true = arm the poller, false = disarm it. Required."),
             expect = list(
               type = "object",
               description = paste0(
                 "Optional pin. Each field provided must match the live session ",
                 "EXACTLY, otherwise SESSION_MISMATCH. Use it to arm the session you ",
                 "inspected and fail loudly if another has taken over."),
               properties = list(
                 pid = list(type = "integer"),
                 started_at = list(type = "string"),
                 session_token = list(type = "string")),
               additionalProperties = FALSE)),
           required = list("armed"),
           additionalProperties = FALSE))
  )
}

.ts_supported_versions <- c("2024-11-05", "2025-06-18")
.ts_preferred_version <- "2025-06-18"

.ts_initialize <- function(params) {
  want <- if (is.list(params)) .as_chr(params$protocolVersion) else NA_character_
  ver <- if (!is.na(want) && want %in% .ts_supported_versions) want else .ts_preferred_version
  list(
    protocolVersion = ver,
    capabilities = list(tools = list(listChanged = FALSE)),
    serverInfo = list(name = "transcriptoshiny-drive", version = "0.6.0-m4"),
    instructions = paste0(
      "Access to a LIVE TranscriptoShiny drive session. Seven tools: three read-only ",
      "(status, verdict, passive snapshot), one controlled input write (set_inputs), ",
      "one controlled run (run, action=run_pipeline only), one bounded observation ",
      "(wait) and one controlled arm/disarm. Every write touches ONE file and is ",
      "acknowledged as a WRITE, never as a business outcome; `wait` never claims an ",
      "analysis finished. No code is executed, no session is selected and no ",
      "analysis is computed here.")
  )
}

.ts_dispatch <- function(req) {
  has_id <- "id" %in% names(req)
  id <- if (has_id) req$id else NULL
  method <- .as_chr(req$method)
  if (is.na(method)) {
    if (!has_id) return(NULL)
    return(.ts_error(id, -32600, "Invalid Request: 'method' must be one string."))
  }
  # Notifications never get a reply.
  if (startsWith(method, "notifications/")) return(NULL)

  if (identical(method, "initialize")) return(.ts_result(id, .ts_initialize(req$params)))
  # An MCP result envelope is an OBJECT: `{}`, not jsonlite's `[]` for list().
  if (identical(method, "ping")) return(.ts_result(id, .ts_empty_object()))
  if (identical(method, "tools/list")) return(.ts_result(id, list(tools = .ts_tools())))

  if (identical(method, "tools/call")) {
    params <- req$params
    nm <- if (is.list(params)) .as_chr(params$name) else NA_character_
    if (is.na(nm)) return(.ts_error(id, -32602, "Invalid params: 'name' must be one string."))
    if (identical(nm, "transcripto_drive_status")) return(.ts_result(id, .ts_tool_status()))
    if (identical(nm, "transcripto_drive_read_result")) {
      return(.ts_result(id, .ts_tool_read_result()))
    }
    if (identical(nm, "transcripto_drive_snapshot")) {
      args <- if (is.list(params$arguments)) params$arguments else list()
      ex <- args$expect
      if (!is.null(ex)) {
        if (!is.list(ex) || length(ex) == 0L) {
          return(.ts_error(id, -32602, "Invalid params: 'expect' must be a non-empty object."))
        }
        bad <- setdiff(names(ex), "session_id")
        if (length(bad)) {
          return(.ts_error(id, -32602, sprintf(
            "Invalid params: unknown 'expect' field(s): %s", paste(bad, collapse = ", "))))
        }
      }
      return(.ts_result(id, .ts_tool_snapshot(ex)))
    }
    if (identical(nm, "transcripto_drive_set_inputs")) {
      args <- if (is.list(params$arguments)) params$arguments else list()
      # PROTOCOL-level shape checks stay here: a malformed request is NOT a
      # domain error, and the two channels are never conflated.
      sq <- args$seq
      if (!is.numeric(sq) || length(sq) != 1L || is.na(sq)) {
        return(.ts_error(id, -32602, "Invalid params: 'seq' must be one integer."))
      }
      md <- args$module
      if (!is.character(md) || length(md) != 1L || is.na(md)) {
        return(.ts_error(id, -32602, "Invalid params: 'module' must be one string."))
      }
      ins <- args$inputs
      if (!is.list(ins)) {
        return(.ts_error(id, -32602, "Invalid params: 'inputs' must be an object."))
      }
      pd <- if (is.null(args$preserve_data)) TRUE else args$preserve_data
      if (!is.logical(pd) || length(pd) != 1L || is.na(pd)) {
        return(.ts_error(id, -32602, "Invalid params: 'preserve_data' must be true or false."))
      }
      ex <- args$expect
      if (!is.list(ex) || length(ex) == 0L) {
        return(.ts_error(id, -32602,
                         "Invalid params: 'expect' is required and must be a non-empty object."))
      }
      bad <- setdiff(names(ex), .ts_expect_fields)
      if (length(bad)) {
        return(.ts_error(id, -32602, sprintf(
          "Invalid params: unknown 'expect' field(s): %s", paste(bad, collapse = ", "))))
      }
      return(.ts_result(id, .ts_tool_set_inputs(sq, md, ins, pd, ex)))
    }
    if (identical(nm, "transcripto_drive_run")) {
      args <- if (is.list(params$arguments)) params$arguments else list()
      sq <- args$seq
      if (!is.numeric(sq) || length(sq) != 1L || is.na(sq)) {
        return(.ts_error(id, -32602, "Invalid params: 'seq' must be one integer."))
      }
      md <- args$module
      if (!is.character(md) || length(md) != 1L || is.na(md)) {
        return(.ts_error(id, -32602, "Invalid params: 'module' must be one string."))
      }
      bt <- args$button
      if (!is.null(bt) && (!is.character(bt) || length(bt) != 1L || is.na(bt))) {
        return(.ts_error(id, -32602, "Invalid params: 'button' must be one string."))
      }
      pd <- if (is.null(args$preserve_data)) TRUE else args$preserve_data
      if (!is.logical(pd) || length(pd) != 1L || is.na(pd)) {
        return(.ts_error(id, -32602, "Invalid params: 'preserve_data' must be true or false."))
      }
      ex <- args$expect
      if (!is.list(ex) || length(ex) == 0L) {
        return(.ts_error(id, -32602,
                         "Invalid params: 'expect' is required and must be a non-empty object."))
      }
      bad <- setdiff(names(ex), .ts_expect_fields)
      if (length(bad)) {
        return(.ts_error(id, -32602, sprintf(
          "Invalid params: unknown 'expect' field(s): %s", paste(bad, collapse = ", "))))
      }
      return(.ts_result(id, .ts_tool_run(sq, md, bt, pd, ex)))
    }
    if (identical(nm, "transcripto_drive_wait")) {
      args <- if (is.list(params$arguments)) params$arguments else list()
      bad <- setdiff(names(args), TS_MCP_WAIT_FIELDS)
      if (length(bad)) {
        return(.ts_error(id, -32602, sprintf(
          "Invalid params: unknown wait field(s): %s", paste(bad, collapse = ", "))))
      }
      sq <- args$seq
      if (!.ts_wait_integer(sq) || sq < 1) {
        return(.ts_error(id, -32602, "Invalid params: 'seq' must be one integer >= 1."))
      }
      md <- args$module
      if (!is.character(md) || length(md) != 1L || is.na(md) || !md %in% TS_DRIVE_MODULES) {
        return(.ts_error(id, -32602, "Invalid params: 'module' must be one allowed drive module."))
      }
      tmo <- if (is.null(args$timeout_s)) TS_MCP_WAIT_DEFAULT_S else args$timeout_s
      if (!.ts_wait_integer(tmo) || tmo < 1 || tmo > TS_MCP_WAIT_MAX_S) {
        return(.ts_error(id, -32602, sprintf(
          "Invalid params: 'timeout_s' must be an integer in [1, %d].", TS_MCP_WAIT_MAX_S)))
      }
      pms <- if (is.null(args$poll_ms)) TS_MCP_WAIT_DEFAULT_MS else args$poll_ms
      if (!.ts_wait_integer(pms) || pms < TS_MCP_WAIT_MIN_MS || pms > TS_MCP_WAIT_MAX_MS) {
        return(.ts_error(id, -32602, sprintf(
          "Invalid params: 'poll_ms' must be an integer in [%d, %d].",
          TS_MCP_WAIT_MIN_MS, TS_MCP_WAIT_MAX_MS)))
      }
      ob <- if (is.null(args$observe)) FALSE else args$observe
      if (!is.logical(ob) || length(ob) != 1L || is.na(ob)) {
        return(.ts_error(id, -32602, "Invalid params: 'observe' must be true or false."))
      }
      ex <- args$expect
      if (!is.list(ex) || length(ex) == 0L) {
        return(.ts_error(id, -32602,
                         "Invalid params: 'expect' is required and must be a non-empty object."))
      }
      bad <- setdiff(names(ex), TS_MCP_WAIT_EXPECT_FIELDS)
      if (length(bad)) {
        return(.ts_error(id, -32602, sprintf(
          "Invalid params: unknown 'expect' field(s): %s", paste(bad, collapse = ", "))))
      }
      return(.ts_result(id, .ts_tool_wait(as.integer(sq), md, as.integer(tmo),
                                          as.integer(pms), ob, ex)))
    }
    if (identical(nm, "transcripto_drive_set_armed")) {
      args <- if (is.list(params$arguments)) params$arguments else list()
      armed <- args$armed
      # Protocol-level validation stays here: a malformed request is NOT a domain error.
      if (is.null(armed) || !is.logical(armed) || length(armed) != 1L || is.na(armed)) {
        return(.ts_error(id, -32602, "Invalid params: 'armed' must be true or false."))
      }
      ex <- args$expect
      if (!is.null(ex)) {
        if (!is.list(ex) || length(ex) == 0L) {
          return(.ts_error(id, -32602, "Invalid params: 'expect' must be a non-empty object."))
        }
        bad <- setdiff(names(ex), .ts_expect_fields)
        if (length(bad)) {
          return(.ts_error(id, -32602, sprintf(
            "Invalid params: unknown 'expect' field(s): %s", paste(bad, collapse = ", "))))
        }
      }
      return(.ts_result(id, .ts_tool_set_armed(isTRUE(armed), ex)))
    }
    # S2 — the one export tool. Note what is NOT here: no handler, no outputId,
    # no destination, no filename, no format, no module. The only fields are the
    # protocol envelope. `additionalProperties = FALSE` in the schema and this
    # strict field check are the two halves of the same promise, and both are
    # needed: a schema a client ignores is not a boundary.
    if (identical(nm, "transcripto_drive_export")) {
      args <- if (is.list(params$arguments)) params$arguments else list()
      ex <- args$expect
      if (is.null(ex) || !is.list(ex) || length(ex) == 0L) {
        return(.ts_error(id, -32602,
          "Invalid params: 'expect' is required and must be a non-empty object: an export writes to the live session and must name it."))
      }
      bad <- setdiff(names(ex), .ts_expect_fields)
      if (length(bad)) {
        return(.ts_error(id, -32602, sprintf(
          "Invalid params: unknown 'expect' field(s): %s", paste(bad, collapse = ", "))))
      }
      unknown <- setdiff(names(args), c("seq", "expect"))
      if (length(unknown)) {
        return(.ts_error(id, -32602, sprintf(
          paste("Invalid params: transcripto_drive_export takes only `seq` and `expect`;",
                "unknown field(s): %s. The route, the destination, the filename and the",
                "format are the APP's choice and are not parameters."),
          paste(unknown, collapse = ", "))))
      }
      sq <- args$seq
      if (is.null(sq) || !is.numeric(sq)) {
        return(.ts_error(id, -32602, "Invalid params: 'seq' must be an integer >= 1."))
      }
      return(.ts_result(id, .ts_tool_export(sq, ex)))
    }
    return(.ts_error(id, -32602, sprintf("Unknown tool: %s", nm)))
  }

  if (!has_id) return(NULL)
  .ts_error(id, -32601, sprintf("Method not found: %s", method))
}

# --- 9. Main loop -----------------------------------------------------------

.ts_serve <- function() {
  con <- file("stdin", "rb")
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  repeat {
    msg <- .ts_read_message(con)
    if (is.null(msg)) break                        # clean EOF at a boundary -> exit 0
    if (isTRUE(msg$too_long)) break                # cannot re-frame -> controlled stop
    last <- isTRUE(msg$eof)                        # final line, no trailing LF
    text <- .ts_strip_cr(msg$text)

    # A blank line carries no message: ignore it rather than answer an error.
    if (nzchar(trimws(text))) {
      req <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE),
                      error = function(e) NULL)
      if (is.null(req) || !is.list(req)) {
        # Controlled protocol response, then RECOVER: the loop continues, so one
        # malformed line cannot take the session down.
        .ts_write_message(.ts_json(
          .ts_error(NULL, -32700, "Parse error: not a valid JSON-RPC message.")))
      } else {
        resp <- tryCatch(.ts_dispatch(req), error = function(e) {
          .ts_error(if ("id" %in% names(req)) req$id else NULL, -32603,
                    paste0("Internal error: ", .ts_clean(conditionMessage(e), 200L)))
        })
        if (!is.null(resp)) .ts_write_message(.ts_json(resp))
      }
    }
    if (last) break
  }
  invisible(0L)
}

# --- 1b. `--check`, SECOND phase --------------------------------------------
# Runs HERE, at the END, because it needs the drive files SOURCED (section 2)
# and the value schema DEFINED (section 7e). R defines top to bottom, so calling
# this from section 1 would be a "could not find function" error.
if ("--check" %in% .args) {
  drive_ok <- file.exists(file.path(.project_root, "R", "core", "drive_watcher.R")) &&
    file.exists(file.path(.project_root, "R", "core", "drive_allowlist.R"))
  .ts_stderr("  drive readers: ", if (drive_ok) "present" else "MISSING")
  .ts_schema_problems <- .ts_mcp_schema_problems()
  .ts_run_problems <- .ts_mcp_run_problems()
  .ts_stderr("  run actions  : ", paste(TS_MCP_RUN_ACTIONS, collapse = ", "),
             " over ", length(TS_MCP_RUN_BUTTONS), " modules / ",
             length(unique(unlist(TS_MCP_RUN_BUTTONS))), " buttons")
  .ts_stderr("  button map   : ",
             paste(sprintf("%s=%s", names(TS_MCP_RUN_BUTTONS),
                           vapply(TS_MCP_RUN_BUTTONS,
                                  function(x) paste(x, collapse = "|"),
                                  character(1))),
                   collapse = ", "))
  .ts_stderr("  run check    : ",
             if (length(.ts_run_problems)) {
               paste0(length(.ts_run_problems), " PROBLEM(S): ",
                      paste(.ts_run_problems, collapse = "; "))
             } else {
               "OK (every button belongs to its module in the app's own table)"
             })
  .ts_stderr("  input schema : ", length(TS_MCP_INPUT_SCHEMA), " exposed, ",
             length(.ts_mcp_not_exposed()), " allowlisted but NOT exposed")
  .ts_stderr("  schema check : ",
             if (length(.ts_schema_problems)) {
               paste0(length(.ts_schema_problems), " PROBLEM(S): ",
                      paste(.ts_schema_problems, collapse = "; "))
             } else {
               "OK (every type matches the app's own widget kind)"
             })
  quit(status = if (.ts_have_jsonlite && drive_ok && !length(.ts_schema_problems) &&
                    !length(.ts_run_problems)) 0L else 1L, save = "no")
}

.ts_stderr("mcp_server: serving 7 drive tools (3 read-only + set_inputs + run + wait + arm/disarm) (stdio, NDJSON, native JSON-RPC)")
.ts_serve()

# =============================================================================
# test-drive-session-contract-fix.R — Handshake port + stale-arm lifecycle
# =============================================================================
# Falsification harness for the 2026-10-26 live incident (3 boots, one rsession):
#   - `ready.json` advertised 58190, then 53214, then 57068 — every one a dead
#     port — while the app actually served on 7346 the whole time. Root cause:
#     `ts_drive_write_ready()` wrote `session$request$REMOTE_PORT`, the CLIENT's
#     own ephemeral TCP source port for one request, which is never a
#     connectable endpoint and changes per connection.
#   - a leftover `arm.json` (`armed: true`, token of a dead session) survived
#     every boot, so every new session read "token mismatch" and the operator's
#     panel offered a takeover of a ghost (same family as the MEASURED
#     2026-10-24 incident).
#
# This file pins the CONTRACT the fix must satisfy; §1 is RED on the
# pre-fix code by construction (REMOTE_PORT wins there). Hermetic like
# test-drive-watcher.R: every path resolves from a temp boot root, the real
# `tools/_drive/` is never touched. No Shiny is booted here — the wiring
# assertions are text-level (§3.6), the end-to-end proof runs on a live
# `shiny::runApp()` (scratch harness, not committed).
# =============================================================================

# Same loader contract as test-drive-watcher.R: the allowlist first (it owns
# TS_DRIVE_PROTOCOL / TS_DRIVE_DIR), then the logger (six error handlers in the
# watcher call ts_log_swallow()), then the watcher itself.
.drvfix_source <- function() {
  source_project_file("tools/pkg_skeleton/R/drive_allowlist.R")
  source_project_file("tools/pkg_skeleton/R/error_log.R")
  source_project_file("tools/pkg_skeleton/R/drive_watcher.R")
}
.drvfix_source()

.drvfix_root <- function() {
  root <- file.path(tempdir(), paste0("tsdrvfix-", as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(root, "tools", "_drive"), recursive = TRUE, showWarnings = FALSE)
  ts_drive_boot(root)
  ts_drive_clear_write_error()
  root
}

# A minimal stand-in for a real Shiny session: `ts_drive_write_ready()` only
# reads `session$request$HTTP_HOST` / `session$request$REMOTE_PORT` and, as a
# fallback, `session$clientData$url_port`. A plain list reproduces all three
# shapes — including the failure modes (missing fields, non-function members).
.drvfix_session <- function(http_host = NULL, remote_port = NULL, url_port = NULL) {
  req <- list()
  if (!is.null(remote_port)) req$REMOTE_PORT <- remote_port
  if (!is.null(http_host)) req$HTTP_HOST <- http_host
  s <- list(request = req)
  if (!is.null(url_port)) s$clientData <- list(url_port = url_port)
  s
}

.drvfix_arm_write <- function(token, armed, extra = list()) {
  payload <- list(protocol = TS_DRIVE_PROTOCOL, token = token, armed = armed)
  payload <- utils::modifyList(payload, extra)
  ts_drive_write_json(payload, ts_drive_path("arm.json"))
}

# =============================================================================
# §1 — The port contract: what an agent dials must be what a client reached.
# =============================================================================

test_that("§1.1 ready.json port is the HTTP_HOST the client reached, NEVER REMOTE_PORT", {
  .drvfix_root()
  s <- .drvfix_session(http_host = "127.0.0.1:7346", remote_port = "53214")
  ts_drive_write_ready(s, "tokAAA1", armed = FALSE, started_at = "2026-10-26T22:00:00Z", hb_n = 0L)
  rd <- ts_drive_read_ready()
  expect_identical(as.character(rd$port), "7346")
  expect_false(identical(as.character(rd$port), "53214"))
})

test_that("§1.2 without HTTP_HOST, the clientData url_port fallback answers", {
  .drvfix_root()
  s <- .drvfix_session(remote_port = "53214", url_port = "7347")
  ts_drive_write_ready(s, "tokAAA2", armed = FALSE, started_at = "2026-10-26T22:00:00Z", hb_n = 0L)
  rd <- ts_drive_read_ready()
  expect_identical(as.character(rd$port), "7347")
})

test_that("§1.3 with neither source, the same session keeps the port it reached before", {
  .drvfix_root()
  s1 <- .drvfix_session(http_host = "127.0.0.1:7346", remote_port = "53214")
  ts_drive_write_ready(s1, "tokAAA3", armed = FALSE, started_at = "2026-10-26T22:00:00Z", hb_n = 0L)
  s2 <- .drvfix_session(remote_port = "59999")  # later tick: no Host header exposed
  ts_drive_write_ready(s2, "tokAAA3", armed = FALSE, started_at = "2026-10-26T22:00:00Z", hb_n = 4L)
  rd <- ts_drive_read_ready()
  expect_identical(as.character(rd$port), "7346")
})

test_that("§1.4 a brand-new session with no reachable port publishes NULL, not a lie", {
  .drvfix_root()
  s <- .drvfix_session(remote_port = "53214")
  ts_drive_write_ready(s, "tokAAA4", armed = FALSE, started_at = "2026-10-26T22:00:00Z", hb_n = 0L)
  rd <- ts_drive_read_ready()
  expect_true(is.null(rd$port) || !nzchar(as.character(rd$port)))
})

# =============================================================================
# §2 — Token churn: the heartbeat monotonicity contract is untouched by the
#      port fix (same token continues hb_n/started_at; a new token restarts).
# =============================================================================

test_that("§2.1 same token continues the heartbeat; a new token restarts it", {
  .drvfix_root()
  ts_drive_write_ready(.drvfix_session(http_host = "127.0.0.1:7346"), "tokBBB1",
                       armed = FALSE, started_at = "2026-10-26T22:00:00Z", hb_n = 41L)
  rd1 <- ts_drive_read_ready()
  expect_identical(as.integer(rd1$hb_n), 41L)
  expect_identical(as.character(rd1$started_at), "2026-10-26T22:00:00Z")

  ts_drive_write_ready(.drvfix_session(http_host = "127.0.0.1:7346"), "tokBBB1",
                       armed = FALSE, started_at = "2026-10-26T22:00:00Z", hb_n = 42L)
  expect_identical(as.integer(ts_drive_read_ready()$hb_n), 42L)

  ts_drive_write_ready(.drvfix_session(http_host = "127.0.0.1:7347"), "tokBBB2",
                       armed = FALSE, hb_n = 0L)
  rd3 <- ts_drive_read_ready()
  expect_identical(as.character(rd3$session_token), "tokBBB2")
  expect_identical(as.integer(rd3$hb_n), 0L)
  expect_false(identical(as.character(rd3$started_at), "2026-10-26T22:00:00Z"))
})

# =============================================================================
# §3 — Stale-arm reap: an `armed: true` from a session nobody holds dies at
#      attach. Wildcards and the current session's arm are not ours to reap.
# =============================================================================

test_that("§3.1 a stale armed:true from a dead token is disarmed in place", {
  .drvfix_root()
  .drvfix_arm_write("deadbeef01", TRUE)
  res <- ts_drive_reap_stale_arm("newtoken01")
  expect_true(isTRUE(attr(res, "written")))
  arm <- ts_drive_read_json(ts_drive_path("arm.json"))
  expect_false(isTRUE(arm$armed))
  expect_true(isTRUE(arm$reaped))
  expect_identical(as.character(arm$token), "deadbeef01")
  expect_identical(as.character(arm$reaped_for), "newtoken01")
})

test_that("§3.2 the wildcard arm belongs to the launcher and is never reaped", {
  .drvfix_root()
  .drvfix_arm_write("*", TRUE)
  ts_drive_reap_stale_arm("newtoken02")
  arm <- ts_drive_read_json(ts_drive_path("arm.json"))
  expect_true(isTRUE(arm$armed))
  expect_null(arm$reaped)
})

test_that("§3.3 the CURRENT session's arm is never reaped", {
  .drvfix_root()
  .drvfix_arm_write("newtoken03", TRUE)
  ts_drive_reap_stale_arm("newtoken03")
  arm <- ts_drive_read_json(ts_drive_path("arm.json"))
  expect_true(isTRUE(arm$armed))
  expect_null(arm$reaped)
})

test_that("§3.4 an already-disarmed arm.json is left byte-identical", {
  .drvfix_root()
  .drvfix_arm_write("deadbeef02", FALSE, extra = list(note = "disarmed by operator"))
  before <- ts_drive_read_json(ts_drive_path("arm.json"))
  ts_drive_reap_stale_arm("newtoken04")
  after <- ts_drive_read_json(ts_drive_path("arm.json"))
  expect_identical(before, after)
})

test_that("§3.5 a malformed arm.json is a no-op, not a crash", {
  .drvfix_root()
  ts_drive_write_json(list(protocol = "other/9", token = "x", armed = TRUE),
                      ts_drive_path("arm.json"))
  res <- ts_drive_reap_stale_arm("newtoken05")
  expect_false(isTRUE(attr(res, "written")))
})

test_that("§3.6 WIRING: ts_drive_attach reaps at boot (text-level guard)", {
  src <- paste(readLines(source_project_file("tools/pkg_skeleton/R/drive_watcher.R"),
                         warn = FALSE), collapse = "\n")
  expect_true(grepl("ts_drive_reap_stale_arm(token)", src, fixed = TRUE))
})

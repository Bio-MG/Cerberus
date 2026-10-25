# =============================================================================
# test-drive-agent-control.R — in-app AGENT CONTROL affordance (arm/disarm)
# =============================================================================
# The drive protocol was ONE-DIRECTIONAL on `arm.json`: only an external agent
# wrote it. A human looking at the app therefore had NO way to tell whether an
# agent was in control, and no way to invite one — the state lived in a file
# nobody was watching.
#
# This file pins the two halves of the fix:
#   1. `ts_drive_control_state()` — a PURE projection of the two wire files
#      into one of five states, safe to render in the UI (no token, no path).
#   2. `ts_drive_write_arm()` — the app writing `arm.json` for ITS OWN token,
#      gated by the same interactivity rule that keeps production inert.
#
# WHAT THIS FILE DELIBERATELY DOES NOT DO
#   It does not boot Shiny. The UI wiring (renderUI + observeEvent) is asserted
#   by the live acceptance run, not here — but every DECISION it renders is a
#   pure function asserted below.
# =============================================================================

# --- Fixtures ---------------------------------------------------------------

.dac_local_root <- function() {
  root <- file.path(tempdir(), paste0("tsdac-", as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(root, "tools", "_drive"), recursive = TRUE, showWarnings = FALSE)
  ts_drive_boot(root)
  ts_drive_clear_write_error()
  ts_drive_job_clear()
  root
}

.dac_source <- function() {
  source_project_file("tools/pkg_skeleton/R/drive_allowlist.R")
  # drive_watcher.R logs through ts_log_swallow() in its error handlers; a test
  # that sources only drive_watcher.R must provide the logger or a provoked
  # read error becomes "could not find function" instead of the asserted NULL.
  source_project_file("tools/pkg_skeleton/R/error_log.R")
  source_project_file("tools/pkg_skeleton/R/drive_watcher.R")
}

.dac_source()

# The arm gate is forced OPEN for the decision half (Rscript is never
# interactive). The gate ITSELF is asserted by its own test below, so opening it
# here cannot hide a regression.
options(ts.drive.interactive = TRUE)

.dac_ready <- function(token, viewer = "visible", hb_n = 0L,
                       protocol = TS_DRIVE_PROTOCOL) {
  ts_drive_write_json(
    list(protocol = protocol, pid = Sys.getpid(), port = "0", root = "/tmp",
         session_token = token, viewer = viewer, last_seq = 0L,
         started_at = "2026-10-24T10:00:00Z", hb_at = "2026-10-24T10:00:00Z",
         hb_n = hb_n, hb_timeout_s = 15L),
    ts_drive_path("ready.json")
  )
}

.dac_arm <- function(token, armed = TRUE, protocol = TS_DRIVE_PROTOCOL) {
  ts_drive_write_json(
    list(protocol = protocol, token = token, armed = armed),
    ts_drive_path("arm.json")
  )
}

.dac_read <- function(token = "tok1234", interactive = TRUE, wildcard_ok = FALSE) {
  ts_drive_control_state(
    token       = token,
    interactive = interactive,
    ready       = ts_drive_read_ready(),
    arm         = ts_drive_read_json(ts_drive_path("arm.json")),
    wildcard_ok = wildcard_ok
  )
}

# =============================================================================
# 1. The five states — a CLOSED set, each one reachable
# =============================================================================

test_that("the state set is closed and every member is reachable", {
  seen <- character(0)

  .dac_local_root()
  seen <- c(seen, .dac_read(interactive = FALSE)$state)   # unavailable
  .dac_ready("tok1234")
  .dac_arm("tok1234", armed = FALSE)
  seen <- c(seen, .dac_read()$state)                      # idle
  .dac_arm("tok1234")
  seen <- c(seen, .dac_read()$state)                      # armed
  .dac_arm("someoneelse")
  seen <- c(seen, .dac_read()$state)                      # armed_other
  ts_drive_boot(file.path(tempdir(), paste0("tsdac-empty-", as.integer(runif(1, 1, 1e9)))))
  dir.create(ts_drive_path(), recursive = TRUE, showWarnings = FALSE)
  seen <- c(seen, .dac_read()$state)                      # no_handshake

  # NON-VACUITY: the assertion is only meaningful if every state was produced.
  expect_setequal(unique(seen), TS_DRIVE_CONTROL_STATES)
})

test_that("a non-interactive session reports 'unavailable' and can never arm", {
  # The single rule that keeps PRODUCTION inert. `Rscript` is non-interactive by
  # definition, so this is exercised by the real predicate, not a mock.
  .dac_local_root()
  .dac_ready("tok1234")
  .dac_arm("tok1234")            # a PERFECT arm.json — and it must not matter

  st <- .dac_read(interactive = FALSE)
  expect_identical(st$state, "unavailable")
  expect_identical(st$reason, "non_interactive")
  expect_false(st$can_arm)
  expect_false(st$can_disarm)
})

test_that("no handshake, a foreign handshake and a foreign protocol all read as 'no_handshake'", {
  .dac_local_root()
  expect_identical(.dac_read()$state, "no_handshake")
  expect_identical(.dac_read()$reason, "no_handshake_file")

  # Another tab owns ready.json: arming THIS tab would arm the wrong session.
  .dac_ready("other999")
  st <- .dac_read(token = "tok1234")
  expect_identical(st$state, "no_handshake")
  expect_identical(st$reason, "other_session")
  expect_false(st$can_arm)

  .dac_ready("tok1234", protocol = "ts-drive/99")
  expect_identical(.dac_read()$reason, "handshake_invalid")
})

test_that("'idle' means: handshake ours, nobody armed — and it is armable", {
  .dac_local_root()
  .dac_ready("tok1234")

  st <- .dac_read()                      # no arm.json at all
  expect_identical(st$state, "idle")
  expect_identical(st$reason, "arm_absent")
  expect_true(st$can_arm)
  expect_false(st$can_disarm)

  .dac_arm("tok1234", armed = FALSE)     # explicit disarm keeps the token
  st <- .dac_read()
  expect_identical(st$state, "idle")
  expect_identical(st$reason, "not_armed")
  expect_true(st$can_arm)

  .dac_arm("tok1234", protocol = "ts-drive/99")
  st <- .dac_read()
  expect_identical(st$state, "idle")
  expect_identical(st$reason, "arm_invalid")
  expect_true(st$can_arm)                # ours to overwrite with a valid payload
})

test_that("'armed' is ours, 'armed_other' is not — and the wildcard is honoured only in dev", {
  .dac_local_root()
  .dac_ready("tok1234")

  .dac_arm("tok1234")
  st <- .dac_read()
  expect_identical(st$state, "armed")
  expect_identical(st$reason, "armed_self")
  expect_false(st$can_arm)
  expect_true(st$can_disarm)

  .dac_arm("someoneelse")
  st <- .dac_read()
  expect_identical(st$state, "armed_other")
  expect_identical(st$reason, "armed_other")
  expect_false(st$can_arm)               # never silently steal another session
  expect_false(st$can_disarm)            # not ours to disarm either

  # The wildcard arms whichever session is listening — dev launcher only.
  .dac_arm("*")
  expect_identical(.dac_read(wildcard_ok = FALSE)$state, "armed_other")
  expect_identical(.dac_read(wildcard_ok = FALSE)$reason, "wildcard_refused")
  st <- .dac_read(wildcard_ok = TRUE)
  expect_identical(st$state, "armed")
  expect_identical(st$reason, "armed_wildcard")
  expect_true(st$can_disarm)
})

test_that("a leftover arm.json can be TAKEN OVER — explicitly, never silently", {
  # MEASURED live (2026-10-24, session on :6515): `arm.json` held the token of a
  # session that no longer existed. The projection said `armed_other`, offered no
  # button, and the operator was locked out of their own app by a dead file.
  # `arm.json` is ONE global slot — not one per session — so the only way forward
  # is an EXPLICIT takeover: the human clicks, and the slot moves to this session.
  .dac_local_root()
  .dac_ready("tok1234")
  .dac_arm("deadbeef")

  st <- .dac_read()
  expect_identical(st$state, "armed_other")
  expect_true(st$can_takeover)
  expect_false(st$can_arm)
  expect_false(st$can_disarm)

  # The takeover is the ordinary writer: it overwrites the whole slot with OUR
  # token, and the projection then reports the session as armed by itself.
  expect_true(ts_drive_write_arm("tok1234", TRUE)$wrote)
  st <- .dac_read()
  expect_identical(st$state, "armed")
  expect_identical(st$reason, "armed_self")
  expect_false(st$can_takeover)
  expect_true(ts_drive_arm_state("tok1234")$armed)
  expect_false(ts_drive_arm_state("deadbeef")$armed)   # the old owner is gone
})

test_that("takeover is offered ONLY in armed_other, and never in production", {
  .dac_local_root()
  .dac_ready("tok1234")

  # Every other state must NOT offer it: `idle` arms, `armed` disarms, and
  # `no_handshake`/`unavailable` have nothing this tab may honestly touch.
  .dac_arm("tok1234", armed = FALSE)
  expect_false(.dac_read()$can_takeover)                # idle
  .dac_arm("tok1234")
  expect_false(.dac_read()$can_takeover)                # armed (ours)
  .dac_arm("someoneelse")
  expect_true(.dac_read()$can_takeover)                 # armed_other
  expect_false(.dac_read(interactive = FALSE)$can_takeover)   # production
})

test_that("the projection never carries a token, a path or a pid", {
  .dac_local_root()
  .dac_ready("tok1234", viewer = "visible", hb_n = 7L)
  .dac_arm("tok1234")

  st <- .dac_read()
  flat <- paste(unlist(st), collapse = " | ")
  expect_false(grepl("tok1234", flat, fixed = TRUE))
  expect_false(grepl(tempdir(), flat, fixed = TRUE))
  expect_false("pid" %in% names(st))
  expect_false("root" %in% names(st))
  # The benign, declared fields DO travel — otherwise the panel would be empty.
  expect_identical(st$viewer, "visible")
  expect_identical(st$heartbeat_n, 7L)
})

# =============================================================================
# 2. The app writing arm.json — same wire format as the agent
# =============================================================================

test_that("a non-interactive session refuses to write arm.json at all", {
  .dac_local_root()
  .dac_ready("tok1234")

  res <- ts_drive_write_arm("tok1234", TRUE, interactive = FALSE)
  expect_false(res$wrote)
  expect_identical(res$reason, "non_interactive")
  expect_false(file.exists(ts_drive_path("arm.json")))
})

test_that("the writer refuses a token it cannot honestly arm", {
  .dac_local_root()
  .dac_ready("tok1234")

  expect_identical(ts_drive_write_arm("", TRUE)$reason, "bad_token")
  expect_identical(ts_drive_write_arm(NA_character_, TRUE)$reason, "bad_token")
  expect_identical(ts_drive_write_arm(c("a", "b"), TRUE)$reason, "bad_token")
  # '*' is the dev launcher's escape hatch: an app must NEVER write it.
  expect_identical(ts_drive_write_arm("*", TRUE)$reason, "bad_token")
  expect_false(file.exists(ts_drive_path("arm.json")))
})

test_that("arming from the app opens the REAL gate (observable effect, not a return value)", {
  # The assertion that matters: not "the function said TRUE", but "the gate the
  # poller actually consults now says armed".
  .dac_local_root()
  .dac_ready("tok1234")
  expect_false(ts_drive_arm_state("tok1234")$armed)

  res <- ts_drive_write_arm("tok1234", TRUE)
  expect_true(res$wrote)
  expect_identical(res$reason, "armed")

  gate <- ts_drive_arm_state("tok1234")
  expect_true(gate$armed)
  expect_identical(gate$reason, "token match")

  # …and the file on disk carries the protocol version and OUR token, never '*'.
  on_disk <- ts_drive_read_json(ts_drive_path("arm.json"))
  expect_identical(on_disk$protocol, TS_DRIVE_PROTOCOL)
  expect_identical(on_disk$token, "tok1234")
  expect_true(on_disk$armed)
})

test_that("disarming keeps the token, so re-arming needs no restart", {
  .dac_local_root()
  .dac_ready("tok1234")
  ts_drive_write_arm("tok1234", TRUE)

  res <- ts_drive_write_arm("tok1234", FALSE)
  expect_true(res$wrote)
  expect_identical(res$reason, "disarmed")

  gate <- ts_drive_arm_state("tok1234")
  expect_false(gate$armed)
  expect_match(gate$reason, "armed=false")

  on_disk <- ts_drive_read_json(ts_drive_path("arm.json"))
  expect_identical(on_disk$token, "tok1234")   # token survives the disarm
  expect_false(on_disk$armed)

  # A second arm on the same token works without any new handshake.
  expect_true(ts_drive_write_arm("tok1234", TRUE)$wrote)
  expect_true(ts_drive_arm_state("tok1234")$armed)
})

test_that("the writer touches arm.json ONLY — ready.json is not rewritten", {
  # Same scope discipline as the MCP's `set_armed` tool: the app must not be
  # able to move its own handshake as a side effect of arming.
  .dac_local_root()
  .dac_ready("tok1234")
  before <- ts_drive_read_ready()
  before_mtime <- ts_drive_mtime(ts_drive_path("ready.json"))

  ts_drive_write_arm("tok1234", TRUE)

  after <- ts_drive_read_ready()
  expect_identical(after$session_token, before$session_token)
  expect_identical(after$hb_n, before$hb_n)
  expect_identical(ts_drive_mtime(ts_drive_path("ready.json")), before_mtime)
})

# =============================================================================
# 3. Round trip: what the app writes, the control panel must read back
# =============================================================================

test_that("arm then disarm round-trips through the projection", {
  .dac_local_root()
  .dac_ready("tok1234")
  expect_identical(.dac_read()$state, "idle")

  ts_drive_write_arm("tok1234", TRUE)
  st <- .dac_read()
  expect_identical(st$state, "armed")
  expect_true(st$can_disarm)

  ts_drive_write_arm("tok1234", FALSE)
  st <- .dac_read()
  expect_identical(st$state, "idle")
  expect_true(st$can_arm)
})

# =============================================================================
# 4. SESSION IDENTIFICATION — the human must be able to NAME their session
# =============================================================================
# WHY THIS EXISTS (asked for 2026-10-24): with two tabs open, the agent drives
# whichever session last wrote `ready.json`, and the human has no way to say
# WHICH tab they are looking at. The sentence "I am on <url>, session <id>" is
# only resolvable if the id shown in the app is the SAME value the MCP server
# publishes as `session_id` in `transcripto_drive_status`.
#
# The id is a DERIVED hash (pid | started_at | token), so it identifies the
# session without publishing the token: a displayed id can never arm anything,
# because arming requires an exact token match.

test_that("ts_drive_session_id() is deterministic, bounded and is not the token", {
  sid <- ts_drive_session_id("35928", "2026-10-24T16:10:20Z", "c0cwqwst")
  expect_identical(sid, ts_drive_session_id("35928", "2026-10-24T16:10:20Z", "c0cwqwst"))
  expect_match(sid, "^[0-9a-f]{16}$")
  expect_false(grepl("c0cwqwst", sid, fixed = TRUE))
  # every component participates: same id for two different sessions would be
  # worse than no id at all
  expect_false(identical(sid, ts_drive_session_id("35929", "2026-10-24T16:10:20Z", "c0cwqwst")))
  expect_false(identical(sid, ts_drive_session_id("35928", "2026-10-24T16:10:21Z", "c0cwqwst")))
  expect_false(identical(sid, ts_drive_session_id("35928", "2026-10-24T16:10:20Z", "c0cwqwsX")))
  # a numeric pid and its character form are the SAME session, because
  # ready.json carries the pid as a JSON number and the MCP reads it as text
  expect_identical(sid, ts_drive_session_id(35928L, "2026-10-24T16:10:20Z", "c0cwqwst"))
})

test_that("the app and the MCP derive the SAME id — one truth, checked twice", {
  # The MCP cannot source `R/` (P0: its code lives in scripts/, and it runs
  # under `--no-init-file`), so the algorithm EXISTS in two places. Two copies
  # of one rule are two truths unless something compares them: this test is
  # that comparison. If either side is edited, this goes red.
  src <- file.path(app_root_dir(), "scripts", "mcp_server.R")
  expect_true(file.exists(src))

  exprs <- parse(src, keep.source = FALSE, encoding = "UTF-8")
  mcp <- NULL
  for (e in exprs) {
    if (is.call(e) && identical(e[[1L]], as.name("<-")) &&
        is.name(e[[2L]]) && identical(as.character(e[[2L]]), ".ts_session_id")) {
      mcp <- e
    }
  }
  expect_false(is.null(mcp))            # anti-vacuity: a rename must not pass
  env <- new.env(parent = baseenv())
  eval(mcp, envir = env)
  expect_true(is.function(env$.ts_session_id))

  cases <- list(
    list("35928", "2026-10-24T16:10:20Z", "c0cwqwst"),
    list("1", "1970-01-01T00:00:00Z", "aaaaaaaa"),
    list("999999", "2026-12-31T23:59:59Z", "Zz09-_.x")
  )
  for (cs in cases) {
    expect_identical(
      ts_drive_session_id(cs[[1]], cs[[2]], cs[[3]]),
      env$.ts_session_id(cs[[1]], cs[[2]], cs[[3]]),
      info = paste("divergence sur", paste(unlist(cs), collapse = " | "))
    )
  }
})

test_that("attach() publishes the id that ready.json implies", {
  # The panel reads `.drive$session_id`, so the value must come from the same
  # triple that was written to ready.json — not from a second clock reading,
  # which would drift by a second and identify nothing.
  .dac_local_root()
  env <- new.env()
  sess <- list(onSessionEnded = function(f) { env$f <- f; invisible(NULL) })
  d <- ts_drive_attach(sess, list())
  hb <- ts_drive_read_ready()
  expect_identical(
    d$session_id,
    ts_drive_session_id(hb$pid, hb$started_at, hb$session_token)
  )
  expect_match(d$session_id, "^[0-9a-f]{16}$")
  expect_false(identical(d$session_id, d$token))
})

test_that("the panel shows the session id, and never the token", {
  .dac_local_root()
  .dac_ready("zzTOKENzz")
  st <- .dac_read("zzTOKENzz")

  html <- as.character(ts_drive_control_ui(
    st, label = "Aucun agent",
    session_id = "38cb416a33dccbea", session_label = "ID de session :"
  ))
  expect_match(html, "38cb416a33dccbea", fixed = TRUE)
  expect_match(html, "ID de session", fixed = TRUE)
  expect_false(grepl("zzTOKENzz", html, fixed = TRUE))

  # No orphan separator: with no id, the line is ABSENT, not rendered empty
  # (the `sprintf("… : %s", "")` trap — an empty segment still prints its label).
  html_none <- as.character(ts_drive_control_ui(
    st, label = "Aucun agent", session_id = "", session_label = "ID de session :"
  ))
  expect_false(grepl("ID de session", html_none, fixed = TRUE))
  html_null <- as.character(ts_drive_control_ui(st, label = "Aucun agent"))
  expect_false(grepl("ID de session", html_null, fixed = TRUE))
})

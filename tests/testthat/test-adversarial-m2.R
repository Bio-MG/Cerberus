# =============================================================================
# test-adversarial-m2.R — Adversarial Challenge Suite for Milestone M2
# =============================================================================
# Roles: critic, empirical_challenger
# Focus areas:
#   1. Host ready.json mutation immunity & sandbox hermetic isolation
#   2. Error ring buffer boundary conditions (unicode, extreme len, NULLs, FIFO)
#   3. Passive state diff-gating (100 ticks -> exactly 1 write, transition writes)
#   4. Session invalidation (onSessionEnded unlinking state.json & ready.json)
#   5. MCP JSON-RPC dispatch integrity for transcripto_drive_state
# =============================================================================

library(testthat)

# Ensure drive allowlist and watcher are sourced
if (file.exists(file.path(ts_project_root(), "R", "core", "drive_allowlist.R"))) {
  source(file.path(ts_project_root(), "R", "core", "drive_allowlist.R"), local = FALSE)
} else if (file.exists(file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "drive_allowlist.R"))) {
  source(file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "drive_allowlist.R"), local = FALSE)
}

if (file.exists(file.path(ts_project_root(), "R", "core", "drive_watcher.R"))) {
  source(file.path(ts_project_root(), "R", "core", "drive_watcher.R"), local = FALSE)
} else if (file.exists(file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "drive_watcher.R"))) {
  source(file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "drive_watcher.R"), local = FALSE)
}

.adversarial_mcp_env <- function() {
  mcp_script <- file.path(ts_project_root(), "scripts", "mcp_server.R")
  e <- new.env(parent = globalenv())
  watcher_path <- if (file.exists(file.path(ts_project_root(), "R", "core", "drive_watcher.R"))) {
    file.path(ts_project_root(), "R", "core", "drive_watcher.R")
  } else {
    file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "drive_watcher.R")
  }
  sys.source(watcher_path, envir = e)

  expressions <- parse(mcp_script, keep.source = TRUE)
  labels <- vapply(expressions, function(x) paste(deparse(x, width.cutoff = 500L), collapse = "\n"), character(1))
  first <- which(grepl("^\\.ts_json <- function", labels))[1L]
  last <- which(grepl("^\\.ts_serve <- function", labels))[1L]
  if (is.na(first) || is.na(last) || last <= first) {
    stop("mcp_server.R boundaries not found")
  }
  for (i in seq.int(first, last - 1L)) eval(expressions[[i]], envir = e)

  root <- file.path(tempdir(), paste0("adv-mcp-", as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(root, "tools", "_drive"), recursive = TRUE, showWarnings = FALSE)
  e$ts_drive_boot(root)
  e
}


# =============================================================================
# 1. Host ready.json Mutation Immunity & Sandbox Hermetic Isolation
# =============================================================================

test_that("ADVERSARIAL 1.1: Host ready.json is 100% immune to sandbox operations", {
  host_drive <- file.path(ts_project_root(), "tools", "_drive")
  dir.create(host_drive, recursive = TRUE, showWarnings = FALSE)
  host_ready <- file.path(host_drive, "ready.json")

  host_existed_before <- file.exists(host_ready)
  original_content <- if (host_existed_before) readLines(host_ready, warn = FALSE) else NULL

  # Plant a distinct mock operator session
  mock_host_content <- c(
    "{",
    '  "session_token": "OPERATOR_HOST_IMMUNE_TOKEN_777",',
    '  "pid": 99999,',
    '  "started_at": "2026-10-25T12:00:00Z",',
    '  "hb_at": "2026-10-25T12:00:01Z",',
    '  "sentinel": "DO_NOT_TOUCH_OPERATOR_DATA"',
    "}"
  )
  writeLines(mock_host_content, host_ready)
  host_info_before <- file.info(host_ready)
  host_mtime_before <- host_info_before$mtime

  on.exit({
    ts_drive_boot(NULL)
    if (host_existed_before) {
      writeLines(original_content, host_ready)
    } else {
      unlink(host_ready)
    }
  }, add = TRUE)

  # Spin up an aggressive sandbox session
  sb_root <- tempfile("adversarial_sb_")
  dir.create(file.path(sb_root, "tools", "_drive"), recursive = TRUE)
  on.exit(unlink(sb_root, recursive = TRUE, force = TRUE), add = TRUE)

  old_env <- Sys.getenv("TRANSCRIPTO_DRIVE_ROOT", unset = NA)
  on.exit(if (!is.na(old_env)) Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = old_env) else Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT"), add = TRUE)
  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = sb_root)

  ts_drive_boot(sb_root)

  # Mutate sandbox with multiple writes and state emissions
  ts_drive_write_json(list(session_token = "SANDBOX_MOCK_TOKEN", pid = 11111), ts_drive_path("ready.json"))
  ts_drive_emit_passive_state(token = "SANDBOX_MOCK_TOKEN", force = TRUE)
  ts_drive_write_json(list(status = "done", seq = 1), ts_drive_path("result.json"))

  # Invalidate sandbox state and ready
  ts_drive_invalidate_state("SANDBOX_MOCK_TOKEN")
  ts_drive_invalidate_ready("SANDBOX_MOCK_TOKEN")

  # Tear down sandbox directory completely
  unlink(sb_root, recursive = TRUE, force = TRUE)

  # ASSERT: Host ready.json must be untouched
  expect_true(file.exists(host_ready))
  host_content_after <- readLines(host_ready, warn = FALSE)
  expect_identical(host_content_after, mock_host_content)
  expect_identical(file.info(host_ready)$mtime, host_mtime_before)
})

test_that("ADVERSARIAL 1.2: Path resolution with Windows backslashes and trailing slashes", {
  sb <- tempfile("adv_slash_")
  dir.create(sb, recursive = TRUE)
  on.exit(unlink(sb, recursive = TRUE), add = TRUE)

  old_env <- Sys.getenv("TRANSCRIPTO_DRIVE_ROOT", unset = NA)
  on.exit(if (!is.na(old_env)) Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = old_env) else Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT"), add = TRUE)

  # Path with Windows backslashes and trailing slash
  win_path <- paste0(gsub("/", "\\\\", sb), "\\")
  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = win_path)

  res <- ts_drive_root()
  expect_false(endsWith(res, "/"))
  expect_false(endsWith(res, "\\"))
  expect_false(grepl("//", res))
  expect_identical(
    normalizePath(res, winslash = "/", mustWork = FALSE),
    normalizePath(sb, winslash = "/", mustWork = FALSE)
  )
})

test_that("ADVERSARIAL 1.3: ts_drive_boot idempotence and isolation", {
  sb1 <- tempfile("adv_boot1_")
  sb2 <- tempfile("adv_boot2_")
  dir.create(sb1, recursive = TRUE)
  dir.create(sb2, recursive = TRUE)
  on.exit({
    ts_drive_boot(NULL)
    unlink(c(sb1, sb2), recursive = TRUE)
  }, add = TRUE)

  old_env <- Sys.getenv("TRANSCRIPTO_DRIVE_ROOT", unset = NA)
  on.exit(if (!is.na(old_env)) Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = old_env) else Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT"), add = TRUE)
  Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT")

  # Boot sb1 explicitly
  ts_drive_boot(sb1)
  expect_identical(normalizePath(ts_drive_root(), winslash = "/"), normalizePath(sb1, winslash = "/"))

  # Boot sb2 explicitly
  ts_drive_boot(sb2)
  expect_identical(normalizePath(ts_drive_root(), winslash = "/"), normalizePath(sb2, winslash = "/"))

  # Explicit boot sb2 overrides TRANSCRIPTO_DRIVE_ROOT
  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = sb1)
  expect_identical(normalizePath(ts_drive_root(), winslash = "/"), normalizePath(sb2, winslash = "/"))

  # Default boot allows TRANSCRIPTO_DRIVE_ROOT to resolve
  ts_drive_boot(NULL)
  expect_identical(normalizePath(ts_drive_root(), winslash = "/"), normalizePath(sb1, winslash = "/"))
})


test_that("ADVERSARIAL 1.4: app.R sources tools/pkg_skeleton/R/drive_watcher.R which honors TRANSCRIPTO_DRIVE_ROOT", {
  # app.R line 90 sources tools/pkg_skeleton/R/drive_watcher.R
  # Verify that tools/pkg_skeleton/R/drive_watcher.R honors TRANSCRIPTO_DRIVE_ROOT
  e_skeleton <- new.env(parent = baseenv())
  sys.source(file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "drive_allowlist.R"), envir = e_skeleton)

  old_env <- Sys.getenv("TRANSCRIPTO_DRIVE_ROOT", unset = NA)
  on.exit(if (!is.na(old_env)) Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = old_env) else Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT"), add = TRUE)

  sb_test <- tempfile("test_sb_diverge_")
  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = sb_test)

  sys.source(file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "drive_watcher.R"), envir = e_skeleton)

  # Verify: pkg_skeleton resolves to TRANSCRIPTO_DRIVE_ROOT
  root_resolved <- e_skeleton$ts_drive_root()
  expect_identical(
    normalizePath(root_resolved, winslash = "/", mustWork = FALSE),
    normalizePath(sb_test, winslash = "/", mustWork = FALSE)
  )
})

# =============================================================================
# 2. Error Ring Buffer Boundary Conditions & Edge Cases
# =============================================================================

test_that("ADVERSARIAL 2.1: Unicode, emoji, and multi-byte error messages", {
  ts_drive_clear_errors()

  unicode_msg <- "Échec critique à l'étape 3: π ≈ 3.14159 \u00e9\u00e8\u00e0 🧬💥 \U0001F680 测试 Ошибка"
  ts_drive_record_error(unicode_msg, module = "sc_pipeline")

  errs <- ts_drive_get_errors()
  expect_equal(length(errs), 1L)
  expect_identical(errs[[1]]$module, "sc_pipeline")
  expect_identical(errs[[1]]$message, unicode_msg)

  # Check that state.json serializes and reads it back without encoding mangling
  sb <- tempfile("adv_unicode_sb_")
  dir.create(file.path(sb, "tools", "_drive"), recursive = TRUE)
  on.exit(unlink(sb, recursive = TRUE), add = TRUE)
  ts_drive_boot(sb)

  ts_drive_emit_passive_state(token = "tok_unicode", force = TRUE)
  st <- ts_drive_read_state()
  expect_equal(length(st$errors), 1L)
  expect_identical(st$errors[[1]]$message, unicode_msg)
})

test_that("ADVERSARIAL 2.2: Extreme message length (>1000 chars) is truncated to 200", {
  ts_drive_clear_errors()

  huge_msg <- paste(rep("ABCDE12345", 250), collapse = "") # 2500 chars
  expect_equal(nchar(huge_msg), 2500L)

  ts_drive_record_error(huge_msg, module = "stress_module")
  errs <- ts_drive_get_errors()
  expect_equal(length(errs), 1L)
  expect_equal(nchar(errs[[1]]$message), 200L)
  expect_identical(errs[[1]]$message, substr(huge_msg, 1L, 200L))
})

test_that("ADVERSARIAL 2.3: Multiline message split on first newline and whitespace trimmed", {
  ts_drive_clear_errors()

  multiline <- "   First line error message   \nSecond line stack trace\nThird line internal details"
  ts_drive_record_error(multiline, module = "trace_mod")

  errs <- ts_drive_get_errors()
  expect_equal(length(errs), 1L)
  expect_identical(errs[[1]]$message, "First line error message")
})

test_that("ADVERSARIAL 2.4: NULL, empty, and non-character messages", {
  ts_drive_clear_errors()

  # NULL message: no-op
  ts_drive_record_error(NULL, module = "app")
  expect_equal(length(ts_drive_get_errors()), 0L)

  # Empty string: no-op
  ts_drive_record_error("", module = "app")
  expect_equal(length(ts_drive_get_errors()), 0L)

  # Whitespace only: trimmed
  ts_drive_record_error("   ", module = "app")
  ts_drive_clear_errors()
})

test_that("ADVERSARIAL 2.4b: ts_drive_record_error safely ignores NA or character(0) without crashing", {
  ts_drive_clear_errors()

  # Safely handled without throwing 'valeur manquante / missing value where TRUE/FALSE needed':
  expect_silent(ts_drive_record_error(NA_character_))
  expect_silent(ts_drive_record_error(character(0)))
  expect_equal(length(ts_drive_get_errors()), 0L)
})

test_that("ADVERSARIAL 2.5: NULL and missing module fall back to 'app'", {
  ts_drive_clear_errors()

  ts_drive_record_error("Missing module error", module = NULL)
  ts_drive_record_error("Empty module error", module = "")

  errs <- ts_drive_get_errors()
  expect_equal(length(errs), 2L)
  expect_identical(errs[[1]]$module, "app")
  expect_identical(errs[[2]]$module, "app")
})

test_that("ADVERSARIAL 2.6: Condition object classification and state extraction", {
  ts_drive_clear_errors()

  # simpleError: class simpleError is omitted by design
  ts_drive_record_error("Simple error", module = "mod_simple", condition = simpleError("simple"))
  errs <- ts_drive_get_errors()
  expect_null(errs[[1]]$class)

  # Custom condition with class
  cust_cond <- structure(
    list(message = "custom failure", call = NULL),
    class = c("sc_ambient_package_missing", "sc_qc_error", "error", "condition")
  )
  ts_drive_record_error("Custom classed error", module = "sc_qc", condition = cust_cond)

  errs <- ts_drive_get_errors()
  expect_equal(length(errs), 2L)
  expect_identical(errs[[2]]$class, "sc_ambient_package_missing")
})

test_that("ADVERSARIAL 2.7: Strict FIFO ring buffer eviction across 100 sequential pushes", {
  ts_drive_clear_errors()

  for (i in 1:100) {
    ts_drive_record_error(sprintf("Stress error #%03d", i), module = "test_loop")
  }

  errs <- ts_drive_get_errors(limit = 100L)
  expect_equal(length(errs), 20L) # Capped at TS_DRIVE_MAX_ERRORS = 20

  # Oldest remaining entry must be #081
  expect_identical(errs[[1]]$message, "Stress error #081")
  # Newest entry must be #100
  expect_identical(errs[[20]]$message, "Stress error #100")

  # Order must be strictly preserved
  expected_seq <- sprintf("Stress error #%03d", 81:100)
  actual_seq <- vapply(errs, function(e) e$message, character(1))
  expect_identical(actual_seq, expected_seq)
})


# =============================================================================
# 3. Passive State Diff-Gating & Zero Disk I/O
# =============================================================================

test_that("ADVERSARIAL 3.1: 100 simulated ticks with unchanged state produce EXACTLY 1 disk write", {
  sb <- tempfile("adv_diff_100_")
  dir.create(file.path(sb, "tools", "_drive"), recursive = TRUE)
  on.exit(unlink(sb, recursive = TRUE), add = TRUE)
  ts_drive_boot(sb)
  ts_drive_clear_errors()

  token <- "adv_tok_diff_100"
  state_path <- ts_drive_path("state.json")

  # Tick 1: Initial emission -> MUST write
  t1_res <- ts_drive_emit_passive_state(token = token)
  expect_true(t1_res)
  expect_true(file.exists(state_path))
  mtime_initial <- file.info(state_path)$mtime

  # Ticks 2 to 100: Identical state -> MUST produce ZERO disk writes
  write_results <- logical(99)
  for (i in 2:100) {
    write_results[i - 1] <- ts_drive_emit_passive_state(token = token)
  }

  expect_equal(sum(write_results), 0L)
  expect_true(all(!write_results))

  # File mtime and contents must be 100% unaltered
  mtime_final <- file.info(state_path)$mtime
  expect_identical(mtime_initial, mtime_final)
})

test_that("ADVERSARIAL 3.2: State transitions trigger disk write only on change", {
  sb <- tempfile("adv_diff_trans_")
  dir.create(file.path(sb, "tools", "_drive"), recursive = TRUE)
  on.exit(unlink(sb, recursive = TRUE), add = TRUE)
  ts_drive_boot(sb)
  ts_drive_clear_errors()

  token <- "adv_tok_trans"
  state_path <- ts_drive_path("state.json")

  # Initial idle write
  w1 <- ts_drive_emit_passive_state(token = token)
  expect_true(w1)
  st1 <- ts_drive_read_state()
  expect_identical(st1$pipeline_status, "idle")

  # 10 idle ticks -> 0 writes
  for (i in 1:10) expect_false(ts_drive_emit_passive_state(token = token))

  # Transition to error via record_error
  ts_drive_record_error("Unexpected failure in computation", module = "bulk_de")
  w2 <- ts_drive_emit_passive_state(token = token)
  expect_true(w2)
  st2 <- ts_drive_read_state()
  expect_equal(length(st2$errors), 1L)
  expect_false(identical(st1$content_hash, st2$content_hash))
  # Jalon filets (2026-10-27) : l'émetteur câble désormais has_recent_error —
  # une erreur récente du tampon porte pipeline_status = "error" (défaut
  # épinglé en M5, T3-ERR-3 : le chemin ring→"error" était inatteignable).
  expect_identical(st2$pipeline_status, "error")

  # 10 subsequent ticks -> 0 writes
  for (i in 1:10) expect_false(ts_drive_emit_passive_state(token = token))

  # Transition to active module via tick_out
  tick_bulk <- list(module = "bulk_de", status = "running", error = NULL)
  w3 <- ts_drive_emit_passive_state(token = token, tick_out = tick_bulk)
  expect_true(w3)
  st3 <- ts_drive_read_state()
  expect_identical(st3$active_module, "bulk_de")
  expect_identical(st3$pipeline_status, "running")

  # 10 subsequent ticks with same tick_out -> 0 writes
  for (i in 1:10) expect_false(ts_drive_emit_passive_state(token = token, tick_out = tick_bulk))
})

test_that("ADVERSARIAL 3.3: Content hash excludes timestamp for stable diff-gating", {
  h1 <- ts_drive_compute_state_hash("tok_a", "bulk", "idle", list())
  Sys.sleep(0.01)
  h2 <- ts_drive_compute_state_hash("tok_a", "bulk", "idle", list())
  expect_identical(h1, h2)

  # But differs when any semantic element changes
  expect_false(identical(h1, ts_drive_compute_state_hash("tok_b", "bulk", "idle", list())))
  expect_false(identical(h1, ts_drive_compute_state_hash("tok_a", "sc", "idle", list())))
  expect_false(identical(h1, ts_drive_compute_state_hash("tok_a", "bulk", "running", list())))
  expect_false(identical(h1, ts_drive_compute_state_hash("tok_a", "bulk", "idle", list(list(message = "err")))))
})


# =============================================================================
# 4. Session Invalidation (onSessionEnded)
# =============================================================================

test_that("ADVERSARIAL 4.1: onSessionEnded unlinks state.json and cleans session state", {
  sb <- tempfile("adv_sess_inval_")
  dir.create(file.path(sb, "tools", "_drive"), recursive = TRUE)
  on.exit(unlink(sb, recursive = TRUE), add = TRUE)
  ts_drive_boot(sb)

  token <- "adv_tok_inval_77"
  ts_drive_write_json(list(session_token = token, pid = 123), ts_drive_path("ready.json"))
  ts_drive_emit_passive_state(token = token, force = TRUE)

  expect_true(file.exists(ts_drive_path("ready.json")))
  expect_true(file.exists(ts_drive_path("state.json")))
  expect_true(!is.null(.ts_drive_state$last_state_hash[[token]]))

  # Simulate the onSessionEnded callback registered in ts_drive_attach
  # We test the exact sequence executed by the session teardown
  ts_drive_invalidate_state(token)
  ts_drive_invalidate_ready(token)

  expect_false(file.exists(ts_drive_path("state.json")))
  expect_false(file.exists(ts_drive_path("ready.json")))
  expect_null(.ts_drive_state$last_state_hash[[token]])
  expect_null(ts_drive_read_state())
})

test_that("ADVERSARIAL 4.2: ts_drive_invalidate_state handles absent files and invalid tokens gracefully", {
  sb <- tempfile("adv_inval_robust_")
  dir.create(file.path(sb, "tools", "_drive"), recursive = TRUE)
  on.exit(unlink(sb, recursive = TRUE), add = TRUE)
  ts_drive_boot(sb)

  # state.json absent: does not throw, returns TRUE
  expect_false(file.exists(ts_drive_path("state.json")))
  res1 <- ts_drive_invalidate_state()
  expect_true(res1)

  # With NULL token
  res2 <- ts_drive_invalidate_state(NULL)
  expect_true(res2)

  # With nonexistent token
  res3 <- ts_drive_invalidate_state("nonexistent_tok")
  expect_true(res3)
})


# =============================================================================
# 5. MCP JSON-RPC Dispatch Integrity for transcripto_drive_state
# =============================================================================

test_that("ADVERSARIAL 5.1: Valid JSON-RPC call returns complete state payload", {
  e <- .adversarial_mcp_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)

  token <- "adv_mcp_tok_99"
  e$ts_drive_emit_passive_state(token = token, force = TRUE)

  req <- list(
    jsonrpc = "2.0",
    id = 999L,
    method = "tools/call",
    params = list(
      name = "transcripto_drive_state",
      arguments = list()
    )
  )

  resp <- e$.ts_dispatch(req)
  expect_identical(resp$jsonrpc, "2.0")
  expect_identical(resp$id, 999L)
  expect_false(isTRUE(resp$result$isError))
  expect_true(is.list(resp$result$content))
  expect_equal(resp$result$content[[1]]$type, "text")
  expect_identical(resp$result$structuredContent$session_token, token)
  expect_true(nchar(resp$result$structuredContent$content_hash) > 0L)
})

test_that("ADVERSARIAL 5.2: JSON-RPC call returns NO_SESSION when state.json is absent", {
  e <- .adversarial_mcp_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)

  unlink(e$ts_drive_path("state.json"))

  req <- list(
    jsonrpc = "2.0",
    id = 888L,
    method = "tools/call",
    params = list(
      name = "transcripto_drive_state",
      arguments = list()
    )
  )

  resp <- e$.ts_dispatch(req)
  expect_identical(resp$jsonrpc, "2.0")
  expect_identical(resp$id, 888L)
  expect_true(resp$result$isError)
  expect_identical(resp$result$structuredContent$code, "NO_SESSION")
  expect_true(grepl("absent", resp$result$structuredContent$message, ignore.case = TRUE))
})

test_that("ADVERSARIAL 5.3: MCP tool list schema compliance for transcripto_drive_state", {
  e <- .adversarial_mcp_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)

  tools_resp <- e$.ts_dispatch(list(jsonrpc = "2.0", id = 1L, method = "tools/list"))
  tools <- tools_resp$result$tools

  match_idx <- which(vapply(tools, function(x) x$name == "transcripto_drive_state", logical(1)))
  expect_equal(length(match_idx), 1L)

  tool <- tools[[match_idx]]
  expect_identical(tool$name, "transcripto_drive_state")
  expect_true(nzchar(tool$description))
  expect_identical(tool$inputSchema$type, "object")
  expect_equal(length(tool$inputSchema$properties), 0L)
  expect_false(isTRUE(tool$inputSchema$additionalProperties))
})

test_that("ADVERSARIAL 5.4: Malformed JSON-RPC requests return proper JSON-RPC error codes", {
  e <- .adversarial_mcp_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)

  # Missing method: -32600 (Invalid Request)
  bad_req1 <- list(jsonrpc = "2.0", id = 101L, params = list())
  resp1 <- e$.ts_dispatch(bad_req1)
  expect_equal(resp1$error$code, -32600)

  # Non-scalar method (e.g. vector length 2): -32600 (Invalid Request)
  bad_req2 <- list(jsonrpc = "2.0", id = 102L, method = c("method_a", "method_b"))
  resp2 <- e$.ts_dispatch(bad_req2)
  expect_equal(resp2$error$code, -32600)

  # Unknown method name: -32601 (Method not found)
  bad_req3 <- list(jsonrpc = "2.0", id = 103L, method = "non_existent_method")
  resp3 <- e$.ts_dispatch(bad_req3)
  expect_equal(resp3$error$code, -32601)

  # Missing name in tools/call: -32602 (Invalid params)
  bad_req4 <- list(jsonrpc = "2.0", id = 104L, method = "tools/call", params = list())
  resp4 <- e$.ts_dispatch(bad_req4)
  expect_equal(resp4$error$code, -32602)

  # Non-scalar name in tools/call: -32602 (Invalid params)
  bad_req5 <- list(jsonrpc = "2.0", id = 105L, method = "tools/call", params = list(name = list("invalid")))
  resp5 <- e$.ts_dispatch(bad_req5)
  expect_equal(resp5$error$code, -32602)

  # Notification (no id) without reply
  notif_req <- list(jsonrpc = "2.0", method = "notifications/initialized")
  resp6 <- e$.ts_dispatch(notif_req)
  expect_null(resp6)
})

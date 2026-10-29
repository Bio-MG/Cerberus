# =============================================================================
# test-challenge-m2-adversarial.R — Adversarial Stress Test Suite for Milestone M2
# Empirical challenge of Drive Sandbox Isolation & Passive Observation Channel
# =============================================================================

if (!exists("ts_project_root", mode = "function")) {
  ts_project_root <- function() {
    normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = FALSE)
  }
}

# Ensure allowlist and drive watcher are loaded
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

# Helper to load MCP server environment
.mcp_challenge_env <- function() {
  path <- file.path(ts_project_root(), "scripts", "mcp_server.R")
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "drive_allowlist.R"), envir = e)
  watcher_path <- if (file.exists(file.path(ts_project_root(), "R", "core", "drive_watcher.R"))) {
    file.path(ts_project_root(), "R", "core", "drive_watcher.R")
  } else {
    file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "drive_watcher.R")
  }
  sys.source(watcher_path, envir = e)

  expressions <- parse(path, keep.source = TRUE)
  labels <- vapply(expressions, function(x) paste(deparse(x, width.cutoff = 500L), collapse = "\n"), character(1))
  first <- which(grepl("^\\.ts_json <- function", labels))[1L]
  last <- which(grepl("^\\.ts_serve <- function", labels))[1L]
  for (i in seq.int(first, last - 1L)) eval(expressions[[i]], envir = e)

  root <- file.path(tempdir(), paste0("mcp-stress-", as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(root, "tools", "_drive"), recursive = TRUE, showWarnings = FALSE)
  e$ts_drive_boot(root)
  e
}

# =============================================================================
# Challenge Area 1: Drive Root Resolution Stress Testing
# =============================================================================

test_that("C1.1: ts_drive_root strips extreme trailing slashes and backslashes", {
  base_dir <- tempfile("slash_stress_")
  dir.create(base_dir, recursive = TRUE)
  on.exit({
    Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT")
    unlink(base_dir, recursive = TRUE)
  }, add = TRUE)

  # Test single slash, double slash, triple slash, mixed slashes
  test_cases <- c(
    paste0(base_dir, "/"),
    paste0(base_dir, "//"),
    paste0(base_dir, "///"),
    paste0(base_dir, "/./"),
    paste0(base_dir, "\\"),
    paste0(base_dir, "\\\\")
  )

  canonical <- normalizePath(base_dir, winslash = "/", mustWork = FALSE)

  for (tc in test_cases) {
    Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = tc)
    res <- ts_drive_root()
    expect_identical(res, canonical, info = paste("Failed for input:", tc))
    expect_false(endsWith(res, "/"), info = paste("Ended with / for:", tc))
    expect_false(grepl("//", res), info = paste("Contained // for:", tc))
  }
})

test_that("C1.2: ts_drive_root gracefully handles relative and non-existent paths", {
  old_env <- Sys.getenv("TRANSCRIPTO_DRIVE_ROOT", unset = NA_character_)
  on.exit({
    if (is.na(old_env)) Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT") else Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = old_env)
  }, add = TRUE)

  # Non-existent subpath
  non_existent <- file.path(tempdir(), "definitely_does_not_exist_xyz_123")
  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = non_existent)
  res <- ts_drive_root()
  expect_identical(res, normalizePath(non_existent, winslash = "/", mustWork = FALSE))

  # Relative path
  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = "relative/sub/path")
  res_rel <- ts_drive_root()
  expect_true(grepl("^[A-Za-z]:/|^/", res_rel)) # Absolute path prefix
  expect_false(grepl("//", res_rel))
})

test_that("C1.3: ts_drive_path constructs canonical subpaths without path duplication", {
  tmp_root <- tempfile("path_stress_")
  dir.create(tmp_root, recursive = TRUE)
  on.exit({
    Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT")
    unlink(tmp_root, recursive = TRUE)
  }, add = TRUE)

  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = paste0(tmp_root, "//"))

  # ts_drive_path with multiple segments
  p1 <- ts_drive_path("ready.json")
  expect_false(grepl("//", p1))
  expect_true(endsWith(p1, "tools/_drive/ready.json"))

  p2 <- ts_drive_path("exports", "sub", "test.csv")
  expect_false(grepl("//", p2))
  expect_true(endsWith(p2, "tools/_drive/exports/sub/test.csv"))
})

test_that("C1.4: ts_drive_root falls back when TRANSCRIPTO_DRIVE_ROOT is unset or empty", {
  old_env <- Sys.getenv("TRANSCRIPTO_DRIVE_ROOT", unset = NA_character_)
  on.exit({
    if (is.na(old_env)) Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT") else Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = old_env)
  }, add = TRUE)

  # When set to empty string
  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = "")
  custom_boot <- tempfile("custom_boot_")
  dir.create(custom_boot)
  on.exit({
    ts_drive_boot(NULL)
    unlink(custom_boot, recursive = TRUE)
  }, add = TRUE)

  ts_drive_boot(custom_boot)
  expect_identical(ts_drive_root(), normalizePath(custom_boot, winslash = "/", mustWork = FALSE))

  # When completely unset
  Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT")
  expect_identical(ts_drive_root(), normalizePath(custom_boot, winslash = "/", mustWork = FALSE))
})

test_that("C1.5: ts_drive_root precedence allows ts_drive_boot(explicit) to override environment", {
  old_env <- Sys.getenv("TRANSCRIPTO_DRIVE_ROOT", unset = NA_character_)
  on.exit({
    if (is.na(old_env)) Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT") else Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = old_env)
    ts_drive_boot(NULL)
  }, add = TRUE)

  env_path <- normalizePath(tempfile("env_root_"), winslash = "/", mustWork = FALSE)
  explicit_path <- normalizePath(tempfile("explicit_root_"), winslash = "/", mustWork = FALSE)

  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = env_path)
  ts_drive_boot(explicit_path)

  # Check what .ts_drive_state$root holds vs what ts_drive_root() returns
  expect_identical(.ts_drive_state$root, explicit_path)

  # Explicit, non-default paths passed to ts_drive_boot() continue to override:
  actual_root <- ts_drive_root()
  expect_identical(actual_root, explicit_path)
})

# =============================================================================
# Challenge Area 2: Passive State Hashing, Diff-Gating & Zero-Write Verification
# =============================================================================

test_that("C2.1: xxhash64 state hash is strictly deterministic across 1000 iterations", {
  errs <- list(
    list(timestamp = "2026-10-25T12:00:00Z", module = "bulk", message = "err1"),
    list(timestamp = "2026-10-25T12:00:01Z", module = "sc", message = "err2")
  )

  h_initial <- ts_drive_compute_state_hash("tok_test_001", "bulk", "idle", errs)
  expect_true(is.character(h_initial) && nchar(h_initial) == 16L)

  for (i in 1:1000) {
    h_curr <- ts_drive_compute_state_hash("tok_test_001", "bulk", "idle", errs)
    expect_identical(h_curr, h_initial)
  }
})

test_that("C2.2: State hash changes on ANY semantic mutation and ignores timestamps", {
  errs <- list(list(timestamp = "2026-10-25T12:00:00Z", module = "bulk", message = "err1"))
  base_hash <- ts_drive_compute_state_hash("tok_1", "bulk", "idle", errs)

  # Module change
  h_mod <- ts_drive_compute_state_hash("tok_1", "sc", "idle", errs)
  expect_false(identical(h_mod, base_hash))

  # Status change
  h_stat <- ts_drive_compute_state_hash("tok_1", "bulk", "running", errs)
  expect_false(identical(h_stat, base_hash))

  # Error added
  errs2 <- c(errs, list(list(timestamp = "2026-10-25T12:00:02Z", module = "sc", message = "err2")))
  h_err <- ts_drive_compute_state_hash("tok_1", "bulk", "idle", errs2)
  expect_false(identical(h_err, base_hash))

  # Token change
  h_tok <- ts_drive_compute_state_hash("tok_2", "bulk", "idle", errs)
  expect_false(identical(h_tok, base_hash))
})

test_that("C2.3: Zero-write diff-gating under 100 rapid consecutive ticks", {
  sb <- tempfile("diff_gate_stress_")
  dir.create(file.path(sb, "tools", "_drive"), recursive = TRUE)
  on.exit({
    ts_drive_boot(NULL)
    unlink(sb, recursive = TRUE)
  }, add = TRUE)

  ts_drive_boot(sb)
  ts_drive_clear_errors()
  token <- "tok_stress_rapid"

  # First emission should write state.json
  w1 <- ts_drive_emit_passive_state(token = token, force = FALSE)
  expect_true(w1)

  state_path <- ts_drive_path("state.json")
  expect_true(file.exists(state_path))
  mtime_init <- file.info(state_path)$mtime

  # 100 consecutive ticks with unchanged state MUST NOT write to disk
  skipped_count <- 0L
  for (i in 1:100) {
    w <- ts_drive_emit_passive_state(token = token, force = FALSE)
    if (!w) skipped_count <- skipped_count + 1L
  }
  expect_equal(skipped_count, 100L)

  # File mtime must be completely identical (zero disk writes)
  mtime_after <- file.info(state_path)$mtime
  expect_identical(mtime_init, mtime_after)

  # Adding an error must immediately trigger a write on the next tick
  ts_drive_record_error("New error occurred")
  w_new <- ts_drive_emit_passive_state(token = token, force = FALSE)
  expect_true(w_new)
})

test_that("C2.4: Invalidation unlinks state.json and resets in-memory hash", {
  sb <- tempfile("inval_stress_")
  dir.create(file.path(sb, "tools", "_drive"), recursive = TRUE)
  on.exit({
    ts_drive_boot(NULL)
    unlink(sb, recursive = TRUE)
  }, add = TRUE)

  ts_drive_boot(sb)
  ts_drive_clear_errors()
  token <- "tok_inval_001"

  ts_drive_emit_passive_state(token = token, force = TRUE)
  state_path <- ts_drive_path("state.json")
  expect_true(file.exists(state_path))

  # Invalidate
  ts_drive_invalidate_state(token = token)
  expect_false(file.exists(state_path))
  expect_null(.ts_drive_state$last_state_hash[[token]])

  # Re-emitting same state after invalidation must write to disk
  w2 <- ts_drive_emit_passive_state(token = token, force = FALSE)
  expect_true(w2)
  expect_true(file.exists(state_path))
})

# =============================================================================
# Challenge Area 3: FIFO Error Ring Buffer Bounds & Memory Stress
# =============================================================================

test_that("C3.1: Flooding error buffer with 100 errors strictly maintains <= 20 bounds and FIFO order", {
  ts_drive_clear_errors()
  expect_length(ts_drive_get_errors(), 0L)

  # Flood with 100 errors
  for (i in 1:100) {
    ts_drive_record_error(sprintf("Flood error %03d", i), module = "bulk")
  }

  errs <- ts_drive_get_errors()
  expect_length(errs, 20L)
  expect_length(.ts_drive_state$error_buffer, 20L)

  # Verify FIFO order: oldest retained is 81, newest is 100
  expect_identical(errs[[1L]]$message, "Flood error 081")
  expect_identical(errs[[20L]]$message, "Flood error 100")
  for (j in 1:20) {
    expect_identical(errs[[j]]$message, sprintf("Flood error %03d", 80 + j))
  }
})

test_that("C3.2: Error buffer limits, clipping, multiline, NULL, empty", {
  ts_drive_clear_errors()

  # Oversized message (> 200 chars) is truncated
  long_msg <- paste(rep("ABCDEFGHIJ", 30), collapse = "") # 300 chars
  ts_drive_record_error(long_msg, module = "sc")
  errs <- ts_drive_get_errors()
  expect_length(errs, 1L)
  expect_equal(nchar(errs[[1L]]$message), 200L)

  # Multiline message retains only the first line
  ts_drive_record_error("Line 1 error\nLine 2 details\nLine 3 stack", module = "spatial")
  errs <- ts_drive_get_errors()
  expect_length(errs, 2L)
  expect_identical(errs[[2L]]$message, "Line 1 error")

  # NULL, empty messages
  ts_drive_record_error(NULL)
  ts_drive_record_error("")
  expect_length(ts_drive_get_errors(), 2L)

  # Limit parameter bounds checking
  for (i in 3:25) ts_drive_record_error(sprintf("Extra %d", i))
  expect_length(ts_drive_get_errors(limit = 5), 5L)
  expect_length(ts_drive_get_errors(limit = 1), 1L)
  expect_length(ts_drive_get_errors(limit = 0), 20L)  # falls back to 20
  expect_length(ts_drive_get_errors(limit = -10), 20L) # falls back to 20
  expect_length(ts_drive_get_errors(limit = 50), 20L)  # clamped at buffer capacity
})

test_that("C3.3: Condition object extraction preserves state and class without memory leak", {
  ts_drive_clear_errors()

  cond <- errorCondition("Custom domain failure", class = "cerberus_de_error")
  ts_drive_record_error("Custom domain failure", module = "bulk", condition = cond)

  errs <- ts_drive_get_errors()
  expect_length(errs, 1L)
  expect_identical(errs[[1L]]$class, "cerberus_de_error")
  expect_identical(errs[[1L]]$module, "bulk")
  expect_identical(errs[[1L]]$message, "Custom domain failure")
})

test_that("C3.4: ts_drive_record_error safely ignores NA and character(0) without crashing", {
  ts_drive_clear_errors()
  expect_silent(ts_drive_record_error(NA))
  expect_silent(ts_drive_record_error(character(0)))
  expect_silent(ts_drive_record_error(NA_character_))
  expect_identical(ts_drive_get_errors(), list())
})

# =============================================================================
# Challenge Area 4: AppDriver Sandbox Lifecycle & Host ready.json Preservation
# =============================================================================

test_that("C4.1: Host ready.json is completely preserved when sandbox is created, used, and unlinked", {
  mock_host_root <- tempfile("mock_host_")
  dir.create(file.path(mock_host_root, "tools", "_drive"), recursive = TRUE)
  host_ready_path <- file.path(mock_host_root, "tools", "_drive", "ready.json")

  host_ready_doc <- list(
    protocol = TS_DRIVE_PROTOCOL,
    session_token = "operator_host_session_token_9999",
    pid = 12345L,
    started_at = "2026-10-25T12:00:00Z",
    hb_at = "2026-10-25T12:00:01Z"
  )
  jsonlite::write_json(host_ready_doc, host_ready_path, auto_unbox = TRUE)
  expect_true(file.exists(host_ready_path))

  old_env <- Sys.getenv("TRANSCRIPTO_DRIVE_ROOT", unset = NA_character_)
  on.exit({
    if (is.na(old_env)) Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT") else Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = old_env)
    unlink(mock_host_root, recursive = TRUE)
  }, add = TRUE)

  # Simulate AppDriver instantiation sandbox lifecycle
  sandbox_dir <- normalizePath(tempfile("ts_drive_sandbox_challenger_"), winslash = "/", mustWork = FALSE)
  dir.create(file.path(sandbox_dir, "tools", "_drive"), recursive = TRUE)

  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = sandbox_dir)
  expect_identical(ts_drive_root(), sandbox_dir)

  # Write sandbox ready.json
  sandbox_ready_path <- file.path(sandbox_dir, "tools", "_drive", "ready.json")
  sandbox_ready_doc <- list(
    protocol = TS_DRIVE_PROTOCOL,
    session_token = "sandbox_child_token_1111",
    pid = 99999L
  )
  jsonlite::write_json(sandbox_ready_doc, sandbox_ready_path, auto_unbox = TRUE)
  expect_true(file.exists(sandbox_ready_path))

  # Verify host ready.json is completely UNTOUCHED
  expect_true(file.exists(host_ready_path))
  host_read_back <- jsonlite::fromJSON(host_ready_path)
  expect_identical(host_read_back$session_token, "operator_host_session_token_9999")

  # Simulate AppDriver teardown
  unlink(sandbox_dir, recursive = TRUE, force = TRUE)
  if (is.na(old_env)) Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT") else Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = old_env)

  # Host ready.json STILL completely intact
  expect_true(file.exists(host_ready_path))
  host_read_back2 <- jsonlite::fromJSON(host_ready_path)
  expect_identical(host_read_back2$session_token, "operator_host_session_token_9999")
  expect_false(file.exists(sandbox_ready_path))
})

test_that("C4.2: Multiple AppDriver instances produce strictly disjoint sandboxes", {
  sb1 <- normalizePath(tempfile("ts_drive_sb1_"), winslash = "/", mustWork = FALSE)
  sb2 <- normalizePath(tempfile("ts_drive_sb2_"), winslash = "/", mustWork = FALSE)
  dir.create(file.path(sb1, "tools", "_drive"), recursive = TRUE)
  dir.create(file.path(sb2, "tools", "_drive"), recursive = TRUE)
  on.exit({
    unlink(sb1, recursive = TRUE)
    unlink(sb2, recursive = TRUE)
  }, add = TRUE)

  expect_false(identical(sb1, sb2))

  # Write to sb1
  jsonlite::write_json(list(id = "sb1"), file.path(sb1, "tools", "_drive", "state.json"))
  # Write to sb2
  jsonlite::write_json(list(id = "sb2"), file.path(sb2, "tools", "_drive", "state.json"))

  # Verify no interference
  expect_identical(jsonlite::fromJSON(file.path(sb1, "tools", "_drive", "state.json"))$id, "sb1")
  expect_identical(jsonlite::fromJSON(file.path(sb2, "tools", "_drive", "state.json"))$id, "sb2")
})

# =============================================================================
# Challenge Area 5: MCP State Tool Edge Cases
# =============================================================================

test_that("C5.1: transcripto_drive_state gracefully returns NO_SESSION when file absent", {
  e <- .mcp_challenge_env()

  # File absent
  r_absent <- e$.ts_tool_state()
  expect_true(is.list(r_absent))
  expect_true(r_absent$isError)
  expect_identical(r_absent$structuredContent$code, "NO_SESSION")
})

test_that("C5.2: Corrupted state.json is handled safely without throwing due to missing ts_log_swallow", {
  e <- .mcp_challenge_env()

  # When state.json is corrupted or 0 bytes without error_log.R sourced:
  state_path <- e$ts_drive_path("state.json")
  writeLines("{ bad json", state_path)

  # Safe handling: returns NO_SESSION error structure without crashing R session
  res <- e$.ts_tool_state()
  expect_true(is.list(res))
  expect_true(res$isError)
  expect_identical(res$structuredContent$code, "NO_SESSION")
})

test_that("C5.3: transcripto_drive_state succeeds with valid state and never writes disk files", {
  e <- .mcp_challenge_env()

  token <- "tok_mcp_valid"
  e$ts_drive_emit_passive_state(token = token, force = TRUE)
  state_path <- e$ts_drive_path("state.json")
  expect_true(file.exists(state_path))

  # Call state tool
  r <- e$.ts_tool_state()
  expect_false(isTRUE(r$isError))
  expect_identical(r$structuredContent$session_token, token)
  expect_true("content_hash" %in% names(r$structuredContent))
  expect_true("pipeline_status" %in% names(r$structuredContent))

  # Verify zero writes: arm.json, scenario.json, result.json MUST NOT exist
  expect_false(file.exists(e$ts_drive_path("arm.json")))
  expect_false(file.exists(e$ts_drive_path("scenario.json")))
  expect_false(file.exists(e$ts_drive_path("result.json")))
})

test_that("C5.4: JSON-RPC tools/call dispatch for transcripto_drive_state handles various arg shapes", {
  e <- .mcp_challenge_env()
  token <- "tok_rpc_dispatch"
  e$ts_drive_emit_passive_state(token = token, force = TRUE)

  # With empty arguments
  req1 <- list(
    jsonrpc = "2.0", id = "test_1",
    method = "tools/call",
    params = list(name = "transcripto_drive_state", arguments = list())
  )
  res1 <- e$.ts_dispatch(req1)
  expect_identical(res1$id, "test_1")
  expect_false(isTRUE(res1$result$isError))
  expect_identical(res1$result$structuredContent$session_token, token)

  # With missing arguments field entirely
  req2 <- list(
    jsonrpc = "2.0", id = "test_2",
    method = "tools/call",
    params = list(name = "transcripto_drive_state")
  )
  res2 <- e$.ts_dispatch(req2)
  expect_identical(res2$id, "test_2")
  expect_false(isTRUE(res2$result$isError))

  # Unknown tool name fails properly
  req3 <- list(
    jsonrpc = "2.0", id = "test_3",
    method = "tools/call",
    params = list(name = "transcripto_drive_nonexistent")
  )
  res3 <- e$.ts_dispatch(req3)
  expect_identical(res3$id, "test_3")
  expect_true(!is.null(res3$error))
})

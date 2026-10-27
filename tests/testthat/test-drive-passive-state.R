# =============================================================================
# test-drive-passive-state.R — Test suite for Features 10-12: Passive Observation
# =============================================================================

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

.mcp_server_script <- function() {
  file.path(ts_project_root(), "scripts", "mcp_server.R")
}

.mcp_passive_env <- function() {
  path <- .mcp_server_script()
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
  if (is.na(first) || is.na(last) || last <= first) {
    stop("mcp_server.R internal definition boundaries were not found")
  }
  for (i in seq.int(first, last - 1L)) eval(expressions[[i]], envir = e)

  root <- file.path(tempdir(), paste0("tsdrive-mcp-pass-", as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(root, "tools", "_drive"), recursive = TRUE, showWarnings = FALSE)
  e$ts_drive_boot(root)
  e
}

# ── Feature 10: Passive state.json Digest & Diff-Gating ──────────────────────

test_that("state.json conforms to frozen schema when emitted", {
  sb <- tempfile("sb_pass_")
  dir.create(file.path(sb, "tools", "_drive"), recursive = TRUE)
  on.exit(unlink(sb, recursive = TRUE), add = TRUE)
  ts_drive_boot(sb)
  ts_drive_clear_errors()

  token <- "tok_pass_001"
  written <- ts_drive_emit_passive_state(token = token, force = TRUE)
  expect_true(written)

  st <- ts_drive_read_state()
  expect_false(is.null(st))
  expect_identical(st$session_token, token)
  expect_true(!is.null(st$content_hash) && nchar(st$content_hash) > 0L)
  expect_true(!is.null(st$timestamp))
  expect_true(st$pipeline_status %in% c("idle", "running", "error"))
  expect_true(is.list(st$errors))
  expect_identical(length(st$errors), 0L)
})

test_that("emitter: ring error older than the recency window keeps status idle", {
  sb <- tempfile("sb_recency_")
  dir.create(file.path(sb, "tools", "_drive"), recursive = TRUE)
  on.exit(unlink(sb, recursive = TRUE), add = TRUE)
  ts_drive_boot(sb)
  ts_drive_clear_errors()

  # Jalon filets (2026-10-27, O18) : la récence a une FENÊTRE. Une erreur plus
  # vieille que TS_DRIVE_ERROR_RECENCY_SECS reste listée mais ne porte plus le
  # statut "error" — le digest ne ment pas sur la santé courante.
  old_ts <- format(Sys.time() - 3600, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ts_drive_record_error("erreur ancienne hors fenêtre", module = "bulk_de", timestamp = old_ts)
  w <- ts_drive_emit_passive_state(token = "tok_recency_old", force = TRUE)
  expect_true(w)
  st <- ts_drive_read_state()
  expect_identical(length(st$errors), 1L)
  expect_identical(st$pipeline_status, "idle")
})

test_that("recency helper: TRUE within window, FALSE for old, malformed, or empty", {
  iso_now <- function(offset_secs) {
    format(Sys.time() + offset_secs, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }
  expect_true(.ts_drive_has_recent_error(list(
    list(timestamp = iso_now(-30), module = "sc_pipeline", message = "récente"))))
  expect_true(.ts_drive_has_recent_error(list(
    list(timestamp = iso_now(-899), module = "sc_pipeline", message = "limite haute"))))
  expect_false(.ts_drive_has_recent_error(list(
    list(timestamp = iso_now(-901), module = "sc_pipeline", message = "limite basse"))))
  expect_false(.ts_drive_has_recent_error(list(
    list(timestamp = iso_now(-7200), module = "sc_pipeline", message = "vieille"))))
  # La NOUVELLE entrée décide : une entrée vieille puis une récente ⇒ récent.
  expect_true(.ts_drive_has_recent_error(list(
    list(timestamp = iso_now(-7200), module = "a", message = "x"),
    list(timestamp = iso_now(-10),   module = "b", message = "y"))))
  # Jamais de crash sur un tampon malformé — jamais récent.
  expect_false(.ts_drive_has_recent_error(list(list(timestamp = "n'importe quoi"))))
  expect_false(.ts_drive_has_recent_error(list(list(message = "sans timestamp"))))
  expect_false(.ts_drive_has_recent_error(list()))
  expect_false(.ts_drive_has_recent_error(NULL))
})

test_that("state.json content hash is computed deterministically with xxhash64", {
  token <- "tok_det_123"
  mod <- "bulk_de"
  status <- "idle"
  errs <- list()

  h1 <- ts_drive_compute_state_hash(token, mod, status, errs)
  h2 <- ts_drive_compute_state_hash(token, mod, status, errs)
  expect_identical(h1, h2)
  expect_type(h1, "character")
  expect_equal(nchar(h1), 16L)

  h3 <- ts_drive_compute_state_hash(token, "bulk_pathways", status, errs)
  expect_false(identical(h1, h3))
})

test_that("diff-gating avoids redundant disk writes when state is unchanged", {
  sb <- tempfile("sb_diff_")
  dir.create(file.path(sb, "tools", "_drive"), recursive = TRUE)
  on.exit(unlink(sb, recursive = TRUE), add = TRUE)
  ts_drive_boot(sb)
  ts_drive_clear_errors()

  token <- "tok_diff_002"
  w1 <- ts_drive_emit_passive_state(token = token)
  expect_true(w1)

  state_path <- ts_drive_path("state.json")
  expect_true(file.exists(state_path))
  mtime1 <- file.info(state_path)$mtime

  # Second tick with no state change: MUST be skipped by diff-gating
  Sys.sleep(0.05)
  w2 <- ts_drive_emit_passive_state(token = token)
  expect_false(w2)
  mtime2 <- file.info(state_path)$mtime
  expect_identical(mtime1, mtime2)
})

test_that("recording an error changes state hash and triggers state.json rewrite", {
  sb <- tempfile("sb_err_diff_")
  dir.create(file.path(sb, "tools", "_drive"), recursive = TRUE)
  on.exit(unlink(sb, recursive = TRUE), add = TRUE)
  ts_drive_boot(sb)
  ts_drive_clear_errors()

  token <- "tok_err_diff_003"
  ts_drive_emit_passive_state(token = token)
  st1 <- ts_drive_read_state()
  expect_equal(length(st1$errors), 0L)

  # Record an error in module
  Sys.sleep(0.05)
  ts_drive_record_error("bulk_de", "DESeq2 model convergence failed")
  w_new <- ts_drive_emit_passive_state(token = token)
  expect_true(w_new)

  st2 <- ts_drive_read_state()
  expect_equal(length(st2$errors), 1L)
  expect_false(identical(st1$content_hash, st2$content_hash))
  expect_identical(st2$errors[[1]]$message, "DESeq2 model convergence failed")
})

test_that("ts_drive_invalidate_state unlinks state.json and clears cached hash", {
  sb <- tempfile("sb_inval_")
  dir.create(file.path(sb, "tools", "_drive"), recursive = TRUE)
  on.exit(unlink(sb, recursive = TRUE), add = TRUE)
  ts_drive_boot(sb)

  token <- "tok_inval_004"
  ts_drive_emit_passive_state(token = token, force = TRUE)
  expect_true(file.exists(ts_drive_path("state.json")))

  ts_drive_invalidate_state(token)
  expect_false(file.exists(ts_drive_path("state.json")))
  expect_null(ts_drive_read_state())
})

# ── Feature 11: Bounded Error Ring Buffer ────────────────────────────────────

test_that("error ring buffer strictly caps capacity at 20 entries", {
  ts_drive_clear_errors()

  for (i in 1:25) {
    ts_drive_record_error("module_test", sprintf("Error message %d", i))
  }

  errs <- ts_drive_get_errors(limit = 100L)
  expect_equal(length(errs), 20L)
  expect_identical(errs[[1]]$message, "Error message 6")
  expect_identical(errs[[20]]$message, "Error message 25")
})

test_that("error ring items conform to schema with timestamp, module, message", {
  ts_drive_clear_errors()
  ts_drive_record_error("sc_qc", "Mitochondrial fraction exceeds threshold")

  errs <- ts_drive_get_errors()
  expect_equal(length(errs), 1L)
  item <- errs[[1]]
  expect_true(all(c("timestamp", "module", "message") %in% names(item)))
  expect_identical(item$module, "sc_qc")
  expect_identical(item$message, "Mitochondrial fraction exceeds threshold")
})

test_that("ts_drive_clear_errors flushes the ring buffer", {
  ts_drive_clear_errors()
  ts_drive_record_error("bulk_filter", "Low count filter error")
  expect_equal(length(ts_drive_get_errors()), 1L)

  ts_drive_clear_errors()
  expect_equal(length(ts_drive_get_errors()), 0L)
})

# ── Feature 12: MCP transcripto_drive_state Tool ─────────────────────────────

test_that("transcripto_drive_state tool is registered in MCP server with correct schema", {
  e <- .mcp_passive_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)

  tools <- e$.ts_tools()
  tool_names <- vapply(tools, function(x) x$name, character(1))
  expect_true("transcripto_drive_state" %in% tool_names)

  state_tool <- tools[[which(tool_names == "transcripto_drive_state")[1L]]]
  expect_identical(state_tool$inputSchema$type, "object")
  expect_equal(length(state_tool$inputSchema$properties), 0L)
  expect_false(isTRUE(state_tool$inputSchema$additionalProperties))
})

test_that("transcripto_drive_state returns NO_SESSION when state.json is absent", {
  e <- .mcp_passive_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)

  # state.json is absent
  unlink(e$ts_drive_path("state.json"))

  res <- e$.ts_tool_state()
  expect_true(res$isError)
  expect_identical(res$structuredContent$code, "NO_SESSION")
})

test_that("transcripto_drive_state returns passive state without arming session", {
  e <- .mcp_passive_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)

  token <- "tok_mcp_pass_005"
  e$ts_drive_emit_passive_state(token = token, force = TRUE)

  res <- e$.ts_tool_state()
  expect_false(res$isError)
  expect_identical(res$structuredContent$session_token, token)
  expect_true(!is.null(res$structuredContent$content_hash))
  expect_true(is.list(res$structuredContent$errors))

  # Calling transcripto_drive_state never writes arm.json
  expect_false(file.exists(e$ts_drive_path("arm.json")))
})

test_that("transcripto_drive_state dispatches via JSON-RPC protocol correctly", {
  e <- .mcp_passive_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)

  token <- "tok_rpc_006"
  e$ts_drive_emit_passive_state(token = token, force = TRUE)

  req <- list(jsonrpc = "2.0", id = 42L, method = "tools/call",
              params = list(name = "transcripto_drive_state", arguments = list()))
  ans <- e$.ts_dispatch(req)
  expect_identical(ans$jsonrpc, "2.0")
  expect_identical(ans$id, 42L)
  expect_false(isTRUE(ans$result$isError))
  expect_identical(ans$result$structuredContent$session_token, token)
})

.mcp_sc_local_script <- function() {
  path <- file.path(ts_project_root(), "scripts", "mcp_server.R")
  if (!file.exists(path)) {
    skip("the MCP server is a local, gitignored capability and is absent from this clone")
  }
  path
}

.mcp_sc_local_env <- function() {
  path <- .mcp_sc_local_script()
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "R", "core", "drive_allowlist.R"), envir = e)
  sys.source(file.path(ts_project_root(), "R", "core", "drive_watcher.R"), envir = e)
  expressions <- parse(path, keep.source = TRUE)
  labels <- vapply(expressions, function(x) paste(deparse(x, width.cutoff = 500L), collapse = "\n"), character(1))
  first <- which(grepl("^\\.ts_json <- function", labels))[1L]
  last <- which(grepl("^\\.ts_serve <- function", labels))[1L]
  if (is.na(first) || is.na(last) || last <= first) {
    stop("mcp_server.R internal definition boundaries were not found")
  }
  for (i in seq.int(first, last - 1L)) eval(expressions[[i]], envir = e)
  e
}

test_that("local MCP keeps EIGHT tools and resolves the three SC action buttons", {
  e <- .mcp_sc_local_env()
  tools <- e$.ts_tools()
  # THE INVARIANT, restated rather than deleted â€” and the distinction is the whole
  # point of the edit. The original line read "The inventory is FROZEN: adding a
  # MODULE must never add a tool", and `expect_length(tools, 7L)` was its
  # enforcement. S2 added an EIGHTH tool for a new ACTION (`export_result`), not
  # for a module, so the rule was not broken â€” but the ENFORCEMENT no longer
  # expressed the rule, and a pin that has quietly become a snapshot is worse than
  # no pin: the next module would have had to widen it again, and nobody could tell
  # a module from a verb by looking at the diff.
  #
  # So both are now stated: the rule (a module never adds a tool) is asserted
  # directly below over the module inventory, and the declared surface is pinned as
  # a separate, visible list.
  expect_length(tools, 8L)
  expect_setequal(vapply(tools, function(x) x$name, character(1)), c(
    "transcripto_drive_status", "transcripto_drive_read_result",
    "transcripto_drive_snapshot", "transcripto_drive_set_inputs",
    "transcripto_drive_run", "transcripto_drive_wait",
    "transcripto_drive_set_armed", "transcripto_drive_export"
  ))
  # THE RULE, enforced where it can be: every module in the app's own table must be
  # reachable through the EXISTING tools, so a new module is a new entry in
  # TS_MCP_RUN_BUTTONS / TS_MCP_SET_INPUT_MODULES and never a new tool name.
  nms <- vapply(tools, function(x) x$name, character(1))
  expect_false(any(grepl("^(sc|bulk|spatial)[_-]", nms)))
  # ... and every tool name is verb-shaped, never module-shaped.
  expect_true(all(grepl("^transcripto_drive_[a-z_]+$", nms)))
  expect_length(e$.ts_mcp_run_problems(), 0L)
  expect_identical(e$TS_MCP_RUN_ACTIONS, "run_pipeline")

  resolved <- e$.ts_mcp_resolve_button("sc_pipeline", NULL)
  expect_true(resolved$ok)
  expect_identical(resolved$button, "sc-pipeline-run_auto_pipeline")
  expect_true(e$.ts_mcp_resolve_button("sc_pipeline", "sc-pipeline-run_auto_pipeline")$ok)
  annotation <- e$.ts_mcp_resolve_button("sc_annotation", NULL)
  expect_true(annotation$ok)
  expect_identical(annotation$button, "sc-annotation-run_annot")
  expect_true(e$.ts_mcp_resolve_button("sc_annotation", "sc-annotation-run_annot")$ok)
  markers <- e$.ts_mcp_resolve_button("sc_markers", NULL)
  expect_true(markers$ok)
  expect_identical(markers$button, "sc-markers-run_markers")
  expect_true(e$.ts_mcp_resolve_button("sc_markers", "sc-markers-run_markers")$ok)
  expect_false(e$.ts_mcp_resolve_button("sc_pipeline", "sc-btn_auto_pipeline_sc")$ok)
  expect_false(e$.ts_mcp_resolve_button("sc_pipeline", "sc-sc_ap_confirm")$ok)
  expect_false(e$.ts_mcp_resolve_button("sc_pipeline", "bulk-de-run_de")$ok)
  expect_false(e$.ts_mcp_resolve_button("sc_annotation", "sc-pipeline-run_auto_pipeline")$ok)
  expect_false(e$.ts_mcp_resolve_button("sc_markers", "sc-annotation-run_annot")$ok)
})

test_that("local MCP exposes SC for run and wait but not set_inputs", {
  e <- .mcp_sc_local_env()
  tools <- stats::setNames(e$.ts_tools(), vapply(e$.ts_tools(), function(x) x$name, character(1)))
  set_modules <- unlist(tools$transcripto_drive_set_inputs$inputSchema$properties$module$enum)
  run_modules <- unlist(tools$transcripto_drive_run$inputSchema$properties$module$enum)
  wait_modules <- unlist(tools$transcripto_drive_wait$inputSchema$properties$module$enum)
  run_buttons <- unlist(tools$transcripto_drive_run$inputSchema$properties$button$enum)

  # The frozen input set is DECLARED, not injected: `set_inputs` must not offer
  # the SC module, or an agent could rewrite the very defaults under test.
  expect_false("sc_pipeline" %in% set_modules)
  expect_false("sc_annotation" %in% set_modules)
  expect_false("sc_markers" %in% set_modules)
  expect_true("sc_pipeline" %in% run_modules)
  expect_true("sc_annotation" %in% run_modules)
  expect_true("sc_markers" %in% run_modules)
  expect_true("sc_pathways" %in% run_modules)
  expect_true("sc_pipeline" %in% wait_modules)
  expect_true("sc_annotation" %in% wait_modules)
  expect_true("sc_markers" %in% wait_modules)
  expect_true("sc_pathways" %in% wait_modules)
  expect_true("sc-pipeline-run_auto_pipeline" %in% run_buttons)
  expect_true("sc-annotation-run_annot" %in% run_buttons)
  expect_true("sc-markers-run_markers" %in% run_buttons)
  expect_true("sc-pathways-run_pathway" %in% run_buttons)
  sc_buttons <- run_buttons[startsWith(run_buttons, "sc-")]
  # FOUR, since Phase E. The `sc-` surface is a CLOSED set, so this pin is what
  # makes the next addition a visible edit here and in the app's own rail.
  expect_setequal(sc_buttons, c("sc-pipeline-run_auto_pipeline",
                                "sc-annotation-run_annot",
                                "sc-markers-run_markers",
                                "sc-pathways-run_pathway"))
  expect_false(any(grepl("^(sc-mapping|sc-pipeline-(run_annot)|sc-da|sc-velocity|import_sc)-",
                         run_buttons)))
})

test_that("local MCP snapshot projects the SC step record and leaks nothing", {
  e <- .mcp_sc_local_env()
  modules <- list(
    import_bulk = list(n_results = 1L),
    bulk_filter = list(n_results = 2L),
    bulk_de = list(n_results = 3L),
    bulk_pathways = list(n_results = 4L),
    spatial_pipeline = list(n_results = 5L, status = "done"),
    sc_pipeline = list(
      module = "sc_pipeline", action = "run_pipeline", status = "done",
      elapsed_s = 12.5, seq = 4L, n_results = 6L, has_data = TRUE, ready = TRUE,
      steps = list(qc = "ran", norm = "ran", pca = "ran", clusters = "ran",
                   umap = "ran", tsne = "ignored", singler = "skipped",
                   markers = "ran", pathway = "error", correlation = "skipped",
                   trajectory = "ran"),
      # Exactly the payloads the disclosure contract forbids.
      top_marker = "SENTINEL_GENE_NAME", root_cell = "SENTINEL_CELL_ID",
      project = "C:/SENTINEL_PATH", log = "SENTINEL_GENE_NAME found"
    ),
    sc_annotation = list(
      module = "sc_annotation", action = "run_pipeline", status = "done",
      elapsed_s = 4.5, seq = 5L, n_results = 2L, has_data = TRUE, ready = TRUE,
      steps = list(singler = "ran"),
      project = "C:/SENTINEL_PATH"
    ),
    sc_markers = list(
      module = "sc_markers", action = "run_pipeline", status = "done",
      elapsed_s = 6.5, seq = 6L, n_results = 3L, has_data = TRUE, ready = TRUE,
      steps = list(markers = "ran"),
      project = "C:/SENTINEL_PATH"
    )
  )
  projected <- e$.ts_project_snapshot(list(modules = modules))

  # An eighth module must not be truncated away.
  expect_true("sc_pipeline" %in% names(projected$modules))
  expect_true("sc_annotation" %in% names(projected$modules))
  expect_true("sc_markers" %in% names(projected$modules))
  expect_false(isTRUE(projected$truncated))
  expect_identical(projected$modules$sc_pipeline$status, "done")
  expect_identical(projected$modules$sc_pipeline$n_results, 6L)
  expect_identical(projected$modules$sc_pipeline$elapsed_s, 12.5)
  expect_identical(projected$modules$sc_annotation$n_results, 2L)
  expect_identical(projected$modules$sc_annotation$steps$singler, "ran")
  expect_identical(projected$modules$sc_markers$n_results, 3L)
  expect_identical(projected$modules$sc_markers$steps$markers, "ran")

  # The per-step record crosses the wire AS SCALARS, `ignored` and `error`
  # included: the agent must be able to see that a step was declined or failed.
  steps <- projected$modules$sc_pipeline$steps
  expect_setequal(names(steps), c("qc", "norm", "pca", "clusters", "umap", "tsne",
                                  "singler", "markers", "pathway", "correlation",
                                  "trajectory"))
  expect_identical(steps$tsne, "ignored")
  expect_identical(steps$pathway, "error")
  expect_identical(steps$singler, "skipped")

  expect_false(any(grepl("SENTINEL", e$.ts_json(projected), fixed = TRUE)))
  for (forbidden in c("top_marker", "root_cell", "project", "log")) {
    expect_false(forbidden %in% names(projected$modules$sc_pipeline), info = forbidden)
  }
  # And the published contract still declares what it never publishes.
  note <- e$.ts_redaction_note(projected$dropped, projected$truncated)
  expect_match(note$policy, "snapshot-allowlist")
  expect_true(all(c("gene names", "spot or cell IDs", "matrices", "raw log text") %in%
                    note$never_published))
  # 10 since Phase D: the Bulk pattern action is the tenth drivable module, and
  # a smaller cap would truncate a runnable module out of every snapshot.
  expect_identical(note$limits$max_modules, 10L)
})

test_that("local MCP run writes the SC scenarios into an external fixture root", {
  e <- .mcp_sc_local_env()
  real_drive <- list.files(file.path(ts_project_root(), "tools", "_drive"), recursive = TRUE)
  root <- tempfile("ts-mcp-sc-")
  dir.create(file.path(root, "tools", "_drive"), recursive = TRUE, showWarnings = FALSE)
  on.exit({
    unlink(root, recursive = TRUE, force = TRUE)
    expect_setequal(list.files(file.path(ts_project_root(), "tools", "_drive"),
                               recursive = TRUE), real_drive)
  }, add = TRUE)

  e$ts_drive_boot(root)
  e$ts_drive_clear_write_error()
  e$ts_drive_job_clear()
  token <- "sctok"
  started <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  e$ts_drive_write_ready(list(), token, armed = TRUE, last_seq = 0L,
                         hb_n = 0L, started_at = started)
  e$ts_drive_write_json(
    list(protocol = e$TS_DRIVE_PROTOCOL, token = token, armed = TRUE),
    e$ts_drive_path("arm.json")
  )
  session_id <- e$.ts_session_id(Sys.getpid(), started, token)
  expectation <- list(session_id = session_id, pid = Sys.getpid(), started_at = started)

  response <- e$.ts_tool_run(23L, "sc_pipeline", NULL, TRUE, expectation)
  expect_false(response$isError)
  expect_true(response$structuredContent$accepted)
  expect_identical(response$structuredContent$button, "sc-pipeline-run_auto_pipeline")

  scenario <- jsonlite::fromJSON(e$ts_drive_path("scenario.json"), simplifyVector = FALSE)
  expect_identical(scenario$protocol, e$TS_DRIVE_PROTOCOL)
  expect_identical(as.integer(scenario$seq), 23L)
  expect_identical(scenario$module, "sc_pipeline")
  expect_identical(scenario$action, "run_pipeline")
  expect_identical(scenario$button, "sc-pipeline-run_auto_pipeline")
  # No inputs: the parameter set is frozen in code, not in the scenario.
  expect_length(scenario$inputs, 0L)

  response_annot <- e$.ts_tool_run(24L, "sc_annotation", NULL, TRUE, expectation)
  expect_false(response_annot$isError)
  expect_true(response_annot$structuredContent$accepted)
  expect_identical(response_annot$structuredContent$button, "sc-annotation-run_annot")

  scenario <- jsonlite::fromJSON(e$ts_drive_path("scenario.json"), simplifyVector = FALSE)
  expect_identical(scenario$module, "sc_annotation")
  expect_identical(scenario$action, "run_pipeline")
  expect_identical(scenario$button, "sc-annotation-run_annot")
  expect_length(scenario$inputs, 0L)

  response_markers <- e$.ts_tool_run(25L, "sc_markers", NULL, TRUE, expectation)
  expect_false(response_markers$isError)
  expect_true(response_markers$structuredContent$accepted)
  expect_identical(response_markers$structuredContent$button, "sc-markers-run_markers")

  scenario <- jsonlite::fromJSON(e$ts_drive_path("scenario.json"), simplifyVector = FALSE)
  expect_identical(scenario$module, "sc_markers")
  expect_identical(scenario$action, "run_pipeline")
  expect_identical(scenario$button, "sc-markers-run_markers")
  expect_length(scenario$inputs, 0L)
})

# =============================================================================
# S2 — the export tool's SCHEMA is the trust boundary, and a schema a client
# ignores is not a boundary. So the schema is asserted here as data: only `seq` and
# `expect`, `additionalProperties = FALSE` at BOTH levels, and no property whose
# name could carry a destination.
# =============================================================================
test_that("the export tool exposes only `seq` and `expect`, at both levels", {
  e <- .mcp_sc_local_env()
  tools <- e$.ts_tools()
  ex <- Filter(function(x) identical(x$name, "transcripto_drive_export"), tools)
  expect_length(ex, 1L)
  if (!length(ex)) return(invisible(NULL))
  sch <- ex[[1]]$inputSchema
  expect_identical(sch$type, "object")
  expect_setequal(names(sch$properties), c("seq", "expect"))
  expect_setequal(sch$required, c("seq", "expect"))
  # Closed at the top level and closed inside `expect`: a field smuggled into
  # `expect` would otherwise reach the session assertion unchecked.
  expect_false(sch$additionalProperties)
  expect_false(sch$properties$expect$additionalProperties)
  expect_setequal(names(sch$properties$expect$properties),
                  c("pid", "started_at", "session_token", "session_id"))
  # And nothing in the schema could be read as a destination, a handler or a
  # format. Checked against the names rather than trusted from the description.
  # Not an AST walker: a recursive collector of the character leaves of the
  # schema. It avoids the reserved names walk()/visit() on purpose — the
  # helper-ast-no-local-walk guard reserves them for the shared AST traversal.
  collect_char_keys <- function(x) {
    if (is.character(x)) return(x)
    if (!is.list(x)) return(character(0))
    unlist(lapply(x, collect_char_keys), use.names = FALSE)
  }
  keys <- collect_char_keys(sch)
  banned <- c("path", "file", "filename", "dir", "dest", "where",
              "format", "type", "ext", "handler", "output", "output_id", "id")
  for (b in banned) {
    expect_false(b %in% keys, info = sprintf("the export schema must not expose `%s`", b))
  }
  # The module is not a parameter either: the route table has one entry and the app
  # resolves it. A `module` argument would be a caller choosing an artefact.
  expect_false("module" %in% names(sch$properties))
})

test_that("the export scenario payload is REBUILT, and carries no import block", {
  e <- .mcp_sc_local_env()
  p <- e$.ts_export_payload(7L, "sometoken")
  expect_identical(p$seq, 7L)
  expect_identical(p$action, "export_result")
  expect_identical(p$module, e$TS_MCP_EXPORT_MODULE)
  expect_identical(names(p), c("protocol", "seq", "session_token", "module", "action"))
  # No `import` block at all: the app-side validator refuses every key, so sending
  # even an empty one would be a refusal waiting to happen.
  expect_false("import" %in% names(p))
  expect_false("inputs" %in% names(p))
  expect_false("button" %in% names(p))
})
# =============================================================================
# THE ARMING BOOTSTRAP (gapF §2.5 item 3).
#
# `ts_drive_write_ready()` beats only while the session is ARMED
# (drive_watcher.R: the `cursor$armed && due` guard), so an unarmed session's
# handshake goes stale after `ts_drive_hb_timeout()` (15 s) — and
# `transcripto_drive_set_armed` refused stale handshakes. The first action of
# every session therefore had to bypass the tool by hand-writing `arm.json`.
#
# The fix: arm/disarm — and ONLY arm/disarm — may act on a stale handshake when
# the caller PINS the session identity (session_id, or pid + started_at) and the
# pinned pid is ALIVE. Arming is the one write that RESTORES liveness, so it is
# the one write a stale handshake cannot corrupt; every other write keeps the
# strict rule. A wrong pin still fails closed (SESSION_MISMATCH), and a dead pid
# is never rescued.
# =============================================================================

.mcp_sc_local_stale_fixture <- function(e) {
  root <- tempfile("ts-mcp-arm-")
  dir.create(file.path(root, "tools", "_drive"), recursive = TRUE, showWarnings = FALSE)
  e$ts_drive_boot(root)
  e$ts_drive_clear_write_error()
  e$ts_drive_job_clear()
  token <- "armtok"
  started <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  e$ts_drive_write_ready(list(), token, armed = FALSE, last_seq = 0L,
                         hb_n = 1L, started_at = started)
  # Age the heartbeat beyond the 15 s timeout WITHOUT touching mtime: staleness
  # is read from the payload's own `hb_at`, so rewriting the field is the honest
  # way to build the fixture.
  hb <- jsonlite::fromJSON(e$ts_drive_path("ready.json"), simplifyVector = FALSE)
  hb$hb_at <- format(Sys.time() - 120, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  e$ts_drive_write_json(hb, e$ts_drive_path("ready.json"))
  expect_false(e$ts_drive_ready_fresh())
  list(token = token, started = started,
       session_id = e$.ts_session_id(Sys.getpid(), started, token))
}

test_that("the arming bootstrap: a stale handshake is rescued only by a live pin", {
  e <- .mcp_sc_local_env()
  fx <- .mcp_sc_local_stale_fixture(e)
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)

  # A — stale, UNPINNED: refused exactly as before the fix.
  r <- e$.ts_tool_set_armed(TRUE, NULL)
  expect_true(r$isError)
  expect_identical(r$structuredContent$code, "STALE_SESSION")
  expect_false(file.exists(e$ts_drive_path("arm.json")))

  # B — stale, pinned by session_id, pid ALIVE: rescued. This is the bootstrap.
  r <- e$.ts_tool_set_armed(TRUE, list(session_id = fx$session_id))
  expect_false(r$isError)
  expect_true(r$structuredContent$wrote)
  expect_true(r$structuredContent$rescued_stale)
  arm <- jsonlite::fromJSON(e$ts_drive_path("arm.json"), simplifyVector = FALSE)
  expect_identical(arm$token, fx$token)
  expect_true(arm$armed)

  # C — stale, WRONG pin: fail closed on identity, never on staleness.
  r <- e$.ts_tool_set_armed(TRUE, list(session_id = "00000000deadbeef"))
  expect_true(r$isError)
  expect_identical(r$structuredContent$code, "SESSION_MISMATCH")

  # D — stale, correct pin, but the pinned pid is NOT alive: no rescue.
  # `.ts_pid_alive` is stubbed because the fixture's pid IS this test process;
  # the stub isolates the rule under test from the host's process table.
  e$.ts_pid_alive <- function(pid) FALSE
  r <- e$.ts_tool_set_armed(FALSE, list(session_id = fx$session_id))
  expect_true(r$isError)
  expect_identical(r$structuredContent$code, "STALE_SESSION")
  # The disarm that never landed must not have touched the armed state above.
  arm <- jsonlite::fromJSON(e$ts_drive_path("arm.json"), simplifyVector = FALSE)
  expect_true(arm$armed)
})

test_that("the arming bootstrap does not loosen the OTHER writes", {
  e <- .mcp_sc_local_env()
  fx <- .mcp_sc_local_stale_fixture(e)
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  expect <- list(session_id = fx$session_id, pid = Sys.getpid(),
                 started_at = fx$started)

  # set_inputs, run and export keep the strict rule: a stale handshake is
  # refused even with a perfect live pin, because those writes CHANGE analysis
  # state and a stale heartbeat is exactly when the session's true state is
  # unknown.
  r <- e$.ts_tool_set_inputs(5L, "bulk_filter", list("bulk-filter-min_count" = 10), TRUE, expect)
  expect_true(r$isError)
  expect_identical(r$structuredContent$code, "STALE_SESSION")

  r <- e$.ts_tool_run(6L, "bulk_filter", NULL, TRUE, expect)
  expect_true(r$isError)
  expect_identical(r$structuredContent$code, "STALE_SESSION")

  r <- e$.ts_tool_export(7L, expect)
  expect_true(r$isError)
  expect_identical(r$structuredContent$code, "STALE_SESSION")
})


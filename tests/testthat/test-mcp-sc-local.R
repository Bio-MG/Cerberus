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
  # HERMETICITY (audit 2026-10-03): the sandbox previously ran on the REAL
  # project tools/_drive — arming a session wrote a real ready.json there, and
  # the per-test `unlink(e$ts_drive_root(), recursive = TRUE)` DELETED the real
  # directory, README included. A leftover real ready.json also tripped
  # test-drive-watcher.R's "absent until written" on the next run (measured).
  # The sandbox now boots on its OWN temp root: same functions, same protocol,
  # zero contact with the operator's drive state. No pin changes — the tests'
  # per-test unlink then cleans the temp root, which is what it always meant
  # to do.
  root <- file.path(tempdir(), paste0("tsdrive-mcp-", as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(root, "tools", "_drive"), recursive = TRUE, showWarnings = FALSE)
  e$ts_drive_boot(root)
  e$ts_drive_clear_write_error()
  e
}

test_that("local MCP keeps TEN tools and resolves the three SC action buttons", {
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
  #
  # TEN since R1 (2026-10-04): `transcripto_drive_read` is the tool for the
  # protocol capability the invariant RULE ITSELF names — "a tool is added only
  # for a new protocol capability (import, READING), never for a module". The
  # read reads the CURRENT export verdict; it names no route, no handle, no
  # path, and adds no module surface.
  expect_length(tools, 10L)
  expect_setequal(vapply(tools, function(x) x$name, character(1)), c(
    "transcripto_drive_status", "transcripto_drive_read_result",
    "transcripto_drive_snapshot", "transcripto_drive_set_inputs",
    "transcripto_drive_run", "transcripto_drive_wait",
    "transcripto_drive_set_armed", "transcripto_drive_export",
    "transcripto_drive_import", "transcripto_drive_read"
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
test_that("the export tool exposes only `seq`, `module` and `expect`, at both levels", {
  e <- .mcp_sc_local_env()
  tools <- e$.ts_tools()
  ex <- Filter(function(x) identical(x$name, "transcripto_drive_export"), tools)
  expect_length(ex, 1L)
  if (!length(ex)) return(invisible(NULL))
  sch <- ex[[1]]$inputSchema
  expect_identical(sch$type, "object")
  expect_setequal(names(sch$properties), c("seq", "module", "expect"))
  expect_setequal(sch$required, c("seq", "module", "expect"))
  # Closed at the top level and closed inside `expect`: a field smuggled into
  # `expect` would otherwise reach the session assertion unchecked.
  expect_false(sch$additionalProperties)
  expect_false(sch$properties$expect$additionalProperties)
  expect_setequal(names(sch$properties$expect$properties),
                  c("pid", "started_at", "session_token", "session_id"))
  # `module` names WHICH declared route — and nothing else: its enum is the app's
  # own route table, read as data, so a route the app did not declare is not
  # reachable from here.
  expect_setequal(unlist(sch$properties$module$enum),
                  names(e$TS_DRIVE_EXPORT_ROUTES))
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
})

test_that("the export scenario payload is REBUILT, and carries no import block", {
  e <- .mcp_sc_local_env()
  p <- e$.ts_export_payload(7L, "sometoken", "spatial_qc")
  expect_identical(p$seq, 7L)
  expect_identical(p$action, "export_result")
  expect_identical(p$module, "spatial_qc")
  expect_identical(names(p), c("protocol", "seq", "session_token", "module", "action"))
  # The second route, by name: the payload is the app's own dialect for EITHER
  # declared route, and nothing else can appear in it.
  p2 <- e$.ts_export_payload(8L, "sometoken", "bulk_de")
  expect_identical(p2$module, "bulk_de")
  expect_identical(names(p2), c("protocol", "seq", "session_token", "module", "action"))
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

  r <- e$.ts_tool_export(7L, "spatial_qc", expect)
  expect_true(r$isError)
  expect_identical(r$structuredContent$code, "STALE_SESSION")
})

# =============================================================================
# S3 — the controlled import tool. The schema is the trust boundary (as with
# export): the tool validates KEY SET, required keys, enums, text bounds and the
# raw `..` rule, and deliberately does NOT judge roots, existence or loader kind
# — the app is a different process with different roots, so that verdict belongs
# to the app and arrives as an `invalid` result. The tests below pin BOTH the
# acceptances and the division of authority.
# =============================================================================

.mcp_sc_local_import_fixture <- function(e, armed = TRUE) {
  root <- tempfile("ts-mcp-imp-")
  dir.create(file.path(root, "tools", "_drive"), recursive = TRUE, showWarnings = FALSE)
  e$ts_drive_boot(root)
  e$ts_drive_clear_write_error()
  e$ts_drive_job_clear()
  token <- "imptok"
  started <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  e$ts_drive_write_ready(list(), token, armed = armed, last_seq = 0L,
                         hb_n = 2L, started_at = started)
  if (armed) {
    e$ts_drive_write_json(
      list(protocol = e$TS_DRIVE_PROTOCOL, token = token, armed = TRUE),
      e$ts_drive_path("arm.json")
    )
  }
  list(token = token, started = started,
       session_id = e$.ts_session_id(Sys.getpid(), started, token))
}

test_that("the import tool writes the app's exact import_file dialect", {
  e <- .mcp_sc_local_env()
  fx <- .mcp_sc_local_import_fixture(e)
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  expect <- list(session_id = fx$session_id, pid = Sys.getpid(),
                 started_at = fx$started)

  r <- e$.ts_tool_import(11L, "import_bulk",
                         list(counts_path = "data/GSE_counts.csv",
                              metadata_path = "data/meta.csv",
                              mode = "per_sample"),
                         expect)
  expect_false(r$isError)
  expect_true(r$structuredContent$accepted)
  expect_identical(r$structuredContent$action, "import_file")
  expect_false(r$structuredContent$applied)

  scenario <- jsonlite::fromJSON(e$ts_drive_path("scenario.json"), simplifyVector = FALSE)
  expect_identical(scenario$protocol, e$TS_DRIVE_PROTOCOL)
  expect_identical(as.integer(scenario$seq), 11L)
  expect_identical(scenario$module, "import_bulk")
  expect_identical(scenario$action, "import_file")
  expect_identical(scenario$session_token, fx$token)
  expect_setequal(names(scenario$import), c("counts_path", "metadata_path", "mode"))
  expect_identical(scenario$import$mode, "per_sample")

  # Spatial: optional fields default app-side, so the tool carries only what was sent.
  r <- e$.ts_tool_import(12L, "import_spatial", list(dir_path = "SPATIAL/visium_outs"), expect)
  expect_false(r$isError)
  scenario <- jsonlite::fromJSON(e$ts_drive_path("scenario.json"), simplifyVector = FALSE)
  expect_identical(scenario$module, "import_spatial")
  expect_setequal(names(scenario$import), "dir_path")

  # SC: both keys required, exactly as the app's poller demands.
  r <- e$.ts_tool_import(13L, "import_sc",
                         list(dir_path = "SC/sample-P1", sample_name = "P1"), expect)
  expect_false(r$isError)
  scenario <- jsonlite::fromJSON(e$ts_drive_path("scenario.json"), simplifyVector = FALSE)
  expect_setequal(names(scenario$import), c("dir_path", "sample_name"))
})

test_that("the import tool refuses the shapes the app should never see", {
  e <- .mcp_sc_local_env()
  fx <- .mcp_sc_local_import_fixture(e)
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  expect <- list(session_id = fx$session_id, pid = Sys.getpid(),
                 started_at = fx$started)

  # A non-importer module is MODULE_NOT_ALLOWED, not a written scenario.
  r <- e$.ts_tool_import(20L, "bulk_filter", list(min_count = 10), expect)
  expect_true(r$isError)
  expect_identical(r$structuredContent$code, "MODULE_NOT_ALLOWED")

  # A key belonging to ANOTHER importer is refused with the block untouched.
  r <- e$.ts_tool_import(21L, "import_bulk", list(dir_path = "x"), expect)
  expect_true(r$isError)
  expect_identical(r$structuredContent$code, "PAYLOAD_REFUSED")
  expect_true("dir_path" %in% names(r$structuredContent$detail$refused))

  # A missing required key is named.
  r <- e$.ts_tool_import(22L, "import_sc", list(dir_path = "SC/s1"), expect)
  expect_true(r$isError)
  expect_true("sample_name" %in% names(r$structuredContent$detail$refused))

  # `..` on the RAW string: the one path rule that is process-independent.
  r <- e$.ts_tool_import(23L, "import_bulk",
                         list(counts_path = "../roots-EVIL/secret.csv"), expect)
  expect_true(r$isError)
  expect_identical(r$structuredContent$code, "PAYLOAD_REFUSED")
  expect_false(file.exists(e$ts_drive_path("scenario.json")))

  # Enum and text bounds, on the M3b pattern.
  r <- e$.ts_tool_import(24L, "import_bulk",
                         list(counts_path = "c.csv", mode = "rows"), expect)
  expect_true(r$isError)
  r <- e$.ts_tool_import(25L, "import_spatial",
                         list(dir_path = "d", technology = "visium ",
                              sample_name = "a\nb"), expect)
  expect_true(r$isError)
  expect_false(file.exists(e$ts_drive_path("scenario.json")))

  # The tool-only bootstrap fails closed WITHOUT a session pin.
  r <- e$.ts_tool_import(26L, "import_bulk", list(counts_path = "c.csv"), NULL)
  expect_true(r$isError)
  expect_identical(r$structuredContent$code, "SESSION_ASSERTION_REQUIRED")

  # Stale seq, so a replayed import cannot overwrite a consumed one: the session
  # first PUBLISHES a consumed sequence (last_seq = 50), then seq 1 is behind it.
  hb <- jsonlite::fromJSON(e$ts_drive_path("ready.json"), simplifyVector = FALSE)
  hb$last_seq <- 50L
  e$ts_drive_write_json(hb, e$ts_drive_path("ready.json"))
  r <- e$.ts_tool_import(1L, "import_bulk", list(counts_path = "c.csv"), expect)
  expect_true(r$isError)
  expect_identical(r$structuredContent$code, "SEQ_STALE")
})

test_that("the import tool is refused on an UNARMED session and echoes no path", {
  e <- .mcp_sc_local_env()
  fx <- .mcp_sc_local_import_fixture(e, armed = FALSE)
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  expect <- list(session_id = fx$session_id)

  r <- e$.ts_tool_import(30L, "import_bulk", list(counts_path = "c.csv"), expect)
  expect_true(r$isError)
  expect_identical(r$structuredContent$code, "SESSION_NOT_ARMED")

  # On the armed path, the caller's path value must not appear anywhere in the
  # answer: KEYS travel, values never do (the import-v1 redaction policy).
  fx2 <- .mcp_sc_local_import_fixture(e, armed = TRUE)
  secret <- "data/very-secret-counts-matrix.csv"
  r <- e$.ts_tool_import(31L, "import_bulk", list(counts_path = secret),
                         list(session_id = fx2$session_id))
  expect_false(r$isError)
  wire <- as.character(jsonlite::toJSON(r$structuredContent, auto_unbox = TRUE))
  expect_false(grepl(secret, wire, fixed = TRUE))
  expect_identical(r$structuredContent$import_keys, "counts_path")
})

test_that("the import schema cross-checks clean against the app's importer table", {
  e <- .mcp_sc_local_env()
  expect_length(e$.ts_mcp_import_problems(), 0L)
})

test_that("the export column contracts cross-check clean against the app's table", {
  e <- .mcp_sc_local_env()
  expect_length(e$.ts_mcp_export_problems(), 0L)
})

test_that("the export tool description carries the per-route column contracts", {
  e <- .mcp_sc_local_env()
  tools <- e$.ts_tools()
  ex <- Filter(function(x) identical(x$name, "transcripto_drive_export"), tools)
  expect_length(ex, 1L)
  if (!length(ex)) return(invisible(NULL))
  desc <- paste(ex[[1]]$description, collapse = " ")
  # One engine-constant name per contract shape: the fixed spatial list, the
  # bulk_de guaranteed set, both pathway modes, the marker normaliser's set.
  expect_match(desc, "gi_star", fixed = TRUE)
  expect_match(desc, "baseMean", fixed = TRUE)
  expect_match(desc, "geneID", fixed = TRUE)
  expect_match(desc, "core_enrichment", fixed = TRUE)
  expect_match(desc, "avg_log2FC", fixed = TRUE)
  # And the description must say the redaction is STILL the wire's rule —
  # declaring the schema must not imply the caller receives these verbatim.
  expect_match(desc, "<redacted>", fixed = TRUE)
})

test_that("the export tool refuses a module outside the app's route table", {
  e <- .mcp_sc_local_env()
  fx <- .mcp_sc_local_import_fixture(e)
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  expect <- list(session_id = fx$session_id, pid = Sys.getpid(),
                 started_at = fx$started)
  # `sc_annotation` has no route (and gets none without its own product
  # call — Slice 4 decides, never a side effect). Modules kept LEAVING the
  # refusal role as routes arrived: bulk_pathways (S2c),
  # bulk_filter/bulk_signatures (Slice 2.3), bulk_wgcna (Slice 4, whose
  # product call GRANTED the gene->module route — the leaver loop below
  # proves it end-to-end).
  r <- e$.ts_tool_export(9L, "sc_annotation", expect)
  expect_true(r$isError)
  expect_identical(r$structuredContent$code, "MODULE_NOT_ALLOWED")
  expect_false(file.exists(e$ts_drive_path("scenario.json")))

  # EVERY declared route is reachable end-to-end through the same tool: the
  # payload carries the caller's module verbatim, and only that field moves.
  # Driven by the APP's own table, so a route added later joins this loop or
  # the test goes red — the route table cannot grow silently.
  for (i in seq_along(names(TS_DRIVE_EXPORT_ROUTES))) {
    m <- names(TS_DRIVE_EXPORT_ROUTES)[i]
    ri <- e$.ts_tool_export(9L + i, m, expect)
    expect_false(ri$isError, info = sprintf("route '%s' must pass the gate", m))
    scenario <- jsonlite::fromJSON(e$ts_drive_path("scenario.json"), simplifyVector = FALSE)
    expect_identical(scenario$module, m,
                     info = sprintf("route '%s': payload module verbatim", m))
    expect_identical(scenario$action, "export_result",
                     info = sprintf("route '%s': payload action", m))
  }
})


# =============================================================================
# R1 — the ONE bounded read tool (option i, D1 approuvé 2026-10-04, revue
# pré-code amendée : PAS de route, PAS de handle — les stems portent des runs
# alphanumériques de 8+ que le sanitiseur redacte, donc l'agent ne peut pas
# renvoyer un handle valide ; l'outil lit LE verdict d'export courant, et
# NO_EXPORT_TARGET est dérivé SANS ÉTAT depuis result.json).
# =============================================================================

.mcp_sc_local_export_fixture <- function(e) {
  # Un verdict d'export `done` crédible dans le root hermétique du sandbox :
  # exactement ce que ts_drive_write_result écrit pour un export réel, avec le
  # descripteur projeté (keep-set export). L'applied_at est MAINTENANT, donc
  # postérieur au started_at du handshake — l'attribution de session passe.
  write_result <- get("ts_drive_write_result", envir = e)
  started <- format(Sys.time() - 5, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  list(
    started = started,
    write_export = function(seq) {
      write_result(seq, "done", "bulk_de", TRUE, descriptor = list(
        format = "csv", file = "bulk_de_results_1.csv", bytes = 120L,
        n_rows = 5L, n_cols = 2L, columns = c("gene", "x")))
    },
    write_read = function(seq) {
      # Un verdict de LECTURE : le descripteur porte `preview` — LE marqueur
      # sans état qui dit « consommé ». Écrit DIRECTEMENT (pas via
      # ts_drive_write_result, dont le projeteur EXPORT retirerait preview) :
      # R2 écrira ce bloc par le projeteur read, qui n'existe pas encore.
      payload <- list(
        protocol      = e$TS_DRIVE_PROTOCOL,
        ack_seq       = as.integer(seq),
        status        = "done",
        applied_at    = e$ts_drive_now_iso(),
        active_module = NULL,
        armed         = TRUE,
        preserve_data = TRUE,
        errors        = list(),
        warnings      = list(),
        snapshot      = NULL,
        job           = NULL,
        descriptor    = list(
          route = "bulk_de", handle = "bulk_de_results_1.csv", seq = as.integer(seq),
          descriptor = list(format = "csv", file = "bulk_de_results_1.csv",
                            bytes = 120L, n_rows = 5L, n_cols = 2L),
          preview = list(rows_returned = 1L, truncated_rows = FALSE,
                         columns = c("gene", "x"), rows = list(list("G1", 1)),
                         cells_truncated = 0L, cells_guarded = 0L),
          col_summary = list(list(name = "gene", class = "character",
                                  n_missing_in_preview = 0L))))
      e$ts_drive_write_json(payload, e$ts_drive_path("result.json"))
    }
  )
}

test_that("the read tool exposes only seq, max_rows and expect, at both levels", {
  e <- .mcp_sc_local_env()
  tools <- e$.ts_tools()
  ex <- Filter(function(x) identical(x$name, "transcripto_drive_read"), tools)
  expect_length(ex, 1L)
  if (!length(ex)) return(invisible(NULL))
  sch <- ex[[1]]$inputSchema
  expect_identical(sch$type, "object")
  expect_setequal(names(sch$properties), c("seq", "max_rows", "expect"))
  expect_false(sch$additionalProperties)
  expect_false(sch$properties$expect$additionalProperties)
  expect_setequal(names(sch$properties$expect$properties),
                  c("pid", "started_at", "session_id"))
  # PAS de session_token : le token brut n'est jamais une valeur fournie par
  # l'appelant ; l'assertion le dérive du heartbeat vivant.
  expect_false("session_token" %in% names(sch$properties$expect$properties))
  expect_setequal(sch$required, c("seq", "expect"))
  expect_identical(sch$properties$max_rows$maximum, 200L)
  expect_identical(sch$properties$max_rows$minimum, 1L)
})

test_that("the read tool refuses out-of-domain values as tool results, and malformed shapes as -32602", {
  e <- .mcp_sc_local_env()
  fx <- .mcp_sc_local_import_fixture(e)
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  expect <- list(session_id = fx$session_id)

  # Domaine : des valeurs BIEN typées hors bornes => tool result à code.
  r0 <- e$.ts_tool_read(0L, NULL, expect)
  expect_true(r0$isError)
  expect_identical(r0$structuredContent$code, "PAYLOAD_REFUSED")
  rlo <- e$.ts_tool_read(11L, 0L, expect)
  expect_true(rlo$isError)
  expect_identical(rlo$structuredContent$code, "PAYLOAD_REFUSED")
  rhi <- e$.ts_tool_read(11L, 201L, expect)
  expect_true(rhi$isError)
  expect_identical(rhi$structuredContent$code, "PAYLOAD_REFUSED")
  # Aucune écriture de scenario sur un refus pré-écriture.
  expect_false(file.exists(e$ts_drive_path("scenario.json")))

  # Protocole (-32602) : les erreurs de FORME, côté dispatch.
  dmiss <- e$.ts_dispatch(list(jsonrpc = "2.0", id = 1L, method = "tools/call",
    params = list(name = "transcripto_drive_read",
                  arguments = list(seq = 11L))))
  expect_identical(dmiss$error$code, -32602,
                   info = "expect absent: -32602, forme du protocole")
  dextra <- e$.ts_dispatch(list(jsonrpc = "2.0", id = 1L, method = "tools/call",
    params = list(name = "transcripto_drive_read",
                  arguments = list(seq = 11L, expect = list(session_id = "s"),
                                   route = "bulk_de"))))
  expect_identical(dextra$error$code, -32602,
                   info = "route est SUPPRIME de l'outil : champ inconnu => -32602")
  dtok <- e$.ts_dispatch(list(jsonrpc = "2.0", id = 1L, method = "tools/call",
    params = list(name = "transcripto_drive_read",
                  arguments = list(seq = 11L,
                                   expect = list(session_id = "s", session_token = "t")))))
  expect_identical(dtok$error$code, -32602,
                   info = "session_token n'est jamais une valeur fournie par l'appelant")
  dseq <- e$.ts_dispatch(list(jsonrpc = "2.0", id = 1L, method = "tools/call",
    params = list(name = "transcripto_drive_read",
                  arguments = list(seq = "12", expect = list(session_id = "s")))))
  expect_identical(dseq$error$code, -32602)
  dmr <- e$.ts_dispatch(list(jsonrpc = "2.0", id = 1L, method = "tools/call",
    params = list(name = "transcripto_drive_read",
                  arguments = list(seq = 11L, max_rows = "20",
                                   expect = list(session_id = "s")))))
  expect_identical(dmr$error$code, -32602)
})

test_that("NO_EXPORT_TARGET is derived statelessly from result.json", {
  e <- .mcp_sc_local_env()
  fx <- .mcp_sc_local_import_fixture(e)
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  ef <- .mcp_sc_local_export_fixture(e)
  expect <- list(session_id = fx$session_id)

  # 1. result.json ABSENT : rien n'a été conduit, donc rien n'a été exporté.
  r0 <- e$.ts_tool_read(11L, NULL, expect)
  expect_true(r0$isError)
  expect_identical(r0$structuredContent$code, "NO_EXPORT_TARGET")

  # 2. verdict d'export `done` : la cible EXISTE (descripteur, pas de preview).
  ef$write_export(10L)
  ok <- e$.ts_tool_read(11L, 5L, expect)
  expect_false(ok$isError)
  expect_identical(ok$structuredContent$dispatched, "read_export")
  expect_identical(ok$structuredContent$max_rows, 5L)
  scn <- jsonlite::fromJSON(e$ts_drive_path("scenario.json"), simplifyVector = FALSE)
  expect_identical(scn$action, "read_export")
  expect_identical(scn$max_rows, 5L)
  expect_identical(scn$session_token, fx$token)
  # Aucun champ de fichier du tout (ni route, ni handle, ni chemin).
  expect_false(any(c("route", "handle", "file", "dir", "path") %in% names(scn)))

  # 3. le verdict est déjà un verdict de LECTURE : consommé => NO_EXPORT_TARGET.
  ef$write_read(11L)
  r2 <- e$.ts_tool_read(12L, NULL, expect)
  expect_true(r2$isError)
  expect_identical(r2$structuredContent$code, "NO_EXPORT_TARGET")
  expect_match(r2$structuredContent$message, "already a read verdict", fixed = TRUE)

  # 4. verdict non terminal (running) : pas d'artefact => NO_EXPORT_TARGET.
  write_result <- get("ts_drive_write_result", envir = e)
  write_result(12L, "running", "bulk_de", TRUE, descriptor = NULL)
  r3 <- e$.ts_tool_read(13L, NULL, expect)
  expect_true(r3$isError)
  expect_identical(r3$structuredContent$code, "NO_EXPORT_TARGET")

  # 5. verdict d'une session ANTIÉRIEURE : attribution réutilisée
  #    (RESULT_SESSION_MISMATCH), pas NO_EXPORT_TARGET.
  stale <- jsonlite::fromJSON(e$ts_drive_path("result.json"), simplifyVector = FALSE)
  stale$applied_at <- format(as.POSIXct("2000-01-01", tz = "UTC"),
                             "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  e$ts_drive_write_json(stale, e$ts_drive_path("result.json"))
  r4 <- e$.ts_tool_read(14L, NULL, expect)
  expect_true(r4$isError)
  expect_identical(r4$structuredContent$code, "RESULT_SESSION_MISMATCH")
})

test_that("the read tool mirrors the export session assertion exactly", {
  e <- .mcp_sc_local_env()
  fx <- .mcp_sc_local_import_fixture(e)
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  ef <- .mcp_sc_local_export_fixture(e)
  ef$write_export(10L)

  # session_id ABSENT : SESSION_ASSERTION_REQUIRED (comme l'export).
  r1 <- e$.ts_tool_read(11L, NULL, list())
  expect_true(r1$isError)
  expect_identical(r1$structuredContent$code, "SESSION_ASSERTION_REQUIRED")
  # Wildcard : AMBIGUOUS_SESSION (comme l'export).
  r2 <- e$.ts_tool_read(11L, NULL, list(session_id = "*"))
  expect_true(r2$isError)
  expect_identical(r2$structuredContent$code, "AMBIGUOUS_SESSION")
  # Mauvais pin : SESSION_MISMATCH (comme l'export).
  r3 <- e$.ts_tool_read(11L, NULL, list(session_id = "s-not-the-session"))
  expect_true(r3$isError)
  expect_identical(r3$structuredContent$code, "SESSION_MISMATCH")
  # SEQ_STALE : rejouer un seq déjà consommé. Le fixture écrit le verdict
  # DIRECTEMENT (le poller réel est ce qui incrémente last_seq), donc le test
  # fait le bump explicitement — même sémantique, sans poller.
  hb <- jsonlite::fromJSON(e$ts_drive_path("ready.json"), simplifyVector = FALSE)
  hb$last_seq <- 10L
  e$ts_drive_write_json(hb, e$ts_drive_path("ready.json"))
  r4 <- e$.ts_tool_read(10L, NULL, list(session_id = fx$session_id))
  expect_true(r4$isError)
  expect_identical(r4$structuredContent$code, "SEQ_STALE")
  expect_false(file.exists(e$ts_drive_path("scenario.json")))
})

test_that("the read keep-set cross-checks clean against the app's table", {
  e <- .mcp_sc_local_env()
  expect_length(e$.ts_mcp_read_problems(), 0L)
  # Les marqueurs déclarés passent le sanitiseur verbatim (sinon les compteurs
  # seraient illisibles sur le wire).
  for (mk in e$TS_DRIVE_READ_CELL_MARKERS) {
    expect_identical(e$ts_drive_badge_sanitize(mk, 200L), mk)
  }
})

test_that("read_result surfaces the verdict descriptor (additive)", {
  e <- .mcp_sc_local_env()
  fx <- .mcp_sc_local_import_fixture(e)
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  ef <- .mcp_sc_local_export_fixture(e)
  ef$write_export(10L)
  rr <- e$.ts_tool_read_result()
  expect_false(rr$isError)
  d <- rr$structuredContent$descriptor
  expect_false(is.null(d))
  expect_identical(d$file, "bulk_de_results_1.csv")
  expect_identical(d$n_rows, 5L)
})
# =============================================================================
# R3 (2026-10-04) — le FLUX CROISÉ : export -> dispatch read -> application ->
# read_result, puis le refus du second read. Le contenu borné doit arriver
# JUSQU'À l'agent, et exactement UNE lecture par export (dérivation sans état).
# =============================================================================

.mcp_sc_local_read_target <- function(e, file_base = "bulk_de_results_1.csv") {
  # Le nom DOIT être celui du verdict d'export écrit par le fixture (le
  # répondant dérive sa cible de result.json, pas d'un argument) : c'est
  # bulk_de_results_1.csv. Les tests R1 n'écrivent jamais le fichier, donc
  # aucun conflit dans le répertoire d'export temporaire du processus.
  d <- file.path(tempdir(), "ts_drive_exports")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(d, file_base)
  utils::write.csv(data.frame(gene = c("TP53", "BRCA1", "MYC"),
                              x = c("1.5", "2.25", "0.1"),
                              stringsAsFactors = FALSE),
                   path, row.names = FALSE, na = "")
  path
}

.mcp_sc_local_apply_read <- function(e) {
  # EXACTEMENT les deux appels du poller : lire le scenario écrit par le
  # dispatch, répondre, écrire le verdict par le projeteur READ.
  scn <- jsonlite::fromJSON(e$ts_drive_path("scenario.json"),
                            simplifyVector = FALSE)
  res <- e$ts_drive_read_export_respond(as.integer(scn$seq), scn$max_rows)
  e$ts_drive_write_result(as.integer(scn$seq), res$status,
                          res$active_module %||% scn$module, TRUE,
                          errors = res$errors, warnings = res$warnings,
                          descriptor = res$descriptor,
                          descriptor_projector = e$ts_drive_read_descriptor)
  res
}

test_that("export then read dispatch then read_result delivers the bounded content", {
  e <- .mcp_sc_local_env()
  fx <- .mcp_sc_local_import_fixture(e)
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  csv <- .mcp_sc_local_read_target(e)
  on.exit(unlink(csv), add = TRUE)
  ef <- .mcp_sc_local_export_fixture(e)
  ef$write_export(10L)

  # 1. Dispatch : le scenario read_export part avec l'assertion de session.
  r <- e$.ts_tool_read(11L, 3L, list(session_id = fx$session_id))
  expect_false(r$isError)
  expect_identical(r$structuredContent$dispatched, "read_export")

  # 2. Application (les deux appels du poller).
  res <- .mcp_sc_local_apply_read(e)
  expect_identical(res$status, "done")

  # 3. read_result : l'agent reçoit la preview bornée — le contenu du FICHIER,
  #    cellule par cellule, et le keep-set du bloc de lecture.
  rr <- e$.ts_tool_read_result()
  expect_false(rr$isError)
  d <- rr$structuredContent$descriptor
  expect_false(is.null(d))
  expect_true(all(names(d) %in% e$TS_DRIVE_READ_KEYS))
  expect_identical(as.character(d$handle), "bulk_de_results_1.csv")
  expect_identical(as.character(d$preview$columns), c("gene", "x"))
  expect_identical(as.integer(d$preview$rows_returned), 3L)
  expect_false(isTRUE(d$preview$truncated_rows))
  expect_identical(as.character(unlist(d$preview$rows[[1]])), c("TP53", "1.5"))
  expect_identical(as.character(unlist(d$preview$rows[[3]])), c("MYC", "0.1"))
})

test_that("exactly one read per export: the second read is refused and writes nothing", {
  e <- .mcp_sc_local_env()
  fx <- .mcp_sc_local_import_fixture(e)
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  csv <- .mcp_sc_local_read_target(e)
  on.exit(unlink(csv), add = TRUE)
  ef <- .mcp_sc_local_export_fixture(e)
  ef$write_export(10L)
  r <- e$.ts_tool_read(11L, 2L, list(session_id = fx$session_id))
  expect_false(r$isError)
  res <- .mcp_sc_local_apply_read(e)
  expect_identical(res$status, "done")

  # Le SECOND read : l'export a été consommé par la première lecture (le
  # verdict de lecture A REMPLACÉ le verdict d'export dans result.json) — la
  # dérivation sans état refuse, sans écrire RIEN nulle part.
  before <- paste(readBin(e$ts_drive_path("result.json"), "raw",
                          file.size(e$ts_drive_path("result.json"))),
                  collapse = " ")
  # Aucun scenario REÉCRIT pour le refus (celui du premier dispatch reste,
  # inchangé), verdict de lecture intact.
  scn_before <- paste(readBin(e$ts_drive_path("scenario.json"), "raw",
                              file.size(e$ts_drive_path("scenario.json"))),
                      collapse = " ")
  r2 <- e$.ts_tool_read(12L, NULL, list(session_id = fx$session_id))
  expect_true(r2$isError)
  expect_identical(r2$structuredContent$code, "NO_EXPORT_TARGET")
  expect_match(r2$structuredContent$message, "already a read verdict",
               fixed = TRUE)
  scn_after <- paste(readBin(e$ts_drive_path("scenario.json"), "raw",
                             file.size(e$ts_drive_path("scenario.json"))),
                     collapse = " ")
  expect_identical(scn_after, scn_before)
  after <- paste(readBin(e$ts_drive_path("result.json"), "raw",
                         file.size(e$ts_drive_path("result.json"))),
                 collapse = " ")
  expect_identical(after, before)
})
# =============================================================================
# Slice 3 (2026-10-05) — le serveur face au vocabulaire session-dérivé : les
# cinq entrées indexées, la matrice INPUT_NOT_READY / VOCAB_STALE /
# INDEX_OUT_OF_RANGE, et la note redaction qui passe de not_exposed à indexed
# (29 -> 34 exposés).
# =============================================================================

.mcp_sc_local_vocab_snapshot <- function(e, with_vocab = TRUE) {
  # Un verdict d'export `done` crédible dont le snapshot porte le bloc
  # `vocabulary` publié par la sonde du module (projété : ce qui Voyage est
  # exactement ce bloc).
  voc <- if (with_vocab) list(
    bulk_de = list(vocabulary = list(
      condition_col = c("condition", "tissue"),
      covariates = c("condition", "tissue"),
      group_levels = c("mock", "CoV2"),
      vocab_rev = 3L)),
    bulk_pathways = list(vocabulary = list(
      scores_source = c("msigdb_hallmark", "progeny", "dorothea", "file"),
      vocab_rev = 7L))) else NULL
  write_result <- get("ts_drive_write_result", envir = e)
  write_result(10L, "done", "bulk_de", TRUE,
               snapshot = if (is.null(voc)) NULL else list(modules = voc))
}

test_that("the five session-derived inputs are exposed by index, and the vocabulary tables agree", {
  e <- .mcp_sc_local_env()
  # La note 29 -> 34 est un pin, pas un décompte décoratif : cinq entrées sont
  # passées de « délibérément non exposées » à « exposées par index ». Slice 4
  # (2026-10-05) : 34 -> 37 — les trois widgets WGCNA (n_genes, power_override,
  # traits) ; 5 -> 6 entrées session-dérivées (traits, par INDEX).
  expect_length(e$TS_MCP_INPUT_SCHEMA, 37L)
  expect_length(e$TS_DRIVE_SESSION_INPUTS, 6L)
  # Le miroir sans drift : les cinq ids sont EXACTEMENT ceux du schéma dont le
  # type est index/index_list, tous selects côté app, clés déclarées.
  expect_length(e$.ts_mcp_vocab_problems(), 0L)
  expect_length(e$.ts_mcp_not_exposed(), 0L)
  types <- vapply(e$TS_DRIVE_SESSION_INPUTS, function(x) x$type, character(1))
  expect_identical(unname(types), c("index", "index_list", "index", "index", "index",
                                    "index_list"))
  # Les nouveaux codes sont bien du domaine (tool result), pas du protocole.
  expect_true(all(c("INPUT_NOT_READY", "VOCAB_STALE", "INDEX_OUT_OF_RANGE")
                  %in% e$.ts_domain_codes))
})

test_that("indexed inputs are validated against the published vocabulary, and the scenario carries the index", {
  e <- .mcp_sc_local_env()
  fx <- .mcp_sc_local_import_fixture(e)
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  .mcp_sc_local_vocab_snapshot(e)
  expect <- list(session_id = fx$session_id)

  # 1. Forme refusée : un nom brut n'est PLUS une valeur acceptable — l'entrée
  #    est adressée par index depuis la Slice 3.
  r0 <- e$.ts_tool_set_inputs(11L, "bulk_de",
                              list(`bulk-de-condition_col` = "condition"), TRUE, expect)
  expect_true(r0$isError)
  expect_identical(r0$structuredContent$code, "VALUE_REFUSED")
  expect_false(file.exists(e$ts_drive_path("scenario.json")))

  # 2. La forme est bonne : le scénario part, et il porte {index, vocab_rev}
  #    NORMALISÉS en entiers — jamais le nom.
  r1 <- e$.ts_tool_set_inputs(11L, "bulk_de", list(
    `bulk-de-condition_col` = list(index = 2L, vocab_rev = 3L),
    `bulk-de-covariates`    = list(index = list(2L, 1L), vocab_rev = 3L),
    `bulk-de-de_engine`     = "deseq2"), TRUE, expect)
  expect_false(r1$isError)
  scn <- jsonlite::fromJSON(e$ts_drive_path("scenario.json"), simplifyVector = FALSE)
  expect_identical(as.integer(scn$inputs$`bulk-de-condition_col`$index), 2L)
  expect_identical(as.integer(scn$inputs$`bulk-de-condition_col`$vocab_rev), 3L)
  expect_identical(as.integer(unlist(scn$inputs$`bulk-de-covariates`$index)), c(2L, 1L))
  expect_identical(as.character(scn$inputs$`bulk-de-de_engine`), "deseq2")
  # La note de redaction dit `indexed`, plus jamais `not_exposed`.
  # Slice 4 : la sixième entrée indexée (les traits WGCNA) rejoint la note.
  expect_setequal(as.character(unlist(r1$structuredContent$redaction$indexed$ids)),
                  c("bulk-de-condition_col", "bulk-de-covariates", "bulk-de-group_ref",
                    "bulk-de-group_target", "bulk-pathways-scores_source",
                    "bulk-wgcna-wgcna_traits"))
  expect_false("not_exposed" %in% names(r1$structuredContent$redaction))

  # 3. rev dépassée : VOCAB_STALE — rien n'est écrit.
  r2 <- e$.ts_tool_set_inputs(12L, "bulk_de", list(
    `bulk-de-condition_col` = list(index = 1L, vocab_rev = 2L)), TRUE, expect)
  expect_true(r2$isError)
  expect_identical(r2$structuredContent$code, "VOCAB_STALE")
  expect_identical(as.integer(jsonlite::fromJSON(
    e$ts_drive_path("scenario.json"), simplifyVector = FALSE)$seq), 11L)

  # 4. Index au-delà du domaine publié : INDEX_OUT_OF_RANGE.
  r3 <- e$.ts_tool_set_inputs(12L, "bulk_de", list(
    `bulk-de-condition_col` = list(index = 9L, vocab_rev = 3L)), TRUE, expect)
  expect_true(r3$isError)
  expect_identical(r3$structuredContent$code, "INDEX_OUT_OF_RANGE")

  # 5. Le pair ref == target : PAYLOAD_REFUSED, avant toute écriture.
  r4 <- e$.ts_tool_set_inputs(12L, "bulk_de", list(
    `bulk-de-group_ref`    = list(index = 1L, vocab_rev = 3L),
    `bulk-de-group_target` = list(index = 1L, vocab_rev = 3L)), TRUE, expect)
  expect_true(r4$isError)
  expect_identical(r4$structuredContent$code, "PAYLOAD_REFUSED")
  expect_match(r4$structuredContent$message, "same index", fixed = TRUE)

  # 6. Aucun vocabulaire publié (données non chargées) : INPUT_NOT_READY —
  #    fail closed, avec le POURQUOI. Le scénario du chemin heureux (case 2)
  #    reste sur le disque : le refus n'écrit RIEN, il est inchangé.
  scn_bytes <- paste(readBin(e$ts_drive_path("scenario.json"), "raw",
                             file.size(e$ts_drive_path("scenario.json"))),
                     collapse = " ")
  .mcp_sc_local_vocab_snapshot(e, with_vocab = FALSE)
  r5 <- e$.ts_tool_set_inputs(12L, "bulk_de", list(
    `bulk-de-condition_col` = list(index = 1L, vocab_rev = 3L)), TRUE, expect)
  expect_true(r5$isError)
  expect_identical(r5$structuredContent$code, "INPUT_NOT_READY")
  expect_identical(paste(readBin(e$ts_drive_path("scenario.json"), "raw",
                                 file.size(e$ts_drive_path("scenario.json"))),
                         collapse = " "), scn_bytes)

  # 7. Les cinq ids restent des ENTRÉES : le compte d'outils ne bouge pas.
  tools <- e$.ts_tools()
  expect_length(tools, 10L)
})

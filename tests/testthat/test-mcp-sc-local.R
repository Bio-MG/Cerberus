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

test_that("local MCP keeps seven tools and resolves the three SC action buttons", {
  e <- .mcp_sc_local_env()
  tools <- e$.ts_tools()
  # The inventory is FROZEN: adding a module must never add a tool.
  expect_length(tools, 7L)
  expect_setequal(vapply(tools, function(x) x$name, character(1)), c(
    "transcripto_drive_status", "transcripto_drive_read_result",
    "transcripto_drive_snapshot", "transcripto_drive_set_inputs",
    "transcripto_drive_run", "transcripto_drive_wait",
    "transcripto_drive_set_armed"
  ))
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
  expect_true("sc_pipeline" %in% wait_modules)
  expect_true("sc_annotation" %in% wait_modules)
  expect_true("sc_markers" %in% wait_modules)
  expect_true("sc-pipeline-run_auto_pipeline" %in% run_buttons)
  expect_true("sc-annotation-run_annot" %in% run_buttons)
  expect_true("sc-markers-run_markers" %in% run_buttons)
  sc_buttons <- run_buttons[startsWith(run_buttons, "sc-")]
  expect_setequal(sc_buttons, c("sc-pipeline-run_auto_pipeline",
                                "sc-annotation-run_annot",
                                "sc-markers-run_markers"))
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

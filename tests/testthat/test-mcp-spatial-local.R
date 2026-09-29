.mcp_local_script <- function() {
  path <- file.path(ts_project_root(), "scripts", "mcp_server.R")
  if (!file.exists(path)) {
    skip("the MCP server is a local, gitignored capability and is absent from this clone")
  }
  path
}

.mcp_local_env <- function() {
  path <- .mcp_local_script()
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

test_that("local MCP keeps EIGHT tools and resolves only the Spatial pipeline button", {
  e <- .mcp_local_env()
  tools <- e$.ts_tools()
  # EIGHT since S2, which added a tool for a new ACTION (`export_result`) and not
  # for a module. The rule this pin used to stand for — a module never adds a tool —
  # is now asserted directly in test-mcp-sc-local.R, where it can be stated over
  # the module inventory instead of over a count. See the note there.
  expect_length(tools, 8L)
  expect_setequal(vapply(tools, function(x) x$name, character(1)), c(
    "transcripto_drive_status", "transcripto_drive_read_result",
    "transcripto_drive_snapshot", "transcripto_drive_set_inputs",
    "transcripto_drive_run", "transcripto_drive_wait",
    "transcripto_drive_set_armed", "transcripto_drive_export"
  ))
  expect_length(e$.ts_mcp_run_problems(), 0L)
  expect_identical(e$TS_MCP_RUN_ACTIONS, "run_pipeline")

  resolved <- e$.ts_mcp_resolve_button("spatial_pipeline", NULL)
  expect_true(resolved$ok)
  expect_identical(resolved$button, "spatial-pipeline-btn_run_all")
  expect_true(e$.ts_mcp_resolve_button("spatial_pipeline", "spatial-pipeline-btn_run_all")$ok)
  expect_false(e$.ts_mcp_resolve_button("spatial_pipeline", "bulk-de-run_de")$ok)
  expect_false(e$.ts_mcp_resolve_button("spatial_pipeline", "spatial-qc-btn_apply_qc")$ok)
})

test_that("local MCP exposes Spatial for run and wait but not set_inputs", {
  e <- .mcp_local_env()
  tools <- stats::setNames(e$.ts_tools(), vapply(e$.ts_tools(), function(x) x$name, character(1)))
  set_modules <- unlist(tools$transcripto_drive_set_inputs$inputSchema$properties$module$enum)
  run_modules <- unlist(tools$transcripto_drive_run$inputSchema$properties$module$enum)
  wait_modules <- unlist(tools$transcripto_drive_wait$inputSchema$properties$module$enum)
  run_buttons <- unlist(tools$transcripto_drive_run$inputSchema$properties$button$enum)

  expect_false("spatial_pipeline" %in% set_modules)
  expect_false("spatial_qc" %in% set_modules)
  expect_true("spatial_pipeline" %in% run_modules)
  expect_true("spatial_qc" %in% run_modules)
  expect_true("import_spatial" %in% run_modules)
  expect_true("spatial_pipeline" %in% wait_modules)
  expect_true("spatial_qc" %in% wait_modules)
  expect_true("spatial-pipeline-btn_run_all" %in% run_buttons)
  expect_true("spatial-qc-btn_hotspots" %in% run_buttons)
  expect_true("import_spatial-btn_import" %in% run_buttons)
  # THREE Spatial buttons since Phase F, and still a CLOSED set: every other
  # child module stays out, and the `import_spatial-` namespace contributes
  # EXACTLY ONE id. Asserted as a set rather than as "no import_spatial- key",
  # because that older form would now be contradicted by the one button that is
  # deliberately exposed — and a test that has to be deleted rather than edited
  # is a test that stops being read.
  spatial_buttons <- run_buttons[startsWith(run_buttons, "spatial-")]
  expect_setequal(spatial_buttons, c("spatial-pipeline-btn_run_all",
                                     "spatial-qc-btn_hotspots"))
  expect_setequal(run_buttons[startsWith(run_buttons, "import_spatial-")],
                  "import_spatial-btn_import")
  expect_false(any(grepl("^spatial-(cluster|deconv|viz|multi|niche|export|report)-",
                         run_buttons)))
  # `import_spatial` is settable-by-default like `import_bulk` (it owns no
  # settable allowlisted input, so every id is refused with a named reason), and
  # it must resolve to exactly its own button.
  expect_true("import_spatial" %in% set_modules)
  expect_identical(e$.ts_mcp_resolve_button("import_spatial", NULL)$button,
                   "import_spatial-btn_import")
  expect_false(e$.ts_mcp_resolve_button("import_spatial", "import_bulk-btn_load")$ok)
  # The near-miss inside the one module that IS exposed must stay absent, so a
  # later reader cannot "correct" the id back into a silent no-op.
  expect_false("spatial-qc-btn_apply_qc" %in% run_buttons)
  expect_false("import_spatial-dir_select" %in% run_buttons)
  expect_false("import_spatial-shared_ref_file" %in% run_buttons)
})

test_that("local MCP snapshot projection retains a fifth Spatial module without leaking", {
  e <- .mcp_local_env()
  modules <- list(
    import_bulk = list(n_results = 1L),
    bulk_filter = list(n_results = 2L),
    bulk_de = list(n_results = 3L),
    bulk_pathways = list(n_results = 4L),
    spatial_pipeline = list(
      module = "spatial_pipeline", action = "run_pipeline", status = "running",
      n_results = 5L, has_data = TRUE,
      project = "SENTINEL_PROJECT", bpcells_dir = "C:/SENTINEL_PATH",
      ids = c("SENTINEL_CELL_A", "SENTINEL_CELL_B")
    )
  )
  projected <- e$.ts_project_snapshot(list(modules = modules))
  expect_true("spatial_pipeline" %in% names(projected$modules))
  expect_identical(projected$modules$spatial_pipeline$status, "running")
  expect_identical(projected$modules$spatial_pipeline$n_results, 5L)
  expect_false("project" %in% names(projected$modules$spatial_pipeline))
  expect_false("bpcells_dir" %in% names(projected$modules$spatial_pipeline))
  expect_false("ids" %in% names(projected$modules$spatial_pipeline))
  expect_false(grepl("SENTINEL", e$.ts_json(projected), fixed = TRUE))
})

test_that("local MCP run writes the sole Spatial scenario into an external fixture root", {
  e <- .mcp_local_env()
  real_drive <- list.files(file.path(ts_project_root(), "tools", "_drive"), recursive = TRUE)
  root <- tempfile("ts-mcp-spatial-")
  dir.create(file.path(root, "tools", "_drive"), recursive = TRUE, showWarnings = FALSE)
  on.exit({
    unlink(root, recursive = TRUE, force = TRUE)
    expect_setequal(list.files(file.path(ts_project_root(), "tools", "_drive"),
                               recursive = TRUE), real_drive)
  }, add = TRUE)

  e$ts_drive_boot(root)
  e$ts_drive_clear_write_error()
  e$ts_drive_job_clear()
  token <- "spatialtok"
  started <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  e$ts_drive_write_ready(list(), token, armed = TRUE, last_seq = 0L,
                         hb_n = 0L, started_at = started)
  e$ts_drive_write_json(
    list(protocol = e$TS_DRIVE_PROTOCOL, token = token, armed = TRUE),
    e$ts_drive_path("arm.json")
  )
  session_id <- e$.ts_session_id(Sys.getpid(), started, token)
  expectation <- list(session_id = session_id, pid = Sys.getpid(), started_at = started)

  response <- e$.ts_tool_run(17L, "spatial_pipeline", NULL, TRUE, expectation)
  expect_false(response$isError)
  expect_true(response$structuredContent$accepted)
  expect_identical(response$structuredContent$button, "spatial-pipeline-btn_run_all")

  scenario <- jsonlite::fromJSON(e$ts_drive_path("scenario.json"), simplifyVector = FALSE)
  expect_identical(scenario$protocol, e$TS_DRIVE_PROTOCOL)
  expect_identical(as.integer(scenario$seq), 17L)
  expect_identical(scenario$module, "spatial_pipeline")
  expect_identical(scenario$action, "run_pipeline")
  expect_identical(scenario$button, "spatial-pipeline-btn_run_all")
  expect_length(scenario$inputs, 0L)
})

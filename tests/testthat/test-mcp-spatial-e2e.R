# =============================================================================
# test-mcp-spatial-e2e.R — the FULL MCP-driven cycle for a SPATIAL session.
# =============================================================================
# WHY THIS FILE EXISTS. Parts A–C of the drive proved each half separately:
# the MCP tools against a fixture session (test-mcp-sc-local.R), the spatial
# import route against its loader (test-mod-import-spatial-drive.R), the
# spatial export route against its exporter (test-mod-spatial-qc-export.R).
# None of them ran the WHOLE chain a real agent runs:
#
#   status -> set_armed -> import_spatial -> [run] spatial_qc hotspots
#          -> export_result (route spatial_qc) -> read -> read_result
#
# This file drives that cycle END TO END on a HERMETIC root: the MCP side
# through .ts_dispatch(tools/call) exactly as a client would (the same
# sandbox pattern as test-mcp-sc-local.R: env-sourced drive files + the
# server's internal definitions, booted on tempdir()); the app side by
# calling the applier functions exactly as the poller does —
# ts_drive_validate_scenario -> ts_drive_apply -> ts_drive_write_result with
# the action-chosen projector, and last_seq raised after each consumed
# scenario. The module seam is a RECORDER (test-bulk-de-staged-injection.R
# pattern): the importer/exporter are published through the real
# ts_drive_publish_importer / ts_drive_publish_export registry calls and the
# `effects` closure dispatches through ts_drive_importer_of /
# ts_drive_export_of, but the LOAD itself is a FIXTURE spatial object (40
# spots, 6 significant — no real data, no BPCells). The EXPORT step runs the
# REAL spatial_qc_export_hotspot_csv() and the READ step the REAL R2
# responder, so the descriptor and the preview are the wire's true shapes.
#
# HERMETICITY. Temp drive root only, unlinked on exit; the real
# tools/_drive/ is never touched; exported CSVs land in the process
# tempdir()/ts_drive_exports and each created file is unlinked on exit.
#
# STEP (b) of the cycle — set_inputs — has NO spatial surface BY DESIGN
# (frozen parameters, folder-only dataset input; see
# docs/mcp_spatial_automation.md §2). It is ASSERTED as absent here, never
# invented.
# =============================================================================

invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))

.mcp_sp_e2e_script <- function() {
  path <- file.path(ts_project_root(), "scripts", "mcp_server.R")
  if (!file.exists(path)) {
    skip("the MCP server is a local, gitignored capability and is absent from this clone")
  }
  path
}

.mcp_sp_e2e_env <- function() {
  path <- .mcp_sp_e2e_script()
  e <- new.env(parent = globalenv())
  # state.R first: ts_drive_registry() reads global_data$drive_registry
  # through state_get(), and the sandbox must not depend on another test
  # file having left it on the globalenv.
  sys.source(file.path(ts_project_root(), "R", "core", "state.R"), envir = e)
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

  # The spatial DOMAIN (exporter, writer, state helpers) in a child env: its
  # drive calls (ts_drive_export_dir, TS_DRIVE_EXPORT_STEM, ...) resolve from
  # the SAME e as the MCP side, so one drive state serves both halves of the
  # cycle. Same file set test-mod-spatial-qc-export.R sources, minus the two
  # drive files this env already carries.
  sp <- new.env(parent = e)
  sp$.tr <- function(key, ...) key
  sp$.t_fmt <- function(template, ...) template
  sp$.tr_plain <- function(key, ...) key
  for (f in c("config/defaults.R", "R/core/io_helpers.R", "R/core/error_state.R",
              "R/spatial/spatial_stats.R", "modules/spatial/mod_spatial_qc.R")) {
    sys.source(file.path(ts_project_root(), f), envir = sp)
  }
  e$sp <- sp

  # HERMETIC root (audit 2026-10-03 pattern): never the real tools/_drive.
  root <- file.path(tempdir(), paste0("tsdrive-mcp-sp-", as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(root, "tools", "_drive"), recursive = TRUE, showWarnings = FALSE)
  e$ts_drive_boot(root)
  e$ts_drive_clear_write_error()
  e$ts_drive_job_clear()
  e
}

#' A live session on the sandbox root. `armed = FALSE` leaves the arming to
#' the tool under test (the cycle test drives set_armed itself).
.mcp_sp_e2e_session <- function(e, armed = TRUE) {
  token <- sprintf("sptok-%d", as.integer(runif(1, 1e5, 9e5)))
  started <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  e$ts_drive_write_ready(list(), token, armed = armed, last_seq = 0L,
                         hb_n = 1L, started_at = started)
  if (armed) {
    e$ts_drive_write_json(
      list(protocol = e$TS_DRIVE_PROTOCOL, token = token, armed = TRUE),
      e$ts_drive_path("arm.json"))
  }
  list(token = token, started = started,
       session_id = e$.ts_session_id(Sys.getpid(), started, token))
}

#' The fixture session state + the module seam, published exactly as the
#' real modules publish (ts_drive_publish_importer / ts_drive_publish_export),
#' driven by a recorder `effects` closure. The loaded object and the hotspot
#' table are FIXTURES: 40 opaque spots on a grid, 6 significant.
.mcp_sp_e2e_state <- function(e) {
  st <- new.env(parent = emptyenv())
  n <- 40L; n_sig <- 6L
  set.seed(4242)
  coords <- data.frame(id = sprintf("E%03d", seq_len(n)),
                       x = as.numeric(seq_len(n) %% 8L),
                       y = as.numeric(seq_len(n) %/% 8L),
                       stringsAsFactors = FALSE)
  qc <- data.frame(id = coords$id, nCount = stats::runif(n, 100, 1000),
                   nFeature = stats::runif(n, 500, 2000),
                   stringsAsFactors = FALSE)
  lab <- rep("NS", n)
  lab[seq_len(n_sig)] <- rep(c("Hotspot (chaud)", "Coldspot (froid)"),
                             length.out = n_sig)
  hotspots <- data.frame(id = coords$id, value = stats::runif(n, 1, 10),
                         gi_star = stats::rnorm(n), p_value = stats::runif(n, 0, 0.2),
                         hotspot = lab, stringsAsFactors = FALSE)

  gd <- shiny::reactiveValues()
  gd$language <- "fr"
  rv <- shiny::reactiveValues()
  rv$qc_metrics <- qc
  gd$drive_registry <- new.env(parent = emptyenv())
  counter <- shiny::reactiveVal(0L)

  st$gd <- gd; st$rv <- rv
  st$hotspots <- hotspots
  st$fixture_obj <- list(coords = coords)
  st$spqc_counter <- counter
  st$bumped <- list(); st$import_requests <- list(); st$export_calls <- 0L
  st$files <- character(0)   # exported artefacts, unlinked by the tests

  # The readiness guard, same shape and same refusal text as
  # .spatial_qc_drive_ready(): coords present or a REASON string.
  st$ready_guard <- function() {
    c_now <- tryCatch(shiny::isolate(gd$spatial_obj$coords), error = function(e) NULL)
    if (!is.data.frame(c_now) || !all(c("id", "x", "y") %in% names(c_now)) ||
        !nrow(c_now)) {
      return("no spatial dataset loaded (global_data$spatial_obj$coords is NULL)")
    }
    TRUE
  }

  # The module's OWN publishes, on the module's own shapes.
  e$ts_drive_publish_importer(gd, e$TS_DRIVE_SPATIAL_IMPORT_MODULE, function(request) {
    st$import_requests <- c(st$import_requests, list(request))
    # The FIXTURE object stands in for the real loader (the recorder seam):
    # the importer "loads" the folder into the session's spatial object.
    gd$spatial_obj <- st$fixture_obj
    list(ok = TRUE, warnings = character(0))
  })
  e$ts_drive_publish_export(gd, e$TS_DRIVE_SPATIAL_QC_MODULE, function() {
    # The REAL exporter, welded to this session's rv exactly as
    # mod_spatial_qc.R:1159 welds it.
    out <- e$sp$spatial_qc_export_hotspot_csv(rv, gd)
    if (isTRUE(out$ok)) {
      st$files <- c(st$files,
                    file.path(e$ts_drive_export_dir(), out$descriptor$file))
    }
    out
  })
  st
}

#' The module-side `effects` seam, one closure, the poller's three uses.
.mcp_sp_e2e_effects <- function(e, st) {
  force(e); force(st)
  function(id, mode = NULL, module = NULL, request = NULL) {
    if (is.null(mode)) {
      # effects(input_id): fire ONE bound button. The counter moves and the
      # module's observeEvent body "runs" — simulated by storing the FIXTURE
      # Getis-Ord table, the same store the real observer publishes through
      # spatial_hotspot_store().
      if (!identical(id, "spatial-qc-btn_hotspots")) return(FALSE)
      st$bumped <- c(st$bumped, list(id))
      cnt <- st$spqc_counter
      cnt(shiny::isolate(cnt()) + 1L)
      st$rv$hotspot_result <- st$hotspots
      st$rv$hotspot_params <- list(source = "qc", metric = "nCount",
                                   k_neighbors = 30L)
      return(TRUE)
    }
    if (identical(mode, "tokens")) {
      if (!identical(module, "spatial_qc")) return(list())
      return(stats::setNames(list(list(
        counter = st$spqc_counter, ready = st$ready_guard,
        state = NULL, long = TRUE)), "spatial-qc-btn_hotspots"))
    }
    if (identical(mode, "import")) {
      out <- e$ts_drive_importer_of(st$gd, module)
      if (!is.function(out)) return(NULL)
      return(out(request))
    }
    if (identical(mode, "export")) {
      st$export_calls <- st$export_calls + 1L
      out <- e$ts_drive_export_of(st$gd, module)
      if (!is.function(out)) return(NULL)
      return(out())
    }
  }
}

#' ONE poller beat, exactly as ts_drive_tick applies a consumed scenario:
#' read scenario.json, validate, apply, write the verdict with the
#' action-chosen projector, raise last_seq.
.mcp_sp_e2e_beat <- function(e, token, effects) {
  scn_raw <- e$ts_drive_read_json(e$ts_drive_path("scenario.json"))
  hb <- e$ts_drive_read_ready()
  last_seq <- suppressWarnings(as.integer(hb$last_seq %|||% 0L))
  v <- e$ts_drive_validate_scenario(scn_raw, token, last_seq)
  if (!identical(v$status, "applied-candidate")) {
    return(list(validate = v, res = NULL))
  }
  scn <- v$scenario
  res <- e$ts_drive_apply(NULL, NULL, scn, effects = effects,
                          owner_token = token)
  e$ts_drive_write_result(
    scn$seq, res$status, res$active_module %||% scn$module, TRUE,
    errors = c(v$errors, res$errors), warnings = c(v$warnings, res$warnings),
    snapshot = NULL,
    descriptor = res$descriptor,
    descriptor_projector = if (identical(scn$action, "read_export")) {
      e$ts_drive_read_descriptor
    } else {
      e$ts_drive_export_descriptor
    })
  # The poller raises last_seq once the scenario is consumed.
  hb$last_seq <- as.integer(scn$seq)
  e$ts_drive_write_json(hb, e$ts_drive_path("ready.json"))
  list(validate = v, res = res, seq = scn$seq, action = scn$action,
       module = scn$module)
}

#' The long-job closing half, exactly as the tick's job-pending block
#' resolves it: the module declares the job over, the NEXT beat publishes
#' the terminal verdict and clears the job.
.mcp_sp_e2e_job_finish <- function(e, token) {
  job <- e$ts_drive_job_state()
  if (is.null(job)) stop("no job in flight")
  ok <- e$ts_drive_job_finish(job$button, status = "done",
                              job_id = job$job_id, owner_token = token)
  pend <- e$ts_drive_job_pending()
  e$ts_drive_write_result(as.integer(job$seq),
                          pend$wire_status %||% pend$status,
                          job$module, TRUE, errors = character(0),
                          snapshot = NULL)
  e$ts_drive_job_clear()
  invisible(ok)
}

#' The read application, EXACTLY the two calls the established pattern uses
#' (test-mcp-sc-local.R .mcp_sc_local_apply_read): the RAW scenario's seq and
#' max_rows into the R2 responder, then the verdict written by the READ
#' projector. (The live validator whitelist drops max_rows — finding F-1 in
#' docs/mcp_spatial_automation.md; the raw-scenario path is the app's intent.)
.mcp_sp_e2e_apply_read <- function(e, token) {
  scn <- e$ts_drive_read_json(e$ts_drive_path("scenario.json"))
  res <- e$ts_drive_read_export_respond(as.integer(scn$seq), scn$max_rows)
  e$ts_drive_write_result(as.integer(scn$seq), res$status,
                          res$active_module %||% scn$module, TRUE,
                          errors = res$errors, warnings = res$warnings,
                          descriptor = res$descriptor,
                          descriptor_projector = e$ts_drive_read_descriptor)
  hb <- e$ts_drive_read_ready()
  hb$last_seq <- as.integer(scn$seq)
  e$ts_drive_write_json(hb, e$ts_drive_path("ready.json"))
  res
}

#' One tools/call. .ts_dispatch answers the JSON-RPC ENVELOPE (the tool result
#' lives under $result, a protocol error under $error); the tests speak in
#' tool results, so the envelope is unwrapped here.
.mcp_sp_e2e_call <- function(e, id, name, arguments) {
  resp <- e$.ts_dispatch(list(jsonrpc = "2.0", id = id, method = "tools/call",
                              params = list(name = name, arguments = arguments)))
  if (!is.null(resp$error)) {
    return(list(isError = TRUE, structuredContent = resp$error,
                content = list(list(text = resp$error$message))))
  }
  resp$result
}

`%|||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# =============================================================================
# THE FULL CYCLE (steps a, c, d, e — plus the bootstrap and step b's absence).
# =============================================================================
test_that("the FULL spatial cycle: arm, import, run, export, read", {
  e <- .mcp_sp_e2e_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  st <- .mcp_sp_e2e_state(e)
  on.exit(unlink(unique(st$files), force = TRUE), add = TRUE)
  eff <- .mcp_sp_e2e_effects(e, st)
  contract <- e$TS_DRIVE_EXPORT_COLUMNS$spatial_qc$fixed

  # ── bootstrap: status before arming, then the arming write ────────────────
  fx <- .mcp_sp_e2e_session(e, armed = FALSE)
  r <- .mcp_sp_e2e_call(e, 1L, "transcripto_drive_status", list())
  expect_false(r$isError)
  expect_false(r$structuredContent$session$armed)
  expect_identical(r$structuredContent$session$session_id, fx$session_id)

  r <- .mcp_sp_e2e_call(e, 2L, "transcripto_drive_set_armed",
                        list(armed = TRUE, expect = list(session_id = fx$session_id)))
  expect_false(r$isError)
  expect_true(r$structuredContent$wrote)
  expect_false(r$structuredContent$rescued_stale)
  arm <- jsonlite::fromJSON(e$ts_drive_path("arm.json"), simplifyVector = FALSE)
  expect_true(arm$armed)
  expect_identical(arm$token, fx$token)
  # The app applies the arm on its next beat: the handshake flips to armed.
  e$ts_drive_write_ready(list(), fx$token, armed = TRUE, last_seq = 0L,
                         hb_n = 2L, started_at = fx$started)
  r <- .mcp_sp_e2e_call(e, 3L, "transcripto_drive_status", list())
  expect_true(r$structuredContent$session$armed)

  # ── step (a): import_spatial ──────────────────────────────────────────────
  sp_dir <- file.path(tempdir(), "ts-mcp-sp-fixture-dir")
  dir.create(sp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(sp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  r <- .mcp_sp_e2e_call(e, 4L, "transcripto_drive_import",
    list(seq = 1L, module = "import_spatial",
         import = list(dir_path = sp_dir, technology = "visium"),
         expect = list(session_id = fx$session_id)))
  expect_false(r$isError, info = "the spatial import dispatch must be accepted")
  expect_true(r$structuredContent$accepted)
  expect_identical(r$structuredContent$action, "import_file")
  expect_false(r$structuredContent$applied)
  expect_setequal(r$structuredContent$import_keys, c("dir_path", "technology"))

  scn <- jsonlite::fromJSON(e$ts_drive_path("scenario.json"), simplifyVector = FALSE)
  expect_identical(scn$protocol, e$TS_DRIVE_PROTOCOL)
  expect_identical(as.integer(scn$seq), 1L)
  expect_identical(scn$module, "import_spatial")
  expect_identical(scn$action, "import_file")
  expect_identical(scn$session_token, fx$token)
  expect_identical(scn$import$technology, "visium")

  # app side: one poller beat applies the import through the REAL seam
  b <- .mcp_sp_e2e_beat(e, fx$token, eff)
  expect_identical(b$action, "import_file")
  expect_identical(b$res$status, "done")
  expect_length(st$import_requests, 1L)
  expect_identical(basename(st$import_requests[[1]]$dir_path), basename(sp_dir))
  expect_identical(st$import_requests[[1]]$technology, "visium")
  # the FIXTURE spatial object is what the session now holds
  obj <- shiny::isolate(st$gd$spatial_obj$coords)
  expect_true(is.data.frame(obj))
  expect_identical(nrow(obj), 40L)
  # the verdict is terminal done for this seq, readable by read_result
  rr <- e$.ts_tool_read_result()
  expect_identical(rr$structuredContent$status, "done")
  expect_true(rr$structuredContent$status_terminal)
  expect_identical(as.integer(rr$structuredContent$ack_seq), 1L)

  # ── step (b): NO set_inputs surface for spatial — asserted, not invented ──
  expect_false(any(grepl("^(spatial|import_spatial)", names(e$TS_MCP_INPUT_SCHEMA))))
  expect_false("spatial_qc" %in% e$TS_MCP_SET_INPUT_MODULES)
  expect_false("spatial_pipeline" %in% e$TS_MCP_SET_INPUT_MODULES)
  r <- .mcp_sp_e2e_call(e, 5L, "transcripto_drive_set_inputs",
    list(seq = 2L, module = "spatial_qc",
         inputs = list(`spatial-qc-hotspot_k` = 30L),
         expect = list(session_id = fx$session_id)))
  expect_true(r$isError)
  expect_identical(r$structuredContent$code, "MODULE_NOT_ALLOWED")
  # NO new write: the on-disk scenario.json is still the import one, untouched
  expect_identical(as.integer(
    jsonlite::fromJSON(e$ts_drive_path("scenario.json"),
                       simplifyVector = FALSE)$seq), 1L)

  # ── step (c): run_pipeline on spatial_qc ──────────────────────────────────
  r <- .mcp_sp_e2e_call(e, 6L, "transcripto_drive_run",
    list(seq = 2L, module = "spatial_qc",
         expect = list(session_id = fx$session_id)))
  expect_false(r$isError, info = "the spatial run dispatch must be accepted")
  expect_true(r$structuredContent$accepted)
  expect_identical(r$structuredContent$button, "spatial-qc-btn_hotspots")
  scn <- jsonlite::fromJSON(e$ts_drive_path("scenario.json"), simplifyVector = FALSE)
  expect_identical(scn$action, "run_pipeline")
  expect_identical(scn$module, "spatial_qc")

  # CONTRACT A: the module declared long = TRUE, so the beat answers running
  b <- .mcp_sp_e2e_beat(e, fx$token, eff)
  expect_identical(b$action, "run_pipeline")
  expect_identical(b$res$status, "running")
  rr <- e$.ts_tool_read_result()
  expect_identical(rr$structuredContent$status, "running")
  expect_false(rr$structuredContent$status_terminal)
  expect_length(st$bumped, 1L)   # the token moved exactly once

  # CONTRACT B on the wire: a second run while the verdict is running refuses
  r2 <- .mcp_sp_e2e_call(e, 7L, "transcripto_drive_run",
    list(seq = 3L, module = "spatial_qc",
         expect = list(session_id = fx$session_id)))
  expect_true(r2$isError)
  expect_identical(r2$structuredContent$code, "JOB_ALREADY_RUNNING")

  # the module closes its job; the next beat publishes the terminal verdict
  .mcp_sp_e2e_job_finish(e, fx$token)
  rr <- e$.ts_tool_read_result()
  expect_identical(rr$structuredContent$status, "done")
  expect_true(rr$structuredContent$status_terminal)
  expect_identical(as.integer(rr$structuredContent$ack_seq), 2L)
  # the run's own outcome is readable through the module state: 6 significant
  expect_identical(as.integer(sum(shiny::isolate(st$rv$hotspot_result$hotspot) != "NS")), 6L)

  # ── step (d): export_result on the spatial_qc route ───────────────────────
  r <- .mcp_sp_e2e_call(e, 8L, "transcripto_drive_export",
    list(seq = 3L, module = "spatial_qc",
         expect = list(session_id = fx$session_id)))
  expect_false(r$isError, info = "the spatial export dispatch must be accepted")
  expect_identical(r$structuredContent$dispatched, "export_result")
  expect_identical(r$structuredContent$module, "spatial_qc")
  scn <- jsonlite::fromJSON(e$ts_drive_path("scenario.json"), simplifyVector = FALSE)
  expect_identical(scn$action, "export_result")
  expect_identical(scn$module, "spatial_qc")
  expect_identical(scn$session_token, fx$token)

  # app side: the beat runs the REAL spatial_qc exporter offline
  b <- .mcp_sp_e2e_beat(e, fx$token, eff)
  expect_identical(b$res$status, "done")
  expect_false(is.null(b$res$descriptor))
  expect_identical(st$export_calls, 1L)

  rr <- e$.ts_tool_read_result()
  d <- rr$structuredContent$descriptor
  expect_false(is.null(d), info = "a done export verdict carries its descriptor")
  expect_identical(d$format, "csv")
  expect_match(d$file, "^spatial_qc_hotspots_[0-9]+\\.csv$")
  expect_gt(d$bytes, 0)
  expect_identical(as.integer(d$bytes),
                   as.integer(file.size(file.path(e$ts_drive_export_dir(), d$file))))
  expect_identical(as.integer(d$n_rows), 40L)
  expect_identical(as.integer(d$n_cols), 5L)
  expect_identical(as.integer(d$n_sig), 6L)
  expect_identical(as.character(d$columns), contract)

  # ── step (e): transcripto_drive_read over the export verdict ──────────────
  r <- .mcp_sp_e2e_call(e, 9L, "transcripto_drive_read",
    list(seq = 4L, max_rows = 5L, expect = list(session_id = fx$session_id)))
  expect_false(r$isError, info = "the read dispatch must be accepted")
  expect_identical(r$structuredContent$dispatched, "read_export")
  expect_identical(r$structuredContent$max_rows, 5L)
  scn <- jsonlite::fromJSON(e$ts_drive_path("scenario.json"), simplifyVector = FALSE)
  expect_identical(scn$action, "read_export")
  expect_identical(as.integer(scn$max_rows), 5L)
  expect_identical(scn$session_token, fx$token)
  # NO route / handle / file argument exists on the read payload
  expect_false(any(c("route", "handle", "file", "dir", "path") %in% names(scn)))

  # app side: the R2 responder streams the REAL file; the verdict is written
  # by the READ projector, exactly the poller's two calls
  res <- .mcp_sp_e2e_apply_read(e, fx$token)
  expect_identical(res$status, "done")

  rr <- e$.ts_tool_read_result()
  d <- rr$structuredContent$descriptor
  expect_false(is.null(d))
  expect_true(all(names(d) %in% e$TS_DRIVE_READ_KEYS))
  expect_match(as.character(d$route), "spatial_qc")
  expect_identical(as.character(d$handle), as.character(b$res$descriptor$file))
  pv <- d$preview
  expect_identical(as.integer(pv$rows_returned), 5L)
  # honest truncation signal: the file holds 40 rows, the preview 5 — "more
  # rows exist than were returned", derived from the ONE extra streamed record
  expect_true(isTRUE(pv$truncated_rows))
  expect_identical(as.character(pv$columns), contract)
  expect_identical(as.integer(pv$cells_truncated), 0L)
  expect_identical(as.integer(pv$cells_guarded), 0L)
  expect_length(d$col_summary, 5L)

  # the preview IS the file head, cell for cell
  head5 <- utils::read.csv(file.path(e$ts_drive_export_dir(), d$handle),
                           nrows = 5L, colClasses = "character",
                           check.names = FALSE)
  for (i in seq_len(5L)) {
    expect_identical(as.character(unlist(pv$rows[[i]])),
                     as.character(unname(head5[i, ])),
                     info = sprintf("preview row %d equals the file head", i))
  }
})

# =============================================================================
# STEP (f): the refusals. A refused write NEVER creates scenario.json; the
# read's NO_EXPORT_TARGET is derived STATELESSLY from result.json.
# =============================================================================
test_that("refusals: no scenario write on refusal, one read per export, stateless derivations", {
  e <- .mcp_sp_e2e_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  st <- .mcp_sp_e2e_state(e)
  on.exit(unlink(unique(st$files), force = TRUE), add = TRUE)
  fx <- .mcp_sp_e2e_session(e, armed = TRUE)
  expect <- list(session_id = fx$session_id)

  # Build the export target directly: the REAL exporter over the fixture
  # table, the verdict written exactly as the poller writes it.
  st$rv$hotspot_result <- st$hotspots
  out <- e$ts_drive_export_of(st$gd, "spatial_qc")()
  expect_true(isTRUE(out$ok))
  e$ts_drive_write_result(10L, "done", "spatial_qc", TRUE,
                          descriptor = out$descriptor)
  hb <- e$ts_drive_read_ready(); hb$last_seq <- 10L
  e$ts_drive_write_json(hb, e$ts_drive_path("ready.json"))

  # 1. PAYLOAD_REFUSED (max_rows = 0): tool result, NOTHING is written —
  #    scenario.json does not even come into existence.
  r <- .mcp_sp_e2e_call(e, 11L, "transcripto_drive_read",
    list(seq = 11L, max_rows = 0L, expect = expect))
  expect_true(r$isError)
  expect_identical(r$structuredContent$code, "PAYLOAD_REFUSED")
  expect_false(file.exists(e$ts_drive_path("scenario.json")))

  # 2. the happy read consumes the export verdict
  r <- .mcp_sp_e2e_call(e, 12L, "transcripto_drive_read",
    list(seq = 11L, max_rows = 3L, expect = expect))
  expect_false(r$isError)
  res <- .mcp_sp_e2e_apply_read(e, fx$token)
  expect_identical(res$status, "done")

  # 3. the SECOND read: the verdict is already a read verdict — refused
  #    STATELESSLY, and neither scenario.json nor result.json moves.
  scn_before <- paste(readBin(e$ts_drive_path("scenario.json"), "raw",
                              file.size(e$ts_drive_path("scenario.json"))),
                      collapse = " ")
  res_before <- paste(readBin(e$ts_drive_path("result.json"), "raw",
                              file.size(e$ts_drive_path("result.json"))),
                      collapse = " ")
  r2 <- .mcp_sp_e2e_call(e, 13L, "transcripto_drive_read",
    list(seq = 12L, expect = expect))
  expect_true(r2$isError)
  expect_identical(r2$structuredContent$code, "NO_EXPORT_TARGET")
  expect_match(r2$structuredContent$message, "already a read verdict", fixed = TRUE)
  expect_identical(paste(readBin(e$ts_drive_path("scenario.json"), "raw",
                                 file.size(e$ts_drive_path("scenario.json"))),
                         collapse = " "), scn_before)
  expect_identical(paste(readBin(e$ts_drive_path("result.json"), "raw",
                                 file.size(e$ts_drive_path("result.json"))),
                         collapse = " "), res_before)

  # 4. a RUNNING verdict is not an artefact: NO_EXPORT_TARGET, no write.
  e$ts_drive_write_result(11L, "running", "spatial_qc", TRUE, descriptor = NULL)
  r3 <- .mcp_sp_e2e_call(e, 14L, "transcripto_drive_read",
    list(seq = 13L, expect = expect))
  expect_true(r3$isError)
  expect_identical(r3$structuredContent$code, "NO_EXPORT_TARGET")
  expect_identical(paste(readBin(e$ts_drive_path("scenario.json"), "raw",
                                 file.size(e$ts_drive_path("scenario.json"))),
                         collapse = " "), scn_before)

  # 5. a STALE verdict is not this session's: RESULT_SESSION_MISMATCH (the
  #    attribution comparison, reused), not NO_EXPORT_TARGET.
  resj <- jsonlite::fromJSON(e$ts_drive_path("result.json"), simplifyVector = FALSE)
  resj$applied_at <- "2000-01-01T00:00:00Z"
  e$ts_drive_write_json(resj, e$ts_drive_path("result.json"))
  r4 <- .mcp_sp_e2e_call(e, 15L, "transcripto_drive_read",
    list(seq = 14L, expect = expect))
  expect_true(r4$isError)
  expect_identical(r4$structuredContent$code, "RESULT_SESSION_MISMATCH")
  expect_identical(paste(readBin(e$ts_drive_path("scenario.json"), "raw",
                                 file.size(e$ts_drive_path("scenario.json"))),
                         collapse = " "), scn_before)
})

# =============================================================================
# STEP (b), documented in full: the spatial set_inputs gap is STRUCTURAL —
# no schema ids, no module enum entry, no allowlisted id, and the watcher
# refuses independently of the server.
# =============================================================================
test_that("step (b): spatial exposes NO set_inputs surface, on both sides", {
  e <- .mcp_sp_e2e_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  fx <- .mcp_sp_e2e_session(e, armed = TRUE)

  # the server's value schema names no spatial input; the two spatial RUN
  # modules are excluded from the M3b module set. `import_spatial` stays in
  # the enum (its button is bound) but owns NO settable input: the only
  # allowlisted entry it has is the button itself.
  expect_false(any(grepl("^(spatial|import_spatial)", names(e$TS_MCP_INPUT_SCHEMA))))
  expect_false("spatial_qc" %in% e$TS_MCP_SET_INPUT_MODULES)
  expect_false("spatial_pipeline" %in% e$TS_MCP_SET_INPUT_MODULES)
  sp_allow <- e$TS_DRIVE_ALLOWLIST[grepl("^(spatial|import_spatial)",
                                         names(e$TS_DRIVE_ALLOWLIST))]
  expect_true(all(vapply(sp_allow, function(x) identical(x$kind, "button"),
                         logical(1))))

  # the tool's own schema agrees
  tools <- stats::setNames(e$.ts_tools(),
                           vapply(e$.ts_tools(), function(x) x$name, character(1)))
  set_mods <- unlist(tools$transcripto_drive_set_inputs$inputSchema$properties$module$enum)
  expect_false("spatial_qc" %in% set_mods)
  expect_false("spatial_pipeline" %in% set_mods)

  # no spatial id exists in the app's allowlist at all — the folder is a
  # shinyDirButton (no update* equivalent) and the QC parameters are frozen
  expect_null(e$ts_drive_allowlist_get("spatial-qc-hotspot_k"))
  expect_null(e$ts_drive_allowlist_get("spatial-qc-hotspot_source"))

  # the dispatch is refused BEFORE any write
  r <- e$.ts_tool_set_inputs(3L, "spatial_qc",
                             list(`spatial-qc-hotspot_k` = 30L), TRUE,
                             list(session_id = fx$session_id))
  expect_true(r$isError)
  expect_identical(r$structuredContent$code, "MODULE_NOT_ALLOWED")
  expect_false(file.exists(e$ts_drive_path("scenario.json")))

  # and the WATCHER refuses independently (defence in depth, not redundancy)
  out <- e$ts_drive_apply(NULL, NULL,
    list(seq = 3L, module = "spatial_qc", action = "set_inputs",
         inputs = list(`spatial-qc-hotspot_k` = 30L)),
    effects = NULL)
  expect_identical(out$status, "invalid")
  expect_match(paste(out$errors, collapse = " "), "does not accept set_inputs",
               fixed = TRUE)
})

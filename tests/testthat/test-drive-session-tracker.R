# =============================================================================
# test-drive-session-tracker.R — DSTV-1 RED tests (O31, GO PHASÉ, DSTV-1 d'abord)
# =============================================================================
# Contract: docs/contracts/DSTV1_SESSION_TRACKER_CONTRACT.md (RED-first — these
# tests are written AGAINST that contract BEFORE any implementation; they MUST
# fail on the unmodified tree, then pin the milestone gate).
#
# Scope (contract §5): producer `drive_watcher.R` (A1+A2), MCP read-side
# `scripts/mcp_server.R` (A3, read-only), `tools/_drive/README.md` state.json
# row (O11 §6). NO UI surface (DSTV-3), no allowlist routes (DSTV-2).
#
# Sandbox discipline (house style, precedents test-drive-sandbox.R:20-34 and
# test-drive-passive-state.R:42-69):
#   * every test boots its OWN temp root and sets TRANSCRIPTO_DRIVE_ROOT to it
#     (restored on exit) — the live tools/_drive/ of the working tree is never
#     touched;
#   * NO Shiny app is booted here: contract §7 rows 1-6 are headless-assertable
#     R-side ("Oui"), so no port is opened and none is needed — the "own port"
#     rule targets app-driving sessions (helper-app-driver.R), which these
#     tests deliberately are not;
#   * no arm.json is ever written: observation WITHOUT arming is the very point
#     of the passive channel (contract §6.1, O11 DECISIONS.md:53).
#
# Open-point decisions encoded below (contract §8 — tranchés conservativement,
# each is the implementer's target; see DSTV-1 openPointDecisions record):
#   §8.1 schema — state.json stays FLAT + THREE additive top-level fields:
#     `active_module_key` (A1 clean key), `pipeline_step` (NULL unless status
#     is a step), `business_state` object carrying the FIVE fiche-frozen
#     scalars verbatim (has_data, object_class, n_elements, n_clusters,
#     deconv_done), ALWAYS present (defaults has_data=FALSE / deconv_done=FALSE,
#     unknown scalars NULL — same shape the MCP no-verdict snapshot already
#     models, mcp_server.R:1124-1126). NO per-transition timestamps: the
#     document-level `timestamp` is the single horodatage (contract §8.1).
#   §8.2 vocabulary — additive English extension of the existing set:
#     {idle, running, error} (drive_watcher.R:3659-3670, frozen at
#     test-drive-passive-state.R:66) + `step` (fiche's own term) + `done` (the
#     protocol terminal already named tools/_drive/README.md:131-135). No
#     French value, no rename. Manual state only fills the "idle" gap: the
#     drive-layer detection (job busy, O18 recency window on the error ring)
#     keeps priority — DSTV-1 ADDS to it, never replaces it (contract §3.3).
#   §8.3 step naming — the step value reuses the EXISTING accordion-panel
#     value convention ("0_autopipeline", modules/sc/mod_sc.R:714; frozen
#     drive-side as TS_DRIVE_SC_PANELS, drive_allowlist.R:76). No new names.
#   §8.4 feed — the emitter ALREADY receives `global_data` (drive_watcher.R:
#     3700-3707); the manual state reaches it as ALREADY-PUBLISHED scalars on
#     that parameter, probe-style (precedent README.md:453-461):
#       global_data$manual_pipeline = list(status=, step=)      (scalars)
#       global_data$business_state = list(has_data=, object_class=,
#                                         n_elements=, n_clusters=,
#                                         deconv_done=)        (scalars)
#     The emitter reads them defensively (never throws, malformed ⇒ defaults)
#     and NEVER inspects a Seurat object or a matrix (README.md:110, C2).
#   §8.5 retention — NO new retention rule: the error ring stays a 20-entry
#     ERROR ring; manual-run transitions leave no ring entry; the only
#     retained memory of a manual run is the current hash-gated document
#     itself. Error recency stays O18's TS_DRIVE_ERROR_RECENCY_SECS window.
#   §8.6 README row — one `state.json` row in the tools/_drive/README.md file
#     table (| File | Writer | Meaning |, :14-21), writer = the app, per O11 §6
#     (DRIVE_PASSIVE_STATE_PROPOSAL.md:73-75); row measured ABSENT at
#     DECISIONS.md:105 — the test below pins its existence.
#   §8.7 A1 echo into result.json — DEFERRED: the contract scopes the textual
#     guard to state.json (critère #2, fiche :109); the result.json verdict
#     path carries clean module keys today (no measured T4 leak there), and
#     touching the verdict schema costs a frozen-contract extension for zero
#     measured gain — re-measure at the DSTV-2 relecture (O31).
# =============================================================================

# Ensure allowlist and drive watcher are loaded (same bootstrap as
# test-drive-passive-state.R:5-16 — R/core first, skeleton tree fallback).
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

.dstv1_or <- function(a, b) if (is.null(a)) b else a

# --- Fixtures ----------------------------------------------------------------

#' Own temp root + TRANSCRIPTO_DRIVE_ROOT sandbox for one test.
#' Never touches the live tools/_drive/ of the working tree. The env var stays
#' scoped to the CALLING test frame (withr::local_envvar) — restored on exit,
#' so the file is safe inside the full suite.
.dstv1_sandbox <- function(tag, envir = parent.frame()) {
  root <- file.path(tempdir(), paste0("tsdrive-dstv1-", tag, "-", as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(root, "tools", "_drive"), recursive = TRUE, showWarnings = FALSE)
  withr::local_envvar(TRANSCRIPTO_DRIVE_ROOT = root, .local_envir = envir)
  ts_drive_boot(root)
  # Reset every session-global that could leak across tests (precedent
  # test-drive-watcher.R:33-44: the state lives in a session-global env).
  ts_drive_clear_errors()
  ts_drive_clear_write_error()
  ts_drive_job_clear()
  root
}

#' Internal slice of scripts/mcp_server.R (.ts_json → .ts_serve) over a sandbox
#' root — precedent test-drive-passive-state.R:18-46 (.mcp_passive_env). The
#' slice env carries its OWN drive singleton: it must be booted onto the
#' sandbox root itself (precedent :42-44, e$ts_drive_boot(root)).
.dstv1_mcp_env <- function(tag) {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "drive_allowlist.R"), envir = e)
  watcher_path <- if (file.exists(file.path(ts_project_root(), "R", "core", "drive_watcher.R"))) {
    file.path(ts_project_root(), "R", "core", "drive_watcher.R")
  } else {
    file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "drive_watcher.R")
  }
  sys.source(watcher_path, envir = e)

  expressions <- parse(file.path(ts_project_root(), "scripts", "mcp_server.R"), keep.source = TRUE)
  labels <- vapply(expressions, function(x) paste(deparse(x, width.cutoff = 500L), collapse = "\n"), character(1))
  first <- which(grepl("^\\.ts_json <- function", labels))[1L]
  last <- which(grepl("^\\.ts_serve <- function", labels))[1L]
  if (is.na(first) || is.na(last) || last <= first) {
    stop("mcp_server.R internal definition boundaries were not found")
  }
  for (i in seq.int(first, last - 1L)) eval(expressions[[i]], envir = e)

  root <- .dstv1_sandbox(tag)
  e$ts_drive_boot(root)
  e$ts_drive_clear_errors()
  e$ts_drive_clear_write_error()
  attr(e, "sandbox_root") <- root
  e
}

#' Hand-written FRESH ready.json (house precedent: hand-written arm.json,
#' test-drive-watcher.R .drv_write_arm) — the minimum honest handshake so the
#' snapshot tool can run headless. Heartbeat timestamp = now ⇒ fresh. Written
#' through the SLICE env's own drive functions (its booted root).
.dstv1_write_ready <- function(e, token) {
  e$ts_drive_write_json(
    list(
      protocol      = e$TS_DRIVE_PROTOCOL,
      armed         = FALSE,
      pid           = Sys.getpid(),
      port          = NULL,
      root          = e$ts_drive_root(),
      session_token = token,
      viewer        = "test_sandbox",
      last_seq      = 0L,
      started_at    = e$ts_drive_now_iso(),
      hb_at         = e$ts_drive_now_iso(),
      hb_n          = 0L,
      hb_timeout_s  = e$ts_drive_hb_timeout()
    ),
    e$ts_drive_path("ready.json")
  )
}

#' Published-scalar feed (open point §8.4): what the module layer publishes on
#' global_data for the emitter to carry — scalars only, never an object.
.dstv1_manual_gd <- function(status = NULL, step = NULL, has_data = FALSE,
                             object_class = NULL, n_elements = NULL,
                             n_clusters = NULL, deconv_done = FALSE) {
  list(
    manual_pipeline = if (is.null(status)) NULL else list(status = status, step = step),
    business_state  = list(has_data = has_data, object_class = object_class,
                           n_elements = n_elements, n_clusters = n_clusters,
                           deconv_done = deconv_done)
  )
}

#' Walk parsed state.json leaves; return every character leaf carrying an HTML
#' tag (the §7#2 textual guard: "plus jamais de HTML dans un champ machine").
.dstv1_html_leaves <- function(x, name = "") {
  leaves <- character(0)
  if (is.list(x)) {
    nm <- .dstv1_or(names(x), rep("", length(x)))
    for (i in seq_along(x)) {
      nm_i <- .dstv1_or(nm[i], "")
      child <- if (nzchar(nm_i)) (if (nzchar(name)) paste0(name, "$", nm_i) else nm_i) else name
      leaves <- c(leaves, .dstv1_html_leaves(x[[i]], child))
    }
  } else if (is.character(x) && length(x) >= 1L) {
    hits <- x[grepl("<[a-zA-Z!/][^>]*>", x)]
    if (length(hits)) leaves <- c(leaves, paste0(name, ": ", hits))
  }
  leaves
}

#' Walk parsed state.json; return every `completion_verified` leaf that is
#' neither absent (NULL) nor FALSE — §7#4: absent or FALSE, never TRUE.
.dstv1_completion_violations <- function(x, name = "") {
  out <- character(0)
  if (is.list(x)) {
    nm <- .dstv1_or(names(x), rep("", length(x)))
    for (i in seq_along(x)) {
      nm_i <- .dstv1_or(nm[i], "")
      child <- if (nzchar(nm_i)) (if (nzchar(name)) paste0(name, "$", nm_i) else nm_i) else name
      if (identical(nm_i, "completion_verified")) {
        v <- x[[i]]
        if (!(is.null(v) || isFALSE(v))) out <- c(out, paste0(child, " is not FALSE"))
      } else {
        out <- c(out, .dstv1_completion_violations(x[[i]], child))
      }
    }
  }
  out
}

.dstv1_emptyish <- function(v) is.null(v) || length(v) == 0L

# ── §7 #1 (T3) — a manual pipeline run visibly progresses state.json ─────────

test_that("a MANUAL pipeline run progresses state.json: pipeline_status leaves idle and business_state.has_data stays true (contract §7#1)", {
  sb <- .dstv1_sandbox("progress")
  on.exit(unlink(sb, recursive = TRUE, force = TRUE), add = TRUE)

  token <- "tok_dstv1_progress_001"
  gd <- .dstv1_manual_gd(status = "running", has_data = TRUE,
                         object_class = "Seurat", n_elements = 1234,
                         n_clusters = 8, deconv_done = FALSE)

  # PENDANT the manual run — the emitter is fed the published manual state.
  w1 <- ts_drive_emit_passive_state(token = token, global_data = gd)
  expect_true(w1)                      # first emission for a fresh token writes
  st1 <- ts_drive_read_state()
  expect_false(is.null(st1))
  # T3 verbatim: state.json must NOT keep pipeline_status: idle during a run.
  expect_identical(st1$pipeline_status, "running")
  expect_false(identical(st1$pipeline_status, "idle"))
  # business_state appears, with the five fiche-frozen scalars (A2).
  # (Numerics: expect_equal — the JSON round-trip returns whole numbers as
  # integer; expect_identical would compare 1234L to 1234 and lie.)
  expect_false(is.null(st1$business_state))
  expect_identical(st1$business_state$has_data, TRUE)
  expect_identical(st1$business_state$object_class, "Seurat")
  expect_equal(st1$business_state$n_elements, 1234)
  expect_equal(st1$business_state$n_clusters, 8)
  expect_identical(st1$business_state$deconv_done, FALSE)

  # PROGRESSES — the manual pipeline steps: status "step", step named with the
  # EXISTING panel value convention (§8.3: "0_autopipeline", mod_sc.R:714).
  gd$manual_pipeline$status <- "step"
  gd$manual_pipeline$step   <- "0_autopipeline"
  w2 <- ts_drive_emit_passive_state(token = token, global_data = gd)
  expect_true(w2)                      # content changed ⇒ hash-gated write
  st2 <- ts_drive_read_state()
  expect_identical(st2$pipeline_status, "step")
  expect_identical(st2$pipeline_step, "0_autopipeline")

  # TERMINÉ — the module's DECLARATION of completion (never a proof, §6.2).
  gd$manual_pipeline$status <- "done"
  gd$manual_pipeline$step   <- NULL
  w3 <- ts_drive_emit_passive_state(token = token, global_data = gd)
  expect_true(w3)
  st3 <- ts_drive_read_state()
  expect_identical(st3$pipeline_status, "done")
  # APRÈS the run, business_state.has_data is still true (§7#1 verbatim).
  expect_identical(st3$business_state$has_data, TRUE)
  expect_equal(st3$business_state$n_clusters, 8)

  # §6.1 — all of this WITHOUT any arming: no arm.json was ever written.
  expect_false(file.exists(ts_drive_path("arm.json")))
})

# ── §7 #2 (T4, A1) — zero HTML in state.json; clean active_module_key ────────

test_that("state.json carries a clean active_module_key and ZERO HTML anywhere (contract §7#2, A1/T4)", {
  sb <- .dstv1_sandbox("html")
  on.exit(unlink(sb, recursive = TRUE, force = TRUE), add = TRUE)

  # (a) The fiche's A1 literal example: the SC module active ⇒ key "sc".
  token_a <- "tok_dstv1_html_sc"
  w <- ts_drive_emit_passive_state(
    token = token_a, input = list(main_nav = "tab_sc"), force = TRUE)
  expect_true(w)
  st_a <- ts_drive_read_state()
  expect_false(is.null(st_a))
  expect_identical(st_a$active_module_key, "sc")
  expect_equal(length(.dstv1_html_leaves(st_a)), 0L)

  # (b) The measured T4 leak verbatim (fiche :43): the i18n-rendered nav label
  # '<span class="i18n" data-key="Single-Cell">Single-Cell</span>' reaching the
  # producer. TODAY it lands raw in state.json$active_module — the contract
  # forbids any HTML in ANY machine field of the emitted document.
  token_b <- "tok_dstv1_html_leak"
  w <- ts_drive_emit_passive_state(
    token = token_b,
    input = list(main_nav = '<span class="i18n" data-key="Single-Cell">Single-Cell</span>'),
    force = TRUE)
  expect_true(w)
  st_b <- ts_drive_read_state()
  expect_false(is.null(st_b))
  html_hits <- .dstv1_html_leaves(st_b)
  expect_equal(length(html_hits), 0L)  # ← fails today: active_module carries the raw span
  # The clean key exists, is non-empty and itself carries no HTML. The label,
  # where the UI needs it, stays OUT of the machine document (§8.1 decision).
  expect_false(is.null(st_b$active_module_key))
  expect_true(nzchar(as.character(st_b$active_module_key)))
  expect_equal(length(.dstv1_html_leaves(list(key = st_b$active_module_key))), 0L)
  expect_equal(length(.dstv1_html_leaves(list(mod = st_b$active_module))), 0L)
})

# ── §7 #3 — hash-gating preserved AND effective on the new fields ────────────

test_that("hash-gating: unchanged state does NOT rewrite; a business_state change DOES (contract §7#3, digest covers new fields)", {
  sb <- .dstv1_sandbox("hashgate")
  on.exit(unlink(sb, recursive = TRUE, force = TRUE), add = TRUE)

  token <- "tok_dstv1_hashgate_003"
  gd <- .dstv1_manual_gd(status = "running", has_data = TRUE,
                         object_class = "Seurat", n_elements = 1000,
                         n_clusters = 8, deconv_done = FALSE)

  w1 <- ts_drive_emit_passive_state(token = token, global_data = gd)
  expect_true(w1)
  state_path <- ts_drive_path("state.json")
  expect_true(file.exists(state_path))
  mtime1 <- file.info(state_path)$mtime
  hash1 <- ts_drive_read_state()$content_hash

  # Unchanged tick: skipped by diff-gating (O11 discipline, unchanged).
  Sys.sleep(0.05)
  w2 <- ts_drive_emit_passive_state(token = token, global_data = gd)
  expect_false(w2)
  expect_identical(file.info(state_path)$mtime, mtime1)

  # A business_state field changes — the contract's §3.1 consequence: the
  # digest MUST cover the new fields, otherwise a business_state change would
  # never emit and hash-gating would be falsified for DSTV-1's own content.
  gd$business_state$n_clusters <- 12
  Sys.sleep(0.05)
  w3 <- ts_drive_emit_passive_state(token = token, global_data = gd)
  expect_true(w3)                      # ← fails today: global_data is ignored
  st3 <- ts_drive_read_state()
  expect_false(identical(st3$content_hash, hash1))
  expect_equal(st3$business_state$n_clusters, 12)
  expect_false(identical(file.info(state_path)$mtime, mtime1))
})

# ── §7 #4 — completion_verified absent or FALSE, never TRUE (O11 caveat) ─────

test_that("completion_verified stays FALSE throughout every emitted document (contract §7#4)", {
  sb <- .dstv1_sandbox("compl")
  on.exit(unlink(sb, recursive = TRUE, force = TRUE), add = TRUE)

  # Across the whole manual lifecycle — including the "done" DECLARATION, which
  # must never become a proof of completion (§6.2, non-negotiable caveat).
  token <- "tok_dstv1_compl_004"
  for (st in c("running", "step", "done", "error")) {
    gd <- .dstv1_manual_gd(status = st, step = if (st == "step") "0_autopipeline" else NULL,
                           has_data = TRUE, object_class = "Seurat",
                           n_elements = 10, n_clusters = 2, deconv_done = TRUE)
    ts_drive_emit_passive_state(token = token, global_data = gd, force = TRUE)
    doc <- ts_drive_read_state()
    expect_false(is.null(doc))
    viol <- .dstv1_completion_violations(doc)
    expect_equal(length(viol), 0L,
                 info = sprintf("status=%s violations: %s", st, paste(viol, collapse = "; ")))
  }
})

# ── §7 #5 — additivity: frozen fields, frozen vocabulary, frozen tool set ────

test_that("additivity: existing state.json fields keep schema+semantics and the vocabulary only grows (contract §7#5, §6.5)", {
  sb <- .dstv1_sandbox("additive")
  on.exit(unlink(sb, recursive = TRUE, force = TRUE), add = TRUE)

  # No manual feed, no drive activity ⇒ the EXISTING document, semantically
  # stable (frozen fields, drive_watcher.R:3739-3748; frozen vocabulary,
  # test-drive-passive-state.R:66).
  token <- "tok_dstv1_additive_005"
  w <- ts_drive_emit_passive_state(token = token, force = TRUE)
  expect_true(w)
  st <- ts_drive_read_state()
  expect_false(is.null(st))
  for (f in c("session_token", "content_hash", "state_hash", "timestamp",
              "active_module", "pipeline_status", "errors", "recent_errors")) {
    expect_true(f %in% names(st), info = sprintf("frozen field missing: %s", f))
  }
  expect_identical(st$session_token, token)
  expect_true(nzchar(st$content_hash))
  expect_true(st$pipeline_status %in% c("idle", "running", "error"))
  expect_true(is.list(st$errors))
  expect_identical(length(st$errors), 0L)
  # §8.1 decision: business_state ALWAYS present, defaults honest.
  expect_identical(st$business_state$has_data, FALSE)
  expect_identical(st$business_state$deconv_done, FALSE)
  expect_true(.dstv1_emptyish(st$business_state$object_class))
  expect_true(.dstv1_emptyish(st$business_state$n_elements))
  expect_true(.dstv1_emptyish(st$business_state$n_clusters))
  # §8.3: no step declared ⇒ pipeline_step absent/empty.
  expect_true(.dstv1_emptyish(st$pipeline_step))

  # Protocol unchanged (§6 non-goal 2): ts-drive/1.
  expect_identical(TS_DRIVE_PROTOCOL, "ts-drive/1")
})

test_that("additivity: the MCP tool set stays EXACTLY the frozen set (§6 non-goal 3 — no tracker tool)", {
  e <- .dstv1_mcp_env("tools")
  on.exit(unlink(attr(e, "sandbox_root"), recursive = TRUE, force = TRUE), add = TRUE)

  tools <- e$.ts_tools()
  tool_names <- sort(vapply(tools, function(x) x$name, character(1)))
  # Measured 2026-10-31 on the unmodified tree (mcp_server.R:3222-3616).
  # Re-basage 11 -> 12, PoC dataset directory P2 (2026-11-03, fiche
  # PROPOSAL_DATASET_DIRECTORY_POC_2026-11-02.md §3.4) : le 12e outil,
  # transcripto_drive_datasets, est PASSIF et LECTURE SEULE (listing du
  # répertoire de datasets) — l'additivité du tracker n'est pas touchée.
  expect_setequal(
    tool_names,
    c("transcripto_drive_export", "transcripto_drive_read",
      "transcripto_drive_import", "transcripto_drive_status",
      "transcripto_drive_read_result", "transcripto_drive_snapshot",
      "transcripto_drive_set_inputs", "transcripto_drive_run",
      "transcripto_drive_wait", "transcripto_drive_set_armed",
      "transcripto_drive_state", "transcripto_drive_datasets")
  )
  expect_identical(length(tool_names), 12L)
})

# ── §3.3 / §8.2 — O18 recency unchanged; manual status fills the idle gap ────

test_that("manual 'done'/'error' declarations publish, but a recent ring error still outranks (O18 window unchanged, contract §3.3)", {
  sb <- .dstv1_sandbox("recency")
  on.exit(unlink(sb, recursive = TRUE, force = TRUE), add = TRUE)

  token <- "tok_dstv1_recency_006"
  gd <- .dstv1_manual_gd(status = "done", has_data = TRUE, object_class = "Seurat",
                         n_elements = 5, n_clusters = 1, deconv_done = TRUE)

  # Manual "done" with a clean drive layer ⇒ the declaration publishes.
  w <- ts_drive_emit_passive_state(token = token, global_data = gd)
  expect_true(w)
  st <- ts_drive_read_state()
  expect_identical(st$pipeline_status, "done")   # ← fails today: idle

  # A recent ring error keeps O18's word: status "error" outranks the module's
  # self-declared "done" — DSTV-1 adds to the detection, never replaces it.
  # (Payload kept ASCII: the O11 read-back verify had a PRE-EXISTING
  # encoding-mark false-negative on non-ASCII payloads under testthat's UTF-8
  # parse — measured on the clean tree at HEAD 8a7476d,
  # test-drive-passive-state.R:84 — fixed in this GREEN step by the byte-level
  # read-back compare in ts_drive_write_attempt. ASCII here keeps this file's
  # focus on O18 precedence, not on that write-contract bug.)
  ts_drive_record_error("manual failure declared in the ring", module = "sc_pipeline")
  w <- ts_drive_emit_passive_state(token = token, global_data = gd)
  expect_true(w)
  st <- ts_drive_read_state()
  expect_identical(st$pipeline_status, "error")
  expect_equal(length(st$errors), 1L)

  # Manual "error" declaration with a clean drive layer (new ring cleared):
  # the module's own failure is published as a status, not swallowed.
  ts_drive_clear_errors()
  gd$manual_pipeline$status <- "error"
  w <- ts_drive_emit_passive_state(token = token, global_data = gd)
  expect_true(w)
  st <- ts_drive_read_state()
  expect_identical(st$pipeline_status, "error")  # ← fails today: idle
})

# ── §7 #6 (A3) — the MCP read-side exposes the new fields ────────────────────

test_that("transcripto_drive_state surfaces active_module_key, pipeline_step and business_state (contract §7#6, A3)", {
  e <- .dstv1_mcp_env("readstate")
  on.exit(unlink(attr(e, "sandbox_root"), recursive = TRUE, force = TRUE), add = TRUE)

  token <- "tok_dstv1_mcp_state_007"
  gd <- .dstv1_manual_gd(status = "step", step = "0_autopipeline", has_data = TRUE,
                         object_class = "Seurat", n_elements = 4321,
                         n_clusters = 7, deconv_done = FALSE)
  expect_true(e$ts_drive_emit_passive_state(token = token, global_data = gd))

  res <- e$.ts_tool_state()
  expect_false(res$isError)
  sc <- res$structuredContent
  expect_identical(sc$session_token, token)
  # The new producer fields flow through the read-only tool unchanged.
  expect_false(is.null(sc$active_module_key))                   # ← fails today: NULL
  expect_identical(sc$pipeline_step, "0_autopipeline")          # ← fails today: NULL
  expect_false(is.null(sc$business_state))                      # ← fails today: NULL
  expect_identical(sc$business_state$has_data, TRUE)
  expect_identical(sc$business_state$object_class, "Seurat")
  expect_equal(sc$business_state$n_elements, 4321)
  expect_equal(sc$business_state$n_clusters, 7)
  expect_identical(sc$business_state$deconv_done, FALSE)
  # Reading never arms: the passive tool writes nothing (O11 contract).
  expect_false(file.exists(e$ts_drive_path("arm.json")))
})

test_that("the snapshot's business_state is enriched from the passive channel, redaction policy unchanged (contract §7#6 / §5, mcp_server.R:1124-1126)", {
  e <- .dstv1_mcp_env("snapshot")
  on.exit(unlink(attr(e, "sandbox_root"), recursive = TRUE, force = TRUE), add = TRUE)

  token <- "tok_dstv1_mcp_snap_008"
  expect_true(.dstv1_write_ready(e, token))      # fresh handshake, still NOT armed
  gd <- .dstv1_manual_gd(status = "running", has_data = TRUE, object_class = "Seurat",
                         n_elements = 999, n_clusters = 11, deconv_done = FALSE)
  expect_true(e$ts_drive_emit_passive_state(token = token, global_data = gd))

  # NO verdict published — the exact measured T3 blindness site: the snapshot
  # used to say "no verdict published yet", has_data false DURING the run.
  res <- e$.ts_tool_snapshot()
  expect_false(res$isError)
  sc <- res$structuredContent
  expect_false(isTRUE(sc$present))
  bs <- sc$business_state
  expect_false(is.null(bs))
  expect_identical(bs$has_data, TRUE)            # ← fails today: absent
  expect_identical(bs$object_class, "Seurat")
  expect_equal(bs$n_elements, 999)
  expect_equal(bs$n_clusters, 11)
  expect_identical(bs$deconv_done, FALSE)
  # The O11 caveat is carried through the read side, untouched.
  expect_identical(bs$completion_verified, FALSE)
  # Redaction policy unchanged (fiche :63 — "déjà redacted par policy"):
  # the closed allowlist projection keeps its frozen identity.
  expect_true(is.list(sc$redaction))
  expect_identical(sc$redaction$policy, "snapshot-allowlist-v1")
  expect_true(is.character(sc$redaction$never_published))
  # And never armed.
  expect_false(file.exists(e$ts_drive_path("arm.json")))
})

# ── §8.6 / O11 §6 — the tools/_drive/README.md state.json row exists ─────────

test_that("tools/_drive/README.md documents state.json in its file table with the app as writer (O11 §6, contract §5)", {
  readme <- file.path(ts_project_root(), "tools", "_drive", "README.md")
  expect_true(file.exists(readme))
  lines <- readLines(readme, warn = FALSE)
  # Measured ABSENT at DECISIONS.md:105 (grep 0 / exit 1) — DSTV-1 adds the row.
  rows <- lines[grepl("^\\| `state\\.json`", lines)]
  expect_equal(length(rows), 1L,
               info = paste0("state.json rows found: ", paste(rows, collapse = " | ")))
  if (length(rows) == 1L) {
    # O11 §6: "table des fichiers (+ state.json, écrivain = app, sémantique)".
    expect_true(grepl("the app", rows[1]),
                info = paste0("the row must name the app as writer: ", rows[1]))
  }
})

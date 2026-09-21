# =============================================================================
# R/core/drive_allowlist.R — Live control protocol: frozen input allowlist
# =============================================================================
# Dev-only sibling of docs/DRIVE_LIVE_CONTROL_PLAN.md (BUILD THIS spec).
# Role: describe, AS DATA, which namespaced Shiny input ids the file-drop
# poller (R/core/drive_watcher.R) is allowed to inject, and with which widget
# semantics. Pure data + pure helpers: NO Shiny call in this file (C2).
#
# WHY DATA AND NOT COMMENTS — spec S4: "Freeze the allowlist in
# R/core/drive_allowlist.R as data, not comments." An allowlist written in
# prose cannot be fed to the validator, so it would only be a wish. Here every
# accepted id is an entry; anything absent is rejected with an error item and
# the session survives (spec S3).
#
# ── MEASURED IDS (G0 discovery, mandatory) ───────────────────────────────────
# Discovered by reading the module sources, never guessed:
#
#   modules/import/mod_import_bulk.R
#     mod_import_bulk_server("import_bulk", global_data)  -> app.R:528
#     -> prefix "import_bulk-"
#   modules/bulk/mod_bulk.R
#     mod_bulk_server("bulk", global_data)                -> app.R:550
#     -> prefix "bulk-"
#   modules/bulk_de/*.R (ONE moduleServer, see modules/bulk_de/mod_bulk_de.R)
#     mod_bulk_de_server("de", global_data, shared_rv)    -> modules/bulk/mod_bulk.R:534
#     -> prefix "bulk-de-"   (nested: bulk + de)
#   modules/bulk/mod_bulk_pathways.R
#     mod_bulk_pathways_server("pathways", global_data, shared_rv)
#     -> modules/bulk/mod_bulk.R:535
#     -> prefix "bulk-pathways-"
#
# ── WIDGET SEMANTICS (proposal contract §... / spec S5) ──────────────────────
#   "select"   -> the VALUE of the choices vector, never the translated label
#                 (C13: a named `choices` displays the name but Shiny returns
#                 the value).
#   "numeric"  -> numeric scalar.
#   "checkbox" -> logical scalar.
#   "text"     -> character scalar.
#   "button"   -> an INTEGER COUNTER. Matched by ts_drive_bind_button() in
#                 modules/ (spec S5: updateActionButton() does NOT click).
#   "nav"      -> a tabset value, applied through bslib::nav_select().
#   "nav_top"  -> a page_navbar value (the root navbar is un-namespaced).
#
# Out of scope (spec S3 + §9): `sc` and `spatial` keys are NOT here and are
# rejected as `invalid` in v1.
# =============================================================================

# --- Protocol constants (frozen) ---------------------------------------------

#' Wire protocol version. A scenario/arm file carrying another value is
#' ignored (never applied), so a stale helper cannot drive a newer session.
TS_DRIVE_PROTOCOL <- "ts-drive/1"

#' Directory holding the runtime IPC files, relative to the APP ROOT captured
#' at boot — never getwd() (RStudio drifts). See ts_drive_root().
TS_DRIVE_DIR <- file.path("tools", "_drive")

#' Root level navbar: page_navbar(id = "main_nav") in app.R. Not namespaced,
#' so nav_select() is called without a module prefix.
TS_DRIVE_TOP_NAV_ID <- "main_nav"

#' Bulk module navbar + accordion ids (modules/bulk/mod_bulk.R).
#'   navset_card_underline(id = ns("main_tabs")) -> "bulk-main_tabs"
#'   accordion(id = ns("acc_bulk"))              -> "bulk-acc_bulk"
TS_DRIVE_BULK_TABS_ID     <- "bulk-main_tabs"
TS_DRIVE_BULK_ACCORDION_ID <- "bulk-acc_bulk"

#' Watchable bulk result tabs (values of bulk-main_tabs), measured in
#' modules/bulk/mod_bulk.R. Used by `nav_select` when module = "bulk".
TS_DRIVE_BULK_TABS <- c(
  "tab_pca", "tab_qc", "tab_batch_qc", "tab_volcano", "tab_ma", "tab_heatmap",
  "tab_table", "tab_updown", "tab_multimethod", "tab_venn", "tab_pathway",
  "tab_signatures", "tab_wgcna", "tab_pattern", "tab_dose", "tab_survival"
)

#' Watchable bulk sidebar accordion panels (values of bulk-acc_bulk).
TS_DRIVE_BULK_PANELS <- c(
  "0_autopipeline", "panel_mapping", "panel_filter", "panel_de",
  "panel_pathways", "panel_signatures", "panel_wgcna", "panel_survival",
  "panel_pattern", "panel_dose", "panel_report", "panel_datasets",
  "panel_merge", "panel_network"
)

# --- The allowlist (frozen data) --------------------------------------------
#
# `kind` is the ONLY thing the injector switches on — it decides which
# allowlisted adapter may touch the value (spec S2: no eval, no parse()).
# `module` is the scenario module the id belongs to, written EXPLICITLY rather
# than re-derived from the string: the ids are `<module ns>-<inputId>` and the
# nesting means `bulk-de-*` and `bulk-pathways-*` cannot be split reliably by
# a dash heuristic. Data beats inference here, because a wrong module would let
# a scenario read/write a control in the wrong panel.
# `note` records the measured UI site so a future reader can re-verify it
# instead of guessing.

TS_DRIVE_ALLOWLIST <- list(

  # ── Import Bulk (prefix "import_bulk-") ────────────────────────────────────
  # NO import is performed here: the poller only sets inputs + fires the
  # bound `btn_load` token. The fileInput widgets (`counts_file`,
  # `metadata_file`, `ps_files`) are deliberately ABSENT: spec S5 forbids
  # faking the widget, and G3 bypasses it through the import helper instead.
  "import_bulk-bulk_import_mode"  = list(kind = "radio", module = "import_bulk", note = "merged_matrix | per_sample (mod_import_bulk.R:84)"),
  "import_bulk-counts_format"     = list(kind = "radio", module = "import_bulk", note = "rows | cols (mod_import_bulk.R:110)"),
  "import_bulk-counts_has_header" = list(kind = "checkbox", module = "import_bulk", note = "mod_import_bulk.R:115"),
  "import_bulk-counts_has_rownames" = list(kind = "checkbox", module = "import_bulk", note = "mod_import_bulk.R:116"),
  "import_bulk-metadata_has_header" = list(kind = "checkbox", module = "import_bulk", note = "mod_import_bulk.R:143"),
  "import_bulk-metadata_has_rownames" = list(kind = "checkbox", module = "import_bulk", note = "mod_import_bulk.R:144"),
  "import_bulk-infer_delimiter"   = list(kind = "text", module = "import_bulk", note = "regex delimiter (mod_import_bulk.R:164)"),
  "import_bulk-project_name"      = list(kind = "text", module = "import_bulk", note = "mod_import_bulk.R:182"),
  "import_bulk-multi_label"       = list(kind = "text", module = "import_bulk", note = "MD-1 optional label (mod_import_bulk.R:188)"),
  "import_bulk-min_counts"        = list(kind = "numeric", module = "import_bulk", note = "pre-filter (mod_import_bulk.R:194)"),
  "import_bulk-ps_dup_threshold"  = list(kind = "numeric", module = "import_bulk", note = "mod_import_bulk.R:215"),
  "import_bulk-ps_fill_zero"      = list(kind = "checkbox", module = "import_bulk", note = "mod_import_bulk.R:232"),

  # ── Bulk DE (prefix "bulk-de-", ONE moduleServer in mod_bulk_de.R) ─────────
  # The six .de_*_server() are PLAIN FUNCTIONS called inside the same
  # moduleServer(id = "de"), so they share ONE namespace: bulk-de-*.
  "bulk-de-condition_col"     = list(kind = "select", module = "bulk_de", note = "mod_bulk_de_ui.R:23"),
  "bulk-de-covariates"        = list(kind = "select", module = "bulk_de", note = "multi (mod_bulk_de_ui.R:24)"),
  "bulk-de-group_ref"         = list(kind = "select", module = "bulk_de", note = "mod_bulk_de_ui.R:32"),
  "bulk-de-group_target"      = list(kind = "select", module = "bulk_de", note = "mod_bulk_de_ui.R:33"),
  # MEASURED values (mod_bulk_de_engine.R:117): the choices are built from
  # requireNamespace(), so the values are the LOWERCASE engine keys, never the
  # display labels — "deseq2", "edger", "limma" (DESeq2 yields only when
  # installed; `eng[1]` is selected by default).
  "bulk-de-de_engine"         = list(kind = "select", module = "bulk_de", note = "deseq2 | edger | limma (mod_bulk_de_engine.R:110)"),
  "bulk-de-shrink_lfc"        = list(kind = "checkbox", module = "bulk_de", note = "mod_bulk_de_ui.R:39"),
  "bulk-de-lfc_thresh"        = list(kind = "numeric", module = "bulk_de", note = "mod_bulk_de_ui.R:49"),
  "bulk-de-padj_thresh"       = list(kind = "numeric", module = "bulk_de", note = "mod_bulk_de_ui.R:50"),
  "bulk-de-heatmap_top_n"     = list(kind = "numeric", module = "bulk_de", note = "mod_bulk_de_ui.R:172"),

  # ── Bulk pathways (prefix "bulk-pathways-") ────────────────────────────────
  "bulk-pathways-enrich_mode"     = list(kind = "radio", module = "bulk_pathways", note = "ora | gsea (mod_bulk_pathways.R:6)"),
  "bulk-pathways-pathway_source"  = list(kind = "select", module = "bulk_pathways", note = "up | down | all_sig | manual (mod_bulk_pathways.R:16)"),
  "bulk-pathways-pathway_db"      = list(kind = "select", module = "bulk_pathways", note = "GOBP | KEGG | Reactome (mod_bulk_pathways.R:27)"),
  "bulk-pathways-pathway_org"     = list(kind = "select", module = "bulk_pathways", note = "human | mouse (mod_bulk_pathways.R:31)"),
  "bulk-pathways-pathway_pval"    = list(kind = "numeric", module = "bulk_pathways", note = "mod_bulk_pathways.R:37"),
  "bulk-pathways-scores_source"   = list(kind = "select", module = "bulk_pathways", note = "bulk_gene_set_choices() (mod_bulk_pathways.R:66)"),
  "bulk-pathways-scores_org"      = list(kind = "select", module = "bulk_pathways", note = "human | mouse (mod_bulk_pathways.R:69)"),
  "bulk-pathways-scores_method"   = list(kind = "select", module = "bulk_pathways", note = "ssgsea | gsva | plage | zscore"),
  "bulk-pathways-scores_min_size" = list(kind = "numeric", module = "bulk_pathways", note = "mod_bulk_pathways.R:80"),
  "bulk-pathways-scores_max_size" = list(kind = "numeric", module = "bulk_pathways", note = "mod_bulk_pathways.R:82"),

  # ── Action buttons exposed to run_pipeline (integer counters) ─────────────
  # Bound through ts_drive_bind_button(); updateActionButton() does NOT click.
  "import_bulk-btn_load"       = list(kind = "button", module = "import_bulk", note = "import confirm (mod_import_bulk.R:253)"),
  "bulk-de-run_de"             = list(kind = "button", module = "bulk_de", note = "DE single pair (mod_bulk_de_run.R:76)"),
  "bulk-pathways-run_pathway"  = list(kind = "button", module = "bulk_pathways", note = "ORA/GSEA enrichment (mod_bulk_pathways.R:44)"),
  "bulk-pathways-run_scores"   = list(kind = "button", module = "bulk_pathways", note = "GSVA/ssGSEA scores (mod_bulk_pathways.R:89)")
)

#' The ONLY FOUR button ids ts_drive_bind_button() is allowed to instrument.
#' Spec G2 says "the three bulk observeEvents" — measured, the bulk pipeline
#' the G3 acceptance runs (import -> DE) plus the pathways panel touch FOUR
#' click sites. The `run_scores` (GSVA per-sample) button is the fourth; it is
#' kept here because the pathway panel's scoring branch is part of the frozen
#' bulk pilot allowlist. Any further bind is visible in review.
TS_DRIVE_BUTTONS <- c(
  "import_bulk-btn_load",
  "bulk-de-run_de",
  "bulk-pathways-run_pathway",
  "bulk-pathways-run_scores"
)

#' The three modules the v1 allowlist covers. Anything else is `invalid`.
TS_DRIVE_MODULES <- c("import_bulk", "bulk_de", "bulk_pathways")

#' Widget kinds the injector knows how to adapt (spec S5).
#'
#' `nav` / `nav_top` are kept in the vocabulary even though no input id uses
#' them: tab switching is a NAVIGATION EFFECT (`ts_drive_nav_plan()`), never an
#' injected input. The top-level navbar is un-namespaced, so a `nav_top` entry
#' would have no namespaced id to live under — `bulk-active_tab` was removed
#' for exactly that reason (the consistency check below refused it).
TS_DRIVE_KINDS <- c("select", "radio", "checkbox", "numeric", "text",
                    "button", "nav", "nav_top")

# --- Pure helpers (no Shiny) -------------------------------------------------

#' Look up one allowlist entry.
#'
#' @param input_id Namespaced input id (`module-inputId`).
#' @return The entry list (`kind`, `module`, `note`) or NULL when not
#'   allowlisted.
ts_drive_allowlist_get <- function(input_id) {
  if (!is.character(input_id) || length(input_id) != 1L || is.na(input_id)) {
    return(NULL)
  }
  TS_DRIVE_ALLOWLIST[[input_id]]
}

#' Refuse an allowlist that contradicts itself.
#'
#' The scenario validator routes on `entry$module`; an entry whose declared
#' module is not one of `TS_DRIVE_MODULES` would be unreachable, and an entry
#' named `sc-*` would silently widen the v1 pilot beyond what the spec froze.
#' Both are cheap to check and expensive to discover at runtime, so the file
#' fails loudly at source() time instead.
#'
#' `allowlist` / `buttons` are injectable so the check is FALSIFIABLE: a checker
#' that can only ever be handed the real, valid data is indistinguishable from
#' `function() TRUE`. `test-drive-watcher.R` feeds it deliberately broken
#' tables and asserts that each defect is named.
#'
#' @param allowlist The table to check. Defaults to the shipped one.
#' @param buttons The bound-button vector to check.
#' @return TRUE when consistent; otherwise a character vector of problems.
ts_drive_allowlist_problems <- function(allowlist = TS_DRIVE_ALLOWLIST,
                                        buttons = TS_DRIVE_BUTTONS) {
  problems <- character(0)
  if (!length(allowlist)) return("allowlist is empty")

  for (id in names(allowlist)) {
    e <- allowlist[[id]]
    if (!is.list(e) || is.null(e$kind) || is.null(e$module)) {
      problems <- c(problems, sprintf("%s: entry must declare `kind` and `module`", id))
      next
    }
    if (!e$kind %in% TS_DRIVE_KINDS) {
      problems <- c(problems, sprintf("%s: kind '%s' is not in TS_DRIVE_KINDS", id, e$kind))
    }
    if (!e$module %in% TS_DRIVE_MODULES) {
      problems <- c(problems, sprintf("%s: module '%s' is not in TS_DRIVE_MODULES", id, e$module))
    }
    # Spec S4: namespaced ids only, so a bare id can never appear.
    if (!grepl("-", id, fixed = TRUE)) {
      problems <- c(problems, sprintf("%s: not a namespaced id (no dash)", id))
    }
    # Spec S3: sc / spatial are out of scope for v1.
    if (e$module %in% c("sc", "spatial") || grepl("^(sc|spatial)-", id)) {
      problems <- c(problems, sprintf("%s: sc/spatial keys are out of scope for v1", id))
    }
  }

  # Every bound button must be a known, allowlisted button entry.
  for (b in buttons) {
    if (is.null(allowlist[[b]])) {
      problems <- c(problems, sprintf("bound button %s is missing from the allowlist", b))
    } else if (!identical(allowlist[[b]]$kind, "button")) {
      problems <- c(problems, sprintf("bound button %s has kind '%s', expected 'button'",
                                      b, allowlist[[b]]$kind))
    }
  }
  if (length(problems)) problems else TRUE
}

# Fail at source() time, not at the first injection. A silent allowlist
# contradiction would surface as "the agent's scenario did nothing", which is
# the hardest class of bug to attribute.
.ts_drive_allowlist_check <- ts_drive_allowlist_problems()
if (!isTRUE(.ts_drive_allowlist_check)) {
  stop("TS_DRIVE_ALLOWLIST is inconsistent:\n  - ",
       paste(.ts_drive_allowlist_check, collapse = "\n  - "), call. = FALSE)
}

#' Is `input_id` allowlisted for injection?
#'
#' @return TRUE/FALSE, never NA.
ts_drive_allowlisted <- function(input_id) {
  !is.null(ts_drive_allowlist_get(input_id))
}

#' Module OWNING an allowlisted (or candidate) namespaced id.
#'
#' An allowlisted id answers from the DATA (`entry$module`), which is exact.
#' Unknown ids fall back to a lexical split on the LAST dash, used only to make
#' the refusal message name the module the agent appeared to target.
#'
#' @param input_id Namespaced input id.
#' @return The module name, or NA_character_ when it cannot be determined.
ts_drive_module_of <- function(input_id) {
  if (!is.character(input_id) || length(input_id) != 1L || is.na(input_id)) {
    return(NA_character_)
  }
  entry <- TS_DRIVE_ALLOWLIST[[input_id]]
  if (!is.null(entry) && !is.null(entry$module)) return(entry$module)
  .ts_drive_split_module(input_id)
}

#' Lexical fallback used for UNKNOWN ids.
#'
#' Splits on the LAST `-`, because ids are `<module ns>-<inputId>` and both
#' halves may contain dashes. Measured: `bulk-de-run_de` -> `bulk-de`,
#' `bulk-pathways-run_pathway` -> `bulk-pathways`. Splitting on the FIRST dash
#' yields `bulk` for both, which is not a module — that mistake was caught by
#' `test-drive-watcher.R`.
#' @noRd
.ts_drive_split_module <- function(input_id) {
  pos <- gregexpr("-", input_id, fixed = TRUE)[[1]]
  if (length(pos) == 1L && pos[1] < 0L) return(NA_character_)
  substr(input_id, 1L, pos[length(pos)] - 1L)
}

#' Frozen button -> module map, used by run_pipeline routing.
ts_drive_button_module <- function(button_id) {
  if (!button_id %in% TS_DRIVE_BUTTONS) return(NA_character_)
  ts_drive_module_of(button_id)
}


# =============================================================================
# Badge state — the PASSIVE dev-only status model
# =============================================================================
# The badge the agent (and a human) reads must be OBSERVATIONAL. This block
# defines (a) the states, (b) the only transitions allowed to produce them, and
# (c) a field projector that CANNOT leak a secret. All of it is pure: no Shiny
# call, so C2 holds and the whole thing is testable from Rscript.

#' Frozen badge states. UI mapping may change; these strings may not.
TS_DRIVE_BADGE_STATES <- c("off", "armed", "running", "done", "error")

#' The ONLY events allowed to move the badge.
#'
#' Anything not in this set is a Bug, not a feature: the badge must not react to
#' an idle tick, a heartbeat, or a poll — otherwise it would be a second,
#' divergent source of truth about the protocol.
TS_DRIVE_BADGE_EVENTS <- c("arm", "disarm", "accepted", "started",
                           "completed", "error", "session_end")

#' Map a scenario status to a badge state.
#'
#' `invalid` and `ignored` deliberately do NOT raise an error badge: they mean
#' "the payload was refused, nothing ran", which is not a session failure. They
#' leave the badge on `armed` so a refusal cannot look like an app crash.
ts_drive_badge_state_for <- function(status) {
  switch(as.character(status),
         applied = "done",
         running = "running",
         done    = "done",
         error   = "error",
         "armed")
}

#' Derive the badge state from a badge event and the resulting status.
ts_drive_badge_next <- function(event, status = NULL) {
  event <- as.character(event)
  if (!event %in% TS_DRIVE_BADGE_EVENTS) return(NA_character_)
  switch(event,
         arm         = "armed",
         disarm      = "off",
         session_end = "off",
         accepted    = "armed",
         started     = "running",
         completed   = ts_drive_badge_state_for(status),
         error       = "error",
         NA_character_)
}

#' Sanitize a free-form message for display.
#'
#' Defence in depth, NOT decoration. The badge is rendered into the UI and can
#' be read by anyone with the tab open, so an error string that happens to
#' contain a token or a Windows path must not survive. Everything after the
#' first line feed is dropped (R conditions carry multi-line bodies), absolute
#' paths are replaced by their basename, and anything shaped like a path or a
#' 8-char alphanumeric token is redacted.
ts_drive_badge_sanitize <- function(msg, max_chars = 80L) {
  if (is.null(msg) || length(msg) == 0L) return("")
  s <- as.character(msg)[1]
  # `NA` and empty are both reachable: a model field defaults to "" and an R
  # condition may carry NA. `nchar(NA)` is NA, and `if (NA)` is an ERROR, so
  # this guard is load-bearing — found by the badge probe, not by reasoning.
  if (is.na(s) || !nzchar(s)) return("")
  s <- strsplit(s, "\n", fixed = TRUE)[[1]][1]
  if (is.na(s) || !nzchar(s)) return("")
  s <- gsub("\\s+", " ", s)
  # Windows and POSIX absolute paths -> basename only.
  s <- gsub("([A-Za-z]:[\\\\/]|[\\\\/])[^ ]*[\\\\/]([^ \\\\/]+)", "<path>", s, perl = TRUE)
  # A bare 8+-char alphanumeric run is token-shaped (ts_drive_new_token()).
  s <- gsub("\\b[A-Za-z0-9]{8,}\\b", "<redacted>", s, perl = TRUE)
  s <- trimws(s)
  if (is.na(s)) return("")
  if (nchar(s) > max_chars) s <- paste0(substr(s, 1L, max_chars - 1L), "\u2026")
  s
}

#' Project a badge model to the ONLY fields the UI may show.
#'
#' Whitelist, not blacklist. A blacklist ("drop token, drop path") fails open the
#' day someone adds a field; a whitelist fails closed. `token`, `root`, `pid`,
#' `payload` and the snapshot are therefore unreachable from the UI by
#' construction — they are simply not in the returned list.
#'
#' @param model list(state, ack_seq, module, action, elapsed_s, error, armed).
#' @return list(visible, state, ack_seq, module, action, elapsed, error).
ts_drive_badge_view <- function(model) {
  if (is.null(model)) return(list(visible = FALSE))
  state <- as.character(model$state %||% "off")
  if (!state %in% TS_DRIVE_BADGE_STATES || identical(state, "off")) {
    return(list(visible = FALSE))
  }
  el <- suppressWarnings(as.numeric(model$elapsed_s %||% NA_real_))
  list(
    visible = TRUE,
    state   = state,
    ack_seq = suppressWarnings(as.integer(model$ack_seq %||% 0L)),
    module  = ts_drive_badge_sanitize(ts_drive_badge_chr(model$module), 24L),
    action  = ts_drive_badge_sanitize(ts_drive_badge_chr(model$action), 24L),
    elapsed = if (length(el) != 1L || is.na(el) || el < 0) "" else sprintf("%.1fs", el),
    error   = ts_drive_badge_sanitize(ts_drive_badge_chr(model$error), 80L)
  )
}

#' Coerce a badge label to a length-1 character, never `NA`.
#'
#' `%||%` only catches `NULL`, so an explicit `NA_character_` (which the tick
#' legitimately produces for `active_module` when a scenario names no module)
#' would reach the sanitizer. Normalising here keeps every caller simple.
#' @noRd
ts_drive_badge_chr <- function(x) {
  if (is.null(x) || length(x) == 0L) return("")
  x <- as.character(x)[1]
  if (is.na(x)) "" else x
}

#' A fresh badge model. Kept here (pure data) so app.R only mutates it.
ts_drive_badge_model <- function() {
  list(state = "off", ack_seq = 0L, module = "", action = "",
       elapsed_s = NA_real_, error = "", armed = FALSE)
}

#' Build the badge UI. Returns `NULL` — i.e. NO element at all — when hidden.
#'
#' Pure `htmltools`, no Shiny call, so it is safe in `R/` and testable.
#' `htmltools::tags` is already a dependency (bslib pulls it in), so this adds
#' no package to the lockfile.
#'
#' The badge is intentionally minimal so the app is indistinguishable from
#' before when the protocol is off (spec: "Do not add a modal, blocking overlay,
#' browser automation, or a permanent notification").
ts_drive_badge_ui <- function(view) {
  if (is.null(view) || !isTRUE(view$visible)) return(NULL)
  label <- switch(view$state,
                  armed   = "drive: armed",
                  running = "drive: running",
                  done    = "drive: done",
                  error   = "drive: error",
                  "drive")
  bits <- label
  if (!is.null(view$ack_seq) && !is.na(view$ack_seq) && view$ack_seq > 0L) {
    bits <- paste0(bits, " \u00b7 seq ", view$ack_seq)
  }
  for (extra in c(view$module, view$action, view$elapsed, view$error)) {
    if (!is.null(extra) && length(extra) == 1L && !is.na(extra) && nzchar(extra)) {
      bits <- paste0(bits, " \u00b7 ", extra)
    }
  }
  htmltools::span(
    id = "ts_drive_badge",
    class = paste0("ts-drive-badge ts-drive-badge-", view$state),
    style = paste(
      "position:fixed; bottom:10px; right:12px; z-index:2000;",
      "font-size:11px; line-height:1.4; padding:3px 8px; border-radius:10px;",
      "background:rgba(33,37,41,.82); color:#f8f9fa;",
      "font-family:ui-monospace,SFMono-Regular,Menlo,monospace;",
      "pointer-events:none; user-select:none; max-width:60vw;",
      "overflow:hidden; text-overflow:ellipsis; white-space:nowrap;"
    ),
    bits
  )
}

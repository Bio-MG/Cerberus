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

  # ── Bulk filter — Step 1 (prefix "bulk-filter-") ───────────────────────────
  # The STAGED workflow's first real stage, and the only producer of
  # `shared_rv$filtered_counts` a scenario can reach:
  #   import_file -> global_data$bulk_obj -> Step 1 Filtering & VST
  #     -> shared_rv$filtered_counts / $dds_blind / $vst_mat
  #     -> design & contrasts -> DE -> pathways
  # Every downstream panel gates on that slot, so without Step 1 the DE and
  # pathways buttons are bound, correctly guarded, and UNREACHABLE. MEASURED on
  # a live session before this entry existed: `run_pipeline` on `bulk_de`
  # answered `invalid — not ready: no bulk object loaded
  # (shared_rv$filtered_counts is NULL)` while `snapshot` answered
  # `has_data=TRUE genes=17925 samples=18` in the SAME instant. Both true: they
  # name different slots.
  #
  # ⚠️ The prefix is `bulk-filter-`, NOT `bulk-`: the filter is a NESTED module
  # (`mod_bulk.R:34` `mod_bulk_filter_ui(ns("filter"))`, `mod_bulk.R:533`
  # `mod_bulk_filter_server("filter", ...)`, both inside the `bulk` module).
  # MEASURED from the RUNNING application — a real client was connected and the
  # DOCUMENT was queried for `*[id]`:
  #   bulk-filter-run_filter_norm -> BUTTON "Lancer Filtrage & VST"
  # The obvious source-only reading, `bulk-run_filter_norm`, is ABSENT from the
  # document. Keying on it would have produced a binding that never fires and
  # never errors. Evidence: .workbuddy-ai/tmp/evidence/G3c_dom_probe.txt
  #
  # The auto-pipeline is deliberately NOT here: it is a second route to the
  # same state, and allowing it would let a scenario skip the staged sequence
  # this milestone exists to make drivable. A test asserts its absence.
  "bulk-filter-min_count"            = list(kind = "numeric", module = "bulk_filter", note = "mod_bulk_filter.R:53, default 10"),
  "bulk-filter-min_samples"          = list(kind = "numeric", module = "bulk_filter", note = "mod_bulk_filter.R:54, default 1"),
  "bulk-filter-min_count_per_sample" = list(kind = "numeric", module = "bulk_filter", note = "mod_bulk_filter.R:55, default 1"),

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
  "bulk-filter-run_filter_norm" = list(kind = "button", module = "bulk_filter", note = "Step 1 Filtering & VST (mod_bulk_filter.R:59, DOM-measured)"),
  "bulk-de-run_de"             = list(kind = "button", module = "bulk_de", note = "DE single pair (mod_bulk_de_run.R:76)"),
  "bulk-pathways-run_pathway"  = list(kind = "button", module = "bulk_pathways", note = "ORA/GSEA enrichment (mod_bulk_pathways.R:44)"),
  "bulk-pathways-run_scores"   = list(kind = "button", module = "bulk_pathways", note = "GSVA/ssGSEA scores (mod_bulk_pathways.R:89)")
)

#' The ONLY FIVE button ids ts_drive_bind_button() is allowed to instrument.
#' Spec G2 says "the three bulk observeEvents" — measured, the bulk pipeline
#' the G3 acceptance runs touches FIVE click sites. The `run_scores` (GSVA
#' per-sample) button is the fourth, and Step 1 (`run_filter_norm`) is the
#' fifth — added by the second G3 milestone because it is the ONLY producer of
#' `shared_rv$filtered_counts`, and therefore the only way any downstream
#' action can ever be reached. Any further bind is visible in review.
TS_DRIVE_BUTTONS <- c(
  "import_bulk-btn_load",
  "bulk-filter-run_filter_norm",
  "bulk-de-run_de",
  "bulk-pathways-run_pathway",
  "bulk-pathways-run_scores"
)

#' The four modules the v1 allowlist covers. Anything else is `invalid`.
#' `bulk_filter` is the nested Step 1 module (`bulk-filter-`), named after the
#' same convention as `bulk_de` / `bulk_pathways`: the DOM prefix with the dash
#' turned into an underscore.
TS_DRIVE_MODULES <- c("import_bulk", "bulk_filter", "bulk_de", "bulk_pathways")

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
# import_file — the G3 import contract (frozen data + pure validation)
# =============================================================================
# Spec G3 / S5 / S11. `import_file` BYPASSES the `fileInput` widget: the agent
# names a PATH, and the module performs the load through the same helper the UI
# uses. Two things therefore have to be frozen as data:
#
#   1. the KEY SET of the `import` payload — a whitelist, exactly like `inputs`,
#      so a field the injector never reads can never reach it;
#   2. the ROOTS a path may come from — spec S11: "Do not execute
#      user-supplied file paths outside the project, `tempdir()`, or an
#      explicit allowlisted data dir. Reject `..`".
#
# Everything here is pure: no Shiny call (C2), and the only I/O is
# `file.exists()` / `dir.exists()`, so the whole rule is testable from Rscript
# without a session.

#' Keys the `import` block may carry (frozen).
TS_DRIVE_IMPORT_KEYS <- c("counts_path", "metadata_path", "mode")

#' Import modes the loader understands — measured from the `bulk_import_mode`
#' radio in modules/import/mod_import_bulk.R, not invented.
TS_DRIVE_IMPORT_MODES <- c("merged_matrix", "per_sample")

#' Registry-key prefix for a published IMPORTER.
#'
#' A published importer is NOT a button token and must never be mistaken for
#' one. `effects(mode = "tokens")` lists the registry by filtering `ls(reg)`
#' through `ts_drive_module_of()`, and this prefix is chosen so that filter
#' DROPS it: `tsdrive-importer-import_bulk` splits on its last dash to
#' `tsdrive-importer-import`, which is no module. Without that, `run_pipeline`
#' would read a published importer as a bound button and report `done` for a
#' click that never happened — the exact `done` lie the readiness gate exists to
#' remove. Pinned by `test-drive-watcher.R` §16.
TS_DRIVE_IMPORTER_PREFIX <- "tsdrive-importer-"

#' Extra roots an OPERATOR may allowlist without editing this file.
#'
#' Read from the environment, deliberately NOT from the scenario: an agent that
#' could widen its own roots would make spec S11 decorative. Setting an env var
#' requires control of the process that launches the app, which the agent does
#' not have on a session a human opened.
TS_DRIVE_IMPORT_EXTRA_ROOTS <- character(0)

#' Roots an import path may be read from (spec S11).
#'
#' The app root is the project itself; `tempdir()` is where a test — and any
#' agent-side staging — writes; `TRANSCRIPTO_DRIVE_DATA_DIR` (path-separator
#' separated) is the operator's data directory; `extra` is for programmatic
#' callers and tests.
ts_drive_import_roots <- function(root = ts_drive_root(), extra = NULL) {
  env <- Sys.getenv("TRANSCRIPTO_DRIVE_DATA_DIR", "")
  env <- if (nzchar(env)) {
    strsplit(env, .Platform$path.sep, fixed = TRUE)[[1]]
  } else {
    character(0)
  }
  unique(c(root, tempdir(), TS_DRIVE_IMPORT_EXTRA_ROOTS, env, extra))
}

#' Validate ONE path an agent asked to import (spec S11).
#'
#' Returns a VERDICT, not a boolean, because a refusal has to be actionable: an
#' agent told only `FALSE` will retry the same path forever.
#'
#' @param path The candidate path (one string).
#' @param roots Allowlisted roots. Defaults to `ts_drive_import_roots()`.
#' @return list(ok = logical, path = normalised-or-NULL, reason = character-or-NULL)
ts_drive_validate_import_path <- function(path, roots = ts_drive_import_roots()) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    return(list(ok = FALSE, path = NULL,
                reason = "the path must be one non-empty string"))
  }

  # `..` is refused on the RAW string, BEFORE any normalisation: normalising
  # first would resolve the traversal away and hide the attempt, so a refusal
  # would depend on where the path happened to land.
  parts <- strsplit(path, "[/\\\\]+")[[1]]
  if (any(parts == "..")) {
    return(list(ok = FALSE, path = NULL,
                reason = "the path contains a `..` component"))
  }

  full <- tryCatch(normalizePath(path, winslash = "/", mustWork = FALSE),
                   error = function(e) NULL)
  if (is.null(full) || is.na(full)) {
    return(list(ok = FALSE, path = NULL, reason = "the path could not be resolved"))
  }

  roots <- tryCatch(normalizePath(roots, winslash = "/", mustWork = FALSE),
                    error = function(e) character(0))
  roots <- roots[!is.na(roots) & nzchar(roots)]
  inside <- any(vapply(roots, function(r)
    identical(substr(full, 1L, nchar(r)), r), logical(1)))
  if (!inside) {
    return(list(ok = FALSE, path = NULL,
                reason = sprintf("the path is outside every allowlisted root (%s)",
                                 paste(basename(roots), collapse = ", "))))
  }

  if (!file.exists(full)) {
    return(list(ok = FALSE, path = NULL, reason = "the path does not exist"))
  }
  if (dir.exists(full)) {
    # MEASURED, not hypothetical: the dataset this grade was handed over as is
    # `GSE164073_Eye_count_matrix.csv/` — a DIRECTORY holding a file of the same
    # name. Naming that is the difference between one retry and a mystery.
    return(list(ok = FALSE, path = NULL,
                reason = "the path is a directory; pass the counts FILE inside it"))
  }
  list(ok = TRUE, path = full, reason = NULL)
}

#' Validate the `import` block of a scenario.
#'
#' @param block The parsed `import` object, or NULL.
#' @return list(ok, errors, import) — `import` is the WHITELISTED block.
ts_drive_validate_import <- function(block, roots = ts_drive_import_roots()) {
  if (is.null(block) || !is.list(block) || !length(block)) {
    return(list(ok = FALSE,
                errors = "`import` must be a JSON object carrying at least `counts_path`",
                import = NULL))
  }
  errors <- character(0)
  unknown <- setdiff(names(block), TS_DRIVE_IMPORT_KEYS)
  if (length(unknown)) {
    errors <- c(errors, sprintf("unknown key(s) in `import`: %s",
                                paste(unknown, collapse = ", ")))
  }

  out <- list()
  cp <- ts_drive_validate_import_path(block$counts_path, roots = roots)
  if (!isTRUE(cp$ok)) errors <- c(errors, sprintf("`counts_path`: %s", cp$reason))
  else out$counts_path <- cp$path

  if (!is.null(block$metadata_path)) {
    mp <- ts_drive_validate_import_path(block$metadata_path, roots = roots)
    if (!isTRUE(mp$ok)) errors <- c(errors, sprintf("`metadata_path`: %s", mp$reason))
    else out$metadata_path <- mp$path
  }

  if (!is.null(block$mode)) {
    m <- as.character(block$mode)
    if (length(m) != 1L || is.na(m) || !m %in% TS_DRIVE_IMPORT_MODES) {
      errors <- c(errors, sprintf("`mode` must be one of: %s",
                                  paste(TS_DRIVE_IMPORT_MODES, collapse = ", ")))
    } else {
      out$mode <- m
    }
  }

  list(ok = !length(errors), errors = errors, import = out)
}

#' Publish a module's IMPORTER so `import_file` can reach it.
#'
#' The counterpart of `ts_drive_publish_token()`, and deliberately a different
#' slot: the importer is a FUNCTION the poller calls with a validated request,
#' not a counter it increments. Stored under `TS_DRIVE_IMPORTER_PREFIX` so the
#' token listing cannot see it.
#'
#' Silently does nothing when no registry is present (a unit test that sources a
#' module alone), so the module keeps working outside the app.
#'
#' @param global_data The app-wide `reactiveValues`.
#' @param module One of `TS_DRIVE_MODULES`.
#' @param importer `function(request)` -> list(ok, status, errors, warnings).
ts_drive_publish_importer <- function(global_data, module, importer) {
  if (!is.character(module) || length(module) != 1L || is.na(module) ||
      !module %in% TS_DRIVE_MODULES) {
    warning(sprintf("ts_drive_publish_importer(): '%s' is not in TS_DRIVE_MODULES — ignored.",
                    module))
    return(invisible(FALSE))
  }
  if (!is.function(importer)) {
    warning("ts_drive_publish_importer(): the importer must be a function — ignored.")
    return(invisible(FALSE))
  }
  reg <- ts_drive_registry(global_data)
  if (is.null(reg)) return(invisible(FALSE))
  reg[[paste0(TS_DRIVE_IMPORTER_PREFIX, module)]] <- importer
  invisible(TRUE)
}

#' Read a module's published importer (used by the poller through `effects`).
#'
#' @return The importer function, or NULL when none was published.
ts_drive_importer_of <- function(global_data, module) {
  reg <- ts_drive_registry(global_data)
  if (is.null(reg)) return(NULL)
  fn <- reg[[paste0(TS_DRIVE_IMPORTER_PREFIX, module)]]
  if (is.function(fn)) fn else NULL
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
ts_drive_badge_sanitize <- function(msg, max_chars = 80L, known = NULL) {
  if (is.null(msg) || length(msg) == 0L) return("")
  s <- as.character(msg)[1]
  # `NA` and empty are both reachable: a model field defaults to "" and an R
  # condition may carry NA. `nchar(NA)` is NA, and `if (NA)` is an ERROR, so
  # this guard is load-bearing — found by the badge probe, not by reasoning.
  if (is.na(s) || !nzchar(s)) return("")
  s <- strsplit(s, "\n", fixed = TRUE)[[1]][1]
  if (is.na(s) || !nzchar(s)) return("")
  # A DECLARED identifier is not free text, and must come back VERBATIM.
  #
  # MEASURED live (2026-09-23, Mode 2 visible run): the badge read
  # `drive: done · seq 2 · bulk_de · <redacted> · 0.1s` for a `snapshot`. The
  # token heuristic below redacts any bare 8+-char alphanumeric run, and
  # "snapshot" is exactly 8 characters — so the badge redacted the very label
  # it exists to show. The blast radius is 5 of the 6 frozen actions
  # (`set_inputs`, `run_pipeline`, `import_file`, `snapshot`, `reset_module`;
  # only `noop` survives) and 3 of the 4 frozen modules (`bulk_filter`,
  # `import_bulk`, `bulk_pathways`; only `bulk_de` survives, at 7 chars).
  #
  # Returning a member of a FROZEN set verbatim is safe by construction: the
  # protocol already validated it against `TS_DRIVE_ACTIONS` / `TS_DRIVE_MODULES`
  # (4 and 6 members), so it cannot carry a token. Everything that is NOT a
  # declared member still goes through every rule below — including the token
  # heuristic, which is where the security property actually lives (free-form
  # `error` text).
  if (length(known) > 0L && s %in% known) return(s)
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
    # `known` names the FROZEN set each label must belong to. A declared member
    # comes back verbatim; anything else is free text and is sanitized in full.
    # `error` deliberately gets NO `known` set: it is the one free-form field,
    # so it keeps every redaction rule — that is where a leaked token would
    # actually travel.
    module  = ts_drive_badge_sanitize(ts_drive_badge_chr(model$module), 24L,
                                      known = TS_DRIVE_MODULES),
    action  = ts_drive_badge_sanitize(ts_drive_badge_chr(model$action), 24L,
                                      known = TS_DRIVE_ACTIONS),
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

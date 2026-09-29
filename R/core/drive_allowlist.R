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
  "tab_table",   "tab_updown", "tab_multimethod", "tab_venn", "tab_pathway",
  "tab_signatures", "tab_wgcna", "tab_pattern", "tab_dose", "tab_survival",
  "tab_bulk_network"
)

#' Watchable bulk sidebar accordion panels (values of bulk-acc_bulk).
TS_DRIVE_BULK_PANELS <- c(
  "0_autopipeline", "panel_mapping", "panel_filter", "panel_de",
  "panel_pathways", "panel_signatures", "panel_wgcna", "panel_survival",
  "panel_pattern", "panel_dose", "panel_report", "panel_datasets",
  "panel_merge", "panel_network"
)

TS_DRIVE_SPATIAL_TOP_TAB      <- "tab_spatial"
TS_DRIVE_SPATIAL_TABS_ID      <- "spatial-results"
TS_DRIVE_SPATIAL_TAB          <- "results_pipeline"
TS_DRIVE_SPATIAL_ACCORDION_ID <- "spatial-steps"
TS_DRIVE_SPATIAL_PANELS       <- "panel_pipeline"

# The QC child module, measured in modules/spatial/mod_spatial_qc.R and
# mod_spatial.R: `mod_spatial_qc_ui(ns("qc"))` at mod_spatial.R:185 and
# `mod_spatial_qc_server("qc", ...)` at :427, hence the `spatial-qc-` prefix. Its
# results live in the `spatial-results` navset (value "results_qc",
# mod_spatial.R:184) and its controls in the `spatial-steps` accordion
# (value "panel_qc", mod_spatial.R:139).
#
# This is the SECOND Spatial action. The first (`spatial-pipeline-btn_run_all`) is a
# separate, explicit edit below in ts_drive_allowlist_problems(), not a default.
TS_DRIVE_SPATIAL_QC_TAB          <- "results_qc"
TS_DRIVE_SPATIAL_QC_PANELS       <- "panel_qc"

# FOURTH navigation level (S1). The QC module has a navset of its OWN, nested
# inside the `spatial-results` one, and it is the one that hosts the hotspot map,
# histogram, table and CSV button (mod_spatial_qc.R:397-...). Until S1 it declared
# no `id` at all, so bslib auto-generated the tab keys and nothing could address
# them: the drive stopped at `results_qc` and a human click had to guess. With
# Shiny's default `suspendWhenHidden = TRUE` that kept all four outputs suspended
# and the download button `disabled`, while `hotspot_status_ui` — which lives in
# the module's SIDEBAR, outside the navset — rendered normally. That asymmetry is
# what made this look like a rendering bug rather than an unreachable panel.
#
# Both values are measured, not invented: the id is `ns("qc_results")` inside
# `mod_spatial_qc_ui(ns("qc"))` called from mod_spatial.R:185, hence the
# `spatial-qc-` prefix, and the panel value is the `nav_panel(value = "hotspots")`
# at mod_spatial_qc.R. They are declared HERE rather than spelled in the watcher
# so the navigation plan and the UI share one source, and
# `test-mod-spatial-qc-drive.R` asserts the UI really carries this id.
TS_DRIVE_SPATIAL_QC_SUB_TABS_ID  <- "spatial-qc-qc_results"
TS_DRIVE_SPATIAL_QC_SUB_TAB      <- "hotspots"

# =============================================================================
# S2 — the ONE allowlisted export route.
# =============================================================================
# WHY THIS EXISTS. An agent could run `spatial-qc-btn_hotspots` and read
# `n_results`, but could not obtain the TABLE: every module state probe is a
# closed scalar contract, and `n_results` is a COUNT of significant spots. The
# hotspot table has one row per spatial element, so the count and the artefact are
# different things — `sum(res$hotspot != "NS")` over 2702 rows is the number a
# drive run reports. Measured before this slice: 0 of 126 `downloadHandler` sites
# were reachable by any drive action, because the protocol had no verb for one.
#
# WHY IT IS ONE, AND WHY THE CLIENT CHOOSES NOTHING. The route is declared as
# FROZEN DATA mapping a module to the registry key its own exporter is published
# under. The caller names neither a handler, nor an outputId, nor a destination,
# nor a filename, nor a format. That is not a limitation of the implementation, it
# is the design: a download verb that lets a remote caller choose where bytes land
# is a file-write primitive, and this one is not.
#
# GENERALISING IS A SEPARATE, EXPLICIT DECISION. Adding a second entry to
# `TS_DRIVE_EXPORT_ROUTES` is the single edit that would do it, and
# `ts_drive_allowlist_problems()` checks the shape of the table so the addition
# cannot be an accident.
TS_DRIVE_EXPORT_ROUTES <- list(
  spatial_qc = "spatial_qc_hotspot_csv"
)

#' Modules allowed to publish an EXPORT route, derived from the routes above.
TS_DRIVE_EXPORT_MODULES <- names(TS_DRIVE_EXPORT_ROUTES)

#' The accepted request fields for `export_result`: NONE. Declared rather than
#' left implicit, because "the validator has no code for it" and "the validator
#' accepts nothing" look identical from the outside and are not the same promise.
TS_DRIVE_EXPORT_FIELDS <- character(0)

#' Fields a caller might try to smuggle a destination or a payload through. They
#' are REFUSED, not ignored: a silently-ignored field is one the caller believed
#' was honoured, and the file lands somewhere they did not choose.
TS_DRIVE_EXPORT_FORBIDDEN <- c(
  "path", "file", "filename", "dir", "directory", "dest", "where", "out",
  "format", "type", "ext", "encoding", "sep",
  "handler", "output", "outputid", "output_id", "id", "route", "action_name"
)

#' Registry key prefix, so an export can never be confused with a token or an
#' importer in the token/importer listing.
TS_DRIVE_EXPORT_PREFIX <- "tsdrive-export-"

#' Where exported bytes may be written: ONE application-controlled directory
#' under R's session temp dir. Not configurable, not derived from a request, and
#' not reachable by a caller. `tempdir()` is already an allowlisted import root,
#' so this adds no new trust surface — but the point here is different: the
#' destination is fixed by the app, not merely permitted to the app.
TS_DRIVE_EXPORT_DIRNAME <- "ts_drive_exports"

#' Retention bound. Exports are a convenience for an operator collecting evidence,
#' not a store: without a cap the directory grows once per export, in the one
#' place on this host an agent is allowed to write.
#'
#' REACHABLE ONLY BECAUSE each export is a distinct file. With one fixed basename
#' the route overwrote a single entry, the directory could never exceed one, and
#' this cap plus `ts_drive_export_prune()` were dead code — measured, and fixed by
#' deriving the index per export.
TS_DRIVE_EXPORT_MAX_FILES <- 8L

#' Filename stem for an export. Deliberately NOT the human download name, which
#' embeds `global_data$active_spatial_dataset` — a sample name. Putting a
#' biological identifier in a filename that reaches a remote caller is the leak
#' this route exists to avoid. The index appended to this stem is derived from the
#' directory's own contents, never from a request or a session field.
TS_DRIVE_EXPORT_STEM <- "spatial_qc_hotspots"

# --- SC auto-pipeline (measured in modules/sc/mod_sc.R) -----------------------
# The SC domain has NO single-click pipeline button. `btn_auto_pipeline_sc`
# (DOM id `sc-btn_auto_pipeline_sc`, mod_sc.R:46) only calls `showModal()`; the
# pipeline runs from `sc_ap_confirm` — a button INSIDE that dynamic modal
# (mod_sc.R:591) — through `run_sc_auto_pipeline()` (mod_sc.R:598-602). Binding
# the DOM id would report `done` for a click that opened a dialog and computed
# nothing, so the protocol button is its OWN id, dispatched by a counter the
# module publishes (decision A1: one action, frozen declared parameters).
#
# The nested module follows the bulk convention: prefix `sc-pipeline-`, module
# name `sc_pipeline` (the DOM prefix with the dash turned into an underscore).
TS_DRIVE_SC_BUTTON <- "sc-pipeline-run_auto_pipeline"
TS_DRIVE_SC_MODULE <- "sc_pipeline"
# Navigation, measured: `tab_sc` is the root navbar panel (app.R:421) and the
# panel lives in a DOUBLY nested accordion — `acc_workflow` (value "grp_prep",
# mod_sc.R:35/40) then `acc_prep` (value "0_autopipeline", mod_sc.R:41/45).
# `acc_prep` opens on "1_pipeline", so the auto-pipeline panel is CLOSED by
# default: both accordions must be opened for a human to watch the run.
TS_DRIVE_SC_TOP_TAB        <- "tab_sc"
TS_DRIVE_SC_PANELS         <- "0_autopipeline"
TS_DRIVE_SC_ACCORDION_IDS  <- c("sc-acc_workflow", "sc-acc_prep")

#' S5: the "Résultats" navset, and the tab each SC READER lives in.
#'
#' The three SC reader modules already had a nav plan, and it was not wrong about
#' the CONTROLS: it selected the navbar and opened the accordion holding the
#' module's buttons. It stopped there, and the READER is somewhere else.
#'
#' MEASURED, live session (S3): `mod_sc.R:693` closes the accordion and THEN opens
#' `navset_card_underline(id = ns("main_tabs"))` — the results navset is a SIBLING
#' of the accordions, not nested in one. The readers are its panels
#' (`mod_sc.R:695-719`). So opening the accordion left the reader in an UNSELECTED
#' tab, and bslib does not render an output there: a drive-driven `sc_markers`
#' reported `done, n_results = 11964` on the wire while `#sc-markers-table_markers`
#' had `childTags: []` and `offsetParent === null`. Selecting `tab_table` by hand
#' rendered it immediately — DataTables reporting "Lignes 1 à 15 sur 11,964",
#' matching the wire exactly. The reader was never broken; it was never SHOWN.
#'
#' `sc_pipeline` is deliberately ABSENT: its own reader is the log and status bar
#' INSIDE the auto-pipeline accordion, not a results tab, and its plan is unchanged.
#'
#' ⚠️ These values are cross-checked against the module's own `nav_panel(value = )`
#' in `test-drive-watcher.R`. That is not belt-and-braces: `bslib::nav_select()` on
#' an id that does not exist is a SILENT no-op, and `ts_drive_perform_nav()` swallows
#' every navigation error on purpose, so a renamed tab would orphan this plan with
#' nothing failing.
TS_DRIVE_SC_MAIN_TABS_ID  <- "sc-main_tabs"

TS_DRIVE_SC_RESULTS_TAB <- c(
  sc_markers    = "tab_table",
  sc_pathways   = "tab_pathway",
  sc_annotation = "tab_annotation"
)
TS_DRIVE_SC_ANNOTATION_BUTTON <- "sc-annotation-run_annot"
TS_DRIVE_SC_ANNOTATION_MODULE <- "sc_annotation"
TS_DRIVE_SC_ANNOTATION_TOP_TAB <- "tab_sc"
TS_DRIVE_SC_ANNOTATION_PANELS <- "2_annotation"
TS_DRIVE_SC_ANNOTATION_ACCORDION_IDS <- c("sc-acc_workflow", "sc-acc_analyse")
TS_DRIVE_SC_MARKERS_BUTTON <- "sc-markers-run_markers"
TS_DRIVE_SC_MARKERS_MODULE <- "sc_markers"
TS_DRIVE_SC_MARKERS_TOP_TAB <- "tab_sc"
TS_DRIVE_SC_MARKERS_PANELS <- "4_markers"
TS_DRIVE_SC_MARKERS_ACCORDION_IDS <- c("sc-acc_workflow", "sc-acc_analyse")

# --- Bulk signatures (measured in modules/bulk/mod_bulk_signatures.R) --------
# The tenth action, and the FIRST Bulk action outside the import/filter/DE/pathway
# chain. Its prerequisite is Step 1 only (`shared_rv$vst_mat`), so it needs no
# contrast; the nested module server id is "signatures", hence the `bulk-signatures-`
# prefix and NEVER `bulk-`.
#
# ⚠️ DECLARED HERE, ABOVE TS_DRIVE_ALLOWLIST, not next to the button list: the
# allowlist is an eagerly evaluated `list()` and its entries name this module, so a
# definition placed further down would make sourcing fail with "object not found".
TS_DRIVE_BULK_SIGNATURES_BUTTON <- "bulk-signatures-run_signatures"
TS_DRIVE_BULK_SIGNATURES_MODULE <- "bulk_signatures"
TS_DRIVE_BULK_SIGNATURES_TOP_TAB <- "tab_bulk"
TS_DRIVE_BULK_SIGNATURES_PANELS <- "panel_signatures"

# --- Bulk pattern clustering (measured in modules/bulk/mod_bulk_pattern.R) ----
# The eleventh action, and the first one whose parameters are resolved by a RULE
# rather than a frozen value: `run_pattern_clustering()` takes `group_column` as a
# REQUIRED argument that must name a metadata column, so the drive action derives
# it (preferring the column step 2 recorded) and refuses with `not_ready` when it
# cannot decide. Nested module server id is "pattern", hence `bulk-pattern-`.
TS_DRIVE_BULK_PATTERN_BUTTON <- "bulk-pattern-run_pattern"
TS_DRIVE_BULK_PATTERN_MODULE <- "bulk_pattern"
TS_DRIVE_BULK_PATTERN_TOP_TAB <- "tab_bulk"
TS_DRIVE_BULK_PATTERN_PANELS <- "panel_pattern"

# --- Bulk pathway network (measured in modules/bulk/mod_bulk_network.R) -------
# The twelfth action, and the first one with NO rule-resolved parameter: every input
# is a widget default or a declared constant, and the only session read is the
# active contrast - a PREREQUISITE, not a parameter. Nested module server id is
# "network", hence `bulk-network-`. The species is frozen to "hsapiens" because it
# is the ONLY species whose pathway network is available offline; every other one
# makes `graphite::pathways()` attempt a download.
TS_DRIVE_BULK_NETWORK_BUTTON <- "bulk-network-run_network"
TS_DRIVE_BULK_NETWORK_MODULE <- "bulk_network"
TS_DRIVE_BULK_NETWORK_TOP_TAB <- "tab_bulk"
TS_DRIVE_BULK_NETWORK_PANELS <- "panel_network"

# --- SC pathway enrichment (measured in modules/sc/mod_sc_pathways.R) ---------
# The fourth SC action, and the first `sc-` key outside the pipeline trio, so it is
# an EXPLICIT widening of the closed `sc-` set in ts_drive_allowlist_problems()
# below rather than a default. It enriches the marker table step 4 produced, which
# makes the marker step a declared PREREQUISITE (readiness says so) instead of a
# silent fallback. The database is frozen to GOBP because it is the only one that
# needs no optional package beyond clusterProfiler + the org annotation database
# (KEGG adds KEGGREST, which can reach the network).
# Measured: `mod_sc_pathways_ui(ns("pathways"))` at mod_sc.R:593 and
# `mod_sc_pathways_server("pathways", ...)` at :941 - hence the `sc-pathways-`
# prefix. The panel value is "6_pathway" (SINGULAR), mod_sc.R:592.
TS_DRIVE_SC_PATHWAYS_BUTTON <- "sc-pathways-run_pathway"
TS_DRIVE_SC_PATHWAYS_MODULE <- "sc_pathways"
TS_DRIVE_SC_PATHWAYS_TOP_TAB <- "tab_sc"
TS_DRIVE_SC_PATHWAYS_PANELS <- "6_pathway"
TS_DRIVE_SC_PATHWAYS_ACCORDION_IDS <- c("sc-acc_workflow", "sc-acc_analyse")

# --- Spatial QC hotspots (measured in modules/spatial/mod_spatial_qc.R) -------
# The second Spatial action, and the second one whose parameters are resolved by a
# RULE rather than frozen: the metric must be one of the QC columns and a remote
# caller cannot see the widget, so the drive action prefers a declared order and
# refuses with `not_ready` when none is usable. Nested module server id is "qc",
# hence `spatial-qc-`. Synchronous by design: no BPCells reopen, no mirai daemon.
TS_DRIVE_SPATIAL_QC_BUTTON <- "spatial-qc-btn_hotspots"
TS_DRIVE_SPATIAL_QC_MODULE <- "spatial_qc"

# --- Spatial import (measured in modules/import/mod_import_spatial.R) ---------
# The Spatial entry point, added in Phase F. It is the FIRST module whose only
# dataset input is a FOLDER, not a file, so `import_file` for it carries a
# `dir_path` rather than a `counts_path` (see TS_DRIVE_IMPORT_SCHEMA below).
#
# The button is a REAL DOM button (`actionButton(ns("btn_import"))`,
# mod_import_spatial.R:258), so unlike the seven dispatched actions it needs no
# counter: `ts_drive_bind_button()` is enough, exactly like `import_bulk-btn_load`.
# It is bound for a second reason that is not cosmetic — a session a HUMAN
# populated through the native folder picker becomes drivable by an agent
# afterwards, which is the half of the flow that needs no `import_file` at all.
#
# 🔴 NO NAVIGATION TARGET IS DECLARED, and that is a MEASURED decision rather than
# an omission. `nav_panel(i18n$t("Spatial"), ...)` (app.R:395) passes no `value=`,
# so bslib derives the navbar value from the TITLE TAG itself. Read off a live
# session:
#
#   Shiny.$inputValues['main_nav'] after clicking Import > Spatial
#     == "<span class=\"i18n\" data-key=\"Spatial\">Spatial</span>"
#
# A 47-character HTML fragment produced by the i18n shim, not an id. Hard-coding
# it would buy a navigation effect whose failure mode is a SILENT no-op the day
# the shim markup changes — the exact class of defect this protocol is built to
# avoid (cf. the `inputs_ok` / `button` omissions at drive_watcher.R:818). The
# import announces itself with a `showNotification` instead, which is how a human
# sees it. `test-drive-watcher.R` pins the measured value so the reasoning stays
# falsifiable.
TS_DRIVE_SPATIAL_IMPORT_BUTTON <- "import_spatial-btn_import"
TS_DRIVE_SPATIAL_IMPORT_MODULE <- "import_spatial"

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
  "bulk-pathways-run_scores"   = list(kind = "button", module = "bulk_pathways", note = "GSVA/ssGSEA scores (mod_bulk_pathways.R:89)"),
  "spatial-pipeline-btn_run_all" = list(kind = "button", module = "spatial_pipeline", note = "pipeline complet (mod_spatial_pipeline.R, DOM-measured)"),
  # The Spatial import confirm. NO dataset input is allowlisted with it, on
  # purpose: `shinyDirButton` has no `update*` equivalent and spec S5 forbids
  # faking a file widget, so a folder reaches this module ONLY through
  # `import_file`'s `dir_path` (or through a human's native picker). A path is
  # data the watcher validates against the allowlisted roots (S11) and hands to
  # the same loader the UI calls — never a widget value.
  #
  # 🔴 The id is written as a LITERAL, not as the constant, because `list(NAME = )`
  # takes a literal name: `TS_DRIVE_SPATIAL_IMPORT_BUTTON = list(...)` would create
  # a key with no dash in it, which the source-time check below refuses. The
  # literal and the constant are tied together by `ts_drive_allowlist_problems()`.
  "import_spatial-btn_import" = list(kind = "button", module = "import_spatial", note = "import spatial (mod_import_spatial.R:258, DOM-measured)")
)

# The SC and Bulk-signature entries are APPENDED rather than written inline
# because `list(NAME = )` takes a LITERAL name: writing `TS_DRIVE_SC_BUTTON = ...`
# would create an entry called "TS_DRIVE_SC_BUTTON" — a key with no dash, which the
# consistency check below refuses at source() time. `setNames()` keeps the constant
# the single source of truth for each id.
#
# ⚠️ THE TWO VECTORS MUST STAY THE SAME LENGTH. `setNames()` names what is there,
# so adding an entry without adding its id leaves that entry named NA, and the
# source-time check then reports both "NA: entry must declare kind and module" and
# "bound button is missing from the allowlist". Two symptoms, one missing id.
TS_DRIVE_ALLOWLIST <- c(
  TS_DRIVE_ALLOWLIST,
  stats::setNames(
    list(
      list(kind = "button", module = TS_DRIVE_SC_MODULE,
           note = paste("pipeline SC complet via run_sc_auto_pipeline() avec le",
                        "jeu de parametres FIGE .sc_ap_drive_inputs()",
                        "(modules/sc/mod_sc.R). Le bouton DOM",
                        "sc-btn_auto_pipeline_sc n'ouvre QUE la modale de",
                        "parametres: il n'est PAS expose.")),
      list(kind = "button", module = TS_DRIVE_SC_ANNOTATION_MODULE,
           note = "SingleR annotation via run_annot() with frozen .sc_annot_drive_inputs(); the DOM button is not exposed"),
      list(kind = "button", module = TS_DRIVE_SC_MARKERS_MODULE,
           note = "FindAllMarkers via run_markers() with frozen .sc_markers_drive_inputs(); the DOM button is not exposed"),
      list(kind = "button", module = TS_DRIVE_BULK_SIGNATURES_MODULE,
           note = paste("signature scoring via run_signatures() with frozen",
                        ".bulk_signatures_drive_inputs(); `sig_rds` (a fileInput",
                        "path) is deliberately absent. The DOM button is not",
                        "exposed")),
      list(kind = "button", module = TS_DRIVE_BULK_PATTERN_MODULE,
           note = paste("profile clustering via run_pattern_profile() with frozen",
                        ".bulk_pattern_drive_inputs(); `group_column` is resolved by",
                        "RULE (.bulk_pattern_group_column), never frozen. The DOM",
                        "button is not exposed")),
      list(kind = "button", module = TS_DRIVE_BULK_NETWORK_MODULE,
           note = paste("PCSF sub-network via run_network_subnet() with frozen",
                        ".bulk_network_drive_inputs(); species frozen to the only",
                        "one available OFFLINE, and an unavailable network is",
                        "refused with state = missing_dependency. The DOM button is",
                        "not exposed")),
      list(kind = "button", module = TS_DRIVE_SC_PATHWAYS_MODULE,
           note = paste("enrichment via run_sc_pathway_enrichment() with frozen",
                        ".sc_pathways_drive_inputs(); the marker table is a declared",
                        "PREREQUISITE, not a fallback, and the database is frozen to",
                        "GOBP so no package can reach the network. The DOM button is",
                        "not exposed")),
      list(kind = "button", module = TS_DRIVE_SPATIAL_QC_MODULE,
           note = paste("local Getis-Ord hotspots via run_spatial_hotspots() with",
                        "frozen .spatial_qc_drive_inputs(); the metric is resolved by",
                        "RULE (.spatial_hotspot_metric), never frozen. The DOM button",
                        "is not exposed"))
    ),
    c(TS_DRIVE_SC_BUTTON, TS_DRIVE_SC_ANNOTATION_BUTTON,
      TS_DRIVE_SC_MARKERS_BUTTON, TS_DRIVE_BULK_SIGNATURES_BUTTON,
      TS_DRIVE_BULK_PATTERN_BUTTON, TS_DRIVE_BULK_NETWORK_BUTTON,
      TS_DRIVE_SC_PATHWAYS_BUTTON, TS_DRIVE_SPATIAL_QC_BUTTON)
  )
)

#' The ONLY FIFTEEN button ids ts_drive_bind_button() is allowed to instrument.
#' Seven are real DOM buttons measured on a live session; the eighth through
#' fourteenth are dispatched by a counter each module publishes (frozen declared
#' parameters, so the DOM id is deliberately not exposed), and the fifteenth — the
#' Spatial import confirm — is a real DOM button again, for the module whose only
#' dataset input is a folder.
TS_DRIVE_BUTTONS <- c(
  "import_bulk-btn_load",
  "bulk-filter-run_filter_norm",
  "bulk-de-run_de",
  "bulk-pathways-run_pathway",
  "bulk-pathways-run_scores",
  TS_DRIVE_BULK_SIGNATURES_BUTTON,
  TS_DRIVE_BULK_PATTERN_BUTTON,
  TS_DRIVE_BULK_NETWORK_BUTTON,
  "spatial-pipeline-btn_run_all",
  TS_DRIVE_SPATIAL_QC_BUTTON,
  "import_spatial-btn_import",
  TS_DRIVE_SC_BUTTON,
  TS_DRIVE_SC_ANNOTATION_BUTTON,
  TS_DRIVE_SC_MARKERS_BUTTON,
  TS_DRIVE_SC_PATHWAYS_BUTTON
)

#' The single-cell importer (S3), measured in `modules/import/mod_import_sc.R`.
#'
#' The MODULE name is the `app.R` router name, so the DOM ids this module owns
#' are `import_sc-*` — the same relationship `import_spatial` has. It is the
#' THIRD importer, and it is the first one whose entry point is a two-STEP human
#' flow: `btn_add_sample` registers `(sample_name, dir_path)` into the module's
#' own `sample_list()` (mod_import_sc.R:408-419) and `btn_load_dir` then loads
#' every registered pair (mod_import_sc.R:430). The drive does not replay those
#' two clicks: it publishes an importer that calls the SAME loader, so a
#' half-registered list is a shape the drive can never produce.
#'
#' ⚠️ NO BUTTON IS BOUND, deliberately. `import_spatial` binds `btn_import`
#' because that button loads whatever folder the picker already holds, so a
#' human-populated session becomes drivable for free. `btn_load_dir` is the
#' opposite: it requires a populated `sample_list()`, which only the human
#' `btn_add_sample` can create. Binding it would advertise an action that is
#' guaranteed to `req()`-fail for an agent, i.e. a bound button that lies.
TS_DRIVE_SC_IMPORT_MODULE <- "import_sc"

#' The three files that make a 10x **v3** CellRanger output, measured on the
#' three single-cell fixtures in the local corpus — all three are the same
#' flat layout: `matrix.mtx.gz`, `features.tsv.gz`, `barcodes.tsv.gz`.
#'
#' v3 is decided by `features.tsv`, not by `genes.tsv`. That is not pedantry:
#' `load_single_cell_data()` calls `.ensure_10x_features()`
#' (mod_import_sc.R:634), which WRITES a `features.tsv.gz` into the directory to
#' upgrade a v2 tree. For a human that is a helpful repair; for an agent it is an
#' unrequested, un-reversible write to a data directory, so the drive refuses the
#' v2 shape instead of silently upgrading it. The uncompressed variants are the
#' same triplet: `Seurat::Read10X()` takes both, and refusing them would be a
#' restriction with no measured reason behind it.
TS_DRIVE_SC_10X_V3_FILES <- list(
  matrix   = c("matrix.mtx.gz", "matrix.mtx"),
  features = c("features.tsv.gz", "features.tsv"),
  barcodes = c("barcodes.tsv.gz", "barcodes.tsv")
)

#' The fourteen modules the allowlist covers. Anything else is `invalid`.
#' `bulk_filter` is the nested Step 1 module (`bulk-filter-`), named after the
#' same convention as `bulk_de` / `bulk_pathways` / `bulk_signatures` /
#' `bulk_pattern` / `bulk_network`: the DOM prefix with the dash turned into an
#' underscore. The SC and Spatial action modules follow it for the same reason; the
#' parent modules `sc` and `spatial` are deliberately NOT members.
#' `import_spatial` is the only member that is a top-level `app.R` module rather
#' than a nested one, and it is the SECOND importer (after `import_bulk`).
#' `import_sc` is the THIRD, and the first whose payload carries a name the
#' importer cannot do without.
TS_DRIVE_MODULES <- c("import_bulk", "import_spatial", TS_DRIVE_SC_IMPORT_MODULE,
                      "bulk_filter", "bulk_de", "bulk_pathways",
                      TS_DRIVE_BULK_SIGNATURES_MODULE, TS_DRIVE_BULK_PATTERN_MODULE,
                      TS_DRIVE_BULK_NETWORK_MODULE,
                      "spatial_pipeline", TS_DRIVE_SPATIAL_QC_MODULE,
                      TS_DRIVE_SC_MODULE,
                      TS_DRIVE_SC_ANNOTATION_MODULE, TS_DRIVE_SC_MARKERS_MODULE,
                      TS_DRIVE_SC_PATHWAYS_MODULE)

#' Widget kinds the injector knows how to adapt (spec S5).
#'
#' `nav` / `nav_top` are kept in the vocabulary even though no input id uses
#' them: tab switching is a NAVIGATION EFFECT (`ts_drive_nav_plan()`), never an
#' injected input. The top-level navbar is un-namespaced, so a `nav_top` entry
#' would have no namespaced id to live under — `bulk-active_tab` was removed
#' for exactly that reason (the consistency check below refused it).
TS_DRIVE_KINDS <- c("select", "radio", "checkbox", "numeric", "text",
                    "button", "nav", "nav_top")

# =============================================================================
# import_file — the frozen payload shape, declared HERE for an ordering reason
# =============================================================================
# ⚠️ THIS BLOCK MUST STAY ABOVE `ts_drive_allowlist_problems()`. That function is
# CALLED at source() time (the `.ts_drive_allowlist_check` guard below) and its new
# rule reads `TS_DRIVE_IMPORT_MODULES`, so a definition further down would make
# sourcing fail with "object not found" — the same trap the `TS_DRIVE_SC_*`
# constants carry a warning about. Moving it is cheaper than removing the rule.
#
# The rest of the `import_file` contract (roots, path validation, the importer
# registry) lives further down, next to the functions that implement it.

#' Import modes the Bulk loader understands — measured from the
#' `bulk_import_mode` radio in modules/import/mod_import_bulk.R, not invented.
TS_DRIVE_IMPORT_MODES <- c("merged_matrix", "per_sample")

#' Spatial technologies the Spatial loader understands — measured from the
#' `technology` radio in modules/import/mod_import_spatial.R:157 / :327-331, the
#' SAME four values in the same order. The value is part of the `import` block and
#' NOT a widget default, because a wrong value selects a different loader: the
#' `switch()` at mod_import_spatial.R:462 has no fallthrough.
TS_DRIVE_SPATIAL_TECHNOLOGIES <- c("visium", "xenium", "cosmx", "slideseq")

#' PER-MODULE shape of the `import` block (frozen, and the single source of truth).
#'
#' The first version had ONE flat key vector, which quietly assumed a single
#' importer. Adding a second importer with a DIFFERENT payload — a folder instead
#' of a counts file — means the key set can no longer be global: a schema that
#' accepts `dir_path` for Bulk would hand a directory to `smart_read()`.
#'
#' Each entry names its REQUIRED key(s) and its optional ones. `required` is
#' what the module cannot start without, measured from the `req()` each observer
#' opens with (`req(dir_path())` at mod_import_spatial.R:438).
#'
#' ⚠️ `required` is a VECTOR, and it was a scalar until `import_sc` arrived. It
#' was read as `block[[schema$required]]` here and as `req[[need]]` in the poller,
#' so a SECOND mandatory key was inexpressible — the alternative was to declare
#' `sample_name` optional and enforce it by hand somewhere else, which is a
#' schema that lies about its own contract. Both readers now loop.
#'
#' `check` is the OPTIONAL name of a per-module validator, called as
#' `fn(block, roots)` and returning the same `list(ok, errors, import)` shape. It
#' is DATA rather than a third `identical(module, ...)` boolean in the shared
#' validator, for the reason `TS_DRIVE_IMPORT_MODULES` gives: the boolean is
#' exactly the place a reader stops looking.
#'
#' `sample_name` is OPTIONAL for Spatial and resolved by RULE when absent (see
#' `.ts_drive_spatial_sample_name()`), never read from the session: the module's
#' own fallback is `basename(dir_path())` (mod_import_spatial.R:439), and reusing
#' the app's own rule is what keeps the drive and the UI in agreement. It is
#' REQUIRED for `import_sc`, which is the whole point of that entry — see
#' `ts_drive_validate_sc_import()`.
TS_DRIVE_IMPORT_SCHEMA <- list(
  import_bulk    = list(required = c("counts_path"),
                        optional = c("metadata_path", "mode")),
  import_spatial = list(required = c("dir_path"),
                        optional = c("sample_name", "technology")),
  import_sc      = list(required = c("dir_path", "sample_name"),
                        optional = character(0),
                        check = "ts_drive_validate_sc_import")
)

#' Actions that must be PINNED to a session (frozen).
#'
#' Two of the seven change what the session holds, and both were MEASURED on a
#' live session accepting a scenario that carried no token at all, because the
#' comparison was guarded by `nzchar()` and an absent token skipped it:
#'
#'   * `export_result` writes a file (S2, 2026-09-26);
#'   * `import_file` REPLACES the session's primary object — `global_data$sc_obj`
#'     for `import_sc`, `spatial_obj` for `import_spatial` (S3, 2026-09-26).
#'
#' The realistic failure for both is a STALE scenario, not a hostile one: written
#' for a previous session, replayed after the user has loaded something else.
#'
#' ⚠️ The other five keep the token-OPTIONAL affordance on purpose. A one-shot
#' scenario an operator drops into `tools/_drive/` by hand carries no token, and
#' widening this to every action is precisely the change that would break them.
#' It is DATA, and not a literal in the comparison, so the S2 and S3 test files
#' assert against the same set instead of each keeping a private copy.
TS_DRIVE_TOKEN_PINNED_ACTIONS <- c("import_file", "export_result")

#' Modules allowed to publish an IMPORTER (frozen).
#'
#' ⚠️ NOT the same set as `TS_DRIVE_MODULES`. A module can be a drivable target
#' and still own no importer, and the first version expressed that with a
#' hard-coded `!identical(module, "import_bulk")` INSIDE the guard. A third
#' importer would then have needed a third edit in the same boolean, and the
#' boolean is exactly the place a reader stops looking. Declared as DATA, the
#' relationship to `names(TS_DRIVE_IMPORT_SCHEMA)` is checkable instead — and
#' `ts_drive_allowlist_problems()` pins that the two agree.
TS_DRIVE_IMPORT_MODULES <- names(TS_DRIVE_IMPORT_SCHEMA)

#' Every key any importer may carry — the union, kept for the refusal message and
#' for callers that need "is this key known at all". Membership in the UNION is not
#' enough to accept a key: `ts_drive_validate_import()` routes on the module first.
TS_DRIVE_IMPORT_KEYS <- unique(unlist(lapply(TS_DRIVE_IMPORT_SCHEMA, function(e) {
  c(e$required, e$optional)
}), use.names = FALSE))

#' Injection ORDER for a module's controls, declared as DATA.
#'
#' A `selectInput` can only hold a value that is among its CURRENT options, so
#' injecting a value whose options do not exist yet cannot work: the browser
#' cannot represent it, the value never reaches the client, and the owning module
#' then supplies its own default in its place.
#'
#' MEASURED live (2026-09-27, GSE164073 27,946 x 18): one scenario carrying
#' `condition_col = "condition"`, `group_ref = "CoV2"`, `group_target = "mock"`
#' left the client holding `mock` / `CoV2` — the module's defaults, which are the
#' requested pair REVERSED only because `lvls[1]`/`lvls[2]` happen to be
#' `mock`/`CoV2` for that dataset. No code swaps ref and target: a keep-if-valid
#' rule in the rebuild observer was implemented and the identical batch still
#' failed, so the observer is not the culprit.
#'
#' The measured proof of the mechanism: inject `condition_col` ALONE, let the
#' module rebuild, then inject the reverse pair — it landed and produced
#' `active_contrast = "mock_vs_CoV2"`, the requested pair under the module's
#' `target_vs_ref` naming.
#'
#' Each entry is a LIST OF STAGES in injection order, and a control appears in
#' exactly one. The drive injects one stage per protocol beat and only begins
#' confirming once the last is in, so a value is never handed to a select that
#' cannot represent it. Nothing is re-injected: each control is sent once.
#'
#' A module absent from this table injects everything in one batch, which is the
#' historical behaviour and the right default — a stage costs an extra beat, so an
#' order is declared only where the dependency has been MEASURED.
#'
#' @section Why this is data and not inference:
#' The dependency is a property of the MODULE's reactive logic, which the drive
#' cannot see and must not guess. An inferred order that is wrong injects a value
#' in the wrong beat, which is the very failure this exists to remove.
TS_DRIVE_INPUT_STAGES <- list(
  bulk_de = list(
    # Stage 1: what the group choices are NOT derived from.
    c("bulk-de-condition_col", "bulk-de-covariates", "bulk-de-de_engine",
      "bulk-de-shrink_lfc", "bulk-de-lfc_thresh", "bulk-de-padj_thresh",
      "bulk-de-heatmap_top_n"),
    # Stage 2: the two selects whose OPTIONS are rebuilt from `condition_col`
    # (mod_bulk_de_engine.R, observeEvent(input$condition_col, ...)).
    c("bulk-de-group_ref", "bulk-de-group_target")
  )
)

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
    if (identical(e$module, "sc")) {
      problems <- c(problems, sprintf("%s: module 'sc' is out of scope", id))
    }
    # The `sc-` namespace is a CLOSED set of FOUR declared actions. The
    # original v1 rail was "no sc- key at all"; replacing it with named
    # exceptions keeps every later widening a visible edit rather than a
    # default. A LYING module field buys nothing: the id prefix alone is enough
    # to refuse the key.
    sc_buttons <- c(TS_DRIVE_SC_BUTTON, TS_DRIVE_SC_ANNOTATION_BUTTON,
                    TS_DRIVE_SC_MARKERS_BUTTON, TS_DRIVE_SC_PATHWAYS_BUTTON)
    if (grepl("^sc-", id) && !id %in% sc_buttons) {
      problems <- c(problems, sprintf(
        "%s: only %s, %s, %s or %s is in scope, every other sc- key is out of scope",
        id, TS_DRIVE_SC_BUTTON, TS_DRIVE_SC_ANNOTATION_BUTTON,
        TS_DRIVE_SC_MARKERS_BUTTON, TS_DRIVE_SC_PATHWAYS_BUTTON))
    }
    if (identical(id, TS_DRIVE_SC_BUTTON) &&
        !identical(e$module, TS_DRIVE_SC_MODULE)) {
      problems <- c(problems, sprintf("%s: must declare module '%s'",
                                      id, TS_DRIVE_SC_MODULE))
    }
    if (identical(id, TS_DRIVE_SC_ANNOTATION_BUTTON) &&
        !identical(e$module, TS_DRIVE_SC_ANNOTATION_MODULE)) {
      problems <- c(problems, sprintf("%s: must declare module '%s'",
                                      id, TS_DRIVE_SC_ANNOTATION_MODULE))
    }
    if (identical(id, TS_DRIVE_SC_MARKERS_BUTTON) &&
        !identical(e$module, TS_DRIVE_SC_MARKERS_MODULE)) {
      problems <- c(problems, sprintf("%s: must declare module '%s'",
                                      id, TS_DRIVE_SC_MARKERS_MODULE))
    }
    if (identical(id, TS_DRIVE_BULK_SIGNATURES_BUTTON) &&
        !identical(e$module, TS_DRIVE_BULK_SIGNATURES_MODULE)) {
      problems <- c(problems, sprintf("%s: must declare module '%s'",
                                      id, TS_DRIVE_BULK_SIGNATURES_MODULE))
    }
    if (identical(id, TS_DRIVE_BULK_PATTERN_BUTTON) &&
        !identical(e$module, TS_DRIVE_BULK_PATTERN_MODULE)) {
      problems <- c(problems, sprintf("%s: must declare module '%s'",
                                      id, TS_DRIVE_BULK_PATTERN_MODULE))
    }
    if (identical(id, TS_DRIVE_BULK_NETWORK_BUTTON) &&
        !identical(e$module, TS_DRIVE_BULK_NETWORK_MODULE)) {
      problems <- c(problems, sprintf("%s: must declare module '%s'",
                                      id, TS_DRIVE_BULK_NETWORK_MODULE))
    }
    if (identical(id, TS_DRIVE_SC_PATHWAYS_BUTTON) &&
        !identical(e$module, TS_DRIVE_SC_PATHWAYS_MODULE)) {
      problems <- c(problems, sprintf("%s: must declare module '%s'",
                                      id, TS_DRIVE_SC_PATHWAYS_MODULE))
    }
    if (identical(id, TS_DRIVE_SPATIAL_QC_BUTTON) &&
        !identical(e$module, TS_DRIVE_SPATIAL_QC_MODULE)) {
      problems <- c(problems, sprintf("%s: must declare module '%s'",
                                      id, TS_DRIVE_SPATIAL_QC_MODULE))
    }
    if (identical(id, TS_DRIVE_SPATIAL_IMPORT_BUTTON) &&
        !identical(e$module, TS_DRIVE_SPATIAL_IMPORT_MODULE)) {
      problems <- c(problems, sprintf("%s: must declare module '%s'",
                                      id, TS_DRIVE_SPATIAL_IMPORT_MODULE))
    }
    if (identical(id, "spatial-pipeline-btn_run_all") &&
        !identical(e$module, "spatial_pipeline")) {
      problems <- c(problems, sprintf("%s: must declare module '%s'",
                                      id, "spatial_pipeline"))
    }
    # The `spatial-` / `import_spatial-` namespace is a CLOSED set of THREE declared
    # actions, for the same reason as the `sc-` set above. The Spatial pipeline,
    # the Spatial local hotspots and the Spatial import confirm are named; every
    # other key in either namespace, including every child-module button, stays out
    # of scope. Widening it is a visible edit here, never a default.
    spatial_buttons <- c("spatial-pipeline-btn_run_all", TS_DRIVE_SPATIAL_QC_BUTTON,
                         TS_DRIVE_SPATIAL_IMPORT_BUTTON)
    if (grepl("^(spatial|import_spatial)-", id) && !id %in% spatial_buttons) {
      problems <- c(problems, sprintf(
        "%s: only %s, %s or %s is in scope, every other spatial- key is out of scope",
        id, spatial_buttons[[1L]], spatial_buttons[[2L]], spatial_buttons[[3L]]))
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
  # MEASURED RULE, not a preference: every importer in TS_DRIVE_IMPORT_SCHEMA is
  # reachable only if it is also a scenario module — `import_file` resolves the
  # importer through the module, so a schema entry with no module would be a
  # payload shape no scenario could ever name. The two tables are declared in
  # different places (one here, one above), and the only automated check that they
  # still agree is this one.
  for (m in TS_DRIVE_IMPORT_MODULES) {
    if (!m %in% TS_DRIVE_MODULES) {
      problems <- c(problems, sprintf(
        "importer module '%s' is not in TS_DRIVE_MODULES, so `import_file` can never route to it", m))
    }
  }
  # S2: the same rule for export routes, plus the shape of the table itself. A
  # route whose module is not a scenario module could never be dispatched, and a
  # route table that grew a non-scalar or an empty value would name nothing.
  for (m in TS_DRIVE_EXPORT_MODULES) {
    if (!m %in% TS_DRIVE_MODULES) {
      problems <- c(problems, sprintf(
        "export module '%s' is not in TS_DRIVE_MODULES, so `export_result` can never route to it", m))
    }
  }
  for (m in TS_DRIVE_EXPORT_MODULES) {
    v <- TS_DRIVE_EXPORT_ROUTES[[m]]
    if (!is.character(v) || length(v) != 1L || is.na(v) || !nzchar(v)) {
      problems <- c(problems, sprintf(
        "export route for '%s' must name exactly one non-empty registry key", m))
    }
  }
  # The trust boundary, asserted at SOURCE time: if a field were ever added to the
  # accepted set, the route would stop being the thing this slice claims it is.
  if (length(TS_DRIVE_EXPORT_FIELDS) != 0L) {
    problems <- c(problems, sprintf(
      "TS_DRIVE_EXPORT_FIELDS must stay EMPTY: `export_result` lets the caller choose nothing (found %d)",
      length(TS_DRIVE_EXPORT_FIELDS)))
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
# uses. Two things therefore have to be frozen as data (the key sets and the roots):
#
#   1. the KEY SET of the `import` payload — a whitelist, exactly like `inputs`,
#      so a field the injector never reads can never reach it. It is PER MODULE
#      rather than global, because two importers have different payloads (a
#      counts FILE vs a data FOLDER); see `TS_DRIVE_IMPORT_SCHEMA`, declared near
#      the top of this file for a source-order reason;
#   2. the ROOTS a path may come from — spec S11: "Do not execute
#      user-supplied file paths outside the project, `tempdir()`, or an
#      explicit allowlisted data dir. Reject `..`".
#
# Everything here is pure: no Shiny call (C2), and the only I/O is
# `file.exists()` / `dir.exists()`, so the whole rule is testable from Rscript
# without a session.

#' Import modes the Bulk loader understands — measured from the
#' `bulk_import_mode` radio in modules/import/mod_import_bulk.R, not invented.
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

#' Resolve and confine ONE agent-supplied path (spec S11), without deciding what
#' kind of filesystem object it must be.
#'
#' Split out of `ts_drive_validate_import_path()` because the Spatial importer
#' needs the IDENTICAL rules for a FOLDER: same `..` refusal on the raw string,
#' same allowlisted-roots confinement, same actionable verdict. Two copies of
#' this function would be two places where a future edit to the security rule
#' could be applied to files and forgotten for directories — and a directory
#' handed to a Spatial loader is the more dangerous of the two, because the
#' loaders read every file under it.
#'
#' @return list(ok, path, reason). `ok = TRUE` does NOT mean the path is usable;
#' the caller still has to check it is the KIND it needs.
#' @noRd
.ts_drive_resolve_import_path <- function(path, roots) {
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
  # 🔴 A PREFIX test is not a CONTAINMENT test. `.../roots-EVIL/secret.csv` shares
  # every character of `.../roots`, so the raw comparison ACCEPTED a path outside
  # every allowlisted root — MEASURED 2026-09-29, one predicate, both public
  # wrappers (`_import_path` and `_import_dir`) and therefore every importer. `..`
  # was already refused, so this was the whole remaining hole.
  # Containment is equality OR a match that ends at a SEPARATOR. Each root has its
  # trailing separator trimmed first, so appending one cannot produce a "//" that
  # refuses everything inside a root written as `.../roots/`, and a drive-letter
  # root — which already ends in "/" — keeps working. Fails CLOSED: a root that
  # trims to nothing is dropped rather than turned into a match-everything prefix.
  roots <- sub("[/\\\\]+$", "", roots)
  roots <- roots[!is.na(roots) & nzchar(roots)]
  inside <- any(vapply(roots, function(r)
    identical(full, r) || startsWith(full, paste0(r, "/")), logical(1)))
  if (!inside) {
    return(list(ok = FALSE, path = NULL,
                reason = sprintf("the path is outside every allowlisted root (%s)",
                                 paste(basename(roots), collapse = ", "))))
  }
  list(ok = TRUE, path = full, reason = NULL)
}

#' Validate ONE FILE an agent asked to import (spec S11).
#'
#' Returns a VERDICT, not a boolean, because a refusal has to be actionable: an
#' agent told only `FALSE` will retry the same path forever.
#'
#' @param path The candidate path (one string).
#' @param roots Allowlisted roots. Defaults to `ts_drive_import_roots()`.
#' @return list(ok = logical, path = normalised-or-NULL, reason = character-or-NULL)
ts_drive_validate_import_path <- function(path, roots = ts_drive_import_roots()) {
  res <- .ts_drive_resolve_import_path(path, roots)
  if (!isTRUE(res$ok)) return(res)

  if (!file.exists(res$path)) {
    return(list(ok = FALSE, path = NULL, reason = "the path does not exist"))
  }
  if (dir.exists(res$path)) {
    # MEASURED, not hypothetical: the dataset this grade was handed over as is
    # `GSE164073_Eye_count_matrix.csv/` — a DIRECTORY holding a file of the same
    # name. Naming that is the difference between one retry and a mystery.
    return(list(ok = FALSE, path = NULL,
                reason = "the path is a directory; pass the counts FILE inside it"))
  }
  list(ok = TRUE, path = res$path, reason = NULL)
}

#' Validate ONE DIRECTORY an agent asked to import (spec S11).
#'
#' The exact mirror of `ts_drive_validate_import_path()`, and deliberately so: the
#' Spatial loaders take the ROOT of a 10X `outs/` tree, so a FILE here means the
#' agent named one of the files inside it — the same mistake in the other
#' direction, and the same actionable wording.
#'
#' @return list(ok = logical, path = normalised-or-NULL, reason = character-or-NULL)
ts_drive_validate_import_dir <- function(path, roots = ts_drive_import_roots()) {
  res <- .ts_drive_resolve_import_path(path, roots)
  if (!isTRUE(res$ok)) return(res)

  if (!file.exists(res$path)) {
    return(list(ok = FALSE, path = NULL, reason = "the path does not exist"))
  }
  if (!dir.exists(res$path)) {
    return(list(ok = FALSE, path = NULL,
                reason = "the path is a file; pass the data FOLDER that contains it"))
  }
  list(ok = TRUE, path = res$path, reason = NULL)
}

#' The sample name a Spatial import uses when the scenario does not name one.
#'
#' A RULE, not a read: the module's own fallback is `basename(dir_path())`
#' (mod_import_spatial.R:439), so deriving it here makes the drive produce the same
#' label a human would get from the same folder. It is NOT read from the session —
#' a name already in `global_data$spatial_datasets` would silently overwrite that
#' dataset, and an agent must not be able to choose that by omission.
#'
#' A BLANK `requested` falls back to the rule rather than producing a dataset
#' keyed on `""`, because that is what the widget does: the UI's own test is
#' `if (nchar(trimws(input$sample_name)))`, so a whitespace-only box already yields
#' `basename(dir)`. Following the app's rule rather than a stricter one of our own
#' is the whole point — two different rules would make the drive and the UI
#' disagree about what a folder is called.
#'
#' @return A single non-empty name, or `NA_character_` when none can be derived.
.ts_drive_spatial_sample_name <- function(dir_path, requested = NULL) {
  if (!is.null(requested)) {
    name <- if (length(requested) == 1L) trimws(as.character(requested)) else ""
    if (!is.na(name) && nzchar(name)) return(name)
  }
  base <- basename(gsub("[/\\\\]+$", "", dir_path))
  if (is.na(base) || !nzchar(base)) NA_character_ else base
}

#' Validate the `import` block of a scenario, for the module that will consume it.
#'
#' @param block The parsed `import` object, or NULL.
#' @param roots Allowlisted roots. Defaults to `ts_drive_import_roots()`.
#' @param module The consuming module. Routes the key set, because a single flat
#'   key vector cannot describe two importers with different payloads.
#' @return list(ok, errors, import) — `import` is the WHITELISTED block.
ts_drive_validate_import <- function(block, roots = ts_drive_import_roots(),
                                     module = "import_bulk") {
  if (!is.character(module) || length(module) != 1L || is.na(module) ||
      !module %in% names(TS_DRIVE_IMPORT_SCHEMA)) {
    return(list(ok = FALSE,
                errors = sprintf("module '%s' publishes no importer; the importers are: %s",
                                 paste(as.character(module), collapse = ", "),
                                 paste(names(TS_DRIVE_IMPORT_SCHEMA), collapse = ", ")),
                import = NULL))
  }
  schema <- TS_DRIVE_IMPORT_SCHEMA[[module]]
  if (is.null(block) || !is.list(block) || !length(block)) {
    return(list(ok = FALSE,
                errors = sprintf("`import` must be a JSON object carrying at least %s",
                                 paste0("`", schema$required, "`",
                                        collapse = if (length(schema$required) > 1L) " and " else "")),
                import = NULL))
  }
  errors <- character(0)
  # A key that belongs to ANOTHER importer is refused exactly like an invented
  # one, and the message says so: `dir_path` is a known key, so a silent drop
  # would leave the agent retrying a payload that can never work.
  unknown <- setdiff(names(block), c(schema$required, schema$optional))
  if (length(unknown)) {
    elsewhere <- vapply(unknown, function(k) {
      owner <- names(TS_DRIVE_IMPORT_SCHEMA)[vapply(TS_DRIVE_IMPORT_SCHEMA,
        function(e) k %in% c(e$required, e$optional), logical(1))]
      if (length(owner)) sprintf("%s (belongs to %s)", k, paste(owner, collapse = "/")) else k
    }, character(1))
    errors <- c(errors, sprintf("unknown key(s) in `import` for '%s': %s",
                                module, paste(elsewhere, collapse = ", ")))
  }

  out <- list()
  # A MISSING required key is named as such, before it reaches the path
  # validator. Without this the refusal was "the path must be one non-empty
  # string", which is true but useless: the payload carried no path at all, and an
  # agent reading it would go looking for a malformed string it never sent.
  # EVERY missing key is named, not just the first: an agent that sent `dir_path`
  # alone has to be told `sample_name` is the other half, or it retries the same
  # half forever.
  absent <- schema$required[vapply(schema$required, function(k) is.null(block[[k]]), logical(1))]
  if (length(absent)) {
    # NOT `sprintf()` over the vector: with two absent keys it would emit two
    # half-sentences, each with the other's plural ("`dir_path` are required by
    # 'import_sc'"). The single-key wording is byte-identical to what the scalar
    # version produced, so the existing importers' refusals do not change.
    one <- length(absent) == 1L
    errors <- c(errors, sprintf("%s %s required by '%s' and %s absent",
                                paste0("`", absent, "`", collapse = if (one) "" else " and "),
                                if (one) "is" else "are", module,
                                if (one) "is" else "are"))
    return(list(ok = FALSE, errors = errors, import = NULL))
  }
  if (identical(module, "import_spatial")) {
    dp <- ts_drive_validate_import_dir(block$dir_path, roots = roots)
    if (!isTRUE(dp$ok)) {
      errors <- c(errors, sprintf("`dir_path`: %s", dp$reason))
    } else {
      out$dir_path <- dp$path
      tech <- if (is.null(block$technology)) "visium" else as.character(block$technology)
      if (length(tech) != 1L || is.na(tech) || !tech %in% TS_DRIVE_SPATIAL_TECHNOLOGIES) {
        errors <- c(errors, sprintf("`technology` must be one of: %s",
                                    paste(TS_DRIVE_SPATIAL_TECHNOLOGIES, collapse = ", ")))
      } else {
        # The DEFAULT is declared, not inherited: `technology` selects the loader
        # in a `switch()` with no fallthrough, so leaving it unset must mean one
        # specific loader rather than "whatever the widget happens to hold".
        out$technology <- tech
        name <- .ts_drive_spatial_sample_name(dp$path, block$sample_name)
        if (is.na(name)) {
          errors <- c(errors, "`sample_name` must be one non-empty string")
        } else {
          out$sample_name <- name
        }
      }
    }
    return(list(ok = !length(errors), errors = errors, import = out))
  }

  # The per-module rules, named by DATA. `import_bulk` is what remains once
  # `import_spatial` has returned, and it is reached by ELIMINATION — a shape
  # that reads badly and breaks the moment a fourth importer lands without a
  # `check`. So an entry that declares one is dispatched here, before the Bulk
  # fallthrough, and an entry that does not keeps the historical Bulk path
  # untouched.
  if (!is.null(schema$check)) {
    fn <- get0(schema$check, envir = globalenv())
    if (!is.function(fn)) {
      # A declared validator that does not exist is a REFUSAL, never a silent
      # skip: skipping would let `import_sc` fall through to the Bulk branch and
      # be handed `counts_path = NULL`.
      return(list(ok = FALSE, import = NULL, errors = c(errors, sprintf(
        "'%s' declares check '%s', which is not a callable - the importer is unavailable",
        module, schema$check))))
    }
    r <- fn(block, roots = roots)
    return(list(ok = !length(errors) && isTRUE(r$ok),
                errors = c(errors, r$errors), import = r$import))
  }

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

#' Modules allowed to publish an IMPORTER (frozen) — `TS_DRIVE_IMPORT_MODULES`,
#' declared at the top of this file with the schema it is derived from.

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
#' The `import_sc` payload rules (S3), reached through `TS_DRIVE_IMPORT_SCHEMA`.
#'
#' Two things are decided here and nowhere else.
#'
#' 1. **The directory must be a 10x-v3 TRIPLET.** `ts_drive_validate_import_dir()`
#'    already confines the path to the allowlisted roots and proves it is a
#'    directory; on top of that this checks the three files
#'    `TS_DRIVE_SC_10X_V3_FILES` names. A v2 tree (`genes.tsv`) is refused rather
#'    than upgraded, because the human path upgrades it by WRITING
#'    `features.tsv.gz` into the caller's data directory
#'    (`.ensure_10x_features()`, mod_import_sc.R:634) — a side effect an agent
#'    neither asked for nor can undo.
#'
#' 2. **`sample_name` is EXPLICIT and REQUIRED.** The Spatial importer falls back
#'    to `basename(dir_path())` and the SC human UI asks for the name in a
#'    `textInput` (mod_import_sc.R:197). The drive takes the second shape: a
#'    basename is a directory label, not a sample identity, and inferring one is
#'    how two datasets end up sharing an `orig.ident`. It is also the value that
#'    becomes a name in the module's `sample_list()`, so a separator or a
#'    traversal in it is refused rather than trimmed.
#'
#' @return list(ok, errors, import) with `import$dir_path` resolved and
#'   `import$sample_name` trimmed.
ts_drive_validate_sc_import <- function(block, roots = ts_drive_import_roots()) {
  errors <- character(0)
  out <- list()

  dp <- ts_drive_validate_import_dir(block$dir_path, roots = roots)
  if (!isTRUE(dp$ok)) {
    # The confinement verdict wins and the triplet is NOT checked: a path outside
    # the roots is refused for being outside, and reporting "features.tsv.gz is
    # missing" about a directory the agent was never allowed to read would be
    # both noisier and a small information leak about a path it may not see.
    return(list(ok = FALSE, errors = sprintf("`dir_path`: %s", dp$reason), import = NULL))
  }
  out$dir_path <- dp$path

  missing <- character(0)
  for (part in names(TS_DRIVE_SC_10X_V3_FILES)) {
    if (!any(file.exists(file.path(dp$path, TS_DRIVE_SC_10X_V3_FILES[[part]])))) {
      missing <- c(missing, part)
    }
  }
  if (length(missing)) {
    # The accepted names are spelled out ONE PER FILE, and each part is labelled
    # with ITS OWN two names.
    #
    # Two drafts of this message were wrong in ways a "does the text contain the
    # filename" test cannot see. The first ran `sprintf()` over a LIST and printed
    # `c("matrix.mtx.gz", "matrix.mtx")`. The second dropped the list-literal but
    # hard-coded `TS_DRIVE_SC_10X_V3_FILES[[1L]]` for ALL THREE parts, so it
    # announced `features (`matrix.mtx.gz` or `matrix.mtx`)` — telling an agent
    # that a barcodes file is an acceptable FEATURES file. Every filename still
    # appeared literally, in the first sentence, so the test stayed green. Hence
    # the per-part `vapply()` over the LIST ITSELF, and a test that asserts the
    # PAIRING rather than the presence of a word.
    # An explicit loop, not `vapply()` over the list: `vapply(X, FUN)` hands FUN
    # each ELEMENT, so `names(z)` inside it is NULL and the label came out empty
    # (measured: "values must be length 1, but FUN(X[[1]]) result is length 0").
    lab <- function(sel, both = FALSE) {
      vapply(sel, function(p) {
        z <- TS_DRIVE_SC_10X_V3_FILES[[p]]
        if (both) sprintf("%s (`%s` or `%s`)", p, z[1L], z[2L])
        else sprintf("%s (`%s`)", p, z[1L])
      }, character(1), USE.NAMES = FALSE)
    }
    errors <- c(errors, sprintf(
      "`dir_path` is not a 10x-v3 output. Missing: %s. A CellRanger v3 triplet directory carries %s.",
      paste(lab(missing), collapse = ", "),
      paste(lab(names(TS_DRIVE_SC_10X_V3_FILES), both = TRUE), collapse = ", ")))
    # The out-of-scope hint belongs to a directory with NO matrix at all: a
    # directory that has one but lacks `features` is a broken v3 tree, and telling
    # the agent to go and use the file-based Option B would send it down the wrong
    # road instead of naming the one file it is missing.
    if ("matrix" %in% missing) {
      errors <- c(errors, paste(
        "  (a `.h5` / `.rds` / `.rda` FILE is the import panel's Option B or C and",
        "is out of scope here: the drive imports a 10x-v3 triplet DIRECTORY only)"))
    }
  }

  nm <- block$sample_name
  if (is.null(nm)) {
    # Unreachable through `ts_drive_validate_import()` (the schema already
    # required it); kept so calling this validator directly cannot under-report.
    errors <- c(errors, "`sample_name` is required and is absent")
  } else if (!is.character(nm) || length(nm) != 1L || is.na(nm)) {
    # A number is refused rather than coerced: `as.character(42)` would quietly
    # accept a payload shape the agent did not mean to send.
    errors <- c(errors, "`sample_name` must be one non-empty string")
  } else {
    trimmed <- trimws(nm)
    if (!nzchar(trimmed)) {
      errors <- c(errors, "`sample_name` must be one non-empty string (it became empty when trimmed)")
    } else if (grepl("[/\\\\]", trimmed) || identical(trimmed, "..") ||
               grepl("^[A-Za-z]:", trimmed)) {
      # It becomes an `orig.ident` AND a key of the module's `sample_list()`.
      # A separator is either a display bug or an attempt to make one sample read
      # as two; the drive has no use for it either way.
      errors <- c(errors, paste0(
        "`sample_name` must be a bare name, not a path: '", trimmed,
        "'. It is stored as the sample identity, so a separator or a drive letter is refused."))
    } else if (grepl("[[:cntrl:]]", trimmed)) {
      errors <- c(errors, "`sample_name` must not contain control characters")
    } else {
      out$sample_name <- trimmed
    }
  }

  list(ok = !length(errors), errors = errors, import = if (length(errors)) NULL else out)
}

#' @param module One of `TS_DRIVE_IMPORT_MODULES`.
#' @param importer `function(request)` -> list(ok, status, errors, warnings).
#' @param state Optional bounded probe, `function()` -> named list. Only the
#'   `import_sc` module uses it: it binds no button, so it has no token to carry
#'   a probe, and the IMPORTER seam is the only honest place left. The registry
#'   entry becomes a LIST only when a probe is attached, so the bare-function form
#'   the other two importers use — and `ts_drive_importer_of()`'s reading of it —
#'   are untouched.
ts_drive_publish_importer <- function(global_data, module, importer, state = NULL) {
  if (!is.character(module) || length(module) != 1L || is.na(module) ||
      !module %in% TS_DRIVE_IMPORT_MODULES) {
    warning(sprintf("ts_drive_publish_importer(): '%s' has no importer in the import allowlist (%s) - ignored.",
                    paste(as.character(module), collapse = ", "),
                    paste(TS_DRIVE_IMPORT_MODULES, collapse = ", ")))
    return(invisible(FALSE))
  }
  if (!is.function(importer)) {
    warning("ts_drive_publish_importer(): the importer must be a function — ignored.")
    return(invisible(FALSE))
  }
  # A non-function `state` is REFUSED, not stored: a list entry whose `state` is
  # not callable would be skipped by the collector exactly like a missing probe,
  # so accepting one would make a typo indistinguishable from "no probe".
  if (!is.null(state) && !is.function(state)) {
    warning("ts_drive_publish_importer(): `state` must be a function or NULL — ignored.")
    state <- NULL
  }
  reg <- ts_drive_registry(global_data)
  if (is.null(reg)) return(invisible(FALSE))
  reg[[paste0(TS_DRIVE_IMPORTER_PREFIX, module)]] <-
    if (is.null(state)) importer else list(importer = importer, state = state)
  invisible(TRUE)
}

#' The MODULE a REGISTRY entry belongs to.
#'
#' The collector walks the registry by id, and an importer entry is named
#' `tsdrive-importer-<module>`, which is NOT an allowlisted input id. The lexical
#' fallback of `ts_drive_module_of()` would split it on the LAST dash and answer
#' `tsdrive-importer-import`, a module that does not exist. The prefix is stripped
#' instead — from the DATA, not from a guess.
#' @noRd
ts_drive_module_of_registry <- function(id) {
  if (is.character(id) && length(id) == 1L && !is.na(id) &&
      startsWith(id, TS_DRIVE_IMPORTER_PREFIX)) {
    mod <- substring(id, nchar(TS_DRIVE_IMPORTER_PREFIX) + 1L)
    return(if (nzchar(mod)) mod else NA_character_)
  }
  ts_drive_module_of(id)
}

#' Read a module's published importer (used by the poller through `effects`).
#'
#' Reads EITHER shape: a bare function (the two button-bound importers) or a
#' `list(importer, state)` entry (an importer that also publishes a probe). The
#' second branch is what keeps the live import path working for all three
#' modalities after that shape was introduced.
#'
#' @return The importer function, or NULL when none was published.
ts_drive_importer_of <- function(global_data, module) {
  reg <- ts_drive_registry(global_data)
  if (is.null(reg)) return(NULL)
  entry <- reg[[paste0(TS_DRIVE_IMPORTER_PREFIX, module)]]
  fn <- if (is.list(entry)) entry$importer else entry
  if (is.function(fn)) fn else NULL
}

# =============================================================================
# S2 — export routes
# =============================================================================

#' The ONE application-controlled directory exported bytes may be written to.
#'
#' Created on demand, and never a parameter of anything a caller reaches. The
#' argument exists only so a test can pin the location; the poller never passes
#' it.
#'
#' @return An existing directory path.
ts_drive_export_dir <- function() {
  d <- file.path(tempdir(), TS_DRIVE_EXPORT_DIRNAME)
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

#' Publish a module's EXPORT route so `export_result` can reach it.
#'
#' The same shape as `ts_drive_publish_importer()` and for the same reason: the
#' module owns the knowledge of what its artefact IS, and the protocol only knows
#' that a route exists. Guarded by `TS_DRIVE_EXPORT_ROUTES`, so a module cannot
#' invent a route by publishing one — the table is the authority.
#'
#' @param global_data The app-wide `reactiveValues`.
#' @param module One of `TS_DRIVE_EXPORT_MODULES`.
#' @param exporter `function(shared_rv, global_data, dir)` -> list(ok, status,
#'   errors, descriptor). It MUST return a verdict rather than throw, for the
#'   reason `run_spatial_import()` gives.
ts_drive_publish_export <- function(global_data, module, exporter) {
  if (!is.character(module) || length(module) != 1L || is.na(module) ||
      !module %in% TS_DRIVE_EXPORT_MODULES) {
    warning(sprintf("ts_drive_publish_export(): '%s' has no export route in the frozen table (%s) - ignored.",
                    paste(as.character(module), collapse = ", "),
                    paste(TS_DRIVE_EXPORT_MODULES, collapse = ", ")))
    return(invisible(FALSE))
  }
  if (!is.function(exporter)) {
    warning("ts_drive_publish_export(): the exporter must be a function - ignored.")
    return(invisible(FALSE))
  }
  reg <- ts_drive_registry(global_data)
  if (is.null(reg)) return(invisible(FALSE))
  reg[[paste0(TS_DRIVE_EXPORT_PREFIX, module)]] <- exporter
  invisible(TRUE)
}

#' Read a module's published exporter, or NULL when none was published.
ts_drive_export_of <- function(global_data, module) {
  reg <- ts_drive_registry(global_data)
  if (is.null(reg)) return(NULL)
  fn <- reg[[paste0(TS_DRIVE_EXPORT_PREFIX, module)]]
  if (is.function(fn)) fn else NULL
}

#' Validate an `export_result` request. The accepted field set is EMPTY.
#'
#' This is the whole trust boundary of the slice, and it is a WHITELIST with
#' nothing on it plus an explicit refusal for every destination-shaped key. The
#' refusal list is not redundant with the whitelist — a whitelist alone silently
#' ignores a field the caller believed was honoured, and "I asked for
#' `filename: ../../x` and the tool said OK" is how an export ends up somewhere
#' nobody chose.
#'
#' @return list(ok, errors, request) where `request` is a REBUILT empty list.
ts_drive_validate_export_request <- function(req) {
  errors <- character(0)
  if (is.null(req)) return(list(ok = TRUE, errors = character(0), request = list()))
  if (!is.list(req)) {
    return(list(ok = FALSE,
                errors = sprintf("`export_result` takes no request fields; got a %s",
                                 class(req)[[1L]]),
                request = list()))
  }
  nms <- names(req)
  if (length(req) && (is.null(nms) || any(!nzchar(nms)))) {
    errors <- c(errors, "`export_result` takes no request fields; every key must be named")
  } else if (length(nms)) {
    # Every key is refused, with its own name in the message. `TS_DRIVE_EXPORT_FORBIDDEN`
    # is what makes the INTENT checkable by a test rather than only by reading this
    # loop: it is the declared list of shapes a caller reaches for when it tries to
    # choose a destination, and a regression that let one through is visible as a
    # constant that no longer covers the key.
    for (nm in nms) {
      hint <- if (tolower(nm) %in% TS_DRIVE_EXPORT_FORBIDDEN) " (a destination/format field)" else ""
      errors <- c(errors, sprintf(
        "`%s`%s is not accepted: `export_result` chooses the destination, the filename and the format itself",
        nm, hint))
    }
  }
  list(ok = !length(errors), errors = errors, request = list())
}

#' Drop the oldest exported files until at most `TS_DRIVE_EXPORT_MAX_FILES` remain.
#'
#' By modification time, and only among files this route created — the directory
#' is the app's, but the function refuses to delete anything it did not write.
ts_drive_export_prune <- function(dir = ts_drive_export_dir(),
                                 cap = TS_DRIVE_EXPORT_MAX_FILES) {
  if (!dir.exists(dir)) return(invisible(0L))
  f <- list.files(dir, full.names = TRUE)
  if (length(f) <= cap) return(invisible(0L))
  info <- file.info(f)
  ord <- order(info$mtime, decreasing = TRUE)
  victims <- f[ord[-seq_len(min(as.integer(cap), length(ord)))]]
  # Never remove anything that is not one of OUR files. The directory is the app's,
  # and a pruner that deletes what it did not write is a pruner that can delete an
  # operator's file.
  ours <- sprintf("^%s_[0-9]+\\.csv$", TS_DRIVE_EXPORT_STEM)
  victims <- victims[grepl(ours, basename(victims))]
  if (!length(victims)) return(invisible(0L))
  unlink(victims)
  invisible(length(victims))
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

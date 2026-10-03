# =============================================================================
# mod_bulk_de.R — Bulk Child 2: Design & Contrastes (Differential Expression)
# ORCHESTRATOR — Step-3.6 refactor
# =============================================================================
# The original ~1300-line monolithic mod_bulk_de.R has been split into
# modules/bulk_de/*.R for maintainability. This file is now ONLY:
#   1. mod_bulk_de_server() — wires the 6 sub-server functions together
#      inside ONE moduleServer() call.
#   2. Public UI functions are NOT re-declared here — mod_bulk_de_ui.R
#      (sourced alongside this file) defines them directly under their
#      original names, so mod_bulk.R's call sites (mod_bulk_de_ui(ns("de")),
#      mod_bulk_de_volcano_ui(ns("de")), etc.) do not change at all.
#
# Why plain functions and NOT nested Shiny modules: every sub-piece needs to
# read/write the SAME sidebar `input$...` (condition_col, group_ref,
# group_target, de_engine, lfc_thresh, padj_thresh, covariates, shrink_lfc —
# all defined once in mod_bulk_de_ui()'s single namespace). Nesting real
# Shiny modules here would mean re-namespacing all of that for no benefit;
# instead each `.de_*_server()` is a plain R function that receives
# input/output/session/ns/global_data/shared_rv explicitly and is called
# from within the SAME moduleServer(id, ...) body — same reactive semantics
# as the pre-refactor single file, just split across files. This is the
# standard pattern for splitting a large Shiny module (see e.g. golem's
# guidance on this exact situation).
#
# File map (all under modules/bulk_de/, sourced by app.R — order does not
# matter, R resolves function-to-function calls at call time, not source
# time, same rationale as the helpers_*.R sourcing note in global.R):
#
#   mod_bulk_de.R              — this file (orchestrator)
#   mod_bulk_de_ui.R           — every mod_bulk_de_*_ui() UI builder
#   mod_bulk_de_engine.R       — engine choices, design formula, readiness
#                                 check, contrast-selector sync, shared_rv
#                                 mirror, .de_make_helpers() factory
#                                 (design_str(), register_contrast())
#   mod_bulk_de_run.R          — Step 2 single-pair run_de + ad-hoc contrast
#   mod_bulk_de_pairwise.R     — pairwise-auto (>2 levels)
#   mod_bulk_de_viz.R          — Volcano / MA-Plot / Heatmap / Table DE tabs
#   mod_bulk_de_summary.R      — Resume Up/Down tab
#   mod_bulk_de_venn.R         — Venn/UpSet ACROSS CONTRASTS tab
#   mod_bulk_de_multimethod.R  — DESeq2/edgeR/limma comparison + consensus
#                                 (Step-3.6 FIX: multimethod_de/consensus now
#                                 live ONLY in shared_rv — see that file's
#                                 header for what was actually broken before)
#
# State contract (shared_rv) — unchanged from the pre-refactor single file;
# see each sub-file's header for exactly which piece reads/writes what.
#   READ  : shared_rv$filtered_counts, shared_rv$vst_mat
#   WRITE : shared_rv$dds_full, shared_rv$contrasts, shared_rv$active_contrast,
#           shared_rv$lfc_thresh, shared_rv$padj_thresh, shared_rv$heatmap_top_n,
#           shared_rv$heatmap_annot, shared_rv$active_condition_col,
#           shared_rv$volcano_role_colors, shared_rv$multimethod_de,
#           shared_rv$multimethod_consensus
# =============================================================================

mod_bulk_de_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Shared closures — single source of truth for design_str()/
    # register_contrast(), used by run/pairwise/multimethod.
    helpers <- .de_make_helpers(input, shared_rv)

    .de_engine_server(     input, output, session, ns, global_data, shared_rv)
    .de_run_server(        input, output, session, ns, global_data, shared_rv, helpers)
    .de_pairwise_server(   input, output, session, ns, global_data, shared_rv, helpers)
    .de_viz_server(        input, output, session, ns, global_data, shared_rv)
    .de_summary_server(    input, output, session, ns, global_data, shared_rv)
    .de_venn_server(       input, output, session, ns, global_data, shared_rv)
    .de_multimethod_server(input, output, session, ns, global_data, shared_rv, helpers)

    # S2b: publish THIS module's drive export route. Published as a closure over
    # this module's own `shared_rv`, and published at all rather than bound to a
    # button — the spatial_qc seam's reasons apply unchanged. The exporter itself
    # is a module-level function in mod_bulk_de_export.R (NOT a closure declared
    # here), so the offline tests can reach it; only the binding to this
    # module's store happens in here.
    ts_drive_publish_export(global_data, "bulk_de", function() {
      bulk_de_export_results_csv(shared_rv, global_data)
    })

  }) # /moduleServer
}

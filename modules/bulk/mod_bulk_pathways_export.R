# =============================================================================
# mod_bulk_pathways_export.R — S2c: the drive export route for the enrichment
# table
# =============================================================================
# The Bulk-PATHWAYS counterpart of `spatial_qc_export_hotspot_csv()` (S2) and
# `bulk_de_export_results_csv()` (S2b), deliberately their mirror: the module
# owns the knowledge of what its artefact IS, the protocol only knows that the
# route `bulk_pathways_enrichment_csv` exists (`TS_DRIVE_EXPORT_ROUTES`). The
# route is published by `mod_bulk_pathways_server()` as a closure over THIS
# module's own `shared_rv`, so nothing upstream can point the export at another
# state.
#
# WHAT THE ARTEFACT IS. The enrichment table `shared_rv$pathway_results` — the
# same object the human `dl_pathway` handler writes (mod_bulk_pathways.R:748).
# ONE route covers BOTH modes: the module stores one table for whichever of
# ORA / GSEA ran last, and the observer resets it to NULL before each run, so
# the table is never ambiguous about its own mode. The scores matrix
# (`shared_rv$pathway_scores`) and the GMT / RDS / network downloads are
# DIFFERENT artefacts for a human workflow, each of which would need its own
# frozen route — not smuggled in here.
#
# WHAT THE DESCRIPTOR CARRIES. format/file/bytes/n_rows/n_cols/columns — no
# `n_sig`: the sibling exporters' rule. A "significant pathway" count depends
# on `p.adjust < cutoff` (and, for ORA, on which genes reached the test at
# all), and a count whose definition the reader cannot know is the `done` lie
# this protocol exists to prevent — the agent counts from the table it just
# received.
#
# The shape guard pins the intersection of the two modes' column contracts
# (R/core/pathway_helpers.R: `ID`/`Description` from clusterProfiler, `pvalue`
# and `p.adjust` on every non-empty frame). GSEA-only columns (`setSize`,
# `NES`) and ORA-only columns (`GeneRatio`, `geneID`) are written as-is when
# present: the file carries whatever the stored table carries.
#
# Returns a VERDICT, never throws, for the reason `run_spatial_import()` gives.
# `status = "invalid"` means "this session has nothing to export";
# `status = "error"` means the write itself failed.
# =============================================================================

#' The next unused export path under `dir`, derived from what is already there.
#'
#' The `bulk_pathways` twin of `bulk_de_export_next_path()`:
#' `TS_DRIVE_EXPORT_STEM_BULK_PATHWAYS` plus an index one above the highest
#' present. Stateless on purpose — it cannot disagree with the directory after
#' a restart, a prune, or two exports in the same session.
bulk_pathways_export_next_path <- function(dir) {
  stem <- TS_DRIVE_EXPORT_STEM_BULK_PATHWAYS
  present <- list.files(dir, pattern = paste0("^", stem, "_[0-9]+\\.csv$"))
  idx <- suppressWarnings(as.integer(sub(paste0("^", stem, "_"), "",
                                         sub("\\.csv$", "", present))))
  idx <- idx[!is.na(idx)]
  n <- if (length(idx)) max(idx) + 1L else 1L
  file.path(dir, sprintf("%s_%d.csv", stem, n))
}

#' Export the enrichment table through the drive route.
#'
#' @param shared_rv The module's reactive values (read via `isolate()` — the
#'   poller calls this from outside any reactive context).
#' @param global_data The app-wide reactiveValues (unused today, kept for the
#'   exporter signature the seam documents).
#' @param dir Destination override — for TESTS only; the poller never passes it,
#'   and the route then writes into `ts_drive_export_dir()`.
#' @return list(ok, status, errors, descriptor)
bulk_pathways_export_enrichment_csv <- function(shared_rv, global_data, dir = NULL) {
  bad <- function(status, msg) {
    list(ok = FALSE, status = status, errors = msg, descriptor = NULL)
  }
  df <- tryCatch(
    shiny::isolate(shared_rv$pathway_results),
    error = function(e) NULL
  )
  if (is.null(df)) {
    return(bad("invalid",
      paste("this session has produced no enrichment result yet; run",
            "bulk-pathways-run_pathway (or the panel's own button) first")))
  }
  # The intersection of the ORA and GSEA column contracts (pathway_helpers.R):
  # every non-empty stored frame carries these four. GSEA aliases `Count` and
  # `GeneRatio` in; ORA gets them from clusterProfiler directly — not pinned,
  # because the route exports what the session produced, not one mode's schema.
  if (!is.data.frame(df) || !nrow(df) ||
      !all(c("ID", "Description", "pvalue", "p.adjust") %in% names(df))) {
    return(bad("invalid", "the stored enrichment result is not a pathway results table"))
  }
  ts_drive_export_write_table(df, bulk_pathways_export_next_path, dir)
}

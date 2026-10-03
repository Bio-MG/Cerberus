# =============================================================================
# mod_sc_pathways_export.R — S2c: the drive export route for the SC enrichment
# table
# =============================================================================
# The SC-PATHWAYS counterpart of the bulk_pathways exporter, deliberately its
# mirror: the module owns the knowledge of what its artefact IS, the protocol
# only knows that the route `sc_pathways_enrichment_csv` exists
# (`TS_DRIVE_EXPORT_ROUTES`). The route is published by
# `mod_sc_pathways_server()` as a closure over the session's own `shared_rv`.
#
# WHAT THE ARTEFACT IS. `shared_rv$pathway_results` of the SC shared state —
# the same object the human `dl_pathway` handler writes
# (mod_sc_pathways.R). Written by BOTH this module's button AND the drive run
# path (`run_sc_pathways_for_drive`, same file), so the store is the single
# source of truth either way. SC runs ORA only (`run_pathway_enrichment`); the
# bulk module has its own shared state and its own route, so the two
# identically-named fields never meet.
#
# WHAT THE DESCRIPTOR CARRIES. format/file/bytes/n_rows/n_cols/columns — no
# `n_sig` (the sibling exporters' rule: a cutoff-dependent count).
#
# The human filename embeds `input$pathway_db` (GOBP / KEGG / Reactome) — a
# UI choice a drive run itself selects. The drive filename does NOT, on the
# S2 rule: the stem is declared data (`TS_DRIVE_EXPORT_STEM_SC_PATHWAYS`).
#
# The shape guard pins the intersection of the enrichment column contracts
# (R/core/pathway_helpers.R): ID, Description, pvalue, p.adjust.
#
# Returns a VERDICT, never throws. `status = "invalid"` means "this session
# has nothing to export"; `status = "error"` means the write itself failed.
# =============================================================================

#' The next unused export path under `dir`, derived from what is already there.
#'
#' The `sc_pathways` twin of `bulk_pathways_export_next_path()`:
#' `TS_DRIVE_EXPORT_STEM_SC_PATHWAYS` plus an index one above the highest
#' present. Stateless on purpose.
sc_pathways_export_next_path <- function(dir) {
  stem <- TS_DRIVE_EXPORT_STEM_SC_PATHWAYS
  present <- list.files(dir, pattern = paste0("^", stem, "_[0-9]+\\.csv$"))
  idx <- suppressWarnings(as.integer(sub(paste0("^", stem, "_"), "",
                                         sub("\\.csv$", "", present))))
  idx <- idx[!is.na(idx)]
  n <- if (length(idx)) max(idx) + 1L else 1L
  file.path(dir, sprintf("%s_%d.csv", stem, n))
}

#' Export the SC enrichment table through the drive route.
#'
#' @param shared_rv The module's reactive values (read via `isolate()` — the
#'   poller calls this from outside any reactive context).
#' @param global_data The app-wide reactiveValues (unused today, kept for the
#'   exporter signature the seam documents).
#' @param dir Destination override — for TESTS only; the poller never passes it,
#'   and the route then writes into `ts_drive_export_dir()`.
#' @return list(ok, status, errors, descriptor)
sc_pathways_export_enrichment_csv <- function(shared_rv, global_data, dir = NULL) {
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
            "sc-pathways-run_pathway (or the panel's own button) first")))
  }
  if (!is.data.frame(df) || !nrow(df) ||
      !all(c("ID", "Description", "pvalue", "p.adjust") %in% names(df))) {
    return(bad("invalid", "the stored enrichment result is not a pathway results table"))
  }
  ts_drive_export_write_table(df, sc_pathways_export_next_path, dir)
}

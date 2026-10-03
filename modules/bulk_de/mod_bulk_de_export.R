# =============================================================================
# mod_bulk_de_export.R — S2b: the drive export route for the DE results table
# =============================================================================
# The Bulk-DE counterpart of `spatial_qc_export_hotspot_csv()` (S2), and
# deliberately its mirror: the module owns the knowledge of what its artefact IS,
# the protocol only knows that the route `bulk_de_results_csv` exists
# (`TS_DRIVE_EXPORT_ROUTES`). The route is published by `mod_bulk_de_server()`
# as a closure over THIS module's own `shared_rv`, so nothing upstream — a
# scenario field, a second module, a different `shared_rv` — can point the
# export at another state.
#
# WHAT THE ARTEFACT IS. The ACTIVE contrast's DE table
# (`shared_rv$contrasts[[shared_rv$active_contrast]]`), the same object the
# human `dl_de_csv` handler writes (mod_bulk_de_viz.R). One route, one table:
# the Venn/multimethod/summary downloads are different artefacts for a human
# workflow, and each would need its own frozen route — not smuggled in here.
#
# WHAT THE DESCRIPTOR CARRIES, AND WHAT IT DELIBERATELY DOES NOT. The wire
# projection (`ts_drive_export_descriptor`) declares format/file/bytes/n_rows/
# n_cols/n_sig/columns. This exporter fills all but `n_sig`: for the hotspot
# table that field is a threshold-free count of the app's own calls, while a DE
# "significant" count DEPENDS on a threshold (`padj_thresh`, `lfc_thresh`) the
# caller does not see. A count whose definition the reader cannot know is the
# `done` lie this protocol exists to prevent — the agent counts from the table
# it just received instead.
#
# The human filename embeds `shared_rv$active_contrast` — a caller-choosable
# label. The drive filename does NOT, on the S2 rule: a user annotation must not
# reach a remote caller through a filename.
#
# Returns a VERDICT, never throws, for the reason `run_spatial_import()` gives.
# `status = "invalid"` means "this session has nothing to export";
# `status = "error"` means the write itself failed.
# =============================================================================

#' The next unused export path under `dir`, derived from what is already there.
#'
#' The `bulk_de` twin of `spatial_qc_export_next_path()`: `TS_DRIVE_EXPORT_STEM_BULK_DE`
#' plus an index one above the highest present. Deriving the index rather than
#' keeping a counter means the function is stateless, so it cannot disagree with
#' the directory after a restart, a prune, or two exports in the same session.
bulk_de_export_next_path <- function(dir) {
  stem <- TS_DRIVE_EXPORT_STEM_BULK_DE
  present <- list.files(dir, pattern = paste0("^", stem, "_[0-9]+\\.csv$"))
  idx <- suppressWarnings(as.integer(sub(paste0("^", stem, "_"), "",
                                         sub("\\.csv$", "", present))))
  idx <- idx[!is.na(idx)]
  n <- if (length(idx)) max(idx) + 1L else 1L
  file.path(dir, sprintf("%s_%d.csv", stem, n))
}

#' Export the ACTIVE contrast's DE table through the drive route.
#'
#' @param shared_rv The module's reactive values (read via `isolate()` — the
#'   poller calls this from outside any reactive context).
#' @param global_data The app-wide reactiveValues (unused today, kept for the
#'   exporter signature the seam documents).
#' @param dir Destination override — for TESTS only; the poller never passes it,
#'   and the route then writes into `ts_drive_export_dir()`.
#' @return list(ok, status, errors, descriptor)
bulk_de_export_results_csv <- function(shared_rv, global_data, dir = NULL) {
  bad <- function(status, msg) {
    list(ok = FALSE, status = status, errors = msg, descriptor = NULL)
  }
  df <- tryCatch(
    shiny::isolate(shared_rv$contrasts[[shared_rv$active_contrast]]),
    error = function(e) NULL
  )
  if (is.null(df)) {
    return(bad("invalid",
      paste("this session has produced no DE result yet; run bulk-de-run_de",
            "(or the panel's own button) first")))
  }
  # The shape `.normalize_de_cols()` guarantees for every registered contrast.
  # The single writer's guard normally makes this unreachable; the export is the
  # one place that must not hand a reader-breaking value to a FILE either.
  if (!is.data.frame(df) || !nrow(df) ||
      !all(c("gene", "log2FoldChange", "pvalue", "padj") %in% names(df))) {
    return(bad("invalid", "the stored DE result is not a normalised contrasts table"))
  }
  ts_drive_export_write_table(df, bulk_de_export_next_path, dir)
}

# =============================================================================
# mod_sc_markers_export.R — S2c: the drive export route for the SC marker table
# =============================================================================
# The SC-MARKERS counterpart of the spatial_qc / bulk_de / bulk_pathways
# exporters, deliberately their mirror: the module owns the knowledge of what
# its artefact IS, the protocol only knows that the route `sc_markers_table_csv`
# exists (`TS_DRIVE_EXPORT_ROUTES`). The route is published by
# `mod_sc_markers_server()` as a closure over the session's own `shared_rv`.
#
# WHAT THE ARTEFACT IS. `shared_rv$markers_data` — the same object the human
# `dl_markers_csv` handler writes (mod_sc_markers.R:398), and the SAME STORE
# every producer writes: this module's own button (mod_sc_markers.R:267) AND
# the SC auto-pipeline (mod_sc.R:461). Reading the shared store rather than the
# module's local `markers_rv` mirror is deliberate: the mirror only exists so
# the panel can observe resets, and the shared store is the single source of
# truth the pathway module also consumes.
#
# WHAT THE DESCRIPTOR CARRIES. format/file/bytes/n_rows/n_cols/columns — no
# `n_sig` (the sibling exporters' rule: a "significant marker" depends on a
# `p_val_adj` cutoff the caller does not see; FindAllMarkers already applied
# its own thresholds when the table was BUILT, so the rows are what they are).
#
# The shape guard pins the four identity columns `.normalize_marker_cols()`
# guarantees for every stored frame (mod_sc_markers.R:31): gene, cluster,
# avg_log2FC, p_val_adj. The pct columns ride along when present.
#
# Returns a VERDICT, never throws. `status = "invalid"` means "this session
# has nothing to export"; `status = "error"` means the write itself failed.
# =============================================================================

#' The next unused export path under `dir`, derived from what is already there.
#'
#' The `sc_markers` twin of `bulk_de_export_next_path()`:
#' `TS_DRIVE_EXPORT_STEM_SC_MARKERS` plus an index one above the highest
#' present. Stateless on purpose.
sc_markers_export_next_path <- function(dir) {
  stem <- TS_DRIVE_EXPORT_STEM_SC_MARKERS
  present <- list.files(dir, pattern = paste0("^", stem, "_[0-9]+\\.csv$"))
  idx <- suppressWarnings(as.integer(sub(paste0("^", stem, "_"), "",
                                         sub("\\.csv$", "", present))))
  idx <- idx[!is.na(idx)]
  n <- if (length(idx)) max(idx) + 1L else 1L
  file.path(dir, sprintf("%s_%d.csv", stem, n))
}

#' Export the marker table through the drive route.
#'
#' @param shared_rv The module's reactive values (read via `isolate()` — the
#'   poller calls this from outside any reactive context).
#' @param global_data The app-wide reactiveValues (unused today, kept for the
#'   exporter signature the seam documents).
#' @param dir Destination override — for TESTS only; the poller never passes it,
#'   and the route then writes into `ts_drive_export_dir()`.
#' @return list(ok, status, errors, descriptor)
sc_markers_export_table_csv <- function(shared_rv, global_data, dir = NULL) {
  bad <- function(status, msg) {
    list(ok = FALSE, status = status, errors = msg, descriptor = NULL)
  }
  df <- tryCatch(
    shiny::isolate(shared_rv$markers_data),
    error = function(e) NULL
  )
  if (is.null(df)) {
    return(bad("invalid",
      paste("this session has produced no marker table yet; run",
            "sc-markers-run_markers (or the SC auto-pipeline) first")))
  }
  # The columns .normalize_marker_cols() guarantees for every stored frame.
  # Both producers normalise before storing, so this guard is normally
  # unreachable — the export is the one place that must not hand a
  # reader-breaking value to a FILE either.
  if (!is.data.frame(df) || !nrow(df) ||
      !all(c("gene", "cluster", "avg_log2FC", "p_val_adj") %in% names(df))) {
    return(bad("invalid", "the stored marker result is not a normalised marker table"))
  }
  ts_drive_export_write_table(df, sc_markers_export_next_path, dir)
}

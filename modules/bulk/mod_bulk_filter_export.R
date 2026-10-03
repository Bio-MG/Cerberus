# =============================================================================
# mod_bulk_filter_export.R — Slice 2.3: the drive export route for the VST
# matrix
# =============================================================================
# The Bulk-FILTER counterpart of the other export route exporters. ONE route
# for ONE artefact: `shared_rv$vst_mat`, the VST-transformed matrix BOTH store
# paths write (Step 1 filter+normalize, mod_bulk_filter.R:359, and the
# ComBat-seq batch correction, :730).
#
# DELIBERATE DIVERGENCE, recorded: every other route mirrors a downloadHandler
# the panel already offers. bulk_filter offers NO CSV download of its matrices
# (only PCA/scree PNGs and a variance-partition table), so this route is a NEW
# artefact on the wire, not a mirror. It is the right artefact anyway: the VST
# matrix is what every downstream Bulk module consumes, and the file is
# written `gene`-first so `import_bulk` can read it straight back — the
# export/import round-trip is a design goal here, not an accident.
#
# SHAPE. The file is `gene` + one column per sample (sample names preserved
# verbatim — `check.names = FALSE`; a sample literally named `gene` would
# produce a duplicate header, i.e. a reader-breaking file, and is REFUSED).
# The descriptor therefore guarantees only the `gene` column; the sample
# columns are the session's own (`TS_DRIVE_EXPORT_COLUMNS$bulk_filter`).
#
# The matrix can be large (genes x samples — a 17k x 18 dataset writes a
# ~15 MB CSV). That is a property of the dataset the operator chose, bounded
# by TS_DRIVE_EXPORT_MAX_FILES retention, and the write happens inside the
# same poller beat as the multi-second analyses the protocol already runs.
#
# Returns a VERDICT, never throws. `status = "invalid"` means "this session
# has nothing to export"; `status = "error"` means the write itself failed.
# =============================================================================

#' The next unused export path under `dir`, derived from what is already there.
#'
#' The `bulk_filter` twin of `bulk_de_export_next_path()`:
#' `TS_DRIVE_EXPORT_STEM_BULK_FILTER` plus an index one above the highest
#' present. Stateless on purpose.
bulk_filter_export_next_path <- function(dir) {
  stem <- TS_DRIVE_EXPORT_STEM_BULK_FILTER
  present <- list.files(dir, pattern = paste0("^", stem, "_[0-9]+\\.csv$"))
  idx <- suppressWarnings(as.integer(sub(paste0("^", stem, "_"), "",
                                         sub("\\.csv$", "", present))))
  idx <- idx[!is.na(idx)]
  n <- if (length(idx)) max(idx) + 1L else 1L
  file.path(dir, sprintf("%s_%d.csv", stem, n))
}

#' Export the VST matrix through the drive route.
#'
#' @param shared_rv The module's reactive values (read via `isolate()` — the
#'   poller calls this from outside any reactive context).
#' @param global_data The app-wide reactiveValues (unused today, kept for the
#'   exporter signature the seam documents).
#' @param dir Destination override — for TESTS only; the poller never passes it,
#'   and the route then writes into `ts_drive_export_dir()`.
#' @return list(ok, status, errors, descriptor)
bulk_filter_export_vst_matrix_csv <- function(shared_rv, global_data, dir = NULL) {
  bad <- function(status, msg) {
    list(ok = FALSE, status = status, errors = msg, descriptor = NULL)
  }
  m <- tryCatch(
    shiny::isolate(shared_rv$vst_mat),
    error = function(e) NULL
  )
  if (is.null(m)) {
    return(bad("invalid",
      paste("this session has produced no VST matrix yet; run",
            "bulk-filter-run_filter_norm (or the panel's own button) first")))
  }
  # A matrix, genes x samples, with names — both store paths build exactly
  # this; the export must not hand a reader-breaking value to a FILE.
  if (!is.matrix(m) || !nrow(m) || !ncol(m) || is.null(rownames(m)) ||
      is.null(colnames(m))) {
    return(bad("invalid", "the stored VST result is not a named genes x samples matrix"))
  }
  # A sample named `gene` would duplicate the first column's header: a file no
  # reader (including import_bulk) can parse unambiguously. Refused, honestly.
  if ("gene" %in% colnames(m)) {
    return(bad("invalid",
      "a sample is named 'gene'; the matrix file would carry a duplicate header"))
  }
  df <- cbind(gene = rownames(m), as.data.frame(m, check.names = FALSE))
  ts_drive_export_write_table(df, bulk_filter_export_next_path, dir)
}

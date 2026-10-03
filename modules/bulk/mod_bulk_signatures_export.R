# =============================================================================
# mod_bulk_signatures_export.R — Slice 2.3: the drive export route for the
# signature-scores table
# =============================================================================
# ONE route for ONE artefact: the long signature x sample scores grid the
# human `dl_sig_csv` handler writes (mod_bulk_signatures.R) via
# `build_signature_scores_export()` (R/bulk/bulk_signatures.R). The exporter
# calls the SAME builder — one source of truth for the schema, including the
# §M3 disclaimer column that must travel with the export. The RDS download
# (the full result object with QC) is a DIFFERENT artefact and gets no route.
#
# The builder is the authority on what is canonical: a non-canonical stored
# object makes the builder stop() with a classified error, and this exporter
# maps that to an honest `invalid` VERDICT rather than throwing.
#
# Returns a VERDICT, never throws. `status = "invalid"` means "this session
# has nothing to export"; `status = "error"` means the write itself failed.
# =============================================================================

#' The next unused export path under `dir`, derived from what is already there.
#'
#' The `bulk_signatures` twin of `bulk_de_export_next_path()`:
#' `TS_DRIVE_EXPORT_STEM_BULK_SIGNATURES` plus an index one above the highest
#' present. Stateless on purpose.
bulk_signatures_export_next_path <- function(dir) {
  stem <- TS_DRIVE_EXPORT_STEM_BULK_SIGNATURES
  present <- list.files(dir, pattern = paste0("^", stem, "_[0-9]+\\.csv$"))
  idx <- suppressWarnings(as.integer(sub(paste0("^", stem, "_"), "",
                                         sub("\\.csv$", "", present))))
  idx <- idx[!is.na(idx)]
  n <- if (length(idx)) max(idx) + 1L else 1L
  file.path(dir, sprintf("%s_%d.csv", stem, n))
}

#' Export the signature-scores table through the drive route.
#'
#' @param shared_rv The module's reactive values (read via `isolate()` — the
#'   poller calls this from outside any reactive context).
#' @param global_data The app-wide reactiveValues (unused today, kept for the
#'   exporter signature the seam documents).
#' @param dir Destination override — for TESTS only; the poller never passes it,
#'   and the route then writes into `ts_drive_export_dir()`.
#' @return list(ok, status, errors, descriptor)
bulk_signatures_export_scores_csv <- function(shared_rv, global_data, dir = NULL) {
  bad <- function(status, msg) {
    list(ok = FALSE, status = status, errors = msg, descriptor = NULL)
  }
  res <- tryCatch(
    shiny::isolate(shared_rv$signature_scores),
    error = function(e) NULL
  )
  if (is.null(res)) {
    return(bad("invalid",
      paste("this session has produced no signature scores yet; run",
            "bulk-signatures-run_signatures (or the panel's own button) first")))
  }
  # The builder owns the canonical-shape verdict; a stored non-canonical
  # object arrives here as a classified error and becomes an honest refusal.
  df <- tryCatch(build_signature_scores_export(res), error = function(e) {
    bad("invalid", paste("the stored signature result is not canonical:",
                         conditionMessage(e)))
  })
  if (!is.data.frame(df)) return(df) # the mapped `invalid` verdict
  ts_drive_export_write_table(df, bulk_signatures_export_next_path, dir)
}

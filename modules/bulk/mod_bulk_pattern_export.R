# =============================================================================
# mod_bulk_pattern_export.R — Slice 2.3: the drive export route for the
# pattern-cluster table
# =============================================================================
# ONE route for ONE artefact: the gene/cluster table the human `dl_pattern`
# handler writes (mod_bulk_pattern.R) via `build_pattern_table_export()`
# (R/bulk/bulk_pattern.R). The exporter calls the SAME builder, and the
# builder's own `assert_bulk_pattern_result()` is the canonical-shape verdict
# — a stored non-canonical object becomes an honest `invalid` VERDICT.
#
# The human filename embeds `shared_rv$pattern_result$k` — a session value a
# drive run itself selects. The drive filename does NOT, on the S2 rule: the
# stem is declared data (`TS_DRIVE_EXPORT_STEM_BULK_PATTERN`).
#
# Returns a VERDICT, never throws. `status = "invalid"` means "this session
# has nothing to export"; `status = "error"` means the write itself failed.
# =============================================================================

#' The next unused export path under `dir`, derived from what is already there.
#'
#' The `bulk_pattern` twin of `bulk_de_export_next_path()`:
#' `TS_DRIVE_EXPORT_STEM_BULK_PATTERN` plus an index one above the highest
#' present. Stateless on purpose.
bulk_pattern_export_next_path <- function(dir) {
  stem <- TS_DRIVE_EXPORT_STEM_BULK_PATTERN
  present <- list.files(dir, pattern = paste0("^", stem, "_[0-9]+\\.csv$"))
  idx <- suppressWarnings(as.integer(sub(paste0("^", stem, "_"), "",
                                         sub("\\.csv$", "", present))))
  idx <- idx[!is.na(idx)]
  n <- if (length(idx)) max(idx) + 1L else 1L
  file.path(dir, sprintf("%s_%d.csv", stem, n))
}

#' Export the pattern-cluster table through the drive route.
#'
#' @param shared_rv The module's reactive values (read via `isolate()` — the
#'   poller calls this from outside any reactive context).
#' @param global_data The app-wide reactiveValues (unused today, kept for the
#'   exporter signature the seam documents).
#' @param dir Destination override — for TESTS only; the poller never passes it,
#'   and the route then writes into `ts_drive_export_dir()`.
#' @return list(ok, status, errors, descriptor)
bulk_pattern_export_clusters_csv <- function(shared_rv, global_data, dir = NULL) {
  bad <- function(status, msg) {
    list(ok = FALSE, status = status, errors = msg, descriptor = NULL)
  }
  res <- tryCatch(
    shiny::isolate(shared_rv$pattern_result),
    error = function(e) NULL
  )
  if (is.null(res)) {
    return(bad("invalid",
      paste("this session has produced no pattern clusters yet; run",
            "bulk-pattern-run_pattern (or the panel's own button) first")))
  }
  # The builder owns the canonical-shape verdict (assert_bulk_pattern_result).
  df <- tryCatch(build_pattern_table_export(res), error = function(e) {
    bad("invalid", paste("the stored pattern result is not canonical:",
                         conditionMessage(e)))
  })
  if (!is.data.frame(df)) return(df) # the mapped `invalid` verdict
  ts_drive_export_write_table(df, bulk_pattern_export_next_path, dir)
}

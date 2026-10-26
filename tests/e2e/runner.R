#!/usr/bin/env Rscript
# =============================================================================
# runner.R — Headless E2E Test Suite Runner for TranscriptoShiny ("Cerberus")
# =============================================================================
# Executes the comprehensive opaque-box E2E test suite across Tiers 1-4:
#   Tier 1: Feature Coverage (Features 1-24: M1, M2, M3, M4)
#   Tier 2: Boundary & Corner Cases (Manifest, Matrix, Metadata, Species, IPC)
#   Tier 3: Cross-Feature Interactions (Handoff -> QC -> Pipeline -> Export)
#   Tier 4: Real-World Application Scenarios (PBMC 10x, CCC, Multi-sample)
#
# Usage:
#   Rscript tests/e2e/runner.R [--tier=1|2|3|4|all] [--verbose] [--help]
#
# Exit code:
#   0 : All executed tests passed (or cleanly skipped under progressive testability)
#   1 : One or more tests failed
# =============================================================================

# Disable warnings truncation
options(warn = 1)

# Locate project root and source shared E2E helper
.find_root <- function(start = getwd(), marker = "app.R", max_up = 8L) {
  d <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in seq_len(max_up)) {
    if (file.exists(file.path(d, marker))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  stop("Could not locate project root from '", start, "'", call. = FALSE)
}

project_root <- .find_root()
setwd(project_root)

helper_path <- file.path(project_root, "tests", "e2e", "helper-e2e.R")
if (!file.exists(helper_path)) {
  stop("helper-e2e.R not found at ", helper_path, call. = FALSE)
}
source(helper_path)

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Package 'testthat' is required to execute E2E test suite.", call. = FALSE)
}

# ── Parse Command Line Arguments ─────────────────────────────────────────────

args <- commandArgs(trailingOnly = TRUE)
tier_filter <- "all"
verbose <- FALSE

for (arg in args) {
  if (grepl("^--tier=", arg)) {
    tier_filter <- sub("^--tier=", "", arg)
  } else if (arg == "--verbose" || arg == "-v") {
    verbose <- TRUE
  } else if (arg == "--help" || arg == "-h") {
    cat("Usage: Rscript tests/e2e/runner.R [options]\n\n")
    cat("Options:\n")
    cat("  --tier=<1|2|3|4|all>   Filter execution to a specific test tier (default: all)\n")
    cat("  --verbose, -v          Display detailed individual test results\n")
    cat("  --help, -h             Display this help message and exit\n")
    quit(status = 0, save = "no")
  }
}

cat("==============================================================================\n")
cat(" TranscriptoShiny ('Cerberus') — Opaque-Box E2E Test Suite Runner\n")
cat(" R version: ", R.version.string, "\n")
cat(" Project root: ", project_root, "\n")
cat(" Active tier selection: ", tier_filter, "\n")
cat(" Timestamp: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "\n")
cat("==============================================================================\n\n")

# ── Define Tiers and Suites ──────────────────────────────────────────────────

tier_defs <- list(
  tier1 = list(
    name = "Tier 1: Feature Coverage Tests",
    dir = file.path(project_root, "tests", "e2e", "tier1_features"),
    description = "Features 1-24 across Milestones M1, M2, M3, M4 (>=5 tests per feature)"
  ),
  tier2 = list(
    name = "Tier 2: Boundary & Corner Cases",
    dir = file.path(project_root, "tests", "e2e", "tier2_boundaries"),
    description = "Boundary, edge conditions, numerical stress, and invalid combinations"
  ),
  tier3 = list(
    name = "Tier 3: Cross-Feature Interactions",
    dir = file.path(project_root, "tests", "e2e", "tier3_cross_feature"),
    description = "Pairwise handoff, QC filtering, async execution, and export routes"
  ),
  tier4 = list(
    name = "Tier 4: Real-World Scenarios",
    dir = file.path(project_root, "tests", "e2e", "tier4_real_world"),
    description = "Multi-step end-to-end biological workloads (PBMC 10x, CCC, Cohort)"
  )
)

selected_tiers <- if (tier_filter == "all") {
  names(tier_defs)
} else {
  matched <- paste0("tier", tier_filter)
  if (!matched %in% names(tier_defs)) {
    stop("Invalid tier '", tier_filter, "'. Options: 1, 2, 3, 4, all", call. = FALSE)
  }
  matched
}

# ── Execute Suites & Aggregate Metrics ───────────────────────────────────────

results_summary <- list()
any_failure <- FALSE
total_passed <- 0L
total_failed <- 0L
total_skipped <- 0L
total_warnings <- 0L

start_time <- Sys.time()

for (tier_key in selected_tiers) {
  td <- tier_defs[[tier_key]]
  cat(sprintf("[EXEC] %s\n", td$name))
  cat(sprintf("       %s\n", td$description))
  
  if (!dir.exists(td$dir)) {
    cat(sprintf("       Directory not found: %s\n\n", td$dir))
    next
  }
  
  test_files <- list.files(td$dir, pattern = "^test-.*\\.R$", full.names = TRUE)
  if (length(test_files) == 0L) {
    cat(sprintf("       No test files found in %s\n\n", td$dir))
    next
  }
  
  tier_passed <- 0L
  tier_failed <- 0L
  tier_skipped <- 0L
  tier_warnings <- 0L
  tier_file_count <- length(test_files)
  
  for (tf in test_files) {
    base_name <- basename(tf)
    res <- tryCatch(
      testthat::test_file(tf, reporter = if (isTRUE(verbose)) "progress" else "silent"),
      error = function(e) {
        cat(sprintf("  [ERROR] Execution failed for %s: %s\n", base_name, conditionMessage(e)))
        NULL
      }
    )
    
    if (is.null(res)) {
      tier_failed <- tier_failed + 1L
      any_failure <- TRUE
      next
    }
    
    df <- as.data.frame(res)
    n_pass <- sum(df$passed, na.rm = TRUE)
    n_fail <- sum(df$failed, na.rm = TRUE) + sum(df$error, na.rm = TRUE)
    n_skip <- sum(as.integer(df$skipped), na.rm = TRUE)
    n_warn <- sum(df$warning, na.rm = TRUE)
    
    tier_passed <- tier_passed + n_pass
    tier_failed <- tier_failed + n_fail
    tier_skipped <- tier_skipped + n_skip
    tier_warnings <- tier_warnings + n_warn
    
    status_icon <- if (n_fail == 0L) "PASS" else "FAIL"
    cat(sprintf("  [%s] %-42s (Pass: %3d | Skip: %2d | Fail: %2d)\n",
                status_icon, base_name, n_pass, n_skip, n_fail))
    
    if (n_fail > 0L) {
      any_failure <- TRUE
    }
  }
  
  total_passed <- total_passed + tier_passed
  total_failed <- total_failed + tier_failed
  total_skipped <- total_skipped + tier_skipped
  total_warnings <- total_warnings + tier_warnings
  
  results_summary[[tier_key]] <- list(
    name = td$name,
    files = tier_file_count,
    passed = tier_passed,
    failed = tier_failed,
    skipped = tier_skipped,
    warnings = tier_warnings
  )
  cat("\n")
}

elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2)

# ── Print Structured Results Table ──────────────────────────────────────────

cat("==============================================================================\n")
cat(" E2E TEST EXECUTION SUMMARY\n")
cat("==============================================================================\n")
cat(sprintf("%-35s | Files | Passed | Skipped | Failed | Status\n", "Test Tier"))
cat("------------------------------------+-------+--------+---------+--------+---------\n")

for (tier_key in names(results_summary)) {
  rs <- results_summary[[tier_key]]
  tier_status <- if (rs$failed == 0L) "PASS" else "FAIL"
  cat(sprintf("%-35s |   %3d |   %4d |    %4d |   %4d | %s\n",
              rs$name, rs$files, rs$passed, rs$skipped, rs$failed, tier_status))
}

cat("------------------------------------+-------+--------+---------+--------+---------\n")
total_status <- if (total_failed == 0L) "PASS" else "FAIL"
cat(sprintf("%-35s |   %3d |   %4d |    %4d |   %4d | %s\n",
            "GRAND TOTAL",
            sum(vapply(results_summary, function(x) x$files, integer(1))),
            total_passed, total_skipped, total_failed, total_status))
cat("==============================================================================\n")
cat(sprintf("Total Elapsed Time : %s seconds\n", elapsed))
cat(sprintf("Final Verdict      : %s\n", if (!any_failure) "ALL TIERS PASSED" else "TEST FAILURES DETECTED"))
cat("==============================================================================\n\n")

# ── Exit Code Resolution ─────────────────────────────────────────────────────

if (any_failure || total_failed > 0L) {
  quit(status = 1, save = "no")
} else {
  quit(status = 0, save = "no")
}

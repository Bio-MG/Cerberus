# Reusable targeted test runner — Rscript tools/run_tests.R <filter> [<filter2> ...]
#
# Locale UTF-8 BEFORE any parse(): Git Bash exports LC_ALL=C.UTF-8, a name R
# does NOT recognise on Windows, so R silently falls back to "C", where it
# cannot parse some UTF-8 sources of the repo (STATUS.md, "Garde C13 +
# semantique testServer() MESUREE": "unexpected invalid token" on
# R/sc/sc_communication_perturbation.R). Not fatal if the locale is missing,
# but the warning must stay visible: a wrong locale yields FALSE failures.
.localectl <- Sys.setlocale("LC_CTYPE", "fr_FR.UTF-8")
if (!nzchar(.localectl)) {
  warning("LC_CTYPE fr_FR.UTF-8 unavailable: parse() false failures are likely")
}
args <- commandArgs(trailingOnly = TRUE)
# Optional --test-dir=<path>: run against another directory of test files
# (used by tests/testthat/test-runner-exit-codes.R to probe this runner's
# exit code on fixture dirs without touching tests/testthat/). Backward
# compatible: without the flag the behaviour is exactly as before. Extracted
# BEFORE the filters so it is never mistaken for a filter regex.
dir_args <- grep("^--test-dir=", args, value = TRUE)
args <- setdiff(args, dir_args)
tests_dir <- if (length(dir_args)) sub("^--test-dir=", "", dir_args[1]) else "tests/testthat"
# No-argument default is the "bulk" SUBSET, never the full suite (policy:
# targeted tests by default; the full suite is tools/run_full_suite.R). The
# banner and the labelled TOTAL below exist so a green subset can no longer be
# mistaken for a full-suite verdict (audit 2026-10-01, section H.3).
if (length(args) == 0L) {
  cat("== ATTENTION : filtre implicite 'bulk' — verdict de SOUS-ENSEMBLE,\n")
  cat("== PAS la suite complete. Suite complete : tools/run_full_suite.R ==\n")
}
filter <- if (length(args) >= 1L) args else "bulk"
# testthat::test_dir(filter=) takes ONE regex, not a vector: given a character
# vector of length > 1 it silently uses only the FIRST element. Measured
# 2026-09-16: asking for c("core-jobs", "da-milo-async") ran only
# test-core-jobs.R and still printed a green TOTAL -- the requested file was
# silently not run. Collapse into an alternation so every filter is honoured.
filter <- paste(filter, collapse = "|")
options(testthat.progress.max_fails = 100)
res <- testthat::test_dir(tests_dir, filter = filter, reporter = "silent",
                          stop_on_failure = FALSE)
df <- as.data.frame(res)
cat("== filter:", paste(filter, collapse = ","), "==\n")
for (i in seq_len(nrow(df))) {
  cat(sprintf("  %-55s fail=%d pass=%d skip=%d warn=%d\n",
              df$file[i], df$failed[i], df$passed[i], df$skipped[i],
              if ("warning" %in% colnames(df)) df$warning[i] else 0))
}
cat(sprintf("TOTAL (filter=%s, files=%d%s) failed=%d passed=%d error=%d skipped=%d\n",
            filter, length(unique(df$file)),
            if (length(args) == 0L) ", SUBSET — NOT the full suite" else "",
            sum(df$failed), sum(df$passed), sum(df$error), sum(df$skipped)))
if (length(args) == 0L) {
  cat("== Ceci est un sous-ensemble : tools/run_full_suite.R pour le verdict complet ==\n")
}
if (sum(df$failed) > 0L || sum(df$error) > 0L) {
  # Re-run with a verbose reporter to show the actual failures.
  cat("\n---- FAILURES ----\n")
  testthat::test_dir(tests_dir, filter = filter,
                     reporter = "summary", stop_on_failure = FALSE)
}
# Exit signaling (audit 2026-10-01, R4): previously ended in invisible(NULL),
# so this runner ALWAYS exited 0 — even on failures — and the CI
# "Sous-suite ciblee" step (which relies on this exit code) could never turn
# red. tools/run_full_suite.R does NOT shell out to this script (it calls
# testthat::test_file itself, verdict = its own BILAN line), so this non-zero
# exit cannot abort the full suite.
quit_status <- if (sum(df$failed) > 0L || sum(df$error) > 0L) 1L else 0L
quit(save = "no", status = quit_status)

# tools/build_census.R — machine-emitted census of the test files (audit
# 2026-10-01, R1). Writes docs/audits/census.txt (untracked per docs/STATUS.md
# §7: docs/ is machine-local). NEVER hand-edit the output: regenerate it with
#   LC_ALL=French_France.65001 Rscript --vanilla tools/build_census.R
# (run from the repo root).
#
# Pattern duplication, on purpose: the one-line list.files() pattern below is
# DUPLICATED from tools/run_full_suite.R (its R2 census emission) instead of
# being shared via a sourced helper, so that run_full_suite.R stays free of
# source() dependencies (crash-resilience rationale in its header). If you
# change the pattern in one file, you MUST change it in the other.
#
# Output is deterministic: sorted filenames, a total count, NO timestamps and
# NO locale-dependent values, so diffs of census.txt stay noise-free.

.localectl <- Sys.setlocale("LC_CTYPE", "fr_FR.UTF-8")
if (!nzchar(.localectl)) {
  warning("LC_CTYPE fr_FR.UTF-8 unavailable: parse() false failures are likely")
}

# --- DUPLICATED PATTERN — keep in sync with tools/run_full_suite.R (R2) ------
files <- sort(list.files("tests/testthat", pattern = "^test-.*\\.R$",
                         full.names = TRUE))
# -----------------------------------------------------------------------------

if (!dir.exists("docs/audits")) dir.create("docs/audits", recursive = TRUE)
con <- file("docs/audits/census.txt", open = "wt")
writeLines("# Census of tests/testthat/test-*.R — machine-emitted by tools/build_census.R. DO NOT HAND-EDIT.", con)
writeLines("CENSUS: begin", con)
for (bn in basename(files)) writeLines(paste0("CENSUS: ", bn), con)
writeLines(sprintf("CENSUS: total=%d", length(files)), con)
close(con)
cat(sprintf("census written: docs/audits/census.txt (%d files)\n", length(files)))

# tools/run_full_suite.R — full test suite, crash-resilient per-file bilan.
# Rationale: R exits with SIGSEGV (139) on this environment when some
# package combos unload (STATUS.md §2l) — with file-redirected stdout the
# test_dir summary is lost. Running file-by-file inside ONE process keeps
# the shared helper state AND flushes a result line per file, so a crash
# is isolated to its file and everything before it survives.
# Locale UTF-8 BEFORE any parse(): Git Bash exports LC_ALL=C.UTF-8, a name R
# does NOT recognise on Windows, so R silently falls back to "C", where it
# cannot parse some UTF-8 sources of the repo (STATUS.md, "Garde C13 +
# semantique testServer() MESUREE"). Not fatal if the locale is missing, but
# the warning must stay visible: a wrong locale yields FALSE failures.
.localectl <- Sys.setlocale("LC_CTYPE", "fr_FR.UTF-8")
if (!nzchar(.localectl)) {
  warning("LC_CTYPE fr_FR.UTF-8 unavailable: parse() false failures are likely")
}
files <- sort(list.files("tests/testthat", pattern = "^test-.*\\.R$",
                         full.names = TRUE))
cat(sprintf("files: %d\n", length(files)))
results_file <- "full_suite_results.txt"
con <- file(results_file, open = "wt")
writeLines(sprintf("start %s | %d files", format(Sys.time(), "%H:%M:%S"), length(files)), con)
flush(con)

total_failed <- 0L; total_passed <- 0L; total_error <- 0L; total_skipped <- 0L
crashed_after <- NA_character_
for (f in files) {
  bn <- basename(f)
  r <- tryCatch({
    res <- testthat::test_file(f, reporter = "silent", stop_on_failure = FALSE,
                               env = testthat::test_env(),
                               load_package = "none")
    df <- as.data.frame(res)
    sprintf("%-58s fail=%d pass=%d err=%d skip=%d",
            bn, sum(df$failed), sum(df$passed), sum(df$error), sum(df$skipped))
  }, error = function(e) sprintf("%-58s RUNNER-ERROR: %.120s", bn, conditionMessage(e)))
  writeLines(r, con); flush(con)
  if (grepl("RUNNER-ERROR", r)) next
  # Tally anchored on the labelled fields (a naive digit grab breaks on
  # filenames containing digits, e.g. test-shinytest2-bulk.R).
  getn <- function(tag) {
    m <- regmatches(r, regexpr(paste0(tag, "=(-?\\d+)"), r))
    if (!length(m)) return(0L)
    as.integer(sub(paste0(tag, "="), "", m))
  }
  total_failed  <- total_failed  + getn("fail")
  total_passed  <- total_passed  + getn("pass")
  total_error   <- total_error   + getn("err")
  total_skipped <- total_skipped + getn("skip")
  rm(r)
}
# Census (audit 2026-10-01, R2): emit the exact list of test files that were
# candidates for this run, next to the BILAN verdict, so the bilan can never be
# read as covering a different population than it actually ran. Deterministic:
# sorted, no timestamps, no locale-dependent values. NOTE: the pattern below is
# DUPLICATED in tools/build_census.R on purpose — if you change one, change the
# other (a shared helper was rejected to keep this crash-resilient script
# free of source() dependencies).
writeLines("CENSUS: begin", con)
for (bn in basename(files)) writeLines(paste0("CENSUS: ", bn), con)
writeLines(sprintf("CENSUS: total=%d", length(files)), con)
writeLines(sprintf("BILAN: failed=%d passed=%d error=%d skipped=%d | %s",
                   total_failed, total_passed, total_error, total_skipped,
                   format(Sys.time(), "%H:%M:%S")), con)
close(con)

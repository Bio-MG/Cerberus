# R4 (audit 2026-10-01): tools/run_tests.R used to end in invisible(NULL) and
# therefore exited 0 even when tests failed. The CI "Sous-suite ciblee" step
# (ci.yml) relies on that exit code, so the gate could never turn red. These
# tests spawn Rscript subprocesses against FIXTURE DIRECTORIES under tempdir()
# and assert the exit codes. Fixture files MUST stay out of tests/testthat/:
# a committed failing fixture would pollute the full-suite census
# (tools/run_full_suite.R) and turn the nightly red.

repo_root <- local({
  d <- normalizePath(getwd(), mustWork = FALSE)
  repeat {
    if (file.exists(file.path(d, "tools", "run_tests.R"))) break
    parent <- dirname(d)
    if (identical(parent, d)) {
      stop("repo root (containing tools/run_tests.R) not found from ", getwd())
    }
    d <- parent
  }
  d
})

rscript <- file.path(R.home("bin"), "Rscript")
runner <- file.path(repo_root, "tools", "run_tests.R")

run_runner <- function(tests_dir, filter) {
  out <- tempfile("runner-exit-probe-")
  on.exit(unlink(out), add = TRUE)
  status <- suppressWarnings(system2(
    rscript,
    c(shQuote(runner), shQuote(paste0("--test-dir=", tests_dir)), filter),
    stdout = out, stderr = out, timeout = 300
  ))
  list(status = status,
       output = if (file.exists(out)) readLines(out, warn = FALSE) else character())
}

make_fixture_dir <- function(name, body) {
  d <- file.path(tempdir(), name)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines(body, file.path(d, paste0("test-", name, ".R")))
  d
}

total_line <- function(output) {
  output[grepl("^TOTAL ", output)]
}

test_that("run_tests.R exits 1 on a deliberately failing fixture", {
  d <- make_fixture_dir(
    "exit-probe-fail",
    'test_that("deliberate failure (exit-code probe)", { expect_equal(1, 2) })'
  )
  r <- run_runner(d, "exit-probe-fail")
  expect_identical(r$status, 1L)
  expect_true(any(grepl("TOTAL .*failed=1", total_line(r$output))),
              label = "TOTAL line reports failed=1 in runner output")
})

test_that("run_tests.R exits 0 on a clean fixture", {
  d <- make_fixture_dir(
    "exit-probe-clean",
    'test_that("clean pass (exit-code probe)", { expect_true(TRUE) })'
  )
  r <- run_runner(d, "exit-probe-clean")
  expect_identical(r$status, 0L)
  expect_true(any(grepl("TOTAL .*failed=0", total_line(r$output))),
              label = "TOTAL line reports failed=0 in runner output")
})

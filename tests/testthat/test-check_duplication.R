# =============================================================================
# test-check_duplication.R — pre-merge guard: fail the suite on ERROR-severity
# findings from tools/check_duplication.R (QA-1 / RF-4, Biomentat 1.0 Step 4.0)
# =============================================================================
# Scans app.R/global.R/helpers_*.R (top-level "."), R/*.R, and modules/**/*.R
# for: output$ redefinitions, top-level function names defined in more than
# one sourced file, and >=15-line identical code blocks -- the exact bug
# class that let the helpers_bulk.R/R/palettes.R and helpers_io.R duplicate
# definitions go unnoticed until found by hand (see tools/check_duplication.R
# header). observeEvent()-trigger duplicates (WARNING severity) are reported
# but do NOT fail this test -- run_check()'s own fail_on_warning stays FALSE,
# matching the project's stated tolerance for that specific soft signal.
#
# NOTE for whoever sees this test go red for the first time: RF-4 widened
# check_duplication.R's default scan from "modules" alone to
# c("modules","R","."). A first run against the full tree may surface
# pre-existing duplication this session's context did not cover. Triage each
# finding on its own merits -- fix genuine bugs, or narrow this test's scope
# (roots/min_block below) with a comment explaining why a given block is an
# accepted, deliberate duplication. Do not just delete the guard.
# =============================================================================

source_project_file("tools/check_duplication.R")

test_that("check_duplication.R reports zero ERROR-severity findings across app.R/global.R/helpers_*.R + R/ + modules/", {
  root   <- ts_project_root()
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(root)

  printed <- capture.output(res <- run_check(c("modules", "R", "."), ext = "R"))

  expect_equal(
    res, 0L,
    info = paste(
      "check_duplication.R found ERROR-severity duplication -- fix it, or",
      "narrow this test's scan scope with a documented reason. Full report:",
      paste(printed, collapse = "\n"),
      sep = "\n"
    )
  )
})

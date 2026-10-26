# =============================================================================
# test-tier3-cross-sandbox-cli-isolation.R — Tier 3 Cross-Feature: CLI & Sandbox
# =============================================================================
# Cross-feature scenario: Upstream CLI validation executed concurrently with
# active operator drive session, asserting complete isolation:
# - Operator tools/_drive/ready.json is untouched by CLI runs
# - CLI writes validation.json strictly to --out directory
# - TRANSCRIPTO_DRIVE_ROOT sandbox prevents file collision
# =============================================================================

if (!exists("e2e_project_root", mode = "function")) {
  if (file.exists("tests/e2e/helper-e2e.R")) {
    source("tests/e2e/helper-e2e.R")
  } else if (file.exists("../helper-e2e.R")) {
    source("../helper-e2e.R")
  }
}

test_that("T3-CLI-SB-1: CLI invocation never touches host tools/_drive/ready.json", {
  host_drive_dir <- file.path(e2e_project_root(), "tools", "_drive")
  host_ready <- file.path(host_drive_dir, "ready.json")
  
  # Ensure host directory exists for testing
  dir.create(host_drive_dir, recursive = TRUE, showWarnings = FALSE)
  mock_token <- paste0("tok_host_", as.integer(runif(1, 1e6, 9e6)))
  writeLines(jsonlite::toJSON(list(token = mock_token, timestamp = Sys.time()), auto_unbox = TRUE), host_ready)
  on.exit(unlink(host_ready), add = TRUE)
  
  mtime_start <- file.info(host_ready)$mtime
  content_start <- readLines(host_ready, warn = FALSE)
  
  # Run CLI in subprocess on a test drop
  d <- e2e_make_test_drop(prefix = "t3_cli_sb_")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  out_dir <- file.path(d, "out")
  
  cli_res <- e2e_run_cli(c(paste0("--drop=", d), paste0("--out=", out_dir)))
  expect_identical(cli_res$exit_code, 0L)
  expect_true(file.exists(file.path(out_dir, "validation.json")))
  
  # Assert host ready.json was completely untouched
  expect_true(file.exists(host_ready))
  expect_identical(readLines(host_ready, warn = FALSE), content_start)
  expect_identical(file.info(host_ready)$mtime, mtime_start)
})

test_that("T3-CLI-SB-2: CLI outputs validation.json strictly into designated output directory", {
  d <- e2e_make_test_drop(prefix = "t3_out_dir_")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  custom_out <- file.path(tempdir(), paste0("custom_out_", as.integer(runif(1, 1e5, 9e5))))
  on.exit(unlink(custom_out, recursive = TRUE), add = TRUE)
  
  cli_res <- e2e_run_cli(c(paste0("--drop=", d), paste0("--out=", custom_out)))
  expect_identical(cli_res$exit_code, 0L)
  expect_true(file.exists(file.path(custom_out, "validation.json")))
  
  # Check no rogue files written to drop_dir
  drop_files <- list.files(d)
  expect_false("validation.json" %in% drop_files)
})

test_that("T3-CLI-SB-3: CLI with invalid drop preserves host drive files while reporting failure", {
  host_ready <- file.path(e2e_project_root(), "tools", "_drive", "ready.json")
  dir.create(dirname(host_ready), recursive = TRUE, showWarnings = FALSE)
  writeLines("{\"host_active\": true}", host_ready)
  on.exit(unlink(host_ready), add = TRUE)
  
  # Run CLI on non-existent drop
  cli_res <- e2e_run_cli(c("--drop=non_existent_drop_dir_xyz", paste0("--out=", tempdir())))
  expect_identical(cli_res$exit_code, 1L)
  
  expect_true(file.exists(host_ready))
  expect_identical(readLines(host_ready, warn = FALSE), "{\"host_active\": true}")
})

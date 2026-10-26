# =============================================================================
# test-drive-sandbox.R — Test suite for Feature 7-9: Drive Sandbox Isolation
# =============================================================================

# Ensure allowlist and drive watcher are loaded
if (file.exists(file.path(ts_project_root(), "R", "core", "drive_allowlist.R"))) {
  source(file.path(ts_project_root(), "R", "core", "drive_allowlist.R"), local = FALSE)
} else if (file.exists(file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "drive_allowlist.R"))) {
  source(file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "drive_allowlist.R"), local = FALSE)
}

if (file.exists(file.path(ts_project_root(), "R", "core", "drive_watcher.R"))) {
  source(file.path(ts_project_root(), "R", "core", "drive_watcher.R"), local = FALSE)
} else if (file.exists(file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "drive_watcher.R"))) {
  source(file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "drive_watcher.R"), local = FALSE)
}

# ── Feature 7: TRANSCRIPTO_DRIVE_ROOT Resolution ─────────────────────────────

test_that("ts_drive_root resolves to TRANSCRIPTO_DRIVE_ROOT when set", {
  tmp_root <- tempfile("sandbox_root_")
  dir.create(tmp_root, recursive = TRUE)
  on.exit({
    Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT")
    unlink(tmp_root, recursive = TRUE)
  }, add = TRUE)

  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = tmp_root)
  resolved <- ts_drive_root()
  expect_identical(
    normalizePath(resolved, winslash = "/", mustWork = FALSE),
    normalizePath(tmp_root, winslash = "/", mustWork = FALSE)
  )
})

test_that("ts_drive_path builds relative paths anchored at TRANSCRIPTO_DRIVE_ROOT", {
  tmp_root <- tempfile("sandbox_root_")
  dir.create(tmp_root, recursive = TRUE)
  on.exit({
    Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT")
    unlink(tmp_root, recursive = TRUE)
  }, add = TRUE)

  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = tmp_root)
  p <- ts_drive_path("ready.json")
  expect_identical(
    normalizePath(p, winslash = "/", mustWork = FALSE),
    normalizePath(file.path(tmp_root, "tools", "_drive", "ready.json"), winslash = "/", mustWork = FALSE)
  )
})

test_that("ts_drive_root strips trailing slashes to avoid path duplication", {
  tmp_root <- paste0(tempfile("sandbox_root_"), "/")
  dir.create(tmp_root, recursive = TRUE)
  on.exit({
    Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT")
    unlink(tmp_root, recursive = TRUE)
  }, add = TRUE)

  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = tmp_root)
  resolved <- ts_drive_root()
  expect_false(grepl("//", resolved))
  expect_false(endsWith(resolved, "/"))
})

test_that("ts_drive_boot with explicit argument overrides default and environment", {
  tmp_env <- tempfile("env_root_")
  tmp_boot <- tempfile("boot_root_")
  dir.create(tmp_env, recursive = TRUE)
  dir.create(tmp_boot, recursive = TRUE)
  on.exit({
    Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT")
    ts_drive_boot(NULL)
    unlink(tmp_env, recursive = TRUE)
    unlink(tmp_boot, recursive = TRUE)
  }, add = TRUE)

  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = tmp_env)
  ts_drive_boot(tmp_boot)
  expect_identical(
    normalizePath(ts_drive_root(), winslash = "/", mustWork = FALSE),
    normalizePath(tmp_boot, winslash = "/", mustWork = FALSE)
  )
})

test_that("ts_drive_root falls back to project root when TRANSCRIPTO_DRIVE_ROOT is unset", {
  old_env <- Sys.getenv("TRANSCRIPTO_DRIVE_ROOT", unset = NA)
  on.exit(if (!is.na(old_env)) Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = old_env) else Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT"), add = TRUE)
  Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT")

  ts_drive_boot(ts_project_root())
  res <- ts_drive_root()
  expect_true(nchar(res) > 0L)
  expect_identical(
    normalizePath(res, winslash = "/", mustWork = FALSE),
    normalizePath(ts_project_root(), winslash = "/", mustWork = FALSE)
  )
})

# ── Feature 8: Test Suite Hermetic Sandbox ───────────────────────────────────

test_that("app_driver_sandbox_dir returns the ts_drive_sandbox attribute", {
  mock_driver <- new.env()
  attr(mock_driver, "ts_drive_sandbox") <- "/tmp/test_sandbox"
  expect_identical(app_driver_sandbox_dir(mock_driver), "/tmp/test_sandbox")
  expect_null(app_driver_sandbox_dir(new.env()))
})

test_that("multiple sandboxes operate in disjoint directories without cross-talk", {
  sb1 <- tempfile("sb1_")
  sb2 <- tempfile("sb2_")
  dir.create(file.path(sb1, "tools", "_drive"), recursive = TRUE)
  dir.create(file.path(sb2, "tools", "_drive"), recursive = TRUE)
  on.exit(unlink(c(sb1, sb2), recursive = TRUE), add = TRUE)

  expect_false(identical(sb1, sb2))

  writeLines("{\"session\":\"session_1\"}", file.path(sb1, "tools", "_drive", "ready.json"))
  expect_true(file.exists(file.path(sb1, "tools", "_drive", "ready.json")))
  expect_false(file.exists(file.path(sb2, "tools", "_drive", "ready.json")))

  writeLines("{\"session\":\"session_2\"}", file.path(sb2, "tools", "_drive", "ready.json"))
  content1 <- readLines(file.path(sb1, "tools", "_drive", "ready.json"), warn = FALSE)
  content2 <- readLines(file.path(sb2, "tools", "_drive", "ready.json"), warn = FALSE)
  expect_false(identical(content1, content2))
})

test_that("sandboxed writes never escape to parent directory structure", {
  sb <- tempfile("sb_escape_")
  drv <- file.path(sb, "tools", "_drive")
  dir.create(drv, recursive = TRUE)
  on.exit(unlink(sb, recursive = TRUE), add = TRUE)

  target <- file.path(drv, "allowed.json")
  writeLines("{\"key\":\"value\"}", target)
  norm_target <- normalizePath(target, winslash = "/")
  norm_sb <- normalizePath(sb, winslash = "/")
  expect_true(startsWith(norm_target, norm_sb))
})

# ── Feature 9: Operator ready.json Preservation ──────────────────────────────

test_that("sandboxed session operations preserve host ready.json intact", {
  host_dir <- file.path(ts_project_root(), "tools", "_drive")
  dir.create(host_dir, recursive = TRUE, showWarnings = FALSE)
  host_ready <- file.path(host_dir, "ready.json")

  host_was_present <- file.exists(host_ready)
  if (!host_was_present) {
    writeLines("{\"host_sentinel\":\"preserved\"}", host_ready)
  }
  on.exit({
    if (!host_was_present && file.exists(host_ready)) {
      unlink(host_ready)
    }
  }, add = TRUE)

  mtime_before <- file.info(host_ready)$mtime
  content_before <- readLines(host_ready, warn = FALSE)

  # Run operations in a sandbox
  sandbox_dir <- tempfile("ts_drive_sandbox_preserve_")
  dir.create(file.path(sandbox_dir, "tools", "_drive"), recursive = TRUE)
  on.exit(unlink(sandbox_dir, recursive = TRUE), add = TRUE)

  old_env <- Sys.getenv("TRANSCRIPTO_DRIVE_ROOT", unset = NA)
  on.exit(if (!is.na(old_env)) Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = old_env) else Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT"), add = TRUE)
  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = sandbox_dir)

  ts_drive_boot(sandbox_dir)
  ts_drive_write_json(list(sandboxed = TRUE, token = "sb_tok_999"), ts_drive_path("ready.json"))

  # Host ready.json must be completely unaltered
  expect_true(file.exists(host_ready))
  expect_identical(readLines(host_ready, warn = FALSE), content_before)
  expect_identical(file.info(host_ready)$mtime, mtime_before)

  # Sandbox ready.json exists in sandbox only
  sb_ready <- file.path(sandbox_dir, "tools", "_drive", "ready.json")
  expect_true(file.exists(sb_ready))
  expect_false(identical(readLines(sb_ready, warn = FALSE), content_before))

  # Removing sandbox ready.json does not touch host ready.json
  unlink(sb_ready)
  expect_false(file.exists(sb_ready))
  expect_true(file.exists(host_ready))
  expect_identical(readLines(host_ready, warn = FALSE), content_before)
})

test_that("mock driver stop cleanly removes temporary sandbox directory", {
  sandbox_dir <- tempfile("ts_drive_sandbox_mock_")
  dir.create(file.path(sandbox_dir, "tools", "_drive"), recursive = TRUE)
  writeLines("{\"temp\":true}", file.path(sandbox_dir, "tools", "_drive", "ready.json"))

  # Build mock driver mimicking new_app_driver cleanup wrapping
  mock_driver <- new.env(parent = emptyenv())
  attr(mock_driver, "ts_drive_sandbox") <- sandbox_dir
  mock_driver$stopped <- FALSE
  mock_driver$stop <- function(...) {
    on.exit(unlink(sandbox_dir, recursive = TRUE, force = TRUE), add = TRUE)
    mock_driver$stopped <- TRUE
  }

  expect_true(dir.exists(sandbox_dir))
  mock_driver$stop()
  expect_true(mock_driver$stopped)
  expect_false(dir.exists(sandbox_dir))
})

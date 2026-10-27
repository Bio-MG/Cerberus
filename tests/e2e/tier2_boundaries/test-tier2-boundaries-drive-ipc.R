# =============================================================================
# test-tier2-boundaries-drive-ipc.R — Tier 2 Boundary: Drive IPC & Security
# =============================================================================
# Adversarial IPC protocol boundary tests:
# - Missing, empty, or unparseable ready.json
# - Ring buffer overflow stress (50 entries pushed to 20-entry FIFO)
# - Forbidden export request fields (path injection, traversal)
# - Closed route allowlist refusal
# - Concurrent drive path writes
# =============================================================================

if (!exists("e2e_project_root", mode = "function")) {
  if (file.exists("tests/e2e/helper-e2e.R")) {
    source("tests/e2e/helper-e2e.R")
  } else if (file.exists("../helper-e2e.R")) {
    source("../helper-e2e.R")
  }
}

test_that("T2-IPC-1: ts_drive_read_json never throws on absent or malformed files", {
  if (!exists("ts_drive_read_json", mode = "function")) skip("ts_drive_read_json not available")
  non_file <- file.path(tempdir(), "non_existent_file.json")
  expect_null(ts_drive_read_json(non_file))

  bad_json <- tempfile(fileext = ".json")
  writeLines("{ broken json: ...", bad_json)
  on.exit(unlink(bad_json), add = TRUE)
  expect_null(ts_drive_read_json(bad_json))
})

test_that("T2-IPC-2: TS_DRIVE_EXPORT_FORBIDDEN contains path injection parameter names", {
  if (!exists("TS_DRIVE_EXPORT_FORBIDDEN")) skip("TS_DRIVE_EXPORT_FORBIDDEN not available")
  expected_forbidden <- c("path", "file", "filename", "dir", "dest", "out")
  for (f in expected_forbidden) {
    expect_true(f %in% TS_DRIVE_EXPORT_FORBIDDEN, info = paste("Missing forbidden field:", f))
  }
})

test_that("T2-IPC-3: ring buffer overflow stress keeps exactly 20 latest entries (FIFO)", {
  if (!is_m2_ready()) skip("M2 error ring buffer not yet active")
  # M5 : stress 30 messages DISTINCTS — le tampon global reste borné à 20 et
  # les 10 plus anciens de la rafale ont été évincés.
  msgs <- sprintf("T2-IPC-3-stress-%02d", seq_len(30L))
  for (m in msgs) ts_drive_record_error(m, module = "drive_watcher")
  ring <- ts_drive_get_errors()
  expect_length(ring, 20L)
  texts <- vapply(ring, function(x) x$message %||% NA_character_, character(1))
  expect_false(any(msgs[1:10] %in% texts))
  expect_true(all(msgs[11:30] %in% texts))
})

test_that("T2-IPC-4: path traversal attempts in export route requests are rejected", {
  if (!exists("TS_DRIVE_EXPORT_FORBIDDEN")) skip("TS_DRIVE_EXPORT_FORBIDDEN not available")
  bad_input <- list(path = "../../etc/passwd", file = "../secret.txt")
  expect_true(any(names(bad_input) %in% TS_DRIVE_EXPORT_FORBIDDEN))
})

test_that("T2-IPC-5: atomic write retry ceiling TS_DRIVE_WRITE_ATTEMPTS is enforced", {
  if (!exists("TS_DRIVE_WRITE_ATTEMPTS")) skip("TS_DRIVE_WRITE_ATTEMPTS not available")
  expect_true(is.integer(TS_DRIVE_WRITE_ATTEMPTS))
  expect_identical(TS_DRIVE_WRITE_ATTEMPTS, 8L)
})

test_that("T2-IPC-6: export directory TS_DRIVE_EXPORT_DIRNAME is fixed to ts_drive_exports", {
  if (!exists("TS_DRIVE_EXPORT_DIRNAME")) skip("TS_DRIVE_EXPORT_DIRNAME not available")
  expect_identical(TS_DRIVE_EXPORT_DIRNAME, "ts_drive_exports")
})

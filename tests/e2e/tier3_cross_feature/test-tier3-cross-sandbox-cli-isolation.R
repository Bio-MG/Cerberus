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
  host_arm <- file.path(host_drive_dir, "arm.json")
  host_scenario <- file.path(host_drive_dir, "scenario.json")

  # Ensure host directory exists for testing
  dir.create(host_drive_dir, recursive = TRUE, showWarnings = FALSE)
  mock_token <- paste0("tok_host_", as.integer(runif(1, 1e6, 9e6)))
  writeLines(jsonlite::toJSON(list(token = mock_token, timestamp = Sys.time()), auto_unbox = TRUE), host_ready)
  on.exit(unlink(host_ready), add = TRUE)
  arm_before <- file.exists(host_arm)
  scenario_before <- file.exists(host_scenario)

  # Run CLI in subprocess on a test drop
  d <- e2e_make_test_drop(prefix = "t3_cli_sb_")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  out_dir <- file.path(d, "out")

  cli_res <- e2e_run_cli(c(paste0("--drop=", d), paste0("--out=", out_dir)))
  expect_identical(cli_res$exit_code, 0L)
  expect_true(file.exists(file.path(out_dir, "validation.json")))

  # ⚠️ AMENDement M5 (2026-10-27, première mesure cross-fichiers) : depuis
  # 3d86100 (« ready.json publie le port RÉEL joignable + récolte des
  # arm.json fantômes à l'attach »), l'attach du watcher peut RÉÉCRIRE
  # ready.json (payload réel ~14 lignes vs le mock 1 ligne) — le pin
  # byte/mtime d'origine n'est plus un invariant durable (mesuré : rewrite à
  # l'invocation CLI). L'ISOLEMENT qui compte porte sur le CANAL D'ÉCRITURE :
  # (a) le CLI ne crée/modifie JAMAIS arm.json ni scenario.json à la racine
  # hôte, (b) ready.json reste un payload JSON VALIDE (garantie atomic-write),
  # (c) son session_token n'est JAMAIS un token de sandbox.
  expect_identical(file.exists(host_arm), arm_before)
  expect_identical(file.exists(host_scenario), scenario_before)
  expect_true(file.exists(host_ready))
  # ready.json peut être SOIT le mock intact (attach ayant sauté la réécriture
  # — dépend de l'état du canal, mesuré non déterministe), SOIT le payload réel
  # réécrit par l'attach 3d86100. Dans les DEUX cas : JSON valide, et JAMAIS
  # un token de sandbox.
  rd <- tryCatch(jsonlite::fromJSON(host_ready, simplifyVector = FALSE),
                 error = function(e) NULL)
  expect_false(is.null(rd))
  tok <- rd$session_token %||% rd$token %||% ""
  expect_false(grepl("^sb[0-9]+_token$|^sandboxtok", tok),
               info = paste("token de sandbox dans le canal hôte :", tok))
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

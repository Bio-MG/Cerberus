# =============================================================================
# test-tier3-cross-error-propagation-digest.R — Tier 3 Cross-Feature: Error Flow
# =============================================================================
# Cross-feature scenario: Upstream or pipeline error triggering structured
# error recording and passive state digest notification.
# - Classed error from handoff or QC captured cleanly
# - Error ring buffer records error timestamp, module, and message
# - Passive state.json serializes error without crashing Shiny session
# =============================================================================

if (!exists("e2e_project_root", mode = "function")) {
  if (file.exists("tests/e2e/helper-e2e.R")) {
    source("tests/e2e/helper-e2e.R")
  } else if (file.exists("../helper-e2e.R")) {
    source("../helper-e2e.R")
  }
}

test_that("T3-ERR-1: handoff error contains classed state and message readable by error handler", {
  d <- e2e_make_test_drop(prefix = "t3_err_class_", write_manifest = FALSE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  # Malformed manifest
  writeLines("{\"invalid\": true}", file.path(d, "manifest.json"))
  
  res <- cerberus_handoff_validate_drop(drop_dir = d)
  expect_false(res$valid)
  expect_identical(res$status, "INVALID")
  expect_true(length(res$errors) >= 1L)
  
  err1 <- res$errors[[1L]]
  expect_true(!is.null(err1$code))
  expect_true(!is.null(err1$message))
})

test_that("T3-ERR-2: validation errors format cleanly for error ring buffer insertion", {
  if (!is_m2_ready()) skip("M2 error ring buffer not yet active")
  # M5 : une erreur CLASSÉE réelle entre dans le tampon et en ressort avec le
  # schéma complet (timestamp / module / message) — lisible par le state.json.
  cond <- errorCondition("T3-ERR-2 message de validation",
                         class = "handoff_validation_error")
  ring <- ts_drive_record_error(conditionMessage(cond), module = "handoff",
                                condition = cond)
  # Le tampon est GLOBAL au processus (les autres fichiers e2e y poussent
  # aussi) — on retrouve NOTRE entrée par contenu, jamais par position.
  texts <- vapply(ring, function(x) x$message %||% "", character(1))
  idx <- which(texts == "T3-ERR-2 message de validation")
  expect_true(length(idx) >= 1L)
  entry <- ring[[idx[length(idx)]]]
  expect_true(all(c("timestamp", "module", "message") %in% names(entry)))
  expect_match(entry$message, "T3-ERR-2", fixed = TRUE)
  expect_identical(entry$module, "handoff")
})

test_that("T3-ERR-3: state.json pipeline_status reflects error state on pipeline exception", {
  if (!is_m2_ready()) skip("M2 passive state digest not yet active")
  # M5 : après une erreur dans le tampon, l'émission (comportement du tick)
  # porte pipeline_status = "error" — le chemin has_recent_error mesuré du
  # détecteur de statut.
  sb <- tempfile("t3err3_")
  dir.create(file.path(sb, "tools", "_drive"), recursive = TRUE)
  on.exit({
    Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT")
    unlink(sb, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = sb)
  ts_drive_record_error("T3-ERR-3 échec pipeline", module = "sc_pipeline")
  # ⚠️ Sous le RUNNER (un seul processus pour 18 fichiers), le heartbeat M3
  # (options(ts.drive.hb_interval = 2)) continue d'écrire le canal en tâche de
  # fond — l'écriture atomique peut perdre une course rename (EBUSY Windows).
  # Réessais bornés : l'essence de l'assertion est « l'état FINIT émis », pas
  # « la première tentative gagne la course ».
  wrote <- FALSE
  for (i in seq_len(5L)) {
    wrote <- isTRUE(ts_drive_emit_passive_state(token = "t3err3", force = TRUE))
    if (wrote) break
    Sys.sleep(0.3)
  }
  expect_true(isTRUE(wrote) || file.exists(ts_drive_path("state.json")),
              info = "state.json jamais émis malgré 5 tentatives")
  st <- jsonlite::fromJSON(ts_drive_path("state.json"), simplifyVector = FALSE)
  expect_true(length(st$errors) >= 1L)
  # (1) Le CONTRAT error→status est réel au niveau du détecteur :
  # has_recent_error = TRUE ⇒ "error".
  expect_identical(ts_drive_detect_pipeline_status(NULL, NULL, has_recent_error = TRUE),
                   "error")
  # ✅ (2) Corrigé au jalon filets (2026-10-27, O18) : l'ÉMETTEUR câble
  # désormais has_recent_error (fenêtre TS_DRIVE_ERROR_RECENCY_SECS = 900 s) —
  # une erreur récente du tampon porte pipeline_status = "error" ; le chemin
  # ring→"error" est ATTEIGNABLE et le statut ne ment plus. Une erreur plus
  # vieille que la fenêtre retombe hors statut (test unitaire dédié).
  expect_identical(st$pipeline_status, "error",
                   info = paste("status inattendu :", st$pipeline_status))
})

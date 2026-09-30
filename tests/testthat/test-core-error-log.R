# =============================================================================
# test-core-error-log.R — journal des erreurs AVALÉES (jalon M-1, infra)
# =============================================================================
# CONSTAT AUDITÉ (audit externe 2026-09-30, D/F/J-4) : sur 776 handlers
# erreur/avertissement, ≈ 586 ne relancent ni ne journalisent — l'utilisateur
# voit « une étape vide sans message ». Premier remède : une passerelle de
# journalisation PURE, `ts_log_swallow(context, condition)`, qu'un handler
# silencieux appelle avant de rendre sa valeur de repli.
#
# Contrat : ne lève JAMAIS (un logger qui échoue ne doit pas casser le handler
# qui l'appelle) ; format de ligne daté `context | classe | message` ; append.
# =============================================================================
source_project_file("R/core/error_log.R")

test_that("ts_log_swallow écrit une ligne datée au format contrat", {
  lp <- file.path(tempdir(), paste0("ts_swallow_test_", Sys.getpid(), ".log"))
  on.exit(unlink(lp), add = TRUE)
  e <- errorCondition("objet introuvable", class = "sc_pipeline_error")
  r <- ts_log_swallow("report_collector.build_bulk_multi", e, log_path = lp)
  expect_null(r)                               # retour sans valeur utile
  lines <- readLines(lp)
  expect_length(lines, 1L)
  expect_match(lines[1], "report_collector\\.build_bulk_multi \\| sc_pipeline_error",
               info = "format : [horodatage] contexte | classe(s) | message")
  expect_match(lines[1], "\\| objet introuvable$",
               info = "le message termine la ligne")
})

test_that("le journal est un APPEND (plusieurs avalements = plusieurs lignes)", {
  lp <- file.path(tempdir(), paste0("ts_swallow_test_", Sys.getpid(), "b.log"))
  on.exit(unlink(lp), add = TRUE)
  ts_log_swallow("ctx.a", errorCondition("premier"), log_path = lp)
  ts_log_swallow("ctx.b", errorCondition("second"), log_path = lp)
  expect_length(readLines(lp), 2L)
})

test_that("le logger NE LÈVE JAMAIS (chemin impossible, condition exotique)", {
  # chemin réellement impossible : un FICHIER bloque le répertoire du journal
  blocker <- file.path(tempdir(), paste0("ts_swallow_blocker_", Sys.getpid()))
  writeLines("x", blocker)
  on.exit(unlink(blocker), add = TRUE)
  bad <- file.path(blocker, "sub", "x.log")
  expect_silent(ts_log_swallow("ctx", errorCondition("boom"), log_path = bad))
  expect_false(file.exists(bad))
  # condition non-standard : pas de crash
  lp <- file.path(tempdir(), paste0("ts_swallow_test_", Sys.getpid(), "c.log"))
  on.exit(unlink(lp), add = TRUE)
  expect_silent(ts_log_swallow("ctx", "pas une condition", log_path = lp))
  expect_length(readLines(lp), 1L)
})

test_that("ts_swallowed_log_path rend un chemin .log sous QC/ (défaut)", {
  p <- ts_swallowed_log_path()
  expect_match(p, "QC", fixed = TRUE)
  expect_match(p, "swallowed_errors\\.log$")
})

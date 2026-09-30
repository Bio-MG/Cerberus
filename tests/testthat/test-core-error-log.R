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

# ── Rotation (M-1 passe 2) : un journal non borné finirait par peser des
#    mégaoctets dans QC/ ; politique : AU-DELA du seuil, le journal courant
#    devient `<log>.1` (une seule génération conservée) et l'append repart
#    à neuf. Le contrat « ne lève jamais » couvre la rotation aussi.
test_that("rotation : au-delà du seuil, le journal courant devient <log>.1", {
  lp <- file.path(tempdir(), paste0("ts_swallow_rot_", Sys.getpid(), ".log"))
  rot <- paste0(lp, ".1")
  on.exit(unlink(c(lp, rot)), add = TRUE)
  # deux avalements "lourds" avec un seuil dérisoire : le 2e doit déclencher
  ts_log_swallow("ctx.premier", errorCondition("contenu genération un"),
                 log_path = lp, max_bytes = 10L)
  ts_log_swallow("ctx.second", errorCondition("contenu genération deux"),
                 log_path = lp, max_bytes = 10L)
  expect_true(file.exists(rot), info = "l'ancien journal est archivé en .1")
  expect_match(readLines(rot)[1], "ctx\\.premier",
               info = ".1 contient la génération précédente")
  cur <- readLines(lp)
  expect_length(cur, 1L)
  expect_match(cur[1], "ctx\\.second",
               info = "le journal courant repart à neuf")
})

test_that("rotation : pas de rotation sous le seuil, .1 écrasé à chaque rotation", {
  lp <- file.path(tempdir(), paste0("ts_swallow_rot2_", Sys.getpid(), ".log"))
  rot <- paste0(lp, ".1")
  on.exit(unlink(c(lp, rot)), add = TRUE)
  ts_log_swallow("ctx.a", errorCondition("a"), log_path = lp, max_bytes = 10L)
  ts_log_swallow("ctx.b", errorCondition("b"), log_path = lp, max_bytes = 10L^6)
  expect_false(file.exists(rot))
  expect_length(readLines(lp), 2L)
  # une deuxième rotation écrase l'ancienne génération .1
  ts_log_swallow("ctx.c", errorCondition("c"), log_path = lp, max_bytes = 10L)
  ts_log_swallow("ctx.d", errorCondition("d"), log_path = lp, max_bytes = 10L)
  expect_match(readLines(rot)[1], "ctx\\.c",
               info = "le .1 précédent est remplacé, pas cumulé")
})

test_that("rotation : le contrat « ne lève jamais » survit à un chemin impossible", {
  blocker <- file.path(tempdir(), paste0("ts_swallow_rotblock_", Sys.getpid()))
  writeLines("x", blocker)
  on.exit(unlink(blocker), add = TRUE)
  bad <- file.path(blocker, "sub", "x.log")
  expect_silent(ts_log_swallow("ctx", errorCondition("boom"),
                               log_path = bad, max_bytes = 1L))
})

test_that("le seuil par défaut est 1 Mo et n'altère pas un petit journal", {
  lp <- file.path(tempdir(), paste0("ts_swallow_def_", Sys.getpid(), ".log"))
  on.exit(unlink(lp), add = TRUE)
  expect_silent(ts_log_swallow("ctx", errorCondition("petit"), log_path = lp))
  expect_false(file.exists(paste0(lp, ".1")))
})

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

# ── Instrumentation de session (chantier #10) ───────────────────────────────
# CONSTAT : le journal ne portait AUCUN token de session. Deux onglets — ou
# deux exécutions successives — produisaient des lignes indiscernables, et un
# avalement survenu dans un worker mirai (aucune session) n'était pas
# distinguable d'un avalement dans l'onglet SC.
#
# Le token est AJOUTÉ DEVANT le contexte : `[horodatage] [session:<token>]
# contexte | classe | message`. Les deux assertions de format du premier bloc
# (adjacence `contexte | classe`, et `| message$`) restent vraies ⇒ c'est une
# EXTENSION du contrat, pas une rupture.
test_that("hors session, le token vaut 'no_session' (repli explicite)", {
  # Ce processus n'a aucun domaine réactif : c'est exactement le cas d'un
  # worker mirai ou d'un script. Le repli doit être EXPLICITE, pas un vide.
  expect_null(shiny::getDefaultReactiveDomain())
  expect_identical(ts_session_token(), "no_session")
})

test_that("dans une session, la ligne porte le VRAI session$token", {
  lp <- file.path(tempdir(), paste0("ts_swallow_tok_", Sys.getpid(), ".log"))
  on.exit(unlink(lp), add = TRUE)
  seen <- NULL
  mod <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      # `<<-` : sortir la valeur du bloc. testServer() n'échoue PAS sur une
      # erreur d'observateur (fail = 0) ⇒ on asserte l'effet OBSERVABLE, le log.
      seen <<- session$token
      ts_log_swallow("ctx.token", errorCondition("boom"), log_path = lp)
    })
  }
  shiny::testServer(mod, {})
  expect_true(is.character(seen) && nzchar(seen))
  line <- readLines(lp)[1]
  expect_match(line, sprintf("\\[session:%s\\]", seen),
               info = "la ligne doit porter le token de la session courante")
  expect_false(grepl("no_session", line, fixed = TRUE))
})

test_that("ts_swallow() : journalise ET rend le repli (gestionnaire unique)", {
  lp <- file.path(tempdir(), paste0("ts_swallow_h_", Sys.getpid(), ".log"))
  on.exit(unlink(lp), add = TRUE)
  h <- ts_swallow("ctx.handler", log_path = lp)
  expect_true(is.function(h))
  expect_null(h(errorCondition("avale")))
  expect_true(file.exists(lp), info = "le gestionnaire doit AVOIR journalisé")
  expect_length(readLines(lp), 1L)
  expect_match(readLines(lp)[1], "ctx\\.handler")
})

test_that("ts_swallow() : repli explicite, et utilisable en `error=`", {
  lp <- file.path(tempdir(), paste0("ts_swallow_h2_", Sys.getpid(), ".log"))
  on.exit(unlink(lp), add = TRUE)
  got <- tryCatch(stop(errorCondition("echec")),
                  error = ts_swallow("ctx.fallback", fallback = NA_integer_,
                                     log_path = lp))
  expect_true(is.na(got))
  expect_true(file.exists(lp), info = "le gestionnaire doit AVOIR journalisé")
  expect_length(readLines(lp), 1L)
  expect_match(readLines(lp)[1], "ctx\\.fallback")
})

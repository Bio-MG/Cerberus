# =============================================================================
# test-spatial-daemons-config.R — daemons mirai lus depuis la config (QW-3)
# =============================================================================
# CONSTAT AUDITÉ (audit externe 2026-09-30, C-2 ; re-mesuré sur l'arbre) :
# `TS_MIRAI_N_DAEMONS <- 6L` était définie dans config/defaults.R mais JAMAIS
# lue — `R/spatial/spatial_async.R` codait `6L` en dur (init de
# `.spatial_async_env` et défaut de `init_spatial_daemons()`). Une constante
# déclarée mais ignorée n'est pas une configuration, c'est un leurre.
#
# Ce fichier épingle le CÂBLAGE (les deux sites lisent la constante, avec le
# repli `exists()` de la maison — cf. TS_MIRAI_TIMEOUT_MS ligne 85) et la
# cohérence de valeur. Il ne lance JAMAIS `mirai::daemons()` (pas de vrais
# workers dans les tests).
# =============================================================================
source_project_file("config/defaults.R")
source_project_file("tools/pkg_skeleton/R/spatial_async.R")  # ex-R/spatial/, D2 M5 lot 4

async_src <- readLines(file.path("..", "..", "tools", "pkg_skeleton", "R", "spatial_async.R"))

test_that("le défaut de init_spatial_daemons() passe par le résolveur unique", {
  # ⚠️ Chantier #5 (2026-10-19) : ces deux assertions épinglaient l'EXPRESSION
  # du défaut (`if (exists(...)) TS_MIRAI_N_DAEMONS else 6L`). La résolution a
  # été déplacée dans UN résolveur (option > constante > 6L), sinon trois sites
  # continuaient de la recoder — dont un littéral 6 dans le module spatial.
  # Le CONTRAT est conservé et STRENGTHENED : on épingle toujours « pas de
  # littéral » et « repli exists() explicite », et on ajoute la précédence.
  fml <- paste(deparse(formals(init_spatial_daemons)$n_daemons), collapse = " ")
  expect_match(fml, "ts_mirai_n_daemons\\(\\)",
               info = "le défaut ne doit plus être un 6L littéral")
  body_src <- paste(deparse(body(ts_mirai_n_daemons)), collapse = " ")
  expect_match(body_src, "TS_MIRAI_N_DAEMONS", fixed = TRUE)
  expect_match(body_src, "exists\\(\"TS_MIRAI_N_DAEMONS\"\\)",
               info = "le repli sans config (idiome exists() de la maison) survit")
})

test_that("l'init de .spatial_async_env$n_daemons lit TS_MIRAI_N_DAEMONS", {
  hit <- grep("^\\.spatial_async_env\\$n_daemons", async_src, value = TRUE)
  expect_length(hit, 1L)
  expect_match(hit, "ts_mirai_n_daemons()", fixed = TRUE)
})

test_that("le repli sans config existe (idiome exists() de la maison)", {
  fml <- paste(deparse(body(ts_mirai_n_daemons)), collapse = " ")
  expect_match(fml, "exists\\(\"TS_MIRAI_N_DAEMONS\"\\)")
})

# ── Chantier #5 (2026-10-19) : le module imposait `n_daemons = 6` en dur au
#    site d'appel, court-circuitant TS_MIRAI_N_DAEMONS ; et RIEN n'arrêtait le
#    pool à la fermeture (0 appelant à stop_spatial_daemons au shutdown) —
#    workers orphelins sur Windows.
test_that("le module spatial n'impose plus 6 daemons en dur", {
  mod_src <- readLines(file.path("..", "..", "modules", "spatial", "mod_spatial.R"))
  hit <- grep("init_spatial_daemons\\(", mod_src, value = TRUE)
  expect_gte(length(hit), 1L)                      # anti-vacuité
  expect_false(any(grepl("n_daemons\\s*=\\s*6", hit)),
               info = "le site d'appel doit lire la config, pas un littéral")
})

test_that("ts_mirai_n_daemons() : option > constante > repli", {
  old <- options(ts.mirai.n_daemons = NULL)
  on.exit(options(old), add = TRUE)
  expect_identical(ts_mirai_n_daemons(), as.integer(TS_MIRAI_N_DAEMONS))
  options(ts.mirai.n_daemons = 3L)
  expect_identical(ts_mirai_n_daemons(), 3L)
  options(ts.mirai.n_daemons = "4")
  expect_identical(ts_mirai_n_daemons(), 4L)
  # une option invalide ne doit PAS produire un pool de 0 worker : on retombe
  # sur la constante, pas sur une valeur arbitraire.
  options(ts.mirai.n_daemons = NA)
  expect_identical(ts_mirai_n_daemons(), as.integer(TS_MIRAI_N_DAEMONS))
  options(ts.mirai.n_daemons = 0L)
  expect_identical(ts_mirai_n_daemons(), as.integer(TS_MIRAI_N_DAEMONS))
})

test_that("stop_spatial_daemons() est idempotent sans pool", {
  # Aucun daemon n'est lancé dans ce fichier : l'appel doit être un no-op
  # silencieux, deux fois de suite (le hook onStop peut courir après un
  # teardown manuel).
  expect_silent(stop_spatial_daemons())
  expect_silent(stop_spatial_daemons())
  expect_false(spatial_daemons_ready())
})

test_that("stop_spatial_daemons() arrête un pool réel même si le drapeau ment", {
  # Le défaut d'origine : le teardown se fiait à `.spatial_async_env$daemons_ready`.
  # Un init partiellement échoué (ou un état hérité) laisse alors un pool VIVANT
  # que le drapeau déclare absent ⇒ workers orphelins. Le test SABOTE le drapeau
  # pour reproduire cet état, puis exige que le pool soit réellement arrêté.
  skip_if_not_installed("mirai")
  on.exit(tryCatch(mirai::daemons(0), error = function(e) NULL), add = TRUE)
  if (!mirai::daemons_set()) suppressMessages(mirai::daemons(1))
  expect_true(mirai::daemons_set())          # anti-vacuité : un pool existe
  .spatial_async_env$daemons_ready <- FALSE  # le drapeau MENT
  stop_spatial_daemons()
  expect_false(mirai::daemons_set())
})

test_that("app.R enregistre le teardown sur onStop", {
  app_src <- readLines(file.path("..", "..", "app.R"))
  expect_true(any(grepl("onStop\\(\\s*stop_spatial_daemons\\s*\\)", app_src)),
              info = "sans onStop, les workers survivent à la fermeture de l'app")
})

test_that("cohérence de valeur : env initialisée == constante config", {
  expect_identical(.spatial_async_env$n_daemons, TS_MIRAI_N_DAEMONS)
})

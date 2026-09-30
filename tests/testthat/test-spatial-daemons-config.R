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
source_project_file("R/spatial/spatial_async.R")

async_src <- readLines(file.path("..", "..", "R", "spatial", "spatial_async.R"))

test_that("le défaut de init_spatial_daemons() lit TS_MIRAI_N_DAEMONS", {
  fml <- deparse(formals(init_spatial_daemons)$n_daemons)
  expect_match(paste(fml, collapse = " "), "TS_MIRAI_N_DAEMONS", fixed = TRUE,
               info = "le défaut ne doit plus être un 6L littéral")
})

test_that("l'init de .spatial_async_env$n_daemons lit TS_MIRAI_N_DAEMONS", {
  hit <- grep("^\\.spatial_async_env\\$n_daemons", async_src, value = TRUE)
  expect_length(hit, 1L)
  expect_match(hit, "TS_MIRAI_N_DAEMONS", fixed = TRUE)
})

test_that("le repli sans config existe (idiome exists() de la maison)", {
  fml <- paste(deparse(formals(init_spatial_daemons)$n_daemons), collapse = " ")
  expect_match(fml, "exists\\(\"TS_MIRAI_N_DAEMONS\"\\)")
})

test_that("cohérence de valeur : env initialisée == constante config", {
  expect_identical(.spatial_async_env$n_daemons, TS_MIRAI_N_DAEMONS)
})

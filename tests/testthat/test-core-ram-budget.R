# =============================================================================
# test-core-ram-budget.R — socle du préflight RAM (jalon M-4)
# =============================================================================
# CONSTAT AUDITÉ (audit externe 2026-09-30, J-8 / registre de risques) :
# « dépassement RAM sans alerte » — l'app lance des pipelines lourds (objets
# Seurat copiés, 6 daemons mirai) sans AUCUNE lecture de la RAM disponible ;
# le seul indicateur était faux (QW-1 corrigé). MODE C, direction 2 : un
# gouverneur exige un BENCHMARK d'abord — en attendant, ce socle mesure et
# projette, et NE BLOQUE JAMAIS (le risque documenté du gouverneur est le faux
# positif qui bloque des analyses légitimes).
#
# Périmètre de ce jalon (socle pur + câblage en alerte) :
#   1. ts_system_ram_mb() — RAM système totale/disponible via ps (optionnel) ;
#   2. ts_ram_budget_check() — décision PURE à paramètres explicites (aucune
#      lecture système dedans : testable sans machine réelle) ;
#   3. le câblage de run_sc_auto_pipeline reste en place (anti-débranchage).
# Le facteur de projection est une constante config (TS_RAM_PREFLIGHT_FACTOR)
# VOLONTAIREMENT à calibrer par benchmark — pas une vérité inventée (§2dr).
# =============================================================================
source_project_file("config/defaults.R")
source_project_file("R/core/memory.R")
source_project_file("R/sc/sc_pipeline.R")

test_that("ts_system_ram_mb rend total/available numériques et plausibles", {
  ram <- ts_system_ram_mb()
  expect_true(is.list(ram) && identical(names(ram), c("total_mb", "available_mb")))
  expect_true(is.numeric(ram$total_mb) && is.numeric(ram$available_mb))
  if (!is.na(ram$total_mb)) {
    expect_gt(ram$total_mb, 1024)                # une machine de dev dépasse 1 Go
    expect_lte(ram$available_mb, ram$total_mb)   # le disponible ne dépasse pas le total
  }
})

test_that("budget sous le disponible : aucune alerte", {
  b <- ts_ram_budget_check(object_bytes = 100 * 1024^2, factor = 3,
                           available_mb = 8192, total_mb = 32768)
  expect_identical(b$level, "none")
  expect_identical(b$message, "")
  expect_equal(b$projected_mb, 300)
})

test_that("budget entre disponible et total : ALERTE (warn), en français", {
  b <- ts_ram_budget_check(object_bytes = 4 * 1024^3, factor = 3,
                           available_mb = 8192, total_mb = 32768)
  expect_identical(b$level, "warn")
  expect_match(b$message, "disponible", fixed = TRUE)
  expect_equal(b$projected_mb, 12288)
})

test_that("budget au-dessus du total : BLOCAGE ANNONCÉ (block), en français", {
  b <- ts_ram_budget_check(object_bytes = 20 * 1024^3, factor = 3,
                           available_mb = 8192, total_mb = 32768)
  expect_identical(b$level, "block")
  expect_match(b$message, "totale", fixed = TRUE)
})

test_that("RAM non mesurable (NA) : niveau none, jamais de blocage", {
  b <- ts_ram_budget_check(object_bytes = 100 * 1024^9, factor = 3,
                           available_mb = NA_real_, total_mb = NA_real_)
  expect_identical(b$level, "none")
})

test_that("le facteur pilote la projection (et reste configurable)", {
  small <- ts_ram_budget_check(1024^3, factor = 2, available_mb = 8192, total_mb = 32768)
  big   <- ts_ram_budget_check(1024^3, factor = 6, available_mb = 8192, total_mb = 32768)
  expect_gt(big$projected_mb, small$projected_mb)
  expect_true(exists("TS_RAM_PREFLIGHT_FACTOR") && TS_RAM_PREFLIGHT_FACTOR > 0)
})

test_that("le câblage du préflight dans run_sc_auto_pipeline est en place", {
  body_txt <- paste(deparse(body(run_sc_auto_pipeline)), collapse = "\n")
  expect_match(body_txt, "ts_ram_budget_check", fixed = TRUE,
               info = "le préflight ne doit pas être débranché en silence")
})

# =============================================================================
# test-core-memory-rss.R — indicateur RAM résidente du processus (jalon QW-1)
# =============================================================================
# CONSTAT AUDITÉ (audit externe 2026-09-30, annexe ; re-mesuré sur l'arbre) :
# l'indicateur de la barre d'état divisait `sum(gc()[, 2])` par 1024, alors
# que `gc()[, 2]` est DÉJÀ en Mo ⇒ affichage ~1024× trop petit, et un gc()
# complet forcé dans le chemin de rendu toutes les 5 s.
#
# Ce fichier épingle : (a) l'existence et la forme du helper
# `ts_process_rss_mb()` ; (b) l'interdit de re-diviser la sortie de gc() ;
# (c) la vraisemblance de la mesure quand `ps` est disponible.
# =============================================================================
source_project_file("R/core/memory.R")

test_that("ts_process_rss_mb existe et renvoie un scalaire numérique ou NA_real_", {
  expect_true(exists("ts_process_rss_mb"),
              info = "le helper doit exister dans R/core/memory.R")
  v <- ts_process_rss_mb()
  expect_true(is.numeric(v) && length(v) == 1L)
  expect_true(is.na(v) || is.finite(v))
})

test_that("le corps du helper ne re-divise JAMAIS la sortie de gc() par 1024", {
  body_txt <- paste(deparse(body(ts_process_rss_mb)), collapse = " ")
  expect_false(grepl("gc\\s*\\([^)]*\\)\\s*\\[,\\s*2\\s*\\]\\s*/\\s*1024", body_txt),
               info = "gc()[, 2] est déjà en Mo — une division par 1024 est le bug historique")
})

test_that("si ps est disponible, la mesure est dans un ordre de grandeur plausible", {
  skip_if_not(requireNamespace("ps", quietly = TRUE))
  v <- ts_process_rss_mb()
  expect_false(is.na(v), info = "ps est installé : le RSS doit être mesurable")
  expect_gt(v, 50)   # un R/Shiny chargé dépasse largement 50 Mo de RSS
})

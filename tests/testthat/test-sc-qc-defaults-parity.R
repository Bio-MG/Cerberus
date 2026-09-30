# =============================================================================
# test-sc-qc-defaults-parity.R — source unique des défauts QC (jalon QW-2)
# =============================================================================
# CONSTAT AUDITÉ (audit externe 2026-09-30, C-1 ; re-mesuré sur l'arbre) :
# `.sc_ap_drive_inputs()` codait des défauts QC PLUS PERMISSIFS que l'UI
# (10 vs 100 min gènes, 10000 vs 8000 max gènes, 50 vs 20 % mito, 10 vs 20
# dims PCA), et AUCUNE de ces valeurs ne vivait dans config/. Un run piloté
# par agent qui omet des clés obtenait un QC nettement plus permissif que le
# run humain ⇒ fork de reproductibilité.
#
# Ce fichier épingle : config est la SOURCE UNIQUE, l'UI et le drive la lisent.
# Toute divergence future entre les canaux fait ROUGIR ce test.
# =============================================================================
source_project_file("config/defaults.R")
source_project_file("modules/sc/mod_sc.R")

mod_src <- readLines(file.path("..", "..", "modules", "sc", "mod_sc.R"))

test_that("les constantes canoniques existent avec les valeurs UI historiques", {
  expect_equal(TS_SC_QC_MIN_GENES,  100)
  expect_equal(TS_SC_QC_MAX_GENES,  8000)
  expect_equal(TS_SC_QC_MAX_PCT_MT, 20)
  expect_equal(TS_SC_QC_PCA_DIMS,   20)
})

test_that("le canal drive lit la config — plus aucune valeur permissive en dur", {
  d <- .sc_ap_drive_inputs()
  expect_equal(d$sc_ap_min_gene, TS_SC_QC_MIN_GENES)
  expect_equal(d$sc_ap_max_gene, TS_SC_QC_MAX_GENES)
  expect_equal(d$sc_ap_mt,       TS_SC_QC_MAX_PCT_MT)
  expect_equal(d$sc_ap_pca_dim,  TS_SC_QC_PCA_DIMS)
})

test_that("l'UI est câblée sur les constantes (plus de littéraux QC)", {
  ui_min  <- grep('numericInput\\(ns_m\\("sc_ap_min_gene"\\)', mod_src, value = TRUE)
  ui_max  <- grep('numericInput\\(ns_m\\("sc_ap_max_gene"\\)', mod_src, value = TRUE)
  ui_mt   <- grep('sliderInput\\(ns_m\\("sc_ap_mt"\\)', mod_src, value = TRUE)
  ui_pca  <- grep('sliderInput\\(ns_m\\("sc_ap_pca_dim"\\)', mod_src, value = TRUE)
  expect_length(ui_min, 1)
  expect_length(ui_max, 1)
  expect_length(ui_mt, 1)
  expect_length(ui_pca, 1)
  expect_match(ui_min, "TS_SC_QC_MIN_GENES", fixed = TRUE)
  expect_match(ui_max, "TS_SC_QC_MAX_GENES", fixed = TRUE)
  expect_match(ui_mt,  "TS_SC_QC_MAX_PCT_MT", fixed = TRUE)
  expect_match(ui_pca, "TS_SC_QC_PCA_DIMS", fixed = TRUE)
})

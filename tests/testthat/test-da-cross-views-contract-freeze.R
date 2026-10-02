# =============================================================================
# test-da-cross-views-contract-freeze.R — Stage 16 : gel des vues croisées
# Milo x scCODA (4E-3)
# =============================================================================
# REFUSE toute evolution incompatible : categories de concordance figees,
# surface publique de R/sc/sc_abundance_cross_views.R, orchestration module
# (3 fonctions top-level, AUCUN calcul), consommation des deux resultats
# canoniques via shared_rv, isolation des consommateurs rapport/export,
# synchronisation code <-> doc.
# =============================================================================

.dacross_top_level_assignments <- function(relpath) {
  exprs <- parse(file.path(ts_project_root(), relpath), keep.source = FALSE)
  nms <- character(0)
  for (e in exprs) {
    if (is.call(e) && identical(deparse(e[[1L]]), "<-") && is.symbol(e[[2L]])) {
      nms <- c(nms, as.character(e[[2L]]))
    }
  }
  unique(nms)
}

test_that("concordance categories are frozen (explicit rules, no composite score)", {
  expect_setequal(da_cross_concordance_categories(), c(
    "concordant_enriched_target", "concordant_enriched_reference",
    "discordant_direction", "milo_only", "sccoda_only", "no_signal",
    "not_comparable"
  ))
})

test_that("public API surface of sc_abundance_cross_views.R is frozen", {
  defined <- .dacross_top_level_assignments("R/sc/sc_abundance_cross_views.R")
  public <- defined[!grepl("^\\.", defined)]
  expect_setequal(public, da_cross_views_public_api())
  expect_true(all(c(".dacross_stop", ".dacross_comparability") %in%
                    setdiff(defined, public)))
})

test_that("cross module defines exactly its three orchestration functions", {
  expect_setequal(
    .dacross_top_level_assignments("modules/sc/mod_sc_da_cross.R"),
    c("mod_sc_da_cross_ui", "mod_sc_da_cross_output_ui",
      "mod_sc_da_cross_server")
  )
  src <- paste(readLines(file.path(ts_project_root(),
                                   "modules/sc/mod_sc_da_cross.R")),
               collapse = "\n")
  # Le module consomme les accesseurs documentes des deux domaines + vues.
  expect_match(src, "build_da_cross_method_summary(", fixed = TRUE)
  expect_match(src, "build_da_cross_concordance_export(", fixed = TRUE)
  expect_match(src, "build_da_cross_provenance(", fixed = TRUE)
  expect_match(src, "da_cross_export_filename(", fixed = TRUE)
  expect_match(src, "plot_da_cross_concordance(", fixed = TRUE)
  expect_match(src, "provenance_append(", fixed = TRUE)
  expect_match(src, "shared_rv$da_milo_result", fixed = TRUE)
  expect_match(src, "shared_rv$da_sccoda_result", fixed = TRUE)
  # AUCUN calcul dans le module : pas de re-execution des methodes.
  expect_false(grepl("run_milo_da\\(|run_sccoda_da\\(|testNhoods\\(|model\\.matrix|reticulate::",
                     src))
})

test_that("cross views are mounted once in mod_sc.R (panel 8f + output tab)", {
  mod_sc_src <- paste(readLines(file.path(ts_project_root(),
                                          "modules/sc/mod_sc.R")),
                      collapse = "\n")
  expect_match(mod_sc_src, "mod_sc_da_cross_server", fixed = TRUE)
  expect_false(grepl("build_da_cross_method_summary", mod_sc_src))
  expect_match(mod_sc_src, "8f", fixed = TRUE)
  expect_match(mod_sc_src, "tab_da_cross", fixed = TRUE)
})

# ── Isolation des consommateurs rapport/export ──────────────────────────────
test_that("report/export consumers do not reimplement cross views yet (Stage 17 scope)", {
  export_src <- paste(readLines(file.path(ts_project_root(),
                                          "R/sc/sc_export.R")),
                      collapse = "\n")
  expect_false(grepl("da_cross|cross_views", export_src, ignore.case = TRUE))
  for (tpl in list.files(file.path(ts_project_root(), "reports"),
                         full.names = TRUE)) {
    tpl_src <- paste(readLines(tpl), collapse = "\n")
    expect_false(grepl("da_cross|cross_views", tpl_src, ignore.case = TRUE),
                 info = basename(tpl))
  }
})

# ── Synchronisation code <-> contrat documentaire ───────────────────────────
test_that("contract document is in sync with the frozen code", {
  doc_path <- file.path(ts_project_root(), "docs", "contracts",
                        "DA_CROSS_VIEWS_CONTRACT.md")
  skip_if_not(file.exists(doc_path), .ts_contract_skip_msg(doc_path))
  doc <- paste(.ts_contract_readlines(doc_path), collapse = "\n")
  for (cat in da_cross_concordance_categories()) {
    expect_match(doc, paste0("`", cat, "`"), fixed = TRUE,
                 info = paste("categorie absente du document :", cat))
  }
  for (fn in c("build_da_cross_method_summary", "build_da_cross_concordance",
               "build_da_cross_provenance", "da_cross_views_public_api",
               "da_cross_concordance_categories")) {
    expect_match(doc, fn, fixed = TRUE,
                 info = paste("fonction absente du document :", fn))
  }
  # Les interdits scientifiques sont documentes.
  expect_match(doc, "p-value de consensus", fixed = TRUE)
  expect_match(doc, "descriptive", ignore.case = TRUE)
  expect_match(doc, "ÉCHANTILLON", fixed = TRUE)
})

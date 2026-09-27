# =============================================================================
# test-sc-metadata-contract-freeze.R — gel du contrat
# docs/contracts/SC_METADATA_CONTRACT.md (design condition / réplicat)
# =============================================================================
# Verrouille : surface publique, signatures, règles métier (échec si un
# échantillon manque — aucun repli silencieux), PURETÉ Shiny de la logique,
# câblage module (additif), sync code <-> contrat.
# =============================================================================
source_project_file("R/sc/sc_metadata.R")

.ts_read <- function(relpath) {
  paste(readLines(file.path(ts_project_root(), relpath), warn = FALSE),
        collapse = "\n")
}

# ── Surface publique gelée ─────────────────────────────────────────────────
test_that("public API surface is frozen (names + signatures)", {
  expect_setequal(
    sc_metadata_public_api(),
    c("sc_metadata_public_api", "sc_metadata_error_class", "sc_metadata_map_modes",
      "sc_metadata_sample_table", "sc_metadata_parse_sample_label",
      "sc_metadata_parse_sample_table", "sc_metadata_read_csv",
      "sc_metadata_join_csv", "sc_metadata_apply", "sc_metadata_design_recap")
  )
  for (fn_name in sc_metadata_public_api()) {
    expect_true(exists(fn_name, where = globalenv(), inherits = FALSE),
                info = paste("fonction absente :", fn_name))
  }
  expected_args <- list(
    sc_metadata_public_api          = list(),
    sc_metadata_error_class         = list(),
    sc_metadata_map_modes           = list(),
    sc_metadata_sample_table        = list(meta = NULL, sample_col = "orig.ident"),
    sc_metadata_parse_sample_label  = list(lbl = NULL, cond_position = c(first = "first", last = "last")),
    sc_metadata_parse_sample_table  = list(sample_tbl = NULL, cond_position = c(first = "first", last = "last")),
    sc_metadata_read_csv            = list(path = NULL),
    sc_metadata_join_csv            = list(meta = NULL, csv = NULL, key_col = "sample",
                                           sample_col = "orig.ident",
                                           condition_col = "condition",
                                           replicate_col = "replicate"),
    sc_metadata_apply               = list(meta = NULL, sample_tbl = NULL, sample_col = "orig.ident"),
    sc_metadata_design_recap        = list(sample_tbl = NULL, cells_per_sample = NULL)
  )
  for (fn_name in names(expected_args)) {
    fmls <- names(formals(get(fn_name, envir = globalenv())))
    expect_identical(fmls, names(expected_args[[fn_name]]),
                     info = paste("signature de", fn_name))
  }
})

# ── États d'erreur + modes gelés ───────────────────────────────────────────
test_that("error class and map modes are frozen", {
  expect_identical(sc_metadata_error_class(), "sc_metadata_error")
  expect_setequal(sc_metadata_map_modes(), c("manual", "parse_labels", "csv"))
  bad <- tryCatch(sc_metadata_sample_table(data.frame()), error = function(e) e)
  expect_s3_class(bad, sc_metadata_error_class())
  # Le contrat cite les règles métier et le plancher de réplicats.
  doc <- .ts_read("docs/contracts/SC_METADATA_CONTRACT.md")
  for (anchor in c("sc_metadata_apply", "sc_metadata_join_csv",
                   "sc_metadata_design_recap", "parse_labels",
                   "TS_DA_MIN_REPLICATES_PER_CONDITION", "sc_metadata_error",
                   "mod_sc_metadata_ui", "orig.ident", "A_1", "1a")) {
    expect_match(doc, anchor, fixed = TRUE, info = anchor)
  }
})

# ── Pureté Shiny (contrat §4) ──────────────────────────────────────────────
test_that("R/sc/sc_metadata.R is pure (no Shiny symbols)", {
  src <- .ts_read("R/sc/sc_metadata.R")
  for (sym in c("reactiveVal", "reactiveValues", "observeEvent", "output\\$",
                "input\\$", "moduleServer", "showNotification",
                "isolate\\(", "reactive\\(")) {
    expect_false(grepl(sym, src, perl = TRUE),
                 info = paste("symbole Shiny interdit trouvé :", sym))
  }
})

# ── Règle métier §3.3 : échec si un échantillon manque (aucun repli) ──────
test_that("apply/join FAIL when a sample is missing from the design", {
  meta <- data.frame(orig.ident = c("A_1", "A_2", "B_1"), stringsAsFactors = FALSE)
  tbl_ok <- data.frame(sample = c("A_1", "A_2", "B_1"),
                       condition = c("A", "A", "B"),
                       replicate = c("1", "2", "1"), stringsAsFactors = FALSE)
  meta_ok <- sc_metadata_apply(meta, tbl_ok)
  expect_identical(as.character(meta_ok$condition), c("A", "A", "B"))
  expect_identical(as.character(meta_ok$replicate), c("1", "2", "1"))

  tbl_short <- tbl_ok[1:2, ]
  expect_error(sc_metadata_apply(meta, tbl_short),
               regexp = "B_1", class = sc_metadata_error_class())
  csv_short <- data.frame(sample = c("A_1", "A_2"), condition = c("A", "A"))
  expect_error(sc_metadata_join_csv(meta, csv_short),
               regexp = "B_1", class = sc_metadata_error_class())
})

# ── Câblage mod_sc.R / app.R (contrat §5) ─────────────────────────────────
test_that("mod_sc.R wires the metadata panel (UI + server, additive)", {
  msc <- .ts_read("modules/sc/mod_sc.R")
  expect_match(msc, "mod_sc_metadata_ui(ns(\"metadata\"))", fixed = TRUE)
  expect_match(msc, "mod_sc_metadata_server(  \"metadata\", global_data)",
               fixed = TRUE)
})

test_that("app.R anchors: pure logic sourced before modules, module sourced", {
  app <- .ts_read("app.R")
  expect_match(app, 'source("R/sc/sc_metadata.R")', fixed = TRUE)
  expect_match(app, 'source("modules/sc/mod_sc_metadata.R")', fixed = TRUE)
})

# ── Garde §6 : le commit du design ne touche PAS à sc_obj_epoch ───────────
test_that("metadata module never writes sc_obj_epoch or sc_datasets", {
  mbd <- .ts_read("modules/sc/mod_sc_metadata.R")
  expect_false(grepl("sc_obj_epoch", mbd, fixed = TRUE),
               info = "le design est un re-commit de même lignée — pas de purge")
  expect_false(grepl("sc_datasets", mbd, fixed = TRUE),
               info = "hors fichiers câblage MD-4 (garde non-régression)")
})

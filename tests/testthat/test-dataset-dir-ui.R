# =============================================================================
# test-dataset-dir-ui.R -- PoC « répertoire de datasets » jalon P1 : section
# « Datasets découverts » du module bulk (RED-first)
# =============================================================================
# FICHE (fait foi) :
# docs/proposals/PROPOSAL_DATASET_DIRECTORY_POC_2026-11-02.md
#   §3.3 (surface UI bulk : section « Datasets découverts », bouton Scanner,
#         table, bouton Charger actif si valid — hook mesuré fileInput :107 /
#         smart_read :498 / observer input$counts_file :653+) ;
#   §1 AC3 (un dataset valid se charge via le chemin de chargement existant ;
#         un invalid affiche la raison, bouton désactivé — jamais d'ingestion
#         de non-validé) ; §4 T6 ; §5 (jamais au cold-boot).
#
# LE CHEMIN DE CHARGEMENT (zéro nouveau) : « Charger » appelle l'importeur
# bulk DÉJÀ PUBLIÉ par le module pour le canal drive —
# ts_drive_publish_importer(global_data, "import_bulk", ...) (module :752,
# lu par smart_read puis assemblé par assemble_bulk_obj), récupéré par
# ts_drive_importer_of() (drive_allowlist.R:1945). Le couplage est volontaire :
# la voie UI et la voie agent déposent l'état AU MÊME ENDROT
# (global_data$bulk_obj), exactement comme la règle « deux voies, un dépôt »
# de mod_sc_communication.R.
#
# QU'EST ASSERTABLE HEADLESS (testServer, motif test-mod-import-bulk.R:94-104 :
# les réactifs internes du module sont lisibles dans l'expr) :
#   * le montage ne scanne PAS (AC2/§5) : ds_scan_rv NULL, bulk_obj NULL,
#     aucun log de scan à l'entrée ;
#   * le scan à la demande liste les 3 formes de fixtures (recyclées du
#     gabarit test-dataset-dir-scan.R, réécrites compactes — le gabarit v1
#     4x2 reste le plancher anti-pseudoréplication biomentat_handoff.R:588) ;
#   * le bouton « Charger » est désactivé pour un dataset non valid et la
#     raison v1 est visible ; actif pour un valid (AC3) — asserted sur la
#     STRUCTURE des tags (helper top-level, pas de sérialisation HTML) ;
#   * « Charger » sur un valid déclenche le chargement RÉEL par l'importeur
#     publié (fixture réelle sur disque — aucun stub nécessaire) ;
#   * « Charger » sur un non-valid est REFUSÉ côté serveur (double garde,
#     même bouton désactivé côté UI).
# =============================================================================

source_project_file("tools/pkg_skeleton/R/io_helpers.R")
source_project_file("tools/pkg_skeleton/R/rdata_io.R")
# state.R : state_get/state_set consommés par ts_drive_registry() au montage
# du module — MESURÉ (leçon test-mod-import-bulk.R:26-33) : un test qui ne
# charge pas ses dépendances teste son ordre de chargement, pas le module.
source_project_file("tools/pkg_skeleton/R/state.R")
source_project_file("tools/pkg_skeleton/R/drive_allowlist.R")
source_project_file("tools/pkg_skeleton/R/drive_watcher.R")
source_project_file("modules/import/mod_rdata_picker.R")
source_project_file("tools/pkg_skeleton/R/bulk_import_engine.R")
# Le moteur PoC P0 + sa dépendance runtime (câblée dans app.R au présent jalon)
source_project_file("tools/pkg_skeleton/R/session_checkpoint.R")
source_project_file("tools/pkg_skeleton/R/biomentat_handoff.R")
source_project_file("tools/pkg_skeleton/R/dataset_directory.R")
source_project_file("modules/import/mod_import_bulk.R")

suppressPackageStartupMessages(library(shiny))

# ── Porte RED sur le SOURCE (motif test-mod-import-bulk-drive.R:18-19 :
#    asserted on the SOURCE so the test cannot survive the wiring deletion) ---
.MDD_MOD <- "modules/import/mod_import_bulk.R"
.mdd_src <- paste(readLines(file.path(ts_project_root(), .MDD_MOD),
                            warn = FALSE, encoding = "UTF-8"), collapse = "\n")
.mdd_wired <- grepl("ts_dataset_dir_scan(", .mdd_src, fixed = TRUE) &&
  grepl("btn_scan_datasets", .mdd_src, fixed = TRUE) &&
  grepl("ts_drive_importer_of(", .mdd_src, fixed = TRUE)
.mdd_red_info <- paste0(
  "section « Datasets découverts » absente de ", .MDD_MOD,
  " : PoC répertoire de datasets jalon P1 (fiche ",
  "PROPOSAL_DATASET_DIRECTORY_POC_2026-11-02.md §3.3/AC3/T6) non livré"
)

# ── Fixtures (gabarit v1 compact — 120 gènes, 4x2 pour le valide) ------------

.mdd_root <- function() {
  d <- tempfile(pattern = "datasets_ui_")
  dir.create(d, recursive = TRUE)
  d
}

.mdd_drop <- function(root, id, samples = paste0("s", seq_len(4L)),
                      conds = c("c", "c", "t", "t")) {
  d <- file.path(root, id)
  dir.create(d, recursive = TRUE)
  set.seed(7)
  m <- matrix(as.integer(rpois(120L * length(samples), 300L)),
              nrow = 120L,
              dimnames = list(sprintf("ENSG%011d", seq_len(120L)), samples))
  utils::write.table(as.data.frame(m), file.path(d, "counts.tsv"),
                     sep = "\t", quote = FALSE, col.names = NA)
  utils::write.table(data.frame(sample = samples, condition = conds),
                     file.path(d, "samplesheet.tsv"),
                     sep = "\t", quote = FALSE, row.names = FALSE)
  writeLines(jsonlite::toJSON(list(
    schema_version = "biomentat-exchange/v1", dataset_id = id,
    species = "human", genome_assembly = "GRCh38",
    feature_id_type = "ensembl", matrix_path = "counts.tsv",
    matrix_type = "raw_counts", samplesheet_path = "samplesheet.tsv"),
    auto_unbox = TRUE), file.path(d, "manifest.json"))
  d
}

.mdd_fixtures <- function() {
  root <- .mdd_root()
  .mdd_drop(root, "DS_UI_VALID")
  .mdd_drop(root, "DS_UI_PSEUDO",
            samples = paste0("s", seq_len(3L)), conds = c("c", "c", "t"))
  dir.create(file.path(root, "DS_UI_EMPTY"), recursive = TRUE)
  root
}

.mdd_env <- function() {
  gd <- new.env(parent = emptyenv())
  gd$i18n <- NULL
  gd$drive_registry <- new.env(parent = emptyenv())   # motif app.R:721
  gd
}

# ── T6-0 -- porte RED sur le source ------------------------------------------

test_that("T6-0 : la section PoC est câblée dans mod_import_bulk.R (source)", {
  expect_true(.mdd_wired, info = .mdd_red_info)
})

# ── T6-1 -- AUCUN scan au montage (AC2/§5 : scan à la demande seulement) -----

test_that("T6-1 montage du module : aucun scan, aucun chargement (zéro ingestion automatique)", {
  expect_true(.mdd_wired, info = .mdd_red_info)
  if (!.mdd_wired) return(invisible(NULL))

  root <- .mdd_fixtures()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  Sys.setenv(TRANSCRIPTO_DATASETS_DIR = root)
  on.exit(Sys.unsetenv("TRANSCRIPTO_DATASETS_DIR"), add = TRUE)

  gd <- .mdd_env()
  mount_logs <- NULL
  shiny::testServer(mod_import_bulk_server, args = list(global_data = gd), expr = {
    # Ordre du harnais maison (test-mod-import-bulk.R:100-103) : les inputs
    # d'option AVANT le premier flush — mesuré : `counts_reactive()` fait
    # `if (input$counts_format == "cols")`, et un flush sans counts_format
    # posé lève « argument is of length zero » (NULL == "cols" = logical(0)),
    # ce qui détruit la session de test AVANT toute assertion. Poser des
    # VALEURS d'inputs n'est ni un scan ni un chargement (AC2 intact).
    session$setInputs(bulk_import_mode = "merged_matrix", counts_format = "rows",
                      counts_has_header = TRUE, counts_has_rownames = TRUE,
                      min_counts = 10, project_name = "P")
    session$flushReact()
    expect_null(shiny::isolate(ds_scan_rv()),
                info = "le scan n'a PAS été lancé au montage (AC2)")
    expect_null(shiny::isolate(global_data$bulk_obj),
                info = "aucun chargement au montage (jamais d'ingestion de non-validé)")
    expect_false(any(grepl("dataset", shiny::isolate(logs()), ignore.case = TRUE)),
                 info = "aucun log de scan au montage")
    mount_logs <<- length(shiny::isolate(logs()))
  })
  expect_null(gd$bulk_obj)
})

# ── T6-2 -- scan à la demande : les 3 formes listées (table de scan P0) ------

test_that("T6-2 bouton Scanner : la table de scan P0 liste valid/invalid/empty avec raisons v1", {
  expect_true(.mdd_wired, info = .mdd_red_info)
  if (!.mdd_wired) return(invisible(NULL))

  root <- .mdd_fixtures()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  Sys.setenv(TRANSCRIPTO_DATASETS_DIR = root)
  on.exit(Sys.unsetenv("TRANSCRIPTO_DATASETS_DIR"), add = TRUE)

  gd <- .mdd_env()
  shiny::testServer(mod_import_bulk_server, args = list(global_data = gd), expr = {
    session$setInputs(bulk_import_mode = "merged_matrix", counts_format = "rows",
                      counts_has_header = TRUE, counts_has_rownames = TRUE,
                      min_counts = 10, project_name = "P")
    session$setInputs(btn_scan_datasets = 1L)
    session$flushReact()
    sc <- shiny::isolate(ds_scan_rv())
    expect_false(is.null(sc), info = "le clic sur Scanner lance le scan")
    expect_length(sc, 3L)
    expect_identical(sort(names(sc)),
                     sort(c("DS_UI_VALID", "DS_UI_PSEUDO", "DS_UI_EMPTY")))
    expect_identical(sc$DS_UI_VALID$status, "valid")
    expect_identical(sc$DS_UI_PSEUDO$status, "invalid")
    expect_identical(sc$DS_UI_EMPTY$status, "empty")
    expect_identical(sc$DS_UI_PSEUDO$raison, "pseudoreplication_detected",
                     info = "raison v1 telle quelle, visible pour l'opérateur")
    # le répertoire résolu est exposé (traçabilité opérateur)
    expect_identical(shiny::isolate(ds_dir_rv()),
                     normalizePath(root, winslash = "/", mustWork = FALSE))
  })
})

# ── T6-3 -- bouton Charger désactivé si non-valid, raison visible (AC3) ------

test_that("T6-3 Charger : désactivé sur invalid/empty avec raison visible, actif sur valid (AC3)", {
  expect_true(.mdd_wired, info = .mdd_red_info)
  if (!.mdd_wired) return(invisible(NULL))

  root <- .mdd_fixtures()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  Sys.setenv(TRANSCRIPTO_DATASETS_DIR = root)
  on.exit(Sys.unsetenv("TRANSCRIPTO_DATASETS_DIR"), add = TRUE)

  gd <- .mdd_env()
  shiny::testServer(mod_import_bulk_server, args = list(global_data = gd), expr = {
    session$setInputs(bulk_import_mode = "merged_matrix", counts_format = "rows",
                      counts_has_header = TRUE, counts_has_rownames = TRUE,
                      min_counts = 10, project_name = "P")
    session$setInputs(btn_scan_datasets = 1L)
    session$flushReact()
    sc <- shiny::isolate(ds_scan_rv())
    rows <- .bulk_datasets_table_rows(sc, ns = function(x) x, tr = function(k) k)
    expect_false(is.null(rows))
    # bouton ACTIF uniquement sur le valid
    expect_identical(rows$DS_UI_VALID$loadable, TRUE)
    expect_null(shiny::tagGetAttribute(rows$DS_UI_VALID$button, "disabled"),
                info = "le bouton Charger du dataset VALID est actif")
    # désactivé + raison v1 visible sur l'invalid
    expect_identical(rows$DS_UI_PSEUDO$loadable, FALSE)
    expect_false(is.null(shiny::tagGetAttribute(rows$DS_UI_PSEUDO$button, "disabled")),
                 info = "le bouton Charger du dataset INVALID est désactivé (AC3)")
    row_html <- paste(capture.output(print(rows$DS_UI_PSEUDO$row)), collapse = "\n")
    expect_match(row_html, "pseudoreplication_detected", fixed = TRUE,
                 info = "la raison v1 est affichée à côté du bouton désactivé")
    expect_false(is.null(shiny::tagGetAttribute(rows$DS_UI_EMPTY$button, "disabled")),
                 info = "empty : rien à charger, bouton désactivé")
  })
})

# ── T6-4 -- Charger sur un valid : chargement RÉEL par l'importeur publié ----

test_that("T6-4 Charger sur valid : l'importeur bulk publié charge counts.tsv+samplesheet.tsv dans global_data$bulk_obj", {
  expect_true(.mdd_wired, info = .mdd_red_info)
  if (!.mdd_wired) return(invisible(NULL))

  root <- .mdd_fixtures()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  Sys.setenv(TRANSCRIPTO_DATASETS_DIR = root)
  on.exit(Sys.unsetenv("TRANSCRIPTO_DATASETS_DIR"), add = TRUE)

  gd <- .mdd_env()
  shiny::testServer(mod_import_bulk_server, args = list(global_data = gd), expr = {
    session$setInputs(bulk_import_mode = "merged_matrix", counts_format = "rows",
                      min_counts = 10, project_name = "DS_POC")
    session$setInputs(btn_scan_datasets = 1L)
    session$flushReact()
    session$setInputs(btn_load_ds_DS_UI_VALID = 1L)
    session$flushReact()
    obj <- shiny::isolate(global_data$bulk_obj)
    expect_false(is.null(obj),
                 info = "le chargement passe par le chemin existant : global_data$bulk_obj alimenté")
    expect_identical(ncol(obj$counts), 4L)
    expect_identical(nrow(obj$counts), 120L)
    expect_true(shiny::isolate(grep("DS_UI_VALID|charg|load",
                                    shiny::isolate(logs()), ignore.case = TRUE)) > 0L,
                info = "le log trace le chargement du dataset")
  })
})

# ── T6-5 -- double garde : Charger sur un non-valid est REFUSÉ côté serveur --

test_that("T6-5 double garde : un clic (forcé) sur le bouton d'un invalid est refusé, bulk_obj intact", {
  expect_true(.mdd_wired, info = .mdd_red_info)
  if (!.mdd_wired) return(invisible(NULL))

  root <- .mdd_fixtures()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  Sys.setenv(TRANSCRIPTO_DATASETS_DIR = root)
  on.exit(Sys.unsetenv("TRANSCRIPTO_DATASETS_DIR"), add = TRUE)

  gd <- .mdd_env()
  shiny::testServer(mod_import_bulk_server, args = list(global_data = gd), expr = {
    session$setInputs(bulk_import_mode = "merged_matrix", counts_format = "rows",
                      min_counts = 10, project_name = "DS_POC")
    session$setInputs(btn_scan_datasets = 1L)
    session$flushReact()
    # Le bouton est désactivé côté UI ; on FORCE quand même l'input pour
    # prouver que le serveur refuse indépendamment de l'UI (AC3 : jamais
    # d'ingestion de non-validé).
    session$setInputs(btn_load_ds_DS_UI_PSEUDO = 1L)
    session$flushReact()
    expect_null(shiny::isolate(global_data$bulk_obj),
                info = "RIEN n'est chargé depuis un dataset non valid (AC3)")
    expect_true(any(grepl("pseudoreplication_detected|DS_UI_PSEUDO",
                          shiny::isolate(logs()))),
                info = "le refus est tracé avec la raison v1")
  })
})

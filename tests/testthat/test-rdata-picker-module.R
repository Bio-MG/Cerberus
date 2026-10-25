# =============================================================================
# test-rdata-picker-module.R — Composant mutualisé mod_rdata_picker
# =============================================================================
# Pattern : shiny::testServer sur le module serveur (fixtures .rda dans
# tempdir, commit_fn enregistreur). Le flux SC hôte est testé en intégration
# (mod_import_sc_server) pour le chemin auto-import objet unique.
# =============================================================================

source_project_file("tools/pkg_skeleton/R/io_helpers.R")   # %||%
suppressPackageStartupMessages({
  library(shiny)       # testServer, reactiveVal
  library(bslib)       # layout_columns
  library(DT)
  library(bsicons)
  library(shinyFiles)  # getVolumes (mod_import_sc)
  library(Seurat)      # prepare_seurat_object (attaché par global.R en prod)
})
source_project_file("R/core/rdata_io.R")
source_project_file("modules/import/mod_rdata_picker.R")

.tmpdir <- tempfile(pattern = "rdata_picker_")
dir.create(.tmpdir, showWarnings = FALSE)

mat <- matrix(1:12, 3, 4)
single_path <- file.path(.tmpdir, "single_mat.rda")
save(mat, file = single_path)

grob <- grid::rectGrob()
single_bad_path <- file.path(.tmpdir, "single_grob.rda")
save(grob, file = single_bad_path)

meta_df <- data.frame(a = 1:3)
other <- "texte"
multi_path <- file.path(.tmpdir, "multi.rda")
save(mat, meta_df, other, file = multi_path)

# Cas d'usage CellChat tutorial : objet unique = liste imbriquee
skin <- list(
  data = list(NL = matrix(1:6, 2, 3), LS = matrix(7:12, 2, 3)),
  meta = data.frame(condition = c("NL", "LS"))
)
skin_path <- file.path(.tmpdir, "data_humanSkin_CellChat.rda")
save(skin, file = skin_path)

mat_rds_path <- file.path(.tmpdir, "single_mat.rds")
saveRDS(mat, mat_rds_path)

# Enregistreur de commits (l'hôte doit stop() en cas d'échec de commit)
.new_recorder <- function() {
  env <- new.env(parent = emptyenv())
  env$commits <- list()
  env$commit_fn <- function(obj, obj_name) {
    env$commits[[length(env$commits) + 1L]] <<- list(obj = obj, name = obj_name)
  }
  env
}

# ── Composant seul ───────────────────────────────────────────────────────────
test_that("picker: objet unique compatible -> commit automatique", {
  rec <- .new_recorder()
  logs <- character(0)
  file_rv <- reactiveVal(NULL)

  testServer(rdata_picker_server, args = list(
    file_rv = file_rv, commit_fn = rec$commit_fn,
    expected = c("matrix", "dgCMatrix"),
    log_fn = function(m) logs <<- c(logs, m)
  ), expr = {
    file_rv(list(datapath = single_path, name = "single_mat.rda", size = 100))
    session$flushReact()
    expect_length(rec$commits, 1L)
    expect_identical(rec$commits[[1L]]$name, "mat")
    expect_identical(dim(rec$commits[[1L]]$obj), c(3L, 4L))
  })
  expect_true(any(grepl("mat", logs)))
})

test_that("picker: objet unique incompatible -> preview forcée, pas de commit", {
  rec <- .new_recorder()
  file_rv <- reactiveVal(NULL)

  testServer(rdata_picker_server, args = list(
    file_rv = file_rv, commit_fn = rec$commit_fn,
    expected = c("Seurat", "matrix"),
    log_fn = function(m) NULL
  ), expr = {
    file_rv(list(datapath = single_bad_path, name = "single_grob.rda", size = 100))
    session$flushReact()
    expect_length(rec$commits, 0L)
    # la carte de preview est rendue malgré l'objet unique
    card <- session$output$picker_card
    expect_true(is.character(card) || is.list(card))
  })
})

test_that("picker: multi-objets -> preview + import sur sélection exacte", {
  rec <- .new_recorder()
  file_rv <- reactiveVal(NULL)

  testServer(rdata_picker_server, args = list(
    file_rv = file_rv, commit_fn = rec$commit_fn,
    expected = c("matrix", "dgCMatrix", "data.frame"),
    log_fn = function(m) NULL
  ), expr = {
    file_rv(list(datapath = multi_path, name = "multi.rda", size = 100))
    session$flushReact()
    expect_length(rec$commits, 0L)

    # Import sans sélection -> refus (notification), pas de commit
    session$setInputs(import_btn = 1L)
    expect_length(rec$commits, 0L)

    # Sélection d'une ligne (la table multi : mat=1, meta_df=2, other=3)
    session$setInputs(preview_table_rows_selected = 1L)
    session$setInputs(import_btn = 2L)
    expect_length(rec$commits, 1L)
    expect_identical(rec$commits[[1L]]$name, "mat")

    # Sélection du data.frame : classe acceptée -> commit
    session$setInputs(preview_table_rows_selected = 2L)
    session$setInputs(import_btn = 3L)
    expect_length(rec$commits, 2L)
    expect_identical(rec$commits[[2L]]$name, "meta_df")
  })
})

test_that("picker: changement de fichier libère l'ancien env (rdata_free)", {
  rec <- .new_recorder()
  file_rv <- reactiveVal(NULL)
  testServer(rdata_picker_server, args = list(
    file_rv = file_rv, commit_fn = rec$commit_fn,
    expected = c("matrix", "dgCMatrix"),
    log_fn = function(m) NULL
  ), expr = {
    file_rv(list(datapath = single_path, name = "a.rda", size = 1))
    session$flushReact()
    expect_length(rec$commits, 1L)
    file_rv(NULL)
    session$flushReact()
    file_rv(list(datapath = multi_path, name = "b.rda", size = 1))
    session$flushReact()
    # pas de commit auto en multi ; les commits précédents ne se dupliquent pas
    expect_length(rec$commits, 1L)
  })
})

# ── Intégration hôte SC (mod_import_sc_server) ───────────────────────────────
# L'auto-import d'un .rda à objet unique traverse le composant puis
# prepare_seurat_object -> global_data$sc_obj (précédent commit des imports).
source_project_file("modules/import/mod_import_sc.R")

test_that("hôte SC: .rda objet unique (matrice) -> global_data$sc_obj Seurat", {
  gd <- new.env(parent = emptyenv())
  gd$i18n <- NULL

  testServer(mod_import_sc_server, args = list(global_data = gd), expr = {
    session$setInputs(single_file_upload = list(
      name = "single_mat.rda", size = as.integer(file.info(single_path)$size),
      datapath = single_path
    ))
    session$flushReact()
    expect_false(is.null(gd$sc_obj))
    expect_s4_class(gd$sc_obj, "Seurat")
    expect_equal(ncol(gd$sc_obj), 4L)
    expect_equal(nrow(gd$sc_obj), 3L)
  })
})

test_that("hôte SC: .rda multi-objets en Option C -> preview, pas de commit direct", {
  gd <- new.env(parent = emptyenv())
  gd$i18n <- NULL

  testServer(mod_import_sc_server, args = list(global_data = gd), expr = {
    session$setInputs(single_file_upload = list(
      name = "multi.rda", size = as.integer(file.info(multi_path)$size),
      datapath = multi_path
    ))
    session$flushReact()
    expect_null(gd$sc_obj)
  })
})

# ── Intégration hôte Bulk (mod_import_bulk_server) ───────────────────────────
# Dépôt .rda (objet unique = matrice) dans le slot counts -> auto-commit ->
# counts_reactive produit la matrice gènes × échantillons (aperçu rendu).
source_project_file("modules/import/mod_import_bulk.R")

test_that("hôte Bulk: .rda matrice unique slot counts -> aperçu counts rendu", {
  gd <- new.env(parent = emptyenv())
  gd$i18n <- NULL

  testServer(mod_import_bulk_server, args = list(global_data = gd), expr = {
    # Simuler les inputs a valeur par defaut du vrai app (radio/checkbox/numeric)
    session$setInputs(bulk_import_mode = "merged_matrix", counts_format = "rows",
                      counts_has_header = TRUE, counts_has_rownames = TRUE,
                      min_counts = 10, project_name = "BulkRNA_Project")
    session$setInputs(counts_file = list(
      name = "single_mat.rda", size = as.integer(file.info(single_path)$size),
      datapath = single_path
    ))
    session$flushReact()
    # flux .rda : counts_reactive expose la matrice depuis l'objet auto-commité
    df <- shiny::isolate(counts_reactive())
    expect_false(is.null(df))
    expect_equal(dim(df), c(3L, 4L))
    expect_match(logs(), "Matrice de counts .RData", fixed = TRUE)
  })
})

# ── Exploration imbriquée (cas data_humanSkin_CellChat.rda) ─────────────────
test_that("picker: liste imbriquée unique -> preview aplatie, pas de commit auto", {
  rec <- .new_recorder()
  file_rv <- reactiveVal(NULL)

  testServer(rdata_picker_server, args = list(
    file_rv = file_rv, commit_fn = rec$commit_fn,
    expected = c("Seurat", "SingleCellExperiment", "matrix", "dgCMatrix",
                 "dgTMatrix", "data.frame"),
    log_fn = function(m) NULL
  ), expr = {
    file_rv(list(datapath = skin_path, name = "data_humanSkin_CellChat.rda",
                 size = 100))
    session$flushReact()
    # le conteneur liste n'est PAS importé automatiquement : preview
    expect_length(rec$commits, 0L)
    card_text <- paste(session$output$picker_card)
    expect_true(any(grepl("preview_table", card_text)))
  })
})

test_that("picker: import d'une feuille imbriquée par chemin (skin$data$NL)", {
  rec <- .new_recorder()
  file_rv <- reactiveVal(NULL)

  testServer(rdata_picker_server, args = list(
    file_rv = file_rv, commit_fn = rec$commit_fn,
    expected = c("Seurat", "SingleCellExperiment", "matrix", "dgCMatrix",
                 "dgTMatrix", "data.frame"),
    log_fn = function(m) NULL
  ), expr = {
    file_rv(list(datapath = skin_path, name = "data_humanSkin_CellChat.rda",
                 size = 100))
    session$flushReact()
    # lignes aplaties : 1=skin, 2=skin$data, 3=skin$data$NL, 4=skin$data$LS,
    # 5=skin$meta — la feuille NL est importable, le conteneur skin est refusé
    session$setInputs(preview_table_rows_selected = 1L)
    session$setInputs(import_btn = 1L)
    expect_length(rec$commits, 0L)  # conteneur liste -> classe refusée

    session$setInputs(preview_table_rows_selected = 3L)
    session$setInputs(import_btn = 2L)
    expect_length(rec$commits, 1L)
    expect_identical(rec$commits[[1L]]$name, "skin$data$NL")
    expect_equal(dim(rec$commits[[1L]]$obj), c(2L, 3L))

    session$setInputs(preview_table_rows_selected = 5L)
    session$setInputs(import_btn = 3L)
    expect_length(rec$commits, 2L)
    expect_identical(rec$commits[[2L]]$name, "skin$meta")
  })
})

test_that("hôte SC: .rds liste imbriquée -> preview ; .rds matrice -> auto-import", {
  gd1 <- new.env(parent = emptyenv()); gd1$i18n <- NULL
  testServer(mod_import_sc_server, args = list(global_data = gd1), expr = {
    session$setInputs(single_file_upload = list(
      name = "container.rds", size = as.integer(file.info(skin_path)$size),
      datapath = skin_path  # un .rda : les .rds passent par le même chemin
    ))
    session$flushReact()
    expect_null(gd1$sc_obj)  # liste imbriquée -> preview, pas de commit
  })

  gd2 <- new.env(parent = emptyenv()); gd2$i18n <- NULL
  rds_path <- file.path(.tmpdir, "single_mat.rds")
  testServer(mod_import_sc_server, args = list(global_data = gd2), expr = {
    session$setInputs(single_file_upload = list(
      name = "single_mat.rds", size = as.integer(file.info(rds_path)$size),
      datapath = rds_path
    ))
    session$flushReact()
    expect_false(is.null(gd2$sc_obj))   # feuille unique -> auto-import
    expect_s4_class(gd2$sc_obj, "Seurat")
  })
})

unlink(.tmpdir, recursive = TRUE)

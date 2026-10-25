# =============================================================================
# test-core-rdata.R — Inspect & Select .rda/.RData (R/core/rdata_io.R)
# =============================================================================
# Scope : helpers purs hors Shiny — chargement en environnement isole,
# description/classification, extraction, validation de classe, export d'une
# selection, liberation. Les objets "Seurat"/"SingleCellExperiment"/"CellChat"
# sont simules par des objets S3 factices (la classification ne repose que sur
# les classes) : pas de dependance Bioconductor ni d'objet lourd ici.
# Contrat : docs/contracts/RDATA_IMPORT_CONTRACT.md.
# =============================================================================

source_project_file("tools/pkg_skeleton/R/rdata_io.R")

# ---------------------------------------------------------------------------
# Fixtures : workspace mixte typique (save.image d'un biologiste)
# ---------------------------------------------------------------------------
.tmpdir <- tempfile(pattern = "rdata_test_")
dir.create(.tmpdir, showWarnings = FALSE)

fake_seurat <- structure(list(assays = list()), class = c("Seurat", "S3"))
fake_sce <- structure(list(), class = c("SingleCellExperiment", "SummarizedExperiment"))
fake_cellchat <- structure(list(), class = c("CellChat", "S4"))
vel_list <- list(spliced = matrix(1:6, 2, 3), unspliced = matrix(1:6, 2, 3),
                 cell_names = paste0("c", 1:2))
counts_dense <- matrix(1:12, 3, 4, dimnames = list(paste0("g", 1:3), paste0("c", 1:4)))
counts_sparse <- Matrix::Matrix(1:12, 3, 4, sparse = TRUE)
meta_df <- data.frame(sample = c("A", "A", "B"), row.names = paste0("c", 1:3))
img_grob <- grid::rectGrob()

ws_path <- file.path(.tmpdir, "workspace.rda")
save(counts_dense, meta_df, vel_list, img_grob,
     fake_seurat, fake_sce, fake_cellchat,
     file = ws_path)

sparse_path <- file.path(.tmpdir, "sparse.rda")
save(counts_sparse, file = sparse_path)

empty_path <- file.path(.tmpdir, "empty.rda")
save(list = character(0), file = empty_path)

# ---------------------------------------------------------------------------
# rdata_supported_extensions() / rdata_is_supported_file()
# ---------------------------------------------------------------------------
test_that("rdata_supported_extensions defaults to rda/rdata and honors TS_*", {
  expect_setequal(rdata_supported_extensions(), c("rda", "rdata"))
  old <- if (exists("TS_IMPORT_RDA_EXTENSIONS", envir = globalenv())) {
    get("TS_IMPORT_RDA_EXTENSIONS", envir = globalenv())
  } else NULL
  assign("TS_IMPORT_RDA_EXTENSIONS", c(".RData"), envir = globalenv())
  expect_setequal(rdata_supported_extensions(), c("rdata"))
  if (is.null(old)) rm(TS_IMPORT_RDA_EXTENSIONS, envir = globalenv()) else {
    assign("TS_IMPORT_RDA_EXTENSIONS", old, envir = globalenv())
  }
  expect_setequal(rdata_supported_extensions(), c("rda", "rdata"))
})

test_that("rdata_is_supported_file accepts .rda/.RData and rejects others", {
  expect_true(rdata_is_supported_file("foo.RData"))
  expect_true(rdata_is_supported_file("foo.rda"))
  expect_false(rdata_is_supported_file("foo.rds"))
  expect_false(rdata_is_supported_file(NULL))
  expect_false(rdata_is_supported_file(""))
})

# ---------------------------------------------------------------------------
# rdata_load_env()
# ---------------------------------------------------------------------------
test_that("rdata_load_env isolates the workspace (never globalenv)", {
  env <- rdata_load_env(ws_path)
  expect_true(is.environment(env))
  nms <- sort(ls(envir = env))
  expect_setequal(nms, c("counts_dense", "fake_cellchat", "fake_sce",
                         "fake_seurat", "img_grob", "meta_df", "vel_list"))
  # isolation : le parent est emptyenv, rien ne fuite vers globalenv
  expect_identical(parent.env(env), emptyenv())
  expect_false(exists("counts_dense", inherits = FALSE))
})

test_that("rdata_load_env fails cleanly on missing/unreadable/empty files", {
  expect_error(rdata_load_env(file.path(.tmpdir, "nope.rda")),
               class = "rdata_import_error")
  writeLines("ceci n'est pas un fichier R", file.path(.tmpdir, "not_rda.rda"))
  # load() râle d'abord sur le magic number (avertissement attendu, supprimé)
  suppressWarnings(expect_error(rdata_load_env(file.path(.tmpdir, "not_rda.rda")),
               class = "rdata_import_error"))
  expect_error(rdata_load_env(empty_path),
               "ne contient aucun objet", class = "rdata_import_error")
  expect_error(rdata_load_env(NULL), class = "rdata_import_error")
})

# ---------------------------------------------------------------------------
# rdata_classify_object()
# ---------------------------------------------------------------------------
test_that("rdata_classify_object returns the contract type codes", {
  expect_identical(rdata_classify_object(fake_seurat), "seurat")
  expect_identical(rdata_classify_object(fake_sce), "sce")
  expect_identical(rdata_classify_object(fake_cellchat), "cellchat")
  expect_identical(rdata_classify_object(vel_list), "velocity")
  expect_identical(rdata_classify_object(meta_df), "metadata")
  expect_identical(rdata_classify_object(counts_dense), "matrix")
  expect_identical(rdata_classify_object(img_grob), "image")
  expect_identical(rdata_classify_object("chaine"), "other")
  expect_identical(rdata_classify_object(list(1, 2)), "other")
})

# ---------------------------------------------------------------------------
# rdata_describe_objects()
# ---------------------------------------------------------------------------
test_that("rdata_describe_objects builds the preview table", {
  env <- rdata_load_env(ws_path)
  on.exit(rdata_free(env))
  info <- rdata_describe_objects(env)
  expect_s3_class(info, "data.frame")
  expect_setequal(names(info),
                  c("name", "class", "dimensions", "size_mb", "type_code"))
  expect_setequal(info$name, c("counts_dense", "meta_df", "vel_list",
                               "img_grob", "fake_seurat", "fake_sce",
                               "fake_cellchat"))
  row_dense <- info[info$name == "counts_dense", ]
  expect_identical(row_dense$dimensions, "3 x 4")
  expect_identical(row_dense$type_code, "matrix")
  expect_true(row_dense$size_mb >= 0)
  expect_identical(info[info$name == "vel_list", "type_code"], "velocity")
  expect_identical(info[info$name == "img_grob", "dimensions"], "-")
})

test_that("rdata_describe_objects handles sparse matrices and empty envs", {
  env <- rdata_load_env(sparse_path)
  on.exit(rdata_free(env))
  info <- rdata_describe_objects(env)
  expect_identical(info$type_code, "matrix")
  expect_identical(info$dimensions, "3 x 4")
  expect_error(rdata_describe_objects(new.env(parent = emptyenv())), NA)
  expect_identical(nrow(rdata_describe_objects(new.env())), 0L)
  expect_error(rdata_describe_objects("pas_un_env"),
               class = "rdata_import_error")
})

# ---------------------------------------------------------------------------
# rdata_extract_object()
# ---------------------------------------------------------------------------
test_that("rdata_extract_object returns the requested object", {
  env <- rdata_load_env(ws_path)
  on.exit(rdata_free(env))
  mat <- rdata_extract_object(env, "counts_dense")
  expect_identical(dim(mat), c(3L, 4L))
  expect_identical(rownames(mat), paste0("g", 1:3))
})

test_that("rdata_extract_object fails with typed French errors", {
  env <- rdata_load_env(ws_path)
  on.exit(rdata_free(env))
  expect_error(rdata_extract_object(env, "inconnu"),
               "introuvable", class = "rdata_import_error")
  expect_error(rdata_extract_object(env, character(0)),
               class = "rdata_import_error")
  expect_error(rdata_extract_object(env, c("a", "b")),
               class = "rdata_import_error")
  expect_error(rdata_extract_object("pas_un_env", "x"),
               class = "rdata_import_error")
})

# ---------------------------------------------------------------------------
# rdata_assert_class()
# ---------------------------------------------------------------------------
test_that("rdata_assert_class validates BEFORE import (pipable, invisible)", {
  env <- rdata_load_env(ws_path)
  on.exit(rdata_free(env))
  mat <- rdata_extract_object(env, "counts_dense")
  res <- rdata_assert_class(mat, c("matrix", "dgCMatrix"), context = "test")
  expect_identical(res, mat)   # invisible mais bien l'objet
  # NULL expected = verification desactivee
  expect_identical(rdata_assert_class("x", NULL), "x")
})

test_that("rdata_assert_class stops with a clear French message", {
  env <- rdata_load_env(ws_path)
  on.exit(rdata_free(env))
  plot_obj <- rdata_extract_object(env, "img_grob")
  err <- tryCatch(
    rdata_assert_class(plot_obj, c("Seurat", "matrix"), context = "import single-cell"),
    error = function(e) e)
  expect_s3_class(err, "simpleError")
  expect_match(conditionMessage(err), "import single-cell", fixed = TRUE)
  expect_match(conditionMessage(err), "grob", fixed = TRUE)
  expect_match(conditionMessage(err), "Seurat, matrix", fixed = TRUE)
  expect_error(rdata_assert_class(NULL, "Seurat", context = "t"),
               "NULL", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# rdata_export_selection() — "sauvegarder en dehors", jamais dans global_data
# ---------------------------------------------------------------------------
test_that("rdata_export_selection writes a reloadable .RData subset", {
  env <- rdata_load_env(ws_path)
  on.exit(rdata_free(env))
  out <- file.path(.tmpdir, "selection.rda")
  exported <- rdata_export_selection(env, c("counts_dense", "meta_df", "inconnu"), out)
  expect_setequal(exported, c("counts_dense", "meta_df"))
  expect_true(file.exists(out))

  env2 <- new.env(parent = emptyenv())
  load(out, envir = env2)
  expect_setequal(ls(envir = env2), c("counts_dense", "meta_df"))
  expect_identical(dim(get("counts_dense", envir = env2)), c(3L, 4L))
})

test_that("rdata_export_selection fails when nothing valid is selected", {
  env <- rdata_load_env(ws_path)
  on.exit(rdata_free(env))
  expect_error(rdata_export_selection(env, "inconnu",
                                      file.path(.tmpdir, "rien.rda")),
               class = "rdata_import_error")
  expect_error(rdata_export_selection("pas_un_env", "x",
                                      file.path(.tmpdir, "rien.rda")),
               class = "rdata_import_error")
})

# ---------------------------------------------------------------------------
# rdata_free()
# ---------------------------------------------------------------------------
test_that("rdata_free empties the environment and tolerates NULL", {
  env <- rdata_load_env(ws_path)
  expect_length(ls(envir = env), 7L)
  rdata_free(env)
  expect_length(ls(envir = env, all.names = TRUE), 0L)
  expect_silent(rdata_free(NULL))
})

# nettoyage
# ---------------------------------------------------------------------------
# Exploration : rdata_flatten_env / rdata_extract_path / read_file_env
# (cas d'usage : data_humanSkin_CellChat.rda — objet unique = liste imbriquee)
# ---------------------------------------------------------------------------
skin <- list(
  data = list(NL = matrix(1:6, 2, 3), LS = matrix(7:12, 2, 3)),
  meta = data.frame(condition = c("NL", "LS")),
  source = "tutorial"
)
skin_path <- file.path(.tmpdir, "data_humanSkin_CellChat.rda")
save(skin, file = skin_path)

mat_rds_path <- file.path(.tmpdir, "single_mat.rds")
saveRDS(counts_dense, mat_rds_path)

test_that("rdata_flatten_env explore les listes imbriquees en chemins", {
  env <- rdata_load_env(skin_path)
  on.exit(rdata_free(env))
  flat <- rdata_flatten_env(env)
  expect_setequal(flat$name, c(
    "skin", "skin$data", "skin$data$NL", "skin$data$LS",
    "skin$meta", "skin$source"
  ))
  expect_identical(flat$type_code[flat$name == "skin$data$NL"], "matrix")
  expect_identical(flat$type_code[flat$name == "skin$meta"], "metadata")
  expect_identical(flat$dimensions[flat$name == "skin$data$NL"], "2 x 3")
})

test_that("rdata_flatten_env respecte max_depth et max_rows", {
  env <- rdata_load_env(skin_path)
  on.exit(rdata_free(env))
  flat1 <- rdata_flatten_env(env, max_depth = 1L)
  expect_setequal(flat1$name, "skin")
  deep <- list(a = list(b = list(c = list(d = list(e = matrix(1))))))
  env2 <- new.env(parent = emptyenv()); assign("deep", deep, envir = env2)
  flat_deep <- rdata_flatten_env(env2, max_depth = 3L)
  expect_false("deep$a$b$c" %in% flat_deep$name)  # 4 niveaux > max_depth
  expect_true("deep$a$b" %in% flat_deep$name)     # 3 niveaux = max_depth
  flat_cap <- rdata_flatten_env(env2, max_rows = 2L)
  expect_identical(nrow(flat_cap), 2L)
})

test_that("rdata_extract_path extrait par chemin et echoue proprement", {
  env <- rdata_load_env(skin_path)
  on.exit(rdata_free(env))
  nl <- rdata_extract_path(env, "skin$data$NL")
  expect_identical(dim(nl), c(2L, 3L))
  expect_identical(rdata_extract_path(env, "skin"), skin)
  expect_error(rdata_extract_path(env, "skin$data$INEXISTANT"),
               "introuvable", class = "rdata_import_error")
  expect_error(rdata_extract_path(env, "inconnu"),
               class = "rdata_import_error")
  expect_error(rdata_extract_path(env, character(0)),
               class = "rdata_import_error")
})

test_that("rdata_read_file_env accepte .rda et .rds (nom R valide)", {
  env_rda <- rdata_read_file_env(skin_path)
  on.exit(rdata_free(env_rda))
  expect_setequal(ls(envir = env_rda), "skin")

  env_rds <- rdata_read_file_env(mat_rds_path)
  on.exit(rdata_free(env_rds), add = TRUE)
  expect_setequal(ls(envir = env_rds), "single_mat")
  expect_identical(dim(rdata_extract_object(env_rds, "single_mat")),
                   c(3L, 4L))

  expect_true(rdata_is_explorable_file("a.rda"))
  expect_true(rdata_is_explorable_file("a.RData"))
  expect_true(rdata_is_explorable_file("a.rds"))
  expect_false(rdata_is_explorable_file("a.h5ad"))
  expect_error(rdata_read_file_env("inexistant.rds"),
               class = "rdata_import_error")
})

test_that("rdata_export_paths bundle des chemins imbriques en .RData", {
  env <- rdata_load_env(skin_path)
  on.exit(rdata_free(env))
  out <- file.path(.tmpdir, "paths_bundle.rda")
  nms <- rdata_export_paths(env, c("skin$data$NL", "skin$meta"), out)
  expect_setequal(nms, c("skin_data_NL", "skin_meta"))
  env2 <- new.env(parent = emptyenv())
  load(out, envir = env2)
  expect_setequal(ls(envir = env2), c("skin_data_NL", "skin_meta"))
  expect_identical(dim(get("skin_data_NL", envir = env2)), c(2L, 3L))
})

unlink(.tmpdir, recursive = TRUE)

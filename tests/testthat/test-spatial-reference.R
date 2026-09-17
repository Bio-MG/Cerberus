# =============================================================================
# test-spatial-reference.R — tests for R/spatial/spatial_reference.R
# =============================================================================
# 16ᵉ incrément de la dette de conventions (2026-09-17).
# Créé AVANT la conversion (règle 5 : test d'abord, rouge d'abord) ⇒ ce
# fichier doit ÉCHOUER sur les 12 assertions de classe tant que les `stop()`
# ne portent pas `class = "spatial_reference_error"`.
#
# ⚠️ Test ÉPONYME et non étendu : aucun test ne portait ce nom, et
# `test-rda-spatial-bulk.R` se contente de *sourcer* le fichier (pour ses
# propres fixtures) sans rien en assérer ⇒ pas de doublon (règle 3).
#
# ⚠️ 3 des 12 sites sont MULTI-ARGUMENTS (36, 87, 106) : sans `paste0()`,
# `errorCondition()` ne garde que le premier argument. Les assertions portent
# donc sur le message ENTIER, concaténation comprise — c'est le seul détecteur
# de troncature (§2bn/§2bo).
# =============================================================================

source_project_file("R/core/io_helpers.R")        # %||%
source_project_file("R/core/rdata_io.R")          # rdata_is_supported_file / rdata_load_env
source_project_file("R/spatial/spatial_reference.R")

.ref_err <- function(expr) {
  tryCatch({ expr; NULL },
           error = function(e) list(msg = conditionMessage(e), class = class(e)))
}
.ref_expect <- function(e, msg) {
  expect_identical(e$msg, msg)
  expect_true("spatial_reference_error" %in% e$class)
}
# Garde `requireNamespace()` atteignable même si le paquet EST installé
# (`with_mocked_bindings(.env = globalenv())` échoue — §2bz.3).
.ref_no_pkg <- function(fun) {
  e <- new.env(parent = globalenv())
  e$requireNamespace <- function(package, ...) FALSE
  f <- get(fun, envir = globalenv())
  environment(f) <- e
  f
}

# ---------------------------------------------------------------------------
# read_reference_scrna()
# ---------------------------------------------------------------------------
test_that("read_reference_scrna : fichier absent (36 — MULTI-ARGUMENTS)", {
  # ⚠️ Le message est `stop("Fichier de reference introuvable : ", path)` : sans
  # `paste0()`, il ne resterait que "Fichier de reference introuvable : ".
  .ref_expect(.ref_err(read_reference_scrna("/chemin/inexistant/reference.rds")),
              "Fichier de reference introuvable : /chemin/inexistant/reference.rds")
})

test_that("read_reference_scrna : .rds inexploitable (48)", {
  f <- tempfile(fileext = ".rds"); saveRDS("hello", f)
  .ref_expect(.ref_err(read_reference_scrna(f)),
              ".rds ne contient ni objet Seurat, ni liste counts/meta, ni matrice exploitable.")
})

test_that("read_reference_scrna : .RData inexploitable (75)", {
  # ⚠️ Il faut franchir la garde « > 1 objet » du site 60 (déjà conforme,
  # `call. = FALSE`) : un .RData à UN SEUL objet, non Seurat et sans $counts.
  f <- tempfile(fileext = ".RData")
  junk <- structure(list(a = 1), class = "toutafaitautre")
  save(junk, file = f)
  .ref_expect(.ref_err(read_reference_scrna(f)),
              ".RData ne contient ni objet Seurat, ni liste counts/meta, ni matrice exploitable.")
})

test_that("read_reference_scrna : aucun lecteur .h5ad (87 — MULTI-ARGUMENTS)", {
  f <- tempfile(fileext = ".h5ad"); writeLines("x", f)
  # ⚠️ Message en DEUX littéraux accolés : sans `paste0()`, il s'arrête après
  # « installez 'schard' ». L'assertion porte donc sur la concaténation.
  .ref_expect(.ref_err(.ref_no_pkg("read_reference_scrna")(f)),
              paste0("Aucun lecteur .h5ad disponible -- installez 'schard' ",
                     "(remotes::install_github('cellgeni/schard'), recommande) ou 'SeuratDisk'."))
})

test_that("read_reference_scrna : hdf5r absent (93)", {
  f <- tempfile(fileext = ".h5"); writeLines("x", f)
  .ref_expect(.ref_err(.ref_no_pkg("read_reference_scrna")(f)),
              "Package 'hdf5r' requis pour lire les fichiers .h5.")
})

test_that("read_reference_scrna : aucun lecteur .loom (106 — MULTI-ARGUMENTS)", {
  f <- tempfile(fileext = ".loom"); writeLines("x", f)
  .ref_expect(.ref_err(.ref_no_pkg("read_reference_scrna")(f)),
              paste0("Aucun lecteur .loom disponible -- installez 'SeuratDisk' ",
                     "(remotes::install_github('mojaveazure/seurat-disk'))."))
})

test_that("read_reference_scrna : extension non supportee (110)", {
  f <- tempfile(fileext = ".txt"); writeLines("x", f)
  .ref_expect(.ref_err(read_reference_scrna(f)),
              "Format de reference non supporte : '.txt' (attendu : .rds, .h5ad, .h5, .loom).")
})

# ---------------------------------------------------------------------------
# prepare_reference_seurat()
# ---------------------------------------------------------------------------
test_that("prepare_reference_seurat : format non reconnu (132)", {
  .ref_expect(.ref_err(prepare_reference_seurat(list(a = 1))),
              "Format de reference non reconnu apres lecture (ni objet Seurat, ni liste counts/meta).")
})

# ---------------------------------------------------------------------------
# prepare_reference_artifact()
# ---------------------------------------------------------------------------
test_that("prepare_reference_artifact : objet non Seurat (188)", {
  .ref_expect(.ref_err(prepare_reference_artifact(list(a = 1), "ct")),
              "prepare_reference_artifact() attend un objet Seurat.")
})

.ref_obj <- function(n = 15L, labs = rep("A", n)) {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("Matrix")
  suppressWarnings(suppressPackageStartupMessages(library(Seurat)))
  cnt <- Matrix::Matrix(matrix(1L, nrow = 3L, ncol = n,
        dimnames = list(c("G1", "G2", "G3"), paste0("c", seq_len(n)))), sparse = TRUE)
  o <- suppressWarnings(CreateSeuratObject(cnt, project = "ref"))
  o@meta.data$ct <- labs
  o
}

test_that("prepare_reference_artifact : colonne absente (190)", {
  .ref_expect(.ref_err(prepare_reference_artifact(.ref_obj(), "colonne_absente")),
              "Colonne 'colonne_absente' absente des metadonnees de la reference.")
})

test_that("prepare_reference_artifact : moins de 10 cellules annotees (200)", {
  # ⚠️ Il faut < 10 libellés non-NA : 5 sur 15.
  o <- .ref_obj(15L, c(rep("A", 5), rep(NA_character_, 10)))
  .ref_expect(.ref_err(prepare_reference_artifact(o, "ct")),
              "Moins de 10 cellules annotees (non-NA) dans la colonne choisie -- reference inexploitable.")
})

test_that("prepare_reference_artifact : tout filtre par rarete (223)", {
  # ⚠️ Il faut franchir 200 (≥ 10 annotés) puis que le filtrage vide tout :
  # 15 cellules d'un seul type, sous `min_cells_per_type = 25` ⇒ fusion en
  # "Autre", puis "Autre" (15 < 25) est SUPPRIMÉ ⇒ 0 < 10.
  o <- .ref_obj(15L, rep("A", 15))
  .ref_expect(.ref_err(prepare_reference_artifact(o, "ct")),
              "Moins de 10 cellules restantes apres filtrage des types trop rares -- reference inexploitable.")
})

# ---------------------------------------------------------------------------
# Garde-fou du mock de dépendance (§2bz.3) : il ne doit PAS fuiter
# ---------------------------------------------------------------------------
test_that("le mock de dependance ne fuit pas dans globalenv", {
  f <- tempfile(fileext = ".h5"); writeLines("x", f)
  invisible(tryCatch(.ref_no_pkg("read_reference_scrna")(f), error = function(e) NULL))
  expect_false(exists("requireNamespace", envir = globalenv(), inherits = FALSE))
  expect_true(requireNamespace("Seurat", quietly = TRUE))
})

# ---------------------------------------------------------------------------
# Verrou source : spatial_reference.R ne doit plus contribuer aucun C10
# ---------------------------------------------------------------------------
test_that("spatial_reference.R ne contribue aucun signalement C10", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools/check_conventions.R"), envir = e)
  before <- length(e$.REPORT$warns)
  e$check_c10_error_style(file.path(ts_project_root(), "R/spatial/spatial_reference.R"))
  w <- e$.REPORT$warns
  n <- length(w) - before
  if (n > 0L) {
    lignes <- vapply(w[(before + 1L):length(w)], function(x) x$line, integer(1))
    cat("Sites C10 restants dans spatial_reference.R :",
        paste(sort(lignes), collapse = ", "), "\n")
  }
  expect_equal(n, 0L)
})

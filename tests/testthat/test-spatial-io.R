# =============================================================================
# test-spatial-io.R — tests for R/spatial/spatial_io.R
# =============================================================================
# 18ᵉ incrément de la dette de conventions (2026-09-17).
# Créé AVANT la conversion (règle 5 : test d'abord, rouge d'abord) ⇒ ce
# fichier doit ÉCHOUER sur les assertions de classe tant que les `stop()` ne
# portent pas `class = "spatial_io_error"`.
#
# ⚠️ CONTRAIREMENT aux trois lots précédents, **aucun test hérité n'existait** :
# aucun fichier ne porte ce nom et aucun test ne `source()` ce fichier (vérifié).
#
# ⚠️ 3 sites (133, 137, 149) ne sont PAS prouvés à l'exécution — voir la
# section « Sites non joignables » en fin de fichier. Ils sont couverts par le
# VERROU SOURCE, qui les englobe tous les 11.
# =============================================================================

source_project_file("R/spatial/spatial_io.R")

.sio_err <- function(expr) {
  tryCatch({ expr; NULL },
           error = function(e) list(msg = conditionMessage(e), class = class(e)))
}
.sio_expect <- function(e, msg) {
  expect_identical(e$msg, msg)
  expect_true("spatial_io_error" %in% e$class)
}
# Garde `requireNamespace()` atteignable même si le paquet EST installé
# (§2bz.3). La chaîne de résolution passe par l'environnement d'exécution ⇒
# le mock est VU par les fermetures imbriquées aussi.
.sio_no_pkg <- function(fun) {
  e <- new.env(parent = globalenv())
  e$requireNamespace <- function(package, ...) FALSE
  f <- get(fun, envir = globalenv())
  environment(f) <- e
  f
}

# ---------------------------------------------------------------------------
# raster_to_rgba_array()
# ---------------------------------------------------------------------------
test_that("raster_to_rgba_array : format raster non reconnu (432 — MULTI-ARGUMENTS)", {
  # ⚠️ `stop("Format raster non reconnu : ", class(r)[1L])` : sans `paste0()`,
  # il ne resterait que "Format raster non reconnu : " ⇒ l'assertion porte sur
  # la concaténation, seul détecteur de troncature.
  .sio_expect(.sio_err(raster_to_rgba_array(list(a = 1))),
              "Format raster non reconnu : list")
})

# ---------------------------------------------------------------------------
# convert_to_bpcells_and_fov()
# ---------------------------------------------------------------------------
test_that("convert_to_bpcells_and_fov : BPCells absent (748 — MULTI-ARGUMENTS)", {
  .sio_expect(.sio_err(.sio_no_pkg("convert_to_bpcells_and_fov")(list(), "ds")),
              paste0("Package 'BPCells' requis pour l'import spatial (stockage sur disque). ",
                     "Installez via remotes::install_github('bnprks/BPCells/r')."))
})

test_that("convert_to_bpcells_and_fov : objet non Seurat (751)", {
  # ⚠️ BPCells doit être DISPONIBLE ici, sinon c'est 748 qui tire à la place.
  skip_if_not_installed("BPCells")
  .sio_expect(.sio_err(convert_to_bpcells_and_fov(list(a = 1), "ds")),
              "seurat_obj doit etre un objet Seurat.")
})

# ---------------------------------------------------------------------------
# compute_qc_metrics_fast()
# ---------------------------------------------------------------------------
test_that("compute_qc_metrics_fast : BPCells absent (1002)", {
  .sio_expect(.sio_err(.sio_no_pkg("compute_qc_metrics_fast")("/nope")),
              "Package 'BPCells' requis.")
})

# ---------------------------------------------------------------------------
# materialize_seurat_subset()
# ---------------------------------------------------------------------------
test_that("materialize_seurat_subset : BPCells absent (1043)", {
  .sio_expect(.sio_err(.sio_no_pkg("materialize_seurat_subset")(list(), c("a"))),
              "Package 'BPCells' requis.")
})

test_that("materialize_seurat_subset : aucun identifiant pour la ROI (1044)", {
  skip_if_not_installed("BPCells")
  .sio_expect(.sio_err(materialize_seurat_subset(list(bpcells_dir = tempdir()), character(0))),
              "Aucun identifiant fourni pour la ROI.")
})

test_that("materialize_seurat_subset : bpcells_dir introuvable (1047)", {
  skip_if_not_installed("BPCells")
  .sio_expect(.sio_err(materialize_seurat_subset(list(bpcells_dir = "/nope/nope"), c("a"))),
              "bpcells_dir introuvable sur disque pour ce jeu de donnees.")
})

# ---------------------------------------------------------------------------
# Garde-fou du mock de dépendance (§2bz.3) : il ne doit PAS fuiter
# ---------------------------------------------------------------------------
test_that("le mock de dependance ne fuit pas dans globalenv", {
  invisible(tryCatch(.sio_no_pkg("compute_qc_metrics_fast")("/nope"),
                     error = function(e) NULL))
  expect_false(exists("requireNamespace", envir = globalenv(), inherits = FALSE))
  expect_true(requireNamespace("stats", quietly = TRUE))
})

# ---------------------------------------------------------------------------
# Sites NON JOIGNABLES (133, 137, 149, 1052)
# ---------------------------------------------------------------------------
# ⚠️ Ces 4 sites ne sont PAS prouvés à l'exécution, et la raison est
# STRUCTURELLE — elle vaut d'être écrite pour qu'on ne la re-cherche pas :
#
# - 133 / 137 / 149 (`Package 'png'|'jpeg'|'magick' requis`) vivent dans la
#   fermeture `read_histology_file()`, imbriquée dans `extract_histology_image()`.
#   Or celle-ci enveloppe TOUT son corps dans un `tryCatch(..., error = ... )`
#   qui convertit l'erreur en **warning** + `NULL` (lignes 223–399) ⇒ l'erreur
#   n'atteint JAMAIS l'appelant : **la CLASSE est inobservable de l'extérieur**,
#   et même le message n'est pas récupérable de façon fiable (deux niveaux de
#   repli — Seurat natif, puis « Aucune image histologique exploitable »).
#   Un mock **sélectif** (ne couper que `png`, en laissant `jsonlite` réel, sinon
#   `json_scale_factors` reste NULL et la branche n'est pas atteinte du tout)
#   n'a pas suffi à faire remonter l'erreur non plus.
# - 1052 exige une **vraie** matrice BPCells sur disque.
#
# ⇒ Ils sont couverts par le verrou source ci-dessous, qui englobe les 11 sites.

# ---------------------------------------------------------------------------
# Verrou source : spatial_io.R ne doit plus contribuer aucun C10
# ---------------------------------------------------------------------------
test_that("spatial_io.R ne contribue aucun signalement C10", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools/check_conventions.R"), envir = e)
  before <- length(e$.REPORT$warns)
  e$check_c10_error_style(file.path(ts_project_root(), "R/spatial/spatial_io.R"))
  w <- e$.REPORT$warns
  n <- length(w) - before
  if (n > 0L) {
    lignes <- vapply(w[(before + 1L):length(w)], function(x) x$line, integer(1))
    cat("Sites C10 restants dans spatial_io.R :", paste(sort(lignes), collapse = ", "), "\n")
  }
  expect_equal(n, 0L)
})

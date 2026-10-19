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

# ---------------------------------------------------------------------------
# .spatial_swap_dir() — échange staging -> live du cache BPCells (chantier #1)
# ---------------------------------------------------------------------------
# CONSTAT (audit externe 2026-10-11 §5.2, RÉOUVERT par l'opérateur le
# 2026-10-19) : convert_to_bpcells_and_fov() faisait
# `unlink(bpcells_dir, recursive = TRUE)` AVANT d'écrire la nouvelle matrice,
# puis écrivait DANS le dossier vivant. Un réimport du même nom détruisait
# donc le cache — et les handles déjà ouverts (daemons mirai, second onglet,
# objet en RAM) — avant même de savoir si la nouvelle écriture réussirait.
#
# Le correctif déplace l'écriture dans un dossier de staging puis ÉCHANGE
# atomiquement. Ces tests épinglent les trois propriétés qui comptent :
#   (1) succès  => le live porte le nouveau contenu, aucun résidu ;
#   (2) échec   => le live est INTACT (jamais détruit « en avance ») ;
#   (3) verrou  => refus explicite plutôt que corruption silencieuse.
# MESURÉ sur cet hôte : file.rename() rend FALSE (warning, PAS une erreur)
# quand un handle ouvert vit dans le dossier ; un dossier BPCells simplement
# ouvert, lui, se renomme sans problème (BPCells relit à la demande).

.sio_mkdir_with <- function(dir, marker) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(marker, file.path(dir, "marker.txt"))
  dir
}

test_that("swap : succès — le live porte la staging, aucun résidu", {
  root <- file.path(tempdir(), paste0("sio_swap_ok_", Sys.getpid()))
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  live <- .sio_mkdir_with(file.path(root, "counts_bpcells"), "ANCIEN")
  staging <- .sio_mkdir_with(file.path(root, ".staging_x"), "NOUVEAU")

  r <- .spatial_swap_dir(staging, live)

  expect_true(isTRUE(r$ok))
  expect_identical(readLines(file.path(live, "marker.txt")), "NOUVEAU")
  expect_false(dir.exists(staging), info = "la staging a été consommée")
  expect_length(list.files(root, pattern = "^\\.staging"), 0L)
  expect_length(list.files(root, pattern = "\\.old_"), 0L)
})

test_that("swap : staging absente — le cache vivant reste INTACT", {
  # Simule un échec d'extraction/écriture AVANT l'échange : rien ne doit
  # avoir touché au cache d'origine.
  root <- file.path(tempdir(), paste0("sio_swap_missing_", Sys.getpid()))
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  live <- .sio_mkdir_with(file.path(root, "counts_bpcells"), "ANCIEN")

  r <- .spatial_swap_dir(file.path(root, ".staging_absente"), live)

  expect_false(isTRUE(r$ok))
  expect_identical(r$reason, "staging_missing")
  expect_true(dir.exists(live), info = "le cache d'origine doit survivre à un échec d'extraction")
  expect_identical(readLines(file.path(live, "marker.txt")), "ANCIEN")
})

test_that("swap : cache vivant verrouillé — refus explicite, cache INTACT", {
  root <- file.path(tempdir(), paste0("sio_swap_busy_", Sys.getpid()))
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  live <- .sio_mkdir_with(file.path(root, "counts_bpcells"), "ANCIEN")
  staging <- .sio_mkdir_with(file.path(root, ".staging_y"), "NOUVEAU")
  # Un handle ouvert dans le live reproduit le cas réel (daemon / autre onglet).
  con <- file(file.path(live, "marker.txt"), open = "r")
  on.exit(try(close(con), silent = TRUE), add = TRUE)

  r <- .spatial_swap_dir(staging, live)

  expect_false(isTRUE(r$ok))
  expect_identical(r$reason, "live_busy")
  expect_identical(readLines(file.path(live, "marker.txt")), "ANCIEN")
  expect_true(dir.exists(staging), info = "la staging est conservée pour diagnostic")
  close(con)
  # une fois le verrou levé, le même échange doit aboutir
  r2 <- .spatial_swap_dir(staging, live)
  expect_true(isTRUE(r2$ok))
  expect_identical(readLines(file.path(live, "marker.txt")), "NOUVEAU")
})

test_that("swap : pas de cache vivant — la staging devient le live", {
  root <- file.path(tempdir(), paste0("sio_swap_new_", Sys.getpid()))
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  live <- file.path(root, "counts_bpcells")
  staging <- .sio_mkdir_with(file.path(root, ".staging_z"), "PREMIER")

  r <- .spatial_swap_dir(staging, live)

  expect_true(isTRUE(r$ok))
  expect_identical(readLines(file.path(live, "marker.txt")), "PREMIER")
})

test_that("convert_to_bpcells_and_fov écrit en staging et n'unlink plus le live", {
  # VERROU SOURCE : la propriété « ne jamais détruire le cache vivant avant
  # d'avoir une staging valide » doit tenir même sur les branches que le test
  # d'exécution n'atteint pas (Seurat spatial complet requis).
  src <- paste(deparse(body(convert_to_bpcells_and_fov)), collapse = "\n")
  expect_match(src, ".spatial_swap_dir(", fixed = TRUE)
  expect_match(src, ".staging", fixed = TRUE)
  expect_false(grepl("unlink(bpcells_dir", src, fixed = TRUE),
               info = "le live ne doit plus être unlink() avant l'écriture")
})

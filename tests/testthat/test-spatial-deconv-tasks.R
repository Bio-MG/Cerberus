# =============================================================================
# test-spatial-deconv-tasks.R — tests for R/spatial/spatial_deconv_tasks.R
# =============================================================================
# 20ᵉ incrément de la dette de conventions (2026-09-18).
# Créé AVANT la conversion (règle 5 : test d'abord, rouge d'abord) ⇒ ce
# fichier doit ÉCHOUER sur les assertions de classe tant que les `stop()` ne
# portent pas `class = "spatial_deconv_tasks_error"`.
#
# 🔴 **Ce lot RÉFUTE le prédicteur de forme tel qu'il était énoncé** (§2cc.2,
# §2cd.1) : `spatial_deconv_tasks.R` a été **écarté** au 19ᵉ incrément au motif
# que c'est une longue fonction « corps de pipeline » dont les sites sont
# enfouis — or **les 5 sites sont prouvables**. Le prédicteur prédit le **coût
# de la preuve**, pas sa **possibilité** : avec le jeu d'outils (environnement
# enfant §2bz.3 + un VRAI répertoire BPCells + un objet Seurat minuscule),
# même ce fichier est prouvé **5/5**. ⇒ Il doit servir à **estimer l'effort**,
# plus jamais à **éliminer** un lot (§2ce.2).
#
# ⚠️ 3 fichiers sourcés (le corps appelle `write_mirai_log()` dès la première
# ligne) : `spatial_async.R` (le log), `spatial_deconv_prep.R`
# (`DECONV_DEFAULT_N_HVG`, `select_hvg_for_deconv`, `cap_matrix_to_hvg`) puis
# le fichier testé.
# =============================================================================

source_project_file("R/spatial/spatial_async.R")
source_project_file("R/spatial/spatial_deconv_prep.R")
source_project_file("R/spatial/spatial_deconv_tasks.R")

.dct_err <- function(expr) {
  tryCatch({ expr; NULL },
           error = function(e) list(msg = conditionMessage(e), class = class(e)))
}
.dct_expect <- function(e, msg) {
  expect_identical(e$msg, msg)
  expect_true("spatial_deconv_tasks_error" %in% e$class)
}
# 🟢 Queue de message VOLATILE (§2by.3) : le site 30 interpole
# `conditionMessage(e)` — ici celui de `gzfile()`, textuellement instable selon
# la version de R. ⇒ PRÉFIXE **+ longueur strictement supérieure** : la
# longueur détecte la troncature là où le contenu ne le peut pas. Le préfixe
# retenu contient le chemin, donc une troncature (C16) le ferait échouer aussi.
.dct_expect_prefix <- function(e, prefix) {
  expect_true(grepl(prefix, e$msg, fixed = TRUE))
  expect_true(nchar(e$msg) > nchar(prefix))
  expect_true("spatial_deconv_tasks_error" %in% e$class)
}
# Garde `requireNamespace()` atteignable même si le paquet EST installé
# (§2bz.3) — spacexr / STdeconvolve / topicmodels / slam / BPCells le sont.
.dct_no_pkg <- function(fun) {
  e <- new.env(parent = globalenv())
  e$requireNamespace <- function(package, ...) FALSE
  f <- get(fun, envir = globalenv())
  environment(f) <- e
  f
}
# 🟢 Un VRAI répertoire BPCells est nécessaire : le prologue appelle
# `BPCells::open_matrix_dir()` AVANT toute garde. Mis en cache (création ~1 s).
.dct_bpdir <- local({
  cache <- NULL
  function() {
    if (!is.null(cache) && dir.exists(cache)) return(cache)
    skip_if_not_installed("BPCells")
    skip_if_not_installed("Matrix")
    m <- Matrix::Matrix(matrix(as.integer(1:200), nrow = 10L, ncol = 20L),
                        sparse = TRUE)
    rownames(m) <- paste0("g", 1:10)
    colnames(m) <- paste0("c", 1:20)
    d <- file.path(tempdir(), "dct_bpcells")
    unlink(d, recursive = TRUE)
    suppressWarnings(BPCells::write_matrix_dir(mat = m, dir = d))
    cache <<- d
    d
  }
})
.dct_coords <- function() {
  data.frame(id = paste0("c", 1:20), x = seq_len(20) + 0.5,
             y = seq_len(20) + 0.25, stringsAsFactors = FALSE)
}
.dct_body <- function(mode, ref_path, fun) {
  fun(.dct_bpdir(), NULL, .dct_coords(), mode, ref_path, 5L, 100L, 10L,
      "lognorm", 3000L, 50L, 2000L, tempfile())
}

# ---------------------------------------------------------------------------
# .load_reference_artifact()
# ---------------------------------------------------------------------------
test_that(".load_reference_artifact : manifest illisible (30 — 4 ARGUMENTS)", {
  # ⚠️ MULTI-ARGUMENTS et le gestionnaire du tryCatch RELANCE ⇒ joignable.
  p <- file.path(tempdir(), "manifest_absent.rds")
  unlink(p)
  # `suppressWarnings()` : l'absence du fichier fait émettre à `gzfile()` un
  # avertissement ATTENDU (on provoque volontairement l'erreur) ; sans lui,
  # testthat le compte et le fichier paraît « sale » à tort.
  .dct_expect_prefix(.dct_err(suppressWarnings(.load_reference_artifact(p))),
                     paste0("Lecture du manifest de reference impossible (", p, ") : "))
})

test_that(".load_reference_artifact : BPCells absent, backend bpcells (35)", {
  mp <- file.path(tempdir(), "manifest_bpcells.rds")
  saveRDS(list(backend = "bpcells", counts_path = "/nope",
               cell_types = factor(rep("A", 20), levels = c("A", "B"))), mp)
  .dct_expect(.dct_err(.dct_no_pkg(".load_reference_artifact")(mp)),
              "Package 'BPCells' requis pour lire la reference preparee (backend bpcells).")
})

# ---------------------------------------------------------------------------
# run_spatial_deconv_body()
# ---------------------------------------------------------------------------
test_that("run_spatial_deconv_body : spacexr absent, mode rctd (91)", {
  .dct_expect(.dct_err(.dct_body("rctd", "/nope", .dct_no_pkg("run_spatial_deconv_body"))),
              "Package 'spacexr' requis (remotes::install_github('dmcable/spacexr')).")
})

test_that("run_spatial_deconv_body : reference trop petite apres filtrage (156)", {
  # 🟢 Pas de mock ici : backend non-bpcells ⇒ `readRDS()`, aucune garde de
  # dépendance sur le chemin. On annote seulement **5** des 20 cellules ⇒
  # `subset()` ne conserve que 5 colonnes ⇒ `ncol(ref_obj) < 10`.
  skip_if_not_installed("Seurat")
  suppressWarnings(suppressPackageStartupMessages(library(Seurat)))
  cnts <- Matrix::Matrix(matrix(1L, nrow = 10L, ncol = 20L), sparse = TRUE)
  rownames(cnts) <- paste0("g", 1:10); colnames(cnts) <- paste0("c", 1:20)
  cp <- file.path(tempdir(), "dct_counts.rds"); saveRDS(cnts, cp)
  mp <- file.path(tempdir(), "manifest_lt.rds")
  saveRDS(list(backend = "rds", counts_path = cp,
               cell_types = setNames(rep("A", 5), paste0("c", 1:5))), mp)
  .dct_expect(.dct_err(.dct_body("labeltransfer", mp, run_spatial_deconv_body)),
              "Reference trop petite apres filtrage des annotations manquantes (< 10 cellules annotees).")
})

test_that("run_spatial_deconv_body : STdeconvolve absent, mode stdeconvolve (221)", {
  # 🟢 Un mode qui n'est ni "rctd" ni "labeltransfer" tombe directement sur la
  # garde STdeconvolve — aucun prérequis intermédiaire.
  .dct_expect(
    .dct_err(.dct_body("stdeconvolve", "/nope", .dct_no_pkg("run_spatial_deconv_body"))),
    "Packages 'STdeconvolve', 'topicmodels' et 'slam' requis.")
})

# ---------------------------------------------------------------------------
# Garde-fou du mock de dépendance (§2bz.3) : il ne doit PAS fuiter
# ---------------------------------------------------------------------------
test_that("le mock de dependance ne fuit pas dans globalenv", {
  invisible(tryCatch(.dct_body("rctd", "/nope", .dct_no_pkg("run_spatial_deconv_body")),
                     error = function(e) NULL))
  expect_false(exists("requireNamespace", envir = globalenv(), inherits = FALSE))
  expect_true(requireNamespace("stats", quietly = TRUE))
})

# ---------------------------------------------------------------------------
# Verrou source : spatial_deconv_tasks.R ne doit plus contribuer aucun C10
# ---------------------------------------------------------------------------
test_that("spatial_deconv_tasks.R ne contribue aucun signalement C10", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools/check_conventions.R"), envir = e)
  before <- length(e$.REPORT$warns)
  e$check_c10_error_style(file.path(ts_project_root(), "R/spatial/spatial_deconv_tasks.R"))
  w <- e$.REPORT$warns
  n <- length(w) - before
  if (n > 0L) {
    lignes <- vapply(w[(before + 1L):length(w)], function(x) x$line, integer(1))
    cat("Sites C10 restants dans spatial_deconv_tasks.R :",
        paste(sort(lignes), collapse = ", "), "\n")
  }
  expect_equal(n, 0L)
})

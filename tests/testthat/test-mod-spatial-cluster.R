# =============================================================================
# test-mod-spatial-cluster.R — tests for modules/spatial/mod_spatial_cluster.R
# =============================================================================
# 25ᵉ incrément de la dette de conventions (2026-09-18, §2cj).
# Créé AVANT la conversion (règle 5 : test d'abord, rouge d'abord) ⇒ ce fichier
# doit ÉCHOUER sur les assertions de classe tant que les deux `stop()` ne
# portent pas `class = "spatial_cluster_error"`.
#
# 🟢 **PREMIER LOT DE `modules/` DONT LES SITES SONT PROUVABLES À L'EXÉCUTION.**
# ⚠️ La conclusion « les sites de `modules/` sont tous en serveurs réactifs donc
# inobservables » (§2bv.3) était **TROP LARGE**, et c'est **MESURÉ** ici : les
# deux sites vivent dans des closures **`mirai::mirai({ … })`** — du **R PUR**
# exécuté dans un démon, **pas** du code réactif. On peut donc extraire
# l'expression du corps par l'**AST** et l'`eval()` dans un **environnement
# enfant** : c'est le **VRAI code du fichier** qui s'exécute (pas une copie),
# exactement comme la technique de §2bz.3.
#
# ⚠️ **PIÈGE MESURÉ — argument MANQUANT dans l'AST.** Le corps contient
# `mat[, pass_idx, drop = FALSE]`, dont le **1ᵉʳ indice est manquant**. Un
# `for (part in as.list(x))` meurt alors sur `argument "part" is missing`
# (mesuré). Idiome correct : **itérer par INDEX** et tester
# `tryCatch(is.call(l[[i]]), error = function(e) FALSE)`.
#
# ⚠️ **C16 / §2bn — le site 291 est MULTI-ARGUMENTS** :
#     stop("Trop peu … cluster ", "(reimportez … entre-temps).")
# `errorCondition()` **n'agrège PAS** ses arguments ⇒ sans `paste0()` la seconde
# moitié du message serait **TRONQUÉE EN SILENCE**. Le test assère donc le
# message **ENTIER** : c'est CE test qui détecte l'oubli de `paste0()`.
#
# ⚠️ **Sélection des corps par CONTENU, jamais par index** : si un 3ᵉ `mirai`
# est ajouté au fichier, un index décalerait tout silencieusement.
#
# ⚠️ **`write_mirai_log` est BOUCHONNÉ** dans l'environnement enfant : il n'est
# pas le sujet du test, et le bouchon sert en plus de **TÉMOIN DE TRAVERSÉE**
# (il enregistre les messages, ce qui prouve que le corps a bien atteint
# l'étape visée avant de lever).
#
# ⚠️ **Fixture BPCells MESURÉE** : `BPCells::write_matrix_dir()` **refuse une
# matrice base** — « "m" must have class IterableMatrix, or dgCMatrix » ⇒ il
# faut passer une **`dgCMatrix`** (`Matrix::Matrix(..., sparse = TRUE)`).
# =============================================================================

source_project_file("modules/spatial/mod_spatial_cluster.R")

.MSC_FILE <- "modules/spatial/mod_spatial_cluster.R"

# --- Extraction AST, robuste aux arguments manquants (cf. en-tête) -----------
.msc_mirai_bodies <- function() {
  p <- parse(file.path(ts_project_root(), .MSC_FILE))
  collect <- function(x, out = list()) {
    if (is.call(x)) {
      is_mirai <- tryCatch(identical(x[[1]], quote(mirai::mirai)),
                           error = function(e) FALSE)
      if (isTRUE(is_mirai)) out[[length(out) + 1L]] <- x[[2]]
      l <- as.list(x)
      for (i in seq_along(l)) {
        ok <- tryCatch(is.call(l[[i]]), error = function(e) FALSE)
        if (isTRUE(ok)) out <- collect(l[[i]], out)
      }
    }
    out
  }
  out <- list()
  for (i in seq_along(p)) out <- collect(p[[i]], out)
  out
}

.msc_body_with <- function(needle) {
  bs  <- .msc_mirai_bodies()
  hit <- Filter(function(b) grepl(needle, paste(deparse(b), collapse = "\n"), fixed = TRUE), bs)
  if (length(hit) != 1L) {
    stop("attendu exactement 1 corps contenant '", needle, "', trouve ", length(hit),
         call. = FALSE)
  }
  hit[[1]]
}

# Fixture : repertoire BPCells reel (dgCMatrix exige, cf. en-tete).
.msc_bpcells_dir <- function(nrow_, ncol_, prefix) {
  dir <- tempfile(prefix)
  m <- Matrix::Matrix(
    matrix(as.numeric(seq_len(nrow_ * ncol_)), nrow = nrow_, ncol = ncol_,
           dimnames = list(paste0("g", seq_len(nrow_)), paste0("c", seq_len(ncol_)))),
    sparse = TRUE)
  BPCells::write_matrix_dir(m, dir)
  dir
}

# --- Environnement enfant : mock de `requireNamespace` + bouchon du logger ---
.msc_env <- function(rann_ok = FALSE, log = NULL, extra = list()) {
  e <- new.env(parent = globalenv())
  e$requireNamespace <- function(package, ...) {
    if (identical(package, "RANN")) rann_ok else base::requireNamespace(package, ...)
  }
  e$write_mirai_log <- function(file, message, step = NULL, total = NULL) {
    if (!is.null(log)) log(message)
    invisible(NULL)
  }
  for (nm in names(extra)) assign(nm, extra[[nm]], envir = e)
  e
}

# `suppressWarnings()` : une erreur provoquee peut s'accompagner d'un
# avertissement attendu, que testthat compterait (§2ce).
.msc_err <- function(expr, envir) {
  tryCatch({ suppressWarnings(eval(expr, envir = envir)); NULL },
           error = function(e) list(msg = conditionMessage(e), class = class(e)))
}

.MSC_RANN_MSG <- "Package 'RANN' requis (install.packages('RANN'))."
.MSC_COMMON_MSG <- paste0(
  "Trop peu d'elements communs entre la matrice QC-filtree et les labels de cluster ",
  "(reimportez ou relancez le clustering si les seuils QC ont change entre-temps).")

# ---------------------------------------------------------------------------
# Site 135 — garde de dependance RANN, 1ʳᵉ instruction du corps du démon
# ---------------------------------------------------------------------------
test_that("mod_spatial_cluster : RANN absent ⇒ classe (site 135)", {
  b <- .msc_body_with("'RANN'")
  res <- .msc_err(b, .msc_env(rann_ok = FALSE))
  expect_false(is.null(res))                                   # erreur levee…
  expect_true("spatial_cluster_error" %in% res$class)          # …avec NOTRE classe
  # Message a UN SEUL argument ⇒ aucun `paste0()` requis ; il est entierement
  # statique ⇒ on assere le message ENTIER, jamais un prefixe (§2bx.3).
  expect_identical(res$msg, .MSC_RANN_MSG)
})

# ---------------------------------------------------------------------------
# Contrôle de BORNE (site 135) — la garde est CONDITIONNELLE
# ---------------------------------------------------------------------------
test_that("la garde RANN est conditionnelle : RANN present ⇒ pas notre classe", {
  skip_if_not_installed("RANN")
  expect_true(requireNamespace("RANN", quietly = TRUE))         # precondition
  b <- .msc_body_with("'RANN'")
  # On franchit la garde ; l'echec qui suit vient de `BPCells::open_matrix_dir()`
  # sur un repertoire inexistant ⇒ on n'asserte QUE l'absence de notre classe.
  e <- .msc_env(rann_ok = TRUE,
                extra = list(log_file = tempfile(fileext = ".log"),
                             bpcells_dir = tempfile("absent_"),
                             pass_idx = NULL))
  res <- .msc_err(b, e)
  expect_false(is.null(res))                                    # une erreur est levee…
  expect_false("spatial_cluster_error" %in% res$class)          # …mais PAS la garde
})

# ---------------------------------------------------------------------------
# Site 291 — garde de seuil, message MULTI-ARGUMENTS (piege C16 / §2bn)
# ---------------------------------------------------------------------------
test_that("mod_spatial_cluster : trop peu d'ids communs ⇒ message ENTIER (site 291)", {
  skip_if_not_installed("BPCells")
  b <- .msc_body_with("Trop peu d'elements")

  dir <- .msc_bpcells_dir(20, 5, "msc_mat_")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  log <- character(0)
  e <- .msc_env(log = function(msg) log <<- c(log, msg),
                extra = list(bpcells_dir = dir,
                             pass_idx = NULL,
                             cluster_labels = stats::setNames(paste0("k", 1:5),
                                                              paste0("c", 1:5))))
  res <- .msc_err(b, e)

  expect_false(is.null(res))
  expect_true("spatial_cluster_error" %in% res$class)
  # ⚠️ Assertion sur le message ENTIER : sans `paste0()`, `errorCondition()` ne
  # garderait que la 1ʳᵉ moitié (troncature SILENCIEUSE, §2bn).
  expect_identical(res$msg, .MSC_COMMON_MSG)
  # Temoin de traversee : le corps a bien atteint l'etape 2 avant de lever.
  expect_true(any(grepl("Alignement des labels", log, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# Contrôle de BORNE (site 291) — au seuil EXACT (10 ids communs) la garde passe
# ---------------------------------------------------------------------------
test_that("la garde de seuil est conditionnelle : 10 ids communs ⇒ pas notre classe", {
  skip_if_not_installed("BPCells")
  b <- .msc_body_with("Trop peu d'elements")

  dir <- .msc_bpcells_dir(10, 10, "msc_mat_ok_")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  log <- character(0)
  e <- .msc_env(log = function(msg) log <<- c(log, msg),
                extra = list(bpcells_dir = dir,
                             pass_idx = NULL,
                             logfc_threshold = 0.25,
                             min_pct = 0.1,
                             cluster_labels = stats::setNames(rep(c("k1", "k2"), 5),
                                                              paste0("c", 1:10))))
  # 10 ids communs ⇒ `length(common_ids) < 10` est FAUX ⇒ la garde ne tire pas.
  res <- .msc_err(b, e)

  # ⚠️ Contrôle de BORNE NON VACUANT (§2cj.3) : sans témoin, un échec survenu
  # AVANT la garde (p. ex. `open_matrix_dir`) ferait passer le test à vide —
  # l'absence de notre classe serait alors vraie pour la mauvaise raison.
  # On exige donc la PREUVE que le corps a franchi la garde : l'étape 3 est
  # journalisée APRÈS elle (ligne 300 du fichier).
  expect_true(any(grepl("FindAllMarkers", log, fixed = TRUE)))
  if (!is.null(res)) expect_false("spatial_cluster_error" %in% res$class)
})

# ---------------------------------------------------------------------------
# Verrou source : mod_spatial_cluster.R ne doit plus contribuer aucun C10
# ---------------------------------------------------------------------------
test_that("mod_spatial_cluster.R ne contribue aucun signalement C10", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools/check_conventions.R"), envir = e)
  before <- length(e$.REPORT$warns)
  e$check_c10_error_style(file.path(ts_project_root(), .MSC_FILE))
  w <- e$.REPORT$warns
  n <- length(w) - before
  if (n > 0L) {
    lignes <- vapply(w[(before + 1L):length(w)], function(x) x$line, integer(1))
    cat("Sites C10 restants dans mod_spatial_cluster.R :",
        paste(sort(lignes), collapse = ", "), "\n")
  }
  expect_equal(n, 0L)
})

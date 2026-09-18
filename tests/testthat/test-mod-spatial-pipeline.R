# =============================================================================
# test-mod-spatial-pipeline.R — tests for modules/spatial/mod_spatial_pipeline.R
# =============================================================================
# 26ᵉ incrément de la dette de conventions (2026-09-18, §2cl).
# Créé AVANT la conversion (règle 5 : test d'abord, rouge d'abord) ⇒ ce fichier
# doit ÉCHOUER sur l'assertion de classe tant que le `stop()` ne porte pas
# `class = "spatial_pipeline_error"`.
#
# 🟢 **Lot choisi par le critère §2cj.1, MESURÉ sur les 12 sites de `modules/`** :
# le site est dans un corps **`mirai::mirai({ … })`** = du **R PUR** exécuté dans
# un démon, donc extractible par l'**AST** et `eval()`-able dans un
# **environnement enfant**. Le site est la **PREMIÈRE instruction** du corps ⇒
# il tire **immédiatement** quand `requireNamespace()` est mocké, sans aucune
# fixture (ni objet Seurat, ni fichier, ni répertoire BPCells).
#
# ⚠️ Le fichier contient **7** corps `mirai` (lignes 286, 338, 358, 372, 399,
# 437, 452) et **UN SEUL** `stop()` — celui du premier corps. On sélectionne donc
# le corps **PAR CONTENU**, jamais par index : un 8ᵉ corps ajouté décalerait tout
# silencieusement.
#
# ⚠️ **PIÈGE AST** (§2cj.2) : un parcours qui fait `for (part in as.list(x))`
# meurt sur `argument "part" is missing` dès qu'un appel porte un argument
# manquant (`mat[, pass_idx, drop = FALSE]`, présent dans ce corps). ⇒ itérer
# par **INDEX**, et tester la sous-expression sous `tryCatch`.
#
# ⚠️ **C16 / §2bn** : le message de ce site est à **UN SEUL** argument
# (« Package 'RANN' requis. ») ⇒ **aucun `paste0()`** requis. Il est
# entièrement statique ⇒ on assère le message **ENTIER** (§2bx.3).
#
# ⚠️ **Le message DIFFÈRE de celui de `mod_spatial_cluster.R`** — qui dit
# « Package 'RANN' requis (install.packages('RANN')). » : deux fichiers, deux
# messages. C'est pourquoi le test assère la chaîne **exacte** de CE fichier,
# jamais un préfixe commun.
#
# ⚠️ **`write_mirai_log` est BOUCHONNÉ** : il n'est pas le sujet du test et sert
# de **TÉMOIN DE TRAVERSÉE** (§2cj.3) — sans lui, le contrôle de borne serait
# **vacuant** (« notre classe est absente » est vrai aussi si le code échoue
# AVANT la garde).
# =============================================================================

source_project_file("modules/spatial/mod_spatial_pipeline.R")

.MSP_FILE <- "modules/spatial/mod_spatial_pipeline.R"

# --- Extraction AST, robuste aux arguments manquants (cf. en-tête) -----------
.msp_mirai_bodies <- function() {
  p <- parse(file.path(ts_project_root(), .MSP_FILE))
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

.msp_body_with <- function(needle) {
  bs  <- .msp_mirai_bodies()
  hit <- Filter(function(b) grepl(needle, paste(deparse(b), collapse = "\n"), fixed = TRUE), bs)
  if (length(hit) != 1L) {
    stop("attendu exactement 1 corps contenant '", needle, "', trouve ", length(hit),
         call. = FALSE)
  }
  hit[[1]]
}

# --- Environnement enfant : mock de `requireNamespace` + bouchon du logger ---
.msp_env <- function(rann_ok = FALSE, log = NULL, extra = list()) {
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
.msp_err <- function(expr, envir) {
  tryCatch({ suppressWarnings(eval(expr, envir = envir)); NULL },
           error = function(e) list(msg = conditionMessage(e), class = class(e)))
}

.MSP_RANN_MSG <- "Package 'RANN' requis."

# ---------------------------------------------------------------------------
# Site 288 — garde de dependance RANN, 1ʳᵉ instruction du corps du démon
# ---------------------------------------------------------------------------
test_that("mod_spatial_pipeline : RANN absent ⇒ classe (site 288)", {
  b <- .msp_body_with("'RANN'")
  res <- .msp_err(b, .msp_env(rann_ok = FALSE))
  expect_false(is.null(res))                                   # erreur levee…
  expect_true("spatial_pipeline_error" %in% res$class)         # …avec NOTRE classe
  # Message a UN SEUL argument ⇒ aucun `paste0()` requis, et il est entierement
  # statique ⇒ on assere le message ENTIER, jamais un prefixe (§2bx.3).
  expect_identical(res$msg, .MSP_RANN_MSG)
})

# ---------------------------------------------------------------------------
# Contrôle de BORNE (site 288) — la garde est CONDITIONNELLE
# ---------------------------------------------------------------------------
test_that("la garde RANN est conditionnelle : RANN present ⇒ pas notre classe", {
  skip_if_not_installed("RANN")
  expect_true(requireNamespace("RANN", quietly = TRUE))         # precondition
  b <- .msp_body_with("'RANN'")

  log <- character(0)
  # On franchit la garde ; l'echec qui suit vient de `BPCells::open_matrix_dir()`
  # sur un repertoire inexistant ⇒ on n'asserte QUE l'absence de notre classe.
  e <- .msp_env(rann_ok = TRUE,
                log = function(msg) log <<- c(log, msg),
                extra = list(log_file = tempfile(fileext = ".log"),
                             bpcells_dir = tempfile("absent_"),
                             pass_idx = NULL))
  res <- .msp_err(b, e)

  # ⚠️ Contrôle de BORNE NON VACUANT (§2cj.3) : sans temoin, un echec survenu
  # AVANT la garde ferait passer le test a vide. L'etape journalisee juste APRES
  # la garde prouve que le corps l'a bien franchie.
  expect_true(any(grepl("[2/9] Ouverture BPCells", log, fixed = TRUE)))
  if (!is.null(res)) expect_false("spatial_pipeline_error" %in% res$class)
})

# ---------------------------------------------------------------------------
# Verrou source : mod_spatial_pipeline.R ne doit plus contribuer aucun C10
# ---------------------------------------------------------------------------
test_that("mod_spatial_pipeline.R ne contribue aucun signalement C10", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools/check_conventions.R"), envir = e)
  before <- length(e$.REPORT$warns)
  e$check_c10_error_style(file.path(ts_project_root(), .MSP_FILE))
  w <- e$.REPORT$warns
  n <- length(w) - before
  if (n > 0L) {
    lignes <- vapply(w[(before + 1L):length(w)], function(x) x$line, integer(1))
    cat("Sites C10 restants dans mod_spatial_pipeline.R :",
        paste(sort(lignes), collapse = ", "), "\n")
  }
  expect_equal(n, 0L)
})

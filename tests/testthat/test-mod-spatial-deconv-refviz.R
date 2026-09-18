# =============================================================================
# test-mod-spatial-deconv-refviz.R
#   tests for modules/spatial/deconv/mod_spatial_deconv_refviz.R
# =============================================================================
# 27ᵉ incrément de la dette de conventions (2026-09-18, §2cm).
# Créé AVANT la conversion (règle 5 : test d'abord, rouge d'abord) ⇒ ce fichier
# doit ÉCHOUER sur l'assertion de classe tant que le `stop()` du site 35 ne
# porte pas `class = "spatial_deconv_refviz_error"`.
#
# 🟢 **C'est LE DERNIER SITE PROUVABLE de `modules/`.** La re-qualification
# mesurée des 12 sites restants (§2cl.5) donnait **2 prouvables / 10 réactifs** ;
# `mod_spatial_pipeline.R` a été converti au 26ᵉ incrément, celui-ci est le
# second et le dernier. Les **10** autres vivent dans des serveurs réactifs
# (`observeEvent` / `renderPlot`) ⇒ **inobservables** sans `testServer()`, qui
# ne fonctionne pas ici (§2bv.3 : éprouvé sur un module trivial).
#
# ⚠️ **POURQUOI CE LOT EST PLUS CHER QUE §2cl** : le site n'est PAS la première
# instruction du corps du démon. La garde est nichée dans la branche
# `if (identical(mode, "local"))` et ne s'évalue qu'APRÈS un `readRDS()` d'un
# objet portant `@meta.data` ⇒ **il faut une vraie fixture Seurat** (le lot
# précédent n'en avait aucune). Coût mesuré : `Seurat::CreateSeuratObject()`
# sur une `dgCMatrix` 4×2 ≈ instantané.
#
# ⚠️ **Sélection du corps par CONTENU, jamais par index** : si un 2ᵉ `mirai` est
# ajouté au fichier, un index décalerait tout silencieusement. Le fichier n'a
# qu'UN corps aujourd'hui, mais la règle reste (§2cj.2).
#
# ⚠️ **PIÈGE AST** (§2cj.2) : un parcours qui fait `for (part in as.list(x))`
# meurt sur `argument "part" is missing` dès qu'un appel porte un argument
# manquant. ⇒ itérer par **INDEX**, et tester la sous-expression sous
# `tryCatch`. Le corps contient `obj[, keep]` (2ᵉ argument présent, mais
# `as.list()` reste le bon idiome pour tout le fichier).
#
# ⚠️ **C16 / §2bn — SANS OBJET ICI** : le message du site 35 est à **UN SEUL**
# argument (« Colonne 'type cellulaire' introuvable. ») et entièrement statique
# ⇒ **aucun `paste0()`** requis. On assère néanmoins le message **ENTIER**,
# jamais un préfixe (§2bx.3 : une assertion sur un préfixe ne voit pas la
# troncature).
#
# ⚠️ **`write_mirai_log` est BOUCHONNÉ** : il n'est pas le sujet du test et sert
# de **TÉMOIN DE TRAVERSÉE** (§2cj.3) — sans lui, le contrôle de borne serait
# **vacuant** (« notre classe est absente » est vrai aussi si le code échoue
# AVANT la garde).
# =============================================================================

source_project_file("modules/spatial/deconv/mod_spatial_deconv_refviz.R")

.MSR_FILE <- "modules/spatial/deconv/mod_spatial_deconv_refviz.R"

# --- Extraction AST, robuste aux arguments manquants (cf. en-tête) -----------
.msr_mirai_bodies <- function() {
  p <- parse(file.path(ts_project_root(), .MSR_FILE))
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

.msr_body_with <- function(needle) {
  bs  <- .msr_mirai_bodies()
  hit <- Filter(function(b) grepl(needle, paste(deparse(b), collapse = "\n"), fixed = TRUE), bs)
  if (length(hit) != 1L) {
    stop("attendu exactement 1 corps contenant '", needle, "', trouve ", length(hit),
         call. = FALSE)
  }
  hit[[1]]
}

# --- Fixture : un VRAI objet Seurat minimal, sérialisé en RDS ----------------
# `Seurat::CreateSeuratObject()` pose par défaut `orig.ident`, `nCount_RNA`,
# `nFeature_RNA` ; on AJOUTE une colonne explicite pour que le contrôle de borne
# prouve que la garde interroge bien `colnames(obj@meta.data)`.
.msr_ref_obj <- function(with_col = "celltype", n_cells = 2L) {
  m <- Matrix::Matrix(
    matrix(as.numeric(seq_len(4L * n_cells)), nrow = 4L, ncol = n_cells,
           dimnames = list(paste0("g", seq_len(4L)), paste0("c", seq_len(n_cells)))),
    sparse = TRUE)
  obj <- Seurat::CreateSeuratObject(counts = m)
  if (!is.null(with_col)) obj@meta.data[[with_col]] <- rep("t1", n_cells)
  obj
}

.msr_rds <- function(with_col = "celltype", n_cells = 2L) {
  f <- tempfile(fileext = ".rds")
  saveRDS(.msr_ref_obj(with_col = with_col, n_cells = n_cells), f)
  f
}

# --- Environnement enfant : bouchon du logger + variables du corps -----------
# Aucun `requireNamespace()` à mocker ici : ce site n'est PAS une garde de
# dépendance (contrairement aux sites de §2cj / §2cl).
.msr_env <- function(ref_obj_path, celltype_col, max_cells,
                     log = NULL, reduction = "umap") {
  e <- new.env(parent = globalenv())
  e$write_mirai_log <- function(file, message, step = NULL, total = NULL) {
    if (!is.null(log)) log(message)
    invisible(NULL)
  }
  e$mode             <- "local"
  e$ref_obj_path     <- ref_obj_path
  e$ref_manifest_path <- NA_character_
  e$celltype_col     <- celltype_col
  e$reduction        <- reduction
  e$max_cells        <- max_cells
  e$log_file         <- tempfile(fileext = ".log")
  e
}

# `suppressWarnings()` : une erreur provoquée peut s'accompagner d'un
# avertissement attendu, que testthat compterait (§2ce).
.msr_err <- function(expr, envir) {
  tryCatch({ suppressWarnings(eval(expr, envir = envir)); NULL },
           error = function(e) list(msg = conditionMessage(e), class = class(e)))
}

.MSR_MSG <- "Colonne 'type cellulaire' introuvable."

# ---------------------------------------------------------------------------
# Site 35 — colonne 'type cellulaire' absente de meta.data
# ---------------------------------------------------------------------------
test_that("mod_spatial_deconv_refviz : colonne absente ⇒ classe (site 35)", {
  skip_if_not_installed("Seurat")
  b <- .msr_body_with("Colonne 'type cellulaire' introuvable")

  rds <- .msr_rds(with_col = "celltype")
  on.exit(unlink(rds), add = TRUE)

  # On DEMANDE une colonne qui n'existe pas ⇒ `!celltype_col %in% colnames(...)`
  # est VRAI ⇒ la garde doit tirer.
  res <- .msr_err(b, .msr_env(ref_obj_path = rds, celltype_col = "colonne_absente",
                              max_cells = 20000L))

  expect_false(is.null(res))                                        # erreur levée…
  expect_true("spatial_deconv_refviz_error" %in% res$class)         # …avec NOTRE classe
  # Message à UN SEUL argument et entièrement statique ⇒ on assère le message
  # ENTIER, jamais un préfixe (§2bx.3).
  expect_identical(res$msg, .MSR_MSG)
})

# ---------------------------------------------------------------------------
# Contrôle de BORNE (site 35) — la garde est CONDITIONNELLE
# ---------------------------------------------------------------------------
test_that("la garde est conditionnelle : colonne présente ⇒ pas notre classe", {
  skip_if_not_installed("Seurat")
  b <- .msr_body_with("Colonne 'type cellulaire' introuvable")

  rds <- .msr_rds(with_col = "celltype")
  on.exit(unlink(rds), add = TRUE)

  log <- character(0)
  # `max_cells = 1` avec 2 cellules ⇒ on franchit la garde PUIS on déclenche le
  # sous-échantillonnage, dont le message est journalisé APRÈS elle.
  res <- .msr_err(b, .msr_env(ref_obj_path = rds, celltype_col = "celltype",
                              max_cells = 1L,
                              log = function(m) log <<- c(log, m)))

  # ⚠️ Contrôle de BORNE NON VACUANT (§2cj.3) : sans témoin, un échec survenu
  # AVANT la garde (p. ex. le `readRDS()`) ferait passer le test à vide —
  # l'absence de notre classe serait alors vraie pour la mauvaise raison.
  # L'étape « Sous-echantillonnage » est journalisée APRÈS la garde (ligne 49 du
  # fichier) ⇒ elle prouve la traversée.
  expect_true(any(grepl("Sous-echantillonnage", log, fixed = TRUE)))
  if (!is.null(res)) expect_false("spatial_deconv_refviz_error" %in% res$class)
})

# ---------------------------------------------------------------------------
# Verrou source : mod_spatial_deconv_refviz.R ne doit plus contribuer aucun C10
# ---------------------------------------------------------------------------
test_that("mod_spatial_deconv_refviz.R ne contribue aucun signalement C10", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools/check_conventions.R"), envir = e)
  before <- length(e$.REPORT$warns)
  e$check_c10_error_style(file.path(ts_project_root(), .MSR_FILE))
  w <- e$.REPORT$warns
  n <- length(w) - before
  if (n > 0L) {
    lignes <- vapply(w[(before + 1L):length(w)], function(x) x$line, integer(1))
    cat("Sites C10 restants dans mod_spatial_deconv_refviz.R :",
        paste(sort(lignes), collapse = ", "), "\n")
  }
  expect_equal(n, 0L)
})

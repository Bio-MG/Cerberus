# =============================================================================
# test-spatial-report.R — tests for R/spatial/spatial_report.R
# =============================================================================
# 45ᵉ incrément de la dette de conventions (2026-09-20, §2df).
# Créé AVANT le correctif (règle 5 : test d'abord, rouge d'abord).
#
# ⚠️ 3ᵉ lot d'affilée **sans aucun test hérité** (§2cc.1, §2cd.1) : aucun
# fichier de test ne porte ce nom ; les **2** qui mentionnent ce fichier
# (`test-conventions-c9b-owned.R` l'utilise comme TÉMOIN C9 ; 
# `test-report-contract-freeze.R` vérifie seulement son `source()` dans
# `app.R`) ne le couvrent **pas exclusivement** ⇒ test écrit de zéro.
#
# 🔴 **Ce lot paie C9 SEUL** (`20 → 19`) — ⚠️ **pas C10** : le fichier porte
# **1** seul `stop()` (ligne 103) qui porte déjà `call. = FALSE`, forme que
# C10 **exempte**. Mesuré : **C10 = 0 site** dans TOUT le dépôt (§2cx, §2dd).
#
# 🟢 **POURQUOI CE FICHIER, ET PAS SEULEMENT POUR LA RÈGLE C9** :
# `build_spatial_report_dataset()` est **CASSÉE POUR LE CAS MAJORITAIRE**.
# Trois `return(NULL)` (lignes 58, 62, 64) sont écrits **à l'intérieur** de
# l'argument `histology_overlay = tryCatch({ ... })` d'un appel `list(...)`.
# Or en R **`return()` remonte au cadre de la FONCTION, pas au `tryCatch`** :
# il sort donc de `build_spatial_report_dataset()` ENTIÈREMENT, avant que
# `list(...)` ne soit construit. Conséquence : la fonction rend **`NULL`** au
# lieu du snapshot documenté, dès que l'objet n'a **pas** d'histologie — le
# cas ORDINAIRE (`extract_histology_image()` rend `NULL` sans image, et
# `spatial_obj$histology` vaut alors `NULL`, cf. `R/spatial/spatial_io.R:840`).
# ⇒ Le rapport Spatial perd **silencieusement** le snapshot du dataset
# (`mod_spatial_report.R:149/157` mettent ce `NULL` dans la liste passée au
# Rmd). C'est la famille §2dd : un défaut trouvé **par l'écriture du test**.
#
# ⚠️ **PREUVE** : le mécanisme est reproduit à part dans la section 0, avec un
# témoin MINIMAL (`return()` dans un `tryCatch` rend `NULL` pour la fonction
# entière) — le test ne dépend donc pas de la lecture de `spatial_report.R`.
# =============================================================================

source_project_file("R/spatial/spatial_report.R")

# ---------------------------------------------------------------------------
# 0. TÉMOIN DU MÉCANISME R — `return()` dans un `tryCatch` sort de la FONCTION
# ---------------------------------------------------------------------------
# Si cette section cassait un jour (non-régression du langage ?), tout le
# raisonnement de classe tombait : on la vérifie donc explicitement.
test_that("TEMOIN : `return()` dans un tryCatch sort de la fonction ENTIERE", {
  g <- function(flag) {
    list(
      a = "A",
      b = tryCatch({
        if (flag) return(NULL)   # sort de g(), pas du tryCatch
        "B"
      }, error = function(e) NULL),
      c = "C"
    )
  }
  # Sans le drapeau : la liste est construite, avec ses 3 champs.
  expect_identical(names(g(FALSE)), c("a", "b", "c"))
  expect_identical(g(FALSE)$b, "B")
  # Avec le drapeau : la fonction rend NULL — les champs a/c sont PERDUS.
  expect_null(g(TRUE))
})

# ---------------------------------------------------------------------------
# 1. `build_spatial_report_dataset()` — CONTRAT : rendre TOUJOURS la liste
# ---------------------------------------------------------------------------
test_that("build_spatial_report_dataset : rend une LISTE pour un objet vide", {
  # 🔴 C'est CETTE assertion qui échoue avant le correctif : la fonction rend
  # NULL au lieu d'une liste.
  r <- build_spatial_report_dataset(list())
  expect_true(is.list(r))
  expect_identical(typeof(r), "list")
})

test_that("build_spatial_report_dataset : les 8 champs du contrat sont presents", {
  r <- build_spatial_report_dataset(list())
  expect_identical(
    names(r),
    c("project", "technology", "n_total", "coords", "sketch_ids",
      "sketch", "histology_overlay", "results")
  )
})

test_that("build_spatial_report_dataset : DEFAUTS pour un objet vide (?, NA, vide)", {
  r <- build_spatial_report_dataset(list())
  expect_identical(r$project, "?")
  expect_identical(r$technology, "?")
  expect_identical(r$n_total, NA_integer_)
  expect_null(r$coords)
  expect_identical(r$sketch_ids, character(0))  # pas NULL : le Rmd itere dessus
  expect_null(r$sketch)
  expect_null(r$histology_overlay)
  expect_identical(r$results, list())
})

test_that("build_spatial_report_dataset : SANS histologie — cas ORDINAIRE", {
  # C'est LE cas de production : extract_histology_image() rend NULL quand le
  # dataset n'a pas d'image (R/spatial/spatial_io.R:840 => histology = NULL).
  obj <- list(project = "P1", technology = "Visium", n_total = 1234L)
  r <- build_spatial_report_dataset(obj)
  expect_true(is.list(r))                     # 🔴 ROUGE avant correctif
  expect_identical(r$project, "P1")
  expect_identical(r$technology, "Visium")
  expect_identical(r$n_total, 1234L)
  expect_null(r$histology_overlay)            # NULL est la bonne valeur
})

test_that("build_spatial_report_dataset : histology NULL explicite (pas absent)", {
  # `histology = NULL` DOIT se comporter comme une cle absente : le premier
  # `return(NULL)` du fichier teste exactement `is.null(spatial_obj$histology)`.
  r <- build_spatial_report_dataset(list(project = "P2", histology = NULL))
  expect_true(is.list(r))                     # 🔴 ROUGE avant correctif
  expect_identical(r$project, "P2")
  expect_null(r$histology_overlay)
})

test_that("build_spatial_report_dataset : histology NON NULL mais inexploitable", {
  # La 2ᵉ et la 3ᵉ garde `return(NULL)` du fichier couvrent ce cas : la
  # structure ne permet pas d'extraire un raster. L'objet reste un DATASET
  # valide ⇒ la fonction doit rendre la liste, avec overlay NULL.
  r <- build_spatial_report_dataset(list(project = "P3", histology = list(bidon = 1)))
  expect_true(is.list(r))                     # 🔴 ROUGE avant correctif
  expect_identical(r$project, "P3")
  expect_null(r$histology_overlay)
})

test_that("build_spatial_report_dataset : coords transitent TELLES QUELLES", {
  # Le contrat dit « only small, already-computed pieces » : aucune copie,
  # aucune transformation. On compare par identical() sur les DEUX objets.
  df <- data.frame(x = c(1.5, 2.5), y = c(3.5, 4.5), row.names = c("b1", "b2"))
  r <- build_spatial_report_dataset(list(coords = df))
  expect_identical(r$coords, df)
  expect_identical(dim(r$coords), dim(df))
  expect_identical(rownames(r$coords), rownames(df))
})

test_that("build_spatial_report_dataset : sketch_ids suit le sketch (colonnes)", {
  # `sketch_ids` = colnames(sketch) quand il y a un sketch, character(0) sinon.
  sk <- matrix(1:6, nrow = 3L, dimnames = list(NULL, c("c1", "c2")))
  r <- build_spatial_report_dataset(list(sketch = sk))
  expect_identical(r$sketch_ids, c("c1", "c2"))
  expect_identical(r$sketch, sk)
})

test_that("build_spatial_report_dataset : `results` est transmis inchange", {
  res <- list(qc_metrics = data.frame(a = 1), cluster_labels = c("A", "B"))
  r <- build_spatial_report_dataset(list(project = "P4"), results = res)
  expect_identical(r$results, res)
  # `results = NULL` (explicite) : le contrat dit `results = list()`.
  r2 <- build_spatial_report_dataset(list(project = "P5"), results = NULL)
  expect_identical(r2$results, list())
})

test_that("build_spatial_report_dataset : la valeur par defaut de `results` est list()", {
  # ⚠️ `formals()$results` est un OBJET LANGAGE (l'expression `list()`), pas la
  # valeur `list()`. On le compare donc par deparse(), et on evalue pour
  # prouver que l'evaluation rend bien une liste vide.
  fml <- formals(build_spatial_report_dataset)
  expect_identical(deparse(fml$results), "list()")
  expect_identical(eval(fml$results), list())
  # `spatial_obj` n'a PAS de defaut (argument requis).
  expect_true(is.symbol(fml$spatial_obj))
})

# ---------------------------------------------------------------------------
# 2. `find_spatial_report_template()` — chemin, et GARDE atteignable
# ---------------------------------------------------------------------------
test_that("find_spatial_report_template : trouve le template depuis la racine", {
  old <- setwd(ts_project_root())
  on.exit(setwd(old), add = TRUE)
  p <- find_spatial_report_template()
  expect_type(p, "character")
  expect_length(p, 1L)
  expect_true(file.exists(p))
  expect_match(basename(p), "^spatial_report_template\\.Rmd$")
})

test_that("find_spatial_report_template : `stop()` JOIGNABLE hors projet", {
  # 🟢 Joignable SANS mock : depuis un repertoire temporaire VIDE, aucune des
  # 4 candidates n'existe ⇒ la garde tire. (Mesure : c'est bien le cas.)
  td <- tempfile("spat_rep_"); dir.create(td)
  old <- setwd(td)
  on.exit({ setwd(old); unlink(td, recursive = TRUE) }, add = TRUE)

  e <- tryCatch({ find_spatial_report_template(); NULL },
                error = function(e) list(msg = conditionMessage(e), class = class(e)))
  expect_false(is.null(e))
  expect_true("simpleError" %in% e$class)
  # Le message NOMME le fichier cherche (sinon l'utilisateur ne sait pas quoi
  # reparer) et LISTE les chemins essayes (4 candidats).
  expect_true(grepl("spatial_report_template.Rmd", e$msg, fixed = TRUE))
  expect_true(grepl("Chemins essayes", e$msg, fixed = TRUE))
})

test_that("find_spatial_report_template : le message liste les 4 CANDIDATES", {
  td <- tempfile("spat_rep_"); dir.create(td)
  old <- setwd(td)
  on.exit({ setwd(old); unlink(td, recursive = TRUE) }, add = TRUE)
  e <- tryCatch(find_spatial_report_template(), error = function(e) conditionMessage(e))
  # 4 candidats = 4 lignes « - … » dans le message.
  n_li <- length(grep("^  - ", strsplit(e, "\n")[[1]]))
  expect_identical(n_li, 4L)
})

test_that("find_spatial_report_template : la 4e candidate est getwd()/modules/spatial", {
  # Verrou de SOURCE sur l'ORDRE des candidates (le 1er trouvé gagne) : la
  # version relative doit passer AVANT la version getwd() du meme chemin,
  # sinon le chemin rendu devient absolu et le message d'erreur change de forme.
  src <- readLines(file.path(ts_project_root(), "R", "spatial", "spatial_report.R"))
  body_txt <- paste(src, collapse = "\n")
  i_rel <- regexpr('file.path("reports", "spatial_report_template.Rmd")', body_txt, fixed = TRUE)
  i_abs <- regexpr('file.path(getwd(), "reports", "spatial_report_template.Rmd")', body_txt, fixed = TRUE)
  expect_true(i_rel > 0L && i_abs > 0L)
  expect_true(i_rel < i_abs)   # la relative d'abord
  # `unique()` est indispensable : sans lui, la liste pourrait contenir 2 fois
  # le meme chemin quand getwd() est la racine.
  expect_true(grepl("unique(", body_txt, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# 3. NON-RÉGRESSION DE FORME — les `return()` NE DOIVENT PAS revenir
# ---------------------------------------------------------------------------
test_that("spatial_report.R : aucun `return(NULL)` dans la construction du list()", {
  # Verrou de SOURCE rendant le P0 FALSIFIABLE : le correctif doit retirer les
  # `return()` fautifs. On compte les `return(` DANS la fonction du builder.
  # ⚠️ On RETIRE les commentaires AVANT de compter : le correctif EXPLIQUE le
  # piège dans un commentaire qui contient litteralement « return(NULL) » et
  # « return() » — compter les lignes brutes ferait echouer le verrou sur sa
  # propre documentation (piege mesure en ecrivant ce fichier).
  src <- readLines(file.path(ts_project_root(), "R", "spatial", "spatial_report.R"))
  code <- sub("#.*$", "", src)          # commentaires retires
  start <- grep("^build_spatial_report_dataset <- function", src)
  expect_length(start, 1L)
  # fin = 1re ligne qui ferme la fonction (accolade en colonne 1)
  close <- which(seq_along(code) > start & grepl("^\\}", code))[1]
  body <- code[start:close]
  n_ret <- sum(lengths(regmatches(body, gregexpr("\\breturn\\(", body))))
  expect_identical(n_ret, 0L)
})

test_that("spatial_report.R : le tryCatch d'histologie ne CONTIENT plus de return", {
  # Assertion plus fine : à l'interieur du bloc histology_overlay, aucun
  # `return(` ne doit subsister (c'est precisement le defaut).
  src <- readLines(file.path(ts_project_root(), "R", "spatial", "spatial_report.R"))
  txt <- paste(sub("#.*$", "", src), collapse = "\n")   # commentaires retires
  m <- regexpr("histology_overlay = tryCatch\\(", txt)
  expect_true(m > 0L)
  # La 2e fermeture `}, error = function(e) NULL),` termine le bloc.
  tail_txt <- substr(txt, m, nchar(txt))
  stop_at <- regexpr("}, error = function\\(e\\) NULL\\)", tail_txt)
  expect_true(stop_at > 0L)
  bloc <- substr(tail_txt, 1L, stop_at)
  expect_false(grepl("\\breturn\\(", bloc))
})

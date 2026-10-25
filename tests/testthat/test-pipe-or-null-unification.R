# =============================================================================
# test-pipe-or-null-unification.R — garde du jalon M3 (D2) : %||% UNIFIÉ
# =============================================================================
# ÉTAT MESURÉ AVANT M3 (2026-10-24, arbre 9ca3765) :
#   21 définitions de `%||%` dans R/ + modules/ — 4 sémantiques distinctes :
#     - canonical (is.null(a) || length(a) == 0 -> b) : 16 fichiers,
#       socle = R/core/io_helpers.R:47 ;
#     - null-only (is.null(a) -> b) : R/plotting/palettes.R:8,
#       modules/import/mod_import_sc.R:21 (+ 1 définition MORTE, locale à une
#       fonction et jamais appelée : R/sc/sc_export.R:16) ;
#     - étendue NA/"" : modules/sc/mod_sc_viz.R:27-28,
#       modules/import/mod_import_bulk.R:10-13.
#
# DÉCISION (plan D2, M3 révisé, règle « grep des appels AVANT fusion ») :
#   CANONICAL GAGNE. Preuve par l'ORDRE du manifeste M1 : parmi les 21
#   définitionnaires, le DERNIER sourcé par app.R est modules/spatial/
#   mod_spatial_report.R (position 154/155) — canonical. Comme app.R source
#   tout dans UN environment global et que %||% se résout À L'APPEL, l'app
#   VIVANTE exécute déjà la sémantique canonical PARTOUT, Y COMPRIS aux sites
#   des variantes étendues (leurs définitions sont écrasées avant le premier
#   rendu réactif). Unifier vers canonical est donc la SEULE convergence à
#   zéro changement de comportement observable de l'app. Promouvoir la
#   variante étendue (comme le plan le prévoit si les appels passent NA/"")
#   CHANGERAIT le comportement de ~133 fichiers consommateurs (numericInput
#   vide = NA, textInput vidé = "" à des centaines de sites) — refusé.
#
# Ce garde est ROUGE tant que la convergence n'est pas livrée (la convergence
# est un LIVRABLE de M3, pas un invariant préexistant — plan D2, texte M3).
# =============================================================================
Sys.setlocale("LC_CTYPE", "fr_FR.UTF-8")

.pu_root <- ts_project_root()

# Extraire TOUTES les définitions de `%||%` d'un fichier (top-level OU
# imbriquées dans un corps de fonction — sc_export.R:16 était imbriquée).
# ⚠ PAS de parcours maison : `ts_ast_walk` est la PRIMITIVE PARTAGÉE de
# helper-ast.R — une copie locale de walk() est interdite par
# test-helper-ast-no-local-walk.R (dette §2dg : 14 copies en 5 fichiers).
.pu_defs_in_file <- function(path) {
  exprs <- parse(path, keep.source = FALSE)
  defs <- list()
  collect <- function(x) {
    h <- x[[1L]]
    if (length(x) >= 3L &&
        (identical(h, as.name("<-")) || identical(h, as.name("=")) ||
           identical(h, as.name("<<-"))) &&
        is.name(x[[2L]]) && identical(as.character(x[[2L]]), "%||%")) {
      defs[[length(defs) + 1L]] <<- x[[3L]]
    }
    FALSE  # ne jamais arrêter le parcours : collecter TOUTES les définitions
  }
  for (e in exprs) ts_ast_walk(e, collect)
  defs
}

# Texte normalisé d'une définition (comparaison insensible aux retours à la
# ligne — les variantes étendues sont multi-lignes).
.pu_norm <- function(expr) paste(deparse(expr, width.cutoff = 500L), collapse = " ")

# Évaluer une définition extraite dans un bac à sable (parent = base) et
# retourner la FONCTION — comportement observable, pas texte source.
.pu_eval <- function(expr) eval(expr, envir = new.env(parent = baseenv()))

# Balayage unique (niveau fichier) : R/**, modules/**, global.R, config/* —
# et, DEPUIS M4-suite (2026-10-25), tools/pkg_skeleton/R/** : les définitions
# de `%||%` SUIVENT leur fichier (palettes.R a déménagé dans le paquet) ;
# le périmètre du balayage suit le périmètre du code applicatif, sinon la
# convergence serait vérifiée sur un arbre qui n'est plus celui de l'app.
.pu_scan <- local({
  files <- sort(c(
    list.files(file.path(.pu_root, "R"), pattern = "[.]R$",
               full.names = TRUE, recursive = TRUE),
    list.files(file.path(.pu_root, "modules"), pattern = "[.]R$",
               full.names = TRUE, recursive = TRUE),
    list.files(file.path(.pu_root, "tools", "pkg_skeleton", "R"),
               pattern = "[.]R$", full.names = TRUE, recursive = TRUE),
    file.path(.pu_root, "global.R"),
    list.files(file.path(.pu_root, "config"), pattern = "[.]R$", full.names = TRUE)
  ))
  files <- files[file.exists(files)]
  defs <- lapply(files, .pu_defs_in_file)
  names(defs) <- files
  list(files = files, defs = defs)
})
.pu_files_with_defs <- .pu_scan$files[lengths(.pu_scan$defs) > 0L]
# D2 M5 lot 1 (2026-10-25) : le SOCLE io_helpers.R demenage dans l'arbre du
# paquet — l'epingle suit le chemin (meme mecanique que palettes.R en M4).
# Le compte arbre-entier ne bouge PAS : la definition suit le fichier entre
# deux racines deja balayees (R/ et tools/pkg_skeleton/R/), plancher 20/19
# inchangé.
.pu_socle_path <- file.path(.pu_root, "tools", "pkg_skeleton", "R", "io_helpers.R")
.pu_socle_defs <- .pu_scan$defs[[.pu_socle_path]]
.pu_socle_norm <- .pu_norm(.pu_socle_defs[[1L]])

test_that("socle : %||% de tools/pkg_skeleton/R/io_helpers.R porte la sémantique unifiée (canonical)", {
  expect_length(.pu_socle_defs, 1L)
  f <- .pu_eval(.pu_socle_defs[[1L]])
  expect_identical(f(NULL, "FB"), "FB")               # NULL -> repli
  expect_identical(f(character(0), "FB"), "FB")       # longueur 0 -> repli
  expect_identical(f(integer(0), 9L), 9L)
  expect_identical(f(NA, "FB"), NA)                   # NA passe AU TRAVERS
  expect_identical(f(NA_integer_, 9L), NA_integer_)
  expect_identical(f("", "FB"), "")                   # "" passe AU TRAVERS
  expect_identical(f("x", "FB"), "x")
  expect_identical(f(5, 9), 5)
})

test_that("M3 : toute définition de %||% de l'arbre est identique au socle (convergence)", {
  # Anti-vacuité : le balayage DOIT trouver les copies (21 avant M3, 20 après
  # suppression de la définition morte de sc_export.R). M4-suite (2026-10-25) :
  # le plancher 20/19 est RE-BASELE DÉLIBÉRÉMENT en étendant le balayage à
  # tools/pkg_skeleton/R/ — palettes.R (et sa définition) y a déménagé, le
  # compte arbre-entier (R/ + modules/ + paquet) reste 20 définitions / 19
  # fichiers. Ne pas confondre : le fichier palettes.R quitte R/ mais la
  # définition ne disparaît PAS de l'arbre applicatif.
  expect_gte(sum(lengths(.pu_scan$defs)), 20L)
  expect_gte(length(.pu_files_with_defs), 19L)
  for (p in .pu_scan$files) {
    for (j in seq_along(.pu_scan$defs[[p]])) {
      expect_identical(
        .pu_norm(.pu_scan$defs[[p]][[j]]), .pu_socle_norm,
        info = sprintf("%s (définition n°%d) diverge du socle tools/pkg_skeleton/R/io_helpers.R (M5 lot 1)",
                       p, j)
      )
    }
  }
})

test_that("palettes.R : la définition locale est le socle (null-only avant M3)", {
  # M4-suite (2026-10-25) : le bloc SUIT le fichier — palettes.R vit
  # désormais dans l'arbre du paquet (tools/pkg_skeleton/R/).
  f <- .pu_eval(.pu_scan$defs[[file.path(.pu_root, "tools", "pkg_skeleton", "R", "palettes.R")]][[1L]])
  expect_identical(f(NULL, "FB"), "FB")
  expect_identical(f(character(0), "FB"), "FB")  # RED avant M3 : null-only rendait character(0)
  expect_identical(f(NA, "FB"), NA)
  expect_identical(f("", "FB"), "")
})

test_that("mod_import_sc.R : la définition locale est le socle (null-only avant M3)", {
  f <- .pu_eval(.pu_scan$defs[[file.path(.pu_root, "modules", "import", "mod_import_sc.R")]][[1L]])
  expect_identical(f(NULL, "FB"), "FB")
  expect_identical(f(character(0), "FB"), "FB")  # RED avant M3
  expect_identical(f(NA, "FB"), NA)
  expect_identical(f("", "FB"), "")
})

test_that("mod_sc_viz.R : la définition locale est le socle (étendue NA/\"\" avant M3)", {
  f <- .pu_eval(.pu_scan$defs[[file.path(.pu_root, "modules", "sc", "mod_sc_viz.R")]][[1L]])
  expect_identical(f(NULL, "FB"), "FB")
  expect_identical(f(character(0), "FB"), "FB")  # RED avant M3 : la variante rendait a
  expect_identical(f(NA, "FB"), NA)              # RED avant M3 : la variante rendait "FB"
  expect_identical(f("", "FB"), "")              # RED avant M3 : la variante rendait "FB"
})

test_that("mod_import_bulk.R : la définition locale est le socle (étendue NA/\"\" avant M3)", {
  f <- .pu_eval(.pu_scan$defs[[file.path(.pu_root, "modules", "import", "mod_import_bulk.R")]][[1L]])
  expect_identical(f(NULL, "FB"), "FB")
  expect_identical(f(character(0), "FB"), "FB")  # déjà le comportement canonical avant M3
  expect_identical(f(NA, "FB"), NA)              # RED avant M3 : la variante rendait "FB"
  expect_identical(f("", "FB"), "")              # RED avant M3 : la variante rendait "FB"
})

test_that("sc_export.R : plus AUCUNE définition locale de %||% (définition morte supprimée en M3)", {
  defs <- .pu_scan$defs[[file.path(.pu_root, "tools", "pkg_skeleton", "R",
                        "sc_export.R")]]  # ex-R/sc/, D2 M5 lot 6
  expect_length(defs, 0L)  # RED avant M3 : 1 définition locale, jamais appelée
})

test_that("ordre du manifeste : le DERNIER définitionnaire sourcé par app.R est canonical (preuve M3)", {
  # Ce bloc PIN le fait mesuré qui fonde la décision « canonical gagne » : à
  # partir du manifeste M1 (ordre EXACT des 155 source() d'app.R), le dernier
  # fichier définissant %||% doit porter la sémantique du socle — sinon
  # l'app vivante changerait de sémantique effective à l'aplacement d'une
  # seule ligne. Mesuré avant M3 : mod_spatial_report.R, position 154/155.
  manifest <- readLines(file.path(.pu_root, "R", ".sources_manifest.txt"), warn = FALSE)
  manifest <- manifest[nzchar(manifest)]
  root_n <- normalizePath(.pu_root, winslash = "/")
  rel <- function(p) substring(normalizePath(p, winslash = "/"), nchar(root_n) + 2L)
  pos <- vapply(.pu_files_with_defs, function(p) match(rel(p), manifest), integer(1L))
  expect_false(any(is.na(pos)), info = "un définitionnaire n'est pas dans le manifeste")
  last_p <- .pu_files_with_defs[[which.max(pos)]]
  expect_identical(.pu_norm(.pu_scan$defs[[last_p]][[1L]]), .pu_socle_norm,
                   info = sprintf("dernier définitionnaire divergent : %s", last_p))
  expect_identical(rel(last_p), "modules/spatial/mod_spatial_report.R")
})

# =============================================================================
# tests/testthat/test-bulk-de-engine-label.R
#
# M13 (MASTERPLAN 2026-10-17) — « l'étiquette du moteur DE ne peut pas mentir ».
#
# DÉFAUT MESURÉ : `.engine_label()` (modules/bulk_de/mod_bulk_de_viz.R) lisait
# `input$de_engine`, c'est-à-dire la SÉLECTION COURANTE du panneau, alors que le
# sous-titre décrit le résultat AFFICHÉ. Le commentaire voisin affirmait
# l'inverse (« reads the SAME input the run step dispatched on »), et c'était
# faux pour au moins deux chemins :
#   - le panneau pairwise (modules/bulk/mod_bulk.R) dispatche sur
#     `input$ap_engine`, un AUTRE select ;
#   - le mode exploratoire sans réplicat dispatche TOUJOURS "edger"
#     (`fixed_dispersion`), quelle que soit la sélection.
# Changer le select après coup réécrivait donc le passé : un contraste edgeR
# s'affichait « — DESeq2 ».
#
# PROPRIÉTÉ ÉPINGLÉE : le moteur voyage AVEC le résultat (attribut `de_engine`
# posé par `bulk_de_stamp_engine()`), et l'étiquette ne lit QUE lui. Quand il
# est inconnu (contraste restauré d'une session antérieure), l'étiquette est
# VIDE — le sous-titre l'omet, au lieu de deviner.
# =============================================================================

source_project_file("tools/pkg_skeleton/R/bulk_helpers.R")

.eng_df <- function() {
  data.frame(gene = c("A", "B", "C"),
             log2FoldChange = c(2.1, -1.4, 0.2),
             padj = c(0.001, 0.04, 0.9),
             stringsAsFactors = FALSE)
}

# --- petits utilitaires AST locaux -------------------------------------------
# `helpers$register_contrast(...)` n'a PAS le symbole pour tête : c'est un `$`.
# `ts_ast_is_call_to()` ne le voit donc pas — d'où ce prédicat dédié.
.eng_is_call_named <- function(x, name) {
  if (!is.call(x)) return(FALSE)
  h <- tryCatch(x[[1]], error = function(e) NULL)
  if (is.symbol(h)) return(identical(as.character(h), name))
  if (!is.call(h)) return(FALSE)
  isTRUE(tryCatch(identical(as.character(h[[1]]), "$"), error = function(e) FALSE)) &&
    isTRUE(tryCatch(identical(as.character(h[[3]]), name), error = function(e) FALSE))
}

.eng_body_of <- function(relpath, name) {
  p <- ts_ast_parse(relpath)
  hit <- ts_ast_find(p, function(x) {
    if (!ts_ast_is_call_to(x, "<-")) return(FALSE)
    l <- tryCatch(as.list(x), error = function(e) NULL)
    if (is.null(l) || length(l) != 3L) return(FALSE)
    lhs <- tryCatch(as.character(l[[2]]), error = function(e) "")
    if (!identical(lhs, name)) return(FALSE)
    rhs <- l[[3]]
    if (!is.call(rhs) || !is.symbol(rhs[[1]])) return(FALSE)
    as.character(rhs[[1]]) %in% c("function", "reactive")
  })
  if (is.null(hit)) stop("definition '", name, "<-' introuvable dans ", relpath, call. = FALSE)
  rhs <- as.list(hit)[[3]]
  if (identical(as.character(rhs[[1]]), "function")) as.list(rhs)[[3]] else as.list(rhs)[[2]]
}

.eng_mentions_input_de_engine <- function(node) {
  found <- FALSE
  ts_ast_walk(node, function(x) {
    if (!ts_ast_is_call_to(x, "$")) return(FALSE)
    l <- tryCatch(as.list(x), error = function(e) NULL)
    if (is.null(l) || length(l) != 3L) return(FALSE)
    a <- tryCatch(as.character(l[[2]]), error = function(e) "")
    b <- tryCatch(as.character(l[[3]]), error = function(e) "")
    if (identical(a, "input") && identical(b, "de_engine")) {
      found <<- TRUE
      return(TRUE)
    }
    FALSE
  })
  found
}


# =============================================================================
test_that("M13: le moteur estampille le resultat et s'y relit", {
  df <- .eng_df()

  # Sans estampille : inconnu. PAS de repli silencieux sur une selection.
  expect_null(bulk_de_engine_of(df))
  expect_null(bulk_de_engine_of(NULL))

  s <- bulk_de_stamp_engine(df, "edger")
  expect_identical(bulk_de_engine_of(s), "edger")

  # Le contrat data.frame ne bouge pas : mêmes dimensions, mêmes colonnes.
  expect_identical(dim(s), dim(df))
  expect_identical(names(s), names(df))
  expect_identical(s$gene, df$gene)

  # Re-estampiller remplace, ne cumule pas.
  s2 <- bulk_de_stamp_engine(s, "deseq2")
  expect_identical(bulk_de_engine_of(s2), "deseq2")
  expect_identical(length(attr(s2, "de_engine")), 1L)

  # Un moteur absent/vide N'ÉCRIT RIEN (sinon on fabriquerait une fausse trace).
  expect_null(bulk_de_engine_of(bulk_de_stamp_engine(df, NULL)))
  expect_null(bulk_de_engine_of(bulk_de_stamp_engine(df, "")))
  expect_null(bulk_de_engine_of(bulk_de_stamp_engine(df, NA_character_)))
  expect_null(bulk_de_engine_of(bulk_de_stamp_engine(df, character(0))))
})


test_that("M13: l'etiquette traduit le nom technique, et reste vide si inconnu", {
  expect_identical(bulk_de_engine_label("deseq2"), "DESeq2")
  expect_identical(bulk_de_engine_label("edger"), "edgeR")
  expect_identical(bulk_de_engine_label("limma"), "limma-voom")

  # Deja lisible : rendu tel quel, jamais double-traduit.
  expect_identical(bulk_de_engine_label("DESeq2"), "DESeq2")

  # Inconnu mais renseigne : on ne prétend pas le contraire, on majuscule.
  expect_identical(bulk_de_engine_label("nonsense"), "NONSENSE")

  # Rien a dire : chaine vide (le sous-titre OMET alors le moteur).
  expect_identical(bulk_de_engine_label(NULL), "")
  expect_identical(bulk_de_engine_label(NA_character_), "")
  expect_identical(bulk_de_engine_label(character(0)), "")
  expect_identical(bulk_de_engine_label(""), "")
  expect_identical(bulk_de_engine_label("   "), "")
})


test_that("M13: l'etiquette resout le moteur du contraste AFFICHE", {
  df <- .eng_df()
  ct <- list(A_vs_B = bulk_de_stamp_engine(df, "edger"),
             C_vs_D = bulk_de_stamp_engine(df, "deseq2"))

  expect_identical(bulk_de_active_engine_label(ct, "A_vs_B"), "edgeR")
  expect_identical(bulk_de_active_engine_label(ct, "C_vs_D"), "DESeq2")

  # Le contraste ACTIF décide, pas l'ordre de la liste.
  expect_identical(bulk_de_active_engine_label(ct[2:1], "A_vs_B"), "edgeR")

  # Cibles absentes : vide, jamais une exception (l'appelant est un render).
  expect_identical(bulk_de_active_engine_label(ct, NULL), "")
  expect_identical(bulk_de_active_engine_label(ct, "absente"), "")
  expect_identical(bulk_de_active_engine_label(list(), "A_vs_B"), "")
  expect_identical(bulk_de_active_engine_label(NULL, "A_vs_B"), "")

  # TÉMOIN DE NON-VACUITÉ : un contraste PRÉSENT mais NON estampillé rend "",
  # et non l'étiquette de son voisin estampillé (mesuré : une injection
  # « repli sur le premier de la liste » ne fait tomber QUE la ligne 142
  # ci-dessus — ce témoin-ci prouve l'absence de contamination entre
  # contrastes, pas l'absence de repli sur une cible absente).
  ct$unstamped <- df
  expect_identical(bulk_de_active_engine_label(ct, "unstamped"), "")

  # La résolution n'est que la composition des deux primitives (pas de 3e
  # vérité qui pourrait diverger).
  expect_identical(bulk_de_active_engine_label(ct, "A_vs_B"),
                   bulk_de_engine_label(bulk_de_engine_of(ct$A_vs_B)))
})


test_that("M13: .engine_label ne lit JAMAIS la selection courante", {
  body <- .eng_body_of("modules/bulk_de/mod_bulk_de_viz.R", ".engine_label")

  # LE défaut d'origine : le sous-titre suivait `input$de_engine`.
  expect_false(.eng_mentions_input_de_engine(body))

  # Et il lit bien le moteur STOCKÉ, via le résolveur unique.
  expect_true(grepl("bulk_de_active_engine_label", ts_ast_deparse(body), fixed = TRUE))

  # Témoin de non-vacuité du détecteur : il voit bien `input$de_engine` quand
  # il y en a (sinon l'assertion ci-dessus passerait sur un détecteur muet).
  probe <- parse(text = "function() switch(input$de_engine, deseq2 = 'DESeq2')")[[1]][[3]]
  expect_true(.eng_mentions_input_de_engine(probe))
})


test_that("M13: tous les ecrivains de contrastes estampillent leur moteur", {
  # Les trois sites passant par le helper partagé.
  for (spec in list(
    list(file = "modules/bulk_de/mod_bulk_de_run.R",       n = 2L),
    list(file = "modules/bulk_de/mod_bulk_de_pairwise.R",  n = 1L)
  )) {
    p <- ts_ast_parse(spec$file)
    calls <- ts_ast_find_all(p, function(x) .eng_is_call_named(x, "register_contrast"))
    expect_identical(length(calls), spec$n, info = spec$file)
    for (cl in calls) {
      nm <- names(as.list(cl))
      expect_true("engine" %in% nm, info = paste(spec$file, ts_ast_deparse(cl)))
      # Un `engine = NULL` explicite porterait le NOM sans rien estampiller :
      # on refuse la constante NULL comme argument.
      expect_false(identical(as.list(cl)[["engine"]], quote(NULL)),
                   info = paste(spec$file, ts_ast_deparse(cl)))
    }
  }

  # Les deux écritures DIRECTES de mod_bulk.R (panneau pairwise, autre select
  # `ap_engine`) doivent estampiller aussi, sinon elles restent muettes.
  p2 <- ts_ast_parse("modules/bulk/mod_bulk.R")
  stamps <- ts_ast_find_all(p2, function(x) .eng_is_call_named(x, "bulk_de_stamp_engine"))
  expect_identical(length(stamps), 2L)

  # Le helper partagé estampille lui-même : un appelant qui oublierait l'attribut
  # ne peut pas produire un contraste muet en passant par `register_contrast`.
  hb <- .eng_body_of("modules/bulk_de/mod_bulk_de_engine.R", "register_contrast")
  expect_true(grepl("bulk_de_stamp_engine", ts_ast_deparse(hb), fixed = TRUE))
})


test_that("M13: le recalcul live de padj ne perd pas l'estampille", {
  # `active_de_results()` peut RECONSTRUIRE le data.frame via
  # extract_deseq2_contrast() (changement de méthode padj). Un `as.data.frame()`
  # ne transporte pas les attributs personnalisés : sans re-stampille, le
  # sous-titre redeviendrait muet précisément quand l'utilisateur change la
  # correction — la panne la plus difficile à voir.
  body <- .eng_body_of("modules/bulk_de/mod_bulk_de_viz.R", "active_de_results")
  expect_true(grepl("bulk_de_stamp_engine", ts_ast_deparse(body), fixed = TRUE))
})

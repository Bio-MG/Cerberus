# =============================================================================
# test-bulk-gene-sets.R — PLOT-S6b : catalogue de jeux de gènes NATIF
# =============================================================================
# Contrat gelé : docs/contracts/BULK_GENE_SETS_CONTRACT.md
#
# Ce que ce fichier couvre :
#   1. le gel de la surface publique (symboles + formals) ;
#   2. le contrat de `bulk_gene_set_choices()` (convention C13 : la VALEUR est
#      l'identifiant de source, le LIBELLÉ est le texte affiché) ;
#   3. la disponibilité déclarée (jamais devinée) et les erreurs classées ;
#   4. le chargement réel des sources natives LOCALES (aucun réseau) ;
#   5. l'aller-retour .gmt (bulk_write_gmt -> bulk_parse_gmt) ;
#   6. la référence au contrat gelé (garde C8).
# =============================================================================
source_project_file("R/core/validation.R")
source_project_file("R/core/provenance.R")
# D2 M4 (2026-10-25) : R/plotting vit desormais dans l'arbre du paquet
# (tools/pkg_skeleton/R/) — charge via load_project_pkg() (cf. helper-source.R).
load_project_pkg()
source_project_file("R/bulk/bulk_batch_qc.R")
source_project_file("R/bulk/bulk_gsva.R")
source_project_file("R/bulk/bulk_gene_sets.R")

test_that("surface publique figée (symboles + formals)", {
  expect_setequal(
    bulk_gene_sets_public_api(),
    c("bulk_gene_sets_public_api", "bulk_gene_set_catalog", "bulk_gene_set_choices",
      "bulk_gene_sets_organisms", "bulk_load_gene_sets", "bulk_write_gmt")
  )
  expected_api <- list(
    bulk_gene_sets_public_api = list(),
    bulk_gene_set_catalog      = list(),
    bulk_gene_set_choices      = list(tr = NULL),
    bulk_gene_sets_organisms   = list(),
    bulk_load_gene_sets        = list(source = NULL, organism = NULL),
    bulk_write_gmt             = list(gene_sets = NULL, path = NULL)
  )
  for (fn in names(expected_api)) {
    f <- get(fn, mode = "function")
    expect_identical(names(formals(f)), names(expected_api[[fn]]), info = fn)
  }
  expect_identical(bulk_gene_sets_organisms(), c("human", "mouse"))
})

test_that("le contrat gelé est référencé et présent (garde C8)", {
  p <- file.path(ts_project_root(), "docs", "contracts", "BULK_GENE_SETS_CONTRACT.md")
  skip_if_not(file.exists(p), .ts_contract_skip_msg(p))
  txt <- paste(.ts_contract_readlines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  for (fn in bulk_gene_sets_public_api()) expect_match(txt, fn, fixed = TRUE)
})

test_that("catalogue : colonnes stables, disponibilité DÉCLARÉE (jamais devinée)", {
  cat_df <- bulk_gene_set_catalog()
  expect_identical(
    colnames(cat_df),
    c("source", "label", "provider", "collection", "subcollection",
      "requires", "available", "description"))
  expect_true(nrow(cat_df) >= 11L)
  expect_identical(anyDuplicated(cat_df$source), 0L)
  expect_true(all(nzchar(cat_df$label)))
  # `available` ne reflète QUE les paquets localement installés
  msig <- cat_df$provider == "msigdbr"
  expect_identical(unique(cat_df$available[msig]),
                   requireNamespace("msigdbr", quietly = TRUE))
  expect_identical(cat_df$available[cat_df$source == "progeny"],
                   requireNamespace("progeny", quietly = TRUE))
  expect_identical(cat_df$available[cat_df$source == "dorothea"],
                   requireNamespace("dorothea", quietly = TRUE))
  expect_true(cat_df$available[cat_df$source == "file"])
})

test_that("choices : la VALEUR est l'identifiant de source, le nom est le libellé (C13)", {
  ch <- bulk_gene_set_choices()
  expect_false(is.null(names(ch)))
  expect_setequal(unname(ch), bulk_gene_set_catalog()$source)
  # le libellé est bien le NOM, jamais la valeur
  expect_identical(unname(ch["MSigDB Hallmark (50 voies)"]), "msigdb_hallmark")
  # une fonction de traduction est appliquée aux libellés seulement
  ch2 <- bulk_gene_set_choices(function(x) paste0("XX", x))
  expect_setequal(unname(ch2), unname(ch))
  expect_true(all(startsWith(names(ch2), "XX")))
})

test_that("chargement : sources natives LOCALES, aucun fichier requis", {
  if (!requireNamespace("msigdbr", quietly = TRUE)) skip("msigdbr absent")
  h <- bulk_load_gene_sets("msigdb_hallmark", "human")
  expect_type(h, "list")
  expect_length(h, 50L)
  expect_true(all(grepl("^HALLMARK_", names(h))))
  expect_true(all(vapply(h, function(g) is.character(g) && length(g) > 0L, logical(1))))
  # forme IDENTIQUE à bulk_parse_gmt() : même type de sortie
  expect_type(bulk_load_gene_sets("msigdb_hallmark", "mouse"), "list")
  # une collection à sous-collection, et une sans (NA -> NULL côté msigdbr)
  expect_length(bulk_load_gene_sets("msigdb_reactome", "human"), 1839L)
  expect_true(length(bulk_load_gene_sets("msigdb_oncogenic", "human")) > 0L)
})

test_that("chargement : PROGENy et DoRothEA lus LOCALEMENT (pas via OmnipathR)", {
  # On ne teste QUE si le paquet de données est installé ; le cas « absent »
  # est couvert par l'erreur classée ci-dessous.
  if (requireNamespace("progeny", quietly = TRUE)) {
    p <- bulk_load_gene_sets("progeny", "human")
    expect_length(p, 14L)                    # 14 voies PROGENy
    expect_true(all(lengths(p) > 0L))
  }
  if (requireNamespace("dorothea", quietly = TRUE)) {
    d <- bulk_load_gene_sets("dorothea", "human")
    expect_true(length(d) > 100L)            # ~271 TF de confiance A/B/C
    expect_true(all(lengths(d) > 0L))
  }
})

test_that("erreurs classées : source inconnue, organisme, dépendance, source file", {
  e1 <- tryCatch(bulk_load_gene_sets("inexistant"), error = function(e) e)
  expect_s3_class(e1, "bulk_gene_sets_error")
  expect_identical(e1$state, "invalid_input")

  e2 <- tryCatch(bulk_load_gene_sets("msigdb_hallmark", "rat"), error = function(e) e)
  expect_identical(e2$state, "invalid_input")

  e3 <- tryCatch(bulk_load_gene_sets("file", "human"), error = function(e) e)
  expect_identical(e3$state, "invalid_input")
  expect_match(conditionMessage(e3), "bulk_parse_gmt")

  # source optionnelle absente -> missing_dependency citant le paquet
  absent <- Filter(function(s) !isTRUE(bulk_gene_set_catalog()$available[
    bulk_gene_set_catalog()$source == s]),
    c("progeny", "dorothea"))
  for (s in absent) {
    e4 <- tryCatch(bulk_load_gene_sets(s, "human"), error = function(e) e)
    expect_s3_class(e4, "bulk_gene_sets_error")
    expect_identical(e4$state, "missing_dependency")
    expect_match(conditionMessage(e4), s)
  }
})

test_that("bulk_write_gmt : aller-retour EXACT, et refuse ce qui serait vide", {
  sets <- list(SET_A = c("G1", "G2", "G1"), SET_B = c("G3", "G4"))
  p <- file.path(tempdir(), paste0("gs_", Sys.getpid(), ".gmt"))
  on.exit(unlink(p), add = TRUE)
  invisible(bulk_write_gmt(sets, p))
  back <- bulk_parse_gmt(p)
  expect_setequal(names(back), names(sets))
  expect_identical(sort(unique(back$SET_A)), c("G1", "G2"))   # doublon dédupliqué
  expect_identical(sort(back$SET_B), c("G3", "G4"))

  # un jeu sans gène est écarté plutôt qu'écrit vide
  invisible(bulk_write_gmt(list(A = c("G1"), VIDE = character(0)), p))
  expect_identical(names(bulk_parse_gmt(p)), "A")

  expect_error(bulk_write_gmt(list(), p), class = "bulk_gene_sets_error")
  expect_error(bulk_write_gmt(list(A = "G1"), ""), class = "bulk_gene_sets_error")
  expect_error(bulk_write_gmt(list(A = character(0)), p), class = "bulk_gene_sets_error")
  expect_error(bulk_write_gmt(setNames(list("G1"), ""), p), class = "bulk_gene_sets_error")
})

# -----------------------------------------------------------------------------
# M5 (2026-10-17) — PROGENy : chaque voie doit recevoir SES propres cibles
# -----------------------------------------------------------------------------
# Défaut mesuré sur `main` : `lapply(seq_len(ncol(m)), function(j) genes)` —
# l'indice `j` n'était JAMAIS lu. Les 14 voies recevaient donc l'UNION des
# lignes du modèle (jusqu'à plusieurs milliers de gènes) au lieu de leurs ~`top`
# cibles. Conséquence : les scores de voies devenaient quasi colinéaires, sans
# la moindre erreur ni le moindre avertissement — un défaut de SCIENCE
# silencieux, pas un plantage.
#
# ⚠️ `progeny` est ABSENT de la bibliothèque du projet sur cet hôte : le bloc
# d'exécution de `bulk_load_gene_sets("progeny", ...)` plus haut est donc SAUTÉ.
# Le défaut est néanmoins décidable sans le paquet, par deux voies :
#   (1) un garde AST interdit le retour de l'argument MORT (c'est la CAUSE) ;
#   (2) la logique de découpage est extraite en fonction PURE, testée sur une
#       matrice synthétique (le COMPORTEMENT, pas la forme).
# -----------------------------------------------------------------------------

#' Les définitions `<- function(...)` du fichier, par NOM.
.bgs_progeny_defs <- function(names_wanted) {
  p   <- ts_ast_parse("R/bulk/bulk_gene_sets.R")
  out <- list()
  for (i in seq_along(p)) {                      # (1) par INDEX — cf. helper-ast.R
    ts_ast_walk(p[[i]], function(x) {
      if (!ts_ast_is_call_to(x, "<-")) return(FALSE)
      l <- tryCatch(as.list(x), error = function(e) NULL)
      if (is.null(l) || length(l) != 3L) return(FALSE)
      nm <- tryCatch(as.character(l[[2]]), error = function(e) "")
      if (!nzchar(nm) || !nm %in% names_wanted) return(FALSE)
      out[[nm]] <<- l[[3]]
      FALSE
    })
  }
  out
}

#' Le symbole `name` apparaît-il dans `expr` ?
#'
#' `all.names()` est le parcours de R : inutile d'en écrire un. (Première version :
#' un `rec()` récursif local — une copie de plus du parcours que `helper-ast.R`
#' centralise ; `all.names` la supprime au lieu de la déplacer.)
.bgs_uses_symbol <- function(expr, name) {
  name %in% all.names(expr, functions = TRUE, unique = TRUE)
}

#' Les arguments formels DÉCLARÉS mais JAMAIS lus dans les lambdas de `expr`.
#'
#' Le parcours est celui de `helper-ast.R` (`ts_ast_walk`) : une copie locale
#' nommée `walk` faisait rougir `test-helper-ast-no-local-walk.R` — le garde a
#' joué son rôle sur du code neuf, ce qui est exactement ce qu'on lui demande.
.bgs_dead_formals <- function(expr) {
  dead <- character(0)
  ts_ast_walk(expr, function(e) {
    head <- tryCatch(e[[1]], error = function(err) NULL)
    if (identical(head, as.name("function"))) {
      fs <- tryCatch(names(as.list(e[[2]])), error = function(err) NULL)
      for (a in fs) {                            # (1) par INDEX
        if (!.bgs_uses_symbol(e[[3]], a)) dead <<- c(dead, a)
      }
    }
    FALSE                                        # jamais d'arrêt : tout l'arbre
  })
  unique(dead)
}

test_that("PROGENy : aucune lambda à argument mort dans le découpage par voie", {
  defs <- .bgs_progeny_defs(c(".bulk_gs_progeny_sets",
                              ".bulk_gs_progeny_sets_from_matrix"))
  expect_true(".bulk_gs_progeny_sets" %in% names(defs))

  bad <- character(0)
  for (nm in names(defs)) {                      # (1) par INDEX
    d <- .bgs_dead_formals(defs[[nm]])
    if (length(d) > 0L) {
      bad <- c(bad, sprintf("%s(function(%s))", nm, paste(d, collapse = ", ")))
    }
  }
  # Un `function(j) <corps>` qui n'emploie pas `j` est EXACTEMENT la forme du
  # défaut : la voie reçoit alors la même liste que ses 13 voies sœurs.
  expect_identical(bad, character(0),
                   info = paste("lambdas à argument mort :",
                                paste(bad, collapse = " | ")))
})

test_that("PROGENy : chaque voie reçoit SES cibles, pas l'union du modèle", {
  # Matrice synthétique 5 gènes x 3 voies. Un 0 = « ce gène n'est pas une cible
  # de CETTE voie » — exactement la structure d'un modèle PROGENy découpé par
  # `top`.
  m <- matrix(c(
     3,  0, -2,
     0,  4,  0,
    -1,  0,  5,
     0, -3,  0,
     2,  0,  0
  ), nrow = 5L, byrow = TRUE,
  dimnames = list(c("G1", "G2", "G3", "G4", "G5"), c("p1", "p2", "p3")))

  sets <- .bulk_gs_progeny_sets_from_matrix(m, top = 500L)

  expect_identical(names(sets), c("p1", "p2", "p3"))
  # Classement par |poids| décroissant, poids nuls exclus.
  expect_identical(sets$p1, c("G1", "G5", "G3"))   # |3| > |2| > |1|
  expect_identical(sets$p2, c("G2", "G4"))         # |4| > |3|
  expect_identical(sets$p3, c("G3", "G1"))         # |5| > |2|
  # Le défaut historique rendait TROIS copies de `rownames(m)` : ceci l'interdit.
  expect_identical(sum(duplicated(sets)), 0L)
  expect_false(identical(sets$p1, rownames(m)))

  # `top` borne les cibles PAR VOIE (pas globalement), et la troncature garde
  # les poids les plus forts.
  t2 <- .bulk_gs_progeny_sets_from_matrix(m, top = 2L)
  expect_identical(t2$p1, c("G1", "G5"))
  expect_identical(t2$p2, c("G2", "G4"))
  expect_identical(t2$p3, c("G3", "G1"))

  # Une voie sans aucune cible rend un vecteur VIDE (le chargeur écarte ensuite
  # les jeux vides) — jamais les gènes des autres voies.
  m2 <- m; m2[, "p2"] <- 0
  s2 <- .bulk_gs_progeny_sets_from_matrix(m2)
  expect_identical(s2$p2, character(0))
  expect_identical(s2$p1, c("G1", "G5", "G3"))

  # `top` invalide retombe sur 500 sans erreur (jamais un `head(, NA)`).
  expect_identical(.bulk_gs_progeny_sets_from_matrix(m, top = NA)$p1,
                   c("G1", "G5", "G3"))
})

test_that("PROGENy : le chargeur public passe par le découpage par voie", {
  defs <- .bgs_progeny_defs(".bulk_gs_progeny_sets")
  expect_length(defs, 1L)
  # Le helper n'est utile que s'il est réellement CÂBLÉ : on l'exige dans le
  # corps du chargeur, pas seulement sur le disque.
  expect_match(ts_ast_deparse(defs[[1]]),
               ".bulk_gs_progeny_sets_from_matrix", fixed = TRUE)
})

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
source_project_file("R/plotting/theme.R")
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

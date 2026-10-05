# =============================================================================
# test-drive-verbatim-guard.R — la garde D1, épingle AVANT son corps (R1).
# =============================================================================
# D1 (opérateur, 2026-10-04, périmètre restreint) : le verbatim n'est permis
# QUE pour les valeurs de cellules de la preview de transcripto_drive_read et
# les libellés/choix du vocabulaire session-dérivé (Slice 3). Chaque chaîne
# verbatim passe UNE garde, partagée par les DEUX consommateurs :
#
#   - max 200 caractères (plafond dur TS_DRIVE_READ_MAX_CELL_CHARS) ;
#   - refuse '..' ; refuse '/' ; refuse '\' ; refuse '~' ;
#   - refuse les caractères de contrôle ;
#   - refuse les NA ambigus : un NA en ENTRÉE sort NA (sémantique « émettre
#     null »), jamais la chaîne "NA" fabriquée ;
#   - bornes totales déclarées (compteurs côté appelant, pas ici).
#
# Contrat de retour : character de MÊME LONGUEUR que l'entrée ; élément qui
# passe = la chaîne ; élément refusé = NA. Pure, sans I/O — l'appelant
# substitue le marqueur déclaré (TS_DRIVE_READ_CELL_MARKERS) et compte.
#
# Le fichier est écrit AVANT le corps de la fonction (rouge mesuré) : la
# signature ts_drive_verbatim_guard(x, max_chars) est le contrat, les tests
# ci-dessous sont sa définition exécutable.
# =============================================================================

source_project_file("R/core/drive_allowlist.R")

.g <- function(...) get0("ts_drive_verbatim_guard", envir = globalenv())

.rep <- function(s, n) paste(rep(s, n), collapse = "")

test_that("the verbatim guard passes a boundary of exactly 200 characters and refuses 201", {
  g <- .g()
  skip_if(is.null(g), "ts_drive_verbatim_guard() not implemented yet (RED expected)")

  expect_identical(as.integer(TS_DRIVE_READ_MAX_CELL_CHARS), 200L)

  ok200 <- .rep("x", 200L)
  expect_identical(g(ok200), ok200)                     # 200 passe
  expect_identical(g(.rep("x", 199L)), .rep("x", 199L)) # 199 passe
  expect_true(is.na(g(.rep("x", 201L))))                # 201 est refusé
})

test_that("the verbatim guard refuses each declared character class", {
  g <- .g()
  skip_if(is.null(g), "ts_drive_verbatim_guard() not implemented yet (RED expected)")

  # Chaque refus explicite de D1, un par un, dans une chaîne sinon anodine.
  expect_true(is.na(g("gene1/gene2")),  info = "'/' est refuse (regle geneID mesurée)")
  expect_true(is.na(g("a..b")),         info = "'..' est refuse (traversee)")
  expect_true(is.na(g("back\\slash")),  info = "'\\' est refuse")
  expect_true(is.na(g("home~dir")),     info = "'~' est refuse")
  expect_true(is.na(g("line1\nline2")), info = "LF est un caractere de controle")
  expect_true(is.na(g("tab\there")),    info = "TAB est un caractere de controle")
  expect_true(is.na(g("nul\001char")),  info = "caractere de controle arbitraire")
})

test_that("the verbatim guard keeps NA semantics and honest pass-throughs", {
  g <- .g()
  skip_if(is.null(g), "ts_drive_verbatim_guard() not implemented yet (RED expected)")

  # NA en entrée => NA en sortie (« émettre null »), JAMAIS la chaîne "NA".
  expect_true(is.na(g(NA_character_)))
  expect_identical(g("NA"), "NA",  info = "le literal 'NA' authentique reste une donnee")
  expect_identical(g(""),    "",   info = "une chaine vide est une donnee legale")

  # Un nom court passe : les identifiants de gènes type BRCA1 survivent à D1.
  expect_identical(g("BRCA1"), "BRCA1")
})

test_that("the verbatim guard is vectorised and length-preserving", {
  g <- .g()
  skip_if(is.null(g), "ts_drive_verbatim_guard() not implemented yet (RED expected)")

  v <- c("ok", "a/b", NA_character_, .rep("x", 201L), "fine")
  out <- g(v)
  expect_length(out, length(v))
  expect_identical(out[[1]], "ok")
  expect_true(is.na(out[[2]]))
  expect_true(is.na(out[[3]]))
  expect_true(is.na(out[[4]]))
  expect_identical(out[[5]], "fine")
})

test_that("the verbatim guard honours the declared ceiling as a parameter", {
  g <- .g()
  skip_if(is.null(g), "ts_drive_verbatim_guard() not implemented yet (RED expected)")

  expect_true(is.na(g(.rep("y", 11L), max_chars = 10L)))
  expect_identical(g(.rep("y", 10L), max_chars = 10L), .rep("y", 10L))
})

# =============================================================================
# test-conventions-c6-strings.R — cas NÉGATIFS des règles C6 et C10
# =============================================================================
# Règle du dépôt : une règle statique doit être éprouvée sur un cas NÉGATIF
# réellement injecté. Un garde qui affiche « 0 erreur » ne prouve rien si on ne
# l'a jamais vu passer au rouge — et, symétriquement, un garde qui crie au loup
# fabrique une dette qui n'existe pas.
#
# DÉFAUT 1 (mesuré le 2026-09-16) : `.strip_strings_and_comments()` applique des
# regex LIGNE PAR LIGNE. Elle ne peut donc pas voir une chaîne qui s'étend sur
# PLUSIEURS lignes — alors que son propre commentaire annonce couvrir ce cas, en
# citant `R/sc/sc_export.R`. Or `R/sc/sc_export.R:47` est bel et bien signalé.
# Le fichier `R/bulk/bulk_report_engine.R` en est le cas massif : il EMBARQUE le
# texte d'un script R reproductible dans des littéraux `'...'` multi-lignes, avec
# des `library()` à l'intérieur. Ces `library()` sont signalés C6 alors qu'ils ne
# sont JAMAIS exécutés au `source()` — ce sont des caractères dans une chaîne.
# Mesure du 2026-09-16 : sur les 16 signalements C6, 11 sont réels, 5 sont faux.
#
# DÉFAUT 2 (mesuré le 2026-09-17) : `check_c6_library_in_r()` comptait les
# accolades avec `gregexpr("\\{", ln, fixed = TRUE)`, qui cherche la chaîne de
# DEUX caractères `\{` et ne trouve donc JAMAIS `{`. Le compteur restait bloqué
# à 0, `depth == 0L` était toujours vrai, et la règle « au top-level de R/ »
# signalait en réalité `library()` à N'IMPORTE QUELLE profondeur. Les 11
# signalements restants étaient TOUS imbriqués (profondeur 1 à 4) : la dette C6
# affichée était entièrement un artefact de mesure. Après correctif : C6 = 0.
#
# DÉFAUT 3 (mesuré le 2026-09-17) : `check_c10_error_style()` jugeait UNE SEULE
# ligne. Un appel multi-lignes parfaitement conforme —
#     stop("message",
#          "suite", call. = FALSE)
# — était donc signalé à tort, `call. = FALSE` vivant sur la ligne suivante. Sur
# 270 signalements C10, 49 (18 %) portaient déjà `call. = FALSE` DANS l'appel.
# Après correctif : C10 = 221.
#
# DÉFAUT 4 (mesuré le 2026-09-17) : analyser l'étendue du `stop()` ne suffit
# pas. Le motif maison route l'erreur par un constructeur local —
#     stop(.bulk_multi_stop("Label vide.", state = "invalid_label"))
# — où `.bulk_multi_stop()` est une fonction dont le corps EST un
# `errorCondition(..., class = "bulk_multi_error")`. Le site est classé, mais
# aucun token `errorCondition` n'apparaît dans le `stop()` : la garde le
# déclarait « non classé ». 46 des 221 signalements étaient de ce type
# (18 `.bulk_multi_stop`, 15 `.sc_multi_stop`, 13 `.bulk_multi_compare_stop`).
# Après correctif : C10 = 175.
#
# Ce fichier vérifie les DEUX directions :
#   A. ce qui n'est PAS une violation n'est PAS signalé (faux positifs) ;
#   B. ce qui EST une violation reste TOUJOURS signalé — le garde ne doit pas
#      devenir aveugle en devenant précis ;
#   C. les pièges d'état : apostrophe française dans un commentaire, `#` dans une
#      chaîne, apostrophes dans une chaîne double.
# =============================================================================

source_project_file("tools/check_conventions.R")

#' Chemin d'un fichier du dépôt (les tests s'exécutent depuis tests/testthat/).
.c6_repo <- function(rel) file.path(ts_project_root(), rel)

#' Écrit une fixture .R et rend son chemin.
.c6_write_fixture <- function(lines) {
  dir <- tempfile("c6fix_")
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "fixture_c6.R")
  writeLines(lines, path, useBytes = TRUE)
  path
}

#' Rejoue la SEULE règle C6 sur un fichier et rend les lignes signalées.
.c6_flagged_lines <- function(path) {
  .REPORT$warns <- list()
  check_c6_library_in_r(path)
  if (!length(.REPORT$warns)) return(integer(0))
  sort(vapply(.REPORT$warns,
              function(w) as.integer(w$line),
              integer(1), USE.NAMES = FALSE))
}

test_that("C6 : un library() qui n'est pas du code n'est PAS signale", {
  lines <- c(
    "# Fixture C6 — negatifs et positifs",                  #  1
    "library(Seurat)",                                      #  2  REEL
    "",                                                     #  3
    "# chaine MULTI-LIGNES : library() n'est pas du code",  #  4
    "script <- '# Script reproductible",                    #  5  ouvre  '
    "library(DESeq2); library(ggplot2)",                    #  6  FAUX (dans la chaine)
    "x <- 1",                                               #  7  FAUX (dans la chaine)
    "'",                                                    #  8  ferme  '
    "",                                                     #  9
    "one <- \"library(patchwork)\"",                        # 10  FAUX (chaine 1 ligne)
    "",                                                     # 11
    "# library(rlang)",                                     # 12  FAUX (commentaire)
    "",                                                     # 13
    "# l'analyse des donnees",                              # 14  piege : apostrophe
    "library(dplyr)",                                       # 15  REEL
    "",                                                     # 16
    "s2 <- \"texte # pas un commentaire\"",                 # 17  piege : # dans chaine
    "library(ggplot2)",                                     # 18  REEL
    "",                                                     # 19
    "s3 <- \"l'analyse et l'autre\"",                       # 20  piege : apostrophes
    "library(tidyr)"                                        # 21  REEL
  )
  path <- .c6_write_fixture(lines)
  on.exit(unlink(dirname(path), recursive = TRUE), add = TRUE)

  flagged <- .c6_flagged_lines(path)

  # A. Les faux positifs disparaissent.
  expect_false(6L %in% flagged,
               info = "library() dans une chaine MULTI-LIGNES signale a tort")
  expect_false(10L %in% flagged,
               info = "library() dans une chaine mono-ligne signale a tort")
  expect_false(12L %in% flagged,
               info = "library() dans un commentaire signale a tort")

  # B. Le garde ne devient pas aveugle : les 4 vrais library() (top-level)
  #    restent signales.
  expect_identical(flagged, c(2L, 15L, 18L, 21L),
                   info = paste0("attendu les lignes 2,15,18,21 ; obtenu : ",
                                 paste(flagged, collapse = ",")))
})

test_that("C6 : l'etat de chaine n'est pas corrompu par les pieges", {
  # Si une apostrophe de commentaire ouvrait une chaine, ou si le `#` d'une
  # chaine ouvrait un commentaire, la ligne SUIVANTE serait avalee et un vrai
  # library() disparaitrait du verdict. On le mesure directement.
  lines <- c(
    "library(Seurat)",            # 1 REEL
    "# l'analyse de l'autre",     # 2 commentaire, 2 apostrophes
    "library(dplyr)",             # 3 REEL (doit survivre au piege ci-dessus)
    "s <- \"a # b l'c\"",         # 4 `#` et apostrophe DANS une chaine double
    "library(tidyr)"              # 5 REEL (doit survivre au piege ci-dessus)
  )
  path <- .c6_write_fixture(lines)
  on.exit(unlink(dirname(path), recursive = TRUE), add = TRUE)

  expect_identical(.c6_flagged_lines(path), c(1L, 3L, 5L))
})

test_that("C6 : le cas REEL cite par le commentaire de la garde est corrige", {
  # Le commentaire de .strip_strings_and_comments() promet que sc_export.R
  # n'est pas signale. On verifie la promesse sur le fichier du depot.
  src <- readLines(.c6_repo("R/sc/sc_export.R"), warn = FALSE, encoding = "UTF-8")
  # Pre-requis : la ligne 47 doit bien etre DANS un litteral (sinon le test
  # ne prouve rien et il faut le reecrire, pas le laisser passer).
  upto <- paste(src[1:47], collapse = "\n")
  n_quotes <- nchar(gsub("[^']", "", gsub("\\\\.", "", upto)))
  expect_identical(n_quotes %% 2L, 1L,
                   info = "sc_export.R:47 n'est plus dans une chaine : test a reecrire")

  expect_false(47L %in% .c6_flagged_lines(.c6_repo("R/sc/sc_export.R")))
})

test_that("C6 : la profondeur d'accolades est REELLEMENT mesuree", {
  # Contre-preuve des DEUX directions, sur fixture injectee. La regle ne vise
  # que le TOP-LEVEL — ce qui s'execute au source() et attache le paquet
  # globalement (CONVENTIONS.md §2 et §3.4). Avant le correctif du 2026-09-17,
  # le compteur d'accolades etait bloque a 0 : les lignes 3 et 5 etaient
  # signalees a tort.
  lines <- c(
    "library(top_level)",              # 1 SIGNALE  (profondeur 0)
    "f <- function() {",               # 2
    "  library(dans_fonction)",        # 3 EXEMPTE  (profondeur 1)
    "  if (TRUE) {",                   # 4
    "    require(dans_if)",            # 5 EXEMPTE  (profondeur 2)
    "  }",                             # 6
    "}",                               # 7
    "library(encore_top_level)"        # 8 SIGNALE  (retour a 0)
  )
  path <- .c6_write_fixture(lines)
  on.exit(unlink(dirname(path), recursive = TRUE), add = TRUE)

  flagged <- .c6_flagged_lines(path)
  expect_identical(flagged, c(1L, 8L),
                   info = paste0("attendu 1 et 8 ; obtenu : ",
                                 paste(flagged, collapse = ",")))
})

test_that("C6 : les library() imbriques du depot ne sont plus signales", {
  # Les 11 signalements restants etaient TOUS imbriques. On verifie la mesure
  # sur les fichiers reels, avec la profondeur en PRE-REQUIS : si un de ces
  # appels remontait un jour au top-level, c'est le TEST qu'il faut reecrire —
  # pas laisser passer en silence.
  sites <- list(
    c("R/core/pathway_helpers.R", 59L),   # library(clusterProfiler)
    c("R/core/pathway_helpers.R", 75L),   # library(org.Hs.eg.db)
    c("R/spatial/spatial_async.R", 222L), # library(Seurat) dans un worker mirai
    c("R/bulk/bulk_wgcna.R", 52L)         # require("WGCNA") dans une fonction
  )
  for (site in sites) {
    f <- .c6_repo(site[1]); ln <- as.integer(site[2])
    code <- .read_code_lines(f)$code
    depth <- 0L
    if (ln > 1L) {
      for (i in seq_len(ln - 1L)) {
        depth <- depth + .count_chars(code[i], "{") - .count_chars(code[i], "}")
      }
    }
    expect_true(depth > 0L,
                info = sprintf("%s:%d n'est plus imbrique (profondeur %d) : test a reecrire",
                               site[1], ln, depth))
    expect_false(ln %in% .c6_flagged_lines(f),
                 info = sprintf("%s:%d (profondeur %d) signale a tort",
                                site[1], ln, depth))
  }
})

#' Rejoue la SEULE règle C10 sur un fichier et rend les lignes signalées.
.c10_flagged_lines <- function(path) {
  .REPORT$warns <- list()
  check_c10_error_style(path)
  if (!length(.REPORT$warns)) return(integer(0))
  sort(vapply(.REPORT$warns,
              function(w) as.integer(w$line),
              integer(1), USE.NAMES = FALSE))
}

test_that("C6/C10 : vider une chaine ne doit pas effacer sa PONCTUATION", {
  # RÉGRESSION trouvée en EXÉCUTANT (2026-09-16, comparée avant/après).
  # Une première version de `.strip_code_lines()` retirait AUSSI les guillemets.
  # `stop("message")` devenait donc `stop()`, forme que C10 EXEMPTE
  # explicitement : **63 avertissements C10 réels** avaient disparu —
  # pathway_helpers.R, sc_trajectory.R, spatial_reference.R… — sans qu'aucune
  # ERREUR n'apparaisse. Le garde était devenu aveugle EN SILENCE, ce qui est
  # pire que bruyant.
  # Le garde lit la FORME du code : on vide le contenu, on garde la ponctuation.
  lines <- c(
    "stop(\"message avec 'apostrophes' et un # diese\")",  # 1 SIGNALE
    "stop()",                                              # 2 exempte (stop() nu)
    "stop(\"ok\", call. = FALSE)",                         # 3 exempte (call.=FALSE)
    "stop(\"x\")"                                          # 4 SIGNALE
  )
  path <- .c6_write_fixture(lines)
  on.exit(unlink(dirname(path), recursive = TRUE), add = TRUE)

  expect_identical(.c10_flagged_lines(path), c(1L, 4L))

  # Et la forme du code doit rester `stop("")` — jamais `stop()`.
  code <- .read_code_lines(path)$code
  expect_identical(code[1], 'stop("")')
  expect_identical(code[2], "stop()")
})

test_that("C10 : un appel MULTI-LIGNES est juge sur l'APPEL, pas sur la ligne", {
  # DÉFAUT mesuré le 2026-09-17 : la règle lisait UNE SEULE ligne, donc un
  # `call. = FALSE` posé sur la ligne SUIVANTE était invisible — 49 des 270
  # signalements étaient des faux positifs. Les DEUX directions sont testées :
  # l'exemption doit traverser les lignes, ET l'absence d'exemption aussi.
  lines <- c(
    "stop(\"a\")",                                  #  1 SIGNALE
    "stop(\"b\", call. = FALSE)",                   #  2 EXEMPTE (meme ligne)
    "stop(\"c\",",                                  #  3 SIGNALE  (multi-lignes SANS classement)
    "     \"suite\")",                              #  4
    "stop(\"d\",",                                  #  5 EXEMPTE  (call. = FALSE plus bas)
    "     \"suite\", call. = FALSE)",               #  6
    "stop(errorCondition(\"e\",",                   #  7 EXEMPTE  (errorCondition plus bas)
    "                    class = \"x_error\"))",    #  8
    "stop()"                                        #  9 EXEMPTE  (stop() nu)
  )
  path <- .c6_write_fixture(lines)
  on.exit(unlink(dirname(path), recursive = TRUE), add = TRUE)

  flagged <- .c10_flagged_lines(path)
  expect_identical(flagged, c(1L, 3L),
                   info = paste0("attendu 1 et 3 ; obtenu : ",
                                 paste(flagged, collapse = ",")))
})

# =============================================================================
# DÉFAUT 4 (mesuré le 2026-09-17) : analyser l'étendue du `stop()` ne suffit
# pas. Un site routé par un constructeur local CLASSÉ —
#     .bulk_multi_stop <- function(msg, state) {
#       errorCondition(msg, class = "bulk_multi_error", state = state)
#     }
#     ...
#     stop(.bulk_multi_stop("Label vide.", state = "invalid_label"))
# — est BEL ET BIEN classé, mais ne contient AUCUN token `errorCondition` :
# la garde le déclarait « non classé ». 46 des 221 signalements étaient de ce
# type (18 `.bulk_multi_stop`, 15 `.sc_multi_stop`, 13
# `.bulk_multi_compare_stop`) — et ces trois fichiers étaient exactement ceux
# que la mesure précédente croyait « déjà classés ».
# Après correctif : C10 = 175.
#
# Les trois directions sont testées, car l'exemption est le geste DANGEREUX :
#   A. routé par un constructeur classé → EXEMPTÉ (c'est le correctif) ;
#   B. routé par une fonction quelconque → TOUJOURS SIGNALÉ ;
#   C. routé par un validateur qui CITE errorCondition sans le retourner →
#      TOUJOURS SIGNALÉ (règle stricte : sans C, une exemption large rendrait
#      la garde aveugle — cf. §12.1).
# =============================================================================

test_that("C10 : un stop() route par un constructeur local CLASSE est exempte", {
  lines <- c(
    ".fx_stop <- function(msg, state) {",                        # 1
    "  errorCondition(msg, class = \"fx_error\", state = state)", # 2
    "}",                                                         # 3
    "stop(.fx_stop(\"a\", state = \"x\"))",                      # 4 EXEMPTE
    "stop(.fx_stop(",                                            # 5 EXEMPTE (multi-lignes)
    "  \"b\", state = \"y\"))",                                  # 6
    "stop(\"c\")"                                                # 7 SIGNALE
  )
  path <- .c6_write_fixture(lines)
  on.exit(unlink(dirname(path), recursive = TRUE), add = TRUE)

  flagged <- .c10_flagged_lines(path)
  expect_identical(flagged, 7L,
                   info = paste0("attendu 7 ; obtenu : ",
                                 paste(flagged, collapse = ",")))
})

test_that("C10 : un stop() route par une fonction NON classe reste signale", {
  # B. Le constructeur doit etre CLASSE. Une fonction qui fabrique une chaine
  # n'exempte rien : sans ce test, « appelle une fonction » suffirait.
  lines <- c(
    ".fx_msg <- function(msg) {",     # 1
    "  paste0(\"prefixe: \", msg)",   # 2
    "}",                              # 3
    "stop(.fx_msg(\"a\"))"            # 4 SIGNALE
  )
  path <- .c6_write_fixture(lines)
  on.exit(unlink(dirname(path), recursive = TRUE), add = TRUE)

  expect_identical(.c10_flagged_lines(path), 4L)
})

test_that("C10 : un validateur qui CITE errorCondition sans le retourner reste signale", {
  # C. Regle STRICTE. `.fx_validate` leve une erreur classee PARMI d'autres
  # verifications : `stop(.fx_validate(x))` peut donc recevoir `invisible(TRUE)`
  # — soit une valeur qui n'est PAS une condition. L'exempter serait faux.
  # Mesure : la regle large retenait 61 noms, la stricte 3, pour le MEME
  # verdict sur les 46 sites.
  lines <- c(
    ".fx_validate <- function(x) {",                             # 1
    "  if (is.null(x)) {",                                       # 2
    "    stop(errorCondition(\"vide\", class = \"fx_error\"))",  # 3 EXEMPTE
    "  }",                                                       # 4
    "  invisible(TRUE)",                                         # 5
    "}",                                                         # 6
    "stop(.fx_validate(x))"                                      # 7 SIGNALE
  )
  path <- .c6_write_fixture(lines)
  on.exit(unlink(dirname(path), recursive = TRUE), add = TRUE)

  flagged <- .c10_flagged_lines(path)
  expect_identical(flagged, 7L,
                   info = paste0("attendu 7 ; obtenu : ",
                                 paste(flagged, collapse = ",")))
})

test_that("C10 : les fichiers du depot routes par un constructeur classe sont muets", {
  # Les trois fichiers concernes ne doivent PLUS produire aucun signalement
  # C10. Le constructeur est verifie en PRE-REQUIS : s'il disparait ou cesse
  # d'etre classe, c'est le TEST qu'il faut reecrire — pas laisser passer en
  # silence (c'est exactement ainsi que C6 avait derive, cf. §12.1).
  for (f in c("R/bulk/bulk_multi.R",
              "R/sc/sc_multi.R",
              "R/bulk/bulk_multi_compare.R")) {
    path <- .c6_repo(f)
    expect_true(length(.collect_classed_error_ctors(path)) >= 1L,
                info = sprintf("%s : plus aucun constructeur classe detecte — test a reecrire", f))
    expect_identical(.c10_flagged_lines(path), integer(0),
                     info = sprintf("%s : des stop() sont encore signales", f))
  }
})

test_that("C10 : un constructeur classe est resolu D'UN FICHIER A L'AUTRE", {
  # En production la garde recoit TOUS les fichiers de R/ : un constructeur
  # partage doit donc exempter un site situe ailleurs. On le mesure en
  # passant deux fichiers a la fois (ce que `.c10_flagged_lines` ne fait pas).
  dir <- tempfile("c10x_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  f_ctor <- file.path(dir, "ctors.R")
  f_use <- file.path(dir, "usage.R")
  writeLines(c(
    ".fx2_stop <- function(msg) {",
    "  errorCondition(msg, class = \"fx2_error\")",
    "}"
  ), f_ctor, useBytes = TRUE)
  writeLines(c(
    "stop(.fx2_stop(\"a\"))",   # 1 EXEMPTE (constructeur d'un AUTRE fichier)
    "stop(\"b\")"               # 2 SIGNALE
  ), f_use, useBytes = TRUE)

  .REPORT$warns <- list()
  check_c10_error_style(c(f_ctor, f_use))
  flagged <- sort(vapply(.REPORT$warns,
                         function(w) as.integer(w$line),
                         integer(1), USE.NAMES = FALSE))
  expect_identical(flagged, 2L)
})

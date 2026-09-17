# =============================================================================
# test-conventions-c16-arity.R — cas NÉGATIF de la règle C16
# =============================================================================
# Règle du dépôt : une règle statique doit être éprouvée sur un cas NÉGATIF. Un
# garde qui affiche « 0 erreur » ne prouve rien si on ne l'a jamais vu au rouge.
#
# Défaut RÉEL (commit `c40121f`, corrigé par `ce6691a`) : en convertissant les 19
# `stop()` de R/bulk/bulk_helpers.R vers la forme classée, TROIS sites dont le
# message tenait en PLUSIEURS arguments ont vu leur message **TRONQUÉ à
# l'exécution**. `stop()` **CONCATÈNE** ses arguments ; `errorCondition(message,
# ...)` **NON** — les suivants deviennent des CHAMPS de la condition, pas du
# message :
#
#     stop("A : ", "B", " fin")                    -> "A : B fin"
#     errorCondition("A : ", "B", " fin", class=)   -> "A : "   <-- TRONQUÉ
#
# L'invariant de conversion « texte source identique » ne pouvait PAS le voir :
# le source était inchangé. Seul le message À L'EXÉCUTION l'était — d'où le test
# runtime ajouté dans `test-bulk-helpers.R`, et cette règle STATIQUE pour que le
# défaut ne puisse pas revenir lors de la conversion des 152 sites restants.
#
# Ce fichier vérifie les TROIS directions :
#   1. le défaut est SIGNALÉ (le rouge existe) ;
#   2. les formes légitimes ne sont PAS des faux positifs (paste0 / sprintf /
#      paste, arguments nommés `class=` / `state=` / `message=`) ;
#   3. le dépôt est conforme (0 site) — c'est cette assertion qui échouerait si
#      le défaut revenait.
# =============================================================================

source_project_file("tools/check_conventions.R")

#' Fixture ASCII pure (aucun caractère non-ASCII ne doit atteindre un parse()).
.c16_write_fixture <- function(lines) {
  dir <- tempfile("c16fix_")
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "fixture_c16.R")
  writeLines(lines, path, useBytes = TRUE)
  path
}

#' Rejoue la SEULE règle C16 sur un fichier et rend les lignes signalées.
.c16_flagged_lines <- function(path) {
  .REPORT$warns <- list()
  check_c16_errorcondition_arity(path)
  if (!length(.REPORT$warns)) return(integer(0))
  sort(vapply(.REPORT$warns,
              function(w) as.integer(w$line),
              integer(1), USE.NAMES = FALSE))
}

test_that("C16 : le message multi-arguments est SIGNALE, les formes legitimes non", {
  lines <- c(
    'stop(errorCondition("a", "b", class = "x"))',                     # 1 SIGNALE
    'stop(errorCondition(paste0("a", "b"), class = "x"))',             # 2 legitime
    'stop(errorCondition(msg, state = "invalid_input", class = "x"))', # 3 legitime
    'stop(errorCondition(sprintf("x %s", y), class = "z"))',           # 4 legitime
    'stop(errorCondition("a", "b", "c", class = "x"))',                # 5 SIGNALE
    'stop(errorCondition(paste("a", "b", sep = ":"), class = "x"))',   # 6 legitime
    'stop(errorCondition(message = "a", class = "x"))'                 # 7 legitime
  )
  path <- .c16_write_fixture(lines)
  on.exit(unlink(dirname(path), recursive = TRUE), add = TRUE)

  # La virgule IMBRIQUEE de paste0()/paste() ne doit pas compter comme un
  # argument de plus : c'est tout l'enjeu du découpage de premier niveau.
  expect_identical(.c16_flagged_lines(path), c(1L, 5L))
})

test_that("C16 : un appel multi-LIGNES est juge sur l'APPEL, pas sur la ligne", {
  # Même exigence que C10 : l'unité est l'APPEL. Un message étalé sur plusieurs
  # lignes doit être vu en entier.
  lines <- c(
    'stop(errorCondition("a",',      # 1 SIGNALE (2 positionnels, sur 2 lignes)
    '                    "b",',
    '                    class = "x"))',
    'stop(errorCondition(paste0("a",',  # 4 legitime
    '  "b"), class = "x"))'
  )
  path <- .c16_write_fixture(lines)
  on.exit(unlink(dirname(path), recursive = TRUE), add = TRUE)

  expect_identical(.c16_flagged_lines(path), 1L)
})

test_that("C16 : le depot est conforme (0 site)", {
  # Direction 3 : c'est CETTE assertion qui échouerait si le défaut revenait
  # lors de la conversion des 152 `stop()` restants.
  files <- c(.collect_files(file.path(ts_project_root(), "R")),
             .collect_files(file.path(ts_project_root(), "modules")))
  .REPORT$warns <- list()
  check_c16_errorcondition_arity(files)
  c16 <- Filter(function(w) identical(w$rule, "C16"), .REPORT$warns)
  expect_identical(
    length(c16), 0L,
    info = paste(vapply(c16, function(w) sprintf("%s:%s", .rel(w$file), w$line),
                        character(1)), collapse = ", ")
  )
})

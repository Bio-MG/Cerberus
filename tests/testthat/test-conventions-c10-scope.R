# =============================================================================
# test-conventions-c10-scope.R — portée de la règle C10
# =============================================================================
# Défaut RÉEL (mesuré le 2026-09-17, §2br) : `run_check()` passait **uniquement
# `R/`** à `check_c10_error_style()`, alors que les règles voisines (C5, C7, C11,
# C13 et surtout **C16**, sa règle sœur sur `errorCondition()`) recevaient
# `R/ + modules/`. La garde sous-mesurait donc la dette de **37,5 %** :
#
#     C10 réellement présent : R/ = 100 · modules/ = 48 · tests/ = 12  -> 160
#     C10 mesuré par la garde : 100                     (60 sites invisibles)
#
# L'angle mort n'était pas marginal : il couvrait la couche la PLUS visible de
# l'application — `mod_import_bulk.R` (10), `mod_geo.R` (8), `mod_import_sc.R`
# (6), `mod_sc_pseudobulk.R` (6) — avec des messages que l'utilisateur lit
# réellement (« Package 'GEOquery' requis », « Aucune paire n'a pu être
# calculée »). Pendant ce temps, les 5ᵉ, 6ᵉ et 7ᵉ incréments classaient des
# helpers de tracé au fond de `R/sc/`.
#
# Ce fichier vérifie les TROIS directions :
#   1. la règle SIGNALE un `stop()` nu et EXEMPTE les formes légitimes
#      (`errorCondition`, `call. = FALSE`) ;
#   2. la POPULATION mesurée par la garde couvre `R/` **et** `modules/`
#      (c'est cette assertion qui était au ROUGE avant le correctif) ;
#   3. `tests/` est EXCLU — décision explicite, pas un oubli : les `stop()` de
#      fixtures ne sont pas du code de production.
# =============================================================================

source_project_file("tools/check_conventions.R")

# Fixture ASCII pure (aucun caractère non-ASCII ne doit atteindre un parse()).
.c10_scope_write_fixture <- function(lines) {
  dir <- tempfile("c10scope_")
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "fixture_c10_scope.R")
  writeLines(lines, path, useBytes = TRUE)
  path
}

# Rejoue la SEULE règle C10 sur un fichier et rend les lignes signalées.
.c10_scope_flagged <- function(path) {
  .REPORT$warns <- list()
  check_c10_error_style(path)
  if (!length(.REPORT$warns)) return(integer(0))
  sort(vapply(.REPORT$warns,
              function(w) as.integer(w$line),
              integer(1), USE.NAMES = FALSE))
}

test_that("C10 : un stop() nu est SIGNALE, les formes legitimes sont exemptes", {
  lines <- c(
    'stop("message nu")',                                        # 1 SIGNALE
    'stop(errorCondition("classe", class = "x_error"))',         # 2 exempt
    'stop("explicite", call. = FALSE)',                          # 3 exempt
    'if (bad) stop("conditionnel")',                             # 4 SIGNALE
    'stop(errorCondition(sprintf("m %s", y), class = "x_error"))' # 5 exempt
  )
  path <- .c10_scope_write_fixture(lines)
  on.exit(unlink(dirname(path), recursive = TRUE), add = TRUE)

  expect_identical(.c10_scope_flagged(path), c(1L, 4L))
})

# Rejoue la garde ENTIÈRE et rend les seuls signalements C10.
#
# ⚠️ PIÈGE MESURÉ : `run_check()` collecte ses fichiers avec des chemins
# RELATIFS (`.collect_files("R")`), et `test_dir()` place le répertoire de
# travail dans `tests/testthat`. Appelée telle quelle depuis un test, la garde
# ne trouve donc **0 fichier** et rend 0 signalement — un vert qui ne prouve
# rien (c'est le défaut observé au premier passage : `length(c10) == 0`).
# D'où le changement de répertoire. Les tests C16 n'y sont pas exposés parce
# qu'ils passent des chemins absolus (`file.path(ts_project_root(), "R")`).
.c10_scope_run_guard <- function() {
  old <- setwd(ts_project_root())
  on.exit(setwd(old), add = TRUE)
  .REPORT$errors <- list()
  .REPORT$warns  <- list()
  invisible(capture.output(run_check()))
  Filter(function(w) identical(w$rule, "C10"), .REPORT$warns)
}

test_that("C10 : la population mesuree par la garde couvre R/ ET modules/", {
  # Direction 2 — l'assertion qui était au ROUGE avant le correctif.
  # On rejoue la garde ENTIÈRE (pas seulement la règle) : c'est le CÂBLAGE de
  # `run_check()` qui définissait la portée, pas `check_c10_error_style()`.
  # Rejouer la règle seule sur `c(R, modules)` aurait passé AVANT le correctif
  # et n'aurait donc rien prouvé.
  c10 <- .c10_scope_run_guard()
  rel <- vapply(c10, function(w) .rel(w$file), character(1))

  expect_gt(length(c10), 0L)
  # Aucune population hors R/ et modules/ ne doit apparaître.
  expect_true(all(startsWith(rel, "R/") | startsWith(rel, "modules/")),
              info = paste(head(setdiff(unique(dirname(rel)),
                                        c("R", "modules")), 5), collapse = ", "))
  # Et modules/ doit réellement contribuer : c'est le cœur du défaut.
  expect_true(sum(startsWith(rel, "modules/")) > 0L,
              label = "au moins un site C10 doit venir de modules/",
              info = "modules/ absent de la population C10 : la regle sous-mesure la dette")
})

test_that("C10 : tests/ est EXCLU (decision explicite)", {
  # Direction 3 — `tests/` porte 12 `stop()` nus (fixtures, gardes de setup).
  # Ce n'est pas du code de production : la règle ne les voit pas, et c'est
  # voulu. On l'énonce pour que l'exclusion ne soit pas confondue avec un
  # nouvel angle mort.
  c10 <- .c10_scope_run_guard()
  rel <- vapply(c10, function(w) .rel(w$file), character(1))

  # Contrôle de validité : sans lui, un vert obtenu avec 0 signalement (cf. le
  # piège du répertoire de travail ci-dessus) validerait aussi cette assertion.
  expect_gt(length(c10), 0L)
  expect_false(any(startsWith(rel, "tests/")))
})

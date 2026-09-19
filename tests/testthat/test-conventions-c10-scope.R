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
# Ce fichier vérifie les QUATRE directions :
#   1. la règle SIGNALE un `stop()` nu et EXEMPTE les formes légitimes
#      (`errorCondition`, `call. = FALSE`) ;
#   2. la POPULATION **SCANNÉE** par la garde couvre `R/` **et** `modules/`
#      (c'est cette assertion qui était au ROUGE avant le correctif) ;
#   3. `tests/` est EXCLU — décision explicite, pas un oubli : les `stop()` de
#      fixtures ne sont pas du code de production ;
#   4. la dette C10 vaut **0** — invariant atteint au §2cx (2026-09-19).
#
# ---------------------------------------------------------------------------
# 🔴 POURQUOI LA DIRECTION 2 PORTE SUR LA POPULATION SCANNÉE, PAS SIGNALÉE
# ---------------------------------------------------------------------------
# Le 38e incrément (§2cx) a ramené la dette C10 à **0**. L'assertion d'origine
# — « au moins un signalement vient de `modules/` » — exigeait une population
# SIGNALÉE NON VIDE : elle est donc devenue **infalsifiable** (et, en pratique,
# ROUGE). La remplacer par « il y a 0 signalement » aurait rendu la portée
# **VACUE** : un vert qui ne prouve rien — exactement le piège que ce fichier
# documente déjà plus bas (le répertoire de travail de `test_dir()`).
# On prouve donc la PORTÉE sur la population **SCANNÉE**, qui ne dépend pas du
# nombre de signalements : c'est le CÂBLAGE `check_c10_error_style(c(r_files,
# m_files))` — précisément ce que le défaut §2br avait cassé.
#
# ⚠️ `source_project_file()` charge la garde dans `globalenv()` (`sys.source`),
# donc l'espion doit être installé dans `globalenv()`, PAS dans l'environnement
# du test : un espion posé dans le test serait simplement **IGNORÉ** (le test
# passerait au vert sans rien mesurer).
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

# ---------------------------------------------------------------------------
# ESPION DU CÂBLAGE — quels fichiers la garde PASSE-t-elle à la règle C10 ?
# ---------------------------------------------------------------------------
# On remplace `check_c10_error_style` par un relais qui ENREGISTRE son argument
# avant de déléguer à l'original. On mesure ainsi la population SCANNÉE, qui est
# non vide même quand la dette est nulle.
#
# Rend des chemins **RELATIFS à la racine** (voir l'avertissement ci-dessous).
#
# ⚠️ Installation dans `globalenv()` (cf. l'avertissement en tête de fichier).
# ⚠️ Restauration par `on.exit()` : un espion laissé en place contaminerait les
# fichiers de test suivants — le garde doit rendre la main EXACTEMENT dans
# l'état où il l'a prise.
# ⚠️ `.rel()` est MÉMOÏSÉ et sa racine est indexée PAR RÉPERTOIRE DE TRAVAIL
# (`.root_dir()`, cf. l'avertissement dans la garde). Appelée APRÈS la
# restauration du `setwd()`, elle ne reconnaît plus le préfixe et rend des
# chemins **ABSOLUS** — mesuré : l'assertion de portée est tombée avec
# `dirname(rel)` = « D:/.../R/bulk ». On convertit donc ICI, tant que le
# répertoire courant EST la racine.
.c10_scope_scanned <- function() {
  old_wd <- setwd(ts_project_root())
  on.exit(setwd(old_wd), add = TRUE)

  real <- get("check_c10_error_style", envir = globalenv())
  seen <- character(0)

  spy <- function(r_files) {
    seen <<- c(seen, r_files)
    real(r_files)
  }
  assign("check_c10_error_style", spy, envir = globalenv())
  on.exit(assign("check_c10_error_style", real, envir = globalenv()),
          add = TRUE)

  .REPORT$errors <- list()
  .REPORT$warns  <- list()
  invisible(capture.output(run_check()))

  vapply(seen, .rel, character(1))
}

test_that("C10 : la population SCANNEE couvre R/ ET modules/ (cablage de la garde)", {
  # Direction 2 — l'assertion qui était au ROUGE avant le correctif.
  # On rejoue la garde ENTIÈRE (pas seulement la règle) : c'est le CÂBLAGE de
  # `run_check()` qui définissait la portée, pas `check_c10_error_style()`.
  # Rejouer la règle seule sur `c(R, modules)` aurait passé AVANT le correctif
  # et n'aurait donc rien prouvé.
  rel <- .c10_scope_scanned()

  # Contrôle de VALIDITÉ : si l'espion n'avait pas été installé (ou si le
  # câblage disparaissait), `rel` serait VIDE et les assertions ci-dessous
  # seraient vraies à vide.
  expect_gt(length(rel), 0L)
  # Aucune population hors R/ et modules/ ne doit être SCANNÉE.
  expect_true(all(startsWith(rel, "R/") | startsWith(rel, "modules/")),
              info = paste(head(setdiff(unique(dirname(rel)),
                                        c("R", "modules")), 5), collapse = ", "))
  # Et modules/ doit réellement être SCANNÉ : c'est le cœur du défaut §2br.
  expect_true(any(startsWith(rel, "modules/")),
              label = "au moins un fichier de modules/ doit etre scanne",
              info = "modules/ absent de la population scannee : la regle sous-mesure la dette")
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

test_that("C10 : tests/ est EXCLU (decision explicite)", {
  # Direction 3 — `tests/` porte des `stop()` nus (fixtures, gardes de setup).
  # Ce n'est pas du code de production : la règle ne les voit pas, et c'est
  # voulu. On l'énonce pour que l'exclusion ne soit pas confondue avec un
  # nouvel angle mort.
  rel <- .c10_scope_scanned()

  # Contrôle de validité : sans lui, un vert obtenu avec 0 fichier scanné
  # validerait aussi cette assertion.
  expect_gt(length(rel), 0L)
  expect_false(any(startsWith(rel, "tests/")))
})

test_that("C10 : la dette est RAMENEE A ZERO (invariant du chantier, §2cx)", {
  # Direction 4 — nouveau PLAFOND. La doctrine du projet est explicite : les
  # compteurs d'avertissements sont des **plafonds** qui ne doivent pas
  # augmenter. Le plafond de C10 est désormais **0** : tout `stop()` nu
  # introduit dans `R/` ou `modules/` fera ROUGIR ce test.
  c10 <- .c10_scope_run_guard()
  expect_length(c10, 0L)
})

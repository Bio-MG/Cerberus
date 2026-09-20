# =============================================================================
# test-conventions-c16-arity.R — cas NÉGATIF de la règle C16, et invariant §14.3
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
# Ce fichier vérifie les SEPT directions :
#   1. le défaut est SIGNALÉ (le rouge existe) ;
#   2. les formes légitimes ne sont PAS des faux positifs (paste0 / sprintf /
#      paste, arguments nommés `class=` / `state=` / `message=`) ;
#   3. le dépôt est conforme (0 site) — c'est cette assertion qui échouerait si
#      le défaut revenait ;
#   4. le signalement atterrit dans le canal **ERREUR** (`.REPORT$errors`) ;
#   5. **TÉMOIN DE CANAL** — une règle qui reste une DETTE (C9) doit, elle,
#      atterrir dans `$warns` : sans ce témoin, la direction 4 passerait aussi
#      sur un garde qui bloque sur n'importe quoi ;
#   6. donc la garde **BLOQUE sans `--strict`** (invariant §14.3), mesuré sur une
#      violation RÉELLE injectée dans `R/` ;
#   7. la table AFFICHÉE ne contredit pas le canal (anti-« mensonge cosmétique »).
#
# ---------------------------------------------------------------------------
# 🔴 §14.3 — POURQUOI LE CORRECTIF « ÉVIDENT » ÉTAIT INERTE (40ᵉ incrément)
# ---------------------------------------------------------------------------
# La décision §14.3 s'énonce « `C16` passe en `ERREUR` », et le code le plus
# proche de cette phrase est la table de niveaux :
#
#     lvl[c("C6", "C8", "C9", "C10", "C11", "C12", "C16")] <- "AVERT."
#
# ⇒ En retirer `"C16"` **ne change RIEN au blocage**. Mesure du modèle de
# sévérité — `blocking <- n_err + (if (strict) n_warn else 0L)` : ce qui décide
# est le **CANAL** où `.add()` dépose le signalement. `lvl` n'est lu QUE pour
# l'affichage (`sprintf("%-5s %-8s ...", r, lvl[[r]], desc[[r]])`). Corriger la
# table seule produirait un **mensonge cosmétique** : elle annoncerait `ERREUR`
# pendant que la garde continuerait de ne pas bloquer.
# ⇒ Le correctif réel est au **SOURCE de la sévérité** : `.add("ERROR", "C16",
# ...)`. Les directions 4 et 7 verrouillent la **PAIRE** (canal **et** affichage) :
# n'en corriger qu'une des deux fait rougir ce fichier.
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
#'
#' ⚠️ Depuis le 40ᵉ incrément (§14.3), C16 dépose dans `.REPORT$errors`. Lire
#' `$warns` rendrait **0** et le test deviendrait **vrai à vide** — c'est
#' exactement ce qui est arrivé à ce fichier lors du passage au nouveau contrat.
.c16_flagged_lines <- function(path) {
  .REPORT$errors <- list()
  .REPORT$warns  <- list()
  check_c16_errorcondition_arity(path)
  if (!length(.REPORT$errors)) return(integer(0))
  sort(vapply(.REPORT$errors,
              function(w) as.integer(w$line),
              integer(1), USE.NAMES = FALSE))
}

# ---------------------------------------------------------------------------
# 🔴 §2df.10 — NETTOYAGE D'ENTREE (au niveau du FICHIER, pas d'un bloc)
# ---------------------------------------------------------------------------
# Le bloc « C16 est BLOQUANT sans --strict » ÉCRIT une violation dans `R/`
# (obligatoire : la garde ne scanne que `R/`) et la retire par `on.exit()`.
# Or `on.exit()` ne s'exécute PAS sur SIGTERM/SIGSEGV : une suite INTERROMPUE
# laisse `R/zzz_probe_c16_tmp.R` sur le disque.
#
# Conséquence MESURÉE le 2026-09-20 : ce résidu a (a) fait rougir
# `test-app-sourcing.R` — le garde P0 qui exige que `app.R` source tout fichier
# de `R/` — et (b) fait rougir DEUX assertions de CE fichier, celles qui
# exigent un dépôt conforme (`.c16_flagged_lines` sur `R/`) et un statut de
# garde à 0. Le tout dans une session qui n'avait rien cassé.
#
# ⚠️ Un `unlink()` placé DANS le bloc concerné ne suffit PAS : testthat évalue
# les `test_that` dans l'ordre du fichier, et les assertions qui mesurent le
# dépôt (lignes ~124 et ~197) s'exécutent AVANT le bloc qui écrit (~205). Le
# nettoyage doit donc être au niveau du FICHIER, avant tout `test_that`.
#
# ⚠️ Ce n'est PAS un pansement : la sentinelle ci-dessous est elle-même
# testée (bloc dédié plus bas), pour que le mode de défaillance reste visible.
.c16_probe_path <- function() {
  file.path(ts_project_root(), "R", "zzz_probe_c16_tmp.R")
}
unlink(.c16_probe_path())     # résidu d'une exécution précédente TUÉE

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
  .REPORT$errors <- list()
  .REPORT$warns  <- list()
  check_c16_errorcondition_arity(files)
  c16 <- Filter(function(w) identical(w$rule, "C16"), .REPORT$errors)
  expect_identical(
    length(c16), 0L,
    info = paste(vapply(c16, function(w) sprintf("%s:%s", .rel(w$file), w$line),
                        character(1)), collapse = ", ")
  )
})

# ---------------------------------------------------------------------------
# CANAL — c'est LUI, et non la table d'affichage, qui décide du blocage
# ---------------------------------------------------------------------------
# `.add(severity, ...)` range dans `.REPORT$errors` SI ET SEULEMENT SI la
# sévérité vaut exactement `"ERROR"` ; tout le reste va dans `.REPORT$warns`.
# Et `run_check()` calcule `blocking <- n_err + (if (strict) n_warn else 0L)`.
# ⇒ La sévérité EFFECTIVE d'une règle est son canal, pas son libellé.
.c16_channel <- function(path) {
  .REPORT$errors <- list()
  .REPORT$warns  <- list()
  check_c16_errorcondition_arity(path)
  if (length(.REPORT$errors)) return("errors")
  if (length(.REPORT$warns))  return("warns")
  "aucun"
}

test_that("C16 : le signalement atterrit dans le canal ERREUR (donc bloquant)", {
  path <- .c16_write_fixture('stop(errorCondition("a", "b", class = "x"))')
  on.exit(unlink(dirname(path), recursive = TRUE), add = TRUE)

  expect_identical(.c16_channel(path), "errors")
})

test_that("C16 : TEMOIN DE CANAL — une regle de DETTE reste dans les avertissements", {
  # ⚠️ Le témoin est CONSTRUIT (chemin bidon dont aucun test éponyme n'existe),
  # jamais lu sur la population du dépôt : une assertion exigeant une population
  # signalée NON VIDE devient **infalsifiable** le jour où la dette tombe à 0 —
  # piège payé au §2cx sur `test-conventions-c10-scope.R`.
  # `check_c9_r_tests()` ne teste PAS l'existence du fichier : elle ne regarde
  # que les noms de tests candidats.
  old <- setwd(ts_project_root())
  on.exit(setwd(old), add = TRUE)

  path <- file.path(ts_project_root(), "R", "core", "zzz_aucun_test_tmp.R")
  .REPORT$errors <- list()
  .REPORT$warns  <- list()
  check_c9_r_tests(path)

  # Le témoin doit avoir TIRÉ (sinon la direction 4 ne prouve rien)...
  expect_identical(length(.REPORT$warns), 1L)
  # ... et il doit avoir tiré dans l'AUTRE canal.
  expect_length(.REPORT$errors, 0L)
})

# ---------------------------------------------------------------------------
# BOUT EN BOUT — la garde BLOQUE-t-elle vraiment, sans `--strict` ?
# ---------------------------------------------------------------------------
# ⚠️ PIÈGE MESURÉ (documenté dans `test-conventions-c10-scope.R`) : `run_check()`
# collecte ses fichiers avec des chemins RELATIFS (`.collect_files("R")`) et
# `test_dir()` place le répertoire de travail dans `tests/testthat`. Sans le
# `setwd()`, la garde ne trouve **0 fichier** et rend un vert qui ne prouve rien.
.c16_guard_once <- function() {
  old <- setwd(ts_project_root())
  on.exit(setwd(old), add = TRUE)
  .REPORT$errors <- list()
  .REPORT$warns  <- list()
  st  <- NA_integer_
  out <- capture.output(st <- run_check(strict = FALSE, use_git = FALSE))
  ln  <- grep("^C16[[:space:]]", out, value = TRUE)
  list(status = st,
       level  = if (length(ln)) strsplit(trimws(ln[[1]]), "[[:space:]]+")[[1]][[2]] else NA_character_)
}

test_that("C16 est BLOQUANT sans --strict (invariant §14.3)", {
  # TÉMOIN : sans violation, la garde NE bloque PAS (le dépôt est à 0 erreur).
  # Sans ce témoin, un `1L` obtenu pour une tout autre raison passerait.
  expect_identical(.c16_guard_once()$status, 0L)

  # Violation RÉELLE, injectée dans `R/`. `.collect_files()` n'est PAS mémoïsé
  # (mesuré) ⇒ le fichier est bien vu par la garde.
  # ⚠️ Ce fichier déclenche AUSSI C9 et C12 — des AVERTISSEMENTS, non bloquants.
  #    C'est précisément l'objet de la mesure : seul le canal ERREUR décide.
  #
  # 🔴 §2df.10 — CE TEST ÉCRIT DANS L'ARBRE, ET SON NETTOYAGE NE SURVIT PAS À UN
  #    `kill`. `on.exit()` ne s'exécute PAS sur SIGTERM/SIGSEGV : une suite
  #    interrompue LAISSE `R/zzz_probe_c16_tmp.R` sur le disque. Conséquence
  #    MESURÉE le 2026-09-20 (20:30) : `test-app-sourcing.R` — le garde P0 qui
  #    exige que `app.R` source tout fichier de `R/` — est devenu ROUGE
  #    (`fail=1`) à cause de ce résidu, dans une session qui n'avait rien cassé.
  #    ⇒ L'IDEMPOTENCE NE SUFFIT PAS ICI : on NETTOIE AVANT d'écrire, pour que
  #    le résidu d'une exécution tuée soit réparé par la suivante. Le test reste
  #    ce qu'il était (il doit écrire dans `R/` pour que la garde le voie) ;
  #    c'est le MODE DE DÉFAILLANCE qui change : de « l'arbre reste sale » à
  #    « le prochain passage répare ».
  probe <- .c16_probe_path()
  unlink(probe)                       # résidu d'une exécution précédente TUÉE
  writeLines('f <- function() stop(errorCondition("a", "b", class = "x"))',
             probe, useBytes = TRUE)
  on.exit(unlink(probe), add = TRUE)

  expect_identical(.c16_guard_once()$status, 1L)
})

test_that("C16 : le nettoyage d'ENTREE existe et est au niveau du FICHIER (§2df.10)", {
  # Ce test verrouille le MODE DE DÉFAILLANCE, pas la règle.
  #
  # Le bloc « BLOQUANT sans --strict » ÉCRIT dans `R/` (obligatoire : la garde
  # ne scanne que `R/`) et retire sa trace par `on.exit()`. Or `on.exit()` ne
  # s'exécute PAS sur SIGTERM/SIGSEGV ⇒ une suite INTERROMPUE laisse
  # `R/zzz_probe_c16_tmp.R` sur le disque. MESURÉ le 2026-09-20 : ce résidu a
  # (a) fait rougir `test-app-sourcing.R` (garde P0 : `app.R` doit sourcer tout
  # fichier de `R/`) et (b) fait rougir les DEUX assertions de CE fichier qui
  # mesurent le dépôt. Dans une session qui n'avait rien cassé.
  #
  # ⚠️ La leçon de conception : un `unlink()` DANS le bloc qui écrit est
  # INSUFFISANT, car testthat évalue les `test_that` dans l'ordre du fichier et
  # les assertions « dépôt conforme » (~l. 124 et ~197) passent AVANT le bloc
  # qui écrit (~l. 205). Le nettoyage doit donc vivre au niveau du FICHIER.
  # C'est ce que ce test protège : si quelqu'un déplace le `unlink()` dans un
  # bloc, il casse ici — AVANT de casser `test-app-sourcing.R`.

  # (1) Le helper existe (le déplacer/supprimer doit faire échouer ici).
  expect_true(exists(".c16_probe_path", mode = "function"))
  expect_identical(.c16_probe_path(),
                   file.path(ts_project_root(), "R", "zzz_probe_c16_tmp.R"))

  # (2) L'APPEL de nettoyage est bien AU NIVEAU DU FICHIER, c.-à-d. HORS de tout
  #     `test_that`. On le lit dans la source : le `unlink(.c16_probe_path())`
  #     d'entrée doit apparaître AVANT la première ligne `test_that(`.
  #     ⚠️ Lecture de SOURCE, donc on retire les commentaires d'abord (sinon on
  #     compterait les mentions du commentaire d'explication ci-dessus).
  src <- readLines("test-conventions-c16-arity.R", warn = FALSE)
  code <- sub("#.*$", "", src)
  first_test <- min(grep("^\\s*test_that\\(", code))
  clean_line <- grep("^\\s*unlink\\(\\.c16_probe_path\\(\\)\\)", code)
  expect_true(length(first_test) == 1L && is.finite(first_test))
  expect_true(length(clean_line) >= 1L,
              info = "le nettoyage d'entree doit exister")
  expect_true(min(clean_line) < first_test,
              info = paste0("le nettoyage d'entree (l.", min(clean_line),
                            ") doit preceder le 1er test_that (l.", first_test,
                            ") : sinon les assertions qui mesurent le depot",
                            " tournent AVANT lui et le residu les fait rougir."))

  # (3) TÉMOIN DE NON-VACUITÉ : la garde voit bien ce chemin. Sans cela, tout ce
  #     qui précède passerait même si `R/` n'était plus scanné.
  probe <- .c16_probe_path()
  writeLines('f <- function() stop(errorCondition("a", "b", class = "x"))',
             probe, useBytes = TRUE)
  on.exit(unlink(probe), add = TRUE)
  expect_identical(.c16_guard_once()$status, 1L)
})

test_that("C16 : la table AFFICHEE ne contredit pas le canal (anti-mensonge cosmetique)", {
  # La table lit `lvl`, le blocage lit le canal. Corriger l'un SANS l'autre
  # donne soit un mensonge cosmétique (affiche ERREUR, ne bloque pas), soit un
  # blocage silencieux (bloque, affiche AVERT.). Les directions 4/5 et 7
  # verrouillent la PAIRE.
  expect_identical(.c16_guard_once()$level, "ERREUR")
})

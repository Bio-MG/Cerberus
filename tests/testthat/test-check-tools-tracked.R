# =============================================================================
# test-check-tools-tracked.R — contrat de tools/check_tools_tracked.R (GARDE 7)
# =============================================================================
# POURQUOI CE FICHIER EXISTE (mesure 2026-10-24)
# ----------------------------------------------
# Six outils du lot 6-7 ont disparu sans qu'aucune suite ne rougisse. La cause
# n'etait pas un bug : c'etait l'ABSENCE de question. Ce garde pose la question
# « un fichier de tools/ est-il suivi par git, ou declare ignore ? », et ce
# fichier de test verrouille la reponse.
#
# 🔴 LA PROPRIETE CENTRALE : un fichier NI suivi NI ignore est une ERREUR, un
# fichier IGNORE est un CHOIX. Les confondre ferait rougir le garde pour de
# l'etat d'execution legitime (`tools/_drive/*.json`) — et un garde qui rougit
# pour rien ne se lit plus, donc ne garde plus rien.
#
# La classification est une FONCTION PURE : ces tests n'ont besoin ni de git ni
# d'un depot, donc ils ne peuvent pas echouer pour une raison etrangere a leur
# sujet (lecon mesuree : un test qui depend d'un paquet absent devient MUET).
# =============================================================================

source_project_file("tools/check_tools_tracked.R")

# =============================================================================
# 1. `ts_tools_ext()` — la brique
# =============================================================================

test_that("l'extension est lue en minuscules, et vide s'il n'y en a pas", {
  expect_identical(ts_tools_ext("a/b/outil.R"), "r")
  expect_identical(ts_tools_ext("a/b/Outil.R"), "r")
  expect_identical(ts_tools_ext("script.PS1"), "ps1")
  expect_identical(ts_tools_ext("README"), "")
  expect_identical(ts_tools_ext("dossier.sans.extension/README"), "")
  # Un point en tete n'est pas une extension (fichier cache).
  expect_identical(ts_tools_ext(".gitignore"), "gitignore")
})

# =============================================================================
# 2. `ts_tools_classify()` — la decision, sans git
# =============================================================================

test_that("un fichier ni suivi ni ignore est une ERREUR (mort silencieuse)", {
  # C'est le cas exact des six outils perdus : presents, jamais commites,
  # invisibles a git.
  r <- ts_tools_classify(untracked = "tools/run_all_gates.R")
  expect_length(r$errors, 1L)
  expect_true(grepl("run_all_gates.R", r$errors[1], fixed = TRUE))
  expect_true(grepl("non suivi", r$errors[1], fixed = TRUE))
  expect_length(r$warnings, 0L)
})

test_that("un fichier suivi mais absent du disque est une ERREUR (l'index ment)", {
  r <- ts_tools_classify(deleted = "tools/check_swarm.R")
  expect_length(r$errors, 1L)
  expect_true(grepl("absent du disque", r$errors[1], fixed = TRUE))
})

test_that("un fichier ignore n'est PAS une erreur — c'est un choix ecrit", {
  # `tools/_drive/*.json` est de l'etat d'execution, declare ignore. Le garde
  # doit le laisser passer, sinon il rougit en permanence et devient du bruit.
  r <- ts_tools_classify(ignored = c("tools/_drive/arm.json",
                                     "tools/_drive/ready.json"))
  expect_length(r$errors, 0L)
  expect_length(r$warnings, 0L)
})

test_that("un fichier ignore qui est du CODE est un AVERTISSEMENT, pas un silence", {
  # Choisir de ne pas versionner un SCRIPT est un choix qui merite d'etre vu :
  # l'outil existe, mais il ne survivra pas a un clone.
  r <- ts_tools_classify(ignored = "tools/local_probe.R")
  expect_length(r$errors, 0L)
  expect_length(r$warnings, 1L)
  expect_true(grepl("code", r$warnings[1], fixed = TRUE))
})

test_that("les avertissements ne se melangent pas aux erreurs", {
  r <- ts_tools_classify(untracked = "tools/neuf.R",
                         ignored = c("tools/_drive/arm.json",
                                     "tools/script_ignore.R"))
  expect_length(r$errors, 1L)
  expect_length(r$warnings, 1L)
  # Le .json ignore ne doit apparaitre NI en erreur NI en avertissement.
  expect_false(any(grepl("arm.json", c(r$errors, r$warnings), fixed = TRUE)))
})

test_that("un repertoire propre ne produit rien — et surtout pas un faux zero trompeur", {
  r <- ts_tools_classify()
  expect_identical(r$errors, character(0))
  expect_identical(r$warnings, character(0))
})

test_that("les listes sont triees et dedoublees", {
  # Deux runs doivent produire la meme sortie : sans tri, l'ordre depend de
  # l'ordre de `git ls-files`, qui n'est pas un contrat.
  r <- ts_tools_classify(untracked = c("tools/b.R", "tools/a.R", "tools/b.R"))
  expect_length(r$errors, 2L)
  expect_true(grepl("a.R", r$errors[1], fixed = TRUE))
  expect_true(grepl("b.R", r$errors[2], fixed = TRUE))
})

# =============================================================================
# 3. Arguments
# =============================================================================

test_that("les valeurs par defaut visent tools/", {
  opt <- ts_tools_args(character(0))
  expect_identical(opt$dir, "tools")
  expect_false(opt$quiet)
  expect_null(opt$root)
})

test_that("--dir= et --quiet se lisent, le reste est une erreur d'usage", {
  expect_identical(ts_tools_args("--dir=R")$dir, "R")
  expect_true(ts_tools_args("--quiet")$quiet)
  expect_error(ts_tools_args("--dir="), "usage")
  expect_error(ts_tools_args("--inconnu"), "inconnu")
})

# =============================================================================
# 4. Integration sur le VRAI depot
# =============================================================================

test_that("le garde s'execute sur le vrai depot et rend un verdict coherent", {
  root <- ts_project_root()
  scan <- ts_tools_scan(root, "tools")
  expect_false(isTRUE(scan$failed))
  # Le scan doit DISTINGUER suivi de non suivi : `check_conventions.R` est
  # commite, il ne doit donc jamais apparaitre comme non suivi. Sans cette
  # assertion, un `git ls-files` casse rendrait deux listes vides et le garde
  # paraitrait « propre » par cecite.
  expect_false(any(grepl("tools/check_conventions.R$", scan$untracked)),
               info = "un fichier suivi est rapporte comme non suivi")
  expect_false(any(grepl("tools/check_conventions.R$", scan$deleted)),
               info = "un fichier present est rapporte comme absent")
  cls <- ts_tools_classify(scan$untracked, scan$deleted, scan$ignored)
  # Invariant : la classification ne rend jamais une liste NA.
  expect_type(cls$errors, "character")
  expect_type(cls$warnings, "character")
})

test_that("le garde attrape un fichier non suivi dans un FAUX depot", {
  # Test de bout en bout sur un depot jetable : c'est le seul moyen de prouver
  # que le cablage git -> classification fonctionne, sans dependre de l'etat
  # (mouvant) du vrai arbre.
  skip_if_not(nzchar(Sys.which("git")), "git introuvable")
  d <- tempfile("ts_git_")
  dir.create(file.path(d, "tools"), recursive = TRUE, showWarnings = FALSE)
  writeLines("x <- 1L", file.path(d, "tools", "suivi.R"))
  run <- function(...) {
    system2("git", c("-C", shQuote(d), ...), stdout = TRUE, stderr = TRUE)
  }
  run("init", "-q")
  run("config", "user.email", "t@t.t")
  run("config", "user.name", "t")
  run("add", "tools/suivi.R")
  run("commit", "-q", "-m", "init")
  # Un fichier neuf, non commite, non ignore : le garde doit le nommer.
  writeLines("y <- 2L", file.path(d, "tools", "orphelin.R"))
  scan <- ts_tools_scan(d, "tools")
  expect_false(isTRUE(scan$failed))
  expect_true(any(grepl("orphelin.R", scan$untracked, fixed = TRUE)),
              info = paste("untracked =", paste(scan$untracked, collapse = ", ")))
  cls <- ts_tools_classify(scan$untracked, scan$deleted, scan$ignored)
  expect_length(cls$errors, 1L)
  expect_true(grepl("orphelin.R", cls$errors[1], fixed = TRUE))
  # Et un fichier DECLARE ignore ne doit pas etre une erreur.
  writeLines("*.json", file.path(d, ".gitignore"))
  writeLines("{}", file.path(d, "tools", "etat.json"))
  scan2 <- ts_tools_scan(d, "tools")
  cls2 <- ts_tools_classify(scan2$untracked, scan2$deleted, scan2$ignored)
  expect_length(cls2$errors, 1L)          # toujours orphelin.R, pas etat.json
  expect_false(any(grepl("etat.json", cls2$errors, fixed = TRUE)))
})

# =============================================================================
# test-conventions-c17-state-access.R — règle C17 : accesseurs d'état dans R/
# =============================================================================
# Jalon M-3 (arbitrage du 2026-09-30, option B retenue :
# docs/proposals/STATE_ACCESS_ARBITRATION.md). La règle AGENTS.md 4 — « access
# through accessors (state_get()/state_set()) » — était écrite mais SANS garde :
# l'audit externe a mesuré ~9 % d'adoption et l'arbitrage a re-mesuré 36 accès
# directs EN CODE dans la couche pure R/ (7 `global_data$` + 29 `shared_rv$`),
# contre ~971 dans modules/ où `$` est l'idiome Shiny canonique (hors périmètre
# de la règle, décision assumée et chiffrée).
#
# C17 (ERREUR) : dans `R/` (hors couche d'état .STATE_LAYER), un accès direct
# `global_data$champ` / `shared_rv$champ` en CODE est signalé ; utiliser
# `state_get(state, "champ")` / `state_set(state, "champ", valeur)`.
#
# Quatre directions éprouvées (règle de la maison : un garde au vert ne prouve
# rien s'il n'a jamais été vu au rouge) :
#   1. le rouge existe (sonde `global_data$` ET `shared_rv$` bien signalées) ;
#   2. la couche d'état (.STATE_LAYER) n'est PAS signalée ;
#   3. les COMMENTAIRES ne sont pas signalés (le code, pas la prose) ;
#   4. la population réelle `R/` est PROPRE (0 signalement, après le jalon).
# =============================================================================
source_project_file("tools/check_conventions.R")

#' Rejoue la SEULE règle C17 sur une liste de fichiers, rend les (chemin:ligne)
#' SIGNALES — une entrée par ERREUR, pas par fichier.
.c17_flagged <- function(files) {
  old <- setwd(ts_project_root())
  on.exit(setwd(old), add = TRUE)
  .REPORT$errors <- list()
  .REPORT$warns  <- list()
  check_c17_state_accessors(files)
  vapply(.REPORT$errors, function(e)
    sprintf("%s:%s", .rel(e$file), e$line), character(1))
}

#' Écrit une sonde dans R/core/ et rend SON CHEMIN — le nettoyage appartient à
#' l'appelant (on.exit d'un helper intermédiaire supprimerait le fichier avant
#' sa lecture), et chaque sonde porte un NOM DISTINCT : `.read_code_lines()`
#' mémoïse par chemin (.code_cache) — réutiliser le même nom lirait le contenu
#' mis en cache par le test précédent (piège payé au premier jet de ce test).
.c17_probe_write <- function(body, name = "c17_probe_tmp.R") {
  probe <- file.path(ts_project_root(), "R", "core", name)
  writeLines(body, probe, useBytes = TRUE)
  probe
}

test_that("C17 signale les accès directs aux DEUX conteneurs dans R/", {
  probe <- .c17_probe_write(c(
    "probe_c17 <- function(global_data, shared_rv) {",
    "  a <- global_data$sc_obj",
    "  b <- shared_rv$markers_data",
    "  list(a, b)"
  ), name = "c17_probe_tmp1.R")
  on.exit(unlink(probe), add = TRUE)
  flagged <- .c17_flagged(probe)
  expect_equal(length(flagged), 2L,
               info = sprintf("signale : %s", paste(flagged, collapse = ", ")))
  old <- setwd(ts_project_root())
  on.exit(setwd(old), add = TRUE)
  .REPORT$errors <- list(); .REPORT$warns <- list()
  check_c17_state_accessors(probe)
  expect_length(.REPORT$errors, 2L)
  expect_true(all(vapply(.REPORT$errors, function(e) identical(e$rule, "C17"), logical(1))))
})

test_that("C17 ignore la couche d'état (.STATE_LAYER) et les commentaires", {
  expect_length(.c17_flagged(file.path(ts_project_root(), "R", "core", "state.R")), 0L)
  probe <- .c17_probe_write(c(
    "# dans un commentaire : global_data$sc_obj et shared_rv$markers_data",
    "probe_c17_ok <- function(global_data) state_get(global_data, \"sc_obj\")"
  ), name = "c17_probe_tmp2.R")
  on.exit(unlink(probe), add = TRUE)
  flagged <- .c17_flagged(probe)
  expect_equal(length(flagged), 0L,
               info = sprintf("signale (attendu vide) : %s", paste(flagged, collapse = ", ")))
})

test_that("la population réelle R/ est PROPRE (0 accès direct en code)", {
  r_files <- list.files(file.path(ts_project_root(), "R"),
                        pattern = "[.]R$", recursive = TRUE, full.names = TRUE)
  flagged <- .c17_flagged(r_files)
  expect_equal(length(flagged), 0L,
               info = sprintf("sites C17 restants : %s",
                              paste(unique(flagged), collapse = ", ")))
})

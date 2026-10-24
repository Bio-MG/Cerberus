# =============================================================================
# test-search-path-poisoning.R — un test ne peut pas attacher jsonlite au
# search path (l'observation intermittente du 2026-10-23/24 est DIAGNOSTIÉE).
# =============================================================================
# LE FAIT MESURÉ (session item A, 2026-10-24). Trois bilans de suite
# (2026-10-23) puis 4 reproductions armées de suite (r1..r4, préfixe-124,
# fichier-par-fichier, un seul processus) échouaient TOUJOURS sur le MÊME
# point : test-mod-spatial-qc-tabs.R L2, `output$metadata_table` lève
# « is.character(txt) is not TRUE ».
#
# LA CHAÎNE EXACTE (prouvée par capture au site du throw + reproduction
# minimale M1) :
#   1. tests/testthat/test-drive-n1-injection-reason.R:23 exécutait
#      `suppressPackageStartupMessages(library(jsonlite))` AU NIVEAU FICHIER
#      — alors que le fichier n'appelle JAMAIS jsonlite directement (il
#      passe par les fonctions du drive, qui qualifient `jsonlite::`).
#   2. Les modules sont sourcés dans globalenv (helper-source.R), donc leur
#      idiome `validate(need(cond, msg))` résout `validate` via le search
#      path AU MOMENT DU RENDU.
#   3. shiny était attaché dès la première session testServer
#      (`shiny:::testServer` commence par `require(shiny)` — fichier ~#54 de
#      l'ordre alphabétique) ; jsonlite attaché ENSUITE (fichier ~#69) prend
#      la position 2 et DEVANCE shiny pour le reste du processus.
#   4. Dans le rendu de metadata_table, `need()` rend NULL quand la condition
#      est satisfaite : `jsonlite::validate(NULL)` → stopifnot → le message
#      historique, stocké dans la promesse de l'output, relancé à la lecture,
#      attrapé par le tryCatch du test L2.
#
# POURQUOI « INTERMITTENT » : l'issue dépend de l'ordre RELATIF des attaches.
# Si jsonlite est attaché AVANT le premier attach de shiny (p. ex. un harnais
# de traçage qui charge jsonlite en premier), shiny repasse devant et le test
# PASSE — c'est exactement ce qui a masqué la panne pendant la baseline
# tracée du 2026-10-24 : l'instrument de mesure réparait la course qu'il
# cherchait.
#
# LE GARDE : aucun fichier de tests/ n'attache jsonlite. Ceux qui veulent
# jsonlite qualifient `jsonlite::` (style du dépôt). Le motif est CONSTRUIT
# par paste0 pour que ce fichier ne contienne jamais la chaîne littérale
# qu'il interdit (auto-détection interdite).
# =============================================================================

.ts_spp_forbidden <- function() {
  paste0(
    "(^|[^[:alnum:]_.])(library|require)\\(\\s*",
    "json", "lite", "\\s*\\)"
  )
}

test_that("aucun fichier de tests/ n'attache jsonlite au search path (ombre sur shiny::validate)", {
  tdir <- file.path(ts_project_root(), "tests", "testthat")
  files <- sort(list.files(tdir, pattern = "^[a-zA-Z0-9._-]+\\.[Rr]$", full.names = TRUE))
  # Auto-exclusion : CE fichier cite l'appel interdit dans sa prose et dans
  # son test d'anti-vacuité (qui prouve que le scanner attrape le motif) ;
  # l'auto-détection n'apporterait rien.
  files <- files[basename(files) != "test-search-path-poisoning.R"]
  expect_true(length(files) > 0L, label = "le scanner doit trouver les fichiers de test")

  motif <- .ts_spp_forbidden()
  contrevenants <- character(0)
  for (f in files) {
    lignes <- readLines(f, warn = FALSE)
    hits <- grep(motif, lignes)
    if (length(hits) > 0L) {
      contrevenants <- c(contrevenants,
                         sprintf("%s:%d", basename(f), hits))
    }
  }
  if (length(contrevenants) == 0L) {
    succeed()
  } else {
    fail(paste0(
      "attach jsonlite détecté (l'ombre portée sur shiny::validate fait ",
      "échouer tout testServer ultérieur qui rend un output via l'idiome ",
      "validate(need(...)) — voir l'en-tête de ce fichier) : ",
      paste(contrevenants, collapse = ", ")))
  }
})

test_that("le scanner détecte bien une attache (anti-vacuité, sur un faux fichier)", {
  faux <- file.path(tempdir(), "ts-spp-faux-test.R")
  writeLines(c(
    "# faux",
    paste0("sup", "pressPackageStartupMessages(library(json", "lite))"),
    "x <- 1"
  ), faux)
  on.exit(unlink(faux), add = TRUE)
  expect_grepl <- grepl(.ts_spp_forbidden(), readLines(faux, warn = FALSE))
  expect_true(any(expect_grepl),
              info = "le motif doit attraper la forme wrappée suppressPackageStartupMessages(library(jsonlite))")
})

test_that("le scanner ignore les usages QUALIFIÉS (aucun faux positif)", {
  lignes <- c("x <- jsonlite::fromJSON(p)", "y <- jsonlite::validate(t)",
              "z <- jsonlite::toJSON(v)")
  expect_false(any(grepl(.ts_spp_forbidden(), lignes)),
               info = "les appels qualifiés jsonlite::* sont le style autorisé")
})

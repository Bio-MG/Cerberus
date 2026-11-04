# =============================================================================
# test-mod-tour.R — N4 cerberus-tour (satellites slice S6) : module UI/serveur
# =============================================================================
# FICHE (fait foi) : PROPOSAL_N_SATELLITES_LAUNCH_ROADMAP_2026-10-26.md §slice
# S6 — skippable, first-run (localStorage), FR via i18n ; le registre PUR est
# RED→GREEN verrouillé par test-tour-registry.R (le RED de ce slice porte sur
# le helper pur, fiche §S6.3). Ce fichier verrouille le MODULE :
#   (a/b) .tour_modal() PURE — verrous STRUCTURELS (leçon CQF-2, bug rendu en
#         attribut du jalon S2) : ids des boutons, PAS de bouton Précédent au
#         1er step (jamais un contrôle inerte), « Terminer » au dernier,
#         compteur .t_fmt, PAS de data-dismiss (easyClose=FALSE — tout chemin
#         de sortie est un bouton explicite) ;
#   (c)   testServer — machine à états capturée par shadowing d'INSTANCE R6
#         (sendModal / sendInputMessage / sendCustomMessage — jamais la
#         classe) : start → 1, next → 2 + nav main_nav sur le tab GELÉ du
#         registre, prev, skip → 0 + removeModal + pose du drapeau (message
#         tour-mark_done), first_run démarre SEULEMENT si inactif ;
#   (d)   UI : bouton ns("start") + le script embarque la clé localStorage et
#         les noms interpolés (jamais codés en dur).
# =============================================================================

source_project_file("tools/pkg_skeleton/R/drive_allowlist.R") # tabs GELÉS
source_project_file("tools/pkg_skeleton/R/tour_registry.R")
source_project_file("modules/session/mod_tour.R")

# i18n : global.R fournit le vrai Translator à l'exécution ; repli no-op
# identique à test-asr3-ui.R (les clés SONT les chaînes françaises).
if (!exists("i18n", envir = globalenv(), inherits = FALSE)) {
  assign("i18n", list(
    t                        = function(key, ...) key,
    set_translation_language = function(language) invisible(NULL),
    get_translation_language = function() "fr"
  ), envir = globalenv())
}
if (!exists(".t_fmt", envir = globalenv(), inherits = FALSE)) {
  assign(".t_fmt", function(template, ...) {
    vals <- list(...)
    for (nm in names(vals)) {
      template <- gsub(paste0("{", nm, "}"), format(vals[[nm]]), template, fixed = TRUE)
    }
    template
  }, envir = globalenv())
}

suppressPackageStartupMessages(library(shiny))

.ns <- function(x) paste0("tour-", x)

# ── (a) Builder pur — structure du 1er step ----------------------------------

test_that("(a) .tour_modal 1er step : PAS de Précédent, ids ns, compteur, easyClose", {
  steps <- ts_tour_steps()
  skip_if_not(is.list(steps) && length(steps) >= 2L)
  s1 <- steps[[1]]
  m <- .tour_modal(s1, 1L, length(steps), ns = .ns, tr = identity)
  html <- as.character(m)
  expect_false(grepl("tour-prev_step", html),
               info = "1er step : PAS de bouton Précédent (jamais un contrôle inerte)")
  expect_true(grepl("tour-next_step", html), info = "id next_step namespacé")
  expect_true(grepl("tour-skip_tour", html), info = "id skip_tour namespacé")
  expect_true(grepl("Suivant", html), info = "pas le dernier step : label Suivant")
  expect_false(grepl("Terminer", html),
               info = "Terminer réservé au dernier step")
  expect_true(grepl("Étape 1 sur 8", html), info = "compteur .t_fmt « Étape {i} sur {n} »")
  expect_true(grepl(s1$title, html, fixed = TRUE),
              info = "titre = registre verbatim (tr identity)")
  expect_false(grepl("data-dismiss", html),
               info = "easyClose=FALSE : aucune sortie silencieuse (data-dismiss)")
})

# ── (b) Builder pur — structure du dernier step ------------------------------

test_that("(b) .tour_modal dernier step : Terminer, Précédent présent, compteur", {
  steps <- ts_tour_steps()
  skip_if_not(is.list(steps) && length(steps) >= 2L)
  n <- length(steps)
  m <- .tour_modal(steps[[n]], n, n, ns = .ns, tr = identity)
  html <- as.character(m)
  expect_true(grepl("Terminer", html), info = "dernier step : label Terminer")
  expect_false(grepl(">Suivant<|\"Suivant\"", html),
               info = "plus de Suivant au dernier step")
  expect_true(grepl("tour-prev_step", html), info = "Précédent présent au dernier step")
  expect_true(grepl(sprintf("Étape %d sur %d", n, n), html),
              info = "compteur reflète la fin de visite")
})

# ── (c) testServer — machine à états + effets capturés par SEAMS -------------

test_that("(c) serveur : start/next/prev/skip/first_run + nav tab gelé + drapeau", {
  shown <- list()
  removed <- 0L
  navs <- list()
  customs <- list()
  testServer(
    mod_tour_server,
    # Seams injectables (arbitrage T7 — le proxy testServer refuse le
    # shadowing d'instance, mesuré) : faux qui CAPTURENT ; les défauts de
    # production sont verrouillés par les tests (a/b) + .tour_nav_select.
    args = list(
      id = "tour", # id EXPLICITE (convention test-asr3-ui.R) : session$ns("mark_done") = "tour-mark_done"
      .show_modal = function(ui) {
        shown[[length(shown) + 1L]] <<- ui
        invisible(NULL)
      },
      .remove_modal = function() {
        removed <<- removed + 1L
        invisible(NULL)
      },
      .nav_select = function(tab, session) {
        navs[[length(navs) + 1L]] <<- tab
        invisible(NULL)
      },
      .send_custom = function(session, name, msg = list()) {
        customs[[length(customs) + 1L]] <<- name
        invisible(NULL)
      }
    ),
    {
      get_index <- session$getReturned()
      expect_equal(get_index(), 0L, info = "visite inactive au montage")

      # start → step 1 (atterrissage : tab NULL, AUCUNE nav)
      session$setInputs(start = 1L)
      session$flushReact()
      expect_equal(get_index(), 1L)
      expect_length(shown, 1L)
      expect_true(grepl(ts_tour_steps()[[1]]$title, as.character(shown[[1]]),
                        fixed = TRUE),
                  info = "verrou structurel : la modale porte le titre du REGISTRE")
      expect_length(navs, 0L) # step d'atterrissage : aucune nav

      # next → step 2 + nav sur le tab GELÉ du registre
      session$setInputs(next_step = 1L)
      session$flushReact()
      expect_equal(get_index(), 2L)
      expect_length(shown, 2L)
      tab2 <- ts_tour_steps()[[2]]$tab
      expect_true(!is.null(tab2) && length(navs) >= 1L &&
                    identical(navs[[length(navs)]], tab2),
                  info = "nav porte la valeur nav_top GELÉE du registre")

      # prev → retour au step 1
      session$setInputs(prev_step = 1L)
      session$flushReact()
      expect_equal(get_index(), 1L)

      # skip depuis le step 1 → fin : index 0 + removeModal + drapeau posé
      session$setInputs(skip_tour = 1L)
      session$flushReact()
      expect_equal(get_index(), 0L)
      expect_true(removed >= 1L, info = "skip ferme la modale (removeModal)")
      expect_true("tour-mark_done" %in% customs,
                  info = "skip pose le drapeau localStorage (message custom)")

      # first_run démarre SEULEMENT si inactif
      session$setInputs(first_run = TRUE)
      session$flushReact()
      expect_equal(get_index(), 1L, info = "first_run : démarrage auto")
      session$setInputs(first_run = TRUE)
      session$flushReact()
      expect_equal(get_index(), 1L,
                   info = "first_run IGNORÉ si une visite est déjà en cours")

      # Jusqu'au dernier step : la modale passe à « Terminer »
      for (k in seq_len(length(ts_tour_steps()) - get_index())) {
        session$setInputs(next_step = k + 1L)
        session$flushReact()
      }
      expect_equal(get_index(), length(ts_tour_steps()))
      last_html <- as.character(shown[[length(shown)]])
      expect_true(grepl("Terminer", last_html),
                  info = "dernier step : Terminer")
      expect_false(grepl(">Suivant<", last_html),
                   info = "dernier step : plus de Suivant")
      tab_last <- ts_tour_steps()[[length(ts_tour_steps())]]$tab
      expect_true(identical(navs[[length(navs)]], tab_last),
                  info = "le dernier step a navigué vers son tab gelé")

      # next sur le dernier step = fin de visite propre
      session$setInputs(next_step = 99L)
      session$flushReact()
      expect_equal(get_index(), 0L, info = "Terminer finit la visite")
      expect_true(sum(customs == "tour-mark_done") >= 2L,
                  info = "Terminer pose aussi le drapeau")
    }
  )
})

# ── (d) UI : bouton start + script interpolé ---------------------------------

test_that("(d) mod_tour_ui : bouton ns(start) + script embarque clé/handler/input", {
  ui <- mod_tour_ui("tour")
  html <- as.character(ui)
  expect_true(grepl("tour-start", html), info = "bouton start namespacé")
  expect_true(grepl(TS_TOUR_STORAGE_KEY, html, fixed = TRUE),
              info = "le script embarque la clé localStorage du REGISTRE")
  expect_true(grepl("tour-mark_done", html), info = "handler custom interpolé")
  expect_true(grepl("tour-first_run", html), info = "input first_run interpolé")
})

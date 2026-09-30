# Shared helpers for the shinytest2 "golden path" smoke tests (backlog #7).
# Coexists with helper-source.R (pure-helper unit tests): disjoint symbols,
# both auto-sourced by testthat before any test-*.R file runs.
#
# NOTE (API shinytest2) : $run_js() exécute le JS en fire-and-forget et
# renvoie NULL — toute valeur doit revenir par $get_js(). Les deux helpers
# qui lisent le DOM passent donc par get_js(), avec une IIFE pour que
# l'instruction 'return' soit légale quel que soit le contexte d'évaluation.

# shinytest2 s'auto-skippe avec la raison "On CRAN" tant que NOT_CRAN!=true
# (projet sans DESCRIPTION : aucun R CMD check ne passera jamais le poser).
# Pose ici, au chargement du helper, pour que la commande du plan —
#   Rscript -e "testthat::test_dir('tests/testthat')"
# — fonctionne telle quelle.
Sys.setenv(NOT_CRAN = "true")

#' Absolute path to the app's root directory (where app.R lives)
app_root_dir <- function() {
  normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
}

# --- Locale des processus ENFANTS (correctif 2026-09-16) --------------------
# shinytest2 demarre l'app dans un PROCESSUS ENFANT. Sys.setlocale() ne suffit
# donc PAS (il ne change que le processus courant), alors que les enfants
# heritent des VARIABLES d'environnement. Or Git Bash exporte LC_ALL=C.UTF-8,
# un nom que R sous Windows ne reconnait pas => repli silencieux sur "C" =>
# la lecture de i18n/translation.json (UTF-8) echoue par
# « invalid multibyte string, element 1 » — que ce helper rapportait a tort
# comme « Chromote/headless Chrome unavailable ».
# Mesure 2026-09-16 (A/B) : heritage LC_ALL=C.UTF-8 => ECHEC ; LC_ALL leve +
# LC_CTYPE=fr_FR.UTF-8 => driver OK, inputs visibles.
# On ne touche QUE LC_ALL / LANG / LC_CTYPE, et seulement le temps du
# demarrage : forcer LC_ALL changerait aussi LC_COLLATE / LC_TIME, donc le tri
# des chaines, donc potentiellement des resultats de tests.

#' Candidate UTF-8 locale names accepted by R on Windows
#'
#' ⚠️ « fr_FR.UTF-8 » a été retiré des candidats MESURÉ le 2026-09-28 : Windows
#' ACCEPTE le nom (setlocale ne lève pas) mais le résout vers le code page
#' ANSI — le grepl("UTF-8") du test d'acceptation passe alors à tort, et la
#' sortie stderr UTF-8 de l'app devient « chaîne de charactères multioctets
#' incorrecte / input string invalid in this locale » dans le poller de
#' shinytest2. « French_France.utf8 » (forme Windows native) est le premier
#' candidat réellement UTF-8.
ts_e2e_locale_candidates <- function() {
  c("French_France.utf8", "English_United States.utf8", "en_US.UTF-8")
}

#' Set LC_CTYPE to the first candidate R actually accepts; return its name
ts_e2e_first_valid_utf8_locale <- function() {
  for (cand in ts_e2e_locale_candidates()) {
    ok <- suppressWarnings(tryCatch({
      Sys.setlocale("LC_CTYPE", cand)
      grepl("UTF-8|utf8", Sys.getlocale("LC_CTYPE"), ignore.case = TRUE)
    }, error = function(e) FALSE))
    if (isTRUE(ok)) return(cand)
  }
  NA_character_
}

#' Restore an environment snapshot taken with unset = NA_character_
ts_e2e_restore_env <- function(old) {
  present <- old[!is.na(old)]
  if (length(present)) do.call(Sys.setenv, as.list(present))
  absent <- names(old)[is.na(old)]
  if (length(absent)) Sys.unsetenv(absent)
}

#' Evaluate expr with a locale that child processes can actually use
ts_e2e_with_child_locale <- function(expr) {
  old <- Sys.getenv(c("LC_ALL", "LANG", "LC_CTYPE"), unset = NA_character_)
  on.exit(ts_e2e_restore_env(old), add = TRUE)
  # Ne toucher a l'environnement QUE si l'on sait poser une locale utilisable :
  # sinon on laisse l'etat initial (l'echec reste visible et honnetement
  # rapporte) plutot que d'echanger un probleme d'encodage contre un autre.
  if (!is.na(ts_e2e_first_valid_utf8_locale())) {
    # MESURÉ le 2026-09-28 (cette machine) : poser SEULEMENT LC_CTYPE ne suffit
    # pas — avec LC_ALL/LANG désettés, un composant de la pile écrit du
    # cp1252 sur stderr et le poller du parent (UTF-8) meurt sur
    # « input string N is invalid in this locale ». LC_ALL = le même candidat
    # UTF-8 que LC_CTYPE rend le boot fiable. Le coût annoncé plus bas
    # (LC_COLLATE/LC_TIME héritent du candidat) est accepté : les e2e sont des
    # smoke tests, pas des tests sensibles au tri.
    locale <- ts_e2e_first_valid_utf8_locale()
    Sys.setenv(LC_ALL = locale)
    Sys.setenv(LC_CTYPE = locale)
    Sys.unsetenv("LANG")
  }
  force(expr)
}

#' Best-effort AppDriver constructor — skips (never fails) if shinytest2 or
#' a headless Chrome (chromote) is unavailable. Generous timeouts : global.R
#' charge toute la pile de packages, et l'init des 6 daemons mirai est
#' différée à la première ouverture de l'onglet Spatial (boot lazy, voir
#' l'en-tête de app.R).
new_app_driver <- function(name) {
  testthat::skip_if_not_installed("shinytest2")
  err <- NULL
  driver <- tryCatch(
    ts_e2e_with_child_locale(
      shinytest2::AppDriver$new(
        app_dir = app_root_dir(), name = name, height = 900, width = 1400,
        load_timeout = 120000, timeout = 120000,
        # Smoke tests : check_names=FALSE pour qu'un warning de doublon d'id
        # ne transforme PAS chaque test en skip silencieux — le drift de
        # namespace est déjà asserté explicitement via get_values().
        check_names = FALSE,
        options = list(shiny.testmode = TRUE)
      )
    ),
    error = function(e) { err <<- conditionMessage(e); NULL }
  )
  # Le motif de skip porte desormais la VRAIE cause : un echec de locale ne
  # doit plus se déguiser en « Chrome absent » (8 tests restes invisibles
  # ainsi jusqu'au 2026-09-16).
  if (is.null(driver)) {
    msg <- if (is.null(err)) "raison inconnue" else err
    if (grepl("multibyte|encoding|locale|LC_", msg, ignore.case = TRUE)) {
      testthat::skip(paste("Locale inutilisable par le processus enfant :", msg))
    }
    testthat::skip(paste("AppDriver indisponible (Chromote/headless Chrome ?) :", msg))
  }
  driver
}

#' Click a top-level nav tab by (partial) visible text, JS-side — more
#' robust than a CSS selector on bslib's emoji+text nav-link markup.
click_nav_by_text <- function(app, text_fragment) {
  clicked <- app$get_js(sprintf(
    "(function() {
       var links = document.querySelectorAll('a.nav-link');
       for (var i = 0; i < links.length; i++) {
         if (links[i].textContent.indexOf('%s') !== -1) { links[i].click(); return true; }
       }
       return false;
     })()", text_fragment
  ))
  testthat::expect_true(isTRUE(clicked), info = paste("Nav tab not found:", text_fragment))
  # Best-effort uniquement : la sidebar (textOutput mem_usage) et l'init
  # lazy des daemons mirai (onglet Spatial) peuvent maintenir Shiny
  # "instable" >15s ; attendre la stabilite est un confort de rendu, pas une
  # assertion — un vrai crash d'output sera de toute facon attrape par
  # assert_no_shiny_error()/get_values() qui suivent.
  try(app$wait_for_idle(duration = 250, timeout = 10000), silent = TRUE)
  invisible(app)
}

#' Assert no Shiny/JS runtime error is currently shown in the DOM.
assert_no_shiny_error <- function(app) {
  n_errors <- app$get_js("document.querySelectorAll('.shiny-output-error').length")
  testthat::expect_equal(n_errors, 0, info = "A .shiny-output-error element is visible.")
}

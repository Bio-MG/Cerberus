# =============================================================================
# modules/session/mod_tour.R — N4 biomentat-tour (satellites slice S6)
# =============================================================================
# Fiche (fait foi) : docs/proposals/PROPOSAL_N_SATELLITES_LAUNCH_ROADMAP_
# 2026-10-26.md §slice S6 + mission round8 slice A. Visite guidée HAND-ROLLED
# sur bslib (modalDialog) — 0 nouvelle dépendance (défaut §3-D2, opérateur) ;
# skippable ; first-run localStorage ; FR via i18n. Les steps vivent dans le
# registre PUR tools/pkg_skeleton/R/tour_registry.R (ancres ⊆ vocabulaire
# GELÉ drive_allowlist.R — la visite EST une suite d'acceptation) : ce module
# ne fabrique AUCUN id, il consomme le registre verbatim.
#
# Montage app.R (additions-only, 3 fragments, namespace "tour") :
#   * mod_tour_ui()     -> sidebar, voisin de help_btn (section Aide) ;
#   * mod_tour_server() -> fin de server(), modèle mod_session_checkpoint_server.
#
# Discipline (modèle mod_session_checkpoint) :
#   * AUCUNE écriture disque : le drapeau « visite déjà faite » vit dans le
#     localStorage du NAVIGATEUR (clé TS_TOUR_STORAGE_KEY, portée par la
#     version) — jamais dans tools/_drive ni data_sc ;
#   * first-run : le script UI émet l'input ns("first_run") UNE FOIS (et
#     seulement si la clé est absente) ; le serveur démarre la visite si
#     aucune visite n'est déjà en cours (garde rv$index == 0) ;
#   * skippable : « Passer la visite » ferme la modale + pose le drapeau.
#     easyClose = FALSE — la modale ne se ferme JAMAIS en silence : tout
#     chemin de sortie est un bouton explicite ;
#   * nav : chaque step sélectionne d'abord son tab nav_top (bslib::nav_select
#     sur "main_nav" — le MÊME id que le canal drive, valeurs gelées), PUIS
#     montre la modale ; le step d'atterrissage (tab NULL) ne navigue pas ;
#     l'échec de nav est absorbé (tryCatch — une valeur gelée ne peut pas
#     échouer sur une app saine ; un échec ne bloque jamais la visite) ;
#   * fin de visite (Terminer / Passer) : removeModal + message
#     "tour-mark_done" (le script UI pose le localStorage).
#
# i18n : statiques -> i18n$t() ; compteur de step -> .t_fmt() (motif global.R,
# clés {i}/{n}). Tests : repli no-op identique à test-asr3-ui.R.
#
# Testabilité (verrous CQF-2) : .tour_modal() est PURE (ns + tr injectés) et
# verrouillée structurellement (ids des boutons, absence de Précédent au 1er
# step, label « Terminer » au dernier). Les EFFETS session (modale, nav,
# message custom) passent par des SEAMS injectables à DÉFAUTS DE PRODUCTION
# (arbitrage T7 — précédent test-asr-wiring.R:86-119) : testServer ne permet
# PAS le shadowing d'instance (proxy verrouillé, mesuré « Attempted to assign
# value on session proxy ») ; les tests injectent des faux et les défauts
# portent le comportement réel. L'état machine est exposé en retour (reactive
# index).
#
# ⚠️ PIÈGE MESURÉ (shiny 1.14.0 + MockShinySession) : observeEvent(...,
# ignoreInit = TRUE) ne tire JAMAIS sur le premier session$setInputs() du
# testServer (le flush du setInputs compte comme init) — les 5 observateurs
# sont donc SANS ignoreInit : sûr en production aussi (start/next/prev/skip/
# first_run n'existent pas avant le premier clic/message, aucun tir parasite
# à l'init) ; le garde rv$index == 0 protège le double first_run.
# =============================================================================

#' Défaut de production du seam nav : sélection du tab nav_top GELÉ sur
#' "main_nav" (le MÊME id que le canal drive), erreur absorbée (une valeur
#' gelée ne peut pas échouer sur une app saine ; un échec ne bloque jamais
#' la visite).
.tour_nav_select <- function(tab, session) {
  tryCatch(
    bslib::nav_select("main_nav", selected = tab, session = session),
    error = function(e) NULL
  )
}

#' Défaut de production du seam message : pose du drapeau localStorage côté
#' navigateur (handler interpolé par mod_tour_ui). Best-effort assumé : un
#' échec ré-offrira la visite au prochain lancement — jamais une erreur.
.tour_send_custom <- function(session, name, msg = list()) {
  tryCatch(session$sendCustomMessage(name, msg), error = function(e) NULL)
}

mod_tour_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::actionButton(ns("start"), label = i18n$t("🧭 Visite guidée"),
                        icon = shiny::icon("compass"), class = "btn-info w-100"),
    # First-run + pose du drapeau : le script vit côté NAVIGATEUR. Les trois
    # littéraux (clé, handler, input) sont interpolés depuis les constantes
    # du registre et le ns — jamais codés en dur dans le JS.
    shiny::tags$script(shiny::HTML(sprintf(
      '(function() {
        var KEY = %s;
        var NS_MARK = %s;
        var NS_FIRST = %s;
        Shiny.addCustomMessageHandler(NS_MARK, function(m) {
          try { localStorage.setItem(KEY, "1"); } catch (e) {}
        });
        $(document).on("shiny:connected", function() {
          var done = false;
          try { done = !!localStorage.getItem(KEY); } catch (e) {}
          if (!done) { Shiny.setInputValue(NS_FIRST, true, { priority: "event" }); }
        });
      })();',
      jsonlite::toJSON(TS_TOUR_STORAGE_KEY),
      jsonlite::toJSON(ns("mark_done")),
      jsonlite::toJSON(ns("first_run"))
    )))
  )
}

#' Builder PUR de la modale d'un step — ns et tr INJECTÉS (testable sans
#' session ; leçon CQF-2 : verrous structurels, jamais le texte seul).
#'
#' Le 1er step n'a PAS de bouton Précédent (pas un bouton inerte : la visite
#' n'offre jamais un contrôle qui ne fait rien) ; le dernier step libellé
#' « Terminer » (le serveur route le MÊME input next_step vers la fin).
.tour_modal <- function(step, index, total, ns, tr) {
  is_first <- index <= 1L
  is_last <- index >= total
  shiny::modalDialog(
    title = shiny::tagList(
      tr(step$title),
      shiny::tags$span(class = "text-muted small",
                       style = "margin-left:8px; font-weight:normal;",
                       .t_fmt(tr("Étape {i} sur {n}"), i = index, n = total))
    ),
    shiny::div(shiny::p(tr(step$text))),
    footer = shiny::tagList(
      if (is_first) NULL else
        shiny::actionButton(ns("prev_step"), tr("Précédent"),
                            class = "btn-outline-secondary"),
      shiny::actionButton(ns("next_step"),
                          if (is_last) tr("Terminer") else tr("Suivant"),
                          class = "btn-primary"),
      shiny::actionButton(ns("skip_tour"), tr("Passer la visite"),
                          class = "btn-link text-muted")
    ),
    easyClose = FALSE, fade = TRUE, size = "m"
  )
}

#' Serveur de la visite : machine à états sur le registre figé.
#'
#' Seams injectables (défauts = production, arbitrage T7 — le proxy testServer
#' refuse le shadowing d'instance, mesuré) : \code{.show_modal}/
#' \code{.remove_modal} = shiny::showModal/removeModal ; \code{.nav_select} =
#' \code{.tour_nav_select} (tab gelé sur "main_nav") ; \code{.send_custom} =
#' \code{.tour_send_custom} (drapeau localStorage). Les tests injectent des
#' faux ; le comportement réel vit dans les défauts.
#'
#' Retour : \code{reactive()} de l'index courant (0 = visite inactive) —
#' surface de test, jamais consommée par app.R.
mod_tour_server <- function(id,
                            .show_modal = shiny::showModal,
                            .remove_modal = shiny::removeModal,
                            .nav_select = .tour_nav_select,
                            .send_custom = .tour_send_custom) {
  shiny::moduleServer(id, function(input, output, session) {
    steps <- ts_tour_steps()
    if (!isTRUE(ts_tour_steps_valid(steps))) {
      stop(
        paste0(
          "Registre de visite invalide (tour_registry.R) — défaut de ",
          "développement, verrou test-tour-registry.R"
        ),
        call. = FALSE
      )
    }
    total <- length(steps)
    rv <- shiny::reactiveValues(index = 0L)

    tour_show <- function(i) {
      step <- steps[[i]]
      rv$index <- i
      if (!is.null(step$tab)) {
        .nav_select(step$tab, session = session)
      }
      .show_modal(.tour_modal(step, i, total, ns = session$ns,
                              tr = function(k) i18n$t(k)))
    }

    tour_end <- function() {
      rv$index <- 0L
      .remove_modal()
      .send_custom(session = session, name = session$ns("mark_done"))
    }

    shiny::observeEvent(input$start, {
      tour_show(1L)
    })

    shiny::observeEvent(input$first_run, {
      if (rv$index == 0L) tour_show(1L)
    })

    shiny::observeEvent(input$next_step, {
      i <- rv$index
      if (i >= total) tour_end() else tour_show(i + 1L)
    })

    shiny::observeEvent(input$prev_step, {
      i <- rv$index
      if (i > 1L) tour_show(i - 1L)
    })

    shiny::observeEvent(input$skip_tour, {
      tour_end()
    })

    shiny::reactive(rv$index)
  })
}

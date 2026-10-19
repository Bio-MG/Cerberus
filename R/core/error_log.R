# R/core/error_log.R — journal des erreurs AVALÉES (jalon M-1, 2026-09-30)
#
# Constat audité (audit externe, §D/§J-4) : sur 776 handlers d'erreur,
# ≈ 586 ne relancent ni ne journalisent — l'utilisateur voit « une étape vide
# sans message », et personne ne peut diagnostiquer après coup. Premier remède
# : une passerelle de journalisation PURE qu'un handler silencieux appelle
# avant de rendre sa valeur de repli :
#
#     error = function(e) { ts_log_swallow("report_collector.build_x", e); NA_integer_ }
#
# Contrats : (1) le logger NE LÈVE JAMAIS (un logger en panne ne doit pas
# casser le handler qui l'appelle) ; (2) format de ligne daté
# `[horodatage] [session:<token>] contexte | classe | message`, en APPEND dans
# `QC/swallowed_errors.log` (QC/ est gitigné — artefact de session, comme le
# reste de QC/). Journal voluntairement BRUT (pas d'i18n) : il est lu par un
# développeur, pas par le biologiste.
#
# Chantier #10 (2026-10-19) : le token de session a été AJOUTÉ DEVANT le
# contexte. Il ne pouvait pas être inséré entre le contexte et la classe sans
# rompre l'adjacence `contexte | classe` que les tests de format épinglent ;
# le placer en tête est une EXTENSION, pas une rupture de contrat.

#' Chemin du journal des erreurs avalées
#'
#' `QC/swallowed_errors.log` sous la racine de l'app (via le marqueur `app.R`
#' déjà utilisé par `.resolve_app_base_dir()` quand il est disponible, sinon
#' le répertoire courant — cas des tests unitaires).
ts_swallowed_log_path <- function() {
  base <- NULL
  if (exists(".resolve_app_base_dir", inherits = TRUE)) {
    base <- tryCatch(.resolve_app_base_dir(), error = function(e) NULL)
  }
  if (is.null(base) || !nzchar(base)) base <- getwd()
  file.path(base, "QC", "swallowed_errors.log")
}

#' Token de session courant, ou "no_session" hors session réactive
#'
#' Chantier #10. Une ligne de journal sans session est indiagnosticable : on
#' ne sait pas si l'avalement vient de l'onglet SC, de l'onglet Spatial, ou
#' d'un worker mirai — où il n'y a AUCUNE session.
#'
#' `shiny::getDefaultReactiveDomain()` rend la session courante aussi bien
#' dans un module que sous `testServer()` (MESURÉ : `session_proxy$token` est
#' identique à `session$token`), et `NULL` hors session. Le repli est la
#' CHAÎNE "no_session" — jamais NA, jamais du vide : une ligne de journal doit
#' rester lisible telle quelle par un développeur.
#'
#' Contrat du logger oblige : cette fonction NE LÈVE JAMAIS.
#'
#' @return Une chaîne non vide : le token, ou "no_session".
ts_session_token <- function() {
  tryCatch({
    if (!requireNamespace("shiny", quietly = TRUE)) return("no_session")
    d <- shiny::getDefaultReactiveDomain()
    if (is.null(d)) return("no_session")
    tok <- d$token
    if (!is.character(tok) || length(tok) != 1L || is.na(tok) || !nzchar(tok)) {
      return("no_session")
    }
    tok
  }, error = function(e) "no_session")
}

#' Journaliser une erreur avalée — NE LÈVE JAMAIS
#'
#' @param context Chaîne courte identifiant le site
#'   (convention : `<fichier>.<fonction>`).
#' @param condition La condition capturée (error/warning) — accepte tout
#'   (une condition exotique ou même une chaîne est journalisée, pas un crash).
#' @param max_bytes Seuil de rotation : quand le journal courant dépasse cette
#'   taille (octets) AVANT l'append, il est renommé `<log>.1` — une seule
#'   génération conservée, l'ancienne .1 est écrasée — et l'append repart à
#'   neuf. Défaut 1 Mo. Politique volontairement minimale : le journal est un
#'   artefact de session gitigné, pas une piste d'audit à long terme ; un seul
#'   fichier de génération évite toute accumulation non bornée.
#' @param log_path Chemin du journal (paramètre explicite : testabilité).
#' @return `NULL` invisible, toujours.
ts_log_swallow <- function(context, condition, log_path = ts_swallowed_log_path(),
                           max_bytes = 1048576L) {
  tryCatch({
    cls <- paste(class(condition), collapse = "/")
    txt <- if (inherits(condition, "condition")) {
      conditionMessage(condition)
    } else {
      paste(as.character(condition), collapse = " ")
    }
    line <- sprintf("[%s] [session:%s] %s | %s | %s",
                    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                    ts_session_token(), context, cls, txt)
    dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
    # Rotation AVANT l'append. isTRUE() car file.info()$size vaut NA quand le
    # fichier manque — un `if (NA)` casserait, ce que le contrat interdit.
    if (isTRUE(file.info(log_path)$size >= max_bytes)) {
      suppressWarnings(file.rename(log_path, sprintf("%s.1", log_path)))
    }
    # suppressWarnings : un chemin non ouvrable émet un warning de cat() — le
    # contrat « ne lève jamais » couvre les warnings aussi (l'erreur, elle, est
    # avalée par le tryCatch).
    suppressWarnings(cat(line, file = log_path, sep = "\n", append = TRUE))
  }, error = function(e) NULL)
  invisible(NULL)
}

#' Fabriquer un gestionnaire `error=` qui journalise puis rend un repli
#'
#' Chantier #10. Un seul site porte la FORME du gestionnaire : sans cela, on
#' écrirait 200 copies de `error = function(e) { ts_log_swallow(...); NULL }`,
#' c'est-à-dire deux vérités pour un même comportement (règle M8).
#'
#'     tryCatch(expr, error = ts_swallow("spatial_export.qc_histogrammes"))
#'
#' ⚠️ NE PAS instrumenter un SONDAGE dont `NULL` est la réponse ATTENDUE
#' (ex. `tryCatch(isolate(global_data$sc_obj), error = NULL)` quand aucun jeu
#' n'est encore chargé, ou une lecture de `reactiveValues` hors contexte) :
#' ce n'est pas un échec avalé, c'est du FLUX DE CONTRÔLE. Les instrumenter
#' noierait le journal sous le bruit et rendrait les vrais échecs invisibles —
#' l'inverse du but. À instrumenter : un site dont l'échec laisse un ARTEFACT
#' MANQUANT (figure d'export, section de rapport) ou une action utilisateur
#' sans effet ni message.
#'
#' @param context Voir `ts_log_swallow()` (convention `<fichier>.<fonction>`).
#' @param fallback Valeur rendue par le gestionnaire (défaut `NULL`).
#' @param log_path Voir `ts_log_swallow()`.
#' @return Une fonction à un argument (la condition), utilisable en `error=`.
ts_swallow <- function(context, fallback = NULL, log_path = ts_swallowed_log_path()) {
  force(context)
  force(fallback)
  force(log_path)          # résolu À LA CRÉATION du gestionnaire, pas à l'appel
  function(e) {
    ts_log_swallow(context, e, log_path = log_path)
    fallback
  }
}

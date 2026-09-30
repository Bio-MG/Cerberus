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
# `[horodatage] contexte | classe | message`, en APPEND dans
# `QC/swallowed_errors.log` (QC/ est gitigné — artefact de session, comme le
# reste de QC/). Journal voluntairement BRUT (pas d'i18n) : il est lu par un
# développeur, pas par le biologiste.

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
    line <- sprintf("[%s] %s | %s | %s",
                    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                    context, cls, txt)
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

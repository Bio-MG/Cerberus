# =============================================================================
# tools/pkg_skeleton/R/report_validator.R (ex-R/reports/ — D2 M5 lot 2) — Rapport consolide 4F : VALIDATEUR (Stage 17)
# =============================================================================
# Valide AVANT rendu chaque section de l'entree collectee :
#   - result absent            -> "absent"   (message gracieux, PAS une erreur
#                                  technique — le rapport dit simplement que
#                                  l'analyse n'a pas ete executee) ;
#   - result present, domaine a contrat :
#       * provenance absente   -> "blocked"  (section REFUSEE — aucune
#                                  reconstruction de provenance, regle 7) ;
#       * identite obsolete    -> "stale"    (l'objet courant a change depuis le
#                                  calcul — bandeau explicite, contenu quand
#                                  meme trace) ;
#       * identite inverifiable-> "unknown"  (objet courant absent ou empreinte
#                                  indisponible) ;
#       * sinon                -> "valid" ;
#   - result present, domaine legacy -> "valid_legacy" (tracabilite par la
#       provenance partagee ; pas d'identite figee pour ces domaines).
#
# Le validateur ne RE-INTERPRETE pas le statut scientifique des resultats :
# un resultat canonique stocke a DEJA passe les gardes assert_* de son contrat
# au moment de sa production. La seule exception figee : design DA de statut
# "invalid_design" -> "invalid" (design refuse, bandeau rouge).
# =============================================================================

#' Etats de validation figés du rapport consolidé
consolidated_report_validation_states <- function() {
  c("absent", "valid", "valid_legacy", "stale", "invalid", "unknown", "blocked")
}

.report_val_stop <- function(msg) {
  stop(errorCondition(
    msg,
    state = "invalid_input",
    class = "report_error"
  ))
}

#' Libellés français figés des états de validation
.report_state_labels <- c(
  absent       = "Analyse non exécutée pour ce projet.",
  valid        = "Résultat canonique valide et traçable.",
  valid_legacy = "Résultat présent — traçabilité par la provenance partagée (pas d'identité figée pour ce domaine).",
  stale        = "Identité obsolète : l'objet courant a changé depuis le calcul (résultat conservé à titre de trace).",
  invalid      = "Résultat produit mais INVALIDE sur le plan scientifique (voir statut).",
  unknown      = "Identité invérifiable (objet courant absent ou empreinte indisponible).",
  blocked      = "Section refusée : provenance absente du résultat canonique — aucune reconstruction."
)

#' Valider l'entrée du rapport consolidé (verdicts par section)
#'
#' @param report_input Sortie de \code{collect_consolidated_report_input()}.
#' @return Liste \code{type = "consolidated_report_validation"} :
#'   \code{verdicts} (liste nommée par domaine : state, label, reason,
#'   analysis_ids), \code{counts} (data.frame état/fréquence),
#'   \code{blocked_sections}, \code{ok_overall}.
#' @errors \code{report_error} (\code{invalid_input}) si l'entrée n'est pas
#'   une sortie du collecteur.
validate_consolidated_report_input <- function(report_input) {
  if (!is.list(report_input) ||
      !identical(report_input$type, "consolidated_report_input")) {
    .report_val_stop(paste0(
      "validate_consolidated_report_input() : attend la sortie de ",
      "collect_consolidated_report_input() (type 'consolidated_report_input')."
    ))
  }

  verdicts <- setNames(lapply(.report_analysis_domains, function(dm) {
    entry <- report_input$analyses[[dm]]
    state <- "absent"
    reason <- unname(.report_state_labels[["absent"]])
    ids <- character(0)

    if (isTRUE(entry$present)) {
      ids <- as.character(entry$analysis_ids %||% character(0))
      state <- "valid"
      reason <- unname(.report_state_labels[["valid"]])

      if (dm %in% .report_contract_domains) {
        # Statut scientifique fige : design DA refuse -> section invalide.
        st <- as.character(
          report_input$analyses[[dm]]$summary$valeur[
            report_input$analyses[[dm]]$summary$champ == "statut"])
        st <- if (length(st) == 1L && nzchar(st)) st else NA_character_
        if (!is.na(st) && st == "invalid_design") {
          state <- "invalid"
          reason <- paste(unname(.report_state_labels[["invalid"]]),
                          "Statut du résultat : invalid_design.")
        } else if (!isTRUE(entry$provenance_available)) {
          state <- "blocked"
          reason <- unname(.report_state_labels[["blocked"]])
        } else if (isTRUE(entry$identity_checked)) {
          if (isFALSE(entry$identity_ok)) {
            state <- "stale"
            reason <- unname(.report_state_labels[["stale"]])
          } else if (is.na(entry$identity_ok)) {
            state <- "unknown"
            reason <- unname(.report_state_labels[["unknown"]])
          }
        }
      } else if (dm %in% .report_global_domains) {
        # 4F-EXT : résultat global auto-daté (global_data) — même état
        # valid_legacy (jamais bloqué), libellé dédié (la traçabilité
        # repose sur ran_at, pas sur la provenance partagée).
        state <- "valid_legacy"
        reason <- "Résultat global présent — auto-daté à la production (ran_at), compilé sans re-analyse."
      } else {
        state <- "valid_legacy"
        reason <- unname(.report_state_labels[["valid_legacy"]])
      }
    }

    list(section = dm, state = state, label = reason,
         analysis_ids = ids, summary = entry$summary,
         identity_note = as.character(entry$identity_note %||% character(0)))
  }), .report_analysis_domains)

  state_vec <- vapply(verdicts, function(v) v$state, character(1))
  counts <- as.data.frame(
    table(etat = factor(state_vec, levels = consolidated_report_validation_states())),
    responseName = "n_sections", stringsAsFactors = FALSE
  )
  counts <- counts[counts$n_sections > 0L, , drop = FALSE]
  rownames(counts) <- NULL

  list(
    type = "consolidated_report_validation",
    version = "1.0",
    validated_at = Sys.time(),
    input_analysis_id = report_input$analysis_id,
    verdicts = verdicts,
    counts = counts,
    blocked_sections = names(state_vec)[state_vec == "blocked"],
    ok_overall = !any(state_vec == "blocked")
  )
}

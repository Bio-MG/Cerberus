# =============================================================================
# R/core/error_state.R — Accesseur GÉNÉRIQUE de l'état sémantique d'une erreur
# =============================================================================
# CHRYSALIS / §14.1. Sourcé dans le bloc CORE de app.R, donc AVANT tout fichier
# de domaine : les 11 accesseurs `*_error_state()` de R/bulk/ et R/sc/ en
# dépendent par délégation.
#
# ── Doctrine (CONVENTIONS.md §14.1, tranchée le 2026-09-19) ──────────────────
# Le dépôt a DEUX AXES ORTHOGONAUX pour une erreur :
#   - `class` = l'axe de ROUTAGE — ce qu'un `tryCatch`/`inherits()` intercepte.
#     OBLIGATOIRE (C10), et il suit les branches (a)/(b)/(c) de §7.
#   - `state` = l'axe SÉMANTIQUE — le cas précis DANS le domaine. FACULTATIF, et
#     JAMAIS porté par la classe : énumérer les cas dans `class` produirait une
#     explosion combinatoire de noms, alors qu'une classe est un VECTEUR en R —
#     elle est faite pour router, pas pour porter une valeur.
# Conséquence : l'accesseur d'état n'a pas à connaître son domaine. Un domaine
# NOUVEAU obtient `state=` gratuitement, sans nouvel accesseur.
#
# ── 🔴 LA PRÉMISSE ÉCRITE DE §14.1 ÉTAIT FAUSSE, ET ELLE EST CORRIGÉE ICI ────
# §14.1 justifiait la consolidation ainsi : « les 11 accesseurs existants sont
# TEXTUELLEMENT IDENTIQUES et sans logique de domaine ». MESURE du 2026-09-20
# (41e incrément) : c'est FAUX. Les 11 corps forment TROIS familles :
#   F1 — gardés par leur CLASSE (`if (inherits(e, "<cls>")) e$state else NA`)
#        → 7 : da_design, milo, sccoda, communication, cellchat_input,
#              population_rarity, velocity ;
#   F2 — AVEUGLES à la classe (`st <- e$state; if (is.null(st)) NA else …`)
#        → 3 : bulk_network, bulk_pattern, bulk_dose ;
#   F3 — gardés sur `"condition"` + tryCatch + contrôle de longueur
#        → 1 : cellchat_engine.
# La divergence est OBSERVABLE, pas cosmétique : appelés sur une erreur de
# classe ÉTRANGÈRE portant un `state`, les 4 accesseurs de F2/F3 rendent cet
# état, les 7 de F1 rendent NA. Table mesurée :
#
#   accesseur                          propre   nu    ÉTRANGER
#   bulk_network_error_state           s_own    NA    s_foreign
#   bulk_pattern_error_state           s_own    NA    s_foreign
#   bulk_dose_error_state              s_own    NA    s_foreign
#   da_design_error_state              s_own    NA    NA
#   milo_error_state                   s_own    NA    NA
#   sccoda_error_state                 s_own    NA    NA
#   communication_error_state          s_own    NA    NA
#   cellchat_engine_error_state        s_own    NA    s_foreign
#   cellchat_input_error_state         s_own    NA    NA
#   population_rarity_error_state      s_own    NA    NA
#   velocity_error_state               s_own    NA    NA
#
# ── D'où la FORME RETENUE : le filtre de classe est un PARAMÈTRE ─────────────
# §14.1 prescrivait `ts_error_state(e)`. Une forme SANS paramètre ne peut pas
# reproduire les DEUX sémantiques à la fois : elle en changerait une. On étend
# donc la prescription d'un argument `class` par défaut `NULL` :
#   - `class = NULL`  → aucune vérification : reproduit F2 et F3 À L'IDENTIQUE ;
#   - `class = "<cls>"` → reproduit F1 à l'identique.
# Un domaine nouveau appelle `ts_error_state(e)` — la promesse de §14.1 est
# tenue ; les domaines qui avaient choisi de GARDER leur classe la déclarent.
# Le paramètre est ce qui rend la consolidation PROUVEABLE plutôt que supposée.
#
# ⚠️ SEULE DIVERGENCE ASSUMÉE (hors contrat, normalisante) : là où F1/F2
# laissaient fuiter un `state` de longueur > 1 tel quel, ou un `state` entier
# dans son type d'origine (`integer`), le générique rend NA ou convertit en
# `character`. Le contrat documenté est « l'état, OU NA » ; F1/F2 le violaient
# sur des entrées qu'aucun appelant ne produit. Le test éponyme
# (`test-core-error-state.R`) épingle la table ci-dessus, donc cette divergence
# ne peut pas se déplacer en silence.
# =============================================================================

#' État sémantique d'une erreur, ou `NA_character_` si elle n'en porte pas.
#'
#' @param e Objet à interroger. Toute entrée qui n'est pas une `condition`
#'   rend `NA_character_` (jamais une erreur).
#' @param class Classe de routage attendue, ou `NULL` (défaut) pour n'imposer
#'   aucune contrainte. Fourni, il restreint la lecture aux erreurs de cette
#'   classe — c'est la sémantique des 7 accesseurs gardés de §14.1.
#' @return Une chaîne scalaire, ou `NA_character_`. Jamais `NULL`, jamais une
#'   chaîne vide, jamais un vecteur.
ts_error_state <- function(e, class = NULL) {
  if (!inherits(e, "condition")) return(NA_character_)
  if (!is.null(class) && !inherits(e, class)) return(NA_character_)
  st <- tryCatch(e$state, error = function(e2) NULL)
  if (is.null(st) || length(st) != 1L || is.na(st)) {
    NA_character_
  } else {
    as.character(st)
  }
}

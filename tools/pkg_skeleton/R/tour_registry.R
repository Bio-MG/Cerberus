# =============================================================================
# tools/pkg_skeleton/R/tour_registry.R — N4 cerberus-tour (satellites slice S6)
# =============================================================================
# Fiche (fait foi) : docs/proposals/PROPOSAL_N_SATELLITES_LAUNCH_ROADMAP_
# 2026-10-26.md §slice S6 — « Hand-rolled tour on existing bslib
# (modals/popovers) — NO new dependency » (§3-D2 : défaut hand-rolled, 0 dep) ;
# « Steps anchored on the FROZEN allowlist DOM ids (drive_allowlist.R) so tour
# steps = drive-checkable anchors » ; « each step's anchor testable by the
# drive channel (the tour IS an acceptance suite) ».
#
# RÔLE : registre PUR (données + accesseurs, ZÉRO appel Shiny — C2, comme
# drive_allowlist.R). Le module UI (modules/session/mod_tour.R) est le seul à
# toucher Shiny : il CONSOMME ce registre verbatim et ne lui ajoute rien.
#
# LE VERROU (la visite EST une suite d'acceptation) : l'ancre d'un step bouton
# DOIT être un token de TS_DRIVE_BUTTONS et le tab d'un step DOIT être une
# valeur nav_top gelée (constantes TS_DRIVE_*_TOP_TAB). Aucun id DOM local à
# la visite : un id inventé ici serait un id que le canal drive ne peut jamais
# vérifier — la propriété exacte que la fiche exige et que test-tour-registry.R
# (e) falsifie. Toute ÉVOLUTION du registre passe par une NOUVELLE version
# (TS_TOUR_VERSION v1.x, jamais silencieuse — règle FORMAT v1.0.0 de S4) et
# une NOUVELLE clé localStorage (TS_TOUR_STORAGE_KEY porte la version).
#
# i18n : title/text portent des CLÉS (chaînes françaises, convention shiny.i18n
# « clé = texte FR ») ajoutées à i18n/translation.json par append SCOPÉ
# (paires fr/en) — le module traduit au moment du rendu ; test-tour-registry.R
# (d) verrouille la présence fr+en de chaque clé.
#
# Position Bloc 03 (app.R) : après job_timebox.R — TS_DRIVE_* (allowlist) est
# sourcé plus haut dans le même bloc, donc les constantes gelées existent au
# moment où ce fichier est évalué (aucune référence vers l'aval).
# =============================================================================

#' Version du registre de visite. Toute évolution = v1.x nouvelle, jamais
#' silencieuse (règle d'évolution FORMAT v1.0.0, satellite S4).
TS_TOUR_VERSION <- "v1"

#' Clé localStorage du drapeau « visite déjà faite ». Porte la version : un
#' changement de parcours v2 repartira avec une clé neuve, jamais en silence.
TS_TOUR_STORAGE_KEY <- "cerberus_tour_done_v1"

#' Les trois valeurs nav_top gelées que la visite est autorisée à sélectionner
#' (références aux CONSTANTES de drive_allowlist.R — aucun littéral local).
#' Import n'a pas de valeur nav_top gelée (nav_menu sans value) : c'est la
#' surface d'atterrissage de l'app, le step correspondant porte tab = NULL.
ts_tour_top_tabs <- function() {
  c(TS_DRIVE_SC_TOP_TAB, TS_DRIVE_BULK_SIGNATURES_TOP_TAB,
    TS_DRIVE_SPATIAL_TOP_TAB)
}

#' Le vocabulaire complet des ancres autorisées : les boutons gelés du canal
#' drive (TS_DRIVE_BUTTONS) + les tabs nav_top gelés. EXACTEMENT cette union —
#' test-tour-registry.R (b) l'épingle contre les constantes elles-mêmes.
ts_tour_anchor_vocabulary <- function() {
  unique(c(TS_DRIVE_BUTTONS, ts_tour_top_tabs()))
}

#' Le registre figé des steps, dans l'ordre de visite.
#'
#' Chaque step = list(id, anchor, tab, kind, title, text) :
#' \itemize{
#'   \item \code{id} : identifiant court unique (journal/falsification) ;
#'   \item \code{anchor} : id DOM GELÉ — token de TS_DRIVE_BUTTONS (kind
#'     "button") ou valeur nav_top (kind "nav_top") ;
#'   \item \code{tab} : valeur nav_top à sélectionner AVANT de montrer le step
#'     (NULL = surface d'atterrissage, aucun nav) ;
#'   \item \code{kind} : "button" (ancre dans TS_DRIVE_BUTTONS) ou "nav_top"
#'     (ancre = le tab lui-même) ;
#'   \item \code{title}, \code{text} : clés i18n (textes français, clé = texte).
#' }
#'
#' Le parcours : import (atterrissage) → chaîne Bulk (Step 1 → DE → pathway →
#' signatures) → Single-Cell (annotation → marqueurs) → Spatial (pipeline).
#' 8 steps, tous portés par des tokens de TS_DRIVE_BUTTONS.
ts_tour_steps <- function() {
  list(
    list(
      id = "import_bulk", anchor = "import_bulk-btn_load", tab = NULL,
      kind = "button",
      title = "🧭 Bienvenue dans Cerberus",
      text = paste(
        "Cerberus analyse vos données Single-Cell, Bulk RNA et Spatial.",
        "Cette visite (8 étapes) présente les surfaces principales ; quittez-la",
        "à tout moment avec « Passer la visite ». Premier arrêt : l'onglet",
        "d'import — le bouton « Charger » crée l'objet d'analyse depuis vos",
        "fichiers (comptes bruts attendus)."
      )
    ),
    list(
      id = "bulk_filter", anchor = "bulk-filter-run_filter_norm",
      tab = TS_DRIVE_BULK_SIGNATURES_TOP_TAB, kind = "button",
      title = "📊 Step 1 — Filtrage & VST (Bulk)",
      text = paste(
        "Le cœur Bulk commence ici : « Lancer Filtrage & VST » prépare la",
        "matrice (filtres qualité, transformation de variance stabilisée).",
        "C'est l'étape 1 de la chaîne Bulk — la DE et les pathways la",
        "consomment."
      )
    ),
    list(
      id = "bulk_de", anchor = "bulk-de-run_de",
      tab = TS_DRIVE_BULK_SIGNATURES_TOP_TAB, kind = "button",
      title = "📊 Analyse différentielle (Bulk)",
      text = paste(
        "Après le choix du design (contraste), « Lancer DE » exécute",
        "l'analyse différentielle (DESeq2) et remplit la table des gènes",
        "différentiels, base des analyses suivantes."
      )
    ),
    list(
      id = "bulk_pathways", anchor = "bulk-pathways-run_pathway",
      tab = TS_DRIVE_BULK_SIGNATURES_TOP_TAB, kind = "button",
      title = "📊 Enrichissement en pathways (Bulk)",
      text = paste(
        "« Lancer Pathway » enrichit la liste de gènes différentiels en voies",
        "biologiques (GO/Reactome) : barplot, dotplot et réseau d'enrichissement",
        "se remplissent au fil des résultats."
      )
    ),
    list(
      id = "bulk_signatures", anchor = "bulk-signatures-run_signatures",
      tab = TS_DRIVE_BULK_SIGNATURES_TOP_TAB, kind = "button",
      title = "📊 Signatures (Bulk)",
      text = paste(
        "Au-delà de la chaîne DE : scores de signatures géniques (empreintes",
        "prédéfinies ou personnalisées) — l'onglet Signatures héberge ces",
        "analyses par échantillon."
      )
    ),
    list(
      id = "sc_annotation", anchor = "sc-annotation-run_annot",
      tab = TS_DRIVE_SC_TOP_TAB, kind = "button",
      title = "🔬 Annotation des cellules (Single-Cell)",
      text = paste(
        "Onglet Single-Cell : « Lancer Annotation » identifie les types",
        "cellulaires (SingleR) — première étape du workflow SC, dont dépend la",
        "lecture biologique de tous les panneaux en aval."
      )
    ),
    list(
      id = "sc_markers", anchor = "sc-markers-run_markers",
      tab = TS_DRIVE_SC_TOP_TAB, kind = "button",
      title = "🔬 Marqueurs (Single-Cell)",
      text = paste(
        "« Lancer Marqueurs » calcule les gènes marqueurs par identité",
        "cellulaire ; la table des marqueurs (onglet Tableau) est le rendu",
        "principal, consultable et exportable."
      )
    ),
    list(
      id = "spatial_pipeline", anchor = "spatial-pipeline-btn_run_all",
      tab = TS_DRIVE_SPATIAL_TOP_TAB, kind = "button",
      title = "🗺️ Pipeline Spatial",
      text = paste(
        "Onglet Spatial : « Lancer le pipeline » enchaîne les étapes",
        "d'analyse spatiale (QC, déconvolution, clustering). Fin de la visite —",
        "elle ne réapparaîtra pas automatiquement ; le bouton « Visite guidée »",
        "de la barre latérale la relance à la demande."
      )
    )
  )
}

#' Valide un registre de steps — PUR, ne JAMAIS stop() (rend TRUE/FALSE).
#'
#' Vérifie : liste non vide ; shape exact (id/anchor/tab/kind/title/text) ;
#' ids uniques non vides ; ancre dans le vocabulaire GELÉ ; kind cohérent avec
#' l'ancre (button ⇔ TS_DRIVE_BUTTONS, nav_top ⇔ tabs gelés) ; tab NULL ou
#' valeur nav_top gelée ; title/text non vides. Un registre invalide est un
#' défaut de développement (verrouillé au test), jamais une condition runtime —
#' le module consomme ts_tour_steps_valid() dans sa garde de montage.
ts_tour_steps_valid <- function(steps = ts_tour_steps()) {
  if (!is.list(steps) || length(steps) == 0L) return(FALSE)
  vocab <- ts_tour_anchor_vocabulary()
  tabs <- ts_tour_top_tabs()
  needed <- c("id", "anchor", "tab", "kind", "title", "text")
  ids <- character(0)
  for (s in steps) {
    if (!is.list(s) || !all(needed %in% names(s))) return(FALSE)
    if (!is.character(s$id) || length(s$id) != 1L || !nzchar(s$id)) return(FALSE)
    ids <- c(ids, s$id)
    if (!is.character(s$anchor) || length(s$anchor) != 1L || !nzchar(s$anchor))
      return(FALSE)
    if (!s$anchor %in% vocab) return(FALSE)
    if (!s$kind %in% c("button", "nav_top")) return(FALSE)
    ok_kind <- (identical(s$kind, "button") && s$anchor %in% TS_DRIVE_BUTTONS) ||
      (identical(s$kind, "nav_top") && s$anchor %in% tabs)
    if (!isTRUE(ok_kind)) return(FALSE)
    tab_ok <- is.null(s$tab) ||
      (is.character(s$tab) && length(s$tab) == 1L && nzchar(s$tab) &&
         s$tab %in% tabs)
    if (!isTRUE(tab_ok)) return(FALSE)
    for (k in c("title", "text")) {
      if (!is.character(s[[k]]) || length(s[[k]]) != 1L || !nzchar(s[[k]]))
        return(FALSE)
    }
  }
  if (anyDuplicated(ids) > 0L) return(FALSE)
  TRUE
}

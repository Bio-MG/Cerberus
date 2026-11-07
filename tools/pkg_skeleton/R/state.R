# =============================================================================
# R/core/state.R — Contrat d'etat transversal + fabriques d'etat partage
# =============================================================================
# CHRYSALIS PHASE — contrat d'etat (ROADMAP 2A + TREE CERBERUS step 3).
# Single source of truth for every reactiveValues schema that is shared across
# modules. Sourced in app.R BEFORE any domain file so modules can call these
# factories without circular deps.
#
# ── Regles de propriete (state ownership) ────────────────────────────────────
#   - CE fichier definit le vocabulaire d'etat transversal et la forme de
#     l'etat d'analyse : create_analysis_state(), is_analysis_state(),
#     assert_analysis_state() et les accesseurs generiques state_get()/
#     state_set()/state_has().
#   - Les extensions de schema propres a un domaine restent definissables
#     dans la couche du domaine (R/sc/sc_state.R et equivalents) ; le schema
#     SC partage (create_sc_shared_state) est consolide ICI pour ne garder
#     qu'une seule definition, R/sc/sc_state.R n'etant qu'un re-export fin
#     pour le code qui le source directement.
#   - Chaque module ne lit/ecrit que les champs documentes de son domaine.
#     Interdit d'inventer des champs ad hoc (shared_rv$<nom_non_enregistre>) :
#     toute nouvelle donnee partagee passe par une extension documentee de la
#     fabrique de domaine ou par un emplacement du contrat d'analyse.
#   - La migration des champs legacy (shared_rv$... a plat) vers le contrat
#     5 emplacements est explicitement DIFFEREE a l'etape de migration des
#     responsabilites (2E). D'ici la, create_sc_shared_state() et
#     create_spatial_shared_state() restent des schemas a plat INDEPENDANTS
#     de create_analysis_state() — on ne les derive PAS du contrat : ils
#     garderaient 5 emplacements inutilises et brouilleraient les deux
#     vocabulaires. Les deux constructeurs sont testes independamment.
#
# ── Contrat d'analyse : les 5 emplacements (namespaces) ─────────────────────
#   input         — identite des donnees brutes importees + descripteurs de
#                   source (JAMAIS de valeur temporaire d'interface) ;
#   preprocessing — sorties des etapes de transformation : QC, normalisation,
#                   reduction dimensionnelle, clustering, filtrage ;
#   results       — sorties persistantes des analyses scientifiques :
#                   marqueurs, pathways, trajectoire, velocity, et futures
#                   communication/abundance ;
#   visualization — preferences d'affichage et selections non scientifiques :
#                   palette, genes selectionnes, reglages de graphiques,
#                   contexte d'affichage actif ;
#   provenance    — emplacement RESERVE (list() vide a la creation) : son
#                   protocole d'ecriture sera implemente par
#                   R/core/provenance.R — entrees PRODUITES a chaque etape,
#                   CONSOLID-EES au rapport, jamais reconstruites apres coup.
# =============================================================================

#' Create the SC shared reactive state
#' @return A reactiveValues object with all SC module fields initialized.
create_sc_shared_state <- function() {
  shiny::reactiveValues(
    markers_data = NULL,
    correlated_genes = NULL,
    corr_target_gene = NULL,
    pathway_results = NULL,
    pathway_db = NULL,
    selected_genes = character(0),
    active_tab = NULL,
    report_viz_list = list(),
    traj_reduction = NULL,
    traj_method = NULL,
    traj_genes = character(0),
    max_cells_heavy = Inf,
    sc_palette = "default",
    sc_manual_colors = NULL,
    sc_manual_gradient = NULL,
    sc_manual_volcano_colors = NULL,
    communication_result = NULL,
    communication_collection = NULL,
    active_communication_sample = NULL,
    qc_snapshot = NULL,
    velocity_result = NULL,
    da_design_result = NULL,
    da_milo_result = NULL,
    da_sccoda_result = NULL,
    population_rarity_result = NULL
  )
}

# -----------------------------------------------------------------------------
# Invalidation des résultats au changement de JEU DE DONNÉES (audit 2026-09-27
# §1.5). Les champs ci-dessous sont des RÉSULTATS dérivés de l'objet courant :
# quand l'objet est remplacé par un nouveau jeu (import, activation d'un
# dataset enregistré), les laisser en place fait mélanger au rapport le jeu B
# avec les résultats du jeu A.
#
# L'invalidation est pilotée par un "epoch" (global_data$sc_obj_epoch) que
# n'incrémentent QUE les remplacements de dataset. Les re-commits de même
# lignée (pipeline, annotation, mapping, trajectoire) ne purgent PAS — sinon
# l'auto-pipeline effacerait ses propres résultats (marqueurs/pathways)
# calculés juste avant son commit final (sc_pipeline.R:402).
# -----------------------------------------------------------------------------

#' Champs résultats du domaine SC (purgés au changement de dataset)
#' @return Character vector of reactiveValues field names.
sc_shared_result_fields <- function() {
  c("markers_data", "correlated_genes", "corr_target_gene",
    "pathway_results", "pathway_db", "qc_snapshot",
    "traj_reduction", "traj_method",
    "velocity_result", "da_design_result", "da_milo_result",
    "da_sccoda_result", "population_rarity_result")
}

#' Purger les champs résultats d'un état partagé SC
#' @param state reactiveValues (create_sc_shared_state()).
#' @return The state, invisible.
sc_purge_shared_results <- function(state) {
  for (fld in sc_shared_result_fields()) state[[fld]] <- NULL
  invisible(state)
}

# -----------------------------------------------------------------------------
# Bump d'epoch des DEUX domaines (F08, revue externe 2026-10-28).
#
# L'epoch n'était incrémenté que par les imports et les activations de dataset.
# Les deux chemins DESTRUCTIFS de `app.R` — `load_session_file` (charger une
# session) et `confirm_reset` (tout effacer) — remplaçaient `sc_obj` / `bulk_obj`
# sans bumper : les résultats partagés du jeu précédent survivaient, et le
# rapport consolidé mélangeait deux jeux.
#
# 🔴 Le trou est CROSS-DOMAINE (`sc_obj_epoch` ET `bulk_obj_epoch`) : une seule
# fonction pour les deux, sinon les deux domaines divergent — c'est précisément
# le défaut d'origine.
# -----------------------------------------------------------------------------

#' Incrémenter les epochs SC et Bulk (remplacement de jeu destructif)
#' @param global_data reactiveValues racine de l'application.
#' @return L'état, invisible.
.ts_bump_all_epochs <- function(global_data) {
  # Base R seule : ce fichier est sourcé AVANT les helpers de domaine, donc
  # `%||%` n'est pas garanti disponible ici.
  bump <- function(x) {
    if (is.null(x) || length(x) != 1L || is.na(x) || !is.numeric(x)) 0L else as.integer(x)
  }
  global_data$sc_obj_epoch   <- bump(global_data$sc_obj_epoch)   + 1L
  global_data$bulk_obj_epoch <- bump(global_data$bulk_obj_epoch) + 1L
  invisible(global_data)
}

# -----------------------------------------------------------------------------
# Domaine BULK — invalidation des résultats au changement de JEU DE DONNÉES
# (parité SC, audit 2026-09-27 §1.5 transposé au domaine bulk — handoff §4).
# Le schéma partagé bulk vivait à plat dans mod_bulk.R (shared_rv) ; il est
# consolidé ICI sur le modèle de create_sc_shared_state() pour que la purge
# soit testable hors application. Les champs ci-dessous sont des RÉSULTATS
# dérivés de l'objet courant : quand bulk_obj est remplacé par un nouveau jeu
# (imports mod_import_bulk.R / mod_geo.R, fusion mod_bulk_merge.R, activation
# d'un dataset enregistré), les laisser en place fait mélanger le jeu B avec
# les contrastes/voies/mapping du jeu A.
#
# L'invalidation est pilotée par un "epoch" (global_data$bulk_obj_epoch) que
# n'incrémentent QUE les remplacements de dataset. Les re-commits de même
# lignée (filtrage, correction ComBat, signatures, WGCNA, survie) ne purgent
# PAS — sinon chaque étape effacerait les résultats qu'elle vient de calculer.
# Les seuils et préférences d'affichage (lfc_thresh, pca_*, bulk_palette...)
# sont des réglages UTILISATEUR, pas des résultats : ils survivent au
# changement de jeu, comme active_tab / selected_genes côté SC.
# -----------------------------------------------------------------------------

#' Create the Bulk shared reactive state
#' @return A reactiveValues object with all Bulk module fields initialized.
create_bulk_shared_state <- function() {
  shiny::reactiveValues(
    counts_mapped = NULL, counts_original = NULL,
    mapping_applied = FALSE, mapping_summary = NULL,
    mapping_summary_data = NULL,
    filtered_counts = NULL, dds_blind = NULL, vst_mat = NULL,
    dds_full = NULL, contrasts = list(), active_contrast = NULL,
    pathway_results = NULL, pathway_scores = NULL,
    pathway_padj_method = NULL, active_tab = NULL,
    pca_color_by = NULL, pca_shape_by = NULL,
    pca_manual_colors = NULL,
    volcano_role_colors = NULL,
    active_condition_col = NULL,
    multimethod_de = NULL,
    multimethod_consensus = NULL,
    # BULK-1 (fix 2026-11-06) : les résultats des modules WGCNA/signatures/
    # pattern/dose/survie/réseau étaient écrits HORS schéma (créés à la volée)
    # et absents de bulk_shared_result_fields() — ils SURVIVAIENT au
    # changement de dataset et s'exportaient sous le jeu actif (mesuré 2026-
    # 11-05 : WGCNA ds1 2 000 gènes exporté pendant ds2 500×6). Déclarés ici
    # pour que la purge couvre un champ du schéma (même discipline que
    # de_bypass).
    wgcna_power = NULL, wgcna_modules = NULL,
    signature_scores = NULL, pattern_result = NULL,
    dose_result = NULL, survival_km = NULL, survival_cox = NULL,
    network_result = NULL, de_padj_ctx = NULL,
    # Chantier 7 (2026-11-08) : registre live du dernier export (rapport/
    # script) — purgé par epoch (bulk_shared_result_fields) pour ne pas
    # survivre à un ré-import (SP-FIND3).
    last_report = NULL,
    lfc_thresh = 1, padj_thresh = 0.05,
    heatmap_top_n = 30, heatmap_annot = NULL,
    pathway_db = "GOBP", pathway_mode = "ora", bulk_palette = "default",
    # BULK-6 (fix 2026-11-06) : validité de la DÉCLARATION du contraste DE.
    # FALSE = le jeu a été remplacé (bulk_obj_epoch) sans re-déclaration
    # explicite — la garde au RUN (mod_bulk_de_run.R) refuse alors avec le
    # banner de sync. N'EST PAS un champ résultat : ne va PAS dans
    # bulk_shared_result_fields() (une purge ne réarme pas la déclaration).
    # BULK-6 (fix 2026-11-06) : validité de la DÉCLARATION du contraste DE.
    # FALSE = le jeu a été remplacé (bulk_obj_epoch) sans re-déclaration
    # explicite — la garde au RUN (mod_bulk_de_run.R) refuse alors avec le
    # banner de sync. N'EST PAS un champ résultat : ne va PAS dans
    # bulk_shared_result_fields() (une purge ne réarme pas la déclaration).
    de_decl_valid = TRUE,
    # R-3bis (fix 2026-11-06) : miroir de l'onglet actif du navset bulk
    # (écrit par mod_bulk_server) — lu par les renders de résultats pour se
    # re-exécuter quand leur onglet devient visible (DT « empoisonnées »).
    main_tabs_current = NULL,
    # écrit dynamiquement par le pipeline auto (mod_bulk.R) en mode exploratoire
    # sans réplicat ; déclaré ici pour que la purge couvre un champ du schéma
    de_bypass = NULL
  )
}

#' Champs résultats du domaine Bulk (purgés au changement de dataset)
#'
#' BULK-1 (fix 2026-11-06) : la liste couvre maintenant TOUS les slots
#' résultats écrits par les modules bulk (WGCNA, signatures, pattern, dose,
#' survie, réseau PCSF, consensus multi-méthodes, scores de voies, mémo padj).
#' Les préférences d'UI (onglet actif, seuils, palettes, annotations heatmap)
#' n'y figurent PAS : elles survivent au changement de jeu par conception.
#'
#' @return Character vector of reactiveValues field names.
bulk_shared_result_fields <- function() {
  c("counts_mapped", "counts_original", "mapping_applied", "mapping_summary",
    "mapping_summary_data",
    "filtered_counts", "dds_blind", "vst_mat", "dds_full",
    "contrasts", "active_contrast", "active_condition_col",
    "pathway_results", "pathway_scores", "pathway_db", "pathway_mode",
    "pathway_padj_method", "multimethod_de", "multimethod_consensus",
    "network_result", "de_padj_ctx",
    "wgcna_power", "wgcna_modules", "signature_scores", "pattern_result",
    "dose_result", "survival_km", "survival_cox",
    "de_bypass",
    # Chantier 7 (2026-11-08) : registre live du dernier export — SP-FIND3 :
    # après ré-import (purge epoch) le rapport ne correspond plus à l'état,
    # la ligne retombe sur « Prêt » au lieu de mentir.
    "last_report")
}

#' Purger les champs résultats d'un état partagé Bulk
#' @param state reactiveValues (create_bulk_shared_state()).
#' @return The state, invisible.
bulk_purge_shared_results <- function(state) {
  for (fld in bulk_shared_result_fields()) state[[fld]] <- NULL
  invisible(state)
}

#' Create the Spatial shared reactive state
#'
#' Mirrors the per-dataset cache schema that mod_spatial.R snapshots into
#' global_data$spatial_results_cache (see mod_spatial.R header v10).
#' One instance per active dataset; the outer module snapshots/restores it
#' on dataset switch so each sample keeps its own QC/clusters/deconv/niches.
#'
#' @return A reactiveValues object with all Spatial module fields initialized.
create_spatial_shared_state <- function() {
  shiny::reactiveValues(
    qc_metrics = NULL,
    qc_pass_idx = NULL,
    qc_params = NULL,
    cluster_labels = NULL,
    cluster_params = NULL,
    deconv_props = NULL,
    deconv_params = NULL,
    moran_results = NULL,
    moran_params = NULL,
    niche_labels = NULL,
    niche_composition = NULL,
    niche_params = NULL,
    umap_df = NULL,
    saved_viz_list = list()
  )
}

# =============================================================================
# Contrat d'analyse generique — create_analysis_state() + validateurs de
# forme (is_analysis_state / assert_analysis_state) + accesseurs generiques
# state_get()/state_set()/state_has().
#
# Les fabriques ci-dessus decrivent le schema PARTAGE par domaine (consomme
# par les modules existants). create_analysis_state() decrit, lui, l'etat
# d'UNE analyse : les 5 emplacements du contrat transversal — c'est l'ancrage
# du manifeste de provenance (R/core/provenance.R, champ reserve
# "provenance") et le moule des futurs etats par-analyse.
#
# Les validateurs ne verifient QUE la forme generique du contrat (type
# reactif du projet + presence des 5 emplacements) : aucune validation
# Seurat/assay/reduction/matrice/design — cela relevera de
# R/core/validation.R.
#
# Les accesseurs sont GENERIQUES : ils fonctionnent sur n'importe quel
# reactiveValues (fabriques de domaine comme etats d'analyse). Les modules
# doivent y router leurs acces au lieu de piquer `$` directement, pour que
# les futures refactors puissent ajouter de la validation de maniere
# transparente.
# =============================================================================

# Emplacements requis du contrat (constante source de verite pour les
# validateurs ; le constructeur les initialise explicitement).
.ANALYSIS_STATE_SLOTS <- c("input", "preprocessing", "results", "visualization", "provenance")

# Noms d'un reactiveValues avec fallback isolate() hors contexte reactif
# (meme contrat que state_get() : dans cette version de shiny, names() sur
# un reactiveValues exige un contexte reactif via .namesDeps$register()).
.state_names_safe <- function(state) {
  tryCatch(names(state), error = function(e) shiny::isolate(names(state)))
}

#' Creer l'etat d'une analyse (contrat 5 emplacements)
#'
#' @param domain Domaine applicatif : "sc", "bulk" ou "spatial".
#' @return Un reactiveValues avec `input`, `preprocessing`, `results`,
#'   `visualization` pre-initialises a NULL, `provenance` a list(), et un
#'   attribut "domain" documentant le domaine.
create_analysis_state <- function(domain = c("sc", "bulk", "spatial")) {
  .domains <- c("sc", "bulk", "spatial")
  # Validation francaise AVANT match.arg (dont le message d'erreur est
  # en anglais) — le defaut multi-valeurs passe tel quel a match.arg().
  if (length(domain) != length(.domains) || !setequal(domain, .domains)) {
    if (length(domain) != 1L || is.na(domain) || !domain %in% .domains) {
      stop(sprintf(
        "create_analysis_state() : domaine '%s' inconnu (autorisés : sc, bulk, spatial).",
        paste(domain, collapse = ", ")
      ), call. = FALSE)
    }
  }
  domain <- match.arg(domain)
  rv <- shiny::reactiveValues(
    input         = NULL,
    preprocessing = NULL,
    results       = NULL,
    visualization = NULL,
    provenance    = list()
  )
  attr(rv, "domain") <- domain
  rv
}

#' L'objet est-il un etat d'analyse valide (forme generique) ?
#'
#' Verifie UNIQUEMENT la forme du contrat : type reactif du projet
#' (reactiveValues) et presence des 5 emplacements. Un etat avec des champs
#' supplementaires (ajout dynamique) reste valide.
#'
#' @param state Objet quelconque.
#' @return Logical.
is_analysis_state <- function(state) {
  shiny::is.reactivevalues(state) && all(.ANALYSIS_STATE_SLOTS %in% .state_names_safe(state))
}

#' Verifier la forme d'un etat d'analyse (garde developpeur)
#'
#' Garde STRUCTURELLE au message francais explicite (style assert_* de
#' validation.R) : n'accepte qu'un reactiveValues portant les 5 emplacements.
#'
#' @param state Etat issu de create_analysis_state() (ou tout objet).
#' @param context Contexte d'appel cite dans le message d'erreur.
#' @return L'etat, invisible (pipable, conforme au style assert_*).
assert_analysis_state <- function(state, context = "analyse") {
  if (!shiny::is.reactivevalues(state)) {
    stop(sprintf(
      "Echec %s : un etat d'analyse Shiny (reactiveValues) est requis (recu : %s).",
      context, if (is.null(state)) "NULL" else paste(class(state), collapse = "/")
    ), call. = FALSE)
  }
  slots_absents <- setdiff(.ANALYSIS_STATE_SLOTS, .state_names_safe(state))
  if (length(slots_absents) > 0L) {
    stop(sprintf(
      "Echec %s : emplacement(s) d'etat absent(s) : %s (requis : %s).",
      context, paste(slots_absents, collapse = ", "),
      paste(.ANALYSIS_STATE_SLOTS, collapse = ", ")
    ), call. = FALSE)
  }
  invisible(state)
}

#' Lire un champ d'etat (fallback isolé hors contexte reactif)
#'
#' Dans un contexte reactif (observer, reactive, render*) la lecture directe
#' est faite — la trace de dependance de reactiveValues est PRESERVEE
#' (comportement identique a `state$champ`). Hors contexte (scripts, tests),
#' la lecture directe d'un reactiveValues leve une erreur : on retombe alors
#' sur shiny::isolate() pour rester testable hors application.
#'
#' @param state reactiveValues (fabrique de domaine ou create_analysis_state()).
#' @param field Nom du champ (character).
#' @return La valeur du champ, NULL si jamais definie.
state_get <- function(state, field) {
  tryCatch(
    state[[field]],
    error = function(e) shiny::isolate(state[[field]])
  )
}

#' Ecrire un champ d'etat
#'
#' @param state reactiveValues.
#' @param field Nom du champ. Un champ absent du schema est ajoute
#'   dynamiquement (comportement reactiveValues standard).
#' @param value Valeur a stocker.
#' @return La valeur, invisible (pipable, conforme au style assert_*).
state_set <- function(state, field, value) {
  state[[field]] <- value
  invisible(value)
}

#' Le champ a-t-il une valeur non-NULL ?
#'
#' Convention du schema : les champs pre-initialises a NULL signifient
#' "pas encore calcule" — state_has() repond donc FALSE jusqu'a la premiere
#' valeur non-NULL (un character(0)/list() vide compte comme present).
#'
#' @param state reactiveValues.
#' @param field Nom du champ.
#' @return Logical.
state_has <- function(state, field) {
  !is.null(state_get(state, field))
}

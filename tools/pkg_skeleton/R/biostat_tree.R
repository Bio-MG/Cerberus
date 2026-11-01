# =============================================================================
# tools/pkg_skeleton/R/biostat_tree.R — Assistant biostatistique : arbre de
# décision actif, LOGIQUE PURE (M2 — décision O27 du 2026-10-29)
# =============================================================================
# CONTRAT GELÉ : docs/contracts/BIOSTAT_DECISION_CONTRACT.md (SPÉC GELÉE
# v1.0.0, M1). Ce fichier SUIT le contrat, il ne le modifie pas (§8 : toute
# divergence record/enum/invariants = bump de version + freeze test + doc).
# Proposition mère : docs/proposals/BIOSTAT_DECISION_TREE_PROPOSAL.md (§12 :
# 1 = GO · 2 = ORIENTATION SEULE · 3 = alpha 0.05 · 4 = accordéon Bulk DE).
#
# POURQUOI LOGIQUE PURE
#
# Aucun input$/output$/reactive() (C2/C17) : tout le calcul vit ici, testé
# hors Shiny par tests/testthat/test-biostat-tree.R (RED d'abord, 2026-11-01).
# L'UI accordéon (M3, décision §12.4) orchestre ; ce moteur descend l'arbre.
#
# ORIENTATION SEULE (O27 §12.2) : les feuilles cont_* rendent une
# ORIENTATION (structure gelée : method_id + clé i18n biostat.r.*), JAMAIS
# un lancement de test — aucun appel de::/t.test()/wilcox.test()/edgeR/DESeq2
# n'existe dans ce fichier, garde lexicale T2 du freeze. Les diagnostics
# Shapiro/Levene (contrat §4, seuils TS_BIOSTAT_* de thresholds.R:103-105)
# ne sont PAS exécutés ici : c'est la couche diagnostic, hors surface M2
# épinglée (entrée de l'arbre + accesseurs de recommandation) ; le record
# porte le champ `diagnostics` gelé (vide pour bulk_*/sc_da_delegate, §5-I4).
#
# ZÉRO DÉPENDANCE NOUVELLE : arbre en listes imbriquées R base. Mesure
# 2026-11-01 : data.tree est dans renv.lock (ligne 17784) mais PAS installé
# dans la bibliothèque active — l'implémentation ne dépend donc d'aucun
# paquet (renv.lock intouché, invariant proposition §0.2). Le champ
# `children` (liste) reste lisible par un parcours data.tree si un jour le
# squelette migre.
#
# PROVENANCE 4F (contrat §1) : chaque recommandation se sérialise par le
# canal EXISTANT new_provenance_entry() / provenance_append()
# (tools/pkg_skeleton/R/provenance.R, sourcé par app.R bloc 03) :
#   entry <- new_provenance_entry(
#     analysis_id = "biostat_decision",
#     method      = paste0("decision_tree_v", biostat_tree_version()),
#     parameters  = list(node_path = rec$node_path, method_id = rec$method_id,
#                        engine = rec$engine, rationale = rec$rationale_keys,
#                        diagnostics = rec$diagnostics, prefilled = rec$prefilled),
#     dataset     = global_data$bulk_obj)
# Composée et vérifiée par T4(d) de test-biostat-tree.R (règle 7 du rapport
# consolidé : PRODUITE, jamais reconstruite ; I2 : user_confirmed = FALSE
# n'écrit RIEN — l'append n'appartient qu'à la couche module, M3/M4).
#
# I18N (§5-I7) : clés uniquement, jamais de texte brut. Les clés
# biostat.q.* / biostat.a.* / biostat.r.* stockées ici sont des DONNÉES
# (aucun appel tr() dans ce fichier — la résolution des libellés se fait au
# rendu, M3) ; les paires {fr, en} atterrissent dans i18n/translation.json
# via tools/add_i18n_keys.R quand la couche UI en a besoin.
# =============================================================================

biostat_tree_version <- function() "1.0.0"

# --- Enum method_id FERMÉ (contrat §3 — 13 valeurs, aucune autre) -----------
.biostat_method_ids <- c(
  "bulk_deseq2_wald",   # DESeq2 Wald (condition à 2 niveaux)
  "bulk_deseq2_lrt",    # DESeq2 LRT (condition >= 3 niveaux)
  "bulk_edger",         # edgeR, réplicats présents
  "bulk_edger_norep",   # edgeR, dispersion imposée (0 réplicat — exploratoire)
  "bulk_voom",          # limma-voom (designs complexes, covariables continues)
  "cont_ttest",         # t de Student, 2 groupes indép.
  "cont_welch",         # Welch (variances inégales)
  "cont_paired_ttest",  # t apparié
  "cont_wilcoxon",      # Mann-Whitney
  "cont_signed_rank",   # Wilcoxon signé
  "cont_anova",         # ANOVA + post-hoc rstatix
  "cont_kruskal",       # Kruskal-Wallis
  "sc_da_delegate"      # délégation au design DA (Milo / scCodA)
)

# =============================================================================
# biostat_build_tree() — ENTRÉE de l'arbre (proposition §1.2)
#
# Nœud = liste à champs gelés par la fiche : name, id, type
# ("question" | "method"), i18n_key, answers (nommé : clé de réponse ->
# nom du nœud enfant), answers_i18n (clé de réponse -> clé biostat.a.*),
# method_id, rationale_key (clé biostat.r.*), children.
#
# CONVENTION MESURÉE : un nœud feuille est NOMMÉ par son method_id — le
# contrat §2 fige « dernier élément de node_path = feuille method_id » et
# l'exemple de la proposition §5 (root > data_counts > replicates_ok >
# groups_three > bulk_deseq2_lrt) est reproduit à l'identique.
#
# AUCUN seuil codé en dur ici : les seuils de diagnostic vivent
# EXCLUSIVEMENT dans thresholds.R (bloc TS_BIOSTAT_*, gelé M1) et ne sont
# lus par aucune ligne de ce fichier tant que la couche diagnostic n'atterrit.
# =============================================================================
biostat_build_tree <- function() {
  list(
    name = "root",
    id = "data_family",
    type = "question",
    i18n_key = "biostat.q.data_family",
    answers = c(counts = "data_counts", continuous = "data_continuous",
                compositions = "compositions"),
    answers_i18n = c(counts = "biostat.a.data_bulk_counts",
                     continuous = "biostat.a.data_continuous_scores",
                     compositions = "biostat.a.data_compositions"),
    method_id = NULL,
    rationale_key = NULL,
    children = list(
      # ── Branche COMPTAGES BULK (hypothèse binomiale négative — §10 : aucun
      #    test de normalité sur cette branche, jamais) ──────────────────────
      list(
        name = "data_counts",
        id = "replicates_ok",
        type = "question",
        i18n_key = "biostat.q.bulk_replicates",
        answers = c(ok = "replicates_ok", none = "bulk_edger_norep"),
        answers_i18n = c(ok = "biostat.a.replicates_ok",
                         none = "biostat.a.replicates_none"),
        method_id = NULL,
        rationale_key = NULL,
        children = list(
          list(
            name = "bulk_edger_norep",
            id = "bulk_edger_norep",
            type = "method",
            i18n_key = NULL,
            answers = NULL,
            answers_i18n = NULL,
            method_id = "bulk_edger_norep",
            rationale_key = "biostat.r.edger_norep",
            children = list()
          ),
          list(
            name = "replicates_ok",
            id = "groups_n",
            type = "question",
            i18n_key = "biostat.q.groups_n",
            answers = c(two = "groups_two", three_plus = "groups_three"),
            answers_i18n = c(two = "biostat.a.groups_two",
                             three_plus = "biostat.a.groups_three_plus"),
            method_id = NULL,
            rationale_key = NULL,
            children = list(
              # Voie >= 3 niveaux : le noeud intermédiaire "groups_three"
              # reproduit l'exemple de node_path du contrat (proposition §5 :
              # root > data_counts > replicates_ok > groups_three >
              # bulk_deseq2_lrt) ; la feuille est NOMMÉE par son method_id.
              list(
                name = "groups_three",
                id = "bulk_lrt_confirm",
                type = "question",
                i18n_key = "biostat.q.bulk_lrt_confirm",
                answers = c(confirm = "bulk_deseq2_lrt"),
                answers_i18n = c(confirm = "biostat.a.confirm"),
                method_id = NULL,
                rationale_key = NULL,
                children = list(
                  list(
                    name = "bulk_deseq2_lrt",
                    id = "bulk_deseq2_lrt",
                    type = "method",
                    i18n_key = NULL,
                    answers = NULL,
                    answers_i18n = NULL,
                    method_id = "bulk_deseq2_lrt",
                    rationale_key = "biostat.r.lrt",
                    children = list()
                  )
                )
              ),
              list(
                name = "groups_two",
                id = "covariates",
                type = "question",
                i18n_key = "biostat.q.bulk_covariates",
                answers = c(none = "cov_none", continuous = "cov_continuous"),
                answers_i18n = c(none = "biostat.a.cov_none",
                                 continuous = "biostat.a.cov_continuous"),
                method_id = NULL,
                rationale_key = NULL,
                children = list(
                  # Feuille NOMMÉE par son method_id (contrat §2 : dernier
                  # élément de node_path = feuille method_id).
                  list(
                    name = "bulk_voom",
                    id = "bulk_voom",
                    type = "method",
                    i18n_key = NULL,
                    answers = NULL,
                    answers_i18n = NULL,
                    method_id = "bulk_voom",
                    rationale_key = "biostat.r.voom",
                    children = list()
                  ),
                  list(
                    name = "cov_none",
                    id = "engine_pref",
                    type = "question",
                    i18n_key = "biostat.q.engine_pref",
                    answers = c(deseq2 = "bulk_deseq2_wald",
                                edger = "bulk_edger"),
                    answers_i18n = c(deseq2 = "biostat.a.engine_deseq2",
                                     edger = "biostat.a.engine_edger"),
                    method_id = NULL,
                    rationale_key = NULL,
                    children = list(
                      list(
                        name = "bulk_deseq2_wald",
                        id = "bulk_deseq2_wald",
                        type = "method",
                        i18n_key = NULL,
                        answers = NULL,
                        answers_i18n = NULL,
                        method_id = "bulk_deseq2_wald",
                        rationale_key = "biostat.r.wald",
                        children = list()
                      ),
                      list(
                        name = "bulk_edger",
                        id = "bulk_edger",
                        type = "method",
                        i18n_key = NULL,
                        answers = NULL,
                        answers_i18n = NULL,
                        method_id = "bulk_edger",
                        rationale_key = "biostat.r.edger",
                        children = list()
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      # ── Branche CONTINUE (scores de signature, phénotypes, métriques par
      #    lot) — orientation seule : le verdict Shapiro/Levene arrive de la
      #    couche diagnostic en réponse, jamais d'exécution ici ──────────────
      list(
        name = "data_continuous",
        id = "groups_c",
        type = "question",
        i18n_key = "biostat.q.groups_n",
        answers = c(two = "cont_groups_two", three_plus = "cont_groups_three"),
        answers_i18n = c(two = "biostat.a.groups_two",
                         three_plus = "biostat.a.groups_three_plus"),
        method_id = NULL,
        rationale_key = NULL,
        children = list(
          list(
            name = "cont_groups_three",
            id = "param_three",
            type = "question",
            i18n_key = "biostat.q.parametric",
            answers = c(ok = "cont_anova", ko = "cont_kruskal"),
            answers_i18n = c(ok = "biostat.a.parametric_ok",
                             ko = "biostat.a.parametric_ko"),
            method_id = NULL,
            rationale_key = NULL,
            children = list(
              list(
                name = "cont_anova",
                id = "cont_anova",
                type = "method",
                i18n_key = NULL,
                answers = NULL,
                answers_i18n = NULL,
                method_id = "cont_anova",
                rationale_key = "biostat.r.anova",
                children = list()
              ),
              list(
                name = "cont_kruskal",
                id = "cont_kruskal",
                type = "method",
                i18n_key = NULL,
                answers = NULL,
                answers_i18n = NULL,
                method_id = "cont_kruskal",
                rationale_key = "biostat.r.kruskal",
                children = list()
              )
            )
          ),
          list(
            name = "cont_groups_two",
            id = "paired",
            type = "question",
            i18n_key = "biostat.q.paired",
            answers = c(paired = "cont_paired", independent = "cont_indep"),
            answers_i18n = c(paired = "biostat.a.paired",
                             independent = "biostat.a.independent"),
            method_id = NULL,
            rationale_key = NULL,
            children = list(
              list(
                name = "cont_paired",
                id = "param_paired",
                type = "question",
                i18n_key = "biostat.q.parametric",
                answers = c(ok = "cont_paired_ttest",
                            ko = "cont_signed_rank"),
                answers_i18n = c(ok = "biostat.a.parametric_ok",
                                 ko = "biostat.a.parametric_ko"),
                method_id = NULL,
                rationale_key = NULL,
                children = list(
                  list(
                    name = "cont_paired_ttest",
                    id = "cont_paired_ttest",
                    type = "method",
                    i18n_key = NULL,
                    answers = NULL,
                    answers_i18n = NULL,
                    method_id = "cont_paired_ttest",
                    rationale_key = "biostat.r.paired_ttest",
                    children = list()
                  ),
                  list(
                    name = "cont_signed_rank",
                    id = "cont_signed_rank",
                    type = "method",
                    i18n_key = NULL,
                    answers = NULL,
                    answers_i18n = NULL,
                    method_id = "cont_signed_rank",
                    rationale_key = "biostat.r.signed_rank",
                    children = list()
                  )
                )
              ),
              list(
                name = "cont_indep",
                id = "param_two",
                type = "question",
                i18n_key = "biostat.q.parametric",
                answers = c(ok = "var_check", ko = "cont_wilcoxon"),
                answers_i18n = c(ok = "biostat.a.parametric_ok",
                                 ko = "biostat.a.parametric_ko"),
                method_id = NULL,
                rationale_key = NULL,
                children = list(
                  list(
                    name = "cont_wilcoxon",
                    id = "cont_wilcoxon",
                    type = "method",
                    i18n_key = NULL,
                    answers = NULL,
                    answers_i18n = NULL,
                    method_id = "cont_wilcoxon",
                    rationale_key = "biostat.r.wilcoxon",
                    children = list()
                  ),
                  list(
                    name = "var_check",
                    id = "variances",
                    type = "question",
                    i18n_key = "biostat.q.variances",
                    answers = c(equal = "cont_ttest",
                                unequal = "cont_welch"),
                    answers_i18n = c(equal = "biostat.a.variances_equal",
                                     unequal = "biostat.a.variances_unequal"),
                    method_id = NULL,
                    rationale_key = NULL,
                    children = list(
                      list(
                        name = "cont_ttest",
                        id = "cont_ttest",
                        type = "method",
                        i18n_key = NULL,
                        answers = NULL,
                        answers_i18n = NULL,
                        method_id = "cont_ttest",
                        rationale_key = "biostat.r.ttest",
                        children = list()
                      ),
                      list(
                        name = "cont_welch",
                        id = "cont_welch",
                        type = "method",
                        i18n_key = NULL,
                        answers = NULL,
                        answers_i18n = NULL,
                        method_id = "cont_welch",
                        rationale_key = "biostat.r.welch",
                        children = list()
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      # ── Branche COMPOSITIONS (Milo / scCodA) — I5 : délégation PURE, l'état
      #    affiché est celui de validate_da_design(), aucune logique ajoutée ──
      list(
        name = "compositions",
        id = "sc_da_confirm",
        type = "question",
        i18n_key = "biostat.q.sc_delegate",
        answers = c(confirm = "sc_da_delegate"),
        answers_i18n = c(confirm = "biostat.a.sc_delegate_confirm"),
        method_id = NULL,
        rationale_key = NULL,
        children = list(
          list(
            name = "sc_da_delegate",
            id = "sc_da_delegate",
            type = "method",
            i18n_key = NULL,
            answers = NULL,
            answers_i18n = NULL,
            method_id = "sc_da_delegate",
            rationale_key = "biostat.r.sc_delegate",
            children = list()
          )
        )
      )
    )
  )
}

# =============================================================================
# biostat_resolve(tree, answers) — DESCENTE déterministe vers la feuille
#
# @param tree    arbre produit par biostat_build_tree()
# @param answers liste/vecteur NOMMÉ : id de question -> clé de réponse
#   (clé de réponse = nom de l'entrée `answers` du nœud question)
# @return liste DÉTERMINISTE (aucun horodatage, aucun aléatoire — contrat §1 :
#   seed non applicable) : node_path (contrat §2 : "root" en tête, feuille
#   method_id en dernier, sans doublon consécutif), method_id, rationale_key,
#   answers (carte réellement consommée), tree_version.
#   Erreurs TYPOÉES du vocabulaire gelé §6 : biostat_tree_missing_answer
#   (réponse absente ou non reconnue) — refus structuré, jamais d'exception
#   brute non classée (C10).
# =============================================================================
biostat_resolve <- function(tree, answers) {
  answers <- as.list(answers)
  if (!is.list(tree) || is.null(tree$name) || !identical(tree$name, "root")) {
    stop("biostat_resolve() : 'tree' doit provenir de biostat_build_tree() ",
         "(racine 'root').", call. = FALSE)
  }
  node <- tree
  path <- character(0)
  used <- list()
  repeat {
    path <- c(path, node$name)
    mid <- node$method_id
    if (!is.null(mid) && length(mid) == 1L && !is.na(mid) && nzchar(mid)) {
      return(list(
        node_path     = path,
        method_id     = mid,
        rationale_key = node$rationale_key,
        answers       = used,
        tree_version  = biostat_tree_version()
      ))
    }
    ans <- node$answers
    if (is.null(ans) || !length(ans) || is.null(names(ans))) {
      stop("biostat_resolve() : noeud question sans champ 'answers' nomme — ",
           "arbre mal forme au noeud '", node$name, "'.", call. = FALSE)
    }
    nid <- if (!is.null(node$id) && length(node$id) == 1L && !is.na(node$id)) {
      node$id
    } else {
      node$name
    }
    choice <- answers[[nid]]
    if (is.null(choice) || length(choice) != 1L || is.na(choice) ||
        !choice %in% names(ans)) {
      stop(errorCondition(
        sprintf(paste0(
          "biostat_resolve() : reponse absente ou non reconnue pour la ",
          "question '%s' (reponses attendues : %s) — contrat §6 : refus ",
          "structure type biostat_tree_missing_answer, jamais d'exception ",
          "brute."),
          nid, paste(names(ans), collapse = ", ")),
        class = "biostat_tree_missing_answer",
        node = nid,
        expected = names(ans)
      ))
    }
    used[[nid]] <- choice
    target <- ans[[choice]]
    child <- NULL
    for (k in node$children) {
      if (identical(k$name, target) || identical(k$id, target)) {
        child <- k
        break
      }
    }
    if (is.null(child)) {
      stop("biostat_resolve() : branche '", target, "' introuvable sous le ",
           "noeud '", node$name, "' — arbre mal forme.", call. = FALSE)
    }
    node <- child
  }
}

# =============================================================================
# biostat_decision_record() — RECORD de recommandation canonique (contrat §2)
#
# Champs gelés, DANS L'ORDRE du contrat : version, node_path, method_id,
# engine, rationale_keys, diagnostics, prefilled, user_confirmed, timestamp.
# method_id hors enum §3 -> erreur TYPOÉE biostat_tree_unknown_method (§6).
# engine : moteur DÉCLARÉ par l'utilisateur pour les feuilles bulk_* ;
# normalisé à "" pour toute feuille non bulk (contrat §2 : « chaîne vide
# sinon »). diagnostics : liste vide OBLIGATOIRE pour bulk_*/sc_da_delegate
# (§5-I4 — jamais de test paramétrique sur des comptages).
# =============================================================================
biostat_decision_record <- function(node_path, method_id, engine = "",
                                    rationale_keys = character(0),
                                    diagnostics = list(),
                                    prefilled = list(groups = "two",
                                                     paired = FALSE),
                                    user_confirmed = FALSE,
                                    timestamp = Sys.time()) {
  # --- enum §3 FERMÉ (contrat §6, ligne 2) — validé EN PREMIER ---------------
  if (missing(method_id) || is.null(method_id) || !is.character(method_id) ||
      length(method_id) != 1L || is.na(method_id) || !nzchar(method_id) ||
      !method_id %in% .biostat_method_ids) {
    stop(errorCondition(
      sprintf(paste0(
        "biostat_decision_record() : method_id '%s' hors de l'enum ferme §3 ",
        "du contrat BIOSTAT_DECISION_CONTRACT.md (13 valeurs) — refus ",
        "structure type biostat_tree_unknown_method."),
        if (is.character(method_id) && length(method_id) >= 1L) method_id[[1L]]
        else paste(class(method_id), collapse = ",")
      ),
      class = "biostat_tree_unknown_method"
    ))
  }
  # --- node_path (contrat §2 : >= 2, "root" en tête, feuille en dernier,
  #     sans doublon consécutif) — garde de programmation, C10 via call.=FALSE
  if (missing(node_path) || !is.character(node_path) ||
      length(node_path) < 2L || any(is.na(node_path)) ||
      !identical(node_path[[1L]], "root") ||
      !identical(node_path[[length(node_path)]], method_id) ||
      any(node_path[-1L] == node_path[-length(node_path)])) {
    stop("biostat_decision_record() : node_path invalide — le contrat §2 fige ",
         ": vecteur character de longueur >= 2, premier element 'root', ",
         "dernier element = feuille method_id, sans doublon consecutif.",
         call. = FALSE)
  }
  # --- engine (contrat §2) ----------------------------------------------------
  if (!grepl("^bulk_", method_id)) {
    engine <- "" # « chaîne vide sinon » — normalisation gelée
  }
  engine <- as.character(engine)
  if (length(engine) != 1L || is.na(engine)) {
    stop("biostat_decision_record() : 'engine' doit etre une chaine unique ",
         "(moteur DECLARE par l'utilisateur pour les feuilles bulk_*).",
         call. = FALSE)
  }
  # --- rationale_keys (contrat §2 / §5-I7 : clés biostat.r.* uniquement) -----
  rationale_keys <- as.character(rationale_keys)
  if (length(rationale_keys) &&
      !all(grepl("^biostat\\.r\\.", rationale_keys))) {
    stop("biostat_decision_record() : rationale_keys doit ne contenir que des ",
         "cles i18n biostat.r.* — jamais de texte brut (contrat §5-I7).",
         call. = FALSE)
  }
  # --- diagnostics (contrat §2 / §5-I4) ---------------------------------------
  if (!is.list(diagnostics)) {
    stop("biostat_decision_record() : 'diagnostics' doit etre une liste.",
         call. = FALSE)
  }
  if (length(diagnostics) > 0L &&
      (grepl("^bulk_", method_id) || identical(method_id, "sc_da_delegate"))) {
    stop("biostat_decision_record() : diagnostics doit rester VIDE pour toute ",
         "feuille bulk_* et sc_da_delegate (contrat §5-I4 : jamais de test ",
         "parametrique sur des comptages, hypothese binomiale negative).",
         call. = FALSE)
  }
  # --- prefilled (contrat §2 : list(groups, paired)) ---------------------------
  if (!is.list(prefilled) || is.null(names(prefilled)) ||
      !all(c("groups", "paired") %in% names(prefilled)) ||
      !identical(prefilled$groups %in% c("two", "three_plus"), TRUE) ||
      !is.logical(prefilled$paired) || length(prefilled$paired) != 1L ||
      is.na(prefilled$paired)) {
    stop("biostat_decision_record() : 'prefilled' doit etre une liste nommee ",
         "list(groups = 'two'|'three_plus', paired = <booleen>) (contrat §2, ",
         "valeurs deduites des metadonnees).", call. = FALSE)
  }
  # --- user_confirmed (contrat §2 / I2 : pas de validation, pas de trace) -----
  if (!is.logical(user_confirmed) || length(user_confirmed) != 1L ||
      is.na(user_confirmed)) {
    stop("biostat_decision_record() : 'user_confirmed' doit etre un ",
         "booleen scalaire (TRUE uniquement apres clic 'Valider ce choix').",
         call. = FALSE)
  }
  timestamp <- as.POSIXct(timestamp)

  # ORDRE DES CHAMPS GELÉ (contrat §2 — le freeze §7 épingle noms, ordre, types)
  list(
    version        = biostat_tree_version(),
    node_path      = node_path,
    method_id      = method_id,
    engine         = engine,
    rationale_keys = rationale_keys,
    diagnostics    = diagnostics,
    prefilled      = prefilled,
    user_confirmed = user_confirmed,
    timestamp      = timestamp
  )
}

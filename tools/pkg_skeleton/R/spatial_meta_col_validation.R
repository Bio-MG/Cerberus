# =============================================================================
# tools/pkg_skeleton/R/spatial_meta_col_validation.R — SPMV-1 : composant
# unique de validation des colonnes de métadonnées Spatial (O-SPMV)
# =============================================================================
# Fiche (fait foi) : docs/proposals/SPATIAL_META_COL_VALIDATION_PROPOSAL.md
# (2026-10-26, arbitrage GO sur recommandation retenue, opérateur 2026-11-02).
#
# CONTRAT (fiche §4a :92-97) — UNE fonction, consommée par le sélecteur
# « Colonne 'type cellulaire' » de la déconvolution de référence
# (modules/spatial/deconv/mod_spatial_deconv_reference.R, §4d) ; réutilisable
# ensuite par SC/Bulk (extension hors périmètre initial, §4a :95-97) :
#
#   spatial_meta_col_validation(meta_data, col)
#     -> list(level      = "ok" | "warn" | "danger",
#             reason_key = <chaîne fr = clé i18n (translation.json), "" si ok>,
#             detail     = <liste nommée de mesures>)
#
# L'outil VALIDE et EXPLIQUE — le fallback silencieux démontré en §2d-1
# (mod_spatial_deconv_reference.R:225-228 au dépôt : « else cols » sans
# avertissement) devient un verdict explicite. Il NE TRANSFORME ni ne corrige
# AUCUNE donnée (fiche §5 :132-133) : entrée lue, verdict rendu, rien d'autre.
#
# TABLE DE CLASSIFICATION (§4b :101-106) — ordre déterministe, du plus
# bloquant au moins bloquant (danger > warn > ok) :
#   danger : colonne absente des métadonnées (verdict explicite, jamais un
#            fallback) ; 1 seul niveau (ou aucune valeur exploitable) ;
#            numérique continue (> 20 valeurs distinctes) ; nunique == n
#            (identifiant par cellule, scénario E2).
#   warn   : numérique à peu de modalités (≤ 20 — non catégoriel au sens
#            §4b qui réserve « ok » à character/factor/logical) ; plus de
#            100 niveaux (bande 101-200 du feedback + au-delà, cas atlas —
#            même verdict warn, la mesure exacte est dans detail) ; nom
#            suspect : regex §4b EXACTE, insensible à la casse
#            (sample|donor|patient|batch|lane|run|orig\.ident|barcode|
#             cell[_ ]?id|spot|uuid|(^|_)id) — scénario E1 (donor_id) ;
#            un niveau couvre >= 99 % des cellules.
#   ok     : catégoriel (character/factor/logical), 2-100 niveaux, nom sain.
#
# Les niveaux sont comptés sur les valeurs NON-NA (un identifiant à NA ne
# débite pas plus de niveaux) ; colonne entièrement NA = 0 niveau exploitable
# -> danger. Cas non couverts par la fiche (Date/POSIX, hors contrat §4b) :
# traités par leurs branches génériques, documentés ici — le composant ne
# devine pas au-delà de la table.
#
# Les reason_key sont des CHAÎNES FRANÇAISES (convention clés = fr du dépôt,
# motif test-asr3-ui.R:69) appendues à i18n/translation.json par le mécanisme
# SCOPÉ (leçon B1 — tool au lot seul puis restauré). Le RENDU (badge, bandeau,
# case « Je confirme ») est le jalon SPMV-2 (fiche §6 :140) — ce fichier ne
# rend RIEN, il juge.
#
# Zéro dépendance nouvelle ; ASCII-safe hors commentaires (clés sans accents,
# motif des clés du domaine spatial). Test : tests/testthat/
# test-spmv1-meta-col-validation.R (RED mesuré avant implémentation :
# failed=14 sur arbre sans ce fichier).
# =============================================================================

# Clés i18n (fr = clé) — paires appendues SCOPÉ dans i18n/translation.json.
.spmv_key_absent <- "Colonne absente des metadonnees : la validation ne peut pas porter sur cette selection."
.spmv_key_single <- "Colonne a un seul niveau : aucune categorie exploitable pour une reference."
.spmv_key_per_cell <- "Identifiant par cellule (toutes les valeurs sont uniques) : colonne de type barcode/ID, pas un type cellulaire."
.spmv_key_continuous <- "Colonne numerique continue (plus de 20 valeurs distinctes) : ce n'est pas une categorie."
.spmv_key_numeric_few <- "Colonne numerique a peu de modalites : si c'est une categorie, preferez un factor/character."
.spmv_key_many_levels <- "Colonne a plus de 100 niveaux : au-dela des types cellulaires usuels — plausible pour un atlas a types fins, a verifier."
.spmv_key_name <- "Nom de colonne suspect (donneur / echantillon / lot / barcode...) : probablement pas un type cellulaire."
.spmv_key_dominant <- "Un seul niveau couvre au moins 99 % des cellules : la colonne ne discrimine presque rien."

# Regex §4b :101-104 EXACTE (insensible à la casse) — motifs d'identifiants
# de donneur / échantillon / lot / barcode.
.spmv_suspect_pattern <- "sample|donor|patient|batch|lane|run|orig\\.ident|barcode|cell[_ ]?id|spot|uuid|(^|_)id"

spatial_meta_col_validation <- function(meta_data, col) {
  # ── 0) Colonne absente : verdict EXPLICITE (jamais de fallback silencieux,
  #      §2d-1/§4c) — le danger dit « ne peut pas s'exécuter ». ---------------
  if (!col %in% names(meta_data)) {
    return(list(
      level      = "danger",
      reason_key = .spmv_key_absent,
      detail     = list(column = col, n_columns = ncol(meta_data))
    ))
  }

  # ── Mesures (lecture seule — AUCUNE écriture dans meta_data, test T5) -----
  x      <- meta_data[[col]]
  v      <- x[!is.na(x)]
  n_val  <- length(v)
  n_lev  <- length(unique(v))

  # ── danger : 1 seul niveau (ou plus aucune valeur exploitable) ------------
  if (n_lev <= 1L) {
    return(list(
      level      = "danger",
      reason_key = .spmv_key_single,
      detail     = list(n_levels = n_lev, n_cells = n_val)
    ))
  }

  # ── danger : numérique continue (> 20 valeurs distinctes, §4b) — E3 -------
  #      AVANT le test nunique == n : un score continu à valeurs toutes
  #      distinctes s'explique par son TYPE, pas par sa cardinalité.
  if (is.numeric(x) && n_lev > 20L) {
    return(list(
      level      = "danger",
      reason_key = .spmv_key_continuous,
      detail     = list(n_unique = n_lev)
    ))
  }

  # ── danger : identifiant par cellule (nunique == n, §4b) — E2 -------------
  if (n_lev == n_val) {
    return(list(
      level      = "danger",
      reason_key = .spmv_key_per_cell,
      detail     = list(n_unique = n_lev, n_cells = n_val)
    ))
  }

  # ── warn : numérique à peu de modalités — non catégoriel au sens §4b ------
  #      (« ok » est réservé à character/factor/logical) mais pas continu.
  if (is.numeric(x)) {
    return(list(
      level      = "warn",
      reason_key = .spmv_key_numeric_few,
      detail     = list(n_unique = n_lev)
    ))
  }

  # ── warn : plus de 100 niveaux (borne « types cellulaires » du feedback,
  #      §4b :107-110 ; la mesure exacte vit dans detail) ---------------------
  if (n_lev > 100L) {
    return(list(
      level      = "warn",
      reason_key = .spmv_key_many_levels,
      detail     = list(n_levels = n_lev)
    ))
  }

  # ── warn : nom suspect (scénario E1 — donor_id passe la fenêtre) ----------
  m <- regmatches(col, regexpr(.spmv_suspect_pattern, col,
                               ignore.case = TRUE, perl = TRUE))
  if (length(m) == 1L && nzchar(m)) {
    return(list(
      level      = "warn",
      reason_key = .spmv_key_name,
      detail     = list(column = col, matched_pattern = m)
    ))
  }

  # ── warn : un niveau couvre >= 99 % des cellules (§4b) ---------------------
  tbl  <- table(v)
  top  <- max(tbl)
  if (top / n_val >= 0.99) {
    return(list(
      level      = "warn",
      reason_key = .spmv_key_dominant,
      detail     = list(top_level    = names(tbl)[which.max(tbl)],
                        share_percent = round(100 * top / n_val, 1))
    ))
  }

  # ── ok : catégoriel, 2-100 niveaux, nom sain (badge discret « N classes ») -
  list(level = "ok", reason_key = "", detail = list(n_levels = n_lev))
}

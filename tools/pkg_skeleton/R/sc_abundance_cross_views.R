# =============================================================================
# R/sc/sc_abundance_cross_views.R — Abondance differentielle 4E-3 : vues
# croisées Milo x scCODA (Stage 16)
# =============================================================================
# BUT : interpretation COTE A COTE des deux methodes SANS fusionner leurs
# significations :
#   - Milo (4E-1)  : DA PAR VOISINAGE (regions d'un graphe kNN, NB GLM,
#     SpatialFDR) — un signal de region, PAS un DA par type cellulaire ;
#   - scCODA (4E-2): DA COMPOSITIONNELLE PAR ECHANTILLON (modele bayesien
#     spike-and-slab, effets credibles) — un effet par identite a l'echelle
#     de l'echantillon.
# La comparaison est DESCRIPTIVE et SENSIBLE A LA METHODE :
#   - AUCUN "p-value de consensus" poolé, aucune meta-analyse ;
#   - chaque identite recoit une CATEGORIE de concordance par des REGLES
#     EXPLICITES (§ concordance), jamais un score composite ;
#   - les desaccords sont EXPOSES (milo_only / sccoda_only /
#     discordant_direction), pas moyennes ;
#   - chaque vue porte le contexte niveau ECHANTILLON (les cellules ne sont
#     pas des replicats) et les references des deux methodes.
#
# Consommateurs PURES des deux resultats canoniques : assert_milo_result +
# assert_sccoda_result ; aucune re-execution, aucune re-inference ; les
# options d'affichage (seuil SpatialFDR, fraction minimale de voisinages
# significatifs) sont enregistrees dans la provenance au moment de l'export.
#
# ── REGLES DE CONCORDANCE (explicites, par identite) ────────────────────────
# Cote Milo   : voisinages annotes a l'identite (DA_table$identity, hors NA) ;
#               "signal" = fraction de ces voisinages avec SpatialFDR <
#               TS_DA_MILO_DISPLAY_ALPHA >= TS_DA_CROSS_SIGNIF_FRACTION ;
#               direction = signe de la mediane des logFC (cible vs reference).
# Cote scCODA : effet de la covariable condition ; "signal" = credible TRUE au
#               seuil fdr_target du resultat ; direction = signe de l'effet.
# Categorie :
#   concordant_enriched_target    — signal des deux cotes, directions egales (+)
#   concordant_enriched_reference — signal des deux cotes, directions egales (-)
#   discordant_direction          — signal des deux cotes, directions opposees
#   milo_only                     — signal Milo seul
#   sccoda_only                   — signal scCODA seul
#   no_signal                     — aucun signal
#   not_comparable                — identite absente d'un cote
# Ces categories sont DESCRIPTIVES : un desaccord peut provenir des echelles
# (voisinage vs echantillon), des effectifs, ou d'une vraie biologie
# differenciellement captée — le resultat ne tranche pas.
#
# Pur domaine : aucune reactivite Shiny. Sourced in app.R AFTER
# R/sc/sc_abundance_sccoda_views.R. Constante TS_DA_CROSS_SIGNIF_FRACTION dans
# config/defaults.R ; le seuil SpatialFDR est REUTILISE (TS_DA_MILO_DISPLAY_ALPHA).
#
# ── API PUBLIQUE FIGEE (Stage 16) ───────────────────────────────────────────
# da_cross_views_public_api() ; test de freeze
# (tests/testthat/test-da-cross-views-contract-freeze.R). Helpers internes
# prefixes d'un point (.dacross_*).
# =============================================================================

#' Surface publique figee de R/sc/sc_abundance_cross_views.R (Stage 16)
#'
#' @return Vecteur character des noms de fonctions publiques.
#' @export
da_cross_views_public_api <- function() {
  c(
    "da_cross_views_public_api",
    "build_da_cross_method_summary",
    "build_da_cross_concordance",
    "build_da_cross_provenance",
    "build_da_cross_concordance_export",
    "da_cross_export_filename",
    "da_cross_concordance_categories",
    "plot_da_cross_sample_composition",
    "plot_da_cross_concordance",
    "plot_da_cross_nhood_mapping"
  )
}

#' Erreur d'une vue croisee (classe du domaine, etat structure)
.dacross_stop <- function(message) {
  stop(errorCondition(
    message,
    state = "invalid_input",
    class = "da_cross_error"
  ))
}

#' Categories de concordance figees (regles explicites, jamais un score)
#'
#' @return Vecteur character des 7 categories.
#' @export
da_cross_concordance_categories <- function() {
  c(
    "concordant_enriched_target",
    "concordant_enriched_reference",
    "discordant_direction",
    "milo_only",
    "sccoda_only",
    "no_signal",
    "not_comparable"
  )
}

#' Compatibilite des deux resultats (pure, testable sans calcul)
#'
#' La comparaison n'est PLEINE que si les deux analyses portent sur le meme
#' objet (empreintes identiques), le meme contraste et la meme colonne
#' d'identite ; tout ecart est EXPOSE (jamais masque, jamais bloque : la
#' lecture croisee reste possible mais drapeauee).
.dacross_comparability <- function(milo_result, sccoda_result) {
  same_object <- identical(
    milo_result$object_identity$fingerprint %||% NULL,
    sccoda_result$object_identity$fingerprint %||% NULL
  )
  same_contrast <- identical(
    milo_result$tested_contrast$target %||% NULL,
    sccoda_result$parameters$target_condition %||% NULL
  ) && identical(
    milo_result$tested_contrast$reference %||% NULL,
    sccoda_result$parameters$reference_condition %||% NULL
  )
  same_identity_column <- identical(
    milo_result$parameters$identity_column %||% NA_character_,
    sccoda_result$parameters$identity_column %||% NA_character_
  )
  flags <- c(
    same_object = same_object,
    same_contrast = same_contrast,
    same_identity_column = same_identity_column
  )
  caveats <- character(0)
  if (!same_object) {
    caveats <- c(caveats, paste0(
      "Les deux resultats ne proviennent pas du meme objet Seurat ",
      "(empreintes differentes) : la lecture croisee est INDICATIVE."
    ))
  }
  if (!same_contrast) {
    caveats <- c(caveats, paste0(
      "Contrastes differents (Milo : ", milo_result$tested_contrast$contrast,
      " ; scCODA : ", sccoda_result$parameters$target_condition, " vs ",
      sccoda_result$parameters$reference_condition,
      ") : les directions ne sont pas comparables terme a terme."
    ))
  }
  if (!same_identity_column) {
    caveats <- c(caveats, paste0(
      "Colonnes d'identite differentes (Milo : ",
      milo_result$parameters$identity_column, " ; scCODA : ",
      sccoda_result$parameters$identity_column,
      ") : la concordance par identite n'est PAS interpretable."
    ))
  }
  list(
    flags = flags,
    fully_comparable = all(flags),
    caveats = caveats
  )
}

#' Table de concordance par identite (pure — regles explicites)
#'
#' @param milo_result Resultat canonique Milo (assert_milo_result).
#' @param sccoda_result Resultat canonique scCODA (assert_sccoda_result).
#' @return data.frame par identite (union des identites des deux cotes) :
#'   identity, milo_n_nhoods, milo_frac_significant, milo_median_logfc,
#'   milo_direction, milo_has_signal, sccoda_effect, sccoda_credible,
#'   sccoda_inclusion_probability, sccoda_direction, sccoda_has_signal,
#'   concordance.
#' @export
build_da_cross_concordance <- function(milo_result, sccoda_result) {
  milo_r <- assert_milo_result(milo_result, context = "vue croisee Milo x scCODA")
  scc_r <- assert_sccoda_result(sccoda_result, context = "vue croisee Milo x scCODA")
  da <- milo_r$DA_table %||% NULL
  et <- scc_r$effect_table %||% NULL
  if (is.null(da) || !is.data.frame(da)) {
    .dacross_stop("Aucune table DA disponible dans le resultat Milo.")
  }
  if (is.null(et) || !is.data.frame(et)) {
    .dacross_stop("Aucune table d'effets disponible dans le resultat scCODA.")
  }
  alpha <- TS_DA_MILO_DISPLAY_ALPHA
  min_frac <- TS_DA_CROSS_SIGNIF_FRACTION

  # Cote Milo : voisinages annotes (NA = voisinage mixte, jamais reaffecte).
  da_id <- da[!is.na(da$identity) & !is.na(da$SpatialFDR), , drop = FALSE]
  milo_by_identity <- if (nrow(da_id)) do.call(rbind, lapply(
    sort(unique(da_id$identity)), function(lv) {
      sub <- da_id[da_id$identity == lv, , drop = FALSE]
      frac_sig <- mean(sub$SpatialFDR < alpha)
      med <- stats::median(sub$logFC)
      data.frame(
        identity = lv,
        milo_n_nhoods = nrow(sub),
        milo_frac_significant = frac_sig,
        milo_median_logfc = med,
        milo_direction = sign(med),
        milo_has_signal = frac_sig >= min_frac,
        stringsAsFactors = FALSE
      )
    }
  )) else NULL

  # Cote scCODA : effets de la covariable condition uniquement.
  cond_rows <- et[grepl("^condition\\[", et$covariate), , drop = FALSE]
  scc_by_identity <- if (nrow(cond_rows)) data.frame(
    identity = cond_rows$identity,
    sccoda_effect = cond_rows$effect,
    sccoda_credible = as.logical(cond_rows$credible),
    sccoda_inclusion_probability = cond_rows$inclusion_probability,
    sccoda_direction = sign(cond_rows$effect),
    sccoda_has_signal = as.logical(cond_rows$credible),
    stringsAsFactors = FALSE
  ) else NULL

  identities <- sort(unique(c(
    if (is.null(milo_by_identity)) character(0) else milo_by_identity$identity,
    if (is.null(scc_by_identity)) character(0) else scc_by_identity$identity
  )))
  rows <- lapply(identities, function(lv) {
    m <- milo_by_identity[milo_by_identity$identity == lv, , drop = FALSE]
    s <- scc_by_identity[scc_by_identity$identity == lv, , drop = FALSE]
    has_m <- nrow(m) == 1L
    has_s <- nrow(s) == 1L
    concordance <- if (!has_m || !has_s) {
      "not_comparable"
    } else if (has_m && m$milo_has_signal && has_s && s$sccoda_has_signal) {
      prod_dir <- m$milo_direction * s$sccoda_direction
      if (prod_dir > 0) {
        if (m$milo_direction > 0) "concordant_enriched_target"
        else "concordant_enriched_reference"
      } else if (prod_dir < 0) {
        "discordant_direction"
      } else {
        # Cas degenere (mediane exactement 0 malgre un signal) : direction
        # non definie — classe "no_signal", jamais une direction inventee.
        "no_signal"
      }
    } else if (has_m && m$milo_has_signal) "milo_only"
    else if (has_s && s$sccoda_has_signal) "sccoda_only"
    else "no_signal"
    data.frame(
      identity = lv,
      milo_n_nhoods = if (has_m) m$milo_n_nhoods else NA_integer_,
      milo_frac_significant = if (has_m) m$milo_frac_significant else NA_real_,
      milo_median_logfc = if (has_m) m$milo_median_logfc else NA_real_,
      milo_direction = if (has_m) m$milo_direction else NA_real_,
      milo_has_signal = if (has_m) m$milo_has_signal else NA,
      sccoda_effect = if (has_s) s$sccoda_effect else NA_real_,
      sccoda_credible = if (has_s) s$sccoda_credible else NA,
      sccoda_inclusion_probability = if (has_s) s$sccoda_inclusion_probability else NA_real_,
      sccoda_direction = if (has_s) s$sccoda_direction else NA_real_,
      sccoda_has_signal = if (has_s) s$sccoda_has_signal else NA,
      concordance = concordance,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    out <- data.frame(
      identity = character(0), milo_n_nhoods = integer(0),
      milo_frac_significant = numeric(0), milo_median_logfc = numeric(0),
      milo_direction = numeric(0), milo_has_signal = logical(0),
      sccoda_effect = numeric(0), sccoda_credible = logical(0),
      sccoda_inclusion_probability = numeric(0), sccoda_direction = numeric(0),
      sccoda_has_signal = logical(0), concordance = character(0),
      stringsAsFactors = FALSE
    )
  }
  if (!all(out$concordance %in% da_cross_concordance_categories())) {
    .dacross_stop("Categorie de concordance inconnue — regles corrompues.")
  }
  out
}

#' Resume croise des deux methodes (pure — lecture directe des resultats)
#'
#' Assemble le contexte de comparaison : compatibilite (objet/contraste/
#' colonne d'identite), recapitulatifs des deux designs, ce que chaque methode
#' teste (textes figes), concordance par identite et tables de desaccord.
#'
#' @param milo_result Resultat canonique Milo.
#' @param sccoda_result Resultat canonique scCODA.
#' @return list(comparability, method_meanings, concordance, disagreement,
#'   milo_recap, sccoda_recap).
#' @export
build_da_cross_method_summary <- function(milo_result, sccoda_result) {
  milo_r <- assert_milo_result(milo_result, context = "resume croise DA")
  scc_r <- assert_sccoda_result(sccoda_result, context = "resume croise DA")
  comparability <- .dacross_comparability(milo_r, scc_r)
  concordance <- build_da_cross_concordance(milo_r, scc_r)
  disagreement <- concordance[concordance$concordance %in%
                                c("discordant_direction", "milo_only",
                                  "sccoda_only"), , drop = FALSE]
  rownames(disagreement) <- NULL
  method_meanings <- list(
    milo = paste0(
      "Milo teste l'abondance differentielle PAR VOISINAGE (regions d'un ",
      "graphe kNN, NB GLM, SpatialFDR). Le signal est NIVEAU VOISINAGE — ",
      "l'annotation d'identite d'un voisinage (fraction >= ",
      TS_DA_MILO_IDENTITY_FRACTION_MIN, ") est descriptive."
    ),
    sccoda = paste0(
      "scCODA teste la composition PAR ECHANTILLON (modele bayesien ",
      "spike-and-slab avec identite de reference). Les effets sont des ",
      "intervalles de credibilite — pas des p-values ; l'unite est ",
      "l'ECHANTILLON (les cellules ne sont pas des replicats)."
    ),
    comparison = paste0(
      "La concordance est DESCRIPTIVE et sensible a la methode : Milo capte ",
      "des regions du graphe, scCODA des changements de composition ",
      "globale. Un desaccord peut provenir des echelles ou des effectifs — ",
      "aucune p-value de consensus n'est calculee."
    )
  )
  list(
    comparability = comparability,
    method_meanings = method_meanings,
    concordance = concordance,
    disagreement = disagreement,
    milo_recap = list(
      analysis_id = milo_r$analysis_id %||% NA_character_,
      contrast = milo_r$tested_contrast$contrast %||% NA_character_,
      identity_column = milo_r$parameters$identity_column %||% NA_character_,
      fdr_weighting = milo_r$parameters$fdr_weighting %||% NA_character_,
      display_alpha = TS_DA_MILO_DISPLAY_ALPHA,
      n_neighbourhoods = nrow(milo_r$DA_table %||% data.frame())
    ),
    sccoda_recap = list(
      analysis_id = scc_r$analysis_id %||% NA_character_,
      target_condition = scc_r$parameters$target_condition %||% NA_character_,
      reference_condition = scc_r$parameters$reference_condition %||% NA_character_,
      identity_column = scc_r$parameters$identity_column %||% NA_character_,
      fdr_target = scc_r$model_specification$fdr_target %||% NA_real_,
      reference_identity = scc_r$reference_identity %||% NA_character_,
      credible_effects = as.character(scc_r$credible_effects %||% character(0))
    )
  )
}

#' Provenance de la lecture croisee (produite a l'export, regle 7)
#'
#' @param milo_result Resultat canonique Milo.
#' @param sccoda_result Resultat canonique scCODA.
#' @param options list d'options d'affichage (ex. include_batch, identites
#'   filtres) — figee dans les parametres.
#' @return Entree new_provenance_entry() enrichie (analysis_type = "da_cross").
#' @export
build_da_cross_provenance <- function(milo_result, sccoda_result,
                                      options = list()) {
  milo_r <- assert_milo_result(milo_result, context = "provenance croisee DA")
  scc_r <- assert_sccoda_result(sccoda_result, context = "provenance croisee DA")
  comp <- .dacross_comparability(milo_r, scc_r)
  entry <- new_provenance_entry(
    analysis_id = "sc-da-cross",
    method = "da_cross_method_views",
    parameters = list(
      display_alpha = TS_DA_MILO_DISPLAY_ALPHA,
      signif_fraction = TS_DA_CROSS_SIGNIF_FRACTION,
      milo_analysis_id = milo_r$analysis_id %||% NA_character_,
      sccoda_analysis_id = scc_r$analysis_id %||% NA_character_,
      milo_fingerprint = milo_r$object_identity$fingerprint %||% NA_character_,
      sccoda_fingerprint = scc_r$object_identity$fingerprint %||% NA_character_,
      same_object = comp$fully_comparable,
      options = as.list(options %||% list()),
      concordance_rules = "milo: frac(SpatialFDR<alpha) >= fraction ; sccoda: credible au fdr_target ; direction = signe",
      composition_unit = "sample"
    ),
    dataset = NULL,
    cells_used = NULL,
    seed = NULL,
    warnings = comp$caveats
  )
  entry$analysis_type <- "da_cross"
  entry$timestamp_utc <- format(entry$timestamp, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  entry
}

#' Export CSV de la concordance (trace par les deux analysis_id)
#'
#' @param milo_result Resultat canonique Milo.
#' @param sccoda_result Resultat canonique scCODA.
#' @return data.frame (concordance + colonnes de trace).
#' @export
build_da_cross_concordance_export <- function(milo_result, sccoda_result) {
  milo_r <- assert_milo_result(milo_result, context = "export concordance DA")
  scc_r <- assert_sccoda_result(sccoda_result, context = "export concordance DA")
  conc <- build_da_cross_concordance(milo_r, scc_r)
  data.frame(
    conc,
    milo_analysis_id = rep(milo_r$analysis_id %||% NA_character_, nrow(conc)),
    sccoda_analysis_id = rep(scc_r$analysis_id %||% NA_character_, nrow(conc)),
    timestamp_utc = rep(format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                        nrow(conc)),
    stringsAsFactors = FALSE
  )
}

#' Nom de fichier d'export croise (Stage 16)
#'
#' @param kind Prefixe descriptif (ex. "da_cross_concordance").
#' @param ext Extension ("csv").
#' @return Chaine "<kind>_sc-da-cross_<date>.<ext>".
#' @export
da_cross_export_filename <- function(kind, ext) {
  sprintf(
    "%s_%s_%s.%s",
    as.character(kind)[1L],
    "sc-da-cross",
    format(Sys.Date(), "%Y-%m-%d"),
    as.character(ext)[1L]
  )
}

# ── Vues (ggplot2 pures) ────────────────────────────────────────────────────
#' Composition par echantillon (contexte commun des deux methodes)
#'
#' Barres empilees par echantillon et condition, d'apres la matrice de
#' composition ANALYSEE par scCODA (meme unite que les deux lectures :
#' l'echantillon). Les cellules ne sont PAS des replicats.
#'
#' @param milo_result Resultat canonique Milo (contexte + compatibilite).
#' @param sccoda_result Resultat canonique scCODA (source de la composition).
#' @return ggplot.
#' @export
plot_da_cross_sample_composition <- function(milo_result, sccoda_result) {
  milo_r <- assert_milo_result(milo_result, context = "vue croisee composition")
  scc_r <- assert_sccoda_result(sccoda_result, context = "vue croisee composition")
  ct <- scc_r$composition_table %||% NULL
  if (is.null(ct) || !is.data.frame(ct) || nrow(ct) == 0L) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5,
                               label = "Aucune composition echantillon disponible.",
                               size = 4.2) +
             ggplot2::theme_void())
  }
  ident_cols <- setdiff(colnames(ct), c("sample", "condition", "batch"))
  long <- stats::reshape(
    ct[, c("sample", "condition", ident_cols), drop = FALSE],
    direction = "long", varying = ident_cols,
    times = ident_cols, timevar = "identity", idvar = "sample_id",
    v.names = "n_cells"
  )
  long$condition <- as.character(long$condition)
  ggplot2::ggplot(long, ggplot2::aes(x = sample, y = n_cells, fill = identity)) +
    ggplot2::geom_col(color = "grey25", linewidth = 0.2) +
    ggplot2::facet_wrap(~condition, scales = "free_x") +
    ggplot2::labs(
      title = "Vues croisees DA — contexte de composition par echantillon",
      subtitle = sprintf(
        paste0("Unite commune : ECHANTILLON (les cellules ne sont pas des ",
               "replicats). Milo : %s ; scCODA : %s vs %s (reference ",
               "d'identite : %s)."),
        milo_r$tested_contrast$contrast %||% NA_character_,
        scc_r$parameters$target_condition %||% NA_character_,
        scc_r$parameters$reference_condition %||% NA_character_,
        scc_r$reference_identity %||% NA_character_
      ),
      caption = "Matrice de composition analysee par scCODA — lecture descriptive.",
      x = NULL, y = "Nombre de cellules", fill = "Identite"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' Concordance par identite : effet scCODA vs mediane logFC Milo
#'
#' Nuage descriptif : x = effet compositionnel scCODA (echantillon), y =
#' mediane des logFC des voisinages Milo annotes a l'identite (voisinage).
#' Les axes portent des GRANDEURS DIFFERENTES (c'est le propos) ; la couleur
#' donne la categorie de concordance (regles explicites).
#'
#' @param milo_result Resultat canonique Milo.
#' @param sccoda_result Resultat canonique scCODA.
#' @return ggplot.
#' @export
plot_da_cross_concordance <- function(milo_result, sccoda_result) {
  milo_r <- assert_milo_result(milo_result, context = "vue croisee concordance")
  scc_r <- assert_sccoda_result(sccoda_result, context = "vue croisee concordance")
  conc <- build_da_cross_concordance(milo_r, scc_r)
  pts <- conc[!is.na(conc$milo_median_logfc) & !is.na(conc$sccoda_effect), ,
              drop = FALSE]
  if (nrow(pts) == 0L) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5,
                               label = paste0("Aucune identite comparable ",
                                              "(voisinages Milo non annotes ou ",
                                              "identites absentes)."),
                               size = 4.2) +
             ggplot2::theme_void())
  }
  ggplot2::ggplot(pts, ggplot2::aes(x = sccoda_effect, y = milo_median_logfc,
                                    color = concordance)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
    ggplot2::geom_point(size = 3.4) +
    ggrepel::geom_text_repel(ggplot2::aes(label = identity), size = 3.1,
                             max.overlaps = 20, seed = 1) +
    ggplot2::labs(
      title = "Vues croisees DA — concordance des identites (descriptive)",
      subtitle = sprintf(
        paste0("Deux GRANDEURS DIFFERENTES : x = effet compositionnel ",
               "scCODA (echantillon, cible vs reference) ; y = mediane des ",
               "logFC des voisinages Milo annotes (voisinage). Categories ",
               "par regles explicites (alpha SpatialFDR %s, fraction >= %s)."),
        TS_DA_MILO_DISPLAY_ALPHA, TS_DA_CROSS_SIGNIF_FRACTION
      ),
      caption = paste0(
        "Aucune p-value de consensus — la comparaison reste exploratoire ; ",
        "un desaccord peut provenir des echelles (voisinage vs echantillon)."
      ),
      x = "Effet scCODA (composition, echantillon)",
      y = "Mediane logFC voisinages Milo (voisinage)",
      color = "Concordance"
    ) +
    ggplot2::theme_classic()
}

#' Mappage contextuel voisinages -> identites (cote Milo)
#'
#' Pour chaque identite annotee : nombre de voisinages, part significative
#' (SpatialFDR < alpha) et mediane des logFC — le pont explicite entre le
#' signal PAR VOISINAGE et la lecture par identite (descriptive, jamais un DA
#' par type cellulaire).
#'
#' @param milo_result Resultat canonique Milo.
#' @return ggplot.
#' @export
plot_da_cross_nhood_mapping <- function(milo_result) {
  milo_r <- assert_milo_result(milo_result, context = "vue croisee mappage voisinages")
  da <- milo_r$DA_table %||% NULL
  if (is.null(da) || !is.data.frame(da) || nrow(da) == 0L ||
      all(is.na(da$identity))) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5,
                               label = paste0("Aucun voisinage annote (colonne ",
                                              "d'identite absente ou tous ",
                                              "voisinages mixtes)."),
                               size = 4.2) +
             ggplot2::theme_void())
  }
  da_id <- da[!is.na(da$identity) & !is.na(da$SpatialFDR), , drop = FALSE]
  agg <- do.call(rbind, lapply(split(da_id, da_id$identity), function(sub) {
    data.frame(
      identity = sub$identity[1],
      n_nhoods = nrow(sub),
      frac_significant = mean(sub$SpatialFDR < TS_DA_MILO_DISPLAY_ALPHA),
      median_logfc = stats::median(sub$logFC),
      stringsAsFactors = FALSE
    )
  }))
  agg$identity <- factor(agg$identity,
                         levels = agg$identity[order(agg$median_logfc)])
  ggplot2::ggplot(agg, ggplot2::aes(x = median_logfc, y = identity,
                                    fill = frac_significant)) +
    ggplot2::geom_col(color = "grey25", linewidth = 0.2) +
    ggplot2::scale_fill_viridis_c(name = "Part de voisinages\nsignificatifs",
                                  limits = c(0, 1), labels = scales::percent) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
    ggplot2::labs(
      title = "Vues croisees DA — mappage voisinages -> identites (cote Milo)",
      subtitle = sprintf(
        paste0("Pour chaque identite annotee : mediane des logFC de ses ",
               "voisinages et part significative (SpatialFDR < %s). ",
               "Contexte : cible '%s' vs reference '%s' — lecture ",
               "DESCRIPTIVE, jamais un DA par type cellulaire."),
        TS_DA_MILO_DISPLAY_ALPHA,
        milo_r$tested_contrast$target %||% NA_character_,
        milo_r$tested_contrast$reference %||% NA_character_
      ),
      caption = sprintf(
        paste0("%d voisinage(s) au total ; voisinages mixtes (fraction ",
               "d'identite < %s) exclus et jamais reaffectes."),
        nrow(da), TS_DA_MILO_IDENTITY_FRACTION_MIN
      ),
      x = "Mediane logFC des voisinages (cible - reference)", y = NULL
    ) +
    ggplot2::theme_classic()
}

# =============================================================================
# R/sc/sc_abundance_milo.R — Abondance differentielle 4E-1 : Milo (Stage 14)
# =============================================================================
# But : DA sensible a l'echantillon sur les VOISINAGES d'un graphe kNN
# (miloR), GATED sur le design valide du Stage 13 — Milo est REFUSE sur un
# design bloque, perime ou absent (assert_da_design_result(method="milo")).
#
# Principe scientifique : le resultata Milo est un signal PAR VOISINAGE
# (region du graphe), PAS un DA par type cellulaire — toute projection
# cellule/identite est descriptive (fraction d'identite du voisinage, score
# d'affichage par cellule) et ne remplace jamais un test par type.
#
# Pur domaine : aucune reactivite Shiny. Dependances lourdes (miloR et ses
# dependances Bioconductor) chargees SEULEMENT au moment du calcul —
# l'absence de miloR est une erreur explicite, jamais un repli silencieux.
# Sourced in app.R AFTER R/sc/sc_abundance_design.R, BEFORE modules/sc/*.R.
# Constantes TS_DA_MILO_* definies dans config/defaults.R.
#
# ── FLUX (Stage 14) ─────────────────────────────────────────────────────────
# objet Seurat valide + design Stage 13 eligible
#   -> preparation (exclusion des cellules sans sample_id, SCE minimal :
#      colData + reduction — seules la reduction et les metadonnees sont
#      utilisees ; les comptes ne servent PAS au modele DA)
#   -> construction des voisinages (buildGraph/makeNhoods/calcNhoodDistance)
#   -> comptage par echantillon (countCells) + modele NB GLM (testNhoods,
#      design ~ 0 + condition [+ batch], contraste explicite cible-reference)
#   -> annotation descriptive par identite (annotateNhoods, fraction >=
#      TS_DA_MILO_IDENTITY_FRACTION_MIN, sinon NA)
#   -> mappage cellule -> voisinages (score d'affichage descriptif)
#   -> resultat canonique finalize_milo_result() + provenance produite ici.
#
# ── CONTRAT DE RESULTAT CANONIQUE (Stage 14) ────────────────────────────────
# Assemble par finalize_milo_result() (champs milo_contract_fields()) :
#
#   type                  "milo_da" (constant)
#   status                etat de validite (milo_validity_states())
#   design                recapitulatif du design Stage 13 consomme (colonnes,
#                         composition_unit, compteurs, avertissements design)
#   parameters            parametres TOUT enregistres (k, prop, d, reduction,
#                         refined, refinement_scheme, fdr_weighting, seed,
#                         colonnes design, identite, fraction min, model_formule)
#   tested_contrast       list(formula, contrast, target, reference)
#   model_specification   list(design_cols, robust, min.mean, fdr_weighting,
#                         intercept.type, n_design_samples)
#   neighbourhood_summary data.frame 1 ligne (n voisinages, tailles, couverture)
#   DA_table              data.frame PAR VOISINAGE (Nhood, n_cells, logFC,
#                         logCPM, F, PValue, FDR, SpatialFDR, identity,
#                         identity_fraction)
#   nhood_assignment      data.frame PAR CELLULE (cell_id, n_neighbourhoods,
#                         mean_logFC) — affichage descriptif sur embedding
#   sample_composition    data.frame PAR ECHANTILLON (condition, batch,
#                         n_cells_total, n_cells_in_nhoods) — contexte
#                         echantillon (les cellules ne sont PAS des replicats)
#   package_versions      list(miloR, edgeR, BiocNeighbors, SingleCellExperiment,
#                         SeuratObject, R)
#   object_identity       list(fingerprint, method, seurat_dims) — empreinte
#                         v2 REUTILISEE (build_object_identity_v2)
#   warnings              vecteur character (design + calcul, prefixes)
#   provenance            entree new_provenance_entry() enrichie
#                         (analysis_type = "milo_da")
#   analysis_id           "sc-da-milo"
#   timestamp_utc         horodatage UTC ISO-8601
#
# ── ETATS DE VALIDITE ───────────────────────────────────────────────────────
#   valid / valid_with_warnings  — resultats produits
#   invalid_input                — erreur structurelle (AUCUN resultat)
#   compute_failed               — echec du calcul miloR (erreur classee,
#                                  AUCUN resultat partiel)
#   design_not_eligible          — design Stage 13 bloque/perime : refuse par
#                                  assert_da_design_result (AUCUN resultat)
#   stale_against_current_seurat_object — etat derive a l'affichage
#
# ── API PUBLIQUE FIGEE (Stage 14) ───────────────────────────────────────────
# milo_public_api() ; test de freeze (tests/testthat/test-milo-contract-freeze.R).
# Helpers internes prefixes d'un point (.milo_*).
# =============================================================================

#' Champs documentes du contrat de resultat Milo canonique
#'
#' Source de verite partagee par le code, le test de freeze et
#' docs/contracts/MILO_RESULT_CONTRACT.md — toute modification passe par les
#' TROIS simultanement.
#'
#' @return Vecteur character des 16 champs contractuels.
#' @export
milo_contract_fields <- function() {
  c(
    "type", "status", "design", "parameters", "tested_contrast",
    "model_specification", "neighbourhood_summary", "DA_table",
    "nhood_assignment", "sample_composition", "package_versions",
    "object_identity", "warnings", "provenance", "analysis_id",
    "timestamp_utc"
  )
}

# Etats de validite (source de verite ; l'accesseur public est l'unique
# lecture autorisee ailleurs).
.MILO_STATUS_STATES <- c(
  "valid",
  "valid_with_warnings",
  "invalid_input",
  "compute_failed",
  "design_not_eligible",
  "stale_against_current_seurat_object"
)

#' Etats de validite du contrat Milo
#'
#' @return Vecteur character des 6 etats documentes.
#' @export
milo_validity_states <- function() .MILO_STATUS_STATES

#' Libelles francais des etats de validite (affichage utilisateur)
#'
#' @return Vecteur character nomme par etat.
#' @export
milo_status_labels <- function() {
  c(
    valid = paste0(
      "Analyse Milo terminee : DA par voisinage produite. ATTENTION : le ",
      "signal est NIVEAU VOISINAGE (region du graphe) — il ne se lit PAS ",
      "comme un DA par type cellulaire."
    ),
    valid_with_warnings = paste0(
      "Analyse Milo terminee avec avertissements : lisez les avertissements ",
      "(design et calcul) avant toute interpretation du DA par voisinage."
    ),
    invalid_input = paste0(
      "Entree invalide : objet/reduction/contraste inexploitable — aucun ",
      "resultat Milo n'est produit."
    ),
    compute_failed = paste0(
      "Echec du calcul Milo : le modele ou la construction des voisinages a ",
      "echoue — aucun resultat partiel n'est expose."
    ),
    design_not_eligible = paste0(
      "Design non eligible : Milo exige un design Stage 13 valide (replicats ",
      "biologiques suffisants, deux conditions au moins) — un design bloque ",
      "n'est JAMAIS consommable."
    ),
    stale_against_current_seurat_object = paste0(
      "Perime : le resultat Milo (ou son design) correspond a un objet ",
      "Seurat anterieur — relancez l'analyse."
    )
  )
}

#' Erreur Milo avec etat structure
.milo_stop <- function(state, message) {
  stop(errorCondition(
    message,
    state = state,
    class = "milo_error"
  ))
}

#' Etat de validite associe a une erreur Milo
#'
#' @param e Condition (erreur) capturee.
#' @return Chaine d'etat structuree, NA_character_ pour une erreur sans etat.
#' @export
# §14.1 : délégation à l'accesseur générique. Famille F1 MESURÉE — la CLASSE
# est vérifiée : l'état d'une erreur d'un autre domaine rend NA.
milo_error_state <- function(e) ts_error_state(e, "milo_error")

#' Champ vide ? (NA, "" ou whitespace)
.milo_is_blank <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

#' miloR est-il disponible ? (versions enregistrees dans la provenance)
#'
#' @param stop_if_missing Logical — lever une erreur explicite si absent.
#' @return list(available, version) ; erreur classee milo_error
#'   (invalid_input) si absent et stop_if_missing.
#' @export
milo_available <- function(stop_if_missing = FALSE) {
  ok <- requireNamespace("miloR", quietly = TRUE)
  if (!ok && isTRUE(stop_if_missing)) {
    .milo_stop(
      "invalid_input",
      paste0(
        "Le package 'miloR' est requis pour l'analyse Milo (4E-1) et n'est ",
        "pas disponible. Installez-le depuis Bioconductor : ",
        "BiocManager::install('miloR') — aucun repli silencieux n'est ",
        "effectue."
      )
    )
  }
  list(
    available = ok,
    version = if (ok) as.character(utils::packageVersion("miloR")) else NA_character_
  )
}

#' Recapitulatif du design Stage 13 consomme par Milo (trace dans le resultat)
#'
#' Delegation a da_design_recap() (Stage 13, partage avec scCODA) — le corps
#' n'est PAS duplique entre les deux methodes.
.milo_design_recap <- function(da_design_result) {
  da_design_recap(da_design_result)
}

#' SCE minimal pour le calcul Milo (colData + reduction, SANS comptes)
#'
#' Seule la reduction (espace latent du graphe) et les metadonnees servent au
#' DA par voisinage ; embarquer les comptes serait un coût memoire inutile.
.milo_build_sce <- function(cell_meta, embeddings, sample_col, condition_col,
                            batch_col, identity_col, context) {
  if (!requireNamespace("SingleCellExperiment", quietly = TRUE)) {
    .milo_stop(
      "invalid_input",
      paste0("Echec ", context, " : le package 'SingleCellExperiment' est ",
             "requis (dependance Bioconductor de miloR) — installez miloR ",
             "via BiocManager::install('miloR').")
    )
  }
  cols <- c(sample_col, condition_col,
            if (!is.null(batch_col)) batch_col,
            if (!is.null(identity_col)) identity_col)
  cd <- cell_meta[, cols, drop = FALSE]
  sce <- SingleCellExperiment::SingleCellExperiment(
    assays = S4Vectors::SimpleList(),
    colData = S4Vectors::DataFrame(cd),
    reducedDims = S4Vectors::SimpleList(MILO = embeddings)
  )
  sce
}

#' Score d'affichage descriptif par cellule (mappage voisinages -> cellules)
#'
#' Pour CHAQUE cellule : nombre de voisinages la contenant et moyenne des
#' logFC de ces voisinages (NA si aucun voisinage). DESCRIPTIF et destine a
#' l'affichage sur embedding — ce n'est PAS un test par cellule.
.milo_cell_display_scores <- function(nhood_matrix, logfc) {
  n <- Matrix::rowSums(nhood_matrix != 0)
  wsum <- as.numeric(nhood_matrix %*% logfc)
  mean_logfc <- ifelse(n > 0L, wsum / pmax(n, 1L), NA_real_)
  data.frame(
    cell_id = rownames(nhood_matrix),
    n_neighbourhoods = as.integer(n),
    mean_logFC = mean_logfc,
    stringsAsFactors = FALSE
  )
}

#' Executer l'analyse Milo (4E-1) — GATED sur le design Stage 13
#'
#' Valide TOUT prerequis AVANT le calcul (design Stage 13 eligible et non
#' perime, reduction presente, conditions du contraste connues, miloR
#' disponible), puis construit les voisinages et teste l'abondance
#' differentielle PAR VOISINAGE avec un contraste explicite
#' (condition cible - condition reference). Le design ~ 0 + condition [+ batch]
#' est estime sur TOUS les echantillons du design (les voisinages sont
#' construits sur le dataset complet) ; le contraste isole les deux
#' conditions choisies.
#'
#' @param seurat_obj Objet Seurat valide (reduction requise).
#' @param da_design_result Resultat canonique du design Stage 13 — REQUIS
#'   (assert_da_design_result(method = "milo") refuse un design bloque ou
#'   perime).
#' @param reduction Nom de la reduction de l'espace latent (ex. "pca").
#' @param target_condition Condition cible du contraste (logFC > 0 =
#'   enrichie dans la cible).
#' @param reference_condition Condition de reference.
#' @param use_batch Inclure le terme batch du design dans le modele
#'   (TRUE par defaut quand le design declare une colonne batch).
#' @param identity_col Colonne d'identite pour l'annotation descriptive des
#'   voisinages ; NULL = colonne du design si declaree, NA pour desactiver.
#' @param k,prop,d Parametres de construction des voisinages (config TS_DA_MILO_*).
#' @param refinement_scheme Mode de raffinement makeNhoods.
#' @param fdr_weighting Ponderation du SpatialFDR (graphSpatialFDR).
#' @param seed Graine enregistree dans la provenance.
#' @param context Contexte cite dans les messages d'erreur.
#' @return Resultat canonique (champs milo_contract_fields()).
#' @export
run_milo_da <- function(seurat_obj,
                        da_design_result,
                        reduction,
                        target_condition,
                        reference_condition,
                        use_batch = TRUE,
                        identity_col = NULL,
                        k = TS_DA_MILO_K,
                        prop = TS_DA_MILO_PROP,
                        d = TS_DA_MILO_D,
                        refinement_scheme = TS_DA_MILO_REFINEMENT_SCHEME,
                        fdr_weighting = TS_DA_MILO_FDR_WEIGHTING,
                        seed = TS_DA_MILO_SEED,
                        context = "abondance differentielle Milo") {
  # ── 0. Prerequis — TOUT valide avant le calcul ───────────────────────────
  seurat_obj <- assert_seurat(seurat_obj, context = context)
  # LA porte Stage 13 : un design bloque, perime ou absent est REFUSE.
  assert_da_design_result(
    da_design_result,
    method = "milo",
    seurat_obj = seurat_obj,
    context = context
  )
  assert_reduction(seurat_obj, reduction, context = context)
  if (is.null(target_condition) || is.null(reference_condition) ||
      is.na(target_condition) || is.na(reference_condition) ||
      identical(target_condition, reference_condition)) {
    .milo_stop(
      "invalid_input",
      sprintf(
        paste0("Echec %s : le contraste exige deux conditions DISTINCTES ",
               "(cible = '%s', reference = '%s')."),
        context, target_condition %||% NA_character_,
        reference_condition %||% NA_character_
      )
    )
  }

  design_recap <- .milo_design_recap(da_design_result)
  conditions <- if (!is.null(da_design_result$condition_summary) &&
                    nrow(da_design_result$condition_summary)) {
    as.character(da_design_result$condition_summary$condition)
  } else character(0)
  for (lv in c(target_condition, reference_condition)) {
    if (!lv %in% conditions) {
      .milo_stop(
        "invalid_input",
        sprintf(
          paste0("Echec %s : la condition '%s' n'appartient pas au design ",
                 "valide (conditions connues : %s)."),
          context, lv, paste(conditions, collapse = ", ")
        )
      )
    }
  }

  avail <- milo_available(stop_if_missing = TRUE)

  cfg <- da_design_result$config %||% list()
  sample_col <- cfg$sample_id
  condition_col <- cfg$condition
  batch_col <- if (isTRUE(use_batch) && !is.na(cfg$batch %||% NA_character_) &&
                   nzchar(cfg$batch %||% "")) cfg$batch else NULL
  if (is.null(identity_col) || is.na(identity_col)) {
    identity_col <- if (!is.na(cfg$identity %||% NA_character_) &&
                        nzchar(cfg$identity %||% "")) cfg$identity else NULL
  } else if (identical(identity_col, NA) || !nzchar(identity_col)) {
    identity_col <- NULL
  } else {
    assert_metadata_column(seurat_obj, identity_col, context = context)
  }

  # ── 1. Preparation — cellules exploitables (meme regle que le Stage 13) ──
  meta <- seurat_obj@meta.data
  sid <- as.character(meta[[sample_col]])
  keep <- !.milo_is_blank(sid)
  n_excluded <- sum(!keep)
  if (!any(keep)) {
    .milo_stop(
      "invalid_input",
      sprintf(
        paste0("Echec %s : aucune cellule avec un '%s' renseigne — le ",
               "comptage par echantillon est impossible."),
        context, sample_col
      )
    )
  }
  cell_meta <- meta[keep, , drop = FALSE]
  embeddings <- SeuratObject::Embeddings(seurat_obj, reduction)
  # Alignement TOUJOURS explicite sur les cellules conservees (ordre de
  # l'objet) — avec comme sans exclusions, reducedDims exige exactement
  # nrow(embeddings) == ncol(sce).
  if (!all(rownames(meta)[keep] %in% rownames(embeddings))) {
    .milo_stop(
      "invalid_input",
      sprintf(
        paste0("Echec %s : des barcodes de cellules sont absents de la ",
               "reduction '%s' — objet incoherent."),
        context, reduction
      )
    )
  }
  embeddings <- embeddings[rownames(meta)[keep], , drop = FALSE]
  d_eff <- min(as.integer(d), ncol(embeddings))
  if (d_eff < 2L) {
    .milo_stop(
      "invalid_input",
      sprintf(
        paste0("Echec %s : la reduction '%s' fournit %d dimension(s) — au ",
               "moins 2 sont requises pour le graphe de voisinage."),
        context, reduction, ncol(embeddings)
      )
    )
  }

  # ── 2. Calcul miloR — encapsule, aucune fuite partielle ──────────────────
  # makeNhoods echantillonne ses sommets initiaux aleatoirement : la graine
  # est APPLIQUEE ici (pas seulement enregistree) — deux executions avec les
  # memes entrees et la meme graine produisent la meme table DA.
  warnings_all <- design_recap$design_warnings
  nhood_assignment <- NULL
  tryCatch({
    set.seed(seed)
    sce <- .milo_build_sce(
      cell_meta = cell_meta, embeddings = embeddings,
      sample_col = sample_col, condition_col = condition_col,
      batch_col = batch_col, identity_col = identity_col, context = context
    )
    milo_obj <- miloR::Milo(sce)
    milo_obj <- miloR::buildGraph(milo_obj, k = k, d = d_eff, reduced.dim = "MILO")
    milo_obj <- miloR::makeNhoods(
      milo_obj, prop = prop, k = k, d = d_eff, refined = TRUE,
      refinement_scheme = refinement_scheme, reduced_dims = "MILO"
    )
    milo_obj <- miloR::calcNhoodDistance(milo_obj, d = d_eff, reduced.dim = "MILO")
    milo_obj <- miloR::countCells(
      milo_obj, samples = sample_col, meta.data = cell_meta
    )

    # Design echantillon-niveau (une ligne par echantillon) ; termes renommes
    # condition/batch pour la formule — le mapping des colonnes d'origine est
    # enregistre dans la provenance.
    md <- as.data.frame(SummarizedExperiment::colData(milo_obj))
    design.df <- unique(md[, c(sample_col, condition_col,
                                if (!is.null(batch_col)) batch_col), drop = FALSE])
    colnames(design.df) <- c("sample", "condition",
                              if (!is.null(batch_col)) "batch")
    design.df$condition <- factor(design.df$condition,
                                  levels = sort(unique(as.character(design.df$condition))))
    if (!is.null(batch_col)) {
      design.df$batch <- factor(design.df$batch,
                                levels = sort(unique(as.character(design.df$batch))))
    }
    rownames(design.df) <- design.df$sample
    formule <- if (!is.null(batch_col)) "~ 0 + condition + batch" else "~ 0 + condition"
    design <- stats::model.matrix(as.formula(formule), data = design.df)
    target_coef <- make.names(paste0("condition", target_condition))
    ref_coef <- make.names(paste0("condition", reference_condition))
    if (!target_coef %in% colnames(design) || !ref_coef %in% colnames(design)) {
      .milo_stop(
        "invalid_input",
        sprintf(
          paste0("Echec %s : colonnes de contraste introuvables dans la ",
                 "matrice de design (%s / %s parmi : %s)."),
          context, target_coef, ref_coef, paste(colnames(design), collapse = ", ")
        )
      )
    }
    contrast <- paste(target_coef, "-", ref_coef)

    withCallingHandlers({
      da_results <- miloR::testNhoods(
        milo_obj, design = design, design.df = design.df,
        fdr.weighting = fdr_weighting, min.mean = TS_DA_MILO_MIN_MEAN,
        robust = TS_DA_MILO_ROBUST, reduced.dim = "MILO",
        model.contrasts = contrast, fail.on.error = FALSE
      )
    }, warning = function(w) {
      warnings_all <- c(warnings_all, paste("miloR :", conditionMessage(w)))
      invokeRestart("muffleWarning")
    })

    if (is.null(da_results) || nrow(da_results) == 0L) {
      .milo_stop(
        "compute_failed",
        sprintf(
          paste0("Echec %s : testNhoods n'a produit AUCUN voisinage testable ",
                 "(%d voisinages construits). Verifiez la reduction et les ",
                 "effectifs par echantillon."),
          context, length(milo_obj@nhoodIndex)
        )
      )
    }

    # Tailles + annotation descriptive par identite (les voisinages mixtes,
    # fraction < TS_DA_MILO_IDENTITY_FRACTION_MIN, sont marques NA — jamais
    # reaffectes).
    da_results$n_cells <- as.integer(Matrix::colSums(miloR::nhoods(milo_obj) != 0))
    identity_used <- NA_character_
    if (!is.null(identity_col) && identity_col %in% colnames(md)) {
      da_results <- miloR::annotateNhoods(
        milo_obj, da_results, coldata_col = identity_col
      )
      identity_used <- identity_col
      frac_col <- paste0(identity_col, "_fraction")
      da_results$identity_fraction <- if (frac_col %in% colnames(da_results)) {
        as.numeric(da_results[[frac_col]])
      } else NA_real_
      da_results$identity <- as.character(da_results[[identity_col]])
      da_results[[identity_col]] <- NULL
      da_results[[frac_col]] <- NULL
    } else {
      da_results$identity <- NA_character_
      da_results$identity_fraction <- NA_real_
    }
    da_results <- da_results[, c("Nhood", "n_cells", "logFC", "logCPM", "F",
                                 "PValue", "FDR", "SpatialFDR", "identity",
                                 "identity_fraction"), drop = FALSE]
    rownames(da_results) <- NULL

    nhood_assignment <- .milo_cell_display_scores(
      miloR::nhoods(milo_obj), da_results$logFC
    )

    # Contexte echantillon : les cellules NE SONT PAS des replicats — le
    # resume reste au niveau echantillon (comptage et couverture voisinage).
    n_in <- rownames(nhood_assignment)[nhood_assignment$n_neighbourhoods > 0L]
    sample_composition <- do.call(rbind, lapply(design.df$sample, function(s) {
      cells_s <- rownames(cell_meta)[cell_meta[[sample_col]] == s]
      data.frame(
        sample = s,
        condition = as.character(design.df$condition[design.df$sample == s]),
        batch = if (!is.null(batch_col)) {
          as.character(design.df$batch[design.df$sample == s])
        } else NA_character_,
        n_cells_total = length(cells_s),
        n_cells_in_nhoods = sum(cells_s %in% n_in),
        stringsAsFactors = FALSE
      )
    }))

    nh_sizes <- da_results$n_cells
    neighbourhood_summary <- data.frame(
      n_neighbourhoods = nrow(da_results),
      n_cells_in_nhoods = sum(nhood_assignment$n_neighbourhoods > 0L),
      fraction_cells_in_nhoods = mean(nhood_assignment$n_neighbourhoods > 0L),
      median_nhood_size = stats::median(nh_sizes),
      min_nhood_size = min(nh_sizes),
      max_nhood_size = max(nh_sizes),
      stringsAsFactors = FALSE
    )

    components <- list(
      da_results = da_results,
      design.df = design.df,
      formule = formule,
      contrast = contrast,
      neighbourhood_summary = neighbourhood_summary,
      nhood_assignment = nhood_assignment,
      sample_composition = sample_composition,
      identity_used = identity_used,
      d_eff = d_eff
    )
  }, error = function(e) {
    if (inherits(e, "milo_error")) stop(e)
    .milo_stop(
      "compute_failed",
      sprintf("Echec %s : le calcul miloR a echoue — %s", context,
              conditionMessage(e))
    )
  })

  # ── 3. Resultat canonique + provenance produite ICI (regle 7) ────────────
  finalize_milo_result(
    components = components,
    seurat_obj = seurat_obj,
    design_recap = design_recap,
    reduction = reduction,
    k = k, prop = prop, d_requested = d, d_eff = d_eff,
    refinement_scheme = refinement_scheme,
    fdr_weighting = fdr_weighting,
    seed = seed,
    identity_col = identity_col,
    batch_col = batch_col,
    sample_col = sample_col,
    condition_col = condition_col,
    target_condition = target_condition,
    reference_condition = reference_condition,
    n_excluded = n_excluded,
    milo_version = avail$version,
    extra_warnings = setdiff(warnings_all, design_recap$design_warnings),
    context = context
  )
}

#' Finaliser le resultat Milo canonique (Stage 14)
#'
#' Assemble l'objet canonique documente (en-tete de fichier) a partir des
#' composantes calculees par run_milo_da(). La provenance est PRODUITE ici
#' (regle 7 AGENTS.md) ; l'appelant l'append a l'etat partage.
#'
#' @param components Liste des composantes calculees (da_results, design.df,
#'   formule, contrast, neighbourhood_summary, nhood_assignment,
#'   sample_composition, identity_used, d_eff).
#' @param seurat_obj Objet Seurat analyse (identite + dataset provenance).
#' @param design_recap Recapitulatif du design Stage 13 consomme.
#' @param reduction Nom de la reduction utilisee.
#' @param k,prop,d_requested,d_eff Parametres de construction enregistres.
#' @param refinement_scheme,fdr_weighting Parametres miloR enregistres.
#' @param seed Graine enregistree.
#' @param identity_col,sample_col,condition_col,batch_col Colonnes utilisees.
#' @param target_condition,reference_condition Contraste teste.
#' @param n_excluded Nombre de cellules exclues (sample_id manquant).
#' @param milo_version Version miloR utilisee.
#' @param extra_warnings Avertissements du calcul (design warnins exclus).
#' @param context Contexte cite dans les messages d'erreur.
#' @return L'objet canonique (champs milo_contract_fields()).
#' @export
finalize_milo_result <- function(components,
                                 seurat_obj,
                                 design_recap,
                                 reduction,
                                 k, prop, d_requested, d_eff,
                                 refinement_scheme, fdr_weighting,
                                 seed,
                                 identity_col = NULL,
                                 batch_col = NULL,
                                 sample_col = NULL,
                                 condition_col = NULL,
                                 target_condition,
                                 reference_condition,
                                 n_excluded = 0L,
                                 milo_version = NA_character_,
                                 extra_warnings = character(0),
                                 context = "abondance differentielle Milo") {
  if (!is.list(components) || is.null(components$da_results) ||
      !is.data.frame(components$da_results)) {
    .milo_stop(
      "invalid_input",
      "finalize_milo_result() : composantes du calcul Milo absentes — run_milo_da() doit etre appelee d'abord."
    )
  }

  warnings_all <- unique(c(
    as.character(design_recap$design_warnings %||% character(0)),
    as.character(extra_warnings)
  ))
  status <- if (length(warnings_all)) "valid_with_warnings" else "valid"

  result <- list()
  result$type <- "milo_da"
  result$status <- status
  result$design <- design_recap
  result$parameters <- list(
    reduction = reduction,
    k = as.integer(k),
    prop = as.numeric(prop),
    d_requested = as.integer(d_requested),
    d_effective = as.integer(d_eff),
    refined = TRUE,
    refinement_scheme = refinement_scheme,
    fdr_weighting = fdr_weighting,
    seed = as.integer(seed),
    sample_id_column = sample_col,
    condition_column = condition_col,
    batch_column = if (is.null(batch_col)) NA_character_ else batch_col,
    batch_in_model = !is.null(batch_col),
    identity_column = if (is.null(identity_col)) NA_character_ else identity_col,
    identity_fraction_min = TS_DA_MILO_IDENTITY_FRACTION_MIN,
    display_alpha = TS_DA_MILO_DISPLAY_ALPHA,
    n_cells_excluded_missing_sample = as.integer(n_excluded),
    composition_unit = "sample"
  )
  result$tested_contrast <- list(
    formula = components$formule,
    contrast = components$contrast,
    target = target_condition,
    reference = reference_condition,
    interpretation = paste0(
      "logFC > 0 = voisinage enrichi en '", target_condition,
      "' ; logFC < 0 = enrichi en '", reference_condition, "'."
    )
  )
  result$model_specification <- list(
    design_cols = colnames(components$design.df),
    robust = TS_DA_MILO_ROBUST,
    min.mean = TS_DA_MILO_MIN_MEAN,
    fdr_weighting = fdr_weighting,
    intercept_type = "fixed",
    fail_on_error = FALSE,
    n_design_samples = nrow(components$design.df)
  )
  result$neighbourhood_summary <- components$neighbourhood_summary
  result$DA_table <- components$da_results
  result$nhood_assignment <- components$nhood_assignment
  result$sample_composition <- components$sample_composition
  result$package_versions <- list(
    miloR = milo_version,
    edgeR = as.character(utils::packageVersion("edgeR")),
    BiocNeighbors = as.character(utils::packageVersion("BiocNeighbors")),
    SingleCellExperiment = as.character(
      utils::packageVersion("SingleCellExperiment")
    ),
    SeuratObject = as.character(utils::packageVersion("SeuratObject")),
    R = paste(R.version$major, R.version$minor, sep = ".")
  )
  result$object_identity <- build_object_identity_v2(seurat_obj)

  result$warnings <- warnings_all

  entry <- new_provenance_entry(
    analysis_id = "sc-da-milo",
    method = "milo_neighbourhood_da",
    parameters = list(
      reduction = reduction,
      k = as.integer(k),
      prop = as.numeric(prop),
      d_effective = as.integer(d_eff),
      refinement_scheme = refinement_scheme,
      fdr_weighting = fdr_weighting,
      seed = as.integer(seed),
      model_formula = components$formule,
      contrast = components$contrast,
      target_condition = target_condition,
      reference_condition = reference_condition,
      identity_column = if (is.null(identity_col)) NA_character_ else identity_col,
      identity_fraction_min = TS_DA_MILO_IDENTITY_FRACTION_MIN,
      sample_id_column = sample_col,
      condition_column = condition_col,
      batch_column = if (is.null(batch_col)) NA_character_ else batch_col,
      n_design_samples = nrow(components$design.df),
      n_neighbourhoods = nrow(components$da_results),
      n_cells_excluded_missing_sample = as.integer(n_excluded),
      design_fingerprint = design_recap$design_fingerprint,
      composition_unit = "sample",
      miloR_version = milo_version,
      object_fingerprint = result$object_identity$fingerprint
    ),
    dataset = seurat_obj,
    cells_used = nrow(components$nhood_assignment),
    cells_excluded = as.integer(n_excluded),
    seed = seed,
    warnings = warnings_all
  )
  entry$analysis_type <- "milo_da"
  entry$status <- status
  entry$timestamp_utc <- format(entry$timestamp, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  result$provenance <- entry
  result$analysis_id <- "sc-da-milo"
  result$timestamp_utc <- entry$timestamp_utc

  result
}

#' Le resultat Milo est-il perime vis-a-vis de l'objet Seurat courant ?
#'
#' @param milo_result Resultat canonique.
#' @param seurat_obj Objet Seurat courant (ou tout objet a dimnames).
#' @return TRUE si les empreintes divergent, FALSE sinon, NA si
#'   indeterminable.
#' @export
milo_result_is_stale <- function(milo_result, seurat_obj) {
  if (is.null(milo_result)) return(NA)
  fp <- milo_result$object_identity$fingerprint %||% NULL
  if (is.null(fp) || is.null(seurat_obj)) return(NA)
  !identical(fp, velocity_object_fingerprint(seurat_obj))
}

#' Verifier que l'objet est un resultat Milo canonique
#'
#' Garde de contrat pour TOUT consommateur (vues Stage 14, vues croisees
#' Stage 16, rapport 4F). Verifie le type, le statut et la presence de la
#' table DA ; la peremption vis-a-vis de l'objet courant est refusee si
#' seurat_obj est fourni.
#'
#' @param milo_result Objet a verifier (resultat canonique attendu).
#' @param seurat_obj Objet Seurat courant optionnel (peremption).
#' @param context Contexte cite dans les messages d'erreur.
#' @return Le resultat, invisible (pipable, conforme au style assert_*).
#' @export
assert_milo_result <- function(milo_result,
                               seurat_obj = NULL,
                               context = "resultat Milo") {
  if (!is.list(milo_result) ||
      !identical(milo_result$type %||% NULL, "milo_da") ||
      is.null(milo_result$status)) {
    .milo_stop(
      "invalid_input",
      sprintf(
        paste0("Echec %s : resultat Milo canonique requis (run_milo_da() / ",
               "finalize_milo_result()) — recu : %s."),
        context,
        if (is.null(milo_result)) "NULL"
        else paste(class(milo_result), collapse = "/")
      )
    )
  }
  if (!milo_result$status %in% milo_validity_states()) {
    .milo_stop(
      "invalid_input",
      sprintf("Echec %s : statut Milo inconnu '%s'.", context, milo_result$status)
    )
  }
  if (milo_result$status %in% c("invalid_input", "compute_failed",
                                "design_not_eligible")) {
    .milo_stop(
      milo_result$status,
      sprintf("Echec %s : %s", context, milo_status_labels()[[milo_result$status]])
    )
  }
  if (!is.null(milo_result$DA_table) && !is.data.frame(milo_result$DA_table)) {
    .milo_stop(
      "invalid_input",
      sprintf("Echec %s : DA_table doit etre un data.frame.", context)
    )
  }
  if (!is.null(seurat_obj) && isTRUE(milo_result_is_stale(milo_result, seurat_obj))) {
    .milo_stop(
      "stale_against_current_seurat_object",
      sprintf("Echec %s : %s", context,
              milo_status_labels()[["stale_against_current_seurat_object"]])
    )
  }
  invisible(milo_result)
}

#' Resume de l'analyse Milo pour export CSV (Stage 14)
#'
#' Une ligne, colonnes stables : identifiants, statut, contraste, formule,
#' parametres de construction, couverture voisinages, versions, empreinte,
#' avertissements. Lecture directe de l'objet canonique — aucune deduction.
#'
#' @param milo_result Resultat canonique.
#' @return data.frame a une ligne, colonnes character.
#' @export
build_milo_summary <- function(milo_result) {
  r <- assert_milo_result(milo_result, context = "resume Milo")
  nh <- r$neighbourhood_summary %||% NULL

  data.frame(
    analysis_id = r$analysis_id %||% NA_character_,
    analysis_type = r$type %||% "milo_da",
    status = r$status %||% NA_character_,
    timestamp_utc = r$timestamp_utc %||% NA_character_,
    design_analysis_id = r$design$design_analysis_id %||% NA_character_,
    reduction = r$parameters$reduction %||% NA_character_,
    k = as.character(r$parameters$k %||% NA_integer_),
    prop = as.character(r$parameters$prop %||% NA_real_),
    d_effective = as.character(r$parameters$d_effective %||% NA_integer_),
    fdr_weighting = r$parameters$fdr_weighting %||% NA_character_,
    seed = as.character(r$parameters$seed %||% NA_integer_),
    model_formula = r$tested_contrast$formula %||% NA_character_,
    tested_contrast = r$tested_contrast$contrast %||% NA_character_,
    target_condition = r$tested_contrast$target %||% NA_character_,
    reference_condition = r$tested_contrast$reference %||% NA_character_,
    n_neighbourhoods = as.character(nh$n_neighbourhoods %||% NA_integer_),
    fraction_cells_in_nhoods = as.character(
      round(as.numeric(nh$fraction_cells_in_nhoods %||% NA_real_), 4)
    ),
    miloR_version = r$package_versions$miloR %||% NA_character_,
    object_fingerprint = r$object_identity$fingerprint %||% NA_character_,
    warnings = paste(r$warnings %||% character(0), collapse = " | "),
    stringsAsFactors = FALSE
  )
}

#' Export CSV de la table DA par voisinage (Stage 14)
#'
#' Une ligne par voisinage, tracee par analysis_id. Les colonnes identity /
#' identity_fraction sont DESCRIPTIVES (composition du voisinage) et ne
#' constituent PAS un DA par type cellulaire.
#'
#' @param milo_result Resultat canonique.
#' @return data.frame (n voisinages lignes).
#' @export
build_milo_da_table_export <- function(milo_result) {
  r <- assert_milo_result(milo_result, context = "export table DA Milo")
  da <- r$DA_table %||% NULL
  if (is.null(da) || !is.data.frame(da)) {
    .milo_stop(
      "invalid_input",
      "Aucune table DA disponible dans le resultat canonique."
    )
  }
  data.frame(
    da,
    analysis_id = rep(r$analysis_id %||% NA_character_, nrow(da)),
    timestamp_utc = rep(r$timestamp_utc %||% NA_character_, nrow(da)),
    stringsAsFactors = FALSE
  )
}

#' Export CSV du mappage cellule -> voisinages (Stage 14)
#'
#' Une ligne par cellule : nombre de voisinages contenant la cellule et
#' moyenne des logFC (descriptif, affichage sur embedding). Les cellules hors
#' voisinage portent n_neighbourhoods = 0 et mean_logFC = NA.
#'
#' @param milo_result Resultat canonique.
#' @return data.frame (n cellules analysees lignes).
#' @export
build_milo_nhood_assignment_export <- function(milo_result) {
  r <- assert_milo_result(milo_result, context = "export mappage cellules Milo")
  na_tab <- r$nhood_assignment %||% NULL
  if (is.null(na_tab) || !is.data.frame(na_tab)) {
    .milo_stop(
      "invalid_input",
      "Aucun mappage cellule -> voisinages disponible dans le resultat canonique."
    )
  }
  data.frame(
    na_tab,
    analysis_id = rep(r$analysis_id %||% NA_character_, nrow(na_tab)),
    timestamp_utc = rep(r$timestamp_utc %||% NA_character_, nrow(na_tab)),
    stringsAsFactors = FALSE
  )
}

#' Export CSV du contexte echantillon (Stage 14)
#'
#' Une ligne par echantillon : condition, batch, effectif total et effectif
#' couvert par les voisinages. Contexte de composition au niveau ECHANTILLON
#' — les cellules ne sont PAS des replicats biologiques.
#'
#' @param milo_result Resultat canonique.
#' @return data.frame (n echantillons lignes).
#' @export
build_milo_sample_composition_export <- function(milo_result) {
  r <- assert_milo_result(milo_result, context = "export contexte echantillon Milo")
  sc <- r$sample_composition %||% NULL
  if (is.null(sc) || !is.data.frame(sc)) {
    .milo_stop(
      "invalid_input",
      "Aucun contexte echantillon disponible dans le resultat canonique."
    )
  }
  data.frame(
    sc,
    analysis_id = rep(r$analysis_id %||% NA_character_, nrow(sc)),
    timestamp_utc = rep(r$timestamp_utc %||% NA_character_, nrow(sc)),
    stringsAsFactors = FALSE
  )
}

#' Nom de fichier d'export trace par l'identifiant d'analyse (Stage 14)
#'
#' @param milo_result Resultat canonique.
#' @param kind Prefixe descriptif (ex. "milo_da_table").
#' @param ext Extension ("csv", "rds").
#' @return Chaine "<kind>_<analysis_id>_<date>.<ext>".
#' @export
milo_export_filename <- function(milo_result, kind, ext) {
  if (is.null(milo_result) || !is.list(milo_result)) {
    .milo_stop(
      "invalid_input",
      "milo_export_filename() : resultat Milo canonique requis."
    )
  }
  aid <- milo_result$analysis_id %||% "sc-da-milo"
  sprintf(
    "%s_%s_%s.%s",
    as.character(kind)[1L],
    aid,
    format(Sys.Date(), "%Y-%m-%d"),
    as.character(ext)[1L]
  )
}

#' Surface publique figee de R/sc/sc_abundance_milo.R (Stage 14)
#'
#' Le test de freeze refuse toute fonction top-level non prefixee d'un point
#' qui ne figure pas dans cette liste.
#'
#' @return Vecteur character des noms de fonctions publiques.
#' @export
milo_public_api <- function() {
  c(
    "milo_contract_fields", "milo_validity_states", "milo_status_labels",
    "milo_error_state", "milo_available", "run_milo_da",
    "finalize_milo_result", "milo_result_is_stale", "assert_milo_result",
    "build_milo_summary", "build_milo_da_table_export",
    "build_milo_nhood_assignment_export",
    "build_milo_sample_composition_export", "milo_export_filename",
    "milo_public_api"
  )
}

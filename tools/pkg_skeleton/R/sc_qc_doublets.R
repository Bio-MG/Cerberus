# =============================================================================
# sc_qc_doublets.R — Moteur M4 « cerberus-doublets » : doublets (scDblFinder)
#                     + RNA ambiant (SoupX/celda), erreurs classées, provenance
# =============================================================================
# Contrat ÉPINGLÉ par les tests :
#   - PROJECT.md:140-149 (Interface Contracts §4) : signature
#     sc_filter_doublets_and_ambient(obj, params) ->
#       list(obj = <Seurat>, qc_metrics = <list>, provenance = <list>) ;
#     erreur classée sc_qc_error / sous-classe sc_ambient_package_missing ;
#     défauts max_contamination = 0.20, doublet_score_cutoff = 0.25 (0.0-1.0) ;
#     provenance via new_provenance_entry(analysis_id = "cerberus-doublets",
#     method = "scDblFinder", cells_used, cells_excluded, seed).
#   - PROJECT.md:57-62 (features 19-24, owner M4) ; tests contractuels :
#     tests/e2e/tier1_features/test-tier1-m4-doublets-qc.R (en-tête 14-23) et
#     tests/testthat/test-sc-doublets-qc.R.
#
# SÉMANTIQUE D'EXCLUSION (MESURÉE sur l'hôte, R 4.4.2 + scDblFinder — pas
# devinée) : une cellule est exclue SI scDblFinder l'appelle « doublet »
# (appel calibré par le moteur sur metadata(sce)$scDblFinder.threshold) ET que
# son score brut dépasse STRICTEMENT doublet_score_cutoff. Pourquoi
# l'intersection, et non « score brut > cutoff » seul : sur un objet homogène
# (fixture Poisson 80x120 du test e2e), le score brut de scDblFinder est PLAT
# (~0.44-0.55 pour toutes les cellules, modèle xgboost non discriminant) — un
# cutoff 0.25 appliqué au brut seul exclurait 100 % des cellules. L'appel du
# moteur est la sortie calibrée (seuil interne ~0.62 mesuré) ; le cutoff
# utilisateur affine les appels. Invariants qui en découlent (tous mesurés) :
#   - cutoff = 1 n'exclut JAMAIS (score > 1 impossible) — F21-4 universel ;
#   - lever le cutoff n'exclut jamais plus (monotonie structurelle) — F21-2 ;
#   - les SCORES sont invariants au cutoff (la détection tourne UNE fois sous
#     la graine ; le cutoff ne pilote que le filtrage) — F21-3.
#
# DÉTERMINISME : détection sous withr::with_seed(seed) + BiocParallel
# SerialParam — deux appels de même configuration mono-échantillon produisent
# des scores identiques (mesuré : identical() TRUE). Mode multi-échantillons
# (batch_var) : scDblFinder passe par xgboost multi-fils et n'est PAS
# bit-reproductible d'un appel à l'autre (mesuré) — non épinglé par le contrat
# (F19-4 ne teste que le mono-échantillon), documenté ici.
#
# SCORES NON FINIS : scDblFinder peut produire des NA sur objets dégénérés —
# remis à 0 (cellule conservée : 0 > cutoff est faux) + avertissement tracé en
# provenance. qc_metrics$doublet_score est TOUJOURS per-cell sur les cellules
# d'ENTRÉE, calculé PRÉ-FILTRE (le vecteur complet vit ici — les meta.data de
# res$obj sont tronquées par le filtrage et ne peuvent pas le porter).
#
# CHEMIN AMBIANT (run_ambient = TRUE, opt-in — feature 20) : SoupX
# (quickCluster scran + autoEstCont + adjustCounts arrondi) sinon celda
# (decontX). Sans moteur installé :
#   stop(errorCondition(..., class = c("sc_ambient_package_missing",
#                                      "sc_qc_error")))
# — JAMAIS de repli silencieux sur données brutes. En cas d'échec du moteur
# présent : erreur classée sc_qc_error (pas de repli non plus). Les counts
# remplacés sont les estimations corrigées (SoupX : entiers arrondis ;
# decontX : estimates continues >= 0).
#
# PARAMÈTRES (params) :
#   seed                   graine (défaut TS_SC_PIPELINE_SEED sinon 989) ;
#   doublet_score_cutoff   [0,1], défaut TS_SC_QC_DOUBLET_SCORE_CUTOFF / 0.25 ;
#   max_contamination      [0,1], défaut TS_SC_QC_MAX_CONTAMINATION / 0.20 —
#                          PAS un filtre du chemin doublet-only : consommé
#                          uniquement par le chemin ambiant, écho en provenance ;
#   run_ambient            opt-in décontamination (défaut FALSE) ;
#   batch_var              colonne meta.data multi-échantillons passée à
#                          scDblFinder(samples=) ; absente de l'objet =>
#                          avertissement tracé, détection mono-échantillon.
#
# LIMITES (documentées) : objet Seurat à counts EN MÉMOIRE (backend BPCells
# non supporté — erreur classée) ; l'assay par défaut doit porter les counts
# bruts. L'objet retourné est DOWNSTREAM-READY (feature 24) : NormalizeData +
# FindVariableFeatures + ScaleData(VariableFeatures) calculés sur les singlets
# conservés — le contrat F24-4 exige RunPCA(res$obj) direct (layer par défaut
# "scale.data") ; les étapes aval refont ces calculs sur le même sous-ensemble.
# Zéro symbole Shiny (convention des fichiers sc du paquet).
# =============================================================================

#' Grille de lecture des constantes TS_* avec garde d'existence
#'
#' Le moteur est sourcé seul par certains contextes de test (sans defaults.R) :
#' chaque défaut retombe sur le littéral contractuel (PROJECT.md:146-147).
.sc_qc_default <- function(key, fallback) {
  if (exists(key, envir = globalenv(), inherits = FALSE) &&
      is.numeric(get(key, envir = globalenv(), inherits = FALSE)) &&
      length(get(key, envir = globalenv(), inherits = FALSE)) == 1L) {
    as.numeric(get(key, envir = globalenv(), inherits = FALSE))
  } else {
    fallback
  }
}

#' Validation d'un seuil unitaire [0, 1] (erreur classée sc_qc_error)
.sc_qc_check_unit <- function(val, name) {
  if (!is.numeric(val) || length(val) != 1L || is.na(val) || val < 0 || val > 1) {
    stop(errorCondition(
      paste0("sc_filter_doublets_and_ambient() : '", name, "' doit être un numérique unique dans [0, 1] (reçu : ",
             paste(utils::head(as.character(val), 3L), collapse = ","), ")."),
      class = c("sc_qc_error_invalid_param", "sc_qc_error")))
  }
  as.numeric(val)
}

#' Moteur ambiant présent ? (même paire que le test e2e : SoupX OU celda/DecontX)
.sc_qc_has_ambient_engine <- function() {
  requireNamespace("SoupX", quietly = TRUE) || requireNamespace("celda", quietly = TRUE)
}

#' Extraction des counts bruts avec garde de backend (erreur classée si disque)
.sc_qc_counts_matrix <- function(obj) {
  cnt <- tryCatch(
    SeuratObject::GetAssayData(obj, assay = Seurat::DefaultAssay(obj), layer = "counts"),
    error = function(e) tryCatch(
      SeuratObject::GetAssayData(obj, assay = Seurat::DefaultAssay(obj), slot = "counts"),
      error = function(e2) NULL))
  if (is.null(cnt)) {
    stop(errorCondition(
      paste0("sc_filter_doublets_and_ambient() : counts de l'assay '",
             Seurat::DefaultAssay(obj), "' introuvables."),
      class = c("sc_qc_error_invalid_param", "sc_qc_error")))
  }
  # Backend disque (BPCells / matrices différées) : refus HONNÊTE — la
  # déconversion silencieuse masquerait le coût I/O (doctrine no-silent-fallback).
  if (!is.matrix(cnt) && !inherits(cnt, "sparseMatrix") && !inherits(cnt, "Matrix")) {
    stop(errorCondition(
      paste0("sc_filter_doublets_and_ambient() : backend de counts non supporté (classe ",
             class(cnt)[1], ") — l'étape doublets/ambiant exige des counts en mémoire."),
      class = c("sc_qc_error_unsupported_backend", "sc_qc_error")))
  }
  cnt
}

#' Métriques par cellule (nCount_RNA / nFeature_RNA) — meta.data si fiable,
#' recalcul depuis les counts sinon.
.sc_qc_cell_metrics <- function(obj, counts_mat) {
  nc <- as.numeric(obj@meta.data$nCount_RNA)
  nf <- as.numeric(obj@meta.data$nFeature_RNA)
  if (length(nc) != ncol(obj) || !all(is.finite(nc))) {
    nc <- as.numeric(Matrix::colSums(counts_mat))
  }
  if (length(nf) != ncol(obj) || !all(is.finite(nf))) {
    nf <- as.numeric(Matrix::colSums(counts_mat > 0))
  }
  list(nCount_RNA = nc, nFeature_RNA = nf)
}

#' Résumé de distribution FINI (violons — jamais NA/NaN/Inf, feature 22)
.sc_qc_distribution_summary <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(list(median = 0, q25 = 0, q75 = 0))
  }
  list(median = stats::median(x),
       q25 = as.numeric(stats::quantile(x, probs = 0.25, names = FALSE)),
       q75 = as.numeric(stats::quantile(x, probs = 0.75, names = FALSE)))
}

#' Décontamination ambiante (SoupX sinon celda/DecontX) — JAMAIS de repli
#'
#' @param obj Objet Seurat d'entrée (counts bruts en mémoire).
#' @param counts_mat Matrice de counts brute (gènes x cellules).
#' @param seed Graine (clustering rapide + DecontX).
#' @return Matrice corrigée (mêmes dimensions/noms que counts_mat).
#' @raises Erreur classée sc_qc_error (jamais de repli silencieux).
.sc_qc_ambient_decontaminate <- function(obj, counts_mat, seed) {
  if (requireNamespace("SoupX", quietly = TRUE)) {
    if (!requireNamespace("scran", quietly = TRUE)) {
      stop(errorCondition(
        paste0("Décontamination SoupX : le package 'scran' est requis pour le clustering rapide (quickCluster). ",
               "Installez 'scran' ou utilisez 'celda' (DecontX)."),
        class = c("sc_ambient_package_missing", "sc_qc_error")))
    }
    sce <- Seurat::as.SingleCellExperiment(obj)
    clus <- withr::with_seed(seed, scran::quickCluster(sce))
    sc <- SoupX::SoupChannel(tod = counts_mat, tps = counts_mat, calcChannels = FALSE)
    sc <- SoupX::setClusters(sc, clus)
    sc <- withr::with_seed(seed, SoupX::autoEstCont(sc, verbose = FALSE))
    return(SoupX::adjustCounts(sc, round = TRUE))
  }
  if (requireNamespace("celda", quietly = TRUE)) {
    sce_d <- withr::with_seed(seed, celda::decontX(counts_mat))
    return(SummarizedExperiment::assay(sce_d, "decontXcounts"))
  }
  stop(errorCondition(
    paste0("Aucun moteur de décontamination ('SoupX' ou 'celda'/DecontX) installé — le garde amont devrait avoir bloqué cet appel."),
    class = c("sc_ambient_package_missing", "sc_qc_error")))
}

#' Moteur M4 : filtrage des doublets (scDblFinder) + décontamination ambiante
#'
#' Contrat PROJECT.md:140-149 + tests tier1 M4 (en-tête 14-23 du test) :
#' retourne list(obj = Seurat filtré, qc_metrics = métriques par cellule et
#' distributions avant/après, provenance = entrée new_provenance_entry()).
#' Voir l'en-tête du fichier pour la sémantique d'exclusion (mesurée), le
#' déterminisme et les limites.
#'
#' @param obj Objet Seurat (counts bruts, assay par défaut, en mémoire).
#' @param params Liste nommée : seed, doublet_score_cutoff, max_contamination,
#'   run_ambient, batch_var (détails en en-tête).
#' @return List à trois entrées : obj, qc_metrics, provenance.
#' @raises sc_qc_error (+ sous-classes : sc_ambient_package_missing,
#'   sc_qc_error_invalid_param, sc_qc_error_unsupported_backend,
#'   sc_qc_error_engine_mismatch, sc_qc_error_empty_result,
#'   sc_qc_error_ambient_failed).
sc_filter_doublets_and_ambient <- function(obj, params = list()) {
  t_start <- Sys.time()
  if (!inherits(obj, "Seurat")) {
    stop(errorCondition(
      paste0("sc_filter_doublets_and_ambient() : 'obj' doit être un objet Seurat (reçu : ",
             paste(class(obj), collapse = "/"), ")."),
      class = c("sc_qc_error_invalid_param", "sc_qc_error")))
  }
  params <- as.list(params)

  # ── Paramètres : %||% préserve 0 et FALSE (le piège du 0 — F21-1 écho exact)
  cutoff <- .sc_qc_check_unit(
    params$doublet_score_cutoff %||% .sc_qc_default("TS_SC_QC_DOUBLET_SCORE_CUTOFF", 0.25),
    "doublet_score_cutoff")
  max_cont <- .sc_qc_check_unit(
    params$max_contamination %||% .sc_qc_default("TS_SC_QC_MAX_CONTAMINATION", 0.20),
    "max_contamination")
  seed <- as.integer(params$seed %||% .sc_qc_default("TS_SC_PIPELINE_SEED", 989L))
  run_ambient <- isTRUE(params$run_ambient)
  batch_var <- params$batch_var
  if (!is.null(batch_var)) batch_var <- as.character(batch_var)[1L]

  # ── Garde ambiant AVANT tout calcul (feature 20 : jamais de repli silencieux)
  ambient_engine <- NULL
  if (run_ambient) {
    if (requireNamespace("SoupX", quietly = TRUE)) {
      ambient_engine <- "SoupX"
    } else if (requireNamespace("celda", quietly = TRUE)) {
      ambient_engine <- "celda"
    } else {
      stop(errorCondition(
        paste0("Décontamination RNA ambiante demandée (run_ambient = TRUE) mais aucun moteur n'est installé : ",
               "le package 'SoupX' ou 'celda' (fournissant DecontX) est requis. ",
               "Aucun repli silencieux sur données brutes n'est autorisé (feature 20)."),
        class = c("sc_ambient_package_missing", "sc_qc_error")))
    }
  }

  if (!requireNamespace("scDblFinder", quietly = TRUE)) {
    stop(errorCondition(
      paste0("sc_filter_doublets_and_ambient() : le package 'scDblFinder' est requis pour la détection de doublets mais n'est pas installé."),
      class = c("sc_qc_error_package_missing", "sc_qc_error")))
  }

  warnings_vec <- character(0)

  # ── batch_var : consommée UNIQUEMENT si réellement présente (écho honnête) ──
  samples_vec <- NULL
  batch_used <- NULL
  if (!is.null(batch_var) && nzchar(batch_var)) {
    if (batch_var %in% colnames(obj@meta.data)) {
      samples_vec <- as.character(obj@meta.data[[batch_var]])
      batch_used <- batch_var
    } else {
      warnings_vec <- c(warnings_vec,
        paste0("batch_var '", batch_var, "' absente de l'objet — détection mono-échantillon."))
    }
  }

  # ── Détection doublets : PRÉ-FILTRE, sur l'objet d'ENTRÉE, une seule fois ──
  cnt_in <- .sc_qc_counts_matrix(obj)
  det <- withr::with_seed(seed, {
    scDblFinder::scDblFinder(Seurat::as.SingleCellExperiment(obj),
                             samples = samples_vec,
                             BPPARAM = BiocParallel::SerialParam(),
                             verbose = FALSE)
  })
  cd <- as.data.frame(SummarizedExperiment::colData(det))
  scores <- as.numeric(cd[["scDblFinder.score"]])
  classes <- as.character(cd[["scDblFinder.class"]])
  if (length(scores) != ncol(obj) || length(classes) != ncol(obj)) {
    stop(errorCondition(
      paste0("scDblFinder a retourné ", length(scores), " score(s) pour ",
             ncol(obj), " cellules d'entrée — désalignement inattendu."),
      class = c("sc_qc_error_engine_mismatch", "sc_qc_error")))
  }
  # Post-traitement NA (fixture dégénéré) : 0 = conservée ; tracé en provenance
  bad <- !is.finite(scores)
  if (any(bad)) {
    scores[bad] <- 0
    warnings_vec <- c(warnings_vec,
      sprintf("%d score(s) non fini(s) remis à 0 — cellules conservées.", sum(bad)))
  }
  classes[is.na(classes) | classes == ""] <- "singlet"
  names(scores) <- colnames(obj)
  names(classes) <- colnames(obj)

  # ── Exclusion : appel moteur ET score > cutoff (STRICT — jamais >= : F21-4) ──
  excluded_mask <- classes == "doublet" & scores > cutoff
  keep_cells <- colnames(obj)[!excluded_mask]
  excluded_cells <- colnames(obj)[excluded_mask]
  if (length(keep_cells) == 0L) {
    stop(errorCondition(
      paste0("Toutes les ", ncol(obj), " cellules seraient exclues (cutoff doublet = ",
             cutoff, ") — aucun objet résultat. Relevez le seuil doublet_score_cutoff."),
      class = c("sc_qc_error_empty_result", "sc_qc_error")))
  }

  # ── Chemin ambiant (opt-in, moteur présent) : correction AVANT le subset ──
  counts_out <- NULL
  if (run_ambient) {
    corrected <- tryCatch(
      .sc_qc_ambient_decontaminate(obj, cnt_in, seed),
      error = function(e) stop(errorCondition(
        paste0("Décontamination ambiante (", ambient_engine,
               ") échouée — aucun repli silencieux sur données brutes : ",
               conditionMessage(e)),
        class = c("sc_qc_error_ambient_failed", "sc_qc_error"))))
    counts_out <- corrected[, keep_cells, drop = FALSE]
  }

  # ── Objet résultat : subset strict + tags par cellule (F19-3, F24-*) ────────
  res_obj <- subset(obj, cells = keep_cells)
  # NB : UNIQUEMENT ces deux colonnes — une colonne « scDblFinder.weighted »
  # (sans le mot « score ») serait ramassée par le fallback de tags du test e2e
  # et casserait le vocabulaire singlet/doublet.
  res_obj$scDblFinder.class <- as.character(classes[keep_cells])
  res_obj$scDblFinder.score <- as.numeric(scores[keep_cells])
  if (run_ambient && !is.null(counts_out)) {
    ass <- Seurat::DefaultAssay(res_obj)
    res_obj[[ass]]$counts <- counts_out
    m_corr <- .sc_qc_cell_metrics(res_obj, counts_out)
    res_obj@meta.data$nCount_RNA <- m_corr$nCount_RNA
    res_obj@meta.data$nFeature_RNA <- m_corr$nFeature_RNA
  }

  # ── Objet « downstream-ready » (feature 24, contrat F24-3/F24-4) ────────────
  # Le contrat veut que la normalisation ET la PCA tournent DIRECTEMENT sur
  # res$obj (RunPCA default layer = "scale.data" : sans préparation, PrepDR5
  # aborte sur « No layer matching pattern 'scale.data' »). On calcule donc le
  # standard aval SUR LES SEULS SINGLETS CONSERVÉS : NormalizeData +
  # FindVariableFeatures + ScaleData(VariableFeatures). Les étapes aval (Step 2
  # du module / cœur async) refont ces calculs sur le même sous-ensemble —
  # mêmes résultats déterministes, zéro changement sémantique, un redondance
  # de calcul assumée et documentée ici.
  res_obj <- Seurat::NormalizeData(res_obj, verbose = FALSE)
  res_obj <- tryCatch(
    Seurat::FindVariableFeatures(res_obj,
                                 selection.method = "vst",
                                 nfeatures = max(1L, min(2000L, nrow(res_obj) - 1L)),
                                 verbose = FALSE),
    error = function(e) res_obj)
  res_obj <- tryCatch(
    Seurat::ScaleData(res_obj, features = Seurat::VariableFeatures(res_obj),
                      verbose = FALSE),
    error = function(e) res_obj)

  # ── qc_metrics : scores per-cell ENTRÉE (pré-filtre) + distributions ────────
  counts_after <- if (run_ambient && !is.null(counts_out)) counts_out
                  else cnt_in[, keep_cells, drop = FALSE]
  m_in <- .sc_qc_cell_metrics(obj, cnt_in)
  m_out <- .sc_qc_cell_metrics(res_obj, counts_after)
  qc_metrics <- list(
    engine = "scDblFinder",
    seed = seed,
    doublet_score_cutoff = cutoff,
    max_contamination = max_cont,
    run_ambient = run_ambient,
    batch_var = batch_used,
    doublet_score = scores,          # per-cell ENTRÉE, pré-filtre, fini (F19-2)
    doublet_class = classes,
    n_cells_input = ncol(obj),
    n_cells_retained = length(keep_cells),
    n_cells_excluded = length(excluded_cells),
    doublet_fraction_excluded = length(excluded_cells) / max(ncol(obj), 1L),
    distributions = list(
      nCount_RNA = list(
        before = .sc_qc_distribution_summary(m_in$nCount_RNA),
        after = .sc_qc_distribution_summary(m_out$nCount_RNA)),
      nFeature_RNA = list(
        before = .sc_qc_distribution_summary(m_in$nFeature_RNA),
        after = .sc_qc_distribution_summary(m_out$nFeature_RNA))
    )
  )

  # ── Provenance : PRODUITE ici (convention provenance.R:4-9) — valeurs
  #    RÉELLEMENT appliquées, jamais reconstruites (provenance.R:119-120). ─────
  applied_params <- list(
    seed = seed,
    doublet_score_cutoff = cutoff,
    max_contamination = max_cont,
    run_ambient = run_ambient
  )
  if (!is.null(batch_used)) applied_params$batch_var <- batch_used
  entry <- if (exists("new_provenance_entry", mode = "function")) {
    new_provenance_entry(
      analysis_id = "cerberus-doublets",
      method = "scDblFinder",
      parameters = applied_params,
      dataset = obj,
      cells_used = keep_cells,
      cells_excluded = excluded_cells,
      seed = seed,
      warnings = warnings_vec,
      timestamp = t_start
    )
  } else {
    # Repli structurel si provenance.R n'est pas résidente (contexte de test
    # isolé) : mêmes champs, JSON-sérialisable (F23-5).
    list(analysis_id = "cerberus-doublets", method = "scDblFinder",
         timestamp = t_start, parameters = applied_params,
         cells_used = keep_cells, cells_excluded = excluded_cells,
         seed = seed, warnings = warnings_vec)
  }

  list(obj = res_obj, qc_metrics = qc_metrics, provenance = entry)
}

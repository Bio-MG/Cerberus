# =============================================================================
# tools/pkg_skeleton/R/batch_correction.R — Correction de batch par ComBat-seq (STAT-S1)
# =============================================================================
# Contrat gelé : docs/contracts/BATCH_CORRECTION_CONTRACT.md
# Gelé par    : tests/testthat/test-bulk-batch-correction-contract-freeze.R
#
# POURQUOI ComBat-seq et pas ComBat classique
#   ComBat-seq (sva) modélise les counts par une loi binomiale négative et
#   corrige SUR LES COUNTS BRUTS. ComBat classique s'applique sur une matrice
#   transformée (log/VST) et peut écraser le signal biologique : il ne sait pas
#   distinguer un effet lot d'une différence de groupe. ComBat-seq accepte une
#   variable `group` (condition biologique) qui est PRÉSERVÉE par la correction.
#   C'est exactement l'usage visé ici : retirer la signature lot sans toucher à
#   la condition.
#
# Position dans le pipeline bulk
#   counts -> filter_bulk_counts() -> [run_combat_seq()] -> build_dds() -> VST
#   L'étage est OPTIONNEL : s'il n'est pas appliqué, le pipeline est inchangé
#   à l'octet près (règle dure n°1 : zéro changement de comportement).
#
# Garde-fous
#   - La matrice d'entrée DOIT être des counts bruts. Une matrice transformée
#     (VST / log2-CPM) est REFUSÉE : c'est l'inverse exact de la garde de
#     bulk_batch_qc.R (qui refuse les counts bruts pour la PCA/variance). Les
#     deux gardes sont volontairement symétriques et se citent mutuellement.
#   - Chaque niveau de lot doit porter au moins
#     TS_BULK_BATCH_MIN_SAMPLES_PER_BATCH échantillons : ComBat-seq estime une
#     dispersion par gène ET par lot, un lot à 1 échantillon n'est pas
#     estimable. Vérifié AVANT l'appel pour renvoyer un message français
#     plutôt qu'une erreur anglaise de sva.
#   - Collinéarité totale lot × condition : la correction est REFUSÉE (blocage),
#     car elle ne peut alors pas séparer l'effet lot de l'effet groupe —
#     réutilise check_design_confounding() via bulk_batch_design_check().
#
# Dépendance
#   sva (Bioconductor). Import PAESSEUX : aucun requireNamespace() au source,
#   uniquement à l'appel — le fichier reste chargeable sans sva installé.
#
# Erreurs : classées `bulk_batch_correction_error` (français, call. = FALSE),
#   états : invalid_input | not_raw_counts | degenerate_batch |
#           missing_dependency | compute_failed
# =============================================================================

#' Surface publique figée du domaine correction de batch (gel par test de freeze)
bulk_batch_correction_public_api <- function() {
  c("bulk_batch_correction_public_api",
    "bulk_assert_raw_counts",
    # O16 (2026-10-27) — détection TPM/FPKM à l'upload (non bloquante,
    # complète le garde bulk_assert_raw_counts)
    "bulk_counts_transformed_signature",
    "bulk_batch_correction_design",
    "bulk_batch_correction_label",
    "run_combat_seq",
    "plot_batch_correction_pca")
}

#' Exiger des COUNTS BRUTS (inverse exact de bulk_assert_transformed_matrix)
#'
#' Heuristique : une matrice de counts est ENTIÈRE par construction (comptages).
#' On exige donc que la fraction de valeurs exactement entières dépasse 95 %.
#' Une matrice VST / log2-CPM / TPM est continue (log2 d'un rapport) : sa
#' fraction d'entiers est quasi nulle. Le seuil de 95 % est le MÊME que celui
#' de `bulk_assert_transformed_matrix()` (tools/pkg_skeleton/R/bulk_batch_qc.R), appliqué
#' dans l'autre sens — les deux gardes sont symétriques et ne peuvent pas
#' accepter la même matrice.
#'
#' @param mat Matrice numérique (gènes x échantillons), counts bruts.
#' @param context Contexte cité dans le message d'erreur.
#' @param allow_non_integer Autoriser EXPLICITEMENT des valeurs non entières
#'   (l'appelant assume alors l'arrondi). Défaut FALSE : toute valeur non
#'   entière est refusée, parce que l'arrondir changerait les chiffres analysés.
#'   Avec TRUE, l'ancienne règle s'applique (>= 95 % d'entiers requis).
#' @return La matrice, invisible — sinon stop classé `bulk_batch_correction_error`.
bulk_assert_raw_counts <- function(mat, context = "correction de batch",
                                   allow_non_integer = FALSE) {
  if (is.null(mat) || !is.matrix(mat) || !is.numeric(mat)) {
    stop(errorCondition(sprintf("Échec %s : une matrice numérique de counts bruts est requise (reçu : %s).",
                 context, if (is.null(mat)) "NULL" else paste(class(mat), collapse = "/")),
                 class = "bulk_batch_correction_error", state = "invalid_input"))
  }
  if (ncol(mat) < 2L) {
    stop(errorCondition(sprintf("Échec %s : au moins 2 échantillons requis (reçu : %d).",
                 context, ncol(mat)),
                 class = "bulk_batch_correction_error", state = "invalid_input"))
  }
  vals <- as.vector(mat)
  if (anyNA(vals)) {
    stop(errorCondition(sprintf("Échec %s : la matrice contient des valeurs manquantes (NA) — ComBat-seq ne les accepte pas.",
                 context),
                 class = "bulk_batch_correction_error", state = "invalid_input"))
  }
  fin <- vals[is.finite(vals)]
  if (length(fin) == 0L) {
    stop(errorCondition(sprintf("Échec %s : la matrice ne contient aucune valeur finie.", context),
                 class = "bulk_batch_correction_error", state = "invalid_input"))
  }
  if (any(fin < 0)) {
    stop(errorCondition(sprintf("Échec %s : la matrice contient des valeurs négatives — des counts ne peuvent pas être négatifs.",
                 context),
                 class = "bulk_batch_correction_error", state = "invalid_input"))
  }
  frac_int <- mean(abs(fin - round(fin)) < 1e-8)

  # PLOT-S6c (P0, audit §2an) — par DÉFAUT, aucune valeur non entière n'est
  # tolérée. L'ancienne règle (>= 95 % d'entiers) laissait passer une matrice
  # « presque entière », et l'appelant (build_dds) arrondissait alors ces
  # quelques valeurs avec un simple warning — dans les branches edgeR/limma,
  # SANS même un avertissement. Les chiffres analysés n'étaient donc plus ceux
  # fournis, sans trace. `allow_non_integer = TRUE` rétablit explicitement
  # l'ancienne tolérance : l'arrondi redevient possible, mais il est DÉCLARÉ.
  if (!isTRUE(allow_non_integer)) {
    if (frac_int < 1) {
      n_bad <- sum(abs(fin - round(fin)) >= 1e-8)
      dev   <- max(abs(fin - round(fin)))
      stop(errorCondition(paste0(
        "Échec ", context, " : ", n_bad, " valeur(s) non entière(s) sur ",
        length(fin), " (écart maximal ", format(dev, digits = 3), "). ",
        "Arrondir modifierait les chiffres analysés sans qu'ils soient ceux ",
        "fournis. Fournissez des counts BRUTS (une matrice normalisée / VST ",
        "n'a pas de sens pour un test de comptage). Si l'arrondi est ",
        "réellement voulu, passez allow_non_integer = TRUE."),
        class = "bulk_batch_correction_error", state = "not_raw_counts"))
    }
  } else if (frac_int <= 0.95) {
    stop(errorCondition(paste0("Échec ", context, " : cette matrice ne ressemble PAS à des counts bruts ",
                 "(valeurs continues — seule ", sprintf("%.1f%%", 100 * frac_int),
                 " des valeurs sont entières). ComBat-seq s'applique sur les counts BRUTS, ",
                 "AVANT la transformation VST. Fournissez la matrice issue du filtrage (étape 1), ",
                 "pas une matrice VST / log2-CPM."),
                 class = "bulk_batch_correction_error", state = "not_raw_counts"))
  }
  invisible(mat)
}

#' Signature « matrice transformée » à l'UPLOAD (O16, 2026-10-27)
#'
#' Détecteur NON BLOQUANT qui complète `bulk_assert_raw_counts()` : le garde
#' aval refuse l'analyse APRÈS coup (le piège mesuré du 2026-10-24 —
#' docs/audits/COUNTS_NON_INTEGER_2026-10-24.md — n'était découvert qu'au lancement
#' de la DE) ; ce détecteur classe la matrice DÈS L'IMPORT pour qu'une bannière
#' d'avertissement précède toute analyse. Trois verdicts :
#'   - "raw"                 : toutes les valeurs finies sont entières >= 0 ;
#'   - "tpm_fpkm_likely"     : non entière + signature TPM/FPKM/CPM (valeurs
#'     positives dans ]0;1[ ET somme médiane des colonnes dans la fenêtre
#'     [TS_BULK_TPM_COLSUM_LOW, TS_BULK_TPM_COLSUM_HIGH] — colonnes ≈1e6) ;
#'   - "transformed_generic" : non entière sans cette signature (VST,
#'     log2-CPM, ...) ou valeurs négatives.
#' Ne s'arrête JAMAIS (un détecteur qui plante ne prévient de rien) : entrée
#' invalide ⇒ verdict "unknown".
#'
#' @param mat Matrice numérique (gènes x échantillons).
#' @return list(verdict, n_values, n_non_integer, frac_integer, max_dev,
#'   frac_unit_interval, median_colsum, message) — message FR affichable tel
#'   quel dans une bannière.
#' @export
bulk_counts_transformed_signature <- function(mat) {
  out <- list(verdict = "unknown", n_values = 0L, n_non_integer = 0L,
              frac_integer = NA_real_, max_dev = NA_real_,
              frac_unit_interval = NA_real_, median_colsum = NA_real_,
              message = "entrée invalide (matrice numérique requise)")
  if (is.null(mat)) return(out)
  if (!is.matrix(mat)) mat <- tryCatch(as.matrix(mat), error = function(e) NULL)
  if (is.null(mat) || !is.numeric(mat)) return(out)
  vals <- as.vector(mat)
  fin <- vals[is.finite(vals)]
  out$n_values <- length(fin)
  if (length(fin) == 0L) return(out)
  if (any(fin < 0)) {
    out$verdict <- "transformed_generic"
    out$message <- paste0(
      "⚠️ Cette matrice ne ressemble PAS à des counts bruts (valeurs négatives ",
      "présentes — des counts ne peuvent pas être négatifs). L'analyse ",
      "différentielle sera refusée par le garde aval (bulk_assert_raw_counts) : ",
      "fournissez la matrice de counts bruts.")
    return(out)
  }
  is_int <- abs(fin - round(fin)) < 1e-8
  out$n_non_integer <- sum(!is_int)
  out$frac_integer <- mean(is_int)
  if (out$n_non_integer > 0L) {
    out$max_dev <- max(abs(fin[!is_int] - round(fin[!is_int])))
  } else {
    out$verdict <- "raw"
    out$message <- "counts bruts (toutes les valeurs finies sont entières >= 0)"
    return(out)
  }
  nz <- fin[fin > 0]
  out$frac_unit_interval <- if (length(nz) > 0L) mean(nz < 1) else 0
  cs <- colSums(mat)
  cs_pos <- cs[is.finite(cs) & cs > 0]
  out$median_colsum <- if (length(cs_pos) > 0L) stats::median(cs_pos) else NA_real_
  low <- if (exists("TS_BULK_TPM_COLSUM_LOW")) TS_BULK_TPM_COLSUM_LOW else 5e5
  high <- if (exists("TS_BULK_TPM_COLSUM_HIGH")) TS_BULK_TPM_COLSUM_HIGH else 5e6
  unit_frac <- if (exists("TS_BULK_TPM_UNIT_FRAC")) TS_BULK_TPM_UNIT_FRAC else 0.01
  tpm_like <- isTRUE(out$frac_unit_interval >= unit_frac) &&
    is.finite(out$median_colsum) && out$median_colsum >= low &&
    out$median_colsum <= high
  if (tpm_like) {
    out$verdict <- "tpm_fpkm_likely"
    out$message <- paste0(
      "⚠️ Matrice TPM/FPKM/CPM PROBABLE à l'import : ", length(fin) - length(nz),
      " valeurs nulles, ", sum(fin > 0 & fin < 1), " valeur(s) dans ]0;1[ (",
      sprintf("%.1f%%", 100 * out$frac_unit_interval),
      " des valeurs positives) et somme médiane des colonnes ≈ ",
      format(out$median_colsum, digits = 2, scientific = TRUE),
      " (signature ≈1e6). L'analyse différentielle sera REFUSÉE par le garde ",
      "aval (bulk_assert_raw_counts) : fournissez la matrice de counts bruts.")
  } else {
    out$verdict <- "transformed_generic"
    out$message <- paste0(
      "⚠️ Matrice continue détectée à l'import (", out$n_non_integer,
      " valeur(s) non entière(s) sur ", length(fin), " — VST / log2-CPM ?). ",
      "L'analyse différentielle sera REFUSÉE par le garde aval ",
      "(bulk_assert_raw_counts) : fournissez la matrice de counts bruts.")
  }
  out
}

#' Contrôle du plan de correction : lot × condition + faisabilité ComBat-seq
#'
#' Réutilise `bulk_batch_design_check()` (tools/pkg_skeleton/R/bulk_batch_qc.R) pour la
#' cross-table et la collinéarité — aucune logique de plan d'expérience n'est
#' dupliquée ici. Cette fonction AJOUTE ce qui est propre à ComBat-seq :
#'   - l'effectif minimal par lot (TS_BULK_BATCH_MIN_SAMPLES_PER_BATCH) ;
#'   - la recommandation d'utiliser `group=` (à savoir : uniquement si une
#'     condition est déclarée ET que le plan n'est pas entièrement collinéaire ;
#'     passer `group=` sur un plan collinéaire ferait disparaître l'effet lot
#'     au lieu de le corriger) ;
#'   - `can_apply`, verdict unique consommé par l'UI pour activer le bouton.
#'
#' @param metadata data.frame métadonnées échantillons.
#' @param batch_col Nom de la colonne de lot.
#' @param condition_col Nom de la colonne de condition (NULL = correction à
#'   l'aveugle, sans variable biologique à préserver).
#' @return list(design_check, batch_col, condition_col, batch_levels,
#'   n_batch_levels, min_samples_per_batch, min_samples_required, use_group,
#'   blocking_messages, warning_messages, can_apply).
bulk_batch_correction_design <- function(metadata, batch_col, condition_col = NULL) {
  chk <- bulk_batch_design_check(metadata, batch_col, condition_col)

  batch_vals <- as.character(metadata[[batch_col]])
  tbl        <- table(batch_vals)
  min_per    <- if (length(tbl) > 0L) as.integer(min(tbl)) else 0L

  blocking <- character(0)
  if (chk$n_batch_levels < 2L) {
    blocking <- c(blocking, sprintf(
      "Le lot « %s » ne contient qu'un seul niveau : il n'y a aucun effet lot à corriger.",
      batch_col))
  }
  if (min_per < TS_BULK_BATCH_MIN_SAMPLES_PER_BATCH) {
    blocking <- c(blocking, sprintf(
      paste0("Le lot le plus petit ne contient que %d échantillon(s) (minimum requis : %d). ",
             "ComBat-seq estime une dispersion par gène et par lot — ce lot n'est pas estimable."),
      min_per, TS_BULK_BATCH_MIN_SAMPLES_PER_BATCH))
  }
  if (isTRUE(chk$fully_collinear)) {
    blocking <- c(blocking, sprintf(
      paste0("Lot (« %s ») et condition (« %s ») sont entièrement collinéaires : ",
             "la correction ne peut PAS distinguer l'effet lot de l'effet groupe, ",
             "elle risquerait de supprimer le signal biologique. Correction refusée."),
      batch_col, condition_col))
  }

  # group= ne sert que si une condition est déclarée ET séparable du lot.
  use_group <- !is.null(condition_col) && !isTRUE(chk$fully_collinear)

  list(
    design_check          = chk,
    batch_col             = batch_col,
    condition_col         = condition_col,
    batch_levels          = names(tbl),
    n_batch_levels        = chk$n_batch_levels,
    min_samples_per_batch = min_per,
    min_samples_required  = TS_BULK_BATCH_MIN_SAMPLES_PER_BATCH,
    use_group             = use_group,
    blocking_messages     = blocking,
    warning_messages      = chk$warning_messages,
    can_apply             = length(blocking) == 0L
  )
}

#' Libellé de provenance décrivant la correction appliquée
#'
#' Pure construction de chaîne : la provenance est PRODUITE ici puis écrite par
#' l'appelant via `bulk_update_provenance()` (tools/pkg_skeleton/R/bulk_provenance.R). Le
#' libellé PRÉSERVE la normalisation déjà déclarée (il la préfixe) au lieu de
#' l'écraser — l'historique de provenance enregistre par ailleurs la
#' transition, donc aucune information n'est perdue.
#'
#' @param batch_col Colonne de lot utilisée.
#' @param condition_col Colonne de condition passée en `group=` (NULL si aucune).
#' @param previous_normalization Normalisation déjà déclarée (NA / NULL si aucune).
#' @return Character scalaire.
bulk_batch_correction_label <- function(batch_col, condition_col = NULL,
                                        previous_normalization = NULL) {
  core <- paste0("ComBat-seq (batch : ", as.character(batch_col))
  if (!is.null(condition_col) && length(condition_col) == 1L &&
      !is.na(condition_col) && nzchar(trimws(as.character(condition_col)))) {
    core <- paste0(core, " ; groupe : ", as.character(condition_col))
  }
  core <- paste0(core, ")")

  prev <- if (is.null(previous_normalization)) NA_character_ else as.character(previous_normalization)
  if (length(prev) == 1L && !is.na(prev) && nzchar(trimws(prev))) {
    paste0(trimws(prev), " + ", core)
  } else {
    core
  }
}

#' Appliquer ComBat-seq sur des counts bruts
#'
#' Wrapper mince autour de `sva::ComBat_seq()`, avec les garde-fous du dépôt
#' (counts bruts, effectif par lot, dépendance explicite) et la restauration
#' des dimnames. La sortie de ComBat-seq est conservée TELLE QUELLE — aucune
#' valeur n'est arrondie ni retouchée : c'est le résultat de la méthode, pas
#' une décision locale. (Mesuré : la sortie est entière dans la configuration
#' par défaut de sva 3.54.0.)
#'
#' @param counts_matrix Matrice counts bruts (gènes x échantillons).
#' @param batch Vecteur de lots, longueur = ncol(counts_matrix).
#' @param group Vecteur de condition biologique à PRÉSERVER (optionnel).
#' @param context Contexte cité dans les messages d'erreur.
#' @return Matrice corrigée, mêmes dimensions et mêmes dimnames que l'entrée.
run_combat_seq <- function(counts_matrix, batch, group = NULL,
                           context = "correction de batch") {
  bulk_assert_raw_counts(counts_matrix, context)

  if (length(batch) != ncol(counts_matrix)) {
    stop(errorCondition(sprintf("Échec %s : longueur du vecteur de lots (%d) différente du nombre d'échantillons (%d).",
                 context, length(batch), ncol(counts_matrix)),
                 class = "bulk_batch_correction_error", state = "invalid_input"))
  }

  b <- factor(as.character(batch))
  if (anyNA(b)) {
    stop(errorCondition(sprintf("Échec %s : le vecteur de lots contient des valeurs manquantes (NA).", context),
                 class = "bulk_batch_correction_error", state = "invalid_input"))
  }
  if (length(levels(b)) < 2L) {
    stop(errorCondition(sprintf("Échec %s : au moins 2 niveaux de lot sont requis (reçu : %d).",
                 context, length(levels(b))),
                 class = "bulk_batch_correction_error", state = "degenerate_batch"))
  }
  per_batch <- table(b)
  if (min(per_batch) < TS_BULK_BATCH_MIN_SAMPLES_PER_BATCH) {
    stop(errorCondition(sprintf("Échec %s : chaque niveau de lot doit porter au moins %d échantillons (le plus petit en a %d).",
                 context, TS_BULK_BATCH_MIN_SAMPLES_PER_BATCH, as.integer(min(per_batch))),
                 class = "bulk_batch_correction_error", state = "degenerate_batch"))
  }

  g <- NULL
  if (!is.null(group)) {
    if (length(group) != ncol(counts_matrix)) {
      stop(errorCondition(sprintf("Échec %s : longueur du vecteur de condition (%d) différente du nombre d'échantillons (%d).",
                   context, length(group), ncol(counts_matrix)),
                   class = "bulk_batch_correction_error", state = "invalid_input"))
    }
    g <- factor(as.character(group))
    if (anyNA(g)) {
      stop(errorCondition(sprintf("Échec %s : le vecteur de condition contient des valeurs manquantes (NA).", context),
                   class = "bulk_batch_correction_error", state = "invalid_input"))
    }
  }

  if (!requireNamespace("sva", quietly = TRUE)) {
    stop(errorCondition(paste0("Échec ", context, " : le package 'sva' est requis pour ComBat-seq. ",
                 "Installez-le depuis la racine du projet : BiocManager::install('sva')."),
                 class = "bulk_batch_correction_error", state = "missing_dependency"))
  }

  out <- tryCatch(
    sva::ComBat_seq(as.matrix(counts_matrix), batch = b, group = g),
    error = function(e) {
      stop(errorCondition(paste0("Échec ", context, " : ComBat-seq a échoué — ", conditionMessage(e)),
                   class = "bulk_batch_correction_error", state = "compute_failed"))
    }
  )

  out <- as.matrix(out)
  if (!identical(dim(out), dim(counts_matrix))) {
    stop(errorCondition(sprintf("Échec %s : ComBat-seq a renvoyé des dimensions inattendues (%s au lieu de %s).",
                 context, paste(dim(out), collapse = "x"), paste(dim(counts_matrix), collapse = "x")),
                 class = "bulk_batch_correction_error", state = "compute_failed"))
  }
  # ComBat_seq préserve normalement les dimnames ; on les réaffirme pour que le
  # contrat de sortie soit garanti, pas supposé.
  dimnames(out) <- dimnames(counts_matrix)
  out
}

#' Diagnostic avant / après : deux PCA côte à côte
#'
#' Consomme DEUX objets ggplot déjà construits par `plot_bulk_pca()`
#' (tools/pkg_skeleton/R/bulk_helpers.R) — aucun code de tracé n'est dupliqué ici, la
#' fonction ne fait que les composer. Les panneaux sont étiquetés A (avant) et
#' B (après) ; la légende de correspondance est dans le sous-titre.
#'
#' @param pca_before,pca_after Objets ggplot issus de plot_bulk_pca().
#' @param tr Fonction de traduction (optionnelle), appliquée aux libellés.
#' @return Objet patchwork.
plot_batch_correction_pca <- function(pca_before, pca_after, tr = NULL) {
  if (is.null(pca_before) || is.null(pca_after)) {
    stop(errorCondition("plot_batch_correction_pca() : deux objets ggplot sont requis (avant ET après correction).",
                 class = "bulk_batch_correction_error", state = "invalid_input"))
  }
  .tr <- function(x) if (is.function(tr)) tr(x) else x
  patchwork::wrap_plots(pca_before, pca_after, ncol = 2L) +
    patchwork::plot_annotation(
      title      = .tr("Diagnostic de correction de batch"),
      subtitle   = .tr("A : avant correction — B : après correction (lots attendus mélangés)"),
      tag_levels = "A"
    )
}

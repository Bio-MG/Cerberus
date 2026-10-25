# =============================================================================
# R/sc/sc_velocity.R — RNA Velocity validation, I/O & plotting
# (extracted from helpers_sc.R, Block 7 refactor)
# =============================================================================
# Pure functions: no Shiny reactivity. Called by:
#   - modules/sc/mod_sc_velocity.R (UI module)
#
# Depends on: Matrix, ggplot2 ; R/core/{io_helpers (fournit %||%), state,
# provenance} pour finalize_velocity_result().
# Sourced in app.R AFTER helpers_io.R, BEFORE modules/sc/*.R
# =============================================================================
#
# ── CONTRAT DE RESULTAT CANONIQUE (Stage 8 — Velocity 3B-1) ─────────────────
# La chaine import -> validation -> alignement produit UN objet canonique,
# assemble par finalize_velocity_result() a partir de la structure validee de
# validate_velocity_matrices() (dont il est un SUR-ENSEMBLE : tous les champs
# historiques restent presents pour compatibilite). Champs du contrat :
#
#   type                  "rna_velocity" (constant)
#   status                etat de validite technique, voir
#                         velocity_validity_states() / velocity_status_labels()
#   spliced, unspliced    matrices alignees (genes x cellules) ; NULL si absent
#   ambiguous             matrice alignee optionnelle ; NULL si non fournie
#   cell_names            barcodes alignes (ordre Seurat)
#   gene_names            identifiants de genes alignes (ordre Seurat)
#   dimensions            c(cells = ncol(spliced), genes = nrow(spliced))
#   orientation           orientation detectee/apres transposition
#   cell_alignment        list(match_mode, n_input, n_matched, n_missing,
#                         overlap_raw, overlap_normalized, low_overlap_override)
#   gene_alignment        list(match_mode, n_input, n_matched, n_missing)
#   embedding_alignment   sortie de align_velocity_embedding() ou NULL
#   velocity_vectors      matrice dx/dy STRICTEMENT validee ou NULL (jamais
#                         fabriquee a partir des coordonnees)
#   vector_validation     list(field, ok, reason) ou NULL
#   input_summary         list(input_mode, files, velocity_source,
#                         velocity_method, orientation_declared,
#                         orientation_detected, strip_cell_suffix,
#                         strip_gene_version, allow_low_overlap, n_cells_input,
#                         n_genes_input)
#   object_identity       list(fingerprint, method, seurat_dims) — empreinte
#                         deterministe (voir velocity_object_fingerprint)
#   warnings              vecteur character (avertissements produits a la
#                         validation ; JAMAIS reconstruits apres coup)
#   provenance            entree new_provenance_entry() enrichie
#                         (analysis_type, input_mode, timestamp_utc, status)
#   analysis_id           identifiant d'analyse (ex. "sc-velocity")
#   timestamp_utc         horodatage UTC ISO-8601 de la validation
#   cell_mapping          data.frame d'alignement cellules (ou NULL)
#   gene_mapping          data.frame d'alignement genes (ou NULL)
#
# Un etat de validite exprime une validite TECHNIQUE exclusivement : il
# n'implique AUCUNE validite biologique. Les resultats perimes
# (stale_against_current_seurat_object) ne doivent jamais etre affiches ni
# exportes comme s'ils etaient current. La documentation complete du contrat
# (schema d'export, politique de compatibilite) est figee au Stage 10 dans
# docs/contracts/VELOCITY_RESULT_CONTRACT.md.
#
# ── API PUBLIQUE FIGEE (Stage 10) ───────────────────────────────────────────
# velocity_public_api() enumere la surface publique de ce fichier ; le test de
# freeze (tests/testthat/test-velocity-contract-freeze.R) extrait les
# affectations top-level et refuse TOUTE fonction publique ajoutée ou
# renommée hors de cette liste (et toute disparition). Les helpers internes,
# réservés à ce fichier, sont préfixés d'un point :
#   .VELOCITY_STATUS_STATES        — états du contrat (accédé via
#                                    velocity_validity_states())
#   .velocity_stop()               — erreur classée velocity_validation_error
#   .velocity_matrix_is_numeric()  — test numérique sparse-aware (dgCMatrix)
#   .read_lines_maybe_gz()         — lecture barcodes/features (I/O MTX)
#   .provenance_versions_string()  — aplatissage versions pour CSV export
# =============================================================================

# Champs du contrat de résultat canonique — SOURCE DE VÉRITÉ partagée par le
# code (ci-dessous), le test de freeze (test-velocity-contract-freeze.R) et la
# documentation (docs/contracts/VELOCITY_RESULT_CONTRACT.md). Toute
# modification doit passer par les TROIS simultanément.

#' Champs documentés du contrat de résultat velocity canonique
#'
#' Le résultat canonique est un SUR-ENSEMBLE de la structure validée (champs
#' historiques conservés pour compatibilité) ; cette liste fige les champs
#' contractuels documentés. Les champs absents restent NULL — jamais
#' fabriqués.
#'
#' @return Vecteur character des 22 champs contractuels.
#' @export
velocity_contract_fields <- function() {
    c(
        "type", "status", "spliced", "unspliced", "ambiguous",
        "cell_names", "gene_names", "dimensions", "orientation",
        "cell_alignment", "gene_alignment", "embedding_alignment",
        "velocity_vectors", "vector_validation", "input_summary",
        "object_identity", "warnings", "provenance",
        "analysis_id", "timestamp_utc", "cell_mapping", "gene_mapping"
    )
}

#' Surface publique figée de R/sc/sc_velocity.R (Stage 10)
#'
#' Le test de freeze refuse toute fonction top-level non préfixée d'un point
#' qui ne figure pas dans cette liste : l'API velocity ne peut évoluer que de
#' façon explicite (code + test + contrat documentaire en même temps).
#'
#' @return Vecteur character des noms de fonctions publiques.
#' @export
velocity_public_api <- function() {
    c(
        # Contrat / états
        "velocity_validity_states", "velocity_status_labels",
        "velocity_status_is_valid", "velocity_status_allows_matrices",
        "assert_velocity_result", "velocity_error_state",
        "velocity_contract_fields", "velocity_public_api",
        # I/O + validation d'entrée
        "detect_velocity_orientation", "validate_velocity_identifiers",
        "normalize_velocity_cell_barcodes", "normalize_velocity_gene_ids",
        "validate_velocity_matrices", "read_velocity_rds",
        "validate_velocity_rds_metadata", "read_velocity_mtx",
        "read_velocity_loom", "read_velocity_h5ad",
        "validate_precomputed_velocity_vectors", "align_velocity_embedding",
        # Résultat canonique / identité / provenance
        "finalize_velocity_result", "velocity_object_fingerprint",
        "velocity_result_is_stale", "build_velocity_validation_summary",
        "build_velocity_alignment_mapping",
        # Visualisations (consommatrices pures)
        "plot_velocity_phase_portrait", "plot_velocity_embedding",
        "plot_velocity_vector_field", "plot_velocity_coverage",
        "plot_velocity_alignment_qc",
        # Exports
        "build_velocity_provenance_export",
        "build_velocity_cell_vectors_export", "velocity_export_filename"
    )
}

# Etats de validite explicites du contrat (source de verite ; ne pas
# re-enseigner ailleurs — velocity_validity_states() est l'accesseur public).
.VELOCITY_STATUS_STATES <- c(
    "valid",
    "valid_no_vectors",
    "valid_partial_embedding",
    "invalid_input",
    "invalid_orientation",
    "invalid_cell_alignment",
    "invalid_gene_alignment",
    "invalid_vector_projection",
    "stale_against_current_seurat_object"
)

#' Etats de validite du contrat RNA velocity
#'
#' Les etats "valid*" expriment une validite TECHNIQUE (matrices alignees,
#' vecteurs verifies) et n'impliquent aucune validite biologique. Les etats
#' "invalid*" et "stale*" sont des echecs : ils sont leves comme erreurs
#' classees (velocity_validation_error) par la chaine de validation, sauf
#' "stale_against_current_seurat_object" qui est un etat derive au moment de
#' l'affichage (velocity_result_is_stale()).
#'
#' @return Vecteur character des 9 etats documentes du contrat.
#' @export
velocity_validity_states <- function() .VELOCITY_STATUS_STATES

#' Libelles francais des etats de validite (affichage utilisateur)
#'
#' Chaque libelle explique l'etat SANS suggerer une validite biologique a
#' partir d'une validite technique.
#'
#' @return Vecteur character nomme par etat (names = velocity_validity_states()).
#' @export
velocity_status_labels <- function() {
    c(
        valid = paste0(
            "Valide (technique) : matrices alignees sur l'objet Seurat et ",
            "vecteurs verifies. Validite technique uniquement — aucune ",
            "interpretation biologique n'est impliquee."
        ),
        valid_no_vectors = paste0(
            "Valide sans vecteurs : matrices alignees, aucun vecteur ",
            "pre-calcule fourni — fleches desactivees, coordonnees seules."
        ),
        valid_partial_embedding = paste0(
            "Valide partiel : les coordonnees d'embedding ne couvrent qu'une ",
            "partie des cellules alignees — la couverture est affichee ",
            "explicitement."
        ),
        invalid_input = paste0(
            "Entree invalide : fichiers ou matrices velocity inutilisables ",
            "(structure, identifiants, dimensions)."
        ),
        invalid_orientation = paste0(
            "Orientation invalide : impossible ou ambigue — l'orientation ",
            "doit etre declaree explicitement."
        ),
        invalid_cell_alignment = paste0(
            "Alignement des cellules invalide : recouvrement insuffisant ou ",
            "nul avec l'objet Seurat."
        ),
        invalid_gene_alignment = paste0(
            "Alignement des genes invalide : aucun gene velocity ne ",
            "correspond a l'objet Seurat."
        ),
        invalid_vector_projection = paste0(
            "Vecteurs rejetes : les vecteurs pre-calcules fournis ont echoue ",
            "a la validation stricte — les matrices alignees restent ",
            "utilisables, fleches desactivees."
        ),
        stale_against_current_seurat_object = paste0(
            "Perime : le resultat velocity correspond a un objet Seurat ",
            "anterieur — reimportez et revalidez les donnees velocity."
        )
    )
}

#' Le statut exprime-t-il un resultat techniquement exploitable ?
#'
#' @param status Chaine d'etat (result$status).
#' @return Logical — TRUE uniquement pour valid / valid_no_vectors /
#'   valid_partial_embedding.
#' @export
velocity_status_is_valid <- function(status) {
    status %in% c("valid", "valid_no_vectors", "valid_partial_embedding")
}

#' Le statut autorise-t-il les vues basees sur les matrices alignees ?
#'
#' Les matrices d'un resultat invalide_vector_projection restent validees :
#' les vues matricielles (phase portrait, coordonnees) restent licites, la
#' couche vecteurs seule est interdite. Les etats fatals ne produisent jamais
#' d'objet stocke (echec avant finalisation).
#'
#' @param status Chaine d'etat (result$status).
#' @return Logical.
#' @export
velocity_status_allows_matrices <- function(status) {
    status %in% c(
        "valid", "valid_no_vectors", "valid_partial_embedding",
        "invalid_vector_projection"
    )
}

#' Verifier que l'objet est un resultat velocity canonique plottable (Stage 9)
#'
#' Garde de contrat pour TOUTE visualisation : consomme uniquement un objet
#' produit par finalize_velocity_result(), refuse un resultat perime vis-a-vis
#' de l'objet Seurat fourni, et applique le prerequis de la vue demandee.
#' Une visualisation ne cree, n'infere et ne repare jamais un resultat
#' scientifique (Stage 9).
#'
#' @param velocity_result Objet a verifier (resultat canonique attendu).
#' @param view Vue demandee : "any" (contrat seul), "matrices" (vues
#'   spliced/unspliced + coordonnees) ou "vectors" (vues fleches — exige des
#'   vecteurs STRICTEMENT valides).
#' @param seurat_obj Objet Seurat courant optionnel : fourni, la peremption
#'   (stale_against_current_seurat_object) est verifiee.
#' @param context Contexte cite dans les messages d'erreur.
#' @return Le resultat, invisible (pipable, conforme au style assert_*).
#' @export
assert_velocity_result <- function(
    velocity_result,
    view = c("any", "matrices", "vectors"),
    seurat_obj = NULL,
    context = "visualisation velocity"
) {
    view <- match.arg(view)

    if (!is.list(velocity_result) ||
        !identical(velocity_result$type %||% NULL, "rna_velocity") ||
        is.null(velocity_result$status)) {
        .velocity_stop(
            "invalid_input",
            sprintf(
                paste0("Echec %s : resultat velocity canonique requis ",
                       "(finalize_velocity_result()) — recu : %s."),
                context,
                if (is.null(velocity_result)) "NULL"
                else paste(class(velocity_result), collapse = "/")
            )
        )
    }

    if (!velocity_result$status %in% velocity_validity_states()) {
        .velocity_stop(
            "invalid_input",
            sprintf(
                "Echec %s : statut velocity inconnu '%s'.",
                context, velocity_result$status
            )
        )
    }

    # Peremption : un resultat perime n'est jamais affiche ni exporte.
    if (!is.null(seurat_obj) &&
        isTRUE(velocity_result_is_stale(velocity_result, seurat_obj))) {
        .velocity_stop(
            "stale_against_current_seurat_object",
            sprintf(
                "Echec %s : %s",
                context,
                velocity_status_labels()[["stale_against_current_seurat_object"]]
            )
        )
    }

    if (view %in% c("matrices", "vectors")) {
        if (is.null(velocity_result$spliced) ||
            is.null(velocity_result$unspliced) ||
            !velocity_status_allows_matrices(velocity_result$status)) {
            .velocity_stop(
                "invalid_input",
                sprintf(
                    paste0("Echec %s : matrices alignees validees requises ",
                           "(statut actuel : %s)."),
                    context, velocity_result$status
                )
            )
        }
    }

    if (identical(view, "vectors") &&
        is.null(velocity_result$velocity_vectors)) {
        .velocity_stop(
            "invalid_vector_projection",
            sprintf(
                paste0("Echec %s : aucun vecteur velocity valide — la vue ",
                       "fleches est refusee. Importez des vecteurs pre-calcules ",
                       "valides ou utilisez la vue coordonnees (points seuls)."),
                context
            )
        )
    }

    invisible(velocity_result)
}

#' Erreur de validation velocity avec etat structure
#'
#' Interne : la chaine de validation leve des erreurs portant la classe
#' "velocity_validation_error" et un champ `state` parmi
#' velocity_validity_states(). Les messages restent inchanges (contractuel).
.velocity_stop <- function(state, message) {
    # errorCondition() construit la condition c("velocity_validation_error",
    # "error", "condition") ; stop() la signale telle quelle.
    stop(errorCondition(
        message,
        state = state,
        class = "velocity_validation_error"
    ))
}

#' Etat de validite associe a une erreur velocity
#'
#' @param e Condition (erreur) capturee.
#' @return La chaine d'etat structuree, NA_character_ pour une erreur sans
#'   etat (ex. erreur R generique).
#' @export
# §14.1 : délégation à l'accesseur générique. Famille F1 MESURÉE — la CLASSE
# est vérifiée : l'état d'une erreur d'un autre domaine rend NA.
velocity_error_state <- function(e) ts_error_state(e, "velocity_validation_error")

# is.numeric() renvoie FALSE pour les matrices creuses Matrix (dgCMatrix) dans
# les versions actuelles du package : la compatibilite "numerique/sparse" du
# contrat passe donc par ce test dedie (double dense OU classe dMatrix).
.velocity_matrix_is_numeric <- function(x) {
    is.numeric(x) || inherits(x, "dMatrix")
}

#' Detect matrix orientation from explicit identifiers
#'
#' @param mat Matrix with row and column identifiers.
#' @param seurat_cells Cell barcodes from the Seurat object.
#' @param seurat_genes Gene identifiers from the Seurat object.
#' @param orientation User-selected orientation.
#' @return One of "genes_x_cells" or "cells_x_genes".
#' @export
detect_velocity_orientation <- function(
    mat,
    seurat_cells = NULL,
    seurat_genes = NULL,
    orientation = c(
        "auto_strict",
        "genes_x_cells",
        "cells_x_genes"
    )
) {
    orientation <- match.arg(orientation)
    mat <- as.matrix(mat)

    if (is.null(rownames(mat)) || is.null(colnames(mat))) {
        .velocity_stop(
            "invalid_orientation",
            paste0(
                "Orientation impossible : les matrices doivent posseder des ",
                "identifiants de lignes et de colonnes."
            )
        )
    }

    if (orientation != "auto_strict") {
        return(orientation)
    }

    if (is.null(seurat_cells) || is.null(seurat_genes)) {
        .velocity_stop(
            "invalid_input",
            paste0(
                "Orientation automatique impossible sans identifiants ",
                "de cellules et de genes Seurat."
            )
        )
    }

    row_cell_overlap <- sum(rownames(mat) %in% seurat_cells)
    col_cell_overlap <- sum(colnames(mat) %in% seurat_cells)
    row_gene_overlap <- sum(rownames(mat) %in% seurat_genes)
    col_gene_overlap <- sum(colnames(mat) %in% seurat_genes)

    genes_x_cells_score <- row_gene_overlap + col_cell_overlap
    cells_x_genes_score <- col_gene_overlap + row_cell_overlap

    if (genes_x_cells_score == cells_x_genes_score) {
        .velocity_stop(
            "invalid_orientation",
            paste0(
                "Orientation ambigue : les deux orientations restent ",
                "plausibles. Choisissez explicitement Genes x cellules ou ",
                "Cellules x genes."
            )
        )
    }

    if (genes_x_cells_score > cells_x_genes_score) {
        return("genes_x_cells")
    }

    "cells_x_genes"
}

#' Validate matrix identifiers
#'
#' @param mat Matrix to validate.
#' @param label Human-readable matrix label.
#' @return Invisible TRUE or stops with an informative error.
#' @export
validate_velocity_identifiers <- function(
    mat,
    label = "velocity matrix"
) {
    if (is.null(rownames(mat)) || is.null(colnames(mat))) {
        .velocity_stop(
            "invalid_input",
            paste0(
                "Identifiants absents dans ",
                label,
                " : rownames et colnames sont obligatoires."
            )
        )
    }

    duplicated_rows <- unique(rownames(mat)[duplicated(rownames(mat))])
    duplicated_cols <- unique(colnames(mat)[duplicated(colnames(mat))])

    if (length(duplicated_rows) > 0L) {
        .velocity_stop(
            "invalid_input",
            paste0(
                "Identifiants de lignes dupliques dans ",
                label,
                " : ",
                paste(head(duplicated_rows, 10L), collapse = ", "),
                "."
            )
        )
    }

    if (length(duplicated_cols) > 0L) {
        .velocity_stop(
            "invalid_input",
            paste0(
                "Identifiants de colonnes dupliques dans ",
                label,
                " : ",
                paste(head(duplicated_cols, 10L), collapse = ", "),
                "."
            )
        )
    }

    invisible(TRUE)
}

#' Normalize cell barcodes without silently creating collisions
#'
#' @param ids Cell barcodes.
#' @param strip_suffix Logical, remove terminal -1/-2 style suffix.
#' @return Normalized barcodes.
#' @export
normalize_velocity_cell_barcodes <- function(
    ids,
    strip_suffix = FALSE
) {
    ids <- as.character(ids)

    if (!isTRUE(strip_suffix)) {
        return(ids)
    }

    normalized <- sub("-[0-9]+$", "", ids)
    collisions <- unique(normalized[duplicated(normalized)])

    if (length(collisions) > 0L) {
        .velocity_stop(
            "invalid_input",
            paste0(
                "Le retrait des suffixes de barcodes cree des collisions : ",
                paste(head(collisions, 10L), collapse = ", "),
                ". Desactivez cette option ou corrigez les identifiants."
            )
        )
    }

    normalized
}

#' Normalize gene identifiers without silently creating collisions
#'
#' @param ids Gene identifiers.
#' @param strip_version Logical, remove terminal Ensembl version suffix.
#' @return Normalized gene identifiers.
#' @export
normalize_velocity_gene_ids <- function(
    ids,
    strip_version = FALSE
) {
    ids <- as.character(ids)

    if (!isTRUE(strip_version)) {
        return(ids)
    }

    normalized <- sub("\\.[0-9]+$", "", ids)
    collisions <- unique(normalized[duplicated(normalized)])

    if (length(collisions) > 0L) {
        .velocity_stop(
            "invalid_input",
            paste0(
                "Le retrait des versions de genes cree des collisions : ",
                paste(head(collisions, 10L), collapse = ", "),
                ". Desactivez cette option ou corrigez les identifiants."
            )
        )
    }

    normalized
}

#' Validate spliced and unspliced matrices
#'
#' @param spliced Spliced matrix.
#' @param unspliced Unspliced matrix.
#' @param ambiguous Optional ambiguous matrix.
#' @param seurat_cells Seurat cell barcodes.
#' @param seurat_genes Seurat gene identifiers.
#' @param orientation Matrix orientation.
#' @param strip_cell_suffix Remove cell barcode suffixes.
#' @param strip_gene_version Remove gene version suffixes.
#' @param allow_low_overlap Allow overlap below the declared minimum.
#' @param min_cell_overlap Optional numeric override of the declared minimum
#'   overlap fraction (TS_VELOCITY_OVERLAP_MIN, historique 0.8). NULL par defaut.
#' @return Validated velocity structure and alignment report. La structure
#'   inclut les tables d'alignement `cell_mapping`/`gene_mapping` (Stage 8)
#'   lorsque les identifiants Seurat sont fournis (NULL sinon), ainsi que les
#'   avertissements produits pendant la validation (`warnings`).
#' @export
validate_velocity_matrices <- function(
    spliced,
    unspliced,
    ambiguous = NULL,
    seurat_cells = NULL,
    seurat_genes = NULL,
    orientation = "auto_strict",
    strip_cell_suffix = FALSE,
    strip_gene_version = FALSE,
    allow_low_overlap = FALSE,
    min_cell_overlap = NULL
) {
    if (!requireNamespace("Matrix", quietly = TRUE)) {
        .velocity_stop(
            "invalid_input",
            "Le package Matrix est requis pour valider les matrices."
        )
    }

    if (!is.matrix(spliced) && !inherits(spliced, "Matrix")) {
        .velocity_stop(
            "invalid_input",
            "La matrice spliced doit etre une matrice numerique."
        )
    }

    if (!.velocity_matrix_is_numeric(spliced)) {
        .velocity_stop(
            "invalid_input",
            "La matrice spliced doit contenir des valeurs numeriques."
        )
    }

    if (!is.matrix(unspliced) && !inherits(unspliced, "Matrix")) {
        .velocity_stop(
            "invalid_input",
            "La matrice unspliced doit etre une matrice numerique."
        )
    }

    if (!.velocity_matrix_is_numeric(unspliced)) {
        .velocity_stop(
            "invalid_input",
            "La matrice unspliced doit contenir des valeurs numeriques."
        )
    }

    # Seuil de recouvrement declare (config/thresholds.R) : valeur explicite
    # prioritaire, sinon parametre declare, sinon historique 0.8.
    min_overlap <- if (!is.null(min_cell_overlap)) {
        as.numeric(min_cell_overlap)
    } else if (exists("TS_VELOCITY_OVERLAP_MIN")) {
        as.numeric(TS_VELOCITY_OVERLAP_MIN)
    } else {
        0.8
    }

    # Avertissements PRODUITS pendant la validation (Stage 8) ; aucun
    # avertissement n'est reconstruit apres coup.
    extra_warnings <- character(0)

    if (!identical(dim(spliced), dim(unspliced))) {
        .velocity_stop(
            "invalid_input",
            paste0(
                "Les matrices spliced et unspliced doivent avoir les memes ",
                "dimensions."
            )
        )
    }

    validate_velocity_identifiers(spliced, "spliced")
    validate_velocity_identifiers(unspliced, "unspliced")

    if (!identical(rownames(spliced), rownames(unspliced)) ||
        !identical(colnames(spliced), colnames(unspliced))) {
        .velocity_stop(
            "invalid_input",
            paste0(
                "Les identifiants des matrices spliced et unspliced ",
                "doivent etre identiques et dans le meme ordre."
            )
        )
    }

    if (!is.null(ambiguous)) {
        if (!identical(dim(spliced), dim(ambiguous))) {
            .velocity_stop(
                "invalid_input",
                paste0(
                    "La matrice ambiguous doit avoir les memes dimensions ",
                    "que spliced et unspliced."
                )
            )
        }

        if (!.velocity_matrix_is_numeric(ambiguous)) {
            .velocity_stop(
                "invalid_input",
                "La matrice ambiguous doit contenir des valeurs numeriques."
            )
        }

        validate_velocity_identifiers(ambiguous, "ambiguous")

        if (!identical(rownames(spliced), rownames(ambiguous)) ||
            !identical(colnames(spliced), colnames(ambiguous))) {
            .velocity_stop(
                "invalid_input",
                paste0(
                    "Les identifiants de la matrice ambiguous doivent etre ",
                    "identiques a ceux de spliced."
                )
            )
        }
    }

    detected_orientation <- detect_velocity_orientation(
        mat = spliced,
        seurat_cells = seurat_cells,
        seurat_genes = seurat_genes,
        orientation = orientation
    )

    if (detected_orientation == "cells_x_genes") {
        spliced <- Matrix::t(spliced)
        unspliced <- Matrix::t(unspliced)
        if (!is.null(ambiguous)) {
            ambiguous <- Matrix::t(ambiguous)
        }
    }

    cell_ids_original <- colnames(spliced)
    gene_ids_original <- rownames(spliced)

    cell_ids <- normalize_velocity_cell_barcodes(
        cell_ids_original,
        strip_suffix = strip_cell_suffix
    )

    gene_ids <- normalize_velocity_gene_ids(
        gene_ids_original,
        strip_version = strip_gene_version
    )

    if (anyDuplicated(cell_ids)) {
        .velocity_stop(
            "invalid_input",
            paste0(
                "Les identifiants de cellules deviennent dupliques apres ",
                "normalisation."
            )
        )
    }

    if (anyDuplicated(gene_ids)) {
        .velocity_stop(
            "invalid_input",
            paste0(
                "Les identifiants de genes deviennent dupliques apres ",
                "normalisation."
            )
        )
    }

    if (!is.null(seurat_cells)) {
        seurat_cells <- as.character(seurat_cells)
        # Raw overlap is computed BEFORE any normalization so the provenance
        # report can show both raw and normalized agreement (BUG 5/BUG 6).
        seurat_cells_raw <- seurat_cells
        raw_cell_overlap <- length(intersect(cell_ids_original, seurat_cells_raw)) /
            max(1L, length(seurat_cells_raw))

        seurat_cells_normalized <- normalize_velocity_cell_barcodes(
            seurat_cells,
            strip_suffix = strip_cell_suffix
        )

        if (anyDuplicated(seurat_cells_normalized)) {
            .velocity_stop(
                "invalid_input",
                paste0(
                    "Les cellules Seurat deviennent dupliquees apres ",
                    "normalisation."
                )
            )
        }

        matched_cells <- intersect(
            seurat_cells_normalized,
            cell_ids
        )

        if (length(matched_cells) == 0L) {
            .velocity_stop(
                "invalid_cell_alignment",
                paste0(
                    "Aucune cellule alignee entre les matrices velocity et ",
                    "l'objet Seurat courant. Verifiez que l'objet charge ",
                    "correspond au meme run Cell Ranger et activez le retrait ",
                    "des suffixes (-1) si necessaire."
                )
            )
        }

        n_cells_input <- length(cell_ids)
        n_cells_matched <- length(matched_cells)
        overlap <- n_cells_matched / max(1L, length(seurat_cells_normalized))

        if (overlap < min_overlap && !isTRUE(allow_low_overlap)) {
            .velocity_stop(
                "invalid_cell_alignment",
                paste0(
                    "Recouvrement des cellules inferieur a ",
                    sprintf("%.0f", 100 * min_overlap), " pourcent : ",
                    sprintf("%.1f", 100 * overlap),
                    " pourcent. Activez explicitement l'option ",
                    "'Forcer l'alignement faible' pour continuer."
                )
            )
        }

        if (overlap < min_overlap && isTRUE(allow_low_overlap)) {
            extra_warnings <- c(extra_warnings, sprintf(
                paste0(
                    "Recouvrement des cellules faible : %.1f pourcent ",
                    "(< %.0f pourcent) — alignement force explicitement via ",
                    "l'option 'Forcer l'alignement faible'. Validite ",
                    "technique conservee, prudence d'interpretation requise."
                ),
                100 * overlap, 100 * min_overlap
            ))
        }

        cell_order <- match(
            seurat_cells_normalized,
            cell_ids
        )

        keep_cells <- !is.na(cell_order)

        spliced <- spliced[, cell_order[keep_cells], drop = FALSE]
        unspliced <- unspliced[, cell_order[keep_cells], drop = FALSE]

        if (!is.null(ambiguous)) {
            ambiguous <- ambiguous[, cell_order[keep_cells], drop = FALSE]
        }

        aligned_cells <- seurat_cells[keep_cells]

        # Table d'alignement cellules (Stage 8) : paires alignees (ordre
        # Seurat), cellules Seurat sans donnees velocity, puis barcodes
        # velocity hors objet. Enregistree a la validation, jamais reconstruite.
        velocity_only_cell_idx <- setdiff(seq_along(cell_ids), cell_order[keep_cells])
        cell_mapping <- rbind(
            data.frame(
                seurat_cell = seurat_cells[keep_cells],
                velocity_cell_original = cell_ids_original[cell_order[keep_cells]],
                velocity_cell_normalized = cell_ids[cell_order[keep_cells]],
                aligned = rep(TRUE, sum(keep_cells)),
                stringsAsFactors = FALSE
            ),
            data.frame(
                seurat_cell = seurat_cells[!keep_cells],
                velocity_cell_original = rep(NA_character_, sum(!keep_cells)),
                velocity_cell_normalized = rep(NA_character_, sum(!keep_cells)),
                aligned = rep(FALSE, sum(!keep_cells)),
                stringsAsFactors = FALSE
            ),
            data.frame(
                seurat_cell = rep(NA_character_, length(velocity_only_cell_idx)),
                velocity_cell_original = cell_ids_original[velocity_only_cell_idx],
                velocity_cell_normalized = cell_ids[velocity_only_cell_idx],
                aligned = rep(FALSE, length(velocity_only_cell_idx)),
                stringsAsFactors = FALSE
            )
        )
    } else {
        aligned_cells <- cell_ids_original
        n_cells_input <- length(cell_ids)
        n_cells_matched <- length(cell_ids)
        overlap <- NA_real_
        raw_cell_overlap <- NA_real_
        cell_mapping <- NULL
    }

    if (!is.null(seurat_genes)) {
        seurat_genes <- as.character(seurat_genes)
        seurat_genes_normalized <- normalize_velocity_gene_ids(
            seurat_genes,
            strip_version = strip_gene_version
        )

        if (anyDuplicated(seurat_genes_normalized)) {
            .velocity_stop(
                "invalid_input",
                paste0(
                    "Les genes Seurat deviennent dupliques apres ",
                    "normalisation."
                )
            )
        }

        gene_order <- match(
            seurat_genes_normalized,
            gene_ids
        )

        keep_genes <- !is.na(gene_order)

        if (!any(keep_genes)) {
            .velocity_stop(
                "invalid_gene_alignment",
                paste0(
                    "Aucun gene velocity ne correspond aux genes de ",
                    "l'objet Seurat."
                )
            )
        }

        spliced <- spliced[gene_order[keep_genes], , drop = FALSE]
        unspliced <- unspliced[gene_order[keep_genes], , drop = FALSE]

        if (!is.null(ambiguous)) {
            ambiguous <- ambiguous[gene_order[keep_genes], , drop = FALSE]
        }

        aligned_genes <- seurat_genes[keep_genes]

        # Table d'alignement genes (Stage 8) : meme contrat que cell_mapping.
        velocity_only_gene_idx <- setdiff(seq_along(gene_ids), gene_order[keep_genes])
        gene_mapping <- rbind(
            data.frame(
                seurat_gene = seurat_genes[keep_genes],
                velocity_gene_original = gene_ids_original[gene_order[keep_genes]],
                velocity_gene_normalized = gene_ids[gene_order[keep_genes]],
                aligned = rep(TRUE, sum(keep_genes)),
                stringsAsFactors = FALSE
            ),
            data.frame(
                seurat_gene = seurat_genes[!keep_genes],
                velocity_gene_original = rep(NA_character_, sum(!keep_genes)),
                velocity_gene_normalized = rep(NA_character_, sum(!keep_genes)),
                aligned = rep(FALSE, sum(!keep_genes)),
                stringsAsFactors = FALSE
            ),
            data.frame(
                seurat_gene = rep(NA_character_, length(velocity_only_gene_idx)),
                velocity_gene_original = gene_ids_original[velocity_only_gene_idx],
                velocity_gene_normalized = gene_ids[velocity_only_gene_idx],
                aligned = rep(FALSE, length(velocity_only_gene_idx)),
                stringsAsFactors = FALSE
            )
        )
    } else {
        aligned_genes <- gene_ids_original
        gene_mapping <- NULL
    }

    # update rownames/colnames to reflect aligned Seurat identifiers for downstream consistency
    rownames(spliced) <- aligned_genes
    rownames(unspliced) <- aligned_genes
    colnames(spliced) <- aligned_cells
    colnames(unspliced) <- aligned_cells
    if (!is.null(ambiguous)) {
        rownames(ambiguous) <- aligned_genes
        colnames(ambiguous) <- aligned_cells
    }

    list(
        spliced = spliced,
        unspliced = unspliced,
        ambiguous = ambiguous,
        cell_names = aligned_cells,
        gene_names = aligned_genes,
        orientation = detected_orientation,
        cell_match_mode = if (isTRUE(strip_cell_suffix)) "suffix" else "exact",
        gene_match_mode = if (isTRUE(strip_gene_version)) "version" else "exact",
        strip_cell_suffix = isTRUE(strip_cell_suffix),
        strip_gene_version = isTRUE(strip_gene_version),
        velocity_source = "import",
        velocity_method = "precomputed",
        n_cells_input = n_cells_input,
        n_genes_input = length(gene_ids),
        n_cells_matched = ncol(spliced),
        n_genes_matched = nrow(spliced),
        n_cells_missing = if (!is.null(seurat_cells)) {
            length(seurat_cells) - ncol(spliced)
        } else {
            0L
        },
        n_genes_missing = if (!is.null(seurat_genes)) {
            length(seurat_genes) - nrow(spliced)
        } else {
            0L
        },
        n_cell_collisions = 0L,
        n_gene_collisions = 0L,
        warnings = extra_warnings,
        overlap = overlap,
        raw_cell_overlap = raw_cell_overlap,
        normalized_cell_overlap = overlap,
        allow_low_overlap = isTRUE(allow_low_overlap),
        cell_mapping = cell_mapping,
        gene_mapping = gene_mapping
    )
}

#' Read a combined RNA velocity RDS object
#'
#' @param path RDS file path.
#' @return Named velocity input list.
#' @export
read_velocity_rds <- function(path) {
    # .rda/.RData — workspace inspecté (contrat RDATA_IMPORT_CONTRACT.md) :
    # seul un fichier à objet unique est importable ici ; pour un workspace
    # multi-objets, utiliser l'aperçu d'import Single-Cell (« Exporter la
    # sélection ») pour isoler la liste velocity, puis réimporter.
    if (rdata_is_supported_file(path)) {
        env <- rdata_load_env(path)
        on.exit(rdata_free(env), add = TRUE)
        rda_names <- ls(envir = env)
        if (length(rda_names) > 1L) {
            .velocity_stop(
                "invalid_input",
                sprintf(paste0(
                    "Le fichier .RData contient %d objets. Utilisez l'aperçu ",
                    "d'import Single-Cell (« Exporter la sélection ») pour ",
                    "isoler la liste velocity dans un fichier dédié, puis réimportez-le."),
                    length(rda_names)
                )
            )
        }
        object <- rdata_extract_object(env, rda_names[1])
    } else {
        object <- readRDS(path)
    }

    allowed_names <- c(
        "spliced",
        "unspliced",
        "ambiguous",
        "cell_names",
        "gene_names",
        "velocity_source",
        "velocity_method",
        "orientation",
        "clusters",
        "umap_embedding",
        "umap_velocity",
        "embedding_velocity",
        "umap_vectors",
        "vectors",
        "embedding_reduction",
        "stored_reduction",
        "reduction",
        "velocity_metadata",
        "alignment"
    )

    if (!is.list(object) || is.null(names(object))) {
        .velocity_stop(
            "invalid_input",
            paste0(
                "Le fichier RDS velocity doit contenir une liste nommee avec ",
                "spliced et unspliced."
            )
        )
    }

    observed_names <- names(object)
    unexpected_names <- setdiff(observed_names, allowed_names)

    if (length(unexpected_names) > 0L) {
        .velocity_stop(
            "invalid_input",
            paste0(
                "Noms inattendus dans le RDS velocity : ",
                paste(unexpected_names, collapse = ", "),
                ". Noms observes : ",
                paste(observed_names, collapse = ", "),
                "."
            )
        )
    }

    if (!all(c("spliced", "unspliced") %in% observed_names)) {
        .velocity_stop(
            "invalid_input",
            paste0(
                "Le RDS velocity doit contenir exactement les elements ",
                "'spliced' et 'unspliced'."
            )
        )
    }

    validate_velocity_rds_metadata(object)

    object
}

#' Validate metadata fields of a combined RNA velocity RDS object
#'
#' Structural checks only — no inference, no transposition, no padding.
#'
#' @param velocity_input Named list returned by read_velocity_rds().
#' @return Invisible TRUE or stops with an informative French error.
#' @export
validate_velocity_rds_metadata <- function(velocity_input) {
    if (is.null(velocity_input) || !is.list(velocity_input)) {
        .velocity_stop(
            "invalid_input",
            "Entrée velocity invalide : liste nommee attendue."
        )
    }

    .assert_optional_single_string <- function(value, field) {
        if (is.null(value)) return(invisible(TRUE))
        if (!is.character(value) || length(value) != 1L || is.na(value)) {
            .velocity_stop(
                "invalid_input",
                sprintf("%s doit etre une chaine de caracteres unique.", field)
            )
        }
        invisible(TRUE)
    }

    .assert_optional_string_vector <- function(value, field) {
        if (is.null(value)) return(invisible(TRUE))
        if (!is.character(value) || anyNA(value)) {
            .velocity_stop(
                "invalid_input",
                sprintf("%s doit etre un vecteur de chaines sans NA.", field)
            )
        }
        invisible(TRUE)
    }

    .assert_two_col_numeric_matrix <- function(value, field) {
        if (is.null(value)) return(invisible(TRUE))
        value <- as.matrix(value)
        if (!is.numeric(value) || ncol(value) != 2L) {
            .velocity_stop(
                "invalid_input",
                sprintf(
                    paste0("%s doit etre une matrice numerique avec ",
                           "exactement deux colonnes."),
                    field
                )
            )
        }
        if (is.null(rownames(value))) {
            .velocity_stop(
                "invalid_input",
                sprintf(
                    paste0("%s doit posseder des noms de lignes (barcodes ",
                           "de cellules)."),
                    field
                )
            )
        }
        if (anyDuplicated(rownames(value))) {
            .velocity_stop(
                "invalid_input",
                sprintf(
                    "%s contient des noms de lignes dupliques.",
                    field
                )
            )
        }
        invisible(TRUE)
    }

    .assert_optional_single_string(
        velocity_input$velocity_source,
        "'velocity_source'"
    )
    .assert_optional_single_string(
        velocity_input$velocity_method,
        "'velocity_method'"
    )

    if (!is.null(velocity_input$orientation) &&
        !velocity_input$orientation %in%
            c("genes_x_cells", "cells_x_genes", "auto_strict")) {
        .velocity_stop(
            "invalid_input",
            paste(
                "'orientation' doit valoir 'genes_x_cells', 'cells_x_genes'",
                "ou 'auto_strict'."
            )
        )
    }

    .assert_optional_string_vector(velocity_input$gene_names, "'gene_names'")
    .assert_optional_string_vector(velocity_input$cell_names, "'cell_names'")

    # clusters: optional, one value per velocity cell (column axis by
    # convention genes_x_cells; row axis when orientation == cells_x_genes).
    if (!is.null(velocity_input$clusters)) {
        spliced_dim <- dim(velocity_input$spliced)
        n_cells_rds <- if (identical(velocity_input$orientation, "cells_x_genes")) {
            spliced_dim[1L]
        } else {
            spliced_dim[2L]
        }
        if (length(velocity_input$clusters) != n_cells_rds) {
            .velocity_stop(
                "invalid_input",
                sprintf(
                    paste0("'clusters' doit contenir une valeur par cellule ",
                           "velocity (%d attendues, %d fournies)."),
                    n_cells_rds, length(velocity_input$clusters)
                )
            )
        }
    }

    # Coordinates: partial alignment is allowed, padding is forbidden.
    .assert_two_col_numeric_matrix(
        velocity_input$umap_embedding,
        "'umap_embedding'"
    )

    # Velocity vectors in embedding space: same structural contract.
    # umap_embedding is deliberately NOT routed through this check as a
    # vector — coordinates and dx/dy remain separate concepts.
    .assert_two_col_numeric_matrix(velocity_input$umap_velocity, "'umap_velocity'")
    .assert_two_col_numeric_matrix(
        velocity_input$embedding_velocity,
        "'embedding_velocity'"
    )

    invisible(TRUE)
}

.read_lines_maybe_gz <- function(path) {
    con <- if (grepl("\\.gz$", path, ignore.case = TRUE)) {
        gzfile(path, open = "rt")
    } else {
        file(path, open = "rt")
    }
    on.exit(close(con), add = TRUE)
    readLines(con, warn = FALSE)
}

#' Read RNA velocity Matrix Market inputs
#'
#' Requires the three files (matrix, barcodes, features/genes). Gzipped
#' barcode/feature files are supported transparently. The sparse Matrix
#' object returned by Matrix::readMM() is preserved as-is (never densified).
#' When the feature file contains MULTIPLE columns (standard 10x layout:
#' gene_id <tab> gene_symbol <tab> ...), an explicit `feature_column` MUST be
#' supplied — the first column is never assumed to be the identifier used by
#' the Seurat object. Duplicate identifiers stop validation (no make.unique).
#'
#' @param matrix_path Matrix Market file (.mtx or .mtx.gz).
#' @param barcode_path Cell barcode file.
#' @param feature_path Gene/feature file.
#' @param feature_column Integer column index in the feature file that holds
#'   the identifiers matching the Seurat rownames. Required when the feature
#'   file has more than one column; ignored for single-column files.
#' @return Named sparse matrix with genes in rows and cells in columns.
#' @export
read_velocity_mtx <- function(
    matrix_path,
    barcode_path,
    feature_path,
    feature_column = NULL
) {
    if (!file.exists(matrix_path) ||
        !file.exists(barcode_path) ||
        !file.exists(feature_path)) {
        .velocity_stop(
            "invalid_input",
            paste0(
                "Les trois fichiers sont obligatoires : matrice, barcodes ",
                "et features/genes."
            )
        )
    }

    mat <- Matrix::readMM(
      if (grepl("\\.gz$", matrix_path, ignore.case = TRUE)) {
          gzfile(matrix_path, open = "rt")
      } else {
          matrix_path
      }
    )
    barcodes_raw <- trimws(.read_lines_maybe_gz(barcode_path))
    barcodes <- barcodes_raw[nzchar(barcodes_raw)]
    features_raw <- trimws(.read_lines_maybe_gz(feature_path))
    features_raw <- features_raw[nzchar(features_raw)]

    split_fields <- strsplit(features_raw, "\t", fixed = TRUE)
    n_detected_cols <- unique(vapply(split_fields, length, integer(1)))

    if (length(n_detected_cols) > 1L) {
        # Ragged feature file — refuse rather than guess a per-row column.
        .velocity_stop(
            "invalid_input",
            paste0(
                "Fichier features/genes irregulier : nombre de colonnes ",
                "variable entre les lignes. Corrigez le fichier ou fournissez ",
                "un TSV tabule homogene."
            )
        )
    }

    n_feature_cols <- if (length(n_detected_cols) == 0L) 1L else n_detected_cols

    feature_column_int <- NULL
    if (!is.null(feature_column) && !is.na(feature_column)) {
        feature_column_int <- suppressWarnings(as.integer(feature_column))
        if (is.na(feature_column_int) || feature_column_int < 1L ||
            feature_column_int > n_feature_cols) {
            .velocity_stop(
                "invalid_input",
                sprintf(
                    paste0("Colonne d'identifiants invalide : %s (le fichier ",
                           "features contient %d colonne(s))."),
                    paste(feature_column, collapse = ","), n_feature_cols
                )
            )
        }
    } else if (n_feature_cols > 1L) {
        .velocity_stop(
            "invalid_input",
            sprintf(
                paste0("Le fichier features/genes contient %d colonnes. ",
                       "Indiquez explicitement quelle colonne correspond aux ",
                       "identifiants de l'objet Seurat (champ 'Colonne ",
                       "d'identifiants features'). La premiere colonne n'est ",
                       "jamais supposee correcte."),
                n_feature_cols
            )
        )
    } else {
        feature_column_int <- 1L
    }

    features <- vapply(
        split_fields,
        function(fields) fields[feature_column_int],
        character(1)
    )

    if (nrow(mat) != length(features) ||
        ncol(mat) != length(barcodes)) {
        .velocity_stop(
            "invalid_input",
            paste0(
                "Les dimensions de la matrice MTX ne correspondent pas aux ",
                "fichiers features/barcodes."
            )
        )
    }

    rownames(mat) <- features
    colnames(mat) <- barcodes

    validate_velocity_identifiers(mat, "MTX velocity")

    mat
}

# ── Loom / H5AD (roadmap SC FUNCTION_TEST M0) ────────────────────────────────
# Lecteurs hdf5r des formats velocity PRE-COMPUTES natifs (velocyto .loom,
# scVelo .h5ad). Aucune inference : les couches spliced/unspliced sont lues
# telles quelles et rendues dans la MÊME structure que read_velocity_rds()
# (liste nommée au format whitelisté, validée par la même métavalidation),
# puis passent par validate_velocity_matrices() comme les deux autres modes.
# Lecture CHUNCKÉE : les couches loom sont denses mais compressées (ex:
# WB_Lysis_3p_Introns_8kCells.loom = 8000 x 36601, 68 Mo) — on ne densifie
# jamais le fichier entier en mémoire, on sparsifie bloc de gènes par bloc.

.VELOCITY_LOOM_CHUNK_GENES <- 2048L

# hdf5r::exists() PROPAGE une erreur HDF5 quand un groupe intermediaire est
# absent (fichier etranger/malforme) — un noeud manquant est un CAS NORMAL
# de validation, jamais une exception.
.h5_node_exists <- function(f, node) {
    tryCatch(f$exists(node), error = function(e) FALSE)
}

.loom_require_dataset <- function(f, node) {
    if (!.h5_node_exists(f, node)) {
        .velocity_stop(
            "invalid_input",
            sprintf("Fichier loom invalide : noeud '%s' absent.", node)
        )
    }
    f[[node]]
}

.loom_read_attrs <- function(f) {
    gene_ds <- .loom_require_dataset(f, "row_attrs/Gene")
    cell_ds <- .loom_require_dataset(f, "col_attrs/CellID")
    genes <- as.character(gene_ds[])
    cells <- as.character(cell_ds[])
    if (length(genes) == 0L || length(cells) == 0L) {
        .velocity_stop(
            "invalid_input",
            "Fichier loom invalide : row_attrs/Gene ou col_attrs/CellID vide."
        )
    }
    if (anyDuplicated(cells)) {
        .velocity_stop(
            "invalid_input",
            "Fichier loom invalide : CellID dupliques (cle d'alignement)."
        )
    }
    list(genes = genes, cells = cells)
}

# Lecture sparse d'une couche loom -> dgCMatrix genes x cells. La couche est
# stockee (cells x genes) OU (genes x cells) selon l'outil d'origine : on
# determine l'orientation avec les longueurs d'attributs (jamais supposee).
.loom_read_layer_sparse <- function(ds, genes, cells, chunk = .VELOCITY_LOOM_CHUNK_GENES) {
    n_g <- length(genes)
    n_c <- length(cells)
    dims <- ds$dims
    stored_genes_x_cells <- if (identical(as.integer(dims[[1L]]), as.integer(n_g)) &&
                                identical(as.integer(dims[[2L]]), as.integer(n_c))) {
        TRUE
    } else if (identical(as.integer(dims[[1L]]), as.integer(n_c)) &&
               identical(as.integer(dims[[2L]]), as.integer(n_g))) {
        FALSE
    } else {
        .velocity_stop(
            "invalid_input",
            sprintf(paste0(
                "Couche loom de dimensions (%s) incompatibles avec les ",
                "attributs (%d genes x %d cellules)."
            ), paste(dims, collapse = "x"), n_g, n_c)
        )
    }
    blocks <- vector("list", ceiling(n_g / chunk))
    bi <- 0L
    start <- 1L
    while (start <= n_g) {
        end <- min(start + chunk - 1L, n_g)
        blk <- if (stored_genes_x_cells) ds[start:end, , drop = FALSE] else
            t(ds[, start:end, drop = FALSE])
        bi <- bi + 1L
        blocks[[bi]] <- Matrix::Matrix(
            as.numeric(blk),
            nrow = end - start + 1L, ncol = n_c, sparse = TRUE
        )
        start <- end + 1L
    }
    mat <- do.call(rbind, blocks)
    dimnames(mat) <- list(genes, cells)
    mat
}

#' Read a velocyto `.loom` file as a velocity input (pre-computed layers)
#'
#' hdf5r-based, chunked, sparse-aware. Layers `spliced`/`unspliced` are
#' required, `ambiguous` optional. `col_attrs` provides `CellID` (barcodes),
#' optional `Clusters` and optional pre-computed embedding `_X`/`_Y`;
#' `row_attrs/Gene` provides gene symbols — duplicated symbols are made
#' unique (warning), the alignment key stays the Seurat side. No inference,
#' no padding: the output feeds the SAME validation chain as the RDS mode.
#'
#' @param path Path to a `.loom` file.
#' @return Named velocity input list (same whitelist as read_velocity_rds()).
#' @export
read_velocity_loom <- function(path) {
    if (!requireNamespace("hdf5r", quietly = TRUE)) {
        .velocity_stop(
            "invalid_input",
            "Lecture .loom indisponible : le paquet 'hdf5r' n'est pas installé."
        )
    }
    if (!file.exists(path)) {
        .velocity_stop("invalid_input", sprintf("Fichier loom introuvable : %s", path))
    }
    f <- tryCatch(hdf5r::h5file(path, mode = "r"),
                  error = function(e) .velocity_stop(
                      "invalid_input",
                      sprintf("Ouverture HDF5 impossible (%s) : %s",
                              basename(path), conditionMessage(e))))
    on.exit(try(f$close_all(), silent = TRUE), add = TRUE)

    attrs <- .loom_read_attrs(f)
    spliced_ds <- .loom_require_dataset(f, "layers/spliced")
    unspliced_ds <- .loom_require_dataset(f, "layers/unspliced")
    has_ambiguous <- .h5_node_exists(f, "layers/ambiguous")

    spliced <- .loom_read_layer_sparse(spliced_ds, attrs$genes, attrs$cells)
    unspliced <- .loom_read_layer_sparse(unspliced_ds, attrs$genes, attrs$cells)
    ambiguous <- if (has_ambiguous) {
        .loom_read_layer_sparse(f[["layers/ambiguous"]], attrs$genes, attrs$cells)
    } else {
        NULL
    }

    # Symboles dupliqués (mesuré : le loom 10x neutrophiles en porte) — la
    # validation aval refuse les identifiants dupliqués, on les rend uniques
    # en le DISANT (l'alignement final reste à l'identifiant unique).
    gene_names <- attrs$genes
    n_dup_genes <- sum(duplicated(gene_names))
    if (n_dup_genes > 0L) {
        warning(sprintf(
            "%s : %d symboles de gènes dupliqués rendus uniques (make.unique).",
            basename(path), n_dup_genes
        ))
        gene_names <- make.unique(gene_names)
        dimnames(spliced) <- list(gene_names, attrs$cells)
        dimnames(unspliced) <- list(gene_names, attrs$cells)
        if (!is.null(ambiguous)) dimnames(ambiguous) <- list(gene_names, attrs$cells)
    }

    clusters <- NULL
    if (.h5_node_exists(f, "col_attrs/Clusters")) {
        clusters <- as.character(f[["col_attrs/Clusters"]][] )
    }
    umap_embedding <- NULL
    if (.h5_node_exists(f, "col_attrs/_X") && .h5_node_exists(f, "col_attrs/_Y")) {
        umap_embedding <- cbind(
            as.numeric(f[["col_attrs/_X"]][] ),
            as.numeric(f[["col_attrs/_Y"]][] )
        )
        rownames(umap_embedding) <- attrs$cells
    }

    out <- list(
        spliced = spliced,
        unspliced = unspliced,
        ambiguous = ambiguous,
        cell_names = attrs$cells,
        gene_names = gene_names,
        orientation = "genes_x_cells",
        velocity_source = "loom",
        clusters = clusters,
        umap_embedding = umap_embedding
    )
    validate_velocity_rds_metadata(out)
    out
}

#' Read a scVelo `.h5ad` file as a velocity input (pre-computed layers)
#'
#' Targets the OLD-anndata layout measured in the wild (ex:
#' endocrinogenesis_day15.h5ad): sparse layers under
#' `/layers/{spliced,unspliced}` stored as `data`/`indices`/`indptr` groups
#' (CSR or CSC — detected from `indptr` length), compound `/obs` and `/var`
#' datasets whose first string field `index` carries barcodes / gene symbols,
#' optional `/obsm/X_umap`. No inference, no padding: same validation chain.
#'
#' @param path Path to a `.h5ad` file.
#' @return Named velocity input list (same whitelist as read_velocity_rds()).
#' @export
read_velocity_h5ad <- function(path) {
    if (!requireNamespace("hdf5r", quietly = TRUE)) {
        .velocity_stop(
            "invalid_input",
            "Lecture .h5ad indisponible : le paquet 'hdf5r' n'est pas installé."
        )
    }
    if (!file.exists(path)) {
        .velocity_stop("invalid_input", sprintf("Fichier h5ad introuvable : %s", path))
    }
    f <- tryCatch(hdf5r::h5file(path, mode = "r"),
                  error = function(e) .velocity_stop(
                      "invalid_input",
                      sprintf("Ouverture HDF5 impossible (%s) : %s",
                              basename(path), conditionMessage(e))))
    on.exit(try(f$close_all(), silent = TRUE), add = TRUE)

    .h5ad_layer <- function(node) {
        if (!.h5_node_exists(f, file.path(node, "data"))) {
            .velocity_stop(
                "invalid_input",
                sprintf("Fichier h5ad invalide : couche sparse '%s' absente.", node)
            )
        }
        list(
            data = f[[file.path(node, "data")]][] ,
            indices = as.integer(f[[file.path(node, "indices")]][] ),
            indptr = as.integer(f[[file.path(node, "indptr")]][] )
        )
    }

    spliced <- .h5ad_layer("layers/spliced")
    unspliced <- .h5ad_layer("layers/unspliced")

    # /obs et /var : datasets COMPOUND (layout old-anndata) — le champ
    # string 'index' porte les barcodes / symboles.
    .h5ad_index <- function(node, what) {
        if (!.h5_node_exists(f, node)) {
            .velocity_stop(
                "invalid_input",
                sprintf("Fichier h5ad invalide : '%s' (%s) absent.", node, what)
            )
        }
        tbl <- tryCatch(f[[node]][] , error = function(e) NULL)
        idx <- if (!is.null(tbl) && is.data.frame(tbl) && "index" %in% names(tbl)) {
            as.character(tbl$index)
        } else {
            NULL
        }
        if (is.null(idx)) {
            .velocity_stop(
                "invalid_input",
                sprintf("Fichier h5ad invalide : champ 'index' illisible dans '%s'.", node)
            )
        }
        idx
    }
    cell_names <- .h5ad_index("obs", "cellules")
    gene_names <- .h5ad_index("var", "gènes")

    n_cells <- length(cell_names)
    n_genes <- length(gene_names)
    if (anyDuplicated(cell_names)) {
        .velocity_stop(
            "invalid_input",
            "Fichier h5ad invalide : barcodes dupliques dans /obs (cle d'alignement)."
        )
    }

    # indptr détermine l'axe de la dimension compressée : longueur n_cells+1
    # => CSR (cellules en lignes) ; n_genes+1 => CSC (gènes en lignes).
    .h5ad_to_genes_x_cells <- function(layer) {
        n_ptr <- length(layer$indptr)
        n_nz <- length(layer$data)
        if (length(layer$indices) != n_nz) {
            .velocity_stop(
                "invalid_input",
                "Fichier h5ad invalide : longueur indices != longueur data."
            )
        }
        if (identical(n_ptr, n_cells + 1L)) {
            rows <- rep(seq_len(n_cells), diff(layer$indptr))
            if (n_nz != length(rows)) {
                .velocity_stop(
                    "invalid_input",
                    "Fichier h5ad invalide : longueur data != longueur indices."
                )
            }
            cols <- layer$indices + 1L
            Matrix::sparseMatrix(
                i = cols, j = rows, x = as.numeric(layer$data),
                dims = c(n_genes, n_cells), dimnames = list(gene_names, cell_names)
            )
        } else if (identical(n_ptr, n_genes + 1L)) {
            cols <- rep(seq_len(n_genes), diff(layer$indptr))
            if (n_nz != length(cols)) {
                .velocity_stop(
                    "invalid_input",
                    "Fichier h5ad invalide : longueur data != longueur indices."
                )
            }
            rows <- layer$indices + 1L
            Matrix::sparseMatrix(
                i = rows, j = cols, x = as.numeric(layer$data),
                dims = c(n_genes, n_cells), dimnames = list(gene_names, cell_names)
            )
        } else {
            .velocity_stop(
                "invalid_input",
                sprintf(paste0(
                    "Fichier h5ad invalide : indptr (%d) incompatible avec ",
                    "%d cellules / %d gènes."
                ), n_ptr, n_cells, n_genes)
            )
        }
    }

    spliced_mat <- .h5ad_to_genes_x_cells(spliced)
    unspliced_mat <- .h5ad_to_genes_x_cells(unspliced)

    n_dup_genes <- sum(duplicated(gene_names))
    if (n_dup_genes > 0L) {
        warning(sprintf(
            "%s : %d symboles de gènes dupliqués rendus uniques (make.unique).",
            basename(path), n_dup_genes
        ))
        gene_names_unique <- make.unique(gene_names)
        dimnames(spliced_mat) <- list(gene_names_unique, cell_names)
        dimnames(unspliced_mat) <- list(gene_names_unique, cell_names)
        gene_names <- gene_names_unique
    }

    clusters <- NULL
    if (.h5_node_exists(f, "obs")) {
        tbl <- tryCatch(f[["obs"]][] , error = function(e) NULL)
        if (!is.null(tbl) && "clusters" %in% names(tbl)) {
            clusters <- as.character(tbl$clusters)
        }
    }
    umap_embedding <- NULL
    if (.h5_node_exists(f, "obsm/X_umap")) {
        # (dims x cells) mesuré sur scVelo ; hdf5r exige la liste d'indices
        # complète pour un dataset 2-D ('[]' seul ne suffit pas ici)
        um <- f[["obsm/X_umap"]][, , drop = FALSE]
        um <- as.matrix(um)
        if (nrow(um) == 2L && ncol(um) == n_cells) {
            um <- t(um)
        } else if (!(nrow(um) == n_cells && ncol(um) == 2L)) {
            um <- NULL
        }
        if (!is.null(um)) {
            umap_embedding <- um
            rownames(umap_embedding) <- cell_names
        }
    }

    out <- list(
        spliced = spliced_mat,
        unspliced = unspliced_mat,
        cell_names = cell_names,
        gene_names = gene_names,
        orientation = "genes_x_cells",
        velocity_source = "h5ad",
        clusters = clusters,
        umap_embedding = umap_embedding
    )
    validate_velocity_rds_metadata(out)
    out
}

#' Plot an RNA velocity phase portrait (Stage 9)
#'
#' Descriptive scatter of spliced vs unspliced counts for one gene. ALL cells
#' are plotted, including zero-count cells (no filtering, no padding). No
#' trend line of any kind is drawn: no geom_smooth(), no lm, no loess, no
#' induction/repression curve. Consomme EXCLUSIVEMENT un resultat canonique
#' valide (assert_velocity_result) — aucune creation, inference ni reparation.
#'
#' Portee des donnees declaree dans le sous-titre :
#'   - data_scope = "preview" : sous-echantillonnage au cap declare
#'     (TS_VELOCITY_MAX_PORTRAIT_CELLS, tirage a graine fixe seed = 1) ;
#'   - data_scope = "full"    : donnees completes, aucun sous-echantillonnage.
#'
#' @param validated_result Resultat velocity canonique (finalize_velocity_result()).
#' @param gene Gene identifier.
#' @param data_scope "preview" (apercu avec cap affiche) ou "full" (complet).
#' @param max_cells Cap explicite ; par defaut le parametre declare en config.
#' @param seurat_obj Objet Seurat courant optionnel (verification de peremption).
#' @return A ggplot object.
#' @export
plot_velocity_phase_portrait <- function(
    validated_result,
    gene,
    data_scope = c("preview", "full"),
    max_cells = NULL,
    seurat_obj = NULL
) {
    data_scope <- match.arg(data_scope)
    assert_velocity_result(
        validated_result, view = "matrices", seurat_obj = seurat_obj,
        context = "phase portrait velocity"
    )

    if (is.null(max_cells)) {
        max_cells <- if (exists("TS_VELOCITY_MAX_PORTRAIT_CELLS")) {
            TS_VELOCITY_MAX_PORTRAIT_CELLS
        } else {
            50000L
        }
    }
    max_cells <- as.integer(max_cells)

    if (!gene %in% rownames(validated_result$spliced) ||
        !gene %in% rownames(validated_result$unspliced)) {
        .velocity_stop(
            "invalid_input",
            paste0(
                "Le gene selectionne est absent des matrices spliced et ",
                "unspliced : ", gene
            )
        )
    }

    spliced_values <- as.numeric(
        validated_result$spliced[gene, ]
    )

    unspliced_values <- as.numeric(
        validated_result$unspliced[gene, ]
    )

    plot_data <- data.frame(
        unspliced = unspliced_values,
        spliced = spliced_values,
        cell_barcode = colnames(validated_result$spliced),
        stringsAsFactors = FALSE
    )

    n_total <- nrow(plot_data)
    scope_text <- sprintf("Portee des donnees : complet (%d cellules).", n_total)
    if (identical(data_scope, "preview") && n_total > max_cells) {
        set.seed(1L)
        plot_data <- plot_data[
            sample.int(n_total, max_cells),
            ,
            drop = FALSE
        ]
        scope_text <- sprintf(
            paste0("Apercu sous-echantillonne : %d/%d cellules ",
                   "(tirage aleatoire a graine fixe : seed = 1)."),
            max_cells, n_total
        )
    }

    ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = unspliced, y = spliced)
    ) +
        ggplot2::geom_point(
            alpha = 0.35,
            size = 0.7,
            colour = "#3B82F6"
        ) +
        ggplot2::theme_classic() +
        ggplot2::labs(
            x = "Transcrits unspliced",
            y = "Transcrits spliced",
            title = paste("Phase portrait :", gene),
            subtitle = paste(
                paste0(
                    "Nuage spliced / unspliced — visualisation descriptive, ",
                    "pas un modele de transcription."
                ),
                scope_text,
                sprintf("Analyse : %s.", validated_result$analysis_id %||% NA_character_),
                sep = " "
            )
        )
}

#' Validate precomputed two-dimensional velocity vectors
#'
#' @param vectors Numeric matrix with one row per aligned cell.
#' @param cell_names Expected aligned cell barcodes.
#' @param selected_reduction Selected reduction name.
#' @param stored_reduction Reduction stored with the vectors.
#' @return Validated vector matrix.
#' @export
validate_precomputed_velocity_vectors <- function(
    vectors,
    cell_names,
    selected_reduction,
    stored_reduction = NULL
) {
    if (is.null(vectors)) {
        .velocity_stop(
            "invalid_vector_projection",
            "Aucun vecteur velocity pre-calcule n'est disponible."
        )
    }

    vectors <- as.matrix(vectors)

    if (!is.numeric(vectors) ||
        ncol(vectors) != 2L) {
        .velocity_stop(
            "invalid_vector_projection",
            paste0(
                "Les vecteurs velocity pre-calcules doivent avoir exactement ",
                "deux colonnes numeriques."
            )
        )
    }

    if (nrow(vectors) != length(cell_names)) {
        .velocity_stop(
            "invalid_vector_projection",
            paste0(
                "Le nombre de vecteurs velocity ne correspond pas au nombre ",
                "de cellules alignees."
            )
        )
    }

    if (is.null(rownames(vectors))) {
        .velocity_stop(
            "invalid_vector_projection",
            paste0(
                "Les vecteurs velocity doivent posseder des noms de lignes ",
                "correspondant aux cellules."
            )
        )
    }

    if (anyDuplicated(rownames(vectors))) {
        .velocity_stop(
            "invalid_vector_projection",
            "Les vecteurs velocity contiennent des noms de lignes dupliques."
        )
    }

    if (!identical(rownames(vectors), as.character(cell_names))) {
        .velocity_stop(
            "invalid_vector_projection",
            paste0(
                "L'ordre des cellules des vecteurs velocity ne correspond pas ",
                "a l'ordre des cellules Seurat."
            )
        )
    }

    if (!is.null(stored_reduction) &&
        !identical(
            as.character(stored_reduction),
            as.character(selected_reduction)
        )) {
        .velocity_stop(
            "invalid_vector_projection",
            paste0(
                "Les vecteurs velocity ont ete calcules pour la reduction '",
                stored_reduction,
                "' et ne peuvent pas etre affiches dans '",
                selected_reduction,
                "'."
            )
        )
    }

    if (any(!is.finite(vectors))) {
        .velocity_stop(
            "invalid_vector_projection",
            paste0(
                "Les vecteurs velocity contiennent des valeurs NA, NaN ou ",
                "infinies."
            )
        )
    }

    vectors
}

#' Align a partial velocity embedding to validated velocity cells
#'
#' Partial embeddings are allowed and never padded: cells without stored
#' coordinates are simply reported as missing (n_embedding_missing). The
#' embedding is NOT transposed and is NOT interpreted as velocity vectors.
#'
#' @param embedding Numeric two-column matrix.
#' @param cell_names Validated velocity cell barcodes.
#' @param selected_reduction Selected reduction name.
#' @param stored_reduction Optional reduction stored with embedding.
#' @return A list with aligned embedding and counts.
#' @export
align_velocity_embedding <- function(
    embedding,
    cell_names,
    selected_reduction = "umap",
    stored_reduction = NULL
) {
    embedding <- as.matrix(embedding)

    if (!is.numeric(embedding) ||
        ncol(embedding) != 2L) {
        .velocity_stop(
            "invalid_input",
            paste0(
                "L'embedding doit avoir exactement deux colonnes numeriques."
            )
        )
    }

    if (is.null(rownames(embedding))) {
        .velocity_stop(
            "invalid_input",
            "L'embedding doit posseder des noms de lignes."
        )
    }

    if (anyDuplicated(rownames(embedding))) {
        .velocity_stop(
            "invalid_input",
            "L'embedding contient des cellules dupliquees."
        )
    }

    if (!is.null(stored_reduction) &&
        !identical(
            as.character(stored_reduction),
            as.character(selected_reduction)
        )) {
        .velocity_stop(
            "invalid_input",
            paste0(
                "L'embedding correspond a la reduction '",
                stored_reduction,
                "' et non a '",
                selected_reduction,
                "'."
            )
        )
    }

    cell_names <- as.character(cell_names)
    matched <- intersect(cell_names, rownames(embedding))

    if (!length(matched)) {
        .velocity_stop(
            "invalid_input",
            paste0(
                "Aucune cellule de l'embedding ne correspond aux cellules ",
                "velocity."
            )
        )
    }

    ordered_embedding <- embedding[
        match(matched, rownames(embedding)),
        ,
        drop = FALSE
    ]

    ordered_cells <- cell_names[
        match(matched, cell_names)
    ]

    rownames(ordered_embedding) <- ordered_cells

    list(
        embedding = ordered_embedding,
        cell_names = ordered_cells,
        n_velocity_cells = length(cell_names),
        n_embedding_cells = nrow(embedding),
        n_embedding_matched = nrow(ordered_embedding),
        n_embedding_missing = length(cell_names) -
            nrow(ordered_embedding)
    )
}

#' Velocity object fingerprint
#'
#' Empreinte deterministe de l'objet Seurat courant (Stage 8) : dimensions des
#' deux axes + 20 premiers/derniers barcodes de cellules + 20 premiers/derniers
#' identifiants de genes. N'utilise JAMAIS l'adresse memoire : deux sessions
#' avec le meme jeu de donnees produisent la meme empreinte. Toute modification
#' des cellules OU des genes rend un resultat velocity anterieur perime
#' (velocity_result_is_stale()).
#'
#' @param obj Seurat object.
#' @return Character fingerprint ("v2::n_genes::n_cells::..."), NULL si objet NULL.
#' @export
velocity_object_fingerprint <- function(obj) {
    if (is.null(obj)) return(NULL)
    paste(
        "v2",
        nrow(obj),
        ncol(obj),
        paste(head(colnames(obj), 20L), collapse = "|"),
        paste(tail(colnames(obj), 20L), collapse = "|"),
        paste(head(rownames(obj), 20L), collapse = "|"),
        paste(tail(rownames(obj), 20L), collapse = "|"),
        sep = "::"
    )
}

#' Le resultat velocity est-il perime vis-a-vis de l'objet Seurat courant ?
#'
#' @param velocity_result Resultat canonique (finalize_velocity_result()).
#' @param seurat_obj Objet Seurat courant.
#' @return TRUE si les empreintes divergent, FALSE sinon, NA si l'identite du
#'   resultat ou de l'objet est indeterminable.
#' @export
velocity_result_is_stale <- function(velocity_result, seurat_obj) {
    if (is.null(velocity_result)) return(NA)
    fp <- velocity_result$object_identity$fingerprint %||% NULL
    if (is.null(fp) || is.null(seurat_obj)) return(NA)
    !identical(fp, velocity_object_fingerprint(seurat_obj))
}

#' Finaliser le resultat velocity canonique (Stage 8 — Velocity 3B-1)
#'
#' Assemble l'objet canonique documente (voir en-tete de fichier) a partir de
#' la structure validee par validate_velocity_matrices(). Aucune inference,
#' aucun remplissage : les donnees absentes restent NULL avec un statut ou un
#' avertissement explicite. Les vecteurs fournis sont re-valides strictement
#' (meme contrat que validate_precomputed_velocity_vectors()). La provenance
#' est PRODUITE ici (regle 7 AGENTS.md) ; l'appelant l'append ensuite a l'etat
#' partage — elle n'est jamais reconstruite apres coup.
#'
#' Resolution du statut (etat le plus limitant d'abord) :
#'   1. invalid_vector_projection — vecteurs fournis mais rejetes (les
#'      matrices alignees restent dans l'objet, fleches interdites) ;
#'   2. valid_partial_embedding  — embedding couvrant partiellement les
#'      cellules alignees ;
#'   3. valid_no_vectors         — aucune vecteur valide disponible ;
#'   4. valid                    — matrices + vecteurs verifies.
#' Les etats invalid_input/invalid_orientation/invalid_cell_alignment/
#' invalid_gene_alignment sont des erreurs levees EN AMONT par la chaine de
#' validation (classe velocity_validation_error) : aucun objet canonique n'est
#' produit dans ces cas.
#'
#' @param validated Structure validee retournee par validate_velocity_matrices()
#'   (enrichie des metadonnees RDS : velocity_source, velocity_method,
#'   input_orientation, embedding_reduction, umap_embedding, clusters).
#' @param input_mode Mode d'import declare : "rds", "mtx" ou "loom"
#'   (.loom/.h5ad — roadmap SC FUNCTION_TEST M0 ; lecture pre-computed,
#'   aucune inference).
#' @param input_files Liste nommee decrivant la source (noms de fichiers
#'   ORIGINAUX — jamais les chemins locaux complets — et options de lecture
#'   comme feature_column).
#' @param seurat_obj Objet Seurat courant. Seule l'identite est extraite ; tout
#'   objet a dimnames est acceptable (testabilite hors Shiny).
#' @param assay_used Assay Seurat utilise, NULL si non applicable (import
#'   pre-calcule : le champ reste NULL avec cette raison documentee).
#' @param requested_reduction Reduction demandee pour l'affichage des vecteurs.
#' @param embedding_alignment Sortie de align_velocity_embedding() ou NULL.
#' @param velocity_vectors Matrice dx/dy deja validee ou NULL. Toute matrice
#'   non NULL est re-valide strictement ici.
#' @param vector_validation list(field =, ok =, reason =) decrivant le sort du
#'   champ de vecteurs fourni (ok = FALSE => statut invalid_vector_projection).
#' @param extra_warnings Avertissements supplementaires produits par
#'   l'orchestration (ex. embedding ignore) — fusionnes, sans doublon.
#' @param analysis_id Identifiant d'analyse (ex. "sc-velocity").
#' @param seed Graine ; NULL car aucune operation stochastique dans la chaine
#'   de validation.
#' @return L'objet canonique : la structure validee ENRICHIE des champs du
#'   contrat (type, status, dimensions, cell_alignment, gene_alignment,
#'   embedding_alignment, velocity_vectors, vector_validation, input_summary,
#'   object_identity, warnings, provenance, analysis_id, timestamp_utc).
#' @export
finalize_velocity_result <- function(
    validated,
    input_mode = c("rds", "mtx", "loom"),
    input_files = NULL,
    seurat_obj = NULL,
    assay_used = NULL,
    requested_reduction = NULL,
    embedding_alignment = NULL,
    velocity_vectors = NULL,
    vector_validation = NULL,
    extra_warnings = character(0),
    analysis_id = "sc-velocity",
    seed = NULL
) {
    input_mode <- match.arg(input_mode)

    if (!is.list(validated) || is.null(validated$spliced) ||
        is.null(validated$unspliced)) {
        .velocity_stop(
            "invalid_input",
            paste0(
                "finalize_velocity_result() : structure validee absente — ",
                "validate_velocity_matrices() doit etre appelee d'abord."
            )
        )
    }

    # Vecteurs fournis : re-validation stricte ; tout echec est une erreur
    # classee invalid_vector_projection (aucune fabrication de fleches).
    if (!is.null(velocity_vectors)) {
        velocity_vectors <- validate_precomputed_velocity_vectors(
            vectors = velocity_vectors,
            cell_names = validated$cell_names,
            selected_reduction = requested_reduction,
            stored_reduction = validated$embedding_reduction
        )
    }

    vector_rejected <- !is.null(vector_validation) && isFALSE(vector_validation$ok)
    n_embedding_missing <- embedding_alignment$n_embedding_missing %||% 0L
    partial_embedding <- !is.null(embedding_alignment) &&
        n_embedding_missing > 0L

    status <- if (isTRUE(vector_rejected)) {
        "invalid_vector_projection"
    } else if (partial_embedding) {
        "valid_partial_embedding"
    } else if (is.null(velocity_vectors)) {
        "valid_no_vectors"
    } else {
        "valid"
    }

    warnings_all <- unique(c(
        as.character(validated$warnings %||% character(0)),
        as.character(extra_warnings)
    ))
    if (isTRUE(vector_rejected)) {
        warnings_all <- c(warnings_all, paste0(
            "Vecteurs velocity pre-calcules rejetes (champ '",
            vector_validation$field %||% "inconnu", "') : ",
            vector_validation$reason %||% "raison inconnue",
            ". Fleches desactivees ; les matrices alignees restent utilisables."
        ))
    }

    result <- validated
    result$type <- "rna_velocity"
    result$status <- status
    result$dimensions <- c(
        cells = ncol(validated$spliced),
        genes = nrow(validated$spliced)
    )
    result$cell_alignment <- list(
        match_mode = validated$cell_match_mode,
        n_input = validated$n_cells_input,
        n_matched = validated$n_cells_matched,
        n_missing = validated$n_cells_missing,
        overlap_raw = validated$raw_cell_overlap,
        overlap_normalized = validated$normalized_cell_overlap %||%
            validated$overlap,
        low_overlap_override = isTRUE(validated$allow_low_overlap)
    )
    result$gene_alignment <- list(
        match_mode = validated$gene_match_mode,
        n_input = validated$n_genes_input,
        n_matched = validated$n_genes_matched,
        n_missing = validated$n_genes_missing
    )
    result$embedding_alignment <- embedding_alignment
    result$velocity_vectors <- velocity_vectors
    result$vector_validation <- vector_validation
    result$input_summary <- list(
        input_mode = input_mode,
        files = input_files,
        velocity_source = validated$velocity_source,
        velocity_method = validated$velocity_method,
        orientation_declared = validated$input_orientation,
        orientation_detected = validated$orientation,
        strip_cell_suffix = isTRUE(validated$strip_cell_suffix),
        strip_gene_version = isTRUE(validated$strip_gene_version),
        allow_low_overlap = isTRUE(validated$allow_low_overlap),
        n_cells_input = validated$n_cells_input,
        n_genes_input = validated$n_genes_input
    )
    result$object_identity <- list(
        fingerprint = velocity_object_fingerprint(seurat_obj),
        method = paste0(
            "v2 : dimensions + 20 premiers/derniers barcodes de cellules et ",
            "identifiants de genes (deterministe, sans adresse memoire)"
        ),
        seurat_dims = if (is.null(seurat_obj)) {
            c(seurat_genes = NA_integer_, seurat_cells = NA_integer_)
        } else {
            c(
                seurat_genes = as.integer(nrow(seurat_obj)),
                seurat_cells = as.integer(ncol(seurat_obj))
            )
        }
    )
    result$warnings <- warnings_all

    # Provenance PRODUITE a la validation (regle 7 AGENTS.md) ; consolidation
    # reservee au rapport (macro-step 4F).
    entry <- new_provenance_entry(
        analysis_id = analysis_id,
        method = validated$velocity_method %||% "precomputed",
        parameters = list(
            input_mode = input_mode,
            input_files = input_files,
            orientation_declared = validated$input_orientation,
            orientation_detected = validated$orientation,
            strip_cell_suffix = isTRUE(validated$strip_cell_suffix),
            strip_gene_version = isTRUE(validated$strip_gene_version),
            allow_low_overlap = isTRUE(validated$allow_low_overlap),
            n_cells_input = validated$n_cells_input,
            n_genes_input = validated$n_genes_input,
            n_cells_matched = validated$n_cells_matched,
            n_genes_matched = validated$n_genes_matched,
            n_cells_missing = validated$n_cells_missing,
            n_genes_missing = validated$n_genes_missing,
            vectors_available = !is.null(velocity_vectors),
            vector_field = vector_validation$field %||% NULL,
            embedding_cells_matched = embedding_alignment$n_embedding_matched %||% NULL,
            embedding_cells_missing = n_embedding_missing,
            requested_reduction = requested_reduction,
            assay_used = assay_used,
            object_fingerprint = velocity_object_fingerprint(seurat_obj)
        ),
        dataset = seurat_obj,
        cells_used = validated$n_cells_matched,
        cells_excluded = validated$n_cells_missing,
        seed = seed,
        warnings = warnings_all
    )
    entry$analysis_type <- "rna_velocity"
    entry$status <- status
    entry$input_mode <- input_mode
    entry$timestamp_utc <- format(
        entry$timestamp,
        "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"
    )

    result$provenance <- entry
    result$analysis_id <- analysis_id
    result$timestamp_utc <- entry$timestamp_utc

    result
}

#' Resume de validation velocity pour export CSV (Stage 8)
#'
#' Une ligne, colonnes stables : analysis_id, horodatage, statut, identite
#' source/resultat, reglages, compteurs d'alignement, couverture d'embedding,
#' disponibilite des vecteurs, empreinte objet, avertissements et versions
#' logicielles. Lecture directe de l'objet canonique — aucune deduction.
#'
#' @param velocity_result Resultat canonique (finalize_velocity_result()).
#' @return data.frame a une ligne, colonnes character.
#' @export
build_velocity_validation_summary <- function(velocity_result) {
    r <- velocity_result
    if (is.null(r) || !is.list(r)) {
        .velocity_stop(
            "invalid_input",
            paste0(
                "build_velocity_validation_summary() : resultat velocity ",
                "canonique requis."
            )
        )
    }

    p <- r$provenance %||% list()
    emb <- r$embedding_alignment
    vv <- r$vector_validation
    fp <- r$object_identity$fingerprint %||% NA_character_
    seurat_dims <- r$object_identity$seurat_dims %||%
        c(seurat_genes = NA_integer_, seurat_cells = NA_integer_)

    .fmt_file_list <- function(files) {
        if (is.null(files) || length(files) == 0L) return(NA_character_)
        files <- as.list(files)
        paste(
            sprintf("%s=%s", names(files), vapply(files, paste, character(1), collapse = ",")),
            collapse = "; "
        )
    }

    data.frame(
        analysis_id = r$analysis_id %||% NA_character_,
        analysis_type = r$type %||% "rna_velocity",
        status = r$status %||% NA_character_,
        timestamp_utc = r$timestamp_utc %||% NA_character_,
        input_mode = r$input_summary$input_mode %||% NA_character_,
        input_files = .fmt_file_list(r$input_summary$files),
        velocity_source = r$velocity_source %||% NA_character_,
        velocity_method = r$velocity_method %||% NA_character_,
        orientation_declared = r$input_summary$orientation_declared %||% NA_character_,
        orientation_detected = r$orientation %||% NA_character_,
        strip_cell_suffix = as.character(isTRUE(r$strip_cell_suffix)),
        strip_gene_version = as.character(isTRUE(r$strip_gene_version)),
        allow_low_overlap = as.character(isTRUE(r$allow_low_overlap)),
        n_cells_input = as.character(r$n_cells_input %||% NA_integer_),
        n_genes_input = as.character(r$n_genes_input %||% NA_integer_),
        n_cells_matched = as.character(r$n_cells_matched %||% NA_integer_),
        n_genes_matched = as.character(r$n_genes_matched %||% NA_integer_),
        n_cells_missing = as.character(r$n_cells_missing %||% NA_integer_),
        n_genes_missing = as.character(r$n_genes_missing %||% NA_integer_),
        raw_cell_overlap = as.character(r$raw_cell_overlap %||% NA_real_),
        normalized_cell_overlap = as.character(
            r$normalized_cell_overlap %||% r$overlap %||% NA_real_
        ),
        embedding_cells_matched = as.character(
            emb$n_embedding_matched %||% NA_integer_
        ),
        embedding_cells_missing = as.character(
            emb$n_embedding_missing %||% NA_integer_
        ),
        embedding_cells_total = as.character(
            emb$n_velocity_cells %||% NA_integer_
        ),
        vectors_available = as.character(!is.null(r$velocity_vectors)),
        vector_field = vv$field %||% NA_character_,
        vector_ok = as.character(vv$ok %||% NA),
        requested_reduction = r$provenance$parameters$requested_reduction %||% NA_character_,
        assay_used = r$provenance$parameters$assay_used %||% "non_applicable_import_precalcule",
        object_fingerprint = fp,
        seurat_n_genes = as.character(seurat_dims["seurat_genes"]),
        seurat_n_cells = as.character(seurat_dims["seurat_cells"]),
        seed = if (is.null(p$seed)) "" else as.character(p$seed),
        warnings = paste(r$warnings %||% character(0), collapse = " | "),
        package_versions = .provenance_versions_string(p$versions),
        stringsAsFactors = FALSE
    )
}

# Versions logicielles aplaties pour l'export CSV (lecture seule de l'entree
# de provenance ; aucune re-interrogation des packages ici).
.provenance_versions_string <- function(versions) {
    if (is.null(versions) || length(versions) == 0L) return(NA_character_)
    paste(
        sprintf("%s=%s", names(versions), as.character(versions)),
        collapse = "; "
    )
}

#' Table d'alignement cellules/genes pour export CSV (Stage 8)
#'
#' Fusionne les tables d'alignement produites a la validation en un format long
#' exportable : axis ("cell"/"gene"), identifiant Seurat, identifiants velocity
#' (original et normalise), statut d'alignement. La table est enregistree au
#' moment de la validation — elle n'est jamais reconstruite ici.
#'
#' @param velocity_result Resultat canonique (finalize_velocity_result()).
#' @return data.frame (colonnes : axis, seurat_id, velocity_id_original,
#'   velocity_id_normalized, aligned).
#' @export
build_velocity_alignment_mapping <- function(velocity_result) {
    r <- velocity_result
    cm <- r$cell_mapping %||% NULL
    gm <- r$gene_mapping %||% NULL

    if (is.null(cm) && is.null(gm)) {
        .velocity_stop(
            "invalid_input",
            paste0(
                "Aucune table d'alignement disponible : les tables ",
                "cell_mapping/gene_mapping sont produites par ",
                "validate_velocity_matrices() lorsque les identifiants Seurat ",
                "sont fournis."
            )
        )
    }

    cells_df <- if (is.null(cm)) NULL else data.frame(
        axis = "cell",
        seurat_id = cm$seurat_cell,
        velocity_id_original = cm$velocity_cell_original,
        velocity_id_normalized = cm$velocity_cell_normalized,
        aligned = cm$aligned,
        stringsAsFactors = FALSE
    )

    genes_df <- if (is.null(gm)) NULL else data.frame(
        axis = "gene",
        seurat_id = gm$seurat_gene,
        velocity_id_original = gm$velocity_gene_original,
        velocity_id_normalized = gm$velocity_gene_normalized,
        aligned = gm$aligned,
        stringsAsFactors = FALSE
    )

    out <- rbind(cells_df, genes_df)
    out[order(match(out$axis, c("cell", "gene"))), , drop = FALSE]
}

#' Vue densite/couverture des vecteurs valides (Stage 9)
#'
#' Histogramme des magnitudes |dx,dy| des vecteurs STRICTEMENT valides du
#' resultat canonique. Vue descriptive : aucune implication de direction ou
#' de causalite. Sans vecteurs valides, affiche un message explicite au lieu
#' d'un graphique vide.
#'
#' @param validated_result Resultat velocity canonique.
#' @param seurat_obj Objet Seurat courant optionnel (peremption).
#' @return ggplot object.
#' @export
plot_velocity_coverage <- function(validated_result, seurat_obj = NULL) {
    assert_velocity_result(
        validated_result, view = "matrices", seurat_obj = seurat_obj,
        context = "couverture velocity"
    )
    vectors <- validated_result$velocity_vectors
    if (is.null(vectors)) {
        return(
            ggplot2::ggplot() +
                ggplot2::annotate(
                    "text", x = 0.5, y = 0.5,
                    label = paste0(
                        "Aucun vecteur velocity valide : vue de couverture ",
                        "indisponible (coordonnees seules, aucune fleche)."
                    ),
                    size = 4.5, colour = "grey40"
                ) +
                ggplot2::theme_void() +
                ggplot2::labs(
                    title = "Densite des vecteurs velocity valides",
                    subtitle = sprintf(
                        "Aucun vecteur valide (analyse : %s) — coordonnees seules.",
                        validated_result$analysis_id %||% NA_character_
                    )
                )
        )
    }

    vectors <- as.matrix(vectors)
    coverage_df <- data.frame(
        speed = sqrt(
            as.numeric(vectors[, 1L])^2 + as.numeric(vectors[, 2L])^2
        )
    )

    ggplot2::ggplot(coverage_df, ggplot2::aes(x = speed)) +
        ggplot2::geom_histogram(
            bins = 40, fill = "#3B82F6", colour = "white", alpha = 0.85
        ) +
        ggplot2::theme_classic() +
        ggplot2::labs(
            x = "Magnitude du vecteur pre-calcule (|dx, dy|)",
            y = "Cellules",
            title = "Densite des vecteurs velocity valides",
            subtitle = sprintf(
                paste0("%d cellules alignees avec vecteur valide (analyse : %s). ",
                       "Vue descriptive — aucune interpretation biologique."),
                nrow(coverage_df),
                validated_result$analysis_id %||% NA_character_
            )
        )
}

#' Vue QC d'alignement velocity (Stage 9)
#'
#' Effectifs d'entree / alignes / absents pour les cellules, les genes et
#' l'embedding, lus directement dans le resultat canonique. QC technique
#' uniquement : aucune interpretation biologique des populations.
#'
#' @param validated_result Resultat velocity canonique.
#' @param seurat_obj Objet Seurat courant optionnel (peremption).
#' @return ggplot object.
#' @export
plot_velocity_alignment_qc <- function(validated_result, seurat_obj = NULL) {
    assert_velocity_result(
        validated_result, view = "matrices", seurat_obj = seurat_obj,
        context = "QC alignement velocity"
    )
    ca <- validated_result$cell_alignment %||% list()
    ga <- validated_result$gene_alignment %||% list()
    emb <- validated_result$embedding_alignment

    qc_df <- data.frame(
        axis = c(
            rep("Cellules", 3L),
            rep("Genes", 3L)
        ),
        categorie = c(
            "Entree", "Alignees", "Absentes",
            "Entree", "Alignes", "Absents"
        ),
        n = as.integer(c(
            ca$n_input %||% 0L, ca$n_matched %||% 0L, ca$n_missing %||% 0L,
            ga$n_input %||% 0L, ga$n_matched %||% 0L, ga$n_missing %||% 0L
        )),
        stringsAsFactors = FALSE
    )
    if (!is.null(emb)) {
        qc_df <- rbind(
            qc_df,
            data.frame(
                axis = c("Embedding", "Embedding"),
                categorie = c("Couvertes", "Manquantes"),
                n = as.integer(c(
                    emb$n_embedding_matched %||% 0L,
                    emb$n_embedding_missing %||% 0L
                )),
                stringsAsFactors = FALSE
            )
        )
    }

    ggplot2::ggplot(
        qc_df,
        ggplot2::aes(x = stats::reorder(categorie, n), y = n)
    ) +
        ggplot2::geom_col(fill = "#2980B9", alpha = 0.9) +
        ggplot2::facet_wrap(~axis, scales = "free_x") +
        ggplot2::theme_classic() +
        ggplot2::labs(
            x = NULL,
            y = "Effectifs",
            title = "QC d'alignement velocity (technique)",
            subtitle = sprintf(
                "Aucune interpretation biologique. Analyse : %s.",
                validated_result$analysis_id %||% NA_character_
            )
        )
}

#' Export CSV par cellule : vecteurs et alignement (Stage 9)
#'
#' Une ligne par cellule alignee : coordonnees d'embedding (si couvertes),
#' composantes dx/dy du vecteur valide (si disponibles), indicateurs
#' has_vector/in_embedding, plus l'identite de l'analyse. Lecture directe du
#' resultat canonique — aucune deduction.
#'
#' @param validated_result Resultat velocity canonique.
#' @return data.frame (n_cells lignes).
#' @export
build_velocity_cell_vectors_export <- function(validated_result) {
    assert_velocity_result(
        validated_result, view = "matrices",
        context = "export vecteurs par cellule"
    )
    cells <- validated_result$cell_names
    n <- length(cells)

    vec <- validated_result$velocity_vectors
    dx <- rep(NA_real_, n)
    dy <- rep(NA_real_, n)
    if (!is.null(vec)) {
        vec <- as.matrix(vec)
        m <- match(cells, rownames(vec))
        ok <- !is.na(m)
        dx[ok] <- as.numeric(vec[m[ok], 1L])
        dy[ok] <- as.numeric(vec[m[ok], 2L])
    }

    ali <- validated_result$embedding_alignment
    ex <- rep(NA_real_, n)
    ey <- rep(NA_real_, n)
    if (!is.null(ali) && !is.null(ali$embedding)) {
        emb <- as.matrix(ali$embedding)
        m2 <- match(cells, rownames(emb))
        ok2 <- !is.na(m2)
        ex[ok2] <- as.numeric(emb[m2[ok2], 1L])
        ey[ok2] <- as.numeric(emb[m2[ok2], 2L])
    }

    data.frame(
        cell_barcode = cells,
        embedding_x = ex,
        embedding_y = ey,
        vector_dx = dx,
        vector_dy = dy,
        has_vector = !is.na(dx),
        in_embedding = !is.na(ex),
        analysis_id = rep(validated_result$analysis_id %||% NA_character_, n),
        status = rep(validated_result$status %||% NA_character_, n),
        timestamp_utc = rep(validated_result$timestamp_utc %||% NA_character_, n),
        stringsAsFactors = FALSE
    )
}

#' Nom de fichier d'export trace par l'identifiant d'analyse (Stage 9)
#'
#' @param validated_result Resultat velocity canonique.
#' @param kind Prefixe descriptif (ex. "velocity_phase_portrait").
#' @param ext Extension ("png", "pdf", "csv").
#' @return Chaine "<kind>_<analysis_id>_<date>.<ext>".
#' @export
velocity_export_filename <- function(validated_result, kind, ext) {
    if (is.null(validated_result) || !is.list(validated_result)) {
        .velocity_stop(
            "invalid_input",
            paste0(
                "velocity_export_filename() : resultat velocity canonique ",
                "requis."
            )
        )
    }
    aid <- validated_result$analysis_id %||% "sc-velocity"
    sprintf(
        "%s_%s_%s.%s",
        as.character(kind)[1L],
        aid,
        format(Sys.Date(), "%Y-%m-%d"),
        as.character(ext)[1L]
    )
}

#' Build velocity provenance export data
#'
#' @param velocity_result Validated velocity result.
#' @param gene Selected gene.
#' @param selected_reduction Selected reduction.
#' @return Data frame for CSV export.
#' @export
build_velocity_provenance_export <- function(
    velocity_result,
    gene,
    selected_reduction = NA_character_
) {
    if (!gene %in% rownames(velocity_result$spliced) ||
        !gene %in% rownames(velocity_result$unspliced)) {
        .velocity_stop(
            "invalid_input",
            paste0("Gene absent des matrices velocity : ", gene)
        )
    }

    n <- ncol(velocity_result$spliced)

    data.frame(
        cell_barcode = colnames(velocity_result$spliced),
        gene = rep(gene, n),
        spliced = as.numeric(velocity_result$spliced[gene, ]),
        unspliced = as.numeric(velocity_result$unspliced[gene, ]),
        velocity_source = rep(
            velocity_result$velocity_source %||% NA_character_,
            n
        ),
        velocity_method = rep(
            velocity_result$velocity_method %||% "precomputed",
            n
        ),
        matrix_orientation = rep(
            velocity_result$orientation %||% NA_character_,
            n
        ),
        cell_match_mode = rep(
            velocity_result$cell_match_mode %||% NA_character_,
            n
        ),
        gene_match_mode = rep(
            velocity_result$gene_match_mode %||% NA_character_,
            n
        ),
        n_cells_matched = rep(
            velocity_result$n_cells_matched %||% n,
            n
        ),
        n_genes_matched = rep(
            velocity_result$n_genes_matched %||%
                nrow(velocity_result$spliced),
            n
        ),
        raw_cell_overlap = rep(
            velocity_result$raw_cell_overlap %||% NA_real_,
            n
        ),
        normalized_cell_overlap = rep(
            velocity_result$normalized_cell_overlap %||%
                velocity_result$overlap %||% NA_real_,
            n
        ),
        suffix_stripping = rep(
            isTRUE(velocity_result$strip_cell_suffix),
            n
        ),
        version_stripping = rep(
            isTRUE(velocity_result$strip_gene_version),
            n
        ),
        allow_low_overlap = rep(
            isTRUE(velocity_result$allow_low_overlap),
            n
        ),
        selected_reduction = rep(
            selected_reduction %||% NA_character_,
            n
        ),
        embedding_cells_matched = rep(
            velocity_result$embedding_alignment$n_embedding_matched %||%
                NA_integer_,
            n
        ),
        embedding_cells_missing = rep(
            velocity_result$embedding_alignment$n_embedding_missing %||%
                NA_integer_,
            n
        ),
        timestamp = rep(
            format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            n
        ),
        stringsAsFactors = FALSE
    )
}

#' Plot the velocity embedding of a validated result (Stage 9)
#'
#' Consomme EXCLUSIVEMENT un resultat canonique valide. Les fleches proviennent
#' UNIQUEMENT des vecteurs strictement valides embarques dans le resultat
#' (result$velocity_vectors) : des coordonnees seules n'engendrent jamais de
#' fleches, et les coordonnees ne sont JAMAIS substituees aux vecteurs.
#' Couverture partielle, rejet de vecteurs et portee de donnees sont affiches
#' explicitement dans le sous-titre.
#'
#' @param validated_result Resultat velocity canonique (finalize_velocity_result()).
#' @param embedding Matrice/data.frame numerique 2 colonnes de coordonnees,
#'   barcodes en rownames (embedding RDS partiels OU reduction Seurat —
#'   resolution par l'appelant).
#' @param selected_reduction Nom de reduction affiche dans le sous-titre.
#' @param data_scope "preview" (cap TS_VELOCITY_MAX_EMBED_CELLS affiche) ou
#'   "full" (complet).
#' @param max_cells Cap explicite ; par defaut le parametre declare en config.
#' @param seurat_obj Objet Seurat courant optionnel (verification de peremption).
#' @return ggplot object.
#' @export
plot_velocity_embedding <- function(
    validated_result,
    embedding,
    selected_reduction = NULL,
    data_scope = c("preview", "full"),
    max_cells = NULL,
    seurat_obj = NULL
) {
    data_scope <- match.arg(data_scope)
    assert_velocity_result(
        validated_result, view = "matrices", seurat_obj = seurat_obj,
        context = "embedding velocity"
    )

    if (is.null(max_cells)) {
        max_cells <- if (exists("TS_VELOCITY_MAX_EMBED_CELLS")) {
            TS_VELOCITY_MAX_EMBED_CELLS
        } else {
            5000L
        }
    }
    max_cells <- as.integer(max_cells)

    embedding <- as.matrix(embedding)
    if (!is.numeric(embedding) || ncol(embedding) != 2L) {
        .velocity_stop(
            "invalid_input",
            paste0(
                "L'embedding d'affichage doit avoir exactement deux ",
                "colonnes numeriques."
            )
        )
    }
    if (is.null(rownames(embedding))) {
        .velocity_stop(
            "invalid_input",
            paste0(
                "L'embedding d'affichage doit posseder des noms de lignes ",
                "(barcodes)."
            )
        )
    }

    # Fleches UNIQUEMENT depuis les vecteurs valides du resultat canonique ;
    # correspondance exacte par barcodes, sans reordre ni fabrication.
    vectors <- validated_result$velocity_vectors

    df <- data.frame(
        x = as.numeric(embedding[, 1]),
        y = as.numeric(embedding[, 2]),
        xend = NA_real_,
        yend = NA_real_,
        stringsAsFactors = FALSE
    )
    rownames(df) <- rownames(embedding)

    if (!is.null(vectors)) {
        vectors <- as.matrix(vectors)
        m <- match(rownames(embedding), rownames(vectors))
        ok <- !is.na(m)
        df$xend[ok] <- df$x[ok] + as.numeric(vectors[m[ok], 1])
        df$yend[ok] <- df$y[ok] + as.numeric(vectors[m[ok], 2])
    }
    has_arrow <- !is.na(df$xend)

    n_total <- nrow(df)
    subsampled <- FALSE
    if (identical(data_scope, "preview") && n_total > max_cells) {
        set.seed(1L)
        keep_idx <- sample.int(n_total, max_cells)
        df <- df[keep_idx, , drop = FALSE]
        has_arrow <- has_arrow[keep_idx]
        subsampled <- TRUE
    }

    status_text <- if (any(has_arrow)) {
        sprintf(
            paste0("Fleches = deplacement predit valide (%d/%d cellules ",
                   "affichees couvertes par un vecteur)."),
            sum(has_arrow), nrow(df)
        )
    } else if (identical(validated_result$status, "invalid_vector_projection")) {
        paste0(
            "Vecteurs rejetes a la validation — coordonnees seules, aucune ",
            "fleche (aucune substitution coordonnees -> vecteurs)."
        )
    } else {
        paste0(
            "Coordonnees uniquement — aucun vecteur velocity valide fourni ",
            "(aucune fleche fabriquee)."
        )
    }

    coverage_text <- NULL
    ali <- validated_result$embedding_alignment
    if (!is.null(ali) && !is.null(ali$n_velocity_cells) &&
        ali$n_velocity_cells > 0L) {
        coverage_text <- sprintf(
            "Couverture embedding RDS : %d/%d cellules.",
            ali$n_embedding_matched, ali$n_velocity_cells
        )
    }

    scope_text <- if (isTRUE(subsampled)) {
        sprintf(
            paste0("Apercu sous-echantillonne : %d/%d cellules ",
                   "(graine fixe : seed = 1)."),
            max_cells, n_total
        )
    } else {
        sprintf("Portee des donnees : complet (%d cellules).", n_total)
    }

    subtitle_parts <- c(
        status_text,
        coverage_text,
        if (!is.null(selected_reduction)) {
            paste0("Reduction : ", selected_reduction)
        },
        scope_text,
        sprintf("Analyse : %s.", validated_result$analysis_id %||% NA_character_)
    )

    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(size = 0.5, alpha = 0.5, colour = "grey60") +
        ggplot2::theme_classic() +
        ggplot2::labs(
            x = "Dimension 1", y = "Dimension 2",
            title = "RNA Velocity — embedding",
            subtitle = paste(subtitle_parts, collapse = " ")
        )

    if (any(has_arrow)) {
        p <- p + ggplot2::geom_segment(
            data = df[has_arrow, , drop = FALSE],
            ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
            arrow = ggplot2::arrow(length = ggplot2::unit(0.12, "cm")),
            linewidth = 0.25, alpha = 0.6, colour = "#3B82F6"
        )
    }

    p
}

#' Vue champ de vecteurs (Stage 9) — embedding AVEC fleches validees
#'
#' Contrat explicite pour la vue fleches : refuse tout resultat sans vecteurs
#' strictement valides (invalid_vector_projection). Delegation pure a
#' plot_velocity_embedding() — aucune logique de rendu dupliquee.
#'
#' @param validated_result Resultat velocity canonique.
#' @param embedding Coordonnees 2 colonnes, barcodes en rownames.
#' @param selected_reduction Nom de reduction affiche.
#' @param data_scope "preview" ou "full".
#' @param max_cells Cap explicite optionnel.
#' @param seurat_obj Objet Seurat courant optionnel (peremption).
#' @return ggplot object.
#' @export
plot_velocity_vector_field <- function(
    validated_result,
    embedding,
    selected_reduction = NULL,
    data_scope = c("preview", "full"),
    max_cells = NULL,
    seurat_obj = NULL
) {
    data_scope <- match.arg(data_scope)
    assert_velocity_result(
        validated_result, view = "vectors", seurat_obj = seurat_obj,
        context = "champ de vecteurs velocity"
    )
    plot_velocity_embedding(
        validated_result = validated_result,
        embedding = embedding,
        selected_reduction = selected_reduction,
        data_scope = data_scope,
        max_cells = max_cells,
        seurat_obj = seurat_obj
    )
}

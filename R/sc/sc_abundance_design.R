# =============================================================================
# R/sc/sc_abundance_design.R — Abondance differentielle 4E-0 : validation du
# plan experimental (Stage 13)
# =============================================================================
# But : BLOQUER les plans invalides AVANT que Milo (4E-1) / scCODA (4E-2)
# n'existent. PRINCIPE CENTRAL : les cellules ne sont PAS des replicats
# biologiques — aucune pseudoreplication n'est toleree (une condition avec un
# seul echantillon est refusee, quelle que soit sa taille).
#
# Pur domaine : aucune reactivite Shiny, aucune dependance Seurat au
# chargement (le module extrait obj@meta.data ; l'identite d'objet reutilise
# velocity_object_fingerprint()). Sourced in app.R AFTER R/sc/sc_communication_views.R,
# BEFORE modules/sc/*.R. Constantes TS_DA_* definies dans config/defaults.R.
#
# ── CONTRAT DE RESULTAT CANONIQUE (Stage 13) ────────────────────────────────
# Assemble par finalize_da_design_result() a partir de la structure validee
# par validate_da_design(). Champs du contrat (da_design_contract_fields()) :
#
#   type                  "da_design" (constant)
#   status                etat de validite (da_design_validity_states())
#   composition_unit      "sample" — l'unite de composition est EXPLICITE
#   config                colonnes choisies (sample/replicate/condition/batch/
#                         identity) + replicate_equals_sample
#   condition_summary     data.frame par condition (n_samples, n_replicates,
#                         n_cells, median/min/max cells)
#   sample_summary        data.frame par echantillon (condition, replicate,
#                         batch, n_cells, n_identities)
#   condition_batch_table data.frame long condition x batch (NULL si batch
#                         non demande)
#   identity_coverage     data.frame par identite (n_samples, total_cells,
#                         min/median par echantillon) ou NULL
#   missingness           data.frame colonne -> n_missing (colonnes utilisees)
#   exclusions            data.frame raison -> n_rows (cellules exclues)
#   milo_eligibility      list(eligible, blockers)
#   sccoda_eligibility    list(eligible, blockers)
#   object_identity       list(fingerprint, method, seurat_dims) — empreinte
#                         v2 REUTILISEE de velocity_object_fingerprint()
#   warnings              vecteur character (produits a la validation)
#   provenance            entree new_provenance_entry() enrichie
#   analysis_id           "sc-da-design"
#   timestamp_utc         horodatage UTC ISO-8601
#
# ── DEUX NIVEAUX D'ECHEC (explicitement separes) ────────────────────────────
#   1. Entree structurale invalide (colonnes absentes, metadata vide...) :
#      ERREUR classee da_design_error, etat invalid_input — AUCUN resultat.
#   2. Plan scientifiquement invalide (pseudoreplication, echantillon multi-
#      conditions, IDs = barcodes, confondance batch...) : LE RESULTAT EST
#      PRODUIT avec status invalid_design + rapport complet — l'UI explique
#      les raisons ; les consommateurs (Milo/scCODA) passent par
#      assert_da_design_result(method = "milo"/"sccoda") qui REFUSE un design
#      non eligible. Un design bloque n'est jamais consommable.
#
# ── API PUBLIQUE FIGEE (Stage 13) ───────────────────────────────────────────
# da_design_public_api() ; test de freeze
# (tests/testthat/test-da-design-contract-freeze.R). Helpers internes :
#   .DA_DESIGN_STATUS_STATES, .da_design_stop(), .da_design_is_blank(),
#   .da_design_object_fingerprint()
# =============================================================================

#' Champs documentes du contrat de design DA canonique
#'
#' Source de verite partagee par le code, le test de freeze et
#' docs/contracts/DA_DESIGN_CONTRACT.md — toute modification passe par les
#' TROIS simultanement.
#'
#' @return Vecteur character des 17 champs contractuels.
#' @export
da_design_contract_fields <- function() {
  c(
    "type", "status", "composition_unit", "config",
    "condition_summary", "sample_summary", "condition_batch_table",
    "identity_coverage", "missingness", "exclusions",
    "milo_eligibility", "sccoda_eligibility",
    "object_identity", "warnings", "provenance",
    "analysis_id", "timestamp_utc"
  )
}

# Etats de validite (source de verite ; l'accesseur public est l'unique
# lecture autorisee ailleurs).
.DA_DESIGN_STATUS_STATES <- c(
  "valid",
  "valid_with_warnings",
  "invalid_design",
  "invalid_input",
  "stale_against_current_seurat_object"
)

#' Etats de validite du contrat design DA
#'
#' @return Vecteur character des 5 etats documentes.
#' @export
da_design_validity_states <- function() .DA_DESIGN_STATUS_STATES

#' Libelles francais des etats de validite (affichage utilisateur)
#'
#' @return Vecteur character nomme par etat.
#' @export
da_design_status_labels <- function() {
  c(
    valid = paste0(
      "Design valide : replicats biologiques suffisants, echantillons ",
      "cohérents, aucune alerte. Les cellules ne sont PAS des replicats — ",
      "seuls les echantillons comptent comme unites statistiques."
    ),
    valid_with_warnings = paste0(
      "Design utilisable avec avertissements : replicats suffisants mais ",
      "desequilibres ou echantillons/identites faibles — lisez les ",
      "avertissements avant toute interpretation."
    ),
    invalid_design = paste0(
      "Design BLOQUE : le plan experimental viole un prerequis scientifique ",
      "(pseudoreplication, echantillon multi-conditions, IDs = barcodes, ",
      "confondance batch...). Les methodes d'abundance differentielle sont ",
      "refusees sur ce design."
    ),
    invalid_input = paste0(
      "Entree invalide : colonnes absentes ou metadata inexploitables — ",
      "aucun rapport de design n'est produit."
    ),
    stale_against_current_seurat_object = paste0(
      "Perime : le resultat de design correspond a un objet Seurat ",
      "anterieur — revalidez le design."
    )
  )
}

#' Le statut exprime-t-il un design consommable (non bloque) ?
#'
#' @param status Chaine d'etat (result$status).
#' @return Logical — TRUE uniquement pour valid / valid_with_warnings.
#' @export
da_design_status_is_valid <- function(status) {
  status %in% c("valid", "valid_with_warnings")
}

#' Erreur de validation design DA avec etat structure
.da_design_stop <- function(state, message) {
  stop(errorCondition(
    message,
    state = state,
    class = "da_design_error"
  ))
}

#' Etat de validite associe a une erreur design DA
#'
#' @param e Condition (erreur) capturee.
#' @return Chaine d'etat structuree, NA_character_ pour une erreur sans etat.
#' @export
# §14.1 : délégation à l'accesseur générique. Famille F1 MESURÉE — la CLASSE
# est vérifiée : l'état d'une erreur d'un autre domaine rend NA.
da_design_error_state <- function(e) ts_error_state(e, "da_design_error")

#' Champ vide ? (NA, "" ou whitespace)
.da_design_is_blank <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

# Empreinte objet : REUTILISE velocity_object_fingerprint (pas de deuxieme
# formule d'empreinte dans le projet).
.da_design_object_fingerprint <- function(obj) {
  if (is.null(obj)) return(NULL)
  if (!exists("velocity_object_fingerprint", mode = "function")) {
    .da_design_stop(
      "invalid_input",
      paste0("Empreinte objet design DA : R/sc/sc_velocity.R doit etre ",
             "sourced avant R/sc/sc_abundance_design.R (velocity_object_",
             "fingerprint est reutilise, jamais duplique).")
    )
  }
  velocity_object_fingerprint(obj)
}

#' Valider le plan experimental d'une analyse d'abundance differentielle
#'
#' Vérifie, sur les metadonnees de cellules (obj@meta.data) :
#'   1. colonnes existantes (erreur structurale si absentes) ;
#'   2. IDs d'echantillon qui ne sont PAS des barcodes de cellules ;
#'   3. chaque echantillon mappe UNE seule condition ;
#'   4. replicats biologiques suffisants par condition (plancher
#'      TS_DA_MIN_REPLICATES_PER_CONDITION — pseudoreplication BLOQUEE) ;
#'   5. confondance condition x batch quand un modele batch est demande
#'      (reutilise check_design_confounding()) ;
#'   6. effectifs cellulaires "meaningful" par echantillon (avertissement) ;
#'   7. representation adequate des identites selectionnees (avertissement) ;
#'   8. missingness surfcee ; 9. unite de composition explicite (sample).
#' Les bloqueurs et avertissements sont COLLECTES (pas de fail-fast) afin que
#' le rapport montre TOUTES les raisons.
#'
#' @param metadata data.frame de metadonnees de cellules (rownames = barcodes).
#' @param sample_id Nom de la colonne ID echantillon (requis).
#' @param condition Nom de la colonne condition (requis).
#' @param replicate_id Nom de la colonne ID replicat biologique, ou NULL/NA
#'   pour declarer explicitement "l'echantillon EST le replicat".
#' @param batch Nom de la colonne batch, ou NULL (modele batch non demande).
#' @param identity Nom de la colonne identite/cluster, ou NULL.
#' @param context Contexte cite dans les messages d'erreur.
#' @return list(status, blockers, warnings, config, sample_table (par
#'   echantillon), condition_summary, condition_batch_table, identity_coverage,
#'   missingness, exclusions, n_conditions, n_samples, n_cells).
#' @export
validate_da_design <- function(metadata,
                               sample_id,
                               condition,
                               replicate_id = NULL,
                               batch = NULL,
                               identity = NULL,
                               context = "validation du design DA") {
  if (is.null(metadata) || !is.data.frame(metadata) || nrow(metadata) == 0L) {
    .da_design_stop(
      "invalid_input",
      sprintf(
        "Echec %s : des metadonnees de cellules non vides sont requises (recu : %s).",
        context,
        if (is.null(metadata)) "NULL" else paste(class(metadata), collapse = "/")
      )
    )
  }
  required <- c(sample_id = sample_id, condition = condition)
  missing_cols <- required[!required %in% colnames(metadata)]
  if (length(missing_cols)) {
    .da_design_stop(
      "invalid_input",
      sprintf(
        paste0("Echec %s : colonne(s) requise(s) absente(s) des ",
               "metadonnees : %s (disponibles : %s)."),
        context,
        paste(missing_cols, collapse = ", "),
        paste(utils::head(colnames(metadata), 15), collapse = ", ")
      )
    )
  }
  optional <- c(
    replicate_id = replicate_id, batch = batch, identity = identity
  )
  optional <- optional[!is.na(optional) & nzchar(optional)]
  missing_opt <- optional[!optional %in% colnames(metadata)]
  if (length(missing_opt)) {
    .da_design_stop(
      "invalid_input",
      sprintf(
        paste0("Echec %s : colonne(s) optionnelle(s) declaree(s) absente(s) ",
               "des metadonnees : %s."),
        context, paste(missing_opt, collapse = ", ")
      )
    )
  }

  barcodes <- as.character(rownames(metadata))
  sid <- as.character(metadata[[sample_id]])
  cond <- as.character(metadata[[condition]])
  rep_col_given <- !is.null(replicate_id) && !is.na(replicate_id) && nzchar(replicate_id)
  repv <- if (rep_col_given) as.character(metadata[[replicate_id]]) else sid
  bat <- if (!is.null(batch) && !is.na(batch) && nzchar(batch)) {
    as.character(metadata[[batch]])
  } else NULL
  ident <- if (!is.null(identity) && !is.na(identity) && nzchar(identity)) {
    as.character(metadata[[identity]])
  } else NULL

  blockers <- character(0)
  warnings_all <- character(0)

  # ── Missingness + exclusions ─────────────────────────────────────────────
  used_cols <- c(sample_id, condition,
                 if (rep_col_given) replicate_id, if (!is.null(bat)) batch,
                 if (!is.null(ident)) identity)
  missingness <- do.call(rbind, lapply(used_cols, function(cn) {
    data.frame(column = cn, n_missing = as.integer(sum(.da_design_is_blank(metadata[[cn]]))),
               stringsAsFactors = FALSE)
  }))

  bad_sid <- .da_design_is_blank(sid)
  n_excluded <- sum(bad_sid)
  exclusions <- data.frame(
    reason = c("sample_id manquant ou vide"),
    n_rows = as.integer(c(n_excluded)),
    stringsAsFactors = FALSE
  )
  if (n_excluded == nrow(metadata)) {
    .da_design_stop(
      "invalid_design",
      "Toutes les cellules ont un sample_id manquant : aucun design evaluable."
    )
  }

  sid_ok <- sid[!bad_sid]
  cond_ok <- cond[!bad_sid]
  # Les colonnes optionnelles sont alignees sur l'espace FILTRE (meme
  # longueur que sid_ok) — sinon les index de la table par echantillon
  # pointeraient dans l'espace non filtre (desalignement desactive les
  # exclusions).
  repv_ok <- if (rep_col_given) repv[!bad_sid] else NULL
  bat_ok <- if (!is.null(bat)) bat[!bad_sid] else NULL
  ident_ok <- if (!is.null(ident)) ident[!bad_sid] else NULL

  # ── B1 : sample IDs = barcodes de cellules (pas de pseudobulk possible) ──
  bc_hits <- unique(sid_ok[sid_ok %in% barcodes])
  if (length(bc_hits)) {
    blockers <- c(blockers, sprintf(
      paste0("Des IDs d'echantillon sont des barcodes de cellules (%s) : ",
             "chaque cellule deviendrait son propre echantillon — ",
             "pseudoreplication totale. Indiquez la colonne d'echantillon ",
             "REELLE (origine biologique)."),
      paste(utils::head(bc_hits, 3), collapse = ", ")
    ))
  }

  # ── Table par echantillon ────────────────────────────────────────────────
  sample_ids <- unique(sid_ok)
  sample_table <- do.call(rbind, lapply(sample_ids, function(s) {
    idx <- which(sid_ok == s)
    conds <- unique(cond_ok[idx])
    data.frame(
      sample = s,
      condition = if (length(conds) == 1L) conds else paste(conds, collapse = "|"),
      n_conditions = length(conds),
      replicate = if (rep_col_given) unique(repv_ok[idx])[1L] else s,
      batch = if (!is.null(bat)) unique(bat_ok[idx])[1L] else NA_character_,
      n_cells = length(idx),
      n_identities = if (!is.null(ident)) {
        length(unique(ident_ok[idx]))
      } else NA_integer_,
      stringsAsFactors = FALSE
    )
  }))
  # Echantillons a condition unique et exploitable : base du resume conditions
  # (les echantillons multi-conditions/sans condition restent visibles dans
  # sample_summary et sont traites par les bloqueurs B2/B3).
  valid_st <- sample_table[!is.na(sample_table$n_conditions) &
                             sample_table$n_conditions == 1L &
                             !.da_design_is_blank(sample_table$condition), , drop = FALSE]

  # ── B2 : chaque echantillon mappe UNE condition ──────────────────────────
  multi <- sample_table[sample_table$n_conditions > 1L, ]
  if (nrow(multi)) {
    blockers <- c(blockers, sprintf(
      paste0("%d echantillon(s) mappent plusieurs conditions (%s) : la ",
             "condition d'un echantillon doit etre unique — corrigez la ",
             "table d'origine."),
      nrow(multi),
      paste(utils::head(sprintf("%s -> %s", multi$sample, multi$condition), 3),
            collapse = " ; ")
    ))
  }

  # ── B3 : echantillon sans condition exploitable ──────────────────────────
  na_cond <- sample_table[.da_design_is_blank(sample_table$condition), , drop = FALSE]
  if (nrow(na_cond)) {
    blockers <- c(blockers, sprintf(
      "%d echantillon(s) sans condition exploitable (%s) : assignez-les ou excludez-les explicitement.",
      nrow(na_cond),
      paste(utils::head(na_cond$sample, 3), collapse = ", ")
    ))
  }

  # ── B4 : replicats biologiques suffisants par condition ──────────────────
  conditions <- sort(unique(valid_st$condition))
  n_conditions <- length(conditions)
  condition_summary <- if (n_conditions) do.call(rbind, lapply(conditions, function(cv) {
    st <- valid_st[valid_st$condition == cv, , drop = FALSE]
    reps <- unique(st$replicate)
    data.frame(
      condition = cv,
      n_samples = nrow(st),
      n_replicates = length(reps),
      n_cells = sum(st$n_cells),
      median_cells_per_sample = if (nrow(st)) stats::median(st$n_cells) else NA_real_,
      min_cells_per_sample = if (nrow(st)) min(st$n_cells) else NA_integer_,
      max_cells_per_sample = if (nrow(st)) max(st$n_cells) else NA_integer_,
      stringsAsFactors = FALSE
    )
  })) else NULL

  if (n_conditions < 2L) {
    blockers <- c(blockers, sprintf(
      paste0("%d seule(s) condition(s) exploitable(s) : une analyse ",
             "d'abundance differentielle necessite AU MOINS deux conditions ",
             "a comparer."),
      n_conditions
    ))
  }
  under_replicated <- if (is.null(condition_summary)) NULL else
    condition_summary[condition_summary$n_replicates <
                        TS_DA_MIN_REPLICATES_PER_CONDITION, , drop = FALSE]
  if (!is.null(under_replicated) && nrow(under_replicated)) {
    blockers <- c(blockers, sprintf(
      paste0("Pseudoreplication : %d condition(s) avec moins de %d ",
             "replicats biologiques (%s). Les cellules NE SONT PAS des ",
             "replicats — ajoutez des echantillons biologiques independents."),
      nrow(under_replicated),
      TS_DA_MIN_REPLICATES_PER_CONDITION,
      paste(sprintf("%s (%d)", under_replicated$condition,
                    under_replicated$n_replicates), collapse = ", ")
    ))
  }

  # ── B5 : confondance condition x batch (modele batch demande) ────────────
  condition_batch_table <- NULL
  if (!is.null(bat)) {
    st <- valid_st[!is.na(valid_st$batch), , drop = FALSE]
    condition_batch_table <- if (nrow(st)) {
      agg <- aggregate(cbind(n_samples = sample) ~ condition + batch,
                       data = st, FUN = length)
      agg[order(agg$condition, agg$batch), , drop = FALSE]
    } else NULL

    bat_missing_samples <- sample_table[.da_design_is_blank(sample_table$batch), , drop = FALSE]
    if (nrow(bat_missing_samples)) {
      blockers <- c(blockers, sprintf(
        paste0("Modele batch demande : %d echantillon(s) sans batch ",
               "renseigne (%s) — impossible de modeliser le batch avec des ",
               "valeurs manquantes."),
        nrow(bat_missing_samples),
        paste(utils::head(bat_missing_samples$sample, 3), collapse = ", ")
      ))
    } else if (nrow(st) >= 2L) {
      # Reutilise check_design_confounding() sur la table par echantillon
      # (memes noms de colonnes que les metadonnees d'origine).
      sample_level_df <- data.frame(
        st$condition, st$batch, stringsAsFactors = FALSE
      )
      colnames(sample_level_df) <- c(condition, batch)
      if (isTRUE(check_design_confounding(sample_level_df, condition, batch))) {
        blockers <- c(blockers, sprintf(
          paste0("La condition '%s' est parfaitement confondue avec le ",
                 "batch '%s' : le modele batch est infittable. Renseignez ",
                 "un batch croise avec les conditions ou abandonnez le ",
                 "terme batch."),
          condition, batch
        ))
      }
    }
  }

  # ── W1 : effectifs cellulaires par echantillon ───────────────────────────
  small_samples <- sample_table[sample_table$n_cells < TS_DA_MIN_CELLS_PER_SAMPLE, , drop = FALSE]
  if (nrow(small_samples)) {
    warnings_all <- c(warnings_all, sprintf(
      "%d echantillon(s) sous le seuil de %d cellules (%s) : effectifs faibles, interpretation prudente.",
      nrow(small_samples), TS_DA_MIN_CELLS_PER_SAMPLE,
      paste(utils::head(sprintf("%s (%d)", small_samples$sample, small_samples$n_cells), 3),
            collapse = ", ")
    ))
  }
  cells_per_sample <- sample_table$n_cells
  if (length(cells_per_sample) >= 2L && min(cells_per_sample) > 0L) {
    ratio <- max(cells_per_sample) / min(cells_per_sample)
    if (ratio > TS_DA_CELLS_IMBALANCE_RATIO) {
      warnings_all <- c(warnings_all, sprintf(
        paste0("Desequilibre d'effectifs entre echantillons (ratio max/min = ",
               "%.1f > %s) : les comparaisons restent valides mais les ",
               "puissances sont heterogenes."),
        ratio, TS_DA_CELLS_IMBALANCE_RATIO
      ))
    }
  }

  # ── W2 : representation des identites selectionnees ──────────────────────
  identity_coverage <- NULL
  if (!is.null(ident)) {
    ident_ok <- ident[!bad_sid]
    id_table <- do.call(rbind, lapply(sort(unique(ident_ok[!.da_design_is_blank(ident_ok)])), function(lv) {
      idx <- which(ident_ok == lv)
      sids <- sid_ok[idx]
      per_sample <- as.integer(table(factor(sids, levels = sample_ids)))
      data.frame(
        identity = lv,
        n_samples = sum(per_sample > 0L),
        total_cells = length(idx),
        min_cells_per_sample = if (any(per_sample > 0L)) min(per_sample[per_sample > 0L]) else 0L,
        median_cells_per_sample = if (any(per_sample > 0L)) stats::median(per_sample[per_sample > 0L]) else 0L,
        stringsAsFactors = FALSE
      )
    }))
    identity_coverage <- id_table
    if (!is.null(id_table) && nrow(id_table)) {
      sparse <- id_table[id_table$n_samples < 2L, , drop = FALSE]
      if (nrow(sparse)) {
        warnings_all <- c(warnings_all, sprintf(
          "%d identite(s) presente(s) dans moins de 2 echantillons (%s) : leur comportement par condition ne sera pas interpretable.",
          nrow(sparse), paste(utils::head(sparse$identity, 5), collapse = ", ")
        ))
      }
      weak <- id_table[id_table$n_samples >= 2L &
                         id_table$min_cells_per_sample < TS_DA_MIN_IDENTITY_CELLS_PER_SAMPLE, , drop = FALSE]
      if (nrow(weak)) {
        warnings_all <- c(warnings_all, sprintf(
          "%d identite(s) avec moins de %d cellules dans au moins un echantillon (%s) : effectifs d'identite faibles.",
          nrow(weak), TS_DA_MIN_IDENTITY_CELLS_PER_SAMPLE,
          paste(utils::head(weak$identity, 5), collapse = ", ")
        ))
      }
    }
  }

  status <- if (length(blockers)) "invalid_design"
            else if (length(warnings_all)) "valid_with_warnings"
            else "valid"

  list(
    status = status,
    blockers = as.character(blockers),
    warnings = as.character(warnings_all),
    config = list(
      sample_id = sample_id,
      condition = condition,
      replicate_id = if (rep_col_given) replicate_id else NA_character_,
      batch = if (!is.null(bat)) batch else NA_character_,
      identity = if (!is.null(ident)) identity else NA_character_,
      replicate_equals_sample = !rep_col_given
    ),
    sample_table = sample_table,
    condition_summary = condition_summary,
    condition_batch_table = condition_batch_table,
    identity_coverage = identity_coverage,
    missingness = missingness,
    exclusions = exclusions,
    n_conditions = as.integer(n_conditions),
    n_samples = as.integer(length(sample_ids)),
    n_cells = as.integer(sum(sample_table$n_cells))
  )
}

#' Finaliser le resultat de design DA canonique (Stage 13)
#'
#' Assemble l'objet canonique documente (en-tete de fichier) a partir de la
#' structure validee par validate_da_design(). La provenance est PRODUITE ici
#' (regle 7 AGENTS.md) — y compris pour un design bloque (le rapport et les
#' raisons font partie de l'historique) ; l'appelant l'append a l'etat partage.
#'
#' @param validated Structure validee retournee par validate_da_design().
#' @param seurat_obj Objet Seurat courant optionnel. Seule l'identite est
#'   extraite ; tout objet a dimnames est acceptable.
#' @param extra_warnings Avertissements supplementaires de l'orchestration.
#' @param analysis_id Identifiant d'analyse.
#' @return L'objet canonique (champs da_design_contract_fields()).
#' @export
finalize_da_design_result <- function(validated,
                                      seurat_obj = NULL,
                                      extra_warnings = character(0),
                                      analysis_id = "sc-da-design") {
  if (!is.list(validated) || is.null(validated$status) ||
      is.null(validated$sample_table)) {
    .da_design_stop(
      "invalid_input",
      "finalize_da_design_result() : structure validee absente — validate_da_design() doit etre appelee d'abord."
    )
  }
  if (!validated$status %in% da_design_validity_states()) {
    .da_design_stop(
      "invalid_input",
      sprintf("finalize_da_design_result() : statut inconnu '%s'.", validated$status)
    )
  }

  warnings_all <- unique(c(
    as.character(validated$warnings %||% character(0)),
    as.character(extra_warnings)
  ))

  eligible <- da_design_status_is_valid(validated$status) &&
    validated$n_conditions >= 2L
  reasons <- if (eligible) character(0) else c(
    as.character(validated$blockers %||% character(0)),
    if (!da_design_status_is_valid(validated$status)) "Design bloque." else NULL
  )
  if (da_design_status_is_valid(validated$status) && validated$n_conditions < 2L) {
    reasons <- c(reasons, "Moins de deux conditions : aucun contraste possible.")
  }

  result <- list()
  result$type <- "da_design"
  result$status <- validated$status
  result$composition_unit <- "sample"
  result$config <- validated$config
  result$condition_summary <- validated$condition_summary
  result$sample_summary <- validated$sample_table
  result$condition_batch_table <- validated$condition_batch_table
  result$identity_coverage <- validated$identity_coverage
  result$missingness <- validated$missingness
  result$exclusions <- validated$exclusions
  result$milo_eligibility <- list(eligible = eligible, blockers = reasons)
  result$sccoda_eligibility <- list(eligible = eligible, blockers = reasons)
  result$object_identity <- build_object_identity_v2(seurat_obj)
  result$warnings <- warnings_all

  entry <- new_provenance_entry(
    analysis_id = analysis_id,
    method = "da_design_validation",
    parameters = list(
      sample_id = validated$config$sample_id,
      condition = validated$config$condition,
      replicate_id = validated$config$replicate_id,
      batch = validated$config$batch,
      identity = validated$config$identity,
      replicate_equals_sample = isTRUE(validated$config$replicate_equals_sample),
      composition_unit = "sample",
      n_samples = validated$n_samples,
      n_conditions = validated$n_conditions,
      n_cells = validated$n_cells,
      min_replicates_required = TS_DA_MIN_REPLICATES_PER_CONDITION,
      min_cells_per_sample = TS_DA_MIN_CELLS_PER_SAMPLE,
      n_blockers = length(validated$blockers %||% character(0)),
      n_warnings = length(warnings_all),
      object_fingerprint = result$object_identity$fingerprint
    ),
    dataset = seurat_obj,
    cells_used = validated$n_cells,
    cells_excluded = validated$exclusions$n_rows %||% 0L,
    seed = NULL,
    warnings = warnings_all
  )
  entry$analysis_type <- "da_design"
  entry$status <- validated$status
  entry$timestamp_utc <- format(entry$timestamp, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  result$provenance <- entry
  result$analysis_id <- analysis_id
  result$timestamp_utc <- entry$timestamp_utc

  result
}

#' Le resultat design DA est-il perime vis-a-vis de l'objet Seurat courant ?
#'
#' @param da_design_result Resultat canonique.
#' @param seurat_obj Objet Seurat courant (ou tout objet a dimnames).
#' @return TRUE si les empreintes divergent, FALSE sinon, NA si indeterminable.
#' @export
da_design_result_is_stale <- function(da_design_result, seurat_obj) {
  if (is.null(da_design_result)) return(NA)
  fp <- da_design_result$object_identity$fingerprint %||% NULL
  if (is.null(fp) || is.null(seurat_obj)) return(NA)
  !identical(fp, .da_design_object_fingerprint(seurat_obj))
}

#' Verifier que l'objet est un resultat design DA canonique (et eligible)
#'
#' Garde de contrat pour TOUT consommateur (vues Stage 16, Milo 4E-1, scCODA
#' 4E-2, rapport 4F). Avec method = "milo"/"sccoda", refuse un design non
#' eligible pour la methode demandee — un design bloque n'est JAMAIS
#' consommable.
#'
#' @param da_design_result Objet a verifier (resultat canonique attendu).
#' @param method Methode consommatrice : "any" (contrat seul), "milo" ou
#'   "sccoda" (eligibility exige).
#' @param seurat_obj Objet Seurat courant optionnel (peremption).
#' @param context Contexte cite dans les messages d'erreur.
#' @return Le resultat, invisible (pipable, conforme au style assert_*).
#' @export
assert_da_design_result <- function(da_design_result,
                                    method = c("any", "milo", "sccoda"),
                                    seurat_obj = NULL,
                                    context = "design DA") {
  method <- match.arg(method)
  if (!is.list(da_design_result) ||
      !identical(da_design_result$type %||% NULL, "da_design") ||
      is.null(da_design_result$status)) {
    .da_design_stop(
      "invalid_input",
      sprintf(
        paste0("Echec %s : resultat design DA canonique requis ",
               "(finalize_da_design_result()) — recu : %s."),
        context,
        if (is.null(da_design_result)) "NULL"
        else paste(class(da_design_result), collapse = "/")
      )
    )
  }
  if (!da_design_result$status %in% da_design_validity_states()) {
    .da_design_stop(
      "invalid_input",
      sprintf("Echec %s : statut design DA inconnu '%s'.",
              context, da_design_result$status)
    )
  }
  if (!is.null(seurat_obj) &&
      isTRUE(da_design_result_is_stale(da_design_result, seurat_obj))) {
    .da_design_stop(
      "stale_against_current_seurat_object",
      sprintf("Echec %s : %s", context,
              da_design_status_labels()[["stale_against_current_seurat_object"]])
    )
  }
  if (method != "any") {
    elig <- da_design_result[[paste0(method, "_eligibility")]] %||% NULL
    if (is.null(elig) || !isTRUE(elig$eligible)) {
      .da_design_stop(
        "invalid_design",
        sprintf(
          paste0("Echec %s : design non eligible pour %s — %s"),
          context, method,
          paste(elig$blockers %||% "raison inconnue", collapse = " | ")
        )
      )
    }
  }
  invisible(da_design_result)
}

#' Resume de validation du design DA pour export CSV (Stage 13)
#'
#' Une ligne, colonnes stables : analyse, statut, colonnes de design, compteurs
#' (conditions/echantillons/cellules), replicats minimaux, eligibility
#' Milo/scCODA, bloqueurs, empreinte objet, avertissements. Lecture directe de
#' l'objet canonique — aucune deduction.
#'
#' @param da_design_result Resultat canonique.
#' @return data.frame a une ligne, colonnes character.
#' @export
build_da_design_summary <- function(da_design_result) {
  r <- assert_da_design_result(da_design_result, context = "resume design DA")
  p <- r$provenance %||% list()
  fp <- r$object_identity$fingerprint %||% NA_character_

  data.frame(
    analysis_id = r$analysis_id %||% NA_character_,
    analysis_type = r$type %||% "da_design",
    status = r$status %||% NA_character_,
    timestamp_utc = r$timestamp_utc %||% NA_character_,
    sample_id_column = r$config$sample_id %||% NA_character_,
    condition_column = r$config$condition %||% NA_character_,
    replicate_id_column = r$config$replicate_id %||% NA_character_,
    replicate_equals_sample = as.character(isTRUE(r$config$replicate_equals_sample)),
    batch_column = r$config$batch %||% NA_character_,
    identity_column = r$config$identity %||% NA_character_,
    composition_unit = r$composition_unit %||% "sample",
    n_samples = as.character(p$parameters$n_samples %||% NA_integer_),
    n_conditions = as.character(p$parameters$n_conditions %||% NA_integer_),
    n_cells = as.character(p$parameters$n_cells %||% NA_integer_),
    min_replicates_required = as.character(
      p$parameters$min_replicates_required %||% TS_DA_MIN_REPLICATES_PER_CONDITION
    ),
    milo_eligible = as.character(isTRUE(r$milo_eligibility$eligible)),
    sccoda_eligible = as.character(isTRUE(r$sccoda_eligibility$eligible)),
    blockers = paste(r$milo_eligibility$blockers %||% character(0),
                     collapse = " | "),
    object_fingerprint = fp,
    warnings = paste(r$warnings %||% character(0), collapse = " | "),
    stringsAsFactors = FALSE
  )
}

#' Export CSV des echantillons du design DA (Stage 13)
#'
#' Une ligne par echantillon : condition, replicat, batch, effectifs,
#' nombre d'identites, trace par analysis_id. Produit a la validation —
#' jamais reconstruit.
#'
#' @param da_design_result Resultat canonique.
#' @return data.frame (n_samples lignes).
#' @export
build_da_design_sample_export <- function(da_design_result) {
  r <- assert_da_design_result(da_design_result, context = "export echantillons design DA")
  ss <- r$sample_summary %||% NULL
  if (is.null(ss) || !is.data.frame(ss)) {
    .da_design_stop(
      "invalid_input",
      "Aucune table d'echantillons disponible dans le resultat canonique."
    )
  }
  data.frame(
    ss,
    analysis_id = rep(r$analysis_id %||% NA_character_, nrow(ss)),
    timestamp_utc = rep(r$timestamp_utc %||% NA_character_, nrow(ss)),
    stringsAsFactors = FALSE
  )
}

#' Nom de fichier d'export trace par l'identifiant d'analyse (Stage 13)
#'
#' @param da_design_result Resultat canonique.
#' @param kind Prefixe descriptif (ex. "da_design_summary").
#' @param ext Extension ("csv", "rds").
#' @return Chaine "<kind>_<analysis_id>_<date>.<ext>".
#' @export
da_design_export_filename <- function(da_design_result, kind, ext) {
  if (is.null(da_design_result) || !is.list(da_design_result)) {
    .da_design_stop(
      "invalid_input",
      "da_design_export_filename() : resultat design DA canonique requis."
    )
  }
  aid <- da_design_result$analysis_id %||% "sc-da-design"
  sprintf(
    "%s_%s_%s.%s",
    as.character(kind)[1L],
    aid,
    format(Sys.Date(), "%Y-%m-%d"),
    as.character(ext)[1L]
  )
}

#' Recapitulatif du design DA consomme par une methode (Stage 15, partage)
#'
#' Structure de trace commune des consommateurs du design (Milo 4E-1,
#' scCODA 4E-2) : colonnes choisies, unite de composition, empreinte du
#' design, compteurs et avertissements. Lecture directe du resultat canonique
#' — aucune deduction. Ajoutee en Stage 15 pour de-dupliquer les recapitulatifs
#' precedemment copies dans chaque methode (evolution d'API additive :
#' code + test de freeze + contrat documente simultanement).
#'
#' @param da_design_result Resultat canonique (finalize_da_design_result()).
#' @return list(design_analysis_id, design_status, design_timestamp_utc,
#'   colonnes, replicate_equals_sample, composition_unit, design_fingerprint,
#'   n_samples, n_conditions, n_cells, design_warnings).
#' @export
da_design_recap <- function(da_design_result) {
  if (is.null(da_design_result) || !is.list(da_design_result) ||
      !identical(da_design_result$type %||% NULL, "da_design")) {
    .da_design_stop(
      "invalid_input",
      "da_design_recap() : resultat design DA canonique requis."
    )
  }
  cfg <- da_design_result$config %||% list()
  p <- da_design_result$provenance$parameters %||% list()
  list(
    design_analysis_id = da_design_result$analysis_id %||% NA_character_,
    design_status = da_design_result$status %||% NA_character_,
    design_timestamp_utc = da_design_result$timestamp_utc %||% NA_character_,
    sample_id_column = cfg$sample_id %||% NA_character_,
    condition_column = cfg$condition %||% NA_character_,
    replicate_id_column = cfg$replicate_id %||% NA_character_,
    replicate_equals_sample = isTRUE(cfg$replicate_equals_sample),
    batch_column = cfg$batch %||% NA_character_,
    identity_column = cfg$identity %||% NA_character_,
    composition_unit = da_design_result$composition_unit %||% "sample",
    design_fingerprint = da_design_result$object_identity$fingerprint %||%
      NA_character_,
    n_samples = p$n_samples %||% NA_integer_,
    n_conditions = p$n_conditions %||% NA_integer_,
    n_cells = p$n_cells %||% NA_integer_,
    design_warnings = as.character(da_design_result$warnings %||% character(0))
  )
}

#' Surface publique figee de R/sc/sc_abundance_design.R (Stage 13)
#'
#' Le test de freeze refuse toute fonction top-level non prefixee d'un point
#' qui ne figure pas dans cette liste.
#'
#' @return Vecteur character des noms de fonctions publiques.
#' @export
da_design_public_api <- function() {
  c(
    "da_design_contract_fields", "da_design_validity_states",
    "da_design_status_labels", "da_design_status_is_valid",
    "da_design_error_state", "assert_da_design_result", "da_design_public_api",
    "validate_da_design", "finalize_da_design_result",
    "da_design_result_is_stale",
    "build_da_design_summary", "build_da_design_sample_export",
    "da_design_export_filename", "da_design_recap"
  )
}

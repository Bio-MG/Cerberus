# =============================================================================
# R/sc/sc_population_rarity.R — Rarete par population annotee (CCC 9, question 1)
# =============================================================================
# But : repondre a la question DESCRIPTIVE et MONO-CONDITION « quelles
# populations annotees sont rares ? » — c'est-a-dire compter les cellules par
# niveau d'une colonne d'identite DECLAREE et qualifier de « rare » celles qui
# passent sous un seuil DECLARE.
#
# ── CHAMP D'APPLICATION : LA PORTE STAGE 13 NE S'APPLIQUE PAS ───────────────
# Le resultat de ce fichier ne porte AUCUNE affirmation differentielle :
#   - aucun test, aucune p-value, aucune comparaison entre conditions ;
#   - un simple decompte de la composition d'UN jeu de donnees.
# La porte `assert_da_design_result` (Stage 13) garde les AFFIRMATIONS
# DIFFERENTIELLES (elle bloque la pseudoreplication, c'est-a-dire l'usage de
# cellules comme replicats dans un TEST). Elle est donc INAPPLICABLE ici, et
# ce motif est ECRIT dans docs/contracts/POPULATION_RARITY_CONTRACT.md — jamais
# laisse implicite. Voir aussi `descriptive_only = TRUE` dans la provenance.
#
# COROLLAIRE CONTRAIGNANT : toute evolution qui introduirait une comparaison
# ENTRE CONDITIONS (« cette population est-elle plus rare en A qu'en B ? »)
# redeviendrait un test d'abondance differentielle et devrait repasser par la
# porte Stage 13 / Milo / scCODA. C'est explicitement HORS PERIMETRE.
#
# ── REGLE 3 : AUCUN MOTEUR DUPLIQUE ─────────────────────────────────────────
# La question ne requiert AUCUN graphe kNN. Compter les cellules par niveau
# d'une colonne de metadonnees est une primitive de base R (`table()` /
# `tabulate()`), pas un moteur :
#   - aucune primitive miloR n'est necessaire (ni graphe, ni voisinage) ;
#   - aucun algorithme n'est reimplemente.
# La regle 3 est donc satisfaite TRIVIALEMENT.
# En particulier, la table d'identites du DESIGN DA
# (R/sc/sc_abundance_design.R) n'est PAS reutilisee : elle est de portee
# « design » et gardee par le Stage 13, et l'utiliser importerait la porte dans
# une question descriptive. On ne la reecrit pas non plus (ce serait dupliquer).
#
# ── AUCUN SEUIL IMPLICITE ───────────────────────────────────────────────────
# Le seuil de rarete est un CHOIX DECLARE obligatoire : `rule_type` ET
# `threshold` sont requis, sans valeur par defaut (meme discipline que le mode
# d'agregation de CCC 7-8). `config/defaults.R` ne fournit que les REGLES
# AUTORISEES et un PLANCHER DE GARDE — jamais un seuil de rarete.
# ⚠️ Ne JAMAIS reutiliser TS_DA_MIN_IDENTITY_CELLS_PER_SAMPLE comme defaut :
# ce serait un defaut implicite ET une confusion semantique entre « plancher de
# testabilite DA » et « seuil de rarete ».
#
# ── ETIQUETAGE ──────────────────────────────────────────────────────────────
# Toute figure et tout export est DESCRIPTIF, jamais causal. `fraction` est une
# part DU JEU ANALYSE, jamais une abondance tissulaire.
#
# Pur domaine : aucune reactivite Shiny. Sourced in app.R AFTER
# R/sc/sc_velocity.R (reutilise velocity_object_fingerprint) and AFTER
# R/core/provenance.R, BEFORE modules/sc/*.R.
# Constantes TS_POPULATION_RARITY_* definies dans config/defaults.R.
#
# ── FLUX ────────────────────────────────────────────────────────────────────
# metadonnees (meta.data) + colonne d'identite DECLAREE + regle DECLAREE
#   -> validation stricte (colonnes, regle, seuil, plancher)
#   -> decompte par population (base R, O(n_cells))
#   -> qualification « rare » selon la regle declaree
#   -> resultat canonique + provenance PRODUITE ici (regle 7).
#
# ── CONTRAT DE RESULTAT CANONIQUE ───────────────────────────────────────────
# Assemble par compute_population_rarity() (champs
# population_rarity_contract_fields()) :
#
#   type                  "sc_population_rarity" (constant)
#   status                etat de validite (population_rarity_validity_states())
#   population_table      data.frame UNE LIGNE PAR POPULATION : population,
#                         n_cells, fraction, is_rare (+ n_samples_present si
#                         une colonne d'echantillon est declaree)
#   rarity_rule           list(rule_type, threshold, direction = "below")
#   summary               list(n_populations, n_rare, n_cells_total,
#                         n_cells_counted, identity_column, declared_rule_label)
#   identity_column       colonne d'identite declaree
#   identity_summary      list(n_levels, n_levels_empty, n_labels_na,
#                         n_cells_counted)
#   parameters            parametres DECLARES (identity_column, rule_type,
#                         threshold, sample_column) — reproductibilite
#   qc                    list(n_cells_total, n_cells_counted, n_labels_na,
#                         n_levels_empty, empty_levels)
#   object_identity       list(fingerprint, method, seurat_dims) — empreinte v2
#                         REUTILISEE (velocity_object_fingerprint)
#   warnings              vecteur character (qualite de donnees uniquement)
#   provenance            entree new_provenance_entry() enrichie
#                         (analysis_type = "population_rarity",
#                          descriptive_only = TRUE)
#   analysis_id           "sc-population-rarity"
#   timestamp_utc         horodatage UTC ISO-8601
#
# ── ETATS DE VALIDITE ───────────────────────────────────────────────────────
#   valid                          — resultat produit
#   valid_with_warnings            — resultat produit + avertissements de QC
#   unavailable_single_population  — une seule population : AUCUNE rarete
#                                    calculable (is_rare = NA) ; la table est
#                                    produite, ce n'est PAS une erreur
#   invalid_input                  — erreur structurelle (stop() classe,
#                                    AUCUN resultat)
#
# ── API PUBLIQUE FIGEE ──────────────────────────────────────────────────────
# population_rarity_public_api() ; test de freeze
# tests/testthat/test-sc-population-rarity-contract-freeze.R.
# Helpers internes prefixes d'un point (.population_rarity_*).
# =============================================================================

# Etats de validite (source de verite ; l'accesseur public est l'unique lecture
# autorisee ailleurs).
.POPULATION_RARITY_STATUS_STATES <- c(
  "valid",
  "valid_with_warnings",
  "unavailable_single_population",
  "invalid_input"
)

# Identifiant d'analyse (constant, repris dans les noms de fichiers d'export).
.POPULATION_RARITY_ANALYSIS_ID <- "sc-population-rarity"

#' Champs documentes du contrat de resultat de rarete par population
#'
#' Source de verite partagee par le code, le test de freeze et
#' docs/contracts/POPULATION_RARITY_CONTRACT.md — toute modification passe par
#' les TROIS simultanement.
#'
#' @return Vecteur character des champs contractuels.
#' @export
population_rarity_contract_fields <- function() {
  c(
    "type", "status", "population_table", "rarity_rule", "summary",
    "identity_column", "identity_summary", "parameters", "qc",
    "object_identity", "warnings", "provenance", "analysis_id",
    "timestamp_utc"
  )
}

#' Etats de validite du contrat de rarete par population
#'
#' @return Vecteur character des 4 etats documentes.
#' @export
population_rarity_validity_states <- function() {
  .POPULATION_RARITY_STATUS_STATES
}

#' Libelles francais des etats de validite
#'
#' @return Vecteur character nomme (noms = etats).
#' @export
population_rarity_status_labels <- function() {
  c(
    valid = "resultat produit",
    valid_with_warnings = "resultat produit avec avertissements",
    unavailable_single_population = "une seule population : aucune rarete calculable",
    invalid_input = "entree invalide (aucun resultat)"
  )
}

#' Regles de rarete autorisees (surface de contrat figee)
#'
#' Le seuil est un CHOIX DECLARE : `rule_type` doit valoir l'une de ces deux
#' valeurs, et la valeur numerique est fournie par l'appelant. Aucune valeur par
#' defaut n'existe nulle part (ni ici, ni dans config/).
#'
#' @return Vecteur character des regles autorisees.
#' @export
population_rarity_rule_types <- function() {
  c("absolute_n_cells", "relative_fraction")
}

# Arret classe (meme idiome que .milo_stop) : toute erreur porte un `state`
# structuré et la classe "population_rarity_error".
.population_rarity_stop <- function(state, message) {
  stop(errorCondition(
    message,
    state = state,
    class = "population_rarity_error"
  ))
}

#' Etat de validite associe a une erreur de rarete
#'
#' @param e Condition (erreur) capturee.
#' @return Chaine d'etat structuree, NA_character_ pour une erreur sans etat.
#' @export
# §14.1 : délégation à l'accesseur générique. Famille F1 MESURÉE — la CLASSE
# est vérifiée : l'état d'une erreur d'un autre domaine rend NA.
population_rarity_error_state <- function(e) ts_error_state(e, "population_rarity_error")

# Champ vide ? (NA, "" ou whitespace) — VECTORISE (meme longueur que x).
# Utilise sur des colonnes entieres (libelles, echantillons).
.population_rarity_is_blank <- function(x) {
  x <- as.character(x)
  if (length(x) == 0L) return(logical(0))
  is.na(x) | trimws(x) == ""
}

# Parametre scalaire invalide ? (NULL, longueur != 1, NA ou vide)
# Utilise pour les parametres DECLARES : evite le `if (logical(0))` qui
# produirait une erreur NON classee au lieu du message metier.
.population_rarity_bad_scalar <- function(x) {
  is.null(x) || length(x) != 1L || is.na(x) || trimws(as.character(x)) == ""
}

# Plancher de garde : en dessous, toute rarete serait illusoire -> refus.
# Lu depuis config/ (jamais code en dur), avec repli sur la valeur declaree
# pour un contexte ou config/ n'aurait pas ete source.
.population_rarity_floor <- function() {
  if (exists("TS_POPULATION_RARITY_MIN_CELLS_TOTAL", inherits = TRUE)) {
    as.integer(TS_POPULATION_RARITY_MIN_CELLS_TOTAL)
  } else {
    50L
  }
}

# Etiquette FR de la regle declaree, telle qu'elle sera affichee et exportee.
.population_rarity_rule_label <- function(rule_type, threshold) {
  if (identical(rule_type, "absolute_n_cells")) {
    sprintf("n_cells < %d", as.integer(threshold))
  } else {
    sprintf("fraction < %s", format(threshold, scientific = FALSE))
  }
}

# Verification du seuil selon la regle declaree. Renvoie le seuil normalise
# (integer pour la regle absolue, numeric pour la regle relative).
.population_rarity_check_threshold <- function(rule_type, threshold) {
  if (.population_rarity_bad_scalar(threshold)) {
    .population_rarity_stop(
      "invalid_input",
      paste0("Echec rarete par population : 'threshold' est requis (aucun ",
             "seuil par defaut n'existe — le seuil est un choix declare).")
    )
  }
  num <- suppressWarnings(as.numeric(threshold))
  if (is.na(num) || !is.finite(num)) {
    .population_rarity_stop(
      "invalid_input",
      sprintf("Echec rarete par population : 'threshold' doit etre numerique (recu : %s).",
              as.character(threshold)[1L])
    )
  }
  if (identical(rule_type, "absolute_n_cells")) {
    if (num < 1 || num != floor(num)) {
      .population_rarity_stop(
        "invalid_input",
        sprintf(paste0("Echec rarete par population : seuil absolu invalide ",
                       "(%s) — attendu un entier >= 1 (nombre de cellules)."),
                as.character(threshold)[1L])
      )
    }
    return(as.integer(num))
  }
  if (num <= 0 || num >= 1) {
    .population_rarity_stop(
      "invalid_input",
      sprintf(paste0("Echec rarete par population : seuil relatif invalide ",
                     "(%s) — attendu une fraction strictement dans ]0, 1[."),
              as.character(threshold)[1L])
    )
  }
  num
}

#' Rarete par population annotee — calcul descriptif (CCC 9, question 1)
#'
#' Compte les cellules par niveau d'une colonne d'identite DECLAREE et qualifie
#' de « rare » les populations sous un seuil DECLARE. Aucune comparaison entre
#' conditions, aucun test : la porte Stage 13 ne s'applique pas (voir l'en-tete
#' du fichier et le contrat).
#'
#' @param meta data.frame de metadonnees (typiquement `obj@meta.data`).
#' @param identity_column Nom de la colonne d'identite (declare, obligatoire).
#' @param rule_type "absolute_n_cells" ou "relative_fraction" (declare,
#'   obligatoire, aucun defaut).
#' @param threshold Seuil declare : entier >= 1 (regle absolue) ou fraction
#'   dans ]0, 1[ (regle relative). Aucun defaut.
#' @param sample_column Nom optionnel d'une colonne d'echantillon : ajoute le
#'   nombre d'echantillons ou la population est presente (DESCRIPTIF — jamais un
#'   test par echantillon).
#' @param seurat_obj Objet Seurat optionnel : sert uniquement a l'empreinte v2
#'   (`velocity_object_fingerprint`), jamais au comptage.
#' @param timestamp POSIXct, horodatage de PRODUCTION (tests deterministes).
#' @return Liste canonique (population_rarity_contract_fields()).
#' @export
compute_population_rarity <- function(meta,
                                      identity_column,
                                      rule_type,
                                      threshold,
                                      sample_column = NULL,
                                      seurat_obj = NULL,
                                      timestamp = Sys.time()) {
  if (is.null(meta) || !is.data.frame(meta)) {
    .population_rarity_stop(
      "invalid_input",
      sprintf(paste0("Echec rarete par population : 'meta' doit etre un ",
                     "data.frame de metadonnees (recu : %s)."),
              if (is.null(meta)) "NULL" else paste(class(meta), collapse = "/"))
    )
  }
  if (.population_rarity_bad_scalar(identity_column)) {
    .population_rarity_stop(
      "invalid_input",
      "Echec rarete par population : 'identity_column' est requis (colonne d'identite declaree)."
    )
  }
  identity_column <- as.character(identity_column)[1L]
  if (!identity_column %in% names(meta)) {
    .population_rarity_stop(
      "invalid_input",
      sprintf(paste0("Echec rarete par population : colonne d'identite '%s' ",
                     "absente des metadonnees. Colonnes disponibles : %s."),
              identity_column,
              paste(utils::head(names(meta), 20L), collapse = ", "))
    )
  }

  if (.population_rarity_bad_scalar(rule_type)) {
    .population_rarity_stop(
      "invalid_input",
      paste0("Echec rarete par population : 'rule_type' est requis (aucune ",
             "regle par defaut n'existe — le seuil est un choix declare).")
    )
  }
  rule_type <- as.character(rule_type)[1L]
  if (!rule_type %in% population_rarity_rule_types()) {
    .population_rarity_stop(
      "invalid_input",
      sprintf(paste0("Echec rarete par population : regle inconnue '%s' ",
                     "(autorisees : %s)."),
              rule_type, paste(population_rarity_rule_types(), collapse = ", "))
    )
  }
  threshold <- .population_rarity_check_threshold(rule_type, threshold)

  sample_column <- if (is.null(sample_column) ||
                       .population_rarity_bad_scalar(sample_column)) {
    NULL
  } else {
    as.character(sample_column)[1L]
  }
  if (!is.null(sample_column) && !sample_column %in% names(meta)) {
    .population_rarity_stop(
      "invalid_input",
      sprintf(paste0("Echec rarete par population : colonne d'echantillon ",
                     "'%s' absente des metadonnees."),
              sample_column)
    )
  }

  labels <- as.character(meta[[identity_column]])
  n_cells_total <- length(labels)
  floor_n <- .population_rarity_floor()
  if (n_cells_total < floor_n) {
    .population_rarity_stop(
      "invalid_input",
      sprintf(paste0("Echec rarete par population : jeu trop petit (%d ",
                     "cellule(s)) — plancher declare TS_POPULATION_RARITY_MIN_CELLS_TOTAL = %d."),
              n_cells_total, floor_n)
    )
  }

  is_blank <- .population_rarity_is_blank(labels)
  n_labels_na <- sum(is_blank)
  kept <- labels[!is_blank]

  empty_levels <- if (is.factor(meta[[identity_column]])) {
    setdiff(levels(meta[[identity_column]]), unique(kept))
  } else {
    character(0)
  }
  n_levels_empty <- length(empty_levels)

  populations <- sort(unique(kept))
  n_populations <- length(populations)
  if (n_populations < 1L) {
    .population_rarity_stop(
      "invalid_input",
      sprintf(paste0("Echec rarete par population : aucune population ",
                     "exploitable dans '%s' (%d cellule(s) sans etiquette)."),
              identity_column, n_labels_na)
    )
  }
  counts <- tabulate(match(kept, populations), nbins = n_populations)
  n_cells_counted <- sum(counts)

  tab <- data.frame(
    population = populations,
    n_cells = as.integer(counts),
    stringsAsFactors = FALSE
  )
  # La fraction est une part DU JEU COMPTE (jamais une abondance tissulaire).
  tab$fraction <- if (n_cells_counted > 0L) tab$n_cells / n_cells_counted else NA_real_

  if (!is.null(sample_column)) {
    sample_kept <- as.character(meta[[sample_column]])[!is_blank]
    tab$n_samples_present <- vapply(
      populations,
      function(p) {
        vals <- sample_kept[kept == p]
        length(unique(vals[!.population_rarity_is_blank(vals)]))
      },
      integer(1L)
    )
  }

  # « Rare » est COMPARATIF : une seule population ne peut etre qualifiee.
  if (n_populations < 2L) {
    tab$is_rare <- rep(NA, n_populations)
    status <- "unavailable_single_population"
  } else {
    tab$is_rare <- if (identical(rule_type, "absolute_n_cells")) {
      tab$n_cells < threshold
    } else {
      tab$fraction < threshold
    }
    status <- "valid"
  }

  warnings <- character(0)
  if (n_labels_na > 0L) {
    warnings <- c(warnings, sprintf(
      "%d cellule(s) sans etiquette d'identite : exclues du decompte et comptabilisees (aucune imputation).",
      n_labels_na
    ))
  }
  if (n_levels_empty > 0L) {
    warnings <- c(warnings, sprintf(
      "%d niveau(x) de facteur sans cellule : exclus et comptabilises (%s).",
      n_levels_empty, paste(utils::head(empty_levels, 5L), collapse = ", ")
    ))
  }
  if (identical(status, "valid") && length(warnings) > 0L) {
    status <- "valid_with_warnings"
  }

  n_rare <- if (all(is.na(tab$is_rare))) NA_integer_ else sum(tab$is_rare)

  object_identity <- list(
    fingerprint = if (is.null(seurat_obj)) NA_character_ else
      velocity_object_fingerprint(seurat_obj),
    method = "velocity_object_fingerprint",
    seurat_dims = if (is.null(seurat_obj)) NULL else dim(seurat_obj)
  )

  declared_rule_label <- .population_rarity_rule_label(rule_type, threshold)

  provenance <- new_provenance_entry(
    analysis_id = .POPULATION_RARITY_ANALYSIS_ID,
    method = "descriptive_count",
    parameters = list(
      identity_column = identity_column,
      rule_type = rule_type,
      threshold = threshold,
      sample_column = sample_column %||% NA_character_,
      declared_rule_label = declared_rule_label,
      descriptive_only = TRUE
    ),
    dataset = seurat_obj,
    cells_used = n_cells_counted,
    cells_excluded = n_labels_na,
    warnings = warnings,
    timestamp = timestamp
  )
  provenance$analysis_type <- "population_rarity"
  # Marqueur explicite : la sortie ne porte AUCUNE affirmation differentielle
  # (rend le motif du champ d'application traçable dans le resultat lui-meme).
  provenance$descriptive_only <- TRUE
  provenance$rarity_rule <- rule_type
  provenance$rarity_threshold <- threshold
  provenance$n_populations <- n_populations
  provenance$n_rare <- n_rare
  provenance$n_cells_total <- n_cells_total
  provenance$object_identity <- object_identity

  list(
    type = "sc_population_rarity",
    status = status,
    population_table = tab,
    rarity_rule = list(
      rule_type = rule_type,
      threshold = threshold,
      direction = "below"
    ),
    summary = list(
      n_populations = n_populations,
      n_rare = n_rare,
      n_cells_total = n_cells_total,
      n_cells_counted = n_cells_counted,
      identity_column = identity_column,
      declared_rule_label = declared_rule_label
    ),
    identity_column = identity_column,
    identity_summary = list(
      n_levels = n_populations,
      n_levels_empty = n_levels_empty,
      n_labels_na = n_labels_na,
      n_cells_counted = n_cells_counted
    ),
    parameters = list(
      identity_column = identity_column,
      rule_type = rule_type,
      threshold = threshold,
      sample_column = sample_column
    ),
    qc = list(
      n_cells_total = n_cells_total,
      n_cells_counted = n_cells_counted,
      n_labels_na = n_labels_na,
      n_levels_empty = n_levels_empty,
      empty_levels = empty_levels
    ),
    object_identity = object_identity,
    warnings = warnings,
    provenance = provenance,
    analysis_id = .POPULATION_RARITY_ANALYSIS_ID,
    timestamp_utc = format(as.POSIXct(timestamp),
                           format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

#' Le resultat de rarete est-il perime vis-a-vis de l'objet Seurat courant ?
#'
#' @param rarity_result Resultat canonique (compute_population_rarity()).
#' @param seurat_obj Objet Seurat courant.
#' @return TRUE si les empreintes divergent, FALSE sinon, NA si l'identite du
#'   resultat ou de l'objet est indeterminable.
#' @export
population_rarity_is_stale <- function(rarity_result, seurat_obj) {
  if (is.null(rarity_result)) return(NA)
  fp <- rarity_result$object_identity$fingerprint %||% NULL
  if (is.null(fp) || length(fp) != 1L || is.na(fp) || is.null(seurat_obj)) {
    return(NA)
  }
  !identical(fp, velocity_object_fingerprint(seurat_obj))
}

#' Verifier un resultat de rarete canonique (style assert_*)
#'
#' @param rarity_result Objet a verifier (resultat canonique attendu).
#' @param context Contexte cite dans les messages d'erreur.
#' @return Le resultat, invisible (pipable, conforme au style assert_*).
#' @export
assert_population_rarity_result <- function(rarity_result,
                                            context = "resultat de rarete par population") {
  if (!is.list(rarity_result) ||
      !identical(rarity_result$type %||% NULL, "sc_population_rarity") ||
      is.null(rarity_result$status)) {
    .population_rarity_stop(
      "invalid_input",
      sprintf(
        paste0("Echec %s : resultat canonique requis ",
               "(compute_population_rarity()) — recu : %s."),
        context,
        if (is.null(rarity_result)) "NULL"
        else paste(class(rarity_result), collapse = "/")
      )
    )
  }
  if (!rarity_result$status %in% population_rarity_validity_states()) {
    .population_rarity_stop(
      "invalid_input",
      sprintf("Echec %s : statut inconnu '%s'.", context, rarity_result$status)
    )
  }
  if (identical(rarity_result$status, "invalid_input")) {
    .population_rarity_stop(
      "invalid_input",
      sprintf("Echec %s : %s", context,
              population_rarity_status_labels()[["invalid_input"]])
    )
  }
  if (!is.null(rarity_result$population_table) &&
      !is.data.frame(rarity_result$population_table)) {
    .population_rarity_stop(
      "invalid_input",
      sprintf("Echec %s : population_table doit etre un data.frame.", context)
    )
  }
  invisible(rarity_result)
}

#' Resume du resultat de rarete pour export CSV
#'
#' Une ligne, colonnes stables : identifiants, statut, regle declaree,
#' compteurs, empreinte, avertissements. Lecture directe de l'objet canonique —
#' aucune deduction.
#'
#' @param rarity_result Resultat canonique.
#' @return data.frame a une ligne, colonnes character.
#' @export
build_population_rarity_summary <- function(rarity_result) {
  r <- assert_population_rarity_result(rarity_result, context = "resume de rarete")
  data.frame(
    analysis_id = r$analysis_id %||% NA_character_,
    analysis_type = r$type %||% "sc_population_rarity",
    status = r$status %||% NA_character_,
    timestamp_utc = r$timestamp_utc %||% NA_character_,
    identity_column = r$identity_column %||% NA_character_,
    rule_type = r$rarity_rule$rule_type %||% NA_character_,
    threshold = as.character(r$rarity_rule$threshold %||% NA_real_),
    declared_rule_label = r$summary$declared_rule_label %||% NA_character_,
    direction = r$rarity_rule$direction %||% NA_character_,
    n_populations = as.character(r$summary$n_populations %||% NA_integer_),
    n_rare = as.character(r$summary$n_rare %||% NA_integer_),
    n_cells_total = as.character(r$qc$n_cells_total %||% NA_integer_),
    n_cells_counted = as.character(r$qc$n_cells_counted %||% NA_integer_),
    n_labels_na = as.character(r$qc$n_labels_na %||% NA_integer_),
    n_levels_empty = as.character(r$qc$n_levels_empty %||% NA_integer_),
    descriptive_only = as.character(isTRUE(r$provenance$descriptive_only)),
    object_fingerprint = r$object_identity$fingerprint %||% NA_character_,
    warnings = paste(r$warnings %||% character(0), collapse = " | "),
    stringsAsFactors = FALSE
  )
}

#' Table de rarete pour export CSV (une ligne par population)
#'
#' @param rarity_result Resultat canonique.
#' @return data.frame : population, n_cells, fraction, is_rare
#'   (+ n_samples_present si declaree).
#' @export
build_population_rarity_table_export <- function(rarity_result) {
  r <- assert_population_rarity_result(rarity_result, context = "export de rarete")
  tab <- r$population_table
  if (is.null(tab) || !is.data.frame(tab)) {
    .population_rarity_stop(
      "invalid_input",
      "Echec export de rarete : population_table absente ou invalide."
    )
  }
  tab
}

#' Nom de fichier d'export (convention <kind>_<analysis_id>_<date>.<ext>)
#'
#' @param rarity_result Resultat canonique.
#' @param kind Prefixe de type (ex. "population_rarity").
#' @param ext Extension ("csv" par defaut).
#' @return Chaine character.
#' @export
population_rarity_export_filename <- function(rarity_result,
                                              kind = "population_rarity",
                                              ext = "csv") {
  if (is.null(rarity_result) || !is.list(rarity_result)) {
    .population_rarity_stop(
      "invalid_input",
      "population_rarity_export_filename() : resultat de rarete canonique requis."
    )
  }
  aid <- rarity_result$analysis_id %||% .POPULATION_RARITY_ANALYSIS_ID
  sprintf(
    "%s_%s_%s.%s",
    as.character(kind)[1L],
    aid,
    format(Sys.Date(), "%Y-%m-%d"),
    as.character(ext)[1L]
  )
}

#' Surface publique figee de R/sc/sc_population_rarity.R
#'
#' Le test de freeze refuse toute fonction top-level non prefixee d'un point
#' qui ne figure pas dans cette liste.
#'
#' @return Vecteur character des noms de fonctions publiques.
#' @export
population_rarity_public_api <- function() {
  c(
    "population_rarity_contract_fields",
    "population_rarity_validity_states",
    "population_rarity_status_labels",
    "population_rarity_rule_types",
    "population_rarity_error_state",
    "compute_population_rarity",
    "population_rarity_is_stale",
    "assert_population_rarity_result",
    "build_population_rarity_summary",
    "build_population_rarity_table_export",
    "population_rarity_export_filename",
    "population_rarity_public_api"
  )
}

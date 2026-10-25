# =============================================================================
# R/sc/sc_communication.R — Communication cellule-cellule 4D-1 : import et
# validation uniquement (Stage 11)
# =============================================================================
# Pur domaine : aucune reactivite Shiny, aucune dependance Seurat au chargement
# (l'harmonisation des identites consomme un vecteur d'identites extrait par le
# module ; l'identite d'objet n'a besoin que de dimnames). Sourced in app.R
# AFTER R/sc/sc_velocity.R (velocity_object_fingerprint est reutilise, jamais
# duplique), BEFORE modules/sc/*.R.
#
# ── CHAMP D'APPLICATION (Stage 11) ──────────────────────────────────────────
# Ce fichier n'effectue AUCUN calcul CellChat/CellPhoneDB : il importe des
# tables de resultats DEJA produites hors de l'application, les convertit vers
# une table canonique unique, harmonise les identites sender/receiver avec UNE
# colonne de metadonnees Seurat explicite, applique un QC d'import et produit
# la provenance. Reseau, chord/circle plots, inference de pathways : hors
# perimetre (Stage 12).
#
# ── CONTRAT DE RESULTAT CANONIQUE (Stage 11) ────────────────────────────────
# Assemble par finalize_communication_result() a partir de la table canonique
# harmonisee et QCee. Champs du contrat :
#
#   type                  "cell_cell_communication" (constant)
#   status                etat de validite technique, voir
#                         communication_validity_states() / labels
#   source_method         "cellchat", "cellphonedb" ou "liana" (jamais
#                         melanges dans un meme resultat : les scores de
#                         sources differentes ne sont PAS comparables sur une
#                         echelle commune)
#   canonical_table       table canonique (12 champs contractuels + colonnes
#                         originales utiles + sender_mapped/receiver_mapped +
#                         duplicate_interaction)
#   identity_column       colonne de metadonnees Seurat choisie explicitement
#   identity_mapping      data.frame label -> identite (exact match uniquement)
#   identity_summary      liste de compteurs d'harmonisation
#   column_mapping        mapping canonical_field -> colonne source originale
#   qc                    liste de compteurs QC produits a l'import
#   input_summary         list(source_method, files, n_rows_input,
#                         n_rows_canonical)
#   object_identity       list(fingerprint, method, seurat_dims) — empreinte
#                         velocity_object_fingerprint() reutilisee (v2)
#   warnings              vecteur character (produits a l'import, jamais
#                         reconstruits apres coup)
#   provenance            entree new_provenance_entry() enrichie
#   analysis_id           "sc-communication-import"
#   timestamp_utc         horodatage UTC ISO-8601 de l'import
#
# Les 12 champs canoniques de la table (communication_contract_fields()) :
#   sender, receiver, ligand, receptor, interaction, pathway, score, p_value,
#   p_adjusted, source_method, source_file, source_cell_identity_level
#
# Champs derives ajoutés a la table (jamais confondus avec les champs
# contractuels) : sender_mapped/receiver_mapped (exact match uniquement —
# JAMAIS de renommage silencieux de populations) et duplicate_interaction.
#
# ── CHAMPS DE MESURE DE RANG (CCC 7-8 route (b), source "liana") ────────────
# Colonnes ADDITIONNELLES presentes UNIQUEMENT quand la source fournit une
# mesure ordinale — voir communication_rank_fields() :
#   rank                  valeur ordinale importee (mean_rank, {methode}.rank
#                         ou aggregate_rank selon le choix de l'utilisateur)
#   rank_direction        "lower_is_better" (dans LIANA, rang 1 = meilleur :
#                         c'est l'INVERSE de 'prob' CellChat)
#   rank_aggregation_mode "specificity" ou "magnitude" — CHOIX EXPLICITE, les
#                         deux modes ne sont pas comparables entre eux
# Elles ne font PAS partie des 12 champs contractuels : ceux-ci sont EXIGES
# de toute source, alors qu'un rang n'existe pas chez CellChat/CellPhoneDB.
# Les y ajouter forcerait ces sources a emettre des colonnes NA et modifierait
# leurs resultats (interdit par la regle 1). Un consommateur DOIT tester leur
# presence. 'score' reste NA sur cette route : un rang n'est pas un score.
#
# ── API PUBLIQUE FIGEE DES LE STAGE 11 ──────────────────────────────────────
# communication_public_api() enumere la surface publique ; le test de freeze
# (tests/testthat/test-communication-contract-freeze.R) refuse toute fonction
# top-level non prefixee d'un point hors de cette liste. Helpers internes :
#   .COMMUNICATION_STATUS_STATES — etats du contrat (via l'accesseur public)
#   .communication_stop()        — erreur classee communication_import_error
#   .communication_field_aliases — alias de colonnes par source (documente)
#   .communication_pick_column() — resolution deterministe d'un alias
#   .communication_is_blank()    — test champ requis vide
#   .communication_coerce_numeric() — coercion numerique tracee
# =============================================================================

#' Champs canoniques du contrat communication (Stage 11)
#'
#' Les 12 champs de la table canonique. Source de verite partagee par le code,
#' le test de freeze et docs/contracts/COMMUNICATION_RESULT_CONTRACT.md — toute
#' modification passe par les TROIS simultanement.
#'
#' @return Vecteur character des 12 champs contractuels.
#' @export
communication_contract_fields <- function() {
  c(
    "sender", "receiver", "ligand", "receptor", "interaction", "pathway",
    "score", "p_value", "p_adjusted",
    "source_method", "source_file", "source_cell_identity_level"
  )
}

#' Sources d'import supportees (v1)
#'
#' `"liana"` a ete ajoutee en CCC 7-8 route (b) — import de rangs produits par
#' LIANA HORS de l'application, sans aucune dependance nouvelle.
#'
#' @return Vecteur character des identifiants de sources supportees.
#' @export
communication_supported_sources <- function() {
  c("cellchat", "cellphonedb", "liana")
}

#' Champs de MESURE de rang — specifiques aux sources qui en fournissent
#'
#' Colonnes ADDITIONNELLES de la table canonique, presentes UNIQUEMENT quand la
#' source importee fournit une mesure ordinale (aujourd'hui : la route LIANA).
#' Elles ne font volontairement PAS partie de communication_contract_fields() :
#' les 12 champs contractuels sont EXIGES de toute source
#' (finalize_communication_result() echoue si l'un manque), alors qu'un rang
#' n'existe pas chez CellChat/CellPhoneDB. Les y ajouter forcerait ces deux
#' sources a emettre des colonnes NA et modifierait leurs resultats — interdit
#' par la regle 1 (zero changement de comportement sur l'existant).
#'
#' Un consommateur DOIT donc tester la presence de ces colonnes avant usage.
#'
#' @return Vecteur character des 3 champs de mesure de rang.
#' @export
communication_rank_fields <- function() {
  c("rank", "rank_direction", "rank_aggregation_mode")
}

#' Modes d'agregation LIANA supportes
#'
#' `specificity` et `magnitude` repondent a DEUX questions differentes
#' (« cette interaction est-elle specifique de ces types cellulaires ? » vs
#' « est-elle abondante ? »). Les melanger produirait un artefact d'analyse :
#' le mode est donc un CHOIX EXPLICITE de l'utilisateur, jamais un defaut
#' implicite.
#'
#' @return Vecteur character des modes acceptes.
#' @export
communication_rank_aggregation_modes <- function() {
  c("specificity", "magnitude")
}

#' Valeur du champ `rank_direction` pour la route LIANA
#'
#' Dans LIANA, **plus petit = meilleur** (rang 1 = interaction la plus
#' specifique/abondante) — l'INVERSE de `prob` (CellChat) ou de
#' `edge_specificity`. Toute comparaison doit inverser explicitement la
#' direction, jamais supposer la meme.
.COMMUNICATION_RANK_DIRECTION_LOWER <- "lower_is_better"

# Etats de validite explicites du contrat (source de verite ; ne pas
# re-enseigner ailleurs — communication_validity_states() est l'accesseur
# public).
.COMMUNICATION_STATUS_STATES <- c(
  "valid",
  "invalid_input",
  "invalid_schema",
  "invalid_identity_mapping",
  "stale_against_current_seurat_object"
)

#' Etats de validite du contrat communication
#'
#' Les etats "invalid*" sont des echecs bloquants : ils sont leves comme
#' erreurs classees (communication_import_error) par la chaine d'import —
#' aucun resultat canonique n'est produit dans ces cas.
#' "stale_against_current_seurat_object" est un etat derive au moment de
#' l'affichage (communication_result_is_stale()). La validite est TECHNIQUE
#' exclusivement : elle n'implique AUCUNE validite biologique, et un score
#' importe n'est jamais recalcule ni compare entre sources.
#'
#' @return Vecteur character des 5 etats documentes du contrat.
#' @export
communication_validity_states <- function() .COMMUNICATION_STATUS_STATES

#' Libelles francais des etats de validite (affichage utilisateur)
#'
#' @return Vecteur character nomme par etat.
#' @export
communication_status_labels <- function() {
  c(
    valid = paste0(
      "Valide (technique) : table canonique importee, identites harmonisees ",
      "sur la colonne choisie, QC termine. Validite technique uniquement — ",
      "aucune validite biologique n'est impliquee."
    ),
    invalid_input = paste0(
      "Entree invalide : fichier absent, illisible, vide ou d'un type ",
      "inattendu."
    ),
    invalid_schema = paste0(
      "Schema invalide : colonnes requises de la source absentes ou ",
      "inexploitables. Les schemas CellChat et CellPhoneDB sont traites ",
      "separement — aucune conversion supposee entre colonnes incompatibles."
    ),
    invalid_identity_mapping = paste0(
      "Harmonisation des identites impossible : aucun label sender/receiver ",
      "ne correspond a la colonne de metadonnees Seurat choisie. Choisissez ",
      "la colonne adequate ou verifiez la source."
    ),
    stale_against_current_seurat_object = paste0(
      "Perime : le resultat d'import correspond a un objet Seurat anterieur ",
      "— reimportez et revalidez la table de communication."
    )
  )
}

#' Le statut exprime-t-il un resultat techniquement exploitable ?
#'
#' @param status Chaine d'etat (result$status).
#' @return Logical.
#' @export
communication_status_is_valid <- function(status) {
  identical(status, "valid")
}

#' Erreur d'import communication avec etat structure
#'
#' Interne : la chaine d'import leve des erreurs portant la classe
#' "communication_import_error" et un champ `state` parmi
#' communication_validity_states().
.communication_stop <- function(state, message) {
  # errorCondition() est requis (stop(structure(...)) echoue : ".Data
  # is missing" dans cette version de R).
  stop(errorCondition(
    message,
    state = state,
    class = "communication_import_error"
  ))
}

#' Etat de validite associe a une erreur d'import communication
#'
#' @param e Condition (erreur) capturee.
#' @return La chaine d'etat structuree, NA_character_ pour une erreur sans
#'   etat (ex. erreur R generique).
#' @export
# §14.1 : délégation à l'accesseur générique. Famille F1 MESURÉE — la CLASSE
# est vérifiée : l'état d'une erreur d'un autre domaine rend NA.
# ⚠️ La classe gardée est `communication_import_error`, PAS `communication_error`.
communication_error_state <- function(e) ts_error_state(e, "communication_import_error")

# Alias de colonnes par source — resolution DETERMINISTE et DOCUMENTEE
# (column_mapping), jamais une supposition silencieuse entre schemas
# incompatibles. Match sur nom trime/minuscule ; premier alias trouve gagne.
.COMMUNICATION_FIELD_ALIASES <- list(
  cellchat = list(
    sender     = c("source", "sender"),
    receiver   = c("target", "receiver"),
    ligand     = c("ligand"),
    receptor   = c("receptor"),
    score      = c("prob", "prob_mean", "score"),
    pathway    = c("pathway", "pathway_name"),
    p_value    = c("pval", "p_value"),
    p_adjusted = c("padj", "p_adjusted")
  ),
  # CellPhoneDB : ligand/receptor proviennent de la SEPARATION de la colonne
  # interacting_pair ("ligand|receptor") — pas d'une colonne dediee.
  cellphonedb = list(
    ligand_pair = c("interacting_pair"),
    p_value_key = c("interacting_pair")
  ),
  # LIANA (CCC 7-8 route (b)) : table agregee produite HORS de l'application.
  #   - `source`/`target` = populations sender/receiver ;
  #   - `ligand`/`receptor` = colonnes DECOMPLEXIFIEES (LIANA decomplexifie
  #     CellChat avant agregation). Les colonnes `.complex` servent de REPLI
  #     quand le decomplexifie est absent : elles sont alors reprises TELLES
  #     QUELLES, jamais decoupees — un complexe (« TGFB1_TGFB2 ») n'a pas de
  #     decoupage univoque, le deviner serait inventer une donnee ;
  #   - `aggregate_rank` est une P-VALUE (RRA : min(p) x k), pas un score :
  #     elle alimente `p_value`, jamais `score`.
  liana = list(
    sender   = c("source", "sender"),
    receiver = c("target", "receiver"),
    ligand   = c("ligand"),
    receptor = c("receptor"),
    ligand_complex   = c("ligand.complex", "ligand_complex"),
    receptor_complex = c("receptor.complex", "receptor_complex"),
    p_value  = c("aggregate_rank")
  )
)

#' Resoudre deterministe-ment la colonne source d'un champ canonique
#'
#' @param tab Table importee (data.frame).
#' @param aliases Vecteur character d'alias acceptes (ordre de priorite).
#' @return Le nom ORIGINAL de la colonne, NA_character_ quand aucun alias
#'   n'est present — l'appelant decide si le champ est requis.
.communication_pick_column <- function(tab, aliases) {
  nms <- tolower(trimws(colnames(tab)))
  for (al in aliases) {
    hit <- match(tolower(al), nms)
    hit <- hit[!is.na(hit)]
    if (length(hit)) return(colnames(tab)[hit[1L]])
  }
  NA_character_
}

#' Champ requis vide ? (NA, "" ou whitespace)
.communication_is_blank <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

#' Coercion numerique tracee (compte les valeurs non convertibles)
#'
#' @param x Vecteur quelconque.
#' @return list(values = numeric, n_coerced_na = integer).
.communication_coerce_numeric <- function(x) {
  if (is.null(x)) return(list(values = rep(NA_real_, 0L), n_coerced_na = 0L))
  raw <- as.character(x)
  raw[.communication_is_blank(raw)] <- NA_character_
  suppressWarnings(vals <- as.numeric(raw))
  n_bad <- sum(is.na(vals) & !is.na(raw) & raw != "", na.rm = TRUE)
  list(values = vals, n_coerced_na = as.integer(n_bad))
}

# =============================================================================
# Parseurs par source
# =============================================================================

#' Importer un objet CellChat sauvegarde (.rds) — extraction, PAS de calcul
#'
#' Route "load" (Stage 12) : lit un objet CellChat produit HORS de
#' l'application et extrait ses tables d'interactions DEJA calculees
#' (@net$prob, @net$pval optionnel). Aucune methode CellChat n'est relancee,
#' aucun score re-agrege au-dela du remodelage deterministe des valeurs non
#' nulles.
#'
#' FORME REELLE de net$prob (MESUREE sur CellChat 2.2.0.9001, 2026-09-15) :
#' array 3-D [groupe SOURCE, groupe CIBLE, interaction_name]. La forme
#' [ligand, recepteur, "sender|receiver"] documentee jusqu'ici etait
#' FICTIVE : aucune version de CellChat ne la produit (0/109 noms
#' d'interaction ne contiennent '|'). Elle est desormais REFUSEE, jamais
#' interpretee a tort.
#'
#' L'extraction est DELEGUEE a .cellchat_engine_extract(), la seule fonction
#' qui connait la structure interne de CellChat (regle 3 : etendre, ne pas
#' dupliquer). `ligand`, `receptor` et `pathway` sont donc resolus via
#' @LR$LRsig quand l'objet le porte (ils ne sont plus systematiquement NA) ;
#' `p_adjusted` reste NA (non produit par CellChat au niveau net). Les etats
#' du moteur sont TRADUITS dans le vocabulaire d'import.
#'
#' @param obj Chemin .rds/.rda, objet S4 CellChat, ou liste nommee avec les
#'   elements `net` (prob, pval optionnel) et `LR` (LRsig).
#' @param source_file Nom ORIGINAL du fichier (jamais le chemin local).
#' @return list(table = champs canoniques, column_mapping, n_input_rows,
#'   warnings) — meme contrat que parse_cellchat_import().
#' @export
parse_cellchat_object <- function(obj, source_file = NA_character_) {
  if (is.character(obj) && length(obj) == 1L) {
    if (!file.exists(obj)) {
      .communication_stop(
        "invalid_input",
        sprintf("Import objet CellChat : fichier introuvable : %s.", obj)
      )
    }
    # .rda/.RData — workspace inspecté (contrat RDATA_IMPORT_CONTRACT.md) :
    # seul un fichier à objet unique est importable ici ; pour un workspace
    # multi-objets, utiliser l'aperçu d'import Single-Cell (« Exporter la
    # sélection ») pour isoler l'objet CellChat, puis réimporter.
    if (rdata_is_supported_file(obj)) {
      env <- rdata_load_env(obj)
      on.exit(rdata_free(env), add = TRUE)
      nms <- ls(envir = env)
      if (length(nms) > 1L) {
        .communication_stop(
          "invalid_input",
          sprintf(paste0(
            "Import objet CellChat : le fichier .RData contient %d objets. ",
            "Utilisez l'aperçu d'import Single-Cell (« Exporter la sélection ») ",
            "pour isoler l'objet CellChat dans un fichier dédié, puis réimportez-le."),
            length(nms)
          )
        )
      }
      obj <- rdata_extract_object(env, nms[1])
    } else {
      obj <- tryCatch(readRDS(obj), error = function(e) {
        .communication_stop(
          "invalid_input",
          sprintf(
            paste0("Import objet CellChat : lecture RDS impossible (%s). ",
                   "L'objet requiert peut-etre un package non installe : %s."),
            basename(obj), conditionMessage(e)
          )
        )
      })
    }
  }

  if (isS4(obj)) {
    slots <- tryCatch(methods::slotNames(obj), error = function(e) character(0))
    if (!"net" %in% slots) {
      .communication_stop(
        "invalid_schema",
        paste0(
          "Import objet CellChat : objet S4 sans slot 'net' — pas un objet ",
          "CellChat reconnaissable. Si le fichier provient d'une session sans ",
          "le package CellChat, reouvrez-le dans R avec CellChat installe ",
          "(remotes::install_github(\"jinworks/CellChat\")) ou exportez la ",
          "table via subsetCommunication()."
        )
      )
    }
  } else if (!is.list(obj)) {
    .communication_stop(
      "invalid_input",
      sprintf(
        paste0("Import objet CellChat : objet CellChat (.rds) ou liste avec ",
               "les elements net/LR requis (recu : %s)."),
        paste(class(obj), collapse = "/")
      )
    )
  }

  # ── Delegation : UNE SEULE lecture de net$prob dans toute l'application ──
  # .cellchat_engine_extract() est la SEULE fonction qui connait la structure
  # interne de CellChat. Reecrire cette lecture ici l'aurait DUPLIQUEE — et
  # c'est precisement ainsi qu'une forme fictive ([ligand, recepteur,
  # "sender|receiver"]) a vecu deux versions sans jamais etre confrontee a un
  # objet reel : tous les tests passaient par un stub de cette forme.
  if (!exists(".cellchat_engine_extract", mode = "function")) {
    .communication_stop(
      "invalid_input",
      paste0("Import objet CellChat : R/sc/sc_communication_engine.R doit ",
             "etre source avant ce fichier (extraction deleguee, jamais ",
             "dupliquee).")
    )
  }
  out <- tryCatch(
    .cellchat_engine_extract(obj, source_file = source_file),
    error = function(e) {
      # Les etats du moteur sont TRADUITS dans le vocabulaire d'import :
      # l'appelant reste un IMPORT, pas un calcul.
      st <- cellchat_engine_error_state(e)
      .communication_stop(
        if (identical(st, "no_interactions")) "invalid_input" else "invalid_schema",
        paste0("Import objet CellChat : ", conditionMessage(e))
      )
    }
  )

  list(
    table = out$table,
    column_mapping = out$column_mapping,
    n_input_rows = out$n_input_rows,
    warnings = out$warnings
  )
}

#' Importer une table de resultats CellChat (table exportee)
#'
#' Convertit une table exportee de CellChat (ex. sortie de
#' subsetCommunication()/netDF exportee en CSV) vers la structure intermediaire
#  du contrat. Colonnes reconnues (deterministes, mapping enregistre) :
#'   sender <- source|sender ; receiver <- target|receiver ;
#'   ligand <- ligand ; receptor <- receptor ;
#'   score <- prob|prob_mean|score ; pathway <- pathway|pathway_name
#'   (optionnel) ; p_value <- pval|p_value (optionnel) ;
#'   p_adjusted <- padj|p_adjusted (optionnel).
#' Les colonnes d'origine utiles (ligand.group, receptor.group, pathway_name)
#' sont conservees si presentes. Aucune valeur n'est inventee : les champs
#' absents de la source restent NA.
#'
#' @param tab Table importee (data.frame) — la lecture du fichier est du
#'   ressort de l'appelant (module).
#' @param source_file Nom ORIGINAL du fichier source (jamais le chemin local
#'   complet).
#' @return list(table = data.frame champs canoniques + colonnes d'origine,
#'   column_mapping = liste nommee, n_input_rows = entier, warnings).
#' @export
parse_cellchat_import <- function(tab, source_file = NA_character_) {
  if (is.null(tab) || !is.data.frame(tab)) {
    .communication_stop(
      "invalid_input",
      paste0(
        "Import CellChat : une table (data.frame) lue depuis le fichier ",
        "exporte est requise (recu : ",
        if (is.null(tab)) "NULL" else paste(class(tab), collapse = "/"), ")."
      )
    )
  }
  if (nrow(tab) == 0L) {
    .communication_stop(
      "invalid_input",
      "Import CellChat : la table importee est vide (0 ligne)."
    )
  }

  col <- vapply(
    c("sender", "receiver", "ligand", "receptor", "score", "pathway",
      "p_value", "p_adjusted"),
    function(f) .communication_pick_column(tab, .COMMUNICATION_FIELD_ALIASES$cellchat[[f]]),
    character(1)
  )

  missing_required <- c("sender", "receiver", "ligand", "receptor", "score")[
    is.na(col[c("sender", "receiver", "ligand", "receptor", "score")])
  ]
  if (length(missing_required)) {
    .communication_stop(
      "invalid_schema",
      sprintf(
        paste0(
          "Import CellChat : colonne(s) requise(s) absente(s) de la table ",
          "importee : %s. Colonnes presentes : %s. Formats attendus : ",
          "table exportee CellChat (source/target/ligand/receptor/prob)."
        ),
        paste(missing_required, collapse = ", "),
        paste(utils::head(colnames(tab), 20), collapse = ", ")
      )
    )
  }

  n <- nrow(tab)

  score_c <- .communication_coerce_numeric(tab[[col[["score"]]]])
  pval_c <- if (!is.na(col[["p_value"]])) {
    .communication_coerce_numeric(tab[[col[["p_value"]]]])
  } else list(values = rep(NA_real_, n), n_coerced_na = 0L)
  padj_c <- if (!is.na(col[["p_adjusted"]])) {
    .communication_coerce_numeric(tab[[col[["p_adjusted"]]]])
  } else list(values = rep(NA_real_, n), n_coerced_na = 0L)

  ligand <- as.character(tab[[col[["ligand"]]]])
  receptor <- as.character(tab[[col[["receptor"]]]])

  table <- data.frame(
    sender     = as.character(tab[[col[["sender"]]]]),
    receiver   = as.character(tab[[col[["receiver"]]]]),
    ligand     = ligand,
    receptor   = receptor,
    interaction = paste(ligand, receptor, sep = " -> "),
    pathway    = if (!is.na(col[["pathway"]])) {
      as.character(tab[[col[["pathway"]]]])
    } else rep(NA_character_, n),
    score      = score_c$values,
    p_value    = pval_c$values,
    p_adjusted = padj_c$values,
    source_method = rep("cellchat", n),
    source_file = rep(as.character(source_file)[1L], n),
    source_cell_identity_level = rep(NA_character_, n),
    stringsAsFactors = FALSE
  )

  # Colonnes d'origine utiles, conservees si presentes (mapping explicite).
  extra_mapping <- list()
  for (pair in list(c("ligand.group", "ligand_group"),
                    c("receptor.group", "receptor_group"),
                    c("pathway_name", "pathway_name"))) {
    picked <- .communication_pick_column(tab, pair[1L])
    if (!is.na(picked)) {
      table[[pair[2L]]] <- as.character(tab[[picked]])
      extra_mapping[[pair[2L]]] <- picked
    }
  }

  warnings <- character(0)
  if (score_c$n_coerced_na > 0L) {
    warnings <- c(warnings, sprintf(
      "Import CellChat : %d valeur(s) de score non numerique(s) mises a NA.",
      score_c$n_coerced_na
    ))
  }

  column_mapping <- list(
    sender = col[["sender"]], receiver = col[["receiver"]],
    ligand = col[["ligand"]], receptor = col[["receptor"]],
    score = col[["score"]],
    pathway = if (!is.na(col[["pathway"]])) col[["pathway"]] else NULL,
    p_value = if (!is.na(col[["p_value"]])) col[["p_value"]] else NULL,
    p_adjusted = if (!is.na(col[["p_adjusted"]])) col[["p_adjusted"]] else NULL
  )
  column_mapping <- c(column_mapping, extra_mapping)

  list(
    table = table,
    column_mapping = column_mapping,
    n_input_rows = as.integer(n),
    warnings = warnings
  )
}

#' Importer les resultats CellPhoneDB (means.txt + pvalues.txt)
#'
#' Format v1 supporte : tables large de CellPhoneDB avec colonne
#' `interacting_pair` ("ligand|receptor") et UNE colonne par paire de
#' populations "sender|receiver" (convention CellPhoneDB partner_a|partner_b :
#' la population AVANT "|" est considerée sender). pvalues.txt optionnel —
#' les p-values sont rapprochees par (interacting_pair, colonne de paire) ;
#' une paire absente de la table de p-values reste NA (jamais fabriquee).
#' p_adjusted n'existe pas dans ce format : il reste NA, avec un
#' avertissement explicite.
#'
#' @param means_tab Table means (data.frame) — requise.
#' @param pvalues_tab Table pvalues (data.frame) — optionnelle (NULL).
#' @param source_file Nom ORIGINAL du fichier means (jamais le chemin local).
#' @return list(table, column_mapping, n_input_rows, warnings).
#' @export
parse_cellphonedb_import <- function(means_tab, pvalues_tab = NULL,
                                     source_file = NA_character_) {
  if (is.null(means_tab) || !is.data.frame(means_tab)) {
    .communication_stop(
      "invalid_input",
      paste0(
        "Import CellPhoneDB : la table means (data.frame) est requise ",
        "(recu : ",
        if (is.null(means_tab)) "NULL" else paste(class(means_tab), collapse = "/"),
        ")."
      )
    )
  }
  if (nrow(means_tab) == 0L) {
    .communication_stop(
      "invalid_input",
      "Import CellPhoneDB : la table means importee est vide (0 ligne)."
    )
  }

  ip_col <- .communication_pick_column(
    means_tab, .COMMUNICATION_FIELD_ALIASES$cellphonedb$ligand_pair
  )
  if (is.na(ip_col)) {
    .communication_stop(
      "invalid_schema",
      sprintf(
        paste0(
          "Import CellPhoneDB : colonne 'interacting_pair' absente de la ",
          "table means. Colonnes presentes : %s. Format attendu : ",
          "means.txt de CellPhoneDB (v2) avec colonne interacting_pair."
        ),
        paste(utils::head(colnames(means_tab), 20), collapse = ", ")
      )
    )
  }

  pair_cols <- grep("|", colnames(means_tab), fixed = TRUE, value = TRUE)
  if (!length(pair_cols)) {
    .communication_stop(
      "invalid_schema",
      sprintf(
        paste0(
          "Import CellPhoneDB : aucune colonne de paire 'sender|receiver' ",
          "trouvee dans la table means (colonnes presentes : %s). Format ",
          "attendu : means.txt avec une colonne par paire de populations."
        ),
        paste(utils::head(colnames(means_tab), 20), collapse = ", ")
      )
    )
  }

  ip <- as.character(means_tab[[ip_col]])
  parts <- strsplit(ip, "|", fixed = TRUE)
  n_parts <- lengths(parts)
  bad_idx <- which(n_parts != 2L)
  if (length(bad_idx)) {
    .communication_stop(
      "invalid_schema",
      sprintf(
        paste0(
          "Import CellPhoneDB : %d valeur(s) d'interacting_pair sans ",
          "separateur '|' unique (ex. : %s). Le format v1 exige ",
          "'ligand|receptor' exactement — aucune conversion supposee."
        ),
        length(bad_idx),
        paste(utils::head(ip[bad_idx], 3), collapse = " ; ")
      )
    )
  }

  means_num <- lapply(pair_cols, function(pc) {
    .communication_coerce_numeric(means_tab[[pc]])
  })
  names(means_num) <- pair_cols

  # P-values optionnelles : rapprochement (interacting_pair, colonne paire).
  pval_lookup <- NULL
  pval_warnings <- character(0)
  if (!is.null(pvalues_tab)) {
    if (!is.data.frame(pvalues_tab)) {
      .communication_stop(
        "invalid_input",
        "Import CellPhoneDB : la table pvalues fournie n'est pas un data.frame."
      )
    }
    ip_p <- .communication_pick_column(
      pvalues_tab, .COMMUNICATION_FIELD_ALIASES$cellphonedb$p_value_key
    )
    pair_p <- grep("|", colnames(pvalues_tab), fixed = TRUE, value = TRUE)
    if (is.na(ip_p) || !length(pair_p)) {
      pval_warnings <- c(pval_warnings, paste0(
        "Import CellPhoneDB : table pvalues fournie mais sans ",
        "'interacting_pair' ni colonne de paire exploitable — p_value laissee ",
        "a NA (jamais fabriquee)."
      ))
    } else {
      pval_lookup <- new.env(parent = emptyenv())
      for (pc in pair_p) {
        vals <- .communication_coerce_numeric(pvalues_tab[[pc]])
        keys <- paste(as.character(pvalues_tab[[ip_p]]), pc, sep = "\r")
        for (i in seq_along(keys)) pval_lookup[[keys[i]]] <- vals$values[i]
      }
    }
  }

  # Reshape long : une ligne par (interaction x paire de populations).
  rows <- list()
  i_row <- 0L
  for (pc in pair_cols) {
    split_pair <- strsplit(pc, "|", fixed = TRUE)[[1L]]
    sender_pc <- if (length(split_pair) >= 1L) trimws(split_pair[1L]) else NA_character_
    receiver_pc <- if (length(split_pair) >= 2L) trimws(split_pair[2L]) else NA_character_
    m_vals <- means_num[[pc]]$values
    for (i in seq_len(nrow(means_tab))) {
      i_row <- i_row + 1L
      key <- paste(ip[i], pc, sep = "\r")
      pv <- if (!is.null(pval_lookup) && !is.null(pval_lookup[[key]])) {
        pval_lookup[[key]]
      } else NA_real_
      rows[[i_row]] <- data.frame(
        sender = sender_pc, receiver = receiver_pc,
        ligand = trimws(parts[[i]][1L]), receptor = trimws(parts[[i]][2L]),
        interaction = paste(trimws(parts[[i]][1L]), trimws(parts[[i]][2L]),
                            sep = " -> "),
        pathway = NA_character_,
        score = m_vals[i], p_value = pv, p_adjusted = NA_real_,
        source_method = "cellphonedb",
        source_file = as.character(source_file)[1L],
        source_cell_identity_level = NA_character_,
        stringsAsFactors = FALSE
      )
    }
  }
  table <- do.call(rbind, rows)

  warnings <- c(pval_warnings, sprintf(
    paste0("Import CellPhoneDB : p_adjusted absent du format means/pvalues ",
           "v1 — colonne laissee a NA pour les %d lignes canoniques."),
    nrow(table)
  ))
  n_bad_means <- sum(vapply(means_num, function(m) m$n_coerced_na, integer(1)))
  if (n_bad_means > 0L) {
    warnings <- c(warnings, sprintf(
      "Import CellPhoneDB : %d valeur(s) de score non numerique(s) mises a NA.",
      n_bad_means
    ))
  }

  column_mapping <- c(
    list(
      ligand = paste0("interacting_pair (", ip_col, ") avant '|'"),
      receptor = paste0("interacting_pair (", ip_col, ") apres '|'"),
      sender = "nom de colonne de paire avant '|'",
      receiver = "nom de colonne de paire apres '|'",
      score = "valeur de la colonne de paire (means)"
    ),
    if (!is.null(pval_lookup)) list(p_value = "pvalues.txt rapproche par (interacting_pair, colonne de paire)") else list()
  )

  list(
    table = table,
    column_mapping = column_mapping,
    n_input_rows = as.integer(nrow(means_tab)),
    warnings = warnings
  )
}

#' Importer les resultats LIANA agreges (rangs) — CCC 7-8 route (b)
#'
#' Route (b) : la table a ETE produite par LIANA HORS de l'application
#' (`liana_aggregate()` / `rank_aggregate()` avec `get_ranks = TRUE` et
#' `get_agrank = TRUE`). L'application n'execute AUCUNE methode CCC, ne
#' recalcule AUCUN rang et ne fabrique AUCUN score : elle lit des colonnes
#' deja calculees et les range dans la table canonique.
#'
#' ── SEMANTIQUE DES RANGS (a ne pas confondre avec un score) ────────────────
#' 1. **Dans LIANA, plus petit = meilleur.** Rang 1 = interaction la plus
#'    specifique (ou la plus abondante selon le mode). C'est l'INVERSE de
#'    `prob` (CellChat) et d'`edge_specificity`. `rank_direction` vaut donc
#'    "lower_is_better" pour TOUTES les lignes.
#' 2. **`aggregate_rank` est une P-VALUE**, pas un score de communication :
#'    produite par Robust Rank Aggregation (`min(p) x k`, `p_i = pbeta(r_i, i,
#'    k-i+1)`). Elle alimente `p_value` — jamais `score`.
#' 3. **`score` reste NA** : le contrat interdit de fabriquer un champ absent,
#'    et convertir un rang en score serait une erreur de categorie (echelles et
#'    directions differentes).
#' 4. **`rank_aggregation_mode` est un CHOIX EXPLICITE** (`specificity` ou
#'    `magnitude`) : les deux modes repondent a des questions differentes et ne
#'    sont pas comparables entre eux. Aucun defaut implicite.
#'
#' ── DEUX PARAMETRES SONT REQUIS (pas deduits du fichier) ───────────────────
#' `rank_column` et `aggregation_mode` ne sont pas deductibles de façon fiable
#' du contenu : un fichier peut porter `mean_rank`, `aggregate_rank` ET une
#' colonne `{methode}.rank` par methode agregee. L'appelant les declare.
#'
#' @param tab Table importee (data.frame) — la lecture du fichier est du
#'   ressort de l'appelant (module).
#' @param rank_column Nom de la colonne de rang a importer (`mean_rank`,
#'   `aggregate_rank` ou `{methode}.rank`). Resolution insensible a la casse.
#' @param aggregation_mode `specificity` ou `magnitude` (voir ci-dessus).
#' @param source_file Nom ORIGINAL du fichier source (jamais le chemin local).
#' @return list(table = champs canoniques + champs de rang
#'   (communication_rank_fields()), column_mapping, n_input_rows, warnings,
#'   external_consensus = logical — TRUE quand la table porte une valeur
#'   agregee INTER-METHODES calculee par LIANA).
#' @export
parse_liana_import <- function(tab, rank_column, aggregation_mode,
                               source_file = NA_character_) {
  if (is.null(tab) || !is.data.frame(tab)) {
    .communication_stop(
      "invalid_input",
      paste0(
        "Import LIANA : une table (data.frame) lue depuis le fichier agrege ",
        "est requise (recu : ",
        if (is.null(tab)) "NULL" else paste(class(tab), collapse = "/"), ")."
      )
    )
  }
  if (nrow(tab) == 0L) {
    .communication_stop(
      "invalid_input",
      "Import LIANA : la table importee est vide (0 ligne)."
    )
  }

  # -- Mode d'agregation : CHOIX EXPLICITE de l'utilisateur. Jamais de defaut
  #    implicite : specificity et magnitude repondent a deux questions
  #    differentes (cf. doc ci-dessus).
  mode_val <- as.character(aggregation_mode)[1L]
  if (is.na(mode_val) || .communication_is_blank(mode_val)) {
    .communication_stop(
      "invalid_input",
      paste0(
        "Import LIANA : le mode d'agregation est requis (aucun defaut ",
        "implicite). Valeurs acceptees : ",
        paste(communication_rank_aggregation_modes(), collapse = ", "), "."
      )
    )
  }
  if (!mode_val %in% communication_rank_aggregation_modes()) {
    .communication_stop(
      "invalid_input",
      sprintf(
        "Import LIANA : mode d'agregation inconnu '%s'. Valeurs acceptees : %s.",
        mode_val, paste(communication_rank_aggregation_modes(), collapse = ", ")
      )
    )
  }

  # -- Colonne de rang : CHOIX EXPLICITE egalement.
  rank_req <- as.character(rank_column)[1L]
  if (is.na(rank_req) || .communication_is_blank(rank_req)) {
    .communication_stop(
      "invalid_input",
      paste0(
        "Import LIANA : la colonne de rang a importer est requise (aucun ",
        "defaut implicite) — ex. 'mean_rank', 'aggregate_rank' ou ",
        "'natmi.rank'. La route (b) importe des RANGS : sans rang, il n'y a ",
        "pas de mesure."
      )
    )
  }

  col <- vapply(
    c("sender", "receiver", "ligand", "receptor", "ligand_complex",
      "receptor_complex", "p_value"),
    function(f) .communication_pick_column(tab, .COMMUNICATION_FIELD_ALIASES$liana[[f]]),
    character(1)
  )

  missing_required <- c("sender", "receiver")[
    is.na(col[c("sender", "receiver")])
  ]
  if (length(missing_required)) {
    .communication_stop(
      "invalid_schema",
      sprintf(
        paste0(
          "Import LIANA : colonne(s) requise(s) absente(s) de la table ",
          "importee : %s. Colonnes presentes : %s. Format attendu : table ",
          "agregee LIANA (source/target/ligand.complex/receptor.complex + ",
          "colonnes de rang)."
        ),
        paste(missing_required, collapse = ", "),
        paste(utils::head(colnames(tab), 20), collapse = ", ")
      )
    )
  }

  # Ligand/recepteur : la colonne DECOMPLEXIFIEE est prioritaire ; a defaut on
  # reprend la colonne `.complex` TELLE QUELLE (jamais decoupee : un complexe
  # n'a pas de decoupage univoque — le deviner serait inventer une donnee).
  lig_col <- if (!is.na(col[["ligand"]])) col[["ligand"]] else col[["ligand_complex"]]
  rec_col <- if (!is.na(col[["receptor"]])) col[["receptor"]] else col[["receptor_complex"]]
  if (is.na(lig_col) || is.na(rec_col)) {
    .communication_stop(
      "invalid_schema",
      sprintf(
        paste0(
          "Import LIANA : ni ligand/receptor decomplexifies ni ligand.complex/",
          "receptor.complex trouves (colonnes presentes : %s)."
        ),
        paste(utils::head(colnames(tab), 20), collapse = ", ")
      )
    )
  }
  ligand_from_complex <- is.na(col[["ligand"]])
  receptor_from_complex <- is.na(col[["receptor"]])

  # -- La colonne de rang demandee doit EXISTER (resolution insensible a la
  #    casse, comme tous les alias).
  rank_col <- .communication_pick_column(tab, rank_req)
  if (is.na(rank_col)) {
    rank_like <- grep("(^mean_rank$)|(^aggregate_rank$)|(\\.rank$)",
                      colnames(tab), value = TRUE, ignore.case = TRUE)
    .communication_stop(
      "invalid_schema",
      sprintf(
        paste0(
          "Import LIANA : colonne de rang '%s' absente de la table importee. ",
          "Colonnes de rang detectees : %s. Colonnes presentes : %s."
        ),
        rank_req,
        if (length(rank_like)) paste(utils::head(rank_like, 20), collapse = ", ") else "(aucune)",
        paste(utils::head(colnames(tab), 20), collapse = ", ")
      )
    )
  }

  n <- nrow(tab)
  rank_c <- .communication_coerce_numeric(tab[[rank_col]])
  pval_c <- if (!is.na(col[["p_value"]])) {
    .communication_coerce_numeric(tab[[col[["p_value"]]]])
  } else {
    list(values = rep(NA_real_, n), n_coerced_na = 0L)
  }

  ligand <- as.character(tab[[lig_col]])
  receptor <- as.character(tab[[rec_col]])

  table <- data.frame(
    sender     = as.character(tab[[col[["sender"]]]]),
    receiver   = as.character(tab[[col[["receiver"]]]]),
    ligand     = ligand,
    receptor   = receptor,
    interaction = paste(ligand, receptor, sep = " -> "),
    # LIANA n'expose pas de pathway dans la table agregee : NA, jamais fabrique.
    pathway    = rep(NA_character_, n),
    # score : NA par CONSTRUCTION (un rang n'est pas un score).
    score      = rep(NA_real_, n),
    # aggregate_rank EST une p-value (RRA) : c'est sa place exacte.
    p_value    = pval_c$values,
    p_adjusted = rep(NA_real_, n),
    source_method = rep("liana", n),
    source_file = rep(as.character(source_file)[1L], n),
    source_cell_identity_level = rep(NA_character_, n),
    # -- Champs de MESURE de rang (communication_rank_fields()).
    rank = rank_c$values,
    rank_direction = rep(.COMMUNICATION_RANK_DIRECTION_LOWER, n),
    rank_aggregation_mode = rep(mode_val, n),
    stringsAsFactors = FALSE
  )

  # -- Consensus inter-methodes : `mean_rank` (moyenne des rangs) et
  #    `aggregate_rank` (RRA) sont des agregats calcules par LIANA sur
  #    PLUSIEURS methodes ; une colonne `{methode}.rank` ne concerne qu'UNE
  #    methode. L'application n'agrege rien elle-meme — elle importe et
  #    MARQUE, pour que la nature de la valeur reste tracable dans le
  #    resultat et pas seulement dans la documentation.
  rank_is_consensus <- grepl("^(mean_rank|aggregate_rank)$", rank_col, ignore.case = TRUE)
  pval_is_consensus <- !is.na(col[["p_value"]])
  external_consensus <- isTRUE(rank_is_consensus || pval_is_consensus)

  warnings <- character(0)
  warnings <- c(warnings, sprintf(
    paste0("Import LIANA : score laisse a NA pour les %d lignes — LIANA ",
           "produit des rangs et des p-values, pas un score comparable a ",
           "'prob' (CellChat). Aucune conversion rang -> score n'est faite."),
    n
  ))
  warnings <- c(warnings, sprintf(
    paste0("Import LIANA : pathway laisse a NA pour les %d lignes — la table ",
           "agregee LIANA n'expose pas de pathway."),
    n
  ))
  warnings <- c(warnings, sprintf(
    paste0("Import LIANA : p_adjusted laisse a NA pour les %d lignes — le ",
           "format agrege n'expose pas de p-value ajustee."),
    n
  ))
  warnings <- c(warnings, sprintf(
    paste0("Import LIANA : rang importe depuis '%s' — direction ",
           "'lower_is_better' (dans LIANA, rang 1 = meilleur ; c'est l'INVERSE ",
           "de 'prob'). Mode d'agregation : '%s'."),
    rank_col, mode_val
  ))
  if (rank_c$n_coerced_na > 0L) {
    warnings <- c(warnings, sprintf(
      "Import LIANA : %d valeur(s) de rang non numerique(s) mises a NA.",
      rank_c$n_coerced_na
    ))
  }
  if (pval_c$n_coerced_na > 0L) {
    warnings <- c(warnings, sprintf(
      "Import LIANA : %d valeur(s) d'aggregate_rank non numerique(s) mises a NA.",
      pval_c$n_coerced_na
    ))
  }
  if (external_consensus) {
    warnings <- c(warnings, paste0(
      "Import LIANA : la table porte une valeur agregee INTER-METHODES ",
      "calculee par LIANA (", rank_col, " et/ou aggregate_rank). ",
      "L'application n'agrege rien elle-meme, mais cette valeur n'est PAS ",
      "le resultat d'une seule methode — voir le contrat, section 6."
    ))
  }

  column_mapping <- list(
    sender = col[["sender"]],
    receiver = col[["receiver"]],
    ligand = if (ligand_from_complex) {
      paste0(lig_col, " (complexe repris tel quel)")
    } else lig_col,
    receptor = if (receptor_from_complex) {
      paste0(rec_col, " (complexe repris tel quel)")
    } else rec_col,
    rank = rank_col,
    rank_direction = "constante (lower_is_better)",
    rank_aggregation_mode = paste0("choix utilisateur : ", mode_val)
  )
  if (!is.na(col[["p_value"]])) {
    column_mapping$p_value <- col[["p_value"]]
  }

  list(
    table = table,
    column_mapping = column_mapping,
    n_input_rows = as.integer(n),
    warnings = warnings,
    external_consensus = external_consensus
  )
}

# =============================================================================
# Harmonisation des identites (exact match uniquement)
# =============================================================================

#' Harmoniser les identites sender/receiver avec une colonne Seurat explicite
#'
#' Compare chaque label sender/receiver de la table canonique aux identites de
#' la colonne de metadonnees Seurat choisie par l'utilisateur. Politique v1 :
#' EXACT match uniquement — aucune correspondance floue, aucun renommage
#' silencieux de population. Les labels sans correspondance restent tels
#' quels dans la table et leurs colonnes *_mapped sont NA ; ils sont listes
#' integralement dans le retour (reviewable). Les collisions de casse entre
#' identites Seurat (ex. "T cells" vs "T Cells") sont signalees ambiguës.
#'
#' @param canonical_table Table canonique (parse_*_import).
#' @param seurat_identities Vecteur character des identites uniques de la
#'   colonne de metadonnees choisie (extrait par le module depuis obj@meta.data).
#' @param identity_column Nom de la colonne choisie (tracons dans la table).
#' @param context Contexte cite dans les messages d'erreur.
#' @return list(table = canonique + sender_mapped/receiver_mapped/
#'   source_cell_identity_level renseignes, mapping = data.frame(label, roles,
#'   matched_identity, matched), summary = list(...), warnings).
#' @export
harmonize_communication_identities <- function(canonical_table,
                                               seurat_identities,
                                               identity_column,
                                               context = "harmonisation communication") {
  if (!is.data.frame(canonical_table) ||
      !all(c("sender", "receiver") %in% colnames(canonical_table))) {
    .communication_stop(
      "invalid_input",
      sprintf(
        "Echec %s : table canonique avec sender/receiver requise (recu : %s).",
        context, if (is.null(canonical_table)) "NULL" else paste(class(canonical_table), collapse = "/")
      )
    )
  }
  if (is.null(seurat_identities) || !length(seurat_identities) ||
      all(is.na(seurat_identities))) {
    .communication_stop(
      "invalid_identity_mapping",
      sprintf(
        paste0("Echec %s : la colonne de metadonnees '%s' ne contient ",
               "aucune identite exploitable (vide ou tout NA)."),
        context, identity_column
      )
    )
  }
  ids <- unique(as.character(seurat_identities[!is.na(seurat_identities)]))

  labels <- unique(c(canonical_table$sender, canonical_table$receiver))
  labels <- labels[!.communication_is_blank(labels)]

  if (!length(labels)) {
    .communication_stop(
      "invalid_schema",
      "Echec harmonisation : aucun label sender/receiver non vide dans la table canonique."
    )
  }

  matched <- labels %in% ids
  mapping <- data.frame(
    label = labels,
    roles = vapply(labels, function(l) {
      r <- character(0)
      if (any(canonical_table$sender == l, na.rm = TRUE)) r <- c(r, "sender")
      if (any(canonical_table$receiver == l, na.rm = TRUE)) r <- c(r, "receiver")
      paste(r, collapse = "+")
    }, character(1), USE.NAMES = FALSE),
    matched_identity = ifelse(matched, labels, NA_character_),
    matched = matched,
    stringsAsFactors = FALSE
  )

  map_one <- function(x) ifelse(x %in% ids, x, NA_character_)
  canonical_table$sender_mapped <- map_one(canonical_table$sender)
  canonical_table$receiver_mapped <- map_one(canonical_table$receiver)
  canonical_table$source_cell_identity_level <- rep(
    as.character(identity_column)[1L], nrow(canonical_table)
  )

  unmatched_labels <- labels[!matched]

  # Ambiguite : identites Seurat (ou labels sources) ne se distinguant que par
  # la casse — l'exact match reste deterministe, mais l'utilisateur doit le
  # savoir.
  ambiguous_ids <- character(0)
  upper <- toupper(ids)
  if (anyDuplicated(upper)) {
    ambiguous_ids <- ids[duplicated(upper) | duplicated(upper, fromLast = TRUE)]
  }
  ambiguous_labels <- character(0)
  upper_l <- toupper(labels)
  if (anyDuplicated(upper_l)) {
    ambiguous_labels <- labels[duplicated(upper_l) |
                                 duplicated(upper_l, fromLast = TRUE)]
  }

  warnings <- character(0)
  if (length(unmatched_labels)) {
    warnings <- c(warnings, sprintf(
      paste0("Harmonisation : %d label(s) sans correspondance exacte dans ",
             "'%s' (conserves tels quels, colonnes *_mapped a NA) : %s."),
      length(unmatched_labels), identity_column,
      paste(utils::head(unmatched_labels, 10), collapse = ", ")
    ))
  }
  if (length(ambiguous_ids)) {
    warnings <- c(warnings, sprintf(
      paste0("Identites Seurat ambiguës (distinguees uniquement par la ",
             "casse) dans '%s' : %s."),
      identity_column, paste(ambiguous_ids, collapse = ", ")
    ))
  }
  if (length(ambiguous_labels)) {
    warnings <- c(warnings, sprintf(
      paste0("Labels de la table ne se distinguant que par la casse : %s. ",
             "L'exact match reste deterministe."),
      paste(ambiguous_labels, collapse = ", ")
    ))
  }

  # Un resultat ou AUCUN label ne correspond est bloque : la colonne choisie
  # ne decrit vraisemblablement pas les populations de la source.
  if (!any(matched)) {
    .communication_stop(
      "invalid_identity_mapping",
      sprintf(
        paste0("Echec %s : aucun label sender/receiver ne correspond a la ",
               "colonne '%s' (identites presentes : %s ; labels sources : %s). ",
               "Choisissez la colonne de metadonnees adequate."),
        context, identity_column,
        paste(utils::head(ids, 10), collapse = ", "),
        paste(utils::head(labels, 10), collapse = ", ")
      )
    )
  }

  n_rows_partial <- sum(is.na(canonical_table$sender_mapped) |
                          is.na(canonical_table$receiver_mapped))

  list(
    table = canonical_table,
    mapping = mapping,
    summary = list(
      identity_column = as.character(identity_column)[1L],
      n_identities = length(ids),
      n_labels = length(labels),
      n_labels_matched = sum(matched),
      n_labels_unmatched = length(unmatched_labels),
      unmatched_labels = unmatched_labels,
      ambiguous_identities = ambiguous_ids,
      ambiguous_labels = ambiguous_labels,
      n_rows = nrow(canonical_table),
      n_rows_with_unmatched_role = as.integer(n_rows_partial)
    ),
    warnings = warnings
  )
}

# =============================================================================
# QC d'import
# =============================================================================

#' QC d'import de la table canonique
#'
#' Compte et signale, sans rien inventer : lignes droppees (sender/receiver/
#' ligand/receptor vide), auto-interactions, interactions dupliquees (cle
#' sender|receiver|ligand|receptor — conservees et flagees, jamais fusionnees
#' silencieusement), pathways manquants, p-values hors [0,1], scores non
#' numeriques. La table en entree est harmonisee ; la table en sortie ajoute
#' la colonne duplicate_interaction.
#'
#' @param canonical_table Table canonique harmonisee.
#' @return list(table = table + duplicate_interaction, counts = liste nommee,
#'   warnings).
#' @export
communication_import_qc <- function(canonical_table) {
  if (!is.data.frame(canonical_table)) {
    .communication_stop(
      "invalid_input",
      paste0("QC communication : table canonique requise (recu : ",
             if (is.null(canonical_table)) "NULL" else paste(class(canonical_table), collapse = "/"),
             ").")
    )
  }
  n_before <- nrow(canonical_table)

  required <- c("sender", "receiver", "ligand", "receptor")
  blank_rows <- Reduce(`|`, lapply(required, function(f) {
    .communication_is_blank(canonical_table[[f]])
  }))
  n_dropped <- sum(blank_rows)
  table <- canonical_table[!blank_rows, , drop = FALSE]

  table$duplicate_interaction <- duplicated(
    paste(table$sender, table$receiver, table$ligand, table$receptor, sep = "|")
  ) | duplicated(
    paste(table$sender, table$receiver, table$ligand, table$receptor, sep = "|"),
    fromLast = TRUE
  )

  n_self <- if (nrow(table)) sum(table$sender == table$receiver) else 0L
  n_dup <- if (nrow(table)) sum(table$duplicate_interaction) else 0L
  n_pathway_missing <- if (nrow(table)) sum(is.na(table$pathway)) else 0L

  p_bad <- 0L
  if (nrow(table)) {
    p <- table$p_value
    p_bad <- sum(!is.na(p) & (p < 0 | p > 1))
  }

  # -- QC des rangs : uniquement quand la source fournit les champs de mesure
  #    (aujourd'hui la route LIANA). Borne HAUTE = nombre de lignes ; borne
  #    BASSE = 0 et non 1 : `aggregate_rank` est une p-value RRA sur [0,1] et
  #    peut legitimement etre importe comme colonne de rang — mieux vaut
  #    sous-signaler que produire un faux positif sur un fichier valide.
  n_rank_bad <- 0L
  n_rank_missing <- 0L
  if (nrow(table) && "rank" %in% colnames(table)) {
    r <- table$rank
    n_rank_bad <- sum(!is.na(r) & (r < 0 | r > n_before))
    n_rank_missing <- sum(is.na(r))
  }

  counts <- list(
    n_rows_before = as.integer(n_before),
    n_rows_after = as.integer(nrow(table)),
    n_dropped_required_fields = as.integer(n_dropped),
    n_self_interactions = as.integer(n_self),
    n_duplicate_interactions = as.integer(n_dup),
    n_pathway_missing = as.integer(n_pathway_missing),
    n_p_value_out_of_range = as.integer(p_bad),
    n_rank_out_of_range = as.integer(n_rank_bad),
    n_rank_missing = as.integer(n_rank_missing),
    duplicate_policy = "conservees_flaggees"
  )

  warnings <- character(0)
  if (n_dropped > 0L) {
    warnings <- c(warnings, sprintf(
      paste0("QC : %d ligne(s) supprimee(s) — sender/receiver/ligand/receptor ",
             "vide (avant : %d, apres : %d)."),
      n_dropped, n_before, nrow(table)
    ))
  }
  if (n_self > 0L) {
    warnings <- c(warnings, sprintf(
      "%d auto-interaction(s) (sender = receiver) conservees et comptabilisees.",
      n_self
    ))
  }
  if (n_dup > 0L) {
    warnings <- c(warnings, sprintf(
      paste0("%d ligne(s) en interaction dupliquee (cle ",
             "sender|receiver|ligand|receptor) — conservees et flagees, ",
             "jamais fusionnees."),
      n_dup
    ))
  }
  if (p_bad > 0L) {
    warnings <- c(warnings, sprintf(
      "%d p-value(s) hors [0,1] — conservees, comptabilisees, jamais corrigees.",
      p_bad
    ))
  }
  if (n_rank_bad > 0L) {
    warnings <- c(warnings, sprintf(
      paste0("%d rang(s) hors [0, n] (n = %d lignes) — conserves, ",
             "comptabilises, jamais corriges."),
      n_rank_bad, nrow(table)
    ))
  }
  if (n_rank_missing > 0L) {
    warnings <- c(warnings, sprintf(
      "%d rang(s) manquant(s) (NA) — conserves, jamais imputes.",
      n_rank_missing
    ))
  }

  list(table = table, counts = counts, warnings = warnings)
}

# =============================================================================
# Finalisation du resultat canonique + garde + peremption + exports
# =============================================================================

#' Verifier que l'objet est un resultat communication canonique
#'
#' Garde de contrat pour TOUT consommateur (visualisation Stage 12, exports,
#' futur rapport) : n'accepte qu'un objet produit par
#' finalize_communication_result(), refuse un resultat perime vis-a-vis de
#' l'objet Seurat fourni. Un consommateur ne cree, n'infere et ne repare
#' jamais un resultat d'import.
#'
#' @param communication_result Objet a verifier (resultat canonique attendu).
#' @param seurat_obj Objet Seurat courant optionnel : fourni, la peremption
#'   (stale_against_current_seurat_object) est verifiee.
#' @param context Contexte cite dans les messages d'erreur.
#' @return Le resultat, invisible (pipable, conforme au style assert_*).
#' @export
assert_communication_result <- function(communication_result,
                                        seurat_obj = NULL,
                                        context = "communication") {
  if (!is.list(communication_result) ||
      !identical(communication_result$type %||% NULL,
                 "cell_cell_communication") ||
      is.null(communication_result$status)) {
    .communication_stop(
      "invalid_input",
      sprintf(
        paste0("Echec %s : resultat communication canonique requis ",
               "(finalize_communication_result()) — recu : %s."),
        context,
        if (is.null(communication_result)) "NULL"
        else paste(class(communication_result), collapse = "/")
      )
    )
  }
  if (!communication_result$status %in% communication_validity_states()) {
    .communication_stop(
      "invalid_input",
      sprintf("Echec %s : statut communication inconnu '%s'.",
              context, communication_result$status)
    )
  }
  if (!is.null(seurat_obj) &&
      isTRUE(communication_result_is_stale(communication_result, seurat_obj))) {
    .communication_stop(
      "stale_against_current_seurat_object",
      sprintf("Echec %s : %s", context,
              communication_status_labels()[["stale_against_current_seurat_object"]])
    )
  }
  invisible(communication_result)
}

#' Le resultat communication est-il perime vis-a-vis de l'objet Seurat courant ?
#'
#' @param communication_result Resultat canonique.
#' @param seurat_obj Objet Seurat courant (ou tout objet a dimnames).
#' @return TRUE si les empreintes divergent, FALSE sinon, NA si l'identite du
#'   resultat ou de l'objet est indeterminable.
#' @export
communication_result_is_stale <- function(communication_result, seurat_obj) {
  if (is.null(communication_result)) return(NA)
  fp <- communication_result$object_identity$fingerprint %||% NULL
  if (is.null(fp) || is.null(seurat_obj)) return(NA)
  if (!exists("velocity_object_fingerprint", mode = "function")) {
    .communication_stop(
      "invalid_input",
      paste0("communication_result_is_stale() : R/sc/sc_velocity.R doit etre ",
             "sourced avant R/sc/sc_communication.R (velocity_object_fingerprint ",
             "est reutilise, jamais duplique).")
    )
  }
  !identical(fp, velocity_object_fingerprint(seurat_obj))
}

#' Finaliser le resultat communication canonique (Stage 11)
#'
#' Assemble l'objet canonique documente (voir en-tete de fichier) a partir de
#' la table canonique harmonisee (harmonize_communication_identities()) et QCee
#' (communication_import_qc()). Aucune inference, aucun remplissage : les
#' donnees absentes restent NA/NULL. La provenance est PRODUITE ici (regle 7
#' AGENTS.md) ; l'appelant l'append ensuite a l'etat partage — elle n'est
#' jamais reconstruite apres coup.
#'
#' @param canonical_table Table harmonisee + QC (champ table de
#'   communication_import_qc()).
#' @param source_method "cellchat", "cellphonedb" ou "liana" (un seul par
#'   resultat).
#' @param source_files Liste nommee des fichiers sources — NOMS ORIGINAUX
#'   uniquement, jamais les chemins locaux complets.
#' @param identity_column Colonne de metadonnees Seurat choisie.
#' @param identity_mapping data.frame label -> identite (ou NULL si absent).
#' @param identity_summary Liste de compteurs d'harmonisation (ou list()).
#' @param column_mapping Mapping canonical_field -> colonne source.
#' @param qc Liste de compteurs QC (communication_import_qc()$counts).
#' @param n_input_rows Nombre de lignes de la source avant conversion.
#' @param seurat_obj Objet Seurat courant. Seule l'identite est extraite ; tout
#'   objet a dimnames est acceptable (testabilite hors Shiny).
#' @param extra_warnings Avertissements supplementaires produits par
#'   l'orchestration (y compris les avertissements d'harmonisation et du QC,
#'   jamais caches ailleurs) — fusionnes, sans doublon.
#' @param external_consensus TRUE quand la table porte une valeur agregee
#'   INTER-METHODES calculee par la source elle-meme (aujourd'hui : `mean_rank`
#'   ou `aggregate_rank` de LIANA). L'application n'agrege RIEN — elle importe
#'   et MARQUE, pour que la nature de la valeur reste tracable dans le
#'   resultat (champ `is_external_consensus`) et pas seulement dans la
#'   documentation.
#' @param analysis_id Identifiant d'analyse.
#' @return L'objet canonique (champs documentes dans l'en-tete de fichier).
#' @export
finalize_communication_result <- function(
    canonical_table,
    source_method = c("cellchat", "cellphonedb", "liana"),
    source_files = list(),
    identity_column = NA_character_,
    identity_mapping = NULL,
    identity_summary = list(),
    column_mapping = list(),
    qc = list(),
    n_input_rows = NA_integer_,
    seurat_obj = NULL,
    extra_warnings = character(0),
    external_consensus = FALSE,
    analysis_id = "sc-communication-import",
    computation = c("import", "engine")
) {
  source_method <- match.arg(source_method)
  # ADDITIF (moteur CellChat, CC-3) : "import" = valeur historique, aucun
  # appel existant ne change de comportement. "engine" marque un resultat
  # CALCULE dans l'application par R/sc/sc_communication_engine.R (Path B).
  computation <- match.arg(computation)
  is_engine <- identical(computation, "engine")

  if (!is.data.frame(canonical_table) || nrow(canonical_table) == 0L) {
    .communication_stop(
      "invalid_input",
      paste0(
        "finalize_communication_result() : table canonique non vide requise ",
        "(harmonize_communication_identities() + communication_import_qc() ",
        "doivent etre appelees d'abord)."
      )
    )
  }
  if (!all(communication_contract_fields() %in% colnames(canonical_table))) {
    .communication_stop(
      "invalid_input",
      sprintf(
        paste0("finalize_communication_result() : champs canoniques absents ",
               "de la table : %s."),
        paste(setdiff(communication_contract_fields(),
                      colnames(canonical_table)), collapse = ", ")
      )
    )
  }
  if (!identical(unique(canonical_table$source_method), source_method)) {
    .communication_stop(
      "invalid_input",
      sprintf(
        paste0("finalize_communication_result() : la table melange des ",
               "sources (%s) alors qu'un resultat canonique ne peut porter ",
               "qu'UNE source (%s). Les scores de sources differentes ne ",
               "sont pas comparables."),
        paste(unique(canonical_table$source_method), collapse = ","),
        source_method
      )
    )
  }

  warnings_all <- unique(as.character(extra_warnings))

  result <- list()
  result$type <- "cell_cell_communication"
  result$status <- "valid"
  result$source_method <- source_method
  result$canonical_table <- canonical_table
  result$identity_column <- as.character(identity_column)[1L]
  result$identity_mapping <- identity_mapping
  result$identity_summary <- identity_summary
  result$column_mapping <- column_mapping
  result$qc <- qc
  result$input_summary <- list(
    source_method = source_method,
    files = source_files,
    n_rows_input = as.integer(n_input_rows),
    n_rows_canonical = as.integer(nrow(canonical_table))
  )
  result$object_identity <- build_object_identity_v2(seurat_obj)
  result$warnings <- warnings_all

  entry <- new_provenance_entry(
    analysis_id = analysis_id,
    method = if (is_engine) source_method else paste0("import_", source_method),
    parameters = list(
      source_method = source_method,
      source_files = source_files,
      identity_column = result$identity_column,
      n_rows_input = as.integer(n_input_rows),
      n_rows_canonical = as.integer(nrow(canonical_table)),
      n_labels_matched = identity_summary$n_labels_matched %||% NULL,
      n_labels_unmatched = identity_summary$n_labels_unmatched %||% NULL,
      n_rows_dropped_qc = qc$n_dropped_required_fields %||% NULL,
      n_self_interactions = qc$n_self_interactions %||% NULL,
      n_duplicate_interactions = qc$n_duplicate_interactions %||% NULL,
      column_mapping = column_mapping,
      object_fingerprint = result$object_identity$fingerprint,
      external_consensus = isTRUE(external_consensus)
    ),
    dataset = seurat_obj,
    cells_used = NULL,
    cells_excluded = NULL,
    seed = NULL,
    warnings = warnings_all
  )
  entry$analysis_type <- "cell_cell_communication"
  entry$status <- "valid"
  entry$import_only <- !is_engine
  entry$computation_path <- if (is_engine) "B" else "A"
  # Marqueur de nature de la valeur (route LIANA) : l'application n'agrege
  # jamais, mais elle doit DIRE quand la valeur importee est un agregat
  # inter-methodes calcule par la source.
  entry$is_external_consensus <- isTRUE(external_consensus)
  entry$timestamp_utc <- format(
    entry$timestamp, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"
  )

  result$provenance <- entry
  result$analysis_id <- analysis_id
  result$timestamp_utc <- entry$timestamp_utc

  result
}

#' Empreinte objet : REUTILISE velocity_object_fingerprint (pas de deuxieme
# formule d'empreinte dans le projet). Erreur explicite si la dependance
# d'ordre de sourcing n'est pas respectee.
.communication_object_fingerprint <- function(obj) {
  if (is.null(obj)) return(NULL)
  if (!exists("velocity_object_fingerprint", mode = "function")) {
    .communication_stop(
      "invalid_input",
      paste0("Empreinte objet communication : R/sc/sc_velocity.R doit etre ",
             "sourced avant R/sc/sc_communication.R (velocity_object_fingerprint ",
             "est reutilise, jamais duplique).")
    )
  }
  velocity_object_fingerprint(obj)
}

#' Resume d'import communication pour export CSV (Stage 11)
#'
#' Une ligne, colonnes stables : analyse, statut, source, fichiers, colonne
#' d'identites, compteurs d'harmonisation et QC, empreinte objet,
#' avertissements. Lecture directe de l'objet canonique — aucune deduction.
#'
#' @param communication_result Resultat canonique.
#' @return data.frame a une ligne, colonnes character.
#' @export
build_communication_import_summary <- function(communication_result) {
  r <- assert_communication_result(
    communication_result, context = "resume d'import communication"
  )
  p <- r$provenance %||% list()
  fp <- r$object_identity$fingerprint %||% NA_character_
  ids <- r$identity_summary %||% list()
  qc <- r$qc %||% list()

  .fmt_files <- function(files) {
    if (is.null(files) || length(files) == 0L) return(NA_character_)
    files <- as.list(files)
    paste(
      sprintf("%s=%s", names(files),
              vapply(files, paste, character(1), collapse = ",")),
      collapse = "; "
    )
  }
  .fmt_unmatched <- function(x) {
    if (is.null(x) || !length(x)) return(NA_character_)
    paste(utils::head(x, 20), collapse = "; ")
  }

  data.frame(
    analysis_id = r$analysis_id %||% NA_character_,
    analysis_type = r$type %||% "cell_cell_communication",
    status = r$status %||% NA_character_,
    timestamp_utc = r$timestamp_utc %||% NA_character_,
    source_method = r$source_method %||% NA_character_,
    source_files = .fmt_files(r$input_summary$files),
    identity_column = r$identity_column %||% NA_character_,
    n_rows_input = as.character(r$input_summary$n_rows_input %||% NA_integer_),
    n_rows_canonical = as.character(r$input_summary$n_rows_canonical %||% NA_integer_),
    n_identities = as.character(ids$n_identities %||% NA_integer_),
    n_labels = as.character(ids$n_labels %||% NA_integer_),
    n_labels_matched = as.character(ids$n_labels_matched %||% NA_integer_),
    n_labels_unmatched = as.character(ids$n_labels_unmatched %||% NA_integer_),
    unmatched_labels = .fmt_unmatched(ids$unmatched_labels),
    n_rows_dropped_qc = as.character(qc$n_dropped_required_fields %||% NA_integer_),
    n_self_interactions = as.character(qc$n_self_interactions %||% NA_integer_),
    n_duplicate_interactions = as.character(qc$n_duplicate_interactions %||% NA_integer_),
    n_pathway_missing = as.character(qc$n_pathway_missing %||% NA_integer_),
    n_p_value_out_of_range = as.character(qc$n_p_value_out_of_range %||% NA_integer_),
    object_fingerprint = fp,
    import_only = as.character(isTRUE(p$import_only)),
    warnings = paste(r$warnings %||% character(0), collapse = " | "),
    stringsAsFactors = FALSE
  )
}

#' Export CSV de la table d'harmonisation des identites (Stage 11)
#'
#' Une ligne par label source : roles ou il apparait, identite correspondante
#' (exact match uniquement) ou NA, trace par analysis_id. La table est
#' produite a l'import — elle n'est jamais reconstruite.
#'
#' @param communication_result Resultat canonique.
#' @return data.frame (mapping + analysis_id), ou erreur si mapping absent.
#' @export
build_communication_identity_mapping_export <- function(communication_result) {
  r <- assert_communication_result(
    communication_result, context = "export mapping identites"
  )
  m <- r$identity_mapping %||% NULL
  if (is.null(m) || !is.data.frame(m)) {
    .communication_stop(
      "invalid_input",
      paste0("Aucune table d'harmonisation disponible : identity_mapping est ",
             "produit par harmonize_communication_identities() et conserve ",
             "dans le resultat canonique.")
    )
  }
  data.frame(
    label = m$label,
    roles = m$roles,
    matched_identity = m$matched_identity,
    matched = m$matched,
    identity_column = rep(r$identity_column %||% NA_character_, nrow(m)),
    analysis_id = rep(r$analysis_id %||% NA_character_, nrow(m)),
    timestamp_utc = rep(r$timestamp_utc %||% NA_character_, nrow(m)),
    stringsAsFactors = FALSE
  )
}

#' Nom de fichier d'export trace par l'identifiant d'analyse (Stage 11)
#'
#' @param communication_result Resultat canonique.
#' @param kind Prefixe descriptif (ex. "communication_import_table").
#' @param ext Extension ("csv", "rds").
#' @return Chaine "<kind>_<analysis_id>_<date>.<ext>".
#' @export
communication_export_filename <- function(communication_result, kind, ext) {
  if (is.null(communication_result) || !is.list(communication_result)) {
    .communication_stop(
      "invalid_input",
      "communication_export_filename() : resultat communication canonique requis."
    )
  }
  aid <- communication_result$analysis_id %||% "sc-communication-import"
  sprintf(
    "%s_%s_%s.%s",
    as.character(kind)[1L],
    aid,
    format(Sys.Date(), "%Y-%m-%d"),
    as.character(ext)[1L]
  )
}

#' Surface publique figee de R/sc/sc_communication.R (Stage 11)
#'
#' Le test de freeze refuse toute fonction top-level non prefixee d'un point
#' qui ne figure pas dans cette liste : l'API communication ne peut evoluer
#' que de façon explicite (code + test + contrat documentaire en meme temps).
#'
#' @return Vecteur character des noms de fonctions publiques.
#' @export
communication_public_api <- function() {
  c(
    # Contrat / etats
    "communication_contract_fields", "communication_supported_sources",
    "communication_validity_states", "communication_status_labels",
    "communication_status_is_valid", "communication_error_state",
    "assert_communication_result", "communication_public_api",
    # Champs de mesure de rang (CCC 7-8 route (b))
    "communication_rank_fields", "communication_rank_aggregation_modes",
    # Parseurs par source
    "parse_cellchat_import", "parse_cellphonedb_import",
    "parse_cellchat_object", "parse_liana_import",
    # Harmonisation / QC / finalisation
    "harmonize_communication_identities", "communication_import_qc",
    "finalize_communication_result", "communication_result_is_stale",
    # Exports
    "build_communication_import_summary",
    "build_communication_identity_mapping_export",
    "communication_export_filename"
  )
}

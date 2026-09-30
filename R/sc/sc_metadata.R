# =============================================================================
# R/sc/sc_metadata.R — Déclaration du design expérimental (condition / réplicat)
# =============================================================================
# Contrat gelé : docs/contracts/SC_METADATA_CONTRACT.md
# Test de gel  : tests/testthat/test-sc-metadata-contract-freeze.R
# Unitaires    : tests/testthat/test-sc-metadata.R
#
# Pure domain logic (aucun symbole Shiny — vérifié par le test de gel).
#
# POURQUOI : aucun module n'écrit `condition` dans meta.data (audit
# 2026-09-27 §2.1, grep exhaustif) — les 4 plots par condition
# (mod_sc.R:798-851), le pseudobulk A-vs-B (sc_abundance_design.R:376-389),
# Milo/scCODA (mod_sc_da_design.R:152-157) et la communication par condition
# sont structurellement inaccessibles pour tout objet produit par l'app.
#
# Principe (décision design) : une CONDITION est une propriété de
# l'ÉCHANTILLON, jamais de la cellule (même règle que
# mod_sc_pseudobulk.R:310-313). La table design est donc par échantillon
# (niveau orig.ident), jamais par cellule.
#
# Zéro changement de comportement : ce module n'AJOUTE des colonnes à
# meta.data que sur commit explicite de l'utilisateur ; aucune analyse
# existante n'est modifiée.
# =============================================================================

#' Public API surface (frozen by the contract freeze test)
#' @return Character vector of exported function names.
sc_metadata_public_api <- function() {
  c(
    "sc_metadata_public_api", "sc_metadata_error_class", "sc_metadata_map_modes",
    "sc_metadata_sample_table", "sc_metadata_parse_sample_label",
    "sc_metadata_parse_sample_table", "sc_metadata_read_csv",
    "sc_metadata_join_csv", "sc_metadata_apply", "sc_metadata_design_recap"
  )
}

#' Classe d'erreur du domaine (gelée)
#' @return Character vector of error classes.
sc_metadata_error_class <- function() "sc_metadata_error"

.sc_metadata_stop <- function(msg) {
  errorCondition(msg, class = sc_metadata_error_class())
}

#' Modes de remplissage de la table design (gelé)
#' @return Character vector of modes.
sc_metadata_map_modes <- function() {
  c("manual", "parse_labels", "csv")
}

#' Construire la table design vide (une ligne par échantillon)
#' @param meta data.frame — meta.data de l'objet courant.
#' @param sample_col Colonne identifiant l'échantillon (défaut orig.ident).
#' @return data.frame(sample, condition, replicate), trié par sample.
sc_metadata_sample_table <- function(meta, sample_col = "orig.ident") {
  if (!is.data.frame(meta)) {
    stop(.sc_metadata_stop("meta doit être un data.frame (meta.data de l'objet)."))
  }
  if (!sample_col %in% colnames(meta)) {
    stop(.sc_metadata_stop(sprintf(
      "Colonne échantillon « %s » absente des métadonnées.", sample_col)))
  }
  samples <- sort(unique(trimws(as.character(meta[[sample_col]]))))
  samples <- samples[nzchar(samples)]
  data.frame(
    sample    = samples,
    condition = rep(NA_character_, length(samples)),
    replicate = rep(NA_character_, length(samples)),
    stringsAsFactors = FALSE
  )
}

#' Déduire {condition, réplicat} du nom d'un échantillon
#'
#' Deux conventions supportées (choix déclaré de l'utilisateur) :
#'   cond_position = "first" — "A_1", "A-1", "A.1", "B2" → condition "A"/"B", réplicat "1"/"2" ;
#'   cond_position = "last"  — "1a", "2b"                → condition "a"/"b",  réplicat "1"/"2".
#' Le SPLIT collé n'opère que sur une partie lettres D'UNE SEULE lettre : un nom
#' comme "patient1" est indéductible (le découper fabriquerait un réplicat).
#' Un nom sans paire déductible donne condition = nom, réplicat = NA.
#'
#' @param lbl Nom de l'échantillon (chaîne unique).
#' @param cond_position "first" (condition en préfixe) ou "last" (suffixe).
#' @return list(condition, replicate).
sc_metadata_parse_sample_label <- function(lbl, cond_position = c("first", "last")) {
  cond_position <- match.arg(cond_position)
  if (!is.character(lbl) || length(lbl) != 1L || is.na(lbl) || !nzchar(trimws(lbl))) {
    stop(.sc_metadata_stop("Nom d'échantillon invalide (chaîne non vide attendue)."))
  }
  lbl <- trimws(lbl)

  # 1. Séparateur explicite : A_1 / A-1 / A.1
  toks <- strsplit(lbl, "[^[:alnum:]]+")[[1]]
  toks <- toks[nzchar(toks)]
  if (length(toks) >= 2L) {
    if (cond_position == "first") {
      return(list(condition = toks[1], replicate = toks[2]))
    }
    return(list(condition = toks[length(toks)],
                replicate = toks[length(toks) - 1L]))
  }

  # 2. Collé : B2 / 1a — SPLIT uniquement si la partie LETTRES est UNE seule
  #    lettre (code de condition). Un nom comme « patient1 » (préfixe multi-
  #    lettres) est INDÉDUCTIBLE : le découper fabriquerait un réplicat.
  m <- regmatches(lbl, regexec("^([A-Za-z])([0-9]+)$", lbl))[[1]]
  if (length(m) == 3L) {
    pair <- if (cond_position == "first") c(m[2], m[3]) else c(m[3], m[2])
    return(list(condition = pair[1], replicate = pair[2]))
  }
  m <- regmatches(lbl, regexec("^([0-9]+)([A-Za-z])$", lbl))[[1]]
  if (length(m) == 3L) {
    pair <- if (cond_position == "first") c(m[2], m[3]) else c(m[3], m[2])
    return(list(condition = pair[1], replicate = pair[2]))
  }

  # 3. Indéductible : condition = nom, réplicat laissé vide (éditable)
  list(condition = lbl, replicate = NA_character_)
}

#' Remplir condition/réplicat de toute la table par parse des noms (mode parse_labels)
#' @param sample_tbl data.frame(sample, condition, replicate) — écrase les colonnes.
#' @param cond_position "first" ou "last".
#' @return La table remplie.
sc_metadata_parse_sample_table <- function(sample_tbl, cond_position = c("first", "last")) {
  cond_position <- match.arg(cond_position)
  if (!is.data.frame(sample_tbl) || !"sample" %in% colnames(sample_tbl)) {
    stop(.sc_metadata_stop("sample_tbl doit être un data.frame avec une colonne sample."))
  }
  cond <- character(0L)
  repl <- character(0L)
  for (s in as.character(sample_tbl$sample)) {
    p <- sc_metadata_parse_sample_label(s, cond_position)
    cond <- c(cond, p$condition)
    repl <- c(repl, p$replicate)
  }
  sample_tbl$condition <- cond
  sample_tbl$replicate <- repl
  sample_tbl
}

#' Lire un CSV de design (détection auto du séparateur , ; ou tabulation)
#' @param path Chemin du fichier.
#' @return data.frame (character, stringsAsFactors = FALSE).
sc_metadata_read_csv <- function(path) {
  seps <- c(",", ";", "\t")
  last_err <- NULL
  for (sep in seps) {
    tab <- tryCatch(
      read.table(path, sep = sep, header = TRUE, stringsAsFactors = FALSE,
                 quote = "\"'", comment.char = "", check.names = FALSE,
                 colClasses = "character"),
      error = function(e) { last_err <<- e; NULL }
    )
    if (!is.null(tab) && ncol(tab) >= 2L && nrow(tab) >= 1L) return(tab)
  }
  stop(.sc_metadata_stop(sprintf(
    "CSV de design illisible (séparateur , ; ou tabulation attendu) : %s.",
    if (is.null(last_err)) "aucune ligne/colonne exploitable" else last_err$message)))
}

#' Joindre un CSV de design à meta.data (clé = échantillon)
#' @param meta data.frame — meta.data de l'objet.
#' @param csv data.frame lu via sc_metadata_read_csv().
#' @param key_col Colonne du CSV portant le nom d'échantillon.
#' @param sample_col Colonne meta.data à joindre.
#' @param condition_col Colonne condition du CSV (défaut "condition").
#' @param replicate_col Colonne réplicat du CSV (optionnelle).
#' @return meta.data enrichi (condition obligatoire, replicate si fournie ;
#'   réplicat manquant ⇒ repli sur l'identifiant échantillon).
sc_metadata_join_csv <- function(meta, csv, key_col = "sample",
                                 sample_col = "orig.ident",
                                 condition_col = "condition",
                                 replicate_col = "replicate") {
  if (!is.data.frame(meta)) stop(.sc_metadata_stop("meta doit être un data.frame."))
  if (!is.data.frame(csv)) stop(.sc_metadata_stop("csv doit être un data.frame."))
  for (cc in c(key_col, condition_col)) {
    if (!cc %in% colnames(csv)) {
      stop(.sc_metadata_stop(sprintf("Colonne « %s » absente du CSV de design.", cc)))
    }
  }
  if (!sample_col %in% colnames(meta)) {
    stop(.sc_metadata_stop(sprintf(
      "Colonne échantillon « %s » absente des métadonnées.", sample_col)))
  }
  keys <- trimws(as.character(csv[[key_col]]))
  if (anyDuplicated(keys)) {
    stop(.sc_metadata_stop(sprintf(
      "Clés dupliquées dans le CSV de design : %s.",
      paste(unique(keys[duplicated(keys)]), collapse = ", "))))
  }
  ids <- trimws(as.character(meta[[sample_col]]))
  missing <- setdiff(unique(ids), keys)
  if (length(missing)) {
    stop(.sc_metadata_stop(sprintf(
      "Échantillons absents du CSV de design : %s.",
      paste(missing, collapse = ", "))))
  }
  idx <- match(ids, keys)
  meta$condition <- factor(trimws(as.character(csv[[condition_col]])[idx]))
  if (replicate_col %in% colnames(csv)) {
    repl <- trimws(as.character(csv[[replicate_col]])[idx])
    meta$replicate <- ifelse(is.na(repl) | !nzchar(repl), ids, repl)
  }
  meta
}

#' Appliquer la table design (mode manual/parse_labels) à meta.data
#' @param meta data.frame — meta.data de l'objet.
#' @param sample_tbl data.frame(sample, condition, replicate) édité côté UI.
#' @param sample_col Colonne meta.data portant l'échantillon.
#' @return meta.data enrichi. Réplicat vide ⇒ repli sur l'identifiant échantillon
#'   (l'échantillon EST le réplicat, convention du module DA).
sc_metadata_apply <- function(meta, sample_tbl, sample_col = "orig.ident") {
  if (!is.data.frame(meta)) stop(.sc_metadata_stop("meta doit être un data.frame."))
  if (!is.data.frame(sample_tbl) ||
      !all(c("sample", "condition", "replicate") %in% colnames(sample_tbl))) {
    stop(.sc_metadata_stop("sample_tbl doit contenir sample, condition, replicate."))
  }
  if (anyDuplicated(sample_tbl$sample)) {
    stop(.sc_metadata_stop("Doublons dans la table design (une ligne par échantillon)."))
  }
  if (!sample_col %in% colnames(meta)) {
    stop(.sc_metadata_stop(sprintf(
      "Colonne échantillon « %s » absente des métadonnées.", sample_col)))
  }
  tbl <- sample_tbl
  tbl$sample    <- trimws(as.character(tbl$sample))
  tbl$condition <- trimws(as.character(tbl$condition))
  tbl$replicate <- trimws(as.character(tbl$replicate))
  tbl$condition[tbl$condition %in% c("", "NA")] <- NA_character_
  tbl$replicate[tbl$replicate %in% c("", "NA")] <- NA_character_
  if (any(is.na(tbl$condition))) {
    stop(.sc_metadata_stop(sprintf(
      "Condition manquante pour : %s — renseignez la table design avant d'appliquer.",
      paste(tbl$sample[is.na(tbl$condition)], collapse = ", "))))
  }
  ids <- trimws(as.character(meta[[sample_col]]))
  missing <- setdiff(unique(ids), tbl$sample)
  if (length(missing)) {
    stop(.sc_metadata_stop(sprintf(
      "Échantillons sans design déclaré : %s.",
      paste(missing, collapse = ", "))))
  }
  idx <- match(ids, tbl$sample)
  meta$condition <- factor(tbl$condition[idx])
  repl <- tbl$replicate[idx]
  meta$replicate <- ifelse(is.na(repl), ids, repl)
  meta
}

#' Récapitulatif du design déclaré (informe AVANT commit — le blocage dur
#' reste dans validate_da_design / le module pseudobulk)
#' @param sample_tbl data.frame(sample, condition, replicate).
#' @param cells_per_sample named numeric/table optionnel (n cellules par échantillon).
#' @return list(recap = data.frame, ok = logical, blockers = character).
sc_metadata_design_recap <- function(sample_tbl, cells_per_sample = NULL) {
  tbl <- sample_tbl
  tbl$condition <- trimws(as.character(tbl$condition))
  tbl$condition[tbl$condition %in% c("", "NA")] <- NA_character_
  tbl$replicate <- trimws(as.character(tbl$replicate))
  tbl$replicate[tbl$replicate %in% c("", "NA")] <- NA_character_

  n_conditions <- length(unique(na.omit(tbl$condition)))
  blockers <- character(0L)
  if (n_conditions < 2L) {
    blockers <- c(blockers, sprintf(
      "%d seule(s) condition(s) déclarée(s) — une analyse A-vs-B exige AU MOINS 2 conditions.",
      n_conditions))
  }
  if (any(is.na(tbl$condition))) {
    blockers <- c(blockers, sprintf(
      "Condition manquante pour : %s.",
      paste(tbl$sample[is.na(tbl$condition)], collapse = ", ")))
  }
  has_repl <- !all(is.na(tbl$replicate))
  min_reps <- if (exists("TS_DA_MIN_REPLICATES_PER_CONDITION")) {
    TS_DA_MIN_REPLICATES_PER_CONDITION
  } else 2L

  conds <- sort(unique(na.omit(tbl$condition)))
  if (!length(conds)) {
    return(list(
      recap = data.frame(condition = character(0), n_samples = integer(0),
                         n_replicates = integer(0), n_cells = integer(0),
                         stringsAsFactors = FALSE),
      ok = FALSE,
      blockers = c(blockers, "Aucune condition renseignée.")
    ))
  }
  rows <- lapply(conds, function(cv) {
    st <- tbl[!is.na(tbl$condition) & tbl$condition == cv, , drop = FALSE]
    reps <- unique(stats::na.omit(st$replicate))
    cells <- if (!is.null(cells_per_sample)) {
      hit <- names(cells_per_sample) %in% st$sample
      sum(as.numeric(cells_per_sample[hit]))
    } else NA_real_
    data.frame(
      condition     = cv,
      n_samples     = nrow(st),
      n_replicates  = if (has_repl) length(reps) else NA_integer_,
      n_cells       = if (is.na(cells)) NA_integer_ else as.integer(cells),
      stringsAsFactors = FALSE
    )
  })
  recap <- do.call(rbind, rows)

  if (has_repl && any(recap$n_replicates < min_reps)) {
    bad <- recap$condition[recap$n_replicates < min_reps]
    blockers <- c(blockers, sprintf(
      "Pseudoreplication : %s — moins de %d réplicat(s)/condition (plancher TS_DA_MIN_REPLICATES_PER_CONDITION).",
      paste(bad, collapse = ", "), min_reps))
  }
  list(recap = recap, ok = length(blockers) == 0L, blockers = blockers)
}

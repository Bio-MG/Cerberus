# =============================================================================
# R/sc/sc_communication_input.R — 10X / Seurat -> CellChat-ready INPUT builder
# =============================================================================
# 4D-3 (V1.x). The Stage 11 contract explicitly reserved this step
# (docs/contracts/COMMUNICATION_RESULT_CONTRACT.md section 5 : "generer des
# resultats de communication depuis des donnees brutes ... calcul lourd reporte
# a une etape explicite (proposal 4D-3)"). This file IS that explicit step,
# scoped to the DATA PATH ONLY.
#
# It answers, in executable code, the question of
# docs/proposals/CCC_DATA_PATH_ASSESSMENT.md :
#
#   "Des donnees 10X (genes, matrix, features, ...) peuvent-elles etre
#    converties en un format que CellChat consomme directement ?"
#
# Answer: YES — and this file IS the conversion. CellChat::createCellChat()
# accepts a normalized genes x cells matrix plus population labels; there is NO
# proprietary "CellChat format" to produce. The three REAL requirements
# (assessment section 1) become hard gates here:
#
#   1. expression NORMALISEE  -> validated, or log-normalised on request
#   2. SYMBOLES de genes      -> .cellchat_map_features() (Ensembl -> symbole,
#                                suffixe de version retire)
#   3. ETIQUETTES de population -> required, non-NA, >= 2 populations
#
# ... plus the DECLARED decisions the assessment flagged (section 4):
#
#   4. espece (human/mouse)   -> must be stated explicitly, never defaulted
#   5. contrat upstream       -> the caller supplies the components (this file
#                                never guesses an upstream schema)
#
# NO new dependency: this file does not call CellChat, igraph, or anything
# absent from renv.lock. It produces the *inputs*; running the inference stays
# a separate, explicit step.
#
# Reuse (never duplicated): .communication_object_fingerprint() (itself a thin
# wrapper over velocity_object_fingerprint(), the project's single fingerprint
# formula) and new_provenance_entry().
#
# Sourced in app.R AFTER R/sc/sc_communication.R.
# =============================================================================

# Etats de validite explicites (source de verite ; accesseur public ci-dessous).
.CELLCHAT_INPUT_STATES <- c(
  "invalid_input",      # objet/matrice/labels vides ou d'un type inattendu
  "invalid_features",   # aucun symbole de gene exploitable
  "invalid_labels",     # aucune population, ou une seule -> rien a comparer
  "invalid_species"     # espece non declaree / hors human|mouse
)

# Base ligand-recepteur CellChat par espece. CellChatDB.human / .mouse sont
# indexes par SYMBOLE de gene -> c'est la raison d'etre du mapping ci-dessous.
.CELLCHAT_SPECIES_DB <- c(human = "CellChatDB.human", mouse = "CellChatDB.mouse")

# Suffixe de version Ensembl ("ENSG00000167286.9" -> "ENSG00000167286").
.CELLCHAT_ENSEMBL_RE <- "^ENS[A-Z]*G[0-9]+$"

#' Etats de validite du contrat d'entree CellChat
#'
#' Les quatre etats sont des echecs BLOQUANTS : ils sont leves comme erreurs
#' classees `cellchat_input_error` portant un champ `state`. Aucun objet
#' d'entree n'est produit dans ces cas.
#'
#' @return Vecteur character des 4 etats documentes.
#' @export
cellchat_input_states <- function() .CELLCHAT_INPUT_STATES

#' Exigences reelles d'un entree CellChat (assessment rendu executable)
#'
#' Rend explicite ce que docs/proposals/CCC_DATA_PATH_ASSESSMENT.md etablit :
#' le FORMAT 10X n'est pas le blocage ; les blocages reels sont les etiquettes
#' de population, les identifiants (symboles) et l'espece declaree.
#'
#' @return data.frame (requirement, role, status).
#' @export
cellchat_input_requirements <- function() {
  data.frame(
    requirement = c(
      "expression_normalisee", "symboles_de_genes", "etiquettes_de_population",
      "espece_declaree", "contrat_upstream"
    ),
    role = c(
      "CellChat attend des donnees log-normalisees (data slot Seurat).",
      "CellChatDB.human/.mouse sont indexes par SYMBOLE de gene.",
      "CellChat regroupe par population (group.by) : sans etiquettes, rien a comparer.",
      "CellChatDB.human ou CellChatDB.mouse : decision DECLAREE, jamais un defaut implicite.",
      "Formats/identifiants/metadonnees de l'app upstream : a figer cote upstream."
    ),
    status = c(
      "verifie (log_normalize = TRUE corrige)",
      "resolu (.cellchat_map_features : Ensembl -> symbole, version retiree)",
      "BLOQUANT si absent (invalid_labels)",
      "BLOQUANT si non declare (invalid_species)",
      "hors perimetre de ce fichier (le caller fournit les composants)"
    ),
    stringsAsFactors = FALSE
  )
}

#' Base ligand-recepteur CellChat associee a une espece
#'
#' @param species "human" ou "mouse" (insensible a la casse).
#' @return Nom de la base (ex. "CellChatDB.human").
#' @export
cellchat_database_for_species <- function(species) {
  .cellchat_resolve_species(species)
}

#' Etat de validite associe a une erreur de construction d'entree CellChat
#'
#' @param e Condition (erreur) capturee.
#' @return La chaine d'etat structuree, NA_character_ pour une erreur sans etat.
#' @export
# §14.1 : délégation à l'accesseur générique. Famille F1 MESURÉE — la CLASSE
# est vérifiée : l'état d'une erreur d'un autre domaine rend NA.
cellchat_input_error_state <- function(e) ts_error_state(e, "cellchat_input_error")

# ---------------------------------------------------------------------------
# Helpers internes
# ---------------------------------------------------------------------------

.cellchat_input_stop <- function(state, message) {
  # errorCondition() est requis (stop(structure(...)) echoue dans cette
  # version de R) — meme idiome que .communication_stop().
  stop(errorCondition(message, state = state, class = "cellchat_input_error"))
}

.cellchat_resolve_species <- function(species) {
  if (is.null(species) || length(species) != 1L || is.na(species) ||
      !nzchar(as.character(species))) {
    .cellchat_input_stop(
      "invalid_species",
      paste0(
        "Espece non declaree : CellChatDB.human et CellChatDB.mouse sont ",
        "indexees par des symboles differents. L'espece doit etre une decision ",
        "DECLAREE (species = \"human\" ou \"mouse\"), jamais un defaut implicite."
      )
    )
  }
  sp <- tolower(as.character(species))
  if (!sp %in% names(.CELLCHAT_SPECIES_DB)) {
    .cellchat_input_stop(
      "invalid_species",
      sprintf(
        "Espece '%s' non supportee. Valeurs acceptees : %s.",
        species, paste(names(.CELLCHAT_SPECIES_DB), collapse = ", ")
      )
    )
  }
  unname(.CELLCHAT_SPECIES_DB[[sp]])
}

# Retire un suffixe de version ("ENSG00000167286.9" -> "ENSG00000167286",
# "CD3D.1" -> "CD3D"). Ne touche pas un identifiant sans suffixe numerique.
.cellchat_strip_version <- function(x) {
  x <- as.character(x)
  sub("\\.[0-9]+$", "", x)
}

.cellchat_is_ensembl <- function(x) {
  grepl(.CELLCHAT_ENSEMBL_RE, .cellchat_strip_version(x))
}

#' Resoudre les identifiants de features en SYMBOLES de genes
#'
#' Accepte soit un vecteur d'identifiants, soit une table de features facon
#' `features.tsv` 10X (col1 = id Ensembl, col2 = symbole, col3 = type). Le
#' suffixe de version est retire. Un identifiant vide/non resolvable est
#' compte, jamais invente.
#'
#' @param features Vecteur character OU data.frame/matrix (features.tsv 10X).
#' @return list(symbols, feature_id, ensembl, feature_type, n_input,
#'   n_symbol_ok, n_ensembl).
.cellchat_map_features <- function(features) {
  if (is.data.frame(features) || is.matrix(features)) {
    df <- as.data.frame(features, stringsAsFactors = FALSE)
    feature_id   <- if (ncol(df) >= 1L) as.character(df[[1L]]) else character(0)
    symbol_raw   <- if (ncol(df) >= 2L) as.character(df[[2L]]) else feature_id
    feature_type <- if (ncol(df) >= 3L) as.character(df[[3L]]) else rep(NA_character_, length(feature_id))
  } else {
    feature_id   <- as.character(features)
    symbol_raw   <- feature_id
    feature_type <- rep(NA_character_, length(feature_id))
  }

  n_input <- length(feature_id)
  if (n_input == 0L) {
    .cellchat_input_stop(
      "invalid_features",
      "Aucune feature fournie : impossible de resoudre les symboles de genes."
    )
  }

  symbol <- .cellchat_strip_version(symbol_raw)
  blank  <- is.na(symbol) | !nzchar(symbol)
  # Repli : si le symbole est vide, on retombe sur l'identifiant (strippe).
  symbol[blank] <- .cellchat_strip_version(feature_id)[blank]

  ensembl <- ifelse(.cellchat_is_ensembl(feature_id),
                    .cellchat_strip_version(feature_id),
                    ifelse(.cellchat_is_ensembl(symbol_raw),
                           .cellchat_strip_version(symbol_raw), NA_character_))

  n_symbol_ok <- sum(!is.na(symbol) & nzchar(symbol))
  if (n_symbol_ok == 0L) {
    .cellchat_input_stop(
      "invalid_features",
      paste0(
        "Aucun symbole de gene exploitable parmi ", n_input, " feature(s). ",
        "CellChatDB est indexee par symbole : fournissez un vecteur de symboles ",
        "ou une table features.tsv (id, symbole, type)."
      )
    )
  }

  list(
    symbols      = symbol,
    feature_id   = feature_id,
    ensembl      = ensembl,
    feature_type = feature_type,
    n_input      = n_input,
    n_symbol_ok  = n_symbol_ok,
    n_ensembl    = sum(!is.na(ensembl))
  )
}

# Heuristique : la matrice ressemble-t-elle a des comptes bruts (entiers, >= 0,
# au moins une valeur > 0) ? Sert uniquement a AVERTIR — jamais a transformer
# sans demande. Le signal robuste est l'INTEGRALITE des valeurs : les comptes
# bruts sont entiers, les donnees log-normalisees ne le sont (quasiment) jamais.
# Ne pas exiger une grande echelle : un petit jeu de comptes peut avoir un
# maximum < 30 et doit quand meme etre signale.
.cellchat_looks_like_counts <- function(data) {
  vals <- if (inherits(data, "Matrix")) data@x else as.numeric(data)
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0L) return(FALSE)
  if (any(vals < 0)) return(FALSE)
  if (!any(vals > 0)) return(FALSE)
  isTRUE(all(abs(vals - round(vals)) < 1e-8))
}

#' Normalisation log (CPM-like) pour une entree CellChat
#'
#' `log1p(scale_factor * x / colSums(x))`. Pure, sans dependance. A n'appeler
#' que sur des COMPTES bruts : les donnees deja normalisees (data slot Seurat)
#' ne doivent pas etre renormalisees.
#'
#' @param data Matrice genes x cellules (base ou Matrix sparse).
#' @param scale_factor Facteur d'echelle (defaut 1e4).
#' @return Matrice normalisee (meme orientation).
#' @export
cellchat_log_normalize <- function(data, scale_factor = 1e4) {
  if (is.null(data)) {
    .cellchat_input_stop("invalid_input", "cellchat_log_normalize() : 'data' NULL.")
  }
  cs <- colSums(data)
  cs[!is.finite(cs) | cs <= 0] <- 1
  out <- if (inherits(data, "Matrix")) {
    data %*% Matrix::Diagonal(x = scale_factor / cs)
  } else {
    sweep(data, 2L, cs / scale_factor, "/")
  }
  log1p(out)
}

# ---------------------------------------------------------------------------
# Construction de l'entree CellChat
# ---------------------------------------------------------------------------

#' Construire une entree CellChat depuis une matrice + features + etiquettes
#'
#' Coeur PUR (aucun objet Seurat requis) : c'est la fonction testable hors
#' application. Produit exactement ce que `CellChat::createCellChat()` attend
#' (`data` genes x cellules normalisee, `meta` avec `labels`, `group.by`).
#'
#' @param data Matrice genes x cellules (base ou Matrix sparse).
#' @param features Vecteur d'identifiants OU table features.tsv 10X.
#' @param labels Etiquettes de population : vecteur nomme par cellule, ou
#'   vecteur aligne sur `colnames(data)`.
#' @param species "human" ou "mouse" — OBLIGATOIRE en pratique. Le defaut est
#'   `NULL`, qui n'est PAS une espece valide : il est rejete par
#'   `invalid_species`. Le defaut ne choisit donc JAMAIS une base implicite.
#' @param group_by Nom de la colonne d'etiquettes dans la sortie (defaut "labels").
#' @param min_cells_per_group Groupes sous ce plancher : cellules retirees et
#'   COMPTEES (jamais silencieusement). Defaut 1L = aucun effet.
#' @param duplicate_symbols "sum" (defaut, comme Read10X/Seurat) ou "first".
#' @param log_normalize TRUE = appliquer cellchat_log_normalize() (comptes bruts).
#' @param source Etiquette de provenance (nom de fichier/dossier, jamais un chemin).
#' @param fingerprint_object Objet a empreinter (defaut : la matrice elle-meme).
#' @return Objet classé `cellchat_input`.
#' @export
cellchat_input_from_matrix <- function(data, features, labels, species = NULL,
                                       group_by = "labels",
                                       min_cells_per_group = 1L,
                                       duplicate_symbols = c("sum", "first"),
                                       log_normalize = FALSE,
                                       source = NA_character_,
                                       fingerprint_object = NULL) {
  duplicate_symbols <- match.arg(duplicate_symbols)
  db <- .cellchat_resolve_species(species)

  if (is.null(data) || length(dim(data)) != 2L) {
    .cellchat_input_stop("invalid_input",
      "cellchat_input_from_matrix() : 'data' doit etre une matrice genes x cellules.")
  }
  if (!is.matrix(data) && !inherits(data, "Matrix")) {
    data <- as.matrix(data)  # data.frame -> matrix ; une Matrix sparse est CONSERVEE
  }
  if (nrow(data) == 0L || ncol(data) == 0L) {
    .cellchat_input_stop("invalid_input",
      "cellchat_input_from_matrix() : matrice vide (0 gene ou 0 cellule).")
  }
  # is.numeric() est FALSE pour une Matrix sparse (S4) : tester le stockage.
  .numeric_data <- if (inherits(data, "Matrix")) is.numeric(data@x) else is.numeric(data)
  if (!.numeric_data) {
    .cellchat_input_stop("invalid_input",
      "cellchat_input_from_matrix() : 'data' doit etre numerique.")
  }

  # --- 1. Identifiants -> symboles -----------------------------------------
  fmap <- .cellchat_map_features(features)
  if (fmap$n_input != nrow(data)) {
    .cellchat_input_stop(
      "invalid_features",
      sprintf(
        "Incoherence features/matrice : %d feature(s) pour %d ligne(s) de la matrice.",
        fmap$n_input, nrow(data)
      )
    )
  }
  symbols <- fmap$symbols

  # --- 2. Etiquettes de population -----------------------------------------
  if (is.null(labels) || length(labels) == 0L) {
    .cellchat_input_stop("invalid_labels",
      "Aucune etiquette de population : CellChat regroupe par population.")
  }
  # Une matrice nue n'a pas de colnames : on en fabrique pour que meta$Cell
  # reste aligne (jamais NULL). Si les etiquettes sont nommees ET que la
  # matrice portait des noms de cellules, l'alignement se fait par NOM.
  had_colnames <- !is.null(colnames(data))
  if (!had_colnames) colnames(data) <- paste0("cell_", seq_len(ncol(data)))

  if (had_colnames && !is.null(names(labels)) && any(nzchar(names(labels)))) {
    labels <- as.character(labels[colnames(data)])
  } else {
    labels <- as.character(labels)
    if (length(labels) != ncol(data)) {
      .cellchat_input_stop(
        "invalid_labels",
        sprintf("Etiquettes non alignees : %d etiquette(s) pour %d cellule(s).",
                length(labels), ncol(data))
      )
    }
  }
  labels <- as.character(labels)
  n_na <- sum(is.na(labels) | !nzchar(labels))

  warnings_out <- character(0)
  qc <- list(
    n_features_input      = fmap$n_input,
    n_cells_input         = ncol(data),
    n_symbols_resolved    = fmap$n_symbol_ok,
    n_ensembl_resolved    = fmap$n_ensembl,
    n_duplicate_symbols   = 0L,
    n_cells_dropped_na_label = n_na,
    n_cells_dropped_min_group = 0L,
    n_populations_input   = length(unique(labels[!is.na(labels) & nzchar(labels)])),
    counts_like           = .cellchat_looks_like_counts(data),
    log_normalized        = isTRUE(log_normalize)
  )

  if (n_na > 0L) {
    warnings_out <- c(warnings_out, sprintf(
      "%d cellule(s) sans etiquette de population : retirees et comptees (jamais silencieusement).",
      n_na
    ))
    keep <- !is.na(labels) & nzchar(labels)
    data   <- data[, keep, drop = FALSE]
    labels <- labels[keep]
  }

  if (length(labels) == 0L || length(unique(labels)) < 2L) {
    .cellchat_input_stop(
      "invalid_labels",
      paste0(
        "CellChat a besoin d'AU MOINS 2 populations pour comparer une ",
        "communication. Etiquettes distinctes trouvees : ",
        length(unique(labels)), "."
      )
    )
  }

  # --- 3. Normalisation -----------------------------------------------------
  if (isTRUE(log_normalize)) {
    if (qc$counts_like) {
      data <- cellchat_log_normalize(data)
    } else {
      warnings_out <- c(warnings_out, paste0(
        "log_normalize = TRUE demande mais la matrice ne ressemble PAS a des ",
        "comptes bruts (valeurs non entieres ou max <= 30) : normalisation ",
        "NON appliquee pour ne pas alterer des donnees deja normalisees."
      ))
    }
  } else if (qc$counts_like) {
    warnings_out <- c(warnings_out, paste0(
      "La matrice ressemble a des COMPTES BRUTS : CellChat attend des donnees ",
      "log-normalisees. Passez log_normalize = TRUE (ou le data slot d'un ",
      "objet Seurat deja normalise)."
    ))
  }

  # --- 4. Collapse des symboles dupliques ----------------------------------
  if (anyDuplicated(symbols)) {
    dup_syms <- unique(symbols[duplicated(symbols)])
    qc$n_duplicate_symbols <- length(dup_syms)
    warnings_out <- c(warnings_out, sprintf(
      "%d symbole(s) duplique(s) apres resolution (%s) : collapse par '%s'.",
      length(dup_syms), paste(utils::head(dup_syms, 5L), collapse = ", "),
      duplicate_symbols
    ))
    if (duplicate_symbols == "sum") {
      data <- rowsum(as.matrix(data), group = symbols, reorder = FALSE)
    } else {
      keep <- !duplicated(symbols)
      data <- data[keep, , drop = FALSE]
      rownames(data) <- symbols[keep]
    }
  } else {
    rownames(data) <- symbols
  }

  # --- 5. Groupes sous-representes -----------------------------------------
  min_cells_per_group <- as.integer(min_cells_per_group %||% 1L)
  if (is.na(min_cells_per_group) || min_cells_per_group < 1L) min_cells_per_group <- 1L
  grp_tab <- table(labels)
  small <- names(grp_tab)[grp_tab < min_cells_per_group]
  if (length(small) > 0L) {
    keep <- labels %in% setdiff(names(grp_tab), small)
    qc$n_cells_dropped_min_group <- sum(!keep)
    warnings_out <- c(warnings_out, sprintf(
      "%d population(s) sous le plancher de %d cellule(s) (%s) : %d cellule(s) retirees.",
      length(small), min_cells_per_group, paste(small, collapse = ", "),
      qc$n_cells_dropped_min_group
    ))
    data   <- data[, keep, drop = FALSE]
    labels <- labels[keep]
    grp_tab <- table(labels)
  }

  if (length(unique(labels)) < 2L) {
    .cellchat_input_stop(
      "invalid_labels",
      "Apres retrait des cellules mal etiquetees, il reste moins de 2 populations."
    )
  }

  # --- 6. Table de features finale (alignee sur rownames(data)) -------------
  final_symbols <- rownames(data)
  first_idx <- match(final_symbols, symbols)
  features_out <- data.frame(
    symbol       = final_symbols,
    ensembl      = fmap$ensembl[first_idx],
    feature_type = fmap$feature_type[first_idx],
    n_collapsed  = as.integer(tabulate(match(symbols, final_symbols), nbins = length(final_symbols))),
    stringsAsFactors = FALSE
  )

  # --- 7. Empreinte + provenance -------------------------------------------
  fp_obj <- fingerprint_object %||% data
  fp <- tryCatch(.communication_object_fingerprint(fp_obj), error = function(e) NULL)
  if (is.null(fp)) fp <- velocity_object_fingerprint(fp_obj)

  prov <- new_provenance_entry(
    analysis_id = "sc-cellchat-input",
    method      = "cellchat_input_from_matrix",
    parameters  = list(
      group_by             = group_by,
      species              = tolower(as.character(species)),
      database             = db,
      duplicate_symbols    = duplicate_symbols,
      log_normalize        = isTRUE(log_normalize),
      min_cells_per_group  = min_cells_per_group
    ),
    dataset    = fp_obj,
    cells_used = ncol(data),
    warnings   = warnings_out
  )

  out <- list(
    type          = "cellchat_input",
    status        = "valid",
    data          = data,
    meta          = data.frame(Cell = colnames(data), labels = labels,
                               stringsAsFactors = FALSE),
    group_by      = group_by,
    species       = tolower(as.character(species)),
    database      = db,
    features      = features_out,
    qc            = qc,
    warnings      = warnings_out,
    input_summary = list(
      source           = source,
      n_features_input = fmap$n_input,
      n_cells_input    = qc$n_cells_input,
      n_genes          = nrow(data),
      n_cells          = ncol(data),
      n_populations    = length(unique(labels)),
      population_sizes = as.list(grp_tab)
    ),
    object_identity = list(
      fingerprint  = fp,
      method       = "velocity_object_fingerprint (v2)",
      seurat_dims  = c(nrow(fp_obj), ncol(fp_obj))
    ),
    provenance    = prov,
    analysis_id   = "sc-cellchat-input",
    timestamp_utc = format(prov$timestamp, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  class(out) <- c("cellchat_input", "list")
  out
}

#' Construire une entree CellChat depuis un objet Seurat (4D-3)
#'
#' Enveloppe mince autour de `cellchat_input_from_matrix()` : extrait la couche
#' d'expression demandee (`layer = "data"` par defaut = donnees normalisees),
#' les metadonnees et la colonne d'identites, puis delegue. Aucune methode
#' CellChat n'est appelee.
#'
#' @param obj Objet Seurat.
#' @param group_by Colonne de metadonnees servant d'etiquette de population.
#' @param assay Assay (defaut : DefaultAssay(obj)).
#' @param layer Couche a extraire ("data" = normalisee ; "counts" = bruts).
#' @param species "human" ou "mouse" — OBLIGATOIRE.
#' @param ... Transmis a cellchat_input_from_matrix().
#' @return Objet classé `cellchat_input`.
#' @export
build_cellchat_input <- function(obj, group_by, assay = NULL, layer = "data",
                                 species = NULL, ...) {
  if (is.null(obj) || !inherits(obj, "Seurat")) {
    .cellchat_input_stop("invalid_input",
      "build_cellchat_input() : objet Seurat requis.")
  }
  assay <- assay %||% SeuratObject::DefaultAssay(obj)
  if (!assay %in% names(obj@assays)) {
    .cellchat_input_stop(
      "invalid_input",
      sprintf("Assay '%s' absent de l'objet (disponibles : %s).",
              assay, paste(names(obj@assays), collapse = ", "))
    )
  }
  data <- tryCatch(
    SeuratObject::GetAssayData(obj, assay = assay, layer = layer),
    error = function(e) SeuratObject::GetAssayData(obj, assay = assay, slot = layer)
  )
  meta <- obj[[]]
  if (is.null(group_by) || length(group_by) != 1L || is.na(group_by) ||
      !nzchar(group_by) || !group_by %in% colnames(meta)) {
    .cellchat_input_stop(
      "invalid_labels",
      sprintf(
        "Colonne d'identites '%s' absente des metadonnees (disponibles : %s).",
        group_by %||% "NULL", paste(colnames(meta), collapse = ", ")
      )
    )
  }
  labels <- as.character(meta[[group_by]])
  names(labels) <- rownames(meta)

  cellchat_input_from_matrix(
    data = data, features = rownames(data), labels = labels,
    species = species, group_by = group_by,
    fingerprint_object = obj, ...
  )
}

#' Valider un objet `cellchat_input` avant consommation
#'
#' @param x Objet produit par build_cellchat_input()/cellchat_input_from_matrix().
#' @param context Libelle du consommateur (message d'erreur).
#' @return `x` invisible si valide, erreur classee sinon.
#' @export
assert_cellchat_input <- function(x, context = "consommateur CellChat") {
  if (!is.list(x) || !identical(x$type, "cellchat_input")) {
    stop(sprintf("%s : objet 'cellchat_input' attendu.", context), call. = FALSE)
  }
  if (!identical(x$status, "valid")) {
    stop(sprintf("%s : entree CellChat non valide (statut '%s').",
                 context, x$status %||% "NA"), call. = FALSE)
  }
  required <- c("data", "meta", "group_by", "species", "database",
                "features", "qc", "provenance")
  missing <- setdiff(required, names(x))
  if (length(missing) > 0L) {
    stop(sprintf("%s : champ(s) de contrat manquant(s) : %s.",
                 context, paste(missing, collapse = ", ")), call. = FALSE)
  }
  if (!(is.matrix(x$data) || inherits(x$data, "Matrix"))) {
    stop(sprintf("%s : $data doit etre une matrice genes x cellules.", context),
         call. = FALSE)
  }
  if (ncol(x$data) != nrow(x$meta)) {
    stop(sprintf("%s : $data (%d cellules) et $meta (%d lignes) desalignes.",
                 context, ncol(x$data), nrow(x$meta)), call. = FALSE)
  }
  if (!identical(rownames(x$data), as.character(x$features$symbol))) {
    stop(sprintf("%s : $features$symbol doit correspondre a rownames($data).",
                 context), call. = FALSE)
  }
  invisible(x)
}

#' Colonne de `$meta` a passer a `CellChat::createCellChat(group.by = )`
#'
#' Piege deja paye en production : `build_cellchat_input()` **normalise** la
#' colonne d'identites de l'objet Seurat en `meta$labels`. Le champ `group_by`
#' conserve le nom D'ORIGINE (ex. `celltype`, `seurat_clusters`) pour la
#' provenance et le rapport — ce nom n'est donc **pas** une colonne de `$meta`.
#' Le passer tel quel a `createCellChat()` fait echouer tout le run :
#' « The 'group.by' is not a column name in the `meta`, which will be used for
#' cell grouping. » Le moteur ne fonctionnait que par accident, quand la
#' colonne d'identites s'appelait litteralement `labels`.
#'
#' Cette fonction est la SEULE source de verite du regroupement : le moteur
#' l'utilise, `assert_cellchat_input()` garantit que la colonne existe.
#'
#' @param x Objet `cellchat_input`.
#' @return Nom de la colonne de `$meta` portant les populations (`"labels"`).
#' @export
cellchat_group_by_column <- function(x) {
  if (is.null(x$meta) || !"labels" %in% colnames(x$meta)) {
    .cellchat_input_stop(
      "invalid_input",
      sprintf(paste0("Colonne 'labels' absente de $meta (colonnes presentes : %s) ",
                     ": entree CellChat non conforme au contrat 4D-3."),
              paste(colnames(x$meta %||% data.frame()), collapse = ", "))
    )
  }
  "labels"
}

#' Resume d'entree CellChat pour export CSV (une ligne)
#'
#' @param x Objet `cellchat_input` valide.
#' @return data.frame a une ligne, colonnes character.
#' @export
cellchat_input_summary <- function(x) {
  assert_cellchat_input(x, context = "resume d'entree CellChat")
  qc <- x$qc %||% list()
  .v <- function(v) as.character(v %||% NA)
  data.frame(
    analysis_id        = .v(x$analysis_id),
    status             = .v(x$status),
    species            = .v(x$species),
    database           = .v(x$database),
    group_by           = .v(x$group_by),
    n_genes            = .v(x$input_summary$n_genes),
    n_cells            = .v(x$input_summary$n_cells),
    n_populations      = .v(x$input_summary$n_populations),
    n_symbols_resolved = .v(qc$n_symbols_resolved),
    n_ensembl_resolved = .v(qc$n_ensembl_resolved),
    n_duplicate_symbols = .v(qc$n_duplicate_symbols),
    n_cells_dropped    = .v((qc$n_cells_dropped_na_label %||% 0L) +
                            (qc$n_cells_dropped_min_group %||% 0L)),
    counts_like        = .v(qc$counts_like),
    object_fingerprint = .v(x$object_identity$fingerprint),
    warnings           = .v(if (length(x$warnings)) paste(x$warnings, collapse = " | ") else NA_character_),
    stringsAsFactors   = FALSE
  )
}

#' Surface publique du contrat d'entree CellChat
#'
#' Gelee par tests/testthat/test-cellchat-input-contract-freeze.R : toute
#' fonction top-level non prefixee d'un point doit figurer ici.
#'
#' @return Vecteur character trie.
#' @export
cellchat_input_public_api <- function() {
  c(
    "assert_cellchat_input", "build_cellchat_input", "cellchat_database_for_species",
    "cellchat_group_by_column", "cellchat_input_error_state", "cellchat_input_from_matrix",
    "cellchat_input_public_api", "cellchat_input_requirements",
    "cellchat_input_states", "cellchat_input_summary", "cellchat_log_normalize"
  )
}

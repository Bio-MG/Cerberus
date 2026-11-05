# =============================================================================
# R/core/biomentat_handoff.R — Moteur de validation du contrat biomentat-exchange/v1
#                             TranscriptoShiny ("Biomentat")
# =============================================================================
# Fournit les vérifications de cohérence et de validité des données quantifiées
# transmises depuis l'application amont vers Biomentat :
#   - vérification du schéma manifest.json
#   - sémantique de comptage brut (règle 95 % / 100 % d'entiers, raw counts)
#   - sanité dimensionnelle et absence de valeurs manquantes / négatives
#   - plan expérimental et plancher de réplicats biologiques (anti-pseudoréplication)
#   - espaces d'identifiants et concordance d'espèces (GRCh38 / GRCm39)
#   - émission du rapport structuré validation.json
#
# Règle contract-first : toute évolution passe SIMULTANÉMENT par ce code,
# le test de gel tests/testthat/test-biomentat-handoff-contract-freeze.R et
# docs/contracts/BIOMENTAT_HANDOFF_CONTRACT.md.
# Pureté Shiny : aucun symbole réactif dans ce fichier (règle C2).
# =============================================================================

# ── Opérateur null-coalescing interne ----------------------------------------
.biomentat_handoff_null_coalesce <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}

# ── Constructeur d'erreur classée --------------------------------------------
.biomentat_handoff_stop <- function(msg, state) {
  stop(errorCondition(
    paste0(msg),
    class = "biomentat_handoff_error",
    state = state,
    code = state
  ))
}

# ── Détection interne du type d'identifiants de gènes ------------------------
.biomentat_handoff_detect_gene_id_type <- function(gene_ids) {
  if (exists("detect_gene_id_type", mode = "function")) {
    return(get("detect_gene_id_type", mode = "function")(gene_ids))
  }
  sample_ids <- trimws(head(gene_ids[!is.na(gene_ids)], 50L))
  if (length(sample_ids) == 0L) return("unknown")
  pct_match <- function(pat) mean(grepl(pat, sample_ids))
  if (pct_match("^ENSG[0-9]{11}") > 0.7 || pct_match("^ENSMUSG[0-9]{11}") > 0.7) return("ensembl")
  if (pct_match("^[0-9]+$") > 0.7) return("entrez")
  if (pct_match("^[0-9]+(_[a-z]_)?_at$") > 0.5) return("affy_probe")
  if (pct_match("^[A-Za-z0-9.-]+$") > 0.7 && pct_match("^[0-9]+$") < 0.3) return("symbol")
  "unknown"
}

# ── Détection interne de l'organisme depuis les identifiants Ensembl ---------
.biomentat_handoff_detect_organism <- function(gene_ids) {
  if (exists("detect_organism_from_ids", mode = "function")) {
    return(get("detect_organism_from_ids", mode = "function")(gene_ids))
  }
  gids <- head(stats::na.omit(as.character(gene_ids)), 200L)
  if (length(gids) == 0L) return(NA_character_)
  gids <- trimws(gids)
  pct_mouse <- mean(grepl("^ENSMUSG[0-9]+", gids))
  pct_human <- mean(grepl("^ENSG[0-9]+", gids))
  if (pct_mouse >= 0.50) return("mouse")
  if (pct_human >= 0.50) return("human")
  NA_character_
}

# ── Vérification interne de colinéarité / confusion de plan d'expérience -----
.biomentat_handoff_check_confounding <- function(metadata, condition_col, covariate_col) {
  if (exists("check_design_confounding", mode = "function")) {
    return(isTRUE(get("check_design_confounding", mode = "function")(metadata, condition_col, covariate_col)))
  }
  if (!condition_col %in% colnames(metadata) || !covariate_col %in% colnames(metadata)) {
    return(FALSE)
  }
  tbl <- table(
    factor(metadata[[condition_col]]),
    factor(metadata[[covariate_col]])
  )
  if (nrow(tbl) < 2L || ncol(tbl) < 2L) return(FALSE)
  nonzero_per_covariate_level <- colSums(tbl > 0L)
  all(nonzero_per_covariate_level == 1L)
}

# ── Constantes et métadonnées du contrat -------------------------------------

biomentat_handoff_schema_version <- function() {
  "biomentat-exchange/v1"
}

biomentat_handoff_supported_species <- function() {
  c("human", "mouse")
}

biomentat_handoff_supported_assemblies <- function() {
  c("GRCh38", "GRCm39")
}

biomentat_handoff_supported_feature_types <- function() {
  c("ensembl", "symbol", "entrez")
}

biomentat_handoff_supported_matrix_types <- function() {
  c("raw_counts", "normalized")
}

biomentat_handoff_states <- function() {
  c(
    "file_corrupted",
    "file_not_found",
    "invalid_feature_ids",
    "invalid_matrix_dimensions",
    "invalid_matrix_values",
    "invalid_species",
    "manifest_invalid_field",
    "manifest_malformed",
    "manifest_missing",
    "negative_counts",
    "not_raw_counts",
    "pseudoreplication_detected",
    "samplesheet_matrix_mismatch",
    "samplesheet_missing_columns",
    "species_assembly_mismatch",
    "species_feature_mismatch"
  )
}

biomentat_handoff_error_class <- function() {
  "biomentat_handoff_error"
}

biomentat_handoff_error_state <- function(e = NULL) {
  if (is.null(e)) return(NA_character_)
  if (exists("ts_error_state", mode = "function")) {
    get("ts_error_state", mode = "function")(e, class = "biomentat_handoff_error")
  } else {
    if (!inherits(e, "biomentat_handoff_error")) return(NA_character_)
    st <- tryCatch(e$state, error = function(e2) NULL)
    if (is.null(st) || length(st) != 1L || is.na(st)) NA_character_ else as.character(st)
  }
}

# ── Lecture des fichiers du drop ---------------------------------------------

biomentat_handoff_read_manifest <- function(drop_dir = NULL) {
  if (is.null(drop_dir) || !nzchar(trimws(drop_dir))) {
    .biomentat_handoff_stop("Répertoire de drop non fourni.", state = "file_not_found")
  }
  if (!dir.exists(drop_dir)) {
    .biomentat_handoff_stop(
      sprintf("Le répertoire de drop n'existe pas : %s", drop_dir),
      state = "file_not_found"
    )
  }
  manifest_path <- file.path(drop_dir, "manifest.json")
  if (!file.exists(manifest_path)) {
    .biomentat_handoff_stop(
      sprintf("Fichier manifest.json introuvable dans %s", drop_dir),
      state = "manifest_missing"
    )
  }
  txt <- tryCatch(readLines(manifest_path, warn = FALSE, encoding = "UTF-8"), error = function(e) {
    .biomentat_handoff_stop(
      sprintf("Impossible de lire manifest.json (%s) : %s", manifest_path, conditionMessage(e)),
      state = "file_corrupted"
    )
  })
  raw_json <- paste(txt, collapse = "\n")
  if (!nzchar(trimws(raw_json))) {
    .biomentat_handoff_stop("manifest.json est vide.", state = "manifest_malformed")
  }
  manifest <- tryCatch(
    jsonlite::fromJSON(raw_json, simplifyVector = TRUE),
    error = function(e) {
      .biomentat_handoff_stop(
        sprintf("manifest.json syntaxiquement invalide : %s", conditionMessage(e)),
        state = "manifest_malformed"
      )
    }
  )
  if (!is.list(manifest) || length(manifest) == 0L) {
    .biomentat_handoff_stop(
      "manifest.json ne contient pas un objet JSON valide.",
      state = "manifest_malformed"
    )
  }
  manifest
}

biomentat_handoff_read_matrix <- function(matrix_path = NULL) {
  if (is.null(matrix_path) || !nzchar(trimws(matrix_path))) {
    .biomentat_handoff_stop("Chemin de la matrice non fourni.", state = "file_not_found")
  }
  if (!file.exists(matrix_path)) {
    .biomentat_handoff_stop(
      sprintf("Fichier de matrice introuvable : %s", matrix_path),
      state = "file_not_found"
    )
  }
  ext <- tolower(tools::file_ext(matrix_path))
  mat <- tryCatch({
    if (ext == "rds") {
      readRDS(matrix_path)
    } else {
      is_csv <- grepl("\\.csv(\\.gz)?$", tolower(matrix_path))
      sep_char <- if (is_csv) "," else "\t"
      con <- if (grepl("\\.gz$", tolower(matrix_path))) gzfile(matrix_path, "rt") else file(matrix_path, "rt")
      on.exit(close(con), add = TRUE)
      df <- utils::read.table(con, header = TRUE, sep = sep_char, row.names = 1L,
                              check.names = FALSE, stringsAsFactors = FALSE,
                              comment.char = "")
      as.matrix(df)
    }
  }, error = function(e) {
    .biomentat_handoff_stop(
      sprintf("Impossible de charger la matrice de comptage (%s) : %s", matrix_path, conditionMessage(e)),
      state = "file_corrupted"
    )
  })

  if (!is.matrix(mat) && !inherits(mat, "dgCMatrix")) {
    .biomentat_handoff_stop(
      sprintf("Le fichier de comptage ne contient ni une matrice dense ni une dgCMatrix (classe : %s).",
              paste(class(mat), collapse = "/")),
      state = "invalid_matrix_dimensions"
    )
  }
  mat
}

biomentat_handoff_read_samplesheet <- function(samplesheet_path = NULL) {
  if (is.null(samplesheet_path) || !nzchar(trimws(samplesheet_path))) {
    .biomentat_handoff_stop("Chemin de la samplesheet non fourni.", state = "file_not_found")
  }
  if (!file.exists(samplesheet_path)) {
    .biomentat_handoff_stop(
      sprintf("Fichier de samplesheet introuvable : %s", samplesheet_path),
      state = "file_not_found"
    )
  }
  is_csv <- grepl("\\.csv(\\.gz)?$", tolower(samplesheet_path))
  sep_char <- if (is_csv) "," else "\t"
  df <- tryCatch({
    con <- if (grepl("\\.gz$", tolower(samplesheet_path))) gzfile(samplesheet_path, "rt") else file(samplesheet_path, "rt")
    on.exit(close(con), add = TRUE)
    utils::read.table(con, header = TRUE, sep = sep_char,
                      check.names = FALSE, stringsAsFactors = FALSE,
                      comment.char = "")
  }, error = function(e) {
    .biomentat_handoff_stop(
      sprintf("Impossible de lire la samplesheet (%s) : %s", samplesheet_path, conditionMessage(e)),
      state = "file_corrupted"
    )
  })
  if (!is.data.frame(df) || nrow(df) == 0L) {
    .biomentat_handoff_stop(
      sprintf("La samplesheet est vide ou malformée (%s).", samplesheet_path),
      state = "file_corrupted"
    )
  }
  df
}

# ── Validation des composants ------------------------------------------------

biomentat_handoff_validate_manifest <- function(manifest = NULL, drop_dir = NULL) {
  if (is.null(manifest) || !is.list(manifest)) {
    .biomentat_handoff_stop("Objet manifeste invalide ou NULL.", state = "manifest_invalid_field")
  }
  # schema_version
  if (is.null(manifest$schema_version) || !identical(as.character(manifest$schema_version), "biomentat-exchange/v1")) {
    .biomentat_handoff_stop(
      sprintf("schema_version manquant ou invalide (attendu : 'biomentat-exchange/v1', reçu : '%s').",
              .biomentat_handoff_null_coalesce(manifest$schema_version, "NULL")),
      state = "manifest_invalid_field"
    )
  }
  # dataset_id
  if (is.null(manifest$dataset_id) || !is.character(manifest$dataset_id) || !nzchar(trimws(manifest$dataset_id))) {
    .biomentat_handoff_stop("dataset_id manquant ou vide dans le manifeste.", state = "manifest_invalid_field")
  }
  # species
  if (is.null(manifest$species) || !is.character(manifest$species) || !nzchar(trimws(manifest$species))) {
    .biomentat_handoff_stop("species manquant ou vide dans le manifeste.", state = "invalid_species")
  }
  sp <- tolower(trimws(manifest$species))
  if (!sp %in% biomentat_handoff_supported_species()) {
    .biomentat_handoff_stop(
      sprintf("Espèce non supportée ('%s'). Espèces admises : %s.",
              manifest$species, paste(biomentat_handoff_supported_species(), collapse = ", ")),
      state = "invalid_species"
    )
  }
  # genome_assembly (champ obligatoire, contract §2)
  if (is.null(manifest$genome_assembly) || !is.character(manifest$genome_assembly) || !nzchar(trimws(manifest$genome_assembly))) {
    .biomentat_handoff_stop("genome_assembly manquant ou vide dans le manifeste.", state = "manifest_invalid_field")
  }
  ass <- trimws(manifest$genome_assembly)
  if (!ass %in% biomentat_handoff_supported_assemblies()) {
    .biomentat_handoff_stop(
      sprintf("Assemblage génomique non supporté ('%s'). Assemblages admis : %s.",
              ass, paste(biomentat_handoff_supported_assemblies(), collapse = ", ")),
      state = "species_assembly_mismatch"
    )
  }
  if (sp == "human" && ass != "GRCh38") {
    .biomentat_handoff_stop(
      sprintf("Discordance espèce/assemblage : pour l'humain, l'assemblage attendu est 'GRCh38' (reçu : '%s').", ass),
      state = "species_assembly_mismatch"
    )
  }
  if (sp == "mouse" && ass != "GRCm39") {
    .biomentat_handoff_stop(
      sprintf("Discordance espèce/assemblage : pour la souris, l'assemblage attendu est 'GRCm39' (reçu : '%s').", ass),
      state = "species_assembly_mismatch"
    )
  }
  # feature_id_type
  if (is.null(manifest$feature_id_type) || !manifest$feature_id_type %in% biomentat_handoff_supported_feature_types()) {
    .biomentat_handoff_stop(
      sprintf("feature_id_type manquant ou non supporté ('%s'). Types admis : %s.",
              .biomentat_handoff_null_coalesce(manifest$feature_id_type, "NULL"),
              paste(biomentat_handoff_supported_feature_types(), collapse = ", ")),
      state = "manifest_invalid_field"
    )
  }
  # matrix_path
  if (is.null(manifest$matrix_path) || !is.character(manifest$matrix_path) || !nzchar(trimws(manifest$matrix_path))) {
    .biomentat_handoff_stop("matrix_path manquant ou vide dans le manifeste.", state = "manifest_invalid_field")
  }
  # matrix_type
  if (is.null(manifest$matrix_type) || !manifest$matrix_type %in% biomentat_handoff_supported_matrix_types()) {
    .biomentat_handoff_stop(
      sprintf("matrix_type manquant ou non supporté ('%s'). Types admis : %s.",
              .biomentat_handoff_null_coalesce(manifest$matrix_type, "NULL"),
              paste(biomentat_handoff_supported_matrix_types(), collapse = ", ")),
      state = "manifest_invalid_field"
    )
  }
  # samplesheet_path
  if (is.null(manifest$samplesheet_path) || !is.character(manifest$samplesheet_path) || !nzchar(trimws(manifest$samplesheet_path))) {
    .biomentat_handoff_stop("samplesheet_path manquant ou vide dans le manifeste.", state = "manifest_invalid_field")
  }

  # Vérification de l'existence des fichiers cibles si drop_dir est fourni
  if (!is.null(drop_dir)) {
    mat_file <- file.path(drop_dir, manifest$matrix_path)
    if (!file.exists(mat_file)) {
      .biomentat_handoff_stop(
        sprintf("Fichier de matrice référencé introuvable : %s", mat_file),
        state = "file_not_found"
      )
    }
    samp_file <- file.path(drop_dir, manifest$samplesheet_path)
    if (!file.exists(samp_file)) {
      .biomentat_handoff_stop(
        sprintf("Fichier de samplesheet référencé introuvable : %s", samp_file),
        state = "file_not_found"
      )
    }
  }
  invisible(TRUE)
}

biomentat_handoff_validate_identifiers <- function(mat = NULL, species = "human", feature_id_type = "ensembl") {
  if (is.null(mat)) {
    .biomentat_handoff_stop("Matrice NULL pour la validation des identifiants.", state = "invalid_matrix_dimensions")
  }
  feat_ids <- rownames(mat)
  if (is.null(feat_ids) || length(feat_ids) == 0L) {
    .biomentat_handoff_stop("Rownames manquants ou vides sur la matrice de comptage.", state = "invalid_feature_ids")
  }
  if (anyNA(feat_ids)) {
    .biomentat_handoff_stop("Rownames contenant des valeurs NA.", state = "invalid_feature_ids")
  }
  if (any(!nzchar(trimws(feat_ids)))) {
    .biomentat_handoff_stop("Rownames manquants ou vides sur la matrice de comptage.", state = "invalid_feature_ids")
  }
  if (is.null(species) || !is.character(species) || !nzchar(trimws(species))) {
    .biomentat_handoff_stop("Espèce manquante ou vide.", state = "invalid_species")
  }
  sp <- tolower(trimws(species))
  if (!sp %in% biomentat_handoff_supported_species()) {
    .biomentat_handoff_stop(sprintf("Espèce non supportée : '%s'.", species), state = "invalid_species")
  }

  det_type <- .biomentat_handoff_detect_gene_id_type(feat_ids)
  if (det_type == "unknown" && feature_id_type %in% c("ensembl", "entrez")) {
    label <- if (identical(feature_id_type, "ensembl")) "Ensembl" else "Entrez"
    .biomentat_handoff_stop(
      sprintf("Identifiants de gènes non reconnus comme des identifiants %s valides.", label),
      state = "invalid_feature_ids"
    )
  }
  if (det_type != "unknown" && det_type != feature_id_type) {
    .biomentat_handoff_stop(
      sprintf("Type d'identifiants discordant : déclaré '%s', détecté '%s'.", feature_id_type, det_type),
      state = "invalid_feature_ids"
    )
  }
  if (identical(feature_id_type, "symbol") && any(grepl("^ENS(MUS)?G[0-9]+", feat_ids))) {
    .biomentat_handoff_stop(
      "Identifiants Ensembl détectés alors que le type d'identifiants déclaré est 'symbol'.",
      state = "invalid_feature_ids"
    )
  }
  if (identical(feature_id_type, "entrez") && any(!grepl("^[0-9]+$", feat_ids))) {
    .biomentat_handoff_stop(
      "Identifiants de gènes non reconnus comme des identifiants Entrez valides (chiffres uniquement requis).",
      state = "invalid_feature_ids"
    )
  }

  # Garde stricte de contamination croisée inter-espèces Ensembl par préfixe
  has_mouse_ensembl <- any(grepl("^ENSMUSG[0-9]+", feat_ids))
  has_human_ensembl <- any(grepl("^ENSG[0-9]+", feat_ids))
  if (sp == "human" && has_mouse_ensembl) {
    .biomentat_handoff_stop(
      "Discordance espèce/identifiants : espèce déclarée 'human', mais identifiants Ensembl de souris (ENSMUSG) détectés.",
      state = "species_feature_mismatch"
    )
  }
  if (sp == "mouse" && has_human_ensembl) {
    .biomentat_handoff_stop(
      "Discordance espèce/identifiants : espèce déclarée 'mouse', mais identifiants Ensembl humains (ENSG) détectés.",
      state = "species_feature_mismatch"
    )
  }

  det_org <- .biomentat_handoff_detect_organism(feat_ids)
  if (!is.na(det_org)) {
    if (sp == "human" && det_org == "mouse") {
      .biomentat_handoff_stop(
        "Discordance espèce/identifiants : espèce déclarée 'human', mais identifiants Ensembl de souris (ENSMUSG) détectés.",
        state = "species_feature_mismatch"
      )
    }
    if (sp == "mouse" && det_org == "human") {
      .biomentat_handoff_stop(
        "Discordance espèce/identifiants : espèce déclarée 'mouse', mais identifiants Ensembl humains (ENSG) détectés.",
        state = "species_feature_mismatch"
      )
    }
  }

  invisible(list(detected_type = det_type, detected_organism = det_org))
}

biomentat_handoff_validate_matrix <- function(mat = NULL, matrix_type = "raw_counts",
                                            feature_id_type = "ensembl", species = "human",
                                            allow_non_integer = FALSE) {
  if (is.null(mat)) {
    .biomentat_handoff_stop("Matrice de comptage NULL.", state = "invalid_matrix_dimensions")
  }
  is_sparse <- inherits(mat, "dgCMatrix")
  if (!is.matrix(mat) && !is_sparse) {
    .biomentat_handoff_stop("L'objet fourni n'est ni une matrix dense ni une dgCMatrix.", state = "invalid_matrix_dimensions")
  }
  if (nrow(mat) < 1L || ncol(mat) < 2L) {
    .biomentat_handoff_stop(
      sprintf("Dimensions de matrice invalides (%d gènes, %d échantillons). Au moins 1 gène et 2 échantillons requis.",
              nrow(mat), ncol(mat)),
      state = "invalid_matrix_dimensions"
    )
  }
  if (is.null(colnames(mat)) || anyNA(colnames(mat)) || any(!nzchar(trimws(colnames(mat))))) {
    .biomentat_handoff_stop("Colnames manquants, NA ou vides sur la matrice de comptage.", state = "samplesheet_matrix_mismatch")
  }

  biomentat_handoff_validate_identifiers(mat, species = species, feature_id_type = feature_id_type)

  if (is_sparse) {
    vals <- mat@x
    if (anyNA(vals) || any(is.infinite(vals))) {
      .biomentat_handoff_stop("La matrice creuse contient des valeurs NA, NaN ou infinies.", state = "invalid_matrix_values")
    }
    if (any(vals < 0)) {
      .biomentat_handoff_stop("La matrice contient des valeurs négatives.", state = "negative_counts")
    }
    fin <- vals[is.finite(vals)]
    frac_int <- if (length(fin) == 0L) 1.0 else mean(abs(fin - round(fin)) < 1e-8)
  } else {
    if (!is.numeric(mat)) {
      .biomentat_handoff_stop("La matrice contient des données non numériques.", state = "invalid_matrix_values")
    }
    if (anyNA(mat) || any(is.infinite(mat))) {
      .biomentat_handoff_stop("La matrice contient des valeurs NA, NaN ou infinies.", state = "invalid_matrix_values")
    }
    if (any(mat < 0)) {
      .biomentat_handoff_stop("La matrice contient des valeurs négatives.", state = "negative_counts")
    }
    vals <- as.vector(mat)
    fin <- vals[is.finite(vals)]
    frac_int <- if (length(fin) == 0L) 1.0 else mean(abs(fin - round(fin)) < 1e-8)
  }

  if (identical(matrix_type, "raw_counts")) {
    if (!is_sparse && exists("bulk_assert_raw_counts", mode = "function")) {
      tryCatch({
        get("bulk_assert_raw_counts", mode = "function")(mat, context = "biomentat_handoff", allow_non_integer = allow_non_integer)
      }, bulk_batch_correction_error = function(e) {
        st <- if (identical(e$state, "not_raw_counts")) "not_raw_counts" else "invalid_matrix_values"
        .biomentat_handoff_stop(conditionMessage(e), state = st)
      })
    } else {
      if (!isTRUE(allow_non_integer)) {
        if (frac_int < 1.0) {
          .biomentat_handoff_stop(
            sprintf("Échec validation counts : la matrice contient des valeurs non entières (frac_int = %.4f < 1.0). Des counts bruts entiers sont requis.", frac_int),
            state = "not_raw_counts"
          )
        }
      } else if (frac_int <= 0.95) {
        .biomentat_handoff_stop(
          sprintf("Échec validation counts : proportion d'entiers insuffisante (%.1f%% <= 95%%).", 100 * frac_int),
          state = "not_raw_counts"
        )
      }
    }
  }

  invisible(list(n_genes = nrow(mat), n_samples = ncol(mat), frac_integer = frac_int))
}

biomentat_handoff_validate_samplesheet <- function(samplesheet = NULL, mat_colnames = NULL) {
  if (is.null(samplesheet) || !is.data.frame(samplesheet)) {
    .biomentat_handoff_stop("Samplesheet NULL ou n'étant pas un data.frame.", state = "samplesheet_missing_columns")
  }
  cols <- colnames(samplesheet)
  s_col <- if ("sample" %in% cols) "sample" else if ("sample_id" %in% cols) "sample_id" else NULL
  if (is.null(s_col)) {
    .biomentat_handoff_stop(
      sprintf("Colonne identifiant échantillon ('sample' ou 'sample_id') introuvable. Colonnes présentes : %s",
              paste(cols, collapse = ", ")),
      state = "samplesheet_missing_columns"
    )
  }
  if (!"condition" %in% cols) {
    .biomentat_handoff_stop(
      sprintf("Colonne 'condition' introuvable dans la samplesheet. Colonnes présentes : %s",
              paste(cols, collapse = ", ")),
      state = "samplesheet_missing_columns"
    )
  }

  samp_ids <- as.character(samplesheet[[s_col]])
  if (anyNA(samp_ids) || any(!nzchar(trimws(samp_ids)))) {
    .biomentat_handoff_stop("Identifiants d'échantillons NA ou vides dans la samplesheet.", state = "samplesheet_matrix_mismatch")
  }
  if (anyDuplicated(samp_ids) > 0L) {
    .biomentat_handoff_stop(
      sprintf("Identifiants d'échantillons dupliqués dans la samplesheet : %s",
              paste(samp_ids[duplicated(samp_ids)], collapse = ", ")),
      state = "samplesheet_matrix_mismatch"
    )
  }

  if (is.null(mat_colnames) || length(mat_colnames) == 0L ||
      anyNA(mat_colnames) || any(!nzchar(trimws(mat_colnames)))) {
    .biomentat_handoff_stop(
      "Identifiants de colonnes de matrice manquants, NULL ou vides pour la concordance des échantillons.",
      state = "samplesheet_matrix_mismatch"
    )
  }

  mat_cols <- as.character(mat_colnames)
  if (!setequal(samp_ids, mat_cols)) {
    missing_in_mat <- setdiff(samp_ids, mat_cols)
    missing_in_sheet <- setdiff(mat_cols, samp_ids)
    .biomentat_handoff_stop(
      sprintf("Non-concordance des échantillons entre la samplesheet et la matrice. Absents de la matrice : [%s] ; absents de la samplesheet : [%s].",
              paste(missing_in_mat, collapse = ", "), paste(missing_in_sheet, collapse = ", ")),
      state = "samplesheet_matrix_mismatch"
    )
  }

  conds <- as.character(samplesheet[["condition"]])
  if (anyNA(conds) || any(!nzchar(trimws(conds)))) {
    .biomentat_handoff_stop("Valeurs manquantes ou vides dans la colonne 'condition'.", state = "pseudoreplication_detected")
  }
  tab <- table(conds)
  if (length(tab) < 2L) {
    .biomentat_handoff_stop(
      sprintf("Le plan d'expérience requiert au moins 2 conditions distinctes pour l'analyse différentielle (reçu : %d).", length(tab)),
      state = "pseudoreplication_detected"
    )
  }
  min_rep <- if (exists("TS_DA_MIN_REPLICATES_PER_CONDITION", inherits = TRUE)) {
    get("TS_DA_MIN_REPLICATES_PER_CONDITION", inherits = TRUE)
  } else 2L
  if (any(tab < min_rep)) {
    bad_grps <- names(tab)[tab < min_rep]
    .biomentat_handoff_stop(
      sprintf("Pseudoréplication détectée : chaque condition doit avoir au moins %d réplicats biologiques (plancher TS_DA_MIN_REPLICATES_PER_CONDITION). Groupes déficients : %s.",
              min_rep, paste(bad_grps, collapse = ", ")),
      state = "pseudoreplication_detected"
    )
  }

  # Détection de colinéarité / confusion des covariables avec la condition
  covar_cols <- setdiff(cols, c(s_col, "condition"))
  confounded <- character(0)
  for (cov in covar_cols) {
    if (.biomentat_handoff_check_confounding(samplesheet, "condition", cov)) {
      confounded <- c(confounded, cov)
    }
  }

  invisible(list(
    valid = TRUE,
    n_samples = length(samp_ids),
    conditions = names(tab),
    confounded_covariates = confounded
  ))
}

# ── Validation globale du drop et émission JSON ------------------------------

biomentat_handoff_write_validation <- function(validation_result = NULL, out_path = NULL) {
  if (is.null(validation_result) || is.null(out_path)) {
    .biomentat_handoff_stop("validation_result ou out_path manquant pour l'écriture.", state = "invalid_matrix_values")
  }
  dir_name <- dirname(out_path)
  if (!dir.exists(dir_name)) {
    dir.create(dir_name, recursive = TRUE, showWarnings = FALSE)
  }
  json_txt <- jsonlite::toJSON(validation_result, pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null")
  tmp_file <- tempfile(pattern = "val_", tmpdir = dir_name, fileext = ".tmp")
  writeLines(json_txt, tmp_file, useBytes = TRUE)
  if (!file.rename(tmp_file, out_path)) {
    file.copy(tmp_file, out_path, overwrite = TRUE)
    unlink(tmp_file)
  }
  invisible(out_path)
}

biomentat_handoff_validate_drop <- function(drop_dir = NULL, out_dir = NULL, allow_non_integer = FALSE) {
  t0 <- Sys.time()
  st <- new.env(parent = emptyenv())
  st$warns <- list()
  st$errs <- list()
  st$manifest <- NULL
  st$mat <- NULL
  st$samplesheet <- NULL
  st$is_valid <- FALSE
  st$dataset_id <- NA_character_
  st$checks_summary <- list()

  add_warn <- function(code, field, message) {
    st$warns <- c(st$warns, list(list(code = code, field = field, message = message)))
  }
  add_err <- function(code, field, message) {
    st$errs <- c(st$errs, list(list(code = code, field = field, message = message)))
  }

  tryCatch({
    # Étape 1 : Lecture et validation du manifeste
    st$manifest <- biomentat_handoff_read_manifest(drop_dir)
    st$dataset_id <- .biomentat_handoff_null_coalesce(st$manifest$dataset_id, NA_character_)
    biomentat_handoff_validate_manifest(st$manifest, drop_dir = drop_dir)
    st$checks_summary$manifest_syntax <- list(valid = TRUE)

    # Étape 2 : Lecture et validation de la matrice de comptage
    mat_path <- file.path(drop_dir, st$manifest$matrix_path)
    st$mat <- biomentat_handoff_read_matrix(mat_path)
    mat_val <- biomentat_handoff_validate_matrix(
      st$mat,
      matrix_type = st$manifest$matrix_type,
      feature_id_type = st$manifest$feature_id_type,
      species = st$manifest$species,
      allow_non_integer = allow_non_integer
    )
    if (nrow(st$mat) < 100L) {
      add_warn("low_feature_count", "matrix", sprintf("Matrice avec seulement %d gènes (< 100).", nrow(st$mat)))
    }
    if (anyDuplicated(rownames(st$mat)) > 0L) {
      add_warn("duplicate_feature_ids", "matrix", "Présence d'identifiants de gènes dupliqués dans les rownames.")
    }
    if (isTRUE(allow_non_integer) && mat_val$frac_integer >= 0.95 && mat_val$frac_integer < 1.0) {
      add_warn("non_integer_counts_tolerated", "matrix",
               sprintf("Proportion de counts entiers (%.4f) tolérée sous allow_non_integer = TRUE.", mat_val$frac_integer))
    }
    st$checks_summary$matrix_semantics <- list(
      valid = TRUE,
      n_features = nrow(st$mat),
      n_samples = ncol(st$mat),
      matrix_type = st$manifest$matrix_type,
      frac_integer = mat_val$frac_integer
    )

    # Étape 3 : Lecture et validation de la samplesheet
    samp_path <- file.path(drop_dir, st$manifest$samplesheet_path)
    st$samplesheet <- biomentat_handoff_read_samplesheet(samp_path)
    sheet_val <- biomentat_handoff_validate_samplesheet(st$samplesheet, mat_colnames = colnames(st$mat))

    confounded_covs <- if (is.list(sheet_val) && !is.null(sheet_val$confounded_covariates)) {
      sheet_val$confounded_covariates
    } else {
      cov_candidates <- setdiff(colnames(st$samplesheet), c("sample", "sample_id", "condition"))
      Filter(function(cv) .biomentat_handoff_check_confounding(st$samplesheet, "condition", cv), cov_candidates)
    }
    for (cov_name in confounded_covs) {
      add_warn("covariate_confounded", cov_name,
               sprintf("Covariable '%s' totalement confondue avec la colonne 'condition' (100%% colinéarité).", cov_name))
    }

    cond_col <- "condition"
    st$checks_summary$experimental_design <- list(
      valid = TRUE,
      condition_col = cond_col,
      n_samples = nrow(st$samplesheet),
      sufficient_replicates = TRUE,
      groups = as.list(table(st$samplesheet[[cond_col]]))
    )

    # Étape 4 : Détection identifiants et organisme
    det_type <- .biomentat_handoff_detect_gene_id_type(rownames(st$mat))
    det_org <- .biomentat_handoff_detect_organism(rownames(st$mat))
    st$checks_summary$species_and_identifiers <- list(
      valid = TRUE,
      declared_species = st$manifest$species,
      detected_species = .biomentat_handoff_null_coalesce(det_org, "unknown"),
      declared_id_type = st$manifest$feature_id_type,
      detected_id_type = det_type
    )

    st$is_valid <- TRUE
  }, biomentat_handoff_error = function(e) {
    add_err(.biomentat_handoff_null_coalesce(e$state, "validation_error"), "validation", conditionMessage(e))
  }, error = function(e) {
    add_err("file_corrupted", "internal", conditionMessage(e))
  })

  dur <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 3)
  status_str <- if (st$is_valid) "VALID" else "INVALID"

  res <- list(
    valid = st$is_valid,
    status = status_str,
    schema_version = "biomentat-exchange/v1",
    dataset_id = st$dataset_id,
    validated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    summary = list(
      valid = st$is_valid,
      n_errors = length(st$errs),
      n_warnings = length(st$warns),
      n_genes = if (!is.null(st$mat)) nrow(st$mat) else 0L,
      n_samples = if (!is.null(st$mat)) ncol(st$mat) else 0L,
      conditions = if (!is.null(st$samplesheet) && "condition" %in% colnames(st$samplesheet)) names(table(st$samplesheet$condition)) else character(0),
      duration_seconds = dur,
      message = if (st$is_valid) "Validation réussie." else if (length(st$errs) > 0L) st$errs[[1L]]$message else "Validation échouée."
    ),
    checks = st$checks_summary,
    errors = st$errs,
    warnings = st$warns
  )

  if (!is.null(out_dir)) {
    biomentat_handoff_write_validation(res, file.path(out_dir, "validation.json"))
  }

  res
}

# Alias public pour commodité d'appel
biomentat_validate_drop <- function(drop_dir = NULL, out_dir = NULL, allow_non_integer = FALSE) {
  biomentat_handoff_validate_drop(drop_dir = drop_dir, out_dir = out_dir, allow_non_integer = allow_non_integer)
}

# ── Inventaire de l'API publique gelée ---------------------------------------

biomentat_handoff_public_api <- function() {
  sort(c(
    "biomentat_handoff_error_class",
    "biomentat_handoff_error_state",
    "biomentat_handoff_public_api",
    "biomentat_handoff_read_manifest",
    "biomentat_handoff_read_matrix",
    "biomentat_handoff_read_samplesheet",
    "biomentat_handoff_schema_version",
    "biomentat_handoff_states",
    "biomentat_handoff_supported_assemblies",
    "biomentat_handoff_supported_feature_types",
    "biomentat_handoff_supported_matrix_types",
    "biomentat_handoff_supported_species",
    "biomentat_handoff_validate_drop",
    "biomentat_handoff_validate_identifiers",
    "biomentat_handoff_validate_manifest",
    "biomentat_handoff_validate_matrix",
    "biomentat_handoff_validate_samplesheet",
    "biomentat_handoff_write_validation",
    "biomentat_validate_drop"
  ))
}

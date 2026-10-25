# =============================================================================
# tools/pkg_skeleton/R/bulk_provenance.R — Manifeste de provenance Bulk RNA-seq (roadmap Bulk
# V2, Milestone 1) — PREREQUIS des analyses GSVA/WGCNA/survie.
# =============================================================================
# PRINCIPE (AGENTS.md regle 6) : la provenance est PRODUITE (a l'import, puis a
# chaque declaration utilisateur) et CONSOLIDEE pour l'affichage/l'export —
# jamais reconstruite apres coup. Ce fichier complete R/core/provenance.R
# (entrees d'analyse par etape, reutilisees telles quelles par les modules) :
# il couvre le manifeste de JEU DE DONNEES `bulk_obj$provenance` :
#   - genome de reference (GRCh38, GRCm39, ...),
#   - release d'annotation,
#   - type de normalisation (counts bruts / VST / TMM log2-CPM ...),
#   - session R (version, plateforme, packages cles).
#
# Fonctions pures, testables hors Shiny. Aucune dependance dure : les versions
# de packages sont relevees uniquement pour les packages installes.
# Erreurs : classed `bulk_provenance_error` (messages francais, call. = FALSE).
# =============================================================================

#' Surface publique figee du domaine provenance bulk (gel par test de freeze)
bulk_provenance_public_api <- function() {
  c("bulk_provenance_public_api",
    "bulk_provenance_known_genomes",
    "bulk_provenance_known_normalizations",
    "bulk_build_provenance",
    "bulk_ensure_provenance",
    "bulk_update_provenance",
    "bulk_provenance_dataframe",
    "bulk_provenance_session_packages")
}

#' Genomes de reference proposes dans l'UI (liste ouverte — "Autre" libre)
bulk_provenance_known_genomes <- function() {
  c("GRCh38", "GRCh37", "GRCm39", "GRCm38", "GRCz11", "Rnor_6.0", "Sus_scrofa", "Autre / inconnu")
}

#' Types de normalisation proposes dans l'UI
bulk_provenance_known_normalizations <- function() {
  c("Counts bruts", "VST (DESeq2)", "rlog (DESeq2)", "TMM log2-CPM (edgeR)",
    "log2-CPM", "TPM", "FPKM", "Autre / inconnu")
}

#' Versions des packages cles du pipeline bulk (installes uniquement)
#'
#' Releve nominal (jamais une re-derivation) : seule la version constatee est
#' enregistree. Les packages absents sont omis (la cle n'existe pas).
#'
#' Le chargement des namespaces est un simple CONSTAT de version : la fonction
#' ne doit laisser AUCUN etat global derriere elle (voir le correctif
#' `Matrix.warnDeprecatedCoerce` ci-dessous).
#'
#' @return Vecteur character nomme, au minimum vide.
bulk_provenance_session_packages <- function() {
  candidates <- c("DESeq2", "edgeR", "limma", "GSVA", "GSEABase", "WGCNA",
                  "variancePartition", "decoupleR", "msigdbr", "survival",
                  "survminer", "ComplexHeatmap", "mirai")
  # Effet de bord tiers neutralise : GSVA::.onLoad() fait
  # options(Matrix.warnDeprecatedCoerce = 2) SANS le restaurer. Charger GSVA
  # ici (uniquement pour LIRE sa version) escaladait donc toute depreciation
  # Matrix ulterieure en ERREUR FATALE (Matrix::Matrix.DeprecatedCoerce force
  # alors options(warn = 2L)). Casse reelle mesuree : Milo
  # (miloR::calcNhoodDistance -> as(<dgTMatrix>, "dgCMatrix")) echouait parce
  # que ce chargement laissait l'option derriere lui, globalement, pour tout le
  # reste de la session. On releve l'option avant le chargement et on la
  # restaure a l'identique (absente -> retiree, valeur NULL).
  mwd_before <- getOption("Matrix.warnDeprecatedCoerce")
  on.exit(options(Matrix.warnDeprecatedCoerce = mwd_before), add = TRUE)
  out <- character(0)
  for (p in candidates) {
    if (requireNamespace(p, quietly = TRUE)) {
      out[[p]] <- tryCatch(as.character(utils::packageVersion(p)),
                           error = function(e) NA_character_)
    }
  }
  out[!is.na(out)]
}

#' Construire le manifeste de provenance d'un jeu bulk (PRODUIT a l'import)
#'
#' @param reference_genome Character ou NA — ex. "GRCh38". NULL => NA (non declare).
#' @param annotation_release Character ou NA — ex. "Ensembl 110", "GENCODE v44".
#' @param normalization Character ou NA — ex. "Counts bruts", "VST (DESeq2)".
#' @param project Character ou NA — nom de projet (optionnel).
#' @param counts Matrice counts (genes x échantillons) ou NULL — enregistre
#'   uniquement les dimensions, jamais la matrice.
#' @param metadata data.frame métadonnées ou NULL — enregistre n et noms de colonnes.
#' @return Liste plate `type = "bulk_provenance"`.
bulk_build_provenance <- function(reference_genome = NULL, annotation_release = NULL,
                                  normalization = NULL, project = NULL,
                                  counts = NULL, metadata = NULL) {
  .as_decl <- function(x) {
    if (is.null(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(as.character(x)))) {
      return(NA_character_)
    }
    trimws(as.character(x))
  }

  dims_counts <- tryCatch(dim(counts), error = function(e) NULL)
  dims_counts <- if (is.null(dims_counts) || length(dims_counts) != 2L) c(NA_integer_, NA_integer_) else as.integer(dims_counts)

  meta_cols <- tryCatch(if (is.data.frame(metadata)) colnames(metadata) else NULL,
                        error = function(e) NULL)

  list(
    type               = "bulk_provenance",
    reference_genome   = .as_decl(reference_genome),
    annotation_release = .as_decl(annotation_release),
    normalization      = .as_decl(normalization),
    project            = .as_decl(project),
    n_genes            = dims_counts[1],
    n_samples          = dims_counts[2],
    metadata_columns   = if (is.null(meta_cols)) character(0) else meta_cols,
    r_version          = paste0(R.version$major, ".", R.version$minor),
    r_version_string   = R.version.string,
    r_platform         = R.version$platform,
    r_running          = R.version$running %||% NA_character_,
    locale             = Sys.getlocale("LC_COLLATE"),
    key_packages       = bulk_provenance_session_packages(),
    created_utc        = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    updated_utc        = NA_character_,
    history            = list()
  )
}

#' Garantir la presence du manifeste sur un bulk_obj (creation paresseuse)
#'
#' Tolere les imports anterieurs a cette fonctionnalite : si `bulk_obj$provenance`
#' est absent ou corrompu, un manifeste est PRODUIT maintenant (horodatage du
#' moment de la creation, jamais antidate). Le bulk_obj est retourne INCHANGE
#' si un manifeste valide existe deja.
#'
#' @param bulk_obj Liste bulk (champs counts/metadata optionnels ici).
#' @param project Nom de projet utilise si un manifeste doit etre cree.
#' @return Le bulk_obj (invisible), avec `$provenance` garantit valide.
bulk_ensure_provenance <- function(bulk_obj, project = NULL) {
  if (is.null(bulk_obj)) {
    stop(errorCondition(
      "bulk_ensure_provenance() : bulk_obj est NULL — importez d'abord un jeu bulk.",
      class = "bulk_provenance_error", state = "invalid_input"))
  }
  if (!is.list(bulk_obj)) {
    stop(errorCondition(sprintf(
      "bulk_ensure_provenance() : un objet bulk (liste) est requis (recu : %s).",
      paste(class(bulk_obj), collapse = "/")),
      class = "bulk_provenance_error", state = "invalid_input"))
  }
  prov <- bulk_obj$provenance
  if (is.list(prov) && identical(prov$type, "bulk_provenance")) {
    return(invisible(bulk_obj))
  }
  bulk_obj$provenance <- bulk_build_provenance(
    normalization = "Counts bruts",
    project       = project,
    counts        = if (is.matrix(bulk_obj$counts)) bulk_obj$counts else NULL,
    metadata      = bulk_obj$metadata
  )
  invisible(bulk_obj)
}

#' Mettre a jour les declarations du manifeste (PRODUCTION d'une nouvelle etat)
#'
#' Les champs declares par l'utilisateur (genome, release, normalisation) sont
#' remplaces ; l'ancienne valeur est preservee dans `history` (jamais perdue) ;
#`  `updated_utc` horodate. Le manifeste lui-meme n'est JAMAIS reconstruit :
#' seuls les champs cites et l'horodatage changent.
#'
#' @param bulk_obj Liste bulk (bulk_ensure_provenance() est appele d'abord).
#' @param reference_genome,annotation_release,normalization Nouvelles declarations
#'   (NULL = champ inchange).
#' @return Le bulk_obj (invisible).
bulk_update_provenance <- function(bulk_obj, reference_genome = NULL,
                                   annotation_release = NULL, normalization = NULL) {
  bulk_obj <- bulk_ensure_provenance(bulk_obj)
  prov     <- bulk_obj$provenance

  .as_decl <- function(x) {
    if (is.null(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(as.character(x)))) {
      return(NA_character_)
    }
    trimws(as.character(x))
  }

  updates <- list(
    reference_genome   = .as_decl(reference_genome),
    annotation_release = .as_decl(annotation_release),
    normalization      = .as_decl(normalization)
  )
  changed <- FALSE
  for (nm in names(updates)) {
    if (!is.na(updates[[nm]]) && !identical(prov[[nm]], updates[[nm]])) {
      prov$history <- c(prov$history, list(list(
        field = nm, previous = prov[[nm]], new = updates[[nm]],
        at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      )))
      prov[[nm]] <- updates[[nm]]
      changed <- TRUE
    }
  }
  if (changed) prov$updated_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  bulk_obj$provenance <- prov
  invisible(bulk_obj)
}

#' Aplatir le manifeste pour affichage / export (CONSOLIDATION)
#'
#' @param prov Manifeste `bulk_provenance` (ou NULL — renvoie un cadre vide).
#' @return data.frame 2 colonnes (Champ, Valeur), une ligne par champ scalaire ;
#'   les packages cles sont compactes en une ligne "nom=version; ...".
bulk_provenance_dataframe <- function(prov) {
  if (!is.list(prov) || !identical(prov$type, "bulk_provenance")) {
    return(data.frame(Champ = character(0), Valeur = character(0),
                      stringsAsFactors = FALSE))
  }
  pkg_str <- if (length(prov$key_packages) == 0L) "" else
    paste(vapply(names(prov$key_packages),
                 function(k) sprintf("%s=%s", k, prov$key_packages[[k]]),
                 character(1)), collapse = "; ")

  rows <- list(
    "Projet"                 = prov$project,
    "Génome de référence"    = prov$reference_genome,
    "Release d'annotation"   = prov$annotation_release,
    "Normalisation"          = prov$normalization,
    "Gènes × échantillons"   = if (!is.na(prov$n_genes) && !is.na(prov$n_samples))
                                  paste0(prov$n_genes, " × ", prov$n_samples) else NA_character_,
    "Version R"              = prov$r_version_string,
    "Plateforme"             = prov$r_platform,
    "Packages clés"          = pkg_str,
    "Créé (UTC)"             = prov$created_utc,
    "Mis à jour (UTC)"       = prov$updated_utc
  )
  data.frame(
    Champ  = names(rows),
    Valeur = vapply(rows, function(v) if (is.null(v) || is.na(v)) "" else as.character(v), character(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

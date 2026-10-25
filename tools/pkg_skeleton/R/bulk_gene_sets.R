# =============================================================================
# tools/pkg_skeleton/R/bulk_gene_sets.R — CATALOGUE de jeux de gènes NATIF (Bulk V2, PLOT-S6b)
# =============================================================================
# PROBLÈME RÉSOLU
# Les scores de voies par échantillon (GSVA/ssGSEA, `bulk_gsva.R`) exigeaient un
# fichier `.gmt` fourni par l'utilisateur : sans fichier externe, la
# fonctionnalité était inutilisable. Or l'application embarque DÉJÀ les
# ressources nécessaires — `msigdbr` (dont les données sont DANS le paquet,
# vérifié hors ligne le 2026-09-15 : Hallmark 50 jeux, C2:CP:REACTOME 1 839,
# C3:TFT:GTRD 506, C5:GO:BP 7 538, C6 189, C7:IMMUNESIGDB 4 872, C8 866 ;
# `msigdbdf` est ABSENT et n'est PAS requis) et `decoupleR` (PROGENy, DoRothEA).
#
# Ce fichier est la SOURCE UNIQUE de « d'où viennent les jeux de gènes » :
#   - `bulk_gene_set_catalog()`  : catalogue des sources natives (+ l'import
#     .gmt conservé pour les ressources maison) ;
#   - `bulk_load_gene_sets()`    : charge une source -> liste nommée
#     (nom du jeu -> vecteur de gènes), EXACTEMENT la forme que renvoie
#     `bulk_parse_gmt()` : rien en aval ne change ;
#   - `bulk_write_gmt()`         : export `.gmt`, pour que les jeux natifs
#     restent réutilisables HORS de l'application (reproductibilité).
#
# AUCUN appel réseau à l'exécution : msigdbr embarque ses données, decoupleR
# lit les paquets `progeny`/`dorothea` locaux. Local-first, comme
# `bulk_signatures.R` (qui délègue désormais son chargement MSigDB ici, pour
# qu'`msigdbr::msigdbr()` ne soit appelé QU'À UN SEUL endroit).
#
# Erreurs classées `bulk_gene_sets_error` (français, errorCondition, `state`) :
#   invalid_input | missing_dependency | compute_failed
# =============================================================================

#' Surface publique figée du domaine catalogue de jeux de gènes (gel)
bulk_gene_sets_public_api <- function() {
  c("bulk_gene_sets_public_api",
    "bulk_gene_set_catalog",
    "bulk_gene_set_choices",
    "bulk_gene_sets_organisms",
    "bulk_load_gene_sets",
    "bulk_write_gmt")
}

#' Organismes supportés par les sources natives
bulk_gene_sets_organisms <- function() c("human", "mouse")

#' Choix `source -> libellé` prêts pour un `selectInput()`
#'
#' Renvoie un vecteur NOMMÉ au sens de la convention C13 du dépôt :
#' `setNames(valeur, libellé)` — la VALEUR (l'identifiant de source) est ce que
#' Shiny renvoie, le LIBELLÉ est ce qu'il affiche. Écrire l'inverse ferait
#' retourner le texte traduit par `input$scores_source`.
#'
#' @param tr Fonction de traduction (`.tr_plain` côté UI) ; par défaut identité.
#' @return Vecteur character nommé (noms = libellés, valeurs = sources).
bulk_gene_set_choices <- function(tr = identity) {
  cat_df <- bulk_gene_set_catalog()
  stats::setNames(
    cat_df$source,
    vapply(cat_df$label, function(l) as.character(tr(l))[1], character(1),
           USE.NAMES = FALSE)
  )
}

#' Catalogue des sources de jeux de gènes
#'
#' @return data.frame(source, label, provider, collection, subcollection,
#'   requires, available, description). `label` et `description` sont des CLÉS
#'   i18n (convention du dépôt : la clé EST la chaîne française).
#'   `available` ne reflète que les paquets LOCALEMENT installés — aucun
#'   téléchargement n'est tenté, ni ici ni au chargement.
bulk_gene_set_catalog <- function() {
  has_msigdbr  <- requireNamespace("msigdbr", quietly = TRUE)
  # PLOT-S6b — PROGENy et DoRothEA sont lus DIRECTEMENT dans leurs paquets de
  # données locaux (`progeny::getModel()`, `dorothea::dorothea_hs`). On ne passe
  # PAS par `decoupleR::get_progeny()` / `get_dorothea()` : depuis decoupleR
  # 2.12.0 ces fonctions passent par `OmnipathR`, donc par le RÉSEAU, et
  # échouent sans lui (mesuré le 2026-09-15 : « there is no package called
  # 'OmnipathR' ») — incompatible avec la règle local-first du dépôt.
  has_progeny  <- requireNamespace("progeny", quietly = TRUE)
  has_dorothea <- requireNamespace("dorothea", quietly = TRUE)

  data.frame(
    source = c("msigdb_hallmark", "msigdb_reactome", "msigdb_kegg",
               "msigdb_go_bp", "msigdb_tft", "msigdb_immunesigdb",
               "msigdb_oncogenic", "msigdb_celltype",
               "progeny", "dorothea", "file"),
    label = c("MSigDB Hallmark (50 voies)",
              "MSigDB Reactome (C2)",
              "MSigDB KEGG (C2, historique)",
              "MSigDB GO Biological Process (C5)",
              "MSigDB Facteurs de transcription (C3)",
              "MSigDB signatures immunitaires (C7)",
              "MSigDB signatures oncogéniques (C6)",
              "MSigDB types cellulaires (C8)",
              "PROGENy (activités de voies)",
              "DoRothEA (réseau de facteurs de transcription)",
              "Fichier .gmt fourni"),
    provider = c(rep("msigdbr", 8), "decoupleR", "decoupleR", "user"),
    collection = c("H", "C2", "C2", "C5", "C3", "C7", "C6", "C8", NA, NA, NA),
    subcollection = c(NA, "CP:REACTOME", "CP:KEGG_LEGACY", "GO:BP",
                      "TFT:GTRD", "IMMUNESIGDB", NA, NA, NA, NA, NA),
    requires = c(rep("msigdbr", 8), "progeny",
                 "dorothea", ""),
    available = c(rep(has_msigdbr, 8), has_progeny, has_dorothea, TRUE),
    description = c(
      "50 ensembles « hallmark » : les processus biologiques de référence.",
      "Voies Reactome (curées, orientées réactions).",
      "Voies KEGG historiques (MSigDB les a retirées ; sous-collection « legacy »).",
      "Termes GO « biological process » — très granulaire, milliers de jeux.",
      "Cibles de facteurs de transcription (GTRD).",
      "Signatures d'états immunitaires (module immuneSigDB).",
      "Signatures oncogéniques (perturbations).",
      "Signatures de types cellulaires.",
      "Activités de voies par échantillon (14 voies).",
      "Cibles de facteurs de transcription (confiance A/B/C).",
      "Jeux de gènes maison : nom<TAB>description<TAB>gènes..."),
    stringsAsFactors = FALSE
  )
}

#' Lever une erreur classée du domaine
.bulk_gs_error <- function(msg, state) {
  stop(errorCondition(msg, class = "bulk_gene_sets_error", state = state))
}

#' Traduire un organisme en nom d'espèce msigdbr
.bulk_gs_msigdb_species <- function(organism) {
  if (identical(organism, "mouse")) "Mus musculus" else "Homo sapiens"
}

#' Charger une collection MSigDB en liste nommée (nom de jeu -> gènes)
#'
#' Appel msigdbr UNIQUE du dépôt (bulk_signatures.R délègue ici). L'API msigdbr
#' a changé de nom d'argument en cours de route (`category` -> `collection`) :
#' les deux sont tentés, sans repli silencieux — l'échec des deux est une erreur
#' classée qui cite la cause réelle.
.bulk_gs_msigdb_sets <- function(collection, subcollection, organism) {
  # Les collections sans sous-collection (H, C6, C8) portent NA dans le
  # catalogue : msigdbr REFUSE NA (assert_that « missing values present ») et
  # exige NULL. La normalisation est faite ici, au plus près de l'appel, pour
  # qu'aucun appelant n'ait à y penser.
  if (length(subcollection) != 1L || is.na(subcollection) ||
      !nzchar(as.character(subcollection))) subcollection <- NULL
  sp <- .bulk_gs_msigdb_species(organism)
  call_with <- function(fun) {
    if (is.null(subcollection)) fun(sp, collection) else fun(sp, collection, subcollection)
  }
  df <- tryCatch(
    call_with(function(sp, coll, sub = NULL) {
      if (is.null(sub)) msigdbr::msigdbr(species = sp, collection = coll)
      else msigdbr::msigdbr(species = sp, collection = coll, subcollection = sub)
    }),
    error = function(e1) tryCatch(
      call_with(function(sp, coll, sub = NULL) {
        if (is.null(sub)) msigdbr::msigdbr(species = sp, category = coll)
        else msigdbr::msigdbr(species = sp, category = coll, subcategory = sub)
      }),
      error = function(e2) .bulk_gs_error(
        paste0("Chargement MSigDB impossible (", collection,
               if (is.null(subcollection)) "" else paste0("/", subcollection),
               ") : ", conditionMessage(e2)),
        "compute_failed")
    )
  )
  gene_col <- if ("gene_symbol" %in% colnames(df)) "gene_symbol" else
    if ("db_gene_symbol" %in% colnames(df)) "db_gene_symbol" else
      if ("gene_entrez" %in% colnames(df)) "gene_entrez" else NULL
  if (is.null(gene_col)) {
    .bulk_gs_error("Colonnes de gènes inattendues dans la table msigdbr.",
                   "compute_failed")
  }
  sets <- split(as.character(df[[gene_col]]), df$gs_name)
  sets[vapply(sets, length, integer(1)) > 0L]
}

#' PROGENy — jeux de gènes (voie -> cibles) lus LOCALEMENT
#'
#' Le modèle embarqué dans le paquet `progeny` (14 voies x ~1300 gènes) est lu
#' par `progeny::getModel()`. AUCUN appel réseau : on n'utilise PAS
#' `decoupleR::get_progeny()`, qui depuis decoupleR 2.12.0 passe par OmnipathR
#' et échoue sans lui (mesuré).
.bulk_gs_progeny_sets <- function(organism, top = 500L) {
  org <- if (identical(organism, "mouse")) "Mouse" else "Human"
  m <- tryCatch(progeny::getModel(organism = org, top = top),
                error = function(e) .bulk_gs_error(
                  paste0("PROGENy (", org, ") illisible : ", conditionMessage(e)),
                  "compute_failed"))
  if (is.null(dim(m)) || length(dim(m)) != 2L || ncol(m) < 1L || nrow(m) < 1L) {
    .bulk_gs_error("PROGENy : modèle inattendu (matrice de poids attendue).",
                   "compute_failed")
  }
  genes <- rownames(m)
  if (is.null(genes) || !any(nzchar(genes))) {
    .bulk_gs_error("PROGENy : gènes absents du modèle.", "compute_failed")
  }
  .bulk_gs_progeny_sets_from_matrix(m, top = top)
}

#' Découper une matrice de poids PROGENy en jeux de gènes PAR VOIE
#'
#' `m` : matrice gènes x voies rendue par `progeny::getModel()`. Un gène peut
#' peser 0 dans une voie (il n'en est pas une cible) : il doit alors être exclu
#' de CETTE voie seulement, jamais des autres.
#'
#' ⚠️ Défaut historique (M5, corrigé le 2026-10-17) : la version précédente
#' rendait `lapply(seq_len(ncol(m)), function(j) genes)` — l'indice `j` n'était
#' jamais lu, donc les 14 voies recevaient l'UNION des lignes du modèle au lieu
#' de leurs ~`top` cibles. Les scores de voies devenaient quasi colinéaires,
#' silencieusement (aucune erreur, aucun avertissement, des résultats
#' plausibles).
#'
#' Fonction PURE (aucune dépendance à `progeny`) : c'est ce qui rend le défaut
#' testable sur un hôte où le paquet est absent.
#'
#' Précondition : `rownames(m)` non NULL — vérifié par `.bulk_gs_progeny_sets()`.
.bulk_gs_progeny_sets_from_matrix <- function(m, top = 500L) {
  n_top <- suppressWarnings(as.integer(top))
  if (length(n_top) != 1L || is.na(n_top) || n_top < 1L) n_top <- 500L
  pws <- colnames(m)
  if (is.null(pws)) pws <- paste0("pathway", seq_len(ncol(m)))
  g <- rownames(m)
  stats::setNames(lapply(seq_len(ncol(m)), function(j) {
    w    <- suppressWarnings(as.numeric(m[, j]))
    keep <- which(is.finite(w) & w != 0)
    if (length(keep) == 0L) return(character(0))
    keep <- keep[order(abs(w[keep]), decreasing = TRUE)]
    g[utils::head(keep, n_top)]
  }), pws)
}

#' DoRothEA — jeux de gènes (facteur de transcription -> cibles) LOCAUX
#'
#' Lit le jeu de données embarqué dans le paquet `dorothea` (`dorothea_hs` /
#' `dorothea_mm`) et le réduit aux niveaux de confiance demandés. AUCUN appel
#' réseau : on n'utilise PAS `decoupleR::get_dorothea()` (OmnipathR).
.bulk_gs_dorothea_sets <- function(organism, levels = c("A", "B", "C")) {
  nm <- if (identical(organism, "mouse")) "dorothea_mm" else "dorothea_hs"
  e  <- new.env()
  ok <- tryCatch({ utils::data(list = nm, package = "dorothea", envir = e); TRUE },
                 error = function(err) FALSE)
  if (!ok || !exists(nm, envir = e, inherits = FALSE)) {
    .bulk_gs_error(paste0("DoRothEA : jeu de données '", nm,
                          "' introuvable dans le paquet dorothea."), "compute_failed")
  }
  df <- get(nm, envir = e)
  if (!is.data.frame(df) || !all(c("tf", "target", "confidence") %in% colnames(df))) {
    .bulk_gs_error("DoRothEA : colonnes inattendues (tf / target / confidence attendues).",
                   "compute_failed")
  }
  df <- df[as.character(df$confidence) %in% levels, , drop = FALSE]
  if (nrow(df) == 0L) {
    .bulk_gs_error("DoRothEA : aucun régulateur aux niveaux de confiance demandés.",
                   "compute_failed")
  }
  split(as.character(df$target), as.character(df$tf))
}

#' Charger des jeux de gènes NATIFS (aucun fichier externe, aucun réseau)
#'
#' @param source Identifiant du catalogue (`bulk_gene_set_catalog()$source`).
#'   `"file"` n'est PAS traité ici : l'import `.gmt` reste le rôle de
#'   `bulk_parse_gmt()`.
#' @param organism "human" ou "mouse" (voir `bulk_gene_sets_organisms()`).
#' @return Liste nommée (nom du jeu -> vecteur character de gènes), de la même
#'   forme que `bulk_parse_gmt()`.
bulk_load_gene_sets <- function(source = "msigdb_hallmark", organism = "human") {
  cat_df <- bulk_gene_set_catalog()
  if (!is.character(source) || length(source) != 1L || is.na(source) ||
      !source %in% cat_df$source) {
    .bulk_gs_error(sprintf(
      "bulk_load_gene_sets() : source '%s' inconnue (disponibles : %s).",
      paste(source, collapse = ", "), paste(cat_df$source, collapse = ", ")),
      "invalid_input")
  }
  if (!identical(source, "file")) {
    if (!is.character(organism) || length(organism) != 1L || is.na(organism) ||
        !organism %in% bulk_gene_sets_organisms()) {
      .bulk_gs_error(sprintf(
        "bulk_load_gene_sets() : organisme '%s' non supporté (attendu : %s).",
        paste(organism, collapse = ", "),
        paste(bulk_gene_sets_organisms(), collapse = ", ")),
        "invalid_input")
    }
  }
  row <- cat_df[cat_df$source == source, , drop = FALSE]
  if (!isTRUE(row$available)) {
    .bulk_gs_error(paste0(
      "bulk_load_gene_sets() : la source '", source,
      "' n'est pas disponible localement (requis : ", row$requires,
      "). Installez le(s) package(s) — aucun téléchargement n'est effectué."),
      "missing_dependency")
  }
  if (identical(source, "file")) {
    .bulk_gs_error(
      "bulk_load_gene_sets() : la source 'file' se charge avec bulk_parse_gmt().",
      "invalid_input")
  }

  sets <- if (identical(row$provider, "msigdbr")) {
    .bulk_gs_msigdb_sets(row$collection, row$subcollection, organism)
  } else if (identical(source, "progeny")) {
    .bulk_gs_progeny_sets(organism)
  } else if (identical(source, "dorothea")) {
    .bulk_gs_dorothea_sets(organism)
  } else {
    .bulk_gs_error(sprintf("bulk_load_gene_sets() : source '%s' non câblée.", source),
                   "compute_failed")
  }

  sets <- lapply(sets, function(g) unique(as.character(g)))
  sets <- sets[vapply(sets, length, integer(1)) > 0L]
  if (length(sets) == 0L) {
    .bulk_gs_error(sprintf(
      "bulk_load_gene_sets() : la source '%s' n'a rendu aucun jeu de gènes exploitable.",
      source), "compute_failed")
  }
  sets
}

#' Écrire des jeux de gènes au format .gmt
#'
#' Format : `nom<TAB>description<TAB>gène1<TAB>gène2...` (une ligne par jeu) —
#' exactement ce que `bulk_parse_gmt()` relit, donc l'aller-retour est garanti.
#' Les doublons de gènes sont supprimés ; les jeux sans gène sont rejetés
#' (plutôt qu'écrits vides, qu'un lecteur tiers interpréterait de travers).
#'
#' @param gene_sets Liste nommée de vecteurs de gènes.
#' @param path Chemin de sortie.
#' @return Invisiblement le chemin normalisé.
bulk_write_gmt <- function(gene_sets, path) {
  if (!is.list(gene_sets) || length(gene_sets) == 0L) {
    .bulk_gs_error("bulk_write_gmt() : liste de jeux de gènes vide.", "invalid_input")
  }
  if (is.null(names(gene_sets)) || any(!nzchar(names(gene_sets)))) {
    .bulk_gs_error("bulk_write_gmt() : chaque jeu doit porter un nom non vide.",
                   "invalid_input")
  }
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    .bulk_gs_error("bulk_write_gmt() : chemin de sortie invalide.", "invalid_input")
  }
  genes <- lapply(gene_sets, function(g) {
    g <- unique(as.character(g))
    g[!is.na(g) & nzchar(g)]
  })
  keep <- vapply(genes, length, integer(1)) > 0L
  if (!any(keep)) {
    .bulk_gs_error("bulk_write_gmt() : aucun jeu ne contient de gène.", "invalid_input")
  }
  genes <- genes[keep]
  lines <- vapply(seq_along(genes), function(i) {
    paste(c(names(genes)[i], "ts_export", genes[[i]]), collapse = "\t")
  }, character(1))
  ok <- tryCatch({ writeLines(lines, path, useBytes = TRUE); TRUE },
                 error = function(e) FALSE)
  if (!isTRUE(ok)) {
    .bulk_gs_error(sprintf("bulk_write_gmt() : écriture impossible (%s).", path),
                   "invalid_input")
  }
  invisible(normalizePath(path, winslash = "/", mustWork = FALSE))
}

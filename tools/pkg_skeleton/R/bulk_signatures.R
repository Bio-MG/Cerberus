# =============================================================================
# tools/pkg_skeleton/R/bulk_signatures.R — Scores de SIGNATURES CELLULAIRES (MSigDB Hallmark,
# PROGENy, DoRothEA, RDS local) — roadmap Bulk V2, Milestone 3 (chantier Flux E).
# =============================================================================
# PRINCIPE : score des signatures par échantillon (réutilisation maximale du
# moteur M2 `compute_pathway_scores()` — ssgsea/gsva/zscore) ou decoupleR
# (ulm) s'il est installé. Les ressources sont LOCALES : msigdbr embarque ses
# données dans le package ; decoupleR::get_progeny()/get_dorothea() lisent les
# packages locaux du même écosystème ; RDS local = fichier fourni par
# l'utilisateur. AUCUN appel réseau à l'exécution (local-first).
#
# GARDE ABSOLU (mission §M3) : les scores de signatures sont RELATIFS —
# estimation d'abondance relative ne remplaçant PAS une quantification
# cytométrique. L'avertissement est porté par :
#   - le résultat (champ `disclaimer`, non effaçable),
#   - CHAQUE export (colonne `disclaimer`),
#   - l'UI (alerte permanente du module).
#
# Erreurs classées `bulk_signatures_error` (français, errorCondition, state).
# =============================================================================

#' Avertissement contractuel porté par le résultat, les figures et les exports
BULK_SIGNATURES_DISCLAIMER <- paste0(
  "Scores de signatures relatifs : estimation d'abondance relative ",
  "ne rempla\u00e7ant pas une quantification cytom\u00e9trique.")

#' Surface publique figée du domaine signatures (gel par test de freeze)
bulk_signatures_public_api <- function() {
  c("bulk_signatures_public_api",
    "bulk_signature_resources",
    "bulk_load_signatures",
    "bulk_score_signatures",
    "build_signature_scores_export")
}

#' Ressources de signatures disponibles (détection locale, jamais réseau)
#'
#' @return data.frame(resource, label, available, requires, description) —
#'   `available` reflète uniquement les packages LOCALEMENT installés.
bulk_signature_resources <- function() {
  has_msigdbr <- requireNamespace("msigdbr", quietly = TRUE)
  has_decoupler <- requireNamespace("decoupleR", quietly = TRUE)
  has_progeny <- has_decoupler && requireNamespace("progeny", quietly = TRUE)
  has_dorothea <- has_decoupler && requireNamespace("dorothea", quietly = TRUE)
  data.frame(
    resource   = c("hallmark", "progeny", "dorothea", "rds_local"),
    label      = c("MSigDB Hallmark", "PROGENy", "DoRothEA", "RDS local"),
    available  = c(has_msigdbr, has_progeny, has_dorothea, TRUE),
    requires   = c("msigdbr", "decoupleR + progeny", "decoupleR + dorothea", ""),
    description = c(
      "50 ensembles de gènes « hallmark » MSigDB (embarqués dans msigdbr).",
      "Pangénome d'activités de voies (14 voies).",
      "Réseau régulateur transcriptionnel (interactions TF -> cibles).",
      "Liste nommée de vecteurs de gènes, ou data.frame (colonnes signature, gene)."
    ),
    stringsAsFactors = FALSE
  )
}

#' Charger une ressource de signatures (LOCAL — assertion réseau sortant = 0)
#'
#' @param resource Une de bulk_signature_resources()$resource. Pour
#'   `rds_local`, voir `rds_path`.
#' @param organism "human" ou "mouse" (hallmark / progeny / dorothea).
#' @param rds_path Chemin d'un .rds local (resource = "rds_local") : liste
#'   nommée de vecteurs de gènes OU data.frame avec colonnes `signature`/`gene`.
#' @return Liste nommée (signature -> vecteur de gènes) — celle de DoRothEA est
#'   réduite aux interactions de confiance >= C (graded) par défaut.
bulk_load_signatures <- function(resource = "hallmark", organism = "human",
                                 rds_path = NULL) {
  res <- bulk_signature_resources()
  if (!resource %in% res$resource) {
    stop(errorCondition(sprintf(
      "bulk_load_signatures() : ressource '%s' inconnue (disponibles : %s).",
      resource, paste(res$resource, collapse = ", ")),
      class = "bulk_signatures_error", state = "invalid_input"))
  }
  avail <- res$available[res$resource == resource]
  if (!isTRUE(avail) && resource != "rds_local") {
    req_pkgs <- res$requires[res$resource == resource]
    stop(errorCondition(paste0(
      "bulk_load_signatures() : la ressource '", resource,
      "' n'est pas disponible localement (requis : ", req_pkgs,
      "). Installez le(s) package(s) — aucun t\u00e9l\u00e9chargement n'est effectu\u00e9."),
      class = "bulk_signatures_error", state = "missing_dependency"))
  }

  sets <- switch(resource,
    hallmark = {
      # Chargement MSigDB DELEGUE a bulk_gene_sets.R : `msigdbr::msigdbr()` n'est
      # appele qu'a UN SEUL endroit du depot (regle 3 — etendre, ne pas
      # dupliquer). Le catalogue y gere aussi les collections autres que
      # Hallmark, la traduction d'organisme et la normalisation NA -> NULL des
      # sous-collections. L'echec est retraduit dans la classe de CE domaine :
      # les appelants de bulk_load_signatures() n'ont rien a changer.
      tryCatch(
        bulk_load_gene_sets("msigdb_hallmark", organism),
        error = function(e) stop(errorCondition(paste0(
          "bulk_load_signatures() : msigdbr a \u00e9chou\u00e9 — ", conditionMessage(e)),
          class = "bulk_signatures_error", state = "compute_failed")))
    },
    progeny = {
      # PLOT-S6b — lecture LOCALE via bulk_gene_sets.R. `decoupleR::get_progeny()`
      # passe par OmnipathR (réseau) depuis decoupleR 2.12.0 et échoue sans lui :
      # mesuré le 2026-09-15 (« there is no package called 'OmnipathR' »). Le
      # modèle embarqué du paquet `progeny` est lu directement.
      tryCatch(
        bulk_load_gene_sets("progeny", organism),
        error = function(e) stop(errorCondition(paste0(
          "bulk_load_signatures() : PROGENy a \u00e9chou\u00e9 — ", conditionMessage(e)),
          class = "bulk_signatures_error", state = "compute_failed")))
    },
    dorothea = {
      # Idem : `dorothea::dorothea_hs` (local) plutôt que decoupleR/OmnipathR.
      tryCatch(
        bulk_load_gene_sets("dorothea", organism),
        error = function(e) stop(errorCondition(paste0(
          "bulk_load_signatures() : DoRothEA a \u00e9chou\u00e9 — ", conditionMessage(e)),
          class = "bulk_signatures_error", state = "compute_failed")))
    },
    rds_local = {
      if (is.null(rds_path) || !file.exists(rds_path)) {
        stop(errorCondition(
          "bulk_load_signatures() : fichier .rds local introuvable.",
          class = "bulk_signatures_error", state = "invalid_input"))
      }
      obj <- tryCatch(readRDS(rds_path), error = function(e) NULL)
      if (is.null(obj)) {
        stop(errorCondition("bulk_load_signatures() : lecture du .rds impossible.",
                            class = "bulk_signatures_error", state = "invalid_input"))
      }
      if (is.list(obj) && !is.data.frame(obj)) {
        out <- obj
      } else if (is.data.frame(obj) && all(c("signature", "gene") %in% colnames(obj))) {
        out <- split(as.character(obj$gene), as.character(obj$signature))
      } else {
        stop(errorCondition(paste0(
          "bulk_load_signatures() : le .rds doit \u00eatre une liste nomm\u00e9e de vecteurs ",
          "de g\u00e8nes ou un data.frame (colonnes signature, gene)."),
          class = "bulk_signatures_error", state = "invalid_input"))
      }
      out <- lapply(out, function(g) unique(as.character(g)))
      out[vapply(out, function(g) length(g) > 0L, logical(1))]
    }
  )

  if (length(sets) == 0L) {
    stop(errorCondition("bulk_load_signatures() : aucune signature charg\u00e9e.",
                        class = "bulk_signatures_error", state = "no_gene_sets"))
  }
  sets
}

#' Score des signatures par échantillon (avertissement relatif embarqué)
#'
#' Réutilise `compute_pathway_scores()` (moteur M2 : ssgsea / gsva / zscore,
#`  mêmes gardes — matrice transformée, identifiants nettoyés, porte de
#' recouvrement, BPPARAM discipliné) ou decoupleR::run_ulm si demandé et
#' installé. Le champ `disclaimer` est TOUJOURS présent dans le résultat.
#'
#' @param expr_matrix Matrice transformée (gènes x échantillons).
#' @param gene_sets Liste nommée de signatures (bulk_load_signatures()).
#' @param method "ssgsea", "gsva", "zscore" (moteur M2) ou "ulm_decoupleR".
#' @param ... Passé à compute_pathway_scores() (min_size, max_size,
#'   overlap_min, workers).
#' @return list(type = "bulk_signature_scores", status, analysis_id, method,
#'   resource (via attr), scores, gene_sets, qc, warnings, disclaimer,
#'   provenance, timestamp_utc) — même architecture que le contrat M2.
bulk_score_signatures <- function(expr_matrix, gene_sets, method = "ssgsea", ...) {
  if (!method %in% c("ssgsea", "gsva", "zscore", "ulm_decoupleR")) {
    stop(errorCondition(sprintf(
      "bulk_score_signatures() : méthode '%s' inconnue (ssgsea, gsva, zscore, ulm_decoupleR).",
      method),
      class = "bulk_signatures_error", state = "invalid_input"))
  }
  if (!is.list(gene_sets) || length(gene_sets) == 0L) {
    stop(errorCondition("bulk_score_signatures() : liste de signatures vide.",
                        class = "bulk_signatures_error", state = "invalid_input"))
  }

  if (identical(method, "ulm_decoupleR")) {
    if (!requireNamespace("decoupleR", quietly = TRUE)) {
      stop(errorCondition(paste0(
        "bulk_score_signatures() : le package 'decoupleR' est requis pour la méthode ulm ",
        "(BiocManager::install('decoupleR')) — sinon utilisez ssgsea/gsva/zscore."),
        class = "bulk_signatures_error", state = "missing_dependency"))
    }
    bulk_assert_transformed_matrix(expr_matrix, context = "scores de signatures")
    filt <- bulk_filter_gene_sets(
      gene_sets, rownames(expr_matrix),
      min_size = if (exists("TS_BULK_GSVA_MIN_SIZE", inherits = TRUE)) TS_BULK_GSVA_MIN_SIZE else 10L,
      max_size = if (exists("TS_BULK_GSVA_MAX_SIZE", inherits = TRUE)) TS_BULK_GSVA_MAX_SIZE else 500L,
      overlap_min = if (exists("TS_BULK_GSVA_OVERLAP_MIN", inherits = TRUE)) TS_BULK_GSVA_OVERLAP_MIN else 0.20)
    warnings <- character(0)
    if (nrow(filt$dropped) > 0L) {
      warnings <- c(warnings, sprintf(
        "%d signature(s) rejet\u00e9e(s) (recouvrement/taille) — voir qc$dropped.",
        nrow(filt$dropped)))
    }
    if (length(filt$sets) == 0L) {
      stop(errorCondition("bulk_score_signatures() : aucune signature ne survit aux filtres.",
                          class = "bulk_signatures_error", state = "no_gene_sets"))
    }
    net <- data.frame(
      source = rep(names(filt$sets), vapply(filt$sets, length, integer(1))),
      target = unlist(filt$sets, use.names = FALSE),
      weight = 1,
      stringsAsFactors = FALSE
    )
    # decoupleR convention (vérifiée dans intersect_regulons) : les FEATURES
    # (gènes) sont en ROWS de mat — on passe expr_matrix SANS transposition.
    scores_df <- tryCatch(
      decoupleR::run_ulm(mat = expr_matrix, net = net, .source = "source"),
      error = function(e) stop(errorCondition(paste0(
        "bulk_score_signatures() : decoupleR a échoué — ", conditionMessage(e)),
        class = "bulk_signatures_error", state = "compute_failed")))
    # run_ulm -> tibble (statistic, source, condition, score) : pivot en matrice
    if (is.null(scores_df) || !all(c("source", "condition", "score") %in% colnames(scores_df))) {
      stop(errorCondition("bulk_score_signatures() : format decoupleR inattendu.",
                          class = "bulk_signatures_error", state = "compute_failed"))
    }
    scores_wide <- reshape2::dcast(as.data.frame(scores_df), source ~ condition,
                                   value.var = "score", fun.aggregate = mean)
    if (nrow(scores_wide) == 0L || !all(colnames(expr_matrix) %in% colnames(scores_wide))) {
      stop(errorCondition("bulk_score_signatures() : format decoupleR inattendu.",
                          class = "bulk_signatures_error", state = "compute_failed"))
    }
    scores <- as.matrix(scores_wide[, -1, drop = FALSE])
    rownames(scores) <- as.character(scores_wide$source)
    scores <- scores[sort(rownames(scores)), colnames(expr_matrix), drop = FALSE]
    n_genes_matched <- length(unique(net$target))
    prov_method <- "decoupleR::run_ulm"
    filt_sets <- filt$sets
    dropped <- data.frame(set = character(0), n_genes = integer(0),
                          n_matched = integer(0), matched_fraction = numeric(0),
                          reason = character(0), stringsAsFactors = FALSE)
    qc <- list(n_input_sets = length(gene_sets), n_used_sets = length(filt$sets),
               dropped = dropped, n_genes_input = nrow(expr_matrix),
               n_genes_matched = n_genes_matched, n_samples = ncol(expr_matrix))
  } else {
    # Le moteur M2 relève des erreurs classées bulk_gsva_error — re-classées
    # dans CE domaine (même message, même state) pour un catch unique côté UI.
    engine_result <- tryCatch(
      compute_pathway_scores(expr_matrix, gene_sets, method = method, ...),
      error = function(e) {
        stop(errorCondition(conditionMessage(e),
                            class = "bulk_signatures_error",
                            state = e$state %||% "invalid_input"))
      })
    scores <- engine_result$scores
    warnings <- engine_result$warnings
    prov_method <- engine_result$provenance$method
    filt_sets <- engine_result$gene_sets
    qc <- engine_result$qc
  }

  provenance <- new_provenance_entry(
    analysis_id = "bulk-signature-scores",
    method      = prov_method,
    parameters  = list(method = method, n_signatures = nrow(scores),
                       disclaimer = BULK_SIGNATURES_DISCLAIMER),
    dataset     = expr_matrix,
    warnings    = warnings
  )

  list(
    type          = "bulk_signature_scores",
    status        = "valid",
    analysis_id   = "bulk-signature-scores",
    method        = method,
    scores        = scores,
    gene_sets     = filt_sets,
    qc            = qc,
    warnings      = warnings,
    disclaimer    = BULK_SIGNATURES_DISCLAIMER,
    provenance    = provenance,
    timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

#' Export plat des scores de signatures (une ligne par signature x échantillon)
#'
#' Comme le contrat M2, PLUS la colonne `disclaimer` portée par CHAQUE ligne
#' (garde §M3 — l'avertissement voyage avec les données exportées).
#'
#' @param result Résultat bulk_score_signatures().
#' @return data.frame(signature, sample, score, method, analysis_id, disclaimer).
build_signature_scores_export <- function(result) {
  if (!is.list(result) || !identical(result$type, "bulk_signature_scores")) {
    stop(errorCondition("build_signature_scores_export() : résultat de signatures invalide.",
                        class = "bulk_signatures_error", state = "invalid_input"))
  }
  s <- result$scores
  grid <- expand.grid(signature = rownames(s), sample = colnames(s),
                      stringsAsFactors = FALSE)
  grid$score    <- as.vector(s[cbind(match(grid$signature, rownames(s)),
                                     match(grid$sample, colnames(s)))])
  grid$method   <- result$method
  grid$analysis_id <- result$analysis_id
  grid$disclaimer <- result$disclaimer %||% BULK_SIGNATURES_DISCLAIMER
  grid[order(grid$signature, grid$sample), ]
}

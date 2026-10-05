# =============================================================================
# R/bulk/bulk_wgcna.R — Réseaux de co-expression WGCNA SAFE-MODE
# (roadmap Bulk V2, Milestone 4 — chantier Flux E).
# =============================================================================
# GARDES DE LA MISSION (§2.4 — non négociables) :
#   1. ARRÊT DUR si N < TS_BULK_WGCNA_MIN_SAMPLES (15) — message citant N et
#      le plancher (jamais de continuation silencieuse sur petit N).
#   2. Pré-filtrage OBLIGATOIRE aux gènes les plus variables : bornes
#      [TS_BULK_WGCNA_MIN_GENES, TS_BULK_WGCNA_MAX_GENES] = [2000, 5000].
#   3. Jamais de TOM complet non borné : WGCNA::blockwiseModules() avec
#      maxBlockSize = TS_BULK_WGCNA_MAX_BLOCKSIZE (calibré RAM 32 Go).
#   4. JAMAIS WGCNA::enableWGCNAThreads() — disableWGCNAThreads() avant chaque
#      calcul (règle dépôt STATUS.md §2h : allowWGCNAThreads(nThreads=1) si
#      mirai ; disable est l'équivalent strictement plus sûr).
#   5. Corrélations ME <-> traits par biweight midcorrobération (WGCNA::bicor),
#      repli stats::cor documenté si WGCNA absent.
#   6. Matrice TRANSFORMÉE exigée (garde réutilisée du domaine QC batch).
#
# Erreurs classées `bulk_wgcna_error` (français, errorCondition, state).
# =============================================================================

#' Surface publique figée du domaine WGCNA (gel par test de freeze)
bulk_wgcna_public_api <- function() {
  c("bulk_wgcna_public_api",
    "bulk_wgcna_select_hvg",
    "bulk_wgcna_choose_power",
    "bulk_wgcna_pick_power",
    "bulk_wgcna_build_modules",
    "bulk_wgcna_prepare_traits",
    "bulk_wgcna_module_trait",
    "plot_wgcna_soft_threshold",
    "plot_wgcna_trait_heatmap",
    "build_wgcna_export")
}

#' Attacher WGCNA le temps d'un appel, puis restaurer le search path
#'
#' QUIRK CONNU (vérifié 2026-09-12, WGCNA 1.74 / R 4.4.2) :
#' `WGCNA::blockwiseModules()` appelé NAMESPACÉ échoue avec
#' « unused arguments (weights.x, weights.y, cosine) » — le `cor()` interne de
#' blockwiseModules doit résoudre vers `WGCNA::cor` via le search path. WGCNA
#' doit donc être ATTACHÉ pendant l'appel. Ce helper attache si nécessaire et
#' DETACHE systématiquement à la sortie (aucun état global persistant —
#' leçon STATUS.md §2k sur les `.onLoad()`).
#'
#' @param expr Expression à évaluer avec WGCNA attaché.
#' @return Valeur de expr.
.wgcna_with_attached <- function(expr) {
  was_attached <- any(search() == "package:WGCNA")
  if (!was_attached) {
    ok <- suppressPackageStartupMessages(
      require("WGCNA", quietly = TRUE, character.only = TRUE))
    if (!ok) {
      stop(errorCondition("Échec WGCNA : le package 'WGCNA' n'a pas pu être chargé.",
                          class = "bulk_wgcna_error", state = "missing_dependency"))
    }
    on.exit(suppressWarnings(detach("package:WGCNA", unload = TRUE)), add = TRUE)
  }
  force(expr)
}

#' Pré-filtrage aux gènes les plus variables (bornes mission en dur)
#'
#' @param vst_matrix Matrice transformée (gènes x échantillons).
#' @param n_top Nombre de gènes demandé (borné aux seuils config).
#' @return Liste(mat = gènes retenus, n_input, n_used, seed, warnings).
bulk_wgcna_select_hvg <- function(vst_matrix, n_top = NULL) {
  # Garde anti-counts-bruts réutilisée du domaine QC batch — re-classée dans
  # CE domaine (même message, même state) pour un catch unique côté UI.
  tryCatch(
    bulk_assert_transformed_matrix(vst_matrix, context = "WGCNA"),
    error = function(e) {
      stop(errorCondition(conditionMessage(e),
                          class = "bulk_wgcna_error",
                          state = e$state %||% "invalid_input"))
    }
  )
  n_top <- n_top %||%
    (if (exists("TS_BULK_WGCNA_MAX_GENES", inherits = TRUE)) TS_BULK_WGCNA_MAX_GENES else 5000L)
  n_top <- max(1L, as.integer(n_top))

  min_g <- if (exists("TS_BULK_WGCNA_MIN_GENES", inherits = TRUE)) TS_BULK_WGCNA_MIN_GENES else 2000L
  max_g <- if (exists("TS_BULK_WGCNA_WGCNA_MAX_GENES", inherits = TRUE)) TS_BULK_WGCNA_MAX_GENES else 5000L
  max_g <- max(min_g, max_g)
  n_top <- min(max(n_top, min_g), max_g)

  n_input <- nrow(vst_matrix)
  warnings <- character(0)
  if (n_input < min_g) {
    stop(errorCondition(sprintf(paste0(
      "Échec WGCNA : %d gènes disponibles, le pré-filtrage exige au moins %d gènes variables ",
      "(bornes de la mission : [%d, %d]). Réduisez le filtrage de l'étape 1 ou renoncez à WGCNA."),
      n_input, min_g, min_g, max_g),
      class = "bulk_wgcna_error", state = "too_few_genes"))
  }
  if (n_input > n_top) {
    rv  <- matrixStats::rowVars(vst_matrix)
    ord <- order(rv, decreasing = TRUE, method = "radix")
    keep <- sort(ord[seq_len(n_top)])
    mat <- vst_matrix[keep, , drop = FALSE]
    warnings <- c(warnings, sprintf(
      "Pré-filtrage : %d -> %d gènes les plus variables (bornes mission [%d, %d]).",
      n_input, n_top, min_g, max_g))
  } else {
    mat <- vst_matrix
  }
  list(mat = mat, n_input = n_input, n_used = nrow(mat), warnings = warnings)
}

#' Choix du power de soft-thresholding (logique pure, testable sans WGCNA)
#'
#' Premier power (croissant) dont le fit scale-free atteint `r2_min`
#' (TS_BULK_WGCNA_R2_MIN = 0.80) ; si aucun n'y parvient, le power au meilleur
#' R² avec un avertissement (jamais de valeur fabriquée).
#'
#' @param power_table data.frame pickSoftThreshold (colonnes Power, SFT.R.sq,
#'   mean.k.).
#' @param r2_min Cible de fit scale-free.
#' @return list(power, r2, mean_k, target_reached, warning).
bulk_wgcna_choose_power <- function(power_table, r2_min = NULL) {
  if (is.null(power_table) || !is.data.frame(power_table) || nrow(power_table) == 0L ||
      !all(c("Power", "SFT.R.sq") %in% colnames(power_table))) {
    stop(errorCondition("bulk_wgcna_choose_power() : table du power invalide.",
                        class = "bulk_wgcna_error", state = "invalid_input"))
  }
  r2_min <- r2_min %||%
    (if (exists("TS_BULK_WGCNA_R2_MIN", inherits = TRUE)) TS_BULK_WGCNA_R2_MIN else 0.80)
  ok <- !is.na(power_table$SFT.R.sq) & power_table$SFT.R.sq >= r2_min
  if (any(ok)) {
    i <- which(ok)[1]
    return(list(power = power_table$Power[i], r2 = power_table$SFT.R.sq[i],
                mean_k = if ("mean.k." %in% colnames(power_table)) power_table$mean.k.[i] else NA_real_,
                target_reached = TRUE, warning = NA_character_))
  }
  i <- which.max(power_table$SFT.R.sq)
  list(power = power_table$Power[i], r2 = power_table$SFT.R.sq[i],
       mean_k = if ("mean.k." %in% colnames(power_table)) power_table$mean.k.[i] else NA_real_,
       target_reached = FALSE, warning = sprintf(
         paste0("Aucun power n'atteint le fit scale-free cible (R2 >= %.2f) — ",
                "meilleur compromis retenu : power = %s (R2 = %.3f). ",
                "Réseau probablement peu modulaire ou jeu trop petit/hétérogène."),
         r2_min, format(power_table$Power[i]), power_table$SFT.R.sq[i]))
}

#' Analyse du power (soft-thresholding) — enveloppe WGCNA::pickSoftThreshold
#'
#' Arrêt dur si N < TS_BULK_WGCNA_MIN_SAMPLES. Threads désactivés (garde §4).
#' Le pré-filtrage HVG est appliqué AVANT (mission : 2000-5000 gènes).
#'
#' @param vst_matrix Matrice transformée (gènes x échantillons).
#' @param n_top Nombre de gènes pour le pré-filtrage.
#' @param powers Vecteur de powers testés (défaut WGCNA : 1..10 puis pairs).
#' @return list(type = "bulk_wgcna_power", status, power_table, chosen,
#'   n_samples, n_genes_input, n_genes_used, warnings, provenance,
#'   timestamp_utc).
bulk_wgcna_pick_power <- function(vst_matrix, n_top = NULL,
                                  powers = c(1:10, seq(12, 20, 2))) {
  if (!requireNamespace("WGCNA", quietly = TRUE)) {
    stop(errorCondition(paste0(
      "Échec WGCNA : le package 'WGCNA' est requis (install.packages('WGCNA')). ",
      "L'application démarre sans lui ; cette fonctionnalité reste indisponible tant qu'il manque."),
      class = "bulk_wgcna_error", state = "missing_dependency"))
  }
  n <- ncol(vst_matrix)
  min_n <- if (exists("TS_BULK_WGCNA_MIN_SAMPLES", inherits = TRUE)) TS_BULK_WGCNA_MIN_SAMPLES else 15L
  if (n < min_n) {
    stop(errorCondition(sprintf(paste0(
      "Échec WGCNA : %d échantillons seulement — l'analyse exige au moins %d échantillons ",
      "(garde de la mission, N < %d refuse l'exécution). Les réseaux de co-expression ",
      "sur petits effectifs produisent des modules non reproductibles."),
      n, min_n, min_n),
      class = "bulk_wgcna_error", state = "samples_min"))
  }
  hvg <- bulk_wgcna_select_hvg(vst_matrix, n_top = n_top)
  WGCNA::disableWGCNAThreads()
  pt <- .wgcna_with_attached(tryCatch(
    WGCNA::pickSoftThreshold(t(hvg$mat), powerVector = powers,
                             networkType = "signed", verbose = 0),
    error = function(e) stop(errorCondition(paste0(
      "Échec WGCNA : pickSoftThreshold a échoué — ", conditionMessage(e)),
      class = "bulk_wgcna_error", state = "compute_failed"))))
  # Selon la version de WGCNA, la table s'appelle fitIndices (récent) ou
  # fitStatistics (ancien) — les deux gérés, colonnes Power/SFT.R.sq requises.
  power_table <- as.data.frame(pt$fitIndices %||% pt$fitStatistics %||% pt)
  chosen <- bulk_wgcna_choose_power(
    power_table,
    r2_min = if (exists("TS_BULK_WGCNA_R2_MIN", inherits = TRUE)) TS_BULK_WGCNA_R2_MIN else 0.80)
  warnings <- c(hvg$warnings, if (!is.na(chosen$warning)) chosen$warning else character(0))

  provenance <- new_provenance_entry(
    analysis_id = "bulk-wgcna-power",
    method      = "WGCNA::pickSoftThreshold",
    parameters  = list(powers = powers, networkType = "signed",
                       n_genes_used = hvg$n_used, n_samples = n,
                       threads = "disabled"),
    dataset     = vst_matrix,
    warnings    = warnings
  )
  list(
    type          = "bulk_wgcna_power",
    status        = "valid",
    power_table   = power_table,
    chosen        = chosen,
    n_samples     = n,
    n_genes_input = hvg$n_input,
    n_genes_used  = hvg$n_used,
    warnings      = warnings,
    provenance    = provenance,
    timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

#' Construction des modules (blockwiseModules, TOM borné)
#'
#' maxBlockSize = TS_BULK_WGCNA_MAX_BLOCKSIZE (jamais de TOM complet non
#' borné) ; minModuleSize = TS_BULK_WGCNA_MIN_MODULE ; réseau signé ;
#' labels numériques (0 = gris / non assigné). Threads désactivés.
#'
#' @param vst_matrix Matrice transformée (gènes x échantillons).
#' @param power Soft-thresholding power (doit être > 0 ; résultat de
#'   bulk_wgcna_pick_power()$chosen$power recommandé).
#' @param n_top Pré-filtrage HVG (bornes mission).
#' @param deep_split Profondeur de découpage (défaut WGCNA 0, laissé exposé).
#' @return list(type = "bulk_wgcna_modules", status, power, colors (named),
#'   MEs (data.frame), n_modules, module_sizes (table), n_samples,
#'   n_genes_input, n_genes_used, warnings, provenance, timestamp_utc).
bulk_wgcna_build_modules <- function(vst_matrix, power, n_top = NULL, deep_split = 0) {
  if (!requireNamespace("WGCNA", quietly = TRUE)) {
    stop(errorCondition(paste0(
      "Échec WGCNA : le package 'WGCNA' est requis (install.packages('WGCNA'))."),
      class = "bulk_wgcna_error", state = "missing_dependency"))
  }
  power <- as.numeric(power)
  if (length(power) != 1L || is.na(power) || power <= 0) {
    stop(errorCondition("Échec WGCNA : le power doit être un nombre > 0 (lancez d'abord l'analyse du power).",
                        class = "bulk_wgcna_error", state = "invalid_input"))
  }
  n <- ncol(vst_matrix)
  min_n <- if (exists("TS_BULK_WGCNA_MIN_SAMPLES", inherits = TRUE)) TS_BULK_WGCNA_MIN_SAMPLES else 15L
  if (n < min_n) {
    stop(errorCondition(sprintf(paste0(
      "Échec WGCNA : %d échantillons seulement — l'analyse exige au moins %d échantillons ",
      "(garde de la mission, N < %d refuse l'exécution)."),
      n, min_n, min_n),
      class = "bulk_wgcna_error", state = "samples_min"))
  }
  hvg <- bulk_wgcna_select_hvg(vst_matrix, n_top = n_top)
  WGCNA::disableWGCNAThreads()
  max_bs <- if (exists("TS_BULK_WGCNA_MAX_BLOCKSIZE", inherits = TRUE)) TS_BULK_WGCNA_MAX_BLOCKSIZE else 5000L
  min_mod <- if (exists("TS_BULK_WGCNA_MIN_MODULE", inherits = TRUE)) TS_BULK_WGCNA_MIN_MODULE else 30L
  # QUIRK : blockwiseModules exige WGCNA ATTACHÉ (cor interne) — cf. .wgcna_with_attached.
  net <- .wgcna_with_attached(tryCatch(
    WGCNA::blockwiseModules(
      t(hvg$mat), power = power, networkType = "signed", TOMType = "signed",
      maxBlockSize = max_bs, minModuleSize = min_mod, deepSplit = deep_split,
      reassignThreshold = 0, numericLabels = TRUE, pamRespectsDendro = FALSE,
      verbose = 0, indent = 0),
    error = function(e) stop(errorCondition(paste0(
      "Échec WGCNA : blockwiseModules a échoué — ", conditionMessage(e)),
      class = "bulk_wgcna_error", state = "compute_failed"))))

  colors_num <- net$colors
  color_labels <- labels2colors_safe(colors_num)
  # F7 (2026-10-06, mesuré sur GSE164073 en session réelle) : les noms des
  # couleurs sont des GÈNES — `hvg$mat` reste gènes x échantillons, donc
  # `rownames`, pas `colnames` (les échantillons). L'ancien `colnames` ne
  # plantait PAS : le `names<-` de R complete une valeur trop courte par NA
  # au lieu d'erreur, d'où 18 noms d'échantillons + 2 482 NA partis tout
  # droit dans l'export (`build_wgcna_export` lit `names(colors)`).
  names(color_labels) <- rownames(hvg$mat)
  mes <- net$MEs
  if (!is.null(mes)) {
    colnames(mes) <- sub("^ME", "", colnames(mes))
  }
  # Dendrogramme (mono-bloc quand maxBlockSize >= n_genes — le cas nominal
  # sous les bornes mission) + couleurs dans l'ORDRE du dendrogramme.
  dendro <- if (length(net$dendrograms) >= 1L) net$dendrograms[[1]] else NULL
  dendro_colors <- NULL
  if (!is.null(dendro) && length(net$blockGenes) >= 1L) {
    dendro_colors <- color_labels[net$blockGenes[[1]]]
    # Même correction F7 : les indices de `blockGenes` indexent les GÈNES.
    names(dendro_colors) <- rownames(hvg$mat)[net$blockGenes[[1]]]
  }
  n_modules <- length(setdiff(unique(color_labels), "grey"))
  warnings <- c(hvg$warnings, if (n_modules == 0L) sprintf(
    "Aucun module détecté (power = %s) — essayez un power supérieur ou vérifiez la qualité des données.",
    format(power)) else character(0))

  provenance <- new_provenance_entry(
    analysis_id = "bulk-wgcna-modules",
    method      = "WGCNA::blockwiseModules",
    parameters  = list(power = power, networkType = "signed", TOMType = "signed",
                       maxBlockSize = max_bs, minModuleSize = min_mod,
                       deepSplit = deep_split, n_genes_used = hvg$n_used,
                       n_samples = n, threads = "disabled"),
    dataset     = vst_matrix,
    warnings    = warnings
  )
  list(
    type          = "bulk_wgcna_modules",
    status        = "valid",
    power         = power,
    colors        = color_labels,
    dendro        = dendro,
    dendro_colors = dendro_colors,
    MEs           = as.data.frame(mes),
    n_modules     = n_modules,
    module_sizes  = table(color_labels),
    n_samples     = n,
    n_genes_input = hvg$n_input,
    n_genes_used  = hvg$n_used,
    warnings      = warnings,
    provenance    = provenance,
    timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

#' labels2colors avec repli pur (WGCNA absent : palette fixe déterministe)
labels2colors_safe <- function(labels) {
  if (requireNamespace("WGCNA", quietly = TRUE)) {
    return(WGCNA::labels2colors(labels))
  }
  palette <- c("blue", "brown", "turquoise", "green", "yellow", "red", "black",
               "pink", "magenta", "purple", "greenyellow", "tan", "salmon",
               "cyan", "midnightblue", "lightcyan", "lightgreen", "lightyellow",
               "royalblue", "darkred", "darkgreen", "darkorange", "darkgrey",
               "grey60", "lightpink", "steelblue")
  idx <- match(labels, sort(unique(labels)))
  ifelse(labels == 0, "grey", palette[((idx - 1L) %% length(palette)) + 1L])
}

#' Préparer les traits cliniques pour les corrélations ME <-> traits
#'
#' Colonnes numériques conservées telles quelles ; facteurs à 2 niveaux
#' codés 0/1 ; autres (caractère > 2 niveaux, tout-NA) écartés avec
#' avertissement — jamais de codage arbitraire multi-niveaux.
#'
#' @param metadata data.frame (rownames = échantillons).
#' @param candidate_cols Colonnes candidates (NULL = toutes).
#' @return list(traits = data.frame numérique, dropped = character, warnings).
bulk_wgcna_prepare_traits <- function(metadata, candidate_cols = NULL) {
  if (is.null(metadata) || !is.data.frame(metadata) || nrow(metadata) == 0L) {
    stop(errorCondition("bulk_wgcna_prepare_traits() : métadonnées vides.",
                        class = "bulk_wgcna_error", state = "invalid_input"))
  }
  cols <- if (is.null(candidate_cols)) colnames(metadata) else
    intersect(candidate_cols, colnames(metadata))
  keep <- list(); dropped <- character(0); warnings <- character(0)
  for (cl in cols) {
    x <- metadata[[cl]]
    if (is.numeric(x) && sum(is.finite(x)) >= 3L) {
      keep[[cl]] <- as.numeric(x)
    } else if (is.factor(x) || is.character(x) || is.logical(x)) {
      lx <- factor(x)
      if (length(levels(lx)) == 2L) {
        keep[[cl]] <- as.numeric(lx == levels(lx)[2])
      } else {
        dropped <- c(dropped, cl)
      }
    } else {
      dropped <- c(dropped, cl)
    }
  }
  if (length(dropped) > 0L) {
    warnings <- c(warnings, sprintf(
      "Traits ignorés (non numériques et non binaires) : %s.",
      paste(dropped, collapse = ", ")))
  }
  if (length(keep) == 0L) {
    stop(errorCondition(
      "bulk_wgcna_prepare_traits() : aucun trait numérique ou binaire exploitable dans les métadonnées.",
      class = "bulk_wgcna_error", state = "no_traits"))
  }
  out <- as.data.frame(keep, stringsAsFactors = FALSE)
  list(traits = out, dropped = dropped, warnings = warnings)
}

#' p-value asymptotique (Student) d'une corrélation — formule WGCNA standard
.bulk_wgcna_cor_pvalue <- function(cor_mat, n) {
  dof <- n - 2
  t_stat <- cor_mat * sqrt(dof / (1 - cor_mat^2))
  2 * stats::pt(-abs(t_stat), df = dof)
}

#' Corrélations modules <-> traits (bicor robuste, repli cor documenté)
#'
#' @param module_result Résultat bulk_wgcna_build_modules().
#' @param traits data.frame numérique (bulk_wgcna_prepare_traits()$traits).
#' @return list(cor, pval, method = "bicor"|"cor", n_samples, warnings).
bulk_wgcna_module_trait <- function(module_result, traits) {
  mes <- module_result$MEs
  if (is.null(mes) || ncol(mes) == 0L) {
    stop(errorCondition("bulk_wgcna_module_trait() : aucun module eigengene (relancez la construction des modules).",
                        class = "bulk_wgcna_error", state = "invalid_input"))
  }
  # blockwiseModules nomme les lignes des MEs (échantillons de la matrice
  # d'entrée). Appariement strict sur les échantillons communs.
  common <- intersect(rownames(mes), rownames(traits))
  if (length(common) < 3L) {
    stop(errorCondition(sprintf(
      "bulk_wgcna_module_trait() : moins de 3 échantillons communs MEs/traits (%d).",
      length(common)),
      class = "bulk_wgcna_error", state = "invalid_input"))
  }
  traits_use <- traits[common, , drop = FALSE]
  mes_use    <- mes[common, , drop = FALSE]

  method <- "bicor"
  cor_mat <- tryCatch({
    WGCNA::bicor(mes_use, traits_use, use = "pairwise.complete.obs")
  }, error = function(e) {
    method <<- "cor"
    stats::cor(mes_use, traits_use, use = "pairwise.complete.obs")
  })
  list(
    cor       = cor_mat,
    pval      = .bulk_wgcna_cor_pvalue(cor_mat, nrow(mes_use)),
    method    = method,
    n_samples = nrow(mes_use)
  )
}

#' Figures du power : fit scale-free (R2) + connectivité moyenne
#'
#' Purement graphique depuis la table pickSoftThreshold (pas de WGCNA requis).
#'
#' @param power_result Résultat bulk_wgcna_pick_power().
#' @param tr Fonction de traduction.
#' @return Liste de 2 ggplots (fit, connectivity).
plot_wgcna_soft_threshold <- function(power_result, tr = NULL) {
  tr <- tr %||% function(x) x
  pt <- power_result$power_table
  if (is.null(pt) || !all(c("Power", "SFT.R.sq", "mean.k.") %in% colnames(pt))) {
    stop(errorCondition("plot_wgcna_soft_threshold() : table du power invalide.",
                        class = "bulk_wgcna_error", state = "invalid_input"))
  }
  r2_min <- if (exists("TS_BULK_WGCNA_R2_MIN", inherits = TRUE)) TS_BULK_WGCNA_R2_MIN else 0.80
  chosen <- power_result$chosen

  p_fit <- ggplot2::ggplot(pt, ggplot2::aes(Power, SFT.R.sq)) +
    ggplot2::geom_hline(yintercept = r2_min, linetype = "dashed", color = "#E74C3C") +
    ggplot2::geom_line(color = "grey40") +
    ggplot2::geom_point(color = "#2C3E50", size = 2.2) +
    ggplot2::geom_point(data = data.frame(Power = chosen$power, SFT.R.sq = chosen$r2),
                        color = "#18BC9C", size = 4, shape = 17) +
    ggplot2::labs(
      title = tr("Analyse du power (soft-thresholding)"),
      subtitle = tr(sprintf("Réseau signé — power retenu : %s (R2 = %.3f, cible %.2f)",
                            format(chosen$power), chosen$r2, r2_min)),
      x = tr("Power"), y = tr("Fit scale-free (R2)")) +
    ts_theme("minimal", 12)

  p_conn <- ggplot2::ggplot(pt, ggplot2::aes(Power, mean.k.)) +
    ggplot2::geom_line(color = "grey40") +
    ggplot2::geom_point(color = "#2C3E50", size = 2.2) +
    ggplot2::labs(title = tr("Connectivité moyenne"),
                  x = tr("Power"), y = tr("Connectivité moyenne (mean.k.)")) +
    ts_theme("minimal", 12)
  list(fit = p_fit, connectivity = p_conn)
}

#' Heatmap des corrélations modules (MEs) <-> traits, avec étoiles
#'
#' @param trait_cor Résultat bulk_wgcna_module_trait().
#' @param tr Fonction de traduction.
#' @return ggplot (tuiles + étoiles de significativité).
plot_wgcna_trait_heatmap <- function(trait_cor, tr = NULL) {
  tr <- tr %||% function(x) x
  cm <- trait_cor$cor
  pm <- trait_cor$pval
  stars <- ifelse(pm < 0.001, "***", ifelse(pm < 0.01, "**",
          ifelse(pm < 0.05, "*", "")))
  df <- data.frame(
    module = rep(rownames(cm), times = ncol(cm)),
    trait  = rep(colnames(cm), each = nrow(cm)),
    cor    = as.vector(cm),
    stars  = as.vector(stars),
    stringsAsFactors = FALSE
  )
  df$module <- factor(df$module, levels = rownames(cm))
  df$trait  <- factor(df$trait, levels = colnames(cm))
  ggplot2::ggplot(df, ggplot2::aes(trait, module, fill = cor)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = stars), size = 4, vjust = 0.75) +
    ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                                  midpoint = 0, limits = c(-1, 1),
                                  name = tr(sprintf("Corr. (%s)", trait_cor$method))) +
    ggplot2::labs(title = tr("Modules <-> traits cliniques"),
                  subtitle = tr(sprintf("Méthode : %s — %d échantillons",
                                        trait_cor$method, trait_cor$n_samples))) +
    ts_theme("minimal", 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

#' Export plat WGCNA : gènes -> modules + eigengenes + corrélations traits
#'
#' @param modules_result Résultat bulk_wgcna_build_modules().
#' @param trait_cor Résultat bulk_wgcna_module_trait() (optionnel).
#' @return data.frame(gene, module) — la table ME/traits reste disponible
#'   via le RDS complet ; l'export plat porte l'association par gène.
build_wgcna_export <- function(modules_result, trait_cor = NULL) {
  if (!is.list(modules_result) || !identical(modules_result$type, "bulk_wgcna_modules")) {
    stop(errorCondition("build_wgcna_export() : résultat de modules invalide.",
                        class = "bulk_wgcna_error", state = "invalid_input"))
  }
  df <- data.frame(gene = names(modules_result$colors),
                   module = unname(modules_result$colors),
                   stringsAsFactors = FALSE)
  df <- df[order(df$module, df$gene), ]
  if (!is.null(trait_cor)) {
    df$trait_cor_method <- trait_cor$method
  }
  df
}

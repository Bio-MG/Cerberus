# =============================================================================
# R/sc/sc_communication_spatial.R — CCC contexte spatial (roadmap avancée
# Phase 1 / V1.x-A)
# =============================================================================
# Consommateur PUR du résultat canonique communication (Stage 11) : il joint
# un contexte SPATIAL aux paires sender/receiver importées — distances,
# rayon, densité locale, enrichissement par permutation optionnel.
#
# REGLES (roadmap avancée, docs/ROADMAP_CCC_ADVANCED.md) :
#   - distance = CONTRAINTE spatiale, jamais une preuve de communication ;
#     « compatibilité spatiale » ≠ « communication biologique directe » ;
#   - la table canonique du résultat parent n'est JAMAIS modifiée ;
#   - aucun score n'est recalculé : les scores importés joints aux paires
#     restent descriptifs, étiquetés « importés, échelle de la source » ;
#   - la source de coordonnées est DÉCLARÉE (objet spatial courant = unités
#     natives, ou réduction 2D = distances de projection NON physiques) ;
#   - labels sans correspondance / paires non cartographiables : exclus ET
#     comptabilisés — jamais imputés, jamais renommés ;
#   - aucune densification silencieuse : distances croisées par chunks
#     mémoire bornés ;
#   - enrichissement par permutation : OPT-IN (n_permutations = 0 par
#     défaut), seed fixée, null = permutation des étiquettes d'identité.
#
# Sourced in app.R AFTER R/sc/sc_communication_views.R. Erreurs classées
# `communication_context_error` (état structuré via l'attribut `state`).
#
# ── API PUBLIQUE FIGÉE (V1.x-A) ─────────────────────────────────────────────
# Le test de freeze (test-communication-spatial-contract-freeze.R) vérifie
# cette surface + le schéma du résultat.
# =============================================================================

#' Surface publique figée de R/sc/sc_communication_spatial.R (V1.x-A)
#'
#' @return Vecteur character des noms de fonctions publiques.
#' @export
communication_spatial_public_api <- function() {
  c(
    "communication_spatial_public_api",
    "communication_spatial_coordinate_sources",
    "communication_spatial_contract_fields",
    "build_communication_spatial_context",
    "plot_communication_spatial_edges",
    "plot_communication_spatial_distance_summary",
    "build_communication_spatial_export"
  )
}

#' Sources de coordonnées déclarables (V1.x-A)
#' @export
communication_spatial_coordinate_sources <- function() {
  c("spatial_object", "reduction")
}

#' Champs contractuels du contexte spatial (V1.x-A)
#' @export
communication_spatial_contract_fields <- function() {
  c(
    "type", "status", "analysis_id", "parent_analysis_id", "source_method",
    "identity_column", "coordinate_source", "coordinate_metric",
    "coordinate_units", "params", "pair_table", "qc", "warnings",
    "provenance", "timestamp_utc"
  )
}

.spc_stop <- function(message, state = "invalid_input") {
  stop(errorCondition(
    message, state = state, class = "communication_context_error"
  ))
}

# Coalescence noeud = identité harmonisée sinon label brut (implémentation
# volontairement vectorisée différemment de .communication_node_keys du
# fichier de vues, dont les helpers internes sont réservés à ce fichier).
.spc_node_keys <- function(table) {
  sender_mapped <- table$sender_mapped %||% rep(NA_character_, nrow(table))
  receiver_mapped <- table$receiver_mapped %||% rep(NA_character_, nrow(table))
  table$sender_node <- ifelse(!is.na(sender_mapped), sender_mapped, table$sender)
  table$receiver_node <- ifelse(!is.na(receiver_mapped), receiver_mapped, table$receiver)
  table
}

# Normalisation des coordonnées : matrix n_cells x 2 OU data.frame avec
# colonnes (id,) x, y — renvoie list(coords = matrix, cell_ids = character).
.spc_normalize_coords <- function(coordinates) {
  if (is.data.frame(coordinates)) {
    if (!all(c("x", "y") %in% names(coordinates))) {
      .spc_stop(paste0(
        "build_communication_spatial_context() : le data.frame de coordonnées ",
        "doit exposer les colonnes 'x' et 'y' (et optionnellement 'id')."
      ))
    }
    ids <- if (!is.null(coordinates$id)) as.character(coordinates$id) else rownames(coordinates)
    xy <- as.matrix(coordinates[, c("x", "y"), drop = FALSE])
  } else {
    xy <- tryCatch(as.matrix(coordinates), error = function(e) NULL)
    if (is.null(xy) || ncol(xy) < 2L) {
      .spc_stop(paste0(
        "build_communication_spatial_context() : 'coordinates' doit être une ",
        "matrix n_cells x 2 (x, y) ou un data.frame (id, x, y)."
      ))
    }
    xy <- xy[, 1:2, drop = FALSE]
    ids <- rownames(xy)
  }
  if (is.null(ids) || length(ids) != nrow(xy) || anyNA(ids) || !all(nzchar(ids))) {
    .spc_stop(paste0(
      "build_communication_spatial_context() : identifiants de cellules des ",
      "coordonnées absents ou invalides (noms de lignes attendus)."
    ))
  }
  if (!is.numeric(xy) || anyNA(xy)) {
    .spc_stop(paste0(
      "build_communication_spatial_context() : coordonnées non numériques ou ",
      "NA détectées — aucune imputation n'est effectuée."
    ))
  }
  if (anyDuplicated(ids)) {
    .spc_stop(paste0(
      "build_communication_spatial_context() : identifiants de cellules ",
      "dupliqués dans les coordonnées."
    ))
  }
  rownames(xy) <- ids
  list(coords = xy, cell_ids = ids)
}

# Distance minimale de chaque ligne de A au point le plus proche de B —
# chunks mémoire bornés (jamais de matrice nA x nB pleine pour les grands
# effectifs ; bloc visé <= ~4e6 entrées).
.spc_cross_min_dist <- function(A, B) {
  nA <- nrow(A)
  out <- numeric(nA)
  if (nA == 0L || nrow(B) == 0L) return(out)
  chunk <- max(1L, min(1024L, as.integer(floor(4e6 / max(nrow(B), 1L)))))
  b2 <- rowSums(B * B)
  i <- 1L
  while (i <= nA) {
    j <- min(i + chunk - 1L, nA)
    Ai <- A[i:j, , drop = FALSE]
    d2 <- rowSums(Ai * Ai) + rep(b2, each = nrow(Ai)) - 2 * tcrossprod(Ai, B)
    out[i:j] <- sqrt(pmax(d2, 0) |> apply(1L, min))
    i <- j + 1L
  }
  out
}

# Statistiques d'une paire (vecteurs d'indices de cellules) — déterministe.
.spc_pair_stats <- function(si, ri, coords, radius) {
  S <- coords[si, , drop = FALSE]
  R <- coords[ri, , drop = FALSE]
  centroid_s <- colMeans(S)
  centroid_r <- colMeans(R)
  r2s <- .spc_cross_min_dist(R, S)  # chaque receiver -> sender le plus proche
  s2r <- .spc_cross_min_dist(S, R)
  frac_r <- if (!is.na(radius)) mean(r2s <= radius) else NA_real_
  frac_s <- if (!is.na(radius)) mean(s2r <= radius) else NA_real_
  list(
    n_sender_cells = length(si), n_receiver_cells = length(ri),
    centroid_distance = sqrt(sum((centroid_s - centroid_r)^2)),
    mean_recv_to_sender_nn = mean(r2s), median_recv_to_sender_nn = stats::median(r2s),
    mean_send_to_recv_nn = mean(s2r), median_send_to_recv_nn = stats::median(s2r),
    frac_receiver_within_radius = frac_r, frac_sender_within_radius = frac_s
  )
}

#' Construire le contexte spatial d'un résultat communication importé (V1.x-A)
#'
#' Joint, pour chaque paire de noeuds (sender, receiver) du résultat, des
#' quantités spatiales descriptives : distance des centroïdes, distances
#' croisées plus-proche-voisin (médiane/moyenne, chaque direction), fraction
#' de cellules à portée d'un rayon explicite, et (OPT-IN) enrichissement par
#' permutation des étiquettes d'identité (z / p permutation, seed fixée).
#'
#' Limites énoncées : la distance est une contrainte spatiale — une
#' compatibilité spatiale n'établit AUCUNE communication biologique ; les
#' distances d'une réduction 2D ne sont PAS des distances physiques.
#'
#' @param communication_result Résultat canonique (assert_communication_result).
#' @param coordinates Matrix n_cells x 2 (rownames = cellules) OU data.frame
#'   (id, x, y) — p.ex. sortie de get_spatial_coords() ou Embeddings()[,1:2].
#' @param cell_identities Vecteur character NOMMÉ (cellule -> identité) extrait
#'   de la colonne d'identités de l'objet courant.
#' @param identity_column Nom de la colonne d'identités (traçabilité) ; un
#'   désaccord avec le résultat parent produit un avertissement enregistré.
#' @param coordinate_source "spatial_object" ou "reduction" (déclaration).
#' @param coordinate_units Unités déclarées (NA accepté ; en réduction,
#'   l'espace est adimensionnel).
#' @param radius Rayon explicite (mêmes unités que les coordonnées) ou NA —
#'   les fractions à portée restent NA sans rayon (jamais de rayon inféré).
#' @param n_permutations Nombre de permutations (0 = pas d'enrichissement).
#' @param perm_seed Seed de permutation (défaut 1 ; déterministe).
#' @param filtered_table Table (canonique ou pré-filtrée par les vues) —
#'   défaut NULL = table complète du résultat.
#' @return Objet canonique de contexte (communication_spatial_contract_fields()).
#' @export
build_communication_spatial_context <- function(communication_result,
                                                coordinates,
                                                cell_identities,
                                                identity_column = NA_character_,
                                                coordinate_source = c("spatial_object", "reduction"),
                                                coordinate_units = NA_character_,
                                                radius = NA_real_,
                                                n_permutations = 0L,
                                                perm_seed = 1L,
                                                filtered_table = NULL) {
  r <- assert_communication_result(communication_result, context = "contexte spatial")
  coordinate_source <- match.arg(coordinate_source)

  if (!is.na(radius) && (!is.numeric(radius) || length(radius) != 1L || radius <= 0)) {
    .spc_stop("build_communication_spatial_context() : 'radius' doit être un nombre > 0 ou NA (aucun rayon inféré).")
  }
  n_perm <- as.integer(n_permutations)
  if (length(n_perm) != 1L || is.na(n_perm) || n_perm < 0L || n_perm > 1000L) {
    .spc_stop("build_communication_spatial_context() : 'n_permutations' doit être un entier entre 0 et 1000.")
  }
  if (n_perm > 0L && (!is.numeric(perm_seed) || length(perm_seed) != 1L || is.na(perm_seed))) {
    .spc_stop("build_communication_spatial_context() : 'perm_seed' doit être un nombre (seed de permutation).")
  }

  coord <- .spc_normalize_coords(coordinates)
  if (!is.character(cell_identities) || is.null(names(cell_identities)) ||
      anyNA(names(cell_identities)) || !all(nzchar(names(cell_identities))) ||
      anyNA(cell_identities) || !all(nzchar(cell_identities))) {
    .spc_stop(paste0(
      "build_communication_spatial_context() : 'cell_identities' doit être un ",
      "vecteur character nommé (cellule -> identité), sans valeur vide."
    ))
  }
  if (anyDuplicated(names(cell_identities))) {
    .spc_stop("build_communication_spatial_context() : noms de cellules dupliqués dans 'cell_identities'.")
  }

  warnings_all <- character(0)
  if (!is.na(identity_column) && !is.na(r$identity_column) &&
      !identical(identity_column, r$identity_column)) {
    warnings_all <- c(warnings_all, sprintf(
      "Colonne d'identités déclarée (%s) différente de celle de l'import (%s) — la coalescence des noeuds reste celle de l'import.",
      identity_column, r$identity_column
    ))
  }
  if (identical(coordinate_source, "reduction")) {
    warnings_all <- c(warnings_all, paste0(
      "Coordonnées issues d'une réduction 2D : les distances sont des ",
      "distances de PROJECTION, non des distances physiques."
    ))
  }

  cells_matched <- intersect(coord$cell_ids, names(cell_identities))
  qc <- list(
    n_cells_coordinates = length(coord$cell_ids),
    n_cells_identities = length(cell_identities),
    n_cells_matched = length(cells_matched),
    n_pairs_total = 0L, n_pairs_mapped = 0L, n_pairs_unmapped = 0L,
    unmapped_pairs = character(0), n_permutations_done = n_perm
  )
  if (length(cells_matched) < 2L) {
    .spc_stop(paste0(
      "build_communication_spatial_context() : moins de 2 cellules communes ",
      "entre coordonnées et identités (", length(cells_matched),
      ") — le contexte spatial n'est pas cartographiable."
    ), state = "invalid_identity_mapping")
  }
  ids_matched <- setNames(cell_identities[cells_matched], cells_matched)
  coords <- coord$coords[cells_matched, , drop = FALSE]

  table <- .spc_node_keys(
    if (is.null(filtered_table)) r$canonical_table else as.data.frame(filtered_table)
  )
  pairs <- unique(paste(table$sender_node, table$receiver_node, sep = "\r"))
  qc$n_pairs_total <- length(pairs)

  per_pair <- lapply(pairs, function(k) {
    sp <- strsplit(k, "\r", fixed = TRUE)[[1L]]
    sub <- table[table$sender_node == sp[1L] & table$receiver_node == sp[2L], ]
    sc <- sub$score[!is.na(sub$score)]
    si <- which(ids_matched == sp[1L])
    ri <- which(ids_matched == sp[2L])
    if (length(si) == 0L || length(ri) == 0L) {
      return(list(mapped = FALSE, pair = sp,
                  n_interactions = nrow(sub),
                  mean_score = if (length(sc)) mean(sc) else NA_real_,
                  n_with_score = length(sc)))
    }
    st <- .spc_pair_stats(si, ri, coords, radius)
    list(mapped = TRUE, pair = sp, n_interactions = nrow(sub),
         mean_score = if (length(sc)) mean(sc) else NA_real_,
         n_with_score = length(sc), stats = st, si = si, ri = ri)
  })

  mapped <- Filter(function(x) isTRUE(x$mapped), per_pair)
  unmapped <- vapply(Filter(function(x) !isTRUE(x$mapped), per_pair),
                     function(x) paste(x$pair, collapse = " -> "), character(1))
  qc$n_pairs_mapped <- length(mapped)
  qc$n_pairs_unmapped <- length(unmapped)
  qc$unmapped_pairs <- unmapped
  if (length(unmapped)) {
    warnings_all <- c(warnings_all, sprintf(
      "%d paire(s) non cartographiable(s) dans l'espace déclaré (population absente des identités) : %s",
      length(unmapped), paste(utils::head(unmapped, 8), collapse = " ; ")
    ))
  }
  if (length(mapped) == 0L) {
    .spc_stop(paste0(
      "build_communication_spatial_context() : aucune paire sender/receiver du ",
      "résultat ne correspond aux identités fournies — vérifiez la colonne ",
      "d'identités et les coordonnées.", state = "invalid_identity_mapping"
    ), state = "invalid_identity_mapping")
  }

  # Enrichissement par permutation (OPT-IN) : null = permutation des étiquettes
  # d'identité parmi les cellules cartographiées, statistic = fraction à portée.
  z_score <- rep(NA_real_, length(mapped))
  p_perm <- rep(NA_real_, length(mapped))
  if (n_perm > 0L && !is.na(radius)) {
    obs <- vapply(mapped, function(x) x$stats$frac_receiver_within_radius, numeric(1))
    null <- matrix(NA_real_, nrow = n_perm, ncol = length(mapped))
    set.seed(as.integer(perm_seed))
    lab <- unname(ids_matched)
    nm <- names(ids_matched)
    for (b in seq_len(n_perm)) {
      # Null d'échangeabilité des étiquettes : les ensembles sender/receiver
      # de chaque paire sont relus depuis la permutation GLOBALE des labels.
      perm <- setNames(sample(lab), nm)
      for (p in seq_along(mapped)) {
        x <- mapped[[p]]
        si <- which(perm == x$pair[1L])
        ri <- which(perm == x$pair[2L])
        if (length(si) == 0L || length(ri) == 0L) next
        st <- .spc_pair_stats(si, ri, coords, radius)
        null[b, p] <- st$frac_receiver_within_radius
      }
    }
    for (p in seq_along(mapped)) {
      v <- null[, p]
      v <- v[!is.na(v)]
      if (length(v) < 2L) next
      sd_v <- stats::sd(v)
      z_score[p] <- if (is.finite(sd_v) && sd_v > 0) (obs[p] - mean(v)) / sd_v else NA_real_
      p_perm[p] <- (1L + sum(v >= obs[p])) / (length(v) + 1L)
    }
    warnings_all <- c(warnings_all, paste0(
      "Enrichissement par permutation des étiquettes (B = ", n_perm,
      ", seed = ", perm_seed, ") : contrainte spatiale sous hypothèse nulle ",
      "d'échangeabilité — PAS une preuve de communication."
    ))
  }

  pair_table <- do.call(rbind, lapply(seq_along(mapped), function(p) {
    x <- mapped[[p]]
    st <- x$stats
    data.frame(
      sender_node = x$pair[1L], receiver_node = x$pair[2L],
      n_interactions = x$n_interactions,
      mean_imported_score = x$mean_score, n_with_score = x$n_with_score,
      n_sender_cells = st$n_sender_cells, n_receiver_cells = st$n_receiver_cells,
      centroid_distance = st$centroid_distance,
      mean_recv_to_sender_nn = st$mean_recv_to_sender_nn,
      median_recv_to_sender_nn = st$median_recv_to_sender_nn,
      mean_send_to_recv_nn = st$mean_send_to_recv_nn,
      median_send_to_recv_nn = st$median_send_to_recv_nn,
      frac_receiver_within_radius = st$frac_receiver_within_radius,
      frac_sender_within_radius = st$frac_sender_within_radius,
      z_score = z_score[p], p_perm = p_perm[p],
      stringsAsFactors = FALSE
    )
  }))

  entry <- new_provenance_entry(
    analysis_id = "sc-communication-spatial",
    method = "spatial_context_euclidean",
    parameters = list(
      coordinate_source = coordinate_source,
      coordinate_metric = "euclidean",
      coordinate_units = coordinate_units,
      identity_column = identity_column,
      radius = radius, n_permutations = n_perm, perm_seed = perm_seed,
      n_pairs_mapped = qc$n_pairs_mapped, n_pairs_unmapped = qc$n_pairs_unmapped,
      n_cells_matched = qc$n_cells_matched,
      parent_analysis_id = r$analysis_id
    ),
    seed = if (n_perm > 0L) as.integer(perm_seed) else NULL,
    warnings = warnings_all
  )
  entry$analysis_type <- "cell_cell_communication_spatial"
  entry$import_only <- FALSE
  entry$parent_analysis_id <- r$analysis_id
  entry$status <- "valid"

  list(
    type = "ccc_spatial_context",
    status = "valid",
    analysis_id = "sc-communication-spatial",
    parent_analysis_id = r$analysis_id,
    source_method = r$source_method,
    identity_column = identity_column,
    coordinate_source = coordinate_source,
    coordinate_metric = "euclidean",
    coordinate_units = coordinate_units,
    params = list(
      radius = radius, n_permutations = n_perm, perm_seed = perm_seed,
      metric = "euclidean"
    ),
    pair_table = pair_table,
    qc = qc,
    warnings = warnings_all,
    provenance = entry,
    timestamp_utc = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  )
}

# Sous-titre commun des figures de contexte spatial (garde-fou de lecture).
.spc_caption <- function(context) {
  src <- if (identical(context$coordinate_source, "reduction"))
    "réduction 2D — distances de projection, NON physiques"
  else "coordonnées spatiales déclarées — unités natives"
  paste0(
    "Distance = contrainte spatiale, pas une preuve de communication. ",
    "Source : ", src, ". Scores importés (échelle de la source : ",
    context$source_method %||% NA, ")."
  )
}

.spc_context_plot_stub <- function(message, title, subtitle) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = message,
                      size = 4.5, colour = "grey40") +
    ggplot2::theme_void() +
    ggplot2::labs(title = title, subtitle = paste(message, subtitle, sep = " — "))
}

#' Vue arêtes d'une paire dans l'espace déclaré (V1.x-A)
#'
#' Cellules colorées par population de la paire, centroïdes, flèche
#' sender -> receiver et distance de centroïdes en sous-titre. Consommatrice
#' pure du contexte : AUCUN recalcul.
#'
#' @param context Contexte spatial (build_communication_spatial_context()).
#' @param sender_node,receiver_node Paire à dessiner.
#' @param coordinates Coordinates au même format que le calcul (re-normalisées,
#'   cellules surnuméraires ignorées).
#' @param cell_identities Vecteur nommé cellule -> identité.
#' @return ggplot.
#' @export
plot_communication_spatial_edges <- function(context, sender_node, receiver_node,
                                             coordinates, cell_identities) {
  if (!is.list(context) || !identical(context$type %||% NULL, "ccc_spatial_context")) {
    .spc_stop("plot_communication_spatial_edges() : contexte spatial requis (build_communication_spatial_context()).")
  }
  coord <- .spc_normalize_coords(coordinates)
  pt <- context$pair_table
  sel <- pt[pt$sender_node == sender_node & pt$receiver_node == receiver_node, , drop = FALSE]
  caption <- .spc_caption(context)
  if (nrow(sel) != 1L) {
    return(.spc_context_plot_stub(
      sprintf("Paire '%s -> %s' absente du contexte spatial (paire non cartographiable ou inexistante).",
              sender_node, receiver_node),
      "Contexte spatial — paire sender-receiver", caption
    ))
  }
  si <- names(cell_identities)[cell_identities == sender_node]
  ri <- names(cell_identities)[cell_identities == receiver_node]
  all <- coord$coords
  df_all <- data.frame(x = all[, 1L], y = all[, 2L])
  df_pair <- do.call(rbind, c(
    if (length(si)) list(data.frame(x = all[si, 1L], y = all[si, 2L], pop = "Sender")),
    if (length(ri)) list(data.frame(x = all[ri, 1L], y = all[ri, 2L], pop = "Receiver"))
  ))
  cs <- colMeans(all[si, , drop = FALSE]); cr <- colMeans(all[ri, , drop = FALSE])
  df_cent <- data.frame(
    x = c(cs[[1L]], cr[[1L]]), y = c(cs[[2L]], cr[[2L]]),
    pop = c("Sender", "Receiver")
  )
  ggplot2::ggplot() +
    ggplot2::geom_point(data = df_all, ggplot2::aes(x, y),
                        colour = "grey85", size = 0.4, alpha = 0.5) +
    ggplot2::geom_point(data = df_pair, ggplot2::aes(x, y, colour = pop),
                        size = 0.8, alpha = 0.65) +
    ggplot2::geom_point(data = df_cent, ggplot2::aes(x, y, colour = pop), size = 4) +
    ggplot2::geom_segment(
      data = data.frame(x = cs[[1L]], y = cs[[2L]]),
      ggplot2::aes(x = x, y = y, xend = cr[[1L]], yend = cr[[2L]]),
      arrow = grid::arrow(length = grid::unit(10, "pt"), type = "closed"),
      linewidth = 1, colour = "#2166AC"
    ) +
    ggplot2::scale_colour_manual(values = c(Sender = "#2166AC", Receiver = "#D6604D"), name = NULL) +
    ggplot2::coord_fixed() +
    ggplot2::theme_classic() +
    ggplot2::labs(
      title = sprintf("Contexte spatial : %s -> %s", sender_node, receiver_node),
      subtitle = sprintf(
        paste0("%s — centroïdes : %.3g ; NN médian receiver→sender : %.3g ; ",
               "%d cellules sender / %d receiver."),
        caption, sel$centroid_distance, sel$median_recv_to_sender_nn,
        sel$n_sender_cells, sel$n_receiver_cells
      )
    )
}

#' Résumé des distances croisées par paire (V1.x-A)
#'
#' Un point par paire : distance NN médiane receiver→sender (x) contre
#' sender→receiver (y), diagonale d'égalité en repère. Consommatrice pure.
#'
#' @param context Contexte spatial.
#' @return ggplot.
#' @export
plot_communication_spatial_distance_summary <- function(context) {
  if (!is.list(context) || !identical(context$type %||% NULL, "ccc_spatial_context")) {
    .spc_stop("plot_communication_spatial_distance_summary() : contexte spatial requis.")
  }
  caption <- .spc_caption(context)
  pt <- context$pair_table
  if (is.null(pt) || nrow(pt) == 0L) {
    return(.spc_context_plot_stub(
      "Aucune paire cartographiable dans le contexte spatial.",
      "Distances croisées par paire", caption
    ))
  }
  pt$pair_label <- paste(pt$sender_node, pt$receiver_node, sep = " -> ")
  lim <- range(c(pt$median_recv_to_sender_nn, pt$median_send_to_recv_nn), na.rm = TRUE)
  ggplot2::ggplot(pt, ggplot2::aes(x = median_recv_to_sender_nn,
                                   y = median_send_to_recv_nn)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey75", linetype = "dashed") +
    ggplot2::geom_point(colour = "#2166AC", size = 2.6, alpha = 0.9) +
    ggplot2::geom_text(ggplot2::aes(label = pair_label), size = 2.9,
                       vjust = -0.8, check_overlap = TRUE) +
    ggplot2::coord_fixed(xlim = lim, ylim = lim) +
    ggplot2::theme_classic() +
    ggplot2::labs(
      x = "NN médian receiver -> sender", y = "NN médian sender -> receiver",
      title = "Distances croisées plus-proche-voisin par paire",
      subtitle = caption
    )
}

#' Export CSV du contexte spatial (traçabilité embarquée) (V1.x-A)
#'
#' @param context Contexte spatial.
#' @return data.frame : pair_table + colonnes analysis_id / parent /
#'   timestamp / source de coordonnées / paramètres.
#' @export
build_communication_spatial_export <- function(context) {
  if (!is.list(context) || !identical(context$type %||% NULL, "ccc_spatial_context")) {
    .spc_stop("build_communication_spatial_export() : contexte spatial requis.")
  }
  n <- nrow(context$pair_table %||% data.frame())
  data.frame(
    context$pair_table,
    analysis_id = rep(context$analysis_id, n),
    parent_analysis_id = rep(context$parent_analysis_id, n),
    timestamp_utc = rep(context$timestamp_utc, n),
    coordinate_source = rep(context$coordinate_source, n),
    coordinate_metric = rep(context$coordinate_metric, n),
    coordinate_units = rep(context$coordinate_units, n),
    radius = rep(context$params$radius, n),
    n_permutations = rep(context$params$n_permutations, n),
    perm_seed = rep(context$params$perm_seed, n),
    stringsAsFactors = FALSE
  )
}

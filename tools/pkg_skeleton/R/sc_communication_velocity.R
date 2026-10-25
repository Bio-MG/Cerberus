# =============================================================================
# R/sc/sc_communication_velocity.R — CCC contexte vélocité (roadmap avancée
# Phase 2 / V1.x-C)
# =============================================================================
# Consommateur PUR du résultat canonique communication (Stage 11) ET du
# résultat velocity canonique (contrat VELOCITY_RESULT_CONTRACT, Stage 10) :
# il joint à chaque paire sender/receiver le contexte de vitesse des
# populations — magnitude moyenne/médiane des vecteurs précalculés importés.
#
# REGLES (roadmap avancée, docs/ROADMAP_CCC_ADVANCED.md) :
#   - « associé à l'état de vitesse » — AUCUNE causalité : la magnitude des
#     vecteurs est une quantité descriptive par population, pas une preuve
#     que le ligand « pilote » la transition ;
#   - les vecteurs précalculés ne sont JAMAIS ré-inférés ni substitués
#     (contrat velocity) : sans vecteur validé → état
#     `unavailable_no_vectors` (objet rendu, table vide — jamais d'erreur
#     silencieuse ni de fabrication) ;
#   - recouvrement cellules identités × cellules velocity : enregistré ;
#     sous le seuil déclaré (TS_VELOCITY_OVERLAP_MIN consommé par le module)
#     → état `insufficient_overlap` explicite ;
#   - la table canonique du résultat parent n'est jamais modifiée.
#
# Sourced in app.R AFTER R/sc/sc_communication_trajectory.R. Erreurs classées
# `communication_context_error` (état structuré via l'attribut `state`).
#
# ── API PUBLIQUE FIGÉE (V1.x-C) ─────────────────────────────────────────────
# Le test de freeze (test-communication-velocity-contract-freeze.R) vérifie
# cette surface + le schéma du résultat.
# =============================================================================

#' Surface publique figée de R/sc/sc_communication_velocity.R (V1.x-C)
#'
#' @return Vecteur character des noms de fonctions publiques.
#' @export
communication_velocity_public_api <- function() {
  c(
    "communication_velocity_public_api",
    "communication_velocity_contract_fields",
    "build_communication_velocity_context",
    "plot_communication_velocity_context",
    "build_communication_velocity_export"
  )
}

#' Champs contractuels du contexte vélocité (V1.x-C)
#' @export
communication_velocity_contract_fields <- function() {
  c(
    "type", "status", "analysis_id", "parent_analysis_id", "source_method",
    "identity_column", "velocity_status", "velocity_analysis_id", "params",
    "pair_table", "qc", "warnings", "provenance", "timestamp_utc"
  )
}

.vel_ctx_stop <- function(message, state = "invalid_input") {
  stop(errorCondition(
    message, state = state, class = "communication_context_error"
  ))
}

# Coalescence noeud = identité harmonisée sinon label brut (implémentation
# propre au fichier).
.vctx_node_keys <- function(table) {
  sender_mapped <- table$sender_mapped %||% rep(NA_character_, nrow(table))
  receiver_mapped <- table$receiver_mapped %||% rep(NA_character_, nrow(table))
  table$sender_node <- ifelse(!is.na(sender_mapped), sender_mapped, table$sender)
  table$receiver_node <- ifelse(!is.na(receiver_mapped), receiver_mapped, table$receiver)
  table
}

# Sous-titre commun des figures de contexte vélocité (garde-fou).
.vctx_caption <- function(context) {
  paste0(
    "Magnitude des vecteurs de vitesse PRÉCALCULÉS importés (espace ",
    "d'embedding) — quantité descriptive par population, AUCUNE causalité : ",
    "« associé à l'état de vitesse » ne signifie pas « piloté par le ",
    "ligand ». Scores importés (échelle de la source : ",
    context$source_method %||% NA, ")."
  )
}

.vctx_stub_plot <- function(message, title, subtitle) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = message,
                      size = 4.5, colour = "grey40") +
    ggplot2::theme_void() +
    ggplot2::labs(title = title, subtitle = paste(message, subtitle, sep = " — "))
}

#' Construire le contexte vélocité d'un résultat communication (V1.x-C)
#'
#' Pour chaque paire de noeuds (sender, receiver) cartographiable dans les
#' identités : magnitude (norme L2 dx/dy) des vecteurs de vitesse précalculés
#' importés, moyenne/médiane par population et effectifs. Le recouvrement
#' cellules × velocity est enregistré ; sans vecteur validé, l'objet rendu
#' porte l'état `unavailable_no_vectors` (aucun vecteur substitué, aucune
#' ré-inférence).
#'
#' @param communication_result Résultat canonique (assert_communication_result).
#' @param velocity_result Résultat velocity canonique (assert_velocity_result,
#'   view="any") — contract VELOCITY_RESULT_CONTRACT.
#' @param cell_identities Vecteur character NOMMÉ (cellule → identité).
#' @param identity_column Nom déclaré de la colonne d'identités (traçabilité).
#' @param min_overlap Seuil de recouvrement (fraction) consommé de
#'   config/thresholds.R (TS_VELOCITY_OVERLAP_MIN) par le module ; NA = pas de
#'   seuil (recouvrement seulement enregistré).
#' @param filtered_table Table canonique ou pré-filtrée par les vues (défaut
#'   NULL = table complète).
#' @return Objet canonique de contexte (communication_velocity_contract_fields()).
#' @export
build_communication_velocity_context <- function(communication_result,
                                                 velocity_result,
                                                 cell_identities,
                                                 identity_column = NA_character_,
                                                 min_overlap = NA_real_,
                                                 filtered_table = NULL) {
  r <- assert_communication_result(communication_result, context = "contexte vélocité")
  vr <- assert_velocity_result(velocity_result, view = "any",
                               context = "contexte vélocité communication")

  if (!is.character(cell_identities) || is.null(names(cell_identities)) ||
      anyNA(names(cell_identities)) || !all(nzchar(names(cell_identities))) ||
      anyNA(cell_identities) || !all(nzchar(cell_identities))) {
    .vel_ctx_stop(paste0(
      "build_communication_velocity_context() : 'cell_identities' doit être un ",
      "vecteur character nommé (cellule -> identité), sans valeur vide."
    ))
  }
  if (anyDuplicated(names(cell_identities))) {
    .vel_ctx_stop("build_communication_velocity_context() : noms de cellules dupliqués dans 'cell_identities'.")
  }
  if (!is.na(min_overlap) && (!is.numeric(min_overlap) || length(min_overlap) != 1L ||
                              min_overlap < 0 || min_overlap > 1)) {
    .vel_ctx_stop("build_communication_velocity_context() : 'min_overlap' doit être une fraction entre 0 et 1, ou NA.")
  }

  warnings_all <- character(0)
  if (!is.na(identity_column) && !is.na(r$identity_column) &&
      !identical(identity_column, r$identity_column)) {
    warnings_all <- c(warnings_all, sprintf(
      "Colonne d'identités déclarée (%s) différente de celle de l'import (%s) — la coalescence des noeuds reste celle de l'import.",
      identity_column, r$identity_column
    ))
  }

  vectors <- vr$velocity_vectors
  qc <- list(
    n_cells_identities = length(cell_identities),
    n_cells_velocity = length(vr$cell_names %||% character(0)),
    n_cells_matched = 0L, overlap_fraction = NA_real_,
    n_pairs_total = 0L, n_pairs_mapped = 0L, n_pairs_unmapped = 0L,
    unmapped_pairs = character(0)
  )

  .make_empty <- function(status, extra_warning) {
    if (length(extra_warning)) warnings_all <<- c(warnings_all, extra_warning)
    entry <- new_provenance_entry(
      analysis_id = "sc-communication-velocity",
      method = "velocity_context_magnitude",
      parameters = list(
        identity_column = identity_column,
        min_overlap = min_overlap,
        velocity_status = vr$status,
        n_cells_matched = qc$n_cells_matched,
        overlap_fraction = qc$overlap_fraction,
        n_pairs = 0L,
        parent_analysis_id = r$analysis_id
      ),
      warnings = warnings_all
    )
    entry$analysis_type <- "cell_cell_communication_velocity"
    entry$import_only <- FALSE
    entry$parent_analysis_id <- r$analysis_id
    entry$status <- status
    list(
      type = "ccc_velocity_context",
      status = status,
      analysis_id = "sc-communication-velocity",
      parent_analysis_id = r$analysis_id,
      source_method = r$source_method,
      identity_column = identity_column,
      velocity_status = vr$status,
      velocity_analysis_id = vr$analysis_id %||% NA_character_,
      params = list(min_overlap = min_overlap, metric = "l2_magnitude_embedding"),
      pair_table = data.frame(sender_node = character(0), receiver_node = character(0)),
      qc = qc,
      warnings = warnings_all,
      provenance = entry,
      timestamp_utc = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
    )
  }

  if (is.null(vectors)) {
    return(.make_empty("unavailable_no_vectors", paste0(
      "Résultat velocity sans vecteurs validés (statut velocity : ", vr$status,
      ") — contexte indisponible : aucun vecteur substitué, aucune ré-inférence."
    )))
  }

  mag <- setNames(sqrt(rowSums(as.matrix(vectors)^2)), rownames(vectors))
  cells_matched <- intersect(names(cell_identities), names(mag))
  qc$n_cells_matched <- length(cells_matched)
  qc$overlap_fraction <- if (length(cell_identities) > 0L)
    length(cells_matched) / length(cell_identities) else NA_real_

  if (!is.na(min_overlap) && qc$overlap_fraction < min_overlap) {
    return(.make_empty("insufficient_overlap", sprintf(
      "Recouvrement identités × velocity (%.1f%%) sous le seuil déclaré (%.1f%%) — contexte non calculé (seuil consommé de config/thresholds.R).",
      100 * qc$overlap_fraction, 100 * min_overlap
    )))
  }
  if (length(cells_matched) < 2L) {
    return(.make_empty("insufficient_overlap", paste0(
      "Moins de 2 cellules communes entre identités et vecteurs velocity — contexte non calculable."
    )))
  }

  table <- .vctx_node_keys(
    if (is.null(filtered_table)) r$canonical_table else as.data.frame(filtered_table)
  )
  pairs <- unique(paste(table$sender_node, table$receiver_node, sep = "\r"))
  qc$n_pairs_total <- length(pairs)

  ids_matched <- cell_identities[cells_matched]
  mag_matched <- mag[cells_matched]

  pair_rows <- list()
  unmapped <- character(0)
  for (k in pairs) {
    sp <- strsplit(k, "\r", fixed = TRUE)[[1L]]
    sub <- table[table$sender_node == sp[1L] & table$receiver_node == sp[2L], ]
    si <- cells_matched[ids_matched == sp[1L]]
    ri <- cells_matched[ids_matched == sp[2L]]
    if (length(si) == 0L || length(ri) == 0L) {
      unmapped <- c(unmapped, paste(sp, collapse = " -> "))
      next
    }
    s_mag <- mag_matched[si]
    r_mag <- mag_matched[ri]
    s_sc <- sub$score[!is.na(sub$score)]
    pair_rows[[length(pair_rows) + 1L]] <- data.frame(
      sender_node = sp[1L], receiver_node = sp[2L],
      n_interactions = nrow(sub),
      mean_imported_score = if (length(s_sc)) mean(s_sc) else NA_real_,
      n_sender_cells = length(si), n_receiver_cells = length(ri),
      sender_velocity_mean = mean(s_mag), sender_velocity_median = stats::median(s_mag),
      receiver_velocity_mean = mean(r_mag), receiver_velocity_median = stats::median(r_mag),
      stringsAsFactors = FALSE
    )
  }
  qc$n_pairs_unmapped <- length(unmapped)
  qc$unmapped_pairs <- unmapped
  qc$n_pairs_mapped <- length(pair_rows)
  if (length(unmapped)) {
    warnings_all <- c(warnings_all, sprintf(
      "%d paire(s) non cartographiable(s) dans le résultat velocity (population absente des cellules alignées) : %s",
      length(unmapped), paste(utils::head(unmapped, 8), collapse = " ; ")
    ))
  }
  if (length(pair_rows) == 0L) {
    return(.make_empty("insufficient_overlap",
                       "Aucune paire sender/receiver cartographiable dans les cellules velocity."))
  }

  pair_table <- do.call(rbind, pair_rows)

  entry <- new_provenance_entry(
    analysis_id = "sc-communication-velocity",
    method = "velocity_context_magnitude",
    parameters = list(
      identity_column = identity_column,
      min_overlap = min_overlap,
      velocity_status = vr$status,
      n_cells_matched = qc$n_cells_matched,
      overlap_fraction = qc$overlap_fraction,
      n_pairs_mapped = qc$n_pairs_mapped,
      n_pairs_unmapped = qc$n_pairs_unmapped,
      metric = "l2_magnitude_embedding",
      parent_analysis_id = r$analysis_id
    ),
    warnings = warnings_all
  )
  entry$analysis_type <- "cell_cell_communication_velocity"
  entry$import_only <- FALSE
  entry$parent_analysis_id <- r$analysis_id
  entry$status <- "valid"

  list(
    type = "ccc_velocity_context",
    status = "valid",
    analysis_id = "sc-communication-velocity",
    parent_analysis_id = r$analysis_id,
    source_method = r$source_method,
    identity_column = identity_column,
    velocity_status = vr$status,
    velocity_analysis_id = vr$analysis_id %||% NA_character_,
    params = list(min_overlap = min_overlap, metric = "l2_magnitude_embedding"),
    pair_table = pair_table,
    qc = qc,
    warnings = warnings_all,
    provenance = entry,
    timestamp_utc = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  )
}

#' Vue magnitudes sender/receiver par paire (V1.x-C)
#'
#' Consommatrice pure : un point par population et paire (moyenne de la
#' magnitude des vecteurs importés), segments reliant sender et receiver.
#' État non valide → message explicite, jamais un graphe trompeur.
#'
#' @param context Contexte vélocité (build_communication_velocity_context()).
#' @return ggplot.
#' @export
plot_communication_velocity_context <- function(context) {
  if (!is.list(context) || !identical(context$type %||% NULL, "ccc_velocity_context")) {
    .vel_ctx_stop("plot_communication_velocity_context() : contexte vélocité requis (build_communication_velocity_context()).")
  }
  caption <- .vctx_caption(context)
  if (!identical(context$status, "valid") ||
      is.null(context$pair_table) || nrow(context$pair_table) == 0L) {
    return(.vctx_stub_plot(
      sprintf("Contexte vélocité indisponible (statut : %s).", context$status %||% NA),
      "Contexte vélocité — magnitudes par population", caption
    ))
  }
  pt <- context$pair_table
  pt$pair_label <- paste(pt$sender_node, pt$receiver_node, sep = " -> ")
  long <- rbind(
    data.frame(pair_label = pt$pair_label, role = "Sender",
               magnitude = pt$sender_velocity_mean),
    data.frame(pair_label = pt$pair_label, role = "Receiver",
               magnitude = pt$receiver_velocity_mean)
  )
  ggplot2::ggplot(long, ggplot2::aes(x = pair_label, y = magnitude, colour = role)) +
    ggplot2::geom_segment(
      data = pt,
      ggplot2::aes(x = pair_label, y = sender_velocity_mean,
                   xend = pair_label, yend = receiver_velocity_mean),
      inherit.aes = FALSE, colour = "grey75", linewidth = 0.8
    ) +
    ggplot2::geom_point(size = 3, alpha = 0.9) +
    ggplot2::scale_colour_manual(values = c(Sender = "#2166AC", Receiver = "#D6604D"), name = NULL) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      x = NULL, y = "Magnitude moyenne des vecteurs (importés)",
      title = "Contexte vélocité des populations par paire",
      subtitle = caption
    )
}

#' Export CSV du contexte vélocité (traçabilité embarquée) (V1.x-C)
#'
#' @param context Contexte vélocité.
#' @return data.frame : pair_table + colonnes analysis_id / parent /
#'   timestamp / statut velocity / seuil de recouvrement.
#' @export
build_communication_velocity_export <- function(context) {
  if (!is.list(context) || !identical(context$type %||% NULL, "ccc_velocity_context")) {
    .vel_ctx_stop("build_communication_velocity_export() : contexte vélocité requis.")
  }
  n <- nrow(context$pair_table %||% data.frame())
  data.frame(
    context$pair_table,
    analysis_id = rep(context$analysis_id, n),
    parent_analysis_id = rep(context$parent_analysis_id, n),
    timestamp_utc = rep(context$timestamp_utc, n),
    velocity_status = rep(context$velocity_status, n),
    overlap_fraction = rep(context$qc$overlap_fraction, n),
    min_overlap = rep(context$params$min_overlap, n),
    stringsAsFactors = FALSE
  )
}

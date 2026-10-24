# =============================================================================
# R/sc/sc_communication_perturbation.R — CCC perturbation IN SILICO
# (roadmap avancée Phase 4 / V1.x-D)
# =============================================================================
# Consommateur PUR du résultat canonique communication (Stage 11) : il
# simule la suppression ou l'atténuation d'une cible du réseau INFÉRÉ
# (ligand, récepteur, interaction ligand->récepteur, population sender ou
# receiver) et quantifie l'effet de PREMIER ORDRE sur les paires et les
# nœuds — à partir des scores importés, jamais recalculés.
#
# GARDE ABSOLU (roadmap §7) :
#   - ceci est une PERTURBATION IN SILICO du réseau inféré — ce n'est PAS
#     une expérience KO, PAS un knockout validé, PAS un effet biologique
#     ni un effet causal. L'étiquette « IN SILICO PERTURBATION » est portée
#     par le résultat, chaque figure et chaque export ;
#   - effets de PREMIER ORDRE uniquement : aucune propagation réseau
#     modélisée (ce serait une inférence non justifiée — anti-feature-
#     creep §21) ; les quantités « affected » listent qui perd du score,
#     elles ne prédisent rien au-delà ;
#   - distinction NETWORK PERTURBATION (suppression/atténuation
#     d'interactions inférées) vs DOWNSTREAM TRANSCRIPTIONAL SIMULATION
#     (jamais implémentée ici) — jamais conflation ;
#   - la table canonique du résultat parent n'est jamais modifiée.
#
# Sourced in app.R AFTER R/sc/sc_communication_velocity.R. Erreurs classées
# `communication_context_error` (état structuré via l'attribut `state`).
#
# ── API PUBLIQUE FIGÉE (V1.x-D) ─────────────────────────────────────────────
# Le test de freeze (test-communication-perturbation-contract-freeze.R)
# vérifie cette surface + le schéma du résultat.
# =============================================================================

#' Surface publique figée de R/sc/sc_communication_perturbation.R (V1.x-D)
#'
#' @return Vecteur character des noms de fonctions publiques.
#' @export
communication_perturbation_public_api <- function() {
  c(
    "communication_perturbation_public_api",
    "communication_perturbation_targets",
    "communication_perturbation_contract_fields",
    "build_communication_perturbation",
    "plot_communication_perturbation_delta",
    "plot_communication_perturbation_nodes",
    "build_communication_perturbation_export"
  )
}

#' Cibles de perturbation déclarables (V1.x-D)
#' @export
communication_perturbation_targets <- function() {
  c("ligand", "receptor", "interaction", "sender", "receiver")
}

#' Champs contractuels du résultat de perturbation (V1.x-D)
#' @export
communication_perturbation_contract_fields <- function() {
  c(
    "type", "status", "analysis_id", "parent_analysis_id", "source_method",
    "target", "value", "mode", "params", "baseline_summary",
    "perturbed_summary", "delta_table", "node_table", "qc", "warnings",
    "provenance", "timestamp_utc"
  )
}

.pert_stop <- function(message, state = "invalid_input") {
  stop(errorCondition(
    message, state = state, class = "communication_context_error"
  ))
}

# Coalescence noeud = identité harmonisée sinon label brut (implémentation
# propre au fichier).
.pert_node_keys <- function(table) {
  sender_mapped <- table$sender_mapped %||% rep(NA_character_, nrow(table))
  receiver_mapped <- table$receiver_mapped %||% rep(NA_character_, nrow(table))
  table$sender_node <- ifelse(!is.na(sender_mapped), sender_mapped, table$sender)
  table$receiver_node <- ifelse(!is.na(receiver_mapped), receiver_mapped, table$receiver)
  table
}

# Garde-fou d'étiquette : présent dans le résultat, les figures et l'export.
.pert_banner <- function() {
  "IN SILICO PERTURBATION — réseau inféré modifié : ce n'est PAS une expérience KO, PAS un effet biologique ni causal validé. Effets de PREMIER ORDRE (aucune propagation réseau modélisée)."
}

# Résumé d'un réseau (table avec sender_node/receiver_node/score).
.pert_network_summary <- function(table) {
  sc <- table$score[!is.na(table$score)]
  list(
    n_interactions = nrow(table),
    n_pairs = length(unique(paste(table$sender_node, table$receiver_node, sep = "\r"))),
    n_nodes = length(unique(c(table$sender_node, table$receiver_node))),
    score_total = if (length(sc)) sum(sc) else NA_real_,
    score_mean = if (length(sc)) mean(sc) else NA_real_,
    n_with_score = length(sc)
  )
}

# Totaux par paire (scores importés uniquement — NA conservés s'il n'y a
# aucun score, jamais de zéro fabriqué).
.pert_pair_totals <- function(table) {
  pairs <- unique(paste(table$sender_node, table$receiver_node, sep = "\r"))
  do.call(rbind, lapply(pairs, function(k) {
    sp <- strsplit(k, "\r", fixed = TRUE)[[1L]]
    sub <- table[table$sender_node == sp[1L] & table$receiver_node == sp[2L], ]
    sc <- sub$score[!is.na(sub$score)]
    data.frame(
      sender_node = sp[1L], receiver_node = sp[2L],
      n_interactions = nrow(sub),
      score_total = if (length(sc)) sum(sc) else NA_real_,
      n_with_score = length(sc),
      stringsAsFactors = FALSE
    )
  }))
}

# Totaux par nœud (sortant + entrant), mêmes règles de NA.
.pert_node_totals <- function(table) {
  nodes <- sort(unique(c(table$sender_node, table$receiver_node)))
  do.call(rbind, lapply(nodes, function(nd) {
    out_rows <- table[table$sender_node == nd, ]
    in_rows <- table[table$receiver_node == nd, ]
    out_sc <- out_rows$score[!is.na(out_rows$score)]
    in_sc <- in_rows$score[!is.na(in_rows$score)]
    data.frame(
      node = nd,
      out_interactions = nrow(out_rows), in_interactions = nrow(in_rows),
      out_score_total = if (length(out_sc)) sum(out_sc) else NA_real_,
      in_score_total = if (length(in_sc)) sum(in_sc) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
}

#' Simuler une perturbation IN SILICO du réseau communication importé (V1.x-D)
#'
#' Supprime ou atténue une cible du réseau INFÉRÉ et quantifie l'effet de
#' premier ordre : deltas de score par paire (sender -> receiver) et par
#' nœud, interactions supprimées/atténuées comptées. Le réseau de référence
#' est la table canonique (ou sa version filtrée par les vues) ; il n'est
#' JAMAIS modifié — les deux réseaux (baseline / perturbé) sont des copies.
#'
#' Cibles (`communication_perturbation_targets()`) : ligand, récepteur,
#' interaction (chaîne exacte « ligand -> récepteur » de la table canonique),
#' population sender, population receiver (nœuds coalescents).
#'
#' @param communication_result Résultat canonique (assert_communication_result).
#' @param target Une des communication_perturbation_targets().
#' @param value Valeur exacte de la cible dans la table (jamais de matching
#'   flou : valeur absente = erreur, aucune supposition).
#' @param mode "remove" (suppression des lignes ciblées) ou "attenuate"
#'   (score := score * factor, lignes conservées et flagees).
#' @param factor Facteur d'atténuation (0 < factor < 1 ; requis en mode
#'   attenuate, ignoré en mode remove).
#' @param filtered_table Table canonique ou pré-filtrée par les vues (défaut
#'   NULL = table complète).
#' @return Objet canonique (communication_perturbation_contract_fields()).
#' @export
build_communication_perturbation <- function(communication_result,
                                             target,
                                             value,
                                             mode = "remove",
                                             factor = 0.5,
                                             filtered_table = NULL) {
  r <- assert_communication_result(communication_result, context = "perturbation in silico")
  if (!is.character(target) || length(target) != 1L || !target %in% communication_perturbation_targets()) {
    .pert_stop(sprintf(
      "build_communication_perturbation() : 'target' doit être une des valeurs [%s].",
      paste(communication_perturbation_targets(), collapse = ", ")
    ))
  }
  if (!is.character(mode) || length(mode) != 1L || !mode %in% c("remove", "attenuate")) {
    .pert_stop("build_communication_perturbation() : 'mode' doit être \"remove\" ou \"attenuate\".")
  }

  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
    .pert_stop("build_communication_perturbation() : 'value' doit être une chaîne non vide (valeur exacte de la cible).")
  }
  if (identical(mode, "attenuate")) {
    if (!is.numeric(factor) || length(factor) != 1L || is.na(factor) ||
        factor <= 0 || factor >= 1) {
      .pert_stop("build_communication_perturbation() : 'factor' doit être un nombre strictement entre 0 et 1 (mode attenuate).")
    }
  }

  warnings_all <- c(
    .pert_banner(),
    "Scores importés (échelle de la source) : les deltas sont des différences de scores IMPORTÉS, pas des effets mesurés."
  )

  base_table <- .pert_node_keys(
    if (is.null(filtered_table)) r$canonical_table else as.data.frame(filtered_table)
  )
  # Colonne cible (nœuds coalescents pour les populations).
  target_col <- switch(target,
    ligand = "ligand", receptor = "receptor", interaction = "interaction",
    sender = "sender_node", receiver = "receiver_node"
  )
  if (!target_col %in% names(base_table)) {
    .pert_stop(sprintf(
      "build_communication_perturbation() : colonne cible '%s' absente de la table canonique.",
      target_col
    ))
  }
  pool <- stats::na.omit(unique(as.character(base_table[[target_col]])))
  if (!value %in% pool) {
    .pert_stop(sprintf(
      paste0("build_communication_perturbation() : valeur '%s' absente de la ",
             "colonne '%s' (aucun matching flou). Valeurs disponibles (%d) : %s"),
      value, target, length(pool),
      paste(utils::head(sort(pool), 12), collapse = ", ")
    ))
  }

  hit <- !is.na(base_table[[target_col]]) & base_table[[target_col]] == value
  pert_table <- base_table
  n_removed <- 0L
  n_attenuated <- 0L
  if (identical(mode, "remove")) {
    pert_table <- base_table[!hit, , drop = FALSE]
    n_removed <- sum(hit)
  } else {
    pert_table$score[hit] <- pert_table$score[hit] * factor
    pert_table$perturbed <- FALSE
    pert_table$perturbed[hit] <- TRUE
    n_attenuated <- sum(hit)
  }

  baseline_summary <- .pert_network_summary(base_table)
  perturbed_summary <- .pert_network_summary(pert_table)

  # Deltas par paire (join sur les clés communes ; paire disparue = delta
  # sur son total baseline, comptée — jamais silencieuse).
  base_pairs <- .pert_pair_totals(base_table)
  pert_pairs <- .pert_pair_totals(pert_table)
  key_base <- paste(base_pairs$sender_node, base_pairs$receiver_node, sep = "\r")
  key_pert <- paste(pert_pairs$sender_node, pert_pairs$receiver_node, sep = "\r")
  m <- match(key_base, key_pert)
  delta_table <- data.frame(
    sender_node = base_pairs$sender_node,
    receiver_node = base_pairs$receiver_node,
    n_interactions_baseline = base_pairs$n_interactions,
    n_interactions_perturbed = ifelse(is.na(m), 0L, pert_pairs$n_interactions[m]),
    score_total_baseline = base_pairs$score_total,
    score_total_perturbed = ifelse(is.na(m), NA_real_, pert_pairs$score_total[m]),
    stringsAsFactors = FALSE
  )
  # delta de score : NA si la paire n'a pas de scores importés (pas 0).
  has_scores <- !is.na(delta_table$score_total_baseline) |
    (!is.na(delta_table$score_total_perturbed) & !is.na(m))
  delta_table$delta_score_total <- ifelse(
    has_scores,
    ifelse(is.na(delta_table$score_total_perturbed), 0,
           delta_table$score_total_perturbed) -
      ifelse(is.na(delta_table$score_total_baseline), 0,
             delta_table$score_total_baseline),
    NA_real_
  )
  delta_table$delta_fraction <- ifelse(
    !is.na(delta_table$delta_score_total) &
      !is.na(delta_table$score_total_baseline) & delta_table$score_total_baseline > 0,
    delta_table$delta_score_total / delta_table$score_total_baseline,
    NA_real_
  )
  delta_table$affected <- (!is.na(delta_table$delta_score_total) &
                             delta_table$delta_score_total != 0) |
    delta_table$n_interactions_perturbed != delta_table$n_interactions_baseline
  delta_table <- delta_table[order(-abs(delta_table$delta_score_total %||% rep(0, nrow(delta_table)))), , drop = FALSE]
  rownames(delta_table) <- NULL

  # Deltas par nœud (sortant + entrant).
  base_nodes <- .pert_node_totals(base_table)
  pert_nodes <- .pert_node_totals(pert_table)
  key_nb <- base_nodes$node
  m_n <- match(key_nb, pert_nodes$node)
  node_table <- data.frame(
    node = base_nodes$node,
    out_interactions_baseline = base_nodes$out_interactions,
    in_interactions_baseline = base_nodes$in_interactions,
    out_score_total_baseline = base_nodes$out_score_total,
    in_score_total_baseline = base_nodes$in_score_total,
    out_interactions_perturbed = ifelse(is.na(m_n), 0L, pert_nodes$out_interactions[m_n]),
    in_interactions_perturbed = ifelse(is.na(m_n), 0L, pert_nodes$in_interactions[m_n]),
    out_score_total_perturbed = ifelse(is.na(m_n), NA_real_, pert_nodes$out_score_total[m_n]),
    in_score_total_perturbed = ifelse(is.na(m_n), NA_real_, pert_nodes$in_score_total[m_n]),
    stringsAsFactors = FALSE
  )
  node_table$delta_out_score <- ifelse(
    !is.na(node_table$out_score_total_baseline) | !is.na(node_table$out_score_total_perturbed),
    ifelse(is.na(node_table$out_score_total_perturbed), 0, node_table$out_score_total_perturbed) -
      ifelse(is.na(node_table$out_score_total_baseline), 0, node_table$out_score_total_baseline),
    NA_real_
  )
  node_table$delta_in_score <- ifelse(
    !is.na(node_table$in_score_total_baseline) | !is.na(node_table$in_score_total_perturbed),
    ifelse(is.na(node_table$in_score_total_perturbed), 0, node_table$in_score_total_perturbed) -
      ifelse(is.na(node_table$in_score_total_baseline), 0, node_table$in_score_total_baseline),
    NA_real_
  )

  qc <- list(
    n_rows_baseline = nrow(base_table),
    n_rows_perturbed = nrow(pert_table),
    n_removed = as.integer(n_removed),
    n_attenuated = as.integer(n_attenuated),
    n_pairs_affected = sum(delta_table$affected %||% rep(FALSE, nrow(delta_table))),
    affected_senders = sort(unique(delta_table$sender_node[delta_table$affected])),
    affected_receivers = sort(unique(delta_table$receiver_node[delta_table$affected]))
  )
  if (identical(mode, "remove") && nrow(pert_table) == 0L) {
    warnings_all <- c(warnings_all,
                      "Toutes les interactions ont été supprimées par cette perturbation (réseau perturbé vide) — comptabilisé, jamais silencieux.")
  }

  entry <- new_provenance_entry(
    analysis_id = "sc-communication-perturbation",
    method = "in_silico_network_perturbation_first_order",
    parameters = list(
      target = target, value = value, mode = mode,
      factor = if (identical(mode, "attenuate")) factor else NA_real_,
      n_removed = as.integer(n_removed), n_attenuated = as.integer(n_attenuated),
      parent_analysis_id = r$analysis_id,
      label = "in_silico_not_ko"
    ),
    warnings = warnings_all
  )
  entry$analysis_type <- "cell_cell_communication_perturbation"
  entry$import_only <- FALSE
  entry$parent_analysis_id <- r$analysis_id
  entry$status <- "valid"

  list(
    type = "ccc_perturbation",
    status = "valid",
    analysis_id = "sc-communication-perturbation",
    parent_analysis_id = r$analysis_id,
    source_method = r$source_method,
    target = target,
    value = value,
    mode = mode,
    params = list(
      factor = if (identical(mode, "attenuate")) factor else NA_real_,
      order = "first_order_no_propagation",
      label = "IN SILICO PERTURBATION"
    ),
    baseline_summary = baseline_summary,
    perturbed_summary = perturbed_summary,
    delta_table = delta_table,
    node_table = node_table,
    qc = qc,
    warnings = warnings_all,
    provenance = entry,
    timestamp_utc = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  )
}

#' Vue deltas par paire après perturbation (V1.x-D)
#'
#' Barres divergentes du delta de score total par paire (top N en valeur
#' absolue). Sous-titre = garde « IN SILICO PERTURBATION ». Consommatrice
#' pure : aucun recalcul.
#'
#' @param context Résultat de perturbation (build_communication_perturbation()).
#' @param top_n Nombre de paires affichées (défaut 20, par |delta| décroissant).
#' @return ggplot.
#' @export
plot_communication_perturbation_delta <- function(context, top_n = 20L) {
  if (!is.list(context) || !identical(context$type %||% NULL, "ccc_perturbation")) {
    .pert_stop("plot_communication_perturbation_delta() : résultat de perturbation requis (build_communication_perturbation()).")
  }
  pt <- context$delta_table
  pt$pair_label <- paste(pt$sender_node, pt$receiver_node, sep = " -> ")
  pt <- pt[!is.na(pt$delta_score_total), , drop = FALSE]
  if (nrow(pt) == 0L) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5,
                               label = "Aucun score importé : deltas de score non calculables (effectifs seuls — voir la table).",
                               size = 4.5, colour = "grey40") +
             ggplot2::theme_void())
  }
  pt <- pt[order(-abs(pt$delta_score_total)), , drop = FALSE]
  top_n <- max(1L, as.integer(top_n %||% 20L))
  if (nrow(pt) > top_n) pt <- pt[seq_len(top_n), , drop = FALSE]
  pt$pair_label <- factor(pt$pair_label, levels = rev(pt$pair_label))
  ggplot2::ggplot(pt, ggplot2::aes(x = delta_score_total, y = pair_label,
                                   fill = delta_score_total > 0)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = "#D6604D", `FALSE` = "#2166AC"),
                               guide = "none") +
    ggplot2::theme_classic() +
    ggplot2::labs(
      x = "Δ score total importé (baseline → perturbé)", y = NULL,
      title = sprintf("Effet de la perturbation [%s = %s, %s]",
                      context$target, context$value, context$mode),
      subtitle = .pert_banner()
    )
}

#' Vue totaux par nœud baseline vs perturbé (V1.x-D)
#'
#' Paires de points (baseline / perturbé) du total de score sortant + entrant
#' par nœud — lecture descriptive de QUI change. Consommatrice pure.
#'
#' @param context Résultat de perturbation.
#' @return ggplot.
#' @export
plot_communication_perturbation_nodes <- function(context) {
  if (!is.list(context) || !identical(context$type %||% NULL, "ccc_perturbation")) {
    .pert_stop("plot_communication_perturbation_nodes() : résultat de perturbation requis.")
  }
  nt <- context$node_table
  # total = somme en traitant NA comme absence (pas de score -> 0 uniquement
  # pour la LECTURE graphique ; les tables gardent les NA).
  .sum0 <- function(a, b) {
    ifelse(is.na(a) & is.na(b), NA_real_, ifelse(is.na(a), 0, a) + ifelse(is.na(b), 0, b))
  }
  nt$total_baseline <- .sum0(nt$out_score_total_baseline, nt$in_score_total_baseline)
  nt$total_perturbed <- .sum0(nt$out_score_total_perturbed, nt$in_score_total_perturbed)
  long <- rbind(
    data.frame(node = nt$node, total = nt$total_baseline, state = "Baseline"),
    data.frame(node = nt$node, total = nt$total_perturbed, state = "Perturbé")
  )
  long <- long[!is.na(long$total), , drop = FALSE]
  if (nrow(long) == 0L) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5,
                               label = "Aucun score importé : vue nœuds indisponible.",
                               size = 4.5, colour = "grey40") +
             ggplot2::theme_void())
  }
  long$node <- factor(long$node, levels = unique(long$node[order(-long$total)]))
  ggplot2::ggplot(long, ggplot2::aes(x = total, y = node, colour = state)) +
    ggplot2::geom_line(ggplot2::aes(group = node), colour = "grey75") +
    ggplot2::geom_point(size = 3, alpha = 0.9) +
    ggplot2::scale_colour_manual(values = c(Baseline = "grey55", "Perturbé" = "#D6604D"),
                                 name = NULL) +
    ggplot2::theme_classic() +
    ggplot2::labs(
      x = "Total de score importé (sortant + entrant)", y = NULL,
      title = sprintf("Nœuds : baseline vs perturbé [%s = %s]",
                      context$target, context$value),
      subtitle = .pert_banner()
    )
}

#' Export CSV de la perturbation (traçabilité + étiquette in silico) (V1.x-D)
#'
#' @param context Résultat de perturbation.
#' @return data.frame : delta_table + analysis_id / parent / timestamp /
#'   cible / mode / facteur / étiquette « in_silico ».
#' @export
build_communication_perturbation_export <- function(context) {
  if (!is.list(context) || !identical(context$type %||% NULL, "ccc_perturbation")) {
    .pert_stop("build_communication_perturbation_export() : résultat de perturbation requis.")
  }
  n <- nrow(context$delta_table %||% data.frame())
  data.frame(
    context$delta_table,
    analysis_id = rep(context$analysis_id, n),
    parent_analysis_id = rep(context$parent_analysis_id, n),
    timestamp_utc = rep(context$timestamp_utc, n),
    target = rep(context$target, n),
    value = rep(context$value, n),
    mode = rep(context$mode, n),
    factor = rep(context$params$factor, n),
    label = rep("IN SILICO PERTURBATION", n),
    stringsAsFactors = FALSE
  )
}

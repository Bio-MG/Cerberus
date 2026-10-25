# =============================================================================
# R/sc/sc_communication_views.R — Communication 4D-2 : vues exploratoires des
# imports valides (Stage 12)
# =============================================================================
# Consommateurs PURES du resultat canonique (assert_communication_result) :
# filtres d'affichage, DotPlot sender-receiver, heatmap de pathways, reseau
# circulaire, centralite descriptive, provenance des filtres et exports.
#
# REGLES (Stage 12) :
#   - une vue ne cree, n'infere et ne repare JAMAIS un resultat ; elle lit la
#     table (fragmente par les filtres) et n'ecrit rien dans le resultat ;
#   - le type de score ET la methode source sont affiches sur CHAQUE figure
#     (sous-titre) : les scores de sources differentes ne sont PAS comparables ;
#   - les p-values sont etiquetees "importees" — aucune reinference ;
#   - une association ligand-receptor n'etablit aucune causalite (legende) ;
#   - la centralite est "derivede du reseau, descriptive" — PAS un controle
#     biologique ;
#   - les filtres sont des operations d'AFFICHAGE : la table canonique du
#     resultat n'est jamais modifiee ; les filtres actifs sont produit dans la
#     provenance (build_communication_filter_provenance) au moment de l'export.
#
# Nuds du reseau : identite harmonisee (sender_mapped/receiver_mapped) quand
# elle existe, sinon label brut — coalescence explicite (.communication_node_keys).
#
# Sourced in app.R AFTER R/sc/sc_communication.R. Pure ggplot2 (aucune
# dependance nouvelle : pas d'igraph/ggraph/circlize — courbes de Bezier
# quadratiques echantillonnees a la main).
#
# ── API PUBLIQUE FIGEE (Stage 12) ───────────────────────────────────────────
# communication_views_public_api() enumere la surface ; le test de freeze
# (test-communication-contract-freeze.R) la verifie. Helper interne :
#   .views_stop()            — erreur classee communication_import_error
#   .communication_node_keys() — coalescence mapped/raw
#   .communication_agg_pairs() — aggregation par paire de noeuds
#   .communication_empty_view_plot() — message explicite au lieu d'un graphe vide
# =============================================================================

#' Surface publique figee de R/sc/sc_communication_views.R (Stage 12)
#'
#' @return Vecteur character des noms de fonctions publiques.
#' @export
communication_views_public_api <- function() {
  c(
    "communication_views_public_api",
    "communication_apply_filters",
    "plot_communication_dotplot",
    "plot_communication_pathway_heatmap",
    "plot_communication_circle",
    "build_communication_centrality",
    "build_communication_filter_provenance",
    "build_communication_filtered_export"
  )
}

#' Erreur d'une vue communication (classe du domaine, etat structure)
.views_stop <- function(message) {
  stop(errorCondition(
    message,
    state = "invalid_input",
    class = "communication_import_error"
  ))
}

#' Coalescence des noeuds : identite harmonisee sinon label brut
#'
#' Ajoute sender_node/receiver_node : la valeur harmonisee (exact match vers
#' la colonne choisie a l'import) quand elle existe, sinon le label brut —
#' la fusion de labels est donc TOUJOURS tracee par la colonne d'identites
#' citee dans les sous-titres.
.communication_node_keys <- function(table) {
  table$sender_node <- ifelse(
    !is.na(table$sender_mapped %||% rep(NA_character_, nrow(table))),
    table$sender_mapped, table$sender
  )
  table$receiver_node <- ifelse(
    !is.na(table$receiver_mapped %||% rep(NA_character_, nrow(table))),
    table$receiver_mapped, table$receiver
  )
  table
}

#' Aggregation descriptive par paire de noeuds (aucune reinference)
#'
#' @return data.frame(sender_node, receiver_node, n_interactions, mean_score,
#'   sum_score, n_with_score).
.communication_agg_pairs <- function(table) {
  keys <- unique(paste(table$sender_node, table$receiver_node, sep = "\r"))
  do.call(rbind, lapply(keys, function(k) {
    sp <- strsplit(k, "\r", fixed = TRUE)[[1L]]
    sub <- table[table$sender_node == sp[1L] & table$receiver_node == sp[2L], ]
    sc <- sub$score[!is.na(sub$score)]
    out <- data.frame(
      sender_node = sp[1L], receiver_node = sp[2L],
      n_interactions = nrow(sub),
      mean_score = if (length(sc)) mean(sc) else NA_real_,
      sum_score = if (length(sc)) sum(sc) else NA_real_,
      n_with_score = length(sc),
      stringsAsFactors = FALSE
    )
    if ("rank" %in% colnames(sub)) {
      rk <- suppressWarnings(as.numeric(sub$rank))
      rk <- rk[is.finite(rk)]
      out$mean_rank <- if (length(rk)) mean(rk) else NA_real_
      out$best_rank <- if (length(rk)) min(rk) else NA_real_
    }
    out
  }))
}

#' Message explicite au lieu d'un graphique vide (aucun graphe trompeur)
#'
#' Le message est porte a la fois dans le sous-titre (visible et testable)
#' et en annotation centrale de la figure.
.communication_empty_view_plot <- function(message, title, subtitle) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = message,
                      size = 4.5, colour = "grey40") +
    ggplot2::theme_void() +
    ggplot2::labs(title = title, subtitle = paste(message, subtitle, sep = " — "))
}

# Sous-titre commun : type de score + methode source (garde-fou d'echelle).
.communication_score_caption <- function(communication_result) {
  if ("rank" %in% colnames(communication_result$canonical_table)) {
    return(sprintf(
      paste0("Source : %s — rang importe, rang 1 = meilleur ",
             "(echelle de la source ; non comparable entre modes). ",
             "P-values importees, non re-inferrees. Association ",
             "ligand-receptor = aucune causalite."),
      communication_result$source_method %||% NA_character_
    ))
  }
  sprintf(
    paste0("Source : %s — score importe, echelle de la source ",
           "(non comparable entre sources). P-values importees, non ",
           "re-inferrees. Association ligand-receptor = aucune causalite."),
    communication_result$source_method %||% NA_character_
  )
}

#' Appliquer les filtres d'affichage (operation d'affichage, jamais destructive)
#'
#' Filtre la table canonique pour les vues. La table du RESULTAT n'est jamais
#' modifiee : une copie filtree est retournee. Semantique explicite :
#'   - score_min / p_value_max : une ligne sans valeur du champ filtre est
#'     retiree (un filtre non verifiable ne passe pas) — le retrait est
#'     comptabilise dans summary ;
#'   - pathways / senders / receivers : appartenance ; les filtres
#'     sender/receiver portent sur le NOEUD coalescent (identite harmonisee
#'     sinon label brut) ;
#'   - include_self = FALSE retire les auto-interactions (sender = receiver).
#'
#' @param communication_result Resultat canonique (assert_communication_result).
#' @param filters list(score_min = NA_real_, p_value_max = NA_real_,
#'   pathways = character(0), senders = character(0), receivers = character(0),
#'   include_self = TRUE) — champs absents pris par defaut.
#' @return list(table = copie filtree (+ sender_node/receiver_node),
#'   filters = filtres normalises, summary = list(n_before, n_after,
#'   dropped_score, dropped_p_value, dropped_pathway, dropped_sender,
#'   dropped_receiver, dropped_self), description = chaine lisible).
#' @export
communication_apply_filters <- function(communication_result,
                                        filters = list()) {
  r <- assert_communication_result(
    communication_result, context = "filtres communication"
  )
  rank_max_raw <- filters$rank_max %||% NA_real_
  if (length(rank_max_raw) != 1L) {
    .views_stop("communication_apply_filters() : rank_max doit etre un scalaire.")
  }
  rank_max <- suppressWarnings(as.numeric(rank_max_raw))
  if (is.na(rank_max) && !is.na(rank_max_raw)) {
    .views_stop("communication_apply_filters() : rank_max doit etre numerique.")
  }
  if (!is.na(rank_max) && rank_max < 0) {
    .views_stop("communication_apply_filters() : rank_max doit etre positif ou nul.")
  }
  f <- list(
    score_min = filters$score_min %||% NA_real_,
    rank_max = rank_max,
    p_value_max = filters$p_value_max %||% NA_real_,
    pathways = as.character(filters$pathways %||% character(0)),
    senders = as.character(filters$senders %||% character(0)),
    receivers = as.character(filters$receivers %||% character(0)),
    include_self = filters$include_self %||% TRUE
  )

  table <- .communication_node_keys(r$canonical_table)
  n_before <- nrow(table)
  keep <- rep(TRUE, n_before)
  score_values <- table$score
  score_available <- any(!is.na(score_values))
  rank_available <- "rank" %in% colnames(table)
  score_requested <- !is.na(f$score_min)
  rank_requested <- !is.na(f$rank_max)

  if (score_requested && rank_requested) {
    .views_stop(paste0(
      "communication_apply_filters() : score_min et rank_max sont incompatibles ",
      "sur une meme selection ; un rang n'est pas un score."
    ))
  }
  if (rank_requested && !rank_available) {
    .views_stop(paste0(
      "communication_apply_filters() : rank_max demande mais aucune colonne ",
      "rank n'est presente dans le resultat."
    ))
  }
  if (score_requested && rank_available && !score_available) {
    .views_stop(paste0(
      "communication_apply_filters() : score_min demande mais le resultat ",
      "ne contient aucune valeur de score ; un rang n'est pas un score."
    ))
  }

  if (score_requested) {
    keep <- keep & !is.na(score_values) & score_values >= f$score_min
  }
  dropped_score <- sum(!keep)
  dropped_rank <- 0L
  if (rank_requested) {
    rank_values <- suppressWarnings(as.numeric(table$rank))
    keep2 <- is.finite(rank_values) & rank_values <= f$rank_max
    dropped_rank <- sum(keep & !keep2)
    keep <- keep & keep2
  }
  if (!is.na(f$p_value_max)) {
    keep2 <- !is.na(table$p_value) & table$p_value <= f$p_value_max
    dropped_p <- sum(keep & !keep2)
    keep <- keep & keep2
  } else dropped_p <- 0L
  if (length(f$pathways)) {
    keep2 <- !is.na(table$pathway) & table$pathway %in% f$pathways
    dropped_pw <- sum(keep & !keep2)
    keep <- keep & keep2
  } else dropped_pw <- 0L
  if (length(f$senders)) {
    keep2 <- table$sender_node %in% f$senders
    dropped_s <- sum(keep & !keep2)
    keep <- keep & keep2
  } else dropped_s <- 0L
  if (length(f$receivers)) {
    keep2 <- table$receiver_node %in% f$receivers
    dropped_r <- sum(keep & !keep2)
    keep <- keep & keep2
  } else dropped_r <- 0L
  if (isFALSE(f$include_self)) {
    keep2 <- !(table$sender_node == table$receiver_node)
    dropped_self <- sum(keep & !keep2)
    keep <- keep & keep2
  } else dropped_self <- 0L

  table <- table[keep, , drop = FALSE]

  description <- sprintf(
    paste0("score_min=%s ; rank_max=%s ; p_value_max=%s ; pathways=[%s] ; ",
           "senders=[%s] ; receivers=[%s] ; include_self=%s ; dropped_rank=%s"),
    ifelse(is.na(f$score_min), "NA", format(f$score_min)),
    ifelse(is.na(f$rank_max), "NA", format(f$rank_max)),
    ifelse(is.na(f$p_value_max), "NA", format(f$p_value_max)),
    paste(f$pathways, collapse = ","), paste(f$senders, collapse = ","),
    paste(f$receivers, collapse = ","), as.character(f$include_self),
    as.character(dropped_rank)
  )

  list(
    table = table,
    filters = f,
    summary = list(
      n_before = as.integer(n_before),
      n_after = as.integer(nrow(table)),
      dropped_score = as.integer(dropped_score),
      dropped_rank = as.integer(dropped_rank),
      dropped_p_value = as.integer(dropped_p),
      dropped_pathway = as.integer(dropped_pw),
      dropped_sender = as.integer(dropped_s),
      dropped_receiver = as.integer(dropped_r),
      dropped_self = as.integer(dropped_self)
    ),
    description = description
  )
}

#' Vue DotPlot sender-receiver (aggregation descriptive)
#'
#' Taille = nombre d'interactions importees par paire de noeuds ; couleur =
#' score moyen importe (absent si aucun score — le point porte alors
#' uniquement l'effectif, sans substitution). Sous-titre : methode source et
#' type de score (garde-fou d'echelle).
#'
#' @param communication_result Resultat canonique.
#' @param filtered_table Table filtree (communication_apply_filters()$table)
#'   ou NULL pour utiliser la table complete du resultat.
#' @param seurat_obj Objet Seurat courant optionnel (peremption).
#' @return ggplot.
#' @export
plot_communication_dotplot <- function(communication_result, filtered_table = NULL,
                                       seurat_obj = NULL) {
  r <- assert_communication_result(
    communication_result, seurat_obj = seurat_obj,
    context = "DotPlot sender-receiver"
  )
  table <- .communication_node_keys(
    if (is.null(filtered_table)) r$canonical_table else filtered_table
  )
  subtitle_base <- .communication_score_caption(r)

  if (nrow(table) == 0L) {
    return(.communication_empty_view_plot(
      "Aucune interaction apres filtres : elargissez les filtres ou reimportez.",
      "DotPlot sender-receiver", subtitle_base
    ))
  }

  agg <- .communication_agg_pairs(table)
  has_rank <- "mean_rank" %in% colnames(agg) &&
    any(is.finite(agg$mean_rank))
  has_score <- any(!is.na(agg$mean_score))

  p <- ggplot2::ggplot(
    agg,
    ggplot2::aes(x = receiver_node, y = sender_node,
                 size = n_interactions,
                 colour = if (has_rank) mean_rank else if (has_score) mean_score else n_interactions)
  ) +
    ggplot2::geom_point(alpha = 0.9) +
    ggplot2::scale_size(range = c(2, 9), name = "Nb interactions") +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      x = "Receiver (noeud harmonise)", y = "Sender (noeud harmonise)",
      title = "Interactions sender-receiver (resultats importes)",
      subtitle = sprintf(
        paste0("%s — %d interaction(s) affichee(s). %s"),
        subtitle_base, sum(agg$n_interactions),
        if (has_rank)
          "Couleur : rang moyen importe par paire (1 = meilleur)."
        else if (has_score)
          "Couleur : score moyen importe par paire."
        else
          "Aucun score dans la selection : l'effectif seule est affiche."
      )
    )
  if (has_rank) {
    p <- p + ggplot2::scale_colour_gradient(
      low = "#E8F1FA", high = "#2166AC",
      trans = scales::reverse_trans(),
      name = "rang moyen (1 = meilleur)"
    )
  } else if (has_score) {
    p <- p + ggplot2::scale_colour_gradient(
      low = "#E8F1FA", high = "#2166AC", name = "Score moyen importe"
    )
  } else {
    p <- p + ggplot2::scale_colour_gradient(
      low = "#E8F1FA", high = "#2166AC", name = "Nb interactions"
    )
  }
  p
}

#' Vue heatmap pathways x paires (aggregation descriptive)
#'
#' Une case = score moyen importe d'un pathway pour une paire de noeuds. Les
#' lignes sans pathway (NA) sont EXCLUES de la vue et comptabilisees dans le
#' sous-titre — jamais imputees a un pathway invente.
#'
#' @param communication_result Resultat canonique.
#' @param filtered_table Table filtree ou NULL (table complete).
#' @param seurat_obj Objet Seurat courant optionnel (peremption).
#' @return ggplot.
#' @export
plot_communication_pathway_heatmap <- function(communication_result,
                                               filtered_table = NULL,
                                               seurat_obj = NULL) {
  r <- assert_communication_result(
    communication_result, seurat_obj = seurat_obj,
    context = "heatmap pathways"
  )
  table <- .communication_node_keys(
    if (is.null(filtered_table)) r$canonical_table else filtered_table
  )
  subtitle_base <- .communication_score_caption(r)
  n_no_pathway <- sum(is.na(table$pathway))
  table <- table[!is.na(table$pathway), , drop = FALSE]

  if (nrow(table) == 0L) {
    return(.communication_empty_view_plot(
      paste0(
        if (n_no_pathway > 0L)
          paste0("Aucun pathway renseigne dans la source (", n_no_pathway,
                 " interaction(s) sans pathway). ")
        else "Aucune interaction apres filtres. ",
        "La vue heatmap necessite des pathways — utilisez une table ",
        "CellChat exportee avec la colonne pathway."
      ),
      "Heatmap pathways", subtitle_base
    ))
  }

  table$pair_label <- paste(table$sender_node, table$receiver_node, sep = " \u2192 ")
  agg <- do.call(rbind, lapply(
    split(table, paste(table$pathway, table$pair_label, sep = "\r")),
    function(sub) data.frame(
      pathway = sub$pathway[1L], pair_label = sub$pair_label[1L],
      mean_score = if (any(!is.na(sub$score))) mean(sub$score, na.rm = TRUE) else NA_real_,
      n_interactions = nrow(sub),
      stringsAsFactors = FALSE
    )
  ))

  ggplot2::ggplot(agg, ggplot2::aes(x = pair_label, y = pathway,
                                    fill = mean_score)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::scale_fill_gradient(low = "#F7F4F0", high = "#B2182B",
                                 na.value = "grey90",
                                 name = "Score moyen importe") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      x = NULL, y = NULL,
      title = "Pathways par paire sender-receiver (resultats importes)",
      subtitle = sprintf(
        "%s — %d interaction(s) avec pathway ; %d sans pathway (exclues, jamais imputees).",
        subtitle_base, sum(agg$n_interactions), n_no_pathway
      )
    )
}

#' Vue reseau circulaire (ggplot pur, courbes de Bezier)
#'
#' Noeuds = identites harmonisees (sinon labels bruts) sur un cercle ; aretes =
#' paires sender-receiver aggregatees, largeur/transparence par poids importe,
#' fleche orientee sender -> receiver. Les auto-interactions ne sont pas
#' dessinables sur le cercle : elles sont comptabilisees dans le sous-titre.
#' VUE DESCRIPTIVE — aucune lecture causale.
#'
#' @param communication_result Resultat canonique.
#' @param filtered_table Table filtree ou NULL (table complete).
#' @param seurat_obj Objet Seurat courant optionnel (peremption).
#' @return ggplot.
#' @export
plot_communication_circle <- function(communication_result, filtered_table = NULL,
                                      seurat_obj = NULL) {
  r <- assert_communication_result(
    communication_result, seurat_obj = seurat_obj,
    context = "reseau circulaire"
  )
  table <- .communication_node_keys(
    if (is.null(filtered_table)) r$canonical_table else filtered_table
  )
  subtitle_base <- .communication_score_caption(r)
  n_self <- sum(table$sender_node == table$receiver_node)
  edges_table <- table[table$sender_node != table$receiver_node, , drop = FALSE]

  if (nrow(edges_table) == 0L) {
    return(.communication_empty_view_plot(
      sprintf(
        paste0("Aucune arete dessinable : %d interaction(s) dans la ",
               "selection dont %d auto-interaction(s) (sender = receiver, ",
               "non dessinables sur le cercle)."),
        nrow(table), n_self
      ),
      "Reseau sender-receiver", subtitle_base
    ))
  }

  agg <- .communication_agg_pairs(edges_table)
  has_rank <- "mean_rank" %in% colnames(agg) &&
    any(is.finite(agg$mean_rank))
  has_score <- any(!is.na(agg$sum_score))
  agg$weight <- if (has_rank) agg$n_interactions else if (has_score) agg$sum_score else agg$n_interactions

  nodes <- sort(unique(c(agg$sender_node, agg$receiver_node)))
  n_nodes <- length(nodes)
  angle <- seq(0, 2 * pi, length.out = n_nodes + 1L)[seq_len(n_nodes)]
  node_pos <- data.frame(
    node = nodes,
    x = sin(angle), y = cos(angle),
    stringsAsFactors = FALSE
  )

  # Bezier quadratique : point de controle tire vers le centre (0.35) —
  # courbure deterministe, echantillonnage fixe.
  n_pts <- 40L
  t_seq <- seq(0, 1, length.out = n_pts)
  edge_df <- do.call(rbind, lapply(seq_len(nrow(agg)), function(i) {
    p1 <- node_pos[node_pos$node == agg$sender_node[i], c("x", "y")]
    p2 <- node_pos[node_pos$node == agg$receiver_node[i], c("x", "y")]
    ctrl <- (p1 + p2) * 0.35
    data.frame(
      edge_id = i,
      t = t_seq,
      x = (1 - t_seq)^2 * p1$x + 2 * (1 - t_seq) * t_seq * ctrl$x + t_seq^2 * p2$x,
      y = (1 - t_seq)^2 * p1$y + 2 * (1 - t_seq) * t_seq * ctrl$y + t_seq^2 * p2$y,
      weight = agg$weight[i],
      stringsAsFactors = FALSE
    )
  }))

  node_tot <- do.call(rbind, lapply(node_pos$node, function(nd) {
    out_w <- sum(agg$weight[agg$sender_node == nd], na.rm = TRUE)
    in_w <- sum(agg$weight[agg$receiver_node == nd], na.rm = TRUE)
    data.frame(node = nd, total = out_w + in_w, stringsAsFactors = FALSE)
  }))
  node_pos$total <- node_tot$total[match(node_pos$node, node_tot$node)]

  ggplot2::ggplot() +
    ggplot2::geom_path(
      data = edge_df,
      ggplot2::aes(x = x, y = y, group = edge_id,
                   linewidth = weight, alpha = weight),
      lineend = "round",
      arrow = grid::arrow(length = grid::unit(7, "pt"), type = "closed")
    ) +
    ggplot2::geom_point(
      data = node_pos,
      ggplot2::aes(x = x, y = y, size = total),
      colour = "#2166AC", alpha = 0.9
    ) +
    ggplot2::geom_text(
      data = node_pos,
      ggplot2::aes(x = x * 1.18, y = y * 1.18, label = node),
      size = 3.4
    ) +
    ggplot2::scale_linewidth(range = c(0.2, 2.4), name = if (has_rank) "Nb interactions" else if (has_score) "Poids (score importe)" else "Nb interactions") +
    ggplot2::scale_alpha(range = c(0.25, 0.9), guide = "none") +
    ggplot2::scale_size(range = c(3, 9), guide = "none") +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::labs(
      title = "Reseau sender-receiver (resultats importes)",
      subtitle = sprintf(
        paste0("%s — %d arete(s) ; %d auto-interaction(s) non dessinees. ",
               "Vue descriptive : aucune causalite.%s"),
        subtitle_base, nrow(agg), n_self,
        if (has_rank)
          " Poids : nombre d'interactions ; rang 1 = meilleur, jamais somme des rangs."
        else ""
      )
    )
}

#' Centralite reseau descriptive (NIVEAU RESEAU — pas un controle biologique)
#'
#' Par noeud : nombre d'interactions sortantes/entrantes, partenaires distincts,
#' somme des scores importes sortants/entrants. Ces quantites sont derivees du
#' graphe des resultats importes — elles ne mesurent AUCUNE activite
#' biologique.
#'
#' @param communication_result Resultat canonique.
#' @param filtered_table Table filtree ou NULL (table complete).
#' @return data.frame trie par poids total decroissant, trace par analysis_id.
#' @export
build_communication_centrality <- function(communication_result,
                                           filtered_table = NULL) {
  r <- assert_communication_result(
    communication_result, context = "centralite communication"
  )
  table <- .communication_node_keys(
    if (is.null(filtered_table)) r$canonical_table else filtered_table
  )
  nodes <- sort(unique(c(table$sender_node, table$receiver_node)))
  has_rank <- "rank" %in% colnames(table)
  if (length(nodes) == 0L) {
    out <- data.frame(
      node = character(),
      n_out_interactions = integer(),
      n_in_interactions = integer(),
      out_partners = integer(),
      in_partners = integer(),
      out_score_total = numeric(),
      in_score_total = numeric(),
      total_interactions = integer(),
      stringsAsFactors = FALSE
    )
    if (has_rank) {
      out$out_best_rank <- numeric()
      out$out_median_rank <- numeric()
      out$in_best_rank <- numeric()
      out$in_median_rank <- numeric()
    }
    out$analysis_id <- rep(r$analysis_id %||% NA_character_, nrow(out))
    out$source_method <- rep(r$source_method %||% NA_character_, nrow(out))
    out$identity_column <- rep(r$identity_column %||% NA_character_, nrow(out))
    return(out)
  }

  out <- do.call(rbind, lapply(nodes, function(nd) {
    s_rows <- table[table$sender_node == nd, ]
    r_rows <- table[table$receiver_node == nd, ]
    s_sc <- s_rows$score[!is.na(s_rows$score)]
    r_sc <- r_rows$score[!is.na(r_rows$score)]
    out_row <- data.frame(
      node = nd,
      n_out_interactions = nrow(s_rows),
      n_in_interactions = nrow(r_rows),
      out_partners = length(unique(s_rows$receiver_node)),
      in_partners = length(unique(r_rows$sender_node)),
      out_score_total = if (length(s_sc)) sum(s_sc) else NA_real_,
      in_score_total = if (length(r_sc)) sum(r_sc) else NA_real_,
      stringsAsFactors = FALSE
    )
    if (has_rank) {
      s_rk <- suppressWarnings(as.numeric(s_rows$rank))
      r_rk <- suppressWarnings(as.numeric(r_rows$rank))
      s_rk <- s_rk[is.finite(s_rk)]
      r_rk <- r_rk[is.finite(r_rk)]
      out_row$out_best_rank <- if (length(s_rk)) min(s_rk) else NA_real_
      out_row$out_median_rank <- if (length(s_rk)) stats::median(s_rk) else NA_real_
      out_row$in_best_rank <- if (length(r_rk)) min(r_rk) else NA_real_
      out_row$in_median_rank <- if (length(r_rk)) stats::median(r_rk) else NA_real_
    }
    out_row
  }))
  out$total_interactions <- out$n_out_interactions + out$n_in_interactions
  out <- out[order(-out$total_interactions), , drop = FALSE]
  rownames(out) <- NULL
  out$analysis_id <- r$analysis_id %||% NA_character_
  out$source_method <- r$source_method %||% NA_character_
  out$identity_column <- r$identity_column %||% NA_character_
  out
}

#' Entree de provenance pour une exploration filtree (PRODUITE a l'export)
#'
#' Les filtres sont des operations d'affichage ; leur trace provenance est
#' PRODUITE quand l'utilisateur exporte (filtres actifs figes dans les
#' parametres) — jamais reconstruite apres coup. L'appelant (module) append
#' l'entree a l'etat partage.
#'
#' @param communication_result Resultat canonique.
#' @param filter_summary Sortie de communication_apply_filters().
#' @return Entree new_provenance_entry() enrichie (analysis_type, status).
#' @export
build_communication_filter_provenance <- function(communication_result,
                                                  filter_summary) {
  r <- assert_communication_result(
    communication_result, context = "provenance filtres communication"
  )
  if (!is.list(filter_summary) || is.null(filter_summary$description)) {
    .views_stop(
      "build_communication_filter_provenance() : filter_summary doit provenir de communication_apply_filters()."
    )
  }
  entry <- new_provenance_entry(
    analysis_id = "sc-communication-explore",
    method = paste0("explore_", r$source_method %||% "unknown"),
    parameters = list(
      applied_filters = filter_summary$description,
      rank_max = filter_summary$filters$rank_max %||% NA_real_,
      n_rows_before = filter_summary$summary$n_before %||% NA_integer_,
      n_rows_after = filter_summary$summary$n_after %||% NA_integer_,
      dropped_score = filter_summary$summary$dropped_score %||% NA_integer_,
      dropped_rank = filter_summary$summary$dropped_rank %||% NA_integer_,
      dropped_p_value = filter_summary$summary$dropped_p_value %||% NA_integer_,
      dropped_pathway = filter_summary$summary$dropped_pathway %||% NA_integer_,
      dropped_sender = filter_summary$summary$dropped_sender %||% NA_integer_,
      dropped_receiver = filter_summary$summary$dropped_receiver %||% NA_integer_,
      dropped_self = filter_summary$summary$dropped_self %||% NA_integer_,
      identity_column = r$identity_column,
      base_analysis_id = r$analysis_id
    ),
    dataset = NULL,
    warnings = as.character(r$warnings %||% character(0))
  )
  entry$analysis_type <- "cell_cell_communication"
  entry$status <- r$status
  entry$import_only <- FALSE
  entry
}

#' Export CSV de la table filtree (filtres + analysis_id embarques)
#'
#' Chaque ligne porte l'identite de l'analyse de base, l'horodatage et la
#' description des filtres actifs — l'export reste reproductible.
#'
#' @param communication_result Resultat canonique.
#' @param filtered_table Table filtree (communication_apply_filters()$table).
#' @param filter_description Description lisible des filtres (champ description).
#' @return data.frame (n_after lignes).
#' @export
build_communication_filtered_export <- function(communication_result,
                                                filtered_table,
                                                filter_description) {
  r <- assert_communication_result(
    communication_result, context = "export table filtree"
  )
  if (!is.data.frame(filtered_table)) {
    .views_stop("build_communication_filtered_export() : table filtree requise (communication_apply_filters()$table).")
  }
  n <- nrow(filtered_table)
  data.frame(
    filtered_table,
    analysis_id = rep(r$analysis_id %||% NA_character_, n),
    base_timestamp_utc = rep(r$timestamp_utc %||% NA_character_, n),
    applied_filters = rep(as.character(filter_description)[1L], n),
    stringsAsFactors = FALSE
  )
}

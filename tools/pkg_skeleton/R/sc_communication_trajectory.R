# =============================================================================
# R/sc/sc_communication_trajectory.R — CCC contexte trajectoire/pseudo-temps
# (roadmap avancée Phase 3 / V1.x-B)
# =============================================================================
# Consommateur PUR du résultat canonique communication (Stage 11) : il
# décrit comment les populations de chaque paire sender/receiver — et
# l'expression de leurs ligands/récepteurs — se distribuent le long d'un
# pseudo-temps, par bins, par lignée (jamais collapsées).
#
# REGLES (roadmap avancée, docs/ROADMAP_CCC_ADVANCED.md) :
#   - PSEUDO-TEMPS ≠ temps réel : ordonnement relatif d'une trajectoire
#     inférée — les sous-titres et exports l'énoncent explicitement ;
#   - le score importé d'une paire est CONSTANT par paire (niveau
#     population) : il n'est PAS décliné par bin — seules l'expression des
#     gènes et la composition des populations varient le long du pseudo-temps ;
#   - plusieurs lignées ne sont JAMAIS collapsées en une trajectoire
#     artificielle globale (mode par lignée explicite) ;
#   - gènes absents de la matrice, cellules sans pseudo-temps, labels sans
#     correspondance : exclus ET comptabilisés — jamais imputés ;
#   - la table canonique du résultat parent n'est jamais modifiée ;
#   - aucune densification : seuls les gènes nécessaires sont extraits.
#
# Sourced in app.R AFTER R/sc/sc_communication_spatial.R. Erreurs classées
# `communication_context_error` (état structuré via l'attribut `state`).
#
# ── API PUBLIQUE FIGÉE (V1.x-B) ─────────────────────────────────────────────
# Le test de freeze (test-communication-trajectory-contract-freeze.R) vérifie
# cette surface + le schéma du résultat.
# =============================================================================

#' Surface publique figée de R/sc/sc_communication_trajectory.R (V1.x-B)
#'
#' @return Vecteur character des noms de fonctions publiques.
#' @export
communication_trajectory_public_api <- function() {
  c(
    "communication_trajectory_public_api",
    "communication_trajectory_contract_fields",
    "communication_fetch_expression_matrix",
    "build_communication_trajectory_context",
    "plot_communication_trajectory_curves",
    "plot_communication_trajectory_heatmap",
    "build_communication_trajectory_export"
  )
}

#' Champs contractuels du contexte trajectoire (V1.x-B)
#' @export
communication_trajectory_contract_fields <- function() {
  c(
    "type", "status", "analysis_id", "parent_analysis_id", "source_method",
    "identity_column", "params", "pair_bin_table", "pair_summary", "qc",
    "warnings", "provenance", "timestamp_utc"
  )
}

.traj_stop <- function(message, state = "invalid_input") {
  stop(errorCondition(
    message, state = state, class = "communication_context_error"
  ))
}

# Coalescence noeud = identité harmonisée sinon label brut (implémentation
# propre au fichier — les helpers des autres fichiers restent chez eux).
.traj_node_keys <- function(table) {
  sender_mapped <- table$sender_mapped %||% rep(NA_character_, nrow(table))
  receiver_mapped <- table$receiver_mapped %||% rep(NA_character_, nrow(table))
  table$sender_node <- ifelse(!is.na(sender_mapped), sender_mapped, table$sender)
  table$receiver_node <- ifelse(!is.na(receiver_mapped), receiver_mapped, table$receiver)
  table
}

# rowMeans compatible matrice dense / sparse (aucune densification).
.traj_row_means <- function(m) {
  if (inherits(m, "sparseMatrix")) Matrix::rowMeans(m) else rowMeans(m)
}

# Sous-titre commun des figures de contexte trajectoire (garde-fou).
.traj_caption <- function(context) {
  paste0(
    "Pseudo-temps = ordonnement relatif d'une trajectoire inférée, ",
    "PAS un temps réel. Scores importés (échelle de la source : ",
    context$source_method %||% NA, "), constants par paire — non déclinés ",
    "par bin. Expression = données normalisées ('data')."
  )
}

.traj_stub_plot <- function(message, title, subtitle) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = message,
                      size = 4.5, colour = "grey40") +
    ggplot2::theme_void() +
    ggplot2::labs(title = title, subtitle = paste(message, subtitle, sep = " — "))
}

#' Extraire la matrice d'expression normalisée de l'objet courant (V1.x-B)
#'
#' Aide-module : renvoie genes × cellules (slot/liter 'data' — données
#' normalisées), lignes restreintes aux gènes demandés présents dans
#' l'assay (mémoire bornée). Les gènes absents sont simplement absents du
#' résultat — le moteur de contexte les compte comme manquants.
#'
#' @param seurat_obj Objet Seurat courant.
#' @param genes Vecteur de gènes demandés (NULL = tous).
#' @param assay Assay déclaré (défaut "RNA").
#' @return Matrix/dgCMatrix genes_present × cells.
#' @export
communication_fetch_expression_matrix <- function(seurat_obj, genes = NULL,
                                                  assay = "RNA") {
  if (is.null(seurat_obj) || !inherits(seurat_obj, "Seurat")) {
    .traj_stop("communication_fetch_expression_matrix() : objet Seurat requis.")
  }
  if (!assay %in% names(seurat_obj@assays)) {
    .traj_stop(sprintf(
      "communication_fetch_expression_matrix() : assay '%s' absent de l'objet (assays : %s).",
      assay, paste(names(seurat_obj@assays), collapse = ", ")
    ))
  }
  data <- tryCatch(
    SeuratObject::GetAssayData(seurat_obj, assay = assay, layer = "data"),
    error = function(e) SeuratObject::GetAssayData(seurat_obj, assay = assay, slot = "data")
  )
  if (!is.null(genes)) {
    data <- data[intersect(as.character(genes), rownames(data)), , drop = FALSE]
  }
  data
}

#' Construire le contexte trajectoire d'un résultat communication (V1.x-B)
#'
#' Pour chaque paire de noeuds (sender, receiver) — et pour chaque lignée si
#' `lineage` est fourni (jamais collapsées) — décrit par bins de pseudo-temps
#' : effectifs de population, fractions de la population présente par bin et
#' expression moyenne des ligands (cellules sender) / récepteurs (cellules
#' receiver). Le score importé reste un attribut de PAIRE (pair_summary) —
#' il n'est jamais décliné par bin.
#'
#' @param communication_result Résultat canonique (assert_communication_result).
#' @param expression_matrix Matrix/dgCMatrix genes × cells (données
#'   normalisées 'data', p.ex. via communication_fetch_expression_matrix()).
#' @param cell_identities Vecteur character NOMMÉ (cellule → identité).
#' @param pseudotime Vecteur numérique NOMMÉ (cellule → pseudo-temps, fini).
#' @param lineage Vecteur character NOMMÉ optionnel (cellule → lignée) ;
#'   fourni = calcul PAR LIGNÉE, jamais de trajectoire globale artificielle.
#' @param identity_column Nom déclaré de la colonne d'identités (traçabilité).
#' @param n_bins Nombre de bins de pseudo-temps (2–100, défaut 10).
#' @param filtered_table Table canonique ou pré-filtrée par les vues (défaut
#'   NULL = table complète).
#' @return Objet canonique de contexte (communication_trajectory_contract_fields()).
#' @export
build_communication_trajectory_context <- function(communication_result,
                                                   expression_matrix,
                                                   cell_identities,
                                                   pseudotime,
                                                   lineage = NULL,
                                                   identity_column = NA_character_,
                                                   n_bins = 10L,
                                                   filtered_table = NULL) {
  r <- assert_communication_result(communication_result, context = "contexte trajectoire")

  n_bins_req <- as.integer(n_bins)
  if (length(n_bins_req) != 1L || is.na(n_bins_req) || n_bins_req < 2L || n_bins_req > 100L) {
    .traj_stop("build_communication_trajectory_context() : 'n_bins' doit être un entier entre 2 et 100.")
  }
  if (!is.matrix(expression_matrix) && !inherits(expression_matrix, "sparseMatrix")) {
    .traj_stop(paste0(
      "build_communication_trajectory_context() : 'expression_matrix' doit être ",
      "une matrix ou dgCMatrix genes x cells (données normalisées 'data')."
    ))
  }
  if (is.null(colnames(expression_matrix)) || is.null(rownames(expression_matrix))) {
    .traj_stop(paste0(
      "build_communication_trajectory_context() : 'expression_matrix' doit porter ",
      "des noms de gènes (lignes) et de cellules (colonnes)."
    ))
  }
  first_vals <- tryCatch(as.vector(expression_matrix[, 1L]), error = function(e) NULL)
  if (is.null(first_vals) || !is.numeric(first_vals)) {
    .traj_stop(paste0(
      "build_communication_trajectory_context() : 'expression_matrix' doit ",
      "contenir des valeurs numériques."
    ))
  }
  if (!is.character(cell_identities) || is.null(names(cell_identities)) ||
      anyNA(names(cell_identities)) || !all(nzchar(names(cell_identities))) ||
      anyNA(cell_identities) || !all(nzchar(cell_identities))) {
    .traj_stop(paste0(
      "build_communication_trajectory_context() : 'cell_identities' doit être un ",
      "vecteur character nommé (cellule -> identité), sans valeur vide."
    ))
  }
  if (anyDuplicated(names(cell_identities))) {
    .traj_stop("build_communication_trajectory_context() : noms de cellules dupliqués dans 'cell_identities'.")
  }
  if (!is.numeric(pseudotime) || is.null(names(pseudotime)) ||
      anyDuplicated(names(pseudotime))) {
    .traj_stop(paste0(
      "build_communication_trajectory_context() : 'pseudotime' doit être un ",
      "vecteur numérique nommé (cellule -> pseudo-temps), sans doublon de noms."
    ))
  }
  if (!is.null(lineage)) {
    if (!is.character(lineage) || is.null(names(lineage)) ||
        anyNA(lineage) || !all(nzchar(lineage)) || anyDuplicated(names(lineage))) {
      .traj_stop(paste0(
        "build_communication_trajectory_context() : 'lineage' doit être un ",
        "vecteur character nommé (cellule -> lignée) ou NULL."
      ))
    }
  }

  warnings_all <- character(0)
  if (!is.na(identity_column) && !is.na(r$identity_column) &&
      !identical(identity_column, r$identity_column)) {
    warnings_all <- c(warnings_all, sprintf(
      "Colonne d'identités déclarée (%s) différente de celle de l'import (%s) — la coalescence des noeuds reste celle de l'import.",
      identity_column, r$identity_column
    ))
  }

  cells_expr <- colnames(expression_matrix)
  cells_pt <- names(pseudotime)[!is.na(pseudotime) & is.finite(pseudotime)]
  cells_matched <- Reduce(intersect, list(cells_expr, names(cell_identities), cells_pt))
  qc <- list(
    n_cells_expression = length(cells_expr),
    n_cells_identities = length(cell_identities),
    n_cells_pseudotime = length(names(pseudotime)),
    n_cells_pseudotime_excluded = length(names(pseudotime)) - length(cells_pt),
    n_cells_matched = length(cells_matched),
    n_pairs_total = 0L, n_combinations_total = 0L,
    n_combinations_mapped = 0L, n_combinations_unmapped = 0L,
    unmapped_combinations = character(0),
    n_bins_requested = n_bins_req, n_bins_effective = NA_integer_,
    lineage_mode = if (is.null(lineage)) "global" else "per_lineage"
  )
  if (length(cells_matched) < 2L) {
    .traj_stop(paste0(
      "build_communication_trajectory_context() : moins de 2 cellules communes ",
      "entre expression, identités et pseudo-temps (", length(cells_matched),
      ") — le contexte trajectoire n'est pas calculable."
    ), state = "invalid_identity_mapping")
  }
  if (qc$n_cells_pseudotime_excluded > 0L) {
    warnings_all <- c(warnings_all, sprintf(
      "%d cellule(s) avec pseudo-temps NA/non fini exclue(s) (comptabilisées, jamais imputées).",
      qc$n_cells_pseudotime_excluded
    ))
  }

  ids_matched <- cell_identities[cells_matched]
  pt_matched <- pseudotime[cells_matched]

  breaks <- stats::quantile(pt_matched,
                            probs = seq(0, 1, length.out = n_bins_req + 1L),
                            names = FALSE)
  breaks_u <- unique(breaks)
  effective_bins <- length(breaks_u) - 1L
  qc$n_bins_effective <- as.integer(effective_bins)
  if (effective_bins < n_bins_req) {
    warnings_all <- c(warnings_all, sprintf(
      "Bins réduits de %d à %d (pseudo-temps à valeurs identiques : quantiles dupliqués) — comptabilisé, jamais forcé.",
      n_bins_req, effective_bins
    ))
  }
  bin_of <- setNames(
    as.integer(cut(pt_matched, breaks = breaks_u, include.lowest = TRUE, labels = FALSE)),
    cells_matched
  )

  table <- .traj_node_keys(
    if (is.null(filtered_table)) r$canonical_table else as.data.frame(filtered_table)
  )
  pairs <- unique(paste(table$sender_node, table$receiver_node, sep = "\r"))
  qc$n_pairs_total <- length(pairs)

  genes_needed <- unique(c(stats::na.omit(table$ligand), stats::na.omit(table$receptor)))
  genes_present <- intersect(genes_needed, rownames(expression_matrix))

  scopes <- if (is.null(lineage)) {
    list(list(label = NA_character_, cells = cells_matched))
  } else {
    lin_matched <- lineage[cells_matched]
    lapply(sort(unique(lin_matched)), function(l) {
      list(label = l, cells = cells_matched[lin_matched == l])
    })
  }

  pair_rows <- list()
  pair_summary_rows <- list()
  unmapped <- character(0)
  for (sc in scopes) {
    sc_cells <- sc$cells
    for (k in pairs) {
      sp <- strsplit(k, "\r", fixed = TRUE)[[1L]]
      sub <- table[table$sender_node == sp[1L] & table$receiver_node == sp[2L], ]
      sc_pool <- ids_matched[sc_cells]
      si <- sc_cells[sc_pool == sp[1L]]
      ri <- sc_cells[sc_pool == sp[2L]]
      if (length(si) == 0L || length(ri) == 0L) {
        unmapped <- c(unmapped, sprintf(
          "%s -> %s%s", sp[1L], sp[2L],
          if (is.na(sc$label)) "" else paste0(" [lignée ", sc$label, "]")
        ))
        next
      }
      lig_genes <- unique(stats::na.omit(sub$ligand))
      rec_genes <- unique(stats::na.omit(sub$receptor))
      lig_in <- intersect(lig_genes, genes_present)
      rec_in <- intersect(rec_genes, genes_present)
      sc_sub <- sub$score[!is.na(sub$score)]
      pair_summary_rows[[length(pair_summary_rows) + 1L]] <- data.frame(
        sender_node = sp[1L], receiver_node = sp[2L],
        lineage = sc$label,
        n_sender_cells = length(si), n_receiver_cells = length(ri),
        ligand_genes = paste(lig_in, collapse = ","),
        receptor_genes = paste(rec_in, collapse = ","),
        n_ligand_genes_missing = length(setdiff(lig_genes, lig_in)),
        n_receptor_genes_missing = length(setdiff(rec_genes, rec_in)),
        mean_imported_score = if (length(sc_sub)) mean(sc_sub) else NA_real_,
        stringsAsFactors = FALSE
      )
      # Expression moyenne par bin : moyenne (sur les cellules du bin) des
      # moyennes par gène — gènes présents uniquement, manquants comptés.
      lig_mean_in_bin <- function(cells_bin) {
        if (length(cells_bin) == 0L || length(lig_in) == 0L) return(NA_real_)
        mean(.traj_row_means(expression_matrix[lig_in, cells_bin, drop = FALSE]))
      }
      rec_mean_in_bin <- function(cells_bin) {
        if (length(cells_bin) == 0L || length(rec_in) == 0L) return(NA_real_)
        mean(.traj_row_means(expression_matrix[rec_in, cells_bin, drop = FALSE]))
      }
      n_sender_total <- length(si)
      n_receiver_total <- length(ri)
      for (b in seq_len(effective_bins)) {
        cells_bin <- sc_cells[bin_of[sc_cells] == b]
        sb <- intersect(cells_bin, si)
        rb <- intersect(cells_bin, ri)
        pair_rows[[length(pair_rows) + 1L]] <- data.frame(
          sender_node = sp[1L], receiver_node = sp[2L], lineage = sc$label,
          bin = as.integer(b),
          bin_from = breaks_u[b], bin_to = breaks_u[b + 1L],
          bin_mid = (breaks_u[b] + breaks_u[b + 1L]) / 2,
          n_cells_in_bin = length(cells_bin),
          n_sender_cells = length(sb), n_receiver_cells = length(rb),
          frac_sender_of_bin = if (length(cells_bin)) length(sb) / length(cells_bin) else NA_real_,
          frac_receiver_of_bin = if (length(cells_bin)) length(rb) / length(cells_bin) else NA_real_,
          frac_sender_of_population = length(sb) / n_sender_total,
          frac_receiver_of_population = length(rb) / n_receiver_total,
          mean_ligand_expression_senders = lig_mean_in_bin(sb),
          mean_receptor_expression_receivers = rec_mean_in_bin(rb),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  qc$n_combinations_total <- length(pairs) * length(scopes)
  qc$n_combinations_mapped <- length(pair_summary_rows)
  qc$n_combinations_unmapped <- length(unmapped)
  qc$unmapped_combinations <- unmapped
  if (length(unmapped)) {
    warnings_all <- c(warnings_all, sprintf(
      "%d combinaison(s) paire/lignée non calculable(s) (population absente des identités du périmètre) : %s",
      length(unmapped), paste(utils::head(unmapped, 8), collapse = " ; ")
    ))
  }
  missing_genes <- setdiff(genes_needed, genes_present)
  if (length(missing_genes)) {
    warnings_all <- c(warnings_all, sprintf(
      "%d gène(s) demandé(s) absent(s) de la matrice d'expression (comptés manquants par paire, jamais imputés) : %s",
      length(missing_genes), paste(utils::head(missing_genes, 10), collapse = ", ")
    ))
  }
  if (length(pair_rows) == 0L) {
    .traj_stop(paste0(
      "build_communication_trajectory_context() : aucune combinaison ",
      "paire/lignée calculable (populations absentes des identités).",
      state = "invalid_identity_mapping"
    ), state = "invalid_identity_mapping")
  }

  pair_bin_table <- do.call(rbind, pair_rows)
  pair_summary <- do.call(rbind, pair_summary_rows)

  entry <- new_provenance_entry(
    analysis_id = "sc-communication-trajectory",
    method = "trajectory_context_bins",
    parameters = list(
      identity_column = identity_column,
      n_bins_requested = n_bins_req,
      n_bins_effective = effective_bins,
      lineage_mode = qc$lineage_mode,
      n_cells_matched = length(cells_matched),
      n_combinations_total = qc$n_combinations_total,
      n_combinations_mapped = qc$n_combinations_mapped,
      n_combinations_unmapped = qc$n_combinations_unmapped,
      expression = "data (normalisées)",
      parent_analysis_id = r$analysis_id
    ),
    warnings = warnings_all
  )
  entry$analysis_type <- "cell_cell_communication_trajectory"
  entry$import_only <- FALSE
  entry$parent_analysis_id <- r$analysis_id
  entry$status <- "valid"

  list(
    type = "ccc_trajectory_context",
    status = "valid",
    analysis_id = "sc-communication-trajectory",
    parent_analysis_id = r$analysis_id,
    source_method = r$source_method,
    identity_column = identity_column,
    params = list(
      n_bins_requested = n_bins_req, n_bins_effective = effective_bins,
      lineage_mode = qc$lineage_mode, expression = "data (normalisées)"
    ),
    pair_bin_table = pair_bin_table,
    pair_summary = pair_summary,
    qc = qc,
    warnings = warnings_all,
    provenance = entry,
    timestamp_utc = format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  )
}

#' Courbes ligand/récepteur le long du pseudo-temps pour une paire (V1.x-B)
#'
#' Consommatrice pure du contexte : deux courbes (expression moyenne des
#' ligands chez les senders, des récepteurs chez les receivers) par bin.
#' Les bins sans cellules restent NA (trou visible, jamais d'interpolation
#' trompeuse) ; sous-titre = garde-fou pseudo-temps.
#'
#' @param context Contexte trajectoire (build_communication_trajectory_context()).
#' @param sender_node,receiver_node Paire à dessiner.
#' @param lineage Lignée à dessiner (NULL = mode global).
#' @return ggplot.
#' @export
plot_communication_trajectory_curves <- function(context, sender_node,
                                                 receiver_node, lineage = NULL) {
  if (!is.list(context) || !identical(context$type %||% NULL, "ccc_trajectory_context")) {
    .traj_stop("plot_communication_trajectory_curves() : contexte trajectoire requis (build_communication_trajectory_context()).")
  }
  caption <- .traj_caption(context)
  pb <- context$pair_bin_table
  sel <- pb[pb$sender_node == sender_node & pb$receiver_node == receiver_node, , drop = FALSE]
  if (!is.null(lineage) && "lineage" %in% names(sel)) {
    sel <- sel[sel$lineage == lineage, , drop = FALSE]
  }
  if (nrow(sel) == 0L) {
    return(.traj_stub_plot(
      sprintf("Paire '%s -> %s'%s absente du contexte trajectoire.",
              sender_node, receiver_node,
              if (is.null(lineage)) "" else paste0(" (lignée ", lineage, ")")),
      "Contexte trajectoire — paire sender-receiver", caption
    ))
  }
  long <- rbind(
    data.frame(bin_mid = sel$bin_mid, value = sel$mean_ligand_expression_senders,
               quantity = "Ligands (cellules sender)"),
    data.frame(bin_mid = sel$bin_mid, value = sel$mean_receptor_expression_receivers,
               quantity = "Récepteurs (cellules receiver)")
  )
  n_na <- sum(is.na(long$value))
  ggplot2::ggplot(long, ggplot2::aes(x = bin_mid, y = value, colour = quantity)) +
    ggplot2::geom_line(na.rm = TRUE, linewidth = 0.9) +
    ggplot2::geom_point(na.rm = TRUE, size = 2.4) +
    ggplot2::theme_classic() +
    ggplot2::labs(
      x = "Pseudo-temps (centre de bin)", y = "Expression moyenne ('data')",
      colour = NULL,
      title = sprintf("Dynamique ligands/récepteurs : %s -> %s", sender_node, receiver_node),
      subtitle = sprintf(
        "%s — %d bin(s) ; %d valeur(s) NA (bins sans cellules de la population).",
        caption, nrow(sel), n_na
      )
    )
}

#' Heatmap paires × bins de la composition des populations (V1.x-B)
#'
#' Consommatrice pure : fraction de la population choisie (sender ou
#' receiver) présente dans chaque bin de pseudo-temps, par paire. En mode
#' par lignée, les lignes portent la lignée (jamais de collapse).
#'
#' @param context Contexte trajectoire.
#' @param population "receiver" (défaut) ou "sender".
#' @return ggplot.
#' @export
plot_communication_trajectory_heatmap <- function(context, population = c("receiver", "sender")) {
  population <- match.arg(population)
  if (!is.list(context) || !identical(context$type %||% NULL, "ccc_trajectory_context")) {
    .traj_stop("plot_communication_trajectory_heatmap() : contexte trajectoire requis.")
  }
  caption <- .traj_caption(context)
  pb <- context$pair_bin_table
  pb$pair_label <- paste(pb$sender_node, pb$receiver_node, sep = " -> ")
  if (!is.null(pb$lineage) && any(!is.na(pb$lineage))) {
    pb$pair_label <- ifelse(is.na(pb$lineage), pb$pair_label,
                            paste0(pb$pair_label, " [", pb$lineage, "]"))
  }
  col <- if (identical(population, "receiver")) "frac_receiver_of_population" else "frac_sender_of_population"
  if (nrow(pb) == 0L) {
    return(.traj_stub_plot("Aucune combinaison paire/lignée calculable.",
                           "Composition des populations par bin", caption))
  }
  ggplot2::ggplot(pb, ggplot2::aes(x = .data$bin, y = .data$pair_label, fill = .data[[col]])) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::scale_fill_gradient(low = "#F7F4F0", high = "#2166AC", na.value = "grey90",
                                 name = "Fraction de la population") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Bin de pseudo-temps", y = NULL,
      title = sprintf("Présence des populations %s le long du pseudo-temps", population),
      subtitle = caption
    )
}

#' Export CSV du contexte trajectoire (traçabilité embarquée) (V1.x-B)
#'
#' @param context Contexte trajectoire.
#' @return data.frame : pair_bin_table + colonnes analysis_id / parent /
#'   timestamp / n_bins / mode lignées.
#' @export
build_communication_trajectory_export <- function(context) {
  if (!is.list(context) || !identical(context$type %||% NULL, "ccc_trajectory_context")) {
    .traj_stop("build_communication_trajectory_export() : contexte trajectoire requis.")
  }
  n <- nrow(context$pair_bin_table %||% data.frame())
  data.frame(
    context$pair_bin_table,
    analysis_id = rep(context$analysis_id, n),
    parent_analysis_id = rep(context$parent_analysis_id, n),
    timestamp_utc = rep(context$timestamp_utc, n),
    n_bins_effective = rep(context$params$n_bins_effective, n),
    lineage_mode = rep(context$params$lineage_mode, n),
    stringsAsFactors = FALSE
  )
}

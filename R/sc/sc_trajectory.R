# =============================================================================
# R/sc/sc_trajectory.R — Trajectory inference & plotting (extracted from
# helpers_sc.R, Block 7 refactor)
# =============================================================================
# Pure functions: no Shiny reactivity. Called by:
#   - modules/sc/mod_sc_trajectory.R (UI module)
#   - modules/sc/sc_report_template.Rmd (report rendering)
#
# Depends on: RANN, igraph, ggplot2, viridis, scales, slingshot (optional),
#             sc_discrete_scale/expression_continuous_scale from R/palettes.R
# Sourced in app.R AFTER R/palettes.R, BEFORE modules/sc/*.R
# =============================================================================

#' Calculate exploratory graph-based pseudotime
#'
#' Builds a k-nearest-neighbour graph using RANN::nn2(), weights edges by
#' Euclidean distance in the selected computation space, and computes
#' shortest-path pseudotime from a validated root cell.
#'
#' This is an exploratory graph-based ordering. It is NOT Slingshot,
#' Monocle, diffusion pseudotime, or a branching lineage-inference method.
#'
#' @param embeddings Numeric matrix. Rows are cells, columns are dimensions.
#'   Should carry attr(embeddings, "reduction") set to the reduction name
#'   (e.g. "pca") for provenance tracking downstream.
#' @param k Integer. Number of nearest neighbours (default 15).
#' @param root_cells Optional integer vector of root cell indices (1-based).
#'   If NULL, root is selected automatically via double-sweep BFS
#'   (diameter-endpoint approximation).
#' @param root_method Character. "diameter" or "manual". Informational only;
#'   actual behavior is driven by root_cells being NULL or not.
#'
#' @return A list with: pseudotime (named numeric vector, min-max normalized
#'   to [0,1], NA outside root component), graph (igraph object), root_cell,
#'   root_cells, component_membership, in_root_component (logical vector),
#'   root_component_size, n_cells, n_edges, k, root_method,
#'   computation_reduction, weighted, normalization, method.
#' @export
calculate_pseudotime <- function(
    embeddings,
    k = 15,
    root_cells = NULL,
    root_method = c("diameter", "manual")
) {
  root_method <- match.arg(root_method)

  if (!requireNamespace("RANN", quietly = TRUE)) {
    stop(errorCondition("Package 'RANN' is required for graph-based pseudotime.", class = "sc_trajectory_error"))
  }
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop(errorCondition("Package 'igraph' is required for graph-based pseudotime.", class = "sc_trajectory_error"))
  }

  embeddings <- as.matrix(embeddings)

  if (!is.numeric(embeddings) || nrow(embeddings) < 3L || ncol(embeddings) < 1L) {
    stop(errorCondition("embeddings must be a numeric matrix with at least 3 cells.", class = "sc_trajectory_error"))
  }
  if (any(!is.finite(embeddings))) {
    stop(errorCondition("embeddings contain NA, NaN, or infinite values.", class = "sc_trajectory_error"))
  }

  n_cells <- nrow(embeddings)
  k <- as.integer(k)
  if (length(k) != 1L || is.na(k) || k < 1L) {
    stop(errorCondition("'k' must be a positive integer.", class = "sc_trajectory_error"))
  }
  k <- min(k, n_cells - 1L)

  if (!is.null(root_cells)) {
    root_cells <- unique(as.integer(root_cells))
    if (anyNA(root_cells) || any(root_cells < 1L) || any(root_cells > n_cells)) {
      stop(errorCondition(paste0("Invalid root cell index. Valid indices are between 1 and ", n_cells, "."), class = "sc_trajectory_error"))
    }
  }

  # RANN returns the query cell itself as the first neighbour -> drop column 1.
  nn <- RANN::nn2(data = embeddings, query = embeddings, k = k + 1L)
  neighbour_idx  <- nn$nn.idx[, -1L, drop = FALSE]
  neighbour_dist <- nn$nn.dists[, -1L, drop = FALSE]

  from   <- rep(seq_len(n_cells), each = k)
  to     <- as.vector(t(neighbour_idx))
  weight <- as.vector(t(neighbour_dist))

  edge_df <- data.frame(from = from, to = to, weight = weight, stringsAsFactors = FALSE)
  edge_df <- edge_df[edge_df$from != edge_df$to, , drop = FALSE]

  edge_key <- paste(pmin(edge_df$from, edge_df$to), pmax(edge_df$from, edge_df$to), sep = "_")
  edge_df  <- edge_df[!duplicated(edge_key), , drop = FALSE]

  if (nrow(edge_df) < 1L) {
    stop(errorCondition("Unable to construct a valid kNN graph.", class = "sc_trajectory_error"))
  }

  graph <- igraph::graph_from_data_frame(
    d = edge_df,
    directed = FALSE,
    vertices = data.frame(name = as.character(seq_len(n_cells)))
  )
  igraph::E(graph)$weight <- pmax(as.numeric(edge_df$weight), .Machine$double.eps)

  components <- igraph::components(graph)
  automatic_root <- is.null(root_cells)

  if (automatic_root) {
    largest_component <- which.max(components$csize)
    component_cells <- which(components$membership == largest_component)

    # Double-sweep BFS: diameter-endpoint approximation.
    start_cell <- component_cells[[1L]]
    d_start <- igraph::distances(graph, v = start_cell, to = component_cells,
                                  weights = igraph::E(graph)$weight)
    far_cell_1 <- component_cells[[which.max(d_start)]]
    d_far <- igraph::distances(graph, v = far_cell_1, to = component_cells,
                                weights = igraph::E(graph)$weight)
    root_cell <- component_cells[[which.max(d_far)]]
    root_cells <- root_cell
  } else {
    root_cell <- root_cells[[1L]]
    root_component  <- components$membership[[root_cell]]
    component_cells <- which(components$membership == root_component)
  }

  root_component <- components$membership[[root_cell]]
  same_component <- components$membership == root_component

  distances <- igraph::distances(graph, v = root_cell, to = seq_len(n_cells),
                                  weights = igraph::E(graph)$weight)
  pseudotime <- as.numeric(distances)
  pseudotime[!same_component] <- NA_real_  # undefined outside root component

  finite_pt <- pseudotime[is.finite(pseudotime)]
  if (length(finite_pt) == 0L) {
    stop(errorCondition("No finite pseudotime values were obtained.", class = "sc_trajectory_error"))
  }

  pt_min <- min(finite_pt)
  pt_max <- max(finite_pt)
  if (pt_max > pt_min) {
    pseudotime <- (pseudotime - pt_min) / (pt_max - pt_min)
  } else {
    pseudotime[] <- 0
  }

  names(pseudotime) <- rownames(embeddings) %||% as.character(seq_len(n_cells))

  list(
    pseudotime             = pseudotime,
    graph                  = graph,
    root_cell              = root_cell,
    root_cells             = root_cells,
    component_membership   = components$membership,
    in_root_component      = same_component,
    root_component_size    = sum(same_component),
    n_cells                = n_cells,
    n_edges                = igraph::ecount(graph),
    k                      = k,
    root_method            = if (automatic_root) "diameter" else "manual",
    computation_reduction  = attr(embeddings, "reduction") %||% NA_character_,
    weighted               = TRUE,
    normalization          = "min-max",
    method                 = "Exploratory weighted kNN graph pseudotime"
  )
}

calculate_slingshot_pseudotime <- function(
    embeddings,
    cluster_labels,
    start_cluster = NULL,
    end_clusters = NULL,
    reduction = NA_character_
) {
  if (!requireNamespace("slingshot", quietly = TRUE)) {
    stop(errorCondition("Package 'Slingshot' is required for Slingshot trajectory inference.", class = "sc_trajectory_error"))
  }

  embeddings <- as.matrix(embeddings)

  if (!is.numeric(embeddings) ||
      nrow(embeddings) < 3L ||
      ncol(embeddings) < 2L) {
    stop(errorCondition("embeddings must be a numeric matrix with at least 3 cells and 2 dimensions.", class = "sc_trajectory_error"))
  }

  if (any(!is.finite(embeddings))) {
    stop(errorCondition("embeddings contain NA, NaN, or infinite values.", class = "sc_trajectory_error"))
  }

  cluster_labels <- as.character(cluster_labels)

  if (length(cluster_labels) != nrow(embeddings)) {
    stop(errorCondition("cluster_labels must have one value per cell.", class = "sc_trajectory_error"))
  }

  if (anyNA(cluster_labels) || any(!nzchar(cluster_labels))) {
    stop(errorCondition("cluster_labels contain missing or empty values.", class = "sc_trajectory_error"))
  }

  cluster_labels <- factor(cluster_labels)

  slingshot_args <- list(
    data = embeddings,
    clusterLabels = cluster_labels
  )

  if (!is.null(start_cluster)) {
    slingshot_args$start.clus <- as.character(start_cluster)
  }

  if (!is.null(end_clusters)) {
    slingshot_args$end.clus <- as.character(end_clusters)
  }

  sce <- do.call(
    slingshot::slingshot,
    slingshot_args
  )

  # slingPseudotime() returns a cells x lineages matrix; some versions may
  # return a bare vector when exactly one lineage is present -> coerce safely.
  pt_matrix <- tryCatch(
    slingshot::slingPseudotime(sce),
    error = function(e) stop("Slingshot pseudotime extraction failed: ", e$message, call. = FALSE)
  )
  if (!is.matrix(pt_matrix)) {
    pt_matrix <- matrix(pt_matrix, ncol = 1L)
  } else {
    pt_matrix <- as.matrix(pt_matrix)
  }
  curve_weights <- as.matrix(slingshot::slingCurveWeights(sce))
  lineages <- slingshot::slingLineages(sce)
  curves <- slingshot::slingCurves(sce)

  if (nrow(pt_matrix) != nrow(embeddings)) {
    stop(errorCondition("Slingshot returned an unexpected pseudotime dimension.", class = "sc_trajectory_error"))
  }

  lineage_names <- colnames(pt_matrix)

  if (is.null(lineage_names) || !length(lineage_names)) {
    lineage_names <- paste0("Lineage_", seq_len(ncol(pt_matrix)))
    colnames(pt_matrix) <- lineage_names
  }

  # Use the first Slingshot lineage as the default exported pseudotime.
  # This is a COMPATIBILITY DEFAULT ONLY -- it is not a biologically
  # validated lineage selection. The full multi-lineage matrix is preserved
  # below in `pseudotime_matrix` so every lineage remains available for
  # downstream use and exports.
  pseudotime <- if (ncol(pt_matrix) > 0L) {
    pt_matrix[, 1L]
  } else {
    rep(NA_real_, nrow(embeddings))
  }

  names(pseudotime) <- rownames(embeddings) %||%
    as.character(seq_len(nrow(embeddings)))

  list(
    pseudotime = pseudotime,
    pseudotime_matrix = pt_matrix,
    curve_weights = curve_weights,
    lineages = lineages,
    curves = curves,
    computation_reduction = reduction %||% NA_character_,
    method = "Slingshot lineage pseudotime",
    root_method = if (is.null(start_cluster)) "slingshot_default" else "start_cluster",
    root_cluster = start_cluster %||% NA_character_,
    n_cells = nrow(embeddings)
  )
}

#' Plot cells coloured by exploratory pseudotime
#'
#' @param embeddings Numeric matrix/data.frame of DISPLAY coordinates
#'   (first two columns used). May differ from the computation reduction.
#' @param pseudotime Numeric vector, same length/order as embeddings rows.
#' @param graph Optional igraph object built on the same cell ordering
#'   (for edge overlay only; NULL skips edges).
#' @param root_cell Integer index of the root cell to highlight.
#' @param show_edges Logical. Draw a subsample of graph edges (default FALSE).
#' @param edge_subsample Integer. Max number of edges drawn if show_edges=TRUE.
#'
#' @return A ggplot object.
#' @export
plot_trajectory <- function(
    embeddings,
    pseudotime,
    graph = NULL,
    root_cell = NULL,
    show_edges = FALSE,
    edge_subsample = 5000L,
    palette = "default",
    manual_gradient = NULL,
    tr = NULL
) {
  tr <- tr %||% function(x) x
  embeddings <- as.data.frame(embeddings)
  if (ncol(embeddings) < 2L) {
    stop(errorCondition("At least two display dimensions are required.", class = "sc_trajectory_error"))
  }
  colnames(embeddings)[1:2] <- c("dim1", "dim2")

  plot_data <- data.frame(
    dim1 = embeddings$dim1,
    dim2 = embeddings$dim2,
    pseudotime = as.numeric(pseudotime),
    cell_index = seq_len(nrow(embeddings))
  )

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = dim1, y = dim2, colour = pseudotime)) +
    ggplot2::geom_point(size = 0.7, alpha = 0.75, na.rm = FALSE) +
    expression_continuous_scale(palette, "color", manual_gradient, base_option = "plasma", na.value = "grey80") +
    ggplot2::labs(colour = tr("Pseudotemps\n(sans unite)")) +
    ggplot2::theme_classic() +
    ggplot2::labs(x = colnames(embeddings)[1], y = colnames(embeddings)[2],
                  subtitle = tr("Pseudotemps exploratoire (graphe kNN pondere)"))

  if (isTRUE(show_edges) && !is.null(graph) && igraph::ecount(graph) > 0L) {
    edge_tbl <- igraph::as_data_frame(graph, what = "edges")
    if (nrow(edge_tbl) > edge_subsample) {
      set.seed(1L)
      edge_tbl <- edge_tbl[sample.int(nrow(edge_tbl), edge_subsample), , drop = FALSE]
    }
    edge_tbl$from <- as.integer(edge_tbl$from)
    edge_tbl$to   <- as.integer(edge_tbl$to)

    edge_data <- data.frame(
      x = embeddings$dim1[edge_tbl$from], y = embeddings$dim2[edge_tbl$from],
      xend = embeddings$dim1[edge_tbl$to], yend = embeddings$dim2[edge_tbl$to]
    )

    p <- p + ggplot2::geom_segment(
      data = edge_data,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      inherit.aes = FALSE, linewidth = 0.15, alpha = 0.15, colour = "grey35"
    )
  }

  if (!is.null(root_cell) && length(root_cell) == 1L &&
      root_cell >= 1L && root_cell <= nrow(plot_data)) {
    p <- p + ggplot2::geom_point(
      data = plot_data[root_cell, , drop = FALSE],
      ggplot2::aes(x = dim1, y = dim2), inherit.aes = FALSE,
      shape = 8, size = 4, stroke = 1.2, colour = "black"
    )
  }

  p
}

plot_slingshot_trajectory <- function(
    embeddings,
    pseudotime,
    curves = NULL,
    palette = "default",
    manual_gradient = NULL,
    tr = NULL
) {
  tr <- tr %||% function(x) x
  embeddings <- as.data.frame(embeddings)

  if (ncol(embeddings) < 2L) {
    stop(errorCondition("At least two display dimensions are required.", class = "sc_trajectory_error"))
  }

  if (length(pseudotime) != nrow(embeddings)) {
    stop(errorCondition("pseudotime must have the same length as embeddings rows.", class = "sc_trajectory_error"))
  }

  colnames(embeddings)[1:2] <- c("dim1", "dim2")

  plot_data <- data.frame(
    dim1 = embeddings$dim1,
    dim2 = embeddings$dim2,
    pseudotime = as.numeric(pseudotime),
    cell_index = seq_len(nrow(embeddings))
  )

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = dim1, y = dim2, colour = pseudotime)
  ) +
    ggplot2::geom_point(
      size = 0.7,
      alpha = 0.75,
      na.rm = FALSE
    ) +
    expression_continuous_scale(palette, "color", manual_gradient, base_option = "plasma", na.value = "grey80") +
    ggplot2::labs(colour = "Pseudotemps Slingshot") +
    ggplot2::theme_classic() +
    ggplot2::labs(
      x = colnames(embeddings)[1],
      y = colnames(embeddings)[2],
      subtitle = "Slingshot — pseudotemps de la première lignée"
    )

  # Guarded curve overlay: draw lineages ONLY if every curve carries a
  # numeric 's' coordinate matrix with at least two columns matching the
  # number of display rows. Any structural surprise -> skip silently
  # rather than fabricate geometry. Curves live in the COMPUTATION space,
  # so an overlay is only meaningful when that equals the DISPLAY space.
  if (!is.null(curves) && is.list(curves) && length(curves) > 0L) {
    ok_overlay <- TRUE
    curve_lines <- list()
    for (i in seq_along(curves)) {
      s_mat <- tryCatch(curves[[i]]$s, error = function(e) NULL)
      if (is.null(s_mat) || !is.numeric(as.matrix(s_mat)) ||
          nrow(as.matrix(s_mat)) < 2L || ncol(as.matrix(s_mat)) < 2L) {
        ok_overlay <- FALSE; break
      }
      s_mat <- as.matrix(s_mat)
      curve_lines[[i]] <- data.frame(
        lineage  = paste0("Lineage_", i),
        dim1 = s_mat[, 1L],
        dim2 = s_mat[, 2L]
      )
    }
    if (ok_overlay && length(curve_lines) > 0L) {
      line_df <- do.call(rbind, curve_lines)
      p <- p + ggplot2::geom_path(
        data = line_df,
        ggplot2::aes(x = dim1, y = dim2, group = lineage),
        inherit.aes = FALSE,
        linewidth = 0.8, colour = "black", alpha = 0.6
      )
    }
  }

  p
}

plot_pseudotime_distribution <- function(seurat_obj, palette = "default", manual_colors = NULL, tr = NULL) {
  tr <- tr %||% function(x) x

  if (!"pseudotime" %in% colnames(seurat_obj@meta.data))

    stop(errorCondition(tr("Pseudotemps non calculé — lancez d'abord 'Calculer Trajectoire'."), class = "sc_trajectory_error"))



  df <- data.frame(

    pseudotime = seurat_obj@meta.data$pseudotime,

    cluster    = as.character(seurat_obj@meta.data$seurat_clusters)

  )

  df <- df[!is.na(df$pseudotime), ]   # cells outside the root component (disconnected graph)

  if (nrow(df) == 0) stop(errorCondition("Distribution non disponible (pseudotemps NA pour toutes les cellules).", class = "sc_trajectory_error"))



  pal_scale <- sc_discrete_scale(palette, manual_colors, "fill")



  p <- ggplot(df, aes(x = pseudotime, fill = cluster)) +

    geom_density(alpha = 0.6)

  p <- if (is.null(pal_scale)) p + scale_fill_viridis_d(option = "turbo") else suppressWarnings(p + pal_scale)

  p +

    labs(title = tr("Distribution du Pseudotemps par Cluster"),

         x = tr("Pseudotemps"), y = tr("Densité"), fill = tr("Cluster")) +

    theme_minimal() +

    theme(plot.title = element_text(face = "bold", size = 13))

}

plot_genes_vs_pseudotime <- function(seurat_obj, genes, smooth_method = "loess",
                                     palette = "default", manual_colors = NULL, tr = NULL) {
  tr <- tr %||% function(x) x

  if (!"pseudotime" %in% colnames(seurat_obj@meta.data))

    stop(errorCondition(tr("Pseudotemps non calculé — lancez d'abord 'Calculer Trajectoire'."), class = "sc_trajectory_error"))



  valid_genes <- intersect(genes, rownames(seurat_obj))

  valid_genes <- head(valid_genes, 8)

  if (length(valid_genes) == 0) stop(errorCondition("Aucun gène valide sélectionné", class = "sc_trajectory_error"))



  expr_df   <- FetchData(seurat_obj, vars = c("pseudotime", valid_genes))

  expr_long <- tidyr::pivot_longer(expr_df, cols = -pseudotime,

                                   names_to = "gene", values_to = "expression")



  pal_scale <- sc_discrete_scale(palette, manual_colors, "color")



  p <- ggplot(expr_long, aes(x = pseudotime, y = expression, color = gene)) +

    geom_point(alpha = 0.3, size = 0.6) +

    geom_smooth(method = smooth_method, se = TRUE, linewidth = 0.9, na.rm = TRUE) +

    facet_wrap(~gene, scales = "free_y", ncol = 2)

  p <- if (is.null(pal_scale)) p + scale_color_viridis_d(option = "turbo") else suppressWarnings(p + pal_scale)

  p + labs(title = tr("Expression génique le long du Pseudotemps"),

         x = tr("Pseudotemps"), y = tr("Expression Normalisée")) +

    theme_minimal() +

    theme(legend.position = "none",

          plot.title = element_text(face = "bold", size = 13),

          strip.text = element_text(face = "bold"))

}

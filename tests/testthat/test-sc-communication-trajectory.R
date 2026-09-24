# =============================================================================
# test-sc-communication-trajectory.R — V1.x-B : comportement du contexte
# trajectoire (bins exacts, expression, lignées, erreurs, vues, export)
# =============================================================================

source_project_file("R/sc/sc_communication_trajectory.R")

test_that("bins de pseudo-temps : effectifs et expression exacts", {
  ctx <- build_communication_trajectory_context(
    .ccx_result(), .ccx_expr(), .ccx_identities(), .ccx_pseudotime(),
    identity_column = "cell_type", n_bins = 5
  )
  v <- ctx$pair_bin_table
  # breaks = quantiles du pseudo-temps ; bin 1 = [0, 0.0733] -> A1..A3
  ab1 <- v[v$sender_node == "A" & v$receiver_node == "B" & v$bin == 1, ]
  expect_equal(ab1$n_sender_cells, 3L)
  expect_equal(ab1$mean_ligand_expression_senders, 4, tolerance = 1e-10)
  ab2 <- v[v$sender_node == "A" & v$receiver_node == "B" & v$bin == 2, ]
  expect_equal(ab2$n_sender_cells, 1L)
  # bin final : B2..B4 (R1 = 5) ; B1 (0.9) est au bin 4
  b5 <- v[v$receiver_node == "B" & v$sender_node == "A" & v$bin == 5, ]
  expect_equal(b5$mean_receptor_expression_receivers, 5, tolerance = 1e-10)
  expect_equal(v$bin_mid[v$sender_node == "A" & v$bin == 1][1],
               (0 + 0.0733333333333333) / 2, tolerance = 1e-6)
  expect_identical(ctx$qc$n_bins_effective, 5L)
  expect_identical(ctx$qc$n_cells_pseudotime_excluded, 0L)
})

test_that("bins sans cellules de la population : NA, jamais 0", {
  ctx <- build_communication_trajectory_context(
    .ccx_result(), .ccx_expr(), .ccx_identities(), .ccx_pseudotime(),
    identity_column = "cell_type", n_bins = 5
  )
  v <- ctx$pair_bin_table
  # A->B : le bin central [0.413, 0.487] ne contient ni cellule A ni cellule B
  mid <- v[v$sender_node == "A" & v$receiver_node == "B" & v$bin == 3, ]
  expect_equal(nrow(mid), 1L)
  expect_equal(mid$n_sender_cells, 0L)
  expect_equal(mid$n_receiver_cells, 0L)
  expect_true(is.na(mid$mean_ligand_expression_senders))
  expect_true(is.na(mid$mean_receptor_expression_receivers))
})

test_that("paires non calculables comptées et listées (mode global)", {
  ctx <- build_communication_trajectory_context(
    .ccx_result(), .ccx_expr(), .ccx_identities(), .ccx_pseudotime(),
    identity_column = "cell_type", n_bins = 3
  )
  expect_identical(ctx$qc$n_pairs_total, 5L)
  expect_identical(ctx$qc$n_combinations_total, 5L)
  expect_identical(ctx$qc$n_combinations_mapped, 4L)
  expect_identical(ctx$qc$n_combinations_unmapped, 1L)
  expect_true(any(grepl("D -> A", ctx$qc$unmapped_combinations, fixed = TRUE)))
})

test_that("mode par lignée : périmètres séparés, jamais collapsés", {
  ctx <- build_communication_trajectory_context(
    .ccx_result(), .ccx_expr(), .ccx_identities(), .ccx_pseudotime(),
    lineage = .ccx_lineage(), identity_column = "cell_type", n_bins = 3
  )
  expect_setequal(unique(ctx$pair_bin_table$lineage), c("L1", "L2"))
  # L1 = A + C1,C2 : A->C limite aux cellules C1,C2
  ac <- ctx$pair_bin_table[ctx$pair_bin_table$sender_node == "A" &
                             ctx$pair_bin_table$lineage == "L1", ]
  expect_true(all(ac$n_receiver_cells <= 2L))
  expect_false(any(ctx$pair_bin_table$sender_node == "A" &
                     ctx$pair_bin_table$lineage == "L2"))
  # ligne "L1" seule : L2 ne contient pas A
  s2 <- ctx$pair_summary[ctx$pair_summary$lineage == "L2", ]
  expect_false(any(s2$sender_node == "A"))
})

test_that("gènes manquants comptés par paire, jamais imputés", {
  tab2 <- .ccx_cellchat_tab()
  tab2$ligand[1] <- "ZZZ"  # gène absent de la matrice
  parsed <- parse_cellchat_import(tab2, source_file = "x.csv")
  harm <- harmonize_communication_identities(parsed$table, unique(.ccx_identities()),
                                             "cell_type", context = "communication import")
  qcr <- communication_import_qc(harm$table)
  r2 <- finalize_communication_result(
    canonical_table = qcr$table, source_method = "cellchat",
    source_files = list(table = "x.csv"), identity_column = "cell_type",
    identity_mapping = harm$mapping, identity_summary = harm$summary,
    column_mapping = parsed$column_mapping, qc = qcr$counts,
    n_input_rows = parsed$n_input_rows,
    seurat_obj = matrix(0, 2, 2, dimnames = list(c("g1", "g2"), c("c1", "c2")))
  )
  ctx <- build_communication_trajectory_context(
    r2, .ccx_expr(), .ccx_identities(), .ccx_pseudotime(),
    identity_column = "cell_type", n_bins = 3
  )
  ab <- ctx$pair_summary[ctx$pair_summary$sender_node == "A" &
                           ctx$pair_summary$receiver_node == "B", ]
  expect_identical(ab$n_ligand_genes_missing, 1L)
  expect_identical(ab$ligand_genes, "")  # aucun gène présent -> chaîne vide, jamais imputé
  expect_true(any(grepl("ZZZ", ctx$warnings)))
  expect_true(is.na(ctx$pair_bin_table$mean_ligand_expression_senders[
    ctx$pair_bin_table$sender_node == "A" &
      ctx$pair_bin_table$receiver_node == "B" & ctx$pair_bin_table$bin == 1][1]))
})

test_that("bins réduits par quantiles dupliqués : avertissement tracé", {
  pt <- .ccx_pseudotime()
  pt["A2"] <- 0  # ties au minimum -> quantile duplique possible
  ctx <- build_communication_trajectory_context(
    .ccx_result(), .ccx_expr(), .ccx_identities(), pt,
    identity_column = "cell_type", n_bins = 20
  )
  expect_true(ctx$qc$n_bins_effective < 20L)
  expect_true(any(grepl("réduits", ctx$warnings)))
})

test_that("cellules sans pseudo-temps : exclues et comptabilisées", {
  pt <- .ccx_pseudotime()
  pt["A1"] <- NA
  ctx <- build_communication_trajectory_context(
    .ccx_result(), .ccx_expr(), .ccx_identities(), pt,
    identity_column = "cell_type", n_bins = 3
  )
  expect_identical(ctx$qc$n_cells_pseudotime_excluded, 1L)
  expect_identical(ctx$qc$n_cells_matched, 11L)
  expect_true(any(grepl("exclue", ctx$warnings)))
})

test_that("table filtrée des vues honorée", {
  r <- .ccx_result()
  fs <- communication_apply_filters(r, list(pathways = "P1"))
  ctx <- build_communication_trajectory_context(
    r, .ccx_expr(), .ccx_identities(), .ccx_pseudotime(),
    identity_column = "cell_type", n_bins = 3, filtered_table = fs$table
  )
  expect_identical(ctx$qc$n_pairs_total, 2L)
  expect_false(any(ctx$pair_bin_table$sender_node == "A" &
                     ctx$pair_bin_table$receiver_node == "C"))
})

test_that("vues consommatrices pures + messages explicites", {
  ctx <- build_communication_trajectory_context(
    .ccx_result(), .ccx_expr(), .ccx_identities(), .ccx_pseudotime(),
    identity_column = "cell_type", n_bins = 5
  )
  p1 <- plot_communication_trajectory_curves(ctx, "A", "B")
  expect_s3_class(p1, "ggplot")
  expect_true(grepl("PAS un temps réel", p1$labels$subtitle %||% ""))
  p2 <- plot_communication_trajectory_heatmap(ctx)
  expect_s3_class(p2, "ggplot")
  p3 <- plot_communication_trajectory_curves(ctx, "Z", "W")
  expect_true(grepl("absente", p3$labels$subtitle %||% ""))
})

test_that("export tracé : traçabilité par ligne", {
  ctx <- build_communication_trajectory_context(
    .ccx_result(), .ccx_expr(), .ccx_identities(), .ccx_pseudotime(),
    identity_column = "cell_type", n_bins = 4
  )
  ex <- build_communication_trajectory_export(ctx)
  expect_equal(nrow(ex), nrow(ctx$pair_bin_table))
  expect_true(all(ex$analysis_id == "sc-communication-trajectory"))
  expect_true(all(ex$parent_analysis_id == "sc-communication-import"))
  expect_true(all(ex$n_bins_effective == 4L))
  expect_true(all(ex$lineage_mode == "global"))
})

test_that("erreurs d'entrée : messages français et états structurés", {
  r <- .ccx_result()
  expect_error(
    build_communication_trajectory_context(r, .ccx_expr(), .ccx_identities(),
                                           .ccx_pseudotime(), n_bins = 1),
    "n_bins", class = "communication_context_error"
  )
  # matrice sans colnames -> invalid_input (validation d'entrée d'abord)
  e <- tryCatch(
    build_communication_trajectory_context(r, matrix(0, 2, 2), .ccx_identities(),
                                           .ccx_pseudotime()),
    error = function(err) err
  )
  expect_s3_class(e, "communication_context_error")
  expect_identical(e$state, "invalid_input")
  # cellules sans recouvrement -> invalid_identity_mapping
  orphan <- matrix(0, 2, 2, dimnames = list(c("g1", "g2"), c("x1", "x2")))
  e2 <- tryCatch(
    build_communication_trajectory_context(r, orphan, .ccx_identities(),
                                           .ccx_pseudotime()),
    error = function(err) err
  )
  expect_s3_class(e2, "communication_context_error")
  expect_identical(e2$state, "invalid_identity_mapping")
})

test_that("trajectory inputs honor the active cell scope with a NULL fallback", {
  src <- paste(readLines(
    file.path(ts_project_root(), "modules/sc/mod_sc_communication_trajectory.R"),
    warn = FALSE
  ), collapse = "\n")
  expect_true(grepl("meta <- meta[rownames(meta) %in%",
                    src, fixed = TRUE))
  expect_true(grepl("pt <- setNames(as.numeric(meta[[col]]), rownames(meta))",
                    src, fixed = TRUE))
  expect_true(grepl("expr <- expr[, colnames(expr) %in%",
                    src, fixed = TRUE))
  positions <- gregexpr("if (!is.null(comm_state$context_cells))", src,
                        fixed = TRUE)[[1L]]
  expect_identical(sum(positions > 0L), 4L)
})

test_that("communication_fetch_expression_matrix : extraction bornée aux gènes", {
  skip_if_not_installed("SeuratObject")
  skip_if_not_installed("Seurat")
  obj <- SeuratObject::CreateSeuratObject(
    counts = Matrix::Matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3,
                            dimnames = list(c("L1", "R1", "G1"),
                                            c("A1", "A2", "B1"))),
    assay = "RNA"
  )
  obj <- Seurat::NormalizeData(obj, verbose = FALSE)
  m <- communication_fetch_expression_matrix(obj, genes = c("L1", "R1", "ZZZ"))
  expect_setequal(rownames(m), c("L1", "R1"))  # ZZZ absent, silencieusement absent du résultat
  expect_equal(ncol(m), 3L)
  expect_error(
    communication_fetch_expression_matrix(obj, genes = "L1", assay = "ABSENT"),
    class = "communication_context_error"
  )
})

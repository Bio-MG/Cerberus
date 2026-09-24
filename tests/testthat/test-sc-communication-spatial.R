# =============================================================================
# test-sc-communication-spatial.R — V1.x-A : comportement du contexte spatial
# =============================================================================
# Distances exactes sur géométrie déterministe, fractions à portée, comptage
# des paires non cartographiables, avertissements, permutation déterministe,
# vues consommatrices pures, export tracé.
# =============================================================================

source_project_file("R/sc/sc_communication_spatial.R")

test_that("distances de centroïdes exactes sur la géométrie de fixture", {
  ctx <- build_communication_spatial_context(
    .ccx_result(), .ccx_coords(), .ccx_identities(),
    identity_column = "cell_type", coordinate_source = "spatial_object"
  )
  pt <- ctx$pair_table
  ab <- pt[pt$sender_node == "A" & pt$receiver_node == "B", ]
  ac <- pt[pt$sender_node == "A" & pt$receiver_node == "C", ]
  expect_equal(ab$centroid_distance, 10, tolerance = 1e-10)
  expect_equal(ac$centroid_distance, sqrt(0.25 + 4), tolerance = 1e-10)
  expect_equal(ab$n_sender_cells, 4L)
  expect_equal(ab$n_receiver_cells, 4L)
  # scores importés joints, jamais recalculés
  expect_equal(ab$mean_imported_score, 0.4, tolerance = 1e-12)
  expect_equal(ab$n_interactions, 1L)
})

test_that("fractions à portée : rayon explicite, NA sans rayon", {
  ctx <- build_communication_spatial_context(
    .ccx_result(), .ccx_coords(), .ccx_identities(),
    identity_column = "cell_type", coordinate_source = "spatial_object",
    radius = 3
  )
  pt <- ctx$pair_table
  ac <- pt[pt$sender_node == "A" & pt$receiver_node == "C", ]
  ab <- pt[pt$sender_node == "A" & pt$receiver_node == "B", ]
  expect_equal(ac$frac_receiver_within_radius, 1)  # C collé à A
  expect_equal(ab$frac_receiver_within_radius, 0)  # B loin de A
  ctx_nr <- build_communication_spatial_context(
    .ccx_result(), .ccx_coords(), .ccx_identities(),
    identity_column = "cell_type", coordinate_source = "spatial_object"
  )
  expect_true(all(is.na(ctx_nr$pair_table$frac_receiver_within_radius)))
})

test_that("paires non cartographiables comptées et listées, jamais imputées", {
  ctx <- build_communication_spatial_context(
    .ccx_result(), .ccx_coords(), .ccx_identities(),
    identity_column = "cell_type", coordinate_source = "spatial_object"
  )
  expect_identical(ctx$qc$n_pairs_total, 5L)
  expect_identical(ctx$qc$n_pairs_mapped, 4L)
  expect_identical(ctx$qc$n_pairs_unmapped, 1L)
  expect_true(any(grepl("D -> A", ctx$qc$unmapped_pairs, fixed = TRUE)))
  expect_true(any(grepl("non cartographiable", ctx$warnings)))
  expect_false(any(ctx$pair_table$sender_node == "D"))
})

test_that("source réduction : avertissement de projection enregistré", {
  ctx <- build_communication_spatial_context(
    .ccx_result(), .ccx_coords(), .ccx_identities(),
    identity_column = "cell_type", coordinate_source = "reduction"
  )
  expect_identical(ctx$coordinate_source, "reduction")
  expect_true(any(grepl("PROJECTION", ctx$warnings)))
})

test_that("désaccord de colonne d'identités : avertissement tracé", {
  ctx <- build_communication_spatial_context(
    .ccx_result(), .ccx_coords(), .ccx_identities(),
    identity_column = "autre_colonne", coordinate_source = "spatial_object"
  )
  expect_true(any(grepl("autre_colonne", ctx$warnings)))
})

test_that("permutation : déterministe, p dans [0,1], hypothèse enregistrée", {
  ctx <- build_communication_spatial_context(
    .ccx_result(), .ccx_coords(), .ccx_identities(),
    identity_column = "cell_type", coordinate_source = "spatial_object",
    radius = 3, n_permutations = 19, perm_seed = 1
  )
  pt <- ctx$pair_table
  expect_false(anyNA(pt$p_perm))
  expect_true(all(pt$p_perm >= 0 & pt$p_perm <= 1))
  expect_identical(ctx$qc$n_permutations_done, 19L)
  expect_true(any(grepl("échangeabilité", ctx$warnings)))
  expect_identical(ctx$provenance$seed, 1L)
  ctx2 <- build_communication_spatial_context(
    .ccx_result(), .ccx_coords(), .ccx_identities(),
    identity_column = "cell_type", coordinate_source = "spatial_object",
    radius = 3, n_permutations = 19, perm_seed = 1
  )
  expect_identical(pt, ctx2$pair_table)
})

test_that("table filtrée des vues honorée (aucun contournement des filtres)", {
  r <- .ccx_result()
  fs <- communication_apply_filters(r, list(pathways = "P1"))
  ctx <- build_communication_spatial_context(
    r, .ccx_coords(), .ccx_identities(),
    identity_column = "cell_type", coordinate_source = "spatial_object",
    filtered_table = fs$table
  )
  expect_identical(ctx$qc$n_pairs_total, 2L)  # A->B et C->B seulement
  expect_false(any(ctx$pair_table$sender_node == "A" &
                     ctx$pair_table$receiver_node == "C"))
})

test_that("vues consommatrices pures + messages explicites", {
  ctx <- build_communication_spatial_context(
    .ccx_result(), .ccx_coords(), .ccx_identities(),
    identity_column = "cell_type", coordinate_source = "spatial_object"
  )
  p1 <- plot_communication_spatial_edges(ctx, "A", "B",
                                         .ccx_coords(), .ccx_identities())
  expect_s3_class(p1, "ggplot")
  p2 <- plot_communication_spatial_distance_summary(ctx)
  expect_s3_class(p2, "ggplot")
  # paire inconnue -> message explicite, jamais un graphe trompeur
  p3 <- plot_communication_spatial_edges(ctx, "Z", "W",
                                         .ccx_coords(), .ccx_identities())
  expect_s3_class(p3, "ggplot")
  expect_true(grepl("absente", p3$labels$subtitle %||% ""))
  expect_true(grepl("contrainte spatiale", p1$labels$subtitle %||% ""))
})

test_that("export tracé : paramètres et traçabilité par ligne", {
  ctx <- build_communication_spatial_context(
    .ccx_result(), .ccx_coords(), .ccx_identities(),
    identity_column = "cell_type", coordinate_source = "reduction",
    radius = 3, n_permutations = 5, perm_seed = 1
  )
  ex <- build_communication_spatial_export(ctx)
  expect_equal(nrow(ex), nrow(ctx$pair_table))
  expect_true(all(ex$analysis_id == "sc-communication-spatial"))
  expect_true(all(ex$parent_analysis_id == "sc-communication-import"))
  expect_true(all(ex$coordinate_source == "reduction"))
  expect_true(all(ex$radius == 3))
  expect_true(all(ex$n_permutations == 5L))
})

test_that("spatial inputs honor the active cell scope with a NULL fallback", {
  src <- paste(readLines(
    file.path(ts_project_root(), "modules/sc/mod_sc_communication_spatial.R"),
    warn = FALSE
  ), collapse = "\n")
  expect_true(grepl("meta <- meta[rownames(meta) %in%",
                    src, fixed = TRUE))
  expect_true(grepl("coords <- coords[as.character(coords$id) %in%",
                    src, fixed = TRUE))
  expect_true(grepl("em <- em[rownames(em) %in%", src, fixed = TRUE))
  positions <- gregexpr("if (!is.null(comm_state$context_cells))", src,
                        fixed = TRUE)[[1L]]
  expect_identical(sum(positions > 0L), 3L)
})

test_that("erreurs d'entrée : messages français et états structurés", {
  r <- .ccx_result()
  expect_error(
    build_communication_spatial_context(r, .ccx_coords(), .ccx_identities(),
                                        radius = -1),
    "rayon", class = "communication_context_error"
  )
  expect_error(
    build_communication_spatial_context(r, .ccx_coords(), .ccx_identities(),
                                        n_permutations = 2000),
    "n_permutations", class = "communication_context_error"
  )
  bad <- .ccx_coords(); bad[1, 1] <- NA
  e <- tryCatch(
    build_communication_spatial_context(r, bad, .ccx_identities()),
    error = function(err) err
  )
  expect_identical(e$state, "invalid_input")
  expect_true(grepl("NA", conditionMessage(e), fixed = TRUE))
})

# =============================================================================
# test-sc-communication-views.R — Stage 12 (4D-2) : vues exploratoires des
# imports communication valides
# =============================================================================
# Route objet CellChat (extraction, PAS de calcul), filtres d'affichage
# (jamais destructifs), DotPlot/heatmap/reseau circulaire en ggplot pur,
# centralite descriptive, provenance des filtres, exports traces.
# Aucune donnee biologique reelle ; fixtures deterministes.
# =============================================================================

.comm_rank_views_result <- function() {
  tab <- .comm_liana_tab()
  tab$source <- c("CD4 T", "CD4 T", "CD4 T", "B")
  tab$target <- c("B", "B", "B", "B")
  tab$mean_rank <- c(1, 3, NA_real_, Inf)
  .comm_liana_result(tab = tab)
}

# ── Route objet CellChat : extraction sans recalcul ─────────────────────────
test_that("Fixture O1 - real net$prob shape [source, target, interaction] is parsed", {
  parsed <- parse_cellchat_object(.comm_cellchat_object_real(),
                                  source_file = "cellchat_obj.rds")
  tab <- parsed$table
  # 3 valeurs non nulles -> 3 lignes extraites (fidele, sans agregation).
  expect_identical(nrow(tab), 3L)
  expect_identical(parsed$n_input_rows, 2L)   # 2 interactions
  expect_identical(sort(unique(tab$sender)), c("B", "CD4 T"))
  expect_identical(sort(unique(tab$receiver)), c("B", "CD4 T"))
  expect_identical(tab$source_method, rep("cellchat", 3L))
  sel <- tab[tab$sender == "CD4 T" & tab$receiver == "B", ]
  expect_identical(nrow(sel), 1L)
  expect_identical(sel$score, 0.5)
  # p-values extraites de net$pval (meme forme) par indices.
  expect_identical(sel$p_value, 0.01)
  # ligand/receptor/pathway sont RESOLUS via @LR$LRsig (appariement exact sur
  # interaction_name) — jamais devines, jamais laisses a NA quand la base est
  # presente (regle des deux voies : meme sortie que le moteur).
  expect_identical(sel$ligand, "IL7")
  expect_identical(sel$receptor, "IL7R")
  expect_identical(sel$pathway, "IL7 signaling")
  expect_identical(sel$interaction, "IL7 -> IL7R")
  expect_true(all(is.na(tab$p_adjusted)))
  expect_identical(parsed$column_mapping$ligand,
                   "object@LR$LRsig$ligand (apparie par interaction_name)")
})

test_that("Fixture O2 - CellChat object route works through an .rds file", {
  path <- tempfile(fileext = ".rds")
  saveRDS(.comm_cellchat_object_real(), path)
  parsed <- parse_cellchat_object(path, source_file = basename(path))
  expect_identical(nrow(parsed$table), 3L)
  expect_true(all(parsed$table$source_file == basename(path)))
})

test_that("Fixture O3 - CellChat object without pval (or wrong pval shape) keeps p NA", {
  parsed <- parse_cellchat_object(.comm_cellchat_object_real(with_pval = FALSE))
  expect_true(all(is.na(parsed$table$p_value)))
  # Forme incompatible : p NA + avertissement, jamais de fabrication.
  bad <- .comm_cellchat_object_real()
  bad$net$pval <- array(1, dim = c(2, 2))
  parsed2 <- parse_cellchat_object(bad)
  expect_true(all(is.na(parsed2$table$p_value)))
  expect_true(any(grepl("forme differente", parsed2$warnings)))
})

test_that("Fixture O4 - CellChat object failure modes are explicit and classed", {
  # net$prob absent.
  e1 <- tryCatch(parse_cellchat_object(list(net = list())), error = function(e) e)
  expect_identical(communication_error_state(e1), "invalid_schema")
  # Array sans dimnames.
  bad <- list(net = list(prob = array(1, dim = c(2, 2, 2))))
  e2 <- tryCatch(parse_cellchat_object(bad), error = function(e) e)
  expect_identical(communication_error_state(e2), "invalid_schema")
  # Forme FICTIVE historique [ligand, recepteur, "sender|receiver"] : REFUSEE,
  # jamais interpretee — la lire ferait croire que "CD4 T" est un ligand.
  e3 <- tryCatch(parse_cellchat_object(.comm_cellchat_object_legacy_shape()),
                 error = function(e) e)
  expect_identical(communication_error_state(e3), "invalid_schema")
  # Tout nul : rien a importer.
  zero <- .comm_cellchat_object_real()
  zero$net$prob[] <- 0
  e4 <- tryCatch(parse_cellchat_object(zero), error = function(e) e)
  expect_identical(communication_error_state(e4), "invalid_input")
  # Objet d'un type inattendu.
  e5 <- tryCatch(parse_cellchat_object(42), error = function(e) e)
  expect_identical(communication_error_state(e5), "invalid_input")
  # Chemin inexistant.
  e6 <- tryCatch(parse_cellchat_object("inexistant.rds"), error = function(e) e)
  expect_identical(communication_error_state(e6), "invalid_input")
  # S4 sans slot 'net' (classe de test definie localement — pas CellChat).
  if (!"CommTestNoNet" %in% methods::getClasses(where = globalenv())) {
    methods::setClass("CommTestNoNet", slots = list(x = "numeric"),
                      where = globalenv())
  }
  s4 <- methods::new("CommTestNoNet", x = 1)
  e7 <- tryCatch(parse_cellchat_object(s4), error = function(e) e)
  expect_identical(communication_error_state(e7), "invalid_schema")
})

# ── Filtres d'affichage : jamais destructifs, comptages explicites ──────────
test_that("Fixture F1 - default filters return the full canonical table", {
  r <- .comm_result_big()
  before <- r$canonical_table
  fs <- communication_apply_filters(r)
  expect_identical(nrow(fs$table), nrow(before))
  expect_identical(fs$summary$n_before, fs$summary$n_after)
  expect_identical(before, r$canonical_table)   # non modifiee
})

test_that("Fixture F2 - score and p-value filters drop unverifiable rows explicitly", {
  r <- .comm_result_big()
  fs <- communication_apply_filters(r, list(score_min = 0.5))
  expect_true(all(fs$table$score >= 0.5))
  expect_identical(fs$summary$dropped_score, sum(r$canonical_table$score < 0.5))
  # p_value_max : les lignes sans p-value sont retirees (filtre non
  # verifiable ne passe pas) et comptabilisees.
  fs2 <- communication_apply_filters(r, list(p_value_max = 0.1))
  expect_true(all(fs2$table$p_value <= 0.1))
  expect_identical(fs2$summary$dropped_p_value,
                   sum(is.na(r$canonical_table$p_value) |
                         r$canonical_table$p_value > 0.1))
  expect_match(fs$description, "score_min=0.5", fixed = TRUE)
})

test_that("Fixture F3 - pathway, sender, receiver and self filters", {
  r <- .comm_result_big()
  fs_pw <- communication_apply_filters(r, list(pathways = "IL7 signaling"))
  expect_true(all(fs_pw$table$pathway == "IL7 signaling"))
  expect_identical(fs_pw$summary$dropped_pathway,
                   sum(is.na(r$canonical_table$pathway) |
                         r$canonical_table$pathway != "IL7 signaling"))

  fs_s <- communication_apply_filters(r, list(senders = "CD4 T"))
  expect_true(all(fs_s$table$sender_node == "CD4 T"))

  # Filtre sur un label SANS correspondance : le noeud coalescent est le
  # label brut (Mono) — la selection reste possible et explicite.
  fs_m <- communication_apply_filters(r, list(senders = "Mono"))
  expect_true(all(fs_m$table$sender == "Mono"))
  expect_identical(nrow(fs_m$table), 1L)

  fs_self <- communication_apply_filters(r, list(include_self = FALSE))
  expect_identical(fs_self$summary$dropped_self, 1L)
  expect_false(any(fs_self$table$sender_node == fs_self$table$receiver_node))
})

test_that("Fixture F4 - combining filters can yield an empty (handled) table", {
  r <- .comm_result_big()
  fs <- communication_apply_filters(r, list(score_min = 0.99, p_value_max = 0.0001))
  expect_identical(nrow(fs$table), 0L)
  expect_identical(fs$summary$n_after, 0L)
  # Les vues sur une selection vide restent des ggplot avec message.
  p <- plot_communication_dotplot(r, fs$table)
  expect_s3_class(p, "ggplot")
})

# ── Vues : consommatrices pures, gardes de peremption ───────────────────────
test_that("Fixture V1 - dotplot aggregates per node pair and shows score guardrails", {
  r <- .comm_result_big()
  tbl <- r$canonical_table
  p <- plot_communication_dotplot(r, seurat_obj = .comm_obj_stub())
  expect_s3_class(p, "ggplot")
  expect_identical(r$canonical_table, tbl)   # la vue ne mute pas le resultat
  # Sous-titre : methode source + garde-fou d'echelle + non-causalite.
  sub <- paste(p$labels$subtitle, collapse = " ")
  expect_match(sub, "cellchat")
  expect_match(sub, "non comparable")
  expect_match(sub, "aucune causalite", ignore.case = TRUE)
})

test_that("Fixture V2b - heatmap on a source without any pathway shows explicit message", {
  # Source sans aucun pathway (CellPhoneDB) : message explicite, pas un
  # graphe vide — le message est porte dans le sous-titre.
  parsed_cpdb <- parse_cellphonedb_import(.comm_cellphonedb_means())
  r_cpdb <- .comm_import_and_finalize(parsed_cpdb,
                                      identities = c("CD4 T", "B", "CD8 T"),
                                      source_files = list(means = "means.txt"))
  p2 <- plot_communication_pathway_heatmap(r_cpdb)
  expect_s3_class(p2, "ggplot")
  expect_match(p2$labels$title, "Heatmap pathways", fixed = TRUE)
  expect_match(p2$labels$subtitle, "Aucun pathway renseigne", fixed = TRUE)
})

test_that("Fixture V2 - pathway heatmap excludes NA pathways with explicit counts", {
  r <- .comm_result_big()
  p <- plot_communication_pathway_heatmap(r)
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$subtitle, "sans pathway", fixed = TRUE)
  expect_match(p$labels$subtitle, "2 sans pathway (exclues, jamais imputees)", fixed = TRUE)
  expect_match(p$labels$subtitle, "7 interaction(s) avec pathway", fixed = TRUE)
})

test_that("Fixture V3 - circle network draws edges minus self, message when nothing drawable", {
  r <- .comm_result_big()
  p <- plot_communication_circle(r, seurat_obj = .comm_obj_stub())
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$subtitle, "1 auto-interaction", fixed = TRUE)
  # Uniquement des auto-interactions : message, pas de crash.
  parsed_self <- parse_cellchat_import(.comm_cellchat_tab())
  r_self <- .comm_import_and_finalize(parsed_self)
  only_self <- r_self$canonical_table
  only_self$receiver <- only_self$sender
  only_self$receiver_mapped <- only_self$sender_mapped
  r_self$canonical_table <- only_self
  p2 <- plot_communication_circle(r_self)
  expect_s3_class(p2, "ggplot")
  expect_match(p2$labels$subtitle, "auto-interaction", fixed = TRUE)
})

test_that("Fixture V4 - stale result is refused by every view", {
  r <- .comm_result_big()
  other <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("a", "b"), c("x", "y")))
  for (fn in list(plot_communication_dotplot, plot_communication_pathway_heatmap,
                  plot_communication_circle)) {
    e <- tryCatch(fn(r, seurat_obj = other), error = function(e) e)
    expect_identical(communication_error_state(e), "stale_against_current_seurat_object")
  }
})

# ── Centralite : descriptive, niveau reseau ─────────────────────────────────
test_that("Fixture C1 - centrality sums and degrees match the table", {
  r <- .comm_result_big()
  cen <- build_communication_centrality(r)
  tbl <- r$canonical_table
  cd4 <- cen[cen$node == "CD4 T", ]
  expect_identical(cd4$n_out_interactions, sum(tbl$sender == "CD4 T"))
  expect_identical(cd4$n_in_interactions, sum(tbl$receiver == "CD4 T"))
  expect_identical(cd4$out_score_total,
                   sum(tbl$score[tbl$sender == "CD4 T"]))
  expect_true(all(c("analysis_id", "source_method", "identity_column") %in% colnames(cen)))
  expect_identical(cen$analysis_id[1], "sc-communication-import")
  # Trie par poids total decroissant.
  expect_true(all(diff(cen$total_interactions) <= 0))
})

test_that("centrality returns a typed empty table for an empty filtered selection", {
  r <- .comm_result_big()
  filtered <- communication_apply_filters(r, list(senders = "__none__"))$table
  centrality <- build_communication_centrality(r, filtered)
  expect_s3_class(centrality, "data.frame")
  expect_identical(nrow(centrality), 0L)
  expect_true(all(c("node", "total_interactions", "analysis_id") %in%
                    colnames(centrality)))
})

# ── Provenance des filtres + export filtre ──────────────────────────────────
test_that("Fixture P1 - filter provenance entry is produced with frozen parameters", {
  r <- .comm_result_big()
  fs <- communication_apply_filters(r, list(score_min = 0.5))
  entry <- build_communication_filter_provenance(r, fs)
  expect_identical(entry$analysis_id, "sc-communication-explore")
  expect_identical(entry$method, "explore_cellchat")
  expect_identical(entry$analysis_type, "cell_cell_communication")
  expect_false(isTRUE(entry$import_only))
  expect_match(as.character(entry$parameters$applied_filters), "score_min=0.5", fixed = TRUE)
  expect_identical(entry$parameters$n_rows_before, fs$summary$n_before)
  expect_identical(entry$parameters$n_rows_after, fs$summary$n_after)
  expect_identical(entry$parameters$base_analysis_id, "sc-communication-import")
  # Appending a un etat d'analyse (chemin module reel).
  state <- create_analysis_state("sc")
  provenance_append(state, entry)
  entries <- state_get(state, "provenance")
  expect_identical(length(entries), 1L)
  # Mauvais argument : erreur classee.
  e <- tryCatch(build_communication_filter_provenance(r, list()), error = function(e) e)
  expect_identical(communication_error_state(e), "invalid_input")
})

test_that("Fixture P2 - filtered export carries filters and analysis_id on every row", {
  r <- .comm_result_big()
  fs <- communication_apply_filters(r, list(senders = "CD4 T"))
  df <- build_communication_filtered_export(r, fs$table, fs$description)
  expect_identical(nrow(df), nrow(fs$table))
  expect_true(all(df$analysis_id == "sc-communication-import"))
  expect_true(all(df$applied_filters == fs$description))
  expect_match(fs$description, "senders=[CD4 T]", fixed = TRUE)
})

test_that("rank filtering retains finite best-first ranks and counts missing ranks", {
  r <- .comm_rank_views_result()
  fs <- communication_apply_filters(r, list(rank_max = 3))
  expect_identical(fs$table$rank, c(1, 3))
  expect_identical(fs$summary$dropped_rank, 2L)
  expect_match(fs$description, "rank_max=3", fixed = TRUE)
  expect_match(fs$description, "dropped_rank=2", fixed = TRUE)
  expect_identical(communication_apply_filters(r)$summary$dropped_rank, 0L)

  r_zero <- r
  r_zero$canonical_table$rank[1L] <- 0
  zero <- communication_apply_filters(r_zero, list(rank_max = 0))
  expect_identical(zero$summary$n_after, 1L)
  expect_error(
    communication_apply_filters(r, list(rank_max = -1)),
    class = "communication_import_error"
  )
  expect_error(
    communication_apply_filters(.comm_result_big(), list(rank_max = 2)),
    class = "communication_import_error"
  )
  expect_error(
    communication_apply_filters(r, list(score_min = 0.5)),
    class = "communication_import_error"
  )
  expect_error(
    communication_apply_filters(r, list(score_min = 0.5, rank_max = 3)),
    class = "communication_import_error"
  )
})

test_that("rank dotplot uses mean rank with a reversed labelled scale", {
  r <- .comm_rank_views_result()
  fs <- communication_apply_filters(r, list(rank_max = 3))
  p <- plot_communication_dotplot(r, fs$table)
  expect_true(all(c("mean_rank", "best_rank") %in% colnames(p$data)))
  expect_false("score" %in% colnames(p$data))
  expect_true(all(is.na(p$data$mean_score)))
  expect_identical(p$data$mean_rank, 2)
  expect_identical(p$data$best_rank, 1)
  scale_colour <- p$scales$get_scales("colour")
  expect_match(scale_colour$name, "rang moyen (1 = meilleur)", fixed = TRUE)
  expect_true(inherits(scale_colour$trans, "transform"))
  expect_true(is.function(scale_colour$trans$transform))
  expect_gt(scale_colour$trans$transform(1), scale_colour$trans$transform(3))
  expect_match(p$labels$subtitle, "rang 1 = meilleur", fixed = TRUE)
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
})

test_that("views fall back explicitly when every imported rank is missing", {
  r <- .comm_rank_views_result()
  r$canonical_table$rank <- NA_real_
  p <- plot_communication_dotplot(r, r$canonical_table)
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$subtitle, "Aucun score dans la selection", fixed = TRUE)
  circle <- plot_communication_circle(r, r$canonical_table)
  expect_s3_class(circle, "ggplot")
  expect_true(all(circle$layers[[1L]]$data$weight == circle$layers[[1L]]$data$weight[1L]))
})

test_that("rank circle weights interactions and states rank semantics", {
  r <- .comm_rank_views_result()
  fs <- communication_apply_filters(r, list(rank_max = 3))
  p <- plot_communication_circle(r, fs$table)
  expect_match(p$labels$subtitle, "nombre d'interactions", fixed = TRUE)
  expect_match(p$labels$subtitle, "rang 1 = meilleur", fixed = TRUE)
  expect_match(p$labels$subtitle, "jamais somme des rangs", fixed = TRUE)
  edge_data <- p$layers[[1L]]$data
  expect_true(all(edge_data$weight == 2))
})

test_that("rank centrality reports best and median rank without summing ranks", {
  r <- .comm_rank_views_result()
  fs <- communication_apply_filters(r, list(rank_max = 3))
  cen <- build_communication_centrality(r, fs$table)
  expect_true(all(c("out_best_rank", "out_median_rank", "in_best_rank", "in_median_rank") %in%
                    colnames(cen)))
  cd4 <- cen[cen$node == "CD4 T", ]
  b <- cen[cen$node == "B", ]
  expect_identical(cd4$n_out_interactions, 2L)
  expect_identical(cd4$out_best_rank, 1)
  expect_identical(cd4$out_median_rank, 2)
  expect_identical(b$n_in_interactions, 2L)
  expect_identical(b$in_best_rank, 1)
  expect_identical(b$in_median_rank, 2)
  expect_true(all(is.na(cen$out_score_total)))
  expect_true(all(is.na(cen$in_score_total)))
  expect_false("out_rank_total" %in% colnames(cen))
  expect_true(all(diff(cen$total_interactions) <= 0))
})

test_that("rank provenance records threshold and dropped rank", {
  r <- .comm_rank_views_result()
  fs <- communication_apply_filters(r, list(rank_max = 3))
  entry <- build_communication_filter_provenance(r, fs)
  expect_equal(entry$parameters$rank_max, 3)
  expect_identical(entry$parameters$dropped_rank, 2L)
  expect_match(as.character(entry$parameters$applied_filters), "rank_max=3", fixed = TRUE)
  expect_match(as.character(entry$parameters$applied_filters), "dropped_rank=2", fixed = TRUE)
})

test_that("score-source views and centrality remain non-regressive", {
  r <- .comm_result_big()
  fs <- communication_apply_filters(r, list(score_min = 0.5))
  p <- plot_communication_dotplot(r, fs$table)
  expect_false(any(c("mean_rank", "best_rank") %in% colnames(p$data)))
  expect_match(p$scales$get_scales("colour")$name, "Score moyen importe", fixed = TRUE)
  expect_true(all(is.finite(p$data$mean_score)))
  pc <- plot_communication_circle(r, fs$table)
  expected_weight <- sum(fs$table$score[fs$table$sender_node == "CD4 T" &
                                           fs$table$receiver_node == "B"])
  expect_true(any(abs(pc$layers[[1L]]$data$weight - expected_weight) < 1e-8))
  cen <- build_communication_centrality(r, fs$table)
  expect_false(any(grepl("rank", colnames(cen), fixed = TRUE)))
  cd4 <- cen[cen$node == "CD4 T", ]
  expect_identical(cd4$out_score_total,
                   sum(fs$table$score[fs$table$sender_node == "CD4 T"]))
  expect_true(all(diff(cen$total_interactions) <= 0))
})

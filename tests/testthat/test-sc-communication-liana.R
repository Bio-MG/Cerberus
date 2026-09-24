# =============================================================================
# test-sc-communication-liana.R — CCC 7-8 route (b) : import de rangs LIANA
# =============================================================================
# Invariants testes :
#   - la route importe des RANGS produits HORS de l'application (aucun calcul,
#     aucun score fabrique) ;
#   - `aggregate_rank` (p-value RRA) alimente `p_value`, JAMAIS `score` ;
#   - `score` reste NA : un rang n'est pas un score (erreur de categorie) ;
#   - `rank_direction` = "lower_is_better" (inverse de `prob` CellChat) ;
#   - `rank_aggregation_mode` est un CHOIX EXPLICITE, jamais un defaut ;
#   - les colonnes `.complex` ne sont JAMAIS decoupees ;
#   - `is_external_consensus` distingue un agregat inter-methodes LIANA d'un
#     rang mono-methode ;
#   - les sources CellChat / CellPhoneDB ne gagnent AUCUNE colonne de rang
#     (regle 1 : zero changement de comportement sur l'existant).
# Fixtures partages : helper-communication-fixtures.R.
# =============================================================================

# ── Schema : les 12 champs contractuels + les champs de mesure de rang ──────
test_that("LIANA table maps to the 12 contract fields plus the rank measures", {
  parsed <- parse_liana_import(.comm_liana_tab(), rank_column = "mean_rank",
                               aggregation_mode = "specificity",
                               source_file = "liana_aggr.csv")
  tab <- parsed$table

  expect_true(all(communication_contract_fields() %in% colnames(tab)))
  expect_true(all(communication_rank_fields() %in% colnames(tab)))
  expect_identical(parsed$n_input_rows, 4L)
  expect_identical(unique(tab$source_method), "liana")
  expect_identical(unique(tab$source_file), "liana_aggr.csv")
  expect_identical(tab$interaction,
                   c("IL7 -> IL7R", "CD40 -> CD40", "CCL5 -> CCR5", "GZMB -> NKG7"))
})

# ── Semantique des rangs : un rang n'est PAS un score ───────────────────────
test_that("score stays NA and aggregate_rank feeds p_value (never score)", {
  parsed <- parse_liana_import(.comm_liana_tab(), rank_column = "mean_rank",
                               aggregation_mode = "specificity")
  tab <- parsed$table

  # score : NA par CONSTRUCTION — aucune conversion rang -> score.
  expect_true(all(is.na(tab$score)))
  # aggregate_rank est une p-value RRA : c'est sa place exacte.
  expect_identical(tab$p_value, c(0.001, 0.010, 0.200, 0.500))
  # p_adjusted / pathway : absents du format agrege, donc NA (jamais fabriques).
  expect_true(all(is.na(tab$p_adjusted)))
  expect_true(all(is.na(tab$pathway)))
  # La mesure ordinale vit dans `rank`, avec sa direction explicite.
  expect_identical(tab$rank, c(1, 2, 3, 4))
  expect_identical(unique(tab$rank_direction), "lower_is_better")
  expect_identical(unique(tab$rank_aggregation_mode), "specificity")
})

test_that("the chosen rank column is the one imported (not a fixed default)", {
  parsed <- parse_liana_import(.comm_liana_tab(), rank_column = "natmi.rank",
                               aggregation_mode = "magnitude")
  expect_identical(parsed$table$rank, c(1, 2, 3, 4))
  expect_identical(unique(parsed$table$rank_aggregation_mode), "magnitude")
  expect_identical(parsed$column_mapping$rank, "natmi.rank")
})

test_that("rank column resolution is case-insensitive and traced", {
  parsed <- parse_liana_import(.comm_liana_tab(), rank_column = "MEAN_RANK",
                               aggregation_mode = "specificity")
  # La colonne ORIGINALE est tracee (pas la saisie utilisateur).
  expect_identical(parsed$column_mapping$rank, "mean_rank")
})

# ── Ligand / recepteur : decomplexifie prioritaire, complexe JAMAIS decoupe ─
test_that("decomplexified ligand/receptor columns take priority", {
  parsed <- parse_liana_import(.comm_liana_tab(), rank_column = "mean_rank",
                               aggregation_mode = "specificity")
  expect_identical(parsed$column_mapping$ligand, "ligand")
  expect_identical(parsed$column_mapping$receptor, "receptor")
})

test_that("complex columns are reused as-is, never split", {
  tab <- .comm_liana_tab_complex_only()
  parsed <- parse_liana_import(tab, rank_column = "mean_rank",
                               aggregation_mode = "specificity")
  # Reprises TELLES QUELLES : un complexe n'a pas de decoupage univoque.
  expect_identical(parsed$table$ligand, c("IL7", "CD40", "CCL5", "GZMB"))
  expect_match(parsed$column_mapping$ligand, "complexe repris tel quel")
  expect_match(parsed$column_mapping$receptor, "complexe repris tel quel")
})

test_that("a genuine complex name is never split into subunits", {
  tab <- .comm_liana_tab_complex_only()
  tab$ligand.complex[1L] <- "TGFB1_TGFB2"
  parsed <- parse_liana_import(tab, rank_column = "mean_rank",
                               aggregation_mode = "specificity")
  # Le nom de complexe reste intact : le decouper serait inventer une donnee.
  expect_identical(parsed$table$ligand[1L], "TGFB1_TGFB2")
})

# ── Consensus externe : importe et MARQUE, jamais calcule par l'app ─────────
test_that("is_external_consensus marks aggregates computed by LIANA", {
  # mean_rank (moyenne des rangs) ET aggregate_rank (RRA) sont des agregats
  # inter-methodes presents dans la table -> TRUE.
  expect_true(parse_liana_import(.comm_liana_tab(), "mean_rank",
                                 "specificity")$external_consensus)
  expect_true(parse_liana_import(.comm_liana_tab(), "aggregate_rank",
                                 "specificity")$external_consensus)
  # Rang mono-methode ET aucune colonne de consensus -> FALSE.
  expect_false(parse_liana_import(.comm_liana_tab_single_method(), "natmi.rank",
                                  "specificity")$external_consensus)
})

test_that("the external-consensus marker reaches the canonical result", {
  res <- .comm_liana_result()
  expect_identical(res$source_method, "liana")
  expect_true(isTRUE(res$provenance$is_external_consensus))
  expect_true(isTRUE(res$provenance$import_only))
  expect_identical(res$provenance$analysis_type, "cell_cell_communication")

  res_single <- .comm_liana_result(
    rank_column = "natmi.rank", tab = .comm_liana_tab_single_method()
  )
  expect_false(isTRUE(res_single$provenance$is_external_consensus))
})

test_that("the external-consensus nuance is stated in a warning", {
  parsed <- parse_liana_import(.comm_liana_tab(), "mean_rank", "specificity")
  expect_true(any(grepl("INTER-METHODES", parsed$warnings)))
  # Un rang mono-methode sans colonne de consensus ne doit PAS produire cet
  # avertissement (sinon il perdrait toute valeur d'alerte).
  parsed_single <- parse_liana_import(.comm_liana_tab_single_method(),
                                      "natmi.rank", "specificity")
  expect_false(any(grepl("INTER-METHODES", parsed_single$warnings)))
})

# ── Mode d'agregation : choix explicite, aucun defaut implicite ─────────────
test_that("both aggregation modes are accepted and recorded", {
  for (m in communication_rank_aggregation_modes()) {
    parsed <- parse_liana_import(.comm_liana_tab(), "mean_rank", m)
    expect_identical(unique(parsed$table$rank_aggregation_mode), m)
  }
})

test_that("a missing or unknown aggregation mode is rejected", {
  expect_error(parse_liana_import(.comm_liana_tab(), "mean_rank", NULL),
               class = "communication_import_error")
  expect_error(parse_liana_import(.comm_liana_tab(), "mean_rank", ""),
               class = "communication_import_error")
  expect_error(parse_liana_import(.comm_liana_tab(), "mean_rank", "bogus"),
               class = "communication_import_error")
  # Etat structure : invalid_input (decision utilisateur manquante).
  e <- tryCatch(parse_liana_import(.comm_liana_tab(), "mean_rank", "bogus"),
                error = function(e) e)
  expect_identical(communication_error_state(e), "invalid_input")
})

# ── Colonne de rang : exigee, resolue, et son absence est diagnostiquee ─────
test_that("a missing rank column selection is rejected", {
  expect_error(parse_liana_import(.comm_liana_tab(), NULL, "specificity"),
               class = "communication_import_error")
  expect_error(parse_liana_import(.comm_liana_tab(), "", "specificity"),
               class = "communication_import_error")
})

test_that("an absent rank column lists the rank columns actually available", {
  e <- tryCatch(parse_liana_import(.comm_liana_tab(), "nonexistent.rank",
                                   "specificity"),
                error = function(e) e)
  expect_identical(communication_error_state(e), "invalid_schema")
  # Le message doit aider : nommer ce qui existe (mean_rank, aggregate_rank, ...).
  expect_match(conditionMessage(e), "mean_rank", fixed = TRUE)
  expect_match(conditionMessage(e), "aggregate_rank", fixed = TRUE)
})

# ── Echecs d'entree / de schema ────────────────────────────────────────────
test_that("invalid inputs and schemas fail with classed errors", {
  expect_error(parse_liana_import("not a table", "mean_rank", "specificity"),
               class = "communication_import_error")
  expect_error(parse_liana_import(NULL, "mean_rank", "specificity"),
               class = "communication_import_error")
  expect_error(parse_liana_import(.comm_liana_tab()[0, ], "mean_rank",
                                  "specificity"),
               class = "communication_import_error")

  no_sender <- .comm_liana_tab()[, setdiff(colnames(.comm_liana_tab()), "source")]
  e <- tryCatch(parse_liana_import(no_sender, "mean_rank", "specificity"),
                error = function(e) e)
  expect_identical(communication_error_state(e), "invalid_schema")

  no_ligand <- .comm_liana_tab()[, setdiff(colnames(.comm_liana_tab()),
                                           c("ligand", "ligand.complex"))]
  e2 <- tryCatch(parse_liana_import(no_ligand, "mean_rank", "specificity"),
                 error = function(e) e)
  expect_identical(communication_error_state(e2), "invalid_schema")
})

# ── QC : les rangs sont comptes, jamais corriges ───────────────────────────
test_that("QC counts out-of-range and missing ranks without correcting them", {
  tab <- .comm_liana_tab()
  tab$mean_rank <- c(1, 2, 3, 99)          # 99 > nrow = 4
  parsed <- parse_liana_import(tab, "mean_rank", "specificity")
  harm <- harmonize_communication_identities(parsed$table, .comm_identities,
                                             "cell_type")
  qcr <- communication_import_qc(harm$table)
  expect_identical(qcr$counts$n_rank_out_of_range, 1L)
  # Conserve et signale, jamais corrige.
  expect_identical(qcr$table$rank[4L], 99)
  expect_true(any(grepl("rang\\(s\\) hors", qcr$warnings)))

  tab2 <- .comm_liana_tab()
  tab2$mean_rank <- c(1, NA, 3, 4)
  p2 <- parse_liana_import(tab2, "mean_rank", "specificity")
  h2 <- harmonize_communication_identities(p2$table, .comm_identities, "cell_type")
  q2 <- communication_import_qc(h2$table)
  expect_identical(q2$counts$n_rank_missing, 1L)
  expect_true(any(grepl("jamais imputes", q2$warnings)))

  tab3 <- .comm_liana_tab()
  parsed3 <- parse_liana_import(tab3, "mean_rank", "specificity")
  harm3 <- harmonize_communication_identities(parsed3$table, .comm_identities,
                                               "cell_type")
  with_blank <- rbind(harm3$table, harm3$table[1L, , drop = FALSE])
  with_blank$rank <- c(1, 2, 3, 4, 5)
  with_blank$sender[nrow(with_blank)] <- NA_character_
  q3 <- communication_import_qc(with_blank)
  expect_identical(q3$counts$n_rank_out_of_range, 0L)
})

test_that("aggregate_rank imported as p_value passes the p-value range QC", {
  res <- .comm_liana_result()
  # Toutes les valeurs sont dans [0,1] : aucune p-value hors bornes.
  expect_identical(res$qc$n_p_value_out_of_range, 0L)
  expect_identical(res$qc$n_rank_out_of_range, 0L)
})

test_that("rank QC columns are present but zero for sources without ranks", {
  parsed <- parse_cellchat_import(.comm_cellchat_tab())
  harm <- harmonize_communication_identities(parsed$table, .comm_identities,
                                             "cell_type")
  qcr <- communication_import_qc(harm$table)
  # Compteurs presents (contrat stable) mais nuls : la colonne rank n'existe pas.
  expect_identical(qcr$counts$n_rank_out_of_range, 0L)
  expect_identical(qcr$counts$n_rank_missing, 0L)
})

# ── Regle 1 : l'existant ne bouge pas ─────────────────────────────────────
test_that("CellChat and CellPhoneDB results gain no rank column (rule 1)", {
  cc <- .comm_import_and_finalize(parse_cellchat_import(.comm_cellchat_tab()))
  cp <- .comm_import_and_finalize(parse_cellphonedb_import(.comm_cellphonedb_means()))

  for (canon in list(cc, cp)) {
    expect_length(intersect(communication_rank_fields(),
                            colnames(canon$canonical_table)), 0L)
    expect_false(isTRUE(canon$provenance$is_external_consensus))
    # Les 12 champs contractuels restent tous presents, inchanges.
    expect_true(all(communication_contract_fields() %in%
                      colnames(canon$canonical_table)))
  }
  expect_identical(cc$source_method, "cellchat")
  expect_identical(cp$source_method, "cellphonedb")
})

test_that("a canonical result never mixes two sources", {
  # Le contrat interdit de melanger les sources : finalize refuse une table
  # dont le source_method ne correspond pas a l'argument.
  parsed <- parse_liana_import(.comm_liana_tab(), "mean_rank", "specificity")
  harm <- harmonize_communication_identities(parsed$table, .comm_identities,
                                             "cell_type")
  qcr <- communication_import_qc(harm$table)
  expect_error(
    finalize_communication_result(
      canonical_table = qcr$table, source_method = "cellchat",
      source_files = list(table = "x.csv"), identity_column = "cell_type",
      identity_mapping = harm$mapping, identity_summary = harm$summary,
      column_mapping = parsed$column_mapping, qc = qcr$counts,
      n_input_rows = parsed$n_input_rows, seurat_obj = .comm_obj_stub()
    ),
    class = "communication_import_error"
  )
})

# ── Le resultat LIANA est un resultat canonique a part entiere ─────────────
test_that("a LIANA result passes the canonical contract guard", {
  res <- .comm_liana_result()
  expect_invisible(assert_communication_result(res))
  expect_identical(res$type, "cell_cell_communication")
  expect_identical(res$status, "valid")
  expect_match(res$object_identity$fingerprint, "^v2::")
  expect_match(res$timestamp_utc, "^\\d{4}-\\d{2}-\\d{2}T")
  # Harmonisation exact match : les 4 populations existent dans les identites.
  expect_identical(res$identity_summary$n_labels_unmatched, 0L)
})

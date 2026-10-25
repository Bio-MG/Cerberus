# =============================================================================
# test-sc-communication-perturbation.R — V1.x-D : comportement de la
# perturbation IN SILICO (deltas exacts, cibles, erreurs, vues, export)
# =============================================================================

source_project_file("tools/pkg_skeleton/R/sc_communication_perturbation.R")

test_that("suppression d'un ligand : deltas exacts par paire", {
  r <- .ccx_result()
  ctx <- build_communication_perturbation(r, target = "ligand", value = "L1",
                                          mode = "remove")
  # L1 : A->B (0.4), C->B (0.2), D->A (0.9) supprimés ; A->C (L2) et C->C (L2) intacts
  expect_identical(ctx$baseline_summary$n_interactions, 5L)
  expect_identical(ctx$perturbed_summary$n_interactions, 2L)
  expect_identical(ctx$qc$n_removed, 3L)
  ab <- ctx$delta_table[ctx$delta_table$sender_node == "A" &
                          ctx$delta_table$receiver_node == "B", ]
  expect_equal(ab$delta_score_total, -0.4, tolerance = 1e-12)
  expect_equal(ab$delta_fraction, -1, tolerance = 1e-12)
  expect_true(ab$affected)
  ac <- ctx$delta_table[ctx$delta_table$sender_node == "A" &
                          ctx$delta_table$receiver_node == "C", ]
  expect_equal(ac$delta_score_total, 0)
  expect_false(ac$affected)
  # population D (non cartographiable) reste traitée normalement
  da <- ctx$delta_table[ctx$delta_table$sender_node == "D", ]
  expect_equal(da$delta_score_total, -0.9, tolerance = 1e-12)
  expect_setequal(ctx$qc$affected_senders, c("A", "C", "D"))
  expect_setequal(ctx$qc$affected_receivers, c("A", "B"))
})

test_that("atténuation d'un récepteur : scores scalés, lignes conservées", {
  r <- .ccx_result()
  ctx <- build_communication_perturbation(r, target = "receptor", value = "R1",
                                          mode = "attenuate", factor = 0.5)
  expect_identical(ctx$perturbed_summary$n_interactions, 5L)
  expect_identical(ctx$qc$n_attenuated, 3L)
  expect_identical(ctx$qc$n_removed, 0L)
  # A->B : 0.4 -> 0.2 => delta -0.2 ; fraction -0.5
  ab <- ctx$delta_table[ctx$delta_table$sender_node == "A" &
                          ctx$delta_table$receiver_node == "B", ]
  expect_equal(ab$score_total_perturbed, 0.2, tolerance = 1e-12)
  expect_equal(ab$delta_score_total, -0.2, tolerance = 1e-12)
  expect_equal(ab$delta_fraction, -0.5, tolerance = 1e-12)
})

test_that("cible interaction : toutes les lignes ligand->récepteur visées", {
  r <- .ccx_result()
  ctx <- build_communication_perturbation(r, target = "interaction",
                                          value = "L2 -> R2", mode = "remove")
  # L2 -> R2 : A->C (0.6) ET C->C (0.3)
  expect_identical(ctx$qc$n_removed, 2L)
  expect_identical(ctx$perturbed_summary$n_interactions, 3L)
})

test_that("cibles population : nœuds coalescents sender/receiver", {
  r <- .ccx_result()
  ctx <- build_communication_perturbation(r, target = "sender", value = "A",
                                          mode = "remove")
  expect_identical(ctx$qc$n_removed, 2L)  # A->B, A->C
  expect_identical(ctx$perturbed_summary$n_interactions, 3L)
  ctx2 <- build_communication_perturbation(r, target = "receiver", value = "B",
                                           mode = "remove")
  expect_identical(ctx2$qc$n_removed, 2L)  # A->B (0.4) + C->B (0.2)
  expect_identical(ctx2$perturbed_summary$n_interactions, 3L)
})

test_that("valeur absente : erreur explicite avec valeurs disponibles", {
  r <- .ccx_result()
  e <- tryCatch(
    build_communication_perturbation(r, target = "ligand", value = "ZZZ"),
    error = function(err) err
  )
  expect_s3_class(e, "communication_context_error")
  expect_identical(e$state, "invalid_input")
  expect_true(grepl("L1", conditionMessage(e), fixed = TRUE))  # liste les valeurs
})

test_that("réseau perturbé vidé d'une paire : deltas exacts", {
  # receiver A = D->A seulement : 1 ligne retirée, les 4 autres intactes
  r <- .ccx_result()
  ctx <- build_communication_perturbation(r, target = "receiver", value = "A",
                                          mode = "remove")
  expect_identical(ctx$perturbed_summary$n_interactions, 4L)
  expect_identical(ctx$qc$n_removed, 1L)
  da <- ctx$delta_table[ctx$delta_table$sender_node == "D", ]
  expect_equal(da$delta_score_total, -0.9, tolerance = 1e-12)
  # le cas réseau VIDE + avertissement est couvert par le test « table filtrée »
})

test_that("table filtrée des vues honorée", {
  r <- .ccx_result()
  fs <- communication_apply_filters(r, list(pathways = "P1"))
  ctx <- build_communication_perturbation(r, target = "ligand", value = "L1",
                                          filtered_table = fs$table)
  expect_identical(ctx$baseline_summary$n_interactions, 2L)  # A->B, C->B
  expect_identical(ctx$perturbed_summary$n_interactions, 0L)
  expect_true(any(grepl("vide", ctx$warnings)))
})

test_that("vues consommatrices pures + garde in silico", {
  r <- .ccx_result()
  ctx <- build_communication_perturbation(r, target = "ligand", value = "L1")
  g1 <- plot_communication_perturbation_delta(ctx)
  expect_s3_class(g1, "ggplot")
  expect_true(grepl("IN SILICO", g1$labels$subtitle %||% ""))
  g2 <- plot_communication_perturbation_nodes(ctx)
  expect_s3_class(g2, "ggplot")
  # sans scores importés : message explicite, pas de graphe trompeur
  tab <- .ccx_cellchat_tab()
  tab$prob <- NA_real_
  parsed <- parse_cellchat_import(tab, source_file = "x.csv")
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
  ctx2 <- build_communication_perturbation(r2, target = "ligand", value = "L1")
  expect_true(is.na(ctx2$delta_table$delta_score_total[1]))
  g3 <- plot_communication_perturbation_delta(ctx2)
  expect_s3_class(g3, "ggplot")
})

test_that("export tracé + étiquette par ligne", {
  r <- .ccx_result()
  ctx <- build_communication_perturbation(r, target = "receptor", value = "R1",
                                          mode = "attenuate", factor = 0.3)
  ex <- build_communication_perturbation_export(ctx)
  expect_equal(nrow(ex), nrow(ctx$delta_table))
  expect_true(all(ex$analysis_id == "sc-communication-perturbation"))
  expect_true(all(ex$parent_analysis_id == "sc-communication-import"))
  expect_true(all(ex$target == "receptor"))
  expect_true(all(ex$mode == "attenuate"))
  expect_true(all(ex$factor == 0.3))
  expect_true(all(ex$label == "IN SILICO PERTURBATION"))
})

# ── M3 (plan D2) : assainissement pré-package — identifiant accentué ─────────
# Mesuré le 2026-10-24 : ce fichier est le SEUL des 154 fichiers .R de R/ +
# modules/ qui ne parse PAS sous LC_CTYPE=C (« unexpected invalid token » à
# l'identifiant nu `Perturbé`, ligne 436 — CONVENTIONS.md §13.5). R CMD check
# (premier contact D2, M3) parse les sources : l'identifiant doit être quoté.
# Le garde = le comportement : parse() du fichier dans un SOUS-PROCESSUS
# LC_CTYPE=C (ne pas flipper la locale du processus de la suite — ça
# empoisonnerait les fichiers suivants), + aucun token SYMBOLE non-ASCII.
M3_TARGET <- normalizePath(
  file.path(ts_project_root(), "tools", "pkg_skeleton", "R",
            "sc_communication_perturbation.R"),  # ex-R/sc/, D2 M5 lot 6
  winslash = "/", mustWork = TRUE
)

test_that("M3 : le fichier parse sous LC_CTYPE=C (identifiant accentué quoté)", {
  rscript <- file.path(R.home("bin"),
                       if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  skip_if(!file.exists(rscript), "Rscript introuvable")
  child <- tempfile(fileext = ".R")
  writeLines(
    c("Sys.setlocale('LC_CTYPE', 'C')",
      sprintf(paste0("tryCatch({ parse('%s'); cat('PARSE_OK') }, ",
                     "error = function(e) cat('PARSE_FAIL:', conditionMessage(e)))"),
              M3_TARGET)),
    child, useBytes = TRUE
  )
  out <- tempfile("m3-cparse-")
  st <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(child)),
                                 stdout = out, stderr = out, timeout = 60))
  # ⚠ la sortie du enfant sous LC_CTYPE=C contient des octets UTF-8 INVALIDES
  # (le message d'erreur de parse affiche l'identifiant mangé) : lire en octets
  # et assainir en ASCII AVANT tout affichage — sinon gsub() du reporter échoue
  # avec « input string 1 is invalid » (CONVENTIONS.md §13.5).
  got_raw <- if (file.exists(out)) {
    readChar(out, file.info(out)$size, useBytes = TRUE)
  } else ""
  got <- gsub("[^ -~\r\n]", "?", got_raw, useBytes = TRUE)
  expect_identical(as.integer(st), 0L, info = got)
  expect_match(got, "PARSE_OK", info = got)  # RED avant M3 : PARSE_FAIL unexpected invalid token
})

test_that("M3 : plus aucun token SYMBOLE non-ASCII dans le fichier", {
  pd <- parse(M3_TARGET, keep.source = TRUE)
  toks <- utils::getParseData(pd)
  syms <- toks[toks$terminal & startsWith(toks$token, "SYMBOL"), , drop = FALSE]
  non_ascii <- unique(syms$text[grepl("[^ -~]", syms$text, useBytes = TRUE)])
  expect_length(non_ascii, 0L)  # RED avant M3 : le symbole nu `Perturbé` (l. 436)
})

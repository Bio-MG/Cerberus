# =============================================================================
# test-sc-milo-contract.R — Stage 14 : contrat du domaine Milo (4E-1)
# =============================================================================
# Portes : design Stage 13 OBLIGATOIRE (absent/bloque/perime refuses avant
# tout calcul), prerequis structurels (reduction, contraste, dimensions),
# forme du resultat canonique, direction du contraste, determinisme graine,
# exclusions, exports traces par analysis_id.
# =============================================================================

# ── Portes Stage 13 (design) ────────────────────────────────────────────────
test_that("milo refuses to run without a canonical Stage 13 design", {
  obj <- .milo_seurat_obj()
  expect_error(
    run_milo_da(obj, da_design_result = NULL, reduction = "pca",
                target_condition = "B", reference_condition = "A"),
    class = "da_design_error"
  )
  expect_error(
    run_milo_da(obj, da_design_result = list(), reduction = "pca",
                target_condition = "B", reference_condition = "A"),
    class = "da_design_error"
  )
  expect_error(
    run_milo_da(obj, da_design_result = "pas un design", reduction = "pca",
                target_condition = "B", reference_condition = "A"),
    class = "da_design_error"
  )
})

test_that("milo refuses a blocked (pseudoreplicated) design — never consumable", {
  blk <- .milo_seurat_obj(meta = .da_meta(
    samples = c(s1 = 50, s2 = 50, s3 = 50, s4 = 50),
    conditions = c(s1 = "A", s2 = "B", s3 = "B", s4 = "B")
  ))
  des <- .milo_design(blk)
  expect_identical(des$status, "invalid_design")
  expect_false(isTRUE(des$milo_eligibility$eligible))
  # Le design bloque est refuse AVANT tout calcul : erreur classee, etat
  # invalid_design, message explicite sur l'eligibilite.
  expect_error(
    run_milo_da(blk, da_design_result = des, reduction = "pca",
                target_condition = "B", reference_condition = "A"),
    class = "da_design_error"
  )
  e <- tryCatch(
    run_milo_da(blk, da_design_result = des, reduction = "pca",
                target_condition = "B", reference_condition = "A"),
    error = function(e) e
  )
  expect_identical(da_design_error_state(e), "invalid_design")
  expect_match(conditionMessage(e), "non eligible", fixed = TRUE)
})

test_that("milo refuses a stale design (object identity changed)", {
  des <- .milo_design(.milo_seurat_obj())
  # Objet de TAILLE differente : empreinte v2 divergente.
  other <- .milo_seurat_obj(meta = .da_meta(samples = c(s1 = 40, s2 = 40, s3 = 35, s4 = 35)))
  expect_true(isTRUE(da_design_result_is_stale(des, other)))
  e <- tryCatch(
    run_milo_da(other, da_design_result = des, reduction = "pca",
                target_condition = "B", reference_condition = "A"),
    error = function(e) e
  )
  expect_identical(da_design_error_state(e), "stale_against_current_seurat_object")
})

# ── Prerequis structurels ───────────────────────────────────────────────────
test_that("milo validates the reduction before any compute", {
  obj <- .milo_seurat_obj()
  des <- .milo_design(obj)
  expect_error(
    run_milo_da(obj, des, reduction = "inexistante",
                target_condition = "B", reference_condition = "A"),
    "reduction 'inexistante' est introuvable"
  )
  # Reduction a UNE dimension : insuffisante pour le graphe de voisinage.
  emb1 <- SeuratObject::Embeddings(obj, "pca")[, 1, drop = FALSE]
  obj[["pc1d"]] <- SeuratObject::CreateDimReducObject(
    embeddings = emb1,
    loadings = matrix(0, nrow = nrow(obj), ncol = 1),
    assay = "RNA", key = "PC1D_"
  )
  e <- tryCatch(
    run_milo_da(obj, des, reduction = "pc1d",
                target_condition = "B", reference_condition = "A"),
    error = function(e) e
  )
  expect_identical(class(e), c("milo_error", "error", "condition"))
  expect_identical(milo_error_state(e), "invalid_input")
})

test_that("milo validates the contrast conditions", {
  obj <- .milo_seurat_obj()
  des <- .milo_design(obj)
  e1 <- tryCatch(run_milo_da(obj, des, "pca", "C", "A"), error = function(e) e)
  expect_identical(class(e1), c("milo_error", "error", "condition"))
  expect_identical(milo_error_state(e1), "invalid_input")
  expect_match(conditionMessage(e1), "n'appartient pas au design", fixed = TRUE)
  e2 <- tryCatch(run_milo_da(obj, des, "pca", "A", "A"), error = function(e) e)
  expect_identical(milo_error_state(e2), "invalid_input")
})

test_that("milo_available exposes an explicit error policy", {
  av <- milo_available()
  expect_true(isTRUE(av$available))
  expect_match(av$version, "^\\d+\\.\\d+\\.\\d+")
})

# ── Resultat canonique ──────────────────────────────────────────────────────
test_that("run_milo_da produces the canonical contract on the valid fixture", {
  obj <- .milo_seurat_obj()
  res <- suppressMessages(.milo_run(obj = obj))
  expect_identical(setdiff(milo_contract_fields(), names(res)), character(0))
  expect_identical(res$type, "milo_da")
  expect_true(res$status %in% c("valid", "valid_with_warnings"))
  expect_identical(res$analysis_id, "sc-da-milo")
  # Table DA par voisinage — colonnes figees
  expect_identical(
    colnames(res$DA_table),
    c("Nhood", "n_cells", "logFC", "logCPM", "F", "PValue", "FDR",
      "SpatialFDR", "identity", "identity_fraction")
  )
  expect_identical(res$neighbourhood_summary$n_neighbourhoods, nrow(res$DA_table))
  expect_true(nrow(res$DA_table) > 0L)
  # Mappage par cellule : TOUTES les cellules conservees (aucune exclusion ici)
  expect_identical(nrow(res$nhood_assignment), as.integer(ncol(obj)))
  expect_setequal(names(res$nhood_assignment),
                  c("cell_id", "n_neighbourhoods", "mean_logFC"))
  # Contexte echantillon : niveau ECHANTILLON (pas de pseudoreplication)
  expect_identical(nrow(res$sample_composition), 4L)
  expect_identical(sum(res$sample_composition$n_cells_total), as.integer(ncol(obj)))
  # Versions et empreinte enregistrees
  expect_match(res$package_versions$miloR, "^\\d+\\.\\d+\\.\\d+")
  expect_match(res$object_identity$fingerprint, "^v2::")
  # Provenance produite a l'analyse (regle 7)
  expect_identical(res$provenance$analysis_type, "milo_da")
  expect_identical(res$provenance$analysis_id, "sc-da-milo")
  expect_identical(res$provenance$parameters$target_condition, "B")
  expect_identical(res$provenance$parameters$reference_condition, "A")
  expect_identical(res$provenance$parameters$composition_unit, "sample")
  expect_match(res$timestamp_utc, "^\\d{4}-\\d{2}-\\d{2}T")
})

test_that("contrast direction and model specification are recorded and readable", {
  obj <- .milo_seurat_obj()
  res <- suppressMessages(.milo_run(obj = obj))
  # La condition B est decalee sur PC1 dans le fixture : les voisinages
  # enrichis en B doivent dominer (logFC > 0 en moyenne).
  expect_gt(mean(res$DA_table$logFC), 0)
  expect_identical(res$tested_contrast$target, "B")
  expect_identical(res$tested_contrast$reference, "A")
  expect_match(res$tested_contrast$contrast, "conditionB")
  expect_match(res$tested_contrast$formula, "~ 0 \\+ condition")
  expect_match(res$tested_contrast$interpretation, "logFC > 0", fixed = TRUE)
  expect_identical(res$parameters$reduction, "pca")
  expect_identical(res$parameters$k, TS_DA_MILO_K)
  expect_identical(res$parameters$seed, TS_DA_MILO_SEED)
  expect_identical(res$parameters$batch_in_model, TRUE)
  expect_identical(res$model_specification$n_design_samples, 4L)
  expect_true("batch" %in% res$model_specification$design_cols)
  expect_identical(res$design$design_status, "valid")
  expect_match(res$design$design_fingerprint, "^v2::")
})

test_that("milo is reproducible for fixed input and seed (and seed-sensitive)", {
  obj <- .milo_seurat_obj()
  res1 <- suppressMessages(.milo_run(obj = obj))
  res2 <- suppressMessages(.milo_run(obj = obj))
  expect_equal(res1$DA_table, res2$DA_table, tolerance = 1e-10)
  expect_identical(res1$DA_table$n_cells, res2$DA_table$n_cells)
  expect_identical(res1$nhood_assignment$cell_id, res2$nhood_assignment$cell_id)
  expect_equal(res1$nhood_assignment$mean_logFC, res2$nhood_assignment$mean_logFC,
               tolerance = 1e-10)
  # La graine est REELLE (makeNhoods echantillonne aleatoirement) : changer
  # la graine peut changer les voisinages — elle est donc enregistree.
  res3 <- suppressMessages(.milo_run(obj = obj, seed = 7L))
  expect_false(identical(res1$DA_table$n_cells, res3$DA_table$n_cells))
})

test_that("cells with a missing sample_id are excluded and counted", {
  meta <- .da_meta(samples = c(s1 = 50, s2 = 50, s3 = 50, s4 = 50),
                   na_sample_id = 5L)
  obj <- .milo_seurat_obj(meta = meta)
  res <- suppressMessages(.milo_run(obj = obj))
  expect_identical(res$parameters$n_cells_excluded_missing_sample, 5L)
  expect_identical(res$provenance$cells_excluded, 5L)
  expect_identical(nrow(res$nhood_assignment), as.integer(ncol(obj)) - 5L)
  expect_identical(sum(res$sample_composition$n_cells_total), as.integer(ncol(obj)) - 5L)
})

test_that("design warnings are carried into the Milo result status", {
  meta <- .da_meta(samples = c(s1 = 8, s2 = 50, s3 = 50, s4 = 50))
  obj <- .milo_seurat_obj(meta = meta)
  res <- suppressMessages(.milo_run(obj = obj))
  expect_identical(res$status, "valid_with_warnings")
  expect_true(length(res$warnings) > 0L)
  expect_identical(res$design$design_status, "valid_with_warnings")
})

# ── Consommation (assert) ───────────────────────────────────────────────────
test_that("assert_milo_result guards consumers (canonical, staleness)", {
  obj <- .milo_seurat_obj()
  res <- suppressMessages(.milo_run(obj = obj))
  expect_invisible(assert_milo_result(res, seurat_obj = obj))
  other <- .milo_seurat_obj(meta = .da_meta(samples = c(s1 = 40, s2 = 40, s3 = 35, s4 = 35)))
  e <- tryCatch(assert_milo_result(res, seurat_obj = other), error = function(e) e)
  expect_identical(class(e), c("milo_error", "error", "condition"))
  expect_identical(milo_error_state(e), "stale_against_current_seurat_object")
  for (bad in list(NULL, list(), list(type = "milo_da"), "resultat")) {
    expect_error(assert_milo_result(bad), class = "milo_error")
  }
  # Un resultat avec un statut non-produit est refuse (jamais consommable).
  bad_status <- res; bad_status$status <- "compute_failed"
  expect_error(assert_milo_result(bad_status), class = "milo_error")
})

test_that("milo_result_is_stale follows the v2 fingerprint", {
  obj <- .milo_seurat_obj()
  res <- suppressMessages(.milo_run(obj = obj))
  expect_false(isTRUE(milo_result_is_stale(res, obj)))
  expect_true(isTRUE(milo_result_is_stale(res, .milo_seurat_obj(seed = 21,
    meta = .da_meta(samples = c(s1 = 45, s2 = 45, s3 = 45, s4 = 45))))))
  expect_false(isTRUE(milo_result_is_stale(res, .milo_seurat_obj(seed = 21))))
  expect_true(is.na(milo_result_is_stale(NULL, obj)))
})

# ── Exports ─────────────────────────────────────────────────────────────────
test_that("milo exports are traced by analysis_id and stable", {
  obj <- .milo_seurat_obj()
  res <- suppressMessages(.milo_run(obj = obj))
  s <- build_milo_summary(res)
  expect_identical(nrow(s), 1L)
  expect_true(all(c("analysis_id", "status", "tested_contrast", "model_formula",
                    "n_neighbourhoods", "miloR_version", "object_fingerprint",
                    "warnings") %in% colnames(s)))
  da <- build_milo_da_table_export(res)
  expect_identical(nrow(da), nrow(res$DA_table))
  expect_true(all(da$analysis_id == "sc-da-milo"))
  na_tab <- build_milo_nhood_assignment_export(res)
  expect_identical(nrow(na_tab), nrow(res$nhood_assignment))
  expect_true(all(na_tab$analysis_id == "sc-da-milo"))
  sc <- build_milo_sample_composition_export(res)
  expect_identical(nrow(sc), nrow(res$sample_composition))
  expect_true(all(sc$analysis_id == "sc-da-milo"))
  expect_match(milo_export_filename(res, "milo_summary", "csv"),
               "^milo_summary_sc-da-milo_\\d{4}-\\d{2}-\\d{2}\\.csv$")
  expect_error(build_milo_summary(list()), class = "milo_error")
})

# ---------------------------------------------------------------------------
# M15 (2026-10-17) — les warnings miloR doivent être COLLECTÉS, pas perdus
# ---------------------------------------------------------------------------
# Défaut mesuré : le gestionnaire écrivait
#
#     warning = function(w) { warnings_all <- c(warnings_all, ...) }
#
# Un `<-` DANS un gestionnaire crée une liaison LOCALE au gestionnaire : le
# `warnings_all` de la fonction appelante ne recevait jamais rien, donc
# `extra_warnings` (plus bas) annonçait « aucun avertissement » même quand
# miloR en avait émis. Le canal de diagnostic mentait, silencieusement.
# `<<-` écrit dans la frame appelante — c'est le seul remède, et c'est
# exactement ce que la garde ci-dessous exige.
test_that("milo : le gestionnaire de warning écrit avec `<<-`, jamais `<-`", {
  p <- ts_ast_parse("tools/pkg_skeleton/R/sc_abundance_milo.R")

  handlers <- ts_ast_find_all(p, function(x) {
    if (!ts_ast_is_call_to(x, "withCallingHandlers")) return(FALSE)
    nm <- names(as.list(x))
    !is.null(nm) && "warning" %in% nm
  })
  # Témoin de non-vacuité : sans gestionnaire trouvé, la garde passerait pour
  # la mauvaise raison (elle n'aurait rien regardé).
  expect_gt(length(handlers), 0L)

  bad <- character(0)
  for (h in handlers) {                          # (1) par INDEX
    l  <- as.list(h)
    nm <- names(l)
    for (i in seq_along(l)) {                    # (1) par INDEX
      if (!identical(nm[i], "warning")) next
      body <- l[[i]]
      # Toute affectation `<-` DANS le gestionnaire est suspecte : elle ne peut
      # pas atteindre la frame appelante. On nomme les cibles pour que l'échec
      # soit actionnable.
      ts_ast_walk(body, function(x) {
        if (!ts_ast_is_call_to(x, "<-")) return(FALSE)
        tgt <- tryCatch(ts_ast_deparse(as.list(x)[[2]]), error = function(e) "")
        bad <<- c(bad, sprintf("warning handler: %s <- ...", tgt))
        FALSE
      })
    }
  }
  expect_identical(bad, character(0),
                   info = paste("affectations locales dans un gestionnaire :",
                                paste(bad, collapse = " | ")))
})

test_that("milo : les warnings sont COLLECTÉS, pas perdus (comportement)", {
  r <- .milo_with_warnings({
    warning("premier")
    warning("second")
    42L
  })
  expect_identical(r$value, 42L)
  expect_identical(r$warnings, c("premier", "second"))

  # Collectés, donc MÛRS : aucun ne s'échappe vers l'appelant.
  expect_silent(.milo_with_warnings({ warning("x"); "ok" }))

  # Sans warning : liste vide et valeur intacte.
  r0 <- .milo_with_warnings(7L)
  expect_identical(r0$value, 7L)
  expect_identical(r0$warnings, character(0))
})

test_that("milo : le préfixe des warnings ne fabrique pas d'entrée orpheline", {
  # PIÈGE MESURÉ : `paste0("miloR : ", character(0))` rend `"miloR : "` — R
  # recycle sur la longueur 1 du préfixe — et NON `character(0)`. Un préfixe
  # orphelin suffirait à faire passer le statut à `valid_with_warnings` alors
  # qu'aucun avertissement n'existe.
  expect_identical(paste0("miloR : ", character(0)), "miloR : ")
  # Le helper PUR neutralise ce piège.
  expect_identical(.milo_prefix_warnings(character(0)), character(0))
  expect_identical(.milo_prefix_warnings("a"), "miloR : a")
  expect_identical(.milo_prefix_warnings(c("a", "b")),
                   c("miloR : a", "miloR : b"))
})

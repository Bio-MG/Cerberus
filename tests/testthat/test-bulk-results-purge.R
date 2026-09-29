# =============================================================================
# test-bulk-results-purge.R — invalidation des résultats au changement de jeu
# =============================================================================
# Parité SC (audit 2026-09-27 §1.5 transposé au domaine bulk — handoff §4) :
# shared_rv bulk n'était jamais purgé quand global_data$bulk_obj était
# remplacé — un jeu B héritait des contrastes/voies/mapping du jeu A. La
# purge est déclenchée par global_data$bulk_obj_epoch (incrémentée UNIQUEMENT
# par les remplacements de dataset — les re-commits de même lignée du
# pipeline ne purgent pas, cf. R/core/state.R).
# =============================================================================
source_project_file("R/core/state.R")

.ts_src <- function(relpath) {
  paste(readLines(file.path(ts_project_root(), relpath), warn = FALSE),
        collapse = "\n")
}

test_that("bulk_shared_result_fields couvre tout slot résultat écrit par un module", {
  expect_setequal(
    bulk_shared_result_fields(),
    c("counts_mapped", "counts_original", "mapping_applied", "mapping_summary",
      "filtered_counts", "dds_blind", "vst_mat", "dds_full",
      "contrasts", "active_contrast", "active_condition_col",
      "pathway_results", "pathway_db", "pathway_mode", "multimethod_de",
      "de_bypass")
  )
})

test_that("create_bulk_shared_state déclare chaque champ résultat (plus de champs hors schéma)", {
  s <- create_bulk_shared_state()
  nms <- shiny::isolate(names(s))
  missing <- setdiff(bulk_shared_result_fields(), nms)
  expect_length(missing, 0L)
})

test_that("bulk_purge_shared_results met à NULL chaque champ résultat — et uniquement eux", {
  s <- create_bulk_shared_state()
  s$contrasts       <- list(A_vs_B = data.frame(gene = "X"))
  s$pathway_results <- data.frame(ID = "GO:1")
  s$vst_mat         <- matrix(0, 2, 2)
  s$de_bypass       <- list(engine = "edger")
  s$active_tab      <- "tab_pca"     # état d'UI : ne doit PAS être purgé
  s$lfc_thresh      <- 2             # seuil utilisateur : ne doit PAS être purgé
  s$bulk_palette    <- "manual"      # préférence viz : ne doit PAS être purgé
  s$heatmap_annot   <- "condition"   # préférence viz : ne doit PAS être purgé

  bulk_purge_shared_results(s)

  for (f in bulk_shared_result_fields()) {
    expect_null(state_get(s, f), info = f)
  }
  expect_identical(state_get(s, "active_tab"), "tab_pca")
  expect_identical(state_get(s, "lfc_thresh"), 2)
  expect_identical(state_get(s, "bulk_palette"), "manual")
  expect_identical(state_get(s, "heatmap_annot"), "condition")
})

test_that("bulk_purge_shared_results est idempotent et invisible", {
  s <- create_bulk_shared_state()
  expect_invisible(bulk_purge_shared_results(s))
  expect_invisible(bulk_purge_shared_results(s))
  for (f in bulk_shared_result_fields()) expect_null(state_get(s, f))
})

# ── Ancres de câblage : l'epoch n'est bumpée QUE par les remplacements de jeu ─
test_that("mod_bulk.R construit l'état partagé via la fabrique et purge sur bulk_obj_epoch", {
  src <- .ts_src("modules/bulk/mod_bulk.R")
  expect_match(src, "shared_rv <- create_bulk_shared_state()", fixed = TRUE)
  expect_match(src, "observeEvent(global_data$bulk_obj_epoch", fixed = TRUE)
  expect_match(src, "bulk_purge_shared_results(shared_rv)", fixed = TRUE)
})

test_that("l'epoch est bumpée aux SEULS remplacements de dataset (imports/GEO/fusion)", {
  bump <- "bulk_obj_epoch <-"  # l'ASSIGNATION (le token nu apparaît 2× par site : écriture + lecture)
  imp  <- .ts_src("modules/import/mod_import_bulk.R")
  # 3 points d'import bulk : widget matrice, one file per sample, import piloté
  # par l'agent (import_file, G3) — aligné sur le contrat .register_multi_dataset.
  expect_identical(lengths(gregexpr(bump, imp, fixed = TRUE)), 3L)
  expect_match(.ts_src("modules/import/mod_geo.R"), bump, fixed = TRUE)
  expect_match(.ts_src("modules/bulk/mod_bulk_merge.R"), bump, fixed = TRUE)

  # Les re-commits de même lignée (provenance) ne bumpent PAS — sinon chaque
  # étape effacerait les résultats qu'elle vient de calculer. L'activation du
  # conteneur (mod_bulk_datasets.R) est couverte par le garde du contrat
  # BULK_MULTI (test-bulk-multi-contract-freeze.R).
  for (f in c("modules/bulk/mod_bulk_filter.R",
              "modules/bulk/mod_bulk_pathways.R",
              "modules/bulk/mod_bulk_signatures.R",
              "modules/bulk/mod_bulk_wgcna.R",
              "modules/bulk/mod_bulk_survival.R",
              "modules/bulk/mod_bulk_mapping.R")) {
    expect_false(grepl(bump, .ts_src(f), fixed = TRUE),
                 info = paste(bump, "ne doit PAS apparaître dans :", f))
  }
})

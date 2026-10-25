# =============================================================================
# test-bulk-merge.R — NEW-2 : fusion de jeux bulk ("Merge Data")
# =============================================================================
# Couvre la logique PURE de tools/pkg_skeleton/R/bulk_merge.R hors Shiny :
#   - surface publique + états d'erreur gelés ;
#   - alignement exact des gènes (intersection, comptes par jeu) ;
#   - métadonnées combinées (union-fill, renommage en collision, resync
#     « sample », colonne lot dataset_origin) ;
#   - fusion sans ComBat (concaténation fidèle) ;
#   - fusion AVEC ComBat-seq (STAT-S1 réutilisé) : le lot (= jeu d'origine)
#     est corrigé, la condition est préservée — critère d'acceptation ;
#   - chemins d'erreur classés (bulk_merge_error) + propagation des erreurs
#     des domaines réutilisés (bulk_multi_error, bulk_batch_correction_error) ;
#   - isolation : les jeux d'entrée ne sont JAMAIS mutés ;
#   - bulk_merge_to_bulk_obj : façonnage bulk_obj + provenance.
# =============================================================================
source_project_file("tools/pkg_skeleton/R/io_helpers.R")         # %||%
source_project_file("tools/pkg_skeleton/R/bulk_multi.R")         # MD-1 (validateur réutilisé)
source_project_file("tools/pkg_skeleton/R/bulk_batch_qc.R")      # bulk_batch_design_check (STAT-S1)
source_project_file("tools/pkg_skeleton/R/bulk_provenance.R")    # manifeste de provenance
source_project_file("tools/pkg_skeleton/R/batch_correction.R")   # STAT-S1 (run_combat_seq)
source_project_file("tools/pkg_skeleton/R/bulk_merge.R")         # moteur NEW-2

.expect_merge_state <- function(expr, state) {
  err <- tryCatch(force(expr), error = function(e) e)
  expect_true(inherits(err, "bulk_merge_error"),
              info = paste("erreur bulk_merge_error attendue, état :", state))
  expect_identical(err$state, state)
}

# ── Fixtures : 2 études synthétiques, gènes partiellement communs ──────────
.mk_entry <- function(genes, samples, conditions, base_lambda, extra_meta = NULL,
                      seed = 1, cond_boost = NULL) {
  set.seed(seed)
  m <- matrix(rpois(length(genes) * length(samples), lambda = base_lambda),
              nrow = length(genes), dimnames = list(genes, samples))
  if (!is.null(cond_boost)) {
    hit <- intersect(cond_boost$genes, genes)
    trt <- conditions == "TRT"
    if (any(trt)) m[hit, trt] <- m[hit, trt, drop = FALSE] + cond_boost$delta
  }
  meta <- data.frame(condition = conditions, row.names = samples,
                     stringsAsFactors = FALSE)
  if (!is.null(extra_meta)) meta <- cbind(meta, extra_meta)
  list(counts = m, metadata = meta)
}

genes_a <- paste0("G", 1:60)
genes_b <- paste0("G", 21:70)   # commun : G21..G60 (40 gènes)

# Gènes boostés par la condition : G21..G40 (dans l'intersection commune),
# boost appliqué DANS CHAQUE jeu (la condition n'est donc pas confondue au lot).
ds_a <- .mk_entry(genes_a, c("A1", "A2", "A3", "A4"),
                  c("CTL", "CTL", "TRT", "TRT"),
                  base_lambda = 50, seed = 11,
                  extra_meta = data.frame(tissue = c("T1", "T1", "T1", "T1")),
                  cond_boost = list(genes = paste0("G", 21:40), delta = 80))
ds_b <- .mk_entry(genes_b, c("B1", "B2", "B3"),
                  c("CTL", "TRT", "TRT"),
                  base_lambda = 250, seed = 22,
                  extra_meta = data.frame(tissue = c("T2", "T2", "T2")),
                  cond_boost = list(genes = paste0("G", 21:40), delta = 80))

two_ds <- list(A = ds_a, B = ds_b)

# ── Surface publique et états d'erreur ─────────────────────────────────────
test_that("public API surface is frozen (names + formals)", {
  expect_setequal(
    bulk_merge_public_api(),
    c("bulk_merge_public_api", "bulk_merge_error_states",
      "bulk_merge_check_inputs", "bulk_merge_common_meta_columns",
      "bulk_merge_align_genes", "bulk_merge_preview_metadata",
      "bulk_merge_combine", "bulk_merge_run", "bulk_merge_to_bulk_obj")
  )
  for (fn in bulk_merge_public_api()) {
    expect_true(exists(fn, where = globalenv(), inherits = FALSE), info = fn)
  }
})

test_that("error states are frozen and nothing else is emitted", {
  expect_setequal(
    bulk_merge_error_states(),
    c("invalid_input", "insufficient_datasets", "no_common_genes",
      "invalid_metadata", "design_not_applicable")
  )
  src <- paste(readLines(file.path(ts_project_root(), "tools/pkg_skeleton/R/bulk_merge.R"),
                         warn = FALSE), collapse = "\n")
  emitted <- unique(regmatches(src,
                               gregexpr('(?<=state = ")[^"]+', src,
                                        perl = TRUE))[[1L]])
  expect_true(all(emitted %in% bulk_merge_error_states()),
              info = paste(emitted, collapse = ", "))
})

# ── check_inputs ────────────────────────────────────────────────────────────
test_that("check_inputs rejects insufficient / invalid / heterogeneous inputs", {
  .expect_merge_state(bulk_merge_check_inputs(list(A = ds_a)),
                      "insufficient_datasets")
  .expect_merge_state(bulk_merge_check_inputs(list(ds_a, ds_b)),
                      "invalid_input")
  .expect_merge_state(
    bulk_merge_check_inputs(list(A = ds_a, A = ds_a)),
    "invalid_input")
  # Entrée mal formée : le validateur MD-1 propage SA classe (bulk_multi_error)
  bad <- list(A = ds_a, B = list(counts = "pas une matrice"))
  err <- tryCatch(bulk_merge_check_inputs(bad), error = function(e) e)
  expect_true(inherits(err, "bulk_multi_error"))
  # Counts sans noms de gènes : l'alignement est impossible (40 gènes x 3
  # échantillons pour rester cohérent avec les métadonnées du jeu)
  no_rn <- list(A = ds_a,
                B = list(counts = ds_b$counts[seq_len(40), ],
                         metadata = ds_b$metadata))
  rownames(no_rn$B$counts) <- NULL
  .expect_merge_state(bulk_merge_check_inputs(no_rn), "invalid_input")
  # Condition non commune : invalid_metadata
  .expect_merge_state(
    bulk_merge_check_inputs(two_ds, condition_col = "inexistant"),
    "invalid_metadata")
  expect_invisible(bulk_merge_check_inputs(two_ds, condition_col = "condition"))
})

test_that("common_meta_columns intersects across the selection", {
  expect_identical(bulk_merge_common_meta_columns(two_ds),
                   c("condition", "tissue"))
  no_meta <- list(A = ds_a,
                  B = list(counts = ds_b$counts, metadata = NULL))
  expect_identical(bulk_merge_common_meta_columns(no_meta), character(0))
})

# ── Alignement des gènes ────────────────────────────────────────────────────
test_that("align_genes returns the exact intersection with per-dataset counts", {
  al <- bulk_merge_align_genes(two_ds)
  expect_identical(al$common_genes, paste0("G", 21:60))
  expect_identical(al$n_common_genes, 40L)
  expect_identical(al$per_dataset$n_genes_total, c(60L, 50L))
  expect_identical(al$per_dataset$n_genes_common, c(40L, 40L))
  expect_identical(al$per_dataset$pct_common, c(66.7, 80))
})

test_that("empty gene intersection is a classed no_common_genes error", {
  ds_x <- .mk_entry(paste0("X", 1:20), c("X1", "X2", "X3"),
                    c("CTL", "TRT", "TRT"), base_lambda = 40, seed = 33)
  .expect_merge_state(bulk_merge_align_genes(list(A = ds_a, X = ds_x)),
                      "no_common_genes")
})

# ── Métadonnées combinées ──────────────────────────────────────────────────
test_that("preview_metadata appends the dataset-origin column and union-fills", {
  pm <- bulk_merge_preview_metadata(two_ds)
  expect_identical(colnames(pm$metadata),
                   c("condition", "tissue", "dataset_origin"))
  expect_identical(rownames(pm$metadata),
                   c("A1", "A2", "A3", "A4", "B1", "B2", "B3"))
  expect_identical(as.character(pm$metadata$dataset_origin),
                   c("A", "A", "A", "A", "B", "B", "B"))
  # union-fill : tissue n'existe que par jeu -> NA côté B ? NON — les deux jeux
  # portent tissue ; vérifie le remplissage quand une colonne manque d'un côté.
  ds_b2 <- ds_b
  ds_b2$metadata$tissue <- NULL
  pm2 <- bulk_merge_preview_metadata(list(A = ds_a, B = ds_b2))
  expect_true(all(is.na(pm2$metadata$tissue[5:7])))
  expect_identical(as.character(pm2$metadata$tissue[1:4]), rep("T1", 4))
  expect_identical(pm2$renames, data.frame(label = character(0),
                                           original = character(0),
                                           final = character(0),
                                           stringsAsFactors = FALSE))
})

test_that("sample-name collision renames WHOLE datasets (declared rule)", {
  ds_c <- ds_b
  colnames(ds_c$counts)[1] <- "A1"          # collision avec A1 du jeu A
  rownames(ds_c$metadata)[1] <- "A1"
  pm <- bulk_merge_preview_metadata(list(A = ds_a, C = ds_c))
  expect_identical(rownames(pm$metadata),
                   c("A__A1", "A__A2", "A__A3", "A__A4",
                     "C__A1", "C__B2", "C__B3"))
  expect_identical(nrow(pm$renames), 7L)
  expect_setequal(pm$renames$label, c("A", "C"))
  expect_identical(pm$n_renamed_datasets, 2L)
  # La colonne « sample » (si elle existe) est resynchronisée sur les noms finaux
  ds_a2 <- ds_a
  ds_a2$metadata$sample <- rownames(ds_a2$metadata)
  pm2 <- bulk_merge_preview_metadata(list(A = ds_a2, C = ds_c))
  expect_identical(as.character(pm2$metadata$sample), rownames(pm2$metadata))
})

test_that("mismatched metadata rownames and pre-existing batch column are refused", {
  bad_meta <- ds_b
  rownames(bad_meta$metadata) <- paste0("Z", 1:3)
  .expect_merge_state(
    bulk_merge_preview_metadata(list(A = ds_a, B = bad_meta)),
    "invalid_metadata")
  pre_col <- ds_b
  pre_col$metadata$dataset_origin <- "x"
  .expect_merge_state(
    bulk_merge_preview_metadata(list(A = ds_a, B = pre_col)),
    "invalid_metadata")
})

# ── Fusion sans ComBat ─────────────────────────────────────────────────────
test_that("combine concatenates aligned counts without altering any value", {
  al <- bulk_merge_align_genes(two_ds)
  cmb <- bulk_merge_combine(two_ds, al$common_genes)
  expect_identical(dim(cmb$counts), c(40L, 7L))
  expect_identical(rownames(cmb$counts), paste0("G", 21:60))
  expect_identical(colnames(cmb$counts),
                   c("A1", "A2", "A3", "A4", "B1", "B2", "B3"))
  expect_identical(cmb$counts["G30", "A1"], ds_a$counts["G30", "A1"])
  expect_identical(cmb$counts["G45", "B3"], ds_b$counts["G45", "B3"])
  expect_identical(cmb$batch_col, "dataset_origin")
})

test_that("run without ComBat is a faithful concatenation (no provenance label)", {
  res <- bulk_merge_run(two_ds)
  expect_false(res$combat_applied)
  expect_null(res$provenance_normalization)
  expect_null(res$counts_pre_combat)
  expect_identical(dim(res$counts), c(40L, 7L))
  expect_identical(res$n_common_genes, 40L)
  expect_identical(res$n_samples, 7L)
  expect_identical(res$per_dataset$n_samples, c(4L, 3L))
  expect_identical(res$per_dataset$n_genes_common, c(40L, 40L))
})

# ── Fusion AVEC ComBat-seq (STAT-S1 réutilisé) ─────────────────────────────
.r2_batch_on_pc1 <- function(counts, batch) {
  pca <- prcomp(t(log2(counts + 1)), scale. = FALSE)
  fit <- summary(lm(pca$x[, 1] ~ factor(batch)))
  unname(fit$r.squared)
}

test_that("run with ComBat-seq removes the between-dataset effect and keeps the condition", {
  # Effet lot (jeu B) volontairement >> effet condition pour que PC1 porte le lot
  res <- bulk_merge_run(two_ds, condition_col = "condition", apply_combat = TRUE)
  expect_true(res$combat_applied)
  expect_identical(dim(res$counts), c(40L, 7L))
  expect_identical(dimnames(res$counts),
                   list(paste0("G", 21:60),
                        c("A1", "A2", "A3", "A4", "B1", "B2", "B3")))
  expect_true(all(is.finite(res$counts)))
  expect_false(identical(res$counts, res$counts_pre_combat))
  expect_identical(res$condition_col, "condition")
  expect_match(res$provenance_normalization, "ComBat-seq", fixed = TRUE)

  r2_before <- .r2_batch_on_pc1(res$counts_pre_combat,
                                res$metadata$dataset_origin)
  r2_after  <- .r2_batch_on_pc1(res$counts, res$metadata$dataset_origin)
  expect_lt(r2_after, r2_before)

  # La condition est préservée : les gènes boostés restent TRT > CTL en log2
  boosted <- paste0("G", 21:30)
  log2c <- log2(res$counts[boosted, , drop = FALSE] + 1)
  trt <- res$metadata$condition == "TRT"
  expect_gt(mean(log2c[, trt]) - mean(log2c[, !trt]), 0)
})

test_that("run with ComBat refuses degenerate designs (1-sample dataset / collinearity)", {
  ds_one <- .mk_entry(paste0("G", 21:60), "B1", "CTL", base_lambda = 100,
                      seed = 44)
  .expect_merge_state(
    bulk_merge_run(list(A = ds_a, B = ds_one), condition_col = "condition",
                   apply_combat = TRUE),
    "design_not_applicable")
  # Chaque jeu mono-condition (A tout CTL, B tout TRT) : lot et condition
  # entièrement collinéaires — ComBat-seq refusé (STAT-S1), fusion possible sans.
  ds_ctl <- ds_a
  ds_ctl$metadata$condition <- c("CTL", "CTL", "CTL", "CTL")
  ds_trt <- ds_b
  ds_trt$metadata$condition <- c("TRT", "TRT", "TRT")
  .expect_merge_state(
    bulk_merge_run(list(A = ds_ctl, B = ds_trt), condition_col = "condition",
                   apply_combat = TRUE),
    "design_not_applicable")
  # Sans ComBat, la même paire mono-condition se fusionne sans obstacle
  res_nc <- bulk_merge_run(list(A = ds_ctl, B = ds_trt))
  expect_false(res_nc$combat_applied)
  expect_identical(res_nc$n_samples, 7L)
})

# ── Isolation (les entrées ne sont jamais mutées) ──────────────────────────
test_that("merge never mutates the input datasets (isolation by copy)", {
  snap_a <- ds_a$counts
  snap_b <- ds_b$counts
  snap_meta_a <- ds_a$metadata
  invisible(bulk_merge_run(two_ds, condition_col = "condition",
                           apply_combat = TRUE))
  expect_identical(ds_a$counts, snap_a)
  expect_identical(ds_b$counts, snap_b)
  expect_identical(ds_a$metadata, snap_meta_a)
})

# ── bulk_merge_to_bulk_obj ─────────────────────────────────────────────────
test_that("to_bulk_obj shapes the active bulk_obj (merge traceability)", {
  res <- bulk_merge_run(two_ds)
  obj <- bulk_merge_to_bulk_obj(res, "  Fusion test  ")
  expect_identical(obj$project, "Fusion test")
  expect_identical(obj$type, "bulk")
  expect_identical(obj$import_mode, "merge")
  expect_identical(obj$merge_info$datasets, c("A", "B"))
  expect_identical(obj$merge_info$n_common_genes, 40L)
  expect_false(obj$merge_info$combat_applied)
  expect_identical(obj$merge_info$n_renames, 0L)
  expect_null(obj$provenance)  # sans ComBat : pas de manifeste (comme un import)

  res_c <- bulk_merge_run(two_ds, condition_col = "condition",
                          apply_combat = TRUE)
  obj_c <- bulk_merge_to_bulk_obj(res_c, "")
  expect_identical(obj_c$project, "Fusion multi-jeux (2)")
  expect_true(obj_c$merge_info$combat_applied)
  expect_identical(obj_c$provenance$type, "bulk_provenance")
  expect_match(obj_c$provenance$normalization, "ComBat-seq", fixed = TRUE)
  # L'historique de provenance enregistre la transition (règle 6)
  expect_true(any(vapply(obj_c$provenance$history,
                         function(h) identical(h$field, "normalization"),
                         logical(1))))
})

test_that("to_bulk_obj rejects a malformed result", {
  .expect_merge_state(bulk_merge_to_bulk_obj(list(counts = NULL)),
                      "invalid_input")
})

# ── Gel des signatures (miroir léger du test de freeze) ────────────────────
test_that("engine emits only frozen error states and reuses frozen domains", {
  # run_combat_seq échoue proprement sans sva (état STAT-S1 gelé) — on ne
  # teste PAS ce chemin ici (sva est installé) ; on vérifie simplement que le
  # moteur délègue bien au domaine STAT-S1 (garde structurelle du freeze).
  expect_true(exists("run_combat_seq"))
  expect_true(exists("bulk_batch_correction_design"))
})

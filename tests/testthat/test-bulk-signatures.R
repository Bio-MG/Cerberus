# =============================================================================
# test-bulk-signatures.R — Signatures cellulaires (Bulk V2 M3)
# =============================================================================
# Couvre : ressources locales (disponibilité par package, jamais réseau),
# chargement Hallmark/RDS (liste + data.frame), disclaimer EMBARQUÉ dans le
# résultat et l'export (garde §M3), moteur M2 réutilisé (ssgsea/gsva/zscore)
# + decoupleR ulm (gènes en ROWS), re-classage des erreurs du domaine.
# =============================================================================
source_project_file("tools/pkg_skeleton/R/validation.R")
source_project_file("tools/pkg_skeleton/R/provenance.R")
# D2 M4 (2026-10-25) : R/plotting vit desormais dans l'arbre du paquet
# (tools/pkg_skeleton/R/) — charge via load_project_pkg() (cf. helper-source.R).
load_project_pkg()
source_project_file("tools/pkg_skeleton/R/bulk_batch_qc.R")
source_project_file("tools/pkg_skeleton/R/bulk_gsva.R")
source_project_file("tools/pkg_skeleton/R/bulk_gene_sets.R")
source_project_file("tools/pkg_skeleton/R/bulk_signatures.R")

.vst_m3 <- function(genes = 80, samples = 8, seed = 1) {
  set.seed(seed)
  matrix(rnorm(genes * samples), genes, samples,
         dimnames = list(paste0("GENE", seq_len(genes)),
                         paste0("s", seq_len(samples)))) + 8
}

test_that("surface publique figée + garde disclaimer contractuelle", {
  expect_setequal(
    bulk_signatures_public_api(),
    c("bulk_signatures_public_api", "bulk_signature_resources",
      "bulk_load_signatures", "bulk_score_signatures",
      "build_signature_scores_export")
  )
  expect_match(BULK_SIGNATURES_DISCLAIMER, "relatifs")
  expect_match(BULK_SIGNATURES_DISCLAIMER, "cytom")
  res_tbl <- bulk_signature_resources()
  expect_setequal(res_tbl$resource, c("hallmark", "progeny", "dorothea", "rds_local"))
  expect_true(is.logical(res_tbl$available) && length(res_tbl$available) == 4L)
  # rds_local toujours disponible (aucune dépendance)
  expect_true(res_tbl$available[res_tbl$resource == "rds_local"])
})

test_that("chargement RDS local : liste nommée et data.frame signature/gene", {
  rds <- file.path(tempdir(), paste0("sig_", Sys.getpid(), ".rds"))
  on.exit(unlink(rds), add = TRUE)
  saveRDS(list(SIG_A = paste0("GENE", 1:30), SIG_B = paste0("GENE", 21:50)), rds)
  sets <- bulk_load_signatures("rds_local", rds_path = rds)
  expect_setequal(names(sets), c("SIG_A", "SIG_B"))

  saveRDS(data.frame(signature = c("S1", "S1", "S2"), gene = c("a", "b", "c")), rds)
  sets2 <- bulk_load_signatures("rds_local", rds_path = rds)
  expect_setequal(names(sets2), c("S1", "S2"))

  # erreurs classées : fichier absent, contenu invalide
  e1 <- tryCatch(bulk_load_signatures("rds_local", rds_path = "absent.rds"),
                 error = function(e) e)
  expect_s3_class(e1, "bulk_signatures_error")
  expect_identical(e1$state, "invalid_input")
  saveRDS(matrix(1:4, 2), rds)
  e2 <- tryCatch(bulk_load_signatures("rds_local", rds_path = rds),
                 error = function(e) e)
  expect_identical(e2$state, "invalid_input")
})

test_that("ressource indisponible : erreur classée missing_dependency, jamais réseau", {
  res_tbl <- bulk_signature_resources()
  for (r in c("progeny", "dorothea")) {
    if (!res_tbl$available[res_tbl$resource == r]) {
      e <- tryCatch(bulk_load_signatures(r), error = function(e) e)
      expect_s3_class(e, "bulk_signatures_error")
      expect_identical(e$state, "missing_dependency")
      expect_match(conditionMessage(e), "aucun t\\u00e9l\\u00e9chargement|t[ée]l[ée]chargement")
    }
  }
  e_unk <- tryCatch(bulk_load_signatures("inexistant"), error = function(e) e)
  expect_identical(e_unk$state, "invalid_input")
})

test_that("Hallmark chargé depuis msigdbr local (50 ensembles)", {
  if (!requireNamespace("msigdbr", quietly = TRUE)) {
    skip("msigdbr absent")
  }
  sets <- bulk_load_signatures("hallmark", organism = "human")
  expect_true(length(sets) >= 40L)
  expect_true(all(grepl("^HALLMARK_", names(sets))))
  # erreur classée si organisme sans traduction msigdbr
})

test_that("scoring : disclaimer EMBARQUÉ, dims voies x échantillons, qc complet", {
  rds <- file.path(tempdir(), paste0("sig2_", Sys.getpid(), ".rds"))
  on.exit(unlink(rds), add = TRUE)
  saveRDS(list(SIG_A = paste0("GENE", 1:30), SIG_B = paste0("GENE", 21:50)), rds)
  sets <- bulk_load_signatures("rds_local", rds_path = rds)
  res <- bulk_score_signatures(.vst_m3(), sets, method = "ssgsea",
                               min_size = 5, max_size = 100)
  expect_identical(res$type, "bulk_signature_scores")
  expect_identical(res$status, "valid")
  expect_identical(dim(res$scores), c(2L, 8L))
  expect_identical(res$disclaimer, BULK_SIGNATURES_DISCLAIMER)
  expect_setequal(names(res$qc), c("n_input_sets", "n_used_sets", "dropped",
                                   "n_genes_input", "n_genes_matched", "n_samples"))
  expect_match(res$provenance$method, "GSVA::")
  expect_match(res$provenance$parameters$disclaimer, "relatifs")
})

test_that("scoring decoupleR ulm : gènes en ROWS, résultat équivalent en dims", {
  if (!requireNamespace("decoupleR", quietly = TRUE)) {
    skip("decoupleR absent")
  }
  rds <- file.path(tempdir(), paste0("sig3_", Sys.getpid(), ".rds"))
  on.exit(unlink(rds), add = TRUE)
  saveRDS(list(SIG_A = paste0("GENE", 1:30), SIG_B = paste0("GENE", 21:50)), rds)
  sets <- bulk_load_signatures("rds_local", rds_path = rds)
  res <- bulk_score_signatures(.vst_m3(), sets, method = "ulm_decoupleR")
  expect_identical(dim(res$scores), c(2L, 8L))
  expect_identical(rownames(res$scores), c("SIG_A", "SIG_B"))
  expect_identical(colnames(res$scores), paste0("s", 1:8))
  expect_identical(res$disclaimer, BULK_SIGNATURES_DISCLAIMER)
})

test_that("erreurs : méthode inconnue, signatures vides, re-classage du domaine", {
  rds <- file.path(tempdir(), paste0("sig4_", Sys.getpid(), ".rds"))
  on.exit(unlink(rds), add = TRUE)
  saveRDS(list(SIG_A = paste0("GENE", 1:30)), rds)
  sets <- bulk_load_signatures("rds_local", rds_path = rds)

  e1 <- tryCatch(bulk_score_signatures(.vst_m3(), sets, method = "nope"),
                 error = function(e) e)
  expect_s3_class(e1, "bulk_signatures_error")
  expect_identical(e1$state, "invalid_input")

  e2 <- tryCatch(bulk_score_signatures(.vst_m3(), list()), error = function(e) e)
  expect_identical(e2$state, "invalid_input")

  # counts bruts : garde M2 réutilisée, re-classée bulk_signatures_error
  set.seed(2)
  counts <- matrix(rpois(50 * 8, lambda = 300), 50, 8,
                   dimnames = list(paste0("G", 1:50), paste0("s", 1:8)))
  e3 <- tryCatch(bulk_score_signatures(counts, sets), error = function(e) e)
  expect_s3_class(e3, "bulk_signatures_error")
  expect_identical(e3$state, "raw_counts_rejected")

  e4 <- tryCatch(build_signature_scores_export(list()), error = function(e) e)
  expect_s3_class(e4, "bulk_signatures_error")
})

test_that("export : colonne disclaimer sur CHAQUE ligne (garde §M3)", {
  rds <- file.path(tempdir(), paste0("sig5_", Sys.getpid(), ".rds"))
  on.exit(unlink(rds), add = TRUE)
  saveRDS(list(SIG_A = paste0("GENE", 1:30)), rds)
  sets <- bulk_load_signatures("rds_local", rds_path = rds)
  res <- bulk_score_signatures(.vst_m3(), sets, method = "ssgsea",
                               min_size = 5, max_size = 100)
  ex <- build_signature_scores_export(res)
  expect_setequal(colnames(ex), c("signature", "sample", "score", "method",
                                  "analysis_id", "disclaimer"))
  expect_true(all(ex$disclaimer == BULK_SIGNATURES_DISCLAIMER))
  expect_identical(nrow(ex), nrow(res$scores) * ncol(res$scores))
})

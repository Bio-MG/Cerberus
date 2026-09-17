# =============================================================================
# test-bulk-no-replicate.R — plan sature (n = p) : detection + mode exploratoire
# =============================================================================
# Contexte (mesuré le 2026-09-17 sur le renv du projet) : quand le design n'a
# aucun degre de liberte residuel, les TROIS moteurs echouent, mais chacun a sa
# maniere —
#   DESeq2  : estimateDispersionsGeneEst() refuse ("same number of samples and
#             coefficients to fit"), sans aucun contournement possible ;
#   edgeR   : estimateDisp() ne refuse PAS, il pose une dispersion NA
#             ("No residual df"), et glmQLFit() meurt ensuite ;
#   limma   : voom() passe, eBayes() echoue ("No residual degrees of freedom").
# D'ou une detection amont unique (design_saturation()) + un seul contournement
# tenable : edgeR avec dispersion IMPOSEE (BCV), declaree par l'utilisateur.
#
# Ces tests verrouillent (a) la detection, (b) le contraste, (c) le fait que le
# contournement produit une table complete, et (d) que les p-values qui en
# sortent dependent du BCV choisi — c'est la raison d'etre de l'attestation
# demandee a l'utilisateur.
# =============================================================================

source_project_file("R/core/io_helpers.R")
source_project_file("R/core/validation.R")
source_project_file("R/bulk/bulk_helpers.R")
source_project_file("R/bulk/batch_correction.R")

# ---------------------------------------------------------------------------
# design_saturation() / check_design_saturated()
# ---------------------------------------------------------------------------
test_that("design_saturation detects n == p (4 echantillons, 4 conditions)", {
  meta <- data.frame(row.names = paste0("s", 1:4),
                     condition = c("A", "B", "C", "D"))
  sat <- design_saturation(meta, "condition")
  expect_true(sat$saturated)
  expect_equal(sat$n, 4L)
  expect_equal(sat$p, 4L)   # intercept + 3 indicatrices
  expect_true(check_design_saturated(meta, "condition"))
})

test_that("design_saturation is FALSE for a replicated design", {
  meta <- data.frame(row.names = paste0("s", 1:4),
                     condition = c("A", "A", "B", "B"))
  sat <- design_saturation(meta, "condition")
  expect_false(sat$saturated)
  expect_equal(sat$n, 4L)
  expect_equal(sat$p, 2L)
})

test_that("design_saturation counts covariate columns in p", {
  # 3 echantillons, 2 groupes + 1 covariable continue => 3 x 3 : sature.
  meta <- data.frame(row.names = paste0("s", 1:3),
                     condition = c("A", "A", "B"),
                     age = c(30, 40, 50))
  sat <- design_saturation(meta, "condition", "age")
  expect_true(sat$saturated)
  expect_equal(sat$p, 3L)
})

test_that("design_saturation excludes incomplete cases from n", {
  meta <- data.frame(row.names = paste0("s", 1:5),
                     condition = c("A", "A", "B", "B", "C"),
                     age = c(30, 40, 50, 60, NA))
  sat <- design_saturation(meta, "condition", "age")
  expect_equal(sat$n, 4L)   # s5 est hors du modele
})

test_that("design_saturation is FALSE (never blocks) when it cannot decide", {
  meta <- data.frame(row.names = paste0("s", 1:4), condition = c("A", "A", "B", "B"))
  expect_false(check_design_saturated(meta, "colonne_absente"))
  expect_false(check_design_saturated(meta, "condition", "covariable_absente"))
  expect_false(design_saturation(NULL, "condition")$saturated)
  expect_false(design_saturation(meta, NA_character_)$saturated)
})

test_that("validate_bulk_design reports the saturated design", {
  meta <- data.frame(row.names = paste0("s", 1:4),
                     condition = c("A", "B", "C", "D"))
  problems <- validate_bulk_design(meta, "condition")
  expect_true(any(grepl("sans r[eé]plicat", problems, ignore.case = TRUE)))
  # Un plan replique ne doit PAS rapporter ce probleme.
  meta2 <- data.frame(row.names = paste0("s", 1:4), condition = c("A", "A", "B", "B"))
  expect_false(any(grepl("sans r[eé]plicat",
                         validate_bulk_design(meta2, "condition"), ignore.case = TRUE)))
})

# ---------------------------------------------------------------------------
# .edger_contrast_vector()
# ---------------------------------------------------------------------------
test_that(".edger_contrast_vector handles target, reference and intercept cases", {
  g    <- factor(c("A", "B", "C", "D"))
  mm   <- stats::model.matrix(~g)
  colnames(mm) <- sub("^g", "grp_keep", colnames(mm))
  # C vs A : A est l'intercepte (pas de colonne) => +1 sur grp_keepC
  expect_equal(.edger_contrast_vector(mm, "C", "A"), c(0, 0, 1, 0))
  # A vs C : -1 sur grp_keepC
  expect_equal(.edger_contrast_vector(mm, "A", "C"), c(0, 0, -1, 0))
  # C vs B : les deux ont une colonne
  expect_equal(.edger_contrast_vector(mm, "C", "B"), c(0, -1, 1, 0))
})

test_that(".edger_contrast_vector refuses a null contrast", {
  g  <- factor(c("A", "B", "C", "D"))
  mm <- stats::model.matrix(~g)
  colnames(mm) <- sub("^g", "grp_keep", colnames(mm))
  expect_error(.edger_contrast_vector(mm, "C", "C"), "m[êe]me niveau")
  expect_error(.edger_contrast_vector(mm, "ZZ", "YY"), "absents")
})

# ---------------------------------------------------------------------------
# run_edger_de(fixed_dispersion = ) — le contournement
# ---------------------------------------------------------------------------
.no_rep_fixture <- function(n_genes = 200L, seed = 11L) {
  set.seed(seed)
  counts <- matrix(stats::rnbinom(n_genes * 4, mu = 200, size = 4), nrow = n_genes,
                   dimnames = list(paste0("g", seq_len(n_genes)), paste0("s", 1:4)))
  meta <- data.frame(row.names = paste0("s", 1:4),
                     condition = c("A", "B", "C", "D"))
  list(counts = counts, meta = meta)
}

test_that("run_edger_de fails on a saturated design WITHOUT imposed dispersion", {
  skip_if_not_installed("edgeR")
  d <- .no_rep_fixture()
  # Le chemin historique : < 4 echantillons retenus (2 groupes x 1 replicat).
  expect_error(
    run_edger_de(d$counts, d$meta, "condition", "B", "A"),
    "Trop peu d'[ée]chantillons valides"
  )
})

test_that("run_edger_de with imposed dispersion returns a complete DE table", {
  skip_if_not_installed("edgeR")
  d   <- .no_rep_fixture()
  res <- run_edger_de(d$counts, d$meta, "condition", "C", "A", fixed_dispersion = 0.4^2)
  expect_true(is.data.frame(res) && nrow(res) == 200L)
  for (col in c("gene", "log2FoldChange", "pvalue", "padj")) {
    expect_true(col %in% colnames(res), info = paste("colonne manquante :", col))
  }
  expect_false(any(is.na(res$padj)))
  expect_false(any(is.na(res$log2FoldChange)))
  # Un contraste inverse doit donner des log2FC opposes.
  inv <- run_edger_de(d$counts, d$meta, "condition", "A", "C", fixed_dispersion = 0.4^2)
  inv <- inv[match(res$gene, inv$gene), ]
  expect_equal(res$log2FoldChange, -inv$log2FoldChange, tolerance = 1e-6)
})

test_that("run_edger_de rejects an invalid fixed_dispersion", {
  skip_if_not_installed("edgeR")
  d <- .no_rep_fixture()
  expect_error(run_edger_de(d$counts, d$meta, "condition", "B", "A", fixed_dispersion = 0))
  expect_error(run_edger_de(d$counts, d$meta, "condition", "B", "A", fixed_dispersion = -1))
  expect_error(run_edger_de(d$counts, d$meta, "condition", "B", "A",
                            fixed_dispersion = c(0.1, 0.2)))
})

test_that("the imposed dispersion drives how many genes come out significant", {
  skip_if_not_installed("edgeR")
  d <- .no_rep_fixture()
  n_sig <- function(bcv) {
    r <- run_edger_de(d$counts, d$meta, "condition", "C", "A", fixed_dispersion = bcv^2)
    sum(r$padj < 0.05, na.rm = TRUE)
  }
  # GARANTIE DU MODE EXPLORATOIRE : plus la dispersion imposee est grande,
  # moins on declare de genes significatifs. Les p-values suivent le BCV
  # choisi par l'utilisateur, elles ne sont pas apportees par les donnees.
  expect_true(n_sig(0.1) >= n_sig(1.0))
})

test_that("run_bulk_de_dispatch forwards fixed_dispersion to the edgeR engine", {
  skip_if_not_installed("edgeR")
  d   <- .no_rep_fixture()
  res <- run_bulk_de_dispatch("edger", d$counts, d$meta, "condition", "C", "A",
                              fixed_dispersion = 0.4^2)
  expect_true("padj" %in% colnames(res))
  expect_equal(nrow(res), 200L)
})

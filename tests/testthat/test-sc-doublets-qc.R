# =============================================================================
# test-sc-doublets-qc.R — Moteur M4 doublets & ambiant (owner M4, PROJECT.md:173)
# =============================================================================
# Tests unitaires du contrat PROJECT.md:140-149 (sc_filter_doublets_and_ambient)
# sur le même fixture Poisson que le test e2e tier1 (80x120) : signature,
# structure de retour, déterminisme, monotonie des cutoffs, invariants de
# provenance, JSON round-trip, erreurs classées. Le chemin ambiant avec moteur
# installé est skip sans SoupX/celda (même conditionnel que le test e2e F20-5).
#
# Sémantique d'exclusion mesurée (cf. en-tête sc_qc_doublets.R) : une cellule
# est exclue SI scDblFinder l'appelle « doublet » ET que son score brut dépasse
# STRICTEMENT le cutoff — le fixture Poisson homogène produit des scores plats
# (~0.5) et zéro appel au cutoff par défaut.
# =============================================================================

skip_if_not_installed("Seurat")
skip_if_not_installed("scDblFinder")
skip_if_not_installed("withr")
skip_if_not_installed("jsonlite")

source_project_file("tools/pkg_skeleton/R/io_helpers.R")    # %||%
source_project_file("tools/pkg_skeleton/R/state.R")
source_project_file("tools/pkg_skeleton/R/provenance.R")
source_project_file("tools/pkg_skeleton/R/sc_qc_doublets.R")

.m4u_make_obj <- function(n_genes = 120L, n_cells = 80L, seed = 42) {
  set.seed(seed)
  mat <- matrix(rpois(n_genes * n_cells, lambda = 3), nrow = n_genes, ncol = n_cells)
  rownames(mat) <- paste0("Gene-", seq_len(n_genes))
  colnames(mat) <- paste0("Cell-", seq_len(n_cells))
  suppressWarnings(Seurat::CreateSeuratObject(counts = mat))
}

.m4u_numeric_leaves <- function(x) {
  if (is.atomic(x)) return(if (is.numeric(x)) x else numeric(0))
  if (is.list(x)) return(unlist(lapply(x, .m4u_numeric_leaves), use.names = FALSE))
  numeric(0)
}

test_that("défauts contractuels PROJECT.md:146-147 lus depuis defaults.R", {
  expect_equal(TS_SC_QC_DOUBLET_SCORE_CUTOFF, 0.25)
  expect_equal(TS_SC_QC_MAX_CONTAMINATION, 0.20)
})

test_that("signature contractuelle : formals obj + params", {
  expect_true(exists("sc_filter_doublets_and_ambient", mode = "function"))
  expect_true("obj" %in% names(formals(sc_filter_doublets_and_ambient)))
  expect_true("params" %in% names(formals(sc_filter_doublets_and_ambient)))
})

test_that("retour : Seurat + qc_metrics + provenance, score per-cell pré-filtre fini", {
  obj <- .m4u_make_obj()
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42))
  expect_s4_class(res$obj, "Seurat")
  expect_true(is.list(res$qc_metrics))
  expect_true(is.list(res$provenance))
  s <- res$qc_metrics$doublet_score
  expect_type(s, "double")
  expect_length(s, ncol(obj))
  expect_true(all(is.finite(s)))
  expect_identical(names(s), colnames(obj))
})

test_that("déterminisme : scores et classes identiques à graine égale", {
  r1 <- sc_filter_doublets_and_ambient(.m4u_make_obj(), params = list(seed = 42))
  r2 <- sc_filter_doublets_and_ambient(.m4u_make_obj(), params = list(seed = 42))
  expect_identical(r1$qc_metrics$doublet_score, r2$qc_metrics$doublet_score)
  expect_identical(r1$qc_metrics$doublet_class, r2$qc_metrics$doublet_class)
})

test_that("scores invariants au cutoff (détection une fois, filtrage ensuite)", {
  obj <- .m4u_make_obj()
  r1 <- sc_filter_doublets_and_ambient(obj, params = list(doublet_score_cutoff = 0.25, seed = 42))
  r2 <- sc_filter_doublets_and_ambient(obj, params = list(doublet_score_cutoff = 0.5, seed = 42))
  expect_identical(r1$qc_metrics$doublet_score, r2$qc_metrics$doublet_score)
  expect_false(identical(r1$provenance$parameters$doublet_score_cutoff,
                         r2$provenance$parameters$doublet_score_cutoff))
})

test_that("monotonie + borne cutoff = 1 (exclusion stricte, jamais >=)", {
  obj <- .m4u_make_obj()
  r1 <- sc_filter_doublets_and_ambient(obj, params = list(doublet_score_cutoff = 0.25, seed = 42))
  r2 <- sc_filter_doublets_and_ambient(obj, params = list(doublet_score_cutoff = 1, seed = 42))
  n_exc1 <- length(as.character(r1$provenance$cells_excluded))
  n_exc2 <- length(as.character(r2$provenance$cells_excluded))
  expect_lte(n_exc2, n_exc1)
  expect_equal(n_exc2, 0L)
  expect_equal(ncol(r2$obj), ncol(obj))
})

test_that("cutoff = 0 : arithmétique cohérente, métriques finies", {
  obj <- .m4u_make_obj()
  r <- sc_filter_doublets_and_ambient(obj, params = list(doublet_score_cutoff = 0, seed = 42))
  expect_equal(length(as.character(r$provenance$cells_used)) +
                 length(as.character(r$provenance$cells_excluded)), ncol(obj))
  expect_equal(length(as.character(r$provenance$cells_used)), ncol(r$obj))
  expect_true(all(is.finite(.m4u_numeric_leaves(r$qc_metrics))))
})

test_that("invariants provenance : somme = entrée, used = ncol(res$obj), subset strict", {
  obj <- .m4u_make_obj()
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42))
  n_used <- length(as.character(res$provenance$cells_used))
  n_exc <- length(as.character(res$provenance$cells_excluded))
  expect_equal(n_used + n_exc, ncol(obj))
  expect_equal(n_used, ncol(res$obj))
  expect_true(all(colnames(res$obj) %in% colnames(obj)))
  excluded_barcodes <- setdiff(colnames(obj), colnames(res$obj))
  expect_false(any(excluded_barcodes %in% colnames(res$obj[["RNA"]]$counts)))
})

test_that("tags singlet/doublet dans meta.data de res$obj", {
  obj <- .m4u_make_obj()
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42))
  tags <- as.character(res$obj$scDblFinder.class)
  expect_length(tags, ncol(res$obj))
  expect_true(all(tolower(tags) %in% c("singlet", "doublet", "0", "1", "false", "true")))
})

test_that("provenance : analysis_id, method, écho des paramètres APPLIQUÉS (piège du 0)", {
  obj <- .m4u_make_obj()
  r1 <- sc_filter_doublets_and_ambient(
    obj, params = list(seed = 42, doublet_score_cutoff = 0.3, max_contamination = 0.15))
  expect_true(grepl("cerberus-doublets", as.character(r1$provenance$analysis_id), fixed = TRUE))
  expect_identical(as.character(r1$provenance$method), "scDblFinder")
  pp <- r1$provenance$parameters
  expect_equal(pp$doublet_score_cutoff, 0.3)
  expect_equal(pp$max_contamination, 0.15)
  expect_equal(pp$seed, 42)
  # Le 0 fourni n'est PAS écrasé par le défaut 0.20 (%||% préserve 0).
  r2 <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42, max_contamination = 0))
  expect_equal(r2$provenance$parameters$max_contamination, 0)
})

test_that("batch_var : écho exact quand consommée, scores complets", {
  obj <- .m4u_make_obj()
  obj@meta.data$sample <- rep(c("donor_A", "donor_B"), length.out = ncol(obj))
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42, batch_var = "sample"))
  expect_identical(as.character(res$provenance$parameters$batch_var), "sample")
  expect_length(res$qc_metrics$doublet_score, ncol(obj))
  expect_true(all(is.finite(res$qc_metrics$doublet_score)))
})

test_that("batch_var absente : avertissement tracé, PAS d'écho (valeurs réellement utilisées)", {
  obj <- .m4u_make_obj()
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42, batch_var = "inexistante"))
  expect_null(res$provenance$parameters$batch_var)
  expect_true(any(grepl("inexistante", as.character(res$provenance$warnings), fixed = TRUE)))
})

test_that("qc_metrics : distributions nCount/nFeature before ET after, feuilles finies", {
  obj <- .m4u_make_obj()
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42))
  flat <- unlist(res$qc_metrics, recursive = TRUE)
  expect_true(any(grepl("nCount_RNA", names(flat), ignore.case = TRUE) &
                    grepl("before|pre", names(flat), ignore.case = TRUE)))
  expect_true(any(grepl("nCount_RNA", names(flat), ignore.case = TRUE) &
                    grepl("after|post", names(flat), ignore.case = TRUE)))
  expect_true(any(grepl("nFeature_RNA", names(flat), ignore.case = TRUE) &
                    grepl("before|pre", names(flat), ignore.case = TRUE)))
  expect_true(any(grepl("nFeature_RNA", names(flat), ignore.case = TRUE) &
                    grepl("after|post", names(flat), ignore.case = TRUE)))
  leaves <- .m4u_numeric_leaves(res$qc_metrics)
  expect_gt(length(leaves), 0)
  expect_true(all(is.finite(leaves)))
})

test_that("provenance : JSON round-trip propre (auto_unbox)", {
  obj <- .m4u_make_obj()
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42))
  js <- jsonlite::toJSON(res$provenance, auto_unbox = TRUE)
  expect_type(js, "character")
  back <- jsonlite::fromJSON(as.character(js), simplifyVector = FALSE)
  expect_identical(as.character(back$method), "scDblFinder")
  expect_true(grepl("cerberus-doublets", as.character(back$analysis_id), fixed = TRUE))
  expect_false(is.null(back$parameters))
})

test_that("provenance_append accepte l'entrée du moteur dans un reactiveValues", {
  skip_if_not_installed("shiny")
  obj <- .m4u_make_obj()
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42))
  st <- shiny::reactiveValues()
  provenance_append(st, res$provenance)
  h <- state_get(st, "provenance")
  expect_length(h, 1)
  expect_identical(as.character(h[[1]]$analysis_id), "cerberus-doublets")
})

test_that("garde no-silent-fallback : run_ambient sans moteur => erreur classée", {
  skip_if(requireNamespace("SoupX", quietly = TRUE) || requireNamespace("celda", quietly = TRUE),
          message = "moteur ambiant installé — chemin du garde non atteignable")
  obj <- .m4u_make_obj()
  err <- tryCatch(
    sc_filter_doublets_and_ambient(obj, params = list(run_ambient = TRUE, seed = 42)),
    error = function(e) e)
  expect_s3_class(err, "error")
  expect_s3_class(err, "sc_qc_error")
  expect_true("sc_ambient_package_missing" %in% class(err))
  expect_true(grepl("ambient|decontaminat|SoupX|celda", conditionMessage(err), ignore.case = TRUE))
  expect_true(any(vapply(c("SoupX", "celda", "DecontX"),
                         function(p) grepl(p, conditionMessage(err), fixed = TRUE), logical(1))))
})

test_that("run_ambient absent : pas de garde, chemin doublet seul", {
  obj <- .m4u_make_obj()
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42))
  expect_false(isTRUE(res$provenance$parameters$run_ambient))
  expect_s4_class(res$obj, "Seurat")
})

test_that("paramètre hors bornes ou non numérique : erreur classée sc_qc_error", {
  obj <- .m4u_make_obj()
  err <- tryCatch(
    sc_filter_doublets_and_ambient(obj, params = list(doublet_score_cutoff = 2)),
    error = function(e) e)
  expect_s3_class(err, "sc_qc_error")
  err2 <- tryCatch(
    sc_filter_doublets_and_ambient(obj, params = list(max_contamination = "haute")),
    error = function(e) e)
  expect_s3_class(err2, "sc_qc_error")
})

test_that("objet non-Seurat : erreur classée sc_qc_error", {
  err <- tryCatch(
    sc_filter_doublets_and_ambient(matrix(0, 2, 2), params = list()),
    error = function(e) e)
  expect_s3_class(err, "sc_qc_error")
})

test_that("downstream-ready : NormalizeData ET RunPCA tournent directement sur res$obj (F24-3/F24-4)", {
  obj <- .m4u_make_obj()
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42))
  norm <- Seurat::NormalizeData(res$obj, verbose = FALSE)
  expect_identical(colnames(norm), colnames(res$obj))
  # Contrat F24-4 : RunPCA par défaut (layer "scale.data") sans préparation aval.
  pca <- Seurat::RunPCA(res$obj, npcs = 5, verbose = FALSE)
  emb <- Seurat::Embeddings(pca, "pca")
  expect_identical(rownames(emb), colnames(res$obj))
  expect_true(all(rownames(emb) %in% colnames(obj)))
  # Les counts restent intacts (dgCMatrix, layer séparé).
  counts <- res$obj[["RNA"]]$counts
  expect_s4_class(counts, "dgCMatrix")
})

test_that("doublets injectés : les exclusions sont des appels moteurs au-dessus du cutoff", {
  set.seed(42)
  n_genes <- 120L; n_cells <- 80L
  mat <- matrix(rpois(n_genes * n_cells, lambda = 3), nrow = n_genes, ncol = n_cells)
  dpairs <- combn(seq_len(40L), 2, simplify = FALSE)[seq_len(8L)]
  dmat <- sapply(dpairs, function(p) mat[, p[1]] + mat[, p[2]])
  mat2 <- cbind(mat, dmat)
  colnames(mat2) <- paste0("Cell-", seq_len(ncol(mat2)))
  obj <- suppressWarnings(Seurat::CreateSeuratObject(counts = mat2))
  res <- sc_filter_doublets_and_ambient(obj, params = list(seed = 42))
  excl <- as.character(res$provenance$cells_excluded)
  calls <- res$qc_metrics$doublet_class
  scores <- res$qc_metrics$doublet_score
  # Invariant structurel : toute cellule exclue est appelée doublet ET au-dessus
  # du cutoff (l'intersection mesurée, cf. en-tête du moteur).
  expect_true(all(calls[excl] == "doublet"))
  expect_true(all(scores[excl] > res$provenance$parameters$doublet_score_cutoff))
  # Sur signal réel, le moteur exclut au moins un appel (fixture injecté : 2/8
  # appelés, mesurés — les deux injectés).
  expect_true(length(excl) >= 1)
  # Le sous-ensemble downstream reste exécutable.
  norm <- Seurat::NormalizeData(res$obj, verbose = FALSE)
  expect_identical(colnames(norm), colnames(res$obj))
})

# =============================================================================
# test-sc-auto-pipeline.R — functional smoke test of the SC auto-pipeline
# =============================================================================
# Runs R/sc/sc_pipeline.R::run_sc_auto_pipeline END-TO-END on a tiny
# deterministic Seurat fixture (full-dataset path: QC → Normalize → PCA →
# clustering → UMAP → secondary t-SNE → FindAllMarkers → trajectory),
# driven through shiny:::withReactiveDomain(MockShinySession) so that
# Progress/showNotification/removeModal work without stubbing.
#
# Purpose (user mandate 2026-09-05): "check le fonctionnement de autopipeline
# sc". Zero production code is modified by this file — it is verification only.
# =============================================================================

source_project_file("tools/pkg_skeleton/R/io_helpers.R")   # %||%
source_project_file("tools/pkg_skeleton/R/state.R")        # create_sc_shared_state
source_project_file("tools/pkg_skeleton/R/sc_helpers.R")     # resolve_sketch_preset, robust_find_clusters, subsample_*
source_project_file("tools/pkg_skeleton/R/sc_bpcells.R")     # smart_scale_data, sc_backend_status
source_project_file("tools/pkg_skeleton/R/sc_trajectory.R")  # calculate_pseudotime
source_project_file("tools/pkg_skeleton/R/memory.R")       # preflight RAM (M-4) lu par le pipeline
source_project_file("tools/pkg_skeleton/R/sc_pipeline.R")    # run_sc_auto_pipeline
source_project_file("modules/sc/mod_sc_pipeline.R")     # .AUTO_TSNE_MAX_CELLS, .BPCELLS_AUTO_THRESHOLD
source_project_file("modules/sc/mod_sc_trajectory.R")   # .MAX_TRAJECTORY_CELLS

# .tr fallback (global.R defines it in the app; tests run without it).
# Assigned into globalenv because run_sc_auto_pipeline resolves .tr lexically
# there (sc_pipeline.R is sys.source'd into globalenv by helper-source.R).
if (!exists(".tr", envir = globalenv()))
  assign(".tr", function(key) key, envir = globalenv())

# ── Deterministic tiny fixture: 2 clusters, 2 samples, 3 MT genes ────────────
.make_tiny_sc_obj <- function(n_per_group = 150L, n_genes = 120L, seed = 42) {
  set.seed(seed)
  n_cells <- 2L * n_per_group
  mat <- matrix(rpois(n_genes * n_cells, lambda = 1),
                nrow = n_genes, dimnames = list(paste0("G", seq_len(n_genes)), NULL))
  # Group-specific marker blocks → two separable clusters.
  mat[1:15, seq_len(n_per_group)] <-
    mat[1:15, seq_len(n_per_group)] + rpois(15L * n_per_group, 8)
  mat[16:30, (n_per_group + 1L):n_cells] <-
    mat[16:30, (n_per_group + 1L):n_cells] + rpois(15L * n_per_group, 8)
  mt <- matrix(rpois(3L * n_cells, lambda = 1), nrow = 3L,
               dimnames = list(c("MT-A", "MT-B", "MT-C"), NULL))
  mat <- rbind(mat, mt)
  obj <- CreateSeuratObject(counts = mat)
  obj$orig.ident <- factor(rep(c("s1", "s2"), each = n_per_group))
  obj
}

# Default input mirroring the auto-pipeline modal (mod_sc.R).
.ap_default_input <- function() {
  list(
    sc_ap_mapping           = FALSE,
    sc_ap_bpcells           = FALSE,
    sc_ap_min_gene          = 10,
    sc_ap_max_gene          = 10000,
    sc_ap_mt                = 50,
    sc_ap_norm              = "log",
    sc_ap_pca_dim           = 10,
    sc_ap_res               = 0.5,
    sc_ap_cluster_algo      = "1",
    sc_ap_compute_umap      = TRUE,
    sc_ap_sketch_preset     = "max",   # ncells = n_total → full-dataset path
    sc_ap_sketch_ncells_custom = NA,
    sc_ap_singler           = FALSE,
    sc_ap_markers           = TRUE,
    sc_ap_pathway           = FALSE,   # needs online databases — out of scope here
    sc_ap_correlation       = FALSE,
    sc_ap_trajectory        = TRUE
  )
}

.ap_run <- function(input, sc_obj) {
  gd <- shiny::reactiveValues(sc_obj = sc_obj)
  shared_rv <- create_sc_shared_state()
  sc_log_rv <- shiny::reactiveVal("")
  mock <- shiny::MockShinySession$new()
  # isolate(): reactiveValues reads need a consumer context;
  # withReactiveDomain(): Progress/notifications need a session domain.
  # Roadmap 5.2 (2026-09-27) : run_sc_auto_pipeline RELANCE désormais son
  # erreur après journalisation/notification — .ap_run la capture pour
  # permettre les assertions de classe.
  err <- shiny::isolate(shiny:::withReactiveDomain(mock, {
    tryCatch(
      run_sc_auto_pipeline(input, gd, shared_rv, mock, sc_log_rv),
      error = function(e) e
    )
  }))
  list(global_data = gd, shared_rv = shared_rv, log = shiny::isolate(sc_log_rv()),
       error = err)
}

testthat::test_that("SC auto-pipeline runs end-to-end on a tiny fixture (full-dataset path)", {
  testthat::skip_if_not_installed("Seurat")
  library(Seurat)   # pipeline uses unqualified Seurat generics (NormalizeData, subset, ...)
  library(shiny)    # ... and unqualified shiny verbs (removeModal, showNotification)

  res <- .ap_run(.ap_default_input(), .make_tiny_sc_obj())

  # Log contains no failure marker and reaches the final steps.
  testthat::expect_false(grepl("\u274c", res$log), info = res$log)
  testthat::expect_match(res$log, "QC : ", fixed = TRUE)
  testthat::expect_match(res$log, "UMAP OK", fixed = TRUE)
  testthat::expect_match(res$log, "t-SNE secondaire OK", fixed = TRUE)

  obj <- shiny::isolate(res$global_data$sc_obj)
  testthat::expect_s4_class(obj, "Seurat")
  testthat::expect_true("seurat_clusters" %in% colnames(obj@meta.data))
  testthat::expect_setequal(c("pca", "umap", "tsne"), intersect(c("pca", "umap", "tsne"), names(obj@reductions)))

  # FindAllMarkers branch wrote canonical markers_data.
  markers <- shiny::isolate(res$shared_rv$markers_data)
  testthat::expect_false(is.null(markers))
  testthat::expect_gt(nrow(markers), 0)
  testthat::expect_true(all(c("gene", "cluster", "avg_log2FC") %in% colnames(markers)))

  # Trajectory branch wrote pseudotime + provenance (exploratory kNN).
  testthat::expect_true("pseudotime" %in% colnames(obj@meta.data))
  testthat::expect_true(all(obj@meta.data$traj_method == "exploratory_knn"))

  # Commit step switched the results tab.
  testthat::expect_identical(shiny::isolate(res$shared_rv$active_tab), "tab_viz")
})

testthat::test_that("SC auto-pipeline fails gracefully when QC removes nearly all cells", {
  testthat::skip_if_not_installed("Seurat")
  library(Seurat)
  library(shiny)

  obj <- .make_tiny_sc_obj()
  bad <- .ap_default_input()
  bad$sc_ap_min_gene <- 100000   # impossible threshold → < 10 cells after QC

  res <- .ap_run(bad, obj)

  # Gracious failure: the error is logged and the function returns without
  # propagating. M-2 phase 1 (2026-09-30): Seurat's raw "No cells found" is
  # now converted by sc_pipeline_qc into the classed French error (same key
  # as the "< 10 cells" guard) - the user gets an actionable message.
  testthat::expect_match(res$log, "Erreur: Seulement 0 cellule(s) apr", fixed = TRUE)
  # Original object untouched (commit never reached).
  testthat::expect_identical(dim(shiny::isolate(res$global_data$sc_obj)), dim(obj))
})

# ── Jalon filets (2026-10-27) : classification des erreurs de dépendance ─────
# Deux défauts épinglés en M5 (T2-QC-5 / T2-QC-6) : des erreurs de dépendance
# BRUTES sortaient du cœur (irlba quand npcs >= n_cellules ; FindClusters à
# haute résolution). Le filet de Step 2-5 les re-classifie `sc_pipeline_error`
# sans altérer le message d'origine ; une condition DÉJÀ classée par le dépôt
# est préservée telle quelle (doctrine F20 — jamais de re-classement).

testthat::test_that("core refuses cells <= pca_dim with a classed actionable error", {
  testthat::skip_if_not_installed("Seurat")
  library(Seurat)
  set.seed(42)
  m <- matrix(rpois(120L * 12L, lambda = 3), nrow = 120L, ncol = 12L,
              dimnames = list(paste0("Gene-", seq_len(120L)), paste0("Cell-", seq_len(12L))))
  obj <- suppressWarnings(CreateSeuratObject(counts = m))
  err <- tryCatch(run_sc_auto_pipeline_core(obj, params = list(run_qc = FALSE)),
                  error = function(e) e)
  testthat::expect_s3_class(err, "error")
  testthat::expect_true(inherits(err, "sc_pipeline_error"),
                        info = paste(class(err), collapse = ","))
  testthat::expect_match(conditionMessage(err), "cellule", ignore.case = TRUE)
  testthat::expect_match(conditionMessage(err), "pca_dim", ignore.case = TRUE)
})

testthat::test_that("core classes the raw FindClusters failure at extreme resolution", {
  testthat::skip_if_not_installed("Seurat")
  library(Seurat)
  set.seed(42)
  m <- matrix(rpois(120L * 80L, lambda = 3), nrow = 120L, ncol = 80L,
              dimnames = list(paste0("Gene-", seq_len(120L)), paste0("Cell-", seq_len(80L))))
  obj <- suppressWarnings(CreateSeuratObject(counts = m))
  err <- tryCatch(run_sc_auto_pipeline_core(obj, params = list(run_qc = FALSE, sc_ap_res = 3.0)),
                  error = function(e) e)
  testthat::expect_s3_class(err, "error")
  testthat::expect_true(inherits(err, "sc_pipeline_error"),
                        info = paste(class(err), collapse = ","))
  # Message de dépendance préservé in extenso derrière le préfixe du filet.
  testthat::expect_match(conditionMessage(err), "autopipeline", fixed = TRUE)
  testthat::expect_gt(nchar(conditionMessage(err)), nchar("autopipeline"))
})

testthat::test_that("the classification net preserves an already classed condition verbatim", {
  testthat::skip_if_not_installed("Seurat")
  origin <- errorCondition("moteur M4 classé — inchangé", class = "sc_ambient_package_missing")
  err <- tryCatch(.ap_classed_step(stop(origin)), error = function(e) e)
  testthat::expect_true(inherits(err, "sc_ambient_package_missing"))
  testthat::expect_identical(conditionMessage(err), "moteur M4 classé — inchangé")
  raw <- tryCatch(.ap_classed_step(stop("erreur brute de dépendance")),
                  error = function(e) e)
  testthat::expect_true(inherits(raw, "sc_pipeline_error"))
  testthat::expect_match(conditionMessage(raw), "erreur brute de dépendance", fixed = TRUE)
})

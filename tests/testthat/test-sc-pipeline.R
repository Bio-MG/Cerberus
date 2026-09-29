# Tests for R/sc/sc_pipeline.R
source_project_file("R/sc/sc_helpers.R")
source_project_file("R/sc/sc_pipeline.R")

test_that("resolve_sketch_preset returns valid structure", {
  for (preset in c("fast","light","medium","standard","high","max")) {
    result <- resolve_sketch_preset(preset, n_total_cells = 200000)
    expect_true(all(c("ncells","max_per_cluster","npcs") %in% names(result)))
    expect_true(result$ncells > 0)
    expect_true(result$npcs > 0)
  }
})

test_that("resolve_sketch_preset caps at n_total_cells", {
  result <- resolve_sketch_preset("high", n_total_cells = 5000)
  expect_equal(result$ncells, 5000)  # capped below 100000
})

# ---------------------------------------------------------------------------
# 25ᵉ incrément — REWRITE du verrou (roadmap 5.2, audit 2026-09-27 §1.9).
# Historiquement ce verrou INTERDISAIT le `stop(` dans le gestionnaire final :
# la classe `sc_pipeline_error` était inobservable (§2cd.2). Le contrat est
# INVERSÉ : le gestionnaire journalise ET notifie, puis RELANCE — l'erreur
# devient observable par les appelants (.sc_ap_run_drive marque le job en
# échec ; les observateurs UI enveloppent dans tryCatch). Le verrou source
# ci-dessous gèle la NOUVELLE propriété : journalisation + notification +
# relance. Les assertions de classe complètes restent dans
# test-sc-auto-pipeline.R (exécution e2e).
# ---------------------------------------------------------------------------

test_that("sc_pipeline.R ne contribue aucun signalement C10", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools/check_conventions.R"), envir = e)
  before <- length(e$.REPORT$warns)
  e$check_c10_error_style(file.path(ts_project_root(), "R/sc/sc_pipeline.R"))
  w <- e$.REPORT$warns
  n <- length(w) - before
  if (n > 0L) {
    lignes <- vapply(w[(before + 1L):length(w)], function(x) x$line, integer(1))
    cat("Sites C10 restants dans sc_pipeline.R :",
        paste(sort(lignes), collapse = ", "), "\n")
  }
  expect_equal(n, 0L)
})

test_that("run_sc_auto_pipeline journalise, notifie PUIS relance (erreur observable)", {
  src <- readLines(file.path(ts_project_root(), "R/sc/sc_pipeline.R"), warn = FALSE)
  h <- grep("error *= *function", src)
  expect_true(length(h) > 0L)
  # Le DERNIER gestionnaire du fichier est celui du tryCatch englobant (30->408).
  tail_src <- paste(src[seq_along(src) > max(h)], collapse = "\n")
  # Il journalise ET notifie…
  expect_true(grepl("log_sc\\(", tail_src))
  expect_true(grepl("showNotification\\(", tail_src))
  # …puis RELANCE : la classe sc_pipeline_error est observable (roadmap 5.2).
  expect_true(grepl("stop\\(e\\)", tail_src))
})

# =============================================================================
# SEED AND DETERMINISM (2026-09-26)
#
# WHY THE PRIMITIVES AND NOT `run_sc_auto_pipeline()`
#   The function itself is not runnable here, and the reason is measured two
#   paragraphs above: `removeModal()` and `Progress$new()` both fail outside a
#   Shiny session. So the unit under test is the chain of STOCHASTIC PRIMITIVES
#   the pipeline composes — `robust_find_clusters()` (sc_helpers.R:1193) then
#   `RunUMAP` / `RunTSNE` (sc_pipeline.R:197/203/225). If a primitive is not
#   reproducible, no composition of it is, and a full pipeline run would cost
#   minutes to establish the same fact less precisely.
#
# WHY THIS TEST EXISTS AT ALL, GIVEN THE PIPELINE IS ALREADY DETERMINISTIC
#   Because the determinism is INHERITED, not declared. Measured on Seurat 5.5.1:
#   passing no seed at all gives identical clusters and bit-identical embeddings
#   across runs AND across different ambient RNG states, while passing an
#   explicit `random.seed` / `seed` CHANGES the result (clusters differ; UMAP
#   max|delta| 3.7 to 5.3; t-SNE 22.6). So the guarantee is real and it is
#   currently a property of library defaults that this repository neither states
#   nor tests. A dependency bump - and Seurat has already moved 5.4.3 -> 5.5.1 in
#   this project's history - could change it with no failing test and no diff here
#   to explain why a figure moved. This block is that missing tripwire.
#
# ⚠️ IT SURVIVES THE FIX. Adding a fixed, declared seed keeps both properties
#   (same input -> same output, and independence from ambient state), so these
#   assertions stay true whether the pipeline keeps passing no seed or starts
#   passing a declared one. That is deliberate: a determinism test must not lock
#   in the CURRENT mechanism, only the guarantee.
# =============================================================================

.sc_det_fixture <- function(n_cell = 300L, n_gene = 200L, seed = 20260926L) {
  set.seed(seed)
  # Planted structure: three blocks of cells over the same features, so the
  # clustering has something to be right or wrong about. A Poisson soup is
  # cheaper and proves nothing - "both runs agree" on data with no signal is not
  # evidence of determinism.
  block <- rep(1:3, length.out = n_cell)
  dense <- matrix(stats::rpois(n_cell * n_gene, lambda = 1), nrow = n_gene,
                  dimnames = list(sprintf("G%03d", seq_len(n_gene)),
                                  sprintf("C%03d", seq_len(n_cell))))
  for (b in 1:3) {
    cols <- which(block == b)
    dense[1:20, cols] <- dense[1:20, cols] +
      matrix(stats::rpois(20 * length(cols), lambda = 6), nrow = 20)
  }
  # Sparse on purpose: a dense matrix makes Seurat emit "Data is of class matrix.
  # Coercing to dgCMatrix" on every call, and five warnings that say nothing about
  # determinism only make a real one harder to see. `Matrix(dense, sparse = TRUE)`
  # rather than `sparseMatrix(which(dense > 0), ...)`: the latter wants `i`+`j` or
  # `p`, and linear indices are none of those.
  counts <- Matrix::Matrix(dense, sparse = TRUE)
  o <- Seurat::CreateSeuratObject(counts)
  o <- Seurat::NormalizeData(o, verbose = FALSE)
  o <- Seurat::FindVariableFeatures(o, nfeatures = 100, verbose = FALSE)
  o <- Seurat::ScaleData(o, verbose = FALSE)
  o <- Seurat::RunPCA(o, npcs = 10, verbose = FALSE)
  Seurat::FindNeighbors(o, dims = 1:10, verbose = FALSE)
}

.sc_det_run <- function(obj, res = 0.8) {
  o <- robust_find_clusters(obj, resolution = res, algo = 1L)
  o <- Seurat::RunUMAP(o, dims = 1:10, verbose = FALSE)
  o <- Seurat::RunTSNE(o, dims = 1:10, verbose = FALSE)
  list(cl = as.integer(Seurat::Idents(o)),
       umap = as.matrix(Seurat::Embeddings(o, "umap")),
       tsne = as.matrix(Seurat::Embeddings(o, "tsne")))
}

test_that("the SC pipeline's stochastic steps are REPRODUCIBLE on the same input", {
  obj <- .sc_det_fixture()
  a <- .sc_det_run(obj)
  b <- .sc_det_run(obj)
  # The clustering must not merely be "similar": identical labelling, and a
  # non-trivial number of clusters so that a degenerate all-one-cluster result
  # cannot pass as determinism.
  expect_identical(a$cl, b$cl)
  expect_gt(length(unique(a$cl)), 1L)
  expect_identical(a$umap, b$umap)
  expect_identical(a$tsne, b$tsne)
})

test_that("and that reproducibility does NOT depend on the ambient RNG state", {
  # The half that a naive determinism test misses. Two runs in one session can
  # agree because the ambient state happens to realign; the guarantee a user
  # relies on is stronger - a fresh R session, a different machine, or any
  # package that consumed draws earlier must not move the result. MEASURED:
  # this holds, and it holds because no seed is passed, which makes the RNG
  # data-derived rather than ambient.
  obj <- .sc_det_fixture()
  set.seed(7L);    a <- .sc_det_run(obj)
  set.seed(999L);  b <- .sc_det_run(obj)
  set.seed(31337L); c <- .sc_det_run(obj)
  expect_identical(a$cl, b$cl)
  expect_identical(a$cl, c$cl)
  expect_identical(a$umap, b$umap)
  expect_identical(a$umap, c$umap)
  expect_identical(a$tsne, b$tsne)
  expect_identical(a$tsne, c$tsne)
})

test_that("adding a seed is a BEHAVIOUR CHANGE, not a free determinism fix", {
  # Executable form of the decision NOT to introduce a seed. The intuition that
  # "seeding makes it reproducible" is right in general and WRONG here: the
  # pipeline is already reproducible, and the seed is a real CONTROL of the
  # result, so passing one MOVES the embeddings. That is hard rule #1 (zero
  # behaviour change) territory and needs an explicit decision.
  #
  # The assertion is "TWO DIFFERENT SEEDS DISAGREE", not "the default differs from
  # 42". A first draft asserted the latter and failed, having measured
  # `default == seed 42` on Seurat 5.5.1 — i.e. it had accidentally picked the one
  # seed value that changes nothing. Which default a library happens to use is an
  # implementation detail nobody should pin; whether the seed is a real control is
  # the durable fact, and the durable fact is what implies a behaviour change.
  obj <- .sc_det_fixture()
  cl_of <- function(sd) as.integer(Seurat::Idents(Seurat::FindClusters(
    obj, resolution = 0.8, algorithm = 1L, verbose = FALSE, random.seed = sd)))
  expect_false(identical(cl_of(1L), cl_of(99L)))
  # The pipeline's own call, for the record: no `random.seed`, so the library
  # default applies and the result is data-derived.
  expect_false(identical(
    as.integer(Seurat::Idents(robust_find_clusters(obj, resolution = 0.8, algo = 1L))),
    cl_of(99L)))
  # And the embedding, which is what a reader would actually see move.
  o <- obj
  umap_of <- function(sd) as.matrix(Seurat::Embeddings(
    Seurat::RunUMAP(o, dims = 1:10, verbose = FALSE, seed = sd), "umap"))
  expect_gt(max(abs(umap_of(1L) - umap_of(99L))), 0)
})

test_that("the SC pipeline declares NO seed, and that gap is recorded, not silent", {
  # The counterpart of the block above: today the pipeline passes no seed, and
  # nothing in `config/` declares one either — unlike Milo (TS_DA_MILO_SEED),
  # scCODA (TS_DA_SCCODA_SEED), CellChat, LIANA and the pattern k-means, which all
  # do. So the determinism is real but UNDOCUMENTED and UNTRACEABLE: a result
  # cannot be tied to the guarantee that produced it.
  #
  # This test does NOT assert "there must never be a seed" — that would make the
  # eventual fix impossible to land. It asserts the SHAPE a fix must take: a seed
  # belongs in `config/` as a named constant and must be passed explicitly, never
  # appear as a bare literal in the pipeline. So when someone closes the gap, the
  # test still passes and the constant is findable.
  src <- paste(readLines(file.path(ts_project_root(), "R/sc/sc_pipeline.R"),
                         warn = FALSE), collapse = "\n")
  bare <- regmatches(src, gregexpr("(?<![_[:alnum:]])(random\\.seed|seed)\\s*=\\s*[0-9]+", src, perl = TRUE))[[1]]
  expect_length(bare, 0L)
  # No `set.seed()` in the pipeline either: a global reseed inside a Shiny
  # session is a side effect on every other reactive in the app, which is why
  # config/defaults.R:220 says so in as many words for CellChat.
  expect_false(grepl("set\\.seed\\(", src))
  # And `robust_find_clusters` passes no `random.seed` — the exact line that makes
  # the current behaviour data-derived. If a seed is ever threaded in, this is the
  # assertion that should be revisited with it.
  helpers <- paste(readLines(file.path(ts_project_root(), "R/sc/sc_helpers.R"),
                              warn = FALSE), collapse = "\n")
  body <- sub("(?s).*robust_find_clusters <- function.*?\\n\\}", "", helpers, perl = TRUE)
  expect_false(grepl("random\\.seed", body))
})

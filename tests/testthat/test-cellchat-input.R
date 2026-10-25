# =============================================================================
# test-cellchat-input.R — 4D-3 : entree CellChat depuis 10X / Seurat
# =============================================================================
# Couvre R/sc/sc_communication_input.R : conversion 10X (features/matrice/
# etiquettes) -> entree consommable par CellChat::createCellChat(), SANS
# dependance nouvelle (aucun appel CellChat n'est fait ici).
#
# Les 3 exigences reelles (assessment section 1) et les 2 decisions declarees
# (section 4) sont testees comme des PORTES DURES, pas des commentaires.
# =============================================================================

source_project_file("tools/pkg_skeleton/R/io_helpers.R")        # %||%
source_project_file("tools/pkg_skeleton/R/provenance.R")        # new_provenance_entry()
# D2 M4 (2026-10-25) : R/plotting vit desormais dans l'arbre du paquet
# (tools/pkg_skeleton/R/) — charge via load_project_pkg() (cf. helper-source.R).
load_project_pkg()
source_project_file("tools/pkg_skeleton/R/sc_velocity.R")         # velocity_object_fingerprint()
source_project_file("tools/pkg_skeleton/R/sc_communication.R")    # .communication_object_fingerprint()
source_project_file("tools/pkg_skeleton/R/sc_communication_input.R")

.mk_counts <- function(n_genes = 4L, n_cells = 6L, seed = 1L) {
  set.seed(seed)
  m <- matrix(rpois(n_genes * n_cells, 5), nrow = n_genes,
              dimnames = list(paste0("G", seq_len(n_genes)),
                              paste0("c", seq_len(n_cells))))
  m
}

# ---------------------------------------------------------------------------
# 1. Contrat (etats, exigences, espece)
# ---------------------------------------------------------------------------
test_that("cellchat_input_states exposes the 4 frozen states", {
  expect_identical(
    cellchat_input_states(),
    c("invalid_input", "invalid_features", "invalid_labels", "invalid_species")
  )
})

test_that("cellchat_input_requirements encodes the assessment's real blockers", {
  req <- cellchat_input_requirements()
  expect_s3_class(req, "data.frame")
  expect_equal(nrow(req), 5L)
  expect_true(all(c("requirement", "role", "status") %in% colnames(req)))
  # the 3 real requirements + the 2 declared decisions
  expect_true(all(c("expression_normalisee", "symboles_de_genes",
                    "etiquettes_de_population", "espece_declaree",
                    "contrat_upstream") %in% req$requirement))
  # the format is explicitly NOT the blocker
  expect_true(any(grepl("indexes par SYMBOLE", req$role)))
})

test_that("cellchat_database_for_species resolves human/mouse and rejects anything else", {
  expect_identical(cellchat_database_for_species("human"), "CellChatDB.human")
  expect_identical(cellchat_database_for_species("mouse"), "CellChatDB.mouse")
  expect_identical(cellchat_database_for_species("HUMAN"), "CellChatDB.human")  # casse ignoree
  for (bad in list(NULL, NA_character_, "", "rat", c("human", "mouse"))) {
    e <- tryCatch(cellchat_database_for_species(bad), error = function(e) e)
    expect_s3_class(e, "cellchat_input_error")
    expect_identical(cellchat_input_error_state(e), "invalid_species")
  }
})

# ---------------------------------------------------------------------------
# 2. Chemin nominal (matrice + symboles + etiquettes)
# ---------------------------------------------------------------------------
test_that("cellchat_input_from_matrix produces a CellChat-ready object", {
  m <- .mk_counts()
  lab <- c("T", "T", "B", "B", "NK", "NK"); names(lab) <- colnames(m)
  x <- cellchat_input_from_matrix(m, rownames(m), lab, species = "human")

  expect_identical(x$type, "cellchat_input")
  expect_identical(x$status, "valid")
  expect_s3_class(x, "cellchat_input")
  expect_identical(x$group_by, "labels")
  expect_identical(x$species, "human")
  expect_identical(x$database, "CellChatDB.human")
  expect_equal(dim(x$data), c(4L, 6L))
  expect_identical(rownames(x$data), rownames(m))
  # meta is aligned with the data columns, exactly as createCellChat() expects
  expect_identical(x$meta$Cell, colnames(x$data))
  expect_identical(x$meta$labels, unname(lab))
  expect_equal(x$input_summary$n_populations, 3L)
  expect_true(nzchar(x$object_identity$fingerprint))
  expect_identical(x$analysis_id, "sc-cellchat-input")
  expect_true(!is.null(x$provenance))
  invisible(assert_cellchat_input(x))
})

test_that("gene identifiers are mapped to SYMBOLS and Ensembl version suffixes are stripped", {
  m <- .mk_counts()
  rownames(m) <- c("ENSG00000167286.9", "CD3D", "MS4A1.1", "NKG7")
  lab <- c("T", "T", "B", "B", "NK", "NK")
  x <- cellchat_input_from_matrix(m, rownames(m), lab, species = "human")
  expect_identical(rownames(x$data), c("ENSG00000167286", "CD3D", "MS4A1", "NKG7"))
  expect_identical(x$features$ensembl[1], "ENSG00000167286")
  expect_true(all(is.na(x$features$ensembl[2:4])))
  expect_equal(x$qc$n_ensembl_resolved, 1L)
})

test_that("a 10X features.tsv-style table (id, symbol, type) is accepted", {
  feat <- data.frame(
    id     = c("ENSG00000000003.14", "ENSG00000000005.6"),
    symbol = c("TSPAN6", "TNMD"),
    type   = c("Gene Expression", "Gene Expression"),
    stringsAsFactors = FALSE
  )
  m  <- matrix(1:8, nrow = 2, dimnames = list(NULL, paste0("c", 1:4)))
  x  <- cellchat_input_from_matrix(m, feat, c("A", "A", "B", "B"), species = "mouse")
  expect_identical(rownames(x$data), c("TSPAN6", "TNMD"))
  expect_identical(x$database, "CellChatDB.mouse")
  expect_identical(x$features$feature_type[1], "Gene Expression")
})

test_that("duplicate symbols are collapsed deterministically (sum by default, first on request)", {
  m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)  # rows: [1,4] [2,5] [3,6]
  sym <- c("G1", "G1", "G2")
  x_sum <- cellchat_input_from_matrix(m, sym, c("A", "B"), species = "human")
  expect_identical(rownames(x_sum$data), c("G1", "G2"))
  expect_equal(unname(x_sum$data["G1", ]), c(3, 9))   # 1+2, 4+5
  expect_equal(x_sum$qc$n_duplicate_symbols, 1L)
  expect_true(any(grepl("duplique", x_sum$warnings)))

  x_first <- cellchat_input_from_matrix(m, sym, c("A", "B"),
                                        species = "human", duplicate_symbols = "first")
  expect_equal(unname(x_first$data["G1", ]), c(1, 4))
})

# ---------------------------------------------------------------------------
# 3. Portes dures : etiquettes de population (blocage reel n.1)
# ---------------------------------------------------------------------------
test_that("cells without a population label are dropped AND counted, never silently", {
  m <- .mk_counts()
  lab <- c("T", "T", NA, "B", "", "B")
  x <- cellchat_input_from_matrix(m, rownames(m), lab, species = "human")
  expect_equal(ncol(x$data), 4L)
  expect_equal(x$qc$n_cells_dropped_na_label, 2L)
  expect_true(any(grepl("sans etiquette", x$warnings)))
})

test_that("fewer than 2 populations is a hard, classed failure", {
  m <- .mk_counts()
  for (lab in list(rep("T", 6), rep(NA_character_, 6), c("T", "T", NA, NA, "", ""))) {
    e <- tryCatch(cellchat_input_from_matrix(m, rownames(m), lab, species = "human"),
                  error = function(e) e)
    expect_s3_class(e, "cellchat_input_error")
    expect_identical(cellchat_input_error_state(e), "invalid_labels")
  }
})

test_that("min_cells_per_group drops under-represented populations and counts them", {
  m <- .mk_counts()
  lab <- c("T", "T", "T", "B", "B", "rare")
  x <- cellchat_input_from_matrix(m, rownames(m), lab, species = "human",
                                  min_cells_per_group = 2L)
  expect_false("rare" %in% x$meta$labels)
  expect_equal(x$qc$n_cells_dropped_min_group, 1L)
  expect_equal(x$input_summary$n_populations, 2L)
})

# ---------------------------------------------------------------------------
# 4. Portes dures : features / input / espece
# ---------------------------------------------------------------------------
test_that("empty / misaligned / symbol-less inputs fail with the right state", {
  m <- .mk_counts()
  lab <- c("A", "A", "B", "B", "A", "B")

  e_empty <- tryCatch(cellchat_input_from_matrix(matrix(numeric(0), 0, 0), character(0), character(0),
                                                 species = "human"), error = function(e) e)
  expect_identical(cellchat_input_error_state(e_empty), "invalid_input")

  e_blank <- tryCatch(cellchat_input_from_matrix(m, rep("", 4), lab, species = "human"),
                      error = function(e) e)
  expect_identical(cellchat_input_error_state(e_blank), "invalid_features")

  e_mis <- tryCatch(cellchat_input_from_matrix(m, c("G1", "G2"), lab, species = "human"),
                    error = function(e) e)
  expect_identical(cellchat_input_error_state(e_mis), "invalid_features")

  e_species <- tryCatch(cellchat_input_from_matrix(m, rownames(m), lab, species = "zebrafish"),
                        error = function(e) e)
  expect_identical(cellchat_input_error_state(e_species), "invalid_species")
})

# ---------------------------------------------------------------------------
# 5. Normalisation (exigence reelle n.1)
# ---------------------------------------------------------------------------
test_that("raw counts are flagged, and log_normalize actually normalises them", {
  m <- .mk_counts()
  lab <- c("A", "A", "B", "B", "A", "B")
  x_raw <- cellchat_input_from_matrix(m, rownames(m), lab, species = "human")
  expect_true(x_raw$qc$counts_like)
  expect_true(any(grepl("COMPTES BRUTS", x_raw$warnings)))

  x_norm <- cellchat_input_from_matrix(m, rownames(m), lab, species = "human",
                                       log_normalize = TRUE)
  expect_true(x_norm$qc$log_normalized)
  # counts_like records the INPUT signal (it WAS raw counts) — that is the
  # whole point; log_normalized records that the normalisation was applied.
  expect_true(x_norm$qc$counts_like)
  expect_lt(max(x_norm$data), 30)              # log1p(CPM) scale
  expect_false(any(grepl("COMPTES BRUTS", x_norm$warnings)))
})

test_that("log_normalize refuses to re-normalise already-normalised data", {
  m <- .mk_counts()
  m_norm <- log1p(sweep(m, 2, colSums(m) / 1e4, "/"))
  x <- cellchat_input_from_matrix(m_norm, rownames(m_norm), c("A", "A", "B", "B", "A", "B"),
                                  species = "human", log_normalize = TRUE)
  expect_false(x$qc$counts_like)
  expect_true(any(grepl("NON appliquee", x$warnings)))
  expect_equal(unname(x$data), unname(m_norm))  # untouched
})

test_that("cellchat_log_normalize is a pure log1p(CPM) transform", {
  # column-major: c1 = (10, 20) -> colSum 30 ; c2 = (30, 40) -> colSum 70
  m <- matrix(c(10, 20, 30, 40), nrow = 2,
              dimnames = list(c("G1", "G2"), c("c1", "c2")))
  out <- cellchat_log_normalize(m)
  expect_equal(unname(out[, "c1"]), log1p(c(10, 20) / 30 * 1e4))
  expect_equal(unname(out[, "c2"]), log1p(c(30, 40) / 70 * 1e4))
})

# ---------------------------------------------------------------------------
# 6. Validateur + resume
# ---------------------------------------------------------------------------
test_that("assert_cellchat_input accepts a valid object and rejects tampering", {
  m <- .mk_counts()
  x <- cellchat_input_from_matrix(m, rownames(m), c("A", "A", "B", "B", "A", "B"),
                                  species = "human")
  expect_invisible(assert_cellchat_input(x))

  expect_error(assert_cellchat_input(list(type = "other")), "cellchat_input")
  bad <- x; bad$meta <- bad$meta[1:2, , drop = FALSE]
  expect_error(assert_cellchat_input(bad), "desalignes")
  bad2 <- x; bad2$features$symbol <- rev(bad2$features$symbol)
  expect_error(assert_cellchat_input(bad2), "rownames")
})

test_that("cellchat_input_summary returns a single traceable row", {
  m <- .mk_counts()
  x <- cellchat_input_from_matrix(m, rownames(m), c("A", "A", "B", "B", "A", "B"),
                                  species = "human")
  s <- cellchat_input_summary(x)
  expect_equal(nrow(s), 1L)
  expect_identical(s$analysis_id, "sc-cellchat-input")
  expect_identical(s$database, "CellChatDB.human")
  expect_identical(s$n_populations, "2")
})

# ---------------------------------------------------------------------------
# 7. Enveloppe Seurat (le chemin reel de l'application)
# ---------------------------------------------------------------------------
test_that("build_cellchat_input extracts a Seurat object's data layer + metadata column", {
  skip_if_not_installed("Seurat")
  m <- .mk_counts()
  so <- Seurat::CreateSeuratObject(counts = m)
  so <- Seurat::NormalizeData(so, verbose = FALSE)
  so$celltype <- c("T", "T", "B", "B", "NK", "NK")

  x <- build_cellchat_input(so, group_by = "celltype", species = "human")
  expect_identical(x$group_by, "celltype")
  expect_equal(ncol(x$data), 6L)
  expect_equal(x$input_summary$n_populations, 3L)
  # the fingerprint is the Seurat object's, not the matrix's
  expect_identical(x$object_identity$fingerprint, velocity_object_fingerprint(so))
  invisible(assert_cellchat_input(x))
})

test_that("the grouping column handed to createCellChat is a REAL column of $meta", {
  skip_if_not_installed("Seurat")
  m <- .mk_counts()
  so <- Seurat::CreateSeuratObject(counts = m)
  so <- Seurat::NormalizeData(so, verbose = FALSE)
  so$celltype <- c("T", "T", "B", "B", "NK", "NK")

  x <- build_cellchat_input(so, group_by = "celltype", species = "human")

  # Piege deja paye en production : CellChat::createCellChat(group.by = ...)
  # exige une COLONNE de `meta`. build_cellchat_input() normalise les
  # identites en `meta$labels`, donc le nom d'origine ($group_by = "celltype")
  # n'existe PAS dans $meta — le passer tel quel fait echouer le run avec
  # « The 'group.by' is not a column name in the `meta` ».
  expect_true(cellchat_group_by_column(x) %in% colnames(x$meta),
              label = "grouping column must exist in $meta")
  # ...et le nom d'origine reste disponible pour la provenance / le rapport.
  expect_identical(x$group_by, "celltype")
})

test_that("build_cellchat_input fails cleanly on a bad object or a missing column", {
  e1 <- tryCatch(build_cellchat_input(list(), group_by = "x", species = "human"),
                 error = function(e) e)
  expect_identical(cellchat_input_error_state(e1), "invalid_input")

  skip_if_not_installed("Seurat")
  so <- Seurat::CreateSeuratObject(counts = .mk_counts())
  so <- Seurat::NormalizeData(so, verbose = FALSE)
  e2 <- tryCatch(build_cellchat_input(so, group_by = "nope", species = "human"),
                 error = function(e) e)
  expect_identical(cellchat_input_error_state(e2), "invalid_labels")

  e3 <- tryCatch(build_cellchat_input(so, group_by = "orig.ident", species = NULL),
                 error = function(e) e)
  expect_identical(cellchat_input_error_state(e3), "invalid_species")

  # omitting species entirely must NOT silently pick a default database
  e4 <- tryCatch(build_cellchat_input(so, group_by = "orig.ident"),
                 error = function(e) e)
  expect_identical(cellchat_input_error_state(e4), "invalid_species")
})

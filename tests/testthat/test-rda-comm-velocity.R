# =============================================================================
# test-rda-comm-velocity.R — Intégrations .rda Commit 4
# =============================================================================
# - read_velocity_rds() : workspace .rda à objet unique (liste spliced/
#   unspliced) importé ; multi-objets -> erreur orientante.
# - parse_cellchat_object() : liste net=... (forme REELLE de CellChat :
#   [source, target, interaction_name] + LR$LRsig) dans un .rda à objet unique ;
#   multi-objets -> erreur classée communication_import_error.
# Contrat : docs/contracts/RDATA_IMPORT_CONTRACT.md.
# =============================================================================

source_project_file("tools/pkg_skeleton/R/io_helpers.R")
source_project_file("tools/pkg_skeleton/R/rdata_io.R")
source_project_file("R/sc/sc_velocity.R")
source_project_file("R/sc/sc_communication.R")
# parse_cellchat_object() delegue l'extraction au moteur (regle 3).
source_project_file("R/sc/sc_communication_engine.R")

.tmpdir <- tempfile(pattern = "rda_comm_vel_")
dir.create(.tmpdir, showWarnings = FALSE)

# ── Velocity : fixture .rda (mêmes structures que helper-velocity-fixtures) ──
.vel_genes <- paste0("gene", 1:10)
.vel_cells <- paste0("cell", 1:5)
.vel_mat <- function() {
  m <- Matrix::Matrix(seq_len(50), nrow = 10, ncol = 5, sparse = TRUE)
  dimnames(m) <- list(.vel_genes, .vel_cells)
  m
}

vel_ok <- list(spliced = .vel_mat(), unspliced = .vel_mat() * 2L,
               cell_names = .vel_cells, gene_names = .vel_genes)
vel_ok_path <- file.path(.tmpdir, "velocity.rda")
save(vel_ok, file = vel_ok_path)

vel_extra <- "not_a_list"
vel_multi_path <- file.path(.tmpdir, "velocity_multi.rda")
save(vel_ok, vel_extra, file = vel_multi_path)

# ── CellChat : forme REELLE [source, target, interaction_name] dans un .rda ──
grp <- c("CD4 T", "B"); inter <- c("IL7_IL7R", "CCL5_CCR5")
prob <- array(0, dim = c(2, 2, 2), dimnames = list(grp, grp, inter))
prob["CD4 T", "B", "IL7_IL7R"] <- 0.5
prob["B", "B", "CCL5_CCR5"] <- 0.4
cc_stub <- list(
  net = list(prob = prob),
  LR = list(LRsig = data.frame(
    interaction_name = inter,
    pathway_name     = c("IL7 signaling", "CCL signaling"),
    ligand           = c("IL7", "CCL5"),
    receptor         = c("IL7R", "CCR5"),
    row.names        = inter,
    stringsAsFactors = FALSE
  ))
)
cc_path <- file.path(.tmpdir, "cellchat.rda")
save(cc_stub, file = cc_path)

cc_multi_path <- file.path(.tmpdir, "cellchat_multi.rda")
cc_other <- "x"
save(cc_stub, cc_other, file = cc_multi_path)

# ---------------------------------------------------------------------------
test_that("read_velocity_rds accepte un .rda à objet unique velocity", {
  res <- read_velocity_rds(vel_ok_path)
  expect_true(is.list(res))
  expect_identical(names(res), c("spliced", "unspliced", "cell_names", "gene_names"))
  expect_equal(dim(res$spliced), c(10L, 5L))
})

test_that("read_velocity_rds refuse un .rda multi-objets avec message orientant", {
  err <- tryCatch(read_velocity_rds(vel_multi_path), error = function(e) e)
  expect_match(class(err)[1], "velocity_validation_error", fixed = TRUE)
  expect_match(conditionMessage(err), "2 objets", fixed = TRUE)
  expect_match(conditionMessage(err), "Exporter la sélection", fixed = TRUE)
})

test_that("parse_cellchat_object accepte un stub liste dans un .rda", {
  parsed <- parse_cellchat_object(cc_path, source_file = "cellchat.rda")
  expect_true(is.list(parsed))
  expect_false(is.null(parsed$table))
  expect_true(nrow(parsed$table) > 0L)
})

test_that("parse_cellchat_object refuse un .rda multi-objets (erreur classée)", {
  err <- tryCatch(parse_cellchat_object(cc_multi_path, source_file = "x.rda"),
                  error = function(e) e)
  expect_match(class(err)[1], "communication_import_error", fixed = TRUE)
  expect_match(conditionMessage(err), "2 objets", fixed = TRUE)
  expect_match(conditionMessage(err), "Exporter la sélection", fixed = TRUE)
})

unlink(.tmpdir, recursive = TRUE)

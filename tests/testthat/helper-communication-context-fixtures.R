# =============================================================================
# helper-communication-context-fixtures.R — fixtures deterministes des
# CONTEXTES communication (V1.x-A spatial / V1.x-B trajectoire / V1.x-C
# vélocité) — roadmap CCC avancée, docs/ROADMAP_CCC_ADVANCED.md
# =============================================================================
# Noms prefixes .ccx_* pour eviter toute collision avec
# helper-communication-fixtures.R (.comm_*). 12 cellules, 3 populations
# (A, B, C) : A emet (ligands L1/L2), B recoit (recepteurs R1/R2), C fait
# les deux faiblement. Coordonnees deterministes : A pres de C, B loin.
# Sourcing : memes contrats que helper-communication-fixtures.R + les
# moteurs de contexte (sourcés au fil des phases).
# =============================================================================

if (!exists("source_project_file", envir = globalenv(), mode = "function")) {
  .ccx_root <- getwd()
  for (.i in 1:8) {
    if (file.exists(file.path(.ccx_root, "app.R"))) break
    .parent <- dirname(.ccx_root)
    if (identical(.parent, .ccx_root)) break
    .ccx_root <- .parent
  }
  sys.source(file.path(.ccx_root, "tests", "testthat", "helper-source.R"),
             envir = globalenv())
}

source_project_file("tools/pkg_skeleton/R/io_helpers.R")   # %||%
source_project_file("R/core/rdata_io.R")     # read_velocity_rds() accepte .rda
source_project_file("tools/pkg_skeleton/R/state.R")
source_project_file("tools/pkg_skeleton/R/provenance.R")
source_project_file("R/sc/sc_velocity.R")
source_project_file("R/sc/sc_communication.R")
source_project_file("R/sc/sc_communication_views.R")
source_project_file("R/sc/sc_communication_spatial.R")      # V1.x-A
source_project_file("R/sc/sc_communication_trajectory.R")   # V1.x-B
source_project_file("R/sc/sc_communication_velocity.R")     # V1.x-C
source_project_file("R/sc/sc_communication_perturbation.R") # V1.x-D

# Extracteur de fonctions top-level (implémentation propre aux contextes,
# volontairement distincte de .comm_top_level_assignments du test de freeze
# Stage 11 — chaque fichier de freeze reste autonome).
.ccx_top_level_assignments <- function(relpath) {
  exprs <- parse(file.path(ts_project_root(), relpath), keep.source = FALSE)
  nms <- character(0)
  for (e in exprs) {
    if (is.call(e) && identical(deparse(e[[1L]]), "<-") && is.symbol(e[[2L]])) {
      nms <- c(nms, as.character(e[[2L]]))
    }
  }
  unique(nms)
}

.ccx_cells <- c(paste0("A", 1:4), paste0("B", 1:4), paste0("C", 1:4))
.ccx_pops  <- c(rep("A", 4), rep("B", 4), rep("C", 4))

.ccx_identities <- function() setNames(.ccx_pops, .ccx_cells)

# Coordonnees : A autour de (0, 0.5), B autour de (10, 0.5), C pres de A.
.ccx_coords <- function() {
  xy <- rbind(
    cbind(0, seq(0, 1, length.out = 4)),
    cbind(10, seq(0, 1, length.out = 4)),
    cbind(0.5, seq(2, 3, length.out = 4))
  )
  rownames(xy) <- .ccx_cells
  colnames(xy) <- c("x", "y")
  xy
}

# Coordonnees data.frame (id, x, y) — meme geometrie.
.ccx_coords_df <- function() {
  xy <- .ccx_coords()
  data.frame(id = rownames(xy), x = xy[, "x"], y = xy[, "y"],
             stringsAsFactors = FALSE)
}

# Pseudo-temps : A tot (0..0.1), C intermediaire (0.4..0.5), B tard (0.9..1).
.ccx_pseudotime <- function() {
  setNames(c(seq(0.00, 0.10, length.out = 4),
             seq(0.90, 1.00, length.out = 4),
             seq(0.40, 0.50, length.out = 4)), .ccx_cells)
}

# Lignées slingshot-like : L1 = A + C1,C2 ; L2 = B + C3,C4 (C = point de
# branchement réparti sur les deux lignées — jamais collapsées par le moteur).
.ccx_lineage <- function() {
  setNames(c(rep("L1", 4), rep("L2", 4), c("L1", "L1", "L2", "L2")),
           .ccx_cells)
}

# Matrice d'expression genes x cells (base matrix) — A exprime L1/L2, B
# exprime R1/R2, C les quatre faiblement ; gènes de remplissage.
.ccx_expr <- function() {
  m <- matrix(0.1, nrow = 8, ncol = 12,
              dimnames = list(c("L1", "L2", "R1", "R2", "G1", "G2", "G3", "G4"),
                              .ccx_cells))
  m["L1", paste0("A", 1:4)] <- 4
  m["L2", paste0("A", 1:4)] <- 3
  m["R1", paste0("B", 1:4)] <- 5
  m["R2", paste0("B", 1:4)] <- 2
  m[c("L1", "L2", "R1", "R2"), paste0("C", 1:4)] <- 0.8
  m
}

# Table CellChat exportee couvrant A->B, A->C, C->B, auto C->C et une
# population D sans correspondance (QC contextes).
.ccx_cellchat_tab <- function() {
  data.frame(
    source   = c("A", "A", "C", "C", "D"),
    target   = c("B", "C", "B", "C", "A"),
    ligand   = c("L1", "L2", "L1", "L2", "L1"),
    receptor = c("R1", "R2", "R1", "R2", "R1"),
    prob     = c(0.4, 0.6, 0.2, 0.3, 0.9),
    pathway  = c("P1", "P2", "P1", "P2", "P9"),
    stringsAsFactors = FALSE
  )
}

# Orchestration miroir du module (harmonisation -> QC -> finalisation).
.ccx_result <- function(identities = .ccx_identities(),
                        identity_column = "cell_type",
                        seurat_obj = matrix(0, 2, 2,
                                            dimnames = list(c("g1", "g2"),
                                                            c("c1", "c2")))) {
  parsed <- parse_cellchat_import(.ccx_cellchat_tab(), source_file = "ctx.csv")
  harm <- harmonize_communication_identities(parsed$table, unique(identities),
                                             identity_column,
                                             context = "communication import")
  qcr <- communication_import_qc(harm$table)
  finalize_communication_result(
    canonical_table = qcr$table,
    source_method   = unique(parsed$table$source_method)[1L],
    source_files    = list(table = "ctx.csv"),
    identity_column = identity_column,
    identity_mapping = harm$mapping,
    identity_summary = harm$summary,
    column_mapping  = parsed$column_mapping,
    qc              = qcr$counts,
    n_input_rows    = parsed$n_input_rows,
    seurat_obj      = seurat_obj
  )
}

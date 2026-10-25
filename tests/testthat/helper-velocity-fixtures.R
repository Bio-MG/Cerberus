# =============================================================================
# helper-velocity-fixtures.R — fixtures deterministes velocity (Stages 8-9)
# =============================================================================
# Auto-source par testthat avant les test-*.R (ordre alphabetique : apres
# helper-source.R, donc source_project_file() est disponible). Noms prefixes
# .vel_* pour eviter toute collision entre fichiers de test. Sourcing des
# contrats requis (io_helpers fournit %||% ; config deja sourcee par
# helper-source.R pour les constantes TS_VELOCITY_*).
# =============================================================================

source_project_file("tools/pkg_skeleton/R/io_helpers.R")   # %||%
source_project_file("tools/pkg_skeleton/R/rdata_io.R")     # read_velocity_rds() accepte .rda
source_project_file("tools/pkg_skeleton/R/state.R")
source_project_file("tools/pkg_skeleton/R/provenance.R")
source_project_file("tools/pkg_skeleton/R/sc_velocity.R")

.vel_genes <- paste0("gene", 1:10)
.vel_cells <- paste0("cell", 1:5)

.vel_mat <- function(genes = .vel_genes, cells = .vel_cells) {
  m <- Matrix::Matrix(
    seq_len(length(genes) * length(cells)),
    nrow = length(genes), ncol = length(cells), sparse = TRUE
  )
  dimnames(m) <- list(genes, cells)
  m
}

.vel_seurat_stub <- function(genes = .vel_genes, cells = .vel_cells) {
  matrix(0, nrow = length(genes), ncol = length(cells),
         dimnames = list(genes, cells))
}

.vel_rds_fixture <- function(path, extra = list()) {
  l <- c(list(
    spliced = .vel_mat(),
    unspliced = .vel_mat() * 2L,
    cell_names = .vel_cells,
    gene_names = .vel_genes
  ), extra)
  saveRDS(l, path)
  path
}

# Enrichissement RDS minimal, reproduit la sequence d'orchestration du module
# (metadonnees reportees sur la structure validee avant finalisation).
.vel_validate_and_enrich <- function(velocity_input,
                                     orientation = "auto_strict",
                                     strip_cell_suffix = FALSE,
                                     strip_gene_version = FALSE,
                                     allow_low_overlap = FALSE,
                                     min_cell_overlap = NULL) {
  validated <- validate_velocity_matrices(
    spliced = velocity_input$spliced,
    unspliced = velocity_input$unspliced,
    ambiguous = velocity_input$ambiguous %||% NULL,
    seurat_cells = .vel_cells,
    seurat_genes = .vel_genes,
    orientation = orientation,
    strip_cell_suffix = strip_cell_suffix,
    strip_gene_version = strip_gene_version,
    allow_low_overlap = allow_low_overlap,
    min_cell_overlap = min_cell_overlap
  )
  validated$velocity_source <- velocity_input$velocity_source %||% "test"
  validated$velocity_method <- velocity_input$velocity_method %||% "precomputed"
  validated$input_orientation <- velocity_input$orientation %||% NA_character_
  validated$embedding_reduction <- velocity_input$embedding_reduction %||%
    NA_character_
  validated$clusters <- velocity_input$clusters %||% NULL
  validated$umap_embedding <- velocity_input$umap_embedding %||% NULL
  validated
}

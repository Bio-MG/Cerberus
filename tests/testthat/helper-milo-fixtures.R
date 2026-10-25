# =============================================================================
# helper-milo-fixtures.R — fixtures deterministes Milo (Stage 14, 4E-1)
# =============================================================================
# Auto-source par testthat avant les test-*.R. Comme helper-da-fixtures.R, ce
# fichier est source AVANT helper-source.R (ordre alphabetique m < s) — on
# source helper-source.R soi-meme si besoin. Noms prefixes .milo_* pour eviter
# toute collision. Sourcing des contrats requis : io_helpers (%||%), state,
# provenance, validation, sc_velocity (empreinte v2 REUTILISEE),
# sc_abundance_design (la PORTE Stage 13), sc_abundance_milo + vues.
# Les metadonnees reutilisent .da_meta() (helper-da-fixtures.R) — meme schema
# deterministe, aucune donnee biologique reelle.
# =============================================================================

if (!exists("source_project_file", envir = globalenv(), mode = "function")) {
  .milo_root <- getwd()
  for (.i in 1:8) {
    if (file.exists(file.path(.milo_root, "app.R"))) break
    .parent <- dirname(.milo_root)
    if (identical(.parent, .milo_root)) break
    .milo_root <- .parent
  }
  sys.source(file.path(.milo_root, "tests", "testthat", "helper-source.R"),
             envir = globalenv())
}

source_project_file("tools/pkg_skeleton/R/io_helpers.R")   # %||%
source_project_file("tools/pkg_skeleton/R/state.R")
source_project_file("tools/pkg_skeleton/R/provenance.R")
source_project_file("tools/pkg_skeleton/R/validation.R")
source_project_file("R/sc/sc_velocity.R")    # velocity_object_fingerprint
source_project_file("R/sc/sc_abundance_design.R")
source_project_file("R/sc/sc_abundance_milo.R")
source_project_file("R/sc/sc_abundance_milo_views.R")

# ── Objet Seurat synthetique deterministe ───────────────────────────────────
# meta    : .da_meta() (schema identique au Stage 13 ; 4 echantillons, 2
#           conditions A/B, batch equilibre, identite cell_type)
# counts  : deterministes (contenu sans importance — le DA Milo n'utilise PAS
#           les comptes, seulement l'embedding et les metadonnees)
# pca     : 2 dimensions ; les cellules de la condition B sont decalees de
#           `shift` sur PC1 → un contraste B vs A doit rendre logFC > 0 dans
#           les voisinages riches en B.
.milo_seurat_obj <- function(meta = .da_meta(samples = c(s1 = 50, s2 = 50, s3 = 50, s4 = 50)),
                             seed = 14, shift = 1.2, n_dims = 2) {
  set.seed(seed)
  n <- nrow(meta)
  genes <- paste0("gene", seq_len(20))
  counts <- Matrix::Matrix(
    seq_len(length(genes) * n) %% 7L + 1L,
    nrow = length(genes), ncol = n, sparse = TRUE
  )
  dimnames(counts) <- list(genes, rownames(meta))
  obj <- SeuratObject::CreateSeuratObject(counts = counts, meta.data = meta)
  emb <- matrix(rnorm(n * n_dims), nrow = n, ncol = n_dims)
  emb[, 1] <- emb[, 1] + ifelse(as.character(meta$condition) == "B", shift, 0)
  rownames(emb) <- rownames(meta)
  colnames(emb) <- paste0("PC_", seq_len(n_dims))
  obj[["pca"]] <- SeuratObject::CreateDimReducObject(
    embeddings = emb,
    loadings = matrix(0, nrow = length(genes), ncol = n_dims),
    assay = "RNA", key = "PC_"
  )
  obj
}

# ── Design Stage 13 attache a l'objet (la PORTE du Stage 14) ───────────────
.milo_design <- function(obj) {
  validated <- validate_da_design(
    metadata     = obj@meta.data,
    sample_id    = "sample_id",
    condition    = "condition",
    replicate_id = "replicate_id",
    batch        = "batch",
    identity     = "cell_type",
    context      = "fixture Milo"
  )
  finalize_da_design_result(validated = validated, seurat_obj = obj)
}

# ── Orchestration miroir du module ──────────────────────────────────────────
.milo_run <- function(obj = .milo_seurat_obj(),
                      design = NULL,
                      target = "B", reference = "A",
                      reduction = "pca", ...) {
  run_milo_da(
    seurat_obj          = obj,
    da_design_result    = design %||% .milo_design(obj),
    reduction           = reduction,
    target_condition    = target,
    reference_condition = reference,
    ...
  )
}

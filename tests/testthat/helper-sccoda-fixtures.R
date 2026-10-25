# =============================================================================
# helper-sccoda-fixtures.R — fixtures deterministes scCODA (Stage 15, 4E-2)
# =============================================================================
# Auto-source par testthat avant les test-*.R (s < h... ce fichier est source
# APRES helper-source.R : source_project_file() est deja disponible ; on se
# source soi-meme par securite). Noms prefixes .sccoda_*. Reutilise .da_meta()
# et .milo_seurat_obj() (helper-da/milo) — MEME objet que Milo avec une
# composition DECALEE (l'identite B est enrichie en condition B) : sert aux
# vues croisees Stage 16 (Milo x scCODA sur le meme objet).
#
# Composition (par echantillon de 50 cellules) :
#   condition A (s1, s2) : 35 CD4 T /  5 B / 10 NK
#   condition B (s3, s4) : 15 CD4 T / 25 B / 10 NK
# → identite la plus abondante = CD4 T (reference par defaut du politique
#   "most_abundant") ; effet attendu : B crediblement enrichi en condition B
#   (fraction B : 10% -> 50%).
# =============================================================================

if (!exists("source_project_file", envir = globalenv(), mode = "function")) {
  .sccoda_root <- getwd()
  for (.i in 1:8) {
    if (file.exists(file.path(.sccoda_root, "app.R"))) break
    .parent <- dirname(.sccoda_root)
    if (identical(.parent, .sccoda_root)) break
    .sccoda_root <- .parent
  }
  sys.source(file.path(.sccoda_root, "tests", "testthat", "helper-source.R"),
             envir = globalenv())
}

source_project_file("tools/pkg_skeleton/R/io_helpers.R")
source_project_file("tools/pkg_skeleton/R/state.R")
source_project_file("tools/pkg_skeleton/R/provenance.R")
source_project_file("R/core/validation.R")
source_project_file("R/sc/sc_velocity.R")
source_project_file("R/sc/sc_abundance_design.R")
source_project_file("R/sc/sc_abundance_milo.R")
source_project_file("R/sc/sc_abundance_milo_views.R")
source_project_file("R/sc/sc_abundance_sccoda.R")
source_project_file("R/sc/sc_abundance_sccoda_views.R")
source_project_file("R/sc/sc_abundance_cross_views.R")  # Stage 16

.sccoda_identities_by_sample <- list(
  s1 = c(rep("CD4 T", 35), rep("B", 5), rep("NK", 10)),
  s2 = c(rep("CD4 T", 35), rep("B", 5), rep("NK", 10)),
  s3 = c(rep("CD4 T", 15), rep("B", 25), rep("NK", 10)),
  s4 = c(rep("CD4 T", 15), rep("B", 25), rep("NK", 10))
)

#' Objet Seurat scCODA : composition decalee + shifts PCA (compatible Milo).
#' identity_shift decale les cellules d'identite B sur PC2 — regions
#' spatialement COHERENTES : les voisinages Milo annotent des nhoods purs B
#' (pont descriptif voisinages -> identite pour les vues croisees Stage 16).
.sccoda_seurat_obj <- function(..., identity_shift = 3) {
  obj <- .milo_seurat_obj(
    meta = .da_meta(samples = c(s1 = 50, s2 = 50, s3 = 50, s4 = 50),
                    identities_by_sample = .sccoda_identities_by_sample),
    ...
  )
  if (identity_shift != 0) {
    emb <- SeuratObject::Embeddings(obj, "pca")
    is_b <- as.character(obj@meta.data$cell_type) == "B"
    emb[is_b, 2] <- emb[is_b, 2] + identity_shift
    obj[["pca"]] <- SeuratObject::CreateDimReducObject(
      embeddings = emb,
      loadings = matrix(0, nrow = nrow(obj), ncol = ncol(emb)),
      assay = "RNA", key = "PC_"
    )
  }
  obj
}

#' Orchestration miroir du module (MCMC reduit pour la vitesse des tests ;
#' l'ESS associe declenche un AVERTISSEMENT, pas un echec)
.sccoda_run <- function(obj = .sccoda_seurat_obj(),
                        design = NULL,
                        target = "B", reference = "A",
                        reference_identity = "CD4 T",
                        num_results = 10000, num_burnin = 2500,
                        resolver = NULL, ...) {
  run_sccoda_da(
    seurat_obj          = obj,
    da_design_result    = design %||% .milo_design(obj),
    target_condition    = target,
    reference_condition = reference,
    reference_identity  = reference_identity,
    num_results         = num_results,
    num_burnin          = num_burnin,
    resolver            = resolver,
    ...
  )
}

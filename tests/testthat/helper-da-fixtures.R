# =============================================================================
# helper-da-fixtures.R — fixtures deterministes design DA (Stage 13, 4E-0)
# =============================================================================
# Auto-source par testthat avant les test-*.R. Comme pour la communication, ce
# fichier est source AVANT helper-source.R (ordre alphabetique a < s) — on
# source helper-source.R soi-meme si besoin. Noms prefixes .da_* pour eviter
# toute collision. Sourcing des contrats requis : io_helpers (%||%), state,
# provenance, validation (check_design_confounding REUTILISE), sc_velocity
# (velocity_object_fingerprint REUTILISE), sc_abundance_design.
# Aucune donnee biologique reelle : metadonnees synthetiques deterministes.
# =============================================================================

if (!exists("source_project_file", envir = globalenv(), mode = "function")) {
  .da_root <- getwd()
  for (.i in 1:8) {
    if (file.exists(file.path(.da_root, "app.R"))) break
    .parent <- dirname(.da_root)
    if (identical(.parent, .da_root)) break
    .da_root <- .parent
  }
  sys.source(file.path(.da_root, "tests", "testthat", "helper-source.R"),
             envir = globalenv())
}

source_project_file("tools/pkg_skeleton/R/io_helpers.R")   # %||%
source_project_file("tools/pkg_skeleton/R/state.R")
source_project_file("tools/pkg_skeleton/R/provenance.R")
source_project_file("tools/pkg_skeleton/R/validation.R")   # check_design_confounding
source_project_file("R/sc/sc_velocity.R")    # velocity_object_fingerprint
source_project_file("R/sc/sc_abundance_design.R")

.da_identities <- c("CD4 T", "B", "NK")

# ── Metadonnees synthetiques deterministes ──────────────────────────────────
# samples       : nom echantillon -> nb cellules
# conditions    : nom echantillon -> condition (noms alignes sur samples)
# replicates    : nom echantillon -> ID replicat biologique
# batch         : nom echantillon -> batch (NULL = pas de colonne batch)
# na_sample_id  : nombre de cellules avec sample_id NA (exclusions)
# identities_by_sample : NULL = cyclage deterministe des 3 identites ;
#   sinon liste nommee echantillon -> vecteur d'identites (recycle).
# Extension par rep(..., times = samples) : deterministe, jamais de
# simplification matricielle (unlist(mapply(...)) peut en produire une).
.da_meta <- function(samples = c(s1 = 40, s2 = 40, s3 = 40, s4 = 40),
                     conditions = c(s1 = "A", s2 = "A", s3 = "B", s4 = "B"),
                     replicates = c(s1 = "r1", s2 = "r2", s3 = "r3", s4 = "r4"),
                     batch = c(s1 = "b1", s2 = "b2", s3 = "b1", s4 = "b2"),
                     na_sample_id = 0L,
                     identities_by_sample = NULL) {
  n_total <- sum(samples)
  sid <- rep(unname(names(samples)), times = samples)
  if (!is.null(identities_by_sample)) {
    ident <- unlist(lapply(names(samples), function(s) {
      rep(identities_by_sample[[s]], length.out = samples[[s]])
    }), use.names = FALSE)
  } else {
    ident <- rep(.da_identities, length.out = n_total)
  }
  if (na_sample_id > 0L) sid[seq_len(na_sample_id)] <- NA_character_
  meta <- data.frame(
    sample_id = sid,
    condition = rep(unname(conditions), times = samples[names(conditions)]),
    replicate_id = rep(unname(replicates), times = samples[names(replicates)]),
    cell_type = ident,
    stringsAsFactors = FALSE
  )
  if (!is.null(batch)) {
    meta$batch <- rep(unname(batch), times = samples[names(batch)])
  }
  rownames(meta) <- paste0("cell_", seq_len(n_total))
  meta
}

# Objet a dimnames (stub) — seule l'identite (velocity_object_fingerprint)
# est extraite.
.da_stub_obj <- function(meta = .da_meta()) {
  matrix(0, nrow = 5, ncol = nrow(meta),
         dimnames = list(paste0("gene", 1:5), rownames(meta)))
}

# ── Orchestration miroir du module ──────────────────────────────────────────
# Defauts = colonnes du fixture (le module lit les selectInput, les tests
# fixent les memes valeurs explicites).
.da_validate_and_finalize <- function(metadata = .da_meta(),
                                      sample_id = "sample_id",
                                      condition = "condition",
                                      replicate_id = "replicate_id",
                                      batch = "batch",
                                      identity = "cell_type",
                                      ...) {
  validated <- validate_da_design(metadata = metadata, sample_id = sample_id,
                                  condition = condition, replicate_id = replicate_id,
                                  batch = batch, identity = identity, ...)
  finalize_da_design_result(validated = validated, seurat_obj = .da_stub_obj(metadata))
}

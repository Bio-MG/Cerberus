# =============================================================================
# test-velocity-contract-freeze.R — Stage 10 : gel du contrat de resultat
# velocity
# =============================================================================
# Ce fichier REFUSE toute evolution incompatible du contrat :
#   - champs du resultat canonique (schema fige, absence = NULL) ;
#   - etats de validite + libelles ;
#   - format d'empreinte objet (v2) ;
#   - surface publique de R/sc/sc_velocity.R (velocity_public_api()) ;
#   - orchestration module (3 fonctions top-level uniquement) ;
#   - isolation des consommateurs rapport/export (aucune reimplemented
#     validation) ;
#   - synchronisation code <-> docs/contracts/VELOCITY_RESULT_CONTRACT.md.
# Toute modification du contrat doit passer simultanement par le code, ce
# test et la documentation.
# Fixtures partages : helper-velocity-fixtures.R.
# =============================================================================

skip_if_not_installed("Matrix")
skip_if_not_installed("digest")

.vel_freeze_canonical_full <- function() {
  emb_partial <- matrix(seq_len(6), nrow = 3, ncol = 2)
  rownames(emb_partial) <- c("cell1", "cell2", "cell3")
  vec <- matrix(seq_len(10), nrow = 5, ncol = 2)
  rownames(vec) <- .vel_cells
  validated <- .vel_validate_and_enrich(
    read_velocity_rds(.vel_rds_fixture(tempfile(fileext = ".rds"),
      extra = list(umap_embedding = emb_partial,
                   embedding_reduction = "umap", vectors = vec)))
  )
  ali <- align_velocity_embedding(
    validated$umap_embedding, validated$cell_names, "umap",
    validated$embedding_reduction
  )
  finalize_velocity_result(
    validated = validated, input_mode = "rds",
    input_files = list(rds = "velocity.rds"),
    seurat_obj = .vel_seurat_stub(), requested_reduction = "umap",
    embedding_alignment = ali,
    velocity_vectors = validate_precomputed_velocity_vectors(
      vec, validated$cell_names, "umap", "umap"),
    vector_validation = list(field = "vectors", ok = TRUE)
  )
}

.vel_freeze_canonical_min <- function() {
  finalize_velocity_result(
    validated = .vel_validate_and_enrich(
      read_velocity_rds(.vel_rds_fixture(tempfile(fileext = ".rds")))),
    input_mode = "rds", seurat_obj = .vel_seurat_stub()
  )
}

# Noms des affectations top-level d'un fichier (assignations "<-").
.top_level_names <- function(relpath) {
  exprs <- parse(file.path(ts_project_root(), relpath))
  nms <- character(0)
  for (e in exprs) {
    if (is.call(e) && identical(e[[1L]], as.name("<-")) && is.name(e[[2L]])) {
      nms <- c(nms, as.character(e[[2L]]))
    }
  }
  unique(nms)
}

# ── Schema du resultat canonique ────────────────────────────────────────────
test_that("canonical result exposes every frozen contract field", {
  canonical <- .vel_freeze_canonical_full()
  missing <- setdiff(velocity_contract_fields(), names(canonical))
  expect_identical(missing, character(0))
})

test_that("contract field list matches the documented frozen schema", {
  expect_setequal(velocity_contract_fields(), c(
    "type", "status", "spliced", "unspliced", "ambiguous",
    "cell_names", "gene_names", "dimensions", "orientation",
    "cell_alignment", "gene_alignment", "embedding_alignment",
    "velocity_vectors", "vector_validation", "input_summary",
    "object_identity", "warnings", "provenance",
    "analysis_id", "timestamp_utc", "cell_mapping", "gene_mapping"
  ))
})

test_that("frozen per-field shapes on the canonical result", {
  canonical <- .vel_freeze_canonical_full()
  expect_identical(canonical$type, "rna_velocity")
  # Precedence du statut : partiel > valid quand l'embedding est partiel.
  expect_identical(canonical$status, "valid_partial_embedding")
  expect_identical(names(canonical$dimensions), c("cells", "genes"))
  expect_true(all(c("match_mode", "n_input", "n_matched", "n_missing",
                    "overlap_raw", "overlap_normalized",
                    "low_overlap_override") %in%
                    names(canonical$cell_alignment)))
  expect_true(all(c("match_mode", "n_input", "n_matched", "n_missing") %in%
                    names(canonical$gene_alignment)))
  expect_identical(canonical$provenance$analysis_type, "rna_velocity")
  expect_match(canonical$object_identity$fingerprint, "^v2::")
  expect_match(canonical$timestamp_utc, "^\\d{4}-\\d{2}-\\d{2}T")
  expect_true(is.data.frame(canonical$cell_mapping))
  expect_true(is.data.frame(canonical$gene_mapping))
  expect_true(is.matrix(canonical$velocity_vectors))
  expect_identical(ncol(canonical$velocity_vectors), 2L)
})

test_that("absent components stay NULL (no fabrication policy)", {
  canonical <- .vel_freeze_canonical_min()
  expect_null(canonical$ambiguous)
  expect_null(canonical$velocity_vectors)
  expect_null(canonical$embedding_alignment)
  expect_null(canonical$vector_validation)
})

# ── Etats de validite figes ─────────────────────────────────────────────────
test_that("validity states and labels are frozen", {
  expect_setequal(velocity_validity_states(), c(
    "valid", "valid_no_vectors", "valid_partial_embedding", "invalid_input",
    "invalid_orientation", "invalid_cell_alignment", "invalid_gene_alignment",
    "invalid_vector_projection", "stale_against_current_seurat_object"
  ))
  expect_setequal(names(velocity_status_labels()), velocity_validity_states())
})

test_that("object fingerprint format is pinned to v2", {
  fp <- velocity_object_fingerprint(.vel_seurat_stub())
  expect_match(fp, "^v2::\\d+::\\d+::")
})

# ── Surface publique figee ──────────────────────────────────────────────────
test_that("public API surface of sc_velocity.R is frozen", {
  defined <- .top_level_names("R/sc/sc_velocity.R")
  internal <- grep("^\\.", defined, value = TRUE)
  public <- setdiff(defined, internal)
  expect_setequal(public, velocity_public_api())
  # Helpers internes prefixes d'un point, jamais publics.
  expect_true(all(c(".VELOCITY_STATUS_STATES", ".velocity_stop",
                    ".velocity_matrix_is_numeric", ".read_lines_maybe_gz",
                    ".provenance_versions_string") %in% internal))
})

test_that("velocity module defines exactly its three orchestration functions", {
  expect_setequal(
    .top_level_names("modules/sc/mod_sc_velocity.R"),
    c("mod_sc_velocity_ui", "mod_sc_velocity_output_ui",
      "mod_sc_velocity_server")
  )
  # Le module consomme les accesseurs documentes (contrat Stage 8/9) —
  # aucune reimplementation locale de logique velocity.
  src <- paste(readLines(file.path(ts_project_root(),
                                   "modules/sc/mod_sc_velocity.R")),
               collapse = "\n")
  expect_match(src, "finalize_velocity_result(", fixed = TRUE)
  expect_match(src, "velocity_status_labels(", fixed = TRUE)
  expect_match(src, "velocity_export_filename(", fixed = TRUE)
})

# ── Isolation des consommateurs rapport/export ──────────────────────────────
test_that("report/export consumers do not reimplement velocity validation", {
  # Le rapport ne fait que monter l'UI velocity (aucune logique de resultat).
  mod_sc_src <- paste(readLines(file.path(ts_project_root(),
                                          "modules/sc/mod_sc.R")),
                      collapse = "\n")
  expect_match(mod_sc_src, "mod_sc_velocity_server", fixed = TRUE)
  expect_false(grepl("finalize_velocity_result|validate_velocity_matrices|velocity_object_fingerprint",
                     mod_sc_src))

  # Le script reproducible ignore le domaine ; les templates n appellent aucune fonction de calcul (post-V1.0 : ils affichent le canonique).
  export_src <- paste(readLines(file.path(ts_project_root(),
                                          "R/sc/sc_export.R")),
                      collapse = "\n")
  expect_false(grepl("velocity", export_src, ignore.case = TRUE))
  # Post-V1.0 : le rapport Rmd AFFICHE le resultat canonique (tables
  # pures, sections velocity/communication/DA) - il n a jamais le droit
  # de REIMPLEMENTER l analyse. Garde ciblee : aucun appel de
  # calcul/validation/empreinte du domaine.
  compute_ban <- "validate_velocity_matrices\\(|finalize_velocity_result\\(|velocity_object_fingerprint\\("
  for (tpl in list.files(file.path(ts_project_root(), "reports"),
                         full.names = TRUE)) {
    tpl_src <- paste(readLines(tpl), collapse = "\n")
    expect_false(grepl(compute_ban, tpl_src, ignore.case = TRUE),
                 info = basename(tpl))
  }
})

# ── Synchronisation code <-> contrat documentaire ───────────────────────────
test_that("contract document is in sync with the frozen code", {
  doc_path <- file.path(ts_project_root(), "docs", "contracts",
                        "VELOCITY_RESULT_CONTRACT.md")
  skip_if_not(file.exists(doc_path), .ts_contract_skip_msg(doc_path))
  doc <- paste(.ts_contract_readlines(doc_path), collapse = "\n")
  for (f in velocity_contract_fields()) {
    expect_match(doc, paste0("`", f, "`"), fixed = TRUE,
                 info = paste("champ contrat absent du document :", f))
  }
  for (st in velocity_validity_states()) {
    expect_match(doc, st, fixed = TRUE,
                 info = paste("etat absent du document :", st))
  }
  expect_match(doc, "finalize_velocity_result", fixed = TRUE)
  expect_match(doc, "assert_velocity_result", fixed = TRUE)
  expect_match(doc, "velocity_public_api", fixed = TRUE)
})

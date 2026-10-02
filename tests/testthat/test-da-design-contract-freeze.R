# =============================================================================
# test-da-design-contract-freeze.R — Stage 13 : gel du contrat de validation
# du design DA (4E-0)
# =============================================================================
# REFUSE toute evolution incompatible : champs du resultat canonique, etats de
# validite + libelles, empreinte v2 (REUTILISEE de velocity), surface publique
# de R/sc/sc_abundance_design.R, orchestration module (3 fonctions top-level),
# isolation des consommateurs rapport/export, synchronisation code <-> doc.
# Extraction des affectations top-level reecrite (pas de duplication ligne a
# ligne des autres tests de freeze).
# =============================================================================

.da_top_level_assignments <- function(relpath) {
  exprs <- parse(file.path(ts_project_root(), relpath), keep.source = FALSE)
  nms <- character(0)
  for (e in exprs) {
    if (is.call(e) && identical(deparse(e[[1L]]), "<-") && is.symbol(e[[2L]])) {
      nms <- c(nms, as.character(e[[2L]]))
    }
  }
  unique(nms)
}

.da_freeze_canonical_full <- function() {
  .da_validate_and_finalize()
}

.da_freeze_canonical_blocked <- function() {
  .da_validate_and_finalize(
    metadata = .da_meta(conditions = c(s1 = "A", s2 = "B", s3 = "B", s4 = "B"))
  )
}

# ── Schema du resultat canonique ────────────────────────────────────────────
test_that("canonical design result exposes every frozen contract field", {
  for (canon in list(.da_freeze_canonical_full(), .da_freeze_canonical_blocked())) {
    expect_identical(setdiff(da_design_contract_fields(), names(canon)),
                     character(0))
  }
})

test_that("frozen per-field shapes on the canonical design result", {
  canonical <- .da_freeze_canonical_full()
  expect_identical(canonical$type, "da_design")
  expect_identical(canonical$status, "valid")
  # Unite de composition explicite (jamais deduite du contexte).
  expect_identical(canonical$composition_unit, "sample")
  expect_true(isTRUE(canonical$provenance$parameters$composition_unit == "sample"))
  expect_true(is.data.frame(canonical$condition_summary))
  expect_true(is.data.frame(canonical$sample_summary))
  expect_true(is.data.frame(canonical$condition_batch_table))
  expect_true(is.data.frame(canonical$identity_coverage))
  expect_true(is.data.frame(canonical$missingness))
  expect_true(is.data.frame(canonical$exclusions))
  expect_true(is.list(canonical$milo_eligibility))
  expect_true(is.list(canonical$sccoda_eligibility))
  expect_identical(canonical$provenance$analysis_type, "da_design")
  expect_identical(canonical$provenance$analysis_id, "sc-da-design")
  expect_match(canonical$object_identity$fingerprint, "^v2::")
  expect_match(canonical$timestamp_utc, "^\\d{4}-\\d{2}-\\d{2}T")
})

test_that("blocked design keeps its report shape (explained, never consumable)", {
  blocked <- .da_freeze_canonical_blocked()
  expect_identical(blocked$status, "invalid_design")
  expect_false(isTRUE(blocked$milo_eligibility$eligible))
  expect_true(length(blocked$milo_eligibility$blockers) > 0L)
  expect_identical(blocked$provenance$status, "invalid_design")
})

# ── Etats de validite figes ─────────────────────────────────────────────────
test_that("validity states and labels are frozen", {
  expect_setequal(da_design_validity_states(), c(
    "valid", "valid_with_warnings", "invalid_design",
    "invalid_input", "stale_against_current_seurat_object"
  ))
  expect_setequal(names(da_design_status_labels()), da_design_validity_states())
})

test_that("object identity is pinned to the v2 fingerprint reused from velocity", {
  expect_match(velocity_object_fingerprint(.da_stub_obj()), "^v2::")
  canonical <- .da_freeze_canonical_full()
  expect_identical(canonical$object_identity$fingerprint,
                   velocity_object_fingerprint(.da_stub_obj()))
  expect_match(canonical$object_identity$method, "velocity_object_fingerprint",
               fixed = TRUE)
})

# ── Surface publique figee ──────────────────────────────────────────────────
test_that("public API surface of sc_abundance_design.R is frozen", {
  defined <- .da_top_level_assignments("R/sc/sc_abundance_design.R")
  public <- defined[!grepl("^\\.", defined)]
  expect_setequal(public, da_design_public_api())
  expect_true(all(c(".DA_DESIGN_STATUS_STATES", ".da_design_stop",
                    ".da_design_is_blank",
                    ".da_design_object_fingerprint") %in%
                    setdiff(defined, public)))
})

test_that("da design module defines exactly its three orchestration functions", {
  expect_setequal(
    .da_top_level_assignments("modules/sc/mod_sc_da_design.R"),
    c("mod_sc_da_design_ui", "mod_sc_da_design_output_ui",
      "mod_sc_da_design_server")
  )
  # Le module consomme les accesseurs documentes — aucune reimplementation
  # locale de la logique de validation.
  src <- paste(readLines(file.path(ts_project_root(),
                                   "modules/sc/mod_sc_da_design.R")),
               collapse = "\n")
  expect_match(src, "validate_da_design(", fixed = TRUE)
  expect_match(src, "finalize_da_design_result(", fixed = TRUE)
  expect_match(src, "da_design_status_labels(", fixed = TRUE)
  expect_match(src, "da_design_export_filename(", fixed = TRUE)
  expect_match(src, "build_da_design_summary(", fixed = TRUE)
  expect_match(src, "build_da_design_sample_export(", fixed = TRUE)
  expect_match(src, "provenance_append(", fixed = TRUE)
  # check_design_confounding appartient au DOMAINE, pas au module.
  expect_false(grepl("check_design_confounding", src))
})

# ── Isolation des consommateurs rapport/export ──────────────────────────────
test_that("report/export consumers do not reimplement DA design validation", {
  mod_sc_src <- paste(readLines(file.path(ts_project_root(),
                                          "modules/sc/mod_sc.R")),
                      collapse = "\n")
  expect_match(mod_sc_src, "mod_sc_da_design_server", fixed = TRUE)
  expect_false(grepl("validate_da_design|finalize_da_design_result",
                     mod_sc_src))

  export_src <- paste(readLines(file.path(ts_project_root(),
                                          "R/sc/sc_export.R")),
                      collapse = "\n")
  expect_false(grepl("da_design|abundance_design", export_src, ignore.case = TRUE))
  # Post-V1.0 : le rapport Rmd AFFICHE le resultat canonique (tables
  # pures, sections velocity/communication/DA) - il n a jamais le droit
  # de REIMPLEMENTER l analyse. Garde ciblee : aucun appel de
  # calcul/validation/empreinte du domaine.
  compute_ban <- "validate_da_design\\(|finalize_da_design_result\\(|assert_da_design_result\\("
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
                        "DA_DESIGN_CONTRACT.md")
  skip_if_not(file.exists(doc_path), .ts_contract_skip_msg(doc_path))
  doc <- paste(.ts_contract_readlines(doc_path), collapse = "\n")
  for (f in da_design_contract_fields()) {
    expect_match(doc, paste0("`", f, "`"), fixed = TRUE,
                 info = paste("champ contrat absent du document :", f))
  }
  for (st in da_design_validity_states()) {
    expect_match(doc, st, fixed = TRUE,
                 info = paste("etat absent du document :", st))
  }
  for (fn in c("validate_da_design", "finalize_da_design_result",
               "assert_da_design_result", "da_design_public_api")) {
    expect_match(doc, fn, fixed = TRUE,
                 info = paste("fonction absente du document :", fn))
  }
})

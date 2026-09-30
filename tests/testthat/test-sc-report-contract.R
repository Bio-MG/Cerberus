# =============================================================================
# test-sc-report-contract.R — Rapport consolidé 4F (Stage 17)
# =============================================================================
# Contrat : docs/contracts/CONSOLIDATED_REPORT_CONTRACT.md (figé).
# Le rapport est un COMPILATEUR : ces tests vérifient que le collecteur ne
# fait que LIRE l'état, que le validateur refuse les sections sans
# provenance, que les analyses absentes sont gracieuses et que le bundle
# exporte des copies fidèles — jamais une re-exécution.
# =============================================================================

source_project_file("R/sc/sc_velocity.R")            # empreinte v2 + staleness velocity
source_project_file("R/sc/sc_communication.R")       # staleness communication
source_project_file("R/sc/sc_communication_engine.R")
source_project_file("R/sc/sc_abundance_design.R")    # design DA (finalizer)
source_project_file("R/sc/sc_abundance_milo.R")      # staleness milo
source_project_file("R/sc/sc_abundance_sccoda.R")    # staleness scCODA
source_project_file("R/reports/report_collector.R")
source_project_file("R/core/error_log.R")   # M-1 : journal des avalées
source_project_file("R/reports/report_validator.R")
source_project_file("R/reports/report_render.R")
source_project_file("R/reports/report_bundle.R")

# ── Fixtures locaux (stub objet commun : 5 genes x 160 cellules) ────────────
.rep_stub_obj <- function() .da_stub_obj()

.rep_velocity_canonical <- function(seurat_obj, ...) {
  velocity_input <- list(spliced = .vel_mat(), unspliced = .vel_mat() * 2L)
  validated <- .vel_validate_and_enrich(velocity_input)
  finalize_velocity_result(validated = validated, input_mode = "rds",
                           input_files = list(rds = "velocity.rds"),
                           seurat_obj = seurat_obj, requested_reduction = "umap",
                           analysis_id = "sc-velocity", ...)
}

.rep_communication_canonical <- function(seurat_obj) {
  parsed <- parse_cellchat_import(.comm_cellchat_tab(),
                                  source_file = "cellchat_export.csv")
  .comm_import_and_finalize(parsed, seurat_obj = seurat_obj)
}

.rep_liana_obj <- function() {
  samples <- c("C01", "C02", "T01", "T02", "T03")
  conditions <- c("CONTROL", "CONTROL", "TREATMENT", "TREATMENT", "TREATMENT")
  cells <- unlist(lapply(samples, function(sample) paste0(sample, "_", 1:2)),
                  use.names = FALSE)
  counts <- Matrix::Matrix(
    matrix(seq_len(20L * length(cells)) %% 7L, nrow = 20L,
           dimnames = list(paste0("G", 1:20), cells)),
    sparse = TRUE
  )
  data <- log1p(counts)
  obj <- suppressWarnings(SeuratObject::CreateSeuratObject(
    counts = counts,
    meta.data = data.frame(
      cell_type = rep(c("A", "B"), length.out = length(cells)),
      sample_id = rep(samples, each = 2L),
      condition = rep(conditions, each = 2L),
      row.names = cells
    )
  ))
  obj[["RNA"]]$data <- data
  obj
}

.rep_liana_collection_fixture <- function() {
  counter <- new.env(parent = emptyenv())
  counter$calls <- 0L
  backend <- list(run = function(sce, ...) {
    counter$calls <- counter$calls + 1L
    data.frame(
      source = c("A", "B"),
      target = c("B", "A"),
      ligand.complex = c("L1", "L2"),
      ligand = c("L1", "L2"),
      receptor.complex = c("R1", "R2"),
      receptor = c("R1", "R2"),
      edge_specificity = c(0.8, 0.6),
      stringsAsFactors = FALSE
    )
  })
  obj <- .rep_liana_obj()
  collection <- run_liana_by_sample(
    obj,
    sample_col = "sample_id",
    condition_col = "condition",
    idents_col = "cell_type",
    method = "natmi",
    resource = "Consensus",
    seed = 17L,
    min_cells = 1L,
    backend = backend
  )
  counter$calls <- 0L
  list(obj = obj, collection = collection, counter = counter)
}

.rep_liana_shared <- function(fixture, active_sample) {
  shared_rv <- create_sc_shared_state()
  active_result <- liana_collection_active(fixture$collection, active_sample)
  for (result in fixture$collection$results) {
    provenance_append(shared_rv, result$provenance)
  }
  provenance_append(shared_rv, fixture$collection$provenance)
  shared_rv$communication_result <- active_result
  shared_rv$communication_collection <- fixture$collection
  shared_rv$active_communication_sample <- active_sample
  shared_rv
}

.rep_design_canonical <- function(seurat_obj) {
  validated <- validate_da_design(metadata = .da_meta(),
                                  sample_id = "sample_id", condition = "condition",
                                  replicate_id = "replicate_id", batch = "batch",
                                  identity = "cell_type")
  finalize_da_design_result(validated = validated, seurat_obj = seurat_obj)
}

# Resultats Milo/scCODA synthetiques — schéma canonique minimal requis par le
# collecteur (champs figés des contrats MILO/SCCODA). Le CALCUL Milo/scCODA
# reel est couvert par les suites 4E ; ici on ne teste que la compilation.
.rep_milo_canonical <- function(seurat_obj) {
  list(
    type = "milo_da", status = "valid", analysis_id = "sc-milo-test",
    tested_contrast = list(target = "B", reference = "A",
                           formula = "~0 + condition",
                           contrast = "conditionB - conditionA",
                           interpretation = "logFC > 0 = enrichi cible"),
    parameters = list(reduction = "umap", seed = 14L),
    neighbourhood_summary = data.frame(
      n_neighbourhoods = 3L, n_cells_in_nhoods = 100L,
      fraction_cells_in_nhoods = 0.6, median_nhood_size = 30,
      min_nhood_size = 20, max_nhood_size = 50),
    DA_table = data.frame(
      Nhood = 1:3, n_cells = c(20L, 30L, 50L), logFC = c(0.5, -0.3, 0.1),
      logCPM = 1, F = 1, PValue = 0.1, FDR = 0.2,
      SpatialFDR = c(0.05, 0.5, 0.8), identity = c("B", "A", NA),
      identity_fraction = 0.9),
    nhood_assignment = NULL,
    sample_composition = data.frame(
      sample = "s1", condition = "A", batch = "b1", n_cells_total = 40L,
      n_cells_in_nhoods = 30L),
    package_versions = list(miloR = "2.2.0"),
    object_identity = list(fingerprint = velocity_object_fingerprint(seurat_obj),
                           method = "v2", seurat_dims = c(5L, 160L)),
    warnings = character(0),
    provenance = new_provenance_entry(analysis_id = "sc-milo-test",
                                      method = "milo", dataset = seurat_obj)
  )
}

.rep_sccoda_canonical <- function(seurat_obj) {
  list(
    type = "sccoda_da", status = "valid", analysis_id = "sc-sccoda-test",
    compositional_unit = "sample", reference_identity = "A",
    credible_effects = c("B"),
    convergence_diagnostics = list(rhat_max = NA, ess_min = 50,
                                   n_divergences = NA, acc_rate = 0.8,
                                   num_results = 20000L, num_burnin = 5000L,
                                   notes = "chaine unique"),
    composition_table = data.frame(sample = "s1", condition = "A",
                                   batch = "b1", A = 30L, B = 10L),
    effect_table = data.frame(
      covariate = "conditionB", identity = "B", effect = 0.4,
      hdi_low = 0.1, hdi_high = 0.7, sd = 0.1, inclusion_probability = 0.99,
      log2_fold_change = 0.5, credible = TRUE, effect_sign_flipped = FALSE),
    parameters = list(fdr_target = 0.05, reference_policy = "explicit"),
    model_specification = list(formula = "~condition", reference_identity = "A"),
    package_versions = list(sccoda = "0.1.9"),
    object_identity = list(fingerprint = velocity_object_fingerprint(seurat_obj),
                           method = "v2", seurat_dims = c(5L, 160L)),
    warnings = character(0),
    provenance = new_provenance_entry(analysis_id = "sc-sccoda-test",
                                      method = "sccoda", dataset = seurat_obj)
  )
}

.rep_full_shared <- function(seurat_obj) {
  shared_rv <- create_sc_shared_state()
  shared_rv$markers_data <- data.frame(
    gene = c("g1", "g2"), cluster = c("1", "2"), avg_log2FC = c(1.1, 0.8))
  shared_rv$correlated_genes <- data.frame(
    gene = c("g3", "g4"), correlation = c(0.91, -0.72), p_adj = c(0.01, 0.03))
  shared_rv$corr_target_gene <- "g1"
  shared_rv$pseudobulk_result <- list(
    type = "sc_pseudobulk_de", engine = "deseq2", target = "B",
    reference = "A", n_genes = 2L, n_significant = 1L,
    de_table = data.frame(gene = c("g1", "g2"), baseMean = c(10, 20),
                          log2FoldChange = c(2.1, -0.3), padj = c(0.01, 0.4)))
  shared_rv$pathway_results <- data.frame(
    pathway = c("PATH1", "PATH2"), pval = c(0.01, 0.04))
  shared_rv$pathway_db <- "GOBP"
  shared_rv$traj_method <- "slingshot"
  shared_rv$velocity_result <- .rep_velocity_canonical(seurat_obj)
  shared_rv$communication_result <- .rep_communication_canonical(seurat_obj)
  shared_rv$da_design_result <- .rep_design_canonical(seurat_obj)
  shared_rv$da_milo_result <- .rep_milo_canonical(seurat_obj)
  shared_rv$da_sccoda_result <- .rep_sccoda_canonical(seurat_obj)
  provenance_append(shared_rv,
    new_provenance_entry(analysis_id = "sc-markers-test", method = "FindAllMarkers",
                         dataset = seurat_obj))
  shared_rv
}

.rep_html_text <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# ── 1. Gardes du collecteur ──────────────────────────────────────────────────
test_that("collector refuses a NULL / dim-less object with a classed French error", {
  e <- tryCatch(collect_consolidated_report_input(NULL), error = function(e) e)
  expect_s3_class(e, "report_error")
  expect_match(conditionMessage(e), "aucun objet Single-Cell", ignore.case = TRUE)

  e2 <- tryCatch(collect_consolidated_report_input("pas_un_objet"), error = function(e) e)
  expect_s3_class(e2, "report_error")
})

test_that("collector produces the frozen canonical input shape", {
  obj <- .rep_stub_obj()
  ri <- collect_consolidated_report_input(obj)
  expect_identical(ri$type, "consolidated_report_input")
  expect_identical(ri$version, "1.0")
  expect_identical(ri$analysis_id, "sc-report-consolide")
  expect_setequal(names(ri$analyses), consolidated_report_analyses())
  expect_identical(ri$dataset$n_cells, 160L)
  expect_identical(ri$dataset$n_genes, 5L)
  # Empreinte v2 REUTILISEE (jamais re-implementee)
  expect_identical(ri$dataset$fingerprint$fingerprint,
                   velocity_object_fingerprint(obj))
  # Provenance vide mais bien typée
  expect_s3_class(ri$provenance_df, "data.frame")
  expect_identical(nrow(ri$provenance_df), 0L)
  expect_true(is.data.frame(ri$config_snapshot))
  expect_true(all(c("TS_DA_MILO_DISPLAY_ALPHA", "TS_REPORT_MAX_TABLE_ROWS")
                  %in% ri$config_snapshot$constante))
})

# ── 2. Projet vide : gracieux, jamais d'erreur technique ────────────────────
test_that("empty project renders every section as gracefully absent", {
  obj <- .rep_stub_obj()
  ri <- collect_consolidated_report_input(obj, create_sc_shared_state())
  val <- validate_consolidated_report_input(ri)
  expect_true(all(vapply(val$verdicts, function(v) v$state == "absent", logical(1))))
  expect_true(val$ok_overall)
  expect_identical(nrow(val$counts), 1L)
  expect_identical(val$counts$etat, "absent")
  expect_identical(val$counts$n_sections, 12L)

  html_path <- file.path(tempdir(), "rep_empty.html")
  write_consolidated_report_html(ri, val, html_path)
  txt <- .rep_html_text(html_path)
  # Message gracieux fige — pas d'erreur technique, pas de contenu fabrique.
  expect_match(txt, "non exécutée pour ce projet", fixed = TRUE)
  expect_match(txt, "sc-report-consolide", fixed = TRUE)
  unlink(html_path)
})

# ── 3. Projet complet : verdicts valides + tracabilite analysis_id ──────────
test_that("full project collects valid verdicts with traceable analysis ids", {
  obj <- .rep_stub_obj()
  shared_rv <- .rep_full_shared(obj)
  ri <- collect_consolidated_report_input(obj, shared_rv)
  val <- validate_consolidated_report_input(ri)

  states <- vapply(val$verdicts, function(v) v$state, character(1))
  # Legacy presents (incl. pseudobulk + correlation, post-V1.0)
  expect_identical(states[["markers"]], "valid_legacy")
  expect_identical(states[["pseudobulk"]], "valid_legacy")
  expect_identical(states[["correlation"]], "valid_legacy")
  expect_identical(states[["pathways"]], "valid_legacy")
  expect_identical(states[["trajectory"]], "valid_legacy")
  # Domaines a contrat, empreinte fraiche
  expect_identical(states[["velocity"]], "valid")
  expect_identical(states[["communication"]], "valid")
  expect_identical(states[["da_design"]], "valid")
  expect_identical(states[["da_milo"]], "valid")
  expect_identical(states[["da_sccoda"]], "valid")
  # Vues croisées : la vue pure peut échouer sur des résultats synthétiques —
  # le collecteur doit rester gracieux (absent), jamais planter.
  expect_true(states[["da_cross"]] %in% c("absent", "valid"))
  expect_true(val$ok_overall)

  html_path <- file.path(tempdir(), "rep_full.html")
  write_consolidated_report_html(ri, val, html_path)
  txt <- .rep_html_text(html_path)
  expect_match(txt, "sc-velocity", fixed = TRUE)
  expect_match(txt, "sc-communication-import", fixed = TRUE)
  expect_match(txt, "sc-milo-test", fixed = TRUE)
  expect_match(txt, "aucune analyse ré-exécutée", ignore.case = TRUE)
  unlink(html_path)
})

test_that("a valid LIANA collection is summarised read-only with unique complete analysis ids", {
  fixture <- .rep_liana_collection_fixture()
  active_sample <- "TREATMENT::T03"
  shared_rv <- .rep_liana_shared(fixture, active_sample)
  collection_before <- state_get(shared_rv, "communication_collection")
  result_before <- state_get(shared_rv, "communication_result")
  active_before <- state_get(shared_rv, "active_communication_sample")
  provenance_before <- shiny::isolate(shared_rv$provenance)

  ri <- collect_consolidated_report_input(fixture$obj, shared_rv)
  entry <- ri$analyses$communication
  summary_value <- function(field) {
    value <- entry$summary$valeur[entry$summary$champ == field]
    if (length(value) == 1L) value else NA_character_
  }
  expected_ids <- unique(c(
    fixture$collection$analysis_id,
    vapply(fixture$collection$results,
           function(result) result$analysis_id, character(1L))
  ))

  expect_true(entry$present)
  expect_identical(summary_value("n_echantillons"), "5")
  expect_identical(summary_value("n_conditions"), "2")
  expect_identical(summary_value("methode_liana"), "natmi")
  expect_identical(summary_value("ressource_liana"), "Consensus")
  expect_identical(summary_value("n_echantillon_actif"), "1")
  expect_identical(summary_value("echantillon_actif"), active_sample)
  expect_setequal(entry$analysis_ids, expected_ids)
  expect_identical(anyDuplicated(entry$analysis_ids), 0L)
  expect_identical(entry$extras$sample_manifest,
                   fixture$collection$sample_manifest)
  expect_identical(entry$extras$condition_summary,
                   fixture$collection$condition_summary)
  expect_identical(entry$extras$by_sample,
                   build_liana_collection_table(fixture$collection))
  expect_identical(fixture$counter$calls, 0L)
  expect_identical(state_get(shared_rv, "communication_collection"),
                   collection_before)
  expect_identical(state_get(shared_rv, "communication_result"), result_before)
  expect_identical(state_get(shared_rv, "active_communication_sample"),
                   active_before)
  expect_identical(shiny::isolate(shared_rv$provenance), provenance_before)

  validation <- validate_consolidated_report_input(ri)
  expect_identical(validation$verdicts$communication$state, "valid")
})

test_that("LIANA collection summary, descriptive tables and bundle files are rendered and exported", {
  fixture <- .rep_liana_collection_fixture()
  active_sample <- "TREATMENT::T03"
  shared_rv <- .rep_liana_shared(fixture, active_sample)
  ri <- collect_consolidated_report_input(fixture$obj, shared_rv)
  validation <- validate_consolidated_report_input(ri)
  html_path <- tempfile("rep_liana_", fileext = ".html")
  on.exit(unlink(html_path), add = TRUE)
  write_consolidated_report_html(ri, validation, html_path)
  html <- .rep_html_text(html_path)

  expect_match(html, "Communication cellulaire</h3>", fixed = TRUE)
  expect_false(grepl("Communication cellulaire (import)", html, fixed = TRUE))
  expect_match(html, "Manifeste des échantillons", fixed = TRUE)
  expect_match(html, "Résumé descriptif des conditions", fixed = TRUE)
  expect_match(html, "Interactions canoniques par échantillon", fixed = TRUE)
  expect_match(html, "l'échantillon biologique est l'unité de réplication",
               fixed = TRUE)
  expect_match(html, "les cellules ne sont pas des réplicats", fixed = TRUE)
  expect_match(html, "Aucun test entre conditions ni conclusion causale",
               fixed = TRUE)

  bundle_dir <- tempfile("bundle_liana_")
  on.exit(unlink(bundle_dir, recursive = TRUE), add = TRUE)
  bundle <- build_report_bundle(bundle_dir, ri, validation)
  expected_files <- c(
    "communication_canonical.csv",
    "communication_sample_manifest.csv",
    "communication_condition_summary.csv",
    "communication_by_sample.csv"
  )
  expect_true(all(file.exists(file.path(
    bundle_dir, "tables", expected_files
  ))))
  expect_true(all(file.exists(file.path(bundle_dir, bundle$files))))
  bundle_manifest <- utils::read.csv(
    file.path(bundle_dir, "manifest_sections.csv"),
    check.names = FALSE
  )
  communication_rows <- bundle_manifest[bundle_manifest$section == "communication", ]
  expect_true(all(nzchar(communication_rows$analysis_ids)))
  expect_true(any(grepl("sc-communication-liana", communication_rows$analysis_ids)))
  expect_identical(fixture$counter$calls, 0L)

  manifest <- utils::read.csv(file.path(
    bundle_dir, "tables", "communication_sample_manifest.csv"
  ), check.names = FALSE)
  conditions <- utils::read.csv(file.path(
    bundle_dir, "tables", "communication_condition_summary.csv"
  ), check.names = FALSE)
  by_sample <- utils::read.csv(file.path(
    bundle_dir, "tables", "communication_by_sample.csv"
  ), check.names = FALSE)
  active <- utils::read.csv(file.path(
    bundle_dir, "tables", "communication_canonical.csv"
  ), check.names = FALSE)
  expect_identical(nrow(manifest), 5L)
  expect_identical(nrow(conditions), 2L)
  expect_identical(nrow(by_sample), 10L)
  expect_true(all(c("sample_id", "condition", "sample_key") %in%
                    colnames(by_sample)))
  expect_identical(nrow(active), 2L)
  expect_identical(unique(active$sample_id), "T03")
})

test_that("a stale active sample invalidates a valid collection verdict", {
  fixture <- .rep_liana_collection_fixture()
  shared_rv <- .rep_liana_shared(fixture, "CONTROL::C01")
  shared_rv$active_communication_sample <- "TREATMENT::missing"
  ri <- collect_consolidated_report_input(fixture$obj, shared_rv)
  expect_false(ri$analyses$communication$identity_ok)
  expect_identical(
    validate_consolidated_report_input(ri)$verdicts$communication$state,
    "stale"
  )
})

test_that("an invalid LIANA collection falls back to the existing active result without rerun", {
  fixture <- .rep_liana_collection_fixture()
  active_sample <- "CONTROL::C01"
  shared_rv <- .rep_liana_shared(fixture, active_sample)
  active_result <- state_get(shared_rv, "communication_result")
  invalid_collection <- fixture$collection
  invalid_collection$sample_manifest$sample_key[[2L]] <-
    invalid_collection$sample_manifest$sample_key[[1L]]
  shared_rv$communication_collection <- invalid_collection

  ri <- collect_consolidated_report_input(fixture$obj, shared_rv)
  entry <- ri$analyses$communication
  expect_identical(entry$analysis_ids, active_result$analysis_id)
  expect_identical(entry$extras$canonical_table,
                   active_result$canonical_table)
  expect_false("n_echantillons" %in% entry$summary$champ)
  expect_null(entry$extras$sample_manifest)
  expect_null(entry$extras$condition_summary)
  expect_null(entry$extras$by_sample)
  expect_match(entry$extras$collection_error, "echantillons", fixed = TRUE)
  expect_identical(fixture$counter$calls, 0L)
})

test_that("imported rank direction and aggregation mode are exposed by the report", {
  obj <- .rep_stub_obj()
  parsed <- parse_liana_import(.comm_liana_tab(), "mean_rank", "specificity")
  result <- .comm_import_and_finalize(parsed, seurat_obj = obj)
  shared_rv <- create_sc_shared_state()
  shared_rv$communication_result <- result
  entry <- collect_consolidated_report_input(obj, shared_rv)$analyses$communication
  values <- setNames(entry$summary$valeur, entry$summary$champ)
  expect_identical(values[["direction_rang"]], "lower_is_better")
  expect_identical(values[["aggregation_rang"]], "specificity")
})

test_that("legacy communication remains unchanged when no LIANA collection exists", {
  obj <- .rep_stub_obj()
  result <- .rep_communication_canonical(obj)
  shared_rv <- create_sc_shared_state()
  shared_rv$communication_result <- result
  ri <- collect_consolidated_report_input(obj, shared_rv)
  entry <- ri$analyses$communication
  expected_summary <- .report_kv_df(c(
    statut = result$status,
    methode_source = result$source_method,
    n_lignes_entree = result$input_summary$n_rows_input,
     n_lignes_canoniques = result$input_summary$n_rows_canonical,
     colonne_identite = result$identity_column,
     direction_rang = "",
     aggregation_rang = ""
  ))

  expect_identical(entry$summary, expected_summary)
  expect_identical(entry$analysis_ids, result$analysis_id)
  expect_named(entry$extras, "canonical_table")
  expect_identical(entry$extras$canonical_table, result$canonical_table)
  validation <- validate_consolidated_report_input(ri)
  html_path <- tempfile("rep_legacy_communication_", fileext = ".html")
  bundle_dir <- tempfile("bundle_legacy_communication_")
  on.exit(unlink(c(html_path, bundle_dir), recursive = TRUE), add = TRUE)
  write_consolidated_report_html(ri, validation, html_path)
  html <- .rep_html_text(html_path)
  expect_match(html, "Communication cellulaire</h3>", fixed = TRUE)
  expect_false(grepl("Manifeste des échantillons", html, fixed = TRUE))
  expect_false(grepl("Résumé descriptif des conditions", html,
                     fixed = TRUE))
  bundle <- build_report_bundle(bundle_dir, ri, validation)
  expect_true(file.exists(file.path(bundle_dir, "tables",
                                    "communication_canonical.csv")))
  expect_false(any(c(
    "communication_sample_manifest.csv",
    "communication_condition_summary.csv",
    "communication_by_sample.csv"
  ) %in% basename(bundle$files)))
})

# ── 4. Obsolete : l'objet courant a change depuis le calcul ─────────────────
test_that("stale identity after object change produces an explicit stale banner", {
  old_obj <- .rep_stub_obj()
  shared_rv <- create_sc_shared_state()
  shared_rv$velocity_result <- .rep_velocity_canonical(.rep_stub_obj())  # empreinte identique
  # L'objet courant CHANGE (une cellule de plus) -> empreinte divergente.
  new_obj <- matrix(0, nrow = 5, ncol = 161,
                    dimnames = list(paste0("gene", 1:5),
                                    c(colnames(.rep_stub_obj()), "cell161")))
  ri <- collect_consolidated_report_input(new_obj, shared_rv)
  val <- validate_consolidated_report_input(ri)
  expect_identical(val$verdicts$velocity$state, "stale")

  html_path <- file.path(tempdir(), "rep_stale.html")
  write_consolidated_report_html(ri, val, html_path)
  expect_match(.rep_html_text(html_path), "obsolète", fixed = TRUE)
  unlink(html_path)
})

test_that("unverifiable identity (no fingerprint) maps to the unknown state", {
  obj <- .rep_stub_obj()
  shared_rv <- create_sc_shared_state()
  vel <- .rep_velocity_canonical(obj)
  vel$object_identity <- list(fingerprint = NULL, method = "v2")
  shared_rv$velocity_result <- vel
  ri <- collect_consolidated_report_input(obj, shared_rv)
  val <- validate_consolidated_report_input(ri)
  expect_identical(val$verdicts$velocity$state, "unknown")
})

# ── 5. Provenance absente : section REFUSEE (regle 7) ───────────────────────
test_that("missing provenance blocks the section and is stated in the HTML", {
  obj <- .rep_stub_obj()
  shared_rv <- create_sc_shared_state()
  comm <- .rep_communication_canonical(obj)
  comm$provenance <- NULL
  shared_rv$communication_result <- comm
  ri <- collect_consolidated_report_input(obj, shared_rv)
  val <- validate_consolidated_report_input(ri)
  expect_identical(val$verdicts$communication$state, "blocked")
  expect_identical(val$blocked_sections, "communication")
  expect_false(val$ok_overall)

  html_path <- file.path(tempdir(), "rep_blocked.html")
  write_consolidated_report_html(ri, val, html_path)
  txt <- .rep_html_text(html_path)
  expect_match(txt, "Section refusée : provenance absente", fixed = TRUE)
  unlink(html_path)
})

test_that("invalid DA design status is flagged invalid, not valid", {
  obj <- .rep_stub_obj()
  shared_rv <- create_sc_shared_state()
  des <- .rep_design_canonical(obj)
  des$status <- "invalid_design"
  shared_rv$da_design_result <- des
  ri <- collect_consolidated_report_input(obj, shared_rv)
  val <- validate_consolidated_report_input(ri)
  expect_identical(val$verdicts$da_design$state, "invalid")
})

# ── 6. Bundle : copies fideles + manifeste, aucune donnee brute ──────────────
test_that("bundle writes report, manifest, provenance, tables and README", {
  obj <- .rep_stub_obj()
  ri <- collect_consolidated_report_input(obj, .rep_full_shared(obj))
  val <- validate_consolidated_report_input(ri)
  bundle_dir <- file.path(tempdir(), paste0("bundle_", as.integer(Sys.time())))
  on.exit(unlink(bundle_dir, recursive = TRUE), add = TRUE)
  bundle <- build_report_bundle(bundle_dir, ri, val)

  expect_true(file.exists(bundle$html_path))
  expect_true(file.exists(file.path(bundle_dir, "manifest_sections.csv")))
  expect_true(file.exists(file.path(bundle_dir, "provenance.csv")))
  expect_true(file.exists(file.path(bundle_dir, "README.txt")))
  expect_true(file.exists(file.path(bundle_dir, "tables", "marqueurs.csv")))
  expect_true(file.exists(file.path(bundle_dir, "tables", "correlation.csv")))
  expect_true(file.exists(file.path(bundle_dir, "tables", "pseudobulk_de.csv")))
  expect_true(file.exists(file.path(bundle_dir, "tables",
                                    "communication_canonical.csv")))
  expect_true(file.exists(file.path(bundle_dir, "tables", "milo_da_table.csv")))
  # Tables = copies fideles (meme nombre de lignes que le canonique)
  milo_csv <- utils::read.csv(file.path(bundle_dir, "tables", "milo_da_table.csv"))
  expect_identical(nrow(milo_csv), 3L)
  # Manifeste : une ligne par verdict + fichiers exportes
  manifest <- utils::read.csv(file.path(bundle_dir, "manifest_sections.csv"))
  expect_true(all(consolidated_report_analyses() %in% manifest$section))
  # Aucune matrice brute n'est exportee : nhood_assignment per-cellule exclu.
  expect_false(file.exists(file.path(bundle_dir, "tables",
                                     "milo_nhood_assignment.csv")))
})

# ── 6b. 4F-EXT : domaine global bulk_multi_comparison ────────────────────────
.rep_bulk_multi_result <- function() {
  list(
    datasets = c("Jeu_A", "pseudobulk_T2_vs_ctrl"),
    contrast = "T2_vs_ctrl",
    lfc_thresh = 1, padj_thresh = 0.05,
    deg_sets = list(Jeu_A = c("G1", "G2"), pseudobulk_T2_vs_ctrl = "G1"),
    up_down_sets = list(),
    intersection_dt = data.frame(gene = "G1", Jeu_A = 1L, pseudo = 1L,
                                 stringsAsFactors = FALSE),
    concordance = data.frame(dataset_a = "Jeu_A",
                             dataset_b = "pseudobulk_T2_vs_ctrl",
                             jaccard_up = 0.5, jaccard_down = NA_real_,
                             n_common_sig = 1L, pct_same_direction = 100,
                             stringsAsFactors = FALSE),
    volcano_scales = list(x = c(-3, 3), y = c(0, 4)),
    per_dataset = data.frame(label = c("Jeu_A", "pseudobulk_T2_vs_ctrl"),
                             n_tested = c(5L, 5L), n_up = c(2L, 1L),
                             n_down = c(1L, 0L),
                             stored_lfc_thresh = c(1, 1),
                             stored_padj_thresh = c(0.05, 0.05),
                             stringsAsFactors = FALSE),
    ran_at = Sys.time()
  )
}

test_that("4F-EXT: global domain bulk_multi_comparison is collected, validated and bundled", {
  obj <- .rep_stub_obj()
  gd <- list(bulk_multi_comparison = .rep_bulk_multi_result())

  # Sans global_data (défaut) : section absente — comportement d'origine.
  ri0 <- collect_consolidated_report_input(obj)
  val0 <- validate_consolidated_report_input(ri0)
  expect_identical(val0$verdicts$bulk_multi_comparison$state, "absent")

  # Avec global_data : present, verdict valid_legacy à libellé dédié.
  ri <- collect_consolidated_report_input(obj, create_sc_shared_state(),
                                          global_data = gd)
  expect_true(ri$analyses$bulk_multi_comparison$present)
  expect_identical(ri$analyses$bulk_multi_comparison$analysis_ids,
                   "bulk-multi-compare")
  expect_false(ri$analyses$bulk_multi_comparison$identity_checked)
  expect_true(is.na(ri$analyses$bulk_multi_comparison$identity_ok))
  val <- validate_consolidated_report_input(ri)
  expect_identical(val$verdicts$bulk_multi_comparison$state, "valid_legacy")
  expect_match(val$verdicts$bulk_multi_comparison$label, "auto-daté",
               fixed = TRUE)
  s <- ri$analyses$bulk_multi_comparison$summary
  expect_match(s$valeur[s$champ == "contraste"], "T2_vs_ctrl", fixed = TRUE)
  expect_identical(s$valeur[s$champ == "n_datasets"], "2")

  # Garde de forme : résultat partial -> section absente (jamais "réparé").
  gd_bad <- list(bulk_multi_comparison = list(datasets = "x"))
  ri_bad <- collect_consolidated_report_input(obj, global_data = gd_bad)
  expect_false(ri_bad$analyses$bulk_multi_comparison$present)

  # Bundle : 3 tables, copies fidèles.
  bundle_dir <- file.path(tempdir(),
                          paste0("bundle_ext_", as.integer(Sys.time())))
  on.exit(unlink(bundle_dir, recursive = TRUE), add = TRUE)
  build_report_bundle(bundle_dir, ri, val)
  expect_true(file.exists(file.path(bundle_dir, "tables",
                                    "bulk_multi_per_dataset.csv")))
  expect_true(file.exists(file.path(bundle_dir, "tables",
                                    "bulk_multi_concordance.csv")))
  expect_true(file.exists(file.path(bundle_dir, "tables",
                                    "bulk_multi_intersection.csv")))
  per <- utils::read.csv(file.path(bundle_dir, "tables",
                                   "bulk_multi_per_dataset.csv"))
  expect_identical(nrow(per), 2L)

  # Rendu HTML : section titrée, analysis_id affiché, jamais bloquée.
  html_path <- file.path(tempdir(), "rep_ext.html")
  on.exit(unlink(html_path), add = TRUE)
  write_consolidated_report_html(ri, val, html_path)
  txt <- .rep_html_text(html_path)
  expect_match(txt, "Comparaison multi-jeux Bulk (DEGs)", fixed = TRUE)
  expect_match(txt, "bulk-multi-compare", fixed = TRUE)
})

# ── 7. Nommage d'export + recap ──────────────────────────────────────────────
test_that("export filename follows the domain pattern", {
  fn <- consolidated_report_export_filename("rapport_consolide", "html")
  expect_match(fn, "^rapport_consolide_sc-report-consolide_[0-9]{4}-[0-9]{2}-[0-9]{2}\\.html$")
})

test_that("recap summarises the collected input without recomputing", {
  obj <- .rep_stub_obj()
  ri <- collect_consolidated_report_input(obj, .rep_full_shared(obj))
  recap <- consolidated_report_input_recap(ri)
  expect_match(recap, "sc-report-consolide")
  expect_match(recap, "velocity")
  expect_match(recap, "provenance", ignore.case = TRUE)
})

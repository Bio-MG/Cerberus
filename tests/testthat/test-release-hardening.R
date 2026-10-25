# =============================================================================
# test-release-hardening.R — DURCISSEMENT FULL RELEASE (Stage 18)
# =============================================================================
# Matrice : docs/release/HARDENING_MATRIX.md. Ce fichier couvre AUTOMATIQUEMENT
# les categories applicables hors CI (fixtures synthetiques uniquement, aucune
# donnee biologique reelle) ; les categories "MANUAL-SMOKE" / "SYNTHETIC-ONLY"
# y sont marquées et documentées dans la matrice.
#
# Catégories testées ici :
#   C1  correct input                        C8  large dataset / BPCells (caps)
#   C2  missing input                        C9  interrupted export (garde-fous)
#   C3  malformed input                      C10 stale result after object change
#   C4  mismatched metadata                  C11 empty result
#   C5  insufficient biological replication  C12 repeated execution / state reset
#   C6  missing optional package             C13 reproducibility (same inputs)
#   C7  failed async / heavy task            C14 UI explanation of limitations
#   +  stress "rapport avec beaucoup de résultats" (plafonds inline vs export)
# =============================================================================

source_project_file("R/sc/sc_communication.R")
source_project_file("R/sc/sc_abundance_milo.R")
source_project_file("R/sc/sc_abundance_sccoda.R")
source_project_file("R/core/jobs.R")      # C7 : wrapper sync/async
source_project_file("R/core/caching.R")   # C12 : reset du cache
# M5 lot 2 : les 4 fichiers du domaine rapport vivent dans
# tools/pkg_skeleton/R/ — ce fichier les source TOUJOURS à l'ancienne mode
# (globalenv), au NOUVEAU chemin. DÉCISION MESURÉE : PAS de bascule
# load_project_pkg() ici — C10/C13 exigent de vrais verdicts "stale"
# (velocity_result_is_stale, milo_result_is_stale), or un namespace ne
# résout PAS via globalenv (leçon M4) : appelé depuis le paquet, le
# collecteur avalerait l'erreur de résolution dans ses tryCatch et
# dériverait stale -> unknown / valid -> unknown. Les 9 fournisseurs R/sc
# doivent rejoindre le paquet AVANT cette bascule (M5 lot 3+). Le double
# chemin de chargement est l'état conçu (cf. test-plotting-pkg-pilot.R).
# TROU D'ISOLATION préexistant comblé au passage (invisible en suite
# complète par accumulation, rouge en run FILTRÉ — le même motif que le
# lot 1) : error_log.R (ts_log_swallow, appelé par les handlers tryCatch
# du collecteur) manquait à la liste des fournisseurs — test-sc-report-
# contract.R le sourceait déjà depuis M-1.
source_project_file("tools/pkg_skeleton/R/error_log.R")
source_project_file("tools/pkg_skeleton/R/report_collector.R")
source_project_file("tools/pkg_skeleton/R/report_validator.R")
source_project_file("tools/pkg_skeleton/R/report_render.R")
source_project_file("tools/pkg_skeleton/R/report_bundle.R")

.hard_stub_obj <- function(n_cells = 160L) {
  matrix(0, nrow = 5, ncol = n_cells,
         dimnames = list(paste0("gene", 1:5), paste0("cell_", seq_len(n_cells))))
}

.hard_velocity <- function(seurat_obj) {
  velocity_input <- list(spliced = .vel_mat(), unspliced = .vel_mat() * 2L)
  validated <- .vel_validate_and_enrich(velocity_input)
  finalize_velocity_result(validated = validated, input_mode = "rds",
                           seurat_obj = seurat_obj, requested_reduction = "umap",
                           analysis_id = "sc-velocity")
}

.hard_comm <- function(seurat_obj) {
  parsed <- parse_cellchat_import(.comm_cellchat_tab(),
                                  source_file = "cellchat_export.csv")
  .comm_import_and_finalize(parsed, seurat_obj = seurat_obj)
}

.hard_design <- function(meta, seurat_obj) {
  validated <- validate_da_design(metadata = meta, sample_id = "sample_id",
                                  condition = "condition",
                                  replicate_id = "replicate_id",
                                  batch = "batch", identity = "cell_type")
  finalize_da_design_result(validated = validated, seurat_obj = seurat_obj)
}

# ── C1 : entrée correcte — les gardes de contrat acceptent les canoniques ───
test_that("C1 correct input: contract guards accept fresh canonical results", {
  obj <- .hard_stub_obj()
  vel <- .hard_velocity(obj)
  expect_invisible(assert_velocity_result(vel))
  comm <- .hard_comm(obj)
  expect_invisible(assert_communication_result(comm))
  design <- .hard_design(.da_meta(), obj)
  expect_invisible(assert_da_design_result(design, method = "milo"))
  expect_invisible(assert_da_design_result(design, method = "sccoda"))
})

# ── C2 : entrée manquante — erreurs classées, jamais de repli silencieux ────
test_that("C2 missing input: classed French errors and NA-safe staleness", {
  e <- tryCatch(collect_consolidated_report_input(NULL), error = function(e) e)
  expect_s3_class(e, "report_error")
  expect_s3_class(tryCatch(build_consolidated_report_html(list(), list()),
                           error = function(e) e), "report_error")
  # Staleness sans objet courant : NA (invérifiable), jamais FALSE mensonger.
  expect_true(is.na(velocity_result_is_stale(.hard_velocity(.hard_stub_obj()), NULL)))
  expect_null(state_get(NULL, "n_importe"))
  expect_null(build_object_identity_v2(NULL)$fingerprint)
})

# ── C3 : entrée malformée — schémas rejetés explicitement ────────────────────
test_that("C3 malformed input: import schemas rejected with classed errors", {
  e1 <- tryCatch(parse_cellchat_import(NULL), error = function(e) e)
  expect_s3_class(e1, "communication_import_error")
  e2 <- tryCatch(parse_cellchat_import(data.frame()), error = function(e) e)
  expect_s3_class(e2, "communication_import_error")
  expect_match(conditionMessage(e1), ".", ignore.case = TRUE)  # message non vide
})

# ── C4 : métadonnées incohérentes — cellules sans sample_id EXCLUES et comptées
test_that("C4 mismatched metadata: missing sample_id cells are excluded and counted", {
  meta <- .da_meta(na_sample_id = 5L)
  design <- .hard_design(meta, .hard_stub_obj(nrow(meta)))
  excl <- design$exclusions
  expect_true(is.data.frame(excl) && sum(excl$n_rows) == 5L)
  expect_true(is.data.frame(design$missingness) && nrow(design$missingness) > 0L)
})

# ── C5 : réplication biologique insuffisante — pseudoreplication BLOQUÉE ────
test_that("C5 insufficient replication: pseudoreplication blocked end-to-end", {
  meta1 <- .da_meta(samples = c(s1 = 30, s2 = 30),
                    conditions = c(s1 = "A", s2 = "B"),
                    replicates = c(s1 = "s1", s2 = "s2"),
                    batch = c(s1 = "b1", s2 = "b1"))
  design <- .hard_design(meta1, .hard_stub_obj(nrow(meta1)))
  expect_identical(design$status, "invalid_design")
  expect_false(isTRUE(design$milo_eligibility$eligible))
  expect_false(isTRUE(design$sccoda_eligibility$eligible))
  # Le garde refuse de laisser Milo consommer ce design.
  e <- tryCatch(assert_da_design_result(design, method = "milo"),
                error = function(e) e)
  expect_s3_class(e, "da_design_error")
  # ... et le rapport le drapeaute INVALID (jamais "valide").
  shared_rv <- create_sc_shared_state()
  shared_rv$da_design_result <- design
  val <- validate_consolidated_report_input(
    collect_consolidated_report_input(.hard_stub_obj(nrow(meta1)), shared_rv))
  expect_identical(val$verdicts$da_design$state, "invalid")
})

# ── C6 : package optionnel absent — environnement scCODA explicite ──────────
test_that("C6 missing optional package: scCODA environment missing is loud", {
  e <- tryCatch(
    sccoda_available(resolver = function() list(available = FALSE,
                                                python_path = NA_character_,
                                                source = "test"),
                     stop_if_missing = TRUE),
    error = function(e) e)
  expect_s3_class(e, "sccoda_error")
  expect_identical(e$state, "environment_missing")
  expect_match(conditionMessage(e), "sccoda", ignore.case = TRUE)
})

# ── C7 : tâche lourde/async en échec — repli synchrone transparent ──────────
test_that("C7 failed async/heavy task: sync fallback warns, on_error absorbs", {
  # Erreur de fn + on_error fourni -> NULL invisible, pas de crash.
  res1 <- run_job(function() stop("boom"), on_error = function(e) NULL)
  expect_null(res1)
  # async=TRUE sans pool -> warning + repli synchrone (résultat quand même produit).
  expect_warning(res2 <- run_job(function() 42, async = TRUE),
                 regexp = "synchrone de secours")
  expect_identical(res2, 42)
})

# ── C8 : gros datasets / BPCells — caps déclarés, chemins présents ──────────
test_that("C8 large data paths: declared caps are coherent (synthetic-only tier)", {
  caps <- c(TS_SKETCH_FAST, TS_SKETCH_LIGHT, TS_SKETCH_MEDIUM,
            TS_SKETCH_STANDARD, TS_SKETCH_HIGH)
  expect_true(all(diff(caps) > 0), info = "les presets sketch doivent être croissants")
  expect_gt(TS_VELOCITY_MAX_PORTRAIT_CELLS, TS_VELOCITY_MAX_EMBED_CELLS)
  # Backend disque (BPCells) : le chemin existe (R/sc/sc_bpcells.R, mod_sc
  # .BPCELLS_AUTO_THRESHOLD) — le test sur données réelles est hors CI,
  # documenté SYNTHETIC-ONLY dans la matrice de durcissement.
  expect_true(file.exists(file.path(ts_project_root(), "R", "sc", "sc_bpcells.R")))
})

# ── C9 : export interrompu — garde-fous d'entrée + zip intègre ──────────────
test_that("C9 interrupted export: bundle guards + zip integrity", {
  e <- tryCatch(build_report_bundle(tempdir(), list(), list()),
                error = function(e) e)
  expect_s3_class(e, "report_error")
  obj <- .hard_stub_obj()
  ri <- collect_consolidated_report_input(obj)
  val <- validate_consolidated_report_input(ri)
  bd <- file.path(tempdir(), paste0("hard_bundle_", as.integer(Sys.time())))
  on.exit(unlink(bd, recursive = TRUE), add = TRUE)
  bundle <- build_report_bundle(bd, ri, val)
  zp <- tempfile(fileext = ".zip")
  on.exit(unlink(zp), add = TRUE)
  zip::zip(zp, files = file.path(bundle$bundle_dir, bundle$files),
           mode = "cherry-pick")
  expect_true(file.exists(zp))
  expect_gt(file.info(zp)$size, 0)
})

# ── C10 : résultat obsolète après changement de l'objet source ──────────────
test_that("C10 stale results: velocity and Milo flagged stale simultaneously", {
  old_obj <- .hard_stub_obj(n_cells = 160L)
  shared_rv <- create_sc_shared_state()
  shared_rv$velocity_result <- .hard_velocity(old_obj)
  milo <- list(
    type = "milo_da", status = "valid", analysis_id = "sc-milo-hard",
    tested_contrast = list(target = "B", reference = "A"),
    parameters = list(reduction = "umap", seed = 14L),
    neighbourhood_summary = data.frame(n_neighbourhoods = 1L,
                                       n_cells_in_nhoods = 10L,
                                       fraction_cells_in_nhoods = 0.5,
                                       median_nhood_size = 10, min_nhood_size = 10,
                                       max_nhood_size = 10),
    DA_table = data.frame(Nhood = 1L, n_cells = 10L, logFC = 0.1, logCPM = 1,
                          F = 1, PValue = 0.5, FDR = 0.6, SpatialFDR = 0.6,
                          identity = "B", identity_fraction = 0.9),
    sample_composition = data.frame(sample = "s1", condition = "A", batch = "b1",
                                    n_cells_total = 40L, n_cells_in_nhoods = 10L),
    object_identity = list(fingerprint = velocity_object_fingerprint(old_obj)),
    provenance = new_provenance_entry(analysis_id = "sc-milo-hard",
                                      method = "milo", dataset = old_obj))
  shared_rv$da_milo_result <- milo
  new_obj <- .hard_stub_obj(n_cells = 161L)  # l'objet courant a changé
  val <- validate_consolidated_report_input(
    collect_consolidated_report_input(new_obj, shared_rv))
  expect_identical(val$verdicts$velocity$state, "stale")
  expect_identical(val$verdicts$da_milo$state, "stale")
})

# ── C11 : résultat vide — gracieux, jamais un faux positif ──────────────────
test_that("C11 empty results: zero-row state fields compile as absent sections", {
  shared_rv <- create_sc_shared_state()
  shared_rv$markers_data <- data.frame()
  shared_rv$pathway_results <- data.frame()
  val <- validate_consolidated_report_input(
    collect_consolidated_report_input(.hard_stub_obj(), shared_rv))
  expect_identical(val$verdicts$markers$state, "absent")
  expect_identical(val$verdicts$pathways$state, "absent")
  expect_true(val$ok_overall)
})

# ── C12 : exécution répétée / reset d'état — compilation idempotente ────────
test_that("C12 repeated execution and state reset: identical verdicts, clean cache", {
  obj <- .hard_stub_obj()
  shared_rv <- create_sc_shared_state()
  shared_rv$markers_data <- data.frame(gene = "g1", cluster = "1")
  val1 <- validate_consolidated_report_input(
    collect_consolidated_report_input(obj, shared_rv))
  val2 <- validate_consolidated_report_input(
    collect_consolidated_report_input(obj, shared_rv))
  states1 <- vapply(val1$verdicts, function(v) v$state, character(1))
  states2 <- vapply(val2$verdicts, function(v) v$state, character(1))
  expect_identical(states1, states2)
  # Reset d'état : un nouvel état partagé repart de zéro (tous absents).
  fresh <- validate_consolidated_report_input(
    collect_consolidated_report_input(obj, create_sc_shared_state()))
  expect_true(all(vapply(fresh$verdicts, function(v) v$state == "absent",
                         logical(1))))
  # Cache : clear idempotent.
  cerberus_cache_set(cerberus_cache_key("trajectory", "k"), 1)
  cerberus_cache_clear()
  expect_null(cerberus_cache_get(cerberus_cache_key("trajectory", "k")))
})

# ── C13 : reproductibilité — mêmes entrées => mêmes empreintes ──────────────
test_that("C13 reproducibility: same inputs produce identical identities", {
  obj <- .hard_stub_obj()
  v1 <- .hard_velocity(obj)
  v2 <- .hard_velocity(obj)
  expect_identical(v1$object_identity$fingerprint, v2$object_identity$fingerprint)
  d1 <- .hard_design(.da_meta(), obj)
  d2 <- .hard_design(.da_meta(), obj)
  expect_identical(d1$object_identity$fingerprint, d2$object_identity$fingerprint)
  # L'empreinte du rapport est celle de l'objet — cohérence inter-domaines.
  ri <- collect_consolidated_report_input(obj)
  expect_identical(ri$dataset$fingerprint$fingerprint,
                   velocity_object_fingerprint(obj))
})

# ── C14 : explication UI des hypothèses / limitations ───────────────────────
test_that("C14 UI explanation: limitation and assumption texts are present", {
  report_mod <- paste(readLines(file.path(ts_project_root(),
    "modules", "sc", "mod_sc_report_consolidated.R")), collapse = "\n")
  expect_match(report_mod, "Aucune analyse n'est ré-exécutée", fixed = TRUE)
  mod_sc_src <- paste(readLines(file.path(ts_project_root(),
    "modules", "sc", "mod_sc.R")), collapse = "\n")
  expect_match(mod_sc_src, "PDF requiert tinytex", fixed = TRUE)
  viz_src <- paste(readLines(file.path(ts_project_root(),
    "modules", "sc", "mod_sc_viz.R")), collapse = "\n")
  # Bloc 3 : fonctionnalités livrées annoncées comme disponibles (mission 4F).
  expect_match(viz_src, " Disponible : vélocité ARN", fixed = TRUE)
})

# ── Stress : rapport avec BEAUCOUP de résultats (plafonds vs exports) ───────
test_that("stress many-results: inline provenance capped, bundle export complete", {
  shared_rv <- create_sc_shared_state()
  obj <- .hard_stub_obj()
  for (i in seq_len(1200L)) {
    provenance_append(shared_rv, new_provenance_entry(
      analysis_id = sprintf("sc-stress-%04d", i), method = "stress", dataset = obj))
  }
  ri <- collect_consolidated_report_input(obj, shared_rv)
  expect_identical(nrow(ri$provenance_df), 1200L)
  val <- validate_consolidated_report_input(ri)
  html_path <- file.path(tempdir(), "rep_stress.html")
  on.exit(unlink(html_path), add = TRUE)
  write_consolidated_report_html(ri, val, html_path)
  txt <- paste(readLines(html_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # Affichage plafonné à TS_REPORT_MAX_PROVENANCE_ROWS, sans tronquer la source.
  expect_match(txt, "Affichage plafonné à 500 lignes", fixed = TRUE)
  bd <- file.path(tempdir(), paste0("stress_bundle_", as.integer(Sys.time())))
  on.exit(unlink(bd, recursive = TRUE), add = TRUE)
  build_report_bundle(bd, ri, val)
  prov_csv <- utils::read.csv(file.path(bd, "provenance.csv"))
  expect_identical(nrow(prov_csv), 1200L)  # l'export n'est JAMAIS plafonné
})

# ── C15 (ajout 2026-09-15) : portée de `ns` dans les serveurs de modules ────
# Bug rencontré en production : `mod_sc_pathways.R` appelait `ns(...)` dans un
# renderUI alors que `ns` n'est lié QUE dans les fonctions UI (`NS(id)`). Le
# serveur ne le liait pas ⇒ « impossible de trouver la fonction "ns" », levé
# uniquement quand la branche s'affiche (donc invisible au démarrage, et
# invisible aux tests qui ne rendent pas l'UI). Même bug dans `mod_sc.R`.
# Cette garde est STATIQUE : elle attrape la classe entière, pas ces deux cas.
test_that("C15 module scope: any server calling ns() must bind it (session$ns)", {
  root <- ts_project_root()
  files <- sort(list.files(file.path(root, "modules"), pattern = "\\.R$",
                           recursive = TRUE, full.names = TRUE))
  expect_true(length(files) > 0L)

  # `ns(` nu — exclut volontairement `session$ns(`, qui EST le bon motif.
  ns_call <- "(^|[^A-Za-z0-9_.$])ns\\s*\\("
  ns_bind <- "(^|[^A-Za-z0-9_.$])ns\\s*<-"

  offenders <- character(0)
  for (f in files) {
    e <- tryCatch(parse(f, keep.source = FALSE), error = function(e) NULL)
    if (is.null(e)) next
    for (top in as.list(e)) {
      if (!(is.call(top) && identical(deparse(top[[1L]]), "<-") &&
            is.symbol(top[[2L]]))) next
      rhs <- top[[3L]]
      if (!(is.call(rhs) && identical(deparse(rhs[[1L]]), "function"))) next
      body <- paste(deparse(rhs), collapse = "\n")
      if (!grepl("moduleServer", body, fixed = TRUE)) next
      if (!grepl(ns_call, body)) next
      # Soit `ns` est lié dans la fonction, soit aucun appel nu n'existe.
      if (grepl(ns_bind, body)) next
      offenders <- c(offenders, paste0(basename(f), "::", as.character(top[[2L]])))
    }
  }
  expect_identical(
    offenders, character(0),
    label = paste0("serveur(s) de module appelant ns() sans le lier ",
                   "(ajouter `ns <- session$ns` en tête du serveur)")
  )
})

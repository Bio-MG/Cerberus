# =============================================================================
# scripts/benchmark_stage18.R — Baseline performance (Stage 18, 4F+release)
# =============================================================================
# Mesures SYNTHETIQUES (aucune donnee biologique reelle) sur la machine de
# reference. Resultats a copier dans docs/release/PERFORMANCE_BASELINE.md.
# Lancer depuis la racine projet :
#   Rscript scripts/benchmark_stage18.R
# =============================================================================

source("tools/pkg_skeleton/R/defaults.R")
# D2 M5 lot 1 : state/io_helpers/provenance vivent dans tools/pkg_skeleton/R/
source("tools/pkg_skeleton/R/state.R"); source("tools/pkg_skeleton/R/io_helpers.R")
source("tools/pkg_skeleton/R/validation.R")
source("tools/pkg_skeleton/R/provenance.R"); source("tools/pkg_skeleton/R/jobs.R"); source("tools/pkg_skeleton/R/caching.R")
# D2 M5 lot 6 : le domaine R/sc ENTIER vit dans tools/pkg_skeleton/R/
source("tools/pkg_skeleton/R/sc_velocity.R"); source("tools/pkg_skeleton/R/sc_abundance_design.R")
source("tools/pkg_skeleton/R/sc_abundance_sccoda.R")
# D2 M5 lot 2 : le domaine R/reports ENTIER vit dans tools/pkg_skeleton/R/
# (app.R le source() au nouveau chemin ; ce script de mesure suit le chemin).
# error_log.R ajoute AUSSI (trou d'isolation preexistant, comble au passage —
# meme motif que test-release-hardening.R ce lot) : les handlers tryCatch du
# collecteur appellent ts_log_swallow(), absent de la liste ci-dessus.
source("tools/pkg_skeleton/R/error_log.R")
source("tools/pkg_skeleton/R/report_collector.R"); source("tools/pkg_skeleton/R/report_validator.R")
source("tools/pkg_skeleton/R/report_render.R"); source("tools/pkg_skeleton/R/report_bundle.R")

.time <- function(label, expr, reps = 5L) {
  timings <- replicate(reps, system.time(expr)[["elapsed"]], simplify = TRUE)
  cat(sprintf("%-55s median %6.3f s  (min %.3f / max %.3f)\n",
              label, median(timings), min(timings), max(timings)))
  invisible(median(timings))
}

cat("== Baseline performance TranscriptoShiny (", R.version.string, ") ==\n")

# Stub objet (5 genes x 160 cellules) — identite + collecte seulement.
stub <- matrix(0, 5, 160, dimnames = list(paste0("gene", 1:5),
                                          paste0("cell_", 1:160)))

shared_rv <- shiny::reactiveValues(
  markers_data = data.frame(gene = paste0("g", 1:200), cluster = rep(1:4, 50)),
  pathway_results = data.frame(pathway = paste0("PATH", 1:300), pval = runif(300))
)

.time("validate_velocity_matrices + finalize (fixture RDS)", {
  validated <- validate_velocity_matrices(
    spliced = Matrix::Matrix(seq_len(50), 10, 5, sparse = TRUE,
                             dimnames = list(paste0("gene", 1:10),
                                             paste0("cell_", 1:5))),
    unspliced = Matrix::Matrix(seq_len(50), 10, 5, sparse = TRUE,
                               dimnames = list(paste0("gene", 1:10),
                                               paste0("cell_", 1:5))) * 2L,
    seurat_cells = paste0("cell_", 1:5), seurat_genes = paste0("gene", 1:10),
    orientation = "auto_strict")
  finalize_velocity_result(validated = validated, input_mode = "rds",
                           seurat_obj = stub)
}, reps = 10L)

.time("validate_da_design + finalize (160 cellules, 4 echantillons)", {
  meta <- data.frame(
    sample_id = rep(c("s1", "s2", "s3", "s4"), each = 40),
    condition = rep(c("A", "A", "B", "B"), each = 40),
    replicate_id = rep(c("r1", "r2", "r3", "r4"), each = 40),
    cell_type = rep(c("CD4 T", "B", "NK"), length.out = 160),
    batch = rep(c("b1", "b2"), each = 80))
  validate_da_design(meta, "sample_id", "condition", "replicate_id",
                     "batch", "cell_type")
}, reps = 10L)

.time("collect_consolidated_report_input (etat simple)", {
  collect_consolidated_report_input(stub, shared_rv)
}, reps = 10L)

.time("collect + validate + write HTML (etat simple)", {
  ri <- collect_consolidated_report_input(stub, shared_rv)
  write_consolidated_report_html(ri, validate_consolidated_report_input(ri),
                                 tempfile(fileext = ".html"))
}, reps = 5L)

# Stress : 1200 entrees de provenance -> bundle complet + zip.
for (i in seq_len(1200L)) {
  provenance_append(shared_rv, new_provenance_entry(
    analysis_id = sprintf("sc-bench-%04d", i), method = "bench", dataset = stub))
}
.time("collect + validate + write HTML (1200 entrees provenance)", {
  ri2 <- collect_consolidated_report_input(stub, shared_rv)
  write_consolidated_report_html(ri2, validate_consolidated_report_input(ri2),
                                 tempfile(fileext = ".html"))
}, reps = 5L)

.time("build_report_bundle + zip (1200 entrees)", {
  ri3 <- collect_consolidated_report_input(stub, shared_rv)
  val3 <- validate_consolidated_report_input(ri3)
  bd <- tempfile("bench_bundle_")
  b <- build_report_bundle(bd, ri3, val3)
  zip::zip(tempfile(fileext = ".zip"),
           files = file.path(b$bundle_dir, b$files), mode = "cherry-pick")
  unlink(bd, recursive = TRUE)
}, reps = 3L)

cat("== fin ==\n")

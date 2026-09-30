# =============================================================================
# test-mod-import-sc.R — modules/import/mod_import_sc.R
# =============================================================================
# Troisième fichier de `modules/` converti au chantier de dette C10 (§2bs,
# §2bt) : **6** `stop()` non classés -> `stop(errorCondition(<msg>, class =
# "sc_import_error"))`.
#
# Classe : `sc_import_error`. L'import est qualifié par son **objet/source**,
# comme le dépôt le fait déjà : `rdata_import_error`,
# `communication_import_error`, `bulk_import_error` (§2bs), `geo_import_error`
# (§2bt).
#
# ⚠️ Le fichier porte **7** `stop()` pour **6** signalements C10 :
#   - **650** est `stop(paste0(...), call. = FALSE)` — forme « héritage » que
#     C10 **exempte** ⇒ **laissé en place** (comme les 4 de
#     `pathway_helpers.R`, §2bp : leur unification relève de la décision
#     `state`/`class`) ;
#   - **303** n'est qu'un **commentaire** mentionnant `stop()` — non signalé,
#     ce qui confirme que la garde ignore bien les commentaires.
#
# ⚠️ PREUVE D'EXÉCUTION : **1 site joignable sur 6**, MESURÉ.
#   - 489, 531 (`stop(integrity$msg)`) et 627 (dossier sans `matrix.mtx`)
#     vivent dans `mod_import_sc_server` et n'ont pas été atteints par les
#     déclencheurs essayés ;
#   - 690 exige `BPCells` + `zellkonverter` + `sceasy` **absents**, 695 exige
#     `loomR` **absent** — or **les quatre sont installés** (mesuré), donc ces
#     deux sites « paquet requis » ne se déclenchent jamais.
#   - **700** (« Format non supporté : <ext> ») EST joignable via
#     `btn_load_single` + un fichier dont l'extension est inconnue. C'est
#     aussi le seul site **multi-arguments** du lot ⇒ celui que `paste0()`
#     doit protéger : l'assertion porte sur la **valeur de l'extension**, qui
#     disparaîtrait si `errorCondition()` tronquait (§2bn/§2bo).
# D'où les deux niveaux : verrou SOURCE (couvre les 6) + témoin de MESSAGE
# sur le site 700.
# =============================================================================

source_project_file("R/core/io_helpers.R")       # %||%
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)       # layout_columns
  library(DT)
  library(bsicons)
  library(shinyFiles)  # getVolumes (mod_import_sc)
  library(Seurat)      # prepare_seurat_object
})
source_project_file("R/core/rdata_io.R")
source_project_file("modules/import/mod_rdata_picker.R")
source_project_file("modules/import/mod_import_sc.R")
source_project_file("tools/check_conventions.R")

assign(".tr", function(key) key, envir = globalenv())
if (!exists(".t_fmt", envir = globalenv(), inherits = FALSE)) {
  assign(".t_fmt", function(template, ...) {
    vals <- list(...)
    for (nm in names(vals)) {
      template <- gsub(paste0("{", nm, "}"), format(vals[[nm]]), template, fixed = TRUE)
    }
    template
  }, envir = globalenv())
}

.mis_tmp <- tempfile("mis_")
dir.create(.mis_tmp, showWarnings = FALSE, recursive = TRUE)

# Fichier a extension INCONNUE : declenche le site 700.
# ⚠️ `size` doit valoir la TAILLE REELLE, sinon `.verify_upload_integrity()`
# echoue avant et l'on journalise « Upload incomplet » a la place (mesure).
.mis_bad <- file.path(.mis_tmp, "counts.unsupportedfmt")
writeLines(c("col", "1"), .mis_bad)

# ── Niveau 1 : verrou source ─────────────────────────────────────────────────
test_that("mod_import_sc : 0 signalement C10 (verrou source, couvre les 6 sites)", {
  path <- file.path(ts_project_root(), "modules", "import", "mod_import_sc.R")
  .REPORT$warns <- list()
  check_c10_error_style(path)
  flagged <- Filter(function(w) identical(w$rule, "C10"), .REPORT$warns)
  expect_identical(
    length(flagged), 0L,
    info = paste(vapply(flagged,
                        function(w) sprintf("%s:%s", .rel(w$file), w$line),
                        character(1)), collapse = ", ")
  )
})

# ── Niveau 2 : invariant de message a l'execution (site 700) ─────────────────
.mis_last_logs <- NULL
.mis_collect <- function(datapath) {
  gd <- new.env(parent = emptyenv())
  gd$i18n <- NULL
  shiny::testServer(mod_import_sc_server, args = list(global_data = gd), expr = {
    session$setInputs(single_file_upload = list(
      name = basename(datapath),
      size = as.integer(file.info(datapath)$size),
      datapath = datapath
    ))
    session$flushReact()
    session$setInputs(btn_load_single = 1)   # sans ce clic, rien ne se charge
    session$flushReact()
    .mis_last_logs <<- paste(logs(), collapse = "\n")
  })
  .mis_last_logs
}

test_that("mod_import_sc : message du site 700 IDENTIQUE a l'execution", {
  lg <- .mis_collect(.mis_bad)
  expect_match(lg, "Format non support", fixed = TRUE)
  # Assertion ANTI-TRONCATURE : sans paste0(), errorCondition() reduirait le
  # message a "Format non supporté : " et l'extension disparaitrait.
  expect_match(lg, "unsupportedfmt", fixed = TRUE)
})

unlink(.mis_tmp, recursive = TRUE, force = TRUE)

# ── Roadmap SC FUNCTION_TEST M1.4 : surface des doublets (metadata.csv) ──────
# Un triplet 10X accompagné d'un metadata.csv portant `multiplets`
# (singlet/doublet/ambs — ex: GSE96583 by_sample/) doit EXPOSER les doublets
# (colonne méta.data + log), jamais les inclure ou les retirer silencieusement.
.m4_tmp <- tempfile("m4_"); dir.create(.m4_tmp, showWarnings = FALSE, recursive = TRUE)
.m4_obj <- function() {
  cells <- c("AAAC-1", "AAAD-1", "AAAE-1", "AAF-1")
  cnt <- Matrix::sparseMatrix(
    i = rep(1:2, 4L), j = rep(1:4, each = 2L), x = 1,
    dims = c(2L, 4L), dimnames = list(c("G1", "G2"), cells)
  )
  SeuratObject::CreateSeuratObject(counts = cnt)
}
.m4_md <- data.frame(
  barcode    = c("AAAC-1", "AAAD-1", "AAAE-1", "AAF-1"),
  donor      = 1015,
  condition  = "control",
  multiplets = c("singlet", "doublet", "ambs", "singlet"),
  stringsAsFactors = FALSE
)
write.csv(.m4_md, file.path(.m4_tmp, "metadata.csv"), row.names = FALSE)

test_that("M1.4 : les doublets documentés sont surfacés dans le méta.data + log", {
  logs <- character(0)
  out  <- .sc_surface_multiplets(.m4_obj(), .m4_tmp, log_fn = function(m) logs <<- c(logs, m))
  expect_true("multiplets" %in% colnames(out@meta.data))
  expect_identical(
    as.character(out$multiplets),
    c("singlet", "doublet", "ambs", "singlet")   # appariement PAR BARCODE
  )
  expect_match(paste(logs, collapse = "\n"), "2 cellules flaggées doublet/multiplets sur 4", fixed = TRUE)
})

test_that("M1.4 : sans metadata.csv (ou sans colonne multiplets) — objet inchangé", {
  obj <- .m4_obj()
  out1 <- .sc_surface_multiplets(obj, tempfile("m4_empty_"))
  expect_false("multiplets" %in% colnames(out1@meta.data))

  dir2 <- file.path(.m4_tmp, "nomults"); dir.create(dir2)
  write.csv(data.frame(barcode = .m4_md$barcode, cluster = 1:4),
            file.path(dir2, "metadata.csv"), row.names = FALSE)
  out2 <- .sc_surface_multiplets(obj, dir2)
  expect_false("multiplets" %in% colnames(out2@meta.data))
})

test_that("M1.4 : metadata.csv SANS les barcodes de l'objet — aucune colonne fabriquée", {
  dir3 <- file.path(.m4_tmp, "bcbien"); dir.create(dir3)
  write.csv(transform(.m4_md, barcode = c("XXX-1", "XXY-1", "XXZ-1", "XW-1")),
            file.path(dir3, "metadata.csv"), row.names = FALSE)
  out <- .sc_surface_multiplets(.m4_obj(), dir3)
  expect_false("multiplets" %in% colnames(out@meta.data))
})

unlink(.m4_tmp, recursive = TRUE, force = TRUE)

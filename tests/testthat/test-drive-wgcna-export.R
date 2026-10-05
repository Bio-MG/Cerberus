# =============================================================================
# test-drive-wgcna-export.R — Slice 4 : la route d'export drive de WGCNA
# (la table gène -> modules)
# =============================================================================
# Le jumeau WGCNA des suites d'export Slice 2.3. L'artefact est la table que
# le téléchargement humain `dl_wgcna_genes` écrit via `build_wgcna_export()` —
# le MÊME builder, jamais un second. Le contrat de colonnes déclaré
# (`TS_DRIVE_EXPORT_COLUMNS$bulk_wgcna`) est épinglé contre la SORTIE RÉELLE
# du builder : sans traits corrélés, `guaranteed` ; avec, `mode_traits`.
#
# Décision produit (2026-10-05) : TOM / RDS / dendrogramme ne sont PAS des
# routes — test 6 épingle l'absence de toute seconde clé pour le module.
# =============================================================================

source_project_file("config/defaults.R")
source_project_file("R/core/io_helpers.R")            # %||%
source_project_file("R/core/error_state.R")
source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")
source_project_file("R/bulk/bulk_wgcna.R")
source_project_file("modules/bulk/mod_bulk_wgcna_export.R")

if (!exists(".tr", envir = globalenv()))
  assign(".tr", function(key) key, envir = globalenv())

# --- Fixtures ----------------------------------------------------------------
.wgc_genes <- sprintf("G%04d", seq_len(8L))

.wgc_result <- function() {
  colors <- c(G1 = "blue", G2 = "blue", G3 = "blue", G4 = "brown",
              G5 = "brown", G6 = "grey", G7 = "grey", G8 = "brown")
  list(
    type         = "bulk_wgcna_modules",
    power        = 6,
    n_modules    = 3L,
    module_sizes = c(blue = 3L, brown = 3L, grey = 2L),
    colors       = colors,
    dendro       = NULL,
    dendro_colors = NULL
  )
}

.wgc_tc <- function() list(method = "bicor", cor = NULL, p = NULL)

.wgc_state <- function(with_result = TRUE) {
  gd <- shiny::reactiveValues()
  gd$language <- "fr"
  rv <- shiny::reactiveValues()
  if (with_result) rv$wgcna_modules <- .wgc_result()
  list(gd = gd, rv = rv)
}

.wgc_exporter   <- function() get0("bulk_wgcna_export_genes_csv", envir = globalenv())
.wgc_next_path  <- function() get0("bulk_wgcna_export_next_path", envir = globalenv())
.wgc_stem       <- function() get0("TS_DRIVE_EXPORT_STEM_BULK_WGCNA", envir = globalenv())

# =============================================================================
# 1. Les tables déclarées : UNE route de plus, le stem, le contrat
# =============================================================================
test_that("bulk_wgcna joins the frozen route table, the stems, and the contract", {
  routes <- get0("TS_DRIVE_EXPORT_ROUTES", envir = globalenv())
  expect_length(routes, 10L)
  expect_identical(routes$bulk_wgcna, "bulk_wgcna_genes_csv")

  stems <- get0("TS_DRIVE_EXPORT_STEMS", envir = globalenv())
  expect_length(stems, 10L)
  expect_identical(stems[["bulk_wgcna"]], .wgc_stem())
  expect_identical(.wgc_stem(), "bulk_wgcna_genes")

  ct <- get0("TS_DRIVE_EXPORT_COLUMNS", envir = globalenv())
  expect_setequal(names(ct), names(routes))
  expect_identical(ct$bulk_wgcna$guaranteed, c("gene", "module"))
  expect_identical(ct$bulk_wgcna$mode_traits, c("gene", "module", "trait_cor_method"))
  # La forme garde ses identités : le mode complet contient le garanti.
  expect_true(all(ct$bulk_wgcna$guaranteed %in% ct$bulk_wgcna$mode_traits))
})

# =============================================================================
# 2. L'exporteur refuse honnêtement quand il n'y a rien à exporter
# =============================================================================
test_that("the bulk_wgcna exporter returns an INVALID verdict, never a throw", {
  x <- .wgc_exporter()
  expect_true(is.function(x))

  st <- .wgc_state(with_result = FALSE)
  r <- x(st$rv, st$gd, tempfile("ts-wgc-"))
  expect_false(isTRUE(r$ok))
  expect_identical(r$status, "invalid")
  expect_null(r$descriptor)
  expect_match(paste(r$errors, collapse = " "), "bulk-wgcna-run_wgcna_modules",
               fixed = TRUE,
               info = "the refusal must name the action that produces the artefact")

  # Un objet stocké non canonique : le verdict du BUILDER, mappé en invalid.
  st2 <- .wgc_state()
  st2$rv$wgcna_modules <- list(n_modules = 3L)   # pas de `type` déclaré
  r2 <- x(st2$rv, st2$gd, tempfile("ts-wgc-"))
  expect_false(isTRUE(r2$ok))
  expect_identical(r2$status, "invalid")
  expect_match(paste(r2$errors, collapse = " "), "canonical", fixed = TRUE)
})

# =============================================================================
# 3. Le chemin heureux : la table écrite, le descripteur sain
# =============================================================================
test_that("the bulk_wgcna exporter writes the gene->module table with a sane descriptor", {
  x <- .wgc_exporter()
  d <- tempfile("ts-wgc-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)

  st <- .wgc_state()
  r <- x(st$rv, st$gd, d)
  expect_true(isTRUE(r$ok))
  expect_identical(r$status, "done")
  desc <- r$descriptor
  expect_identical(desc$format, "csv")
  expect_match(desc$file, "^bulk_wgcna_genes_[0-9]+\\.csv$")
  expect_true(file.exists(file.path(d, desc$file)))
  expect_identical(as.integer(desc$n_rows), 8L)
  expect_identical(as.integer(desc$n_cols), 2L)
  expect_identical(as.character(desc$columns), c("gene", "module"))

  # Le contenu du fichier EST la table du builder (ordre module puis gène).
  # read.csv renumérote les row.names (1:8) : le builder, lui, réordonne en
  # gardant les noms d'origine — on compare les VALEURS, pas les noms de lignes.
  df <- utils::read.csv(file.path(d, desc$file), stringsAsFactors = FALSE)
  ref <- build_wgcna_export(.wgc_result(), NULL)
  expect_identical(df, `rownames<-`(ref, NULL))
})

# =============================================================================
# 4. La colonne conditionnelle : le contrat épingle la SORTIE RÉELLE
# =============================================================================
test_that("the conditional trait_cor_method column matches the declared mode_traits", {
  x <- .wgc_exporter()
  d <- tempfile("ts-wgc-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)

  ct <- get0("TS_DRIVE_EXPORT_COLUMNS", envir = globalenv())
  st <- .wgc_state()

  # Sans traits corrélés : EXACTEMENT le contrat `guaranteed`.
  r0 <- x(st$rv, st$gd, d)
  expect_identical(as.character(r0$descriptor$columns), ct$bulk_wgcna$guaranteed)

  # Avec : EXACTEMENT le contrat `mode_traits`, et la valeur du method.
  r1 <- x(st$rv, st$gd, d, trait_cor = .wgc_tc())
  expect_identical(as.character(r1$descriptor$columns), ct$bulk_wgcna$mode_traits)
  df <- utils::read.csv(file.path(d, r1$descriptor$file), stringsAsFactors = FALSE)
  expect_true(all(df$trait_cor_method == "bicor"))
})

# =============================================================================
# 5. Le numérotage sans état : 1 puis 2 dans le même répertoire
# =============================================================================
test_that("bulk_wgcna_export_next_path numbers statelessly, one above the highest", {
  np <- .wgc_next_path()
  d <- tempfile("ts-wgc-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  expect_match(basename(np(d)), "^bulk_wgcna_genes_1\\.csv$")
  file.create(file.path(d, "bulk_wgcna_genes_7.csv"))
  expect_match(basename(np(d)), "^bulk_wgcna_genes_8\\.csv$")
})

# =============================================================================
# 6. La décision produit : PAS de seconde route WGCNA
# =============================================================================
test_that("the declared route is the ONLY WGCNA registry key shape", {
  routes <- get0("TS_DRIVE_EXPORT_ROUTES", envir = globalenv())
  wkeys <- grep("wgcna", names(routes), value = TRUE, ignore.case = TRUE)
  expect_identical(wkeys, "bulk_wgcna",
                   info = paste("the product decision names ONE artefact (the gene->module",
                                "table); a TOM/RDS/dendrogram route must not appear silently"))
})

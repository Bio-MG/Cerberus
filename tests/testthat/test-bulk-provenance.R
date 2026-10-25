# =============================================================================
# test-bulk-provenance.R — Manifeste de provenance Bulk (roadmap Bulk V2, M1)
# =============================================================================
source_project_file("tools/pkg_skeleton/R/bulk_provenance.R")

.meta_prov <- function(n = 12) {
  data.frame(row.names = paste0("s", seq_len(n)),
             batch = rep(c("b1", "b2"), length.out = n),
             condition = rep(c("A", "B"), each = n / 2))
}

test_that("surface publique figée", {
  expect_setequal(
    bulk_provenance_public_api(),
    c("bulk_provenance_public_api", "bulk_provenance_known_genomes",
      "bulk_provenance_known_normalizations", "bulk_build_provenance",
      "bulk_ensure_provenance", "bulk_update_provenance",
      "bulk_provenance_dataframe", "bulk_provenance_session_packages")
  )
  expect_true("GRCh38" %in% bulk_provenance_known_genomes())
  expect_true("VST (DESeq2)" %in% bulk_provenance_known_normalizations())
})

test_that("build : declarations vides -> NA, jamais de valeur fabriquée", {
  p <- bulk_build_provenance()
  expect_identical(p$type, "bulk_provenance")
  expect_true(is.na(p$reference_genome))
  expect_true(is.na(p$annotation_release))
  expect_true(is.na(p$normalization))
  expect_identical(p$n_genes, NA_integer_)
  expect_identical(p$n_samples, NA_integer_)
  expect_identical(p$metadata_columns, character(0))
  expect_identical(p$history, list())
  expect_match(p$r_version, "^4\\.")
  expect_true(nzchar(p$created_utc))
})

test_that("build : declarations fournies + dimensions enregistrées", {
  cnt <- matrix(1:24, 4, 6, dimnames = list(paste0("g", 1:4), paste0("s", 1:6)))
  p <- bulk_build_provenance(reference_genome = "GRCh38 ",
                             annotation_release = "Ensembl 110",
                             normalization = "Counts bruts",
                             project = "Projet X",
                             counts = cnt, metadata = .meta_prov(6))
  expect_identical(p$reference_genome, "GRCh38")  # trimws appliqué
  expect_identical(p$annotation_release, "Ensembl 110")
  expect_identical(p$n_genes, 4L)
  expect_identical(p$n_samples, 6L)
  expect_setequal(p$metadata_columns, c("batch", "condition"))
  pkgs <- bulk_provenance_session_packages()
  if (length(pkgs) > 0L) expect_false(is.null(names(pkgs)))  # versions nommées par package
})

test_that("session_packages : seuls les packages installés, jamais NA", {
  pkgs <- bulk_provenance_session_packages()
  expect_type(pkgs, "character")
  expect_true(all(nzchar(pkgs)))
  expect_true(all(grepl("^[0-9]+\\.[0-9]+", pkgs)))
})

test_that("session_packages ne laisse aucun état global (option Matrix)", {
  # Régression mesurée : GSVA::.onLoad() fait
  # options(Matrix.warnDeprecatedCoerce = 2) SANS le restaurer. Comme GSVA était
  # chargé par cette fonction, l'option fuyait sur tout le reste de la session et
  # escaladait toute dépréciation Matrix en erreur fatale -> Milo
  # (miloR::calcNhoodDistance -> as(<dgTMatrix>, "dgCMatrix")) échouait.
  before <- getOption("Matrix.warnDeprecatedCoerce")
  invisible(bulk_provenance_session_packages())
  expect_identical(getOption("Matrix.warnDeprecatedCoerce"), before)
  # L'option doit aussi être REMISE à sa valeur d'origine quand elle existait.
  old <- options(Matrix.warnDeprecatedCoerce = 1)
  on.exit(options(old), add = TRUE)
  invisible(bulk_provenance_session_packages())
  expect_identical(getOption("Matrix.warnDeprecatedCoerce"), 1)
})

test_that("ensure : création paresseuse + idempotence + erreurs classées", {
  bo <- list(counts = matrix(1:24, 4, 6), metadata = .meta_prov(6))
  out <- bulk_ensure_provenance(bo)
  expect_identical(out$provenance$type, "bulk_provenance")
  expect_identical(out$provenance$normalization, "Counts bruts")  # déclarée, pas devinée
  # idempotence : pas de réinitialisation silencieuse
  out2 <- bulk_ensure_provenance(out)
  expect_identical(out2$provenance$created_utc, out$provenance$created_utc)

  err <- tryCatch(bulk_ensure_provenance(NULL), error = function(e) e)
  expect_s3_class(err, "bulk_provenance_error")
  err2 <- tryCatch(bulk_ensure_provenance(42), error = function(e) e)
  expect_s3_class(err2, "bulk_provenance_error")
})

test_that("update : champ changé + historique préservé + NULL = inchangé", {
  bo <- list(counts = NULL, metadata = .meta_prov(6))
  bo <- bulk_ensure_provenance(bo)
  created <- bo$provenance$created_utc
  bo <- bulk_update_provenance(bo, reference_genome = "GRCm39")
  expect_identical(bo$provenance$reference_genome, "GRCm39")
  expect_identical(bo$provenance$created_utc, created)  # jamais antidaté
  expect_length(bo$provenance$history, 1L)
  expect_identical(bo$provenance$history[[1]]$field, "reference_genome")
  expect_true(is.na(bo$provenance$history[[1]]$previous))  # NA antérieur préservé
  expect_true(nzchar(bo$provenance$updated_utc))
  # NULL laisse tout inchangé (aucune entrée d'historique fantôme)
  hist_before <- length(bo$provenance$history)
  bo <- bulk_update_provenance(bo)
  expect_length(bo$provenance$history, hist_before)
  # même valeur re-déclarée : pas de doublon d'historique
  bo <- bulk_update_provenance(bo, reference_genome = "GRCm39")
  expect_length(bo$provenance$history, hist_before)
})

test_that("dataframe : consolidation pour affichage/export", {
  expect_identical(nrow(bulk_provenance_dataframe(NULL)), 0L)
  df <- bulk_provenance_dataframe(bulk_ensure_provenance(
    list(counts = matrix(1:12, 3, 4)))$provenance)
  expect_identical(colnames(df), c("Champ", "Valeur"))
  expect_identical(nrow(df), 10L)
  expect_true(all(c("Génome de référence", "Normalisation", "Packages clés") %in% df$Champ))
  # projet non déclaré -> cellule vide, jamais "NA" littéral affiché
  expect_identical(df$Valeur[df$Champ == "Projet"], "")
})

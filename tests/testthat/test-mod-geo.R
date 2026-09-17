# =============================================================================
# test-mod-geo.R — GEO import module (modules/import/mod_geo.R)
# =============================================================================
# House split:
#  - pure helpers (.load_counts_file / .sniff_delim_ext / .geo_fetch live-gated)
#  - testServer functional smoke of the offline flow (counts -> preview ->
#    confirm -> bulk_obj contract + bulk_datasets registration)
#  - server-side validation paths (accession validation, empty metadata)
# Live network tests are gated by TS_GEO_LIVE_SMOKE=1 so the standard suite
# stays offline/deterministic.
# =============================================================================

source_project_file("R/core/io_helpers.R")
source_project_file("R/bulk/bulk_multi.R")
source_project_file("modules/import/mod_geo.R")

# .t_fmt lives in global.R (not sourced here) — provide the same fallback the
# module would get in the app, only if the test env doesn't already have it.
if (!exists(".t_fmt", envir = globalenv(), inherits = FALSE)) {
  assign(".t_fmt", function(template, ...) {
    vals <- list(...)
    for (nm in names(vals)) {
      template <- gsub(paste0("{", nm, "}"), format(vals[[nm]]), template, fixed = TRUE)
    }
    template
  }, envir = globalenv())
}

suppressPackageStartupMessages(library(shiny))

.geo_tmp <- tempfile("geo_mod_test")
dir.create(.geo_tmp, showWarnings = FALSE, recursive = TRUE)

.geo_genes <- paste0("GENE", 1:5)
.geo_df <- data.frame(
  gene = .geo_genes,
  S1 = 1:5, S2 = 6:10, S3 = 11:15, S4 = 16:20, S5 = 21:25
)
.geo_write_tsv <- function(path, df) {
  write.table(df, path, sep = "\t", row.names = FALSE, quote = FALSE)
}

csv_path   <- file.path(.geo_tmp, "counts.csv")
tsv_path   <- file.path(.geo_tmp, "counts.tsv")
txt_path   <- file.path(.geo_tmp, "counts.txt")
tab_path   <- file.path(.geo_tmp, "counts.tab")
xlsx_path  <- file.path(.geo_tmp, "counts.xlsx")
gz_path    <- file.path(.geo_tmp, "counts.txt.gz")
gz_noinner <- file.path(.geo_tmp, "counts_txt.gz")
ws_path    <- file.path(.geo_tmp, "counts_ws.txt")
bad_path   <- file.path(.geo_tmp, "bad.csv")
empty_path <- file.path(.geo_tmp, "empty.tsv")
meta_path  <- file.path(.geo_tmp, "meta.tsv")

write.csv(.geo_df, csv_path, row.names = FALSE)
.geo_write_tsv(tsv_path, .geo_df)
.geo_write_tsv(txt_path, .geo_df)
.geo_write_tsv(tab_path, .geo_df)
openxlsx::write.xlsx(.geo_df, xlsx_path)
R.utils::gzip(tsv_path, destname = gz_path, overwrite = TRUE, remove = FALSE)
R.utils::gzip(txt_path, destname = gz_noinner, overwrite = TRUE, remove = FALSE, ext = "gz")
write.table(.geo_df, ws_path, sep = " ", row.names = FALSE, quote = FALSE)
write.csv(data.frame(gene = .geo_genes, S1 = c("a", "b", "c", "d", "e"), S2 = 6:10),
          bad_path, row.names = FALSE)
writeLines(character(0), empty_path)
.geo_write_tsv(meta_path, data.frame(sample_id = paste0("S", 1:5),
                                     group = c("A", "A", "B", "B", "B")))

# ── .load_counts_file ─────────────────────────────────────────────────────────

test_that(".load_counts_file parses csv/tsv/txt/tab/xlsx with first col as rownames", {
  for (p in c(csv_path, tsv_path, txt_path, tab_path, xlsx_path)) {
    res <- .load_counts_file(p)
    expect_true(res$ok, info = basename(p))
    expect_true(is.matrix(res$counts), info = basename(p))
    expect_identical(dim(res$counts), c(5L, 5L), info = basename(p))
    expect_identical(rownames(res$counts), .geo_genes, info = basename(p))
    expect_true(is.numeric(res$counts), info = basename(p))
  }
})

test_that(".load_counts_file handles .gz with an inner extension (R.utils path)", {
  res <- .load_counts_file(gz_path)
  expect_true(res$ok)
  expect_identical(dim(res$counts), c(5L, 5L))
  expect_identical(rownames(res$counts), .geo_genes)
})

test_that(".load_counts_file sniffs separator for .gz without inner extension", {
  # e.g. GSE123_counts_rpkm.gz — after decompression the name has no extension
  res <- .load_counts_file(gz_noinner)
  expect_true(res$ok)
  expect_identical(dim(res$counts), c(5L, 5L))
  expect_identical(rownames(res$counts), .geo_genes)
})

test_that(".load_counts_file retries whitespace-delimited tables read as 1 column", {
  res <- .load_counts_file(ws_path)
  expect_true(res$ok)
  expect_identical(dim(res$counts), c(5L, 5L))
  expect_identical(rownames(res$counts), .geo_genes)
})

test_that(".load_counts_file refuses non-numeric matrices with actionable French error", {
  res <- .load_counts_file(bad_path)
  expect_false(res$ok)
  expect_match(res$msg, "colonne\\(s\\) non numérique\\(s\\)", fixed = FALSE)
  expect_match(res$msg, "S1", fixed = TRUE)      # offending column is named
  expect_match(res$msg, "mode hors-ligne", fixed = TRUE)
})

test_that(".load_counts_file gives a French error on empty or missing files", {
  res <- .load_counts_file(empty_path)
  expect_false(res$ok)
  expect_match(res$msg, "Fichier vide ou introuvable", fixed = TRUE)
  expect_null(res$counts)

  res2 <- .load_counts_file(file.path(.geo_tmp, "does_not_exist.csv"))
  expect_false(res2$ok)
  expect_match(res2$msg, "Fichier vide ou introuvable", fixed = TRUE)
})

test_that(".load_counts_file de-duplicates duplicated colnames", {
  dup_path <- file.path(.geo_tmp, "dupcols.csv")
  write.csv(data.frame(gene = .geo_genes, S1 = 1:5, S1 = 6:10, check.names = FALSE),
            dup_path, row.names = FALSE)
  res <- .load_counts_file(dup_path)
  expect_true(res$ok)
  expect_false(anyDuplicated(colnames(res$counts)) > 0)
})

# ── .sniff_delim_ext ──────────────────────────────────────────────────────────

test_that(".sniff_delim_ext distinguishes csv from tsv headers", {
  expect_identical(.sniff_delim_ext(csv_path), "csv")
  expect_identical(.sniff_delim_ext(tsv_path), "tsv")
})

# ── .geo_fetch — live, gated ──────────────────────────────────────────────────

test_that(".geo_fetch live smoke on a small clean accession (TS_GEO_LIVE_SMOKE=1)", {
  skip_if_not(identical(Sys.getenv("TS_GEO_LIVE_SMOKE"), "1"),
              "Live GEO ping disabled (set TS_GEO_LIVE_SMOKE=1).")
  res <- .geo_fetch("GSE145919")
  expect_true(res$ok)
  expect_true(length(res$suppl_choices) >= 1)
  expect_false(is.null(res$metadata))
  loaded <- .load_counts_file(res$suppl_choices[[1]])
  expect_true(loaded$ok)
  expect_gte(ncol(loaded$counts), 1)
})

# ── testServer functional smoke — offline flow ───────────────────────────────

.new_geo_global_data <- function() {
  gd <- new.env(parent = emptyenv())
  gd$i18n <- NULL           # modules fall back to raw keys
  gd$session <- NULL
  gd$bulk_obj <- NULL
  gd$bulk_datasets <- list()
  gd
}

test_that("mod_geo_server offline flow: counts -> preview -> confirm writes bulk_obj", {
  gd <- .new_geo_global_data()

  testServer(mod_geo_server, args = list(global_data = gd), expr = {
    session$setInputs(mode = "offline")
    session$setInputs(file_counts = list(name = "counts.csv", size = 1000, datapath = csv_path))
    session$flushReact()

    # Preview renders with dimensions + no-metadata hint
    pv <- session$output$ui_preview
    expect_false(is.null(pv))

    session$setInputs(btn_confirm = 1)
    session$flushReact()

    obj <- gd$bulk_obj
    expect_false(is.null(obj))
    expect_identical(obj$type, "bulk")
    expect_identical(obj$import_mode, "geo")
    expect_identical(obj$project, "GEO_import")   # no accession in offline mode
    expect_identical(dim(obj$counts), c(5L, 5L))
    expect_identical(rownames(obj$counts), .geo_genes)
    expect_s3_class(obj$metadata, "data.frame")
    expect_identical(nrow(obj$metadata), 5L)
    expect_true("sample" %in% colnames(obj$metadata))
    expect_false(is.null(obj$timestamp))
    expect_true(is.character(obj$gene_id_type))
  })
})

test_that("mod_geo_server registers the confirmed dataset in bulk_datasets", {
  gd <- .new_geo_global_data()

  testServer(mod_geo_server, args = list(global_data = gd), expr = {
    session$setInputs(mode = "offline")
    session$setInputs(file_counts = list(name = "counts.csv", size = 1000, datapath = csv_path))
    session$flushReact()
    session$setInputs(btn_confirm = 1)
    session$flushReact()

    expect_true("GEO_import" %in% names(gd$bulk_datasets))
    entry <- gd$bulk_datasets$GEO_import
    expect_identical(entry$producer, "import")
    expect_identical(dim(entry$obj$counts), c(5L, 5L))

    # Duplicate label on a second confirm -> bulk_multi_error path = warning,
    # the import itself must NOT fail and the container must stay consistent.
    session$setInputs(btn_confirm = 2)
    session$flushReact()
    expect_false(is.null(gd$bulk_obj))
    expect_identical(length(gd$bulk_datasets), 1L)
  })
})

test_that("mod_geo_server keeps provided metadata when samples align", {
  gd <- .new_geo_global_data()

  testServer(mod_geo_server, args = list(global_data = gd), expr = {
    session$setInputs(mode = "offline")
    session$setInputs(file_counts = list(name = "counts.csv", size = 1000, datapath = csv_path))
    session$setInputs(file_meta = list(name = "meta.tsv", size = 200, datapath = meta_path))
    session$flushReact()
    session$setInputs(btn_confirm = 1)
    session$flushReact()

    meta <- gd$bulk_obj$metadata
    expect_identical(nrow(meta), 5L)
    expect_identical(meta$group, c("A", "A", "B", "B", "B"))
    expect_true("sample" %in% colnames(meta))
    expect_identical(rownames(meta), colnames(gd$bulk_obj$counts))
  })
})

test_that("mod_geo_server survives an empty metadata file (no crash, no metadata)", {
  gd <- .new_geo_global_data()

  testServer(mod_geo_server, args = list(global_data = gd), expr = {
    session$setInputs(mode = "offline")
    session$setInputs(file_counts = list(name = "counts.csv", size = 1000, datapath = csv_path))
    # Regression guard: used to throw `NA || …` on the empty-file sniff
    expect_error(session$setInputs(file_meta = list(name = "empty.tsv", size = 0, datapath = empty_path)),
                 regexp = NA)
    session$flushReact()
    session$setInputs(btn_confirm = 1)
    session$flushReact()

    expect_false(is.null(gd$bulk_obj))          # import still works
    expect_true("sample" %in% colnames(gd$bulk_obj$metadata))  # auto-generated
  })
})

test_that("mod_geo_server rejects malformed accessions without network access", {
  gd <- .new_geo_global_data()

  testServer(mod_geo_server, args = list(global_data = gd), expr = {
    session$setInputs(mode = "online")

    # Whitespace-only accession: visible error message, no fetch attempt
    expect_error(session$setInputs(accession = "   ", btn_fetch = 1), regexp = NA)
    session$flushReact()
    expect_null(gd$bulk_obj)

    # Wrong format: visible error message, no fetch attempt
    expect_error(session$setInputs(accession = "not-a-gse", btn_fetch = 2), regexp = NA)
    session$flushReact()
    expect_null(gd$bulk_obj)
  })
})

# ── Classification des erreurs (C10) — mod_geo.R ──────────────────────────────
# Inséré AVANT le cleanup : `.geo_tmp` doit encore exister.
#
# ⚠️ PREUVE D'EXÉCUTION PARTIELLE, MESURÉE (pas supposée) — deux raisons :
#   1. `.geo_fetch` (4 sites : 358, 366, 393, 412) est **live-gated** : GEOquery
#      EST installé (mesuré), donc le site 358 « paquet requis » n'est pas
#      déclenchable, et 366/393/412 exigent le réseau.
#   2. `.load_counts_file` **AVALE** l'erreur : `tryCatch(..., error =
#      function(e) list(ok = FALSE, counts = NULL, msg = conditionMessage(e)))`
#      (ligne 505) ⇒ la **classe** n'est PAS observable ; seul le **message**
#      l'est, dans `res$msg`. Le site 465 (readxl absent) n'est pas déclenchable
#      non plus : readxl EST installé (mesuré).
#
# Restent **3 sites joignables** : 468, 481, 498 — tous via `.load_counts_file`,
# en R pur, hors réseau. Le 498 est déjà couvert par le test ci-dessus
# (« refuses non-numeric matrices »), qui sert ici de **témoin**.
#
# Le site 468 est le plus important : c'est un `stop()` à **PLUSIEURS
# ARGUMENTS**. Sans `paste0()`, `errorCondition()` le **TRONQUE** à
# `"Extension non supportée : "` (§2bn/§2bo) — l'assertion sur la VALEUR de
# l'extension est donc le garde-fou décisif, et elle est en ASCII pur.
source_project_file("tools/check_conventions.R")

test_that("mod_geo : 0 signalement C10 (verrou source, couvre les 8 sites)", {
  path <- file.path(ts_project_root(), "modules", "import", "mod_geo.R")
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

test_that("mod_geo : messages d'erreur IDENTIQUES a l'execution (sites 468, 481)", {
  # Site 468 — extension non supportée. Message MULTI-ARGUMENTS :
  # l'extension doit FIGURER dans le message rendu (sinon = troncature).
  p_ext <- file.path(.geo_tmp, "counts.unsupportedext")
  writeLines(c("gene\tS1", "G1\t1"), p_ext)
  res <- .load_counts_file(p_ext)
  expect_false(res$ok)
  expect_match(res$msg, "Extension non support", fixed = TRUE)
  expect_match(res$msg, "unsupportedext", fixed = TRUE)   # ASCII : anti-troncature

  # Site 481 — fichier lisible mais 0 ligne (en-tête seul).
  p_empty <- file.path(.geo_tmp, "header_only.csv")
  writeLines("gene,S1", p_empty)
  res2 <- .load_counts_file(p_empty)
  expect_false(res2$ok)
  expect_match(res2$msg, "0 ligne ou 0 colonne", fixed = TRUE)
})

# ── cleanup ───────────────────────────────────────────────────────────────────
unlink(.geo_tmp, recursive = TRUE, force = TRUE)

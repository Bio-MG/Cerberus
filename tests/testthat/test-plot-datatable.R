# =============================================================================
# test-plot-datatable.R — PLOT-S3 : wrapper DT harmonise ts_datatable()
# =============================================================================
# Objectif central (MISE A JOUR jalon DT-EXPORT 2026-09-14, contrat §6
# option B) :
#   - buttons : defaut = TS_DT_BUTTONS_DEFAULT (TRUE) — les tables de
#     resultats sont boutonnees ; les tables d'APERCU passent
#     buttons = FALSE explicitement (ni extensions ni dom alors poses)
#   - page_length est TOUJOURS OBLIGATOIRE dans la signature : la
#     normalisation a 15 se fait site par site, jamais par un defaut cache
#   - plus AUCUN appel direct a DT::datatable() dans modules/ et R/
#     (hors R/plotting/datatable.R lui-meme)
# =============================================================================
suppressWarnings(suppressPackageStartupMessages(library(DT)))

# D2 M4 (2026-10-25) : R/plotting vit desormais dans l'arbre du paquet
# (tools/pkg_skeleton/R/) — charge via load_project_pkg() (cf. helper-source.R).
load_project_pkg()

.tsd_df <- function() {
  data.frame(gene = c("A", "B", "C"), padj = c(0.0001, 0.02, 0.4),
             stringsAsFactors = FALSE)
}

# --- 1. Contrat du resolveur -------------------------------------------------

test_that("surface publique figee", {
  expect_setequal(
    ts_datatable_public_api(),
    c("ts_datatable", "ts_datatable_buttons",
      "ts_datatable_page_lengths", "ts_datatable_public_api")
  )
  expect_false(anyDuplicated(ts_datatable_public_api()) > 0)
})

test_that("page_length est OBLIGATOIRE — aucune valeur par defaut n'est neutre", {
  df <- .tsd_df()
  e <- tryCatch(ts_datatable(df), error = function(e) e)
  expect_s3_class(e, "plot_datatable_error")
  expect_identical(e$state, "invalid_page_length")
  expect_match(conditionMessage(e), "obligatoire")
  expect_match(conditionMessage(e), "10, 15, 8, 20 et 6")
  # il ne doit PAS y avoir de valeur par defaut dans la signature
  expect_true(is.name(formals(ts_datatable)$page_length) ||
              is.symbol(formals(ts_datatable)$page_length))
})

test_that("page_length invalide => invalid_page_length", {
  df <- .tsd_df()
  for (bad in list(NULL, NA, 0, -5, "15", c(10, 15))) {
    e <- tryCatch(ts_datatable(df, page_length = bad), error = function(e) e)
    expect_s3_class(e, "plot_datatable_error")
    expect_identical(e$state, "invalid_page_length")
  }
})

test_that("les tailles de page reelles de l'app sont toutes connues", {
  expect_setequal(ts_datatable_page_lengths(), c(6L, 8L, 10L, 15L, 20L))
})

test_that("donnees invalides => invalid_data ; matrix/liste sont coercés", {
  e <- tryCatch(ts_datatable("pas une table", page_length = 10),
                error = function(e) e)
  expect_s3_class(e, "plot_datatable_error")
  expect_identical(e$state, "invalid_data")

  m <- matrix(1:6, nrow = 2, dimnames = list(NULL, c("a", "b", "c")))
  expect_s3_class(ts_datatable(m, page_length = 10), "datatables")
  expect_s3_class(ts_datatable(list(a = 1:3), page_length = 10), "datatables")
})

test_that("buttons non logique => invalid_buttons", {
  df <- .tsd_df()
  for (bad in list("oui", NA, c(TRUE, FALSE))) {
    e <- tryCatch(ts_datatable(df, page_length = 10, buttons = bad),
                  error = function(e) e)
    expect_s3_class(e, "plot_datatable_error")
    expect_identical(e$state, "invalid_buttons")
  }
})

# --- 2. GARANTIE du defaut DT-EXPORT (contrat §6, option B) -------------------

test_that("appel nu = table de resultats boutonnee (defaut TS_DT_BUTTONS_DEFAULT)", {
  df <- .tsd_df()
  got <- ts_datatable(df, page_length = 15)
  expect_identical(got$x$options$dom, "Bfrtip")
  expect_identical(got$x$options$pageLength, 15)
  expect_identical(got$x$options$scrollX, TRUE)
  expect_true("Buttons" %in% unlist(got$x$extensions))
  # aucun jeu de boutons nomme : DT applique ses boutons par defaut
  expect_null(got$x$options$buttons)
  expect_identical(got$x$filter, "top")
  expect_false(isTRUE(got$x$rownames))
})

test_that("le defaut buttons suit la constante TS_DT_BUTTONS_DEFAULT", {
  df <- .tsd_df()
  old <- if (exists("TS_DT_BUTTONS_DEFAULT", envir = globalenv())) {
    get("TS_DT_BUTTONS_DEFAULT", envir = globalenv())
  } else {
    NULL
  }
  assign("TS_DT_BUTTONS_DEFAULT", FALSE, envir = globalenv())
  on.exit({
    if (is.null(old)) {
      if (exists("TS_DT_BUTTONS_DEFAULT", envir = globalenv()))
        rm("TS_DT_BUTTONS_DEFAULT", envir = globalenv())
    } else {
      assign("TS_DT_BUTTONS_DEFAULT", old, envir = globalenv())
    }
  }, add = TRUE)
  got <- ts_datatable(df, page_length = 15)
  expect_null(got$x$options$dom)
  expect_true(is.null(got$x$extensions) || length(got$x$extensions) == 0L)
})

test_that("buttons = FALSE ne pose NI extensions NI dom (tables d'apercu)", {
  df <- .tsd_df()
  got <- ts_datatable(df, page_length = 8, buttons = FALSE)
  expect_null(got$x$options$dom)
  expect_true(is.null(got$x$extensions) || length(got$x$extensions) == 0L)
})

test_that("buttons = TRUE reproduit exactement le site pathways (dom + Buttons)", {
  df <- .tsd_df()
  ref <- DT::datatable(df, filter = "top", rownames = FALSE,
                       options = list(pageLength = 10, scrollX = TRUE,
                                      dom = "Bfrtip"),
                       extensions = "Buttons")
  got <- ts_datatable(df, page_length = 10, buttons = TRUE)
  expect_identical(got$x$options$dom, "Bfrtip")
  expect_identical(got$x$options$pageLength, 10)
  expect_identical(got$x$options$scrollX, TRUE)
  expect_true("Buttons" %in% unlist(got$x$extensions))
  # aucun jeu de boutons nomme : DT applique ses boutons par defaut
  expect_null(got$x$options$buttons)
  expect_identical(got$x$options$dom, ref$x$options$dom)
})

test_that("filename_base ajoute les 4 boutons nommes (forme recommandee)", {
  df <- .tsd_df()
  got <- ts_datatable(df, page_length = 10, buttons = TRUE,
                      filename_base = "pathways")
  expect_identical(got$x$options$dom, "Bfrtip")
  btns <- got$x$options$buttons
  expect_type(btns, "list")
  flat <- unlist(btns)
  expect_true("copy"  %in% flat)
  expect_true("print" %in% flat)
  expect_true("csv"   %in% flat)
  expect_true("excel" %in% flat)
  expect_true("pathways" %in% flat)
})

test_that("dom / extensions explicites ont la priorite sur buttons", {
  df <- .tsd_df()
  got <- ts_datatable(df, page_length = 10, buttons = TRUE, dom = "tip")
  expect_identical(got$x$options$dom, "tip")
})

test_that("extra_options : fusionnee avec priorite maximale, sinon invalid_extra_options", {
  df <- .tsd_df()
  got <- ts_datatable(df, page_length = 15, filename_base = "x",
                      extra_options = list(
                        language = list(search = "Filtrer :"),
                        lengthMenu = c(10, 15, 25)))
  expect_identical(got$x$options$language$search, "Filtrer :")
  expect_identical(got$x$options$lengthMenu, c(10, 15, 25))
  # les options posees par le wrapper restent presentes
  expect_identical(got$x$options$dom, "Bfrtip")
  # extra_options a la priorite sur le wrapper (ex. surcharger dom)
  got2 <- ts_datatable(df, page_length = 15, extra_options = list(dom = "t"))
  expect_identical(got2$x$options$dom, "t")
  # erreur classee si liste non nommee
  e <- tryCatch(ts_datatable(df, page_length = 15, extra_options = list(1, 2)),
                error = function(e) e)
  expect_s3_class(e, "plot_datatable_error")
  expect_identical(e$state, "invalid_extra_options")
})

test_that("les parametres de forme sont respectes et les ... transmis", {
  df <- .tsd_df()
  got <- ts_datatable(df, page_length = 20, filter = "none",
                      rownames = TRUE, scroll_x = FALSE,
                      selection = list(mode = "none"))
  expect_identical(got$x$options$pageLength, 20)
  expect_identical(got$x$options$scrollX, FALSE)
  expect_identical(got$x$filter, "none")
  # pas d'erreur => les `...` (selection, etc.) ont bien ete acceptes
  expect_s3_class(got, "datatables")
})

test_that("ts_datatable_buttons() est deterministe", {
  b <- ts_datatable_buttons("x")
  expect_identical(b, ts_datatable_buttons("x"))
  expect_identical(b[[1]], "copy")
  expect_identical(b[[3]]$filename, "x")
  expect_identical(b[[4]]$extend, "excel")
})

# --- 3. Anti-regression -------------------------------------------------------

.tsd_migrated <- c(
  "tools/pkg_skeleton/R/bulk_helpers.R",
  "R/core/pathway_helpers.R",
  "R/sc/sc_helpers.R",
  "modules/bulk_de/mod_bulk_de_multimethod.R",
  "modules/bulk_de/mod_bulk_de_venn.R",
  "modules/spatial/mod_spatial_cluster.R"
)

test_that("PLOT-S3 : les 6 sites canoniques passent par ts_datatable()", {
  for (rel in .tsd_migrated) {
    path <- file.path(ts_project_root(), rel)
    code <- readLines(path, warn = FALSE, encoding = "UTF-8")
    code <- grep("^\\s*#", code, value = TRUE, invert = TRUE)
    src <- paste(code, collapse = "\n")
    # On cible precisement la forme canonique migree, pas n'importe quelle
    # table du fichier (certains fichiers contiennent d'autres tables, de forme
    # differente, volontairement laissees hors du perimetre PLOT-S3).
    pat <- paste0(
      "(?:DT::)?datatable\\s*\\(\\s*[A-Za-z0-9_.$]+\\s*,\\s*",
      "filter\\s*=\\s*.top.\\s*,\\s*rownames\\s*=\\s*FALSE\\s*,\\s*",
      "options\\s*=\\s*list\\(\\s*pageLength\\s*=\\s*[0-9]+\\s*,\\s*",
      "scrollX\\s*=\\s*TRUE")
    hits <- grepl(pat, src, perl = TRUE)
    expect_false(hits,
      paste0(rel, " : une table de forme canonique appelle encore datatable() ",
             "directement au lieu de ts_datatable()"))
  }
})

test_that("DT-EXPORT : les sites canoniques sont normalises (page 15 + boutons)", {
  # page_length = 15L + filename_base sur les tables de resultats
  expect_match(paste(readLines(file.path(ts_project_root(),
                                         "tools/pkg_skeleton/R/bulk_helpers.R")), collapse = "\n"),
               "ts_datatable\\(df_display, page_length = 15L, filename_base = \"bulk_degenes\"\\)")
  expect_match(paste(readLines(file.path(ts_project_root(),
                                         "R/core/pathway_helpers.R")), collapse = "\n"),
               "page_length = 15L")
  expect_match(paste(readLines(file.path(ts_project_root(),
                                         "modules/spatial/mod_spatial_cluster.R")), collapse = "\n"),
               "page_length = 15L")
})

test_that("DT-EXPORT : plus AUCUN appel datatable() direct hors du wrapper", {
  roots <- c(file.path(ts_project_root(), "modules"),
             file.path(ts_project_root(), "R"))
  offenders <- character(0)
  for (root in roots) {
    files <- list.files(root, pattern = "\\.R$", recursive = TRUE,
                        full.names = TRUE, ignore.case = TRUE)
    for (f in files) {
      if (normalizePath(f, winslash = "/") ==
          normalizePath(file.path(ts_project_root(), "R/plotting/datatable.R"),
                        winslash = "/")) {
        next
      }
      code <- readLines(f, warn = FALSE, encoding = "UTF-8")
      code <- grep("^\\s*#", code, value = TRUE, invert = TRUE)
      src <- paste(code, collapse = "\n")
      # (?<![A-Za-z0-9_]) : exclut ts_datatable( ; casse lowercase = appels reels
      if (grepl("(?<![A-Za-z0-9_])(?:DT::)?datatable\\s*\\(", src, perl = TRUE)) {
        offenders <- c(offenders, f)
      }
    }
  }
  expect_length(offenders, 0L)
  info <- if (length(offenders)) paste(offenders, collapse = ", ") else ""
  expect_identical(info, "")
})

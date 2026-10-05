# =============================================================================
# tests/testthat/test-mod-bulk-de-drive-vocab.R
#
# Slice 3 — LE PIN DE PUBLICATION : la sonde de vocabulaire du module bulk_de
# est épinglée contre les choix RÉELS de la session, comme les contrats de
# colonnes sont épinglés contre la sortie réelle des exporteurs.
#
# Le serveur RÉEL du module boote sous testServer (seuls les fichiers du
# module sont sourcés — aucun moteur DE n'est chargé : la sonde ne lit que
# global_data$bulk_obj$metadata et le miroir `shared_rv$active_condition_col`
# qu'un observe() EXISTANT du moteur alimente). Les formules attendues sont
# réécrites INDÉPENDAMMENT dans le test — si le module et le test divergent,
# c'est une dérive détectée, pas une tautologie.
#
# La publication est la MÊME closure pour les deux canaux : `state` (le wire,
# via ts_drive_module_states -> projection) et `vocab` (la résolution d'index
# de l'applicateur). C'est épinglé ici : le rev de l'état publié est celui que
# la résolution compare.
# =============================================================================

source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")
source_project_file("R/core/io_helpers.R")
source_project_file("modules/bulk_de/mod_bulk_de_engine.R")
source_project_file("modules/bulk_de/mod_bulk_de_run.R")
suppressPackageStartupMessages(library(shiny))

.VW_server <- function(id, global_data, shared_rv, helpers) {
  moduleServer(id, function(input, output, session) {
    .de_run_server(input, output, session, session$ns,
                   global_data, shared_rv, helpers)
  })
}

# La formule attendue, RÉÉCRITE indépendamment (mod_bulk_de_engine.R) :
# colonnes character/factor, toutes les colonnes en repli ; niveaux = unique
# non-NA de la colonne de condition active, dans l'ordre d'apparition.
.VW_expected <- function(meta, col) {
  cat_cols <- names(meta)[vapply(meta, function(x) is.character(x) || is.factor(x), logical(1))]
  cat_cols <- if (!length(cat_cols)) names(meta) else cat_cols
  lvls <- if (!is.null(col) && col %in% names(meta)) {
    unique(na.omit(as.character(meta[[col]])))
  } else character(0)
  list(condition_col = cat_cols, group_levels = lvls)
}

.VW_fixture <- function() {
  data.frame(
    condition = c("mock", "CoV2", "CoV2", "mock", "mock"),
    tissue    = factor(c("cornea", "limbus", "cornea", "sclera", "limbus")),
    score     = c(1.2, 3.4, 5.6, 7.8, 9.0),   # numérique : JAMAIS un choix
    stringsAsFactors = FALSE)
}

test_that("the module's vocabulary probe publishes the widget's real domain, and the rev is monotone", {
  gv <- shiny::reactiveValues()
  sv <- shiny::reactiveValues()
  # Le registre est PAR SESSION et créé par app.R ; sans lui,
  # ts_drive_publish_token ne fait RIEN (silencieusement, par contrat). Le
  # test crée le sien — le même objet, la même clé.
  shiny::isolate(gv$drive_registry <- new.env(parent = emptyenv()))
  h  <- .de_make_helpers(NULL, sv)
  testServer(.VW_server, args = list(global_data = gv, shared_rv = sv, helpers = h), {
    # Avant toute donnée : listes VIDES (INPUT_NOT_READY est l'état honnête),
    # et le rev avance déjà (la publication existe, le domaine non).
    session$flushReact()
    entry0 <- ts_drive_registry(gv)[["bulk-de-run_de"]]
    expect_false(is.null(entry0))
    v0 <- entry0$vocab()
    expect_length(v0$condition_col, 0L)
    expect_length(v0$group_levels, 0L)
    expect_true(is.integer(v0$vocab_rev) && v0$vocab_rev >= 1L)

    # Des métadonnées arrivent : le domaine publié suit la formule réécrite.
    gv$bulk_obj <- list(metadata = .VW_fixture())
    sv$active_condition_col <- "condition"
    session$flushReact()
    voc <- ts_drive_registry(gv)[["bulk-de-run_de"]]$vocab()
    exp <- .VW_expected(.VW_fixture(), "condition")
    expect_identical(as.character(voc$condition_col), exp$condition_col)
    expect_identical(as.character(voc$covariates), exp$condition_col)
    expect_identical(as.character(voc$group_levels), exp$group_levels)
    rev1 <- voc$vocab_rev
    expect_true(is.integer(rev1) && rev1 >= 1L)

    # MÊME closure pour le canal wire : l'état publié porte le même bloc.
    st <- ts_drive_registry(gv)[["bulk-de-run_de"]]$state()
    expect_identical(st$vocabulary$condition_col, voc$condition_col)
    expect_identical(as.integer(st$vocabulary$vocab_rev), as.integer(rev1))
    # ... et le COLLECTEUR du poller le projette sans le déformer.
    ms <- ts_drive_module_states(gv)[["bulk_de"]]
    expect_false(is.null(ms$vocabulary))
    expect_identical(as.character(ms$vocabulary$condition_col), exp$condition_col)
    expect_true(all(names(ms$vocabulary) %in%
                      c(TS_DRIVE_VOCABULARY_KEYS$bulk_de, "vocab_rev")))

    # Monotone : un état IDENTIQUE ne bouge pas le rev ; un CHANGEMENT (le
    # miroir de condition_col change => les niveaux aussi) l'incrémente.
    rev_same <- ts_drive_registry(gv)[["bulk-de-run_de"]]$vocab()$vocab_rev
    expect_identical(as.integer(rev_same), as.integer(rev1))
    sv$active_condition_col <- "tissue"
    session$flushReact()
    voc2 <- ts_drive_registry(gv)[["bulk-de-run_de"]]$vocab()
    expect_identical(as.integer(voc2$vocab_rev), as.integer(rev1) + 1L)
    expect_identical(as.character(voc2$group_levels),
                     as.character(.VW_expected(.VW_fixture(), "tissue")$group_levels))

    # Et la résolution d'index fonctionne contre la sonde du module VRAI :
    # index 2 => le second choix réel.
    r <- ts_drive_resolve_session_inputs(
      list(`bulk-de-condition_col` = list(index = 2L, vocab_rev = voc2$vocab_rev)),
      "bulk_de", ts_drive_registry(gv)[["bulk-de-run_de"]]$vocab)
    expect_true(r$ok)
    expect_identical(as.character(r$values$`bulk-de-condition_col`),
                     as.character(voc2$condition_col[[2]]))
  })
})

test_that("the bulk_pathways publication is wired (text locks), and its catalogue is the scores_source domain", {
  # Le serveur pathways traîne des dépendances de rendu ; ce qui doit tenir en
  # ligne est verrouillé par le TEXTE du module (pattern
  # test-mod-import-sc-drive.R), et le domaine est épinglé contre la fonction
  # réelle dont le select est construit.
  src <- paste(readLines(file.path(ts_project_root(), "modules", "bulk",
                                   "mod_bulk_pathways.R"), warn = FALSE),
               collapse = "\n")
  expect_true(grepl("drive_vocabulary <- function", src, fixed = TRUE))
  expect_true(grepl("s$vocabulary <- drive_vocabulary()", src, fixed = TRUE))
  expect_true(grepl("vocab = drive_vocabulary", src, fixed = TRUE))
  # Le domaine = LES VALEURS du select réel (source, pas libellé i18n) —
  # 11 sources déclarées, courtes, toutes sous la garde verbatim.
  source_project_file("R/bulk/bulk_gene_sets.R")
  srcs <- as.character(unname(bulk_gene_set_choices()))
  expect_length(srcs, 11L)
  expect_true("msigdb_hallmark" %in% srcs)
  guarded <- ts_drive_verbatim_guard(srcs)
  expect_false(any(is.na(guarded)))
  # La projection d'un tel bloc est fidèle et le keep-set refermé.
  proj <- ts_drive_project_vocabulary(
    list(scores_source = srcs, vocab_rev = 4L), "bulk_pathways")
  expect_identical(as.character(proj$scores_source), srcs)
  expect_identical(as.integer(proj$vocab_rev), 4L)
  expect_true(all(names(proj) %in% c(TS_DRIVE_VOCABULARY_KEYS$bulk_pathways,
                                     "vocab_rev")))
})

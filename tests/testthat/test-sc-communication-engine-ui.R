# =============================================================================
# test-sc-communication-engine-ui.R — CC-5 : UI « calculer dans l'application »
# =============================================================================
# Gel STATIQUE du cablage du moteur CellChat natif (Path B) dans
# modules/sc/mod_sc_communication.R.
#
# Ce fichier ne lance pas l'application (les tests Shiny sont couverts par les
# shinytest2) : il verifie les invariants qui casseraient SILENCIEUSEMENT si le
# cablage derivait :
#   - « cellchat_engine » est une source declaree et n'est PAS importable ;
#   - l'espece est un choix DECLARE (jamais un defaut implicite) ;
#   - les DEUX voies deposent leur resultat AU MEME ENDROIT (.store_result) :
#     c'est la regle des deux voies — une divergence de forme est un BUG ;
#   - l'absence de CellChat est un etat PREVU, annonce AVEC son remede ;
#   - une erreur du moteur est affichee AVEC son etat (6 etats geles).
#
# Contrat : docs/contracts/CELLCHAT_ENGINE_CONTRACT.md
# =============================================================================

.ts_read <- function(relpath) {
  if (grepl("^docs/contracts/", relpath) &&
      !file.exists(file.path(ts_project_root(), relpath))) {
    skip(.ts_contract_skip_msg(relpath))
  }
  paste(readLines(file.path(ts_project_root(), relpath), warn = FALSE),
        collapse = "\n")
}

.cc_ui_src  <- .ts_read("modules/sc/mod_sc_communication.R")
.cc_app_src <- .ts_read("app.R")

.count <- function(src, needle) {
  m <- gregexpr(needle, src, fixed = TRUE)[[1L]]
  if (length(m) == 1L && m == -1L) 0L else length(m)
}

test_that("the module still parses after the CC-5 wiring", {
  expect_silent(invisible(parse(text = .cc_ui_src)))
})

test_that("both native engines are distinct and only four import routes remain", {
  expect_true(grepl('"cellchat_engine"', .cc_ui_src, fixed = TRUE))
  expect_true(grepl('"liana_engine"', .cc_ui_src, fixed = TRUE))
  for (s in c('"cellchat"', '"cellchat_object"', '"cellphonedb"', '"liana"')) {
    expect_true(grepl(s, .cc_ui_src, fixed = TRUE), label = paste("source", s))
  }
  expect_true(grepl('actionButton(ns("comm_compute")', .cc_ui_src, fixed = TRUE))
  expect_true(grepl('actionButton(ns("comm_compute_liana")', .cc_ui_src, fixed = TRUE))
  expect_true(grepl("input.comm_source == 'cellchat' || input.comm_source == 'cellchat_object' || input.comm_source == 'cellphonedb' || input.comm_source == 'liana'",
                    .cc_ui_src, fixed = TRUE))
  expect_true(grepl('identical(src, "cellchat_engine") || identical(src, "liana_engine")',
                    .cc_ui_src, fixed = TRUE))
  expect_false(grepl("input.comm_source != 'cellchat_engine'", .cc_ui_src, fixed = TRUE))
})

test_that("the native engine is the DEFAULT source and comes FIRST in the list", {
  # Decision utilisateur du 2026-09-16 (STATUS.md §2bd.5 #2) : le moteur natif
  # n'est pas un repli mais la voie NORMALE de la communication ⇒ propose EN
  # TETE et SELECTIONNE par defaut. Ce gel protege trois proprietes distinctes
  # qu'une edition distraite casserait en silence.
  i <- regexpr('radioButtons(ns("comm_source")', .cc_ui_src, fixed = TRUE)
  expect_true(i > 0L)
  # Le bloc des choix s'arrete au premier `selected =` : au-dela ce sont
  # d'autres widgets (espece, mode LIANA), dont les choix ne doivent pas etre
  # lus ici — sans cette borne, le test pourrait passer par accident.
  block <- substr(.cc_ui_src, i, i + 1200L)
  j <- regexpr("selected =", block, fixed = TRUE)
  expect_true(j > 0L)
  block <- substr(block, 1L, j + 40L)

  # 1. le defaut DECLARE est le moteur natif
  expect_true(grepl('selected = "cellchat_engine"', block, fixed = TRUE))

  # 2. il est en TETE de liste. NB : les guillemets ferment la comparaison,
  #    donc `"cellchat"` ne matche PAS l'interieur de `"cellchat_engine"` —
  #    le piege de sous-chaine est evite par construction.
  p_engine <- regexpr('"cellchat_engine"', block, fixed = TRUE)
  p_table  <- regexpr('"cellchat"', block, fixed = TRUE)
  expect_true(p_engine > 0L && p_table > 0L)
  expect_lt(p_engine, p_table)

  # 3. le reordonnancement n'a pas DESYNCHRONISE valeurs et libelles : les deux
  #    vecteurs de setNames() doivent rester dans le meme ordre, sinon le
  #    premier libelle annoncerait une source qui n'est pas la premiere valeur.
  p_lab_engine <- regexpr("Calculer dans l'application (CellChat)",
                          block, fixed = TRUE)
  p_lab_table  <- regexpr("CellChat (table exportee)", block, fixed = TRUE)
  p_lab_liana_import <- regexpr("LIANA (rangs agreges)", block, fixed = TRUE)
  p_lab_liana_engine <- regexpr("Calculer dans l'application (LIANA)", block, fixed = TRUE)
  expect_true(p_lab_engine > 0L && p_lab_table > 0L &&
              p_lab_liana_import > 0L && p_lab_liana_engine > 0L)
  expect_lt(p_lab_engine, p_lab_table)
  expect_lt(p_lab_liana_import, p_lab_liana_engine)

  p_liana_import <- regexpr('"liana"', block, fixed = TRUE)
  p_liana_engine <- regexpr('"liana_engine"', block, fixed = TRUE)
  expect_true(p_liana_import > 0L && p_liana_engine > 0L)
  expect_lt(p_liana_import, p_liana_engine)

  # 4. le repli SERVEUR doit valoir le defaut DECLARE : s'ils divergent, un
  #    input absent retombe silencieusement sur la branche import (req() muet)
  #    au lieu d'aiguiller vers le bouton de calcul.
  expect_true(grepl('input$comm_source %||% "cellchat_engine"',
                    .cc_ui_src, fixed = TRUE))
})

test_that("species is a DECLARED choice, never an implicit default", {
  expect_true(grepl('selectInput(ns("comm_engine_species")',
                    .cc_ui_src, fixed = TRUE))
  # pas de valeur par defaut : selected = character(0), comme pour LIANA
  i <- regexpr('comm_engine_species', .cc_ui_src, fixed = TRUE)
  bloc <- substr(.cc_ui_src, i, i + 400L)
  expect_true(grepl("selected = character(0)", bloc, fixed = TRUE))
  # les deux bases sont proposees (dans les libelles des choix d'espece)
  expect_true(grepl("CellChatDB.human", .cc_ui_src, fixed = TRUE))
  expect_true(grepl("CellChatDB.mouse", .cc_ui_src, fixed = TRUE))
})

test_that("seed and nboot are exposed and come from the declared config", {
  expect_true(grepl('numericInput(ns("comm_engine_seed")', .cc_ui_src, fixed = TRUE))
  expect_true(grepl('numericInput(ns("comm_engine_nboot")', .cc_ui_src, fixed = TRUE))
  expect_true(grepl("TS_CELLCHAT_SEED_DEFAULT", .cc_ui_src, fixed = TRUE))
  expect_true(grepl("TS_CELLCHAT_NBOOT_DEFAULT", .cc_ui_src, fixed = TRUE))
})

test_that("the native LIANA panel exposes explicit design and configured parameters", {
  expect_true(grepl("input.comm_source == 'liana_engine'", .cc_ui_src, fixed = TRUE))
  expect_true(grepl('selectInput(ns("comm_liana_sample_col")', .cc_ui_src, fixed = TRUE))
  expect_true(grepl('selectInput(ns("comm_liana_condition_col")', .cc_ui_src, fixed = TRUE))
  expect_true(grepl('selectInput(ns("comm_identity_column")', .cc_ui_src, fixed = TRUE))
  expect_true(grepl('selectInput(ns("comm_liana_method")', .cc_ui_src, fixed = TRUE))
  expect_true(grepl('selectInput(ns("comm_liana_resource")', .cc_ui_src, fixed = TRUE))
  expect_true(grepl('numericInput(ns("comm_liana_seed")', .cc_ui_src, fixed = TRUE))
  expect_true(grepl('numericInput(ns("comm_liana_min_cells")', .cc_ui_src, fixed = TRUE))
  expect_true(grepl("TS_LIANA_SEED_DEFAULT", .cc_ui_src, fixed = TRUE))
  expect_true(grepl("TS_LIANA_MIN_CELLS_DEFAULT", .cc_ui_src, fixed = TRUE))
  expect_true(grepl('method <- input$comm_liana_method %||% TS_LIANA_METHOD_DEFAULT',
                    .cc_ui_src, fixed = TRUE))
  expect_true(grepl('resource <- input$comm_liana_resource %||% TS_LIANA_RESOURCE_DEFAULT',
                    .cc_ui_src, fixed = TRUE))
  expect_true(grepl('actionButton(ns("comm_compute_liana")', .cc_ui_src, fixed = TRUE))
  expect_true(grepl('run_liana_by_sample(', .cc_ui_src, fixed = TRUE))
  expect_true(grepl('liana_engine_available()', .cc_ui_src, fixed = TRUE))
  expect_true(grepl('liana_engine_error_state(e)', .cc_ui_src, fixed = TRUE))
  expect_false(grepl("liana::", .cc_ui_src, fixed = TRUE))
})

test_that("the rank filter, collection selector, provenance and exports are wired", {
  expect_true(grepl('numericInput(ns("comm_f_rank_max")', .cc_ui_src, fixed = TRUE))
  expect_true(grepl('comm_f_rank_max")', .cc_ui_src, fixed = TRUE))
  expect_match(
    .cc_ui_src,
    'numericInput\\(ns\\("comm_f_rank_max"\\)(?s:.*?)min = 0',
    perl = TRUE
  )
  expect_true(grepl("rank_max     = .num_filter(input$comm_f_rank_max)", .cc_ui_src, fixed = TRUE))
  expect_true(grepl("fs$dropped_rank", .cc_ui_src, fixed = TRUE))
  expect_true(grepl('updateNumericInput(session, "comm_f_rank_max", value = NA)', .cc_ui_src, fixed = TRUE))
  expect_true(grepl('selectInput(ns("comm_active_sample")', .cc_ui_src, fixed = TRUE))
  expect_true(grepl("liana_collection_sample_manifest(collection)", .cc_ui_src, fixed = TRUE))
  expect_true(grepl("liana_collection_active(collection, key)", .cc_ui_src, fixed = TRUE))
  expect_true(grepl("provenance_append(shared_rv, collection$provenance)", .cc_ui_src, fixed = TRUE))
  expect_true(grepl('shared_rv$communication_collection <- collection', .cc_ui_src, fixed = TRUE))
  expect_true(grepl('shared_rv$active_communication_sample <- active_sample', .cc_ui_src, fixed = TRUE))
  expect_true(grepl("append_provenance = FALSE", .cc_ui_src, fixed = TRUE))
  expect_true(grepl("dl_comm_liana_manifest", .cc_ui_src, fixed = TRUE))
  expect_true(grepl("dl_comm_liana_condition_summary", .cc_ui_src, fixed = TRUE))
})

test_that("LIANA context cells are derived from the active manifest key and reset", {
  expect_identical(.count(.cc_ui_src, "context_cells = NULL"), 1L)
  expect_identical(.count(.cc_ui_src, "comm_state$context_cells <- NULL"), 1L)
  expect_true(grepl("if (!is.null(collection)", .cc_ui_src, fixed = TRUE))
  expect_true(grepl("manifest$sample_key", .cc_ui_src, fixed = TRUE))
  expect_true(grepl("sample_id <- as.character(manifest$sample_id[[sample_index]])",
                    .cc_ui_src, fixed = TRUE))
  expect_true(grepl("context_cells <- rownames(meta)[", .cc_ui_src, fixed = TRUE))
})

test_that("the compute path orchestrates without ever computing here", {
  # entree construite par le contrat 4D-3 (jamais reconstruite a la main)
  expect_true(grepl("build_cellchat_input(obj, group_by = identity_col",
                    .cc_ui_src, fixed = TRUE))
  # le calcul est delegue au moteur, avec la graine et le nombre de permutations
  expect_true(grepl("run_cellchat(", .cc_ui_src, fixed = TRUE))
  expect_true(grepl("seed  = as.integer(input$comm_engine_seed)",
                    .cc_ui_src, fixed = TRUE))
  expect_true(grepl("nboot = as.integer(input$comm_engine_nboot)",
                    .cc_ui_src, fixed = TRUE))
  # dependance paresseuse : l'absence est un etat annonce avec son remede
  expect_true(grepl("cellchat_engine_available()", .cc_ui_src, fixed = TRUE))
  expect_true(grepl("install_github", .cc_ui_src, fixed = TRUE))
  # retour utilisateur pendant un calcul long
  expect_true(grepl("withProgress(", .cc_ui_src, fixed = TRUE))
})

test_that("both paths store their result in the SAME place (règle des deux voies)",
          {
  # .store_result() est appele par l'import ET par le calcul : c'est la
  # garantie que les vues et les exports ne divergent pas.
  expect_identical(.count(.cc_ui_src, ".store_result("), 4L)
  expect_true(grepl(".store_result(canonical, obj)", .cc_ui_src, fixed = TRUE))
  expect_true(grepl(".store_result(result, obj)", .cc_ui_src, fixed = TRUE))
  expect_true(grepl(".store_result(first_result, obj", .cc_ui_src, fixed = TRUE))
  expect_true(grepl("append_provenance = FALSE", .cc_ui_src, fixed = TRUE))
  expect_identical(.count(.cc_ui_src, "comm_state$result <- canonical"), 1L)
  expect_identical(.count(.cc_ui_src, ".clear_result_state()"), 4L)
  expect_identical(.count(.cc_ui_src, "shared_rv$communication_result <-"), 2L)
  expect_identical(.count(.cc_ui_src, "provenance_append(shared_rv"), 3L)
})

test_that("engine errors are shown with their structured state", {
  expect_true(grepl(".liana_ui_stop <- function", .cc_ui_src, fixed = TRUE))
  expect_true(grepl(".import_ui_stop <- function", .cc_ui_src, fixed = TRUE))
  expect_true(grepl("communication_error_state(e)", .cc_ui_src, fixed = TRUE))
  expect_true(grepl("cellchat_engine_error_state(e)", .cc_ui_src, fixed = TRUE))
  expect_true(grepl("Erreur calcul communication :", .cc_ui_src, fixed = TRUE))
})

test_that("app.R sources the engine", {
  expect_true(grepl('source("R/sc/sc_communication_engine.R")',
                    .cc_app_src, fixed = TRUE))
})

test_that("the new UI strings are translated", {
  tr <- jsonlite::fromJSON(
    file.path(ts_project_root(), "i18n", "translation.json"),
    simplifyVector = FALSE
  )
  fr <- vapply(tr$translation, function(x) x$fr %||% "", character(1))
  en <- vapply(tr$translation, function(x) x$en %||% "", character(1))
  expect_identical(length(unique(fr)), length(fr))  # pas de doublon
  for (k in c("Calculer dans l'application (CellChat)",
              "Lancer le calcul CellChat",
              "Espece (base ligand-recepteur)",
              "Humain (CellChatDB.human)",
              "Souris (CellChatDB.mouse)",
              "Graine",
              "Nombre de permutations",
              "Erreur calcul communication :",
              "Communication cellule-cellule — import de resultats externes (CellChat / CellPhoneDB / LIANA) ou calcul CellChat / LIANA dans l'application. ",
              "Calculer dans l'application (LIANA)",
              "LIANA natif : les résultats sont calculés par échantillon biologique.",
              "Colonne d'echantillon (LIANA)",
              "Colonne de condition (LIANA)",
              "La colonne d'identites utilise le selecteur commun.",
              "Methode LIANA",
              "Ressource LIANA",
              "Graine LIANA",
              "Cellules minimum par echantillon",
              "La graine et le minimum de cellules sont des paramètres du calcul et sont conservés dans la provenance.",
              "Lancer le calcul LIANA",
              "Echantillon actif (LIANA)",
              "Exporter manifeste des echantillons (CSV)",
              "Exporter resume des conditions (CSV)",
              "Rang maximum (importé)",
              "%d interaction(s) affichée(s) sur %d — retirées : score %d, rang %d, p-value %d, pathway %d, sender %d, receiver %d, auto %d",
              "Aucune collection LIANA.",
              "Collection LIANA : statut {status} — {n} echantillon(s), {c} condition(s).",
              "Calcul LIANA par echantillon en cours...",
              "Choisissez d'abord la colonne de metadonnees Seurat decrivant les identites des populations.",
              "Choisissez la colonne d'echantillon LIANA.",
              "Choisissez la colonne de condition LIANA.",
              "Le moteur LIANA natif n'est pas disponible : installez les paquets requis.",
              "La collection LIANA ne contient aucun echantillon.",
              "Calcul LIANA termine.",
              "Echantillon actif absent de la collection LIANA.",
              "Echantillon actif : {name}",
              "Erreur calcul LIANA :",
              "Source d'import inconnue : %s.",
              "Les sources natives ne sont pas importables : utilisez « Lancer le calcul CellChat » ou « Lancer le calcul LIANA ».")) {
    expect_true(k %in% fr, label = paste("cle i18n", k))
    expect_true(nzchar(en[match(k, fr)]), label = paste("traduction en de", k))
  }
})

test_that("the contract documents the UI exposure", {
  doc <- file.path(ts_project_root(), "docs", "contracts",
                   "CELLCHAT_ENGINE_CONTRACT.md")
  skip_if_not(file.exists(doc), .ts_contract_skip_msg(doc))
  txt <- .ts_read("docs/contracts/CELLCHAT_ENGINE_CONTRACT.md")
  for (needle in c("CC-5", "comm_compute", "cellchat_engine",
                   "cellchat_engine_available")) {
    expect_true(grepl(needle, txt, fixed = TRUE),
                label = paste("le contrat doit mentionner", needle))
  }
})

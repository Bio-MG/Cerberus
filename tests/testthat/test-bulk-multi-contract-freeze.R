# =============================================================================
# test-bulk-multi-contract-freeze.R — MD-1 : gel du contrat
# docs/contracts/BULK_MULTI_CONTRACT.md (conteneur bulk_datasets & jeux nommés)
# =============================================================================
# Verrouille : surface publique, signatures, états d'erreur, champs de pipeline
# capturés, PURETÉ Shiny de la logique, seuil config + repli, ancres app.R
# (init / snapshot / restore / reset), câblage module, ZÉRO référence à
# bulk_datasets dans le pipeline existant (garde §2.3), sync code <-> contrat,
# clés i18n.
# =============================================================================
source_project_file("R/bulk/bulk_multi.R")
.local_source_if_exists("config/thresholds.R")

.ts_top_level_names <- function(relpath) {
  exprs <- parse(file.path(ts_project_root(), relpath))
  nms <- character(0)
  for (e in exprs) {
    if (is.call(e) && identical(e[[1L]], as.name("<-")) && is.name(e[[2L]])) {
      nms <- c(nms, as.character(e[[2L]]))
    }
  }
  unique(nms)
}

.ts_read <- function(relpath) {
  if (grepl("^docs/contracts/", relpath) &&
      !file.exists(file.path(ts_project_root(), relpath))) {
    skip(.ts_contract_skip_msg(relpath))
  }
  paste(readLines(file.path(ts_project_root(), relpath), warn = FALSE),
        collapse = "\n")
}

# ── Surface publique gelée ─────────────────────────────────────────────────
test_that("public API surface is frozen (names + signatures)", {
  expect_setequal(
    bulk_multi_public_api(),
    c("bulk_multi_public_api", "bulk_multi_error_states",
      "bulk_multi_pipeline_fields", "bulk_multi_check_label",
      "bulk_multi_check_obj", "bulk_multi_capture_pipeline",
      "bulk_multi_register", "bulk_multi_remove", "bulk_multi_get",
      "bulk_multi_summary")
  )
  expect_setequal(
    bulk_multi_public_api(),
    intersect(bulk_multi_public_api(), .ts_top_level_names("R/bulk/bulk_multi.R"))
  )
  expected_args <- list(
    bulk_multi_public_api         = list(),
    bulk_multi_error_states       = list(),
    bulk_multi_pipeline_fields    = list(),
    bulk_multi_check_label        = list(label = NULL),
    bulk_multi_check_obj          = list(obj = NULL),
    bulk_multi_capture_pipeline   = list(state = NULL),
    bulk_multi_register           = list(datasets = NULL, label = NULL, obj = NULL,
                                         pipeline_state = NULL,
                                         producer = "pipeline_save",
                                         overwrite = FALSE, max_datasets = NULL),
    bulk_multi_remove             = list(datasets = NULL, label = NULL),
    bulk_multi_get                = list(datasets = NULL, label = NULL),
    bulk_multi_summary            = list(datasets = NULL)
  )
  for (fn_name in names(expected_args)) {
    expect_true(exists(fn_name, where = globalenv(), inherits = FALSE),
                info = paste("fonction publique manquante :", fn_name))
    fn <- get(fn_name, envir = globalenv())
    expect_true(is.function(fn), info = fn_name)
    expect_identical(names(formals(fn)), names(expected_args[[fn_name]]),
                     info = fn_name)
  }
})

# ── États d'erreur figés (contrat §5) ──────────────────────────────────────
test_that("error states are frozen in code AND contract (no silent inflation)", {
  states_code <- bulk_multi_error_states()
  expect_setequal(states_code, c("invalid_input", "invalid_label",
                                 "invalid_obj", "invalid_pipeline",
                                 "duplicate_label", "unknown_label",
                                 "capacity_exceeded",
                                 "insufficient_datasets",
                                 "no_common_contrast",
                                 "no_significant_genes"))
  # Chaque état émis par le source appartient à l'ensemble gelé (le fichier
  # MD-1 n'émet que les 7 états de stockage ; les états MD-2 vivent dans
  # bulk_multi_compare.R, gelés par son propre test).
  src <- .ts_read("R/bulk/bulk_multi.R")
  emitted <- unique(regmatches(src,
                               gregexpr('(?<=state = ")[^"]+', src,
                                        perl = TRUE))[[1L]])
  expect_true(all(emitted %in% states_code),
              info = paste("états hors ensemble gelé :",
                           paste(setdiff(emitted, states_code), collapse = ", ")))
  expect_setequal(emitted, c("invalid_input", "invalid_label", "invalid_obj",
                             "invalid_pipeline", "duplicate_label",
                             "unknown_label", "capacity_exceeded"))
  # Le contrat cite les 10 états.
  doc <- .ts_read("docs/contracts/BULK_MULTI_CONTRACT.md")
  for (st in states_code) {
    expect_match(doc, paste0("`", st, "`"), fixed = TRUE,
                 info = paste("état absent du contrat :", st))
  }
})

# ── Champs de pipeline capturés figés (contrat §4.3) ───────────────────────
test_that("captured pipeline fields are frozen (12 fields, doc-synced)", {
  fields <- bulk_multi_pipeline_fields()
  expect_length(fields, 12L)
  expect_setequal(
    fields,
    c("mapping_applied", "mapping_summary", "filtered_counts", "vst_mat",
      "contrasts", "active_contrast", "multimethod_de",
      "lfc_thresh", "padj_thresh", "pathway_results", "pathway_db",
      "pathway_mode")
  )
  # Les objets volumineux non comparables ne sont JAMAIS capturés (garde §2.6).
  expect_false(any(c("dds_blind", "dds_full", "counts_original",
                     "counts_mapped") %in% fields))
  doc <- .ts_read("docs/contracts/BULK_MULTI_CONTRACT.md")
  for (f in fields) {
    expect_match(doc, f, fixed = TRUE,
                 info = paste("champ absent du contrat :", f))
  }
})

# ── PURETÉ Shiny (contrat §2.7) ────────────────────────────────────────────
test_that("R/bulk/bulk_multi.R contains no Shiny symbols (pure logic)", {
  src <- .ts_read("R/bulk/bulk_multi.R")
  # Regexes (perl) : « reactiveVal » ne doit pas matcher « reactiveValues »
  # cité dans un message FR ; on vise les CONSTRUCTS Shiny dans le code.
  forbidden <- c(
    "reactiveVal(?!ues)", "reactive\\(", "observeEvent", "observe\\(",
    "output\\$", "input\\$", "moduleServer", "showNotification",
    "session\\$", "\\bNS\\(", "renderDT"
  )
  for (sym in forbidden) {
    expect_false(grepl(sym, src, perl = TRUE),
                 info = paste("symbole Shiny interdit trouvé :", sym))
  }
})

# ── Producteurs étiquetés (contrat §2.8) ───────────────────────────────────
test_that("producer tags are frozen (import | pipeline_save | pseudobulk)", {
  src <- .ts_read("R/bulk/bulk_multi.R")
  for (p in c('"import"', '"pipeline_save"', '"pseudobulk"')) {
    expect_match(src, p, fixed = TRUE, info = p)
  }
  doc <- .ts_read("docs/contracts/BULK_MULTI_CONTRACT.md")
  expect_match(doc, "`pseudobulk` = pont depuis", fixed = TRUE)
  expect_match(doc, "modules/sc/mod_sc_pseudobulk.R", fixed = TRUE)
})

# ── MD-3 : le pont pseudobulk est un consommateur mince de l'API pure ──────
test_that("mod_sc_pseudobulk.R is a thin consumer of the pure API (MD-3)", {
  msp <- .ts_read("modules/sc/mod_sc_pseudobulk.R")
  for (call in c("bulk_multi_check_label(", "bulk_multi_capture_pipeline(",
                 "bulk_multi_register(")) {
    expect_match(msp, call, fixed = TRUE, info = call)
  }
  expect_match(msp, 'producer = "pseudobulk"', fixed = TRUE)
  expect_match(msp, "overwrite = TRUE", fixed = TRUE)
  # Le pont ne fait que LIRE son état pb$ — jamais d'écriture sur le jeu
  # bulk actif ni sur shared_rv$contrasts (garde contrat §2.1 / §6 prod. 3).
  expect_false(grepl("global_data\\$bulk_obj\\s*(<-|\\[\\[\\]\\s*<-)", msp,
                     perl = TRUE),
               info = "écriture interdite sur global_data$bulk_obj")
  expect_false(grepl("shared_rv\\$contrasts\\s*<-", msp, perl = TRUE),
               info = "écriture interdite sur shared_rv$contrasts")
})

# ── Seuil config + repli (contrat §9) ──────────────────────────────────────
test_that("capacity threshold is declared in config and consumed with fallback", {
  expect_true(exists("TS_BULK_MULTI_MAX_DATASETS", inherits = TRUE))
  expect_identical(TS_BULK_MULTI_MAX_DATASETS, 20L)
  cfg <- .ts_read("config/thresholds.R")
  expect_match(cfg, "TS_BULK_MULTI_MAX_DATASETS <- 20L", fixed = TRUE)
  src <- .ts_read("R/bulk/bulk_multi.R")
  expect_match(src, 'exists("TS_BULK_MULTI_MAX_DATASETS", inherits = TRUE)',
               fixed = TRUE)
  # Le contrat cite le seuil et la valeur.
  doc <- .ts_read("docs/contracts/BULK_MULTI_CONTRACT.md")
  expect_match(doc, "TS_BULK_MULTI_MAX_DATASETS", fixed = TRUE)
  expect_match(doc, "(20)", fixed = TRUE)
})

# ── Ancres app.R (contrat §6/§7) ───────────────────────────────────────────
test_that("app.R anchors: init, session snapshot, restore fallback, reset", {
  app <- .ts_read("app.R")
  expect_match(app, "bulk_datasets = list(),", fixed = TRUE)
  expect_match(app, "bulk_datasets = global_data$bulk_datasets,", fixed = TRUE)
  expect_match(app, "global_data$bulk_datasets <- snapshot$bulk_datasets %||% list()",
               fixed = TRUE)
  expect_match(app, "global_data$bulk_datasets <- list()", fixed = TRUE)
  expect_match(app, 'source("R/bulk/bulk_multi.R")', fixed = TRUE)
  expect_match(app, 'source("modules/bulk/mod_bulk_datasets.R")', fixed = TRUE)
})

# ── Câblage module Bulk (contrat §6) ───────────────────────────────────────
test_that("mod_bulk.R wires the datasets module (UI panel + server call)", {
  mb <- .ts_read("modules/bulk/mod_bulk.R")
  expect_match(mb, "mod_bulk_datasets_ui(ns(\"datasets\"))", fixed = TRUE)
  expect_match(mb, 'mod_bulk_datasets_server("datasets", global_data, shared_rv)',
               fixed = TRUE)
})

# ── GARDE §2.3 — ZÉRO référence à bulk_datasets dans le pipeline existant ──
test_that("existing bulk pipeline never references bulk_datasets (additive strict)", {
  untouched <- c(
    "modules/bulk/mod_bulk_filter.R",
    "modules/bulk/mod_bulk_mapping.R",
    "modules/bulk_de/mod_bulk_de.R",
    "modules/bulk_de/mod_bulk_de_engine.R",
    "modules/bulk_de/mod_bulk_de_run.R",
    "modules/bulk_de/mod_bulk_de_viz.R",
    "modules/bulk_de/mod_bulk_de_ui.R",
    "modules/bulk_de/mod_bulk_de_venn.R",
    "modules/bulk_de/mod_bulk_de_multimethod.R",
    "modules/bulk_de/mod_bulk_de_summary.R",
    "modules/bulk_de/mod_bulk_de_pairwise.R",
    "modules/bulk/mod_bulk_pathways.R",
    "R/bulk/bulk_gsva.R",
    "R/bulk/bulk_signatures.R",
    "R/bulk/bulk_wgcna.R",
    "R/bulk/bulk_survival.R"
  )
  for (f in untouched) {
    expect_false(grepl("bulk_datasets", .ts_read(f), fixed = TRUE),
                 info = paste("bulk_datasets ne doit PAS apparaître dans :", f))
  }
})

# ── Producteur "import" — câblage optionnel, jamais bloquant (contrat §6) ──
test_that("import module registers optionally via the pure API, import never blocked", {
  imp <- .ts_read("modules/import/mod_import_bulk.R")
  expect_match(imp, "multi_label", fixed = TRUE)
  expect_match(imp, ".register_multi_dataset", fixed = TRUE)
  # Exactement 3 points d'appel — et ce chiffre est un CONTRAT, pas une
  # photographie d'humeur : matrice fusionnée (widget), one file per sample
  # (widget), et import PILOTÉ PAR L'AGENT (`import_file`, G3 — commit 358a2be).
  #
  # Le troisième a été RE-FIGÉ délibérément le 2026-09-23. Il n'est pas
  # cosmétique : sans lui, un jeu chargé par l'agent n'entrerait pas dans le
  # conteneur multi-jeux et n'aurait donc pas accès à la comparaison MD-2, alors
  # que le MÊME import déclenché par le widget l'a. Cette divergence silencieuse
  # entre deux chemins d'entrée est exactement ce que l'extraction de
  # `assemble_bulk_obj()` existe pour empêcher (358a2be).
  #
  # Le contrat avait donc DÉRIVÉ le 2026-09-22 sans que personne puisse le lire :
  # depuis 448209b la suite complète s'arrêtait au fichier 56/137, si bien que
  # ce fichier-ci n'a jamais atteint un BILAN. Corrigé par b455da0 ; première
  # mesure complète : 137/137, ce test vert.
  calls <- length(gregexpr(".register_multi_dataset(global_data$bulk_obj)",
                           imp, fixed = TRUE)[[1L]])
  expect_identical(calls, 3L)
  expect_match(imp, 'producer = "import"', fixed = TRUE)
  # L'échec d'enregistrement est une ALERTE, pas une interruption : le corps
  # du helper ne contient aucun stop() (l'import ne doit jamais être bloqué).
  lines <- readLines(file.path(ts_project_root(),
                               "modules/import/mod_import_bulk.R"),
                     warn = FALSE)
  start <- grep(".register_multi_dataset <- function(obj) {", lines,
                fixed = TRUE)
  expect_length(start, 1L)
  closes <- which(lines[(start + 1L):length(lines)] == "    }")
  expect_true(length(closes) > 0L,
              info = "fin du helper .register_multi_dataset introuvable")
  body_txt <- paste(lines[start:(start + closes[1L])], collapse = "\n")
  expect_false(grepl("stop(", body_txt, fixed = TRUE),
               info = "l'enregistrement import ne doit jamais stopper l'import")
  expect_match(body_txt, "bulk_multi_register(", fixed = TRUE)
  expect_match(body_txt, 'inherits(res, "bulk_multi_error")', fixed = TRUE)
})

# ── Le module de gestion consomme l'API pure (pas de ré-implémentation) ────
test_that("mod_bulk_datasets.R is a thin consumer of the pure API", {
  mbd <- .ts_read("modules/bulk/mod_bulk_datasets.R")
  for (call in c("bulk_multi_check_label(", "bulk_multi_capture_pipeline(",
                 "bulk_multi_register(", "bulk_multi_remove(",
                 "bulk_multi_summary(")) {
    expect_match(mbd, call, fixed = TRUE, info = call)
  }
  expect_match(mbd, 'producer = "pipeline_save"', fixed = TRUE)
  expect_match(mbd, "overwrite = TRUE", fixed = TRUE)
  # Parité SC_MULTI roadmap 4.1 (2026-09-27) : l'ACTIVATION d'un dataset
  # enregistré écrit global_data$bulk_obj via bulk_multi_get — SEULE écriture
  # autorisée (contrat BULK_MULTI_CONTRACT.md §4, amended). Toute autre
  # écriture reste interdite. Le bump de bulk_obj_epoch qui suit purge les
  # résultats partagés (audit 2026-09-27 §1.5).
  mbd_no_activation <- gsub("global_data\\$bulk_obj <- entry\\$obj", "", mbd,
                            perl = TRUE)
  expect_false(grepl("global_data\\$bulk_obj\\s*(<-|\\[\\[\\]\\s*<-)",
                     mbd_no_activation, perl = TRUE),
               info = "écriture interdite sur global_data$bulk_obj hors activation")
  expect_match(mbd, "bulk_multi_get(", fixed = TRUE)
  expect_match(mbd, "bulk_obj_epoch", fixed = TRUE)
})

# ── Sync code <-> contrat ──────────────────────────────────────────────────
test_that("contract doc cites the public API, structure and key guards", {
  doc <- .ts_read("docs/contracts/BULK_MULTI_CONTRACT.md")
  for (fn in bulk_multi_public_api()) {
    expect_match(doc, fn, fixed = TRUE,
                 info = paste("fonction absente du contrat :", fn))
  }
  for (anchor in c("global_data$bulk_datasets", "overwrite = TRUE",
                   "registered_at", "updated_at",
                   "ts_datatable", "page_length = 6", "MD-2", "MD-3",
                   "MD-4", "Zéro mutation", "Isolation par copie")) {
    expect_match(doc, anchor, fixed = TRUE, info = anchor)
  }
  src <- .ts_read("R/bulk/bulk_multi.R")
  expect_match(src, "docs/contracts/BULK_MULTI_CONTRACT.md", fixed = TRUE)
})

# ── Clés i18n présentes (contrat §9) ───────────────────────────────────────
test_that("MD-1 i18n keys exist in translation.json (no FR leak in EN UI)", {
  json <- jsonlite::fromJSON(
    file.path(ts_project_root(), "i18n", "translation.json"),
    simplifyVector = FALSE)
  fr_keys <- vapply(json$translation, function(x) x$fr %||% "", character(1))
  keys <- c(
    "Label multi-datasets (optionnel)",
    "ex : GSE123_T2",
    "Si renseigné, le jeu importé est aussi enregistré sous ce nom pour la comparaison multi-jeux (Bulk > Multi-jeux).",
    "⚠️ Dataset non enregistré (multi-jeux) : %s",
    "📦 Import enregistré pour la comparaison multi-jeux : « %s ».",
    "Multi-jeux — Datasets enregistrés",
    "Enregistrez l'état courant (import + filtrage + DE + voies) sous un label, pour le comparer plus tard à d'autres jeux. Le jeu actif n'est jamais modifié.",
    "Label du dataset",
    "Enregistrer l'état courant",
    "Datasets enregistrés",
    "Aucun jeu Bulk actif à enregistrer.",
    "✓ État enregistré sous « %s ».",
    "✓ État enregistré (mis à jour) sous « %s ».",
    "Aucun dataset enregistré pour l'instant.",
    "Dataset à supprimer",
    "Supprimer le dataset",
    "✓ Dataset « %s » supprimé.",
    "Dataset à activer",
    "✅ Activer ce dataset",
    "Aucun dataset à activer.",
    "✓ Jeu « %s » activé (%s) — objet actif remplacé, résultats purgés."
  )
  for (k in keys) {
    expect_true(k %in% fr_keys, info = paste("clé i18n absente :", k))
  }
})

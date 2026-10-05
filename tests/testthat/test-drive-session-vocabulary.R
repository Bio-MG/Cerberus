# =============================================================================
# tests/testthat/test-drive-session-vocabulary.R
#
# Slice 3 — le vocabulaire session-dérivé (GO 2026-10-04, D1 rendue).
#
# Ce qui est épinglé ici, côté APP :
#   1. les CINQ entrées indexées sont une DONNÉE figée (modules, clés, types,
#      étages d'injection) — la table ne se devine pas ;
#   2. la PROJECTION du bloc `vocabulary` : keep-set fermé, choix verbatim D1
#      via `ts_drive_verbatim_guard()` (la MÊME fonction que la preview), les
#      POSITIONS préservées (un choix refusé part en NA, jamais disparu —
#      disparaître désalignerait les index), vocab_rev entier ;
#   3. le RÉSOLVEUR : la forme {index, vocab_rev} est résolue contre les
#      choix de la sonde au battement d'application, avec la matrice de refus
#      INPUT_NOT_READY / VOCAB_STALE / INDEX_OUT_OF_RANGE / PAYLOAD_REFUSED ;
#      une valeur STRING passe INCHANGÉE (chemin interne — les scénarios des
#      tests d'injection par étages restent exactement ce qu'ils étaient) ;
#   4. le flux d'APPLICATION (ts_drive_apply) : l'injection ne voit que des
#      valeurs RÉSOLUES, et un refus ne résout rien.
#
# Le pin de PUBLICATION (la sonde contre les vrais choix du widget) vit dans
# test-mod-bulk-de-drive-vocab.R : il boote le vrai serveur du module.
# =============================================================================

source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")

.VOC <- function() {
  list(condition_col = c("condition", "tissue"),
       covariates    = c("condition", "tissue"),
       group_levels  = c("mock", "CoV2"),
       vocab_rev     = 3L)
}

test_that("the session-derived inputs are DECLARED data, and the table agrees with the allowlist and the stages", {
  # Slice 4 (2026-10-05) : la sixieme entree, les traits WGCNA (index_list,
  # allow_empty, max_items 32).
  expect_length(TS_DRIVE_SESSION_INPUTS, 6L)
  expect_setequal(names(TS_DRIVE_SESSION_INPUTS), c(
    "bulk-de-condition_col", "bulk-de-covariates", "bulk-de-group_ref",
    "bulk-de-group_target", "bulk-pathways-scores_source",
    "bulk-wgcna-wgcna_traits"))
  mods <- vapply(TS_DRIVE_SESSION_INPUTS, function(x) x$module, character(1))
  expect_identical(unname(mods), c("bulk_de", "bulk_de", "bulk_de", "bulk_de",
                                   "bulk_pathways", "bulk_wgcna"))
  keys <- vapply(TS_DRIVE_SESSION_INPUTS, function(x) x$key, character(1))
  expect_identical(unname(keys), c("condition_col", "covariates", "group_levels",
                                   "group_levels", "scores_source", "traits"))
  types <- vapply(TS_DRIVE_SESSION_INPUTS, function(x) x$type, character(1))
  expect_identical(unname(types), c("index", "index_list", "index", "index", "index",
                                    "index_list"))
  # Chaque id est un select ALLOWLISTÉ (jamais un bouton), et son module est
  # celui du module d'appartenance réel.
  for (id in names(TS_DRIVE_SESSION_INPUTS)) {
    e <- ts_drive_allowlist_get(id)
    expect_false(is.null(e), info = id)
    expect_identical(e$kind, "select", info = id)
    expect_identical(e$module, TS_DRIVE_SESSION_INPUTS[[id]]$module, info = id)
  }
  # Les clés déclarées par module sont exactement celles que la table référence.
  expect_setequal(names(TS_DRIVE_VOCABULARY_KEYS),
                  c("bulk_de", "bulk_pathways", "bulk_wgcna"))
  expect_setequal(TS_DRIVE_VOCABULARY_KEYS$bulk_de,
                  unique(unname(keys[mods == "bulk_de"])))
  expect_setequal(TS_DRIVE_VOCABULARY_KEYS$bulk_pathways, "scores_source")
  expect_setequal(TS_DRIVE_VOCABULARY_KEYS$bulk_wgcna, "traits")
  expect_true(TS_DRIVE_VOCAB_MAX_CHOICES > 0L)
  # Les étages d'injection restent la loi : condition_col/covariates en
  # étage 1, le pair ref/target en étage 2 (la dépendance MESURÉE).
  st <- TS_DRIVE_INPUT_STAGES[["bulk_de"]]
  expect_true("bulk-de-condition_col" %in% st[[1]])
  expect_true("bulk-de-covariates" %in% st[[1]])
  expect_false(any(c("bulk-de-group_ref", "bulk-de-group_target") %in% st[[1]]))
  expect_true(all(c("bulk-de-group_ref", "bulk-de-group_target") %in% st[[2]]))
})

test_that("the vocabulary projection keeps the keep-set closed and the positions aligned", {
  # Toute clé hors du keep-set déclaré est LÂCHÉE — une sonde ne peut pas
  # élargir le wire en renvoyant un champ de plus.
  v <- ts_drive_project_vocabulary(
    c(list(condition_col = "condition", covariates = "condition",
           group_levels = c("mock", "CoV2"), vocab_rev = 2L),
      list(sneaky = "classified_data")),
    "bulk_de")
  expect_false("sneaky" %in% names(v))
  expect_true(all(names(v) %in% c(TS_DRIVE_VOCABULARY_KEYS$bulk_de, "vocab_rev")))
  expect_identical(as.integer(v$vocab_rev), 2L)

  # D1 : les choix voyagent verbatim — SAUF ceux qui échouent à la garde
  # partagée, qui partent en NA À LEUR POSITION (jamais supprimés : les index
  # de l'agent doivent rester alignés sur les choix réels du widget).
  v2 <- ts_drive_project_vocabulary(
    list(condition_col = c("gene1/gene2", "condition", "a..b"),
         vocab_rev = 1L),
    "bulk_de")
  expect_identical(as.character(v2$condition_col), c(NA_character_, "condition", NA_character_))

  # Un module qui ne déclare pas de vocabulaire n'en publie PAS — même si sa
  # sonde en renvoie un.
  expect_null(ts_drive_project_vocabulary(list(condition_col = "x", vocab_rev = 1L),
                                           "bulk_filter"))

  # La borne déclarée est fail CLOSED : liste trop longue => liste VIDE et
  # `vocab_error` sanitise. JAMAIS tronquée — tronquer désalignerait les index.
  big <- list(condition_col = paste0("col", seq_len(TS_DRIVE_VOCAB_MAX_CHOICES + 1L)),
              vocab_rev = 1L)
  v3 <- ts_drive_project_vocabulary(big, "bulk_de")
  expect_length(v3$condition_col, 0L)
  expect_match(as.character(v3$vocab_error), "over the bound", fixed = TRUE)
  expect_identical(as.character(ts_drive_badge_sanitize(v3$vocab_error, 200L)),
                   as.character(v3$vocab_error))
})

test_that("the resolver passes plain strings through and refuses honestly", {
  # Pas d'id session-dérivé : ok, inchangé.
  r0 <- ts_drive_resolve_session_inputs(list(`bulk-de-de_engine` = "deseq2"),
                                        "bulk_de", .VOC)
  expect_true(r0$ok)
  # Une valeur STRING pour un id indexé passe INCHANGÉE : c'est le chemin
  # interne (scénarios d'injection par étages), et son comportement ne change
  # pas — la surface INDEXÉE est celle du serveur.
  r1 <- ts_drive_resolve_session_inputs(
    list(`bulk-de-condition_col` = "condition", `bulk-de-de_engine` = "deseq2"),
    "bulk_de", .VOC)
  expect_true(r1$ok)
  expect_identical(r1$values$`bulk-de-condition_col`, "condition")

  # Pas de sonde du tout : INPUT_NOT_READY.
  r2 <- ts_drive_resolve_session_inputs(
    list(`bulk-de-condition_col` = list(index = 1L, vocab_rev = 3L)),
    "bulk_de", NULL)
  expect_false(r2$ok)
  expect_true(startsWith(r2$errors[1], "INPUT_NOT_READY"))

  # Domaine vide (données non chargées) : INPUT_NOT_READY — fail closed, avec
  # la raison.
  voc_empty <- .VOC
  body(voc_empty)[[length(body(voc_empty))]] <- quote(
    list(condition_col = character(0), covariates = character(0),
         group_levels = character(0), vocab_rev = 1L))
  r3 <- ts_drive_resolve_session_inputs(
    list(`bulk-de-condition_col` = list(index = 1L, vocab_rev = 1L)),
    "bulk_de", voc_empty)
  expect_false(r3$ok)
  expect_true(any(startsWith(r3$errors, "INPUT_NOT_READY")))

  # rev dépassée : VOCAB_STALE — l'agent re-snapshotte.
  r4 <- ts_drive_resolve_session_inputs(
    list(`bulk-de-condition_col` = list(index = 1L, vocab_rev = 2L)),
    "bulk_de", .VOC)
  expect_false(r4$ok)
  expect_true(any(startsWith(r4$errors, "VOCAB_STALE")))

  # Hors bornes : INDEX_OUT_OF_RANGE.
  r5 <- ts_drive_resolve_session_inputs(
    list(`bulk-de-condition_col` = list(index = 3L, vocab_rev = 3L)),
    "bulk_de", .VOC)
  expect_false(r5$ok)
  expect_true(any(startsWith(r5$errors, "INDEX_OUT_OF_RANGE")))

  # Formes refusées : index nul, doublons, trop d'items.
  r6 <- ts_drive_resolve_session_inputs(
    list(`bulk-de-condition_col` = list(index = 0L, vocab_rev = 3L)),
    "bulk_de", .VOC)
  expect_true(any(startsWith(r6$errors, "PAYLOAD_REFUSED")))
  r7 <- ts_drive_resolve_session_inputs(
    list(`bulk-de-covariates` = list(index = c(1L, 1L), vocab_rev = 3L)),
    "bulk_de", .VOC)
  expect_true(any(startsWith(r7$errors, "PAYLOAD_REFUSED")))
  r8 <- ts_drive_resolve_session_inputs(
    list(`bulk-de-covariates` = list(index = as.integer(seq_len(9L)), vocab_rev = 3L)),
    "bulk_de", .VOC)
  expect_true(any(startsWith(r8$errors, "PAYLOAD_REFUSED")))

  # Le pair ref == target (même index) : PAYLOAD_REFUSED — re-vérifié côté app.
  r9 <- ts_drive_resolve_session_inputs(
    list(`bulk-de-group_ref`    = list(index = 1L, vocab_rev = 3L),
         `bulk-de-group_target` = list(index = 1L, vocab_rev = 3L)),
    "bulk_de", .VOC)
  expect_false(r9$ok)
  expect_true(any(grepl("same choice", r9$errors, fixed = TRUE)))

  # Chemin heureux : l'index devient la VALEUR, dans l'ordre demandé ; la
  # covariable vide est résolue en vecteur vide (= pas de covariable).
  r10 <- ts_drive_resolve_session_inputs(
    list(`bulk-de-condition_col` = list(index = 2L, vocab_rev = 3L),
         `bulk-de-group_ref`     = list(index = 1L, vocab_rev = 3L),
         `bulk-de-group_target`  = list(index = 2L, vocab_rev = 3L),
         `bulk-de-covariates`    = list(index = list(2L, 1L), vocab_rev = 3L)),
    "bulk_de", .VOC)
  expect_true(r10$ok)
  expect_identical(as.character(r10$values$`bulk-de-condition_col`), "tissue")
  expect_identical(as.character(r10$values$`bulk-de-group_ref`), "mock")
  expect_identical(as.character(r10$values$`bulk-de-group_target`), "CoV2")
  expect_identical(as.character(unlist(r10$values$`bulk-de-covariates`)),
                   c("tissue", "condition"))
})

test_that("the apply resolves indexed inputs before injection, and a refusal resolves nothing", {
  injects <- list()
  inject <- function(inputs, module) {
    injects[[length(injects) + 1L]] <<- inputs
    list(applied = names(inputs), refused = character(0), warnings = character(0))
  }
  effects <- function(id, mode = NULL, module = NULL, request = NULL) {
    if (identical(mode, "tokens")) {
      return(list(`bulk-de-run_de` = list(counter = NULL, vocab = .VOC)))
    }
    FALSE
  }
  scn <- function(inputs) list(
    seq = 11L, module = "bulk_de", action = "set_inputs",
    preserve_data = TRUE, inputs = inputs)

  # Chemin heureux : l'injecteur reçoit des NOMS, jamais des index.
  v <- ts_drive_apply(NULL, NULL, scn(list(
    `bulk-de-condition_col` = list(index = 1L, vocab_rev = 3L),
    `bulk-de-de_engine`     = "deseq2")),
    effects = effects, inject = inject)
  expect_identical(v$status, "applied")
  expect_length(injects, 1L)
  expect_identical(as.character(injects[[1]]$`bulk-de-condition_col`), "condition")
  expect_identical(as.character(injects[[1]]$`bulk-de-de_engine`), "deseq2")

  # Chaque refus => verdict `invalid`, code en tête d'errors[], RIEN n'injecté.
  for (case in list(
    list(list(`bulk-de-condition_col` = list(index = 1L, vocab_rev = 9L)), "VOCAB_STALE"),
    list(list(`bulk-de-condition_col` = list(index = 7L, vocab_rev = 3L)), "INDEX_OUT_OF_RANGE"),
    list(list(`bulk-de-condition_col` = list(index = 1L, vocab_rev = 3L),
              `bulk-de-group_ref`    = list(index = 1L, vocab_rev = 3L),
              `bulk-de-group_target` = list(index = 1L, vocab_rev = 3L)), "PAYLOAD_REFUSED"))) {
    n_before <- length(injects)
    v2 <- ts_drive_apply(NULL, NULL, scn(case[[1]]),
                         effects = effects, inject = inject)
    expect_identical(v2$status, "invalid")
    expect_true(startsWith(as.character(v2$errors[1]), case[[2]]),
                info = case[[2]])
    expect_length(injects, n_before)
  }

  # Sans sonde publiée : INPUT_NOT_READY, et rien n'injecté.
  effects_novocab <- function(id, mode = NULL, module = NULL, request = NULL) {
    if (identical(mode, "tokens")) return(list(`bulk-de-run_de` = list(counter = NULL)))
    FALSE
  }
  v3 <- ts_drive_apply(NULL, NULL, scn(list(
    `bulk-de-condition_col` = list(index = 1L, vocab_rev = 3L))),
    effects = effects_novocab, inject = inject)
  expect_identical(v3$status, "invalid")
  expect_true(startsWith(as.character(v3$errors[1]), "INPUT_NOT_READY"))
  expect_length(injects, 1L)
})

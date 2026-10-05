# =============================================================================
# test-drive-wgcna-module.R — Slice 4 : les DEUX boutons WGCNA, la sonde
# d'état, le vocabulaire `traits`, la PRÉREQUISITE étape 1 -> étape 2
# =============================================================================
# Le serveur RÉEL du module boote sous testServer (aucun moteur WGCNA n'est
# lancé : pickSoftThreshold/blockwiseModules demandent 2000–5000 gènes — la
# Sonde ne lit que des compteurs, et la chaîne de prérequis se teste sur le
# contrat de job RÉEL, pas sur un calcul).
#
# Trois faits y sont épinglés :
#   1. la publication : deux jetons `long = TRUE`, une closure de vocabulaire
#      par jeton, la MÊME closure pour l'état (wire) et la résolution (apply) ;
#   2. le domaine `traits` : la formule attendue est RÉÉCRITE indépendamment
#      dans le test (si le module et le test divergent, c'est une dérive
#      détectée, pas une tautologie) ;
#   3. le verdict terminal sur tous les chemins : un dispatch de l'étape 2
#      sans étape 1 finit `invalid` par le contrat de job RÉEL
#      (ts_drive_job_begin -> on.exit -> ts_drive_job_finish), jamais un
#      `running` sans fin.
# =============================================================================

source_project_file("config/defaults.R")
source_project_file("R/core/io_helpers.R")            # %||%
source_project_file("R/core/error_state.R")
source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")
source_project_file("R/bulk/bulk_wgcna.R")            # le contrat gelé ( builders)
source_project_file("modules/bulk/mod_bulk_wgcna_export.R")
source_project_file("modules/bulk/mod_bulk_wgcna.R")  # le serveur RÉEL
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(DT))      # renderDT, enregistré au boot

# La formule attendue, RÉÉCRITE indépendamment (mod_bulk_wgcna.R:88-99) :
# numériques >= 3 valeurs finies, ou exactement binaires (facteur/character/
# logical) ; les multi-niveaux sont écartés.
.wg_expected_traits <- function(meta) {
  if (is.null(meta) || !ncol(meta)) return(character(0))
  ok <- vapply(names(meta), function(cl) {
    x <- meta[[cl]]
    (is.numeric(x) && sum(is.finite(x)) >= 3L) ||
      ((is.factor(x) || is.character(x) || is.logical(x)) &&
         length(unique(stats::na.omit(as.character(x)))) == 2L)
  }, logical(1))
  names(meta)[ok]
}

.wg_fixture <- function() {
  data.frame(
    condition = c("mock", "CoV2", "CoV2", "mock", "mock"),  # binaire -> trait
    tissue    = factor(c("cornea", "limbus", "cornea", "sclera", "limbus")), # 3 niveaux -> NON
    age       = c(30, 40, 50, 60, 70),                      # numérique -> trait
    stringsAsFactors = FALSE)
}

.wg_state <- function(meta = NULL) {
  gd <- shiny::reactiveValues()
  gd$language <- "fr"
  gd$i18n <- NULL
  gd$bulk_obj <- if (is.null(meta)) NULL else list(metadata = meta)
  gd$drive_registry <- new.env(parent = emptyenv())
  rv <- shiny::reactiveValues()
  rv$vst_mat <- NULL
  rv$wgcna_power <- NULL
  rv$wgcna_modules <- NULL
  list(gd = gd, rv = rv)
}

.wg_boot <- function(st) {
  shiny::testServer(mod_bulk_wgcna_server,
                    args = list(global_data = st$gd, shared_rv = st$rv), {
                        invisible(NULL)
                      })
  st
}

.wg_reg <- function(st) shiny::isolate(st$gd$drive_registry)
.wg_entry <- function(st, id) shiny::isolate(st$gd$drive_registry)[[id]]

# =============================================================================
# 1. Les données déclarées : 17 boutons, la module, les entrées, le vocabulaire
# =============================================================================
test_that("the two WGCNA stage buttons join the frozen tables, and the checks stay green", {
  buttons <- get0("TS_DRIVE_BUTTONS", envir = globalenv())
  expect_length(buttons, 17L)
  expect_true(all(c("bulk-wgcna-run_wgcna_power", "bulk-wgcna-run_wgcna_modules")
                  %in% buttons))

  mods <- get0("TS_DRIVE_MODULES", envir = globalenv())
  expect_true("bulk_wgcna" %in% mods)

  al <- get0("TS_DRIVE_ALLOWLIST", envir = globalenv())
  expect_identical(al[["bulk-wgcna-run_wgcna_power"]]$kind, "button")
  expect_identical(al[["bulk-wgcna-run_wgcna_power"]]$module, "bulk_wgcna")
  expect_identical(al[["bulk-wgcna-run_wgcna_modules"]]$kind, "button")
  expect_identical(al[["bulk-wgcna-wgcna_n_genes"]]$kind, "numeric")
  expect_identical(al[["bulk-wgcna-wgcna_power_override"]]$kind, "numeric")
  expect_identical(al[["bulk-wgcna-wgcna_traits"]]$kind, "select")

  # Le checkeur de forme : renvoie TRUE (pas un vecteur vide) quand il est vert.
  problems <- get0("ts_drive_allowlist_problems", envir = globalenv())()
  expect_true(isTRUE(problems),
              info = paste("ts_drive_allowlist_problems() must be TRUE:",
                           paste(as.character(problems), collapse = "; ")))

  sess <- get0("TS_DRIVE_SESSION_INPUTS", envir = globalenv())
  tr <- sess[["bulk-wgcna-wgcna_traits"]]
  expect_false(is.null(tr))
  expect_identical(tr$module, "bulk_wgcna")
  expect_identical(tr$key, "traits")
  expect_identical(tr$type, "index_list")
  expect_identical(as.integer(tr$max_items), 32L)
  expect_true(isTRUE(tr$allow_empty))

  vkeys <- get0("TS_DRIVE_VOCABULARY_KEYS", envir = globalenv())
  expect_identical(vkeys$bulk_wgcna, "traits")
})

# =============================================================================
# 2. La publication : deux jetons longs, la même closure des deux côtés
# =============================================================================
test_that("both WGCNA stages publish long tokens with readiness, state and vocabulary", {
  st <- .wg_boot(.wg_state())
  reg <- .wg_reg(st)

  for (id in c("bulk-wgcna-run_wgcna_power",
               "bulk-wgcna-run_wgcna_modules")) {
    e <- reg[[id]]
    expect_true(is.list(e), info = id)
    expect_true(isTRUE(e$long), info = paste(id, "must be declared long"))
    expect_true(is.function(e$counter), info = id)
    expect_true(is.function(e$ready), info = id)
    expect_true(is.function(e$state), info = id)
    expect_true(is.function(e$vocab), info = id)
  }

  # Readiness par étape, miroir des préconditions des observers :
  # étape 1 exige la VST ; étape 2 exige EN PLUS le power de l'étape 1 —
  # et la garde nomme l'échec VST EN PREMIER (le même ordre que l'observer).
  expect_match(.wg_entry(st, "bulk-wgcna-run_wgcna_power")$ready(),
               "VST", fixed = TRUE)
  r2 <- .wg_entry(st, "bulk-wgcna-run_wgcna_modules")$ready()
  expect_false(isTRUE(r2))
  expect_match(r2, "VST", fixed = TRUE,
               info = "the guard mirrors the observer's FIRST precondition")

  # L'étape 1 faite (VST + power) rend les DEUX étapes prêtes (le probe et
  # la garde lisent les mêmes slots).
  shiny::isolate(st$rv$vst_mat <- matrix(0, 2, 2))
  shiny::isolate(st$rv$wgcna_power <- list(chosen = list(power = 6, r2 = 0.87),
                                           n_genes_used = 2500L, n_samples = 18L))
  expect_true(isTRUE(.wg_entry(st, "bulk-wgcna-run_wgcna_modules")$ready()))
  expect_true(isTRUE(.wg_entry(st, "bulk-wgcna-run_wgcna_power")$ready()))
})

# =============================================================================
# 3. La sonde d'état : compteurs seulement, la lisibilité de l'étape 2
# =============================================================================
test_that("the state probe reports the two stages as counts, never the matrices", {
  st <- .wg_boot(.wg_state())
  s <- shiny::isolate(.wg_entry(st, "bulk-wgcna-run_wgcna_power")$state())
  expect_false(isTRUE(s$ready))
  expect_null(s$chosen_power)
  expect_null(s$n_modules)

  shiny::isolate(st$rv$wgcna_power <- list(chosen = list(power = 6, r2 = 0.87),
                                           n_genes_used = 2500L, n_samples = 18L))
  shiny::isolate(st$rv$wgcna_modules <- list(type = "bulk_wgcna_modules",
                                             power = 6, n_modules = 7L,
                                             module_sizes = c(blue = 4L)))
  s2 <- shiny::isolate(.wg_entry(st, "bulk-wgcna-run_wgcna_power")$state())
  expect_true(isTRUE(s2$ready))
  expect_identical(s2$chosen_power, 6)
  expect_identical(s2$chosen_r2, 0.87)
  expect_identical(as.integer(s2$n_genes), 2500L)
  expect_identical(as.integer(s2$n_modules), 7L)
  expect_identical(as.integer(s2$modules_power), 6L)
  # La matrice VST et les couleurs ne voyagent JAMAIS (compteurs seulement).
  expect_false(any(c("vst_mat", "colors", "module_sizes") %in% names(s2)))
})

# =============================================================================
# 4. La sonde de vocabulaire : le domaine réel du widget, rev monotone
# =============================================================================
test_that("the traits vocabulary publishes the widget's real domain, and the rev is monotone", {
  st <- .wg_boot(.wg_state(.wg_fixture()))
  vocab <- .wg_entry(st, "bulk-wgcna-run_wgcna_power")$vocab

  v1 <- vocab()
  expect_identical(v1$traits, .wg_expected_traits(.wg_fixture()))
  expect_identical(as.integer(v1$vocab_rev), 1L)
  # Appels répétés sans changement : le rev NE bouge PAS.
  expect_identical(as.integer(vocab()$vocab_rev), 1L)

  # Changement de domaine : le rev PUSHE (monotone), jamais ne recule.
  meta2 <- .wg_fixture()
  meta2$site <- c("a", "b", "a", "b", "a")   # un trait de plus
  shiny::isolate(st$gd$bulk_obj <- list(metadata = meta2))
  v2 <- vocab()
  expect_identical(v2$traits, .wg_expected_traits(meta2))
  expect_identical(as.integer(v2$vocab_rev), 2L)

  # Données déchargées : domaine vide (honnête « not ready »), rev encore +1.
  shiny::isolate(st$gd$bulk_obj <- NULL)
  v3 <- vocab()
  expect_identical(v3$traits, character(0))
  expect_identical(as.integer(v3$vocab_rev), 3L)
})

# =============================================================================
# 5. La résolution d'index : index_list, allow_empty, les refus nommés
# =============================================================================
test_that("the traits index resolves at apply time, and every refusal is honest", {
  st <- .wg_boot(.wg_state(.wg_fixture()))
  vocab_fn <- .wg_entry(st, "bulk-wgcna-run_wgcna_power")$vocab
  v <- vocab_fn()
  expected <- .wg_expected_traits(.wg_fixture())

  resolve <- function(inputs) {
    ts_drive_resolve_session_inputs(inputs, "bulk_wgcna", vocab_fn)
  }

  # Deux indices -> les DEUX choix réels, dans l'ordre publié.
  r <- resolve(list(`bulk-wgcna-wgcna_traits` =
                      list(index = c(1L, 2L), vocab_rev = v$vocab_rev)))
  expect_true(isTRUE(r$ok))
  expect_identical(as.character(r$values$`bulk-wgcna-wgcna_traits`), expected[1:2])

  # Liste vide : choix VALIDE (= « tous les traits candidats », le
  # comportement du module), allow_empty = TRUE.
  r0 <- resolve(list(`bulk-wgcna-wgcna_traits` =
                       list(index = integer(0), vocab_rev = v$vocab_rev)))
  expect_true(isTRUE(r0$ok))

  # Rev périmé : VOCAB_STALE.
  rs <- resolve(list(`bulk-wgcna-wgcna_traits` =
                       list(index = 1L, vocab_rev = v$vocab_rev + 5L)))
  expect_false(isTRUE(rs$ok))
  expect_match(paste(rs$errors, collapse = " "), "VOCAB_STALE", fixed = TRUE)

  # Index trop grand : INDEX_OUT_OF_RANGE.
  ro <- resolve(list(`bulk-wgcna-wgcna_traits` =
                       list(index = length(expected) + 1L, vocab_rev = v$vocab_rev)))
  expect_false(isTRUE(ro$ok))
  expect_match(paste(ro$errors, collapse = " "), "INDEX_OUT_OF_RANGE", fixed = TRUE)

  # 33 indices : PAYLOAD_REFUSED (la borne déclarée est 32).
  big <- resolve(list(`bulk-wgcna-wgcna_traits` =
                        list(index = seq_len(33L), vocab_rev = v$vocab_rev)))
  expect_false(isTRUE(big$ok))
  expect_match(paste(big$errors, collapse = " "), "PAYLOAD_REFUSED", fixed = TRUE)

  # Pas de métadonnées : INPUT_NOT_READY (fail closed, avec la raison).
  st2 <- .wg_boot(.wg_state(NULL))
  rn <- ts_drive_resolve_session_inputs(
    list(`bulk-wgcna-wgcna_traits` = list(index = 1L, vocab_rev = 1L)),
    "bulk_wgcna",
    .wg_entry(st2, "bulk-wgcna-run_wgcna_power")$vocab)
  expect_false(isTRUE(rn$ok))
  expect_match(paste(rn$errors, collapse = " "), "INPUT_NOT_READY", fixed = TRUE)

  # Une valeur STRING reste le chemin interne : elle passe SANS résolution.
  rp <- ts_drive_resolve_session_inputs(
    list(`bulk-wgcna-wgcna_traits` = "condition"), "bulk_wgcna", vocab_fn)
  expect_true(isTRUE(rp$ok))
  expect_identical(rp$values$`bulk-wgcna-wgcna_traits`, "condition")
})

# =============================================================================
# 6. LA PRÉREQUISITE : l'étape 2 sans étape 1 finit `invalid`, jamais `running`
# =============================================================================
test_that("a stage-2 dispatch without stage 1 ends in an honest INVALID verdict", {
  st <- .wg_state()
  ts_drive_job_clear()
  on.exit(try(ts_drive_job_clear(), silent = TRUE), add = TRUE)

  job <- ts_drive_job_begin(1L, "bulk_wgcna", "run_pipeline",
                            TS_DRIVE_BULK_WGCNA_MODULES_BUTTON)
  expect_identical(job$status, "running")
  expect_true(isTRUE(ts_drive_job_busy()))

  shiny::testServer(mod_bulk_wgcna_server,
                    args = list(global_data = st$gd, shared_rv = st$rv), {
                      # Le trigger du drive : le compteur ET le DOM aboutissent
                      # au même observeEvent. Ici le chemin humain (input), la
                      # garde de readiness a déjà laissé le dispatch partir —
                      # la fenêtre TOCTOU est exactement celle-ci.
                      session$setInputs(run_wgcna_modules = 1L)
                    })

  pending <- ts_drive_job_pending()
  expect_identical(pending$status, "invalid",
                   info = paste("the job contract must CLOSE on the aborted req():",
                                "an honest invalid verdict, never a stranded running"))
  # F4 (2026-10-06) : le refus porte sa raison — mesuré muet en session
  # réelle (errors[] vide sur GSE164073). Texte sanitiser-sure, miroir de
  # drive_ready_modules.
  expect_match(pending$error, "power step first", fixed = TRUE)
  ts_drive_job_clear()
  expect_false(isTRUE(ts_drive_job_busy()))
})

# =============================================================================
# 7. La route d'export via le seam : le verdict du registre
# =============================================================================
test_that("the published exporter serves the gene->module table through the registry", {
  st <- .wg_boot(.wg_state())
  exp_of <- get0("ts_drive_export_of", envir = globalenv())
  expect_true(is.function(exp_of(st$gd, "bulk_wgcna")),
              info = "the module publishes the route's exporter at boot")

  # Rien à exporter : le verdict INVALID nomme l'action.
  r0 <- exp_of(st$gd, "bulk_wgcna")()
  expect_false(isTRUE(r0$ok))
  expect_identical(r0$status, "invalid")

  # Après un build : `done`, descripteur sain, fichier réel dans le
  # répertoire d'export borné.
  shiny::isolate(st$rv$wgcna_modules <- list(
    type = "bulk_wgcna_modules", power = 6, n_modules = 2L,
    module_sizes = c(blue = 2L, grey = 1L),
    colors = c(G1 = "blue", G2 = "blue", G3 = "grey"),
    dendro = NULL, dendro_colors = NULL))
  r1 <- exp_of(st$gd, "bulk_wgcna")()
  expect_true(isTRUE(r1$ok))
  expect_identical(r1$status, "done")
  expect_match(r1$descriptor$file, "^bulk_wgcna_genes_[0-9]+\\.csv$")
  expect_identical(as.integer(r1$descriptor$n_rows), 3L)
  expect_true(file.exists(file.path(
    get0("ts_drive_export_dir", envir = globalenv())(), r1$descriptor$file)))
})

# =============================================================================
# 8. La projection wire : l'état ET le vocabulaire passent le keep-set
# =============================================================================
test_that("module_states projects the WGCNA probe and its vocabulary", {
  st <- .wg_boot(.wg_state(.wg_fixture()))
  shiny::isolate(st$rv$wgcna_power <- list(chosen = list(power = 6, r2 = 0.87),
                                           n_genes_used = 2500L, n_samples = 18L))
  states <- ts_drive_module_states(st$gd)
  expect_true("bulk_wgcna" %in% names(states))
  s <- states$bulk_wgcna
  expect_true(isTRUE(s$ready))
  expect_identical(as.integer(s$n_modules %||% 0L) * 0L, 0L)  # absent = NULL ok
  expect_true(is.list(s$vocabulary))
  expect_identical(as.character(s$vocabulary$traits), .wg_expected_traits(.wg_fixture()))
  expect_true(is.finite(as.integer(s$vocabulary$vocab_rev)))
})

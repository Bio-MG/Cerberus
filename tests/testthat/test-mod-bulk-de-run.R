# =============================================================================
# test-mod-bulk-de-run.R — the terminal OUTCOME of the DE job
# (2026-09-24, jalon « DE error-outcome correctness »)
# =============================================================================
# Eponymous test for `modules/bulk_de/mod_bulk_de_run.R`.
#
# 🔴 **LE DÉFAUT QUE CE FICHIER EXISTE POUR TUER**, mesuré le 2026-09-24
# (`STATUS.md` §2do.4) :
#
# L'observateur déclare son job (contrat B, `long = TRUE`) puis, dans son
# `tryCatch`, écrit le résultat par une affectation LOCALE :
#
#     job_outcome <- "refused"
#     on.exit(ts_drive_job_finish(..., status = switch(job_outcome, ok = "done",
#                                                      failed = "error", "invalid")),
#             add = TRUE)
#     ...
#     tryCatch({ ...; job_outcome <- "ok" },
#              error = function(e) { job_outcome <- "failed"; ... })
#
# Or un gestionnaire `error = function(e)` est une **closure** : son `<-` se lie
# à la frame DU GESTIONNAIRE, pas à celle de l'observateur. La frame de
# l'observateur reste donc à `"refused"`, et l'`on.exit()` publie **`invalid`** —
# « un garde a renoncé, rien n'a tourné » — pour une DE qui a **LEVÉ**.
#
# Deux conséquences, toutes deux mesurables : l'opérateur est invité à ignorer un
# badge rouge (un refus n'est pas un échec), et l'erreur d'exécution est
# **cachée** — `error = NULL`, aucune trace sur le fil.
#
# Le correctif est une **cellule mutable** (`new.env(parent = emptyenv())`), qui
# a une sémantique de RÉFÉRENCE : l'écriture du gestionnaire est vue par la
# lecture de l'`on.exit()`. Ce fichier le prouve par COMPORTEMENT — il exécute le
# vrai bloc d'observateur, il ne relit pas le source.
#
# ⚠️ Ce fichier ne teste QUE la comptabilité d'issue du job. Il ne touche ni au
# calcul statistique, ni au protocole, ni à l'allowlist.

.MDR_FILE   <- "modules/bulk_de/mod_bulk_de_run.R"
.MDR_BUTTON <- "bulk-de-run_de"

# Un message d'erreur PIÉGÉ : il porte un faux jeton (>= 8 alphanumériques) et un
# chemin absolu. Les deux DOIVENT disparaître avant publication — c'est la
# propriété de sécurité du champ `error`, le seul champ LIBRE du contrat.
.MDR_SECRET <- "TOKENabcdef123456"
.MDR_PATH   <- "D:/secret/dir/file.R"
.MDR_MSG    <- paste0("DE boom ", .MDR_SECRET, " at ", .MDR_PATH)

# --- Le bloc d'observateur, extrait par AST (jamais par numéro de ligne) -----
.mdr_block <- function() ts_ast_observe_block(.MDR_FILE, "drive_trigger")

# --- Un VRAI appel de fonction autour du bloc ---------------------------------
# Indispensable : `on.exit()` s'enregistre sur la frame de la FONCTION courante.
# Évaluer le bloc avec `eval()` l'attacherait à la frame du TEST, et les
# expressions de sortie seraient résolues ailleurs que dans notre environnement.
.mdr_runner <- function(envir) {
  f <- function() NULL
  body(f) <- .mdr_block()
  environment(f) <- envir
  f
}

.mdr_fake_res <- function() {
  data.frame(log2FoldChange = c(2, -2, 0.1, 3),
             padj           = c(0.001, 0.002, 0.9, 0.003),
             baseMean       = rep(10, 4L))
}

# --- Environnement enfant : `input` / `shared_rv` / job / recorder simulés ----
# `ts_drive_badge_sanitize` est le VRAI (sourcé depuis `drive_allowlist.R`) : un
# test qui re-implémenterait le caviardage ne prouverait rien sur le caviardage
# réellement utilisé.
.mdr_env <- function(dispatch = NULL, engine = "deseq2", req_aborts = FALSE) {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "R/core/drive_allowlist.R"), envir = e)

  e$`%||%` <- function(a, b) if (is.null(a)) b else a

  # Le RECORDER : ce que le module passe réellement à la couture terminale.
  e$calls <- list()
  e$ts_drive_job_busy   <- function() TRUE
  e$ts_drive_job_state  <- function() list(button = .MDR_BUTTON)
  e$ts_drive_job_finish <- function(button, status = "done", error = NULL) {
    e$calls[[length(e$calls) + 1L]] <- list(button = button, status = status,
                                            error = error)
    invisible(TRUE)
  }

  # `req()` de Shiny avorte en LEVANT une condition silencieuse.
  # `"condition"` doit figurer dans la classe : sans lui, `conditionMessage()`
  # n'a aucune methode et l'erreur devient un ERROR de test au lieu d'un chemin
  # de sortie observable (mesure : 2 ERROR avant ce correctif).
  e$req <- if (isTRUE(req_aborts)) {
    function(...) stop(structure(class = c("shiny.silent.error", "error", "condition"),
                                 list(message = "", call = NULL)))
  } else {
    function(...) invisible(TRUE)
  }

  e$drive_counter <- function() 0L
  e$input <- list(run_de = 1L, condition_col = "cond", group_ref = "a",
                  group_target = "b", de_engine = engine,
                  covariates = character(0), no_rep_enable = FALSE,
                  no_rep_attest = FALSE, no_rep_bcv = 0.4,
                  padj_method = "BH", shrink_lfc = FALSE, contrast_name = "",
                  padj_thresh = 0.05, lfc_thresh = 1)
  e$session     <- list()
  e$global_data <- list(
    bulk_obj = list(metadata = data.frame(
      cond = c("a", "a", "a", "b", "b", "b"),
      row.names = paste0("s", 1:6))),
    i18n = NULL)

  # `shared_rv` a une sémantique de RÉFÉRENCE dans l'app (reactiveValues) : un
  # environnement la reproduit, une liste serait COPIÉE et l'écriture perdue.
  e$shared_rv <- list2env(list(
    filtered_counts = matrix(1:24, nrow = 6L),
    contrasts = list(), active_contrast = NULL,
    de_bypass = NULL, dds_full = NULL), parent = emptyenv())

  e$helpers <- list(
    design_str         = function() "~ cond",
    register_contrast  = function(name, res) { e$shared_rv$contrasts[[name]] <- res },
    remember_padj_ctx  = function(...) invisible(NULL))

  e$TS_PADJ_METHOD_DEFAULT    <- "BH"
  e$check_design_confounding  <- function(...) FALSE
  e$design_saturation         <- function(...) list(saturated = FALSE, n = 6L, p = 2L)
  e$build_dds                 <- function(...) structure(list(fake = TRUE), class = "dds")
  e$.normalize_de_cols        <- function(res, ...) res
  e$updateSelectInput         <- function(...) invisible(NULL)
  e$showNotification          <- function(...) invisible(NULL)
  e$.tr                       <- function(key) key
  e$.t_fmt                    <- function(fmt, ...) fmt

  # Le seul point d'injection : la DE elle-même. `stop()` ici = « la DE a levé ».
  e$run_bulk_de_dispatch <- if (is.null(dispatch)) {
    function(...) .mdr_fake_res()
  } else {
    dispatch
  }
  e
}

# --- Exécute le VRAI bloc et laisse `on.exit()` faire son travail -------------
# Le domaine réactif n'est pas un confort : MESURÉ, `shiny::Progress$new()` LEVE
# « Can only use Progress$new() inside a Shiny app » hors session. Or la barre de
# progression est créée AVANT le `tryCatch` du module — sans domaine, le bloc
# mourrait avant d'atteindre le chemin d'erreur que ce fichier doit mesurer, et
# le test prouverait autre chose que ce qu'il annonce.
.mdr_run <- function(e) {
  f <- .mdr_runner(e)
  shiny::withReactiveDomain(shiny::MockShinySession$new(), {
    tryCatch(f(), error = function(err) {
      # `req()` avorte en silence : c'est le comportement de Shiny, pas un bug.
      if (!inherits(err, "shiny.silent.error")) stop(err)
      invisible(NULL)
    })
  })
  invisible(e)
}

.mdr_last <- function(e) e$calls[[length(e$calls)]]

# Un message qui dit « la couture n'a pas été appelée » vaut mieux qu'un `NULL`
# qui fait échouer cinq assertions avec le même « subscript out of bounds ».
.mdr_status <- function(e) {
  last <- .mdr_last(e)
  if (is.null(last)) NA_character_ else last$status
}

# ---------------------------------------------------------------------------
# (a) 🔴 LE CŒUR DU JALON — une DE qui LÈVE doit publier `error`
# ---------------------------------------------------------------------------
test_that("une DE qui leve publie `error`, ni `invalid` ni `refused`", {
  e <- .mdr_env(dispatch = function(...) stop(.MDR_MSG))
  .mdr_run(e)

  expect_length(e$calls, 1L)
  expect_identical(.mdr_status(e), "error",
                   info = "la DE a LEVE : l'issue doit etre `error`, pas un refus")
})

# ---------------------------------------------------------------------------
# (b) Le champ `error` est PRESENT, CAVIARDE et TRONQUE
# ---------------------------------------------------------------------------
test_that("le message d'erreur publie est present et caviarde", {
  e <- .mdr_env(dispatch = function(...) stop(.MDR_MSG))
  .mdr_run(e)

  last <- .mdr_last(e)
  expect_false(is.null(last))
  err <- last$error

  expect_true(is.character(err) && length(err) == 1L && nzchar(err),
              info = "aucun message d'erreur publie (champ vide ou absent)")

  # Le faux jeton et le chemin absolu ne doivent PAS survivre : le badge est
  # rendu dans l'UI, lisible par quiconque a l'onglet ouvert.
  expect_false(grepl(.MDR_SECRET, err, fixed = TRUE),
               info = "un jeton traverse le caviardage du champ error")
  expect_false(grepl("secret/dir", err, fixed = TRUE),
               info = "un chemin absolu traverse le caviardage du champ error")
  # ... et le caviardage doit avoir EFFECTIVEMENT tourne (pas un `""` muet).
  expect_true(grepl("<redacted>", err, fixed = TRUE),
              info = "le jeton n'a pas ete remplace par <redacted>")
  expect_true(grepl("<path>", err, fixed = TRUE),
              info = "le chemin n'a pas ete remplace par <path>")
  # Le texte utile survit : un message vide serait « caviarde » mais inutile.
  expect_true(grepl("DE boom", err, fixed = TRUE),
              info = "le message publie ne dit plus rien d'actionnable")
})

# ---------------------------------------------------------------------------
# (c) Le chemin NOMINAL reste `done` (preserve)
# ---------------------------------------------------------------------------
test_that("une DE reussie reste `done`", {
  e <- .mdr_env()
  .mdr_run(e)

  expect_identical(.mdr_status(e), "done")
  expect_null(.mdr_last(e)$error,
              info = "le champ error doit rester NULL hors `error`")
})

# ---------------------------------------------------------------------------
# (d) Un REFUS reste `invalid` (preserve : ce n'est PAS un echec)
# ---------------------------------------------------------------------------
test_that("un refus publie `invalid`, pas `error`", {
  e <- .mdr_env(req_aborts = TRUE)
  .mdr_run(e)

  expect_identical(.mdr_status(e), "invalid",
                   info = "un garde qui renonce n'est pas une DE qui echoue")
})

# ---------------------------------------------------------------------------
# (e) Aucune issue PERIMEE ne fuit dans le run suivant
# ---------------------------------------------------------------------------
test_that("une issue echouee ne fuit pas dans le run suivant", {
  e <- .mdr_env(dispatch = function(...) stop(.MDR_MSG))
  .mdr_run(e)
  expect_identical(.mdr_status(e), "error")

  # Le meme module, une DE qui reussit : l'issue doit etre RECALCULEE, pas
  # heritee. Une cellule creee UNE fois hors de l'observateur produirait ici
  # un `error` fantome.
  e$run_bulk_de_dispatch <- function(...) .mdr_fake_res()
  .mdr_run(e)
  expect_identical(.mdr_status(e), "done",
                   info = "l'issue du run precedent a fuite dans le suivant")
})

# ---------------------------------------------------------------------------
# (f) Un second run apres un echec repart PROPREMENT
# ---------------------------------------------------------------------------
test_that("un second run apres echec repart proprement", {
  e <- .mdr_env(dispatch = function(...) stop(.MDR_MSG))
  .mdr_run(e)
  e$run_bulk_de_dispatch <- function(...) .mdr_fake_res()
  .mdr_run(e)

  # Deux runs, deux fermetures : le job n'est pas reste en vol (contrat B), donc
  # le `run_pipeline` suivant n'est pas refuse.
  expect_length(e$calls, 2L)
  expect_identical(vapply(e$calls, function(c) c$status, character(1)),
                   c("error", "done"))
  expect_true(all(vapply(e$calls, function(c) identical(c$button, .MDR_BUTTON),
                         logical(1))))
})

# ---------------------------------------------------------------------------
# (g) La cellule de job est fermee sur CHAQUE chemin de sortie
# ---------------------------------------------------------------------------
test_that("le job est ferme sur chaque chemin de sortie", {
  # nominal
  e1 <- .mdr_env(); .mdr_run(e1)
  expect_length(e1$calls, 1L)
  # echec
  e2 <- .mdr_env(dispatch = function(...) stop(.MDR_MSG)); .mdr_run(e2)
  expect_length(e2$calls, 1L)
  # refus (abandon de `req`)
  e3 <- .mdr_env(req_aborts = TRUE); .mdr_run(e3)
  expect_length(e3$calls, 1L)

  # Un job declare `long` et JAMAIS ferme rendrait le module definitivement
  # impilotable (tout `run_pipeline` suivant serait refuse).
  for (e in list(e1, e2, e3)) {
    expect_identical(e$calls[[1L]]$button, .MDR_BUTTON)
    expect_true(e$calls[[1L]]$status %in% c("done", "error", "invalid"))
  }
})

# ---------------------------------------------------------------------------
# (h) Le job n'est PAS ferme quand le job en vol n'est pas le sien
# ---------------------------------------------------------------------------
test_that("le module ne ferme pas le job d'un autre bouton", {
  e <- .mdr_env()
  e$ts_drive_job_state <- function() list(button = "bulk-de-run_de_typo")
  .mdr_run(e)
  # `ts_drive_job_finish()` refuse deja par `button` : le module ne doit pas
  # l'appeler du tout pour un job qui n'est pas le sien.
  expect_length(e$calls, 0L)
})

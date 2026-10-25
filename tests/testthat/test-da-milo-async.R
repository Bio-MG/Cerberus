# =============================================================================
# test-da-milo-async.R — 4E-4 : la DA Milo tourne dans le POOL APPLICATIF
# =============================================================================
# Couvre :
#   1. la liste de preload applicative (APP_DAEMON_SOURCE_FILES) — un chemin
#      errone n'est PAS fatal dans un daemon (il se contente d'un message que
#      personne ne lit) : la garde est donc STATIQUE et exhaustive ;
#   2. le cablage du module (run_job + pool applicatif, scCODA exclu) ;
#   3. la garde decisive : a graine egale, le resultat rendu par le daemon est
#      IDENTIQUE a celui du chemin synchrone. C'est exactement ce que la
#      mesure du 2026-09-16 a mis en defaut (RNGkind du daemon), et rien
#      d'autre ne l'attrape : les deux chemins renvoyaient `valid`.
# =============================================================================

source_project_file("R/core/jobs.R")
source_project_file("tools/pkg_skeleton/R/spatial_async.R")  # ex-R/spatial/, D2 M5 lot 4

test_that("APP_DAEMON_SOURCE_FILES couvre la fermeture Milo et pointe des fichiers reels", {
  # La fermeture mesuree de run_milo_da() doit etre presente, sinon le daemon
  # echoue a l'execution avec « could not find function ».
  # ⚠️ D2 M5 lot 4 : les 7 chemins spatial du preload suivent le fichier dans
  # l'arbre du paquet (meme ordre v5 : async, io, multi, niche, deconv_prep,
  # deconv_tasks, stats).
  expect_true(all(c(
    "tools/pkg_skeleton/R/spatial_async.R",
    "config/defaults.R", "config/thresholds.R",
    "tools/pkg_skeleton/R/io_helpers.R", "tools/pkg_skeleton/R/state.R",
    "tools/pkg_skeleton/R/provenance.R",
    "R/core/validation.R",
    "R/sc/sc_velocity.R", "R/sc/sc_abundance_design.R",
    "R/sc/sc_abundance_milo.R", "R/sc/sc_abundance_milo_views.R"
  ) %in% APP_DAEMON_SOURCE_FILES))

  # Aucun chemin fantome : un fichier absent est silencieusement ignore par le
  # preload du daemon, donc invisible en production.
  missing <- APP_DAEMON_SOURCE_FILES[
    !file.exists(file.path(ts_project_root(), APP_DAEMON_SOURCE_FILES))
  ]
  expect_identical(missing, character(0))

  # scCODA reste EXCLU (reticulate/TensorFlow : un interpreteur Python par daemon).
  expect_false(any(grepl("sccoda", APP_DAEMON_SOURCE_FILES, ignore.case = TRUE)))
})

test_that("le module Milo passe par le pool applicatif partage (4E-4)", {
  src <- paste(readLines(file.path(ts_project_root(), "modules/sc/mod_sc_da_milo.R")),
               collapse = "\n")
  expect_match(src, "run_job(", fixed = TRUE)
  expect_match(src, "async      = TRUE", fixed = TRUE)
  expect_match(src, "ensure_app_daemons(", fixed = TRUE)
  expect_match(src, "TS_DA_MILO_TIMEOUT_MS", fixed = TRUE)
  # Le repli synchrone doit etre DECLARE (statut + notification), jamais muet.
  expect_match(src, "daemons_set()", fixed = TRUE)

  # scCODA n'est PAS cable sur l'async (contrainte actee, STATUS.md 2am.1).
  sccoda <- paste(readLines(file.path(ts_project_root(), "modules/sc/mod_sc_da_sccoda.R")),
                  collapse = "\n")
  expect_false(grepl("run_job(", sccoda, fixed = TRUE))
  expect_false(grepl("ensure_app_daemons(", sccoda, fixed = TRUE))
})

test_that("le plafond du job DA est un parametre declare", {
  defaults <- paste(readLines(file.path(ts_project_root(), "config/defaults.R"),
                              warn = FALSE),
                    collapse = "\n")
  expect_match(defaults, "TS_DA_MILO_TIMEOUT_MS", fixed = TRUE)
  expect_true(is.numeric(TS_DA_MILO_TIMEOUT_MS))
  expect_true(TS_DA_MILO_TIMEOUT_MS > 0)
})

# Le preload est resolu par le marqueur app.R, PAS par getwd() : sous
# testthat::test_file() le repertoire courant est celui des tests, et un
# preload resolu par getwd() echouait SILENCIEUSEMENT (file.exists() faux dans
# chaque daemon -> message() invisible -> pool « degrade »).
test_that("le repertoire de base du pool est resolu par le marqueur app.R", {
  expect_true(file.exists(file.path(.resolve_app_base_dir(), "app.R")))
  expect_identical(.resolve_app_base_dir(),
                   normalizePath(ts_project_root(), winslash = "/"))

  sub <- file.path(ts_project_root(), "tests", "testthat")
  old <- setwd(sub)
  on.exit(setwd(old), add = TRUE)
  expect_identical(.resolve_app_base_dir(),
                   normalizePath(ts_project_root(), winslash = "/"))
})

# ── Garde decisive : le daemon doit rendre EXACTEMENT le resultat synchrone ──
test_that("Milo rend le meme resultat dans le pool applicatif qu'en synchrone", {
  skip_if_not_installed("mirai")
  skip_if_not_installed("miloR")
  skip_on_cran()
  on.exit(tryCatch(mirai::daemons(0), error = function(e) NULL), add = TRUE)

  obj <- .milo_seurat_obj()
  des <- .milo_design(obj)

  sync_res <- run_milo_da(
    seurat_obj = obj, da_design_result = des, reduction = "pca",
    target_condition = "B", reference_condition = "A"
  )
  # ⚠️ M15 (2026-10-17) : ce statut etait epingle a `"valid"` parce que les
  # warnings de miloR etaient PERDUS (le gestionnaire ecrivait `<-`, donc le
  # vecteur de la fonction appelante ne recevait rien). Depuis le correctif ils
  # remontent, et le statut suit : `valid_with_warnings`.
  #
  # MESURE, et c'est pourquoi la valeur exacte n'est PAS epinglee ici : le seul
  # avertissement emis sur cette fixture est la depreciation Matrix
  # (`as(<dgTMatrix>, "dgCMatrix")`), et R ne l'emet qu'UNE FOIS PAR SESSION
  # (mesure : 2 appels consecutifs -> 1er `valid_with_warnings`, 2e `valid`).
  # Le statut depend donc de l'historique de session, pas du calcul. Epingler
  # une valeur ici rendrait le test dependant de l'ordre d'execution.
  # Ce que CE test doit garantir, c'est son titre : que le pool rende le MEME
  # resultat que le synchrone. C'est asserte juste en dessous.
  expect_true(sync_res$status %in% c("valid", "valid_with_warnings"))

  expect_true(isTRUE(ensure_app_daemons(n_daemons = 2L)))

  async_res <- run_job(
    run_milo_da,
    seurat_obj = obj, da_design_result = des, reduction = "pca",
    target_condition = "B", reference_condition = "A",
    async = TRUE, timeout_ms = 300000
  )

  # Le resultat canonique doit etre identique — statut, table DA et graine.
  expect_identical(async_res$status, sync_res$status)
  expect_identical(async_res$DA_table, sync_res$DA_table)
  expect_identical(async_res$provenance$seed, sync_res$provenance$seed)
})

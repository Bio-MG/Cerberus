# =============================================================================
# test-core-jobs.R — wrapper fin d'execution R/core/jobs.R (CHRYSALIS 2D)
# =============================================================================
# Couvre : chemin sync (historique), callbacks on_progress/on_error, propa-
# gation d'erreur a l'identique quand on_error=NULL, repli synchrone si
# 'mirai' absent ou pool inactif, et le chemin async DELEGUE quand un pool
# mirai est actif (pattern spatial, cf. regle 9 AGENTS.md).

source_project_file("tools/pkg_skeleton/R/jobs.R")

test_that("run_job sync path returns the function result", {
  expect_equal(run_job(function(x) x * 2, 21), 42)
  expect_equal(run_job(sum, 1, 2, 3), 6)
  expect_identical(run_job(function() "ok"), "ok")
})

test_that("run_job sync calls on_progress at start and completion", {
  msgs <- character(0)
  res <- run_job(function() 1, on_progress = function(m) msgs <<- c(msgs, m))
  expect_equal(res, 1)
  expect_identical(msgs, c("demarrage", "termine"))
})

test_that("run_job with on_error absorbs the error and returns NULL", {
  got <- NULL
  res <- run_job(function() stop("boom"), on_error = function(e) got <<- e)
  expect_null(res)
  expect_s3_class(got, "condition")
  expect_match(conditionMessage(got), "boom")
})

test_that("run_job WITHOUT on_error re-raises the error (historical behavior)", {
  expect_error(run_job(function() stop("boom")), "boom")
})

test_that("run_job validates that fn is a function (French)", {
  expect_error(run_job(42), "fonction")
})

test_that("run_job warns and falls back to sync when no mirai pool is active", {
  skip_if_not_installed("mirai")
  on.exit(tryCatch(mirai::daemons(0), error = function(e) NULL), add = TRUE)
  # Garantir un pool INACTIF pour ce test
  if (mirai::daemons_set()) mirai::daemons(0)
  expect_warning(res <- run_job(function(x) x + 1, 1, async = TRUE), "mirai")
  expect_equal(res, 2)
})

test_that("run_job async delegates to the active mirai pool (blocking collect)", {
  skip_if_not_installed("mirai")
  skip_on_cran()
  on.exit(tryCatch(mirai::daemons(0), error = function(e) NULL), add = TRUE)
  if (!mirai::daemons_set()) mirai::daemons(2)

  msgs <- character(0)
  res <- run_job(
    function(x, y) x + y, 20, 22,
    async = TRUE, timeout_ms = 30000,
    on_progress = function(m) msgs <<- c(msgs, m)
  )
  expect_equal(res, 42)
  expect_true("soumission mirai" %in% msgs)
  expect_true("termine" %in% msgs)
})

test_that("run_job async routes daemon errors through on_error", {
  skip_if_not_installed("mirai")
  skip_on_cran()
  on.exit(tryCatch(mirai::daemons(0), error = function(e) NULL), add = TRUE)
  if (!mirai::daemons_set()) mirai::daemons(2)

  got <- NULL
  res <- run_job(function() stop("echec daemon"), async = TRUE,
                 timeout_ms = 30000, on_error = function(e) got <<- e)
  expect_null(res)
  expect_match(conditionMessage(got), "echec daemon")
})

# ── 4E-4 : determinisme du chemin async ─────────────────────────────────────
# Cause racine MESUREE (2026-09-16) : un daemon mirai tourne en
# "L'Ecuyer-CMRG" alors que le processus principal tourne en
# "Mersenne-Twister". Sans restauration, `set.seed(s)` dans le job ne produit
# PAS la meme suite qu'en synchrone — un calcul stochastique (Milo /
# makeNhoods) rendait alors 19 voisinages au lieu de 20, avec la somme des
# logFC de signe INVERSE, tout en declarant le meme `seed` en provenance et le
# meme statut `valid` : invisible.
test_that("run_job async reproduces the caller's RNG stream (RNGkind trap)", {
  skip_if_not_installed("mirai")
  skip_on_cran()
  on.exit(tryCatch(mirai::daemons(0), error = function(e) NULL), add = TRUE)
  if (!mirai::daemons_set()) mirai::daemons(2)

  draw <- function() { set.seed(1234L); stats::runif(3) }
  sync_val <- run_job(draw)
  expect_identical(run_job(draw, async = TRUE, timeout_ms = 60000), sync_val)

  # Le daemon n'a effectivement pas le RNGkind de l'appelant : c'est la cause.
  # La sonde doit desactiver la restauration (rng_kind = NULL), sinon elle
  # mesure le correctif et non le defaut.
  kind_daemon <- run_job(function() RNGkind(), async = TRUE, timeout_ms = 60000,
                         rng_kind = NULL)
  skip_if(identical(as.character(kind_daemon), as.character(RNGkind())),
          "le daemon partage deja le RNGkind de l'appelant")

  # ... et c'est bien ce qui rendrait le job non reproductible sans correctif.
  expect_false(identical(
    run_job(draw, async = TRUE, timeout_ms = 60000, rng_kind = NULL),
    sync_val
  ))
})

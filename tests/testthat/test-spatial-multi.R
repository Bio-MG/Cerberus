# =============================================================================
# test-spatial-multi.R — tests for R/spatial/spatial_multi.R
# =============================================================================
# 21ᵉ incrément de la dette de conventions (2026-09-18).
# Créé AVANT la conversion (règle 5 : test d'abord, rouge d'abord) ⇒ ce
# fichier doit ÉCHOUER sur l'assertion de classe tant que le `stop()` ne porte
# pas `class = "spatial_multi_error"`.
#
# 🟢 Lot choisi par RENDEMENT × PREUVE (§2bx.6), pas par habitude :
#   - rendement : `R/` paie **C10 + C9** ⇒ **−2** (les lots de `modules/` ne
#     paient que C10, et leurs 14 sites restants sont tous en serveurs
#     réactifs, donc inobservables — §2bw.6, §2ce.5) ;
#   - preuve : le site est une **garde d'argument en TÊTE de fonction**
#     (`if (length(sketch_paths) < 2)`), sur une **fonction pure** de
#     premier niveau (C2 interdit Shiny dans `R/`). Aucun paquet, aucun mock,
#     aucune fixture, aucun aléa ⇒ le site le moins cher du chantier.
#
# ⚠️ Ce fichier est NOUVEAU, il n'existe aucun test hérité (§2bz.2, §2cb.2) :
# 3 fichiers MENTIONNENT `spatial_multi` (`test-bulk-multi-compare-contract-
# freeze.R`, `test-plot-theme.R`, `test-shinytest2-spatial.R`) mais **aucun ne
# le couvre seul** ⇒ pas de `git mv` gratuit (§2cc.1, §2cd.4 : le critère est
# « existe-t-il un test qui ne couvre QUE lui ? », pas « est-il mentionné ? »).
#
# ⚠️ `R/spatial/spatial_multi.R` n'est PAS auto-suffisant (§2ce.6) : son
# prologue définit `.log()` qui appelle `write_mirai_log()` (défini dans
# `spatial_async.R`) ⇒ ce fichier doit être sourcé AVANT, sinon l'échec rouge
# ressemble à une erreur de conversion. Vérifié : `app.R:125` le source bien
# (pas de P0 de sourçage).
# =============================================================================

source_project_file("R/spatial/spatial_async.R")
source_project_file("R/spatial/spatial_multi.R")

.sm_err <- function(expr) {
  tryCatch({ expr; NULL },
           error = function(e) list(msg = conditionMessage(e), class = class(e)))
}
.sm_expect <- function(e, msg) {
  expect_identical(e$msg, msg)
  expect_true("spatial_multi_error" %in% e$class)
}

.MSG_2_SAMPLES <- "Selectionnez au moins 2 echantillons pour l'integration multi-coupes."

# ---------------------------------------------------------------------------
# integrate_spatial_sketches() — garde d'argument, en tête de fonction
# ---------------------------------------------------------------------------
test_that("integrate_spatial_sketches : 1 seul echantillon (68)", {
  # 🟢 Message à UN SEUL argument ⇒ PAS de `paste0()` à ajouter (§2bw.4) : la
  # règle C16 ne s'applique qu'aux `stop()` multi-arguments (§2bn).
  # `expect_identical` sur le message ENTIER, pas un préfixe : une assertion
  # de préfixe ne verrait pas une troncature (§2bx.3).
  .sm_expect(.sm_err(integrate_spatial_sketches(c(slice1 = "x.rds"))),
             .MSG_2_SAMPLES)
})

test_that("integrate_spatial_sketches : 0 echantillon (borne exacte < 2)", {
  .sm_expect(.sm_err(integrate_spatial_sketches(character(0))), .MSG_2_SAMPLES)
})

# ---------------------------------------------------------------------------
# Témoin NOMINAL — la garde ne doit PAS tirer au-delà de sa borne
# ---------------------------------------------------------------------------
# Contrôle de validité indispensable : sans lui, un `stop()` déplacé au mauvais
# endroit (ou une garde trop large) passerait les deux tests ci-dessus. On
# vérifie donc que `length >= 2` FRANCHIT la garde — l'échec qui suit vient de
# `readRDS()` sur un chemin absent, pas de notre classe.
test_that("integrate_spatial_sketches : 2 chemins absents franchissent la garde", {
  p <- c(a = file.path(tempdir(), "sm_absent_a.rds"),
         b = file.path(tempdir(), "sm_absent_b.rds"))
  e <- .sm_err(suppressWarnings(integrate_spatial_sketches(p)))
  expect_false(is.null(e))                                  # une erreur est bien levée
  expect_false("spatial_multi_error" %in% e$class)          # …mais PAS la garde
})

# ---------------------------------------------------------------------------
# Verrou source : spatial_multi.R ne doit plus contribuer aucun C10
# ---------------------------------------------------------------------------
# Ancré sur 0, jamais sur un compte (§PITFALLS) : stable à chaque incrément.
test_that("spatial_multi.R ne contribue aucun signalement C10", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools/check_conventions.R"), envir = e)
  before <- length(e$.REPORT$warns)
  e$check_c10_error_style(file.path(ts_project_root(), "R/spatial/spatial_multi.R"))
  w <- e$.REPORT$warns
  n <- length(w) - before
  if (n > 0L) {
    lignes <- vapply(w[(before + 1L):length(w)], function(x) x$line, integer(1))
    cat("Sites C10 restants dans spatial_multi.R :",
        paste(sort(lignes), collapse = ", "), "\n")
  }
  expect_equal(n, 0L)
})

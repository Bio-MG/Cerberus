# =============================================================================
# test-spatial-niche.R — tests for R/spatial/spatial_niche.R
# =============================================================================
# 19ᵉ incrément de la dette de conventions (2026-09-17).
# Créé AVANT la conversion (règle 5 : test d'abord, rouge d'abord) ⇒ ce
# fichier doit ÉCHOUER sur les assertions de classe tant que les `stop()` ne
# portent pas `class = "spatial_niche_error"`.
#
# ⚠️ 2ᵉ lot d'affilée **sans aucun test hérité** (§2cc.1) : aucun fichier de
# test ne porte ce nom, et les deux qui mentionnent `spatial_niche`
# (`test-plot-theme.R`, `test-shinytest2-spatial.R`) ne le couvrent pas
# exclusivement ⇒ aucun `git mv` gratuit n'est possible (PITFALLS #42).
#
# 🟢 Lot choisi par le prédicteur de §2cc.2 : `spatial_niche.R` n'a **aucune**
# fonction qui AVALE les erreurs (son unique `tryCatch`, ligne 117, **relance**
# via `stop()` dans le gestionnaire) — à la différence de
# `spatial_deconv_tasks.R`, une longue fonction « corps de pipeline » dont les
# sites sont profondément enfouis. **Les 5 sites sont prouvés à l'exécution.**
# =============================================================================

source_project_file("R/spatial/spatial_niche.R")

.nch_err <- function(expr) {
  tryCatch({ expr; NULL },
           error = function(e) list(msg = conditionMessage(e), class = class(e)))
}
.nch_expect <- function(e, msg) {
  expect_identical(e$msg, msg)
  expect_true("spatial_niche_error" %in% e$class)
}
# 🟢 Message à QUEUE VOLATILE (§2by.3) : le site 120 interpole
# `conditionMessage(e)` de `stats::kmeans()`, texte interne à R. On assère donc
# le PRÉFIXE **plus une longueur strictement supérieure** : la longueur détecte
# la troncature (C16) là où le contenu ne le peut pas.
.nch_expect_prefix <- function(e, prefix) {
  expect_true(grepl(prefix, e$msg, fixed = TRUE))
  expect_true(nchar(e$msg) > nchar(prefix))
  expect_true("spatial_niche_error" %in% e$class)
}
# Garde `requireNamespace()` atteignable même si le paquet EST installé
# (§2bz.3) — RANN **est** installé ici.
.nch_no_pkg <- function(fun) {
  e <- new.env(parent = globalenv())
  e$requireNamespace <- function(package, ...) FALSE
  f <- get(fun, envir = globalenv())
  environment(f) <- e
  f
}
# Coordonnées DÉTERMINISTES (pas de runif) : le test ne doit pas flamber.
.nch_coords <- function(n) {
  data.frame(id = paste0("c", seq_len(n)),
             x = seq_len(n) + 0.5, y = seq_len(n) + 0.25,
             stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# compute_spatial_niches()
# ---------------------------------------------------------------------------
test_that("compute_spatial_niches : RANN absent (73)", {
  .nch_expect(
    .nch_err(.nch_no_pkg("compute_spatial_niches")(
      .nch_coords(20), setNames(rep("A", 20), paste0("c", 1:20)))),
    "Package 'RANN' requis (install.packages('RANN')).")
})

test_that("compute_spatial_niches : < 10 elements communs (80 — MULTI-ARGUMENTS)", {
  # ⚠️ Deux littéraux accolés sur DEUX lignes : sans `paste0()`, il ne resterait
  # que le premier ⇒ l'assertion porte sur la concaténation.
  .nch_expect(
    .nch_err(compute_spatial_niches(
      .nch_coords(4), setNames(rep(c("A", "B"), 2), paste0("c", 1:4)))),
    paste0("Moins de 10 elements communs entre les coordonnees et le regroupement choisi ",
           "(cluster ou deconvolution) — recalculez ce regroupement si necessaire."))
})

test_that("compute_spatial_niches : une seule categorie (89 — MULTI-ARGUMENTS)", {
  .nch_expect(
    .nch_err(compute_spatial_niches(
      .nch_coords(20), setNames(rep("A", 20), paste0("c", 1:20)))),
    paste0("Le regroupement choisi n'a qu'une seule categorie — impossible de calculer ",
           "une composition de voisinage informative."))
})

test_that("compute_spatial_niches : k-means echoue (120 — 3 ARGUMENTS)", {
  # 🟢 Le seul `tryCatch()` du fichier (ligne 117) RELANCE l'erreur au lieu de
  # l'avaler ⇒ le site EST joignable. Pour faire échouer k-means sans dépendre
  # d'un message interne, on CONFOND les 12 points : les lignes de composition
  # deviennent identiques ⇒ « more cluster centers than distinct data points ».
  cd <- data.frame(id = paste0("c", 1:12), x = rep(0, 12), y = rep(0, 12),
                   stringsAsFactors = FALSE)
  labs <- setNames(rep(c("A", "B"), 6), paste0("c", 1:12))
  .nch_expect_prefix(.nch_err(compute_spatial_niches(cd, labs)),
                     "k-means a echoue sur la composition de voisinage (")
})

# ---------------------------------------------------------------------------
# dominant_group_labels()
# ---------------------------------------------------------------------------
test_that("dominant_group_labels : aucune colonne de type (156)", {
  .nch_expect(
    .nch_err(dominant_group_labels(data.frame(id = "c1", stringsAsFactors = FALSE))),
    "Aucune colonne de type cellulaire dans deconv_props.")
})

# ---------------------------------------------------------------------------
# Garde-fou du mock de dépendance (§2bz.3) : il ne doit PAS fuiter
# ---------------------------------------------------------------------------
test_that("le mock de dependance ne fuit pas dans globalenv", {
  invisible(tryCatch(.nch_no_pkg("compute_spatial_niches")(
    .nch_coords(20), setNames(rep("A", 20), paste0("c", 1:20))),
    error = function(e) NULL))
  expect_false(exists("requireNamespace", envir = globalenv(), inherits = FALSE))
  expect_true(requireNamespace("stats", quietly = TRUE))
})

# ---------------------------------------------------------------------------
# Verrou source : spatial_niche.R ne doit plus contribuer aucun C10
# ---------------------------------------------------------------------------
test_that("spatial_niche.R ne contribue aucun signalement C10", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools/check_conventions.R"), envir = e)
  before <- length(e$.REPORT$warns)
  e$check_c10_error_style(file.path(ts_project_root(), "R/spatial/spatial_niche.R"))
  w <- e$.REPORT$warns
  n <- length(w) - before
  if (n > 0L) {
    lignes <- vapply(w[(before + 1L):length(w)], function(x) x$line, integer(1))
    cat("Sites C10 restants dans spatial_niche.R :", paste(sort(lignes), collapse = ", "), "\n")
  }
  expect_equal(n, 0L)
})

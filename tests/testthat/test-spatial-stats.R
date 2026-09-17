# =============================================================================
# test-spatial-stats.R — tests for R/spatial/spatial_stats.R
# =============================================================================
# ⚠️ RENOMMÉ le 2026-09-17 (15ᵉ incrément de la dette de conventions).
# L'ancien nom `test-utils_spatial_stats.R` était un **POINTEUR PÉRIMÉ** : le
# fichier testé s'appelait `R/utils_spatial_stats.R` avant son déplacement dans
# `R/spatial/`. Conséquence **mesurable** : C9 ne le voyait pas (il cherche
# `test-spatial_stats*` / `test-spatial-stats*`) et signalait le fichier comme
# « sans test éponyme » **alors qu'il portait déjà 15 tests**.
# ⇒ Le renommage fait baisser C9 **sans écrire une seule ligne de test**, et
# évite un second fichier qui aurait fait doublon (règle 3 : étendre).
# =============================================================================
# compute_composition_differential() is pure base-R (stats::chisq.test only)
# and is fully exercised here with exact-value assertions.
#
# spatial_neighborhood_enrichment(), compute_getis_ord_hotspots() and
# ripley_k_random_labeling() all hard-require the 'RANN' package (no
# fallback — see each function's own requireNamespace() guard) and the
# permutation-based ones (enrichment, Ripley's K) have no internal
# set.seed(), so exact numeric values are not meaningfully reproducible
# across R versions/BLAS. They're skipped gracefully when RANN is absent
# and, when present, asserted on STRUCTURE + a directionally-obvious signal
# (seeded via set.seed() at the call site) rather than exact floating point
# values — robust across R/BLAS versions while still catching a broken
# formula or a shape/sign regression.
# =============================================================================

source_project_file("R/spatial/spatial_stats.R")

# --- 15ᵉ incrément : assertions sur le message ENTIER + la CLASSE -----------
# ⚠️ Tous les `stop()` de ce fichier sont à UN SEUL argument ⇒ aucune troncature
# possible (§2bn) et aucun `paste0()` à ajouter — à la différence de §2bx/§2by
# où 5 sites étaient multi-arguments. Les assertions portent donc sur le texte
# complet ET sur la classe, ce que les assertions d'origine (préfixes seuls)
# ne faisaient pas.
.sp_err <- function(expr) {
  tryCatch({ expr; NULL },
           error = function(e) list(msg = conditionMessage(e), class = class(e)))
}
.sp_expect <- function(e, msg) {
  expect_identical(e$msg, msg)
  expect_true("spatial_stats_error" %in% e$class)
}

# --- gardes `requireNamespace()` : atteignables MEME quand le paquet existe --
# ⚠️ `testthat::with_mocked_bindings(..., .env = globalenv())` ECHOUE ici :
#    « No packages loaded with pkgload » — il exige un .env adosse a un paquet,
#    ce que globalenv() n'est pas. On copie donc la fonction dans un
#    ENVIRONNEMENT ENFANT de globalenv ou vit le mock : la copie voit d'abord
#    `e` (le mock), puis globalenv, puis base. globalenv n'est JAMAIS modifie
#    => le harness testthat n'est pas menace. Mesure : apres l'appel,
#    `exists("requireNamespace", globalenv(), inherits = FALSE)` est toujours
#    FALSE et RANN reste joignable.
.sp_no_pkg <- function(fun) {
  e <- new.env(parent = globalenv())
  e$requireNamespace <- function(package, ...) FALSE
  f <- get(fun, envir = globalenv())
  environment(f) <- e
  f
}


# ---------------------------------------------------------------------------
# compute_composition_differential() — pure base R, no skip needed
# ---------------------------------------------------------------------------
test_that("compute_composition_differential returns the expected list shape", {
  set.seed(1)
  emb <- data.frame(
    dataset = rep(c("A", "B"), each = 40),
    cluster = c(sample(c("c1","c2"), 40, replace = TRUE, prob = c(0.8, 0.2)),
               sample(c("c1","c2"), 40, replace = TRUE, prob = c(0.2, 0.8)))
  )
  res <- compute_composition_differential(emb)
  expect_true(all(c("contingency","chisq","residuals","proportions") %in% names(res)))
  expect_true(all(c("statistic","p_value","method") %in% names(res$chisq)))
  expect_identical(colnames(res$residuals), c("dataset","cluster","std_resid"))
  expect_identical(colnames(res$proportions), c("dataset","cluster","proportion","n"))
})

test_that("compute_composition_differential detects an obviously different composition", {
  # Dataset A is 100% c1, dataset B is 100% c2 -> maximally different,
  # chi-squared statistic should be large and p-value tiny.
  emb <- data.frame(
    dataset = rep(c("A", "B"), each = 50),
    cluster = c(rep("c1", 50), rep("c2", 50))
  )
  res <- compute_composition_differential(emb)
  expect_gt(res$chisq$statistic, 50)
  expect_lt(res$chisq$p_value, 0.001)
})

test_that("compute_composition_differential proportions sum to 1 within each dataset", {
  emb <- data.frame(
    dataset = rep(c("A","B","C"), each = 30),
    cluster = sample(c("x","y","z"), 90, replace = TRUE)
  )
  res <- compute_composition_differential(emb)
  totals <- stats::aggregate(proportion ~ dataset, data = res$proportions, FUN = sum)
  expect_equal(totals$proportion, rep(1, nrow(totals)), tolerance = 1e-9)
})

test_that("compute_composition_differential errors with fewer than 2 datasets", {
  emb <- data.frame(dataset = rep("A", 10), cluster = sample(c("x","y"), 10, replace = TRUE))
  .sp_expect(.sp_err(compute_composition_differential(emb)),
             "Au moins 2 echantillons requis pour un test de composition differentielle.")
})

test_that("compute_composition_differential errors when required columns are missing", {
  emb <- data.frame(foo = 1:5, bar = 1:5)
  .sp_expect(.sp_err(compute_composition_differential(emb)),
             "embeddings doit contenir les colonnes 'dataset' et 'cluster'.")
})

test_that("compute_composition_differential falls back to simulated p-value for sparse tables", {
  # Tiny counts -> expected cell counts < 5 -> must use simulate.p.value=TRUE
  emb <- data.frame(dataset = c("A","A","B","B"), cluster = c("x","y","x","y"))
  res <- compute_composition_differential(emb)
  expect_match(res$chisq$method, "simule")
})

# ---------------------------------------------------------------------------
# compute_getis_ord_hotspots() — needs RANN
# ---------------------------------------------------------------------------
test_that("compute_getis_ord_hotspots flags a synthetic hot region as significant", {
  skip_if_not_installed("RANN")
  set.seed(42)
  n <- 200
  coords <- data.frame(id = paste0("s", seq_len(n)),
                       x = runif(n, 0, 100), y = runif(n, 0, 100))
  # "Hot" cluster: a tight group of high values in one corner; background low.
  is_hot <- coords$x < 15 & coords$y < 15
  values <- ifelse(is_hot, rnorm(n, mean = 50, sd = 2), rnorm(n, mean = 0, sd = 2))
  names(values) <- coords$id

  res <- compute_getis_ord_hotspots(coords, values, k_neighbors = 10)
  expect_identical(colnames(res), c("id","value","gi_star","p_value","hotspot"))
  expect_equal(nrow(res), n)
  # Points inside the hot region should have systematically higher Gi* than
  # points outside it — checks the sign/direction of the statistic, not an
  # exact numeric value.
  mean_gi_hot  <- mean(res$gi_star[is_hot])
  mean_gi_cold <- mean(res$gi_star[!is_hot])
  expect_gt(mean_gi_hot, mean_gi_cold)
  expect_true(any(res$hotspot[is_hot] == "Hotspot (chaud)"))
})

test_that("compute_getis_ord_hotspots errors with fewer than 10 COMMON ids (112)", {
  skip_if_not_installed("RANN")
  coords <- data.frame(id = paste0("s", 1:5), x = 1:5, y = 1:5)
  values <- stats::setNames(1:5, coords$id)
  .sp_expect(.sp_err(compute_getis_ord_hotspots(coords, values)),
             "Moins de 10 elements communs entre coordonnees et valeurs.")
})

test_that("compute_getis_ord_hotspots errors on a zero-variance metric", {
  skip_if_not_installed("RANN")
  coords <- data.frame(id = paste0("s", 1:20), x = runif(20), y = runif(20))
  values <- stats::setNames(rep(5, 20), coords$id)   # constant -> zero variance
  .sp_expect(.sp_err(compute_getis_ord_hotspots(coords, values)),
             "Variance nulle pour cette metrique -- Getis-Ord non calculable.")
})

# ---------------------------------------------------------------------------
# spatial_neighborhood_enrichment() — needs RANN, permutation-based
# ---------------------------------------------------------------------------
test_that("spatial_neighborhood_enrichment detects self-attraction of a spatially segregated group", {
  skip_if_not_installed("RANN")
  set.seed(7)
  n <- 240
  # Two tight, spatially SEPARATE blobs, one per group -> strong self-
  # attraction (A near A, B near B), strong mutual exclusion (A near B rare).
  coords <- data.frame(
    id = paste0("s", seq_len(n)),
    x = c(rnorm(n / 2, mean = 0, sd = 3), rnorm(n / 2, mean = 100, sd = 3)),
    y = c(rnorm(n / 2, mean = 0, sd = 3), rnorm(n / 2, mean = 100, sd = 3))
  )
  labels <- stats::setNames(rep(c("A", "B"), each = n / 2), coords$id)

  res <- spatial_neighborhood_enrichment(coords, labels, k_neighbors = 10, n_perm = 50)
  expect_true(all(c("enrichment","matrix","levels","k_neighbors","n_perm") %in% names(res)))
  z_AA <- res$enrichment$z_score[res$enrichment$from == "A" & res$enrichment$to == "A"]
  z_AB <- res$enrichment$z_score[res$enrichment$from == "A" & res$enrichment$to == "B"]
  expect_gt(z_AA, 0)     # A next to A: enriched vs random labeling
  expect_lt(z_AB, 0)     # A next to B: depleted vs random labeling
})

test_that("spatial_neighborhood_enrichment errors with a single-level grouping", {
  skip_if_not_installed("RANN")
  coords <- data.frame(id = paste0("s", 1:20), x = runif(20), y = runif(20))
  labels <- stats::setNames(rep("only_one", 20), coords$id)
  .sp_expect(.sp_err(spatial_neighborhood_enrichment(coords, labels)),
             "Le regroupement choisi n'a qu'une seule categorie.")
})

# ---------------------------------------------------------------------------
# ripley_k_random_labeling() — needs RANN, permutation-based
# ---------------------------------------------------------------------------
test_that("ripley_k_random_labeling detects aggregation of a tightly clustered target label", {
  skip_if_not_installed("RANN")
  set.seed(11)
  n_bg <- 150; n_target <- 40
  # Target points tightly packed in a small corner; background spread widely.
  coords <- data.frame(
    id = paste0("s", seq_len(n_bg + n_target)),
    x = c(runif(n_bg, 0, 100), runif(n_target, 0, 8)),
    y = c(runif(n_bg, 0, 100), runif(n_target, 0, 8))
  )
  labels <- stats::setNames(c(rep("bg", n_bg), rep("tight", n_target)), coords$id)

  res <- ripley_k_random_labeling(coords, labels, target_level = "tight", n_perm = 49)
  expect_identical(res$target_level, "tight")
  expect_equal(res$n_target, n_target)
  expect_false(res$subsampled)
  # At the smallest radius, a tightly packed target should show clear
  # spatial aggregation relative to the random-labeling null envelope.
  expect_gt(res$curve$k_observed[1], res$curve$k_perm_hi[1])
})

test_that("ripley_k_random_labeling errors when the target level doesn't exist", {
  skip_if_not_installed("RANN")
  coords <- data.frame(id = paste0("s", 1:25), x = runif(25), y = runif(25))
  labels <- stats::setNames(rep("A", 25), coords$id)
  .sp_expect(.sp_err(ripley_k_random_labeling(coords, labels, target_level = "not_there")),
             "Niveau cible 'not_there' introuvable.")
})

test_that("ripley_k_random_labeling errors with fewer than 10 target points", {
  skip_if_not_installed("RANN")
  coords <- data.frame(id = paste0("s", 1:25), x = runif(25), y = runif(25))
  labels <- stats::setNames(c(rep("A", 5), rep("B", 20)), coords$id)
  .sp_expect(.sp_err(ripley_k_random_labeling(coords, labels, target_level = "A")),
             "Moins de 10 elements dans le groupe cible -- test non fiable.")
})

# ---------------------------------------------------------------------------
# Verrou source — le fichier ne doit plus contribuer UN SEUL signalement C10.
# La détection est celle du GARDE lui-même (jamais réimplémentée). Il couvre
# aussi les 3 sites injoignables (49, 110, 305 : RANN est INSTALLÉ, donc les
# branches « paquet manquant » ne s'exécutent jamais).
# ---------------------------------------------------------------------------
test_that("spatial_stats.R ne contribue aucun signalement C10 (verrou source)", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools/check_conventions.R"), envir = e)
  before <- length(e$.REPORT$warns)
  e$check_c10_error_style(file.path(ts_project_root(), "R/spatial/spatial_stats.R"))
  w <- e$.REPORT$warns
  n <- length(w) - before
  if (n > 0L) {
    lignes <- vapply(w[(before + 1L):length(w)], function(x) x$line, integer(1))
    cat("Sites C10 restants dans spatial_stats.R :",
        paste(sort(lignes), collapse = ", "), "\n")
  }
  expect_equal(n, 0L)
})

# --- sites non couverts avant le 15ᵉ incrément ------------------------------

test_that("spatial_neighborhood_enrichment : moins de 10 ids communs (54)", {
  # ⚠️ skip RANN : le garde 49 précède ce site, sans RANN c'est LUI qui tirerait.
  skip_if_not_installed("RANN")
  coords <- data.frame(id = paste0("p", 1:12), x = runif(12), y = runif(12))
  labels <- stats::setNames(rep(c("A", "B"), length.out = 12), paste0("q", 1:12))
  .sp_expect(.sp_err(spatial_neighborhood_enrichment(coords, labels)),
             "Moins de 10 elements communs entre coordonnees et regroupement.")
})

test_that("compute_getis_ord_hotspots : moins de 10 points VALIDES (118)", {
  skip_if_not_installed("RANN")
  # ⚠️ 12 ids communs (pour franchir 112) mais 4 valeurs non finies => 8 < 10.
  coords <- data.frame(id = paste0("p", 1:12), x = runif(12), y = runif(12))
  values <- stats::setNames(c(runif(8), rep(NA_real_, 4)), paste0("p", 1:12))
  .sp_expect(.sp_err(compute_getis_ord_hotspots(coords, values)),
             "Moins de 10 elements valides (coordonnees + valeur finie).")
})

test_that("ripley_k_random_labeling : moins de 20 ids communs (224)", {
  coords <- data.frame(id = paste0("p", 1:25), x = runif(25), y = runif(25))
  labels <- stats::setNames(rep("A", 25), paste0("q", 1:25))
  .sp_expect(.sp_err(ripley_k_random_labeling(coords, labels, target_level = "A")),
             "Moins de 20 elements communs entre coordonnees et regroupement.")
})

test_that("ripley_k_random_labeling : etendue spatiale nulle (247)", {
  # ⚠️ Il faut franchir 224 (>= 20 communs) ET 231 (>= 10 cibles) avant
  # d'atteindre 247 — sinon c'est 231 qui tire (mesuré).
  coords <- data.frame(id = paste0("p", 1:25), x = rep(1, 25), y = runif(25))
  labels <- stats::setNames(rep(c("A", "B"), c(10, 15)), paste0("p", 1:25))
  .sp_expect(.sp_err(ripley_k_random_labeling(coords, labels, target_level = "B")),
             "Etendue spatiale nulle -- test impossible.")
})

test_that("spatial_lr_score : moins de 10 ids communs (309)", {
  skip_if_not_installed("RANN")
  coords <- data.frame(id = paste0("p", 1:12), x = runif(12), y = runif(12))
  expr <- matrix(runif(30), nrow = 10,
                 dimnames = list(paste0("g", 1:10), paste0("p", 1:3)))
  lr <- data.frame(ligand = "ZZZ", receptor = "YYY", stringsAsFactors = FALSE)
  .sp_expect(.sp_err(spatial_lr_score(coords, expr, lr)),
             "Moins de 10 elements communs entre coordonnees et matrice d'expression.")
})

test_that("spatial_lr_score : aucune paire ligand-recepteur exploitable (320)", {
  skip_if_not_installed("RANN")
  coords <- data.frame(id = paste0("p", 1:12), x = runif(12), y = runif(12))
  expr <- matrix(runif(120), nrow = 10,
                 dimnames = list(paste0("g", 1:10), paste0("p", 1:12)))
  lr <- data.frame(ligand = "ZZZ", receptor = "YYY", stringsAsFactors = FALSE)
  .sp_expect(.sp_err(spatial_lr_score(coords, expr, lr)),
             "Aucune paire ligand-recepteur exploitable (genes absents).")
})

# --- gardes d'absence de RANN (49, 110, 305) --------------------------------
# ⚠️ RANN EST installe dans ce projet : sans mock ces 3 sites sont INATTEIGNABLES
# (le flux nominal passe). `.sp_no_pkg()` les rend prouvables => 16/16 sites.

test_that("spatial_neighborhood_enrichment : RANN absent (49)", {
  coords <- data.frame(id = paste0("p", 1:12), x = runif(12), y = runif(12))
  labels <- stats::setNames(rep(c("A", "B"), length.out = 12), paste0("p", 1:12))
  f <- .sp_no_pkg("spatial_neighborhood_enrichment")
  .sp_expect(.sp_err(f(coords, labels)),
             "Package 'RANN' requis (install.packages('RANN')).")
})

test_that("compute_getis_ord_hotspots : RANN absent (110)", {
  coords <- data.frame(id = paste0("p", 1:12), x = runif(12), y = runif(12))
  values <- stats::setNames(runif(12), paste0("p", 1:12))
  f <- .sp_no_pkg("compute_getis_ord_hotspots")
  .sp_expect(.sp_err(f(coords, values)),
             "Package 'RANN' requis (install.packages('RANN')).")
})

test_that("spatial_lr_score : RANN absent (305)", {
  coords <- data.frame(id = paste0("p", 1:12), x = runif(12), y = runif(12))
  expr <- matrix(runif(120), nrow = 10,
                 dimnames = list(paste0("g", 1:10), paste0("p", 1:12)))
  lr <- data.frame(ligand = "G1", receptor = "G2", stringsAsFactors = FALSE)
  f <- .sp_no_pkg("spatial_lr_score")
  .sp_expect(.sp_err(f(coords, expr, lr)), "Package 'RANN' requis.")
})

test_that("le mock de dependance ne fuit pas dans globalenv", {
  # ⚠️ GARDE-FOU de la technique elle-meme : si ce test echoue, les 3 preuves
  # ci-dessus « passent » en realite sur un requireNamespace reellement casse,
  # et toutes les preuves suivantes du fichier seraient suspectes.
  f <- .sp_no_pkg("spatial_lr_score")
  invisible(tryCatch(f(data.frame(id = "p1", x = 0, y = 0),
                       matrix(0, 1, 1, dimnames = list("g1", "p1")),
                       data.frame(ligand = "g1", receptor = "g1")),
                     error = function(e) NULL))
  expect_false(exists("requireNamespace", envir = globalenv(), inherits = FALSE))
  expect_true(requireNamespace("RANN", quietly = TRUE))
})

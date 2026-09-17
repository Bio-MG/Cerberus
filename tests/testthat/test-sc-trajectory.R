# =============================================================================
# test-sc-trajectory.R — R/sc/sc_trajectory.R
# =============================================================================
# 13ᵉ incrément de la dette de conventions : classer les `stop()` du fichier
# avec `errorCondition(<msg>, class = "sc_trajectory_error")`.
#
# Portée : les 21 sites C10 de `R/sc/sc_trajectory.R`, répartis dans 6
# fonctions **pures** de premier niveau (R/ n'a pas le droit au Shiny — C2),
# donc appelables en R pur ⇒ la CLASSE est observable, contrairement aux
# serveurs réactifs de `modules/` (§2bv.3).
#
# ⚠️ Pourquoi ce fichier existe : `calculate_pseudotime()` était déjà exercé
# par `test-sc-helpers.R` (les functions ont déménagé lors du « Block 7 »), mais
# C9 exige un test ÉPONYME — d'où ce fichier, qui est leur vraie maison.
# Règle 5 : le test est écrit AVANT la conversion.
#
# ⚠️ ANTI-TRONCATURE (§2bn) : `errorCondition(msg, ...)` NE concatène PAS ses
# arguments positionnels, il garde le premier et perd la suite. Or
# `test-sc-helpers.R` assère des PRÉFIXES (`"Invalid root cell index"`), ce qui
# passerait **même si le message était tronqué**. Ce fichier assère donc les
# messages **ENTIERS**, au caractère près — c'est sa raison d'être.
#
# Sites injoignables, mesurés (verrou source seul) : 48, 51 (RANN / igraph
# installés), 175 (slingshot installé) — inatteignables par ABSENCE DU DÉFAUT ;
# 93, 135, 236 (exigent un échec interne de graphe / de Slingshot).
# =============================================================================

source_project_file("R/core/io_helpers.R")     # %||%
source_project_file("R/plotting/palettes.R")   # sc_discrete_scale / expression_continuous_scale
source_project_file("R/sc/sc_trajectory.R")

.traj_catch <- function(expr) {
  tryCatch({ expr; NULL },
           error = function(e) list(msg = conditionMessage(e), class = class(e)))
}

# Objet Seurat minimal (3 gènes x n cellules) — suffisant pour les gardes des
# fonctions de tracé, qui ne lisent que `@meta.data` et `rownames()`.
.traj_obj <- function(pseudotime = NULL, n = 3L) {
  skip_if_not_installed("SeuratObject")
  skip_if_not_installed("Matrix")
  # dgCMatrix directement : CreateSeuratObject() prévient sinon (« coercing to
  # dgCMatrix ») à chaque appel, ce qui polluerait la sortie du test.
  cnt <- Matrix::Matrix(matrix(1L, nrow = 3L, ncol = n,
                               dimnames = list(c("G1", "G2", "G3"),
                                               paste0("c", seq_len(n)))),
                        sparse = TRUE)
  md <- data.frame(row.names = paste0("c", seq_len(n)),
                   seurat_clusters = factor(rep("0", n)))
  if (!is.null(pseudotime)) md$pseudotime <- pseudotime
  SeuratObject::CreateSeuratObject(counts = cnt, meta.data = md)
}

# ---------------------------------------------------------------------------
# Verrou source : le fichier ne doit plus contribuer UN SEUL signalement C10.
# La détection est celle de la garde elle-même (jamais réimplémentée).
# ---------------------------------------------------------------------------
test_that("sc_trajectory.R ne contribue aucun signalement C10 (verrou source)", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools/check_conventions.R"), envir = e)
  before <- length(e$.REPORT$warns)
  e$check_c10_error_style(file.path(ts_project_root(), "R/sc/sc_trajectory.R"))
  w <- e$.REPORT$warns
  n <- length(w) - before
  if (n > 0L) {
    lignes <- vapply(w[(before + 1L):length(w)], function(x) x$line, integer(1))
    cat("Sites C10 restants dans sc_trajectory.R :",
        paste(sort(lignes), collapse = ", "), "\n")
  }
  expect_equal(n, 0L)
})

# ---------------------------------------------------------------------------
# calculate_pseudotime() — sites 57, 60, 66, 73
# ---------------------------------------------------------------------------
test_that("calculate_pseudotime : embeddings trop petit / non fini / k invalide", {
  emb2 <- matrix(1:4, nrow = 2L)                 # 2 cellules < 3
  e <- .traj_catch(calculate_pseudotime(emb2, k = 2))
  expect_identical(e$msg, "embeddings must be a numeric matrix with at least 3 cells.")
  expect_true("sc_trajectory_error" %in% e$class)

  emb_na <- cbind(c(1, 2, NA), c(1, 2, 3))       # 3 cellules mais un NA
  e <- .traj_catch(calculate_pseudotime(emb_na, k = 2))
  expect_identical(e$msg, "embeddings contain NA, NaN, or infinite values.")
  expect_true("sc_trajectory_error" %in% e$class)

  emb <- cbind(c(1, 2, 3, 4), c(1, 2, 3, 4))
  e <- .traj_catch(calculate_pseudotime(emb, k = 0))
  expect_identical(e$msg, "'k' must be a positive integer.")
  expect_true("sc_trajectory_error" %in% e$class)
})

test_that("calculate_pseudotime : index de racine hors bornes — message ENTIER (anti-troncature)", {
  # ⚠️ Site MULTI-ARGUMENTS : `stop("…between 1 and ", n_cells, ".")`.
  # Sans `paste0()`, `errorCondition()` tronquerait à "…between 1 and " —
  # or l'assertion de `test-sc-helpers.R` ("Invalid root cell index") passerait
  # quand même. D'où l'assertion du message COMPLET, `n_cells` et point inclus.
  emb <- cbind(c(1, 2, 3, 4), c(1, 2, 3, 4))
  e <- .traj_catch(calculate_pseudotime(emb, k = 2, root_cells = 99))
  expect_identical(e$msg, "Invalid root cell index. Valid indices are between 1 and 4.")
  expect_true("sc_trajectory_error" %in% e$class)
})

# ---------------------------------------------------------------------------
# calculate_slingshot_pseudotime() — sites 183, 187, 193, 197
# ---------------------------------------------------------------------------
test_that("calculate_slingshot_pseudotime : gardes sur embeddings et cluster_labels", {
  emb2 <- matrix(1:4, nrow = 2L)
  e <- .traj_catch(calculate_slingshot_pseudotime(emb2, cluster_labels = c("a", "b")))
  expect_identical(e$msg, "embeddings must be a numeric matrix with at least 3 cells and 2 dimensions.")
  expect_true("sc_trajectory_error" %in% e$class)

  emb_na <- cbind(c(1, 2, NA), c(1, 2, 3))
  e <- .traj_catch(calculate_slingshot_pseudotime(emb_na, cluster_labels = c("a", "a", "a")))
  expect_identical(e$msg, "embeddings contain NA, NaN, or infinite values.")
  expect_true("sc_trajectory_error" %in% e$class)

  emb <- cbind(c(1, 2, 3), c(1, 2, 3))
  e <- .traj_catch(calculate_slingshot_pseudotime(emb, cluster_labels = c("a", "b")))
  expect_identical(e$msg, "cluster_labels must have one value per cell.")
  expect_true("sc_trajectory_error" %in% e$class)

  e <- .traj_catch(calculate_slingshot_pseudotime(emb, cluster_labels = c("a", "a", NA)))
  expect_identical(e$msg, "cluster_labels contain missing or empty values.")
  expect_true("sc_trajectory_error" %in% e$class)
})

# ---------------------------------------------------------------------------
# plot_trajectory() / plot_slingshot_trajectory() — sites 301, 365, 369
# ---------------------------------------------------------------------------
test_that("plot_trajectory : moins de deux dimensions affichables", {
  e <- .traj_catch(plot_trajectory(matrix(1:3, ncol = 1L), pseudotime = c(1, 2, 3)))
  expect_identical(e$msg, "At least two display dimensions are required.")
  expect_true("sc_trajectory_error" %in% e$class)
})

test_that("plot_slingshot_trajectory : dimensions et longueur du pseudotemps", {
  e <- .traj_catch(plot_slingshot_trajectory(matrix(1:3, ncol = 1L), pseudotime = c(1, 2, 3)))
  expect_identical(e$msg, "At least two display dimensions are required.")
  expect_true("sc_trajectory_error" %in% e$class)

  e <- .traj_catch(plot_slingshot_trajectory(matrix(1:6, ncol = 2L), pseudotime = c(1, 2)))
  expect_identical(e$msg, "pseudotime must have the same length as embeddings rows.")
  expect_true("sc_trajectory_error" %in% e$class)
})

# ---------------------------------------------------------------------------
# plot_pseudotime_distribution() — sites 439, 453
# ---------------------------------------------------------------------------
test_that("plot_pseudotime_distribution : pseudotemps absent, puis tout NA", {
  obj <- .traj_obj()                             # pas de colonne pseudotime
  e <- .traj_catch(plot_pseudotime_distribution(obj))
  expect_identical(e$msg, "Pseudotemps non calculé — lancez d'abord 'Calculer Trajectoire'.")
  expect_true("sc_trajectory_error" %in% e$class)

  obj_na <- .traj_obj(pseudotime = rep(NA_real_, 3L))
  e <- .traj_catch(plot_pseudotime_distribution(obj_na))
  expect_identical(e$msg, "Distribution non disponible (pseudotemps NA pour toutes les cellules).")
  expect_true("sc_trajectory_error" %in% e$class)
})

# ---------------------------------------------------------------------------
# plot_genes_vs_pseudotime() — sites 485, 493
# ---------------------------------------------------------------------------
test_that("plot_genes_vs_pseudotime : pseudotemps absent, puis aucun gène valide", {
  obj <- .traj_obj()
  e <- .traj_catch(plot_genes_vs_pseudotime(obj, genes = "G1"))
  expect_identical(e$msg, "Pseudotemps non calculé — lancez d'abord 'Calculer Trajectoire'.")
  expect_true("sc_trajectory_error" %in% e$class)

  obj_pt <- .traj_obj(pseudotime = c(0, 0.5, 1))
  e <- .traj_catch(plot_genes_vs_pseudotime(obj_pt, genes = "ABSENT"))
  expect_identical(e$msg, "Aucun gène valide sélectionné")
  expect_true("sc_trajectory_error" %in% e$class)
})

# ---------------------------------------------------------------------------
# Témoin nominal : la conversion ne doit pas casser le chemin heureux.
# ---------------------------------------------------------------------------
test_that("plot_trajectory produit un ggplot sur un chemin nominal", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("RANN")
  skip_if_not_installed("igraph")
  emb <- cbind(a = c(1, 2, 3, 5, 8, 13), b = c(1, 1.5, 2, 3, 5, 8))
  res <- calculate_pseudotime(emb, k = 3)
  p <- plot_trajectory(emb, pseudotime = res$pseudotime)
  expect_s3_class(p, "ggplot")
})

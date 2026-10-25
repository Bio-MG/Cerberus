# =============================================================================
# test-provenance.R — manifeste de provenance R/core/provenance.R (2C)
# =============================================================================
# Principe teste : la provenance est PRODUITE (new_provenance_entry au moment
# du calcul, provenance_append immediatement) et seulement CONSOLIDEE
# (provenance_to_dataframe) — jamais reconstruite. Au moins 2 entrees
# chainees dans chaque scenario de flattening.

source_project_file("tools/pkg_skeleton/R/io_helpers.R")   # %||%
source_project_file("tools/pkg_skeleton/R/state.R")
source_project_file("tools/pkg_skeleton/R/provenance.R")

skip_if_not_installed("digest")   # empreintes de dataset (renv.lock l'engage)

# ---------------------------------------------------------------------------
# new_provenance_entry()
# ---------------------------------------------------------------------------
test_that("new_provenance_entry records the full manifest with a small dataset", {
  ds <- matrix(1:12, nrow = 3, ncol = 4)
  e <- new_provenance_entry(
    analysis_id = "sc-traj-1", method = "slingshot",
    parameters = list(k = 5, reduction = "UMAP"),
    dataset = ds, cells_used = 100, seed = 42, warnings = c("w1", "w2")
  )
  expect_identical(e$analysis_id, "sc-traj-1")
  expect_identical(e$method, "slingshot")
  expect_s3_class(e$timestamp, "POSIXct")
  expect_identical(e$dataset_dims, c(3L, 4L))
  expect_true(isTRUE(e$hash_exact))            # petit objet -> empreinte exacte
  expect_match(e$dataset_hash, "^[0-9a-f]+$")  # xxhash64 hex
  expect_identical(e$parameters, list(k = 5, reduction = "UMAP"))
  expect_true("R" %in% names(e$versions))      # versions logicielles au moins R
  expect_identical(e$cells_used, 100)
  expect_null(e$cells_excluded)
  expect_identical(e$seed, 42)
  expect_identical(e$warnings, c("w1", "w2"))
})

test_that("dataset identity: character name carries no hash, NULL is all-NA", {
  e <- new_provenance_entry("a", "m", dataset = "GSE12345")
  expect_identical(e$dataset_name, "GSE12345")
  expect_true(is.na(e$dataset_hash))
  expect_true(is.na(e$dataset_dims[1]))

  e2 <- new_provenance_entry("a", "m", dataset = NULL)
  expect_true(is.na(e2$dataset_name))
  expect_true(is.na(e2$dataset_hash))
  expect_true(is.na(e2$dataset_dims[1]))
})

test_that("hash is deterministic for identical small inputs", {
  ds <- matrix(1:12, nrow = 3, ncol = 4)
  h1 <- new_provenance_entry("a", "m", dataset = ds)$dataset_hash
  h2 <- new_provenance_entry("a", "m", dataset = ds)$dataset_hash
  expect_identical(h1, h2)
})

test_that("large objects get a SHALLOW hash (never serialize gigabytes)", {
  big <- matrix(runif(2000 * 2000), nrow = 2000)   # ~32 Mo > seuil 10 Mo
  e <- new_provenance_entry("a", "m", dataset = big)
  expect_false(isTRUE(e$hash_exact))
  expect_true(is.na(e$hash_exact) || isFALSE(e$hash_exact))
  expect_identical(e$dataset_dims, c(2000L, 2000L))
  rm(big); invisible(gc(FALSE, FALSE))
})

test_that("new_provenance_entry validates analysis_id and method (French)", {
  expect_error(new_provenance_entry("", "m"), "analysis_id")
  expect_error(new_provenance_entry("a", NULL), "method")
  expect_error(new_provenance_entry("a", "m"), NA)  # sanity: passage OK
})

test_that("cells_used accepts a vector of cell ids (flattened as a count later)", {
  e <- new_provenance_entry("a", "m", cells_used = c("c1", "c2", "c3"))
  expect_identical(e$cells_used, c("c1", "c2", "c3"))
})

# ---------------------------------------------------------------------------
# provenance_append()
# ---------------------------------------------------------------------------
test_that("provenance_append chains entries on a fresh analysis state", {
  s <- create_analysis_state()
  i1 <- provenance_append(s, new_provenance_entry("step-1", "methodA"))
  i2 <- provenance_append(s, new_provenance_entry("step-2", "methodB"))
  expect_identical(i1, 1L)
  expect_identical(i2, 2L)
  entries <- state_get(s, "provenance")
  expect_length(entries, 2)
  expect_identical(entries[[1]]$analysis_id, "step-1")
  expect_identical(entries[[2]]$analysis_id, "step-2")
  expect_identical(entries[[2]]$index, 2L)   # index de production marque
})

test_that("provenance_append tolerates a NULL state (module sans etat partage)", {
  expect_invisible(provenance_append(NULL, new_provenance_entry("a", "m")))
  expect_null(provenance_append(NULL, new_provenance_entry("a", "m")))
})

test_that("provenance_append refuses anything not built by new_provenance_entry", {
  s <- create_analysis_state()
  expect_error(provenance_append(s, list(foo = 1)), "new_provenance_entry")
  expect_error(provenance_append(s, "not-a-list"), "new_provenance_entry")
})

test_that("provenance_append refuses a corrupted provenance field", {
  s <- create_analysis_state()
  state_set(s, "provenance", data.frame(x = 1))
  expect_error(provenance_append(s, new_provenance_entry("a", "m")),
               "n'est pas une liste")
})

# ---------------------------------------------------------------------------
# provenance_to_dataframe() — CONSOLIDATION uniquement
# ---------------------------------------------------------------------------
test_that("provenance_to_dataframe flattens 2 chained entries in order", {
  s <- create_analysis_state()
  provenance_append(s, new_provenance_entry(
    "step-1", "slingshot",
    parameters = list(k = 5, root = "manual"),
    dataset = matrix(1:12, 3, 4), cells_used = 100, seed = 42,
    warnings = c("w1", "w2")
  ))
  provenance_append(s, new_provenance_entry(
    "step-2", "monocle3",
    parameters = list(npcs = 30), cells_excluded = 5
  ))
  df <- provenance_to_dataframe(s)
  expect_equal(nrow(df), 2)
  expect_identical(df$analysis_id, c("step-1", "step-2"))
  expect_identical(df$method, c("slingshot", "monocle3"))
  # Parametres aplaties "cle=valeur" separes par "; "
  expect_match(df$parameters[1], "k=5", fixed = TRUE)
  expect_match(df$parameters[1], "root=manual", fixed = TRUE)
  expect_match(df$parameters[2], "npcs=30", fixed = TRUE)
  # Dimensions du dataset de l'entree 1
  expect_identical(df$dataset_n_rows[1], "3")
  expect_identical(df$dataset_n_cols[1], "4")
  # Comptage cellules : scalaire -> valeur ; vecteur -> longueur
  expect_identical(df$cells_used[1], "100")
  expect_identical(df$cells_excluded[2], "5")
  expect_identical(df$cells_excluded[1], "")
  expect_identical(df$seed[1], "42")
  # Warnings joints par "; "
  expect_identical(df$warnings[1], "w1; w2")
  expect_identical(df$warnings[2], "")
  # Timestamp formate (chaine)
  expect_type(df$timestamp, "character")
  expect_match(df$timestamp[1], "^[0-9]{4}-[0-9]{2}-[0-9]{2}")
})

test_that("provenance_to_dataframe counts a vector of cell ids", {
  s <- create_analysis_state()
  provenance_append(s, new_provenance_entry("a", "m",
                                            cells_used = c("c1", "c2", "c3")))
  df <- provenance_to_dataframe(s)
  expect_identical(df$cells_used[1], "3")
})

test_that("provenance_to_dataframe returns a typed empty frame when nothing recorded", {
  df_empty <- provenance_to_dataframe(create_analysis_state())
  # Un etat NULL est traite comme un etat sans entree (jamais d'erreur pour
  # le rapport — consolidation tolerant).
  df_null <- provenance_to_dataframe(NULL)
  for (d in list(df_empty, df_null)) {
    expect_equal(nrow(d), 0)
    for (col in c("analysis_id", "method", "timestamp", "dataset_name",
                  "dataset_hash", "hash_exact", "dataset_n_rows", "dataset_n_cols",
                  "parameters", "versions", "cells_used", "cells_excluded",
                  "seed", "warnings")) {
      expect_true(col %in% colnames(d), info = col)
    }
  }
})

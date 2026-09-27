# =============================================================================
# test-sc-multi.R — MD-4 : conteneur `sc_datasets` & double jeu SC (modes 1/2)
# =============================================================================
# Couvre le contrat docs/contracts/SC_MULTI_CONTRACT.md : surface publique,
# validation des labels/objets/relations, enregistrement (overwrite explicite,
# plafond, producteurs), suppression, résumé, isolation (remplacer sc_obj ne
# change jamais une entrée), relations déclarées de la décision 5.
# =============================================================================
source_project_file("R/sc/sc_multi.R")

.make_sc_obj <- function(n_genes = 50, n_cells = 20, seed = 1,
                         project = "Test_SC") {
  set.seed(seed)
  mat <- matrix(rpois(n_genes * n_cells, lambda = 2), nrow = n_genes)
  dimnames(mat) <- list(paste0("GENE", seq_len(n_genes)),
                        paste0("c", seq_len(n_cells)))
  obj <- Seurat::CreateSeuratObject(counts = mat, project = project)
  obj$orig.ident <- factor(rep(c("s1", "s2"), length.out = n_cells))
  obj
}

.expect_multi_state <- function(expr, state) {
  err <- tryCatch(force(expr), error = function(e) e)
  expect_true(inherits(err, "sc_multi_error"),
              info = paste("erreur sc_multi_error attendue, état :", state))
  expect_identical(err$state, state)
}

test_that("surface publique, états d'erreur et relations figés", {
  expect_setequal(
    sc_multi_public_api(),
    c("sc_multi_public_api", "sc_multi_error_states", "sc_multi_relations",
      "sc_multi_check_label", "sc_multi_check_obj", "sc_multi_check_relation",
      "sc_multi_register", "sc_multi_remove", "sc_multi_get",
      "sc_multi_summary")
  )
  expect_setequal(
    sc_multi_error_states(),
    c("invalid_input", "invalid_label", "invalid_obj", "invalid_relation",
      "duplicate_label", "unknown_label", "capacity_exceeded")
  )
  expect_setequal(
    sc_multi_relations(),
    c("standalone", "shared_params", "distinct_params")
  )
})

test_that("labels : trim, vide, trop long, caractères de contrôle, non-caractère", {
  expect_identical(sc_multi_check_label("  Jeu_SC_1  "), "Jeu_SC_1")
  .expect_multi_state(sc_multi_check_label("   "), "invalid_label")
  .expect_multi_state(sc_multi_check_label(""), "invalid_label")
  .expect_multi_state(sc_multi_check_label(strrep("x", 81L)), "invalid_label")
  .expect_multi_state(sc_multi_check_label("a\nb"), "invalid_label")
  .expect_multi_state(sc_multi_check_label(42), "invalid_label")
})

test_that("objets : Seurat avec >= 1 cellule exigé", {
  sc_multi_check_obj(.make_sc_obj())
  .expect_multi_state(sc_multi_check_obj(list(counts = 1)), "invalid_obj")
  .expect_multi_state(sc_multi_check_obj(NULL), "invalid_obj")
  .expect_multi_state(sc_multi_check_obj("Seurat"), "invalid_obj")
})

test_that("relations : standalone | shared_params (mode 1) | distinct_params (mode 2)", {
  sc_multi_check_relation("standalone")
  sc_multi_check_relation("shared_params")
  sc_multi_check_relation("distinct_params")
  .expect_multi_state(sc_multi_check_relation("merged"), "invalid_relation")
  .expect_multi_state(sc_multi_check_relation(NULL), "invalid_relation")
  .expect_multi_state(sc_multi_check_relation(1), "invalid_relation")
})

test_that("enregistrement : entrée conforme (producteur, relation, horodatages)", {
  obj <- .make_sc_obj()
  ds <- sc_multi_register(NULL, " Jeu_A ", obj,
                          relation = "shared_params", producer = "import")
  expect_setequal(names(ds), "Jeu_A")
  e <- ds[["Jeu_A"]]
  expect_identical(e$label, "Jeu_A")
  expect_identical(e$producer, "import")
  expect_identical(e$relation, "shared_params")
  expect_s3_class(e$registered_at, "POSIXct")
  expect_s3_class(e$updated_at, "POSIXct")
  expect_identical(e$obj, obj)
})

test_that("duplicate_label refusé sans overwrite explicite ; overwrite conserve registered_at", {
  ds <- sc_multi_register(NULL, "Jeu_A", .make_sc_obj())
  .expect_multi_state(
    sc_multi_register(ds, "Jeu_A", .make_sc_obj()),
    "duplicate_label")
  first <- ds[["Jeu_A"]]$registered_at
  ds2 <- sc_multi_register(ds, "Jeu_A", .make_sc_obj(seed = 2),
                           relation = "distinct_params",
                           producer = "pipeline_save", overwrite = TRUE)
  e <- ds2[["Jeu_A"]]
  expect_identical(e$producer, "pipeline_save")
  expect_identical(e$relation, "distinct_params")
  expect_identical(e$registered_at, first)
  expect_true(e$updated_at >= first)
})

test_that("plafond du conteneur (repli 5L) — capacity_exceeded", {
  ds <- NULL
  for (i in 1:5) {
    ds <- sc_multi_register(ds, paste0("Jeu_", i), .make_sc_obj(seed = i),
                            max_datasets = 5L)
  }
  .expect_multi_state(
    sc_multi_register(ds, "Jeu_6", .make_sc_obj(), max_datasets = 5L),
    "capacity_exceeded")
  # overwrite d'un label existant même conteneur plein : autorisé
  ds2 <- sc_multi_register(ds, "Jeu_3", .make_sc_obj(seed = 9),
                           overwrite = TRUE, max_datasets = 5L)
  expect_identical(names(ds2)[3], "Jeu_3")
  .expect_multi_state(
    sc_multi_register(ds, "Jeu_6", .make_sc_obj(), max_datasets = 0L),
    "invalid_input")
})

test_that("résumé : colonnes figées, compteurs Seurat, relation exposée", {
  ds <- sc_multi_register(NULL, "Jeu_A", .make_sc_obj(),
                          relation = "shared_params", producer = "import")
  s <- sc_multi_summary(ds)
  expect_setequal(
    names(s),
    c("label", "producer", "relation", "n_cells", "n_genes", "n_samples",
      "has_clusters", "median_nFeature_RNA", "median_nCount_RNA",
      "median_percent_mt", "registered_at", "updated_at")
  )
  expect_identical(s$label, "Jeu_A")
  expect_identical(s$producer, "import")
  expect_identical(s$relation, "shared_params")
  expect_identical(s$n_cells, 20L)
  expect_identical(s$n_genes, 50L)
  expect_identical(s$n_samples, 2L)   # 2 orig.ident distincts
  expect_false(s$has_clusters)        # pas de clustering sur le jeu brut
  # conteneur vide : 0 ligne, mêmes colonnes
  s0 <- sc_multi_summary(list())
  expect_identical(nrow(s0), 0L)
  expect_setequal(names(s0), names(s))
})

test_that("remove/get : unknown_label sur label absent", {
  ds <- sc_multi_register(NULL, "Jeu_A", .make_sc_obj())
  .expect_multi_state(sc_multi_remove(ds, "Jeu_B"), "unknown_label")
  .expect_multi_state(sc_multi_get(ds, "Jeu_B"), "unknown_label")
  expect_identical(names(sc_multi_remove(ds, "Jeu_A")), character(0))
  expect_s4_class(sc_multi_get(ds, "Jeu_A")$obj, "Seurat")
})

test_that("isolation : remplacer l'objet actif ne change jamais l'entrée", {
  obj <- .make_sc_obj()
  ds <- sc_multi_register(NULL, "Jeu_A", obj)
  # le module continue de vivre : l'utilisateur recharge un AUTRE objet
  # (affectation d'un nouvel objet, convention Seurat — pas de mutation
  # en place de l'objet enregistré).
  global_copy <- obj
  global_copy <- .make_sc_obj(seed = 7, project = "Autre")
  e <- ds[["Jeu_A"]]
  expect_s4_class(e$obj, "Seurat")
  expect_identical(as.character(e$obj@project.name), "Test_SC")
  expect_false(identical(e$obj, global_copy))
})

test_that("pureté Shiny : aucun symbole réactif dans R/sc/sc_multi.R", {
  src <- paste(readLines(file.path(ts_project_root(), "R/sc/sc_multi.R"),
                         warn = FALSE), collapse = "\n")
  for (sym in c("reactiveVal", "reactiveValues", "observeEvent", "output\\$",
                "input\\$", "moduleServer", "showNotification")) {
    expect_false(grepl(sym, src, perl = TRUE),
                 info = paste("symbole Shiny interdit trouvé :", sym))
  }
})

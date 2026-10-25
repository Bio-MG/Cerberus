# =============================================================================
# test-bulk-multi.R — MD-1 : conteneur `bulk_datasets` & jeux bulk nommés
# =============================================================================
# Couvre le contrat docs/contracts/BULK_MULTI_CONTRACT.md : surface publique,
# validation des labels/objets/pipeline, enregistrement (overwrite explicite,
# plafond, producteurs), suppression, résumé, ISOLATION par copie (garde §2.2
# — muter la source après enregistrement ne change jamais l'entrée).
# =============================================================================
source_project_file("tools/pkg_skeleton/R/bulk_multi.R")

.bulk_multi_obj <- function(n_genes = 50, n_samples = 6, seed = 1,
                            import_mode = "merged_matrix") {
  set.seed(seed)
  counts <- matrix(round(abs(rnorm(n_genes * n_samples, 100, 30))), nrow = n_genes)
  dimnames(counts) <- list(paste0("GENE", seq_len(n_genes)),
                           paste0("s", seq_len(n_samples)))
  meta <- data.frame(row.names = colnames(counts),
                     condition = rep(c("A", "B"), length.out = n_samples))
  list(counts = counts, metadata = meta, project = "Projet_Test", type = "bulk",
       timestamp = Sys.time(), gene_id_type = "symbol", de_allowed = TRUE,
       import_mode = import_mode)
}

.bulk_multi_pipeline <- function(obj) {
  list(
    mapping_applied = FALSE, mapping_summary = NULL,
    filtered_counts = obj$counts[1:30, , drop = FALSE],
    vst_mat = obj$counts[1:20, , drop = FALSE],
    contrasts = list(c1 = data.frame(id = "G1", baseMean = 10, p = 0.01)),
    active_contrast = "c1", multimethod_de = NULL,
    lfc_thresh = 1, padj_thresh = 0.05,
    pathway_results = data.frame(pathway = "PW1", p = 0.01),
    pathway_db = "GOBP", pathway_mode = "ora"
  )
}

.expect_multi_state <- function(expr, state) {
  err <- tryCatch(force(expr), error = function(e) e)
  expect_true(inherits(err, "bulk_multi_error"),
              info = paste("erreur bulk_multi_error attendue, état :", state))
  expect_identical(err$state, state)
}

test_that("surface publique, états d'erreur et champs de pipeline figés", {
  expect_setequal(
    bulk_multi_public_api(),
    c("bulk_multi_public_api", "bulk_multi_error_states",
      "bulk_multi_pipeline_fields", "bulk_multi_check_label",
      "bulk_multi_check_obj", "bulk_multi_capture_pipeline",
      "bulk_multi_register", "bulk_multi_remove", "bulk_multi_get",
      "bulk_multi_summary")
  )
  expect_setequal(
    bulk_multi_error_states(),
    c("invalid_input", "invalid_label", "invalid_obj", "invalid_pipeline",
      "duplicate_label", "unknown_label", "capacity_exceeded",
      "insufficient_datasets", "no_common_contrast", "no_significant_genes")
  )
  expect_setequal(
    bulk_multi_pipeline_fields(),
    c("mapping_applied", "mapping_summary", "filtered_counts", "vst_mat",
      "contrasts", "active_contrast", "multimethod_de",
      "lfc_thresh", "padj_thresh", "pathway_results", "pathway_db",
      "pathway_mode")
  )
})

test_that("labels : trim, vide, trop long, caractères de contrôle, non-caractère", {
  expect_identical(bulk_multi_check_label("  Mon_Jeu  "), "Mon_Jeu")
  .expect_multi_state(bulk_multi_check_label("   "), "invalid_label")
  .expect_multi_state(bulk_multi_check_label(""), "invalid_label")
  .expect_multi_state(bulk_multi_check_label(strrep("x", 81L)), "invalid_label")
  .expect_multi_state(bulk_multi_check_label("a\nb"), "invalid_label")
  .expect_multi_state(bulk_multi_check_label(c("a", "b")), "invalid_label")
  .expect_multi_state(bulk_multi_check_label(42), "invalid_label")
})

test_that("objets : structure bulk_obj exigée (counts matrice nommée + metadata)", {
  bulk_multi_check_obj(.bulk_multi_obj())
  expect_invisible(bulk_multi_check_obj(.bulk_multi_obj()))
  .expect_multi_state(bulk_multi_check_obj("pas une liste"), "invalid_obj")
  .expect_multi_state(bulk_multi_check_obj(list(project = "P")), "invalid_obj")

  no_names <- .bulk_multi_obj()
  dimnames(no_names$counts) <- NULL
  .expect_multi_state(bulk_multi_check_obj(no_names), "invalid_obj")

  bad_meta <- .bulk_multi_obj()
  bad_meta$metadata <- as.matrix(bad_meta$metadata)
  .expect_multi_state(bulk_multi_check_obj(bad_meta), "invalid_obj")

  bad_dims <- .bulk_multi_obj()
  bad_dims$metadata <- bad_dims$metadata[-1L, , drop = FALSE]
  .expect_multi_state(bulk_multi_check_obj(bad_dims), "invalid_obj")
})

test_that("capture pipeline : liste OU reactiveValues-like, champs exacts", {
  obj <- .bulk_multi_obj()
  snap <- bulk_multi_capture_pipeline(.bulk_multi_pipeline(obj))
  expect_setequal(names(snap), bulk_multi_pipeline_fields())
  expect_identical(snap$contrasts$c1$id, "G1")
  expect_identical(snap$padj_thresh, 0.05)

  env_like <- new.env(parent = emptyenv())
  env_like$contrasts <- list(z = 1)
  snap_env <- bulk_multi_capture_pipeline(env_like)
  expect_setequal(names(snap_env), bulk_multi_pipeline_fields())
  expect_identical(snap_env$contrasts, list(z = 1))
  expect_null(snap_env$vst_mat)

  .expect_multi_state(bulk_multi_capture_pipeline(NULL), "invalid_input")
  .expect_multi_state(bulk_multi_capture_pipeline(42), "invalid_input")
})

test_that("enregistrement : structure d'entrée, producteur, retours invisibles", {
  obj <- .bulk_multi_obj()
  ds <- bulk_multi_register(NULL, "  Jeu_A  ", obj, producer = "import")
  expect_type(ds, "list")
  expect_setequal(names(ds), "Jeu_A")
  e <- ds[["Jeu_A"]]
  expect_identical(e$label, "Jeu_A")
  expect_identical(e$producer, "import")
  expect_identical(e$registered_at, e$updated_at)
  expect_identical(e$obj$project, "Projet_Test")
  expect_null(e$pipeline)
  expect_invisible(bulk_multi_register(NULL, "Jeu_A", obj, producer = "import"))
})

test_that("GARDE §2.2 — isolation par copie : muter la source ne change pas l'entrée", {
  obj <- .bulk_multi_obj()
  ds <- bulk_multi_register(NULL, "Jeu_A", obj, producer = "import")

  obj$counts[1L, 1L] <- -999L                      # mutation en place de la source
  obj$project <- "Modifié"
  expect_identical(ds[["Jeu_A"]]$obj$counts[1L, 1L],
                   .bulk_multi_obj()$counts[1L, 1L])
  expect_identical(ds[["Jeu_A"]]$obj$project, "Projet_Test")

  # Même garde pour le pipeline capturé
  pipe <- .bulk_multi_pipeline(.bulk_multi_obj())
  ds2 <- bulk_multi_register(ds, "Jeu_B", .bulk_multi_obj(seed = 2),
                             pipeline_state = pipe)
  pipe$contrasts <- list(hacked = TRUE)
  expect_identical(ds2[["Jeu_B"]]$pipeline$contrasts$c1$id, "G1")
})

test_that("duplicate_label : refus sauf overwrite explicite (registered_at conservé)", {
  obj <- .bulk_multi_obj()
  ds <- bulk_multi_register(NULL, "Jeu_A", obj, producer = "import")
  Sys.sleep(0.01)  # horodatages distincts

  .expect_multi_state(
    bulk_multi_register(ds, "Jeu_A", .bulk_multi_obj(seed = 3),
                        producer = "pipeline_save"),
    "duplicate_label")

  ds2 <- bulk_multi_register(ds, "Jeu_A", .bulk_multi_obj(seed = 3),
                             pipeline_state = .bulk_multi_pipeline(.bulk_multi_obj()),
                             producer = "pipeline_save", overwrite = TRUE)
  e <- ds2[["Jeu_A"]]
  expect_identical(e$producer, "pipeline_save")
  expect_false(is.null(e$pipeline))
  expect_true(e$updated_at >= e$registered_at)

  .expect_multi_state(
    bulk_multi_register(ds, "Jeu_A", obj, overwrite = NA),
    "invalid_input")
})

test_that("capacity_exceeded : plafond respecté, overwrite toujours permis à la limite", {
  obj <- .bulk_multi_obj()
  ds <- list(
    A = bulk_multi_register(NULL, "A", obj)[["A"]],
    B = bulk_multi_register(NULL, "B", obj)[["B"]]
  )
  .expect_multi_state(
    bulk_multi_register(ds, "C", obj, max_datasets = 2L),
    "capacity_exceeded")
  ds3 <- bulk_multi_register(ds, "B", .bulk_multi_obj(seed = 4),
                             overwrite = TRUE, max_datasets = 2L)
  expect_setequal(names(ds3), c("A", "B"))
})

test_that("producteur étiqueté : import | pipeline_save | pseudobulk UNIQUEMENT", {
  obj <- .bulk_multi_obj()
  .expect_multi_state(bulk_multi_register(NULL, "X", obj, producer = "autre"),
                      "invalid_input")
  .expect_multi_state(bulk_multi_register(NULL, "X", obj, producer = NULL),
                      "invalid_input")
  for (p in c("import", "pipeline_save", "pseudobulk")) {
    ds <- bulk_multi_register(NULL, "X", obj, producer = p)
    expect_identical(ds[["X"]]$producer, p)
  }
})

test_that("pipeline_state : noms exacts exigés (pas d'inflation sans contrat)", {
  obj <- .bulk_multi_obj()
  .expect_multi_state(
    bulk_multi_register(NULL, "X", obj, pipeline_state = list(champ = 1)),
    "invalid_pipeline")
  ds <- bulk_multi_register(NULL, "X", obj,
                            pipeline_state = .bulk_multi_pipeline(obj))
  expect_setequal(names(ds[["X"]]$pipeline), bulk_multi_pipeline_fields())
})

test_that("remove/get : suppression explicite, unknown_label sinon", {
  obj <- .bulk_multi_obj()
  ds <- bulk_multi_register(NULL, "Jeu_A", obj)
  expect_identical(bulk_multi_get(ds, "Jeu_A")$label, "Jeu_A")
  .expect_multi_state(bulk_multi_get(ds, "Absent"), "unknown_label")
  .expect_multi_state(bulk_multi_remove(ds, "Absent"), "unknown_label")
  ds2 <- bulk_multi_remove(ds, "Jeu_A")
  expect_length(ds2, 0L)
  expect_invisible(bulk_multi_remove(ds, "Jeu_A"))
})

test_that("conteneur invalide (liste non nommée) refusé", {
  .expect_multi_state(
    bulk_multi_register(list("a", "b"), "X", .bulk_multi_obj()),
    "invalid_input")
})

test_that("résumé : colonnes figées, valeurs exactes, conteneur vide sûr", {
  cols <- c("label", "producer", "n_genes", "n_samples", "has_filtered",
            "n_contrasts", "has_pathways", "import_mode", "registered_at",
            "updated_at")
  s0 <- bulk_multi_summary(NULL)
  expect_s3_class(s0, "data.frame")
  expect_identical(names(s0), cols)
  expect_identical(nrow(s0), 0L)
  expect_identical(bulk_multi_summary(list()), bulk_multi_summary(NULL))

  obj <- .bulk_multi_obj(import_mode = "per_sample")
  ds <- bulk_multi_register(NULL, "Jeu_import", obj, producer = "import")
  ds <- bulk_multi_register(ds, "Jeu_complet", .bulk_multi_obj(seed = 2),
                            pipeline_state = .bulk_multi_pipeline(.bulk_multi_obj()),
                            producer = "pipeline_save")
  s <- bulk_multi_summary(ds)
  expect_identical(names(s), cols)
  expect_identical(nrow(s), 2L)
  imp <- s[s$label == "Jeu_import", ]
  expect_identical(imp$producer, "import")
  expect_identical(imp$n_genes, 50L)
  expect_identical(imp$n_samples, 6L)
  expect_false(imp$has_filtered)
  expect_identical(imp$n_contrasts, 0L)
  expect_false(imp$has_pathways)
  expect_identical(imp$import_mode, "per_sample")
  cmp <- s[s$label == "Jeu_complet", ]
  expect_identical(cmp$producer, "pipeline_save")
  expect_true(cmp$has_filtered)
  expect_identical(cmp$n_contrasts, 1L)
  expect_true(cmp$has_pathways)
  expect_true(grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}$", imp$registered_at))
})

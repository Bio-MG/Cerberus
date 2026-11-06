# test-deferred-boot-init.R — garde du fix F2 « réordonnancement partiel »
# (2026-11-06) : l'init lourde (packages d'analyse + plan future) doit être
# schedulée APRES la mise à l'écoute via later::later(), derrière la porte
# .biomentat_deferred_boot_init() qui attend le premier accept TCP armé.
# Ancres ASCII volontaires (leçon readLines hors UTF-8 : f393f8a).
# Résolution de racine : testthat cwd = tests/testthat — on remonte au
# marqueur app.R (même règle que helper-source.R), jamais getwd() cru.

.f2_root <- local({
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  for (i in seq_len(6)) {
    if (file.exists(file.path(d, "app.R"))) return(d)
    p <- dirname(d)
    if (identical(p, d)) break
    d <- p
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
})

test_that("global.R split LIGHT/DEFERRED : la liste LIGHT ne charge plus les packages d'analyse", {
  lines <- readLines(file.path(.f2_root, "global.R"), warn = FALSE)
  joined <- paste(lines, collapse = "\n")
  # Les deux fonctions de la porte existent
  expect_true(grepl(".biomentat_deferred_boot_init <- function", joined, fixed = TRUE),
              label = "porte .biomentat_deferred_boot_init definie dans global.R")
  expect_true(grepl(".biomentat_deferred_boot_run <- function", joined, fixed = TRUE),
              label = "corps differe .biomentat_deferred_boot_run defini dans global.R")
  # La liste LIGHT (avant la section 3bis/DEFERRED) ne contient plus les
  # packages d'analyse differes
  split_marks <- grep("Section 3bis|DEFERRED", lines)
  cut <- if (length(split_marks)) split_marks[1] else length(lines)
  light_zone <- paste(lines[seq_len(cut)], collapse = "\n")
  for (pkg in c("Seurat", "SingleR", "celldex", "destiny", "DESeq2", "slingshot", "sva", "SingleCellExperiment", "BPCells")) {
    expect_false(grepl(paste0('"', pkg, '"'), light_zone, fixed = TRUE),
                 label = paste0("package d'analyse ", pkg, " absent de la liste LIGHT (pre-listen)"))
  }
})

test_that("app.R schedule l'init differee via later, AVANT shinyApp, exactement une fois", {
  lines <- readLines(file.path(.f2_root, "app.R"), warn = FALSE)
  sched_idx <- grep("later::later(function() .biomentat_deferred_boot_init()", lines, fixed = TRUE)
  app_idx <- grep("shinyApp(ui, server)", lines, fixed = TRUE)
  expect_length(sched_idx, 1L)
  expect_true(length(app_idx) >= 1L)
  expect_true(sched_idx[1] < app_idx[1],
              label = "scheduling later AVANT l'appel shinyApp")
  # Repli honnête si later absent : run() sérialisé
  joined <- paste(lines, collapse = "\n")
  expect_true(grepl(".biomentat_deferred_boot_run()", joined, fixed = TRUE),
              label = "repli honnete = run() serialise si later absent")
})

# =============================================================================
# test-mod-sc-metadata.R — modules/sc/mod_sc_metadata.R
# =============================================================================
# Régression P0 (plan d'exécution 2026-10-17, item M1a).
#
# L'observateur de resynchronisation reprenait le design déjà appliqué avec
#   `if (!is.na(hit)) tbl$condition[!is.na(hit)] <- ...`
# où `hit <- match(tbl$sample, as.character(meta$orig.ident))` est un VECTEUR
# (un élément par échantillon). Depuis R 4.2, `if()` sur une condition de
# longueur > 1 est une ERREUR (« the condition has length > 1 ») : le module
# plantait donc dès qu'un objet portait **>= 2 échantillons** ET une colonne
# `condition` — c'est-à-dire le cas ORDINAIRE d'un design déclaré.
#
# ⚠️ **PIÈGE MESURÉ — ne pas réécrire ce test en `tryCatch(testServer(...))`.**
# `testServer()` n'échoue PAS quand un observateur lève une erreur : il imprime
# la trace de l'observateur et **continue** (mesuré : le module encore cassé
# rendait `fail=0 pass=1`, avec seulement `warn=1`). Une assertion « pas
# d'erreur » autour de `testServer` est donc un **faux vert**.
# ⇒ On assère l'**effet observable** : l'observateur casse AVANT `map_rv(tbl)`,
# donc le récapitulatif reste « En attente d'un objet SC. » alors qu'un design
# complet a été déclaré. Rouge attendu = ce récapitulatif vide.
# =============================================================================

source_project_file("R/core/io_helpers.R")   # %||%
source_project_file("R/sc/sc_metadata.R")    # sc_metadata_sample_table()
source_project_file("modules/sc/mod_sc_metadata.R")

suppressPackageStartupMessages(library(shiny))

# Objet minimal : 12 cellules, DEUX échantillons, et un design déjà déclaré
# (condition + replicate) que le module doit reprendre tel quel.
.msm_fixture <- function(n = 12L) {
  skip_if_not_installed("Seurat")
  suppressWarnings(suppressPackageStartupMessages(library(Seurat)))
  set.seed(17)
  genes  <- paste0("gene", seq_len(6))
  counts <- Matrix::Matrix(
    seq_len(length(genes) * n) %% 4L,
    nrow = length(genes), ncol = n, sparse = TRUE
  )
  dimnames(counts) <- list(genes, paste0("cell", seq_len(n)))
  meta <- data.frame(
    orig.ident = factor(rep(c("s1", "s2"), each = n / 2)),
    condition  = rep(c("ctrl", "stim"), each = n / 2),
    replicate  = rep(c("r1", "r2"), each = n / 2),
    row.names  = colnames(counts),
    stringsAsFactors = FALSE
  )
  CreateSeuratObject(counts = counts, meta.data = meta)
}

test_that("mod_sc_metadata : le design d'un objet à 2 échantillons est repris", {
  obj <- .msm_fixture()
  gd  <- shiny::reactiveValues(sc_obj = NULL, i18n = NULL, language = "fr")

  recap <- NULL
  testServer(mod_sc_metadata_server,
             args = list(id = "metadata", global_data = gd), {
    # L'input est posé AVANT l'objet : l'observateur `input$sample_col` ne fait
    # rien tant que `sc_obj` est NULL (son `req()`), il n'écrase donc pas le
    # design repris par l'observateur `sc_obj`.
    session$setInputs(sample_col = "orig.ident")
    gd$sc_obj <- obj
    session$flushReact()
    # `<<-` est NÉCESSAIRE : testServer évalue son bloc dans un environnement
    # dérivé, un `<-` y créerait une liaison locale et le `recap` du test
    # resterait NULL (mesuré : 4 échecs trompeurs).
    recap <<- output$design_recap
  })

  recap <- paste(as.character(recap), collapse = "\n")
  # Le récapitulatif agrège par CONDITION (il n'affiche pas les noms
  # d'échantillon) : la preuve que le design a été repris, c'est la présence
  # des deux conditions déclarées dans l'objet.
  expect_match(recap, "ctrl", fixed = TRUE)
  expect_match(recap, "stim", fixed = TRUE)
  # …et surtout PAS l'état « rien reçu », qui est la signature du plantage
  # (l'observateur casse AVANT map_rv(tbl)).
  expect_false(grepl("En attente d'un objet SC", recap, fixed = TRUE))
})

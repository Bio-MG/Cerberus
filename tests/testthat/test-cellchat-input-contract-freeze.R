# =============================================================================
# test-cellchat-input-contract-freeze.R — 4D-3 : gel du contrat d'entree CellChat
# =============================================================================
# Ce fichier REFUSE toute evolution incompatible du contrat de
# R/sc/sc_communication_input.R :
#   - surface publique (cellchat_input_public_api()) — aucune fonction
#     top-level non prefixee d'un point hors de cette liste ;
#   - les 4 etats de validite (jamais etendus silencieusement) ;
#   - helpers internes dotes d'un point ;
#   - synchronisation code <-> docs/contracts/CELLCHAT_INPUT_CONTRACT.md ;
#   - absence de dependance nouvelle : AUCUN appel CellChat dans le code
#     (le fichier produit les ENTREES ; l'inference reste une etape separee).
# Toute modification du contrat doit passer simultanement par le code, ce test
# et la documentation.
# =============================================================================

source_project_file("R/core/io_helpers.R")
source_project_file("R/core/provenance.R")
source_project_file("R/plotting/palettes.R")
source_project_file("R/sc/sc_velocity.R")
source_project_file("R/sc/sc_communication.R")
source_project_file("R/sc/sc_communication_input.R")

.cellchat_input_top_level <- function(relpath) {
  exprs <- parse(file.path(ts_project_root(), relpath), keep.source = FALSE)
  nms <- character(0)
  for (e in exprs) {
    if (is.call(e) && identical(deparse(e[[1L]]), "<-") && is.symbol(e[[2L]])) {
      nms <- c(nms, as.character(e[[2L]]))
    }
  }
  unique(nms)
}

test_that("the public API surface is EXACTLY the frozen list", {
  top <- .cellchat_input_top_level("R/sc/sc_communication_input.R")
  public <- setdiff(top, grep("^\\.", top, value = TRUE))
  expect_setequal(public, cellchat_input_public_api())
  # the frozen list is sorted and duplicate-free
  expect_false(anyDuplicated(cellchat_input_public_api()) > 0)
  expect_identical(cellchat_input_public_api(), sort(cellchat_input_public_api()))
})

test_that("every internal helper stays dot-prefixed", {
  top <- .cellchat_input_top_level("R/sc/sc_communication_input.R")
  internal <- grep("^\\.", top, value = TRUE)
  expect_true(length(internal) > 0L)
  expect_true(".cellchat_input_stop" %in% internal)
  # no public-looking name leaks outside the API list
  expect_setequal(internal, setdiff(top, cellchat_input_public_api()))
})

test_that("the 4 validity states are frozen", {
  expect_identical(
    cellchat_input_states(),
    c("invalid_input", "invalid_features", "invalid_labels", "invalid_species")
  )
})

test_that("the contract doc is in sync with the code (states + public API)", {
  doc_path <- file.path(ts_project_root(), "docs", "contracts", "CELLCHAT_INPUT_CONTRACT.md")
  skip_if_not(file.exists(doc_path), .ts_contract_skip_msg(doc_path))
  txt <- paste(.ts_contract_readlines(doc_path, warn = FALSE), collapse = "\n")
  for (st in cellchat_input_states()) {
    expect_true(grepl(st, txt, fixed = TRUE), info = st)
  }
  for (fn in cellchat_input_public_api()) {
    expect_true(grepl(fn, txt, fixed = TRUE), info = fn)
  }
  # the doc must state that the 10X FORMAT is not the blocker
  expect_true(grepl("n'est pas le blocage", txt, fixed = TRUE) ||
              grepl("pas le blocage", txt, fixed = TRUE))
})

test_that("the file adds NO new dependency and never calls CellChat", {
  code <- readLines(file.path(ts_project_root(), "R/sc/sc_communication_input.R"),
                    warn = FALSE)
  code <- grep("^\\s*#", code, value = TRUE, invert = TRUE)  # strip comments
  expect_false(any(grepl("CellChat::", code, fixed = TRUE)))
  expect_false(any(grepl("library(CellChat", code, fixed = TRUE)))
  expect_false(any(grepl("requireNamespace(\"CellChat\"", code, fixed = TRUE)))
})

test_that("the three real requirements and two declared decisions are encoded", {
  req <- cellchat_input_requirements()
  expect_setequal(
    req$requirement,
    c("expression_normalisee", "symboles_de_genes", "etiquettes_de_population",
      "espece_declaree", "contrat_upstream")
  )
  # species has NO USABLE default in the builder signatures -> the caller MUST
  # declare it. The default is NULL, which is not a valid species: omitting it
  # raises a classed invalid_species rather than silently picking a database.
  expect_true("species" %in% names(formals(cellchat_input_from_matrix)))
  expect_true("species" %in% names(formals(build_cellchat_input)))
  m <- matrix(1:8, nrow = 2, dimnames = list(c("G1", "G2"), paste0("c", 1:4)))
  for (omit in list(
        tryCatch(cellchat_input_from_matrix(m, c("G1", "G2"), c("A", "A", "B", "B")),
                 error = function(e) e),
        tryCatch(build_cellchat_input(NULL, group_by = "x"), error = function(e) e)
      )) {
    expect_s3_class(omit, "cellchat_input_error")
  }
  expect_identical(cellchat_input_error_state(
    tryCatch(cellchat_input_from_matrix(m, c("G1", "G2"), c("A", "A", "B", "B")),
             error = function(e) e)
  ), "invalid_species")
  expect_identical(cellchat_input_error_state(
    tryCatch(cellchat_database_for_species(NULL), error = function(e) e)
  ), "invalid_species")
})

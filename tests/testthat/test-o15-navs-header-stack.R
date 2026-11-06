# test-o15-navs-header-stack.R — garde P0-17/18 (2026-11-06) : les navset_card
# titrés à nombreux onglets wrappent (UL 158 px ~3 rangées mesuré au DOM) et
# le titre du header se retrouve posé au milieu du bloc d'onglets. La règle
# O15 dans global.R empile : titre sur sa propre rangée, nav full-width
# dessous. Ancres ASCII volontaires (leçon readLines hors UTF-8 : f393f8a).

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

test_that("la règle d'empilement titre/onglets est présente dans le bloc O15 de global.R", {
  lines <- readLines(file.path(.f2_root, "global.R"), warn = FALSE)
  joined <- paste(lines, collapse = "\n")
  expect_true(grepl(".card-header.bslib-navs-card-title > span:first-child", joined, fixed = TRUE),
              label = "selecteur du titre (premier span du header navset titre)")
  expect_true(grepl(".card-header.bslib-navs-card-title > .nav", joined, fixed = TRUE),
              label = "selecteur de la nav (full-width sous le titre)")
  expect_true(grepl("flex: 0 0 100%", joined, fixed = TRUE),
              label = "le titre prend sa propre rangée (flex-basis 100%)")
  # La règle vit DANS le bloc bs_add_rules (une seule source de vérité de style, O15)
  block_start <- grep("bs_add_rules(my_theme", lines, fixed = TRUE)
  expect_true(length(block_start) >= 1L)
  rule_at <- grep("span:first-child", lines)
  expect_true(length(rule_at) >= 1L && rule_at[1] > block_start[1],
              label = "la règle est posée après bs_add_rules(my_theme (dans le bloc de règles)")
})

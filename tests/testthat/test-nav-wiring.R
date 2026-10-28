# =============================================================================
# test-nav-wiring.R — M7 : un `nav_select()` doit viser une valeur DÉCLARÉE
# =============================================================================
# Défaut mesuré sur `main` (2026-10-17) : cinq `nav_select(selected = "...")`
# désignaient un identifiant qui n'existe nulle part dans l'arbre des onglets.
# Les `nav_panel()` de ces trois modules ne portaient PAS de `value =`, donc
# leur valeur est leur TITRE (bslib : `nav_panel(title, ..., value = title)`),
# et ce titre est une chaîne traduite (`i18n$t("Heatmap")`, `"PCA"`, ...).
# Aucun `"sig_heatmap_tab"` ne peut donc correspondre : la navigation ne se
# faisait pas, silencieusement — l'utilisateur restait sur l'onglet courant
# après une action censée l'emmener ailleurs.
#
# Le motif CORRECT existait juste à côté : `mod_bulk_pathways.R:113` déclare
# `nav_panel(..., value = "tab_scores", ...)` et son `nav_select()` vise
# `"tab_scores"`. C'est le témoin que l'invariant ci-dessous est atteignable.
#
# Portée : tout `modules/**/*.R`. Les valeurs littérales visées par les
# importeurs (`"tab_bulk"`, `"tab_sc"`) sont déclarées dans `app.R`, qui porte le
# navset PARENT : la déclaration est donc cherchée dans le fichier lui-même ET
# dans `app.R`. Un `selected` non littéral (état réactif, variable) est hors
# portée : on ne peut pas le décider statiquement, et le prétendre serait une
# assertion qui mesure la forme du code, pas son câblage.
# =============================================================================

.nav_literal_arg <- function(expr, argname) {
  if (!is.call(expr)) return(NULL)
  l  <- tryCatch(as.list(expr), error = function(e) NULL)
  if (is.null(l)) return(NULL)
  nm <- names(l)
  if (is.null(nm)) return(NULL)
  hit <- NULL
  for (i in seq_along(l)) {                      # (1) par INDEX — helper-ast.R
    if (!identical(nm[i], argname)) next
    v <- tryCatch(l[[i]], error = function(e) NULL)
    if (is.character(v) && length(v) == 1L && nzchar(v)) hit <- v
  }
  hit
}

.nav_calls <- function(relpath, fun) {
  p <- ts_ast_parse(relpath)
  ts_ast_find_all(p, function(x) ts_ast_is_call_to(x, fun))
}

.nav_selected_literals <- function(relpath) {
  hits <- .nav_calls(relpath, "nav_select")
  out  <- lapply(hits, .nav_literal_arg, argname = "selected")
  unlist(Filter(Negate(is.null), out), use.names = FALSE)
}

.nav_declared_values <- function(relpath) {
  hits <- .nav_calls(relpath, "nav_panel")
  out  <- lapply(hits, .nav_literal_arg, argname = "value")
  unlist(Filter(Negate(is.null), out), use.names = FALSE)
}

.nav_module_files <- function() {
  root <- file.path(ts_project_root(), "modules")
  rel  <- list.files(root, pattern = "[.]R$", recursive = TRUE)
  file.path("modules", gsub("\\\\", "/", rel))
}

test_that("chaque nav_select() littéral vise une valeur réellement déclarée", {
  app_declared <- .nav_declared_values("app.R")

  files   <- .nav_module_files()
  expect_gt(length(files), 0L)

  offenders <- character(0)
  n_checked <- 0L
  for (f in files) {                             # (1) par INDEX
    sel <- .nav_selected_literals(f)
    if (length(sel) == 0L) next
    declared <- c(.nav_declared_values(f), app_declared)
    for (s in sel) {                             # (1) par INDEX
      n_checked <- n_checked + 1L
      if (!s %in% declared) {
        offenders <- c(offenders, sprintf("%s -> \"%s\"", f, s))
      }
    }
  }

  # Témoin de NON-VACUITÉ : si le parcours cessait de trouver des sites, le
  # test passerait pour la mauvaise raison (il n'aurait rien regardé).
  expect_gt(n_checked, 0L)

  expect_identical(offenders, character(0),
                   info = paste("cibles non déclarées :",
                                paste(offenders, collapse = " | ")))
})

test_that("l'invariant est ATTEIGNABLE : le motif correct existe (témoin)", {
  # Sans ce témoin, un `nav_panel()` qui ne porterait JAMAIS de `value =`
  # rendrait le test précédent impossible à satisfaire pour une raison qui
  # n'aurait rien à voir avec le défaut.
  expect_true("tab_scores" %in% .nav_declared_values("modules/bulk/mod_bulk_pathways.R"))
  expect_true("tab_scores" %in% .nav_selected_literals("modules/bulk/mod_bulk_pathways.R"))
  # ...et les cibles des importeurs sont déclarées par le navset PARENT.
  expect_true("tab_bulk" %in% .nav_declared_values("app.R"))
  expect_true("tab_sc"   %in% .nav_declared_values("app.R"))
})

# ---------------------------------------------------------------------------
# O14 (2026-10-27) : second CTA « Configurer design & QC » — coexistence
# ---------------------------------------------------------------------------
test_that("O14 : le CTA design & QC coexiste avec le CTA LOT 4A (ajout, pas remplacement)", {
  src <- paste(readLines(file.path(ts_project_root(), "modules/import/mod_import_bulk.R"),
                         warn = FALSE), collapse = "\n")
  expect_true(grepl('ns("goto_mapping")', src, fixed = TRUE))
  expect_true(grepl('ns("goto_design_qc")', src, fixed = TRUE))
  # Le CTA O14 ouvre LES DEUX panneaux visés (QC + design) — valeurs réelles
  # de l'accordéon bulk (mod_bulk.R:24-68).
  expect_true(grepl('values = c("panel_filter", "panel_de")', src, fixed = TRUE))
})

# ── B1-D (O21, 2026-10-28) : ordre de la sidebar bulk ────────────────────────
# La proposition V1.x (§2.2) demande la progression 3e → 3f → 3g → « 4.
# Livrables » ; l'arbre mesuré (dossier round 5) montre 3g APRÈS « 4.
# Livrables » et après les blocs Multi-jeux. Épingle l'ORDRE par le source.
test_that("B1-D: le panneau 3g. Réseau PCSF précède « 4. Livrables » dans la sidebar bulk", {
  lines <- readLines(file.path(ts_project_root(), "modules", "bulk", "mod_bulk.R"), warn = FALSE)
  pos_net <- grep('value = "panel_network"', lines)
  pos_rep <- grep('value = "panel_report"', lines)
  testthat::expect_length(pos_net, 1L)
  testthat::expect_length(pos_rep, 1L)
  testthat::expect_lt(pos_net, pos_rep)
})

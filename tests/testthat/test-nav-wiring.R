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

# ── B3-C (round 5, 2026-10-28) : couche présentation du verrouillage ─────────
# La moitié FONCTIONNELLE est déjà livrée côté calcul (gardes bulk_wgcna.R,
# bulk_survival.R, dose_response.R, bulk_batch_qc.R — hors périmètre ici, ZÉRO
# changement comportemental). Ces épingles portent la PRÉSENTATION :
#   (1) badge 🔒 sur les TITRES des trois accordion_panel GARDÉS de la sidebar
#       bulk, concaténé APRÈS l'appel i18n$t() — la clé de traduction elle-même
#       doit rester intacte (garde C7 exige l'existence des clés utilisées) ;
#   (2) empty state réactif sur l'onglet Power WGCNA : les 2 plotOutput
#       statiques rendaient une zone blanche silencieuse tant qu'aucune
#       analyse n'avait tourné (renderPlot req() sans message).
# NB : le caractère emoji est écrit dans CE garde via son échappement R
# "\U0001F512" pour rester ASCII pur (leçon C7 : un garde ne doit pas dépendre
# de la locale) ; il matche le caractère littéral 🔒 que readLines() rend
# depuis le fichier module (encodage UTF-8 natif depuis R 4.2).
.test_lock <- "\U0001F512"

test_that("B3-C: badge 🔒 sur les titres des panneaux gardés (wgcna, survie, dose)", {
  lines <- readLines(file.path(ts_project_root(), "modules", "bulk", "mod_bulk.R"),
                     warn = FALSE)
  locked <- c("panel_wgcna", "panel_survival", "panel_dose")
  n_checked <- 0L
  offenders <- character(0)
  for (v in locked) {
    pos <- grep(paste0('value = "', v, '"'), lines, fixed = TRUE)
    if (length(pos) != 1L) {
      offenders <- c(offenders,
                     sprintf("%s : %d occurrence(s) de value= (attendu 1)", v, length(pos)))
      next
    }
    n_checked <- n_checked + 1L
    ln <- lines[pos]
    key <- regmatches(ln, regexpr('i18n\\$t\\("[^"]*"\\)', ln))
    ok <- grepl("i18n$t(", ln, fixed = TRUE) &&
      grepl(.test_lock, ln, fixed = TRUE) &&
      length(key) == 1L &&
      !grepl(.test_lock, key, fixed = TRUE) &&   # la clé i18n reste INTACTE
      regexpr(.test_lock, ln, fixed = TRUE) >
        regexpr("i18n\\$t\\(", ln)               # badge APRÈS l'appel i18n$t()
    if (!isTRUE(ok)) offenders <- c(offenders, v)
  }
  # Témoin de non-vacuité : les 3 panneaux gardés doivent exister, sinon le
  # test passerait pour la mauvaise raison (rien regardé).
  testthat::expect_identical(n_checked, 3L)
  testthat::expect_identical(offenders, character(0),
                             info = paste("panneaux sans badge \U0001F512 concaténé après i18n$t() :",
                                          paste(offenders, collapse = " | ")))
})

test_that("B3-C: empty state réactif sur l'onglet Power WGCNA (renderUI, plus de blanc silencieux)", {
  src <- paste(readLines(file.path(ts_project_root(), "modules", "bulk", "mod_bulk_wgcna.R"),
                         warn = FALSE), collapse = "\n")
  # (1) les 2 plotOutput statiques cèdent la place à un conteneur conditionnel
  #     (l'identifiant wgcna_power_ui est celui nommé par le dossier round 5)
  testthat::expect_true(grepl('uiOutput(ns("wgcna_power_ui"))', src, fixed = TRUE),
                        info = "l'onglet Power doit porter uiOutput(ns(\"wgcna_power_ui\"))")
  # (2) le renderUI existe dans le serveur et DISTINGUE l'état vide
  testthat::expect_true(grepl("output$wgcna_power_ui <- renderUI(", src, fixed = TRUE),
                        info = "renderUI wgcna_power_ui absent du serveur du module")
  testthat::expect_true(grepl("is.null(shared_rv$wgcna_power)", src, fixed = TRUE),
                        info = "le renderUI ne teste pas l'état vide shared_rv$wgcna_power")
  # (3) la carte info guide vers l'étape 3c (message prescrit par le dossier)
  testthat::expect_true(grepl("pour alimenter ces graphiques", src, fixed = TRUE),
                        info = "message d'empty state absent")
  # (4) NON-RÉGRESSION : les renderPlot existants et leur garde req() restent
  #     inchangés — occurrences comptées sur la source COLLAPSÉE (length(grep())
  #     sur une chaîne unique ne compte pas les occurrences multiples).
  .count_occ <- function(needle) {
    lengths(regmatches(src, gregexpr(needle, src, fixed = TRUE)))
  }
  testthat::expect_identical(.count_occ("req(shared_rv$wgcna_power)"), 2L,
                             info = "req(shared_rv$wgcna_power) doit rester présent 2x (renderPlot)")
  testthat::expect_identical(.count_occ("output$wgcna_power_fit <- renderPlot"), 1L)
  testthat::expect_identical(.count_occ("output$wgcna_power_conn <- renderPlot"), 1L)
})

# ── B2-D (round 5, 2026-10-28) : sélecteur persistant de contraste actif ─────
# Dossier round 5 §2.8 : le sélecteur de contraste ne vivait qu'en sidebar
# Étape 2 (`active_contrast_view`, mod_bulk_de_ui.R) — changer de contraste
# depuis les onglets de visualisation (Volcano / MA-Plot / Heatmap) imposait
# l'aller-retour sidebar. Épingles readLines :
#   (1) UI : le selectInput sticky `active_contrast_sticky` (label
#       « Contraste actif », clé i18n PRÉEXISTANTE translation.json:8049 —
#       garde C7) existe, et la barre est EFFECTIVEMENT montée en tête des
#       onglets de visualisation (slot `header` du navset « Résultats Bulk
#       RNA-seq », mod_bulk.R) ;
#   (2) Serveur : synchro BIDIRECTIONNELLE dans mod_bulk_de_run.R — deux
#       observeEvent, chacun met à jour l'AUTRE contrôle, garde
#       `!identical(..., isolate(...))` pour casser la récursion ; la source
#       de vérité reste `active_contrast_view` (aucune écriture shared_rv ici).
# ZÉRO changement statistique : la synchro n'émet que des updateSelectInput.
.b2d_count_occ <- function(needle, src) {
  lengths(regmatches(src, gregexpr(needle, src, fixed = TRUE)))
}

test_that("B2-D: le sélecteur sticky active_contrast_sticky est câblé en tête des onglets de visualisation", {
  ui_src <- paste(readLines(file.path(ts_project_root(), "modules", "bulk_de",
                                      "mod_bulk_de_ui.R"), warn = FALSE), collapse = "\n")
  # (1) le selectInput sticky existe dans l'UI DE
  testthat::expect_identical(.b2d_count_occ('ns("active_contrast_sticky")', ui_src), 1L,
                             info = "selectInput sticky active_contrast_sticky absent de mod_bulk_de_ui.R")
  # (2) le label prescrit (clé i18n préexistante, pure ASCII)
  testthat::expect_true(grepl('i18n$t("Contraste actif")', ui_src, fixed = TRUE),
                        info = "le label « Contraste actif » est absent de la barre sticky")
  # (3) témoin de MONTAGE : la barre doit être instanciée dans mod_bulk.R,
  #     sinon le selectInput serait du code mort jamais rendu dans l'app.
  bulk_src <- paste(readLines(file.path(ts_project_root(), "modules", "bulk",
                                        "mod_bulk.R"), warn = FALSE), collapse = "\n")
  testthat::expect_true(grepl('mod_bulk_de_contrast_bar_ui(ns("de"))', bulk_src, fixed = TRUE),
                        info = "mod_bulk_de_contrast_bar_ui(ns(\"de\")) jamais montée dans mod_bulk.R")
})

test_that("B2-D: la synchro bidirectionnelle sticky <-> sidebar existe dans le serveur", {
  src <- paste(readLines(file.path(ts_project_root(), "modules", "bulk_de",
                                   "mod_bulk_de_run.R"), warn = FALSE), collapse = "\n")
  # (1) les DEUX sens de la synchro : un observeEvent par contrôle
  testthat::expect_identical(.b2d_count_occ("observeEvent(input$active_contrast_sticky", src), 1L,
                             info = "l'observateur sticky -> sidebar manque")
  testthat::expect_identical(.b2d_count_occ("observeEvent(input$active_contrast_view", src), 1L,
                             info = "l'observateur sidebar -> sticky manque")
  # (2) chaque sens écrit l'AUTRE contrôle (jamais le sien). Comptes mesurés :
  #     "active_contrast_view" était écrit 2x avant B2-D (.de_drive_commit +
  #     adhoc) ; la synchro sticky -> sidebar porte le total à 3.
  testthat::expect_identical(.b2d_count_occ('updateSelectInput(session, "active_contrast_sticky"', src), 1L,
                             info = "la synchro n'écrit jamais le contrôle sticky")
  testthat::expect_identical(.b2d_count_occ('updateSelectInput(session, "active_contrast_view"', src), 3L,
                             info = "le sélecteur sidebar doit être écrit 3x (2 commits DE préexistants + synchro sticky)")
  # (3) la garde != sur isolate() casse la récursion — présente dans les DEUX sens
  testthat::expect_identical(.b2d_count_occ("shiny::isolate(input$active_contrast_sticky)", src), 1L,
                             info = "garde isolate() absente du sens sidebar -> sticky")
  testthat::expect_identical(.b2d_count_occ("shiny::isolate(input$active_contrast_view)", src), 1L,
                             info = "garde isolate() absente du sens sticky -> sidebar")
  testthat::expect_identical(.b2d_count_occ("!identical(input$active_contrast_", src), 2L,
                             info = "la garde !identical doit exister dans les deux sens")
})

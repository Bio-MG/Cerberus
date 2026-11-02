# =============================================================================
# test-cqf-identity-guidance.R — CQF-2 (fiche
# SC_COMM_QUICKFIXES_PROPOSAL_2026-11-02 §3-CQF-2)
# =============================================================================
# Guidage annotation-first du sélecteur « Colonne d'identités cellulaires »
# (modules/sc/mod_sc_communication.R) :
#   1. POINT UNIQUE du motif d'annotation SingleR — helper pur
#      .singler_annotation_column_pattern() dans
#      tools/pkg_skeleton/R/sc_communication_input.R ; plus aucun littéral
#      "^SingleR_" dans modules/ (les 3 grep de mod_sc_annotation.R passent
#      par le helper) ;
#   2. choix GROUPÉS (optgroups natifs shiny, mesurés) : annotation EN TÊTE,
#      puis seurat_clusters, puis le reste — AUCUNE colonne retirée
#      (principe LIANA : proposées, jamais imposées) ;
#   3. INDICE PASSIF sous le sélecteur : colonne numérique ou préfixée
#      cluster -> message i18n, jamais bloquant.
#
# Harnais : pattern de test-mod-sc-identity-mapping.R (B5) — shims globalenv
# pour les calls UI non implémentés par MockShinySession, sous-modules
# contextuels no-op, updateSelectInput CAPTURÉ (sous testServer, les update*
# ne déplacent pas input$<id> ; on asserte ce que le SERVEUR envoie).
# =============================================================================

Sys.setenv(OMNIPATHR_LOGFILE = "none")

source_project_file("tools/pkg_skeleton/R/defaults.R")
source_project_file("tools/pkg_skeleton/R/io_helpers.R")
source_project_file("tools/pkg_skeleton/R/validation.R")
source_project_file("tools/pkg_skeleton/R/provenance.R")
source_project_file("tools/pkg_skeleton/R/error_state.R")
source_project_file("tools/pkg_skeleton/R/datatable.R")
source_project_file("tools/pkg_skeleton/R/sc_velocity.R")
source_project_file("tools/pkg_skeleton/R/sc_communication.R")
source_project_file("tools/pkg_skeleton/R/sc_communication_input.R")
source_project_file("tools/pkg_skeleton/R/sc_communication_views.R")
source_project_file("modules/sc/mod_sc_communication.R")

suppressPackageStartupMessages({
  library(shiny)
})

# ── Sous-modules contextuels HORS PÉRIMÈTRE CQF-2 : no-op (cf. B5) ──────────
assign("mod_sc_communication_spatial_server",
       function(...) invisible(NULL), envir = globalenv())
assign("mod_sc_communication_trajectory_server",
       function(...) invisible(NULL), envir = globalenv())
assign("mod_sc_communication_velocity_server",
       function(...) invisible(NULL), envir = globalenv())
assign("mod_sc_communication_perturbation_server",
       function(...) invisible(NULL), envir = globalenv())

# ── Shims : avales les calls UI non implémentés par MockShinySession ────────
# + CAPTURE des updateSelectInput (choices réellement envoyés par le serveur).
.cqf_install_shims <- function() {
  box <- new.env(parent = emptyenv())
  box$updates <- list()
  box$notifs <- list()
  olds <- lapply(c("showNotification", "showModal", "removeModal",
                   "updateSelectInput"), get0, envir = globalenv())
  assign("showNotification", function(ui, type = "default", duration = 5, ...) {
    box$notifs <- c(box$notifs, list(list(ui = as.character(ui), type = type)))
    invisible(NULL)
  }, envir = globalenv())
  assign("showModal", function(ui, ...) invisible(NULL), envir = globalenv())
  assign("removeModal", function(...) invisible(NULL), envir = globalenv())
  assign("updateSelectInput",
         function(session, inputId, label = NULL, choices = NULL,
                  selected = NULL, ...) {
           box$updates[[inputId]] <- list(choices = choices,
                                          selected = selected)
           invisible(NULL)
         }, envir = globalenv())
  list(box = box, restore = function() {
    for (i in seq_along(olds)) {
      nm <- c("showNotification", "showModal", "removeModal",
              "updateSelectInput")[i]
      if (is.null(olds[[i]])) rm(list = nm, envir = globalenv())
      else assign(nm, olds[[i]], envir = globalenv())
    }
  })
}

# ── Fixture : métadonnées COUVRANTES — annotation SingleR, clusters, ────────
# numérique, facteur neutre. 12 cellules, comptes triviaux.
.cqf_obj <- function() {
  cells <- paste0("C", 1:12)
  counts <- Matrix::Matrix(
    matrix(seq_len(10 * 12) %% 5L, nrow = 10L,
           dimnames = list(paste0("G", 1:10), cells)),
    sparse = TRUE
  )
  obj <- SeuratObject::CreateSeuratObject(
    counts = counts,
    meta.data = data.frame(
      SingleR_hpca_main = rep(c("DC", "Monocyte", "NK"), length.out = 12L),
      row.names = cells
    )
  )
  obj$seurat_clusters <- factor(rep(c(0L, 1L, 2L), length.out = 12L))
  obj$RNA_snn_res.0.5 <- factor(rep(c(0L, 1L, 2L), length.out = 12L))
  obj
}

.cqf_hint_key <- paste0(
  "Colonne de clusters \u2014 les paires seront num\u00e9rot\u00e9es. ",
  "Lancez l'annotation (SingleR) pour des types cellulaires nomm\u00e9s.")

# ═══ Niveau 1 — helpers purs (point unique + groupement + indice) ═══════════

test_that("CQF-2 : le motif SingleR est déclaré EN UN POINT UNIQUE", {
  expect_identical(.singler_annotation_column_pattern(), "^SingleR_")
  # Verrou modules/ : plus AUCUN littéral "^SingleR_" — les consommateurs
  # (3 grep de mod_sc_annotation.R) passent par le helper.
  mod_files <- list.files(file.path(ts_project_root(), "modules", "sc"),
                          pattern = "\\.R$", full.names = TRUE)
  hits <- 0L
  for (f in mod_files) {
    src <- paste(readLines(f, warn = FALSE), collapse = "\n")
    hits <- hits + lengths(regmatches(src, gregexpr('"\\^SingleR_"', src)))
  }
  expect_identical(hits, 0L,
                   info = "le motif d'annotation doit vivre dans le helper unique")
  # Les 3 sites consommateurs de mod_sc_annotation.R citent le helper.
  annot <- paste(readLines(file.path(ts_project_root(),
                                     "modules/sc/mod_sc_annotation.R"),
                           warn = FALSE), collapse = "\n")
  expect_identical(
    lengths(regmatches(annot,
                       gregexpr(".singler_annotation_column_pattern()",
                                annot, fixed = TRUE))),
    3L
  )
})

test_that("CQF-2 : groupement des choix — annotation, clusters, reste, RIEN de retiré", {
  cols <- c("nCount_RNA", "SingleR_hpca_main", "orig.ident",
            "seurat_clusters", "RNA_snn_res.0.5")
  g <- .comm_identity_choice_groups(cols)
  expect_identical(names(g),
                   c("Annotation (SingleR)", "Clusters", "Autres colonnes"))
  expect_identical(g[["Annotation (SingleR)"]], "SingleR_hpca_main")
  expect_identical(g[["Clusters"]], "seurat_clusters")
  # l'ordre du reste suit l'ordre des métadonnées (aucun tri implicite)
  expect_identical(g[["Autres colonnes"]],
                   c("nCount_RNA", "orig.ident", "RNA_snn_res.0.5"))
  # principe LIANA : AUCUNE colonne retirée, aucune dupliquée
  expect_setequal(unlist(g, use.names = FALSE), cols)
  expect_identical(length(unlist(g, use.names = FALSE)), length(cols))
  # sans annotation ni seurat_clusters : groupes absents (pas de libellé vide)
  g2 <- .comm_identity_choice_groups(c("a", "b"), "A", "C", "R")
  expect_identical(names(g2), "R")
  expect_identical(g2[["R"]], c("a", "b"))
  # libellés passés par l'appelant (i18n au site d'appel, pas dans le helper) ;
  # sans colonne « reste », PAS de groupe vide (pas d'optgroup fantôme)
  g3 <- .comm_identity_choice_groups(c("SingleR_x", "seurat_clusters"),
                                     "Ann", "Clu", "Aut")
  expect_identical(names(g3), c("Ann", "Clu"))
})

test_that("CQF-2 : détection cluster-like (numérique ou préfixée cluster)", {
  expect_true(.comm_identity_is_cluster_like("seurat_clusters", FALSE))
  expect_true(.comm_identity_is_cluster_like("RNA_snn_res.0.5", FALSE))
  expect_true(.comm_identity_is_cluster_like("integrated_snn_res.1", FALSE))
  expect_true(.comm_identity_is_cluster_like("nCount_RNA", TRUE))
  expect_false(.comm_identity_is_cluster_like("SingleR_hpca_main", FALSE))
  expect_false(.comm_identity_is_cluster_like("orig.ident", FALSE))
  expect_false(.comm_identity_is_cluster_like(NA_character_, TRUE))
  expect_false(.comm_identity_is_cluster_like(character(0), TRUE))
})

test_that("CQF-2 : clés i18n FR/EN présentes dans translation.json", {
  j <- jsonlite::fromJSON(file.path(ts_project_root(), "i18n",
                                    "translation.json"),
                          simplifyVector = FALSE)
  fr <- vapply(j$translation, function(x) x$fr %||% "", character(1))
  en <- vapply(j$translation, function(x) x$en %||% "", character(1))
  for (k in c("Annotation (SingleR)", "Autres colonnes", .cqf_hint_key)) {
    expect_true(k %in% fr, info = paste("clé FR manquante :", k))
    expect_true(nzchar(en[match(k, fr)]), info = paste("EN vide :", k))
  }
})

# ═══ Niveau 2 — verrou de câblage (pattern B3/B5 du dépôt) ══════════════════
# ⚠️ Limite MESURÉE (cf. B5) : sous MockShinySession (shiny 1.14), un
# observeEvent déclenché par une assignation reactiveValues ne s'exécute pas
# de façon déterministe — la capture des choices passés à updateSelectInput
# n'y est donc pas observable. Le GROUPAGE est verrouillé par (a) les tests
# purs ci-dessus sur .comm_identity_choice_groups() et (b) le verrou source
# ci-dessous ; l'ÉTAT du sélecteur en session réelle reste couvert par la
# vérification manuelle de la fiche §5.

test_that("CQF-2 verrou : l'observateur sert les choix groupés, défaut inchangé", {
  src <- paste(readLines(file.path(ts_project_root(),
                                   "modules/sc/mod_sc_communication.R"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # L'observateur sc_obj passe par le helper pur (pas de re-listage local) :
  expect_match(src, "choices = .comm_identity_choice_groups(", fixed = TRUE)
  # Les 3 libellés d'optgroup passent par .tr (i18n au site d'appel, C13) :
  expect_match(src, 'label_annot = .tr("Annotation (SingleR)")', fixed = TRUE)
  expect_match(src, 'label_clusters = .tr("Clusters")', fixed = TRUE)
  expect_match(src, 'label_rest = .tr("Autres colonnes")', fixed = TRUE)
  # Zéro changement de choix : le défaut reste seurat_clusters si présent.
  expect_match(src,
               "selected = if (\"seurat_clusters\" %in% meta_cols) \"seurat_clusters\" else meta_cols[1]",
               fixed = TRUE)
  # UI : l'indice passif est branché sous le sélecteur.
  expect_match(src, "textOutput(ns(\"comm_identity_hint\"))", fixed = TRUE)
})

# ═══ Niveau 3 — testServer : l'indice passif, bout en bout ══════════════════
# Les OUTPUTS se calculent À LA LECTURE sous MockShinySession (mesuré) :
# renderText est donc observable, contrairement aux update* (limite B5).

test_that("CQF-2 testServer : l'indice passif reflète la colonne sélectionnée", {
  obj <- .cqf_obj()
  global_data <- shiny::reactiveValues(sc_obj = NULL, i18n = NULL,
                                       language = "fr")
  shared_rv <- shiny::reactiveValues(communication_result = NULL)
  shims <- .cqf_install_shims()
  on.exit(shims$restore(), add = TRUE)

  testServer(mod_sc_communication_server,
             args = list(id = "communication", global_data = global_data,
                         shared_rv = shared_rv),
    {
      global_data$sc_obj <- obj

      # Colonne cluster (facteur seurat_clusters) -> message complet.
      session$setInputs(comm_identity_column = "seurat_clusters")
      expect_identical(output$comm_identity_hint, .cqf_hint_key)

      # Résultat d'annotation SingleR -> PAS d'indice (paires nommées).
      session$setInputs(comm_identity_column = "SingleR_hpca_main")
      expect_identical(output$comm_identity_hint, "")

      # Colonne numérique -> même indice (paires numérotées).
      session$setInputs(comm_identity_column = "nCount_RNA")
      expect_identical(output$comm_identity_hint, .cqf_hint_key)

      # Colonne cluster Seurat dérivée (_snn_res.) -> indice.
      session$setInputs(comm_identity_column = "RNA_snn_res.0.5")
      expect_identical(output$comm_identity_hint, .cqf_hint_key)

      # Facteur neutre (orig.ident) -> pas d'indice.
      session$setInputs(comm_identity_column = "orig.ident")
      expect_identical(output$comm_identity_hint, "")

      # Objet absent -> pas d'indice (jamais bloquant, jamais d'erreur).
      global_data$sc_obj <- NULL
      session$setInputs(comm_identity_column = "seurat_clusters")
      expect_identical(output$comm_identity_hint, "")
    })
})

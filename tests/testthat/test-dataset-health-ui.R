# =============================================================================
# test-dataset-health-ui.R — N5 dataset-doctor (satellites slice S2) : câblage
# additif du rapport de santé dans le module bulk import
# =============================================================================
# FICHE (fait foi) :
# docs/proposals/PROPOSAL_N_SATELLITES_LAUNCH_ROADMAP_2026-10-26.md
#   §slice S2 §2 : « Wire the report into the import UI as an additive panel
#   (French, action-oriented) ... DO NOT block import by default (warn +
#   one-click proceed) ». Brief jalon round8 : AC2.4 — warn + chemin d'import
#   utilisable immédiatement, JAMAIS de blocage (aucun stop()/validate()
#   nouveau sur le chemin d'import quand severity=warn) ; .warn_if_transformed_bulk
#   (O16, 2026-10-27) reste TEL QUEL ; zéro changement de comportement du
#   chemin d'import existant.
#
# LEÇON CQF-2 (motif test-dataset-dir-ui.R) : MockShinySession n'observe pas
# update* — privilégier les fonctions PURES (.dataset_health_html) assertables
# hors session + verrous STRUCTURELS sur le source. testServer sert au
# branchement réel (le rapport alimente reactiveVal + logs au site d'import).
#
# Portes (motif test-dataset-dir-ui.R:57-69 — asserted on the SOURCE so the
# test cannot survive the wiring deletion) :
#   * délégation branchée : le module appelle ts_dataset_health() ;
#   * O16 INTACT : .warn_if_transformed_bulk garde SES 3 sites d'appel
#     (880/1496/1656 au moment du jalon) et son corps inchangé ;
#   * AC2.4 : la fonction de rapport .dataset_health_report ne contient NI
#     stop() NI validate() ;
#   * rendu pur : .dataset_health_html — warn-only, titres via traducteur.
# =============================================================================

source_project_file("tools/pkg_skeleton/R/io_helpers.R")
source_project_file("tools/pkg_skeleton/R/rdata_io.R")
# state.R : state_get/state_set consommés par ts_drive_registry() au montage
# du module — MESURÉ (leçon test-mod-import-bulk.R:26-33).
source_project_file("tools/pkg_skeleton/R/state.R")
source_project_file("tools/pkg_skeleton/R/drive_allowlist.R")
source_project_file("tools/pkg_skeleton/R/drive_watcher.R")
source_project_file("modules/import/mod_rdata_picker.R")
source_project_file("tools/pkg_skeleton/R/bulk_import_engine.R")
source_project_file("tools/pkg_skeleton/R/session_checkpoint.R")
source_project_file("tools/pkg_skeleton/R/cerberus_handoff.R")
source_project_file("tools/pkg_skeleton/R/dataset_directory.R")
# Moteur du doctor + ses dépendances de délégation (O16 + vocabulaire)
source_project_file("tools/pkg_skeleton/R/defaults.R")
source_project_file("tools/pkg_skeleton/R/bulk_provenance.R")
source_project_file("tools/pkg_skeleton/R/batch_correction.R")
source_project_file("tools/pkg_skeleton/R/dataset_health.R")
source_project_file("modules/import/mod_import_bulk.R")

suppressPackageStartupMessages(library(shiny))

# ── Porte sur le SOURCE (motif test-dataset-dir-ui.R:57-69) ------------------
.MIB_MOD <- "modules/import/mod_import_bulk.R"
.mib_lines <- readLines(file.path(ts_project_root(), .MIB_MOD),
                        warn = FALSE, encoding = "UTF-8")
.mib_src <- paste(.mib_lines, collapse = "\n")
# Code sans commentaires (verrou sur le CODE — leçon mesurée test-dataset-health.R :
# un nom dans un commentaire d'en-tête fait passer un grep naïf).
.mib_code <- paste(sub("#.*$", "", sub("^\\s*#.*$", "", .mib_lines)),
                   collapse = "\n")

.mib_wired <- grepl("ts_dataset_health(", .mib_code, fixed = TRUE) &&
  grepl("dataset_health_ui", .mib_src, fixed = TRUE) &&
  grepl(".dataset_health_report(", .mib_code, fixed = TRUE)

.mib_red_info <- paste0(
  "câblage N5 dataset-doctor absent de ", .MIB_MOD,
  " : rapport de santé unifié (satellites slice S2 §2, AC2.4) non branché"
)

# Extraction du CORPS de .dataset_health_report (fonction interne du server)
# pour le verrou AC2.4 : ni stop() ni validate(). NA si absent (RED).
.mib_report_fn_text <- function(lines) {
  i0 <- grep("^    \\.dataset_health_report <- function", lines)
  if (length(i0) == 0L) return(NA_character_)
  i0 <- i0[1L]
  after <- which(grepl("^    }\\s*$", lines[-seq_len(i0)]))
  if (length(after) == 0L) return(NA_character_)
  paste(lines[i0:(i0 + after[1L])], collapse = "\n")
}

# ── Fixtures (motif test-dataset-health.R) -----------------------------------
.dhu_tpm_matrix <- function(genes = 2000L, samples = 4L, seed = 11) {
  set.seed(seed)
  n <- as.integer(genes) * as.integer(samples)
  small <- runif(n) < 0.05
  v <- ifelse(small, runif(n, 0.001, 0.9), runif(n, 10, 990))
  m <- matrix(v, nrow = as.integer(genes))
  dimnames(m) <- list(sprintf("ENSG%011d", seq_len(genes)),
                      paste0("S", seq_len(samples)))
  m
}

.dhu_raw_matrix <- function(genes = 200L, samples = 4L, seed = 12) {
  set.seed(seed)
  m <- matrix(rpois(as.integer(genes) * as.integer(samples), lambda = 200),
              nrow = as.integer(genes))
  rn <- sprintf("GENE%04d", seq_len(genes))
  rn[1L] <- "MT-CO1"; rn[2L] <- "MT-ND1"; rn[3L] <- "mt-cyb"
  dimnames(m) <- list(rn, paste0("S", seq_len(samples)))
  m
}

.dhu_env <- function() {
  gd <- new.env(parent = emptyenv())
  gd$i18n <- NULL
  gd$drive_registry <- new.env(parent = emptyenv())   # motif app.R:721
  gd
}

# ── T-0 : portes sur le source (délégation branchée, O16 intact, AC2.4) ------

test_that("T-0 : le rapport de santé est câblé dans mod_import_bulk.R (source)", {
  expect_true(.mib_wired, info = .mib_red_info)
})

test_that("T-0 : O16 .warn_if_transformed_bulk reste TEL QUEL (3 sites d'appel, corps inchangé)", {
  # gèle le signal préexistant : zéro changement de comportement du chemin d'import
  n_sites <- length(gregexpr("\\.warn_if_transformed_bulk\\(global_data\\$bulk_obj\\)",
                             .mib_src, perl = TRUE)[[1L]])
  expect_identical(n_sites, 3L,
                   info = "les 3 sites d'appel O16 (import drive, merged_matrix, per_sample) doivent rester")
  expect_true(grepl(".warn_if_transformed_bulk <- function(obj) {", .mib_src, fixed = TRUE),
              info = "la définition O16 doit rester telle quelle (délégation bulk_counts_transformed_signature + add_log + showNotification persistante)")
  # corps O16 : la notification persistante (duration = NULL) n'a pas bougé
  expect_true(grepl("duration = NULL", .mib_src, fixed = TRUE))
})

test_that("T-0 : AC2.4 — .dataset_health_report ne bloque JAMAIS (ni stop ni validate)", {
  fn <- .mib_report_fn_text(.mib_lines)
  expect_false(is.na(fn), info = paste0(
    ".dataset_health_report introuvable dans ", .MIB_MOD, " : ", .mib_red_info))
  if (is.na(fn)) return(invisible(NULL))
  expect_false(grepl("stop(", fn, fixed = TRUE),
               info = "AC2.4 : AUCUN stop() sur le chemin d'import quand severity=warn")
  expect_false(grepl("validate(", fn, fixed = TRUE),
               info = "AC2.4 : AUCUN validate() sur le chemin d'import quand severity=warn")
  expect_true(grepl("tryCatch", fn, fixed = TRUE),
               info = "un échec du doctor ne doit jamais casser l'import (rapport inert en cas d'erreur)")
})

# ── Rendu PUR (leçon CQF-2 : assertable sans session) ------------------------

test_that("rendu pur : .dataset_health_html — bloc warn-only, titre via traducteur, NULL si rien", {
  # RED-safe (règle error=0) : fonction absente => expectation en échec, jamais
  # une erreur « could not find function ».
  expect_true(exists(".dataset_health_html"), info = .mib_red_info)
  if (!exists(".dataset_health_html")) return(invisible(NULL))
  health <- ts_dataset_health(mat = .dhu_tpm_matrix())
  tag <- .dataset_health_html(health, tr = function(x) x)
  expect_false(is.null(tag), info = "un rapport warn produit un bloc visible")
  expect_identical(shiny::tagGetAttribute(tag, "class"), "alert alert-warning")
  html <- paste(capture.output(print(tag)), collapse = "\n")
  expect_match(html, "TPM", fixed = TRUE,
               info = "le libellé d'échelle (vocabulaire provenance) est rendu")
  expect_match(html, "counts bruts", fixed = TRUE,
               info = "le message FR actionnable délégué de l'O16 est rendu")
  expect_match(html, "Santé du dataset", fixed = TRUE,
               info = "le titre du bloc passe par le traducteur")

  # falsification live (coordinateur, 2026-11-03, commit 5904fa5) : le contenu
  # doit être des ENFANTS <li> du <ul>, jamais un attribut `.list` (texte
  # présent dans le HTML mais liste invisible à l'écran — le test texte seul
  # passait). Verrous structurels (leçon CQF-2).
  ul <- tag$children[[2]]
  expect_identical(ul$name, "ul", info = "2e enfant du bloc = <ul>")
  expect_null(ul$attribs$.list, info = "aucun attribut .list (bug falsifié live)")
  expect_true(length(ul$children) >= 1L, info = "les findings sont des enfants <li>")
  expect_identical(ul$children[[1]]$name, "li", info = "1er finding = <li>")
  expect_match(paste(capture.output(print(ul$children[[1]])), collapse = "\n"),
               "TPM", fixed = TRUE,
               info = "le libellé d'échelle vit DANS le <li> rendu")

  # rapport propre (counts bruts + MT présents) : AUCUN bloc (pas de bruit)
  clean <- ts_dataset_health(mat = .dhu_raw_matrix())
  expect_null(.dataset_health_html(clean, tr = function(x) x),
              info = "un rapport sans warn ne rend RIEN (bloc additif warn-only)")

  # entrées vides : jamais d'erreur (AC2.4)
  expect_null(.dataset_health_html(NULL, tr = function(x) x))
  expect_null(.dataset_health_html(list(findings = data.frame()), tr = function(x) x))
})

# ── Branchement réel (testServer — motif test-dataset-dir-ui.R T6) -----------

test_that("testServer : .dataset_health_report alimente reactiveVal + logs, sans bloquer (AC2.4)", {
  expect_true(.mib_wired, info = .mib_red_info)
  if (!.mib_wired) return(invisible(NULL))

  gd <- .dhu_env()
  shiny::testServer(mod_import_bulk_server, args = list(global_data = gd), expr = {
    # Ordre du harnais maison (test-dataset-dir-ui.R:137-145) : inputs d'option
    # AVANT le premier flush (counts_format NULL détruirait la session de test).
    session$setInputs(bulk_import_mode = "merged_matrix", counts_format = "rows",
                      counts_has_header = TRUE, counts_has_rownames = TRUE,
                      min_counts = 10, project_name = "P")
    session$flushReact()

    # matrice TPM-like injectée DIRECTEMENT dans global_data (l'import lui-même
    # est déjà couvert par test-mod-import-bulk / test-dataset-dir-ui) ;
    # le rapport est appelé via la fermeture server, comme aux sites d'import.
    global_data$bulk_obj <- list(
      counts = local({
        set.seed(11)
        n <- 2000L * 4L
        small <- runif(n) < 0.05
        v <- ifelse(small, runif(n, 0.001, 0.9), runif(n, 10, 990))
        m <- matrix(v, nrow = 2000L)
        dimnames(m) <- list(sprintf("ENSG%011d", seq_len(2000L)), paste0("S", 1:4))
        m
      }),
      metadata = data.frame(sample = paste0("S", 1:4), row.names = paste0("S", 1:4)),
      project = "P")
    .dataset_health_report(global_data$bulk_obj)
    session$flushReact()

    health <- shiny::isolate(ds_health_rv())
    expect_false(is.null(health), info = "le rapport est stocké (reactiveVal)")
    expect_identical(health$verdict, "warn")
    expect_true(any(health$findings$id == "scale"))
    expect_true(any(grepl("TPM", shiny::isolate(logs()), fixed = TRUE)),
                info = "les findings warn sont tracés dans le log opérateur")
    # le rendu UI est alimenté (bloc additif à côté de « Datasets découverts »)
    expect_true(!is.null(.dataset_health_html(health, tr = function(x) x)))

    # AC2.4 : un rapport sur un objet impropre ne casse RIEN (pas d'exception)
    .dataset_health_report(NULL)
    .dataset_health_report(list(counts = NULL))
    session$flushReact()
    expect_true(TRUE, info = "aucune exception levée sur entrées impropres")

    # AC2.4 proceed : le chemin d'import reste utilisable — un rapport clean ne
    # change aucun état d'import
    global_data$bulk_obj2 <- list(counts = local({
      set.seed(12)
      m <- matrix(rpois(200L * 4L, 200), nrow = 200L)
      rn <- sprintf("GENE%04d", seq_len(200L)); rn[1L] <- "MT-CO1"
      dimnames(m) <- list(rn, paste0("S", 1:4)); m
    }))
    .dataset_health_report(global_data$bulk_obj2)
    session$flushReact()
    h2 <- shiny::isolate(ds_health_rv())
    expect_identical(h2$verdict, "ok", info = "counts bruts propres => verdict ok, aucune anomalie")
  })
})

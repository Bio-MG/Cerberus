# =============================================================================
# test-mod-import-bulk.R — modules/import/mod_import_bulk.R
# =============================================================================
# Premier fichier de `modules/` converti au chantier de dette C10 (§2br) :
# **10** `stop()` non classés -> `stop(errorCondition(<msg>, class =
# "bulk_import_error"))`.
#
# Classe : `bulk_import_error`. Le dépôt qualifie déjà l'import par son OBJET
# (`rdata_import_error`, `communication_import_error`) ; le domaine ici est
# l'import **bulk**, et le nom s'aligne sur la famille `bulk_*` (`bulk_de_error`,
# `bulk_gsva_error`, `bulk_merge_error`) — `tools/pkg_skeleton/R/bulk_import_engine.R` pourra
# reprendre la même classe.
#
# ⚠️ PREUVE D'EXÉCUTION PARTIELLE, MESURÉE (pas supposée). Les 10 sites vivent
# DANS `mod_import_bulk_server` — le fichier n'expose que deux fonctions
# top-level, l'UI et le serveur. Et `counts_reactive()` enveloppe toute la
# lecture dans un `tryCatch(..., error = function(e) { add_log(); showNotification(); NULL })` :
# l'erreur est donc **avalée**, jamais propagée. Conséquence : la CLASSE n'est
# PAS observable à l'exécution (mesuré : appel de `shiny::isolate(counts_reactive())`
# avec un format non supporté ne lève rien).
#
# Mais le **MESSAGE** l'est, via `logs()` — et c'est exactement l'invariant de
# §2bn : `errorCondition(message, ...)` TRONQUE les arguments multiples, défaut
# que l'invariant *source* ne peut pas voir. D'où les deux niveaux :
#   1. verrou SOURCE (C10 = 0 sur le fichier) -> couvre les 10 sites ;
#   2. invariant de MESSAGE À L'EXÉCUTION sur les 3 sites joignables (403, 408
#      dans `smart_read` ; 476 dans `counts_reactive`).
# =============================================================================

source_project_file("tools/pkg_skeleton/R/io_helpers.R")
source_project_file("tools/pkg_skeleton/R/rdata_io.R")
# G3: `mod_import_bulk_server()` now also calls `ts_drive_publish_importer()`,
# on top of the G2 `ts_drive_bind_button()`. BOTH live in `R/core/`, and
# without these two sources this file only passed when another test file had
# already sourced them — i.e. it PASSED IN THE FULL SUITE AND ERRORED ALONE.
# MEASURED: filtered to `mod-import-bulk` alone, `moduleServer()` init raised
# `could not find function "ts_drive_bind_button"` (1 error); adding `drive`
# to the filter made it green, which is the signature of an order dependency.
# The module is not being tested in isolation if its dependencies are not
# loaded, so they are loaded here, explicitly.
source_project_file("tools/pkg_skeleton/R/drive_allowlist.R")
source_project_file("tools/pkg_skeleton/R/drive_watcher.R")
source_project_file("modules/import/mod_rdata_picker.R")
# Le module appelle l'ENGINE à l'exécution (`.infer_sample_id`,
# `.prepare_one_sample`, `.merge_per_sample_tables`) : `app.R` source les deux.
# MESURÉ : sans cette ligne, le bloc per_sample lève
# « could not find function ".infer_sample_id" » — un test qui ne charge pas
# ses dépendances ne teste pas le module, il teste son ordre de chargement.
source_project_file("tools/pkg_skeleton/R/bulk_import_engine.R")
source_project_file("modules/import/mod_import_bulk.R")
source_project_file("tools/check_conventions.R")

assign(".tr", function(key) key, envir = globalenv())
if (!exists(".t_fmt", envir = globalenv(), inherits = FALSE)) {
  assign(".t_fmt", function(template, ...) {
    vals <- list(...)
    for (nm in names(vals)) {
      template <- gsub(paste0("{", nm, "}"), format(vals[[nm]]), template, fixed = TRUE)
    }
    template
  }, envir = globalenv())
}
suppressPackageStartupMessages(library(shiny))

.mib_tmp <- tempfile("mib_")
dir.create(.mib_tmp, showWarnings = FALSE, recursive = TRUE)

# Fichier au format NON SUPPORTE : declenche smart_read (sites 403 puis 408).
.mib_bad <- file.path(.mib_tmp, "counts.unsupportedformat")
writeLines(c("a\tb", "1\t2"), .mib_bad)

# CSV a en-tete seul : 0 ligne de donnees -> « Matrice vide apres lecture » (476).
.mib_empty <- file.path(.mib_tmp, "counts_empty.csv")
writeLines("gene,S1", .mib_empty)

# ── Niveau 1 : verrou source ─────────────────────────────────────────────────
test_that("mod_import_bulk : 0 signalement C10 (verrou source, 10 sites)", {
  path <- file.path(ts_project_root(), "modules", "import", "mod_import_bulk.R")
  .REPORT$warns <- list()
  check_c10_error_style(path)
  flagged <- Filter(function(w) identical(w$rule, "C10"), .REPORT$warns)
  expect_identical(
    length(flagged), 0L,
    info = paste(vapply(flagged,
                        function(w) sprintf("%s:%s", .rel(w$file), w$line),
                        character(1)), collapse = ", ")
  )
})

# ── Niveau 2 : invariant de message a l'execution ────────────────────────────
# Le message est lu dans `logs()` : l'erreur est AVALEE par `counts_reactive`
# (cf. en-tête), donc on n'attend aucune classe ici — seulement que le texte
# n'ait PAS changé (garde-fou contre le défaut de troncature de §2bn).
# `testServer` ne rend pas de valeur : on passe par un collecteur.
.mib_last_logs <- NULL
.mib_collect <- function(datapath) {
  gd <- new.env(parent = emptyenv())
  gd$i18n <- NULL
  shiny::testServer(mod_import_bulk_server, args = list(global_data = gd), expr = {
    session$setInputs(bulk_import_mode = "merged_matrix", counts_format = "rows",
                      counts_has_header = TRUE, counts_has_rownames = TRUE,
                      min_counts = 10, project_name = "P")
    session$setInputs(counts_file = list(name = basename(datapath),
                                         size = 100L, datapath = datapath))
    session$flushReact()
    invisible(shiny::isolate(counts_reactive()))
    .mib_last_logs <<- paste(logs(), collapse = "\n")
  })
  .mib_last_logs
}

test_that("mod_import_bulk : messages d'erreur IDENTIQUES a l'execution", {
  # Site 403 (format non supporte) re-leve par le site 408 : le message final
  # concatene les deux. Un `errorCondition()` multi-arguments tronquerait ici.
  l1 <- .mib_collect(.mib_bad)
  expect_match(l1, "Erreur lecture counts: Erreur de lecture: Format de fichier non support",
               fixed = TRUE)

  # Site 476 : matrice lue mais vide.
  l2 <- .mib_collect(.mib_empty)
  expect_match(l2, "Erreur lecture counts: Matrice vide apr", fixed = TRUE)
})

# ── Niveau 3 : mode per_sample — collisions de basenames (P0 2026-10-21) ─────
# Défaut MESURÉ sur le jeu réel GSE52778 (16 dossiers GSMxxxx, chacun avec un
# `counts.tsv`) : `fileInput` ne transmet que le basename ⇒ 16 sample_id
# "counts" ⇒ l'import est REFUSÉ par la garde « sample_id dupliqués ». Pire :
# la métadata auto-générée préservait les éditions par `match(filename)`, or
# `match()` rend le MÊME index pour 16 lignes homonymes — une édition de la
# ligne 1 était donc recopiée sur les 16 (mesuré : unique(sample_id) restait à
# 1 après édition). L'utilisateur n'avait AUCUN moyen de sortir de l'impasse
# depuis l'interface.
.mib_ps_fixture <- function(n = 4L) {
  root <- file.path(.mib_tmp, "per_sample")
  dir.create(root, showWarnings = FALSE, recursive = TRUE)
  paths <- character(n)
  for (i in seq_len(n)) {
    d <- file.path(root, sprintf("S%d", i))
    dir.create(d, showWarnings = FALSE)
    p <- file.path(d, "counts.tsv")
    writeLines(c("gene_id\tcount", "G1\t3", "G2\t7", "G3\t0"), p)
    paths[i] <- p
  }
  paths
}

.mib_ps_state <- function(paths) {
  out <- new.env(parent = emptyenv())
  gd  <- new.env(parent = emptyenv())
  gd$i18n <- NULL
  shiny::testServer(mod_import_bulk_server, args = list(global_data = gd), expr = {
    session$setInputs(bulk_import_mode = "per_sample", project_name = "P",
                      ps_dup_threshold = 0.05, ps_fill_zero = TRUE,
                      btn_load = 0)
    session$setInputs(ps_files = data.frame(
      name = basename(paths), size = file.size(paths), type = "text/tsv",
      datapath = paths, stringsAsFactors = FALSE))
    session$flushReact()
    out$md1 <- rv_ps$metadata

    # Édition utilisateur : la 1re ligne seulement.
    x <- rv_ps$metadata
    x$sample_id[1] <- "EDITED"
    rv_ps$metadata <- x
    session$flushReact()
    out$md2 <- rv_ps$metadata

    session$setInputs(btn_load = 1)
    session$flushReact()
    obj <- tryCatch(shiny::isolate(gd$bulk_obj), error = function(e) NULL)
    out$loaded <- !is.null(obj)
    out$dim    <- tryCatch(dim(obj$counts), error = function(e) NULL)
    out$logs   <- paste(logs(), collapse = "\n")
  })
  out
}

test_that("mod_import_bulk (per_sample) : basenames homonymes ⇒ ids uniques, édition LOCALE, import possible", {
  paths <- .mib_ps_fixture(4L)
  st <- .mib_ps_state(paths)

  # 1. la collision est résolue AVANT l'import (sinon la garde bloque tout)
  expect_equal(nrow(st$md1), 4L)
  expect_equal(length(unique(st$md1$sample_id)), 4L)

  # 2. une édition de la ligne 1 ne touche QUE la ligne 1
  expect_identical(st$md2$sample_id[1], "EDITED")
  expect_equal(length(unique(st$md2$sample_id)), 4L)
  expect_identical(st$md2$sample_id[-1], st$md1$sample_id[-1])

  # 3. l'import aboutit réellement (effet observable, pas un `fail=0`)
  expect_true(st$loaded)
  expect_equal(st$dim[2], 4L)
})

# ── Niveau 4 : métadonnées fournies par FICHIER (même P0) ───────────────────
# Le complément indispensable du test précédent : rendre les ids uniques ne
# suffit pas, la `condition` restait NA (déduite d'un basename `counts.tsv` que
# `.infer_condition()` rejette) ⇒ DE désactivée. L'upload est aligné par ORDRE,
# seule clé disponible quand les basenames sont homonymes.
.mib_ps_meta_fixture <- function(n = 4L) {
  paths <- .mib_ps_fixture(n)
  meta <- file.path(.mib_tmp, "per_sample_meta.csv")
  writeLines(c("sample_id,condition,cell_line",
               "GSM1,A,L1", "GSM2,A,L1", "GSM3,B,L2", "GSM4,B,L2"), meta)
  list(files = paths, meta = meta)
}

test_that("mod_import_bulk (per_sample) : métadonnées uploadées ⇒ condition réelle, DE autorisée", {
  fx <- .mib_ps_meta_fixture(4L)
  out <- new.env(parent = emptyenv())
  gd  <- new.env(parent = emptyenv())
  gd$i18n <- NULL
  shiny::testServer(mod_import_bulk_server, args = list(global_data = gd), expr = {
    session$setInputs(bulk_import_mode = "per_sample", project_name = "P",
                      ps_dup_threshold = 0.05, ps_fill_zero = TRUE, btn_load = 0)
    session$setInputs(ps_files = data.frame(
      name = basename(fx$files), size = file.size(fx$files), type = "text/tsv",
      datapath = fx$files, stringsAsFactors = FALSE))
    session$setInputs(ps_metadata_file = list(
      name = basename(fx$meta), size = file.size(fx$meta), datapath = fx$meta))
    session$flushReact()

    out$md <- rv_ps$metadata

    session$setInputs(btn_load = 1)
    session$flushReact()
    obj <- tryCatch(shiny::isolate(gd$bulk_obj), error = function(e) NULL)
    out$loaded  <- !is.null(obj)
    out$cond    <- tryCatch(table(obj$metadata$condition), error = function(e) NULL)
    out$de_ok   <- tryCatch(isTRUE(obj$de_allowed), error = function(e) NA)
    out$meta_nm <- tryCatch(names(obj$metadata), error = function(e) NULL)
  })

  expect_equal(nrow(out$md), 4L)
  expect_identical(out$md$sample_id, c("GSM1", "GSM2", "GSM3", "GSM4"))
  expect_identical(out$md$condition, c("A", "A", "B", "B"))
  expect_true("cell_line" %in% names(out$md))      # colonne supplémentaire conservée

  expect_true(out$loaded)
  expect_equal(as.integer(out$cond[["A"]]), 2L)     # 2 réplicats par groupe
  expect_equal(as.integer(out$cond[["B"]]), 2L)
  expect_true(isTRUE(out$de_ok))                    # DE n'est plus désactivée
  expect_true("cell_line" %in% out$meta_nm)         # propagation dans bulk_obj
})



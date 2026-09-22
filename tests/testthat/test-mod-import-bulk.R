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
# `bulk_gsva_error`, `bulk_merge_error`) — `R/bulk/bulk_import_engine.R` pourra
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

source_project_file("R/core/io_helpers.R")
source_project_file("R/core/rdata_io.R")
# G3: `mod_import_bulk_server()` now also calls `ts_drive_publish_importer()`,
# on top of the G2 `ts_drive_bind_button()`. BOTH live in `R/core/`, and
# without these two sources this file only passed when another test file had
# already sourced them — i.e. it PASSED IN THE FULL SUITE AND ERRORED ALONE.
# MEASURED: filtered to `mod-import-bulk` alone, `moduleServer()` init raised
# `could not find function "ts_drive_bind_button"` (1 error); adding `drive`
# to the filter made it green, which is the signature of an order dependency.
# The module is not being tested in isolation if its dependencies are not
# loaded, so they are loaded here, explicitly.
source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")
source_project_file("modules/import/mod_rdata_picker.R")
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

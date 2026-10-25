# =============================================================================
# helper-source.R — auto-sourced by testthat before any test-*.R file runs
# =============================================================================
# TranscriptoShiny has no package structure AT THE REPO ROOT (the D2 package
# skeleton deliberately lives under tools/pkg_skeleton/ — a root DESCRIPTION
# would change project detection in renv/testthat/shinytest2, measured M3;
# see load_project_pkg() at the bottom of this file). This walks UP from the
# current working directory until it finds "app.R" to locate the project root.
# =============================================================================

.ts_find_project_root <- function(start = getwd(), marker = "app.R", max_up = 8) {
  d <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in seq_len(max_up)) {
    if (file.exists(file.path(d, marker))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  stop(
    "Impossible de localiser la racine du projet (fichier '", marker,
    "' introuvable en remontant depuis '", start, "').",
    call. = FALSE
  )
}

ts_project_root <- local({
  root <- NULL
  function() {
    if (is.null(root)) root <<- .ts_find_project_root()
    root
  }
})

#' Source a project file (relative to project root)
#' Handles both old-style top-level files and new R/ subdirectory files.
source_project_file <- function(relpath) {
  full <- file.path(ts_project_root(), relpath)
  if (!file.exists(full)) {
    stop("Fichier projet introuvable : ", full, call. = FALSE)
  }
  sys.source(full, envir = globalenv())
  invisible(full)
}

# Source config files first (if they exist — tests may run without them)
# D2 M5 lot 5 (2026-10-25) : defaults.R + thresholds.R vivent dans l'arbre du
# paquet (tools/pkg_skeleton/R/) — le bootstrap de test suit les chemins ;
# app.R les source() TOUJOURS en Bloc 02 (mêmes positions).
.local_source_if_exists <- function(relpath) {
  full <- file.path(ts_project_root(), relpath)
  if (file.exists(full)) sys.source(full, envir = globalenv())
}
.local_source_if_exists("tools/pkg_skeleton/R/defaults.R")
.local_source_if_exists("tools/pkg_skeleton/R/thresholds.R")

# §14.1 : `ts_error_state()` est appelé par les 11 accesseurs `*_error_state()`
# de tools/pkg_skeleton/R/ et R/sc/ (délégation d'une ligne). app.R le source dans le bloc
# CORE, donc AVANT tout domaine — le bootstrap de test doit refléter app.R.
# Sans cette ligne, un test qui source UN SEUL fichier de domaine (ex.
# test-bulk-network.R) perd l'accesseur et échoue pour une raison étrangère à
# son sujet. Les deux lignes ci-dessus suivent déjà ce motif pour config/.
# D2 M5 lot 5 : error_state.R vit désormais AUSSI dans l'arbre du paquet.
.local_source_if_exists("tools/pkg_skeleton/R/error_state.R")

# =============================================================================
# D2 M4 pilot — package-based loading for EXTRACTED domain files
# =============================================================================
# Extracted files keep ONE physical home: tools/pkg_skeleton/R/. app.R still
# source()s them into globalenv there (zero app behavior change); tests load
# the SAME physical files through the package namespace with this helper.
#
# Measured 2026-10-25 (M4 pilot, BEFORE any test migration):
#   - pkgload 1.5.3 is ALREADY in renv.lock and the project library (no new
#     dependency, nothing new to lock);
#   - pkgload 1.5.3 has NO `document` argument: load_all NEVER runs roxygen,
#     so it cannot rewrite NAMESPACE/ nor create man/ in the tree;
#   - load_all: 0.43 s; the namespace provides the symbols; attach
#     (export_all default) exposes them on the search path; globalenv stays
#     clean (exists(..., inherits = FALSE));
#   - a subprocess started at the repo root (renv auto-activation via
#     .Rprofile, same as the harness) loads the package fine.
load_project_pkg <- function(quiet = TRUE) {
  pkgload::load_all(file.path(ts_project_root(), "tools", "pkg_skeleton"),
                    quiet = quiet)
}

# R/core/memory.R — mesure de la RAM résidente du processus courant (jalon QW-1)
#
# Historique : l'indicateur de la barre d'état d'app.R divisait
# `sum(gc()[, 2])` par 1024, alors que `gc()[, 2]` est DÉJÀ exprimé en Mo —
# l'affichage était ~1024× trop petit, avec un gc() complet forcé dans le
# chemin de rendu (audit externe 2026-09-30, annexe ; re-mesuré sur l'arbre).
# Ce helper isole la mesure et teste helper-à-part (test-core-memory-rss.R).

#' Resident memory of the current R process, in MB
#'
#' Uses `ps::ps_memory_info()` (RSS du processus, paquet déjà au renv.lock).
#' Fallback sans `ps` : NA_real_ — l'appelant affiche alors le tas R seul
#' (`sum(gc(full = FALSE)[, 2])`, qui est déjà en Mo : NE PAS diviser).
#'
#' @return Numeric scalar (MB), or `NA_real_` when `ps` is unavailable.
ts_process_rss_mb <- function() {
  if (!requireNamespace("ps", quietly = TRUE)) return(NA_real_)
  v <- tryCatch(ps::ps_memory_info()[["rss"]], error = function(e) NA_real_)
  if (is.finite(v)) round(v / 1024^2) else NA_real_
}

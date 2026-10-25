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

# ── Préflight RAM (jalon M-4, 2026-09-30) ──────────────────────────────────
# Constat audité : l'app lançait des pipelines lourds sans AUCUNE lecture de la
# RAM disponible (audit externe J-8). Deux fonctions : la mesure système, puis
# la décision de budget — cette dernière est PURE (paramètres explicites) pour
# rester testable sans machine réelle. Ni l'une ni l'autre ne BLOQUE : le
# gouverneur complet exige un benchmark au préalable (MODE C, direction 2 ;
# risque documenté = le faux positif qui bloque une analyse légitime).

#' System RAM (total / available), in MB
#'
#' Uses `ps::ps_system_memory()` (paquet déjà au renv.lock). Repli sans `ps` :
#' les deux champs valent `NA_real_` — l'appelant doit traiter `NA` comme
#' « non mesurable ⇒ aucun blocage ».
#'
#' @return Named list `total_mb`, `available_mb` (numeric, possibly NA).
ts_system_ram_mb <- function() {
  if (!requireNamespace("ps", quietly = TRUE)) {
    return(list(total_mb = NA_real_, available_mb = NA_real_))
  }
  m <- tryCatch(ps::ps_system_memory(), error = function(e) NULL)
  if (is.null(m) || is.null(m[["total"]]) || is.null(m[["avail"]])) {
    return(list(total_mb = NA_real_, available_mb = NA_real_))
  }
  list(total_mb = round(m[["total"]] / 1024^2),
       available_mb = round(m[["avail"]] / 1024^2))
}

#' Budget RAM d'un objet lourd avant un pipeline (décision PURE)
#'
#' Projette la consommation d'un objet (`object_bytes` × `factor`) et la
#' compare à la RAM disponible/totale passées en paramètres explicites.
#'
#' @param object_bytes Numeric scalar, taille de l'objet en octets
#'   (ex. `as.numeric(object.size(obj))`).
#' @param factor Facteur de projection (copies internes, travail) — constante
#'   config `TS_RAM_PREFLIGHT_FACTOR`, VOLONTAIREMENT à calibrer par benchmark.
#' @param available_mb,total_mb RAM système mesurée (`NA` = non mesurable).
#' @return List `level` ("none"|"warn"|"block"), `projected_mb`, `available_mb`,
#'   `total_mb`, `message` (français, `""` si "none"). NE LÈVE JAMAIS.
ts_ram_budget_check <- function(object_bytes, factor = 3,
                                available_mb = NA_real_, total_mb = NA_real_) {
  object_mb <- as.numeric(object_bytes) / 1024^2
  projected_mb <- round(object_mb * factor)
  out <- list(level = "none", projected_mb = projected_mb,
              available_mb = available_mb, total_mb = total_mb, message = "")
  if (is.na(available_mb) || is.na(total_mb)) return(out)
  if (projected_mb > total_mb) {
    out$level <- "block"
    out$message <- sprintf(
      paste0("🔴 RAM : l'objet (~%.0f Mo) projeté à ~%.0f Mo (facteur %.0f) ",
             "dépasse la RAM totale (%.0f Mo) — l'exécution échouera très ",
             "probablement. Utilisez le sketch / BPCells ou un objet plus petit."),
      object_mb, projected_mb, factor, total_mb)
  } else if (projected_mb > available_mb) {
    out$level <- "warn"
    out$message <- sprintf(
      paste0("⚠️ RAM : l'objet (~%.0f Mo) projeté à ~%.0f Mo (facteur %.0f) ",
             "dépasse la RAM disponible (%.0f Mo sur %.0f Mo) — risque d'échec ",
             "mémoire élevé."),
      object_mb, projected_mb, factor, available_mb, total_mb)
  }
  out
}

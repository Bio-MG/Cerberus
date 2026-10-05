# =============================================================================
# mod_bulk_wgcna_export.R — Slice 4 : la route d'export drive pour WGCNA
# =============================================================================
# UNE route pour UN artefact : la table gene -> module que le telechargement
# humain `dl_wgcna_genes` ecrit (mod_bulk_wgcna.R) via `build_wgcna_export()`
# (R/bulk/bulk_wgcna.R). L'exporteur appelle le MEME builder, et le contrat de
# forme du builder (`type == "bulk_wgcna_modules"`) est le verdict canonique —
# un objet stocke non canonique devient un verdict `invalid` honnete.
#
# Les artefacts REFUSES par decision produit (2026-10-05) : la TOM (matrice
# 5000 x 5000 — classe de la garde memoire, et aucun telechargement humain ne
# l'expose), le RDS (objets R arbitraires — pas un artefact lisible par le wire),
# le dendrogramme (objet graphique base, pas de forme tabulaire sans un second
# builder). Une route sans telechargement humain serait un second builder sous
# un autre nom.
#
# Le nom de fichier humain embarque `Sys.Date()` — une valeur de session. Le
# nom de fichier drive N'EMBARQUE RIEN, sur la regle S2 : le stem est une donnee
# declaree (`TS_DRIVE_EXPORT_STEM_BULK_WGCNA`).
#
# Renvoie un VERDICT, jamais une exception. `status = "invalid"` signifie « cette
# session n'a rien a exporter » ; `status = "error"` signifie que l'ecriture a
# echoue. `trait_cor` est un parametre, pas une lecture interne : le module passe
# sa reactive isolee (`wgcna_trait_cor()`), les tests hors ligne passent un
# double direct — le builder est le seul a decider si la colonne conditionnelle
# `trait_cor_method` existe.
# =============================================================================

#' The next unused export path under `dir`, derived from what is already there.
#'
#' The `bulk_wgcna` twin of `bulk_pattern_export_next_path()`:
#' `TS_DRIVE_EXPORT_STEM_BULK_WGCNA` plus an index one above the highest
#' present. Stateless on purpose.
bulk_wgcna_export_next_path <- function(dir) {
  stem <- TS_DRIVE_EXPORT_STEM_BULK_WGCNA
  present <- list.files(dir, pattern = paste0("^", stem, "_[0-9]+\\.csv$"))
  idx <- suppressWarnings(as.integer(sub(paste0("^", stem, "_"), "",
                                         sub("\\.csv$", "", present))))
  idx <- idx[!is.na(idx)]
  n <- if (length(idx)) max(idx) + 1L else 1L
  file.path(dir, sprintf("%s_%d.csv", stem, n))
}

#' Export the gene -> module table through the drive route.
#'
#' @param shared_rv The module's reactive values (read via `isolate()` — the
#'   poller calls this from outside any reactive context).
#' @param global_data The app-wide reactiveValues (unused today, kept for the
#'   exporter signature the seam documents).
#' @param dir Destination override — for TESTS only; the poller never passes it,
#'   and the route then writes into `ts_drive_export_dir()`.
#' @param trait_cor The MEs <-> traits correlation result, or NULL. The MODULE
#'   passes `shiny::isolate()` of its own debounced reactive; offline tests pass
#'   a value directly. NULL is a valid state (no traits correlated yet), and the
#'   builder then omits the conditional `trait_cor_method` column.
#' @return list(ok, status, errors, descriptor)
bulk_wgcna_export_genes_csv <- function(shared_rv, global_data, dir = NULL,
                                        trait_cor = NULL) {
  bad <- function(status, msg) {
    list(ok = FALSE, status = status, errors = msg, descriptor = NULL)
  }
  res <- tryCatch(
    shiny::isolate(shared_rv$wgcna_modules),
    error = function(e) NULL
  )
  if (is.null(res)) {
    return(bad("invalid",
      paste("this session has built no WGCNA modules yet; run",
            "bulk-wgcna-run_wgcna_modules (or the panel's own button) first")))
  }
  # The builder owns the canonical-shape verdict (`type == "bulk_wgcna_modules"`).
  df <- tryCatch(build_wgcna_export(res, trait_cor), error = function(e) {
    bad("invalid", paste("the stored WGCNA result is not canonical:",
                         conditionMessage(e)))
  })
  if (!is.data.frame(df)) return(df) # the mapped `invalid` verdict
  ts_drive_export_write_table(df, bulk_wgcna_export_next_path, dir)
}

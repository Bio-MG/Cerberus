# =============================================================================
# test-mod-sc-annotation.R — modules/sc/mod_sc_annotation.R
# =============================================================================
# Cinquième fichier de `modules/` converti au chantier de dette C10 (§2bs,
# §2bt, §2bu, §2bv) : **4** `stop()` non classés -> `stop(errorCondition(<msg>,
# class = "sc_annotation_error"))`.
#
# Classe : `sc_annotation_error`. Qualifiée par le **domaine** (annotation
# SingleR côté SC), dans la famille `sc_*` : `sc_import_error` (§2bu),
# `sc_pseudobulk_error` (§2bv), `sc_helpers_error` (§2bq), `sc_multi_error`.
#
# 🎯 **Ce lot a été CHOISI par le critère de §2bv.2** : c'est le **seul** des 13
# fichiers de `modules/` restants dont **tous** les sites vivent dans des
# **helpers purs** (`.load_ref`, `.run_singler_safe`) — les 14 autres sites
# sont dans des serveurs réactifs, donc **inobservables** (§2bv.3).
#
# ⚠️ **2 sites joignables sur 4**, MESURÉ — et les deux non joignables le sont
# par **absence du défaut**, pas par complexité :
#   - **91** (« installez org.Hs.eg.db / org.Mm.eg.db ») est la branche `else`
#     de `requireNamespace(AnnotationDbi) && requireNamespace(orgdb_pkg)` — or
#     **les trois sont installés** (mesuré) ⇒ elle ne s'exécute jamais ;
#   - **144** (« 0 gène commun ») exige `.load_ref()`, donc un **téléchargement
#     celldex** (ExperimentHub) ⇒ **réseau**. Non testé volontairement : un test
#     réseau pendrait (le dépôt « live-gate » déjà le smoke GEO, §2ba).
#
# 🟢 Les 2 sites joignables le sont **sans `testServer()`** :
#   - **21**  : `.load_ref("bogus")` — instantané ;
#   - **108** : `.run_singler_safe()` sur un objet **> 100 000 cellules**
#     (`.annot_is_big()` = `ncol(obj) > 100000L`). Une `dgCMatrix` **creuse** de
#     100 001 colonnes ne coûte que ~1,1 s et quelques Mo : le seuil se franchit
#     **sans** construire un gros jeu de données.
# =============================================================================

source_project_file("R/core/io_helpers.R")        # %||%, detect_*_from_ids
source_project_file("R/sc/sc_bpcells.R")          # sc_backend_status
suppressPackageStartupMessages({
  library(Seurat)      # CreateSeuratObject
  library(Matrix)      # sparseMatrix
  library(shiny)       # le module est réactif
})
source_project_file("modules/sc/mod_sc_annotation.R")
source_project_file("tools/check_conventions.R")

# ── Fixture : objet « big » (> 100000 cellules) mais CREUX ⇒ ~1 s ────────────
.ann_big_obj <- function() {
  cnt <- Matrix::sparseMatrix(i = integer(0), j = integer(0),
                              dims = c(3L, 100001L),
                              dimnames = list(c("G1", "G2", "G3"), NULL))
  colnames(cnt) <- paste0("c", seq_len(100001L))
  SeuratObject::CreateSeuratObject(counts = cnt)
}

# Capture message + classe : la classe est OBSERVABLE ici (helpers purs, §2bv.2).
.ann_catch <- function(expr) {
  tryCatch({ expr; NULL },
           error = function(e) list(msg = conditionMessage(e), class = class(e)))
}

# ── Niveau 1 : verrou source (couvre les 4 sites) ────────────────────────────
test_that("mod_sc_annotation : 0 signalement C10 (verrou source, couvre les 4 sites)", {
  path <- file.path(ts_project_root(), "modules", "sc", "mod_sc_annotation.R")
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

# ── Niveau 2 : invariants d'exécution + classe (2 sites joignables) ──────────
test_that("mod_sc_annotation : messages IDENTIQUES et classe observable à l'exécution", {
  # site 21 — référence inconnue (switch -> dernière branche)
  e21 <- .ann_catch(.load_ref("bogus"))
  expect_identical(e21$msg, "Unknown reference: bogus")
  expect_true("sc_annotation_error" %in% e21$class)

  # site 108 — chemin « gros jeu » sans seurat_clusters
  obj <- .ann_big_obj()
  expect_true(.annot_is_big(obj))            # garde-fou : le seuil est bien franchi
  e108 <- suppressWarnings(.ann_catch(.run_singler_safe(obj, "hpca", "main")))
  expect_identical(e108$msg, "Lancez le clustering (Pipeline) avant l'annotation.")
  expect_true("sc_annotation_error" %in% e108$class)
})

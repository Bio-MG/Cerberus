# =============================================================================
# test-mod-sc-pseudobulk.R — modules/sc/mod_sc_pseudobulk.R
# =============================================================================
# Quatrième fichier de `modules/` converti au chantier de dette C10 (§2bs,
# §2bt, §2bu) : **6** `stop()` non classés -> `stop(errorCondition(<msg>,
# class = "sc_pseudobulk_error"))`.
#
# Classe : `sc_pseudobulk_error`. Qualifiée par le **domaine** (pseudobulk côté
# SC), dans la famille `sc_*` déjà présente : `sc_import_error` (§2bu),
# `sc_helpers_error` (§2bq), `sc_multi_error`.
#
# ⚠️ Le fichier porte **7** `stop(` pour **6** signalements C10 : la ligne
# **149** n'est qu'un **commentaire** (roxygen) qui mentionne `stop()`.
#   ⇒ Dénombrer n'est PAS greper le token (§2bu.4).
#
# 🟢 PREUVE D'EXÉCUTION : **4 sites sur 6** sont joignables **en R pur** —
#   ce fichier expose deux fonctions **top-level PURES**
#   (`aggregate_pseudobulk_counts`, `resolve_pseudobulk_condition`), contrairement
#   aux lots précédents de `modules/` qui n'exposent que UI + serveur.
#   ⇒ la **CLASSE est OBSERVABLE** ici (`tryCatch(error = function(e) class(e))`),
#   ce qui est une PREMIÈRE sur le front `modules/` (§2bs : erreurs avalées par
#   un `tryCatch` de réactif ⇒ classe invisible).
#
# 🔴 **2 sites sur 6 restent INOBSERVABLES** (309, 316), MESURÉ :
#   - ils vivent dans `observeEvent(input$run_aggregate)` ;
#   - leur `tryCatch(..., error =)` n'appelle **pas** `add_log` — il écrit dans
#     un `reactiveVal` (`agg_status_rv`) + `showNotification` ;
#   - et **`session$getOutput()` ne fonctionne PAS** dans cet environnement
#     (éprouvé sur un module trivial : « output$txt hasn't been defined yet »).
#   ⇒ ces 2 sites sont couverts par le **verrou source seul**.
#
# Le site **106** est le **décisif** : c'est le seul `stop()` **multi-arguments**
# du lot. Sans `paste0()`, `errorCondition()` le tronquerait à
# `"Moins de 2 groupes (echantillon"` (§2bn/§2bo) — l'assertion porte donc sur le
# **bout du message** (`"cellules -- pseudobulk impossible."`), qui disparaît.
# =============================================================================

source_project_file("R/core/io_helpers.R")        # %||%
suppressPackageStartupMessages({
  library(Seurat)      # CreateSeuratObject, LayerData
  library(Matrix)      # sparseMatrix
  library(shiny)       # le module est réactif
})
source_project_file("modules/sc/mod_sc_pseudobulk.R")
source_project_file("tools/check_conventions.R")

# ── Fixture : Seurat minimal, 30 cellules / 3 gènes / 3 échantillons ─────────
.pb_obj <- function(cond = rep(c("A", "A", "B"), each = 10)) {
  set.seed(1)
  cnt <- Matrix::sparseMatrix(
    i = rep(1:3, 30), j = rep(1:30, each = 3),
    x = as.integer(sample(0:5, 90, replace = TRUE)), dims = c(3L, 30L)
  )
  dimnames(cnt) <- list(paste0("g", 1:3), paste0("c", 1:30))
  md <- data.frame(row.names = paste0("c", 1:30),
                   sampleid = rep(c("S1", "S2", "S3"), each = 10),
                   cond = cond, stringsAsFactors = FALSE)
  SeuratObject::CreateSeuratObject(counts = cnt, meta.data = md)
}

# Capture message + classe : la classe est OBSERVABLE ici (cf. en-tête).
.pb_catch <- function(expr) {
  tryCatch({ expr; NULL },
           error = function(e) list(msg = conditionMessage(e), class = class(e)))
}

# ── Niveau 1 : verrou source (couvre les 6 sites) ────────────────────────────
test_that("mod_sc_pseudobulk : 0 signalement C10 (verrou source, couvre les 6 sites)", {
  path <- file.path(ts_project_root(), "modules", "sc", "mod_sc_pseudobulk.R")
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

# ── Niveau 2 : invariants d'exécution + classe (4 sites joignables) ──────────
test_that("mod_sc_pseudobulk : messages IDENTIQUES et classe observable à l'exécution", {
  obj <- .pb_obj()

  # site 79 — sample_col absent
  e79 <- .pb_catch(aggregate_pseudobulk_counts(obj, sample_col = "nope"))
  expect_identical(e79$msg, "Colonne d'echantillon 'nope' introuvable dans les metadonnees.")
  expect_true("sc_pseudobulk_error" %in% e79$class)

  # site 92 — split_by absent
  e92 <- .pb_catch(aggregate_pseudobulk_counts(obj, sample_col = "sampleid", split_by = "nope"))
  expect_identical(e92$msg, "Colonne de regroupement 'nope' introuvable dans les metadonnees.")
  expect_true("sc_pseudobulk_error" %in% e92$class)

  # site 106 — MOINS DE 2 GROUPES : le seul multi-arguments du lot (C16)
  e106 <- .pb_catch(aggregate_pseudobulk_counts(obj, sample_col = "sampleid",
                                                min_cells_per_group = 1000L))
  expect_identical(
    e106$msg,
    "Moins de 2 groupes (echantillon) avec au moins 1000 cellules -- pseudobulk impossible."
  )
  expect_true("sc_pseudobulk_error" %in% e106$class)

  # site 154 — condition_col absent
  pb <- aggregate_pseudobulk_counts(obj, sample_col = "sampleid")
  e154 <- .pb_catch(resolve_pseudobulk_condition(obj, pb$metadata, "sampleid", "nope"))
  expect_identical(e154$msg, "Colonne de condition 'nope' introuvable dans les metadonnees.")
  expect_true("sc_pseudobulk_error" %in% e154$class)
})

# ── Témoin : le chemin nominal ne doit PAS lever ─────────────────────────────
test_that("mod_sc_pseudobulk : le chemin nominal agrège sans erreur", {
  obj <- .pb_obj()
  res <- aggregate_pseudobulk_counts(obj, sample_col = "sampleid")
  expect_identical(ncol(res$counts), 3L)
  cond <- resolve_pseudobulk_condition(obj, res$metadata, "sampleid", "cond")
  expect_identical(length(cond$inconsistent), 0L)
})

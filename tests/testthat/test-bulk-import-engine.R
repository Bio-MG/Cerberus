# =============================================================================
# test-bulk-import-engine.R — tests for tools/pkg_skeleton/R/bulk_import_engine.R
# =============================================================================
# 23ᵉ incrément de la dette de conventions (2026-09-18).
# Créé AVANT la conversion (règle 5 : test d'abord, rouge d'abord) ⇒ ce
# fichier doit ÉCHOUER sur les assertions de classe tant que le `stop()` ne
# porte pas `class = "bulk_import_engine_error"`.
#
# 🟢 Lot choisi par RENDEMENT × PREUVE : c'est le **DERNIER lot de `R/`** qui
# paie **C10 + C9** (−2). Le fichier n'avait AUCUN test éponyme (C9) et ses
# helpers purs n'étaient couverts par aucun test existant
# (`grep` : 0 fichier pour `.read_per_sample_file`, `.prepare_one_sample`,
# `.infer_sample_id`, `.validate_design`) ⇒ test écrit de zéro.
# ⚠️ `test-mod-import-bulk.R` MENTIONNE `bulk_import_engine` mais ne le couvre
# pas seul (§2cc.1, §2cd.4, §2cf.2, §2cg.2 : 4ᵉ confirmation du motif).
#
# ⚠️ **1 seul token `stop(` pour 1 site** (mesuré : `grep -n "stop(" ` → 1
# occurrence, ligne 29) — pas de piège de dénombrement ici.
#
# 🟢 **Le site 29 est JOIGNABLE SANS AUCUN MOCK** (§2cg.5) : c'est le
# gestionnaire `error =` d'un `tryCatch()` qui **RELANCE**
# (`error = function(e) stop(sprintf(...))`). Un simple **chemin absent**
# suffit à le traverser. C'est l'opposé du cas §2cd.2 (gestionnaire qui
# **retourne** une valeur ⇒ avale l'erreur) et du cas §2bs (réactif qui
# avale) : ici le gestionnaire **propage**.
#
# ⚠️ **Piège de sonde mesuré (§2cg.5)** : avec un nom en **`.gz`**, l'échec
# survient à l'ouverture de la connexion (`gzfile()`), donc SANS le préfixe
# `"Lecture … "` — le gestionnaire n'est pas traversé. Le test utilise donc un
# nom **non-`.gz`**.
#
# ⚠️ **Queue de message VOLATILE** (§2by.3) : le message est
# `sprintf("Lecture %s : %s", filename, e$message)` — la seconde moitié vient de
# `readr` et peut changer selon la version ou l'OS. On assère donc le
# **PRÉFIXE** *et* une **longueur STRICTEMENT supérieure** : assérer un préfixe
# seul ne verrait pas une troncature (§2bx.3, §2cd).
#
# 🟢 **Contrôle de BORNE** (§2cf.1, §2cg.2) : un second test franchit le
# gestionnaire avec un VRAI fichier lisible et vérifie qu'aucune erreur n'est
# levée. Sans lui, un `stop()` inconditionnel placé dans le gestionnaire
# passerait le test d'erreur sans rien prouver.
# =============================================================================

source_project_file("tools/pkg_skeleton/R/bulk_import_engine.R")

.bie_err <- function(expr) {
  tryCatch({ expr; NULL },
           error = function(e) list(msg = conditionMessage(e), class = class(e)))
}

.PREFIX_S1 <- "Lecture s1.tsv : "

# ---------------------------------------------------------------------------
# .read_per_sample_file() — site 29, gestionnaire de tryCatch qui RELANCE
# ---------------------------------------------------------------------------
test_that(".read_per_sample_file : fichier illisible ⇒ classe (29)", {
  # Message à UN SEUL argument positionnel (`sprintf(...)`) ⇒ **aucun
  # `paste0()`** (§2bw.4 ; C16 ne concerne que les `stop()` multi-arguments).
  missing_path <- file.path(tempdir(), "bulk_import_engine_absent.tsv")
  if (file.exists(missing_path)) unlink(missing_path)
  expect_false(file.exists(missing_path))     # précondition du test

  # `suppressWarnings()` : l'erreur provoquée peut s'accompagner d'un
  # avertissement readr, que testthat compterait (§2ce).
  res <- .bie_err(suppressWarnings(
    .read_per_sample_file(path = missing_path, filename = "s1.tsv")
  ))

  expect_false(is.null(res))                                  # erreur levée…
  expect_true("bulk_import_engine_error" %in% res$class)      # …avec NOTRE classe
  expect_true(startsWith(res$msg, .PREFIX_S1))                # préfixe exact
  # Queue volatile : on exige une longueur STRICTEMENT supérieure au préfixe,
  # sans figer le texte de `readr` (§2by.3).
  expect_gt(nchar(res$msg), nchar(.PREFIX_S1))
})

# ---------------------------------------------------------------------------
# Contrôle de BORNE — le gestionnaire n'échoue PAS quand la lecture réussit
# ---------------------------------------------------------------------------
test_that(".read_per_sample_file : lecture nominale ⇒ aucune erreur", {
  skip_if_not_installed("readr")
  tmp <- tempfile(fileext = ".tsv")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c("gene_id\tcount", "G1\t3", "G2\t7"), tmp)

  df <- .read_per_sample_file(path = tmp, filename = "s1.tsv")

  expect_s3_class(df, "data.frame")            # pas d'erreur ⇒ objet rendu
  expect_equal(nrow(df), 2L)
  expect_true(all(c("gene_id", "count") %in% names(df)))
  expect_equal(df$count, c(3, 7))
})

# ---------------------------------------------------------------------------
# Contrôle de BORNE (bis) — le préfixe du message est bien dérivé du FILENAME
# ---------------------------------------------------------------------------
test_that(".read_per_sample_file : le préfixe suit le filename passé", {
  missing_path <- file.path(tempdir(), "bulk_import_engine_absent.tsv")
  res <- .bie_err(suppressWarnings(
    .read_per_sample_file(path = missing_path, filename = "autre_nom.tsv")
  ))
  expect_true("bulk_import_engine_error" %in% res$class)
  expect_true(startsWith(res$msg, "Lecture autre_nom.tsv : "))
  expect_false(startsWith(res$msg, .PREFIX_S1))
})

# ---------------------------------------------------------------------------
# P0 2026-10-21 — « un dossier = un échantillon » (GSE52778, 16 x counts.tsv)
# ---------------------------------------------------------------------------
# MESURÉ sur le vrai jeu : 16 dossiers `GSMxxxx/converted/by_sample/` contenant
# CHACUN un `counts.tsv` ⇒ `shiny::fileInput` ne transmet que le BASENAME, donc
# `.infer_sample_id()` rend "counts" 16 fois. Deux défauts distincts en
# découlent, et aucun n'était couvert :
#   1. collision d'identifiants : le module refuse l'import (« sample_id
#      dupliqués ») et rien dans l'UI ne permet d'en sortir (l'édition de la
#      table était diffusée à toutes les lignes — cf. test-mod-import-bulk.R) ;
#   2. `.merge_per_sample_tables()` ne se plaint PAS : `dplyr::full_join()`
#      renomme silencieusement en `counts.x`, `counts.y`, `counts.x.x`… (mesuré
#      39376 x 17 colonnes illisibles) ⇒ un appelant qui oublierait la garde
#      produirait une matrice aux noms d'échantillons faux, sans erreur.
# ---------------------------------------------------------------------------
test_that(".unique_sample_ids : basenames homonymes ⇒ identifiants uniques", {
  out <- .unique_sample_ids(rep("counts", 3L))
  expect_identical(out, c("counts", "counts_1", "counts_2"))
  expect_equal(length(unique(out)), 3L)         # la propriété qui compte
  expect_identical(out[1], "counts")            # la 1re occurrence garde son nom

  # Contrôle de BORNE : des identifiants déjà uniques ne bougent pas.
  expect_identical(.unique_sample_ids(c("S1", "S2")), c("S1", "S2"))
  expect_identical(.unique_sample_ids(character(0)), character(0))
})

test_that(".merge_per_sample_tables : colonnes homonymes ⇒ erreur CLASSÉE", {
  a <- data.frame(feature_id = c("G1", "G2"), counts = c(1, 2), check.names = FALSE)
  b <- data.frame(feature_id = c("G1", "G2"), counts = c(3, 4), check.names = FALSE)

  res <- .bie_err(.merge_per_sample_tables(list(a, b)))
  expect_false(is.null(res))                                # erreur levée…
  expect_true("bulk_import_engine_error" %in% res$class)    # …avec NOTRE classe
  expect_match(res$msg, "counts", fixed = TRUE)

  # Contrôle de BORNE : des noms distincts fusionnent normalement.
  names(b)[2] <- "S2"
  merged <- .merge_per_sample_tables(list(a, b))
  expect_identical(names(merged), c("feature_id", "counts", "S2"))
  expect_equal(nrow(merged), 2L)
})

# ---------------------------------------------------------------------------
# B3-A (round 5, 2026-10-28) — bulk_concordance_summary() : badge PERSISTANT
# de concordance counts ↔ métadonnées
# ---------------------------------------------------------------------------
# MESURÉ au HEAD a931ed7 : 0 badge (`grep -in -E 'concordance|badge'
# modules/import/mod_import_bulk.R` → 0 hit) ; le SEUL contrôle counts ↔
# sample_id est la garde ÉPHÉMÈRE de `.validate_design()`
# (bulk_import_engine.R:184-186) — une notification à l'import, aucune trace
# durable — tandis que le fallback mod_import_bulk.R:567 remplace
# SILENCIEUSEMENT la métadonnée par des défauts. Le badge de la carte
# « Résumé des Données Bulk » a besoin d'un résumé PUR, sans réactivité, qu'il
# peut re-rendre à tout instant depuis `global_data$bulk_obj`.
#
# Contrat (dossier d'arbitrage round 5, item B3-A) :
#   `list(ok, n_common, counts_only, meta_only, reason)` avec
#   `ok` ssi intersection NON VIDE **ET** aucun orphelin des deux côtés ;
#   NULL/vide d'un côté ⇒ ok=FALSE, reason="empty".
# ---------------------------------------------------------------------------
test_that("bulk_concordance_summary : concordance parfaite ⇒ ok=TRUE", {
  res <- bulk_concordance_summary(c("S1", "S2", "S3"), c("S1", "S2", "S3"))
  expect_true(res$ok)
  expect_identical(res$n_common, 3L)
  expect_identical(res$counts_only, character(0))
  expect_identical(res$meta_only, character(0))
  expect_identical(res$reason, "ok")
})

test_that("bulk_concordance_summary : orphelins des deux côtés ⇒ ok=FALSE", {
  # S2 sans métadonnée, M4 sans counts — l'intersection {S1,S3} reste non vide,
  # donc ok=FALSE à cause des orphelins, PAS de l'intersection vide.
  res <- bulk_concordance_summary(c("S1", "S2", "S3"), c("S1", "S3", "M4"))
  expect_false(res$ok)
  expect_identical(res$n_common, 2L)
  expect_identical(res$counts_only, "S2")   # échantillon sans métadonnée
  expect_identical(res$meta_only, "M4")     # métadonnée orpheline
  expect_identical(res$reason, "orphelins")
})

test_that("bulk_concordance_summary : aucun chevauchement ⇒ ok=FALSE", {
  res <- bulk_concordance_summary(c("S1", "S2"), c("M1", "M2"))
  expect_false(res$ok)
  expect_identical(res$n_common, 0L)
  expect_identical(res$reason, "aucun_chevauchement")
})

test_that("bulk_concordance_summary : NULL/vide d'un côté ⇒ ok=FALSE, empty", {
  cases <- list(
    counts_null   = list(NULL, "S1"),          # bulk_obj$counts absent
    meta_null     = list("S1", NULL),          # metadata NULL (caller ⇒ character(0))
    counts_empty  = list(character(0), "S1"),
    meta_empty    = list("S1", character(0)),  # metadata NULL passée par le badge
    both_empty    = list(character(0), character(0)),
    both_null     = list(NULL, NULL)           # bulk_obj absent
  )
  for (nm in names(cases)) {
    res <- do.call(bulk_concordance_summary, cases[[nm]])
    expect_false(res$ok, info = nm)
    expect_identical(res$reason, "empty", info = nm)
    expect_identical(res$n_common, 0L, info = nm)
    expect_identical(res$counts_only, character(0), info = nm)
    expect_identical(res$meta_only, character(0), info = nm)
  }
})

# ---------------------------------------------------------------------------
# B3-A — ÉPINGLE de câblage : le badge est DANS la carte « Résumé des Données
# Bulk », pas ailleurs (assertion sur le SOURCE du module, cf. motif §2cc.1)
# ---------------------------------------------------------------------------
test_that("B3-A épingle : badge concordance câblé dans la carte Résumé du module", {
  src <- paste(readLines(file.path(ts_project_root(), "modules/import/mod_import_bulk.R"),
                         warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # (1) la sortie serveur existe et consomme le helper PUR du moteur
  expect_match(src, "output$concordance_badge <- renderUI", fixed = TRUE)
  expect_match(src, "bulk_concordance_summary(", fixed = TRUE)
  # (2) les identifiants viennent bien de bulk_obj, comme spécifié (l'alias
  # `obj <- global_data$bulk_obj` puis colnames/rownames sur ses champs)
  expect_match(src, "obj <- global_data$bulk_obj", fixed = TRUE)
  expect_match(src, "colnames(obj$counts)", fixed = TRUE)
  expect_match(src, "rownames(obj$metadata)", fixed = TRUE)
  # (3) le uiOutput est DANS la carte Résumé : APRÈS la grille KPI, AVANT la console
  expect_match(src, 'uiOutput(ns("concordance_badge"))', fixed = TRUE)
  pos_kpi    <- regexpr('class = "bulk-kpi-grid"', src, fixed = TRUE)
  pos_badge  <- regexpr('uiOutput(ns("concordance_badge"))', src, fixed = TRUE)
  pos_consol <- regexpr('bulk-console-scroll', src, fixed = TRUE)
  expect_gt(pos_kpi, 0L)      # les 3 ancres existent (sinon ordering trivial)
  expect_gt(pos_badge, 0L)
  expect_gt(pos_consol, 0L)
  expect_gt(pos_badge, pos_kpi)
  expect_gt(pos_consol, pos_badge)
})

# ---------------------------------------------------------------------------
# Verrou source : bulk_import_engine.R ne doit plus contribuer aucun C10
# ---------------------------------------------------------------------------
test_that("bulk_import_engine.R ne contribue aucun signalement C10", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools/check_conventions.R"), envir = e)
  before <- length(e$.REPORT$warns)
  e$check_c10_error_style(file.path(ts_project_root(), "tools/pkg_skeleton/R/bulk_import_engine.R"))
  w <- e$.REPORT$warns
  n <- length(w) - before
  if (n > 0L) {
    lignes <- vapply(w[(before + 1L):length(w)], function(x) x$line, integer(1))
    cat("Sites C10 restants dans bulk_import_engine.R :",
        paste(sort(lignes), collapse = ", "), "\n")
  }
  expect_equal(n, 0L)
})

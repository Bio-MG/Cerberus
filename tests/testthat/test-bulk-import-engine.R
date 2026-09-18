# =============================================================================
# test-bulk-import-engine.R — tests for R/bulk/bulk_import_engine.R
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

source_project_file("R/bulk/bulk_import_engine.R")

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
# Verrou source : bulk_import_engine.R ne doit plus contribuer aucun C10
# ---------------------------------------------------------------------------
test_that("bulk_import_engine.R ne contribue aucun signalement C10", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools/check_conventions.R"), envir = e)
  before <- length(e$.REPORT$warns)
  e$check_c10_error_style(file.path(ts_project_root(), "R/bulk/bulk_import_engine.R"))
  w <- e$.REPORT$warns
  n <- length(w) - before
  if (n > 0L) {
    lignes <- vapply(w[(before + 1L):length(w)], function(x) x$line, integer(1))
    cat("Sites C10 restants dans bulk_import_engine.R :",
        paste(sort(lignes), collapse = ", "), "\n")
  }
  expect_equal(n, 0L)
})

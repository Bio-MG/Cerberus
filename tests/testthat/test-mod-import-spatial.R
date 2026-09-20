# =============================================================================
# test-mod-import-spatial.R — tests for modules/import/mod_import_spatial.R
# =============================================================================
# 31ᵉ incrément de la dette de conventions (2026-09-18, §2cq).
# Créé AVANT la conversion (règle 5 : test d'abord, rouge d'abord) ⇒ ce fichier
# doit ÉCHOUER sur les assertions de verrou source tant que le `stop()` de la
# ligne 481 ne porte pas `class = "spatial_import_error"`.
#
# 🔑 CLASSE `spatial_import_error` — doctrine §7 appliquée, MESURÉE.
# Le domaine `modules/import/` porte déjà trois classes, toutes de la forme
# `<modalité>_import_error` : `bulk_import_error` (`mod_import_bulk.R`, §2bs),
# `geo_import_error` (`mod_geo.R`, §2bt), `sc_import_error`
# (`mod_import_sc.R`, §2bu). `mod_import_spatial.R` est la **4ᵉ porte d'entrée**
# de l'application (Visium / Visium HD / Xenium / CosMx / Slide-seq) ⇒ le mapping
# mécanique de `modules/` (retirer `mod_`, garder le domaine) donne
# **`spatial_import_error`**. ⚠️ Ce nom ne collisionne pas avec les classes de
# `R/spatial/` (`spatial_io_error`, `spatial_stats_error`, …) : celles-ci sont
# déclarées dans `R/`, celle-ci dans `modules/import/`.
#
# 🔴 **LE LOT QUI BOUCLE — ET CORRIGE — LE DOMAINE `modules/import/`.**
# §2bu conclut : « Le domaine `modules/import/` est bouclé ». C'est une
# **sur-affirmation** : le texte précise ensuite « les **trois portes d'entrée** »
# (bulk / GEO / SC) — or le dossier contient un **4ᵉ** fichier porteur d'un site
# C10, `mod_import_spatial.R:481`, jamais converti. Après ce lot, `modules/import/`
# contribue **0** site C10 — c'est l'invariant durable asséré par le DERNIER test
# (verrou de domaine), et §2bu est corrigé par un renvoi vers §2cq.
#
# 🟢 **FORME NOUVELLE : un site C10 qui est le BRAS PAR DÉFAUT d'un `switch()`.**
# §2by a établi que « la FORME du fichier prédit la joignabilité mieux que son
# domaine ». Ce lot ajoute un cas mesuré :
#   `switch(input$technology, "visium" = {…}, "xenium" = …, "cosmx" = …,
#           "slideseq" = …, stop(…))`
# Le `stop()` est le **bras par défaut** (dernier argument NON nommé) ⇒ il tire
# dès que `input$technology` ne correspond à AUCUNE modalité connue, **sans
# exécuter les autres bras** (R n'évalue que le bras retenu). Prologue minimal :
# ni DESeq2, ni Seurat, ni `rmarkdown` — le moins coûteux des 6 sites restants.
#
# 🔴 **DEUX VERDICTS SÉPARÉS (§2cp.1), tous deux MESURÉS ICI :
# ATTEIGNABLE, mais la classe est AVALÉE.**
#   (1) JOIGNABILITÉ : **OUI**, prouvée par témoin — le `tryCatch` ouvert **L449**
#       ferme **L617** et son gestionnaire (**L614-617**) fait
#       `msg <- paste(.tr("❌ Erreur import spatial:"), conditionMessage(e)) ;
#        add_log(msg) ; showNotification(msg, …)`.
#       C'est le message **DU GARDE** qui ressort par `add_log` : preuve non
#       vacante, obtenue sans que la classe ne s'échappe.
#   (2) REMONTÉE : **NON**. Le gestionnaire **RETOURNE** une valeur (le résultat
#       de `showNotification`) au lieu de **relancer** ⇒ il **AVALE** l'erreur.
#       Critère §2cd.2 : « ce n'est pas `tryCatch` qui condamne un site, c'est le
#       GESTIONNAIRE ».
# ⇒ **3ᵉ occurrence** du profil « atteignable + avalé » après `mod_bulk.R`
#   (§2cp) et `mod_geo.R` / `mod_import_sc.R` (§2bs / §2bu).
#
# 🟢 **CONTRÔLE DE BORNE — RÉEL, et c'est l'apport propre de ce lot.**
# Contrairement à `mod_sc.R:707` (§2co) et à `mod_bulk_report.R:227` — dont la
# garde exige qu'un `rmarkdown::render()` **RÉUSSISSE**, donc un contrôle de
# borne **irréalisable** — la garde du site 481 se franchit par une entrée **DANS
# le domaine** : `technology = "visium"` sélectionne le bras **nommé**, qui appelle
# `load_spatial_visium(...)`. On bouchonne ce chargeur pour lever une sentinelle
# ⇒ on **PROUVE la CONDITIONNALITÉ** (le garde ne tire que sur une modalité
# inconnue) tout en restant dans les valeurs réelles du `selectInput`.
# ⚠️ **Valeur HORS DOMAINE assumée pour la joignabilité** : `input$technology =
# "INCONNU"`. Même procédé que `test-mod-sc.R` (site 707, `report_format =
# "INCONNU"`) : c'est une **sonde de joignabilité**, pas une affirmation sur le
# comportement utilisateur — la conditionnalité, elle, est prouvée par le
# contrôle de borne ci-dessus.
#
# ⚠️ **`withProgress()` DOIT être bouchonné — mesuré.** Signature réelle
# `(expr, min, max, value, message, detail, style, session, env, quoted)` : le
# bloc positionnel tombe sur **`expr`**, le PREMIER formel. Hors session Shiny la
# primitive lève ⇒ on la remplace par `function(expr, ...) expr` (retourner `expr`
# force le bloc à s'exécuter). C'est une **primitive d'UI**, comme
# `showNotification` — pas le sujet du test.
# ⚠️ **`incProgress()` est appelé AVANT le `switch`** (L461) ⇒ bouchon requis, et
# il sert de **témoin de traversée**.
# ⚠️ **`req()` est RÉEL** (`shiny::req`), non mocké : mesuré, il passe hors
# contexte réactif dès que ses arguments sont truthy.
# ⚠️ **`dir_path` est un `reactiveVal`** dans le module (L361) : on le remplace par
# une fonction constante — on ne teste pas la sélection de dossier.
# ⚠️ **C16 / §2bn sans objet** : message à **UN SEUL** argument, statique.
# ⚠️ **Le handler d'`observeEvent` est un BLOC, pas une DÉFINITION** : le 3ᵉ
# élément de l'appel est `{ … }`. Il faut donc l'**évaluer** (`eval`), **pas**
# l'appeler — l'**inverse** du piège §2cn.3. Se tromper donne
# `could not find function "f"` (§2cp).
#
# 🔴 **PREUVE = VERROU SOURCE + TÉMOIN D'EXÉCUTION** (et non exécution de la
# classe) — la classe étant avalée, la SOURCE est le seul canal où elle est
# vérifiable, et c'est exactement ce que C10 mesure. Le verrou est **rendu
# falsifiable** : le 4ᵉ test assère que le gestionnaire englobant journalise +
# notifie **SANS `stop(`** ⇒ si un jour il relance, le test **échoue** et signale
# que le lot devient **prouvable à l'exécution** (§2cd.2, motif repris de §2cp).
# =============================================================================

source_project_file("modules/import/mod_import_spatial.R")

.MIS_FILE <- "modules/import/mod_import_spatial.R"

# Le message du garde est PUREMENT ASCII (pas d'échappement \u00xx dans la
# source) ⇒ on peut l'assérer ENTIER sans dépendre de l'encodage.
.MIS_MSG <- "Technologie inconnue."

# --- AST : le BLOC (3ᵉ élément) de `observeEvent(input$btn_import, …)` ---------
# Harnais partage `helper-ast.R` (§2dg) : ce fichier portait TROIS copies du
# parcours recursif `walk()` — les trois sont remplacees ici (3 des 14 du depot).
.mis_handler_expr <- function() ts_ast_observe_block(.MIS_FILE, "btn_import")

# --- AST : le GESTIONNAIRE `error = function(e) …` qui porte « Erreur import
# spatial » (L614-617). Sert à la justification falsifiable du verrou source.
.mis_outer_handler <- function() ts_ast_trycatch_handler(.MIS_FILE, "Erreur import spatial")

# --- Environnement enfant ----------------------------------------------------
# `rec` enregistre les étapes (témoins) ; `loader_throws` sert au contrôle de
# BORNE : on franchit le `switch` par son bras NOMMÉ et on s'arrête juste après.
.mis_env <- function(technology, rec = NULL, loader_throws = FALSE) {
  e <- new.env(parent = globalenv())
  e$.tr    <- function(s, ...) s
  e$.t_fmt <- function(fmt, ...) fmt
  # cf. R/core/io_helpers.R:47
  e$`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  e$req    <- shiny::req            # RÉEL (mesure §2cn.4)
  # Primitives d'UI : hors session Shiny elles lèvent (mesure) ⇒ bouchons.
  e$withProgress <- function(expr, ...) expr
  e$incProgress  <- function(...) { if (!is.null(rec)) rec("incProgress"); invisible(NULL) }
  # Canaux d'observation du gestionnaire avaleur (L615-616).
  e$add_log <- function(msg) {
    if (!is.null(rec)) rec(paste0("log:", paste0(msg)))
    invisible(NULL)
  }
  e$showNotification <- function(msg, ...) {
    if (!is.null(rec)) rec(paste0("notify:", paste0(msg)))
    invisible(NULL)
  }
  # `dir_path` est un `reactiveVal` (L361) ⇒ fonction constante.
  e$dir_path <- function() "FAKE_DIR"
  # `get_visium_import_mode()` n'est appelé QUE si technology == "visium" ; on le
  # fait lever pour que son tryCatch (L442) rende "visium" — mode nominal, non-HD.
  e$get_visium_import_mode <- function(dir) stop("__mode_ko__")
  e$load_spatial_visium <- function(...) {
    if (!is.null(rec)) rec("load_spatial_visium")
    if (loader_throws) stop("__arret_volontaire__")
    NULL
  }
  e$global_data <- list(active_spatial_dataset = NULL, spatial_datasets = list(),
                        spatial_results_cache = list())
  e$input <- list(technology = technology, sample_name = "s1",
                  hd_bin_size = NULL, min_counts = 10, min_features = 20,
                  min_counts_ss = 100, min_features_ss = 200)
  e
}

# --- Exécution : le corps est un BLOC ⇒ on l'ÉVALUE (jamais on ne l'appelle).
.mis_run <- function(envir) {
  tryCatch({
    eval(.mis_handler_expr(), envir = envir)
    NULL
  }, error = function(e) list(msg = conditionMessage(e), class = class(e)))
}

# ---------------------------------------------------------------------------
# TÉMOIN D'EXÉCUTION : le garde 481 S'EXÉCUTE (prouvé) mais l'erreur est AVALÉE
# ---------------------------------------------------------------------------
test_that("mod_import_spatial : le garde 481 est ATTEINT (temoin) puis AVALE", {
  steps <- character(0)
  err <- .mis_run(.mis_env("INCONNU", rec = function(s) steps <<- c(steps, s)))

  # 1) TÉMOIN DE TRAVERSÉE NON VACUANT : le gestionnaire L614-617 a journalisé le
  #    message DU GARDE ⇒ le garde s'est bien exécuté. Ce n'est pas une déduction,
  #    c'est SON message qui ressort par `add_log`.
  expect_true(any(grepl(.MIS_MSG, steps, fixed = TRUE)))
  # 2) …et on est bien allé jusqu'au `switch` : `incProgress` (L461) le précède.
  expect_true("incProgress" %in% steps)
  # 3) 🔴 AUCUNE erreur ne s'échappe ⇒ le gestionnaire l'a AVALÉE.
  expect_null(err)
})

# ---------------------------------------------------------------------------
# La classe est INOBSERVABLE : c'est la justification du verrou source
# ---------------------------------------------------------------------------
test_that("mod_import_spatial : la classe spatial_import_error est INOBSERVABLE", {
  steps <- character(0)
  err <- .mis_run(.mis_env("INCONNU", rec = function(s) steps <<- c(steps, s)))

  # La classe n'apparaît dans AUCUN canal observable : ni en erreur remontée…
  expect_null(err)
  # …ni dans les canaux du gestionnaire, qui ne portent que le MESSAGE.
  expect_false(any(grepl("spatial_import_error", steps, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# Contrôle de BORNE : le garde est CONDITIONNEL (bras nommé ⇒ il ne tire pas)
# ---------------------------------------------------------------------------
test_that("le garde 481 est conditionnel : technology='visium' => il ne tire pas", {
  steps <- character(0)
  err <- .mis_run(.mis_env("visium", rec = function(s) steps <<- c(steps, s),
                           loader_throws = TRUE))

  # TÉMOIN DE TRAVERSÉE : le bras NOMMÉ « visium » a bien été atteint — le
  # chargeur a été appelé, et son échec est remonté au gestionnaire. Sans ce
  # témoin, l'absence du message du garde serait vraie aussi si le corps avait
  # échoué AVANT le `switch` (§2cj.3).
  expect_true("load_spatial_visium" %in% steps)
  expect_true(any(grepl("__arret_volontaire__", steps, fixed = TRUE)))
  # …et le message du garde n'apparaît NULLE PART ⇒ il n'a pas tiré.
  expect_false(any(grepl(.MIS_MSG, steps, fixed = TRUE)))
  expect_null(err)
})

# ---------------------------------------------------------------------------
# Justification EXÉCUTABLE du verrou source : le gestionnaire ne RELANCE pas
# ---------------------------------------------------------------------------
test_that("le gestionnaire englobant journalise + notifie SANS stop( (falsifiable)", {
  d <- paste(deparse(.mis_outer_handler()), collapse = "\n")
  # Il rend la main en journalisant ET en notifiant…
  expect_true(grepl("add_log", d, fixed = TRUE))
  expect_true(grepl("showNotification", d, fixed = TRUE))
  # …et NE relance PAS. Si un jour il relançait, ce test échouerait et
  # signalerait que le lot devient prouvable à l'exécution (§2cd.2).
  expect_false(grepl("stop(", d, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Verrou source CIBLÉ : le garde 481 porte bien NOTRE classe
# ---------------------------------------------------------------------------
test_that("le garde 481 porte la classe spatial_import_error (verrou source cible)", {
  # Harnais partage `helper-ast.R` (§2dg) : 3ᵉ et derniere copie `walk()` du fichier.
  hits <- ts_ast_stop_sites(.MIS_FILE, .MIS_MSG)

  expect_length(hits, 1L)                                   # exactement UN site
  expect_true(grepl("errorCondition", hits[[1]], fixed = TRUE))
  expect_true(grepl('class = "spatial_import_error"', hits[[1]], fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Verrou de DOMAINE : `modules/import/` ne contribue plus AUCUN C10
# ---------------------------------------------------------------------------
# C'est l'invariant durable du lot (§2bu corrigé) : les QUATRE portes d'entrée
# (bulk, GEO, SC, spatial) sont classées. Le test échoue si un fichier de ce
# dossier réintroduit un site — c'est voulu : le domaine est déclaré bouclé.
test_that("le domaine modules/import/ ne contribue aucun signalement C10", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools/check_conventions.R"), envir = e)
  dir_import <- file.path(ts_project_root(), "modules/import")
  files <- list.files(dir_import, pattern = "\\.R$", full.names = TRUE)
  # Garde-fou de non-vacuité : un dossier vide passerait le test pour rien.
  expect_gte(length(files), 4L)
  expect_true(any(basename(files) == "mod_import_spatial.R"))

  before <- length(e$.REPORT$warns)
  for (f in files) e$check_c10_error_style(f)
  w <- e$.REPORT$warns
  n <- length(w) - before
  if (n > 0L) {
    cat("Sites C10 restants dans modules/import/ :\n")
    for (i in (before + 1L):length(w)) cat(" ", w[[i]]$file, ":", w[[i]]$line, "\n")
  }
  expect_equal(n, 0L)
})

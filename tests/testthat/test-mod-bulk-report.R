# =============================================================================
# test-mod-bulk-report.R — tests for modules/bulk/mod_bulk_report.R
# =============================================================================
# 32ᵉ incrément de la dette de conventions (2026-09-18, §2cr).
# Créé AVANT la conversion (règle 5 : test d'abord, rouge d'abord) ⇒ ce fichier
# doit ÉCHOUER sur l'assertion de CLASSE et sur les deux verrous source tant que
# le `stop()` de la ligne 227 ne porte pas `class = "bulk_report_error"`.
#
# 🔑 CLASSE `bulk_report_error` — doctrine §7 appliquée, MESURÉE.
# Mapping mécanique de `modules/` (retirer `mod_`, garder le domaine :
# `mod_bulk_pathways.R` → `bulk_pathways_error`, §2cn) ⇒ **`bulk_report_error`**.
# Le nom est **libre** (grep des **51** classes `_error` déclarées dans `R/` +
# `modules/` : aucune n'est `bulk_report_error`) **et** non interdit par la
# doctrine (ce n'est pas un nom « trop large » comme `bulk_error` ou `sc_error`).
# ⚠️ Il **cohabite** avec `report_error`, porté par les **4** fichiers de
# `R/reports/` (`report_bundle`, `report_collector`, `report_render`,
# `report_validator`) : ce sont **deux domaines distincts** — le *bundle* de
# rapports générique vs le rapport **Bulk** — exactement comme
# `bulk_pathways_error` (§2cn) cohabite avec `pathway_error` (§2bp).
# `R/bulk/bulk_report_engine.R` **ne déclare aucune** `errorCondition` (mesuré) :
# il n'y a donc rien à partager avec lui.
#
# 🔴 **LE LOT QUI BOUCLE LE DOMAINE `modules/bulk/`.** Le garde ne signale qu'**un**
# site C10 dans ce dossier (le 227) : après conversion, `modules/bulk/` contribue
# **0** — c'est l'invariant asséré par le DERNIER test (verrou de domaine, motif
# introduit au §2cq).
#
# ---------------------------------------------------------------------------
# 🟢 LE VERDICT EST L'INVERSE DE §2cp ET §2cq : ICI LA CLASSE S'ÉCHAPPE
# ---------------------------------------------------------------------------
# `content = function(file)` du `downloadHandler` `output$dl_report` (**L111 →
# L231**) n'est **enveloppé par AUCUN `tryCatch`** : les **deux** `tryCatch` du
# corps (L122-125 template, L218-224 rendu) sont **antérieurs** au site 227 et
# **aucun** ne l'englobe. ⇒ Le `stop()` du 227 remonte **tel quel** jusqu'à
# l'appelant : la classe **EST observable** (`expect_true("bulk_report_error" %in%
# res$class)`), et c'est une preuve **COMPORTEMENTALE** — la plus forte *pour la
# conversion*, contrairement aux lots §2cp/§2cq dont la preuve était un **verrou
# source** (classe avalée).
#
# ⇒ **CONTRASTE MESURÉ, à conserver** : `mod_sc.R:707` (§2co) porte **le même
# message** (`Aucun format généré.`) dans **la même forme** (`downloadHandler` +
# `content`) et s'échappe **aussi** ; `mod_bulk.R:427` (§2cp) et
# `mod_import_spatial.R:481` (§2cq) sont **atteignables mais avalés**. Le
# discriminant n'est ni le dossier ni le type de construit : c'est la présence
# d'un **gestionnaire avaleur sur le chemin de sortie** (§2cd.2).
# Ce lot est donc **une preuve directe en faveur de la branche `class`** du
# verrou `state` vs `class` (§2ck.1) : la classe y sert **réellement**.
#
# 🔴 **LIMITE DOCUMENTÉE — le site 227 n'a PAS de contrôle de borne.** Franchir sa
# garde exige que `out_files` soit NON VIDE, donc qu'un `rmarkdown::render()`
# **RÉUSSISSE** (`out_files <- c(out_files, res)`), et cet appel passe par `::` ⇒
# **non mockable** (§2cp.2). Même limite que `mod_sc.R:707` (§2co). ⚠️
# **L'assertion d'erreur n'est PAS vacante pour autant** : la classe n'apparaît
# QUE si notre `stop()` converti s'est exécuté — ce qui EST la preuve de
# joignabilité. Ce qui manque est la preuve de **conditionnalité**.
#
# ⚠️ **Valeur HORS DOMAINE assumée pour la joignabilité** : `report_format =
# "INCONNU"` ⇒ `switch()` rend `NULL` ⇒ `formats_needed` est `NULL` ⇒ la boucle
# `for (fmt in formats_needed)` ne tourne **pas** ⇒ `out_files` reste vide ⇒ le
# garde tire, **sans jamais appeler `rmarkdown::render()`** (donc sans pandoc, sans
# latex, sans coût). Même procédé que `test-mod-sc.R` (site 707) : c'est une
# **sonde de joignabilité**, pas une affirmation sur le comportement utilisateur.
#
# ⚠️ **`withProgress()` DOIT être bouchonné — mesuré.** Signature réelle
# `(expr, min, max, value, message, detail, style, session, env, quoted)` : le
# bloc positionnel tombe sur **`expr`**, le PREMIER formel, et hors session Shiny
# la primitive lève. On la remplace par `function(expr, ...) expr` — retourner
# `expr` **force** le bloc à s'exécuter. C'est une **primitive d'UI**, comme
# `showNotification`, pas le sujet du test.
# ⚠️ **`incProgress()` n'est PAS bouchonné, volontairement** : la boucle ne tourne
# pas, donc il ne doit jamais être appelé. S'il l'était un jour, l'absence de
# bouchon produirait un échec **bruyant** — c'est le comportement voulu.
# ⚠️ **`req()` est RÉEL** (`shiny::req`) : mesuré, il passe hors contexte réactif
# dès que ses arguments sont truthy (§2cn.4).
# ⚠️ **`%||%` est REDÉFINI localement** (cf. `R/core/io_helpers.R:47`) : le corps
# en fait un usage intensif dans `render_params`.
# ⚠️ **`stats::setNames` / `vapply` sont RÉELS** (base) : `.tr_fn()` doit rendre
# une **fonction** `character(1) -> character(1)`, sinon `vapply` échoue.
# ⚠️ **C16 / §2bn sans objet** : message à **UN SEUL** argument, statique ⇒
# **aucun `paste0()`**. On assère le message **ENTIER** (§2bx.3).
#
# ⚠️ **Sélecteur AST : on collecte TOUS les candidats, puis on exige `length ==
# 1`.** `test-mod-sc.R`/`test-mod-bulk.R`/`test-mod-import-spatial.R` retournent
# le **premier** `downloadHandler(content=)` trouvé — ce qui est sûr tant qu'on ne
# l'affirme pas. Ici le fichier en contient **plusieurs** (`dl_report`,
# `dl_r_script`, …) : on **énumère** et on **assère l'unicité** de la sélection,
# au lieu de l'espérer.
# =============================================================================

source_project_file("modules/bulk/mod_bulk_report.R")

.MBR_FILE   <- "modules/bulk/mod_bulk_report.R"
# Fragments ASCII : la source porte des echappements \u00xx, on reste insensible
# a l'encodage (le deparse() peut rendre l'accent OU l'echappement).
.MBR_NEEDLE <- "Aucun format"
.MBR_MSG    <- "Aucun format g\u00e9n\u00e9r\u00e9."

# --- AST : TOUS les `content` de `downloadHandler` portant la garde -----------
.mbr_content_exprs <- function() {
  p   <- parse(file.path(ts_project_root(), .MBR_FILE))
  out <- list()
  walk <- function(x) {
    if (is.call(x)) {
      is_dh <- tryCatch(identical(x[[1]], quote(downloadHandler)), error = function(e) FALSE)
      if (isTRUE(is_dh)) {
        l  <- as.list(x)
        nm <- names(l)
        for (i in seq_along(l)) {
          if (!is.null(nm) && identical(nm[i], "content")) {
            d <- paste(deparse(l[[i]]), collapse = "\n")
            if (grepl(.MBR_NEEDLE, d, fixed = TRUE))
              out[[length(out) + 1L]] <<- l[[i]]
          }
        }
      }
      l <- as.list(x)
      for (i in seq_along(l)) {
        ok <- tryCatch(is.call(l[[i]]), error = function(e) FALSE)
        if (isTRUE(ok)) walk(l[[i]])
      }
    }
    invisible(NULL)
  }
  for (i in seq_along(p)) walk(p[[i]])
  out
}

.mbr_content_expr <- function() {
  out <- .mbr_content_exprs()
  # Unicite de la selection : sinon on testerait un downloadHandler au hasard.
  if (length(out) != 1L)
    stop("selection non unique : ", length(out), " downloadHandler(content=) portent '",
         .MBR_NEEDLE, "'", call. = FALSE)
  out[[1L]]
}

# --- Environnement enfant ----------------------------------------------------
# `rec` enregistre les etapes (temoins de traversee).
.mbr_env <- function(report_format, rec = NULL) {
  e <- new.env(parent = globalenv())
  e$.tr    <- function(s, ...) s
  e$.tr_fn <- function(gd) function(s, ...) s   # character(1) -> character(1)
  # cf. R/core/io_helpers.R:47
  e$`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  e$req    <- shiny::req            # REEL (mesure §2cn.4)
  # Primitive d'UI : hors session Shiny elle leve (mesure) => bouchon.
  e$withProgress <- function(expr, ...) expr
  e$showNotification <- function(msg, ...) {
    if (!is.null(rec)) rec(paste0("notify:", paste0(msg)))
    invisible(NULL)
  }
  # Helpers maison : a BOUCHONNER (la recherche retomberait sinon sur globalenv).
  e$.find_bulk_report_template <- function() {
    if (!is.null(rec)) rec("find_template")
    "FAKE_TEMPLATE.Rmd"
  }
  e$file.copy <- function(...) {
    if (!is.null(rec)) rec("file.copy")
    TRUE
  }
  e$global_data <- list(language = "fr",
                        bulk_obj = list(metadata = data.frame(x = 1)))
  e$shared_rv   <- list(vst_mat = matrix(1, 2, 2))
  e$input <- list(report_format = report_format, report_sections = character(0),
                  pairwise_layout = "grid", report_title = "T", report_subtitle = "S",
                  report_notes = "N", report_interactive = FALSE)
  e
}

# ⚠️ `eval()` rend la FONCTION ; il faut l'APPELER (§2cn.3).
.mbr_call <- function(envir) {
  tryCatch({
    f <- eval(.mbr_content_expr(), envir = envir)
    f(tempfile(fileext = ".html"))
    NULL
  },
  error = function(e) list(msg = conditionMessage(e), class = class(e)))
}

# ---------------------------------------------------------------------------
# LE COEUR DU LOT — la classe S'ECHAPPE (verdict inverse de §2cp / §2cq)
# ---------------------------------------------------------------------------
test_that("mod_bulk_report : aucun format genere => la classe S'ECHAPPE (site 227)", {
  steps <- character(0)
  res <- .mbr_call(.mbr_env("INCONNU", rec = function(s) steps <<- c(steps, s)))

  # 1) L'erreur REMONTE (aucun gestionnaire avaleur sur le chemin de sortie)…
  expect_false(is.null(res))
  # 2) …et elle porte NOTRE classe. C'est la preuve COMPORTEMENTALE de la
  #    conversion — pas un verrou source.
  expect_true("bulk_report_error" %in% res$class)
  # 3) Message a UN SEUL argument, entierement statique => message ENTIER (§2bx.3).
  expect_identical(res$msg, .MBR_MSG)
  # 4) TEMOIN DE TRAVERSEE NON VACUANT : le template a ete localise ET copie
  #    (L122-128), donc on a bien franchi le prologue AVANT le garde 227.
  expect_true("find_template" %in% steps)
  expect_true("file.copy" %in% steps)
})

# ---------------------------------------------------------------------------
# Aucun gestionnaire avaleur : rien n'a ete notifie (contraste avec §2cp/§2cq)
# ---------------------------------------------------------------------------
test_that("mod_bulk_report : aucun gestionnaire avaleur n'a tire", {
  steps <- character(0)
  res <- .mbr_call(.mbr_env("INCONNU", rec = function(s) steps <<- c(steps, s)))

  expect_false(is.null(res))
  # Les DEUX `tryCatch` du corps (template L122-125, rendu L218-224) notifient en
  # cas d'echec. Aucun n'a tire : ni l'un ni l'autre n'est sur le chemin de sortie
  # du garde 227.
  expect_false(any(grepl("notify:", steps, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# Justification STRUCTURELLE et falsifiable : le garde n'est dans AUCUN tryCatch
# ---------------------------------------------------------------------------
# C'est l'equivalent, pour un site OBSERVABLE, de la « justification falsifiable »
# des lots §2cp/§2cq : on encode la CAUSE du verdict, pas seulement son effet.
# Si un jour quelqu'un enveloppe le garde dans un `tryCatch`, ce test ECHOUE et
# signale que le lot redevient « source-lock seulement ».
test_that("le garde 227 n'est englobe par AUCUN tryCatch (falsifiable)", {
  inside_try <- FALSE
  found      <- FALSE
  walk <- function(x, inside) {
    if (is.call(x)) {
      is_stop <- tryCatch(identical(x[[1]], quote(stop)), error = function(e) FALSE)
      if (isTRUE(is_stop)) {
        d <- gsub("\\s+", " ", paste(deparse(x), collapse = " "))
        if (grepl(.MBR_NEEDLE, d, fixed = TRUE)) {
          inside_try <<- inside
          found      <<- TRUE
          return(invisible(NULL))
        }
      }
      is_tc <- tryCatch(identical(x[[1]], quote(tryCatch)), error = function(e) FALSE)
      l <- as.list(x)
      for (i in seq_along(l)) {
        ok <- tryCatch(is.call(l[[i]]), error = function(e) FALSE)
        if (isTRUE(ok)) walk(l[[i]], inside || isTRUE(is_tc))
      }
    }
    invisible(NULL)
  }
  walk(.mbr_content_expr(), FALSE)

  expect_true(found)          # le garde est bien DANS le corps extrait
  expect_false(inside_try)    # …et hors de tout tryCatch => la classe s'echappe
})

# ---------------------------------------------------------------------------
# Verrou source CIBLE : le garde 227 porte bien NOTRE classe
# ---------------------------------------------------------------------------
test_that("le garde 227 porte la classe bulk_report_error (verrou source cible)", {
  p <- parse(file.path(ts_project_root(), .MBR_FILE))
  hits <- character(0)
  walk <- function(x) {
    if (is.call(x)) {
      is_stop <- tryCatch(identical(x[[1]], quote(stop)), error = function(e) FALSE)
      if (isTRUE(is_stop)) {
        d <- gsub("\\s+", " ", paste(deparse(x), collapse = " "))
        if (grepl(.MBR_NEEDLE, d, fixed = TRUE)) hits <<- c(hits, d)
      }
      l <- as.list(x)
      for (i in seq_along(l)) {
        ok <- tryCatch(is.call(l[[i]]), error = function(e) FALSE)
        if (isTRUE(ok)) walk(l[[i]])
      }
    }
    invisible(NULL)
  }
  for (i in seq_along(p)) walk(p[[i]])

  expect_length(hits, 1L)                                   # exactement UN site
  expect_true(grepl("errorCondition", hits[[1]], fixed = TRUE))
  expect_true(grepl('class = "bulk_report_error"', hits[[1]], fixed = TRUE))
  # C16 : le message reste a UN SEUL argument (aucun paste0() ajoute).
  expect_false(grepl("paste0(", hits[[1]], fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Verrou de DOMAINE : `modules/bulk/` ne contribue plus AUCUN C10
# ---------------------------------------------------------------------------
test_that("le domaine modules/bulk/ ne contribue aucun signalement C10", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools/check_conventions.R"), envir = e)
  dir_bulk <- file.path(ts_project_root(), "modules/bulk")
  files <- list.files(dir_bulk, pattern = "\\.R$", full.names = TRUE)
  # Garde-fou de non-vacuite : un dossier vide passerait le test pour rien.
  expect_gte(length(files), 10L)
  expect_true(any(basename(files) == "mod_bulk_report.R"))

  before <- length(e$.REPORT$warns)
  for (f in files) e$check_c10_error_style(f)
  w <- e$.REPORT$warns
  n <- length(w) - before
  if (n > 0L) {
    cat("Sites C10 restants dans modules/bulk/ :\n")
    for (i in (before + 1L):length(w)) cat(" ", w[[i]]$file, ":", w[[i]]$line, "\n")
  }
  expect_equal(n, 0L)
})

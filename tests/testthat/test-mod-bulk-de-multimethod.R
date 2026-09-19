# =============================================================================
# test-mod-bulk-de-multimethod.R
#   tests for modules/bulk_de/mod_bulk_de_multimethod.R
# =============================================================================
# 33ᵉ incrément de la dette de conventions (2026-09-19, §2cs).
# Créé AVANT la conversion (règle 5 : test d'abord, rouge d'abord) ⇒ ce fichier
# doit ÉCHOUER sur l'assertion de classe et sur le verrou source tant que le
# `stop()` de la ligne 109 ne porte pas `class = "bulk_de_multimethod_error"`.
#
# 🔑 CLASSE `bulk_de_multimethod_error` — mapping mécanique de `modules/`
# (retirer le préfixe `mod_`, garder le domaine) appliqué SANS collision :
# `mod_bulk_de_multimethod.R` → `bulk_de_multimethod_error`, nom LIBRE.
# ⚠️ La tentation était de RÉUTILISER `bulk_de_error` — il existe déjà, mais il
# appartient à `R/bulk/bulk_helpers.R`. Mesure du dépôt (33ᵉ incrément) :
# **AUCUNE** classe n'est partagée entre un fichier de `R/` et un fichier de
# `modules/` (les seules classes multi-fichiers vivent TOUTES à l'intérieur de
# `R/` : `report_error` sur 4 fichiers, `communication_context_error` sur 4,
# `milo_error`/`sccoda_error`/`da_design_error` sur 2). ⇒ la classe d'un module
# est la SIENNE. C'est la même règle que `bulk_pathways_error` (§2cn) coexistant
# avec `pathway_error` (§2bp).
#
# ---------------------------------------------------------------------------
# 🔴 VERDICTS — ce que ce lot PROUVE, et ce qu'il NE prouve PAS
# ---------------------------------------------------------------------------
#   (1) ATTEIGNABLE ................ OUI, prouvé (témoin non vacuante ci-dessous)
#   (2) LA CLASSE S'ÉCHAPPE ........ NON — le `tryCatch` ouvert L80 ferme L145 et
#       son gestionnaire (L139-145) **journalise + notifie** au lieu de
#       **relancer** ⇒ il **AVALE** l'erreur (§2cd.2). ⇒ preuve = VERROU SOURCE.
#   (3) CONDITIONNEL ................ OUI, prouvé — avec un TÉMOIN DE TRAVERSÉE
#       (§2cj.3/#60) : au-dessus de la borne, la notification de SUCCÈS part et
#       `multimethod_status_rv()` reçoit `n_methods = 2`. « Notre message est
#       absent » serait vrai aussi si le code avait échoué AVANT le garde.
#   (4) MESSAGE VARIABLE ............ OUI, prouvé (§2ch.3/#56) — le message
#       interpole `n = length(de_list)` : il est asséré VARIER (1 puis 0).
#   (5) DOMAINE BOUCLÉ .............. OUI — `modules/bulk_de/` (9 fichiers) ne
#       porte plus aucun site C10 (vérifié par le verrou source, et re-mesuré
#       par le garde `--list-all` filtré sur le dossier).
#
# ⚠️ **(2) est le verdict le plus FAIBLE des deux formes de preuve** (§2cr a
# obtenu une preuve COMPORTEMENTALE ; ici, non). C'est dit explicitement plutôt
# que masqué : ce lot a été choisi sur le NOMBRE de verdicts distincts prouvés
# (#79), pas sur la force d'un seul.
#
# ⚠️ **C16 / §2bn sans objet** : le message du garde est à **UN SEUL** argument
# (`stop(.t_fmt(...))`), déjà construit par `.t_fmt` ⇒ **aucun `paste0()`** à
# ajouter (§2bw : l'appliquer par réflexe serait un bruit).
#
# ⚠️ **Le handler d'`observeEvent` est un BLOC, pas une DÉFINITION** : le 3ᵉ
# élément de l'appel est `{ … }` ⇒ il faut l'**ÉVALUER**, jamais l'appeler
# (piège inverse de §2cn.3).
#
# ⚠️ **`shiny::Progress$new()` (L77) est appelé via `::` ⇒ NON mockable** : sans
# `withReactiveDomain(MockShinySession$new(), …)` le corps meurt AVANT le site
# (motif §2cc, débloqué §2cp.2).
# =============================================================================

source_project_file("modules/bulk_de/mod_bulk_de_multimethod.R")

.MBM_FILE <- "modules/bulk_de/mod_bulk_de_multimethod.R"

# Fragment ASCII du message du garde : le littéral source porte des échappements
# \u00xx (L109) ⇒ on assère une tranche SANS accent, insensible à l'encodage.
.MBM_MSG_ASCII <- "Au moins 2 m"
# Fragment ASCII du message de SUCCÈS (témoin de traversée, §2cj.3).
.MBM_OK_ASCII  <- "Comparaison multi-m"
# Fragment ASCII du message du gestionnaire avaleur (identification par CONTENU).
.MBM_HANDLER_ASCII <- "Erreur comparaison multi-m"

# --- AST : le BLOC (3ᵉ élément) de `observeEvent(input$run_multimethod, …)` ----
.mbm_handler_expr <- function() {
  p   <- parse(file.path(ts_project_root(), .MBM_FILE))
  out <- NULL
  walk <- function(x) {
    if (!is.null(out)) return(invisible(NULL))
    if (is.call(x)) {
      is_obs <- tryCatch(identical(x[[1]], quote(observeEvent)), error = function(e) FALSE)
      if (isTRUE(is_obs)) {
        l <- as.list(x)
        if (length(l) >= 3L) {
          d <- paste(deparse(l[[2]]), collapse = " ")
          if (grepl("run_multimethod", d, fixed = TRUE)) { out <<- l[[3]]; return(invisible(NULL)) }
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
  if (is.null(out)) stop("observeEvent(input$run_multimethod, ...) introuvable", call. = FALSE)
  out
}

# --- AST : le GESTIONNAIRE `error = function(e) …` du tryCatch L80 ------------
.mbm_outer_handler <- function() {
  p   <- parse(file.path(ts_project_root(), .MBM_FILE))
  out <- NULL
  walk <- function(x) {
    if (!is.null(out)) return(invisible(NULL))
    if (is.call(x)) {
      is_tc <- tryCatch(identical(x[[1]], quote(tryCatch)), error = function(e) FALSE)
      if (isTRUE(is_tc)) {
        l  <- as.list(x)
        nm <- names(l)
        for (i in seq_along(l)) {
          if (!is.null(nm) && identical(nm[i], "error")) {
            d <- paste(deparse(l[[i]]), collapse = " ")
            if (grepl(.MBM_HANDLER_ASCII, d, fixed = TRUE)) { out <<- l[[i]]; return(invisible(NULL)) }
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
  if (is.null(out)) stop("gestionnaire 'Erreur comparaison multi-methodes' introuvable", call. = FALSE)
  out
}

# --- Le VRAI `.t_fmt` (global.R:275), extrait par parse() --------------------
# ⚠️ On ne le RECOPIE pas (règle 3 : ne pas dupliquer une logique du dépôt) et on
# ne `source()` pas `global.R` (qui initialise l'application). On extrait la
# SEULE définition dont on a besoin, pour que l'interpolation testée soit la
# VRAIE — sans quoi l'assertion « le message varie » ne mesurerait que notre
# propre bouchon (défaut-signature : l'instrument mesure autre chose que l'outil).
.mbm_real_t_fmt <- function() {
  p   <- parse(file.path(ts_project_root(), "global.R"))
  out <- NULL
  walk <- function(x) {
    if (!is.null(out)) return(invisible(NULL))
    if (is.call(x)) {
      l <- as.list(x)
      if (identical(x[[1]], quote(`<-`)) && length(l) == 3L) {
        nm <- tryCatch(as.character(l[[2]]), error = function(e) "")
        if (identical(nm, ".t_fmt")) { out <<- eval(l[[3]], envir = globalenv()); return(invisible(NULL)) }
      }
      for (i in seq_along(l)) {
        ok <- tryCatch(is.call(l[[i]]), error = function(e) FALSE)
        if (isTRUE(ok)) walk(l[[i]])
      }
    }
    invisible(NULL)
  }
  for (i in seq_along(p)) walk(p[[i]])
  if (is.null(out)) stop(".t_fmt introuvable dans global.R", call. = FALSE)
  out
}

# --- Environnement enfant ----------------------------------------------------
# `rec` enregistre les etapes (temoins) ; `n_methods` pilote la BORNE.
.mbm_env <- function(rec, n_methods = 1L) {
  e <- new.env(parent = globalenv())
  # `.tr` reel avec i18n NULL ⇒ rend la CLE, qui EST la chaine FR (cf. PITFALLS).
  e$.tr    <- function(key, ...) key
  e$.t_fmt <- .mbm_real_t_fmt()
  e$`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  e$req    <- shiny::req                      # REEL (mesure §2cn.4)
  e$showNotification <- function(msg, ...) {
    rec(paste0("notify:", paste0(msg))); invisible(NULL)
  }
  e$multimethod_status_rv <- function(x) {
    rec(paste0("status:", if (is.null(x)) "NULL" else paste0("n_methods=", x$n_methods)))
    invisible(NULL)
  }
  # Helpers maison : a BOUCHONNER (la recherche retomberait sinon sur globalenv,
  # cf. §2cg : pour une chaine de N maillons, N injections).
  e$check_design_confounding <- function(meta, col, cov) FALSE
  e$design_saturation <- function(meta, col, covs) list(saturated = FALSE, n = nrow(meta), p = 1L)
  e$build_dds <- function(...) { rec("build_dds"); stop("build_dds ne doit pas etre atteint") }
  e$getAllDE <- function(...) {
    rec(paste0("getAllDE:", n_methods))
    # ⚠️ `paste0("m", integer(0))` rend `"m"` (LONGUEUR 1) — `paste0()` ne
    # propage PAS la longueur nulle : `names(x) <- <longueur 1>` sur un vecteur
    # de longueur 0 LEVE. On construit donc les noms explicitement.
    out <- lapply(seq_len(n_methods), function(i) data.frame(gene = "g1"))
    names(out) <- if (n_methods > 0L) paste0("m", seq_len(n_methods)) else character(0)
    out
  }
  e$rankConsensus <- function(...) { rec("rankConsensus"); data.frame(gene = "g1") }
  e$helpers <- list(design_str = function() "~ condition")
  meta <- data.frame(condition = c("A", "A", "B", "B"))
  e$global_data <- list(i18n = NULL, bulk_obj = list(metadata = meta))
  # `dds_full` porte deja `design_str_cache` identique ⇒ `needs_fit` est FAUX
  # ⇒ `build_dds()` (et donc DESeq2) n'est jamais atteint : prologue minimal.
  e$shared_rv <- list(
    filtered_counts = matrix(as.integer(seq_len(40)), nrow = 10),
    dds_full = structure(list(), design_str_cache = "~ condition")
  )
  e$input <- list(condition_col = "condition", group_ref = "A", group_target = "B",
                  covariates = NULL, shrink_lfc = FALSE,
                  padj_method = "BH", lfc_thresh = 1, padj_thresh = 0.05)
  e
}

# --- Execution : le corps est un BLOC => on l'EVALUE (jamais on ne l'appelle).
.mbm_run <- function(envir) {
  err <- tryCatch({
    shiny::withReactiveDomain(
      shiny::MockShinySession$new(),
      eval(.mbm_handler_expr(), envir = envir)
    )
    NULL
  }, error = function(e) list(msg = conditionMessage(e), class = class(e)))
  err
}

# ---------------------------------------------------------------------------
# TEMOIN D'EXECUTION : le garde 109 S'EXECUTE (prouve) mais l'erreur est AVALEE
# ---------------------------------------------------------------------------
test_that("mod_bulk_de_multimethod : le garde 109 est ATTEINT (temoin) puis AVALE", {
  skip_if_not(exists("MockShinySession", envir = asNamespace("shiny")),
              "shiny::MockShinySession indisponible")

  steps <- character(0)
  err <- .mbm_run(.mbm_env(function(s) steps <<- c(steps, s), n_methods = 1L))

  # 1) TEMOIN NON VACUANT : le gestionnaire L139-145 a notifie le message DU
  #    GARDE => le garde s'est bien execute (c'est son message qui ressort).
  expect_true(any(grepl(.MBM_MSG_ASCII, steps, fixed = TRUE)))
  # 2) ...et `getAllDE()` a bien ete tente AVANT lui (prologue reellement couru).
  expect_true(any(grepl("getAllDE:1", steps, fixed = TRUE)))
  # 3) Prologue minimal confirme : le chemin cache a evite `build_dds()`.
  expect_false(any(steps == "build_dds"))
  # 4) 🔴 AUCUNE erreur ne s'echappe => le gestionnaire l'a AVALEE.
  expect_null(err)
})

# ---------------------------------------------------------------------------
# La classe est INOBSERVABLE : c'est la justification du verrou source
# ---------------------------------------------------------------------------
test_that("mod_bulk_de_multimethod : la classe est INOBSERVABLE a l'execution", {
  skip_if_not(exists("MockShinySession", envir = asNamespace("shiny")),
              "shiny::MockShinySession indisponible")

  steps <- character(0)
  err <- .mbm_run(.mbm_env(function(s) steps <<- c(steps, s), n_methods = 1L))

  # La classe n'apparait dans AUCUN canal observable : ni en erreur remontee...
  expect_null(err)
  # ...ni dans la notification, qui ne porte que le MESSAGE.
  expect_false(any(grepl("bulk_de_multimethod_error", steps, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# Le message du garde VARIE avec son entree (§2ch.3 / #56) — sinon un message
# code en dur passerait ce test.
# ---------------------------------------------------------------------------
test_that("le message du garde 109 varie avec le nombre de methodes", {
  skip_if_not(exists("MockShinySession", envir = asNamespace("shiny")),
              "shiny::MockShinySession indisponible")

  steps1 <- character(0)
  invisible(.mbm_run(.mbm_env(function(s) steps1 <<- c(steps1, s), n_methods = 1L)))
  steps0 <- character(0)
  invisible(.mbm_run(.mbm_env(function(s) steps0 <<- c(steps0, s), n_methods = 0L)))

  # L'interpolation `{n}` est celle du VRAI `.t_fmt` (global.R:275).
  expect_true(any(grepl("comparer (1 ", steps1, fixed = TRUE)))
  expect_true(any(grepl("comparer (0 ", steps0, fixed = TRUE)))
  # Et le premier message ne matche plus quand l'entree change.
  expect_false(any(grepl("comparer (0 ", steps1, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# Controle de BORNE **avec TEMOIN DE TRAVERSEE** (§2cj.3 / #60) : au-dessus de la
# borne le garde ne tire PAS, et l'execution CONTINUE (prouve par le succes).
# ---------------------------------------------------------------------------
test_that("le garde 109 est conditionnel : 2 methodes => il ne tire pas et l'execution CONTINUE", {
  skip_if_not(exists("MockShinySession", envir = asNamespace("shiny")),
              "shiny::MockShinySession indisponible")

  steps <- character(0)
  err <- .mbm_run(.mbm_env(function(s) steps <<- c(steps, s), n_methods = 2L))

  # Temoin de TRAVERSEE : le garde n'a PAS tire...
  expect_false(any(grepl(.MBM_MSG_ASCII, steps, fixed = TRUE)))
  # ...et une etape JOURNALISEE APRES lui prouve qu'on l'a franchi (sans ce
  # temoin, « notre message est absent » serait vrai aussi en cas d'echec AVANT).
  expect_true(any(grepl(.MBM_OK_ASCII, steps, fixed = TRUE)))
  expect_true(any(steps == "status:n_methods=2"))
  expect_null(err)
})

# ---------------------------------------------------------------------------
# Justification EXECUTABLE du verrou source : le gestionnaire ne RELANCE pas
# ---------------------------------------------------------------------------
test_that("le gestionnaire englobant journalise + notifie SANS stop( (falsifiable)", {
  d <- paste(deparse(.mbm_outer_handler()), collapse = "\n")
  # Il rend la main en notifiant...
  expect_true(grepl("showNotification", d, fixed = TRUE))
  # ...et NE relance PAS. Si un jour il relancait, ce test echouerait et
  # signalerait que le lot devient prouvable a l'execution (§2cd.2).
  expect_false(grepl("stop(", d, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Verrou source CIBLE : le garde 109 porte bien NOTRE classe
# ---------------------------------------------------------------------------
test_that("le garde 109 porte la classe bulk_de_multimethod_error (verrou source cible)", {
  # ⚠️ La classe etant INOBSERVABLE a l'execution, la SOURCE est le seul canal
  # ou elle est verifiable — et c'est exactement ce que C10 mesure.
  p <- parse(file.path(ts_project_root(), .MBM_FILE))
  hits <- character(0)
  walk <- function(x) {
    if (is.call(x)) {
      is_stop <- tryCatch(identical(x[[1]], quote(stop)), error = function(e) FALSE)
      if (isTRUE(is_stop)) {
        d <- gsub("\\s+", " ", paste(deparse(x), collapse = " "))
        if (grepl(.MBM_MSG_ASCII, d, fixed = TRUE)) hits <<- c(hits, d)
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
  expect_true(grepl('class = "bulk_de_multimethod_error"', hits[[1]], fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Verrou source : mod_bulk_de_multimethod.R ne doit plus contribuer aucun C10
# ---------------------------------------------------------------------------
test_that("mod_bulk_de_multimethod.R ne contribue aucun signalement C10", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools/check_conventions.R"), envir = e)
  before <- length(e$.REPORT$warns)
  e$check_c10_error_style(file.path(ts_project_root(), .MBM_FILE))
  w <- e$.REPORT$warns
  n <- length(w) - before
  if (n > 0L) {
    lignes <- vapply(w[(before + 1L):length(w)], function(x) x$line, integer(1))
    cat("Sites C10 restants dans", .MBM_FILE, ":", paste(sort(lignes), collapse = ", "), "\n")
  }
  expect_equal(n, 0L)
})

# =============================================================================
# test-mod-bulk.R — tests for modules/bulk/mod_bulk.R
# =============================================================================
# 30ᵉ incrément de la dette de conventions (2026-09-18, §2cp).
# Créé AVANT la conversion (règle 5 : test d'abord, rouge d'abord) ⇒ ce fichier
# doit ÉCHOUER sur l'assertion de classe et sur le verrou source tant que le
# `stop()` de la ligne 427 ne porte pas `class = "mod_bulk_error"`.
#
# 🔑 CLASSE `mod_bulk_error` — décision §2co.1 appliquée. `mod_bulk.R` est le
# **2ᵉ routeur parent** (« Auto-pipeline » de l'onglet Bulk) : mapping mécanique
# ⇒ `bulk_error`, nom **trop large** au même titre que `sc_error` (il cohabite
# avec `bulk_de_error`, `bulk_dose_error`, `bulk_merge_error`, … — **17**
# classes `bulk_*_error` existent, et AUCUNE n'est `bulk_error`). Branche (b) de
# `CONVENTIONS.md` §7 : la classe porte le FICHIER ⇒ **`mod_bulk_error`**.
#
# ---------------------------------------------------------------------------
# 🔴 RÉSULTAT CENTRAL DE CE LOT — « ATTEIGNABLE » n'est PAS « OBSERVABLE »
# ---------------------------------------------------------------------------
# Le site 427 est **ATTEINT à l'exécution** (prouvé ci-dessous par témoin) mais
# sa classe est **INOBSERVABLE** : le `tryCatch` ouvert L344 ferme **L528** et
# son gestionnaire (L525-528) **journalise + notifie** au lieu de **relancer**
# ⇒ il **AVALE** l'erreur. C'est le critère §2cd.2 (« ce n'est pas `tryCatch`
# qui condamne un site, c'est le GESTIONNAIRE »).
#
# ⇒ Raffinement de §2cn.1 : §2cn.1 a montré que « réactif » n'est pas
# « inobservable » (le CORPS est du R pur extractible). Ce lot ajoute la couche
# **indépendante** suivante : même un corps **prouvé atteint** peut rester
# **inobservable**, parce qu'un gestionnaire avaleur se trouve **sur le chemin
# de sortie**. ⇒ **Deux verdicts séparés** : (1) le corps est-il atteignable ?
# (2) l'erreur remonte-t-elle ? Le lot `mod_bulk.R` répond **oui** puis **non**.
#
# 🟢 TECHNIQUE NOUVELLE, MESURÉE (§2cp.2) : deux primitives du prologue exigent
# une SESSION et échouent hors app — `removeModal()` (« attempt to apply
# non-function ») et `shiny::Progress$new()` (« Can only use Progress$new()
# inside a Shiny app »). Cette seconde est appelée via `::` ⇒ **non mockable**,
# et c'est précisément le motif qui avait fait conclure « injoignable » (§2cc).
# ⇒ **`shiny::withReactiveDomain(shiny::MockShinySession$new(), …)` les
# débloque TOUTES LES DEUX.** Le corps devient donc atteignable.
#
# ⚠️ **3ᵉ verrou : `DESeq2::estimateSizeFactors()` (L383), aussi via `::`.**
# Comme `::` résout le VRAI namespace (`substitute(pkg)`), un binding local nommé
# `DESeq2` est IGNORÉ. ⇒ Le mock de `build_dds()` doit rendre un **VRAI**
# `DESeqDataSet` (`DESeq2::DESeqDataSetFromMatrix`), que `estimateSizeFactors()`
# accepte. C'est le prologue le plus lourd du chantier (~14 bouchons), mais il
# est **payé** : il prouve le témoin.
#
# ⚠️ **Le handler d'`observeEvent` est un BLOC, pas une DÉFINITION** : le 3ᵉ
# élément de l'appel est `{ … }`. Il faut donc l'**évaluer** (`eval`), **pas**
# l'appeler — c'est l'**inverse** du piège §2cn.3 (« `eval()` d'une définition
# n'est pas un appel »). Se tromper donne `could not find function "f"`.
#
# ⚠️ **C16 / §2bn sans objet** : message à **UN SEUL** argument, statique.
#
# 🔴 **PREUVE = VERROU SOURCE + TÉMOIN D'EXÉCUTION** (et non exécution de la
# classe). Le verrou source est **rendu falsifiable** : le dernier test assère
# que le gestionnaire englobant journalise + notifie **SANS `stop(`** ⇒ si un
# jour il relance, le test **échoue** et signale que le lot devient
# **prouvable à l'exécution** (§2cc, motif repris).
# =============================================================================

source_project_file("modules/bulk/mod_bulk.R")

.MBK_FILE <- "modules/bulk/mod_bulk.R"

# Fragment ASCII du message du garde (le littéral source porte des echappements
# \u00xx : on assère une tranche SANS accent pour rester insensible a l'encodage).
.MBK_MSG_ASCII <- "Aucune paire n'a pu"

# --- AST : le BLOC (3ᵉ élément) de `observeEvent(input$ap_confirm, …)` ---------
.mbk_handler_expr <- function() {
  p   <- parse(file.path(ts_project_root(), .MBK_FILE))
  out <- NULL
  walk <- function(x) {
    if (!is.null(out)) return(invisible(NULL))
    if (is.call(x)) {
      is_obs <- tryCatch(identical(x[[1]], quote(observeEvent)), error = function(e) FALSE)
      if (isTRUE(is_obs)) {
        l <- as.list(x)
        if (length(l) >= 3L) {
          d <- paste(deparse(l[[2]]), collapse = " ")
          if (grepl("ap_confirm", d, fixed = TRUE)) { out <<- l[[3]]; return(invisible(NULL)) }
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
  if (is.null(out)) stop("observeEvent(input$ap_confirm, ...) introuvable", call. = FALSE)
  out
}

# --- AST : le GESTIONNAIRE `error = function(e) …` qui porte « Erreur pipeline »
.mbk_outer_handler <- function() {
  p   <- parse(file.path(ts_project_root(), .MBK_FILE))
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
            if (grepl("Erreur pipeline", d, fixed = TRUE)) { out <<- l[[i]]; return(invisible(NULL)) }
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
  if (is.null(out)) stop("gestionnaire 'Erreur pipeline' introuvable", call. = FALSE)
  out
}

# --- Environnement enfant ----------------------------------------------------
# `rec` enregistre les etapes (temoins) ; `dispatch_ok` sert au controle de BORNE.
.mbk_env <- function(rec, dispatch_ok = FALSE) {
  e <- new.env(parent = globalenv())
  e$.tr    <- function(s, ...) s
  e$.t_fmt <- function(fmt, ...) fmt
  # cf. R/core/io_helpers.R:47
  e$`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  e$req    <- shiny::req            # REEL (mesure §2cn.4)
  e$removeModal <- function() invisible(NULL)   # primitive d'UI
  e$showNotification <- function(msg, ...) {
    rec(paste0("notify:", paste0(msg))); invisible(NULL)
  }
  e$auto_log_rv <- function(...) { rec("log"); invisible(NULL) }
  # Helpers maison : a BOUCHONNER (la recherche retomberait sinon sur globalenv).
  e$design_saturation  <- function(meta, col) list(saturated = FALSE, n = nrow(meta), p = 1L)
  e$detect_gene_id_type <- function(x) "unknown"
  e$filter_bulk_counts  <- function(counts, ...) counts
  # ⚠️ `DESeq2::estimateSizeFactors()` (L383) est appele via `::` => NON mockable
  # => `build_dds` doit rendre un VRAI DESeqDataSet (cf. en-tete).
  e$build_dds <- function(...) {
    rec("build_dds")
    m  <- matrix(as.integer(seq_len(40)), nrow = 10,
                 dimnames = list(paste0("g", 1:10), paste0("s", 1:4)))
    cd <- data.frame(condition = factor(c("A", "A", "B", "B")))
    rownames(cd) <- colnames(m)
    DESeq2::DESeqDataSetFromMatrix(m, cd, ~1)
  }
  e$get_vst_matrix <- function(dds) { rec("get_vst_matrix"); matrix(1, 10, 4) }
  e$run_bulk_de_dispatch <- function(...) {
    rec("dispatch")
    if (dispatch_ok) {
      data.frame(gene = "g1", log2FoldChange = 1, padj = 0.01, stringsAsFactors = FALSE)
    } else {
      stop("__dispatch_ko__")
    }
  }
  e$.normalize_de_cols <- function(x, ...) x
  meta <- data.frame(condition = c("A", "A", "B", "B", "C", "C"))
  e$global_data <- list(bulk_obj = list(counts = matrix(1, 10, 6), metadata = meta))
  e$shared_rv   <- list()
  e$input <- list(ap_condition = "condition", ap_no_rep_enable = FALSE,
                  ap_no_rep_attest = FALSE, ap_no_rep_bcv = 0.4,
                  ap_map_ids = FALSE, ap_map_organism = "human",
                  ap_min_count = 10, ap_min_samples = 2,
                  ap_pairwise = TRUE, ap_engine = "edger",
                  ap_lfc = 1, ap_padj = 0.05)
  e
}

# --- Execution : le corps est un BLOC => on l'EVALUE (jamais on ne l'appelle).
# ⚠️ `withReactiveDomain(MockShinySession)` est REQUIS : sans lui le corps
# echoue a `shiny::Progress$new()` (L337) AVANT d'atteindre le site.
.mbk_run <- function(envir) {
  err <- tryCatch({
    shiny::withReactiveDomain(
      shiny::MockShinySession$new(),
      eval(.mbk_handler_expr(), envir = envir)
    )
    NULL
  }, error = function(e) list(msg = conditionMessage(e), class = class(e)))
  err
}

# ---------------------------------------------------------------------------
# TEMOIN D'EXECUTION : le garde 427 S'EXECUTE (prouve) mais l'erreur est AVALEE
# ---------------------------------------------------------------------------
test_that("mod_bulk : le garde 427 est ATTEINT (temoin) puis AVALE", {
  skip_if_not(exists("MockShinySession", envir = asNamespace("shiny")),
              "shiny::MockShinySession indisponible")

  steps <- character(0)
  err <- .mbk_run(.mbk_env(function(s) steps <<- c(steps, s)))

  # 1) TEMOIN DE TRAVERSEE NON VACUANT : le gestionnaire L525-528 a notifie le
  #    message DU GARDE => le garde s'est bien execute (ce n'est pas une
  #    deduction, c'est son message qui ressort par la notification).
  expect_true(any(grepl(.MBK_MSG_ASCII, steps, fixed = TRUE)))
  # 2) ...et les 3 paires ont bien ete tentees avant lui.
  expect_true(sum(steps == "dispatch") >= 3L)
  # 3) 🔴 AUCUNE erreur ne s'echappe => le gestionnaire l'a AVALEE.
  expect_null(err)
})

# ---------------------------------------------------------------------------
# La classe est INOBSERVABLE : c'est la justification du verrou source
# ---------------------------------------------------------------------------
test_that("mod_bulk : la classe mod_bulk_error est INOBSERVABLE a l'execution", {
  skip_if_not(exists("MockShinySession", envir = asNamespace("shiny")),
              "shiny::MockShinySession indisponible")

  steps <- character(0)
  err <- .mbk_run(.mbk_env(function(s) steps <<- c(steps, s)))

  # La classe n'apparait dans AUCUN canal observable : ni en erreur remontee...
  expect_null(err)
  # ...ni dans la notification, qui ne porte que le MESSAGE.
  expect_false(any(grepl("mod_bulk_error", steps, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# Controle de BORNE : le garde est CONDITIONNEL (une paire reussit => pas de tir)
# ---------------------------------------------------------------------------
test_that("le garde 427 est conditionnel : une paire reussit => il ne tire pas", {
  skip_if_not(exists("MockShinySession", envir = asNamespace("shiny")),
              "shiny::MockShinySession indisponible")

  steps <- character(0)
  err <- .mbk_run(.mbk_env(function(s) steps <<- c(steps, s), dispatch_ok = TRUE))

  # Temoin de traversee : les 3 paires ont ete tentees (on est bien alle jusque-la)...
  expect_true(sum(steps == "dispatch") >= 3L)
  # ...et le garde n'a PAS tire, puisque `ok > 0`.
  expect_false(any(grepl(.MBK_MSG_ASCII, steps, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# Justification EXECUTABLE du verrou source : le gestionnaire ne RELANCE pas
# ---------------------------------------------------------------------------
test_that("le gestionnaire englobant journalise + notifie SANS stop( (falsifiable)", {
  d <- paste(deparse(.mbk_outer_handler()), collapse = "\n")
  # Il rend la main en notifiant...
  expect_true(grepl("showNotification", d, fixed = TRUE))
  # ...et NE relance PAS. Si un jour il relancait, ce test echouerait et
  # signalerait que le lot devient prouvable a l'execution (§2cd.2).
  expect_false(grepl("stop(", d, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Verrou source CIBLE : le garde 427 porte bien NOTRE classe
# ---------------------------------------------------------------------------
test_that("le garde 427 porte la classe mod_bulk_error (verrou source cible)", {
  # ⚠️ La classe etant INOBSERVABLE a l'execution, la SOURCE est le seul canal
  # ou elle est verifiable — et c'est exactement ce que C10 mesure. On assere
  # donc le `stop()` du site, precisement, plutot que le seul compteur C10.
  p <- parse(file.path(ts_project_root(), .MBK_FILE))
  hits <- character(0)
  walk <- function(x) {
    if (is.call(x)) {
      is_stop <- tryCatch(identical(x[[1]], quote(stop)), error = function(e) FALSE)
      if (isTRUE(is_stop)) {
        d <- gsub("\\s+", " ", paste(deparse(x), collapse = " "))
        if (grepl(.MBK_MSG_ASCII, d, fixed = TRUE)) hits <<- c(hits, d)
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
  expect_true(grepl('class = "mod_bulk_error"', hits[[1]], fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Verrou source : mod_bulk.R ne doit plus contribuer aucun C10
# ---------------------------------------------------------------------------
test_that("mod_bulk.R ne contribue aucun signalement C10", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools/check_conventions.R"), envir = e)
  before <- length(e$.REPORT$warns)
  e$check_c10_error_style(file.path(ts_project_root(), .MBK_FILE))
  w <- e$.REPORT$warns
  n <- length(w) - before
  if (n > 0L) {
    lignes <- vapply(w[(before + 1L):length(w)], function(x) x$line, integer(1))
    cat("Sites C10 restants dans mod_bulk.R :", paste(sort(lignes), collapse = ", "), "\n")
  }
  expect_equal(n, 0L)
})

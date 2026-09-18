# =============================================================================
# test-mod-bulk-pathways.R — tests for modules/bulk/mod_bulk_pathways.R
# =============================================================================
# 28ᵉ incrément de la dette de conventions (2026-09-18, §2cn).
# Créé AVANT la conversion (règle 5 : test d'abord, rouge d'abord) ⇒ ce fichier
# doit ÉCHOUER sur l'assertion de classe tant que le `stop()` du site 533 ne
# porte pas `class = "bulk_pathways_error"`.
#
# 🔴 **CE LOT RÉFUTE §2cm.5** — la phrase « les 10 sites restants de `modules/`
# sont tous réactifs, donc inobservables ». Le §2cm.5 avait classé les sites par
# leur **construit englobant** ; c'est exactement le raccourci que §2cj avait
# déjà réfuté (§2ce.2 : le prédicteur prédit le **COÛT** de la preuve, pas sa
# **POSSIBILITÉ**). Mesure : ce site n'est pas dans un `observeEvent` mais dans
# une **FONCTION NOMMÉE locale** :
#
#     .gsea_curve_plot_fn <- function() { … }
#
# ⇒ on l'extrait par l'**AST** et on l'**appelle** dans un **environnement
# enfant** — la même technique que les corps de `mirai` (§2cj.1), sans démon.
#
# ⚠️ **Extraction AST : le piège est qu'une définition n'est pas un appel.**
# `eval()` sur l'expression `function() {…}` rend une **FONCTION** ; il faut
# ensuite l'**APPELER**. Mesuré : sans l'appel, le corps ne s'exécute pas du
# tout et `requireNamespace` n'est jamais invoqué — la sonde conclut à tort
# « aucune erreur ».
#
# ⚠️ **`req()` n'est PAS mocké** : il est fourni par `shiny::req`. Mesuré :
# hors contexte réactif, `req()` avec des arguments **truthy** passe
# normalement (`getDefaultReactiveDomain()` est `NULL` mais n'est consulté que
# si un argument est falsy). ⇒ fidélité maximale, et le site reste atteint pour
# la bonne raison.
#
# ⚠️ **C16 / §2bn sans objet** : le message est à **UN SEUL** argument et
# entièrement statique ⇒ **aucun `paste0()`**. On assère le message **ENTIER**
# (§2bx.3).
#
# ⚠️ **Contrôle de BORNE à témoin DOUBLE** : on enregistre les appels à
# `requireNamespace()` — ce qui prouve que l'exécution a bien ATTEINT la ligne
# 532 — et l'échec qui suit vient d'**EN AVAL** (`enrichplot::gseaplot2()`
# appelé pour de vrai). Sans ce témoin, « notre classe est absente » serait
# vrai aussi si le code avait échoué AVANT la garde (§2cj.3).
# =============================================================================

source_project_file("modules/bulk/mod_bulk_pathways.R")

.MBP_FILE <- "modules/bulk/mod_bulk_pathways.R"

# --- Extraction AST : `NAME <- function(...) {...}` à n'importe quelle profondeur
.mbp_fun_expr <- function(name) {
  p <- parse(file.path(ts_project_root(), .MBP_FILE))
  find <- function(x) {
    if (is.call(x)) {
      is_assign <- tryCatch(identical(x[[1]], quote(`<-`)), error = function(e) FALSE)
      if (isTRUE(is_assign)) {
        lhs  <- tryCatch(x[[2]], error = function(e) NULL)
        same <- tryCatch(identical(lhs, as.name(name)), error = function(e) FALSE)
        if (isTRUE(same)) return(x[[3]])
      }
      l <- as.list(x)
      for (i in seq_along(l)) {
        ok <- tryCatch(is.call(l[[i]]), error = function(e) FALSE)
        if (isTRUE(ok)) {
          r <- find(l[[i]])
          if (!is.null(r)) return(r)
        }
      }
    }
    NULL
  }
  for (i in seq_along(p)) {
    r <- find(p[[i]])
    if (!is.null(r)) return(r)
  }
  stop("definition de '", name, "' introuvable dans ", .MBP_FILE, call. = FALSE)
}

# --- Environnement enfant : `req` REEL + mock de `requireNamespace` -----------
.mbp_env <- function(enrichplot_ok, rec = NULL) {
  e <- new.env(parent = globalenv())
  e$.tr    <- function(s) s
  e$.t_fmt <- function(s, ...) s
  e$req    <- shiny::req          # REEL (cf. en-tete : truthy hors reactif OK)
  e$input  <- list(gsea_curve_pathway      = "PATH_A",
                   gsea_curve_pvalue_table = TRUE)
  # `attr(..., "gsea_obj")` doit etre non-NULL, sinon `req()` sort AVANT la garde.
  e$shared_rv <- list(
    pathway_results = structure(data.frame(ID = "PATH_A"), gsea_obj = list(fake = TRUE)))
  e$requireNamespace <- function(package, ...) {
    if (!is.null(rec)) rec(package)
    if (identical(package, "enrichplot")) enrichplot_ok else base::requireNamespace(package, ...)
  }
  e
}

# ⚠️ `eval()` rend la FONCTION ; il faut l'APPELER (cf. en-tete).
.mbp_call <- function(envir) {
  tryCatch({
    f <- eval(.mbp_fun_expr(".gsea_curve_plot_fn"), envir = envir)
    f()
    NULL
  },
  error = function(e) list(msg = conditionMessage(e), class = class(e)))
}

.MBP_MSG <- "Package 'enrichplot' requis (BiocManager::install('enrichplot'))."

# ---------------------------------------------------------------------------
# Site 533 — garde de dépendance enrichplot, dans une FONCTION NOMMÉE locale
# ---------------------------------------------------------------------------
test_that("mod_bulk_pathways : enrichplot absent ⇒ classe (site 533)", {
  res <- .mbp_call(.mbp_env(enrichplot_ok = FALSE))
  expect_false(is.null(res))                                  # erreur levée…
  expect_true("bulk_pathways_error" %in% res$class)           # …avec NOTRE classe
  # Message a UN SEUL argument et entierement statique ⇒ message ENTIER (§2bx.3).
  expect_identical(res$msg, .MBP_MSG)
})

# ---------------------------------------------------------------------------
# Contrôle de BORNE (site 533) — la garde est CONDITIONNELLE
# ---------------------------------------------------------------------------
test_that("la garde enrichplot est conditionnelle : present ⇒ pas notre classe", {
  calls <- character(0)
  res <- .mbp_call(.mbp_env(enrichplot_ok = TRUE,
                            rec = function(p) calls <<- c(calls, p)))

  # ⚠️ TÉMOIN DE TRAVERSÉE (§2cj.3) : l'appel enregistré prouve que l'exécution a
  # ATTEINT la ligne 532. Sans lui, l'absence de notre classe serait vraie aussi
  # si le corps avait échoué AVANT la garde (p. ex. dans `req()`).
  expect_true("enrichplot" %in% calls)
  # Et l'échec qui suit est bien EN AVAL : `enrichplot::gseaplot2()` est appelé
  # pour de vrai sur un faux objet. Il échoue — mais PAS avec notre classe.
  expect_false(is.null(res))
  expect_false("bulk_pathways_error" %in% res$class)
})

# ---------------------------------------------------------------------------
# Verrou source : mod_bulk_pathways.R ne doit plus contribuer aucun C10
# ---------------------------------------------------------------------------
test_that("mod_bulk_pathways.R ne contribue aucun signalement C10", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools/check_conventions.R"), envir = e)
  before <- length(e$.REPORT$warns)
  e$check_c10_error_style(file.path(ts_project_root(), .MBP_FILE))
  w <- e$.REPORT$warns
  n <- length(w) - before
  if (n > 0L) {
    lignes <- vapply(w[(before + 1L):length(w)], function(x) x$line, integer(1))
    cat("Sites C10 restants dans mod_bulk_pathways.R :",
        paste(sort(lignes), collapse = ", "), "\n")
  }
  expect_equal(n, 0L)
})

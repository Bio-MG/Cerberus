# =============================================================================
# test-sc-bpcells.R — tests for R/sc/sc_bpcells.R
# =============================================================================
# 22ᵉ incrément de la dette de conventions (2026-09-18).
# Créé AVANT la conversion (règle 5 : test d'abord, rouge d'abord) ⇒ ce
# fichier doit ÉCHOUER sur les assertions de classe tant que le `stop()` ne
# porte pas `class = "sc_bpcells_error"`.
#
# 🟢 Lot choisi par RENDEMENT × PREUVE (§2bx.6) : `R/` paie **C10 + C9** (−2),
# et le site 60 est une **garde en TÊTE de fonction** (`if (!.bpcells_available())`).
# ⚠️ Aucun test hérité : `sc_bpcells.R` EST sourcé par deux tests
# (`test-mod-sc-annotation.R`, `test-sc-auto-pipeline.R`) mais **aucun ne le
# couvre SEUL** ⇒ pas de `git mv` gratuit (§2cc.1, §2cd.4, §2cf.2).
#
# ⚠️⚠️ **PIÈGE MESURÉ, ET C'EST LE CŒUR DE CE LOT : la technique de
# l'environnement enfant (§2bz.3) ne se propage PAS à une indirection à DEUX
# niveaux.** `convert_seurat_to_bpcells()` n'appelle pas `requireNamespace()`
# directement : il appelle le helper **maison** `.bpcells_available()`, qui
# l'appelle. Re-pointer seulement l'environnement de la fonction externe ne
# suffit donc PAS — la recherche de `.bpcells_available` part de `e`, ne l'y
# trouve pas, **retombe sur `globalenv`** (la version non mockée) ⇒
# `requireNamespace` réel ⇒ `TRUE` ⇒ **la garde ne tire pas** et le test
# échoue sur un `could not find function "GetAssayData"` (mesuré, cf. sonde).
# ⇒ Il faut **déposer aussi le helper mocké DANS l'environnement enfant**.
# Le VRAI corps de `.bpcells_available()` est conservé (seul `requireNamespace`
# est mocké), donc la garde est exercée pour de bon.
#
# 🟢 **Contrôle de BORNE** (§2cf.1) : la garde est **conditionnelle**. Un
# second test vérifie qu'avec le VRAI environnement (BPCells installé) notre
# classe **ne tire pas** — sinon une garde inconditionnelle passerait le test
# d'erreur sans rien prouver.
#
# ⚠️ **Dépendance d'exécution invisible au verrou source** (§2by.6 #3) :
# `sc_bpcells.R` appelle `GetAssayData()` **sans préfixe** (4 sites). Le test
# d'erreur n'en dépend pas (la garde est en tête), mais le **contrôle** franchit
# la garde et y tombe — il n'asserte donc que l'ABSENCE de notre classe, jamais
# le texte de l'erreur suivante.
# =============================================================================

source_project_file("R/sc/sc_bpcells.R")

.scb_err <- function(expr) {
  tryCatch({ expr; NULL },
           error = function(e) list(msg = conditionMessage(e), class = class(e)))
}

.MSG_BPCELLS <- "Package 'BPCells' non installé. Installez-le via remotes::install_github('bnprks/BPCells/r')."

# Mock de dépendance à DEUX niveaux (cf. l'en-tête) : `requireNamespace` est
# mocké dans `e`, ET `.bpcells_available` y est déposé après re-pointage de son
# environnement, pour que la recherche depuis `convert_seurat_to_bpcells`
# (dont l'environnement est `e`) le trouve AVANT de retomber sur globalenv.
.scb_no_bpcells <- function(fun) {
  e <- new.env(parent = globalenv())
  e$requireNamespace <- function(package, ...) FALSE
  g <- get(".bpcells_available", envir = globalenv())
  environment(g) <- e
  e$.bpcells_available <- g
  f <- get(fun, envir = globalenv())
  environment(f) <- e
  f
}

# ---------------------------------------------------------------------------
# convert_seurat_to_bpcells() — garde de dépendance, en tête de fonction
# ---------------------------------------------------------------------------
test_that("convert_seurat_to_bpcells : BPCells absent (60)", {
  # Message à UN SEUL argument ⇒ **aucun `paste0()`** (§2bw.4 ; C16 ne concerne
  # que les `stop()` multi-arguments, §2bn).
  # Le message est **entièrement statique** ⇒ on assère le message **ENTIER**,
  # jamais un préfixe : une assertion de préfixe ne verrait pas une troncature
  # (§2bx.3, §2cd).
  res <- .scb_err(.scb_no_bpcells("convert_seurat_to_bpcells")(NULL))
  expect_identical(res$msg, .MSG_BPCELLS)
  expect_true("sc_bpcells_error" %in% res$class)
})

# ---------------------------------------------------------------------------
# Contrôle de BORNE — la garde est CONDITIONNELLE, pas inconditionnelle
# ---------------------------------------------------------------------------
test_that("la garde est conditionnelle : BPCells disponible ⇒ pas de sc_bpcells_error", {
  skip_if_not_installed("BPCells")
  expect_true(.bpcells_available())            # précondition documentée
  # La garde est franchie ; l'erreur qui suit vient de `GetAssayData()` (appel
  # non préfixé) — on n'asserte donc QUE l'absence de notre classe.
  res <- .scb_err(suppressWarnings(convert_seurat_to_bpcells(NULL)))
  expect_false(is.null(res))                   # une erreur est bien levée…
  expect_false("sc_bpcells_error" %in% res$class)   # …mais PAS la garde
})

# ---------------------------------------------------------------------------
# Garde-fou du mock (§2bz.3) : il ne doit PAS fuiter, ni corrompre les vrais
# ---------------------------------------------------------------------------
test_that("le mock de dependance ne fuit pas et ne corrompt rien", {
  invisible(tryCatch(.scb_no_bpcells("convert_seurat_to_bpcells")(NULL),
                     error = function(e) NULL))
  expect_false(exists("requireNamespace", envir = globalenv(), inherits = FALSE))
  expect_true(requireNamespace("stats", quietly = TRUE))
  # les VRAIES fonctions sont intactes : le mock a copié, jamais muté
  expect_true(.bpcells_available())
  expect_identical(environment(get("convert_seurat_to_bpcells", envir = globalenv())),
                   globalenv())
})

# ---------------------------------------------------------------------------
# Verrou source : sc_bpcells.R ne doit plus contribuer aucun C10
# ---------------------------------------------------------------------------
test_that("sc_bpcells.R ne contribue aucun signalement C10", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools/check_conventions.R"), envir = e)
  before <- length(e$.REPORT$warns)
  e$check_c10_error_style(file.path(ts_project_root(), "R/sc/sc_bpcells.R"))
  w <- e$.REPORT$warns
  n <- length(w) - before
  if (n > 0L) {
    lignes <- vapply(w[(before + 1L):length(w)], function(x) x$line, integer(1))
    cat("Sites C10 restants dans sc_bpcells.R :",
        paste(sort(lignes), collapse = ", "), "\n")
  }
  expect_equal(n, 0L)
})

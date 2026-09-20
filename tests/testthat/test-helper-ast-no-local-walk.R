# =============================================================================
# test-helper-ast-no-local-walk.R — verrou : plus AUCUNE copie locale de `walk()`
# =============================================================================
# Défaut d'OUTILLAGE soldé au 47e incrément (2026-09-20, §2dg) : le parcours
# récursif d'AST (`walk()`) était **copié à l'identique 14 fois dans 5 fichiers
# de test**. Chaque incrément du chantier C10 en ajoutait une de plus : la
# duplication **CROISSAIT avec le chantier qu'elle servait**.
#
# ⚠️ `check_duplication.R` **NE SCANNE PAS `tests/`** — c'était donc un angle
# mort : aucun garde ne signalait ces 14 copies. `helper-ast.R` (créé au §2ct
# pour cette dette) ne les avait **jamais** remplacées jusqu'ici.
#
# ⇒ Ce fichier est le **garde manquant** : il échoue si une copie locale
# réapparaît. Sans lui, la dette se reconstitue par simple oubli — c'est
# exactement ce qui s'est produit pendant 34 incréments.
#
# 🔑 Ce qu'on mesure : une **DÉFINITION** `x <- function(...)` dont le nom est
# `walk` (ou `visit`, le nom employé par `helper-ast.R` lui-même). On ne compte
# PAS les APPELS (`walk(...)`) : un appel n'est pas une duplication.
#
# ⚠️ `helper-ast.R` porte `ts_ast_walk` ET une fonction interne nommée `visit`
# dans `ts_ast_find_all`/`ts_ast_find_in_trycatch` : ce sont les **primitives
# partagées**, pas des copies. On les EXCLUT explicitement (sinon le garde
# s'auto-interdirait d'exister), et on l'asserte pour que l'exclusion soit
# visible et non un trou silencieux.
# =============================================================================

.THAW_DIR <- "tests/testthat"

# ⚠️ `helper-ast.R` est le PORTEUR légitime du parcours : il définit `visit` en
# interne dans `ts_ast_find_all`/`ts_ast_find_in_trycatch`. L'inclure ferait
# s'auto-interdire au garde d'exprimer sa propre primitive — on l'exclut donc par
# son CHEMIN, et le test suivant asserte cette exclusion pour qu'elle soit
# visible plutôt que silencieuse.
.THAW_SELF <- "tests/testthat/helper-ast.R"

# Fichiers de test qui portent légitimement un parcours MAISON (aucun aujourd'hui).
# Liste VOLONTAIREMENT vide : toute exception future devra être écrite ici, donc
# devenir visible en revue — c'est le but.
.THAW_EXEMPT <- c(.THAW_SELF)

# --- Sonde : les définitions locales de `walk`/`visit` dans un texte ----------
#
# ⚠️ MESURÉ : `regexec()` rend le match COMPLET **plus** les groupes de capture
# (`len=2` ici, pas 3) — j'avais écrit `== 3L`, la sonde ne matchait donc RIEN et
# le test passait au vert sur un fichier qui portait une copie. Le garde était
# AVEUGLE, exactement le défaut qu'il est censé interdire.
#
# ⚠️ Et un scan LIGNE À LIGNE rate la 2ᵉ définition d'une même ligne
# (`walk <- function(x) { visit <- function(y) y }`). D'où : on PARSE le texte et
# on parcourt l'AST (via `ts_ast_walk`, la primitive du helper testé — boucle
# vertueuse : le garde s'appuie sur ce qu'il protège, donc le casse s'il casse).
.tha_walk_defs <- function(txt) {
  exprs <- tryCatch(parse(text = txt), error = function(e) NULL)
  if (is.null(exprs)) return(character(0))
  hits <- character(0)
  for (i in seq_along(exprs)) {
    ts_ast_walk(exprs[[i]], function(x) {
      if (!is.call(x)) return(FALSE)
      h <- tryCatch(x[[1]], error = function(e) NULL)
      if (!is.symbol(h) || !identical(as.character(h), "<-")) return(FALSE)
      l <- tryCatch(as.list(x), error = function(e) NULL)
      if (is.null(l) || length(l) != 3L) return(FALSE)
      nm <- tryCatch(as.character(l[[2]]), error = function(e) character(0))
      # ⚠️ `as.character()` sur un CALL est VECTORISE : `nm` peut avoir une
      # longueur > 1, et `nm %in% ...` rend alors un vecteur => « 'length = 2'
      # in coercion to 'logical(1)' » (mesure §2dg). On exige length 1.
      if (length(nm) != 1L) return(FALSE)
      if (!(nm %in% c("walk", "visit"))) return(FALSE)
      rhs <- tryCatch(l[[3]], error = function(e) NULL)
      if (is.call(rhs) &&
          is.symbol(tryCatch(rhs[[1]], error = function(e) NULL)) &&
          identical(as.character(rhs[[1]]), "function")) {
        hits <<- c(hits, nm)
      }
      FALSE
    })
  }
  hits
}

test_that("aucune copie locale de walk()/visit() dans tests/testthat/", {
  files <- list.files(file.path(ts_project_root(), .THAW_DIR),
                      pattern = "\\.R$", full.names = TRUE)
  # Garde-fou de non-vacuite : un dossier vide passerait le test pour rien.
  expect_gte(length(files), 100L)

  offenders <- character(0)
  for (f in files) {
    rel <- paste0(.THAW_DIR, "/", basename(f))
    if (rel %in% .THAW_EXEMPT) next
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    hits <- .tha_walk_defs(txt)
    if (length(hits)) {
      offenders <- c(offenders, paste0(rel, " @ ", hits))
    }
  }

  if (length(offenders)) cat("Copies locales restantes :\n  ", paste(offenders, collapse = "\n  "), "\n")
  expect_length(offenders, 0L)
})

test_that("l'API partagee helper-ast.R est bien celle qui porte le parcours", {
  helper <- readLines(file.path(ts_project_root(), .THAW_SELF), warn = FALSE)
  helper <- paste(helper, collapse = "\n")

  # La primitive de parcours EST dans le helper...
  expect_true(grepl("ts_ast_walk", helper, fixed = TRUE))
  # ...et le helper est le SEUL exempt, pour une raison NOMMEE (il porte `visit`).
  expect_identical(.THAW_EXEMPT, .THAW_SELF)
  # Non-vacuite de la raison : le helper contient bien un `visit` interne, sinon
  # l'exemption n'aurait plus de motif et devrait disparaitre.
  expect_true(grepl("visit <- function", helper, fixed = TRUE))
})

test_that("la sonde du garde est DISCRIMINANTE (elle ne signale que ce qu'il faut)", {
  # Vrai positif : une definition locale non qualifiee.
  expect_length(.tha_walk_defs("walk <- function(x) { x }"), 1L)
  expect_length(.tha_walk_defs("  walk <- function(x, inside) { x }"), 1L)
  # Vrai positif PIEGE : deux definitions sur UNE ligne (le scan ligne-a-ligne
  # en ratait une — mesure §2dg).
  expect_length(.tha_walk_defs("a <- function() { walk <- function(x) x; visit <- function(y) y }"), 2L)
  # Vraies negatives : un APPEL n'est pas une definition...
  expect_length(.tha_walk_defs("  walk(l[[i]])"), 0L)
  # ...un nom DIFFERENT non plus...
  expect_length(.tha_walk_defs("ts_ast_walk <- function(x, fn) { x }"), 0L)
  # ...et une affectation non-fonction non plus.
  expect_length(.tha_walk_defs("walk <- 1"), 0L)
})

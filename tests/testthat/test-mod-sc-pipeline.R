# =============================================================================
# test-mod-sc-pipeline.R
#   tests for modules/sc/mod_sc_pipeline.R
# =============================================================================
# 37e increment de la dette de conventions (2026-09-19, §2cw).
# Cree AVANT la conversion (test d'abord, rouge d'abord) => ce fichier doit
# ECHOUER sur les assertions de classe et sur le verrou C10 tant que le `stop()`
# de la ligne 260 ne porte pas `class = "mod_sc_pipeline_error"`.
#
# 🔑 CLASSE `mod_sc_pipeline_error` — branche (c) de CONVENTIONS.md §7 : le nom
# MECANIQUE `sc_pipeline_error` est DEJA PRIS par `R/sc/sc_pipeline.R:71`, le
# JUMEAU `R/` du meme garde (meme message « Seulement %d cellule(s) apres QC »)
# => on retombe sur la branche (b) : la classe porte le FICHIER, `mod_` inclus.
# Mesure prealable : `mod_sc_pipeline_error` n'apparait dans AUCUN fichier .R du
# depot, et `sc_pipeline_error` n'apparait que dans `R/sc/sc_pipeline.R` + son test.
# ⚠️ La doctrine §7 n'autorise AUCUNE classe partagee `R/` <-> `modules/`
# (mesure) : on ne reutilise donc PAS `sc_pipeline_error`.
#
# ---------------------------------------------------------------------------
# 🔴 VERDICTS — ce que ce lot PROUVE, et ce qu'il NE prouve PAS
# ---------------------------------------------------------------------------
#   (1) ATTEIGNABLE ................ NON PROUVE, et c'est DIT. Le site vit dans
#       le corps d'un `observeEvent` (L220) dont le PROLOGUE exige un objet
#       Seurat : `PercentageFeatureSet()` (L236), `subset()` (L253),
#       `obj@meta.data` (L245). Ce lot ne monte donc PAS le reactif : il execute
#       la GARDE ELLE-MEME, extraite de l'AST — un APPEL `eval()` EST execute
#       (§2ct.2). C'est PLUS FORT qu'un simple verrou source (le jumeau §2ci
#       s'en tenait la), et MOINS FORT qu'une preuve comportementale (§2cr).
#   (2) LA CLASSE S'ECHAPPE ........ NON — le `tryCatch` ouvert L227 se ferme
#       L471 sur un gestionnaire (L469-471) qui NOTIFIE au lieu de RELANCER
#       => il AVALE l'erreur (§2cd.2). => preuve = VERROU SOURCE, rendu
#       FALSIFIABLE par le test du gestionnaire (ci-dessous).
#   (3) CONDITIONNEL ................ OUI, prouve A L'EXECUTION sur la garde
#       extraite : 5 cellules => la condition est TRUE et la garde tire ;
#       50 cellules => la condition est FALSE et la garde ne tire pas.
#   (4) MESSAGE ..................... assere des DEUX bouts (tete ET queue) : la
#       queue est le detecteur de troncature (C16). Le message est a UN SEUL
#       argument (`sprintf`) => aucun `paste0()` a ajouter, C16 sans objet ici.
# =============================================================================

.MSP_FILE <- "modules/sc/mod_sc_pipeline.R"

# Fragments ASCII : le litteral source porte des accents (L262) dont l'encodage
# de lecture n'est pas garanti => on assere des tranches SANS accent.
.MSP_MSG_ASCII     <- "Seulement %d cellule(s)"
# Fragment identifiant le gestionnaire avaleur PAR SON CONTENU (jamais par sa
# position : le fichier porte plusieurs `tryCatch`).
.MSP_HANDLER_ASCII <- 'paste(.tr("Erreur pipeline:")'


# --- Les deux noeuds APPEL extraits de la SOURCE -----------------------------
# ⚠️ On ne recopie RIEN : ces noeuds sont les expressions REELLES du fichier.
.msp_guard_call <- function() {
  ts_ast_find(ts_ast_parse(.MSP_FILE), function(x) {
    ts_ast_is_call_to(x, "if") &&
      grepl(.MSP_MSG_ASCII, ts_ast_deparse(x), fixed = TRUE)
  })
}

.msp_stop_call <- function() {
  ts_ast_find(ts_ast_parse(.MSP_FILE), function(x) {
    ts_ast_is_call_to(x, "stop") &&
      grepl(.MSP_MSG_ASCII, ts_ast_deparse(x), fixed = TRUE)
  })
}


# --- Environnement d'execution de la garde -----------------------------------
# La garde ne lit QUE : `ncol(obj)`, `n_before`, les 3 seuils `input$qc_*`, les
# 4 compteurs `n_ok_*` et `.tr()`. On injecte exactement cette surface — un
# maillon de moins et la garde meurt sur « object not found » (faux rouge).
# `obj` : seul `ncol()` est lu par la garde => une matrice suffit, et c'est ce
# qui rend le test HERMETIQUE (ni Seurat charge, ni objet reel a construire).
.msp_env <- function(n_cells, n_before = 100L, min_gene = 200L,
                     max_gene = 5000L, mt = 20L) {
  e <- new.env(parent = globalenv())
  e$obj      <- matrix(0, nrow = 1L, ncol = n_cells)
  e$n_before <- n_before
  e$n_ok_min <- as.integer(n_cells)
  e$n_ok_max <- as.integer(n_cells)
  e$n_ok_mt  <- as.integer(n_cells)
  e$n_ok_all <- as.integer(n_cells)
  e$input    <- list(qc_min_gene = min_gene, qc_max_gene = max_gene,
                     qc_mt = mt)
  e$.tr      <- function(key, ...) key   # i18n NULL rend la CLE = la chaine FR
  e
}

# Execute la garde REELLE et rend la condition levee, ou NULL si elle ne tire pas.
.msp_run_guard <- function(envir) {
  tryCatch({
    eval(.msp_guard_call(), envir = envir)
    NULL
  }, error = function(e) e)
}


# ---------------------------------------------------------------------------
# CONDITIONNEL — sous la borne, la condition est TRUE et la garde TIRE
# ---------------------------------------------------------------------------
test_that("le garde 260 est conditionnel : 5 cellules => condition TRUE et il TIRE", {
  e <- .msp_env(n_cells = 5L)

  # La CONDITION du `if`, reelle (2e element du noeud) — evaluee, pas recopiee.
  expect_true(isTRUE(eval(.msp_guard_call()[[2]], envir = e)))

  cond <- .msp_run_guard(e)
  expect_true(inherits(cond, "error"))
  # Tete du message : les compteurs RENDUS (pas le gabarit brut).
  expect_true(grepl("Seulement 5 cellule(s)", conditionMessage(cond), fixed = TRUE))
})


# ---------------------------------------------------------------------------
# CONDITIONNEL — au-dessus de la borne, la condition est FALSE : TEMOIN DE
# TRAVERSEE (§2cj.3). « Notre message est absent » serait vrai AUSSI si la
# garde n'avait pas ete atteinte => on prouve que la condition est FALSE, ce qui
# garantit que l'execution CONTINUE.
# ---------------------------------------------------------------------------
test_that("le garde 260 est conditionnel : 50 cellules => condition FALSE, il ne tire pas", {
  e <- .msp_env(n_cells = 50L)

  expect_false(isTRUE(eval(.msp_guard_call()[[2]], envir = e)))
  expect_null(.msp_run_guard(e))
})


# ---------------------------------------------------------------------------
# CLASSE — executee sur l'ARGUMENT du stop (jamais sur le `stop()` lui-meme :
# `eval()` d'un APPEL l'EXECUTERAIT, §2ct.2)
# ---------------------------------------------------------------------------
test_that("la classe est mod_sc_pipeline_error (execution de l'ARGUMENT du stop)", {
  call <- .msp_stop_call()
  expect_false(is.null(call))

  arg  <- call[[2]]
  cond <- eval(arg, envir = .msp_env(n_cells = 5L))

  expect_true(inherits(cond, "condition"))
  expect_true(inherits(cond, "mod_sc_pipeline_error"))

  msg <- if (inherits(cond, "condition")) conditionMessage(cond) else ""
  # Tete ET QUEUE : la queue est le detecteur de troncature (C16).
  expect_true(grepl("Seulement 5 cellule(s)", msg, fixed = TRUE))
  expect_true(grepl("% Mito (35%).", msg, fixed = TRUE))
})


# ---------------------------------------------------------------------------
# VERROU SOURCE CIBLE : le site porte bien NOTRE classe
# ---------------------------------------------------------------------------
test_that("le site 260 porte errorCondition + class = mod_sc_pipeline_error", {
  # ⚠️ La classe etant INOBSERVABLE a l'execution (gestionnaire avaleur), la
  # SOURCE est le seul canal ou elle est verifiable — et c'est exactement ce que
  # C10 mesure.
  hits <- ts_ast_stop_sites(.MSP_FILE, .MSP_MSG_ASCII)
  expect_length(hits, 1L)                                   # exactement UN site
  expect_true(grepl("errorCondition", hits[[1]], fixed = TRUE))
  expect_true(grepl('class = "mod_sc_pipeline_error"', hits[[1]], fixed = TRUE))
})


# ---------------------------------------------------------------------------
# Justification EXECUTABLE du verrou source : le gestionnaire ne RELANCE pas
# ---------------------------------------------------------------------------
test_that("le gestionnaire englobant notifie SANS stop( (falsifiable)", {
  d <- ts_ast_deparse(ts_ast_trycatch_handler(.MSP_FILE, .MSP_HANDLER_ASCII))
  # Il rend la main en notifiant...
  expect_true(grepl("showNotification", d, fixed = TRUE))
  # ...et NE relance PAS. Si un jour il relancait, ce test echouerait et
  # signalerait que le lot devient prouvable a l'execution (§2cd.2).
  expect_false(grepl("stop(", d, fixed = TRUE))
})


# ---------------------------------------------------------------------------
# VERROU SOURCE : mod_sc_pipeline.R ne doit plus contribuer aucun C10
# ---------------------------------------------------------------------------
test_that("mod_sc_pipeline.R ne contribue aucun signalement C10", {
  expect_identical(ts_c10_sites(.MSP_FILE), integer(0))
})

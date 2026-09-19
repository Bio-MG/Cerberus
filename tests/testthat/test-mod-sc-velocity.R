# =============================================================================
# test-mod-sc-velocity.R
#   tests for modules/sc/mod_sc_velocity.R
# =============================================================================
# 38e increment de la dette de conventions (2026-09-19, §2cx) — DERNIER site C10
# du depot (C10 1 -> 0).
#
# 🔑 CLASSE `sc_velocity_error` — et NON `mod_sc_velocity_error`.
# Le pas suivant avait ete ANNONCE comme `mod_sc_velocity_error` (§2cv.6, §2cw.7).
# C'etait une PREDICTION, et la MESURE la REFUTE : le mapping mecanique de
# `modules/` est « retirer le prefixe `mod_`, garder le domaine » — TROIS
# precedents MESURES : `mod_sc_annotation.R` -> `sc_annotation_error`,
# `mod_sc_markers.R` -> `sc_markers_error`, `mod_sc_pseudobulk.R` ->
# `sc_pseudobulk_error`. Seul le ROUTEUR PARENT (`mod_sc.R`) garde `mod_`
# (`mod_sc_error`, §2co.1). `sc_velocity_error` est LIBRE (mesure : 0 occurrence
# dans tout le depot) => branche (a) de §7, PAS de repli sur (b).
# ⚠️ La classe du JUMEAU `R/sc/sc_velocity.R` est `velocity_validation_error`
# (7 occurrences) — classe DIFFERENTE, donc aucune classe PARTAGEE `R/` <->
# `modules/` : la doctrine §7 l'interdit et le depot ne le fait nulle part.
#
# ---------------------------------------------------------------------------
# 🔴 VERDICTS — ce que ce lot PROUVE, et ce qu'il NE prouve PAS
# ---------------------------------------------------------------------------
#   (1) ATTEIGNABLE ................ NON PROUVE, et c'est DIT. Le site vit dans
#       `observeEvent(input$velocity_validate)` (L342), sous un `tryCatch`
#       (L345), apres un PROLOGUE lourd (lecture de fichiers, objet Seurat,
#       `validate_velocity_matrices()`). Ce lot ne monte donc PAS le reactif.
#   (2) LA CLASSE S'ECHAPPE ........ NON — le `tryCatch` ouvert L345 se ferme
#       L590 sur un gestionnaire (L581-589) qui NOTIFIE et pose un statut, sans
#       RELANCER => il AVALE l'erreur (§2cd.2). => preuve = VERROU SOURCE.
#       ⚠️ MAIS la classe est quand meme PROUVEE A L'EXECUTION : la garde REELLE
#       est extraite de l'AST et EVALUEE (un `eval()` d'un APPEL execute, §2ct.2)
#       => PLUS FORT qu'un simple verrou source (§2ci), MOINS FORT qu'une preuve
#       comportementale (§2cr).
#   (3) CONDITIONNEL ................ OUI, prouve A L'EXECUTION : 0 cellule
#       alignee => la condition est TRUE et la garde TIRE ; 5 cellules => FALSE
#       et elle ne tire pas (TEMOIN DE TRAVERSEE, §2cj.3).
#   (4) MESSAGE NON TRONQUE (C16) ... OUI. Le `stop()` d'origine est
#       MULTI-ARGUMENTS (4 fragments) : `errorCondition()` n'AGREGE PAS ses
#       arguments, il faut donc `paste0()` — c'est le cas « MIXTE » annonce.
#       La QUEUE du message est asseree comme DETECTEUR DE TRONCATURE.
# =============================================================================

.MSV_FILE <- "modules/sc/mod_sc_velocity.R"

# Fragment ASCII : le message source ne porte AUCUN accent (mesure) => on peut
# asserter le texte brut, sans se soucier de l'encodage de lecture.
# ⚠️ LONGUEUR CHOISIE PAR MESURE, pas par gout : le fragment court
# « Aucune cellule alignee » apparait DEUX fois dans le fichier — le site 438
# ET un label de `renderPlot` (L605, « ... : revalidez les donnees velocity. »).
# Comme `ts_ast_find` rend le PREMIER noeud rencontre, un fragment ambigu ferait
# dependre le test de l'ORDRE du fichier (piege #105). Le fragment long est
# UNIQUE (mesure : 1 occurrence) => le noeud trouve est le bon, par construction.
.MSV_MSG <- "Aucune cellule alignee entre les matrices velocity"

# Fragment identifiant le gestionnaire AVALEUR par son CONTENU (jamais par sa
# position : le fichier porte plusieurs `tryCatch`).
.MSV_HANDLER <- "Erreur validation velocity"


# --- Les deux noeuds APPEL extraits de la SOURCE -----------------------------
# ⚠️ On ne RECOPIE rien : ces noeuds sont les expressions REELLES du fichier
# (regle 3 : etendre, ne pas dupliquer).
.msv_guard_call <- function() {
  ts_ast_find(ts_ast_parse(.MSV_FILE), function(x) {
    ts_ast_is_call_to(x, "if") &&
      grepl(.MSV_MSG, ts_ast_deparse(x), fixed = TRUE)
  })
}

.msv_stop_call <- function() {
  ts_ast_find(ts_ast_parse(.MSV_FILE), function(x) {
    ts_ast_is_call_to(x, "stop") &&
      grepl(.MSV_MSG, ts_ast_deparse(x), fixed = TRUE)
  })
}


# --- Environnement d'execution de la garde -----------------------------------
# La garde ne lit QUE `validated$n_cells_matched`. On injecte exactement cette
# surface : un maillon de moins et la garde meurt sur « object not found »
# (faux rouge). Une simple LISTE suffit => le test est HERMETIQUE (ni Seurat
# charge, ni objet reel a construire).
.msv_env <- function(n_matched) {
  e <- new.env(parent = globalenv())
  e$validated <- list(n_cells_matched = as.integer(n_matched))
  e
}

# Execute la garde REELLE et rend la condition levee, ou NULL si elle ne tire pas.
.msv_run_guard <- function(envir) {
  tryCatch({
    eval(.msv_guard_call(), envir = envir)
    NULL
  }, error = function(e) e)
}


# ---------------------------------------------------------------------------
# CONDITIONNEL — sous la borne, la condition est TRUE et la garde TIRE
# ---------------------------------------------------------------------------
test_that("la garde 436 est conditionnelle : 0 cellule alignee => TRUE et elle TIRE", {
  e <- .msv_env(0L)

  # La CONDITION du `if`, reelle (2e element du noeud) — evaluee, pas recopiee.
  expect_true(isTRUE(eval(.msv_guard_call()[[2]], envir = e)))

  cond <- .msv_run_guard(e)
  expect_true(inherits(cond, "error"))
  # Tete du message : elle prouve que la garde a bien TIRE.
  expect_true(grepl("Aucune cellule alignee", conditionMessage(cond), fixed = TRUE))
})


# ---------------------------------------------------------------------------
# CONDITIONNEL — au-dessus de la borne, la condition est FALSE : TEMOIN DE
# TRAVERSEE (§2cj.3). « Notre message est absent » serait vrai AUSSI si la garde
# n'avait pas ete atteinte => on prouve que la condition est FALSE, ce qui
# garantit que l'execution CONTINUE.
# ---------------------------------------------------------------------------
test_that("la garde 436 est conditionnelle : 5 cellules => FALSE, elle ne tire pas", {
  e <- .msv_env(5L)

  expect_false(isTRUE(eval(.msv_guard_call()[[2]], envir = e)))
  expect_null(.msv_run_guard(e))
})


# ---------------------------------------------------------------------------
# CLASSE — executee sur l'ARGUMENT du stop (jamais sur le `stop()` lui-meme :
# `eval()` d'un APPEL l'EXECUTERAIT, §2ct.2)
# ---------------------------------------------------------------------------
test_that("la classe est sc_velocity_error (execution de l'ARGUMENT du stop)", {
  call <- .msv_stop_call()
  expect_false(is.null(call))

  arg  <- call[[2]]
  cond <- eval(arg, envir = .msv_env(0L))

  expect_true(inherits(cond, "condition"))
  expect_true(inherits(cond, "sc_velocity_error"))
  # ⚠️ PAS `mod_sc_velocity_error` : la mesure a REFUTE la prediction.
  expect_false(inherits(cond, "mod_sc_velocity_error"))

  msg <- conditionMessage(cond)
  # Tete ET QUEUE : la queue est le detecteur de troncature (C16). Sans
  # `paste0()`, `errorCondition()` ne garderait que le PREMIER fragment.
  expect_true(grepl("Aucune cellule alignee", msg, fixed = TRUE))
  expect_true(grepl("des suffixes (-1) si necessaire.", msg, fixed = TRUE))
})


# ---------------------------------------------------------------------------
# VERROU SOURCE CIBLE : le site porte bien NOTRE classe
# ---------------------------------------------------------------------------
test_that("le site 436 porte errorCondition + paste0 + class = sc_velocity_error", {
  # ⚠️ La classe etant INOBSERVABLE sur le chemin reel (gestionnaire avaleur), la
  # SOURCE est le seul canal ou elle est verifiable — et c'est exactement ce que
  # C10 mesure.
  hits <- ts_ast_stop_sites(.MSV_FILE, .MSV_MSG)
  expect_length(hits, 1L)                                   # exactement UN site
  expect_true(grepl("errorCondition", hits[[1]], fixed = TRUE))
  # C16 : le message d'origine a QUATRE arguments => `paste0()` est obligatoire.
  expect_true(grepl("paste0(", hits[[1]], fixed = TRUE))
  expect_true(grepl('class = "sc_velocity_error"', hits[[1]], fixed = TRUE))
})


# ---------------------------------------------------------------------------
# Justification EXECUTABLE du verrou source : le gestionnaire ne RELANCE pas
# ---------------------------------------------------------------------------
test_that("le gestionnaire englobant notifie SANS stop( (falsifiable)", {
  d <- ts_ast_deparse(ts_ast_trycatch_handler(.MSV_FILE, .MSV_HANDLER))
  # Il rend la main en notifiant...
  expect_true(grepl("showNotification", d, fixed = TRUE))
  # ...et NE relance PAS. Si un jour il relancait, ce test echouerait et
  # signalerait que le lot devient prouvable a l'execution (§2cd.2).
  expect_false(grepl("stop(", d, fixed = TRUE))
})


# ---------------------------------------------------------------------------
# VERROU SOURCE : mod_sc_velocity.R ne doit plus contribuer aucun C10
# ---------------------------------------------------------------------------
test_that("mod_sc_velocity.R ne contribue aucun signalement C10", {
  expect_identical(ts_c10_sites(.MSV_FILE), integer(0))
})

# =============================================================================
# test-mod-sc-markers.R
#   tests for modules/sc/mod_sc_markers.R
# =============================================================================
# 34e increment de la dette de conventions (2026-09-19, §2ct).
# Cree AVANT la conversion (regle 5 : test d'abord, rouge d'abord) => ce fichier
# doit ECHOUER sur les assertions de classe et sur le verrou C10 tant que le
# `stop()` de la ligne 213 ne porte pas `class = "sc_markers_error"`.
#
# 🔑 CLASSE `sc_markers_error` — mapping mecanique de `modules/` (retirer le
# prefixe `mod_`, garder le domaine) applique SANS collision. Mesure prealable :
# `sc_markers_error` n'apparait dans AUCUN fichier .R du depot.
# ⚠️ Le nom est celui de la BRANCHE (a) de `CONVENTIONS.md` §7 (domaine unique
# `markers` dans le fichier), PAS la branche (b) des routeurs parents : ce
# fichier n'est pas un routeur, il porte un domaine unique.
#
# ---------------------------------------------------------------------------
# 🔴 VERDICTS — ce que ce lot PROUVE, et ce qu'il NE prouve PAS
# ---------------------------------------------------------------------------
#   (1) ATTEIGNABLE ................ OUI, prouve (temoin non vacuante ci-dessous)
#   (2) LA CLASSE S'ECHAPPE ........ NON — le `tryCatch` ouvert L211 ferme L270
#       et son gestionnaire (L270-275) **journalise + notifie** au lieu de
#       **relancer** => il **AVALE** l'erreur (§2cd.2). => preuve = VERROU SOURCE.
#   (3) CONDITIONNEL ................ OUI, prouve — avec un TEMOIN DE TRAVERSEE
#       (§2cj.3/#60) : au-dessus de la borne, la notification de SUCCES part et
#       `shared_rv$markers_data` est renseigne. « Notre message est absent »
#       serait vrai aussi si le code avait echoue AVANT le garde.
#   (4) MESSAGE VARIABLE ............ SANS OBJET, et c'est DIT : le message du
#       garde (`"Au moins 2 groupes necessaires"`) **n'interpole aucune entree**
#       => la regle §2ch.3/#56 ne s'applique pas. On assere donc le message
#       ENTIER (regle de non-troncature), pas une variation.
#   (5) DOMAINE BOUCLE .............. NON — `modules/sc/` conserve 2 autres
#       sites C10 (`mod_sc_pipeline.R:260`, `mod_sc_velocity.R:436`). Aucune
#       revendication de fermeture n'est faite ici.
#
# ⚠️ **(2) est la forme la plus FAIBLE des deux preuves** (§2cr a obtenu une
# preuve COMPORTEMENTALE). C'est dit explicitement plutot que masque.
#
# ⚠️ **C16 / §2bn sans objet** : le message du garde est a UN SEUL argument,
# passe a `.tr()` => aucun `paste0()` a ajouter.
#
# ⚠️ **`observeEvent` : le corps est un BLOC** (3e element de l'appel) => il faut
# l'EVALUER, jamais l'appeler (piege inverse de §2cn.3).
#
# ⚠️ **`shiny::Progress$new()` (L207) est appele via `::` => NON mockable** : sans
# `withReactiveDomain(MockShinySession$new(), ...)` le corps meurt AVANT le site
# (motif §2cc, debloque §2cp.2).
#
# ⚠️ **Le prologue exige un objet S4** (`obj@meta.data` a L204) : un `list()`
# avec `class = "Seurat"` NE SUFFIT PAS (`@` exige un objet S4). On utilise donc
# une classe S4 MOCK dont le SEUL slot est `meta.data` — c'est exactement la
# surface que le garde lit, et cela rend le test HERMETIQUE (ni Seurat charge, ni
# objet reel a construire). Ce qui n'est PAS teste ici : le comportement de
# Seurat lui-meme, hors sujet pour un verrou de classe d'erreur.
# =============================================================================

source_project_file("modules/sc/mod_sc_markers.R")

.SM_FILE <- "modules/sc/mod_sc_markers.R"

# Fragments ASCII : le litteral source porte un accent (L213) dont l'encodage de
# lecture n'est pas garanti => on assere une tranche SANS accent, insensible a
# l'encodage, et le message ENTIER est compare separement (cf. test dedie).
.SM_MSG_ASCII     <- "Au moins 2 groupes"
# Fragment ASCII de la PROGRESSION (journalisee AVANT le garde, L206).
.SM_PROLOGUE_ASCII <- "Recherche en cours"
# Fragment ASCII du message de SUCCES (temoin de TRAVERSEE, §2cj.3).
.SM_OK_ASCII      <- "marqueurs"
# Fragment identifiant le gestionnaire avaleur PAR SON CONTENU.
.SM_HANDLER_ASCII <- 'paste(.tr("Erreur:")'


# --- Classe S4 mock : la SEULE surface lue par le garde ----------------------
.sm_mock_obj <- function(n_groups) {
  if (!methods::isClass("TSMockSeuratMarkers")) {
    methods::setClass("TSMockSeuratMarkers", slots = c(meta.data = "data.frame"))
  }
  clusters <- rep(as.character(seq_len(n_groups)), length.out = 4L)
  methods::new("TSMockSeuratMarkers",
               meta.data = data.frame(seurat_clusters = clusters,
                                      stringsAsFactors = FALSE))
}


# --- Environnement enfant ----------------------------------------------------
# `rec` enregistre les etapes (temoins) ; `n_groups` pilote la BORNE.
# ⚠️ CHAQUE maillon est injecte : un helper MAISON non mocke retomberait sur
# `globalenv` (§2cg : pour une chaine de N maillons, N injections).
.sm_env <- function(rec, n_groups = 1L) {
  e <- new.env(parent = globalenv())

  # `.tr` reel avec i18n NULL rend la CLE, qui EST la chaine FR (cf. PITFALLS).
  e$.tr    <- function(key, ...) key
  # Le VRAI `.t_fmt` (global.R:275), extrait par AST — jamais recopie.
  e$.t_fmt <- ts_ast_assignment("global.R", ".t_fmt")
  e$`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  e$req    <- shiny::req                      # REEL (mesure §2cn.4)

  # Canaux observables : ce sont EUX qui portent les temoins.
  e$marker_log_rv <- function(x) {
    rec(paste0("log:", if (is.null(x)) "NULL" else paste0(x)))
    invisible(NULL)
  }
  e$markers_rv <- function(x) {
    rec(paste0("markers_rv:", if (is.null(x)) "NULL" else paste0(nrow(x), " lignes")))
    invisible(NULL)
  }
  e$showNotification <- function(msg, ...) {
    rec(paste0("notify:", paste0(msg)))
    invisible(NULL)
  }

  # Helpers maison du prologue : a bouchonner un par un.
  e$`Idents<-` <- function(obj, value) { rec("Idents<-"); obj }
  e$subsample_seurat_for_analysis <- function(obj, max_per_group, group_col) {
    rec("subsample")
    list(was_subsampled = FALSE, object = obj, n_before = 4L, n_after = 4L)
  }
  e$optimize_bpcells_for_markers <- function(obj) { rec("bpcells"); NULL }
  e$FindAllMarkers <- function(obj, ...) {
    rec("FindAllMarkers")
    data.frame(gene = "G1", cluster = "1", avg_log2FC = 1.5,
               p_val_adj = 0.01, pct.1 = 0.8, pct.2 = 0.1,
               stringsAsFactors = FALSE)
  }

  e$global_data <- list(i18n = NULL, sc_obj = .sm_mock_obj(n_groups))
  e$shared_rv   <- list(max_cells_heavy = Inf)
  e$input <- list(marker_test = "wilcox", marker_min_pct = 0.10,
                  marker_logfc = 0.25, sort_by = "logfc")
  e$session <- list(onSessionEnded = function(f) invisible(NULL))
  e
}


# --- Execution : le corps est un BLOC => on l'EVALUE (jamais on ne l'appelle).
.sm_run <- function(envir) {
  tryCatch({
    shiny::withReactiveDomain(
      shiny::MockShinySession$new(),
      eval(ts_ast_observe_block(.SM_FILE, "run_markers"), envir = envir)
    )
    NULL
  }, error = function(e) list(msg = conditionMessage(e), class = class(e)))
}


# ---------------------------------------------------------------------------
# TEMOIN D'EXECUTION : le garde 213 S'EXECUTE (prouve) mais l'erreur est AVALEE
# ---------------------------------------------------------------------------
test_that("mod_sc_markers : le garde 213 est ATTEINT (temoin) puis AVALE", {
  skip_if_not(exists("MockShinySession", envir = asNamespace("shiny")),
              "shiny::MockShinySession indisponible")

  steps <- character(0)
  err <- .sm_run(.sm_env(function(s) steps <<- c(steps, s), n_groups = 1L))

  # 1) Le PROLOGUE a reellement couru jusqu'a L206 (etape journalisee AVANT le
  #    garde) : sans ce temoin, le suivant pourrait matcher une erreur anterieure.
  expect_true(any(grepl(.SM_PROLOGUE_ASCII, steps, fixed = TRUE)))
  # 2) TEMOIN NON VACUANT : le gestionnaire L270-275 a notifie le message DU
  #    GARDE => c'est bien LUI qui s'est execute (c'est son message qui ressort).
  expect_true(any(grepl(.SM_MSG_ASCII, steps, fixed = TRUE)))
  # 3) La borne a bien ete lue sur l'objet : `FindAllMarkers` n'a PAS ete atteint.
  expect_false(any(steps == "FindAllMarkers"))
  # 4) 🔴 AUCUNE erreur ne s'echappe => le gestionnaire l'a AVALEE.
  expect_null(err)
})


# ---------------------------------------------------------------------------
# Le message du garde arrive ENTIER (regle de non-troncature) — et il n'est PAS
# un message de bouchon : c'est bien celui du garde, compare au litteral SOURCE.
# ---------------------------------------------------------------------------
test_that("le message notifie est ENTIEREMENT celui du garde 213", {
  skip_if_not(exists("MockShinySession", envir = asNamespace("shiny")),
              "shiny::MockShinySession indisponible")

  # Le litteral est EXTRAIT de la source puis re-evalue avec un `.tr` identitaire
  # (exactement l'environnement enfant) : deux copies pourraient diverger, et
  # l'encodage d'une recopie n'est pas garanti.
  hits <- ts_ast_stop_sites(.SM_FILE, .SM_MSG_ASCII)
  expect_length(hits, 1L)
  # ⚠️ On evalue l'ARGUMENT du `stop()`, JAMAIS l'appel : `eval()` d'un appel
  # `stop(...)` l'EXECUTE et leve (forme inverse du piege « eval() d'une
  # definition n'est pas un appel », §2cn.3).
  # ⚠️ Et on lit le message par `conditionMessage()` quand l'evaluation rend une
  # CONDITION : le garde est enveloppe (`errorCondition(...)`) APRES conversion et
  # ne l'est PAS avant => une assertion ecrite sur la FORME du noeud casse
  # mecaniquement le jour de la conversion. Celle-ci vaut dans les DEUX etats, ce
  # qui est precisement ce qui rend le ROUGE lisible (3 echecs, pas 4).
  hits <- ts_ast_stop_sites(.SM_FILE, .SM_MSG_ASCII)
  expect_length(hits, 1L)
  guard_call <- parse(text = hits[[1]])[[1]]
  guard_val  <- eval(guard_call[[2]],
                     envir = list2env(list(.tr = function(key, ...) key),
                                      parent = baseenv()))
  guard_msg  <- if (inherits(guard_val, "condition")) conditionMessage(guard_val) else guard_val

  steps <- character(0)
  invisible(.sm_run(.sm_env(function(s) steps <<- c(steps, s), n_groups = 1L)))

  notifs <- steps[grepl("^notify:", steps)]
  expect_true(any(notifs == paste0("notify:Erreur: ", guard_msg)))
})


# ---------------------------------------------------------------------------
# La classe est INOBSERVABLE : c'est la justification du verrou source
# ---------------------------------------------------------------------------
test_that("mod_sc_markers : la classe est INOBSERVABLE a l'execution", {
  skip_if_not(exists("MockShinySession", envir = asNamespace("shiny")),
              "shiny::MockShinySession indisponible")

  steps <- character(0)
  err <- .sm_run(.sm_env(function(s) steps <<- c(steps, s), n_groups = 1L))

  # La classe n'apparait dans AUCUN canal observable : ni en erreur remontee...
  expect_null(err)
  # ...ni dans les notifications / journaux, qui ne portent que le MESSAGE.
  expect_false(any(grepl("sc_markers_error", steps, fixed = TRUE)))
})


# ---------------------------------------------------------------------------
# Controle de BORNE **avec TEMOIN DE TRAVERSEE** (§2cj.3 / #60) : au-dessus de la
# borne le garde ne tire PAS, et l'execution CONTINUE (prouve par le succes).
# ---------------------------------------------------------------------------
test_that("le garde 213 est conditionnel : 2 groupes => il ne tire pas et l'execution CONTINUE", {
  skip_if_not(exists("MockShinySession", envir = asNamespace("shiny")),
              "shiny::MockShinySession indisponible")

  steps <- character(0)
  e <- .sm_env(function(s) steps <<- c(steps, s), n_groups = 2L)
  err <- .sm_run(e)

  # Temoin de TRAVERSEE : le garde n'a PAS tire...
  expect_false(any(grepl(.SM_MSG_ASCII, steps, fixed = TRUE)))
  # ...et une etape JOURNALISEE APRES lui prouve qu'on l'a franchi (sans ce
  # temoin, « notre message est absent » serait vrai aussi en cas d'echec AVANT).
  expect_true(any(grepl(.SM_OK_ASCII, steps, fixed = TRUE)))
  expect_true(any(steps == "FindAllMarkers"))
  expect_true(any(steps == "markers_rv:1 lignes"))
  expect_false(is.null(e$shared_rv$markers_data))
  expect_null(err)
})


# ---------------------------------------------------------------------------
# Justification EXECUTABLE du verrou source : le gestionnaire ne RELANCE pas
# ---------------------------------------------------------------------------
test_that("le gestionnaire englobant journalise + notifie SANS stop( (falsifiable)", {
  d <- ts_ast_deparse(ts_ast_trycatch_handler(.SM_FILE, .SM_HANDLER_ASCII))
  # Il rend la main en notifiant...
  expect_true(grepl("showNotification", d, fixed = TRUE))
  # ...et NE relance PAS. Si un jour il relancait, ce test echouerait et
  # signalerait que le lot devient prouvable a l'execution (§2cd.2).
  expect_false(grepl("stop(", d, fixed = TRUE))
})


# ---------------------------------------------------------------------------
# Verrou source CIBLE : le garde 213 porte bien NOTRE classe
# ---------------------------------------------------------------------------
test_that("le garde 213 porte la classe sc_markers_error (verrou source cible)", {
  # ⚠️ La classe etant INOBSERVABLE a l'execution, la SOURCE est le seul canal
  # ou elle est verifiable — et c'est exactement ce que C10 mesure.
  hits <- ts_ast_stop_sites(.SM_FILE, .SM_MSG_ASCII)
  expect_length(hits, 1L)                                   # exactement UN site
  expect_true(grepl("errorCondition", hits[[1]], fixed = TRUE))
  expect_true(grepl('class = "sc_markers_error"', hits[[1]], fixed = TRUE))
})


# ---------------------------------------------------------------------------
# Verrou source : mod_sc_markers.R ne doit plus contribuer aucun C10
# ---------------------------------------------------------------------------
test_that("mod_sc_markers.R ne contribue aucun signalement C10", {
  expect_equal(ts_c10_sites(.SM_FILE), integer(0))
})

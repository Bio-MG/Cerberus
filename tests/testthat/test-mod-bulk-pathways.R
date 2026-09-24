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

# =============================================================================
# OBSERVABILITÉ DES ACTIONS — le publisher d'état de `bulk_pathways`
# (2026-09-24, jalon « pathway state observability »)
# =============================================================================
# 🔴 **LE DÉFAUT QUE CE BLOC EXISTE POUR TUER**, mesuré en session VIVANTE le
# 2026-09-24 : `run_pipeline` sur `bulk-pathways-run_pathway` répond **`done`**
# AUSSI BIEN quand l'enrichissement a produit 2 voies que quand l'observateur
# sort par son garde **silencieux** des `< 10` gènes (`return()` nu, l. ~474).
# Le statut est **honnête** sur ce qu'il mesure (« le token a bougé », spec
# §2.3) et **muet** sur ce qui compte. Preuve de la cécité : au même instant,
# `result.json` portait `status="done"` et `snapshot$modules` **ne contenait
# AUCUNE entrée `bulk_pathways`** — alors que le **DOM** portait la vérité
# (`✓ 2 pathways trouvés [ GOBP ]`). Un `done` de **protocole** n'est pas un
# `done` de **métier**.
#
# ⇒ Le module doit publier un **`state`** par la couture EXISTANTE
# (`ts_drive_publish_token(..., state = ...)`, cf. `bulk_filter` et `bulk_de`),
# et ce `state` doit distinguer les **cinq** issues : `not_ready`, `running`,
# `done`, `error`, `empty` (le garde vide, aujourd'hui invisible).
#
# ⚠️ **`running` exige un PRODUCTEUR réel.** Le job pathways est SYNCHRONE :
# aucun tick ne tourne pendant qu'il s'exécute, donc un drapeau posé dans le
# corps de l'observateur ne serait **jamais observable** — un `running` sans
# producteur est un **mensonge**, pas une observabilité (c'est exactement le
# motif « le badge était construit pour ce signal et le producteur manquait »).
# Le producteur existe déjà : `long = TRUE` fait écrire `running` **au
# dispatch** par le poller, et le module ferme la boucle par
# `ts_drive_job_finish()`. Les deux jetons le déclarent.

.MBP_STATE_FIELDS <- c("module", "action", "seq", "status", "elapsed_s",
                       "n_results", "error")

.MBP_STATE_STATUSES <- c("not_ready", "running", "done", "error", "empty")

# --- Région source du probe (ligne à ligne, comme le test éponyme de `bulk_de`)
.mbp_probe_region <- function() {
  src <- readLines(file.path(ts_project_root(), .MBP_FILE),
                   warn = FALSE, encoding = "UTF-8")
  start <- grep("^\\s*drive_state <- function\\(\\)", src)
  if (length(start) != 1L) return(character(0))
  closer <- grep("^    \\}$", src)
  end <- closer[closer > start[1]]
  if (length(end) == 0L) return(character(0))
  src[start[1]:end[1]]
}

# --- Fenêtre source d'un appel `ts_drive_publish_token()` (2 lignes)
.mbp_publish_window <- function(token, span = 3L) {
  src <- readLines(file.path(ts_project_root(), .MBP_FILE),
                   warn = FALSE, encoding = "UTF-8")
  i <- grep(paste0('ts_drive_publish_token\\(.*"', token, '"'), src)
  if (length(i) != 1L) return(character(0))
  src[i:min(length(src), i + span - 1L)]
}

# --- Environnement enfant : `shared_rv` / `input` / job / recorder simulés.
# `ts_drive_badge_sanitize` est le VRAI (sourcé depuis `drive_allowlist.R`) :
# un test qui re-implémenterait le caviardage ne prouverait rien sur le
# caviardage réellement utilisé.
.mbp_state_env <- function(status = NA_character_, action = NA_character_,
                           n_results = NA_integer_, error = "",
                           elapsed_s = NA_real_, job = NULL,
                           filtered = TRUE, vst = TRUE) {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "R/core/drive_allowlist.R"), envir = e)
  e$shared_rv <- list(
    filtered_counts = if (isTRUE(filtered)) matrix(1:4, 2L) else NULL,
    vst_mat         = if (isTRUE(vst)) matrix(1:4, 2L) else NULL)
  e$input <- list()
  e$ts_drive_job_state <- function() job
  e$drive_last <- local({
    d <- new.env(parent = emptyenv())
    d$action <- action; d$status <- status; d$n_results <- n_results
    d$error <- error; d$elapsed_s <- elapsed_s; d$started <- NULL
    d
  })
  e
}

# Un probe ABSENT doit produire un ROUGE QUI DIT « le probe manque », pas un
# plantage d'extraction : `NULL` laisse chaque assertion echouer avec son propre
# message. Meme choix que `.drv_de_probe_region()` (renvoie `character(0)`).
.mbp_call_state <- function(envir) {
  expr <- tryCatch(.mbp_fun_expr("drive_state"), error = function(e) NULL)
  if (is.null(expr)) return(NULL)
  f <- eval(expr, envir = envir)
  f()
}

.mbp_job <- function(seq = 7L, action = "run_pipeline",
                     button = "bulk-pathways-run_pathway") {
  list(seq = seq, module = "bulk_pathways", action = action, button = button,
       started_at = "2026-09-24T00:00:00Z", started = as.numeric(Sys.time()))
}

# ---------------------------------------------------------------------------
# (a) Le probe est ÉCRIT et CÂBLÉ sur les DEUX jetons, qui déclarent `long`
# ---------------------------------------------------------------------------
test_that("bulk_pathways : un state est cable sur SES DEUX jetons, et declares long", {
  expect_true(length(.mbp_probe_region()) > 0L,
              info = "mod_bulk_pathways.R ne definit aucun probe `drive_state`")

  for (tok in c("bulk-pathways-run_pathway", "bulk-pathways-run_scores")) {
    w <- .mbp_publish_window(tok)
    expect_true(length(w) > 0L, info = paste("aucun publish pour", tok))
    joined <- paste(w, collapse = "\n")
    # Un probe DÉFINI mais jamais publié laisse l'agent exactement où il était.
    expect_match(joined, "state = drive_state",
                 info = paste("le jeton", tok, "ne publie pas son state"))
    # `running` n'a de producteur QUE si le module se declare long.
    expect_match(joined, "long = TRUE",
                 info = paste("le jeton", tok, "ne declare pas long = TRUE"))
  }
})

# ---------------------------------------------------------------------------
# (b) Le module FERME le job qu'il déclare long (sinon contrat B le bloque à vie)
# ---------------------------------------------------------------------------
test_that("bulk_pathways ferme ses deux jobs declares long", {
  src <- readLines(file.path(ts_project_root(), .MBP_FILE),
                   warn = FALSE, encoding = "UTF-8")
  joined <- paste(src, collapse = "\n")
  # Déclarer `long` SANS fermer laisserait le job en vol : tout `run_pipeline`
  # suivant serait REFUSÉ (`invalid`, contrat B) et le module deviendrait
  # définitivement impilotable — la panne silencieuse la plus coûteuse.
  expect_true(length(grep("ts_drive_job_finish\\(", src)) >= 2L,
              info = "les deux observateurs doivent fermer leur job")
  expect_match(joined, 'ts_drive_job_finish\\("bulk-pathways-run_pathway"')
  expect_match(joined, 'ts_drive_job_finish\\("bulk-pathways-run_scores"')
})

# ---------------------------------------------------------------------------
# (c) spec §6 : toute lecture réactive du probe est `isolate()`-gardée
# ---------------------------------------------------------------------------
test_that("le probe d'etat de bulk_pathways isole toutes ses lectures reactives", {
  region <- .mbp_probe_region()
  expect_true(length(region) > 0L)
  code <- sub("#.*$", "", region)   # un commentaire ne doit pas satisfaire la regle

  # Le probe DOIT lire l'etat partage : sinon il ne peut rien dire de l'objet.
  expect_true(length(grep("shared_rv\\$", code)) >= 1L,
              info = "le probe ne lit aucun etat partage")
  # ÉGALITÉ des comptes : une seule lecture nue suffit à enroler l'état du
  # module dans le jeu de dépendances du poller. Un « contient-il isolate() ? »
  # ne verrait jamais la lecture oubliée.
  expect_identical(length(grep("shared_rv\\$", code)),
                   length(grep("isolate\\(shared_rv\\$", code)),
                   info = "une lecture reactive du probe n'est pas isolee")
})

# ---------------------------------------------------------------------------
# (d) Le probe ne publie QUE des scalaires légers (spec : pas de matrices)
# ---------------------------------------------------------------------------
test_that("le probe ne publie que des scalaires legers", {
  got <- .mbp_call_state(.mbp_state_env())
  expect_true(is.list(got), info = "le probe d'etat est absent ou n'est pas une liste")
  # `names(NULL)` vaut NULL et `expect_setequal(NULL, x)` LEVE : on normalise
  # pour que l'absence de probe soit un ECHEC qui dit « le probe manque ».
  nm <- if (is.list(got)) names(got) else character(0)
  expect_setequal(nm, .MBP_STATE_FIELDS)
  # Garde anti-derive : un champ lourd ajoute plus tard doit casser ici.
  heavy <- grep("matrix|plot|table|log|genes|pathway_results|scores",
                nm, value = TRUE)
  expect_length(heavy, 0L)
})

# ---------------------------------------------------------------------------
# (e) Les CINQ issues sont distinguables — c'est tout l'objet du jalon
# ---------------------------------------------------------------------------
test_that("l'etat distingue not_ready / running / done / error / empty", {
  # not_ready — AUCUN objet de travail (le garde de l'observateur, rendu visible)
  expect_identical(.mbp_call_state(.mbp_state_env(filtered = FALSE,
                                                  vst = FALSE))$status,
                   "not_ready")
  # not_ready — l'objet est la mais rien n'a encore tourne
  expect_identical(.mbp_call_state(.mbp_state_env())$status, "not_ready")
  # ... et un statut RESOLU ne survit PAS a la disparition de l'objet de
  # travail : le module ne peut plus ni rejouer ni defendre ce resultat, donc il
  # ne doit pas le presenter comme `done`. Sans ce cas, la garde `ready` du
  # probe serait un chemin jamais exerce — la coincidence `status = NA` rendait
  # deja `not_ready`.
  expect_identical(
    .mbp_call_state(.mbp_state_env(status = "done", n_results = 2L,
                                   filtered = FALSE, vst = FALSE))$status,
    "not_ready")
  # running — un job est en vol (producteur : `long = TRUE` au dispatch)
  expect_identical(.mbp_call_state(.mbp_state_env(job = .mbp_job()))$status,
                   "running")
  # done / empty / error — les trois issues RESOLUES
  expect_identical(.mbp_call_state(.mbp_state_env(status = "done",
                                                  n_results = 2L))$status, "done")
  expect_identical(.mbp_call_state(.mbp_state_env(status = "empty",
                                                  n_results = 0L))$status, "empty")
  expect_identical(.mbp_call_state(.mbp_state_env(status = "error",
                                                  error = "boom"))$status, "error")
  # Le vocabulaire est FERME : aucune sixieme valeur ne doit apparaitre.
  expect_setequal(.MBP_STATE_STATUSES,
                  c("not_ready", "running", "done", "error", "empty"))
})

# ---------------------------------------------------------------------------
# (f) `empty` N'EST PAS `done` — le compte les sépare (le défaut d'origine)
# ---------------------------------------------------------------------------
test_that("`empty` n'est pas `done` : le compte de resultats les separe", {
  d <- .mbp_call_state(.mbp_state_env(status = "done",  n_results = 2L))
  e <- .mbp_call_state(.mbp_state_env(status = "empty", n_results = 0L))
  expect_identical(d$n_results, 2L)
  expect_identical(e$n_results, 0L)
  # Si ces deux etaient identiques, le jalon n'aurait rien apporte : c'est
  # EXACTEMENT la confusion que `result.json` faisait avant.
  expect_false(identical(d$status, e$status))
})

# ---------------------------------------------------------------------------
# (g) Le message d'erreur est CAVIARDÉ (un token ne doit pas fuir dans l'UI)
# ---------------------------------------------------------------------------
test_that("le message d'erreur publie est caviarde", {
  tok <- "aavivxlk"
  got <- .mbp_call_state(.mbp_state_env(
    status = "error",
    error = paste0("enrichment failed for ", tok, " at C:/secret/dir/file.R")))
  # `any()` : sur un probe absent, `got$error` est NULL et `grepl()` rend
  # `logical(0)` — `expect_false(logical(0))` leverait au lieu d'echouer.
  expect_false(any(grepl(tok, got$error, fixed = TRUE)))
  expect_false(any(grepl("secret", got$error, fixed = TRUE)))
})

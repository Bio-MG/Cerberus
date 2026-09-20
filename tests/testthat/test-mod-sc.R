# =============================================================================
# test-mod-sc.R — tests for modules/sc/mod_sc.R
# =============================================================================
# 29ᵉ incrément de la dette de conventions (2026-09-18, §2co).
# Créé AVANT la conversion (règle 5 : test d'abord, rouge d'abord) ⇒ ce fichier
# doit ÉCHOUER sur les assertions de classe tant que les deux `stop()` ne
# portent pas `class = "mod_sc_error"`.
#
# 🔴 **CLASSE `mod_sc_error`, ET NON `sc_error` — décision doctrinale mesurée.**
# Le mapping mécanique de `modules/` (retirer le préfixe `mod_`, garder le
# domaine : `mod_sc_pseudobulk.R` → `sc_pseudobulk_error`) donnerait ici
# **`sc_error`** — soit EXACTEMENT le nom que `CONVENTIONS.md` §7 (:334-340)
# **interdit** : « **`sc_helpers_error`** — et **non** `sc_error`, trop large :
# il prétendrait désigner « l'erreur single-cell » générique alors que
# `sc_multi_error`, `sccoda_error`, `milo_error` et `velocity_validation_error`
# coexistent ». Le dépôt a donc **deux** branches dans §7 : (a) le fichier a un
# domaine unique ⇒ `class = "<domaine>_error"` ; (b) il n'en a pas ⇒ **la classe
# porte le FICHIER** (`R/sc/sc_helpers.R` → `sc_helpers_error`).
# `mod_sc.R` est de la branche (b) : son en-tête le déclare « **Parent Router
# Module** », et il empile des observateurs **sans domaine commun** (langue,
# onglet actif, panier de viz, auto-pipeline) puis l'export du rapport. ⇒ La
# classe porte le fichier : **`mod_sc_error`**.
# ⚠️ **Portée de cette décision** : elle vaut pour les **3 routeurs parents**
# (`mod_sc.R`, `mod_bulk.R`, `mod_spatial.R`) — les deux autres portent encore
# des sites C10 (`mod_bulk.R:427`) et devaient donc être tranchés *avant* d'y
# toucher. Elle est consignée dans `CONVENTIONS.md` §7 pour ne pas être
# re-litigée au prochain lot.
#
# 🟢 **LOT « QUI PAIE DEUX FOIS »** : `mod_sc.R` porte **DEUX** sites C10
# (625 et 707), tous deux dans le **MÊME** `content = function(file)` du
# `downloadHandler` du rapport (`output$dl_report`, **L621 → L711**).
# ⇒ un seul fichier converti = **−2** sur C10.
#
# ⚠️ **Extraction AST : le `content` est un ARGUMENT NOMMÉ**, pas un top-level.
# On cherche donc les appels `downloadHandler(...)` et on prend l'élément
# `content` dont le `deparse()` contient la garde — jamais « le 1ᵉʳ trouvé » :
# le fichier en contient **plusieurs** (`dl_report`, `dl_sc_r_script`, …).
#
# ⚠️ **`file.exists()` est le CŒUR de la garde du site 625** : c'est lui qu'on
# mocke. Il est **enregistré** (témoin de traversée) car le contrôle de borne
# en a besoin.
#
# 🔴 **`withProgress()` DOIT être bouchonné — mesuré.** Sa signature réelle est
# `(expr, min, max, value, message, detail, style, session, env, quoted)` : le
# bloc positionnel du module tombe donc sur **`expr`**, le PREMIER formel. Mais
# hors session Shiny il lève `'session' is not a ShinySession object`. On le
# remplace par `function(expr, ...) expr` — retourner `expr` force le bloc à
# s'exécuter. (C'est une **primitive d'UI**, comme `showNotification`, pas le
# sujet du test.)
#
# ⚠️ **`%||%` est REDÉFINI localement** (cf. `R/core/io_helpers.R:47`) : le
# `content` en fait un usage intensif dans `render_params`. ⚠️ Le dépôt en
# contient **deux définitions DIVERGENTES** (`io_helpers.R` teste aussi
# `length(a) == 0`, `R/plotting/palettes.R` non) — on reproduit celle de
# `io_helpers.R`.
#
# ⚠️ **`req()` et `isolate()` sont RÉELS** (`shiny::`), pas mockés : mesuré,
# `req()` passe hors contexte réactif dès que ses arguments sont truthy.
#
# ⚠️ **C16 / §2bn sans objet sur les deux sites** : messages à **UN SEUL**
# argument et entièrement statiques ⇒ **aucun `paste0()`**. On assère les
# messages **ENTIERS** (§2bx.3).
#
# 🔴 **LIMITE DOCUMENTÉE — le site 707 n'a PAS de contrôle de borne.** Franchir
# sa garde exige que `out_files` soit NON VIDE, donc qu'un `rmarkdown::render()`
# **réussisse** (`out_files <- c(out_files, res)`), et `rmarkdown::render` est
# appelé via `::` ⇒ **non mockable**. Le contrôle n'est donc pas réalisable à
# coût raisonnable. ⚠️ **L'assertion d'erreur, elle, n'est PAS vacante** : la
# classe n'apparaît QUE si notre `stop()` converti s'est exécuté — ce qui EST la
# preuve de joignabilité. Ce qui manque est la preuve de **conditionnalité**.
# =============================================================================

source_project_file("modules/sc/mod_sc.R")

.MSCR_FILE <- "modules/sc/mod_sc.R"

# --- Extraction AST : l'argument `content` du downloadHandler qui porte la garde
# Harnais partage `helper-ast.R` (§2dg) : derniere des **14** copies du depot.
# ⚠️ Le needle porte sur le TEXTE du `content`, pas sur l'appel englobant : on
# collecte donc l'appel `downloadHandler` puis on filtre sur son `content`.
.mscr_content_expr <- function(needle) {
  p     <- ts_ast_parse(.MSCR_FILE)
  calls <- ts_ast_find_all(p, function(x) {
    if (!ts_ast_is_call_to(x, "downloadHandler")) return(FALSE)
    l  <- as.list(x)
    nm <- names(l)
    if (is.null(nm)) return(FALSE)
    for (i in seq_along(l)) {                    # (1) par INDEX
      if (!identical(nm[i], "content")) next
      if (grepl(needle, ts_ast_deparse(l[[i]]), fixed = TRUE)) return(TRUE)
    }
    FALSE
  })
  for (x in calls) {
    l  <- as.list(x)
    nm <- names(l)
    for (i in seq_along(l)) {
      if (!is.null(nm) && identical(nm[i], "content")) {
        d <- ts_ast_deparse(l[[i]])
        if (grepl(needle, d, fixed = TRUE)) return(l[[i]])
      }
    }
  }
  stop("aucun downloadHandler(content=) contenant '", needle, "'", call. = FALSE)
}

.MSCR_NEEDLE <- "Template introuvable"

# --- Environnement enfant -----------------------------------------------------
# `exists_fun(path)` décide de `file.exists()` ; `rec` enregistre les étapes
# (témoin de traversée) ; `file_copy_throws` sert au contrôle de borne du 625.
.mscr_env <- function(exists_fun, report_format, rec = NULL, file_copy_throws = FALSE) {
  e <- new.env(parent = globalenv())
  e$.tr       <- function(s) s
  e$.tr_plain <- function(s) s
  # cf. R/core/io_helpers.R:47 (l'autre definition du depot omet `length(a) == 0`)
  e$`%||%`    <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  e$req       <- shiny::req       # REEL (mesure : truthy hors reactif OK)
  e$isolate   <- shiny::isolate   # REEL
  e$state_get <- function(state, field) NULL
  e$showNotification <- function(...) invisible(NULL)
  # Primitive d'UI : hors session Shiny elle leve (mesure) ⇒ bouchon.
  e$withProgress <- function(expr, ...) expr
  e$file.exists <- function(...) {
    path <- list(...)[[1]]
    v <- exists_fun(path)
    if (!is.null(rec)) rec(paste0("file.exists:", basename(path), "=", v))
    v
  }
  e$file.copy <- function(...) {
    if (!is.null(rec)) rec("file.copy")
    if (file_copy_throws) stop("__arret_volontaire__")
    TRUE
  }
  e$global_data <- list(sc_obj = "FAKE_OBJ", language = "fr", i18n = NULL)
  e$shared_rv   <- list()
  e$input <- list(report_format = report_format, report_sections = character(0),
                  report_title = "T", report_subtitle = "S", report_notes = "N",
                  report_interactive = FALSE)
  e
}

# ⚠️ `eval()` rend la FONCTION ; il faut l'APPELER (§2cn.3).
.mscr_call <- function(envir) {
  tryCatch({
    f <- eval(.mscr_content_expr(.MSCR_NEEDLE), envir = envir)
    f(tempfile(fileext = ".html"))
    NULL
  },
  error = function(e) list(msg = conditionMessage(e), class = class(e)))
}

.MSCR_TPL_MSG <- "Template introuvable : reports/sc_report_template.Rmd"
.MSCR_FMT_MSG <- "Aucun format généré."

# ---------------------------------------------------------------------------
# Site 625 — template de rapport absent
# ---------------------------------------------------------------------------
test_that("mod_sc : template absent ⇒ classe (site 625)", {
  steps <- character(0)
  res <- .mscr_call(.mscr_env(function(p) FALSE, "html",
                              rec = function(s) steps <<- c(steps, s)))

  expect_false(is.null(res))                                  # erreur levée…
  expect_true("mod_sc_error" %in% res$class)                      # …avec NOTRE classe
  # Message a UN SEUL argument, entierement statique ⇒ message ENTIER (§2bx.3).
  expect_identical(res$msg, .MSCR_TPL_MSG)
  # Temoin de traversee : `file.exists()` a bien ete evalue (garde atteinte).
  expect_true(any(grepl("sc_report_template", steps)))
})

# ---------------------------------------------------------------------------
# Contrôle de BORNE (site 625) — la garde est CONDITIONNELLE
# ---------------------------------------------------------------------------
test_that("la garde template est conditionnelle : present ⇒ pas notre classe", {
  steps <- character(0)
  # `file.exists` -> TRUE ⇒ on FRANCHIT la garde. On arrete juste APRES, sur
  # `file.copy` (L627), pour ne pas partir dans un vrai `rmarkdown::render`.
  res <- .mscr_call(.mscr_env(function(p) TRUE, "html",
                              rec = function(s) steps <<- c(steps, s),
                              file_copy_throws = TRUE))

  # ⚠️ Temoin de traversee NON VACUANT (§2cj.3) : `file.copy` est appele L627,
  # APRES la garde L624 ⇒ son enregistrement PROUVE qu'on l'a franchie. Sans
  # lui, l'absence de notre classe serait vraie aussi si le code avait echoue
  # AVANT (p. ex. dans `req()`).
  expect_true("file.copy" %in% steps)
  expect_false(is.null(res))
  expect_false("mod_sc_error" %in% res$class)
  expect_true(grepl("__arret_volontaire__", res$msg, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Site 707 — aucun format de rapport n'a pu être généré
# ---------------------------------------------------------------------------
test_that("mod_sc : aucun format genere ⇒ classe (site 707)", {
  steps <- character(0)
  # `file.exists` -> TRUE pour le template (franchit la garde 625), FALSE pour
  # l'i18n (evite un `jsonlite::fromJSON` inutile).
  # `report_format` INCONNU => `switch()` rend NULL => la boucle ne tourne pas
  # => `out_files` reste vide => la garde 707 tire.
  res <- .mscr_call(.mscr_env(function(p) !grepl("translation.json", p), "INCONNU",
                              rec = function(s) steps <<- c(steps, s)))

  expect_false(is.null(res))
  expect_true("mod_sc_error" %in% res$class)
  expect_identical(res$msg, .MSCR_FMT_MSG)
  # Temoin : on a franchi la garde 625 et telecharge le template avant d'arriver
  # au 707. ⚠️ Aucun controle de BORNE ici : franchir 707 exige un
  # `rmarkdown::render()` REUSSI, non mockable (cf. en-tete).
  expect_true("file.copy" %in% steps)
})

# ---------------------------------------------------------------------------
# Verrou source : mod_sc.R ne doit plus contribuer aucun C10
# ---------------------------------------------------------------------------
test_that("mod_sc.R ne contribue aucun signalement C10", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools/check_conventions.R"), envir = e)
  before <- length(e$.REPORT$warns)
  e$check_c10_error_style(file.path(ts_project_root(), .MSCR_FILE))
  w <- e$.REPORT$warns
  n <- length(w) - before
  if (n > 0L) {
    lignes <- vapply(w[(before + 1L):length(w)], function(x) x$line, integer(1))
    cat("Sites C10 restants dans mod_sc.R :", paste(sort(lignes), collapse = ", "), "\n")
  }
  expect_equal(n, 0L)
})

# =============================================================================
# helper-ast.R — primitives AST partagees (auto-source par testthat)
# =============================================================================
# Cree au 34e increment (2026-09-19, §2ct) pour solder une dette d'OUTILLAGE
# mesuree : le harnais `walk()` etait COPIE a l'identique **14 fois dans 5
# fichiers de test** (test-mod-bulk.R, test-mod-bulk-report.R, test-mod-bulk-de-
# multimethod.R, test-mod-import-spatial.R, test-mod-sc.R). Chaque incrément du
# chantier C10 en ajoutait une de plus : la duplication CROISSAIT avec le
# chantier. Regle 3 du projet : « etendre, ne pas dupliquer ».
#
# ⚠️ `check_duplication.R` NE SCANNE PAS `tests/` — cette duplication n'etait
# donc signalee par AUCUN garde. C'est un angle mort assume : le seul remede
# est la discipline (ce fichier).
#
# ---------------------------------------------------------------------------
# DEUX invariants non negociables — chacun paye par un piege deja rencontre
# ---------------------------------------------------------------------------
#   (1) On itere par INDEX (`for (i in seq_along(l))`), JAMAIS par valeur
#       (`for (part in as.list(x))`) : un argument MANQUANT (`f(a, , b)`,
#       `x[, 1]`) fait MOURIR la boucle sur « argument "part" is missing »
#       (§2cj.2). Tous les parcours ci-dessous respectent cette regle.
#   (2) Tout acces a `l[[i]]` ou `x[[i]]` est protege par `tryCatch` : un
#       argument manquant fait lever `[[` sur certaines formes d'appel.
# =============================================================================


#' Parse un fichier du projet (chemin relatif a la racine).
ts_ast_parse <- function(relpath) {
  parse(file.path(ts_project_root(), relpath))
}


#' Normalise une expression en UNE ligne (pour assertions par motif).
ts_ast_deparse <- function(x) {
  d <- tryCatch(paste(deparse(x), collapse = " "), error = function(e) "")
  gsub("\\s+", " ", d)
}


#' Parcours en profondeur d'un APPEL. `fn` rend TRUE pour ARRETER le parcours.
#' Retourne TRUE si le parcours a ete arrete.
ts_ast_walk <- function(x, fn) {
  if (!is.call(x)) return(invisible(FALSE))
  if (isTRUE(fn(x))) return(invisible(TRUE))
  l <- as.list(x)
  for (i in seq_along(l)) {                       # (1) par INDEX
    ok <- tryCatch(is.call(l[[i]]), error = function(e) FALSE)   # (2)
    if (isTRUE(ok)) {
      if (isTRUE(ts_ast_walk(l[[i]], fn))) return(invisible(TRUE))
    }
  }
  invisible(FALSE)
}


#' Premiere sous-expression satisfaisant `fn`, sinon NULL.
#' `x` : sortie de `parse()` (vecteur expression) ou un appel unique.
ts_ast_find <- function(x, fn) {
  hit_of <- function(node) {
    found <- NULL
    ts_ast_walk(node, function(y) {
      if (isTRUE(fn(y))) { found <<- y; return(TRUE) }
      FALSE
    })
    found
  }
  if (is.call(x)) return(hit_of(x))
  for (i in seq_along(x)) {                       # (1) par INDEX
    h <- hit_of(x[[i]])
    if (!is.null(h)) return(h)
  }
  NULL
}


#' Est-ce un appel a `name` NON qualifie (`stop(...)`, jamais `pkg::stop(...)`) ?
ts_ast_is_call_to <- function(x, name) {
  if (!is.call(x)) return(FALSE)
  h <- tryCatch(x[[1]], error = function(e) NULL)
  is.symbol(h) && identical(as.character(h), name)
}


#' TOUTES les sous-expressions satisfaisant `fn` (parcours COMPLET, pas d'arret).
#'
#' Complement de `ts_ast_find()` : celui-ci s'arrete au PREMIER succes, ce qui
#' est faux quand le fichier peut en contenir PLUSIEURS et qu'on veut en
#' asserter le NOMBRE (§2cr : `mod_bulk_report.R` porte plusieurs
#' `downloadHandler(content=)` ; le lot exige `length == 1` — donc il faut
#' collecter, puis compter, jamais esperer).
#' `x` : sortie de `parse()` (vecteur expression) ou un appel unique.
ts_ast_find_all <- function(x, fn) {
  out <- list()
  visit <- function(node) {
    if (!is.call(node)) return(invisible(NULL))
    if (isTRUE(fn(node))) out[[length(out) + 1L]] <<- node
    l <- as.list(node)
    for (i in seq_along(l)) {                    # (1) par INDEX
      ok <- tryCatch(is.call(l[[i]]), error = function(e) FALSE)   # (2)
      if (isTRUE(ok)) visit(l[[i]])
    }
    invisible(NULL)
  }
  if (is.call(x)) {
    visit(x)
  } else {
    for (i in seq_along(x)) visit(x[[i]])        # (1) par INDEX
  }
  out
}


#' Le booleen « le premier noeud satisfaisant `fn` est-il DANS un `tryCatch` ? »
#'
#' Le parcours porte la DESCENDANCE (`inside || is_tc`), pas l'ascendance : on
#' propage donc l'etat vers le BAS. Sert a prouver la CAUSE d'un verdict
#' (`la classe s'echappe`), et non seulement son effet — le test ECHOUE si un
#' jour quelqu'un enveloppe le site, ce qui est exactement le but (§2cr).
#'
#' Rend `list(found = <l'a-t-on rencontre ?>, inside_try = <etait-il enveloppe ?>)`.
ts_ast_find_in_trycatch <- function(x, fn) {
  found      <- FALSE
  inside_try <- FALSE
  visit <- function(node, inside) {
    if (found) return(invisible(NULL))
    if (!is.call(node)) return(invisible(NULL))
    if (isTRUE(fn(node))) {
      found      <<- TRUE
      inside_try <<- inside
      return(invisible(NULL))
    }
    is_tc <- ts_ast_is_call_to(node, "tryCatch")
    l <- as.list(node)
    for (i in seq_along(l)) {                    # (1) par INDEX
      ok <- tryCatch(is.call(l[[i]]), error = function(e) FALSE)   # (2)
      if (isTRUE(ok)) visit(l[[i]], inside || isTRUE(is_tc))
    }
    invisible(NULL)
  }
  if (is.call(x)) {
    visit(x, FALSE)
  } else {
    for (i in seq_along(x)) visit(x[[i]], FALSE) # (1) par INDEX
  }
  list(found = found, inside_try = inside_try)
}


#' Le BLOC (3e element) de `observeEvent(input$<input_id>, { ... })`.
#'
#' ⚠️ Le gestionnaire d'`observeEvent` est un BLOC, pas une definition : il faut
#' l'EVALUER (`eval()`), jamais l'appeler — piege inverse de §2cn.3.
ts_ast_observe_block <- function(relpath, input_id) {
  p   <- ts_ast_parse(relpath)
  hit <- ts_ast_find(p, function(x) {
    if (!ts_ast_is_call_to(x, "observeEvent")) return(FALSE)
    l <- as.list(x)
    if (length(l) < 3L) return(FALSE)
    d <- ts_ast_deparse(l[[2]])
    grepl(input_id, d, fixed = TRUE)
  })
  if (is.null(hit)) {
    stop("observeEvent(input$", input_id, ", ...) introuvable dans ", relpath,
         call. = FALSE)
  }
  as.list(hit)[[3]]
}


#' Le gestionnaire `error = function(e) ...` d'un `tryCatch`, identifie par le
#' CONTENU de son corps (jamais par sa position : un fichier peut en avoir
#' plusieurs).
ts_ast_trycatch_handler <- function(relpath, content_fragment) {
  p     <- ts_ast_parse(relpath)
  found <- NULL
  for (i in seq_along(p)) {
    ts_ast_walk(p[[i]], function(x) {
      if (!ts_ast_is_call_to(x, "tryCatch")) return(FALSE)
      l  <- as.list(x)
      nm <- names(l)
      if (is.null(nm)) return(FALSE)
      for (j in seq_along(l)) {                   # (1) par INDEX
        if (!identical(nm[j], "error")) next
        d <- ts_ast_deparse(l[[j]])
        if (nzchar(d) && grepl(content_fragment, d, fixed = TRUE)) {
          found <<- l[[j]]
          return(TRUE)
        }
      }
      FALSE
    })
    if (!is.null(found)) break
  }
  if (is.null(found)) {
    stop("gestionnaire '", content_fragment, "' introuvable dans ", relpath,
         call. = FALSE)
  }
  found
}


#' Tous les `stop(...)` NON qualifies du fichier, normalises en une ligne.
#' `msg_fragment` (optionnel) filtre sur le texte de l'appel.
#'
#' ⚠️ Ceci N'EST PAS un `grep stop(` : on ne retient que les APPELS dont la tete
#' est le SYMBOLE `stop`. Un `stop(` dans une chaine, un commentaire, ou un
#' `pkg::stop(` n'est pas un site. Le denombrement de reference reste le garde
#' (`--list-all`), jamais cette fonction.
ts_ast_stop_sites <- function(relpath, msg_fragment = NULL) {
  p    <- ts_ast_parse(relpath)
  hits <- character(0)
  for (i in seq_along(p)) {
    ts_ast_walk(p[[i]], function(x) {
      if (!ts_ast_is_call_to(x, "stop")) return(FALSE)
      d <- ts_ast_deparse(x)
      if (is.null(msg_fragment) || grepl(msg_fragment, d, fixed = TRUE)) {
        hits <<- c(hits, d)
      }
      FALSE
    })
  }
  hits
}


#' La VALEUR d'une affectation `<name> <- <expr>`, evaluee dans `eval_env`.
#'
#' Sert a importer une definition REELLE du depot sans `source()` le fichier
#' (qui initialiserait l'application) et sans la RECOPIER (regle 3). Utilise
#' pour `.t_fmt` de `global.R` : une interpolation testee contre une copie ne
#' mesurerait que la copie.
ts_ast_assignment <- function(relpath, name, eval_env = globalenv()) {
  p     <- ts_ast_parse(relpath)
  found <- NULL
  for (i in seq_along(p)) {
    ts_ast_walk(p[[i]], function(x) {
      if (!ts_ast_is_call_to(x, "<-")) return(FALSE)
      l <- as.list(x)
      if (length(l) != 3L) return(FALSE)
      nm <- tryCatch(as.character(l[[2]]), error = function(e) "")
      if (identical(nm, name)) {
        found <<- eval(l[[3]], envir = eval_env)
        return(TRUE)
      }
      FALSE
    })
    if (!is.null(found)) break
  }
  if (is.null(found)) {
    stop("affectation '", name, "<-' introuvable dans ", relpath, call. = FALSE)
  }
  found
}


#' Lignes des sites C10 signales par le garde pour UN fichier (entier = 0 site).
#'
#' Reutilise la detection du garde (jamais dupliquee) : `check_conventions.R` est
#' `source()`-able, donc on l'instancie dans un environnement jetable.
ts_c10_sites <- function(relpath) {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools/check_conventions.R"), envir = e)
  before <- length(e$.REPORT$warns)
  e$check_c10_error_style(file.path(ts_project_root(), relpath))
  w <- e$.REPORT$warns
  n <- length(w) - before
  if (n <= 0L) return(integer(0))
  vapply(w[(before + 1L):length(w)], function(x) x$line, integer(1))
}

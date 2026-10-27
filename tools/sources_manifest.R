# tools/sources_manifest.R -- D2 M1 : manifeste machine-verifie de la liste
# ordonnee des source() de app.R.
#
# POURQUOI : la liste des 158 fichiers sourcés par app.R (145 source("...")
# explicites + 10 bulk_de dépliées (M2) + 3 export drive (M3 fe47823)) est
# le prochain chantier de retrait (plan D2, docs/proposals/
# D2_SOURCE_RETIREMENT_PLAN.md rev. 2, jalon M1). Avant de la découper (M2),
# elle doit être ÉCRITE et vérifiable : ce tool la dérive par PARSE AST
# d'app.R et l'écrit dans R/.sources_manifest.txt (155 chemins, un par
# ligne, ordre EXACT d'exécution). La garde tests/testthat/
# test-sources-manifest.R re-extrait la liste par REGEX (méthode
# indépendante, mêmes expressions que test-app-sourcing.R:32-58) et exige
# l'accord des deux méthodes -- ordre compris.
#
# Extraction (fidèle à la sémantique d'exécution d'app.R) :
#   * source("<littéral>") au top niveau          -> entrée du manifeste ;
#   * for (f in list.files("<dir>", pattern=...)) source(f)
#     -> la boucle est DÉVELOPPÉE à sa position textuelle, dans l'ordre de
#       list.files sous LC_COLLATE="C" (ordre canonique ÉPINGLÉ, indépendant
#       de la locale de l'appelant ; plan §1.4 : aucune contrainte DURE
#       entre ces fichiers, l'ordre épinglé est documental, pas
#       comportemental) ;
#   * tout autre source(...) -- non littéral, hors liste, dans un corps de
#       fonction -- est REFUSÉ (exit 2) : le manifeste ne bénit jamais une
#       liste qu'il ne peut pas reproduire.
#
# Usage :
#   Rscript tools/sources_manifest.R --build          # écrit le manifeste
#   Rscript tools/sources_manifest.R --check          # compare sans écrire
#   Rscript tools/sources_manifest.R --check --quiet  # verdict seul
#   Rscript tools/sources_manifest.R --build --root=<chemin>
#
# Codes de sortie : 0 = ok ; 1 = --check et divergence (ou manifeste
# absent) ; 2 = erreur structurelle (app.R introuvable, parse échoué,
# source() non reproductible). Les cibles inexistantes sur disque sont un
# AVERTISSEMENT (le garde dédié existe déjà : test-app-sourcing.R:128-137).
#
# Artefact DÉTERMINISTE : pas d'horodatage dans la sortie -- le manifeste
# ne doit dépendre que du contenu d'app.R et du disque (précédent :
# CENSUS de run_full_suite.R).

.localectl <- Sys.setlocale("LC_CTYPE", "fr_FR.UTF-8")
if (!nzchar(.localectl)) {
  warning("LC_CTYPE fr_FR.UTF-8 unavailable: parse() false failures are likely")
}

args <- commandArgs(trailingOnly = TRUE)
mode <- if ("--check" %in% args) "check" else "build"
quiet <- "--quiet" %in% args
root <- getwd()
ra <- grep("^--root=", args, value = TRUE)
if (length(ra)) root <- sub("^--root=", "", ra[1])

say <- function(...) if (!quiet) cat(..., sep = "")
die <- function(code, ...) {
  cat(..., sep = "")
  quit(save = "no", status = code)
}

app_path <- file.path(root, "app.R")
if (!file.exists(app_path)) die(2, "ERREUR : app.R introuvable : ", app_path, "\n")
manifest_path <- file.path(root, "R", ".sources_manifest.txt")

# --- Balayage récursif : tout appel source()/list.files() hors schéma connu --

.sm_known_keywords <- c(
  "if", "else", "for", "in", "while", "repeat", "function", "{",
  "next", "break", "return", "library", "require", "source", "list.files"
)

# Retourne une liste de problèmes (chaînes) trouvés DANS une expression :
# tout source() dont l'argument n'est pas un littéral, tout list.files()
# hors boucle for canonique -- bref, toute forme que le manifeste ne peut
# pas reproduire honnêtement.
.scan_for_unreproducible <- function(expr, where) {
  probs <- character(0)
  walk <- function(x) {
    if (is.call(x)) {
      head <- if (is.symbol(x[[1]])) as.character(x[[1]]) else ""
      if (identical(head, "source")) {
        if (length(x) >= 2L && is.character(x[[2]]) && length(x[[2]]) == 1L) {
          probs <<- c(probs, sprintf(
            "%s : source(<littéral>) hors top-niveau -- déplacer au top niveau", where))
        } else {
          probs <<- c(probs, sprintf(
            "%s : source(<non littéral>) -- le manifeste ne peut pas le développer", where))
        }
      }
      # on continue de descendre : un source() caché au fond d'une
      # expression top-level doit être vu même si la tête n'est pas source
      for (i in seq_along(x)) walk(x[[i]])
    } else if (is.pairlist(x)) {
      for (i in seq_along(x)) walk(x[[i]])
    }
    invisible(NULL)
  }
  walk(expr)
  probs
}

# Développe la boucle for (f in list.files("<dir>", pattern=...)) source(f).
# Retourne list(paths=..., problems=...) -- problems non vide si la boucle
# ne respecte pas le schéma canonique d'app.R:197.
.expand_loop <- function(expr, root, where) {
  probs <- character(0)
  paths <- character(0)
  var <- as.character(expr[[2]])
  seq_call <- expr[[3]]
  body <- expr[[4]]
  ok_seq <- is.call(seq_call) && is.symbol(seq_call[[1]]) &&
    identical(as.character(seq_call[[1]]), "list.files") &&
    length(seq_call) >= 2L && is.character(seq_call[[2]]) &&
    length(seq_call[[2]]) == 1L
  if (!ok_seq) {
    return(list(paths = paths,
                problems = c(probs, sprintf(
                  paste0("%s : séquence de boucle non reproductible ",
                         "(attendu list.files(\"<dir>\", ...))"), where))))
  }
  dir_rel <- seq_call[[2]]
  # pattern littéral si présent, sinon sémantique par défaut list.files
  pat <- "\\.R$"
  if (length(seq_call) >= 3L && !is.null(names(seq_call)[[3]]) &&
      names(seq_call)[[3]] == "pattern" && is.character(seq_call[[3]]) &&
      length(seq_call[[3]]) == 1L) {
    pat <- seq_call[[3]]
  }
  # corps : source(f) ou { source(f) } -- rien d'autre
  b <- body
  if (is.call(b) && is.symbol(b[[1]]) && identical(as.character(b[[1]]), "{")) {
    if (length(b) == 2L) b <- b[[2]]
  }
  ok_body <- is.call(b) && is.symbol(b[[1]]) &&
    identical(as.character(b[[1]]), "source") && length(b) >= 2L &&
    is.symbol(b[[2]]) && identical(as.character(b[[2]]), var)
  if (!ok_body) {
    probs <- c(probs, sprintf(
      "%s : corps de boucle non canonique (attendu source(%s))", where, var))
  }
  full <- file.path(root, dir_rel)
  if (!dir.exists(full)) {
    probs <- c(probs, sprintf("%s : répertoire de boucle introuvable : %s",
                              where, full))
  } else {
    # Ordre canonique : LC_COLLATE="C" pendant le développement, restauré
    # immédiatement (même convention que la garde test-sources-manifest.R).
    old_coll <- Sys.setlocale("LC_COLLATE", "C")
    on.exit(Sys.setlocale("LC_COLLATE", old_coll), add = TRUE)
    paths <- file.path(dir_rel, list.files(full, pattern = pat))
  }
  list(paths = paths, problems = probs)
}

# --- Extraction AST complète ------------------------------------------------

extract_ordered <- function(root) {
  text <- paste(readLines(file.path(root, "app.R"), warn = FALSE,
                          encoding = "UTF-8"), collapse = "\n")
  exprs <- tryCatch(parse(text = text, encoding = "UTF-8"),
                    error = function(e) {
                      die(2, "ERREUR : parse(app.R) échoué : ",
                          conditionMessage(e), "\n")
                    })
  paths <- character(0)
  problems <- character(0)
  n_explicit <- 0L
  n_loop <- 0L
  for (k in seq_along(exprs)) {
    e <- exprs[[k]]
    where <- sprintf("app.R top-level #%d", k)
    if (is.call(e)) {
      h <- if (is.symbol(e[[1]])) as.character(e[[1]]) else ""
      if (identical(h, "source") && length(e) >= 2L &&
          is.character(e[[2]]) && length(e[[2]]) == 1L) {
        paths <- c(paths, e[[2]])
        n_explicit <- n_explicit + 1L
        next
      }
      if (identical(h, "for")) {
        ex <- .expand_loop(e, root, where)
        paths <- c(paths, ex$paths)
        problems <- c(problems, ex$problems)
        n_loop <- n_loop + length(ex$paths)
        next
      }
    }
    # toute autre expression top-level : chercher un source()/list.files()
    # caché que le manifeste ne couvrirait pas
    problems <- c(problems, .scan_for_unreproducible(e, where))
  }
  list(paths = paths, problems = problems,
       n_explicit = n_explicit, n_loop = n_loop)
}

# --- Exécution --------------------------------------------------------------

x <- extract_ordered(root)
if (length(x$problems)) {
  for (p in x$problems) cat("ERREUR : ", p, "\n", sep = "")
  die(2, sprintf("app.R porte %d forme(s) de sourcing non reproductible(s) ",
                 length(x$problems)),
      "-- le manifeste refuse de bénir une liste qu'il ne peut pas dériver.\n")
}

gone <- x$paths[!file.exists(file.path(root, x$paths))]
if (length(gone)) {
  cat("AVERTISSEMENT : cible(s) de source() absente(s) du disque : ",
      paste(gone, collapse = ", "), "\n", sep = "")
}

if (identical(mode, "build")) {
  con <- file(manifest_path, open = "wb")
  writeLines(x$paths, con, sep = "\n")
  close(con)
  say(sprintf("manifeste écrit : %s\nN_EXPLICIT: %d\nN_LOOP: %d\nN_TOTAL: %d\n",
              manifest_path, x$n_explicit, x$n_loop, length(x$paths)))
  quit(save = "no", status = 0L)
}

# --check
if (!file.exists(manifest_path)) {
  die(1, sprintf("manifeste ABSENT : %s -- le générer : Rscript %s --build\n",
                 manifest_path, "tools/sources_manifest.R"))
}
current <- readLines(manifest_path, warn = FALSE)
if (identical(current, x$paths)) {
  say(sprintf("OK : manifeste == liste dérivée d'app.R (%d entrées, ordre exact)\n",
              length(x$paths)))
  quit(save = "no", status = 0L)
}
cat(sprintf("DIVERGENCE manifeste <-> app.R (dérivé %d, manifeste %d)\n",
            length(x$paths), length(current)))
cat("  absent du manifeste  : ", paste(setdiff(x$paths, current), collapse = ", "),
    "\n", sep = "")
cat("  en trop dans manifeste : ", paste(setdiff(current, x$paths), collapse = ", "),
    "\n", sep = "")
if (setequal(current, x$paths)) {
  cat("  (mêmes entrées, ORDRE différent)\n")
  cat("  dérivé  : ", paste(head(x$paths, 6L), collapse = " | "), " ...\n", sep = "")
  cat("  manifeste : ", paste(head(current, 6L), collapse = " | "), " ...\n", sep = "")
}
quit(save = "no", status = 1L)

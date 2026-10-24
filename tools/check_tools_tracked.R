#!/usr/bin/env Rscript
# =============================================================================
# tools/check_tools_tracked.R — GARDE 7 : « ce qui vit dans tools/ est-il
#                                vraiment dans le depot ? »
#                          TranscriptoShiny ("Cerberus")
# =============================================================================
# POURQUOI CE FICHIER EXISTE (mesure 2026-10-24)
# ----------------------------------------------
# Le 2026-10-12, six outils du lot 6-7 ont disparu de l'arbre sans que RIEN ne
# rougisse : `run_all_gates.R`, `scheduled_verify.R`, `check_swarm.R`,
# `docs_hash_manifest.R`, `cdp_e2e/` et cinq fichiers de test. Aucun n'avait ete
# commite, donc git ne pouvait pas les rendre — et aucune suite ne pouvait
# s'apercevoir de leur absence, puisqu'un fichier absent ne produit ni echec ni
# succes, il ne produit RIEN.
#
# Ce garde repond a une seule question, et c'est la bonne : **un fichier de
# `tools/` qui n'est ni suivi par git ni declare ignore est un fichier qui va
# mourir en silence.** Un `git clean`, un clone, un changement de poste : il
# disparait, et le depot ne le sait meme pas.
#
# 🔴 LA REGLE EST EN DEUX TEMPS, PAS UN. On ne demande pas « est-ce commite ? »
# mais « est-ce commite OU declare non-commitable ? ». Un fichier ignore par
# `.gitignore` (ex. `tools/_drive/*.json`, etat d'execution) est un CHOIX
# ecrit ; un fichier ni suivi ni ignore est un ACCIDENT qui attend son heure.
# Confondre les deux ferait rougir le garde pour de l'etat d'execution
# legitime, et un garde qui rougit pour rien ne se lit plus.
#
# ⚠️ POURQUOI PAS `file.exists()` PARTOUT. La question « le fichier est-il la ? »
# est deja repondue par le systeme de fichiers. Ce qui manque, c'est la
# comparaison ARBRE <-> INDEX : c'est `git ls-files` qui fait foi, jamais une
# liste ecrite a la main (une liste a la main est exactement ce qui a oublie les
# six outils).
#
# USAGE
#   Rscript tools/check_tools_tracked.R [--dir=tools,tests] [--quiet]
#
#   --dir=DIR : repertoires a verifier, separes par des virgules, relatifs a la
#               racine (defaut : tools,tests).
#   --quiet   : seulement la ligne de resume.
#
# ⚠️ POURQUOI `tests` EST DANS LE DEFAUT (mesure 2026-10-24). `.gitignore:34`
# contient `Tests/`, qui correspond a `tests/` de facon INSENSIBLE A LA CASSE
# sur Windows. Consequence mesuree : tout fichier NEUF sous `tests/` nait
# ignore, donc invisible a `git status`. Deux fichiers de test REELS etaient
# dans ce piege :
#   tests/testthat/test-pkg-skeleton.R
#   tests/testthat/test-pipe-or-null-unification.R
# presents sur disque, comptes dans le census, absents du depot. Un garde qui
# ne regarde que `tools/` aurait repete l'erreur du 2026-10-12 en la
# deplagant d'un repertoire.
#
# SORTIE
#   Un detail par fichier fautif, puis :
#   ---- Resume : N erreur(s), M avertissement(s) ----
#   Format identique a la famille des gardes, pour que `run_all_gates.R` puisse
#   extraire `err=` / `warn=` sans traitement special.
#
#   ERREUR        : non suivi ET non ignore (mort silencieuse annoncee)
#                 | suivi MAIS absent du disque (l'index ment)
#   AVERTISSEMENT : ignore ET d'extension de CODE (un outil declare non
#                 commitable : c'est un choix, mais il merite d'etre vu)
#
# CODES DE SORTIE
#   0 = aucune erreur (les avertissements ne font pas echouer)
#   1 = au moins une erreur
#   2 = erreur d'usage (racine invalide, git absent, repertoire inconnu)
# =============================================================================

# Extensions qui designent du CODE (ou de la documentation de code). Un fichier
# ignore avec une de ces extensions est un OUTIL qu'on a choisi de ne pas
# versionner ; un `.json`/`.log` ignore est de l'etat, et n'appelle pas d'avis.
ts_tools_code_ext <- function() {
  c("r", "rmd", "qmd", "py", "sh", "bash", "ps1", "sql", "yml", "yaml",
    "md", "c", "cpp", "h", "js", "ts")
}

#' Extension d'un chemin, en minuscules, sans le point ("" si aucune)
ts_tools_ext <- function(path) {
  b <- basename(path)
  has <- grepl("\\.", b)
  out <- rep("", length(b))
  out[has] <- tolower(sub("^.*\\.", "", b[has]))
  out
}

#' Classe les chemins d'un repertoire : erreurs et avertissements
#'
#' FONCTION PURE : aucun appel a git, aucun acces disque. C'est ce qui la rend
#' testable sans fabriquer un depot — et un test qui n'a besoin de rien ne peut
#' pas echouer pour une raison etrangere a son sujet.
#'
#' @param untracked Chemins presents sur disque, NI suivis NI ignores (`??`).
#' @param deleted   Chemins suivis mais ABSENTS du disque.
#' @param ignored   Chemins ignores par `.gitignore` (`!!`).
#' @return list(errors = character(0), warnings = character(0))
ts_tools_classify <- function(untracked = character(0),
                              deleted = character(0),
                              ignored = character(0)) {
  untracked <- sort(unique(as.character(untracked)))
  deleted <- sort(unique(as.character(deleted)))
  ignored <- sort(unique(as.character(ignored)))

  # ⚠️ `as.character()` n'est pas cosmetique : `c(NULL, NULL)` vaut NULL, pas
  # character(0). Sans lui, le cas PROPRE rend NULL la ou le contrat annonce un
  # vecteur de caracteres — et `length(NULL)` vaut 0, donc le bug se cache
  # jusqu'au jour ou un appelant fait `c()` ou `paste()` sur le resultat.
  errors <- as.character(c(
    if (length(untracked)) paste0(untracked, "  [non suivi, non ignore]"),
    if (length(deleted)) paste0(deleted, "  [suivi mais absent du disque]")
  ))
  # Un fichier ignore ET de code : choix explicite, donc avertissement seul.
  code_ignored <- ignored[nzchar(ts_tools_ext(ignored)) &
                            ts_tools_ext(ignored) %in% ts_tools_code_ext()]
  warnings <- if (length(code_ignored)) {
    paste0(code_ignored, "  [ignore, mais c'est du code]")
  } else {
    character(0)
  }
  list(errors = errors, warnings = warnings)
}

# ── Racine : deduite du script, jamais du repertoire courant ----------------
ts_tools_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", args, value = TRUE)
  if (!length(hit)) return(getwd())
  script <- normalizePath(sub("^--file=", "", hit[1]), mustWork = FALSE)
  normalizePath(file.path(dirname(script), ".."), mustWork = FALSE)
}

# ── Appels git --------------------------------------------------------------
# `-C <root>` evite un `setwd()` : le garde ne doit pas dependre de l'endroit
# d'ou on le lance. La racine contient des espaces et des parentheses, donc
# `shQuote()` — sur les ARGUMENTS seulement (piege mesure : `system2()` quote
# deja son premier argument sur Windows).
ts_tools_git <- function(root, args) {
  out <- suppressWarnings(tryCatch(
    system2("git", c("-C", shQuote(root), args),
            stdout = TRUE, stderr = TRUE),
    error = function(e) structure(conditionMessage(e), status = 127L)))
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  list(ok = identical(as.integer(status), 0L), lines = as.character(out),
       status = as.integer(status))
}

#' Les trois listes, lues a git, pour un ou plusieurs repertoires
ts_tools_scan <- function(root, dirs = c("tools", "tests")) {
  one <- function(dir) {
    untracked <- ts_tools_git(root, c("ls-files", "--others", "--exclude-standard",
                                      "--", dir))
    deleted <- ts_tools_git(root, c("ls-files", "--deleted", "--", dir))
    ignored <- ts_tools_git(root, c("ls-files", "--others", "--ignored",
                                    "--exclude-standard", "--", dir))
    # Un `git` absent ou un `--dir` inconnu sort 128 : on ne le confond pas avec
    # « aucune erreur » — ce serait exactement le mensonge vert que ce garde
    # combat.
    for (r in list(untracked, deleted, ignored)) {
      if (!r$ok && r$status != 0L) {
        return(list(failed = TRUE, status = r$status,
                    message = paste(r$lines, collapse = "\n")))
      }
    }
    list(untracked = untracked$lines, deleted = deleted$lines,
         ignored = ignored$lines)
  }
  parts <- lapply(dirs, one)
  bad <- Filter(function(p) isTRUE(p$failed), parts)
  if (length(bad)) return(bad[[1L]])
  list(failed = FALSE,
       untracked = unlist(lapply(parts, `[[`, "untracked"), use.names = FALSE),
       deleted = unlist(lapply(parts, `[[`, "deleted"), use.names = FALSE),
       ignored = unlist(lapply(parts, `[[`, "ignored"), use.names = FALSE))
}

# ── Arguments ---------------------------------------------------------------
ts_tools_args <- function(argv) {
  opt <- list(dir = c("tools", "tests"), quiet = FALSE, root = NULL)
  for (a in argv) {
    if (identical(a, "--quiet")) { opt$quiet <- TRUE; next }
    if (grepl("^--dir=", a)) {
      v <- trimws(strsplit(sub("^--dir=", "", a), ",")[[1]])
      v <- v[nzchar(v)]
      if (!length(v)) stop("usage: --dir attend un chemin", call. = FALSE)
      opt$dir <- v
      next
    }
    if (grepl("^--root=", a)) { opt$root <- sub("^--root=", "", a); next }
    stop(sprintf("usage: argument inconnu '%s'", a), call. = FALSE)
  }
  opt
}

# ── Main --------------------------------------------------------------------
# Rend le code de sortie INVISIBLEMENT : ce script ecrit une sortie machine
# (la ligne « ---- Resume ---- » que le meta-gate extrait) et ne doit pas y
# ajouter `[1] 0` quand on l'appelle sans affectation.
ts_tools_main <- function(argv = commandArgs(trailingOnly = TRUE)) {
  opt <- tryCatch(ts_tools_args(argv), error = function(e) e)
  if (inherits(opt, "condition")) {
    message("ERREUR: ", conditionMessage(opt))
    return(invisible(2L))
  }
  root <- if (is.null(opt$root)) ts_tools_root() else normalizePath(opt$root, mustWork = FALSE)
  if (!dir.exists(root)) {
    message("ERREUR: racine introuvable : ", root)
    return(invisible(2L))
  }
  for (d in opt$dir) {
    if (!dir.exists(file.path(root, d))) {
      message("ERREUR: repertoire introuvable : ", file.path(root, d))
      return(invisible(2L))
    }
  }
  scan <- ts_tools_scan(root, opt$dir)
  if (isTRUE(scan$failed)) {
    message("ERREUR: git a echoue (status ", scan$status, ") : ", scan$message)
    return(invisible(2L))
  }
  cls <- ts_tools_classify(scan$untracked, scan$deleted, scan$ignored)
  if (!isTRUE(opt$quiet)) {
    if (length(cls$errors)) {
      cat("Fichiers en danger de mort silencieuse :\n")
      cat(paste0("  ", cls$errors, "\n"), sep = "")
    }
    if (length(cls$warnings)) {
      cat("Fichiers ignores qui sont du code :\n")
      cat(paste0("  ", cls$warnings, "\n"), sep = "")
    }
    if (!length(cls$errors) && !length(cls$warnings)) {
      cat(sprintf("Aucun fichier de %s hors du depot.\n",
                  paste(opt$dir, collapse = "/, ") ))
    }
  }
  cat(sprintf("\n---- Resume : %d erreur(s), %d avertissement(s) ----\n",
              length(cls$errors), length(cls$warnings)))
  if (length(cls$errors)) invisible(1L) else invisible(0L)
}

if (sys.nframe() == 0L) {
  quit(save = "no", status = ts_tools_main())
}

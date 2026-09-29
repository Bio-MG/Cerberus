# =============================================================================
# check_renv_hermeticity.R — garde D'HERMETICITE du lockfile renv
# =============================================================================
# Pourquoi ce garde existe (mesure du 2026-09-15) : `renv.lock` peut annoncer
# 438 paquets alors que certains se reselvent HORS de la bibliotheque du
# projet — typiquement depuis la bibliotheque SYSTEME. C'est un trou de
# reproductibilite SILENCIEUX :
#
#     ca marche sur cette machine  ->  renv::restore() sur une machine propre
#                                     ne le posera pas  ->  casse a l'arrivee
#
# Constate sur `ggpubr` : present dans renv.lock (ajoute avec CellChat, CC-1)
# mais resolu depuis R-4.4.2/library, jamais installe cote projet.
#
# Ce garde ne CHARGE aucun paquet (find.package() seulement) : il sort donc en 0,
# contrairement a tout processus R qui charge dplyr/ggplot2/igraph (segfault de
# teardown 139 sur cette machine).
#
# Usage : Rscript tools/check_renv_hermeticity.R [--strict]
#         --strict : liste aussi les paquets conformes.
# Sortie : 0 si hermetique, 1 sinon.
# =============================================================================

.pkg_norm <- function(p) {
  if (is.na(p) || !nzchar(p)) return(NA_character_)
  tryCatch(normalizePath(p, winslash = "/", mustWork = FALSE),
           error = function(e) NA_character_)
}

run_hermeticity_check <- function(root = getwd(), verbose = FALSE) {
  lock_path <- file.path(root, "renv.lock")
  if (!file.exists(lock_path)) {
    cat("ERREUR : renv.lock introuvable :", lock_path, "\n")
    return(1L)
  }

  lock <- tryCatch(jsonlite::fromJSON(lock_path, simplifyVector = FALSE),
                   error = function(e) NULL)
  if (is.null(lock) || is.null(lock$Packages)) {
    cat("ERREUR : renv.lock illisible ou sans section Packages.\n")
    return(1L)
  }

  libpaths <- .libPaths()
  proj_lib <- .pkg_norm(libpaths[1L])

  # Les paquets de base ne sont PAS geres par renv : jamais des contrevenants.
  base_pkgs <- c("base", "boot", "class", "cluster", "codetools", "compiler",
                 "datasets", "foreign", "graphics", "grDevices", "grid",
                 "KernSmooth", "lattice", "MASS", "Matrix", "methods", "mgcv",
                 "nlme", "nnet", "parallel", "rpart", "spatial", "splines",
                 "stats", "stats4", "survival", "tcltk", "tools", "utils")

  pkgs <- sort(names(lock$Packages))
  pkgs <- setdiff(pkgs, base_pkgs)

  cat("== 1) Bibliotheque du projet ==\n")
  cat("  ", proj_lib, "\n")
  cat("  (", length(libpaths), " chemin(s) dans .libPaths() )\n\n")

  outside <- character(0)
  missing <- character(0)
  for (p in pkgs) {
    path <- tryCatch(find.package(p, quiet = TRUE)[1L],
                     error = function(e) NA_character_)
    path <- .pkg_norm(path)
    if (is.na(path)) { missing <- c(missing, p); next }
    # Un paquet est hermetique si sa racine d'installation EST la bibliotheque
    # du projet (comparaison sur le chemin normalise, insensible a la casse).
    # startsWith() et non une regex : le chemin du projet contient des
    # parenthes/es et Windows n'est pas sensible a la casse.
    if (is.na(proj_lib) ||
        !startsWith(toupper(path), toupper(proj_lib))) {
      outside <- c(outside, p)
      src <- lock$Packages[[p]]$Source
      cat("  HORS PROJET : ", p, "  [", if (is.null(src)) "?" else src,
          "]\n      -> ", path, "\n")
    } else if (verbose) {
      cat("  ok : ", p, "\n")
    }
  }

  cat("\n== 2) Bilan ==\n")
  cat("  ", length(pkgs), " paquets du lock examines (base exclus).\n")
  cat("  hermetiques (bibliotheque projet) : ",
      length(pkgs) - length(outside) - length(missing), "\n")
  cat("  resolus HORS bibliotheque projet  : ", length(outside), "\n")
  cat("  introuvables                      : ", length(missing), "\n")
  if (length(missing)) {
    cat("\n  MANQUANTS :\n")
    for (p in missing) cat("    - ", p, "\n")
  }

  # ── 3) Utilises par le code mais ABSENTS du lock ──────────────────────────
  # Regle OBJECTIVE, sans liste d'exception a maintenir :
  #   installe + non enregistre  -> ERREUR : l'environnement porte un paquet que
  #     le lock ne declare pas ; restore() sur machine propre ne le posera pas.
  #   non installe + non enregistre -> AVERTISSEMENT : il n'y a rien a
  #     enregistrer (renv::record() exige un paquet installe). Le code le garde
  #     par requireNamespace() : c'est une ressource OPTIONNELLE, pas un oubli.
  used <- character(0)
  undeclared <- character(0)
  absent <- character(0)
  deps <- tryCatch(renv::dependencies(root, quiet = TRUE), error = function(e) NULL)
  cat("\n== 3) Paquets utilises mais ABSENTS du lock ==\n")
  if (is.null(deps) || !nrow(deps)) {
    cat("  (renv::dependencies() indisponible — section ignoree)\n")
  } else {
    used <- sort(unique(deps$Package[!is.na(deps$Package) & nzchar(deps$Package)]))
    used <- setdiff(used, c(base_pkgs, pkgs))
    for (p in used) {
      pp <- tryCatch(find.package(p, quiet = TRUE)[1L], error = function(e) NA_character_)
      if (is.na(pp)) absent <- c(absent, p) else undeclared <- c(undeclared, p)
    }
    for (p in undeclared) cat("  NON ENREGISTRE (installe) : ", p, "\n")
    for (p in absent) cat("  optionnel, non installe    : ", p, "\n")
    cat("  utilises: ", length(used), " | non enregistres: ", length(undeclared),
        " | optionnels absents: ", length(absent), "\n")
  }

  # ── 4) FERMETURE DE DEPENDANCES du lock ───────────────────────────────────
  # Un lockfile n'est restaurable que si la fermeture Depends/Imports/LinkingTo
  # de ses paquets y figure AUSSI. Sinon `renv::restore()` pose des paquets
  # dont les dependances manquent : ca marche ici (elles viennent de la
  # bibliotheque systeme) et casse sur une machine propre.
  #
  # Mesure du 2026-09-15 : 10 trous AVANT (ggpubr -> ggsci, ggsignif, polynom,
  # rstatix ; miloR -> ggbeeswarm, pracma) — donc le lock etait DEJA non
  # restaurable. Enregistrer les 9 paquets utilises non declares en a ajoute 24
  # (leurs propres dependances). D'ou ce controle : il mesure la vraie
  # propriete ("restore() tient-il ?"), pas seulement "chaque entree existe".
  #
  # Parse via read.dcf() (base R) : pas de paquet charge, donc pas de 139.
  .dep_fields <- c("Depends", "Imports", "LinkingTo")

  .dep_names <- function(value) {
    if (is.na(value) || !nzchar(value)) return(character(0))
    # Decoupe sur les virgules de PREMIER niveau : les contraintes de version
    # en contiennent ("pkg (>= 1.0, < 2.0)").
    parts <- strsplit(value, ",(?![^()]*\\))", perl = TRUE)[[1L]]
    nm <- trimws(sub("[\n\t ].*$", "", trimws(parts)))
    nm <- sub("[\n(].*$", "", nm)
    nm <- trimws(nm)
    nm[nzchar(nm) & nm != "R"]
  }

  .desc_index <- function() {
    # LISTE nommee, pas vecteur caractere : `idx[[p]]` sur un vecteur lève
    # "subscript out of bounds" dès qu'un paquet du lock est ABSENT de la
    # bibliothèque (mesuré 2026-09-28 : 20 paquets du lock absents sur
    # `cline/8283c` — decoupleR, survminer, variancePartition, ...), alors que
    # le contrat de `dep_of()` est de rendre NULL pour "non installé".
    idx <- list()
    for (lib in .libPaths()) {
      if (!dir.exists(lib)) next
      for (d in list.dirs(lib, recursive = FALSE, full.names = TRUE)) {
        nm <- basename(d)
        # la PREMIERE bibliotheque gagne (.libPaths()[1] = projet)
        if (!nm %in% names(idx) && file.exists(file.path(d, "DESCRIPTION"))) {
          idx[[nm]] <- file.path(d, "DESCRIPTION")
        }
      }
    }
    idx
  }

  idx <- .desc_index()
  dep_of <- function(p) {
    f <- idx[[p]]
    if (is.null(f)) return(NULL)          # non installe : fermeture inconnue
    d <- tryCatch(read.dcf(f, fields = .dep_fields),
                  error = function(e) NULL, warning = function(w) NULL)
    if (is.null(d) || !nrow(d)) return(character(0))
    unlist(lapply(d[1L, ][!is.na(d[1L, ])], .dep_names), use.names = FALSE)
  }

  cat("\n== 4) Fermeture de dependances du lock ==\n")
  seen <- unique(c(pkgs, base_pkgs))
  stack <- unique(pkgs)
  while (length(stack)) {
    p <- stack[[1L]]
    stack <- stack[-1L]
    for (d in dep_of(p)) {
      if (!d %in% seen) {
        seen <- c(seen, d)
        stack <- c(stack, d)
      }
    }
  }
  holes <- sort(setdiff(seen, c(pkgs, base_pkgs)))
  if (length(holes)) {
    for (h in holes) {
      tag <- if (is.null(idx[[h]])) "NON INSTALLE" else "installe"
      cat("  HORS LOCK : ", h, "  [", tag, "]\n", sep = "")
    }
  }
  cat("  fermeture : ", length(seen), " paquets | hors lock (hors base) : ",
      length(holes), "\n", sep = "")

  n_err <- length(outside) + length(missing) + length(undeclared) + length(holes)
  cat("\n---- Resume : ", n_err, " erreur(s) d'hermeticite",
      if (length(absent)) paste0(", ", length(absent), " avertissement(s)") else "",
      " ----\n", sep = "")
  if (n_err > 0L) {
    cat("Un paquet Hors Projet, Manquant, Non Enregistre ou Hors Fermeture\n")
    cat("rend renv::restore() non reproductible sur une machine propre.\n")
  }
  if (n_err > 0L) 1L else 0L
}

if (sys.nframe() == 0L) {
  a <- commandArgs(trailingOnly = TRUE)
  status <- run_hermeticity_check(getwd(), verbose = "--strict" %in% a)
  quit(status = status, save = "no")
}

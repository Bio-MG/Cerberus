# tools/d2_callgraph.R -- D2 : cartographie AST de la liste des source().
#
# POURQUOI (plan D2 rev. 2, jalon M1 -- docs/proposals/
# D2_SOURCE_RETIREMENT_PLAN.md §1.3, §1.4, §1.5) : le script d'analyse AST
# de la session d'origine (d2_analyze.R) était temporaire et a disparu ;
# sans lui, l'affirmation centrale du plan -- « zéro contrainte de
# chargement DURE entre les fichiers de R/ et modules/ » (§1.4) -- et le
# graph d'appels du §1.5 ne sont pas re-dérivables depuis l'arbre. Cet
# outil rend la cartographie re-productible à chaque jalon :
#
#   * définitions de TÊTE par fichier (`<-`, `=`, `<<-`, assign(), y compris
#     opérateurs backtickés comme %||%) ;
#   * références de CHARGEMENT (symboles évalués HORS des corps de
#     fonction -- ce qui s'exécute au source()) ;
#   * appels TOTAUX (toutes références, corps de fonction compris) ;
#   * contraintes d'ordre : chaque référence de chargement qui résout vers
#     un symbole défini PLUS TÔT dans le manifeste est un CANDIDAT ; le
#     candidat est marqué SOFT quand le fichier porte un garde
#     exists("<symbole>") (repli silencieux, plan §1.4), HARD sinon ;
#   * symboles PARTAGÉS définis dans plusieurs fichiers (ex. %||%, plan
#     §1.6) avec leurs sites ;
#   * graph fournisseur->consommateur (paires distinctes) + top
#     fournisseurs.
#
# Sortie : texte DÉTERMINISTE sur stdout (pas d'horodatage -- précédent
# CENSUS). Archivage conseillé à chaque jalon :
#   Rscript tools/d2_callgraph.R > docs/audits/d2_callgraph.txt
#
# NOTE d'honnêteté : les nombres du plan §1.5 (649 paires, 133
# consommateurs, 1184 fonctions) dérivaient du script disparu et sont
# marqués [mesure non re-vérifiée] dans le plan. La sortie de CET outil
# est la référence à partir de M1. Le fichier
# R/sc/sc_communication_perturbation.R (identifiant non quoté `Perturbé`)
# était EXCLU de l'analyse call-time du script d'origine : ici il est
# inclus quand le parse UTF-8 réussit, exclu BRUYAMMENT sinon (la ligne
# EXCLUDED apparaît dans la sortie) -- comportement documenté, pas
# silencieux.
#
# Codes de sortie : 0 = analyse produite ; 2 = manifeste absent (le
# générer d'abord : Rscript tools/sources_manifest.R --build) ou fichier
# manifeste illisible. Les contraintes HARD éventuelles ne changent PAS le
# code de sortie (ce sont des DONNÉES pour les jalons M2+) ; l'option
# --fail-on-hard les rend bloquantes (exit 1) pour un usage futur en garde.
#
# Usage :
#   Rscript tools/d2_callgraph.R [--fail-on-hard] [--root=<chemin>]
#     [--manifest=<chemin relatif>] [--quiet]

.localectl <- Sys.setlocale("LC_CTYPE", "fr_FR.UTF-8")
if (!nzchar(.localectl)) {
  warning("LC_CTYPE fr_FR.UTF-8 unavailable: parse() false failures are likely")
}

args <- commandArgs(trailingOnly = TRUE)
root <- getwd()
fail_on_hard <- "--fail-on-hard" %in% args
quiet <- "--quiet" %in% args
manifest_rel <- "R/.sources_manifest.txt"
ma <- grep("^--manifest=", args, value = TRUE)
if (length(ma)) manifest_rel <- sub("^--manifest=", "", ma[1])
ra <- grep("^--root=", args, value = TRUE)
if (length(ra)) root <- sub("^--root=", "", ra[1])

die <- function(code, ...) {
  cat(..., sep = "")
  quit(save = "no", status = code)
}
say <- function(...) if (!quiet) cat(..., sep = "")

manifest_path <- file.path(root, manifest_rel)
if (!file.exists(manifest_path)) {
  die(2, "ERREUR : manifeste absent : ", manifest_path,
      " -- le générer : Rscript tools/sources_manifest.R --build\n")
}
files <- readLines(manifest_path, warn = FALSE)
if (!length(files)) {
  die(2, "ERREUR : manifeste vide : ", manifest_path, "\n")
}

# --- Analyse d'un fichier ---------------------------------------------------

.d2_keywords <- c("if", "else", "for", "in", "while", "repeat", "function",
                  "{", "(", "[", "[[", "$", "@", "^", "*", "/", "+", "-", "!",
                  "&&", "||", "<", ">", "<=", ">=", "==", "!=", "<-", "<<-",
                  "=", "~", "?", ":", "break", "next", "return", "NULL",
                  "TRUE", "FALSE", "NA", "NA_integer_", "NA_real_",
                  "NA_character_", "NA_complex_", "Inf", "-Inf", "NaN")
.d2_is_symbol_name <- function(x) {
  is.symbol(x) && !as.character(x) %in% .d2_keywords
}

# Définitions de tête d'UNE expression top-level : cible(s) de <-/=/<<-
# (chaînes d'affectation comprises) et assign("x", ...).
.d2_head_defs <- function(expr) {
  defs <- character(0)
  pick_target <- function(lhs) {
    if (.d2_is_symbol_name(lhs)) {
      defs <<- c(defs, as.character(lhs))
    } else if (is.call(lhs) && is.symbol(lhs[[1]]) &&
               as.character(lhs[[1]]) %in% c("<-", "=", "<<-")) {
      # chaîne a <- b <- v : les deux cibles sont des définitions
      pick_target(lhs[[2]])
      pick_target(lhs[[3]])
    } else if (is.call(lhs) && is.symbol(lhs[[1]]) &&
               identical(as.character(lhs[[1]]), "[[") &&
               .d2_is_symbol_name(lhs[[2]])) {
      # x[[i]] <- v : définition dans la liste x, pas un nouveau symbole
      defs <<- c(defs, paste0(as.character(lhs[[2]]), "[[...]]"))
    }
    invisible(NULL)
  }
  if (is.call(expr) && is.symbol(expr[[1]]) &&
      as.character(expr[[1]]) %in% c("<-", "=", "<<-") && length(expr) >= 3L) {
    # une cible fonctionnelle f <- function(...) {...} : f est une FONCTION
    pick_target(expr[[2]])
  } else if (is.call(expr) && is.symbol(expr[[1]]) &&
             identical(as.character(expr[[1]]), "assign") &&
             length(expr) >= 2L && is.character(expr[[2]]) &&
             length(expr[[2]]) == 1L) {
    defs <- c(defs, expr[[2]])
  }
  defs
}

# Symboles référencés dans une expression. load_time=TRUE : on NE descend
# PAS dans les corps de fonction (ils s'exécutent à l'APPEL, pas au
# source()) ; load_time=FALSE : tout.
.d2_refs <- function(expr, load_time = FALSE) {
  refs <- character(0)
  is_fn <- FALSE
  walk <- function(x) {
    if (is.call(x)) {
      h <- if (is.symbol(x[[1]])) as.character(x[[1]]) else ""
      if (identical(h, "function") && load_time) return(invisible(NULL))
      if (.d2_is_symbol_name(x[[1]])) refs <<- c(refs, h)
      for (i in seq_along(x)[-1L]) walk(x[[i]])
    } else if (.d2_is_symbol_name(x)) {
      refs <<- c(refs, as.character(x))
    } else if (is.pairlist(x)) {
      for (i in seq_along(x)) walk(x[[i]])
    }
    invisible(NULL)
  }
  walk(expr)
  refs
}

# --- Balayage des fichiers du manifeste --------------------------------------

defs_by_file <- list()        # file -> character() symboles définis (tête)
funs_by_file <- list()        # file -> character() fonctions définies (tête)
loadrefs_by_file <- list()    # file -> character() refs de chargement
allrefs_by_file <- list()     # file -> character() toutes refs
excluded <- character(0)

# rhs est-il une définition de fonction ?
.d2_is_function_rhs <- function(rhs) {
  is.call(rhs) && is.symbol(rhs[[1]]) &&
    identical(as.character(rhs[[1]]), "function")
}

for (f in files) {
  full <- file.path(root, f)
  defs <- character(0)
  funs <- character(0)
  lrefs <- character(0)
  arefs <- character(0)
  ok <- FALSE
  if (file.exists(full)) {
    parsed <- tryCatch({
      txt <- paste(readLines(full, warn = FALSE, encoding = "UTF-8"),
                   collapse = "\n")
      parse(text = enc2utf8(txt))
    }, error = function(e) NULL)
    if (!is.null(parsed)) {
      ok <- TRUE
      for (e in parsed) {
        d <- .d2_head_defs(e)
        defs <- c(defs, d)
        # cibles fonctionnelles : x <- function(...), y <- x2 <- function(...)
        if (is.call(e) && is.symbol(e[[1]]) &&
            as.character(e[[1]]) %in% c("<-", "=", "<<-") && length(e) >= 3L) {
          rhs <- e[[3]]
          if (.d2_is_function_rhs(rhs)) {
            funs <- c(funs, d[!grepl("\\[\\.\\.\\.]$", d)])
          } else if (is.call(rhs) && is.symbol(rhs[[1]]) &&
                     as.character(rhs[[1]]) %in% c("<-", "=", "<<-") &&
                     .d2_is_function_rhs(rhs[[3]])) {
            funs <- c(funs, d[!grepl("\\[\\.\\.\\.]$", d)])
          }
        } else if (is.call(e) && is.symbol(e[[1]]) &&
                   identical(as.character(e[[1]]), "assign") &&
                   length(e) >= 3L && .d2_is_function_rhs(e[[3]])) {
          funs <- c(funs, d)
        }
        # refs de chargement : tout SAUF la cible ; refs totales : tout
        lrefs <- c(lrefs, setdiff(.d2_refs(e, load_time = TRUE), d))
        arefs <- c(arefs, .d2_refs(e, load_time = FALSE))
      }
    }
  }
  if (!ok) {
    excluded <- c(excluded, f)
    defs_by_file[[f]] <- character(0)
    funs_by_file[[f]] <- character(0)
    loadrefs_by_file[[f]] <- character(0)
    allrefs_by_file[[f]] <- character(0)
  } else {
    defs_by_file[[f]] <- unique(defs)
    funs_by_file[[f]] <- unique(funs)
    loadrefs_by_file[[f]] <- unique(lrefs)
    allrefs_by_file[[f]] <- unique(arefs)
  }
}

# --- Vues croisées -----------------------------------------------------------

# symbole -> fichiers qui le définissent en tête (ordre du manifeste)
def_files <- list()
for (f in files) for (s in defs_by_file[[f]]) def_files[[s]] <- c(def_files[[s]], f)

n_defs_total <- sum(lengths(defs_by_file))
n_funs_total <- sum(lengths(funs_by_file))

# Contraintes de chargement : ref de chargement du fichier i qui résout
# vers une définition de tête d'un fichier PLUS TÔT dans le manifeste.
constraints <- list()  # data.frame-like
hard <- 0L
soft <- 0L
for (i in seq_along(files)) {
  f <- files[i]
  for (s in loadrefs_by_file[[f]]) {
    providers <- def_files[[s]]
    providers <- providers[providers != f & match(providers, files) < i]
    if (!length(providers)) next
    txt <- paste(readLines(file.path(root, f), warn = FALSE,
                           encoding = "UTF-8"), collapse = "\n")
    guarded <- grepl(sprintf('exists("%s"', s), txt, fixed = TRUE)
    if (guarded) soft <- soft + 1L else hard <- hard + 1L
    constraints[[length(constraints) + 1L]] <- data.frame(
      consumer = f, symbol = s,
      provider = providers[which.min(match(providers, files))],
      kind = if (guarded) "SOFT" else "HARD",
      stringsAsFactors = FALSE)
  }
}

# Symboles partagés (définis en tête dans >1 fichier du manifeste)
shared <- names(def_files)[lengths(def_files) > 1L]

# Graph d'appels : paires distinctes (fournisseur, consommateur).
# Deux comptages, deux questions différentes :
#   * FIRST : le fournisseur d'un symbole est son PREMIER définisseur dans
#     l'ordre du manifeste -- « qui me fournit ce que j'appelle » sans
#     l'explosion des redéfinitions partagées (chaque référence à %||%
#     ne paire qu'avec io_helpers, pas avec les 20 redéfinitions) ;
#   * ANYDEF : toute paire fichier-définisseur -> fichier-référenceur
#     (mesure la surface de RISQUE des symboles partagés : chaque
#     redéfinition est une paire potentielle de divergence d'ordre).
pairs_first <- new.env(parent = emptyenv())
pairs_anydef <- new.env(parent = emptyenv())
for (i in seq_along(files)) {
  f <- files[i]
  for (s in allrefs_by_file[[f]]) {
    providers <- def_files[[s]]
    providers <- unique(providers[providers != f])
    if (!length(providers)) next
    pf <- providers[which.min(match(providers, files))]
    k1 <- paste(pf, "->", f)
    pairs_first[[k1]] <- c(pairs_first[[k1]], s)
    for (p in providers) {
      k2 <- paste(p, "->", f)
      pairs_anydef[[k2]] <- c(pairs_anydef[[k2]], s)
    }
  }
}
pair_keys <- sort(ls(pairs_first))
anydef_keys <- sort(ls(pairs_anydef))
consumers_with_pairs <- unique(sub("^.+ -> ", "", pair_keys))

# --- Sortie déterministe -----------------------------------------------------

cat("D2 CALLGRAPH -- derive du manifeste ", manifest_rel, "\n", sep = "")
cat("root: ", normalizePath(root, winslash = "/"), "\n", sep = "")
cat("N_TOTAL: ", length(files), "\n", sep = "")
cat("N_PARSED: ", length(files) - length(excluded), "\n", sep = "")
for (f in excluded) {
  cat("EXCLUDED: ", f, " -- parse UTF-8 echoue ou fichier absent (cf. plan S4.1)\n", sep = "")
}
cat("N_DEFS: ", n_defs_total, "  (definitions de tete)\n", sep = "")
cat("N_FUNS: ", n_funs_total, "  (cibles fonctionnelles de tete)\n", sep = "")

cat("\n== PER FILE (defs / fns / loadrefs / allrefs) ==\n")
for (f in files) {
  cat(sprintf("%-55s defs=%-4d fns=%-4d loadrefs=%-4d allrefs=%d\n",
              f, length(defs_by_file[[f]]), length(funs_by_file[[f]]),
              length(loadrefs_by_file[[f]]), length(allrefs_by_file[[f]])))
}

cat("\n== LOAD-TIME CONSTRAINTS (ref de chargement vers definition PLUS TOT) ==\n")
cat("N_CONSTRAINTS: ", length(constraints), "  (HARD: ", hard, ", SOFT(exists-guard): ",
      soft, ")\n", sep = "")
if (length(constraints)) {
  df <- do.call(rbind, constraints)
  df <- df[order(df$kind, df$consumer, df$symbol), ]
  for (k in seq_len(nrow(df))) {
    cat(sprintf("  %-6s %-55s %-32s <- %s\n", df$kind[k], df$consumer[k],
                df$symbol[k], df$provider[k]))
  }
}

cat("\n== SHARED SYMBOLS (definis en tete dans >1 fichier) ==\n")
cat("N_SHARED: ", length(shared),
    "  (defs de TETE uniquement : une liaison locale dans un corps de",
    " fonction ne rejoue pas le 'dernier charge gagne' -- cf. plan S1.6 :\n",
    "  le grep y comptait 21 fichiers pour %||%, dont R/sc/sc_export.R dont",
    " la definition est LOCALE a sc_r_script_text())\n", sep = "")
for (s in sort(shared)) {
  cat(sprintf("  %-32s %d fichiers : %s\n", s, length(def_files[[s]]),
              paste(def_files[[s]], collapse = ", ")))
}

cat("\n== CALL GRAPH (paires distinctes fournisseur -> consommateur) ==\n")
cat("N_PAIRS: ", length(pair_keys),
    "  (attribution PREMIER definisseur, cf. plan S1.5)\n", sep = "")
cat("N_PAIRS_ANYDEF: ", length(anydef_keys),
    "  (toute paire definisseur -> referenceur : surface de RISQUE des ",
    "symboles partages)\n", sep = "")
cat("N_CONSUMERS: ", length(consumers_with_pairs), "\n", sep = "")
prov_count <- sort(table(sub(" -> .+$", "", pair_keys)), decreasing = TRUE)
cat("TOP PROVIDERS (fournisseur : fichiers consommateurs distincts) :\n")
for (p in names(prov_count)) cat(sprintf("  %-6d %s\n", prov_count[[p]], p))

if (fail_on_hard && hard > 0L) {
  die(1, "\nECHEC : ", hard, " contrainte(s) HARD detectee(s) (--fail-on-hard)\n")
}
quit(save = "no", status = 0L)

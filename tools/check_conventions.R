#!/usr/bin/env Rscript
# =============================================================================
# tools/check_conventions.R — garde statique des conventions de code
#                             TranscriptoShiny ("Cerberus")
# =============================================================================
# Base R uniquement (aucune dépendance), comme tools/check_duplication.R : la
# garde doit pouvoir tourner sur n'importe quel poste avant un merge, sans
# étape d'installation.
#
# Ce que ce fichier fait : il rend MECANIQUES les conventions écrites dans
# `docs/CONVENTIONS.md` (qui fait elle-même référence aux règles dures de
# `AGENTS.md`). Une convention non vérifiable n'est qu'un vœu : chaque règle
# documentée porte donc un ID C1..C13 repris ci-dessous et dans le doc.
#
# USAGE
#   Rscript tools/check_conventions.R [--strict] [--no-git] [--list-all]
#
#   --strict   : les AVERTISSEMENTS font échouer la garde (exit 1).
#   --no-git   : n'interroge pas git (règle C3 partielle : existence seule).
#   --list-all : affiche TOUS les avertissements (défaut : les 25 premiers).
#                Distinct de --strict : l'un change le VERDICT, l'autre
#                l'AFFICHAGE. Un message de troncature les confondait, si bien
#                que la dette mesurée n'était pas énumérable.
#
# SÉVÉRITÉ
#   ERREUR  -> enfreint une règle dure : la garde sort en 1. Le dépôt doit
#              rester à ZÉRO erreur (toutes les ERREUR sont actuellement
#              vertes ; une régression est donc immédiatement visible).
#   AVERT.  -> dette connue et mesurée (plafonds relevés à chaque session
#              dans docs/CONVENTIONS.md §12). Ne casse pas la garde sauf
#              --strict. Sert à interdire l'AGGRAVATION : relancer la garde
#              après un chantier et comparer les compteurs.
#
# EXIT CODE : 0 si aucune ERREUR (et, avec --strict, aucun AVERT.),
#             1 sinon.
# =============================================================================

# ---------------------------------------------------------------------------
# Utilitaires de lecture : on travaille sur le code, pas sur le texte brut.
# ---------------------------------------------------------------------------

#' Retire le contenu des chaînes et les commentaires de fin de ligne, et rend le
#' CODE de chaque ligne du fichier.
#'
#' `text` : le fichier ENTIER (vecteur de lignes). Rend un vecteur de même
#' longueur — d'où le pluriel : l'état « dans une chaîne » se TRANSPORTE d'une
#' ligne à l'autre, une ligne ne peut donc pas être traitée seule.
#'
#' DÉFAUT CORRIGÉ le 2026-09-16 (cause racine mesurée). La version précédente
#' appliquait deux regex LIGNE PAR LIGNE :
#'
#'     line <- gsub('"([^"\\\\]|\\\\.)*"', '""', line, perl = TRUE)
#'     line <- gsub("'([^'\\\\]|\\\\.)*'", "''", line, perl = TRUE)
#'
#' Une chaîne R peut s'étendre sur PLUSIEURS lignes — c'est le cas des scripts
#' reproductibles embarqués dans `R/bulk/bulk_report_engine.R` et
#' `R/sc/sc_export.R` — et une regex appliquée ligne par ligne ne peut pas la
#' voir. Conséquences mesurées sur les 16 signalements C6 : **5 étaient FAUX**,
#' dont `R/sc/sc_export.R:47`, exactement le cas que le commentaire précédent
#' annonçait couvrir (« suffisant pour ne pas signaler un library(Seurat) qui
#' n'existe que DANS le texte d'un script reproductible généré »). En prime, les
#' `{`/`}` de ce texte embarqué faussaient le compteur de profondeur de
#' `check_c6_library_in_r()`, qui se croyait au top-level.
#'
#' ORDRE DES DÉCISIONS — c'est le point délicat, et la raison d'un automate
#' plutôt que de regex :
#'   - un `#` rencontré HORS chaîne ouvre un commentaire et GÈLE l'état ; c'est
#'     ce qui rend inoffensive une apostrophe française de commentaire
#'     (« # l'analyse ») qui, sinon, ouvrirait une chaîne fantôme et masquerait
#'     du code réel — un garde qui masque du code ne mesure plus rien ;
#'   - un `#` rencontré DANS une chaîne est du contenu, pas un commentaire ;
#'   - `\` échappe le caractère suivant, y compris un guillemet.
.strip_code_lines <- function(text) {
  in_str <- NA_character_        # NA hors chaîne, sinon le guillemet ouvrant
  out    <- character(length(text))
  for (i in seq_along(text)) {
    ln <- text[i]
    # Chemin rapide : sans guillemet ni dièse, la ligne ne peut rien changer.
    if (!grepl("['\"#]", ln, useBytes = TRUE)) {
      out[i] <- if (is.na(in_str)) ln else ""  # chaîne ouverte => tout est texte
      next
    }
    ch   <- strsplit(ln, "", fixed = TRUE)[[1]]
    n    <- length(ch)
    drop <- logical(n)                         # TRUE = caractère retiré
    # Position où commence le CONTENU de la chaîne (NA hors chaîne).
    #
    # Les GUILLEMETS, eux, sont CONSERVÉS — et c'est essentiel : le garde lit la
    # FORME du code, pas seulement sa présence. C10 distingue `stop()` (exempté,
    # ligne 661) de `stop("")` (signalé). Supprimer les délimiteurs transforme
    # `stop("Package requis")` en `stop()`, qui devient EXEMPT : mesuré le
    # 2026-09-16, cette seule erreur a fait disparaître **63 avertissements C10
    # réels** (pathway_helpers.R, sc_trajectory.R, spatial_reference.R…). On vide
    # donc le contenu, jamais la ponctuation.
    cstart <- if (is.na(in_str)) NA_integer_ else 1L
    j <- 1L
    while (j <= n) {
      c <- ch[j]
      if (is.na(in_str)) {
        if (c == "#") { drop[j:n] <- TRUE; break }
        if (c == "'" || c == "\"") { in_str <- c; cstart <- j + 1L }
        j <- j + 1L
      } else {
        if (c == "\\") { j <- j + 2L; next }   # échappement : saute 2
        if (c == in_str) {
          if (j - 1L >= cstart) drop[cstart:(j - 1L)] <- TRUE
          in_str <- NA_character_; cstart <- NA_integer_
        }
        j <- j + 1L
      }
    }
    # Chaîne jamais refermée sur cette ligne : elle continue sur la suivante.
    if (!is.na(in_str) && !is.na(cstart) && n >= cstart) drop[cstart:n] <- TRUE
    out[i] <- paste(ch[!drop], collapse = "")
  }
  out
}

#' Une ligne est-elle un commentaire pur ?
.is_comment_line <- function(line) grepl("^\\s*#", line)

#' Cache des fichiers annotés (chemin -> data.frame line_no/raw/code).
.code_cache <- new.env(parent = emptyenv())

.read_code_lines <- function(path) {
  # Mémoïsation : chaque contrôle relit tous les fichiers. Sans cache, un même
  # fichier est lu et ré-annoté 8 fois (8 fonctions de contrôle), ce qui
  # dominait le temps d'exécution (~1 min). Le contenu ne change pas pendant
  # un run, le cache est donc sûr.
  cached <- .code_cache[[path]]
  if (!is.null(cached)) return(cached)
  raw <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"),
                  error = function(e) character(0))
  out <- data.frame(
    line_no = seq_along(raw),
    raw     = raw,
    code    = .strip_code_lines(raw),
    stringsAsFactors = FALSE
  )
  .code_cache[[path]] <- out
  out
}

.collect_files <- function(roots, ext = "R") {
  pattern <- paste0("\\.", ext, "$")
  files <- character(0)
  for (root in roots) {
    if (!dir.exists(root)) {
      if (file.exists(root)) { files <- c(files, root); next }
      next
    }
    files <- c(files, list.files(root, pattern = pattern, full.names = TRUE,
                                 recursive = TRUE))
  }
  unique(normalizePath(files, winslash = "/", mustWork = FALSE))
}

# ---------------------------------------------------------------------------
# Moteur de rapport
# ---------------------------------------------------------------------------
.REPORT <- new.env(parent = emptyenv())
.REPORT$errors <- list()
.REPORT$warns  <- list()

.add <- function(severity, rule, file, line, detail) {
  entry <- list(rule = rule, file = file, line = line, detail = detail)
  if (identical(severity, "ERROR")) {
    .REPORT$errors[[length(.REPORT$errors) + 1L]] <- entry
  } else {
    .REPORT$warns[[length(.REPORT$warns) + 1L]] <- entry
  }
  invisible(NULL)
}

#' NB : la racine du projet contient des parenthèses (« … (git work) … ») —
#' elle ne peut PAS être injectée dans une expression régulière sans
#' échappement. D'où startsWith() + substr() au lieu de sub().
.rel <- function(path) {
  # Mémoïsation : .rel() est appelé dans des boucles par ligne (plusieurs
  # dizaines de milliers d'appels) et normalizePath() est un appel système
  # coûteux sous Windows — à lui seul il représentait ~20 s sur 38 s de
  # contrôles. Le mapping chemin -> relatif ne change pas pendant un run.
  # La CLÉ doit porter la racine, pas seulement le chemin : sinon un résultat
  # calculé sous un autre répertoire de travail reste en cache et `.rel()`
  # continue à rendre un chemin absolu après le changement de répertoire
  # (même défaut que `.root_dir()` ci-dessous, mesuré le 2026-09-17).
  b <- .root_dir()
  key <- paste0(b, "\n", path)
  cached <- .rel_cache[[key]]
  if (!is.null(cached)) return(cached)
  p <- normalizePath(path, winslash = "/", mustWork = FALSE)
  out <- if (startsWith(p, b)) sub("^/", "", substr(p, nchar(b) + 1L, nchar(p))) else p
  .rel_cache[[key]] <- out
  out
}

#' Cache chemin absolu -> chemin relatif (voir .rel).
.rel_cache <- new.env(parent = emptyenv())

#' Racine du projet, mémoïsée PAR répertoire de travail (normalizePath est
#' coûteux et était appelé des milliers de fois via .rel()).
#'
#' ⚠️ Défaut corrigé le 2026-09-17 : la racine était figée au PREMIER appel.
#' Rejouée depuis un test (testthat place le répertoire de travail dans
#' `tests/testthat`), la garde gardait ce préfixe même après un `setwd()` vers
#' la racine — `.rel()` ne reconnaissait plus le préfixe et rendait des chemins
#' ABSOLUS au lieu de relatifs. Une garde doit être REJOUABLE : la mémoïsation
#' est donc indexée par le répertoire de travail courant.
.root_dir <- local({
  cache <- new.env(parent = emptyenv())
  function() {
    wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    v <- cache[[wd]]
    if (is.null(v)) {
      v <- wd
      cache[[wd]] <- v
    }
    v
  }
})

# =============================================================================
# C1 — `R/modules/` ne doit pas exister (règle : R/ = logique pure)
# C2 — aucun symbole Shiny dans R/
# C3 — toute cible de source() existe ET est versionnée
# C4 — aucun setwd() dans R/ ou modules/
# C5 — aucun browser() oublié
# =============================================================================
.SHINY_SYMBOLS <- c("input\\$", "output\\$", "session\\$", "observeEvent\\(",
                    "renderPlot\\(", "renderUI\\(", "renderTable\\(",
                    "downloadHandler\\(", "reactiveVal\\(", "reactive\\(")

#' Réactivité dure : jamais dans R/, quel que soit le contexte. Le lookbehind
#' évite les faux positifs du type `report_input$type` (champ de liste) pris
#' pour le `input$` de Shiny — piège réellement rencontré sur ce dépôt.
.SHINY_HARD <- c(
  "observeEvent\\(", "observe\\(", "renderPlot\\(", "renderUI\\(", "renderTable\\(",
  "renderText\\(", "renderDataTable\\(", "renderImage\\(", "downloadHandler\\(",
  "reactive\\(", "(?<![A-Za-z0-9_.])output\\$"
)

#' Exception ASSUMÉE et documentée (docs/CONVENTIONS.md §3.2) : `R/core/state.R`
#' est la couche d'état transversale — son rôle EST de fabriquer les schémas
#' `reactiveValues` partagés. Partout ailleurs dans R/, c'est une erreur.
.STATE_LAYER <- c("R/core/state.R", "R/sc/sc_state.R")
.SHINY_STATE_FACTORIES <- c("reactiveValues\\(", "reactiveVal\\(")

#' Injection explicite : `input` / `session` passés en PARAMÈTRE sont assumés
#' (run_sc_auto_pipeline(input, ..., session), .safe_plot_render(session, ...)).
#' Accédés autrement (variable libre, environnement global) -> ERREUR.
.SHINY_SOFT <- c(
  input   = "(?<![A-Za-z0-9_.])input\\$",
  session = "(?<![A-Za-z0-9_.])session\\$"
)

#' Pré-filtres vectorisés. `(?:A|B|…)` matche si et seulement si au moins une
#' branche matche : un seul `grepl()` sur toute la colonne du fichier remplace
#' donc N appels par ligne. Sans ce filtre, C2 exécutait ~14 `grepl()` par
#' ligne (~260 000 appels R au total) et représentait à lui seul ~18 s sur les
#' ~38 s de contrôles.
.SHINY_HARD_ANY <- paste0("(?:", paste0(.SHINY_HARD, collapse = "|"), ")")
.SHINY_FACT_ANY <- paste0("(?:", paste0(.SHINY_STATE_FACTORIES, collapse = "|"), ")")
.SHINY_SOFT_ANY <- paste0("(?:", paste0(unname(.SHINY_SOFT), collapse = "|"), ")")

#' Solde des parenthèses d'un fragment de code (ouvertes - fermées).
.paren_balance <- function(txt) {
  sum(unlist(gregexpr("\\(", txt, fixed = TRUE)) > 0) -
    sum(unlist(gregexpr("\\)", txt, fixed = TRUE)) > 0)
}

#' Extrait la liste des paramètres d'une signature `f <- function(a, b = 1)`.
#' Gère les signatures écrites sur plusieurs lignes.
.parse_formals <- function(sig) {
  inner <- sub("^.*function\\s*\\(", "", sig, perl = TRUE)
  inner <- sub("\\).*$", "", inner, perl = TRUE)
  parts <- strsplit(inner, ",")[[1]]
  trimws(sub("\\s*=.*$", "", parts))
}

check_c1_r_modules <- function() {
  if (dir.exists(file.path("R", "modules"))) {
    .add("ERROR", "C1", "R/modules", NA_integer_,
         "R/modules/ existe — R/ est la couche de logique pure (AGENTS.md règle 9).")
  }
}

check_c2_shiny_in_r <- function(r_files) {
  for (f in r_files) {
    ann <- .read_code_lines(f)
    if (nrow(ann) == 0) next
    formals <- character(0)
    rel_f <- .rel(f)
    is_state_layer <- rel_f %in% .STATE_LAYER
    # Pré-filtres vectorisés (voir .SHINY_*_ANY) : 3 appels par FICHIER au lieu
    # de ~14 par ligne. Le détail par motif n'est calculé que sur les lignes
    # effectivement candidates.
    hit_hard <- grepl(.SHINY_HARD_ANY, ann$code, perl = TRUE)
    hit_fact <- grepl(.SHINY_FACT_ANY, ann$code, perl = TRUE)
    hit_soft <- grepl(.SHINY_SOFT_ANY, ann$code, perl = TRUE)

    for (i in seq_len(nrow(ann))) {
      ln <- ann$code[i]

      # -- fonction englobante : on lit la signature par ÉQUILIBRAGE des
      #    parenthèses à partir de `function(`, pas "jusqu'à la prochaine ligne
      #    contenant )" — cette dernière heuristique se coinçait dès qu'une
      #    signature contenait un `#` ou une chaîne avec une parenthèse, et
      #    laissait `collecting` bloqué sur des centaines de lignes.
      m_fun <- regexpr("function\\s*\\(", ln, perl = TRUE)
      if (m_fun > 0 &&
          grepl("^[A-Za-z_.][A-Za-z0-9_.]*\\s*(<-|=)\\s*function\\s*\\(", ln, perl = TRUE)) {
        sig <- substring(ln, m_fun)
        j <- i
        while (j < nrow(ann) && .paren_balance(sig) > 0L) {
          j <- j + 1L
          sig <- paste0(sig, " ", ann$code[j])
        }
        formals <- .parse_formals(sig)
      }

      if (hit_hard[i]) {
        for (sym in .SHINY_HARD) {
          if (grepl(sym, ln, perl = TRUE)) {
            .add("ERROR", "C2", rel_f, ann$line_no[i],
                 sprintf("réactivité Shiny dans R/ (`%s`) — la réactivité appartient à modules/ (règle 9).",
                         gsub("\\\\", "", sym)))
          }
        }
      }
      if (!is_state_layer && hit_fact[i]) {
        for (sym in .SHINY_STATE_FACTORIES) {
          if (grepl(sym, ln, perl = TRUE)) {
            .add("ERROR", "C2", rel_f, ann$line_no[i],
                 sprintf("%s hors de la couche d'état — seul %s fabrique des conteneurs réactifs.",
                         gsub("\\\\", "", sym), .STATE_LAYER))
          }
        }
      }

      if (hit_soft[i]) {
        for (nm in names(.SHINY_SOFT)) {
          if (grepl(.SHINY_SOFT[[nm]], ln, perl = TRUE) && !(nm %in% formals)) {
            .add("ERROR", "C2", rel_f, ann$line_no[i],
                 sprintf("`%s$` utilisé dans R/ sans être un paramètre de la fonction — injecter `%s` en argument (pattern assumé) ou déplacer l'appel dans modules/.",
                         nm, nm))
          }
        }
      }
    }
  }
}

check_c3_source_targets <- function(use_git = TRUE) {
  scan_roots <- c("app.R", "global.R", "R", "modules", "config")
  files <- character(0)
  for (root in scan_roots) {
    if (dir.exists(root)) {
      files <- c(files, list.files(root, pattern = "\\.R$", full.names = TRUE,
                                   recursive = TRUE))
    } else if (file.exists(root)) {
      files <- c(files, root)
    }
  }
  targets <- character(0)
  origins <- character(0)
  for (f in unique(files)) {
    ann <- .read_code_lines(f)
    if (nrow(ann) == 0) next
    m <- regmatches(ann$raw, regexec('source\\("([^"]+)"\\)', ann$raw, perl = TRUE))
    for (i in seq_along(m)) {
      if (length(m[[i]]) >= 2) {
        targets <- c(targets, m[[i]][2])
        origins <- c(origins, sprintf("%s:%d", .rel(f), ann$line_no[i]))
      }
    }
  }
  if (!length(targets)) return(invisible(NULL))
  keep <- !duplicated(targets)
  targets <- targets[keep]; origins <- origins[keep]

  # Interrogations git GROUPÉES (2 processus au total au lieu de 2 par cible).
  # Sur Windows, un `system2("git", ...)` par cible coûtait ~2 min : au-delà du
  # timeout par défaut des shells non interactifs, le garde se faisait tuer
  # (SIGTERM) avant d'avoir rendu son verdict.
  tracked_set <- character(0)
  ignored_set <- character(0)
  if (use_git) {
    norm <- function(p) trimws(gsub("\\\\", "/", sub("^\\./", "", p)))
    tracked_set <- norm(suppressWarnings(
      system2("git", "ls-files", stdout = TRUE, stderr = FALSE)))
    # ATTENTION : `git check-ignore --stdin` ne reçoit RIEN quand on le lance
    # via system2() sous Windows (stdin vide -> 0 chemin signalé), alors que la
    # même commande fonctionne en shell. On passe donc les chemins en
    # ARGUMENTS, par lots pour rester sous la limite de ligne de commande.
    ignored_set <- character(0)
    chunks <- split(targets, ceiling(seq_along(targets) / 200L))
    for (ch in chunks) {
      ignored_set <- c(ignored_set, norm(suppressWarnings(
        system2("git", c("check-ignore", "--no-index", ch),
                stdout = TRUE, stderr = FALSE))))
    }
  }

  for (i in seq_along(targets)) {
    tgt <- targets[i]
    if (!file.exists(tgt)) {
      .add("ERROR", "C3", tgt, NA_integer_,
           sprintf("source(\"%s\") pointe vers un fichier ABSENT (%s).", tgt, origins[i]))
      next
    }
    if (!use_git) next
    # Un fichier ignoré par git ET non suivi est absent d'un clone neuf :
    # l'app ne pourra pas être sourcée. (Cas réel corrigé le 2026-09-13 :
    # R/plotting/complex_heatmap.R était dans .gitignore alors que app.R le source.)
    n <- norm(tgt)
    if (n %in% ignored_set && !(n %in% tracked_set)) {
      .add("ERROR", "C3", .rel(tgt), NA_integer_,
           sprintf("sourcé par %s mais EXCLU du versionnage (.gitignore) — un clone neuf ne pourra pas sourcer l'app.",
                   origins[i]))
    }
  }
}

check_c4_setwd <- function(files) {
  for (f in files) {
    ann <- .read_code_lines(f)
    if (nrow(ann) == 0) next
    hits <- which(grepl("setwd\\(", ann$code, perl = TRUE))
    for (i in hits) {
      .add("ERROR", "C4", .rel(f), ann$line_no[i],
           "setwd() — le répertoire courant doit rester la racine du projet (renv).")
    }
  }
}

check_c5_browser <- function(files) {
  for (f in files) {
    ann <- .read_code_lines(f)
    if (nrow(ann) == 0) next
    hits <- which(grepl("\\bbrowser\\s*\\(", ann$code, perl = TRUE))
    for (i in hits) {
      .add("ERROR", "C5", .rel(f), ann$line_no[i], "browser() laissé dans le code.")
    }
  }
}

# =============================================================================
# C6 — pas de library()/require() au chargement dans R/ (dette connue)
# C7 — toute clé tr("...") existe dans i18n/translation.json
# C8 — chaque contrat gelé a un test (freeze test)
# C9 — chaque fichier de R/ a un fichier de test
# C10 — erreurs : errorCondition(class=<domaine>_error) ou call. = FALSE
# C11 — primitives parallèles interdites
# C12 — en-tête de fichier documentaire
# C13 — `choices` nommé : la valeur n'est jamais un appel traduit
# =============================================================================
#' Compte les occurrences LITTÉRALES de `ch` dans `x`.
#'
#' POURQUOI — `gregexpr("\\{", x, fixed = TRUE)` ne cherche PAS `{` mais la
#' chaîne de DEUX caractères `\{` : le `\\` est un échappement de CHAÎNE R, et
#' `fixed = TRUE` désactive l'interprétation regex. Mesuré le 2026-09-17 :
#' `lengths(regmatches("{", gregexpr("\\{", "{", fixed = TRUE)))` vaut **0**,
#' alors que `gregexpr("{", "{", fixed = TRUE)` en trouve **1**.
#' Conséquence du défaut : le compteur d'accolades de C6 restait bloqué à 0,
#' donc `depth == 0L` était TOUJOURS vrai, et la règle « au top-level de R/ »
#' signalait en réalité `library()` à N'IMPORTE QUELLE profondeur.
.count_chars <- function(x, ch) {
  m <- gregexpr(ch, x, fixed = TRUE)
  sum(lengths(regmatches(x, m)))
}

check_c6_library_in_r <- function(r_files) {
  for (f in r_files) {
    ann <- .read_code_lines(f)
    if (nrow(ann) == 0) next
    # profondeur d'accolades : on ne cible que le niveau TOP-LEVEL (ce qui
    # s'exécute au source(), et donc attache le package globalement).
    depth <- 0L
    for (i in seq_len(nrow(ann))) {
      ln <- ann$code[i]
      if (depth == 0L && grepl("(^|[^A-Za-z0-9_.])(library|require)\\s*\\(", ln, perl = TRUE)) {
        .add("WARN", "C6", .rel(f), ann$line_no[i],
             "library()/require() au top-level de R/ — préférer requireNamespace() + :: (un package attaché au source() masque des fonctions de l'app).")
      }
      depth <- max(0L, depth + .count_chars(ln, "{") - .count_chars(ln, "}"))
    }
  }
}

#' Rend une chaîne « ASCII-safe » : tout caractère hors ASCII devient son
#' échappement `\uXXXX` (ou `\UXXXXXXXX` au-delà du BMP).
#'
#' POURQUOI — défaut mesuré le 2026-09-15, cause racine prouvée octet par octet.
#' `parse()` convertit le texte en encodage NATIF avant de le lire. Sous une
#' locale non-UTF-8 — constaté : `LC_CTYPE=C`, ce que Git Bash exporte via
#' `LC_ALL=C.UTF-8`, nom que R ne reconnaît pas sous Windows — un caractère
#' UTF-8 est remplacé par la chaîne LITTÉRALE « <U+00E9> » (7 octets ASCII) :
#'
#'     eval(parse(text = '"3c. Réseau"'))  ->  "3c. R<U+00E9>seau"
#'
#' Conséquence : toute clé i18n accentuée ou emoji était déclarée ABSENTE de
#' translation.json à tort — 218 fausses erreurs C7 sous `C`, 71 sous
#' `French_France.1252` (les emoji restent hors CP1252), 0 sous `fr_FR.UTF-8`.
#' Le résultat du garde dépendait donc de la locale de l'appelant.
#'
#' En n'envoyant à `parse()` que de l'ASCII, le décodage devient indépendant de
#' la locale. La substitution est sans ambiguïté : un caractère non-ASCII n'est
#' jamais membre d'une séquence d'échappement, celles-ci étant ASCII par
#' construction (`\`, `u`, `U`, chiffres hexadécimaux).
.asciify_non_ascii <- function(s) {
  vapply(s, function(x) {
    if (length(x) == 0L || is.na(x)) return(NA_character_)
    # Chemin rapide : en ASCII pur, octets == caractères. Comparaison
    # indépendante de la locale (nchar(type = "chars") sait compter les
    # caractères d'une chaîne marquée UTF-8 quelle que soit la locale).
    if (nchar(x, type = "bytes") == nchar(x, type = "chars")) return(x)
    cp <- utf8ToInt(enc2utf8(x))
    paste0(vapply(cp, function(c) {
      if (c < 128L) intToUtf8(c)
      else if (c <= 0xFFFFL) sprintf("\\u%04X", c)
      else sprintf("\\U%08X", c)
    }, character(1)), collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

#' Décode les échappements \uXXXX d'une chaîne (base R, sans jsonlite).
#' `s` est un vecteur : la fonction doit rester vectorielle (elle est appelée
#' sur les 2000+ clés d'un coup), d'où le vapply() et le `repeat` par élément.
.unescape_r <- function(s) {
  vapply(s, function(x) {
    if (length(x) == 0 || is.na(x)) return(NA_character_)
    repeat {
      m <- regmatches(x, regexpr("\\\\u[0-9a-fA-F]{4}", x, perl = TRUE))
      if (!length(m)) break
      x <- sub("\\\\u[0-9a-fA-F]{4}", intToUtf8(strtoi(substring(m, 3L), 16L)), x)
    }
    x
  }, character(1), USE.NAMES = FALSE)
}

#' Décode un vecteur de littéraux JSON en un seul `parse()`.
#'
#' Un `eval(parse(text = ...))` par élément coûtait ~4400 appels à `parse()` par
#' exécution (C7), ce qui dominait le temps total. On construit ici
#' `c("a", "b", ...)` et on ne parse qu'une fois. Si un seul littéral est
#' invalide, le parse groupé échoue : on retombe alors sur le décodage élément
#' par élément, ce qui garantit un résultat identique à l'ancienne version.
#'
#' Les littéraux passent d'abord par `.asciify_non_ascii()` : sans cela, la
#' conversion en encodage natif faite par `parse()` corrompt tout caractère
#' non-ASCII hors de la locale courante (voir la note de `.asciify_non_ascii`).
#' Le résultat est ramené en UTF-8 : c'est ce qui rend la comparaison C7
#' indépendante de la locale (`fr_FR.UTF-8` rend du UTF-8, `French_France.1252`
#' du latin1, `C` de l'ASCII — les trois doivent comparer égal).
.decode_json_literals <- function(lits) {
  if (!length(lits)) return(character(0))
  safe <- .asciify_non_ascii(lits)
  vals <- tryCatch(
    eval(parse(text = paste0("c(", paste0(safe, collapse = ", "), ")"))),
    error = function(e) NULL
  )
  if (is.null(vals) || length(vals) != length(lits)) {
    vals <- vapply(safe, function(l) {
      tryCatch(eval(parse(text = l)), error = function(e) NA_character_)
    }, character(1), USE.NAMES = FALSE)
  }
  enc2utf8(as.character(vals))
}

#' Clés i18n : ensemble UTILISÉ par le code vs ensemble DÉFINI dans le JSON.
#'
#' Extrait de `check_c7_i18n_keys()` pour être testable sur une FIXTURE. Le cas
#' négatif — « le garde détecte-t-il encore une clé réellement absente ? » — ne
#' peut pas se vérifier sur le dépôt réel, qui doit rester à 0 signalement : un
#' garde vert dont on n'a jamais vu le rouge ne prouve rien.
#'
#' @return `NULL` si le JSON est absent ou de format inattendu, sinon une liste
#'   `used` (clé -> emplacements `fichier:ligne`), `defined` (clés du JSON) et
#'   `missing` (clés utilisées et non définies).
.i18n_key_sets <- function(json_path, code_files) {
  if (!file.exists(json_path)) return(NULL)
  raw <- paste(readLines(json_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  fr_matches <- regmatches(raw, gregexpr('"fr"\\s*:\\s*"([^"\\\\]|\\\\.)*"', raw, perl = TRUE))[[1]]
  if (!length(fr_matches)) return(NULL)

  # Les littéraux JSON sont décodés par R lui-même (eval/parse, rendu sûr par
  # .asciify_non_ascii) : c'est le seul moyen fiable d'aligner `\"`, `\n`,
  # `\u00e9` côté JSON avec ce que produit réellement tr("...") côté R — sinon
  # toute clé contenant une apostrophe échappée ou un guillemet serait
  # faussement signalée manquante.
  fr_literals <- sub('^"fr"\\s*:\\s*', "", fr_matches, perl = TRUE)
  decoded_all <- .decode_json_literals(fr_literals)
  bad <- is.na(decoded_all)
  if (any(bad)) decoded_all[bad] <- .unescape_r(gsub('^"|"$', "", fr_literals[bad]))
  # enc2utf8 des DEUX côtés : c'est la condition pour que la comparaison ne
  # dépende pas de la locale qui a produit les chaînes.
  fr_keys <- enc2utf8(unique(decoded_all))

  lits <- character(0)
  locs <- character(0)
  for (f in code_files) {
    ann <- .read_code_lines(f)
    if (nrow(ann) == 0) next
    rel_f <- .rel(f)
    for (i in seq_len(nrow(ann))) {
      if (.is_comment_line(ann$raw[i])) next
      m <- regmatches(ann$raw[i],
                      gregexpr('\\b(?:tr|i18n\\$t)\\s*\\(\\s*"([^"\\\\]|\\\\.)*"',
                               ann$raw[i], perl = TRUE))[[1]]
      if (!length(m)) next
      lits <- c(lits, sub('^\\b(?:tr|i18n\\$t)\\s*\\(\\s*', "", m, perl = TRUE))
      locs <- c(locs, rep(sprintf("%s:%d", rel_f, ann$line_no[i]), length(m)))
    }
  }
  if (!length(lits)) {
    return(list(used = character(0), defined = fr_keys, missing = character(0)))
  }
  keys <- enc2utf8(.unescape_r(.decode_json_literals(lits)))
  keep <- !is.na(keys)
  used <- split(locs[keep], keys[keep])
  list(used = used, defined = fr_keys,
       missing = names(used)[!names(used) %in% fr_keys])
}

check_c7_i18n_keys <- function(all_files) {
  json_path <- file.path("i18n", "translation.json")
  if (!file.exists(json_path)) {
    .add("ERROR", "C7", json_path, NA_integer_, "fichier de traduction absent.")
    return(invisible(NULL))
  }
  sets <- .i18n_key_sets(json_path, all_files)
  if (is.null(sets)) {
    .add("ERROR", "C7", json_path, NA_integer_, "aucune entrée {fr, en} trouvée — format inattendu.")
    return(invisible(NULL))
  }
  for (k in sets$missing) {
    .add("ERROR", "C7", .rel(sets$used[[k]][1]), NA_integer_,
         sprintf("clé i18n absente de translation.json : \"%s\" (ajouter via tools/add_i18n_keys.R).",
                 substr(k, 1L, 80L)))
  }
  invisible(length(sets$used))
}

check_c8_contract_tests <- function() {
  contracts <- list.files(file.path("docs", "contracts"), pattern = "\\.md$",
                          full.names = TRUE)
  if (!length(contracts)) return(invisible(NULL))
  tests <- list.files(file.path("tests", "testthat"), pattern = "\\.R$",
                      full.names = TRUE)
  test_blob <- if (length(tests)) {
    tolower(paste(vapply(tests, function(t)
      paste(readLines(t, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
      character(1)), collapse = "\n"))
  } else ""

  for (cpath in contracts) {
    base <- tools::file_path_sans_ext(basename(cpath))
    token <- tolower(sub("_CONTRACT$", "", base))
    if (!nzchar(test_blob) || !grepl(token, test_blob, fixed = TRUE)) {
      .add("WARN", "C8", .rel(cpath), NA_integer_,
           sprintf("contrat `%s` : aucun test ne le mentionne (contrat-first exige code + freeze test + doc).",
                   basename(cpath)))
    }
  }
}

#' Convention de nommage RÉELLE du dépôt : `R/bulk/bulk_gsva.R` se teste dans
#' `tests/testthat/test-bulk-gsva.R` (domaine en préfixe, `_` -> `-`), et non
#' `test-bulk_gsva.R`. Les deux formes sont acceptées pour ne pas pénaliser
#' l'historique.
#'
#' Alias de domaine MESURÉS (39e incrément, §2cy) : le domaine peut être ABRÉGÉ
#' dans le nom du test. `R/plotting/X.R` se teste dans `test-plot-X.R` — 5 des 6
#' fichiers du dossier suivent cette forme, et elle est GELÉE par deux contrats
#' (`PLOT_DATATABLE_CONTRACT.md` et `PLOT_EXPORT_CONTRACT.md` citent nommément
#' `test-plot-datatable.R` et `test-plot-export.R`). La garde ne générait que la
#' forme LITÉRALE `test-plotting-X.R`, inexistante => **4 faux positifs mesurés**
#' (`theme`, `datatable`, `export`, `complex_heatmap`). `plot_dims.R` y échappait
#' seulement parce que sa base contient DÉJÀ `plot` (`test-plot-dims.R` = forme
#' `<tiret>`). ⇒ Renommer les tests étant EXCLU (contrats gelés), on étend la garde.
#' ⚠️ Table VOLONTAIREMENT minimale et explicite : un alias appliqué à TOUS les
#' domaines blanchirait à tort des fichiers réellement sans test — éprouvé par le
#' témoin de traversée de `test-conventions-c9-domain-alias.R`.
.C9_DOMAIN_ALIAS <- c(plotting = "plot")

check_c9_r_tests <- function(r_files) {
  for (f in r_files) {
    base <- tools::file_path_sans_ext(basename(f))
    dashed <- gsub("_", "-", base)
    domain <- basename(dirname(f))
    candidates <- c(
      file.path("tests", "testthat", paste0("test-", base, ".R")),
      file.path("tests", "testthat", paste0("test-", dashed, ".R")),
      file.path("tests", "testthat", paste0("test-", domain, "-", base, ".R")),
      file.path("tests", "testthat", paste0("test-", domain, "-", dashed, ".R"))
    )
    alias <- if (domain %in% names(.C9_DOMAIN_ALIAS)) {
      .C9_DOMAIN_ALIAS[[domain]]
    } else {
      NULL
    }
    if (!is.null(alias)) {
      candidates <- c(
        candidates,
        file.path("tests", "testthat", paste0("test-", alias, "-", base, ".R")),
        file.path("tests", "testthat", paste0("test-", alias, "-", dashed, ".R"))
      )
    }
    if (!any(file.exists(candidates))) {
      .add("WARN", "C9", .rel(f), NA_integer_,
           sprintf("aucun test éponyme (%s) — règle 5 : tout fichier de R/ est livré avec ses tests.",
                   paste(basename(candidates), collapse = " ou ")))
    }
  }
}

#' C9b (43e increment, 2026-09-20) — forme B : les fonctions POSSEDEES d'un
#' fichier sont-elles MENTIONNEES par son test eponyme ?
#'
#' POURQUOI — C9 ne verifie que l'EXISTENCE d'un fichier au bon nom. Elle est
#' donc satisfaite par un NOM : `R/sc/sc_pipeline.R` a un `test-sc-pipeline.R`
#' qui `source()` le fichier et n'appelle JAMAIS sa seule fonction (§2ck.1). C9
#' rend « vert » sur un fichier que rien n'exerce. C9b est **la seule regle du
#' depot qui mesure l'APPEL**, et c'est tout son objet (§14.2 de
#' `docs/CONVENTIONS.md`).
#'
#' ---------------------------------------------------------------------------
#' TROIS DECISIONS, CHACUNE IMPOSEE PAR UNE MESURE (jamais par une preference)
#' ---------------------------------------------------------------------------
#' 1. DEUX FILTRES SANS LESQUELS LA REGLE EST FAUSSE (§14.2, mesures du
#'    2026-09-19 et 2026-09-20) :
#'    - ne compter que les fonctions **POSSEDEES** (definies dans UN SEUL
#'      fichier de `R/`). Le depot partage **21** noms (`%||%`, `.tr`, `add`,
#'      `add_log`, `log`, ...) : sans ce filtre, tout fichier qui definit
#'      `%||%` est declare « exerce » et **la regle ne peut plus echouer** —
#'      defaut du TOKEN juge a la place de l'UNITE (§12.1), deja corrige QUATRE
#'      fois dans ce depot.
#'    - **EXEMPTER** les fichiers qui ne possedent **aucune** fonction
#'      (`R/sc/sc_state.R` : ses 3 noms sont partages). Le critere y est
#'      INAPPLICABLE ; le signaler produirait un faux positif permanent, non
#'      corrigeable sans polluer le fichier.
#' 2. NON-SUPERPOSITION A C9. Une premiere version signalait **6** fichiers,
#'    dont **5 deja** signales par C9 : c'etait **5 doublons**, pas 5
#'    informations. C9b ne signale donc QUE « **test eponyme PRESENT, code
#'    jamais mentionne** » — c'est-a-dire le seul defaut que C9 ne peut pas
#'    voir. Cout mesure apres filtre : **37** fichiers et **174** fonctions
#'    orphelines (mesure du 2026-09-20, apres correction — voir 4.).
#' 3. SENS DE « APPELE » = application `f(...)` OU **MENTION** comme mot
#'    entier (`get("f")`, `` `f` ``, `do.call`). ⚠️ Ce choix est **MESURE** dans
#'    le depot, pas deduit : `test-plot-export.R` affirme l'exclusion de
#'    `R/sc/sc_export.R` par `expect_match(code, "ggsave\\(")` — une MENTION,
#'    pas un appel. Un sens STRICT laisserait C9b AVEUGLE sur une convention
#'    que le depot pratique depuis PLOT-S2. La definition retenue est l'UNION
#'    des deux : plus PERMISSIVE, donc conservatrice (elle ne peut que reduire
#'    les faux positifs). ⚠️ `R/sc/sc_export.R` reste signale **malgre** elle :
#'    le test exclut le fichier par un motif regex sur `ggsave(`, jamais sur un
#'    nom de fonction possede — l'exemption est une decision documentee, pas un
#'    effet de bord de la mesure. C'est un **faux positif connu**, epingle par
#'    `test-conventions-c9b-owned.R`.
#' 4. 🔴 LA PREMISSE DE §14.2 ETAIT FAUSSE, ET C'EST LA MESURE QUI L'A DITE.
#'    §14.2 annoncait « cout mesure aujourd'hui : **0 signalement** » et en
#'    deduisait un **invariant prospectif**, donc `ERROR` (un invariant a cout
#'    nul doit bloquer, comme C16). **Cette premisse repose sur une sonde
#'    FAUSSE** : son motif d'identifiant (`[.A-Za-z][.A-Za-z0-9_.]*`) ne
#'    pouvait pas capturer un nom commencant par un POINT — les **195**
#'    fonctions privees (`.*`) du depot etaient donc INVISIBLES a la sonde, et
#'    le « compteur » de 4 trous ne comptait que des fonctions PUBLIQUES.
#'    Re-mesure avec la detection du garde : **37** fichiers / **174**
#'    fonctions orphelines, dont **176 privees sur 272** au total du depot
#'    (36 % des fonctions possedees ne sont citees par AUCUN test).
#'    ⇒ La conclusion « invariant prospectif » tombe, et avec elle la
#'    justification de `ERROR`. C9b est un **COMPTEUR DE DETTE** comme C9 et
#'    C10 : sa severite est `WARN`. ⚠️ Ce n'est PAS un renoncement — c'est le
#'    meme raisonnement que §14.3 applique **dans l'autre sens** : C16 a ete
#'    promu parce qu'il mesurait **0** site ; C9b est laisse en avertissement
#'    parce qu'il en mesure **174**. Une regle dont le compteur depend de la
#'    mesure, jamais de l'intention.
check_c9b_owned_functions_mentioned <- function(r_files) {
  # --- 1. Fonctions definies par fichier, et leur degre de POSSESSION --------
  defs <- list()
  for (f in r_files) {
    ann <- .read_code_lines(f)
    code <- if (nrow(ann) == 0L) character(0) else ann$code
    defs[[f]] <- .collect_function_names(code)
  }
  universe <- unique(unlist(defs, use.names = FALSE))
  if (!length(universe)) return(invisible(NULL))
  holders <- vapply(universe, function(nm) {
    sum(vapply(defs, function(v) nm %in% v, logical(1)))
  }, integer(1))

  # --- 2. Univers de TOKENS cites par les tests -----------------------------
  t_files <- list.files(file.path("tests", "testthat"),
                        pattern = "[.]R$", full.names = TRUE)
  test_tokens <- character(0)
  for (t in t_files) {
    ann <- .read_code_lines(t)
    code <- if (nrow(ann) == 0L) character(0) else ann$code
    test_tokens <- c(test_tokens, .mention_tokens(code))
  }
  test_tokens <- unique(test_tokens)

  # --- 3. Le fichier a-t-il un test EPONYME ? (criteres de C9, NON DUPLIQUES)
  dir_tests <- file.path("tests", "testthat")

  for (f in r_files) {
    owned <- defs[[f]][holders[defs[[f]]] == 1L]
    # (1) critere INAPPLICABLE : aucune fonction possedee => hors population.
    if (!length(owned)) next

    # (2) NON-SUPERPOSITION : si C9 signale deja le fichier, on se tait.
    base   <- tools::file_path_sans_ext(basename(f))
    dashed <- gsub("_", "-", base)
    domain <- basename(dirname(f))
    cands  <- c(
      file.path(dir_tests, paste0("test-", base, ".R")),
      file.path(dir_tests, paste0("test-", dashed, ".R")),
      file.path(dir_tests, paste0("test-", domain, "-", base, ".R")),
      file.path(dir_tests, paste0("test-", domain, "-", dashed, ".R"))
    )
    alias <- if (domain %in% names(.C9_DOMAIN_ALIAS)) {
      .C9_DOMAIN_ALIAS[[domain]]
    } else {
      NULL
    }
    if (!is.null(alias)) {
      cands <- c(cands,
                 file.path(dir_tests, paste0("test-", alias, "-", base, ".R")),
                 file.path(dir_tests,
                           paste0("test-", alias, "-", dashed, ".R")))
    }
    if (!any(file.exists(cands))) next

    # (3) Le VRAI critere : au moins une fonction possedee est-elle citee ?
    missing <- setdiff(owned, test_tokens)
    if (length(missing)) {
      # WARN, pas ERROR : 37 fichiers / 174 fonctions (cf. en-tete, point 4).
      .add("WARN", "C9b", .rel(f), NA_integer_,
           sprintf(paste0("test eponyme PRESENT mais %d/%d fonction(s) possedee(s) ",
                          "jamais mentionnee(s) : %s -> C9 est satisfaite par le NOM, ",
                          "pas par la couverture."),
                   length(missing), length(owned),
                   paste(utils::head(sort(missing), 6L), collapse = ", ")))
    }
  }
}

#' Noms des fonctions definies dans un fichier (motif maison `<nom> <- function`).
#'
#' ⚠️ On lit le code ANNOTE (`.read_code_lines()`), pas le texte brut : un
#' `f <- function` cite dans une CHAINE (cas frequent ici — les fichiers du
#' depot embarquent des scripts R generes, cf. `R/sc/sc_export.R`) ferait de
#' `f` une fausse fonction possedee. Meme classe de defaut que C6 (§2bg) et C10.
#'
#' ⚠️ DEUX PIEGES MESURES le 2026-09-20, chacun paye par une 1re version FAUSSE
#' qui signalait **37** fichiers au lieu de **5** :
#'   - un ARGUMENT NOMME dont l'expression `function` tombe en debut de ligne
#'     (`}, error = function(e) ...`, `cell_fun = function(...)`) est
#'     indistinguable d'une definition par une regex LIGNE A LIGNE. Mesure :
#'     `error`, `cell_fun` (2 faux), plus `create_nested_histograms` et
#'     `render_communication_sunburst` (formes multi-lignes equivalentes)
#'     entraient dans la population a couvrir. Remede ADOPTE : la forme `=`
#'     n'est acceptee qu'a **indentation NULLE** (`^[A-Za-z]`), qui est la
#'     convention de TOUT le depot pour une definition (`<-` y compris). Le
#'     filtre est DECLARE : il peut manquer une definition indentee ecrite avec
#'     `=` (aucune mesuree dans `R/`) — defaut dans le sens CONSERVATEUR, il
#'     ferait SIGNALER a tort, jamais taire ;
#'   - les mots RESERVES qui satisfont `[A-Za-z]+ *= *function` (`NA`, `NULL`,
#'     `TRUE`, `FALSE`, `Inf`, ...) sont ecartes explicitement. Mesure : `NA`
#'     etait rendu comme « fonction possedee » dans **16** fichiers.
.collect_function_names <- function(code) {
  if (!length(code)) return(character(0))
  get1 <- function(pat, ln) {
    m <- regexpr(pat, ln, perl = TRUE)
    if (m < 0L) return(NULL)
    gsub("`", "", trimws(sub("\\s*(<-|=)\\s*function.*$", "",
                             regmatches(ln, m))), fixed = TRUE)
  }
  # Forme `<-` : n'importe ou (aucun argument nomme ne s'ecrit `<-`), nom
  # precede d'un debut d'instruction (whitespace seulement).
  # Forme `=` : INDENTATION NULLE uniquement (definition top-level).
  pat_arrow <- "^\\s*(`[^`]+`|[.A-Za-z]([.A-Za-z0-9_]|\\.(?![.A-Za-z0-9_]))*)\\s*<-\\s*function"
  pat_equal <- "^(`[^`]+`|[.A-Za-z]([.A-Za-z0-9_]|\\.(?![.A-Za-z0-9_]))*)\\s*=\\s*function"
  nm <- c(
    vapply(code, function(ln) get1(pat_arrow, ln) %||% NA_character_, character(1)),
    vapply(code, function(ln) get1(pat_equal, ln) %||% NA_character_, character(1))
  )
  nm <- unique(nm[!is.na(nm) & nzchar(nm)])
  # Mots reserves : ils ne peuvent pas nommer une fonction.
  nm[!nm %in% c("NA", "NULL", "TRUE", "FALSE", "Inf", "NaN", "if", "else",
                "for", "while", "repeat", "function", "return")]
}

#' Tokens cites par un fichier de test : identifiants + operateurs infixes.
#'
#' Deux regles de precision, chacune payee par un faux positif possible :
#'   - un POINT n'est admis dans un identifiant que s'il n'est PAS suivi d'un
#'     caractere de mot : sinon on collecte aussi les methodes S3 `fun.classe`,
#'     qui ne sont pas des fonctions possedees.
#'   - les operateurs infixes declares entre backticks (`` `%||%` ``) sont
#'     ajoutes, sans les quotes : `.collect_function_names()` rend le nom NU.
.mention_tokens <- function(code) {
  if (!length(code) || !any(nzchar(code))) return(character(0))
  blob <- paste(code, collapse = "\n")
  ids <- unlist(regmatches(blob, gregexpr(
    "[.A-Za-z]([.A-Za-z0-9_]|\\.(?![.A-Za-z0-9_]))*", blob, perl = TRUE)))
  ops <- unlist(regmatches(blob,
                           gregexpr("`%[^`%]{1,10}%`", blob, perl = TRUE)))
  unique(c(ids, gsub("`", "", ops, fixed = TRUE)))
}

#' Noms des constructeurs d'erreur CLASSÉE définis dans le projet.
#'
#' POURQUOI — le motif maison est un constructeur local :
#'     .bulk_multi_stop <- function(msg, state) {
#'       errorCondition(msg, class = "bulk_multi_error", state = state)
#'     }
#' appelé par `stop(.bulk_multi_stop(...))`. Le site est donc BEL ET BIEN
#' classé, mais le token `errorCondition` n'apparaît PAS dans l'étendue du
#' `stop()` : une garde qui ne regarde que cette étendue le déclare « non
#' classé ». C'est le même défaut de classe que C6 et C10 (§12.1, §12.3) — la
#' garde juge un TOKEN au lieu de RÉSOUDRE l'appel.
#'
#' RÈGLE RETENUE (stricte, et MESURÉE) : constructeur = fonction dont le corps
#' est UNE SEULE expression `errorCondition(...)`. La règle large (« le corps
#' cite errorCondition ») est trop permissive : elle exempterait
#' `stop(validateur(x))` où `validateur` lève une erreur classée PARMI d'autres
#' vérifications, donc où `stop()` peut recevoir une simple chaîne. Mesure du
#' 2026-09-17 : règle large = 61 noms, règle stricte = 3 noms, et le MÊME
#' verdict sur les 46 sites concernés — la règle stricte ne perd rien.
#'
#' Le SECOND idiome maison — `stop(errorCondition(...))` DANS le helper, p. ex.
#' `.bulk_gs_error` — n'a pas besoin d'exemption : le site appelant n'écrit
#' aucun `stop()`, donc C10 ne le voit jamais.
.collect_classed_error_ctors <- function(r_files) {
  ctor <- character(0)
  for (f in r_files) {
    ann <- .read_code_lines(f)
    if (nrow(ann) == 0) next
    code <- ann$code
    n <- length(code)
    for (i in seq_len(n)) {
      m <- regmatches(code[i],
            regexec("^[ \t]*([.A-Za-z0-9_]+)[ \t]*<-[ \t]*function", code[i]))
      if (!length(m[[1]])) next
      depth <- 0L
      started <- FALSE
      j <- i
      body <- character(0)
      while (j <= n) {
        depth <- depth + .count_chars(code[j], "{") - .count_chars(code[j], "}")
        if (.count_chars(code[j], "{") > 0L) started <- TRUE
        body <- c(body, code[j])
        if (started && depth <= 0L) break
        j <- j + 1L
      }
      if (!started) next
      txt <- paste(body, collapse = " ")
      txt <- sub("^[^{]*\\{", "", txt)     # signature + accolade ouvrante
      txt <- sub("\\}[ \t]*$", "", txt)    # accolade fermante
      txt <- gsub("[[:space:]]", "", txt)
      if (startsWith(txt, "errorCondition(") && endsWith(txt, ")")) {
        ctor <- c(ctor, m[[1]][[2L]])
      }
    }
  }
  unique(ctor)
}

#' L'étendue d'un `stop()` appelle-t-elle un constructeur d'erreur classée ?
.span_calls_classed_ctor <- function(span, ctors) {
  if (!length(ctors)) return(FALSE)
  for (nm in ctors) {
    pat <- paste0("(^|[^A-Za-z0-9_.])", gsub("\\.", "\\\\.", nm), "[ \t]*\\(")
    if (grepl(pat, span, perl = TRUE)) return(TRUE)
  }
  FALSE
}

#' Aplatit le code d'un fichier en UNE chaîne, avec le décalage de chaque ligne.
#'
#' POURQUOI APLATIR. Pour décider si un `stop(x)` porte sur un FORMEL de la
#' fonction englobante, il faut suivre des appariements d'accolades À CHEVAL sur
#' les lignes. Un compteur ligne par ligne s'y fait piéger par une ligne qui
#' porte une FERMETURE avant son ouverture — `}, error = function(e) {` : la
#' profondeur y retombe à zéro, le corps se réduit à cette seule ligne, et la
#' fonction englobante n'est plus trouvée. Mesuré le 2026-09-17 : ce défaut
#' faisait répondre « pas de fonction englobante » sur 2 des 4 sites réels. En
#' aplatissant, on raisonne en POSITIONS et l'appariement devient exact.
.flatten_code <- function(code) {
  n <- length(code)
  if (n == 0L) return(list(flat = "", off = integer(0)))
  list(
    flat = paste(code, collapse = "\n"),
    off  = cumsum(c(1L, nchar(code)[-n] + 1L))
  )
}

#' Toutes les définitions de fonction d'un fichier : étendue du CORPS et formels.
#'
#' L'étendue du corps est cherchée APRÈS la parenthèse fermante de la signature,
#' jamais avant : c'est ce qui empêche de compter une accolade appartenant à
#' l'expression PRÉCÉDENTE (`}, error = function(e) {`).
.collect_function_defs <- function(code) {
  fl   <- .flatten_code(code)
  flat <- fl$flat
  if (!nzchar(flat)) return(list())
  chars <- strsplit(flat, "", fixed = TRUE)[[1]]
  nch   <- length(chars)
  m     <- gregexpr("function[ \t]*\\(", flat, perl = TRUE)[[1]]
  if (m[1] == -1L) return(list())
  lens <- attr(m, "match.length")
  out  <- list()
  for (k in seq_along(m)) {
    op <- m[k] + lens[k] - 1L            # position de la '(' de la signature
    d <- 0L; q <- op; cp <- NA_integer_
    while (q <= nch) {
      if (chars[q] == "(") d <- d + 1L
      else if (chars[q] == ")") { d <- d - 1L; if (d == 0L) { cp <- q; break } }
      q <- q + 1L
    }
    if (is.na(cp)) next
    forms <- trimws(strsplit(substr(flat, op + 1L, cp - 1L), ",", fixed = TRUE)[[1]])
    forms <- trimws(sub("=.*$", "", forms))
    forms <- forms[nzchar(forms) & forms != "..."]
    r <- cp + 1L
    while (r <= nch && (chars[r] == " " || chars[r] == "\t")) r <- r + 1L
    if (r <= nch && chars[r] == "{") {
      d2 <- 0L; s2 <- r; e2 <- NA_integer_
      while (s2 <= nch) {
        if (chars[s2] == "{") d2 <- d2 + 1L
        else if (chars[s2] == "}") { d2 <- d2 - 1L; if (d2 == 0L) { e2 <- s2; break } }
        s2 <- s2 + 1L
      }
      if (is.na(e2)) e2 <- nch
    } else {
      nl <- regexpr("\n", substr(flat, r, nch), fixed = TRUE)[1]
      e2 <- if (nl < 0L) nch else r + nl - 2L      # corps sans accolades
    }
    out[[length(out) + 1L]] <- list(from = r, to = e2, forms = forms)
  }
  out
}

#' Les formels de TOUTES les fonctions englobant la position `p`.
#'
#' On prend toutes les englobantes, pas seulement la plus interne : dans
#' `function(e) lapply(x, function(y) stop(e))`, le symbole vient toujours de
#' l'APPELANT, donc sa classe se juge à l'origine de la condition.
.enclosing_formals <- function(defs, off, p) {
  if (!length(defs) || !length(off)) return(character(0))
  forms <- character(0)
  for (d in defs) if (d$from <= p && p <= d$to) forms <- c(forms, d$forms)
  unique(forms)
}

#' Texte du PREMIER argument d'un appel `stop(...)`.
.stop_first_arg <- function(span) {
  m <- regexpr("stop[ \t]*\\(", span, perl = TRUE)
  if (m < 0L) return("")
  txt <- substr(span, m + attr(m, "match.length"), nchar(span))
  ch  <- strsplit(txt, "", fixed = TRUE)[[1]]
  d <- 0L
  for (q in seq_along(ch)) {
    if (ch[q] == "(" || ch[q] == "[") d <- d + 1L
    else if (ch[q] == ")" || ch[q] == "]") {
      d <- d - 1L
      if (d < 0L) return(substr(txt, 1L, q - 1L))
    } else if (ch[q] == "," && d == 0L) {
      return(substr(txt, 1L, q - 1L))
    }
  }
  txt
}

#' `x` est-il un symbole nu (ni littéral de chaîne, ni appel) ?
.is_bare_symbol <- function(x) grepl("^[.A-Za-z][.A-Za-z0-9_]*$", trimws(x))

#' C10 — `stop()` doit être classé (`errorCondition(class=...)`) ou porter
#' `call. = FALSE`.
#'
#' DÉFAUT MESURÉ le 2026-09-17 : la version précédente jugeait **UNE SEULE
#' ligne**. Un appel MULTI-LIGNES parfaitement conforme —
#'     stop("message",
#'          "suite", call. = FALSE)
#' — était donc signalé à tort, parce que `call. = FALSE` vit sur la ligne
#' suivante. Mesure : sur **270** signalements, **49 (18 %)** portaient déjà
#' `call. = FALSE` ou `errorCondition` DANS l'appel ⇒ faux positifs, et la
#' dette paraissait plus grosse qu'elle n'est.
#' On analyse désormais l'APPEL LOGIQUE : de la ligne de `stop(` jusqu'à
#' l'équilibrage des parenthèses. `ann$code` est déjà débarrassé des chaînes et
#' des commentaires, donc compter les parenthèses est fiable.
#'
#' SECOND DÉFAUT MESURÉ le 2026-09-17 : analyser l'étendue du `stop()` ne suffit
#' pas. Un site routé par un constructeur local classé —
#' `stop(.bulk_multi_stop(...))` — est CLASSÉ, mais ne contient aucun token
#' `errorCondition`. 46 des 221 signalements étaient de ce type (18
#' `.bulk_multi_stop`, 15 `.sc_multi_stop`, 13 `.bulk_multi_compare_stop`), et
#' les trois fichiers concernés étaient précisément ceux que la mesure
#' précédente croyait « déjà classés ». La règle résout donc les constructeurs
#' du projet via `.collect_classed_error_ctors()`.
#' TROISIÈME DÉFAUT MESURÉ le 2026-09-17 : un `stop()` de RE-LEVÉ n'a pas de
#' classe à recevoir. `stop(e)` dans `error = function(e) { ... }` re-signale
#' une condition QUI EXISTE DÉJÀ ; sa classe se juge à son ORIGINE, pas ici.
#' 4 des 175 signalements étaient de ce type. La règle retenue est étroite et
#' mesurée : le site est exempté SI ET SEULEMENT SI l'argument est un SYMBOLE NU
#' qui est un FORMEL d'une fonction englobante. Un symbole LOCAL
#' (`msg <- "x"; stop(msg)`) fabrique la valeur DANS la fonction : il reste
#' signalé. Mesure des 175 : 145 littéraux, 26 appels (`sprintf`, `paste0`,
#' `tr`) — tous de vrais `stop()` de message — et 4 symboles nus, tous des
#' re-levés portant sur `e`. Après correctif : C10 = 171.
#' COÛT MAÎTRISÉ : l'analyse des définitions de fonction APLATIT le fichier ;
#' elle n'est donc déclenchée que si un symbole nu est en jeu ET que les
#' exemptions bon marché ont échoué (4 fois sur 70 fichiers).
#' INVARIANTE CONSERVÉE : un appel multi-lignes SANS classement reste signalé,
#' ET un `stop()` routé par une fonction qui n'est PAS un constructeur classé
#' reste signalé (les deux sont testés sur cas négatif injecté) — sinon la
#' garde deviendrait aveugle en croyant se réparer (cf. CONVENTIONS.md §12.1).
check_c10_error_style <- function(r_files) {
  ctors <- .collect_classed_error_ctors(r_files)
  for (f in r_files) {
    ann <- .read_code_lines(f)
    if (nrow(ann) == 0) next
    code <- ann$code
    n <- length(code)
    defs <- NULL     # paresseux — voir « COÛT MAÎTRISÉ » ci-dessus
    fl   <- NULL
    i <- 1L
    while (i <= n) {
      ln <- code[i]
      if (!grepl("(^|[^A-Za-z0-9_.])stop\\s*\\(", ln, perl = TRUE)) {
        i <- i + 1L
        next
      }
      # étendue de l'APPEL : jusqu'à équilibrage des parenthèses
      depth <- 0L
      j <- i
      while (j <= n) {
        depth <- depth + .count_chars(code[j], "(") - .count_chars(code[j], ")")
        if (depth <= 0L) break
        j <- j + 1L
      }
      span <- paste(code[i:min(j, n)], collapse = "\n")
      exempt <- grepl("errorCondition", span, fixed = TRUE) ||      # forme maison
        grepl("call\\.\\s*=\\s*FALSE", span, perl = TRUE) ||        # forme explicite
        grepl("^\\s*stop\\(\\)", ln, perl = TRUE) ||                # stop() nu
        .span_calls_classed_ctor(span, ctors)                       # constructeur local
      if (!exempt) {
        arg1 <- trimws(.stop_first_arg(span))
        if (.is_bare_symbol(arg1)) {
          if (is.null(defs)) {
            fl   <- .flatten_code(code)
            defs <- .collect_function_defs(code)
          }
          mp <- regexpr("stop[ \t]*\\(", ln, perl = TRUE)
          if (mp > 0L) {
            exempt <- arg1 %in% .enclosing_formals(defs, fl$off, fl$off[i] + mp - 1L)
          }
        }
      }
      if (!exempt) {
        .add("WARN", "C10", .rel(f), ann$line_no[i],
             "stop() sans errorCondition(class=<domaine>_error) ni call. = FALSE (dette héritée ; obligatoire pour tout code neuf).")
      }
      i <- i + 1L
    }
  }
}

check_c11_parallel <- function(files) {
  patterns <- c(
    "enableWGCNAThreads" = "WGCNA::enableWGCNAThreads() est interdit sous Windows (règle transverse).",
    "mclapply"           = "mclapply() = fork, non fiable sous Windows — utiliser mirai (règle 8).",
    "makeCluster"        = "makeCluster() = second framework parallèle — un seul pool : mirai (règle 8).",
    "MulticoreParam"     = "BiocParallel::MulticoreParam : autorisé UNIQUEMENT sous Unix (SerialParam sous Windows) — vérifier le garde-fou."
  )
  for (f in files) {
    ann <- .read_code_lines(f)
    if (nrow(ann) == 0) next
    for (p in names(patterns)) {
      hits <- which(grepl(p, ann$code, fixed = TRUE))
      for (i in hits) {
        .add("WARN", "C11", .rel(f), ann$line_no[i], patterns[[p]])
      }
    }
  }
}

check_c12_headers <- function(r_files) {
  for (f in r_files) {
    first <- tryCatch(readLines(f, n = 3L, warn = FALSE, encoding = "UTF-8"),
                      error = function(e) character(0))
    if (!length(first) || !any(grepl("^\\s*#", first))) {
      .add("WARN", "C12", .rel(f), NA_integer_,
           "pas d'en-tête commenté (rôle du fichier, chantier d'origine, garde-fous).")
    }
  }
}

# =============================================================================
# C13 — `choices` nommé : la valeur ne doit jamais être un appel traduit
# =============================================================================
#' Appels de traduction reconnus. `.tr_plain` est placé AVANT `.tr` : l'alternance
#' est ordonnée, et `.tr` matcherait le préfixe de `.tr_plain` avant d'échouer sur
#' `\s*\(` (le `_` suit) sans jamais revenir sur la branche correcte.
.C13_TRANSLATED_CALL <- "\\.tr_plain|\\.tr|tr_plain|i18n\\$t|tr"

#' Le motif `"nom" = <appel traduit>` est-il présent sur cette ligne de CODE ?
#'
#' On travaille sur `ann$code` — chaînes déjà remplacées par `""` — et non sur le
#' texte brut : le nom devient `""`, ce qui suffit à reconnaître la FORME sans
#' dépendre du libellé, et surtout sans lire un seul caractère non-ASCII, donc
#' sans dépendre de la locale (même exigence que C7).
.c13_line_hit <- function(code_line) {
  grepl(sprintf('"\\s*=\\s*(%s)\\s*\\(', .C13_TRANSLATED_CALL),
        code_line, perl = TRUE)
}

#' Numéros de ligne en infraction dans un fichier (entiers ; vide = conforme).
#'
#' Extrait de `check_c13_choices_named_values()` pour être testable sur une
#' FIXTURE — un garde vert dont on n'a jamais vu le rouge ne prouve rien :
#' voir tests/testthat/test-conventions-c13-choices.R.
#'
#' Périmètre : uniquement l'intérieur d'un `choices = c(` (équilibre des
#' parenthèses suivi depuis le `c(`). Un vecteur nommé ailleurs —
#' `c("EN" = .tr("Hello"))` — est légitime : le nom y est une clé stable et la
#' valeur est le texte affiché. La règle ne vise que le contrat de `choices`.
.c13_find_hits <- function(path) {
  ann <- .read_code_lines(path)
  if (nrow(ann) == 0L) return(integer(0))
  hits <- integer(0)
  depth <- 0L
  for (i in seq_len(nrow(ann))) {
    ln <- ann$code[i]
    m <- regexpr("choices\\s*=\\s*c\\s*\\(", ln, perl = TRUE)
    if (m > 0L) {
      # Entrée dans un `choices = c(` : le libellé peut contenir des
      # parenthèses, mais elles sont parties avec la chaîne.
      depth <- max(0L, .paren_balance(substring(ln, m)))
    } else if (depth > 0L) {
      depth <- max(0L, depth + .paren_balance(ln))
    } else {
      next
    }
    if (.c13_line_hit(ln)) hits <- c(hits, ann$line_no[i])
  }
  hits
}

check_c13_choices_named_values <- function(files) {
  for (f in files) {
    for (i in .c13_find_hits(f)) {
      .add("ERROR", "C13", .rel(f), i,
           paste0("`choices` nommé : le libellé traduit est du côté VALEUR — ",
                  "Shiny AFFICHE le nom et RENVOIE la valeur, donc `input$` ",
                  "recevra le texte traduit au lieu de la valeur attendue. ",
                  "Écrire setNames(c(\"valeur\"), c(.tr(\"libellé\")))."))
    }
  }
}

#' Découpe les arguments de PREMIER NIVEAU d'un texte d'appel (l'intérieur d'une
#' paire de parenthèses). Les virgules IMBRIQUÉES — appels internes, crochets —
#' sont ignorées : `paste0("a", "b"), class = "x"` donne DEUX arguments, pas
#' trois. Utilisé par C16.
.split_top_level_args <- function(inner) {
  ch <- strsplit(inner, "", fixed = TRUE)[[1]]
  depth <- 0L
  args  <- character(0)
  cur   <- character(0)
  for (c in ch) {
    if (c == "(" || c == "[") depth <- depth + 1L
    else if (c == ")" || c == "]") depth <- depth - 1L
    if (c == "," && depth == 0L) {
      args <- c(args, paste(cur, collapse = ""))
      cur  <- character(0)
    } else {
      cur <- c(cur, c)
    }
  }
  c(args, paste(cur, collapse = ""))
}

#' C16 (ajout 2026-09-17) — `errorCondition()` doit n'avoir qu'UN argument
#' POSITIONNEL : c'est le message.
#'
#' RÉGRESSION MESURÉE (commit `c40121f`, corrigé par `ce6691a`). `stop()` CONCATÈNE
#' ses arguments ; `errorCondition(message, ...)` NON — les suivants deviennent des
#' CHAMPS de la condition, pas du message :
#'
#'     stop("A : ", "B", " fin")                   -> "A : B fin"
#'     errorCondition("A : ", "B", " fin", class=)  -> "A : "   <-- TRONQUÉ
#'
#' Envelopper un `stop()` MULTI-ARGUMENTS dans `errorCondition()` tronque donc le
#' message EN SILENCE — les tests qui matchent sur le message passent encore, et
#' l'invariant de conversion « texte source identique » ne voit rien. 3 des 19
#' sites convertis étaient touchés. Forme correcte : envelopper dans `paste0()`.
#'
#' Les arguments NOMMÉS (`class=`, `state=`, `call.=`) ne comptent pas : seule la
#' pluralité d'arguments POSITIONNELS est le défaut.
#'
#' SEVERITE : `ERREUR`, pas `AVERT.` (§14.3, code au 40e increment). C16 ne detecte
#' pas une preference de style mais une TRONCATURE SILENCIEUSE : l'application
#' tourne, le diagnostic est ampute, et la troncature est INVISIBLE au niveau
#' source. Les autres `AVERT.` sont des COMPTEURS DE DETTE (code herite a
#' migrer) ; C16 est un INVARIANT a 0 site. Un invariant a cout nul doit etre
#' bloquant.
#'
#' ATTENTION : la severite se regle ICI (le CANAL de `.add()`), PAS dans la table
#' `lvl`. Le blocage vaut `blocking <- n_err + (if (strict) n_warn else 0L)` : il
#' ne lit que les canaux. Corriger `lvl` seul produirait un MENSONGE COSMIQUE
#' (affiche `ERREUR`, ne bloque pas) - ce que verrouille
#' `test-conventions-c16-arity.R`.
check_c16_errorcondition_arity <- function(files) {
  for (f in files) {
    ann <- .read_code_lines(f)
    if (nrow(ann) == 0L) next
    code <- ann$code
    if (!any(grepl("errorCondition", code, fixed = TRUE))) next
    n <- length(code)
    for (i in seq_len(n)) {
      m <- regexpr("errorCondition\\s*\\(", code[i], perl = TRUE)
      if (m < 0L) next
      # étendue de l'APPEL : jusqu'à équilibrage des parenthèses
      depth <- 0L
      j <- i
      while (j <= n) {
        depth <- depth + .count_chars(code[j], "(") - .count_chars(code[j], ")")
        if (depth <= 0L) break
        j <- j + 1L
      }
      span <- paste(code[i:min(j, n)], collapse = "\n")
      k <- m + attr(m, "match.length") - 1L          # position de la '('
      ch <- strsplit(span, "", fixed = TRUE)[[1]]
      d <- 0L
      e <- NA_integer_
      for (q in k:length(ch)) {
        if (ch[q] == "(") d <- d + 1L
        else if (ch[q] == ")") {
          d <- d - 1L
          if (d == 0L) { e <- q; break }
        }
      }
      if (is.na(e)) next
      args  <- .split_top_level_args(paste(ch[(k + 1L):(e - 1L)], collapse = ""))
      named <- grepl("^\\s*[A-Za-z_.][A-Za-z0-9_.]*\\s*=(?!=)", args, perl = TRUE)
      n_pos <- sum(!named)
      if (n_pos > 1L) {
        .add("ERROR", "C16", .rel(f), ann$line_no[i],
             sprintf(paste0("errorCondition() a %d arguments POSITIONNELS : seul ",
                            "le 1er devient le message (`stop()` concatene, ",
                            "`errorCondition()` NON) -> envelopper dans paste0()."),
                     n_pos))
      }
    }
  }
}

# ---------------------------------------------------------------------------
# Rapport final
# ---------------------------------------------------------------------------
run_check <- function(strict = FALSE, use_git = TRUE, list_all = FALSE) {
  r_files  <- .collect_files("R")
  m_files  <- .collect_files("modules")
  all_code <- c(r_files, m_files, "app.R", "global.R")
  all_code <- all_code[file.exists(all_code)]

  cat(sprintf("[check_conventions] %d fichier(s) R/ — %d fichier(s) modules/ — git: %s\n",
              length(r_files), length(m_files), if (use_git) "oui" else "non"))

  check_c1_r_modules()
  check_c2_shiny_in_r(r_files)
  check_c3_source_targets(use_git)
  check_c4_setwd(c(r_files, m_files))
  check_c5_browser(all_code)
  check_c6_library_in_r(r_files)
  check_c7_i18n_keys(all_code)
  check_c8_contract_tests()
  check_c9_r_tests(r_files)
  # C9b (43e increment) — meme POPULATION que C9 (`r_files` seul), decision
  # explicitement mesuree : `modules/` est hors sujet (C9 n'y est pas applique,
  # §2bs.6) et `tests/` n'est pas du code de production. Passer une autre liste
  # ferait de C9b une regle SUPERPOSEE, ce que §14.2 a refuse.
  check_c9b_owned_functions_mentioned(r_files)
  # ⚠️ PORTÉE : `R/` **et** `modules/`, pas `R/` seul. Jusqu'au 2026-09-17 la
  # règle ne recevait que `r_files` : elle sous-mesurait la dette de 37,5 %
  # (100 sites mesurés sur 160 réels — 48 invisibles dans `modules/`, 12 dans
  # `tests/`). L'angle mort couvrait la couche la PLUS VISIBLE de
  # l'application : `mod_import_bulk.R` (10), `mod_geo.R` (8),
  # `mod_import_sc.R` (6), `mod_sc_pseudobulk.R` (6). Indice que c'était un
  # oubli et non une décision : C16 — règle sœur, sur `errorCondition()`, donc
  # le même sujet — recevait déjà `c(r_files, m_files)`. On aligne C10 sur C16.
  #
  # `tests/` reste EXCLU, volontairement : les `stop()` de fixtures ne sont pas
  # du code de production (12 sites). Décision explicite, énoncée par
  # `tests/testthat/test-conventions-c10-scope.R`.
  check_c10_error_style(c(r_files, m_files))
  check_c11_parallel(all_code)
  check_c12_headers(r_files)
  check_c13_choices_named_values(c(r_files, m_files))
  check_c16_errorcondition_arity(c(r_files, m_files))

  rules <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C9b", "C10",
             "C11", "C12", "C13", "C16")

  cat("\n--------------------------------------------------------------------\n")
  cat(sprintf("%-5s %-8s %s\n", "RÈGLE", "NIVEAU", "DESCRIPTION"))
  cat("--------------------------------------------------------------------\n")
  desc <- c(
    C1  = "R/modules/ ne doit pas exister (R/ = logique pure)",
    C2  = "aucun symbole Shiny dans R/",
    C3  = "toute cible de source() existe et est versionnée",
    C4  = "aucun setwd() dans R/ ou modules/",
    C5  = "aucun browser() oublié",
    C6  = "pas de library()/require() au top-level de R/ (dette)",
    C7  = "toute clé tr() existe dans i18n/translation.json",
    C8  = "chaque contrat gelé est référencé par un test (dette)",
    C9  = "chaque fichier de R/ a son fichier de test (dette)",
    C9b = "test eponyme PRESENT : ses fonctions sont-elles citees ? (appel)",
    C10 = "stop() classé (errorCondition) ou call. = FALSE (dette)",
    C11 = "primitives parallèles à vérifier (mirai uniquement)",
    C12 = "en-tête commenté dans chaque fichier de R/ (dette)",
    C13 = "choices nommé : la valeur n'est jamais un appel traduit",
    C16 = "errorCondition() : un SEUL argument positionnel (sinon message tronqué)"
  )
  lvl <- setNames(rep("ERREUR", length(rules)), rules)
  lvl[c("C6", "C8", "C9", "C9b", "C10", "C11", "C12")] <- "AVERT."
  for (r in rules) {
    n <- sum(vapply(.REPORT$errors, function(e) identical(e$rule, r), logical(1))) +
      sum(vapply(.REPORT$warns, function(e) identical(e$rule, r), logical(1)))
    cat(sprintf("%-5s %-8s %-55s %s\n", r, lvl[[r]], desc[[r]],
                if (n == 0) "OK (0)" else sprintf("%d signalement(s)", n)))
  }
  cat("--------------------------------------------------------------------\n")

  if (length(.REPORT$errors)) {
    cat("\n-- ERREURS (bloquantes) --\n")
    for (e in .REPORT$errors) {
      loc <- if (is.na(e$line)) .rel(e$file) else sprintf("%s:%d", .rel(e$file), e$line)
      cat(sprintf("  %-4s %s\n       %s\n", e$rule, loc, e$detail))
    }
  }
  if (length(.REPORT$warns)) {
    n_warn_total <- length(.REPORT$warns)
    # Défaut corrigé le 2026-09-16 : le message invitait à passer `--strict`
    # « pour tout lister ». Or `--strict` ne change RIEN à l'affichage — il rend
    # seulement les avertissements BLOQUANTS (voir `blocking` plus bas). La garde
    # demandait donc une action qui ne produisait pas l'effet annoncé, et la
    # dette mesurée n'était PAS énumérable : on ne peut pas réduire ce qu'on ne
    # peut pas lister. D'où `--list-all`.
    n_show <- if (isTRUE(list_all)) n_warn_total else min(25L, n_warn_total)
    cat(sprintf("\n-- AVERTISSEMENTS (dette mesurée, %d)%s --\n",
                n_warn_total,
                if (n_show < n_warn_total) " — premiers signalements" else ""))
    for (e in .REPORT$warns[seq_len(n_show)]) {
      loc <- if (is.na(e$line)) .rel(e$file) else sprintf("%s:%d", .rel(e$file), e$line)
      cat(sprintf("  %-4s %s\n       %s\n", e$rule, loc, e$detail))
    }
    if (n_show < n_warn_total) {
      cat(sprintf("  ... et %d autre(s) — relancer avec --list-all pour tout lister.\n",
                  n_warn_total - n_show))
    }
  }

  n_err <- length(.REPORT$errors)
  n_warn <- length(.REPORT$warns)
  cat(sprintf("\n---- Résumé : %d erreur(s), %d avertissement(s) ----\n", n_err, n_warn))
  blocking <- n_err + (if (strict) n_warn else 0L)
  invisible(if (blocking > 0) 1L else 0L)
}

# ---------------------------------------------------------------------------
# Point d'entrée — ne s'exécute qu'en `Rscript` (jamais au source()).
# ---------------------------------------------------------------------------
if (identical(environment(), globalenv()) && sys.nframe() == 0L &&
    !interactive() && length(grep("--file=", commandArgs(trailingOnly = FALSE))) > 0) {
  argv <- commandArgs(trailingOnly = TRUE)
  status <- run_check(strict = "--strict" %in% argv,
                      use_git = !("--no-git" %in% argv),
                      list_all = "--list-all" %in% argv)
  quit(status = status, save = "no")
}

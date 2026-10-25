# =============================================================================
# tools/check_writers.R — une AUTRE session ecrit-elle le meme arbre ?
# =============================================================================
# Contexte (paye le 2026-09-16) : deux sessions Workbuddy ont ecrit le meme
# arbre en parallele. Consequences mesurees : une section de ROADMAP.md
# remplacee en silence, et deux suites de tests lancees en meme temps
# (contention du port Chrome) qui ont produit un faux « 5406 PASS / 2 SKIP »
# la ou l'arbre tranquille donne « 5408 PASS / 1 SKIP ».
#
# Workbuddy n'a PAS de verrou de session. Ce script est le detecteur le moins
# couteux qui ne demande AUCUNE cooperation de l'autre session : il regarde
# ce que l'autre session laisse forcement derriere elle (mtimes, reflog).
#
# Usage :
#   Rscript tools/check_writers.R                 # diagnostic (defaut : 30 min)
#   Rscript tools/check_writers.R --minutes=5     # fenetre plus courte
#   Rscript tools/check_writers.R --register      # (re)pose MON battement
#
# MODE ESSAIM (jalon SW-3, decision O9 2026-10-25 : extension du garde, pas un
# 3e outil — O4) :
#   Rscript tools/check_writers.R --swarm             # racine = "."
#   Rscript tools/check_writers.R --swarm --root=X    # racine de fixture
# Valide : (1) le BLOC ESSAIM de docs/agent/HANDOFF.md s'il existe (exactement
# un « etat : EN COURS » porteur ; roles dans {orchestrateur, collecteur,
# batisseur, verificateur} ; perimetres d'ecriture disjoints ; branche jetable
# = naissance + destruction programmees, convention 2026-10-14) — incoherence
# => constat classe, exit 1 ; ABSENCE de bloc => silence (sessions solo) ;
# (2) le LINT DES REGISTRES DE CLAIMS (globs docs/agent/swarm/**/
# collector_*_claims.md + fiches de verification : table a 8 colonnes du
# schema docs/agent/swarm/CLAIMS_LEDGER_SCHEMA.md pour les registres ;
# verdicts dans EXACTEMENT {CONFIRME, CONFIRME-NUANCE, REFUTE, NON VERIFIE} ;
# une commande de preuve par ligne-claim) — ecart => constat fichier:ligne.
# En in-process : swarm_check(root = ".") -> character() de constats (vide =
# silence). Le mode PAR DEFAUT ci-dessus est INCHANGE.
#
# Le battement est OPT-IN : il ne sert que si les DEUX sessions l'appellent.
# Sans hook, la detection repose sur les mtimes et le reflog, qui suffisent.
#
# Sortie : stdout. Code de sortie 0 = aucun ecrivain concurrent (defaut) /
# aucun ecart essaim (--swarm) ; 1 = au moins un. Rscript peut sortir en 139
# au teardown : lire la SORTIE, pas le code (piege connu du depot).
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^--", name, "="), "", hit[[1]])
}

MINUTES <- suppressWarnings(as.numeric(arg_value("minutes", 30)))
if (is.na(MINUTES) || MINUTES <= 0) MINUTES <- 30
REGISTER <- "--register" %in% args

# Dossiers lourds ou non pertinents : jamais parcourus.
SKIP <- c("renv", "python_env_sccoda", ".git", "node_modules", "QC", ".Rproj.user")

# Racines scannees : le code et la documentation, pas les donnees.
SCAN <- c("R", "modules", "tests", "tools", "config", "i18n", "www",
          "scripts", "mcp.examples", "docs")
ROOT_FILES <- c("app.R", "global.R", "CHANGELOG.md", "AGENTS.md",
                "renv.lock", "CONVENTIONS.md")

HB_DIR <- file.path(".workbuddy-ai", "sessions")
fmt <- function(t) format(t, "%Y-%m-%d %H:%M:%S")

# --- 0. Mode essaim (SW-3 / O9) ----------------------------------------------
# Lint du bloc essaim de HANDOFF + des registres de claims. REUSE de la regle 3
# du schema : une locale doit etre posee avant tout parse/regex multi-octet.

SWARM_VERDICTS <- c("CONFIRMÉ", "CONFIRMÉ-NUANCÉ", "RÉFUTÉ", "NON VÉRIFIÉ")
SWARM_ROLES <- c("orchestrateur", "collecteur", "bâtisseur", "vérificateur")

.swarm_locale <- function() {
  tryCatch(
    Sys.setlocale("LC_CTYPE", "English_United States.utf8"),
    error = function(e) invisible(NULL),
    warning = function(w) invisible(NULL)
  )
  invisible(NULL)
}

.swarm_read <- function(path) readLines(path, warn = FALSE)

# Cellules d'une ligne de table markdown. Les pipes ECHAPPES (\|, ex.
# « awk ... \| grep -c » dans les preuves) ne sont pas des separateurs.
.swarm_cells <- function(line) {
  clean <- gsub("\\|", "", line, fixed = TRUE)  # pipes echappes (awk ... \| grep)
  # Pipes NON echappes a l'interieur d'un span backtick (ex. « `%||%` ») : ils
  # ne sont pas des separateurs — spans masques avant split, restitues apres
  # (mesure : D2-18 se decalait de 2 colonnes, 2026-10-25).
  spans <- regmatches(clean, gregexpr("`[^`]*`", clean, perl = TRUE))[[1]]
  for (j in seq_along(spans)) {
    clean <- sub(spans[[j]], paste0("\001", j, "\001"), clean, fixed = TRUE)
  }
  clean <- sub("^\\s*\\|", "", clean)
  clean <- sub("\\|\\s*$", "", clean)
  parts <- trimws(strsplit(clean, "|", fixed = TRUE)[[1]])
  for (j in seq_along(spans)) {
    parts <- gsub(paste0("\001", j, "\001"), spans[[j]], parts, fixed = TRUE)
  }
  # Marquer UTF-8 : avec perl=TRUE, R passe PCRE en mode UTF-8 — sinon \b est
  # BYTE-based et l'octet final de « é » (0xA9, non-mot) cree une fausse
  # frontiere : « exécuté » contiendrait « cut » (mesure, 2026-10-25).
  Encoding(parts) <- "UTF-8"
  parts
}

.swarm_grab <- function(entry, label) {
  rx <- paste0("(?i)", label, "[[:space:]]*:[[:space:]]*([^·—]*)")
  m <- regexec(rx, entry, perl = TRUE)[[1]]
  if (m[[1]] == -1L) return(NA_character_)
  trimws(substring(entry, m[[2]], m[[2]] + attr(m, "match.length")[[
    2]] - 1L))
}

# (1a) BLOC ESSAIM de HANDOFF — repere par un TITRE contenant « bloc essaim »
# (une simple mention dans la prose ou un tableau ne declenche RIEN).
.swarm_check_block <- function(path) {
  lines <- .swarm_read(path)
  head_rx <- "^#{1,6}[[:space:]]"
  hits <- grep(paste0(head_rx, ".*[Bb]loc[[:space:]]+essaim"), lines)
  if (!length(hits)) return(character(0))
  start <- hits[[1]]
  after <- which(seq_along(lines) > start & grepl(head_rx, lines))
  end <- if (length(after)) min(after) - 1L else length(lines)
  blk <- (start + 1L):end

  entry_idx <- blk[grepl("^[[:space:]]*-[[:space:]]", lines[blk])]
  if (!length(entry_idx)) {
    return(paste0("BLOC_ESSAIM docs/agent/HANDOFF.md:", start,
                  " — bloc essaim vide (aucune entrée agent « - … »)"))
  }

  entries <- lines[entry_idx]
  names_ <- vapply(entries, function(e) {
    m <- regmatches(e, regexec("\\*\\*([^*]+)\\*\\*", e, perl = TRUE))[[1]]
    if (length(m) >= 2L) return(m[[2]])
    n <- trimws(sub("^[[:space:]]*-[[:space:]]*", "", e))
    n <- sub("[·—].*$", "", n)
    if (!nzchar(n)) n <- "?"
    n
  }, character(1), USE.NAMES = FALSE)

  etats <- vapply(entries, .swarm_grab, character(1), "état", USE.NAMES = FALSE)
  roles <- vapply(entries, .swarm_grab, character(1), "rôle", USE.NAMES = FALSE)
  perims <- vapply(entries, .swarm_grab, character(1), "périmètre",
                   USE.NAMES = FALSE)
  branches <- vapply(entries, .swarm_grab, character(1), "branche",
                     USE.NAMES = FALSE)

  findings <- character(0)

  # (a) exactement UN porteur « EN COURS »
  porteurs <- names_[!is.na(etats) & grepl("EN COURS", etats, fixed = TRUE)]
  if (length(porteurs) != 1L) {
    findings <- c(findings, paste0(
      "BLOC_ESSAIM docs/agent/HANDOFF.md:", start, " — ", length(porteurs),
      " agent(s) « EN COURS » (attendu : exactement un porteur) : ",
      if (length(porteurs)) paste(porteurs, collapse = ", ") else "(aucun)"))
  }

  # (b) roles declares coherents
  for (i in seq_along(entries)) {
    if (is.na(roles[[i]]) || !tolower(roles[[i]]) %in% SWARM_ROLES) {
      findings <- c(findings, paste0(
        "BLOC_ESSAIM docs/agent/HANDOFF.md:", entry_idx[[i]], " — rôle « ",
        if (is.na(roles[[i]]) || !nzchar(roles[[i]])) "(absent)" else roles[[i]],
        " » hors vocabulaire (orchestrateur | collecteur | bâtisseur | ",
        "vérificateur) [", names_[[i]], "]"))
    }
  }

  # (c) perimetres d'ecriture disjoints entre agents declares
  scope_map <- list()
  for (i in seq_along(entries)) {
    if (is.na(perims[[i]]) || !nzchar(perims[[i]])) next
    items <- trimws(strsplit(perims[[i]], ",", fixed = TRUE)[[1]])
    items <- gsub("`", "", items, fixed = TRUE)
    items <- items[nzchar(items)]
    for (it in items) scope_map[[it]] <- c(scope_map[[it]], names_[[i]])
  }
  for (it in names(scope_map)) {
    decl <- unique(scope_map[[it]])
    if (length(decl) > 1L) {
      findings <- c(findings, paste0(
        "BLOC_ESSAIM docs/agent/HANDOFF.md:", start, " — périmètre NON ",
        "disjoint : ", it, " déclaré par ", length(decl), " agents (",
        paste(decl, collapse = ", "), ")"))
    }
  }

  # (d) branche jetable = naissance + destruction programmees (2026-10-14)
  for (i in seq_along(entries)) {
    if (is.na(branches[[i]]) || !nzchar(branches[[i]])) next
    ok <- grepl("naissance", branches[[i]], ignore.case = TRUE) &&
      grepl("destruction", branches[[i]], ignore.case = TRUE)
    if (!ok) {
      findings <- c(findings, paste0(
        "BLOC_ESSAIM docs/agent/HANDOFF.md:", entry_idx[[i]], " — branche ",
        "jetable sans « naissance » ET « destruction programmée » ",
        "mentionnées (convention 2026-10-14) [", names_[[i]], "]"))
    }
  }

  findings
}

# Commande de preuve : soit dans un span backtick (les registres reels citent
# leur commande entre backticks), soit token delimite par la classe ASCII
# explicite dans la cellule. PAS de \b PCRE ici : en mode non-UTF il est
# BYTE-based et l'octet final de « é » (0xA9, non-mot) cree une fausse
# frontiere — « exécuté » contiendrait « cut » (mesure, 2026-10-25).
.swarm_tokens <- paste0("(grep|sed|awk|git|rscript|sha256|sha1|md5|wc|ls|find|",
                        "cat|head|tail|jq|diff|cut|sort|R -e)")

.swarm_has_command <- function(cell) {
  spans <- regmatches(cell, gregexpr("`[^`]+`", cell))[[1]]
  if (length(spans) && any(grepl(.swarm_tokens, spans, ignore.case = TRUE))) {
    return(TRUE)
  }
  rx <- paste0("(^|[^A-Za-z0-9_\\x80-\\xff])", .swarm_tokens,
               "($|[^A-Za-z0-9_\\x80-\\xff])")
  grepl(rx, cell, ignore.case = TRUE, perl = TRUE)
}

# (1b) LINT DES REGISTRES : collector_*_claims.md (8 colonnes du schema +
# vocabulaire a 4 verdicts + commande de preuve) et fiches de verification
# (meme vocabulaire / commande ; leur table propre est plus courte).
.swarm_check_ledgers <- function(root) {
  swarm_dir <- file.path(root, "docs", "agent", "swarm")
  if (!dir.exists(swarm_dir)) return(character(0))
  files <- list.files(swarm_dir, recursive = TRUE, full.names = TRUE)
  is_reg <- grepl("collector_[^/\\\\]*_claims\\.md$", files)
  base <- basename(files)
  is_fiche <- grepl("fiche|verification", base, ignore.case = TRUE) &
    !grepl("TEMPLATE|SCHEMA|README", base, ignore.case = TRUE)
  files <- files[is_reg | is_fiche]
  rel_of <- function(p) paste0("docs/agent/swarm/",
                               substring(p, nchar(swarm_dir) + 2L))

  findings <- character(0)
  for (f in files) {
    lines <- .swarm_read(f)
    rel <- rel_of(f)
    is_reg_f <- grepl("collector_[^/\\\\]*_claims\\.md$", f)
    tab <- grep("^\\s*\\|", lines)
    if (!length(tab)) {
      findings <- c(findings, paste0("REGISTRE ", rel, ":1",
                                     " — aucune table markdown (registre/fiche vide)"))
      next
    }
    hdr_i <- tab[[1]]
    hdr <- .swarm_cells(lines[[hdr_i]])

    if (is_reg_f && length(hdr) != 8L) {
      findings <- c(findings, paste0("REGISTRE ", rel, ":", hdr_i,
        " — table à ", length(hdr), " colonnes au lieu de 8 ",
        "(schéma CLAIMS_LEDGER_SCHEMA.md : ID | source | claim | domain | ",
        "severity | verdict | proof | scope)"))
      next
    }

    vcol <- which(grepl("verdict", hdr, ignore.case = TRUE))
    pcol <- which(grepl("proof|preuve", hdr, ignore.case = TRUE))
    if (!length(vcol)) {
      findings <- c(findings, paste0("REGISTRE ", rel, ":", hdr_i,
                                     " — colonne « verdict » introuvable"))
      next
    }
    vcol <- vcol[[1]]
    if (!length(pcol)) {
      findings <- c(findings, paste0("REGISTRE ", rel, ":", hdr_i,
                                     " — colonne « proof/preuve » introuvable"))
      next
    }
    pcol <- pcol[[1]]

    sep_i <- hdr_i + 1L
    is_data <- FALSE
    # Cas degenere (contre-lecture 2026-10-25) : en-tete ou separateur en
    # DERNIERES lignes du fichier — la plage (sep_i+1):length(lines) serait
    # descendante et lines[[i]] planterait. Constat au lieu de crash.
    if (sep_i + 1L > length(lines)) {
      findings <- c(findings, paste0("REGISTRE ", rel, ":", hdr_i,
        " — aucune ligne-claim (table sans ligne de données)"))
      next
    }
    for (i in (sep_i + 1L):length(lines)) {
      if (!grepl("^\\s*\\|", lines[[i]])) {
        if (is_data) break else next
      }
      is_data <- TRUE
      cells <- .swarm_cells(lines[[i]])
      id <- if (length(cells) >= 1L) cells[[1]] else "?"
      verdict <- if (length(cells) >= vcol) cells[[vcol]] else ""
      if (!verdict %in% SWARM_VERDICTS) {
        findings <- c(findings, paste0("REGISTRE ", rel, ":", i,
          " — verdict « ", verdict, " » hors vocabulaire ",
          "(CONFIRMÉ | CONFIRMÉ-NUANCÉ | RÉFUTÉ | NON VÉRIFIÉ) [", id, "]"))
      }
      proof <- if (length(cells) >= pcol) cells[[pcol]] else ""
      if (!.swarm_has_command(proof)) {
        findings <- c(findings, paste0("REGISTRE ", rel, ":", i,
          " — preuve SANS commande (toute mesure porte la commande exacte ",
          "qui l'a produite) [", id, "]"))
      }
    }
  }
  findings
}

#' Mode essaim du garde : constats classes, character(0) = silence.
swarm_check <- function(root = ".") {
  .swarm_locale()
  findings <- character(0)
  handoff <- file.path(root, "docs", "agent", "HANDOFF.md")
  if (file.exists(handoff)) findings <- c(findings, .swarm_check_block(handoff))
  findings <- c(findings, .swarm_check_ledgers(root))
  findings
}

# --- 1. Battement de session (opt-in) ---------------------------------------
self_id <- paste0(Sys.info()[["nodename"]], "-", Sys.getpid())

check_writers_run <- function(minutes = 30, register = FALSE) {
  if (is.na(minutes) || minutes <= 0) minutes <- 30
  if (register) {
    dir.create(HB_DIR, recursive = TRUE, showWarnings = FALSE)
    writeLines(fmt(Sys.time()), file.path(HB_DIR, paste0(self_id, ".hb")))
    cat("Battement pose : ", file.path(HB_DIR, paste0(self_id, ".hb")), "\n", sep = "")
  }

  foreign <- character(0)
  if (dir.exists(HB_DIR)) {
    hb <- list.files(HB_DIR, pattern = "\\.hb$", full.names = TRUE)
    hb <- hb[basename(hb) != paste0(self_id, ".hb")]
    if (length(hb)) {
      age <- as.numeric(difftime(Sys.time(), file.mtime(hb), units = "mins"))
      foreign <- hb[age <= minutes]
    }
  }

  # --- 2. Fichiers modifies recemment (le detecteur principal) -----------------
  paths <- character(0)
  for (d in SCAN) {
    if (dir.exists(d)) {
      paths <- c(paths, list.files(d, recursive = TRUE, full.names = TRUE))
    }
  }
  paths <- c(paths, ROOT_FILES[file.exists(ROOT_FILES)])
  # `full.names = TRUE` rend le filtre exact (jamais de regex : la racine
  # contient des parentheses).
  paths <- paths[!vapply(strsplit(paths, "/", fixed = TRUE),
                         function(p) any(p %in% SKIP), logical(1))]

  mt <- file.mtime(paths)
  recent <- paths[!is.na(mt) & as.numeric(difftime(Sys.time(), mt, units = "mins")) <= minutes]
  recent <- recent[order(file.mtime(recent), decreasing = TRUE)]

  # --- 3. Reflog : ce que l'arbre a enregistre, et quand -----------------------
  reflog <- suppressWarnings(
    tryCatch(system2("git", c("reflog", "--date=iso", "-8"),
                     stdout = TRUE, stderr = FALSE), error = function(e) character(0))
  )

  # --- Rapport ----------------------------------------------------------------
  cat("=== check_writers.R — fenetre ", minutes, " min — ", fmt(Sys.time()), " ===\n\n", sep = "")

  cat("1. BATTEMENTS DE SESSION ETRANGERS\n")
  if (!dir.exists(HB_DIR)) {
    cat("   (aucun dossier de battements : mecanisme opt-in non utilise)\n")
  } else if (!length(foreign)) {
    cat("   aucun\n")
  } else {
    for (f in foreign) cat("   ACTIF : ", basename(f), " (", fmt(file.mtime(f)), ")\n", sep = "")
  }

  cat("\n2. FICHIERS MODIFIES DANS LES ", minutes, " DERNIERES MINUTES : ", length(recent), "\n", sep = "")
  if (length(recent)) {
    for (i in seq_len(min(25L, length(recent)))) {
      cat("   ", fmt(file.mtime(recent[[i]])), "  ", recent[[i]], "\n", sep = "")
    }
    if (length(recent) > 25L) cat("   ... et ", length(recent) - 25L, " autre(s)\n", sep = "")
  } else {
    cat("   aucun\n")
  }

  cat("\n3. DERNIERS MOUVEMENTS DE L'ARBRE (reflog)\n")
  if (!length(reflog)) {
    cat("   (reflog illisible)\n")
  } else {
    for (l in reflog) cat("   ", l, "\n", sep = "")
  }

  cat("\n--- VERDICT ---\n")
  if (length(foreign)) {
    cat("ECRIVAIN CONCURRENT : ", length(foreign),
        " battement(s) etranger(s) actif(s). NE PAS editer de doc partage,\n", sep = "")
    cat("NE PAS lancer de suite de tests, NE PAS committer un etat intermediaire.\n")
    cat("Consequence connue : tout chiffre mesure maintenant est FAUX.\n")
  } else if (length(recent)) {
    cat("Aucun battement etranger, mais ", length(recent), " fichier(s) modifie(s) dans la fenetre.\n", sep = "")
    cat("Si vous n'en etes pas l'auteur : une autre session ecrit. Verifier les horaires\n")
    cat("ci-dessus contre votre propre historique avant d'editer un doc partage.\n")
  } else {
    cat("Aucun ecrivain concurrent detecte : l'arbre est a vous.\n")
  }

  invisible(if (length(foreign)) 1L else 0L)
}

# --- Dispatch CLI -------------------------------------------------------------
# Garde d'execution repris tel quel de tools/check_duplication.R:417-419
# (source sous testthat -> PAS de run ; Rscript -> run). En source in-process,
# le mode par defaut ne s'execute plus : utiliser check_writers_run() /
# swarm_check().
.CWR_IS_CLI <- identical(environment(), globalenv()) && sys.nframe() == 0L &&
  !interactive() && length(grep("--file=", commandArgs(trailingOnly = FALSE))) > 0

if (.CWR_IS_CLI) {
  if ("--swarm" %in% args) {
    .swarm_locale()
    findings <- swarm_check(root = arg_value("root", "."))
    cat("=== check_writers.R --swarm — mode essaim (SW-3 / O9) — ",
        fmt(Sys.time()), " ===\n\n", sep = "")
    if (!length(findings)) {
      cat("Aucun ecart : bloc essaim coherent (ou absent), registres de claims conformes.\n")
      quit(save = "no", status = 0L)
    }
    cat(length(findings), " ecart(s) detecte(s) :\n\n", sep = "")
    for (f in findings) cat("  - ", f, "\n", sep = "")
    cat("\nVERDICT : bloc essaim / registres de claims NON conformes (exit 1).\n")
    quit(save = "no", status = 1L)
  }
  invisible(check_writers_run(MINUTES, REGISTER))
}

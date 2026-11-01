#!/usr/bin/env Rscript
# =============================================================================
# tools/check_duplication.R — static anti-duplication guard for TranscriptoShiny
# =============================================================================
# Zero-dependency (base R only), so it runs on any machine with just R
# installed — no package install step needed before a merge-gate check.
#
# Detects three concrete bug classes that have ALREADY bitten this project
# (see HANDOFF_TranscriptoShiny.txt Phase 0, and helpers_sc.R's own header
# note about a shadowed duplicate build_sc_viz_plot()):
#
#   1. output$xxx <- assigned more than once IN THE SAME FILE — the later
#      definition silently wins; the earlier renderer/downloadHandler is
#      dead code (found + fixed in mod_spatial_viz.R this session: dl_png
#      defined twice).
#   2. The SAME top-level function name defined in MORE THAN ONE sourced
#      file — app.R source()s every file into one shared global environment,
#      so the file sourced LAST silently wins and shadows the other(s). This
#      is the exact bug class behind the (already fixed, per helpers_sc.R's
#      own comment) duplicate 2-arg build_sc_viz_plot(), and — confirmed
#      live during this session — bulk_role_colors() / bulk_annotation_colors()
#      / .default_manual_colors() / manual_color_picker_ui() / bulk_color_scale(),
#      duplicated between helpers_bulk.R and R/palettes.R. helpers_bulk.R's
#      copy of bulk_role_colors() has NO requireNamespace() guard around
#      viridisLite/RColorBrewer (R/palettes.R's copy does) — currently only
#      safe because R/palettes.R is sourced AFTER helpers_bulk.R in app.R and
#      overwrites it; reorder that sourcing (or call the function from a
#      script/test that only sources helpers_bulk.R) and it throws on every
#      call, including palette="default". See tests/testthat/test-helpers_bulk.R
#      for a reproducible regression check exercising the REAL app.R order.
#   3. observeEvent(...) blocks sharing the exact same (whitespace-normalized)
#      first argument more than once IN THE SAME FILE — usually an accidental
#      copy-paste of a whole reactive block (soft signal: reported as a
#      WARNING, not a hard failure, since re-triggering off the same source
#      in two legitimately different observers does happen).
#
# PLUS the originally-requested generic check:
#   4. Any >= MIN_BLOCK (default 15) consecutive non-blank lines that are
#      byte-identical (after trimming) somewhere else in the scanned tree —
#      within the same file OR across two different files. Overlapping
#      window hits on the same diagonal are merged into one contiguous span
#      per duplicate pair, so a single big duplicated block is reported ONCE.
#
# USAGE
#   Rscript tools/check_duplication.R [roots...] [--min-block=N] [--ext=R]
#                                     [--fail-on-warning]
#
#   No roots given -> defaults to "modules" (this session's explicit ask:
#   "sur modules/**/*.R avant tout merge"). To ALSO catch cross-file function
#   duplication involving the top-level helpers_*.R / R/*.R files (where the
#   bulk_role_colors() finding above actually lives — outside modules/),
#   widen the scope explicitly:
#
#     Rscript tools/check_duplication.R modules R . --ext=R
#
#   ("." scans the project root non-recursively for *.R files, i.e.
#   app.R/global.R/helpers_*.R, without descending into modules/ again if
#   already listed separately — duplicate scanning of the same file is
#   de-duplicated automatically by absolute path.)
#
# EXIT CODE: 0 if clean, 1 if any ERROR-severity finding exists (findings
# classes 1, 2, 4 above). Class 3 (observeEvent duplicate trigger) is a
# WARNING and does not affect the exit code unless --fail-on-warning is set.
# Intended to be wired into a pre-merge CI step; also runnable ad hoc.
# =============================================================================

MIN_BLOCK_DEFAULT <- 15L

# ---------------------------------------------------------------------------
# CLI parsing (base R only)
# ---------------------------------------------------------------------------
.parse_args <- function(argv) {
  flags <- list(min_block = MIN_BLOCK_DEFAULT, ext = "R", fail_on_warning = FALSE)
  roots <- character(0)
  for (a in argv) {
    if (grepl("^--min-block=", a)) {
      flags$min_block <- as.integer(sub("^--min-block=", "", a))
    } else if (grepl("^--ext=", a)) {
      flags$ext <- sub("^--ext=", "", a)
    } else if (identical(a, "--fail-on-warning")) {
      flags$fail_on_warning <- TRUE
    } else if (startsWith(a, "--")) {
      warning("Option inconnue ignoree : ", a)
    } else {
      roots <- c(roots, a)
    }
  }
  if (length(roots) == 0) roots <- "modules"
  list(roots = roots, min_block = flags$min_block, ext = flags$ext,
       fail_on_warning = flags$fail_on_warning)
}

# ---------------------------------------------------------------------------
# File discovery
# ---------------------------------------------------------------------------
#' Collect *.R files under one or more roots (recursive for directories,
#' non-recursive top-level scan if a root IS itself just "." — avoids
#' re-descending into an already-listed subdirectory when "." is combined
#' with e.g. "modules").
.collect_files <- function(roots, ext) {
  pattern <- paste0("\\.", ext, "$")
  files <- character(0)
  for (root in roots) {
    if (!dir.exists(root)) {
      if (file.exists(root)) { files <- c(files, root); next }
      warning("Chemin introuvable, ignore : ", root); next
    }
    if (identical(root, ".")) {
      hits <- list.files(root, pattern = pattern, full.names = TRUE, recursive = FALSE)
    } else {
      hits <- list.files(root, pattern = pattern, full.names = TRUE, recursive = TRUE)
    }
    files <- c(files, hits)
  }
  files <- unique(normalizePath(files, winslash = "/", mustWork = FALSE))
  sort(files)
}

# ---------------------------------------------------------------------------
# Line preprocessing helpers
# ---------------------------------------------------------------------------
#' Strip a best-effort approximation of string-literal contents and line
#' comments before counting braces — avoids miscounting "{" or "}" that
#' appear inside a quoted string or after a "#". Deliberately simple (not a
#' real R tokenizer): good enough for brace-depth tracking, not intended for
#' anything requiring byte-perfect parsing.
.strip_strings_and_comments <- function(line) {
  # Remove double- and single-quoted strings (non-greedy, handles \" \\ escapes)
  line <- gsub('"([^"\\\\]|\\\\.)*"', '""', line, perl = TRUE)
  line <- gsub("'([^'\\\\]|\\\\.)*'", "''", line, perl = TRUE)
  # Remove a trailing # comment (best-effort: does not know it's already
  # inside a string at this point since strings were stripped above)
  sub("#.*$", "", line)
}

#' Read a file's lines and return a data.frame(line_no, raw, trimmed, depth)
#' where `depth` is the brace+paren nesting depth *before* that line is
#' evaluated (0 = top level, i.e. what actually lands in the sourcing
#' environment).
.read_annotated_lines <- function(path) {
  raw <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"),
                  error = function(e) character(0))
  depth <- 0L
  depths <- integer(length(raw))
  for (i in seq_along(raw)) {
    depths[i] <- depth
    clean <- .strip_strings_and_comments(raw[i])
    opens  <- lengths(regmatches(clean, gregexpr("\\{|\\(", clean)))
    closes <- lengths(regmatches(clean, gregexpr("\\}|\\)", clean)))
    # closing braces on THIS line reduce the depth attributed to THIS line
    # itself only if they close before any open earlier on the same line —
    # approximated by just netting per-line (good enough: function-def
    # detection below only cares about depth == 0 at the START of the line,
    # which this preserves correctly for the common one-brace-per-line style
    # used throughout this codebase).
    depth <- max(0L, depth + opens - closes)
  }
  data.frame(line_no = seq_along(raw), raw = raw, trimmed = trimws(raw),
            depth = depths, stringsAsFactors = FALSE)
}

# =============================================================================
# CHECK 1 + 2 — output$xxx <- and top-level `name <- function(` duplicates
# =============================================================================
.FUN_DEF_RE    <- "^([a-zA-Z_.][a-zA-Z0-9_.]*)\\s*(<-|=)\\s*function\\s*\\("
.OUTPUT_DEF_RE <- "output\\$([a-zA-Z0-9_.]+)\\s*<-"
.OBSERVE_RE    <- "observeEvent\\s*\\("

find_output_and_function_duplicates <- function(files) {
  output_hits   <- list()  # key "file" -> character vector of output names, in order seen
  function_defs <- list()  # key "funcname" -> data.frame(file, line)
  observe_hits  <- list()  # key "file" -> list(trigger -> line numbers)

  for (f in files) {
    ann <- .read_annotated_lines(f)
    if (nrow(ann) == 0) next

    # -- output$xxx <- (any depth: renderX()/downloadHandler() bodies are
    #    themselves nested, so this deliberately does NOT filter by depth).
    #    NOTE: regexpr()+regmatches() silently DROPS non-matching elements
    #    instead of aligning with the input vector — grepl() is used for the
    #    boolean mask so `idx` stays aligned with ann$line_no.
    mask <- grepl(.OUTPUT_DEF_RE, ann$trimmed, perl = TRUE)
    idx <- which(mask)
    if (length(idx) > 0) {
      names_here <- sub(paste0(".*", .OUTPUT_DEF_RE, ".*"), "\\1", ann$trimmed[idx], perl = TRUE)
      for (k in seq_along(idx)) {
        key <- names_here[k]
        output_hits[[f]][[key]] <- c(output_hits[[f]][[key]], ann$line_no[idx[k]])
      }
    }

    # -- top-level function definitions (depth == 0 only) --
    top <- ann[ann$depth == 0L, , drop = FALSE]
    fm  <- regmatches(top$trimmed, regexec(.FUN_DEF_RE, top$trimmed, perl = TRUE))
    for (k in seq_along(fm)) {
      if (length(fm[[k]]) >= 2 && nzchar(fm[[k]][2])) {
        fname <- fm[[k]][2]
        function_defs[[fname]] <- rbind(
          function_defs[[fname]],
          data.frame(file = f, line = top$line_no[k], stringsAsFactors = FALSE)
        )
      }
    }

    # -- observeEvent() duplicate first-argument trigger, same file --
    obs_idx <- grep(.OBSERVE_RE, ann$trimmed, perl = TRUE)
    for (i in obs_idx) {
      # First argument = text between the matching outer parens' first comma
      # at depth 0 relative to the call — approximated by taking everything
      # up to the first top-level "," on this line (observeEvent(...) calls
      # in this codebase are always written with the trigger expression and
      # the opening "{" of the handler on the SAME line, per house style).
      after <- sub(".*observeEvent\\s*\\(", "", ann$trimmed[i])
      trig  <- sub(",.*$", "", after)
      trig  <- gsub("\\s+", " ", trimws(trig))
      if (!nzchar(trig)) next
      if (is.null(observe_hits[[f]])) observe_hits[[f]] <- list()
      observe_hits[[f]][[trig]] <- c(observe_hits[[f]][[trig]], ann$line_no[i])
    }
  }

  list(output_hits = output_hits, function_defs = function_defs, observe_hits = observe_hits)
}

# =============================================================================
# CHECK 4 — >= min_block identical consecutive-line spans (within or across
# files), via sliding-window hashing + diagonal-run merging.
# =============================================================================
#' Cheap, deterministic, ASCII-only string hash (djb2 variant).
#'
#' Used ONLY as a bucket key for the sliding-window index below — never for
#' anything security-sensitive. Keying an R environment (used as a hash map)
#' directly on raw multi-byte file content triggered spurious "unable to
#' translate to native encoding" warnings under some locales; hashing to a
#' plain ASCII digit string sidesteps that entirely. Collisions are resolved
#' by an exact identical() comparison before ever reporting a match (see
#' find_duplicate_blocks()), so hash quality affects performance only, never
#' correctness.
.text_hash <- function(x) {
  bytes <- as.integer(charToRaw(x))
  h <- 5381   # plain double (no L suffix) — arithmetic below stays in
              # double precision throughout, which is exact for integers
              # up to 2^53 and therefore never overflows the way 32-bit
              # integer multiplication (`33L * h`) would.
  for (b in bytes) h <- (h * 33 + b) %% 2147483647
  as.character(h)
}

find_duplicate_blocks <- function(files, min_block) {
  # significant_lines[[file]] = data.frame(line_no, text) for non-blank lines
  significant_lines <- list()
  for (f in files) {
    ann <- .read_annotated_lines(f)
    keep <- nzchar(ann$trimmed)
    significant_lines[[f]] <- ann[keep, c("line_no", "trimmed")]
  }

  # window_index: hash(joined W lines) -> list of (file, pos); pos is the
  # INDEX into significant_lines[[file]] (not the raw line number yet)
  window_index <- new.env(hash = TRUE, parent = emptyenv())
  for (f in files) {
    sl <- significant_lines[[f]]
    n  <- nrow(sl)
    if (n < min_block) next
    for (start in seq_len(n - min_block + 1)) {
      window_text <- paste(sl$trimmed[start:(start + min_block - 1)], collapse = "\n")
      key <- .text_hash(window_text)
      cur <- window_index[[key]]
      window_index[[key]] <- c(cur, list(list(file = f, pos = start, text = window_text)))
    }
  }

  # Collect raw pairwise hits: (fileA, posA, fileB, posB), fileA<=fileB and
  # posA<posB when fileA==fileB, to avoid symmetric duplicates. A hash
  # bucket can (rarely) hold non-identical windows that collided — verified
  # with identical() before being accepted as a real match.
  raw_pairs <- list()
  for (key in ls(window_index)) {
    locs <- window_index[[key]]
    if (length(locs) < 2) next
    for (i in seq_len(length(locs) - 1)) {
      for (j in seq((i + 1), length(locs))) {
        a <- locs[[i]]; b <- locs[[j]]
        if (identical(a$file, b$file) && a$pos == b$pos) next
        if (!identical(a$text, b$text)) next  # hash collision, not a real match
        # canonical order
        if (a$file > b$file || (a$file == b$file && a$pos > b$pos)) { tmp <- a; a <- b; b <- tmp }
        raw_pairs[[length(raw_pairs) + 1]] <- list(fileA = a$file, posA = a$pos,
                                                     fileB = b$file, posB = b$pos)
      }
    }
  }
  if (length(raw_pairs) == 0) return(data.frame())

  df <- do.call(rbind, lapply(raw_pairs, as.data.frame))
  df <- unique(df)
  df <- df[order(df$fileA, df$fileB, df$posA, df$posB), ]

  # Diagonal-run merge: consecutive rows (same fileA/fileB) where posA and
  # posB BOTH increment by exactly 1 belong to the same underlying block.
  spans <- list()
  i <- 1L
  while (i <= nrow(df)) {
    j <- i
    while (j < nrow(df) &&
           df$fileA[j + 1] == df$fileA[i] && df$fileB[j + 1] == df$fileB[i] &&
           df$posA[j + 1] == df$posA[j] + 1 && df$posB[j + 1] == df$posB[j] + 1) {
      j <- j + 1L
    }
    posA_start <- df$posA[i]; posA_end <- df$posA[j] + min_block - 1L
    posB_start <- df$posB[i]; posB_end <- df$posB[j] + min_block - 1L
    slA <- significant_lines[[df$fileA[i]]]
    slB <- significant_lines[[df$fileB[i]]]
    spans[[length(spans) + 1]] <- data.frame(
      fileA = df$fileA[i], lineA_start = slA$line_no[posA_start], lineA_end = slA$line_no[posA_end],
      fileB = df$fileB[i], lineB_start = slB$line_no[posB_start], lineB_end = slB$line_no[posB_end],
      n_lines = posA_end - posA_start + 1L,
      stringsAsFactors = FALSE
    )
    i <- j + 1L
  }
  out <- do.call(rbind, spans)
  out[order(-out$n_lines), ]
}

# =============================================================================
# Reporting
# =============================================================================
.rel <- function(path, base = getwd()) {
  p <- normalizePath(path, winslash = "/", mustWork = FALSE)
  b <- normalizePath(base, winslash = "/", mustWork = FALSE)
  ifelse(startsWith(p, b), sub(paste0("^", b, "/?"), "", p), p)
}

run_check <- function(roots, min_block = MIN_BLOCK_DEFAULT, ext = "R", fail_on_warning = FALSE) {
  files <- .collect_files(roots, ext)
  cat(sprintf("[check_duplication] %d fichier(s) .%s scanne(s) sous : %s\n",
              length(files), ext, paste(roots, collapse = ", ")))
  if (length(files) == 0) {
    cat("Aucun fichier trouve — rien a verifier.\n")
    return(invisible(0L))
  }

  n_errors   <- 0L
  n_warnings <- 0L

  cat("\n== 1) output$xxx <- defini plusieurs fois DANS LE MEME FICHIER ==\n")
  res1 <- find_output_and_function_duplicates(files)
  any1 <- FALSE
  for (f in names(res1$output_hits)) {
    for (nm in names(res1$output_hits[[f]])) {
      lines <- res1$output_hits[[f]][[nm]]
      if (length(lines) > 1) {
        any1 <- TRUE; n_errors <- n_errors + 1L
        cat(sprintf("  ERREUR  %s : output$%s defini %d fois (lignes %s) — la derniere definition ecrase silencieusement les precedentes.\n",
                    .rel(f), nm, length(lines), paste(lines, collapse = ", ")))
      }
    }
  }
  if (!any1) cat("  OK — aucun output$ duplique.\n")

  cat("\n== 2) Meme nom de fonction top-level defini dans plusieurs fichiers ==\n")
  any2 <- FALSE
  for (fname in names(res1$function_defs)) {
    d <- res1$function_defs[[fname]]
    d <- d[!duplicated(d$file), ]  # a name defined twice in ONE file is caught by check 4/manual review, not here
    if (nrow(d) > 1) {
      any2 <- TRUE; n_errors <- n_errors + 1L
      cat(sprintf("  ERREUR  %s() defini dans %d fichiers : %s\n",
                  fname, nrow(d), paste(sprintf("%s:%d", .rel(d$file), d$line), collapse = "  |  ")))
      cat("           -> app.R source() ces fichiers dans un ORDRE FIXE ; seule la ",
          "definition sourcee EN DERNIER est active dans l'app. Verifiez que les deux ",
          "copies sont bien identiques, ou supprimez la copie morte.\n", sep = "")
    }
  }
  if (!any2) cat("  OK — aucune fonction top-level dupliquee entre fichiers.\n")

  cat("\n== 3) observeEvent() avec le meme declencheur repete DANS LE MEME FICHIER (avertissement) ==\n")
  any3 <- FALSE
  for (f in names(res1$observe_hits)) {
    for (trig in names(res1$observe_hits[[f]])) {
      lines <- res1$observe_hits[[f]][[trig]]
      if (length(lines) > 1) {
        any3 <- TRUE; n_warnings <- n_warnings + 1L
        cat(sprintf("  AVERT.  %s : observeEvent(%s, ...) repete %d fois (lignes %s) — verifiez que ce n'est pas un copier-coller accidentel d'un bloc reactif entier.\n",
                    .rel(f), trig, length(lines), paste(lines, collapse = ", ")))
      }
    }
  }
  if (!any3) cat("  OK — aucun declencheur observeEvent() duplique detecte.\n")

  cat(sprintf("\n== 4) Blocs de >= %d lignes identiques (meme fichier ou entre fichiers) ==\n", min_block))
  blocks <- find_duplicate_blocks(files, min_block)
  # Passe de fix frontière (2026-11-01) — exemption CONTRACTUELLE épinglée au
  # BLOC (première ligne), jamais au fichier : les jumeaux .comm_drawer_groups
  # de mod_sc_communication.R (copie UI + copie serveur) sont EXIGÉS par le gel
  # test-communication-contract-freeze.R (« exactement 3 fonctions top-level » —
  # aucune constante partagée possible) ; l'égalité des copies est verrouillée
  # par test-mod-sc-communication-ui.R. Tout AUTRE doublon dans ce fichier
  # reste signalé.
  .DUP_BLOCK_EXEMPT_MARKERS <- c(".comm_drawer_groups <- list(")
  if (nrow(blocks) > 0L) {
    keep <- logical(nrow(blocks))
    for (i in seq_len(nrow(blocks))) {
      b <- blocks[i, ]
      a_lines <- tryCatch(readLines(b$fileA, warn = FALSE, encoding = "UTF-8"),
                          error = function(e) character(0))
      lo <- as.integer(b$lineA_start); hi <- as.integer(b$lineA_end)
      seg <- if (length(a_lines) >= hi && hi >= lo) a_lines[lo:hi] else character(0)
      keep[i] <- !any(vapply(.DUP_BLOCK_EXEMPT_MARKERS, function(mk)
        any(grepl(mk, seg, fixed = TRUE)), logical(1)))
    }
    blocks <- blocks[keep, , drop = FALSE]
  }
  if (nrow(blocks) == 0) {
    cat("  OK — aucun bloc duplique detecte.\n")
  } else {
    n_errors <- n_errors + nrow(blocks)
    for (i in seq_len(nrow(blocks))) {
      b <- blocks[i, ]
      same_file <- identical(b$fileA, b$fileB)
      cat(sprintf("  ERREUR  %d lignes identiques :\n           %s:%d-%d\n           %s%s:%d-%d\n",
                  b$n_lines, .rel(b$fileA), b$lineA_start, b$lineA_end,
                  if (same_file) "(meme fichier) " else "", .rel(b$fileB), b$lineB_start, b$lineB_end))
    }
  }

  cat(sprintf("\n---- Resume : %d erreur(s), %d avertissement(s) ----\n", n_errors, n_warnings))
  total_blocking <- n_errors + (if (fail_on_warning) n_warnings else 0L)
  invisible(if (total_blocking > 0) 1L else 0L)
}

# ---------------------------------------------------------------------------
# Entry point — only runs when invoked via `Rscript`, never on source()
# (lets this file also be source()'d from an interactive session or a
# testthat test without immediately executing / calling quit()).
# ---------------------------------------------------------------------------
if (identical(environment(), globalenv()) && sys.nframe() == 0L &&
    !interactive() && length(grep("--file=", commandArgs(trailingOnly = FALSE))) > 0) {
  args <- .parse_args(commandArgs(trailingOnly = TRUE))
  status <- run_check(args$roots, min_block = args$min_block, ext = args$ext,
                      fail_on_warning = args$fail_on_warning)
  quit(status = status, save = "no")
}

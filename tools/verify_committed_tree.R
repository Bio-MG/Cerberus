#!/usr/bin/env Rscript
# =============================================================================
# tools/verify_committed_tree.R — measure the tree a COMMIT ships
# =============================================================================
# WHY THIS EXISTS. Every other gate in this repository measures the WORKING
# TREE. That is a real gap, and it was MEASURED on 2026-09-27, on this host:
#
#   working tree,  at commit da652d7 : check_conventions.R -> 0 errors
#   committed tree, at commit da652d7 : check_conventions.R -> 1 error
#                                         C7 modules/spatial/mod_spatial_qc.R:1059
#
# Same machine, same commit, same moment. The single difference is an i18n key
# that exists only in an UNTRACKED `i18n/translation.json`:
#
#   tr("Aucun resultat de hotspots a exporter.")   # mod_spatial_qc.R:1059
#
# and `git show da652d7:i18n/translation.json` does not contain it. So the green
# working tree was HIDING a red commit — the 59-warning baseline everybody
# quotes is measured on a tree that does not ship.
#
# This tool therefore measures the COMMITTED TREE, by extracting
# `git archive <ref>` and running the gates THERE. It never looks at the
# checkout, so it cannot be fooled by an uncommitted file.
#
# 🔴 IT FIXES NOTHING. The C7 key stays missing on purpose: `translation.json`
# is a FOREIGN file under concurrent edit, and "make the gate green" is not a
# licence to edit it. The tool's whole output is a verdict plus the evidence
# behind it, and the verdict is RED while that key is missing.
#
# 🔴 WHEN THE TWO TREES DISAGREE, THE TOOL SAYS SO. That is the finding, not an
# inconvenience: a check that is red in the commit and green in the working tree
# is being MASKED by an uncommitted file, and the name of the masked check is
# the single most useful thing this tool can print.
#
# Usage:
#   Rscript tools/verify_committed_tree.R              # HEAD, both trees
#   Rscript tools/verify_committed_tree.R <ref>        # a named ref
#   Rscript tools/verify_committed_tree.R --no-work    # committed tree only
#
# Exit status: 0 when the COMMITTED tree is clean, 1 otherwise.

# --- Pure layer -------------------------------------------------------------
# These three are the testable core: classification and reporting, with no
# process, no git and no filesystem. The test file `sys.source`s this file and
# calls them directly, so a test of the verdict never runs a real gate.

#' Read a gate's own words. NEVER its exit code alone.
#'
#' @param txt Combined stdout/stderr of the gate.
#' @param exit_status The process exit status, recorded but NOT decisive.
#' @return list(errors, warnings, findings, unparsed, blocking, exit_status)
#' @noRd
vct_parse_gate_output <- function(txt, exit_status = 0L) {
  if (is.null(txt)) txt <- ""
  lines <- tryCatch(strsplit(as.character(txt), "\r?\n")[[1]],
                    error = function(e) character(0))

  # The summary is ASCII by construction ("erreur(s)", "avertissement(s)"),
  # even though the word around it may carry an accent. Matching the numbers
  # rather than the label keeps this locale-proof.
  #
  # 🔴 `regexec` (capture GROUPS), not `regexpr`+`regmatches`: the latter returns
  # the WHOLE match, and `as.integer(strsplit(...))` on
  # "0 erreur(s), 3 avertissement(s)" then yields c(0, NA, 3, NA) — the count
  # is silently NA. MEASURED: that shipped a green-looking `errors = 0` beside
  # `warnings = NA`, and a NULL-vs-0 mistake in a verdict tool is the one thing
  # that must not be subtle.
  flat <- paste(lines, collapse = "\n")
  m <- regmatches(flat, regexec("([0-9]+) erreur\\(s\\), ([0-9]+) avertissement\\(s\\)", flat))[[1]]
  if (length(m) != 3L) {
    # No summary => we did not understand the output. `unparsed` BLOCKS on
    # purpose: a tool that defaults to OK on a parse failure is a tool that
    # hides outages, which is the exact failure this script exists to end.
    return(list(errors = NULL, warnings = NULL, findings = character(0),
                unparsed = TRUE, blocking = TRUE, exit_status = as.integer(exit_status)))
  }
  findings <- vct_harvest_findings(lines)
  list(errors = as.integer(m[2]), warnings = as.integer(m[3]), findings = findings,
       unparsed = FALSE, blocking = as.integer(m[2]) > 0L,
       exit_status = as.integer(exit_status))
}

#' Harvest the SITE of every ERROR-level finding, and nothing else.
#'
#' The gates print a rule table, then indent one detail line per report under
#' the rule that owns it. So a detail line has to be attributed to the rule
#' ABOVE it, and a WARNING-level rule's details must be dropped — `C10` prints
#' the standing debt ceiling on every run, and harvesting it would report that
#' as a fresh failure.
#'
#' Levels are classified by their FIRST LETTER rather than by their text. The
#' level words are "ERREUR", "AVERT." and "OK"; only the first letter is stable,
#' and matching accented text is a documented way to make a probe lie (see
#' STATUS.md: a `parse()` without `Sys.setlocale()` reported a parseable file
#' as broken). E = error, A = warning, O = ok.
#' @noRd
vct_harvest_findings <- function(lines) {
  rule_re  <- "^(C[0-9]+[a-z]?)[ \t]+([^ \t]+)"
  detail_re <- "^[ \t]+(C[0-9]+[a-z]?)[ \t]+([^ \t]+):([0-9]+)[ \t]*$"
  out <- character(0)
  level <- NA_character_
  for (ln in lines) {
    d <- regmatches(ln, regexec(detail_re, ln))[[1]]
    if (length(d) == 4L) {
      # An indented site line belongs to the rule currently in scope.
      if (identical(level, "E")) out <- c(out, sprintf("%s %s:%s", d[2], d[3], d[4]))
      next
    }
    r <- regmatches(ln, regexec(rule_re, ln))[[1]]
    if (length(r) == 3L) level <- substr(r[3], 1L, 1L)
  }
  out
}

#' Turn per-check results into a verdict, keeping the three failure MODES apart.
#'
#' The modes have different causes and different fixes, so collapsing them would
#' destroy the finding:
#'   * red in the COMMIT and green in the working tree  -> an UNCOMMITTED file
#'     is masking a broken commit. This is the dangerous one.
#'   * red in BOTH -> the commit itself is broken; nothing local explains it.
#'   * green in the commit, red in the working tree -> someone's uncommitted
#'     edit broke it. Not a packaging problem.
#'
#' @param rows list of list(check, committed=list(ok,detail), working=list(ok,detail))
#' @noRd
vct_classify <- function(rows) {
  if (is.null(rows)) rows <- list()
  nm <- vapply(rows, function(r) as.character(r$check %||% "?"), character(1))
  # 🔴 `NA` for "not measured", so the two absences stay apart. Collapsing them
  # with `isTRUE()` first is what made an unmeasured working side look RED:
  # `isTRUE(NULL)` is FALSE, and FALSE is a MEASUREMENT. A check nobody looked
  # at was about to be reported as "broken by uncommitted work".
  tri <- function(x) if (is.null(x)) NA else isTRUE(x)
  craw <- vapply(rows, function(r) tri(r$committed$ok), logical(1))
  wraw <- vapply(rows, function(r) tri(r$working$ok), logical(1))

  # A row is failing if the committed tree is not demonstrably clean. `ok = NULL`
  # (an unparsed check) therefore FAILS: absence of evidence is not evidence.
  cbad <- is.na(craw) | !craw
  wbad <- is.na(wraw) | !wraw
  failing      <- nm[cbad]
  divergent    <- nm[cbad & !is.na(wraw) & wraw]
  locally_bad  <- nm[!cbad & !is.na(wraw) & wbad]

  ok <- length(rows) > 0L && length(failing) == 0L
  # 🔴 `ok` answers ONE question — "does the COMMIT ship?" — because that is what
  # the exit code gates, and a broken local tree does not make a good commit
  # unshippable. MEASURED while writing this: the first version let a red
  # WORKING tree force `ok = FALSE`, which would report a perfectly good commit
  # as broken and send whoever is packaging to hunt a defect in the commit.
  #
  # But it must not read as an unqualified pass either, so the VERDICT string
  # keeps the two states apart: "CLEAN" is only said when both trees agree.
  verdict <- if (length(rows) == 0L) {
    "NO-CHECKS"
  } else if (!ok) {
    "RED"
  } else if (length(locally_bad) > 0L) {
    "CLEAN-COMMIT / LOCAL-RED"
  } else {
    "CLEAN"
  }

  explanation <- if (length(divergent) == 0L) {
    "no divergence between the committed tree and the working tree"
  } else {
    paste0("the working tree hides ", length(divergent),
           " failing check(s) via UNCOMMITTED file(s): ",
           paste(divergent, collapse = ", "),
           ". The commit itself does not pass; the local tree only appears to.")
  }
  list(ok = ok, verdict = verdict, failing = failing, divergent = divergent,
       locally_broken = locally_bad, explanation = explanation, rows = rows,
       names = nm)
}

#' The report a reader can act on WITHOUT re-running anything.
#' @noRd
vct_format_report <- function(v, ref = "HEAD") {
  if (is.null(v)) v <- vct_classify(list())
  mark <- function(ok) if (isTRUE(ok)) "PASS" else "FAIL"
  L <- c(
    sprintf("  committed-tree verification  ref = %s", ref),
    sprintf("  VERDICT: %s", v$verdict),
    "",
    "  check                committed  working",
    "  -------------------  ---------  -------")
  for (i in seq_along(v$rows)) {
    r <- v$rows[[i]]
    L <- c(L, sprintf("  %-19s  %-9s  %s", as.character(r$check),
                      mark(isTRUE(r$committed$ok)), mark(isTRUE(r$working$ok))))
    for (f in r$committed$findings) L <- c(L, sprintf("        %s", f))
    # 🔴 A FAIL with no stated reason is an instrument that cannot be trusted:
    # the reader is left to guess, and a guess about a gate is how a real
    # failure gets filed as noise. So every non-PASS side carries its counts.
    if (!isTRUE(r$committed$ok) || !isTRUE(r$working$ok)) {
      L <- c(L, sprintf("        committed: %s | working: %s",
                        r$committed$detail %||% "?",
                        r$working$detail %||% "?"))
    }
  }
  if (length(v$divergent) > 0L) {
    L <- c(L, "", sprintf("  DIVERGENT (red in the commit, green locally): %s",
                          paste(v$divergent, collapse = ", ")))
  }
  if (length(v$locally_broken) > 0L) {
    L <- c(L, "", sprintf("  broken by UNCOMMITTED work: %s",
                          paste(v$locally_broken, collapse = ", ")))
  }
  L <- c(L, "", paste0("  ", v$explanation))
  if (!isTRUE(v$ok)) {
    L <- c(L, "",
           "  The committed tree is NOT gate-clean. This is REPORTED, not fixed:",
           "  the missing i18n key lives in a FOREIGN file and is deliberately",
           "  left alone.")
  }
  paste(L, collapse = "\n")
}

# --- Execution layer --------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Run a command, capturing BOTH its output and its real exit status.
#'
#' 🔴 MEASURED, and it is the defect this whole tool is about, committed to its
#' own source. `system2(..., stdout = TRUE, stderr = TRUE)` returns the output
#' as a character vector and **NO `status` attribute** — it is NULL. The first
#' version read `attr(o, "status") %||% 0L`, so:
#'
#'   * `mcp --check` reported PASS for ANY outcome, because a missing status
#'     became 0 by the `??` fallback — a gate that cannot fail;
#'   * the new preflight refused EVERY run with "exited NA".
#'
#' Nothing in the unit suite noticed until a test exercised the real `git`
#' path, and nothing in a manual run would have: the earlier mcp result was
#' genuinely 0, so the lie matched the truth by luck.
#'
#' `system2` DOES return the status when the streams are redirected to FILES.
#' So: two temp files, read them back, return both. Same lesson as the
#' `git archive | tar` corruption — a channel has to be proven, not assumed.
#' @noRd
vct_run_cmd <- function(cmd, args) {
  so <- tempfile(); se <- tempfile()
  on.exit(unlink(c(so, se), force = TRUE), add = TRUE)
  st <- suppressWarnings(tryCatch(
    system2(cmd, args, stdout = so, stderr = se),
    error = function(e) 127L))
  read1 <- function(f) if (file.exists(f)) paste(readLines(f, warn = FALSE), collapse = "\n") else ""
  list(out = paste(read1(so), read1(se), collapse = "\n"), status = as.integer(st))
}

#' Run git and hand back its output AND its status.
#' @noRd
vct_git_probe <- function(...) vct_run_cmd("git", c(...))

#' Refuse EARLY, and say why.
#'
#' 🔴 MEASURED, and this is the tool's own principle applied to itself. Running
#' the tool from inside an already-extracted `git archive` dies with
#' "git archive HEAD failed (status 128)" — true, and useless: the cause is that
#' the directory is not a repository, and 128 is git's generic "fatal". A gate
#' whose failure mode is an opaque number cannot be debugged from a CI log, and
#' the same file already refuses to print a FAIL without a reason.
#'
#' @param ref The ref to verify.
#' @param probe Injection point for the unit tests; defaults to real git.
#' @return NULL when fine, else a one-sentence reason.
#' @noRd
vct_archive_preflight <- function(ref = "HEAD", probe = vct_git_probe) {
  gd <- probe("rev-parse", "--git-dir")
  if (!identical(gd$status, 0L)) {
    return(sprintf(paste0(
      "not inside a git repository (`git rev-parse --git-dir` exited %s). This tool ",
      "measures a COMMITTED tree by extracting `git archive <ref>`, so it must run ",
      "in the repository - not inside an already-extracted archive, which has no ref ",
      "to measure."), if (is.na(gd$status)) "NA" else gd$status))
  }
  vr <- probe("rev-parse", "--verify", paste0(ref, "^{commit}"))
  if (!identical(vr$status, 0L)) {
    return(sprintf("ref `%s` does not resolve to a commit (`git rev-parse --verify` exited %s).",
                   ref, if (is.na(vr$status)) "NA" else vr$status))
  }
  NULL
}

#' The repository root, as git itself sees it.
#' @noRd
vct_repo_root <- function(probe = vct_git_probe) {
  r <- probe("rev-parse", "--show-toplevel")
  if (!identical(r$status, 0L)) return(NULL)
  out <- trimws(r$out)
  if (!nzchar(out)) return(NULL)
  out
}

#' Extract a ref into `dest` with `git archive` + `unzip`.
#'
#' 🔴 NEVER `git archive <ref> | tar -x` through a shell. MEASURED on this host:
#' the PowerShell pipeline re-encoded the stream and tar reported
#' "Damaged tar archive (bad header checksum)" until it gave up, leaving ZERO
#' files extracted — and a verifier that then "measures" an empty directory
#' would report a nonsense result (it did: exit 0xC0000005 on an empty tree).
#' `--format=zip` plus `unzip()` is binary-safe in both shells.
#'
#' 🔴 `git -C <root>` IS NOT OPTIONAL, and this was MEASURED, not reasoned. Run
#' from a SUBDIRECTORY, `git archive HEAD` archives THAT SUBDIRECTORY ONLY: from
#' `tests/testthat` it produced 161 files and no `app.R`, where the full tree is
#' 345. The committed tree being graded would then depend on the caller's
#' working directory — the worst possible failure for a gate, because a partial
#' tree can contain no violation and produce a FALSE PASS. `testthat` runs its
#' tests with the working directory set to `tests/testthat`, so the unit test
#' caught what a manual run from the root never could.
#' @noRd
vct_archive <- function(ref, dest) {
  why <- vct_archive_preflight(ref)
  if (!is.null(why)) stop(why, call. = FALSE)
  root <- vct_repo_root()
  if (is.null(root)) {
    stop("cannot determine the repository root (`git rev-parse --show-toplevel` failed)",
         call. = FALSE)
  }
  unlink(dest, recursive = TRUE, force = TRUE)
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  zip <- file.path(tempdir(), paste0("vct_", gsub("[^A-Za-z0-9]", "_", ref), ".zip"))
  unlink(zip, force = TRUE)
  st <- system2("git", c("-C", shQuote(root), "archive", "--format=zip",
                         "-o", shQuote(zip), shQuote(ref)),
                stdout = FALSE, stderr = FALSE)
  if (!identical(as.integer(st), 0L) || !file.exists(zip)) {
    stop(sprintf("git archive %s failed (status %s)", ref, st), call. = FALSE)
  }
  utils::unzip(zip, exdir = dest)
  unlink(zip, force = TRUE)
  n <- length(list.files(dest, recursive = TRUE, all.files = TRUE, no.. = TRUE))
  if (n == 0L) stop(sprintf("git archive %s extracted ZERO files", ref), call. = FALSE)
  # 🔴 REFUSE A PARTIAL TREE. If `app.R` is absent, the archive was taken from a
  # subdirectory (see above) and every gate would then run against a fragment.
  # Better to stop than to certify a fragment.
  if (!file.exists(file.path(dest, "app.R"))) {
    stop(sprintf(paste0("the archive of %s has no app.R, so it is NOT the project root ",
                       "(%d files extracted) - refusing to grade a partial tree"),
                 ref, n), call. = FALSE)
  }
  n
}

vct_rscript <- function() {
  # The project's R, not whatever is on PATH: an Rscript from another
  # installation would activate another renv and measure the wrong library.
  cand <- c("D:/Data_science/R-4.4.2/bin/Rscript.exe",
            file.path(R.home("bin"), "Rscript.exe"),
            file.path(R.home("bin"), "Rscript"))
  for (p in cand) if (file.exists(p)) return(p)
  "Rscript"
}

#' Run one gate inside `dir` and parse it. Never mutates the tree.
#' @noRd
vct_run_check <- function(dir, kind) {
  rscript <- vct_rscript()
  owd <- setwd(dir)
  on.exit(setwd(owd), add = TRUE)
  out <- switch(kind,
    "conventions" = {
      r <- vct_run_cmd(rscript, c("tools/check_conventions.R"))
      vct_parse_gate_output(r$out, exit_status = r$status)
    },
    "duplication" = {
      r <- vct_run_cmd(rscript, c("tools/check_duplication.R", "modules", "R", ".",
                                  "--ext=R", "--fail-on-warning"))
      # 🔴 `--fail-on-warning` makes this exit 1 for the THREE standing
      # `observeEvent` warnings with ZERO errors. The exit code is a POLICY flag
      # here, not a verdict; the parsed error count is the fact. Keying on the
      # status would report a clean gate as red on every single run.
      vct_parse_gate_output(r$out, exit_status = r$status)
    },
    "mcp_check" = {
      # `--no-init-file` is REQUIRED: the project .Rprofile writes two lines to
      # stdout at start-up, which would corrupt the check's own output.
      r <- vct_run_cmd(rscript, c("--no-init-file", "scripts/mcp_server.R", "--check"))
      list(errors = if (identical(r$status, 0L)) 0L else 1L, warnings = 0L,
           findings = character(0), unparsed = FALSE, blocking = r$status != 0L,
           exit_status = r$status)
    },
    stop("unknown check kind: ", kind, call. = FALSE)
  )
  # A gate that could not even START (missing file, unparsable) must block, and
  # must say so in the detail rather than looking like a pass.
  if (isTRUE(out$unparsed)) {
    out$detail <- "gate output could not be parsed (see vct_parse_gate_output)"
  } else {
    out$detail <- sprintf("%d error(s), %d warning(s)", out$errors, out$warnings)
  }
  out
}

#' Assemble ONE row from two parsed gate results.
#'
#' 🔴 THIS FUNCTION IS THE TESTED ONE, and it is here rather than inline in
#' `vct_build_rows()` because that is exactly where the first version of this
#' script had the bug that mattered. `vct_run_check()` returns a PARSED result
#' carrying `errors` / `warnings` / `unparsed` / `blocking` — and it has NO `ok`
#' field. The row builder read `wk$ok`, so it was always NULL, so every working
#' side came out as `isTRUE(NULL)` = FALSE.
#'
#' MEASURED consequence: the tool reported `duplication` and `mcp --check` as
#' **FAIL in the working tree** while printing their real counts — "0 error(s),
#' 3 warning(s)" on a FAIL line. It announced a broken checkout that was green,
#' i.e. it manufactured a false finding in the one place a verifier must not
#' lie. A missing field read as a value is silent; a wrong value is not.
#'
#' So the mapping from a parsed result to a verdict lives HERE, where a unit
#' test can pin it, and `ok = NULL` (not measured) stays distinguishable from
#' `ok = FALSE` (measured red).
#' @noRd
vct_make_row <- function(check, cparsed, wparsed) {
  side <- function(x) {
    if (is.null(x)) {
      return(list(ok = NULL, detail = "not measured", findings = character(0)))
    }
    # `blocking = NULL` means "we could not tell" and must stay NULL.
    ok <- if (is.null(x$blocking)) NULL else !isTRUE(x$blocking)
    list(ok = ok, detail = x$detail %||% "?", findings = x$findings %||% character(0))
  }
  c1 <- side(cparsed)
  w1 <- side(wparsed)
  list(check = check, committed = c1, working = w1)
}

vct_build_rows <- function(dir, with_working, root) {
  kinds <- c(conventions = "conventions", duplication = "duplication",
             `mcp --check` = "mcp_check")
  same <- identical(normalizePath(dir), normalizePath(root))
  rows <- lapply(names(kinds), function(nm) {
    cp <- vct_run_check(dir, kinds[[nm]])
    wp <- if (isTRUE(with_working) && !same) vct_run_check(root, kinds[[nm]]) else NULL
    vct_make_row(nm, cp, wp)
  })
  names(rows) <- names(kinds)
  rows
}

#' The process exit status: the tool's only contract with CI.
#'
#' 🔴 THIS IS A SEPARATE FUNCTION BECAUSE IT WAS NOT TESTED. Mutation T10
#' replaced the return with a literal `0L` and the suite stayed GREEN: the line
#' lived inside `vct_main()`, which the unit tests never call (it archives a ref
#' and runs three real gates). A packaging gate whose exit status is always 0 is
#' silently useless in automation — it cannot fail a pipeline, and nobody would
#' notice until a broken commit shipped. The decision is trivial; that is
#' precisely why it must be pinned rather than left inline.
#' @noRd
vct_exit_status <- function(v) if (isTRUE(v$ok)) 0L else 1L

vct_main <- function(argv) {
  ref <- if (length(argv)) argv[[1]] else "HEAD"
  with_working <- !("--no-work" %in% argv)
  # 🔴 The working-tree baseline is GIT's project root, NOT `getwd()`. MEASURED
  # by running the tool from `tests/`: with `normalizePath(".")` the local gates
  # were executed inside `tests/`, could not find `tools/check_conventions.R`,
  # and reported duplication and mcp --check as FAIL — a verdict that depends on
  # the caller's shell. A gate whose answer changes with the directory you typed
  # it from is not a measurement.
  root <- vct_repo_root()
  if (is.null(root)) {
    stop("not inside a git repository: nothing to verify", call. = FALSE)
  }
  dest <- file.path(tempdir(), "vct_committed")
  n <- vct_archive(ref, dest)
  cat(sprintf("  extracted %d files from `git archive %s`\n", n, ref))
  cat(sprintf("  project root: %s\n", root))
  if (!identical(normalizePath(".", winslash = "/", mustWork = FALSE), root)) {
    cat(sprintf("  (invoked from %s - the project root is used for every gate)\n",
                normalizePath(".", winslash = "/", mustWork = FALSE)))
  }
  cat("\n")
  rows <- vct_build_rows(dest, with_working, root)
  v <- vct_classify(rows)
  cat(vct_format_report(v, ref = ref), "\n\n")
  unlink(dest, recursive = TRUE, force = TRUE)
  vct_exit_status(v)
}

# --- main guard -------------------------------------------------------------
# Same shape as `tools/check_conventions.R` and `tools/check_duplication.R`, so
# `sys.source()` from a test has NO side effect. Without `sys.nframe() == 0L`
# the guard would fire on source and the unit tests would run real gates.
if (identical(environment(), globalenv()) && sys.nframe() == 0L &&
    !interactive() && length(grep("--file=", commandArgs(trailingOnly = FALSE))) > 0) {
  quit(status = vct_main(commandArgs(trailingOnly = TRUE)), save = "no")
}

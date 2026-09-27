# =============================================================================
# test-verify-committed-tree.R — the PACKAGING gate
# =============================================================================
# WHAT THIS TOOL IS FOR. Every other check in this repo measures the WORKING
# TREE. That is a real gap, and MEASURED on 2026-09-27: `check_conventions.R`
# reports 0 errors in the working tree and 1 error in the committed tree, at the
# same commit, on the same machine. The difference is a single i18n key that
# exists only in an UNTRACKED `i18n/translation.json`.
#
# So a green working tree is not evidence that a COMMIT ships. This tool
# measures the COMMITTED TREE (via `git archive`, never the checkout) and, when
# the two disagree, says so instead of picking the flattering one.
#
# It FIXES NOTHING, by design. The C7 key is left exactly as it is; the tool's
# only output is a verdict and the evidence behind it.

# `ts_project_root()` comes from `tests/testthat/helper-source.R`, which every
# test file already loads. It is deliberately NOT redefined here: a test that
# shipped its own root-finder would be measuring its own idea of the project.

# 🔴 THE TOOL DOES NOT EXIST YET, and this harness turns that into a RED THAT
# MEANS SOMETHING. A direct call to a missing symbol raises "object not found"
# and the file reports ERROR; an ERROR says "the test is broken", not "the
# feature is missing" (CONVENTIONS.md). So every call goes through `.vct()`,
# which returns NULL when the tool or the function is absent, and each test
# opens by asserting the SHAPE it expects. A NULL then fails cleanly, and once
# the tool exists the assertions run in full — so the green proves the whole
# contract rather than its first link.
.vct_env <- function() {
  f <- file.path(ts_project_root(), "tools/verify_committed_tree.R")
  if (!file.exists(f)) return(NULL)
  e <- new.env(parent = globalenv())
  # `sys.source`, NOT executed: the tool's `main()` is guarded by
  # `sys.nframe() == 0L`, the same guard `check_conventions.R` and
  # `check_duplication.R` use, so sourcing has no side effect. A test that ran
  # the real gates would measure the gates, not the classifier.
  sys.source(f, envir = e)
  e
}

.vct <- function(name, ...) {
  e <- .vct_env()
  if (is.null(e)) return(NULL)
  fn <- e[[name]]
  if (!is.function(fn)) return(NULL)
  fn(...)
}

# ---------------------------------------------------------------------------
# Parsing a gate's own words — never its exit code alone
# ---------------------------------------------------------------------------
# 🔴 WHY PARSE AND DO NOT TRUST `$?`: `check_duplication.R` is documented to run
# with `--fail-on-warning`, and it returns exit 1 for the THREE PRE-EXISTING
# `observeEvent` warnings — with ZERO errors. A verifier keyed on the exit code
# would report a clean gate as red, every time, and an agent would learn to
# ignore it. The ERROR COUNT is the fact; the exit code is a policy flag.
# 🔴 `paste(..., collapse = "\n")`, NOT `paste0(...)`. `paste0` concatenates with
# NO separator, so the first version of this fixture handed the parser a single
# 200-character line. The line-oriented harvest then legitimately found nothing,
# and the run looked like a tool bug. A fixture that is not shaped like real
# output will produce a false finding, which is why the summary line, the rule
# table and the indented site line each need their own line.
GATE_OK <- paste0("---- Resume : 0 erreur(s), 3 avertissement(s) ----")
# 🔴 The C10 WARNING rule carries a DETAIL LINE of its own, on purpose. Without
# one, "warning-level findings are not harvested" was VACUOUS: the harvest would
# return the same single C7 site whether or not it filtered by level, so the
# assertion could not fail. A rule only exercises the filter if it HAS findings
# to filter. (Real `check_conventions.R` output does print indented sites under
# warning rules, so this is faithful to the format, not a contrivance.)
GATE_C7 <- paste(
  "---- Resume : 1 erreur(s), 59 avertissement(s) ----",
  "C1    ERREUR   R/modules/ ne doit pas exister            OK (0)",
  "C7    ERREUR   toute cle tr() existe dans i18n           1 signalement(s)",
  "  C7   modules/spatial/mod_spatial_qc.R:1059",
  "C10   AVERT.   stop() classe ou call. = FALSE           1 signalement(s)",
  "  C10  R/bulk/bulk_helpers.R:512",
  "C13   ERREUR   choices nomme                            OK (0)",
  sep = "\n")

test_that("le verifier existe et rend un resultat exploitable", {
  e <- .vct_env()
  expect_true(is.environment(e), "tools/verify_committed_tree.R est absent ou illisible")
  if (!is.environment(e)) return()
  for (fn in c("vct_parse_gate_output", "vct_classify", "vct_format_report")) {
    expect_true(is.function(e[[fn]]), info = paste("fonction absente:", fn))
  }
})

test_that("vct_parse_gate_output lit les ERREURS, pas le code de sortie", {
  ok <- .vct("vct_parse_gate_output", GATE_OK)
  expect_true(is.list(ok))
  if (!is.list(ok)) return()
  expect_identical(ok$errors, 0L)
  expect_identical(ok$warnings, 3L)
  expect_length(ok$findings, 0L)

  # The same text WITH exit status 1 — the duplication baseline. Still clean.
  dup <- .vct("vct_parse_gate_output", GATE_OK, exit_status = 1L)
  expect_true(is.list(dup))
  if (!is.list(dup)) return()
  expect_identical(dup$errors, 0L, "3 warnings + exit 1 is NOT an error")
  expect_identical(dup$blocking, FALSE)

  red <- .vct("vct_parse_gate_output", GATE_C7)
  expect_true(is.list(red))
  if (!is.list(red)) return()
  expect_identical(red$errors, 1L)
  expect_identical(red$warnings, 59L)
  expect_identical(red$blocking, TRUE)
})

test_that("vct_parse_gate_output isole le SITE de chaque erreur, et rien d'autre", {
  red <- .vct("vct_parse_gate_output", GATE_C7)
  expect_true(is.list(red))
  if (!is.list(red)) return()
  # exactly ONE site, and it is the C7 one. C13 is an ERREUR rule that reports
  # OK, so it must contribute nothing either.
  expect_length(red$findings, 1L)
  expect_match(red$findings[1], "mod_spatial_qc.R:1059", fixed = TRUE)
  expect_match(red$findings[1], "C7")
  # 🔴 a WARNING-level rule is not an error. The fixture's C10 carries a REAL
  # detail line, so this assertion now bites: harvest it anyway and this goes
  # red. Without the line the assertion was vacuous.
  expect_identical(any(grepl("C10", red$findings, fixed = TRUE)), FALSE)
  expect_identical(any(grepl("bulk_helpers.R", red$findings, fixed = TRUE)), FALSE)
  # and the OK rules must not appear either
  expect_identical(any(grepl("C1 ", red$findings, fixed = TRUE)), FALSE)
  expect_identical(any(grepl("C13", red$findings, fixed = TRUE)), FALSE)
})

test_that("vct_parse_gate_output refuse de pretendre avoir compris", {
  # No parsable summary at all: the honest answer is "unknown", which must NOT
  # be read as green. A tool that defaults to OK on a parse failure is a tool
  # that hides outages.
  junk <- .vct("vct_parse_gate_output", "Error: something went very wrong\n")
  expect_true(is.list(junk))
  if (!is.list(junk)) return()
  expect_identical(junk$unparsed, TRUE)
  expect_null(junk$errors)
  expect_null(junk$warnings)
  expect_identical(junk$blocking, TRUE, "unparsed output must BLOCK, never pass silently")
})

# ---------------------------------------------------------------------------
# Classification: committed vs working
# ---------------------------------------------------------------------------
.row <- function(name, committed, working) {
  list(check = name,
       committed = list(ok = committed, detail = ""),
       working   = list(ok = working,   detail = ""))
}

test_that("vct_classify dit RED quand l'ARBRE COMME ne l'est pas", {
  v <- .vct("vct_classify", list(
    .row("mcp --check",  TRUE,  TRUE),
    .row("conventions",  FALSE, TRUE)))   # committed red, working green
  expect_true(is.list(v))
  if (!is.list(v)) return()
  expect_identical(v$ok, FALSE)
  expect_match(v$verdict, "RED")
  # 🔴 AND it must name the DIVERGENCE, because that is the whole finding:
  # a green working tree is hiding a broken commit.
  expect_length(v$divergent, 1L)
  expect_identical(v$divergent[1], "conventions")
  expect_match(v$explanation, "uncommitted", ignore.case = TRUE)
})

test_that("vct_classify distingue 'casse par un commit' de 'casse par le travail local'", {
  # red in BOTH: the commit itself is broken — no uncommitted file explains it
  both <- .vct("vct_classify", list(.row("conventions", FALSE, FALSE)))
  expect_true(is.list(both))
  if (!is.list(both)) return()
  expect_identical(both$ok, FALSE)
  expect_length(both$divergent, 0L)
  expect_length(both$locally_broken, 0L)

  # green in the commit, red in the working tree: someone's UNCOMMITTED edit
  # broke it. Different diagnosis, different fix, and it must NOT be reported as
  # a broken commit — a good commit does not become unshippable because the
  # checkout is dirty, and `ok` is what drives the packaging exit code.
  #
  # 🔴 The VERDICT still refuses to say "CLEAN" in that case. If it did, a
  # reader would take away "everything is fine" and be wrong about one tree.
  local_broken <- .vct("vct_classify", list(.row("conventions", TRUE, FALSE)))
  expect_true(is.list(local_broken))
  if (!is.list(local_broken)) return()
  expect_identical(local_broken$ok, TRUE, "the COMMIT still ships")
  expect_match(local_broken$verdict, "LOCAL-RED")
  expect_length(local_broken$divergent, 0L)
  expect_length(local_broken$locally_broken, 1L)
  expect_identical(local_broken$locally_broken[1], "conventions")
})

test_that("vct_classify ne fabrique ni ne masque un controle absent", {
  # An unparsed check BLOCKS (see above), so it must appear in the verdict's
  # failing list rather than being skipped.
  v <- .vct("vct_classify", list(
    list(check = "conventions", committed = list(ok = NULL, detail = "unparsed"),
         working = list(ok = NULL, detail = "unparsed"))))
  expect_true(is.list(v))
  if (!is.list(v)) return()
  expect_identical(v$ok, FALSE)
  expect_true("conventions" %in% v$failing)

  # An EMPTY run is not a pass. Zero checks means the verifier proved nothing,
  # and reporting that as clean is the same lie as parsing a failure as green.
  empty <- .vct("vct_classify", list())
  expect_true(is.list(empty))
  if (!is.list(empty)) return()
  expect_identical(empty$ok, FALSE)
  expect_match(empty$verdict, "NO-CHECKS")
})

test_that("vct_classify est vert seulement si TOUT l'est", {
  v <- .vct("vct_classify", list(.row("a", TRUE, TRUE), .row("b", TRUE, TRUE),
                                 .row("c", TRUE, TRUE)))
  expect_true(is.list(v))
  if (!is.list(v)) return()
  expect_identical(v$ok, TRUE)
  expect_match(v$verdict, "CLEAN")
  expect_length(v$failing, 0L)
  expect_length(v$divergent, 0L)
  expect_length(v$locally_broken, 0L)
})

test_that("vct_make_row mappe un resultat PARSE, et n'invente pas de champ", {
  # 🔴 CETTE LIGNE EXISTE PARCE QUE L'OUTIL A FABRIQUE UN FAUX SIGNAL.
  # `vct_run_check()` renvoie un résultat PARSÉ (`errors`/`warnings`/
  # `unparsed`/`blocking`) et n'a AUCUN champ `ok`. La première version lisait
  # `wk$ok` : toujours NULL, donc `isTRUE(NULL)` = FALSE, donc « FAIL » sur
  # l'ARBRE DE TRAVAIL — avec des compteurs parfaitement corrects affichés sur
  # la ligne (« 0 error(s), 3 warning(s) »). Elle annonçait un checkout cassé
  # qui était vert, sur un outil dont le seul travail est de ne PAS mentir.
  e <- .vct_env()
  parsed_ok <- list(errors = 0L, warnings = 3L, findings = character(0),
                    unparsed = FALSE, blocking = FALSE, detail = "0 error(s), 3 warning(s)")
  row <- .vct("vct_make_row", "duplication", parsed_ok, parsed_ok)
  expect_true(is.list(row))
  if (!is.list(row)) return()
  expect_identical(row$committed$ok, TRUE)
  expect_identical(row$working$ok, TRUE, "0 erreurs + exit 1 n'est PAS un echec")
  expect_match(row$working$detail, "0 error(s)", fixed = TRUE)

  # A genuinely red parse still reads red — the mapping is not a rubber stamp.
  parsed_bad <- parsed_ok; parsed_bad$errors <- 1L; parsed_bad$blocking <- TRUE
  bad <- .vct("vct_make_row", "conventions", parsed_bad, parsed_ok)
  expect_identical(bad$committed$ok, FALSE)
  expect_identical(bad$working$ok, TRUE)
})

test_that("vct_make_row distingue 'non mesure' de 'mesure rouge'", {
  # `working = NULL` is "we did not look" and must stay NULL, so a check that
  # was never measured cannot be counted as a divergence, nor reported red.
  e <- .vct_env()
  parsed_ok <- list(errors = 0L, warnings = 0L, findings = character(0),
                    unparsed = FALSE, blocking = FALSE, detail = "0 error(s)")
  row <- .vct("vct_make_row", "mcp --check", parsed_ok, NULL)
  expect_true(is.list(row))
  if (!is.list(row)) return()
  expect_null(row$working$ok)
  expect_match(row$working$detail, "not measured", fixed = TRUE)
  v <- .vct("vct_classify", list(row))
  # The COMMITTED side passed, so the commit ships and nothing diverges.
  expect_identical(v$ok, TRUE)
  expect_length(v$divergent, 0L)
  expect_length(v$locally_broken, 0L)
})

test_that("vct_format_report montre la RAISON d'un echec, pas seulement FAIL", {
  # A FAIL line with no stated reason sends the reader to guess, and a guess
  # about a gate is how a real failure gets filed as noise.
  parsed_bad <- list(errors = 1L, warnings = 59L,
                     findings = "C7 modules/spatial/mod_spatial_qc.R:1059",
                     unparsed = FALSE, blocking = TRUE,
                     detail = "1 error(s), 59 warning(s)")
  row <- .vct("vct_make_row", "conventions", parsed_bad, NULL)
  v <- .vct("vct_classify", list(row))
  txt <- .vct("vct_format_report", v, ref = "da652d7")
  expect_true(is.character(txt) && length(txt) == 1L && nzchar(txt))
  if (!is.character(txt) || length(txt) != 1L || !nzchar(txt)) return()
  expect_match(txt, "1 error(s), 59 warning(s)", fixed = TRUE)
  expect_match(txt, "mod_spatial_qc.R:1059", fixed = TRUE)
  # and it must state the standing refusal to "fix" it
  expect_match(txt, "NOT gate-clean", fixed = TRUE)
  expect_match(txt, "FOREIGN", fixed = TRUE)
})

test_that("le code de SORTIE est le seul contrat de l'outil avec une CI", {
  # 🔴 Cette fonction existe parce que la mutation T10 a MIS SON ANCHRE
  # DANS LE VERT : la ligne etait dans `vct_main()`, que les tests n'appellent
  # jamais (elle archive un ref et lance trois vraies portes). Remplacer son
  # retour par `0L` laissait la suite VERTE. Une porte de packaging dont le
  # statut de sortie vaut toujours 0 est muette en automatisation, et personne
  # ne le remarke avant qu'un commit casse parte en production.
  v <- .vct("vct_classify", list(.row("conventions", FALSE, FALSE)))
  expect_identical(.vct("vct_exit_status", v), 1L)
  clean <- .vct("vct_classify", list(.row("conventions", TRUE, TRUE)))
  expect_identical(.vct("vct_exit_status", clean), 0L)
  # et un verdict absent, ou une liste vide, ne vaut PAS 0
  expect_identical(.vct("vct_exit_status", NULL), 1L)
  empty <- .vct("vct_classify", list())
  expect_identical(.vct("vct_exit_status", empty), 1L)
})

test_that("le rapport nomme le controle, son statut et sa divergence", {
  v <- .vct("vct_classify", list(
    .row("mcp --check",  TRUE,  TRUE),
    .row("conventions",  FALSE, TRUE)))
  txt <- .vct("vct_format_report", v, ref = "da652d7")
  expect_true(is.character(txt) && length(txt) == 1L && nzchar(txt))
  if (!is.character(txt) || length(txt) != 1L || !nzchar(txt)) return()
  # the reader must be able to act on it without re-running anything
  expect_match(txt, "da652d7", fixed = TRUE)
  expect_match(txt, "mcp --check", fixed = TRUE)
  expect_match(txt, "conventions", fixed = TRUE)
  expect_match(txt, "DIVERG", ignore.case = TRUE)
  # and it must NOT read as a success
  expect_identical(grepl("ALL GREEN", txt, fixed = TRUE), FALSE)
})

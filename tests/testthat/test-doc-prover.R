# =====================================================================
# test-doc-prover.R — jalon 4 (slice S5 N6 doc-prover) — moteur PUR.
#
# Cible : tools/satellites/doc_prover/prover_lib.R, sourcé par chemin
# relatif au PRÉSENT fichier de test (../../tools/satellites/doc_prover/),
# avec repli cwd-racine pour testthat::test_file lancé depuis la racine.
#
# Règle maison RED→GREEN : le ROUGE = failures d'expectation, error = 0.
#
# Rien de session-dépendant : chaque claim de test est évalué sur un
# mini-arbre planté dans tempdir() (les checkers résolvent tout via
# ctx$repo_root) ; les faits non-fichier (git) sont INJECTÉS via ctx
# (jamais de subprocess dans les tests) ; l'horloge est un argument.
# Le satellite n'est PAS sourcé par l'app — 0 app change, par construction.
# =====================================================================

.prover_lib_candidates <- c(
  "../../tools/satellites/doc_prover/prover_lib.R", # relatif au présent fichier (tests/testthat/)
  "tools/satellites/doc_prover/prover_lib.R"        # variante cwd = racine du dépôt
)
.prover_lib_hit <- .prover_lib_candidates[file.exists(.prover_lib_candidates)]
if (length(.prover_lib_hit) != 1L) {
  stop("prover_lib.R introuvable ni relatif au test ni depuis la racine")
}
source(normalizePath(.prover_lib_hit), local = FALSE)

# ---- fabrique de mini-arbres -----------------------------------------------

.prover_tmp_root <- function(tag) {
  root <- file.path(tempdir(), sprintf("prover-test-%s-%s-%d", tag,
                                       format(Sys.time(), "%Y%m%d%H%M%S"),
                                       sample.int(1e9, 1)))
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  root
}

.prover_plant <- function(path, lines) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path, useBytes = TRUE)
}

# plante un contenu démarrant à la ligne `at` (remplissage de lignes vides)
.prover_at <- function(at, lines) {
  c(rep("", max(0L, at - 1L)), lines)
}

# ---- claims de test ---------------------------------------------------------

.prover_claim_launcher <- function() {
  list(id = "static-launcher", type = "static",
       doc = "docs/mcp/DRIVE_SESSION_RUNBOOK.md", line = 146L,
       assertion = "tools/launch_dev_drive.R existe et sa ligne 4 porte « The ONLY supported way »",
       expected = "tools/launch_dev_drive.R:4 contient la citation",
       evidence_note = "claim de test — arbre planté")
}

.prover_claim_banner <- function() {
  list(id = "static-anchor-mcp-banner", type = "static",
       doc = "docs/mcp/DRIVE_SESSION_RUNBOOK.md", line = 19L,
       assertion = "les lignes 104-107 de scripts/mcp_server.R nomment les trois outils read-only cités",
       expected = "status + read_result + snapshot tous présents dans la fenêtre 104-107",
       evidence_note = "claim de test — arbre planté (fenêtre ré-ancrée passe doc 2026-11-04)")
}

.prover_claim_commits <- function() {
  list(id = "static-commits-exist", type = "static",
       doc = "docs/mcp/DRIVE_SESSION_RUNBOOK.md", line = 193L,
       assertion = "les commits cités par le runbook existent",
       expected = "chaque hash résout un objet git de type commit",
       evidence_note = "faits git INJECTÉS via ctx$git_cat — aucun subprocess en test")
}

.prover_claim_live_hb <- function() {
  list(id = "live-hb-advance", type = "live",
       doc = "docs/mcp/DRIVE_SESSION_RUNBOOK.md", line = 67L,
       assertion = "hb_n avance entre deux lectures espacées d'environ 4 s",
       expected = "hb_n(2e lecture) > hb_n(1re lecture)",
       evidence_note = "sonde INJECTÉE via ctx$probe — jamais de session réelle en test")
}

ctx_min <- function(root) {
  list(repo_root = root, drive_dir = file.path(root, "tools", "_drive"),
       git_cat = NULL, probe = NULL, now = as.POSIXct("2026-11-03T16:37:20Z",
       format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
}

# =============================================================================

test_that("chargement du moteur pur — la surface publique existe", {
  expect_true(exists("evaluate_claim") && is.function(evaluate_claim))
  expect_true(exists("build_report") && is.function(build_report))
  expect_true(exists("classify_drive_state") && is.function(classify_drive_state))
  expect_true(exists("gate_live_ok") && is.function(gate_live_ok))
})

test_that("verdict PASS — claim évalué sur un arbre planté conforme", {
  root <- .prover_tmp_root("pass")
  .prover_plant(file.path(root, "tools", "launch_dev_drive.R"), .prover_at(4L,
    "# The ONLY supported way to start a drive-enabled session (spec §4, G4 #16)."))
  res <- evaluate_claim(.prover_claim_launcher(), ctx_min(root))
  expect_equal(res$id, "static-launcher")
  expect_equal(res$verdict, "pass")
  expect_true(is.character(res$evidence) && nzchar(res$evidence))
})

test_that("verdict FAIL — l'ancre citée ne porte pas la substance (drift)", {
  root <- .prover_tmp_root("fail")
  .prover_plant(file.path(root, "tools", "launch_dev_drive.R"), .prover_at(4L,
    "# autre chose : le contrat header a déménagé"))
  res <- evaluate_claim(.prover_claim_launcher(), ctx_min(root))
  expect_equal(res$verdict, "fail")
  expect_true(nzchar(res$evidence))
})

test_that("verdict FAIL sur fenêtre de lignes — la substance est hors fenêtre citée", {
  # fenêtre ré-ancrée passe doc 2026-11-04 : le runbook réparé cite 104-107
  # (snapshot réellement à la 107). Variante NÉGATIVE : snapshot hors fenêtre
  # (à la 108) -> le checker doit dire fail ; variante conforme -> pass.
  root <- .prover_tmp_root("window")
  .prover_plant(file.path(root, "scripts", "mcp_server.R"), c(.prover_at(104L,
    c('  .ts_stderr("  tools        : transcripto_drive_status, transcripto_drive_read_result")',
      '  .ts_stderr("                 transcripto_drive_state (M2: passive, read-only)")',
      '  .ts_stderr("                 transcripto_drive_datasets (P2 PoC datasets)")',
      '  .ts_stderr("                 fin de la bannière")',
      '  .ts_stderr("                 transcripto_drive_snapshot (M3a: passive, read-only)")')),
    .prover_at(231L, "       structuredContent = structured, isError = FALSE)")))
  res <- evaluate_claim(.prover_claim_banner(), ctx_min(root))
  expect_equal(res$verdict, "fail")
  # et la variante où le runbook aurait raison -> pass :
  root2 <- .prover_tmp_root("window-ok")
  .prover_plant(file.path(root2, "scripts", "mcp_server.R"), .prover_at(104L,
    c('  .ts_stderr("  tools        : transcripto_drive_status, transcripto_drive_read_result")',
      '  .ts_stderr("                 transcripto_drive_snapshot (read-only)")',
      '  .ts_stderr("                 fin")')))
  res2 <- evaluate_claim(.prover_claim_banner(), ctx_min(root2))
  expect_equal(res2$verdict, "pass")
})

test_that("claims malformés -> untested, JAMAIS une erreur", {
  root <- .prover_tmp_root("malformed")
  malformed <- list(
    null_claim = NULL,
    empty = list(),
    no_id = list(type = "static", line = 1L, assertion = "x", expected = "y"),
    bad_type = list(id = "static-launcher", type = "banana", line = 4L,
                    assertion = "x", expected = "y"),
    no_line = list(id = "static-launcher", type = "static",
                   assertion = "x", expected = "y"),
    unknown_id = list(id = "static-nimporte-quoi", type = "static", line = 1L,
                      assertion = "x", expected = "y")
  )
  for (nm in names(malformed)) {
    res <- evaluate_claim(malformed[[nm]], ctx_min(root))
    expect_equal(res$verdict, "untested", info = nm)
    expect_true(is.character(res$evidence) && nzchar(res$evidence), info = nm)
  }
})

test_that("faits indisponibles -> untested honnête (jamais devinés)", {
  root <- .prover_tmp_root("nofacts")
  # commits : ctx$git_cat = NULL -> le checker refuse de conclure
  res <- evaluate_claim(.prover_claim_commits(), ctx_min(root))
  expect_equal(res$verdict, "untested")
  # claims live : pas de sonde -> untested (le gate live n'est pas satisfait)
  res2 <- evaluate_claim(.prover_claim_live_hb(), ctx_min(root))
  expect_equal(res2$verdict, "untested")
  expect_true(grepl("sonde|gate|live", res2$evidence))
})

test_that("faits git injectés — verdicts pass/fail sans subprocess", {
  root <- .prover_tmp_root("git")
  hashes <- c("7cccd6c4af46c75f285dce992ab6dc0c0c756cfe",
              "db12712259cc11da5e6e877e525f095b59fd5fbe",
              "59c488803e3486ca41fccb64bbbe6f6d5df16a1c")
  ctx_ok <- ctx_min(root); ctx_ok$git_cat <- setNames(as.list(c("commit", "commit", "commit")), hashes)
  res <- evaluate_claim(.prover_claim_commits(), ctx_ok)
  expect_equal(res$verdict, "pass")
  ctx_bad <- ctx_min(root); ctx_bad$git_cat <- setNames(as.list(c("commit", "blob", "commit")), hashes)
  res2 <- evaluate_claim(.prover_claim_commits(), ctx_bad)
  expect_equal(res2$verdict, "fail")
})

test_that("sonde live injectée — verdicts pass/fail du claim hb_n", {
  root <- .prover_tmp_root("probe")
  ctx_p <- ctx_min(root)
  ctx_p$probe <- list(fresh = TRUE, hb_advance = TRUE, hb_n1 = 10L, hb_n2 = 12L,
                      age_s = 2.5, hb_timeout_s = 15)
  res <- evaluate_claim(.prover_claim_live_hb(), ctx_p)
  expect_equal(res$verdict, "pass")
  ctx_f <- ctx_min(root)
  ctx_f$probe <- list(fresh = TRUE, hb_advance = FALSE, hb_n1 = 10L, hb_n2 = 10L,
                      age_s = 40, hb_timeout_s = 15)
  res2 <- evaluate_claim(.prover_claim_live_hb(), ctx_f)
  expect_equal(res2$verdict, "fail")
})

test_that("build_report — statut par claim, comptes exacts, verdict global", {
  claims <- list(.prover_claim_launcher(), .prover_claim_commits(),
                 .prover_claim_live_hb())
  results <- list(
    list(id = "static-launcher", verdict = "pass", evidence = "ok"),
    list(id = "static-commits-exist", verdict = "pass", evidence = "ok"),
    list(id = "static-anchor-mcp-banner", verdict = "fail", evidence = "drift"),
    list(id = "live-hb-advance", verdict = "untested", evidence = "pas de sonde")
  )
  rep <- build_report(claims, results)
  expect_equal(rep$counts$pass, 2L)
  expect_equal(rep$counts$fail, 1L)
  expect_equal(rep$counts$untested, 1L)
  expect_equal(length(rep$rows), 4L)
  ids <- vapply(rep$rows, function(r) r$id, character(1))
  expect_setequal(ids, c("static-launcher", "static-commits-exist",
                         "static-anchor-mcp-banner", "live-hb-advance"))
  expect_equal(length(rep$findings), 1L)
  expect_equal(rep$findings[[1]]$id, "static-anchor-mcp-banner")
  expect_equal(rep$verdict_global, "1 findings de drift filés")
  # aucun fail -> runbook complet :
  rep2 <- build_report(claims, results[c(1, 2, 4)])
  expect_equal(rep2$counts$fail, 0L)
  expect_equal(rep2$verdict_global, "runbook ✅ complet")
})

test_that("classify_drive_state — les quatre états du contrat (horloge figée)", {
  iso <- function(s) as.POSIXct(s, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  # (a) heartbeat frais (fixture watchtower « alive », now=16:37:20) :
  ready <- list(hb_at = "2026-11-03T16:37:12Z", hb_n = 341L, hb_timeout_s = 15,
                session_token = "fxalive1", armed = TRUE)
  st <- classify_drive_state(ready, list(status = "done"), iso("2026-11-03T16:37:20Z"))
  expect_equal(st$label, "VIVANT")
  # (b) gelé + running en vol (fixture « frozen-running », now=16:29:00) :
  ready2 <- list(hb_at = "2026-11-03T16:20:03Z", hb_n = 2203L, hb_timeout_s = 15,
                 session_token = "fxfrozen2", armed = TRUE)
  st2 <- classify_drive_state(ready2, list(status = "running"), iso("2026-11-03T16:29:00Z"))
  expect_equal(st2$label, "JOB IN FLIGHT")
  # (c) gelé sans running :
  st3 <- classify_drive_state(ready2, list(status = "done"), iso("2026-11-03T16:29:00Z"))
  expect_equal(st3$label, "STALE — HB GELÉ")
  # (d) pas de ready.json :
  st4 <- classify_drive_state(NULL, list(status = "done"), iso("2026-11-03T16:29:00Z"))
  expect_equal(st4$label, "NO_SESSION")
  # (e) hb_at illisible — la fraîcheur est indécidable, jamais « vivante » :
  st5 <- classify_drive_state(
    list(hb_at = "pas-une-date", hb_n = 1L, hb_timeout_s = 15),
    list(status = "done"), iso("2026-11-03T16:29:00Z"))
  expect_equal(st5$label, "READY.JSON ILLISIBLE")
})

test_that("règle 🔴 — aucun état n'est JAMAIS étiqueté DEAD", {
  iso <- function(s) as.POSIXct(s, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  combos <- list(
    list(ready = NULL, result = NULL),
    list(ready = list(hb_at = "2026-11-03T16:20:03Z", hb_timeout_s = 15),
         result = list(status = "running")),
    list(ready = list(hb_at = "2026-11-03T16:20:03Z", hb_timeout_s = 15),
         result = list(status = "done")),
    list(ready = list(hb_at = "2026-11-03T16:37:12Z", hb_timeout_s = 15),
         result = list(status = "running"))
  )
  labels <- vapply(combos, function(cmb) {
    classify_drive_state(cmb$ready, cmb$result, iso("2026-11-03T16:29:00Z"))$label
  }, character(1))
  expect_false(any(grepl("DEAD", labels, fixed = TRUE)))
  expect_false(any(is.na(labels)))
})

test_that("fonctions pures — même entrée, même sortie (double appel)", {
  root <- .prover_tmp_root("pure")
  .prover_plant(file.path(root, "tools", "launch_dev_drive.R"), .prover_at(4L,
    "# The ONLY supported way to start a drive-enabled session (spec §4, G4 #16)."))
  c1 <- .prover_claim_launcher(); ctx1 <- ctx_min(root)
  expect_identical(evaluate_claim(c1, ctx1), evaluate_claim(c1, ctx1))
  iso <- function(s) as.POSIXct(s, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ready <- list(hb_at = "2026-11-03T16:37:12Z", hb_timeout_s = 15)
  expect_identical(classify_drive_state(ready, list(status = "done"), iso("2026-11-03T16:37:20Z")),
                   classify_drive_state(ready, list(status = "done"), iso("2026-11-03T16:37:20Z")))
})

test_that("gate live — prédicat pur sur la sonde, jamais sur la session", {
  expect_false(gate_live_ok(NULL))
  expect_false(gate_live_ok(list(fresh = TRUE)))
  expect_false(gate_live_ok(list(hb_advance = TRUE)))
  expect_true(gate_live_ok(list(fresh = TRUE, hb_advance = TRUE)))
})

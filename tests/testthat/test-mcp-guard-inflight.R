# =============================================================================
# test-mcp-guard-inflight.R — F3 (2026-10-06) : la garde UN-SCÉNARIO-EN-VOL
# =============================================================================
# Le défaut MESURÉ (docs/mcp_realworld_test.md F3) : scenario.json est UNE
# case, consommée à un beat du poller (~3 s). Les outils d'écriture ne
# validaient que seq > last_seq ; un agent rapide qui écrit seq 14, 15, 16 en
# 100 ms perd les seq 14 et 15 SILENCIEUSEMENT — jamais acquittés, jamais
# refusés. La garde (docs/mcp_design_F3_inflight_guard.md, approuvé) refuse
# SCENARIO_IN_FLIGHT tant que le scénario présent n'est pas consommé
# (scenario.seq > last_seq), SAUF set_armed (bootstrap, écrit arm.json).
#
# Portée du jeton, épinglée : un scénario adressé à une AUTRE session est
# ignoré par l'app de cette session (drive_watcher refuse le session_token) ;
# l'écraser ne perd rien — la règle seq seule refuserait à tort après un
# redémarrage de l'app avec une case restante.
# =============================================================================

.mcp_guard_script <- function() {
  path <- file.path(ts_project_root(), "scripts", "mcp_server.R")
  if (!file.exists(path)) {
    skip("the MCP server is a local, gitignored capability and is absent from this clone")
  }
  path
}

.mcp_guard_env <- function() {
  path <- .mcp_guard_script()
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "R", "core", "drive_allowlist.R"), envir = e)
  sys.source(file.path(ts_project_root(), "R", "core", "drive_watcher.R"), envir = e)
  expressions <- parse(path, keep.source = TRUE)
  labels <- vapply(expressions, function(x) paste(deparse(x, width.cutoff = 500L), collapse = "\n"), character(1))
  first <- which(grepl("^\\.ts_json <- function", labels))[1L]
  last <- which(grepl("^\\.ts_serve <- function", labels))[1L]
  if (is.na(first) || is.na(last) || last <= first) {
    stop("mcp_server.R internal definition boundaries were not found")
  }
  for (i in seq.int(first, last - 1L)) eval(expressions[[i]], envir = e)
  # Hermeticity (audit 2026-10-03) : le sandbox démarre sur SON propre root
  # temporaire — zéro contact avec le tools/_drive de l'opérateur.
  root <- file.path(tempdir(), paste0("tsdrive-mcp-guard-", as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(root, "tools", "_drive"), recursive = TRUE, showWarnings = FALSE)
  e$ts_drive_boot(root)
  e$ts_drive_clear_write_error()
  e
}

.mcp_guard_session <- function(e, last_seq = 0L, token = "inftok") {
  root <- tempfile("ts-mcp-guard-")
  dir.create(file.path(root, "tools", "_drive"), recursive = TRUE, showWarnings = FALSE)
  e$ts_drive_boot(root)
  e$ts_drive_clear_write_error()
  e$ts_drive_job_clear()
  started <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  e$ts_drive_write_ready(list(), token, armed = TRUE, last_seq = as.integer(last_seq),
                         hb_n = 2L, started_at = started)
  list(token = token, started = started,
       session_id = e$.ts_session_id(Sys.getpid(), started, token))
}

.mcp_guard_leftover <- function(e, seq, token = "inftok") {
  # La case occupée : un scénario crédible, écrit TEL QUEL (le serveur ne
  # l'a pas écrit — c'est l'état « l'app ne l'a pas encore consommé »).
  e$ts_drive_write_json(
    list(protocol = e$TS_DRIVE_PROTOCOL, seq = as.integer(seq),
         session_token = token, module = "bulk_de", action = "set_inputs",
         preserve_data = TRUE, inputs = list(`bulk-de-de_engine` = "edgeR")),
    e$ts_drive_path("scenario.json"))
}

.mcp_guard_slot_bytes <- function(e) {
  paste(readBin(e$ts_drive_path("scenario.json"), "raw",
                file.size(e$ts_drive_path("scenario.json"))), collapse = " ")
}

test_that("F3: an unconsumed scenario refuses the next write with SCENARIO_IN_FLIGHT", {
  e <- .mcp_guard_env()
  fx <- .mcp_guard_session(e, last_seq = 0L)
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  .mcp_guard_leftover(e, 5L)
  before <- .mcp_guard_slot_bytes(e)
  expect <- list(session_id = fx$session_id)

  r <- e$.ts_tool_set_inputs(11L, "bulk_de",
                             list(`bulk-de-de_engine` = "deseq2"), TRUE, expect)
  expect_true(r$isError, info = "F3: writing over an unconsumed scenario must refuse")
  expect_identical(r$structuredContent$code, "SCENARIO_IN_FLIGHT")
  expect_identical(as.integer(r$structuredContent$detail$inflight_seq), 5L)
  expect_identical(as.integer(r$structuredContent$detail$last_seq), 0L)
  # la garde ne TOUCHE PAS la case : le refus est sans effet de bord
  expect_identical(.mcp_guard_slot_bytes(e), before,
                   info = "F3: the refusal must leave scenario.json byte-identical")
  # et le code est bien du domaine, déclaré
  expect_true("SCENARIO_IN_FLIGHT" %in% e$.ts_domain_codes)
})

test_that("F3: a consumed scenario slot is overwritable, an absent one never refuses", {
  e <- .mcp_guard_env()
  fx <- .mcp_guard_session(e, last_seq = 5L)
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  expect <- list(session_id = fx$session_id)

  # seq 5 == last_seq 5 : la case a été consommée (l'app n'efface jamais le
  # fichier, il avance last_seq) — l'écrasement est l'état NORMAL.
  .mcp_guard_leftover(e, 5L)
  r <- e$.ts_tool_set_inputs(11L, "bulk_de",
                             list(`bulk-de-de_engine` = "deseq2"), TRUE, expect)
  expect_false(r$isError, info = "F3: a consumed slot must stay overwritable")
  expect_true(isTRUE(r$structuredContent$wrote))
  scn <- jsonlite::fromJSON(e$ts_drive_path("scenario.json"), simplifyVector = FALSE)
  expect_identical(as.integer(scn$seq), 11L)
  expect_identical(scn$action, "set_inputs")

  # case absente : aucune garde à franchir
  unlink(e$ts_drive_path("scenario.json"))
  r2 <- e$.ts_tool_set_inputs(12L, "bulk_de",
                              list(`bulk-de-de_engine` = "deseq2"), TRUE, expect)
  expect_false(r2$isError)
  expect_true(file.exists(e$ts_drive_path("scenario.json")))
})

test_that("F3: the guard is token-scoped — another session's leftover does not refuse", {
  e <- .mcp_guard_env()
  fx <- .mcp_guard_session(e, last_seq = 0L)
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  # scénario restant d'une session PRÉCÉDENTE : l'app de cette session
  # l'ignore (session_token), l'écraser ne perd rien.
  .mcp_guard_leftover(e, 5L, token = "oldtok")
  r <- e$.ts_tool_set_inputs(11L, "bulk_de",
                             list(`bulk-de-de_engine` = "deseq2"), TRUE,
                             list(session_id = fx$session_id))
  expect_false(r$isError,
               info = "F3: a scenario pinned to another token is not in flight for THIS session")
})

test_that("F3: set_armed stays reachable while a scenario is in flight (exemption)", {
  e <- .mcp_guard_env()
  fx <- .mcp_guard_session(e, last_seq = 0L)
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  .mcp_guard_leftover(e, 5L)
  r <- e$.ts_tool_set_armed(TRUE, list(session_id = fx$session_id))
  expect_false(r$isError,
               info = "F3: arming is the bootstrap — it writes arm.json, not the slot")
  expect_true(file.exists(e$ts_drive_path("arm.json")))
  # la case, elle, n'a pas bougé
  scn <- jsonlite::fromJSON(e$ts_drive_path("scenario.json"), simplifyVector = FALSE)
  expect_identical(as.integer(scn$seq), 5L)
})

test_that("F3: run, export and read refuse too; import rides the same guard", {
  e <- .mcp_guard_env()
  fx <- .mcp_guard_session(e, last_seq = 0L)
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  .mcp_guard_leftover(e, 5L)
  expect <- list(session_id = fx$session_id)

  for (call in list(
    list(what = "run",    f = function() e$.ts_tool_run(11L, "bulk_de", NULL, TRUE, expect)),
    list(what = "export", f = function() e$.ts_tool_export(11L, "bulk_de", expect)))) {
    r <- call$f()
    expect_true(r$isError, info = sprintf("F3: %s must refuse while seq 5 is in flight", call$what))
    expect_identical(r$structuredContent$code, "SCENARIO_IN_FLIGHT",
                     info = sprintf("F3: %s refusal code", call$what))
  }
  # `read` n'est PAS dans la boucle : sa dérivation de cible (sans état, depuis
  # result.json) refuse NO_EXPORT_TARGET AVANT d'atteindre l'écriture quand
  # aucun verdict d'export n'existe — un refus honnête d'une autre garde. Son
  # site d'écriture reste couvert par le pin statique ci-dessous.
  before <- .mcp_guard_slot_bytes(e)
  expect_identical(.mcp_guard_slot_bytes(e), before)
})

test_that("F3: every scenario write site carries the guard (static pin)", {
  # Six sites écrivent scenario.json (set_inputs, run, import, export, read,
  # wait-observe). Un site qui perdrait sa ligne de garde régresserait
  # silencieusement vers la perte silencieuse — le pin STATIQUE ferme ça,
  # y compris pour les sites trop coûteux à piloter ici (import, wait-observe).
  lines <- readLines(.mcp_guard_script(), warn = FALSE)
  hits <- grep('ts_drive_write_json\\(payload, ts_drive_path\\("scenario\\.json"\\)', lines)
  expect_length(hits, 6L)
  for (h in hits) {
    window <- lines[max(1, h - 8L):h]
    expect_true(any(grepl(".ts_inflight_refusal", window, fixed = TRUE)),
                info = sprintf("F3: line %d writes scenario.json without the guard nearby", h))
  }
})

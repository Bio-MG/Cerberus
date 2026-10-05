# =============================================================================
# tests/testthat/test-drive-multi-agent-turns.R
#
# Le TOUR DE PAROLE entre DEUX écrivains qui partagent UNE session drive.
#
# L'écrivain 1 joue le rôle "agent MCP" ; l'écrivain 2 est l'agent VISUEL,
# simulé par un helper de test SANS navigateur (pas de shinytest2, pas de
# Playwright). L'agent visuel est HONNÊTE par construction : il ne lit que
#   - ready.json  (l'identité de session : session_token, pid, started_at),
#   - result.json (le DERNIER verdict + son ack_seq),
# puis il compose son propre scénario avec seq = ack_seq + 1 et le MÊME pin de
# session, et l'app le consomme par la PAIRE exacte que le poller exécute :
#   ts_drive_validate_scenario() -> ts_drive_apply() -> ts_drive_write_result().
#
# MESURÉ EN LISANT LE CODE (et épinglé ici pour ne pas être réinventé) :
#   - il n'existe AUCUN code SESSION_MISMATCH / SEQ_STALE / STALE_SESSION dans
#     R/core/ : la famille de refus honnête est (a) le seq périmé => verdict
#     status "ignored" (ok = FALSE, errors VIDE, warning "stale seq N (<=
#     last_seq M)") et (b) le token étranger => verdict "invalid" avec l'erreur
#     "session_token mismatch". Le test affirme l'ABSENCE des trois codes et
#     épingle la vraie famille ;
#   - le plancher de seq de l'app est le CURSEUR last_seq, REPRISE de
#     result.json's ack_seq au attach (ts_drive_attach, règle du resumed_ack) :
#     c'est exactement le handshake dont l'agent visuel a besoin — il lit
#     ack_seq, il écrit ack_seq + 1 ;
#   - un seq <= last_seq est "ignored" et le tick n'ÉCRIT PAS result.json pour
#     un ignored : le dernier verdict bon SURVIT au replay — c'est le pin du
#     contrôle négatif ;
#   - ÉCART au modèle supposé : result.json ne porte AUCUN champ d'identité de
#     session (pas de session_id ; le wire figé est protocol/ack_seq/status/
#     applied_at/active_module/armed/preserve_data/errors/warnings/snapshot/
#     job/descriptor/write_error). L'invariant de session partagée vit donc
#     dans le VALIDATEUR (le pin de token refusé en cas d'étranger), pas dans
#     le verdict : le test l'affirme LÀ où le code le porte.
#
# Herméticité : chaque bloc boote son propre root temporaire (ts_drive_boot)
# et ne touche JAMAIS le tools/_drive réel du dépôt.
# =============================================================================

source_project_file("R/core/drive_allowlist.R")
# R/core/drive_watcher.R appelle ts_log_swallow() dans ses gestionnaires
# d'erreur (ts_drive_read_json, ...) : le journaliseur doit exister ici, comme
# dans test-drive-watcher.R (app.R le source au L56).
source_project_file("R/core/error_log.R")
source_project_file("R/core/drive_watcher.R")

# La porte d'armement est forcée OUVERTE pour tout le fichier : Rscript n'est
# jamais interactif, et la porte elle-même est épinglée dans
# test-drive-watcher.R ("non-interactive refuses") — pas ici.
options(ts.drive.interactive = TRUE)


# --- Fixtures -----------------------------------------------------------------

# Root hermétique : le protocole résout TOUT chemin depuis le root booté, donc
# pointer ts_drive_boot() vers un tempdir isole chaque bloc du tools/_drive
# réel. Le reset des diagnostics d'écriture suit test-drive-watcher.R : l'état
# vit dans un environnement global de session et fuite sinon.
.turn_root <- function() {
  root <- file.path(tempdir(), paste0("tsdrive-turn-", as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(root, "tools", "_drive"), recursive = TRUE, showWarnings = FALSE)
  ts_drive_boot(root)
  ts_drive_clear_write_error()
  ts_drive_job_clear()
  root
}

# L'agent dépose arm.json (agent -> app), exactement comme le fait
# test-drive-watcher.R : protocole figé, token de la session, armed = TRUE.
.turn_write_arm <- function(token) {
  ts_drive_write_json(
    list(protocol = TS_DRIVE_PROTOCOL, token = token, armed = TRUE),
    ts_drive_path("arm.json")
  )
}

# Le PLANCHER de seq de l'app : relu depuis result.json (le ack_seq du dernier
# verdict), la règle EXACTE de reprise de ts_drive_attach (resumed_ack). Un
# canal vide vaut 0 — la non-vacuité est affirmée dans le bloc d'armement.
.turn_app_floor <- function() {
  ack <- suppressWarnings(as.integer(ts_drive_read_result()$ack_seq %||% 0L))
  if (length(ack) != 1L || is.na(ack) || ack < 0L) 0L else ack
}

# L'app, miroir FIDÈLE des branches de ts_drive_tick qui touchent un scénario :
#   validate -> si "invalid" : verdict écrit, consommé ;
#              si "ignored" (seq périmé) : RIEN consommé, RIEN écrit ;
#              si "applied-candidate" : apply, puis write_result.
# C'est la PAIRE demandée validate -> apply -> write_result, avec les mêmes
# garde-fous que le tick réel (un "ignored" ne réécrit jamais le verdict).
.turn_app_consume <- function(token) {
  scn_raw <- ts_drive_read_json(ts_drive_path("scenario.json"))
  v <- ts_drive_validate_scenario(scn_raw, token, .turn_app_floor())
  if (identical(v$status, "invalid")) {
    ts_drive_write_result(
      if (is.null(v$scenario)) .turn_app_floor() else v$scenario$seq,
      "invalid", NA_character_, TRUE,
      errors = v$errors, warnings = v$warnings)
    return(list(consumed = TRUE, verdict = "invalid", validation = v))
  }
  if (!identical(v$status, "applied-candidate")) {
    # seq <= last_seq : "ignored". Le tick sort SANS écrire — le verdict
    # précédent reste le dernier mot de result.json.
    return(list(consumed = FALSE, verdict = "ignored", validation = v))
  }
  res <- ts_drive_apply(NULL, NULL, v$scenario)
  ts_drive_write_result(
    v$scenario$seq, res$status, res$active_module %||% v$scenario$module, TRUE,
    preserve_data = isTRUE(v$scenario$preserve_data),
    errors = c(v$errors, res$errors), warnings = c(v$warnings, res$warnings))
  list(consumed = TRUE, verdict = res$status, validation = v, apply = res)
}

# ÉCRIVAIN 1 — rôle agent MCP : il compose un scénario inoffensif (noop sur un
# module allowlisté), porte le pin de session et son seq, le dépose sur le
# wire, et l'app le consomme par la paire du poller.
.turn_mcp_write <- function(seq, token) {
  scn <- list(protocol = TS_DRIVE_PROTOCOL, seq = as.integer(seq),
              module = "bulk_de", action = "noop", preserve_data = TRUE,
              session_token = token, inputs = list())
  expect_true(ts_drive_write_json(scn, ts_drive_path("scenario.json")))
  .turn_app_consume(token)
}

# ÉCRIVAIN 2 — l'agent VISUEL, simulé SANS navigateur. HONNÊTE : il ne lit que
# ready.json (identité) et result.json (dernier verdict + ack_seq) ; il
# vérifie le verdict du tour précédent, puis compose SON scénario avec
# seq = ack_seq + 1 et le MÊME pin de session. stopifnot car un agent qui
# ne peut pas prouver le verdict ne doit PAS écrire.
.turn_visual_agent_turn <- function(identity_at_arm) {
  ready <- ts_drive_read_ready()
  res   <- ts_drive_read_result()
  # (a) le verdict du tour précédent est terminal et BON ;
  stopifnot(!is.null(res),
            identical(as.character(res$status), "done"))
  # (b) l'identité de session n'a PAS tourné depuis l'armement : le pin de
  #     session que l'agent visuel va porter est celui qu'il a lu à l'armement ;
  stopifnot(identical(as.character(ready$session_token),
                      as.character(identity_at_arm$session_token)),
            identical(as.integer(ready$pid),
                      as.integer(identity_at_arm$pid)),
            identical(as.character(ready$started_at),
                      as.character(identity_at_arm$started_at)))
  # (c) il compose son propre scénario : seq = ack_seq + 1, même session_token.
  scn <- list(protocol = TS_DRIVE_PROTOCOL,
              seq = as.integer(res$ack_seq) + 1L,
              module = "bulk_de", action = "noop", preserve_data = TRUE,
              session_token = ready$session_token, inputs = list())
  expect_true(ts_drive_write_json(scn, ts_drive_path("scenario.json")))
  .turn_app_consume(ready$session_token)
}

# Les trois codes de conflit SUPPOSÉS par le modèle de tour de parole — mesurés
# absents du code. L'assertion est leur ABSENCE dans tout verdict.
.turn_conflict_codes <- c("SESSION_MISMATCH", "SEQ_STALE", "STALE_SESSION")

# Aucun code de conflit dans les errors[]/warnings[] du verdict courant.
.turn_expect_no_conflict <- function() {
  res <- ts_drive_read_result()
  msgs <- as.character(unlist(c(res$errors, res$warnings)))
  hits <- vapply(.turn_conflict_codes, function(code) {
    any(startsWith(msgs, code))
  }, logical(1))
  expect_false(any(hits),
               info = sprintf("codes de conflit dans le verdict : %s",
                              paste(msgs, collapse = " | ")))
  invisible(res)
}


# 0. Armement — l'identité de session est publiée, le canal démarre VIDE
# =============================================================================

test_that("arming publishes the session identity and the seq channel starts empty", {
  .turn_root()
  on.exit(unlink(ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)

  token <- ts_drive_new_token()
  started_at <- ts_drive_now_iso()
  # L'app écrit le handshake AVANT que l'agent n'arme : armed = FALSE, hb_n 0.
  ready <- ts_drive_write_ready(list(), token, armed = FALSE,
                                started_at = started_at, hb_n = 0L, last_seq = 0L)
  expect_true(isTRUE(attr(ready, "written")))

  # Les champs d'identité de session TELS QUE LE CODE LES PORTE : session_token,
  # pid, started_at (il n'y a PAS de session_id dans ce protocole — mesuré).
  back <- ts_drive_read_ready()
  expect_identical(as.character(back$session_token), token)
  expect_identical(as.integer(back$pid), as.integer(Sys.getpid()))
  expect_identical(as.character(back$started_at), started_at)
  expect_identical(as.integer(back$hb_n), 0L)
  expect_false(isTRUE(back$armed))
  expect_identical(as.integer(back$last_seq), 0L)

  # Non-vacuité : pas d'arm.json, pas de result.json — le canal est VIDE.
  expect_false(file.exists(ts_drive_path("arm.json")))
  expect_false(file.exists(ts_drive_path("result.json")))

  # L'agent arme ; la porte du token s'ouvre pour CETTE session seulement.
  expect_true(.turn_write_arm(token))
  st <- ts_drive_arm_state(token)
  expect_true(isTRUE(st$armed))
  expect_match(st$reason, "token match")
  # Un token étranger n'ouvre PAS la même porte.
  expect_false(isTRUE(ts_drive_arm_state("intruder1")$armed))
})


# 1. Le tour de parole — deux écrivains, une session, seq en escalier
# =============================================================================

test_that("two writers take turns on one shared session: seq 1 then ack_seq + 1", {
  .turn_root()
  on.exit(unlink(ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)

  token <- ts_drive_new_token()
  started_at <- ts_drive_now_iso()
  ts_drive_write_ready(list(), token, armed = FALSE,
                       started_at = started_at, hb_n = 0L, last_seq = 0L)
  identity <- list(session_token = token, pid = Sys.getpid(), started_at = started_at)
  expect_true(.turn_write_arm(token))

  # ── Tour 1 : l'agent MCP écrit seq = 1 ────────────────────────────────────
  t1 <- .turn_mcp_write(1L, token)
  expect_true(isTRUE(t1$consumed))
  expect_identical(t1$verdict, "done")
  r1 <- ts_drive_read_result()
  expect_identical(as.character(r1$status), "done")
  expect_identical(as.integer(r1$ack_seq), 1L)
  .turn_expect_no_conflict()

  # ── Tour 2 : l'agent VISUEL lit ready.json + result.json SEULEMENT ────────
  # Le helper asserte lui-même le verdict du tour 1 (status done) et
  # l'identité (session_token / pid / started_at), puis compose seq 2.
  t2 <- .turn_visual_agent_turn(identity)
  expect_true(isTRUE(t2$consumed))
  expect_identical(t2$verdict, "done")
  r2 <- ts_drive_read_result()
  expect_identical(as.character(r2$status), "done")
  expect_identical(as.integer(r2$ack_seq), 2L)
  # Le verdict du tour 2 ack le seq que l'agent visuel a COMPOSÉ (ack_seq + 1).
  expect_identical(as.integer(t2$validation$scenario$seq), 2L)
  .turn_expect_no_conflict()

  # ── L'invariant de session partagée, LÀ OÙ le code le porte ───────────────
  # MESURÉ : result.json ne porte AUCUN champ d'identité de session — l'app
  # n'y écho ni session_token ni pid. L'invariant vit dans le VALIDATEUR :
  # chaque scénario consommé a été validé contre le token de ready.json, et
  # un token étranger est REFUSÉ (affirmé au bloc suivant).
  expect_false(any(c("session_token", "session_id", "pid", "started_at")
                   %in% names(r2)))

  # L'identité de session n'a pas bougé pendant tout l'échange.
  fin <- ts_drive_read_ready()
  expect_identical(as.character(fin$session_token), token)
  expect_identical(as.integer(fin$pid), as.integer(identity$pid))
  expect_identical(as.character(fin$started_at), identity$started_at)
})

test_that("a scenario pinned to a FOREIGN session is refused: the shared-session invariant", {
  .turn_root()
  on.exit(unlink(ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)

  token <- ts_drive_new_token()
  started_at <- ts_drive_now_iso()
  ts_drive_write_ready(list(), token, armed = FALSE,
                       started_at = started_at, hb_n = 0L, last_seq = 0L)
  identity <- list(session_token = token, pid = Sys.getpid(), started_at = started_at)
  expect_true(.turn_write_arm(token))

  # L'agent MCP consomme seq 1, puis l'agent visuel consomme seq 2.
  t1 <- .turn_mcp_write(1L, token)
  expect_identical(t1$verdict, "done")
  t2 <- .turn_visual_agent_turn(identity)
  expect_identical(t2$verdict, "done")

  # Un TROISIÈME écrivain qui prétend une autre session : le même seq utile
  # (3) mais le pin d'un token étranger. Le validateur refuse AVANT tout
  # apply — le pin de token EST l'invariant de session partagée.
  intruder <- list(protocol = TS_DRIVE_PROTOCOL, seq = 3L,
                   module = "bulk_de", action = "noop", preserve_data = TRUE,
                   session_token = "intruder", inputs = list())
  expect_true(ts_drive_write_json(intruder, ts_drive_path("scenario.json")))
  t3 <- .turn_app_consume(token)
  expect_identical(t3$verdict, "invalid")
  expect_true(startsWith(as.character(t3$validation$errors[1]), "session_token mismatch"),
              info = paste(t3$validation$errors, collapse = " | "))
  # Le verdict écrit porte le refus. MESURÉ : le retour "invalid" TARDIF du
  # validateur (token étranger) transporte quand même le scénario reconstruit
  # (seuls les retours PRÉCOCES — scn absent, protocole inconnu, seq périmé —
  # reviennent scenario = NULL), donc le tick ack le seq DU SCÉNARIO REFUSÉ :
  # ack_seq 3, status "invalid" — le canal avance, l'agent ne re-soumettra pas.
  expect_identical(as.integer(t3$validation$scenario$seq), 3L)
  r3 <- ts_drive_read_result()
  expect_identical(as.character(r3$status), "invalid")
  expect_identical(as.integer(r3$ack_seq), 3L)
  expect_true(any(startsWith(as.character(unlist(r3$errors)),
                             "session_token mismatch")))
  .turn_expect_no_conflict()
})


# 2. Contrôle négatif — le replay d'un vieux seq est DÉTECTÉ, pas silencieux
# =============================================================================

test_that("replaying writer 1's stale scenario is refused honestly and the last good verdict survives", {
  .turn_root()
  on.exit(unlink(ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)

  token <- ts_drive_new_token()
  started_at <- ts_drive_now_iso()
  ts_drive_write_ready(list(), token, armed = FALSE,
                       started_at = started_at, hb_n = 0L, last_seq = 0L)
  identity <- list(session_token = token, pid = Sys.getpid(), started_at = started_at)
  expect_true(.turn_write_arm(token))

  # Le tour de parole complet : seq 1 (MCP), seq 2 (visuel).
  t1 <- .turn_mcp_write(1L, token)
  expect_identical(t1$verdict, "done")
  t2 <- .turn_visual_agent_turn(identity)
  expect_identical(t2$verdict, "done")

  # Le DERNIER VERDICT BON, figé en bytes avant le replay.
  res_path <- ts_drive_path("result.json")
  before <- paste(readBin(res_path, "raw", file.size(res_path)), collapse = " ")

  # L'agent MCP REJOUE son VIEUX scénario (seq 1, même pin de session) : un
  # replay honnête est exactement ça — le même payload re-déposé sur le wire.
  old <- .turn_mcp_write(1L, token)
  # Le refus honnête MESURÉ : PAS de code SEQ_STALE (il n'existe pas), mais
  # status "ignored", ok = FALSE, errors VIDE, et le warning nomme la règle.
  expect_false(isTRUE(old$consumed))
  expect_identical(old$verdict, "ignored")
  expect_identical(old$validation$status, "ignored")
  expect_false(isTRUE(old$validation$ok))
  expect_length(old$validation$errors, 0L)
  expect_match(as.character(old$validation$warnings[1]),
               "stale seq 1 (<= last_seq 2)", fixed = TRUE)
  .turn_expect_no_conflict()

  # Le verdict précédent SURVIT byte pour byte : un "ignored" n'écrit PAS
  # result.json — le dernier mot du canal reste le seq 2 de l'agent visuel.
  after <- paste(readBin(res_path, "raw", file.size(res_path)), collapse = " ")
  expect_identical(after, before)
  r <- ts_drive_read_result()
  expect_identical(as.character(r$status), "done")
  expect_identical(as.integer(r$ack_seq), 2L)

  # L'escalier continue APRÈS le replay : le tour suivant du visuel compose
  # seq 3 depuis le ack_seq relu — le replay n'a pas entamé le curseur.
  t3 <- .turn_visual_agent_turn(identity)
  expect_identical(t3$verdict, "done")
  expect_identical(as.integer(ts_drive_read_result()$ack_seq), 3L)
})

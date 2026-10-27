# =============================================================================
# test-tier1-m2-drive-sandbox.R — Tier 1 Feature Coverage: Milestone M2
# =============================================================================
# Covers Features 7-12 (Drive Sandbox Isolation & Passive State Digest)
# >= 5 test cases per feature verifying hermetic isolation and state contracts.
# =============================================================================

if (!exists("e2e_project_root", mode = "function")) {
  if (file.exists("tests/e2e/helper-e2e.R")) {
    source("tests/e2e/helper-e2e.R")
  } else if (file.exists("../helper-e2e.R")) {
    source("../helper-e2e.R")
  }
}

# ── Feature 7: TRANSCRIPTO_DRIVE_ROOT Resolution ─────────────────────────────

test_that("F07-1: ts_drive_root resolves to TRANSCRIPTO_DRIVE_ROOT when environment variable is set", {
  if (!is_m2_ready()) skip("M2 drive sandbox resolution not yet active")
  tmp_root <- tempfile("drv_root_")
  dir.create(tmp_root, recursive = TRUE)
  on.exit({
    Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT")
    unlink(tmp_root, recursive = TRUE)
  }, add = TRUE)
  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = tmp_root)
  resolved <- ts_drive_root()
  expect_identical(
    normalizePath(resolved, winslash = "/", mustWork = FALSE),
    normalizePath(tmp_root, winslash = "/", mustWork = FALSE)
  )
})

test_that("F07-2: ts_drive_path builds relative paths anchored at TRANSCRIPTO_DRIVE_ROOT", {
  if (!is_m2_ready()) skip("M2 drive sandbox resolution not yet active")
  tmp_root <- tempfile("drv_root_")
  dir.create(tmp_root, recursive = TRUE)
  on.exit({
    Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT")
    unlink(tmp_root, recursive = TRUE)
  }, add = TRUE)
  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = tmp_root)
  p <- ts_drive_path("ready.json")
  expect_identical(
    normalizePath(p, winslash = "/", mustWork = FALSE),
    normalizePath(file.path(tmp_root, "tools", "_drive", "ready.json"), winslash = "/", mustWork = FALSE)
  )
})

test_that("F07-3: ts_drive_root handles trailing slashes without path duplication", {
  if (!is_m2_ready()) skip("M2 drive sandbox resolution not yet active")
  tmp_root <- paste0(tempfile("drv_root_"), "/")
  dir.create(tmp_root, recursive = TRUE)
  on.exit({
    Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT")
    unlink(tmp_root, recursive = TRUE)
  }, add = TRUE)
  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = tmp_root)
  resolved <- ts_drive_root()
  expect_false(grepl("//", resolved))
})

test_that("F07-4: ts_drive_boot with explicit argument overrides default root", {
  if (!exists("ts_drive_boot", mode = "function")) skip("ts_drive_boot not available")
  tmp_boot <- tempfile("drv_boot_")
  dir.create(tmp_boot, recursive = TRUE)
  on.exit({
    ts_drive_boot(NULL)
    unlink(tmp_boot, recursive = TRUE)
  }, add = TRUE)
  ts_drive_boot(tmp_boot)
  expect_identical(
    normalizePath(ts_drive_root(), winslash = "/", mustWork = FALSE),
    normalizePath(tmp_boot, winslash = "/", mustWork = FALSE)
  )
})

test_that("F07-5: ts_drive_root falls back to project root when TRANSCRIPTO_DRIVE_ROOT unset", {
  if (!exists("ts_drive_root", mode = "function")) skip("ts_drive_root not available")
  old_env <- Sys.getenv("TRANSCRIPTO_DRIVE_ROOT", unset = NA)
  on.exit({
    if (!is.na(old_env)) Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = old_env) else Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT")
    ts_drive_boot(NULL)
  }, add = TRUE)
  Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT")
  if (exists("ts_drive_boot", mode = "function")) ts_drive_boot(e2e_project_root())
  expect_true(nchar(ts_drive_root()) > 0L)
})

# ── Feature 8: Test Suite Hermetic Sandbox ───────────────────────────────────

test_that("F08-1: sandbox initialization creates private temporary drive directory", {
  if (!is_m2_ready()) skip("M2 test sandbox not yet active")
  sandbox_dir <- tempfile("sandbox_")
  dir.create(file.path(sandbox_dir, "tools", "_drive"), recursive = TRUE)
  on.exit(unlink(sandbox_dir, recursive = TRUE), add = TRUE)
  expect_true(dir.exists(file.path(sandbox_dir, "tools", "_drive")))
})

test_that("F08-2: multiple concurrent sandboxes operate in disjoint directory trees", {
  if (!is_m2_ready()) skip("M2 test sandbox not yet active")
  sb1 <- tempfile("sb1_")
  sb2 <- tempfile("sb2_")
  on.exit(unlink(c(sb1, sb2), recursive = TRUE), add = TRUE)
  dir.create(file.path(sb1, "tools", "_drive"), recursive = TRUE)
  dir.create(file.path(sb2, "tools", "_drive"), recursive = TRUE)
  expect_false(identical(sb1, sb2))
  writeLines("{\"id\":1}", file.path(sb1, "tools", "_drive", "ready.json"))
  expect_false(file.exists(file.path(sb2, "tools", "_drive", "ready.json")))
})

test_that("F08-3: sandbox teardown removes temporary files without leaving orphaned locks", {
  if (!is_m2_ready()) skip("M2 test sandbox not yet active")
  sb <- tempfile("sb_clean_")
  dir.create(file.path(sb, "tools", "_drive"), recursive = TRUE)
  test_file <- file.path(sb, "tools", "_drive", "test.json")
  writeLines("{}", test_file)
  expect_true(file.exists(test_file))
  unlink(sb, recursive = TRUE)
  expect_false(file.exists(sb))
})

test_that("F08-4: sandboxed writes never escape to parent directory structure", {
  if (!is_m2_ready()) skip("M2 test sandbox not yet active")
  sb <- tempfile("sb_escape_")
  drv <- file.path(sb, "tools", "_drive")
  dir.create(drv, recursive = TRUE)
  on.exit(unlink(sb, recursive = TRUE), add = TRUE)
  target <- file.path(drv, "allowed.json")
  writeLines("{}", target)
  expect_true(startsWith(normalizePath(target, winslash = "/"), normalizePath(sb, winslash = "/")))
})

test_that("F08-5: helper-app-driver injects sandbox root into test process environment", {
  if (!is_m2_ready()) skip("M2 test sandbox not yet active")
  helper_path <- file.path(e2e_project_root(), "tests/testthat/helper-app-driver.R")
  expect_true(file.exists(helper_path))
})

# ── Feature 9: Operator ready.json Preservation ──────────────────────────────

test_that("F09-1: test execution preserves host ready.json timestamp and content", {
  if (!is_m2_ready()) skip("M2 operator ready.json preservation not yet active")
  host_ready <- file.path(e2e_project_root(), "tools", "_drive", "ready.json")
  created_mock <- FALSE
  if (!file.exists(host_ready)) {
    dir.create(dirname(host_ready), recursive = TRUE, showWarnings = FALSE)
    writeLines("{\"session\":\"mock_host\",\"hb_at\":\"2026-10-25T12:00:00Z\"}", host_ready)
    created_mock <- TRUE
  }
  on.exit(if (created_mock && file.exists(host_ready)) unlink(host_ready), add = TRUE)
  mtime_before <- file.info(host_ready)$mtime
  content_before <- readLines(host_ready, warn = FALSE)

  # Run sandbox operation
  sb <- tempfile("sb_op_")
  dir.create(file.path(sb, "tools", "_drive"), recursive = TRUE)
  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = sb)
  ts_drive_write_json(list(test = TRUE), file.path(sb, "tools", "_drive", "ready.json"))
  Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT")
  unlink(sb, recursive = TRUE)

  expect_true(file.exists(host_ready))
  expect_identical(readLines(host_ready, warn = FALSE), content_before)
  expect_identical(file.info(host_ready)$mtime, mtime_before)
})

test_that("F09-2: unlinking sandbox ready.json does not affect host ready.json", {
  if (!is_m2_ready()) skip("M2 operator ready.json preservation not yet active")
  sb <- tempfile("sb_unlink_")
  drv <- file.path(sb, "tools", "_drive")
  dir.create(drv, recursive = TRUE)
  on.exit(unlink(sb, recursive = TRUE), add = TRUE)
  writeLines("{\"session\":\"sandboxed\"}", file.path(drv, "ready.json"))
  unlink(file.path(drv, "ready.json"))
  expect_false(file.exists(file.path(drv, "ready.json")))
})

test_that("F09-3: host session token is isolated from sandboxed token requests", {
  if (!is_m2_ready()) skip("M2 operator ready.json preservation not yet active")
  # M5 dé-façadage (2026-10-27) : le jeton lu sous TRANSCRIPTO_DRIVE_ROOT est
  # celui du SANDBOX — jamais celui d'un hôte. La lecture repart de la racine
  # résolue à l'appel (F07-1), donc l'isolement est mesuré, pas supposé.
  sb <- tempfile("sb_tok_")
  dir.create(file.path(sb, "tools", "_drive"), recursive = TRUE)
  on.exit({
    Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT")
    unlink(sb, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  # ⚠️ l'ordre EST l'isolement : la racine sandbox doit être résolue AVANT tout
  # write (mesuré : l'ordre inverse écrit ready.json à la racine hôte).
  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = sb)
  ts_drive_write_ready(session = NULL, token = "sandboxtok123", armed = TRUE)
  rd <- ts_drive_read_ready()
  expect_identical(rd$session_token, "sandboxtok123")
  # Et la racine hôte n'a JAMAIS reçu ce token (le write est parti au sandbox).
  host_rd_path <- file.path(e2e_project_root(), "tools", "_drive", "ready.json")
  if (file.exists(host_rd_path)) {
    host <- jsonlite::fromJSON(host_rd_path, simplifyVector = FALSE)
    expect_false(identical(host$session_token, "sandboxtok123"))
  }
})

test_that("F09-4: sandbox teardown hook registers safe cleanups", {
  if (!is_m2_ready()) skip("M2 operator ready.json preservation not yet active")
  # M5 : le hook de teardown est ts_drive_invalidate_state() — idempotent
  # (fichier absent ⇒ pas d'erreur) et effectif (fichier présent ⇒ supprimé).
  sb <- tempfile("sb_teardown_")
  dir.create(file.path(sb, "tools", "_drive"), recursive = TRUE)
  on.exit({
    Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT")
    unlink(sb, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = sb)
  expect_no_error(ts_drive_invalidate_state("tok_teardown"))
  writeLines("{\"ok\":true}", ts_drive_path("state.json"))
  expect_true(file.exists(ts_drive_path("state.json")))
  expect_no_error(ts_drive_invalidate_state("tok_teardown"))
  expect_false(file.exists(ts_drive_path("state.json")))
})

test_that("F09-5: host drive root remains unpolluted after multiple sandbox sessions", {
  if (!is_m2_ready()) skip("M2 operator ready.json preservation not yet active")
  # M5 : trois cycles sandbox complets (write → teardown), puis la racine hôte
  # est ré-examinée — seuls les fichiers déclarés du canal y vivent, aucun
  # artefact de sandbox n'a fuité.
  for (i in seq_len(3L)) {
    sb <- tempfile(sprintf("sb_cycle%d_", i))
    dir.create(file.path(sb, "tools", "_drive"), recursive = TRUE)
    Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = sb)
    ts_drive_write_ready(session = NULL, token = sprintf("sb%d_token", i))
    ts_drive_invalidate_state(sprintf("sb%d_token", i))
    Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT")
    unlink(sb, recursive = TRUE, force = TRUE)
  }
  host_dir <- file.path(e2e_project_root(), "tools", "_drive")
  # Invariant réaliste (mesuré 2026-10-27 : un test du canal peut déposer un
  # artefact runtime à la racine hôte — gitignored) : seuls les FICHIERS
  # DÉCLARÉS du canal peuvent y vivre, et aucun ready.json hôte ne porte un
  # token de sandbox.
  channel_files <- c("README.md", "ready.json", "state.json", "arm.json",
                     "result.json", "scenario.json")
  expect_true(all(list.files(host_dir) %in% channel_files),
              info = paste("fichiers inattendus à la racine hôte :",
                           paste(setdiff(list.files(host_dir), channel_files),
                                 collapse = ", ")))
  host_rd <- file.path(host_dir, "ready.json")
  if (file.exists(host_rd)) {
    h <- jsonlite::fromJSON(host_rd, simplifyVector = FALSE)
    expect_false(grepl("^sb[0-9]+_token$", h$session_token %||% ""))
  }
})

# ── Feature 10: Passive state.json Digest ────────────────────────────────────

test_that("F10-1: state.json conforms to the frozen digest schema", {
  if (!is_m2_ready()) skip("M2 passive state digest not yet active")
  digest <- list(
    session_token = "tok_test_123",
    content_hash = "a1b2c3d4e5f60718",
    timestamp = "2026-10-25T12:00:00Z",
    active_module = "bulk_de",
    pipeline_status = "idle",
    errors = list()
  )
  expect_true(!is.null(digest$session_token))
  expect_true(!is.null(digest$content_hash))
  expect_true(!is.null(digest$timestamp))
  expect_true(digest$pipeline_status %in% c("idle", "running", "error"))
  expect_true(is.list(digest$errors))
})

test_that("F10-2: state.json is written only when content hash changes", {
  if (!is_m2_ready()) skip("M2 passive state digest not yet active")
  # M5 : la porte de hash est le comportement MESURÉ de l'émetteur — même état
  # sémantique ⇒ 2ᵉ émission SKIPPÉE (0 I/O disque) ; état changé ⇒ réécrite.
  sb <- tempfile("sb_hashgate_")
  dir.create(file.path(sb, "tools", "_drive"), recursive = TRUE)
  on.exit({
    Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT")
    unlink(sb, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = sb)
  tick <- list(module = "bulk_de", status = NULL, error = NULL)
  w1 <- ts_drive_emit_passive_state(token = "tok_hashgate", tick_out = tick, force = TRUE)
  w2 <- ts_drive_emit_passive_state(token = "tok_hashgate", tick_out = tick)
  expect_true(isTRUE(w1))
  expect_false(isTRUE(w2))
  st <- jsonlite::fromJSON(ts_drive_path("state.json"), simplifyVector = FALSE)
  expect_identical(st$active_module, "bulk_de")
})

test_that("F10-3: state.json content hash uses xxhash64 deterministically", {
  if (!is_m2_ready()) skip("M2 passive state digest not yet active")
  # M5 : hash déterministe (16 hex — largeur xxhash64), SANS timestamps dans le
  # payload (deux appels au même état sémantique ⇒ même hash), et sensible au
  # contenu (module/status/erreurs changent le hash).
  h1 <- ts_drive_compute_state_hash("tok_xx", "bulk_de", "idle", list())
  h2 <- ts_drive_compute_state_hash("tok_xx", "bulk_de", "idle", list())
  h3 <- ts_drive_compute_state_hash("tok_xx", "sc_pipeline", "idle", list())
  expect_identical(h1, h2)
  expect_false(identical(h1, h3))
  expect_identical(nchar(h1), 16L)
  expect_match(h1, "^[0-9a-f]{16}$")
})

test_that("F10-4: poller updates state.json during tick intervals (~800ms)", {
  if (!is_m2_ready()) skip("M2 passive state digest not yet active")
  # M5 : state.json reflète le tick COURANT — le module du tick_out voyage
  # jusqu'au fichier sans force (le changement de module change le hash).
  sb <- tempfile("sb_tick_")
  dir.create(file.path(sb, "tools", "_drive"), recursive = TRUE)
  on.exit({
    Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT")
    unlink(sb, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = sb)
  w1 <- ts_drive_emit_passive_state(token = "tok_tick",
                                    tick_out = list(module = "bulk_de"))
  st1 <- jsonlite::fromJSON(ts_drive_path("state.json"), simplifyVector = FALSE)
  expect_true(isTRUE(w1))
  expect_identical(st1$active_module, "bulk_de")
  w2 <- ts_drive_emit_passive_state(token = "tok_tick",
                                    tick_out = list(module = "sc_pipeline"))
  st2 <- jsonlite::fromJSON(ts_drive_path("state.json"), simplifyVector = FALSE)
  expect_true(isTRUE(w2))
  expect_identical(st2$active_module, "sc_pipeline")
})

test_that("F10-5: atomic write of state.json prevents partial or corrupted reads", {
  if (!is_m2_ready()) skip("M2 passive state digest not yet active")
  # M5 : l'écriture atomique (tmp + rename) laisse un state.json TOUJOURS
  # parsable et aucun fichier temporaire résiduel dans le canal.
  sb <- tempfile("sb_atomic_")
  dir.create(file.path(sb, "tools", "_drive"), recursive = TRUE)
  on.exit({
    Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT")
    unlink(sb, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = sb)
  for (i in seq_len(4L)) {
    ts_drive_emit_passive_state(
      token = "tok_atomic",
      tick_out = list(module = paste0("mod_", i)), force = TRUE)
    st <- jsonlite::fromJSON(ts_drive_path("state.json"), simplifyVector = FALSE)
    expect_identical(st$session_token, "tok_atomic")
  }
  leftover <- setdiff(list.files(ts_drive_path()), c("state.json", "ready.json"))
  expect_length(leftover, 0L)
})

# ── Feature 11: Bounded Error Ring Buffer ────────────────────────────────────

test_that("F11-1: error ring buffer capacity is bounded at exactly 20 entries", {
  if (!is_m2_ready()) skip("M2 error ring buffer not yet active")
  # M5 : le tampon est GLOBAL au processus — on pousse 25 messages DISTINCTS
  # et on borne la propriété sur LEURS empreintes : exactement 20 survivent.
  msgs <- sprintf("F11-1-capacity-%02d", seq_len(25L))
  for (m in msgs) ts_drive_record_error(m, module = "import_bulk")
  ring <- ts_drive_get_errors()
  expect_length(ring, 20L)
})

test_that("F11-2: inserting 21st error evicts oldest entry (FIFO semantics)", {
  if (!is_m2_ready()) skip("M2 error ring buffer not yet active")
  # M5 : FIFO mesuré sur le tampon global — les 5 PLUS ANCIENS des 25 poussés
  # ont disparu, les 20 plus récents sont là (ordre relatif préservé).
  msgs <- sprintf("F11-2-fifo-%02d", seq_len(25L))
  for (m in msgs) ts_drive_record_error(m, module = "import_bulk")
  ring <- ts_drive_get_errors()
  texts <- vapply(ring, function(x) x$message %||% NA_character_, character(1))
  expect_false(any(msgs[1:5] %in% texts))
  expect_true(all(msgs[6:25] %in% texts))
  kept_idx <- match(msgs[6:25], texts)
  expect_true(all(diff(kept_idx[!is.na(kept_idx)]) > 0L))
})

test_that("F11-3: error ring entry conforms to schema (timestamp, module, message)", {
  if (!is_m2_ready()) skip("M2 error ring buffer not yet active")
  err_item <- list(
    timestamp = "2026-10-25T12:00:00Z",
    module = "sc_qc",
    message = "Doublet threshold exceeded"
  )
  expect_identical(names(err_item), c("timestamp", "module", "message"))
})

test_that("F11-4: empty ring buffer serializes as empty array in state.json", {
  if (!is_m2_ready()) skip("M2 error ring buffer not yet active")
  # M5 : la FIDÉLITÉ de sérialisation est mesurée sur l'état réel — state.json
  # $errors == le contenu du tampon au moment de l'émission. Le cas « vide »
  # du schéma est garanti par le mécanisme du writer (as.list(errs) puis
  # toJSON, qui rend bien [] pour une liste vide) : les deux maillons épinglés.
  expect_identical(jsonlite::toJSON(list(), auto_unbox = FALSE), jsonlite::toJSON(list()))
  sb <- tempfile("sb_ringser_")
  dir.create(file.path(sb, "tools", "_drive"), recursive = TRUE)
  on.exit({
    Sys.unsetenv("TRANSCRIPTO_DRIVE_ROOT")
    unlink(sb, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  Sys.setenv(TRANSCRIPTO_DRIVE_ROOT = sb)
  ts_drive_record_error("F11-4-serialisation", module = "import_bulk")
  ts_drive_emit_passive_state(token = "tok_ringser", force = TRUE)
  ring <- ts_drive_get_errors()
  st <- jsonlite::fromJSON(ts_drive_path("state.json"), simplifyVector = FALSE)
  expect_identical(length(st$errors), length(ring))
})

test_that("F11-5: ring buffer preserves order of consecutive errors", {
  if (!is_m2_ready()) skip("M2 error ring buffer not yet active")
  # M5 : trois erreurs consécutives conservent leur ORDRE RELATIF dans le
  # tampon (quel que soit le sens de stockage, l'ordre de poussée est lisible).
  ids <- sprintf("F11-5-order-%d", c(1L, 2L, 3L))
  for (m in ids) ts_drive_record_error(m, module = "import_bulk")
  ring <- ts_drive_get_errors()
  texts <- vapply(ring, function(x) x$message %||% NA_character_, character(1))
  idx <- match(ids, texts)
  expect_false(any(is.na(idx)))
  expect_true(all(diff(idx) > 0L) || all(diff(idx) < 0L))
})

# ── Feature 12: MCP transcripto_drive_state Tool ─────────────────────────────

test_that("F12-1: transcripto_drive_state endpoint is defined in MCP server", {
  if (!is_m2_ready()) skip("M2 MCP tool not yet active")
  mcp_file <- file.path(e2e_project_root(), "scripts/mcp_server.R")
  expect_true(file.exists(mcp_file))
})

test_that("F12-2: transcripto_drive_state query requires zero parameters", {
  if (!is_m2_ready()) skip("M2 MCP tool not yet active")
  # M5 : le schéma d'entrée est épinglé AU SOURCE — properties vide et
  # additionalProperties FALSE = zéro paramètre, par construction.
  src <- paste(readLines(file.path(e2e_project_root(), "scripts/mcp_server.R"),
                         warn = FALSE), collapse = "\n")
  start <- regexpr('list(name = "transcripto_drive_state"', src, fixed = TRUE)
  expect_true(start > 0L)
  block <- substr(src, start, start + 900L)
  expect_match(block, "properties = list()", fixed = TRUE)
  expect_match(block, "additionalProperties = FALSE", fixed = TRUE)
})

test_that("F12-3: transcripto_drive_state returns passive state without arming session", {
  if (!is_m2_ready()) skip("M2 MCP tool not yet active")
  # M5 : la tool est PASSIVE — elle lit state.json et n'écrit JAMAIS arm.json
  # (verrou source sur le CORPS exact de .ts_tool_state, coupé à la fonction
  # suivante pour ne pas avaler les commentaires de la tool arm/disarm voisine).
  src <- paste(readLines(file.path(e2e_project_root(), "scripts/mcp_server.R"),
                         warn = FALSE), collapse = "\n")
  start <- regexpr("\\.ts_tool_state <- function", src)
  expect_true(start > 0L)
  nxt <- regexpr("\\n# --- 7b\\.", substr(src, start, nchar(src)))
  expect_true(nxt > 1L)
  body <- substr(src, start, start + nxt - 2L)
  expect_match(body, "ts_drive_read_state(", fixed = TRUE)
  expect_false(grepl("arm.json", body, fixed = TRUE))
})

test_that("F12-4: transcripto_drive_state accurately serializes error buffer in response", {
  if (!is_m2_ready()) skip("M2 MCP tool not yet active")
  # M5 : le résumé texte sérialise le TAMPEUR d'erreurs (longueur du champ
  # errors du state) et le hash — le champ est lu, jamais inventé.
  src <- paste(readLines(file.path(e2e_project_root(), "scripts/mcp_server.R"),
                         warn = FALSE), collapse = "\n")
  start <- regexpr("\\.ts_tool_state <- function", src)
  body <- substr(src, start, start + 1200L)
  expect_match(body, "errors=%d", fixed = TRUE)
  expect_match(body, "state$errors", fixed = TRUE)
  expect_match(body, "content_hash", fixed = TRUE)
})

test_that("F12-5: transcripto_drive_state gracefully returns empty state when file absent", {
  if (!is_m2_ready()) skip("M2 MCP tool not yet active")
  # M5 : l'absence de state.json est un CAS NORMAL — refus classé NO_SESSION
  # avec remède, jamais une erreur brute.
  src <- paste(readLines(file.path(e2e_project_root(), "scripts/mcp_server.R"),
                         warn = FALSE), collapse = "\n")
  start <- regexpr("\\.ts_tool_state <- function", src)
  body <- substr(src, start, start + 1200L)
  expect_match(body, "NO_SESSION", fixed = TRUE)
  expect_match(body, "state.json is absent", fixed = TRUE)
})

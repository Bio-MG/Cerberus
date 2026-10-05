# =============================================================================
# test-mcp-wgcna-local.R — Slice 4 : le serveur MCP face au module bulk_wgcna
# =============================================================================
# Le twin WGCNA de la suite sc-local. TROIS faits y sont épinglés :
#   1. l'invariant OUTILS : bulk_wgcna ajoute DEUX boutons et UNE route, et
#      ZÉRO outil (run_pipeline + export_result existent) — 10 outils reste 10 ;
#   2. l'entrée session-dérivée `bulk-wgcna-wgcna_traits` est validée contre
#      le vocabulaire publié de result.json (INPUT_NOT_READY / VOCAB_STALE /
#      INDEX_OUT_OF_RANGE / allow_empty), et le scenario part en entiers ;
#   3. la route d'export est dispatchable, avec le contrat de colonnes rendu
#      dans la description de l'outil (guaranteed + mode_traits).
# =============================================================================

.mcp_wgcna_script <- function() {
  path <- file.path(ts_project_root(), "scripts", "mcp_server.R")
  if (!file.exists(path)) {
    skip("the MCP server is a local, gitignored capability and is absent from this clone")
  }
  path
}

.mcp_wgcna_env <- function() {
  path <- .mcp_wgcna_script()
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
  # Hermeticity: the sandbox boots on its OWN temp root (audit 2026-10-03) —
  # zero contact with the operator's tools/_drive.
  root <- file.path(tempdir(), paste0("tsdrive-mcp-wgcna-", as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(root, "tools", "_drive"), recursive = TRUE, showWarnings = FALSE)
  e$ts_drive_boot(root)
  e$ts_drive_clear_write_error()
  e
}

.mcp_wgcna_arm <- function(e, armed = TRUE) {
  root <- tempfile("ts-mcp-wgc-")
  dir.create(file.path(root, "tools", "_drive"), recursive = TRUE, showWarnings = FALSE)
  e$ts_drive_boot(root)
  e$ts_drive_clear_write_error()
  e$ts_drive_job_clear()
  token <- "wgctok"
  started <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  e$ts_drive_write_ready(list(), token, armed = armed, last_seq = 0L,
                         hb_n = 2L, started_at = started)
  if (armed) {
    e$ts_drive_write_json(
      list(protocol = e$TS_DRIVE_PROTOCOL, token = token, armed = TRUE),
      e$ts_drive_path("arm.json"))
  }
  list(token = token, started = started,
       session_id = e$.ts_session_id(Sys.getpid(), started, token))
}

.mcp_wgcna_vocab <- function(e, with_vocab = TRUE) {
  # Un verdict `done` dont le snapshot porte le bloc `vocabulary` publié par
  # la sonde du module bulk_wgcna (projété : exactement ce bloc).
  voc <- if (with_vocab) list(
    bulk_wgcna = list(vocabulary = list(
      traits    = c("condition", "age", "batch"),
      vocab_rev = 2L))) else NULL
  write_result <- get("ts_drive_write_result", envir = e)
  write_result(10L, "done", "bulk_wgcna", TRUE,
               snapshot = if (is.null(voc)) NULL else list(modules = voc))
}

# =============================================================================
# 1. L'invariant : deux boutons, une route, ZÉRO outil de plus
# =============================================================================
test_that("bulk_wgcna adds buttons and a route, and NO tool: TEN stays TEN", {
  e <- .mcp_wgcna_env()
  tools <- e$.ts_tools()
  expect_length(tools, 10L)
  expect_setequal(vapply(tools, function(x) x$name, character(1)),
                  c("transcripto_drive_status", "transcripto_drive_read_result",
                    "transcripto_drive_snapshot", "transcripto_drive_set_inputs",
                    "transcripto_drive_run", "transcripto_drive_wait",
                    "transcripto_drive_set_armed", "transcripto_drive_export",
                    "transcripto_drive_import", "transcripto_drive_read"))

  # La table de run du serveur connaît les DEUX boutons du module.
  expect_setequal(e$TS_MCP_RUN_BUTTONS$bulk_wgcna,
                  c("bulk-wgcna-run_wgcna_power", "bulk-wgcna-run_wgcna_modules"))
  expect_length(e$.ts_mcp_run_problems(), 0L)
  expect_length(e$.ts_mcp_export_problems(), 0L)
  expect_length(e$.ts_mcp_vocab_problems(), 0L)
})

# =============================================================================
# 2. La validation serveur de `traits` : index contre le vocabulaire publié
# =============================================================================
test_that("indexed traits are validated against the published vocabulary and normalised", {
  e <- .mcp_wgcna_env()
  fx <- .mcp_wgcna_arm(e)
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  .mcp_wgcna_vocab(e)
  expect <- list(session_id = fx$session_id, pid = Sys.getpid(),
                 started_at = fx$started)

  r <- e$.ts_tool_set_inputs(12L, "bulk_wgcna", list(
    `bulk-wgcna-wgcna_traits` = list(index = c(1L, 2L), vocab_rev = 2L),
    `bulk-wgcna-wgcna_n_genes` = 2500),
    TRUE, expect)
  expect_false(r$isError)
  # Le scenario part en ENTIERS normalisés (les index JSON arrivent en
  # doubles) — lisible via fromJSON, comme la suite sc-local l'épingle.
  scn <- jsonlite::fromJSON(e$ts_drive_path("scenario.json"), simplifyVector = FALSE)
  v <- scn$inputs$`bulk-wgcna-wgcna_traits`
  expect_identical(as.integer(unlist(v$index)), c(1L, 2L))
  expect_identical(as.integer(v$vocab_rev), 2L)
  expect_identical(as.numeric(scn$inputs$`bulk-wgcna-wgcna_n_genes`), 2500)
})

test_that("empty traits is a VALID indexed payload (allow_empty), everywhere", {
  e <- .mcp_wgcna_env()
  fx <- .mcp_wgcna_arm(e)
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  .mcp_wgcna_vocab(e)
  expect <- list(session_id = fx$session_id, pid = Sys.getpid(),
                 started_at = fx$started)

  # Côté serveur : forme acceptée (allow_empty = TRUE).
  r <- e$.ts_tool_set_inputs(12L, "bulk_wgcna", list(
    `bulk-wgcna-wgcna_traits` = list(index = integer(0), vocab_rev = 2L)),
    TRUE, expect)
  expect_false(r$isError)

  # Côté app : la résolution honore allow_empty (Slice 4, driver_watcher.R) —
  # un covariates vide passerait désormais le wire ET l'apply.
  rv <- e$ts_drive_resolve_session_inputs(
    list(`bulk-wgcna-wgcna_traits` = list(index = integer(0), vocab_rev = 2L)),
    "bulk_wgcna",
    function() list(traits = c("condition", "age"), vocab_rev = 2L))
  expect_true(isTRUE(rv$ok))
  expect_identical(as.character(rv$values$`bulk-wgcna-wgcna_traits`), character(0))
})

test_that("the trait refusals keep their names, and NOTHING is written on refusal", {
  e <- .mcp_wgcna_env()
  fx <- .mcp_wgcna_arm(e)
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  .mcp_wgcna_vocab(e)
  expect <- list(session_id = fx$session_id, pid = Sys.getpid(),
                 started_at = fx$started)
  scn_path <- e$ts_drive_path("scenario.json")
  scn_before <- if (file.exists(scn_path))
    paste(readBin(scn_path, "raw", file.size(scn_path)), collapse = " ") else NULL

  # VOCAB_STALE : l'agent épingle une rev dépassée.
  r1 <- e$.ts_tool_set_inputs(12L, "bulk_wgcna", list(
    `bulk-wgcna-wgcna_traits` = list(index = 1L, vocab_rev = 9L)), TRUE, expect)
  expect_true(r1$isError)
  expect_identical(r1$structuredContent$code, "VOCAB_STALE")

  # INDEX_OUT_OF_RANGE : au-delà des trois choix publiés.
  r2 <- e$.ts_tool_set_inputs(13L, "bulk_wgcna", list(
    `bulk-wgcna-wgcna_traits` = list(index = 4L, vocab_rev = 2L)), TRUE, expect)
  expect_true(r2$isError)
  expect_identical(r2$structuredContent$code, "INDEX_OUT_OF_RANGE")

  # 33 indices : refusés AU CONTRÔLE DE FORME (VALUE_REFUSED — la borne
  # déclarée est 32 ; la forme est l'affaire du serveur, le domaine celui du
  # tool). Le refus NOMME la borne.
  r3 <- e$.ts_tool_set_inputs(14L, "bulk_wgcna", list(
    `bulk-wgcna-wgcna_traits` = list(index = seq_len(33L), vocab_rev = 2L)), TRUE, expect)
  expect_true(r3$isError)
  expect_identical(r3$structuredContent$code, "VALUE_REFUSED")
  # La raison nominative (refus par entrée) porte la borne.
  expect_match(jsonlite::toJSON(r3$structuredContent, auto_unbox = TRUE),
               "declared maximum is 32", fixed = TRUE)

  # VALUE_REFUSED : hors bornes figées du widget (2000-5000).
  r4 <- e$.ts_tool_set_inputs(15L, "bulk_wgcna", list(
    `bulk-wgcna-wgcna_n_genes` = 1999), TRUE, expect)
  expect_true(r4$isError)
  expect_identical(r4$structuredContent$code, "VALUE_REFUSED")

  # INPUT_MODULE_MISMATCH : une entrée bulk_wgcna adressée à bulk_de.
  r5 <- e$.ts_tool_set_inputs(16L, "bulk_de", list(
    `bulk-wgcna-wgcna_traits` = list(index = 1L, vocab_rev = 2L)), TRUE, expect)
  expect_true(r5$isError)
  expect_identical(r5$structuredContent$code, "INPUT_MODULE_MISMATCH")

  # AUCUN refus n'écrit : le scenario est resté OCTET POUR OCTET celui du
  # dernier dispatch accepté (ou absent).
  if (file.exists(scn_path)) {
    scn_after <- paste(readBin(scn_path, "raw", file.size(scn_path)), collapse = " ")
    expect_false(is.null(scn_before))
    expect_identical(scn_after, scn_before)
  } else {
    expect_null(scn_before)
  }

  # Pas de vocabulaire publié du tout : INPUT_NOT_READY.
  .mcp_wgcna_vocab(e, with_vocab = FALSE)
  r6 <- e$.ts_tool_set_inputs(17L, "bulk_wgcna", list(
    `bulk-wgcna-wgcna_traits` = list(index = 1L, vocab_rev = 2L)), TRUE, expect)
  expect_true(r6$isError)
  expect_identical(r6$structuredContent$code, "INPUT_NOT_READY")
})

# =============================================================================
# 3. La route d'export : dispatchable, contrat rendu, surface inchangée
# =============================================================================
test_that("the export tool dispatches bulk_wgcna and advertises the traits mode", {
  e <- .mcp_wgcna_env()
  fx <- .mcp_wgcna_arm(e)
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  expect <- list(session_id = fx$session_id, pid = Sys.getpid(),
                 started_at = fx$started)

  r <- e$.ts_tool_export(20L, "bulk_wgcna", expect)
  expect_false(r$isError)
  expect_identical(r$structuredContent$dispatched, "export_result")
  # Le scenario écrit porte l'action et le module, RIEN d'autre (pas de
  # route, pas de handle : les stems portent des runs de 8+ redactés).
  scn <- jsonlite::fromJSON(e$ts_drive_path("scenario.json"), simplifyVector = FALSE)
  expect_identical(as.character(scn$action), "export_result")
  expect_identical(as.character(scn$module), "bulk_wgcna")
  expect_false(any(c("route", "handle", "file", "format") %in% names(scn)))

  # Le contrat de colonnes est rendu dans la description de l'outil, avec le
  # mode conditionnel des traits.
  ex <- Filter(function(x) identical(x$name, "transcripto_drive_export"), e$.ts_tools())[[1]]
  expect_match(ex$description, "gene, module", fixed = TRUE)
  expect_match(ex$description, "trait_cor_method", fixed = TRUE)

  # La note redaction : 6 entrées indexées désormais.
  idx_ids <- vapply(e$TS_DRIVE_SESSION_INPUTS, function(x) x$module, character(1))
  expect_length(idx_ids, 6L)
  expect_true("bulk-wgcna-wgcna_traits" %in% names(e$TS_DRIVE_SESSION_INPUTS))
})

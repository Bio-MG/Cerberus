# =============================================================================
# test-drive-n1-injection-reason.R — N1 (2026-10-06) : un verdict `invalid`
# d'injection portait DEUX raisons sur le disque (VOCAB_STALE pour group_ref
# ET group_target) et l'agent lisait `errors: []`.
#
# DEUX défauts mesurés, tous deux verrouillés ici, sur la chaîne EXACTE du
# cycle réel (scénario écrit → tick de l'app → result.json → lecture MCP) :
#
#   1. `.ts_tool_read_result` coalesçait `res$errors` avec `%|NA|%`, dont le
#      contrat est SCALAIRE (`length(a) != 1L` → repli) : un verdict portant
#      ≥ 2 raisons publiait `[]`. Mesuré en réel : seq 6 (retest 2026-10-06).
#   2. La locale C de la session (le `LC_CTYPE` qui refuse de monter, bruit
#      F8) fait tronquer par jsonlite tout message porteur d'un octet non
#      ASCII : l'em-dash du message du résolveur coupait la queue actionable
#      (« … at 3 b », « — re-snapshot and retry. » perdu). Le pliage ASCII à
#      l'écriture du verdict garantit que la raison reste ENTIÈRE sur le wire.
# =============================================================================

source_project_file("R/core/error_log.R")
source_project_file("R/core/state.R")
source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")
suppressPackageStartupMessages(library(jsonlite))
# Le gate d'armement doit être OUVERT pour le tick (Rscript n'est jamais
# interactif) — le même forçage que test-drive-watcher.R.
options(ts.drive.interactive = TRUE)

.N1_root <- function() {
  root <- file.path(tempdir(), paste0("tsdrive-n1-", as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(root, "tools", "_drive"), recursive = TRUE, showWarnings = FALSE)
  ts_drive_boot(root)
  ts_drive_clear_write_error()
  ts_drive_job_clear()
  root
}

.N1_session <- function(token = "n1tok", last_seq = 5L) {
  ts_drive_write_json(list(protocol = TS_DRIVE_PROTOCOL, token = token, armed = TRUE),
                      ts_drive_path("arm.json"))
  ts_drive_write_ready(list(), token, armed = TRUE, last_seq = as.integer(last_seq),
                       hb_n = 2L, started_at = ts_drive_now_iso())
}

# La sonde de vocabulaire de l'app a AVANCÉ (rev 3, niveaux mock/CoV2) pendant
# que l'agent épingle encore rev 2 sur la table périmée publiée ([cornea,
# limbus, sclera]) — l'état exact du seq 6 du retest réel.
.N1_effects <- function() {
  fx <- list(`bulk-de-run_de` = list(token = 1L, vocab = function() list(
    condition_col = c("condition", "tissue"),
    covariates    = c("condition", "tissue"),
    group_levels  = c("mock", "CoV2"),
    vocab_rev     = 3L)))
  function(id, mode, module = NULL) switch(mode, tokens = fx, NULL)
}

.N1_write_group_scenario <- function(token) {
  ts_drive_write_json(list(
    protocol = TS_DRIVE_PROTOCOL, seq = 6L, session_token = token,
    module = "bulk_de", action = "set_inputs", preserve_data = TRUE,
    inputs = list(`bulk-de-group_ref` = list(index = 1L, vocab_rev = 2L),
                  `bulk-de-group_target` = list(index = 2L, vocab_rev = 2L))),
    ts_drive_path("scenario.json"))
}

test_that("N1: an injection refused on stale vocabulary writes BOTH reasons, complete, to disk", {
  .N1_root()
  .N1_session()
  .N1_write_group_scenario("n1tok")
  t <- ts_drive_tick(NULL, NULL, list(), "n1tok", last_seq = 5L,
                     armed = TRUE, effects = .N1_effects())
  cat("DBG consumed=", t$consumed, " selected=", t$selected, " armed=", t$armed,
      " status=", t$status, " err=", t$error %||% "-", " opt=",
      getOption("ts.drive.interactive"), " loc=", Sys.getlocale("LC_CTYPE"), "\n")
  res <- ts_drive_read_json(ts_drive_path("result.json"))
  expect_identical(res$status, "invalid")
  expect_identical(res$ack_seq, 6L)
  # LES DEUX raisons, pas une, pas zéro
  expect_length(res$errors, 2L)
  for (msg in res$errors) {
    expect_match(msg, "VOCAB_STALE", fixed = TRUE)
    # LA QUEUE ACTIONNABLE survit au passage sur le disque : c'est elle que la
    # locale C + jsonlite coupaient (l'em-dash du message du résolveur).
    expect_match(msg, "re-snapshot and retry", fixed = TRUE,
                 info = "N1: the reason must survive the C-locale jsonlite write intact")
  }
  # les ids visés sont nommés — l'agent sait QUOI re-photographier
  expect_true(any(grepl("bulk-de-group_ref", res$errors, fixed = TRUE)))
  expect_true(any(grepl("bulk-de-group_target", res$errors, fixed = TRUE)))
})

test_that("N1: verdict diagnostics are folded to ASCII at the writer", {
  # La locale du test est UTF-8, où jsonlite s'en sortait seul — mais la
  # session RÉELLE tourne en locale C (LC_CTYPE refuse de monter). Le pliage
  # est un CONTRAT du writer, pas un correctif local : tout octet non-ASCII
  # dans les diagnostics est un pied de colère qui attend la prochaine
  # corruption. Épinglé octet par octet.
  .N1_root()
  msg <- "a pin with an em-dash \u2014 and a tail that must survive"
  invisible(ts_drive_write_result(2L, "invalid", "bulk_de", TRUE, errors = msg))
  p <- ts_drive_path("result.json")
  txt <- readChar(p, file.info(p)$size, useBytes = TRUE)
  expect_false(grepl("[^\\x20-\\x7E\\n\\r\\t]", txt, perl = TRUE),
               info = "N1: the verdict file carries ASCII diagnostics only")
  expect_match(txt, "and a tail that must survive", fixed = TRUE,
               info = "N1: the folded message keeps its tail")
  res <- ts_drive_read_json(p)
  expect_match(res$errors[[1]], "must survive", fixed = TRUE)
})

test_that("N1: the MCP read_result publishes every refusal reason the verdict carries", {
  root <- .N1_root()
  .N1_session()
  .N1_write_group_scenario("n1tok")
  invisible(ts_drive_tick(NULL, NULL, list(), "n1tok", last_seq = 5L,
                          armed = TRUE, effects = .N1_effects()))

  # Le bac à sable du serveur : le MÊME harness que test-mcp-sc-local.R, booté
  # sur CE root — read_result est donc exactement la surface de l'agent.
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "R", "core", "drive_allowlist.R"), envir = e)
  sys.source(file.path(ts_project_root(), "R", "core", "drive_watcher.R"), envir = e)
  expressions <- parse(file.path(ts_project_root(), "scripts", "mcp_server.R"), keep.source = TRUE)
  labels <- vapply(expressions, function(x) paste(deparse(x, width.cutoff = 500L), collapse = "\n"), character(1))
  first <- which(grepl("^\\.ts_json <- function", labels))[1L]
  last <- which(grepl("^\\.ts_serve <- function", labels))[1L]
  for (i in seq.int(first, last - 1L)) eval(expressions[[i]], envir = e)
  e$ts_drive_boot(root)
  e$ts_drive_clear_write_error()

  rr <- e$.ts_tool_read_result()
  sc <- rr$structuredContent
  expect_false(rr$isError)
  expect_identical(sc$status, "invalid")
  expect_identical(sc$ack_seq, 6L)
  # LE DÉFAUT MESURÉ : deux raisons sur le disque, `[]` sur le wire.
  expect_length(sc$errors, 2L)
  for (msg in sc$errors) expect_match(msg, "VOCAB_STALE", fixed = TRUE)
  # warnings suit la même loi qu'errors (même opérateur, même piège)
  expect_identical(length(sc$warnings), 0L)
})

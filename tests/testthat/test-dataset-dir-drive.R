# =============================================================================
# test-dataset-dir-drive.R -- PoC « répertoire de datasets » jalon P2 : l'outil
# MCP PASSIF transcripto_drive_datasets (RED-first)
# =============================================================================
# FICHE (fait foi) :
# docs/proposals/PROPOSAL_DATASET_DIRECTORY_POC_2026-11-02.md
#   §3.4 (variante tranchée : OUTIL PASSIF transcripto_drive_datasets —
#         lecture seule, liste, cohérent avec transcripto_drive_state outil
#         n°11, sans élargir l'allowlist d'écriture) ;
#   §1 AC4 (l'agent liste les datasets détectés) ; §4 T7.
#
# PATTERN (mesuré) : l'outil n°11 transcripto_drive_state —
# scripts/mcp_server.R:563-578 : lecture en tryCatch -> refus classé
# (.ts_tool_err) sinon .ts_tool_ok(structured, text) ; enregistré dans
# .ts_tools() (:3675-3685, inputSchema objet vide), dispatché (:3734).
# Le NOUVEL outil est le SIXIÈME read-only : chaîne initialize :3700
# « Eleven tools: five read-only » -> « Twelve tools: six read-only ».
#
# ALLOWLIST (mesurée) : ts_drive_import_roots() (drive_allowlist.R:1487-1495)
# inclut la racine projet ts_drive_root(), tempdir(), les entrées
# TRANSCRIPTO_DRIVE_DATA_DIR — le répertoire défaut <root>/data_sc/datasets
# est donc DANS les racines ; la racine résolue est confinée par la règle S11
# des répertoires (ts_drive_validate_import_dir / .ts_drive_resolve_import_path,
# drive_allowlist.R:1511-1560 : refus `..` brut, containment à séparateur,
# fail-closed). AUCUN élargissement : une racine hors racines -> OUTSIDE_ALLOWLIST.
#
# SCAN LÉGER : validate = FALSE est supporté par le moteur P0 (figé T5,
# test-dataset-dir-scan.R) — le listing ne porte PAS de verdict v1 (status/
# raison absents), seulement le résumé manifeste + chemins + mtime.
#
# SANS SESSION : contrairement à l'outil n°11 (lit state.json -> NO_SESSION),
# le listing est un acte de système de fichiers — il ne requiert ni ready.json
# ni state.json (AC4 : l'agent voit ce qui est disponible AVANT de démarrer
# l'app). ZÉRO écriture : aucun fichier touché nulle part (T7-3).
# =============================================================================

source_project_file("tools/pkg_skeleton/R/session_checkpoint.R")
source_project_file("tools/pkg_skeleton/R/biomentat_handoff.R")
source_project_file("tools/pkg_skeleton/R/dataset_directory.R")
source_project_file("tools/pkg_skeleton/R/drive_allowlist.R")
source_project_file("tools/pkg_skeleton/R/drive_watcher.R")

.mcp2_script <- file.path(ts_project_root(), "scripts", "mcp_server.R")
.mcp2_src <- paste(readLines(.mcp2_script, warn = FALSE, encoding = "UTF-8"),
                   collapse = "\n")

# ── Porte RED sur le SOURCE (motif test-mod-import-bulk-drive.R / F12-2) -----

test_that("T7-0 : l'outil transcripto_drive_datasets est câblé dans mcp_server.R (source)", {
  expect_true(grepl("transcripto_drive_datasets", .mcp2_src, fixed = TRUE),
              info = paste0("outil passif P2 absent de scripts/mcp_server.R : fiche ",
                            "PROPOSAL_DATASET_DIRECTORY_POC_2026-11-02.md §3.4/AC4/T7 ",
                            "non livré"))
})

# ── Harnais (motif .adversarial_mcp_env, test-adversarial-m2.R:474-491) ------

.mcp2_env <- function(boot = NULL) {
  e <- new.env(parent = globalenv())
  # Section 2 du serveur, étendue aux dépendances du scan PoC (le harnais
  # adversarial ne source que drive_watcher — l'outil datasets a besoin des
  # 4 fichiers moteur PoC + de l'allowlist pour le confinement S11).
  for (f in c("tools/pkg_skeleton/R/session_checkpoint.R",
              "tools/pkg_skeleton/R/biomentat_handoff.R",
              "tools/pkg_skeleton/R/dataset_directory.R",
              "tools/pkg_skeleton/R/drive_allowlist.R",
              "tools/pkg_skeleton/R/drive_watcher.R")) {
    sys.source(file.path(ts_project_root(), f), envir = e)
  }
  expressions <- parse(.mcp2_script, keep.source = TRUE)
  labels <- vapply(expressions, function(x) paste(deparse(x, width.cutoff = 500L),
                                                  collapse = "\n"), character(1))
  first <- which(grepl("^\\.ts_json <- function", labels))[1L]
  last <- which(grepl("^\\.ts_serve <- function", labels))[1L]
  if (is.na(first) || is.na(last) || last <= first) {
    stop("mcp_server.R boundaries not found")
  }
  for (i in seq.int(first, last - 1L)) eval(expressions[[i]], envir = e)
  root <- file.path(tempdir(), paste0("mcp2-ds-", as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(root, "tools", "_drive"), recursive = TRUE,
             showWarnings = FALSE)
  # boot : sandbox par défaut (motif adversarial — les autres outils y
  # écrivent ready/state) ; boot = ts_project_root() pour T7-5, qui mesure le
  # répertoire DÉFAUT tel que le serveur réel le boote (mcp_server.R section 2 :
  # ts_drive_boot(.project_root)). ts_drive_boot est PUR (état only, mesuré).
  e$ts_drive_boot(if (is.null(boot)) root else boot)
  e
}

# Fixture (3e variante compacte du gabarit v1 — 120 gènes, 4x2 pour éviter
# tout bloc identique >= 15 lignes avec les 2 autres fichiers du PoC) :
# le listing est validate = FALSE, la conformité v1 complète est couverte par
# T2/T4 de test-dataset-dir-scan.R — ici seule la DÉTECTION doit marcher.
.mcp2_fixtures <- function() {
  root <- tempfile("datasets_p2_")
  dir.create(root, recursive = TRUE)
  for (id in c("DS_P2_ALPHA", "DS_P2_BETA")) {
    d <- file.path(root, id)
    dir.create(d)
    set.seed(99)
    m <- matrix(as.integer(rpois(120L * 4L, 250L)), nrow = 120L,
                dimnames = list(sprintf("GENE%04d", seq_len(120L)),
                                c("w1", "w2", "w3", "w4")))
    utils::write.table(as.data.frame(m), file.path(d, "counts.tsv"),
                       sep = "\t", quote = FALSE, col.names = NA)
    utils::write.table(data.frame(sample = c("w1", "w2", "w3", "w4"),
                                  condition = c("A", "A", "B", "B")),
                       file.path(d, "samplesheet.tsv"),
                       sep = "\t", quote = FALSE, row.names = FALSE)
    writeLines(jsonlite::toJSON(list(
      schema_version = "biomentat-exchange/v1", dataset_id = id,
      species = "human", genome_assembly = "GRCh38",
      feature_id_type = "ensembl", matrix_path = "counts.tsv",
      matrix_type = "raw_counts", samplesheet_path = "samplesheet.tsv"),
      auto_unbox = TRUE), file.path(d, "manifest.json"))
  }
  dir.create(file.path(root, "DS_P2_EMPTY"))
  root
}

# Snapshot récursif chemins+tailles (variantes compactes distinctes des
# snapshots des 2 autres fichiers — même discipline, autre forme).
.mcp2_snapshot <- function(root) {
  rel <- gsub("\\\\", "/", list.files(root, recursive = TRUE, all.files = FALSE,
                                      no.. = TRUE))
  full <- file.path(root, rel)
  stats::setNames(file.info(full)$size, rel)
}

.mcp2_unset <- function() {
  Sys.unsetenv("TRANSCRIPTO_DATASETS_DIR")
  Sys.unsetenv("TRANSCRIPTO_DRIVE_DATA_DIR")
}

# ── T7-1 -- enregistrement + chaîne de version (12e outil, 6e read-only) -----

test_that("T7-1 l'outil est enregistré (inputSchema vide, motif n°11) et la chaîne dit twelve/six", {
  expect_true(grepl("transcripto_drive_datasets", .mcp2_src, fixed = TRUE))
  e <- .mcp2_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)

  tools <- e$.ts_tools()
  tool_names <- vapply(tools, function(x) x$name, character(1))
  expect_true("transcripto_drive_datasets" %in% tool_names)
  ds_tool <- tools[[which(tool_names == "transcripto_drive_datasets")[1L]]]
  expect_identical(ds_tool$inputSchema$type, "object")
  expect_equal(length(ds_tool$inputSchema$properties), 0L,
               info = "outil passif SANS paramètre (pattern n°11, F12-2)")
  expect_false(isTRUE(ds_tool$inputSchema$additionalProperties))
  expect_true(nzchar(ds_tool$description))

  init <- e$.ts_initialize(list(protocolVersion = "2025-06-18"))
  expect_match(init$instructions, "Twelve tools: six read-only", fixed = TRUE,
               info = "chaîne de version : eleven/five -> twelve/six (P2, 2026-11-03)")
})

# ── T7-2 -- le listing liste N datasets (validate = FALSE, sans session) -----

test_that("T7-2 tools/call transcripto_drive_datasets liste les 3 fixtures détectées, sans session ni verdict", {
  expect_true(grepl("transcripto_drive_datasets", .mcp2_src, fixed = TRUE))
  .mcp2_unset()
  on.exit(.mcp2_unset(), add = TRUE)
  root <- .mcp2_fixtures()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  Sys.setenv(TRANSCRIPTO_DATASETS_DIR = root)

  e <- .mcp2_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)

  # SANS session : pas de ready.json/state.json — le listing n'en dépend pas
  # (différence documentée avec l'outil n°11, qui rend NO_SESSION).
  resp <- e$.ts_dispatch(list(jsonrpc = "2.0", id = 7L, method = "tools/call",
                              params = list(name = "transcripto_drive_datasets",
                                            arguments = list())))
  expect_false(isTRUE(resp$result$isError),
               info = "le listing ne requiert AUCUNE session (AC4)")
  sc <- resp$result$structuredContent
  expect_identical(sc$count, 3L)
  expect_identical(sc$datasets_dir,
                   normalizePath(root, winslash = "/", mustWork = FALSE))
  ids <- vapply(sc$datasets, function(x) x$dataset_id, character(1))
  expect_setequal(ids, c("DS_P2_ALPHA", "DS_P2_BETA", "DS_P2_EMPTY"))
  # résumé manifeste porté (lecteur v1), verdicts v1 ABSENTS (validate=FALSE)
  alpha <- sc$datasets[[which(ids == "DS_P2_ALPHA")[1L]]]
  expect_identical(alpha$species, "human")
  expect_identical(alpha$matrix_type, "raw_counts")
  expect_true(is.null(alpha$status),
              info = "validate=FALSE : aucun verdict v1 dans le listing léger")
  expect_match(resp$result$content[[1]]$text, "3",
               info = "résumé texte au pattern de l'outil n°11")
})

# ── T7-3 -- ZÉRO écriture : snapshot du répertoire et du canal inchangés -----

test_that("T7-3 l'outil n'écrit RIEN : répertoire datasets et canal drive inchangés", {
  expect_true(grepl("transcripto_drive_datasets", .mcp2_src, fixed = TRUE))
  .mcp2_unset()
  on.exit(.mcp2_unset(), add = TRUE)
  root <- .mcp2_fixtures()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  Sys.setenv(TRANSCRIPTO_DATASETS_DIR = root)

  e <- .mcp2_env()
  boot_root <- e$ts_drive_root()
  on.exit(unlink(boot_root, recursive = TRUE, force = TRUE), add = TRUE)

  before_ds <- .mcp2_snapshot(root)
  before_drive <- .mcp2_snapshot(boot_root)

  resp <- e$.ts_dispatch(list(jsonrpc = "2.0", id = 8L, method = "tools/call",
                              params = list(name = "transcripto_drive_datasets",
                                            arguments = list())))
  expect_false(isTRUE(resp$result$isError))

  expect_identical(.mcp2_snapshot(root), before_ds,
                   info = "AUCUN fichier du répertoire datasets touché (lecture seule, AC2)")
  expect_identical(.mcp2_snapshot(boot_root), before_drive,
                   info = "AUCUN fichier du canal drive touché (pas d'arm.json, pas de state.json)")
})

# ── T7-4 -- hors allowlist : refus classé, AUCUN élargissement (S11) ---------

test_that("T7-4 racine hors des racines autorisées -> OUTSIDE_ALLOWLIST (fail-closed)", {
  expect_true(grepl("transcripto_drive_datasets", .mcp2_src, fixed = TRUE))
  .mcp2_unset()
  on.exit(.mcp2_unset(), add = TRUE)
  # Un dossier SŒUR de tempdir() : ni sous la racine projet, ni sous tempdir()
  # (RtmpXXX), ni sous TRANSCRIPTO_DRIVE_DATA_DIR — hors de TOUTES les racines.
  outside <- file.path(dirname(tempdir()), paste0("mcp2_outside_",
                                                  as.integer(runif(1, 1, 1e9))))
  dir.create(outside, recursive = TRUE)
  on.exit(unlink(outside, recursive = TRUE, force = TRUE), add = TRUE)
  Sys.setenv(TRANSCRIPTO_DATASETS_DIR = outside)

  e <- .mcp2_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)

  resp <- e$.ts_dispatch(list(jsonrpc = "2.0", id = 9L, method = "tools/call",
                              params = list(name = "transcripto_drive_datasets",
                                            arguments = list())))
  expect_true(isTRUE(resp$result$isError),
              info = "une racine hors allowlist est REFUSÉE, jamais lue ni élargie (S11)")
  expect_identical(resp$result$structuredContent$code, "OUTSIDE_ALLOWLIST")
})

# ── T7-5 -- répertoire défaut DANS l'allowlist (mesure de la condition ask) --

test_that("T7-5 sans env : le répertoire défaut <root>/data_sc/datasets est dans l'allowlist et répond liste vide", {
  expect_true(grepl("transcripto_drive_datasets", .mcp2_src, fixed = TRUE))
  .mcp2_unset()
  on.exit(.mcp2_unset(), add = TRUE)

  # boot = projet (comme le serveur réel) — NE PAS unlink le boot root :
  # c'est la racine du DÉPÔT (ts_drive_boot est pur, rien à nettoyer).
  e <- .mcp2_env(boot = ts_project_root())

  expected_root <- normalizePath(file.path(ts_project_root(), "data_sc", "datasets"),
                                 winslash = "/", mustWork = FALSE)
  resp <- e$.ts_dispatch(list(jsonrpc = "2.0", id = 10L, method = "tools/call",
                              params = list(name = "transcripto_drive_datasets",
                                            arguments = list())))
  expect_false(isTRUE(resp$result$isError),
               info = "le répertoire défaut <root>/data_sc/datasets est DANS les racines (ts_drive_import_roots inclut la racine projet) — condition de l'ask mesurée, aucun élargissement")
  sc <- resp$result$structuredContent
  expect_identical(sc$datasets_dir, expected_root)
  expect_identical(sc$count, 0L)
  expect_false(dir.exists(expected_root),
               info = "le listing ne crée JAMAIS le répertoire défaut (P0 AC2)")
})

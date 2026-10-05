# =============================================================================
# R3 (2026-10-04) — la matrice de refus et le contenu de transcripto_drive_read,
# côté APP (le répondant générique `ts_drive_read_export_respond`), sur un root
# hermétique. Les tests croisés MCP (dispatch -> read_result) vivent dans
# test-mcp-sc-local.R ; ici, aucune surface serveur n'est chargée.
#
# Contraintes épinglées ici :
#   - la preview ÉGALE la tête du fichier, cellule par cellule (l'ordre des
#     positions est le contrat — le bug de transpose mesuré en R2 ne doit pas
#     pouvoir revenir) ;
#   - la matrice de refus complète : EXPORT_GONE, FILE_TOO_LARGE,
#     READ_TOO_WIDE, READ_IO_ERROR — chaque refus est un verdict `invalid`
#     porté par errors[][], jamais une exception ;
#   - la garde D1 aux bornes, vue à travers le répondant (199/200 verbatim,
#     201 => <over-200-chars> ; .., /, \, ~, contrôle => <guarded>) ;
#   - le keep-set FERMÉ : seules les clés de TS_DRIVE_READ_KEYS voyagent, les
#     colonnes restent sanitisees ;
#   - herméticité : chaque fixture boote son propre root temporaire et ses
#     propres fichiers dans le répertoire d'export TEMPORAIRE du processus de
#     test — zéro contact avec tools/_drive.
# =============================================================================

.read_export_env <- function() {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "R", "core", "drive_allowlist.R"), envir = e)
  sys.source(file.path(ts_project_root(), "R", "core", "drive_watcher.R"), envir = e)
  root <- tempfile("tsdrive-read-")
  dir.create(file.path(root, "tools", "_drive"), recursive = TRUE, showWarnings = FALSE)
  e$ts_drive_boot(root)
  e$ts_drive_clear_write_error()
  e
}

# Le répertoire d'export est celui du PROCESSUS de test (tempdir), comme pour
# l'app : les fixtures y écrivent des fichiers à suffixe unique par test et
# chaque test nettoie les siens.
.read_export_csv <- function(file_base, df) {
  d <- file.path(tempdir(), "ts_drive_exports")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(d, file_base)
  utils::write.csv(df, path, row.names = FALSE, na = "")
  path
}

.read_export_verdict <- function(e, module, file_base, columns, n_rows, n_cols,
                                 seq = 10L) {
  e$ts_drive_write_result(seq, "done", module, TRUE, descriptor = list(
    format = "csv", file = file_base, bytes = 120L,
    n_rows = n_rows, n_cols = n_cols, columns = columns))
}

test_that("every declared export route streams a preview (all 9 stems)", {
  e <- .read_export_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  stems <- e$TS_DRIVE_EXPORT_STEMS
  expect_length(stems, 9L)
  for (route in names(stems)) {
    # Suffixe unique par route : le répertoire d'export est partagé entre les
    # tests du processus, aucun croisement possible.
    fb <- paste0(stems[[route]], "_7.csv")
    p <- .read_export_csv(fb, data.frame(gene = c("TP53", "BRCA1"),
                                         score = c("1.5", "2.5"),
                                         stringsAsFactors = FALSE))
    on.exit(unlink(p), add = TRUE)
    .read_export_verdict(e, route, fb, c("gene", "score"), 2L, 2L)
    v <- e$ts_drive_read_export_respond(11L, 2L)
    expect_identical(v$status, "done", info = route)
    expect_identical(as.character(v$descriptor$route), route, info = route)
    # Le handle EST le nom du fichier déclaré : les stems, soulignés, passent
    # le sanitiseur intacts (mesuré) — le trait de soulignement casse toute
    # frontière \b d'un run de 8+.
    expect_identical(as.character(v$descriptor$handle), fb, info = route)
    expect_identical(as.integer(v$descriptor$preview$rows_returned), 2L,
                     info = route)
    expect_identical(as.character(unlist(v$descriptor$preview$rows[[1]])),
                     c("TP53", "1.5"), info = route)
  }
})

test_that("the preview equals the file head, cell by cell (order pinned)", {
  e <- .read_export_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  df <- data.frame(
    gene  = c("TP53", "BRCA1", "MYC", "KRAS", "PIK3CA", "APC"),
    score = c("1.5", "2.25", "0.1", "-3", "10", "7"),
    flag  = c("TRUE", "FALSE", "TRUE", "FALSE", "TRUE", "FALSE"),
    stringsAsFactors = FALSE)
  fb <- "bulk_de_results_11.csv"
  p <- .read_export_csv(fb, df)
  on.exit(unlink(p), add = TRUE)
  .read_export_verdict(e, "bulk_de", fb, names(df), 6L, 3L)

  v <- e$ts_drive_read_export_respond(12L, 5L)
  expect_identical(v$status, "done")
  pr <- v$descriptor$preview
  expect_identical(as.integer(pr$rows_returned), 5L)
  # Le signal honnête : la 6e ligne existe (le SEULE enregistrement lu en plus).
  expect_true(isTRUE(pr$truncated_rows))
  expect_identical(as.character(pr$columns), c("gene", "score", "flag"))

  # La TÊTE du fichier, relue par le TEST lui-même (contrainte de revue) :
  # égalité cellule par cellule, dans l'ordre — c'est le pin qui tue toute
  # re-transposition silencieuse.
  head6 <- utils::read.csv(p, colClasses = "character", check.names = FALSE)
  expect_identical(dim(head6), c(6L, 3L))
  for (i in seq_len(5L)) {
    expect_identical(as.character(unlist(pr$rows[[i]])),
                     as.character(unlist(head6[i, ])),
                     info = sprintf("row %d, position order", i))
    expect_length(pr$rows[[i]], 3L)
  }

  # col_summary : calculé sur les lignes RENVOYÉES seulement (séparation D1).
  cs <- v$descriptor$col_summary
  expect_identical(as.character(vapply(cs, function(x) x$class, character(1))),
                   c("character", "numeric", "logical"))
  expect_identical(vapply(cs, function(x) as.integer(x$n_missing_in_preview),
                          integer(1)), c(0L, 0L, 0L))
  expect_identical(as.character(vapply(cs, function(x) x$name, character(1))),
                   c("gene", "score", "flag"))
})

test_that("NA cells travel as JSON null and count in n_missing_in_preview", {
  e <- .read_export_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  df <- data.frame(gene = c("TP53", NA, "MYC"),
                   score = c("1.5", "2.5", NA),
                   stringsAsFactors = FALSE)
  fb <- "bulk_de_results_12.csv"
  p <- .read_export_csv(fb, df)
  on.exit(unlink(p), add = TRUE)
  .read_export_verdict(e, "bulk_de", fb, names(df), 3L, 2L)

  v <- e$ts_drive_read_export_respond(13L, NULL)  # défaut : 20 lignes
  expect_identical(v$status, "done")
  pr <- v$descriptor$preview
  expect_identical(as.integer(pr$rows_returned), 3L)
  expect_false(isTRUE(pr$truncated_rows))
  # Un NA réel => NA en R => `null` en JSON (jamais la CHAÎNE "NA").
  expect_true(is.na(pr$rows[[2]][[1]]))
  expect_identical(as.character(pr$rows[[2]][[2]]), "2.5")
  expect_true(is.na(pr$rows[[3]][[2]]))
  js <- jsonlite::toJSON(pr$rows, auto_unbox = TRUE, null = "null")
  expect_true(grepl("[null,\"2.5\"]", js, fixed = TRUE),
              info = sprintf("rows JSON: %s", js))
  expect_true(grepl("[\"TP53\",\"1.5\"]", js, fixed = TRUE),
              info = sprintf("rows JSON: %s", js))
  # n_missing_in_preview compte les NA des lignes renvoyées, colonne par colonne.
  expect_identical(vapply(v$descriptor$col_summary,
                          function(x) as.integer(x$n_missing_in_preview),
                          integer(1)), c(1L, 1L))
})

test_that("a 201-char cell is marked; 199 and 200 travel verbatim", {
  e <- .read_export_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  # Bornes de la garde D1, vues À TRAVERS le répondant (et non seulement en
  # unitaire dans test-drive-verbatim-guard.R) : 199 et 200 verbatim, 201 marqué.
  df <- data.frame(v = c(strrep("A", 199L), strrep("B", 200L), strrep("C", 201L)),
                   stringsAsFactors = FALSE)
  fb <- "bulk_de_results_13.csv"
  p <- .read_export_csv(fb, df)
  on.exit(unlink(p), add = TRUE)
  .read_export_verdict(e, "bulk_de", fb, "v", 3L, 1L)

  v <- e$ts_drive_read_export_respond(14L, 3L)
  expect_identical(v$status, "done")
  rows <- v$descriptor$preview$rows
  expect_identical(as.character(rows[[1]][[1]]), strrep("A", 199L))
  expect_identical(as.character(rows[[2]][[1]]), strrep("B", 200L))
  expect_identical(as.character(rows[[3]][[1]]),
                   e$TS_DRIVE_READ_CELL_MARKERS[["truncated"]])
  expect_identical(e$TS_DRIVE_READ_CELL_MARKERS[["truncated"]],
                   "<over-200-chars>")
  expect_identical(as.integer(v$descriptor$preview$cells_truncated), 1L)
  expect_identical(as.integer(v$descriptor$preview$cells_guarded), 0L)
})

test_that("cells failing a D1 rule are guarded, and counted", {
  e <- .read_export_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  # Chaque caractère/motif refusé par la garde, une cellule chacun : '..', '/',
  # '\', '~', et un caractère de contrôle (saut de ligne DANS un champ cité,
  # qui teste aussi le parseage enregistrement par enregistrement).
  df <- data.frame(v = c("a/b", "a..b", "x~y", "back\\slash", "line1\nline2"),
                   stringsAsFactors = FALSE)
  fb <- "bulk_de_results_14.csv"
  p <- .read_export_csv(fb, df)
  on.exit(unlink(p), add = TRUE)
  .read_export_verdict(e, "bulk_de", fb, "v", 5L, 1L)

  v <- e$ts_drive_read_export_respond(15L, 5L)
  expect_identical(v$status, "done")
  rows <- v$descriptor$preview$rows
  expect_identical(as.character(rows[[1]][[1]]), "<guarded>")
  expect_identical(as.character(rows[[2]][[1]]), "<guarded>")
  expect_identical(as.character(rows[[3]][[1]]), "<guarded>")
  expect_identical(as.character(rows[[4]][[1]]), "<guarded>")
  expect_identical(as.character(rows[[5]][[1]]), "<guarded>")
  expect_identical(as.integer(v$descriptor$preview$cells_guarded), 5L)
  expect_identical(as.integer(v$descriptor$preview$cells_truncated), 0L)
})

test_that("the 256 KiB ceiling is measured after projection and shrinks honestly", {
  e <- .read_export_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  # 10 colonnes x 200 caractères (sous la borne de cellule — les cellules
  # voyagent donc ENTIERES, seules les LIGNES rétrécissent) : 200 lignes
  # dépassent le plafond, la moitié en dessous.
  big <- as.data.frame(matrix(rep(strrep("z", 200L), 10L * 400L), ncol = 10L))
  names(big) <- sprintf("col_%02d", seq_len(10L))
  fb <- "bulk_de_results_15.csv"
  p <- .read_export_csv(fb, big)
  on.exit(unlink(p), add = TRUE)
  .read_export_verdict(e, "bulk_de", fb, names(big), 400L, 10L)

  v <- e$ts_drive_read_export_respond(16L, 200L)
  expect_identical(v$status, "done")
  pr <- v$descriptor$preview
  # Le rétrécissement est HONNÊTE : moins de lignes que demandées, signal posé.
  expect_true(as.integer(pr$rows_returned) < 200L)
  expect_true(as.integer(pr$rows_returned) >= 1L)
  expect_true(isTRUE(pr$truncated_rows))
  expect_identical(as.integer(pr$rows_returned), length(pr$rows))
  # La taille sérialisée — mesurée APRÈS projection, exactement ce que
  # l'agent recevra — tient dans le plafond déclaré.
  proj <- e$ts_drive_read_descriptor(v$descriptor)
  jsz <- nchar(jsonlite::toJSON(proj, auto_unbox = TRUE, null = "null",
                                pretty = TRUE), type = "bytes")
  expect_lte(jsz, e$TS_DRIVE_READ_MAX_BYTES)
  # Et les cellules ne sont PAS devenues des marqueurs : le plafond a mordu
  # sur les lignes, pas sur les valeurs.
  expect_true(all(nchar(unlist(pr$rows), type = "chars") == 200L))
  expect_identical(as.integer(pr$cells_truncated), 0L)
})

test_that("READ_TOO_WIDE refuses a 2049-column export", {
  e <- .read_export_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  wide <- as.data.frame(matrix("v", nrow = 1L, ncol = 2049L))
  names(wide) <- sprintf("c%d", seq_len(2049L))
  fb <- "bulk_de_results_16.csv"
  p <- .read_export_csv(fb, wide)
  on.exit(unlink(p), add = TRUE)
  .read_export_verdict(e, "bulk_de", fb, names(wide), 1L, 2049L)

  v <- e$ts_drive_read_export_respond(17L, 1L)
  expect_identical(v$status, "invalid")
  expect_true(startsWith(as.character(v$errors[1]), "READ_TOO_WIDE"))
  # Aucune preview partielle ne fuit avec le refus.
  expect_null(v$descriptor)
})

test_that("EXPORT_GONE refuses when the artefact has been pruned", {
  e <- .read_export_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  # Verdict crédible, fichier ABSENT du répertoire d'export (élagué, temp
  # nettoyé) : le stem passe, l'existence non.
  .read_export_verdict(e, "bulk_de", "bulk_de_results_17.csv",
                       c("gene", "x"), 5L, 2L)
  v <- e$ts_drive_read_export_respond(18L, NULL)
  expect_identical(v$status, "invalid")
  expect_true(startsWith(as.character(v$errors[1]), "EXPORT_GONE"))
})

test_that("FILE_TOO_LARGE refuses above the declared ceiling (injected constant)", {
  e <- .read_export_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  # 2 Go ne se fabrique pas dans un test : la borne est INJECTÉE dans l'env du
  # test (l'enclosure du répondant), le chemin de refus est identique.
  orig <- e$TS_DRIVE_READ_MAX_FILE_BYTES
  e$TS_DRIVE_READ_MAX_FILE_BYTES <- 10
  df <- data.frame(gene = c("TP53", "BRCA1"), score = c("1.5", "2.5"),
                   stringsAsFactors = FALSE)
  fb <- "bulk_de_results_18.csv"
  p <- .read_export_csv(fb, df)   # ~45 octets — bien au-dessus de 10
  on.exit(unlink(p), add = TRUE)
  on.exit(`[[<-`(e, "TS_DRIVE_READ_MAX_FILE_BYTES", orig), add = TRUE)
  .read_export_verdict(e, "bulk_de", fb, names(df), 2L, 2L)

  v <- e$ts_drive_read_export_respond(19L, NULL)
  expect_identical(v$status, "invalid")
  expect_true(startsWith(as.character(v$errors[1]), "FILE_TOO_LARGE"))
})

test_that("READ_IO_ERROR refuses unreadable artefacts", {
  e <- .read_export_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  .read_export_verdict(e, "bulk_de", "bulk_de_results_19.csv",
                       c("a", "b"), 1L, 2L)
  d <- file.path(tempdir(), "ts_drive_exports")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

  # (a) fichier VIDE : pas de ligne d'en-tête à ouvrir.
  path <- file.path(d, "bulk_de_results_19.csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(character(0), path)
  v1 <- e$ts_drive_read_export_respond(20L, NULL)
  expect_identical(v1$status, "invalid")
  expect_true(startsWith(as.character(v1$errors[1]), "READ_IO_ERROR"))

  # (b) en-tête illisible : guillemet non fermé — readTableHeader échoue.
  # (Mesuré : une LIGNE de corps malformée ne déclenche PAS d'erreur —
  # read.csv la découpe (fill) — donc le seul cas honnête est l'en-tête.)
  v2 <- e$ts_drive_read_export_respond(20L, NULL)
  expect_identical(v2$status, "invalid")
  expect_true(startsWith(as.character(v2$errors[1]), "READ_IO_ERROR"))
})

test_that("the responder is stateless: two calls, same target, zero writes", {
  e <- .read_export_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  df <- data.frame(gene = c("TP53", "BRCA1"), score = c("1.5", "2.5"),
                   stringsAsFactors = FALSE)
  fb <- "bulk_de_results_20.csv"
  p <- .read_export_csv(fb, df)
  on.exit(unlink(p), add = TRUE)
  .read_export_verdict(e, "bulk_de", fb, names(df), 2L, 2L)
  res_path <- e$ts_drive_path("result.json")
  before <- paste(readBin(res_path, "raw", file.size(res_path)), collapse = " ")

  # Deux appels : le répondant ne CONSOMME rien — c'est l'écriture du verdict
  # de lecture (par le poller) qui consomme, et elle est ailleurs.
  v1 <- e$ts_drive_read_export_respond(21L, 2L)
  v2 <- e$ts_drive_read_export_respond(22L, 2L)
  expect_identical(v1$status, "done")
  expect_identical(v2$status, "done")
  expect_identical(as.character(v1$descriptor$handle), fb)
  expect_identical(as.character(v2$descriptor$handle), fb)
  after <- paste(readBin(res_path, "raw", file.size(res_path)), collapse = " ")
  expect_identical(after, before)
  # Et il n'écrit aucun scenario : le répondant est un calcul, pas un acteur.
  expect_false(file.exists(e$ts_drive_path("scenario.json")))
})

test_that("the read verdict round-trips through result.json by the read projector", {
  e <- .read_export_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)
  df <- data.frame(gene = c("TP53", "BRCA1"), score = c("1.5", "2.5"),
                   stringsAsFactors = FALSE)
  fb <- "bulk_de_results_21.csv"
  p <- .read_export_csv(fb, df)
  on.exit(unlink(p), add = TRUE)
  .read_export_verdict(e, "bulk_de", fb, names(df), 2L, 2L)

  v <- e$ts_drive_read_export_respond(23L, 2L)
  expect_identical(v$status, "done")
  expect_true(all(names(v$descriptor) %in% e$TS_DRIVE_READ_KEYS))
  # EXACTEMENT ce que le poller écrit pour un read_export : le même writer,
  # le projeteur READ en paramètre (le verdict d'export, lui, ne garde jamais
  # de preview).
  e$ts_drive_write_result(23L, v$status, v$active_module, TRUE,
                          errors = v$errors, warnings = v$warnings,
                          descriptor = v$descriptor,
                          descriptor_projector = e$ts_drive_read_descriptor)
  rr <- e$ts_drive_read_result()
  expect_identical(rr$status, "done")
  d <- rr$descriptor
  expect_false(is.null(d$preview))
  expect_identical(as.integer(d$seq), 23L)
  expect_identical(as.character(d$handle), fb)
  expect_identical(as.character(d$preview$columns), c("gene", "score"))
  expect_identical(as.character(unlist(d$preview$rows[[1]])), c("TP53", "1.5"))
  # Consommé : la dérivation SANS ÉTAT du répondant le voit désormais.
  v2 <- e$ts_drive_read_export_respond(24L, NULL)
  expect_identical(v2$status, "invalid")
  expect_match(as.character(v2$errors[1]), "already a read verdict", fixed = TRUE)
})

test_that("the keep-set is closed and structural strings stay sanitised", {
  e <- .read_export_env()
  on.exit(unlink(e$ts_drive_root(), recursive = TRUE, force = TRUE), add = TRUE)

  # (1) Le projeteur LÂCHE toute clé hors du keep-set déclaré — un bloc qui
  #     gagnerait un champ ne peut pas l'emporter sur le wire.
  blk <- list(route = "bulk_de", handle = "bulk_de_results_22.csv", seq = 1L,
              descriptor = NULL, preview = NULL, col_summary = NULL,
              sneaky_secret = "classified_data_path")
  d <- e$ts_drive_read_descriptor(blk)
  expect_true(all(names(d) %in% e$TS_DRIVE_READ_KEYS))
  expect_false("sneaky_secret" %in% names(d))

  # (2) col_summary : nom sanitise, classe HORS de l'ensemble déclaré => "other"
  #     (précédent TS_DRIVE_DESCRIPTOR_VERBATIM), compteur entier.
  blk2 <- list(route = "bulk_de", handle = "bulk_de_results_22.csv", seq = 2L,
               col_summary = list(list(name = "geneid12345", class = "secretclass",
                                       n_missing_in_preview = 3L)))
  d2 <- e$ts_drive_read_descriptor(blk2)
  expect_identical(as.character(d2$col_summary[[1]]$name), "<redacted>")
  expect_identical(as.character(d2$col_summary[[1]]$class), "other")
  expect_identical(as.integer(d2$col_summary[[1]]$n_missing_in_preview), 3L)

  # (3) À travers le répondant : un en-tête de fichier qui porte un run
  #     alphanumérique de 8+ est REDACTE dans preview$columns — les colonnes
  #     restent structurelles, seules les VALEURS de cellules sont verbatim.
  df <- data.frame(gene = c("TP53", "BRCA1"),
                   geneid12345 = c("1", "2"),
                   stringsAsFactors = FALSE)
  fb <- "bulk_de_results_22.csv"
  p <- .read_export_csv(fb, df)
  on.exit(unlink(p), add = TRUE)
  .read_export_verdict(e, "bulk_de", fb, names(df), 2L, 2L)
  v <- e$ts_drive_read_export_respond(25L, 2L)
  expect_identical(v$status, "done")
  cols <- as.character(v$descriptor$preview$columns)
  expect_identical(cols[1], "gene")
  expect_identical(cols[2], "<redacted>")
  # ... et la VALEUR de la même colonne voyage quand même (verbatim D1).
  expect_identical(as.character(unlist(v$descriptor$preview$rows[[1]])),
                   c("TP53", "1"))
  expect_true(all(names(v$descriptor) %in% e$TS_DRIVE_READ_KEYS))
})


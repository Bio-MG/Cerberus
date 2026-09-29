# =============================================================================
# test-mod-import-spatial-drive.R â€” the `import_spatial` drive importer (Phase F)
# =============================================================================
# WHY THIS FILE EXISTS
#   Phase E closed with Spatial BLOCKED at import and named the reason as a
#   zero-height picker container. That diagnosis was MEASURED FALSE on 2026-09-26
#   (see docs/mcp_propagation.md Â§10.4): `#import_spatial-dir_select_ui` carries
#   Shiny's own `display: contents`, so it generates no box and measures 0x0 while
#   the `shinyDirButton` inside it renders at 352x37, enabled, in the viewport.
#   The real blocker is that a native directory chooser cannot be answered by an
#   automated client â€” and the fix for THAT is a drive importer, not a CSS rule.
#
# WHAT IS PROVEN HERE, AND HOW
#   * the CONTRACT, in pure R with no session: the per-module key schema, the
#     directory validator (`..`, outside-root, file-vs-directory), the
#     technology enum, and the sample-name rule;
#   * the WIRING, read from the module's own source through the shared AST
#     harness â€” the published importer must call the shared writer, and there
#     must be exactly ONE writer;
#   * the HAPPY PATH, as a REAL domain execution: a synthesised Slide-seq tree in
#     `tempdir()` goes through the module's own `run_spatial_import()`, which
#     calls the real `load_spatial_slideseq()` and the real
#     `convert_to_bpcells_and_fov()` (BPCells). No mock anywhere on that path, so
#     a green result means the modality's import works offline, not that a stub
#     was called;
#   * the ERROR PATHS, including the distinction that matters most: a folder the
#     watcher ACCEPTED and a loader REJECTED must answer `error`, never `invalid`
#     â€” `invalid` would tell the agent to send a different payload, and no
#     different payload would help;
#   * the READINESS guard, whose whole job is to stop `done` being reported for an
#     import that never ran.
#
# FALSIFIABILITY. Every assertion below was checked against a deliberately broken
# variant before being kept; the three that matter are noted inline. The heavy
# execution is gated on BPCells rather than assumed.
# =============================================================================

Sys.setlocale("LC_CTYPE", "fr_FR.UTF-8")

# The module, plus the two drive-core files whose functions the assertions below
# call directly (`ts_drive_nav_plan`, `ts_drive_perform_nav`,
# `ts_drive_publish_importer`). Sourced explicitly rather than relied upon from a
# sibling test file: `source_project_file()` puts things in `globalenv()`, and a
# test that silently depends on whichever file testthat happened to load first is
# a test that passes for the wrong reason when run alone.
source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")
# The real Spatial loaders. The happy path below is a REAL domain execution, so
# these must be the shipped functions and not stubs â€” a stub would turn the whole
# point of that test into a tautology.
source_project_file("R/core/error_state.R")
source_project_file("R/core/state.R")
source_project_file("R/core/io_helpers.R")
source_project_file("R/spatial/spatial_io.R")
source_project_file("modules/import/mod_import_spatial.R")

.MISD_FILE <- "modules/import/mod_import_spatial.R"
.misd_read <- function() paste(readLines(file.path(ts_project_root(), .MISD_FILE),
                                        warn = FALSE), collapse = "\n")

# -----------------------------------------------------------------------------
# A REAL, MINIMAL Slide-seq tree. 40 beads x 60 genes is the smallest shape the
# loader accepts on its own terms: `load_spatial_slideseq()` refuses a barcode
# intersection below 10, so anything smaller would test the refusal and not the
# import.
#
# Deterministic on purpose (`set.seed`): a fixture that moved would make a
# failure impossible to reproduce, and "it passed yesterday" is not evidence.
# -----------------------------------------------------------------------------
.misd_fixture <- function() {
  set.seed(20260926L)
  root <- file.path(tempdir(), "ts-misd-slideseq")
  unlink(root, recursive = TRUE, force = TRUE)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  n_bead <- 40L
  n_gene <- 60L
  barcodes <- sprintf("bead_%03d-1", seq_len(n_bead))
  genes <- sprintf("GENE%03d", seq_len(n_gene))
  m <- Matrix::sparseMatrix(
    i = sample(n_gene, 900L, replace = TRUE),
    j = sample(n_bead, 900L, replace = TRUE),
    x = as.numeric(sample(1:60, 900L, replace = TRUE)),
    dims = c(n_gene, n_bead), dimnames = list(genes, barcodes))
  Matrix::writeMM(m, file.path(root, "matrix.mtx"))
  writeLines(barcodes, file.path(root, "barcodes.tsv"))
  # `col.names = FALSE`: a header line makes `Seurat::ReadMtx()` count 61
  # features for a 60-row matrix and refuse the whole import. Measured, not
  # assumed.
  write.table(data.frame(gene = genes, feature_type = "Gene Expression"),
              file.path(root, "features.tsv"), sep = "\t", row.names = FALSE,
              quote = FALSE, col.names = FALSE)
  write.table(data.frame(barcode = barcodes,
                         x = round(seq(0, 10, length.out = n_bead), 3),
                         y = round(rev(seq(0, 10, length.out = n_bead)), 3)),
              file.path(root, "coords.tsv"), sep = "\t", row.names = FALSE,
              quote = FALSE)
  root
}

# The environment the writer is evaluated in. `global_data` is a real environment
# (not a list) because the writer ASSIGNS into it â€” `global_data$spatial_datasets[[
# name]] <- pkg` â€” and a list would silently accept a copy, which is the exact
# "environment recorder vs list" trap recorded for the Bulk pathway probe.
.misd_env <- function(rec = NULL) {
  e <- new.env(parent = globalenv())
  e$.tr <- function(s, ...) s
  e$.t_fmt <- function(fmt, ...) {
    out <- fmt
    for (nm in names(list(...))) {
      out <- gsub(paste0("{", nm, "}"), as.character(list(...)[[nm]]), out, fixed = TRUE)
    }
    out
  }
  e$`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  # The frozen drive constants the module's own code names. Without them the
  # importer body would fail to CLOSE, and the failure would look like a product
  # bug rather than a missing fixture.
  e$TS_DRIVE_SPATIAL_IMPORT_MODULE <- TS_DRIVE_SPATIAL_IMPORT_MODULE
  e$ts_drive_publish_importer <- ts_drive_publish_importer
  e$global_data <- new.env(parent = emptyenv())
  e$global_data$spatial_datasets <- list()
  e$global_data$active_spatial_dataset <- NULL
  e$global_data$spatial_results_cache <- list()
  e$add_log <- function(msg) { if (!is.null(rec)) rec(paste0("log:", msg)); invisible(NULL) }
  e$showNotification <- function(msg, ...) {
    if (!is.null(rec)) rec(paste0("notify:", msg)); invisible(NULL)
  }
  # `withProgress` / `incProgress` are Shiny progress primitives with no meaning
  # outside a session. They are stubbed to PASS THROUGH (`expr` untouched) rather
  # than to `force()` a value: a stub that evaluated its argument would change
  # lazy-evaluation semantics and could mask an error the real app would raise.
  e$withProgress <- function(expr, ...) expr
  e$incProgress <- function(...) invisible(NULL)
  # The widget values a drive run inherits: the DECLARED DEFAULTS, because the
  # drive never opened the panel. This is the point of reading them by
  # `isolate()` â€” an agent that touched nothing gets the same numbers a human
  # would have got from the untouched widgets.
  e$input <- list(technology = "slideseq", sample_name = "", min_counts = 0,
                  min_features = 0, min_counts_ss = 0, min_features_ss = 0,
                  max_sketch = 5000, norm_method = "lognorm", simplify_tol = 20,
                  load_raw_also = FALSE, orient_swap_xy = FALSE,
                  orient_flip_x = FALSE, orient_flip_y = FALSE, hd_bin_size = NULL)
  e
}

# The writer, extracted BY NAME through the shared harness. Not a copy: the
# module's own `run_spatial_import <-` right-hand side, evaluated in `envir` so
# that env becomes its closure. This is what makes the happy path a real
# execution of the shipped code.
.misd_writer <- function(envir) {
  ts_ast_assignment(.MISD_FILE, "run_spatial_import", envir)
}

# -----------------------------------------------------------------------------
# 1. THE CONTRACT â€” pure R, no session
# -----------------------------------------------------------------------------
test_that("import_spatial publishes a FOLDER contract, and a folder is not a file", {
  # `.ts_drive_resolve_import_path()` is named here on purpose (C9b): it is the
  # PRIVATE half both validators share, and it is the security-relevant half â€” the
  # `..` refusal and the roots confinement. A rule that only ever appears in the two
  # public wrappers is a rule a future reader has to take on trust.
  expect_true(is.function(.ts_drive_resolve_import_path))
  expect_false(.ts_drive_resolve_import_path("", roots = tempdir())$ok)
  expect_match(.ts_drive_resolve_import_path(file.path(tempdir(), "..", "x"),
                                             roots = tempdir())$reason, "\\.\\.")

  root <- file.path(tempdir(), "ts-misd-roots")
  unlink(root, recursive = TRUE, force = TRUE)
  dir.create(file.path(root, "data", "outs"), recursive = TRUE, showWarnings = FALSE)
  writeLines("a,b", file.path(root, "data", "outs", "counts.csv"))
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  # A directory is accepted where the Bulk validator REFUSES one. That refusal is
  # the reason a shared validator could not simply be reused: it is the mirror
  # case, not the same case.
  ok <- ts_drive_validate_import_dir(file.path(root, "data", "outs"), roots = root)
  expect_true(ok$ok)
  expect_null(ok$reason)
  expect_true(dir.exists(ok$path))

  # â€¦and the mirror refusal is a FILE here, with wording that says which way round
  # the mistake is. An agent told only FALSE retries the same payload forever.
  bad <- ts_drive_validate_import_dir(file.path(root, "data", "outs", "counts.csv"),
                                      roots = root)
  expect_false(bad$ok)
  expect_match(bad$reason, "is a file")
  expect_match(bad$reason, "data FOLDER")

  # The Bulk validator still refuses the directory, with ITS wording. Falsified:
  # making either validator accept both kinds would have removed the one signal
  # that tells an agent it named the wrong thing.
  expect_match(
    ts_drive_validate_import_path(file.path(root, "data", "outs"), roots = root)$reason,
    "is a directory")

  # spec S11 confinement applies to directories exactly as it does to files.
  expect_match(
    ts_drive_validate_import_dir(file.path(root, "..", "escape"), roots = root)$reason,
    "\\.\\.")
  expect_match(
    ts_drive_validate_import_dir(file.path(dirname(root), "elsewhere"), roots = root)$reason,
    "outside every allowlisted root")
  expect_match(
    ts_drive_validate_import_dir(file.path(root, "nope"), roots = root)$reason,
    "does not exist")

  # ── S11 CONFINEMENT BOUNDARY (2026-09-29) ──────────────────────────────────
  # The predicate above is a raw string PREFIX test, so a sibling whose name
  # merely STARTS with the root's characters was ACCEPTED: with root
  # `.../ts-misd-roots`, the path `.../ts-misd-roots-EVIL/secret.csv` shares 13
  # characters and was read, i.e. a path outside every allowlisted root. `..` is
  # refused, so this was the whole remaining hole, and it was ONE predicate wide.
  # 🔑 The sibling is built as an ABSOLUTE path on purpose: spelled with `..` it
  # would be refused by the traversal rule, and the test would pass for the wrong
  # reason. Both kinds exist here, because both are accepted by the two public
  # wrappers and both must be confined identically.
  evil <- file.path(tempdir(), "ts-misd-roots-EVIL")
  unlink(evil, recursive = TRUE, force = TRUE)
  dir.create(evil, recursive = TRUE, showWarnings = FALSE)
  writeLines("secret", file.path(evil, "secret.csv"))
  on.exit(unlink(evil, recursive = TRUE, force = TRUE), add = TRUE)

  # `paste0()` on the reason, not `expect_match` on it: a NULL reason (the path
  # was ACCEPTED, so there is nothing to say) must read as a FAILURE, not as an
  # ERROR — a red that crashes says "the test is broken", not "the hole is open".
  evil_file <- ts_drive_validate_import_path(file.path(evil, "secret.csv"), roots = root)
  expect_false(evil_file$ok,
               info = "a sibling sharing the root's PREFIX must not be readable")
  expect_true(grepl("outside every allowlisted root", paste0(evil_file$reason)),
              info = "the refusal must NAME the boundary it is refusing")
  expect_false(
    ts_drive_validate_import_dir(evil, roots = root)$ok,
    info = "the same hole, through the DIRECTORY wrapper")

  # NON-VACUITY, both directions, and the boundary cases the fix must not break.
  # "Refuse everything" passes the three assertions above; these make the criterion
  # falsifiable, and they are the reason the fix cannot be a blunt one:
  #   * a genuine nested FILE and the root's own DIRECTORY stay accepted;
  #   * so does the root ITSELF, which is a legitimate thing to name;
  #   * a root written WITH a trailing separator still accepts what is inside it
  #     (naively appending "/" to it would produce "//" and refuse everything);
  #   * and a drive-letter root, where the separator is already the last character.
  expect_true(
    ts_drive_validate_import_path(file.path(root, "data", "outs", "counts.csv"),
                                  roots = root)$ok,
    info = "a real file inside the root must stay importable")
  expect_true(ts_drive_validate_import_dir(root, roots = root)$ok,
              info = "naming the root itself must stay legal")
  expect_true(
    ts_drive_validate_import_dir(file.path(root, "data", "outs"),
                                 roots = paste0(root, "/"))$ok,
    info = "a root written with a trailing separator must still accept what is inside")
  expect_true(
    ts_drive_validate_import_path(file.path(root, "data", "outs", "counts.csv"),
                                  roots = substr(dirname(root), 1, 3))$ok,
    info = "a drive-letter root must keep working")
})

test_that("the import block routes on the module and refuses the other importer's keys", {
  root <- file.path(tempdir(), "ts-misd-sch")
  unlink(root, recursive = TRUE, force = TRUE)
  dir.create(file.path(root, "outs"), recursive = TRUE, showWarnings = FALSE)
  writeLines("x", file.path(root, "counts.tsv"))
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  outs <- file.path(root, "outs")

  # HAPPY: dir_path + optional technology, and the DEFAULTS are declared.
  v <- ts_drive_validate_import(list(dir_path = outs), roots = root, module = "import_spatial")
  expect_true(v$ok, info = paste(v$errors, collapse = "; "))
  expect_identical(v$import$technology, "visium")
  expect_identical(v$import$sample_name, "outs")   # the module's own basename() rule

  # The Bulk keys are a real refusal, and the message says WHO owns them: a
  # "silently dropped" `counts_path` would leave an agent retrying a payload that
  # can never reach a Spatial loader.
  cross <- ts_drive_validate_import(list(dir_path = outs, counts_path = file.path(root, "counts.tsv")),
                                    roots = root, module = "import_spatial")
  expect_false(cross$ok)
  expect_match(paste(cross$errors, collapse = " "), "belongs to import_bulk")

  # The reverse direction, same rule.
  rev <- ts_drive_validate_import(list(counts_path = file.path(root, "counts.tsv"), dir_path = outs),
                                  roots = root, module = "import_bulk")
  expect_false(rev$ok)
  expect_match(paste(rev$errors, collapse = " "), "belongs to import_spatial")

  # `dir_path` is REQUIRED, and the message names the key the module needs rather
  # than complaining that a string is missing â€” a payload with no path at all
  # would otherwise be reported as a malformed string the agent never sent.
  miss <- ts_drive_validate_import(list(technology = "visium"), roots = root, module = "import_spatial")
  expect_false(miss$ok)
  expect_match(paste(miss$errors, collapse = " "), "`dir_path` is required by 'import_spatial'",
               fixed = TRUE)

  # A module with no importer is refused by NAME, with the list of real ones.
  none <- ts_drive_validate_import(list(dir_path = outs), roots = root, module = "spatial_pipeline")
  expect_false(none$ok)
  expect_match(paste(none$errors, collapse = " "), "publishes no importer")
  expect_match(paste(none$errors, collapse = " "), "import_bulk, import_spatial")
})

test_that("technology is a frozen enum, and sample_name follows the module's own rule", {
  root <- file.path(tempdir(), "ts-misd-tech")
  unlink(root, recursive = TRUE, force = TRUE)
  dir.create(file.path(root, "puck"), recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  puck <- file.path(root, "puck")

  for (tech in TS_DRIVE_SPATIAL_TECHNOLOGIES) {
    v <- ts_drive_validate_import(list(dir_path = puck, technology = tech),
                                  roots = root, module = "import_spatial")
    expect_true(v$ok, info = tech)
    expect_identical(v$import$technology, tech)
  }
  # MEASURED against the radio, not invented: an unknown technology selects a
  # different loader in a `switch()` with no fallthrough, so it is refused HERE
  # rather than inside the import.
  bad <- ts_drive_validate_import(list(dir_path = puck, technology = "visium_hd"),
                                  roots = root, module = "import_spatial")
  expect_false(bad$ok)
  expect_match(paste(bad$errors, collapse = " "), "`technology` must be one of")

  # An explicit name wins, trimmed; a BLANK one falls back to the rule rather
  # than producing a dataset keyed on "". The fallback is the MODULE's own test
  # (`if (nchar(trimws(input$sample_name)))`, mod_import_spatial.R), so a
  # whitespace-only box in the UI and a whitespace-only name in a scenario name
  # the same dataset.
  named <- ts_drive_validate_import(list(dir_path = puck, sample_name = "  puck_a  "),
                                    roots = root, module = "import_spatial")
  expect_identical(named$import$sample_name, "puck_a")
  blank <- ts_drive_validate_import(list(dir_path = puck, sample_name = "   "),
                                    roots = root, module = "import_spatial")
  expect_true(blank$ok)
  expect_identical(blank$import$sample_name, "puck")
  # A trailing separator must not leak into the derived name.
  expect_identical(.ts_drive_spatial_sample_name("a/b/puck/"), "puck")
  expect_identical(.ts_drive_spatial_sample_name("a/b/puck", requested = "  "), "puck")
  # â€¦and a folder whose basename is unrecoverable says so rather than inventing "".
  expect_true(is.na(.ts_drive_spatial_sample_name("/")))
})

# -----------------------------------------------------------------------------
# 2. THE WIRING â€” read from the module's own source
# -----------------------------------------------------------------------------
test_that("the module publishes an importer that calls the SHARED writer", {
  # The single-writer invariant, measured rather than asserted in prose. Two
  # `convert_to_bpcells_and_fov()` call sites in this module would mean two
  # import bodies to keep in step, and the drive would reach one of them by
  # accident.
  p <- ts_ast_parse(.MISD_FILE)
  sites <- ts_ast_find_all(p, function(x) ts_ast_is_call_to(x, "convert_to_bpcells_and_fov"))
  expect_length(sites, 1L)

  published <- ts_ast_find_all(p, function(x) ts_ast_is_call_to(x, "ts_drive_publish_importer"))
  expect_length(published, 1L)
  # The published function must take its three scalars FROM THE REQUEST, by the
  # validated names. A writer called with `input$â€¦` here would be the widget
  # faking spec S5 forbids, and it is invisible at the call site.
  txt <- paste(deparse(published[[1]]), collapse = "\n")
  expect_match(txt, "run_spatial_import", fixed = TRUE)
  expect_match(txt, "request$dir_path", fixed = TRUE)
  expect_match(txt, "request$sample_name", fixed = TRUE)
  expect_match(txt, "request$technology", fixed = TRUE)
  expect_false(grepl("input\\$", txt))

  # â€¦and the BUTTON path is a SECOND caller of the same writer, through ONE
  # observer with TWO triggers (`drive_trigger`, the `mod_import_bulk` idiom).
  # Exactly ONE observer: two observers on the same button would mean two bodies
  # to keep in step, and `check_duplication.R` counts repeated triggers per file.
  obs <- ts_ast_find_all(p, function(x) ts_ast_is_call_to(x, "observeEvent"))
  bodies <- vapply(obs, function(x) paste(deparse(x, width.cutoff = 500L),
                                          collapse = "\n"), character(1))
  callers <- grep("run_spatial_import", bodies, fixed = TRUE)
  expect_length(callers, 1L)
  expect_match(bodies[[callers]], "drive_trigger", fixed = TRUE)
  expect_match(bodies[[callers]], "req(dir_path())", fixed = TRUE)
  # The trigger itself must depend on BOTH the button and the drive counter, or
  # one of the two paths is dead. Measured against the SOURCE, not the
  # `observeEvent` list: `drive_trigger` is a `reactive()`, so it is not in `obs`
  # and searching that list for it would be a search that can never match.
  expect_match(.misd_read(),
               "drive_trigger <- shiny::reactive(list(drive_counter(), input$btn_import))",
               fixed = TRUE)
  # The falsifier: a trigger built from the counter alone would leave the human's
  # click doing nothing, and this is what makes the assertion above discriminating.
  expect_false(grepl("drive_trigger <- shiny::reactive(list(drive_counter()))",
                     .misd_read(), fixed = TRUE))
})

test_that("the bound Spatial import button is a real DOM id with a readiness guard", {
  # The allowlist id must be the module's own `actionButton(ns("btn_import"))` id
  # and not a plausible-looking invention. Checked against the SOURCE (the DOM
  # was measured live on 2026-09-26 and carries the same id).
  src <- .misd_read()
  expect_match(src, 'actionButton(ns("btn_import")', fixed = TRUE)
  expect_identical(TS_DRIVE_SPATIAL_IMPORT_BUTTON, "import_spatial-btn_import")
  expect_identical(ts_drive_module_of(TS_DRIVE_SPATIAL_IMPORT_BUTTON), "import_spatial")
  expect_identical(ts_drive_button_module(TS_DRIVE_SPATIAL_IMPORT_BUTTON), "import_spatial")
  expect_true(TS_DRIVE_SPATIAL_IMPORT_BUTTON %in% TS_DRIVE_BUTTONS)
  expect_identical(TS_DRIVE_ALLOWLIST[[TS_DRIVE_SPATIAL_IMPORT_BUTTON]]$kind, "button")
  # The module name is the convention the other thirteen follow: DOM prefix with
  # the dash turned into an underscore.
  expect_identical(TS_DRIVE_SPATIAL_IMPORT_MODULE, "import_spatial")

  # READINESS: the guard must be a NAMED refusal, not a boolean. A `ready` that
  # returned FALSE would leave the poller reporting `done` for an import that
  # never ran, which is the one lie this protocol exists to remove.
  expect_match(src, "drive_ready <- function", fixed = TRUE)
  expect_match(src, "is.null(shiny::isolate(dir_path()))", fixed = TRUE)
  expect_match(src, "no data folder selected", fixed = TRUE)
  # âš ï¸ A double-quoted R string, not a single-quoted one: inside 'â€¦' a `\"` is a
  # LITERAL backslash-quote, so a single-quoted needle with `fixed = TRUE` would
  # search for text that does not exist in the file and fail for a reason that has
  # nothing to do with the code under test.
  expect_match(src, "ts_drive_publish_token(global_data, \"import_spatial-btn_import\",",
               fixed = TRUE)
})

test_that("the Spatial import nav plan is a DELIBERATE empty one, with the reason pinned", {
  plan <- ts_drive_nav_plan("import_spatial")
  expect_null(plan$top)
  expect_null(plan$tab)
  expect_null(plan$panel)
  # Not a missing branch: the plan is SHAPED like every other plan, so a caller
  # that reads `plan$top` gets NULL rather than a missing field.
  expect_setequal(names(plan), c("top", "tab", "panel", "tab_id", "accordion_id"))
  # The MEASURED reason, so the reasoning stays falsifiable. `nav_panel()` passes
  # no `value=` for the Spatial import panel, so bslib derives the navbar value
  # from the i18n title TAG; read off a live session after clicking
  # Import > Spatial it was this HTML fragment, not an id. The evidence lives in
  # a COMMENT, and `deparse()` strips comments â€” so the source is read, not the
  # parse tree. Reading the wrong one of the two would have made this assertion
  # vacuously true.
  watcher <- paste(readLines(file.path(ts_project_root(), "R/core/drive_watcher.R"),
                             warn = FALSE), collapse = "\n")
  expect_match(watcher, "data-key=", fixed = TRUE)
  # â€¦and the empty plan is a real branch, not the fall-through: without it the
  # function would answer `top = "tab_bulk"` for a Spatial import.
  expect_match(watcher, "if (identical(module, TS_DRIVE_SPATIAL_IMPORT_MODULE))", fixed = TRUE)
  # A neighbouring module must be UNAFFECTED: an empty plan that leaked would
  # silently stop navigating the Bulk analysis tabs.
  expect_identical(ts_drive_nav_plan("bulk_de")$top, "tab_bulk")
  expect_false(is.null(ts_drive_perform_nav(NULL, plan)))
})

# -----------------------------------------------------------------------------
# 3. THE HAPPY PATH â€” a REAL domain execution, no mock on the import path
# -----------------------------------------------------------------------------
test_that("a real Slide-seq tree imports through the shared writer and registers the dataset", {
  skip_if_not_installed("BPCells")
  skip_if_not_installed("Seurat")
  root <- .misd_fixture()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  envir <- .misd_env()
  steps <- character(0)
  envir$add_log <- function(msg) { steps <<- c(steps, msg); invisible(NULL) }
  writer <- .misd_writer(envir)

  res <- suppressWarnings(writer(root, "probe", "slideseq"))

  # The verdict contract, exactly as the drive importer consumes it.
  expect_true(res$ok, info = paste(res$error, collapse = "; "))
  expect_identical(res$status, "applied")
  expect_null(res$error)
  expect_identical(res$name, "probe")
  expect_identical(res$technology, "slideseq")
  # SCALARS ONLY. The verdict is published through `result.json`, so a matrix, a
  # plot or a log riding along would be both a leak and a size problem.
  expect_true(is.integer(res$n_total) && length(res$n_total) == 1L)
  expect_true(is.integer(res$n_sketch) && length(res$n_sketch) == 1L)
  expect_identical(res$n_total, 40L)
  expect_identical(res$n_sketch, 40L)
  expect_identical(res$n_datasets, 1L)

  # The WRITER really wrote, into the environment it was given: a list would have
  # accepted a copy here and the assertion below would have been theatre.
  expect_true("probe" %in% names(envir$global_data$spatial_datasets))
  pkg <- envir$global_data$spatial_datasets[["probe"]]
  expect_equal(pkg$n_total, 40)
  expect_identical(envir$global_data$active_spatial_dataset, "probe")
  on.exit(unlink(pkg$bpcells_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # A real conversion happened â€” not a stub that returned a list.
  expect_true(dir.exists(pkg$bpcells_dir))
  expect_s4_class(pkg$sketch, "Seurat")

  # And a human saw it: the log carries the module's own completion line.
  expect_true(any(grepl("Import termine", steps, fixed = TRUE)))
})

test_that("the published importer translates the writer's verdict into the watcher contract", {
  skip_if_not_installed("BPCells")
  root <- .misd_fixture()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  # The importer FUNCTION, taken from the module's own
  # `ts_drive_publish_importer(...)` call and evaluated beside the real writer.
  # Reading the module's code is the point: a hand-written stand-in would prove
  # nothing about the shipped argument translation.
  #
  # `tail(as.list(x), 1L)` rather than `as.list(x)[[3]]`: the call's arguments are
  # `global_data`, the module constant, then the function, and a positional index
  # would silently pick the constant up if the signature ever grew a parameter.
  p <- ts_ast_parse(.MISD_FILE)
  call_node <- ts_ast_find(p, function(x) ts_ast_is_call_to(x, "ts_drive_publish_importer"))
  envir <- .misd_env()
  # The importer's closure must SEE the writer, exactly as it does inside
  # `server()`. Without this the function would close over an environment without
  # it and fail with "object not found" â€” a fixture gap dressed up as a product
  # defect.
  envir$run_spatial_import <- .misd_writer(envir)
  fn <- eval(tail(as.list(call_node), 1L)[[1]], envir = envir)
  expect_true(is.function(fn))

  # The registry path: publishing wires it under the frozen prefix.
  gd <- list(drive_registry = new.env(parent = emptyenv()))
  expect_true(ts_drive_publish_importer(gd, "import_spatial", fn))
  got <- ts_drive_importer_of(gd, "import_spatial")
  expect_true(is.function(got))
  on.exit(unlink(envir$global_data$spatial_datasets$probe$bpcells_dir,
                 recursive = TRUE, force = TRUE), add = TRUE)

  out <- got(list(dir_path = root, sample_name = "probe", technology = "slideseq"))
  expect_true(out$ok, info = paste(out$errors, collapse = "; "))
  expect_identical(out$status, "applied")
  expect_length(out$errors, 0L)
  expect_length(out$warnings, 0L)
  expect_identical(envir$global_data$active_spatial_dataset, "probe")
})

# -----------------------------------------------------------------------------
# 4. THE ERROR PATHS â€” and the refusal that must NOT be `invalid`
# -----------------------------------------------------------------------------
test_that("a folder the watcher accepted and a loader refused answers `error`, not `invalid`", {
  # The distinction is the whole point of the verdict channel: `invalid` means
  # "send a different payload", and no different payload would help for a folder
  # that is simply not a dataset. Reporting `invalid` here would send an agent
  # into a retry loop with the correct path.
  empty <- file.path(tempdir(), "ts-misd-empty")
  unlink(empty, recursive = TRUE, force = TRUE)
  dir.create(empty, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(empty, recursive = TRUE, force = TRUE), add = TRUE)

  envir <- .misd_env()
  writer <- .misd_writer(envir)
  res <- suppressWarnings(writer(empty, "probe", "slideseq"))

  expect_false(res$ok)
  expect_identical(res$status, "error")
  expect_true(nzchar(res$error))
  # The message is the DOMAIN's, not a generic one: it names what was missing.
  expect_match(res$error, "introuvable|empty|vide", ignore.case = TRUE)
  # Nothing was registered. A failed import that left a dataset behind would make
  # the next action believe the modality is ready.
  expect_length(envir$global_data$spatial_datasets, 0L)
  expect_null(envir$global_data$active_spatial_dataset)
})

test_that("an unknown technology is refused by the DOMAIN, and named as a Spatial error", {
  envir <- .misd_env()
  writer <- .misd_writer(envir)
  res <- suppressWarnings(writer(tempdir(), "probe", "not_a_technology"))
  expect_false(res$ok)
  expect_identical(res$status, "error")
  expect_match(res$error, "Technologie inconnue", fixed = TRUE)
})

test_that("an empty path is refused by the writer's own guard, before any loader runs", {
  # Falsified by giving it a REAL folder: had the guard been missing, the loader
  # would have been reached and this would still have failed â€” but for a
  # different reason, and the message would have named the loader instead.
  steps <- character(0)
  envir <- .misd_env(rec = function(s) steps <<- c(steps, s))
  writer <- .misd_writer(envir)
  res <- suppressWarnings(writer("", "probe", "slideseq"))
  expect_false(res$ok)
  expect_identical(res$status, "error")
  expect_match(res$error, "dossier", ignore.case = TRUE)
  # NO LOADER ran. The log is NOT empty â€” the writer's own handler journals the
  # refusal so a human watching sees it â€” so the invariant to assert is the
  # absence of the loader's own trace, not the absence of output. Asserting
  # "no output" here would have demanded silence from a UI that is supposed to
  # report a refusal.
  expect_false(any(grepl("load_spatial_slideseq", steps, fixed = TRUE)))
  expect_false(any(grepl("BPCells", steps, fixed = TRUE)))
  expect_true(any(grepl("Aucun dossier selectionne", steps, fixed = TRUE)))
  # And nothing was registered.
  expect_length(envir$global_data$spatial_datasets, 0L)
})

test_that("a re-import under the same name replaces it and says so", {
  skip_if_not_installed("BPCells")
  skip_if_not_installed("Seurat")
  root <- .misd_fixture()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  envir <- .misd_env()
  writer <- .misd_writer(envir)

  first <- suppressWarnings(writer(root, "probe", "slideseq"))
  expect_true(first$ok, info = first$error)
  on.exit(unlink(envir$global_data$spatial_datasets$probe$bpcells_dir,
                 recursive = TRUE, force = TRUE), add = TRUE)
  steps <- character(0)
  envir$add_log <- function(msg) { steps <<- c(steps, msg); invisible(NULL) }

  second <- suppressWarnings(writer(root, "probe", "slideseq"))
  expect_true(second$ok, info = second$error)
  expect_identical(second$n_datasets, 1L)   # replaced, not accumulated
  expect_true(any(grepl("existait deja", steps, fixed = TRUE)))
  # Re-importing the ACTIVE dataset must emit the explicit re-import signal, or
  # the centralised observer in mod_spatial.R would not re-synchronise.
  expect_false(is.null(envir$global_data$spatial_reimport_signal))
  unlink(envir$global_data$spatial_datasets$probe$bpcells_dir,
         recursive = TRUE, force = TRUE)
})

test_that("the READINESS guard refuses with a named reason when no folder is picked", {  # The guard itself is a closure inside `server()`, so it cannot be called from
  # Rscript. What CAN be pinned â€” and is what matters â€” is that it exists, that it
  # is wired to the bound button, and that it tests the module's own picked path
  # rather than a scenario string. All three are source facts.
  src <- .misd_read()
  expect_match(src, "drive_ready <- function", fixed = TRUE)
  expect_match(src, "is.null(shiny::isolate(dir_path()))", fixed = TRUE)
  expect_match(src, "no data folder selected", fixed = TRUE)
  # A `ready` that returned a bare FALSE would let the poller report `done` for
  # an import that never ran: the string is load-bearing, not decoration. (The
  # `publish_token` literal that wires this guard to the button is asserted in the
  # previous block, where it belongs.)
  # The observer that consumes the merged trigger opens with `req(dir_path())`, so
  # a doomed click aborts in silence instead of dispatching an impossible import.
  expect_match(src, "observeEvent(drive_trigger(),", fixed = TRUE)
  expect_match(src, "req(dir_path())", fixed = TRUE)
})

# -----------------------------------------------------------------------------
# 5. THE SPATIAL HOTSPOT EXPORT (Phase F) â€” the one artefact the chain could not
#    hand back. `mod_spatial_viz.R` exports its table and `mod_spatial_export.R`
#    exports a bundle, but the hotspot result existed only in `shared_rv` and in
#    the DOM, so an agent that ran `spatial-qc-btn_hotspots` had no way to OBTAIN
#    the numbers it had just produced.
# -----------------------------------------------------------------------------
test_that("the Spatial hotspot table has a CSV export that reads the SHOWN slot", {
  qc <- file.path(ts_project_root(), "modules", "spatial", "mod_spatial_qc.R")
  src <- paste(readLines(qc, warn = FALSE), collapse = "\n")
  expect_match(src, "output$dl_hotspot_csv <- downloadHandler(", fixed = TRUE)
  expect_match(src, "downloadButton(ns(\"dl_hotspot_csv\")", fixed = TRUE)
  # It must read the SAME slot the table renders. A handler that recomputed, or
  # read a different slot, could export numbers the panel never showed.
  expect_match(src, "df <- shared_rv$hotspot_result", fixed = TRUE)
  # The refusal is a `validate(need(...))` and not a bare write: an empty result
  # would otherwise produce a 0-byte file that looks like a successful export.
  expect_match(src, "validate(need(!is.null(df) && nrow(df) > 0,", fixed = TRUE)
  # `row.names = FALSE`, for the same reason the DT hides them: an exported first
  # column of row indices is a file an analyst has to clean before use.
  expect_match(src, "write.csv(df, file, row.names = FALSE)", fixed = TRUE)
  # The i18n key the guard C7 checks must be the UNACCENTED string the module
  # actually passes, exactly like the neighbouring guard in mod_spatial_viz.R.
  expect_match(src, ".tr(\"Aucun resultat de hotspots a exporter.\")", fixed = TRUE)
  keys <- jsonlite::fromJSON(file.path(ts_project_root(), "i18n", "translation.json"),
                             simplifyVector = FALSE)
  fr <- vapply(keys$translation, function(x) x$fr %||% "", character(1))
  expect_true("Aucun resultat de hotspots a exporter." %in% fr)
})

test_that("the hotspot export filename carries the dataset but never publishes it", {
  # The dataset name is legitimate in a FILENAME a human downloads and
  # illegitimate in `result.json`, which a remote caller reads. So the name may
  # reach the filename and must not reach any published field.
  qc <- file.path(ts_project_root(), "modules", "spatial", "mod_spatial_qc.R")
  src <- paste(readLines(qc, warn = FALSE), collapse = "\n")
  expect_match(src, "active_spatial_dataset", fixed = TRUE)
  expect_match(src, "gsub(\"[^A-Za-z0-9._-]\", \"_\",", fixed = TRUE)
  # â€¦and the sanitiser is applied to the NAME, so a dataset called
  # "../../etc/passwd" cannot escape the download directory through the filename.
  expect_match(src, "Sys.Date()", fixed = TRUE)
})
# =============================================================================
# GAP 1 â€” the `import_spatial` STATE PROBE
# =============================================================================
# WHY: measured LIVE on 2026-09-28 (Â§13.2/Â§13.5 of docs/mcp_propagation.md). The
# `import_file` action answered `done` and `snapshot.modules` carried NO entry for
# `import_spatial` â€” 12 modules published a probe and the three importers published
# none. So `done` on an import was indistinguishable from `done` on an import that
# loaded nothing, and only the DOM value boxes could settle it. Same defect Â§2dn
# closed for `bulk_pathways`.
#
# The probe is a CLOSURE inside `server()`, so it is extracted BY NAME through the
# shared AST harness â€” the shipped code runs, not a copy of it.
#
# `global_data` and `input` are supplied by the test because the probe's whole job
# is to read them; the corpus is REAL (a synthesised Slide-seq tree through the real
# loaders), so `has_data`/`n_genes`/`n_samples`/`n_total` are the module's own
# numbers rather than values the test chose.
.misd_probe_env <- function(envir) {
  envir$input <- list(technology = "slideseq")
  envir
}

test_that("import_spatial publishes a BOUNDED state probe, non-NULL on success and empty on nothing", {
  skip_if_not_installed("BPCells")
  skip_if_not_installed("Seurat")
  root <- .misd_fixture()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  envir <- .misd_env()
  envir$add_log <- function(msg) invisible(NULL)
  writer <- .misd_writer(envir)

  # (a) the REAL importer entry point, on a REAL corpus.
  res <- suppressWarnings(writer(root, "probe", "slideseq"))
  expect_true(res$ok, info = paste(res$error, collapse = "; "))
  pkg <- envir$global_data$spatial_datasets[["probe"]]
  envir$global_data$spatial_obj <- pkg
  .misd_probe_env(envir)

  # The WIRING: the probe is reachable through the EXISTING `state =` seam, on the
  # token the module already published. Asserted on the SOURCE so the test cannot
  # keep passing after the wiring is deleted.
  expect_match(.misd_read(), "state = drive_state", fixed = TRUE)

  probe <- ts_ast_assignment(.MISD_FILE, "drive_state", envir)
  expect_true(is.function(probe))

  # (b) present and NON-NULL on success, with the module's OWN numbers.
  st <- probe()
  expect_type(st, "list")
  expect_identical(st$module, "import_spatial")
  expect_true(st$has_data)
  expect_identical(st$n_genes, 60L)   # the fixture is 40 beads x 60 genes
  expect_identical(st$n_samples, 40L)
  expect_identical(st$n_total, 40L)
  expect_identical(st$technology, "slideseq")

  # BOUNDED, and the bound is the MODULE's job: `ts_drive_module_states()` projects
  # only `descriptor` and passes every other field through WHOLE
  # (drive_watcher.R:1164-1185), so a probe that grew a matrix would ship it.
  expect_setequal(names(st), c("module", "has_data", "n_genes", "n_samples",
                               "n_total", "technology"))
  for (f in st) {
    expect_true(is.atomic(f) || is.null(f),
                info = "a probe field must be a scalar, never a matrix or a list")
  }

  # "Nothing imported" is has_data FALSE with NULL dimensions â€” NOT a NULL probe:
  # a probe returning NULL is reported as `probe_error`, i.e. as a CRASH.
  envir$global_data$spatial_obj <- NULL
  st0 <- probe()
  expect_type(st0, "list")
  expect_false(st0$has_data)
  expect_null(st0$n_genes)
  expect_null(st0$n_samples)
  expect_null(st0$n_total)
  # `technology` is the WIDGET, not the object: it survives an empty session, and
  # saying so is the difference between "no data" and "no data, and here is the
  # loader that would have run".
  expect_identical(st0$technology, "slideseq")

  # (c) FALSIFICATION: the same predicate that passes on the real record must
  # reject a MUTATED one, or the assertions above are decorative.
  ok <- function(s) isTRUE(s$has_data) && identical(s$n_genes, 60L) &&
    identical(s$technology, "slideseq")
  expect_true(ok(st))
  for (mut in list(
    function(s) { s$n_genes <- 999L; s },
    function(s) { s$has_data <- FALSE; s },
    function(s) { s$technology <- "visium"; s },
    function(s) { s$n_genes <- NULL; s }
  )) {
    expect_failure(expect_true(ok(mut(st))), label = "a mutated probe record was accepted")
  }
})

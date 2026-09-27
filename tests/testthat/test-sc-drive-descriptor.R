# =============================================================================
# tests/testthat/test-sc-drive-descriptor.R
#
# A BOUNDED DESCRIPTOR on the four SC state probes, so `n_results` stops being a
# number with no subject.
#
# THE GAP, MEASURED (read-only census, 2026-09-27). All four SC probes publish the
# same generic contract:
#
#   status, elapsed_s, seq, n_results, has_data, ready, step/steps, error
#
# `n_results` is the only content-bearing number, and it is real per module
# (markers: nrow of the table; pathways: nrow of the enrichment; annotation: levels
# in the last annotation column; auto-pipeline: a count of produced artefacts). But
# an agent cannot act on it:
#
#   * NO REFERENT - which marker gene, which pathway, which annotation column;
#   * NO SCHEMA   - the columns are not published, so the count is uninterpretable;
#   * NO CONVENTION - `bulk_de` declares the fixed rule behind its `n_significant`
#     ("padj < 0.05 & |log2FoldChange| > 1"), so its number is reproducible. SC has
#     no equivalent, so its counts are not comparable ACROSS modules;
#   * NO BYPASS   - a real result cannot be told from a shortcut.
#
# This is the same class of gap the S2 hotspot export closed with a bounded
# descriptor (columns + n_rows + n_sig). It reuses that mechanism: NO new button,
# NO new action, NO new route.
#
# THE SECURITY DISCIPLINE IS THE POINT, and it is NOT automatic. MEASURED while
# building this: `ts_drive_module_states()` returns a probe's list WHOLE, with no
# whitelist - unlike `ts_drive_export_descriptor()`, which projects and sanitises.
# So a descriptor arriving through module state would skip both rules unless the
# projection is applied where it enters the wire.
# =============================================================================

source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")

.SCD <- function() {
  data.frame(gene = c("Cdkn1a", "Mki67", "Top2a"),
             cluster = c("c0", "c1", "c1"),
             logFC = c(1.2, -0.4, 2.9),
             padj = c(0.001, 0.4, 1e-9),
             stringsAsFactors = FALSE)
}

test_that("the descriptor is BOUNDED: schema and counts, never values", {
  d <- ts_drive_table_descriptor(.SCD(), convention = "rows of the marker table")
  # Doubles, not integers, and deliberately: JSON has no integer type, and the wire
  # projection normalises every numeric with `as.numeric()`, so asserting `3L` would
  # be asserting an accident of the local representation rather than the contract.
  expect_equal(d$n_rows, 3)
  expect_equal(d$n_cols, 4)          # gene, cluster, logFC, padj
  expect_identical(d$kind, "table")
  expect_identical(d$convention, "rows of the marker table")
  # The schema travels, so the count is interpretable.
  expect_true(all(c("gene", "cluster", "logFC", "padj") %in% d$columns))
  # 🔴 AND NOTHING ELSE. A descriptor that carried a cell would be a data leak on
  # the wire, so the property is asserted on the KEYS, not read as "it looks fine".
  expect_identical(sort(names(d)),
                   sort(c("kind", "n_rows", "n_cols", "columns", "convention")))
  # No element may be a data.frame/matrix/list: only scalars and short strings.
  for (k in names(d)) {
    v <- d[[k]]
    expect_true(is.numeric(v) || is.character(v))
    expect_false(is.data.frame(v) || is.matrix(v) || is.list(v))
  }
})

test_that("the column list is CAPPED, and long names are truncated", {
  wide <- as.data.frame(matrix(0, nrow = 1, ncol = 40))
  names(wide) <- paste0("a_very_long_column_name_number_", sprintf("%02d", 1:40))
  d <- ts_drive_table_descriptor(wide)
  # A bounded wire field: the cap is what stops a 400-column table from writing a
  # kilobyte of schema into every snapshot.
  expect_true(length(d$columns) <= 12L)
  expect_true(all(nchar(d$columns) <= 60L))
  # 🔴 The cap must be VISIBLE, not silent: an agent told "12 columns" about a
  # 40-column table would draw the wrong conclusion, so the truncation is declared.
  expect_true(isTRUE(d$columns_truncated))
  expect_equal(d$n_cols, 40)
  # And a table that fits is NOT flagged, so the flag carries information.
  small <- ts_drive_table_descriptor(.SCD())
  expect_null(small$columns_truncated)
})

test_that("absent input yields NO descriptor, never a fabricated empty one", {
  # A descriptor saying "0 rows, 0 columns" for a module that never ran is a
  # lie an agent cannot distinguish from a real empty result. `NULL` is the answer.
  expect_null(ts_drive_table_descriptor(NULL))
  expect_null(ts_drive_table_descriptor(list()))
  expect_null(ts_drive_table_descriptor(character(0)))
  expect_null(ts_drive_table_descriptor(NULL, convention = "x"))
})

test_that("a non-table result still describes itself, by kind", {
  # Annotation publishes a LEVEL COUNT, not a table: there is no frame to describe.
  d <- ts_drive_table_descriptor(NULL, kind = "levels", n_levels = 7L,
                                 convention = "unique levels in the annotation column")
  expect_identical(d$kind, "levels")
  expect_equal(d$n_levels, 7)
  expect_null(d$n_rows)
})

test_that("the wire PROJECTION whitelists and sanitises a probe's descriptor", {
  # 🔴 THE MEASURED GAP. `ts_drive_module_states()` hands a probe's list through
  # whole, so a descriptor arriving that way would carry anything the probe
  # returned. The projection is what makes the two rules of the S2 descriptor
  # ("only the DECLARED keys travel", "every string is sanitised") apply here too.
  evil <- list(
    n_rows = 3L, n_cols = 5L, convention = "rows",
    columns = c("gene", "cluster"),
    secret_path = "D:/secret/dir/file.csv",
    api_key = "TOKENabcdef123456",
    nested = list(a = 1))
  p <- ts_drive_project_descriptor(evil, TS_DRIVE_STATE_DESCRIPTOR_KEYS)
  expect_false("secret_path" %in% names(p))
  expect_false("api_key" %in% names(p))
  expect_false("nested" %in% names(p))
  # The declared keys survive.
  expect_true(all(c("n_rows", "n_cols", "convention", "columns") %in% names(p)))
  # And the sanitiser did its job on the strings that remain.
  expect_true(is.character(p$convention))
  # 🔴 The whitelist alone is NOT the second rule. A DECLARED key can still carry a
  # secret: `convention` is allowed through, so if it held a token-like run or an
  # absolute path the whitelist would happily publish it. MEASURED while building
  # this: the first version of this block only asserted the whitelist, so removing
  # the sanitiser entirely left it GREEN.
  dirty <- ts_drive_project_descriptor(
    list(n_rows = 1L,
         convention = "rows TOKENabcdef123456 at D:/secret/dir/file.csv"),
    TS_DRIVE_STATE_DESCRIPTOR_KEYS)
  expect_false(grepl("TOKENabcdef123456", dirty$convention, fixed = TRUE))
  expect_false(grepl("D:/secret", dirty$convention, fixed = TRUE))
  # A long column name is truncated on the wire too, not only when built.
  long <- ts_drive_project_descriptor(
    list(columns = paste(rep("averyveryverylongcolumnname", 40), collapse = "")),
    TS_DRIVE_STATE_DESCRIPTOR_KEYS)
  expect_true(all(nchar(long$columns) <= 200L))
})

test_that("a module probe's descriptor is PROJECTED, not passed through", {
  # End to end through the real collector: a probe that returns a descriptor with
  # an extra key must have that key removed on the wire.
  reg <- new.env(parent = emptyenv())
  reg[["sc-markers-run_markers"]] <- list(
    counter = NULL, ready = function() TRUE, long = FALSE, timeout_s = 600,
    state = function() list(
      module = "sc_markers", n_results = 3L,
      descriptor = list(n_rows = 3L, columns = c("gene"), leak = "D:/secret/x.csv")))
  gd <- list(drive_registry = reg)
  out <- ts_drive_module_states(gd)
  d <- out$sc_markers$descriptor
  expect_false(is.null(d))
  expect_false("leak" %in% names(d))
  expect_true("n_rows" %in% names(d))
  # A probe with no descriptor must not gain one.
  reg2 <- new.env(parent = emptyenv())
  reg2[["sc-markers-run_markers"]] <- list(
    counter = NULL, ready = function() TRUE, long = FALSE, timeout_s = 600,
    state = function() list(module = "sc_markers", n_results = 3L))
  out2 <- ts_drive_module_states(list(drive_registry = reg2))
  expect_null(out2$sc_markers$descriptor)
})

test_that("a DECLARED vocabulary survives the sanitiser; a user string does not", {
  # 🔴 MEASURED LIVE (2026-09-27). The convention
  # "number of pipeline artefacts produced (not rows)" came back on the wire as
  # "number of <redacted> <redacted> <redacted> (not rows)": the sanitiser redacts
  # every 8+-character alphanumeric run, and "pipeline", "artefacts" and "produced"
  # each qualify. The field whose whole purpose is to make a count INTERPRETABLE
  # arrived unreadable. Same defect the badge had with its own label "snapshot",
  # same fix: a FROZEN set is returned verbatim, before the path/token rules.
  conv <- "number of pipeline artefacts produced (not rows)"
  expect_true(conv %in% TS_DRIVE_DESCRIPTOR_VERBATIM)
  p <- ts_drive_project_descriptor(
    list(kind = "table", n_rows = 3L, columns = c("step"), convention = conv),
    TS_DRIVE_STATE_DESCRIPTOR_KEYS, verbatim = TS_DRIVE_DESCRIPTOR_VERBATIM)
  # The declared convention arrives INTACT - the whole point of the field.
  expect_identical(p$convention, conv)
  expect_false(grepl("redacted", p$convention, fixed = TRUE))
  expect_identical(p$kind, "table")

  # 🔴 AND THE EXEMPTION IS NARROW. A value that is NOT in the frozen set is still
  # sanitised, otherwise the exemption becomes a hole: a `column` holding a
  # user-supplied annotation label, or a convention a module invented at runtime,
  # must not travel raw.
  dirty <- ts_drive_project_descriptor(
    list(convention = "TOKENabcdef123456 at D:/secret/x.csv",
         column = "TOKENabcdef123456"),
    TS_DRIVE_STATE_DESCRIPTOR_KEYS, verbatim = TS_DRIVE_DESCRIPTOR_VERBATIM)
  expect_false(grepl("TOKENabcdef123456", dirty$convention, fixed = TRUE))
  expect_false(grepl("TOKENabcdef123456", dirty$column, fixed = TRUE))
})

test_that("`columns` is an ARRAY at every width, so the wire shape is stable", {
  # 🔴 MEASURED LIVE: a one-column descriptor published `"columns": "step"` (a JSON
  # string) while a four-column one published an array, because the writer
  # auto-unboxes a length-1 vector. An agent then handles two shapes for one field.
  reg <- function(cols) {
    r <- new.env(parent = emptyenv())
    r[["sc-markers-run_markers"]] <- list(
      counter = NULL, ready = function() TRUE, long = FALSE, timeout_s = 600,
      state = function() list(module = "sc_markers",
                             descriptor = list(kind = "table", n_rows = 3L,
                                               columns = cols)))
    ts_drive_module_states(list(drive_registry = r))$sc_markers$descriptor
  }
  one <- reg("step")
  four <- reg(c("gene", "cluster", "logFC", "padj"))
  # Both are LISTS of names, so both serialise as arrays.
  expect_true(is.list(one$columns))
  expect_true(is.list(four$columns))
  expect_identical(one$columns[[1]], "step")
  expect_length(four$columns, 4L)
  # And they agree in type, which is the property that was missing.
  expect_identical(typeof(one$columns), typeof(four$columns))
})

test_that("a real SC PROBE builds the descriptor, not just the view it calls", {
  # The view test above proves the four builders ACCEPT a descriptor. This proves a
  # probe actually PRODUCES one, because a probe that never passes it would leave
  # the view's new parameters dead code and the whole slice inert on a live session.
  # The probe is imported by AST and CALLED with stubbed collaborators, so the code
  # under test is the real one.
  e <- new.env(parent = globalenv())
  pull <- function(file, names) {
    for (nm in names) assign(nm, tryCatch(ts_ast_assignment(file, nm, eval_env = e),
                                           error = function(err) NULL), envir = e)
  }
  pull("modules/sc/mod_sc.R", c(".sc_markers_drive_state", ".sc_markers_drive_view",
                                ".SC_MARKERS_DRIVE_MODULE", ".SC_AP_DRIVE_STEP_STATES",
                                ".SC_MARKERS_DRIVE_CONVENTION"))
  e$ts_drive_job_state  <- function() NULL
  e$ts_drive_job_pending <- function() NULL
  e$.sc_markers_drive_ready <- function(global_data) "ready"

  markers <- data.frame(gene = c("Cdkn1a", "Mki67", "Top2a"),
                        cluster = c("c0", "c1", "c1"),
                        logFC = c(1.2, -0.4, 2.9),
                        padj = c(0.001, 0.4, 1e-9),
                        stringsAsFactors = FALSE)
  shared_rv <- new.env(parent = emptyenv())
  assign("markers_data", markers, envir = shared_rv)
  gd <- new.env(parent = emptyenv())
  assign("sc_obj", NULL, envir = gd)

  v <- e$.sc_markers_drive_state(gd, shared_rv, "done", last_step = "markers")
  # The probe produced a descriptor, and it describes the table it counted.
  expect_false(is.null(v$descriptor))
  expect_equal(v$descriptor$n_rows, 3)
  expect_true("gene" %in% v$descriptor$columns)
  expect_identical(v$convention, e$.SC_MARKERS_DRIVE_CONVENTION)
  # 🔴 AND NO CELL. A descriptor that leaked a value would ship patient-adjacent
  # biology on every snapshot, so this is asserted on the content, not the keys.
  blob <- paste(unlist(v$descriptor), collapse = "|")
  for (gene in markers$gene) expect_false(grepl(gene, blob, fixed = TRUE))

  # And with NO table, the probe omits the descriptor rather than describing an
  # empty one - "never ran" must not read as "ran and found nothing".
  shared_rv2 <- new.env(parent = emptyenv())
  assign("markers_data", NULL, envir = shared_rv2)
  v2 <- e$.sc_markers_drive_state(gd, shared_rv2, "not_ready", last_step = NULL)
  expect_null(v2$descriptor)
})

test_that("each of the THREE OWNED SC view builders carries a descriptor and a convention", {
  # 🔴 WHY THREE, NOT FOUR. `sc_pathways` is the fourth probe and it is DELIBERATELY
  # not extended here: its module file `modules/sc/mod_sc_pathways.R` carries a
  # concurrent +253-line block inserted by another author, and the drive view and
  # state probe sit after it. Adding a descriptor there means the commit would carry
  # 301 lines of someone else's work, which is the one thing this repository's
  # packaging discipline has refused all along. The mechanism is identical and is
  # exercised on the other three; the pathways probe needs its file's owner to add
  # the same three lines (a constant, two `descriptor`/`convention` arguments, and a
  # `ts_drive_table_descriptor()` call). The gap is left OPEN and recorded, not
  # quietly narrowed: `sc_pathways` still publishes a bare `n_results`.
  #
  # The view builders are plain file-level functions, so they are imported by AST
  # and CALLED - not source-locked. A source lock would pass while the builder
  # silently dropped the field, which is the failure a projection exists to catch.
  #
  # A view references file-level CONSTANTS (`.SC_AP_DRIVE_STEP_STATES`, the module
  # names, and for the auto-pipeline two step helpers). `ts_ast_assignment()` with
  # `eval_env = globalenv()` does not bring those along, and the first version
  # errored on `.SC_AP_DRIVE_STEP_STATES` not found. They are pulled in the SAME
  # way, from the SAME file, so nothing is re-typed.
  e <- new.env(parent = globalenv())
  # `eval_env = e` for EVERY pulled name, not just the constants: a pulled FUNCTION
  # closes over the env it was evaluated in, so pulling `.sc_ap_steps_as_list` with
  # `globalenv()` left it unable to see `.SC_AP_DRIVE_STEPS` and the auto-pipeline
  # view died with "object not found". One env for the whole graph.
  pull <- function(file, names) {
    for (nm in names) {
      assign(nm, tryCatch(ts_ast_assignment(file, nm, eval_env = e),
                         error = function(err) NULL), envir = e)
    }
  }
  pull("modules/sc/mod_sc.R", c(
    ".SC_AP_DRIVE_STEP_STATES", ".SC_PATHWAYS_DRIVE_STEP_STATES",
    ".SC_AP_DRIVE_MODULE", ".SC_ANNOT_DRIVE_MODULE", ".SC_MARKERS_DRIVE_MODULE",
    ".SC_AP_DRIVE_STEPS", ".SC_AP_DRIVE_STEP_ORDER",
    ".sc_ap_steps_as_list", ".sc_ap_steps_idle"))
  pull("modules/sc/mod_sc_pathways.R", c(".SC_PATHWAYS_DRIVE_MODULE"))

  for (spec in list(
    list(f = "modules/sc/mod_sc.R", fn = ".sc_markers_drive_view", mod = "sc_markers"),
    list(f = "modules/sc/mod_sc.R", fn = ".sc_annot_drive_view",   mod = "sc_annotation"),
    list(f = "modules/sc/mod_sc.R", fn = ".sc_ap_drive_view",      mod = "sc_pipeline")
  )) {
    view <- ts_ast_assignment(spec$f, spec$fn, eval_env = e)
    expect_true(is.function(view))
    v <- view(status = "done", n_results = 3L, has_data = TRUE, ready = TRUE,
              descriptor = list(n_rows = 3L, n_cols = 5L, columns = c("gene")),
              convention = "rows of the table")
    expect_identical(v$module, spec$mod)
    expect_false(is.null(v$descriptor))
    expect_equal(v$descriptor$n_rows, 3)
    # The convention is what makes the count interpretable and comparable.
    expect_identical(v$convention, "rows of the table")
    # Absent descriptor -> the field is simply absent, never a fabricated empty one.
    v2 <- view(status = "not_ready", n_results = 0L, has_data = FALSE, ready = FALSE)
    expect_null(v2$descriptor)
  }
})

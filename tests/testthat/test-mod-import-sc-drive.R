# =============================================================================
# S3 - the 10x-v3 SC directory import, driven.
#
# Contract (frozen by decision, not discovered here):
#   - a 10x-v3 TRIPLET directory only: matrix.mtx[.gz] + features.tsv[.gz] +
#     barcodes.tsv[.gz];
#   - `sample_name` is EXPLICIT and REQUIRED. The Spatial importer falls back to
#     basename(dir_path()) (mod_import_spatial.R:439) and the SC importer inherits
#     the same affordance for a human (mod_import_sc.R:417). The drive must NOT:
#     a basename is not a sample name, and inferring one is how two datasets end
#     up sharing an `orig.ident`.
#   - the human loader, its validations and the shared object builder are REUSED.
#     There is no second SC path.
#
# These tests were written BEFORE the implementation and measured RED (rule 5 +
# the repo's "write the test that fails" rule). Every block below is aimed at the
# seam the POLLER actually calls - a first draft called
# `ts_drive_validate_import()` for shapes and then tested the sample-name rules
# against the module, which is the "the probe lies" shape this repo keeps hitting.
# =============================================================================

# --- helpers ----------------------------------------------------------------

# Sourced EXPLICITLY, for the reason given at test-mod-import-spatial-drive.R:40-45:
# `source_project_file()` writes into `globalenv()`, so a test that leans on
# whichever sibling testthat happened to load first passes for the wrong reason
# when run alone. The module itself is NOT sourced - every assertion here runs in
# pure R against the allowlist, and the module is read as TEXT for the wiring
# locks, so this file needs neither Seurat nor a session.
source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")

.sc_src <- function() {
  readLines(file.path(ts_project_root(), "modules", "import", "mod_import_sc.R"),
            warn = FALSE)
}

# The module's CODE, with whole-line comments removed.
# All three wiring locks below count CALL SITES, and a first draft counted raw
# text: it found 2 `Read10X(` where there is 1, and 3 `sc_sample_object(` where
# there are 2 — every extra hit was a sentence in a comment. A count that includes
# prose measures the comment, not the wiring, and it is wrong in the direction
# that makes a duplicate invisible.
#
# Scope, stated rather than hidden: whole-line comments only. A trailing comment
# or a `#` inside a string is NOT stripped, so these counts would over-count on a
# line like `x <- "a # b"`. That is the safe direction for a "there must be
# exactly one" assertion - it errs towards reporting a duplicate - and the
# patterns used here match no such line.
.sc_code <- function() grep("^[[:space:]]*#", .sc_src(), invert = TRUE, value = TRUE)

# The TEXT of one definition or call, located by brace counting.
# `ts_ast_assignment()` evaluates the right-hand side, which is the wrong tool
# here: these closures close over `prepare_seurat_object`, `global_data` and the
# i18n shim, so evaluating one outside the server would fail on its own
# dependencies and the assertion would be about the harness, not the wiring.
.sc_body <- function(name) {
  lines <- .sc_src()
  pat <- paste0(name, " <- function(")
  i <- which(startsWith(trimws(lines), pat))[1]
  if (is.na(i)) return(NULL)
  depth <- 0L
  started <- FALSE
  txt <- character(0)
  for (k in i:length(lines)) {
    for (ch in strsplit(lines[k], "")[[1]]) {
      if (ch == "{") { depth <- depth + 1L; started <- TRUE }
      if (ch == "}") depth <- depth - 1L
    }
    txt <- c(txt, lines[k])
    if (started && depth <= 0L) return(paste(txt, collapse = "\n"))
  }
  paste(txt, collapse = "\n")
}

# A 10x-v3 triplet built from EMPTY files: the validator must decide on the
# NAMES, so a fixture that carries no matrix is both cheap and unambiguous. A
# validator that only counted files would pass here, which is the point.
.sc_triplet <- function(dir, files = c("matrix.mtx.gz", "features.tsv.gz", "barcodes.tsv.gz")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (f in files) file.create(file.path(dir, f))
  dir
}

.sc_root <- function(name = "ts-sc-drive") {
  r <- file.path(tempdir(), name)
  unlink(r, recursive = TRUE)
  dir.create(r, recursive = TRUE, showWarnings = FALSE)
  r
}

.sc_validate <- function(req, roots) {
  ts_drive_validate_import(req, roots = roots, module = "import_sc")
}

# Fail-then-return. A block that dereferences a not-yet-existing entry CRASHES,
# and a crashing red says "the test is broken" rather than "the feature is
# missing" - which is the distinction the repo insists on keeping. So every
# block asserts the precondition first and leaves if it does not hold.
.sc_need <- function(cond, what) {
  ok <- isTRUE(cond)
  testthat::expect_true(ok, info = what)
  # An explicit logical: `expect_true()` returns the value it was given, so
  # returning it directly made `if (!.sc_need(...))` a negation of NULL - and a
  # type error rather than a clean skip, which is the very thing this helper
  # exists to prevent.
  ok
}

.sc_entry <- function() {
  e <- TS_DRIVE_IMPORT_SCHEMA[["import_sc"]]
  if (!.sc_need(!is.null(e), "TS_DRIVE_IMPORT_SCHEMA must declare an `import_sc` entry")) {
    return(invisible(NULL))
  }
  e
}

# Resolver that also absorbs the zero-argument case: a resolver and a target look
# identical from here, and passing the resolver by mistake produced "unused
# arguments" three times in the sibling S2 file before it was made structural.
.exp_need <- function(fn, what) {
  if (is.function(fn) && length(formals(fn)) == 0L) {
    r <- tryCatch(fn(), error = function(e) NULL)
    if (is.function(r)) fn <- r
  }
  ok <- is.function(fn)
  testthat::expect_true(ok, info = paste(what, "must exist as a callable"))
  if (!ok) return(invisible(NULL))
  fn
}

.exp_validate_scenario <- function() get0("ts_drive_validate_scenario", envir = globalenv())

# =============================================================================
test_that("the schema declares import_sc with BOTH keys required and a check", {
  e <- .sc_entry()
  if (is.null(e)) return(invisible(NULL))
  # `required` had to become a VECTOR: it was a scalar, read as `block[[required]]`
  # in two places, so a second mandatory key was inexpressible and the alternative
  # was a `sample_name` declared optional and then enforced by hand elsewhere -
  # a schema that lies about its own contract.
  expect_setequal(e$required, c("dir_path", "sample_name"))
  expect_length(e$optional, 0L)
  # And the module-specific rules are DATA (a function name), not a third
  # `identical(module, ...)` boolean in the shared validator - the anti-pattern
  # the allowlist file names in its own comments.
  if (!.sc_need(is.character(e$check) && length(e$check) == 1L,
                "`import_sc` must declare a single `check` function name")) {
    return(invisible(NULL))
  }
  expect_true(is.function(get0(e$check, envir = globalenv())))
})

test_that("required becoming a vector did not change the two existing importers", {
  sch <- TS_DRIVE_IMPORT_SCHEMA
  expect_setequal(sch$import_bulk$required, "counts_path")
  expect_setequal(sch$import_spatial$required, "dir_path")
  r <- file.path(tempdir(), "ts-sc-reg")
  dir.create(r, recursive = TRUE, showWarnings = FALSE)

  b <- ts_drive_validate_import(list(), roots = r, module = "import_bulk")
  expect_false(b$ok)
  expect_true(any(grepl("counts_path", b$errors, fixed = TRUE)))

  s <- ts_drive_validate_import(list(), roots = r, module = "import_spatial")
  expect_false(s$ok)
  expect_true(any(grepl("dir_path", s$errors, fixed = TRUE)))

  # The refusal must NAME every missing key, not just the first: an agent that
  # sent `dir_path` alone needs to be told `sample_name` is the other half.
  sc <- .sc_validate(list(), r)
  expect_false(sc$ok)
  expect_true(any(grepl("dir_path", sc$errors, fixed = TRUE)))
  expect_true(any(grepl("sample_name", sc$errors, fixed = TRUE)))
})

test_that("a complete 10x-v3 triplet is accepted, and the names are resolved", {
  root <- .sc_root()
  d <- .sc_triplet(file.path(root, "pbmc"))
  r <- .sc_validate(list(dir_path = d, sample_name = "  Patient1  "), root)
  if (!.sc_need(r$ok, "a complete v3 triplet must be accepted")) return(invisible(NULL))
  expect_length(r$errors, 0L)
  # The sample name is TRIMMED, so the name that reaches `orig.ident` is the one
  # the agent typed rather than a whitespace-padded variant of it.
  expect_identical(r$import$sample_name, "Patient1")
  expect_true(dir.exists(r$import$dir_path))
})

test_that("the UNCOMPRESSED v3 triplet is equally valid (Read10X takes both)", {
  root <- .sc_root()
  d <- .sc_triplet(file.path(root, "plain"),
                   c("matrix.mtx", "features.tsv", "barcodes.tsv"))
  r <- .sc_validate(list(dir_path = d, sample_name = "P1"), root)
  expect_true(r$ok)
})

test_that("a CellRanger v2 directory is refused and NOT silently upgraded", {
  # v2 ships `genes.tsv` instead of `features.tsv`. The human loader copes by
  # calling `.ensure_10x_features()`, which WRITES a `features.tsv.gz` into the
  # user's directory. For an agent that is a side effect nobody asked for and
  # cannot undo, so the drive refuses the shape outright. The test also pins that
  # nothing was created: a refusal that mutated the corpus would be worse than
  # the v2 support it replaced.
  root <- .sc_root()
  d <- .sc_triplet(file.path(root, "v2"),
                   c("matrix.mtx.gz", "genes.tsv.gz", "barcodes.tsv.gz"))
  r <- .sc_validate(list(dir_path = d, sample_name = "P1"), root)
  expect_false(r$ok)
  expect_true(any(grepl("features", r$errors, ignore.case = TRUE)))
  expect_false(file.exists(file.path(d, "features.tsv.gz")))
  expect_equal(sort(list.files(d)), sort(c("barcodes.tsv.gz", "genes.tsv.gz", "matrix.mtx.gz")))
})

test_that("each missing member of the triplet is named in the refusal", {
  root <- .sc_root()
  for (miss in c("matrix.mtx.gz", "features.tsv.gz", "barcodes.tsv.gz")) {
    keep <- setdiff(c("matrix.mtx.gz", "features.tsv.gz", "barcodes.tsv.gz"), miss)
    d <- .sc_triplet(file.path(root, paste0("no_", sub("\\..*$", "", miss))), keep)
    r <- .sc_validate(list(dir_path = d, sample_name = "P1"), root)
    expect_false(r$ok)
    expect_true(any(grepl(sub("\\..*$", "", miss), r$errors, ignore.case = TRUE)))
  }
})

test_that("sample_name is required, and an empty or blank one is refused", {
  root <- .sc_root()
  d <- .sc_triplet(file.path(root, "s1"))
  expect_false(.sc_validate(list(dir_path = d), root)$ok)
  for (bad in list("", "   ", "\t\n")) {
    r <- .sc_validate(list(dir_path = d, sample_name = bad), root)
    expect_false(r$ok)
    expect_true(any(grepl("sample_name", r$errors, fixed = TRUE)))
  }
  # A non-string is refused too: `sample_name` becomes `orig.ident` and a list
  # name, and `as.character(42)` would quietly accept a number.
  r <- .sc_validate(list(dir_path = d, sample_name = 42), root)
  expect_false(r$ok)
  expect_true(.sc_validate(list(dir_path = d, sample_name = "P1"), root)$ok)
})

test_that("sample_name must not carry a path separator or a traversal", {
  # It becomes `orig.ident` AND a name in the module's sample list. A value with
  # a separator is either a display bug or an attempt to make one sample look
  # like two; either way the drive has no use for it.
  root <- .sc_root()
  d <- .sc_triplet(file.path(root, "s2"))
  for (bad in c("a/b", "a\\b", "..", "../x", "C:/x", "/abs")) {
    r <- .sc_validate(list(dir_path = d, sample_name = bad), root)
    expect_false(r$ok)
    expect_true(any(grepl("sample_name", r$errors, fixed = TRUE)))
  }
})

test_that("dir_path keeps the shared confinement rules (no `..`, allowlisted roots)", {
  root <- .sc_root()
  d <- .sc_triplet(file.path(root, "s3"))
  # `..` refused on the RAW string, before normalisation - the rule the shared
  # path resolver owns, reused rather than re-implemented for folders.
  r <- .sc_validate(list(dir_path = paste0(d, "/../s3"), sample_name = "P1"), root)
  expect_false(r$ok)
  expect_true(any(grepl("\\.\\.", r$errors)))
  # A real triplet OUTSIDE the roots is refused on the roots, not on the names:
  # proving the confinement runs first is what stops "add the dir" being read as
  # the whole security story.
  outside <- .sc_triplet(file.path(tempdir(), "ts-sc-outside", "x"))
  r2 <- .sc_validate(list(dir_path = outside, sample_name = "P1"), file.path(root, "nowhere"))
  expect_false(r2$ok)
  expect_true(any(grepl("allowlisted root", r2$errors)))
})

test_that("a directory that is not a 10x triplet at all is refused by name", {
  root <- .sc_root()
  empty <- file.path(root, "empty")
  dir.create(empty, recursive = TRUE, showWarnings = FALSE)
  r <- .sc_validate(list(dir_path = empty, sample_name = "P1"), root)
  expect_false(r$ok)
  expect_true(any(grepl("matrix", r$errors, ignore.case = TRUE)))
  # An .h5 or a .rds is a DIFFERENT entry point (the loader's Option B/C). Naming
  # it as out of scope stops an agent from concluding the directory is broken.
  expect_true(any(grepl("h5|rds|10x|triplet", r$errors, ignore.case = TRUE)))
})

test_that("a key belonging to another importer is refused, and says who owns it", {
  root <- .sc_root()
  d <- .sc_triplet(file.path(root, "s4"))
  r <- .sc_validate(list(dir_path = d, sample_name = "P1", technology = "visium"), root)
  expect_false(r$ok)
  expect_true(any(grepl("technology", r$errors, fixed = TRUE)))
  r2 <- .sc_validate(list(dir_path = d, sample_name = "P1", counts_path = "x.csv"), root)
  expect_false(r2$ok)
  expect_true(any(grepl("counts_path", r2$errors, fixed = TRUE)))
})

test_that("the POLLER requires BOTH keys, not just the schema", {
  # `ts_drive_apply()` re-checked the required key itself (drive_watcher.R:1741)
  # with `req[[need]]`, which silently only ever looked at the FIRST key once
  # `required` became a vector. The schema is not the gate the poller uses.
  root <- .sc_root()
  d <- .sc_triplet(file.path(root, "s5"))
  tok <- "tokpin01"
  sc <- function(req) list(protocol = "ts-drive/1", seq = 5, module = "import_sc",
                           action = "import_file", session_token = tok, import = req)
  for (bad in list(list(dir_path = d), list(sample_name = "P1"), list())) {
    r <- ts_drive_validate_scenario(sc(bad), tok, 0)
    expect_false(r$ok)
  }
  good <- ts_drive_validate_scenario(
    sc(list(dir_path = d, sample_name = "P1")), tok, 0)
  if (!.sc_need(good$ok, "a well-formed import_sc scenario must validate")) {
    return(invisible(NULL))
  }
  expect_setequal(names(good$scenario$import), c("dir_path", "sample_name"))
})

test_that("the POLLER's OWN gate stops a half-populated import, seam untouched", {
  # ⚠️ This block exists because the one before it was measuring the wrong thing.
  # It drove `ts_drive_validate_scenario()` — the NORMALIZER — and called that
  # proof that "the poller requires both keys". The poller re-checks the required
  # keys itself, in `ts_drive_apply()` (drive_watcher.R:1741), and that second gate
  # had no test at all: falsifying it to the scalar subscript
  # `is.null(req[[need]])` left the whole file GREEN, because
  # `req[[c("dir_path","sample_name")]]` is an integer subscript that returns a
  # 2-element list, `is.null()` is FALSE, and the gate waves an EMPTY payload
  # through. The same probe-vs-product mistake as S2, in a third place.
  #
  # So this drives the injector, and asserts the strongest available property: the
  # importer seam is NEVER reached.
  root <- .sc_root()
  d <- .sc_triplet(file.path(root, "poller"))
  called <- FALSE
  effects <- function(id, mode = NULL, module = NULL, request = NULL) {
    called <<- TRUE
    list(ok = TRUE, status = "applied")
  }
  for (bad in list(list(), list(dir_path = d), list(sample_name = "P1"))) {
    called <- FALSE
    out <- ts_drive_apply(NULL, NULL,
      list(action = "import_file", module = "import_sc", import = bad),
      effects = effects)
    expect_identical(out$status, "invalid")
    expect_false(called)
  }
  # A complete payload DOES reach the seam — otherwise "the seam is never called"
  # would be satisfied by an importer that is simply never wired.
  called <- FALSE
  ok <- ts_drive_apply(NULL, NULL,
    list(action = "import_file", module = "import_sc",
         import = list(dir_path = d, sample_name = "P1")),
    effects = effects)
  expect_true(called)
  expect_true(ok$status %in% c("applied", "done", "running"))
  # And the scalar form is refused for a module that declares exactly one key, so
  # the loop cannot have quietly broken the two existing importers.
  called <- FALSE
  b <- ts_drive_apply(NULL, NULL,
    list(action = "import_file", module = "import_bulk", import = list()),
    effects = effects)
  expect_identical(b$status, "invalid")
  expect_false(called)
})

test_that("an import_sc with NO session token is refused (it replaces sc_obj)", {
  # MEASURED on a live session, and the second half of the S2 finding rather than a
  # new idea. S2 established that `export_result` — the action that writes a file —
  # must carry the token, and scoped the rule to that action on purpose. The
  # `import_file` gate was left with the historical `nzchar()` guard, so an absent
  # token skipped the comparison entirely: a scenario addressed to no session
  # REPLACED the live `sc_obj` and came back `done`.
  #
  # That is the realistic failure, not a hostile one: a scenario written for a
  # previous session, replayed after the user has loaded something else, silently
  # overwrites the object they are looking at. The blast radius is the whole
  # single-cell object, which is why the same rule applies.
  #
  # SCOPED to the two actions that mutate the session (`import_file`,
  # `export_result`). The other actions keep the token-optional affordance, because
  # a one-shot scenario an operator drops in by hand still has to work — widening
  # it to every action is the change that would break them.
  vs <- .exp_need(.exp_validate_scenario, "ts_drive_validate_scenario()")
  if (is.null(vs)) return(invisible(NULL))
  root <- .sc_root()
  d <- .sc_triplet(file.path(root, "tok"))
  tok <- "tokpin01"
  imp <- list(dir_path = d, sample_name = "P1")
  sc <- function(action, token) {
    e <- list(protocol = "ts-drive/1", seq = 5, module = "import_sc",
              action = action, import = imp)
    if (!is.null(token)) e$session_token <- token
    e
  }
  for (ac in c("import_file", "export_result")) {
    # The right request block per action: `export_result` takes NO field, so
    # pairing it with the SC payload is itself a refusal — a first draft asserted
    # "accepted with the right token" while handing it two fields it must refuse.
    blk <- if (ac == "import_file") imp else list()
    sc <- function(action, token) {
      e <- list(protocol = "ts-drive/1", seq = 5, module = "import_sc",
                action = action, import = blk)
      if (!is.null(token)) e$session_token <- token
      e
    }
    r <- vs(sc(ac, NULL), tok, 0)
    expect_false(r$ok)
    expect_true(any(grepl("session_token", r$errors)))
    expect_false(vs(sc(ac, ""), tok, 0)$ok)
    expect_true(vs(sc(ac, tok), tok, 0)$ok)
  }
  # The pinned set is DATA, and both actions in it are exactly the two that
  # mutate the session. A third pinned action would be a visible edit here.
  expect_setequal(TS_DRIVE_TOKEN_PINNED_ACTIONS, c("import_file", "export_result"))
  expect_true(all(TS_DRIVE_TOKEN_PINNED_ACTIONS %in% TS_DRIVE_ACTIONS))
  # And an action that mutates nothing is still addressable without a token.
  expect_true(vs(list(protocol = "ts-drive/1", seq = 5, module = "import_sc",
                      action = "snapshot"), tok, 0)$ok)
  # The token is a credential: a mismatch must not echo it.
  r <- vs(c(sc("import_file", "deadbeef")), tok, 0)
  expect_false(any(grepl("deadbeef", r$errors, fixed = TRUE)))
})

test_that("the triplet refusal names the FILENAMES, not an R list literal", {
  # MEASURED live: the message rendered the accepted names as
  # `c("matrix.mtx.gz", "matrix.mtx")`, because `sprintf()` vectorised over a LIST
  # and printed its deparse. An agent reading that has to guess which two strings
  # are filenames, and the second sentence then contradicted the first.
  root <- .sc_root()
  empty <- file.path(root, "msg")
  dir.create(empty, recursive = TRUE, showWarnings = FALSE)
  r <- .sc_validate(list(dir_path = empty, sample_name = "P1"), root)
  expect_false(r$ok)
  msg <- paste(r$errors, collapse = " ")
  expect_false(grepl("c(\"", msg, fixed = TRUE))
  # Every accepted filename appears literally, so the agent can create the file.
  for (f in unlist(TS_DRIVE_SC_10X_V3_FILES)) {
    expect_true(grepl(f, msg, fixed = TRUE))
  }
  # And the out-of-scope hint only appears when there is no matrix at all: a
  # directory that HAS a matrix but no features is a broken v3 tree, and telling
  # the agent to go use Option B would send it down the wrong road.
  expect_true(grepl("Option B or C", msg, fixed = TRUE))
  d <- .sc_triplet(file.path(root, "nofeat"), c("matrix.mtx.gz", "barcodes.tsv.gz"))
  r2 <- .sc_validate(list(dir_path = d, sample_name = "P1"), root)
  expect_false(r2$ok)
  expect_false(grepl("Option B or C", paste(r2$errors, collapse = " "), fixed = TRUE))
})

test_that("the triplet refusal labels EACH part with ITS OWN filenames", {
  # The assertion above ("every accepted filename appears somewhere") was NOT
  # enough, and the live run proved it: the second sentence was built with
  # `TS_DRIVE_SC_10X_V3_FILES[[1L]]` hard-coded, so it announced
  #   features (`matrix.mtx.gz` or `matrix.mtx`)
  # i.e. it told the agent that a barcodes file is an acceptable FEATURES file.
  # Every name still appeared literally — in the FIRST sentence — so the earlier
  # test stayed green while the message was actively wrong. A "does the string
  # contain the word" assertion cannot see a name attached to the wrong noun.
  #
  # So this asserts the PAIRING: each part name must be followed by its own two
  # accepted filenames, in both sentences.
  root <- .sc_root()
  empty <- file.path(root, "pairing")
  dir.create(empty, recursive = TRUE, showWarnings = FALSE)
  msg <- paste(.sc_validate(list(dir_path = empty, sample_name = "P1"), root)$errors,
               collapse = " ")
  for (part in names(TS_DRIVE_SC_10X_V3_FILES)) {
    files <- TS_DRIVE_SC_10X_V3_FILES[[part]]
    # The "or" form, which is the sentence that was wrong.
    expect_true(grepl(sprintf("%s (`%s` or `%s`)", part, files[1], files[2]), msg,
                      fixed = TRUE))
    # And no OTHER part's filenames may be attributed to this one.
    for (other in setdiff(names(TS_DRIVE_SC_10X_V3_FILES), part)) {
      of <- TS_DRIVE_SC_10X_V3_FILES[[other]]
      if (!any(of %in% files)) {
        expect_false(grepl(sprintf("%s (`%s` or `%s`)", part, of[1], of[2]), msg,
                           fixed = TRUE))
      }
    }
  }
})

test_that("import_sc is reachable as an IMPORTER and binds no button, on purpose", {
  # The MCP self-check requires every drivable module to be reachable, by run
  # button OR as a declared importer. `import_sc` takes the second road, so this
  # pins BOTH halves: that it really is an importer (the exemption is earned), and
  # that no button was bound to it (the exemption was not achieved by quietly
  # inventing a click target).
  expect_true(TS_DRIVE_SC_IMPORT_MODULE %in% TS_DRIVE_IMPORT_MODULES)
  expect_true(TS_DRIVE_SC_IMPORT_MODULE %in% TS_DRIVE_MODULES)
  # No bound button: every id in the table that belongs to this module would be one.
  owned <- names(TS_DRIVE_BUTTONS)[vapply(TS_DRIVE_BUTTONS, function(b) {
    identical(ts_drive_button_module(b), TS_DRIVE_SC_IMPORT_MODULE)
  }, logical(1))]
  expect_length(owned, 0L)
  # And the two existing importers DO keep real buttons, so the exemption did not
  # become a blanket one.
  expect_true("import_bulk-btn_load" %in% TS_DRIVE_BUTTONS)
  expect_true("import_spatial-btn_import" %in% TS_DRIVE_BUTTONS)
})

test_that("the drive REUSES the human loader: one Read10X, one sample builder", {
  # The S3 decision forbids a parallel SC path. The cheapest honest proof that
  # there is none is that the file still contains exactly ONE `Read10X(` call
  # site and ONE `prepare_seurat_object` definition, and that BOTH entry points
  # go through one shared sample builder rather than each assembling an object.
  code <- .sc_code()
  n_read10x <- sum(grepl("(?<![_[:alnum:]])Read10X\\(", code, perl = TRUE))
  expect_equal(n_read10x, 1L)
  expect_equal(sum(grepl("prepare_seurat_object <- function", code, fixed = TRUE)), 1L)
  expect_equal(sum(grepl("sc_sample_object <- function", code, fixed = TRUE)), 1L)
  expect_true(any(grepl("ts_drive_publish_importer", code, fixed = TRUE)))
  # The shared builder is reached from BOTH sides, twice over: the human loop
  # inside `btn_load_dir` and the published drive importer. A builder that only
  # the drive called would be the parallel path wearing a shared name.
  expect_equal(sum(grepl("sc_sample_object(", code, fixed = TRUE)), 2L)
})

test_that("the sample identity is the EXPLICIT name, never a basename", {
  # `orig.ident` is what every downstream SC reader groups by, so this is where a
  # guessed name would do its damage: two directories called `sample` would
  # silently merge into one identity. The name is set in the SHARED builder, from
  # the argument it is given - and both callers pass the name they were handed.
  builder <- .sc_body("sc_sample_object")
  if (is.null(builder)) return(invisible(NULL))
  expect_true(grepl("orig.ident", builder, fixed = TRUE))
  expect_true(grepl("sample_name", builder, fixed = TRUE))
  expect_false(grepl("basename(", builder, fixed = TRUE))

  # And the published importer passes the request's name straight through, with
  # no derivation of its own.
  pub <- .sc_body("ts_drive_publish_importer")
  if (is.null(pub)) return(invisible(NULL))
  expect_false(grepl("basename(", pub, fixed = TRUE))
  expect_true(grepl("sc_sample_object(", pub, fixed = TRUE))
  expect_true(grepl("sample_name", pub, fixed = TRUE))
})

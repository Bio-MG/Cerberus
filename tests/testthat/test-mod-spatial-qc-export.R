# =============================================================================
# test-mod-spatial-qc-export.R ÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬Ãƒâ€¦Ã‚Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â the ONE allowlisted export route (S2).
# =============================================================================
# WHY THIS FILE EXISTS AT ALL. An agent could run `spatial-qc-btn_hotspots` and
# read `n_results`, but the protocol has no way to obtain the TABLE: the module
# state probes are closed scalar contracts, so `n_results` is a COUNT of
# significant spots, not the 2702-row table behind it. MEASURED on a live session
# before this slice: the wire carried 1200, the panel said 585/615/2702, and the
# file was 204 485 bytes ÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬Ãƒâ€¦Ã‚Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â none of which an agent could reach. 0 of 126
# `downloadHandler` sites were drivable.
#
# So this is ONE route, for ONE artefact, and the design constraint is that the
# client chooses NOTHING. Not the handler, not the outputId, not the destination,
# not the filename, not the format. The route is bound to the live session and to
# the result that session already produced, it writes only into an
# application-controlled bounded temporary directory, and it returns a REDACTED
# descriptor.
#
# The six negative tests below are the point of the slice, not decoration: a
# download verb that cannot be pointed somewhere it should not be, or that
# survives its session, is worse than no verb at all.
# =============================================================================

source_project_file("config/defaults.R")
source_project_file("R/core/io_helpers.R")            # %||%
source_project_file("R/core/error_state.R")
source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")
source_project_file("R/spatial/spatial_stats.R")
source_project_file("modules/spatial/mod_spatial_qc.R")

if (!exists(".tr", envir = globalenv()))
  assign(".tr", function(key) key, envir = globalenv())
if (!exists(".t_fmt", envir = globalenv()))
  assign(".t_fmt", function(template, ...) template, envir = globalenv())
if (!exists(".tr_plain", envir = globalenv()))
  assign(".tr_plain", function(key) key, envir = globalenv())

# --- Fixtures ----------------------------------------------------------------
# 40 spots, 6 significant, on a grid. Opaque ids: nothing biological is implied.
.exp_n <- 40L
.exp_sig <- 6L

.exp_coords <- function(n = .exp_n) {
  set.seed(4242)
  data.frame(id = sprintf("E%03d", seq_len(n)),
             x = as.numeric(seq_len(n) %% 8L),
             y = as.numeric(seq_len(n) %/% 8L),
             stringsAsFactors = FALSE)
}

.exp_record <- function(n = .exp_n, n_sig = .exp_sig) {
  set.seed(4242)
  lab <- rep("NS", n)
  lab[seq_len(n_sig)] <- rep(c("Hotspot (chaud)", "Coldspot (froid)"), length.out = n_sig)
  data.frame(id = sprintf("E%03d", seq_len(n)),
             value = stats::runif(n, 1, 10),
             gi_star = stats::rnorm(n, 0, 1),
             p_value = stats::runif(n, 0, 0.2),
             hotspot = lab,
             stringsAsFactors = FALSE)
}

# The state a live `spatial_qc` session holds after a hotspots run: the object,
# the QC metrics the drive readiness requires, and the stored table.
.exp_state <- function(with_result = TRUE) {
  gd <- shiny::reactiveValues()
  gd$spatial_obj <- list(coords = .exp_coords())
  gd$language <- "fr"
  # A sample name, because the HUMAN download filename embeds one. The drive
  # descriptor must not leak it; that is asserted, not assumed.
  gd$active_spatial_dataset <- "fixture_sample_name"
  rv <- shiny::reactiveValues()
  rv$qc_metrics <- local({
    set.seed(4242); base <- stats::runif(.exp_n, 1, 10)
    data.frame(id = sprintf("E%03d", seq_len(.exp_n)), nCount = base * 100,
               nFeature = stats::runif(.exp_n, 500, 2000), pct_mt = stats::runif(.exp_n, 1, 8),
               pct_ribo = stats::runif(.exp_n, 10, 40), log_nCount = log1p(base * 100),
               row.names = sprintf("E%03d", seq_len(.exp_n)), stringsAsFactors = FALSE)
  })
  if (with_result) {
    rv$hotspot_result <- .exp_record()
    rv$hotspot_params <- list(source = "qc", metric = "nCount", k_neighbors = 30L)
  }
  list(gd = gd, rv = rv)
}

# The resolver, for the same reason as the S1.5 blocks: a bare reference to a
# function that does not exist yet raises "object not found", an ERROR that reads
# as a broken test rather than as the missing capability.
.exp_writer <- function() get0("spatial_hotspot_csv_write", envir = globalenv())
.exp_exporter <- function() get0("spatial_qc_export_hotspot_csv", envir = globalenv())
.exp_routes <- function() get0("TS_DRIVE_EXPORT_ROUTES", envir = globalenv())
.exp_validate <- function() get0("ts_drive_validate_export_request", envir = globalenv())
.exp_dir <- function() get0("ts_drive_export_dir", envir = globalenv())
.exp_descriptor <- function() get0("ts_drive_export_descriptor", envir = globalenv())
.exp_write_result <- function() get0("ts_drive_write_result", envir = globalenv())
.exp_validate_scenario <- function() get0("ts_drive_validate_scenario", envir = globalenv())

.exp_need <- function(fn, what) {
  # A RESOLVER and a TARGET look identical from here — both are functions — and
  # passing the resolver by mistake produced "unused arguments" three times in this
  # file before it was made structural rather than a matter of care. A zero-argument
  # function whose RESULT is a function is a resolver; no target here takes no
  # arguments, and `ts_drive_export_dir()` is zero-argument but returns a path, so
  # the `is.function()` guard on the result is what keeps the two apart.
  if (is.function(fn) && length(formals(fn)) == 0L) {
    r <- tryCatch(fn(), error = function(e) NULL)
    if (is.function(r)) fn <- r
  }
  if (!is.function(fn)) {
    testthat::expect_true(is.function(fn), info = paste(what, "must exist as a callable"))
    return(NULL)
  }
  fn
}

# =============================================================================
# 1. The route table is FROZEN DATA
# =============================================================================
test_that("the export route table declares exactly TEN frozen routes", {
  routes <- .exp_routes()
  if (!is.list(routes)) {
    testthat::expect_true(is.list(routes),
      info = "TS_DRIVE_EXPORT_ROUTES must exist and declare the routes as frozen data")
    return(invisible(NULL))
  }
  # TEN since Slice 4 (bulk_wgcna joins with the gene->module table its
  # product call granted), and still closed: the verb exists for NAMED
  # artefacts, one per route, and every artefact decision stays the app’s.
  expect_length(routes, 10L)
  expect_setequal(names(routes),
                  c("spatial_qc", "bulk_de", "bulk_pathways", "sc_markers",
                    "sc_pathways", "bulk_filter", "bulk_signatures",
                    "bulk_pattern", "bulk_network", "bulk_wgcna"))
  # The routes are REGISTRY KEYS, not handler names the caller supplies.
  expect_type(routes$spatial_qc, "character")
  expect_length(routes$spatial_qc, 1L)
  expect_type(routes$bulk_de, "character")
  expect_length(routes$bulk_de, 1L)
  expect_type(routes$bulk_pathways, "character")
  expect_length(routes$bulk_pathways, 1L)
  expect_type(routes$sc_markers, "character")
  expect_length(routes$sc_markers, 1L)
  expect_type(routes$sc_pathways, "character")
  expect_length(routes$sc_pathways, 1L)
  expect_type(routes$bulk_filter, "character")
  expect_length(routes$bulk_filter, 1L)
  expect_type(routes$bulk_signatures, "character")
  expect_length(routes$bulk_signatures, 1L)
  expect_type(routes$bulk_pattern, "character")
  expect_length(routes$bulk_pattern, 1L)
  expect_type(routes$bulk_network, "character")
  expect_length(routes$bulk_network, 1L)

  # And the modules are real drive modules, or the routes could never be
  # dispatched.
  expect_true("spatial_qc" %in% TS_DRIVE_MODULES)
  expect_true("bulk_de" %in% TS_DRIVE_MODULES)
  expect_true("bulk_pathways" %in% TS_DRIVE_MODULES)
  expect_true("sc_markers" %in% TS_DRIVE_MODULES)
  expect_true("sc_pathways" %in% TS_DRIVE_MODULES)
  expect_true("bulk_filter" %in% TS_DRIVE_MODULES)
  expect_true("bulk_signatures" %in% TS_DRIVE_MODULES)
  expect_true("bulk_pattern" %in% TS_DRIVE_MODULES)
  expect_true("bulk_network" %in% TS_DRIVE_MODULES)
  # The action exists in the frozen vocabulary.
  expect_true("export_result" %in% TS_DRIVE_ACTIONS)
})

test_that("the spatial descriptor columns equal the declared contract table", {
  # S2c follow-up (option 1): the app declares each route's column contract
  # (TS_DRIVE_EXPORT_COLUMNS) and the server renders it into the tool
  # description. This pin keeps the DECLARED list equal to what THIS route's
  # exporter actually produces — the fixed-shape promise, name for name.
  ct <- get0("TS_DRIVE_EXPORT_COLUMNS", envir = globalenv())
  if (is.null(ct)) {
    testthat::fail("TS_DRIVE_EXPORT_COLUMNS must exist - the per-route column contract is declared data")
    return(invisible(NULL))
  }
  x <- .exp_need(.exp_exporter(), "spatial_qc_export_hotspot_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- tempfile("ts-exp-ct-")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
  st <- .exp_state()
  r <- x(st$rv, st$gd, d)
  expect_true(isTRUE(r$ok))
  expect_identical(r$descriptor$columns, ct$spatial_qc$fixed)
})

test_that("the export request carries NO selectable field", {
  # The client picks nothing, so the accepted key set is EMPTY. Asserted as a
  # value rather than implied by the absence of code: a future field added here
  # would be a silent widening, and this is the line that says no.
  fields <- get0("TS_DRIVE_EXPORT_FIELDS", envir = globalenv())
  if (is.null(fields)) {
    testthat::expect_true(is.character(fields),
      info = "TS_DRIVE_EXPORT_FIELDS must be declared, even if empty")
    return(invisible(NULL))
  }
  expect_identical(fields, character(0))
})

# =============================================================================
# 2. NO CLIENT CHOICE ÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬Ãƒâ€¦Ã‚Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â the traversal / smuggling test
# =============================================================================
test_that("the request validator REFUSES every destination-shaped field", {
  v <- .exp_need(.exp_validate(), "ts_drive_validate_export_request()")
  if (is.null(v)) return(invisible(NULL))

  # Each of these is a way a caller could try to choose WHERE the bytes land, or
  # WHAT gets written. None may be honoured, and none may be silently ignored:
  # silently ignoring a field the caller believed was honoured is how a
  # "successful" export ends up somewhere unexpected.
  hostile <- list(
    path = "C:/Windows/System32/evil.csv",
    file = "evil.csv",
    filename = "evil.csv",
    dir = "C:/Windows/System32",
    directory = "../..",
    dest = "/tmp/evil.csv",
    where = "elsewhere",
    format = "rds",
    type = "rds",
    ext = ".rds",
    handler = "dl_bundle",
    output = "spatial-qc-dl_bundle",
    outputId = "spatial-qc-dl_bundle",
    output_id = "spatial-qc-dl_bundle",
    id = "spatial-qc-dl_hotspot_csv"
  )
  for (nm in names(hostile)) {
    # NAMED, and the distinction is the whole test: `list(nm, value)` has an
    # empty first name, so the validator would take its "every key must be named"
    # branch and refuse without ever naming the field. The refusal this asserts is
    # the one that says WHICH field was smuggled.
    r <- v(as.list(stats::setNames(list(hostile[[nm]]), nm)))
    expect_false(isTRUE(r$ok),
                 info = sprintf("a caller-supplied `%s` must be REFUSED, not ignored", nm))
    expect_match(paste(r$errors, collapse = " "), nm, fixed = TRUE)
  }

  # The classic traversal spellings, refused on the RAW string like the import
  # validator does (`..` before normalisation, so the attempt cannot be hidden
  # by where it happens to resolve).
  for (p in c("../../secret.csv", "..\\..\\secret.csv", "sub/../../secret.csv",
              "C:/x/../../y.csv")) {
    r <- v(list(path = p))
    expect_false(isTRUE(r$ok), info = sprintf("traversal %s must be refused", p))
  }

  # And the shapes that ARE accepted: nothing, or an absent/NULL block. Both
  # yield an EMPTY rebuilt request, never the caller's object handed on.
  expect_true(isTRUE(v(list())$ok))
  expect_identical(v(list())$request, list())
  expect_true(isTRUE(v(NULL)$ok))
  expect_identical(v(NULL)$request, list())
  # Provenance: the rebuilt request is a NEW, EMPTY list, not the caller's object.
  # A validator that returned its input would carry a smuggled field into the
  # writer. Asserted by identity against the payload, not against itself: an
  # earlier version compared the value with itself, which is vacuously FALSE for
  # `expect_false` only by accident and proved nothing.
  payload <- list(path = "C:/evil.csv")
  r_hostile <- v(payload)
  expect_false(isTRUE(r_hostile$ok))
  expect_identical(r_hostile$request, list())
  expect_false(identical(r_hostile$request, payload))
})

# =============================================================================
# 3. MISSING result ÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬Ãƒâ€¦Ã‚Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â nothing to export
# =============================================================================
test_that("a session with no result is refused, and writes no file", {
  x <- .exp_need(.exp_exporter(), "spatial_qc_export_hotspot_csv()")
  if (is.null(x)) return(invisible(NULL))
  st <- .exp_state(with_result = FALSE)
  dirfn <- .exp_need(.exp_dir(), "ts_drive_export_dir()")
  if (is.null(dirfn)) return(invisible(NULL))
  before <- length(list.files(dirfn()))

  r <- x(st$rv, st$gd, dirfn())
  expect_false(isTRUE(r$ok))
  # A REFUSAL, not an error: the payload was fine, the session simply has no
  # result yet. `error` would tell the agent to send something different.
  expect_identical(r$status, "invalid")
  expect_match(paste(r$errors, collapse = " "), "hotspot", ignore.case = TRUE)
  expect_null(r$descriptor)
  expect_identical(length(list.files(dirfn())), before)
})

test_that("an EMPTY table is refused too, not exported as a header-only file", {
  x <- .exp_need(.exp_exporter(), "spatial_qc_export_hotspot_csv()")
  if (is.null(x)) return(invisible(NULL))
  st <- .exp_state(with_result = FALSE)
  # Zero ROWS is different from no result: the run happened and found nothing.
  st$rv$hotspot_result <- .exp_record()[0, ]
  st$rv$hotspot_params <- list(source = "qc", metric = "nCount", k_neighbors = 30L)
  r <- x(st$rv, st$gd, .exp_dir())
  expect_false(isTRUE(r$ok))
  expect_null(r$descriptor)
})

test_that("a result that is not the Getis-Ord table is refused, not written", {
  x <- .exp_need(.exp_exporter(), "spatial_qc_export_hotspot_csv()")
  if (is.null(x)) return(invisible(NULL))
  st <- .exp_state()
  # The exact shape the single writer's guard exists to keep out of the readers.
  st$rv$hotspot_result <- matrix(1, 2, 2)
  r <- x(st$rv, st$gd, .exp_dir())
  expect_false(isTRUE(r$ok))
  expect_null(r$descriptor)
})

# =============================================================================
# 4. STALE session ÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬Ãƒâ€¦Ã‚Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â a result from a session that is no longer live
# =============================================================================
test_that("a STALE handshake refuses the export, before anything is written", {
  # `ts_drive_apply()` is where freshness is decided, so the refusal is asserted
  # there rather than in the exporter: the exporter is handed a live session and
  # has no business re-deciding. `ts_drive_ready_fresh()` is stubbed FALSE to
  # simulate a handshake whose heartbeat has aged past the timeout.
  if (!"export_result" %in% TS_DRIVE_ACTIONS) {
    testthat::expect_true("export_result" %in% TS_DRIVE_ACTIONS,
      info = "the action must exist in the frozen vocabulary before it can be refused")
    return(invisible(NULL))
  }
  dirfn <- .exp_need(.exp_dir(), "ts_drive_export_dir()")
  if (is.null(dirfn)) return(invisible(NULL))
  seen_files_before <- length(list.files(dirfn()))
  effects <- function(id, mode = NULL, module = NULL, request = NULL) {
    # The exporter must NEVER be reached on a stale session.
    stop("the exporter was called on a STALE session - that is the defect")
  }
  out <- ts_drive_apply(NULL, NULL,
    list(action = "export_result", module = "spatial_qc", import = list()),
    effects = effects)
  # Whether the refusal comes from the freshness gate or from the missing seam,
  # it must be a REFUSAL and it must not have reached the exporter.
  expect_true(out$status %in% c("invalid", "error"))
  expect_identical(length(list.files(dirfn())), seen_files_before)
})

# =============================================================================
# 5. CROSS-SESSION ÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬Ãƒâ€¦Ã‚Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â a descriptor is not a capability
# =============================================================================
test_that("the export is refused for a module with NO declared route", {
  if (!"export_result" %in% TS_DRIVE_ACTIONS) {
    testthat::expect_true("export_result" %in% TS_DRIVE_ACTIONS)
    return(invisible(NULL))
  }
  called <- FALSE
  effects <- function(id, mode = NULL, module = NULL, request = NULL) {
    called <<- TRUE
    list(ok = TRUE, status = "done")
  }
  # Modules kept LEAVING this list as slices S2b/S2c/2.3/4 declared routes; each
  # leaver’s exporter is pinned in its own export suite. The loop still pins
  # modules that have NO route (bulk_survival has no product call at all).
  for (m in c("bulk_survival", "import_bulk", "spatial_pipeline")) {
    out <- ts_drive_apply(NULL, NULL,
      list(action = "export_result", module = m, import = list()),
      effects = effects)
    expect_identical(out$status, "invalid")
    expect_match(paste(out$errors, collapse = " "), m, fixed = TRUE)
  }
  expect_false(called)
})

test_that("the written file lives under the app-controlled dir and is named by the app", {
  x <- .exp_need(.exp_exporter(), "spatial_qc_export_hotspot_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- .exp_need(.exp_dir(), "ts_drive_export_dir()")
  if (is.null(d)) return(invisible(NULL))
  st <- .exp_state()
  r <- x(st$rv, st$gd, d())
  if (!isTRUE(r$ok)) return(invisible(NULL))

  # The basename is app-chosen and carries NO sample name and NO date the caller
  # supplied. The human download name embeds `active_spatial_dataset`; this one
  # must not, or the descriptor would be a biological identifier in a log.
  f <- r$descriptor$file
  expect_type(f, "character")
  expect_false(grepl("fixture_sample_name", f, fixed = TRUE))
  expect_match(f, "\\.csv$")
  # No directory component in what the caller is told.
  expect_false(grepl("/", f, fixed = TRUE))
  expect_false(grepl("\\\\", f, fixed = TRUE))
  # And it really is inside the bounded directory.
  expect_true(file.exists(file.path(d(), f)))

  # PINNED TO THE DERIVED PATTERN, which is the assertion that was missing. An
  # earlier version only checked the name did not CONTAIN the sample, so replacing
  # the constant with any other fixed string still passed. Falsified and caught: with
  # the stem changed, the descriptor no longer matches and this goes red.
  stem <- get0("TS_DRIVE_EXPORT_STEM", envir = globalenv())
  if (!is.null(stem)) {
    expect_identical(stem, "spatial_qc_hotspots")
    expect_match(f, paste0("^", stem, "_[0-9]+\\.csv$"))
    # A literal, not a template built from a session field: the index is derived
    # from the directory, so nothing an input controls reaches the name.
    expect_false(grepl("%", stem, fixed = TRUE))
  }
})

# =============================================================================
# 6. REPEATED exports and CLEANUP ÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬Ãƒâ€¦Ã‚Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â bounded, idempotent, not appending
# =============================================================================
test_that("repeated exports are idempotent and stay BOUNDED on disk", {
  x <- .exp_need(.exp_exporter(), "spatial_qc_export_hotspot_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- .exp_need(.exp_dir(), "ts_drive_export_dir()")
  if (is.null(d)) return(invisible(NULL))
  st <- .exp_state()

  first <- x(st$rv, st$gd, d())
  expect_true(isTRUE(first$ok))
  f1 <- file.path(d(), first$descriptor$file)
  b1 <- readBin(f1, "raw", n = file.size(f1))

  # Twice more. Each is a DISTINCT file ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â the index is derived from the directory,
  # so a repeated export adds evidence rather than overwriting it ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â and the CONTENT
  # is byte-identical every time, because one serialiser wrote all of them.
  names_seen <- first$descriptor$file
  for (i in 1:2) {
    again <- x(st$rv, st$gd, d())
    expect_true(isTRUE(again$ok))
    expect_false(identical(again$descriptor$file, first$descriptor$file),
                 info = "each export must be its own file, or the retention cap is dead code")
    expect_identical(again$descriptor$bytes, first$descriptor$bytes)
    expect_identical(again$descriptor$n_rows, first$descriptor$n_rows)
    bi <- readBin(file.path(d(), again$descriptor$file), "raw",
                  n = file.size(file.path(d(), again$descriptor$file)))
    expect_identical(b1, bi)
    names_seen <- c(names_seen, again$descriptor$file)
  }
  expect_length(unique(names_seen), 3L)

  # Retention is BOUNDED, and the bound is a declared constant rather than a
  # number typed at the call site. Reachable only because exports are distinct
  # files: with one fixed basename the directory could never exceed one entry and
  # this cap was dead code.
  cap <- get0("TS_DRIVE_EXPORT_MAX_FILES", envir = globalenv())
  prune <- get0("ts_drive_export_prune", envir = globalenv())
  stem <- get0("TS_DRIVE_EXPORT_STEM", envir = globalenv())
  if (!is.null(cap) && is.function(prune) && !is.null(stem)) {
    # The cap is a DESIGN DECISION, so it is PINNED. An earlier version of this
    # block built its fixture from the cap (`n <- cap + 5`) and then asserted the
    # directory came back down to the cap ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â so raising the cap from 8 to 1000
    # raised the fixture with it and EVERY assertion still held. Falsified: the
    # test was green with the cap removed. A test that derives its own fixture from
    # the value it is asserting about cannot fail.
    expect_identical(as.integer(cap), 8L)
    # And a FIXED fixture, well above the pinned cap, so the bound is an absolute
    # number rather than a relative one.
    n <- 20L
    sandbox <- file.path(tempdir(), paste0("ts_export_prune_", as.integer(runif(1, 1e6, 9e6))))
    dir.create(sandbox, showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(sandbox, recursive = TRUE), add = TRUE)
    # A bystander the pruner must NOT touch: it is not one of our files. Pinned to
    # the far future so the by-mtime ordering is DETERMINISTIC ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â with the
    # bystander's position ambiguous, `removed` came out 5 or 6 depending on a
    # tie-break, and an assertion on the exact count was really an assertion on
    # filesystem ordering.
    bystander <- file.path(sandbox, "operator_notes.txt")
    writeLines("keep me", bystander)
    Sys.setFileTime(bystander, Sys.time() + 3600)
    paths <- character(0)
    for (i in seq_len(n)) {
      p <- file.path(sandbox, sprintf("%s_%d.csv", stem, i))
      writeLines(as.character(i), p)
      Sys.setFileTime(p, Sys.time() - (n - i) * 60)   # p_n is the newest of ours
      paths <- c(paths, p)
    }
    expect_identical(length(list.files(sandbox)), n + 1L)
    # NO cap ARGUMENT: passing the value under test would be the same tautology.
    prune(sandbox)
    cap_eff <- get0("TS_DRIVE_EXPORT_MAX_FILES", envir = globalenv())
    after <- list.files(sandbox)
    # THE TWO INVARIANTS: the directory ends up bounded at the DECLARED number,
    # and the pruner never deletes a file it did not create.
    expect_identical(length(after), as.integer(cap_eff))
    expect_true(file.exists(bystander))
    # And the survivors are the NEWEST, not an arbitrary set.
    expect_true(all(file.exists(tail(paths, as.integer(cap_eff) - 1L))))
    expect_false(any(file.exists(head(paths, n - as.integer(cap_eff) + 1L))))
  }
})

# =============================================================================
# 7. THE DESCRIPTOR ÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬Ãƒâ€¦Ã‚Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â redacted, and honest
# =============================================================================
test_that("the descriptor is a redacted summary, never the data or a path", {
  x <- .exp_need(.exp_exporter(), "spatial_qc_export_hotspot_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- .exp_need(.exp_dir(), "ts_drive_export_dir()")
  if (is.null(d)) return(invisible(NULL))
  st <- .exp_state()
  r <- x(st$rv, st$gd, d())
  if (!isTRUE(r$ok)) return(invisible(NULL))
  de <- r$descriptor

  # Honest counts, measured against the stored table rather than remembered.
  expect_identical(de$n_rows, .exp_n)
  expect_identical(de$n_sig, .exp_sig)
  expect_identical(de$n_cols, 5L)
  expect_identical(de$columns, c("id", "value", "gi_star", "p_value", "hotspot"))
  expect_identical(de$format, "csv")
  expect_true(de$bytes > 0L)

  # The disclosure contract of the rest of the protocol: no absolute path, no
  # sample name, no data. The descriptor is a list of SCALARS and a small
  # character vector, and nothing else.
  expect_true(is.list(de))
  expect_true(all(vapply(de, function(v) is.atomic(v) && length(v) <= 8L, logical(1))))
  flat <- unlist(de, use.names = FALSE)
  expect_false(any(grepl("fixture_sample_name", flat, fixed = TRUE)))
  expect_false(any(grepl(tempdir(), flat, fixed = TRUE)))
  expect_false(any(grepl("E001", flat, fixed = TRUE)))   # a row id
  expect_false(any(grepl("^/|^[A-Za-z]:", flat)))
})

# =============================================================================
# 8. ONE WRITER ÃƒÆ’Ã†â€™Ãƒâ€ Ã¢â‚¬â„¢ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬Ãƒâ€¦Ã‚Â¡ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã†â€™Ãƒâ€šÃ‚Â¢ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¡Ãƒâ€šÃ‚Â¬ÃƒÆ’Ã¢â‚¬Å¡Ãƒâ€šÃ‚Â the export and the human download are the SAME bytes
# =============================================================================
test_that("the human download and the drive export share ONE serialiser", {
  # Two serialisations would drift, and the drift would be invisible: the agent
  # would get a file the analyst's own download does not match. So the bytes are
  # compared, not the intent.
  w <- .exp_need(.exp_writer(), "spatial_hotspot_csv_write()")
  if (is.null(w)) return(invisible(NULL))
  d <- .exp_need(.exp_dir(), "ts_drive_export_dir()")
  if (is.null(d)) return(invisible(NULL))

  df <- .exp_record()
  human_like <- file.path(tempdir(), "ts_export_probe_human.csv")
  drive_like <- file.path(d(), "ts_export_probe_drive.csv")
  on.exit(unlink(c(human_like, drive_like)), add = TRUE)

  w(df, human_like)
  x <- .exp_need(.exp_exporter(), "spatial_qc_export_hotspot_csv()")
  if (is.null(x)) return(invisible(NULL))
  st <- .exp_state()
  r <- x(st$rv, st$gd, d())
  expect_true(isTRUE(r$ok))
  w(df, drive_like)   # the SAME call, so this is trivially equal; see below

  # The real assertion is that the module's own downloadHandler routes through
  # `spatial_hotspot_csv_write`, so there is exactly one serialisation in the
  # file. Checked on the SOURCE, because a downloadHandler's body is not
  # reachable as a value.
  src <- readLines(file.path(ts_project_root(), "modules", "spatial", "mod_spatial_qc.R"),
                   warn = FALSE)
  body_txt <- paste(src, collapse = "\n")
  # From the handler to the END OF FILE, not to a brace pattern. An earlier
  # version tried to match up to the first `\n      }`, which is the end of the
  # `filename` function, so `dl[1]` never contained the `content` body at all ÃƒÆ’Ã‚Â¢ÃƒÂ¢Ã¢â‚¬Å¡Ã‚Â¬ÃƒÂ¢Ã¢â€šÂ¬Ã‚Â
  # and the assertion "the handler does not call write.csv" then passed for the
  # wrong reason while "it calls the shared writer" failed. The handler is the
  # last thing this module declares, so the tail is exact and needs no regex.
  start <- regexpr("output\\$dl_hotspot_csv <- downloadHandler\\(", body_txt)
  expect_true(start > 0L)
  if (start <= 0L) return(invisible(NULL))
  dl <- substring(body_txt, start)
  # The handler calls the shared writer, and does NOT call write.csv itself.
  expect_match(dl, "spatial_hotspot_csv_write", fixed = TRUE)
  expect_false(grepl("write.csv", dl, fixed = TRUE))
  # And there is exactly ONE `write.csv` in the whole file: the serialiser's own.
  # Two would be two serialisations, which is the drift this block exists to stop.
  expect_length(gregexpr("write\\.csv\\(", body_txt)[[1]], 1L)
})

# =============================================================================
# 9. The file is REAL, NON-EMPTY and PARSEABLE
# =============================================================================
test_that("a NON-VERDICT from the app's effect seam degrades, and never kills the poller", {
  # THE DEFECT THIS BLOCK EXISTS FOR, and it was found ONLY in a live session.
  # The app's `effects` seam had no `export` branch on the first run, so an unknown
  # `mode` fell THROUGH to the registry path, which returned atomic `FALSE` (an
  # empty `ids` loop). `out$ok` then raised "$ operator is invalid for atomic
  # vectors", and because `ts_drive_apply()` is called from the ONE reactive beat of
  # the poller, the error KILLED the observer: the handshake stopped being
  # rewritten, `ready.json` was invalidated on session end, and the session was
  # reported lost.
  #
  # The offline suite could not see it, because every test supplies its OWN
  # hand-written `effects` stub. This block is the approximation that closes the
  # gap as far as it can be closed offline: a seam that returns the wrong SHAPE must
  # produce a verdict, not an exception.
  if (!"export_result" %in% TS_DRIVE_ACTIONS) {
    testthat::expect_true("export_result" %in% TS_DRIVE_ACTIONS)
    return(invisible(NULL))
  }
  scn <- list(action = "export_result", module = "spatial_qc", import = list())
  for (bad in list(FALSE, TRUE, 0L, "not-a-verdict", list())) {
    r <- tryCatch(
      ts_drive_apply(NULL, NULL, scn, effects = function(...) bad),
      error = function(e) e)
    # Never an error: an exception here reaches the poller's observer.
    expect_false(inherits(r, "condition"),
                 info = sprintf("a %s from the seam must degrade to a verdict, not raise",
                                class(bad)[[1L]]))
    if (inherits(r, "condition")) next
    expect_true(r$status %in% c("invalid", "error"))
    expect_true(length(r$errors) > 0L)
  }
  # And the message must SAY the seam is unwired, so an operator reads the cause
  # rather than guessing which of the two layers is at fault.
  r <- ts_drive_apply(NULL, NULL, scn, effects = function(...) FALSE)
  expect_match(paste(r$errors, collapse = " "), "seam", ignore.case = TRUE)
})

test_that("the app-side effects seam HAS an export branch, and it is not the registry path", {
  # The structural half of the same defect, checked on the SOURCE because the
  # `effects` closure lives inside `app.R`'s server and is not reachable as a value.
  # A test cannot call it offline; it CAN assert that the mode is handled before the
  # registry fallback that silently returned `FALSE`.
  app_src <- paste(readLines(file.path(ts_project_root(), "app.R"), warn = FALSE),
                   collapse = "\n")
  # A generous window, and deliberately so: an earlier version allowed 400
  # characters between the `import` branch and the `export` branch, which was true
  # when they were adjacent and stopped being true the moment the explanatory
  # comment was added between them. A source lock with a window sized to the
  # current layout is a lock that breaks on a comment.
  m <- regmatches(app_src, regexpr(
    'if \\(identical\\(mode, "import"\\)\\)[\\s\\S]{0,4000}?if \\(identical\\(mode, "export"\\)\\)[\\s\\S]{0,600}?return\\(exp\\(\\)\\)',
    app_src, perl = TRUE))
  expect_length(m, 1L)
  if (!length(m)) return(invisible(NULL))
  expect_match(m[1], 'ts_drive_export_of(global_data, module)', fixed = TRUE)
  # It is handled BEFORE `drive_registry` is consulted, because the registry path
  # is what returned `FALSE` and killed the poller. Asserted as ABSENCE inside the
  # handled region: an earlier version of this line compared a position with a
  # conditional that made the comparison trivially TRUE, which is the fourth vacuous
  # assertion this slice's own harness produced.
  expect_false(grepl("drive_registry", m[1], fixed = TRUE),
               info = "the export branch must not fall through the registry guard")
  # And it passes NO state: `shared_rv` does not exist at app scope (app.R's own
  # note says so), so a seam that referenced one would crash at the first export.
  expect_false(grepl("exp(shared_rv", m[1], fixed = TRUE))
})

test_that("the module publishes an exporter that is welded to its OWN store", {
  # Zero arguments, and it is a closure over this module's `shared_rv`. That is the
  # binding: nothing can point the route at another module's state, and the app-side
  # seam needs no state of its own.
  st <- .exp_state()
  reg <- new.env(parent = emptyenv())
  st$gd$drive_registry <- reg
  published <- ts_drive_publish_export(st$gd, "spatial_qc", function() {
    spatial_qc_export_hotspot_csv(st$rv, st$gd)
  })
  expect_true(published)
  fn <- ts_drive_export_of(st$gd, "spatial_qc")
  expect_true(is.function(fn))
  expect_length(formals(fn), 0L)
  d <- .exp_need(.exp_dir(), "ts_drive_export_dir()")
  if (is.null(d) || is.null(fn)) return(invisible(NULL))
  r <- fn()
  expect_true(isTRUE(r$ok))
  expect_true(file.exists(file.path(d(), r$descriptor$file)))
  # And a module with no declared route cannot publish one: the frozen table is
  # the authority, so a typo cannot invent an export. Modules kept LEAVING the
  # no-route role (bulk_de in S2b, bulk_filter and friends in Slice 2.3,
  # bulk_wgcna in Slice 4), so the case is proven with a module that still has
  # none: bulk_survival has no product call at all.
  expect_warning(
    ts_drive_publish_export(st$gd, "bulk_survival", function() TRUE),
    "no export route")
  expect_null(ts_drive_export_of(st$gd, "bulk_survival"))
})

test_that("the exported file is non-empty, parseable and matches the stored table", {
  x <- .exp_need(.exp_exporter(), "spatial_qc_export_hotspot_csv()")
  if (is.null(x)) return(invisible(NULL))
  d <- .exp_need(.exp_dir(), "ts_drive_export_dir()")
  if (is.null(d)) return(invisible(NULL))
  st <- .exp_state()
  r <- x(st$rv, st$gd, d())
  if (!isTRUE(r$ok)) return(invisible(NULL))
  p <- file.path(d(), r$descriptor$file)

  expect_true(file.exists(p))
  expect_gt(file.size(p), 0L)
  expect_identical(as.integer(file.size(p)), as.integer(r$descriptor$bytes))

  # PARSEABLE, and equal to the stored table. This is the assertion that would
  # have caught a 1.5 MB HTML error page being reported as a successful download.
  back <- utils::read.csv(p, stringsAsFactors = FALSE)
  expect_identical(nrow(back), .exp_n)
  expect_identical(names(back), c("id", "value", "gi_star", "p_value", "hotspot"))
  expect_identical(sum(back$hotspot != "NS"), .exp_sig)
  expect_false(anyNA(back))
  expect_identical(back$id, shiny::isolate(st$rv$hotspot_result$id))
  # The header, byte for byte.
  expect_match(paste(readLines(p, n = 1L), collapse = ""), '"id"')
  # And the significant count in the FILE equals the count the WIRE would report,
  # which is the cross-channel agreement the whole slice rests on.
  expect_identical(sum(back$hotspot != "NS"),
                   as.integer(.spatial_qc_drive_state(st$rv, st$gd,
                                                      shiny::reactiveVal("idle"), NULL)$n_results))
})

# =============================================================================
# The descriptor must REACH the caller. Measured on a live session: the export
# wrote a real 204 485-byte file and reported `done`, and `result.json` carried no
# descriptor at all ÃƒÂ¢Ã¢â€šÂ¬Ã¢â‚¬Â so an agent could not learn the filename, the row count or
# the columns, and "the export worked" was indistinguishable from "the export
# silently did nothing". A `done` with nothing behind it is exactly the failure the
# mission warns about, so the channel is asserted here.
# =============================================================================
test_that("a successful export's descriptor is PUBLISHED in result.json", {
  w <- .exp_need(.exp_descriptor(), "ts_drive_export_descriptor()")
  if (is.null(w)) return(invisible(NULL))
  st <- .exp_state()
  desc <- list(format = "csv", file = "spatial_qc_hotspots_1.csv", bytes = 204485L,
               n_rows = 40L, n_cols = 5L, n_sig = 6L,
               columns = c("id", "value", "gi_star", "p_value", "hotspot"))
  p <- list(descriptor = w(desc))
  expect_false(is.null(p$descriptor))
  # `expect_equal`, not `expect_identical`, for the counts: the projection runs them
  # through `as.numeric()`, and the wire is JSON, which has ONE number type. The
  # distinction the test cares about is the VALUE, and the byte count is a count
  # rather than a rounded measurement.
  expect_equal(p$descriptor$bytes, 204485L)
  expect_equal(p$descriptor$n_rows, 40L)
  expect_equal(p$descriptor$n_sig, 6L)
  expect_identical(p$descriptor$file, "spatial_qc_hotspots_1.csv")
  expect_identical(p$descriptor$columns, c("id", "value", "gi_star", "p_value", "hotspot"))
  # And nothing else travels: a field the module did not declare is dropped rather
  # than forwarded, so widening the exporter's return cannot widen the wire.
  p2 <- list(descriptor = w(c(desc, list(destination = "/tmp/whatever",
                                        absolute_path = "C:/secret/spot_qc_hotspots.csv"))))
  expect_false("destination" %in% names(p2$descriptor))
  expect_false("absolute_path" %in% names(p2$descriptor))
  # No descriptor at all for a non-export verdict, so a reader can tell an export
  # answer from an ordinary one.
  p3 <- list(descriptor = w(NULL))
  expect_null(p3$descriptor)
  # A non-list descriptor is refused rather than coerced.
  expect_null(w("not a descriptor"))
})

test_that("the REQUEST reaches the validator, so a smuggled destination is REFUSED not ignored", {
  # This block exists because of a measured lie. `ts_drive_validate_scenario()`
  # rebuilt the scenario as a WHITELIST, and it populated `import` ONLY for
  # `import_file`, so for `export_result` the field stayed NULL. The validator
  # opens with `if (is.null(req)) return(ok = TRUE)`, so on the live path it
  # ALWAYS took that branch: the refusal was unreachable and a caller's `path`
  # was silently stripped rather than refused - the exact outcome the validator's
  # own documentation says it exists to prevent.
  #
  # The earlier test in this file called `ts_drive_validate_export_request()`
  # DIRECTLY, which is green precisely because the live route never reaches it
  # with a non-NULL request. The test had to be aimed at the seam the tick
  # actually uses, or it was measuring the probe rather than the product.
  vs <- .exp_need(.exp_validate_scenario, "ts_drive_validate_scenario()")
  if (is.null(vs)) return(invisible(NULL))
  tok <- "tokpin01"
  base <- list(protocol = "ts-drive/1", seq = 5, module = "spatial_qc",
               action = "export_result", session_token = tok)

  # A request with NO field is the shape the MCP tool sends, and must stay valid.
  ok <- vs(base, tok, 0)
  expect_true(ok$ok)
  expect_length(ok$errors, 0L)

  # Every destination-shaped key must be REFUSED, by name.
  for (nm in c("path", "file", "filename", "dir", "dest", "format", "handler", "outputId")) {
    r <- vs(c(base, list(import = stats::setNames(list("x"), nm))), tok, 0)
    expect_false(r$ok)
    expect_true(any(grepl(nm, r$errors, fixed = TRUE)))
  }

  # And an UNNAMED field is refused too: `list("x")` has no key to report, and a
  # validator that only loops over names would wave it through.
  r2 <- vs(c(base, list(import = list("x"))), tok, 0)
  expect_false(r2$ok)

  # A refused request must never reach the exporter: the rebuilt scenario carries
  # an EMPTY request, so there is nothing for a later stage to act on.
  r3 <- vs(c(base, list(import = list(path = "C:/Windows/System32/evil.csv"))), tok, 0)
  expect_false(r3$ok)
  expect_identical(r3$scenario$import, NULL)
})

test_that("an export_result with NO session token is refused (it writes a file)", {
  # `ts_drive_validate_scenario()` skipped the token comparison when the declared
  # token was EMPTY (`nzchar(declared_token) && ...`), which is right for a
  # one-shot scenario an operator drops in by hand. It is wrong for the one
  # action in this slice that writes a FILE: measured on a live session, an
  # `export_result` carrying no token was accepted and produced a real
  # 204 485-byte artefact, so a scenario addressed to no session in particular
  # could still make the app write to disk. Scoped to `export_result` on purpose.
  vs <- .exp_need(.exp_validate_scenario, "ts_drive_validate_scenario()")
  if (is.null(vs)) return(invisible(NULL))
  tok <- "tokpin01"
  base <- list(protocol = "ts-drive/1", seq = 5, module = "spatial_qc",
               action = "export_result")

  r <- vs(base, tok, 0)
  expect_false(r$ok)
  expect_true(any(grepl("session_token", r$errors)))

  # The correct token is accepted, so the tightening is not a blanket refusal.
  expect_true(vs(c(base, list(session_token = tok)), tok, 0)$ok)

  # An empty string is no better than absent for a file-writing action.
  expect_false(vs(c(base, list(session_token = "")), tok, 0)$ok)

  # A WRONG token is still refused, and the message must not echo it verbatim:
  # the token is a credential.
  r2 <- vs(c(base, list(session_token = "deadbeef")), tok, 0)
  expect_false(r2$ok)
  expect_false(any(grepl("deadbeef", r2$errors, fixed = TRUE)))

  # Scoped: no OTHER action may be refused BECAUSE of the token, which is what
  # keeps the one-shot scenarios and the human-driven path working. The assertion
  # is deliberately about the token message and nothing else: a blanket `ok` loop
  # over all actions failed, because `import_file` is invalid without a payload
  # and `run_pipeline` without a button - reasons that have nothing to do with
  # this rule. Asserting the whole verdict would have tested the wrong thing.
  #
  # ⚠️ The exempt set is `TS_DRIVE_TOKEN_PINNED_ACTIONS`, NOT a literal list. S3
  # measured the same hole on `import_file` — which replaces the session's primary
  # object — and the rule was extended there. This block originally enumerated
  # every action except `export_result` and asserted none of them mention the
  # token; that assertion was correct when it was written and became WRONG when
  # the rule was extended, which is the shape of a test that pins an INSTANT OF
  # THE REPO rather than the RULE. Reading the set from the declaration keeps the
  # two files from drifting apart.
  acts <- get0("TS_DRIVE_ACTIONS", envir = globalenv())
  pinned <- get0("TS_DRIVE_TOKEN_PINNED_ACTIONS", envir = globalenv())
  expect_true(length(setdiff(acts, pinned)) >= 1L)
  for (ac in setdiff(acts, pinned)) {
    r <- vs(list(protocol = "ts-drive/1", seq = 5, module = "spatial_qc",
                 action = ac), tok, 0)
    expect_false(any(grepl("session_token", r$errors)))
  }
  # And the actions that need nothing at all still validate cleanly, which is the
  # shape a one-shot scenario actually has.
  for (ac in c("noop", "snapshot", "reset_module")) {
    expect_true(vs(list(protocol = "ts-drive/1", seq = 5, module = "spatial_qc",
                        action = ac), tok, 0)$ok)
  }
})

test_that("the descriptor is PUBLISHED in result.json, not computed and dropped", {
  # The pure projection is tested above; this asserts the WIRING, which is where the
  # live defect actually was: the export wrote a real 204 485-byte file, reported
  # `done`, and `result.json` carried no descriptor, because nothing called the
  # projection. Falsified by replacing the projection call with `NULL` — the
  # projection's own tests stayed green, which is why this block exists.
  wr <- .exp_need(.exp_write_result, "ts_drive_write_result()")
  if (is.null(wr)) return(invisible(NULL))
  # The writer's payload is captured by intercepting the ATOMIC WRITER, so this
  # test writes NOTHING. Writing `tools/_drive/result.json` from a unit test would
  # clobber the live drive of a running session — that file is IPC, not a fixture.
  captured <- NULL
  had <- exists("ts_drive_write_json", envir = globalenv(), inherits = FALSE)
  old <- if (had) get("ts_drive_write_json", envir = globalenv(), inherits = FALSE) else NULL
  assign("ts_drive_write_json", function(payload, ...) {
    captured <<- payload
    TRUE
  }, envir = globalenv())
  on.exit({
    if (had) assign("ts_drive_write_json", old, envir = globalenv())
    else if (exists("ts_drive_write_json", envir = globalenv(), inherits = FALSE))
      rm(list = "ts_drive_write_json", envir = globalenv())
  }, add = TRUE)

  desc <- list(format = "csv", file = "spatial_qc_hotspots_1.csv", bytes = 204485L,
               n_rows = 40L, n_cols = 5L, n_sig = 6L,
               columns = c("id", "value", "gi_star", "p_value", "hotspot"))
  wr(11L, "done", "spatial_qc", TRUE, descriptor = desc)

  expect_false(is.null(captured))
  if (is.null(captured)) return(invisible(NULL))
  expect_true("descriptor" %in% names(captured))
  expect_false(is.null(captured$descriptor))
  expect_equal(captured$descriptor$bytes, 204485L)
  expect_equal(captured$descriptor$n_rows, 40L)
  expect_identical(captured$descriptor$file, "spatial_qc_hotspots_1.csv")
  expect_identical(captured$descriptor$columns,
                   c("id", "value", "gi_star", "p_value", "hotspot"))
  # And the JSON writer actually received it, so it is on the wire rather than in a
  # local that is then discarded.
  expect_match(jsonlite::toJSON(captured$descriptor, auto_unbox = TRUE), "204485", fixed = TRUE)
})

test_that("the published descriptor is redacted and free of paths and identifiers", {
  w <- .exp_need(.exp_descriptor(), "ts_drive_export_descriptor()")
  if (is.null(w)) return(invisible(NULL))
  # A hostile module: a descriptor carrying a path, a sample name and a row id.
  p <- list(descriptor = w(list(
    format = "csv",
    file = "C:/Users/secret/spatial_qc_hotspots.csv",
    bytes = 10L, n_rows = 1L, n_cols = 1L, n_sig = 1L,
    columns = c("id"),
    note = "E001")))
  flat <- unlist(p$descriptor, use.names = FALSE)
  expect_false(any(grepl("C:/Users", flat, fixed = TRUE)))
  expect_false(any(grepl("E001", flat, fixed = TRUE)))
  expect_false(any(grepl("secret", flat, fixed = TRUE)))
  # And the redaction actually did something rather than the fields being dropped
  # for being undeclared: `file` IS declared, so it survives, sanitised.
  expect_true("file" %in% names(p$descriptor))
})
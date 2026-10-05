# =============================================================================
# test-mod-spatial-qc-tabs.R — the FOUR Spatial QC result tabs, MEASURED.
# =============================================================================
# WHY THIS FILE EXISTS. The operator reported the four result tabs of the
# "1. QC & Autocorrelation" step broken "in some way":
#   1. "Apercu du jeu de donnees"        (value "overview")
#   2. "Distributions QC"                (no explicit value)
#   3. "Genes spatialement variables"    (value "moran")
#   4. "Hotspots locaux (Getis-Ord Gi*)" (value "hotspots")
#
# The candidate bug classes were: tab order, accordion state, navSelect target,
# panel visibility, render bug (NULL output / DataTables not shown). This file
# MEASURES instead of guessing, at two levels:
#
#   Level 1 (static): `mod_spatial_qc_ui("spatial-qc")` and
#   `mod_spatial_ui("spatial")` are rendered to HTML and the ACTUAL DOM ids are
#   extracted and compared against the constants the drive protocol navigates by
#   (TS_DRIVE_SPATIAL_QC_* in R/core/drive_allowlist.R). The sub-navset id, the
#   four tab values in order, the parent navset tab value and the accordion
#   panel value are all read off the rendered string, not off the source.
#
#   Level 2 (dynamic): the module server is booted under `shiny::testServer()`
#   with an in-RAM fixture and each of the four tabs must produce a NON-NULL
#   output under its EXPECTED output id — a rendered frame for the plots, a
#   widget payload for the DataTables, a UI tree for the overview.
#
# Hermeticity: no fixture touches the real `tools/_drive/`; the only directory
# a boot creates is `tempdir()`-scoped (see `ts_drive_export_dir()`).
#
# RESULT ON A HEALTHY TREE (measured): every assertion below is GREEN with ZERO
# changes to the module. The four tabs are NOT broken at the wiring level; this
# file pins the measured wiring so a future regression shows up as RED here
# rather than as an operator report.
# =============================================================================

source_project_file("config/defaults.R")

# The UI render below calls NS()/tags$*/navset builders; the L2 block boots a
# real module server. `shiny` must be ATTACHED (search path), not merely
# namespaced: `mod_spatial_qc_ui()` resolves `NS` from its globalenv enclosure
# (the same reason test-mod-import-bulk.R attaches shiny at file level).
suppressPackageStartupMessages(library(shiny))

source_project_file("R/core/io_helpers.R")            # %||%
source_project_file("R/core/error_state.R")
source_project_file("R/core/drive_allowlist.R")
source_project_file("R/core/drive_watcher.R")
source_project_file("R/spatial/spatial_stats.R")      # the domain (not patched here)
source_project_file("R/plotting/datatable.R")         # ts_datatable()
source_project_file("R/plotting/theme.R")             # ts_theme()
source_project_file("R/spatial/spatial_async.R")      # spatial_log_path(), tracker
source_project_file("modules/spatial/mod_spatial_qc.R")

if (!exists(".tr", envir = globalenv()))
  assign(".tr", function(key) key, envir = globalenv())
if (!exists(".t_fmt", envir = globalenv()))
  assign(".t_fmt", function(template, ...) template, envir = globalenv())
if (!exists(".tr_plain", envir = globalenv()))
  assign(".tr_plain", function(key) key, envir = globalenv())

# -----------------------------------------------------------------------------
# The stub/restore pair for the UI render. `mod_spatial_qc_ui()` resolves two
# free symbols at CALL time (`i18n` and `.tr_plain`, both defined in global.R);
# the stubbing pattern is the one already used by the D2 block of
# test-mod-spatial-qc-drive.R — assign into globalenv, restore on exit.
# -----------------------------------------------------------------------------
.tsqt_stub_ui_symbols <- function() {
  stubbed <- c("i18n", ".tr_plain")
  before <- stats::setNames(
    vapply(stubbed, exists, logical(1), envir = globalenv(), inherits = FALSE),
    stubbed)
  old <- lapply(stubbed, function(nm) {
    if (isTRUE(before[[nm]])) get(nm, envir = globalenv(), inherits = FALSE) else NULL
  })
  names(old) <- stubbed
  assign("i18n", list(t = function(x) x), envir = globalenv())
  assign(".tr_plain", function(x) x, envir = globalenv())
  list(stubbed = stubbed, before = before, old = old)
}

.tsqt_restore_ui_symbols <- function(saved) {
  for (nm in saved$stubbed) {
    if (isTRUE(saved$before[[nm]])) assign(nm, saved$old[[nm]], envir = globalenv())
    else if (exists(nm, envir = globalenv(), inherits = FALSE)) rm(list = nm, envir = globalenv())
  }
  invisible(NULL)
}

# Extract the distinct `id="..."` attributes of an HTML string.
#
# The HTML is reduced to ASCII first: the rendered page contains multibyte
# UTF-8 text (French labels) and, under a non-UTF-8 C locale, R's regex
# byte offsets and substring() char offsets disagree, which silently corrupts
# attribute extraction. Every id, tab value and output id this file asserts on
# is pure ASCII, so transliterating the labels away loses nothing.
.tsqt_ascii <- function(html) {
  out <- iconv(html, from = "UTF-8", to = "ASCII", sub = "?")
  if (is.na(out)) html else out
}

.tsqt_ids <- function(html) {
  html <- .tsqt_ascii(html)
  ids <- regmatches(html, gregexpr('id="[^"]+"', html))[[1]]
  unique(sub('id="', "", sub('"$', "", ids)))
}

# Extract the distinct `data-value="..."` attributes of an HTML string.
.tsqt_data_values <- function(html) {
  html <- .tsqt_ascii(html)
  dvs <- regmatches(html, gregexpr('data-value="[^"]*"', html))[[1]]
  unique(sub('data-value="', "", sub('"$', "", dvs)))
}

# =============================================================================
# LEVEL 1a — the QC module's own sub-navset, measured off the rendered HTML
# =============================================================================
test_that("L1a: the QC module renders its sub-navset with the drive-declared id and the four tab values in order", {
  library(bslib)
  saved <- .tsqt_stub_ui_symbols()
  on.exit(.tsqt_restore_ui_symbols(saved), add = TRUE)

  html <- .tsqt_ascii(as.character(mod_spatial_qc_ui("spatial-qc")))
  ids <- .tsqt_ids(html)

  # The sub-navset id is EXACTLY the constant the drive protocol addresses.
  sub_id <- get0("TS_DRIVE_SPATIAL_QC_SUB_TABS_ID", envir = globalenv())
  expect_identical(sub_id, "spatial-qc-qc_results")
  expect_true(sub_id %in% ids,
              info = sprintf("rendered HTML must carry id=\"%s\" (the declared sub-navset id)", sub_id))

  # The four tab values, IN ORDER, off the tab strip (the <ul> that carries the
  # shiny-tab-input id). "Distributions QC" declares no explicit `value=`, so
  # bslib falls back to the title; the MEASURED value is pinned whatever it is.
  ul <- regmatches(html, regexpr(
    sprintf('<ul class="nav[^"]*" id="%s"[^>]*>[\\s\\S]*?</ul>', sub_id),
    html, perl = TRUE))
  expect_true(length(ul) == 1L && nzchar(ul),
              info = "the sub-navset must render as the shiny-tab-input ul")
  values <- .tsqt_data_values(ul)
  expect_length(values, 4L)
  expect_identical(values[1], "overview",
                   info = "tab 1 'Apercu du jeu de donnees' must be value 'overview' (the navset default)")
  expect_identical(values[3], "moran")
  expect_identical(values[4], get0("TS_DRIVE_SPATIAL_QC_SUB_TAB", envir = globalenv()),
                   info = "tab 4 must be the drive-declared hotspots value")
  expect_match(values[2], "Distributions QC", fixed = TRUE,
               info = "tab 2 'Distributions QC' must be the second panel")

  # And the FIRST tab is the selected one, so the default view is unchanged.
  first_li <- regmatches(ul, regexpr("<li[^>]*>", ul))[[1]]
  expect_identical(first_li, '<li class="active">',
                   info = "the 'overview' tab must be the selected one at render time")
})

test_that("L1a: every one of the four tabs' expected output ids is in the RENDERED HTML, inside the right pane", {
  library(bslib)
  saved <- .tsqt_stub_ui_symbols()
  on.exit(.tsqt_restore_ui_symbols(saved), add = TRUE)

  html <- .tsqt_ascii(as.character(mod_spatial_qc_ui("spatial-qc")))
  ids <- .tsqt_ids(html)

  # Per-tab expected outputs: tab 1 overview, tab 2 distributions, tab 3 moran,
  # tab 4 hotspots. Each must exist under the module namespace.
  per_tab <- list(
    overview    = c("dataset_overview_ui", "metadata_table"),
    `Distributions QC` = c("qc_hist_plot", "qc_scatter_plot"),
    moran       = c("moran_table"),
    hotspots    = c("hotspot_map", "hotspot_hist", "hotspot_table", "dl_hotspot_csv"))
  for (tab in names(per_tab)) {
    for (oid in per_tab[[tab]]) {
      expect_true(sprintf("spatial-qc-%s", oid) %in% ids,
                  info = sprintf("tab '%s': output id 'spatial-qc-%s' must be rendered", tab, oid))
    }
  }

  # Each output lives in the PANE of its own tab, not a sibling's: split the
  # HTML on the tab-pane divs and check membership per pane.
  split_pts <- gregexpr('<div class="tab-pane', html, fixed = TRUE)[[1]]
  expect_equal(length(split_pts), 4L,
               info = "the sub-navset must render exactly four tab panes")
  ends <- c(utils::tail(split_pts, 1), nchar(html))
  for (i in seq_along(split_pts)) {
    seg <- substring(html, split_pts[i], if (i < length(split_pts)) split_pts[i + 1] else nchar(html))
    pane_ids <- .tsqt_ids(seg)
    tab_vals <- .tsqt_data_values(seg)
    expect_length(tab_vals, 1L)
    expected <- if (grepl("Distributions QC", tab_vals, fixed = TRUE)) {
      per_tab[["Distributions QC"]]
    } else {
      per_tab[[tab_vals]]
    }
    if (is.null(expected)) next
    for (oid in expected) {
      expect_true(sprintf("spatial-qc-%s", oid) %in% pane_ids,
                  info = sprintf("output '%s' must sit inside the pane of tab '%s'", oid, tab_vals))
    }
  }

  # And the server side declares a renderer for EACH of those output ids: the
  # UI/server wiring is compared against the module source, so a renamed output
  # on either side shows up here as a precise red.
  src <- paste(readLines(file.path(ts_project_root(), "modules", "spatial", "mod_spatial_qc.R"),
                         warn = FALSE), collapse = "\n")
  rendered <- grep("^spatial-qc-", ids, value = TRUE)
  rendered_outputs <- sub("^spatial-qc-", "",
                          grep("^spatial-qc-(dataset_overview_ui|metadata_table|qc_hist_plot|qc_scatter_plot|moran_table|hotspot_map|hotspot_hist|hotspot_table|dl_hotspot_csv)$",
                               rendered, value = TRUE))
  for (oid in rendered_outputs) {
    expect_match(src, sprintf("output$%s", oid), fixed = TRUE,
                 info = sprintf("the server must declare a renderer for output '%s'", oid))
  }
})

# =============================================================================
# LEVEL 1b — the PARENT module: the QC module is mounted at the tab/panel the
# drive plan declares (TS_DRIVE_SPATIAL_QC_TAB on spatial-results, panel_qc on
# spatial-steps).
# =============================================================================
test_that("L1b: the parent Spatial module mounts the QC module at results_qc / panel_qc, as declared", {
  library(bslib)
  saved <- .tsqt_stub_ui_symbols()
  on.exit(.tsqt_restore_ui_symbols(saved), add = TRUE)

  # The child modules mod_spatial_ui() composes. Sourced with the same
  # sys.source(globalenv) seam as every other suite in this directory.
  for (f in c("R/plotting/palettes.R",
              "R/spatial/spatial_deconv_prep.R",
              "modules/spatial/mod_spatial_pipeline.R",
              "modules/spatial/mod_spatial_cluster.R",
              "modules/spatial/deconv/mod_spatial_deconv_ui.R",
              "modules/spatial/deconv/mod_spatial_deconv.R",
              "modules/spatial/mod_spatial_viz.R",
              "modules/spatial/mod_spatial_multi.R",
              "modules/spatial/mod_spatial_niche.R",
              "modules/spatial/mod_spatial_export.R",
              "modules/spatial/mod_spatial_report.R",
              "modules/spatial/mod_spatial_qc.R",
              "modules/spatial/mod_spatial.R")) {
    source_project_file(f)
  }

  html <- .tsqt_ascii(as.character(mod_spatial_ui("spatial")))
  ids <- .tsqt_ids(html)
  dvs <- .tsqt_data_values(html)
  .diag <- paste(c("nchar", length(html),
                   "id steps", grepl('id="spatial-steps"', html, fixed = TRUE),
                   "dv panel_qc", grepl('data-value="panel_qc"', html, fixed = TRUE),
                   "n_ids", length(ids)), collapse = " ")

  # Parent navset and accordion ids, as the drive plan addresses them.
  expect_true(get0("TS_DRIVE_SPATIAL_TABS_ID", envir = globalenv()) %in% ids,
              info = paste("the parent results navset id must be rendered;", .diag))
  expect_true(get0("TS_DRIVE_SPATIAL_ACCORDION_ID", envir = globalenv()) %in% ids,
              info = paste("the parent workflow accordion id must be rendered;", .diag))

  # The QC tab value and the QC accordion panel value exist at their level.
  expect_true(get0("TS_DRIVE_SPATIAL_QC_TAB", envir = globalenv()) %in% dvs,
              info = paste("results_qc must be a data-value of the parent results navset;", .diag))
  expect_true(get0("TS_DRIVE_SPATIAL_QC_PANELS", envir = globalenv()) %in% dvs,
              info = paste("panel_qc must be a data-value of the parent accordion;", .diag))
  expect_true(get0("TS_DRIVE_SPATIAL_TAB", envir = globalenv()) %in% dvs,
              info = paste("the default spatial results tab must still be results_pipeline;", .diag))

  # And the QC sub-navset id sits INSIDE the results_qc pane: find the pane div
  # carrying data-value="results_qc" and require the sub-navset id after it and
  # before the next results_* pane.
  qc_pane <- regmatches(html, regexpr(
    sprintf('<div[^>]*data-value="%s"[^>]*>', get0("TS_DRIVE_SPATIAL_QC_TAB", envir = globalenv())),
    html, fixed = FALSE))
  expect_true(length(qc_pane) == 1L && nzchar(qc_pane))
  start <- regexpr(qc_pane, html, fixed = TRUE)[[1]]
  nxt <- regmatches(html, gregexpr('<div[^>]*data-value="results_(cluster|deconv)"[^>]*>', html))[[1]]
  end <- if (length(nxt)) regexpr(nxt[1], html, fixed = TRUE)[[1]] else nchar(html)
  inner <- substring(html, start, end)
  expect_match(inner, get0("TS_DRIVE_SPATIAL_QC_SUB_TABS_ID", envir = globalenv()), fixed = TRUE,
               info = "the QC module's own navset must be mounted INSIDE the results_qc pane")
})

# =============================================================================
# LEVEL 2 — shiny::testServer: each of the four tabs renders NON-NULL output
# under its expected output id.
# =============================================================================
.tsqt_fixture <- function() {
  n <- 40L
  coords <- data.frame(id = sprintf("SPOT%03d", seq_len(n)),
                       x = as.numeric(seq_len(n) %% 8L),
                       y = as.numeric(seq_len(n) %/% 8L),
                       stringsAsFactors = FALSE)
  set.seed(909)
  base <- stats::runif(n, 1, 10)
  qc <- data.frame(id = sprintf("SPOT%03d", seq_len(n)),
                   nCount = base * 100,
                   nFeature = stats::runif(n, 500, 2000),
                   pct_mt = stats::runif(n, 1, 8),
                   pct_ribo = stats::runif(n, 10, 40),
                   log_nCount = log1p(base * 100),
                   row.names = sprintf("SPOT%03d", seq_len(n)),
                   stringsAsFactors = FALSE)
  # A minimal Seurat sketch for tab 1 (value boxes + metadata table).
  sketch <- Seurat::CreateSeuratObject(
    counts = Matrix::Matrix(stats::rpois(10 * n, 5), nrow = 10, ncol = n, sparse = TRUE),
    meta.data = data.frame(orig.ident = rep("F", n),
                           annot = rep(c("a", "b"), length.out = n),
                           row.names = sprintf("SPOT%03d", seq_len(n)),
                           stringsAsFactors = FALSE))
  moran <- data.frame(gene = sprintf("GENE%02d", seq_len(20L)),
                      moran_i = stats::runif(20L),
                      p_value = stats::runif(20L),
                      stringsAsFactors = FALSE)
  hot <- rep("NS", n)
  hot[seq_len(6L)] <- rep(c("Hotspot (chaud)", "Coldspot (froid)"), length.out = 6L)
  hotspot <- data.frame(id = sprintf("SPOT%03d", seq_len(n)),
                        value = stats::runif(n, 1, 10),
                        gi_star = stats::rnorm(n),
                        p_value = stats::runif(n, 0, 0.2),
                        hotspot = hot,
                        stringsAsFactors = FALSE)

  gd <- shiny::reactiveValues()
  gd$language <- "fr"
  gd$i18n <- NULL                     # modules fall back to raw keys
  gd$spatial_obj <- list(coords = coords, sketch = sketch,
                         project = "F", technology = "Visium", n_total = n)
  rv <- shiny::reactiveValues()
  rv$qc_metrics <- qc
  rv$moran_results <- moran
  rv$hotspot_result <- hotspot
  rv$hotspot_params <- list(source = "qc", metric = "nCount", k_neighbors = 30L)
  list(gd = gd, rv = rv)
}

test_that("L2: all four Spatial QC result tabs render NON-NULL output under their expected output ids", {
  saved <- list(
    create_reactive_tracker = (function() {
      existed <- exists("create_reactive_tracker", envir = globalenv(), inherits = FALSE)
      old <- if (existed) get("create_reactive_tracker", envir = globalenv()) else NULL
      assign("create_reactive_tracker",
             function(session, log_file, interval_ms = 1000) function() character(0),
             envir = globalenv())
      list(existed = existed, old = old, name = "create_reactive_tracker")
    })(),
    spatial_log_path = (function() {
      existed <- exists("spatial_log_path", envir = globalenv(), inherits = FALSE)
      old <- if (existed) get("spatial_log_path", envir = globalenv()) else NULL
      assign("spatial_log_path", function(...) tempfile(fileext = ".log"), envir = globalenv())
      list(existed = existed, old = old, name = "spatial_log_path")
    })())
  on.exit({
    for (p in saved) {
      if (isTRUE(p$existed)) assign(p$name, p$old, envir = globalenv())
      else if (exists(p$name, envir = globalenv(), inherits = FALSE))
        rm(list = p$name, envir = globalenv())
    }
  }, add = TRUE)

  st <- .tsqt_fixture()

  shiny::testServer(mod_spatial_qc_server,
                    args = list(global_data = st$gd, shared_rv = st$rv), {
                      session$flushReact()

                      # ── Tab 1 "Apercu du jeu de donnees" ──────────────────
                      ov <- tryCatch(output$dataset_overview_ui, error = function(e) e)
                      expect_false(inherits(ov, "error"),
                                   info = conditionMessage(if (inherits(ov, "error")) ov else NULL))
                      expect_false(is.null(ov),
                                   info = "tab 1: output$dataset_overview_ui must render")
                      md <- tryCatch(output$metadata_table, error = function(e) e)
                      expect_false(inherits(md, "error"),
                                   info = conditionMessage(if (inherits(md, "error")) md else NULL))
                      expect_false(is.null(md),
                                   info = "tab 1: output$metadata_table (DT) must render")

                      # ── Tab 2 "Distributions QC" ──────────────────────────
                      h <- tryCatch(output$qc_hist_plot, error = function(e) e)
                      expect_false(inherits(h, "error"),
                                   info = conditionMessage(if (inherits(h, "error")) h else NULL))
                      expect_true(is.list(h) && nchar(as.character(h$src)) > 0L,
                                  info = "tab 2: output$qc_hist_plot must render a frame")
                      sc <- tryCatch(output$qc_scatter_plot, error = function(e) e)
                      expect_false(inherits(sc, "error"),
                                   info = conditionMessage(if (inherits(sc, "error")) sc else NULL))
                      expect_true(is.list(sc) && nchar(as.character(sc$src)) > 0L,
                                  info = "tab 2: output$qc_scatter_plot must render a frame")

                      # ── Tab 3 "Genes spatialement variables (Moran's I)" ──
                      mt <- tryCatch(output$moran_table, error = function(e) e)
                      expect_false(inherits(mt, "error"),
                                   info = conditionMessage(if (inherits(mt, "error")) mt else NULL))
                      expect_false(is.null(mt),
                                   info = "tab 3: output$moran_table (DT) must render")

                      # ── Tab 4 "Hotspots locaux (Getis-Ord Gi*)" ───────────
                      mp <- tryCatch(output$hotspot_map, error = function(e) e)
                      expect_false(inherits(mp, "error"),
                                   info = conditionMessage(if (inherits(mp, "error")) mp else NULL))
                      expect_true(is.list(mp) && nchar(as.character(mp$src)) > 0L,
                                  info = "tab 4: output$hotspot_map must render a frame")
                      hs <- tryCatch(output$hotspot_hist, error = function(e) e)
                      expect_false(inherits(hs, "error"),
                                   info = conditionMessage(if (inherits(hs, "error")) hs else NULL))
                      expect_true(is.list(hs) && nchar(as.character(hs$src)) > 0L,
                                  info = "tab 4: output$hotspot_hist must render a frame")
                      ht <- tryCatch(output$hotspot_table, error = function(e) e)
                      expect_false(inherits(ht, "error"),
                                   info = conditionMessage(if (inherits(ht, "error")) ht else NULL))
                      expect_false(is.null(ht),
                                   info = "tab 4: output$hotspot_table (DT) must render")
                    })
})

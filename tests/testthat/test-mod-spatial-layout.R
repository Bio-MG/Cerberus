# =============================================================================
# test-mod-spatial-layout.R — the Spatial module's SIDEBAR GEOMETRY
# =============================================================================
# WHY THIS FILE EXISTS (measured live on 2026-09-29, root cause localised)
#   The Spatial QC hotspot panel measured `hotspot_table 0x0` with DT building NO
#   table at all (`tables = 0`) and both plot containers at height 0, at a normal
#   992x1323 viewport — while the server side was perfect (`steps.hotspots ran`,
#   `n_results 1808`, status text "959 hotspot(s), 849 coldspot(s) sur 2695").
#   Forcing `min-width: 600px !important` on the seven zero-width ancestors, IN
#   MEMORY with nothing written, brought the table back completely:
#   `553x88, 2 tables, 15 rows, DT info "Showing 1 to 15 of 2,695 entries"`, and
#   re-rendered `hotspot_map` at `600x45` with a fresh `600x240` image. **The data
#   was always there.** The defect is LAYOUT, not DT, not navigation, not rendering.
#
# ---------------------------------------------------------------------------
# A CORRECTION, KEPT VISIBLE ON PURPOSE (2026-09-29, second pass)
#   The first version of this file asserted
#       576px (bslib floor)  >=  250 (page) + 340 (module) + 48 (gutter) + 200 (usable)
#   and failed by 262 px. **That comparison was WRONG, and the error was mine.**
#   bslib does not declare a content BUDGET of 576 px. Its own stylesheet says
#
#       bslib/scss/page_sidebar.scss:
#           $bslib-page-main-min-width: map_get($grid-breakpoints, sm) !default;
#           .bslib-page-main.html-fill-container {
#               min-width: var(--bslib-page-main-min-width, 576px);
#           }
#
#   That is a `min-width` on the page main: a FLOOR that makes the page OVERFLOW and
#   scroll when the chrome is wider. bslib is explicitly designed to let the content
#   exceed the viewport rather than collapse. (576 is also Bootstrap's `sm` media
#   query, which is how I first "found" the number and misread it as a budget. A third
#   probe even read bslib 0.12.0 from the SYSTEM library instead of the project's;
#   the project pins 0.11.0.) So the honest statement is NOT "the app exceeds its
#   framework budget" — it is the narrow-viewport geometry measured below. The
#   original numbers are asserted in the DISPROVEN PREMISE block so the record of the
#   wrong turn lives in the repository instead of in a memory.
#   Nothing here weakens an assertion to obtain GREEN: the disproven block asserts
#   the DISPROOF, and the regression block asserts real measured geometry.
# ---------------------------------------------------------------------------
#
# THE LAW, SOURCED TO bslib's OWN STYLESHEET
#   bslib/components/scss/sidebar.scss:21, :46, :78
#       $bslib-sidebar-column-sidebar:
#           Min(calc(100% - var(--_padding-icon)), var(--_sidebar-width));
#       --_padding-icon: calc(var(--_icon-button-size, 2rem) * 1.5);   /* = 48px */
#       grid-template-columns: $bslib-sidebar-column-sidebar var(--_column-main);
#   =>   main = column - min(column - 48, sidebar_width)
#   When `sidebar_width >= column - 48` the minimum selects `column - 48` and
#   **`main` is exactly 48 px, whatever the width.** So "narrow the panel sidebar a
#   bit" cannot fix anything: 350 px and 320 px both leave 48 px, and every
#   `html-fill-item` below (a flex item with `min-width: 0` and no intrinsic width)
#   then collapses to 0.
#
# THE REGRESSION, IN ONE LINE — with the app's ORIGINAL module-level layout
#   At a 992 px viewport the module's own 340 px sidebar is reserved, so this panel's
#   column is 334 px, and the QC panel's own 350 px sidebar takes
#   `min(334 - 48, 350) = 286`:
#       main = 334 - 286 = 48 px   ->  content 0 px
#   MEASURED: `hotspot_table 0x0` with DT building NO table (`tables = 0`) and both
#   plots at height 0, while the server side was perfect (`n_results 1808`).
#   bslib's law says no width value fixes that, so the only single-argument lever is
#   THIS PANEL's own initial state: `open = "closed"`, which collapses the 286 px
#   track to 0 and hands the panel its whole 334 px. No server logic is involved.
#
# A SECOND EXPERIMENT, MEASURED, REJECTED, AND DELIBERATELY NOT PART OF THIS FIX
#   (2026-09-29) The same argument was first applied to the MODULE-level sidebar, on
#   the theory that its 340 px was redundant. It was implemented, falsified, measured
#   live — and it is a REGRESSION, not a fix, so it was reverted byte-exactly:
#       document.getElementById('spatial-steps')
#             .closest('.bslib-sidebar-layout')      <- unambiguous: the module's own
#                                                       accordion id, ns("steps") with
#                                                       ns = "spatial", unique in DOM
#         before click   : collapsed=true  .sidebar 0x0  .main 0x0  #spatial-steps 0x0
#         after  click 1 : IDENTICAL — the toggle EXISTS (title="Toggle sidebar") but
#                          MEASURES 0x0 and clicking it does not change `collapsed`
#         after  click 2 : IDENTICAL again; 16 accordion headers, first at 0x0
#   The workflow sidebar, permanently visible at 339x516 before, became UNREACHABLE
#   at 992 px, and the module's own `.main` measured 0x0: bslib collapses the whole
#   layout at this viewport, not just the sidebar track.
#   A NUMBER FROM THAT EXPERIMENT IS RETRACTED HERE: "module .main = 742 px" came from
#   the selector `.bslib-sidebar-layout > .main`, which matches the FIRST such element
#   in the document — the PAGE main, not the module's. Measured unambiguously, the
#   module's own main is 0x0. That is the THIRD ambiguous-selector defect of this
#   investigation, and the reason every probe in this file's evidence anchors on a
#   unique id and uses `.closest()`.
#   Consequence for this test: it pins the ORIGINAL module-level state and owns nothing
#   at that level. The QC panel is fixed on its own, with the app's own skeleton intact.
# =============================================================================

.BSLIB_GUTTER_PX <- 48L        # bslib sidebar.scss:46, --_padding-icon = 2rem * 1.5
# A usable content column. THE PRODUCT FLOOR, and the one judgement here: a DT table
# with a search box is not usable much below this. Named so the decision is visible.
.USABLE_MAIN_PX <- 200L
# The QC panel's column, MEASURED live at a 992x1323 viewport on 2026-09-29 with the
# app's ORIGINAL module-level layout (its 340 px sidebar open): 334 px.
# ⚠️ While the rejected module experiment was in place this constant read 226 px, and
# a second one read 742 px for the results column. Both are recorded above as
# MEASUREMENTS OF A CONFIGURATION THAT NO LONGER EXISTS, and neither is used here: this
# test now describes the app as shipped, plus one nested sidebar closed by default.
.MEASURED_QC_COLUMN_PX <- 334L

# --- read the REAL sources, so a test cannot drift from the product ------------
# The RAW reader, kept for bslib's own SCSS: those files use `//` line comments, so
# this file's `#`-stripping must NOT be applied to them. It accepts an ABSOLUTE path
# as well as a repo-relative one, because `system.file()` hands back an absolute path
# and joining that onto the project root silently yields a file that does not exist —
# measured as 5 phantom failures before this line existed.
.tsl_read <- function(rel) {
  p <- if (file.exists(rel)) rel else file.path(ts_project_root(), rel)
  if (!file.exists(p)) return(character(0))
  readLines(p, warn = FALSE)
}

# COMMENTS ARE STRIPPED BEFORE ANY SCAN of the product's R code, and that is not
# cosmetic. MEASURED: with `open = "closed"` removed from the production code, this
# test still read `"closed"` — out of the COMMENT that documents the argument, which
# sits INSIDE the `sidebar(` call at depth 1, so the falsification passed with the
# fix reverted. A test that a comment can satisfy is not a test. The idiom is the one
# the sibling drive tests already use, and a trailing comment is stripped too, so
# `width = 350, # the sidebar` cannot leak either.
.tsl_code <- function(rel) {
  lines <- .tsl_read(rel)
  if (!length(lines)) return(character(0))
  lines <- sub("#.*$", "", lines)                            # trailing comments
  grep("^[[:space:]]*[^#[:space:]]", lines, value = TRUE)    # drop comment-only lines
}

# bslib's shipped SCSS, located rather than hard-coded, so a bslib upgrade that
# changes the rule is visible here instead of silently invalidating the geometry.
.tsl_bslib_file <- function(nm) {
  hits <- list.files(system.file(package = "bslib"), pattern = paste0("^", nm, "$"),
                     recursive = TRUE, full.names = TRUE)
  hits[1]
}

# Every `width = <n>` passed DIRECTLY to a `sidebar()` call, in order.
#
# Two earlier scanner defects were measured and fixed, and both are the kind that
# report a product fact as a harness fact:
#   1. the trigger was `layout_sidebar(`, and `app.R` does not use that call — it
#      passes `sidebar = sidebar(`, so the page skeleton measured ZERO widths and
#      the test said "the constant is missing" about a constant that is right there
#      at `app.R:269`. The trigger is now the thing that actually CARRIES the width.
#   2. the walk counted braces only, so it stopped on the opening line of a call
#      that opens with `(` and never reached the `width =` one line below. It is
#      now paren-aware.
# A width is recorded only at nesting depth 1 — a direct argument of `sidebar()` — so
# a `width =` belonging to a nested widget cannot be mistaken for the layout's.
.tsl_sidebar_widths <- function(rel) {
  lines <- .tsl_code(rel); out <- integer(0); i <- 1L
  while (i <= length(lines)) {
    if (!grepl("sidebar\\s*=\\s*sidebar\\(", lines[i])) { i <- i + 1L; next }
    depth <- 0L
    repeat {
      ln <- lines[i]
      m <- regmatches(ln, regexpr("width\\s*=\\s*[0-9]+", ln))
      if (length(m) && depth == 1L) out <- c(out, as.integer(sub("^.*?([0-9]+).*$", "\\1", m)))
      depth <- depth + lengths(regmatches(ln, gregexpr("\\(", ln))) -
        lengths(regmatches(ln, gregexpr("\\)", ln)))
      if (depth <= 0L || i >= length(lines)) break
      i <- i + 1L
    }
    i <- i + 1L
  }
  out
}

# The `open = "..."` value declared for the FIRST `sidebar()` call of a file, or ""
# when the call does not declare one. Same discipline: comments stripped by
# `.tsl_code()`, paren-aware, depth 1.
.tsl_sidebar_open <- function(rel) {
  lines <- .tsl_code(rel); i <- 1L
  while (i <= length(lines)) {
    if (!grepl("sidebar\\s*=\\s*sidebar\\(", lines[i])) { i <- i + 1L; next }
    depth <- 0L
    repeat {
      ln <- lines[i]
      m <- regmatches(ln, regexpr("open\\s*=\\s*\"[a-z]+\"", ln))
      if (length(m) && depth == 1L) return(sub("^.*open\\s*=\\s*\"([a-z]+)\".*$", "\\1", m))
      depth <- depth + lengths(regmatches(ln, gregexpr("\\(", ln))) -
        lengths(regmatches(ln, gregexpr("\\)", ln)))
      if (depth <= 0L || i >= length(lines)) break
      i <- i + 1L
    }
    i <- i + 1L
  }
  ""
}

.tsl_panel_files <- function() {
  sort(list.files(file.path(ts_project_root(), "modules", "spatial"),
                  pattern = "^mod_.*[.]R$", full.names = FALSE))
}

# The bslib law as a FUNCTION of its inputs, so every assertion below is a claim
# about geometry rather than a memorised number.
.tsl_main_width <- function(column, sidebar) {
  column - min(column - .BSLIB_GUTTER_PX, sidebar)
}

test_that("the Spatial sidebar GEOMETRY is read from the real sources, not from the test", {
  # The two outer terms. If someone changes the page skeleton or the module's own
  # layout, this test must notice — otherwise the arithmetic below asserts a fiction.
  page <- .tsl_sidebar_widths(file.path("app.R"))
  expect_gt(length(page), 0L)
  expect_identical(as.integer(page[1]), 250L)

  mod <- .tsl_sidebar_widths(file.path("modules", "spatial", "mod_spatial.R"))
  expect_gt(length(mod), 0L)
  expect_identical(as.integer(mod[1]), 340L)
})

test_that("EVERY Spatial sub-panel sidebar is enumerated, and the worst is named", {
  # MEASURED live: the hotspot panel was the one REPORTED, but the geometry is
  # shared. A test that only looked at mod_spatial_qc.R would have called a
  # nine-panel problem a one-panel one — the mistake the handoff's item nearly became.
  panels <- .tsl_panel_files()
  widths <- lapply(panels, function(f) .tsl_sidebar_widths(file.path("modules", "spatial", f)))
  names(widths) <- panels
  declared <- widths[vapply(widths, length, integer(1)) > 0L]

  expect_gte(length(declared), 8L)
  expect_true("mod_spatial_qc.R" %in% names(declared))
  expect_identical(as.integer(declared[["mod_spatial_qc.R"]][1]), 350L)
  for (f in names(declared)) expect_gte(as.integer(declared[[f]][1]), 320L)
})

test_that("DISPROVEN PREMISE, KEPT AS A RECORD: 576px is a FLOOR, not a budget", {
  # The first version of this test asserted `576 >= 838` and called the app's Spatial
  # module a budget overrun. It was wrong, and this block asserts the DISPROOF so the
  # wrong turn stays in the repository. The original numbers are preserved verbatim,
  # because they are MEASUREMENTS — only the COMPARISON was invalid, and the error was
  # reading a `min-width` as a budget.
  sb_scss <- .tsl_bslib_file("page_sidebar.scss")
  expect_true(nzchar(sb_scss))
  sb <- paste(.tsl_read(sb_scss), collapse = "\n")

  expect_match(sb, "\\$bslib-page-main-min-width")
  expect_match(sb, "min-width:\\s*var\\(--bslib-page-main-min-width")

  page <- .tsl_sidebar_widths(file.path("app.R"))[1]
  mod <- .tsl_sidebar_widths(file.path("modules", "spatial", "mod_spatial.R"))[1]
  chrome <- as.integer(page) + as.integer(mod) + .BSLIB_GUTTER_PX
  need <- chrome + .USABLE_MAIN_PX
  expect_identical(chrome, 638L)                  # 250 + 340 + 48
  expect_identical(need, 838L)                    # + 200
  expect_identical(as.integer(need) - 576L, 262L)  # the "deficit" that never was one

  # The invalid inference, stated as a disproof: a `min-width` on the content is a
  # promise that the content will be AT LEAST that wide (and overflow), not a promise
  # that the chrome fits inside it. The two outer terms alone (590) already exceed
  # 576, so nothing was ever over budget.
  expect_gt(as.integer(page) + as.integer(mod), 576L)
})

test_that("bslib's ACTUAL behaviour: the grid law, and the 48px collapse it produces", {
  sb <- .tsl_bslib_file("sidebar.scss")
  expect_true(nzchar(sb))
  txt <- paste(.tsl_read(sb), collapse = "\n")
  expect_match(txt, "--_padding-icon:\\s*calc\\(var\\(--_icon-button-size,\\s*2rem\\)\\s*\\*\\s*1\\.5\\)")
  expect_match(txt, "Min\\(calc\\(100% - var\\(--_padding-icon\\)\\), var\\(--_sidebar-width\\)\\)")
  expect_match(txt, "grid-template-columns:\\s*\\$bslib-sidebar-column-sidebar")

  # The consequence, computed rather than asserted from memory: whenever the sidebar
  # asks for at least `column - 48`, main is EXACTLY 48 px, whatever the width. That
  # is why narrowing a panel cannot fix this.
  expect_identical(.tsl_main_width(334L, 350L), 48L)
  expect_identical(.tsl_main_width(334L, 320L), 48L)   # viz, 320: the same 48 px
  expect_identical(.tsl_main_width(334L, 380L), 48L)   # lr / niche / report
  # the same panel in the page main's 742 px (that page width is a real measurement of
  # the PAGE main, and is unrelated to the retracted "module main = 742" claim above)
  expect_identical(.tsl_main_width(742L, 350L), 392L)
})

test_that("the module-level sidebar stays as shipped: closing it was measured and rejected", {
  # NOT a geometric claim. This block encodes a PRODUCT RULING of 2026-09-29: the
  # module-level `open = "closed"` experiment was implemented, falsified, measured live,
  # and REJECTED because it made the workflow accordion unreachable at 992 px (its
  # toggle measured 0x0 and two clicks changed nothing) while the module's own `.main`
  # measured 0x0. The evidence is in the header of this file; it is kept here rather
  # than in a scratch note so the measurement cannot be quietly forgotten.
  # The assertion is deliberately minimal and asserts ONE thing: the module sidebar is
  # not collapsed by default. It is not a design opinion about the 340 px skeleton —
  # the app's own layout — and it does not claim any width is right or wrong. A future
  # decision to revisit that skeleton is a product decision, and this line is the
  # reminder that the obvious variant was already tried and measured.
  mod_file <- file.path("modules", "spatial", "mod_spatial.R")
  declared <- .tsl_sidebar_open(mod_file)

  expect_false(identical(declared, "closed"),
               info = paste("the module-level sidebar must keep bslib's default (open):",
                            "the closed-by-default variant was measured to leave the",
                            "workflow toggle at 0x0 and the module main at 0x0 at 992 px"))
  # The panel that WAS fixed is a different, nested one — the distinction the two
  # experiments turn on, and the one a merged change would have hidden.
  expect_true("mod_spatial_qc.R" %in% .tsl_panel_files())
})

test_that("RED: the QC panel's OWN sidebar starves its plots, and one lever clears it", {
  # THE REGRESSION, and it is deliberately QC-ONLY: the app's module-level skeleton
  # and the eight other sub-panels are NOT touched. With the module sidebar as
  # shipped, this panel's column is 334 px (MEASURED, .MEASURED_QC_COLUMN_PX) and its
  # own `sidebar(width = 350)` takes `min(334 - 48, 350) = 286`, leaving main 48 px.
  # The bslib law says NO width value fixes that — any sidebar at or above
  # `column - 48` leaves exactly 48 px — so the only single-argument lever is this
  # panel's own initial state: `open = "closed"` on its `sidebar()` call, which
  # collapses the 286 px track to 0, hands the panel its whole 334 px, and keeps the
  # QC controls (title, thresholds, sliders, the hotspot button) one click away on
  # bslib's own toggle. No CSS is forced and no `!important` is added anywhere.
  # ⚠️ This does NOT promise the plots become readable: 334 px split 7/5 by
  # `layout_columns()` is ~191 px and ~137 px, and the measured card heights were
  # 120 px. The live measurement owns that verdict; what this test owns is the
  # STRUCTURE (the starved main) and the single lever.
  qc_file <- file.path("modules", "spatial", "mod_spatial_qc.R")
  declared <- .tsl_sidebar_open(qc_file)
  qc_sidebar <- .tsl_sidebar_widths(qc_file)[1]
  column <- .MEASURED_QC_COLUMN_PX

  # 1. the declaration, read from CODE (comments are stripped, see .tsl_code)
  expect_identical(declared, "closed",
                   info = "the QC panel sidebar must declare open = \"closed\"")

  # 2. the starved main it produces WITHOUT the lever, asserted so the fix cannot be a
  #    no-op, and computed from the product's OWN declared width rather than a copy
  main_open <- .tsl_main_width(column, as.integer(qc_sidebar))
  expect_identical(main_open, 48L)

  # 3. the consequence of the lever, from the same measured column
  main_closed <- if (identical(declared, "closed")) column else main_open
  expect_true(main_closed >= .USABLE_MAIN_PX,
              info = sprintf("QC panel main is %d px (column %d, sidebar %d declared open = \"%s\"); the usable floor is %d px",
                            main_closed, column, as.integer(qc_sidebar),
                            if (nzchar(declared)) declared else "none", .USABLE_MAIN_PX))
})

# =============================================================================
# SLICE 3 — the hotspot grid's VERTICAL contract and its RESPONSIVE breakpoint
# =============================================================================
# WHY, MEASURED (all values read from the running app on 2026-09-29, viewport height
# 1323, the app's own 340 px module sidebar OPEN, `steps.hotspots = ran`,
# `n_results = 1808`):
#   * VERTICAL. bslib's `card()` has `fill = TRUE` by DEFAULT and ends with
#     `bindFillRole(tag, container = TRUE, item = fill)`, so the card, its body and the
#     `plotOutput()` are all `html-fill-item`; the governing declaration, matched in the
#     browser, is
#         .html-fill-container > .html-fill-item { flex: 1 1 auto; min-height: 0px; }
#     `layout_columns()` renders ONE `auto` row, and an `auto` row cannot derive a
#     height from a subtree whose every member may shrink to 0, so the row measured
#     11.7031 px at a 992 px viewport: card1 104x12, plot1 40x0, the card body an
#     `overflow: auto` window 32 px tall over 256 px of plot, and the plot's own
#     inline `height: 520px` demoted to advisory. Setting the row to a CONTENT height
#     restores the declared 520 px with no card-body scrollbar at 1440, 992 and 768.
#   * HORIZONTAL. `bslib-layout-columns` is a 12-track `minmax(0, 1fr)` grid with
#     `gap: 1rem`, so ELEVEN 16 px gaps consume 176 px before any content gets a pixel,
#     and each card additionally carries (span - 1) internal gaps of dead space. Both
#     facts are in the measured table below.
# =============================================================================

# MEASURED grid width and the two plot widths, viewport width -> value. Read from the
# live DOM, anchored on `#spatial-qc-hotspot_map` and `.closest('.bslib-grid')`.
.MEASURED_GRIDW_PX  <- c(`1280` = 477, `1360` = 557, `1400` = 597, `1440` = 637, `1920` = 1117)
.MEASURED_PLOT2_PX  <- c(`1280` = 140, `1360` = 189, `1400` = 205, `1440` = 222, `1920` = 422)
# The PRODUCT FLOOR for the narrower of the two plots (the Gi* distribution, which needs
# readable tick labels). Named so the judgement is visible rather than buried in a
# breakpoint. 190 px is where the axis labels stop being legible.
.HOTSPOT_MIN_PLOT_PX <- 190L
# The chosen stacking threshold, DERIVED from the two tables above and asserted as such
# in the block below: the narrowest viewport whose narrower plot still clears the floor.
.HOTSPOT_STACK_BREAKPOINT_PX <- 1400L
# bslib's grid constants, measured in the browser: `gap: var(--bslib-spacer, 1rem)`
# resolved to 16 px over 12 tracks.
.BSLIB_GRID_TRACKS <- 12L
.BSLIB_GRID_GAP_PX  <- 16L

# A call may open on one line and close twenty lines later, so a per-line regex would
# silently truncate it. This is the paren-balanced idiom already used by
# `.tsl_sidebar_open()`, generalised and returning the TEXT.
#
# The paren COUNT ignores string literals, and that is not a refinement. The first
# version counted every "(" and ")" on the line, including the ones inside the CSS
# payload and inside the explanatory comments, and a comment containing "2695 entries)."
# — one closing paren with no opener — drove the running depth negative and ended the
# capture mid-rule, so the test then reported a MISSING min-height on a stylesheet that
# contained it. The text returned is the ORIGINAL line: only the COUNT uses the blanked
# view, so the captured CSS still carries its literals.
.tsl_code_view <- function(lines) gsub('"[^"]*"', '""', lines)

.tsl_call_from <- function(lines, i) {
  view <- .tsl_code_view(lines)
  depth <- 0L; started <- FALSE; out <- character(0)
  repeat {
    if (i > length(lines)) break
    out <- c(out, lines[i])
    depth <- depth + lengths(regmatches(view[i], gregexpr("\\(", view[i]))) -
      lengths(regmatches(view[i], gregexpr("\\)", view[i])))
    if (depth > 0L) started <- TRUE
    if (started && depth <= 0L) break
    i <- i + 1L
  }
  out
}

# The `layout_columns()` call of the HOTSPOTS panel — located from the panel's own
# `value = "hotspots"` marker, not from the first match in the file, because the QC
# module contains other grids (a second `layout_columns()` lives in the server, and the
# Moran's I panel has its own). The search runs on the blanked view, because a COMMENT
# that mentions `layout_columns()` used to win the search and the extractor then returned
# the comment instead of the call.
.tsl_hotspot_grid_call <- function(rel) {
  lines <- .tsl_code(rel)
  view <- .tsl_code_view(lines)
  start <- grep('value = "hotspots"', lines)
  if (!length(start)) return(character(0))
  i <- start[1]
  while (i <= length(view) && !grepl("layout_columns\\s*\\(", view[i])) i <- i + 1L
  if (i > length(view)) return(character(0))
  .tsl_call_from(lines, i)
}

# The CSS shipped with the hotspot panel, taken from its `tags$style(HTML(...))` call.
# Two things this extractor must NOT do, both learned the hard way:
#   * it returns the string LITERALS inside the call, one per element, because `HTML()`
#     concatenates its arguments — the literals ARE the stylesheet. Returning the call
#     text instead handed `tags$style(HTML(` to the selector parser, which reported a
#     scoping failure that did not exist;
#   * it reads the RAW file, never `.tsl_code()`. `.tsl_code()` strips `#` to end of line
#     to remove R comments, and inside a CSS payload a `#` is an ID SELECTOR: the line
#     `"  #spatial-qc-hotspot_table {"` came back as `"  `, i.e. the selector was deleted
#     before any assertion could see it, and the test then reported a missing rule on a
#     stylesheet that contained it. An instrument that silently removes the thing under
#     test is worse than no instrument.
.tsl_hotspot_css <- function(rel) {
  raw <- .tsl_read(rel)
  hit <- grep("tags\\$style", raw)
  if (!length(hit)) return(character(0))
  # `collapse` is REQUIRED here: `regmatches()` on a character VECTOR returns one element
  # per line, so without it `[[1]]` saw only the opening `tags$style(HTML(` line, found no
  # quotes in it, and the extractor returned nothing at all — reported as "no
  # tags$style(HTML(...)) found in the QC module" for a module that plainly has one.
  call <- paste(.tsl_call_from(raw, hit[1]), collapse = "\n")
  lits <- regmatches(call, gregexpr('"[^"]*"', call))[[1]]
  if (!length(lits)) return(character(0))
  sub('^"(.*)"$', "\\1", lits)
}

# The number of CLASS selectors in a selector string, which is what decides the
# winner among the rules that compete for `grid-column` here. `:where()` contributes
# ZERO by definition, which is exactly why bslib's own stacking rule loses.
.tsl_class_count <- function(sel) {
  sel <- gsub("::?[a-zA-Z-]+\\([^)]*\\)", "", sel)          # drop pseudo-classes
  sel <- gsub("[>+~][[:space:]]*[a-zA-Z][a-zA-Z0-9-]*", "", sel)  # drop element names
  length(gregexpr("\\.[A-Za-z_][A-Za-z0-9_-]*", sel)[[1]][gregexpr("\\.[A-Za-z_][A-Za-z0-9_-]*", sel)[[1]] > 0])
}

test_that("the hotspot grid declares a CONTENT row height, and none of the rejected levers", {
  # SLICE 3, vertical half. `row_heights` is a bslib 0.11.0 argument of
  # `layout_columns()`; it feeds `--bslib-grid--row-heights`, i.e. `grid-auto-rows`,
  # which is the very declaration the collapsed `auto` row was resolving against.
  # `row_heights_css_vars()` passes a string through untouched, so "min-content" is a
  # supported value and is preferred over a hard-coded pixel: the card header's height
  # depends on the translated label, and a fixed row would clip it the moment the label
  # wrapped.
  grid_call <- paste(.tsl_hotspot_grid_call(file.path("modules", "spatial", "mod_spatial_qc.R")),
                     collapse = "\n")

  expect_true(grepl("row_heights\\s*=", grid_call),
              info = "the hotspot grid must declare row_heights so the auto row can size to its content")
  expect_match(grid_call, "row_heights\\s*=\\s*\"min-content\"")

  # The three levers this slice was told NOT to use, asserted so a later change cannot
  # reintroduce them by accident: a fixed grid height, gap = 0 as the primary fix, and
  # opting the cards out of the fill contract.
  expect_false(grepl("(^|[,(\\s])height\\s*=", grid_call),
               info = "no fixed grid height: it forces a box and reintroduces the scrollbar")
  expect_false(grepl("gap\\s*=\\s*0", grid_call),
               info = "gap = 0 is not the fix: it is horizontal-only and measured 0 px wide at 768")
  expect_false(grepl("fill\\s*=\\s*FALSE", grid_call),
               info = "fill = FALSE is not the first solution: the tab-pane above is a fill container too")
})

test_that("the hotspot grid carries a scoping class, so the responsive rule cannot leak", {
  # A responsive rule needs a handle. `class =` is bslib's supported way to put one on
  # the grid, and the rule below is scoped to it — the alternative, a bare
  # `.bslib-grid` selector, would be the global override this slice forbids.
  grid_call <- paste(.tsl_hotspot_grid_call(file.path("modules", "spatial", "mod_spatial_qc.R")),
                     collapse = "\n")
  expect_match(grid_call, "class\\s*=\\s*\"ts-qc-hotspot-grid\"",
               info = "the hotspot grid must carry the ts-qc-hotspot-grid scoping class")
})

test_that("the scoped responsive rule stacks the cards below the MEASURED breakpoint", {
  css <- .tsl_hotspot_css(file.path("modules", "spatial", "mod_spatial_qc.R"))
  expect_true(nzchar(paste(css, collapse = "")), info = "no tags$style(HTML(...)) found in the QC module")

  flat <- paste(css, collapse = "\n")
  # CSS COMMENTS ARE STRIPPED before any rule-level assertion, and that is not
  # cosmetic: the rule's own comment explains WHY it needs no `!important`, so
  # grepping the raw text for that token reported a failure against a rule that does
  # not use it. `.tsl_code()` strips `#` comments from the R source but not the `/* */`
  # comments inside the CSS string, so the strip happens here. `(?s)` is REQUIRED and
  # not optional: the comment is written as one R string per line, so `/*` and `*/` sit
  # on different lines of the source and a `.` that stops at the newline never reaches
  # the closing marker — the strip silently did nothing and the token survived.
  flat_nc <- gsub("(?s)/\\*.*?\\*/", "", flat, perl = TRUE)
  flat_nc <- gsub("[[:space:]]+", " ", flat_nc)

  # 1. it is a max-width media query, and its threshold is the chosen breakpoint
  expect_match(flat_nc, "@media[^{]*max-width:\\s*1399\\.98px")
  bp <- as.numeric(sub(".*max-width:\\s*([0-9.]+)px.*", "\\1", flat_nc))
  expect_identical(bp, .HOTSPOT_STACK_BREAKPOINT_PX - 0.02)

  # 2. it stacks by spanning the full row, which is what bslib itself does below 768
  expect_match(flat_nc, "grid-column:\\s*1\\s*/\\s*-1")

  # 3. it is SCOPED: every selector in the rule names the scoping class, so it cannot
  #    touch any other grid in the app. The selectors are read with a capture of the
  #    text immediately preceding each `{`, because a `sub("^.*\\{", …)` is GREEDY and
  #    would swallow the whole file up to the last brace, at-rules and comments
  #    included — the first version of this assertion read the comment block as a
  #    selector and failed for the wrong reason.
  raw <- regmatches(flat_nc, gregexpr("[^{}]+\\{", flat_nc))[[1]]
  selectors <- trimws(sub("\\{+$", "", raw))
  selectors <- selectors[nzchar(selectors) & !grepl("^@", selectors)]
  expect_true(length(selectors) > 0L,
              info = "no selector could be read out of the rule: the scoping assertion would be vacuous")
  # Every selector must name ONE OF THIS PANEL'S TWO HANDLES: the grid's scoping class,
  # or the hotspot table's own element id. Both are narrow — one grid, one table — and
  # neither can reach another module. The list has two entries because the panel ships
  # two rules (the grid's stacking, the table's height floor), and a loop that demanded
  # the grid class in every selector would forbid fixing the table at all.
  for (s in selectors) {
    expect_match(s, "ts-qc-hotspot-grid|spatial-qc-hotspot_table",
                 info = paste0("selector '", s, "' is scoped to neither the hotspot grid nor the hotspot table"))
  }
  # and the grid rule specifically must use the CLASS, so it keeps working if the
  # module is ever mounted under another id
  expect_true(any(grepl("ts-qc-hotspot-grid", selectors) & grepl("bslib-grid-item", selectors)),
              info = "the stacking rule must address the grid items through the scoping class")

  # 4. NO !important anywhere, and no global element/id selector. This is the assertion
  #    that keeps the slice honest: the span is NOT set inline (measured: the item has
  #    no style attribute at all), so a plain, more specific rule is enough.
  expect_false(grepl("!important", flat_nc, fixed = TRUE),
               info = "the rule must beat the span on specificity, not on !important")
  expect_false(grepl("(^|[^A-Za-z0-9_-])[a-z]+\\s*\\{", flat_nc),
               info = "no bare element selector: that would be a global override")
  # An id IS allowed, for one reason only: the table's floor has to outrank
  # `.html-fill-container > .html-fill-item { min-height: 0 }`, which is (0,2,0), and a
  # class-scoped rule cannot beat that without `!important`. The id is therefore required
  # to BE the hotspot table's — asserted here, and composed from the two source files by
  # the block below, so it cannot rot into a guess.
  ids_here <- regmatches(flat_nc, gregexpr("#[A-Za-z0-9_-]+", flat_nc))[[1]]
  for (i in ids_here) {
    expect_match(i, "hotspot_table",
                 info = paste0("id selector '", i, "' does not address the hotspot table"))
  }
})

test_that("the scoped rule really outranks the rule that sets the span (specificity, measured)", {
  # The precedence claim is ASSERTED, not assumed, and it is anchored to the CSS bslib
  # and Bootstrap actually ship in this project: the rule that wins in the browser is
  # `@media (min-width: 576px) .grid .g-col-sm-7 { grid-column: auto / span 7 }`, TWO
  # class selectors. bslib's own stacking rule is
  # `@media (max-width: 767.98px) bslib-layout-columns:where(.bslib-grid) > * { … }`,
  # whose `:where()` contributes ZERO, so it is (0,1,1) and LOSES to (0,2,0) — which
  # is the measured reason the cards never stacked between 576 and 992 px.
  shipped <- paste(.tsl_read(.tsl_bslib_file("components.css")), collapse = "\n")
  # MEASURED trap: bslib ships SEVERAL `bootstrap.min.css` (a `css-precompiled/3/…`
  # Bootstrap 3 build first, then 4 and 5), and only the Bootstrap 5 one carries the
  # `g-col-*` utilities. Reading the FIRST match silently asserts against the wrong
  # framework version, so every build is searched and the hit is named.
  boots <- list.files(file.path(dirname(dirname(.tsl_bslib_file("bootstrap.min.css"))), ".."),
                      pattern = "^bootstrap.*[.]min[.]css$", recursive = TRUE, full.names = TRUE)
  if (!length(boots)) {
    boots <- list.files(sub("css-precompiled/.*$", "", .tsl_bslib_file("bootstrap.min.css")),
                        pattern = "^bootstrap.*[.]min[.]css$", recursive = TRUE, full.names = TRUE)
  }
  carriers <- boots[vapply(boots, function(f) {
    txt <- paste(.tsl_read(f), collapse = "\n")
    grepl("g-col-sm-7", txt, fixed = TRUE) && grepl("grid-column", txt, fixed = TRUE)
  }, logical(1))]
  expect_true(length(carriers) > 0L,
              info = "no shipped bootstrap build pairs g-col-sm-7 with grid-column: the specificity claim would be unfounded")
  boot <- paste(unlist(lapply(carriers, function(f) .tsl_read(f))), collapse = "\n")
  blob <- paste(shipped, boot, collapse = "\n")

  # the competing rule exists as described, read from the installed packages. The
  # selector is matched WITHOUT its surrounding whitespace on purpose: the assertion is
  # about WHICH rule wins, and pinning the compiled CSS's exact spacing would make this
  # a formatting test that a bslib recompile could break for no product reason.
  expect_match(blob, "\\.grid \\.g-col-sm-7")
  expect_match(blob, "bslib-layout-columns:where\\(\\.bslib-grid\\)")

  # and our rule has strictly more class selectors than the rule it must beat
  ours <- ".bslib-grid.ts-qc-hotspot-grid > .bslib-grid-item"
  theirs <- ".grid .g-col-sm-7"
  expect_true(.tsl_class_count(ours) > .tsl_class_count(theirs),
              info = "the scoped rule must outrank .grid .g-col-sm-7 on class count alone")
  expect_identical(.tsl_class_count(ours), 3L)
  expect_identical(.tsl_class_count(theirs), 2L)
  # `:where()` contributes ZERO by definition, which is precisely why bslib's own
  # stacking rule (an element selector plus a zero-weight `:where()`) loses to a
  # two-class rule and never stacked the cards between 576 and 992 px.
  expect_identical(.tsl_class_count("bslib-layout-columns:where(.bslib-grid) > *"), 0L)
})

test_that("the breakpoint is DERIVED from the measurements, not asserted by faith", {
  # Two measured tables, and the breakpoint has to be consistent with both:
  #   * the narrower plot must still clear the product floor at the narrowest
  #     side-by-side width;
  #   * one step below it, the narrower plot is already marginal, so the threshold is
  #     not lower than that.
  for (w in names(.MEASURED_PLOT2_PX)) {
    expect_gt(as.integer(.MEASURED_PLOT2_PX[[w]]), 0L)
    expect_gt(as.integer(.MEASURED_GRIDW_PX[[w]]), 0L)
  }
  # 176 px of the grid is gap before any track gets a pixel: (12 - 1) * 16.
  expect_identical((.BSLIB_GRID_TRACKS - 1L) * .BSLIB_GRID_GAP_PX, 176L)
  expect_true(all(.MEASURED_GRIDW_PX > 176L),
              info = "a 12-track grid needs more than its 176 px of gap to give any track a pixel")

  bp <- as.character(.HOTSPOT_STACK_BREAKPOINT_PX)
  expect_true(bp %in% names(.MEASURED_PLOT2_PX),
              info = "the breakpoint must be one of the MEASURED viewport widths")
  expect_true(as.integer(.MEASURED_PLOT2_PX[[bp]]) >= .HOTSPOT_MIN_PLOT_PX,
              info = sprintf("at %s px the narrower plot is %d px, below the %d px floor",
                             bp, as.integer(.MEASURED_PLOT2_PX[[bp]]), .HOTSPOT_MIN_PLOT_PX))
  # the width just below it must NOT clear the floor, or the threshold is too high.
  # MEASURED trap, hit while writing this: subsetting a NAMED vector with
  # `as.integer(names(v))[as.integer(names(v)) < k]` returns the VALUES (1280, 1360),
  # not the positions, and `v[[1280]]` is then an index out of bounds. Subset the names.
  below <- names(.MEASURED_PLOT2_PX)[as.integer(names(.MEASURED_PLOT2_PX)) <
                                    .HOTSPOT_STACK_BREAKPOINT_PX]
  expect_true(length(below) > 0L)
  for (w in below) {
    expect_true(as.integer(.MEASURED_PLOT2_PX[[w]]) < .HOTSPOT_MIN_PLOT_PX,
                info = sprintf("at %s px the narrower plot is %d px, which already clears the floor: the breakpoint is too high",
                               w, as.integer(.MEASURED_PLOT2_PX[[w]])))
  }
})

test_that("stacking is never WORSE than side-by-side, and the panel keeps its parts", {
  # The point of the breakpoint: below it, one full-width card beats two half-width
  # cards. In a 12-track grid a card spans 12t + 11 gaps = the whole grid width, so a
  # stacked card is `gridW` wide against a side-by-side `span * t + (span - 1) * gap`.
  for (w in names(.MEASURED_GRIDW_PX)) {
    gw <- as.integer(.MEASURED_GRIDW_PX[[w]])
    t <- (gw - (.BSLIB_GRID_TRACKS - 1L) * .BSLIB_GRID_GAP_PX) / .BSLIB_GRID_TRACKS
    side_by_side_5 <- 5 * t + 4 * .BSLIB_GRID_GAP_PX      # the narrower card, col_widths = c(7, 5)
    stacked <- gw
    expect_true(stacked >= side_by_side_5,
                info = sprintf("at %s px stacking must not narrow the card: %d vs %d",
                               w, stacked, side_by_side_5))
  }

  # the panel still has BOTH cards, the table and the export control
  panel <- paste(.tsl_call_from(.tsl_code(file.path("modules", "spatial", "mod_spatial_qc.R")),
                                grep('value = "hotspots"',
                                     .tsl_code(file.path("modules", "spatial", "mod_spatial_qc.R")))[1]),
                 collapse = "\n")
  expect_true(grepl("hotspot_map", panel, fixed = TRUE))
  expect_true(grepl("hotspot_hist", panel, fixed = TRUE))
  expect_match(panel, "DT::DTOutput\\(ns\\(\"hotspot_table\"\\)\\)")
  expect_match(panel, "downloadButton\\(ns\\(\"dl_hotspot_csv\"\\)")
  # both plots keep their declared height: it is the value the row now honours
  expect_match(panel, "plotOutput\\(ns\\(\"hotspot_map\"\\), height = \"520px\"\\)")
  expect_match(panel, "plotOutput\\(ns\\(\"hotspot_hist\"\\), height = \"520px\"\\)")
})

# =============================================================================
# SLICE 4 — the hotspot TABLE, which the grid's `row_heights` cannot reach
# =============================================================================
# WHY, MEASURED (live, 2026-09-29, `steps.hotspots = ran`, `n_results = 1808`; the DT
# instance is complete at every width: 2 tables, 15 rows, "Showing 1 to 15 of 2,695
# entries", and the CSV export control is enabled):
#   viewport -> hotspot table height, with SLICE 3 in place
#       992 ->     5 px   <- the defect: built, but 5 px tall
#      1360 ->   163 px   <- still collapsed
#      1400 ->   263 px   <- usable, and the widest case that is NOT stacked
#      1920 ->   318 px   <- usable
#   Its natural height, measured with the fill role removed, is 1277 px.
#   The cause is NOT the grid: the table is a SIBLING of `layout_columns()`, inside the
#   navset card's `card-body`, and it is an `html-fill-item` with
#   `flex: 1 1 400px; min-height: 0` (measured computed style), so in a 319 px column it
#   shrinks to 5 px. Its own inline style is `width: 100%; height: auto`, which means a
#   stylesheet CANNOT set `height` (inline wins) and only `min-height` is available.
#
# THE THREE OPTIONS, COMPARED BY MEASUREMENT, NOT BY REASONING
#   C. `DT::DTOutput(ns("hotspot_table"), height = "300px")` — REFUTED. Measured 189x5 at
#      992, unchanged, and 272 at 1440: the request does not survive `flex-shrink`, so it
#      changes nothing at either width.
#   B. `DT::DTOutput(ns("hotspot_table"), fill = FALSE)` — REFUTED by the requirement it
#      breaks. It does fix the height (measured 189x1277 at 992, no v-scroll) but it also
#      removes DT's internal scrolling at wide widths: at 1440 the table went from
#      637x272 WITH V-SCROLL(1054>272) to 637x1056 with NO v-scroll, i.e. the page scroll
#      replaced the table scroll.
#   A. a `min-height` floor on this one table, inside the SAME narrow-only media query.
#      Kept: it leaves the wide widths untouched BY CONSTRUCTION (the query), keeps DT's
#      own scrolling inside the floor, and needs no `!important` because an id selector
#      outranks the (0,2,0) fill rule on specificity alone.
# =============================================================================

# The table's MEASURED height per viewport width, and its natural height.
.MEASURED_TABLE_H_PX <- c(`992` = 5, `1360` = 163, `1400` = 263, `1920` = 318)
.MEASURED_TABLE_NATURAL_PX <- 1277L
# The floor. NOT arbitrary: it sits INSIDE the range the app already produces at wide
# widths (263 at 1400, 318 at 1920), so the narrow case is made to look like the wide
# case instead of being given a number nobody has ever seen. Named so the judgement is
# visible, and asserted against the measurements in the block below.
.HOTSPOT_TABLE_FLOOR_PX <- 300L

test_that("the hotspot table is floored ONLY below the breakpoint, by a measured value", {
  css <- .tsl_hotspot_css(file.path("modules", "spatial", "mod_spatial_qc.R"))
  flat <- gsub("(?s)/\\*.*?\\*/", "", paste(css, collapse = "\n"), perl = TRUE)
  flat <- gsub("[[:space:]]+", " ", flat)

  # the floor exists, and it is a min-height (the only property available: the output's
  # own inline style is `height: auto`, which no stylesheet can override without
  # !important — and !important is forbidden here)
  expect_match(flat, "min-height:\\s*300px",
               info = sprintf("the hotspot table needs a min-height floor of %d px below the breakpoint",
                              .HOTSPOT_TABLE_FLOOR_PX))

  # it lives INSIDE the narrow-only media query, so the wide widths cannot be affected.
  # Asserted structurally: the min-height must appear after the query's opening brace and
  # before its closing one.
  q <- regexpr("@media[^{]*max-width:\\s*1399\\.98px[^{]*\\{", flat)
  expect_true(q > 0L, info = "the narrow-only media query is missing")
  tail_after <- substring(flat, q)
  expect_match(tail_after, "min-height:\\s*300px",
               info = "the table floor must be INSIDE the narrow-only media query, or the wide widths change")
  # and it must not also appear outside: count the declarations, expect exactly one
  expect_identical(length(gregexpr("min-height", flat, fixed = TRUE)[[1]]), 1L)

  # the floor clears the measured defect and stays under the natural height, so DT still
  # paginates and scrolls instead of being handed a box taller than its content
  expect_true(.HOTSPOT_TABLE_FLOOR_PX > as.integer(.MEASURED_TABLE_H_PX[["992"]]),
              info = sprintf("at 992 the table measures %d px, so a %d px floor is not a fix",
                             as.integer(.MEASURED_TABLE_H_PX[["992"]]), .HOTSPOT_TABLE_FLOOR_PX))
  expect_true(.HOTSPOT_TABLE_FLOOR_PX <= .MEASURED_TABLE_NATURAL_PX,
              info = "the floor must stay under the natural height or DT stops scrolling internally")
  # and it is inside the range the app already produces at wide widths
  expect_true(.HOTSPOT_TABLE_FLOOR_PX >= min(as.integer(.MEASURED_TABLE_H_PX[["1400"]]),
                                             as.integer(.MEASURED_TABLE_H_PX[["1920"]])))
  expect_true(.HOTSPOT_TABLE_FLOOR_PX <= max(as.integer(.MEASURED_TABLE_H_PX[["1400"]]),
                                             as.integer(.MEASURED_TABLE_H_PX[["1920"]])))
})

test_that("the table floor addresses the id the module ACTUALLY produces, not a guessed one", {
  # The selector is an id, and the id is built across TWO files: the Spatial module mounts
  # the QC module as `mod_spatial_qc_ui(ns("qc"))` and the QC module emits
  # `DT::DTOutput(ns("hotspot_table"))`, which the browser measured as
  # `spatial-qc-hotspot_table`. A hard-coded id that nobody checks is a rule that silently
  # stops applying the day a module is renamed, so the id is COMPOSED from the sources
  # here and compared with the CSS.
  spatial <- paste(.tsl_code(file.path("modules", "spatial", "mod_spatial.R")), collapse = "\n")
  expect_match(spatial, "mod_spatial_qc_ui\\(ns\\(\"[a-zA-Z0-9_-]+\"\\)\\)",
               info = "the QC module must be mounted with an ns() id, or the composed id is meaningless")
  inner <- sub(".*mod_spatial_qc_ui\\(ns\\(\"([a-zA-Z0-9_-]+)\"\\)\\).*", "\\1", spatial)
  outer <- sub("^([a-zA-Z0-9_-]*)-.*$", "\\1", sub(".*ns\\(\"([a-zA-Z0-9_-]+)\"\\).*", "\\1",
              sub("^(.*)mod_spatial_ui.*$", "\\1", spatial)))
  # `outer` is the Spatial module's own id as passed by app.R; read it rather than guess
  app_lines <- .tsl_code(file.path("app.R"))
  sp_call <- grep("mod_spatial_ui\\(", app_lines)
  expect_true(length(sp_call) > 0L, info = "app.R must mount the Spatial module for the id chain to close")
  outer_id <- sub(".*mod_spatial_ui\\(\"?([a-zA-Z0-9_-]+)\"?.*", "\\1", app_lines[sp_call[1]])
  expected <- paste0(outer_id, "-", inner, "-hotspot_table")

  css <- .tsl_hotspot_css(file.path("modules", "spatial", "mod_spatial_qc.R"))
  flat <- paste(css, collapse = "\n")
  ids <- regmatches(flat, gregexpr("#[A-Za-z0-9_-]+", flat))[[1]]
  expect_true(length(ids) > 0L, info = "the table rule must address the table by its element id")
  for (id in ids) {
    expect_identical(sub("^#", "", id), expected,
                     info = paste0("CSS id '", id, "' is not the id the module produces (", expected, ")"))
  }
})

test_that("the DT instance, the export control and the wide behaviour are untouched", {
  # The floor must not be bought by reconfiguring DT: `page_length = 15L` is what makes
  # the measured "Showing 1 to 15 of 2,695 entries", and the fill contract is what gives
  # the table its internal scrolling at wide widths. Option B was rejected precisely
  # because it took the fill role away, so its absence is asserted here.
  qc <- paste(.tsl_code(file.path("modules", "spatial", "mod_spatial_qc.R")), collapse = "\n")
  expect_match(qc, "page_length\\s*=\\s*15L")
  expect_match(qc, "DT::DTOutput\\(ns\\(\"hotspot_table\"\\)\\)",
               info = "the output must keep bslib's fill role: fill = FALSE was measured to remove DT's internal scrolling")
  expect_false(grepl("DT::DTOutput\\(ns\\(\"hotspot_table\"\\)[^)]*(fill|height)\\s*=", qc),
               info = "no height/fill argument on the output: option C was measured to do nothing and option B to break the wide case")
  expect_match(qc, "downloadButton\\(ns\\(\"dl_hotspot_csv\"\\)")
})

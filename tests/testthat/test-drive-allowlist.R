# =============================================================================
# test-drive-allowlist.R — Live Control Protocol, the FROZEN allowlist (S3/S4)
# =============================================================================
# Eponymous test for `R/core/drive_allowlist.R` (rule 5: every R/ file ships
# with its own test file).
#
# WHY THIS FILE EXISTS SEPARATELY — measured, not stylistic. The C9 guard in
# `tools/check_conventions.R` builds its candidate names from the BASENAME of
# the R/ file and the DOMAIN of its folder, and then asks only
# `file.exists()`. It therefore checks the NAME, never the CONTENT. Two
# consequences were measured on 2026-09-21:
#
#   1. `R/core/drive_watcher.R` was NOT reported by C9 at all — it was
#      "covered" by `test-drive-watcher.R` purely by string resemblance
#      (base `drive-watcher` vs base `drive_watcher`, `_` -> `-`).
#   2. `R/core/drive_allowlist.R` WAS reported, even though most of its
#      assertions lived inside that same `test-drive-watcher.R`. The guard
#      could not see them: it never opens the file.
#
# That is the C9 blind spot C9b exists to pierce (existence != coverage). The
# honest fix is not a rename of an unrelated file: it is to give the allowlist
# the eponymous file the rule asks for, and to let `test-drive-watcher.R` keep
# only what is genuinely the WATCHER's business. No assertion was deleted in
# the move; the sections were relocated verbatim.
#
# Spec: docs/DRIVE_LIVE_CONTROL_PLAN.md, rails S3 (v1 scope is bulk-only) and
# S4 (the allowlist is FROZEN as data, not as comments).
# =============================================================================

# --- Fixtures ---------------------------------------------------------------

# The allowlist is pure data plus pure helpers — no Shiny, no file system, no
# boot root. It is the one drive file that needs no fixture at all, which is
# precisely why it is worth freezing: nothing about it can drift silently.
.drv_al_source <- function() {
  source_project_file("R/core/drive_allowlist.R")
}

.drv_al_source()

# =============================================================================
# 0. The allowlist refuses to contradict itself at source() time
# =============================================================================

test_that("the shipped allowlist is internally consistent", {
  # The file self-checks with `stop()` at source time, so reaching this line
  # already proves the shipped table passes. Asserting it again pins the
  # CONTRACT (the check is callable and returns TRUE), not just the side effect.
  expect_true(isTRUE(ts_drive_allowlist_problems()))
})

test_that("a misplaced entry is detected, not silently accepted", {
  # FALSIFICATION: the consistency check must be able to FAIL. A checker that
  # only ever returns TRUE is not a check. So it takes the table as an
  # argument and is handed deliberately broken ones here.
  base <- list("bulk-de-probe" = list(kind = "select", module = "bulk_de"))
  expect_true(isTRUE(ts_drive_allowlist_problems(base, buttons = character(0))))

  bad_mod <- list("bulk-de-probe" = list(kind = "select", module = "sc"))
  problems <- ts_drive_allowlist_problems(bad_mod, buttons = character(0))
  expect_type(problems, "character")
  expect_true(any(grepl("not in TS_DRIVE_MODULES", problems)))
  expect_true(any(grepl("out of scope", problems)))

  bad_kind <- list("bulk-de-probe" = list(kind = "telepathy", module = "bulk_de"))
  expect_true(any(grepl("kind 'telepathy'", ts_drive_allowlist_problems(bad_kind, character(0)))))

  incomplete <- list("bulk-de-probe" = list(kind = "select"))
  expect_true(any(grepl("must declare", ts_drive_allowlist_problems(incomplete, character(0)))))

  bare <- list("nodefault" = list(kind = "select", module = "bulk_de"))
  expect_true(any(grepl("no dash", ts_drive_allowlist_problems(bare, character(0)))))

  # An `sc-` NAMESPACE is refused even when the module field lies about it.
  liar <- list("sc-nonsense" = list(kind = "select", module = "bulk_de"))
  expect_true(any(grepl("out of scope", ts_drive_allowlist_problems(liar, character(0)))))

  expect_identical(ts_drive_allowlist_problems(list(), character(0)), "allowlist is empty")
})

test_that("a bound button that is missing or mistyped is detected", {
  tbl <- list("bulk-de-run_de" = list(kind = "button", module = "bulk_de"))
  expect_true(any(grepl("missing from the allowlist",
                        ts_drive_allowlist_problems(tbl, c("bulk-de-run_de", "bulk-de-ghost")))))
  mistyped <- list("bulk-de-run_de" = list(kind = "select", module = "bulk_de"))
  expect_true(any(grepl("expected 'button'",
                        ts_drive_allowlist_problems(mistyped, "bulk-de-run_de"))))
})

# =============================================================================
# 1. Frozen data, and the module derivation that guards S3/S4
# =============================================================================

test_that("the allowlist is data, and every entry has a usable kind and module", {
  expect_true(length(TS_DRIVE_ALLOWLIST) > 0)
  for (id in names(TS_DRIVE_ALLOWLIST)) {
    entry <- TS_DRIVE_ALLOWLIST[[id]]
    expect_true(is.list(entry), info = id)
    expect_true(entry$kind %in% TS_DRIVE_KINDS, info = id)
    expect_true(entry$module %in% TS_DRIVE_MODULES, info = id)
    # S4: namespaced ids ONLY. A bare id could never resolve in a live session
    # and would silently no-op, which is worse than a refusal. The MODULE half
    # may contain dashes (`bulk-de-`, `bulk-pathways-`), the inputId half may
    # not — so the shape check is "two non-empty halves around a final dash".
    expect_match(id, "^[A-Za-z_][A-Za-z0-9_-]*-[A-Za-z0-9_]+$", info = id)
  }
})

test_that("every allowlisted id declares the module its prefix implies", {
  # Cross-check the DATA against the STRING: an entry whose declared module
  # disagrees with its own prefix would route a scenario to the wrong panel.
  expect_identical(ts_drive_module_of("bulk-de-run_de"), "bulk_de")
  expect_identical(ts_drive_module_of("bulk-pathways-run_pathway"), "bulk_pathways")
  expect_identical(ts_drive_module_of("import_bulk-btn_load"), "import_bulk")
  for (id in names(TS_DRIVE_ALLOWLIST)) {
    prefix <- sub("-.*$", "", id)
    expect_true(startsWith(id, prefix), info = id)
  }
})

test_that("the four bound buttons are exactly the ones the spec names", {
  expect_setequal(
    TS_DRIVE_BUTTONS,
    c("import_bulk-btn_load", "bulk-de-run_de",
      "bulk-pathways-run_pathway", "bulk-pathways-run_scores")
  )
  # Every button must be in the allowlist with kind = "button", otherwise
  # run_pipeline would accept an id the injector cannot classify.
  for (b in TS_DRIVE_BUTTONS) {
    expect_identical(TS_DRIVE_ALLOWLIST[[b]]$kind, "button", info = b)
  }
})

test_that("sc and spatial keys are NOT allowlisted (spec S3, v1 brake)", {
  # Explicit negative: the allowlist must not be silently widened to a domain
  # the spec froze out. If someone adds an sc key, this fails on purpose.
  sc_ids <- names(TS_DRIVE_ALLOWLIST)[startsWith(names(TS_DRIVE_ALLOWLIST), "sc-")]
  sp_ids <- names(TS_DRIVE_ALLOWLIST)[startsWith(names(TS_DRIVE_ALLOWLIST), "spatial-")]
  expect_length(sc_ids, 0)
  expect_length(sp_ids, 0)
})

test_that("ts_drive_module_of prefers the declared module, and falls back on the LAST dash", {
  expect_identical(ts_drive_module_of("bulk-de-run_de"), "bulk_de")
  expect_identical(ts_drive_module_of("bulk-pathways-run_pathway"), "bulk_pathways")
  expect_identical(ts_drive_module_of("import_bulk-btn_load"), "import_bulk")
  # Unknown ids still get a usable answer for the error message.
  expect_identical(ts_drive_module_of("bulk-de-unknown_probe"), "bulk-de")
  expect_identical(ts_drive_module_of("sc-nonsense"), "sc")
  expect_true(is.na(ts_drive_module_of("button")))
  expect_true(is.na(ts_drive_module_of(NA_character_)))
  expect_true(is.na(ts_drive_module_of(NULL)))
})

test_that("allowlist lookup refuses anything absent, including near-misses", {
  expect_true(ts_drive_allowlisted("bulk-de-run_de"))
  expect_false(ts_drive_allowlisted("bulk-de-run_de_typo"))
  expect_false(ts_drive_allowlisted("sc-run_analysis"))
  expect_false(ts_drive_allowlisted(NULL))
  expect_false(ts_drive_allowlisted(NA_character_))
  expect_null(ts_drive_allowlist_get("nothing-like-this"))
})

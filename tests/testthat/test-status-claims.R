# =============================================================================
# test-status-claims.R — le garde « document vs relevé » se prouve lui-même
# (doctrine « le sonde ment, pas le code »). Les chemins sont injectables :
# chaque cas construit ses fixtures dans tempdir() et appelle le GARDE REEL
# via Rscript, exit code lu par redirection de flux vers FICHIERS (mesure
# verify_committed_tree.R : stdout=TRUE ne rend pas le status de façon fiable).
# =============================================================================

.tc_tmp <- tempdir()
.tc_rscript <- file.path(R.home("bin"), "Rscript")
# Racine du dépôt, découverte robuste (même recette que test-runner-exit-codes.R)
.tc_root <- local({
  d <- normalizePath(getwd(), mustWork = FALSE)
  repeat {
    if (file.exists(file.path(d, "tools", "check_status_claims.R"))) break
    parent <- dirname(d)
    if (identical(parent, d)) stop("racine du dépôt introuvable depuis ", getwd())
    d <- parent
  }
  d
})
.tc_guard <- file.path(.tc_root, "tools", "check_status_claims.R")

.tc_write <- function(name, lines) {
  p <- file.path(.tc_tmp, name)
  writeLines(lines, p, useBytes = TRUE)
  p
}
.tc_run <- function(status, results, census) {
  out <- tempfile("status-claims-probe-")
  on.exit(unlink(out), add = TRUE)
  st <- suppressWarnings(system2(
    .tc_rscript,
    c(shQuote(.tc_guard), shQuote(status), shQuote(results), shQuote(census)),
    stdout = out, stderr = out, timeout = 120
  ))
  list(status = st, out = if (file.exists(out)) paste(readLines(out, warn = FALSE), collapse = "\n") else character())
}
.tc_status_ok <- c(
  "## 0. État courant",
  "Baseline : `failed=0 passed=10 error=0 skipped=2` sur **3** fichiers.",
  "",
  "## 1. Autre section",
  "Vieille baseline historique : `failed=9 passed=999 error=0 skipped=9` — NE DOIT PAS être lue."
)
.tc_results_ok <- c(
  "start 00:00:00 | 3 files",
  "CENSUS: begin", "CENSUS: total=3",
  "BILAN: failed=0 passed=10 error=0 skipped=2 elapsed_s=1 | 00:00:42"
)
.tc_census_ok <- c("# Census", "CENSUS: total=3")

test_that("accord document/relevé -> exit 0, et §0 seul est lu (pas les sections suivantes)", {
  st <- .tc_run(.tc_write("st_ok.md", .tc_status_ok),
                .tc_write("res_ok.txt", .tc_results_ok),
                .tc_write("cen_ok.txt", .tc_census_ok))
  expect_identical(st$status, 0L)
  expect_match(st$out, "0 desaccord", fixed = TRUE)
})

test_that("BILAN contredit §0 -> exit 1 avec le champ fautif nommé", {
  st <- .tc_run(.tc_write("st_ok2.md", .tc_status_ok),
                .tc_write("res_ko.txt", gsub("passed=10", "passed=9", .tc_results_ok)),
                .tc_write("cen_ok2.txt", .tc_census_ok))
  expect_identical(st$status, 1L)
  expect_match(st$out, "passed", fixed = TRUE)
})

test_that("deux baselines contradictoires DANS §0 -> exit 1 (auto-police)", {
  st_dup_lines <- c(.tc_status_ok[1:2], "Encore : `failed=0 passed=11 error=0 skipped=2`.", .tc_status_ok[3:5])
  st <- .tc_run(.tc_write("st_dup.md", st_dup_lines),
                .tc_write("res_dup.txt", .tc_results_ok),
                .tc_write("cen_dup.txt", .tc_census_ok))
  expect_identical(st$status, 1L)
  expect_match(st$out, "CONTRADICTOIRES", fixed = TRUE)
})

test_that("§0 sans baseline chiffrée -> exit 1 (la baseline est la politique)", {
  st <- .tc_run(.tc_write("st_vide.md", c("## 0. État courant", "Rien de chiffré ici.")),
                .tc_write("res_vide.txt", .tc_results_ok),
                .tc_write("cen_vide.txt", .tc_census_ok))
  expect_identical(st$status, 1L)
})

test_that("faits absents (clone frais) -> RAS, exit 0 — le garde ne juge pas sans faits", {
  st <- .tc_run(.tc_write("st_ok3.md", .tc_status_ok),
                file.path(.tc_tmp, "inexistant_results.txt"),
                file.path(.tc_tmp, "inexistant_census.txt"))
  expect_identical(st$status, 0L)
})

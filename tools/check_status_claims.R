# tools/check_status_claims.R — garde local « document vs relevé » (H2 #9).
# POURQUOI : en 3 jours, les chiffres hérités des documents ont mentis 5 fois
# (8 baselines du brief d'audit, « 171/171 » vs 173, « 2 skips » vs 4). La CI
# GitHub Actions étant abandonnée (2026-10-03), ce garde vit LOCALEMENT :
# à lancer à la main après chaque suite complète, ou avant un push sensible.
#
# Ce qu'il fait : extrait de `docs/STATUS.md` §0 la(les) baseline(s) chiffrée(s)
# revendiquée(s) — `failed=… passed=… error=… skipped=…` et `N fichiers` — et
# les compare aux FAITS mesurés quand ils existent :
#   * `full_suite_results.txt` (BILAN + CENSUS: total) ;
#   * `docs/audits/census.txt` (CENSUS: total).
# Règles :
#   * plusieurs baselines dans §0 ⇒ elles doivent être IDENTIQUES entre elles
#     (deux chiffres contradictoires dans le même §0 = échec) ;
#   * faits absents (clone frais : §7 exclut docs/ et le relevé est local)
#     ⇒ « RAS », exit 0 — le garde ne juge pas là où il n'a pas de faits ;
#   * désaccord fait/relevé ⇒ exit 1.
# Usage : LC_ALL=French_France.65001 Rscript --vanilla tools/check_status_claims.R
# (chemins surchargeables en arguments pour les tests : status, results, census).

.localectl <- Sys.setlocale("LC_CTYPE", "fr_FR.UTF-8")
if (!nzchar(.localectl)) {
  warning("LC_CTYPE fr_FR.UTF-8 unavailable: parse() false failures are likely")
}
args <- commandArgs(trailingOnly = TRUE)
status_path   <- if (length(args) >= 1L) args[1] else "docs/STATUS.md"
results_path  <- if (length(args) >= 2L) args[2] else "full_suite_results.txt"
census_path   <- if (length(args) >= 3L) args[3] else "docs/audits/census.txt"

fail <- function(msg) {
  cat(sprintf("ERREUR : %s\n", msg))
  quit(save = "no", status = 1L)
}

if (!file.exists(status_path)) {
  cat("RAS : pas de STATUS.md à vérifier (clone frais, §7)\n")
  quit(save = "no", status = 0L)
}
# §0 uniquement : du titre « ## 0. » jusqu'au prochain titre « ## » — découpage
# PAR LIGNES (un regex `.` ne traverse pas les \n, et les fins CRLF/LF varient).
status_lines <- readLines(status_path, warn = FALSE, encoding = "UTF-8")
i0 <- grep("^## 0\\.", status_lines)[1]
if (is.na(i0)) fail("§0 introuvable dans STATUS.md — la baseline n'est plus déclarée")
headings <- grep("^## ", status_lines)
i1 <- headings[headings > i0][1]
if (is.na(i1)) i1 <- length(status_lines) + 1L
section0 <- paste(status_lines[i0:(i1 - 1L)], collapse = "\n")
section0_lines <- status_lines[i0:(i1 - 1L)]

# Toutes les baselines revendiquées dans §0 — elles doivent coïncider.
claims_m <- gregexpr("failed=\\d+ passed=\\d+ error=\\d+ skipped=\\d+", section0, perl = TRUE)
claims_txt <- regmatches(section0, claims_m)[[1]]
if (!length(claims_txt)) fail("§0 ne revendique aucune baseline chiffrée (failed=/passed=/…)")
if (length(unique(claims_txt)) > 1L) {
  fail(sprintf("§0 porte %d baselines CONTRADICTOIRES : %s",
               length(unique(claims_txt)), paste(unique(claims_txt), collapse = "  VS  ")))
}
# Comptes de fichiers : uniquement sur les LIGNES qui portent une baseline
# (sinon « 3 fichiers de R/ à couverture nulle » du tableau serait capté) ;
# comparaison sur les ENTIERS (« 173** fichiers » et « 173 fichiers » = pareil).
claim_lines <- grep("failed=\\d+ passed=\\d+", section0_lines, value = TRUE)
nf <- unlist(regmatches(claim_lines, gregexpr("\\d+\\*{0,2}\\s*fichiers", claim_lines, perl = TRUE)))
nf_n <- as.integer(sub("\\*{0,2}\\s*fichiers", "", nf))
if (length(unique(nf_n)) > 1L) {
  fail(sprintf("§0 porte %d comptes de fichiers CONTRADICTOIRES : %s",
               length(unique(nf_n)), paste(unique(nf_n), collapse = "  VS  ")))
}
claim_nfiles <- if (length(nf_n)) nf_n[1] else NA_integer_
getn <- function(txt, tag) {
  m <- regmatches(txt, regexpr(paste0(tag, "=(\\d+)"), txt))
  if (!length(m)) return(NA_integer_)
  as.integer(sub(paste0(tag, "="), "", m))
}
claim <- c(failed = getn(claims_txt[1], "failed"), passed = getn(claims_txt[1], "passed"),
           error = getn(claims_txt[1], "error"), skipped = getn(claims_txt[1], "skipped"))

problems <- character(0)

if (file.exists(results_path)) {
  res_txt <- paste(readLines(results_path, warn = FALSE), collapse = "\n")
  bilan <- regmatches(res_txt, regexpr("BILAN: [^|]*", res_txt))
  if (!length(bilan)) {
    problems <- append(problems, "full_suite_results.txt présent mais sans ligne BILAN")
  } else {
    fact <- c(failed = getn(bilan, "failed"), passed = getn(bilan, "passed"),
              error = getn(bilan, "error"), skipped = getn(bilan, "skipped"))
    diffs <- names(claim)[which(claim != fact)]
    if (length(diffs)) {
      problems <- append(problems, sprintf(
        "BILAN contredit §0 sur %s (document : %s / relevé : %s)", paste(diffs, collapse = ","),
        claims_txt[1], trimws(sub("\\|.*$", "", bilan))))
    }
  }
  # NB : le « CENSUS: total » du RELEVÉ décrit la population du run mesuré —
  # il n'est PAS comparé ici (il diverge légitimement à chaque test ajouté).
  # Le compte de fichiers de §0 est confronté au census COURANT (bloc suivant).
} else {
  cat("Note : aucun full_suite_results.txt local — les chiffres de §0 ne sont pas confrontés au relevé.\n")
}

if (file.exists(census_path)) {
  cen_all <- paste(readLines(census_path, warn = FALSE), collapse = "\n")
  cen <- regmatches(cen_all, regexpr("CENSUS: total=\\d+", cen_all))
  if (length(cen)) {
    fact_n <- as.integer(sub("CENSUS: total=", "", cen))
    if (!is.na(claim_nfiles) && fact_n != claim_nfiles) {
      problems <- append(problems, sprintf(
        "census.txt (%d fichiers) contredit §0 (%d fichiers)", fact_n, claim_nfiles))
    }
  }
} else {
  cat("Note : pas de docs/audits/census.txt (clone frais, §7) — régénérer avec tools/build_census.R.\n")
}

if (length(problems)) {
  for (p in problems) cat(sprintf("ERREUR : %s\n", p))
  cat(sprintf("---- Resume : %d desaccord(s) document vs releve ----\n", length(problems)))
  quit(save = "no", status = 1L)
}
cat(sprintf("---- Resume : 0 desaccord — §0 (%s ; %s fichiers) concorde avec les faits disponibles ----\n",
            claims_txt[1], if (is.na(claim_nfiles)) "?" else claim_nfiles))
quit(save = "no", status = 0L)

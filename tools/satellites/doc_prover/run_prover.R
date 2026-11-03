#!/usr/bin/env Rscript
# run_prover.R — runner du satellite doc_prover (jalon 4, slice S5 N6).
#
# USAGE (depuis la racine du dépôt ou n'importe où — le script se repère) :
#   Rscript tools/satellites/doc_prover/run_prover.R           # static uniquement
#   Rscript tools/satellites/doc_prover/run_prover.R --live    # + claims LIVE
#
# Ce que le runner fait :
#   1. lit claims.json (jsonlite — DÉJÀ présent dans renv.lock, jamais installé ici) ;
#   2. évalue TOUJOURS les claims static (headless : fichiers, ancres, contrats) ;
#   3. n'évalue les claims LIVE que si --live ET que la sonde détecte une session
#      armée (ready.json frais + hb_n qui avance) — sinon ils rendent "untested",
#      JAMAIS une invention ; les deux claims live à précondition propre
#      (handshake périmé pour live-stale-tool, job en vol observé pour
#      live-job-inflight) restent untested si leur état n'est pas observé ;
#   4. écrit report.json + report.md (statut par claim, evidence, comptes
#      ✅/❌/UNTESTED, verdict global) à côté de ce script.
#
# Le runner est LECTURE-seul sur le dépôt ; seule exception, sous --live : la
# réécriture de arm.json (drop-box tools/_drive/*.json, GITIGNORÉE,
# .gitignore:100) pour la falsification du re-arm §4.2. L'app n'est jamais
# lancée, jamais sourcée ; le satellite n'est jamais chargé par l'app.
#
# Code de sortie : 0 si le rapport a été écrit (les findings de drift sont un
# LIVRABLE, pas un échec du prover) ; 2 si l'entrée est illisible.

.lib_dir <- function() {
  # répertoire de CE script (via --file=), sinon cwd
  a <- commandArgs(trailingOnly = FALSE)
  f <- grep("^--file=", a, value = TRUE)
  if (length(f) == 1L) return(dirname(normalizePath(sub("^--file=", "", f))))
  normalizePath(".")
}

try(Sys.setlocale("LC_ALL", "French_France.65001"), silent = TRUE)

.lib_dir_ <- .lib_dir()
.repo_root <- normalizePath(file.path(.lib_dir_, "..", "..", ".."))
.claims_path <- file.path(.lib_dir_, "claims.json")

cat(sprintf("doc-prover — dépôt : %s\n", .repo_root))
cat(sprintf("doc-prover — claims : %s\n", .claims_path))

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  cat("ERREUR : jsonlite indisponible — claims illisibles (0 nouvelle dépendance assumée).\n")
  quit(save = "no", status = 2)
}
if (!file.exists(.claims_path)) {
  cat(sprintf("ERREUR : %s absent.\n", .claims_path))
  quit(save = "no", status = 2)
}
.claims_raw <- tryCatch(
  jsonlite::fromJSON(.claims_path, simplifyDataFrame = FALSE),
  error = function(e) NULL)
if (is.null(.claims_raw) || is.null(.claims_raw$claims)) {
  cat("ERREUR : claims.json illisible ou sans champ « claims ».\n")
  quit(save = "no", status = 2)
}
.claims <- .claims_raw$claims
source(file.path(.lib_dir_, "prover_lib.R"), local = TRUE)

# -- faits git pré-collectés (le checker est pur : il reçoit les résultats) ----
.git_hashes <- c("7cccd6c4af46c75f285dce992ab6dc0c0c756cfe",
                 "db12712259cc11da5e6e877e525f095b59fd5fbe",
                 "59c488803e3486ca41fccb64bbbe6f6d5df16a1c")
.git_cat <- local({
  out <- list()
  for (h in .git_hashes) {
    r <- tryCatch(suppressWarnings(system2("git", c("cat-file", "-t", h),
                                           stdout = TRUE, stderr = FALSE, timeout = 30)),
                  error = function(e) NULL, warning = function(w) NULL)
    if (is.null(r) || length(r) == 0L || !nzchar(trimws(paste(r, collapse = "")))) {
      # distinguer « git inutilisable » (NULL -> untested) de « objet absent »
      st <- attr(r, "status")
      out[[h]] <- if (!is.null(r) && !is.null(st) && any(st != 0)) "absent" else NA_character_
    } else {
      out[[h]] <- trimws(paste(r, collapse = " "))
    }
  }
  if (any(vapply(out, is.na, logical(1)))) NULL else out
})

# -- sonde live (seule source d'impureté, opt-in --live) ----------------------
.live_flag <- "--live" %in% commandArgs(trailingOnly = TRUE)
.drive_dir <- file.path(.repo_root, "tools", "_drive")
.probe <- NULL
if (.live_flag) {
  cat("doc-prover — --live : sonde de la session drive (2 lectures espacées ~4 s)...\n")
  .probe <- probe_drive_session(.drive_dir, wait_s = 4L,
                                do_rearm = TRUE, do_stale_tool = TRUE,
                                repo_root = .repo_root)
  if (isTRUE(.probe$ready_missing)) {
    cat("doc-prover — ready.json ABSENT : gate live non satisfait, claims live = untested.\n")
  } else if (gate_live_ok(.probe)) {
    cat(sprintf("doc-prover — session armée détectée (age %.1f s, hb_n %s -> %s).\n",
                .probe$age_s, format(.probe$hb_n1), format(.probe$hb_n2)))
  } else {
    cat("doc-prover — pas de session armée fraîche + battante : claims live = untested (honnêteté).\n")
  }
} else {
  cat("doc-prover — sans --live : claims live = untested par construction.\n")
}

.ctx <- list(repo_root = .repo_root, drive_dir = .drive_dir,
             git_cat = .git_cat, probe = .probe,
             now = Sys.time())

.results <- lapply(.claims, evaluate_claim, ctx = .ctx)
.report <- build_report(.claims, .results, meta = list(
  engine = "prover_lib (base R)",
  doc = .claims_raw$doc,
  live = .live_flag,
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  repo_root = .repo_root
))

# -- sorties -------------------------------------------------------------------
.json_path <- file.path(.lib_dir_, "report.json")
.md_path <- file.path(.lib_dir_, "report.md")

writeLines(jsonlite::toJSON(.report, auto_unbox = TRUE, pretty = TRUE, null = "null"),
           .json_path, useBytes = TRUE)

.md <- c(
  sprintf("# Rapport doc-prover — %s", .claims_raw$doc),
  "",
  sprintf("Généré : %s · --live : %s · moteur : %s",
          .report$meta$generated_at, if (.live_flag) "oui" else "non", .report$meta$engine),
  "",
  sprintf("**Verdict global : %s**", .report$verdict_global),
  "",
  sprintf("Comptes : ✅ pass = %d · ❌ fail = %d · UNTESTED = %d — total %d claims (%d static / %d live)",
          .report$counts$pass, .report$counts$fail, .report$counts$untested,
          length(.report$rows),
          sum(vapply(.claims, function(x) identical(x$type, "static"), logical(1))),
          sum(vapply(.claims, function(x) identical(x$type, "live"), logical(1)))),
  ""
)
if (length(.report$findings) > 0L) {
  .md <- c(.md, "## ❌ Findings de drift (filés, pas réparés par ce jalon)", "")
  for (f in .report$findings) {
    .md <- c(.md,
      sprintf("### %s — %s l.%s", f$id, f$doc, format(f$line)),
      sprintf("- **Assertion (runbook)** : %s", f$assertion),
      sprintf("- **Attendu** : %s", f$expected),
      sprintf("- **Mesuré** : %s", f$evidence),
      "")
  }
} else {
  .md <- c(.md, "## ❌ Findings de drift", "", "Aucun — toutes les assertions statiques mesurées sont vraies.", "")
}
.untested <- Filter(function(r) identical(r$verdict, "untested"), .report$rows)
if (length(.untested) > 0L) {
  .md <- c(.md, "## UNTESTED (honnêteté — jamais inventés)", "")
  for (u in .untested) {
    .md <- c(.md, sprintf("- **%s** (l.%s) : %s", u$id, format(u$line), u$evidence))
  }
  .md <- c(.md, "")
}
.md <- c(.md, "## Détail par claim", "",
  "| id | type | l. | verdict | evidence |",
  "|---|---|---|---|---|")
for (r in .report$rows) {
  .badge <- switch(r$verdict, pass = "✅", fail = "❌", "UNTESTED")
  .ev <- gsub("\\|", "/", r$evidence)
  .md <- c(.md, sprintf("| %s | %s | %s | %s | %s |", r$id, r$type, format(r$line), .badge, .ev))
}
.md <- c(.md, "", "---",
  "Satellite doc_prover (jalon 4 / S5 N6) — lecture-seul sur le dépôt ; voir README.md (instruction de service, procédure de falsification).")
writeLines(.md, .md_path, useBytes = TRUE)

cat(sprintf("\n=== doc-prover : %s ===\n", .report$verdict_global))
cat(sprintf("pass=%d fail=%d untested=%d\n", .report$counts$pass,
            .report$counts$fail, .report$counts$untested))
cat(sprintf("rapports écrits : %s | %s\n", .json_path, .md_path))
quit(save = "no", status = 0)

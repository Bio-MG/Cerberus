# =============================================================================
# test-check-writers.R — MODE ESSAIM du garde tools/check_writers.R (SW-3, O9)
# =============================================================================
# Jalon SW-3 (docs/ROADMAP_SWARM.md §SW-3 + décision O9, docs/agent/DECISIONS.md
# 2026-10-25) : check_writers.R gagne un MODE essaim (CLI `--swarm`), le mode
# par défaut restant INCHANGÉ (décision O4 : étendre le garde, pas un 3e outil).
#
# CONTRAT que ce test épingle (implémentation attendue dans le garde) :
#   1. Fonction in-process : `swarm_check(root = ".")` -> character() de
#      constats ; VIDE = silence. Chaque constat : `<CLASSE> <fichier>:<ligne>
#      — <message>` avec CLASSE ∈ {BLOC_ESSAIM, REGISTRE}.
#   2. CLI : `Rscript tools/check_writers.R --swarm [--root=<racine>]`
#      -> exit 0 si silence, exit != 0 si au moins un constat.
#   3. BLOC ESSAIM de docs/agent/HANDOFF.md (section repérée par un titre
#      contenant « bloc essaim », insensible à la casse) — s'il existe :
#        a. EXACTEMENT UNE entrée `état : EN COURS` (le porteur) ;
#        b. `rôle :` ∈ {orchestrateur, collecteur, bâtisseur, vérificateur} ;
#        c. `périmètre :` disjoints entre agents déclarés ;
#        d. toute `branche :` jetable mentionne ET la naissance ET la
#           destruction programmée (convention 2026-10-14).
#      Incohérence => constat classé. ABSENCE de bloc => silence (sessions solo).
#   4. LINT DES REGISTRES (globs : docs/agent/swarm/**/collector_*_claims.md
#      et fiches de vérification = fichiers *fiche*/*verification* hors
#      TEMPLATE/SCHEMA/README) :
#        a. table du schéma à 8 colonnes (docs/agent/swarm/CLAIMS_LEDGER_SCHEMA.md)
#           pour les registres collector_*_claims.md ;
#        b. verdicts dans EXACTEMENT {CONFIRMÉ, CONFIRMÉ-NUANCÉ, RÉFUTÉ,
#           NON VÉRIFIÉ} — « CONFIRMÉ (dérive) » est HORS vocabulaire ;
#        c. chaque ligne-claim porte une COMMANDE de preuve (cellule « proof »)
#           — « lecture du code » seul n'est pas une preuve.
#      Écart => constat avec fichier:ligne (+ ID de claim).
#
# Harness : réutilise les idiomes mesurés des gardes existants —
#   - test-check_duplication.R : source_project_file() + appel in-process ;
#   - test-run-all-gates.R:618-635 : system2() — commande NON re-quotée
#     (piège mesuré), ARGUMENTS shQuote()s (la racine contient des espaces).
# Fixtures : tempdir() UNIQUEMENT (rien dans l'arbre suivi). Piège locale
# (règle 3 du schéma de claims) : Sys.setlocale("LC_CTYPE",
# "English_United States.utf8") avant tout parse / écriture multi-octet.
# =============================================================================

.cwr_utf8 <- tryCatch({
  Sys.setlocale("LC_CTYPE", "English_United States.utf8")
  grepl("utf", tolower(Sys.getlocale("LC_CTYPE")), fixed = TRUE)
}, error = function(e) FALSE, warning = function(w) FALSE)

testthat::skip_if_not(.cwr_utf8,
  "locale UTF-8 indisponible sur cet hôte — fixtures accentuées inexactes (piège LC_CTYPE Windows)")

source_project_file("tools/check_writers.R")

# --- harnais ------------------------------------------------------------------

.cwr_rscript <- function() {
  file.path(R.home("bin"),
            if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
}

# Subprocess : cwd = racine de fixture, script par chemin ABSOLU quoté.
# Le code de sortie est lu dans attr(out, "status") — NULL <=> 0.
.cwr_cli <- function(root, flags) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(root)
  script <- file.path(ts_project_root(), "tools", "check_writers.R")
  out <- suppressWarnings(system2(.cwr_rscript(),
                                  c(shQuote(script), flags),
                                  stdout = TRUE, stderr = TRUE))
  list(output = paste(out, collapse = "\n"),
       status = if (is.null(attr(out, "status"))) 0L else attr(out, "status"))
}

.cwr_write_utf8 <- function(path, lines) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(enc2utf8(lines), con, useBytes = TRUE)
}

.cwr_tmp_root <- function(tag) {
  root <- file.path(tempdir(), paste0("cwr_swarm_", tag, "_",
                                      as.integer(Sys.time()), "_",
                                      as.integer(stats::runif(1, 1, 1e6))))
  dir.create(file.path(root, "docs", "agent"), recursive = TRUE,
             showWarnings = FALSE)
  root
}

.cwr_swarm_dir <- function(root) file.path(root, "docs", "agent", "swarm")

# --- fixtures (tempdir, jamais dans l'arbre suivi) ----------------------------

# BLOC ESSAIM INCOHÉRENT : 2 états EN COURS ; rôle hors vocabulaire
# (« boulanger ») ; périmètres qui se recouvrent (check_writers.R entre
# agent-a/agent-b, R/core/foo.R entre agent-b/agent-c) ; branche jetable
# sans destruction programmée (agent-d).
.cwr_fixture_handoff_incoherent <- function(root) {
  .cwr_write_utf8(file.path(root, "docs", "agent", "HANDOFF.md"), c(
    "# HANDOFF — fixture test-check-writers",
    "",
    "## Propriété des écritures",
    "",
    "| | |",
    "|---|---|",
    "| **État** | EN COURS — fixture |",
    "",
    "## Bloc essaim — 2026-10-25",
    "",
    "- **agent-a** — rôle : bâtisseur · état : EN COURS · périmètre : `tools/check_writers.R` · branche : `sw-x` (naissance `abc1234` → destruction programmée après ff-merge)",
    "- **agent-b** — rôle : boulanger · état : EN COURS · périmètre : `tools/check_writers.R`, `R/core/foo.R`",
    "- **agent-c** — rôle : collecteur · état : LIVRÉ · périmètre : `R/core/foo.R`",
    "- **agent-d** — rôle : vérificateur · état : LIVRÉ · branche : `sw-y` (naissance `def5678`)",
    ""
  ))
}

# BLOC ESSAIM dont AUCUNE entrée n'est EN COURS (aucun porteur).
.cwr_fixture_handoff_sans_porteur <- function(root) {
  .cwr_write_utf8(file.path(root, "docs", "agent", "HANDOFF.md"), c(
    "# HANDOFF — fixture test-check-writers",
    "",
    "## Bloc essaim — 2026-10-25",
    "",
    "- **agent-a** — rôle : bâtisseur · état : LIVRÉ · périmètre : `tools/check_writers.R` · branche : `sw-x` (naissance `abc1234` → destruction programmée après ff-merge)",
    ""
  ))
}

# BLOC ESSAIM CONFORME : exactement 1 EN COURS (bâtisseur), rôles valides,
# périmètres disjoints, branche = naissance + destruction programmée.
.cwr_fixture_handoff_conforme <- function(root) {
  .cwr_write_utf8(file.path(root, "docs", "agent", "HANDOFF.md"), c(
    "# HANDOFF — fixture test-check-writers",
    "",
    "## Bloc essaim — 2026-10-25",
    "",
    "- **orch-1** — rôle : orchestrateur · état : LIVRÉ · périmètre : `docs/agent/HANDOFF.md`",
    "- **bat-1** — rôle : bâtisseur · état : EN COURS · périmètre : `tools/check_writers.R`, `tests/testthat/test-check-writers.R` · branche : `sw-t` (naissance `abc1234` → destruction programmée après ff-merge)",
    "- **col-1** — rôle : collecteur · état : LIVRÉ · périmètre : `docs/agent/swarm/round2/collector_SC_claims.md`",
    "- **ver-1** — rôle : vérificateur · état : LIVRÉ",
    ""
  ))
}

# REGISTRE NON CONFORME (8 colonnes) : TB-01 verdict HORS vocabulaire
# (« CONFIRMÉ (dérive) » — l'écart de forme réel mesuré en SW-4, collector_BULK
# BULK-17) ; TB-02 sans commande de preuve (« lecture visuelle » seul).
.cwr_fixture_registre_non_conforme <- function(root) {
  .cwr_write_utf8(
    file.path(.cwr_swarm_dir(root), "round9", "collector_TB_claims.md"), c(
      "# Registre de claims — collecteur TB (fixture)",
      "",
      "- **Shard couvert** : `TB` — fixture test-check-writers.",
      "",
      "| ID | source (file:line) | claim (verbatim, ≤25 mots) | domain | severity AS STATED | verdict | proof (command + measured value) | scope |",
      "|---|---|---|---|---|---|---|---|",
      "| TB-01 | docs/p.md:10 | « affirmation dérivée » | docs-drift | P1 | CONFIRMÉ (dérive) | `grep -c \"foo\" app.R` → 2 | arbre daté 2026-10-25 |",
      "| TB-02 | docs/p.md:11 | « sans commande » | docs-drift | P1 | RÉFUTÉ | lecture visuelle du code, rien exécuté | arbre daté |",
      ""
    ))
}

# REGISTRE à 7 colonnes (schéma = 8).
.cwr_fixture_registre_sept_colonnes <- function(root) {
  .cwr_write_utf8(
    file.path(.cwr_swarm_dir(root), "round8", "collector_SEVEN_claims.md"), c(
      "# Registre — fixture 7 colonnes",
      "",
      "| ID | source (file:line) | claim | domain | severity | verdict | proof |",
      "|---|---|---|---|---|---|---|",
      "| T7-01 | docs/p.md:1 | « x » | docs-drift | P1 | CONFIRMÉ | `grep -c \"x\" app.R` → 2 |",
      ""
    ))
}

# REGISTRE CONFORME : 8 colonnes, verdicts dans le vocabulaire à 4 valeurs,
# commande de preuve sur chaque ligne.
.cwr_fixture_registre_conforme <- function(root) {
  .cwr_write_utf8(
    file.path(.cwr_swarm_dir(root), "round2", "collector_TC_claims.md"), c(
      "# Registre de claims — collecteur TC (fixture conforme)",
      "",
      "- **Shard couvert** : `TC` — fixture test-check-writers.",
      "",
      "| ID | source (file:line) | claim (verbatim, ≤25 mots) | domain | severity AS STATED | verdict | proof (command + measured value) | scope |",
      "|---|---|---|---|---|---|---|---|",
      "| TC-01 | docs/p.md:1 | « conforme un » | docs-drift | P1 | CONFIRMÉ | `grep -c \"foo\" app.R` → 2 | arbre daté 2026-10-25 |",
      "| TC-02 | docs/p.md:2 | « conforme deux » | environment | P2 | NON VÉRIFIÉ | `ls docs/absent.md` → artefact absent (cause c) | arbre daté 2026-10-25 |",
      "| TC-03 | docs/p.md:3 | « conforme trois » | contracts | P2 | CONFIRMÉ-NUANCÉ | `git rev-parse HEAD` → b272e56 | arbre daté 2026-10-25 |",
      ""
    ))
}

# REGISTRE DÉGÉNÉRÉ : en-tête + séparateur en DERNIÈRES lignes du fichier
# (aucune ligne-claim). La plage (sep_i+1):length(lines) devient descendante —
# le garde doit émettre un constat, pas planter (contre-lecture 2026-10-25).
.cwr_fixture_registre_vide <- function(root) {
  .cwr_write_utf8(
    file.path(.cwr_swarm_dir(root), "round7", "collector_EMPTY_claims.md"), c(
      "# Registre de claims — collecteur EMPTY (fixture dégénérée)",
      "",
      "| ID | source (file:line) | claim (verbatim, ≤25 mots) | domain | severity AS STATED | verdict | proof (command + measured value) | scope |",
      "|---|---|---|---|---|---|---|---|"
    ))
}

# FICHE DE VÉRIFICATION NON CONFORME : VF-01 verdict hors vocabulaire,
# VF-02 sans commande de preuve.
.cwr_fixture_fiche_non_conforme <- function(root) {
  .cwr_write_utf8(
    file.path(.cwr_swarm_dir(root), "round3", "VERIFICATION_FICHE_TEST.md"), c(
      "# Vérification de l'objet X (2026-10-25) — fixture",
      "",
      "| ID | constat source (cité) | verdict | preuve (commande + valeur mesurée) | nuance / portée |",
      "|---|---|---|---|---|",
      "| VF-01 | « constat un » | CONFIRMÉ (dérive) | `grep -c \"x\" app.R` → 2 | arbre |",
      "| VF-02 | « constat deux » | RÉFUTÉ | lecture visuelle | arbre |",
      ""
    ))
}

# =============================================================================
# 1. Fonction in-process swarm_check() — le contrat du mode essaim
# =============================================================================

test_that("swarm_check() existe (mode essaim livré dans le garde)", {
  expect_true(exists("swarm_check", envir = globalenv(), inherits = FALSE),
              info = "swarm_check() introuvable : le mode essaim de check_writers.R (SW-3, O9) n'est pas implémenté")
})

test_that("bloc essaim incohérent => signalé (état, rôle, périmètre, branche)", {
  root <- .cwr_tmp_root("incoh")
  .cwr_fixture_handoff_incoherent(root)
  findings <- swarm_check(root = root)
  expect_true(length(findings) > 0,
              info = "aucun constat sur un bloc essaim incohérent (2 EN COURS, rôle « boulanger », périmètres recouverts, branche sans destruction)")
  expect_true(any(grepl("BLOC_ESSAIM", findings, fixed = TRUE)),
              info = paste("constats :", paste(findings, collapse = " | ")))
  expect_true(any(grepl("EN COURS", findings, fixed = TRUE)),
              info = "l'incohérence d'état (2 porteurs EN COURS) n'est pas signalée")
  expect_true(any(grepl("boulanger", findings, fixed = TRUE)),
              info = "le rôle hors vocabulaire « boulanger » n'est pas signalé")
  expect_true(any(grepl("check_writers.R", findings, fixed = TRUE)),
              info = "le recouvrement de périmètre (check_writers.R) n'est pas signalé")
  expect_true(any(grepl("destruction", findings, fixed = TRUE)),
              info = "la branche jetable sans destruction programmée n'est pas signalée (convention 2026-10-14)")
})

test_that("bloc essaim sans AUCUN porteur EN COURS => signalé", {
  root <- .cwr_tmp_root("sansport")
  .cwr_fixture_handoff_sans_porteur(root)
  findings <- swarm_check(root = root)
  expect_true(any(grepl("BLOC_ESSAIM", findings, fixed = TRUE)) &&
                any(grepl("EN COURS", findings, fixed = TRUE)),
              info = paste("constats :", paste(findings, collapse = " | ")))
})

test_that("registre non conforme => verdict hors vocabulaire + ligne sans commande, avec fichier:ligne", {
  root <- .cwr_tmp_root("registre")
  .cwr_fixture_registre_non_conforme(root)
  findings <- swarm_check(root = root)
  expect_true(any(grepl("TB-01", findings, fixed = TRUE)) &&
                any(grepl("verdict", findings, fixed = TRUE)),
              info = paste("le verdict hors vocabulaire « CONFIRMÉ (dérive) » de TB-01 n'est pas signalé — constats :",
                           paste(findings, collapse = " | ")))
  expect_true(any(grepl("TB-02", findings, fixed = TRUE)) &&
                any(grepl("commande", findings, fixed = TRUE)),
              info = paste("la ligne TB-02 sans commande de preuve n'est pas signalée — constats :",
                           paste(findings, collapse = " | ")))
  expect_true(any(grepl("collector_TB_claims\\.md:[0-9]+", findings)),
              info = paste("les constats registre ne citent pas fichier:ligne — constats :",
                           paste(findings, collapse = " | ")))
})

test_that("registre à 7 colonnes (schéma = 8) => signalé", {
  root <- .cwr_tmp_root("septcol")
  .cwr_fixture_registre_sept_colonnes(root)
  findings <- swarm_check(root = root)
  expect_true(any(grepl("REGISTRE", findings, fixed = TRUE)) &&
                any(grepl("colon", findings)),
              info = paste("constats :", paste(findings, collapse = " | ")))
})

test_that("registre dégénéré (en-tête+séparateur en fin de fichier) => constat, pas crash", {
  root <- .cwr_tmp_root("vide")
  .cwr_fixture_registre_vide(root)
  findings <- swarm_check(root = root)
  expect_true(any(grepl("REGISTRE", findings, fixed = TRUE)) &&
                any(grepl("collector_EMPTY_claims\\.md", findings)),
              info = paste("constats :", paste(findings, collapse = " | ")))
})

test_that("fiche de vérification non conforme => signalée", {
  root <- .cwr_tmp_root("fiche")
  .cwr_fixture_fiche_non_conforme(root)
  findings <- swarm_check(root = root)
  expect_true(any(grepl("VF-01", findings, fixed = TRUE)) &&
                any(grepl("verdict", findings, fixed = TRUE)),
              info = paste("constats :", paste(findings, collapse = " | ")))
  expect_true(any(grepl("VF-02", findings, fixed = TRUE)) &&
                any(grepl("commande", findings, fixed = TRUE)),
              info = paste("constats :", paste(findings, collapse = " | ")))
})

# =============================================================================
# 2. Silence attendu (sessions solo + essaim conforme)
# =============================================================================

test_that("bloc essaim conforme + registre conforme => silence (character(0))", {
  root <- .cwr_tmp_root("conforme")
  .cwr_fixture_handoff_conforme(root)
  .cwr_fixture_registre_conforme(root)
  findings <- swarm_check(root = root)
  expect_identical(as.character(findings), character(0),
                   info = paste("faux positifs sur fixtures conformes :",
                                paste(findings, collapse = " | ")))
})

test_that("absence de bloc essaim (session solo) => silence", {
  root <- .cwr_tmp_root("solo")
  .cwr_write_utf8(file.path(root, "docs", "agent", "HANDOFF.md"), c(
    "# HANDOFF — fixture solo",
    "",
    "| **État** | EN COURS — fixture sans bloc essaim |",
    ""
  ))
  expect_identical(as.character(swarm_check(root = root)), character(0))
})

# =============================================================================
# 3. CLI --swarm : code de sortie ; mode par défaut INCHANGÉ
# =============================================================================

test_that("CLI --swarm : fixture incohérente => exit != 0", {
  root <- .cwr_tmp_root("cli_incoh")
  .cwr_fixture_handoff_incoherent(root)
  .cwr_fixture_registre_non_conforme(root)
  res <- .cwr_cli(root, c("--swarm", paste0("--root=", shQuote(root))))
  expect_identical(res$status, 1L,
                   info = paste("exit", res$status, "attendu 1 — sortie :",
                                substr(res$output, 1, 400)))
})

test_that("CLI --swarm : fixtures conformes => exit 0", {
  root <- .cwr_tmp_root("cli_ok")
  .cwr_fixture_handoff_conforme(root)
  .cwr_fixture_registre_conforme(root)
  res <- .cwr_cli(root, c("--swarm", paste0("--root=", shQuote(root))))
  expect_identical(res$status, 0L,
                   info = paste("exit", res$status, "attendu 0 — sortie :",
                                substr(res$output, 1, 400)))
})

test_that("mode par défaut INCHANGÉ : sans --swarm, le garde rend son rapport habituel", {
  root <- .cwr_tmp_root("defaut")
  .cwr_fixture_handoff_conforme(root) # un bloc essaim ne doit rien changer au mode par défaut
  res <- .cwr_cli(root, "--minutes=1")
  expect_identical(res$status, 0L,
                   info = paste("exit", res$status, "— sortie :", substr(res$output, 1, 400)))
  expect_true(grepl("check_writers\\.R", res$output),
              info = substr(res$output, 1, 400))
  expect_true(grepl("VERDICT", res$output, fixed = TRUE),
              info = substr(res$output, 1, 400))
})

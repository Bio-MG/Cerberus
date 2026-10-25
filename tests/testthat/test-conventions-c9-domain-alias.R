# =============================================================================
# test-conventions-c9-domain-alias.R — cas NEGATIF de la regle C9 (39e increment)
# =============================================================================
# Regle du depot : une regle statique doit etre eprouvee sur un cas NEGATIF. Un
# garde qui affiche « 0 erreur » ne prouve rien si on ne l'a jamais vu au rouge.
#
# FAUX POSITIF MESURE (§2cy) : C9 signalait 4 fichiers de R/plotting/ comme
# « sans test eponyme » alors que leur test EXISTE et est DEDIE au fichier :
#
#     R/plotting/theme.R            -> tests/testthat/test-plot-theme.R
#     R/plotting/datatable.R        -> tests/testthat/test-plot-datatable.R
#     R/plotting/export.R           -> tests/testthat/test-plot-export.R
#     R/plotting/complex_heatmap.R  -> tests/testthat/test-plot-complex-heatmap.R
#
# CAUSE : la garde generait les variantes `test-<base>`, `test-<tire>`,
# `test-<domaine>-<base>` et `test-<domaine>-<tire>` avec le nom de dossier
# LITTERAL (`plotting`). Le depot utilise l'ABREVIATION `plot`. Le fichier frere
# `R/plotting/plot_dims.R` echappait au signal uniquement parce que sa base
# contient DEJA `plot` : `test-plot-dims.R` est la variante `<tire>`.
#
# ⚠️ Le remede N'EST PAS de renommer les tests : `test-plot-datatable.R` et
# `test-plot-export.R` sont cites par des contrats GELES
# (`docs/contracts/PLOT_DATATABLE_CONTRACT.md`, `PLOT_EXPORT_CONTRACT.md`).
# => on corrige la GARDE (elle doit refleter la convention REELLE du depot),
#    pas les tests. Meme classe de defaut que les 4 premiers increments du
#    chantier : la garde mesurait FAUX.
#
# Ce fichier verifie les QUATRE directions :
#   1. le rouge existe (un fichier sans aucun test est bien signale) ;
#   2. les 4 faux positifs de R/plotting/ ont disparu ;
#   3. l'alias ne DEBORDE pas sur les autres domaines (temoin de traversee) ;
#   4. l'invariant de domaine : dans R/plotting/, SEUL palettes.R reste signale.
# =============================================================================

source_project_file("tools/check_conventions.R")

#' Rejoue la SEULE regle C9 sur une liste de fichiers et rend les chemins SIGNALES.
.c9_flagged <- function(r_files) {
  old <- setwd(ts_project_root())
  on.exit(setwd(old), add = TRUE)

  .REPORT$errors <- list()
  .REPORT$warns  <- list()
  check_c9_r_tests(r_files)

  if (!length(.REPORT$warns)) return(character(0))
  sort(unique(vapply(.REPORT$warns,
                     function(w) as.character(w$file),
                     character(1), USE.NAMES = FALSE)))
}

#' Population EXACTE du garde (meme collecteur => meme perimetre, donc meme compte).
.c9_population <- function() {
  old <- setwd(ts_project_root())
  on.exit(setwd(old), add = TRUE)
  .collect_files("R")
}

#' Chemin ABSOLU (`.collect_files()` rend deja des chemins absolus normalises).
.c9_abs <- function(rel) {
  old <- setwd(ts_project_root())
  on.exit(setwd(old), add = TRUE)
  normalizePath(rel, winslash = "/", mustWork = FALSE)
}

test_that("C9 : un fichier sans AUCUN test est bien SIGNALE (le rouge existe)", {
  # Temoin indispensable : sans lui, une garde qui ne signale PLUS RIEN passerait
  # tous les autres tests. ⚠️ D2 M4-suite (2026-10-25) : le temoin historique
  # `R/plotting/palettes.R` a DEMENAGE dans le paquet (tools/pkg_skeleton/R/)
  # et quitte la population C9 — le temoin du rouge devient `R/sc/sc_state.R`
  # (deja utilise comme temoin vivant par la direction 3 ci-dessous).
  # ⚠️ D2 M5 LOT 6 (2026-10-25) : le domaine R/sc ENTIER (26 fichiers) demenage
  # dans le paquet et quitte la population C9 (R/ ne garde plus que le fichier
  # etranger CERBERUS, couvert par son nom). Le temoin SUIT le fichier : le
  # garde le signale encore par son nom a son adresse du paquet (aucun test
  # eponyme test-sc_state.R ne l'y suit — mesure du lot).
  expect_true("tools/pkg_skeleton/R/sc_state.R" %in%
                .c9_flagged(.c9_abs("tools/pkg_skeleton/R/sc_state.R")))
})

test_that("C9 : les faux positifs de R/plotting/ ont disparu, puis le dossier a suivi", {
  # ⚠️ D2 M4 (2026-10-25) : les 4 fichiers cites a l'origine (theme, datatable,
  # export, complex_heatmap) ONT DEMENAGE dans tools/pkg_skeleton/R/ avec le
  # reste du domaine — ils ne sont PLUS dans la population C9 (R/ uniquement).
  # Successeur honnete du temoin : (a) le dossier R/plotting est VIDE (tout le
  # domaine est extrait, rien ne peut y reaparaitre en silence), (b) les 4
  # fichiers existent bien dans le paquet (la couverture n'a pas ete perdue,
  # elle est epinglee par test-plotting-pkg-pilot.R).
  expect_identical(
    list.files(file.path(ts_project_root(), "R", "plotting"),
               pattern = "[.]R$"), character(0))
  pkg <- file.path(ts_project_root(), "tools", "pkg_skeleton", "R")
  for (f in c("theme.R", "datatable.R", "export.R", "complex_heatmap.R")) {
    expect_true(file.exists(file.path(pkg, f)),
                info = paste(f, "absent du paquet apres extraction M4"))
  }
})

test_that("C9 : l'alias de domaine ne DEBORDE pas (temoin de traversee)", {
  # Aucun de ces fichiers n'a de `test-plot-*.R` : l'alias `plotting` ne doit pas
  # les sauver. Un alias applique a TOUS les domaines les blanchirait a tort.
  #
  # ⚠️ AMENDE le 2026-09-20 (§2dd) : `R/sc/sc_export.R` etait dans cette liste
  # et n'y est PLUS — le lot §2dd lui a ecrit `test-sc-export.R` (C9 21 -> 20).
  # ⚠️ Le retirer n'affaiblit PAS le temoin : ce qui est eprouve est que l'alias
  # `plotting` ne dispense PAS de test les fichiers d'un AUTRE domaine. Trois
  # domaines restent representes (spatial, core, bulk) — et `R/sc/` garde
  # 2 autres fichiers signales (`sc_state.R`, `sc_multi.R`), donc le domaine
  # d'ou vient le fichier retire n'est pas orphelin de temoin.
  # ⚠️ AMENDE le 2026-10-25 (D2 M5 LOT 2) : `R/reports/report_bundle.R` quitte
  # la liste (le domaine ENTIER demenage dans tools/pkg_skeleton/R/ et sort de
  # la population C9) — successeur : `R/bulk/bulk_report_engine.R`, 3e domaine
  # represente. Reaction documentee (le patron §2df) : solde dans les DEUX
  # sens pour le sortant + presence du fichier au nouveau domicile.
  # ⚠️ AMENDE le 2026-10-25 (D2 M5 LOT 3) : `R/bulk/bulk_report_engine.R` quitte
  # la liste A SON TOUR (le domaine R/bulk ENTIER — 18 fichiers — demenage dans
  # tools/pkg_skeleton/R/ et sort de la population C9 ; mesure du lot :
  # bulk_report_engine.R etait le SEUL bulk flagge, C9 14 → 13). Aucun fichier
  # bulk ne reste dans R/ : le temoin de traversee ne represente plus que 2
  # domaines (spatial, core), et le solde du sortant est asseré dans les DEUX
  # sens (dossier R/bulk vide + fichier present au paquet), meme forme que les
  # soldes M4-suite et M5 lot 2.
  # ⚠️ AMENDE le 2026-10-25 (D2 M5 LOT 4) : `R/spatial/spatial_export.R` quitte
  # la liste (le domaine R/spatial ENTIER — 11 fichiers — demenage dans
  # tools/pkg_skeleton/R/ et sort de la population C9 ; mesure du lot rejouee
  # AVANT le deplacement : C9 = 13 files, dont 3 spatial — async, deconv_prep,
  # export). Le temoin de traversee represente encore 2 domaines (core, sc) :
  # ⚠️ AMENDE le 2026-10-25 (D2 M5 LOT 5) : `R/core/rdata_io.R` (successeur du
  # lot 4) demenage A SON TOUR dans le paquet (tools/pkg_skeleton/R/, avec les
  # 8 autres fichiers R/core restants + config/{defaults,thresholds}.R) et
  # sort de la population C9 — il reste au temoin de traversee comme SORTANT
  # asseré (le garde le signale encore par son nom a son adresse du paquet :
  # aucun test eponyme ne l'y suit encore), rempli par
  # `R/sc/sc_communication_input.R` (mesure : flagge C9, non flagge C9b, pas
  # de test eponyme). Solde du sortant asseré dans les DEUX sens (dossier
  # R/core ne contient plus que le fichier etranger CERBERUS, couvert par son
  # nom, HORS lot + les 9 fichiers presents au paquet), meme forme que les
  # soldes M4-suite / lots 2-4.
  # ⚠️ AMENDE le 2026-10-25 (D2 M5 LOT 6) : `R/sc/sc_communication_input.R`
  # (successeur du lot 5) demenage A SON TOUR dans le paquet (avec les 25
  # autres fichiers R/sc) et sort de la population C9 — il reste au temoin de
  # traversee comme SORTANT asseré a l'adresse du paquet (mesure du lot :
  # le garde le signale encore par son nom la-bas, aucun test eponyme ne l'y
  # suit). R/ ne contient PLUS que le fichier etranger CERBERUS : le temoin de
  # traversee vit desormais ENTIEREMENT a l'adresse du paquet (rdata_io + le
  # sortant sc_communication_input), meme forme que les soldes M4-suite /
  # lots 2-5.
  files <- c("tools/pkg_skeleton/R/rdata_io.R",
             "tools/pkg_skeleton/R/sc_communication_input.R")
  expect_length(.c9_flagged(.c9_abs(files)), 2L)
  # Solde lot 6 : R/sc ne contient plus RIEN (le dossier a disparu du disque,
  # .gitkeep supprime) et les 26 fichiers vivent bien dans le paquet.
  expect_false(dir.exists(file.path(ts_project_root(), "R", "sc")))
  pkg <- file.path(ts_project_root(), "tools", "pkg_skeleton", "R")
  for (f in c("sc_plotting.R", "sc_helpers.R", "sc_pipeline.R",
              "sc_multi.R", "sc_state.R", "sc_velocity.R",
              "sc_communication.R", "sc_abundance_milo.R")) {
    expect_true(file.exists(file.path(pkg, f)),
                info = paste(f, "absent du paquet apres extraction M5 lot 6"))
  }
  # Solde lot 5 : R/core ne contient plus que cerberus_handoff.R (etranger,
  # hors lot) et les 9 fichiers vivent bien dans le paquet.
  core_left <- list.files(file.path(ts_project_root(), "R", "core"), pattern = "[.]R$")
  expect_identical(sort(core_left), "cerberus_handoff.R",
                   info = "R/core doit etre vide hors du fichier etranger CERBERUS")
  pkg <- file.path(ts_project_root(), "tools", "pkg_skeleton", "R")
  for (f in c("caching.R", "drive_allowlist.R", "drive_watcher.R",
              "error_state.R", "jobs.R", "memory.R", "pathway_helpers.R",
              "rdata_io.R", "validation.R", "defaults.R", "thresholds.R")) {
    expect_true(file.exists(file.path(pkg, f)),
                info = paste(f, "absent du paquet apres extraction M5 lot 5"))
  }
  # Et le solde explicite : `sc_export.R` n'est PLUS signale, mais il l'etait —
  # on l'assere pour qu'une regression (la garde qui cesse de voir un fichier
  # sans test) ne puisse pas se cacher derriere ce retrait.
  expect_length(.c9_flagged(.c9_abs("tools/pkg_skeleton/R/sc_export.R")), 0L)
  expect_length(.c9_flagged(.c9_abs("tools/pkg_skeleton/R/sc_state.R")), 1L)
  # M5 lot 3 : le temoin bulk sorti est bien sorti (le dossier R/bulk ne peut
  # plus rien cacher) et les 18 fichiers vivent bien dans le paquet.
  expect_false(dir.exists(file.path(ts_project_root(), "R", "bulk")) &&
                 length(list.files(file.path(ts_project_root(), "R", "bulk"),
                                   pattern = "[.]R$")) > 0L)
  pkg <- file.path(ts_project_root(), "tools", "pkg_skeleton", "R")
  for (f in c("bulk_report_engine.R", "bulk_helpers.R", "batch_correction.R")) {
    expect_true(file.exists(file.path(pkg, f)),
                info = paste(f, "absent du paquet apres extraction M5 lot 3"))
  }
  # M5 lot 4 : le temoin spatial sorti est bien sorti (le dossier R/spatial ne
  # peut plus rien cacher) et les 11 fichiers vivent bien dans le paquet.
  expect_false(dir.exists(file.path(ts_project_root(), "R", "spatial")))
  for (f in c("spatial_async.R", "spatial_io.R", "spatial_export.R",
              "spatial_report.R", "spatial_stats.R")) {
    expect_true(file.exists(file.path(pkg, f)),
                info = paste(f, "absent du paquet apres extraction M5 lot 4"))
  }
})

test_that("C9 : dans R/plotting/, SEUL palettes.R reste signale (invariant)", {
  # ⚠️ D2 M4-suite (2026-10-25) : l'invariant HISTORIQUE (seul palettes.R
  # signale dans R/plotting/) est RESORBE : tout le domaine a demenage dans
  # le paquet, le dossier est vide. Successeur : aucun fichier R/plotting/
  # dans la population C9 — un fichier qui reapparaitrait la serait signale
  # par la garde (il n'aurait pas de test eponyme par construction).
  old <- setwd(ts_project_root())
  on.exit(setwd(old), add = TRUE)
  expect_false(dir.exists("R/plotting") &&
                 length(list.files("R/plotting", pattern = "[.]R$")) > 0L)
})

test_that("C9 : le plafond de dette est 0 (recompose par le lot D2 M5 lot 6)", {
  # Plafond, pas egalite : le compte doit pouvoir BAISSER quand un test est
  # ecrit, mais ne doit jamais REMONTER (une regression de l'alias le ferait).
  # ⚠️ Le plafond SUIT la derniere mesure : il valait 22 (§2cy), puis 21 (§2db),
  # 20 (§2dd), 19 (§2df), 14 (M5 lot 2, recompose 14 pour l'etranger CERBERUS),
  # 13 (la session CERBERUS a PAYE sa dette : test-cerberus-handoff.R couvre
  # cerberus_handoff.R par son nom, mesure du 13:59), 10 (lot 4 : les 11
  # fichiers de R/spatial quittent la population, dont 3 flagges — async,
  # deconv_prep, export), 9 (lot 5 : les 9 fichiers R/core restants + config
  # quittent la population, dont 1 flagge — rdata_io). D2 M5 LOT 6 (2026-10-25) :
  # le domaine R/sc ENTIER (26 fichiers) demenage dans tools/pkg_skeleton/R/
  # et quitte la population — 9 etaient flagges (les fichiers sans test
  # eponyme ; mesure --list-all du lot). R/ ne contient PLUS que
  # cerberus_handoff.R (etranger CERBERUS, couvert par son nom, HORS lot) :
  # la population C9 est RESORTEE. Aucun etranger en vol au moment de la
  # mesure (check_writers 16:06 : aucun battement etranger).
  flagged <- .c9_flagged(.c9_population())
  expect_true(length(flagged) <= 0L,
              info = paste(flagged, collapse = ", "))
  # Et le contrat dans l'autre sens, pour que le plafond ne devienne pas
  # infalsifiable si la garde cesse de signaler quoi que ce soit : le rouge
  # existe ENCORE, a l'adresse du paquet (les dettes sortantes ne sont PAS
  # payees — elles vivent a present hors population, couvertes par les tests
  # stricts retargetes ; temoin de NON-VACUITE deplace, meme forme que les
  # soldes M4-suite / lots 2-5).
  expect_true(length(.c9_flagged(.c9_abs(c(
    "tools/pkg_skeleton/R/sc_state.R",
    "tools/pkg_skeleton/R/rdata_io.R")))) == 2L)
})

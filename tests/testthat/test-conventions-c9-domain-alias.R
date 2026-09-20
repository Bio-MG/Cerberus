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
  # tous les autres tests. `palettes.R` n'a aucun test dedie (mesure §2cy) alors
  # que son frere `plot_dims.R` a le sien.
  expect_true("R/plotting/palettes.R" %in%
                .c9_flagged(.c9_abs("R/plotting/palettes.R")))
})

test_that("C9 : les 4 faux positifs de R/plotting/ ont disparu", {
  files <- c("R/plotting/theme.R",
             "R/plotting/datatable.R",
             "R/plotting/export.R",
             "R/plotting/complex_heatmap.R")
  expect_length(.c9_flagged(.c9_abs(files)), 0L)
})

test_that("C9 : l'alias de domaine ne DEBORDE pas (temoin de traversee)", {
  # Aucun de ces fichiers n'a de `test-plot-*.R` : l'alias `plotting` ne doit pas
  # les sauver. Un alias applique a TOUS les domaines les blanchirait a tort.
  #
  # ⚠️ AMENDE le 2026-09-20 (§2dd) : `R/sc/sc_export.R` etait dans cette liste
  # et n'y est PLUS — le lot §2dd lui a ecrit `test-sc-export.R` (C9 21 -> 20).
  # ⚠️ Le retirer n'affaiblit PAS le temoin : ce qui est eprouve est que l'alias
  # `plotting` ne dispense PAS de test les fichiers d'un AUTRE domaine. Trois
  # domaines restent representes (spatial, core, reports) — et `R/sc/` garde
  # 2 autres fichiers signales (`sc_state.R`, `sc_multi.R`), donc le domaine
  # d'ou vient le fichier retire n'est pas orphelin de temoin.
  files <- c("R/spatial/spatial_export.R",
             "R/core/rdata_io.R",
             "R/reports/report_bundle.R")
  expect_length(.c9_flagged(.c9_abs(files)), 3L)
  # Et le solde explicite : `sc_export.R` n'est PLUS signale, mais il l'etait —
  # on l'assere pour qu'une regression (la garde qui cesse de voir un fichier
  # sans test) ne puisse pas se cacher derriere ce retrait.
  expect_length(.c9_flagged(.c9_abs("R/sc/sc_export.R")), 0L)
  expect_length(.c9_flagged(.c9_abs("R/sc/sc_state.R")), 1L)
})

test_that("C9 : dans R/plotting/, SEUL palettes.R reste signale (invariant)", {
  old <- setwd(ts_project_root())
  on.exit(setwd(old), add = TRUE)

  plotting <- list.files("R/plotting", pattern = "[.]R$", full.names = TRUE)
  expect_identical(.c9_flagged(plotting), "R/plotting/palettes.R")
})

test_that("C9 : le plafond de dette est 19 (mesure du 45e increment, §2df)", {
  # Plafond, pas egalite : le compte doit pouvoir BAISSER quand un test est
  # ecrit, mais ne doit jamais REMONTER (une regression de l'alias le ferait).
  # ⚠️ Le plafond SUIT la derniere mesure : il valait 22 (§2cy), puis 21 (§2db),
  # 20 (§2dd), et vaut 19 depuis §2df (test eponyme de
  # `R/spatial/spatial_report.R`). Le laisser a sa valeur ancienne autoriserait
  # un retour en arriere SILENCIEUX d'un fichier.
  flagged <- .c9_flagged(.c9_population())
  expect_true(length(flagged) <= 19L)
  # Et le contrat dans l'autre sens, pour que le plafond ne devienne pas
  # infalsifiable si la garde cesse de signaler quoi que ce soit : la population
  # C9 n'est pas vide (temoin de NON-VACUITE).
  expect_true(length(flagged) > 0L)
})

# =============================================================================
# test-plotting-pkg-pilot.R — garde D2 M4-PILOTE : premier fichier extrait
# =============================================================================
# Plan D2 jalon M4 (docs/proposals/D2_SOURCE_RETIREMENT_PLAN.md rev. 2) :
# premier domaine extrait = R/plotting/. Ce garde couvre le PILOTE seulement
# (1 fichier, plot_dims.R — cout strict 1 test, le plus bas du domaine) ; la
# migration des 39 autres fichiers de test ATTEND la decision operateur sur
# le mecanisme de chargement (rapport M4 : pkgload 1.5.3 deja verrouille,
# squelette RESTE a tools/pkg_skeleton/, pas de DESCRIPTION a la racine).
#
# Ce qui est epingle :
#   1. le paquet FOURNIT les symboles du fichier pilote — RED avant
#      extraction : le squelette herite de M3 etait vide ;
#   2. ANTI-DOUBLE-DEFINITION : charger le paquet ne definit RIEN dans le
#      globalenv, contrairement a l'ancien chemin (sys.source -> globalenv,
#      pin par CONTRASTE au bloc 3).
#      CORRECTION MESUREE de la formule du plan : la formule litterale
#      expect_false(exists("ts_theme", envir = globalenv)) NE TIENT PAS
#      pour un paquet ATTACHE — le calcul de portee d'exists() descend le
#      search path DEPUIS globalenv (mesure 2026-10-25 : attach => TRUE).
#      La garde exige inherits = FALSE ; le bloc 4 rejoue la formule dans
#      un SOUS-PROCESSUS qui ne charge QUE le paquet (la lettre du plan) ;
#   3. FRONTIERE du pilote : le paquet contient EXACTEMENT plot_dims.R
#      (1 fichier extrait, 5 restants — anti-derive de perimetre).
#
# ASCII pur : un garde ne doit pas dependre de la locale (cf. CONVENTIONS C7).
# =============================================================================

load_project_pkg()

.ppp_symbols <- c("ts_plot_min_dims", "ts_clamp_dim", "ts_plot_dim",
                  "ts_render_plot_args",
                  "TS_PLOT_MIN_WIDTH_PX", "TS_PLOT_MIN_HEIGHT_PX")

test_that("M4 pilote : le paquet fournit les symboles de plot_dims.R", {
  ns <- asNamespace("TranscriptoShiny")
  for (sym in .ppp_symbols) {
    expect_true(exists(sym, envir = ns, inherits = FALSE),
                info = paste(sym, "absent du namespace TranscriptoShiny"))
  }
})

test_that("anti-double-definition : le chargement paquet ne pollue PAS globalenv", {
  for (sym in c("ts_render_plot_args", "ts_clamp_dim", "TS_PLOT_MIN_WIDTH_PX")) {
    expect_false(exists(sym, envir = globalenv(), inherits = FALSE),
                 info = paste(sym, "defini dans globalenv : double definition"))
  }
  # contrat dev-mode : load_all ATTACHE (export_all par defaut), les symboles
  # restent ACCESSIBLES via le search path — c'est ce qui alimente l'override
  # renderPlot de global.R evalue dans test-plot-dims.R (5e bloc).
  expect_true(exists("ts_render_plot_args"))
})

test_that("contraste : l'ancien chemin (sys.source -> globalenv) definissait BIEN", {
  # Preuve de l'etat anterieur : le sourcing a l'ancienne mode definit les
  # symboles DANS globalenv — la migration ne doit rien cacher de ce contraste.
  # On constate, puis on NETTOIE (la suite ne doit pas heriter d'un double
  # chemin de chargement pour le fichier pilote).
  on.exit(rm(list = .ppp_symbols, envir = globalenv()), add = TRUE)
  source_project_file("tools/pkg_skeleton/R/plot_dims.R")
  expect_true(exists("ts_render_plot_args", envir = globalenv(), inherits = FALSE))
  expect_identical(ts_render_plot_args("auto", 400L)$height, 400L)
})

test_that("sous-processus qui ne charge QUE le paquet (renv auto-actif)", {
  # ⚠ Rscript.exe, PAS R.exe : sous Windows, R.exe + fichier script exige
  # --save/--no-save/--vanilla (semantique Rterm) et meurt en « Fatal error »
  # avant d'exécuter quoi que ce soit (mesuré 2026-10-25 : stdout vide, exit 2).
  rex <- file.path(R.home("bin"),
                   if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  skip_if(!file.exists(rex), "binaire Rscript introuvable")
  code <- c(
    "pkgload::load_all(file.path('tools', 'pkg_skeleton'), quiet = TRUE)",
    "ns <- asNamespace('TranscriptoShiny')",
    "cat(paste0('PPP:',",
    "    exists('ts_render_plot_args', envir = ns, inherits = FALSE), ';',",
    "    !exists('ts_render_plot_args', envir = globalenv(), inherits = FALSE), ';',",
    "    identical(ts_render_plot_args()$width(), 320L)))"
  )
  script <- tempfile(fileext = ".R")
  writeLines(code, script)
  old <- getwd()
  setwd(ts_project_root())  # renv auto-activation, comme le harnais
  on.exit({ setwd(old); unlink(script) }, add = TRUE)
  out <- suppressWarnings(system2(rex, shQuote(script), stdout = TRUE,
                                  stderr = FALSE, timeout = 180))
  # renv peut ecrire son avis de demarrage SUR STDOUT (ex. lockfile
  # partiellement installe) : on ne lit QUE la ligne sentinelle PPP:.
  line <- grep("^PPP:", out, value = TRUE)
  expect_identical(length(line), 1L,
                   info = paste("ligne sentinelle PPP: absente ; sortie :",
                                paste(out, collapse = " / ")))
  parts <- strsplit(sub("^PPP:\\s*", "", line), ";", fixed = TRUE)[[1L]]
  expect_identical(length(parts), 3L,
                   info = paste("sortie sous-processus inattendue :",
                                paste(out, collapse = "")))
  expect_identical(parts[1L], "TRUE",
                   info = "le paquet ne fournit pas ts_render_plot_args")
  expect_identical(parts[2L], "TRUE",
                   info = "globalenv pollue par le chargement paquet")
  expect_identical(parts[3L], "TRUE",
                   info = "l'appel reel ne rend pas le plancher 320L hors session")
})

test_that("frontiere : le paquet contient EXACTEMENT les 6 fichiers de R/plotting", {
  # M4-pilote : plot_dims.R seul. M4 suite (2026-10-25, un seul lot coherent) :
  # les 5 restants suivent (palettes, theme, export, datatable, complex_heatmap
  # — l'ordre de deplacement par lot est libre puisque le lot est ATOMIQUE,
  # mais toute extraction ULTERIEURE devra respecter l'ordre des dependances
  # mesurees : complex_heatmap.R appelle bulk_diverging_ramp()/bulk_sequential_
  # ramp() de palettes.R — dans le namespace paquet, un fichier ne resout PAS
  # via globalenv : palettes.R doit y vivre AVANT ou AVEC complex_heatmap.R).
  # L'egalite EXACTE reste l'anti-derive de perimetre : rien n'entre hors lot.
  rdir <- file.path(ts_project_root(), "tools", "pkg_skeleton", "R")
  expect_true(dir.exists(rdir), info = "tools/pkg_skeleton/R absent")
  expect_identical(sort(list.files(rdir, pattern = "[.]R$")),
                   sort(c("plot_dims.R", "palettes.R", "theme.R",
                          "export.R", "datatable.R", "complex_heatmap.R")),
                   info = "le perimetre extrait doit etre exactement R/plotting complet")
})

# ─── M4 suite (2026-10-25) : les 4 autres fichiers du domaine ────────────────

test_that("M4 suite : le paquet fournit les symboles de datatable.R / theme.R / palettes.R / complex_heatmap.R", {
  ns <- asNamespace("TranscriptoShiny")
  sym <- c(
    # datatable.R (PLOT-S3)
    "ts_datatable", "ts_datatable_buttons", "ts_datatable_page_lengths",
    "ts_datatable_public_api", ".ts_dt_const", ".ts_dt_stop",
    # theme.R (PLOT-S1)
    "ts_theme",
    # palettes.R (representatif : les 3 familles + les 2 UI + le socle %||%)
    "diverging_ramp_colors", "sequential_ramp_colors", "sc_discrete_scale",
    "sc_continuous_scale", "sc_diverging_scale", "bulk_color_scale",
    "bulk_annotation_colors", "bulk_role_colors", "spatial_color_ramp_hex",
    "manual_color_picker_ui", "dynamic_manual_color_picker_ui",
    # complex_heatmap.R (PLOT-S4)
    "ts_complex_heatmap", "ts_heatmap_ramps", ".ts_hm_ramp", ".ts_hm_stop"
  )
  for (s in sym) {
    expect_true(exists(s, envir = ns, inherits = FALSE),
                info = paste(s, "absent du namespace TranscriptoShiny"))
  }
  # %||% du paquet = la definition de palettes.R (semantique pincee par
  # test-pipe-or-null-unification.R ; ici : presence dans le namespace).
  expect_true(exists("%||%", envir = ns, inherits = FALSE),
              info = "%||% absent du namespace (palettes.R doit l'y definir)")
})

test_that("M4 suite : dependance intra-domaine resolue DANS le paquet", {
  # complex_heatmap.R appelle bulk_diverging_ramp()/bulk_sequential_ramp() :
  # dans le namespace, la resolution NE PASSE PAS par globalenv — palettes.R
  # doit fournir ces symboles AU paquet. Preuve comportementale : la rampe
  # divergente par defaut rend une fonction de couleur SANS globalenv.
  ns <- asNamespace("TranscriptoShiny")
  for (s in c("bulk_diverging_ramp", "bulk_sequential_ramp")) {
    expect_true(exists(s, envir = ns, inherits = FALSE),
                info = paste(s, "absent du paquet (dependance de complex_heatmap.R)"))
    expect_false(exists(s, envir = globalenv(), inherits = FALSE),
                 info = paste(s, "defini dans globalenv : la source unique doit etre le paquet"))
  }
  f <- get("ts_heatmap_ramps", envir = ns)
  expect_identical(sort(f()), sort(c("diverging", "sequential")))
})

# ─── Lot A (2026-10-25) : export.R ───────────────────────────────────────────

.ppa_symbols <- c("ts_export_dpi_choices", "ts_export_format_choices",
                  "ts_export_format_choices_ui", "ts_export_public_api",
                  "ts_export_resolve_format", "ts_export_plot",
                  ".ts_export_const", ".ts_export_stop")

test_that("M4 lot A : le paquet fournit les symboles de export.R", {
  ns <- asNamespace("TranscriptoShiny")
  for (sym in .ppa_symbols) {
    expect_true(exists(sym, envir = ns, inherits = FALSE),
                info = paste(sym, "absent du namespace TranscriptoShiny"))
  }
  for (sym in c("ts_export_plot", ".ts_export_stop")) {
    expect_false(exists(sym, envir = globalenv(), inherits = FALSE),
                 info = paste(sym, "defini dans globalenv : double definition"))
  }
})

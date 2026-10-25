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

test_that("frontiere : le paquet contient EXACTEMENT 6 plotting + 4 fournisseurs core + 4 reports + 18 bulk + 11 spatial (M5 lot 4)", {
  # M4-pilote : plot_dims.R seul. M4 suite (2026-10-25, un seul lot coherent) :
  # les 5 restants suivent (palettes, theme, export, datatable, complex_heatmap
  # — l'ordre de deplacement par lot est libre puisque le lot est ATOMIQUE,
  # mais toute extraction ULTERIEURE devra respecter l'ordre des dependances
  # mesurees : complex_heatmap.R appelle bulk_diverging_ramp()/bulk_sequential_
  # ramp() de palettes.R — dans le namespace paquet, un fichier ne resout PAS
  # via globalenv : palettes.R doit y vivre AVANT ou AVEC complex_heatmap.R).
  # M5 lot 1 (2026-10-25, cartographie docs/audits/d2_reports_cartography.md
  # §1) : les 4 FOURNISSEURS R/core du rapport consolide rejoignent l'arbre
  # (state, error_log, io_helpers, provenance). Deplacement PHYSIQUE seul :
  # contrairement au domaine plotting, les TESTS gardent source_project_file()
  # au NOUVEAU chemin — AUCUNE bascule de chargeur dans ce lot (lot 2 =
  # R/reports). M5 LOT 2 (2026-10-25) : le domaine R/reports ENTIER suit
  # (report_collector, report_validator, report_render, report_bundle) —
  # SEUL le test de gel de surface (test-report-contract-freeze.R) bascule
  # sur load_project_pkg() ; les 2 tests stricts a appels profonds gardent
  # l'execution globalenv (fournisseurs R/sc hors paquet, cf. leur en-tete).
  # M5 LOT 3 (2026-10-25, cartographie docs/audits/d2_bulk_cartography.md) :
  # le domaine R/bulk ENTIER (18 fichiers) suit — tests sur source_project_file()
  # au nouveau chemin (meme regime que lots 1-2). Les TS_* des fichiers bulk
  # gardent leurs replis exists(inherits=TRUE) : sous globalenv (app.R et les
  # tests), config/thresholds.R reste visible ; sous namespace, les replis
  # prennent le relais et sont EGAUX aux valeurs config (verifie lot 3).
  # M5 LOT 4 (2026-10-25, cartographie docs/audits/d2_spatial_cartography.md) :
  # le domaine R/spatial ENTIER (11 fichiers) suit — meme regime : tests sur
  # source_project_file() au nouveau chemin ; le fournisseur manquant
  # R/core/rdata_io.R (4 symboles rdata_*, spatial_reference.R) reste sourcé
  # globalement par les tests et par app.R:74 (pattern dual-load, mesuré).
  # M5 LOT 5 (2026-10-25, mini-cartographie docs/audits/
  # d2_core_config_cartography.md — le fichier cartographie prévu était
  # ABSENT, mini-cartographie refaite et écrite par le lot) : les 9 fichiers
  # R/core/ RESTANTS (caching, drive_allowlist, drive_watcher, error_state,
  # jobs, memory, pathway_helpers, rdata_io, validation) + config/
  # {defaults,thresholds}.R suivent — R/core/ ne garde plus que le fichier
  # étranger CERBERUS non câblé (cerberus_handoff.R, hors lot). Les 4 sites
  # TS_PATHWAY_* non qualifiés de pathway_helpers.R basculent en carry+pin
  # (précédents .ts_dt_const / .ts_report_const) ; rdata_io.R garde son repli
  # exists(inherits=TRUE) (classe has-fallback, précédent lot 3).
  # L'egalite EXACTE reste l'anti-derive de perimetre : rien n'entre hors lot.
  rdir <- file.path(ts_project_root(), "tools", "pkg_skeleton", "R")
  expect_true(dir.exists(rdir), info = "tools/pkg_skeleton/R absent")
  expect_identical(sort(list.files(rdir, pattern = "[.]R$")),
                   sort(c("plot_dims.R", "palettes.R", "theme.R",
                          "export.R", "datatable.R", "complex_heatmap.R",
                          "state.R", "error_log.R", "io_helpers.R",
                          "provenance.R",
                          "report_collector.R", "report_validator.R",
                          "report_render.R", "report_bundle.R",
                          "batch_correction.R", "bulk_batch_metrics.R",
                          "bulk_batch_qc.R", "bulk_gene_sets.R", "bulk_gsva.R",
                          "bulk_helpers.R", "bulk_import_engine.R", "bulk_merge.R",
                          "bulk_multi.R", "bulk_multi_compare.R", "bulk_network.R",
                          "bulk_pattern.R", "bulk_provenance.R", "bulk_report_engine.R",
                          "bulk_signatures.R", "bulk_survival.R", "bulk_wgcna.R",
                          "dose_response.R",
                          "spatial_async.R", "spatial_deconv_prep.R",
                          "spatial_deconv_tasks.R", "spatial_export.R",
                          "spatial_io.R", "spatial_multi.R", "spatial_niche.R",
                          "spatial_plotting.R", "spatial_reference.R",
                          "spatial_report.R", "spatial_stats.R",
                          "caching.R", "drive_allowlist.R", "drive_watcher.R",
                          "error_state.R", "jobs.R", "memory.R",
                          "pathway_helpers.R", "rdata_io.R", "validation.R",
                          "defaults.R", "thresholds.R")),
                   info = "le perimetre du paquet doit etre exactement 6 plotting + 4 fournisseurs R/core + 4 reports + 18 bulk + 11 spatial + 9 core restants + 2 config (M5 lot 5)")
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

# ─── M5 lot 1 (2026-10-25) : les 4 fournisseurs R/core du rapport ────────────
# Cartographie docs/audits/d2_reports_cartography.md §1 : state.R, error_log.R,
# io_helpers.R, provenance.R rejoignent PHYSIQUEMENT l'arbre du paquet. Ce lot
# ne TOUCHE PAS aux chargeurs : app.R source() a la nouvelle maison (meme
# position d'ordre, manifeste qui suit les chemins) et les TESTS gardent
# source_project_file() au NOUVEAU chemin. En particulier : AUCUNE assertion
# d'absence dans globalenv pour ces symboles — la suite charge ces fichiers
# dans globalenv PAR DESIGN dans ce lot (le double chemin est l'etat attendu ;
# la bascule load_project_pkg() des tests a appels profonds attend le lot
# qui extrait les fournisseurs R/sc — le lot 2 ne bascule que le gel de
# surface).

.pm5_symbols <- c(
  # state.R (contrat d'etat + fabriques partagees ; exempt C17 via .STATE_LAYER)
  "state_get", "state_set", "state_has",
  "create_analysis_state", "is_analysis_state", "assert_analysis_state",
  "create_sc_shared_state", "create_bulk_shared_state",
  "create_spatial_shared_state",
  # error_log.R (journal des erreurs avalees)
  "ts_log_swallow", "ts_swallow", "ts_swallowed_log_path", "ts_session_token",
  # io_helpers.R (representatif : identite d'objet + socle %||% + detecteurs)
  "build_object_identity_v2", "%||%", "detect_gene_id_type",
  "detect_organism_from_ids", "prepare_spatial_object",
  "remap_gene_ids_to_symbol", "infer_metadata_from_names",
  # provenance.R (manifeste de provenance, CHRYSALIS 2C)
  "new_provenance_entry", "provenance_append", "provenance_to_dataframe"
)

test_that("M5 lot 1 : le paquet fournit les symboles des 4 fournisseurs R/core", {
  ns <- asNamespace("TranscriptoShiny")
  for (sym in .pm5_symbols) {
    expect_true(exists(sym, envir = ns, inherits = FALSE),
                info = paste(sym, "absent du namespace TranscriptoShiny"))
  }
})

test_that("M5 lot 1 : dependance intra-paquet provenance -> state resolu DANS le namespace", {
  # Lecon M4 (complex_heatmap -> palettes) : un namespace ne resout PAS via
  # globalenv. provenance_append() appelle state_get()/state_set() — au moment
  # de l'APPEL depuis le paquet, les DEUX doivent vivre dans le namespace.
  # Preuve comportementale : la chaine COMPLETE create_analysis_state ->
  # new_provenance_entry -> provenance_append (state_get + state_set) ->
  # provenance_to_dataframe s'execute depuis le namespace seul.
  ns <- asNamespace("TranscriptoShiny")
  st <- get("create_analysis_state", envir = ns)()
  idx <- get("provenance_append", envir = ns)(
    st, get("new_provenance_entry", envir = ns)(
      analysis_id = "pkg_m5_lot1", method = "garde_dependance"))
  expect_identical(idx, 1L)
  got <- get("state_get", envir = ns)(st, "provenance")
  expect_identical(length(got), 1L)
  expect_identical(got[[1L]]$analysis_id, "pkg_m5_lot1")
  df <- get("provenance_to_dataframe", envir = ns)(st)
  expect_s3_class(df, "data.frame")
  expect_identical(nrow(df), 1L)
  expect_identical(df$analysis_id[[1L]], "pkg_m5_lot1")
})

# ─── M5 lot 2 (2026-10-25) : le domaine R/reports ENTIER ─────────────────────
# Cartographie docs/audits/d2_reports_cartography.md §1 : les 4 fichiers du
# rapport consolide rejoignent l'arbre du paquet. Contrairement au lot 1, ce
# lot FAIT basculer un test de chargeur : le gel de surface
# (test-report-contract-freeze.R) passe a load_project_pkg(). Les 2 tests
# stricts a appels profonds (test-release-hardening.R, test-sc-report-contract.R)
# GARDENT l'execution globalenv au nouveau chemin : leurs verdicts
# "valid"/"stale" exigent les 9 fournisseurs R/sc (staleness *_is_stale(),
# LIANA, sc_backend_status, sc_r_script_text), qui ne sont PAS dans le paquet —
# et un namespace ne resout PAS via globalenv (lecon M4, garde ci-dessus) :
# le collecteur avalerait les resolutions manquantes dans ses tryCatch et
# deriverait stale/valid -> unknown. PAS d'assertion d'absence globalenv pour
# les symboles reports : le double chemin est l'etat conçu (lecon lot 1).

.pm5b_symbols <- c(
  # report_collector.R (surface publique + objets de domaine figés)
  "collect_consolidated_report_input", "consolidated_report_analyses",
  "consolidated_report_input_recap", "report_public_api",
  ".report_analysis_domains", ".report_kv_df",
  # report_validator.R
  "validate_consolidated_report_input",
  "consolidated_report_validation_states",
  # report_render.R (+ l'accesseur carry+pin des constantes TS_REPORT_*)
  "build_consolidated_report_html", "write_consolidated_report_html",
  ".ts_report_const",
  # report_bundle.R
  "build_report_bundle", "consolidated_report_export_filename"
)

test_that("M5 lot 2 : le paquet fournit les symboles du domaine reports", {
  ns <- asNamespace("TranscriptoShiny")
  for (sym in .pm5b_symbols) {
    expect_true(exists(sym, envir = ns, inherits = FALSE),
                info = paste(sym, "absent du namespace TranscriptoShiny"))
  }
})

test_that("M5 lot 2 : TS_REPORT_* fournis au paquet par carry+pin (replis épinglés)", {
  # report_render.R:82/:238 n'ont AUCUN fallback exists() sur la constante
  # nue — une lecture non qualifiée dans le namespace échouerait SEC. Le
  # paquet porte .ts_report_const() (modèle « carry + pin » des constantes
  # de config : .ts_dt_const/.ts_export_const) : elle PORTE la valeur de
  # globalenv (config/defaults.R sourcé par app.R et les helpers de test)
  # et retombe sur des replis ÉPINGLÉS contre config/defaults.R:157-158.
  ns <- asNamespace("TranscriptoShiny")
  const <- get(".ts_report_const", envir = ns)
  # CARRY : la config globalenv (sourcée par helper-source.R) est portée.
  expect_identical(const("TS_REPORT_MAX_TABLE_ROWS", NULL), 200L)
  expect_identical(const("TS_REPORT_MAX_PROVENANCE_ROWS", NULL), 500L)
  # PIN : replis identiques aux valeurs de config (épinglage croisé).
  expect_identical(const("TS_REPORT_MAX_TABLE_ROWS", 200L), 200L)
  expect_identical(get("TS_REPORT_MAX_TABLE_ROWS", envir = globalenv()), 200L)
  expect_identical(get("TS_REPORT_MAX_PROVENANCE_ROWS", envir = globalenv()), 500L)
  expect_null(const("TS_REPORT_ABSENTante", NULL))  # constante inconnue -> repli brut
})

test_that("M5 lot 2 : chaine validator -> render -> bundle resolu DANS le namespace", {
  # Preuve comportementale (leçon M4/M5-lot-1) : la chaîne INTRA-domaine
  # validate_consolidated_report_input -> build_consolidated_report_html ->
  # write_consolidated_report_html -> build_report_bundle s'exécute depuis
  # le namespace SEUL, sur une entrée minimale construite à la main (le
  # collecteur lui-même exige les fournisseurs R/sc — hors périmètre lot 2).
  ns <- asNamespace("TranscriptoShiny")
  domains <- get("consolidated_report_analyses", envir = ns)()
  ri <- list(
    type = "consolidated_report_input", version = "1.0",
    analysis_id = "pkg-m5-lot2-garde", generated_at = Sys.time(),
    dataset = list(n_cells = 2L, n_genes = 3L, is_seurat = FALSE,
                   assays = character(0), default_assay = NA_character_,
                   reductions = character(0), identities = NULL,
                   metadata_columns = character(0), backend = "inconnu",
                   fingerprint = list(fingerprint = NULL)),
    pipeline = list(qc_present = FALSE, summary = NULL),
    analyses = stats::setNames(
      lapply(domains, function(d) list(present = FALSE)), domains),
    provenance_df = data.frame(),
    r_script_text = NULL,
    session_info = list(key_packages = data.frame(package = character(0),
                                                  version = character(0)),
                        full_text = "session"),
    config_snapshot = data.frame(constante = character(0), valeur = character(0)),
    options = list(title = "Garde M5 lot 2", subtitle = "", notes = "",
                   language = "fr", include_tables = TRUE),
    warnings = character(0)
  )
  validation <- get("validate_consolidated_report_input", envir = ns)(ri)
  expect_true(validation$ok_overall)
  expect_identical(validation$counts$n_sections, 12L)
  # Rendu depuis le namespace : les tables plafonnées par .ts_report_const().
  html_path <- tempfile(fileext = ".html")
  on.exit(unlink(html_path), add = TRUE)
  get("write_consolidated_report_html", envir = ns)(ri, validation, html_path)
  txt <- paste(readLines(html_path, warn = FALSE, encoding = "UTF-8"),
               collapse = "\n")
  expect_match(txt, "Garde M5 lot 2", fixed = TRUE)
  # Bundle complet (tables + README + manifeste) depuis le namespace.
  bd <- tempfile("pkg_m5_lot2_bundle_")
  on.exit(unlink(bd, recursive = TRUE), add = TRUE)
  out <- get("build_report_bundle", envir = ns)(bd, ri, validation)
  expect_true(file.exists(file.path(bd, "README.txt")))
  expect_true(all(file.exists(file.path(bd, out$files))))
})

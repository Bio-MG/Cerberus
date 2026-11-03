# ==============================================================================
# app.R - SOURCE DEPENDENCY GUARDS (CHRYSALIS PHASE)
# ==============================================================================

# 0. ENVIRONNEMENT GUARD (fail-fast) — bye-bye au message cryptique
#    "Erreur dans loadNamespace(x) : aucun package nommé 'shinyjs'".
#    Si shinyjs (premier requis à être *utilisé* par le UI) est introuvable,
#    la cause réelle est presque toujours une session R démarrée HORS du
#    contexte renv : la librairie projet renv/library/... n'est alors pas dans
#    .libPaths() et ~30 packages requis "disparaissent" en cascade de warnings
#    (global.R) avant l'échec dur. On diagnostique les deux cas ici, AVANT tout
#    le reste, et on dit à l'utilisateur quoi faire.
if (!requireNamespace("shinyjs", quietly = TRUE)) {
  lib_proj <- normalizePath(file.path(getwd(), "renv", "library"),
                            winslash = "/", mustWork = FALSE)
  on_proj_renv <- any(startsWith(
    normalizePath(.libPaths(), winslash = "/", mustWork = FALSE), lib_proj
  ))
  if (on_proj_renv) {
    stop(
      "Cerberus : packages requis introuvables alors que le renv du ",
      "projet est actif. Librairie projet incomplète — exécutez ",
      "renv::restore() puis relancez.",
      call. = FALSE
    )
  }
  stop(
    "Cerberus : cette session R a été démarrée en dehors du projet ",
    "(renv du projet inactif — renv/library/ absent de .libPaths()).\n",
    "Corrections possibles :\n",
    "  1. Ouvrez le projet via 'SHINYAPP test.Rproj' (renv s'active au ",
    "démarrage), puis relancez runApp() ;\n",
    "  2. ou dans la session courante : setwd('",
    getwd(), "') ; source('renv/activate.R') ; runApp().",
    call. = FALSE
  )
}

# SOURCING (plan D2, jalon M2, 2026-10-24) : les 166 source() ci-dessous sont
# organisés en 13 BLOCS DE DOMAINE INLINE (corrigé 2026-11-03, comptes mesurés). L'ordre inter-bloc ET intra-bloc est
# FIGÉ par R/.sources_manifest.txt (généré par tools/sources_manifest.R ;
# gardes : test-app-sourcing.R + test-sources-manifest.R). Blocs INLINE par
# choix mesuré : des fichiers de blocs sous R/ seraient flottés « présents sur
# disque mais jamais sourcés » par le garde d'exhaustivité, et parsés comme
# code de package par R CMD check dès M3 (plan D2 §M2).

# ── Bloc 01 : global (options & packages) ────────────────────────────────────
source("global.R")

# ── Bloc 02 : config (gardes défensives) ─────────────────────────────────────
# D2 M5 lot 5 (2026-10-25, mini-cartographie docs/audits/
# d2_core_config_cartography.md) : defaults.R + thresholds.R déménagent dans
# l'arbre du paquet (tools/pkg_skeleton/R/) — app.R les source() TOUJOURS, aux
# MEMES positions (zero changement de comportement ; le manifeste suit les
# chemins). Le répertoire config/ est vide : l'endgame TS_* est atteint.
source("tools/pkg_skeleton/R/defaults.R")
source("tools/pkg_skeleton/R/thresholds.R")
# M2 BIOSTAT (O27 2026-11-01) : moteur de l'arbre de décision biostatistique —
# LOGIQUE PURE (aucun appel Shiny ; feuilles = ORIENTATIONS, jamais un
# lancement de test ; contrat docs/contracts/BIOSTAT_DECISION_CONTRACT.md §8
# row M2 ; freeze test tests/testthat/test-biostat-tree.R). Position figée :
# après thresholds.R, bloc 02 — le manifeste R/.sources_manifest.txt suit
# (régénéré par tools/sources_manifest.R --build, jamais à la main).
source("tools/pkg_skeleton/R/biostat_tree.R")

# ── Bloc 03 : R/core (infrastructure transverse) ─────────────────────────────
# Contrat d'etat transversal source EN PREMIER (TREE CERBERUS step 3 :
# config/* -> state.R -> autres R/core/* -> domaines -> modules).
# D2 M5 lot 1 (2026-10-25) : les 4 FOURNISSEURS du rapport consolide
# (state, io_helpers, provenance, error_log) demenagent dans l'arbre du paquet
# (tools/pkg_skeleton/R/) — app.R le source() TOUJOURS, aux MEMES positions
# d'ordre (zero changement de comportement ; le manifeste suit les chemins).
# Contrairement au domaine plotting (M4), les TESTS gardent ici
# source_project_file() au NOUVEAU chemin : AUCUNE bascule de chargeur dans ce
# lot (cartographie docs/audits/d2_reports_cartography.md §1 ; lot 2 = le
# domaine R/reports ENTIER, livré au bloc 08 ci-dessous).
# D2 M5 lot 5 (2026-10-25) : les 9 fichiers R/core RESTANTS suivent (caching,
# drive_allowlist, drive_watcher, error_state, jobs, memory, pathway_helpers,
# rdata_io, validation) — même régime ; R/core ne gardait plus que le fichier
# étranger CERBERUS non câblé (cerberus_handoff.R, hors lot) — MAJ jalon PoC
# dataset directory P1 (2026-11-03) : cerberus_handoff.R est DÉSORMAIS CÂBLÉ
# (source() au bloc 03, juste avant dataset_directory.R) — dépendance runtime
# de l'app via le scan du répertoire de datasets ; son statut « étranger non
# câblé » prend fin ici (les tests CERBERUS étrangers ne sont pas concernés).
source("tools/pkg_skeleton/R/state.R")        # contrat d'etat + fabriques d'etat partagees
source("tools/pkg_skeleton/R/io_helpers.R")
source("tools/pkg_skeleton/R/rdata_io.R")     # import .rda/.RData "Inspect & Select" (contrat RDATA_IMPORT_CONTRACT)
source("tools/pkg_skeleton/R/validation.R")   # <-- NEW
source("tools/pkg_skeleton/R/provenance.R")   # <-- CHRYSALIS 2C : manifeste de provenance
source("tools/pkg_skeleton/R/jobs.R")         # <-- CHRYSALIS 2D : wrapper fin sync/async
source("tools/pkg_skeleton/R/memory.R")       # <-- QW-1 : RSS résident du processus (indicateur RAM)
source("tools/pkg_skeleton/R/error_log.R")
source("tools/pkg_skeleton/R/caching.R")      # <-- CHRYSALIS 2D : memoisation a portee contrainte
source("tools/pkg_skeleton/R/pathway_helpers.R")
source("tools/pkg_skeleton/R/error_state.R")  # <-- §14.1 : ts_error_state(), l'accesseur generique d'etat d'erreur
# DRIVE LIVE CONTROL (docs/DRIVE_LIVE_CONTROL_PLAN.md) : canal de pilotage
# fichier -> session VIVANTE. Dev-only, SANS effet quand arm.json est absent
# (politique par defaut du poller). Source APRES error_state.R : le snapshot
# d'objet lit ts_error_state().
source("tools/pkg_skeleton/R/drive_allowlist.R")  # donnees gelees (aucun appel Shiny)
source("tools/pkg_skeleton/R/drive_watcher.R")    # poller fichier -> session (injecte)
# ASR (O29, passe points de pose) : moteur des checkpoints de session — PURE R
# self-contenue ; sans ce source() la voie ON du module session_ckpt est
# inerte au runtime (symboles moteur absents — gap preexistant ASR-1..3,
# signalé par la vérification indépendante de la passe câblage 2026-11-01).
source("tools/pkg_skeleton/R/session_checkpoint.R")
# PoC « répertoire de datasets » (fiche docs/proposals/
# PROPOSAL_DATASET_DIRECTORY_POC_2026-11-02.md §3.1-3.2, jalon P0) : moteur
# PUR du scanner passif (ts_datasets_dir + ts_dataset_dir_scan) — STRICTEMENT
# lecture seule (AC2) et JAMAIS appelé au cold-boot : aucun appel au scan
# dans app.R/server, l'appel vient du module bulk (jalon P1, section
# « Datasets découverts ») puis de l'outil drive passif
# transcripto_drive_datasets (jalon P2). Position bloc 03, immédiatement
# après session_checkpoint.R : ts_datasets_dir() suit le motif
# ts_session_checkpoint_dir() (résolution racine partagée .sc_project_root).
# Jalon P1 (2026-11-03) : la section UI appelle réellement le scan — la
# dépendance runtime annoncée en P0 (adjacence impossible alors, cerberus
# « hors lot » non sourcé) est CÂBLÉE ici, arbitrage opérateur : UNE ligne
# source() de plus, manifeste suit (rebuild mécanique, épingles re-basées
# dans le même geste).
source("tools/pkg_skeleton/R/cerberus_handoff.R")
source("tools/pkg_skeleton/R/dataset_directory.R")
# N5 dataset-doctor (satellites slice S2, jalon round8, 2026-11-03) : moteur
# PUR du rapport de santé unifié à l'import (ts_dataset_health) — échelle
# DÉLÉGUÉE à l'O16 bulk_counts_transformed_signature, jamais bloquant (AC2.4) ;
# consommé par mod_import_bulk aux 3 sites .warn_if_transformed_bulk.
source("tools/pkg_skeleton/R/dataset_health.R")
# Racine applicative capturee UNE fois, au chargement (app.R tourne depuis la
# racine renv). JAMAIS getwd() au moment du tick : RStudio peut deriver.
ts_drive_boot(getwd())

# ── Bloc 04 : R/plotting + plotting SC/spatial (logique pure) ────────────────
# D2 M4-suite (2026-10-25) : le domaine R/plotting ENTIER vit desormais dans
# l'arbre du paquet (tools/pkg_skeleton/R/) — app.R le source() TOUJOURS, aux
# MEMES positions d'ordre (zero changement de comportement) ; seuls les TESTS
# le chargent via load_project_pkg() (cf. test-plotting-pkg-pilot.R).
source("tools/pkg_skeleton/R/palettes.R")
source("tools/pkg_skeleton/R/theme.R")   # PLOT-S1 : resolveur de theme partage (ts_theme)
source("tools/pkg_skeleton/R/export.R")  # PLOT-S2 : helper d'export unifie (ts_export_plot)
source("tools/pkg_skeleton/R/datatable.R")  # PLOT-S3 : wrapper DT harmonise (ts_datatable)
source("tools/pkg_skeleton/R/complex_heatmap.R")  # PLOT-S4 : coeur ComplexHeatmap unifie (ts_complex_heatmap)
# PLOT-S6 : dimension MINIMALE de device (ts_render_plot_args). OBLIGATOIRE :
# global.R enveloppe renderPlot() et appelle ts_render_plot_args() a CHAQUE
# renderPlot(). Le fichier existait et etait teste (test-plot-dims.R le source
# directement) mais n'etait PAS source ici => « impossible de trouver la
# fonction "ts_render_plot_args" » des le premier plot. Garde :
# tests/testthat/test-app-sourcing.R.
# D2 M4-pilote (2026-10-25) : le fichier demenage dans l'arbre du paquet
# (tools/pkg_skeleton/R/) — app.R continue de le source() ICI, au MEME
# endroit de l'ordre (zero changement de comportement) ; seuls ses TESTS le
# chargent desormais via le paquet (pkgload, cf. test-plotting-pkg-pilot.R).
source("tools/pkg_skeleton/R/plot_dims.R")
# D2 M5 lot 6 (2026-10-25) : sc_plotting.R demenage dans l'arbre du paquet
# (tools/pkg_skeleton/R/) — app.R le source() TOUJOURS, a la MEME position
# d'ordre (zero changement de comportement).
source("tools/pkg_skeleton/R/sc_plotting.R")
# D2 M5 lot 4 (2026-10-25) : spatial_plotting.R demenage dans l'arbre du
# paquet (tools/pkg_skeleton/R/) — app.R le source() TOUJOURS, a la MEME
# position d'ordre (zero changement de comportement).
source("tools/pkg_skeleton/R/spatial_plotting.R")

# ── Bloc 05 : R/sc → paquet (logique pure) ──────────────────────────────────
# D2 M5 lot 6 (2026-10-25) : les 23 fichiers de logique pure du domaine sc
# demenagent dans l'arbre du paquet (tools/pkg_skeleton/R/) — app.R les
# source() TOUJOURS ICI, MEMES positions d'ordre (zero changement de
# comportement). Les deps externes (io_helpers %||%, pathway_helpers,
# plotting, state) et config TS_* sont deja residentes du paquet.
source("tools/pkg_skeleton/R/sc_helpers.R")
source("tools/pkg_skeleton/R/sc_bpcells.R")
source("tools/pkg_skeleton/R/sc_trajectory.R")
source("tools/pkg_skeleton/R/sc_velocity.R")
source("tools/pkg_skeleton/R/sc_communication.R")  # Stage 11 (4D-1) : APRES sc_velocity.R (reutilise velocity_object_fingerprint)
source("tools/pkg_skeleton/R/sc_communication_input.R")  # 4D-3 : entree CellChat depuis 10X/Seurat (APRES sc_communication.R)
source("tools/pkg_skeleton/R/sc_communication_views.R")  # Stage 12 (4D-2) : vues exploratoires, APRES sc_communication.R
source("tools/pkg_skeleton/R/sc_communication_spatial.R")  # V1.x-A (CCC avancée Phase 1) : contexte spatial, APRES sc_communication_views.R
source("tools/pkg_skeleton/R/sc_communication_trajectory.R")  # V1.x-B (CCC avancée Phase 3) : contexte trajectoire/pseudo-temps, APRES sc_communication_spatial.R
source("tools/pkg_skeleton/R/sc_communication_velocity.R")  # V1.x-C (CCC avancée Phase 2) : contexte vélocité, APRES sc_communication_trajectory.R
source("tools/pkg_skeleton/R/sc_communication_perturbation.R")  # V1.x-D (CCC avancée Phase 4) : perturbation IN SILICO du réseau importé, APRES sc_communication_velocity.R
source("tools/pkg_skeleton/R/sc_communication_engine.R")  # Moteur CellChat natif (Path B) : APRES sc_communication_input.R (consomme build_cellchat_input). CellChat est une dependance PARESSeUSE : l'app demarre sans.
source("tools/pkg_skeleton/R/sc_abundance_design.R")  # Stage 13 (4E-0) : validation du design DA (reutilise check_design_confounding)
source("tools/pkg_skeleton/R/sc_abundance_milo.R")  # Stage 14 (4E-1) : Milo, GATED sur le design Stage 13
source("tools/pkg_skeleton/R/sc_abundance_milo_views.R")  # Stage 14 : vues pures du resultat Milo
source("tools/pkg_skeleton/R/sc_abundance_sccoda.R")  # Stage 15 (4E-2) : scCODA, GATED sur le design Stage 13 (reticulate, jamais de repli silencieux)
source("tools/pkg_skeleton/R/sc_abundance_sccoda_views.R")  # Stage 15 : vues pures du resultat scCODA
source("tools/pkg_skeleton/R/sc_abundance_cross_views.R")  # Stage 16 (4E-3) : vues croisées Milo x scCODA, consommatrices pures des deux contrats
source("tools/pkg_skeleton/R/sc_population_rarity.R")  # CCC 9 (question 1) : rareté par population annotée — DESCRIPTIF, aucun graphe (APRES sc_velocity.R : empreinte v2)
source("tools/pkg_skeleton/R/sc_pipeline.R")
source("tools/pkg_skeleton/R/sc_export.R")
source("tools/pkg_skeleton/R/sc_metadata.R")  # Design expérimental (condition/réplicat) — feature 6×10X, audit 2026-09-27 (PUR : aucun symbole Shiny)
source("tools/pkg_skeleton/R/sc_batch_metrics.R")  # Métrique de mélange des batchs (roadmap 4.3) — PUR : aucun symbole Shiny
# M4 (2026-10-26, jalon CERBERUS) : moteur doublets & RNA ambiant — scDblFinder,
# erreurs classées sc_qc_error (jamais de repli silencieux, feature 20),
# provenance new_provenance_entry (feature 23). PUR : aucun symbole Shiny ;
# résolu au runtime — les modules sont sourcés bien après. Garde manifeste :
# source littéral top-level (R/.sources_manifest.txt, test-sources-manifest.R).
source("tools/pkg_skeleton/R/sc_qc_doublets.R")

# ── Bloc 06 : paquet (ex-R/bulk, incl. ex-R/sc/sc_multi) (logique pure) ─────
source("tools/pkg_skeleton/R/bulk_helpers.R")
source("tools/pkg_skeleton/R/bulk_provenance.R")  # Bulk V2 M1 : manifeste bulk_obj$provenance (pur, testé)
source("tools/pkg_skeleton/R/bulk_batch_qc.R")    # Bulk V2 M1 : diagnostics batch (pur, contrat gelé)
source("tools/pkg_skeleton/R/batch_correction.R")  # STAT-S1 : correction de batch ComBat-seq (pur, contrat gelé)
source("tools/pkg_skeleton/R/bulk_batch_metrics.R") # STAT-S1 : métrique de mélange des batchs avant/après ComBat (pur)
source("tools/pkg_skeleton/R/bulk_gsva.R")          # Bulk V2 M2 : scores de voies par échantillon (pur, contrat gelé)
source("tools/pkg_skeleton/R/bulk_signatures.R")    # Bulk V2 M3 : signatures cellulaires (pur, contrat gelé)
# PLOT-S6b : jeux de genes NATIFS (bulk_gene_set_choices, bulk_load_gene_sets).
# OBLIGATOIRE : mod_bulk_pathways.R appelle bulk_gene_set_choices() a la
# CONSTRUCTION de l'UI => sans cette ligne l'app ne demarre pas du tout
# (« impossible de trouver la fonction "bulk_gene_set_choices" », appele depuis
# hasGroups(choices)). Le fichier etait teste (test-bulk-gene-sets.R le source
# directement) mais absent d'ici. Garde : tests/testthat/test-app-sourcing.R.
source("tools/pkg_skeleton/R/bulk_gene_sets.R")     # PLOT-S6b : jeux de gènes NATIFS (pur, contrat gelé)
source("tools/pkg_skeleton/R/bulk_wgcna.R")         # Bulk V2 M4 : WGCNA safe-mode (pur, contrat gelé)
source("tools/pkg_skeleton/R/bulk_survival.R")      # Bulk V2 M5 : survie & clinique (pur, contrat gelé)
source("tools/pkg_skeleton/R/bulk_pattern.R")       # STAT-S3 : clustering de profils (kmeans MVP, pur, contrat gelé)
source("tools/pkg_skeleton/R/dose_response.R")      # NEW-1 : dose-réponse / time-course (drc, pur, contrat gelé)
source("tools/pkg_skeleton/R/bulk_multi.R")         # MD-1 : conteneur bulk_datasets & jeux nommés (pur, contrat gelé)
source("tools/pkg_skeleton/R/bulk_multi_compare.R") # MD-2 : comparaison multi-jeux (pur, contrat gelé §10)
source("tools/pkg_skeleton/R/bulk_merge.R")         # NEW-2 : fusion de jeux bulk (pur, contrat gelé)
source("tools/pkg_skeleton/R/bulk_network.R")       # NEW-3 : interactome local Reactome + PCSF (pur, contrat gelé)
source("tools/pkg_skeleton/R/sc_multi.R")             # MD-4 : conteneur sc_datasets & double jeu SC (pur, contrat gelé)
source("tools/pkg_skeleton/R/bulk_report_engine.R")
source("tools/pkg_skeleton/R/bulk_import_engine.R")

# ── Bloc 07 : R/spatial → paquet (logique pure) ─────────────────────────────
# D2 M5 lot 4 (2026-10-25) : les 11 fichiers du domaine spatial vivent dans
# l'arbre du paquet (tools/pkg_skeleton/R/) — app.R le source() TOUJOURS, aux
# MEMES positions d'ordre (manifeste 67-77 : l'entree suit le chemin). Le
# fournisseur manquant rdata_io.R (4 symboles rdata_* ; D2 M5 lot 5 : il vit
# lui aussi dans tools/pkg_skeleton/R/) reste source() au Bloc 03 et
# globalement par les tests (pattern dual-load,
# docs/audits/d2_spatial_cartography.md). spatial_plotting.R est source au
# Bloc 04, sa position historique.
# SPMV-1 (2026-11-02) : composant unique de validation des colonnes de
# métadonnées Spatial — fiche docs/proposals/SPATIAL_META_COL_VALIDATION_PROPOSAL.md
# §4a/§4d ; consommé par mod_spatial_deconv_reference.R (fenêtre muette 2-200 +
# fallback silencieux « else cols » supprimés, §4c). Le composant VALIDE et
# EXPLIQUE, il ne transforme aucune donnée. (+1, pin test-app-sourcing 163 -> 164.)
source("tools/pkg_skeleton/R/spatial_meta_col_validation.R")
source("tools/pkg_skeleton/R/spatial_async.R")
source("tools/pkg_skeleton/R/spatial_io.R")
source("tools/pkg_skeleton/R/spatial_multi.R")
source("tools/pkg_skeleton/R/spatial_reference.R")
source("tools/pkg_skeleton/R/spatial_niche.R")
source("tools/pkg_skeleton/R/spatial_stats.R")
source("tools/pkg_skeleton/R/spatial_deconv_prep.R")
source("tools/pkg_skeleton/R/spatial_deconv_tasks.R")
source("tools/pkg_skeleton/R/spatial_export.R")
source("tools/pkg_skeleton/R/spatial_report.R")

# ── Bloc 08 : R/reports → paquet (compilateur consolidé) ─────────────────────
# 4F / Stage 17 — COMPILATEUR d'etat + de provenance ; APRES tous les domaines
# (reutilise les gardes assert_*, les empreintes v2, sc_r_script_text et les
# vues croisées pures). D2 M5 lot 2 : les 4 fichiers vivent dans l'arbre du
# paquet (tools/pkg_skeleton/R/) — app.R le source() TOUJOURS, aux MEMES
# positions d'ordre (manifeste 77-80 : l'entree suit le chemin).
source("tools/pkg_skeleton/R/report_collector.R")
source("tools/pkg_skeleton/R/report_validator.R")
source("tools/pkg_skeleton/R/report_render.R")
source("tools/pkg_skeleton/R/report_bundle.R")

# ── Bloc 09 : modules/import (Shiny : UI + reactive glue) ────────────────────
source("modules/import/mod_rdata_picker.R")  # composant mutualise .rda/.RData
source("modules/import/mod_import_sc.R")
source("modules/import/mod_import_bulk.R")
source("modules/import/mod_import_spatial.R")
source("modules/import/mod_geo.R")

# ── Bloc 10 : modules/sc ─────────────────────────────────────────────────────
source("modules/sc/mod_sc_pipeline.R")
source("modules/sc/mod_sc_metadata.R")  # 0.5 Métadonnées — design condition/réplicat (feature 6×10X, audit 2026-09-27)
source("modules/sc/mod_sc_annotation_export.R")  # M3 : route d'export drive (métadonnées cellules SC)
source("modules/sc/mod_sc_annotation.R")
source("modules/sc/mod_sc_rarity.R")  # CCC 9 (Q1) : onglet « Rareté par population » (descriptif)
source("modules/sc/mod_sc_viz_export.R")  # M3 : route d'export drive (coordonnées 2D SC)
source("modules/sc/mod_sc_viz.R")
source("modules/sc/mod_sc_markers_export.R")  # S2c : route d'export drive (table marqueurs)
source("modules/sc/mod_sc_markers.R")
source("modules/sc/mod_sc_pseudobulk.R")
source("modules/sc/mod_sc_corr.R")
source("modules/sc/mod_sc_pathways_export.R")  # S2c : route d'export drive (table d'enrichissement SC)
source("modules/sc/mod_sc_pathways.R")
source("modules/sc/mod_sc_trajectory_export.R")  # M3 : route d'export drive (pseudotemps trajectoire SC)
source("modules/sc/mod_sc_trajectory.R")
source("modules/sc/mod_sc_velocity.R")
source("modules/sc/mod_sc_communication.R")
source("modules/sc/mod_sc_communication_spatial.R")  # V1.x-A : onglet « Contexte spatial » du panneau Communication
source("modules/sc/mod_sc_communication_trajectory.R")  # V1.x-B : onglet « Contexte trajectoire » du panneau Communication
source("modules/sc/mod_sc_communication_velocity.R")  # V1.x-C : onglet « Contexte vélocité » du panneau Communication
source("modules/sc/mod_sc_communication_perturbation.R")  # V1.x-D : onglet « Perturbation (in silico) » du panneau Communication
source("modules/sc/mod_sc_da_design.R")
source("modules/sc/mod_sc_da_milo.R")  # Stage 14 (4E-1) : orchestration Milo (consomme le design 8c)
source("modules/sc/mod_sc_da_sccoda.R")  # Stage 15 (4E-2) : orchestration scCODA (consomme le design 8c)
source("modules/sc/mod_sc_da_cross.R")  # Stage 16 (4E-3) : vues croisées (consomme 8d + 8e, aucun calcul)
source("modules/sc/mod_sc_report_consolidated.R")  # Stage 17 (4F) : rapport consolidé (compile l'état partagé, aucun calcul)
source("modules/sc/mod_sc_datasets.R")  # MD-4 : gestion du conteneur sc_datasets (double jeu SC)
source("modules/sc/mod_sc_mapping.R")
source("modules/sc/mod_sc.R")

# ── Bloc 11 : modules/bulk + modules/bulk_de (déplié) ────────────────────────
source("modules/bulk/mod_bulk_mapping.R")
source("modules/bulk/mod_bulk_filter_export.R")  # Slice 2.3 : route d'export drive (matrice VST)
source("modules/bulk/mod_bulk_filter.R")
# M2 (plan D2, 2026-10-24) : l'ancienne boucle de sourcing du dossier
# modules/bulk_de est DÉPLIÉE ci-dessous en 10 source() explicites. L'ordre
# (LC_COLLATE=C) est celui du manifeste R/.sources_manifest.txt — le tri
# alphabétique implicite de la boucle disparaît comme variable cachée.
source("modules/bulk_de/mod_bulk_de.R")
source("modules/bulk_de/mod_bulk_de_engine.R")
source("modules/bulk_de/mod_bulk_de_export.R")
source("modules/bulk_de/mod_bulk_de_multimethod.R")
source("modules/bulk_de/mod_bulk_de_pairwise.R")
source("modules/bulk_de/mod_bulk_de_run.R")
source("modules/bulk_de/mod_bulk_de_summary.R")
source("modules/bulk_de/mod_bulk_de_ui.R")
source("modules/bulk_de/mod_bulk_de_venn.R")
source("modules/bulk_de/mod_bulk_de_viz.R")
source("modules/bulk_de/mod_biostat_tree.R")  # M3 BIOSTAT O27 : accordéon latéral Bulk DE + miroir lecture SC (contrat BIOSTAT_DECISION_CONTRACT.md §8 row M3)
source("modules/bulk/mod_bulk_pathways_export.R")  # S2c : route d'export drive (table d'enrichissement)
source("modules/bulk/mod_bulk_pathways.R")
source("modules/bulk/mod_bulk_signatures_export.R")  # Slice 2.3 : route d'export drive (scores de signatures)
source("modules/bulk/mod_bulk_signatures.R")  # Bulk V2 M3 : signatures cellulaires
source("modules/bulk/mod_bulk_wgcna_export.R")  # Slice 4 : route d'export drive (gènes -> modules WGCNA)
source("modules/bulk/mod_bulk_wgcna.R")       # Bulk V2 M4 : WGCNA safe-mode
source("modules/bulk/mod_bulk_survival.R")    # Bulk V2 M5 : survie & clinique
source("modules/bulk/mod_bulk_pattern_export.R")  # Slice 2.3 : route d'export drive (clusters de profils)
source("modules/bulk/mod_bulk_pattern.R")     # STAT-S3 : clustering de profils (kmeans MVP)
source("modules/bulk/mod_bulk_dose_response.R")  # NEW-1 : dose-réponse / time-course (drc)
source("modules/bulk/mod_bulk_datasets.R")    # MD-1 : gestion du conteneur bulk_datasets
source("modules/bulk/mod_bulk_multi.R")       # MD-2 : comparaison multi-jeux
source("modules/bulk/mod_bulk_merge.R")       # NEW-2 : fusion de jeux bulk
source("modules/bulk/mod_bulk_network_export.R")  # Slice 2.3 : route d'export drive (noeuds PCSF)
source("modules/bulk/mod_bulk_network.R")     # NEW-3 : réseau PCSF / interactome
source("modules/bulk/mod_bulk_report.R")
source("modules/bulk/mod_bulk.R")

# ── Bloc 12 : modules/spatial ────────────────────────────────────────────────
source("modules/spatial/mod_spatial_qc.R")
source("modules/spatial/mod_spatial_cluster.R")
source("modules/spatial/mod_spatial_viz.R")
source("modules/spatial/mod_spatial_multi.R")
source("modules/spatial/mod_spatial_niche.R")
source("modules/spatial/deconv/mod_spatial_deconv_ui.R")
source("modules/spatial/deconv/mod_spatial_deconv_reference.R")
source("modules/spatial/deconv/mod_spatial_deconv_refviz.R")
source("modules/spatial/deconv/mod_spatial_deconv_outputs.R")
source("modules/spatial/deconv/mod_spatial_deconv.R")
source("modules/spatial/mod_spatial_pipeline.R")
source("modules/spatial/mod_spatial_export.R")
source("modules/spatial/mod_spatial_report.R")
source("modules/spatial/mod_spatial.R")

# ── Bloc 13 : modules/session (persistance de session) ───────────────────────
# ASR-3 (O29, SESSION_CHECKPOINT_RESTORE_CONTRACT.md §4/§5/§8 row ASR-3) : la
# surface UI des checkpoints de session — toggle opt-in (OFF par défaut),
# bannière de reprise (rend le payload du plan ASR-2, jamais un recalcul),
# ligne d'état. Module dédié : app.R n'est jamais sourcé par la suite
# (convention test-app-sourcing.R), le module est la surface testServer-
# testable (tests/testthat/test-asr3-ui.R) ; app.R n'en fait que le MONTAGE
# (sidebar + header= + server) et n'appelle jamais le moteur directement.
source("modules/session/mod_session_checkpoint.R")







# --- UI ---

ui <- page_navbar(

  # LOT 4A (V1.x UX): explicit id so import modules can jump natively
  # (nav_select) to the analysis tabs that host the ID-mapping panels.
  id = "main_nav",

  title = "Cerberus v1.0 - Multi-Omics Platform",
  
  theme = my_theme,

  # ── i18n (Phase 1): client-side translation shim ──────────────────────────
  # MUST appear exactly once in the UI. Injects the JS handler that
  # re-translates all STATIC labels (i18n$t() calls inside *_ui() functions)
  # in-browser when the server calls shiny.i18n::update_lang() — no page
  # reload, no UI rebuild.
  # FIX: injected via `header=` — page_navbar() strictly validates its `...`
  # children (nav_panel/nav_menu/nav_spacer only); a raw usei18n() fragment
  # among them aborts UI build with "Navigation containers expect a
  # collection of nav_panel()s".
  # FIX 2 (.usei18n_fixed, global.R): stock usei18n() lets htmltools
  # entity-escape the embedded translation dict ("&" -> "&amp;"), so every
  # key containing & ' " < > silently failed client-side lookup. The wrapper
  # re-renders the dict <script> as raw HTML.
  #
  # The DRIVE LIVE CONTROL badge rides in the SAME `header=` slot, for the same
  # reason: it is not a nav_panel(). The slot is shared with the i18n shim via
  # a tagList so neither can displace the other when one of them is absent.
  # `uiOutput()` yields an EMPTY container when renderUI() returns NULL, and it
  # is `display:contents`-free: it is a zero-height span until the badge shows,
  # so the layout is byte-identical to before when the protocol is off.
  header = tagList(
    if (I18N_AVAILABLE) .usei18n_fixed(i18n),
    uiOutput("drive_badge"),
    # ASR-3 (O29) : bannière de reprise de session — même discipline que le
    # badge ci-dessus : renderUI() NULL (toggle OFF, aucun checkpoint validé,
    # offre refusée ou purgée) => conteneur vide, empreinte nulle, layout
    # inchangé (précédent app.R:341-346). Non intrusive, jamais d'injection
    # silencieuse : deux boutons explicites (Reprendre / Continuer sans).
    mod_session_checkpoint_banner_ui("session_ckpt")
  ),
  
  
  
  # Sidebar global pour infos système
  
  sidebar = sidebar(
    
    title = i18n$t("Système"),
    
    width = 250,
    
    # ── i18n (Phase 1): language selector ────────────────────────────────────
    # VALUES ("fr"/"en") are the contract — server logic keys off them.
    # DISPLAY names are endonyms (each language in its own name) and are
    # deliberately NEVER translated. Hidden entirely if shiny.i18n is absent.
    if (I18N_AVAILABLE) {
      selectInput("selected_lang", label = i18n$t("Langue"),
                  choices = c("Français" = "fr", "English" = "en"),
                  selected = I18N_DEFAULT_LANG, width = "100%")
    } else NULL,
    
    # ── AGENT CONTROL (MCP / Live Control) ───────────────────────────────────
    # The drive protocol used to be ONE-WAY: only an external agent could arm a
    # session, so a human had no way to tell whether an agent was in control.
    # MEASURED (2026-10-24): the operator could not see the drive at all — the
    # handshake named a dead pid, the serving process never heartbeated, and
    # `arm.json` held a token nobody owned. This panel is the human's side of
    # `arm.json`: it always shows the state, and lets the operator hand control
    # over (or take it back) without touching a file.
    #
    # Gated at BUILD time on the same predicate that keeps production inert
    # (`ts_drive_interactive()`): a deployed app can never be armed, so a
    # permanent grey line there would advertise a capability that does not
    # exist — and the spec requires the UI to be unchanged when the protocol is
    # off. Dev/`Run App` sessions get the panel; production gets nothing.
    if (isTRUE(ts_drive_interactive())) tagList(
      hr(),
      h6(i18n$t("Contrôle agent (MCP)"), style = "font-weight: bold;"),
      uiOutput("drive_control_ui"),
      # DSTV-3 (C2, fiche :83-85) : la ligne d'activité du pilote —
      # « Agent : module X — action A — il y a Ns ». Abonnée au CHANGEMENT DE
      # HASH de state.json (un événement par écriture réelle, jamais au tick :
      # zéro churn, même discipline que le badge ci-dessus). Hors headless
      # uniquement (C3 — la porte vit dans le renderUI, pas ici).
      uiOutput("drive_activity_line")
    ) else NULL,
    
    
    
    h6(i18n$t("Mémoire Utilisée"), style = "font-weight: bold;"),
    
    textOutput("mem_usage"),
    
    actionButton("gc_btn", i18n$t("🧹 Nettoyer RAM"), 
                 
                 class = "btn-xs btn-outline-secondary w-100 mt-2"),
    
    
    
    hr(),
    
    
    
    h6(i18n$t("Paramètres RAM"), style = "font-weight: bold;"),
    
    div(class = "small text-muted", style = "font-size:0.75rem;",
        
        i18n$t("Bulk n'est presque jamais limité par la RAM (matrices de quelques Mo)."),
        " ",
        i18n$t("La pression vient surtout du Single-Cell/Spatial (objets Seurat volumineux) —"),
        " ",
        i18n$t("ajustez ici si vous travaillez avec de très gros jeux de données.")),
    
    numericInput("ram_future_gb", i18n$t("Limite mémoire par tâche parallèle (Go)"),
                 
                 value = 10, min = 1, max = 64, step = 1),
    
    numericInput("ram_upload_gb", i18n$t("Taille maximale d'upload (Go)"),
                 
                 value = 5, min = 1, max = 50, step = 1),
    
    actionButton("apply_ram_settings", i18n$t("Appliquer"),
                 
                 class = "btn-sm btn-outline-primary w-100"),
    
    
    
    hr(),
    
    
    
    h6(i18n$t("Objets Chargés"), style = "font-weight: bold;"),
    
    uiOutput("global_status_panel"),
    
    
    
    hr(),
    
    
    
    h6(i18n$t("Session"), style = "font-weight: bold;"),
    
    downloadButton("save_session_btn", i18n$t("💾 Sauvegarder Session"),
                   
                   class = "btn-sm btn-outline-primary w-100"),
    
    fileInput("load_session_file", i18n$t("📂 Charger Session (.rds)"),

              accept = c(".rds", ".rda", ".RData"), width = "100%"),
    
    div(class = "small text-muted", style = "font-size:0.75rem;",
        
        i18n$t("La sauvegarde capture l'ensemble des données chargées (Single-Cell, Bulk, Spatial) —"),
        " ",
        i18n$t("elle est déclenchée uniquement par ce bouton, jamais automatiquement. Pour le Spatial,"),
        " ",
        i18n$t("seul le \"sketch\" (echantillon RAM) est garanti portable : les donnees BPCells sur"),
        " ",
        i18n$t("disque doivent rester disponibles au meme chemin pour relancer clustering/deconvolution.")),
    
    
    
    # ASR-3 (O29) — checkpoints de session : toggle opt-in (OFF par défaut)
    # + ligne d'état du dernier checkpoint. Tant que le toggle est OFF :
    # AUCUN appel moteur, AUCUN rendu (contrat §4/§7.5 — comportement actuel
    # byte pour byte). Panneau placé dans la section Session, voisin des
    # boutons Sauvegarder/Charger Session (même domaine : persistance).
    mod_session_checkpoint_ui("session_ckpt"),

    hr(),



    actionButton("help_btn", i18n$t("📖 Guide / Aide"),
                 
                 icon = icon("question-circle"), 
                 
                 class = "btn-info w-100")
    
  ),
  
  
  
  # === ONGLETS D'IMPORT ===

  nav_menu(

    tagList("📥 ", i18n$t("Import Données")),

    icon = icon("upload"),

    nav_panel(i18n$t("Single-Cell"),

              icon = icon("braille"),

              mod_import_sc_ui("import_sc")),



    nav_panel(i18n$t("RNA Bulk"),

              icon = icon("table"),

              mod_import_bulk_ui("import_bulk")),



    nav_panel(i18n$t("Spatial"),

              icon = icon("map"),

              mod_import_spatial_ui("import_spatial")),

    # LOT 6A (revised per user 2026-09-05): GEO stays labelled as a public
    # DATA SOURCE but is placed LAST in the Import menu (user preference).
    nav_panel(i18n$t("Source publique (GEO)"),

              icon = icon("database"),

              mod_geo_ui("geo")) ##ajout verif

  ),
  
  
  
  # === ONGLETS D'ANALYSE ===
  
  nav_spacer(),
  
  
  
  # NB: titles are French-by-default as required; English on switch via the
  # shiny.i18n JS shim (usei18n above + update_lang in the server observer).
  nav_panel(tagList("🔬 ", i18n$t("Analyse Single-Cell")), value = "tab_sc",

            icon = icon("microscope"),

            # M3 BIOSTAT O27 : miroir LECTURE SEULE de la recommandation
            # validée côté Bulk (même source global_data$biostat_decision,
            # aucune re-dérivation ; uiOutput NULL = empreinte nulle à vide).
            tagList(mod_sc_ui("sc"), mod_biostat_mirror_ui("biostat_sc"))),



  nav_panel(tagList("📊 ", i18n$t("Analyse Bulk RNA")), value = "tab_bulk",

            icon = icon("chart-line"),

            # M3 BIOSTAT O27 : accordéon latéral de l'assistant biostatistique
            # (orientation seule — O27 §12.2 ; monté au niveau du nav_panel,
            # surface minimale, l'accordéon interne de mod_bulk.R reste
            # inchangé).
            tagList(mod_bulk_ui("bulk"), mod_biostat_tree_ui("biostat_bulk"))),
  
  
  
  nav_panel(tagList("🗺️ ", i18n$t("Analyse Spatiale")), value = "tab_spatial",
            
            icon = icon("layer-group"),
            
            mod_spatial_ui("spatial"))
  
)



# --- SERVER ---

server <- function(input, output, session) {
  
  # S-RES-1 (§5 #14 / O30) — le navigateur doit TOUJOURS tenter une reconnexion (veille PC) ; shinyOptions(allowReconnect=) est INERTE sur shiny 1.14.0 (renv.lock:16212), mécanisme effectif = session$allowReconnect (vérifié namespace/client/upstream, 2026-10-31)
  session$allowReconnect("force")
  
  # Chantier #5 : sans ce hook, fermer l'app laissait les workers mirai vivants
  # (processus orphelins sur Windows) — stop_spatial_daemons() n'avait AUCUN
  # appelant au shutdown. onStop() est le point de sortie propre de la session ;
  # le teardown est idempotent (no-op sans pool).
  onStop(stop_spatial_daemons)
  
  
  
  # === DONNÉES GLOBALES PARTAGÉES ===
  
  global_data <- reactiveValues(
    
    sc_obj = NULL,      # Objet Seurat Single-Cell

    sc_obj_epoch = 0L,  # incrémenté UNIQUEMENT au remplacement de jeu (imports) — déclenche la purge des résultats partagés (audit 2026-09-27 §1.5)

    sc_datasets = list(),  # MD-4 : jeux SC nommés (double jeu — modes 1/2 décision 5)
    # — docs/contracts/SC_MULTI_CONTRACT.md ; sc_obj reste le jeu "actif"

    bulk_obj = NULL,    # Objet Bulk (liste avec counts + metadata)

    bulk_obj_epoch = 0L,  # incrémenté UNIQUEMENT au remplacement de jeu (imports/fusion/activation) — déclenche la purge des résultats partagés bulk (parité SC, audit 2026-09-27 §1.5)

    bulk_datasets = list(),  # MD-1 : jeux bulk nommés (comparaison multi-jeux)
    # — docs/contracts/BULK_MULTI_CONTRACT.md ; bulk_obj reste le jeu "actif"

    spatial_obj = NULL,  # Spatial : liste (sketch, bpcells_dir, coords, ...) — voir tools/pkg_skeleton/R/spatial_io.R (ex-R/utils_spatial_io.R)
    # — POINTE TOUJOURS vers l'echantillon "actif" (voir spatial_datasets ci-dessous)
    
    spatial_datasets = list(),
    
    active_spatial_dataset = NULL,
    
    # v10 (backlog court-terme #2) : cache des resultats par-echantillon
    # (QC/clusters/deconv/niches), ecrit/lu par mod_spatial.R -- vit ici
    # (plutot que dans un reactiveVal interne au module) precisement pour
    # etre capturable par save_session_btn / restaurable par
    # load_session_file ci-dessous.
    spatial_results_cache = list(),
    
    # backlog court-terme #1 : reference scRNA-seq PARTAGEE (preparee une
    # fois depuis l'onglet Import > Spatial), reutilisee par RCTD ET Label
    # Transfer sans reparser -- voir mod_import_spatial.R et
    # mod_spatial_deconv.R. list(path=, n_cells=, n_genes=, backend=,
    # celltype_col=, source_label=, n_dropped_rare=, created_at=) ou NULL.
    spatial_reference = NULL,
    
    # Feedback biologiste (rapport multi-echantillons) : dernier resultat
    # d'integration conjointe (mod_spatial_multi.R), miroir en LECTURE pour
    # mod_spatial_report.R -- voir mod_spatial_multi.R v2. list(embeddings=,
    # n_per_dataset=, reduction_used=, datasets=, computed_at=) ou NULL.
    spatial_multi_integration = NULL,
    
    spatial_reimport_signal = NULL,   # {name=, at=} -- signal re-import sous nom deja actif (voir mod_spatial.R)

    # ── i18n (Phase 1) ──────────────────────────────────────────────────────
    # language : reactive TRIGGER — every language-aware renderUI reads it,
    #            so those outputs re-render automatically on switch.
    # i18n     : SESSION-scoped Translator for all dynamic content
    #            (showNotification, renderUI, Progress, plot titles...).
    #            The global \`i18n\` is never mutated here (multi-user safety).
    language = I18N_DEFAULT_LANG,
    i18n     = .new_session_i18n()

  )

  # LOT 4A (V1.x UX): root (un-namespaced) session handle, read by the import
  # modules' "Go to ID mapping" buttons so they can nav_select() the top-level
  # page_navbar and open the analysis accordion panels. Write-once, never
  # mutated afterwards — no reactive side effects.
  global_data$session <- session

  # ── DRIVE LIVE CONTROL (docs/DRIVE_LIVE_CONTROL_PLAN.md) ──────────────────
  # A poller is ALWAYS registered, and does NOTHING until tools/_drive/arm.json
  # is valid. This is the only design that lets an agent drive a session the
  # human ALREADY opened (Run App clicked, Viewer or localhost tab, data
  # loaded): an env var set after the load cannot retrofit server(), and
  # session$setInputs() does not exist on a live session. Idle cost is a few
  # file.exists() calls per tick; with arm.json absent the app is
  # indistinguishable from before.
  #
  # `global_data$drive_registry` is the SESSION-scoped table in which the bulk
  # modules publish the button counters that `run_pipeline` fires (grade G2).
  # It lives here, not in a global environment, so two tabs can never share a
  # counter (spec §2.1).
  global_data$drive_registry <- new.env(parent = emptyenv())
  .drive <- ts_drive_attach(session, input)
  # ASR-5 (D7, en-tête test-asr5-drive.R) : le token drive est posé sur la
  # surface userData de la session — un champ littéral session$token est
  # IMPOSSIBLE sur shiny 1.14.0 (ShinySession R6 lock_objects=TRUE, mesuré).
  # Le module session_ckpt le lit pour porter la santé checkpoint sur la
  # ligne sessions[] de CETTE session (aucun fichier nouveau, contrat ASR
  # §5.5/§7.6).
  session$userData$drive_token <- .drive$token

  # ── DEV-ONLY STATUS BADGE ─────────────────────────────────────────────────
  # A LIGHTWEIGHT, DEDICATED reactive state — deliberately NOT derived from
  # global_data, Seurat objects, matrices, plots, reports or long-running jobs.
  # Deriving it from any of those would (a) invalidate expensive reactives on
  # every protocol beat and (b) couple a dev affordance to the data path. This
  # value is written ONLY on a real protocol transition (see `.drive$events`).
  drive_state <- shiny::reactiveVal(ts_drive_badge_model())

  # DSTV-3 (C2) — the activity line's dedicated reactive state, on the SAME
  # discipline as the badge above: written ONLY when the emitter actually
  # wrote state.json (one event per real hash change, drained below in the
  # poller), never on an idle tick — zero re-render churn (fiche :85/:103).
  drive_activity <- shiny::reactiveVal(NULL)

  # This session is "selected" when ready.json names ITS token. With several
  # tabs open the last one to boot owns the file, so only that tab badges.
  .drive_selected <- function() {
    hb <- ts_drive_read_ready()
    !is.null(hb) && identical(as.character(hb$session_token), .drive$token)
  }

  # Read the badge view. `visible` is FALSE unless the dev protocol is on, the
  # session is live and selected, and it is armed — so in every other case
  # `output$drive_badge` renders NOTHING and the UI is untouched.
  output$drive_badge <- shiny::renderUI({
    st <- drive_state()
    vis <- ts_drive_badge_visible(
      enabled     = identical(Sys.getenv("TRANSCRIPTO_DEV_DRIVE"), "1"),
      interactive = isTRUE(ts_drive_interactive()),
      selected    = .drive_selected(),
      armed       = isTRUE(.drive$armed()),
      state       = st$state
    )
    if (!vis) return(NULL)
    ts_drive_badge_ui(ts_drive_badge_view(st))
  })

  # ── AGENT CONTROL PANEL — the human's side of `arm.json` ──────────────────
  # A dedicated, CHEAP reactive state on the same discipline as the badge: the
  # projection is recomputed on every protocol beat (two small reads the tick
  # already performs), but the reactive value is written ONLY when it CHANGED —
  # a `set()` per beat would repaint the sidebar 1.25 times per second and steal
  # focus from whatever the human is typing.
  #
  # Why it exists at all: the drive was ONE-WAY on `arm.json`. Only an external
  # agent wrote it, so a human had no way to tell whether an agent held the
  # session — MEASURED 2026-10-24, where the operator could not see the drive at
  # all and every symptom read as "the app ignores the agent".
  drive_control <- shiny::reactiveVal(NULL)

  .drive_control_now <- function() {
    ts_drive_control_state(
      token       = .drive$token,
      interactive = isTRUE(ts_drive_interactive()),
      ready       = ts_drive_read_ready(),
      arm         = ts_drive_read_json(ts_drive_path("arm.json")),
      # The wildcard is the launcher's privilege: `TRANSCRIPTO_DEV_DRIVE=1` is
      # set for that process only (tools/launch_dev_drive.R), never in .Renviron.
      wildcard_ok = identical(Sys.getenv("TRANSCRIPTO_DEV_DRIVE"), "1")
    )
  }

  .drive_control_refresh <- function(force = FALSE) {
    st <- .drive_control_now()
    if (isTRUE(force) || !identical(st, drive_control())) drive_control(st)
    invisible(st)
  }

  # First paint: the panel must be honest BEFORE the first tick (~800 ms).
  .drive_control_refresh(force = TRUE)

  output$drive_control_ui <- shiny::renderUI({
    global_data$language                      # re-render on language switch
    st <- drive_control()
    if (is.null(st)) return(NULL)
    label <- switch(
      st$state,
      unavailable  = .tr("Contr\u00f4le agent indisponible (session non interactive)"),
      no_handshake = switch(
        st$reason,
        other_session     = .tr("Un autre onglet d\u00e9tient le handshake agent"),
        handshake_invalid = .tr("Handshake agent invalide"),
        .tr("Aucun handshake agent publi\u00e9 pour cette session")
      ),
      idle = switch(
        st$reason,
        arm_invalid = .tr("Aucun agent \u2014 arm.json illisible, vous gardez le contr\u00f4le"),
        .tr("Aucun agent \u2014 vous gardez le contr\u00f4le")
      ),
      armed = switch(
        st$reason,
        armed_wildcard = .tr("AGENT EN CONTR\u00d4LE (wildcard dev)"),
        .tr("AGENT EN CONTR\u00d4LE \u2014 cette session est pilot\u00e9e")
      ),
      armed_other = switch(
        st$reason,
        wildcard_refused = .tr("Armement wildcard refus\u00e9 (lanceur dev requis)"),
        .tr("Une autre session est arm\u00e9e")
      ),
      ""
    )
    # ONE button, and its label states the direction. The three flags are
    # mutually exclusive by construction (idle arms, armed disarms, armed_other
    # may be taken over), so there is no ambiguous case to handle — and when
    # none holds (no handshake) no button is offered, because there is nothing
    # this tab may honestly do.
    btn <- if (isTRUE(st$can_arm)) {
      shiny::actionButton("drive_control_toggle",
                          .tr("Donner le contr\u00f4le \u00e0 un agent"),
                          class = "btn-xs btn-outline-warning w-100 mt-2")
    } else if (isTRUE(st$can_disarm)) {
      shiny::actionButton("drive_control_toggle",
                          .tr("Reprendre le contr\u00f4le"),
                          class = "btn-xs btn-outline-success w-100 mt-2")
    } else if (isTRUE(st$can_takeover)) {
      # A leftover arm.json from a session that no longer exists must not lock
      # the operator out of their own app (MEASURED live, :6515). The takeover
      # is an explicit click, and it only ever writes OUR token.
      shiny::actionButton("drive_control_toggle",
                          .tr("Reprendre le contr\u00f4le pour cette session"),
                          class = "btn-xs btn-outline-warning w-100 mt-2")
    } else NULL
    ts_drive_control_ui(
      st, label = label, button = btn,
      # The human has to be able to NAME this session when an agent is attached
      # (`transcripto_drive_status` publishes the same derived id), otherwise
      # "drive my session" is ambiguous as soon as a second tab exists.
      # MEASURED 2026-10-26: the fixed label "ID de session :" made a conflict
      # state read as if the DISPLAYED id were the other session's, when it is
      # always THIS connection's own derived id — the label now says which.
      session_id    = .drive$session_id,
      session_label = switch(
        st$state,
        armed        = .tr("ID de session (donnez-le \u00e0 l'agent) :"),
        armed_other  = .tr("Votre connexion (non arm\u00e9e) :"),
        no_handshake = .tr("Votre connexion (sans handshake) :"),
        .tr("ID de session :")
      ),
      hint = .tr("Un agent externe (MCP) peut piloter cette session pour des tests ou du d\u00e9bogage. L'app applique arm.json au tick suivant (~800 ms).")
    )
  })

  # DSTV-3 (C2/C3) — the agent-activity line: « Agent : module X — action A —
  # il y a Ns ». The reactive state (`drive_activity`) is written ONLY on a
  # real state.json write (hash change, drained in the poller above) — the
  # renderUI NEVER re-runs on an idle tick. The display gates live HERE, not
  # in the drain: interactive (the protocol gate), the headless discriminant
  # (fiche :87 « une session headless n'affiche rien de nouveau ») and armed
  # (the line says « Agent : » — an unarmed session is the human navigating).
  # The event carries armed, so the DISARM write is what removes the line
  # without a second subscription. Absent view => NO element at all (the
  # `sprintf("… : %s", "")` trap, same rule as the session line above).
  output$drive_activity_line <- shiny::renderUI({
    global_data$language                      # re-render on language switch
    view <- drive_activity()
    if (is.null(view)) return(NULL)
    if (!isTRUE(ts_drive_activity_visible(
          interactive = isTRUE(ts_drive_interactive()),
          viewer      = view$viewer,
          armed       = isTRUE(view$armed)))) return(NULL)
    age <- sprintf("%.0f", max(0, suppressWarnings(as.numeric(view$age_s)[1])))
    # ASR-5 (contrat SESSION_CHECKPOINT_RESTORE_CONTRACT.md §8 row ASR-5) :
    # la santé checkpoint de la ligne sessions[] est rendue en ligne
    # secondaire — le champ est déjà rédigé/borné par la vue
    # (ts_drive_activity_view) ; absent -> AUCUN élément (le piège
    # sprintf("… : %s", ""), même règle que la ligne d'activité au-dessus).
    ck_txt <- suppressWarnings(as.character(view$checkpoint)[1])
    ck_bit <- if (is.null(ck_txt) || is.na(ck_txt) || !nzchar(ck_txt)) NULL else
      shiny::div(
        style = "font-size:0.68rem; margin-top:1px; line-height:1.25;",
        sprintf(.tr("Checkpoint : %s"), ck_txt)
      )
    shiny::div(
      id = "ts_drive_activity_line",
      style = "margin-top:4px; font-size:0.72rem; line-height:1.3;",
      sprintf(.tr("Agent : module %s \u2014 action %s \u2014 il y a %s s"),
              view$module, view$action, age),
      ck_bit
    )
  })

  # Arm / disarm. The STATE decides the direction, never the click count: the
  # button label and the write must not be able to disagree.
  observeEvent(input$drive_control_toggle, {
    st <- drive_control()
    if (is.null(st)) return()
    target <- isTRUE(st$can_arm) || isTRUE(st$can_takeover)  # else take it back
    res <- ts_drive_write_arm(.drive$token, target)
    if (!isTRUE(res$wrote)) {
      showNotification(paste0(.tr("\u00c9chec du contr\u00f4le agent : "), res$reason),
                       type = "error", duration = 8)
      return(invisible(NULL))
    }
    .drive_control_refresh(force = TRUE)
    showNotification(
      if (target) .tr("Contr\u00f4le donn\u00e9 \u00e0 l'agent \u2014 il peut piloter cette session.")
      else .tr("Contr\u00f4le repris \u2014 l'agent ne peut plus piloter cette session."),
      type = "message", duration = 5
    )
    invisible(NULL)
  }, ignoreInit = TRUE)

  # The ONE reactive beat of the protocol. `invalidateLater()` is the only
  # polling style used (spec §6) — not `reactiveFileReader`, to avoid a second
  # mechanism. `R/` carries the decision; this observer only beats the heart.
  #
  # No `isolate()` is needed on `global_data`: the tick runs INSIDE this
  # observer, so reads of the reactiveValues are legitimately reactive. An
  # `isolate()` wrapper would ALSO be possible; what is forbidden is reading a
  # reactive value outside any reactive consumer.
  observe({
    # The single gate keeping production inert (spec §1).
    if (!isTRUE(ts_drive_interactive())) return()
    invalidateLater(.drive$poll_ms, session)

    tick <- .drive$on_tick(
      global_data,
      # `effects()` is how the poller reaches module-owned counters without any
      # reactive machinery living in R/. Two modes: bump one button, or list a
      # module's announced tokens.
      effects = function(input_id, mode = "bump", module = NULL, request = NULL) {
        # G3: `import_file` does NOT touch a counter. It calls the module's
        # published IMPORTER with an already-validated request. Returning NULL
        # means "no importer published", which the watcher reports as a missing
        # SEAM — never as a rejected file. Handled BEFORE the registry guard
        # below, because `ts_drive_importer_of()` does its own lookup and a
        # missing registry is one of the cases it already answers.
        if (identical(mode, "import")) {
          imp <- ts_drive_importer_of(global_data, module)
          if (is.null(imp)) return(NULL)
          return(imp(request))
        }
        # S2: `export_result` reaches a module's published EXPORTER, exactly as
        # `import_file` reaches its importer, and for the same reason it is handled
        # HERE and before the registry guard: the exporter is a function the poller
        # calls, not a counter it increments.
        #
        # MEASURED, and the omission was invisible offline. This branch was missing
        # at first, so an unknown `mode` fell THROUGH to the registry path below,
        # which returned atomic `FALSE` (an empty `ids` loop), and the watcher's
        # `!isTRUE(out$ok)` then raised "$ operator is invalid for atomic vectors".
        # The error escaped the ONE reactive beat, so the poller observer DIED: the
        # handshake stopped being rewritten and the session was reported lost. The
        # unit tests could not see it because they supply their own `effects` stub,
        # so only a live session found it — which is the whole argument for running
        # the real app.
        #
        # `exp()` takes NO ARGUMENTS, and that is deliberate. The module publishes a
        # closure over ITS OWN `shared_rv`, so the route is bound to the state that
        # produced the result and cannot be pointed at another module's store. It
        # also means this seam needs no `shared_rv`, which does not exist at app
        # scope (see the note at :699) — passing one here was the second, quieter
        # version of the same mistake.
        if (identical(mode, "export")) {
          exp <- ts_drive_export_of(global_data, module)
          if (is.null(exp)) return(NULL)
          return(exp())
        }
        reg <- global_data$drive_registry
        if (!is.environment(reg)) {
          # Should be unreachable: the registry is created a few lines above.
          # If it ever isn't, the protocol must SAY so rather than report a
          # silently unbound button (which would read as "G2 not implemented").
          message("[drive] drive_registry is missing — button binds unavailable")
          if (identical(mode, "tokens")) return(list())
          return(FALSE)
        }
        ids <- if (identical(mode, "tokens")) {
          if (is.null(module)) ls(reg) else ls(reg)[vapply(ls(reg), function(i) {
            identical(ts_drive_module_of(i), module)
          }, logical(1))]
        } else if (is.null(input_id)) {
          character(0)
        } else {
          input_id
        }
        if (identical(mode, "tokens")) {
          return(stats::setNames(lapply(ids, function(i) reg[[i]]), ids))
        }
        ok <- FALSE
        for (i in ids) ok <- isTRUE(ts_drive_bump_token(reg[[i]])) || ok
        ok
      }
    )

    # Navigation is an EFFECT (bslib + session), so it is performed here, from
    # the one-shot plan the tick stored. A plan is read ONCE: an idle tick
    # afterwards must not replay a tab jump the human has since undone.
    plan <- .drive$pending_nav()
    if (!is.null(plan)) ts_drive_perform_nav(session, plan)

    # Badge: apply ONLY the transitions the tick actually produced. An idle
    # tick returns an empty list, so the badge is untouched — no reactive
    # invalidation, no repaint, nothing. This keeps the badge OBSERVATIONAL:
    # it never triggers a pipeline, never touches `input`, never clears data.
    evs <- .drive$pending_events()
    if (length(evs) > 0L) {
      st <- drive_state()
      for (e in evs) {
        st <- ts_drive_badge_advance(
          st, e$event, status = e$status, ack_seq = e$ack_seq,
          module = e$module, action = e$action,
          elapsed_s = e$elapsed_s, error = e$error
        )
      }
      drive_state(st)
    }

    # Activity line (DSTV-3, C2): drain the ONE-SHOT events the emitter queued
    # on each real state.json write — the hash-change subscription the fiche
    # asks for (:85). No write -> empty list -> the reactive state is untouched
    # -> zero churn. The LAST event is the current truth; the DISPLAY gates
    # (interactive / headless / armed) live in the renderUI below, so an
    # unarmed write is what makes the line disappear when the agent lets go.
    acts <- ts_drive_pending_activity_events()
    if (length(acts) > 0L) {
      drive_activity(ts_drive_activity_view(acts[[length(acts)]]))
    }

    # Agent control: recompute the projection on every beat, but publish it to
    # the sidebar only when it changed (see `.drive_control_refresh`). This is
    # also what makes an arm/disarm written by an EXTERNAL agent visible here
    # within one tick, without polling anything new.
    .drive_control_refresh()
  })
  
  # ── i18n (Phase 1): language switch ───────────────────────────────────────
  # Three coordinated effects:
  #   1) session translator updated → every future dynamic string (notifs,
  #      Progress, renderUI rebuilds) uses the new language;
  #   2) global_data$language mutated → invalidates and re-renders every
  #      language-aware renderUI in every module (they read this field);
  #   3) shiny.i18n::update_lang() → the JS shim re-translates all STATIC
  #      labels (i18n$t() in *_ui() functions) in-browser, no reload.
  # ignoreInit=TRUE: avoids a pointless fr→fr round-trip at session start.
  observeEvent(input$selected_lang, {
    lang <- input$selected_lang
    if (is.null(lang) || !lang %in% c("fr", "en")) return()
    global_data$language <- lang
    global_data$i18n$set_translation_language(lang)
    if (I18N_AVAILABLE) shiny.i18n::update_lang(language = lang, session = session)
  }, ignoreInit = TRUE)

  # ── i18n sidebar dynamic labels (fileInput has no update*; JS shim handles static h6/div text) ──
  observeEvent(global_data$language, {
    .tr_s <- function(k) { tr <- global_data$i18n; if (is.null(tr)) return(k); tryCatch(.strip_i18n_html(tr$t(k)), error=function(e) k) }
    updateActionButton(session, "gc_btn", label = .tr_s("🧹 Nettoyer RAM"))
    updateNumericInput(session, "ram_future_gb", label = .tr_s("Limite mémoire par tâche parallèle (Go)"))
    updateNumericInput(session, "ram_upload_gb", label = .tr_s("Taille maximale d'upload (Go)"))
    updateActionButton(session, "apply_ram_settings", label = .tr_s("Appliquer"))
    # downloadButton has no update*; label flips via JS shim (i18n$t at UI build)
    updateActionButton(session, "help_btn", label = .tr_s("📖 Guide / Aide"))
  }, ignoreInit = TRUE)
  
  
  
  # === MODULES D'IMPORT ===
  
  mod_import_sc_server("import_sc", global_data)
  
  mod_import_bulk_server("import_bulk", global_data)
  
  mod_import_spatial_server("import_spatial", global_data)
  
  
  # AFTER (fix #1) : mod_geo_server("geo", global_data) shared_rv undefined at app scope → crash
  mod_geo_server("geo", global_data)
  
  
  # === MODULES D'ANALYSE ===
  
  mod_sc_server("sc", global_data)
  
  # AUDIT FIX (quick win) : l'appel racine mod_sc_mapping_server("mapping", global_data)
  # a ete retire d'ici. Il enregistrait un moduleServer sous le namespace
  # top-level "mapping", qui n'a JAMAIS eu de UI correspondante (le vrai
  # panneau "0. Mapping IDs" du Single-Cell vit sous le namespace "sc-mapping",
  # cree par l'appel imbrique mod_sc_mapping_server("mapping", global_data)
  # DEJA present a l'interieur de mod_sc_server(), voir modules/sc/mod_sc.R).
  # C'etait donc un module fantome : aucun input ne pointait jamais vers son
  # namespace "mapping" nu, seulement de la reactivite morte. Retire.
  
  mod_bulk_server("bulk", global_data)

  # M3 BIOSTAT O27 : assistant biostatistique — parcours de l'arbre (moteur
  # M2, orientation seule) + confirmation -> provenance I2/I3 (couche module) ;
  # miroir lecture SC = rendu de la MÊME publication (aucune re-dérivation).
  # M4 : pont bulk_shared_rv (publié par mod_bulk_server, lecture seule I1) —
  # prefill des questions + moteur déclaré lu au clic ; NULL-safe (M3 si bulk
  # n'a pas tourné). isolate() OBLIGATOIRE : lecture d'un reactiveValues hors
  # consommateur au montage — dans une session RÉELLE, la lecture nue fait
  # mourir la session (shiny 1.14.0, reproduit 2026-11-02 sur app minimale :
  # shinytest2 « 404 for values URL .../dataobj/shinytest ») ; testServer
  # masquait le problème. La valeur est stable : le handle publié par
  # mod_bulk_server n'est jamais remplacé (la purge mute ses champs en place).
  mod_biostat_tree_server("biostat_bulk", global_data,
                          shared_rv = isolate(global_data$bulk_shared_rv))

  mod_biostat_mirror_server("biostat_sc", global_data)

  mod_spatial_server("spatial", global_data)
  
  
  
  # === STATUT DES OBJETS — INDICATEUR DE PROGRESSION GLOBAL (point 8) ===
  
  output$global_status_panel <- renderUI({
    global_data$language
    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(as.character(key))
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) as.character(key))
    }
    
    # ── Single-Cell state ────────────────────────────────────────────────
    
    sc_state <- if (is.null(global_data$sc_obj)) {
      
      list(icon = "⚪", label = .tr("Single-Cell"), detail = .tr("Aucune donnée"))
      
    } else {
      
      n_cells   <- ncol(global_data$sc_obj)
      
      n_samples <- length(unique(global_data$sc_obj$orig.ident))
      
      has_clusters <- "seurat_clusters" %in% colnames(global_data$sc_obj@meta.data)
      
      list(
        
        icon   = if (has_clusters) "🟢" else "🟡",
        
        label  = .tr("Single-Cell"),
        
        detail = paste0(format(n_cells, big.mark = ","), " ", .tr("cellules,"), " ", n_samples, " ", .tr("échantillon(s)"), if (has_clusters) paste0(" ", .tr("— pipeline exécuté")) else paste0(" ", .tr("— pipeline non lancé")))
        
      )
      
    }
    
    
    
    # ── Bulk state ───────────────────────────────────────────────────────
    
    bulk_state <- if (is.null(global_data$bulk_obj)) {
      
      list(icon = "⚪", label = .tr("Bulk RNA"), detail = .tr("Aucune donnée"))
      
    } else {
      
      n_samples <- ncol(global_data$bulk_obj$counts)
      
      list(
        
        icon   = "🟢",
        
        label  = .tr("Bulk RNA"),
        
        detail = .t_fmt(.tr("{n} échantillon(s) importé(s)"), n = n_samples)
        
      )
      
    }
    
    
    
    # ── Spatial state ────────────────────────────────────────────────────
    
    spatial_state <- if (is.null(global_data$spatial_obj)) {
      
      list(icon = "⚪", label = .tr("Spatial"), detail = .tr("Aucune donnée"))
      
    } else {
      
      obj <- global_data$spatial_obj
      
      n_total  <- obj$n_total %||% ncol(obj$sketch)
      
      n_sketch <- ncol(obj$sketch)
      
      disk_ok  <- !is.null(obj$bpcells_dir) && dir.exists(obj$bpcells_dir)
      
      n_ds     <- length(global_data$spatial_datasets)
      
      list(
        
        icon   = if (disk_ok) "🟢" else "🟠",
        
        label  = .tr("Spatial"),
        
        detail = paste0(format(n_total, big.mark = ","), " ", .tr("elements"), " (", format(n_sketch, big.mark = ","), " ", .tr("en RAM, sketch"), ")",
                         
                         if (!disk_ok) paste0(" ", .tr("— disque introuvable")) else "",
                         
                         if (n_ds > 1) paste0(" ", .t_fmt(.tr("— {n} echantillons charges"), n = n_ds)) else "",
                         
                         if (!is.null(global_data$spatial_reference)) {
                           
                           paste0(" ", .t_fmt(.tr("— reference partagee : {c} cellules"), c = format(global_data$spatial_reference$n_cells %||% 0, big.mark = ",")))
                           
                         } else "")
        
      )
      
    } 
    
    
    
    render_row <- function(state) {
      
      div(style = "padding:4px 0;border-bottom:1px solid #eee;font-size:0.82rem;",
          
          tags$strong(paste(state$icon, state$label)),
          
          tags$br(),
          
          tags$span(style = "color:#666;", state$detail))
      
    }
    
    
    
    tagList(render_row(sc_state), render_row(bulk_state), render_row(spatial_state))
    
  })
  
  
  
  # === SAUVEGARDE / CHARGEMENT DE SESSION (point 7) ===
  
  output$save_session_btn <- downloadHandler(
    
    filename = function() paste0("cerberus_session_", Sys.Date(), ".rds"),
    
    content  = function(file) {
      
      session_snapshot <- list(
        
        sc_obj      = global_data$sc_obj,

        sc_datasets = global_data$sc_datasets,

        bulk_obj    = global_data$bulk_obj,

        bulk_datasets = global_data$bulk_datasets,

        bulk_multi_comparison = global_data$bulk_multi_comparison,

        spatial_obj = global_data$spatial_obj,
        
        spatial_datasets = global_data$spatial_datasets,
        
        active_spatial_dataset = global_data$active_spatial_dataset,
        
        spatial_results_cache = global_data$spatial_results_cache,
        
        spatial_reference = global_data$spatial_reference,
        
        spatial_multi_integration = global_data$spatial_multi_integration,
        
        saved_at    = Sys.time(),
        
        app_version = "Cerberus v1.0"
        
      )
      
      saveRDS(session_snapshot, file)
      
    }
    
  )
  
  
  
  observeEvent(input$load_session_file, {
    
    req(input$load_session_file)
    
    
    
    tryCatch({

      # .rda/.RData accepté (contrat RDATA_IMPORT_CONTRACT.md) : workspace à
      # objet unique = snapshot ; multi-objets -> message orientant.
      if (rdata_is_supported_file(input$load_session_file$datapath)) {
        rda_env <- rdata_load_env(input$load_session_file$datapath)
        rda_names <- ls(envir = rda_env)
        if (length(rda_names) > 1L) {
          showNotification(
            "❌ Le fichier .RData contient plusieurs objets — sauvegardez le snapshot de session seul (un objet unique) puis rechargez-le.",
            type = "error", duration = 10
          )
          rdata_free(rda_env)
          return()
        }
        snapshot <- rdata_extract_object(rda_env, rda_names[1])
        rdata_free(rda_env)
      } else {
        snapshot <- readRDS(input$load_session_file$datapath)
      }
      
      
      
      if (!is.list(snapshot) || !all(c("sc_obj", "bulk_obj", "spatial_obj") %in% names(snapshot))) {
        
        showNotification(
          
          "❌ Fichier de session invalide — structure non reconnue.",
          
          type = "error", duration = 8
          
        )
        
        return()
        
      }
      
      
      
      global_data$sc_obj      <- snapshot$sc_obj

      # MD-4 : conteneur absent des snapshots antérieurs — repli list() propre.
      global_data$sc_datasets <- snapshot$sc_datasets %||% list()

      global_data$bulk_obj    <- snapshot$bulk_obj

      # MD-1 : conteneur absent des snapshots antérieurs — repli list() propre.
      global_data$bulk_datasets <- snapshot$bulk_datasets %||% list()

      # MD-2 : résultat de comparaison absent des snapshots antérieurs.
      global_data$bulk_multi_comparison <- snapshot$bulk_multi_comparison %||% NULL

      global_data$spatial_obj <- snapshot$spatial_obj
      
      global_data$spatial_datasets <- snapshot$spatial_datasets %||%
        (if (!is.null(snapshot$spatial_obj)) {
          stats::setNames(list(snapshot$spatial_obj), snapshot$spatial_obj$project %||% "Echantillon_1")
        } else list())
      
      global_data$active_spatial_dataset <- snapshot$active_spatial_dataset %||%
        (if (length(global_data$spatial_datasets) > 0) names(global_data$spatial_datasets)[1] else NULL)
      
      # v10 (backlog court-terme #2/#1) : absents des sessions sauvegardees
      
      # AVANT cette version -- %||% list()/NULL degrade proprement (cache
      
      # vide / pas de reference partagee) plutot que d'echouer sur un
      
      # snapshot ancien.
      
      global_data$spatial_results_cache <- snapshot$spatial_results_cache %||% list()
      
      global_data$spatial_reference     <- snapshot$spatial_reference %||% NULL
      
      global_data$spatial_multi_integration <- snapshot$spatial_multi_integration %||% NULL
      
      # F08 (revue externe 2026-10-28) : restaurer une session REMPLACE les
      # jeux SC et Bulk. Sans bump d'epoch, les résultats partagés du jeu
      # précédent survivent et le rapport consolidé mélange deux jeux.
      .ts_bump_all_epochs(global_data)
      
      
      
      saved_label <- if (!is.null(snapshot$saved_at)) {
        
        format(snapshot$saved_at, "%Y-%m-%d %H:%M")
        
      } else "date inconnue"
      
      
      
      showNotification(
        
        paste("✓ Session restaurée (sauvegardée le", saved_label, ")"),
        
        type = "message", duration = 6
        
      )
      
      
      
      if (!is.null(snapshot$spatial_obj)) {
        
        disk_ok <- !is.null(snapshot$spatial_obj$bpcells_dir) &&
          
          dir.exists(snapshot$spatial_obj$bpcells_dir)
        
        if (!disk_ok) {
          
          showNotification(
            
            paste0("⚠️ Session Spatial : donnees BPCells introuvables sur ce disque ",
                   
                   "(", snapshot$spatial_obj$bpcells_dir %||% "chemin inconnu", "). ",
                   
                   "Seule la vue \"sketch\" est disponible — reimportez pour relancer ",
                   
                   "clustering/deconvolution."),
            
            type = "warning", duration = 12
            
          )
          
        }
        
      }
      
      # v10 (backlog court-terme #1) : la reference scRNA-seq PARTAGEE est un
      
      # artefact sur DISQUE (tempdir(), voir prepare_reference_artifact())
      
      # -- jamais garanti survivre a un redemarrage/changement de machine,
      
      # contrairement au sketch (qui, lui, est bien serialise dans le .rds).
      
      # Meme logique d'avertissement que pour bpcells_dir ci-dessus.
      
      if (!is.null(global_data$spatial_reference)) {
        
        ref_disk_ok <- !is.null(global_data$spatial_reference$path) &&
          
          file.exists(global_data$spatial_reference$path)
        
        if (!ref_disk_ok) {
          
          showNotification(
            
            paste0("⚠️ Reference scRNA-seq partagee : artefact introuvable sur ce disque — ",
                   
                   "re-uploadez-la depuis l'onglet Import > Spatial avant de relancer RCTD/",
                   
                   "Label Transfer."),
            
            type = "warning", duration = 12
            
          )
          
          global_data$spatial_reference <- NULL
          
        }
        
      }
      
    }, error = function(e) {
      
      showNotification(
        
        paste("❌ Erreur lors du chargement de la session:", conditionMessage(e)),
        
        type = "error", duration = 10
        
      )
      
    })
    
  })
  
  
  
  # === AIDE DIDACTIQUE ===
  
  observeEvent(input$help_btn, {
    .tr_h <- function(k) { tr <- global_data$i18n; if (is.null(tr)) return(k); tryCatch(.strip_i18n_html(tr$t(k)), error=function(e) k) }
    showModal(modalDialog(
      
      title = .tr_h("📖 Guide d'Utilisation - Cerberus v1.0"),
      
      size = "l",
      
      easyClose = TRUE,
      
      
      
      tags$div(
        
        h4(.tr_h("🎯 Workflow Recommandé"), style = "color: #2C3E50;"),
        
        
        
        h5(.tr_h("1️⃣ Importation des Données")),
        
        p(.tr_h("Utilisez le menu 'Import Données' pour charger vos fichiers :")),
        
        tags$ul(
          
          tags$li(.tr_h("Single-Cell: Dossiers 10X CellRanger, fichiers .rds, .h5, .h5ad")),
          
          tags$li(.tr_h("RNA Bulk: Matrices de counts (CSV/TSV) + Métadonnées optionnelles")),
          
          tags$li(.tr_h("Spatial: Visium / Xenium / CosMx — converti automatiquement en matrice BPCells sur disque + echantillon RAM (\"sketch\"). Importez plusieurs echantillons pour activer l'onglet \"5. Multi-echantillons\" (comparaison/ integration Harmony conjointe)."))
          
        ),
        
        
        
        tags$div(class = "alert alert-info", style = "font-size: 0.9rem;",
                 
                 bsicons::bs_icon("lightbulb"), 
                 
                 .tr_h("Astuce: Pour Harmony (correction de batch), importez 2 échantillons ou plus dans l'onglet Single-Cell (ou Spatial, voir \"Multi-echantillons\").")
                 
        ),
        
        
        
        h5(.tr_h("Multi-échantillons (Single-Cell)")),
        
        tags$ul(
          
          tags$li(.tr_h("Importez plusieurs dossiers 10X / fichiers .rds / .h5 dans une même session : chaque import devient un échantillon distinct (orig.ident).")),
          
          tags$li(.tr_h("Harmony s'applique automatiquement sur l'identité d'échantillon (orig.ident) dès que ≥ 2 échantillons sont détectés (choisissez Harmony comme méthode de réduction).")),
          
          tags$li(.tr_h("Les analyses aval (pseudobulk, Milo, scCODA) exploitent l'identité d'échantillon pour éviter la pseudo-réplication."))
          
        ),
        
        
        
        hr(),
        
        
        
        h5(.tr_h("2️⃣ Analyse Single-Cell")),
        
        p(.tr_h("Suivez le workflow numéroté dans la barre latérale :")),
        
        tags$ol(
          
          tags$li(.tr_h("Pipeline: QC → Normalisation → Réduction dimensionnelle (UMAP/PCA/t-SNE/Harmony)")),
          
          tags$li(.tr_h("Annotation: Identification automatique des types cellulaires (SingleR)")),
          
          tags$li(.tr_h("Visualisation: Choix de plots interactifs (DimPlot, Violin, Heatmap, etc.)")),
          
          tags$li(.tr_h("Marqueurs: Recherche de gènes différentiels par cluster"))
          
        ),
        
        
        
        tags$div(class = "alert alert-success", style = "font-size: 0.9rem;",
                 
                 bsicons::bs_icon("star"), 
                 
                 .tr_h("Nouvelle fonctionnalité: Cliquez directement sur un gène dans le tableau des marqueurs pour l'ajouter automatiquement à la visualisation !")
                 
        ),
        
        
        
        hr(),
        
        
        
        h5(.tr_h("3️⃣ Analyses Bulk et Spatial")),
        
        tags$ul(
          
          tags$li(.tr_h("Bulk RNA: Analyse différentielle avec DESeq2/edgeR")),
          
          tags$li(.tr_h("Spatial: Pipeline auto (1 clic) OU QC → Clustering (BANKSY, asynchrone) → Deconvolution (RCTD/Label Transfer/STdeconvolve, asynchrone) → Visualisation WebGL → Multi-echantillons (integration Harmony conjointe, asynchrone) → Export (paquet .zip / script R reproductible)"))
          
        ),
        
        
        
        hr(),
        
        
        
        h5(.tr_h("💾 Gestion de la Mémoire")),
        
        p(.tr_h("Le bouton Nettoyer RAM permet de libérer la mémoire entre les analyses. Pour le Spatial, les calculs lourds (clustering, déconvolution, intégration multi-échantillons) s'exécutent dans des processus séparés (mirai).")),
        
        
        
        hr(),
        
        
        
        h5(.tr_h("📚 Ressources")),
        
        tags$ul(
          
          tags$li(tags$a(.tr_h("Documentation Seurat"), 
                         
                         href = "https://satijalab.org/seurat/", 
                         
                         target = "_blank")),
          
          tags$li(tags$a(.tr_h("SingleR Guide"), 
                         
                         href = "https://bioconductor.org/packages/release/bioc/vignettes/SingleR/inst/doc/SingleR.html", 
                         
                         target = "_blank"))
          
        )
        
      ),
      
      
      
      footer = tagList(
        
        modalButton(.tr_h("Fermer")),
        
        actionButton("reset_app", .tr_h("🔄 Réinitialiser l'App"), class = "btn-warning")
        
      )
      
    ))
    
  })
  
  
  
  # Réinitialisation complète
  
  observeEvent(input$reset_app, {
    
    showModal(modalDialog(
      
      title = "⚠️ Confirmation",
      
      "Êtes-vous sûr de vouloir effacer toutes les données chargées ?",
      
      footer = tagList(
        
        modalButton("Annuler"),
        
        actionButton("confirm_reset", "Oui, tout effacer", class = "btn-danger")
        
      )
      
    ))
    
  })
  
  
  
  observeEvent(input$confirm_reset, {
    
    global_data$sc_obj <- NULL

    global_data$sc_datasets <- list()

    global_data$bulk_obj <- NULL

    global_data$bulk_datasets <- list()

    global_data$bulk_multi_comparison <- NULL

    global_data$spatial_obj <- NULL
    
    global_data$spatial_datasets <- list()
    
    global_data$active_spatial_dataset <- NULL
    
    global_data$spatial_results_cache <- list()
    
    global_data$spatial_reference <- NULL
    
    global_data$spatial_multi_integration <- NULL
    
    # F08 (revue externe 2026-10-28) : tout effacer = remplacer les jeux par
    # rien. Le bump est ce qui déclenche la purge des résultats partagés
    # (mod_sc.R / mod_bulk.R sur observeEvent(<domaine>_obj_epoch)).
    .ts_bump_all_epochs(global_data)
    
    clean_mem()
    
    removeModal()
    
    showNotification("✓ Application réinitialisée", type = "message")
    
  })
  
  
  
  # === GESTION RAM ===
  
  output$mem_usage <- renderText({

    input$gc_btn

    invalidateLater(10000)  # Mise à jour toutes les 10 s — plus de gc() forcé dans le chemin de rendu (QW-1)

    mem_mb <- ts_process_rss_mb()
    if (is.na(mem_mb)) mem_mb <- round(sum(gc(full = FALSE)[, 2]))  # repli : tas R seul — gc()[, 2] est DÉJÀ en Mo, ne PAS diviser

    paste0("💾 ", mem_mb, " MB")

  })
  
  
  
  observeEvent(input$gc_btn, { 
    
    clean_mem() 
    .tr_gc <- function(k) { tr <- global_data$i18n; if (is.null(tr)) return(k); tryCatch(.strip_i18n_html(tr$t(k)), error=function(e) k) }
    showNotification(.tr_gc("🧹 Mémoire nettoyée"), type = "message", duration = 2)
    
  })
  
  
  
  # === PARAMÈTRES RAM AJUSTABLES À CHAUD ===
  
  observeEvent(input$apply_ram_settings, {
    
    options(future.globals.maxSize = input$ram_future_gb * 1024^3)
    
    options(shiny.maxRequestSize    = input$ram_upload_gb * 1024^3)
    .tr_ram <- function(k) { tr <- global_data$i18n; if (is.null(tr)) return(k); tryCatch(.strip_i18n_html(tr$t(k)), error=function(e) k) }
    showNotification(
      
      .t_fmt(.tr_ram("✓ Limites mises à jour : {a} Go (tâches parallèles), {b} Go (upload max)."),
              
              a = input$ram_future_gb, b = input$ram_upload_gb),
      
      type = "message", duration = 5
      
    )
    
  })
  
  # ── ASR-3 (O29) — checkpoints de session (toggle/bannière/ligne d'état) ───
  # Monté EN DERNIER : jamais avant la première instruction session$allowReconnect
  # (garde lexical S-RES-1, test-sres1-allow-reconnect.R). OFF par défaut =>
  # le module n'appelle AUCUNE fonction du moteur et ne rend rien (contrat
  # §4/§7.5) ; ON => scan événementiel unique, offre = plan ASR-2 validé,
  # acceptation = apply() dans global_data, refus = purge de l'offre seule.
  mod_session_checkpoint_server("session_ckpt", global_data)
  
}



# Lancement de l'application

shinyApp(ui, server)

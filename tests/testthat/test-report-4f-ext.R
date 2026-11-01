# =============================================================================
# test-report-4f-ext.R — Jalon 4F-ext : extension tri-domaine du rapport
# consolide (bulk / spatial) — TEST RED (étape 1)
# =============================================================================
# Mandat : docs/release/UPGRADE_AND_COMPATIBILITY.md §3c — « Étendre
# collect_consolidated_report_input() aux états bulk/spatial via expositions
# additives documentées + contrats résultat (bulk DE, spatial deconv).
# Pattern identique au Stage 17 ; aucun merge de significations. »
# docs/ROADMAP.md §2.0 row « → Flux C : 4F-ext » (parking §3c consommé).
#
# ── ARCHITECTURE GELÉE (décisions de coordination — en-tête contractuel) ────
# D1. DEUX domaines additifs figés, ajoutés à .report_analysis_domains à la
#     suite de bulk_multi_comparison : « bulk » et « spatial ». Aucun domaine
#     existant n'est renommé ni réordonné.
# D2. Le collecteur devient TRI-DOMAINE. La garde actuelle (refus si sc_obj
#     absent, tools/pkg_skeleton/R/report_collector.R:510) est REMPLACÉE :
#     refus UNIQUEMENT si AUCUN domaine porteur (pas d'objet SC, conteneur
#     bulk_datasets vide/absent ET état spatial vide/absent). sc_obj absent ->
#     les sections SC restent absent/NULL — JAMAIS fabriquées. Un rapport peut
#     être bulk-seul, spatial-seul, sc-seul ou mixte.
# D3. Section « bulk » = état bulk, sources DISTINCTES listées SANS fusion :
#       (a) chaque entrée du conteneur global_data$bulk_datasets — le contrat
#           BULK_MULTI s'applique : pipeline capturé = EXACTEMENT les
#           bulk_multi_pipeline_fields() (12 champs gelés,
#           docs/contracts/BULK_MULTI_CONTRACT.md ; le « dataset actif » du
#           mandat = les entrées enregistrées du conteneur, l'activation
#           restant une écriture du module MD-1, hors périmètre rapport) ;
#           traçabilité = registered_at/updated_at (motif existant du domaine
#           global auto-daté, cf. bulk_multi_comparison) ;
#       (b) shared_rv$pattern_result SI présent (contrat BULK_PATTERN —
#           résultat canonique type "bulk_pattern_clusters", analysis_id
#           "bulk-pattern-clusters", MOTIF DE PROVENANCE EXISTANT :
#           res$provenance$analysis_id) ;
#       (c) shared_rv$dose_result SI présent (contrat BULK_DOSE_RESPONSE —
#           type "bulk_dose_response", analysis_id "bulk-dose-response",
#           même motif de provenance).
#     Exposition ADDITIVE (pattern Stage 17 : mod_sc_velocity.R:544-549,
#     mod_sc_communication.R:328-331) : le collecteur LIT global_data /
#     shared_rv passés en arguments, ne RECALCULE rien, n'ÉCRIT rien.
# D4. Section « spatial » = état spatial : (a) global_data$spatial_datasets
#     (descriptif des datasets importés) ; (b) le résultat de déconvolution
#     retenu shared_rv$deconv_props (data.frame id + proportions,
#     modules/spatial/deconv/mod_spatial_deconv.R:199) AVEC SA PROVENANCE =
#     shared_rv$deconv_params (mode, ref_source_label, n_topics…,
#     modules/spatial/deconv/mod_spatial_deconv.R:169). deconv_props SANS
#     deconv_params -> verdict "blocked" (motif existant : une section sans
#     provenance est REFUSÉE, report_validator.R:89-91). Déconvolution
#     absente -> section honnête sans résultat (jamais fabriquée).
# D5. AUCUN merge de significations : des sections SÉPARÉES par domaine,
#     chacune avec sa provenance ; aucun champ d'une section ne porte une
#     valeur dérivée d'un autre domaine ; aucune écriture sur l'état lu.
# D6. Contrat : nouvelle row d'évolution dans
#     docs/contracts/CONSOLIDATED_REPORT_CONTRACT.md (cite 4F-ext + UPGRADE
#     §3c) ; les verdicts figés de cette extension sont couverts PAR CE TEST
#     (le test principal du domaine + le freeze suivront à l'étape GREEN —
#     règle contract-first : code + freeze test + doc simultanément).
#
# ÉTAT : RED→GREEN (2026-11-01) — à HEAD (55f8b76) : RED mesuré, T1-T4 et T6
# échouaient (rep_stop sur sc_obj NULL et domaines bulk/spatial absents du
# collecteur, zéro occurrence « bulk/spatial » — vérification indépendante),
# T5 passait par construction ; l'implémentation GREEN est le commit présent.
# =============================================================================

source_project_file("tools/pkg_skeleton/R/io_helpers.R")   # %||%
source_project_file("tools/pkg_skeleton/R/state.R")        # state_get + create_sc_shared_state
source_project_file("tools/pkg_skeleton/R/provenance.R")   # provenance_* + new_provenance_entry
source_project_file("tools/pkg_skeleton/R/error_log.R")    # ts_log_swallow (avalées du collecteur)
source_project_file("tools/pkg_skeleton/R/sc_velocity.R")  # build_object_identity_v2 (réutilisée, jamais ré-implémentée)
source_project_file("tools/pkg_skeleton/R/bulk_multi.R")   # conteneur bulk_datasets gelé (register/capture/pipeline_fields)
source_project_file("tools/pkg_skeleton/R/report_collector.R")
source_project_file("tools/pkg_skeleton/R/report_validator.R")
source_project_file("tools/pkg_skeleton/R/report_render.R")
source_project_file("tools/pkg_skeleton/R/report_bundle.R")

# ── Fixtures locaux ──────────────────────────────────────────────────────────

# Conteneur bulk_datasets : 2 entrées DISTINCTES produites par l'API gelée
# (bulk_multi_register) — "Jeu_A" avec pipeline capturé (12 champs
# BULK_MULTI), "Jeu_B" sans pipeline (register accepte NULL).
.f4_bulk_counts <- function() {
  matrix(seq_len(20L), nrow = 5L, ncol = 4L,
         dimnames = list(paste0("G", 1:5), paste0("b", 1:4)))
}
.f4_bulk_obj <- function() {
  list(counts = .f4_bulk_counts(),
       metadata = data.frame(condition = rep(c("A", "B"), each = 2L),
                             row.names = paste0("b", 1:4),
                             stringsAsFactors = FALSE))
}
.f4_bulk_pipeline <- function() {
  bulk_multi_capture_pipeline(list(
    mapping_applied = FALSE, mapping_summary = NULL,
    filtered_counts = .f4_bulk_counts(), vst_mat = NULL,
    contrasts = data.frame(contrast = "T2_vs_ctrl", stringsAsFactors = FALSE),
    active_contrast = "T2_vs_ctrl", multimethod_de = NULL,
    lfc_thresh = 1, padj_thresh = 0.05,
    pathway_results = NULL, pathway_db = NULL, pathway_mode = NULL
  ))
}
.f4_bulk_datasets <- function() {
  ds <- bulk_multi_register(NULL, "Jeu_A", .f4_bulk_obj(),
                            pipeline_state = .f4_bulk_pipeline())
  bulk_multi_register(ds, "Jeu_B", .f4_bulk_obj(), pipeline_state = NULL)
}

# Résultat pattern canonique (contrat BULK_PATTERN — 14 champs contractuels,
# dont provenance + analysis_id "bulk-pattern-clusters").
.f4_pattern_result <- function() {
  list(
    type = "bulk_pattern_clusters", status = "valid",
    clusters = data.frame(gene = c("G1", "G2"), cluster = c(1L, 2L),
                          stringsAsFactors = FALSE),
    cluster_profiles = data.frame(cluster = c(1L, 2L), b1 = c(0.1, 0.3),
                                  stringsAsFactors = FALSE),
    summary = list(n_genes = 2L, k = 2L),
    group_column = "condition", k = 2L, seed = 17L,
    parameters = list(kmax = 12L),
    qc = list(n_genes = 2L),
    warnings = character(0),
    provenance = new_provenance_entry(analysis_id = "bulk-pattern-clusters",
                                      method = "kmeans"),
    analysis_id = "bulk-pattern-clusters",
    timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

# Résultat dose-réponse canonique (contrat BULK_DOSE_RESPONSE — même motif).
.f4_dose_result <- function() {
  list(
    type = "bulk_dose_response", status = "valid",
    fits = data.frame(gene = "G1", logIC50 = 1, slope = -2, min = 0, max = 1,
                      stringsAsFactors = FALSE),
    curves = data.frame(dose = c(0, 1, 10), gene = "G1",
                        response = c(0.1, 0.5, 0.9),
                        stringsAsFactors = FALSE),
    data = data.frame(sample = c("s1", "s2"), dose = c(0, 10), G1 = c(0.1, 0.9),
                      stringsAsFactors = FALSE),
    dose_column = "dose", model = "LL.4",
    summary = list(n_genes_used = 1L, n_doses = 2L),
    parameters = list(dose_column = "dose", model = "LL.4"),
    qc = list(n_genes_used = 1L, n_samples = 2L, n_doses = 2L),
    warnings = character(0),
    provenance = new_provenance_entry(analysis_id = "bulk-dose-response",
                                      method = "drc"),
    analysis_id = "bulk-dose-response",
    timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

# Conteneur spatial_datasets : 1 paquet spatial (forme du producteur Import >
# Spatial — mod_import_spatial.R:711 : sketch + coords + n_total + technology).
.f4_spatial_datasets <- function() {
  spots <- paste0("spot", 1:4)
  list(Slice1 = list(
    technology = "visium",
    n_total = 4L,
    sketch = matrix(seq_len(20L), nrow = 5L, ncol = 4L,
                    dimnames = list(paste0("G", 1:5), spots)),
    coords = data.frame(id = spots, x = c(1, 2, 1, 2), y = c(1, 1, 2, 2),
                        stringsAsFactors = FALSE)
  ))
}
.f4_deconv_props <- function() {
  data.frame(id = paste0("spot", 1:4),
             Astro = c(0.7, 0.6, 0.2, 0.1), Neuron = c(0.3, 0.4, 0.8, 0.9),
             stringsAsFactors = FALSE)
}
.f4_deconv_params <- function() {
  list(mode = "rctd",
       ref_source_label = "reference partagee (Import > Spatial)",
       ref_path = "ref.rds", n_topics = NULL, n_top_od = 1000L,
       lt_npcs = 30L, lt_norm_method = "lognorm", lt_ncells = 3000L,
       rctd_n_hvg = 3000L)
}

.f4_summary_value <- function(entry, field) {
  v <- entry$summary$valeur[entry$summary$champ == field]
  if (length(v) == 1L) v else NA_character_
}

# ── T1 : bulk-seul ───────────────────────────────────────────────────────────

test_that("T1 bulk-seul : le conteneur bulk_datasets produit la section bulk et les sections SC ne sont JAMAIS fabriquées", {
  gd <- list(bulk_datasets = .f4_bulk_datasets())

  # RED : aujourd'hui le collecteur refuse (rep_stop "aucun objet Single-Cell")
  # dès que sc_obj est NULL — la garde :510 sc-only doit devenir tri-domaine.
  ri <- collect_consolidated_report_input(NULL, NULL, global_data = gd)

  # La section bulk est présente, construite depuis l'état passé en argument.
  bulk <- ri$analyses$bulk
  expect_identical(bulk$present, TRUE)
  expect_identical(.f4_summary_value(bulk, "n_datasets"), "2")

  # Contrat BULK_MULTI : le pipeline capturé du dataset porte EXACTEMENT les
  # 12 champs gelés — aucun champ ajouté, aucun fusionné.
  pipeline_a <- bulk$extras$datasets[["Jeu_A"]]$pipeline
  expect_setequal(names(pipeline_a), bulk_multi_pipeline_fields())
  expect_identical(length(names(pipeline_a)), 12L)
  expect_identical(pipeline_a$active_contrast, "T2_vs_ctrl")
  expect_identical(pipeline_a$lfc_thresh, 1)
  expect_identical(pipeline_a$padj_thresh, 0.05)
  # Sources DISTINCTES : les 2 entrées du conteneur sont listées séparément,
  # fidèles à l'état (pas de recalcul, pas de fusion).
  expect_setequal(names(bulk$extras$datasets), c("Jeu_A", "Jeu_B"))
  expect_identical(bulk$extras$datasets[["Jeu_A"]]$producer, "pipeline_save")
  expect_true(!is.null(bulk$extras$datasets[["Jeu_A"]]$registered_at))

  # sc_obj absent -> les sections SC restent absentes, JAMAIS fabriquées.
  sc_domains <- c("markers", "pseudobulk", "correlation", "pathways",
                  "trajectory", "velocity", "communication", "da_design",
                  "da_milo", "da_sccoda", "da_cross")
  for (dm in sc_domains) {
    expect_false(isTRUE(ri$analyses[[dm]]$present), info = dm)
  }
  expect_null(ri$dataset$fingerprint)

  # Verdict : motif existant du domaine global auto-daté (registered_at /
  # updated_at — cf. bulk_multi_comparison), jamais bloqué.
  val <- validate_consolidated_report_input(ri)
  expect_identical(val$verdicts$bulk$state, "valid_legacy")
  expect_true(val$ok_overall)
})

test_that("T1b bulk : pattern/dose SI présents — sources distinctes, copie fidèle, jamais fusionnées aux datasets", {
  gd <- list(bulk_datasets = .f4_bulk_datasets())
  sr <- create_sc_shared_state()
  # Fixtures capturés UNE fois (new_provenance_entry horodate à la creation —
  # une seconde invocation produirait un timestamp différent).
  pat_fixture <- .f4_pattern_result()
  dose_fixture <- .f4_dose_result()
  sr$pattern_result <- pat_fixture
  sr$dose_result <- dose_fixture

  ri <- collect_consolidated_report_input(NULL, sr, global_data = gd)
  bulk <- ri$analyses$bulk
  expect_identical(bulk$present, TRUE)
  # Chaque résultat est une source DISTINCTE, copiée fidèlement (aucun
  # recalcul — exposition additive, pattern Stage 17).
  expect_identical(bulk$extras$pattern_result, pat_fixture)
  expect_identical(bulk$extras$dose_result, dose_fixture)
  # Motif de provenance existant : les analysis_id des résultats à contrat
  # sont tracés dans la section.
  expect_true("bulk-pattern-clusters" %in% bulk$analysis_ids)
  expect_true("bulk-dose-response" %in% bulk$analysis_ids)
  # Aucune fusion : le pipeline 12 champs des datasets reste intact à côté.
  expect_setequal(names(bulk$extras$datasets[["Jeu_A"]]$pipeline),
                  bulk_multi_pipeline_fields())

  val <- validate_consolidated_report_input(ri)
  expect_false(val$verdicts$bulk$state %in% c("blocked", "invalid"))
  expect_true(val$ok_overall)
})

# ── T2 : spatial-seul ────────────────────────────────────────────────────────

test_that("T2 spatial-seul : datasets + deconv_props avec sa provenance (deconv_params)", {
  gd <- list(spatial_datasets = .f4_spatial_datasets())
  sr <- create_sc_shared_state()
  sr$deconv_params <- .f4_deconv_params()
  sr$deconv_props <- .f4_deconv_props()

  # RED : rep_stop sc-only aujourd'hui.
  ri <- collect_consolidated_report_input(NULL, sr, global_data = gd)
  spatial <- ri$analyses$spatial
  expect_identical(spatial$present, TRUE)
  # Résultat de déconvolution retenu : copie fidèle de l'état (aucun calcul).
  expect_identical(spatial$extras$deconv_props, .f4_deconv_props())
  # Datasets spatiaux listés comme sources distinctes.
  expect_setequal(names(spatial$extras$datasets), "Slice1")
  expect_identical(.f4_summary_value(spatial, "n_datasets"), "1")
  # Provenance du résultat retenu : deconv_params (motif existant — une
  # section tracée porte provenance_available + note d'identité).
  expect_true(spatial$provenance_available)
  expect_match(paste(spatial$identity_note, collapse = " "),
               "deconv_params", fixed = TRUE)
  expect_identical(.f4_summary_value(spatial, "mode_deconv"), "rctd")
  # Le domaine SC reste absent (jamais fabriqué) et l'empreinte SC nulle.
  expect_false(isTRUE(ri$analyses$velocity$present))
  expect_null(ri$dataset$fingerprint)

  val <- validate_consolidated_report_input(ri)
  expect_identical(val$verdicts$spatial$state, "valid")
  expect_true(val$ok_overall)
})

test_that("T2b spatial sans déconvolution : section honnête (datasets seuls), résultat jamais fabriqué", {
  gd <- list(spatial_datasets = .f4_spatial_datasets())

  ri <- collect_consolidated_report_input(NULL, create_sc_shared_state(),
                                          global_data = gd)
  spatial <- ri$analyses$spatial
  expect_identical(spatial$present, TRUE)          # l'état spatial existe
  expect_null(spatial$extras$deconv_props)         # résultat absent -> NULL
  expect_false("mode_deconv" %in% spatial$summary$champ)
  expect_true("n_datasets" %in% spatial$summary$champ)
  expect_setequal(names(spatial$extras$datasets), "Slice1")

  val <- validate_consolidated_report_input(ri)
  expect_identical(val$verdicts$spatial$state, "valid_legacy")
  expect_true(val$ok_overall)
})

# ── T3 : mixte sc + bulk + spatial ───────────────────────────────────────────

test_that("T3 mixte sc+bulk+spatial : trois familles de sections séparées, provenances distinctes, état lu non muté", {
  obj <- .da_stub_obj()
  sr <- create_sc_shared_state()
  sr$markers_data <- data.frame(gene = c("g1", "g2"), cluster = c("1", "2"),
                                avg_log2FC = c(1.1, 0.8),
                                stringsAsFactors = FALSE)
  provenance_append(sr, new_provenance_entry(
    analysis_id = "sc-markers-test", method = "FindAllMarkers", dataset = obj))
  sr$pattern_result <- .f4_pattern_result()
  sr$deconv_params <- .f4_deconv_params()
  sr$deconv_props <- .f4_deconv_props()
  gd <- list(bulk_datasets = .f4_bulk_datasets(),
             spatial_datasets = .f4_spatial_datasets())

  # Snapshots AVANT (expositions additives : lecture seule, pattern Stage 17).
  before <- list(
    markers = shiny::isolate(sr$markers_data),
    pattern = shiny::isolate(sr$pattern_result),
    deconv_props = shiny::isolate(sr$deconv_props),
    deconv_params = shiny::isolate(sr$deconv_params),
    prov_nrow = nrow(provenance_to_dataframe(sr))
  )

  ri <- collect_consolidated_report_input(obj, sr, global_data = gd)

  # Les trois familles coexistent, chacune dans SA section.
  expect_true(isTRUE(ri$analyses$markers$present))
  expect_true(isTRUE(ri$analyses$bulk$present))
  expect_true(isTRUE(ri$analyses$spatial$present))

  # Provenances distinctes : le SC trace par la provenance partagée, bulk
  # par registered_at + provenance des résultats à contrat, spatial par
  # deconv_params — aucune source ne se substitue à une autre.
  expect_true("bulk-pattern-clusters" %in% ri$analyses$bulk$analysis_ids)
  expect_true(ri$analyses$spatial$provenance_available)
  expect_identical(ri$analyses$markers$extras$table, before$markers)

  # AUCUNE écriture sur l'état lu (expositions additives, D5).
  expect_identical(shiny::isolate(sr$markers_data), before$markers)
  expect_identical(shiny::isolate(sr$pattern_result), before$pattern)
  expect_identical(shiny::isolate(sr$deconv_props), before$deconv_props)
  expect_identical(shiny::isolate(sr$deconv_params), before$deconv_params)
  expect_identical(nrow(provenance_to_dataframe(sr)), before$prov_nrow)
  expect_identical(names(gd$bulk_datasets), c("Jeu_A", "Jeu_B"))

  val <- validate_consolidated_report_input(ri)
  expect_true(val$ok_overall)
  states <- vapply(val$verdicts, function(v) v$state, character(1))
  expect_identical(states[["markers"]], "valid_legacy")
  expect_identical(states[["bulk"]], "valid_legacy")
  expect_identical(states[["spatial"]], "valid")
})

# ── T4 : motif de provenance sur les nouvelles sections ──────────────────────

test_that("T4 provenance : une nouvelle section sans provenance est REFUSÉE par le validateur (motif existant re-mesuré)", {
  gd <- list(bulk_datasets = .f4_bulk_datasets())

  # bulk : pattern_result présent SANS provenance -> section refusée.
  pat_no_prov <- .f4_pattern_result()
  pat_no_prov$provenance <- NULL
  sr <- create_sc_shared_state()
  sr$pattern_result <- pat_no_prov
  ri <- collect_consolidated_report_input(NULL, sr, global_data = gd)
  val <- validate_consolidated_report_input(ri)
  expect_identical(val$verdicts$bulk$state, "blocked")
  expect_true("bulk" %in% val$blocked_sections)
  expect_false(val$ok_overall)

  # Re-mesure : même section AVEC sa provenance -> pas de refus.
  sr2 <- create_sc_shared_state()
  sr2$pattern_result <- .f4_pattern_result()
  ri2 <- collect_consolidated_report_input(NULL, sr2, global_data = gd)
  val2 <- validate_consolidated_report_input(ri2)
  expect_false(val2$verdicts$bulk$state %in% c("blocked", "invalid"))

  # spatial : deconv_props SANS deconv_params (sa provenance) -> refusée.
  gd_sp <- list(spatial_datasets = .f4_spatial_datasets())
  sr3 <- create_sc_shared_state()
  sr3$deconv_props <- .f4_deconv_props()
  ri3 <- collect_consolidated_report_input(NULL, sr3, global_data = gd_sp)
  val3 <- validate_consolidated_report_input(ri3)
  expect_identical(val3$verdicts$spatial$state, "blocked")
  expect_true("spatial" %in% val3$blocked_sections)
  expect_false(val3$ok_overall)
})

# ── T5 : anti-régression — le chemin SC-seul est inchangé ────────────────────

test_that("T5 anti-régression : le chemin SC-seul rend exactement les mêmes sections qu'avant (contrat inchangé sur SC)", {
  obj <- .da_stub_obj()

  # Les 12 domaines historiques restent présents, dans l'ordre figé d'origine
  # (les 2 nouveaux domaines s'ajoutent APRÈS bulk_multi_comparison).
  expect_identical(
    head(consolidated_report_analyses(), 12L),
    c("markers", "pseudobulk", "correlation", "pathways", "trajectory",
      "velocity", "communication", "da_design", "da_milo", "da_sccoda",
      "da_cross", "bulk_multi_comparison"))
  expect_setequal(names(
    collect_consolidated_report_input(obj, create_sc_shared_state())$analyses),
    consolidated_report_analyses())

  # Projet SC vide : toutes les sections SC absentes, gracieusement.
  ri0 <- collect_consolidated_report_input(obj, create_sc_shared_state())
  val0 <- validate_consolidated_report_input(ri0)
  expect_true(all(vapply(val0$verdicts, function(v) v$state == "absent",
                         logical(1))))

  # Verdict legacy SC inchangé (markers présent -> valid_legacy).
  sr <- create_sc_shared_state()
  sr$markers_data <- data.frame(gene = "g1", cluster = "1", avg_log2FC = 1,
                                stringsAsFactors = FALSE)
  ri <- collect_consolidated_report_input(obj, sr)
  val <- validate_consolidated_report_input(ri)
  expect_identical(val$verdicts$markers$state, "valid_legacy")

  # Les 7 états de validation restent figés.
  expect_setequal(consolidated_report_validation_states(),
                  c("absent", "valid", "valid_legacy", "stale", "invalid",
                    "unknown", "blocked"))

  # Sans état bulk/spatial : les nouvelles sections sont ABSENTES, jamais
  # fabriquées (sc-seul reste un rapport SC, rien d'autre).
  expect_identical(ri$analyses$bulk$present, FALSE)
  expect_identical(ri$analyses$spatial$present, FALSE)
})

# ── T6 : aucun merge entre domaines (assertion structurelle) ─────────────────

test_that("T6 aucun merge : aucun champ d'une section ne porte une valeur dérivée d'un autre domaine", {
  obj <- .da_stub_obj()
  sr <- create_sc_shared_state()
  sr$markers_data <- data.frame(gene = c("g1", "g2"), cluster = c("1", "2"),
                                avg_log2FC = c(1.1, 0.8),
                                stringsAsFactors = FALSE)
  sr$pattern_result <- .f4_pattern_result()
  sr$deconv_params <- .f4_deconv_params()
  sr$deconv_props <- .f4_deconv_props()
  gd <- list(bulk_datasets = .f4_bulk_datasets(),
             spatial_datasets = .f4_spatial_datasets())

  ri <- collect_consolidated_report_input(obj, sr, global_data = gd)
  txt <- function(entry) paste(unlist(entry$summary), collapse = " | ")
  bulk_txt <- txt(ri$analyses$bulk)
  spatial_txt <- txt(ri$analyses$spatial)
  sc_txt <- txt(ri$analyses$markers)

  # Les gènes SC ne migrent dans aucune section bulk/spatial…
  for (g in c("g1", "g2")) {
    expect_false(grepl(g, bulk_txt, fixed = TRUE), info = g)
    expect_false(grepl(g, spatial_txt, fixed = TRUE), info = g)
  }
  # …l'état spatial ne migre pas dans bulk, ni l'état bulk dans spatial…
  expect_false(grepl("Slice1", bulk_txt, fixed = TRUE))
  expect_false(grepl("Jeu_A", spatial_txt, fixed = TRUE))
  expect_false(grepl("Jeu_B", spatial_txt, fixed = TRUE))
  # …et aucun champ d'une section ne porte la valeur d'un autre domaine :
  # la section bulk n'expose rien du spatial (ni réciproquement).
  expect_null(ri$analyses$bulk$extras$deconv_props)
  expect_null(ri$analyses$bulk$extras$deconv_params)
  expect_setequal(names(ri$analyses$bulk$extras$datasets), c("Jeu_A", "Jeu_B"))
  expect_setequal(names(ri$analyses$spatial$extras$datasets), "Slice1")

  # Aucun merge de significations : les marqueurs SC restent EXACTEMENT ceux
  # de l'état SC (les DEGs bulk du conteneur ne sont PAS fondus dedans).
  expect_identical(ri$analyses$markers$extras$table,
                   shiny::isolate(sr$markers_data))
  expect_identical(nrow(ri$analyses$markers$extras$table), 2L)

  # Le rapport lui-même reste un compilateur : type et analysis_id inchangés.
  expect_identical(ri$type, "consolidated_report_input")
  expect_identical(ri$analysis_id, "sc-report-consolide")
})

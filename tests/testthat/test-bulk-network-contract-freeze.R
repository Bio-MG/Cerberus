# =============================================================================
# test-bulk-network-contract-freeze.R — NEW-3 : gel du contrat
# docs/contracts/BULK_NETWORK_CONTRACT.md (réseau PCSF / interactome)
# =============================================================================
# Verrouille : surface publique (noms + signatures), états d'erreur (code <>
# contrat), PURETÉ Shiny du moteur, réutilisation stricte d'igraph (jamais
# réimplémenté), module qui n'écrit QUE shared_rv$network_result, ancres app.R +
# mod_bulk.R, seuils DÉCLARÉS dans config/ (jamais en dur), invariant de forêt,
# honnêteté de l'heuristique, contrat MD-1 INCHANGÉ (network_result hors
# snapshot), sync code <-> contrat, clés i18n.
# =============================================================================
source_project_file("R/core/io_helpers.R")      # %||%
source_project_file("R/bulk/bulk_network.R")

.ts_top_level_names <- function(relpath) {
  exprs <- parse(file.path(ts_project_root(), relpath), keep.source = FALSE)
  nms <- character(0)
  for (e in exprs) {
    if (is.call(e) && identical(e[[1L]], as.name("<-")) && is.name(e[[2L]])) {
      nms <- c(nms, as.character(e[[2L]]))
    }
  }
  unique(nms)
}

# ⚠️ Les blancs sont NORMALISÉS (retours à la ligne et indentation -> une seule
# espace). Sans cela, une assertion sur une expression de plusieurs mots
# échoue dès que le markdown la coupe en fin de ligne — « rien ne se relierait
# jamais » et « PAS un PPI » sont réellement présents dans le contrat, mais
# répartis sur deux lignes (payé le 2026-09-16 : 3 faux échecs).
.ts_read <- function(relpath) {
  if (grepl("^docs/contracts/", relpath) &&
      !file.exists(file.path(ts_project_root(), relpath))) {
    skip(.ts_contract_skip_msg(relpath))
  }
  txt <- paste(readLines(file.path(ts_project_root(), relpath), warn = FALSE),
               collapse = "\n")
  gsub("[[:space:]]+", " ", txt)
}

.engine_src  <- .ts_read("R/bulk/bulk_network.R")
.module_src  <- .ts_read("modules/bulk/mod_bulk_network.R")
.contract    <- .ts_read("docs/contracts/BULK_NETWORK_CONTRACT.md")
.app_src     <- .ts_read("app.R")
.parent_src  <- .ts_read("modules/bulk/mod_bulk.R")
.thresh_src  <- .ts_read("config/thresholds.R")

# ── Surface publique gelée (contrat §10) ───────────────────────────────────
test_that("public API surface is frozen (names + signatures)", {
  expect_setequal(bulk_network_public_api(), c(
    "assert_bulk_network_object", "assert_bulk_network_result",
    "build_bulk_network_table_export", "bulk_network_contract_fields",
    "bulk_network_error_state", "bulk_network_map_ids",
    "bulk_network_memo_clear", "bulk_network_node_roles",
    "bulk_network_pcsf_params", "bulk_network_pcsf_params_default",
    "bulk_network_public_api", "bulk_network_source_available",
    "bulk_network_species_supported",
    "bulk_network_species_unavailable_reason", "bulk_network_validity_states",
    "load_bulk_network", "plot_bulk_network", "run_bulk_network_pcsf"))
  # triée : tout ajout/retrait se voit dans le diff sans dépendre de l'ordre
  expect_identical(bulk_network_public_api(), sort(bulk_network_public_api()))
  expect_setequal(bulk_network_public_api(),
                  intersect(bulk_network_public_api(),
                            .ts_top_level_names("R/bulk/bulk_network.R")))
  # tout top-level NON pointé est public (aucune fonction cachée)
  tl <- .ts_top_level_names("R/bulk/bulk_network.R")
  expect_setequal(tl[!startsWith(tl, ".")], bulk_network_public_api())

  # ⚠️ `names(formals(f))` vaut NULL — et non character(0) — pour une fonction
  # SANS argument : utiliser character(0) ferait échouer les 7 fonctions sans
  # paramètre (payé le 2026-09-16).
  expected_args <- list(
    bulk_network_contract_fields         = NULL,
    bulk_network_validity_states         = NULL,
    bulk_network_node_roles              = NULL,
    bulk_network_error_state             = "e",
    bulk_network_species_supported       = NULL,
    bulk_network_species_unavailable_reason = "species",
    bulk_network_source_available        = "species",
    load_bulk_network                    = c("species", "source", "refresh"),
    bulk_network_map_ids                 = c("ids", "species"),
    bulk_network_memo_clear              = NULL,
    assert_bulk_network_object           = c("network", "context"),
    bulk_network_public_api              = NULL,
    bulk_network_pcsf_params_default     = NULL,
    bulk_network_pcsf_params             = "overrides",
    run_bulk_network_pcsf                = c("prizes", "network", "species",
                                             "threshold", "params",
                                             "convert_ids"),
    assert_bulk_network_result           = c("result", "context"),
    plot_bulk_network                    = c("result", "tr", "max_label_nodes",
                                             "layout_seed"),
    build_bulk_network_table_export      = "result"
  )
  for (fn_name in names(expected_args)) {
    expect_true(exists(fn_name, where = globalenv(), inherits = FALSE),
                info = paste("fonction publique manquante :", fn_name))
    fn <- get(fn_name, envir = globalenv())
    expect_true(is.function(fn), info = fn_name)
    expect_identical(names(formals(fn)), expected_args[[fn_name]],
                     info = fn_name)
  }
})

# ── États d'erreur figés (contrat §9) ──────────────────────────────────────
test_that("error states are frozen in code AND contract (no silent inflation)", {
  states_code <- c("invalid_input", "source_unavailable", "missing_dependency",
                   "empty_source", "no_overlap", "too_many_prizes",
                   "contract_violation")
  emitted <- unique(regmatches(
    .engine_src,
    gregexpr('(?<=state = ")[^"]+', .engine_src, perl = TRUE))[[1L]])
  expect_true(all(emitted %in% states_code),
              info = paste("états hors ensemble gelé :",
                           paste(setdiff(emitted, states_code), collapse = ", ")))
  # les états RÉELLEMENT émis doivent tous être documentés
  for (st in emitted) {
    expect_match(.contract, st, fixed = TRUE,
                 info = paste("état absent du contrat :", st))
  }
  expect_match(.engine_src, 'class = "bulk_network_error"', fixed = TRUE)
})

# ── Pureté Shiny du moteur (contrat §2, R/ = logique pure) ─────────────────
test_that("engine is pure R (no Shiny symbols)", {
  shiny_patterns <- c("input\\$", "output\\$", "renderPlot", "renderUI",
                      "renderDT", "observeEvent", "observe\\(", "reactive\\(",
                      "reactiveVal", "reactiveValues", "moduleServer",
                      "showNotification", "NS\\(", "session\\$", "req\\(",
                      "isolate\\(", "Progress\\$")
  for (pat in shiny_patterns) {
    expect_false(grepl(pat, .engine_src),
                 info = paste("symbole Shiny détecté dans le moteur :", pat))
  }
})

# ── Réutilisation stricte (contrat §2) : igraph consommé, jamais réécrit ───
test_that("engine consumes igraph and never reimplements graph algorithms", {
  for (sym in c("igraph::graph_from_data_frame", "igraph::distances",
                "igraph::shortest_paths", "igraph::components")) {
    expect_match(.engine_src, sym, fixed = TRUE,
                 info = paste("primitive igraph réutilisée absente :", sym))
  }
  # Pas de réimplémentation « à la main » d'un parcours de graphe : aucun
  # empilement/file de frontière nommé comme tel dans le moteur.
  expect_false(grepl("BFS|Dijkstra|Floyd", .engine_src),
               info = "algorithme de graphe réimplémenté à la main — utiliser igraph")
  # AnnotationDbi/org.Hs.eg.db réutilisés, jamais réécrits
  for (sym in c("AnnotationDbi::mapIds", "AnnotationDbi::keys",
                "org.Hs.eg.db")) {
    expect_match(.engine_src, sym, fixed = TRUE,
                 info = paste("réutilisation absente :", sym))
  }
  # graphite / reactome.db : lecture hors ligne, jamais de téléchargement
  expect_match(.engine_src, "graphite::pathways", fixed = TRUE)
  expect_match(.engine_src, "graphite::edges", fixed = TRUE)
  expect_false(grepl("download|url\\(|http", .engine_src, ignore.case = TRUE),
               info = "accès réseau détecté dans le moteur — interdit (local-first)")
})

test_that("module reuses the engine and never redefines it", {
  for (sym in c("load_bulk_network", "run_bulk_network_pcsf",
                "assert_bulk_network_result", "plot_bulk_network",
                "build_bulk_network_table_export", "bulk_network_pcsf_params",
                "ts_datatable", ".strip_i18n_html")) {
    expect_match(.module_src, sym, fixed = TRUE,
                 info = paste("symbole réutilisé absent du module :", sym))
  }
  for (sym in c("load_bulk_network", "run_bulk_network_pcsf",
                "plot_bulk_network", "build_bulk_network_table_export")) {
    expect_false(grepl(paste0(sym, "\\s*<-\\s*function"), .module_src),
                 info = paste("redéfinition interdite dans le module :", sym))
  }
})

# ── Module : n'écrit QUE son propre résultat (contrat §11) ────────────────
test_that("module writes ONLY shared_rv$network_result", {
  expect_match(.module_src, "shared_rv$network_result <- res", fixed = TRUE)
  expect_false(grepl("global_data\\$bulk_obj\\s*<-", .module_src),
               info = "écriture interdite sur global_data$bulk_obj")
  expect_false(grepl("bulk_datasets\\s*<-", .module_src),
               info = "écriture interdite sur le conteneur bulk_datasets")
  expect_false(grepl("bulk_multi_comparison", .module_src),
               info = "écriture interdite sur bulk_multi_comparison")
  # Aucun calcul de graphe dans le module : tout passe par le moteur
  expect_false(grepl("igraph::", .module_src),
               info = "igraph utilisé dans le module — la logique vit dans R/")
})

# ── Ancres de câblage (contrat §11) ───────────────────────────────────────
test_that("app.R and mod_bulk.R anchors are present", {
  expect_match(.app_src, 'source("R/bulk/bulk_network.R")', fixed = TRUE)
  expect_match(.app_src, 'source("modules/bulk/mod_bulk_network.R")', fixed = TRUE)
  expect_match(.parent_src, 'value = "panel_network"', fixed = TRUE)
  expect_match(.parent_src, 'value = "tab_bulk_network"', fixed = TRUE)
  expect_match(.parent_src, 'mod_bulk_network_ui(ns("network"))', fixed = TRUE)
  expect_match(.parent_src, 'mod_bulk_network_output_ui(ns("network"))',
               fixed = TRUE)
  expect_match(.parent_src,
               'mod_bulk_network_server("network", global_data, shared_rv)',
               fixed = TRUE)
})

# ── Seuils DÉCLARÉS (contrat §8) : en config, consommés par leur nom ──────
test_that("thresholds are declared in config and consumed by name", {
  for (th in c("TS_BULK_NETWORK_MIN_MAP_RATE", "TS_BULK_NETWORK_MAX_NODES")) {
    expect_match(.thresh_src, th, fixed = TRUE,
                 info = paste("seuil déclaré absent de config/ :", th))
    expect_match(.engine_src, th, fixed = TRUE,
                 info = paste("seuil non consommé par le moteur :", th))
    expect_match(.contract, th, fixed = TRUE,
                 info = paste("seuil absent du contrat :", th))
  }
  # consommés via le helper (motif du dépôt : exists(inherits = TRUE))
  expect_match(.engine_src, "exists(name, inherits = TRUE)", fixed = TRUE)
  expect_match(.engine_src, ".bulk_network_config", fixed = TRUE)
  # plafond mesuré : 200 primes, valeur gelée
  expect_identical(TS_BULK_NETWORK_MAX_NODES, 200L)
})

# ── Contrat MD-1 INCHANGÉ : network_result hors du snapshot multi-jeux ────
test_that("the frozen MD-1 snapshot field set is NOT modified (contrat §11.1)", {
  source_project_file("R/bulk/bulk_multi.R")
  expect_identical(bulk_multi_pipeline_fields(), c(
    "mapping_applied", "mapping_summary", "filtered_counts", "vst_mat",
    "contrasts", "active_contrast", "multimethod_de",
    "lfc_thresh", "padj_thresh", "pathway_results", "pathway_db",
    "pathway_mode"))
  # et le module NEW-3 ne prétend pas le contraire
  expect_false(grepl("bulk_multi_pipeline_fields", .module_src),
               info = "le module NEW-3 ne doit pas toucher au snapshot MD-1")
  expect_match(.contract, "bulk_multi_pipeline_fields", fixed = TRUE)
})

# ── Honnêteté de l'heuristique (contrat §6, §13.4) ────────────────────────
test_that("the engine states it is a HEURISTIC, never an optimum", {
  expect_match(.engine_src, "HEURISTIQUE, pas un optimum", fixed = TRUE)
  # le contrat le dit dans son titre de §6 ET dans ses limites (§13.4)
  expect_match(.contract, "PAS un optimum", fixed = TRUE)
  expect_match(.contract, "pas optimum", fixed = TRUE)
  expect_match(.module_src, "heuristique", fixed = TRUE)
  # le verdict est porté par le RÉSULTAT, pas seulement par un commentaire
  net <- list(edges = data.frame(from = c("A", "B"), to = c("B", "C"),
                                 stringsAsFactors = FALSE),
              nodes = c("A", "B", "C"), n_edges = 2L, n_nodes = 3L,
              mean_degree = 4 / 3, density = 2 / 3, n_pathways = 1L,
              n_raw_edges = 2L, n_dropped_non_protein = 0L,
              source_db = "reactome", source_version = "test",
              id_type = "UNIPROT", build_seconds = NA_real_, from_memo = TRUE)
  r <- run_bulk_network_pcsf(c(A = 5, C = 5), network = net, convert_ids = FALSE)
  expect_match(r$qc$algorithm, "HEURISTIQUE", fixed = TRUE)
  # et AUCUN optimum n'est revendiqué nulle part
  expect_false(grepl("optimal", r$qc$algorithm, ignore.case = TRUE))
})

# ── Invariant de forêt + rôles gelés (contrat §4.1, §4.2) ─────────────────
test_that("forest invariant and node roles are frozen", {
  expect_setequal(bulk_network_node_roles(), c("terminal", "relay"))
  expect_setequal(bulk_network_validity_states(),
                  c("valid", "valid_with_warnings"))
  expect_setequal(bulk_network_contract_fields(), c(
    "type", "status", "nodes", "edges", "prizes", "node_role", "species",
    "source_db", "source_version", "id_type", "map_rate", "parameters",
    "qc", "warnings", "provenance", "analysis_id", "timestamp_utc"))
  # invariant de forêt documenté (token simple : le contrat utilise un signe
  # moins U+2212, qu'un `fixed = TRUE` sur la formule entière rendrait fragile)
  expect_match(.contract, "forêt", fixed = TRUE)
  expect_match(.contract, "qc$n_trees", fixed = TRUE)
  # le moteur vérifie LUI-MÊME sa conformité contractuelle
  expect_match(.engine_src, "contract_violation", fixed = TRUE)
})

# ── Critère de rentabilité gelé (contrat §6.3) ────────────────────────────
test_that("the profitability criterion is frozen and legible", {
  # ω=10, β=1, μ=1 -> max_join_hops = 5 (défauts calibrés, mesurés)
  p <- bulk_network_pcsf_params()
  expect_identical(p$omega, 10)
  expect_identical(p$beta, 1)
  expect_identical(p$mu, 1)
  expect_identical(p$max_join_hops, 5)
  # ω=1 est le cas DÉGÉNÉRÉ : le contrat doit l'expliquer, pas le subir
  expect_identical(bulk_network_pcsf_params(list(omega = 1))$max_join_hops, 0)
  expect_match(.contract, "rien ne se relierait jamais", fixed = TRUE)
  expect_match(.engine_src, "ω > β·d + μ·(d−1)", fixed = TRUE)
  # l'UI affiche le critère en clair
  expect_match(.module_src, "max_join_hops", fixed = TRUE)
})

# ── Sync code <> contrat ─────────────────────────────────────────────────
test_that("contract document is in sync (key sections present)", {
  skip_if_not(file.exists(file.path(ts_project_root(), "docs/contracts/BULK_NETWORK_CONTRACT.md")), .ts_contract_skip_msg("docs/contracts/BULK_NETWORK_CONTRACT.md"))
  for (token in c("bulk_network_contract_fields", "load_bulk_network",
                  "bulk_network_map_ids", "run_bulk_network_pcsf",
                  "plot_bulk_network", "build_bulk_network_table_export",
                  "assert_bulk_network_result", "bulk_network_public_api",
                  "bulk_network_error", "no_overlap", "too_many_prizes",
                  "contract_violation", "graphite", "reactome.db",
                  "mmuReactome.db", "org.Hs.eg.db",
                  "292 895", "11 030", "53,1", "max_join_hops",
                  "PAS un PPI", "Réseau PCSF (interactome)",
                  "3g. Réseau PCSF (interactome)", "tab_bulk_network")) {
    expect_match(.contract, token, fixed = TRUE,
                 info = paste("token absent du contrat :", token))
  }
  # les corrections de mesure sont documentées (pas seulement les chiffres)
  expect_match(.contract, "19 107", fixed = TRUE)
  expect_match(.contract, "1,7", fixed = TRUE)
})

# ── Clés i18n (contrat §11) ──────────────────────────────────────────────
test_that("i18n keys used by the module exist in translation.json", {
  json <- jsonlite::fromJSON(
    file.path(ts_project_root(), "i18n/translation.json"),
    simplifyVector = FALSE)$translation
  fr_keys <- vapply(json, function(x) x$fr %||% "", character(1))
  needed <- c(
    "3g. Réseau PCSF (interactome)", "Réseau PCSF",
    "Sous-réseau PCSF (heuristique)",
    "Interactome dérivé des voies Reactome — ce n'est PAS un PPI.",
    "Espèce (déclarée)", "Humain (hsapiens)", "Source des gènes d'intérêt",
    "Prime (score du nœud)", "-log10(padj) — recommandé",
    "|log2FC| — direction ignorée", "Seuil de prime (déclaré)",
    "Paramètres du moteur (heuristique)", "ω (ouverture d'arbre)",
    "β (par arête)", "μ (par relais)",
    "Deux groupes ne sont reliés que s'ils sont séparés par au plus {d} arête(s).",
    "Calculer le sous-réseau", "Nœuds", "Paramètres & QC", "Export CSV",
    "Erreur réseau PCSF:",
    "Chargement du réseau de voies (Reactome, hors ligne)...",
    "Calcul du sous-réseau PCSF (heuristique)...",
    "{n} avertissement(s).")
  for (k in needed) {
    expect_true(k %in% fr_keys, info = paste("clé i18n manquante :", k))
  }
})

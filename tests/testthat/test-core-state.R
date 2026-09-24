# =============================================================================
# test-core-state.R — contrat d'etat R/core/state.R (CHRYSALIS 2A + step 3)
# =============================================================================
# Point cle couvert ici : les accesseurs state_get()/state_set()/state_has()
# doivent fonctionner HORS contexte reactif (scripts/tests) SANS casser la
# trace de dependance DANS l'app (test d'invalidation via compteur).
# Step 3 ajoute : validateurs de forme is_analysis_state()/assert_analysis_state(),
# schema SC integral, et etancheite entre instances (aucune fuite d'etat).

source_project_file("R/core/state.R")

# ---------------------------------------------------------------------------
# Fabriques de schema domaine (consolidation existante)
# ---------------------------------------------------------------------------
test_that("create_sc_shared_state preserves every documented SC shared field", {
  s <- create_sc_shared_state()
  expect_s3_class(s, "reactivevalues")
  expect_setequal(
    shiny::isolate(names(s)),   # names() exige un contexte reactif dans shiny
    c("markers_data", "correlated_genes", "corr_target_gene", "pathway_results",
      "pathway_db", "selected_genes", "active_tab", "report_viz_list",
      "traj_reduction", "traj_method", "traj_genes", "max_cells_heavy",
      "sc_palette", "sc_manual_colors", "sc_manual_gradient",
      "sc_manual_volcano_colors", "communication_result",
      "communication_collection", "active_communication_sample")
  )
  for (f in c("markers_data", "correlated_genes", "corr_target_gene",
              "pathway_results", "pathway_db", "active_tab",
              "traj_reduction", "traj_method", "sc_manual_colors",
               "sc_manual_gradient", "sc_manual_volcano_colors",
               "communication_result", "communication_collection",
               "active_communication_sample")) {
    expect_null(state_get(s, f), info = f)
  }
  expect_identical(state_get(s, "selected_genes"), character(0))
  expect_identical(state_get(s, "traj_genes"), character(0))
  expect_identical(state_get(s, "report_viz_list"), list())
  expect_identical(state_get(s, "max_cells_heavy"), Inf)
  expect_identical(state_get(s, "sc_palette"), "default")
})

test_that("create_spatial_shared_state exposes the spatial cache schema", {
  s <- create_spatial_shared_state()
  expect_s3_class(s, "reactivevalues")
  for (f in c("qc_metrics", "qc_params", "cluster_labels", "cluster_params",
              "deconv_props", "moran_results", "niche_labels", "umap_df")) {
    expect_null(state_get(s, f), info = f)
  }
  expect_identical(state_get(s, "saved_viz_list"), list())
})

# ---------------------------------------------------------------------------
# create_analysis_state() — contrat 5 emplacements (2A + step 3)
# ---------------------------------------------------------------------------
test_that("create_analysis_state creates the 5-slot contract for every domain", {
  for (d in c("sc", "bulk", "spatial")) {
    s <- create_analysis_state(d)
    expect_s3_class(s, "reactivevalues")
    expect_identical(attr(s, "domain"), d)
    for (slot in c("input", "preprocessing", "results", "visualization")) {
      expect_null(state_get(s, slot), info = paste(d, slot))
    }
    expect_identical(state_get(s, "provenance"), list())
  }
})

test_that("create_analysis_state defaults to the 'sc' domain and rejects unknown ones", {
  expect_identical(attr(create_analysis_state(), "domain"), "sc")
  expect_error(create_analysis_state("virology"), "domaine")
})

# ---------------------------------------------------------------------------
# Validateurs de forme is_analysis_state() / assert_analysis_state() (step 3)
# ---------------------------------------------------------------------------
test_that("is_analysis_state accepts only the reactive 5-slot contract", {
  expect_true(is_analysis_state(create_analysis_state()))
  expect_true(is_analysis_state(create_analysis_state("bulk")))
  expect_false(is_analysis_state(NULL))
  # Meme forme en apparence mais pas un reactiveValues du projet
  expect_false(is_analysis_state(list(input = NULL, preprocessing = NULL,
                                      results = NULL, visualization = NULL,
                                      provenance = list())))
  # Schemas de domaine a plat : PAS des etats d'analyse
  expect_false(is_analysis_state(create_sc_shared_state()))
  expect_false(is_analysis_state(create_spatial_shared_state()))
})

test_that("assert_analysis_state accepts a valid state and returns it invisibly", {
  s <- create_analysis_state()
  expect_invisible(assert_analysis_state(s))
  expect_identical(assert_analysis_state(s, context = "test"), s)
})

test_that("assert_analysis_state rejects malformed states with a clear developer error", {
  expect_error(assert_analysis_state(NULL), "reactiveValues")
  expect_error(assert_analysis_state(list(input = NULL, provenance = list())),
               "reactiveValues")
  expect_error(assert_analysis_state("not-a-state"), "reactiveValues")
  # reactiveValues sans les 5 emplacements (schemas de domaine a plat)
  expect_error(assert_analysis_state(create_sc_shared_state()), "emplacement")
  expect_error(assert_analysis_state(create_spatial_shared_state()), "emplacement")
})

# ---------------------------------------------------------------------------
# Etancheite entre instances (step 3) — aucune fuite d'etat
# ---------------------------------------------------------------------------
test_that("two analysis states are fully independent (no value leakage)", {
  s1 <- create_analysis_state()
  s2 <- create_analysis_state()
  state_set(s1, "results", list(markers = data.frame(gene = "GAPDH")))
  state_set(s1, "visualization", list(palette = "viridis"))
  expect_null(state_get(s2, "results"))
  expect_null(state_get(s2, "visualization"))
  expect_identical(state_get(s2, "provenance"), list())
})

test_that("a mutated SC shared state does not affect another fresh instance", {
  a <- create_sc_shared_state()
  b <- create_sc_shared_state()
  state_set(a, "markers_data", data.frame(gene = "GAPDH"))
  state_set(a, "sc_palette", "npg")
  expect_null(state_get(b, "markers_data"))
  expect_identical(state_get(b, "sc_palette"), "default")
  # ... et le monde domaine reste etanche vis-a-vis du contrat d'analyse
  st <- create_analysis_state()
  state_set(a, "selected_genes", c("A", "B"))
  expect_null(state_get(st, "visualization"))
  expect_identical(state_get(st, "provenance"), list())
})

# ---------------------------------------------------------------------------
# Accesseurs state_get()/state_set()/state_has()
# ---------------------------------------------------------------------------
test_that("state_set/state_get round-trip works OUTSIDE a reactive context", {
  s <- create_analysis_state()
  expect_invisible(state_set(s, "results", "payload"))
  expect_identical(state_get(s, "results"), "payload")
})

test_that("state_get returns NULL for a field never set", {
  s <- create_analysis_state()
  expect_null(state_get(s, "no_such_field"))
  expect_false(state_has(s, "no_such_field"))
})

test_that("state_has distinguishes NULL-initialized from set fields", {
  s <- create_analysis_state()
  expect_false(state_has(s, "input"))          # schema NULL = pas encore calcule
  state_set(s, "input", list(a = 1))
  expect_true(state_has(s, "input"))
  # Un character(0)/list() vide compte comme present (valeur non-NULL)
  state_set(s, "results", character(0))
  expect_true(state_has(s, "results"))
})

test_that("state_set can add a field not pre-declared in the schema (dynamic add)", {
  s <- create_sc_shared_state()
  state_set(s, "qc_snapshot", list(n_cells = 10))
  expect_identical(state_get(s, "qc_snapshot"), list(n_cells = 10))
})

test_that("state_get PRESERVES reactive dependency tracking inside the app", {
  s <- create_sc_shared_state()
  counter <- new.env(parent = emptyenv())
  counter$evals <- 0L
  p <- shiny::reactive({
    counter$evals <- counter$evals + 1L
    state_get(s, "sc_palette")
  })
  expect_identical(shiny::isolate(p()), "default")
  shiny::isolate(p())                            # cache : pas de re-evaluation
  expect_identical(counter$evals, 1L)
  shiny::isolate(state_set(s, "sc_palette", "manual"))
  expect_identical(shiny::isolate(p()), "manual") # invalide puis re-evalue
  expect_identical(counter$evals, 2L)
})

# ---------------------------------------------------------------------------
# sc_state.R reste un re-export fin de la definition canonique
# ---------------------------------------------------------------------------
test_that("sourcing R/sc/sc_state.R standalone keeps the canonical factory", {
  source_project_file("R/sc/sc_state.R")   # no-op si deja charge (guard)
  s <- create_sc_shared_state()
  expect_s3_class(s, "reactivevalues")
  expect_identical(state_get(s, "sc_palette"), "default")
})

# =============================================================================
# test-mod-spatial-viz-subset.R — site d'appel de materialize_seurat_subset()
# =============================================================================
# Régression P0 (plan d'exécution 2026-10-17, item M1b).
#
# `output$dl_roi_object` (téléchargement de l'objet restreint à la ROI) appelait
#   materialize_seurat_subset(global_data$spatial_obj, cellids = ids)
# alors que la signature est
#   materialize_seurat_subset(spatial_obj, cell_ids, project = NULL)
# sans `...`. R ne relie PAS `cellids` à `cell_ids` (le matching partiel exige
# un PRÉFIXE exact : « celli… » ≠ « cell_… ») ⇒ « unused argument (cellids) »
# mesuré sur R 4.4.2, et `cell_ids` manquant. Le bouton de téléchargement ROI
# plantait donc à chaque clic.
#
# Test de COUTURE (source-level), pas comportemental : le défaut est un NOM
# d'argument au site d'appel, et le dépôt teste déjà ses `downloadHandler` par
# AST (`helper-ast.R`, cf. test-mod-bulk-report.R) plutôt qu'en exécutant un
# téléchargement. On compare aux **vrais** `formals()` de la fonction sourcée —
# jamais à une liste recopiée (règle 3).
#
# Rouge attendu = `cellids` présent ⇒ deux assertions en échec, sans erreur.
# =============================================================================

source_project_file("R/spatial/spatial_io.R")   # materialize_seurat_subset()

test_that("materialize_seurat_subset est appelé avec ses vrais noms d'arguments", {
  relpath <- "modules/spatial/mod_spatial_viz.R"
  p <- ts_ast_parse(relpath)

  calls <- ts_ast_find_all(p, function(x) ts_ast_is_call_to(x, "materialize_seurat_subset"))
  # Le site existe toujours : sinon le test ne prouverait rien (il ne doit pas
  # devenir vert par DISPARITION du code testé).
  expect_gt(length(calls), 0L)

  fml <- names(formals(materialize_seurat_subset))
  expect_true("cell_ids" %in% fml)          # garde-fou de la sonde elle-même

  for (cl in calls) {
    nm <- names(as.list(cl))
    nm <- nm[nzchar(nm)]                     # le 1er élément est la fonction

    # (a) aucun argument nommé hors signature
    expect_equal(setdiff(nm, fml), character(0),
                 info = ts_ast_deparse(cl))

    # (b) l'argument clé est bien NOMMÉ `cell_ids` (le site le nomme ; une
    #     suppression silencieuse de l'argument ne doit pas passer inaperçue)
    expect_true("cell_ids" %in% nm, info = ts_ast_deparse(cl))
  }
})

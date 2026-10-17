# =============================================================================
# test-sc-pipeline-harmony-reduction.R — le clustering suit-il l'espace intégré ?
# =============================================================================
# Régression (plan d'exécution 2026-10-17, item M4).
#
# `R/sc/sc_pipeline.R` sélectionne l'espace de clustering dans `clust_red`
# ("pca" par défaut, "harmony" quand l'intégration réussit) et ajuste
# `clust_dims`. `RunUMAP()` recevait bien `reduction=clust_red`, mais
# `FindNeighbors()` — qui construit le graphe sur lequel `robust_find_clusters()`
# segmente — était appelé SANS `reduction=` : il retombait sur la PCA de l'assay.
# Résultat : l'UI annonçait « clustering sur l'espace intégré » et les clusters
# étaient calculés sur la PCA brute. Erreur scientifique SILENCIEUSE.
#
# `dims` est du même défaut : `1:pca_dim` ignore `clust_dims`. Ce n'est pas
# cosmétique — Harmony expose `min(30, pca_dim)` dimensions ; avec le curseur
# « Dims PCA » à 50 (bornes 5..50), `dims=1:50` sur `reduction="harmony"`
# demanderait des dimensions inexistantes. Les deux arguments doivent donc
# suivre les variables du chemin, pas les valeurs par défaut.
#
# Test de COUTURE (source-level) assumé : le chemin complet de l'auto-pipeline
# exige un RunHarmony réel + toutes les étapes (coûteux et dépendant d'harmony).
# Le dépôt teste déjà ses câblages par AST (helper-ast.R). On n'asserte pas
# « il y a un reduction= » (forme faible, satisfaite par n'importe quelle
# valeur) mais « c'est EXACTEMENT `clust_red` / `1:clust_dims` ».
# =============================================================================

test_that("FindNeighbors du chemin principal suit clust_red / clust_dims", {
  p <- ts_ast_parse("R/sc/sc_pipeline.R")

  calls <- ts_ast_find_all(p, function(x) ts_ast_is_call_to(x, "FindNeighbors"))
  # Sonde DISCRIMINANTE : deux sites (sketch + chemin principal). Si le compte
  # change, le test doit le dire plutôt que de tomber juste par hasard.
  expect_length(calls, 2L)

  deps   <- vapply(calls, ts_ast_deparse, character(1))
  sketch <- grepl("sketch_params", deps, fixed = TRUE)
  expect_equal(sum(sketch), 1L)

  # Le chemin SKETCH ne peut pas porter d'intégration (elle force use_sketch
  # à FALSE en amont) : il reste sur la PCA, c'est voulu.
  main <- calls[[which(!sketch)]]

  .arg <- function(call, name) {
    l <- as.list(call)
    nm <- names(l)
    i <- which(nm == name)
    if (!length(i)) return(NULL)
    ts_ast_deparse(l[[i]])
  }

  expect_identical(.arg(main, "reduction"), "clust_red")
  expect_identical(.arg(main, "dims"), "1:clust_dims")

  # Contrôle de cohérence : UMAP utilisait déjà la même variable (le motif
  # correct existait juste à côté — c'est ce qui rend le défaut crédible).
  umap <- ts_ast_find_all(p, function(x) ts_ast_is_call_to(x, "RunUMAP"))
  expect_gt(length(umap), 0L)
  expect_true(any(vapply(umap, function(u) identical(.arg(u, "reduction"), "clust_red"),
                         logical(1))))
})

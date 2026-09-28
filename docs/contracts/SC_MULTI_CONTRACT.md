# SC_MULTI_CONTRACT.md — Conteneur `sc_datasets` & double jeu SC (MD-4, décision 5)

Gelé le 2026-09-27 (contrat reconstruit et suivi dans git depuis cet audit —
il était absent du disque et les tests de gel lisaient un document fantôme).
Test de gel : `tests/testthat/test-sc-multi-contract-freeze.R`.
Unitaires : `tests/testthat/test-sc-multi.R`.

## 1. Objet

Conteneur de jeux Single-Cell nommés (`global_data$sc_datasets`) — miroir de
`R/bulk/bulk_multi.R` (MD-1, contrat `BULK_MULTI_CONTRACT.md`) appliqué au
domaine Single-Cell. Chaque entrée = objet Seurat complet (lourd : budget
RAM 32 Go) + label + producteur + relation déclarée + horodatages.

## 2. Surface publique gelée (R/sc/sc_multi.R)

`sc_multi_public_api()`, `sc_multi_error_states()`, `sc_multi_relations()`,
`sc_multi_check_label()`, `sc_multi_check_obj()`, `sc_multi_check_relation()`,
`sc_multi_register()`, `sc_multi_remove()`, `sc_multi_get()`,
`sc_multi_summary()`. Signatures gelées (test de gel).

## 3. Règles (gelées)

- Erreurs : classe `sc_multi_error`, états = invalid_input, invalid_label,
  invalid_obj, invalid_relation, duplicate_label, unknown_label,
  capacity_exceeded.
- Relations déclarées (décision 5) : `standalone`, `shared_params` (mode 1),
  `distinct_params` (mode 2). La relation est une annotation de PROVENANCE —
  jamais appliquée mécaniquement (l'UI le dit : « paramètres partagés » =
  engagement utilisateur, pas exécution automatique).
- Plafond : `TS_SC_MULTI_MAX_DATASETS` (config/thresholds.R) — **20** depuis
  le 2026-09-27 (feature 6×10X : 6 réplicats doivent tenir ; aligné sur le
  conteneur bulk). Repli du code pur : `20L` si la config n'est pas sourcée.
- `sc_multi_register(..., overwrite = TRUE)` met à jour en conservant
  `registered_at` et en rafraîchissant `updated_at`.

## 4. Résumé (colonnes gelées, §4.2)

`sc_multi_summary()` retourne : label, producer, relation, n_cells, n_genes,
n_samples, has_clusters, registered_at, updated_at — étendu 2026-09-27 avec
la QC par dataset : median_nFeature_RNA, median_nCount_RNA, median_percent_mt
(NA quand la métrique est absente de l'objet).

## 5. Pureté

`R/sc/sc_multi.R` est pur : aucun symbole Shiny (garde du test de gel).

## 6. Câblage (producteurs + consommation)

- Producteur "import" : `modules/import/mod_import_sc.R`
  (`.register_sc_multi_dataset`, 5 points d'appel depuis le merge main —
  4 chemins humains (picker .rda + options A/B/C) + le chemin drive
  `import_file` du module import_sc ; échec = alerte sans stop).
- Producteur "pipeline_save" : `modules/sc/mod_sc_datasets.R`
  (save / summary / delete / **activate**).
- **Activation (amendé 2026-09-27, roadmap 4.1)** : le bouton « Activer ce
  dataset » du module de gestion relit l'entrée via `sc_multi_get()` et
  écrit `global_data$sc_obj <- entry$obj` — SEULE écriture autorisée sur le
  jeu actif dans ce module. Le bump de `global_data$sc_obj_epoch` qui suit
  purge les résultats partagés (audit 2026-09-27 §1.5).
- Ancres app.R : source, init (`sc_datasets = list()`), snapshot
  (`sc_datasets = global_data$sc_datasets`), restore, reset.
- Garde de non-régression : les fichiers SC préexistants ne référencent PAS
  `sc_datasets` (zéro changement de comportement).
- Affichage : `ts_datatable(..., page_length = 6, buttons = FALSE)` pour le
  résumé et la gestion.

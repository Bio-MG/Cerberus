# MILO_RESULT_CONTRACT.md — Résultat d'abondance différentielle Milo

> **Contrat reconstruit le 2026-09-27.** Le document original était
> absent du dépôt : `docs/` était ignoré par `.gitignore` (audit
> 2026-09-27 §0) et le test de gel lisait un document fantôme. Ce
> document est aligné sur le test de gel EXÉCUTABLE (source de vérité)
> et sur le code gelé. Règle contract-first : toute évolution passe
> SIMULTANÉMENT par le code, le test de gel et ce document.

## 1. Fichiers gelés

- Code : `R/sc/sc_abundance_milo.R`
- Test : `tests/testthat/test-milo-contract-freeze.R`

## 2. Champs du contrat (16, gelés)

Champs portés par le résultat : `type`, `status`, `design`, `parameters`,
`tested_contrast`, `model_specification`, `neighbourhood_summary`,
`DA_table`, `nhood_assignment`, `sample_composition`, `package_versions`,
`object_identity`, `warnings`, `provenance`, `analysis_id`,
`timestamp_utc`.

## 3. États de validité (gelés)

valid, valid_with_warnings, invalid_input, compute_failed,
design_not_eligible, stale_against_current_seurat_object.

## 4. Fonctions citées par le test de gel

`run_milo_da`, `finalize_milo_result`, `assert_milo_result`,
`milo_public_api`, `milo_views_public_api`. La porte Stage 13 est
documentée : le résultat exige un design validé (consommation de
`assert_da_design_result`), sinon l'état design_not_eligible.

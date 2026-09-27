# SCCODA_RESULT_CONTRACT.md — Résultat d'abondance différentielle scCODA

> **Contrat reconstruit le 2026-09-27.** Le document original était
> absent du dépôt : `docs/` était ignoré par `.gitignore` (audit
> 2026-09-27 §0) et le test de gel lisait un document fantôme. Ce
> document est aligné sur le test de gel EXÉCUTABLE (source de vérité)
> et sur le code gelé. Règle contract-first : toute évolution passe
> SIMULTANÉMENT par le code, le test de gel et ce document.

## 1. Fichiers gelés

- Code : `R/sc/sc_abundance_sccoda.R`
- Test : `tests/testthat/test-sccoda-contract-freeze.R`

## 2. Champs du contrat (17, gelés)

`type`, `status`, `compositional_unit`, `design`, `parameters`,
`model_specification`, `convergence_diagnostics`, `composition_table`,
`effect_table`, `credible_effects`, `reference_identity`,
`package_versions`, `object_identity`, `warnings`, `provenance`,
`analysis_id`, `timestamp_utc`.

## 3. États de validité (gelés)

valid, valid_with_warnings, invalid_input, environment_missing,
compute_failed, convergence_failure, design_not_eligible,
stale_against_current_seurat_object.

## 4. Fonctions citées par le test de gel

`run_sccoda_da`, `finalize_sccoda_result`, `assert_sccoda_result`,
`sccoda_public_api`, `sccoda_views_public_api`, `sccoda_available`,
`sccoda_convergence_assessment`.

## 5. Porte Stage 13 et séparation d'avec Milo

Le résultat exige un design validé (consommation de
`assert_da_design_result`), sinon l'état design_not_eligible. L'unité
compositionnelle est l'ÉCHANTILLON (`compositional_unit = "sample"`) :
scCODA est un modèle compositionnel bayésien par échantillon — les
résultats ne se comparent pas à un DA de voisinage (Milo).

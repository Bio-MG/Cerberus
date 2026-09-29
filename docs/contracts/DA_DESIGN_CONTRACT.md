# DA_DESIGN_CONTRACT.md — Design d'abondance différentielle (Stage 13)

> **Contrat reconstruit le 2026-09-27.** Le document original était
> absent du dépôt : `docs/` était ignoré par `.gitignore` (audit
> 2026-09-27 §0) et le test de gel lisait un document fantôme. Ce
> document est aligné sur le test de gel EXÉCUTABLE (source de vérité)
> et sur le code gelé. Règle contract-first : toute évolution passe
> SIMULTANÉMENT par le code, le test de gel et ce document.

## 1. Fichiers gelés

- Code : `R/sc/sc_abundance_design.R`
- Test : `tests/testthat/test-da-design-contract-freeze.R`

## 2. Champs du contrat (17, gelés)

Champs portés par le résultat du design : `type`, `status`,
`composition_unit`, `config`, `condition_summary`, `sample_summary`,
`condition_batch_table`, `identity_coverage`, `missingness`, `exclusions`,
`milo_eligibility`, `sccoda_eligibility`, `object_identity`, `warnings`,
`provenance`, `analysis_id`, `timestamp_utc`.

## 3. États de validité (gelés)

valid, valid_with_warnings, invalid_design, invalid_input,
stale_against_current_seurat_object.

## 4. Fonctions citées par le test de gel

`validate_da_design` (validation du plan avant tout calcul),
`finalize_da_design_result`, `assert_da_design_result`,
`da_design_public_api`.

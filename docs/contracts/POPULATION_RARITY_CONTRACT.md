# POPULATION_RARITY_CONTRACT_CONTRACT.md — Rareté par population annotée (descriptif, CCC 9)

> **Contrat reconstruit le 2026-09-27.** Le document original était
> absent du dépôt : `docs/` était ignoré par `.gitignore` (audit
> 2026-09-27 §0) et le test de gel lisait un document fantôme. Ce
> document est aligné sur le test de gel EXÉCUTABLE (source de vérité)
> et sur le code gelé. Règle contract-first : toute évolution passe
> SIMULTANÉMENT par le code, le test de gel et ce document.

## 1. Fichiers gelés

- Test : `tests/testthat/test-sc-population-rarity-contract-freeze.R`

## 2. Surface publique et vectors figés (extrait du code)

## 3. Jetons de synchronisation cités par le test de gel

Chaque jeton ci-dessous doit rester présent dans ce document
(garde de synchronisation code <-> contrat).

- .POPULATION_RARITY_ANALYSIS_ID
- .POPULATION_RARITY_STATUS_STATES
- .population_rarity_bad_scalar
- .population_rarity_check_threshold
- .population_rarity_floor
- .population_rarity_is_blank
- .population_rarity_rule_label
- .population_rarity_stop
- absolute_n_cells
- assert_population_rarity_result
- compute_population_rarity
- declared_rule_label
- direction
- empty_levels
- fingerprint
- fraction
- identity_column
- invalid_input
- is_rare
- method
- mod_sc_rarity_output_ui
- mod_sc_rarity_server
- mod_sc_rarity_ui
- n_cells
- n_cells_counted
- n_cells_total
- n_labels_na
- n_levels
- n_levels_empty
- n_populations
- n_rare
- population
- population_rarity_contract_fields
- population_rarity_public_api
- population_rarity_validity_states
- relative_fraction
- rule_type
- sample_column
- seurat_dims
- threshold
- unavailable_single_population
- valid
- valid_with_warnings

## Champs figés et porte Stage 13 (extrait du code)

Champs du contrat : `type`, `status`, `population_table`, `qc`,
`identity_column`, `identity_summary`, `parameters`, `rarity_rule`,
`summary`, `object_identity`, `warnings`, `provenance`, `analysis_id`,
`timestamp_utc`.

La porte Stage 13 n'est PAS appelée ici : `assert_da_design_result` est
INAPPLICABLE — la rareté par population est `descriptive_only` (aucun
test A-vs-B). Règles de lecture : `regle 3` (population signalée comme
plafond, jamais un compte exact) et `aucun seuil implicite` — tout seuil
est déclaré par l'utilisateur.

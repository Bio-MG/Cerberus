# COMMUNICATION_VELOCITY_CONTRACT_CONTRACT.md — Communication croisées avec la vélocité ARN

> **Contrat reconstruit le 2026-09-27.** Le document original était
> absent du dépôt : `docs/` était ignoré par `.gitignore` (audit
> 2026-09-27 §0) et le test de gel lisait un document fantôme. Ce
> document est aligné sur le test de gel EXÉCUTABLE (source de vérité)
> et sur le code gelé. Règle contract-first : toute évolution passe
> SIMULTANÉMENT par le code, le test de gel et ce document.

## 1. Fichiers gelés

- Code : `R/sc/sc_communication_velocity.R`
- Test : `tests/testthat/test-communication-velocity-contract-freeze.R`

## 2. Surface publique et vectors figés (extrait du code)

`communication_velocity_contract_fields` :
- `analysis_id`
- `identity_column`
- `pair_table`
- `params`
- `parent_analysis_id`
- `provenance`
- `source_method`
- `status`
- `timestamp_utc`
- `type`
- `velocity_analysis_id`
- `velocity_status`
- `warnings`
`communication_velocity_public_api` :
- `build_communication_velocity_context`
- `build_communication_velocity_export`
- `communication_velocity_contract_fields`
- `communication_velocity_public_api`
- `plot_communication_velocity_context`

## 3. Jetons de synchronisation cités par le test de gel

Chaque jeton ci-dessous doit rester présent dans ce document
(garde de synchronisation code <-> contrat).

- analysis_id
- build_communication_velocity_context
- build_communication_velocity_export
- communication_velocity_contract_fields
- communication_velocity_public_api
- identity_column
- mean_imported_score
- mod_sc_communication_velocity_server
- mod_sc_communication_velocity_ui
- n_interactions
- n_receiver_cells
- n_sender_cells
- pair_table
- params
- parent_analysis_id
- plot_communication_velocity_context
- provenance
- receiver_node
- receiver_velocity_mean
- receiver_velocity_median
- sender_node
- sender_velocity_mean
- sender_velocity_median
- source_method
- status
- timestamp_utc
- type
- velocity_analysis_id
- velocity_status
- warnings

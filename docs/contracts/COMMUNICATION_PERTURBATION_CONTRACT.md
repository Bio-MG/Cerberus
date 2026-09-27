# COMMUNICATION_PERTURBATION_CONTRACT_CONTRACT.md — Perturbation in silico du réseau de communication

> **Contrat reconstruit le 2026-09-27.** Le document original était
> absent du dépôt : `docs/` était ignoré par `.gitignore` (audit
> 2026-09-27 §0) et le test de gel lisait un document fantôme. Ce
> document est aligné sur le test de gel EXÉCUTABLE (source de vérité)
> et sur le code gelé. Règle contract-first : toute évolution passe
> SIMULTANÉMENT par le code, le test de gel et ce document.

## 1. Fichiers gelés

- Code : `R/sc/sc_communication_perturbation.R`
- Test : `tests/testthat/test-communication-perturbation-contract-freeze.R`

## 2. Surface publique et vectors figés (extrait du code)

`communication_perturbation_contract_fields` :
- `analysis_id`
- `baseline_summary`
- `delta_table`
- `mode`
- `node_table`
- `params`
- `parent_analysis_id`
- `perturbed_summary`
- `provenance`
- `source_method`
- `status`
- `target`
- `timestamp_utc`
- `type`
- `value`
- `warnings`
`communication_perturbation_public_api` :
- `build_communication_perturbation`
- `build_communication_perturbation_export`
- `communication_perturbation_contract_fields`
- `communication_perturbation_public_api`
- `communication_perturbation_targets`
- `plot_communication_perturbation_delta`
- `plot_communication_perturbation_nodes`

## 3. Jetons de synchronisation cités par le test de gel

Chaque jeton ci-dessous doit rester présent dans ce document
(garde de synchronisation code <-> contrat).

- affected
- analysis_id
- baseline_summary
- build_communication_perturbation
- build_communication_perturbation_export
- communication_perturbation_contract_fields
- communication_perturbation_public_api
- communication_perturbation_targets
- delta_fraction
- delta_score_total
- delta_table
- interaction
- ligand
- mod_sc_communication_perturbation_server
- mod_sc_communication_perturbation_ui
- mode
- n_interactions_baseline
- n_interactions_perturbed
- node_table
- params
- parent_analysis_id
- perturbed_summary
- plot_communication_perturbation_delta
- plot_communication_perturbation_nodes
- provenance
- receiver
- receiver_node
- receptor
- score_total_baseline
- score_total_perturbed
- sender
- sender_node
- source_method
- status
- target
- timestamp_utc
- type
- value
- warnings

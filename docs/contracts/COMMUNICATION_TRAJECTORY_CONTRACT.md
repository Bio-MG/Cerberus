# COMMUNICATION_TRAJECTORY_CONTRACT_CONTRACT.md — Communication le long de la trajectoire / pseudo-temps

> **Contrat reconstruit le 2026-09-27.** Le document original était
> absent du dépôt : `docs/` était ignoré par `.gitignore` (audit
> 2026-09-27 §0) et le test de gel lisait un document fantôme. Ce
> document est aligné sur le test de gel EXÉCUTABLE (source de vérité)
> et sur le code gelé. Règle contract-first : toute évolution passe
> SIMULTANÉMENT par le code, le test de gel et ce document.

## 1. Fichiers gelés

- Code : `R/sc/sc_communication_trajectory.R`
- Test : `tests/testthat/test-communication-trajectory-contract-freeze.R`

## 2. Surface publique et vectors figés (extrait du code)

`communication_trajectory_contract_fields` :
- `analysis_id`
- `identity_column`
- `pair_bin_table`
- `pair_summary`
- `params`
- `parent_analysis_id`
- `provenance`
- `source_method`
- `status`
- `timestamp_utc`
- `type`
- `warnings`
`communication_trajectory_public_api` :
- `build_communication_trajectory_context`
- `build_communication_trajectory_export`
- `communication_fetch_expression_matrix`
- `communication_trajectory_contract_fields`
- `communication_trajectory_public_api`
- `plot_communication_trajectory_curves`
- `plot_communication_trajectory_heatmap`

## 3. Jetons de synchronisation cités par le test de gel

Chaque jeton ci-dessous doit rester présent dans ce document
(garde de synchronisation code <-> contrat).

- analysis_id
- bin
- bin_from
- bin_mid
- bin_to
- build_communication_trajectory_context
- build_communication_trajectory_export
- communication_fetch_expression_matrix
- communication_trajectory_contract_fields
- communication_trajectory_public_api
- frac_receiver_of_bin
- frac_receiver_of_population
- frac_sender_of_bin
- frac_sender_of_population
- identity_column
- ligand_genes
- lineage
- mean_imported_score
- mean_ligand_expression_senders
- mean_receptor_expression_receivers
- mod_sc_communication_trajectory_server
- mod_sc_communication_trajectory_ui
- n_cells_in_bin
- n_ligand_genes_missing
- n_receiver_cells
- n_receptor_genes_missing
- n_sender_cells
- pair_bin_table
- pair_summary
- params
- parent_analysis_id
- plot_communication_trajectory_curves
- plot_communication_trajectory_heatmap
- provenance
- receiver_node
- receptor_genes
- sender_node
- source_method
- status
- timestamp_utc
- type
- warnings

# COMMUNICATION_SPATIAL_CONTRACT_CONTRACT.md — Contexte spatial de la communication cellulaire

> **Contrat reconstruit le 2026-09-27.** Le document original était
> absent du dépôt : `docs/` était ignoré par `.gitignore` (audit
> 2026-09-27 §0) et le test de gel lisait un document fantôme. Ce
> document est aligné sur le test de gel EXÉCUTABLE (source de vérité)
> et sur le code gelé. Règle contract-first : toute évolution passe
> SIMULTANÉMENT par le code, le test de gel et ce document.

## 1. Fichiers gelés

- Code : `R/sc/sc_communication_spatial.R`
- Test : `tests/testthat/test-communication-spatial-contract-freeze.R`

## 2. Surface publique et vectors figés (extrait du code)

`communication_spatial_contract_fields` :
- `analysis_id`
- `coordinate_metric`
- `coordinate_source`
- `coordinate_units`
- `identity_column`
- `pair_table`
- `params`
- `parent_analysis_id`
- `provenance`
- `source_method`
- `status`
- `timestamp_utc`
- `type`
- `warnings`
`communication_spatial_coordinate_sources` :
- `reduction`
- `spatial_object`
`communication_spatial_public_api` :
- `build_communication_spatial_context`
- `build_communication_spatial_export`
- `communication_spatial_contract_fields`
- `communication_spatial_coordinate_sources`
- `communication_spatial_public_api`
- `plot_communication_spatial_distance_summary`
- `plot_communication_spatial_edges`

## 3. Jetons de synchronisation cités par le test de gel

Chaque jeton ci-dessous doit rester présent dans ce document
(garde de synchronisation code <-> contrat).

- analysis_id
- build_communication_spatial_context
- build_communication_spatial_export
- centroid_distance
- communication_spatial_contract_fields
- communication_spatial_coordinate_sources
- communication_spatial_public_api
- coordinate_metric
- coordinate_source
- coordinate_units
- frac_receiver_within_radius
- frac_sender_within_radius
- identity_column
- mean_imported_score
- mean_recv_to_sender_nn
- mean_send_to_recv_nn
- median_recv_to_sender_nn
- median_send_to_recv_nn
- mod_sc_communication_spatial_server
- mod_sc_communication_spatial_ui
- n_interactions
- n_receiver_cells
- n_sender_cells
- n_with_score
- p_perm
- pair_table
- params
- parent_analysis_id
- plot_communication_spatial_distance_summary
- plot_communication_spatial_edges
- provenance
- receiver_node
- reduction
- sender_node
- source_method
- spatial_object
- status
- timestamp_utc
- type
- warnings
- z_score

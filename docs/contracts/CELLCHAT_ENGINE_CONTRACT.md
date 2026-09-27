# CELLCHAT_ENGINE_CONTRACT_CONTRACT.md — Moteur CellChat natif (Path B)

> **Contrat reconstruit le 2026-09-27.** Le document original était
> absent du dépôt : `docs/` était ignoré par `.gitignore` (audit
> 2026-09-27 §0) et le test de gel lisait un document fantôme. Ce
> document est aligné sur le test de gel EXÉCUTABLE (source de vérité)
> et sur le code gelé. Règle contract-first : toute évolution passe
> SIMULTANÉMENT par le code, le test de gel et ce document.

## 1. Fichiers gelés

- Code : `R/core/io_helpers.R`
- Code : `R/core/provenance.R`
- Code : `R/plotting/palettes.R`
- Code : `R/sc/sc_communication.R`
- Code : `R/sc/sc_communication_engine.R`
- Code : `R/sc/sc_communication_input.R`
- Code : `R/sc/sc_velocity.R`
- Test : `tests/testthat/test-sc-communication-engine-ui.R`
- Test : `tests/testthat/test-sc-communication-engine.R`

## 2. Surface publique et vectors figés (extrait du code)

`cellchat_engine_public_api` :
- `cellchat_analysis_identity`
- `cellchat_engine_available`
- `cellchat_engine_error_state`
- `cellchat_engine_public_api`
- `cellchat_engine_states`
- `cellchat_engine_summary`
- `run_cellchat`
`cellchat_input_public_api` :
- `assert_cellchat_input`
- `build_cellchat_input`
- `cellchat_database_for_species`
- `cellchat_group_by_column`
- `cellchat_input_error_state`
- `cellchat_input_from_matrix`
- `cellchat_input_public_api`
- `cellchat_input_requirements`
- `cellchat_input_states`
- `cellchat_input_summary`
- `cellchat_log_normalize`
`communication_contract_fields` :
- `interaction`
- `ligand`
- `p_adjusted`
- `p_value`
- `pathway`
- `receiver`
- `receptor`
- `score`
- `sender`
- `source_cell_identity_level`
- `source_file`
- `source_method`
`communication_public_api` :
- `assert_communication_result`
- `build_communication_identity_mapping_export`
- `build_communication_import_summary`
- `communication_contract_fields`
- `communication_error_state`
- `communication_export_filename`
- `communication_import_qc`
- `communication_public_api`
- `communication_rank_aggregation_modes`
- `communication_rank_fields`
- `communication_result_is_stale`
- `communication_status_is_valid`
- `communication_status_labels`
- `communication_supported_sources`
- `communication_validity_states`
- `finalize_communication_result`
- `harmonize_communication_identities`
- `parse_cellchat_import`
- `parse_cellchat_object`
- `parse_cellphonedb_import`
- `parse_liana_import`
`liana_engine_public_api` :
- `assert_liana_collection`
- `build_liana_collection_table`
- `liana_collection_active`
- `liana_collection_condition_summary`
- `liana_collection_sample_manifest`
- `liana_engine_available`
- `liana_engine_error_state`
- `liana_engine_public_api`
- `liana_engine_states`
- `run_liana`
- `run_liana_by_sample`
`velocity_contract_fields` :
- `ambiguous`
- `analysis_id`
- `cell_alignment`
- `cell_mapping`
- `cell_names`
- `dimensions`
- `embedding_alignment`
- `gene_alignment`
- `gene_mapping`
- `gene_names`
- `input_summary`
- `object_identity`
- `orientation`
- `provenance`
- `spliced`
- `status`
- `timestamp_utc`
- `type`
- `unspliced`
- `vector_validation`
- `velocity_vectors`
- `warnings`
`velocity_public_api` :
- `align_velocity_embedding`
- `assert_velocity_result`
- `build_velocity_alignment_mapping`
- `build_velocity_cell_vectors_export`
- `build_velocity_provenance_export`
- `build_velocity_validation_summary`
- `detect_velocity_orientation`
- `finalize_velocity_result`
- `normalize_velocity_cell_barcodes`
- `normalize_velocity_gene_ids`
- `plot_velocity_alignment_qc`
- `plot_velocity_coverage`
- `plot_velocity_embedding`
- `plot_velocity_phase_portrait`
- `plot_velocity_vector_field`
- `read_velocity_mtx`
- `read_velocity_rds`
- `validate_precomputed_velocity_vectors`
- `validate_velocity_identifiers`
- `validate_velocity_matrices`
- `validate_velocity_rds_metadata`
- `velocity_contract_fields`
- `velocity_error_state`
- `velocity_export_filename`
- `velocity_object_fingerprint`
- `velocity_public_api`
- `velocity_result_is_stale`
- `velocity_status_allows_matrices`
- `velocity_status_is_valid`
- `velocity_status_labels`
- `velocity_validity_states`

## 3. Jetons de synchronisation cités par le test de gel

Chaque jeton ci-dessous doit rester présent dans ce document
(garde de synchronisation code <-> contrat).

- align_velocity_embedding
- ambiguous
- analysis_id
- assert_cellchat_input
- assert_communication_result
- assert_liana_collection
- assert_velocity_result
- build_cellchat_input
- build_communication_identity_mapping_export
- build_communication_import_summary
- build_liana_collection_table
- build_velocity_alignment_mapping
- build_velocity_cell_vectors_export
- build_velocity_provenance_export
- build_velocity_validation_summary
- cell_alignment
- cell_mapping
- cell_names
- cellchat_analysis_identity
- cellchat_database_for_species
- cellchat_engine_available
- cellchat_engine_error_state
- cellchat_engine_public_api
- cellchat_engine_states
- CC-5 (exposition UI du moteur : etat PREVU si paquet absent, remede annonce, 6 etats geles)
- comm_compute (canaux d'appel moteur)
- cellchat_engine_summary
- cellchat_group_by_column
- cellchat_input_error_state
- cellchat_input_from_matrix
- cellchat_input_public_api
- cellchat_input_requirements
- cellchat_input_states
- cellchat_input_summary
- cellchat_log_normalize
- communication_contract_fields
- communication_error_state
- communication_export_filename
- communication_import_qc
- communication_public_api
- communication_rank_aggregation_modes
- communication_rank_fields
- communication_result_is_stale
- communication_status_is_valid
- communication_status_labels
- communication_supported_sources
- communication_validity_states
- detect_velocity_orientation
- dimensions
- embedding_alignment
- finalize_communication_result
- finalize_velocity_result
- gene_alignment
- gene_mapping
- gene_names
- harmonize_communication_identities
- input_summary
- interaction
- liana_collection_active
- liana_collection_condition_summary
- liana_collection_sample_manifest
- liana_engine_available
- liana_engine_error_state
- liana_engine_public_api
- liana_engine_states
- ligand
- normalize_velocity_cell_barcodes
- normalize_velocity_gene_ids
- object_identity
- orientation
- p_adjusted
- p_value
- parse_cellchat_import
- parse_cellchat_object
- parse_cellphonedb_import
- parse_liana_import
- pathway
- plot_velocity_alignment_qc
- plot_velocity_coverage
- plot_velocity_embedding
- plot_velocity_phase_portrait
- plot_velocity_vector_field
- provenance
- read_velocity_mtx
- read_velocity_rds
- receiver
- receptor
- run_cellchat
- run_liana
- run_liana_by_sample
- score
- sender
- source_cell_identity_level
- source_file
- source_method
- spliced
- status
- timestamp_utc
- type
- unspliced
- validate_precomputed_velocity_vectors
- validate_velocity_identifiers
- validate_velocity_matrices
- validate_velocity_rds_metadata
- vector_validation
- velocity_contract_fields
- velocity_error_state
- velocity_export_filename
- velocity_object_fingerprint
- velocity_public_api
- velocity_result_is_stale
- velocity_status_allows_matrices
- velocity_status_is_valid
- velocity_status_labels
- velocity_validity_states
- velocity_vectors
- warnings

## Jetons moteur cités par le test de gel (sync documentation <-> code)

Le moteur gèle les règles suivantes : `database_version` voyage avec tout
résultat (deux résultats de même version de base restent comparables) ;
`updateCellChatDB` n'est JAMAIS appelé (la base LR ne mute pas entre deux
runs) ; `nboot` est un paramètre déclaré du run ; l'identité cellulaire est
portée par `source_cell_identity_level` ; les points d'entrée gelés sont
`run_cellchat` et `cellchat_analysis_identity`.

# COMMUNICATION_RESULT_CONTRACT_CONTRACT.md — Résultat canonique de communication cellulaire (import LIANA)

> **Contrat reconstruit le 2026-09-27.** Le document original était
> absent du dépôt : `docs/` était ignoré par `.gitignore` (audit
> 2026-09-27 §0) et le test de gel lisait un document fantôme. Ce
> document est aligné sur le test de gel EXÉCUTABLE (source de vérité)
> et sur le code gelé. Règle contract-first : toute évolution passe
> SIMULTANÉMENT par le code, le test de gel et ce document.

## 1. Fichiers gelés

- Test : `tests/testthat/test-communication-contract-freeze.R`

## 2. Surface publique et vectors figés (extrait du code)

## 3. Jetons de synchronisation cités par le test de gel

Chaque jeton ci-dessous doit rester présent dans ce document
(garde de synchronisation code <-> contrat).

- .COMMUNICATION_FIELD_ALIASES
- .COMMUNICATION_STATUS_STATES
- .communication_agg_pairs
- .communication_coerce_numeric
- .communication_empty_view_plot
- .communication_is_blank
- .communication_node_keys
- .communication_object_fingerprint
- .communication_pick_column
- .communication_stop
- .views_stop
- CCL signaling
- CD40 signaling
- IL7 signaling
- analysis_id
- canonical_table
- column_mapping
- duplicate_interaction
- identity_column
- identity_mapping
- identity_summary
- input_summary
- invalid_identity_mapping
- invalid_input
- invalid_schema
- magnitude
- mod_sc_communication_output_ui
- mod_sc_communication_server
- mod_sc_communication_ui
- object_identity
- provenance
- rank
- rank_aggregation_mode
- rank_direction
- receiver_mapped
- sender_mapped
- source_method
- specificity
- stale_against_current_seurat_object
- status
- timestamp_utc
- type
- valid
- warnings

## 4. Champs, états et fonctions figés (extrait du code)

Champs du contrat (table canonique) : `sender`, `receiver`, `ligand`,
`receptor`, `interaction`, `pathway`, `score`, `p_value`, `p_adjusted`,
`source_method`, `source_file`, `source_cell_identity_level`.

États de validité : valid, invalid_input, invalid_schema,
invalid_identity_mapping, stale_against_current_seurat_object.

Champs de mesure de rang (route (b) LIANA, exclus des sources existantes) :
`rank`, `rank_direction`, `rank_aggregation_mode` — modes d'agrégation :
specificity, magnitude (sélectionnés via `aggregate_rank`, `rank_max` étant
l'agrégation retenue ; `lower_is_better` documente la direction par métrique).
Le statut d'import externe est porté par `is_external_consensus`.

Fonctions et surfaces citées : `finalize_communication_result`,
`assert_communication_result`, `communication_public_api`,
`parse_cellchat_object`, `parse_liana_import`,
`communication_apply_filters`, `build_communication_filter_provenance`,
`communication_views_public_api`, `communication_rank_fields`,
`communication_rank_aggregation_modes`, `liana_engine` — voir aussi
`LIANA_ENGINE_CONTRACT.md`.

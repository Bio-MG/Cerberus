# VELOCITY_RESULT_CONTRACT_CONTRACT.md — Résultat canonique de vélocité ARN (Stage 8-9)

> **Contrat reconstruit le 2026-09-27.** Le document original était
> absent du dépôt : `docs/` était ignoré par `.gitignore` (audit
> 2026-09-27 §0) et le test de gel lisait un document fantôme. Ce
> document est aligné sur le test de gel EXÉCUTABLE (source de vérité)
> et sur le code gelé. Règle contract-first : toute évolution passe
> SIMULTANÉMENT par le code, le test de gel et ce document.

## 1. Fichiers gelés

- Test : `tests/testthat/test-velocity-contract-freeze.R`

## 2. Surface publique et vectors figés (extrait du code)

## 3. Jetons de synchronisation cités par le test de gel

Chaque jeton ci-dessous doit rester présent dans ce document
(garde de synchronisation code <-> contrat).

- .VELOCITY_STATUS_STATES
- .provenance_versions_string
- .read_lines_maybe_gz
- .velocity_matrix_is_numeric
- .velocity_stop
- ambiguous
- analysis_id
- cell1
- cell2
- cell3
- cell_alignment
- cell_mapping
- cell_names
- cells
- dimensions
- embedding_alignment
- gene_alignment
- gene_mapping
- gene_names
- genes
- input_summary
- invalid_cell_alignment
- invalid_gene_alignment
- invalid_input
- invalid_orientation
- invalid_vector_projection
- low_overlap_override
- match_mode
- mod_sc_velocity_output_ui
- mod_sc_velocity_server
- mod_sc_velocity_ui
- n_input
- n_matched
- n_missing
- object_identity
- orientation
- overlap_normalized
- overlap_raw
- provenance
- spliced
- stale_against_current_seurat_object
- status
- timestamp_utc
- type
- unspliced
- valid
- valid_no_vectors
- valid_partial_embedding
- vector_validation
- velocity_vectors
- warnings

## Champs, états et fonctions figés (extrait du code)

Champs du contrat : `type`, `status`, `spliced`, `unspliced`,
`velocity_vectors`, `gene_names`, `cell_names`, `dimensions`,
`orientation`, `input_summary`, `gene_mapping`, `gene_alignment`,
`cell_mapping`, `cell_alignment`, `embedding_alignment`,
`vector_validation`, `object_identity`, `warnings`, `provenance`,
`analysis_id`, `timestamp_utc`.

États d'alignement/orientation : `ambiguous`. Fonctions et surfaces
citées : `finalize_velocity_result`, `assert_velocity_result`,
`velocity_public_api`.

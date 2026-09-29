# BULK_MERGE_CONTRACT_CONTRACT.md — Fusion de jeux bulk (NEW-2)

> **Contrat reconstruit le 2026-09-27.** Le document original était
> absent du dépôt : `docs/` était ignoré par `.gitignore` (audit
> 2026-09-27 §0) et le test de gel lisait un document fantôme. Ce
> document est aligné sur le test de gel EXÉCUTABLE (source de vérité)
> et sur le code gelé. Règle contract-first : toute évolution passe
> SIMULTANÉMENT par le code, le test de gel et ce document.

## 1. Fichiers gelés

- Code : `R/bulk/batch_correction.R`
- Code : `R/bulk/bulk_batch_qc.R`
- Code : `R/bulk/bulk_merge.R`
- Code : `R/bulk/bulk_multi.R`
- Code : `R/bulk/bulk_provenance.R`
- Code : `R/core/io_helpers.R`
- Test : `tests/testthat/test-bulk-merge-contract-freeze.R`

## 2. Surface publique et vectors figés (extrait du code)

`bulk_batch_correction_public_api` :
- `bulk_assert_raw_counts`
- `bulk_batch_correction_design`
- `bulk_batch_correction_label`
- `bulk_batch_correction_public_api`
- `plot_batch_correction_pca`
- `run_combat_seq`
`bulk_batch_qc_public_api` :
- `bulk_assert_transformed_matrix`
- `bulk_batch_design_check`
- `bulk_batch_qc_public_api`
- `bulk_variance_partition`
- `plot_bulk_batch_scree`
- `plot_bulk_varpart`
`bulk_merge_error_states` :
- `design_not_applicable`
- `insufficient_datasets`
- `invalid_input`
- `invalid_metadata`
- `no_common_genes`
`bulk_merge_public_api` :
- `bulk_merge_align_genes`
- `bulk_merge_check_inputs`
- `bulk_merge_combine`
- `bulk_merge_common_meta_columns`
- `bulk_merge_error_states`
- `bulk_merge_preview_metadata`
- `bulk_merge_public_api`
- `bulk_merge_run`
- `bulk_merge_to_bulk_obj`
`bulk_multi_error_states` :
- `capacity_exceeded`
- `duplicate_label`
- `insufficient_datasets`
- `invalid_input`
- `invalid_label`
- `invalid_obj`
- `invalid_pipeline`
- `no_common_contrast`
- `no_significant_genes`
- `unknown_label`
`bulk_multi_pipeline_fields` :
- `active_contrast`
- `contrasts`
- `filtered_counts`
- `lfc_thresh`
- `mapping_applied`
- `mapping_summary`
- `multimethod_de`
- `padj_thresh`
- `pathway_db`
- `pathway_mode`
- `pathway_results`
- `vst_mat`
`bulk_multi_public_api` :
- `bulk_multi_capture_pipeline`
- `bulk_multi_check_label`
- `bulk_multi_check_obj`
- `bulk_multi_error_states`
- `bulk_multi_get`
- `bulk_multi_pipeline_fields`
- `bulk_multi_public_api`
- `bulk_multi_register`
- `bulk_multi_remove`
- `bulk_multi_summary`
`bulk_provenance_public_api` :
- `bulk_build_provenance`
- `bulk_ensure_provenance`
- `bulk_provenance_dataframe`
- `bulk_provenance_known_genomes`
- `bulk_provenance_known_normalizations`
- `bulk_provenance_public_api`
- `bulk_provenance_session_packages`
- `bulk_update_provenance`

## 3. Jetons de synchronisation cités par le test de gel

Chaque jeton ci-dessous doit rester présent dans ce document
(garde de synchronisation code <-> contrat).

- Colonne condition commune (optionnel)
- ComBat-seq
- Datasets à fusionner (>= 2)
- Fusion de jeux
- Fusion impossible : %s
- Fusionner et charger comme jeu actif
- Gènes par dataset
- Harmonisation ComBat-seq (lot = jeu d'origine)
- Le jeu actif courant sera remplacé (comme un import). Enregistrez-le d'abord via « Enregistrer l'état courant » si nécessaire.
- Multi-jeux — Fusion de jeux
- NS\\(
- Nom du jeu fusionné
- PCA avant / après
- Progress\\$
- Résultat de la fusion
- Sélectionnez au moins 2 datasets enregistrés.
- active_contrast
- build_dds
- bulk_assert_raw_counts
- bulk_assert_transformed_matrix
- bulk_batch_correction_design
- bulk_batch_correction_label
- bulk_batch_correction_public_api
- bulk_batch_design_check
- bulk_batch_qc_public_api
- bulk_build_provenance
- bulk_ensure_provenance
- bulk_merge_align_genes
- bulk_merge_check_inputs
- bulk_merge_combine
- bulk_merge_common_meta_columns
- bulk_merge_error
- bulk_merge_error_states
- bulk_merge_preview_metadata
- bulk_merge_public_api
- bulk_merge_run
- bulk_merge_to_bulk_obj
- bulk_multi_capture_pipeline
- bulk_multi_check_label
- bulk_multi_check_obj
- bulk_multi_error_states
- bulk_multi_get
- bulk_multi_pipeline_fields
- bulk_multi_public_api
- bulk_multi_register
- bulk_multi_remove
- bulk_multi_summary
- bulk_provenance_dataframe
- bulk_provenance_known_genomes
- bulk_provenance_known_normalizations
- bulk_provenance_public_api
- bulk_provenance_session_packages
- bulk_update_provenance
- bulk_variance_partition
- capacity_exceeded
- contrasts
- design_not_applicable
- duplicate_label
- filtered_counts
- get_vst_matrix
- input\\$
- insufficient_datasets
- invalid_input
- invalid_label
- invalid_metadata
- invalid_obj
- invalid_pipeline
- isolate\\(
- lfc_thresh
- mapping_applied
- mapping_summary
- moduleServer
- multimethod_de
- no_common_contrast
- no_common_genes
- no_significant_genes
- observeEvent
- observe\\(
- output\\$
- padj_thresh
- pathway_db
- pathway_mode
- pathway_results
- plot_batch_correction_pca
- plot_bulk_batch_scree
- plot_bulk_pca
- plot_bulk_varpart
- reactiveVal
- reactiveValues
- reactive\\(
- renderDT
- renderPlot
- renderUI
- req\\(
- run_combat_seq
- sans correction
- session\\$
- showNotification
- ts_datatable
- unknown_label
- vst_mat

## 4. Colonne lot déclarée et façonnage du produit (extrait du code gelé)

- Le nom de colonne lot ajouté par la fusion est figé : `"dataset_origin"`.
- Le produit est façonné en bulk_obj avec `import_mode = "merge"` (contrat §7)
  et expose `counts_pre_combat` : les counts bruts fusionnés (PCA « avant »),
  à côté des counts corrigés ComBat-seq.
- Écart assumé : la fusion est chargée depuis le module
  `modules/bulk/mod_bulk_merge.R` et le produit est chargé comme jeu actif
  (comportement d'un import `mod_import_bulk`) — l'ancien chemin d'import
  widget n'est plus le seul point d'entrée.

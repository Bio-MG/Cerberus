# BULK_GSVA_CONTRACT_CONTRACT.md — Scores de voies par échantillon GSVA/ssGSEA (Bulk V2 M2)

> **Contrat reconstruit le 2026-09-27.** Le document original était
> absent du dépôt : `docs/` était ignoré par `.gitignore` (audit
> 2026-09-27 §0) et le test de gel lisait un document fantôme. Ce
> document est aligné sur le test de gel EXÉCUTABLE (source de vérité)
> et sur le code gelé. Règle contract-first : toute évolution passe
> SIMULTANÉMENT par le code, le test de gel et ce document.

## 1. Fichiers gelés

- Code : `R/bulk/bulk_batch_qc.R`
- Code : `R/bulk/bulk_gsva.R`
- Code : `R/core/io_helpers.R`
- Code : `R/core/provenance.R`
- Code : `R/core/validation.R`
- Code : `R/plotting/theme.R`
- Test : `tests/testthat/test-bulk-gsva-contract-freeze.R`

## 2. Surface publique et vectors figés (extrait du code)

`bulk_batch_qc_public_api` :
- `bulk_assert_transformed_matrix`
- `bulk_batch_design_check`
- `bulk_batch_qc_public_api`
- `bulk_variance_partition`
- `plot_bulk_batch_scree`
- `plot_bulk_varpart`
`bulk_gsva_public_api` :
- `build_pathway_scores_export`
- `bulk_bpparam`
- `bulk_clean_gene_ids`
- `bulk_filter_gene_sets`
- `bulk_gsva_methods`
- `bulk_gsva_public_api`
- `bulk_parse_gmt`
- `compute_pathway_scores`
- `plot_pathway_scores_heatmap`
- `plot_pathway_scores_pca`

## 3. Jetons de synchronisation cités par le test de gel

Chaque jeton ci-dessous doit rester présent dans ce document
(garde de synchronisation code <-> contrat).

- TS_BULK_GSVA_MAX_SIZE
- TS_BULK_GSVA_MIN_SIZE
- TS_BULK_GSVA_OVERLAP_MIN
- analysis_id
- build_pathway_scores_export
- bulk_assert_transformed_matrix
- bulk_batch_design_check
- bulk_batch_qc_public_api
- bulk_bpparam
- bulk_clean_gene_ids
- bulk_filter_gene_sets
- bulk_gsva_methods
- bulk_gsva_public_api
- bulk_parse_gmt
- bulk_variance_partition
- compute_failed
- compute_pathway_scores
- dropped
- gene_sets
- invalid_input
- matched_fraction
- method
- missing_dependency
- n_genes
- n_genes_input
- n_genes_matched
- n_input_sets
- n_matched
- n_samples
- n_used_sets
- no_gene_sets
- params
- pathway
- per_sample
- plot_bulk_batch_scree
- plot_bulk_varpart
- plot_pathway_scores_heatmap
- plot_pathway_scores_pca
- provenance
- raw_counts_rejected
- reason
- sample
- score
- scores
- set
- status
- timestamp_utc
- type
- warnings

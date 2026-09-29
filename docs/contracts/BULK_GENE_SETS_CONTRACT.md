# BULK_GENE_SETS_CONTRACT_CONTRACT.md — Catalogue natif de jeux de gènes (PLOT-S6b)

> **Contrat reconstruit le 2026-09-27.** Le document original était
> absent du dépôt : `docs/` était ignoré par `.gitignore` (audit
> 2026-09-27 §0) et le test de gel lisait un document fantôme. Ce
> document est aligné sur le test de gel EXÉCUTABLE (source de vérité)
> et sur le code gelé. Règle contract-first : toute évolution passe
> SIMULTANÉMENT par le code, le test de gel et ce document.

## 1. Fichiers gelés

- Code : `R/bulk/bulk_batch_qc.R`
- Code : `R/bulk/bulk_gene_sets.R`
- Code : `R/bulk/bulk_gsva.R`
- Code : `R/core/provenance.R`
- Code : `R/core/validation.R`
- Code : `R/plotting/theme.R`
- Test : `tests/testthat/test-bulk-gene-sets.R`

## 2. Surface publique et vectors figés (extrait du code)

`bulk_batch_qc_public_api` :
- `bulk_assert_transformed_matrix`
- `bulk_batch_design_check`
- `bulk_batch_qc_public_api`
- `bulk_variance_partition`
- `plot_bulk_batch_scree`
- `plot_bulk_varpart`
`bulk_gene_sets_public_api` :
- `bulk_gene_set_catalog`
- `bulk_gene_set_choices`
- `bulk_gene_sets_organisms`
- `bulk_gene_sets_public_api`
- `bulk_load_gene_sets`
- `bulk_write_gmt`
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

- build_pathway_scores_export
- bulk_assert_transformed_matrix
- bulk_batch_design_check
- bulk_batch_qc_public_api
- bulk_bpparam
- bulk_clean_gene_ids
- bulk_filter_gene_sets
- bulk_gene_set_catalog
- bulk_gene_set_choices
- bulk_gene_sets_organisms
- bulk_gene_sets_public_api
- bulk_gsva_methods
- bulk_gsva_public_api
- bulk_load_gene_sets
- bulk_parse_gmt
- bulk_variance_partition
- bulk_write_gmt
- compute_pathway_scores
- plot_bulk_batch_scree
- plot_bulk_varpart
- plot_pathway_scores_heatmap
- plot_pathway_scores_pca

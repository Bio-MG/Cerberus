# BULK_BATCH_QC_CONTRACT_CONTRACT.md — QC anti-counts-bruts & variance-partition (Bulk V2 M1)

> **Contrat reconstruit le 2026-09-27.** Le document original était
> absent du dépôt : `docs/` était ignoré par `.gitignore` (audit
> 2026-09-27 §0) et le test de gel lisait un document fantôme. Ce
> document est aligné sur le test de gel EXÉCUTABLE (source de vérité)
> et sur le code gelé. Règle contract-first : toute évolution passe
> SIMULTANÉMENT par le code, le test de gel et ce document.

## 1. Fichiers gelés

- Code : `R/bulk/bulk_batch_qc.R`
- Code : `R/bulk/bulk_helpers.R`
- Code : `R/core/io_helpers.R`
- Code : `R/core/validation.R`
- Code : `R/plotting/theme.R`
- Test : `tests/testthat/test-bulk-batch-qc-contract-freeze.R`

## 2. Surface publique et vectors figés (extrait du code)

`bulk_batch_qc_public_api` :
- `bulk_assert_transformed_matrix`
- `bulk_batch_design_check`
- `bulk_batch_qc_public_api`
- `bulk_variance_partition`
- `plot_bulk_batch_scree`
- `plot_bulk_varpart`

## 3. Jetons de synchronisation cités par le test de gel

Chaque jeton ci-dessous doit rester présent dans ce document
(garde de synchronisation code <-> contrat).

- .subset_variable_genes
- .varpart_pure_r_fallback
- Residuals
- batch
- bulk_assert_transformed_matrix
- bulk_batch_design_check
- bulk_batch_qc_public_api
- bulk_variance_partition
- condition
- cross_table
- empty_cells
- formula
- fully_collinear
- invalid_input
- method
- n_batch_levels
- n_condition_levels
- n_genes_total
- n_genes_used
- n_samples
- plot_bulk_batch_scree
- plot_bulk_varpart
- pur_lm_partial_r2
- raw_counts_rejected
- seed
- timestamp_utc
- var_part
- variancePartition::fitExtractVarPartModel
- warning_messages
- warnings

## 4. Seuil config (extrait du code gelé)

- Le sous-échantillonnage de gènes pour la variancePartition est plafonné par
  `TS_BULK_VARPART_MAX_GENES` (config/thresholds.R) — borne mémoire déclarée,
  consommée avec repli du code pur.

# BULK_WGCNA_CONTRACT_CONTRACT.md — WGCNA safe-mode (Bulk V2 M4)

> **Contrat reconstruit le 2026-09-27.** Le document original était
> absent du dépôt : `docs/` était ignoré par `.gitignore` (audit
> 2026-09-27 §0) et le test de gel lisait un document fantôme. Ce
> document est aligné sur le test de gel EXÉCUTABLE (source de vérité)
> et sur le code gelé. Règle contract-first : toute évolution passe
> SIMULTANÉMENT par le code, le test de gel et ce document.

## 1. Fichiers gelés

- Code : `R/bulk/bulk_batch_qc.R`
- Code : `R/bulk/bulk_wgcna.R`
- Code : `R/core/io_helpers.R`
- Code : `R/core/provenance.R`
- Code : `R/core/validation.R`
- Code : `R/plotting/theme.R`
- Test : `tests/testthat/test-bulk-wgcna-contract-freeze.R`

## 2. Surface publique et vectors figés (extrait du code)

`bulk_batch_qc_public_api` :
- `bulk_assert_transformed_matrix`
- `bulk_batch_design_check`
- `bulk_batch_qc_public_api`
- `bulk_variance_partition`
- `plot_bulk_batch_scree`
- `plot_bulk_varpart`
`bulk_wgcna_public_api` :
- `build_wgcna_export`
- `bulk_wgcna_build_modules`
- `bulk_wgcna_choose_power`
- `bulk_wgcna_module_trait`
- `bulk_wgcna_pick_power`
- `bulk_wgcna_prepare_traits`
- `bulk_wgcna_public_api`
- `bulk_wgcna_select_hvg`
- `plot_wgcna_soft_threshold`
- `plot_wgcna_trait_heatmap`

## 3. Jetons de synchronisation cités par le test de gel

Chaque jeton ci-dessous doit rester présent dans ce document
(garde de synchronisation code <-> contrat).

- MEs
- TS_BULK_WGCNA_MAX_BLOCKSIZE
- TS_BULK_WGCNA_MAX_GENES
- TS_BULK_WGCNA_MIN_GENES
- TS_BULK_WGCNA_MIN_MODULE
- TS_BULK_WGCNA_MIN_SAMPLES
- TS_BULK_WGCNA_R2_MIN
- build_wgcna_export
- bulk_assert_transformed_matrix
- bulk_batch_design_check
- bulk_batch_qc_public_api
- bulk_variance_partition
- bulk_wgcna_build_modules
- bulk_wgcna_choose_power
- bulk_wgcna_module_trait
- bulk_wgcna_pick_power
- bulk_wgcna_prepare_traits
- bulk_wgcna_public_api
- bulk_wgcna_select_hvg
- chosen
- colors
- compute_failed
- dendro
- dendro_colors
- gene
- invalid_input
- mean_k
- missing_dependency
- module
- module_sizes
- n_genes_input
- n_genes_used
- n_modules
- n_samples
- no_traits
- plot_bulk_batch_scree
- plot_bulk_varpart
- plot_wgcna_soft_threshold
- plot_wgcna_trait_heatmap
- power
- power_table
- provenance
- raw_counts_rejected
- samples_min
- status
- target_reached
- timestamp_utc
- too_few_genes
- trait_cor_method
- type
- warning
- warnings

## 4. Corrélation biweight figée

Les MEs et leurs associations aux traits utilisent `bicor` (corrélation
biweight midcorrelation, robuste aux outliers) — jamais la corrélation de
Pearson brute, et le choix est gelé dans le code et dans ce contrat.

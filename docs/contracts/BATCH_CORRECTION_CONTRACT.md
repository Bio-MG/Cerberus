# BATCH_CORRECTION_CONTRACT_CONTRACT.md — Correction de batch bulk (ComBat-seq, STAT-S1)

> **Contrat reconstruit le 2026-09-27.** Le document original était
> absent du dépôt : `docs/` était ignoré par `.gitignore` (audit
> 2026-09-27 §0) et le test de gel lisait un document fantôme. Ce
> document est aligné sur le test de gel EXÉCUTABLE (source de vérité)
> et sur le code gelé. Règle contract-first : toute évolution passe
> SIMULTANÉMENT par le code, le test de gel et ce document.

## 1. Fichiers gelés

- Code : `R/bulk/batch_correction.R`
- Code : `R/bulk/bulk_batch_qc.R`
- Code : `R/core/io_helpers.R`
- Code : `R/core/validation.R`
- Test : `tests/testthat/test-bulk-batch-correction-contract-freeze.R`

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

## 3. Jetons de synchronisation cités par le test de gel

Chaque jeton ci-dessous doit rester présent dans ce document
(garde de synchronisation code <-> contrat).

- bulk_assert_raw_counts
- bulk_assert_transformed_matrix
- bulk_batch_correction_design
- bulk_batch_correction_label
- bulk_batch_correction_public_api
- bulk_batch_design_check
- bulk_batch_qc_public_api
- bulk_variance_partition
- compute_failed
- degenerate_batch
- invalid_input
- missing_dependency
- not_raw_counts
- plot_batch_correction_pca
- plot_bulk_batch_scree
- plot_bulk_varpart
- run_combat_seq

## 4. Garde-fous et dépendances (extrait du code gelé)

- L'entrée de `run_combat_seq(` est validée par `bulk_assert_raw_counts(` :
  counts bruts uniquement (matrice continue refusée), et effectif minimal par
  lot `TS_BULK_BATCH_MIN_SAMPLES_PER_BATCH` (config/thresholds.R).
- Le plan de correction est contrôlé par `bulk_batch_correction_design(`, qui
  réutilise `bulk_batch_design_check(` (R/bulk/bulk_batch_qc.R) pour la
  cross-table et la collinéarité — aucune logique de plan dupliquée.
- `plot_batch_correction_pca(` ne trace rien elle-même : elle compose DEUX
  objets ggplot produits par `plot_bulk_pca(` (R/bulk/bulk_helpers.R),
  étiquetés A (avant) et B (après).

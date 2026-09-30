# BULK_SURVIVAL_CONTRACT_CONTRACT.md — Survie & associations cliniques (Bulk V2 M5)

> **Contrat reconstruit le 2026-09-27.** Le document original était
> absent du dépôt : `docs/` était ignoré par `.gitignore` (audit
> 2026-09-27 §0) et le test de gel lisait un document fantôme. Ce
> document est aligné sur le test de gel EXÉCUTABLE (source de vérité)
> et sur le code gelé. Règle contract-first : toute évolution passe
> SIMULTANÉMENT par le code, le test de gel et ce document.

## 1. Fichiers gelés

- Code : `R/bulk/bulk_survival.R`
- Code : `R/core/io_helpers.R`
- Code : `R/core/provenance.R`
- Code : `R/core/validation.R`
- Code : `R/plotting/theme.R`
- Test : `tests/testthat/test-bulk-survival-contract-freeze.R`

## 2. Surface publique et vectors figés (extrait du code)

`bulk_survival_public_api` :
- `build_survival_export`
- `bulk_survival_candidates`
- `bulk_survival_cox`
- `bulk_survival_km`
- `bulk_survival_public_api`
- `bulk_survival_split_groups`
- `bulk_survival_validate_metadata`
- `plot_survival_km`

## 3. Jetons de synchronisation cités par le test de gel

Chaque jeton ci-dessous doit rester présent dans ce document
(garde de synchronisation code <-> contrat).

- 0/1
- 1/2
- TS_BULK_SURV_MIN_EVENTS
- bicor_ou_cor_rien
- build_survival_export
- bulk_survival_candidates
- bulk_survival_cox
- bulk_survival_km
- bulk_survival_public_api
- bulk_survival_split_groups
- bulk_survival_validate_metadata
- compute_failed
- coxph
- invalid_input
- invalid_status
- invalid_time
- log-rank
- min_events
- plot_survival_km
- survfit

## 4. Point de coupure figé

Le cutpoint optimal (max LRT / min log-rank p) est calculé côté moteur et
voyage avec le résultat : le terme figé `cutpoint` désigne ce seuil
dichotomisé, et toute sortie KM/Cox le cite tel quel (aucun seuil
reconstruit après coup).

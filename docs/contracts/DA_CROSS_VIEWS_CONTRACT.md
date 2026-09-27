# DA_CROSS_VIEWS_CONTRACT.md — Vues croisées Milo × scCODA (Stage 16, 4E-3)

> **Contrat reconstruit le 2026-09-27.** Le document original était
> absent du dépôt : `docs/` était ignoré par `.gitignore` (audit
> 2026-09-27 §0) et le test de gel lisait un document fantôme. Ce
> document est aligné sur le test de gel EXÉCUTABLE (source de vérité)
> et sur le code gelé. Règle contract-first : toute évolution passe
> SIMULTANÉMENT par le code, le test de gel et ce document.

## 1. Fichiers gelés

- Code : `R/sc/sc_abundance_cross_views.R`
- Test : `tests/testthat/test-da-cross-views-contract-freeze.R`

## 2. Catégories de concordance (gelées)

`concordant_enriched_target`, `concordant_enriched_reference`,
`discordant_direction`, `milo_only`, `sccoda_only`, `no_signal`,
`not_comparable`.

## 3. Fonctions citées par le test de gel

`build_da_cross_method_summary`, `build_da_cross_concordance`,
`build_da_cross_provenance`, `da_cross_views_public_api`,
`da_cross_concordance_categories`.

## 4. Interdits scientifiques (documentés)

Aucune p-value de consensus n'est fabriquée : la vue croisée est
DESCRIPTIVE — elle croise les verdicts de deux méthodes par population.
La comparaison s'exprime au niveau ÉCHANTILLON (composition par
échantillon), jamais au niveau cellule.

# CONSOLIDATED_REPORT_CONTRACT_CONTRACT.md — Rapport consolidé — compilateur d'état (Stage 17, 4F)

> **Contrat reconstruit le 2026-09-27.** Le document original était
> absent du dépôt : `docs/` était ignoré par `.gitignore` (audit
> 2026-09-27 §0) et le test de gel lisait un document fantôme. Ce
> document est aligné sur le test de gel EXÉCUTABLE (source de vérité)
> et sur le code gelé. Règle contract-first : toute évolution passe
> SIMULTANÉMENT par le code, le test de gel et ce document.

## 1. Fichiers gelés

- Code : `R/reports/report_bundle.R`
- Code : `R/reports/report_collector.R`
- Code : `R/reports/report_render.R`
- Code : `R/reports/report_validator.R`
- Test : `tests/testthat/test-report-contract-freeze.R`
- Test : `tests/testthat/test-sc-report-contract.R`

## 2. Surface publique et vectors figés (extrait du code)

`report_public_api` :
- `build_consolidated_report_html`
- `build_report_bundle`
- `collect_consolidated_report_input`
- `consolidated_report_analyses`
- `consolidated_report_export_filename`
- `consolidated_report_input_recap`
- `consolidated_report_validation_states`
- `docs/contracts/CONSOLIDATED_REPORT_CONTRACT.md`
- `report_public_api`
- `validate_consolidated_report_input`
- `write_consolidated_report_html`

## 3. Jetons de synchronisation cités par le test de gel

Chaque jeton ci-dessous doit rester présent dans ce document
(garde de synchronisation code <-> contrat).

- .rep_stop
- .report_analysis_domains
- .report_banner
- .report_banner_colors
- .report_bundle_readme
- .report_bundle_stop
- .report_bundle_tables
- .report_config_keys
- .report_contract_domains
- .report_css
- .report_domain_labels
- .report_domain_summary
- .report_global_domains
- .report_html_table
- .report_kv_df
- .report_legacy_domains
- .report_render_stop
- .report_state_labels
- .report_val_stop
- absent
- blocked
- build_consolidated_report_html
- build_report_bundle
- bulk_multi_comparison
- bulk_multi_concordance.csv
- bulk_multi_intersection.csv
- bulk_multi_per_dataset.csv
- collect_consolidated_report_input
- communication
- communication_by_sample.csv
- communication_canonical.csv
- communication_condition_summary.csv
- communication_sample_manifest.csv
- consolidated_report_analyses
- consolidated_report_export_filename
- consolidated_report_input_recap
- consolidated_report_validation_states
- correlation
- da_cross
- da_design
- da_milo
- da_sccoda
- docs/contracts/CONSOLIDATED_REPORT_CONTRACT.md
- invalid
- markers
- mod_sc_report_consolidated_output_ui
- mod_sc_report_consolidated_server
- mod_sc_report_consolidated_ui
- pathways
- pseudobulk
- report_bundle.R
- report_public_api
- report_render.R
- report_validator.R
- stale
- trajectory
- unknown
- valid
- valid_legacy
- validate_consolidated_report_input
- velocity
- write_consolidated_report_html

## Surface figée et états (jetons du test de gel)

- L'identifiant de compilation du rapport est `sc-report-consolide`, produit
  par le COMPILATEUR de sections (aucune section ne se calcule elle-même).
- Plafond de taille : `TS_REPORT_MAX_TABLE_ROWS` (config/thresholds.R).
- Consommation multi-jeux bulk : `global_data$bulk_multi_comparison` ;
  communication : `communication_collection` et
  `active_communication_sample`.
- Couverture : `12 domaines figés` ; principe de composition : l'échantillon biologique est l'unité de réplication
  (le rapport ne ré-exécute JAMAIS une analyse : aucune analyse ré-exécutée, uniquement composition des résultats gelés ;
  règle 9b appliquée aux populations signalées — plafond, jamais compte exact).
- États de fraîcheur des sections : `7 états **figés**` —
  `valid`, `valid_legacy`, `unknown`, `stale`, `invalid`, `blocked`,
  `absent`.

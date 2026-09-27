# BULK_MULTI_CONTRACT_CONTRACT.md — Conteneur bulk_datasets, comparaison multi-jeux & pont pseudobulk (MD-1/2/3)

> **Contrat reconstruit le 2026-09-27.** Le document original était
> absent du dépôt : `docs/` était ignoré par `.gitignore` (audit
> 2026-09-27 §0) et le test de gel lisait un document fantôme. Ce
> document est aligné sur le test de gel EXÉCUTABLE (source de vérité)
> et sur le code gelé. Règle contract-first : toute évolution passe
> SIMULTANÉMENT par le code, le test de gel et ce document.

## 1. Fichiers gelés

- Code : `R/bulk/bulk_helpers.R`
- Code : `R/bulk/bulk_multi.R`
- Code : `R/bulk/bulk_multi_compare.R`
- Code : `R/core/io_helpers.R`
- Code : `R/core/validation.R`
- Code : `R/plotting/theme.R`
- Test : `tests/testthat/test-bulk-multi-compare-contract-freeze.R`
- Test : `tests/testthat/test-bulk-multi-contract-freeze.R`
- Test : `tests/testthat/test-bulk-multi.R`

## 2. Surface publique et vectors figés (extrait du code)

`bulk_multi_compare_public_api` :
- `bulk_multi_common_contrasts`
- `bulk_multi_compare_public_api`
- `bulk_multi_concordance`
- `bulk_multi_deg_gene_sets`
- `bulk_multi_entry_contrasts`
- `bulk_multi_run_comparison`
- `bulk_multi_volcano_panel`
- `bulk_multi_volcano_scales`
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

## 3. Jetons de synchronisation cités par le test de gel

Chaque jeton ci-dessous doit rester présent dans ce document
(garde de synchronisation code <-> contrat).

- (20)
- Aucun dataset enregistré pour l'instant.
- Aucun gène dans les intersections avec ces seuils.
- Aucun jeu Bulk actif à enregistrer.
- Aucun nom de contraste commun aux datasets sélectionnés.
- Comparaison impossible : %s
- Comparaison multi-jeux
- Comparez des jeux enregistrés (import nommé ou bouton « Enregistrer l'état courant ») partageant un même nom de contraste. Les seuils choisis s'appliquent à TOUS les jeux comparés.
- Concordance de direction
- Contraste commun
- Dataset à supprimer
- Datasets enregistrés
- Datasets à comparer (>= 2)
- Détail des datasets
- En attente — sélectionnez >= 2 datasets puis lancez la comparaison.
- Enregistrer l'état courant
- Enregistrez l'état courant (import + filtrage + DE + voies) sous un label, pour le comparer plus tard à d'autres jeux. Le jeu actif n'est jamais modifié.
- Gènes par intersection
- Isolation par copie
- Jaccard = recouvrement des ensembles Up (resp. Down) entre deux jeux. « % même direction » = proportion des gènes significatifs communs qui varient dans le même sens.
- Label du dataset
- Label multi-datasets (optionnel)
- Lancer la comparaison
- MD-2
- MD-3
- MD-4
- Moins de 2 datasets avec des gènes significatifs — pas d'UpSet.
- Multi-jeux — Datasets enregistrés
- R/bulk/bulk_gsva.R
- R/bulk/bulk_signatures.R
- R/bulk/bulk_survival.R
- R/bulk/bulk_wgcna.R
- Recouvrement des DEGs
- Résultats de la comparaison
- Si renseigné, le jeu importé est aussi enregistré sous ce nom pour la comparaison multi-jeux (Bulk > Multi-jeux).
- Supprimer le dataset
- Sélectionnez au moins 2 datasets traités (via « Multi-jeux — Datasets enregistrés » ou l'import nommé).
- TS_BULK_MULTI_MAX_DATASETS
- Un dataset comparé a été supprimé depuis le calcul — relancez la comparaison.
- Volcanos côte à côte
- Zéro mutation
- \\bNS\\(
- `pseudobulk` = pont depuis
- active_contrast
- build_contrast_gene_sets <- function
- build_contrast_gene_sets(
- build_contrast_intersection_dt <- function
- build_contrast_intersection_dt(
- bulk_multi_capture_pipeline
- bulk_multi_capture_pipeline(
- bulk_multi_check_label
- bulk_multi_check_label(
- bulk_multi_check_obj
- bulk_multi_common_contrasts
- bulk_multi_compare_public_api
- bulk_multi_concordance
- bulk_multi_deg_gene_sets
- bulk_multi_entry_contrasts
- bulk_multi_error_states
- bulk_multi_get
- bulk_multi_pipeline_fields
- bulk_multi_public_api
- bulk_multi_register
- bulk_multi_register(
- bulk_multi_remove
- bulk_multi_remove(
- bulk_multi_run_comparison
- bulk_multi_summary
- bulk_multi_summary(
- bulk_multi_volcano_panel
- bulk_multi_volcano_scales
- capacity_exceeded
- contrasts
- coord_cartesian
- counts_mapped
- counts_original
- dds_blind
- dds_full
- duplicate_label
- ex : GSE123_T2
- filtered_counts
- global_data$bulk_datasets
- global_data$bulk_multi_comparison
- input\\$
- insufficient_datasets
- invalid_input
- invalid_label
- invalid_obj
- invalid_pipeline
- jaccard_up
- lfc_thresh
- mapping_applied
- mapping_summary
- moduleServer
- modules/bulk/mod_bulk_filter.R
- modules/bulk/mod_bulk_mapping.R
- modules/bulk/mod_bulk_pathways.R
- modules/bulk_de/mod_bulk_de.R
- modules/bulk_de/mod_bulk_de_engine.R
- modules/bulk_de/mod_bulk_de_multimethod.R
- modules/bulk_de/mod_bulk_de_pairwise.R
- modules/bulk_de/mod_bulk_de_run.R
- modules/bulk_de/mod_bulk_de_summary.R
- modules/bulk_de/mod_bulk_de_ui.R
- modules/bulk_de/mod_bulk_de_venn.R
- modules/bulk_de/mod_bulk_de_viz.R
- modules/sc/mod_sc_pseudobulk.R
- multimethod_de
- no_common_contrast
- no_significant_genes
- observeEvent
- observe\\(
- output\\$
- overwrite = TRUE
- padj_thresh
- page_length = 6
- patchwork::wrap_plots
- pathway_db
- pathway_mode
- pathway_results
- pct_same_direction
- plot_volcano_bulk <- function
- plot_volcano_bulk(
- reactiveVal(?!ues)
- reactive\\(
- registered_at
- renderDT
- session\\$
- showNotification
- spatial_multi_integration
- stored_
- ts_datatable
- unknown_label
- updated_at
- vst_mat
- ⚠️ Dataset non enregistré (multi-jeux) : %s
- ✓ %s — %d datasets : %s
- ✓ Comparaison calculée (contraste « %s », %d datasets).
- ✓ Dataset « %s » supprimé.
- ✓ État enregistré (mis à jour) sous « %s ».
- ✓ État enregistré sous « %s ».
- 📦 Import enregistré pour la comparaison multi-jeux : « %s ».

## 4. Activation d'un dataset enregistré (amendé 2026-09-27, parité SC_MULTI roadmap 4.1)

Le bouton « ✅ Activer ce dataset » du module de gestion
(`modules/bulk/mod_bulk_datasets.R`) relit l'entrée via `bulk_multi_get(`
et écrit `global_data$bulk_obj <- entry$obj` — SEULE écriture autorisée
sur le jeu actif dans ce module (la garde de non-écriture §2.1 est amendée
en conséquence, test de gel aligné). Le bump de `global_data$bulk_obj_epoch`
qui suit déclenche la purge des résultats partagés bulk (audit 2026-09-27
§1.5 transposé au domaine bulk — handoff §4) : `bulk_purge_shared_results()`
(R/core/state.R) remet à NULL les champs résultats de `shared_rv`
(contrastes, voies, mapping, VST...), pilotée par l'observer de `mod_bulk.R`
sur `bulk_obj_epoch`. Les re-commits de même lignée (filtrage, ComBat,
signatures, WGCNA, survie) ne bumpent PAS l'epoch — chaque étape ne doit pas
effacer les résultats qu'elle vient de calculer.

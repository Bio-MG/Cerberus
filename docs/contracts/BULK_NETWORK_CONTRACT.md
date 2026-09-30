# BULK_NETWORK_CONTRACT_CONTRACT.md — Réseau PCSF / interactome local (NEW-3)

> **Contrat reconstruit le 2026-09-27.** Le document original était
> absent du dépôt : `docs/` était ignoré par `.gitignore` (audit
> 2026-09-27 §0) et le test de gel lisait un document fantôme. Ce
> document est aligné sur le test de gel EXÉCUTABLE (source de vérité)
> et sur le code gelé. Règle contract-first : toute évolution passe
> SIMULTANÉMENT par le code, le test de gel et ce document.

## 1. Fichiers gelés

- Code : `R/bulk/bulk_multi.R`
- Code : `R/bulk/bulk_network.R`
- Code : `R/core/io_helpers.R`
- Test : `tests/testthat/test-bulk-network-contract-freeze.R`

## 2. Surface publique et vectors figés (extrait du code)

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
`bulk_network_contract_fields` :
- `analysis_id`
- `edges`
- `id_type`
- `map_rate`
- `node_role`
- `nodes`
- `parameters`
- `prizes`
- `provenance`
- `source_db`
- `source_version`
- `species`
- `status`
- `timestamp_utc`
- `type`
- `warnings`
`bulk_network_public_api` :
- `assert_bulk_network_object`
- `assert_bulk_network_result`
- `build_bulk_network_table_export`
- `bulk_network_contract_fields`
- `bulk_network_error_state`
- `bulk_network_map_ids`
- `bulk_network_memo_clear`
- `bulk_network_node_roles`
- `bulk_network_pcsf_params`
- `bulk_network_pcsf_params_default`
- `bulk_network_public_api`
- `bulk_network_source_available`
- `bulk_network_species_supported`
- `bulk_network_species_unavailable_reason`
- `bulk_network_validity_states`
- `load_bulk_network`
- `plot_bulk_network`
- `run_bulk_network_pcsf`

## 3. Jetons de synchronisation cités par le test de gel

Chaque jeton ci-dessous doit rester présent dans ce document
(garde de synchronisation code <-> contrat).

- -log10(padj) — recommandé
- .strip_i18n_html
- 11 030
- 292 895
- 3g. Réseau PCSF (interactome)
- 53,1
- AnnotationDbi::keys
- AnnotationDbi::mapIds
- Calcul du sous-réseau PCSF (heuristique)...
- Calculer le sous-réseau
- Chargement du réseau de voies (Reactome, hors ligne)...
- Deux groupes ne sont reliés que s'ils sont séparés par au plus {d} arête(s).
- Erreur réseau PCSF:
- Espèce (déclarée)
- Export CSV
- Humain (hsapiens)
- Interactome dérivé des voies Reactome — ce n'est PAS un PPI.
- NS\\(
- Nœuds
- PAS un PPI
- Paramètres & QC
- Paramètres du moteur (heuristique)
- Prime (score du nœud)
- Progress\\$
- Réseau PCSF
- Réseau PCSF (interactome)
- Seuil de prime (déclaré)
- Source des gènes d'intérêt
- Sous-réseau PCSF (heuristique)
- TS_BULK_NETWORK_MAX_NODES
- TS_BULK_NETWORK_MIN_MAP_RATE
- active_contrast
- analysis_id
- assert_bulk_network_object
- assert_bulk_network_result
- build_bulk_network_table_export
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
- bulk_network_contract_fields
- bulk_network_error
- bulk_network_error_state
- bulk_network_map_ids
- bulk_network_memo_clear
- bulk_network_node_roles
- bulk_network_pcsf_params
- bulk_network_pcsf_params_default
- bulk_network_public_api
- bulk_network_source_available
- bulk_network_species_supported
- bulk_network_species_unavailable_reason
- bulk_network_validity_states
- capacity_exceeded
- context
- contract_violation
- contrasts
- convert_ids
- duplicate_label
- edges
- empty_source
- filtered_counts
- graphite
- id_type
- ids
- igraph::components
- igraph::distances
- igraph::graph_from_data_frame
- igraph::shortest_paths
- input\\$
- insufficient_datasets
- invalid_input
- invalid_label
- invalid_obj
- invalid_pipeline
- isolate\\(
- layout_seed
- lfc_thresh
- load_bulk_network
- map_rate
- mapping_applied
- mapping_summary
- max_join_hops
- max_label_nodes
- missing_dependency
- mmuReactome.db
- moduleServer
- multimethod_de
- network
- no_common_contrast
- no_overlap
- no_significant_genes
- node_role
- nodes
- observeEvent
- observe\\(
- org.Hs.eg.db
- output\\$
- padj_thresh
- parameters
- params
- pathway_db
- pathway_mode
- pathway_results
- plot_bulk_network
- prizes
- provenance
- reactiveVal
- reactiveValues
- reactive\\(
- reactome.db
- refresh
- relay
- renderDT
- renderPlot
- renderUI
- req\\(
- result
- run_bulk_network_pcsf
- session\\$
- showNotification
- source
- source_db
- source_unavailable
- source_version
- species
- status
- tab_bulk_network
- terminal
- threshold
- timestamp_utc
- too_many_prizes
- ts_datatable
- type
- unknown_label
- valid
- valid_with_warnings
- vst_mat
- warnings
- {n} avertissement(s).
- |log2FC| — direction ignorée
- β (par arête)
- μ (par relais)
- ω (ouverture d'arbre)

## 4. Honnêteté de l'heuristique

Le moteur PCSF est une HEURISTIQUE, PAS un optimum : le sous-réseau produit
n'est pas optimum au sens strict (pas de garantie d'optimalité), et la mise
en garde est portée par l'UI ET par le résultat, pas seulement par un
commentaire de code.

## 5. Invariant de forêt, critère de rentabilité, mesures corrigées

- Le sous-graphe retenu est une forêt : chaque composante connectée paie son
  arbre (ω par arbre dans le score). Le moteur vérifie lui-même sa conformité
  contractuelle (`contract_violation`) et documente le nombre de composantes
  dans `qc$n_trees` — le réseau Reactome n'est pas connexe, certaines primes
  ne sont pas reliables.
- Critère de rentabilité gelé (§6.3) : ω > β·d + μ·(d−1) — une arête de
  jonction de d arêtes n'est prise que si la prime depasse son coût. ω=1 est
  le cas DÉGÉNÉRÉ (max_join_hops = 0) : avec une prime à 1 par arête, rien ne
  se relierait jamais — le contrat l'explique, il ne le subit pas.
- Mesures du interactome : la sonde de la proposition annonçait « 19 107
  arêtes dédupliquées, degré moyen ~1,7, graphe creux ». Re-mesuré sur
  `graphite::edges(which = "protein")` : 292 895 arêtes dédupliquées et un
  degré moyen de 53,1. Les chiffres annoncés (19 107, 1,7) ne sont pas
  reproductibles ; le contrat gelé retient les mesures locales corrigées.

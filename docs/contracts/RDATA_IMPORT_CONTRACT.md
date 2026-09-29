# RDATA_IMPORT_CONTRACT_CONTRACT.md — Import .rda/.RData — inspection & classification

> **Contrat reconstruit le 2026-09-27.** Le document original était
> absent du dépôt : `docs/` était ignoré par `.gitignore` (audit
> 2026-09-27 §0) et le test de gel lisait un document fantôme. Ce
> document est aligné sur le test de gel EXÉCUTABLE (source de vérité)
> et sur le code gelé. Règle contract-first : toute évolution passe
> SIMULTANÉMENT par le code, le test de gel et ce document.

## 1. Fichiers gelés

- Code : `R/core/rdata_io.R`
- Test : `tests/testthat/test-core-rdata.R`
- Test : `tests/testthat/test-rda-comm-velocity.R`
- Test : `tests/testthat/test-rdata-contract-freeze.R`

## 2. Surface publique et vectors figés (extrait du code)

## 3. Jetons de synchronisation cités par le test de gel

Chaque jeton ci-dessous doit rester présent dans ce document
(garde de synchronisation code <-> contrat).

- cellchat
- image
- matrix
- metadata
- other
- rdata_assert_class
- rdata_classify_object
- rdata_describe_objects
- rdata_export_paths
- rdata_export_selection
- rdata_extract_object
- rdata_extract_path
- rdata_flatten_env
- rdata_free
- rdata_load_env
- rdata_read_file_env
- sce
- seurat
- spliced
- unspliced
- velocity

## Erreurs classées (extrait du code)

Toute erreur d'import .RData/.Rds est classée `rdata_import_error` :
message français, état structuré, aucune reconstruction silencieuse de
l'objet manquant.

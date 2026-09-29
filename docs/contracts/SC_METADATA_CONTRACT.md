# SC_METADATA_CONTRACT.md — Design expérimental (condition / réplicat)

Gelé le 2026-09-27. Test de gel : `tests/testthat/test-sc-metadata-contract-freeze.R`.
Unitaires : `tests/testthat/test-sc-metadata.R`.

## 1. Objet

Déclarer `condition` et `replicate` dans `meta.data` d'un objet Seurat, au
niveau ÉCHANTILLON (`orig.ident` par défaut), sur commit explicite.
Motivation (audit 2026-09-27 §2) : aucun module n'écrivait `condition` —
pseudobulk A-vs-B (`sc_abundance_design.R:376-389`), Milo/scCODA
(`mod_sc_da_design.R:152-157`), plots par condition (`mod_sc.R:798-851`) et
communication par condition étaient structurellement inaccessibles.

## 2. Surface publique gelée (R/sc/sc_metadata.R)

`sc_metadata_public_api()`, `sc_metadata_error_class()`,
`sc_metadata_map_modes()`, `sc_metadata_sample_table(meta, sample_col)`,
`sc_metadata_parse_sample_label(lbl, cond_position)`,
`sc_metadata_parse_sample_table(sample_tbl, cond_position)`,
`sc_metadata_read_csv(path)`, `sc_metadata_join_csv(meta, csv, key_col,
sample_col, condition_col, replicate_col)`,
`sc_metadata_apply(meta, sample_tbl, sample_col)`,
`sc_metadata_design_recap(sample_tbl, cells_per_sample)`.

Signatures gelées — tout ajout passe par une extension documentée du contrat.

## 3. Règles métier (gelées)

1. Une CONDITION est une propriété de l'ÉCHANTILLON, jamais de la cellule.
2. Modes de remplissage : `manual` (table éditable), `parse_labels`
   (`cond_position ∈ {first, last}` : "A_1" → A/1 ; "1a" → a/1), `csv`
   (clé = échantillon, colonnes `sample`, `condition`, [`replicate`]).
3. `sc_metadata_apply()` / `sc_metadata_join_csv()` ÉCHOUENT
   (`sc_metadata_error`) si un échantillon de l'objet manque au design —
   aucune condition fabriquée, aucun repli silencieux.
4. Réplicat vide ⇒ repli sur l'identifiant échantillon (l'échantillon EST le
   réplicat — convention du module DA, `da_replicate_col`).
5. `sc_metadata_design_recap()` informe AVANT commit ; il ne bloque pas — le
   blocage dur reste dans `validate_da_design` / le module pseudobulk
   (plancher `TS_DA_MIN_REPLICATES_PER_CONDITION = 2L`, repli 2L).
6. Erreurs : classe `sc_metadata_error` unique (pattern assert_* du dépôt).

## 4. Pureté

`R/sc/sc_metadata.R` est pur : AUCUN symbole Shiny (reactiveVal,
reactiveValues, observeEvent, moduleServer, showNotification, output$,
input$, isolate(, reactive() interdits — même garde que SC_MULTI_CONTRACT).

## 5. Câblage module (additif)

- `modules/sc/mod_sc_metadata.R` : UI « 0.5 Métadonnées — condition /
  réplicat » dans `grp_prep`, valeur de panneau `0_metadata` ;
- `mod_sc.R` : `mod_sc_metadata_ui(ns("metadata"))` +
  `mod_sc_metadata_server("metadata", global_data)` ;
- `app.R` : `source("R/sc/sc_metadata.R")` (avant les modules) et
  `source("modules/sc/mod_sc_metadata.R")`.
- Zéro changement de comportement sans commit explicite : le module
  n'AJOUTE que des colonnes meta.data ; aucune écriture sur sc_datasets ;
  aucune écriture sur `global_data$sc_obj` hors le commit du design.

## 6. Interactions état (audit 2026-09-27 §1.5)

Le commit du design ré-assigne `global_data$sc_obj` (même lignée) — il
n'incrément PAS `global_data$sc_obj_epoch` : les résultats déjà calculés
restent valides (ajouter des colonnes ne change ni comptages ni clusters).

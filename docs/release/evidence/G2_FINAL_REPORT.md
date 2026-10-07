# Rapport — DRIVE LIVE CONTROL, **grade G2**

**Date** : 2026-09-22 · **HEAD** : `5a33b5773c722f225897004730fd040df8ac61a0` (`5a33b57`)
**Arbre** : propre (`git status --short` vide) · **G3** : non démarré
**Garde de conventions** : non modifiée · **drive `sc`/`spatial`** : non ajouté

---

## 1. Périmètre demandé, et ce qui y a été fait

| # | Demande | État | Preuve |
|---|---|---|---|
| 1 | Lier les vrais boutons bulk sur la session vivante | ✅ | les 4 sites répondent, et le refus **nomme le bouton** — ce qu'un bouton NON lié ne peut pas faire |
| 2 | Garder `preserve_data=true` par défaut | ✅ | `preserve_data: true` dans **chaque** `result.json` (seq 1→8) |
| 3 | Prouver que **deux** `run_pipeline` consécutifs partent | ⚠️ **partiellement** | live : seq 3 et 4 **tous deux consommés** (`ack_seq` 3 puis 4, le second n'est pas perdu). Le **déclenchement effectif** (`done`) exige un objet préchargé → reste **G2/G3** (formulation gelée) |
| 4 | Sans objet chargé ⇒ `invalid` **explicite** | ✅ | `invalid` + « bound but not ready: no bulk object loaded (shared_rv$filtered_counts is NULL) » |
| 5 | Ne pas imiter `fileInput` | ✅ | aucun id `fileInput` dans l'allowlist ; la garde import lit le **widget**, jamais un chemin |
| 6 | S'arrêter après G2 ; attendre avant G3 | ✅ | `import_file` / `reset_module` rendent toujours `invalid` |

**G2 n'est PAS déclaré « complet » au-delà de ces lignes** : le point 3 dans sa
forme `done` (exécution bulk réelle sur objet préchargé) n'a **pas** été prouvé,
et n'a pas été tenté — c'est le périmètre G2/G3 gelé.

---

## 2. Commits

| SHA | Objet |
|---|---|
| `5a33b57` | `drive: grade G2 — live bulk button bind, readiness gate, two silent seams fixed` — 6 fichiers, **+688 / −31** |

`R/core/drive_watcher.R`, `modules/bulk/mod_bulk_pathways.R`,
`modules/bulk_de/mod_bulk_de_run.R`, `modules/import/mod_import_bulk.R`,
`tests/testthat/test-drive-watcher.R`, `tools/_drive/README.md`.

## 3. Tests drive

| Fichier | fail | pass | error | skip |
|---|---|---|---|---|
| `test-drive-watcher.R` | **0** | **422** | **0** | **0** |
| `test-drive-allowlist.R` | **0** | **212** | **0** | **0** |
| **Total** | **0** | **634** | **0** | **0** |

Ligne de base G0/G1 : **557**. **Δ = +77 assertions.**

**Falsifications** (bug restauré, puis arbre rétabli **à l'octet**) :

| Retiré | Effet |
|---|---|
| la garde dans `ts_drive_apply()` | **2 tests rouges** |
| `ready = …` d'**un** module | **1 test rouge** |
| l'ancienne lecture du registre (`$` nu) | **9 assertions rouges** |
| `button` / `expect` de la whitelist | **5 échecs + 1 erreur** |

## 4. Garde de conventions

```
---- Résumé : 0 erreur(s), 59 avertissement(s) ----
```

**Identique** à la ligne de base gelée. `tools/check_conventions.R` **hors du
diff** (`git diff HEAD~1 -- tools/check_conventions.R` → vide).

## 5. Table d'acceptation — session Shiny **VIVANTE**

Harnais : `chromote` (client réel) + `tools/launch_dev_drive.R`, port **7789**,
`options(ts.drive.interactive = TRUE)`. Rien n'est simulé : `ready.json` provient
d'un vrai `httpuv`.

| seq | action | module | status | ack_seq | preserve_data | erreur |
|---|---|---|---|---|---|---|
| 1 | `snapshot` | bulk_de | `done` | 1 | `true` | — |
| 2 | `set_inputs` | bulk_de | `applied` | 2 | `true` | — |
| 3 | `run_pipeline` | bulk_de | `invalid` | 3 | `true` | `bound but not ready: no bulk object loaded (shared_rv$filtered_counts is NULL)` |
| 4 | `run_pipeline` | bulk_de | `invalid` | 4 | `true` | idem — **2ᵉ scénario consécutif, non perdu** |
| 5 | `run_pipeline` | bulk_pathways | `invalid` | 5 | `true` | `bulk-pathways-run_pathway … not ready: no bulk object loaded` |
| 6 | `run_pipeline` | import_bulk | `invalid` | 6 | `true` | `import_bulk-btn_load … not ready: no counts file selected (fileInput `counts_file` is empty)` |
| 7 | `run_pipeline` (bouton explicite) | bulk_pathways | `invalid` | 7 | `true` | `bulk-pathways-run_scores … not ready: Step 1 has not produced a VST matrix (shared_rv$vst_mat is NULL)` |
| 8 | `snapshot` | bulk_de | `done` | 8 | `true` | — |
| 9 | `noop` après désarmement | bulk_de | **non consommé** | — | — | correct (spec G0.1) |

**Les quatre boutons liés sont prouvés vivants** : chacun rend le refus
**propre à sa garde**, et un bouton non lié rendrait « *not bound — its
observeEvent does not read…* », message qu'**aucun** des quatre ne produit plus.

Heartbeat : `hb_timeout_s=15` ; **re-arm** de la même session ⇒ `armed:true`,
`hb_n` 9 → 10 → 11, `ready.json` **frais** après 16 s ; **aucun** `*.tmp` résiduel.

## 6. Défauts trouvés par la session vivante (les deux muets)

1. **Le registre n'était jamais peuplé.** `ts_drive_publish_token()` lisait
   `global_data$drive_registry` avec un `$` nu ; Shiny **lève** sur un champ de
   `reactiveValues` lu **hors reactive consumer** — et l'init d'un
   `moduleServer()` en est un :
   `Can't access reactive value 'drive_registry' outside of reactive consumer.`
   Un `tryCatch(..., error = function(e) NULL)` transformait cet abort en `NULL`
   **muet** ⇒ les 4 boutons « not bound » alors que le câblage source était
   **correct**. **C'est la cause du constat live G0/G1.** Corrigé :
   `ts_drive_registry()` → `shiny::isolate()`.
2. **Le validateur avalait `button` et `expect`.** `ts_drive_validate_scenario()`
   reconstruit le scénario en whitelist ; les deux champs en étaient absents ⇒
   `scn$button` toujours `NULL`, retombée sur le bouton **par défaut**, et
   `bulk-pathways-run_scores` **inatteignable**. Constaté en live : un scénario
   nommant `run_scores` est revenu pour `run_pathway`.

## 7. Observation **reportée**, non corrigée

`ready.json` n'est **pas** réécrit au **premier** arm suivant le démarrage de la
session : il conserve `armed:false, hb_n:0` et devient **périmé** après
`hb_timeout_s`, alors que le protocole **fonctionne** (les scénarios sont
traités). **Re-armer la même session** le rétablit (`hb_n` monte, frais à 16 s).
Suspect : violation de partage Windows sur `unlink()`/`file.rename()` d'un
fichier tout juste créé, rendue invisible par le `try(..., silent = TRUE)` du
heartbeat — **même classe** que le défaut 1. **Territoire G0/G1, gelé, hors
périmètre G2** : signalé, pas corrigé.

## 8. Reproduction

```bash
D:/Data_science/R-4.4.2/bin/Rscript.exe tools/check_conventions.R
D:/Data_science/R-4.4.2/bin/Rscript.exe -e 'Sys.setlocale("LC_CTYPE","fr_FR.UTF-8");
  library(testthat); library(shiny);
  test_file("tests/testthat/test-drive-watcher.R", reporter="summary")'
D:/Data_science/R-4.4.2/bin/Rscript.exe tools/launch_dev_drive.R   # session vivante
```

⚠️ `EXIT=139` est **normal même en succès** (démontage) : le verdict vit dans les
fichiers, jamais dans le code de sortie. La session vivante est une **observation
unique** ; ce qu'un clone reproduit, c'est la suite hors-ligne et la garde.

## 9. En attente

**G3 n'est pas démarré.** Prochaines marches, sur instruction seulement :
`import_file`, préservation des données, `run_pipeline` sur **objet préchargé**
(le seul point de G2 laissé explicitement ouvert par la formulation gelée).

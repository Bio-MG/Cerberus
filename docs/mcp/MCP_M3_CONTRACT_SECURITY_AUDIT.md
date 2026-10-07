# M3 contract & security audit — READ-ONLY (2026-09-24)

**Nature** : audit **lecture seule**. Aucun fichier du dépôt, du serveur MCP, du protocole drive, de
l'application, des tests, des configs clients, de la doc, du `renv.lock` ou des gardes n'a été
modifié. **Aucune** session armée/désarmée. **`snapshot`, `set_inputs`, `run` et `wait` n'ont été ni
invoqués ni implémentés.** **M3 et M4 ne sont pas commencés.**

## 0. Porte pré-audit

| Contrôle | Résultat |
|---|---|
| Manifeste recalculé | `48f758e35f4962ac57074d60590b0c7f78ccef21519e3e0308b5ee49a49115f1` (**437** f.) |
| Exigé | `48f758e35f4962ac57074d60590b0c7f78ccef21519e3e0308b5ee49a49115f1` |
| Verdict | ✅ **PASS** — aucun dérive |
| `git status` / `HEAD` | ` M renv.lock` (préexistant) · `9cb1a06` |

---

## 1. Contrats mesurés (sources : `R/core/drive_watcher.R`, `R/core/drive_allowlist.R`)

### 1.1 Actions du fil (`TS_DRIVE_ACTIONS`)
`noop` · `set_inputs` · `run_pipeline` · `import_file` · `snapshot` · `reset_module`.
⚠️ `reset_module` est **déclaré mais NON implémenté** : `ts_drive_apply()` répond
`invalid` + *"reset_module is not implemented in this grade (G3)"*.

### 1.2 `snapshot` — données exactes exposables
`ts_drive_snapshot(global_data)` rend :
`has_data`, `object_class` (= `class(obj)[1]`), `n_genes`/`n_samples` (= `nrow`/`ncol` de
`obj$counts`), `error_state` (via `ts_error_state()`), et **`modules`** = un objet par module, produit
par les sondes `state` que les modules publient via `ts_drive_publish_token()`.

**Ce que les sondes exposent RÉELLEMENT** (mesuré) :

| Sonde | Champs |
|---|---|
| `mod_bulk_filter.R:294` | `filtered_counts` / `vst_mat` → `list(n_genes, n_samples, **samples = colnames(m)**)` |
| `mod_bulk_de_run.R:63` | `n_contrasts`, **`active_contrast`** (nom de contraste), `n_genes`, `n_padj_finite`, `n_significant`, `convention`, `bypass` |
| `mod_bulk_pathways.R:229` | `module`, `action`, `seq`, `status`, `elapsed_s`, `n_results`, `error` |

🔴 **F-B — `samples = colnames(m)` EST de la donnée biologique** (identifiants d'échantillons) et
`active_contrast` est un libellé utilisateur. ⇒ **un outil `snapshot` qui relaierait `snapshot$modules`
verbatim FUITERAIT.** Le garde-fou existe déjà : `.ts_tool_read_result()` **projette** le snapshot sur
`has_data / object_class / n_genes / n_samples` **et jette `modules`** (`scripts/mcp_server.R:402-405`).
**Aucune fuite aujourd'hui** ; c'est une **contrainte de conception pour M3**, pas un défaut.

### 1.3 `set_inputs` — le contrat réel
**Allowlist gelée** : `TS_DRIVE_ALLOWLIST` = **39 entrées** (`allowlisted_inputs = 39`), réparties sur
`import_bulk` (12), `bulk_de` (9), `bulk_filter` (3), `bulk_pathways` (10) + **5 boutons**.
`kind` ∈ `radio | checkbox | text | numeric | select | button` — `kind` est **la seule chose** dont
l'injecteur dépend (spec S2 : ni `eval`, ni `parse`).

**Adaptateurs** (`ts_drive_adapters`) — c'est **tout** l'arbitrage de valeur :

| `kind` | Appel | Contrôle de valeur |
|---|---|---|
| `select` | `shiny::updateSelectInput(selected = value)` | **AUCUN** |
| `radio` | `updateRadioButtons(selected = value)` | **AUCUN** |
| `checkbox` | `updateCheckboxInput(value = isTRUE(value))` | coercition logique |
| `numeric` | `updateNumericInput(value = as.numeric(value))` | coercition numérique, **AUCUNE BORNE** |
| `text` | `updateTextInput(value = as.character(value))` | coercition texte, **AUCUNE LONGUEUR** |

🔴 **F-A — `set_inputs` ne valide NI type NI bornes NI énumération.** Les `note` de l'allowlist
documentent les valeurs attendues (`"deseq2 | edger | limma"`, `"GOBP | KEGG | Reactome"`…) mais
**rien ne les applique**. Pire : `updateSelectInput(selected = <hors choix>)` **ne lève pas** — 
l'adaptateur rend `TRUE`, donc le statut est **`applied` alors que l'input n'a pas changé** : un
**no-op silencieux rapporté comme un succès**.
**Refus effectivement implémentés** : id inconnu → `refused` + warning ; id allowlisté d'un **autre
module** → `refused` + warning (jamais appliqué) ; `kind` sans adaptateur → `refused`.
**Protection contre la mutation directe** : ✅ il n'existe **aucun** `session$setInputs()` (inexistant
sur une session live) et **aucun** `eval`/`parse`/`source` — uniquement les `update*Input` officiels.
Les boutons ne sont **jamais posés** : ils sont **incrémentés** (`ts_drive_bump_token`), et seulement
si la sonde de disponibilité répond `ready`.

### 1.4 `run_pipeline` — préconditions et transitions
Résolution du bouton : `scn$button` sinon défaut du module
(`import_bulk→import_bulk-btn_load`, `bulk_filter→bulk-filter-run_filter_norm`,
`bulk_de→bulk-de-run_de`, `bulk_pathways→bulk-pathways-run_pathway`), **doit** ∈ `TS_DRIVE_BUTTONS`.
**Préconditions, dans cet ordre** (chacune répond `invalid` avec un message distinct) :
1. **CONTRACT B** — `ts_drive_job_busy()` ⇒ *"another job is already running (module …, seq …)"*.
   **REFUSÉ, pas mis en file** (spec S7).
2. **Sonde de disponibilité** — `not-ready` / `probe-failed` ⇒ refus **AVANT** que le compteur bouge.
3. **Câblage** — bouton non lié ⇒ refus (distinct de « pas prêt »).
4. **CONTRACT A** — si le module a déclaré `long = TRUE` ⇒ `ts_drive_job_begin()` et statut
   **`running`** (non terminal).
5. Sinon ⇒ **`done`**, qui signifie *« le jeton a bougé »*, **jamais** « le pipeline a fini ».

🔑 **Acquittement de protocole ≠ achèvement métier** : `done` est terminal **pour le `seq`**, pas une
preuve de fin. La fin se lit par un `snapshot` **suivant**.

### 1.5 Jobs longs et terminalité
`ts_drive_status_terminal()` = **`done | error | ignored | invalid`**. **`applied` et `running` sont
NON terminaux.**
`ts_drive_job_finish(button, status, error)` : refuse un `button` qui n'est pas le job en vol
(deux onglets ne se ferment pas mutuellement) et refuse un statut non terminal. Seul **le module**
déclare la fin ; le watcher ne devine jamais. Un **échec d'écriture garde le job pending** (retry au
beat suivant).
🔴 **RÉSIDUEL MESURÉ** : le calcul long est **synchrone** ⇒ la boucle d'événements est bloquée, le
battement s'arrête et `ready_fresh()` passe **FALSE** pendant tout le job. **`result.json:running`
EST la preuve de vie.** ⇒ **pour un job long, relire `result.json` en boucle est garanti de
timeouter** ; il faut **un `snapshot` FRAIS par poll**.

### 1.6 Identité, armement, battement (déjà M1/M2)
`ready.json` = `protocol`, `armed`, `pid`, `port`, `root`, `session_token`, `viewer`, `last_seq`,
`started_at`, `hb_at`, `hb_n`, `hb_timeout_s` (défaut **15 s**). `ts_drive_ready_fresh()` exige
protocole identique **et** âge ∈ [-60 s, timeout].
**Validateur de scénario** (`ts_drive_validate_scenario`) : protocole inconnu ⇒ **`ignored`** ;
`seq` manquant/non numérique ⇒ **erreur** ; `seq ≤ last_seq` ⇒ **`ignored`** (périmé) ;
`session_token` divergent ⇒ **erreur** ; action inconnue ⇒ erreur ; module hors `TS_DRIVE_MODULES` ⇒
erreur ; `preserve_data` non logique ⇒ erreur ; `inputs` non-objet ⇒ erreur ; id inconnu ⇒ erreur ;
id d'un autre module ⇒ warning + ignoré.
🔴 **F-J — le `session_token` du scénario est OPTIONNEL** : `if (nzchar(declared_token) && …)` ⇒ un
scénario **sans** `session_token` passe la validation dès que la session est armée. La borne réelle
est donc **`arm.json`**, pas le scénario.
🔴 **F-C — `expect` est à moitié mort** : `expect.nav` est **consommé** (`ts_drive_nav_plan`), mais
`expect.timeout_s` et `expect.outputs` (spec §2.3) **ne sont JAMAIS lus**.

### 1.7 Rails de sécurité (spec S1–S11) — état implémenté
| Rail | État mesuré |
|---|---|
| S1 pas de `stop()` sur le chemin drive | ✅ `tryCatch` partout, tout devient `errors[]`/`warnings[]` |
| S2 pas d'`eval`/`parse`/`source` | ✅ aucun dans `R/core/drive_*.R` ni `scripts/mcp_server.R` |
| S3 allowlist par module | ✅ id inconnu ⇒ erreur ; hors module ⇒ ignoré |
| S4 ids namespacés | ✅ allowlist = **données**, avec `module` écrit explicitement |
| S5 contrat d'injection | ✅ `update*Input` uniquement ; boutons incrémentés ; `nav_select()` |
| S6 le watcher ne lance pas DESeq2/Seurat | ✅ il ne fait que poser des inputs / incrémenter |
| S7 un scénario = un module | ✅ CONTRACT B refuse le second job |
| S8 UTF-8 + rename Windows | ✅ `unlink` puis `rename`, 8 essais, budget 1,0 s |
| S9 pas de secret dans `tools/_drive/` | ✅ `ready.json` porte le jeton **par conception** (fichier local, gitignoré) |
| S10 conventions vertes | ✅ **0 erreur / 59 avert.** (inchangé) |
| S11 chemins d'import bornés | ⚠️ **voir F-D** |

🔴 **F-D — la borne S11 est une comparaison de PRÉFIXE de chaîne** :
`inside <- identical(substr(full, 1L, nchar(r)), r)` (`drive_allowlist.R:431-432`). Un répertoire
**frère** dont le nom **prolonge** une racine (`<root>_evil/…`) est donc **accepté**. `..` **est**
refusé (sur la chaîne brute, avant normalisation) et le chemin doit exister et ne pas être un
répertoire. Impact pratique **limité** (l'agent a déjà accès au FS local) mais c'est un **écart à
l'intention de S11**.
🟡 **F-I — aucune limite de charge utile** : ni sur `scenario.json` (lu entier par `jsonlite`), ni sur
le nombre d'`inputs`. Le serveur MCP borne **1 MiB par ligne** (posé au correctif de transport).
🟡 **F-H — agents concurrents** : `scenario.json` est **un seul fichier**, dernier écrivain gagnant.
`seq ≤ last_seq` ⇒ `ignored` : deux agents qui écrivent en parallèle **s'entrelacent sans arbitrage**.

---

## 2. Frontières proposées M3a / M3b / M3c

| Lot | Contenu | Nature | Justification |
|---|---|---|---|
| **M3a** | `transcripto_drive_snapshot` — lit `result.json.snapshot` et le **projette** | **READ-ONLY** | Aucune écriture, aucune mutation d'input. C'est la **primitive d'observation** dont M3b/M3c ont besoin pour être **vérifiables** (sinon on ne mesure que `done`, le mensonge documenté). |
| **M3b** | `transcripto_drive_set_inputs` — écrit `scenario.json` `action=set_inputs` | **ÉCRITURE CONTRÔLÉE** | Écrit **un seul** fichier, ne clique rien, ne change aucune donnée (`preserve_data` par défaut). |
| **M3c** | `transcripto_drive_run` — écrit `scenario.json` `action=run_pipeline` | **ÉCRITURE CONTRÔLÉE** | Déclenche du **travail réel** ⇒ préconditions, CONTRACT A/B, distinction `done`/`running`. |
| **M4** | `wait` + **snapshot ACTIF** (scénario `snapshot` par poll) + lecture des jobs longs | ÉCRITURE CONTRÔLÉE + boucle | Seul moyen de distinguer `applied/running/done/error/invalid/stale` sur un job synchrone bloquant. |

**Recommandation demandée — `snapshot` en M3 ou M4 ?** ⇒ **LES DEUX, séparés.**
- Le **snapshot PASSIF** (lire `result.json.snapshot`, projeter) appartient à **M3a** : lecture pure,
  coût nul, et **indispensable** pour prouver que M3b/M3c ont réellement agi.
- Le **snapshot ACTIF** (écrire un scénario `snapshot` puis attendre `ack_seq == seq`) appartient à
  **M4** : il introduit une **écriture** et une **attente**, et sa seule raison d'être est le
  **job long** (§1.5), où relire un fichier statique est garanti de timeouter.
- ⚠️ Conséquence : en M3a le snapshot est **périmé par nature** (il ne bouge qu'à la consommation d'un
  scénario). L'outil **doit** le dire (champ `stale_since` / `ack_seq`) sous peine de rejouer le
  mensonge du `done`.

---

## 3. Schémas d'outils proposés (exacts)

**M3a — `transcripto_drive_snapshot`** (read-only)
```json
{ "name": "transcripto_drive_snapshot",
  "inputSchema": { "type": "object", "properties": {}, "additionalProperties": false } }
```
`structuredContent` : `present`, `protocol`, `ack_seq`, `applied_at`, `active_module`,
`has_data`, `object_class`, `n_genes`, `n_samples`, `error_state`,
`modules` = projection **fermée** `{ <module>: { <compteurs scalaires autorisés> } }`
— **jamais** `samples`, **jamais** `active_contrast` brut, **jamais** de chemin.

**M3b — `transcripto_drive_set_inputs`**
```json
{ "name": "transcripto_drive_set_inputs",
  "inputSchema": {
    "type": "object",
    "properties": {
      "module":  { "type": "string",
                   "enum": ["import_bulk","bulk_filter","bulk_de","bulk_pathways"] },
      "inputs":  { "type": "object",
                   "description": "inputId -> valeur. Doit etre sur l'allowlist gelée ET appartenir à `module`." },
      "preserve_data": { "type": "boolean", "default": true },
      "expect":  { "type": "object",
                   "properties": { "nav": { "type": "string" } },
                   "additionalProperties": false }
    },
    "required": ["module", "inputs"],
    "additionalProperties": false } }
```

**M3c — `transcripto_drive_run`**
```json
{ "name": "transcripto_drive_run",
  "inputSchema": {
    "type": "object",
    "properties": {
      "module":  { "type": "string",
                   "enum": ["import_bulk","bulk_filter","bulk_de","bulk_pathways"] },
      "button":  { "type": "string",
                   "enum": ["import_bulk-btn_load","bulk-filter-run_filter_norm",
                            "bulk-de-run_de","bulk-pathways-run_pathway",
                            "bulk-pathways-run_scores"] },
      "inputs":  { "type": "object" },
      "preserve_data": { "type": "boolean", "default": true },
      "expect":  { "type": "object",
                   "properties": { "nav": { "type": "string" } },
                   "additionalProperties": false }
    },
    "required": ["module"],
    "additionalProperties": false } }
```
Tous les trois portent `expect` = **assertion d'identité** `{pid, started_at, session_token}` sur le
modèle **déjà validé par M2** (`ts_drive_set_armed`) — jamais une source.

## 4. Codes d'erreur de domaine proposés

**Existants (inchangés)** : `NO_SESSION` · `STALE_SESSION` · `RESULT_SESSION_MISMATCH` ·
`INVALID_PROTOCOL` · `READ_FAILED` · `AMBIGUOUS_SESSION` · `SESSION_MISMATCH` · `ARM_WRITE_FAILED`.
**Protocole** : `-32700` / `-32600` / `-32601` / `-32602` / `-32603`.

**Nouveaux, alignés sur les messages que l'app écrit déjà dans `errors[]`** :

| Code | Déclencheur (source app) |
|---|---|
| `SCENARIO_WRITE_FAILED` | l'écriture atomique de `scenario.json` n'a pas atterri (8 essais / 1,0 s) |
| `MODULE_NOT_ALLOWED` | module hors `TS_DRIVE_MODULES` |
| `INPUT_NOT_ALLOWED` | id hors `TS_DRIVE_ALLOWLIST` |
| `INPUT_MODULE_MISMATCH` | id allowlisté appartenant à un autre module |
| `INPUT_VALUE_REFUSED` | l'adaptateur a rendu `FALSE` |
| `BUTTON_NOT_BOUND` | le module n'a annoncé aucun jeton pour ce bouton |
| `MODULE_NOT_READY` | sonde `not-ready` / `probe-failed` |
| `JOB_ALREADY_RUNNING` | CONTRACT B |
| `ACTION_NOT_SUPPORTED` | `reset_module` (non implémenté) |
| `SEQ_STALE` | `seq ≤ last_seq` — aujourd'hui `ignored`, **pas** une erreur |
| `RESULT_TIMEOUT` *(M4)* | `ack_seq == seq` jamais atteint / jamais terminal |

## 5. Décisions NON tranchées (à arbitrer avant M3)

1. **Bornes et énumérations** : M3 applique-t-il une validation de **valeur** (F-A), ou reste-t-on sur
   les `update*Input` bruts avec `applied` optimiste ? ⇒ **le point le plus important**.
2. **Snapshot passif vs actif** (§2) — la scission proposée est-elle acceptée ?
3. **`session_token` obligatoire** dans un scénario M3 (F-J) ?
4. **Plafonds de charge utile** : nombre d'`inputs`, longueur des chaînes (F-I) ?
5. **Agents concurrents** : accepter le dernier-écrivain-gagnant, ou introduire un bail/verrou (F-H) ?
6. **`import_file` dans M3 ?** Il porte un **chemin** et écrit `global_data$bulk_obj`.
7. **`reset_module`** : jamais exposé, ou M4+ ?
8. **`expect.timeout_s` / `expect.outputs`** : les implémenter ou les retirer du contrat (F-C) ?
9. **Politique de divulgation** pour la sur-caviardage de `pathways` — **différée** sur instruction.

## 6. Fichiers que M3 modifierait

| Fichier | Action | Nature |
|---|---|---|
| `scripts/mcp_server.R` | **ÉTENDU** (outils M3a/M3b/M3c + projections + codes) | **le SEUL fichier d'implémentation**, exactement comme M1/M2 |
| `tests/testthat/test-mcp-*.R` | **CRÉÉS** (nouveaux fichiers) | additif — **aucun test existant modifié** |

**Intacts** : `R/core/drive_watcher.R`, `R/core/drive_allowlist.R` (le protocole est **réutilisé**),
`app.R`, `modules/` (les **5** boutons sont **déjà** liés), `renv.lock` (aucune dépendance),
`.zcode/config.json` et `~/.workbuddy-ai/mcp.json` (**aucune** modification : `tools/list` est
dynamique), `docs/`, gardes.

## 7. Déclaration

> Audit **lecture seule** : **0** fichier modifié, manifeste **inchangé**
> (`48f758e35f4962ac57074d60590b0c7f78ccef21519e3e0308b5ee49a49115f1`). **Aucun** scénario écrit,
> **aucune** session armée, **aucun** outil M3 implémenté. **M3 et M4 restent NON commencés** et
> exigent une autorisation **nouvelle et explicite**.

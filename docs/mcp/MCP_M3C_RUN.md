# M3c — controlled `run` tool (2026-09-24)

**Authorisation** : **M3c uniquement**. **M4, `wait`, snapshot actif et orchestration de jobs longs :
NON implémentés.** **M4 reste NON autorisé.**

## 1. Porte pré-édition

| Contrôle | Résultat |
|---|---|
| Manifeste recalculé | `522cfab37e127e26b99309949857005c1fb88751433616b721d09f8bba313154` (**437** f.) |
| Exigé | identique |
| Verdict | ✅ **PASS** — aucune dérive |
| Cible avant | `scripts/mcp_server.R` = `a443cbefe5ee7db7…` |

## 2. Spécification (rapportée AVANT implémentation)

### 2.1 Allowlist d'actions de run — **UNE seule**
`TS_MCP_RUN_ACTIONS = c("run_pipeline")`.

`TS_DRIVE_ACTIONS` porte aussi `import_file` (**porte un chemin**), `snapshot` (M3a/M4),
`reset_module` (**non implémenté**) et `set_inputs` (M3b) : **aucune n'est une action de run**, et
**aucune n'est atteignable** depuis cet outil (`redaction.not_reachable`). Un garde défensif refuse
d'écrire si cette allowlist venait à ne plus valoir exactement `run_pipeline`.

### 2.2 Boutons de run par module (fermé)
| Module | Bouton(s) |
|---|---|
| `import_bulk` | `import_bulk-btn_load` |
| `bulk_filter` | `bulk-filter-run_filter_norm` |
| `bulk_de` | `bulk-de-run_de` |
| `bulk_pathways` | `bulk-pathways-run_pathway` **ou** `bulk-pathways-run_scores` |

`.ts_mcp_run_problems()` **croise cette table avec celle de l'app** (`TS_DRIVE_BUTTONS` +
`ts_drive_button_module()`) : chaque bouton doit exister **et** appartenir à son module.
`--check` l'exécute et **échoue** en cas de dérive — mesuré : **OK (every button belongs to its
module in the app's own table)**, `run_pipeline over 4 modules / 5 buttons`.

### 2.3 Schéma d'entrée exact
```json
{ "type": "object",
  "properties": {
    "seq": { "type": "integer", "minimum": 1 },
    "module": { "type": "string", "enum": ["import_bulk","bulk_filter","bulk_de","bulk_pathways"] },
    "button": { "type": "string",
                "enum": ["import_bulk-btn_load","bulk-filter-run_filter_norm","bulk-de-run_de",
                         "bulk-pathways-run_pathway","bulk-pathways-run_scores"] },
    "preserve_data": { "type": "boolean" },
    "expect": { "type": "object",
                "properties": { "session_id": {"type":"string"},
                                "pid": {"type":"integer"},
                                "started_at": {"type":"string"} },
                "required": ["session_id"],
                "additionalProperties": false } },
  "required": ["seq", "module", "expect"],
  "additionalProperties": false }
```
`button` est **optionnel** uniquement quand le module n'expose **qu'une** action ; sinon l'omettre
donne **`ACTION_AMBIGUOUS`** — M3c ne choisit **jamais** un bouton à la place de l'appelant.

### 2.4 Préconditions (ce que le serveur PEUT voir)
`session présente` · `protocole` · `identité complète` · `assertion `session_id` non-joker` ·
`battement frais` · **`armée`** · **`job non occupé`** · **`seq monotone`** ·
**`module/action compatibles`**. Toutes re-vérifiées **immédiatement avant l'écriture** (identité,
armement, `last_seq`, battement, job).

**Délégué à l'app, et NOMMÉ dans la réponse** (`preconditions.delegated_to_app`) : le garde de
**disponibilité du module** (« not ready: no bulk object loaded »), le fait que le bouton soit
**lié**, et le **cycle de vie** du job. Les reproduire ici produirait exactement le mensonge `done`
que le contrat des jobs longs existe pour empêcher ; ils arrivent dans le **verdict**.

🔑 **Contract B lu SUR LE FIL.** L'app applique « un seul job à la fois » contre son état **en
process**, illisible depuis le serveur. Mais elle **PUBLIE** cet état : un job long écrit
`status = "running"` dans `result.json` **au dispatch**. Donc **un verdict `running` = session
occupée** ⇒ `JOB_ALREADY_RUNNING`. ⚠️ Limite documentée : un job qui n'a jamais publié `running`
est **invisible** pour nous — l'app le refuse quand même, et ce refus arrive en `invalid`.

### 2.5 Cartographie d'état
| État | Qui le produit | Sens |
|---|---|---|
| **`accepted`** | **ce serveur** | le scénario est **écrit** ; l'app ne l'a pas encore consommé |
| `applied` | app | inputs posés, pipeline **non** fini |
| `running` | app | job démarré, **non** fini |
| `done` | app | terminal **pour le `seq`** — **pas** une preuve de fin d'analyse |
| `error` / `invalid` | app | terminal, avec une raison |

La réponse porte `state = "accepted"` et **`business_state.completion_verified = false`**.
`protocol_acknowledgement.ack_seq = NULL` (le serveur **n'attend pas**).

### 2.6 Codes d'erreur
**Nouveaux** : `ACTION_NOT_ALLOWED` · `ACTION_AMBIGUOUS` · `BUTTON_MODULE_MISMATCH` ·
`JOB_ALREADY_RUNNING`.
**Réutilisés** : `NO_SESSION` · `STALE_SESSION` · `INVALID_PROTOCOL` · `READ_FAILED` ·
`AMBIGUOUS_SESSION` · `SESSION_MISMATCH` · `SESSION_ASSERTION_REQUIRED` · `SESSION_NOT_ARMED` ·
`MODULE_NOT_ALLOWED` · `PAYLOAD_REFUSED` · `PAYLOAD_TOO_LARGE` · `SEQ_STALE` ·
`SCENARIO_WRITE_FAILED`. **Protocole** : `-32602`.

### 2.7 Fichiers à modifier
**`scripts/mcp_server.R` uniquement.**

## 3. Vérification — 17 cas, tous conformes (`results_m3c.txt`)

| Cas | Réponse |
|---|---|
| action autorisée, bouton explicite (`bulk_de`) · bouton unique implicite (`bulk_filter`) · 2ᵉ bouton pathways (`run_scores`) | ✅ **OK `state=accepted`**, bouton correct, **`completion=False`** |
| action inconnue | ✅ `ACTION_NOT_ALLOWED` |
| module/action incohérents | ✅ `BUTTON_MODULE_MISMATCH` |
| **action ambiguë** (pathways, aucune nommée) | ✅ `ACTION_AMBIGUOUS` |
| module inconnu | ✅ `MODULE_NOT_ALLOWED` |
| **préconditions** : non armée · périmée · aucune session | ✅ `SESSION_NOT_ARMED` · `STALE_SESSION` · `NO_SESSION` |
| **session occupée** (verdict `running`) | ✅ `JOB_ALREADY_RUNNING` |
| seq rejoué (`== last_seq`) | ✅ `SEQ_STALE` |
| PID/session incohérents · `session_id` faux | ✅ `SESSION_MISMATCH` ×2 |
| `expect` absent · sans `session_id` | ✅ `-32602` · `SESSION_ASSERTION_REQUIRED` |
| **échec d'écriture** (`scenario.json` remplacé par un **répertoire**) | ✅ `SCENARIO_WRITE_FAILED` |
| pureté stdout · fuite (jeton / chemin) | ✅ **sur les 17** |

**Sur disque (fixture)** : `{protocol: ts-drive/1, seq: 50, action: run_pipeline, module: bulk_de,
button: bulk-de-run_de, preserve_data: true, session_token: …}`.
**`tools/_drive/` du dépôt = `README.md` seul, aucun `scenario.json`.**

### 3.1 SDK officiel `@modelcontextprotocol/sdk` 1.30.1
`connect_ok` ✅ · **6 outils** ✅ · `0.5.0-m3c` · `run` **atteignable et refusé** `ACTION_NOT_ALLOWED`
(**sans écriture**) · `set_inputs` refusé · `ping {}` · **`LEAKS: []`** · fermeture propre.

### 3.2 Régressions
Vecteurs **WorkBuddy** / **ZCode** (configs exactes) + rejeu de forme + `bare` : **6/6 outils**,
pureté, erreurs structurées, récupération, `ping {}` ✅ ; **contrôle négatif** → `framing_pure=False` ✅.
**M3a** : les 7 cas **inchangés** ✅. **M3b** : les 22 cas **tous répondus** ✅.
Les **cinq** outils antérieurs sont inchangés (aucun champ retiré).

## 4. Avant / après

| | Valeur |
|---|---|
| Manifeste **avant** | `522cfab37e127e26b99309949857005c1fb88751433616b721d09f8bba313154` (437 f.) |
| Manifeste **après** | `aa7c8aa57251adc26f453716fbb0b82fa788989a949a6d47aa3c34ee975e30cb` (437 f.) |
| **Delta exact** | `scripts/mcp_server.R` **seul** |

Gardes **0 erreur / 59 avert.** (inchangé) · `git status` = ` M renv.lock` · `HEAD` = `9cb1a06` ·
configs clients **inchangées** (`.zcode/config.json` `4746a850…`, `~/.workbuddy-ai/mcp.json`
`0febf769…`) · **aucune écriture drive réelle**.

## 5. Inventaire des outils — **six**

| Outil | Nature |
|---|---|
| `transcripto_drive_status` | read-only |
| `transcripto_drive_read_result` | read-only |
| `transcripto_drive_snapshot` | read-only (passif, M3a) |
| `transcripto_drive_set_inputs` | écriture contrôlée (M3b) |
| **`transcripto_drive_run`** | **écriture contrôlée (M3c) — `run_pipeline` uniquement** |
| `transcripto_drive_set_armed` | arm/disarm (M2) |

**Aucun `wait`. Aucun snapshot actif. Aucune orchestration de jobs longs.**

## 6. Déclaration

> **Un seul fichier modifié** : `scripts/mcp_server.R`. `app.R`, `modules/`, `R/core/drive_*.R`,
> `tests/`, `renv.lock`, `docs/`, les configs clients et les gardes sont **intacts**. Aucun appel
> direct à Shiny, Seurat, DESeq2, une fonction de pathways, `eval`, `parse` ou `source` : l'outil
> **écrit un fichier** et s'arrête ; c'est l'observateur existant de l'app qui agit.
> **M4 reste NON commencé et NON autorisé.**

# M3b — controlled `set_inputs` tool (2026-09-24)

**Authorisation** : **M3b uniquement**, exécuté **séquentiellement** avant M3c.
**`run`, snapshot actif, `wait`, orchestration de jobs et M4 : NON implémentés.**
**M3c reste NON commencé** et exige une **seconde confirmation explicite**.

## 1. Porte pré-édition

| Contrôle | Résultat |
|---|---|
| Manifeste recalculé | `3c881275a38ca039a2e7b4c2c593acc2b3cb43bcfb42ff530f73141afd34d5f0` (**437** f.) |
| Exigé | identique |
| Verdict | ✅ **PASS** — aucune dérive |
| Cible avant | `scripts/mcp_server.R` = `0653261643a36d7b…` |

## 2. Spécification (rapportée AVANT implémentation)

### 2.1 Nom exact de l'outil
`transcripto_drive_set_inputs`

### 2.2 Schéma d'entrée exact
```json
{ "type": "object",
  "properties": {
    "seq": { "type": "integer", "minimum": 1 },
    "module": { "type": "string",
                "enum": ["import_bulk","bulk_filter","bulk_de","bulk_pathways"] },
    "inputs": { "type": "object", "description": "inputId -> valeur" },
    "preserve_data": { "type": "boolean" },
    "expect": { "type": "object",
                "properties": { "session_id": {"type":"string"},
                                "pid": {"type":"integer"},
                                "started_at": {"type":"string"} },
                "required": ["session_id"],
                "additionalProperties": false } },
  "required": ["seq", "module", "inputs", "expect"],
  "additionalProperties": false }
```

🔴 **Correction de conception trouvée PENDANT la vérification, et pourquoi elle compte.**
La première version exigeait `expect.session_token` — mais **aucun outil ne publie jamais le jeton**
(il est `<redacted>` partout). L'assertion requise était donc **insatisfaisable par tout client MCP**,
et le test SDK l'a montré (il ne pouvait produire que `SESSION_MISMATCH`). L'assertion porte
désormais sur **`expect.session_id`**, l'identifiant **DÉRIVÉ** :
- **non-wildcard par construction** (16 hex ; `"*"` est explicitement refusé) ;
- **obtenable sans divulguer de secret** — `transcripto_drive_status` **publie maintenant
  `session.session_id`** (champ **additif**) ;
- il ne permet ni d'usurper une session ni de la retrouver.

### 2.3 Allowlist exposée — **29 entrées** (sur 34 non-boutons)
Le schéma de valeur est une **seconde table** (`TS_MCP_INPUT_SCHEMA`), donc **vérifiée contre
l'allowlist de l'app** : `.ts_mcp_schema_problems()` exige que chaque id existe dans
`TS_DRIVE_ALLOWLIST` **et** que son `type` corresponde au `kind` du widget. `--check` l'exécute et
**échoue** en cas de dérive. Mesuré : **OK (every type matches the app's own widget kind)**.

| Module | Exposés |
|---|---|
| `import_bulk` | 12 (dont 2 `enum`, 5 `boolean`, 3 `text`, 2 `number`) |
| `bulk_de` | 5 (1 `enum`, 1 `boolean`, 3 `number`) |
| `bulk_filter` | 3 (`number`) |
| `bulk_pathways` | 9 (5 `enum`, 4 `number`) |

🔴 **5 entrées délibérément NON exposées** (`redaction.not_exposed`) : `bulk-de-condition_col`,
`bulk-de-covariates`, `bulk-de-group_ref`, `bulk-de-group_target` (domaine = **métadonnées de la
session vivante**) et `bulk-pathways-scores_source` (`bulk_gene_set_choices()`). Leur enum n'est
**pas énumérable statiquement** : ce serveur ne peut donc pas honorer « rejeter une valeur non
supportée » pour elles. **Fail closed** — exposer un input dont on ne peut pas vérifier le domaine
rouvrirait exactement le trou (F-A) que M3b ferme.

### 2.4 Validateurs de valeur (avant toute écriture)
| `type` | Règle |
|---|---|
| `boolean` | `is.logical`, longueur 1, non `NA` |
| `number` | coercible, **fini**, entier si `integer = TRUE`, dans `[min, max]` |
| `text` | `is.character`, `nchar <= max_chars`, **aucun saut de ligne** |
| `enum` | `is.character` **et** `v %in% values` |

Les bornes (`min`/`max`) sont une **politique serveur généreuse**, pas un contrat de l'app : elles
rejettent l'absurde sans contredire un réglage d'analyse légitime.

### 2.5 Codes d'erreur
**Nouveaux** : `SESSION_ASSERTION_REQUIRED` · `SESSION_NOT_ARMED` · `MODULE_NOT_ALLOWED` ·
`INPUT_NOT_ALLOWED` · `INPUT_MODULE_MISMATCH` · `VALUE_REFUSED` · `PAYLOAD_REFUSED` ·
`PAYLOAD_TOO_LARGE` · `SEQ_STALE` · `SCENARIO_WRITE_FAILED`.
**Réutilisés** : `NO_SESSION` · `STALE_SESSION` · `INVALID_PROTOCOL` · `READ_FAILED` ·
`AMBIGUOUS_SESSION` · `SESSION_MISMATCH`.
**Protocole** : `-32602` pour une requête malformée (`expect` absent, `seq` non entier, …).

### 2.6 Fichiers à modifier
**`scripts/mcp_server.R` uniquement.** Aucun test modifié ni créé (vérification externe, comme
M1/M2/M3a).

## 3. Implémentation (plan exécuté)

1. **`.ts_tool_err(..., detail = NULL)`** — détail **optionnel et additif** : les enveloppes M1/M2/M3a
   sont **inchangées**.
2. **`.ts_session_assert(expect, require_assertion)`** — les **cinq** contrôles d'identité
   (présence, protocole, identité complète, non-joker, battement) **extraits** de l'outil M2, pour que
   les deux outils d'écriture **ne puissent pas diverger**. `.ts_tool_set_armed()` les consomme
   désormais (comportement identique, revérifié).
3. **`TS_MCP_INPUT_SCHEMA`** + `.ts_mcp_schema_problems()` + `.ts_mcp_not_exposed()` +
   `.ts_input_value_ok()`.
4. **`.ts_tool_set_inputs()`** — valide, puis écrit via **`ts_drive_write_json()`** (le writer
   atomique existant) dans `scenario.json`, puis **s'arrête**.
5. Enregistrement (5ᵉ outil), branche de dispatch, codes, `serverInfo.version` → **`0.4.0-m3b`**.
6. `--check` **restructuré en deux phases** : la phase 2 (schéma + lecteurs drive) tourne **à la fin**,
   car R définit de haut en bas — un appel depuis la section 1 levait « could not find function ».

**Garde-fous d'écriture** : assertion obligatoire **non-wildcard** · session **armée** exigée (sinon
l'app ne consommerait jamais le scénario ⇒ refus au lieu d'un no-op silencieux) · **`seq > last_seq`**,
puis **re-lu immédiatement avant l'écriture** (course entre deux écrivains) · **re-vérification de
l'identité et du battement** juste avant l'écriture · plafonds **24 inputs** et **4096 octets**
sérialisés.

## 4. Vérification

### 4.1 Matrice complète (22 cas) — `results_m3b.txt`
| Cas | Réponse |
|---|---|
| écriture valide (`bulk_filter`), `enum` (`bulk_pathways`), `boolean` (`bulk_de`) | ✅ **OK**, `accepted=true`, `seq` correct |
| module inconnu | ✅ `MODULE_NOT_ALLOWED` |
| input inconnu | ✅ `INPUT_NOT_ALLOWED` |
| **allowlisté mais non exposé** | ✅ `INPUT_NOT_ALLOWED` |
| module/input incohérents | ✅ `INPUT_MODULE_MISMATCH` |
| violation d'`enum` · de bornes (`padj=5`) · de type (`"abc"` pour un nombre) · non-entier (`2.5`) | ✅ `VALUE_REFUSED` ×4 |
| `seq == last_seq` (rejoué) | ✅ `SEQ_STALE` |
| `expect` absent | ✅ `-32602` (protocole) |
| `expect` sans `session_id` | ✅ `SESSION_ASSERTION_REQUIRED` |
| assertion joker `"*"` | ✅ `AMBIGUOUS_SESSION` |
| `session_id` faux · `pid` faux | ✅ `SESSION_MISMATCH` ×2 |
| session **non armée** | ✅ `SESSION_NOT_ARMED` |
| session périmée · aucune session | ✅ `STALE_SESSION` · `NO_SESSION` |
| 25 entrées | ✅ `PAYLOAD_TOO_LARGE` |
| **valeur sentinelle** | ✅ **jamais réémise** (`value_leaked=False`) |
| pureté stdout | ✅ sur **les 22 cas** |

**Sur disque (fixture)** : `scenario.json` = `{protocol: ts-drive/1, seq: 40, action: set_inputs,
module: bulk_filter, inputs: {bulk-filter-min_count: 7}, preserve_data: true, session_token: …}`.
**`tools/_drive/` du dépôt = `README.md` seul, aucun `scenario.json`.**

### 4.2 SDK officiel `@modelcontextprotocol/sdk` 1.30.1
`connect_ok` ✅ · **5 outils** ✅ · `0.4.0-m3b` · `status` publie `session_id`
(`125502c731259f00`) · `set_inputs` **atteignable et refusé** `MODULE_NOT_ALLOWED` (sans écriture) ·
`SESSION_MISMATCH` sur un pin faux · `ping {}` · **`LEAKS: []`** · fermeture propre.

### 4.3 Régressions
Vecteurs **WorkBuddy** et **ZCode** (configs exactes, racine réelle) + rejeu de forme + `bare` :
5/5 outils, pureté, erreurs structurées, récupération, `ping {}` ✅ ; **contrôle négatif**
(`--no-init-file` omis) → `framing_pure=False` ✅.
M3a revérifié : les **7** cas (`NO_SESSION`, `STALE_SESSION`, `INVALID_PROTOCOL`,
`RESULT_SESSION_MISMATCH`, `running→stale`, pas de verdict, sain) **inchangés** ✅.
`transcripto_drive_status` : seul ajout = `session.session_id` (**additif**).

## 5. Avant / après

| | Valeur |
|---|---|
| Manifeste **avant** | `3c881275a38ca039a2e7b4c2c593acc2b3cb43bcfb42ff530f73141afd34d5f0` (437 f.) |
| Manifeste **après** | `522cfab37e127e26b99309949857005c1fb88751433616b721d09f8bba313154` (437 f.) |
| **Delta exact** | `scripts/mcp_server.R` **seul** |

Gardes **0 erreur / 59 avert.** (inchangé) · `git status` = ` M renv.lock` · `HEAD` = `9cb1a06` ·
configs clients **inchangées** · **aucune écriture drive réelle**.

## 6. Inventaire

**Cinq outils** : `transcripto_drive_status` · `transcripto_drive_read_result` ·
`transcripto_drive_snapshot` (read-only) · **`transcripto_drive_set_inputs`** (écriture contrôlée) ·
`transcripto_drive_set_armed` (arm/disarm). **Aucun `run`, aucun `wait`.**

## 7. Déclaration

> **Un seul fichier modifié** : `scripts/mcp_server.R`. `run`, le snapshot actif, `wait` et
> l'orchestration de jobs **ne sont pas implémentés**. `app.R`, `modules/`, `R/core/drive_*.R`,
> `tests/`, `renv.lock`, `docs/`, les configs clients et les gardes sont **intacts**.
> **M3c et M4 restent NON commencés** ; M3c exige une **seconde confirmation explicite**.

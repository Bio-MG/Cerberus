# M3a — passive read-only snapshot tool (2026-09-24)

**Authorisation** : **M3a uniquement**. `set_inputs`, `run`, le snapshot **actif**, `wait` et toute
capacité M4 **ne sont pas implémentés**. **M3b, M3c et M4 restent NON commencés.**

## 1. Porte pré-édition

| Contrôle | Résultat |
|---|---|
| Manifeste recalculé | `48f758e35f4962ac57074d60590b0c7f78ccef21519e3e0308b5ee49a49115f1` (**437** f.) |
| Exigé | identique |
| Verdict | ✅ **PASS** — aucune dérive |
| Cible avant | `scripts/mcp_server.R` = `14a6620e50695073…` |

## 2. Plan d'implémentation (tel qu'exécuté)

**Un seul fichier modifié : `scripts/mcp_server.R`.** Ajouts :

1. `.ts_snapshot_module_allow` — **allowlist fermée** des scalaires publiables.
2. `.ts_snapshot_slot_allow` — **allowlist fermée** des slots récursables (`filtered_counts`, `vst_mat`).
   Sans elle, `bulk_filter` (dont l'état est `slot -> list(...)`) se projetait **à rien** — mesuré
   pendant la mise au point, puis corrigé.
3. Bornes : `.ts_snapshot_max_modules/fields/depth/string/bytes`.
4. `.ts_snapshot_scalar()` — un scalaire publiable : longueur 1, jamais `NA`, jamais non-fini.
5. `.ts_snapshot_project_module()` — récursion bornée, **liste blanche** à chaque niveau.
6. `.ts_project_snapshot()` — projection de l'objet + des modules + **plafond d'octets dur**.
7. `.ts_redaction_note()` — publie la politique, le compte des champs jetés et les limites.
8. `.ts_session_id()` — identifiant **dérivé** (corrélation, pas cryptographie).
9. `.ts_tool_snapshot()` — l'outil.
10. Enregistrement dans `.ts_tools()` (4ᵉ outil), branche dans `.ts_dispatch()`,
    `serverInfo.version` → **`0.3.0-m3a`**, `instructions` mis à jour.

**Le payload brut n'est jamais réutilisé** : la projection est construite **champ par champ**.

## 3. Spécification (les éléments demandés AVANT implémentation)

### 3.1 Nom exact de l'outil
`transcripto_drive_snapshot`

### 3.2 Schéma d'entrée exact
```json
{ "type": "object",
  "properties": {
    "expect": { "type": "object",
                "properties": { "session_id": { "type": "string" } },
                "additionalProperties": false } },
  "additionalProperties": false }
```
`expect.session_id` est une **assertion**, jamais une source : si le `session_id` fourni diffère du
`session_id` dérivé de la session vivante ⇒ **`SESSION_MISMATCH`**. Un champ `expect` inconnu ⇒
`-32602`.

### 3.3 Schéma de sortie exact (`structuredContent`)
```
present        bool
protocol       "ts-drive/1"
session        { session_id, viewer, armed, identity_ok, fresh,
                 heartbeat_age_s, heartbeat_timeout_s, last_seq }
observation    { ack_seq, age_s, stale, non_terminal, stale_reason }
protocol_state { status, terminal, acknowledged }
business_state { active_module, completion_verified, note }
object         { has_data, object_class, n_genes, n_samples, error_state }
modules        { <module>: { <scalaire> | <slot>: { <scalaire> } } }
redaction      { policy, dropped_field_count, truncated, limits, never_published }
```
`protocol_state` et `business_state` sont **deux objets séparés** : un acquittement de protocole n'est
pas un résultat métier. `business_state.completion_verified` est **toujours `false`** — M3a est une
lecture **passive** et ne peut pas vérifier qu'une analyse a fini.

### 3.4 Règles de caviardage
- **Liste blanche fermée, appliquée à chaque niveau.** Un champ non nommé est **jeté et compté**.
  Scalaires autorisés : `n_genes, n_samples, n_contrasts, n_padj_finite, n_significant, n_results,
  has_data, bypass, ready, elapsed_s, seq, status, state, module, action, convention, probe_error`.
  Slots récursables : `filtered_counts, vst_mat` (profondeur 2 max).
- **Jamais publiés** : noms d'échantillons, noms de gènes, matrices, charge utile biologique, chemins,
  jeton de session, secrets, journaux bruts, `pid`, `started_at`, `active_contrast`.
- **Identité** : seulement un `session_id` **dérivé** (16 hex, deux accumulateurs 31 bits bornés) +
  `viewer`. Aucun `pid`, aucun `started_at`, aucun jeton.
- **Chaînes** : passées par `.ts_clean()` (caviardage des jetons/chemins) puis tronquées à **120** car.
- **Modules** : seuls ceux de `TS_DRIVE_MODULES` (ensemble fermé) sont publiés.

### 3.5 Limites de taille
| Limite | Valeur |
|---|---|
| modules publiés | **4** |
| champs par niveau | **12** |
| profondeur de récursion | **2** |
| longueur de chaîne | **120** car. |
| projection sérialisée | **8192** octets — au-delà, le détail des modules est **retiré** et `truncated: true` |

### 3.6 Politique de session périmée
- Porte `.ts_session()` (réutilisée de M1) : `NO_SESSION`, `STALE_SESSION`, `INVALID_PROTOCOL`.
- `result.json` sans `applied_at` exploitable ⇒ `READ_FAILED`.
- Verdict **antérieur** à la session ⇒ `RESULT_SESSION_MISMATCH`.
- `observation.stale = true` si **(a)** le statut n'est **pas terminal** (`applied`/`running`), ou
  **(b)** l'âge du verdict dépasse le **timeout de battement** (15 s). Deux raisons indépendantes,
  dérivées des **fichiers seuls**.
- ⚠️ **N'utilise PAS `ts_drive_job_busy()`** : le job vit dans le processus de l'**app**, donc depuis
  le serveur MCP cet appel est un **`FALSE` constant** — le même mensonge silencieux que
  `ts_drive_arm_state()`.
- Verdict absent ⇒ `present: false`, `stale: true`, **pas** une erreur.

### 3.7 Fichiers à modifier
**`scripts/mcp_server.R` uniquement.** Aucun test modifié ni créé (la vérification est externe, comme
pour M1/M2 — `scripts/` n'est pas scanné par C3/C9, et l'énoncé n'autorisait un test que s'il était
« strictement requis et listé d'abord » ; il ne l'est pas).

## 4. Vérifications après implémentation

| Vérification | Résultat |
|---|---|
| **Aucune session** (racine réelle) | ✅ `NO_SESSION` |
| **Session périmée** (battement 600 s) | ✅ `STALE_SESSION` |
| **Protocole invalide** (`ts-drive/0`) | ✅ `INVALID_PROTOCOL` |
| **Verdict résiduel** (`applied_at` antérieur) | ✅ `RESULT_SESSION_MISMATCH` |
| **Verdict non terminal** (`running`) | ✅ `OK`, `stale=true`, `non_terminal=true` |
| **Aucun verdict encore** | ✅ `present=false`, `stale=true` |
| **Session de fixture valide** | ✅ `OK`, `status=done`, `stale=false` |
| **Pin erroné** (`expect.session_id` faux) | ✅ `SESSION_MISMATCH` |
| **Caviardage** — 15 sentinelles plantées (`AUDITSAMPLE1/2`, `AUDITCONTRAST`, `AUDITGENE1/2`, `AUDITPATH`, `AUDITLEAK`, `AUDITERRSTATE`, jeton `AUDITSECRET42`, `secret_data.h5ad`, chemin de fixture, clés `"samples":`, `active_contrast`, `gene_names`, `raw_path`) | ✅ **`LEAKS: []`** — aucune n'apparaît dans les réponses d'outil |
| **Sortie bornée** | ✅ 415 octets pour la projection de test ; plafond 8192 **appliqué et publié** |
| **Pureté stdout** | ✅ `framing_pure=True` sur **toutes** les exécutions |
| **SDK officiel `@modelcontextprotocol/sdk` 1.30.1** | ✅ `connect_ok`, `initialize`, `tools/list` = **4**, `tools/call` ×3, `ping {}`, `close()` propre |
| **Régression des 3 outils existants** | ✅ `status` et `read_result` inchangés ; `set_armed` **visible, jamais invoqué** ; `ping {}` ; erreurs `-32601/-32602/-32700` ; **récupération** après ligne malformée ; ligne CRLF acceptée |
| **Vecteurs de lancement WorkBuddy + ZCode** | ✅ 4/4 outils, pureté, récupération, aucune fuite ; **contrôle négatif** (`--no-init-file` omis) → `framing_pure=False` ✅ |
| **Aucune écriture drive réelle** | ✅ `tools/_drive/` = `README.md` **seul** ; toute la fixture vit **hors dépôt** |
| **Delta de manifeste** | ✅ **1 fichier** — `scripts/mcp_server.R` |

## 5. Avant / après

| | Valeur |
|---|---|
| Manifeste **avant** | `48f758e35f4962ac57074d60590b0c7f78ccef21519e3e0308b5ee49a49115f1` (437 f.) |
| Manifeste **après** | `3c881275a38ca039a2e7b4c2c593acc2b3cb43bcfb42ff530f73141afd34d5f0` (437 f.) |
| **Delta exact** | `scripts/mcp_server.R` **seul** (`14a6620e…` → `b16cd296…`) |

Configs clients **inchangées** (`.zcode/config.json` `4746a850…`, `~/.workbuddy-ai/mcp.json`
`0febf769…`) · gardes **0 erreur / 59 avert.** (inchangé) · `git status` = ` M renv.lock` ·
`HEAD` = `9cb1a06`.

## 6. Inventaire des outils

**Quatre outils** : `transcripto_drive_status` (read-only) · `transcripto_drive_read_result`
(read-only) · **`transcripto_drive_snapshot` (read-only, PASSIF)** ·
`transcripto_drive_set_armed` (arm/disarm). Aucun `set_inputs`, `run`, `wait`.

## 7. Déclaration

> **Un seul fichier modifié** : `scripts/mcp_server.R`. `set_inputs`/`run`, le snapshot actif, `wait`
> et l'orchestration de jobs ne sont **pas** implémentés. `app.R`, `modules/`, `R/core/drive_*.R`,
> `tests/`, `renv.lock`, `docs/`, les configs clients et les gardes sont **intacts**.
> **M3b, M3c et M4 restent NON commencés** et exigent une autorisation **nouvelle et explicite**.

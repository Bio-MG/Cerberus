# M4 — long-job observation (`transcripto_drive_wait`) — REPORT (2026-09-24)

**Statut** : **M4 COMPLET.** Plan accepté (un seul outil ; aucun snapshot frais pendant un job en
cours), puis implémenté, vérifié et rapporté. Aucun travail de propagation de module n'a été entamé.

## 1. Porte pré-édition

| Contrôle | Résultat |
|---|---|
| Manifeste recalculé | `aa7c8aa57251adc26f453716fbb0b82fa788989a949a6d47aa3c34ee975e30cb` (**437** f.) |
| Exigé | identique |
| Verdict | ✅ **PASS** — aucune dérive |
| Cible avant | `scripts/mcp_server.R` = `4d92ee4861bce72b…`, **6** outils |
| Plan persisté | `.workbuddy-ai/audits/MCP_M4_PLAN.md` — **hors du manifeste gelé** (`.workbuddy-ai/` est exclu), donc **aucun** effet sur l'empreinte |

## 2. Schéma final

```json
{ "type": "object",
  "properties": {
    "seq":       { "type": "integer", "minimum": 1 },
    "module":    { "type": "string", "enum": ["import_bulk","bulk_filter","bulk_de","bulk_pathways"] },
    "timeout_s": { "type": "integer", "minimum": 1, "maximum": 600 },
    "poll_ms":   { "type": "integer", "minimum": 200, "maximum": 5000 },
    "observe":   { "type": "boolean" },
    "expect":    { "type": "object",
                   "properties": { "session_id": {"type":"string"},
                                   "pid": {"type":"integer"},
                                   "started_at": {"type":"string"} },
                   "required": ["session_id"], "additionalProperties": false } },
  "required": ["seq", "module", "expect"],
  "additionalProperties": false }
```
Défauts : `timeout_s = 120` (plafond dur **600**), `poll_ms = 500` (min 200, max 5000),
`observe = false`. Hors bornes ⇒ `-32602`.

## 3. Sémantique de timeout et de polling
- Boucle **bornée par horloge murale** ; le serveur dort entre deux polls (il ne répond pas à
  d'autres requêtes pendant ce temps — documenté).
- **Détection de changement** : empreinte `ack_seq|status|applied_at`. Un verdict **inchangé n'est
  jamais rapporté comme progression** ; après **3** polls inchangés l'intervalle **recule**
  (doublement, plafonné à `4 × poll_ms`). Mesuré : `unchanged_polls=6`, `polls=7`, `stalled=true`.
- 🔴 **`timeout` signifie UNIQUEMENT que cet appel est terminé** — jamais que le job a échoué ou
  fini. Il est rapporté comme un **résultat normal** (`isError:false`) portant la dernière
  observation.

## 4. Table des états terminaux (implémentée)

| Observation | `state` | Terminal |
|---|---|---|
| pas de verdict, ou `ack_seq < seq` | `accepted` | non |
| `ack_seq == seq`, `applied` | `applied` | **non** |
| `ack_seq == seq`, `running` | `running` | **non** |
| `ack_seq == seq`, `done` / `error` / `invalid` / `ignored` | `done` / `error` / `invalid` / `ignored` | **oui** |
| statut **inconnu** | traité comme **non terminal** (on attend) | non |
| `timeout_s` écoulé | `timeout` | terminal **pour l'appel** |
| session disparue / jeton changé / (battement périmé **ET** pid mort **ET** ≠ `running`) | `session_lost` | oui |
| verdict non terminal **ou** plus vieux que le timeout de battement | **`observation.stale`** (drapeau orthogonal) | — |

`state_enum` publie les **10** valeurs et `state_semantics` les explique une par une, en indiquant
que `stale` est un **drapeau** et que `timeout` est un **résultat d'appel**.

## 5. Contrat d'erreur
**Nouveaux** : `SESSION_LOST` · `OBSERVE_FAILED` · `OBSERVE_SKIPPED_NO_TERMINAL`.
**Réutilisés** : `NO_SESSION` · `INVALID_PROTOCOL` · `READ_FAILED` · `SESSION_MISMATCH` ·
`SESSION_ASSERTION_REQUIRED` · `SESSION_NOT_ARMED` · `MODULE_NOT_ALLOWED` · `SEQ_STALE` ·
`SCENARIO_WRITE_FAILED`. **Protocole** : `-32602`.
🔴 **`STALE_SESSION` n'est PAS utilisé par `wait`** : pendant un job long le battement **doit**
s'arrêter (résidu mesuré). Un battement périmé pendant `running` est rapporté
(`heartbeat_stalled_by_job: true`) et **n'interrompt jamais** l'attente.

## 6. Fichiers modifiés
**`scripts/mcp_server.R` seul.** `app.R`, `modules/`, `R/core/drive_watcher.R`,
`R/core/drive_allowlist.R`, `tests/`, `renv.lock`, `docs/`, les configs clients et le protocole
drive : **intacts**.

## 7. Snapshots frais — garanties, et la limite confirmée
`observe=true` écrit **UN** scénario `snapshot` (`seq = max(last_seq, run_seq) + 1`) via le writer
atomique, attend son ack, et rend la **projection à liste blanche M3a**.
- **Fraîcheur prouvée** : `business_state.observed_after_terminal = true` **et**
  `observe_seq = 8 > last_seq = 7` (mesuré) ⇒ l'observation est bien postérieure.
- **Sécurité** : écrit **après** le terminal ⇒ ne peut pas écraser le terminal rendu ;
  `last_seq` **relu immédiatement avant** l'écriture.
- 🔴 **Limite (b) respectée** : `observe=true` **n'écrit rien** tant que l'état n'est pas terminal ⇒
  mesuré **`OBSERVE_SKIPPED_NO_TERMINAL`**, `scenario.json` **absent**.
- **Ack manquant** ⇒ **`OBSERVE_FAILED`**, `completion_verified=false` (mesuré).

## 8. Achèvement métier — comment il est vérifié
**Il n'est pas vérifié, et le serveur le dit.** `business_state.completion_verified` est **toujours
`false`** : aucun contrat d'achèvement applicatif n'existe. M4 publie le **terminal de protocole**
pour le `seq` exact, et — avec `observe=true` — **l'état publié par le module** (via la projection
M3a) pour que l'appelant juge. Mesuré : `completion_verified=False` avec
`modules = {"bulk_de": {"n_contrasts": 1, "n_genes": 2000, "n_significant": 1012}}` (et
`active_contrast` **caviardé**, comme en M3a).

## 9. Vérification — 16 cas (`results_m4.txt`)

| Cas | Résultat |
|---|---|
| `accepted` (notre seq pas encore sur le fil) | ✅ `timeout` + `obs_status=done` (ack 5 ≠ 6) |
| `applied` · `running` · **statut inconnu** | ✅ `timeout`, et **`running` ⇒ `heartbeat_stalled_by_job=true`** ; inconnu **non terminal** |
| `done` · `error` · `invalid` · `ignored` | ✅ `state` = l'état, **terminal**, `stalled=false`, immédiat |
| verdict inchangé | ✅ `stalled=true`, `unchanged_polls=6`, backoff |
| **session perdue** (jeton changé en vol) | ✅ **`session_lost`** |
| assertion fausse | ✅ `SESSION_MISMATCH` |
| `observe=false` | ✅ `state=done`, **`writes=0`**, aucun `scenario.json` |
| `observe=true` **sans** terminal | ✅ **`OBSERVE_SKIPPED_NO_TERMINAL`**, aucun fichier écrit |
| `observe=true` **après** terminal (app-simulateur de fixture) | ✅ `observe_seq=8`, `after_terminal=true`, `completion_verified=false` |
| snapshot post-terminal **jamais acké** | ✅ **`OBSERVE_FAILED`**, `completion=false` |
| fuite / taille | ✅ jeton, chemin et **nom de contraste** absents ; **2302** octets |
| pureté stdout | ✅ sur **tous** les cas |
| **aucune écriture drive réelle** | ✅ `tools/_drive/` = `README.md` seul |

### 9.1 SDK officiel `@modelcontextprotocol/sdk` 1.30.1
`connect_ok` ✅ · **7 outils** ✅ · `0.6.0-m4` · `wait` → `state=done`, `terminal=true`,
`completion=false`, **`writes=0`** · `run`/`set_inputs` toujours refusés proprement ·
**`LEAKS: []`** · fermeture propre.

### 9.2 Régressions
Vecteurs **WorkBuddy** / **ZCode** (configs exactes) + rejeu de forme + `bare` : **7/7 outils**,
pureté, erreurs structurées, récupération, `ping {}` ✅ ; **contrôle négatif** → `framing_pure=False` ✅.
**M3a** : 7 cas inchangés ✅. **M3b** : 22 réponses ✅. **M3c** : 17 réponses ✅.

## 10. Avant / après

| | Valeur |
|---|---|
| Manifeste **avant** | `aa7c8aa57251adc26f453716fbb0b82fa788989a949a6d47aa3c34ee975e30cb` (437 f.) |
| Manifeste **après** | `93a67e2e742492d2d32c8a0797949b614455fb57ddc748dec18d99c455a90c72` (437 f.) |
| **Delta exact** | `scripts/mcp_server.R` **seul** |

Gardes **0 erreur / 59 avert.** (inchangé) · `git status` = ` M renv.lock` · `HEAD` = `9cb1a06` ·
configs clients **inchangées** · `--check` : run check **OK**, schema check **OK**, lecteurs drive
**present**.

## 11. Inventaire — **sept outils**

| Outil | Nature |
|---|---|
| `transcripto_drive_status` | read-only |
| `transcripto_drive_read_result` | read-only |
| `transcripto_drive_snapshot` | read-only (passif, M3a) |
| `transcripto_drive_set_inputs` | écriture contrôlée (M3b) |
| `transcripto_drive_run` | écriture contrôlée — `run_pipeline` seul (M3c) |
| **`transcripto_drive_wait`** | **observation bornée ; ≤ 1 snapshot, post-terminal (M4)** |
| `transcripto_drive_set_armed` | arm/disarm (M2) |

## 12. Déclaration

> **M4 est COMPLET.** Un seul fichier modifié (`scripts/mcp_server.R`). Aucune action, aucun module,
> aucun bouton ni capacité d'écriture n'a été ajouté au-delà du **seul** snapshot post-terminal
> explicitement autorisé. Aucun `eval`/`parse`/`source`, aucun appel direct à Shiny, Seurat, DESeq2
> ou une fonction de pathways, aucune mutation directe d'input Shiny. Le protocole drive est
> **réutilisé tel quel**. **Aucun travail de propagation de module n'a été entamé.**

# MCP M2 — Implementation plan (controlled arm/disarm) — BEFORE any edit

**Date** : 2026-09-24 · **Autorisation** : explicite, **M2 uniquement** (M3/M4 NON autorisés)

## 0. Pre-edit gate and repository state

| Contrôle | Résultat |
|---|---|
| Manifeste recalculé | **`40f300bd3b28d587…`** — **exigée** ✅ **PASS** |
| `HEAD` | **`9cb1a06`** (inchangé) |
| `git status` | **` M renv.lock`** **seul** (modification **préexistante**) |
| Fichiers | **437** |
| Dérive vs dernier état connu | **AUCUNE** (`cmp` identique à `manifest-cfgfix-after.txt`) |

---

## 1. Décision de conception : **UN SEUL outil** (pas deux)

**Réponse à la question posée** : arm et disarm sont **UNE** opération contrôlée, pas deux outils.

Justification :
1. **Le protocole drive les modélise déjà comme un booléen** — `arm.json` porte `armed: true|false`,
   et `ts_drive_arm_state()` traite `armed=false` **ou** l'absence du fichier comme un désarmement.
   Deux outils dupliqueraient une distinction que le protocole **n'a pas**.
2. **Minimisation** : le mandat exige « only the minimum required MCP tools ». **+1** outil, pas +2.
3. **Idempotence native** : « armer deux fois » et « désarmer deux fois » sont le **même** appel.

⇒ **Inventaire M2 : 3 outils** (2 read-only de M1 + **1** arm/disarm). Aucun autre.

## 2. Nom et schéma de l'outil

**Nom** : `transcripto_drive_set_armed`

```json
{
  "name": "transcripto_drive_set_armed",
  "description": "Controlled arm/disarm of the LIVE drive poller. Writes only tools/_drive/arm.json, atomically, using the live session's own token. Validates protocol, session identity (pid, started_at, session_token), heartbeat freshness and selected-session metadata before writing; refuses stale, mismatched, malformed or ambiguous sessions. Never accepts the wildcard token, never waits, never runs analysis, never touches Shiny inputs.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "armed": {
        "type": "boolean",
        "description": "true = arm the poller, false = disarm it. Required."
      },
      "expect": {
        "type": "object",
        "description": "Optional pin. Each field provided must match the live session EXACTLY, otherwise SESSION_MISMATCH. Use it to arm the session you inspected, and fail loudly if a different one has taken over.",
        "properties": {
          "pid":           { "type": "integer" },
          "started_at":    { "type": "string" },
          "session_token": { "type": "string" }
        },
        "additionalProperties": false
      }
    },
    "required": ["armed"],
    "additionalProperties": false
  }
}
```

**Pourquoi `expect` existe** : sans lui, les cas exigés *« PID mismatch »*, *« session-token
mismatch »*, *« started_at mismatch »* et *« selected-session mismatch »* seraient **inexerçables**.
`expect` est une **assertion**, jamais une source d'écriture : le jeton écrit dans `arm.json` est
**toujours** celui lu dans `ready.json`.

## 3. Réponse en succès

```json
{
  "content": [{ "type": "text", "text": "armed: true (arm.json written for pid 4242)" }],
  "structuredContent": {
    "action": "arm",
    "armed": true,
    "wrote": true,
    "arm_file": "arm.json",
    "wildcard_used": false,
    "session": {
      "pid": 4242,
      "started_at": "2026-09-24T15:41:38Z",
      "viewer": "visible",
      "heartbeat_age_s": 1.2,
      "heartbeat_timeout_s": 15,
      "session_token": "<redacted>"
    },
    "pinned": { "pid": true, "started_at": false, "session_token": false },
    "observed_armed_before": false,
    "note": "The app applies the arm on its next poll tick (~800 ms). This tool does not wait."
  },
  "isError": false
}
```

## 4. Contrat d'erreur (canal **contenu structuré**, jamais des codes JSON-RPC)

| Code | Déclencheur | `field` |
|---|---|---|
| `NO_SESSION` | `ready.json` absent | — |
| `INVALID_PROTOCOL` | `protocol != "ts-drive/1"` | `protocol` |
| `READ_FAILED` | fichier présent mais illisible/malformé | — |
| `AMBIGUOUS_SESSION` | `pid` / `started_at` / `session_token` **manquant ou vide**, **ou** jeton `= "*"` | le champ fautif |
| `STALE_SESSION` | battement périmé (> 15 s) — **inclut** le cas « app lancée sans le gate » | — |
| `SESSION_MISMATCH` | un champ de `expect` **diffère** du réel | le champ fautif |
| `ARM_WRITE_FAILED` | l'écriture **atomique** de `arm.json` a échoué | — |

**Erreurs de PROTOCOLE** (inchangées) : `-32602` params invalides (`armed` absent/non booléen,
`expect` mal formé), `-32601` méthode inconnue, `-32700` JSON illisible, `-32600` requête invalide.

## 5. Ordre de validation (strict, du moins coûteux au plus coûteux)

1. **Params** : `armed` présent **et** booléen ; `expect` (si présent) = objet aux clés autorisées
   → sinon **`-32602`** (erreur de **protocole**).
2. `ready.json` existe → sinon **`NO_SESSION`**.
3. `protocol == TS_DRIVE_PROTOCOL` → sinon **`INVALID_PROTOCOL`**.
4. Identité **complète** : `pid` non NULL, `started_at` non vide, `session_token` non vide **et
   `!= "*"`** → sinon **`AMBIGUOUS_SESSION`**.
5. **Battement frais** (`ts_drive_ready_fresh()`, timeout 15 s) → sinon **`STALE_SESSION`**.
6. **`expect`** : chaque champ fourni doit **égaler** le réel → sinon **`SESSION_MISMATCH`**.
7. **Écriture atomique** de `arm.json` via **`ts_drive_write_json()`** (réutilisé, non réécrit) :
   `{protocol, token, armed}` → échec ⇒ **`ARM_WRITE_FAILED`**.
8. Succès.

## 6. Sécurité et garanties

| Garantie | Mise en œuvre |
|---|---|
| **Jamais le jeton joker `"*"`** | le jeton écrit vient **toujours** de `ready.json` ; un jeton `*` ⇒ `AMBIGUOUS_SESSION` ; `expect.session_token = "*"` ⇒ `SESSION_MISMATCH` |
| **Jamais de jeton exposé** | `session_token` toujours `<redacted>` |
| **Jamais de chemin absolu** | `arm_file` = **basename** `"arm.json"` ; `root` jamais lu |
| **Pas de données biologiques / payloads / journaux / secrets** | seuls les champs d'identité et de fraîcheur sortent |
| **Aucune écriture hors du canal** | destination = `ts_drive_path("arm.json")`, **calculée**, jamais fournie par l'appelant |
| **Pas d'attente** | on écrit et on rend ; l'app applique au tick suivant (~800 ms) |
| **Pas d'exécution de code / pas d'input Shiny** | aucun `eval`/`parse`/`source`/`session$setInputs` |
| **Windows** | écriture **atomique** de `ts_drive_write_json` (8 essais, budget 1,0 s) |
| **Pureté stdout** | aucun `cat()`/`print()` vers stdout ; diagnostics sur stderr |
| **Indépendant du client** | protocole MCP standard ; aucun nom WorkBuddy/ZCode dans le code |

## 7. Fichiers

| Fichier | Action | Versionné ? | Dans le manifeste ? |
|---|---|---|---|
| `scripts/mcp_server.R` | **ÉTENDRE** (ajout de l'outil + du validateur + du writer) | **gitignoré** | oui (1 entrée) |

**Aucun fichier créé. Aucun fichier `R/`, aucun module, aucun test, aucun doc, aucune config.**

## 8. Vérifications prévues

Sonde à **racine-fixture** (comme en M1, sous `.workbuddy-ai/`, hors manifeste) :
no-session · stale-session · **PID mismatch** · **token mismatch** · **started_at mismatch** ·
**heartbeat timeout** · **selected-session mismatch** · requête malformée · **arm réussi** ·
**disarm réussi** · **disarm répété (idempotent)** · pureté stdout · codes de sortie Windows ·
**absence de fuite** (jeton/chemin absolu) · **delta de manifeste limité à `scripts/mcp_server.R`**.
🔑 L'**effet réel** de l'armement est vérifié en **relisant `arm.json`** écrit (le fichier est le
verdict), **sans** attendre le tick de l'app.

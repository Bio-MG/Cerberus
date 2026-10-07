# MCP M2 — Implementation report (controlled arm/disarm)

**Date** : 2026-09-24 (~19:1x) · **Autorisation** : explicite, **M2 uniquement** · **M3/M4 NON commencés**

---

## 1. Pre-edit gate and repository state

| Contrôle | Résultat |
|---|---|
| Manifeste recalculé | **`40f300bd3b28d587…`** — **exigée** ✅ **PASS** |
| `HEAD` | **`9cb1a06`** (inchangé) |
| `git status` | **` M renv.lock`** **seul** (modification préexistante) |
| Fichiers | **437** |
| Dérive vs dernier état connu | **AUCUNE** (`cmp` identique) |

## 2. Décision de conception : **UN SEUL outil**

**Réponse à la question posée** : arm et disarm = **UNE** opération contrôlée, **pas deux outils**.

1. Le protocole les modélise **déjà** comme un booléen (`arm.json.armed`), et `ts_drive_arm_state()`
   traite `armed=false` **ou** l'absence du fichier comme un désarmement ⇒ deux outils
   **dupliqueraient** une distinction que le protocole n'a pas.
2. Le mandat exige « only the minimum required MCP tools » ⇒ **+1**, pas +2.
3. **Idempotence native** : armer/désarmer deux fois = le **même** appel.

**Inventaire M2 : 3 outils** (2 read-only de M1 + 1 arm/disarm).

## 3. Schéma retenu (implémenté tel que proposé)

```json
{ "name": "transcripto_drive_set_armed",
  "inputSchema": {
    "type": "object",
    "properties": {
      "armed":  { "type": "boolean" },
      "expect": { "type": "object",
                  "properties": { "pid": {"type":"integer"},
                                  "started_at": {"type":"string"},
                                  "session_token": {"type":"string"} },
                  "additionalProperties": false }
    },
    "required": ["armed"],
    "additionalProperties": false } }
```

`expect` est **une assertion, jamais une source d'écriture** : le jeton écrit dans `arm.json` est
**toujours** celui publié par la session dans `ready.json`.

## 4. Contrat d'erreur

| Code | Déclencheur |
|---|---|
| `NO_SESSION` | `ready.json` absent |
| `INVALID_PROTOCOL` | `protocol != "ts-drive/1"` |
| `READ_FAILED` | fichier présent mais illisible/malformé |
| `AMBIGUOUS_SESSION` | `pid`/`started_at`/`session_token` manquant ou vide, **ou jeton `"*"`** |
| `STALE_SESSION` | battement périmé (> 15 s) |
| `SESSION_MISMATCH` | un champ de `expect` diffère du réel |
| `ARM_WRITE_FAILED` | l'écriture atomique de `arm.json` a échoué |

**Erreurs de PROTOCOLE** (canal distinct) : `-32602` params invalides · `-32601` méthode inconnue ·
`-32700` JSON illisible · `-32600` requête invalide.

## 5. Fichiers

| Fichier | Action | Versionné ? | Dans le manifeste ? |
|---|---|---|---|
| `scripts/mcp_server.R` | **ÉTENDU** (outil + validateur + écriture) | **gitignoré** | oui (1 entrée) |

**Aucun fichier créé.** `R/`, modules, drive files, `tests/`, `renv.lock`, gardes : **intacts**.

## 6. Vérifications — 18/18 PASS (+ régression M1 6/6)

| Cas | Attendu | Mesuré |
|---|---|---|
| no-session | `NO_SESSION` | ✅ |
| stale-session (600 s) | `STALE_SESSION` | ✅ |
| **heartbeat timeout** (20 s) | `STALE_SESSION` | ✅ |
| **PID mismatch** | `SESSION_MISMATCH` | ✅ |
| **session-token mismatch** | `SESSION_MISMATCH` | ✅ |
| **started_at mismatch** | `SESSION_MISMATCH` | ✅ |
| **selected-session mismatch** | `SESSION_MISMATCH` | ✅ |
| ambigu (jeton vide) | `AMBIGUOUS_SESSION` | ✅ |
| **`ready.json` publie `"*"`** | `AMBIGUOUS_SESSION` | ✅ **jamais accepté** |
| **appelant épingle `"*"`** | `SESSION_MISMATCH` | ✅ **jamais accepté** |
| requête malformée (`armed` absent / non booléen / champ inconnu) | `-32602` (protocole) | ✅ ×3 |
| **arm réussi** | `OK`, `isError:false` | ✅ `arm.json` = `{protocol: ts-drive/1, token: a1b2c3d4, armed: true}` |
| **disarm réussi** | `OK` | ✅ `{…, armed: false}` |
| **disarm répété** | idempotent | ✅ **fichier identique** |
| pureté stdout | `STREAM_PURE=True` | ✅ sur **tous** les cas |
| codes de sortie Windows | 0 | ✅ `--check` 0 / EOF 0 |
| fuite de jeton ou de chemin | aucune | ✅ `leaks -> NONE` |
| inventaire | 3 outils | ✅ exactement les 3 |
| **régression M1** | 6 scénarios | ✅ **tous PASS** |

🔑 **Le verdict est le FICHIER** : `arm.json` est relu après l'appel et comparé (protocole, jeton
**réel**, booléen). L'effet n'est **pas** déduit d'une réponse — et l'outil **n'attend pas** le tick de
l'app (`wait` est hors périmètre M2).

## 7. Empreintes

| | Valeur |
|---|---|
| **Avant** | `40f300bd3b28d587…` — 437 fichiers |
| **Après** | **`23ec9d7af70b3722…`** — 437 fichiers |
| **Delta exact** | **`scripts/mcp_server.R`** — et **rien d'autre** |
| `git status` | ` M renv.lock` **seul** · `HEAD` = `9cb1a06` |
| `tools/_drive/` réel | `README.md` **seul** — **jamais touché** (toute la vérification passe par la racine-fixture) |

## 8. Notes de conception

1. **`observed_armed_before` lit `ready.json.armed`**, pas `ts_drive_arm_state()` : ce dernier est
   **côté app** et **gated par `ts_drive_interactive()`**, qui est **FALSE** sous `Rscript` ⇒ il
   répondrait « non armé » en permanence depuis le serveur MCP. `ready.json.armed` est la vue **de
   l'app**, donc la bonne source.
2. **Le battement couvre le cas « app lancée sans le gate »** : sans `tools/launch_dev_drive.R`,
   l'observateur retourne sur sa 1ʳᵉ instruction et le battement **cesse** ⇒ `STALE_SESSION`. Aucune
   couture supplémentaire n'est requise.
3. **Écriture** : `ts_drive_write_json()` (le writer **existant** de l'app) — atomique, 8 essais,
   budget 1,0 s, compatible Windows. Destination **calculée** (`ts_drive_path("arm.json")`), jamais
   fournie par l'appelant.
4. **Désarmement = écrire `armed:false`** (et non supprimer) : réutilise le writer atomique et rend
   l'opération **idempotente** par construction.

## 9. Déclaration finale

> **Un seul fichier modifié** : `scripts/mcp_server.R` (**gitignoré**). **Aucun fichier suivi** n'a
> changé. **Non implémentés** : snapshot, `set_inputs`, `run`, `wait`, toute opération d'analyse.
> **Non touchés** : `app.R`, modules Shiny, `R/core/drive_watcher.R`, `R/core/drive_allowlist.R`,
> `tests/`, `renv.lock`, gardes de convention, documentation, configurations d'exemple. Le
> **protocole drive n'a pas changé** (réutilisé tel quel). Aucune évaluation R arbitraire, aucune
> mutation directe d'input Shiny. **M3 et M4 ne sont pas commencés.**

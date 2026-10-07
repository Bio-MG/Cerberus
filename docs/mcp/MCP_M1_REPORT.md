# MCP M1 — Implementation report (native JSON-RPC, 2 read-only tools)

**Date** : 2026-09-24 (~17:4x, Europe/Paris) · **Option B** implémentée · **M2 NON commencé**

---

## 1. Files changed

| File | Action | Versionné ? | Dans le manifeste gelé ? |
|---|---|---|---|
| `scripts/mcp_server.R` | **RÉÉCRITURE** (118 l. → ~420 l.) : transport btw remplacé par un handler JSON-RPC natif | **gitignoré** (`.gitignore:43`) | oui (1 entrée) |

**Fichiers créés : AUCUN.** Tout le code M1 tient dans le seul fichier **gitignoré**.
Aucun fichier sous `R/`. Aucun `mcptools`, `btw`, `ellmer`, `nanonext`, `processx`.

## 2. Did any TRACKED file change?

## **NON.** `git status --porcelain` rend **uniquement** ` M renv.lock` — état **antérieur**,
**non modifié** par M1 (il l'était déjà avant le jalon). `HEAD` reste **`9cb1a06`**.
`scripts/mcp_server.R` étant **gitignoré**, il n'apparaît **jamais** dans `git status`.

## 3. Exact tool inventory (mesuré par le fil, `tools/list`)

**Exactement 2 outils** — aucun autre :

| # | Nom | Rôle | `inputSchema` |
|---|---|---|---|
| 1 | `transcripto_drive_status` | session live : protocole, `viewer`, `pid`, `started_at`, âge du battement, `armed`, job | `{type:object, properties:{}, additionalProperties:false}` |
| 2 | `transcripto_drive_read_result` | verdict drive : `status`, `ack_seq`, `applied_at`, module, erreurs, avertissements, snapshot | idem |

**Absents par construction** : `list_r_sessions`, `select_r_session`, tout `btw_tool_*`
(**dont `btw_tool_run_r`**), `eval`/`parse`/`source`, `global_data`, Seurat, DESeq2, pathways.
`serverInfo.name = "transcriptoshiny-drive"`, version `0.1.0-m1`.

## 4. Test / probe results (evidence: `.workbuddy-ai/freeze/M1_EVIDENCE.txt`)

| # | Cas | Attendu | Mesuré |
|---|---|---|---|
| 1 | Démarrage propre (`--check`) | exit 0, **stdout 0 octet** | ✅ exit 0, **0 octet**, diagnostics sur stderr |
| 2 | EOF propre (stdin vide) | exit 0, stdout vide | ✅ exit 0, 0 octet |
| 3 | `initialize` + négociation | version négociée | ✅ `2024-11-05` → **2025-06-18** ; `2025-06-18` → `2025-06-18` |
| 4 | `tools/list` | exactement 2 outils | ✅ **2** |
| 5 | `tools/call` × 2 (happy path) | `<ok>` / `<ok>` | ✅ `isError:false` |
| 6 | **no-session** | `NO_SESSION` × 2 | ✅ |
| 7 | **stale-session** (battement 600 s, PID mort) | `STALE_SESSION` × 2 | ✅ + indice « process is gone » |
| 8 | **session mismatch** (`applied_at` < `started_at`) | `RESULT_SESSION_MISMATCH` | ✅ |
| 9 | **invalid protocol** (`ts-drive/0`) | `INVALID_PROTOCOL` × 2 | ✅ |
| 10 | **read failure** (`result.json` sans `applied_at`) | `READ_FAILED` | ✅ |
| 11 | JSON malformé puis trame valide | `-32700` **puis** réponse normale | ✅ **récupération prouvée** |
| 12 | `id` conservé | écho exact | ✅ ids 1, 2, 3, 4, 11 |
| 13 | Méthode inconnue / outil inconnu | `-32601` / `-32602` | ✅ |
| 14 | **Pureté stdout** | uniquement des trames | ✅ `STREAM_PURE=True` sur **tous** les cas |
| 15 | Sortie de processus Windows | exit 0 | ✅ exit 0 partout |
| 16 | Fuites (chemins, jetons) | aucune | ✅ `root` **non exposé**, `session_token` `<redacted>`, chemins `<path>` |

**Sonde de pureté stricte** : le flux doit être **exactement** `(Content-Length: N\r\n\r\n<body>)*`
depuis l'octet 0. Tout octet hors trame — y compris un préambule — est compté comme pollution.

🔑 **Contre-épreuve** : lancé **sans** `--no-init-file`, `STREAM_PURE=False` — **110 octets** de
pollution renv avant la 1ʳᵉ trame. ⇒ **`--no-init-file` est PORTEUR**, pas cosmétique.

## 5. Final manifest fingerprint

| | Valeur |
|---|---|
| **Avant édition** (porte du mandat) | **`451a7a4ceb0d9ef1…`** — **exigée** ✅ PASS |
| **Après implémentation** | **`c59ab42c4fc1d226…`** |
| Fichiers | **437 → 437** (aucun ajout, aucune suppression) |
| **Delta exact** | **`scripts/mcp_server.R`** — et **rien d'autre** |

⚠️ **L'égalité stricte avant/après est IMPOSSIBLE ici** : `scripts/mcp_server.R` est **dans** le
manifeste (mesuré : `grep -c` = 1). Le critère tenable est donc **« delta = exactement le fichier
autorisé »**, et c'est ce qui est mesuré. **Aucun autre fichier gelé n'a bougé.**

## 6. Changes to the launch command (REQUIRED, declared)

La commande devient **`Rscript --no-init-file scripts/mcp_server.R`**.
Motif **mesuré** : `.Rprofile` source `renv/activate.R`, dont le contrôle de synchronisation écrit
**2 lignes sur STDOUT** — donc **dans le transport MCP** — *pendant le démarrage de R*, donc
**impossible à supprimer depuis le script**. `--no-init-file` conserve `.Renviron`.
Alternative équivalente : `RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE` dans l'`env` du client.
⇒ Les 5 configs de `mcp.examples/` (**gitignorées**) doivent être mises à jour — **hors M1**,
signalé comme reste-à-faire.

## 7. Two real defects found and fixed DURING M1

1. 🔴 **`viewer` était caviardé** — `ts_drive_viewer()` rend `"headless"`, qui fait **8 caractères**
   ⇒ la règle « jeton 8+ » le masquait. C'est **exactement le piège `"snapshot"` déjà documenté**,
   rejoué parce que mon ensemble `known` était **incomplet** (il manquait `TS_DRIVE_VIEWERS`).
   **Corrigé** : `known` inclut désormais les 4 vocabulaires gelés. *(Sans le paramètre `known`,
   le caviardage détruit le vocabulaire du protocole.)*
2. 🔴 **Mauvaise source pour `viewer`** — `ts_drive_viewer()` décrit **le processus MCP**, pas la
   session de l'app (il rend `headless` sous `Rscript`). **Corrigé** : on lit le champ `viewer` que
   **l'app publie** dans `ready.json`.
3. ⚠️ **Artefact de sonde nettoyé** : `file("stdout","wb")` **crée un fichier littéral nommé
   `stdout`** au lieu d'écrire sur le flux — c'est pourquoi la sonde binaire rendait 0 octet. Un
   fichier parasite de 58 o a été créé puis **supprimé** (il n'était **pas** dans le manifeste gelé).

## 8. Windows findings that shaped the implementation

| Fait mesuré | Conséquence |
|---|---|
| `writeBin()`/`writeChar()` vers `stdout()` ⇒ **0 octet** | écriture binaire **inutilisable** ici |
| `file("stdout","wb")` ⇒ **crée un fichier `stdout`** | idem |
| mode texte : `\n` → `\r\n` ⇒ écrire `"\r\n"` donne `"\r\r\n"` | écrire **le newline de la plateforme** |
| corps mono-ligne (`pretty=FALSE`) ⇒ **aucun LF littéral** | `Content-Length` == octets écrits (vérifié : 76 == 76) |
| `\s` **ne matche pas** dans `regexec()` | utiliser `[ ]*` |
| `tasklist /FI` rend **0 ligne** même pour un PID vivant ; `/FO CSV` marche mais coûte **~762 ms** | sonde PID **seulement** quand le battement est déjà périmé |

## 9. Explicit confirmation

> **M2 was NOT started.** Aucun outil d'écriture (`arm.json`, `scenario.json`), aucune exécution de
> code, aucune sélection de session, aucun second serveur, aucun `mcptools`/`btw`/`ellmer` ajouté.
> Le protocole drive est **réutilisé tel quel** (lecteurs `R/core/drive_*.R` sourcés, non modifiés) ;
> `app.R`, les modules Shiny, les watchers, les allowlists, `tests/`, `renv.lock` et les gardes de
> convention sont **intacts**. **Aucun fichier suivi n'a changé.** `tools/_drive/` contient toujours
> `README.md` seul. La suite complète **n'a pas été lancée** (non demandée).

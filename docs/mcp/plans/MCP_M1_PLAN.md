# M1 — Implementation plan (option B, native JSON-RPC handler)

**Date** : 2026-09-24 · **Porte pré-édition** : `451a7a4ceb0d9ef1…` ✅ PASS

## 1. Exact files to modify

| File | Nature | Versionné ? | Dans le manifeste gelé ? |
|---|---|---|---|
| `scripts/mcp_server.R` | **RÉÉCRITURE** : remplace le transport btw par un handler JSON-RPC natif | **gitignoré** (`.gitignore:43`) | **oui** (1 entrée) |

⇒ Le manifeste **changera** (attendu, déclaré). **Aucun** autre fichier du dépôt n'est touché.

## 2. Exact files to create

**AUCUN.** Tout le code M1 vit dans le seul fichier **gitignoré** `scripts/mcp_server.R`.
Raison : créer `scripts/mcp_tools.R` en ferait un fichier **VERSIONNÉ** (`scripts/` n'est pas
gitignoré) et exigerait de modifier `.gitignore` (fichier **suivi**) ⇒ deux changements de
fichiers suivis au lieu de zéro. Le mandat dit « garder le code bêta gitignoré sous `scripts/` ».

## 3. Functions reused (no duplication)

**Depuis `R/core/drive_watcher.R`** (sourcé, non modifié) :
`ts_drive_boot()` · `ts_drive_read_ready()` · `ts_drive_read_result()` · `ts_drive_ready_age()` ·
`ts_drive_ready_fresh()` · `ts_drive_hb_timeout()` · `ts_drive_job_state()` · `ts_drive_job_busy()` ·
`ts_drive_arm_state()` · `ts_drive_viewer()` · `ts_drive_status_terminal()` · `ts_drive_path()` ·
`TS_DRIVE_PROTOCOL` · `TS_DRIVE_STATUSES` · `TS_DRIVE_ACTIONS` · `TS_DRIVE_MODULES` ·
`TS_DRIVE_BUTTONS` · `TS_DRIVE_ALLOWLIST`.

**Depuis `R/core/drive_allowlist.R`** : `ts_drive_badge_sanitize()` (redaction : chemins → `<path>`,
jetons 8+ → `<redacted>`, 1ʳᵉ ligne seulement, troncature) · `ts_drive_allowlisted()`.

🔑 **Aucun** lecteur n'est réécrit ; **aucun** écrivain drive n'est appelé (M1 = **read-only**).
**Aucune** couture neuve dans le protocole drive.

## 4. Framing strategy (MESURÉE, pas supposée)

**Contraintes découvertes par sonde** :
- `writeBin()`/`writeChar()` vers `stdout()` ou `file("stdout","wb")` ⇒ **0 octet écrit** sur cet hôte.
- `cat()`/`writeLines()` vers `stdout()` **fonctionnent** mais la **traduction texte Windows**
  transforme `\n` → `\r\n` : écrire `"\r\n"` produit `"\r\r\n"` (**casse le framing**).

**Stratégie retenue** :
- **Corps** : `jsonlite::toJSON(..., auto_unbox = TRUE, null = "null", pretty = FALSE)` ⇒ JSON
  **mono-ligne**, **aucun LF littéral** (les `\n` internes sont échappés `\\n`) ⇒ la traduction
  n'a **rien** à traduire ⇒ `Content-Length` == octets réellement écrits.
- **Séparateur d'en-tête** : `eol <- if (.Platform$OS.type == "windows") "\n" else "\r\n"`
  (sur Windows la traduction texte **ajoute** le `\r` ; sur Unix on l'écrit nous-mêmes).
- **Écriture** : `cat(paste0("Content-Length: ", nbytes, eol, eol, payload), file = stdout())`
  puis `flush(stdout())`.
- **Lecture** : `file("stdin", "rb")`, lecture **octet par octet** jusqu'à `\r\n\r\n`
  (13,10,13,10), parsing `Content-Length:[ ]*([0-9]+)`, puis `readBin(con, "raw", n = len)`.
  ⚠️ `\s` **ne matche pas** dans `regexec()` sur cet hôte (mesuré) ⇒ classes `[ ]`.
- **Vérifié bout en bout** : en-tête `\r\n\r\n` exact, `declared 76 == actual 76`.

## 5. Error schema

**Deux canaux distincts — jamais confondus** (le schéma MCP n'a **pas** de champ `code`) :

**(a) Erreurs de PROTOCOLE** → objet `error` JSON-RPC standard :
`-32700` parse · `-32600` requête invalide · `-32601` méthode inconnue · `-32602` params/outil inconnus.

**(b) Erreurs de DOMAINE** → **contenu structuré** d'un **résultat d'outil** :
```json
{"content":[{"type":"text","text":"…"}],
 "structuredContent":{"code":"NO_SESSION","message":"…","hint":"…"},
 "isError":true}
```
Codes fermés : `NO_SESSION` · `STALE_SESSION` · `RESULT_SESSION_MISMATCH` · `INVALID_PROTOCOL` ·
`READ_FAILED`.

**Validation d'identité** (contrat existant, réutilisé) :
`protocol` (`ts-drive/1`, sinon `INVALID_PROTOCOL`) · `pid` + `started_at` = **identité de session** ·
`session_token` (présent, **caviardé**) · **fraîcheur du battement** (`ts_drive_ready_fresh()`,
timeout 15 s) · **jamais `ack_seq` seul**.
- `NO_SESSION` = `ready.json` absent.
- `STALE_SESSION` = présent mais battement **périmé** (sonde PID alors lancée pour distinguer
  « app fermée » de « app vivante mais job synchrone en vol »).
- `RESULT_SESSION_MISMATCH` = `result.json` antérieur à `ready.json$started_at` (résidu d'une
  session précédente) ou protocole divergent.
- `READ_FAILED` = fichier présent mais illisible/illégitime.

## 6. Security / path restrictions

- **Read-only strict** : seuls `ready.json` et `result.json` sont lus, via `ts_drive_path()`.
  **Aucun** `unlink`, **aucun** `write*`, **aucune** création de fichier.
- **Jamais exposé** : `root` (chemin absolu), `session_token` (caviardé), `port`, charge utile
  brute, journaux, secrets, données biologiques. Projection **en liste blanche**.
- **Caviardage** : `ts_drive_badge_sanitize(msg, max_chars, known = TS_DRIVE_STATUSES ∪
  TS_DRIVE_MODULES ∪ TS_DRIVE_ACTIONS)` — le paramètre `known` est **load-bearing** (le piège
  mesuré du mot `"snapshot"` : 8 caractères ⇒ caviardé à tort sans `known`).
- **Aucun** outil d'exécution : ni `eval`, `parse`, `source`, ni `btw`, ni outil de session, ni
  `global_data`, ni Seurat/DESeq2/pathways.
- **Pureté stdout** : tout diagnostic ⇒ `stderr`. Lancement **`Rscript --no-init-file`** (mesuré :
  `.Rprofile` déclenche le contrôle de synchronisation **renv** qui écrit sur **stdout**).

## 7. Verification commands

```bash
# porte
find . <exclusions> | sort -z | xargs -0 sha256sum   # attendu 451a7a4ceb0d9ef1 AVANT
# pureté stdout + initialize + tools/list + tools/call
{ printf frames; sleep 8; } | Rscript --no-init-file scripts/mcp_server.R 1>out 2>err
awk '{ if ($0 ~ /^\{/) print "JSON" else print "POLLUT" }' out    # attendu : que des JSON
# inventaire exact (2 outils), absence de session, battement périmé, mismatch, protocole, EOF
```

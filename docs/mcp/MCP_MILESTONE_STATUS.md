# MCP — Milestone status (gitignored workspace record)

**Updated** : 2026-09-24 · **Maintained by** : agent workspace (`.workbuddy-ai/`)
**Nature** : enregistrement **local, gitignoré**, **hors** du manifeste gelé.
⚠️ Ce fichier **ne remplace pas** la documentation projet (`docs/`) — voir §5.

---

## 1. Statut des jalons (état demandé)

| Jalon | Statut | Portée |
|---|---|---|
| **M0** | ✅ **COMPLETE** | Audit lecture seule de l'architecture MCP existante |
| **M1** | ✅ **COMPLETE**, *including active-launcher integration* | Handler JSON-RPC **natif** + **2 outils read-only** + correction des **2 lanceurs clients actifs** |
| **M2** | ✅ **COMPLETE** | **Arm/disarm contrôlé** — **1** outil `transcripto_drive_set_armed`, écrit **uniquement** `tools/_drive/arm.json`, avec validation d'identité et de battement |
| **M3a** | ✅ **COMPLETE** | `transcripto_drive_snapshot` — observation passive, projection fermée, staleness et état protocole/métier séparés |
| **M3b** | ✅ **COMPLETE** | `transcripto_drive_set_inputs` — écriture contrôlée, valeurs validées, identité obligatoire, séquence et plafonds |
| **M3c** | ✅ **COMPLETE** | `transcripto_drive_run` — `run_pipeline` uniquement, Contract B, état `accepted` sans inférence de fin |
| **M4** | ✅ **COMPLETE** | `transcripto_drive_wait` — observation bornée, backoff, identité/chronologie/rejeu validés, snapshot post-terminal unique si demandé |

> ✅ **INTEROPÉRABILITÉ DE TRANSPORT M1/M2 : MCP-CONFORME ET VÉRIFIÉE** (2026-09-24, correction
> autorisée séparément — voir §2 et §3). Le framing **`Content-Length` (LSP)** a été remplacé par le
> **JSON délimité par des newlines** exigé par la spec MCP (`basic/transports` §stdio : *« Messages
> are delimited by newlines, and MUST NOT contain embedded newlines »*). Vérifié de bout en bout
> contre le **SDK officiel `@modelcontextprotocol/sdk` 1.30.1** — un **client tiers réel**, pas la
> sonde maison. Avant : `initialize` en **timeout `-32001`**. Après : `initialize`, `tools/list`
> (**exactement 3** outils), `tools/call` ×2, **`ping` = `{}`**, erreurs structurées, **récupération**
> après une ligne malformée, **fermeture propre**. ⚠️ **La sonde M1 partageait le framing du
> serveur : elle ne pouvait pas le tester** — c'est le SDK qui a révélé le défaut.

### 🔴 Frontière de clôture
M0–M4 sont clos. La propagation de modules vers **SC**, les **extensions Bulk** et **Spatial** constitue un projet futur séparé. Elle n'est pas autorisée ni démarrée automatiquement par la clôture M4.

Aucun autre outil, action, module, bouton ou capacité d'écriture n'est autorisé par cette clôture. Toute nouvelle capacité exige une autorisation explicite et distincte.

---

## 2. Ce qui a été livré

### M3a–M3c — observation et écritures contrôlées
- **M3a** : `transcripto_drive_snapshot`, passif et read-only, projection à liste blanche fermée.
- **M3b** : `transcripto_drive_set_inputs`, écriture contrôlée avec validation de valeur, identité, séquence et plafonds.
- **M3c** : `transcripto_drive_run`, `run_pipeline` uniquement, avec refus explicite des actions et jobs incompatibles.

### M4 — attente de job long
- **Outil unique** : `transcripto_drive_wait`.
- Attend un `run_pipeline` autorisé par `result.json`, sans écrire de snapshot pendant un job synchrone.
- Borné par `timeout_s`/`poll_ms`, backoff après verdicts inchangés, validation stricte du schéma, du protocole, de la chronologie et de la fenêtre de séquence.
- `observe=true` ne peut écrire qu'un snapshot post-terminal; `completion_verified` reste `false`.
- Vérifié par fixtures externes, SDK officiel `@modelcontextprotocol/sdk` 1.30.1, vecteurs WorkBuddy/ZCode et régressions M3a/M3b/M3c.

### M0 — audit (lecture seule)
`.workbuddy-ai/audits/MCP_M0_AUDIT.md` — architecture mesurée, inventaire d'outils, transport,
codes de sortie, dépendances, réutilisation des lecteurs drive, risques sécurité/Windows.

### M1 — implémentation (option B : handler natif)
- **`scripts/mcp_server.R`** réécrit (118 → ~420 l.) : transport `btw`/`mcptools` **remplacé** par un
  handler JSON-RPC **natif**. **Aucune** dépendance ajoutée (`jsonlite` seul, déjà au `renv.lock`).
- **Inventaire exact : 2 outils** — `transcripto_drive_status`, `transcripto_drive_read_result`
  (read-only). **Aucun** `btw_tool_*`, aucun outil de session, aucun `eval`/`parse`/`source`,
  pas de `global_data`, pas de Seurat/DESeq2/pathways.
- Erreurs **métier** structurées dans `structuredContent` (`NO_SESSION`, `STALE_SESSION`,
  `RESULT_SESSION_MISMATCH`, `INVALID_PROTOCOL`, `READ_FAILED`) ; erreurs de **protocole** en
  JSON-RPC standard (`-32700`, `-32600`, `-32601`, `-32602`, `-32603`).
- Rapports : `MCP_M1_COMPAT_PROBE_AND_MEMO.md`, `MCP_M1_PLAN.md`, `MCP_M1_REPORT.md`,
  `MCP_M1_INTEGRATION_AUDIT.md` ; preuve brute : `.workbuddy-ai/freeze/M1_EVIDENCE.txt`.

### M1 — intégration des lanceurs actifs (lot séparé, clos)
- `.zcode/config.json` et `~/.workbuddy-ai/mcp.json` : **1 ligne** `"--no-init-file",` insérée en
  tête de `args` (aucun autre champ touché ; **CRLF sans BOM** préservés).
- Rapport : `.workbuddy-ai/audits/MCP_M1_CFG_FIX_REPORT.md`.

### M1/M2 — correction de transport : NDJSON conforme MCP (lot séparé, clos et accepté)
- **Défaut** (audit multi-clients, `MCP_MULTI_CLIENT_AUDIT.md`) : le serveur parlait
  `Content-Length: N\r\n\r\n` (**framing LSP**), alors que la spec MCP impose le **JSON délimité par
  des newlines**. Le SDK de référence sérialise `JSON.stringify(msg) + '\n'` et **n'a aucun transport
  `Content-Length`** : `initialize` **échouait** (timeout), et son lecteur recevait la ligne
  `"Content-Length: 409"`. ⇒ **défaut SERVEUR**, pas client : tout client conforme échouait.
- **Correctif** (`scripts/mcp_server.R`, **seul fichier modifié**) : un message JSON par ligne
  (`.ts_write_message` / `.ts_read_message` / `.ts_strip_cr`) ; ligne blanche **ignorée** ; ligne
  malformée → `-32700` **puis la boucle CONTINUE** (récupération) ; diagnostics **stderr** uniquement ;
  EOF propre ; **tolérance CR** pour les clients CRLF (le `cat()` de R émet CRLF sous Windows).
- **Corrigés au passage** : **F-2** — `ping` rend `{}` (et non `[]`) ; **F-3** —
  `transcripto_drive_status` lit **`ready.json.armed`** (vue publiée par l'app) au lieu de
  `ts_drive_arm_state()`, gaté par `ts_drive_interactive()` (donc `FALSE` sous `Rscript`) ⇒ les **2**
  outils read-only **s'accordent**.
- **Inventaire à cette étape : exactement 3 outils.** **Aucune** dépendance ajoutée, **aucun** second
  serveur, **aucun** `snapshot`/`set_inputs`/`run`/`wait`, **aucune** évaluation R arbitraire.
  Protocole drive, `app.R`, modules, `tests/`, `renv.lock`, documentation, configs clients, allowlists
  et gardes : **intacts**. Gardes : **0 erreur / 59 avert.** (inchangé).
- **Preuve** : SDK officiel **1.30.1** ✅ (fixture **et** racine réelle) ; WorkBuddy + ZCode (configs
  exactes) ✅ ; **contrôle négatif** (`--no-init-file` omis) → pollution **détectée** ✅ ;
  `STALE_SESSION` / `NO_SESSION` / `-32601` / `-32602` / `-32700` **préservés** ; refus du joker
  **inchangé** (il vit dans `set_armed`, non touché) ; **aucune fuite** (jeton, chemin, gènes, `raw_path`).
- Rapport : `.workbuddy-ai/audits/MCP_TRANSPORT_CORRECTION.md`.

---

## 3. Empreintes (chaîne d'intégrité)

| Étape | Empreinte | Fichiers |
|---|---|---|
| Gel de départ (avant M0) | `451a7a4ceb0d9ef1…` | 437 |
| Après M1 (`scripts/mcp_server.R`) | `c59ab42c4fc1d226…` | 437 |
| Après correctif des lanceurs (`.zcode/config.json`) | `40f300bd3b28d587…` | 437 |
| Après **M2** (`scripts/mcp_server.R`) | `23ec9d7af70b3722…` | 437 |
| Après **M3c** (`scripts/mcp_server.R`) | `aa7c8aa57251adc26f453716fbb0b82fa788989a949a6d47aa3c34ee975e30cb` | 437 |
| Après **M4**, avant correctif (`scripts/mcp_server.R`) | `121415e5eea6253cbbfe737aa72de7a81dd4bd5e7b7dc56d42a9b534a64fceea` | 437 |
| Après **correctif M4** (`scripts/mcp_server.R`) | **`de48f50988fcd4a19bb4de11748f1f39b4614ed1cbae8ee8c47af544f6d566cc`** | 437 |

**Deltas autorisés, cumulés** : `scripts/mcp_server.R` (M1, M2, M3a, M3b, M3c, M4 et correctif M4) +
`.zcode/config.json` (intégration) + les 9 fichiers de la passe documentaire. La config externe
`~/.workbuddy-ai/mcp.json` est **hors dépôt** ⇒ **0** entrée au manifeste.

**Correction de transport (2026-09-24, autorisée séparément)** — delta = **`scripts/mcp_server.R`
SEUL** (`6065207d…` → `14a6620e…`). Corrige le défaut **F-1** de l'audit multi-clients :
le framing `Content-Length` (LSP) est remplacé par le **JSON délimité par des newlines** exigé par la
spec MCP. Corrige aussi **F-2** (`ping` rend `{}`) et **F-3** (`status` lit `ready.json.armed`).
**Preuve** : le SDK officiel `@modelcontextprotocol/sdk` **1.30.1** se connecte et complète
`initialize` / `tools/list` / `tools/call` ×2 / `ping` (il **échouait** avant).
Rapport : `MCP_TRANSPORT_CORRECTION.md`. **État historique :** cette preuve précédait M3/M4 ; l'état actuel est §1.

## 4. État du dépôt

- `HEAD` = **`9cb1a06`** (inchangé depuis la séparation des chantiers).
- `git status` = **` M renv.lock`** **seul** — modification **préexistante**, non touchée par les
  lots MCP.
- `scripts/mcp_server.R` est **gitignoré** : les changements M3/M4 y sont invisibles à `git status`,
  mais restent mesurés dans le manifeste gelé. Aucun fichier suivi applicatif n'a été modifié.

## 5. ✅ Divergence RÉSOLUE — passe documentaire CLOSE et ACCEPTÉE (2026-09-24)

La divergence « hors roadmap / à valider » qui était signalée ici est **CORRIGÉE**. Une **passe
documentaire séparée et explicitement autorisée** (lot **doc-only**) a été exécutée, vérifiée, puis
**acceptée et close par l'utilisateur**.

| Contrôle | Résultat |
|---|---|
| Delta | **9 fichiers**, tous autorisés : `docs/STATUS.md`, `docs/ROADMAP.md`, `AGENTS.md`, `mcp.examples/README.md` + les **5** `mcp.examples/*.json` |
| Porte pré-édition | `23ec9d7af70b3722…` (**437** f.) ✅ **PASS** |
| Après | `bccc980bd46c0bceda8534c91bac467500a2c1bfabed6d94472d6d4855e16fc1` (**437** f.) |
| `docs/archive/STATUS_JOURNAL.md` | **byte-identique** (`5011ef1b0ba7a131…`) |
| Zones interdites | **inchangées** — `scripts/mcp_server.R`, `app.R`, `R/`, `modules/`, `tools/_drive/`, `tests/`, `renv.lock`, gardes, configs clients vivantes |
| JSON | **5/5** parsent ; **aucun** BOM ; **aucune** fin de ligne altérée (tout le dépôt est en **LF**) |
| `opencode.json` | schéma **`command`-array conservé** (pas de clé `args`) |
| Serveur (état historique de la passe documentaire) | hash **inchangé** ; **exactement 3** outils (M0–M2) |

**État actuel** : **M0 ✅ / M1 ✅ / M2 ✅ / M3a ✅ / M3b ✅ / M3c ✅ / M4 ✅**. Le serveur expose
exactement **7** outils, conserve le framing NDJSON et l'inventaire M3, et le correctif M4 est validé.

**Frontière future** : la propagation vers **SC**, les extensions **Bulk** et **Spatial** est un projet
séparé. Elle n'est ni autorisée ni lancée automatiquement par la clôture M4. Toute nouvelle
propagation ou capacité nécessite une autorisation explicite et distincte.

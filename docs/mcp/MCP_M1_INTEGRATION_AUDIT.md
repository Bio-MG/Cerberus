# MCP M1 — Final read-only integration audit

**Date** : 2026-09-24 (~17:5x) · **Aucun fichier modifié** · **M2 NON commencé**
**Portée** : audit d'intégration **lecture seule** avant clôture de M1.

---

## 0. Méthode et non-modification

Recherche **ignore-blind** (`grep -rI` + `--exclude-dir`), parce que l'outil de recherche habituel
**respecte `.gitignore`** — or tout le composant MCP est **gitignoré**. Une recherche standard
aurait donc **manqué** `scripts/mcp_server.R`, `mcp.examples/`, `docs/`, `AGENTS.md` et `.zcode/`.

> **Aucun fichier n'a été modifié par cet audit.** Manifeste gelé **inchangé** :
> `c59ab42c4fc1d226…`, **437** fichiers. `git status` = ` M renv.lock` **seul** (antérieur).
> `HEAD` = `9cb1a06`.

---

## 1. Toute commande de lancement documentée utilise-t-elle `--no-init-file` ?

# ❌ **NON — aucune.**

Mesure : `grep -rlI -- "no-init-file"` sur tout le dépôt (ignore-blind) ne rend que **3** fichiers :

| Fichier | Nature |
|---|---|
| `scripts/mcp_server.R` | le serveur lui-même — **son en-tête documente bien `--no-init-file`** (5 mentions) |
| `.workbuddy-ai/audits/MCP_M1_PLAN.md` | rapport interne |
| `.workbuddy-ai/audits/MCP_M1_REPORT.md` | rapport interne |

⇒ **Aucun lanceur, aucun exemple, aucune configuration, aucune documentation** ne porte le drapeau.
**Toutes** les commandes documentées sont donc **périmées** au sens « pureté stdout ».

---

## 2. Inventaire EXACT des fichiers qui invoquent le serveur sans `--no-init-file`

### A. Lanceurs RÉELS (opérationnels, sur cette machine)

| # | Fichier | Commande actuelle | Versionné ? | Dans le manifeste ? |
|---|---|---|---|---|
| 1 | `.zcode/config.json` | `args: [<script>]`, `env: {}` | non (`.gitignore:39`) | **oui** (4 entrées `.zcode`) |
| 2 | `~/.workbuddy-ai/mcp.json` | `args: [<script>]` | **hors dépôt** | non |

### B. Exemples / gabarits (`mcp.examples/`, 6 fichiers)

| # | Fichier | Où insérer le drapeau |
|---|---|---|
| 3 | `mcp.examples/claude_desktop.json` | `args` (préfixer) |
| 4 | `mcp.examples/opencode.json` | ⚠️ **pas de clé `args`** : c'est `command: [Rscript, <script>]` ⇒ insérer **dans le tableau `command`** |
| 5 | `mcp.examples/vscode_mcp.json` | `args` (préfixer) |
| 6 | `mcp.examples/workbuddy_template.json` | `args` (préfixer) |
| 7 | `mcp.examples/zcode_template.json` | `args` (préfixer) |
| 8 | `mcp.examples/README.md` | **3 lignes** : l. 38-39 (install + `--check` PowerShell), l. 46 (`--check` sh), l. 56 (`claude mcp add`) |

### C. Documentation

| # | Fichier | Contenu | Verdict |
|---|---|---|---|
| 9 | `AGENTS.md:98` | ``check: `Rscript scripts/mcp_server.R --check` `` | à mettre à jour (doc) |
| 10 | `docs/archive/STATUS_JOURNAL.md:354` | `Rscript scripts/mcp_server.R --check` | 🚫 **ARCHIVE — NE PAS toucher** |
| 11 | `docs/ROADMAP.md` (l. 433, 470), `docs/PROPOSAL_PILOTAGE_PROGRAMMATIQUE.md`, `docs/STATUS.md` | **mentionnent** le serveur, **ne portent aucune commande de lancement** | aucun changement nécessaire |

⚠️ **Tous** ces fichiers (sauf le n° 2, hors dépôt) sont **dans le manifeste gelé** ⇒ les éditer
**changera l'empreinte** : `mcp.examples/` = **6** entrées, `.zcode` = **4**, `AGENTS.md` = **1**,
`docs/ROADMAP.md` = **1**, `docs/STATUS.md` = **1**.

*Note* : `.workbuddy-ai/tmp/` contient des **copies périmées** de fichiers du dépôt
(`baseline/`, `at43/`) — artefacts de sessions, **hors manifeste** (`.workbuddy-ai/` est exclu),
**pas des lanceurs**.

---

## 3. Les 5 configurations `mcp.examples/` : gitignorées ? opérationnellement requises ?

- **Gitignorées : OUI** — les **6** fichiers (`README.md` + 5 JSON) sont ignorés ; **0** suivi par git.
- **Opérationnellement requises : NON.** Aucun code ne les **lit**. Le seul fichier qui nomme
  `mcp.examples` est `tools/check_writers.R:44`, où il figure dans la liste `SCAN` (racines
  **parcourues** par le détecteur d'écrivains) — ce n'est **pas** une lecture de configuration.
- ⇒ Ce sont des **gabarits de documentation**. Les **seuls** lanceurs réels sont
  `.zcode/config.json` et `~/.workbuddy-ai/mcp.json`.

---

## 4. Hypothèses d'environnement (vérifiées)

| # | Hypothèse | Mesure |
|---|---|---|
| 1 | **Racine du projet** | `scripts/` est à **1 niveau** de la racine ; `app.R` **présent**, `renv/activate.R` **présent** ⇒ la remontée (≤ 5 niveaux) aboutit au **1ᵉʳ** essai |
| 2 | **Chemin du script** | résolu depuis `--file=` ; le script **doit rester dans `scripts/`** (sinon `stop()` explicite) |
| 3 | **Résolution de `jsonlite`** | globs `renv/library/*/R-*/*` + `renv/library/*/R-*` ⇒ **trouvés** ; `jsonlite` **2.0.0** en bibliothèque **projet** ET en bibliothèque **système** (`D:/Data_science/R-4.4.2/library`), identique au lockfile ⇒ **double filet** : le serveur démarre même si la bibliothèque renv est absente |
| 4 | **Invocation Rscript Windows** | `D:/Data_science/R-4.4.2/bin/Rscript.exe` — **R 4.4.2** ; R portable **absent du PATH** ⇒ chemin **absolu obligatoire** ; slashs obliques OK, chemin avec **espaces et parenthèses** à garder en **une seule** chaîne JSON |
| 5 | **`.Renviron`** | **conservé** par `--no-init-file` (contrairement à `--vanilla`) ⇒ les réglages renv du projet (sandbox désactivée) restent appliqués |

---

## 5. `--check` et EOF propre restent-ils sûrs sous la commande documentée ?

| Contrôle | Résultat |
|---|---|
| `Rscript --no-init-file scripts/mcp_server.R --check` | **exit 0**, **stdout = 0 octet** ✅ |
| `Rscript --no-init-file scripts/mcp_server.R </dev/null` (EOF propre) | **exit 0**, **stdout = 0 octet** ✅ |

⇒ Les deux modes **n'émettent rien** sur le transport ; les diagnostics vont sur **stderr**.

**Rappel de la contre-épreuve** (mesurée en M1) : **sans** `--no-init-file`, le flux contient
**110 octets** de pollution renv **avant** la 1ʳᵉ trame (`STREAM_PURE=False`).

---

## 6. Classification des fichiers à modifier (pour un futur lot)

### 🔴 A — Requis pour l'USABILITÉ de M1
Sans ces deux-là, un client réellement configuré reçoit un flux **corrompu**.

1. **`.zcode/config.json`** — lanceur ZCode **vivant** (`args` à préfixer par `--no-init-file`).
   Alternative **1 ligne** : renseigner `env` (déjà présent, vide) avec
   `RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE` — pas de changement d'`args`.
2. **`~/.workbuddy-ai/mcp.json`** — lanceur WorkBuddy **vivant**, **hors du dépôt**.

### 🟡 B — Mises à jour de documentation / exemples (optionnelles)
Ne bloquent **rien** : ce sont des gabarits, aucun code ne les lit.

3-7. les **5** `mcp.examples/*.json` (dont `opencode.json`, où le drapeau va dans **`command`**).
8. `mcp.examples/README.md` — **3** lignes de commande.
9. `AGENTS.md:98` — ligne `--check`.

### ⚪ C — Hors périmètre
10. `docs/archive/STATUS_JOURNAL.md:354` — **archive** : l'éditer **falsifierait un enregistrement
    historique**. À laisser tel quel (éventuellement une note « commande historique »).
11. `docs/ROADMAP.md` / `PROPOSAL_PILOTAGE_PROGRAMMATIQUE.md` / `STATUS.md` — **aucune commande**
    ⇒ rien à changer.

---

## 7. Déclaration finale

> **Aucun fichier modifié.** Aucun lanceur, exemple ou document mis à jour — conformément à la
> consigne. Le manifeste gelé reste **`c59ab42c4fc1d226…`** (**437** fichiers) ; `git status` =
> ` M renv.lock` **seul** (antérieur, intact) ; `HEAD` = `9cb1a06`. `app.R`, les modules, les
> fichiers drive, `tests/`, `renv.lock` et les gardes de convention sont **intacts**.
> **M2 n'est pas commencé.**

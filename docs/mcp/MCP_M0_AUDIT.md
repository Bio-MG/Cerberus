# MCP M0 — Read-only audit report

**Date** : 2026-09-24 (16:2x, Europe/Paris)
**Mandat** : MCP M0 — *audit only*. Aucun outil MCP implémenté. Aucun test lancé.
**Nature** : rapport **local, gitignored** (`.workbuddy-ai/` — `.gitignore:40`).
**Statut du composant** : MCP = bêta local, **gitignored**, hors roadmap séquencée.

---

## 0. Porte d'entrée (gate) et intégrité de la mesure

| Contrôle | Résultat |
|---|---|
| Manifeste gelé recalculé **avant** M0 | `451a7a4ceb0d9ef1…` |
| Valeur **exigée** | `451a7a4ceb0d9ef1…` |
| Verdict | ✅ **PASS** — 437 fichiers |
| `HEAD` | `9cb1a06` (3 commits non amendés, reflog linéaire) |
| `git status` | ` M renv.lock` **seul** |

🔑 **Non-perturbation PROUVÉE par re-mesure** : le manifeste a été re-haché **après** chaque
invocation R de cet audit (sonde de réutilisation, `--check`, garde `--file=`) ⇒ **identique**
`451a7a4ceb0d9ef1…` à chaque fois. Les 3 commandes ci-dessous n'ont **rien écrit** :

| Invocation | Effet mesuré |
|---|---|
| sonde `source()` + lecteurs | 0 fichier modifié |
| `mcp_server.R --check` | 0 fichier modifié, `tools/_drive/` reste `README.md` seul |
| `Rscript -e 'source(mcp_server.R)'` | 0 fichier modifié (s'arrête au garde) |

**Emplacement du rapport** : `.workbuddy-ai/audits/MCP_M0_AUDIT.md`, **pas** `docs/`.
Raison **mesurée** : `docs/` pèse **93 entrées** dans le manifeste gelé, `.workbuddy-ai/` en pèse **0**
(`grep -c`). Écrire dans `docs/` aurait fait passer le manifeste de 437 → 438 fichiers et déclenché
une **fausse alerte de dérive** à la prochaine porte. Si vous préférez `docs/`, dites-le : le
déplacement coûte +1 fichier au manifeste, à déclarer.

---

## 1. Architecture MCP actuelle (mesurée)

```
client MCP (Claude Desktop / OpenCode / VS Code / ZCode / WorkBuddy)
        │  JSON-RPC 2.0, stdio (stdin/stdout), AUCUN port réseau
        ▼
D:/Data_science/R-4.4.2/bin/Rscript.exe  scripts/mcp_server.R
        │  1. résout la racine depuis --file= (remontée ≤ 5 niveaux, app.R + renv/activate.R)
        │  2. setwd(racine) + active renv/activate.R SI la bibliothèque projet n'est pas sur .libPaths()
        │  3. exige mcptools + btw + ellmer (fail-fast avec le remède)
        │  4. --check → affiche et quit(status = 0)
        │  5. sinon → btw::btw_mcp_server(btw::btw_tools("docs", "pkg"))   ← BLOQUANT
        ▼
outils servis = docs + pkg  (documentation R + outillage de développement de paquet)
        ⚠️ AUCUN outil du projet. AUCUN lien avec le drive.
```

**Fait structurant** : `scripts/mcp_server.R` est **gitignoré** (`.gitignore:43`) ⇒ il **n'apparaît
jamais** dans `git status`, n'est dans **aucun commit**, et un clone neuf ne le possède pas. Idem
`mcp.examples/` (`.gitignore:42`, 6 fichiers). C'est **exactement** la posture « bêta local » annoncée
— mais cela signifie aussi que **toute extension de ce fichier sera invisible au versionnage**.

**Taille** : `scripts/mcp_server.R` = **118 lignes**. 5 sections numérotées (racine, renv, deps,
`--check`, serveur).

---

## 2. Inventaire des outils actuels (mesuré)

| Source | Outils | Lien avec le projet |
|---|---|---|
| `btw::btw_tools("docs")` | aide/documentation R | **aucun** |
| `btw::btw_tools("pkg")` | outillage de développement de paquet R | **aucun** |
| `mcptools::mcp_session()` (optionnel, hors lanceur) | plomberie de session interactive | indirect |

🔴 **Il n'existe aujourd'hui AUCUN outil MCP qui parle au drive.** Le commentaire du lanceur est
explicite : *« Bare `mcptools::mcp_server()` would serve session plumbing only »*. Le choix « docs + pkg »
a été fait pour **ne pas recouvrir** les outils fichier/exécution que l'agent possède déjà.

⇒ **M1 ne « branche » pas des outils existants : il en crée.** Le canal `scripts/mcp_server.R` est
aujourd'hui un **serveur de documentation R**, pas un plan de contrôle.

---

## 3. Démarrage et transport (mesurés)

| Aspect | Mesure |
|---|---|
| Transport | **stdio uniquement**. Aucun port. `type="http"` **délibérément** non configuré. |
| Commande | `Rscript scripts/mcp_server.R` (bloquant) |
| Vérification | `Rscript scripts/mcp_server.R --check` |
| Résolution racine | depuis `--file=` ; remontée ≤ **5** niveaux ; exige `app.R` **et** `renv/activate.R` |
| `cwd` client | **confort, pas exigence** (le lanceur fait `setwd()` + active renv lui-même) |
| renv | activé **explicitement** via `renv/activate.R` (`.Rprofile` n'active que pour une session **démarrée** dans la racine) |
| Secrets | **aucun** lu ni requis (« Never add API keys/tokens here ») |

**Formats clients fournis** (`mcp.examples/`) — tous en **chemins absolus à slashs obliques** :

| Fichier | Cible | Statut |
|---|---|---|
| `claude_desktop.json` | `%APPDATA%\Claude\claude_desktop_config.json` | format vérifié |
| `opencode.json` | `opencode.json` (clé `mcp`) | vérifié, supporte `cwd` |
| `vscode_mcp.json` | `.vscode/mcp.json` | vérifié |
| `zcode_template.json` | `.zcode/config.json` (`mcp.servers`) | vérifié (2026-09-12), schéma **strict** |
| `workbuddy_template.json` | `%USERPROFILE%\.workbuddy-ai\mcp.json` | vérifié |

⚠️ `zcode_template.json` documente un point dur : **schéma STRICT** — toute clé inconnue **fait
disparaître le serveur** (seuls `type/command/args/cwd/env/enabled/timeoutMs`).

---

## 4. Schémas requête/réponse (mesurés)

- **Protocole** : **MCP / JSON-RPC 2.0 sur stdio**, **entièrement délégué** à `btw`/`mcptools`.
  **Aucun** schéma n'est défini côté projet : le dépôt ne contient **aucun** handler JSON-RPC.
- **Conséquence pour M1** : servir des outils du projet **oblige à implémenter un handler**
  (framing, `initialize`, `tools/list`, `tools/call`) **ou** à passer par l'API d'enregistrement de
  `btw`. C'est le **choix structurant** de M1 (§11, décision 1).

### Comportement d'erreur et codes de sortie (MESURÉS)

| Cas | Sortie mesurée | Code |
|---|---|---|
| `--check` nominal | `mcp_server check: OK` + root, `renv_active: TRUE`, `mcptools: 1.0.2`, `btw: 1.5.0`, `transport: stdio` | **0** |
| pas d'argument `--file=` (mauvaise invocation) | `Error: mcp_server: cannot determine the script path (no --file= argument). Launch this server via Rscript, e.g. Rscript scripts/mcp_server.R` | **1** |
| paquet manquant | `Error: mcp_server: required package '<p>' is missing. From the project root run: renv::install("<p>")` | **1** (par `stop()`) |
| racine introuvable | `Error: mcp_server: project root not found (expected app.R + renv/activate.R above …)` | **1** |
| renv non activable | `Error: mcp_server: failed to activate project renv: …` | **1** |

🔎 **Deux observations fines** :
1. `.mcp_require("ellmer", …)` s'exécute **avant** le bloc `--check` ⇒ `ellmer` est **exigé** mais
   **jamais affiché**. Le contrôle est donc **plus strict** que son rapport.
2. `--check` s'exécute **après** les `require` : si une dépendance manque, **`--check` échoue aussi**
   (il ne peut pas servir de diagnostic « dégradé »). Un `--check` qui n'exigerait rien serait un
   meilleur outil de diagnostic — **proposition M1**.

⚠️ Les **deux** messages renv/locale apparaissent à **chaque** démarrage (voir §8) et **polluent
stdout** — sur un transport **stdio**, écrire du texte parasite sur stdout est un **risque de
protocole réel** (§8).

---

## 5. Flux d'appel MCP → drive

### Aujourd'hui (mesuré)
```
client → mcp_server.R → btw (docs+pkg) → réponse
                                   ✗ aucun contact avec tools/_drive/
```

### Cible M1 (read-only) — le contrat drive est **déjà** en place et **réutilisable**
```
client MCP ──stdio──▶ mcp_server.R ──▶ [lecteurs drive réutilisés] ──▶ tools/_drive/ready.json
                                                                        tools/_drive/result.json
                    ◀── réponse ◀── (PID, token, fraîcheur, snapshot, statut de job, erreurs)
```
🔑 **M1 ne parle PAS à R en direct et n'exécute rien** : il **lit des fichiers** que la session
vivante écrit. C'est le même contrat que l'agent utilise déjà (spec : *« Re-reading `result.json` is
not polling »* — et `result.json` **ne porte PAS** de session token).

---

## 6. Les 12 points du mandat — réponses mesurées

| # | Point | Mesure |
|---|---|---|
| 1 | Point d'entrée `scripts/mcp_server.R` | 118 lignes, 5 sections, **gitignoré** (`.gitignore:43`) |
| 2 | Transport | **stdio**, aucun port ; `setwd()` + renv activé par le lanceur |
| 3 | Commande de démarrage | `Rscript scripts/mcp_server.R` (+ `--check`), Rscript **portable** absolu requis |
| 4 | Outils existants | `btw` **docs + pkg** uniquement — **0** outil projet, **0** outil drive |
| 5 | Schémas requête/réponse | **JSON-RPC 2.0 / MCP** délégué à btw/mcptools ; **0** handler dans le dépôt |
| 6 | Erreurs / codes de sortie | `--check` → **0** ; 4 chemins d'erreur → **1** par `stop()` (§4) |
| 7 | Compatibilité Windows | voir §8 (slashs obliques OK, espaces/parenthèses, `file.rename` ~5,1 s, bruit stdout) |
| 8 | Restrictions de chemin | racines d'import + refus `..` **avant** normalisation + existence + non-répertoire (§7) |
| 9 | Dépendances / lockfile | **`mcptools` 1.0.2, `btw` 1.5.0, `ellmer` 0.5.0 : installés, **0** entrée `renv.lock** ; `nanonext` 1.10.1 et `processx` 3.9.0 **sont** dans le lockfile |
| 10 | Réutilisation lecteurs/écrivains drive | ✅ **MESURÉ : faisable, sans Shiny** (§ ci-dessous) |
| 11 | Validation (protocole/PID/token/started_at/battement/session) | §9 |
| 12 | Compatibilité Viewer / Chrome visible / headless | §10 |

### Point 10 — réutilisation headless : **MESURÉE, pas supposée**

Sonde `.workbuddy-ai/freeze/probe-mcp-reuse.R` exécutée en **`Rscript` nu** (aucune session Shiny) :

| Contrôle | Résultat |
|---|---|
| `source("R/core/drive_allowlist.R")` puis `source("R/core/drive_watcher.R")` | ✅ **TRUE / TRUE** |
| `"shiny" %in% loadedNamespaces()` après `source()` | ❌ **FALSE** — Shiny **n'est PAS** chargé |
| `ts_drive_read_ready()` (pas de fichier) | `NULL` — correct |
| `ts_drive_read_result()` | `NULL` — correct |
| `ts_drive_ready_age()` / `ready_fresh()` | `Inf` / `FALSE` — correct (aucune session) |
| `ts_drive_arm_state()` / `ts_drive_viewer()` | `list(armed, reason)` / `"headless"` |
| `ts_drive_job_state()` / `job_busy()` | `NULL` / `FALSE` |
| Corps des lecteurs citant `shiny::` | **FALSE** pour les 6 testés |

⇒ **Les lecteurs sont du R de base + `jsonlite` (déjà au lockfile), sans Shiny.** Le seul écrivain
réutilisable est `ts_drive_write_json(obj, path)` — atomique, borné (8 essais / budget **1,0 s**),
utilisé **côté app**. M1 **read-only n'en a pas besoin**.

🔴 **Piège d'instrument mesuré** : `ts_drive_allowlist_problems()` renvoie le **sentinelle `TRUE`**
quand tout va bien (`if (length(problems)) problems else TRUE`). Donc `length(...) == 1` signifie
**« cohérent »**, **pas** « 1 problème ». La sonde naïve l'a d'abord lu de travers. **Le bon test est
`isTRUE(...)`.** (Le contrôle tourne **au `source()`** — l. 288-292 — donc un `source()` réussi
**prouve** que l'allowlist est cohérente.)

---

## 7. Restrictions de chemin actuelles (mesurées)

`ts_drive_validate_import_path()` rend un **VERDICT** (`list(ok, path, reason)`), jamais un booléen —
parce qu'un agent à qui on répond `FALSE` **réessaie indéfiniment**.

Racines autorisées (`ts_drive_import_roots()`) :
1. la **racine du projet** ;
2. `tempdir()` ;
3. `TS_DRIVE_IMPORT_EXTRA_ROOTS` — **`character(0)`** (mesuré) ;
4. `TRANSCRIPTO_DRIVE_DATA_DIR` (séparateur de chemin) — **vide** par défaut.

Refus explicites : `..` refusé sur la **chaîne brute AVANT normalisation** (sinon la traversée
serait résolue et **masquée**) · chemin non résolvable · **hors de toute racine** · inexistant ·
**répertoire** (cas réel mesuré : `GSE164073_Eye_count_matrix.csv/` est un **dossier**).

---

## 8. Risques Windows (mesurés ou écrits)

| Risque | Mesure / origine | Impact MCP |
|---|---|---|
| `file.rename()` sur destination existante | **~5,1 s pour échouer** ⇒ 8 essais = **41,4 s** dans un battement ⇒ d'où `TS_DRIVE_WRITE_BUDGET_S = 1,0` | côté app ; M1 read-only **n'y touche pas** |
| **Bruit sur stdout** | à chaque démarrage : « One or more packages recorded in the lockfile are not installed » + **4** avertissements `LC_*` | 🔴 **réel sur stdio** : le transport MCP **est** stdout. Un `cat()` parasite peut **corrompre le framing JSON-RPC** ⇒ M1 doit router les diagnostics vers **stderr** et **flusher** explicitement |
| Espaces **et parenthèses** dans le chemin | `…/SHINYAPP test (git work)/SHINYAPP test/…` | slashs obliques OK (documenté) ; **jamais** de shell non quoté |
| `ERROR_PIPE_BUSY` (231) / `processx` | casse **`chromote`** sur cet hôte (tubes nommés) | **sans effet** : le MCP est **stdio**, il ne crée **aucun** tube nommé |
| Locale | `LC_ALL=C.UTF-8` (Git Bash) ⇒ 4 avertissements au démarrage | cosmétique, mais **pollue stdout** (cf. ci-dessus) |
| Avertissement renv | « packages recorded in the lockfile are not installed » | **item ouvert déclaré** — ne pas corriger ici |

---

## 9. Validation prévue (protocole · PID · token · started_at · battement · session)

Contrat **déjà gelé** (`docs/DRIVE_LIVE_CONTROL_PLAN.md` §2.1) — M1 **n'invente rien**, il **lit** :

| Champ | Valeur / usage |
|---|---|
| `protocol` | **`"ts-drive/1"`** (`TS_DRIVE_PROTOCOL`) — tout autre ⇒ **ignorer** |
| `pid` | PID du processus app — **doit être vivant** ; un `result.json` résiduel d'un **autre** PID n'est **pas** fiable |
| `session_token` | **8 caractères** ; **n'est PAS un secret** ⇒ M1 doit le **caviarder** (`ts_drive_badge_sanitize`) |
| `started_at` | ISO-8601 UTC — couple `(pid, started_at)` = identité de session |
| battement | `hb_n` réécrit toutes les **3 s** (`hb_interval`), **timeout 15 s** (`hb_timeout`) ; `ts_drive_ready_fresh()` |
| session **sélectionnée** | `app.R:539` `.drive_selected()` compare `ready.json$session_token` au token de l'instance ; « **last connected session wins** » |
| `viewer` | `headless` \| `visible` \| `unknown` |
| statuts | `ignored \| invalid \| applied \| running \| done \| error` (`TS_DRIVE_STATUSES`) |
| actions | `noop \| set_inputs \| run_pipeline \| import_file \| snapshot \| reset_module` |

🔴 **Deux règles opérationnelles à respecter par M1** :
1. **Un nouvel onglet ROTATIONNE le token** ⇒ **relire `ready.json` APRÈS** l'ouverture.
2. **Ne JAMAIS** conclure « session morte » de `ready_fresh() == FALSE` pendant un **job en vol**
   (job **synchrone** ⇒ le battement **meurt**) : `result.json:running` **est** la preuve de vie.

**Périmètre v1 du drive** : `TS_DRIVE_MODULES = import_bulk, bulk_filter, bulk_de, bulk_pathways`
(**4**) ; `TS_DRIVE_BUTTONS` = **5** ; `TS_DRIVE_ALLOWLIST` = **39** entrées. **`sc` et `spatial` sont
hors périmètre v1** (refusés par le garde d'allowlist).

---

## 10. Compatibilité (Viewer / Chrome visible / headless)

| Mode | Mécanisme | Compatibilité M1 |
|---|---|---|
| **RStudio Viewer** | httpuv, même session | ✅ lecture de fichiers, **identique** |
| **Chrome localhost visible** | `http://127.0.0.1:<port>`, même httpuv | ✅ **identique** — `ready.json.viewer` publie `visible` |
| **headless** | `launch.browser = FALSE` ; une session n'existe **qu'après** connexion d'un client | ✅ outils **read-only** ⇒ rien à démarrer |

🔑 **Les deux modes de visibilité ne sont qu'UN protocole** : seul le **client** change. M1 **n'a
donc aucun travail de compatibilité** : il lit les mêmes fichiers. Le mode **`visible`** est aussi le
chemin d'acceptation **indépendant de `chromote`** — donc le seul praticable sur cet hôte.

⚠️ Corollaire : `tools/_drive/*.json` doivent être **nettoyés avant** toute session (un `scenario.json`
périmé produit un `result.json` **trompeur**).

---

## 11. Outils M1 read-only **proposés** (aucun implémenté)

| Outil proposé | Source (réutilisée) | Rend |
|---|---|---|
| `drive_status` | `ts_drive_read_ready` + `ready_age`/`ready_fresh` | protocole, PID, port, `viewer`, `started_at`, âge, fraîcheur ; **token caviardé** |
| `drive_result` | `ts_drive_read_result` | `ack_seq`, `status`, `errors`, `warnings`, `applied_at` |
| `drive_snapshot` | `result.json.snapshot` (**fichier**) | `has_data`, `object_class`, `n_genes`, `n_samples`, états par module |
| `drive_job` | `ts_drive_job_state` / `busy` / `pending` | état du job en vol (preuve de vie) |
| `drive_allowlist` | `TS_DRIVE_ALLOWLIST` / `BUTTONS` / `MODULES` | inventaire des ids injectables **et** des boutons |
| `drive_validate_path` | `ts_drive_validate_import_path` | **verdict actionnable** sur un chemin candidat |
| `drive_protocol` | constantes | version de protocole + statuts/actions autorisés |
| `drive_badge` | `ts_drive_badge_model` / `badge_view` | état du badge (diagnostic) |

**Interdits par le mandat — M1 ne les expose PAS** : `eval`, `parse`, `source`, code R arbitraire,
`global_data`, **Seurat**, **DESeq2**, fonctions de **pathways**.
🔴 **M1 est strictement read-only** : **aucun** écrivain `arm.json` / `scenario.json`. Le passage à
l'écriture est un jalon **séparé et autorisé explicitement**, jamais un effet de bord de M1.

---

## 12. Risques de sécurité (constatés, non corrigés)

| # | Risque | Sévérité | Détail mesuré |
|---|---|---|---|
| S1 | **Token = identifiant, PAS un secret** | moyenne | 8 caractères (`ts_drive_new_token`) ; toute réponse M1 doit le **caviarder** |
| S2 | **`result.json` ne porte PAS de token** | moyenne | ⇒ confusion possible entre un résultat **frais** et un **résidu** ; M1 doit comparer **`pid` + `started_at`**, jamais le seul `status` |
| S3 | **Token d'armement `*` accepté** quand `Sys.getenv("TRANSCRIPTO_DEV_DRIVE") == "1"` | **élevée si l'env est posé** | ouvre l'armement à **n'importe quel** token ; à ne **jamais** laisser posé hors lancement de dev |
| S4 | **Containment d'import = préfixe de chaîne BRUT** | moyenne | `substr(full, 1, nchar(r)) == r` ⇒ `D:/proj-evil` **passe** si la racine est `D:/proj` (collision de **nom frère**) |
| S5 | **Aucune authentification sur le canal** | faible (local) | stdio **local** ; mais quiconque écrit `tools/_drive/*.json` peut **piloter** la session |
| S6 | **Surface de `btw` plus large que nécessaire** | moyenne | M1 devrait **retirer** le jeu `docs`+`pkg` : ce sont des outils **sans rapport** avec le projet, donc de la surface **gratuite** |
| S7 | **Nouveau fichier `scripts/*.R` deviendrait VERSIONNÉ** | faible | `scripts/mcp_server.R` est gitignoré, mais **`scripts/` ne l'est pas** ⇒ un fichier frère apparaîtrait dans `git status` |

---

## 13. Fichiers que M1 modifierait (**exacts**)

| Fichier | Action | Versionné ? | Contrainte |
|---|---|---|---|
| `scripts/mcp_server.R` | **ÉTENDRE** (remplacer le jeu btw par les outils projet ; handler JSON-RPC ; garder `--check`) | **gitignoré** (`.gitignore:43`) | conforme au mandat « étendre, ne pas dupliquer » |
| `scripts/mcp_tools.R` *(optionnel)* | **CRÉER** (définitions d'outils) | ⚠️ **VERSIONNÉ** (scripts/ non ignoré) | ferait apparaître un fichier dans `git status` |

### 🔴 Contraintes structurelles MESURÉES qui ferment des portes

1. **INTERDIT d'ajouter un fichier dans `R/`** : le garde **P0** `test-app-sourcing.R` exige que
   `app.R` **`source()` tout** fichier de `R/` — or le mandat **interdit de modifier `app.R`**.
   ⇒ Le code M1 **ne peut pas** vivre dans `R/`.
2. **C3 ne scanne PAS `scripts/`** : `check_c3_source_targets()` parcourt
   `c("app.R", "global.R", "R", "modules", "config")`. ⇒ un `source("scripts/mcp_tools.R")` dans le
   lanceur **ne déclenche pas** C3. **Mais** C3 exige que toute cible de `source()` soit **versionnée**
   — d'où la tension si un jour le lanceur sourçait un fichier **gitignoré**.
3. **INTERDIT de toucher `tests/`** ⇒ M1 **ne peut pas** être validé par un test unitaire classique.
   La vérification devra passer par une **session drive vivante** (mode `visible`).
4. **Aucune dépendance ajoutée** : pas de `mcptools`, pas de nouvelle entrée `renv.lock`.

**Non modifiés** (mandat) : `app.R` · modules Shiny · `R/core/drive_watcher.R` ·
`R/core/drive_allowlist.R` · `tests/` · `renv.lock` · `tools/check_conventions.R` (et tout autre garde).

---

## 14. Décisions NON tranchées (à trancher AVANT M1)

1. **Handler natif vs enveloppe `btw`** — le mandat dit « ne pas **ajouter** `{mcptools}` », mais le
   serveur **utilise déjà** `btw`/`mcptools`. **M1 conserve-t-il `btw` comme transport** en
   enregistrant des outils projet, **ou** implémente-t-il JSON-RPC **en R de base** ? Non tranché.
   ⚠️ Implémenter en base R = framing `Content-Length` + flush stdout **à la main** (§8).
2. **Emplacement du code** : tout dans `scripts/mcp_server.R` (1 fichier gitignoré, invisible au
   versionnage) **ou** un `scripts/mcp_tools.R` **versionné** ?
3. **Nom du serveur** : `transcriptoshiny-r-btw` contient « **btw** » — **trompeur** si M1 sert des
   outils projet. Le renommer casse les configs clients existantes (5 fichiers).
4. **Vérification sans `tests/`** : accepte-t-on une validation **uniquement** par session drive
   vivante (mode `visible`) ?
5. **Faut-il exposer `drive_allowlist`** (inventaire complet des ids) ? Utile, mais **élargit** la
   surface d'information de l'agent.
6. **Comportement sans session** : refuser de démarrer, ou servir des outils qui répondent
   `not_ready` ? (Recommandé : **servir**, et répondre `not_ready` — un serveur qui meurt est
   indiscernable d'un serveur cassé.)
7. **`--check`** : le rendre **indépendant** des dépendances (aujourd'hui il échoue si une dépendance
   manque, donc il ne peut pas diagnostiquer l'absence).
8. **Futur de `renv.lock`** : la posture bêta « hors lockfile » est **confirmée** (0 entrée) — mais
   elle implique qu'un **clone neuf n'a pas `mcptools`/`btw`/`ellmer`** et que le serveur **ne peut pas
   démarrer**. À assumer explicitement.

---

## 15. Déclaration finale

> **Aucun fichier du dépôt n'a été modifié.** Aucun fichier **suivi par git** n'a été touché ;
> `git status` affiche **uniquement** ` M renv.lock` (état **antérieur**, non modifié par cet audit) ;
> `HEAD` reste `9cb1a06`. Le **manifeste gelé est resté `451a7a4ceb0d9ef1…`** (437 fichiers) —
> **re-vérifié après chaque commande** de cet audit. Le présent rapport est **gitignoré**
> (`.workbuddy-ai/`) et **hors** du manifeste gelé. **Aucun outil MCP n'a été implémenté. Aucun test
> n'a été lancé.** M0 s'arrête ici ; **M1 n'est pas commencé** et attend une autorisation explicite.

### Artefacts de preuve (tous dans `.workbuddy-ai/freeze/`)
`probe-mcp-reuse.R` / `.log` · `probe-allowlist.log` · `probe-mcp-check.log` · `probe-mcp-guard.log` ·
`manifest-milestone-start.txt` (gel de départ) · manifeste de porte `/tmp/manifest-m0-gate.txt`.

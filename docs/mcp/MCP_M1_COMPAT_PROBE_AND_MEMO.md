# MCP M1 — Compatibility probe + decision memo (A vs B)

**Date** : 2026-09-24 (~17:0x, Europe/Paris)
**Mandat** : sonde de compatibilité **read-only** avant M1, puis mémo de décision. **Aucune**
implémentation. **Aucun** fichier applicatif / test / `renv.lock` / watcher / allowlist / garde /
manifeste gelé modifié.
**Livrable gitignoré** : `.workbuddy-ai/` (hors manifeste gelé).

---

## 0. Portes d'entrée et intégrité

| Contrôle | Valeur |
|---|---|
| Manifeste **avant** la sonde | `451a7a4ceb0d9ef1…` |
| Manifeste **après** la sonde | `451a7a4ceb0d9ef1…` |
| Verdict | ✅ **PASS — gel inchangé** |
| `HEAD` | `9cb1a06` (3 commits non amendés) |
| `git status` | ` M renv.lock` **seul** |
| `tools/_drive/` | `README.md` **seul** (aucun JSON créé) |

**Sessions stdio réelles exécutées** : 6. Toutes terminées **exit 0**, **0 fichier écrit** dans le
dépôt. Preuves dans `.workbuddy-ai/freeze/` : `rpc-out2.txt`, `rpc-err-out.txt`, `rpc-toolerr-out.txt`,
`rpc-v2-out.txt`, `rpc-clean-out.txt`, `split-stdout.txt` / `split-stderr.txt`, `native-result.txt`,
`probe-btw-api.log`, `probe-btw-src.log`.

---

## 1. Résultats de la sonde (Q1 → Q5)

### Q1 — btw peut-il n'enregistrer QUE `transcripto_drive_status` + `transcripto_drive_read_result` ?

## ❌ **NON** — pas via `btw`

**Chaîne mesurée dans le source** :

```r
btw::btw_mcp_server <- function(tools = NULL) {
  ...
  tools <- tools %||% btw_mcp_tools()
  tools <- flatten_and_check_tools(tools)
  mcptools::mcp_server(tools = tools)      # <-- AUCUN session_tools transmis
}
mcptools::mcp_server <- function(tools = NULL, ..., type = c("stdio","http"),
                                 host = "127.0.0.1", port = ..., session_tools = TRUE)
```

🔴 **`session_tools = TRUE` est le défaut, et `btw_mcp_server()` ne l'expose PAS.** mcptools injecte
donc **2 outils de session** quoi qu'on passe.

**Preuve par le fil** : `tools/list` a rendu **12** outils alors que le lanceur n'en passe que **10** :

```
[1..10] btw_tool_docs_* (5) + btw_tool_pkg_* (5)
[11]    list_r_sessions        <-- injecté par mcptools
[12]    select_r_session       <-- injecté par mcptools
```

⇒ **Minimum atteignable via btw = 2 + 2 = 4 outils**, jamais 2.
⇒ Pour obtenir **exactement 2**, il faut appeler `mcptools::mcp_server(tools = …, session_tools = FALSE)`
**directement** — donc **sortir de btw**.

### Q2 — btw garde-t-il stdout strictement cadré MCP ?

## ❌ **NON aujourd'hui — MAIS le défaut n'est PAS imputable à btw, et il est CORRIGEABLE en 1 ligne**

Mesure par **séparation des flux** (`1>` / `2>`) :

| Flux | Contenu mesuré |
|---|---|
| **stdout** (= le transport MCP !) | 🔴 **`- One or more packages recorded in the lockfile are not installed.`** + **`- Use renv::status() for more details.`** — **AVANT** la 1ʳᵉ trame |
| **stderr** | les 4 avertissements `LC_*` (inoffensifs : hors transport) |
| **erreurs** | ✅ **cadrées** : méthode inconnue → `{"error":{"code":-32601,"message":"Method not found"}}` |
| **JSON malformé** | ✅ **silence** — aucune sortie non cadrée |

Classification ligne à ligne du stdout réel :
```
  POLLUT : - One or more packages recorded in the lockfile are not installed.
  POLLUT : - Use `renv::status()` for more details.
  JSON   : {"jsonrpc":"2.0","id":1,"result":{...}}
  JSON   : {"jsonrpc":"2.0","id":2,"result":{"tools":[...]}}
```

**Cause identifiée** : le **contrôle de synchronisation renv** (lockfile ↔ bibliothèque), déclenché par
`source("renv/activate.R")` en `.Rprofile:1`. Il écrit sur **stdout**, donc **dans le transport**.

✅ **Correctif MESURÉ** : `RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE` ⇒ stdout **100 % JSON** :

```
--- stdout classified (avec le correctif) ---
  JSON   : {"jsonrpc":"2.0","id":1,"result":{"protocolVersion":"2024-11-05",...
  JSON   : {"jsonrpc":"2.0","id":2,"result":{"tools":[...]}}
  (0 ligne POLLUT — 21 592 octets, 2 trames)
```

🔑 **Ce correctif est au niveau du LANCEUR, pas de btw** ⇒ **il est requis pour A *et* pour B.**
⚠️ Il désactive la détection de dérive lockfile **pour le processus serveur uniquement** — pas pour l'app.

### Q3 — btw peut-il rendre des erreurs structurées avec les codes demandés ?

## ⚠️ **PARTIELLEMENT — `isError` oui, les CODES non (le schéma MCP ne les a pas)**

Source de `as_tool_call_result()` (lu) :
```r
is_error <- FALSE
if (inherits(result, "ellmer::ContentToolResult")) {
  is_error <- !is.null(result@error)      # <-- seul moyen de poser isError:true
}
... jsonrpc_response(data$id, drop_nulls(list(content = ..., structuredContent = ..., isError = is_error)))
```
Source de `execute_tool_call()` (lu) :
```r
tryCatch(as_tool_call_result(data, do.call(data$tool, args)),
  error = function(e) jsonrpc_response(data$id, error = list(code = -32603, message = conditionMessage(e))))
```

| Situation | Réponse MCP mesurée |
|---|---|
| outil rend normalement | `{"result":{"content":[…],"isError":false}}` |
| outil **lève** | `{"error":{"code":-32603,"message":"…"}}` — **JSON-RPC**, **sans code métier** |
| outil rend un `ellmer::ContentToolResult` avec `@error` | `{"result":{…,"isError":true}}` |

🔴 **Le schéma MCP d'un résultat d'outil n'a PAS de champ `code`** — seulement un booléen `isError`.
⇒ `NO_SESSION` / `STALE_SESSION` / `RESULT_SESSION_MISMATCH` / `INVALID_PROTOCOL` / `READ_FAILED` ne
peuvent PAS être des codes de protocole ; ils doivent voyager **DANS le contenu** (bloc texte JSON, ou
`structuredContent`). mcptools **émet** `structuredContent` quand l'outil en fournit.

**Bonne nouvelle de protocole** : la négociation fonctionne — demander `2025-06-18` rend `2025-06-18`
(mesuré). `ellmer::ContentToolResult` est une classe **S7** dont la propriété `error` accepte
`NULL | character | S3<condition>` ⇒ un `isError:true` **est** constructible.

**Coût selon l'option** : en **A**, il faut construire des `ellmer::ContentToolResult` (API ellmer) ;
en **B**, on écrit le JSON soi-même ⇒ contrôle total de l'enveloppe.

### Q4 — les outils `docs`/`pkg` peuvent-ils être désactivés/exclus ?

## ✅ **OUI, trivialement** — mais les outils de **session**, non

- `btw_tools("docs","pkg")` est **opt-in** : ne pas les passer suffit. ✅
- Mécanismes d'exclusion **supplémentaires** repérés dans mcptools : `ignore_tools` (vecteur de
  caractères) et `mcp_ignore_tool_pattern_regex()` (**supporte `*`**). ✅
- ⚠️ Mais les **2 outils de session** ne sont retirables **que** par `session_tools = FALSE`, **non
  exposé par btw** (cf. Q1).
- 🔴 **Canal latéral** : `btw_tools()` appelle `custom_agent_discover_tools()` — il **découvre et
  fusionne** des outils depuis des fichiers `agent-*.md`. Des outils **non déclarés** peuvent donc
  entrer dans l'inventaire.
- 🔴 **Danger latent mesuré** : `btw_tools()` **sans argument** expose **36** outils, dont
  **`btw_tool_run_r`** (exécution de code R arbitraire) et `btw_tool_files_write`/`edit`. Le lanceur
  actuel n'en expose que 10 (docs+pkg) — mais un appel malencontreux ouvrirait l'exécution de code,
  précisément ce que le mandat interdit.

### Q5 — compatibilité Windows et fonctionnement stdio préservés ?

## ✅ **OUI — MESURÉ sur 6 sessions réelles**

| Test | Résultat |
|---|---|
| `initialize` (2024-11-05) | ✅ réponse JSON valide, `serverInfo.name = "R mcptools server"` |
| `initialize` (2025-06-18) | ✅ version **négociée** à `2025-06-18` |
| `tools/list` | ✅ 12 outils, trame unique valide |
| `tools/call` nominal | ✅ `{"content":[…],"isError":false}` |
| `tools/call` en erreur | ✅ `{"error":{"code":-32603,…}}` cadré |
| méthode inconnue | ✅ `-32601` cadré |
| JSON malformé | ✅ ignoré, **aucune** pollution |
| exit code | ✅ **0** |

⚠️ **Contraintes/risques Windows relevés** :
1. `mcptools::mcp_server()` commence par **`check_not_interactive()`** ⇒ **abandonne si la session est
   interactive** (un serveur MCP doit être non-interactif — OK sous `Rscript`, à documenter).
2. Le chemin **stdio** appelle tout de même **`nanonext::reap(the$session_socket)`** et, si
   `session_tools = TRUE`, **`ensure_socket_dir(socket_dir_in_use())`** ⇒ **nanonext est sollicité même
   sans HTTP**.
3. `mcptools` **Imports** : `cli, ellmer, httpuv, httr2, jsonlite, nanonext, openssl, processx,
   promises, rlang, yaml` ⇒ **`processx`/`httpuv`** — la famille exacte dont les **tubes nommés** sont
   cassés sur cet hôte (chromote). Le chemin **stdio a fonctionné**, mais c'est une **dette
   d'environnement**.
4. La pollution **stdout** (Q2) est le **seul défaut réellement bloquant** — et il est corrigeable.

### Bonus mesuré — le chemin **natif** (option B) tient sans renv ni mcptools

```
jsonlite available: TRUE 2.0.0
shiny loaded    : FALSE
mcptools loaded : FALSE
btw loaded      : FALSE
nanonext loaded : FALSE
```
En injectant **directement** `.libPaths()` (bibliothèque projet), on obtient **`jsonlite` seul** —
**aucune** activation renv, **aucun** mcptools/btw/nanonext.
⚠️ Piège payé : le 1ᵉʳ essai en `--vanilla` a **segfaulté (139)** et a **perdu son stdout bufferisé** —
la preuve a été obtenue en **écrivant dans un fichier** depuis R (jamais stdout).

---

## 2. Mémo de décision — A vs B

**A** = étendre le transport btw existant (`btw_mcp_server(tools = …)`).
**B** = remplacer la couche de dispatch par un **handler JSON-RPC natif minimal** dans
`scripts/mcp_server.R` (base R + `jsonlite`).

| # | Critère | **A — transport btw** | **B — handler natif** |
|---|---|---|---|
| 1 | **Correction du protocole** | ⚠️ mcptools est correct (trames vérifiées, version négociée) **mais** stdout pollué par renv ; `btw_tools()` a un canal latéral `agent-*.md` | ✅ Contrôle total : on n'écrit **que** des trames ; tout le reste → stderr. Coût : implémenter le framing + `flush()` soi-même |
| 2 | **Minimisation des outils exposés** | ❌ **ÉCHEC** — `session_tools=TRUE` non exposé par btw ⇒ **+2 outils inamovibles** (min. 4) | ✅ **Exactement 2** outils, rien d'autre |
| 3 | **Erreurs structurées** | ⚠️ `isError` OK via `ellmer::ContentToolResult` ; **aucun champ `code`** ; codes ⇒ dans le contenu | ✅ Enveloppe écrite à la main : `isError:true` **+** `structuredContent.code` normalisé |
| 4 | **Compatibilité Windows** | ⚠️ fonctionne (mesuré) ; mais `nanonext` + `processx`/`httpuv` sollicités ; `check_not_interactive()` | ✅ base R + `jsonlite` ; **aucun** tube nommé ; risque = `flush()`/CRLF à soigner |
| 5 | **Risque de dépendance** | 🔴 **ÉLEVÉ** — btw → mcptools → `{ellmer, nanonext, httpuv, httr2, openssl, processx, promises, yaml, cli, rlang}` ; **0** de ces paquets n'est dans `renv.lock` | ✅ **`jsonlite` seul**, **déjà dans `renv.lock`** (mesuré : disponible sans renv, sans mcptools/btw) |
| 6 | **Maintenabilité** | ⚠️ on hérite de l'API **large et mouvante** de btw (groupes, alias, découverte `agent-*.md`) + de ses bugs | ✅ ~200 lignes lisibles, **dans le dépôt**, dont on est propriétaire ; coût = suivre les révisions MCP soi-même |
| 7 | **Clients headless futurs** | ✅ stdio **et** http disponibles (`type=`, `host=`, `port=`) | ✅ stdio par défaut ; négociation de version à implémenter (≈10 lignes) |

### Verdict

**La préférence provisoire est CONFIRMÉE : retenir B.**

Parce que **A échoue franchement sur 2 des 7 critères** :
- **critère 2** — btw **ne peut pas** retirer les 2 outils de session (`session_tools` non exposé) ⇒
  l'inventaire minimal exigé est **inatteignable via btw** ;
- **critère 5** — btw entraîne **10 paquets hors `renv.lock`** (dont `nanonext`, `processx`, `httpuv`).

et qu'il **n'atteint pas pleinement** le critère 3 (pas de champ `code`) ni le critère 1 (pureté stdout
non garantie par btw).

**Argument décisif supplémentaire, MESURÉ** : **B élimine la cause racine de la pollution stdout**.
En n'activant **pas** renv et en injectant `.libPaths()` directement, il n'y a **plus de contrôle de
synchronisation renv** ⇒ **plus de sortie parasite**, sans même recourir au correctif
`RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE`. B est donc **propre par construction**, pas par rustine.

⚠️ **Réserve honnête sur B** : implémenter JSON-RPC à la main déplace le risque *vers* notre code
(framing `Content-Length`, `flush`, CRLF, gestion d'`id`). Ce risque est **borné** (~200 lignes, base R)
et **testable** par le même harnais stdio utilisé ici.

### Requis quelle que soit l'option

1. **Pureté stdout** : ne jamais écrire hors trames (⇒ `RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE` en A ;
   pas d'activation renv en B).
2. **Aucun outil de code/exécution** : jamais `btw_tool_run_r`, `btw_tools()` sans argument, ni
   `eval`/`parse`/`source`/`global_data`/Seurat/DESeq2/pathways.
3. **Codes métier dans le contenu** (`structuredContent`), jamais supposés portés par le protocole.
4. **Placement** : bêta **gitignoré sous `scripts/`** ; **aucun** fichier versionné sans autorisation
   explicite ; **rien** dans `R/` (P0 `test-app-sourcing.R`).

---

## 3. Décisions restant ouvertes (avant M1)

1. **A vs B** — ce mémo recommande **B** ; décision utilisateur requise.
2. **Emplacement exact** : tout dans `scripts/mcp_server.R` (1 fichier gitignoré) **ou** un
   `scripts/mcp_tools.R` (**deviendrait versionné**).
3. **Nom du serveur** : `transcriptoshiny-r-btw` devient **trompeur** si btw est retiré (5 configs
   clients à mettre à jour).
4. **Vérification sans `tests/`** : accepte-t-on une validation **uniquement** par sessions stdio
   scriptées (harnais de cette sonde) + une session drive `visible` ?
5. **`--check`** : le rendre **indépendant** des dépendances (aujourd'hui il échoue si une dépendance
   manque).
6. **Sort de `mcptools`/`btw`/`ellmer`** : restent installés (bêta, hors lockfile) ou à désinstaller
   après B ?
7. **Comportement sans session** : refuser de démarrer, ou servir et répondre `NO_SESSION` ?
   (Recommandé : **servir** — un serveur qui meurt est indiscernable d'un serveur cassé.)

---

## 4. Déclaration finale

> **Aucun fichier du dépôt n'a été modifié.** Le **manifeste gelé est resté
> `451a7a4ceb0d9ef1…`** — recalculé **avant ET après** la sonde (437 fichiers, comparaison `cmp`
> OK). `git status` = ` M renv.lock` **seul** (état antérieur) ; `HEAD` = `9cb1a06`.
> **Aucun** fichier applicatif, test, `renv.lock`, watcher, allowlist, garde ou manifeste touché.
> **Aucun** outil MCP implémenté. **Aucun** `mcptools` ajouté. **Aucun** second serveur créé.
> Le présent mémo est **gitignoré** (`.workbuddy-ai/`) et **hors** du manifeste gelé.
> **M1 n'est pas commencé** — il attend une autorisation explicite.

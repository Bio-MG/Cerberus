# MCP M1 — Integration fix report (two live client configurations)

**Date** : 2026-09-24 (~18:0x) · **Portée** : exactement **2** fichiers de configuration · **M2 NON commencé**

---

## 1. Pre-edit gate

| Contrôle | Valeur |
|---|---|
| Manifeste recalculé **avant édition** | **`c59ab42c4fc1d226…`** |
| Valeur **exigée** | `c59ab42c4fc1d226…` |
| Verdict | ✅ **PASS** (437 fichiers) |

## 2. Cibles résolues et affichées

| Cible | Chemin réel | Dans le dépôt ? | Gitignoré ? | Dans le manifeste ? | Taille avant |
|---|---|---|---|---|---|
| `.zcode/config.json` | `D:/Data_science/SHINYAPP test (git work)/SHINYAPP test/.zcode/config.json` | **OUI** (sous la racine du projet) | oui (`.gitignore:39`) | **oui** (1 entrée) | 432 o |
| `~/.workbuddy-ai/mcp.json` | `C:/Users/marcg/.workbuddy-ai/mcp.json` | **NON — hors du dépôt** | (sans objet) | **non** (0 entrée) | 234 o |

`.zcode/` compte **4** entrées au manifeste (`config.json` + **3** `plans/*.md` non touchés).

### ✅ Confirmation demandée : `~/.workbuddy-ai/mcp.json` est bien la configuration cliente ACTIVE

Preuves :
1. **Emplacement documenté** : `mcp.examples/workbuddy_template.json` déclare *« Target file:
   `%USERPROFILE%\.workbuddy-ai\mcp.json` (NOT `.mcp.json`) »* — c'est **exactement** ce chemin.
2. **Le fichier existe** et porte l'entrée `transcriptoshiny-r-btw` pointant sur notre script.
3. **L'alternative n'existe pas** : `~/.workbuddy-ai/.mcp.json` est **absent** (mesuré).
4. **Hors du dépôt** : chemin dans le profil utilisateur ⇒ il **ne peut pas** figurer au manifeste
   (vérifié après édition : **0** entrée).

## 3. Modifications (2 insertions d'UNE ligne chacune)

Méthode : édition **au niveau octet** — les deux fichiers sont en **CRLF sans BOM** (mesuré), donc une
édition textuelle naïve aurait pu casser les fins de ligne. L'insertion reprend l'**indentation** de la
ligne voisine et **rien d'autre** n'est touché.

```diff
 .zcode/config.json
         "args": [
+          "--no-init-file",
           "D:/Data_science/SHINYAPP test (git work)/SHINYAPP test/scripts/mcp_server.R"
         ],

 ~/.workbuddy-ai/mcp.json
       "args": [
+        "--no-init-file",
         "D:/Data_science/SHINYAPP test (git work)/SHINYAPP test/scripts/mcp_server.R"
       ]
```

**Préservés à l'identique** : forme de la commande · chemin **absolu** du Rscript portable
(`D:/Data_science/R-4.4.2/bin/Rscript.exe`) · `cwd` du projet (zcode) · `type`, `env`, `enabled`,
`timeoutMs` (zcode) · indentation · **CRLF** · aucune clé ajoutée ni retirée.
✅ Le **workaround d'environnement n'a PAS été utilisé** : le schéma autorise `args`, donc la
préférence exprimée (arguments plutôt que `env`) a été appliquée.

**Sauvegardes** : `.workbuddy-ai/freeze/cfgfix-zcode-before.json` et `cfgfix-workbuddy-before.json`
(sha256 d'origine : `.zcode` = `9c68f884…`, externe = `71d0ed66…`).

## 4. Vérifications après édition

| # | Vérification | `.zcode/config.json` | `~/.workbuddy-ai/mcp.json` |
|---|---|---|---|
| 1 | **JSON valide** | ✅ | ✅ |
| 2 | `command` (chemin absolu préservé) | ✅ `…/Rscript.exe` | ✅ `…/Rscript.exe` |
| 3 | `args[0] == "--no-init-file"` | ✅ | ✅ |
| 4 | **Fins de ligne CRLF** | ✅ 18 lignes CR, **0** LF seul | ✅ 11 lignes CR, **0** LF seul |
| 5 | `--check` **via la commande configurée** | ✅ exit **0**, **stdout 0 o** | ✅ exit **0**, **stdout 0 o** |
| 6 | **EOF propre** via la commande configurée | ✅ exit **0**, **stdout 0 o** | ✅ exit **0**, **stdout 0 o** |
| 7 | **Flux MCP valide** | ✅ `STREAM_PURE=True` (2 trames) | ✅ `STREAM_PURE=True` (2 trames) |
| 8 | Outils exposés | ✅ les **2** attendus | ✅ les **2** attendus |

Commande effective réellement exécutée (extraite du JSON, `cwd` honoré) :
```
D:/Data_science/R-4.4.2/bin/Rscript.exe --no-init-file
  D:/Data_science/SHINYAPP test (git work)/SHINYAPP test/scripts/mcp_server.R
```
Inventaire servi : `['transcripto_drive_status', 'transcripto_drive_read_result']` — **2**, inchangé.

## 5. Manifeste après édition

| | Valeur |
|---|---|
| **Avant** | `c59ab42c4fc1d226…` — **437** fichiers |
| **Après** | **`40f300bd3b28d587…`** — **437** fichiers |
| **Delta exact** | **`.zcode/config.json`** — et **rien d'autre** |
| Configuration externe | **0** entrée au manifeste ✅ (hors dépôt ⇒ sans effet, comme prévu) |
| `git status` | ` M renv.lock` **seul** (état antérieur, non touché) |
| `HEAD` | `9cb1a06` (inchangé) |

⇒ Conforme à l'attendu : **le manifeste du dépôt change parce que `.zcode/config.json` en fait
partie** ; le fichier externe **n'a aucun effet** sur le manifeste.

## 6. Déclaration finale

> **Exactement 2 fichiers modifiés**, tous deux des **configurations de lancement** — `.zcode/config.json`
> (dans le dépôt, gitignoré) et `~/.workbuddy-ai/mcp.json` (hors dépôt). **Aucun** fichier suivi par git
> n'a changé. **Non touchés**, conformément à la consigne : `mcp.examples/*`, `mcp.examples/README.md`,
> `AGENTS.md`, `docs/archive/STATUS_JOURNAL.md`, `ROADMAP.md`, `STATUS.md`, `app.R`, les modules, les
> fichiers drive, `tests/`, `renv.lock`, les gardes de convention. **M2 n'est pas commencé.**

### Reste ouvert (hors périmètre de ce lot, non exécuté)
Les **exemples** (`mcp.examples/*.json`, `README.md`) et `AGENTS.md:98` portent encore l'ancienne
commande — ce sont des **gabarits de documentation**, aucun code ne les lit, et ils ne bloquent pas
l'usage. `docs/archive/STATUS_JOURNAL.md` reste **volontairement** inchangé (archive).

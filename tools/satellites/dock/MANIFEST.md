# MANIFEST.md — N3 `biomentat-dock` : format figé du manifeste des satellites

**Statut : FIGÉ v1.0.0** (contract-first — fiche
`docs/proposals/PROPOSAL_N_SATELLITES_LAUNCH_ROADMAP_2026-10-26.md` §2-S7 :
« Manifest first (frozen format) »). Toute ÉVOLUTION = version `1.x` nouvelle,
jamais silencieuse (règle d'évolution FORMAT v1.0.0 du satellite S4/inspector) ;
la version porte aussi les fichiers : `dock_manifest.json` (`format_version`) et
chaque `capabilities/<satellite>.json` (`capability_version`).

---

## 1. Objet et règles non négociables

Le dock est l'**hôte des satellites** (N3) : il connaît, par manifeste, comment
démarrer / sonder / arrêter chaque satellite. Trois règles PORTÉES PAR LA FICHE
(§2-S7) et par ce contrat :

1. **Le dock ne démarre JAMAIS rien sans la commande explicite de
   l'opérateur.** Pas de watcher, pas de daemon, pas de démarrage au boot de
   l'app : le dock est un CLI (`run_dock.R`) dont chaque verbe est un geste
   posé (`--start`, `--health`, `--status`, `--stop`). Sans verbe : usage,
   rien d'autre.
2. **Un process par satellite (O6 intact).** Aucun satellite ne partage
   JAMAIS le process de l'app (`capabilités[].process.shares_app_process =
   false`, verrouillé au validateur) ; le dock ne partage pas davantage les
   process entre satellites (un pid-file par satellite, un port par
   satellite — unicité verrouillée au validateur).
3. **Un satellite vit sous `tools/satellites/<name>/`** et n'est jamais
   démarré par l'application : le canal reste externe à l'app (l'app ne sait
   même pas que le dock existe).

Cécité de garde **assumée** (modèle S1/S5, note reprise dans le README) : le
code R du dock vit sous `tools/` — la conventions gate ne le voit **pas par
construction** ; la contrepartie est la minimisation du R (lib pure + CLI fin)
et la validation par `tests/testthat/test-biomentat-dock.R` + falsification live.

## 2. `dock_manifest.json` — schéma d'entrée (figé)

Un seul objet JSON, champ par champ — **tout autre champ top-level est refusé**
(frozen format, le validateur reject les inconnus) :

| Champ | Type | Règle |
|---|---|---|
| `format_version` | string | DOIT valoir `"1.0.0"` (figé ; autre valeur = refus honnête, jamais interprétée) |
| `satellites` | array (≥1) | un objet par satellite, champs ci-dessous |

Chaque **satellite** :

| Champ | Type | Règle |
|---|---|---|
| `name` | string | unique ; `[a-z0-9_]+` (utilisé tel quel dans les chemins d'état) |
| `command` | array de strings (≥1) | argv complet ; `command[1]` = exécutable ; le port déclaré DOIT figurer dans la ligne (cohérence verrouillée au test) |
| `cwd` | string | répertoire de travail du process, relatif à la racine du dépôt (`"."` = racine — disposition mesurée de watchtower : le serveur statique doit tourner à la racine pour servir page ET JSON en same-origin) |
| `port` | entier 1..65535 | unique entre satellites ; le port du process, jamais un port de l'app |
| `health` | object | `{url, expect_status, timeout_s, interval_s}` — `url` http(s) absolu ; `expect_status` entier (200 pour un serveur statique) ; `timeout_s` > 0 ; `interval_s` > 0 |
| `capability` | string | chemin (relatif au dock) du fichier `capabilities/<name>.json`, DOIT exister |
| `enabled` | booléen | `false` = connu mais non démarrable (`--start` refuse honnêtement) |
| `notes` | string | libre, non interprété |

## 3. `capabilities/<name>.json` — schéma capability (figé, aligné plugin-sdk)

La fiche §2-S7 : « capability.json shape aligned with #55 plugin-sdk's
authoring contract ». Le contrat d'authoring retenu (v1, minimal et falsifiable)
— un capability décrit CE QUE le satellite expose et CE QUI le porte :

| Champ | Type | Règle |
|---|---|---|
| `capability_version` | string | `"1.0.0"` (figé) |
| `satellite` | string | = `name` du manifeste (cohérence verrouillée au test) |
| `satellite_version` | string | version du satellite lui-même |
| `role` | string | une phrase : à quoi sert le satellite (rattachement décision/lien) |
| `surface.kind` | string | `"static_page"` \| `"r_script"` (vocabulaire fermé v1) |
| `surface.entry` | string | point d'entrée servi (`/tools/satellites/<name>/index.html` pour une page) |
| `surface.reads` | array de strings | chemins du dépôt lus (lecture seule revendiquée) |
| `surface.writes` | array | DOIT être vide en v1 (les satellites du lot sont read-only ; un satellite écrivain = évolution v2) |
| `process.kind` | string | `"static_http_server"` \| `"r_process"` (vocabulaire fermé v1) |
| `process.owner` | string | `"dock"` — seul le dock démarre/arrête le process (jamais l'app) |
| `process.shares_app_process` | bool | DOIT être `false` (règle O6, verrouillée au validateur) |
| `contract_refs` | array de strings | fichiers de contrat/README que le capability engage |

## 4. Verbes du CLI `run_dock.R` (le dock n'est QUE ces verbes)

```
Rscript tools/satellites/dock/run_dock.R --start  <name>   # démarre (commande explicite), attend la santé, écrit le pid-file
Rscript tools/satellites/dock/run_dock.R --health <name>   # sonde la santé (verdict OK/FAIL/TIMEOUT + détail)
Rscript tools/satellites/dock/run_dock.R --status          # tableau : pid/port/santé par satellite (rien ne démarre)
Rscript tools/satellites/dock/run_dock.R --stop   <name>   # arrêt propre : kill de l'arbre du process + retrait du pid-file
Rscript tools/satellites/dock/run_dock.R                   # usage seul — AUCUNE action
```

- `--start` refuse (exit 2) si : manifeste invalide, satellite inconnu,
  `enabled = false`, déjà démarré (pid vivant ou santé déjà OK — jamais un
  doublon de process).
- `--stop` sur un satellite non démarré : no-op annoncé (exit 0), jamais une
  erreur.
- Le pid-file vit dans `tools/_drive/satellites/<name>.pid` (répertoire
  gitigné — état runtime, jamais commité) ; il porte `pid`, `started_at`,
  `command` (echo), `manifest_format_version`.
- Exit codes : `0` succès · `1` verdict santé FAIL/TIMEOUT (probe) · `2`
  refus/refus d'usage (manifeste invalide, satellite inconnu, déjà démarré,
  verbe manquant).

## 5.Premier locataire : N1 watchtower (dogfood)

`dock_manifest.json` shippe **UN** locataire, `watchtower`, dont la commande est
l'instruction de service EXACTE mesurée par S1
(`tools/satellites/watchtower/README.md` § « Instruction de service EXACTE ») :
`python -m http.server 4892` lancé à la racine du dépôt (`cwd: "."`) — le même
port sert la page ET les JSON drive en same-origin. Santé = HTTP 200 sur la
page. La falsification live du dock (start → health → status → stop → health
FAIL) est décrite dans le README du dock.

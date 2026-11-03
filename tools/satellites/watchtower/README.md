# N1 `drive-watchtower` — le tableau de bord humain du canal drive

Satellite **statique** (un `index.html` + un `app.js`, vanilla, **zéro dépendance,
zéro CDN**) qui rend visible, pour un **humain**, l'état de `tools/_drive/` :
identité de session, fraîcheur du heartbeat, dernier verdict, scénario en attente.

**Lecture seule, absolue.** La page ne fait que des `fetch` GET sur trois fichiers
JSON ; elle n'écrit jamais dans le répertoire drive, ne touche à aucun processus R,
et n'est jamais chargée par l'application.

---

## Objet et rattachement — O11/SW-5c, le complément humain du même canal

- **O11** (décision tranchée 2026-10-25, option A) a été **livrée en SW-5c M2
  (`fe47823`, 2026-10-26)** : l'outil **agent-facing** `transcripto_drive_state`
  (« the agent can't see where the human is ») —
  `docs/proposals/DRIVE_PASSIVE_STATE_PROPOSAL.md:3` et `:84`.
- **N1 est le complément : la vue HUMAINE du même canal.** Source :
  `docs/ideation/NEW_SATELLITES_BROWSER_2026-10-26.md:16-39` (« N1 is the
  **complement**: the HUMAN-facing view of the same channel. Not a duplicate —
  the two halves of one observability story ») ; fiche de lancement :
  `docs/proposals/PROPOSAL_N_SATELLITES_LAUNCH_ROADMAP_2026-10-26.md`, §1 ligne N1,
  §2 slice S1.
- Le canal consommé est le contrat **Live Control Protocol** documenté dans
  `tools/_drive/README.md` (fichiers, heartbeat, statuts, job long). La page ne
  remplace pas `transcripto_drive_state` (l'agent) ni le badge in-app : elle
  donne à l'opérateur ce que l'agent a déjà, sans devoir « tailer » les JSON à la
  main.

## Pourquoi ce satellite existe : leçon 537 s

Un job bulk synchrone **bloque la boucle Shiny** pendant toute sa durée : le
heartbeat de `ready.json` **cesse**, alors que le travail continue. Mesuré :
`bulk_de` répond `done` en **2,1 s** et le résultat n'est observable que **852,7 s**
plus tard ; une autre classe de job a gelé l'écran **537 s** (fiche N7, §0 de la
fiche de lancement). Le contrat l'assume explicitement :
`result.json` portant `status: "running"` est **la preuve de vie que le heartbeat
ne peut pas donner** pendant cette fenêtre (`tools/_drive/README.md`, §
« RESIDUAL » : *« it is what separates "alive and busy" from "dead" »*).

D'où la **règle de rendu normative** de la slice S1 : heartbeat gelé +
`result.json` portant `running` ⇒ le bandeau affiche **« JOB IN FLIGHT »**,
**JAMAIS « DEAD »** (ni « MORT » — aucun bandeau n'est jamais ÉTIQUETÉ ainsi ; la seule occurrence du mot est la négation du texte `why` de l'état flight, app.js).

## Note de cécité de garde (assumée)

Cette page contient **zéro logique R** : la conventions gate (exigence
« 0 erreur ») voit `modules/` et l'arbre du package — pas `tools/`, et encore
moins du HTML/JS statique. Le satellite échappe donc **par construction** au
gate, comme tout code satellite sous `tools/satellites/` (fiche §2, *context* :
« satellite R code under tools/satellites/ escapes it by construction »). Ce
choix est **assumé et conforme à la fiche** (« No conventions-gate exposure (no
R logic) ») : il n'y a **rien à gating** — aucune dépendance, aucun rendu R,
aucun test testthat applicable. La contrepartie assumée : la validation tient à
la **falsification manuelle** ci-dessous (3 états de fixture) et à la revue du
bloc pur de `app.js`, pas à un gate automatique. `check_writers` n'est pas non
plus concerné : la page ne déclare aucun writer et n'écrit rien.

## Instruction de service EXACTE (mesurée)

Depuis la **racine du dépôt** (`SHINYAPP test/`), et pas ailleurs :

```bash
python -m http.server 4892
```

puis ouvrir :

```
http://localhost:4892/tools/satellites/watchtower/index.html
```

C'est la disposition qui marche, **mesurée** (curl 200 sur la page et sur les
JSON, voir § Falsification) :

- le serveur est lancé **à la racine du dépôt**, donc le même port sert **la page
  ET les JSON en same-origin** — condition requise pour que `fetch` fonctionne
  (`file://` est insuffisant : fetch y est bloqué par CORS) ;
- les fetch visent alors `tools/_drive/ready.json`, `tools/_drive/result.json`,
  `tools/_drive/scenario.json` via le défaut `?dir=../../_drive` (relatif à la
  page : `tools/satellites/watchtower/` → `tools/_drive/`). La fiche §2 mentionnait
  littéralement `python -m http.server 4892 -d tools/_drive` : cette variante ne
  sert que le répertoire JSON (la page elle-même n'est plus accessible), elle est
  écartée au profit de la racine — c'est « l'équivalent mesuré » ;
- la partie *« OR a tiny R httpuv page IF httpuv is already locked »* de la fiche
  est également écartée : httpuv exigerait du code R satellite (exposition gate
  inutile) pour un service qu'une commande Python standard rend triviale.

### Paramètres d'URL

| Paramètre | Rôle | Défaut |
|---|---|---|
| `?dir=` | répertoire drive sondé, **relatif à la page** (ex. `fixtures/alive`) | `../../_drive` |
| `?now=` | ISO-8601 : **fige l'horloge** de référence (fixtures : état déterministe quel que soit le jour de la capture) | horloge du navigateur |

Un `?dir=` absolu, avec schéma, backslash, `#` ou `?` est **refusé** (défaut
appliqué, mention dans le pied de page). L'horloge figée est toujours annoncée
dans le pied de page (« FIGÉE sur … ») pour qu'une capture ne soit jamais prise
pour une observation live.

## Ce que la page rend

| Zone | Contenu | Source |
|---|---|---|
| Bandeau (le verdict visuel unique) | `VIVANT` (vert) · `STALE — HB GELÉ` (jaune) · `JOB IN FLIGHT` (bleu) · `PAS DE SESSION` (rouge) · `READY.JSON ILLISIBLE` / `INJOIGNABLE` | `ready.json` + `result.json` |
| Session & heartbeat | `session_token`, `pid`, `port`, `armed`, `viewer`, `hb_n`, `hb_at`, **âge du heartbeat vs `hb_timeout_s`**, `last_seq`, `started_at` | `ready.json` |
| Dernier verdict | `ack_seq`, `status` (étiquette colorée : `done` vert, `error` rouge, `running` jaune, `applied` bleu, `invalid`/`ignored` gris), **terminal ?** (les 4 terminaux du contrat : `done`/`error`/`ignored`/`invalid`), `module`, `action`, `elapsed_s`, `job.status`, `applied_at`, compteurs `errors`/`warnings`, liste des erreurs | `result.json` |
| Scénario | seq, action, module, button, expect + état : `EN ATTENTE` (seq > last_seq), `consommée` (seq ≤ last_seq), `EN ATTENTE — mais AUCUNE session vivante`, `aucun` | `scenario.json` vs `ready.last_seq` |

Règles de classification (bloc pur de `app.js`, commenté, testé ci-dessous) :

1. `ready.json` **absent** ⇒ `PAS DE SESSION` (le contrat interdit d'inventer une
   session — `tools/_drive/README.md` : « missing or stale file means **no live
   client** »).
2. âge heartbeat = `now − hb_at` ; **frais** si âge ≤ `hb_timeout_s` ⇒ `VIVANT`.
3. **gelé** (âge > timeout) :
   - `result.json.status === "running"` ⇒ **`JOB IN FLIGHT`** (leçon 537 s) ;
   - sinon ⇒ `STALE — HB GELÉ` (ne pas armer ce token).
4. `ready.json` présent mais `hb_at` illisible ⇒ `READY.JSON ILLISIBLE` (la
   fraîcheur est indécidable — jamais interprétée comme vivante).

Caveats affichés honnêtement par la page elle-même : `result.json` est **statique
entre deux scénarios consommés** (son `elapsed_s` est la valeur mesurée **à
l'écriture**, pas un compteur vivant) et il **ne porte pas de session_token**
(donc un verdict affiché peut venir d'une session précédente — caveat repris de
`tools/_drive/README.md`). Un âge négatif (horloge client en retard) est affiché
tel quel avec sa mention.

## Falsification — les trois états (fixtures TRACKED)

Les fixtures sont des **données de test du satellite**, trackées, conformes au
contrat (`tools/_drive/README.md` ; champs mesurés dans
`tools/pkg_skeleton/R/drive_watcher.R:1936-1988` pour `result.json` et
`tools/pkg_skeleton/R/drive_watcher.R:4801-4827` pour le bloc `job`). Elles
s'adressent à la page via `?dir=fixtures/<état>` (relatif à la page).

**Procédure** (screenshots pris en live par le coordinateur) :

```bash
# 1. à la racine du dépôt :
python -m http.server 4892
# 2. ouvrir successivement les trois URLs ci-dessous et vérifier le rendu ATTENDU.
```

### (a) Heartbeat vivant — `fixtures/alive/`

```
http://localhost:4892/tools/satellites/watchtower/index.html?dir=fixtures/alive&now=2026-11-03T16:37:20Z
```

**DOIT afficher :** bandeau **`VIVANT`** (vert), « heartbeat frais : 8.0 s ≤
hb_timeout_s (15 s) » (16:37:20 − 16:37:12 = 8,0 s) ; session `fxalive1`, pid
19532, port 7789, `armed: oui (armé)`, `hb_n 341`, viewer `headless` ; verdict
`ack_seq 4`, étiquette **`done`** verte, terminal « oui », `elapsed_s 2.1 s`,
module `bulk_de`, action `run_pipeline` ; scénario seq 5 `snapshot` **`EN
ATTENTE`** (5 > last_seq 4). Sans `?now=`, la fixture vieillit et finirait par
rendre `STALE` — **le `?now=` fait partie de la capture**, il est annoncé dans le
pied de page.

### (b) Heartbeat gelé + `running` en vol — `fixtures/frozen-running/`

```
http://localhost:4892/tools/satellites/watchtower/index.html?dir=fixtures/frozen-running&now=2026-11-03T16:29:00Z
```

**DOIT afficher :** bandeau **`JOB IN FLIGHT`** (bleu) — et **jamais** « DEAD »/«
MORT » — « heartbeat gelé (537.0 s > 15 s) MAIS result.json porte « running » ⇒
travail en cours, PAS une session morte (leçon 537 s) » (16:29:00 − 16:20:03 =
**537 s**, la durée de la leçon) ; session `fxfrozen2`, pid 21410, armé, `hb_n
2203` figé ; verdict `ack_seq 7`, étiquette **`running`** jaune, « non — continuer
à sonder », `elapsed_s 0.9 s` (valeur à l'écriture, la note le rappelle), job
interne `running` ; scénario : **« aucun scénario en attente »** (un 2e
`run_pipeline` pendant un job serait REFUSÉ, pas mis en file — contrat § job).

### (c) Pas de `ready.json` — `fixtures/no-ready/`

```
http://localhost:4892/tools/satellites/watchtower/index.html?dir=fixtures/no-ready
```

**DOIT afficher :** bandeau **`PAS DE SESSION`** (rouge), « ready.json absent —
aucun client vivant (contrat : ne pas inventer de session) » ; panneau session « —
(ready.json absent) » ; le **dernier verdict écrit subsiste** à l'affichage
(`ack_seq 6`, `done`, `elapsed_s 3.1 s`) avec la note « peut venir d'une session
précédente » ; scénario seq 6 `set_inputs` : **`EN ATTENTE — mais AUCUNE session
vivante`** + note contractuelle. (Pas de `?now=` nécessaire : rien d'horodaté à
classer.) Le fichier `result.json` de cette fixture porte volontairement
`armed:false` et un avertissement `stale seq` : c'est la trace d'une session
terminée.

### Falsification logique (sans navigateur)

Le bloc pur de `app.js` (classification, âge, terminaison, scénario) est
**exécutable seul** : `node` l'évalue et rejoue les trois états de fixture
(voir la tranche de checks du builder). Le rendu DOM, lui, est validé par la
lecture bloc par bloc + les captures live ci-dessus — la page ne peut pas booter
de navigateur elle-même, c'est assumé.

## Limites (assumées)

- La page reflète les fichiers **tels qu'ils sont** : elle ne relance rien, ne
  réarme rien, ne répare rien. Panneau passif, comme le badge in-app.
- L'âge du heartbeat dépend de l'horloge du navigateur vs celle de la session R
  (biais d'horloge signalé à l'écran si l'âge est négatif).
- Sans `?now=`, les **fixtures** vieillissent (c'est leur nature) : les captures
  des états (a) et (b) exigent le `?now=` documenté ; l'état live (défaut) n'en
  a évidemment pas besoin.
- N1 rend `ready/result/scenario` — pas `state.json` (digest O11/M2), qui reste
  l'apanage de l'outil agent `transcripto_drive_state`. Le canal est le même ;
  le consommateur est différent.

# FORMAT v1.0.0 — le contrat journey/report de `biomentat-inspector` (FIGÉ)

Satellite **N2 `biomentat-inspector`** — la pièce **contract-first** de la slice
S4 (fiche de lancement
`docs/proposals/PROPOSAL_N_SATELLITES_LAUNCH_ROADMAP_2026-10-26.md`, §2 slice
S4, lignes 161-178 : *« Define the scenario + report FORMAT first (frozen,
documented) »*).

**Ce répertoire ne contient AUCUN code exécutable.** Le driver est la
capability externe `control-browser` — **PAS du code commité** (fiche §2-S4 :
*« the driver is the external control-browser capability (NOT committed
code) »*). Ce fichier et le `README.md` sont le seul contenu tracké du
satellite : 0 `.R`, 0 test, 0 dépendance, 0 app change.

**RÈGLE D'ÉVOLUTION — le contrat est FIGÉ.** Toute évolution (nouveau champ,
nouveau verdict, changement de keying) = **une nouvelle version de ce FORMAT**
(v1.1.0, v1.2.0…), avec sa propre justification et sa date — **jamais un
changement silencieux** de ce fichier. Un REPORT commité déclare toujours la
version du format qui l'a produit (`meta.format_version`) : un rapport v1.0.0
n'est pas re-lisible sous une v1.1.0 sans le dire.

---

## 1. Schéma JOURNEY

Une **journey** = une séquence ordonnée de steps qui parcourt une zone de
l'app et capture un screenshot à chaque état attendu.

```json
{
  "id": "j1-gchain-bulk",
  "app_area": "Import Données > RNA Bulk → analyse Bulk (G-chain)",
  "objectif": "…",
  "steps": [
    {
      "n": 1,
      "action": { "kind": "ui|drive", "…": "…voir grammaire ci-dessous…" },
      "expected_dom_anchor": "<rôle> \"<nom accessible>\"",
      "screenshot_slug": "j1-s1-nav-import-bulk"
    }
  ]
}
```

| Champ | Type | Contrat |
|---|---|---|
| `id` | string | identifiant stable de la journey (kebab-case) |
| `app_area` | string | zone d'app couverte, en notation de navigation `A > B` |
| `objectif` | string | ce que la journey prouve, une phrase |
| `steps[].n` | entier ≥ 1 | ordre imposé, sans trou, sans doublon |
| `steps[].action` | object | **commande drive OU geste UI** — grammaire ci-dessous |
| `steps[].expected_dom_anchor` | string | ancre DOM attendue après le step — **rôle + nom accessibles** (notation ci-dessous) |
| `steps[].screenshot_slug` | string | kebab-case `j<m>-s<n>-<nom>` ; le PNG est capturé **après** que l'état attendu est atteint |

### Grammaire de `action`

**Forme drive** — un `scenario.json` conforme au contrat
`tools/_drive/README.md` (une action par fichier, écriture atomique, verdict
lu dans `result.json` — *« Read this, never stdout »*) :

```json
{ "kind": "drive", "scenario": { "action": "run_pipeline", "module": "bulk_filter", "button": "run_filter_norm" } }
```

- `action` ∈ `noop | snapshot | set_inputs | run_pipeline` ; `seq` est géré par
  le driver (incrément monotone, un `seq ≤ last_seq` est ignoré par design).
- Le driver attend le terminal : `done`/`error` (terminaux) ou
  `ignored`/`invalid` (refus terminaux). `applied` = entrées posées, pipeline
  **pas fini** — continuer à sonder ; `running` = job en vol — continuer à
  sonder. **Ne jamais lire `result.json` en boucle comme un poll** (le fichier
  est statique entre deux scénarios — piège mesuré, `tools/_drive/README.md`
  § « Two instruments that lied ») : émettre un **`snapshot` frais par
  sondage**.
- Job **long** (ex. `bulk_de` : `done` en 2,1 s, contraste observable 852,7 s
  plus tard — mesuré, `tools/_drive/README.md` § G-chain) : `result.json`
  portant `running` est la **preuve de vie**, jamais un échec (leçon 537 s).

**Forme UI** — un geste exécuté par la capability externe `control-browser`
(jamais du code commité) :

```json
{ "kind": "ui", "gesture": "click", "target": "button « Scanner le répertoire »" }
```

- `gesture` ∈ `nav | click | fill | select | scroll` ; `target` = rôle + nom
  accessible (même notation que l'ancre).
- La forme UI est la SEULE voie possible quand l'élément est hors allowlist
  (ex. les boutons dynamiques « Charger » par dataset,
  `import_bulk-btn_load_ds_<id>` — ids créés APRÈS le scan,
  `modules/import/mod_import_bulk.R:1017-1023`).

### Notation de `expected_dom_anchor`

`<rôle> "<nom accessible>"` + identifiant DOM **mesuré** entre parenthèses
quand il existe :

```
tab "RNA Bulk" (app.R:578)
button « Scanner le répertoire » (import_bulk-btn_scan_datasets)
accordion_panel "1. Pipeline Bulk — Contrôle qualité & filtrage" (bulk-acc_bulk / value=panel_filter)
heading "Scree Plot — Variance Expliquée (QC Batch)" (mod_bulk_filter.R:182)
```

Rôles du vocabulaire v1 : `tab`, `button`, `accordion_panel`, `card_header`,
`heading`, `table`. Le nom accessible est le libellé **français rendu**
(l'app est French via i18n) — un renommage UI qui casse l'ancre est un
**drift à détecter**, pas un défaut du format. Les ids cités sont mesurés dans
les sources au moment de la rédaction (références de lignes incluses) ; le
contexte drive les recoupe avec l'allowlist gelée
`tools/pkg_skeleton/R/drive_allowlist.R` (ex. `bulk-filter-run_filter_norm`,
mesuré live par le probe DOM G3c — commentaire de l'allowlist).

---

## 2. Schéma REPORT

Un **report** = le résultat daté de l'exécution d'une journey sur UNE version
de l'app.

```json
{
  "meta": {
    "format_version": "1.0.0",
    "app_version": "v1.1.0-20-g4bb23af",
    "date": "2026-11-04T00:00:00Z",
    "session_drive": { "token": "…", "pid": 0, "port": 0, "mode": "headless|visible|none" }
  },
  "steps": [
    {
      "n": 1,
      "screenshot_path": "QC/inspector/v1.1.0-20-g4bb23af/j1-s1-nav-import-bulk.png",
      "state_verdict": "PASS|FAIL|UNTESTED",
      "visual_verdict": "…jugement humain, libre-texte…",
      "notes": "…mesures, refus drive, motif d'un UNTESTED…"
    }
  ],
  "bilan": "…conclusion libre-texte + comptes…"
}
```

| Champ | Contrat |
|---|---|
| `meta.format_version` | version de CE format qui a produit le report (règle d'évolution) |
| `meta.app_version` | **`git describe --tags`** de l'app au moment de la capture (ex. mesuré 2026-11-04 : `v1.1.0-20-g4bb23af`) — la clé de keying |
| `meta.date` | ISO-8601 UTC |
| `meta.session_drive` | identité de session lue dans `tools/_drive/ready.json` (`session_token`/`pid`/`port`/`viewer`) ; `mode: "none"` si journey à gestes UI purs sans session armée |
| `steps[].screenshot_path` | **`QC/inspector/<app_version>/<screenshot_slug>.png`** — voir §4 Keying |
| `steps[].state_verdict` | `PASS \| FAIL \| UNTESTED` — vocabulaire figé, §3 |
| `steps[].visual_verdict` | **jugement HUMAIN v1, libre-texte** — §3 |
| `steps[].notes` | mesures objectives (refus drive verbatim, valeurs lues, `elapsed_s`), motif obligatoire d'un `UNTESTED` |
| `bilan` | conclusion + comptes `{pass, fail, untested}` |

Le report est un **fichier commité** (sous `tools/satellites/inspector/reports/`,
nommé `<date>-<app_version>-<journey>.json|md`) : il référence les chemins
`QC/` mais ne contient **jamais** les PNG (gitignorés, §4). *(Aucun report
réel n'est produit par la slice S4 elle-même — voir README § Limites ; le
schéma ci-dessus est l'illustration du contrat, les verdicts en majuscules
sont des types, pas des valeurs mesurées.)*

---

## 3. Vocabulaires FIGÉS

### `state_verdict` ∈ `PASS | FAIL | UNTESTED` — motif prover, jamais inventé

Motif `doc-prover` (N6, `tools/satellites/doc_prover/README.md` § « Les
claims ») : un verdict est **mesuré**, jamais deviné — et un `UNTESTED`
inventé est un bug, pas un état acceptable.

- **PASS** — l'ancre attendue est trouvée et le step est exécuté (verdict
  drive terminal `done`, ou geste UI abouti + ancre présente). Mesuré à la
  date du report, sur la version keyée.
- **FAIL** — mesuré **faux** : ancre absente, verdict drive `error`/`invalid`
  (citer le refus verbatim dans `notes`), état attendu non atteint. Un FAIL
  est un **finding de drift** (livrable), pas une panne du format.
- **UNTESTED** — non mesurable **dans ce contexte** : session non armée,
  prérequis absent (ex. J2 sans données chargées), capability indisponible,
  fixture de drop manquante. Le motif est **obligatoire** dans `notes`.

`state_verdict` juge **l'état uniquement** (l'ancre est-elle là ? le verdict
drive est-il terminal `done` ?) — jamais le rendu visuel.

### `visual_verdict` — jugement HUMAIN v1, libre-texte

**PAS d'auto-fail pixel** (fiche §2-S4 : *« verdicts are HUMAN-reviewed first
pass (no auto-fail on pixel diff — layout diffs are judged, not thresholded,
v1) »*). Aucun seuil, aucun scoring automatisé, aucune comparaison d'images
qui produirait un verdict toute seule. Le mode diff (§5) est un **aide à la
revue humaine** (deux captures côte à côte), jamais un juge. Le texte libre
décrit ce que l'humain voit et juge (layout, libellés, plottings, anomalies).

---

## 4. Keying des screenshots

```
QC/inspector/<app_version>/<screenshot_slug>.png
```

- `<app_version>` = `git describe --tags` au moment de la capture. Mesuré au
  2026-11-04 : **`v1.1.0-20-g4bb23af`** (re-mesurer à chaque session — la clé
  CHANGE à chaque commit).
- **`QC/` est gitigné ENTIer** — mesuré : `.gitignore:74` contient la règle
  `QC` (une ligne, racine, couvre l'arbre). Conséquence : **les PNG ne
  shippe JAMAIS** ; ils vivent localement, datés par répertoire de version.
- Les **reports commités référencent les chemins `QC/`** (relatifs à la racine
  du dépôt) : le lecteur d'un report doit pouvoir résoudre le chemin sur SA
  machine si la capture y existe, et sinon savoir exactement quelle capture
  re-produire (slug + ancre + version).
- `QC/inspector/` n'existe pas encore au 2026-11-04 (mesuré : `ls QC/inspector`
  → absent) ; il est créé par la première exécution réelle.

---

## 5. Mode diff (inter-versions)

Comparer deux versions = poser les deux REPORTS côte à côte **par step** (les
slugs sont stables d'une version à l'autre) :

```
QC/inspector/<desc-A>/<slug>.png  vs  QC/inspector/<desc-B>/<slug>.png
```

- Le jugement est **HUMAIN** (§3) : un diff de layout est **jugé, pas
  thresholdé** — v1 n'automatise aucun verdict.
- Le contrat v1.0.0 fige journey + report uniquement ; la mise en forme du
  document de diff reste libre (deux REPORTS + une section de comparaison
  rédigée). Le « premier rapport de diff inter-versions » est le critère de
  succès S4 de la fiche (ligne 238) — il naîtra du premier REPORT réel.

---

## 6. Note de relève — successeur maintenu de `tools/cdp_e2e/`

- `tools/cdp_e2e/` est **ABSENT du disque** (mesuré round8, brief
  `docs/agent/swarm/round8/NEXT_SESSION_BRIEF_2026-11-03.md` dérive c ;
  `ls tools/cdp_e2e` → « No such file or directory », re-mesuré 2026-11-04).
- La **preuve antérieure** du driver disparu tient dans les logs QC :
  `QC/cdp_run1.log` … `QC/cdp_run5.log` (mesurés 2026-11-04 : **5 logs**,
  datés 2026-10-12, ~15,8 Ko chacun ; `QC/cdp_run1.log` boot l'app sur
  `127.0.0.1:8127 (test.mode=TRUE)` et clôt sur **« 8 PASS / 0 SKIP / 0
  FAIL »** avec l'artifact `tools\cdp_e2e\e2e_results.json` — la trace d'un
  runner dont le CODE a disparu, les logs étant gitignorés ont survécu).
- **Positionnement** : l'inspector est le **successeur maintenu** de cette
  couche (fiche §2-S4 point 4 : *« positions itself as the maintained
  successor »*). Ce qui ne disparaîtra plus : le FORMAT (commité, figé) ; ce
  qui vit hors dépôt par design : le driver (capability externe), les PNG
  (QC/ gitigné). Un runner commité peut être perdu ; un contrat + un keying
  non.

---

## 7. JOURNEY 1 « j1-gchain-bulk » — la G-chain (canonique)

La chaîne bulk **import → Step 1 → DE → pathway** (fiche §2-S4 l.168 : *« the
G-chain already driven live in September »*). Ancres mesurées dans les sources
au 2026-11-04 ; commandes drive alignées sur la séquence G2/G3 mesurée de
`tools/_drive/README.md` (§ « Grade G2 » et § G-chain : Step 1 `done` 1,1 s ;
`bulk_de` `done` 2,1 s, contraste observable 852,7 s plus tard).

**Prérequis** : une session armée (`tools/launch_dev_drive.R`, README §
Instruction de service) OU une app visible pilotée aux gestes ; **un drop
`biomentat-exchange/v1` valide** dans le répertoire datasets (manifest.json +
matrix.csv de counts bruts + samplesheet.csv — chemin prouvé au jalon 2
round8, ledger `docs/agent/swarm/round8/ROUND8_EXECUTION_LEDGER.md` § 2 :
drop construit hors dépôt, import abouti, **6 échantillons / 200 gènes
chargés 🟢**).

| n | action | expected_dom_anchor | screenshot_slug |
|---|---|---|---|
| 1 | `ui nav "Import Données > RNA Bulk"` | `tab "Import Données"` (app.R:566) puis `tab "RNA Bulk"` (app.R:578) ; sidebar `heading "Import RNA Bulk"` (mod_import_bulk.R:159) | `j1-s1-nav-import-bulk` |
| 2 | `ui click "button « Scanner le répertoire »"` (après dépôt du drop dans le répertoire datasets) | `accordion_panel "Datasets découverts"` (mod_import_bulk.R:368, value=panel_datasets) ; `button « Scanner le répertoire »` (`import_bulk-btn_scan_datasets`, mod_import_bulk.R:374) ; `table` de scan : la ligne du dataset en statut `valid` (vocabulaire gelé `ts_dataset_dir_scan_states()`) | `j1-s2-drop-scan-datasets` |
| 3 | `ui click "button « Charger »"` de la ligne du dataset (activé seulement si `valid`, attribut `disabled` réel sinon — mod_import_bulk.R:79-97) ; *alternative drive : `set_inputs` sur l'allowlist `import_bulk-*` puis `run_pipeline` token `import_bulk-btn_load` (l'importeur drive réutilisé, fileInput jamais simulé — allowlist, commentaire § Import Bulk)* | `button "Charger"` (`import_bulk-btn_load_ds_<id>`) ; `card_header "Résumé des Données Bulk"` (mod_import_bulk.R:399) rempli (KPI gènes/échantillons) ; optionnel : bloc santé N5 warn-only (`dataset_health_ui`, mod_import_bulk.R:382) | `j1-s3-charger-dataset` |
| 4 | `drive run_pipeline bulk_filter {button: "run_filter_norm"}` (préréglages 10/1/1 acceptés) | `accordion_panel "1. Pipeline Bulk — Contrôle qualité & filtrage"` (`bulk-acc_bulk`/panel_filter, mod_bulk.R:33, ouvert par défaut l.23) ; `button « Lancer Filtrage & VST »` (`bulk-filter-run_filter_norm`, mod_bulk_filter.R:59) ; verdict `done` | `j1-s4-step1-filter-vst` |
| 5 | `drive set_inputs bulk_de {condition_col, group_ref, group_target, de_engine}` puis `drive run_pipeline bulk_de {button: "run_de"}` — **JOB LONG** : `running` puis `done`, puis `snapshot` frais pour lire le contraste (paire volontairement inversée `group_ref`/`group_target` = auto-contrôle du readback, `tools/_drive/README.md` § G-chain) | `accordion_panel "2. Design & Contrastes"` (mod_bulk.R:35) ; `button « Lancer l'Analyse Différentielle »` (`bulk-de-run_de`, mod_bulk_de_ui.R:77) ; `tab "Table DE"` (tab_table, mod_bulk.R:92) peuplée | `j1-s5-de-run` |
| 6 | `drive set_inputs bulk_pathways {enrich_mode: "ora", pathway_db, pathway_org}` puis `drive run_pipeline bulk_pathways {button: "run_pathway"}` | `accordion_panel "3. Pathway Enrichment"` (mod_bulk.R:37) ; `button « Lancer Enrichissement »` (`bulk-pathways-run_pathway`, mod_bulk_pathways.R:44) ; `tab "Pathway"` (tab_pathway, mod_bulk.R:96) peuplée | `j1-s6-pathway-ora` |

⚠️ **Step 6 — honnêteté mesurée** : les tokens pathway (`bulk-pathways-run_pathway`,
`bulk-pathways-run_scores`) sont **bound et gardés mais jamais exercés** au
grade G3 (`tools/_drive/README.md` § « Not validated at this grade ») : la
première exécution réelle de ce step est précisément un livrable de
l'inspector (un `invalid` nommant l'objet manquant est un PASS de contrat
drive… et un FAIL de journey si le prérequis DE est pourtant satisfait —
distinguer les deux dans `notes`).

---

## 8. JOURNEY 2 « j2-qc-panels » — les panneaux QC (canonique)

**Prérequis** : les steps 1-3 de J1 exécutés (données chargées) — sans quoi
les panneaux QC rendent vides et les steps sont **UNTESTED avec ce motif**,
jamais FAIL.

| n | action | expected_dom_anchor | screenshot_slug |
|---|---|---|---|
| 1 | `ui nav "analyse Bulk (module bulk)"` (données chargées, J1 S1-S3 faits) | navset `heading "Résultats Bulk RNA-seq"` (`main_tabs`, mod_bulk.R:80) ; `accordion_panel "1. Pipeline Bulk — Contrôle qualité & filtrage"` ouvert (mod_bulk.R:23) | `j2-s1-resultats-zone` |
| 2 | `ui click "tab « QC Échantillons »"` | `tab "QC Échantillons"` (tab_qc, mod_bulk.R:87 ; UI `mod_bulk_filter_qc_ui`, mod_bulk_filter.R:106) ; plots QC par échantillon rendus | `j2-s2-qc-echantillons` |
| 3 | `ui click "tab « QC Batch »"` | `tab "QC Batch"` (tab_batch_qc, mod_bulk.R:88 ; UI `mod_bulk_filter_batch_ui`, mod_bulk_filter.R:134) ; `card_header "QC Batch"` (mod_bulk_filter.R:144) ; `heading "Scree Plot — Variance Expliquée (QC Batch)"` (mod_bulk_filter.R:182) | `j2-s3-qc-batch` |

Variante non canonique : `tab "PCA"` (tab_pca, mod_bulk.R:86 — même module
`bulk_filter`, famille QC/VST) peut être insérée entre les steps 2 et 3 ; une
telle insertion = **usage du format, pas évolution du format** (les journeys
canoniques ci-dessus restent figées).

---

## 9. Rappel du périmètre du satellite

Ce satellite **ne touche jamais l'app** (fiche §2 contraintes : *« Satellites
live under tools/satellites/ … and NEVER touch the app process »*) : il lit le
canal drive en lecture-seule côté fichiers, écrit `tools/_drive/scenario.json`
/ `arm.json` (drop-box gitignorée, le protocole existant) pour la forme drive,
et capture des PNG sous `QC/` (gitigné). `app.R`, `R/.sources_manifest.txt`,
`i18n/`, `modules/` : intouchés. 0 nouvelle dépendance.

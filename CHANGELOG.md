# Changelog

Tous les changements notables de TranscriptoShiny (« Cerberus ») sont documentés ici.
Format inspiré de [Keep a Changelog](https://keepachangelog.com/fr/1.1.0/) ;
versionnement [SemVer](https://semver.org/lang/fr/). Une étape = un commit sur `main`.

> ℹ️ **Trou de maintenance assumé** : entre `V1.x-D` (2026-09-06) et l'entrée
> `[V1.x — FERMETURE renv]` (2026-09-15), plusieurs jalons ont été livrés **sans
> entrée de changelog** (PLOT-Q1..Q5, PLOT-S1..S5, Bulk V2 / batch-QC, correctif
> Milo, STAT-Q1..Q4). Quatre jalons supplémentaires sont également concernés
> (2026-09-15/16) : garde **C13** (`choices` nommé, `dff0041`), **marges de
> figures** sur device de surface nulle (`abcebcd`), **jeux de gènes natifs**
> (`440878b`) et **P0 `round()` du chemin DE** (`3a0827d`). Leur état fait foi
> dans **`docs/STATUS.md`** — respectivement « Garde C13 + sémantique
> `testServer()` MESURÉE » et « PLOT-S6 : rendu des plots, jeux de gènes natifs,
> garde P0 counts ».
>
> ✅ **Défaut de numérotation CORRIGÉ le 2026-09-16** : `STATUS.md` contenait
> **deux** sections étiquetées `2aw` (`### 2aw.` « Garde C13 » et `## §2aw`
> « PLOT-S6 »), `PLOT-S6` ayant repris un numéro déjà pris. Renumérotation
> **chronologique** : `PLOT-S6` → **`[§2ax](docs/archive/STATUS_JOURNAL.md)`**, `4E-4` → **`[§2ay](docs/archive/STATUS_JOURNAL.md)`**. ⚠️ Le message
> du commit `03951cb` cite encore `[§2ax](docs/archive/STATUS_JOURNAL.md)` pour 4E-4 : c'est l'état **avant** la
> renumérotation. Détail : `STATUS.md` §6, item 6.
>
> ✅ **Trou de maintenance des incréments 21 → 45 COMBLÉ le 2026-09-20** : le
> fichier s'arrêtait au **20ᵉ** incrément (2026-09-18) alors que `STATUS.md` en
> comptait **45** ⇒ **25 jalons** n'avaient **aucune** entrée. Les 25 entrées ont
> été **écrites d'après `STATUS.md`** (`[§2cf](docs/archive/STATUS_JOURNAL.md)`…`[§2df](docs/archive/STATUS_JOURNAL.md)`), **pas** d'après le message
> de commit : ce fichier est un **index**, il **cite**, il ne **re-mesure** pas.
> ⚠️ Les chiffres de garde de ces entrées sont ceux **consignés** dans `STATUS.md`
> au moment du jalon ; en cas d'écart, **`STATUS.md` §0 et §6 font foi**.

## [V1.x — M-4 phase 1 : préflight RAM (mesurer, projeter, alerter)] — 2026-09-30

Audit externe J-8 : les pipelines lourds se lançaient sans aucune lecture
de la RAM disponible (détail : journal archivé §2dr ; commit `6cbbaa9`).

### Ajouté
- `ts_system_ram_mb()` (ps, optionnel) et `ts_ram_budget_check()` — décision
  PURE none/warn/block avec messages français ; câblée dans
  `run_sc_auto_pipeline` via son canal de log. **N'bloque jamais** : le
  gouverneur complet attend le benchmark de calibration (MODE C dir. 2).
- Constante `TS_RAM_PREFLIGHT_FACTOR` (config, défaut 3) — à calibrer.

## [V1.x — M-3 : la règle d'accès à l'état est gardée (C17)] — 2026-09-30

Arbitrage `docs/proposals/STATE_ACCESS_ARBITRATION.md`, **option B retenue**
par l'utilisateur (détail : journal archivé §2dq ; commit `a410dc2`).

### Ajouté
- **C17 (ERREUR)** dans `tools/check_conventions.R` : dans `R/` (hors couche
  d'état), tout accès direct `global_data$champ` / `shared_rv$champ` échoue
  à la garde — utiliser `state_get()/state_set()`.

### Corrigé
- **36 sites** de la couche pure `R/` convertis vers les accesseurs
  (sémantique préservée : trace de dépendance en contexte réactif, repli
  `isolate()` hors contexte). Les ~971 accès directs de `modules/` sont la
  l'idiome réactif canonique : dette assumée chiffrée, pas un chantier.

### Documenté
- `CONVENTIONS.md` §12.8 + table §12 ; `AGENTS.md` règle 4 amendée
  (portée `R/` uniquement) ; `test-conventions-c17-state-access.R`
  (6 PASS, 4 directions dont 2 cas négatifs).

## [V1.x — audit externe : quick wins RAM / QC / daemons] — 2026-09-30

Trois correctifs issus de l'analyse de l'audit externe MODE B/C
(`docs/archive/AUDIT_TRANSCRIPTOSHINY_MODE_B_C [resuslts meta prompt].md` ;
détail mesuré : journal archivé §2dp). Tests écrits ROUGE d'abord ; conventions
0 erreur / 59 avertissements (niveau de référence) après chaque jalon.

### Corrigé
- **QW-1** (`51e662d`) : indicateur RAM — `gc()[, 2]` est DÉJÀ en Mo, la
  division par 1024 affichait ~1024× trop petit, avec un `gc()` complet forcé
  dans le chemin de rendu. Helper `ts_process_rss_mb()` (`ps` optionnel,
  déjà au renv.lock) ; repli tas R sans division ; rafraîchissement 10 s.
- **QW-2** (`2cfa331`) : défauts QC de l'autopipeline SC en SOURCE UNIQUE —
  `TS_SC_QC_MIN_GENES/MAX_GENES/MAX_PCT_MT/PCA_DIMS` dans `config/defaults.R`,
  lus par l'UI ET par `.sc_ap_drive_inputs()`. Le canal drive codait
  10/10000/50/10 contre 100/8000/20/20 côté UI (fork de reproductibilité,
  audit C-1). `.sc_ap_run_drive()` gagne un paramètre `inputs` optionnel
  (production inchangée) pour que les tests de mécanique surchargent le QC
  de leur fixture, comme le permet le contrat des entrées.
- **QW-3** (`1524a75`) : `TS_MIRAI_N_DAEMONS` réellement lue — la constante
  était définie mais ignorée, `spatial_async.R` codait `6L` en dur à deux
  sites (audit C-2). Repli `exists()` conforme à l'idiome de la maison.

## [V1.x — dette de conventions, 45ᵉ incrément] — 2026-09-20 — `R/spatial/spatial_report.R` : C9 payé, **et un 2ᵉ P0 de produit trouvé par le test**

Écrit `test-spatial-report.R` (**17** blocs, **52** assertions) — le dernier fichier de `R/`
dont **aucune** fonction n'était citée. Le test a révélé un défaut **antérieur** :

### Corrigé
- 🔴 **P0 de produit** : `build_spatial_report_dataset()` rendait `NULL` au lieu de son
  instantané documenté pour **tout** jeu de données **sans histologie** — c'est-à-dire le cas
  **ordinaire**. Trois `return(NULL)` vivaient à l'intérieur d'un `tryCatch` **argument** de
  `list(...)` : en R, `return()` y sort de la **fonction**, donc le `list(...)` n'était **jamais
  construit**. Les appelants (`mod_spatial_report.R:149` et `:157`) poussaient `NULL` dans
  `ds_list`, transmis au Rmd comme `datasets` ⇒ **perte silencieuse** de l'instantané.
  Corrigé en `if`/élse rendant des valeurs.
- 🟡 En-tête périmé : `R/utils_spatial_report.R` → `R/spatial/spatial_report.R`
  (🔴 défaut **systémique mesuré** : **10** fichiers de `R/spatial/` déclarent encore `R/utils_*.R`).

### Vérifié
- Règle 0 exécutée en entier : test **écrit d'abord** (🔴 **25 fail / 26 pass**) → correctif →
  🟢 **0 fail / 52 pass** → **falsifié** (un `return(NULL)` réintroduit ⇒ **23 fail / 29 pass**) →
  **restauré à l'octet** (md5 `458c697f…`).
- Garde : `0 erreur / 58 avert.` → **`0 erreur / 57 avert.`** ; **C9 20 → 19** ; C9b **inchangé à 37** ;
  C10 = 0. `spatial_report.R` **n'est pas** dans la liste C9b ⇒ paiement le plus propre du chantier.

### Constaté (listé, NON corrigé)
- `Embeddings()` **non préfixé** en **4** sites (re-mesuré : `sc_export.R:31`, `sc_helpers.R:1238`,
  `mod_sc_pipeline.R:434`, `mod_sc_viz.R:598`) — et non **1** comme l'affirmait [§2dd.5](docs/archive/STATUS_JOURNAL.md).

## [V1.x — dette de conventions, 44ᵉ incrément] — 2026-09-20 — C9 direct : `R/sc/sc_export.R` + **un P0 trouvé en chemin**

### Corrigé
- 🔴 **P0 de produit** : le script R **généré** par `sc_r_script_text()` n'était **pas du R valide**
  dans la branche `has_mt = FALSE` — un **`else` orphelin** (R exige que `else` commence sur la
  **même ligne** que la fin du `if`). **Portée** : l'export était **inexécutable** pour tout objet
  **sans** `percent.mt`, sur **les deux** sorties (téléchargement **et** rapport). Corrigé en
  **2** lignes (+2/−2, accolades), **falsifié** (bug restauré à l'octet ⇒ **1** assertion rouge).
- 🟢 Test éponyme `test-sc-export.R` — **C9 21 → 20**, gardes **59 → 58**.

### 🔴 Une affirmation écrite TROIS fois, corrigée par la mesure
« `sc_export.R` est le **seul** fichier dont aucune fonction n'est citée » ([§2da.5](docs/archive/STATUS_JOURNAL.md), [§2dc.3](docs/archive/STATUS_JOURNAL.md), [§2dc.11.1](docs/archive/STATUS_JOURNAL.md)) :
ils sont **3** — `R/sc/sc_export.R` (**0/1**, soldé ici), `R/spatial/spatial_export.R` (**0/8**),
`R/spatial/spatial_report.R` (**0/2**). ⚠️ **Un chiffre n'est pas une mesure parce qu'il est écrit.**

### 🔴 Le lot a CASSÉ DEUX TESTS DE GARDE
`test-conventions-c9b-owned.R` et `test-conventions-c9-domain-alias.R` (**1 fail** chacun) parce
qu'ils **épinglaient une PHOTOGRAPHIE de la population C9**, mêlant une **invariante** de la règle à
un **état** du dépôt. Le correctif **n'a pas supprimé** les assertions (elles seraient devenues
**infalsifiables**) : il **sépare** les deux, garde l'**invariante**, assère le **solde dans les deux
sens**, ajoute une assertion de **couverture réelle**, et abaisse le **plafond de dette de 22 à 20**
avec une assertion de **non-vacuité**. 🔑 *Un test qui épingle une POPULATION doit distinguer
l'**invariante de la règle** de l'**instantané du dépôt**.* ⚠️ **Corollaire** : un lot **« local »** peut
casser les tests d'une **autre** règle ⇒ la suite **complète** n'est **jamais** facultative.

## [V1.x — dette de conventions, 43ᵉ incrément] — 2026-09-20 — `C9b` CODÉE, et la prémisse de §14.2 était FAUSSE

### Ajouté
- 🛡️ **Règle `C9b`** (`tools/check_conventions.R`) : signale « test éponyme PRÉSENT, code jamais
  cité ». Population = fichiers ayant **déjà** un test éponyme ; sévérité **`WARN`**.

### 🔴 La prémisse « 0 signalement » ÉTAIT FAUSSE
La sonde d'origine portait **deux** cécités **silencieuses**, dont une regex de noms qui ne
**capturait jamais** un identifiant **commençant par `.`** ⇒ **195** fonctions **privées** invisibles
(**36 %** des **762** fonctions possédées). Mesure réelle : **37** fichiers / **174** fonctions
orphelines, chaque fois **re-confirmée** par recomputation indépendante (**34** ; l'écart de **3** est
l'**alias de domaine** `plotting → plot`, [§2cy](docs/archive/STATUS_JOURNAL.md)).
⇒ **Verdict INVERSE de §14.3** (`C16`, lui, a **0** site sur **136** fichiers ⇒ **`ERREUR`**).
🔑 *La sévérité se décide sur le COÛT MESURÉ, jamais sur la forme de la règle* — et **la mesure
doit venir d'une sonde PROUVÉE SAINE**, sinon on grave dans la doctrine un chiffre faux.

## [V1.x — dette de conventions, 42ᵉ incrément] — 2026-09-20 — C9 : le premier lot choisi sur la MESURE DE COUVERTURE

### Modifié
- `R/spatial/spatial_plotting.R` : test éponyme **écrit** — **C9 22 → 21**.

### 🟢 Premier lot C9 choisi par la mesure de couverture, pas par la liste du garde
`spatial_plotting.R` était le **seul** fichier signalé dont **aucune** des **10** fonctions n'était
citée par un test ⇒ le test **exerce** réellement le fichier, au lieu de satisfaire la règle par
son **nom**.

## [V1.x — dette de conventions, 41ᵉ incrément] — 2026-09-20 — §14.1 CODÉ : `ts_error_state()`, et la PRÉMISSE de la décision était FAUSSE

### Ajouté
- 🟢 `R/core/error_state.R` : **`ts_error_state(e, class = NULL)`** — les **11** accesseurs
  `*_error_state()` deviennent des **délégations d'une ligne** (leur nom est appelé par les
  contrats gelés : on ne les renomme pas).

### 🔴 La prémisse écrite de §14.1 était FAUSSE
« les 11 corps sont textuellement identiques » : mesure ⇒ **3** familles (**7** gardés par leur
classe, **3** aveugles, **1** gardé sur `"condition"`), et la divergence est **observable** — les
aveugles rendent l'état d'une erreur **étrangère**, les gardés rendent `NA`. D'où l'**élargissement**
de la forme prescrite d'un argument `class` **optionnel**, qui **reproduit** les trois familles au
lieu d'en changer une. ⚠️ **Aucun compteur de garde ne bouge** (C9 = 22, C10 = 0, C11 = 1, C16 = 0).

### ⚠️ Piège
- **Sans** la ligne ajoutée à `helper-source.R`, le lot casse **11 domaines** : `test-bulk-network.R`
  ne source que son propre fichier.

## [V1.x — dette de conventions, 40ᵉ incrément] — 2026-09-19 — §14.3 CODÉ : `C16` passe en `ERREUR` (au SOURCE de la sévérité)

### Modifié
- 🛡️ **`C16`** (`paste0()`/`paste()` appelé seul) est promue **`ERREUR`** et codée — au **source
  de la sévérité**, c'est-à-dire au **canal** de `.add()`.

### 🔑 Leçon
- **La sévérité EFFECTIVE d'une règle est le CANAL de `.add()`, pas son libellé** : `lvl` n'est lu
  que pour l'**affichage**. Mesure : **0** site sur **136** fichiers ⇒ `ERREUR` est sans coût.

## [V1.x — dette de conventions, 39ᵉ incrément] — 2026-09-19 — CORRECTION DE MESURE C9 : l'alias de domaine

### Corrigé
- 🔴 **Erreur de mesure** : le calcul de la population `C9b` séparait à tort **`plotting`** de
  **`plot`** — ce sont les **mêmes** test (**C9 26 → 22** après correction).

### 🔑 Leçon
- Un **alias de domaine** ne se voit **pas** en lisant la règle : il se **mesure**.

## [V1.x — dette de conventions, 38ᵉ incrément] — 2026-09-19 — `modules/sc/mod_sc_velocity.R` classe `sc_velocity_error` : 🏁 la dette C10 est SOLDÉE

### Modifié
- `modules/sc/mod_sc_velocity.R` : **10 → 1** site apres classement — **C10 1 → 0**.
- 🏁 **C10 = 0 site dans tout le dépôt** : la dette **C10 est SOLDÉE**.

### 🔑 Ce qui reste
- Ce qui reste ouvert n'est **plus** C10 mais **C9** (**20** fichiers, dont **2** à couverture
  **nulle**) **et** `C9b` (**37**).

## [V1.x — dette de conventions, 37ᵉ incrément] — 2026-09-19 — `modules/sc/mod_sc_pipeline.R` classe `mod_sc_pipeline_error`

### Modifié
- `modules/sc/mod_sc_pipeline.R` → classe `mod_sc_pipeline_error` — **C10 2 → 1**.

### Corrigé
- 🔴 **Défaut latent** de `sprintf()` corrigé au passage.

## [V1.x — dette de conventions, 36ᵉ incrément] — 2026-09-19 — `R/sc/sc_velocity.R` rejoint la classe EXISTANTE `velocity_validation_error`

### Modifié
- `R/sc/sc_velocity.R` : les sites rejoignent la classe **existante** `velocity_validation_error`
  — **C10 3 → 2**. 🏁 **Le front `R/` est TERMINÉ.**

## [V1.x — dette de conventions, 35ᵉ incrément] — 2026-09-19 — `bulk_batch_qc.R` classe `bulk_batch_qc_error`, et le `state` d'une erreur AVALÉE

### Modifié
- `R/bulk/bulk_batch_qc.R` → `bulk_batch_qc_error` — **C10 4 → 3**.

### 🔑 Résultat de doctrine
- Le **`state` d'une erreur AVALÉE** : démonstration qu'une erreur **avalée** par un gestionnaire qui
  **retourne** une valeur ne peut pas être observée par sa classe ⇒ preuve = **verrou source**.

## [V1.x — dette de conventions, 34ᵉ incrément] — 2026-09-19 — `mod_sc_markers.R` classe `sc_markers_error`, et les 4 décisions ouvertes TRANCHÉES

### Modifié
- `modules/sc/mod_sc_markers.R` → classe `sc_markers_error` — **C10 5 → 4**.

### 🟢 `docs/CONVENTIONS.md` §14 — les 4 décisions ouvertes sont TRANCHÉES
- `state`/`class` = **deux axes orthogonaux** + **UN** accesseur générique ⇒ **`R/` est DÉBLOQUÉ**
  (les 2 lots gelés redeviennent convertibles) ;
- `C9b` = forme B **avec clause de non-superposition** (0 signal neuf) ;
- `C16` = **promu `ERREUR`** et **codé** (§14.3, [§2cz](docs/archive/STATUS_JOURNAL.md) — au **source** de la sévérité) ;
- portée de `C6` = **profondeur d'accolade 0**.

## [V1.x — dette de conventions, 33ᵉ incrément] — 2026-09-19 — `mod_bulk_de_multimethod.R` → `bulk_de_multimethod_error`

### Modifié
- `modules/bulk/mod_bulk_de_multimethod.R` → `bulk_de_multimethod_error` — **C10 6 → 5**.
  **6ᵉ domaine `modules/` bouclé**.

## [V1.x — dette de conventions, 32ᵉ incrément] — 2026-09-18 — `modules/bulk/mod_bulk_report.R` → `bulk_report_error` — **la CLASSE S'ÉCHAPPE**

### Modifié
- `modules/bulk/mod_bulk_report.R` → `bulk_report_error` — **C10 7 → 6**, dette **34 → 33**.
  Domaine `modules/bulk/` **bouclé**.

### 🟢 Verdict INVERSE de [§2cp](docs/archive/STATUS_JOURNAL.md)/[§2cq](docs/archive/STATUS_JOURNAL.md)
- **La classe S'ÉCHAPPE** (aucun gestionnaire avaleur sur le chemin de sortie) : preuve
  **COMPORTEMENTALE**. Le **discriminant** est la présence d'un gestionnaire qui **retourne** une
  valeur, **pas** le dossier ni le construit.

## [V1.x — dette de conventions, 31ᵉ incrément] — 2026-09-18 — `modules/import/mod_import_spatial.R` → `spatial_import_error`

### Modifié
- `modules/import/mod_import_spatial.R` → `spatial_import_error` — le lot qui **BOUCLE** — et
  **CORRIGE** — un domaine.

### 🔑 Premier **contrôle de BORNE** réel
- Le site est le **bras par DÉFAUT** d'un `switch()`.

## [V1.x — dette de conventions, 30ᵉ incrément] — 2026-09-18 — `modules/bulk/mod_bulk.R` → `mod_bulk_error`

### Modifié
- `modules/bulk/mod_bulk.R` → `mod_bulk_error` — **C10 9 → 8**. 2ᵉ **routeur parent**.

### 🔑 « ATTEIGNABLE » n'est PAS « OBSERVABLE »
- Le corps du site est **atteint** (prouvé par témoin : le message du garde ressort par la
  notification) mais son erreur est **AVALÉE** par un `tryCatch` dont le gestionnaire **retourne**
  une valeur au lieu de relancer ⇒ preuve = **verrou source**, rendu **falsifiable** par un test qui
  assère que le gestionnaire ne contient pas `stop(`.

### 🟢 Technique neuve et réutilisable
- `shiny::withReactiveDomain(shiny::MockShinySession$new(), …)` **débloque les primitives à
  session** appelées via `::` (`Progress$new()`, `removeModal()`) — exactement le motif qui avait
  fait conclure « injoignable » au [§2cc](docs/archive/STATUS_JOURNAL.md).

## [V1.x — dette de conventions, 29ᵉ incrément] — 2026-09-18 — `modules/sc/mod_sc.R` → `mod_sc_error` (lot « qui paie DEUX fois »)

### Modifié
- `modules/sc/mod_sc.R` → **`mod_sc_error`** — **C10 11 → 9**.

### 🟢 Doctrine de nommage tranchée
- Un **routeur parent** (`mod_sc.R`, `mod_bulk.R`, `mod_spatial.R`) n'a **pas** de domaine unique
  ⇒ sa classe porte le **FICHIER**, `mod_` inclus (`mod_sc_error`) — **pas** `sc_error`, nom
  **interdit** par `CONVENTIONS.md` §7.

## [V1.x — dette de conventions, 28ᵉ incrément] — 2026-09-18 — re-qualification de 10 sites, puis `mod_bulk_pathways.R` → `bulk_pathways_error`

### Modifié
- Re-qualification des **10** sites de `modules/`, puis `mod_bulk_pathways.R` →
  `bulk_pathways_error` — **C10 12 → 11**.

## [V1.x — dette de conventions, 27ᵉ incrément] — 2026-09-18 — `mod_spatial_deconv_refviz.R` → `spatial_deconv_refviz_error`

### Modifié
- `modules/spatial/mod_spatial_deconv_refviz.R` → `spatial_deconv_refviz_error` —
  **C10 13 → 12**.

## [V1.x — dette de conventions, 26ᵉ incrément] — 2026-09-18 — re-qualification de 12 sites, puis `mod_spatial_pipeline.R` → `spatial_pipeline_error`

### Modifié
- Re-qualification des **12** sites de `modules/`, puis `mod_spatial_pipeline.R` →
  `spatial_pipeline_error` — **C10 14 → 13**.

## [V1.x — dette de conventions, 25ᵉ incrément] — 2026-09-18 — `modules/spatial/mod_spatial_cluster.R` → `spatial_cluster_error`

### Modifié
- `modules/spatial/mod_spatial_cluster.R` → `spatial_cluster_error` — **C10 16 → 14**.

## [V1.x — dette de conventions, 24ᵉ incrément] — 2026-09-18 — `R/sc/sc_pipeline.R` → `sc_pipeline_error`

### Modifié
- `R/sc/sc_pipeline.R` → `sc_pipeline_error` — **premier lot du chantier qui ne paie QUE C10**.

### 🔴 Et son test éponyme est TROMPEUR, mesuré
- `test-sc-pipeline.R` n'exerce que `resolve_sketch_preset()`, qui vit dans **`R/sc/sc_helpers.R`** ;
  `sc_pipeline.R` ne contient qu'**UNE** fonction et le test ne l'appelle **jamais** ⇒ **C9 est
  satisfaite par le NOM, jamais par la couverture**.
- 🟢 La justification est rendue **EXÉCUTABLE** : un second test lit la source et assère que le
  **dernier** gestionnaire journalise + notifie **sans** `stop(` — si un jour il relance, le test
  **échoue** et signale que le lot devient **prouvable à l'exécution**. C'est la réponse au piège
  « une justification non vérifiée pourrit ».

## [V1.x — dette de conventions, 23ᵉ incrément] — 2026-09-18 — `R/bulk/bulk_import_engine.R` → `bulk_import_engine_error`

### Modifié
- `R/bulk/bulk_import_engine.R` → `bulk_import_engine_error` — **C9 27 → 26** *et* **C10 18 → 17**.
  **1/1 = 100 %** prouvé. Dernier lot de `R/` qui **paie −2**.

### 🔑 Joignabilité : elle se lit dans le CORPS du gestionnaire
- Le site est un gestionnaire de `tryCatch` qui **RELANCE** ⇒ joignable **sans aucun mock**, par un
  simple **chemin absent** — l'**inverse** du cas où le gestionnaire **retourne** une valeur.

## [V1.x — dette de conventions, 22ᵉ incrément] — 2026-09-18 — `R/sc/sc_bpcells.R` → `sc_bpcells_error`

### Modifié
- `R/sc/sc_bpcells.R` → `sc_bpcells_error` — **C9 28 → 27** *et* **C10 19 → 18**.
  **1/1 = 100 %** prouvé.

### 🔴 La technique de l'environnement enfant NE SE PROPAGE PAS à une indirection à DEUX niveaux
- Re-pointer seulement la fonction externe **échoue en silence** : la recherche du helper part de
  l'environnement enfant, ne l'y trouve pas, **retombe sur `globalenv`** (version non mockée).
  ⇒ **Déposer AUSSI le helper mocké DANS l'environnement enfant.** Dès qu'une **indirection**
  s'interpose, il faut **injecter chaque maillon**.

## [V1.x — dette de conventions, 21ᵉ incrément] — 2026-09-18 — `R/spatial/spatial_multi.R` → `spatial_multi_error`

### Modifié
- `R/spatial/spatial_multi.R` → `spatial_multi_error` — **C9 29 → 28** *et* **C10 20 → 19**.
  **1/1 = 100 %** prouvé. **Le lot le moins cher du chantier**.

### 🟢 Le témoin nominal a payé
- Un 3ᵉ test vérifie que `length >= 2` **franchit** la garde — sans ce contrôle de validité, une
  garde **trop large** passerait les deux tests de borne.

### 🔴 Rechute d'un défaut déjà payé
- La ligne « Reste ouvert technique » de `STATUS.md` §0 annonçait `324 → 112` alors que la ligne
  « Gardes » du **même tableau** disait **50** ⇒ **2ᵉ occurrence** : le chiffre vrai est sorti de
  l'**incohérence interne**, pas d'une relecture.

## [V1.x — dette de conventions, 20ᵉ incrément] — 2026-09-18 — `spatial_deconv_tasks.R` classé (`spatial_deconv_tasks_error`)

**5 sites sur 5 prouvés à l'exécution (100 %)** — et surtout : 🔴 **ce lot
RÉFUTE partiellement le prédicteur de forme** sur lequel il avait été **écarté**
la veille. C10 **25 → 20**, C9 **30 → 29**, dette **56 → 50** (−6 = 5 + 1).

### Modifié

- `R/spatial/spatial_deconv_tasks.R` : les **5** `stop()` non classés passent à
  `stop(errorCondition(<msg>, class = "spatial_deconv_tasks_error"))` — sites
  **30, 35, 91, 156, 221**. **Un seul multi-argument** (**30**, **4** arguments)
  ⇒ `paste0()` (C16). ⚠️ Le fichier porte **7** occurrences du token `stop(`
  pour **5** sites : 2 portent `call. = FALSE` et sont **exemptées** ⇒ les
  numéros de ligne viennent du **garde**, jamais d'un `grep` (écart : **40 %**).
- `tests/testthat/test-spatial-deconv-tasks.R` — **NOUVEAU**, rouge d'abord
  (`failed=6 passed=8`) puis vert (`failed=0 passed=14`). Il **source 3
  fichiers** : `spatial_async.R` (`write_mirai_log`, appelé dès la 1ʳᵉ ligne),
  `spatial_deconv_prep.R` (`DECONV_DEFAULT_N_HVG`, `select_hvg_for_deconv`,
  `cap_matrix_to_hvg`), puis le fichier testé — un fichier de `R/` **n'est pas
  auto-suffisant**.

### 🔴 Le prédicteur de forme prédit le COÛT, pas la POSSIBILITÉ

Ce fichier avait été **écarté** au 19ᵉ incrément avec cette motivation :
« une longue fonction « corps de pipeline » (~150 lignes), sites enfouis
derrière de nombreux prérequis ». Converti au 20ᵉ, il s'est révélé
**entièrement prouvable**, par trois leviers :

1. **Un VRAI répertoire BPCells** — le prologue appelle
   `BPCells::open_matrix_dir()` **avant toute garde** ; sans artefact réel,
   aucun site de la fonction n'est atteignable. `BPCells::write_matrix_dir()`
   sur une `dgCMatrix` 10 × 20 suffit (~1 s).
2. **L'environnement enfant** ([§2bz.3](docs/archive/STATUS_JOURNAL.md)) force chaque garde de dépendance, alors
   que `spacexr`, `STdeconvolve`, `topicmodels`, `slam` **et** `BPCells` sont
   **tous installés**.
3. **Un mode « tiers »** (`"stdeconvolve"`, ni `"rctd"` ni `"labeltransfer"`)
   tombe **directement** sur la garde STdeconvolve, sans prérequis intermédiaire.

Le site **156** n'a besoin d'**aucun mock** : avec un `backend` non-`bpcells`,
`.load_reference_artifact()` fait un simple `readRDS()`, et annoter seulement
**5** des 20 cellules fait tomber `ncol(ref_obj) < 10` après `subset()`.

⇒ **Correction de doctrine** : le prédicteur sert désormais à **estimer
l'effort**, **jamais à éliminer** un lot, et **tout lot écarté sur ce critère
doit être réexaminé**.

⚠️ **Une erreur volontairement provoquée émet un avertissement ATTENDU**
(`gzfile()` sur un fichier absent) ⇒ `suppressWarnings()` dans le test, sinon
testthat le compte et le fichier paraît sale à tort.

### 🟢 Mesures

| Indicateur | Avant | Après |
|---|---|---|
| Suite complète | `failed=0 passed=6150 error=0 skipped=1` (110 fichiers) | **`failed=0 passed=6164 error=0 skipped=1`** (**111**) — **+14** (6150 + 14 = 6164 **exactement**) |
| Test ciblé | — | **ROUGE `failed=6 passed=8`** → **VERT `failed=0 passed=14`**, `skipped=0` |
| Garde — total | 0 erreur / **56** avert. | 0 erreur / **50** avert. |
| C10 · C9 · C11 | 25 · 30 · 1 | **20** · **29** · 1 |

⚠️ Diff asymétrique (5/6), attendu : le site 30, bi-ligne, a été replié.
Conclusif : `parse()` (4 expressions) + delta de déséquilibre par ligne
**40 → 38**.

### 🟡 Prochaine étape — une DÉCISION, pas un lot

`R/` n'a **plus aucun fichier multi-sites** : **6 sites / 6 fichiers, tous à 1
site** (dont **2 gelés** par l'idiome `state`, **1** avec test éponyme, **3**
sans test). Les deux fronts sont maintenant à 1 site par lot :

- rester sur `R/` : **−2** par lot (C10 **+** C9), **preuve d'exécution
  possible**, mais **3** lots seulement ;
- basculer sur `modules/` : **−1** par lot, **aucun** C9 à payer, **14** lots,
  mais **preuve réduite au verrou source** (tous en serveurs réactifs).

⇒ Trancher par **objectif** (réduire vite vs réduire avec preuve), pas par
habitude.

## [V1.x — dette de conventions, 19ᵉ incrément] — 2026-09-18 — `spatial_niche.R` classé (`spatial_niche_error`)

**5 sites sur 5 prouvés à l'exécution (100 %)** — le lot a été **choisi par le
prédicteur** posé la veille au [§2cc.2](docs/archive/STATUS_JOURNAL.md), et il a fonctionné. C10 **30 → 25**, C9
**31 → 30**, dette **62 → 56** (−6 = 5 + 1).

### Modifié

- `R/spatial/spatial_niche.R` : les **5** `stop()` non classés passent à
  `stop(errorCondition(<msg>, class = "spatial_niche_error"))` — sites **73,
  80, 89, 120, 156**. **3 multi-arguments** (**80**, **89**, **120** ⇒ ce
  dernier en a **trois**) ⇒ **`paste0()`** obligatoire (C16).
- `tests/testthat/test-spatial-niche.R` — **NOUVEAU**, écrit **rouge d'abord**
  (`failed=6 passed=8`) puis vert (`failed=0 passed=14`). 🟡 **2ᵉ lot d'affilée
  sans aucun test hérité** : deux fichiers *mentionnent* `spatial_niche`
  (`test-plot-theme.R`, `test-shinytest2-spatial.R`) **sans le couvrir
  exclusivement** ⇒ aucun `git mv` gratuit n'était possible.

### 🔵 Le lot a été choisi par un critère, et le critère s'en trouve raffiné

Deux candidats payaient **5 sites + 1 C9** — la taille ne départageait pas :

| Candidat | Forme | Verdict |
|---|---|---|
| `spatial_niche.R` | 2 fonctions top-level, gardes **en tête**, aucune fonction qui avale | **retenu** → 5/5 |
| `spatial_deconv_tasks.R` | 1 longue fonction « corps de pipeline » (~150 lignes), sites enfouis | écarté |

**Raffinement du prédicteur** : `spatial_niche.R` contient **un** `tryCatch`
(ligne 117) — un comptage naïf l'aurait classé « à risque » — mais son site 120
**est** joignable, parce que le gestionnaire **relance** (`error = function(e)
stop(...)`). Ce qui condamne un site, ce n'est donc pas la présence de
`tryCatch`, c'est un gestionnaire qui **retourne** une valeur (`NULL`) ou émet
un `warning()` ⇒ **lire le corps du gestionnaire**.

🟢 **Astuce réutilisable** : pour faire échouer `kmeans()` sans dépendre d'un
message interne, on **confond** les coordonnées (12 points au même endroit) ⇒
lignes de composition identiques ⇒ « more cluster centers than distinct data
points ». Zéro aléatoire, donc zéro flake. ⚠️ La **queue** du message 120 est
volatile (elle interpole `conditionMessage(e)` de `stats::kmeans()`) ⇒ assertion
par **préfixe + longueur strictement supérieure**.

### 🟢 Mesures

| Indicateur | Avant | Après |
|---|---|---|
| Suite complète | `failed=0 passed=6136 error=0 skipped=1` (109 fichiers) | **`failed=0 passed=6150 error=0 skipped=1`** (**110**) — **+14** (6136 + 14 = 6150 **exactement**) |
| Test ciblé | — | **ROUGE `failed=6 passed=8`** → **VERT `failed=0 passed=14`**, `skipped=0` |
| Garde — total | 0 erreur / **62** avert. | 0 erreur / **56** avert. |
| C10 · C9 · C11 | 30 · 31 · 1 | **25** · **30** · 1 |

⚠️ **Diff asymétrique (5/8), encore une fois ATTENDU** : trois appels bi-lignes
repliés ⇒ `2+2+2+1+1 = 8` suppressions, `1×5 = 5` insertions. Conclusif :
`parse()` + delta de déséquilibre par ligne **18 → 12** (les 6 lignes repliées).

## [V1.x — dette de conventions, 18ᵉ incrément] — 2026-09-17 — `spatial_io.R` classé (`spatial_io_error`)

**7 sites sur 11 prouvés à l'exécution (64 %)** — 🟡 **la série de trois lots
INTÉGRAUX s'arrête ici** ([§2bz](docs/archive/STATUS_JOURNAL.md) 16/16, [§2ca](docs/archive/STATUS_JOURNAL.md) 12/12, [§2cb](docs/archive/STATUS_JOURNAL.md) 7/7 = 100 %), et la
raison est **structurelle**, pas un manque d'effort. C10 **41 → 30**, C9
**32 → 31**, dette **74 → 62** (−12 = 11 sites + 1 C9).

### Modifié

- `R/spatial/spatial_io.R` : les **11** `stop()` non classés passent à
  `stop(errorCondition(<msg>, class = "spatial_io_error"))` — sites **133, 137,
  149, 432, 748, 751, 1002, 1043, 1044, 1047, 1052**. **3 multi-arguments**
  (**149**, **432**, **748**) ⇒ **`paste0()`** obligatoire (C16) ; 149 et 748
  étaient des appels **multi-lignes**, 432 un `stop("…", class(r)[1L])`.
- `tests/testthat/test-spatial-io.R` — 🟢 **NOUVEAU, et premier lot du chantier
  dont le test a dû être ÉCRIT de zéro** : vérifié, **aucun** fichier de test ne
  porte ce nom **et** aucun ne `source()` `spatial_io.R` — à la différence des
  **quatre** lots précédents, où un test hérité existait sous un nom périmé
  ([§2bz.2](docs/archive/STATUS_JOURNAL.md), [§2ca](docs/archive/STATUS_JOURNAL.md), [§2cb.2](docs/archive/STATUS_JOURNAL.md)).

### 🔴 Les 4 sites non prouvés, et pourquoi c'est structurel

- **133 / 137 / 149** (`Package 'png'|'jpeg'|'magick' requis`) vivent dans la
  fermeture `read_histology_file()`, imbriquée dans `extract_histology_image()`.
  Or celle-ci enveloppe **tout** son corps dans un
  `tryCatch(..., error = function(e) { warning(...); NULL })` ⇒ l'erreur est
  **avalée** et convertie en **warning** : **la classe est inobservable de
  l'extérieur**, même avec un mock **sélectif** (couper `png` seul en laissant
  `jsonlite` réel, sinon `json_scale_factors` reste `NULL` et la branche n'est
  jamais atteinte).
- **1052** exige une **vraie** matrice BPCells sur disque.

⇒ **La joignabilité est une propriété de la FONCTION ENGLOBANTE, pas du site** :
les 3 sites de `extract_histology_image()` sont **tous** injoignables (3/3),
ceux des fonctions sans avaleur **tous** joignables (7/7). Ces 4 sites sont
couverts par le **verrou source**, qui englobe les 11.

### 🟢 Mesures

| Indicateur | Avant | Après |
|---|---|---|
| Suite complète | `failed=0 passed=6119 error=0 skipped=1` (108 fichiers) | **`failed=0 passed=6136 error=0 skipped=1`** (**109**) — **+17** = le nouveau fichier (6119 + 17 = 6136 **exactement**) |
| Test ciblé | — | **ROUGE `failed=8 passed=9`** → **VERT `failed=0 passed=17`** (`skipped=0` ⇒ BPCells **installé**, les 3 sites gardés ont bien tourné) |
| Garde — total | 0 erreur / **74** avert. | 0 erreur / **62** avert. |
| C10 · C9 · C11 | 41 · 32 · 1 | **30** · **31** · 1 |

🟢 **Le diff n'était PAS symétrique — et c'était attendu** : 11 insertions /
15 suppressions, parce que 149 (4 lignes → 1) et 748 (2 → 1) ont été
**repliées**. Compté : `4 + 2 + 9 = 15` suppressions, `1 + 1 + 9 = 11`
insertions. ⚠️ **La symétrie du diff n'est qu'un signal, pas une preuve** — seul
`parse()` (16 expressions) et le delta de déséquilibre par ligne
(**113 → 109**, soit exactement les 4 lignes repliées) ont conclu.

## [V1.x — dette de conventions, 17ᵉ incrément] — 2026-09-17 — `io_helpers.R` classé (`io_helpers_error`)

**7 sites sur 7 prouvés à l'exécution (100 %)** — troisième lot intégral.
C10 **48 → 41**, C9 **33 → 32**, dette **82 → 74** (−8 = 7 + 1).

### Modifié

- `R/core/io_helpers.R` : les **7** `stop()` non classés passent à
  `stop(errorCondition(<msg>, class = "io_helpers_error"))` — sites **988,
  1084, 1093, 1128, 1132, 1135, 1142**. **Tous à un seul argument** ⇒ **aucun
  `paste0()`** à ajouter (1084 est un `stop(paste0(...))` multi-ligne, mais le
  `paste0()` est **dans** l'appel, pas un second argument).
- `tests/testthat/test-core-io.R` → **`test-core-io-helpers.R`** (`git mv`) :
  l'ancien nom ne correspondait à **aucune des 4 formes** que C9 accepte, si
  bien que la garde signalait `io_helpers.R` « sans test éponyme » **alors que
  ce fichier le testait déjà (292 lignes, ~24 blocs)**. ⇒ **C9 baisse de 33 à
  32 sans écrire une seule ligne de test** (mesuré). Deuxième occurrence du
  geste de [§2bz.2](docs/archive/STATUS_JOURNAL.md).
- Le fichier est **étendu** : 2 assertions **préfixe seul** montées au message
  entier + classe ([§2bx.3](docs/archive/STATUS_JOURNAL.md)), 5 sites newly covered, garde-fou du mock, verrou
  source. Rouge **8 échecs** → vert **69 PASS**.

### 🔴 Un en-tête périmé DEUX FOIS, et une justification réfutée par la mesure

L'en-tête du fichier annonçait `test-helpers_io.R — pure-function tests for
helpers_io.R` : **les deux noms sont faux**. Il déclarait surtout
`remap_gene_ids_to_symbol()` **hors de portée**, « they need
`org.Hs.eg.db`/`org.Mm.eg.db` or `hdf5r` and a real 10x-style dataset ».

**Mesuré faux** : ses **4** sites C10 (1128, 1132, 1135, 1142) sont **tous**
atteignables — 1128 et 1142 sans aucun paquet, 1132 et 1135 par la technique
d'environnement enfant ([§2bz.3](docs/archive/STATUS_JOURNAL.md)). ⚠️ Même défaut qu'au [§2bp](docs/archive/STATUS_JOURNAL.md) : **une
*justification* périmée est aussi dangereuse qu'un chiffre périmé**, et c'est
la **technique disponible** qui décide de la portée, pas la présence du paquet.

### 🟢 Ce que le lot apporte

- **Troisième lot d'affilée à 100 %** (après [§2bz](docs/archive/STATUS_JOURNAL.md) et [§2ca](docs/archive/STATUS_JOURNAL.md)) : la technique de
  mock des gardes de dépendance est devenue le levier principal de preuve.
- **Le site 1142 vit dans un `switch()`** (`affy_probe = if (organism ==
  "human") "PROBEID" else stop(...)`) : un `stop()` en position
  d'**expression**, pas d'instruction — la conversion fonctionne à
  l'identique, mais un dénombrement naïf pourrait le manquer.
- ⚠️ **`io_helpers.R` est sourcé par ~40 fichiers de test** : c'est le lot le
  plus **largement connecté** du chantier ⇒ la suite complète y est plus
  nécessaire qu'ailleurs (elle a été jouée).

## [V1.x — dette de conventions, 16ᵉ incrément] — 2026-09-17 — `spatial_reference.R` classé (`spatial_reference_error`)

**12 sites sur 12 prouvés à l'exécution (100 %)** — deuxième lot intégralement
prouvé. C10 **60 → 48**, C9 **34 → 33**, dette **95 → 82** (−13 = 12 + 1).

### Modifié

- `R/spatial/spatial_reference.R` : les **12** `stop()` non classés passent à
  `stop(errorCondition(<msg>, class = "spatial_reference_error"))` — sites
  **36, 48, 75, 87, 93, 106, 110, 132, 188, 190, 200, 223**.
- **3** sites sont multi-arguments (**36**, **87**, **106**) ⇒ **`paste0()`** ;
  les 9 autres sont à un seul argument. ⚠️ **87** et **106** le sont **sur deux
  lignes** (deux littéraux accolés) ⇒ le compte de parenthèses doit être fait
  sur l'**appel**, pas sur la ligne.
- Le site **60** (`stop(paste0(...), call. = FALSE)`, multi-ligne) est **laissé
  en place** : déjà conforme, comme les 4 de `pathway_helpers.R` ([§2bp](docs/archive/STATUS_JOURNAL.md)).
- `tests/testthat/test-spatial-reference.R` : **créé** — verrou source + **12
  preuves d'exécution**. Rouge mesuré **13 échecs** (12 classes + verrou
  `n = 12`) → vert **27 PASS**.

### 🔴 Une hypothèse de défaut de garde MESURÉE… et réfutée

En lisant le fichier, le site **75** semblait être un **faux positif** : la
ligne 60 porte `call. = FALSE` — que C10 **exempte** — et un angle mort
« ligne par ligne » (le même que [§2bg](docs/archive/STATUS_JOURNAL.md)) l'aurait manqué.

**Mesure : 0 faux positif sur les 60 sites.** La ligne **60** est correctement
**exemptée** et la ligne **75** — un `stop()` *distinct*, sans `call. = FALSE`
— correctement **signalée**. La garde gère déjà le multi-ligne pour C10.

⇒ **Un soupçon de défaut de garde se mesure comme une réduction de dette** :
ici la mesure a **innocenté** la garde. C'est le 5ᵉ examen de ce type et le
**premier dont le verdict est « la garde a raison »** — les quatre précédents
([§2bg](docs/archive/STATUS_JOURNAL.md), [§2bh](docs/archive/STATUS_JOURNAL.md), [§2bi](docs/archive/STATUS_JOURNAL.md), [§2br](docs/archive/STATUS_JOURNAL.md)) l'avaient mise en défaut.

### 🟢 Ce que le lot apporte de plus

- **Les gardes d'absence de dépendance sont devenues routinières** : 87, 93 et
  106 sont atteintes par la technique d'environnement enfant ([§2bz.3](docs/archive/STATUS_JOURNAL.md)), avec le
  **garde-fou de non-fuite**. Deuxième lot d'affilée à **100 %**.
- **Un `.RData` « à objet unique non exploitable »** atteint le site 75 — il
  faut écrire un objet d'une classe inattendue, sinon c'est la garde
  « > 1 objet » (site 60) qui tire à sa place.

## [V1.x — dette de conventions, 15ᵉ incrément] — 2026-09-17 — `spatial_stats.R` classé (`spatial_stats_error`)

**16 sites sur 16 prouvés à l'exécution (100 %)** — le seul lot **intégralement**
prouvé du chantier. C10 **76 → 60**, C9 **35 → 34**, dette **112 → 95** (−17).

### Modifié

- `R/spatial/spatial_stats.R` : les **16** `stop()` non classés passent à
  `stop(errorCondition(<msg>, class = "spatial_stats_error"))`.
- **Aucun `paste0()`** : les 16 sites sont à un seul argument — à la différence
  de `sc_trajectory.R` (1 site) et `sc_plotting.R` (5 sites). `paste0()` n'est
  nécessaire **que** pour les messages multi-arguments (C16).
- `tests/testthat/test-utils_spatial_stats.R` → **`test-spatial-stats.R`**
  (`git mv`) : l'ancien nom était un **pointeur périmé** (le fichier testé
  s'appelait `R/utils_spatial_stats.R` avant son déplacement). C9 ne le voyait
  donc **pas** et signalait `R/spatial/spatial_stats.R` comme « sans test
  éponyme » **alors qu'il portait déjà 15 tests**. ⇒ Le renommage fait baisser
  C9 **sans écrire une seule ligne de test**.
- Le fichier est **étendu**, pas dupliqué (règle 3) : 8 assertions préfixe-seul
  montées au **message entier + classe**, verrou source ajouté, 4 tests ajoutés.
  Rouge mesuré **17 échecs** (16 classes + verrou `n = 16`) → vert **54 PASS**.

### 🟢 La nouveauté : rendre prouvables les gardes d'absence de dépendance

Les 3 sites **49**, **110**, **305** sont des gardes `requireNamespace("RANN")`.
RANN **est** installé ⇒ ces sites étaient **inatteignables** (13/16 seulement).
Deux techniques mesurées :

- ❌ `testthat::with_mocked_bindings(..., .env = globalenv())` **échoue** ici :
  « No packages loaded with pkgload » — il exige un `.env` adossé à un paquet,
  ce que `globalenv()` n'est pas.
- ✅ **Environnement enfant** : copier la fonction dans `new.env(parent =
  globalenv())` où vit le mock, puis `environment(f) <- e`. La copie voit
  d'abord `e` (le mock), puis `globalenv`, puis `base` ⇒ **`globalenv` n'est
  jamais modifié** et le harness testthat n'est pas menacé. Un **garde-fou** de
  la technique (le mock ne doit pas fuiter) est ajouté au test.

Le motif `requireNamespace() + stop()` court sur **14 lignes / 5 fichiers** de
`R/` — dont `spatial_io.R` (11 sites) et `io_helpers.R` (7) encore en dette :
la technique se paie donc sur la suite du chantier.

### ⚠️ Deux pièges mesurés dans cette passe

1. **Parenthèse manquante** : transformer `stop(X)` en
   `stop(errorCondition(X, class = …))` exige d'insérer la classe **ET** une
   fermeture — sinon `errorCondition(` absorbe la `)` finale et `stop(` reste
   ouvert. Ni le comptage de tokens (`stop(` 16, `errorCondition(` 16) ni
   `git diff --stat` (**16/16 symétrique**) ne voient le bug : **seul
   `parse()`** le voit. Le fichier a dû être restauré puis reconverti.
2. **Fins de ligne** : le dépôt est **mixte** (pas de `.gitattributes`,
   `core.autocrlf=true`) — un `git checkout --` a rebascoulé ce fichier de LF
   vers **CRLF** alors que son voisin `spatial_io.R` est en LF. Git normalise
   en LF à l'index, on ré-écrit donc en LF : sinon un outil qui ne normalise
   pas voit un diff portant sur **369 lignes** au lieu de **16**.

## [V1.x — dette de conventions, 14ᵉ incrément] — 2026-09-17 — `sc_plotting.R` classé (`sc_plotting_error`)

**16 sites sur 17 prouvés à l'exécution (94 %)** — le meilleur ratio du chantier,
et le deuxième lot qui paie **deux fois** (C9 −1 *et* C10 −17).

### Modifié

- `R/sc/sc_plotting.R` : les **17** `stop()` non classés passent à
  `stop(errorCondition(<msg>, class = "sc_plotting_error"))`.
- **5** sites sont multi-arguments (**63**, **194**, **200**, **278**, **284**)
  ⇒ **`paste0()`**. Les 12 autres sont à un seul argument ⇒ pas de `paste0()`.
- `tests/testthat/test-sc-plotting.R` : **créé** — verrou source + **16 sites
  prouvés à l'exécution**.

### 🟢 Pourquoi un tel ratio : un dispatcheur PUR

`build_sc_viz_plot(obj, cfg, …)` est la **seule** fonction publique du fichier :
un dispatcheur sur `cfg$type`. Chaque branche commence par une **garde**
(`if (…) stop(…)`) ⇒ **une entrée triviale suffit**, et **aucun calcul Seurat**
n'est atteint. C'est la forme la plus favorable du chantier : **94 %**, contre
**71 %** pour `sc_trajectory.R` ([§2bx](docs/archive/STATUS_JOURNAL.md)) et **44 %** pour `sc_helpers.R` ([§2bq](docs/archive/STATUS_JOURNAL.md)).
⇒ **La forme du fichier prédit la joignabilité mieux que son domaine.**

### ⚠️ Deux leçons de mesure

- **Un message à queue VOLATILE peut quand même être asséré.** Site **200** :
  `stop("FindMarkers: ", e$message)` — la queue est **interne à Seurat**, donc
  instable d'une version à l'autre. On n'assère donc pas sa **valeur** mais le
  **préfixe** *et* une **longueur strictement supérieure** à celle du préfixe nu :
  sans `paste0()`, `errorCondition()` ne garderait que `"FindMarkers: "`
  (13 caractères). ⇒ **La longueur est un détecteur de troncature là où le
  contenu ne peut pas l'être.**
- **Un `stop()` partagé par 6 branches se convertit en une seule substitution**
  (`"Aucun gène valide"` × 6, branches `feature`, `violin`, `stacked_violin`,
  `ridge`, `dot`, `heatmap`) — mais il faut **vérifier le compteur
  d'occurrences**, sinon on convertit à l'aveugle.

### Le seul site non joignable : 201

`if (!nrow(markers)) stop("Aucun marqueur trouvé")` exige que `FindMarkers()`
**réussisse** et rende **0 ligne** — or un objet dégénéré la fait **échouer**
(c'est le site 200), pas réussir à vide. Couvert par le **verrou source**.

### Vérifié

- **Test ROUGE d'abord** : **`FAIL 17 | WARN 0 | PASS 18`** — 1 verrou source
  listant exactement les **17** lignes + **16** assertions de classe ⇒ **vert** :
  **`FAIL 0 | WARN 0 | SKIP 0 | PASS 35`**.
- 🟢 **Les 16 messages étaient déjà identiques AVANT la conversion** : l'invariant
  de non-régression est établi **avant**, pas constaté après.
- `parse()` **OK** ; **C10 fichier : 0** ; **C16 fichier : 0** ; diff **17/17**
  symétrique ; **286** lignes (inchangé) ; **17** tokens `stop(` avant **et**
  après ⇒ aucun site `call. = FALSE` n'a été touché.
- Garde : **0 erreur / 112 avert.** — **C10 93 → 76**, **C9 36 → 35**,
  **C11 = 1** (faux positif connu), **C6 = 0**, **C16 = 0**.

### Constaté (listé, NON corrigé)

- `R/` : **62** sites restants sur **12** fichiers (`spatial_stats.R` 16,
  `spatial_reference.R` 12, `spatial_io.R` 11, `io_helpers.R` 7…).
- `modules/` : **14** sites, **tous** réactifs ⇒ verrou source seul.
- ⚠️ `sc_plotting.R` appelle `ggtitle()` / `theme_*()` **sans préfixe** : le
  test doit **attacher** ggplot2, sinon `could not find function "ggtitle"`.
  🟢 **C'est le témoin nominal qui l'a vu, pas le verrou source** — un verrou
  statique ne dit rien sur les dépendances d'exécution.


## [V1.x — dette de conventions, 13ᵉ incrément] — 2026-09-17 — `sc_trajectory.R` classé (`sc_trajectory_error`)

**Le plus gros lot depuis le 7ᵉ** (21 sites) et **le premier qui paie deux fois** :
le test éponyme exigé par C9 fait baisser **C9 et C10 à la fois**. 🟢 **Bascule
vers `R/`** décidée par mesure ([§2bw.6](docs/archive/STATUS_JOURNAL.md)) : le front `modules/` est épuisé côté
preuve (14 sites restants, **tous** dans des serveurs réactifs — revérifié ici
par une méthode **validée** sur un cas dont la vérité terrain est connue).

### Modifié

- `R/sc/sc_trajectory.R` : les **21** `stop()` non classés passent à
  `stop(errorCondition(<msg>, class = "sc_trajectory_error"))`.
- **Un seul** des 21 est multi-arguments (ligne **73**, `n_cells`) ⇒ **`paste0()`**
  obligatoire (C16). Les 20 autres sont déjà à un seul argument ⇒ **pas** de
  `paste0()` : l'ajouter par réflexe serait du bruit, pas une preuve.
- La ligne **224** (`stop(..., call. = FALSE)`) est **laissée en place** — déjà
  conforme ; son unification relève de la décision ouverte `state`/`class`.
- `tests/testthat/test-sc-trajectory.R` : **créé** — verrou source + **15 sites
  prouvés à l'exécution** (message **entier** + classe observée).

### 🔴 Le piège que ce lot a mesuré : une assertion-PRÉFIXE est aveugle

`test-sc-helpers.R` exerçait déjà `calculate_pseudotime()` et assérait
`"Invalid root cell index"`. Or le site **73** est **multi-arguments** : sans
`paste0()`, `errorCondition()` aurait **tronqué** le message juste après
« between 1 and » — et l'assertion existante aurait **quand même passé**, parce
qu'elle ne vise qu'un **préfixe**. ⇒ Le nouveau test assère les messages
**entiers**, au caractère près (`n_cells` et le point final inclus). C'est sa
raison d'être — pas une redite de `test-sc-helpers.R`.

### Les 15 sites joignables

Tous sont des **gardes** (`if (…) stop(…)`) dans 6 fonctions **pures** de
premier niveau — `R/` n'a pas droit au Shiny (C2), donc tout est appelable en
R pur, **sans** `testServer()` :

| Fonction | Sites |
|---|---|
| `calculate_pseudotime` | 57, 60, 66, **73** |
| `calculate_slingshot_pseudotime` | 183, 187, 193, 197 |
| `plot_trajectory` | 301 |
| `plot_slingshot_trajectory` | 365, 369 |
| `plot_pseudotime_distribution` | 439, 453 |
| `plot_genes_vs_pseudotime` | 485, 493 |

### 🔴 Les 6 sites non joignables, par ABSENCE DU DÉFAUT (mesuré)

- **48**, **51**, **175** : `RANN`, `igraph` et `slingshot` sont **installés** ⇒
  les branches « paquet manquant » ne s'exécutent jamais ;
- **93**, **135**, **236** : exigent un échec **interne** (graphe kNN vide,
  pseudotemps entièrement non fini, dimension Slingshot inattendue).

### Vérifié

- **Test ROUGE d'abord** : **`FAIL 16 | WARN 4 | PASS 16`** — 1 verrou source
  listant exactement les **21** lignes + **15** assertions de classe ⇒ **vert** :
  **`FAIL 0 | WARN 0 | SKIP 0 | PASS 32`**.
- 🟢 **Les 15 messages étaient déjà identiques AVANT la conversion** :
  l'invariant de non-régression est établi **avant**, pas constaté après.
- `parse()` **OK** ; **C10 fichier : 0** ; **C16 fichier : 0** ; diff **21/21**
  symétrique, **531** lignes (inchangé), **LF** et saut final préservés.
- Garde : **0 erreur / 130 avert.** — **C10 114 → 93**, **C9 37 → 36**,
  **C11 = 1** (faux positif connu), **C6 = 0**, **C16 = 0**.

### Constaté (listé, NON corrigé)

- `R/` : **79** sites restants sur **13** fichiers (`sc_plotting.R` 17,
  `spatial_stats.R` 16, `spatial_reference.R` 12, `spatial_io.R` 11…).
- `modules/` : **14** sites, **tous** réactifs ⇒ verrou source seul.
- **Le rapport coût/preuve confirme `R/`** : un lot y rapporte
  `sites C10 + 1 C9`, contre `sites C10` seulement dans `modules/` — et la
  classe y reste **observable**.


## [V1.x — dette de conventions, 12ᵉ incrément] — 2026-09-17 — `mod_sc_annotation.R` classé (`sc_annotation_error`)

**Premier lot choisi par le critère de [§2bv](docs/archive/STATUS_JOURNAL.md)** (« expose-t-il des helpers purs ? »)
plutôt que par « a-t-il un test éponyme ? ». Et **le dernier lot de `modules/`
prouvable à l'exécution** : après lui, les 14 sites restants sont **tous** dans
des serveurs réactifs.

### Modifié

- `modules/sc/mod_sc_annotation.R` : les **4** `stop()` non classés passent à
  `stop(errorCondition(<msg>, class = "sc_annotation_error"))`.
- **Aucun** des 4 n'est multi-arguments : les messages passent déjà par
  `sprintf()` / `paste()` ⇒ **pas de `paste0()` à ajouter** (C16 reste à 0).
  ⚠️ C'est le **premier lot** du chantier dans ce cas — le réflexe « envelopper
  dans `paste0()` » ne s'applique qu'aux `stop()` à **plusieurs arguments**.
- `tests/testthat/test-mod-sc-annotation.R` : **créé** — verrou source + **2 sites
  prouvés à l'exécution** (message **identique** + **classe observée**).

### Classe : `sc_annotation_error`

Qualifiée par le **domaine** (annotation SingleR), famille `sc_*` :
`sc_import_error` · `sc_pseudobulk_error` · `sc_helpers_error` · `sc_multi_error`.

### 🎯 Le critère [§2bv](docs/archive/STATUS_JOURNAL.md) a fonctionné — et il a prédit la suite

Mesuré au [§2bv.6](docs/archive/STATUS_JOURNAL.md) #4 : `mod_sc_annotation.R` était le **seul** des 13 fichiers
restants dont **tous** les sites vivent dans des **helpers purs** (`.load_ref`,
`.run_singler_safe`), les 14 autres étant dans des serveurs réactifs. Le lot a
donc été pris en premier — et il a livré ce que le critère promettait :
**2 sites joignables sur 4 sans `testServer()`**.

### Les 2 sites joignables, et le moyen de les atteindre

- **21** : `.load_ref("bogus")` — instantané (`switch` → dernière branche).
- **108** : le chemin « gros jeu » exige `ncol(obj) > 100000L`. 💡 Une
  **`dgCMatrix` creuse** de 100 001 colonnes **vides** ne coûte que **~1,1 s** et
  quelques Mo : le seuil se franchit **sans** construire un gros jeu de données.
  Le test assère d'ailleurs `.annot_is_big(obj)` comme **garde-fou** — si le
  seuil changeait, le test le dirait au lieu de prouver autre chose.

### 🔴 Les 2 sites non joignables, par ABSENCE DU DÉFAUT (mesuré)

- **91** est la branche `else` de `requireNamespace("AnnotationDbi") &&
  requireNamespace(orgdb_pkg)` — or **AnnotationDbi**, **org.Hs.eg.db** et
  **org.Mm.eg.db** sont **installés** ⇒ elle ne s'exécute jamais ;
- **144** exige `.load_ref()`, donc un **téléchargement celldex** (ExperimentHub)
  ⇒ **réseau**. **Non testé volontairement** : un test réseau pendrait (le dépôt
  « live-gate » déjà le smoke GEO, [§2ba](docs/archive/STATUS_JOURNAL.md)).

### ⚠️ Un piège d'écriture, pas de lecture

La conversion du site 91 a **dupliqué une ligne** (`head(test_ids, …)`) parce que
la ligne de fermeture portait déjà son propre contenu. détecté au `git diff`
(**7 insertions / 6 suppressions** au lieu de 6/6) ⇒ corrigé ⇒ **6/6**.
Rappel : quand on remplace une ligne **de fermeture**, vérifier qu'elle
n'embarque **pas** la précédente.

### Vérifié

- **Test ROUGE d'abord** : **3** échecs — 1 verrou source listant exactement les
  **4** lignes (21, 91, 108, 144) + **2** assertions de classe ⇒ **vert** :
  `failed=0 passed=6` (était `failed=3 passed=3`).
- `parse()` **OK** ; **C10 fichier : 0** ; **C16 fichier : 0**.
- Conversion au niveau des **octets** : **LF** et **saut de ligne final**
  préservés (diff final **6 insertions / 6 suppressions**, aucun bruit CRLF).
- Garde : **C10 118 → 114**, total **0 erreur / 156 → 152 avert.**

### Constaté (listé, NON corrigé)

- **14** sites de `modules/` restants, **tous** dans des serveurs réactifs
  (`mod_sc.R` 2 · `mod_spatial_cluster.R` 2 · 10 fichiers à 1 site) ⇒
  **inobservables** ([§2bv.3](docs/archive/STATUS_JOURNAL.md)) : le front `modules/` devient **verrou source seul**.
- **100** sites de `R/` restants ⇒ **C9 + C10 simultanément**. ⚠️ **Le rapport
  coût/preuve bascule maintenant en faveur de `R/`** : c'est le seul front où
  l'on peut encore prouver la classe.


## [V1.x — dette de conventions, 11ᵉ incrément] — 2026-09-17 — `mod_sc_pseudobulk.R` classé (`sc_pseudobulk_error`)

**Le lot de `modules/` le mieux prouvé à ce jour** — et il renverse une
conclusion tenue depuis [§2bs](docs/archive/STATUS_JOURNAL.md) : la classe d'erreur **n'est pas** inobservable
dans `modules/`, elle l'était dans les fichiers qu'on avait traités.

### Modifié

- `modules/sc/mod_sc_pseudobulk.R` : les **6** `stop()` non classés passent à
  `stop(errorCondition(<msg>, class = "sc_pseudobulk_error"))`.
- **1 des 6** a un message en **plusieurs arguments** ⇒ **`paste0()` obligatoire**
  (C16) : le site **106**, `stop("Moins de 2 groupes (echantillon", …)`. Sans
  `paste0()`, le message serait **tronqué** à `"Moins de 2 groupes (echantillon"`.
- `tests/testthat/test-mod-sc-pseudobulk.R` : **créé** — verrou source + **4 sites
  prouvés à l'exécution** (message **identique** + **classe observée**).

### Classe : `sc_pseudobulk_error`

Qualifiée par le **domaine** (pseudobulk côté SC), dans la famille `sc_*` déjà
présente : `sc_import_error` ([§2bu](docs/archive/STATUS_JOURNAL.md)) · `sc_helpers_error` ([§2bq](docs/archive/STATUS_JOURNAL.md)) · `sc_multi_error`.

### 🟢 La CLASSE est observable — première sur le front `modules/`

Depuis [§2bs](docs/archive/STATUS_JOURNAL.md), la règle était : *dans `modules/`, les réactifs **avalent**
l'erreur, donc la classe n'est pas observable ; on prouve par verrou source +
témoin de message*. Ce fichier **n'est pas** dans ce cas : il expose deux
fonctions **top-level PURES** — `aggregate_pseudobulk_counts()` et
`resolve_pseudobulk_condition()` — dont les **4** `stop()` de validation sont
joignables **en R pur**, sans `testServer()`. `tryCatch(error = function(e)
class(e))` y rend donc la **classe**.

⇒ **Le front `modules/` n'est pas uniforme.** Le critère pertinent n'est pas
« `modules/` vs `R/` » mais **« le fichier expose-t-il des helpers purs ? »**.

### 🔴 2 sites restent inobservables — et pourquoi (mesuré)

**309** et **316** vivent dans `observeEvent(input$run_aggregate)` :

- leur `tryCatch(..., error =)` n'appelle **pas** `add_log` — il écrit dans un
  `reactiveVal` (`agg_status_rv`) + `showNotification` ;
- et **`session$getOutput()` ne fonctionne PAS** dans cet environnement : éprouvé
  sur un module **trivial** (`output$txt <- renderText("bonjour")`), il échoue
  avec « output$txt hasn't been defined yet ». Ce n'est donc **pas** un défaut du
  module.

Ces 2 sites sont couverts par le **verrou source seul**.

### ⚠️ Dénombrer n'est toujours pas greper `stop(`

**7** occurrences du token pour **6** sites : la ligne **149** est un **commentaire**
roxygen qui mentionne `stop()` — le même piège qu'au [§2bu.4](docs/archive/STATUS_JOURNAL.md), dans un fichier
différent : il se répète, il n'était pas local.

### Vérifié

- **Test ROUGE d'abord** : **5** échecs — 1 verrou source listant exactement les
  **6** lignes attendues (79, 92, 106, 154, 309, 316) + **4** assertions de classe
  ⇒ **vert** : `failed=0 passed=11` (était `failed=5 passed=6`).
- `parse()` **OK** ; **C10 fichier : 0** ; **C16 fichier : 0**.
- Conversion écrite au niveau des **octets** : **LF** et **saut de ligne final**
  préservés (diff = **10 insertions / 8 suppressions**, aucun bruit CRLF).
- Garde : **C10 124 → 118**, total **0 erreur / 162 → 156 avert.**

### Constaté (listé, NON corrigé)

- **18** sites de `modules/` restants : `mod_sc_annotation.R` 4 · `mod_sc.R` 2 ·
  `mod_spatial_cluster.R` 2 · puis 10 fichiers à 1 site.
- **100** sites de `R/` restants (`sc_trajectory.R` 21 · `sc_plotting.R` 17 ·
  `spatial_stats.R` 16 …) : **C9 + C10 simultanément**.
- 🔴 **0 des 14 fichiers de `modules/` alors restants n'avait de test éponyme**
  (mesuré au [§2bu.6](docs/archive/STATUS_JOURNAL.md) #4) : le critère est épuisé sur les **deux** fronts.


## [V1.x — dette de conventions, 10ᵉ incrément] — 2026-09-17 — `mod_import_sc.R` classé (`sc_import_error`)

**Le domaine `modules/import/` est bouclé** (bulk → `bulk_import_error`, GEO →
`geo_import_error`, SC → `sc_import_error`) : les trois portes d'entrée de
l'application sont désormais **classées**, alors que c'est la couche la plus
visible pour l'utilisateur.

### Modifié

- `modules/import/mod_import_sc.R` : les **6** `stop()` non classés passent à
  `stop(errorCondition(<msg>, class = "sc_import_error"))`.
- **1 des 6** a un message en **plusieurs arguments** ⇒ **`paste0()` obligatoire**
  (C16) : `stop("Format non supporté : ", ext)` (site 700). Sans `paste0()`, le
  message serait **tronqué** à `"Format non supporté : "` — exactement le défaut
  que la règle C16 interdit.
- `tests/testthat/test-mod-import-sc.R` : **créé** (verrou source + témoin de
  message d'exécution). ⚠️ `C9` n'oblige à un test éponyme **que dans `R/`** —
  ici il a été écrit **parce qu'il était le seul moyen de prouver** la
  conversion, pas par obligation de garde.

### Classe : `sc_import_error`

Qualifiée par la **source** de l'import, comme le dépôt le fait déjà :
`rdata_import_error` · `communication_import_error` · `bulk_import_error` ·
`geo_import_error`.

### Preuve à deux niveaux — la répartition est MESURÉE

**5 sites sur 6 sont INJOIGNABLES**, et pour des raisons vérifiées une à une :

- **690** (`.h5ad` sans convertisseur) et **695** (`loomR` requis) : les quatre
  paquets concernés (`BPCells`, `zellkonverter`, `sceasy`, `loomR`) sont
  **installés** (mesuré) ⇒ les deux sites ne se déclenchent jamais ;
- **489** et **531** exigent un upload **tronqué** (intégrité) ;
- **627** exige un dossier 10x **sans** `matrix.mtx` ni `.h5`.

Le site **700** est donc le **seul** joignable, et il est **avalé** par le
`tryCatch` réactif du serveur ⇒ la **classe n'est pas observable**, seul le
**message** l'est, dans les logs. L'assertion porte sur la **valeur de
l'extension** (`".unsupportedext"`), qui **disparaît** si `paste0()` manque.

⚠️ Le dénombrement n'est **pas** un `grep` de `stop(` : la ligne **650** porte
déjà `call. = FALSE` (exempte, **laissée telle quelle**) et la ligne **303**
n'est qu'un **commentaire** qui mentionne `stop()`. Compter les occurrences du
token aurait donné **8** au lieu de **6**.

### Vérifié

- **Test ROUGE d'abord** : **1** échec listant exactement les **6** lignes
  attendues (489, 531, 627, 690, 695, 700) ⇒ **vert** : `failed=0 passed=3`
  (était `failed=1 passed=2`).
- `parse()` **OK** ; **C10 fichier : 0** ; **C16 fichier : 0**.
- Conversion écrite au niveau des **octets** : **LF** et **saut de ligne final**
  préservés (diff = **8 insertions / 7 suppressions**, aucun bruit CRLF).
- Garde : **C10 130 → 124**, total **0 erreur / 168 → 162 avert.**
- **Suite complète** (obligatoire — le code applicatif est touché) : **103**
  fichiers, `failed=0 passed=5965 error=0 skipped=1` — **+3**, sans écart
  inexpliqué (le seul SKIP reste le smoke GEO **live**). ⚠️ Lancer
  `tools/run_full_suite.R` : `tools/run_tests.R` **sans argument** n'exécute que
  **30** fichiers et rend un TOTAL vert trompeur.

### Constaté (listé, NON corrigé)

- **24** sites de `modules/` restants : `mod_sc_pseudobulk.R` 6 ·
  `mod_sc_annotation.R` 4 · `mod_sc.R` 2 · `mod_spatial_cluster.R` 2 ·
  `mod_import_spatial.R` 1 · puis 9 fichiers à 1 site.
- **100** sites de `R/` restants (`sc_trajectory.R` 21 · `sc_plotting.R` 17 ·
  `spatial_stats.R` 16 …) : le critère « test éponyme » y est **épuisé** ⇒ le
  prochain lot est **C9 + C10 simultanément**.


## [V1.x — dette de conventions, 9ᵉ incrément] — 2026-09-17 — `mod_geo.R` classé (`geo_import_error`)

**Deuxième fichier de `modules/` converti**, et le **premier qui dispose d'un
test éponyme préexistant** (`test-mod-geo.R`) : la preuve d'exécution n'est donc
pas à construire, elle est **étendue** (règle 3).

### Modifié

- `modules/import/mod_geo.R` : les **8** `stop()` non classés passent à
  `stop(errorCondition(<msg>, class = "geo_import_error"))`.
- **4 des 8** ont un message en **plusieurs arguments** ⇒ **`paste0()` obligatoire**
  (C16) : `stop("Extension non supportée : ", ext)`, les deux `stop("Impossible
  de … ", conditionMessage(e))`, et le `stop()` bi-ligne « aucun fichier
  tabulaire ». Sans `paste0()`, chacun serait **tronqué** à son premier argument.
- `tests/testthat/test-mod-geo.R` : **2** tests ajoutés (verrou source +
  invariant de message) — **aucun** fichier de test créé.

### Classe : `geo_import_error`

L'import est qualifié par sa **source**, comme le dépôt le fait déjà :
`rdata_import_error` · `communication_import_error` · `bulk_import_error`.

### Preuve à deux niveaux — la répartition est MESURÉE

**5 sites sur 8 sont INJOIGNABLES**, et pour des raisons vérifiées une à une :

- les **4** de `.geo_fetch` (358, 366, 393, 412) sont **live-gated** :
  `GEOquery` **est installé** (mesuré), donc le site 358 « paquet requis » ne se
  déclenche jamais, et les trois autres exigent le **réseau** ;
- le site **465** (« readxl requis ») exige `readxl` **absent** — il est installé.

Les **3** sites joignables (468, 481, 498) passent par `.load_counts_file`, en R
pur et hors réseau — mais cette fonction **AVALE** l'erreur :
`tryCatch(..., error = function(e) list(ok = FALSE, msg = conditionMessage(e)))`
⇒ la **classe n'est pas observable**, seul le **message** l'est, dans `res$msg`.

Le site **468** est le décisif : c'est un `stop()` multi-arguments dont le
message serait tronqué à `"Extension non supportée : "`. L'assertion porte donc
sur la **valeur de l'extension** (`"unsupportedext"`, en ASCII pur) — elle
échoue immédiatement si `paste0()` manque. Le site 498 est couvert par un test
**déjà vert** (« refuses non-numeric matrices »), qui sert ici de **témoin**.

### Vérifié

- **Test ROUGE d'abord** : **1** échec listant exactement les **8** lignes
  attendues (358, 366, 393, 412, 465, 468, 481, 498) ⇒ **vert** :
  `failed=0 passed=81` (était `failed=1 passed=80`).
- `parse()` **OK** ; **C10 fichier : 0** ; **C16 fichier : 0**.
- **8/8** listes d'arguments **identiques** (espaces normalisés). ⚠️ Le site 412
  a été **replié sur une ligne** (515 → 514) : une comparaison **octet à octet de
  la ligne** aurait signalé une fausse différence — on compare les **arguments**,
  pas les lignes.
- Conversion écrite au niveau des **octets** : **LF** et **saut de ligne final**
  préservés (le piège CRLF du lot précédent ne s'est pas reproduit).
- Garde : **C10 138 → 130**, total **0 erreur / 176 → 168 avert.**

### Constaté (listé, NON corrigé)

- **30** sites de `modules/` restants : `mod_import_sc.R` 6 ·
  `mod_sc_pseudobulk.R` 6 · `mod_sc_annotation.R` 4 · `mod_sc.R` 2 ·
  `mod_spatial_cluster.R` 2 · puis 10 fichiers à 1 site.
- ⚠️ **Le « test éponyme » n'existe que pour une minorité de `modules/`**
  (ici `test-mod-geo.R`) : `C9` ne couvre que `R/`, donc rien n'oblige à en
  écrire un. Une **règle sœur** reste une décision à prendre ([§2bs.6](docs/archive/STATUS_JOURNAL.md) #3).

## [V1.x — dette de conventions, 8ᵉ incrément] — 2026-09-17 — `mod_import_bulk.R` classé (`bulk_import_error`)

**Premier fichier de `modules/` converti** — et il n'aurait pas pu l'être la
veille : ces **10** sites faisaient partie des **48** que la garde ne voyait pas
(entrée « angle mort mesuré » du même jour). Le chantier touche enfin la couche
que l'utilisateur voit en premier.

### Modifié

- `modules/import/mod_import_bulk.R` : les **10** `stop()` non classés passent à
  `stop(errorCondition(<msg>, class = "bulk_import_error"))`. **Aucun** n'a un
  message en plusieurs arguments ⇒ pas de `paste0()` nécessaire (mesuré : 5
  littéraux, 3 `sprintf()`, 2 `paste()`).
- `tests/testthat/test-mod-import-bulk.R` (nouveau).

### Classe : `bulk_import_error`, pas `import_error`

Le dépôt qualifie déjà l'import par son **objet** : `rdata_import_error`,
`communication_import_error`. Un `import_error` nu cohabiterait avec les deux et
prétendrait désigner « l'import » en général. Ici le domaine est l'import
**bulk**, et le nom s'aligne sur la famille `bulk_*` (`bulk_de_error`,
`bulk_gsva_error`, `bulk_merge_error`) — `R/bulk/bulk_import_engine.R` pourra
reprendre la même classe.

### Preuve à deux niveaux — et pourquoi la 2ᵉ est PARTIELLE

**Mesuré, pas supposé** : le fichier n'expose que **deux** fonctions top-level
(l'UI et le serveur) ⇒ les 10 sites vivent dans `mod_import_bulk_server`. Et
`counts_reactive()` enveloppe toute la lecture dans un
`tryCatch(..., error = function(e) { add_log(); showNotification(); NULL })` :
l'erreur est **avalée**. Conséquence : **la classe n'est pas observable à
l'exécution** — mesuré par un appel direct à
`shiny::isolate(counts_reactive())` avec un format non supporté, qui ne lève
rien. Une assertion `expect_error(class = "bulk_import_error")` serait donc un
mensonge.

Le **message**, lui, est observable via `logs()` — et c'est exactement
l'invariant de [§2bn](docs/archive/STATUS_JOURNAL.md) (`errorCondition()` tronque les arguments multiples, défaut
invisible dans le source). D'où :

1. **Verrou source** — `check_c10_error_style()` sur le fichier rend **0** :
   couvre les **10** sites, y compris les 7 injoignables.
2. **Invariant de message à l'exécution** — les **3** sites joignables (403 et
   408 dans `smart_read`, 476 dans `counts_reactive`) : le texte loggué doit
   rester identique. Ces deux assertions étaient **déjà vertes avant** la
   conversion : ce sont des témoins, pas des cibles.

### Vérifié

- **Test ROUGE d'abord** : **1** échec listant exactement les **10** sites
  attendus (397, 403, 408, 476, 974, 977, 980, 991, 1004, 1042) ⇒ puis **vert** :
  `failed=0 passed=3`.
- `parse()` **OK** ; **C10 fichier : 0** ; **C16 fichier : 0** ; **1123** lignes
  avant/après.
- **Messages identiques 10/10, octet à octet** (comparaison sur les octets, pas
  sur des chaînes R — voir « Constaté »).
- Garde : **C10 148 → 138**, total **0 erreur / 186 → 176 avert.**

### Constaté (hors périmètre — listé, NON corrigé)

- ⚠️ **Deux faux diagnostics de mon propre instrument**, corrigés par une
  mesure au bon niveau :
  1. `writeLines(useBytes = TRUE)` écrit de la **CRLF** et **ajoute un saut de
     ligne final** sous Windows — l'original était **LF sans saut final**. Le
     diff montrait 11 lignes changées au lieu de 10. Corrigé : **10
     insertions / 10 délétions**, le reste octet pour octet.
  2. Une comparaison « avant/après » faite sur des **chaînes R** hors locale
     UTF-8 rendait `<U+00E9>` à la place de `é` et signalait **7 fausses
     différences**. Refaite sur les **octets** : **10/10** identiques.
- Les **38** autres sites de `modules/` restent à classer (`mod_geo.R` 8,
  `mod_import_sc.R` 6, `mod_sc_pseudobulk.R` 6, `mod_sc_annotation.R` 4…).

## [V1.x — garde C10 : angle mort mesuré] — 2026-09-17 — la garde sous-mesurait **37,5 %** de la dette

**Ce n'est pas un incrément de conversion : c'est une correction de mesure.**
`run_check()` passait **uniquement `R/`** à `check_c10_error_style()`, alors que
les règles voisines (C5, C7, C11, C13) — et surtout **C16**, sa règle sœur sur
`errorCondition()`, donc le **même sujet** — recevaient `R/ + modules/`. La dette
affichée n'était donc pas la dette réelle.

### Mesuré avant / après

| | Avant | Après |
|---|---|---|
| C10 réellement présent | 160 (`R/` 100 · `modules/` 48 · `tests/` 12) | 160 |
| C10 **mesuré** par la garde | **100** | **148** |
| Total garde | 0 erreur / **138** avert. | 0 erreur / **186** avert. |

Les 48 sites invisibles n'étaient **pas** marginaux : ils couvrent la couche la
**plus visible** de l'application — `mod_import_bulk.R` (10), `mod_geo.R` (8),
`mod_import_sc.R` (6), `mod_sc_pseudobulk.R` (6), `mod_sc_annotation.R` (4) —
avec des messages que l'utilisateur lit réellement (« Package 'GEOquery'
requis », « Aucune paire n'a pu être calculée »). Pendant ce temps, les 5ᵉ, 6ᵉ
et 7ᵉ incréments classaient des **helpers de tracé** au fond de `R/sc/`.
Mesure de plus : **aucun** fichier de `modules/` ne contenait `errorCondition` —
la couche était entièrement inconvertie, contrairement à `bulk_helpers.R` qui
était à moitié fait.

### Modifié

- `tools/check_conventions.R` : `check_c10_error_style(c(r_files, m_files))`
  aligné sur **C16**. `tests/` reste **exclu**, volontairement : les `stop()` de
  **fixtures** ne sont pas du code de production (12 sites).
- `tools/check_conventions.R` : **défaut connexe corrigé** — `.root_dir()`
  mémoïsait `getwd()` au **premier appel** et `.rel()` mémoïsait par **chemin
  seul**. Rejouée depuis un test (testthat place le répertoire de travail dans
  `tests/testthat`), la garde gardait ce préfixe même après un `setwd()` vers la
  racine, et `.rel()` rendait des chemins **absolus** au lieu de relatifs. Les
  deux mémoïsations sont désormais indexées par le répertoire courant : une
  garde doit être **rejouable**.
- `tests/testthat/test-conventions-c10-scope.R` (nouveau) : trois directions —
  la règle **signale** un `stop()` nu et **exempte** `errorCondition` /
  `call. = FALSE` ; la **population** mesurée couvre `R/` **et** `modules/` ;
  `tests/` est **exclu** (décision énoncée, pas un oubli).

### Vérifié

- **Test ROUGE d'abord**, et un **premier rouge était un faux rouge** : il
  échouait sur `length(c10) == 0` — la garde, appelée depuis `tests/testthat`,
  ne trouvait **0 fichier** (chemins relatifs). D'où le correctif `.rel()`
  ci-dessus ; sans lui, le vert n'aurait rien prouvé.
- **Test de MUTATION** : portée remise à `R/` seul ⇒ **exactement 1** échec, et
  c'est la **bonne** assertion (`modules/ absent de la population`), l'assertion
  sur les chemins relatifs restant verte. Le test détecte donc bien le défaut de
  portée, pas l'artefact de répertoire.
- Gardes : conventions **0 erreur / 186 avert.** ; tests de conventions
  `failed=0 passed=69` ; duplication **0/3** et hermeticité **0/0** inchangées.

### Constaté (listé, NON corrigé — règle 9)

- Les **48** sites de `modules/` sont désormais visibles : chacun devra être
  classé à son tour. Le réflexe `errorCondition()` multi-arguments y est
  **déjà interdit** par C16.
- `app.R` et `global.R` portent **0** site C10 (mesuré) : leur exclusion de la
  portée est sans effet aujourd'hui, mais non documentée.

## [V1.x — dette de conventions, 7ᵉ incrément] — 2026-09-17 — `sc_helpers.R` classé (`sc_helpers_error`)

**Troisième fichier converti** (après `bulk_helpers.R` [§2bl](docs/archive/STATUS_JOURNAL.md) et
`pathway_helpers.R` [§2bp](docs/archive/STATUS_JOURNAL.md)) — et le **plus gros lot** de ce chantier à ce jour :
**34** sites d'un coup.

### Modifié

- `R/sc/sc_helpers.R` : les **34** `stop()` non classés passent à
  `stop(errorCondition(<msg>, class = "sc_helpers_error"))`. Le fichier en compte
  **35** : le 35ᵉ est le **re-levé nu `stop(e)`** (ligne ~1205), forme que C10
  **exempte** et qui est **laissée telle quelle**.
- **1** site a un message en **plusieurs arguments** (`stop("Agregation par
  groupe impossible : ", conditionMessage(e2))`) ⇒ enveloppé dans **`paste0()`**.
  Il est **injoignable** depuis un test (double `tryCatch` exigeant > 5000
  cellules **et** deux échecs d'`AverageExpression`) ⇒ couvert par **trois gardes
  statiques indépendantes** : le verrou source, l'assertion du script de
  conversion (`+1 paste0(`) et la règle **C16**.
- `tests/testthat/test-sc-helpers.R` : deux tests à **deux niveaux** — verrou
  **source** (0 signalement C10 sur le fichier, couvre les **19** sites
  injoignables) et assertions **d'exécution** sur les **15 sites joignables**.

### Classe : nommée par le FICHIER, pas par un domaine

`sc_helpers.R` est un **fourre-tout hétérogène** (recherche de gènes, remapping
d'IDs, heatmap, densité 2D, nuage 3D) : **aucun domaine d'analyse unique** ne le
décrit. `sc_error` serait trop large — il cohabite avec `sc_multi_error`,
`sccoda_error`, `milo_error` — et prétendrait désigner « l'erreur single-cell »
générique. §7 demande `class = "<domaine>_error"` ; ici le domaine **est** la
couche de helpers.

### Vérifié

- **Test ROUGE d'abord** : **15 échecs** (34 signalements C10 sur le fichier, et
  14 `simpleError` au lieu de `sc_helpers_error` — les `expect_match` sur les
  messages passaient déjà, seul le **classement** manquait) ⇒ puis **vert** :
  `failed=0 passed=72`.
- **Invariant au bon niveau** ([§2bn](docs/archive/STATUS_JOURNAL.md)) : les 15 déclencheurs rejoués ⇒ messages
  **identiques 15/15** au caractère près ; classes `simpleError|error|condition`
  → `sc_helpers_error|error|condition`.
- **Conversion par SPLICE ÉQUILIBRÉ PARENTHÈSE**, pas par regex de ligne : 6 sites
  sont des `stop(sprintf(...))` **étalés sur plusieurs lignes**. Le script est
  conscient des **chaînes** et des **commentaires** (assertion : **0** `stop(`
  détecté dans une chaîne ou un commentaire).
- `parse()` **OK** ; **1564** lignes avant/après ; **35** `stop(` inchangés ;
  `errorCondition(` 0 → 34 ; `call. = FALSE` **0** ; aucun double emballage ;
  **C16 sur le fichier : 0**.
- Garde : **C10 134 → 100**, total **0 erreur / 172 → 138 avert.** ; duplication
  **0/3** et hermeticité **0/0** inchangées.

### Constaté (hors périmètre — listé, NON corrigé)

- ⚠️ **L'en-tête du fichier est périmé** : il annonce **6 fonctions**
  (`plot_trajectory`, `plot_slingshot_trajectory`, `plot_pseudotime_distribution`,
  `plot_genes_vs_pseudotime`, `calculate_pseudotime`,
  `calculate_slingshot_pseudotime`) qui ont **déménagé dans `sc_trajectory.R`**.
  C12 ne vérifie que la **présence** d'un en-tête, jamais son **exactitude**.
- `find_correlated_genes()` appelée avec un gène **valide** sur un objet **non
  normalisé** échoue avec « no 'dimnames' attribute for array » (mesuré :
  `.get_norm_matrix()` rend une matrice **0×0** sans dimnames faute de couche
  `data`). Message peu explicite, **pas** un défaut produit.

## [V1.x — dette de conventions, 6ᵉ incrément] — 2026-09-17 — `pathway_helpers.R` classé (`pathway_error`)

**Deuxième fichier converti** du chantier de réduction de la dette C10 (après
`bulk_helpers.R`, [§2bl](docs/archive/STATUS_JOURNAL.md)), et **premier lot choisi par un critère MESURÉ** : sur les
14 fichiers sans classe, **3 seulement** avaient un test éponyme.

### Modifié

- `R/core/pathway_helpers.R` : les **18** `stop()` non classés passent à
  `stop(errorCondition(<msg>, class = "pathway_error"))` — 15 littéraux, et
  **3 messages multi-arguments enveloppés dans `paste0()`** (règle C16 : un
  `errorCondition()` à plusieurs arguments positionnels est **tronqué**).
- Les **4** `stop(..., call. = FALSE)` du même fichier sont **laissés en place** :
  ils sont déjà conformes pour C10, et leur unification sur la classe relève de la
  décision ouverte `state`/`class` (règle 9 — mesurée, **non exécutée** ici).
- `tests/testthat/test-pathway-helpers.R` : deux tests, à **deux niveaux** —
  un verrou **source** (le fichier ne contribue plus aucun signalement C10, ce qui
  couvre aussi les 9 sites injoignables) et des assertions **d'exécution** sur les
  **9 sites joignables**.

### Vérifié

- **Test ROUGE d'abord** : **10 échecs** (`simpleError` au lieu de `pathway_error`,
  plus les 18 signalements C10) — puis **vert** : `failed=0 passed=37`.
- **Invariant au bon niveau** ([§2bn](docs/archive/STATUS_JOURNAL.md)) : les 9 sites joignables rejoués ⇒ messages
  **identiques 9/9** au caractère près, classes passées de
  `simpleError|error|condition` à `pathway_error|error|condition`.
- `parse()` **OK** (porte obligatoire après réécriture programmatique) ;
  **685** lignes avant/après ; **22** `stop(` inchangés ; `errorCondition(` 0 → 18 ;
  `call. = FALSE` **4** inchangés ; aucun double emballage.
- Garde : **C10 152 → 134**, total **0 erreur / 190 → 172 avert.** ; duplication
  **0/3** et hermeticité **0/0** inchangées.

### Mesuré (joignabilité des sites)

- Les **9** sites atteignables **sans réseau** sont ceux de la **validation
  d'entrée** : organisme inconnu, aucun gène valide, aucun gène converti, base
  inconnue (ORA et GSEA), colonnes manquantes, trop peu de gènes, échec de
  conversion. Les **9** autres sont les gardes « paquet manquant », **injoignables
  par construction** sur une machine où le paquet est installé.
- ⚠️ L'en-tête de `test-pathway-helpers.R` affirmait que ces fonctions n'avaient
  « pas de tranche de logique pure » testable, faute de pile Bioconductor : **faux
  sur cette machine** (clusterProfiler, org.Hs/Mm.eg.db, ReactomePA, enrichplot,
  DOSE sont tous présents). Une *justification* périmée est aussi dangereuse qu'un
  chiffre périmé.

## [V1.x — garde C16] — 2026-09-17 — `errorCondition()` tronquait : la règle qui l'interdit

**Première règle de ce chantier produite AVANT le défaut** qu'elle prévient — les
cinq incréments précédents corrigeaient la garde **après coup**.

### Ajouté

- **Règle C16** (`tools/check_conventions.R`) : un `errorCondition()` doit avoir
  **exactement un argument POSITIONNEL**. `stop()` **concatène** ses arguments,
  `errorCondition(message, ...)` **non** — les suivants deviennent des **champs**
  de la condition. Un message multi-arguments enveloppé dans `errorCondition()`
  est donc **tronqué en silence** (détail : `docs/CONVENTIONS.md` §12.6).
- `tests/testthat/test-conventions-c16-arity.R` : fixture de 7 lignes
  (**2 signalées**, **5 muettes**), appel multi-lignes, et conformité du dépôt.

### Vérifié

- Fixture : `c(1L, 5L)` — `paste0` / `sprintf` / `paste` et les arguments nommés
  (`class=`, `state=`, `message=`) ne sont **pas** des faux positifs, et les
  virgules **imbriquées** ne comptent pas comme un argument de plus.
- **Test éprouvé par MUTATION** : seuil `n_pos > 1L` remplacé par `> 99L` ⇒ les
  **2 fixtures échouent**. Le test prouve que le détecteur **détecte**, pas
  seulement que la fonction existe.
- **Contre la garde d'AVANT** (`git show HEAD:…`) : les **3 tests échouent**
  (`could not find function`) — ils dépendent bien de la règle neuve.
- Dépôt entier (**136** fichiers) : **0 site**. Garde : **0 erreur / 190
  avertissements** (inchangé ; **+1 ligne** de tableau).
- `conventions` + `release-hardening` : **0 échec**.

### Ouvert

- **Promouvoir C16 en `ERREUR` ?** — c'est une **décision**, pas une correction :
  un message tronqué **dégrade** sans casser l'app. Plafond à **0** en attendant.

## [V1.x — correctif] — 2026-09-17 — `errorCondition()` TRONQUAIT les messages multi-arguments

**Régression introduite par l'incrément « 5ᵉ » ci-dessous**, et **non détectée
par ses propres preuves** : l'invariant qu'il mesurait comparait le **texte
source** (préservé), pas le **message à l'exécution**.

### Le défaut (mesuré)

`errorCondition(message, ...)` **n'agrège pas** ses arguments supplémentaires :
ils deviennent des **champs de la condition**, pas du message.

| expression | `conditionMessage()` |
|---|---|
| `stop("A : ", "B", " fin")` | `"A : B fin"` |
| `errorCondition("A : ", "B", " fin", class = "x")` | **`"A : "`** — tronqué |

`stop()` **concatène** ses arguments ; `errorCondition()` ne garde que le
**premier**. Envelopper un `stop()` **multi-arguments** dans `errorCondition()`
**tronque donc le message**. **3 des 19** sites convertis étaient concernés
(`R/bulk/bulk_helpers.R` : limma-voom, moteur inconnu, diagramme de Venn).

### Corrigé

Les 3 messages sont enveloppés dans `paste0(...)`, ce qui restitue exactement la
sémantique de `stop()` :
```r
# avant — tronqué à l'exécution
stop(errorCondition("Moteur DE non supporté : ", engine, class = "bulk_de_error"))
# après
stop(errorCondition(paste0("Moteur DE non supporté : ", engine), class = "bulk_de_error"))
```

### Vérifié

- **Rouge d'abord** : un nouveau test (`test-bulk-helpers.R`) assertait le
  message **à l'exécution** — **3 échecs** avant correctif
  (`grepl("avez 5", …)`, `grepl("UpSet", …)`, `grepl("moteur_inconnu", …)` tous
  `FALSE`), **0** après.
- Re-scan statique : `errorCondition` à **>1 argument positionnel** : **3 → 0**.
- `test-bulk-helpers.R` : **69 PASS / 0 FAIL** (64 avant, +5 assertions).
- Gardes **inchangées** : **0 erreur / 190 avert.**, duplication 0/3.

### Leçon

**Un invariant qui compare du TEXTE SOURCE ne prouve rien sur le comportement.**
La conversion précédente était « identique au caractère près » — et pourtant
**3 messages avaient changé à l'exécution**. Le seul invariant qui valait était
le **message runtime**. Corollaire pour la suite du chantier : **tout site dont
le message tient en plusieurs arguments doit passer par `paste0()`** — et
`R/core/pathway_helpers.R` (18 sites, non converti) en contient.

## [V1.x — correctif C15] — 2026-09-17 — le serveur pseudobulk appelait `ns()` sans le lier

Découvert par la **re-mesure de la suite complète** exigée par l'incrément de
dette ci-dessous — **pas** par lui : le défaut est **antérieur** (introduit par
`4abfc97`, le jalon « mode exploratoire sans réplicat »). Le [§2bk.5](docs/archive/STATUS_JOURNAL.md) #3 disait
explicitement que la suite complète n'avait **pas** été rejouée ; c'est
précisément ce trou que la re-mesure a comblé.

### Corrigé

- **`modules/sc/mod_sc_pseudobulk.R`** : `mod_sc_pseudobulk_server` appelle
  `ns("pb_no_rep_enable" / "pb_no_rep_bcv" / "pb_no_rep_attest")` dans un
  `renderUI` (le panneau « plan sans réplicat » livré par [§2bk](docs/archive/STATUS_JOURNAL.md)) **sans lier**
  `ns`. Shiny lève alors « could not find function "ns"` **au moment où la
  branche s'affiche** — donc invisible au démarrage, et invisible aux tests
  ciblés. Correctif d'**une ligne**, au motif maison (**30** autres modules
  écrivent `ns <- session$ns`) :
  ```r
  moduleServer(id, function(input, output, session) {
    ns <- session$ns          # <- ajouté
  ```

### Vérifié

- **Le test qui échouait existait déjà** : `test-release-hardening.R` (règle
  **C15**, statique) passait de `fail=1 pass=49` à **`fail=0 pass=50`**. Aucun
  test ajouté — la règle couvrait déjà la classe entière, il manquait le
  correctif, pas la détection.
- Re-scan C15 sur `modules/` (66 fichiers, 54 serveurs `moduleServer`) :
  **1 offenseur → 0**.
- **Suite complète re-mesurée** : **99 fichiers, `failed=0 passed=5885 error=0
  skipped=1`** (le seul SKIP = smoke GEO live) — soit **exactement** `5884 + 1`.

### Leçon

**Une suite complète n'est pas un luxe.** Le jalon précédent avait validé sa
livraison par des **tests ciblés** + les gardes ; le défaut ne vivait ni dans
les fonctions R testées ni dans `check_conventions.R`, mais dans le **chemin
Shiny**. Une politique « tests ciblés » laisse ce type de régression invisible
jusqu'à ce que quelqu'un rejoue la suite — ici, la re-mesure imposée par
l'incrément de dette.

## [V1.x — dette de conventions, 5ᵉ incrément] — 2026-09-17 — premier CLASSEMENT de domaine (C10 171 → 152, 209 → 190)

Cinquième incrément, et **le premier qui touche du code applicatif** : les quatre
précédents corrigeaient la **garde** ; celui-ci réduit la **vraie dette**, comme
décidé le 2026-09-17 (`errorCondition(class = "<domaine>_error")`, **pas**
`call. = FALSE`). **209 → 190** avertissements, **19 sites convertis**.

### Corrigé

- **C10 : les 19 `stop()` de `R/bulk/bulk_helpers.R` sont classés
  `bulk_de_error`** (171 → 152). La classe **existait déjà** dans le fichier
  (3 sites convertis antérieurement) : le fichier était donc **à moitié
  converti**, pas vierge.
  ```r
  # avant
  stop("Aucun gène ne passe le filtre — seuils trop stricts.")
  # après (forme attendue de CONVENTIONS.md §7)
  stop(errorCondition("Aucun gène ne passe le filtre — seuils trop stricts.",
                      class = "bulk_de_error"))
  ```
  Les **messages sont inchangés au caractère près** — le diff ne fait
  qu'**insérer** — donc les tests existants, qui matchent sur le message,
  restent valides. Mesuré sur l'artefact : **24 `stop()` avant, 24 après**, et
  les **24 messages identiques** une fois le wrapper `errorCondition(…)`
  neutralisé **des deux côtés**. (Piège : un comparateur qui ne prend que le
  **premier argument** signale de fausses différences sur les `stop()` à
  **plusieurs arguments** — `stop("A : ", paste(x, …))` — dont le message
  entier est correctement placé dans le wrapper.)

### Ajouté

- **5 assertions de classe** dans `tests/testthat/test-bulk-helpers.R`
  (`filter_bulk_counts`, `plot_heatmap_bulk`,
  `plot_sample_correlation_heatmap`, `run_bulk_de_dispatch` ×2).

### Vérifié

- **Rouge d'abord (P0)** : les 5 assertions **échouent** AVANT la conversion —
  `Expected class: bulk_de_error` / `Actual class: simpleError/error/condition`.
  Après : `test-bulk-helpers.R` **64 PASS / 0 FAIL** (59 PASS / 5 FAIL avant).
- **Aucune ligne ajoutée ni retirée** (1648 → 1648), diff **23 lignes / 23**.
- **Porte de sortie : le candidat doit PARSER avant écriture.** Ce n'est pas
  cosmétique — un premier essai a produit `stop(errorCondition(x, class = "y")`,
  un fichier **qui ne parse plus**, alors que la **garde restait verte à 190** :
  elle lit du **texte**, pas un AST. Le contrôle de parse a été ajouté au script,
  et le fichier restauré depuis une copie avant toute réécriture.
- **Suite complète re-mesurée** — le fichier modifié est du **code applicatif**,
  contrairement aux 4 incréments précédents. Résultat : **99 fichiers,
  `failed=1 passed=5884 error=0 skipped=1`** — et **le seul échec n'est PAS
  imputable à cet incrément** : c'est la régression **C15** du serveur
  pseudobulk, corrigée par l'entrée ci-dessus. Établir cela *était* le but de la
  re-mesure.
- ⚠️ **Une croyance corrigée par la mesure.** Le relevé précédent affirmait
  « aucun des 17 fichiers n'a d'`errorCondition` » : **FAUX pour 3 d'entre eux**
  (`bulk_batch_qc.R`, `bulk_helpers.R`, `sc_velocity.R` portaient déjà une classe
  d'erreur). Le fichier retenu est celui qui offrait le meilleur rapport : classe
  **déjà définie** *et* 19 sites à convertir.

### Ouvert

- **C10 = 152** — 16 fichiers restants, dont `sc_helpers.R` (34),
  `sc_trajectory.R` (21), `pathway_helpers.R` (18), `sc_plotting.R` (17),
  `spatial_stats.R` (16).
- ⚠️ **`state` : convention NON documentée, et incohérence locale.**
  `CONVENTIONS.md` §7 prescrit `class` **seul** ; or 10 fichiers portent un champ
  `state` **et** exportent un accesseur `*_error_state(e)`. `bulk_helpers.R`
  garde 3 sites historiques avec `state` **sans** accesseur ⇒ à trancher :
  documenter `state` (+ accesseur) dans §7, ou aligner ces 3 sites.
- **C11 = 1** (faux positif) · **C9 = 37** · portée de C6.

## [V1.x — dette de conventions, 4ᵉ incrément] — 2026-09-17 — la garde signalait les RE-LEVÉS (213 → 209)

Quatrième incrément du chantier, et **quatrième fois le même défaut de classe** :
la garde juge un **token** là où la règle parle d'une **unité logique**. Aucun
code applicatif n'est touché. **213 → 209** avertissements, **0 ajouté**,
**4 retirés** (diff avant/après, l'« avant » issu de `git`).

### Corrigé

- **C10 : `stop(<symbole nu>)` de re-levé (175 → 171).** `stop(e)` dans
  `error = function(e) { ... }` re-signale une condition **qui existe déjà** :
  sa classe se juge à son **origine**, pas au site du re-levé. 4 signalements
  étaient de ce type, et les 4 portaient sur `e`.
  ```r
  tryCatch(plot(x), error = function(e) {
    if (grepl("margins", conditionMessage(e))) return(invisible(NULL))
    stop(e)                      # re-levé : RIEN à classer ici
  })
  ```
  Règle retenue, **étroite** : le site n'est exempté que si l'argument est un
  **symbole nu** qui est un **FORMEL d'une fonction englobante**. Un symbole
  **local** (`msg <- "x"; stop(msg)`) fabrique la valeur *dans* la fonction :
  il reste signalé.

### Ajouté

- `.flatten_code()`, `.collect_function_defs()`, `.enclosing_formals()`,
  `.stop_first_arg()`, `.is_bare_symbol()` : le fichier est **aplati** en une
  chaîne pour suivre les accolades **en positions**. Un compteur ligne par ligne
  se fait piéger par `}, error = function(e) {` — une **fermeture avant son
  ouverture** : la profondeur y retombe à zéro et la fonction englobante n'est
  plus trouvée (défaut **mesuré** sur 2 des 4 sites réels).
- **5 tests** (8 assertions) dans `test-conventions-c6-strings.R`, dont **2
  dédiés aux cas VOISINS** qui doivent RESTER signalés (symbole local ; symbole
  nu **hors** de toute fonction) — exempter est le geste dangereux.

### Vérifié

- **A/B contre la garde d'AVANT** (`git show HEAD:tools/check_conventions.R`) :
  sur la fixture, l'ancienne signale `5,10,13,17,19,20,21,24`, la nouvelle
  `10,19,20`. Les sites **retirés** sont exactement les formes de re-levé
  (gestionnaire anonyme, gestionnaire **nommé**, corps sur **une ligne**,
  fonction englobante **imbriquée**, ligne `}, error = ...`) ; le symbole
  **local** et les deux `stop()` de message **restent signalés**.
- **Diff des ensembles de sites sur tout `R/`** : **0 ajouté**, **4 retirés**
  (`bulk_helpers.R:694`, `jobs.R:82`, `sc_abundance_milo.R:541`,
  `sc_helpers.R:1205`), sans résidu.
- **Les 5 nouveaux tests ÉCHOUENT contre la garde d'AVANT** (mesuré : `53 pass /
  7 fail`) et **passent** contre la nouvelle (`60 pass / 0 fail`) — aucun autre
  test ne bouge.
- **Coût maîtrisé et mesuré** : l'analyse des définitions de fonction n'est
  déclenchée que si un symbole nu est en jeu **et** que les exemptions bon
  marché ont échoué ⇒ **20,3–20,6 s → 20,8–21,3 s** (+2,5 %).

### Ouvert

- **C10 = 171** — de la **vraie dette** cette fois, mesurée par **forme
  d'argument** : **145** littéraux + **26** appels (`sprintf`, `paste0`, `tr`),
  soit **171** `stop()` de message à **classer par domaine**
  (`errorCondition(class = "<domaine>_error")`), **pas** par `call. = FALSE`.
  Les **19** fichiers concernés n'ont, pour la plupart, **aucun**
  `errorCondition` : un constructeur de domaine doit y être **défini** d'abord.
- **C11 = 1** (faux positif mesuré, `bulk_gsva.R:185` sous garde Windows) ·
  **C9 = 37** (tests éponymes) · portée de C6 (`ROADMAP.md` §6).

## [V1.x — dette de conventions, 3ᵉ incrément] — 2026-09-17 — la garde ne résolvait pas le constructeur local (259 → 213)

Troisième incrément du chantier engagé le 2026-09-16, et **troisième fois le
même défaut de classe** : la garde juge un **token** là où la règle parle d'une
**unité logique**. Aucun code applicatif n'est touché. **259 → 213**
avertissements, **0 ajouté**, **46 retirés** (diff avant/après, l'« avant »
issu de `git`).

### Corrigé

- **C10 : `stop()` routé par un constructeur local classé (221 → 175).**
  Le motif maison route l'erreur par un constructeur local :
  ```r
  .bulk_multi_stop <- function(msg, state) {
    errorCondition(msg, class = "bulk_multi_error", state = state)
  }
  # ...
  stop(.bulk_multi_stop("Label vide.", state = "invalid_label"))
  ```
  Le site est **bel et bien classé**, mais le token `errorCondition`
  n'apparaît **pas** dans l'étendue du `stop()` : la garde le déclarait « non
  classé ». 46 signalements étaient de ce type — 18 `.bulk_multi_stop`,
  15 `.sc_multi_stop`, 13 `.bulk_multi_compare_stop` — et ces trois fichiers
  étaient exactement ceux que la mesure précédente croyait « déjà classés ».
  `check_c10_error_style()` résout désormais les constructeurs du projet via
  `.collect_classed_error_ctors()`.

### Ajouté

- `.collect_classed_error_ctors()` : constructeur classé = fonction dont le
  corps est **UNE SEULE** expression `errorCondition(...)`. Règle **stricte**,
  et **mesurée** : la règle large (« le corps cite `errorCondition` ») retenait
  **61** noms contre **3**, pour le **même** verdict sur les 46 sites. Sans
  cette stricte, `stop(validateur(x))` — où `validateur` lève une erreur
  classée *parmi* d'autres vérifications — serait exempté à tort.
- `.span_calls_classed_ctor()` et **5 tests** (dont 3 négatifs) dans
  `tests/testthat/test-conventions-c6-strings.R` : routé par un constructeur
  classé ⇒ exempté ; routé par une fonction quelconque ⇒ signalé ; routé par un
  validateur qui **cite** `errorCondition` sans le retourner ⇒ signalé ;
  constructeur résolu **d'un fichier à l'autre**.

### Vérifié

| Preuve | Résultat |
|---|---|
| Diff avant/après | **0 ajouté**, **46** retirés (18 + 13 + 15) |
| Fixture négative contre la garde d'**AVANT** (`git show HEAD:`) | ancienne garde : `4,5,7` · nouvelle : `7` |
| Fichiers du dépôt, ancienne vs nouvelle garde | `bulk_multi.R` 18 → **0** · `sc_multi.R` 15 → **0** · `bulk_multi_compare.R` 13 → **0** |
| `run_tests.R conventions` | **52 pass / 0 fail / 0 error** (3 fichiers, chacun vérifié présent au bilan) |
| `check_conventions.R` | **0 erreur / 213 avertissements** |

### Ouvert

- **Réduction par CLASSEMENT** (décision utilisateur du 2026-09-17) : les
  **175** `stop()` réels restent à convertir en
  `errorCondition(class = "<domaine>_error")`. **19 fichiers**, dont
  `sc_helpers.R` (35), `sc_trajectory.R` (21), `bulk_helpers.R` (20),
  `pathway_helpers.R` (18) — **aucun n'a d'`errorCondition`** : un constructeur
  de domaine doit y être **défini** avant toute conversion.
- Inchangés : **C9 = 37**, **C11 = 1**, portée de C6 (`ROADMAP.md` §6).

## [V1.x — dette de conventions, 2ᵉ incrément] — 2026-09-17 — deux règles jugeaient la mauvaise unité (319 → 259)

Suite du chantier de réduction engagé le 2026-09-16. Comme le premier incrément,
celui-ci ne touche **aucun code applicatif** : il corrige la **garde**, qui
gonflait la dette mesurée. **319 → 259** avertissements, **0 ajouté**, 60
retirés, tous justifiés (diff avant/après, l'« avant » issu de `git`).

### Corrigé

- **C6 : le compteur d'accolades ne comptait rien (11 → 0).**
  `check_c6_library_in_r()` mesurait la profondeur d'imbrication avec
  `gregexpr("\\{", ln, fixed = TRUE)`. Or `\\{` est en R la chaîne de **deux**
  caractères `\{`, et `fixed = TRUE` désactive l'interprétation regex : le motif
  ne trouvait donc **jamais** `{`. Mesuré :
  `lengths(regmatches("{", gregexpr("\\{", "{", fixed = TRUE)))` = **0**.
  Le compteur restait bloqué à 0, donc `depth == 0L` était **toujours vrai**, et
  la règle « pas de `library()` **au top-level** de `R/` » signalait en réalité
  `library()` à **n'importe quelle** profondeur. Mesure des 11 sites : profondeurs
  **1 à 4** — **aucun** n'était top-level. La dette C6 affichée était donc
  **entièrement un artefact de mesure**. Corrigé par `.count_chars()`.

- **C10 : un appel multi-lignes jugé sur sa première ligne (270 → 221).**
  `check_c10_error_style()` cherchait `call. = FALSE` **sur la ligne** de
  `stop(`. Un appel conforme mais réparti sur deux lignes —
  `stop("message",` / `     "suite", call. = FALSE)` — était donc signalé à tort.
  Sur 270 signalements, **49 (18 %)** portaient déjà `call. = FALSE` ou
  `errorCondition` **dans** l'appel. La règle analyse désormais l'**appel
  logique**, jusqu'à l'équilibrage des parenthèses.

### Vérifié

- Diff **avant/après** : `0 ajouté`, `49 C10 + 11 C6` retirés, et les 49
  correspondent **exactement** aux 49 prédits par un classifieur Python
  indépendant (concordance entre deux mesures indépendantes).
- Les deux tests à fixtures ont été **éprouvés contre la garde d'AVANT** (issue
  de `git show HEAD:`) : C6 rendait `1,3,5,8` et C10 `1,3,5` — ils **échouent**
  donc sans le correctif. Un test vert qui n'a jamais été vu rouge ne prouve rien.
- Non-aveuglement conservé : un `library()` **top-level** reste signalé, et un
  `stop()` multi-lignes **sans** classement reste signalé.
- `tools/run_tests.R conventions` : **42 pass / 0 fail / 0 error** (3 fichiers,
  chacun vérifié **présent au bilan**).
- `check_conventions.R` : **0 erreur / 259 avertissements**.

### Ouvert (règle 9 — listé, non exécuté)

- Les 11 `library()` **imbriqués** ne violent plus la règle *telle qu'elle est
  écrite*, mais un `library()` exécuté dans une fonction attache le paquet
  **globalement** au moment de l'appel et peut masquer une fonction maison —
  c'est la raison d'être de C6. **Faut-il écrire la règle plus large ?** Décision
  cadrée dans `ROADMAP.md` §6.
- Reste de la dette : **C10 = 221** · **C9 = 37** · **C11 = 1**.

## [V1.x — dette de conventions] — 2026-09-16 — la garde mesurait FAUX (324 → 319) + `--list-all`

### Pourquoi

`ROADMAP.md` §5 **décision 10** laissait le choix : *« chantier de réduction, ou
statu quo avec plafond ? »*. **Choix utilisateur : la dette.** Premier incrément —
et il ne touche **aucun code applicatif** : il corrige la **garde**, qui gonflait
la dette mesurée.

### Corrigé

- **Chaînes multi-lignes ignorées** (`tools/check_conventions.R`) :
  `.strip_strings_and_comments()` appliquait ses regex **ligne par ligne**, donc
  ne voyait pas les chaînes R s'étendant sur plusieurs lignes — les scripts
  reproductibles **embarqués** dans `R/bulk/bulk_report_engine.R` et
  `R/sc/sc_export.R`. Les `library()` contenus dans ce **texte** étaient signalés
  alors qu'ils ne sont **jamais exécutés au `source()`** : **5 des 16 signalements
  C6 étaient FAUX**, dont `R/sc/sc_export.R:47` — exactement le cas que le
  commentaire de la fonction annonçait couvrir. Les `{`/`}` du texte embarqué
  faussaient en plus le compteur de profondeur de `check_c6_library_in_r()`.
  Remplacé par un **automate d'états** transportant l'état « dans une chaîne »
  d'une ligne à l'autre.
- **Dette non énumérable** : le message de troncature invitait à passer
  `--strict` « pour tout lister », mais `--strict` ne change **rien** à
  l'affichage — il rend seulement les avertissements **bloquants**. Ajout de
  **`--list-all`**, désormais distinct de `--strict`.

### Piège trouvé EN EXÉCUTANT (gelé par test)

Une **première** version de l'automate retirait **aussi les guillemets**. Or le
garde lit la **FORME** du code : C10 exempte `stop()` mais signale `stop("")`.
`stop("Package requis")` devenait `stop()` → forme exemptée → **63 avertissements
C10 réels disparaissaient** (`pathway_helpers.R`, `sc_trajectory.R`,
`spatial_reference.R`…) **sans qu'aucune ERREUR n'apparaisse**. Révélé par
l'écart **avant/après** (l'« avant » venant de **git**) : les 63 disparus étaient
« hors chaîne », donc **injustifiés**. Le garde était **aveugle en silence**.
Corrigé : on vide le **contenu**, on garde la **ponctuation**.

### Mesuré

| Garde | Avant | Après |
|---|---|---|
| `check_conventions.R` | 0 err / **324** avert. | 0 err / **319** avert. |
| dont C6 | 16 | **11** |
| C9 · C10 · C11 | 37 · 270 · 1 | 37 · 270 · 1 (inchangés) |
| Durée | 30 s | **20 s** |
| Tests de conventions | — | **33 PASS / 0 FAIL / 0 ERROR** |

**0 apparu, 5 disparus, 5 justifiés, 0 injustifié.** Aucune ERREUR, aucun
compteur aggravé.

### Ajouté

- `tests/testthat/test-conventions-c6-strings.R` — 11 assertions, **éprouvé par
  mutation** (réinjecter le défaut fait passer 2 assertions au rouge).

### Documentation

`docs/CONVENTIONS.md` **§12.1** et **§12.2** (plafonds 324 → 319) ; `STATUS.md`
**[§2bg](docs/archive/STATUS_JOURNAL.md)** + §0 ; `ROADMAP.md` **§5 #10** (décision close : réduction engagée).

## [V1.x — NEW-3] — 2026-09-16 — interactome local + moteur PCSF heuristique (+ 3 prémisses corrigées, 3 défauts trouvés en exécutant)

### Pourquoi

NEW-3 (`ROADMAP.md` §2.0 rang 6) était le **dernier jalon débloqué non ouvert**.
Objectif : relier des gènes d'intérêt (résultat DE) en un **sous-réseau** dans un
interactome **local**, sans aucune dépendance nouvelle ni accès réseau
(*local-first*).

### Changement

- **`R/bulk/bulk_network.R`** (nouveau, pur) :
  - **N-1** `bulk_network_map_ids()` — UniProt → SYMBOL, taux **tracé**, plancher
    **déclaré** ;
  - **N-2** `load_bulk_network()` — réseau Reactome **dédupliqué** (paire non
    orientée), mémoïsation **de session** ;
  - **N-3** `run_bulk_network_pcsf()` — moteur PCSF **heuristique déterministe**
    (plus courts chemins + arbre couvrant + élagage), paramètres ω/β/μ déclarés,
    `max_join_hops` rendant le critère **lisible** ;
  - **N-4** `plot_bulk_network()` + `build_bulk_network_table_export()`.
- **`modules/bulk/mod_bulk_network.R`** (nouveau) — panneau `3g. Réseau PCSF
  (interactome)`, onglet `Réseau PCSF`. La mise en garde sémantique est
  **affichée** (bandeau + légende du tracé), pas seulement documentée.
- **`docs/contracts/BULK_NETWORK_CONTRACT.md`** (nouveau, gelé) +
  **`tests/testthat/test-bulk-network-contract-freeze.R`**.
- **`config/thresholds.R`** — `TS_BULK_NETWORK_MIN_MAP_RATE` (0.50),
  `TS_BULK_NETWORK_MAX_NODES` (200), justifiés par mesure.
- **32 clés i18n** (2537 → 2569). `app.R` + `mod_bulk.R` câblés.

### Corrigé — trois prémisses de la proposition étaient fausses (mesuré)

1. **« 19 107 arêtes, degré ~1,7 »** → **292 895 arêtes**, **11 030 nœuds**,
   **degré moyen 53,1** : le degré était sous-estimé d'un **facteur ~30**, et
   c'est lui qui calibre ω.
2. **« `org.Mm.eg.db` absent »** → il **est** installé. La souris est
   indisponible à cause de **`mmuReactome.db`** (base de **voies** absente ⇒
   `graphite` tente un **téléchargement**, interdit par *local-first*).
   Conclusion « humain seul » inchangée, **raison différente et mesurée**.
3. **« préfixe `UNIPROT:` à retirer »** → les accessions sont **NUES**.

### Corrigé — trois défauts trouvés **en exécutant**

1. **Critère incohérent avec l'objectif** : la rentabilité était testée
   `ω > β·d`, alors que l'objectif annoncé compte **aussi** `μ` par relais.
   Corrigé en `ω > β·d + μ·(d−1)` ; ω=1 était de fait **dégénéré** (rien ne se
   reliait jamais sur un réseau de degré 53).
2. **Boucle infinie** : l'appartenance à un groupe était lue dans une liste de
   membres indexée par le nœud d'**absorption**, si bien qu'un nœud du même
   groupe gardait une liste singleton ⇒ des paires intra-groupe restaient
   finies et le nombre de groupes ne décroissait plus. **> 10 min, processus
   tué** ; **0,69 s** après correction (borne de terminaison **prouvable**).
3. **`AnnotationDbi::mapIds()` échoue** (« None of the keys entered are valid
   keys ») quand **aucune** clé n'est dans l'espace, au lieu de rendre `NA` —
   précisément le cas qu'un plancher doit rendre **lisible**. Corrigé par
   pré-filtrage sur l'espace de clés (mémoïsé), **sans `tryCatch`** (qui
   masquerait aussi les vraies erreurs).

### Garde

- `test-bulk-network.R` : fixture synthétique à sous-réseau **optimal connu par
  construction**, élagage, critère de rentabilité (dont le cas **dégénéré**
  ω=1), déterminisme, invariant de **forêt** (`arêtes = nœuds − composantes`),
  états d'erreur, plancher sur **cas négatif réellement injecté**, coût borné.
- Freeze test : surface publique **triée** + signatures, états, **pureté Shiny**,
  réutilisation d'`igraph` (interdit désormais toute réimplémentation de
  graphe : `BFS|Dijkstra|Floyd`), ancres `app.R`/`mod_bulk.R`, seuils déclarés,
  **`bulk_multi_pipeline_fields()` inchangé** (contrat MD-1 non modifié),
  clés i18n, sync code ↔ contrat.

### Vérifié

`390 PASS / 0 FAIL / 0 ERROR / 0 SKIP` (2 fichiers) · `check_conventions.R`
**0/324** (inchangé) · `check_duplication.R` **0/3** (inchangé) ·
`check_renv_hermeticity.R` **0** · `audit_doc_refs.py` **0 err / 18 warn**
(inchangé) · lancement headless `Listening on http://127.0.0.1:4917` sans
erreur · **aucune dépendance nouvelle**.

### Non fait

- **`network_result` reste HORS du snapshot multi-jeux** — précédent respecté
  (`pattern_result`/`dose_result`/`survival_result` en sont aussi absents) ;
  l'y ajouter toucherait un contrat **gelé** ⇒ décision séparée
  (`ROADMAP.md` §6).
- Réseau **PPI physique** et algorithme **exact** (PLNE) : hors périmètre,
  consignés en §6.

---

## [V1.x — CC-5] — 2026-09-16 — le moteur CellChat devient la source PAR DÉFAUT (+ audit documentaire re-mesuré)

### Pourquoi
Le moteur CellChat natif (CC-5) était présenté comme une **cinquième** option,
placée **en fin de liste** et **non sélectionnée** : l'UI faisait donc passer
l'import d'une table exportée pour la voie normale, alors que le **calcul dans
l'application** est la voie voulue par l'utilisateur (demande consignée
`STATUS.md` [§2bd.5](docs/archive/STATUS_JOURNAL.md) #2).

### Changement
- `modules/sc/mod_sc_communication.R` : `cellchat_engine` passe **en tête** de
  `comm_source` et devient le **défaut** (`selected = "cellchat_engine"`). Les
  quatre sources d'import restent, dans leur ordre d'origine.
- **Repli serveur aligné** : `input$comm_source %||% "cellchat_engine"`. Avant,
  un input absent retombait sur la branche « CellChat table » et y échouait **en
  silence** (`req()` muet) au lieu d'aiguiller vers le bouton de calcul.
- `docs/contracts/CELLCHAT_ENGINE_CONTRACT.md` §10 : le défaut est documenté
  comme **invariant d'UI gelé** (contract-first : code + test + doc ensemble).
- Aucun champ canonique, aucune vue, aucun export ne change — la **règle des
  deux voies** reste intacte.

### Garde
- `tests/testthat/test-sc-communication-engine-ui.R` — nouveau test
  « the native engine is the DEFAULT source and comes FIRST in the list » :
  4 assertions (défaut déclaré, ordre des **valeurs**, ordre des **libellés** —
  un réordonnancement peut désynchroniser les deux vecteurs de `setNames()` sans
  que rien ne casse à l'exécution — et accord UI ↔ serveur).
- **Éprouvé sur un cas négatif réel** : les 4 assertions rejouées sur le texte de
  `HEAD` (`git show HEAD:modules/sc/mod_sc_communication.R`) rendent **FALSE
  toutes les quatre**. ⚠️ Un premier essai sur une « avant » reconstruit à la
  main était **incomplet** et faisait passer la 3ᵉ assertion à tort ⇒ le « avant »
  doit venir de **git**, jamais d'une reconstitution.

### Vérifié
- Tests **ciblés** (4 fichiers, chacun vérifié présent au bilan) : **266 pass /
  0 fail / 0 error / 0 skip**. Suite complète **non re-lancée** (politique :
  en fin de version).
- Gardes : conventions **0 err / 324 avert.**, duplication **0 err / 3 avert.**,
  herméticité renv **0 err / 0 avert.** — ⚠️ les **2 avertissements** du relevé
  précédent n'apparaissent plus et la cause **n'a pas été instruite** (ce jalon
  n'a touché aucune dépendance) : ne pas s'appuyer dessus sans re-mesure.

### Documentation
- `STATUS.md` **[§2be](docs/archive/STATUS_JOURNAL.md)** (nouveau) + §0 rafraîchi + [§2bd.5](docs/archive/STATUS_JOURNAL.md) #2 **clos**.
- `ROADMAP.md` §2 (bandeau) et §6 (**autopipeline étendu débloqué**).
- **Audit documentaire re-mesuré** : **0 erreur · 59 → 18 avertissements**. Les
  « 59 avertissements sur 8 fichiers » annoncés étaient **faux** : **41 des 59**
  sont **un seul token**, `archive/STATUS_JOURNAL.md` — et c'est la forme que la
  convention **PRESCRIT**. La section 1 de l'auditeur **résout** ce lien contre le
  dossier de l'émetteur et le déclare valide ; sa section 2 cherche des *tokens
  nus* et les résout contre la **racine**, où `archive/` n'existe pas ⇒ **faux
  positif par construction**. Accepté (`docs/audit-accepted.txt`, classe 6),
  documenté (`docs/archive/README.md`), et le détecteur a été **vérifié sur une
  sonde injectée** (lien mort signalé en erreur, lien correct muet).

### Non fait — **listé, non exécuté** (règle 9)
NEW-3 (5 décisions ; **D5 débloquée**) · LIANA natif (route (a), décision
séparée) · autopipeline étendu (**débloqué**, à cadrer) · réseau d'enrichissement
**interactif** (à cadrer) · dette `STATUS.md` [§2bd.4](docs/archive/STATUS_JOURNAL.md).

## [V1.x — doc] — 2026-09-16 — passe de nettoyage : `STATUS.md` 187 Ko → ~64 Ko, suite RE-MESURÉE à 5408 PASS

### Pourquoi
`docs/STATUS.md` est le *NEXT SESSION ENTRY POINT* (`AGENTS.md`), mais il
cumulait l'état courant **et** un journal : **187 Ko**, inexploitable.

### Changement
- **Journal extrait** : `§2i`…`§2aw` (2026-09-10 → 2026-09-15) déplacés **à
  l'identique** (sous-sections `2am.1`…`2am.4`, `2ao.1`…`2ao.7` incluses) vers
  **`docs/archive/STATUS_JOURNAL.md`** (142 Ko). **Rien de supprimé.**
- **Index inséré** : une section `§2i … 2aw` récapitule chaque entrée archivée
  **en une ligne** (objet + verdict) → les décisions restent découvrables.
- **Nouveau `§0 « État courant »`** : 7 lignes (suite / prochain jalon / gelé /
  bloqué / reste ouvert / arbre / historique).
- **~80 références** `STATUS.md §2xx` réécrites vers `STATUS_JOURNAL.md §2xx`
  dans `STATUS.md` et `ROADMAP.md`.
- **Archivés** (périmés / orphelins) : `ROADMAP_HANDOFF_NEXT.md` (périmé sur son
  objet), `ROADMAP_HANDOFF_STAGE_PLOT_S6.md` (prémisse périmée),
  `helper-mock-data.R` (orphelin — ⚠️ ne pas le remettre dans `tests/testthat/`,
  testthat source tout `helper-*.R`). Index : `docs/archive/README.md`.
- `AGENTS.md` : baselines et durées remises à jour (voir ci-dessous).

### Suite complète RE-MESURÉE — `5408 PASS / 0 FAIL / 0 ERROR / 1 SKIP`
L'ancienne référence « 5103 PASS / 87 fichiers / ~37 min » était **périmée** :
obtenue avant le correctif de locale des runners, avant les jalons 4E-4 et
P0-sourcing, et avec deux fichiers e2e sautés. Nouvelle mesure : **95 fichiers,
~11 min**. Le seul SKIP restant est `test-mod-geo.R` = smoke GEO **live**
(réseau), voulu.

⚠️ **La lenteur était un symptôme** : le « flake chromote »
`test-shinytest2-import.R` **stallait ~15 min** (`handle_read_frame error`).
Le correctif de locale supprime aussi ce stall. Détail : `STATUS.md` [§2ba](docs/archive/STATUS_JOURNAL.md).

## [V1.x — e2e] — 2026-09-16 — les 4 fichiers `test-shinytest2-*` étaient sautés : la couverture e2e était nulle

### Symptôme
Suite complète : **9 SKIP** au lieu du 1 attendu. Les quatre fichiers
`test-shinytest2-{bulk,import,sc,spatial}.R` étaient intégralement sautés
(`pass=0 skip=2`), donc **zéro couverture e2e** — or ces tests pinnent les ids
d'inputs **namespacés** des 4 domaines, c'est-à-dire exactement le drift de
namespace qu'aucun test unitaire ne peut voir.

### Cause — un faux motif de skip qui masquait la vraie erreur
Le helper annonçait `"Chromote/headless Chrome unavailable."`, mais Chrome **est**
présent. La vraie erreur était **`invalid multibyte string, element 1`** : un
problème de **locale**, pas de navigateur. Le `message()` qui portait la cause
réelle partait sur stderr, noyé dans la sortie — seul le motif de skip (faux)
restait visible.

`shinytest2` démarre l'app dans un **processus enfant** : il hérite des
**variables d'environnement**, pas des appels `Sys.setlocale()` du parent. Or Git
Bash exporte `LC_ALL=C.UTF-8`, nom que R sous Windows **ne reconnaît pas**
(`Warning: Setting LC_CTYPE=C.UTF-8 failed`) ⇒ repli sur `C` ⇒
`i18n/translation.json` (UTF-8) illisible. `LC_ALL` **prime sur `LC_CTYPE`** :
poser `LC_CTYPE` seul ne suffit pas.

| Environnement hérité | `AppDriver$new()` |
|---|---|
| `LC_ALL=C.UTF-8` (tel quel) | ❌ `invalid multibyte string, element 1` |
| `LC_ALL` levé + `LC_CTYPE=fr_FR.UTF-8` | ✅ driver OK, inputs lus |

### Changement
`tests/testthat/helper-app-driver.R` : `ts_e2e_with_child_locale()` lève
`LC_ALL`/`LANG`, pose un `LC_CTYPE` UTF-8 **réellement accepté** (candidats testés
via `Sys.setlocale`), et **restaure l'environnement** dès le driver créé. On ne
force donc pas `LC_ALL` : cela toucherait aussi `LC_COLLATE`/`LC_TIME`, donc le
tri des chaînes, donc potentiellement des résultats de tests. Le motif de skip
cite désormais l'**erreur réelle**. Deux `skip_if(is.null(app), …)` devenus
**inatteignables** ont été retirés de `test-shinytest2-import.R`.

### Effet mesuré
| | avant | après |
|---|---|---|
| fichiers e2e seuls | 0 pass / 8 skip | **18 pass / 0 fail / 0 skip** |

Les assertions de drift de namespace des 4 domaines passent : **aucun drift**.

## [V1.x — P0 LANCEMENT] — 2026-09-16 — l'app ne démarrait plus : deux fichiers de `R/` n'étaient pas sourcés

### Symptôme
```
Erreur dans bulk_gene_set_choices(.tr_plain) :
  impossible de trouver la fonction "bulk_gene_set_choices"
Called from: hasGroups(choices)
```
L'application **ne démarrait plus du tout** : l'erreur tombe à la construction de
l'UI, avant même l'ouverture de la session.

### Cause
`R/bulk/bulk_gene_sets.R` (ajouté par `440878b`, jalon « jeux de gènes NATIFS »)
n'était **pas** dans la liste `source()` de `app.R`. `bulk_gene_set_choices()`
n'était donc jamais définie, alors que `mod_bulk_pathways.R` l'appelle dans un
`choices =` de l'UI.

**Pourquoi les tests ne l'ont pas vu** : `test-bulk-gene-sets.R` fait
`source_project_file("R/bulk/bulk_gene_sets.R")` — il **contourne** la liste de
sources de `app.R` et reste vert pendant que l'app est cassée. Un harnais de
test n'est pas un test de démarrage.

### Vérification systématique : il y en avait DEUX
`find R -name '*.R'` comparé aux `source("R/...")` de `app.R` ⇒ **69 fichiers sur
disque, 66 sourcés** :

| Fichier | Sort |
|---|---|
| `R/bulk/bulk_gene_sets.R` | **non sourcé** → P0 (l'UI ne se construit pas) |
| `R/plotting/plot_dims.R` (PLOT-S6) | **non sourcé** → P0 (crash au **premier** plot : `global.R` enveloppe `renderPlot()` et appelle `ts_render_plot_args()` à chaque invocation) |
| `R/sc/sc_state.R` | non sourcé **volontairement** (re-export LEGACY) |
| `modules/spatial/mod_spatial_lr.R` | module **parqué** (vague 8/backlog), aucun appelant |

Le second P0 était **masqué** par le premier : corriger seulement
`bulk_gene_sets.R` aurait déplacé le crash au premier rendu de plot.

### Corrigé
Deux lignes dans `app.R` : `source("R/plotting/plot_dims.R")` et
`source("R/bulk/bulk_gene_sets.R")`.

### Ajouté — garde d'EXHAUSTIVITÉ
`tests/testthat/test-app-sourcing.R` : **tout** fichier de `R/` et `modules/`
doit être sourcé par `app.R` — directement, ou via `list.files(<dir>)` (cas
`modules/bulk_de`). Il **généralise** le précédent « patch CellChat »
(`test-sc-communication-engine-ui.R` assertait `app.R sources the engine` pour
**un** fichier). Deux exceptions en allowlist justifiée.

- **Cas négatif éprouvé** : un fichier réel injecté dans `R/` fait rougir le
  garde en le nommant, puis la suppression rend le vert.
- **Auto-contrôle** : le garde vérifie que ses deux extracteurs voient bien
  quelque chose — assertion qui a **attrapé un bug du garde lui-même** au premier
  essai (`regmatches()` renvoie la correspondance entière, guillemet fermant
  inclus ⇒ `dir.exists()` faux).

### Vérifié
- Lancement : `source("app.R")` OK, `ui` construit — **1 093 434 caractères de
  HTML**, panneau Bulk présent, `renderPlot()` opérationnel.
- Tests ciblés : **202 PASS / 0 FAIL / 0 ERROR / 0 SKIP** (4 fichiers).
- `check_conventions.R` 0 err / 324 avert. ; `check_duplication.R` 0 err / 3 avert.

## [V1.x — outils] — 2026-09-16 — les runners de tests sont enfin utilisables depuis Git Bash

### Le problème mesuré
Deux défauts distincts, tous deux **silencieux**, dans `tools/run_tests.R` et
`tools/run_full_suite.R` :

1. **Locale.** Git Bash exporte `LC_ALL=C.UTF-8` — un nom que R **ne reconnaît
   pas** sous Windows. R retombait donc sur `C`, où `parse(file = )` échoue sur
   certaines sources UTF-8 du dépôt (`unexpected invalid token` sur
   `R/sc/sc_communication_perturbation.R`) : **lancer la suite depuis Git Bash
   était impossible**. Aucun des deux runners ne posait la locale (point ouvert
   documenté dans « Garde C13 + sémantique `testServer()` MESURÉE »).
2. **Filtres multiples ignorés.** `tools/run_tests.R` annonçait
   `<filter> [<filter2> ...]`, mais `testthat::test_dir(filter = )` prend **une
   seule** regex : avec un vecteur de longueur > 1, seul le **premier** élément
   est utilisé. Mesuré : demander `c("core-jobs", "da-milo-async")` n'exécutait
   que `test-core-jobs.R` (19 tests) **tout en imprimant un `TOTAL` vert** — le
   fichier demandé n'était pas lancé, et rien ne le signalait.

Le second défaut est le plus dangereux : il transforme une commande de
vérification en **faux témoin de succès**.

### Corrigé
- `tools/run_tests.R` + `tools/run_full_suite.R` : `Sys.setlocale("LC_CTYPE",
  "fr_FR.UTF-8")` **avant tout `parse()`**, avec un `warning()` **visible** si la
  locale est indisponible — ne jamais remplacer un échec par un silence.
- `tools/run_tests.R` : `filter <- paste(filter, collapse = "|")` ⇒ tous les
  filtres demandés sont honorés.

### Vérifié
Depuis Git Bash (locale `LC_ALL=C.UTF-8`), plus aucun `RUNNER-ERROR`.
`Rscript tools/run_tests.R core-jobs da-milo-async` ⇒ **40 PASS / 0 FAIL /
0 ERROR / 0 SKIP** (les **deux** fichiers sont bien exécutés : 19 + 21).

## [V1.x — 4E-4] — 2026-09-16 — la DA Milo passe par le pool applicatif (sync ≡ async)

### Le problème mesuré
La proposition 4E-4 (Stage 18) était bloquée par un prérequis **jamais
arbitré** : « accepter la sérialisation mirai des objets Seurat (coût IPC) ».
Mesuré plutôt que supposé :

| Objet | Seurat complet | Charge utile Milo | Ratio | IPC complet | IPC charge utile |
|---|---|---|---|---|---|
| 6 000 × 2 000 | 13,0 Mo | 1,74 Mo | 7,5 × | 0,00 s | ~0,00 s |
| 20 000 × 5 000 | 98,2 Mo | 5,81 Mo | 16,9 × | 0,59 s | 0,01 s |
| 40 000 × 10 000 | 379,5 Mo | 11,63 Mo | **32,6 ×** | 1,67 s | 0,01 s |

L'affirmation du Stage 18 est **confirmée pour l'objet entier** et **infirmée
pour la charge utile** : Milo ne consomme que les embeddings + `meta.data`. Le
prérequis est donc **fermé par la mesure**, pas par une décision d'opinion.

### Corrigé — un défaut de reproductibilité découvert en EXÉCUTANT
Le premier câblage « réussissait » : `status = valid`, même graine enregistrée,
aucune erreur. Il produisait pourtant **un autre résultat** que le chemin
synchrone :

| Chemin | Quartiers | Σ logFC | `md5` |
|---|---|---|---|
| synchrone (×3 identiques) | 20 | +5,45101795671 | `d61d2fd6fd05` |
| daemon **sans** correctif | **19** | **−0,538258164913** | `1cc65c57d664` |
| daemon **avec** correctif | 20 | +5,45101795671 | `d61d2fd6fd05` |

Cause mesurée : `rngkind` **principal** = `Mersenne-Twister/Inversion/Rejection`
vs **daemon** = `L'Ecuyer-CMRG/Inversion/Rejection` ⇒ `set.seed()` n'a pas le
même sens dans un daemon. Écartés **par la mesure** : la sérialisation
(aller-retour `serialize`/`unserialize` en processus = octets identiques, même
résultat), le backend parallèle (`SnowParam` des deux côtés) et `mc.cores`
(`NA` des deux côtés).

### Ajouté
- **`run_job(..., rng_kind = RNGkind())`** (`R/core/jobs.R`) : le `RNGkind()` de
  l'appelant est figé dans le **processus principal** et transmis au job, qui le
  restaure avant `fn` puis rétablit le précédent. Corrigé **dans le wrapper**,
  pas dans le domaine (règle 3 : étendre, ne pas dupliquer — évite de toucher un
  contrat gelé).
- **`APP_DAEMON_SOURCE_FILES`** (`R/spatial/spatial_async.R`) : 17 fichiers
  préchargés dans les daemons (7 spatial + `config/*` + `R/core/*` +
  `R/sc/*` design/Milo/velocity) ⇒ le pool sert désormais **aussi** le domaine
  SC **sans** créer un second framework async (règle 8).
- **`ensure_app_daemons()`** : point d'entrée applicatif neutre et idempotent.
- **`.resolve_app_base_dir()`** : résout la racine par le marqueur `app.R` au
  lieu de faire confiance à `getwd()`.
- **`TS_DA_MILO_TIMEOUT_MS`** (`config/defaults.R`) = 30 min — paramètre déclaré.
- **2 clés i18n** (+ `tools/add_i18n_keys.R` mis à jour, forme canonique
  `\uXXXX`).
- **`tests/testthat/test-da-milo-async.R`** (nouveau, 21 assertions) et une
  garde de non-régression du flux RNG dans `test-core-jobs.R`.

### Corrigé — deux échecs silencieux
- Le pool résolvait les chemins de préchargement depuis `getwd()` : sous
  `testthat::test_file()` (cwd = dossier de test) **tous** les `file.exists()`
  étaient faux, le préchargement était **sauté en silence** et le daemon levait
  `could not find function "assert_seurat"`. Corrigé par
  `.resolve_app_base_dir()`, **plus** un avertissement côté processus principal
  quand un fichier de préchargement manque — l'échec n'est plus muet.
- Repli synchrone **déclaré et averti** (notification UI) si le pool est
  indisponible, au lieu d'un échec non expliqué.

### ⚠️ Ce que ce jalon NE fait PAS
- **L'UI reste bloquante.** `run_job(async = TRUE)` fait un collect **bloquant**
  par contrat. Le passage en non-bloquant (`ExtendedTask` +
  `bslib::bind_task_button()`) est un **jalon séparé, non ouvert**. Ne pas
  annoncer « UI débloquée ».
- **scCODA reste synchrone** (reticulate/TensorFlow = un interpréteur Python par
  daemon) — exclusion **permanente**, pas un report.
- Sérialiser la **charge utile seule** exigerait de modifier le contrat gelé
  `MILO_RESULT_CONTRACT.md` — non fait, non ouvert.

### Vérifié
Tests ciblés : **255 PASS / 0 FAIL / 0 ERROR / 0 SKIP** (7 fichiers).
`check_conventions.R` = 0 err / 324 avert. (baseline exacte) ;
`check_duplication.R` = 0 err / 3 avert. ; arbre applicatif sourcé de bout en
bout (124 fichiers, 0 échec). Détail : `docs/STATUS.md` [§2ay](docs/archive/STATUS_JOURNAL.md), rapport
`docs/ROADMAP_HANDOFF_STAGE_4E_4.md`.

## [V1.x — FERMETURE renv] — 2026-09-15 — fermeture de dépendances complète (447 → 482) + garde §4

### Le problème mesuré
Un lockfile n'est restaurable que si la **fermeture** `Depends`/`Imports`/
`LinkingTo` de ses paquets y figure aussi. Mesure : elle ne l'était pas.

| État | Paquets | Trous de fermeture |
|---|---|---|
| `HEAD` | 438 | **10** (pré-existants) |
| après enregistrement des 9 | 447 | **35** (+24, leurs dépendances) |
| après fermeture | **482** | **0** |

Les 10 trous pré-existants venaient de paquets **déjà** dans le lock :
`ggpubr` → `ggsci`, `ggsignif`, `polynom`, `rstatix` ; `miloR` →
`ggbeeswarm`, `pracma` ; `jsonlite` → `RcppML`. **Le lock était donc déjà non
restaurable avant ce jalon** — le défaut n'a pas été introduit ici.

### Ajouté
- **`tools/check_renv_hermeticity.R` §4 — fermeture de dépendances** : remonte
  `Depends`/`Imports`/`LinkingTo` récursivement depuis les `DESCRIPTION`
  installés (via `read.dcf`, base R, aucun paquet chargé) et compte comme
  **erreur** tout paquet de la fermeture absent du lock. Le garde mesure
  désormais la vraie propriété — *« `restore()` tient-il ? »* — et non plus
  seulement *« chaque entrée existe-t-elle ? »*.
  **Vérifié sur un cas négatif** : le lock de `HEAD` est bien signalé à 10 trous.

### Corrigé
- **44 paquets enregistrés** (447 → 482) : les 9 utilisés mais non déclarés
  (`decoupleR`, `GSVA`, `msigdbr`, `survminer`, `sva`, `variancePartition`,
  `WGCNA`, `sceasy`, `loomR`) puis les **35** de la fermeture (`GSEABase`,
  `dynamicTreeCut`, `impute`, `preprocessCore`, `lmerTest`, `Hmisc`,
  `SpatialExperiment`, `fastcluster`, `genefilter`, …).
- **15 paquets** ramenés de `R-4.4.2/library` vers la bibliothèque du projet
  (copie, pas déplacement) : `beeswarm`, `corrplot`, `ggbeeswarm`, `ggsci`,
  `ggsignif`, `Hmisc`, `htmlTable`, `litedown`, `markdown`, `polynom`,
  `preprocessCore`, `RcppML`, `rstatix`, `SpatialExperiment`, `vipor`.
- Les 4 paquets Bioconductor enregistrés via `renv::record()` (`GSVA`,
  `decoupleR`, `sva`, `variancePartition`) passés à la forme **canonique**
  `Source: Bioconductor` + `Repository: "Bioconductor 3.20"` (celle des 67
  entrées existantes, et celle qu'écrit `renv::snapshot()`), au lieu de la forme
  minoritaire `Source: Repository` + `Repository: BioCsoft`.
- Après correction : **470 / 470 hermétiques, 0 hors projet, 0 trou de
  fermeture, 0 erreur** (2 avertissements = `dorothea`, `progeny`, optionnels).

### Note technique — enregistrer un paquet GitHub **hors réseau**
`renv::record("owner/repo@sha")` échoue ou **bloque** (résolution de remote
distante ; 4 min 27 sans résultat), et `renv::record("sceasy")` répond
`failed to resolve remote 'sceasy'`. Solution retenue : appeler la fonction que
renv emploie **lui-même** pour enregistrer un paquet installé,
`renv:::renv_snapshot_description(<chemin>)` — hors réseau, et garanti identique
à ce qu'un `snapshot()` écrirait.

### Note technique — `git diff --numstat` peut mentir
Insérer un gros bloc fait mal aligner le diff de **Myers** : ce jalon affichait
`1631 23` (23 « suppressions ») alors qu'**aucun** bloc pré-existant n'était
modifié. Vérification fiable, dans cet ordre :
1. `git diff --diff-algorithm=patience --numstat` → `1608 0` ;
2. comparaison **bloc par bloc** des `Packages` (nom → lignes) entre `HEAD` et
   le fichier : 438 → 482, **0 disparu, 0 modifié**.

### ⚠️ Reste ouvert (jalon distinct)
**14 dérives de version** lock ↔ bibliothèque — `bbotk`, `bit64`, `bslib`,
`class`, `future`, `hexbin`, `igraph` (lock 2.2.1 / installé 2.3.3),
`mlr3learners`, `nnet`, `sf`, `spatstat.explore`, `spatstat.geom`,
`spatstat.random`, `xml2`. **Toutes pré-existantes** (versions identiques à
`HEAD`, vérifié). C'est ce que `renv::status()` signale encore
(`synchronized: FALSE`). Les corriger suppose de **réinstaller** ces paquets aux
versions du lock — donc re-mesurer la suite complète ensuite.

## [V1.x — HERMÉTICITÉ renv] — 2026-09-15 — 7 paquets hors bibliothèque projet + garde

### Ajouté
- **`tools/check_renv_hermeticity.R`** (nouveau garde) : pour chaque paquet du
  lock, vérifie **où il se résout**. Sortie `0` si hermétique. Ne charge aucun
  paquet ⇒ échappe au segfault de teardown 139.
  Usage : `Rscript tools/check_renv_hermeticity.R [--strict]`

### Corrigé
- **7 paquets** se résolvaient hors de la bibliothèque du projet, depuis
  `R-4.4.2/library` : `AsioHeaders`, `chromote`, `ggpubr`, `pingr`,
  **`shiny.i18n`**, `shinytest2`, `websocket`. `shiny.i18n` est une
  dépendance **d'exécution** (i18n), pas un détail de test.
  Après correction : **426 / 426 hermétiques, 0 erreur**.
- Versions vérifiées **identiques au lock** avant déplacement ⇒ aucun
  changement de comportement. `renv.lock` **inchangé** (md5 identique).

### Note technique — pourquoi `renv::install("ggpubr")` échouait
Aucun **binaire Windows** pour ggpubr **1.0.0** (seul 0.6.3 est binaire) ⇒
construction depuis les sources ⇒ `ERROR: lazy loading failed`, le segfault de
teardown déjà documenté en CC-1. Contournement identique :
`Rcmd INSTALL --no-clean-on-error --no-test-load` puis
`tools:::.install_package_namespace_info()` (127 exports, chargement OK).

### ⚠️ Reste ouvert (jalon distinct)
**11 paquets utilisés mais absents de `renv.lock`** : `decoupleR`, `GSVA`,
`loomR`, `msigdbr`, `sceasy`, `survminer`, `sva`, `variancePartition`,
`WGCNA`, et **`dorothea` / `progeny` non installés du tout**.
`renv::restore()` sur une machine propre ne les poserait pas.

> ✅ **Traité par le jalon suivant** — `[V1.x — FERMETURE renv]` (ci-dessus) :
> les 9 sont enregistrés, et la **fermeture de dépendances** complète l'est
> aussi (482 paquets, 0 trou).

## [V1.x — HOTFIX `ns`] — 2026-09-15 — « impossible de trouver la fonction "ns" » dans deux serveurs

### Corrigé
- `modules/sc/mod_sc_pathways.R` (`mod_sc_pathways_server`) : le `renderUI`
  `network_ui` (réseau d'enrichissement STAT-S2) appelait `ns(...)` alors que
  `ns` n'est lié **que** dans les fonctions UI (`NS(id)`), jamais dans le
  serveur ⇒ `impossible de trouver la fonction "ns"` à l'affichage du panneau,
  après une analyse de voies.
- `modules/sc/mod_sc.R` (`mod_sc_server`) : **même bug**, trouvé par balayage
  systématique, dans le `renderUI` `multisample_overview_ui` (4 appels `ns()`).
- Correctif : `ns <- session$ns` en tête des deux serveurs.

### Pourquoi c'était invisible
L'erreur n'est levée **que** quand la branche `renderUI` s'affiche : elle passe
à travers le démarrage de l'application et à travers tout test qui ne rend pas
l'UI. `session$ns(...)` (déjà utilisé ailleurs dans le dépôt) y échappe.

### Ajouté
- Garde **statique** `C15` dans `test-release-hardening.R` : tout serveur de
  module qui appelle `ns()` doit le lier. Elle attrape la **classe** entière,
  pas seulement ces deux occurrences — **vérifié sur cas négatif** : elle
  détecte bien les deux versions committées d'avant correctif.

## [V1.x — CC-6] — 2026-09-15 — Import d'un objet CellChat (.rds) : lis enfin un VRAI objet

### Corrigé (défaut bloquant, trouvé à l'exécution — hors proposition)
- `parse_cellchat_object()` lisait `net$prob` comme
  `[ligand, récepteur, "sender|receiver"]` : une forme **fictive** qu'aucune
  version de CellChat ne produit. Mesuré sur un objet CellChat 2.2.0.9001
  réel : `net$prob` est **3 × 3 × 109** = `[groupe source, groupe cible,
  interaction_name]`, et **0/109** noms d'interaction ne contiennent `|`.
  La route « importer un objet CellChat (.rds) » échouait donc pour **tout
  objet réel**, avec un message accusant le fichier de l'utilisateur
  (« sans separateur '|' unique »). Aucun test ne l'avait vu : tous passaient
  par un stub de la forme fictive — qui était même acceptée silencieusement.

### Modifié
- L'extraction est **déléguée** à `.cellchat_engine_extract()` (règle 3 :
  étendre, ne pas dupliquer) : il n'existe plus qu'**une seule** lecture de
  `net$prob` dans l'application, commune au moteur Path B et à l'import.
- `ligand`, `receptor` et **`pathway`** sont désormais **résolus** via
  `@LR$LRsig` sur cette route (ils ne sont plus systématiquement `NA`) —
  alignement des deux voies. `docs/contracts/COMMUNICATION_RESULT_CONTRACT.md`
  §5 mis à jour **dans le même commit** (code + contrat + test de gel).
- Les états du moteur sont **traduits** dans le vocabulaire d'import
  (`no_interactions` → `invalid_input`, sinon `invalid_schema`).

### Ajouté
- Un test de non-régression qui **construit un véritable objet CellChat** et
  vérifie que l'import le lit (c'est lui qui manquait).
- Garde de gel : interdiction du retour du découpage fictif
  (`pairs_split`, `dimnames(prob)[[`) dans `R/sc/sc_communication.R`.

## [V1.x — CC-5] — 2026-09-15 — Calcul CellChat dans l'application (UI Path B)

Le moteur était **exécutable mais inexposable** : aucune action ne permettait
de lancer un calcul. `modules/sc/mod_sc_communication.R` gagne une **5ᵉ
source**, « Calculer dans l'application (CellChat) ».

### Ajouté
- **`comm_engine_species`** : espèce `human`/`mouse` **sans défaut** — la base
  ligand-récepteur en découle et ne se devine pas à partir des données (même
  motif que le mode d'agrégation LIANA : un choix implicite produirait un
  artefact d'analyse).
- **`comm_engine_seed`** / **`comm_engine_nboot`** : graine et nombre de
  permutations, exposés et tracés dans la provenance.
- **`comm_compute`** : bouton dédié. Le bouton « Importer et valider » est
  **masqué** pour cette source, et la branche import s'en protège explicitement
  (sans quoi elle exigerait un fichier inexistant).
- Test de gel `tests/testthat/test-sc-communication-engine-ui.R` (53
  assertions).

### Modifié
- **`.store_result()`** : le dépôt du résultat (état, empreinte objet, rapport
  consolidé, provenance, remise à zéro des filtres) est **factorisé** et
  désormais **commun aux deux voies** — c'est la garantie que vues et exports
  ne peuvent pas diverger.
- **Aucune vue ni aucun export n'a été modifié** : preuve mécanique de la règle
  des deux voies.

### Corrigé
- `test-communication-contract-freeze.R` : l'invariant « aucun appel CellChat »
  était exprimé par un grep sur `CellChatDB`, qui interdisait aussi le simple
  **libellé** du choix d'espèce. Reformulé **sans perdre la garantie** : plus
  de `library()`, plus de `computeCommunProb`, plus d'accès `CellChatDB$`, plus
  d'appel `CellChat::`, et le nom de la base n'apparaît que dans des libellés
  (occurrences comptées).

## [V1.x — CC-1] — 2026-09-15 — CellChat épinglé par SHA (moteur exécutable)

Le moteur Path B livré au jalon précédent était **non exécutable** faute de
dépendance installée. `CellChat` 2.2.0.9001 est désormais **épinglé par SHA**
(`75253cd0…358f`) et inséré au lockfile avec les **12 dépendances manquantes**
mesurées : 425 → **438** entrées, **+535 / −0 lignes**, **0 changement de
version**. Le bloc « run réel » des tests n'est plus skippé.
Détail : `docs/archive/STATUS_JOURNAL.md` **§2ao.5**.

### Ajouté
- `renv.lock` : `CellChat` (`Source: GitHub` + SHA) et `coda`, `collapse`,
  `ggalluvial`, `ggnetwork`, `ggpubr`, `gridBase`, `network`, `NMF`, `registry`,
  `rngtools`, `sna`, `statnet.common`.
- Garde amont dans `run_cellchat()` : si `nrow(LR$LRsig) == 0`, état
  **`no_interactions`** avec un message qui dit ce qui manque — au lieu du
  « subscript out of bounds » opaque remonté par CellChat.
- Test « aucune paire LR exploitable » (jeu jouet mesuré : 0 paire).

### Corrigé
- **`database_version` restait `NA`** : `CellChatDB.human$version` n'existe pas.
  La version est lue dans `interaction$version` — base **mixte** `v1` + `v2`,
  valeurs distinctes rapportées, jamais un choix arbitraire.
- **`engine_sha` restait `NA`** : `utils::packageDescription()` lit
  `Meta/package.rds`, qui ne porte pas les champs `Remote*`. Repli sur
  `read.dcf()` du `DESCRIPTION` lui-même.

### ⚠️ Réserve assumée
- Installation de `CellChat` **incomplète sur ce poste** : `R CMD INSTALL`
  échoue au lazy-load à cause du **segfault de teardown** (tout processus R
  chargeant `dplyr` / `ggplot2` / `igraph` sort en 139 — documenté `STATUS_JOURNAL.md`
  §2l). Contournement `--no-clean-on-error --no-test-load` puis
  `Meta/nsInfo.rds` régénéré : le paquet **fonctionne**, mais son arbre `Meta/`
  reste partiel (`data.rds`, index d'aide).
- `ggpubr` (dépendance directe) est résolu depuis la bibliothèque **système**
  `R-4.4.2/library`, pas depuis celle du projet : enregistré au lockfile, mais
  l'isolation renv n'est pas totale tant qu'il n'est pas installé côté projet.

## [V1.x — CC-2/3/4] — 2026-09-15 — Moteur CellChat natif (Path B)

Décision utilisateur du 2026-09-14 (`docs/archive/STATUS_JOURNAL.md` §2al) : **choix B retenu**,
**Path A (import CSV/TSV externe) impérativement conservé** et export conservé.
Jalons **CC-2** (contrat), **CC-3** (moteur) et **CC-4** (tests + gardes) livrés
ensemble — la règle contract-first exige code + test + doc dans le même commit.
⚠️ **CC-1 (épinglage de la dépendance) n'est pas abouti** : le moteur est livré
mais **non exécutable** tant que `CellChat` n'est pas installé
(`run_cellchat()` lève `missing_dependency` avec guidage). L'application démarre
et Path A continue de fonctionner — détail : `docs/archive/STATUS_JOURNAL.md` **§2ao**.

### Ajouté
- **`R/sc/sc_communication_engine.R`** — `run_cellchat(cellchat_input, seed,
  nboot, …)` : `createCellChat()` → `subsetData()` →
  `identifyOverExpressedGenes/Interactions()` → `computeCommunProb()` →
  `computeCommunProbPathway()` → extraction → `finalize_communication_result()`
  avec `computation = "engine"`. Réduction **immédiate** aux **12 champs
  canoniques** du contrat Stage 11 ; objet moteur **éphémère** (jamais stocké).
- **`cellchat_analysis_identity()`** — identité d'analyse **dérivée** de
  `new_provenance_entry()` (jamais dupliquée, règle 3) + les deux seuls champs
  réellement nouveaux : `engine_sha` et `database_version`.
- `cellchat_engine_available()` / `cellchat_engine_states()` /
  `cellchat_engine_error_state()` / `cellchat_engine_summary()`.
- **29ᵉ contrat gelé** : `docs/contracts/CELLCHAT_ENGINE_CONTRACT.md`.
- `config/defaults.R` : `TS_CELLCHAT_NBOOT_DEFAULT` (100),
  `TS_CELLCHAT_SEED_DEFAULT` (1), `TS_CELLCHAT_MIN_GROUPS` (2). **Aucun** seuil
  de RAM / cellules / clusters : benchmark d'abord (proposition §9.3).

### Modifié
- `R/sc/sc_communication.R` : **un seul** argument additif,
  `computation = c("import", "engine")` (défaut `"import"`), qui bascule
  `provenance$method` et `provenance$import_only`. **Aucun appel existant ne
  change de comportement** — le freeze test Stage 11 qui assère
  `import_only = TRUE` continue de passer.
- `app.R` : `source()` du moteur **après** `R/sc/sc_communication_input.R`.
- `docs/contracts/COMMUNICATION_RESULT_CONTRACT.md` : §1 et §5 requalifiés.

### Corrigé au passage (documentation)
- La proposition annonçait « +8 à +12 » entrées de lockfile : **mesuré**, 39
  dépendances directes dont **32 déjà au lock** et **11 à installer**. Et le
  paramètre de permutation s'appelle **`nboot`**, pas `nPerm` — piège signalé
  par la proposition §9.4, désormais verrouillé par un test.

## [V1.x — NEW-1] — 2026-09-13 — Dose–réponse / time-course (drc)

Rang 4 de l'ordre d'actionnabilité. **Première dépendance nouvelle depuis
l'amendement du 2026-09-13** : `drc` 3.0-1 (CRAN, pur R, léger) + transitifs
(`multcomp`, `sandwich`, `TH.data`, `plotrix`, `mvtnorm`) — justification
documentée au contrat (`docs/contracts/BULK_DOSE_RESPONSE_CONTRACT.md` §2) ;
`renv.lock` 419 → 425 entrées par insertion chirurgicale (les packages MCP
restent volontairement exclus). Analyse **descriptive** : aucune p-value de
comparaison.

### Ajouté
- **`run_dose_response(vst_mat, metadata, dose_column, genes, model, …)`
  (`R/bulk/dose_response.R`)** — ajustement `drc::drm()` **par gène** (LL.4
  Hill par défaut, W1.4/W2.4/BC.4), extraction b/c/d/e (EC50 = exp(e)),
  pseudo-R², grille de courbe (100 pts) + IC 95 %. Dose numérique **déclarée**
  et **strictement positive** (les modèles sont en log(dose)) ; ≥ 4 doses
  distinctes ; plafond 200 gènes ; échecs par gène comptabilisés
  (`fit_ok`/`message`), `compute_failed` si aucun ne converge.
- **Module « 3f. Dose–réponse / time-course »** (`mod_bulk_dose_response.R`)
  côté Bulk : sources up/down/all_sig **triées par p.adjust**, top N déclaré,
  modèle choisi ; sortie : courbe par gène (points + courbe + ruban IC, axe
  log10), table EC50/pente/R², exports CSV + PNG.
- `config/thresholds.R` : `TS_BULK_DOSE_{MIN_DOSES,MAX_GENES,CURVE_POINTS}`.
- Tests : `test-bulk-dose-response.R` **38 PASS / 0 FAIL** (courbes de Hill
  synthétiques avec EC50 connu retrouvé, échecs comptabilisés) ; ciblés :
  e2e `shinytest2-bulk` 4 PASS, freezes app.R 68+80, i18n 15.
  **Suite complète non lancée ce jalon (consigne utilisateur).**
- i18n : **24** clés FR/EN (2434 → 2458 entrées cumulées).

### Justification de dépendance (renv.lock)
- `drc` : curve-fitting Hill/log-logistique/Weibull — aujourd'hui réalisable
  **sans DRomics complet** (on réutilise filtrage/DE/VST existants).
- Ajout pur : aucun appel existant modifié, aucune régression mesurée.

### Hors périmètre
- DRomics complet, modèles 5 paramètres, comparaison de courbes entre groupes,
  intégration au rapport bulk — non demandés.

## [V1.x — STAT-S3] — 2026-09-13 — Clustering de profils (kmeans MVP)

Rang 3 de l'ordre d'actionnabilité. **Zéro dépendance nouvelle** — `stats::kmeans`
et la matrice VST existante (étape 1). Analyse **descriptive** de la forme des
profils entre groupes : aucune p-value produite.

### Ajouté
- **`run_pattern_clustering(vst_mat, metadata, group_column, genes, k, seed, …)`
  (`R/bulk/bulk_pattern.R`)** — moyenne VST par groupe, **z-score par gène** à
  travers les groupes, `stats::kmeans()` (nstart/itermax déclarés en config,
  graine tracée dans le résultat). Exclusions comptabilisées : gènes fournis
  absents de la matrice, gènes constants/NA, échantillons sans libellé.
- **`docs/contracts/BULK_PATTERN_CONTRACT.md`** — contrat gelé (type
  `bulk_pattern_clusters`, `analysis_id` `"bulk-pattern-clusters"`) ; surface
  publique figée (8 fonctions) ; erreurs classées `bulk_pattern_error` (FR).
- **Module « 3e. Clustering de profils »** (`mod_bulk_pattern.R`) côté Bulk :
  sources de gènes `up`/`down`/`all_sig` (convention du module Enrichissement),
  colonne de groupe déclarée, k (2–12, plafond configuré, **aucun défaut
  métier**) et graine déclarés ; sortie : courbes de profils moyens par
  cluster (`plot_pattern_profiles`, palette partagée), table gènes→clusters,
  export CSV.
- `config/thresholds.R` : `TS_PATTERN_KMEANS_{NSTART,MAX_K,ITERMAX,SEED}`.
- Tests : `test-bulk-pattern.R` **31 PASS / 0 FAIL** (100 % hors-ligne) ;
  ciblés : e2e `shinytest2-bulk` 4 PASS, freezes consommant app.R 68+80 PASS,
  i18n 15 PASS. **Suite complète non lancée ce jalon (demande utilisateur).**
- i18n : **24** clés FR/EN (2410 → 2434 entrées cumulées).

### Non retenu
- **V2 floue (Mfuzz)** — option de la fiche : dépendance Bioconductor
  nouvelle, aucun besoin exprimé (écrit au contrat §8).
- Sélection automatique de k (silhouette/elbow) — non demandée.

## [V1.x — STAT-S2] — 2026-09-13 — Réseau d'enrichissement (emapplot / cnetplot)

Rang 2 de l'ordre d'actionnabilité. **Aucune dépendance nouvelle** —
`enrichplot` (1.26.6) était déjà dans `renv.lock`. Visualisation **descriptive**
des voies : les arêtes codent une similarité de gènes ou une appartenance,
jamais une causalité.

### Ajouté
- **`plot_pathway_network(df, db_label, top_n, mode, tr)`**
  (`R/core/pathway_helpers.R`) — `mode = "emap"` : voies reliées par similarité
  de gènes (`enrichplot::pairwise_termsim()` + `emapplot()`) ; `mode = "cnet"` :
  voies reliées à leurs gènes (`cnetplot()`). Erreurs FR classées (`call. =
  FALSE`) : attribut brut absent, `top_n < 2`, moins de deux voies.
- **Attribut additif `enrich_obj`** : `run_pathway_enrichment()` (ORA) attache
  désormais l'objet `enrichResult` brut à son data.frame — même pattern que
  `attr(., "gsea_obj")` (GSEA) ; le contrat data.frame des appelants existants
  est inchangé.
- **Onglet « Réseau »** dans les cartes de sortie pathway **bulk**
  (`mod_bulk_pathways.R`) et **single-cell** (`mod_sc_pathways.R`) : radio
  emap/cnet, nombre de voies (2–100), export PNG 300 dpi (bulk), message d'aide
  quand aucun résultat n'est disponible.
- Tests : 5 nouveaux blocs dans `test-pathway-helpers.R` (**16 PASS**) — dont
  un happy path avec un **vrai `enrichGO` hors-ligne** (GO:0007049 depuis
  `org.Hs.egGO2ALLEGS`), zéro accès réseau.
- i18n : **8** clés FR/EN (2375 → 2410 entrées cumulées).

### Modifié
- `R/core/pathway_helpers.R` (attribut + fonction), `modules/bulk/
  mod_bulk_pathways.R`, `modules/sc/mod_sc_pathways.R`,
  `tools/add_i18n_keys.R`, `i18n/translation.json`.

### Non modifié
- Barplot/dotplot/table existants (consomment le même data.frame) ;
  `renv.lock` ; aucune dépendance nouvelle.

## [V1.x — CCC 9] — 2026-09-13 — Rareté par population annotée (descriptif)

Implémentation de la **question 1** de la phase 9 (« quelles populations
annotées sont rares ? »), tranchée le 2026-09-13. Jalon **descriptif et
mono-condition** : **aucun graphe kNN**, aucune affirmation différentielle —
la porte Stage 13 est inapplicable et le motif est **écrit au contrat**.
**Aucune dépendance nouvelle, `renv.lock` intouché.**

### Ajouté
- **`compute_population_rarity(meta, identity_column, rule_type, threshold,
  sample_column, seurat_obj)`** — décompte des cellules par niveau d'une
  colonne d'identité déclarée ; `is_rare` résulte d'une **règle déclarée**
  (`absolute_n_cells` ou `relative_fraction`), jamais d'une vérité biologique.
  La règle est **dans le résultat** (`rarity_rule`) et dans la provenance
  (`descriptive_only = TRUE`).
- **`docs/contracts/POPULATION_RARITY_CONTRACT.md`** — contrat gelé
  (`type = "sc_population_rarity"`, `analysis_id = "sc-population-rarity"`) ;
  porte Stage 13 inapplicable, motif écrit (§1.1) ; corollaire contraignant :
  toute comparaison entre conditions repasse par la porte DA.
- **Surface publique gelée** (12 fonctions) : `population_rarity_contract_fields`,
  `population_rarity_validity_states`, `population_rarity_status_labels`,
  `population_rarity_rule_types`, `population_rarity_error_state`,
  `population_rarity_is_stale` (empreinte v2 réutilisée),
  `assert_population_rarity_result`, `build_population_rarity_summary`,
  `build_population_rarity_table_export`, `population_rarity_export_filename`,
  `population_rarity_public_api`.
- **Onglet « 2b. Rareté par population »** dans le panneau Single-Cell existant
  (jamais un nouveau panneau latéral) : table, compteurs, figure descriptive,
  export CSV. Le champ seuil part **vide** — le calcul refuse sans seuil
  (**aucun défaut implicite**).
- **Section optionnelle du rapport SC** (« Rareté par population »), consommant
  le résultat canonique — aucune ré-exécution.
- `config/defaults.R` : `TS_POPULATION_RARITY_RULES` (règles autorisées) et
  `TS_POPULATION_RARITY_MIN_CELLS_TOTAL` (plancher de garde 50) — **aucun
  seuil de rareté par défaut**.
- Tests : `test-sc-population-rarity.R` (**98** assertions) +
  `test-sc-population-rarity-contract-freeze.R` (**83**) — suite complète :
  **0 FAIL / 0 ERROR** ; 4788 PASS / 2 SKIP sur le run (SKIP de référence GEO +
  flake chromote sur `test-shinytest2-bulk`, repassé seul **4/4**) → effectif
  **4790 PASS** (référence 4609 → +181, aucune régression).

### Modifié
- `modules/sc/mod_sc.R` — onglet + case `report_sections` + serveur + passage
  du résultat au rapport.
- `app.R` — 2 `source()` ; `reports/sc_report_template.Rmd` — paramètre +
  section ; `i18n/translation.json` + `tools/add_i18n_keys.R` — clés FR/EN.

### Non modifié
- Rapport consolidé 4F (12 domaines figés), tableau croisé cluster × type
  (`mod_sc_annotation.R`), table d'identités du design DA (`sc_abundance_design.R`),
  `renv.lock`. Les questions 2 (rareté par voisinage) et 3 (rareté ×
  communication) ne sont **pas** retenues.

## [V1.x — CCC 7–8 route (b)] — 2026-09-13 — Import de rangs LIANA

Premier jalon d'**interopération communication cellule–cellule** (`a88577f`).
**Aucune dépendance nouvelle, `renv.lock` intouché**, aucun calcul d'inférence
dans l'application : la route (b) importe une table **agrégée produite par
LIANA hors de l'app**.

### Ajouté
- **`parse_liana_import(tab, rank_column, aggregation_mode, source_file)`** —
  convertit une table agrégée LIANA vers la table canonique de communication.
  Ni le mode d'agrégation ni la colonne de rang ne sont déduits du fichier :
  les deux sont **déclarés explicitement** (aucun défaut implicite).
- **`communication_rank_fields()`** = `rank`, `rank_direction`,
  `rank_aggregation_mode` ; **`communication_rank_aggregation_modes()`** =
  `specificity`, `magnitude`. Surface **séparée** des 12 champs contractuels.
- **`"liana"`** ajouté à `communication_supported_sources()`.
- **`provenance$is_external_consensus`** — TRUE quand la table porte un agrégat
  **inter-méthodes** calculé par LIANA (`mean_rank`, `aggregate_rank`).
- **QC des rangs** : `n_rank_out_of_range`, `n_rank_missing` (0 pour les sources
  sans rang).
- **UI** : 4ᵉ route dans le sélecteur de source existant + panneau conditionnel
  (mode d'agrégation **sans sélection par défaut**, colonne de rang proposée
  depuis l'en-tête du fichier). **+8** clés i18n (2367 → **2375**).
- `tests/testthat/test-sc-communication-liana.R` — **73** assertions.

### Modifié
- `docs/contracts/COMMUNICATION_RESULT_CONTRACT.md` — §1, §2, §4, §5, §6, §9,
  §10 : source `liana`, champs de mesure de rang, nuance « consensus importé ≠
  consensus calculé », évolution **additive** documentée. **Même commit** que le
  code et les tests de gel (contract-first).
- `tests/testthat/test-communication-contract-freeze.R` — gels étendus +
  `parse_liana_` ajouté au ban des consommateurs rapport.

### Notes
- **`aggregate_rank` → `p_value`** : c'est une p-value (*Robust Rank
  Aggregation*, `min(p) × k`), pas un score de communication.
- **`score` reste `NA`** sur cette route : un rang n'est pas un score (échelle
  **et** direction différentes).
- **`rank_direction = "lower_is_better"`** : dans LIANA, rang 1 = meilleur —
  c'est l'**inverse** de `prob` (CellChat).
- **Les colonnes `.complex` ne sont jamais découpées** : un complexe n'a pas de
  découpage univoque, le deviner serait inventer une donnée.
- **Non-régression** : CellChat et CellPhoneDB **ne gagnent aucune colonne de
  rang** et leur résultat est inchangé (test dédié) — les parseurs existants
  n'ont pas été touchés.
- **Risque connu, non traité** : les vues exploratoires supposent un score
  orienté « plus grand = meilleur » ; sur une source de rangs, échelles et
  filtre « score minimum » peuvent être inversés. À traiter **vue par vue**.

## [V1.x — Maintenance] — 2026-09-13 — Conventions de code, i18n, versionnage

Passe transversale **sans changement de comportement** : aucune méthode
statistique, aucun paramètre par défaut, aucun tracé modifié.

### Ajouté
- **`docs/CONVENTIONS.md`** — une seule source de vérité pour les conventions
  de code : arborescence et responsabilités, `R/` pur (avec l'exception
  assumée de la couche d'état), nommage, contract-first, erreurs classées,
  i18n, paramètres déclarés, async/cache/provenance, tests. Chaque règle porte
  un identifiant **C1..C12**.
- **`tools/check_conventions.R`** — garde statique en **base R uniquement**
  (même esprit que `check_duplication.R`, exécutable sans installation) :
  vérifie C1..C12. État mesuré : **0 erreur**, **324 avertissements** de dette
  (C6 = 16 `library()` au top-level de `R/`, C9 = 37 fichiers sans test
  éponyme, C10 = 270 `stop()` non classés, C11 = 1 `MulticoreParam` sous garde
  Unix). Exit 0 = vert ; `--strict` fait échouer sur la dette.

### Corrigé
- **🐛 Un fichier sourcé n'avait jamais été commité.** `R/plotting/complex_heatmap.R`
  (cœur de PLOT-S4, marqué livré) était absent de `HEAD` et exclu par le
  `.gitignore` local, alors que `app.R:64` le source : **l'application ne
  démarrait que sur ce poste**. Fichier sorti du `.gitignore` et **commité
  pour la première fois** ; la règle **C3** interdit toute récidive (toute
  cible de `source()` doit exister **et** être versionnée).
- **Dette i18n soldée** : **101 clés** utilisées par `tr()` / `i18n$t()` mais
  absentes de `i18n/translation.json` ont été ajoutées avec leur traduction
  anglaise via `tools/add_i18n_keys.R` (idempotent). Total **2367 clés**,
  **0 clé manquante** (C7), aucune entrée `en` vide — un utilisateur en
  anglais ne retombe plus sur du français non traduit.

### Documentation
- `docs/ROADMAP.md` — nouveau **§2.0 « ordre d'actionnabilité »** daté
  (arbitrage utilisateur 2026-09-13 : CCC 7–8 → CCC 9 → STAT-S2/S3 →
  NEW-1..3 → UX → 4E-4), contrats **24** (au lieu de 13/18), flux B aligné sur
  le parking tranché, décisions 4/5/8 mises à jour, **décisions 9 et 10**
  ajoutées (route CCC 7–8 ; dette de conventions).
- `docs/ROADMAP_HANDOFF_NEXT.md` — **ré-écrit** : le prompt MD-1 (consommé)
  est retiré, l'étape courante devient **CCC 7–8 route (b)** (import de
  résultats LIANA sans dépendance) avec ancres re-vérifiées dans le dépôt.
- `docs/STATUS.md` — §2g corrigé (le « double jeu SC » n'était plus « non
  démarré » : **MD-4** l'a livré), §2e parking CCC tranché, **§2z** = cette
  passe.
- `AGENTS.md` — règles dures **10** (conventions = doc + garde) et **11**
  (jamais gitignorer un fichier sourcé).

> Rappel : les jalons `MD-1`→`MD-4`, `4F-EXT`, `PLOT-S1..S5`, Bulk V2 M2–M5
> n'ont **pas** d'entrée propre ici — leur état fait foi dans `docs/STATUS.md`.

## [V1.x — STAT-S1] — 2026-09-12 — Correction de batch ComBat-seq

Retrait d'un **effet de lot technique** avant l'analyse différentielle Bulk,
par **ComBat-seq** (`sva`) plutôt que le ComBat classique : il agit sur les
**comptages bruts** (modèle binomial négatif) et son paramètre `group=`
**protège la condition biologique** — un ComBat sur matrice transformée peut
l'effacer. Contrat gelé `docs/contracts/BATCH_CORRECTION_CONTRACT.md`
(freeze : `test-bulk-batch-correction-contract-freeze.R`).

### Ajouté
- **Noyau pur** `R/bulk/batch_correction.R` :
  `bulk_batch_correction_public_api()` (inventaire gelé) ;
  `bulk_assert_raw_counts()` — **miroir exact** de
  `bulk_assert_transformed_matrix()` (`bulk_batch_qc.R`) : **même seuil de
  0,95** de fraction entière, appliqué **dans le sens inverse** (ce que l'une
  accepte, l'autre le refuse) ; `bulk_batch_correction_design()` — réutilise
  `bulk_batch_design_check()` (cross-table, **colinéarité lot/condition**) et
  décide `can_apply` / `use_group` ; `bulk_batch_correction_label()` —
  provenance **préfixée** (`"<normalisation> + ComBat-seq (batch : X ; groupe :
  Y)"`), jamais écrasée ; `run_combat_seq()` — enveloppe **paresseuse**
  (`requireNamespace("sva")` à l'appel, **jamais** au `source`) ; 
  `plot_batch_correction_pca()` — compose **deux** tracés `plot_bulk_pca()` via
  `patchwork` (`tr` **en dernier**, piège de signature PLOT-S1/S2).
- **5 états d'erreur classés gelés** (`bulk_batch_correction_error`) :
  `invalid_input`, `not_raw_counts`, `degenerate_batch`, `missing_dependency`,
  `compute_failed` — le freeze test **compte** les `state = "…"` du source pour
  interdire l'inflation silencieuse de cette surface.
- **UI** — section repliable « Correction de batch (optionnel) — ComBat-seq »
  dans l'onglet **QC Batch** de `mod_bulk_filter.R` : sélection de la colonne de
  lot, choix de la condition à préserver, diagnostic PCA **avant / après**. La
  copie « pristine » des comptages vit **dans le module** (`bc_pristine`) — 
  **aucune clé de `shared_rv` ajoutée** ; le pipeline ne change **que sur clic
  explicite** (idempotent), et les contrastes déjà calculés sont invalidés.
- **Dépendance** : `sva` ajouté à **`bioc_packages`** — ⚠️ **pas** à
  `required_packages` : l'application **démarre sans `sva`**, la fonctionnalité
  échoue alors proprement en erreur classée `missing_dependency` (message FR
  avec le remède).
- **Seuil déclaré** `config/thresholds.R` :
  `TS_BULK_BATCH_MIN_SAMPLES_PER_BATCH <- 2L`.
- **i18n** : **+15** clés `{fr, en}` (2084 → 2099, 0 doublon).
- Tests : `test-bulk-batch-correction.R` (**57** assertions) + freeze test
  dédié (**69** assertions) → **126 PASS / 0 FAIL / 0 ERROR**.

### Mesuré
- **Critère d'acceptation quantifié** (la fiche proposait une lecture visuelle,
  non testable) : sur comptages binomials négatifs sur-dispersés à **effet de
  lot multiplicatif propre à chaque gène**, le **R² du lot sur PC1** passe de
  **0,74 → 0,23** (< 50 % de l'initial) et l'**écart entre conditions est
  préservé** (> 70 %). `dimnames` conservés.
- Porte de duplication : **0 erreur / 3 avertissements** (baseline).
- `app.R` se source de bout en bout (UI + server construits) — vérifié.

### Limite connue (gelée au contrat §11.1)
- Un **décalage additif uniforme sur tous les gènes n'est PAS corrigé** :
  ComBat-seq le lit comme un **effet de profondeur de séquençage** et l'absorbe
  par son offset. C'est statistiquement correct, mais c'est un **piège de
  fixture** — un test fige ce comportement pour qu'il ne soit pas « corrigé »
  par erreur.

### Notes
- ⚠️ Le contrat vit sous `docs/`, **gitignoré** : le freeze test lit donc
  l'arbre, pas git. Sur un **clone neuf**, ce test échoue (contrat absent) alors
  que le code est intact — voir la décision 4 de `docs/ROADMAP.md` §5.
- ⚠️ `sva` est installé mais **pas encore snapshoté dans `renv.lock`** (snapshot
  différé, règle du dépôt) — dégradation propre si absent.

## [V1.x-D] — 2026-09-06 — Perturbation IN SILICO (roadmap CCC avancée, Phase 4)

Simulation de perturbation sur le **réseau INFÉRÉ** importé (suppression /
atténuation d'un ligand, récepteur, interaction ligand->récepteur ou
population) avec quantification des effets de **premier ordre** sur les
paires et les nœuds, à partir des scores IMPORTÉS — jamais recalculés, aucune
propagation réseau modélisée (anti-feature-creep). Contrat
`docs/contracts/COMMUNICATION_PERTURBATION_CONTRACT.md` (freeze :
`test-communication-perturbation-contract-freeze.R`).

### Ajouté
- **Noyau pur** `R/sc/sc_communication_perturbation.R` :
  `build_communication_perturbation()` (cibles
  `communication_perturbation_targets()` = ligand / receptor / interaction /
  sender / receiver ; valeur EXACTE issue de la table — absente = erreur FR
  classée avec liste des valeurs disponibles ; mode `remove` ou `attenuate`
  avec facteur strictement (0,1)) ; deltas par paire (score_total,
  delta_fraction, affected) et par nœud (sortant + entrant) ; résumés
  baseline/perturbé (NA sans scores — jamais 0 fabriqué) ; la table
  canonique n'est JAMAIS modifiée (copie d'affichage).
- **GARDE ABSOLU** : étiquette « IN SILICO PERTURBATION » portée par le
  résultat (`params$label`, provenance `label = "in_silico_not_ko"`), les
  avertissements, le sous-titre de chaque figure et une colonne de chaque
  export — jamais « KO », « effet biologique » ni « causal ». Effets de
  PREMIER ORDRE explicitement énoncés (NETWORK PERTURBATION ≠ DOWNSTREAM
  TRANSCRIPTIONAL SIMULATION, jamais conflation).
- **Vues** : `plot_communication_perturbation_delta()` (barres divergentes
  top N), `plot_communication_perturbation_nodes()` (baseline vs perturbé
  par nœud) ; `build_communication_perturbation_export()` (traçabilité +
  étiquette par ligne).
- **Module** `modules/sc/mod_sc_communication_perturbation.R` — onglet
  « Perturbation (in silico) » dans le panneau Communication existant,
  bandeau permanent, choix de valeur synchronisé avec la table (filtres du
  panneau honorés), calcul sur bouton explicite, péremption vérifiée,
  CSV/PNG/PDF tracés.
- Tests : `test-sc-communication-perturbation.R` (72 assertions) + freeze
  test dédié.

### Corrigé
- `app.R` : lignes `source()` V1.x-A..D réparées (des insertions par
  sous-chaîne avaient concaténé le commentaire de la ligne précédente sur
  chaque nouvelle ligne et dupliqué trajectory/velocity ; commentaires un
  par ligne, doublons supprimés, lignes perturbation restaurées — commit
  `62510b9`).

### Parking (reste de la roadmap CCC avancée)
Phases 5-6 (ligand→target / NicheNet-like — exigent un réseau prior et une
proposition 4D-3 avec le contrat d'entrée de l'app upstream), Phase 7-8
(OmniPath / LIANA — packages nouveaux, renv.lock justifié ; import de
RÉSULTATS LIANA externes possible en extension du contrat Stage 11 avec
ajout simultané code+freeze+doc), Phase 9 (rare-cell — auditer le
chevauchement Milo), Phase 10 (gallery — règle « aucune dépendance graph
nouvelle ») — voir `docs/ROADMAP_CCC_ADVANCED.md`.

## [V1.x-A/B/C] — 2026-09-06 — Contextes CCC (roadmap CCC avancée, Phases 1-3)

Extension de la Communication cellule-cellule par trois **contextes dérivés**
(consommateurs purs du résultat canonique Stage 11/12 — table gelée intacte,
aucun score recalculé, aucune inférence) : roadmap `docs/ROADMAP_CCC_ADVANCED.md`,
mandat utilisateur « go on, implement phases ». Trois onglets DANS le panneau
Communication existant (aucun nouveau panneau latéral) ; erreurs classées
`communication_context_error` ; provenance PRODUITE au calcul avec
`parent_analysis_id` (chaîne CCC → contexte traçable) ; WIP utilisateur
préservé (staging partiel de app.R).

### Ajouté
- **Contexte spatial (V1.x-A, `1d5d0ab`)** : `R/sc/sc_communication_spatial.R`
  — distances de centroïdes + NN croisées (chunks mémoire bornés) par paire,
  fractions à portée d'un rayon EXPLICITE (jamais inféré), enrichissement par
  permutation OPT-IN (seed fixée, null = permutation des étiquettes) ;
  source de coordonnées DÉCLARÉE (bundle spatial courant ou réduction 2D —
  « distances de projection, NON physiques ») ; la distance = CONTRAINTE
  spatiale, jamais une preuve de communication. Contrat
  `COMMUNICATION_SPATIAL_CONTRACT.md` + freeze test dédié.
- **Contexte trajectoire (V1.x-B, `da84e28`)** :
  `R/sc/sc_communication_trajectory.R` — composition des populations et
  expression ligands/récepteurs le long du pseudo-temps par bins quantiles
  (dégradation comptabilisée, jamais forcée) ; mode PAR LIGNÉE slingshot
  (jamais de collapse) ; gènes absents / cellules sans pseudo-temps comptés
  jamais imputés ; score importé CONSTANT par paire, jamais décliné par bin ;
  `communication_fetch_expression_matrix()` (extraction « data » bornée aux
  gènes, branche layer/slot explicite). Contrat
  `COMMUNICATION_TRAJECTORY_CONTRACT.md` + freeze test dédié.
- **Contexte vélocité (V1.x-C, `4903829`)** :
  `R/sc/sc_communication_velocity.R` — magnitude L2 des vecteurs de vitesse
  PRÉCALCULÉS importés (contrat Stage 10 consommé, jamais ré-inféré) par
  population et paire ; états gracieux `unavailable_no_vectors` /
  `insufficient_overlap` (aucun vecteur substitué, aucune fabrication) ;
  seuil `TS_VELOCITY_OVERLAP_MIN` consommé du config/thresholds.R.
  Contrat `COMMUNICATION_VELOCITY_CONTRACT.md` + freeze test dédié.
- **Modules d'orchestration** : `mod_sc_communication_{spatial,trajectory,
  velocity}.R` montés par le module Communication — calcul sur bouton
  explicite, péremption vérifiée avant rendu/export, CSV/PNG/PDF tracés
  (`analysis_id` + `parent_analysis_id` + timestamp par ligne).
- Tests : `test-sc-communication-{spatial,trajectory,velocity}.R` +
  3 freeze tests + fixtures partagées
  (`helper-communication-context-fixtures.R`).

### Parking (Phases 4-10 de la roadmap)
Perturbation in silico, Ligand→receptor→target, NicheNet-like, OmniPath,
LIANA, rare-cell, gallery — requièrent 4D-3 (contrat d'entrée app upstream
non gelé), des dépendances nouvelles (renv.lock justifié) ou une proposition
scientifique dédiée ; voir `docs/ROADMAP_CCC_ADVANCED.md` §4.

## [V1.1.0-rc] — 2026-09-05 — Import .rda/.RData « Inspect & Select »

Support des fichiers `.rda`/`.RData` (workspaces `save.image()`, listes
mixtes) selon le paradigme **« Inspecter d'abord, importer ensuite »** :
contrat `docs/contracts/RDATA_IMPORT_CONTRACT.md` (gel :
`test-rdata-contract-freeze.R`). Zéro changement sur les chemins `.rds`,
`.h5`, `.h5ad`, `.loom` existants.

### Ajouté
- **Noyau pur** `R/core/rdata_io.R` : `rdata_load_env()` (environnement
  isolé `parent = emptyenv()`, jamais globalenv), `rdata_describe_objects()`
  (Nom / Classe / Dimensions / Taille / Type), `rdata_classify_object()`
  (codes gelés, AFFICHAGE UNIQUEMENT — jamais d'auto-guess),
  `rdata_extract_object()`, `rdata_assert_class()` (validation AVANT écriture
  dans `global_data`), `rdata_export_selection()`, `rdata_free()`.
- **Composant Shiny mutualisé** `modules/import/mod_rdata_picker.R` :
  carte de preview (DT multi-sélection) avec « Importer l'objet sélectionné »
  (exactement 1 ligne) et « Exporter la sélection (.RData) » — bundle .RData
  téléchargé via le navigateur, SANS écriture dans `global_data`.
- **Exploration imbriquée + .rds** : les listes nommées sont APLATIES en
  chemins explorables (`objet$enfant$...`, profondeur 3 — cas type
  `data_humanSkin$data$NL` d'un workspace CellChat tutorial) ; les `.rds`
  (même contenant une liste) sont explorés comme les `.rda` ;
  « Exporter l'objet sélectionné (.rds) » sauvegarde n'importe quelle
  feuille en monofile `.rds` (remplace le workflow manuel
  `load()` → `CreateSeuratObject` → `saveRDS`) ; `rdata_flatten_env()`,
  `rdata_extract_path()`, `rdata_read_file_env()`, `rdata_export_paths()`.
- **Import Single-Cell** : Options B/C acceptent `.rda`/`.RData` ; objet
  unique compatible → auto-import via `prepare_seurat_object()` ; workspace
  multi-objets → carte de preview (choisir 1 objet à importer, ou en exporter
  plusieurs vers un .RData).
- **Import Bulk** : `.rda` accepté pour les slots comptages ET métadonnées
  (2 instances du composant, libellés « Utiliser comme matrice de
  comptages » / « Utiliser comme métadonnées » ; transposition gérée) ;
  un même workspace peut alimenter les deux slots.
- **Référence spatiale** (`read_reference_scrna`) : `.rda` objet unique
  (Seurat / liste counts+meta / matrice) ; multi-objets → erreur orientante.
- **Communication / Vélocité** : `parse_cellchat_object()` et
  `read_velocity_rds()` acceptent un workspace `.rda` à objet unique
  (multi-objets → erreur orientante vers l'aperçu SC).
- **Snapshot de session** (app.R) : `.rda` accepté (objet unique = snapshot).
- **Config** : `TS_IMPORT_RDA_EXTENSIONS`, `TS_IMPORT_RDA_WARN_MB` (500 —
  avertissement mémoire, jamais un blocage). **i18n** : 24 clés FR/EN.
- **Tests** : `test-core-rdata.R`, `test-rdata-contract-freeze.R`,
  `test-rdata-picker-module.R` (testServer), `test-rda-spatial-bulk.R`,
  `test-rda-comm-velocity.R`.

## [V1.1.0-rc] — 2026-09-05 (vague UX/UI, mandat utilisateur — candidat pré-release)

Refonte UX/UI fondée sur l'audit utilisateur (6 frictions, see
`docs/proposals/V1X_UX_REFACTOR_PROPOSAL.md`) : **zéro changement de
comportement scientifique**, contrats figés intouchés, IDs de modules
inchangés, migration par lots reversibles (un lot = un commit).

### Ajouté
- **Paradigme pipeline mutualisé** : le pipeline auto (1 clic) est le
  **premier panneau de l'accordéon (0.)** dans les trois domaines (SC, Bulk,
  Spatial), avec badge de paradigme en tête de chaque sidebar (« auto dispo » /
  « mode guidé » / « auto async »).
- **Bulk** : étape « 1. Pipeline Bulk — Contrôle qualité & filtrage » ;
  vue « Résumé Pipeline Bulk » (champs `shared_rv` existants uniquement,
  aucun nouveau calcul) ; dernier panneau = « 4. Livrables — Rapport & Script R ».
- **Spatial** : conteneur standard `layout_sidebar` — étapes numérotées en
  accordéon à gauche (une étape ouverte à la fois), résultats à droite ;
  dataset + statut des daemons toujours visibles dans la sidebar ; le module
  pipeline est scindé (contrôles dans l'accordéon, résumé dans le navset
  droit, même namespace → serveur inchangé).
- **Import** : rappel « mapping des IDs » + bouton natif « Aller au mapping
  des IDs » dans Import Single-Cell/Bulk/GEO (saut via `page_navbar(id="main_nav")`
  + `accordion_panel_open` ; aucun panneau déplacé, aucune UI dupliquée).
- **GEO** : renommé « Source publique (GEO) » (source de données, pas une
  4e modalité), phrase d'orientation ; placé en DERNIER du menu Import
  (préférence utilisateur du 2026-09-05).
- **i18n** : 36 clés FR/EN (glossaire double libellé : Pseudobulk, Vélocité
  ARN, Niches spatiales… + tooltips Moran/Milo/scCODA). Clés figées 8b→9b
  conservées (gate d'intégrité vert).
- **Test fonctionnel** `test-sc-auto-pipeline.R` : run complet de
  `run_sc_auto_pipeline` sur fixture minima (QC → PCA → clustering → UMAP →
  t-SNE → marqueurs → trajectoire → commit) + chemin d'échec gracieux.

### Modifié
- **SC** : accordéon plat (17 panneaux) regroupé en 5 sections parent
  (Préparation / Analyse / Dynamique / Abondance cellulaire / Livrables) ;
  les 4 panneaux DA 8c–8f sont nidifiés en un panneau « Abondance
  différentielle » à onglets internes (A. design — B. Milo/scCODA —
  C. vues croisées ; gating inchangé). Valeurs de panneaux préservées.

### Corrigé
- **Crash Spatial** : le binding accordéon bslib retourne un **vecteur de tous
  les panneaux ouverts** (`multiple=TRUE` par défaut) — l'observer de sync
  crashait par indexation récursive dès l'ouverture d'un 2e panneau.
  Correctif : `multiple = FALSE` (une étape à la fois, comme l'ancien navset
  horizontal) + défense `tail(1)`.
- **Gate G4** (`scripts/verify_release_gates.R`) : faux positif permanent
  depuis le Stage 19 (le script se matchait lui-même sur ses propres regex de
  chemins locaux) ; exclusion de soi via pathspec git.

### Vérification (pré-release)
- Suite complète : **1873 assertions PASS / 0 FAIL / 0 ERROR / 0 SKIP**
  (référence V1.0 : 1858 + 15 du nouveau test fonctionnel).
- Duplication : 0 erreur / 3 warnings pré-existants. Boot headless HTTP 200.
- Gates packaging : 6 PASS / 1 WARN / 0 FAIL. e2e shinytest2 : 13 PASS.

## [Post-V1.0] — 2026-09-04 (polish mandat utilisateur — revue des manquants)

Revue des fonctionnalités manquantes demandée après la V1.0 : les rapports ne
restitution pas les domaines ajoutés depuis leur création.

### Rapport Rmd Single-Cell (panneau 9)
- **Nouvelles sections** (tables pures, restituées du résultat canonique tel
  quel — aucune re-exécution, aucun recalcul de figure) : **Vitesse ARN**
  (statut, dimensions alignées, analysis_id), **Communication cellulaire**
  (méthode source, compteurs d'import, table canonique en extrait), **DA**
  (design expérimental + éligibilités, Milo — voisinages avec disclaimer
  « niveau voisinage », scCODA — effets crédibles avec disclaimer « pas des
  p-values »). Opt-in via les cases « Sections » du panneau 9 (aucun changement
  des rapports par défaut) ; paramètres canoniques exposés depuis
  `shared_rv` (expositions Stage 17 + pseudobulk).

### Rapport consolidé 4F (panneau 9b)
- **Domaines ajoutés au contrat (9 → 11)** : `pseudobulk` (exposition additive
  `shared_rv$pseudobulk_result` du panneau 4b — moteur, contraste, table DE)
  et `correlation` (gène cible + table de corrélations, déjà dans l'état
  partagé). Bundle : `tables/correlation.csv`, `tables/pseudobulk_de.csv`.
  Contrat + test de freeze + tests fonctionnels mis à jour simultanément.
- i18n : 10 clés FR/EN ajoutées (libellés de sections + disclaimers
  scientifiques).

### Restants (parking V1.x, décision requise)
- Rapports Rmd Bulk/Spatial : périmètre déjà complet par domaine (contrôlé).
- Compilation 4F des états Bulk/Spatial : proposition (extension contrat).
- 4D-3 / 4E-4 : inchangés (voir `UPGRADE_AND_COMPATIBILITY.md` §3).

## [V1.0.0] — 2026-09-04 (Stage 20)

Déclaration V1.0 : toutes les exigences de la roadmap vérifiées et prouvées
(`docs/release/V1_0_DECLARATION.md`) — contrats figés, provenance active,
rapports compilés, exports traçables, limitations documentées, gates RC passées.

### Vérification finale
- Suite complète : **1837 PASS / 0 FAIL / 0 ERROR / 0 SKIP / 16 warnings bénins**.
- Gates packaging : 6 PASS / 1 WARN documenté / 0 FAIL ; duplication 0 erreur ;
  lancement headless HTTP 200.
- Tags : `v1.0.0-rc.1` → `v1.0.0` (locaux ; push après confirmation).

## [V1.0.0-rc.1] — 2026-09-04 (Release Candidate, Stage 19)

Première version formelle candidate à la V1.0. Macro-fonctionnalités livres :
plateforme multi-omique 3 domaines (Single-Cell, Bulk RNA, Spatial), vitesse ARN
(3B), communication cellulaire (4D), abondance différentielle (4E), rapport
consolidé + reproductibilité (4F), durcissement release.

### Ajouts — Analyse Single-Cell
- **Vitesse ARN (Stages 8–10)** : import mtx/rds validé, 9 états de validité,
  empreinte d'objet v2, contrat résultat figé (`VELOCITY_RESULT_CONTRACT.md`),
  visualisations consommatrices pures + exports PNG/PDF/CSV.
- **Communication cellulaire (Stages 11–12)** : import CellChat/CellPhoneDB
  (schémas stricts, jamais de devinette), table canonique 12 champs,
  harmonisation d'identités par correspondance EXACTE, QC d'import, vues
  exploratoires (dotplot, heatmap pathways, réseau circulaire ggplot2 pur),
  centralité descriptive, filtres avec provenance. Génération depuis données
  brutes = proposition 4D-3 (parking).
- **Abondance différentielle (Stages 13–16)** : validation de design
  expérimental bloquant la pseudoreplication (les cellules ne sont pas des
  réplicas biologiques) ; Milo 4E-1 (voisinages, seed appliquée) ; scCODA 4E-2
  (composition par échantillon, environnement Python explicite, diagnostic de
  convergence en pur R) ; vues croisées 4E-3 descriptives (7 catégories de
  concordance figées, aucune p-value de consensus).
- **Rapport consolidé 4F (Stage 17)** : compilateur d'état canonique + de
  provenance (aucune ré-exécution) — collecteur, validateur à 7 états (les
  sections sans provenance sont refusées), rendu HTML autonome sans pandoc,
  bundle d'export projet (manifeste, tables fidèles, script R reproductible,
  session info).

### Ajouts — Socle & release
- **Durcissement (Stage 18)** : matrice 14 catégories × domaines
  (`docs/release/HARDENING_MATRIX.md`), tests de stress (rapport à 1200 entrées
  de provenance), baseline performance synthétique, limitations connues
  documentées.
- **Packaging RC (Stage 19)** : `scripts/verify_release_gates.R` (lock valide,
  aucune dépendance obligatoire non verrouillée, i18n sans doublon, aucun
  chemin local/credential dans les fichiers suivis) ; renv.lock complété des
  dépendances obligatoires manquantes **shiny.i18n 0.3.0, shinyWidgets 0.9.1,
  shinycssloaders 1.1.0** (419 packages au total) ; test de non-régression
  i18n (un doublon de clé FR fait crasher le démarrage).

### Corrections
- Onglet « 3. Visualisation » : le Bloc 3 « Dynamique & Écosystème » passe de
  « à venir » à « disponible » (panneaux 8 → 9b livrés) — la carte annonçait
  des fonctionnalités existantes comme futures.
- Clé i18n dupliquée « Verdict » (crash au démarrage via shiny.i18n) —
  détectée par le gate de lancement headless, corrigée avant commit.

### Sécurité / données
- Aucune donnée brute embarquée dans les rapports par défaut ; exports =
  résumés et tables de résultats uniquement ; noms de fichiers sources
  originaux seulement (jamais de chemins locaux) dans la provenance.

## [Versions antérieures — non étiquetées]
- Chrysalis 2A–2F : socle `R/core/` (state, validation, provenance, jobs,
  caching, io/pathway helpers) avec tests ; gate de duplication.
- Phases 1–7 historiques : import SC/Bulk/Spatial, pipeline SC (QC, HVG,
  PCA/UMAP/t-SNE, clustering, sketch, BPCells), marqueurs, corrélations,
  pathways, trajectoire, annotation SingleR, pseudobulk, rapports Rmd par
  domaine, spatial (QC, clustering, déconvolution RCTD/STdeconvolve, niches,
  Moran, multi-échantillons).

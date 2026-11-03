# N2 `cerberus-inspector` — l'inspecteur qui rend les journeys reproductibles

Satellite **sans code** (`FORMAT.md` contrat figé v1.0.0 + ce README — **zéro
`.R`, zéro dépendance, zéro app change**) qui donne à l'app une couche
d'**inspection navigateur** : des **journeys** (séquences de steps ancées sur
le DOM réel) exécutées sur une version donnée de l'app, des **screenshots**
keyés par version, et des **reports** dont les verdicts sont **comparables
d'une version à l'autre**.

**L'exécution n'est PAS du code commité.** Le driver est la capability externe
`control-browser` (fiche §2-S4 : *« the driver is the external control-browser
capability (NOT committed code) »*). Ce que le dépôt porte, et qui ne peut plus
se perdre : **le contrat** (FORMAT.md) et **les reports** (texte commité
référençant des PNG gitignorés).

---

## Objet et rattachement — le troisième membre de la famille satellites

- **S1 `watchtower`** (`tools/satellites/watchtower/`) : la vue **humaine et
  passive** de l'état du canal drive — elle *regarde*.
- **S5 `doc-prover`** (`tools/satellites/doc_prover/`) : la preuve headless que
  la **documentation** du canal dit encore la vérité — elle *vérifie*.
- **N2 (ce satellite)** : le membre qui **pousse** la session et **documente le
  rendu** — fiche §2 contraintes : *« they read the drive channel read-only
  (watchtower, doc-prover) or **drive it through the existing protocol**
  (inspector) »*. Source : fiche de lancement
  `docs/proposals/PROPOSAL_N_SATELLITES_LAUNCH_ROADMAP_2026-10-26.md`, §1
  ligne N2, §2 slice S4 ; brief round8 l.124 (variante S4 FORMAT-first).
- L'objet mesurable : *« l'inspecteur rend les journeys **reproductibles** et
  les verdicts **comparables** d'une version à l'autre »* — reproductible =
  mêmes steps, mêmes ancres, mêmes slugs ; comparable = `QC/inspector/<git
  describe>/` par version + verdicts au vocabulaire figé (FORMAT §3), mode
  diff humain (FORMAT §5). Deux journeys canoniques livrées : **J1 « G-chain
  bulk »** (import → Step 1 → DE → pathway) et **J2 « QC panels »** — FORMAT
  §7-§8.

## Note de relève — le driver disparu, la preuve qui a survécu

- **Dérive c du brief round8** (`docs/agent/swarm/round8/NEXT_SESSION_BRIEF_2026-11-03.md:23`) :
  `tools/cdp_e2e/` **ABSENT du disque** (`ls` : « No such file ») — le runner
  e2e CDP qui drivait l'app a disparu, cité par la garde STATUS e2e CDP et
  AGENTS.
- **La preuve antérieure a survécu parce qu'elle était gitignorée** :
  `QC/cdp_run1.log`–`QC/cdp_run3.log` (cités par le brief) — **mesuré
  2026-11-04 : cinq logs présents**, `QC/cdp_run1.log` à `QC/cdp_run5.log`
  (datés 2026-10-12). `QC/cdp_run1.log` : boot app sur `127.0.0.1:8127
  (test.mode=TRUE)`, bilan final **« 8 PASS / 0 SKIP / 0 FAIL »**, artifact
  pointant `tools\cdp_e2e\e2e_results.json` — la trace d'un driver dont le
  code n'existe plus.
- **Leçon assumée et corrigée par le format** : ce qui se perd, c'est le code ;
  ce qui tient, c'est le contrat et les données gitignorées horodatées.
  L'inspector se positionne comme le **successeur maintenu** (fiche §2-S4
  point 4 : *« the inspector's format doc cites QC/cdp_run*.log as prior
  evidence and positions itself as the maintained successor »*) : le FORMAT
  est commité et figé ; le driver est une capability externe (rien à perdre) ;
  les PNG vivent sous `QC/` gitigné, keyés par `git describe` — la même
  longévité que les logs cdp qui ont survécu à leur runner.

## Note de cécité de garde (assumée)

La conventions gate (exigence « 0 erreur ») voit `app.R`, `global.R`, `R/`,
`modules/`, `config/` — **pas `tools/`** (`tools/check_conventions.R:406` :
`scan_roots <- c("app.R", "global.R", "R", "modules", "config")` — mesuré au
jalon S1, re-mesuré 2026-11-04). Le satellite échappe donc **par construction**
au gate, comme toute la famille `tools/satellites/` (fiche §2 *context* :
*« satellite R code under tools/satellites/ escapes it by construction »*).

Ici la cécité est **totale et vide de risque** : le satellite ne contient
**aucun code du tout** (aucun `.R`, aucun JS — seulement deux fichiers
`.md` de contrat). Rien à gating, rien à tester testthat, rien à sourcer :
`app.R`, `R/.sources_manifest.txt` et `i18n/` restent intouchés (0 app
change), aucune dépendance ajoutée. La contrepartie assumée : la validation
tient à la **falsification documentaire** ci-dessous (ancres mesurées,
vérifiables par lecture/grep) et à la première exécution réelle — pas à un
gate automatique. `check_writers` n'est pas concerné : aucun writer déclaré,
aucune écriture par le satellite lui-même.

## Instruction de service EXACTE — exécuter une journey

Depuis la **racine du dépôt** (`SHINYAPP test/`), locale R conforme à la fiche
(`LC_ALL` figé, ex. `English_United States.utf8` pour les runs R ; l'UI rend
ses libellés français via i18n) :

1. **Armer la session** — le seul démarrage supporté (spéc §4, G4 #16) :

   ```bash
   Rscript tools/launch_dev_drive.R            # headless (défaut)
   Rscript tools/launch_dev_drive.R --visible  # fenêtre visible (gestes UI)
   ```

   Attendre un `tools/_drive/ready.json` **frais** (`hb_at` ≤ `hb_timeout_s`
   = 15 s, `hb_n` qui avance entre deux lectures espacées ~4 s — runbook
   `docs/mcp/DRIVE_SESSION_RUNBOOK.md` § 2, commandes 1-2). Session absente
   ou périmée = **pas de session inventée** : les steps drive rendront
   `UNTESTED` (motif obligatoire), jamais un PASS forcé.

2. **Piloter** — la capability externe `control-browser` exécute les steps de
   la journey canonique (FORMAT §7-§8) : gestes UI (`nav`/`click`/`fill`) sur
   les rôles+nommes accessibles cités, et/ou scénarios drive conformes
   (`tools/_drive/scenario.json`, un `seq` par action, verdict lu dans
   `result.json` — **jamais stdout**, `tools/_drive/README.md`). Jobs longs :
   `result.json` portant `running` = **en vol, on continue de sonder par
   `snapshot` frais** (leçon 537 s / 852,7 s), jamais un échec.

3. **Capturer** — après chaque step, screenshot vers :

   ```
   QC/inspector/$(git describe --tags)/<screenshot_slug>.png
   ```

   (re-mesurer `git describe --tags` à la session — mesuré 2026-11-04 :
   `v1.1.0-20-g4bb23af` ; `QC/` gitigné entier, `.gitignore:74`).

4. **Rédiger le REPORT** — schéma FORMAT §2 (`meta`/`steps`/`bilan`),
   verdicts au vocabulaire figé FORMAT §3 : `state_verdict` ∈ PASS/FAIL/
   UNTESTED **mesuré** (motif prover ✅/❌/UNTESTED — un UNTESTED porte son
   motif dans `notes`), `visual_verdict` = jugement **humain** libre-texte
   (**pas d'auto-fail pixel** — fiche §2-S4). Commiter le **report seul** :
   les PNG restent sous `QC/`, le report les référence par chemin
   (FORMAT §4).

5. **Dé-armer** en supprimant `tools/_drive/arm.json` (le contrat : disarm
   ne supprime pas `ready.json`).

## Falsification — ce que le contrat permet de prouver sans exécuter

1. **Un report forgé se trahit par ses chemins** : chaque
   `screenshot_path` référencé doit exister sous `QC/inspector/<app_version>/`
   au moment de la revue (`ls` local) — un `state_verdict: "PASS"` sans PNG
   correspondant (ou avec un `app_version` ≠ `git describe` de la capture) est
   une **falsification détectable**, pas un verdict.
2. **Un UNTESTED sans motif est un bug** (motif doc-prover) : toute ligne
   `UNTESTED` doit porter sa raison dans `notes` (session non armée,
   prérequis J1 absent, capability indisponible). Un PASS/FAIL non adossé à
   une mesure (ancre trouvée, verdict drive terminal cité verbatim) pareil.
3. **Une ancre qui dérive est le livrable attendu** : renommer un libellé
   (ex. « Lancer Filtrage & VST », mod_bulk_filter.R:59) puis exécuter le
   step → `state_verdict: FAIL` avec l'ancre attendue vs le DOM réel — c'est
   exactement le drift que l'inspector existe pour détecter, **pas** une panne
   du format. Les ancres du FORMAT citent source+ligne mesurée : chaque
   référence est re-vérifiable par lecture (`grep -n` sur le module cité).
4. **La reproductibilité se prouve en rejouant** : deux exécutions de J1 sur
   deux versions produisent `QC/inspector/<desc-A>/…` et
   `QC/inspector/<desc-B>/…` avec **les mêmes slugs** step par step — c'est ce
   qui rend le mode diff humain (FORMAT §5) possible. Deux exécutions sur la
   MÊME version doivent donner deux REPORTS aux mêmes ancres (la capture peut
   différer pixel-wise ; l'état, non).
5. **Les commandes drive sont vérifiables hors session** : chaque forme drive
   des journeys cite module + token de l'allowlist gelée
   (`tools/pkg_skeleton/R/drive_allowlist.R` — ex. `bulk-filter-run_filter_norm`,
   mesuré live par le probe DOM G3c, commentaire de l'allowlist) et la
   séquence mesurée du runbook drive (`tools/_drive/README.md` : Step 1
   `done` 1,1 s ; `bulk_de` `done` 2,1 s, contraste observable 852,7 s plus
   tard). Un token absent de l'allowlist dans un step = bug de format.

## Limites (assumées)

- **Verdicts humains v1, pas d'auto-fail pixel** (fiche §2-S4) : le
  `visual_verdict` est libre-texte ; aucun seuil de diff, aucun scoring — le
  mode diff est un aide à la revue, jamais un juge.
- **Les ancres sont les libellés français rendus** (i18n) : un changement de
  libellé casse l'ancre — **voulu** (c'est le drift à détecter) ; la notation
  porte aussi l'id DOM mesuré pour lever l'ambiguïté.
- **PIDs/ports/valeurs chiffrées citées = photographies datées** (8127, 1,1 s,
  2,1 s, 852,7 s, `v1.1.0-20-g4bb23af`…) : elles prouvent que le chemin a été
  mesuré, pas l'état courant — re-mesurer à chaque session.
- **Les journeys supposent une fixture de drop `cerberus-exchange/v1` valide**
  (manifest.json + matrix.csv counts bruts + samplesheet.csv) dans le
  répertoire datasets — construite **hors dépôt** (motif jalon 2,
  `_jalon2_falsif/datasets/`) ; sans elle, les steps 2-6 de J1 sont UNTESTED
  (motif obligatoire).
- **Pas de premier report réel dans cette slice** : S4 livre le contrat + les
  2 journeys + le README ; le « first real report » / « first cross-version
  diff report » (fiche §2-S4 l.177, critère ligne 238) exige une session
  armée pilotée par la capability — geste opérateur, documenté en §
  Instruction de service.
- Le satellite **ne répare jamais** : un FAIL est un finding filé au report,
  le correctif est un jalon séparé (motif doc-prover).

# N6 `doc-prover` — le prouveur headless de la documentation drive

Satellite **R en ligne de commande** (`prover_lib.R` moteur pur + `run_prover.R`
runner + `claims.json` corpus) qui vérifie, **headless**, que le runbook
`docs/mcp/DRIVE_SESSION_RUNBOOK.md` dit encore la vérité sur le dépôt :
fichiers et outils cités existent, commits résolvent, ancres de lignes portent
la substance citée, contrats JSON documentés correspondent au contrat réel.

**Lecture-seul sur le dépôt, absolue.** Le runner ne source jamais l'app, ne
lance jamais l'app, n'écrit jamais dans `docs/` ni dans `R/` ni dans
`modules/`. La seule écriture de tout le satellite est l'optionnelle
réécriture de `arm.json` sous `--live` (drop-box `tools/_drive/*.json`,
**gitignorée**, `.gitignore:100` — donc `git status` reste propre).

---

## Objet et rattachement — O11/SW-5c, troisième consommateur du même canal

- **O11** (décision tranchée 2026-10-25, option A) a été **livrée en SW-5c M2
  (`fe47823`)** : l'outil agent-facing `transcripto_drive_state` —
  `docs/proposals/DRIVE_PASSIVE_STATE_PROPOSAL.md:3` et `:84`.
- **N1** (`tools/satellites/watchtower/`, slice S1 du même batch) est la vue
  **HUMAINE** du même canal ; **N6 (ce satellite) en est le vérificateur** :
  là où la watchtower *rend visible* l'état de `tools/_drive/`, le doc-prover
  *prouve* que la **documentation** de ce canal (le runbook) colle encore à
  l'état du dépôt. Source : fiche de lancement
  `docs/proposals/PROPOSAL_N_SATELLITES_LAUNCH_ROADMAP_2026-10-26.md`, slice S5
  N6 ; complément N1 (S1, même canal).

## Pourquoi ce satellite existe : il absorbe le rôle des deux outils PERDUS

Deux gardes documentaires ont **disparu** de `tools/` (confirmé par mesure le
2026-11-03 : les deux fichiers sont absents) :

- `tools/docs_hash_manifest.R` — le manifeste sha256 du corpus `docs/`
  (tamper-evidence de l'arbre documentaire, STATUS.md:433 le cite encore) ;
- `tools/scheduled_verify.R` — le substitut local au CI (méta-gate + garde
  statut → historique daté `docs/audits/gates_history/`, STATUS.md:433 idem).

**NOTÉ, PAS RECONSTRUIT** (consigne de la fiche, reprise par le brief round8
l.114 : « *the doc-prover absorbs their role (note it, do not rebuild blindly)
— the non-behavioral docs hash will be re-added later* »). Ce que N6 reprend
**aujourd'hui** : la fonction **sémantique** de vérification documentaire —
non pas hacher les fichiers (le hash sera ré-ajouté plus tard, mécanique), mais
**falsifier le contenu** : chaque assertion du runbook est confrontée à l'état
réel du dépôt et reçoit un verdict. Le corpus vivant est `claims.json`
(20 claims : 14 static / 6 live), extrait du runbook SEUL.

## Note de cécité de garde (assumée)

Le moteur et le runner sont du **code R sous `tools/satellites/`** : la
conventions gate (exigence « 0 erreur », `check_conventions.R`) voit `modules/`
et l'arbre du package — **pas `tools/`, et encore moins un satellite**
(`tools/` hors scan — mesuré au jalon S1 : `check_conventions.R:406`). Le
satellite échappe donc **par construction** au gate, comme toute la famille
`tools/satellites/` ; ce choix est **assumé et conforme à la fiche**. La
contrepartie assumée : la validation tient au **test testthat du moteur pur**
(`tests/testthat/test-doc-prover.R` — RED→GREEN mesurés, FAIL 0 / PASS 53) et à
la **falsification manuelle** ci-dessous, pas à un gate automatique. Le
satellite n'est **jamais sourcé par l'app** : `app.R`,
`R/.sources_manifest.txt` et `i18n/` restent intouchés (0 app change).

## Instruction de service EXACTE

Depuis la **racine du dépôt** (`SHINYAPP test/`) :

```bash
# 1. Statique seulement — headless, toujours sûr, ~1 s :
Rscript tools/satellites/doc_prover/run_prover.R

# 2. Avec claims LIVE — exige une session drive ARMÉE (ready.json frais +
#    hb_n qui avance), sinon les claims live rendent UNTESTED :
Rscript tools/satellites/doc_prover/run_prover.R --live
```

Sorties, écrites **à côté du runner** (jamais ailleurs) :
`tools/satellites/doc_prover/report.json` et `report.md` — statut par claim
(✅ / ❌ / UNTESTED) avec l'evidence mesurée, comptes, verdict global :
« runbook ✅ complet » ou « N findings de drift filés ». Code de sortie 0 dès
que le rapport est écrit (un finding de drift est un **livrable**, pas un
échec) ; 2 si `claims.json` est illisible.

Ce que `--live` fait de plus, et ce qu'il ne fait **pas** :

- il **sonde** `tools/_drive/ready.json` (2 lectures espacées ~4 s, commande 2
  du runbook), mesure l'âge du heartbeat et la vitalité du PID (commande 1) ;
- il **ré-arme** la session courante avec son propre token (§4.2) pour prouver
  que le re-arm conserve la session — écriture `arm.json`, drop-box gitignorée ;
- si le handshake est **périmé**, il interroge `transcripto_drive_status` via
  un subprocess `Rscript --no-init-file scripts/mcp_server.R` (timeout 180 s)
  pour prouver la réponse `STALE_SESSION` (§1) ;
- il ne relance **jamais** l'app lui-même (`launch_dev_drive.R` reste un geste
  opérateur) et ne fabrique **jamais** un état (job en vol, handshake périmé)
  qui ne serait pas observé — ces claims restent UNTESTED.

## Les claims (corpus `claims.json`)

Chaque claim = `{id, type static|live, doc, line, assertion, expected,
evidence_note}`. `line` est l'ancre **dans le runbook**. Un verdict :

- **✅ pass** — l'assertion est mesurée vraie sur le dépôt ;
- **❌ fail** — mesurée **fausse** = **FINDING DE DRIFT** : le prover le file
  dans le rapport, il ne répare **jamais** le runbook (le correctif documentaire
  est un jalon séparé) ;
- **UNTESTED** — non mesurable dans ce contexte (pas de `--live`, `jsonlite`
  absent, subprocess en échec, état non observé) — **jamais deviné**.

## Falsification — procédure

1. **Le moteur est pur et testé** : `tests/testthat/test-doc-prover.R` couvre
   les verdicts pass/fail/untested, les comptes du rapport, les claims
   malformés (→ untested, jamais une erreur), la pureté (double appel
   identique) et le classifieur d'état (fixtures watchtower, règle 🔴 :
   *JOB IN FLIGHT*, **jamais** « DEAD »).

   ```bash
   "D:/Data_science/R-4.4.2/bin/x64/Rscript.exe" -e "Sys.setlocale('LC_ALL','French_France.65001'); testthat::test_file('tests/testthat/test-doc-prover.R', stop_on_failure = FALSE)"
   ```

2. **Une ancre du runbook a dérivé ?** Modifier le runbook (ou déplacer une
   fonction) puis relancer le runner statique : le claim concerné passe ❌ et
   le finding apparaît dans le rapport avec l'ancre attendue vs la position
   réelle mesurée. Restaurer : le finding disparaît (le prover est
   reproductible — il ne se souvient de rien).

3. **Un claim est trop laxiste ?** `claims.json` est un corpus **déclaratif** :
   chaque assertion dit ce qu'elle attend (`expected`) ; le checker correspondant
   vit nommément dans `prover_lib.R` (`.prover_checkers()`, un checker par id).
   Un checker qui ne peut pas mesurer rend `untested` avec sa raison — un
   `untested` inventé est un bug, pas un état acceptable.

4. **Live** : avec une session armée, `--live` doit rendre pass sur
   `live-hb-advance` / `live-hb-age` / `live-launch-arms` ; dé-armer (supprimer
   `arm.json`) puis re-lancer : les mêmes claims tombent en untested (gate non
   satisfait), preuve que le prover ne fabrique rien. Le runner a été exécuté
   **sans** `--live` lors du jalon (session opérateur respectée) : les 6 claims
   live y sont UNTESTED **par construction**.

## Limites (assumées)

- Le prover vérifie le runbook **au moment de la mesure** : un `pass` est une
  photographie datée (horodatée dans le rapport), pas un contrat permanent.
- Les ancres de lignes sont fragiles par nature (tout edit du runbook les
  décale) — c'est **voulu** : c'est exactement ce que le prover doit détecter.
- `live-stale-tool` lance un subprocess MCP lourd (le serveur charge la pile
  d'exécution) ; en échec ou timeout → `untested` honnête, jamais un verdict
  forgé à partir d'une sortie partielle.
- Les mesures PowerShell du runbook (§2, sorties datées 2026-10-25) sont des
  **photographies datées** que le prover ne rejoue pas telles quelles : il
  mesure leurs **équivalents R** (lecture JSON, âge, vitalité PID).

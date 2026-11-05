# N3 `biomentat-dock` — l'hôte des satellites

Satellite **hôte** (R, CLI) : il connaît, par **manifeste figé**, comment
démarrer / sonder / arrêter chaque satellite du dépôt. Fiche de lancement :
`docs/proposals/PROPOSAL_N_SATELLITES_LAUNCH_ROADMAP_2026-10-26.md` §1 ligne N3,
§2 slice S7. **Contrat figé : `MANIFEST.md` v1.0.0** (schéma du manifeste, schéma
`capabilities/<name>.json`, verbes, exit codes — toute évolution = version
nouvelle, jamais silencieuse).

---

## Règle absolue : le dock ne démarre JAMAIS rien sans la commande opérateur

Pas de watcher, pas de daemon, pas de hook de boot : le dock est un CLI dont
chaque verbe est un geste posé. Sans verbe, `run_dock.R` affiche l'usage et ne
fait **rien**. L'app ne sait même pas que le dock existe — les satellites ne
sont jamais démarrés par l'application, et **un process par satellite** (O6
intact : `shares_app_process = false` verrouillé au validateur, un port par
satellite, jamais partagé).

## Fichiers

| Fichier | Rôle |
|---|---|
| `MANIFEST.md` | **Contrat figé v1.0.0** (schémas + verbes + règles) |
| `dock_manifest.json` | Le manifeste vivant — **1 locataire : `watchtower`** (dogfood, fiche §2-S7) |
| `capabilities/watchtower.json` | Le capability du locataire (shape alignée plugin-sdk, read-only v1) |
| `dock_lib.R` | Lib **pure** (charge/valide manifeste, verdict santé, pid-file) — testée |
| `run_dock.R` | CLI (effets) : `--start` / `--health` / `--status` / `--stop` |
| état runtime | `tools/_drive/satellites/` (pid + log, **gitigné** — jamais commité) |

## Service (verbes)

```bash
Rscript tools/satellites/dock/run_dock.R --start  watchtower   # démarre + attend la santé
Rscript tools/satellites/dock/run_dock.R --health watchtower   # OK/FAIL/TIMEOUT
Rscript tools/satellites/dock/run_dock.R --status              # tableau (ne démarre RIEN)
Rscript tools/satellites/dock/run_dock.R --stop   watchtower   # kill de l'arbre + retrait pid
```

Exit codes : `0` succès · `1` santé FAIL/TIMEOUT · `2` refus (manifeste
invalide, satellite inconnu, `enabled=false`, déjà démarré, verbe inconnu).
`--start` refuse un doublon de process (pid vivant ou port déjà servi) — jamais
un deuxième http.server sur le même port.

## Note de cécité de garde (assumée — modèle S1/S5)

Le R du dock vit sous `tools/` : la conventions gate (exigence « 0 erreur »)
voit `modules/` et l'arbre du package — **pas** `tools/`. Le satellite échappe
donc **par construction** au gate. Contrepartie assumée : R minimal (une lib
pure + un CLI fin, ~0 logique métier), et **la porte de validation est
`tests/testthat/test-biomentat-dock.R`** (32 assertions : validateur du format
figé avec 8 corruptions refusées, matrice de verdict santé, cohérence disque du
locataire, verrou O6). Dépendances : **aucune nouvelle** — `processx` + `curl`
sont déjà verrouillés (renv.lock, infra de test / http).

## Pourquoi watchtower premier locataire (dogfood)

Le premier process géré par le dock est le satellite le plus simple et le plus
documenté : N1 `drive-watchtower` (S1, `c992dfe`), dont l'instruction de service
EXACTE (`python -m http.server 4892` à la racine — page et JSON drive en
same-origin) devient la `command` du manifeste. Si le dock sait démarrer,
sonder et arrêter watchtower proprement, le format est prouvé ; les satellites
suivants (inspector, doc-prover) s'ajoutent comme des lignes de manifeste +
un capability, **sans changer le dock**.

## Falsification (procédure)

1. `--start watchtower` → exit 0, message « DÉMARRÉ (pid …) » + santé OK.
2. `curl -s -o /dev/null -w "%{http_code}" http://127.0.0.1:4892/tools/satellites/watchtower/index.html` → `200`
   (page servie) ; idem sur `tools/_drive/ready.json` → `200` (same-origin OK).
3. `--status` → `running=oui`, `santé=OK`.
4. `--stop watchtower` → exit 0, « ARRÊTÉ », port libéré.
5. `--health watchtower` → FAIL (connexion refusée) — preuve de l'arrêt réel.
6. `--start` refus honnête : satellite inconnu → exit 2 ; `enabled=false` → exit 2.

## Limites (honnêtes)

- v1 = **un satellite par verbe** ; pas de démarrage de groupe (jamais demandé).
- Le pid-file est le seul marqueur de gestion : un process démarré À LA MAIN
  (hors dock) sur le même port fait refuser `--start` (santé déjà OK) — assumé,
  c'est la garde anti-doublon.
- Windows-first (host de développement) : `processx` gère le kill d'arbre ;
  `--stop` vérifie la libération réelle du port avant d'annoncer succès.
- La sonde HTTP v1 = status code uniquement (pas de contenu) — le verdict
  détaillé (DOM, JSON drive frais) reste le travail de watchtower/doc-prover.

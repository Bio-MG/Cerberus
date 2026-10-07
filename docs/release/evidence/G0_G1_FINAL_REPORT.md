# Rapport final G0/G1 — Protocole de contrôle live `ts-drive/1`

**Date** : 2026-09-21 · **HEAD** : `8b7e1bf` · **Arbre** : propre (`git status --porcelain` vide)

## Formulation du jalon (verbatim)

> G0/G1 live-control protocol validated on a real Shiny session. Real bulk execution on a preloaded object is not yet validated and remains a G2/G3 acceptance.

## 1. Commits

| SHA | Objet |
|---|---|
| `64f8a81` | `drive: pin the result-status enum and the six badge-visibility gates` — `R/core/drive_watcher.R` (+25 l.) et `tests/testthat/test-drive-watcher.R` (+299 l.) |
| `4a115c0` | `docs(drive): version the acceptance scope, the G2/G3 gap and terminality` — `tools/_drive/README.md` (+57/−3) |
| `8b7e1bf` | `fix(conventions): C9b ceiling pinned 37 while the measurement said 39` — test seul (+10/−3) |

**La garde n'a PAS été modifiée** (contrainte n° 7) : `tools/check_conventions.R`
est intact, et rend `0 erreur(s), 59 avertissement(s)` — identique à l'avant.

## 2. Commandes exactes

```bash
# Suite complète (autoritative, JAMAIS run_tests.R sans argument)
D:/Data_science/R-4.4.2/bin/Rscript.exe tools/run_full_suite.R

# Garde de conventions, avec la liste des sites
D:/Data_science/R-4.4.2/bin/Rscript.exe tools/check_conventions.R --list-all

# Suite drive isolée
D:/Data_science/R-4.4.2/bin/Rscript.exe -e 'Sys.setlocale("LC_CTYPE","fr_FR.UTF-8");
  library(testthat); library(shiny);
  res <- test_file("tests/testthat/test-drive-watcher.R", reporter="silent")'
```

## 3. Résultats

### Suite complète — **137 / 137 fichiers**

| Mesure | fail | pass | err | skip |
|---|---|---|---|---|
| **Somme des lignes de `full_suite_results.txt`** | **0** | **7350** | **0** | **1** |
| Ligne `BILAN` **finale** (`23:51:06`, run terminé) | 0 | 7350 | 0 | 1 |

Les deux **concordent**. Le seul `SKIP` = `test-mod-geo.R` (smoke GEO live),
conforme à `STATUS.md` §2ba.

⚠️ **Piège rencontré** : un `BILAN` lu à `23:32:09` affichait `failed=1 passed=7349`
— c'était un **rendu intermédiaire d'un run encore en cours** (la ligne n'est
écrite qu'au `close(con)`, et le processus a vécu jusqu'à `23:51:06`).
J'avais **déjà** conclu, par re-calcul sur les lignes, `fail=0 pass=7350`.
Le run **terminé** confirme ce re-calcul. Voir §7.

### Suite drive

| Fichier | fail | pass |
|---|---|---|
| `test-drive-allowlist.R` | 0 | 212 |
| `test-drive-watcher.R` | 0 | 345 |
| **Total drive** | **0** | **557** |

`0 FAIL / 0 ERROR / 0 SKIP`. Avant ce jalon : 487. **Δ = +70 assertions.**

### Gardes de conventions

```
test-conventions-c9b-owned.R     fail=0 pass=42   (était fail=1 pass=41)
test-conventions-c9-domain-alias.R fail=0 pass=8
test-conventions-c10-scope.R     fail=0 pass=7
Garde : 0 erreur(s), 59 avertissement(s)
```

## 4. Table d'acceptation — session Shiny VIVANTE

| # | Point | Statut |
|---|---|---|
| 1 | Handshake `ready.json` (app → agent) | ✅ |
| 2 | `arm.json` / disarm | ✅ |
| 3 | Badge passif (observational, aucun effet de bord) | ✅ |
| 4 | Fraîcheur du heartbeat (`hb_at`, `hb_n`, `hb_timeout_s`) | ✅ |
| 5 | `noop` | ✅ |
| 6 | `snapshot` | ✅ |
| 7 | `set_inputs` | ✅ |
| 8 | Gestion des séquences (`ack_seq == seq`) | ✅ |
| 9 | Invalidation de session | ✅ |
| 10 | Rejet de token périmé | ✅ |

### Portes de visibilité du badge — 6 tests durables (section 15)

| Porte | Situation | Statut |
|---|---|---|
| GATE 1 | feature drive désactivée | ✅ |
| GATE 2 | pas de `arm.json` valide | ✅ |
| GATE 3 / 3b | mauvais token / token périmé | ✅ |
| GATE 4 | heartbeat / session périmé | ✅ |
| GATE 5 | session désarmée | ✅ |
| GATE 6 | fin de session | ✅ |

## 5. Rotation de token — mesurée en live

| Scénario | Token | Verdict |
|---|---|---|
| Rechargement (reload) | `7h88ewiz` → `v4z732tt` | **rotation** |
| Navigateur séparé | `u0jrebu4` (nouveau `started_at`) | **rotation** |
| Deux onglets, un script | `tok1 == tok2` | **artefact de harnais** — `ChromoteSession$new()` réutilise une cible |

Le protocole **est** correct (« dernière session connectée gagne ») ; c'est le
**harnais** qui était aveugle. Test durable ajouté : « a SECOND session mints a
new token; the previous one is replaced » + « the PREVIOUS tab's token can
neither arm nor display ».

## 6. Terminalité des statuts (`ts-drive/1`)

Énumération **GELÉE** (`TS_DRIVE_STATUSES`, `R/core/drive_watcher.R`) :

```
ignored | invalid | applied | running | done | error
```

| Statut | Nature |
|---|---|
| `applied` | **acquittement** — entrées mises à jour, pipeline NON fini |
| `running` | job démarré — **non terminal** |
| `done` / `error` | **TERMINAUX** |
| `ignored` / `invalid` | refus terminaux |

Prédicat `ts_drive_status_terminal()` ajouté. **4 falsifications**, toutes
restaurées byte-identiquement : `selected` ignoré ⇒ 2 rouges ; `armed=false`
ignoré ⇒ 10 ; `applied` traité comme terminal ⇒ 2 ; token constant ⇒ 5.

## 7. ⚠️ Point d'honnêteté — un `BILAN` lu **en cours de run** est un instantané partiel

À mi-parcours, la ligne `BILAN` affichait `failed=1 passed=7349 | 23:32:09`.
Le run a en réalité vécu jusqu'à **`23:51:06`** et rendu :
`BILAN: failed=0 passed=7350 error=0 skipped=1`.

⇒ Le `failed=1` était le **rendu d'un run non terminé** (la ligne `BILAN` n'est
écrite qu'à la fin du `for`, et un enfant peut réécrire/flusher entretemps) —
**pas** une corruption du fichier ni un vrai rouge. La somme des lignes du
fichier, elle, valait **déjà** `fail=0 pass=7350` et **n'a jamais changé**.

**Méthode qui a tranché, et qui reste la règle** :
① recomper **la somme des lignes** (découpage sur **CR**, pas LF) ;
② vérifier **nombre de fichiers enregistrés == nombre déclaré** (`start … | N files`) ;
③ chercher `fail=[1-9]` **dans les octets** ;
④ **ne lire le `BILAN` qu'une fois le processus TERMINÉ** (croissance des sorties
arrêtée, pendant ≥ 30 s). Un `EXIT=139` est **normal même en succès**.

Le fichier de résultats fait foi ; la ligne `BILAN` ne fait foi qu'**à la fin**.

## 8. Périmètre de documentation (contrainte n° 6)

| Élément | Statut |
|---|---|
| `tools/_drive/README.md` | **VERSIONNÉ** (seule doc drive versionnée) |
| `R/core/drive_watcher.R`, `test-drive-watcher.R`, `test-drive-allowlist.R` | **VERSIONNÉS** |
| `docs/STATUS.md` §2di | **LOCAL ONLY** — `docs/` est gitignoré (`.gitignore:36`) |

⇒ `docs/STATUS.md` §2di n'est **pas** reproductible depuis le dépôt. Les
résultats de cette section sont reproductibles via `tools/_drive/README.md`
et les tests versionnés **uniquement**.

## 9. En attente — G2 / G3

- **`run_pipeline` → `invalid`** : **rejet de protocole validé**, PAS une
  exécution bulk. **L'exécution bulk réelle sur un objet chargé n'est PAS
  validée** et reste une **acceptation G2/G3**.
- Dépend de : liaison des boutons G2/G3, et du travail
  `import_file` / préservation des données.
- **G3 n'a PAS été démarré** (contrainte n° 8).

## 10. Leçon transversale du jalon

Le plafond C9b `37` **était faux dès sa pose** : le commit `a53dc82` — celui qui
l'a écrit — mesure **39**, liste **byte-identique** à celle d'aujourd'hui ⇒
**Δ 0** vs mon changement. Le test était rouge **depuis sa naissance**, invisible
parce que la suite était lue ailleurs.

🔑 **Un compteur de dette se MESURE sur une ligne de base EXTRAITE, jamais ne
se déduit.**

# Rapport doc-prover — docs/mcp/DRIVE_SESSION_RUNBOOK.md

Généré : 2026-11-03T22:52:31Z · --live : oui · moteur : prover_lib (base R)

**Verdict global : 7 findings de drift filés**

Comptes : ✅ pass = 11 · ❌ fail = 7 · UNTESTED = 2 — total 20 claims (14 static / 6 live)

## ❌ Findings de drift (filés, pas réparés par ce jalon)

### static-anchor-mcp-banner — docs/mcp/DRIVE_SESSION_RUNBOOK.md l.19
- **Assertion (runbook)** : §1 : l'ancre « scripts/mcp_server.R:104-106 » nomme les trois outils read-only cités
- **Attendu** : les 3 noms présents dans la fenêtre de lignes 104-106
- **Mesuré** : fenêtre 104-106 mesurée :   .ts_stderr("  tools        : transcripto_drive_status, transcripto_drive_read_result (read-only)") |   .ts_stderr("                 transcripto_drive_state (M2: passive, read-only)") |   .ts_stderr("                 transcripto_drive_datasets (P2 PoC datasets: passive, read-only, session-free)") — « transcripto_drive_snapshot » ABSENT de la fenêtre citée (première occurrence réelle : ligne 107) — ancre dérivée (drift)

### static-anchor-stale-code — docs/mcp/DRIVE_SESSION_RUNBOOK.md l.21
- **Assertion (runbook)** : §1 : « STALE_SESSION » est un code de domaine porté par « scripts/mcp_server.R:219 » (cité aussi l.38)
- **Attendu** : la ligne 219 contient STALE_SESSION
- **Mesuré** : scripts/mcp_server.R:219 ne porte PAS le code ; mesuré : «        structuredContent = structured, isError = FALSE) ». Le code existe ailleurs (occurrences mesurées : 231, 279, 422, 781, 1815, 2303 ; table .ts_domain_codes à la première d'entre elles) — SUBSTANCE VRAIE, ancre dérivée (drift)

### static-result-module-field — docs/mcp/DRIVE_SESSION_RUNBOOK.md l.75
- **Assertion (runbook)** : §2 commande 3 : result.json porte un champ top-level « module » lisible par « Select-Object status, module, applied_at »
- **Attendu** : une ligne « module = » existe dans le payload top-level du writer (fenêtre 1936-1990)
- **Mesuré** : PAS de champ top-level « module » dans le contrat mesuré (1936-1990) — le champ top-level est « active_module » ; « module » n'existe que DANS le bloc job (mesuré à 4801-4827 : module = as.character(job$module) ; fixture watchtower : module ABSENT top-level). La commande 3 du runbook (« Select-Object status, module, applied_at », l.75) afficherait une colonne module VIDE — drift SUBSTANTIEL

### static-core-path — docs/mcp/DRIVE_SESSION_RUNBOOK.md l.47
- **Assertion (runbook)** : §2/§4.4/l.265 : le fichier cité « R/core/drive_watcher.R » (ancres :588-589, :688, :806, :4486) existe
- **Attendu** : R/core/drive_watcher.R présent dans l'arbre
- **Mesuré** : R/core/drive_watcher.R ABSENT (contenu de R/core/ mesuré : VIDE) — le fichier vit à tools/pkg_skeleton/R/drive_watcher.R (présent). Les citations l.47/215/217/265 du runbook pointent un chemin déménagé — drift majeur, substance couverte par static-functions-exist

### static-agents-rule — docs/mcp/DRIVE_SESSION_RUNBOOK.md l.226
- **Assertion (runbook)** : §5 : la règle opérationnelle 🔴 (« ne JAMAIS conclure session morte de ready_fresh()==FALSE pendant un job en vol ») est écrite à « AGENTS.md:7 », texte cité
- **Attendu** : la ligne 7 de AGENTS.md contient « RÈGLE OPÉRATIONNELLE » et « ready_fresh »
- **Mesuré** : AGENTS.md:7 mesuré = « 🟢 **MODE BOOST — LEVÉ le 2026-11-01 (consigne opérateur « Unset boost mod » ; condition O25/D-D remplie : frontière de v… » (MODE BOOST) — la règle 🔴 « ne JAMAIS conclure “session morte”… » est maintenant à la ligne 11 (cité AGENTS.md:7 aux l.226/236 du runbook) — ancre dérivée (drift)

### static-drive-readme-minor — docs/mcp/DRIVE_SESSION_RUNBOOK.md l.185
- **Assertion (runbook)** : ancres FINES de tools/_drive/README.md exactes — :38 (cadence ~3 s), :39 (hb_n 0 at boot), :143-144 (étape 7 « Disarm by removing arm.json »), :527-556 (citation « not evidence of a lost write »)
- **Attendu** : 4/4 motifs dans leurs fenêtres citées
- **Mesuré** : ancres fines dérivées (4/4) : l.185 : cadence ~3 s à README:38 ; l.115 : hb_n « 0 at boot » à README:39 ; l.182 : étape 7 « Disarm by removing arm.json » à README:143-144 ; l.190 : « not evidence of a lost write » à README:527-556 — positions réelles mesurées : ~3 s -> ligne réelle 39 ; 0 at boot -> ligne réelle 40 ; 7. Disarm by removing -> ligne réelle 145 ; not evidence of a lost write -> ligne réelle 558

### live-rearm-keeps — docs/mcp/DRIVE_SESSION_RUNBOOK.md l.183
- **Assertion (runbook)** : §4.2 : le re-arm conserve la MÊME session — session_token inchangé, armed:true, hb_n repart sans redémarrer au milieu de session
- **Attendu** : après écriture arm.json {même token, armed:true} : token identique, armed=true, hb_n avancé
- **Mesuré** : re-arm mesuré incomplet : token_same=TRUE armed=FALSE hb_advance=FALSE

## UNTESTED (honnêteté — jamais inventés)

- **live-stale-tool** (l.20) : aucun handshake PÉRIMÉ observé pendant la sonde (la session détectée est vivante) — l'expérience exige un ready.json stale + l'outil read-only ; jamais fabriquée
- **live-job-inflight** (l.224) : état « ready.json périmé + result.json:running » non observé pendant la sonde — la fenêtre de job en vol n'est pas fabricable sans job long réel ; jamais inventée

## Détail par claim

| id | type | l. | verdict | evidence |
|---|---|---|---|---|
| static-launcher | static | 146 | ✅ | tools/launch_dev_drive.R:4 = « # The ONLY supported way to start a drive-enabled session (spec §4, G4 #16). » |
| static-tools-exist | static | 18 | ✅ | 3/3 outils enregistrés dans scripts/mcp_server.R (registrations mesurées aux lignes 3365, 3418, 3475, 3524, 3533, 3542, 3567, 3627) |
| static-anchor-mcp-banner | static | 19 | ❌ | fenêtre 104-106 mesurée :   .ts_stderr("  tools        : transcripto_drive_status, transcripto_drive_read_result (read-only)") /   .ts_stderr("                 transcripto_drive_state (M2: passive, read-only)") /   .ts_stderr("                 transcripto_drive_datasets (P2 PoC datasets: passive, read-only, session-free)") — « transcripto_drive_snapshot » ABSENT de la fenêtre citée (première occurrence réelle : ligne 107) — ancre dérivée (drift) |
| static-anchor-stale-code | static | 21 | ❌ | scripts/mcp_server.R:219 ne porte PAS le code ; mesuré : «        structuredContent = structured, isError = FALSE) ». Le code existe ailleurs (occurrences mesurées : 231, 279, 422, 781, 1815, 2303 ; table .ts_domain_codes à la première d'entre elles) — SUBSTANCE VRAIE, ancre dérivée (drift) |
| static-commits-exist | static | 193 | ✅ | git cat-file -t : 7cccd6c4=commit, db127122=commit, 59c48880=commit — les 3 commits cités existent |
| static-result-contract | static | 10 | ✅ | 13/13 champs mesurés au writer (drive_watcher.R:1936-1990) : protocol, ack_seq, status, applied_at, active_module, armed, preserve_data, errors, warnings, snapshot, job, descriptor, write_error ; fixture watchtower conformes (mêmes 13 noms) — la SUBSTANCE du runbook (status/applied_at documentés l.10/l.75) est couverte |
| static-result-module-field | static | 75 | ❌ | PAS de champ top-level « module » dans le contrat mesuré (1936-1990) — le champ top-level est « active_module » ; « module » n'existe que DANS le bloc job (mesuré à 4801-4827 : module = as.character(job$module) ; fixture watchtower : module ABSENT top-level). La commande 3 du runbook (« Select-Object status, module, applied_at », l.75) afficherait une colonne module VIDE — drift SUBSTANTIEL |
| static-ready-contract | static | 43 | ✅ | champs mesurés au writer ready (ts_drive_write_ready, 561-660) : hb_at, hb_n, hb_timeout_s ; ts_drive_hb_timeout() retourne bien 15 par défaut (return(15) mesuré) ; fixture watchtower : les 3 champs présents |
| static-core-path | static | 47 | ❌ | R/core/drive_watcher.R ABSENT (contenu de R/core/ mesuré : VIDE) — le fichier vit à tools/pkg_skeleton/R/drive_watcher.R (présent). Les citations l.47/215/217/265 du runbook pointent un chemin déménagé — drift majeur, substance couverte par static-functions-exist |
| static-functions-exist | static | 216 | ✅ | mesuré au nouveau chemin : ts_drive_session_id déf. l.902 ; ts_drive_ready_fresh déf. l.735 ; session_id publié l.4015, 4128, 4188 — substance du §4.4 VRAIE (seul le chemin/numéro cité par le runbook a dérivé, filé ailleurs) |
| static-agents-rule | static | 226 | ❌ | AGENTS.md:7 mesuré = « 🟢 **MODE BOOST — LEVÉ le 2026-11-01 (consigne opérateur « Unset boost mod » ; condition O25/D-D remplie : frontière de v… » (MODE BOOST) — la règle 🔴 « ne JAMAIS conclure “session morte”… » est maintenant à la ligne 11 (cité AGENTS.md:7 aux l.226/236 du runbook) — ancre dérivée (drift) |
| static-drive-readme-anchors | static | 262 | ✅ | 11/11 ancres majeures mesurées : chaque fenêtre citée porte la substance citée |
| static-drive-readme-minor | static | 185 | ❌ | ancres fines dérivées (4/4) : l.185 : cadence ~3 s à README:38 ; l.115 : hb_n « 0 at boot » à README:39 ; l.182 : étape 7 « Disarm by removing arm.json » à README:143-144 ; l.190 : « not evidence of a lost write » à README:527-556 — positions réelles mesurées : ~3 s -> ligne réelle 39 ; 0 at boot -> ligne réelle 40 ; 7. Disarm by removing -> ligne réelle 145 ; not evidence of a lost write -> ligne réelle 558 |
| static-audit-anchors | static | 22 | ✅ | 5/5 ancres de l'audit mesurées : chaque fenêtre citée porte la substance citée (§1, l.22/88-89/33 du runbook) |
| live-hb-advance | live | 67 | ✅ | mesuré : hb_n 3442 -> 3444 (2 lectures espacées ~4 s) — session vivante |
| live-hb-age | live | 54 | ✅ | âge heartbeat mesuré 3.9 s ≤ hb_timeout_s (15 s) |
| live-launch-arms | live | 147 | ✅ | session armée mesurée : pid 41780 vivant, armed=true, hb_n avance — signes du lancement supporté (§4.1) |
| live-rearm-keeps | live | 183 | ❌ | re-arm mesuré incomplet : token_same=TRUE armed=FALSE hb_advance=FALSE |
| live-stale-tool | live | 20 | UNTESTED | aucun handshake PÉRIMÉ observé pendant la sonde (la session détectée est vivante) — l'expérience exige un ready.json stale + l'outil read-only ; jamais fabriquée |
| live-job-inflight | live | 224 | UNTESTED | état « ready.json périmé + result.json:running » non observé pendant la sonde — la fenêtre de job en vol n'est pas fabricable sans job long réel ; jamais inventée |

---
Satellite doc_prover (jalon 4 / S5 N6) — lecture-seul sur le dépôt ; voir README.md (instruction de service, procédure de falsification).

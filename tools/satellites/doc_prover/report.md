# Rapport doc-prover — docs/mcp/DRIVE_SESSION_RUNBOOK.md

Généré : 2026-11-04T17:45:44Z · --live : non · moteur : prover_lib (base R)

**Verdict global : runbook ✅ complet**

Comptes : ✅ pass = 14 · ❌ fail = 0 · UNTESTED = 6 — total 20 claims (14 static / 6 live)

## ❌ Findings de drift

Aucun — toutes les assertions statiques mesurées sont vraies.

## UNTESTED (honnêteté — jamais inventés)

- **live-hb-advance** (l.67) : gate live non satisfait — pas de sonde (--live absent ou session non détectée)
- **live-hb-age** (l.54) : gate live non satisfait — âge du heartbeat non mesuré
- **live-launch-arms** (l.147) : gate live non satisfait — pas de sonde (launch_dev_drive.R n'est PAS relancé par le prover : invasif)
- **live-rearm-keeps** (l.183) : re-arm non exécuté (exige --live ; écriture limitée au drop-box gitigné tools/_drive/*.json, .gitignore:100)
- **live-stale-tool** (l.20) : aucun handshake PÉRIMÉ observé pendant la sonde (la session détectée est vivante) — l'expérience exige un ready.json stale + l'outil read-only ; jamais fabriquée
- **live-job-inflight** (l.224) : état « ready.json périmé + result.json:running » non observé pendant la sonde — la fenêtre de job en vol n'est pas fabricable sans job long réel ; jamais inventée

## Détail par claim

| id | type | l. | verdict | evidence |
|---|---|---|---|---|
| static-launcher | static | 146 | ✅ | tools/launch_dev_drive.R:4 = « # The ONLY supported way to start a drive-enabled session (spec §4, G4 #16). » |
| static-tools-exist | static | 18 | ✅ | 3/3 outils enregistrés dans scripts/mcp_server.R (registrations mesurées aux lignes 3365, 3418, 3475, 3524, 3533, 3542, 3567, 3627) |
| static-anchor-mcp-banner | static | 19 | ✅ | les lignes 104-107 nomment les trois outils read-only (ré-ancrage passe doc 2026-11-04 ; snapshot première occurrence : ligne 107) |
| static-anchor-stale-code | static | 21 | ✅ | scripts/mcp_server.R:231 = « .ts_domain_codes <- c("NO_SESSION", "STALE_SESSION", "RESULT_SESSION_MISMATCH", » |
| static-commits-exist | static | 193 | ✅ | git cat-file -t : 7cccd6c4=commit, db127122=commit, 59c48880=commit — les 3 commits cités existent |
| static-result-contract | static | 10 | ✅ | 13/13 champs mesurés au writer (drive_watcher.R:1936-1990) : protocol, ack_seq, status, applied_at, active_module, armed, preserve_data, errors, warnings, snapshot, job, descriptor, write_error ; fixture watchtower conformes (mêmes 13 noms) — la SUBSTANCE du runbook (status/applied_at documentés l.10/l.75) est couverte |
| static-result-module-field | static | 75 | ✅ | le champ top-level du payload result.json est « active_module » (fenêtre 1936-1990 ; « module » confirmé DANS le bloc job 4801-4827 ; fixture watchtower : active_module présent) — commande 3 réparée « Select-Object status, active_module, applied_at » cohérente (passe doc 2026-11-04) |
| static-ready-contract | static | 43 | ✅ | champs mesurés au writer ready (ts_drive_write_ready, 561-660) : hb_at, hb_n, hb_timeout_s ; ts_drive_hb_timeout() retourne bien 15 par défaut (return(15) mesuré) ; fixture watchtower : les 3 champs présents |
| static-core-path | static | 47 | ✅ | tools/pkg_skeleton/R/drive_watcher.R existe (runbook réparé : citations l.47/215/217/265 au nouveau chemin — passe doc 2026-11-04 ; l'ancien R/core/drive_watcher.R reste absent, conforme) |
| static-functions-exist | static | 216 | ✅ | mesuré au nouveau chemin : ts_drive_session_id déf. l.902 ; ts_drive_ready_fresh déf. l.735 ; session_id publié l.4015, 4128, 4188 — substance du §4.4 VRAIE (seul le chemin/numéro cité par le runbook a dérivé, filé ailleurs) |
| static-agents-rule | static | 226 | ✅ | AGENTS.md:11 porte la règle opérationnelle 🔴 (ré-ancrage passe doc 2026-11-04) |
| static-drive-readme-anchors | static | 262 | ✅ | 11/11 ancres majeures mesurées : chaque fenêtre citée porte la substance citée |
| static-drive-readme-minor | static | 185 | ✅ | 4/4 ancres fines mesurées exactes |
| static-audit-anchors | static | 22 | ✅ | 5/5 ancres de l'audit mesurées : chaque fenêtre citée porte la substance citée (§1, l.22/88-89/33 du runbook) |
| live-hb-advance | live | 67 | UNTESTED | gate live non satisfait — pas de sonde (--live absent ou session non détectée) |
| live-hb-age | live | 54 | UNTESTED | gate live non satisfait — âge du heartbeat non mesuré |
| live-launch-arms | live | 147 | UNTESTED | gate live non satisfait — pas de sonde (launch_dev_drive.R n'est PAS relancé par le prover : invasif) |
| live-rearm-keeps | live | 183 | UNTESTED | re-arm non exécuté (exige --live ; écriture limitée au drop-box gitigné tools/_drive/*.json, .gitignore:100) |
| live-stale-tool | live | 20 | UNTESTED | aucun handshake PÉRIMÉ observé pendant la sonde (la session détectée est vivante) — l'expérience exige un ready.json stale + l'outil read-only ; jamais fabriquée |
| live-job-inflight | live | 224 | UNTESTED | état « ready.json périmé + result.json:running » non observé pendant la sonde — la fenêtre de job en vol n'est pas fabricable sans job long réel ; jamais inventée |

---
Satellite doc_prover (jalon 4 / S5 N6) — lecture-seul sur le dépôt ; voir README.md (instruction de service, procédure de falsification).

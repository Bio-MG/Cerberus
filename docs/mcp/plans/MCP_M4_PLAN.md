# M4 — long-job observation (`transcripto_drive_wait`) — PLAN (2026-09-24)

**Statut** : plan **accepté** par l'utilisateur avec deux décisions : (a) **un seul** outil
`transcripto_drive_wait` avec un booléen `observe` ; (b) **aucun** snapshot frais pendant un job
**synchrone en cours**. Ce fichier est **hors du manifeste gelé** (`.workbuddy-ai/` est exclu).

## 0. Porte pré-édition
Manifeste recalculé = `aa7c8aa57251adc26f453716fbb0b82fa788989a949a6d47aa3c34ee975e30cb` (**437** f.)
= exigé ⇒ **PASS**. Cible avant : `scripts/mcp_server.R` = `4d92ee4861bce72b…`, **6** outils.

## 1. Ce que M4 est, et n'est pas
**Un outil** qui suit un `run_pipeline` **déjà autorisé**. Il n'ajoute **aucune** action, module,
bouton ni capacité d'écriture — **sauf UN snapshot post-terminal**, sur demande explicite
(`observe=true`), qui est le mécanisme d'observation fraîche du protocole lui-même.

🔴 **Contrainte mesurée qui gouverne tout** (README §188 « RESIDUAL ») : un job long est
**SYNCHRONE** — il bloque la boucle d'événements, donc pendant sa durée **aucun tick ne tourne**, le
battement s'arrête **par conception**, et **rien ne peut être consommé**. Une observation de
progression *pendant* le job est donc **impossible**, et un snapshot frais *pendant* le job serait
**inutile ET dangereux** (§6).

## 2. Nom exact
**`transcripto_drive_wait`** — 7ᵉ outil. Aucune extension d'un outil existant.

## 3. Schéma d'entrée final
```json
{ "type": "object",
  "properties": {
    "seq":       { "type": "integer", "minimum": 1 },
    "module":    { "type": "string", "enum": ["import_bulk","bulk_filter","bulk_de","bulk_pathways"] },
    "timeout_s": { "type": "integer", "minimum": 1, "maximum": 600 },
    "poll_ms":   { "type": "integer", "minimum": 200, "maximum": 5000 },
    "observe":   { "type": "boolean" },
    "expect":    { "type": "object",
                   "properties": { "session_id": {"type":"string"},
                                   "pid": {"type":"integer"},
                                   "started_at": {"type":"string"} },
                   "required": ["session_id"], "additionalProperties": false } },
  "required": ["seq", "module", "expect"],
  "additionalProperties": false }
```

## 4. Sémantique de timeout et de polling
- `timeout_s` défaut **120**, plafond dur **600** ; `poll_ms` défaut **500**, min **200**, max **5000**.
  Hors bornes ⇒ `-32602` (requête malformée, canal protocole).
- Boucle bornée par **horloge murale**, jamais infinie.
- **Détection de changement** : chaque poll calcule une empreinte du verdict (`ack_seq|status|applied_at`).
  Un verdict **inchangé n'est JAMAIS rapporté comme progression** et ne compte jamais comme
  transition. Après `stall_rounds = 3` polls inchangés, l'intervalle **recule** (doublement jusqu'à
  ≤ `4 × poll_ms`) ⇒ la boucle ne peut pas tourner à vide.
- La réponse porte toujours `observation.changed`, `unchanged_polls`, `last_change_age_s`, `stalled`.
- Le serveur **dort** entre deux polls : pendant ce temps il ne répond pas à d'autres requêtes
  (documenté).

## 5. Table des états terminaux
Conforme au README §121–145 et à `ts_drive_status_terminal()` (= `done|error|ignored|invalid`) :

| Observation | `state` | Terminal ? |
|---|---|---|
| pas de verdict, ou `ack_seq < seq` | **`accepted`** (scénario pas encore consommé) | non |
| `ack_seq == seq`, `applied` | `applied` (inputs posés, pipeline non fini) | **non** |
| `ack_seq == seq`, `running` | `running` (job en vol) | **non** |
| `ack_seq == seq`, `done` | **`done`** | **oui** |
| `ack_seq == seq`, `error` | `error` | **oui** |
| `ack_seq == seq`, `invalid` | `invalid` (refus de l'app) | **oui** |
| `ack_seq == seq`, `ignored` | `ignored` (protocole/seq périmé) | **oui** |
| **statut inconnu** | traité comme **NON terminal** (on continue d'attendre) | non |
| `timeout_s` écoulé | **`timeout`** | terminal **pour l'APPEL** seulement |
| session disparue / jeton changé / (battement périmé **ET** pid mort **ET** pas `running`) | **`session_lost`** | oui |
| verdict non terminal **ou** plus vieux que le timeout de battement | **`stale`** — drapeau **`observation.stale`** | orthogonal |

🔴 **`timeout` ne signifie QUE « l'appel d'attente est terminé »** — jamais que le job a échoué ni
qu'il a fini.
🔴 **`stale` est orthogonal** à `state` : une observation périmée peut accompagner n'importe quel état
non terminal. Il est donc rapporté comme **drapeau** (et documenté dans `state_semantics`), jamais
confondu avec un état terminal.

## 6. Snapshots frais — garanties, et pourquoi pas pendant le job
`observe=true` déclenche **le** mécanisme du protocole : écrire **UN** scénario `snapshot` avec
`seq = max(last_seq, run_seq) + 1`, attendre son ack, et rendre la **projection à liste blanche
M3a**.
- **Fraîcheur** : le snapshot est émis **APRÈS** le terminal ⇒ son `ack_seq > run_seq` ;
  `business_state.observed_after_terminal` n'est `true` que si c'est vérifié.
- **Sécurité** : émis **après** le terminal ⇒ il ne peut **pas** écraser le terminal déjà rendu.
  `last_seq` est **relu immédiatement avant** l'écriture (même garde que M3b/M3c).
- 🔴 **Décision (b) confirmée** : **aucun** snapshot frais **pendant** un job en cours. Le tick
  résout le terminal **pending AVANT** de consommer quoi que ce soit, donc un snapshot mis en file
  pendant le job serait consommé au beat **suivant** et pourrait **écraser le terminal que l'attente
  cherche**. Une escalade mi-job est donc **inutile** (la boucle est bloquée) **et** risquée.
  Condition explicite : `observe=true` n'écrit **que** si l'état est terminal ; sinon
  `OBSERVE_SKIPPED_NO_TERMINAL`.
- Si l'ack n'arrive pas dans le délai d'observation ⇒ **`OBSERVE_FAILED`**, **sans** revendiquer
  aucune complétion métier.

## 7. Achèvement métier — comment il est vérifié
**Il ne l'est PAS par M4, et M4 le dit.** `business_state.completion_verified` reste **`false`**,
avec une note explicite : le `done` du protocole signifie « le jeton a bougé » ou « le module a
déclaré son job terminé » ; ni l'un ni l'autre ne prouve qu'une analyse a fini. M4 :
- rapporte le **terminal de protocole** pour le `seq` exact ;
- avec `observe=true`, publie **l'état publié par le module** (ses scalaires) pour que l'appelant
  juge — et indique si cette observation est **postérieure** au terminal.

Aucun contrat d'achèvement applicatif explicite n'existe aujourd'hui ⇒ **aucune** inférence.

## 8. Contrat d'erreur
**Nouveaux** : `SESSION_LOST` · `OBSERVE_FAILED` · `OBSERVE_SKIPPED_NO_TERMINAL`.
**Réutilisés** : `NO_SESSION` · `INVALID_PROTOCOL` · `READ_FAILED` · `SESSION_MISMATCH` ·
`SESSION_ASSERTION_REQUIRED` · `SESSION_NOT_ARMED` · `MODULE_NOT_ALLOWED` · `SEQ_STALE` ·
`SCENARIO_WRITE_FAILED`. **Protocole** : `-32602`.
🔴 **`STALE_SESSION` n'est PAS utilisé par `wait`** : pendant un job long le battement **doit**
s'arrêter — c'est le résidu documenté, pas une faute. Un battement périmé pendant `running` est
rapporté (`heartbeat_stalled_by_job: true`) et **n'interrompt jamais** l'attente.

## 9. Fichiers à modifier
**`scripts/mcp_server.R` uniquement.** Aucune modification de `app.R`, `modules/`,
`R/core/drive_watcher.R`, `R/core/drive_allowlist.R`, `tests/`, `renv.lock`, `docs/`, des configs
clients ni du protocole drive.

## 10. Exigences transverses
- **Séquences** : `wait` n'écrit pas (sauf `observe`), qui utilise `last_seq + 1` **relu juste avant**
  ⇒ un écrivain concurrent provoque `SEQ_STALE`, jamais une collision. Rejeu impossible : `seq` doit
  être celui de l'appelant.
- **Agents concurrents** : `expect.session_id` lie l'attente à UNE session ; `wait` est en lecture
  seule, il ne peut pas perturber un autre agent.
- **Charge utile bornée** : scalaires + projection M3a (déjà plafonnée à 8192 o) + `timeline` de
  **8** transitions au maximum.
- **Aucune fuite** : jeton `<redacted>` ; aucun chemin absolu, matrice, charge biologique ni journal
  brut ; l'état de module passe par la **liste blanche M3a**, jamais par la sonde brute.
- **Framing NDJSON** inchangé ; SDK **1.30.1** (7 outils) ; régression WorkBuddy/ZCode et M3a/M3b/M3c.

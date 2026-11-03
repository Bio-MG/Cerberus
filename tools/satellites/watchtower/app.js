"use strict";
/* ============================================================================
 * N1 drive-watchtower — app.js (vanilla, zéro dépendance, zéro CDN)
 *
 * Lecture SEULE de tools/_drive/{ready,result,scenario}.json par fetch polling.
 * La page n'écrit jamais : elle observe le même canal que
 * transcripto_drive_state (O11/SW-5c), côté humain.
 *
 * Le fichier est en deux blocs :
 *   1. BLOC PUR   — fonctions sans DOM, testables seules (falsification :
 *                   `node` évalue ce bloc et rejoue les 3 états de fixture) ;
 *   2. BLOC DOM   — polling + rendu, référencé par index.html.
 *
 * Contrat consommé (mesuré) :
 *   - ready.json : hb_at (ISO UTC), hb_n (compteur monotone), hb_timeout_s
 *     (défaut 15 s), session_token, pid, port, armed, viewer, last_seq,
 *     started_at — tools/_drive/README.md:37-53.
 *   - result.json : ack_seq, status (6 valeurs figées :
 *     ignored|invalid|applied|running|done|error — drive_watcher.R:1062),
 *     job.elapsed_s, applied_at, errors, warnings, active_module
 *     (drive_watcher.R:1958-1988). Statique entre deux scénarios consommés.
 *   - scenario.json : seq, action, module, button, expect, session_token
 *     (liste blanche ts_drive_validate_scenario, drive_watcher.R:1088+).
 * ========================================================================== */

/* ============================================================================
 * 1. BLOC PUR — aucune référence au DOM, aucune variable externe
 * ========================================================================== */

/** Les quatre statuts TERMINAUX d'un verdict (spec §2.4 ; ts_drive_status_terminal,
 *  drive_watcher.R:1077-1085). Un statut INCONNU n'est PAS terminal : on continue
 *  de sonder au lieu de croire à une complétion. */
const TERMINAL_STATUSES = new Set(["done", "error", "ignored", "invalid"]);

function isTerminal(status) {
  return typeof status === "string" && TERMINAL_STATUSES.has(status);
}

/** Lit les paramètres d'URL.
 *  ?dir=  répertoire drive sondé — chemin RELATIF à cette page. Défaut
 *         "../../_drive" : depuis tools/satellites/watchtower/ cela vise
 *         tools/_drive/ quand `python -m http.server 4892` tourne à la RACINE
 *         du dépôt (mesuré : c'est l'unique disposition qui sert à la fois la
 *         page ET les JSON en same-origin ; `-d tools/_drive` ne sert que les
 *         JSON, et file:// bloque fetch).
 *  ?now=  ISO-8601 : fige l'horloge de référence (fixtures — garde les états
 *         « alive »/« frozen » déterministes quel que soit le jour de la capture).
 * @returns {{dir:string|null, dirInvalid:boolean, nowMs:number, nowFixed:string|null}}
 */
function readParams(search, nowMsFallback) {
  const q = new URLSearchParams(search || "");
  let dir = q.get("dir");
  let dirInvalid = false;
  if (dir !== null) {
    // Garde-fou minimal : on refuse un chemin absolu, un schéma, un backslash,
    // un fragment — la page ne sonde qu'un sous-chemin relatif du serveur local.
    if (dir === "" || /^[a-z]+:/i.test(dir) || dir.startsWith("/") ||
        dir.startsWith("\\") || dir.includes("\\") || dir.includes("#") ||
        dir.includes("?")) {
      dir = null;
      dirInvalid = true;
    }
  }
  const nowRaw = q.get("now");
  let nowMs = (typeof nowMsFallback === "number") ? nowMsFallback : Date.now();
  let nowFixed = null;
  if (nowRaw !== null && nowRaw !== "") {
    const t = Date.parse(nowRaw);
    if (!Number.isNaN(t)) { nowMs = t; nowFixed = nowRaw; }
  }
  return { dir, dirInvalid, nowMs, nowFixed };
}

/** Âge du heartbeat en secondes, ou null si hb_at est absent/illisible.
 *  Peut être NÉGATIF (horloge client en retard) : l'appelant l'affiche tel quel. */
function hbAgeS(hbAtIso, nowMs) {
  if (typeof hbAtIso !== "string" || hbAtIso === "") return null;
  const t = Date.parse(hbAtIso);
  if (Number.isNaN(t)) return null;
  return (nowMs - t) / 1000;
}

/** Timeout effectif : la valeur du fil, sinon le défaut du contrat (15 s,
 *  tools/_drive/README.md:40). */
function hbTimeoutS(ready) {
  const n = Number(ready && ready.hb_timeout_s);
  return Number.isFinite(n) && n > 0 ? n : 15;
}

/* --- LE cœur : classification d'état ---------------------------------------
 * Cinq états, un seul verdict visuel à l'écran :
 *   "absent"   ready.json ABSENT            → « PAS DE SESSION »   (rouge)
 *   "invalid"  présent mais hb_at illisible → « READY ILLISIBLE »  (rouge)
 *   "alive"    âge hb ≤ hb_timeout_s        → « VIVANT »           (vert)
 *   "flight"   hb gelé MAIS result.json porte status==="running"
 *              → « JOB IN FLIGHT » — JAMAIS « DEAD » (leçon 537 s : un
 *              job synchrone bloque la boucle Shiny, le heartbeat cesse,
 *              et result.json carrying running est la preuve de vie que
 *              le heartbeat ne peut pas donner — _drive/README.md:189-204)
 *   "stale"    hb gelé, aucun running en vol → « STALE — HB GELÉ » (jaune)
 * @param ready  objet ready.json parsé, ou null si absent/illisible
 * @param result objet result.json parsé, ou null
 * @param nowMs  horloge de référence (ms epoch)
 */
function classifyState(ready, result, nowMs) {
  if (ready === null || ready === undefined) return "absent";
  const age = hbAgeS(ready.hb_at, nowMs);
  if (age === null) return "invalid";
  if (age <= hbTimeoutS(ready)) return "alive";
  return (result && result.status === "running") ? "flight" : "stale";
}

/* --- Scénario : en attente ou consommé ? -----------------------------------
 * Le watcher ne supprime pas scenario.json après consommation (aucun unlink
 * de scenario.json dans drive_watcher.R) : la déduplication se fait par seq
 * (un seq <= last_seq est ignoré, _drive/README.md:141-142). D'où :
 *   "absent"              pas de scenario.json
 *   "invalid"             présent mais seq illisible
 *   "pending-no-session"  présent, pas de ready.json   → en attente sans client
 *   "pending"             scn.seq > ready.last_seq     → EN ATTENTE
 *   "consumed"            scn.seq <= ready.last_seq    → consommée
 */
function scenarioState(scn, ready) {
  if (scn === null || scn === undefined) return "absent";
  const seq = Number(scn && scn.seq);
  if (!Number.isFinite(seq)) return "invalid";
  if (ready === null || ready === undefined) return "pending-no-session";
  const last = Number(ready.last_seq);
  if (!Number.isFinite(last)) return "pending";
  return seq > last ? "pending" : "consumed";
}

/* ============================================================================
 * 2. BLOC DOM — polling + rendu
 * ========================================================================== */

const POLL_INTERVAL_MS = 1000; // beat heartbeat ≈ 3 s : sonder 1×/s suffit.
const DEFAULT_DIR = "../../_drive";

const els = {};
["banner-pill", "banner-why", "s-token", "s-pid", "s-port", "s-armed",
 "s-viewer", "s-hbn", "s-hbat", "s-hbage", "s-hbto", "s-lastseq", "s-started",
 "r-seq", "r-status", "r-terminal", "r-module", "r-action", "r-elapsed",
 "r-job", "r-applied", "r-errors", "r-note", "r-errlist",
 "c-state", "c-seq", "c-action", "c-module", "c-button", "c-expect", "c-note",
 "f-dir", "f-clock", "f-poll", "poll-error",
].forEach((id) => { els[id] = document.getElementById(id); });

function set(id, text) { els[id].textContent = text; }
function fmtNum(v, unit) {
  return (v === null || v === undefined || v === "") ? "—" : String(v) + (unit || "");
}
function fmtS(v) { // secondes, 1 décimale
  return (typeof v === "number" && Number.isFinite(v)) ? v.toFixed(1) + " s" : "—";
}

/** Une seule règle d'écriture DOM : textContent. Aucun innerHTML alimenté par
 *  les JSON — un fichier tiers ne peut pas injecter de markup dans la page. */

/* --- Fetch d'un fichier du drive -------------------------------------------
 * 404       → { kind: "absent" }   (absence = information, pas une panne)
 * autre HTTP→ { kind: "error" }
 * corps non JSON → { kind: "invalid" }
 * cache:"no-store" : le SimpleHTTPRequestHandler envoie Last-Modified et les
 * navigateurs peuvent servir une réponse périmée — interdit pour un heartbeat.
 */
async function fetchDriveFile(url) {
  let res;
  try {
    res = await fetch(url, { cache: "no-store" });
  } catch (e) {
    return { kind: "error", msg: String(e) };
  }
  if (res.status === 404) return { kind: "absent" };
  if (!res.ok) return { kind: "error", msg: "HTTP " + res.status };
  try {
    return { kind: "ok", data: await res.json() };
  } catch (e) {
    return { kind: "invalid" };
  }
}

function dirBaseUrl(dirRaw) {
  const dir = dirRaw || DEFAULT_DIR;
  // Trailing slash : la résolution relative se fait sur le RÉPERTOIRE.
  return new URL(dir.replace(/\/+$/, "") + "/", location.href);
}

/* --- Rendu du bandeau (le verdict visuel unique) ---------------------------- */
const BANNERS = {
  alive:   { cls: "alive",  text: "VIVANT" },
  stale:   { cls: "stale",  text: "STALE — HB GELÉ" },
  flight:  { cls: "flight", text: "JOB IN FLIGHT" },
  absent:  { cls: "absent", text: "PAS DE SESSION" },
  invalid: { cls: "err",    text: "READY.JSON ILLISIBLE" },
  error:   { cls: "err",    text: "READY.JSON INJOIGNABLE" },
};

function renderBanner(state, ready, age, timeout) {
  const b = BANNERS[state];
  els["banner-pill"].className = "pill " + b.cls;
  els["banner-pill"].textContent = b.text;
  let why;
  switch (state) {
    case "alive":
      why = "heartbeat frais : " + fmtS(age) + " ≤ hb_timeout_s (" + timeout + " s).";
      break;
    case "flight":
      why = "heartbeat gelé (" + fmtS(age) + " > " + timeout + " s) MAIS result.json porte « running » " +
            "⇒ travail en cours, PAS une session morte (leçon 537 s).";
      break;
    case "stale":
      why = "heartbeat figé depuis " + fmtS(age) + " > " + timeout + " s, aucun « running » en vol " +
            "⇒ session probablement partie. Ne pas armer ce token.";
      break;
    case "absent":
      why = "ready.json absent — aucun client vivant (contrat : ne pas inventer de session).";
      break;
    case "invalid":
      why = "ready.json présent mais illisible (JSON ou hb_at) : la fraîcheur est indécidable.";
      break;
    default: // "error"
      why = "ready.json n'a pas pu être récupéré (réseau/HTTP) — l'état de session est indécidable.";
  }
  els["banner-why"].textContent = why;
}

/* --- Rendu des panneaux ------------------------------------------------------ */
function renderSession(ready, fileState) {
  if (fileState.kind !== "ok") {
    const label = fileState.kind === "absent" ? "— (ready.json absent)"
      : fileState.kind === "invalid" ? "— (JSON invalide)"
      : "— (erreur de lecture)";
    set("s-token", label);
    ["s-pid", "s-port", "s-armed", "s-viewer", "s-hbn", "s-hbat", "s-hbage",
     "s-hbto", "s-lastseq", "s-started"].forEach((id) => set(id, "—"));
    return;
  }
  set("s-token", String(ready.session_token ?? "—"));
  set("s-pid", fmtNum(ready.pid));
  set("s-port", fmtNum(ready.port));
  set("s-armed", ready.armed === true ? "oui (armé)" : ready.armed === false ? "non" : "—");
  set("s-viewer", fmtNum(ready.viewer));
  set("s-hbn", fmtNum(ready.hb_n));
  set("s-hbat", fmtNum(ready.hb_at));
  set("s-hbage", fmtS(hbAgeS(ready.hb_at, nowMs())));
  set("s-hbto", fmtNum(ready.hb_timeout_s, " s") +
      (Number.isFinite(Number(ready.hb_timeout_s)) ? "" : " (défaut contrat)"));
  set("s-lastseq", fmtNum(ready.last_seq));
  set("s-started", fmtNum(ready.started_at));
}

function renderResult(result, fileState) {
  els["r-errlist"].hidden = true;
  els["r-note"].hidden = true;
  if (fileState.kind !== "ok") {
    const label = fileState.kind === "absent" ? "— (aucun verdict encore)"
      : fileState.kind === "invalid" ? "— (JSON invalide)" : "— (erreur de lecture)";
    ["r-seq", "r-status", "r-terminal", "r-module", "r-action", "r-elapsed",
     "r-job", "r-applied", "r-errors"].forEach((id) => set(id, label));
    return;
  }
  const job = result.job || {};
  const status = result.status;
  set("r-seq", fmtNum(result.ack_seq));
  els["r-status"].textContent = "";
  const tag = document.createElement("span");
  tag.className = "tag " + (typeof status === "string" ? status : "unknown");
  tag.textContent = String(status ?? "—");
  els["r-status"].appendChild(tag);
  set("r-terminal", isTerminal(status) ? "oui" : "non — continuer à sonder");
  set("r-module", fmtNum(result.active_module));
  set("r-action", fmtNum(job.action));
  set("r-elapsed", fmtS(job.elapsed_s));
  set("r-job", job.status ? String(job.status) : "—");
  set("r-applied", fmtNum(result.applied_at));
  const nErr = Array.isArray(result.errors) ? result.errors.length : 0;
  const nWarn = Array.isArray(result.warnings) ? result.warnings.length : 0;
  set("r-errors", nErr + " / " + nWarn);
  if (nErr > 0) {
    els["r-errlist"].hidden = false;
    els["r-errlist"].textContent = "";
    result.errors.slice(0, 6).forEach((m) => {
      const li = document.createElement("li");
      li.textContent = String(m);
      els["r-errlist"].appendChild(li);
    });
  }
  els["r-note"].hidden = false;
  els["r-note"].textContent =
    "result.json est STATIQUE entre deux scénarios consommés : elapsed_s est mesuré à " +
    "l'ÉCRITURE, pas un compteur vivant. Le fichier ne porte pas de session_token : " +
    "ce verdict peut venir d'une session précédente (caveat _drive/README.md).";
}

function renderScenario(scn, fileState, ready) {
  els["c-note"].hidden = true;
  const st = fileState.kind === "ok" ? scenarioState(scn, ready)
    : fileState.kind === "absent" ? "absent"
    : fileState.kind === "invalid" ? "invalid" : "error";
  const stateLabel = {
    "absent": "aucun scénario en attente",
    "invalid": "— (JSON invalide)",
    "error": "— (erreur de lecture)",
    "pending-no-session": "EN ATTENTE — mais AUCUNE session vivante",
    "pending": "EN ATTENTE",
    "consumed": "consommée (seq ≤ last_seq)",
  }[st];
  set("c-state", stateLabel);
  if (fileState.kind !== "ok") {
    ["c-seq", "c-action", "c-module", "c-button", "c-expect"].forEach((id) => set(id, "—"));
    return;
  }
  set("c-seq", fmtNum(scn.seq));
  set("c-action", fmtNum(scn.action));
  set("c-module", fmtNum(scn.module));
  set("c-button", fmtNum(scn.button));
  set("c-expect", fmtNum(scn.expect));
  if (st === "pending-no-session") {
    els["c-note"].hidden = false;
    els["c-note"].textContent =
      "Sans ready.json frais, le contrat interdit d'appliquer un scénario : " +
      "il resterait sans destinataire (ne pas inventer de session).";
  }
}

/* --- Boucle de sondage -------------------------------------------------------- */
const params = readParams(location.search);
const baseUrl = dirBaseUrl(params.dir);
const URL_READY = new URL("ready.json", baseUrl).href;
const URL_RESULT = new URL("result.json", baseUrl).href;
const URL_SCENARIO = new URL("scenario.json", baseUrl).href;

function nowMs() { return params.nowFixed ? Date.parse(params.nowFixed) : Date.now(); }

set("f-dir", decodeURIComponent(baseUrl.pathname));
set("f-clock", params.nowFixed
  ? "FIGÉE sur " + params.nowFixed + " (?now=)"
  : "horloge locale du navigateur");
if (params.dirInvalid) {
  els["poll-error"].textContent = "?dir= refusé (chemin non relatif) — défaut appliqué.";
}

let pollCount = 0;

async function poll() {
  const [stReady, stResult, stScenario] = await Promise.all([
    fetchDriveFile(URL_READY),
    fetchDriveFile(URL_RESULT),
    fetchDriveFile(URL_SCENARIO),
  ]);

  const ready = stReady.kind === "ok" ? stReady.data : null;
  const result = stResult.kind === "ok" ? stResult.data : null;

  // Le bandeau reflète d'abord la lisibilité de ready.json (LE fichier de vie),
  // puis la classification contrat. Un problème de lecture n'est PAS une absence.
  let state;
  if (stReady.kind === "error") state = "error";
  else if (stReady.kind === "invalid") state = "invalid";
  else state = classifyState(ready, result, nowMs());

  renderBanner(state, ready,
    ready ? hbAgeS(ready.hb_at, nowMs()) : null,
    ready ? hbTimeoutS(ready) : null);

  if (state === "alive" || state === "flight" || state === "stale") {
    const age = hbAgeS(ready.hb_at, nowMs());
    if (age !== null && age < 0) {
      els["banner-why"].textContent +=
        " (âge négatif : horloge client en retard sur hb_at — biais d'horloge.)";
    }
  }

  if ([stReady, stResult, stScenario].every((f) => f.kind === "error")) {
    els["poll-error"].textContent = "fetch en échec sur les 3 fichiers — serveur local down ?";
  } else if (!params.dirInvalid) {
    els["poll-error"].textContent = "";
  }

  renderSession(ready, stReady);
  renderResult(result, stResult);
  renderScenario(stScenario.kind === "ok" ? stScenario.data : null, stScenario, ready);

  els["f-poll"].textContent = new Date().toLocaleTimeString() +
    " (sondage n°" + (++pollCount) + ")";
}

poll();
setInterval(poll, POLL_INTERVAL_MS);

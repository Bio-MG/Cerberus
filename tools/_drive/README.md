# tools/_drive/ — runtime IPC, gitignored

This directory is the drop-box of the **Live Control Protocol**
(`docs/DRIVE_LIVE_CONTROL_PLAN.md`). It is a *runtime* channel between an
agent and an already-running Shiny session — **not** source code.

Everything here except this `README.md` is gitignored (`tools/_drive/*.json`,
`tools/_drive/*.tmp`). The directory itself is tracked because the protocol
needs it to exist, and because this note is the only contract a future reader
gets without opening the plan document.

## Files

| File | Writer | Meaning |
|---|---|---|
| `ready.json` | the app | A session is listening. Removed on session end. Carries a **heartbeat**. |
| `arm.json` | the agent | Arms the poller. Delete it (or set `armed:false`) to disarm. |
| `scenario.json` | the agent | One action to apply. Atomic writes only. |
| `scenario.tmp` | the agent | Staging file — never parsed by the app. |
| `result.json` | the app | The verdict for one `seq`. **Read this, never stdout.** |
| `result.tmp` | the app | Staging file. |

## Why the verdict is a file

`Rscript` exits with code **139** (a teardown segfault) even on success, and
stdout is buffered and lost when that happens. Any protocol whose verdict lived
on stdout would be unreadable exactly when it matters. `result.json` is
therefore the only source of truth.

## Heartbeat — telling a live session from a stale file

A `ready.json` left behind by a process that died mid-session is
**byte-identical** to a live one if you only check that the file exists. Three
fields settle it:

| Field | Meaning |
|---|---|
| `hb_at` | ISO-8601 UTC timestamp of the last beat. Moves every ~3 s while armed. |
| `hb_n` | Monotonically increasing beat counter. **0 at boot, never restarts** mid-session. |
| `hb_timeout_s` | The window the app itself considers fresh (default **15 s**). |

Rules of use:

- **Reject any `ready.json` whose `hb_at` is older than `hb_timeout_s`.** Then
  the session is gone and a scenario written for it would be applied to nothing.
- `hb_n` is what distinguishes *the same session still alive* from *a new session
  that reused the pid*. Compare it across reads; it must never decrease.
- The heartbeat is written **by the Shiny session**, never by the agent, and
  stops advancing when the session ends or is disarmed.
- Disarming does **not** delete `ready.json`. `armed:false` plus a frozen `hb_n`
  says "this session exists but is no longer driven", and keeping the file
  preserves the token needed to re-arm the **same** session without a restart.

### Timeout configuration

`hb_timeout_s` and the interval are read, in order, from:

1. `getOption("ts.drive.hb_timeout")` / `getOption("ts.drive.hb_interval")`, then
2. `TRANSCRIPTO_DEV_DRIVE_HB_TIMEOUT` / `TRANSCRIPTO_DEV_DRIVE_HB_INTERVAL`.

They are **never** read from a scenario payload — a scenario that could widen
its own liveness window would let a stale file look alive, which is the exact
failure the heartbeat exists to prevent.

## Badge — the dev-only status readout

When the protocol is on, `app.R` renders a small passive badge (bottom-right)
driven by a dedicated reactive value, `drive_state`. It is **observational
only**: it never triggers a pipeline, never touches `input`, never clears data,
and never changes navigation.

It appears **only** when all of these hold: `TRANSCRIPTO_DEV_DRIVE=1`, the
session is live, it is the session named in `ready.json`, and it is armed. In
every other case `renderUI()` returns `NULL` and no element exists — the app is
indistinguishable from the protocol being absent.

It shows: status (`armed` / `running` / `done` / `error`), acknowledged `seq`,
module, action, elapsed time, and one short sanitized error. It never shows a
token, an absolute path, a scenario payload, biological data, an object, or a
log. The projection is a **whitelist**, so a newly added field cannot leak by
accident. `invalid` and `ignored` deliberately stay on `armed`: a refused
payload is not an app failure, and red must keep meaning red.

### A defect this badge had: it redacted its own labels

`ts_drive_badge_sanitize()` redacts absolute paths **and any 8+-character
alphanumeric run** (its token heuristic). MEASURED (2026-09-23): `"snapshot"` is
**exactly 8 characters**, so the badge rendered
`drive: done · seq 2 · bulk_de · <redacted> · 0.1s` — and the blast radius was
**5 of 6 actions and 3 of 4 modules**. The projection was right; the *labels*
were what its own heuristic caught.

Fixed with a `known` set: a member of a FROZEN set (`TS_DRIVE_MODULES`,
`TS_DRIVE_ACTIONS`) is returned **verbatim**, *before* the path/token rules.
Deliberately **not** applied to `error` — that is the one free-form field, and
the one place where a leaked token would actually travel. Verified live after
the fix: `drive: done · seq 2 · bulk_filter · snapshot · 0.1s`.

## Performance contract — what the watcher and the badge may NOT do

A visible browser is acceptable for development and is **not** optimised
prematurely: the cost that matters is the biological computation, not a passive
badge or a low-frequency watcher. What the drive layer may not do is *add* to it.
Every clause below is pinned by a test, and each scan carries a **positive
control** (a file that DOES call a pipeline) so a green result means "the
invariant holds", not "the scan looked at nothing".

| Clause | How it is enforced |
|---|---|
| never inspect a Seurat object or a matrix | scan of `R/core/drive_*.R`: no `Seurat::`, no `as.matrix(`, no `@meta.data`, no `GetAssayData` |
| never run a pipeline | same scan: no `run_bulk_*`, no `DESeq`, no `limma`, no `gsva` |
| never invalidate heavy reactives | no `reactiveVal(` / `reactiveValues(` outside the state layer (convention C2), no `observe(` / `observeEvent(` in `R/` |
| never block the Shiny event loop | the only blocking primitive is the **bounded** write backoff, and its bound is a NAMED constant (`TS_DRIVE_WRITE_ATTEMPTS`, `TS_DRIVE_WRITE_BUDGET_S`) |
| no unbounded loop | the watcher runs one `invalidateLater()` beat; no `while (TRUE)`, no `repeat` |
| long computations use the existing async layer | the module declares `long = TRUE`; the job contract (above) is what makes it **observable** — the async migration itself is the module's business |

⚠️ A naive `grep` for these words is **not** the test, and would be wrong three
times over: two hits are frozen `note` strings and one is a comment quoting the
spec. The invariant is checked on the **parse tree**, at call level.

## Minimal agent loop

1. Wait for a `ready.json` that is **fresh** (see the heartbeat rules above). A
   missing or stale file means **no live client** — do not invent a session,
   ask the human to open the app.
2. Write `arm.json` with that `session_token`.
3. Write `scenario.json` with `seq: 1`, `action: "snapshot"`.
4. Poll `result.json` until `ack_seq == seq` and `status` is terminal, or
   `timeout_s` elapses. **`ack_seq` is the acknowledgement** — match it against
   the `seq` submitted.
   The six statuses (spec §2.4) split into **acknowledgements** and **terminals**:
   - `applied` — inputs updated, **pipeline not finished** → keep polling
   - `running` — a job has started → keep polling
   - `done` / `error` — **terminal** for that `seq`
   - `ignored` / `invalid` — **terminal refusals** (nothing happened, nothing will)

   So `set_inputs` acknowledging with `applied` is **not** completion; a
   `run_pipeline` reports `done` (or `error`). An **unknown** status must be
   treated as **not terminal** — keep waiting rather than assume completion.
5. Two consecutive scenarios must **both** be processed; the second is never
   lost. Always increment `seq` (a `seq <= last_seq` is ignored by design).
6. Repeat with `seq: 2, 3, …` to modify inputs and re-test **without**
   restarting the app.
7. Disarm by removing `arm.json`.

## Long jobs — the job-state contract

A `run_pipeline` whose module declares the job **long** must not be left as a
badge-only state. The contract is explicit and testable, and it comes in two
shapes:

- **A. `accepted -> running -> done | error`** — the job is declared `long`, so
  the tick answers **`running`** immediately (the token has moved; the analysis
  has **not** finished) and `result.json` carries that `running` **on the wire**.
  The module then DECLARES the outcome when its own observer ends, and the
  **next** tick writes the terminal.
- **B. `accepted -> invalid`** — a second `run_pipeline` while a job is already
  in flight is **REFUSED, not queued**, and the message names the module, `seq`
  and start time of the job in flight.

`running` is a real producer, not a badge transition: the badge table already
mapped `running`; the producer was what was missing.

| Symbol | Role |
|---|---|
| `ts_drive_job_state()` / `ts_drive_job_busy()` | the in-flight job, or `NULL` |
| `ts_drive_job_begin(seq, module, action, button)` | the TICK records a dispatch |
| `ts_drive_job_finish(button, status, error)` | the MODULE declares the terminal |
| `ts_drive_job_clear()` | forget it, once the terminal is ON THE WIRE |
| `ts_drive_entry_long(entry)` | a module's `long = TRUE` declaration; **fails CLOSED** |

Rules that keep it honest:

- **The declaration must be the job in flight AND terminal.** A `button` that is
  not the one in flight is refused (two tabs must not close each other's job),
  and a non-terminal status (`applied` / `running`) is refused — a module
  declaring its own job "still running" would leave an agent polling for a
  completion that will never be written.
- **Only the module can declare.** The watcher may not run a pipeline, so it may
  not guess that one finished.
- **The terminal is resolved BEFORE the arm gate**, and consumes nothing else, so
  a queued scenario cannot overwrite `result.json` in the same beat.
- **A failed terminal WRITE keeps the job pending**, so the next beat retries
  instead of losing the outcome.
- An **undeclared** button keeps the documented `done` semantics ("the token
  moved"), unchanged.

### RESIDUAL — measured, and deliberately not masked

The long computation is still **synchronous**, so it blocks the Shiny event loop
for its whole duration. No tick runs meanwhile, so `ready.json`'s heartbeat
stalls and `ts_drive_ready_fresh()` (default 15 s) goes **FALSE** for the rest of
the job. `result.json` carrying `running` is therefore **also** the proof of life
the heartbeat cannot give during that window: it is what separates "alive and
busy" from "dead".

**Consequence for an agent, and it is not optional:** for a long job, do **not**
re-read a static `result.json` forever — it changes only when a scenario is
consumed, so a loop that re-reads it is *guaranteed* to time out. Issue a
**fresh `snapshot` scenario per poll** and read the field you care about from the
answer. Restoring the heartbeat needs the computation itself to stop blocking,
i.e. to go through the existing asynchronous layer (`run_job()`,
`R/core/jobs.R`) — a change to the **module**, not to this protocol.

MEASURED, and it is why the contract exists: `bulk_de` on the real dataset
answered `done` in **2.1 s**, and the contrast was first observable **852.7 s**
later. Without a published `running` state, that whole 14-minute window is
indistinguishable from a run that never started — the observer's opening
`req(...)` aborts in **silence**.

## Two visibility modes, ONE protocol

The protocol has exactly **two supported visibility modes**. They differ on a
**single axis** — whether a browser window is visible — and share *everything*
else: the same `ready.json`, `arm.json`, `scenario.json` and `result.json`, the
same session-selection and heartbeat rules, the same passive badge. **There is
deliberately no visible-mode protocol.** Only the client's visibility changes;
the IPC contract is identical.

| | Mode 1 — `headless` (default) | Mode 2 — `visible` |
|---|---|---|
| `launch.browser` | `FALSE` | `TRUE` |
| Client | a real httpuv server + a real chromote client | RStudio Viewer, or a visible localhost Chrome/Edge tab |
| Window | none | visible |
| `ready.json.viewer` | `"headless"` | `"visible"` |
| Use | reproducible automated tests, long unattended runs | interactive development, human-in-the-loop |

`ready.json`'s `viewer` field is the **only** thing that differs on the wire, and
it is PUBLISHED so an agent never has to guess whether a human is watching. The
resolution order is **declared beats derived, derived beats guessing**:

1. `getOption("ts.drive.viewer")`, then
2. `TRANSCRIPTO_DEV_DRIVE_VIEWER` (set by the launcher), then
3. `interactive()` — TRUE → `"visible"`, FALSE → `"headless"`.

A declaration outside `TS_DRIVE_VIEWERS` (`headless`, `visible`, `unknown`) is
reported as `"unknown"`, **never coerced**: telling an agent "headless" while a
human is watching would be worse than telling it nothing. The field is **not**
readable from a scenario payload — a scenario that could relabel its own session
would make the field worthless.

### Launching a drive-enabled session

```bash
Rscript tools/launch_dev_drive.R              # Mode 1 "headless" (default)
Rscript tools/launch_dev_drive.R --visible    # Mode 2 "visible"
Rscript tools/launch_dev_drive.R --print      # prepare env, do NOT start
```

Unknown flags, and `--headless` together with `--visible`, are rejected with
`stop()` rather than ignored: a silently mis-parsed mode is a session whose
`viewer` field lies.

The launcher sets, **for that process only** (never in a committed `.Renviron`):

- `TRANSCRIPTO_DEV_DRIVE=1` — enables the wildcard arm token `"*"`;
- `TRANSCRIPTO_DEV_DRIVE_VIEWER=<mode>` — declares the visibility mode;
- the heartbeat knobs, published in `ready.json` so the agent reads them rather
  than hardcoding;
- `options(ts.drive.interactive = TRUE)` — **the arm gate**. See below.

RStudio's **Run App** button does **not** go through this script, so the
wildcard is unavailable there: arm with the real token printed on the console
(`session_token=...`). That safer default is why the console line exists. Run
App is nevertheless a supported **Mode 2** session: nobody declared the mode, so
`ts_drive_viewer()` derives it from `interactive()` — TRUE there — and
`ready.json` still reports `viewer: "visible"`.

### ⚠️ Opening a visible tab ROTATES the token — never reuse an old one

A new tab creates a **new session**, and the last connected session wins: it
**rewrites `ready.json` with a new `session_token`**. So the agent must:

1. launch with `launch.browser = TRUE`;
2. let the Viewer/tab open and **connect**;
3. **re-read the FRESH `ready.json`** — the handshake is written from `server()`,
   and Shiny creates a session only when a client connects, so a `ready.json`
   read *before* the tab existed describes a different (or no) session;
4. arm with the token from **that** read.

Arming with a stale token is **refused**, and the refusal is indistinguishable
from "nobody armed me" — the exact silent seam this rule closes. The rule is not
"read `ready.json` once": it is **read it again after every new client**.

### The arm gate must be OPEN, or the launcher produces an INERT app

`ts_drive_interactive()` is `isTRUE(interactive())` unless overridden, and
**`Rscript` is never interactive**. MEASURED (2026-09-23): without the launcher's
`options(ts.drive.interactive = TRUE)`, the poller's observer returned on its
**first statement**, so `invalidateLater()` was never called and the arm gate
answered "non-interactive session" to every request. The app listened on
127.0.0.1:7067 and logged **no** `session_token=` — perfectly healthy-looking,
permanently inert, and with nothing in the log saying why. An agent sees a
handshake frozen at attach time and cannot tell it from a dead session.

The gate is set in the **launcher**, not in `app.R`, for the same reason as
`TRANSCRIPTO_DEV_DRIVE=1`: it exists to keep PRODUCTION inert, and production
never runs this script.

### Mode 2 smoke acceptance — 8 steps, PASSED

Run on a real `--visible` session (`launch.browser = TRUE`, real localhost tab):

| # | Step | Result |
|---|---|---|
| 1 | launch with `launch.browser = TRUE` | ✅ port up, no `--headless` |
| 2 | open the Viewer / localhost tab | ✅ real client connected |
| 3 | read the FRESH `ready.json` | ✅ `viewer: "visible"`, `session_token: "suwuyn5z"` |
| 4 | badge visible after arming | ✅ DOM `ts-drive-badge-armed`, text `drive: armed`; `hb_n` climbed 1 → 11 |
| 5 | run `noop` and `snapshot` | ✅ `done` (~2 s); the snapshot carried the full `modules.bulk_de` / `modules.bulk_filter` payload |
| 6 | the SAME session is controlled, without reload | ✅ session marker `p-nafpibzqhs` unchanged **and** token unchanged |
| 7 | close the visible tab | ✅ `ready.json` deleted within ~1 s |
| 8 | heartbeat / session invalidation | ✅ file gone ⇒ no live client; a later arm is refused |

Step 6 is the load-bearing one: marker *and* token both surviving proves the
scenario was applied to the tab that was already open, i.e. **no reload**.

## Two hard limits

- A session started **before** this code existed cannot be armed — the poller
  is not in it. Stop + Run App once; after that, arming never needs a restart.
- Editing R **source** still requires Stop + Run App. The drive controls
  UI/state, not the interpreter.

## Data safety

`preserve_data` defaults to `true`. Disarming does **not** unload anything, and
only an explicit `reset_module` clears a module's objects. Silent wipes are
treated as a bug, not a convenience.

## Validation status and documentation scope

**Validated so far — grade G0 + G1, on a REAL Shiny session** (headless Chrome
via `chromote`, app started with `tools/launch_dev_drive.R`, port 7789):

| Acceptance | Result on the live session |
|---|---|
| `ready.json` handshake (protocol, pid, port, token, `started_at`, heartbeat) | valid |
| arm / disarm | `armed:true` accepted with the exact token; `armed:false` disarms |
| passive badge | `drive: armed` on arm, `<ABSENT>` while unarmed or disarmed, **no reload** |
| heartbeat freshness | `hb_n` monotonic, token/pid/`started_at` preserved; stale file rejected |
| `noop` | `ack_seq` matched, `status: done` |
| `snapshot` | `ack_seq` matched, `status: done` |
| `set_inputs` | `ack_seq` matched, `status: applied` (inputs set, pipeline NOT run) |
| sequence handling | two consecutive scenarios both processed, none lost |
| session invalidation | closing the tab removes `ready.json` |
| stale-token rejection | a `ready.json` older than `hb_timeout_s` is refused |

## Grade G2 — the four bulk buttons, on the live session

**Validated on a REAL session** (same harness: `chromote` client, port 7789,
`tools/launch_dev_drive.R`):

| # | Acceptance | Result on the live session |
|---|---|---|
| 1 | the real bulk buttons are BOUND on a live session | ✅ all four answer. The refusal names the button **and** the missing object, which only a *bound* button can do |
| 2 | `preserve_data` defaults to `true` | ✅ `preserve_data: true` echoed on every `result.json` |
| 3 | two consecutive `run_pipeline` both fire | ⚠️ both are **consumed and acked** (`ack_seq` 3 then 4, second not lost); the *firing* case needs a preloaded object — see below |
| 4 | no object loaded ⇒ `invalid`, explicitly | ✅ `invalid` + `button '<id>' is bound but not ready: no bulk object loaded (shared_rv$filtered_counts is NULL)` |
| 5 | `fileInput` is never faked | ✅ no `fileInput` id is on the allowlist; the import guard reads the WIDGET, never a path |
| 6 | G3 not started | ✅ `import_file` / `reset_module` still return `invalid` — **true at G2 time**; `import_file` is implemented in G3 (see the G3 sections below) |

Observed refusals, per click site, on a session with nothing loaded:

| button | refusal |
|---|---|
| `bulk-de-run_de` | `not ready: no bulk object loaded (shared_rv$filtered_counts is NULL)` |
| `bulk-pathways-run_pathway` | `not ready: no bulk object loaded (shared_rv$filtered_counts is NULL)` |
| `bulk-pathways-run_scores` | `not ready: Step 1 has not produced a VST matrix (shared_rv$vst_mat is NULL)` |
| `import_bulk-btn_load` | `not ready: no counts file selected (fileInput `counts_file` is empty)` |

### Two defects the G2 acceptance run exposed

Both were **silent** — no error, no warning — and both were invisible to the
offline suite as it stood. Each now has a durable test.

1. **The registry was never populated.** `ts_drive_publish_token()` read
   `global_data$drive_registry` with a bare `$`, and Shiny ABORTS a
   `reactiveValues` field read outside a reactive consumer — which is exactly
   what module init is:
   `Can't access reactive value 'drive_registry' outside of reactive consumer.`
   A defensive `tryCatch(..., error = function(e) NULL)` turned that abort into
   a silent `NULL`, so every button reported **"not bound"** on a live session
   while the source-level wiring looked perfect. That is the G0/G1 live finding,
   explained. The read now goes through `ts_drive_registry()`, which wraps it in
   `shiny::isolate()`.
2. **The validator dropped `button` and `expect`.** `ts_drive_validate_scenario()`
   rebuilds the scenario from a field whitelist, and that whitelist omitted both
   — so `scn$button` was always `NULL`, `ts_drive_apply()` always fell back to the
   module default, and `bulk-pathways-run_scores` was **unreachable**
   (`bulk_pathways` defaults to `run_pathway`). A scenario naming `run_scores`
   came back for `run_pathway`. Found on the live session, not by a test.

### Not yet validated — still a G2/G3 acceptance, not a G2 one

- **Real bulk execution against a PRELOADED object** (`result.status: done` for
  a `run_pipeline` that actually computed something). On a session with nothing
  loaded every `run_pipeline` is now refused *explicitly* — which is the honest
  answer, and not the same thing as a successful run. It needs a human or the
  G3 `import_file` route to put an object in memory first.
- `import_file` and `reset_module` return **`invalid`** by design in this grade
  (spec §G3.14: not-implemented must return `invalid`, never silently no-op).
  ⚠️ **Superseded for `import_file`** — implemented in G3, see below.

## Grade G3 (part 1/3) — `import_file` on a live session

**Validated on a REAL session** (port 7790, `chromote` client, dataset
`D:/Data_science/Données/bulk/GSE164073_Eye_count_matrix.csv`, which is a
DIRECTORY holding the counts CSV and the metadata CSV):

| # | Acceptance | Result on the live session |
|---|---|---|
| 11 | agent loads a real matrix; `snapshot` reports it | ✅ `done` (terminal) in **3.1 s**, `has_data=TRUE`, **17925 × 18** |
| 13 | `preserve_data=true` + `set_inputs` does not drop it | ✅ `applied`, matrix intact |
| 14 | `reset_module` returns `invalid` | ✅ never a silent no-op |

## Grade G3 (part 2/3) — Step 1 Filtering & VST, on a live session

Bulk is a **staged** workflow: `import_file → global_data$bulk_obj → Step 1
Filtering & VST → shared_rv$filtered_counts / $dds_blind / $vst_mat → design &
contrasts → DE`. Step 1 is the ONLY producer of `shared_rv$filtered_counts` a
scenario can reach, and every downstream panel gates on it — so before this
milestone, `run_pipeline` on DE was **bound, correctly guarded, and
unreachable**.

**One fresh session**, `seq` 1 → 10, all `ack_seq` matched:

| # | Step | Result |
|---|---|---|
| 1 | `import_file` | `done` 3.1 s, `has_data=TRUE`, **17925 × 18** |
| 2 | `snapshot` BEFORE Step 1 | `modules` published = **1**; `bulk_filter$filtered_counts` = **NULL** (absence is reported, not implied) |
| 3 | `set_inputs` Step 1 params (10 / 1 / 1) | `applied`, data preserved |
| 4 | `run_pipeline` **`bulk_filter`** (the real button) | `done`, token fired in **1.1 s** |
| 5 | `snapshot` | **`filtered_counts` 17925 × 18**, **`vst_mat` 17925 × 18**, 18 sample names (all unique, first: `MW1_cornea_mock_1`), imported object **still available** |
| 6–7 | **second** Step 1, same params | `done`; **17925 × 18** unchanged (idempotent), 18 samples, object intact |
| 8–10 | `set_inputs` `min_count=100` then Step 1 again | `done`; **`filtered_counts` 14658 × 18** — the count **DROPPED**, which is what proves `set_inputs` really reaches the filter |

⚠️ **Why steps 8–10 are not decoration.** With 10 / 1 / 1 alone, a `set_inputs`
that silently failed would be **invisible**: the widgets already default to
10 / 1 / 1, so the run yields 17925 either way. The higher threshold is the only
thing that distinguishes "injected" from "never arrived".

⚠️ **`17925` coincides with the import's own `min_counts = 10` pre-filter**, so
it is NOT by itself evidence that Step 1 re-filtered. The `100 → 14658` run is.

**The id is `bulk-filter-run_filter_norm`, NOT `bulk-run_filter_norm`.** The
filter is a NESTED module (`mod_bulk.R:34`, `mod_bulk.R:533`), and the id was
MEASURED from the running application by querying the document for `*[id]` —
the obvious source-only reading is **absent** from the DOM.

### `snapshot.modules` — additive, and why it exists

`has_data` / `n_genes` / `n_samples` read `global_data$bulk_obj`, which reports
the **IMPORTED** dimensions before AND after Step 1. So the frozen fields cannot
tell "Step 1 ran" from "Step 1 never ran", and an agent polling them would wait
forever on a stage that had already finished. A module may now publish a `state`
probe; the snapshot surfaces it under `modules[[module]]`. A probe that throws
becomes `modules[[module]]$probe_error` — **never** silently omitted, so "no
state" and "probe crashed" stay distinguishable.

**Not claimed:** DE has NOT been driven. `run_pipeline` on `bulk_de` still
requires the design/contrast inputs, which are their own milestone. A DE
refusal immediately after `import_file` is **expected**, not a protocol bug.

### The FIRST-ARM symptom was the app's SLOW BOOT, not a lost write

Measured on a fresh session: the **first** `arm.json` after session start is
honoured (scenarios are processed) but `ready.json` is **not** rewritten for a
while, so it keeps `armed:false, hb_n:0` and goes **stale** after
`hb_timeout_s`. Re-arming the **same** session recovers it.

**The G2 write-up blamed a lost first write. That was WRONG, and the corrected
measurement is below.** `ts_drive_attach()` runs early in `server()`, but Shiny
does **not** start a session's reactive flush until `server()` **returns** — and
this app's `server()` does heavy init (spatial `mirai` daemons, plotly). No
protocol beat had run, so `ready.json` was still the file written at attach
time. Arming early is not a bug: `arm.json` persists and the first beat honours
it.

Measured on a **clean session** (fresh process, 2026-09-22), arming exactly once
and never re-arming:

| Observation | Value |
|---|---|
| `ready.json` appeared after | 1.0 s |
| arm.json writes in the run | **1** |
| delay from that single arm to `armed:true` | **22.4 s** |
| `hb_n` after that single arm | **1** |
| write failures logged by the app | **0** |
| `hb_n` over the next 15 s | 1 → 2 → 2 → 3 → 4 → 5 (monotone, never restarts) |

⇒ The arm was honoured; it simply waited for `server()` to finish. **A stale
`ready.json` immediately after session start is not evidence of a lost write.**
The `hb_n` sequence shows one repeated sample: the 3 s sampling and the 3 s
cadence drift against each other, and **zero** write failures were logged, so no
beat was lost.

### Three REAL write-path defects, found while chasing that symptom

None of these caused the symptom above — each was measured on its own — but all
three were genuine and are now fixed:

1. **The throttle advanced on ATTEMPT, not on SUCCESS.** The guard was
   `!inherits(wrote, "try-error")` around a function that **returns** its payload
   instead of throwing, so it was **always true**: any failed write would have
   silenced the heartbeat for a whole interval and left a stale-but-plausible
   handshake. The rule is now the pure function
   `ts_drive_hb_next_at(now, hb_at, wrote_ok)`; a failed write leaves `hb_at`
   alone, so the next beat (800 ms) retries instead of waiting out the interval.
   `hb_n` is also rolled back, so the counter the agent reads mirrors what is on
   **disk** and stays gap-free.
2. **A failed write was invisible.** `ts_drive_write_json()` now records a
   **sanitized** failure (`ts_drive_last_write_error()`), exposed on the wire as
   `result.json.write_error` — a different channel, because `ready.json` is
   itself a file that can fail to be written. The session-start write is checked
   too, and every failure is logged rather than swallowed.
3. **A doomed write stalled the app for 41.4 s.** MEASURED: `file.rename()` onto
   a destination that cannot be replaced (a directory, a read-only file, a file
   held open) takes **~5.1 s to fail** on this host, so an 8-attempt budget
   parked the Shiny observer for **41.4 s** inside a single beat. Two bounds now:
   a cheap **pre-flight** (`unlink()` tells us in microseconds what the rename
   would take 5.1 s to say) and a **wall-clock budget** beside the attempt cap.

**The fix is validated LIVE, by deliberate injection** (not by waiting for a
failure to happen): with a real session armed and `hb_n` climbing, `ready.json`
was replaced by a **directory** so every write to it had to fail. Measured:

| Observation | Value |
|---|---|
| new `[drive] ready.json write FAILED` lines | **4** (before this change: silent) |
| first failure line | `… target exists and cannot be removed (locked or read-only)` |
| delay from de-obstruction to repaired handshake | **1.0 s** (one beat) |
| re-arms needed | **0** |
| `hb_n` across the failure | 5 → 6 (no gap, no restart) |
| `*.tmp` leftovers | none |

⇒ A failed write is now loud, and the handshake repairs itself on the next beat
instead of waiting out an interval.

### A FOURTH write-path defect — the handler closed a connection it did not own

Found 2026-09-23 (`b455da0`), by accident and only because the **full** suite was
re-measured. It is the most expensive of the four: it did not merely go unnoticed,
it **destroyed the instrument that would have noticed it**.

`ts_drive_write_attempt()`'s error handler ran `close(con)` **unguarded**. When
`file(tmp, open = "wb")` itself **throws** — the missing-parent destination the
writer tests already exercise — its own `con` was **never assigned**, so R
resolved `con` **lexically**, up through the writer's enclosing environments, and
closed an unrelated connection that merely shared the name.

**The victim was `tools/run_full_suite.R` itself**, which keeps its results
connection in a global called `con`. One failed write closed it, the next
`writeLines()` raised `invalid connection`, and the suite **aborted at file 56 of
137**. Measured, in a single-process reproduction of the harness call:

| Observation | Value |
|---|---|
| `showConnections()` before the drive test file | **4** (stdin, stdout, stderr, **and the caller's own**) |
| `showConnections()` after | **3** — the caller's slot is **gone** |
| `trace("close.connection")` stack | `test_that("a write that cannot land…")` → `ts_drive_write_json` → `ts_drive_write_attempt` → **the error handler** → `try(close(con))` |
| the drive test file's own verdict | **570 pass / 0 fail** — green throughout |

**Dating.** The handler came from `db0785e` (this protocol); the test that
**triggers the throw** came from `448209b` (the heartbeat fix). So **no full suite
reached a BILAN between `448209b` and `b455da0`**, while four G3 milestones were
declared green on the strength of RED→GREEN tests, a falsification, a clean guard
and a **filtered** drive suite at `820/820`.

⚠️ **Why the filtered suite could not see it.** It holds no long-lived
connection — so there is **no victim**, and therefore **no symptom**. Green, on a
population that was not the one breaking. Same shape as "`C9` only measures a
NAME", transposed from the guard to the harness.

**Fix.** `con <- NULL` is initialised in the attempt's own frame before the
opening, and the handler is guarded (`if (!is.null(con))`). The rule it encodes:
**the writer may only close what the writer opened.** Pinned by the test
"a failed write never closes a connection it does not own", which places a
connection named `con` in `globalenv()` on purpose — the only scope the writer's
lexical chain can reach, R being **lexically** scoped (a caller's *local* `con` is
invisible to it) — and restores any pre-existing binding rather than dropping it.
`isOpen()` is wrapped in `tryCatch`: it **throws** on a closed connection instead
of answering `FALSE`, so an unguarded call reports the defect as an opaque error
rather than as the assertion it is.

**Measured.** RED `fail=1 pass=571` (exactly the new assertion) → GREEN
`fail=0 pass=572`. Falsified: restoring the original handler reddens exactly that
assertion again and the caller's connection is lost; the fix was restored
byte-identically. Full suite, first **complete** run since `448209b`: **137/137
files**, `BILAN: failed=1 passed=7614 error=0 skipped=1` (24m 25s) — the single
remaining failure being a separate frozen-contract drift from `358a2be`.

### The wire-write contract (`ts_drive_write_json`)

One write = `payload` → **unique** `<dest>.<pid>.<n>.<tag>.tmp` → `flush` →
**close** → `unlink(dest)` → `file.rename` → **read back and compare**.

- **Unique tmp per attempt**, never the fixed `<dest>.tmp`: a fixed name is
  shared by every writer of that destination, so a leftover tmp from a killed
  process (or a second tab) makes two writers fight over one name and the loser's
  rename fails against a file the winner already moved.
- **Close before rename.** On Windows the rename fails with "utilisé par un autre
  processus" while the handle is open; an `on.exit(close(con))` runs *after* the
  rename (the first version did exactly that and every write failed silently).
- **Read-back verification.** A rename that reports success while the
  destination holds someone else's bytes is the silent failure this exists to
  stop, and only a read-back can see it. Without it, "written" means "we asked".
- **Bounded, and loud.** Attempts ≤ 8, wall-clock ≤ 1 s, and a failure is always
  recorded — never swallowed. A failed write must be treated as *not done*.
- **A reader can still see the file MISSING** for a moment: `unlink(dest)` then
  `file.rename()` has a window with no destination, and Windows offers no atomic
  replace. A reader must treat "absent" as "no handshake right now" — which is
  what `ts_drive_ready_fresh()` already does.

Note for readers of the failure messages: they pass through
`ts_drive_badge_sanitize()`, which redacts absolute paths **and any 8+-character
alphanumeric run** (its token heuristic). Messages are worded to survive it.

## Grade G3 (part 3/3) — the DE contrast, on a live session

Steps 6 and 7 of the staged workflow: design & contrast inputs, then the DE
action. **One fresh session** (fresh app process, drop dir deleted first),
`seq` 1 → 10, all `ack_seq` matched, dataset
`D:/Data_science/Données/bulk/GSE164073_Eye_count_matrix.csv`
(17 925 genes × 18 samples, 9 mock / 9 CoV2), engine **DESeq2**.

| # | Step | Result |
|---|---|---|
| 1 | `import_file` | `done` 3.1 s → **17925 × 18** |
| 2–3 | Step 1 (`bulk-filter-run_filter_norm`) | `filtered_counts` **17925 × 18**, `vst_mat` **17925 × 18**, 18 unique sample names |
| 4 | `snapshot` BEFORE any contrast | `modules$bulk_de` present; **`n_contrasts = 0`**, **`active_contrast = NULL`** — the probe says "no contrast yet" instead of leaving it to be inferred |
| 5 | `set_inputs` `bulk-de-condition_col = condition` | `applied` |
| 6 | `set_inputs` `group_ref = CoV2`, `group_target = mock`, `de_engine = deseq2` | `applied`; DOM readback returned **`CoV2` / `mock` / `deseq2`** |
| 7 | `run_pipeline` **`bulk_de`** | `done` in **2.1 s** — and the contrast was first observable **852.7 s** later |
| 8 | `snapshot` | **`n_contrasts = 1`**, **`active_contrast = mock_vs_CoV2`**, `n_genes = 17925`, `n_padj_finite = 14102`, `n_significant = 29`, `bypass = FALSE` |
| 9–10 | **second** DE run, same pair | `done`; `n_contrasts` stays **1** (same name overwritten), `n_genes` unchanged, imported object **intact**, 18 samples |

⚠️ **Why `done` in 2.1 s is the whole point.** `status: done` means the token
MOVED — that the module's `observeEvent` was triggered — **not** that the
analysis finished. Here it finished **14 minutes later**. Without a published
state, that entire window is indistinguishable from a run that never started,
because the observer's opening `req(shared_rv$filtered_counts, input$condition_col,
input$group_ref, input$group_target, input$de_engine)` aborts in **silence**.

⚠️ **`mock_vs_CoV2` is the INVERTED pair, and that is deliberate.** The module's
own observer defaults to ref = `lvls[1]` = `mock`, target = `lvls[2]` = `CoV2`,
so the auto-generated name `CoV2_vs_mock` appears with **no injection at all**.
The driver injected the pair reversed, so reading back `mock_vs_CoV2` proves
**both** `group_ref` and `group_target` took. This is why `bulk-de-contrast_name`
stays **absent from the allowlist**: the auto-name is the only end-to-end
self-check a scenario has, and a free-text name would overwrite that evidence.

⚠️ **`n_genes = 17925` equals Step 1's gene count**, so the DE ran on
`shared_rv$filtered_counts` — not on the imported matrix.

⚠️ **`n_padj_finite = 14102`** (of 17925) is what shows the model actually
fitted; `n_significant = 29` is computed under a FIXED, NAMED convention
(`padj < 0.05 & |log2FoldChange| > 1`) reported in the payload as `convention`,
**never** under the panel's live thresholds — those are inputs, and a probe that
borrowed them would describe the last click instead of the state.

### Two instruments that lied during this run (both nearly became false findings)

**1. Re-reading `result.json` is not polling.** The file is written when a
scenario is *consumed* and is static in between, so a wait loop that re-reads it
can never observe a transition — it is **guaranteed to time out**. It did, twice
(`filtered_counts appeared after (s) = TIMEOUT`, then 852.7 s of apparent
silence). The fix is to issue a **fresh `snapshot` scenario each poll** and read
the field you care about from the answer.

**2. A DOM option list measures the SELECTED value, not the choices.** Every
select in this app reported exactly one option — its selected value — because a
JS enhancement owns the real list. `bulk-de-condition_col` reported `["tissue"]`
for a column set that genuinely contained `condition`, and `group_ref` /
`group_target` reported one option each. That was one step from being filed as a
**product defect that does not exist**; an offline replication of the same import
path returned `cat_cols = tissue | condition`, and the injected values read back
correctly.

Trust instead: the element's **`.value` readback**, and the app's **derived
output** (the contrast name). Also: use `textContent`, not `innerText`, to read
a panel in an inactive tab — `innerText` returns `""` for `display:none`.

### Not validated at this grade

- **`edger` and `limma` were NOT driven** — only `deseq2`.
- **Pathways were NOT driven** (`bulk-pathways-run_pathway`,
  `bulk-pathways-run_scores` remain bound, guarded, and unexercised).
- The **padj-recompute path (STAT-Q1)** was NOT exercised.
- `result.json` carries **no session token**, so a driver matching on `ack_seq`
  alone can read a *previous* session's verdict. Clean the drop dir and use a
  fresh app process; this is a harness hazard, not a protocol verdict.

### Where the evidence lives (read this before citing a result)

`docs/` is **gitignored** in this repository, so:

- **Versioned (reproducible from the repo):** `R/core/drive_allowlist.R`,
  `R/core/drive_watcher.R`, `app.R`, the modules, `tests/testthat/test-drive-*.R`,
  and **this file**. The offline suite and the guard can be re-run from a clone.
- **Local-only (NOT reproducible from the repo):** `docs/STATUS.md` (§2di),
  `docs/DRIVE_LIVE_CONTROL_PLAN.md`, `.workbuddy-ai/memory/*`, and the raw
  evidence artefacts under `.workbuddy-ai/tmp/evidence/`. These are **working
  notes**, not part of the deliverable — a claim sourced only from them is not
  independently reproducible.

⚠️ **The harness client is not part of the contract.** The earlier grades above
record a `chromote` client; the Mode 2 smoke acceptance (2026-09-23) used a
**zero-dependency Node CDP driver** instead, because `chromote` 0.5.1 failed
deterministically on this host (`error_no_available_port`, then "processx
supervisor was not ready after 5 seconds", 4/4 attempts). The protocol is
indifferent to *which* client connects — `ready.json` appears when a client
CONNECTS — so a change of driver is not a change of protocol. Do not read a
grade's client as a property of the protocol.

The **live-session acceptance run is a one-shot observation**: it depends on a
real browser, a real httpuv session and a human or agent driving the files. The
**offline** suite is what a clone can reproduce.

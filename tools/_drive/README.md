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

## Launching a drive-enabled session

```bash
Rscript tools/launch_dev_drive.R
```

This sets `TRANSCRIPTO_DEV_DRIVE=1` for that process only (never committed to
`.Renviron`), which enables the wildcard arm token `"*"`. Start it, then open
the Viewer or the `http://127.0.0.1:<port>` tab.

RStudio's **Run App** button does **not** go through this script, so the
wildcard is unavailable there: arm with the real token printed on the console
(`session_token=...`). That safer default is why the console line exists.

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
| 6 | G3 not started | ✅ `import_file` / `reset_module` still return `invalid` |

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

### FIXED — heartbeat mirror on the FIRST arm

Measured on a fresh session: the **first** `arm.json` after session start was
honoured (scenarios were processed) but `ready.json` was **not** rewritten, so it
kept `armed:false, hb_n:0` and went **stale** after `hb_timeout_s`. Re-arming the
**same** session recovered it. The suspected cause was a Windows sharing
violation hidden by `try(..., silent = TRUE)`.

**The suspicion was half right, and the measurements changed the fix.** The
`try(..., silent = TRUE)` was indeed hiding it, but not the way it looked: the
guard was `!inherits(wrote, "try-error")` around a function that **returns** its
payload instead of throwing, so it was **always true** and the heartbeat throttle
advanced even when nothing had been written. A lost first write therefore
silenced the heartbeat for a whole interval and left a stale-but-plausible
handshake. Three defects, all now fixed:

1. **The throttle advanced on ATTEMPT, not on SUCCESS.** The rule is now the pure
   function `ts_drive_hb_next_at(now, hb_at, wrote_ok)`; a failed write leaves
   `hb_at` alone, so the next beat (800 ms) retries instead of waiting out the
   interval. `hb_n` is also rolled back, so the counter the agent reads mirrors
   what is on **disk** and stays gap-free.
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

Note for readers of the failure messages: they pass through
`ts_drive_badge_sanitize()`, which redacts absolute paths **and any 8+-character
alphanumeric run** (its token heuristic). Messages are worded to survive it.

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

The **live-session acceptance run is a one-shot observation**: it depends on a
real browser, a real httpuv session and a human or agent driving the files. The
**offline** suite is what a clone can reproduce.

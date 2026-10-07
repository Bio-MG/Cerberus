# Git reconciliation plan — porting the MCP line onto `origin/main`

**Date**: 2026-10-06 · **Mode**: READ-ONLY PLANNING. Nothing was created, moved,
cherry-picked, reset, pushed or force-pushed. **A/B/C is deliberately not
chosen** — this document is the input to that decision.

Companion: `GIT_TOPOLOGY_FORENSIC_2026-10-06.md` (the measurements).
Every number below is measured from the object database.

---

## 0. The fact that makes this plan cheap

| Comparison | Result |
|---|---|
| `origin/main` tree | `4f97c61be60be6bd78eb90ae0593fd99f9b44089` (376 files) |
| `93cbecb` tree (squash root) | `700d1f8e437841595b755a73167ef9c50bf51655` (376 files) |
| shared paths | **376 / 376** |
| identical blobs | **374** |
| differing blobs | **2** — `scripts/mcp_server.R`, `tests/testthat/test-mcp-sc-local.R` |

Those 2 files are exactly the ones `93cbecb` ("mcp-S0") itself modified.

🔑 **Therefore `93cbecb` == `origin/main` + the S0 change, and nothing else.**
The "rewrite" was a *history* operation, not a *content* operation: the squash
captured `origin/main`'s tree faithfully and then the MCP work proceeded on top.

**Consequence — the port is verifiable by tree hash, not by eyeballing diffs:**

```
after replaying S0 :  tree must equal  700d1f8e437841595b755a73167ef9c50bf51655
after replaying all:  tree must equal  c6f07bf21f69f1fd4f3ced004765226d5f3a77da  (= main's tree)
```

If the final tree matches, the port is **byte-exact** — no file-by-file review
needed to trust it. This is the single most important property of the plan.

---

## 1. How to create a branch from `origin/main` without changing any existing ref

Three mechanisms, in increasing strictness. **All three leave the 7 existing
refs (`main`, `origin/main`, `origin/HEAD`, `mcp-complete`,
`origin/mcp-complete`, `backup-pre-rewrite`, `backup-pre-squash`) byte-identical.**

| # | Command | What it creates | Existing refs |
|---|---|---|---|
| 1 | `git branch <new> origin/main` | a **new** ref `refs/heads/<new>`; HEAD untouched | untouched |
| 2 | `git switch -c <new> origin/main` | new ref + HEAD moves + working tree updates | untouched |
| 3 | `git worktree add --detach <path> origin/main` | **no branch ref at all**; a detached worktree | untouched |

Recommended for this reconciliation: **#1 then #2**, because a named branch is
easier to audit than a detached HEAD, and the name is self-documenting
(suggested: `recon/mcp-on-published`).

Why these are safe, precisely:

- `git branch` / `git switch -c` write **one new ref** and (for switch) `HEAD`.
  Neither rewrites, deletes nor repoints any pre-existing ref. The reflog of
  `main` is not touched.
- They do **not** fetch. `origin/main` is used exactly as the locally cached
  remote-tracking ref (`df3369d`), so no network and no `FETCH_HEAD` change.
- ⚠️ They **do** add a ref / worktree entry to *this* repository. If even that is
  unwanted, do it in a **scratch clone** (§5, Stage 0) and the source repository
  stays literally untouched — which is the recommended route anyway.

**Post-condition check (must be run after any of the three):**

```
git show-ref                      # the 7 SHAs above, unchanged, + the one new ref
git rev-parse HEAD                # still b494404 (or unchanged if #3)
git status --porcelain            # empty
```

---

## 2. Commit → files map (what each validated MCP commit needs)

Source range: `9345615..main` (17 commits) plus `93cbecb` (S0) as a patch.
`+/-` are measured `numstat` values.

### 2.1 Protocol / MCP production

| Commit | Subject | Files |
|---|---|---|
| `93cbecb` | mcp-S0 arming bootstrap | `scripts/mcp_server.R`, `tests/testthat/test-mcp-sc-local.R` **(the only 2 files that differ from `origin/main`)** |
| `9345615` | mcp-S1 `import` tool | `scripts/mcp_server.R` +380/-3 · `test-mcp-sc-local.R` +156/-3 · `test-mcp-spatial-local.R` +9/-7 |
| `7b20b10` | S2 export routes 2→9 (one atomic commit) | `R/core/drive_allowlist.R` +178/-19 · `app.R` +7 · `scripts/mcp_server.R` +195/-41 · **8 new `modules/**/*_export.R`** · 8 modified modules (+7..+10 each) · 9 new test files · `test-mcp-sc-local.R` +71/-9 · `test-mod-spatial-qc-export.R` +69/-13 |
| `10d1ba4` | R1 `read` (bounded read) | `R/core/drive_allowlist.R` +111/-5 · `R/core/drive_watcher.R` +12/-3 · `scripts/mcp_server.R` +352/-3 · `test-drive-verbatim-guard.R` (new) · `test-mcp-sc-local.R` +237/-3 · `test-mcp-spatial-local.R` +9/-8 · `test-mod-import-sc-drive.R` +7/-3 |
| `cffa0ae` | R2 generic `read_export` responder | `R/core/drive_watcher.R` +308/-3 |
| `4e09e53` | Slice 3 indexed vocabulary | `R/core/drive_allowlist.R` +51 · `R/core/drive_watcher.R` +208/-2 · `mod_bulk_pathways.R` +38/-2 · `mod_bulk_de_run.R` +55/-2 · `scripts/mcp_server.R` +222/-18 · 3 new test files |
| `39d2367` | Slice 4 WGCNA pilotable | `R/core/drive_allowlist.R` +80/-11 · `R/core/drive_watcher.R` +10 · `app.R` +1 · `mod_bulk_wgcna.R` +176/-5 · `mod_bulk_wgcna_export.R` (new) · `scripts/mcp_server.R` +37/-2 · 4 new test files · 4 modified test files |
| `a00011c` | F1 `max_rows` in the rebuilt whitelist | `R/core/drive_watcher.R` +6 · `test-drive-read-export.R` +41 |
| `fb9a1e8` | F2 vocabulary on all 3 read surfaces | `scripts/mcp_server.R` +103/-14 · `test-mcp-sc-local.R` +111 |
| `56a755a` | F3 one-in-flight guard | `scripts/mcp_server.R` +87/-1 · `test-mcp-guard-inflight.R` (new) · `test-mcp-sc-local.R` +26 |
| `b494404` | N1 refusal reasons survive the wire | `R/core/drive_watcher.R` +19/-2 · `scripts/mcp_server.R` +9/-2 · `test-drive-n1-injection-reason.R` (new) |

### 2.2 Cerberus bug fixes discovered *by* the MCP work (valuable independently)

| Commit | Subject | Files | Why it matters beyond MCP |
|---|---|---|---|
| `430d644` | `build_bulk_network_table_export`: `tabulate(match(...))` without `nb = nrow(nd)` → last isolated node produced a short `degree` and `data.frame()` raised *"differing number of rows"* | `R/bulk/bulk_network.R` +7/-1 · `test-bulk-network.R` +27 | **the HUMAN download path (`dl_network`) failed identically** — a real user-facing bug |
| `5b185a6` | WGCNA export gene column carried **sample names + NA** (real-world F7) | `R/bulk/bulk_wgcna.R` +9/-2 · `test-bulk-wgcna.R` +32/-1 | a content-correctness bug the plumbing test could not see |
| `3424cc1` | refused jobs now carry their reason on the wire | `R/core/drive_watcher.R` +12/-1 · `mod_bulk_pathways.R` +14/-2 · `mod_bulk_wgcna.R` +17/-4 · `mod_bulk_de_run.R` +10/-2 · 2 test files | makes an `invalid` verdict actionable for any consumer |

### 2.3 Tests-only commits

| Commit | Subject | Files |
|---|---|---|
| `0d048bf` | hermetic `.mcp_sc_local_env` fixture (the sandbox was booting on the **real** `tools/_drive/`) | `test-mcp-sc-local.R` +13 |
| `3c06fdd` | full bounded-read matrix + cross MCP flow | `test-drive-read-export.R` (new, 413) · `test-mcp-sc-local.R` +106 |
| `45dbdcc` | spatial QC measured correct + full MCP cycle automatable | `test-mcp-spatial-e2e.R` (new, 624) · `test-mod-spatial-qc-tabs.R` (new, 411) |
| `f3f9e7c` | multi-agent turn-taking, hermetic (two writers, one session) | `test-drive-multi-agent-turns.R` (new, 354) |

### 2.4 Totals

23 existing files touched, **30 new files**, over the whole range:
`+3478 / -144` lines in `R/`, `scripts/`, `app.R`, `modules/` — plus the test
files (which are the majority of the added lines).

---

## 3. Expected conflicts, per target area

**Headline: near-zero textual conflicts are expected**, because the replay base
(`origin/main` → `+S0`) reproduces `93cbecb`'s content exactly. Conflicts would
only appear if a file were modified on the published line after `df3369d` —
and `origin/main` is a static, already-fetched ref.

| Area | Files | Base differs? | Commits touching it | Expected conflict | Risk |
|---|---|---|---|---|---|
| `R/core/drive_allowlist.R` | 1 | no (identical at base) | **4** (`7b20b10`, `10d1ba4`, `4e09e53`, `39d2367`) | none expected | **LOW** — but it grows 98 050 → 120 157 B (445 changed lines) in a 1 804-line file; the frozen tables (`TS_DRIVE_EXPORT_ROUTES`, `TS_DRIVE_VOCABULARY_KEYS`, `TS_DRIVE_SESSION_INPUTS`) are **additive**, so hunks land in different regions |
| `R/core/drive_watcher.R` | 1 | no | **7** (`10d1ba4`, `cffa0ae`, `4e09e53`, `a00011c`, `39d2367`, `3424cc1`, `b494404`) | none expected | **MEDIUM** — the most-touched file (167 975 → 196 459 B, 586 lines). `cffa0ae` alone is +308. Sequential replay is the safe order; **do not** squash these 7 |
| `scripts/mcp_server.R` | 1 | **YES** (S0's own change) | **9** (S0, `9345615`, `7b20b10`, `10d1ba4`, `4e09e53`, `39d2367`, `fb9a1e8`, `56a755a`, `b494404`) | **handled in Stage 2** | **MEDIUM-HIGH** — 127 937 → 198 528 B (1 519 changed lines, +56 %). Every stage touches it; the final tree-hash check is what makes this safe |
| `app.R` | 1 | no | 2 (`7b20b10` +7, `39d2367` +1) | none | **LOW** — 8 added lines total (52 930 → 53 754 B) |
| `modules/` | 23 | no | 5 commits | none expected | **LOW** — 8 of the 23 files are **brand-new** `*_export.R` (pure additions); the 15 modifications are 7–198 lines each |
| `tests/` | 34 | **YES** (1 file) | 12 commits | none expected after Stage 2 | **LOW** — 30 new files (additions never conflict) + 4 modified. `test-mcp-sc-local.R` is the base-differing one; it is also the most-repeatedly-modified test |
| `R/bulk/bulk_network.R`, `R/bulk/bulk_wgcna.R` | 2 | no | 1 each | none | **LOW** — small targeted fixes |

**The two files that CAN conflict if done wrong** are `scripts/mcp_server.R` and
`tests/testthat/test-mcp-sc-local.R`: they differ from `origin/main` at the base.
They must be brought to `93cbecb`'s state **before** the 17-commit replay, or
every subsequent cherry-pick onto them will conflict. Stage 2 exists for exactly
this.

---

## 4. Separation of concerns

| Bucket | Content | Commits | Publication |
|---|---|---|---|
| **A. MCP production changes** | server + drive protocol + export seams + vocabulary | `93cbecb`, `9345615`, `7b20b10`, `10d1ba4`, `cffa0ae`, `4e09e53`, `39d2367`, `a00011c`, `fb9a1e8`, `56a755a`, `b404404` | yes — belongs on the published line |
| **B. Cerberus bug fixes found by MCP** | network degree bug, WGCNA gene column, job refusal reasons | `430d644`, `5b185a6`, `3424cc1` | **yes, independently of MCP** — these are app bugs, two of them visible to human users |
| **C. Tests** | 30 new + 4 modified test files | `0d048bf`, `3c06fdd`, `45dbdcc`, `f3f9e7c` (pure) + tests inside A and B | yes — they are the evidence for A and B |
| **D. Docs** | `docs/**` (126 files, **0 tracked**), `mcp/docs/*` (2 files, on `mcp-complete`) | none on `main` | **policy decision** — §6 |
| **E. Local agent artefacts** | `.workbuddy-ai/**` (memory, audits, `tmp/`, `fixture/`, `freeze/`), `docs/` working notes, `tools/_drive/*.json` (transient), `QC/`, `mcp.examples/`, `python_env_sccoda/`, `renv/`, repomix outputs | none | **never publish** — all already covered by `.gitignore` |

⚠️ **Bucket E must stay ignored — explicitly including
`.workbuddy-ai/audits/`.** That directory is a *parallel local audit area*: it
holds measured internals (live session ids, host paths, transient protocol
dumps). It is covered today by the `.workbuddy-ai/` rule (`.gitignore:38`,
verified). **No change to that rule is proposed.**

---

## 5. Staged port plan (cherry-pick), with byte/hash gates

**Nothing below has been executed.** Each stage names its own acceptance gate.

### Stage 0 — Freeze and clone (no ref touched in the source repo)

```
# 1. record the freeze (read-only)
git rev-parse main origin/main mcp-complete origin/mcp-complete \
              backup-pre-rewrite backup-pre-squash
git rev-parse main^{tree} origin/main^{tree}          # c6f07bf… / 4f97c61…

# 2. scratch clone OUTSIDE the repo — the source repo gains nothing
git clone --no-hardlinks --no-local "<repo>" "<tmp>/recon"
cd "<tmp>/recon" && git switch -c recon/mcp-on-published origin/main
```

**Gate 0:** in the *source* repo, `git show-ref` and `git rev-parse HEAD` are
byte-identical to the freeze; `git status --porcelain` empty.
In the clone: `git rev-parse HEAD == df3369d4d15d5dd7a9d5ec4fb9288b98f84ada3d`.

### Stage 1 — Establish the correct base

Apply **only** the 2-file delta `origin/main → 93cbecb`:

```
git diff origin/main 93cbecb -- scripts/mcp_server.R tests/testthat/test-mcp-sc-local.R > s0.patch
git apply --index s0.patch && git commit -m "mcp-S0 (ported): arming bootstrap"
```

**Gate 1 (hard):** `git rev-parse HEAD^{tree}` **must equal**
`700d1f8e437841595b755a73167ef9c50bf51655`.
Also `git diff --stat HEAD 93cbecb` must be **empty** (same tree, so no diff).

### Stages 2–18 — Replay the 17 commits in order

```
git cherry-pick 9345615 430d644 7b20b10 0d048bf 10d1ba4 cffa0ae 3c06fdd \
                4e09e53 45dbdcc a00011c f3f9e7c 5b185a6 39d2367 3424cc1 \
                fb9a1e8 56a755a b494404
```

Order is **chronological, one commit at a time** — deliberately *not* squashed:
the 7 `drive_watcher.R` commits and the 9 `mcp_server.R` commits overlap, and
squashing them would destroy the ability to bisect a failure.

**Per-stage gate (run after every cherry-pick):**

```
git rev-parse HEAD^{tree}                       # record it
git status --porcelain                          # must be empty (no conflict markers left)
git diff --stat HEAD^ HEAD                      # must equal that commit's numstat (§2)
```

**Optional semantic gate** — run the family the commit belongs to, in the clone:

```
Rscript tools/run_tests.R mcp drive     # for protocol commits
Rscript tools/run_tests.R bulk-         # for the bucket-B bug fixes
```

⚠️ **Known pre-existing red**: the canonical family already fails
**25 / 2 error** on the untouched tree, clustered on the DE confirmation-fire
path (`INPUT_NOT_READY` where `confirm` is expected; `invalid` where `running`
is expected). **Do not read those 25 as port damage** — establish the baseline
in the clone *before* Stage 2, on the same machine, and compare.

### Stage 19 — Final equivalence gate (the decisive one)

```
git rev-parse HEAD^{tree}                       # MUST equal c6f07bf21f69f1fd4f3ced004765226d5f3a77da
git diff --stat HEAD main                       # MUST be empty
git diff --quiet HEAD main && echo "PORT IS BYTE-EXACT"
```

**Gate 19:** `git diff --quiet HEAD main` exits 0 ⇒ the ported branch's content
is **identical to `main`** while its ancestry now includes `origin/main`. That is
the whole objective, and it is checked by one hash — not by 30 file reviews.

If Gate 19 fails, the delta localises it exactly: `git diff HEAD main` names the
files, and `git log --oneline -- <file>` in the clone names the stage.

### Stage 20 — Decide (not part of the port)

Only after Gate 19 passes does A/B/C become a *clean* choice, because at that
point option A no longer requires a blind rewrite — it requires a **fast-forward**
of the ported branch onto `origin/main`. **Still not executed here.**

---

## 6. Docs publication policy — explicit allowlists, not a blanket exception

**Current state (measured):** `.gitignore` contains a bare `docs/`, which
excludes the *directory*. Git cannot re-include a file whose parent directory is
excluded, so **any** future `!docs/...` line is inert as long as `docs/` stands.
That is a latent trap, not just a policy gap.

**Proposed mechanism — three edits, all reversible:**

1. Replace `docs/` with `docs/*` (excludes contents, keeps the directory
   traversable, so negations become effective).
2. Add an **explicit allowlist** of published documents — never a wildcard:
   ```
   !docs/README.md
   !docs/CONVENTIONS.md
   !docs/ROADMAP.md
   !docs/contracts/**          # frozen contracts — the reason to publish
   !docs/release/**
   ```
3. Keep everything else excluded by default: `docs/archive/**`,
   `docs/audits/**`, `docs/proposals/**`, and every working note.

**Tiering (proposed):**

| Tier | Paths | Rationale |
|---|---|---|
| **Publish** | `docs/contracts/**` (35 files), `docs/release/**` (7), `docs/CONVENTIONS.md`, `docs/ROADMAP.md`, `docs/README.md`, `docs/DRIVE_LIVE_CONTROL_PLAN.md` | frozen contracts and conventions are what an external consumer needs; they are stable by design |
| **Publish after curation** | `docs/STATUS.md`, `docs/PITFALLS.md`, the `ROADMAP_*` files | high value but volatile and large; publish only if the owner accepts ongoing maintenance |
| **Never publish** | `docs/archive/**` (39), `docs/audits/**` (2), `docs/proposals/**` (8), `docs/mcp_hardtest.md` and the `docs/mcp_*.md` working notes, `.workbuddy-ai/**` | measured internals, host paths, transient protocol dumps, superseded states |

⚠️ `docs/mcp_hardtest.md` and `.workbuddy-ai/audits/**` contain **live session
identifiers, host paths and transient protocol evidence**. They must stay
unpublished regardless of the tier chosen.

**Rule:** the allowlist is a list of *explicit file/dir paths*, reviewed as a
diff. A blanket `!docs/**` is explicitly rejected — it would publish 126 files
including the audit area by accident.

---

## 7. Canonical location of the MCP server — no two editable copies

**Current state (measured):** the server exists in three places —
`scripts/mcp_server.R` (all refs; 198 528 B on `main`, 127 937 B on
`origin/main`), and `mcp/server/mcp_server.R` on `mcp-complete` only
(199 434 B = the `main` copy **+ 13 lines** of provenance header,
`numstat 0 13`).

**Proposal:**

1. **Canonical = `scripts/mcp_server.R` in Cerberus.** It is the executable, it
   is where the drive core it sources actually lives (`R/core/drive_*.R`), and
   it is the path every configured client already points at.
2. **`mcp/server/mcp_server.R` becomes a GENERATED artefact, never hand-edited.**
   Add a one-command generator (`tools/emit_mcp_extract.R`, proposed) that
   produces the stamped copy, and a guard in the existing `--check` idiom that
   **fails** when the committed copy ≠ generated output. Same shape as the
   existing `TS_DRIVE_EXPORT_ROUTES` mirror check — the repo already knows how
   to do this.
3. **Alternative if the extraction is abandoned:** delete `mcp/server/` and keep
   the documentation only. A stamped copy with no generator is a copy that
   *will* drift — it already carries a 13-line difference that only a human
   remembers.
4. **Rejected:** a `source()` shim from `mcp/` into Cerberus. It looks like one
   copy but makes the extracted project non-self-contained, which is the one
   thing an extraction is for.
5. **Compatibility:** whatever is chosen, keep `scripts/mcp_server.R` at its
   current path so `mcp.examples/*.json`, `.zcode/config.json` and
   `~/.workbuddy-ai/mcp.json` keep working unchanged.

---

## 8. Human–agent cohabitation contract, and the known live bugs

### 8.1 Cohabitation contract (one live session, two actors)

**Ownership.** Exactly one actor drives at a time. The badge is the visible
arbiter; `tools/_drive/ready.json.armed` is the machine truth. The agent arms;
the human watches.

**Non-negotiables, each anchored in a measured failure:**

| Rule | Why |
|---|---|
| Never close/stop the R session mid-cycle | `ready.json` becomes an orphan indistinguishable from a live one; the agent answers `STALE_SESSION` |
| Never edit/delete `scenario.json`, `result.json`, `arm.json` by hand | breaks atomicity and the monotonic `seq` floor (`last_seq` is re-derived from `result.json` at boot) |
| Never rename/move `tools/_drive/` or the export dir | the root is resolved once at boot; the app then writes where the agent does not read |
| Never load a different dataset without telling the agent | advances `vocab_rev`; the agent's already-written scenario is refused `VOCAB_STALE` / `INPUT_NOT_READY` / `INDEX_OUT_OF_RANGE` |
| Never click the same action button the agent will fire | the human click and the drive click share **one** observer; two triggers = two real pipeline runs and desynchronised counters |
| Never open a new tab or reload during a session | "last connected session wins" — the token rotates and every agent pin answers `SESSION_MISMATCH` |
| Finish cleanly: **disarm first, then stop** | disarm stops the heartbeat and drops the badge; stopping first leaves a stale handshake |
| Agent: poll to a terminal verdict **before** the next write | `scenario.json` is one slot; the F3 guard now refuses the overwrite, but the write is still wasted |

**Agent-side additions from this audit:** a reader of `ready.json` must tolerate
**both** `ENOENT` and `PermissionError` (see F9); and `result.json:running` —
not heartbeat freshness — is the liveness proof during a long synchronous job.

### 8.2 Known live bugs — status and owner-visible impact

| ID | Severity | What it is | Where it bites | Status |
|---|---|---|---|---|
| **F9** | 🔴 **new, unfixed** | `ready.json` is rewritten every ~3 s via `unlink()`+`rename()`; **2.06 %** of direct reads fail (ENOENT 3 143 / `PermissionError` 1 891 per 243 982 reads), and it **leaks onto the MCP surface: 2/300 `status` calls returned a false `NO_SESSION`** on a live, armed session | any agent polling a read-only tool can conclude "session gone" and abort a healthy cycle | measured 2026-10-06; **no fix**. Shape: bounded retries inside `ts_drive_read_ready()`, or a replace-in-place write |
| **F10** | 🟡 new, unfixed | badge never shows `running`; during a 9.6 s DE job it displayed the **previous** terminal (`done · seq 50 · import_bulk`) | a human watching the badge is told a finished step is current | DOM-measured 2026-10-06; confirms and extends F6 |
| **F11** | ⚪ new, unfixed | `status.session.pid_alive` is `null` on a healthy session (assigned only when the handshake is stale, `mcp_server.R:381`) | the field cannot be used as a liveness probe in the normal case | measured; cosmetic |
| **F12** | ⚪ known (was F8) | `initialize.instructions` says "**Nine** tools"; `tools/list` publishes **10** | misleading first-contact text | unchanged since 2026-10-05 |
| **O1** | 🟡 new, needs a probe | after a tab rotation the new session **cannot run** a job on the previously imported data (`run bulk_de` → `invalid` in 0.1 s) while `result.json`/snapshot keep publishing `has_data=true, n_genes=17925` | the published snapshot **over-promises** for the current session | observed; **likely the same root cause as the canonical family's 25 failures** (DE confirmation-fire path, `INPUT_NOT_READY`) — the single most valuable thing to investigate next |
| **N5** | 🟡 known, unfixed | server-side refusal reasons carry **non-ASCII**: the app ASCII-folds diagnostics, but `.ts_vocabulary_ok` passes through the server's C-locale JSON encoder unguarded — an em-dash truncates the tail (`"publishes 2 b"`) | refusal text is cut off exactly where it is needed | declared in `mcp/README.md`; fix shape = fold once at the server's encode choke point |
| **N4** | ⚪ known, unfixed | the `\b[A-Za-z0-9]{8,}\b` sanitiser redacts `baseMean` / `log2FoldChange` in bounded-read `columns` (map by position). **N4bis**: the same rule redacts words inside `errors[]`, so the VOCAB_STALE hint reads `re-<redacted> and retry` | an agent can see the data but cannot name two of the columns it exported | unchanged; cosmetic, values intact |
| *(F5)* | 🟡 known | published vocabulary lags one probe republication after an injection | forces a re-snapshot | refined by the N1 validation: the invalid verdict itself carries the fresh vocabulary |
| *(N2, N3)* | ⚪ known | `active_contrast` not in the snapshot keep-set; `set_inputs` never reaches a terminal status | an agent cannot confirm which contrast produced a result; "poll to terminal" burns a timeout | declared by design |

**Recommended fix order** (if the owner authorises fixes at all): **O1** (it
plausibly explains the 25 canonical failures *and* O1), then **F9** (it is a
false negative on a read-only surface — the cheapest to make trustworthy), then
**N5** (one encode choke point), then F12/N4 (cosmetic), F10/F11 (observability).

---

## 9. Stop condition

- **No branch created.**
- **No cherry-pick performed.**
- **No reset performed.**
- **No force-push performed.**
- **No file moved.**
- **No ref changed** — `git show-ref` and `HEAD` are byte-identical to the
  freeze recorded at the start of this audit.

### Approval required before any stage

1. **Approve the mechanism?** Port by **sequential cherry-pick of 18 commits**
   (§5) with the tree-hash gates, in a **scratch clone** so this repository
   gains no ref.
2. **Approve the acceptance test?** Final gate = `git diff --quiet HEAD main`
   (tree `c6f07bf…`) — i.e. **byte-exact equivalence with `main`**, not a
   file-by-file review.
3. **Approve the docs allowlist (§6)?** In particular: publish
   `docs/contracts/**` + `docs/release/**` + conventions/roadmap, keep
   `docs/archive/**`, `docs/audits/**`, `docs/proposals/**` and all
   `docs/mcp_*.md` notes unpublished.
4. **Approve the canonical server decision (§7)?** `scripts/mcp_server.R`
   canonical, `mcp/server/mcp_server.R` generated + guard-checked — or delete
   the extracted copy.
5. **Authorise the O1 investigation?** It is the only item that may explain both
   a live-session defect and the 25 pre-existing canonical failures.

**Awaiting approval. Nothing will be executed until then.**

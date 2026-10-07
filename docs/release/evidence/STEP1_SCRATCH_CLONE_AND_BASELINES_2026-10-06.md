# Step 1 — Scratch clone, evidence preservation, baselines

Date: 2026-10-06 · Mode: **read-only w.r.t. the original repository**
Owner decision recorded: published `origin/main` = historical base to preserve;
local `main` @ `b494404` = content reference; S0 = the **two-file** delta
`df3369d → 93cbecb`; **do not** cherry-pick the root commit.

## 0. Scope actually executed

| Item | Done |
|---|---|
| Preserve local-only (gitignored) evidence | ✅ |
| Independent scratch clone (published base + local source reachable) | ✅ |
| Record exact refs + ordered 17 commits | ✅ |
| Prepare the S0 two-file delta (not applied, not published) | ✅ |
| Reproducible test baselines (narrow vs broad, failure identities) | ✅ |
| Correct the hard-test export summary | ✅ |
| **Apply S0** | ❌ not done |
| **Cherry-pick / branch / reset / push / force-push** | ❌ not done |
| **Modify an original ref / the original worktree** | ❌ not done |

---

## 1. Preserved local-only evidence

**Backup location:** `D:/Data_science/ts_local_evidence_backup_2026-10-06`
**Local and confidential — not added to Git.**

| Subdir | Source | Files | Nature |
|---|---|---|---|
| `docs/` | `<repo>/docs/` | 126 | public docs + `contracts/` (35) + `archive/` (39) + `audits/` (2) + `proposals/` (8) + `release/` (7) — **all gitignored** |
| `workbuddy-ai__audits/` | `<repo>/.workbuddy-ai/audits/` | 52 | MCP audits + git forensic/reconciliation reports |
| `workbuddy-ai__memory/` | `<repo>/.workbuddy-ai/memory/` | 20 | dated work logs + curated `MEMORY.md` |
| `hardtest/` | `%TEMP%/ts_hardtest/` | 33 | MCP hard-test driver (`*.py`, `badge.js`), raw traces (`logs/*.jsonl`, `*.log`, `ram_cpu.csv`), corpus copy, `drive_backup/` |
| `ts_g/` | `%TEMP%/ts_g/*.txt` | 9 | prior S0/S1/S2/FIX logs (`check_*.txt`, `fam_*.txt`) |
| `ts_retest/` | `%TEMP%/ts_retest/` | 58 | earlier real-world drivers (`drv*.R`, `n1_*.R`) + logs |
| `ts_realworld/` | `%TEMP%/ts_realworld/` | 45 | earlier MCP client drivers (`mcp_client*.R`) + logs |

**Total 343 files, ~13 MB.** Integrity: `MANIFEST.sha256` — verified **343/343 `: OK`**.

**Deliberate exclusions**
- `%TEMP%/ts_hardtest/chrome_profile/` — Chrome user-data profile (runtime artefact, potentially sensitive: cookies/session). **Not** backed up.
- `%TEMP%/ts_g/{S0,S0b,S1,S2,FIX,FIXT}/` — **linked-worktree checkouts of the original repo** (a *shared-object* setup). Not copied; their content is reachable from the repository objects.

---

## 2. Independent scratch clone

**Location:** `D:/Data_science/ts_reconcile_scratch`

Method (no hardlinks, no shared-object dependency):
```
git clone --no-hardlinks "<repo>" D:/Data_science/ts_reconcile_scratch
cd ts_reconcile_scratch
git remote rename origin local-src
git fetch --no-tags local-src "refs/remotes/origin/main:refs/remotes/published/main"
git fetch --no-tags local-src "refs/remotes/origin/mcp-complete:refs/remotes/published/mcp-complete"
```

**Independence — measured, not assumed**

| Check | Result |
|---|---|
| pack inode, source vs clone | **different** (`…7263671/…431589` vs `…533374/…533377`) |
| pack link count | **1** in both → true copies, not hardlinks |
| `git fsck --full` | **exit 0** (only benign dangling commits) |
| clone `.git` size | 36 MB (own pack, `in-3854` objects) |

**Refs in the clone**

| Ref | Commit |
|---|---|
| `refs/heads/main` | `b494404…` (local source, checked out) |
| `refs/remotes/published/main` | `df3369d…` ← **published base** |
| `refs/remotes/published/mcp-complete` | `c2ed117…` |
| `refs/remotes/local-src/{main,mcp-complete,backup-pre-rewrite,backup-pre-squash}` | mirrors of the source's local refs |

All three anchors resolve in the clone; `merge-base main published/main` is **empty**
(disjoint histories reproduced). The clone is **not** a worktree of the original.

---

## 3. Exact references

| Anchor | Commit | Tree |
|---|---|---|
| published base (`origin/main`) | `df3369d4d15d5dd7a9d5ec4fb9288b98f84ada3d` | `4f97c61be60be6bd78eb90ae0593fd99f9b44089` |
| local source (`main`) | `b4944041141a288b3c802e656e007af722a857a9` | `c6f07bf21f69f1fd4f3ced004765226d5f3a77da` |
| S0 (`93cbecb`) | `93cbecb6519a48347fb90591268579b9c1d33e92` | `700d1f8e437841595b755a73167ef9c50bf51655` |

`93cbecb` has **0 parents** (root) and a 376-file tree → **must be applied as a
patch, never cherry-picked**.

**The 17 post-root commits — exact ancestry order** (each parent = predecessor;
chain verified linear):

| # | SHA | Subject |
|---|---|---|
| 1 | `93456150bb580172ba69024b57e83a34a03fa2d6` | feat(mcp-S1): transcripto_drive_import |
| 2 | `430d6445d370bf5dd4df268c7caf9fb67a717e1e` | fix(bulk): network export degree (human path failed identically) |
| 3 | `7b20b101e1fae2645d3f0d3ae0f071de7d61bc87` | feat(mcp-S2): export routes 2→9 (atomic) |
| 4 | `0d048bf706c768f38f3a1b4bb52e5e320d623fd5` | test(mcp): hermetic `.mcp_sc_local_env` fixture |
| 5 | `10d1ba4d0bef252cc3221cd80cd43c3332e82ab6` | feat(mcp-R1): transcripto_drive_read (bounded) |
| 6 | `cffa0ae4f78b5200c27fc9a9b1df31e1ed377d93` | feat(drive-R2): generic `read_export` responder |
| 7 | `3c06fdd0d0eb4e79764ad66c220a2abaad9e97d3` | test(mcp-R3): bounded-read matrix |
| 8 | `4e09e534a57c8bf2cabdbc1b6696c62ffe03cd7f` | feat(mcp-Slice3): 5 session-derived inputs by index |
| 9 | `45dbdcc24fc276c4aa7e98d84c92389aaa3d1835` | test(spatial): spatial QC + full MCP cycle |
| 10 | `a00011cab8c2463f6b43a12d00e34efe725f5a14` | fix(drive-F1): `max_rows` in the rebuilt allowlist |
| 11 | `f3f9e7c7a0ea4d585f8617ece90bb9e0b69f0486` | test(drive): hermetic multi-agent turn-taking |
| 12 | `5b185a6d11882684305147075bbbb6878d98125e` | fix(bulk-wgcna): gene column carried SAMPLE names + NA (F7) |
| 13 | `39d23677efd0d6d2fe34a106067caaa68eac4101` | feat(drive-Slice4): WGCNA pilotable (2 steps, 1 export) |
| 14 | `3424cc1607c93429a66e293c1d06a3ea69079c59` | fix(drive-F4): refused jobs carry their reason |
| 15 | `fb9a1e8348678f609795c2ba51091c25e0dd57af` | fix(mcp-F2): vocabulary on all three read surfaces |
| 16 | `56a755a4a1bbb701d65495387831dc5b7877655f` | fix(mcp-F3): refuse a write while prev seq unconsumed |
| 17 | `b4944041141a288b3c802e656e007af722a857a9` | fix(drive-N1): refusal reasons survive the full wire |

---

## 4. S0 delta — prepared, verified, **not applied**

```
git diff df3369d 93cbecb     # 2 files changed, 152 insertions(+), 11 deletions(-)
```

| Path | numstat |
|---|---|
| `scripts/mcp_server.R` | `+52  −10` |
| `tests/testthat/test-mcp-sc-local.R` | `+100  −1` |

Patch prepared locally (not a published deliverable):
`D:/Data_science/ts_reconcile_prep/S0_df3369d_to_93cbecb.patch` — 11 374 bytes.

**Verification by tree hash (temp index, no working-tree touch):**
```
GIT_INDEX_FILE=<tmp> git read-tree df3369d^{tree}   → 4f97c61be60be6bd78eb90ae0593fd99f9b44089
git apply --cached --check <patch>                  → OK
git apply --cached <patch> ; git write-tree         → 700d1f8e437841595b755a73167ef9c50bf51655
expected (93cbecb tree)                             → 700d1f8e437841595b755a73167ef9c50bf51655  ✓
```
(The patch carries one benign `new blank line at EOF` whitespace warning.)

---

## 5. Reproducible test baselines

**Environment**
- R **4.4.2** (2024-10-31 ucrt), `x86_64-w64-mingw32`
- `renv` **1.2.4**; project = the repo root; `renv.lock` declares R 4.4.2 / Bioconductor **3.20**
- `.libPaths()`: `[1] <repo>/renv/library/windows/R-4.4/x86_64-w64-mingw32`, `[2] D:/Data_science/R-4.4.2/library`
- `.Renviron`: `RENV_CONFIG_SANDBOX_ENABLED = FALSE`; `.Rprofile`: `source("renv/activate.R")`
- ⚠️ **`renv` warns "One or more packages recorded in the lockfile are not installed."** — recorded as measured; **root cause NOT asserted** here.
- ⚠️ `renv.lock` working file is 1 107 374 B but the blob is 1 088 613 B — explained: `.gitattributes` pins `renv.lock text eol=lf`; the worktree carries **18 761 CRLF** (measured in Python). `git hash-object renv.lock` = `21b04bbf…` = the HEAD blob → **normalized-identical, `git status` clean is correct.**

**Runner commands (exact)**
```
# narrow — MCP/drive surface
Rscript tools/run_tests.R mcp drive

# broad — the documented canonical family
Rscript tools/run_tests.R "drive-|mcp-|import-spatial|import-bulk|spatial|i18n|conventions|check-|sc-|bulk-"

# full suite (different runner; exit 139 even on success — read full_suite_results.txt)
Rscript tools/run_full_suite.R
```

**Measured results (on the untouched tree)**

| Scope | Files | failed | passed | error | skipped | exit |
|---|---|---|---|---|---|---|
| narrow (`mcp\|drive`) | 26 | **0** | 3 685 | **0** | **0** | 0 |
| broad (canonical family) | 144 | **25** | 10 032 | **2** | **2** | **1** |

⚠️ The two filters are **not nested** (`mcp|drive` vs `drive-|mcp-|…`); the narrow
scope is green, the broad scope is **red**, and it was already red **before** any
replay — so it cannot serve as a per-batch gate. Use the narrow scope for the
replay gate, and treat the broad scope as a **pre-existing baseline**.

**Individual failure identities (broad scope)**

Failures by file (per-file sum of the runner's `fail=` rows; note the runner emits
one row per *file+context*, 1 551 rows over 144 files — the sums reconcile to
TOTAL `failed=25`):

| File | Failures |
|---|---|
| `test-bulk-de-input-confirm.R` | **21** |
| `test-bulk-de-staged-injection.R` | **3** |
| `test-conventions-c10-scope.R` | **1** |

Named assertions (the runner's summary reporter caps detail at 10 blocks — all 10
are from `test-bulk-de-input-confirm.R`):

| # | Location | Assertion | Actual |
|---|---|---|---|
| 1 | `:104:3` | `msg` matches `"confirm"` | `INPUT_NOT_READY: the module published no vocabulary for this session.` |
| 2 | `:450:3` | `out$status` identical to `"running"` | `"invalid"` |
| 3 | `:452:3` | `is.null(p$prior)` is FALSE | `TRUE` |
| 4 | `:453:3` | `names(p$prior)` = `names(vals)` | `NULL` |
| 5 | `:455:3` | `p$prior[["bulk-de-shrink_lfc"]]` is TRUE | `NULL` |
| 6 | `:456:3` | `p$prior[["bulk-de-condition_col"]]` = `"tissue"` | `NULL` |
| 7 | `:457:3` | `p$values[["bulk-de-shrink_lfc"]]` is FALSE | `NULL` |
| 8 | `:459:3` | `p$session_token` = `"tokpin01"` | `NULL` |
| 9 | `:551:3` | `out$status` identical to `"running"` | `"invalid"` |
| 10 | `:553:3` | `is.null(p)` is FALSE | `TRUE` |

… then `... and 22 more` (summary-reporter cap; `Maximum number of 10 failures reached`).

⚠️ **Open point, recorded not diagnosed:** the table pass reports **25** failures,
while the in-process verbose re-run reports **10 + 22 more = 32**. The two passes
of the same filter therefore do **not** agree. Cause not asserted.

**Individual error identities (broad scope) — both are the same seam**

```
Error in with_random_port(launch_chrome_impl, path = path, args = args) :
  Cannot find an available port. Please try again.
Caused by error in `supervisor_start()`:
! processx supervisor was not ready after 5 seconds.
```
(×2) — i.e. the **chromote / `processx` supervisor** launch path. The narrow
MCP/drive scope reports **0 errors**, so this error lives outside it.

---

## 6. Acceptance targets for the later replay

| Stage | Target | Status |
|---|---|---|
| after S0 (patch onto `df3369d`) | tree `700d1f8e437841595b755a73167ef9c50bf51655` | **verified** on a temp index |
| after all 17 commits | tree `c6f07bf21f69f1fd4f3ced004765226d5f3a77da` (= `main`) | pending replay |
| ancestry | `df3369d` must be an **ancestor** of the final result | currently **NO** (unrelated) — this is the goal |
| scope | **no** extra files, **no** docs changes, **no** `/mcp` extraction | constraint |

---

## 7. Hard-test export summary — corrected

Checked against the raw trace (`logs/mcp_calls_routes.jsonl` + `logs/results.jsonl`).
The 10 declared routes resolve to **4 `done` / 6 `invalid`** (not 5/5):

- `done`: `bulk_de`, `bulk_pathways`, `bulk_filter`, `bulk_wgcna`
- `invalid`: `spatial_qc`, `sc_markers`, `sc_pathways`, `bulk_signatures`, `bulk_pattern`, `bulk_network`

`docs/mcp_hardtest.md` corrected in two places:
- §1 table row `transcripto_drive_export` → "4 `done` (real files), 6 honest `invalid`"
- §2 prose → "The **six** `invalid` verdicts…"

(The §2 route table already listed 4 done / 6 invalid; only the two summary lines were wrong.)

---

## 8. Original repository — unchanged

| Check | Result |
|---|---|
| `git show-ref` | the **same 7 refs**, same SHAs |
| `HEAD` | `b494404…` on `main` |
| `git status --porcelain` | empty (clean) |
| `git worktree list` | unchanged (main + pre-existing `ts_g/S0b`) |
| `.git/FETCH_HEAD` | mtime 2026-10-06 **17:58** (before Step 1) — untouched |
| `.git/ORIG_HEAD` | mtime 2026-10-06 **01:18** — untouched |
| `git reflog main` | top still `b494404` — no new entry |

The only file written inside the repo is `docs/mcp_hardtest.md` (gitignored), the
correction in §7. Everything else lives in the three sibling directories:
`ts_reconcile_scratch/`, `ts_local_evidence_backup_2026-10-06/`, `ts_reconcile_prep/`.

---

## 9. Stop condition

**No changes made** to the original repository · **No push performed** ·
**No force-push performed** · No branch created, no cherry-pick, no reset, no
file move, **S0 not applied**.

**Decision required before Step 2:**
1. Confirm the replay **work branch name** and **where** it is created — the
   scratch clone `D:/Data_science/ts_reconcile_scratch` (recommended) or the original.
2. Confirm **batch size = 3** and that the per-batch gate is the **narrow** scope
   (`run_tests.R mcp drive`, currently 3 685/0/0/0) — since the broad scope is
   pre-existing red.
3. Confirm the S0 patch is applied **as a patch** (not a cherry-pick), then the 17
   commits replayed in the order in §3.
4. The `25` vs `32` failure-count instability (§5) and the 2 chromote errors:
   investigate now, or keep as documented baseline?

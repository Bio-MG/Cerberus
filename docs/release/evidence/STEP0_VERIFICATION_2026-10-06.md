# Step 0 — Preparation verification (READ-ONLY)

**Date**: 2026-10-06 19:45 · **Status**: Step 0 executed. **Steps 1–7 NOT started.**
No branch created, no cherry-pick, no reset, no force-push, no file moved.
`git show-ref` and `HEAD` are unchanged from the pre-Step-0 freeze.

---

## 0.1 Refs — verified

⚠️ **The requested command is invalid on this git.** `git show-ref --heads --remotes`
is rejected:

```
git version 2.55.0.windows.3
$ git show-ref --heads --remotes
error: unknown option `remotes'
```

`show-ref` accepts `--head`, `--branches`, `--tags`, `--dereference` — there is
**no `--remotes`**. The equivalent that works is `git show-ref` (all refs) or
`git branch -a`. Result:

| Ref | SHA | Type |
|---|---|---|
| `refs/heads/main` | `b4944041141a288b3c802e656e007af722a857a9` | branch (**HEAD**) |
| `refs/remotes/origin/main` | `df3369d4d15d5dd7a9d5ec4fb9288b98f84ada3d` | remote-tracking |
| `refs/remotes/origin/HEAD` | `df3369d4d15d5dd7a9d5ec4fb9288b98f84ada3d` | → `origin/main` |
| `refs/heads/mcp-complete` | `c2ed11701779f434926ac1f71407ac36ae40a6a8` | branch |
| `refs/remotes/origin/mcp-complete` | `c2ed11701779f434926ac1f71407ac36ae40a6a8` | remote-tracking |
| `refs/heads/backup-pre-rewrite` | `caee4711df03dd06013c6a53a73e0f9010f872ec` | branch |
| `refs/heads/backup-pre-squash` | `54eb6029af3eb56b9325c3468f0db651078cd003` | branch |

**7 refs, all unchanged.** No `reconcile` branch exists yet. Working tree clean
(`git status --porcelain` empty). Worktrees: main checkout + one detached
scratch worktree at `93cbecb` (pre-existing, untouched).

## 0.2 Tree hashes — verified

| Object | Tree SHA |
|---|---|
| `origin/main` | `4f97c61be60be6bd78eb90ae0593fd99f9b44089` |
| `main` | `c6f07bf21f69f1fd4f3ced004765226d5f3a77da` |
| `93cbecb` (squash root) | `700d1f8e437841595b755a73167ef9c50bf51655` |

Commit SHAs: `origin/main` = `df3369d`, `main` = `HEAD` = `b494404`.

## 0.3 Divergence — reported

| Check | Result |
|---|---|
| `merge-base --all main origin/main` | **empty** — unrelated histories |
| `rev-list --left-right --count main...origin/main` | `18  382` |
| shared paths | **376 / 376** (0 missing from `main`) |
| identical blobs | **374** |
| differing blobs | **2** — `scripts/mcp_server.R`, `tests/testthat/test-mcp-sc-local.R` |

✅ The divergence is **historical only**. `main` contains everything
`origin/main` has, plus the MCP work.

---

## 🚫 Three blockers — these change the plan, please read before approving Step 1

### BLOCKER A — Step 2 as written will fail on commit #1

Step 2 says *"For each commit in the 18-commit list: `git cherry-pick <sha>`"*.
**Commit #1 (`93cbecb`) is a root commit** — measured: **0 `parent` lines**, tree
of **376 files**. A cherry-pick computes the diff against the parent; with no
parent the "diff" is *the entire tree added*. Applied onto `origin/main`, all
376 files already exist → the cherry-pick aborts with *"already exists"* /
whole-tree conflicts.

**The real S0 delta is only 2 files:**

```
git diff --stat origin/main 93cbecb
 scripts/mcp_server.R               |  62 +++++++++++++++++++----
 tests/testthat/test-mcp-sc-local.R | 101 ++++++++++++++++++++++++++++++++++++-
 2 files changed, 152 insertions(+), 11 deletions(-)
```

**Corrected Step 2:** replace the cherry-pick of `93cbecb` with a patch
application of that 2-file delta (commit as "mcp-S0 (ported)"), then cherry-pick
**commits #2–#18** (17 commits) normally. Acceptance: after the patch,
`git rev-parse HEAD^{tree}` must equal `700d1f8e437841595b755a73167ef9c50bf51655`.

### BLOCKER B — Step 6 names two directories that do not exist

| Requested | Reality |
|---|---|
| `docs/contracts/` | ✅ exists (35 files) |
| `docs/release/` | ✅ exists (7 files) |
| **`docs/architecture/`** | ❌ **DOES NOT EXIST** |
| **`docs/protocols/`** | ❌ **DOES NOT EXIST** |
| `mcp/docs/` | ❌ **not on `main` and not on `origin/main`** — 4 `mcp/` files exist **only on `mcp-complete`** |

`docs/architecture/` and `docs/protocols/` would have to be **created** (and
populated) before they can be published. `mcp/docs/` cannot be published on
`main` at all unless the `mcp/` extraction is ported — and the 18-commit list
does **not** include `c2ed117` (the extraction). So Step 6 needs a decision:
create those dirs, drop them, or port the extraction too.

### BLOCKER C — Step 6's `.gitignore` edit will be inert as written

The current rule is a **bare `docs/`**, which excludes the *directory*. Git
cannot re-include a file whose parent directory is excluded, so adding
`!docs/contracts/` **without first changing `docs/` to `docs/*`** has **no
effect**. The edit must be: `docs/` → `docs/*`, *then* the explicit `!` allowlist.

---

## Corrected Step 2 replay order (18 steps)

| # | SHA | Subject (truncated) | Step 2 action |
|---|---|---|---|
| 1 | `93cbecb` | mcp-S0 arming bootstrap | **patch, not cherry-pick** (Blocker A) |
| 2 | `9345615` | mcp-S1 `import` tool | cherry-pick |
| 3 | `430d644` | fix(bulk) network degree | cherry-pick |
| 4 | `7b20b10` | mcp-S2 export routes 2→9 | cherry-pick |
| 5 | `0d048bf` | hermetic `.mcp_sc_local_env` fixture | cherry-pick |
| 6 | `10d1ba4` | mcp-R1 bounded read | cherry-pick |
| 7 | `cffa0ae` | drive-R2 `read_export` responder | cherry-pick |
| 8 | `3c06fdd` | mcp-R3 bounded-read matrix | cherry-pick |
| 9 | `4e09e53` | mcp-Slice3 indexed vocabulary | cherry-pick |
| 10 | `45dbdcc` | spatial QC + full MCP cycle | cherry-pick |
| 11 | `a00011c` | drive-F1 `max_rows` whitelist | cherry-pick |
| 12 | `f3f9e7c` | drive multi-agent turn-taking | cherry-pick |
| 13 | `5b185a6` | bulk-wgcna gene column (F7) | cherry-pick |
| 14 | `39d2367` | drive-Slice4 WGCNA pilotable | cherry-pick |
| 15 | `3424cc1` | drive-F4 refusal reasons | cherry-pick |
| 16 | `fb9a1e8` | mcp-F2 vocabulary on 3 surfaces | cherry-pick |
| 17 | `56a755a` | mcp-F3 one-in-flight guard | cherry-pick |
| 18 | `b494404` | drive-N1 refusal reasons on the wire | cherry-pick |

---

## ⚠️ Two additional cautions on later steps (not blocking, but plan them now)

**Step 4 — `git branch -D main` is a force-delete of the only ref named `main`.**
The 18 commits stay reachable via `mcp-complete` (`c2ed117` → `b494404`) and the
two `backup-*` refs, so it is **recoverable** — but it is the one irreversible-
looking step in the sequence and deserves its own explicit approval. A safer
equivalent exists: `git branch -m main main-rewritten` (rename, no deletion),
then `git branch -m reconcile main`.

**Step 2's test gate is currently RED before you start.** The canonical family
already reports **25 failed / 2 error / 10 032 passed** on the untouched tree
(clustered on the DE confirmation-fire path: `INPUT_NOT_READY` where `confirm`
is expected, `invalid` where `running` is expected). **Establish the baseline in
the work branch before the first cherry-pick**, and compare against it — do not
read those 25 as port damage. `tools/run_tests.R mcp drive` is green
(3685 / 0 / 0 / 0), so it is the better per-batch gate.

---

## Stop condition

- **No branch created.** **No cherry-pick performed.** **No reset performed.**
- **No force-push performed.** **No file moved.**
- 7 refs and `HEAD` byte-identical to the Step 0 freeze.

### Awaiting approval to run Step 1

> Step 1 as written is: `git branch reconcile origin/main` then
> `git switch reconcile`.
>
> Please confirm **one** of:
> 1. **Proceed as written** (creates the `reconcile` branch in *this* repository;
>    all 7 existing refs stay untouched), or
> 2. **Proceed in a scratch clone** (this repository gains no ref at all — my
>    recommendation, since it makes "no existing ref changed" trivially true).
>
> And please decide on **Blocker A** (patch S0 instead of cherry-picking it) and
> **Blocker B** (`docs/architecture/`, `docs/protocols/`, `mcp/docs/`) before
> Step 2 and Step 6 respectively. **Blocker C** is a mechanical correction I will
> apply inside Step 6 if you approve it.

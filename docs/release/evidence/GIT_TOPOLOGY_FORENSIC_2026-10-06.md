# Git topology forensic report — TranscriptoShiny ("Cerberus")

**Date**: 2026-10-06 · **Mode**: AUDIT ONLY — no mutation of any kind was
performed. **Repository**: `D:/Data_science/SHINYAPP test (git work)/SHINYAPP test`
**Remote**: `origin` = `https://github.com/Bio-MG/ShinyApp---TranscriptoShiny.git`

Every number below was measured with read-only plumbing (`rev-parse`, `rev-list`,
`merge-base`, `ls-tree`, `cat-file`, `for-each-ref`, `reflog`, `show-ref`).
No `fetch`, `push`, `reset`, `merge`, `rebase`, `filter-repo`, `gc`, `prune`,
branch/tag creation or deletion, and no file move was executed.

---

## 0. Headline

**The premise is CONFIRMED: local `main` and `origin/main` have NO common
ancestor.** `git merge-base --all main origin/main` returns **empty**. They are
two unrelated root histories that nonetheless carry **94 % identical content**.

The cause is measurable: the published history (382 commits) was **squashed into
a single root commit** — `93cbecb`, whose tree contains **376 files**, exactly
`origin/main`'s tracked file count. Everything after that is new work.

---

## 1. Git topology

### 1.1 Ref inventory (`git show-ref`, `git for-each-ref`)

| Ref | Full SHA | Short | Type |
|---|---|---|---|
| `refs/heads/main` | `b4944041141a288b3c802e656e007af722a857a9` | `b494404` | branch (HEAD) |
| `refs/remotes/origin/main` | `df3369d4d15d5dd7a9d5ec4fb9288b98f84ada3d` | `df3369d` | remote-tracking |
| `refs/remotes/origin/HEAD` | `df3369d4d15d5dd7a9d5ec4fb9288b98f84ada3d` | `df3369d` | symbolic → `origin/main` |
| `refs/heads/mcp-complete` | `c2ed11701779f434926ac1f71407ac36ae40a6a8` | `c2ed117` | branch |
| `refs/remotes/origin/mcp-complete` | `c2ed11701779f434926ac1f71407ac36ae40a6a8` | `c2ed117` | remote-tracking |
| `refs/heads/backup-pre-rewrite` | `caee4711df03dd06013c6a53a73e0f9010f872ec` | `caee471` | branch |
| `refs/heads/backup-pre-squash` | `54eb6029af3eb56b9325c3468f0db651078cd003` | `54eb602` | branch |

No tags, no notes, no `refs/original`, no stashes. One additional **worktree**:
`C:/Users/marcg/AppData/Local/Temp/ts_g/S0b` at detached `93cbecb` (= `main`'s
root commit — an orphan scratch worktree).

⚠️ **Stale `packed-refs` entries** (measured, `.git/packed-refs`):

```
80f977c75bf8e721f8379ebb24fca30336534d20 refs/heads/main
80f977c75bf8e721f8379ebb24fca30336534d20 refs/remotes/origin/main
```

Loose refs shadow these, so they are inert — but they are **leftovers of the
rewrite** and record what `main`/`origin/main` pointed at on 2026-10-02
(`80f977c` "fix(tests): da-cross-views eager Milo/scCODA setup wrapped…", still
reachable via other refs, so not dangling). They are a useful forensic
fingerprint: they prove the ref names were re-pointed after being packed.

### 1.2 Per-ref metadata

| Ref | Short | Author / date | Parents | Commits | Tree | Root(s) |
|---|---|---|---|---|---|---|
| `main` | `b494404` | marc. paul `<mastermarcg@gmail.com>` · 2026-10-06 12:36:23 +0200 · *"fix(drive-N1) refusal reasons survive the full wire, not just the disk"* | `56a755a` | **18** | `c6f07bf21f69f1fd4f3ced004765226d5f3a77da` | `93cbecb` |
| `origin/main` | `df3369d` | same author · 2026-10-03 18:00:16 +0200 · *"feat(garde): check_status_claims.R — document vs relevé (H2#9…)"* | `c4e7198` | **382** | `4f97c61be60be6bd78eb90ae0593fd99f9b44089` | `39ee2e7`, `5594d49` |
| `mcp-complete` | `c2ed117` | same author · 2026-10-06 15:41:26 +0200 · *"feat(mcp): extract the MCP layer as a separate project (not integrated yet)"* | `b494404` | **19** | `f5426e8bea1fdf2fba4d83134ce01d98b8b9e5a0` | `93cbecb` |
| `origin/mcp-complete` | `c2ed117` | identical object to local `mcp-complete` | `b494404` | **19** | `f5426e8bea1fdf2fba4d83134ce01d98b8b9e5a0` | `93cbecb` |
| `backup-pre-rewrite` | `caee471` | same author · 2026-10-03 20:35:28 +0200 · *"test(mcp): fixture .mcp_sc_local_env hermetique (audit 2026-10-03)…"* | `cf3bde4` | **392** | `3018c43d7f7c0aaa6eedf3fbaeb2f2d6b76e4b9d` | `39ee2e7`, `5594d49` |
| `backup-pre-squash` | `54eb602` | same author · 2026-10-03 20:35:28 +0200 · *same subject as `backup-pre-rewrite`* | `bdf3ac1` | **10** | `3018c43d7f7c0aaa6eedf3fbaeb2f2d6b76e4b9d` | `93cbecb` |

🔑 **`backup-pre-rewrite` and `backup-pre-squash` have the SAME TREE**
(`3018c43d…`) with **different histories** (392 vs 10 commits). That is the
signature of a pure history rewrite: content preserved, ancestry recreated.

### 1.3 Ancestry — `merge-base --all` + `rev-list --left-right --count`

| A ... B | `merge-base --all` | `--left-right --count` (A←→B) | A ancestor of B | B ancestor of A |
|---|---|---|---|---|
| `main` … `origin/main` | **(empty)** | `18  382` | NO | NO |
| `main` … `mcp-complete` | `b494404` | `0  1` | **YES** | NO |
| `mcp-complete` … `origin/mcp-complete` | `c2ed117` | `0  0` | YES | YES |
| `backup-pre-rewrite` … `main` | **(empty)** | `392  18` | NO | NO |
| `backup-pre-squash` … `main` | `9345615` | `8  16` | NO | NO |
| `backup-pre-rewrite` … `backup-pre-squash` | **(empty)** | `392  10` | NO | NO |
| `backup-pre-rewrite` … `origin/main` | `df3369d` | `10  0` | NO | **YES** |
| `backup-pre-squash` … `origin/main` | **(empty)** | `10  382` | NO | NO |
| `origin/main` … `origin/mcp-complete` | **(empty)** | `382  19` | NO | NO |

### 1.4 Containment of the named commits

`YES` = the commit is an ancestor of that ref (`git merge-base --is-ancestor`).

| Commit | `main` | `origin/main` | `mcp-complete` | `origin/mcp-complete` | `backup-pre-rewrite` | `backup-pre-squash` |
|---|---|---|---|---|---|---|
| `b494404` (N1 fix, `main` tip) | **YES** | – | YES | YES | – | – |
| `c2ed117` (MCP extraction) | – | – | **YES** | **YES** | – | – |
| `df3369d` (`origin/main` tip) | – | **YES** | – | – | **YES** | – |
| `56a755a` (F3 in-flight guard) | YES | – | YES | YES | – | – |
| `7b20b10` (S2 export routes) | YES | – | YES | YES | – | – |
| `10d1ba4` (R1 bounded read) | YES | – | YES | YES | – | – |
| `93cbecb` (root / mcp-S0) | YES | – | YES | YES | – | YES |
| `9345615` (mcp-S1) | YES | – | YES | YES | – | YES |
| `caee471` (`backup-pre-rewrite` tip) | – | – | – | – | **YES** | – |
| `54eb602` (`backup-pre-squash` tip) | – | – | – | – | – | **YES** |

**Every MCP commit in `main` is absent from `origin/main`** — not because the
work is missing, but because the commits were re-created on a new root.

### 1.5 Reconstructed graph

```
line P  (published ancestry — origin/main, backup-pre-rewrite)
  39ee2e7, 5594d49  (two roots, deep history)
    └── … 382 commits …  df3369d  = origin/main  (2026-10-03)
          └── 27a50cb mcp-S0 → 03c5a43 S1 → 1e8effa S2.1 → 6b11544 S2.2a
              → cf41eaf S2.2b → 04ae413 S2.2c → accb49d S2c-suivi
              → c2d2219 fix(bulk network) → cf3bde4 S2.3 → caee471 fixture
              = backup-pre-rewrite (392 commits, 393 files)

   ══ REWRITE #1 : the 382 published commits collapsed into ONE root ══

line S  (squashed ancestry — backup-pre-squash, main, mcp-complete)
  93cbecb  mcp-S0  *** ROOT, NO PARENT, 376 files = origin/main's tree ***
    └── 9345615  mcp-S1
          ├── [backup-pre-squash]  52c151e S2.1 → 09eb750 S2.2c → f34cf06 S2.2b
          │                        → 3faf620 S2.2a → c5f6035 S2c-suivi
          │                        → 6f36041 fix(bulk network) → bdf3ac1 S2.3
          │                        → 54eb602 fixture            (10 commits)
          └── [main]  430d644 fix(bulk network) → 7b20b10 S2 (6 sub-slices
                      squashed into ONE commit) → 0d048bf fixture
                      → 10d1ba4 R1 → cffa0ae R2 → 3c06fdd R3 → 4e09e53 Slice3
                      → 45dbdcc spatial → a00011c F1 → f3f9e7c test(drive)
                      → 5b185a6 F7 → 39d2367 Slice4 → 3424cc1 F4 → fb9a1e8 F2
                      → 56a755a F3 → b494404 N1                (18 commits)
                        └── [mcp-complete / origin/mcp-complete]  c2ed117
                            extraction into mcp/ (19 commits, 410 files)
```

Reflog evidence for the rewrite window (all `+0200`, 2026-10-03):

```
backup-pre-rewrite@{2026-10-03 22:03:48}: branch: Created from caee471
backup-pre-squash @{2026-10-03 23:10:37}: branch: Created from 54eb602
mcp-complete      @{2026-10-06 15:36:33}: branch: Created from HEAD   (b494404)
mcp-complete      @{2026-10-06 15:41:26}: commit: feat(mcp): extract the MCP layer…
```

`main`'s reflog also records two resets (`5b185a6@{2026-10-06 01:18:45}: reset:
moving to HEAD~1`; `3c06fdd@{2026-10-05 16:07:21}: reset: moving to 3c06fdd`).

### 1.6 Consequence

- `git push origin main` **cannot fast-forward** — it would be rejected as a
  non-fast-forward on unrelated histories.
- Any `pull`/`merge` between `main` and `origin/main` would have to pass
  `--allow-unrelated-histories`, and would produce a conflict of the whole tree.
- A PR `main → origin/main` on GitHub would be reported as **unrelated
  histories**.
- `origin/mcp-complete` **is not empty** — it resolves to `c2ed117`, i.e. it is
  already pushed (contradicting the earlier working assumption "origin/mcp-complete
  exists but empty").

---

## 2. Remote safety

**No remote mutation was performed in this session.** No `fetch`, `push`,
`ls-remote`, `remote prune`, ref deletion or update.

The earlier fetch (performed outside this audit) is the reason
`origin/main`/`origin/mcp-complete` exist locally. ⚠️ **Important caveat**:
remote-tracking refs are a *local cache* of the last fetch. Everything reported
about `origin/*` is the state **as of that fetch**, not a live reading of the
GitHub server. Verifying the server's actual refs would require `git ls-remote`
(network, read-only) — **not authorised here** and therefore **not performed**.

---

## 3. File inventory comparison

| Metric | `origin/main` | `main` | `mcp-complete` | `origin/mcp-complete` |
|---|---|---|---|---|
| tracked files | **376** | **406** | **410** | **410** |
| top-level | `.Renviron .Rprofile .gitattributes .gitignore CHANGELOG.md R README.md RENV_SETUP.md app.R archive config global.R i18n modules renv.lock renv reports scripts tests tools` | same **+ nothing new at top level** | same **+ `mcp`** | same **+ `mcp`** |
| `mcp/` files | **0** | **0** | **4** | **4** |
| `scripts/mcp_server.R` | present | present | present | present |
| `R/core/drive_*` | `drive_allowlist.R`, `drive_watcher.R` | same 2 | same 2 | same 2 |
| `tests/testthat/test-mcp*` | 2 | **5** | **5** | **5** |
| `docs/` tracked | **0** (gitignored) | **0** | **0** | **0** |
| `.gitignore` blob | `5052ed0` (1371 B) | `5052ed0` (1371 B) | `c167b95` (1598 B) | `c167b95` (1598 B) |

**`mcp/` (only on `mcp-complete` / `origin/mcp-complete`)** — 4 files, all added
by `c2ed117`:

| Path | Blob SHA | Bytes |
|---|---|---|
| `mcp/README.md` | `1d00c02256565f52631b6a3eb9baff4578322dd9` | 14 419 |
| `mcp/docs/mcp_agent_guide.md` | `8803742ad9a2c9816b7b2258a23c8ceb336a743e` | 10 149 |
| `mcp/docs/mcp_protocol.md` | `52820ec92d073b530b6455de723c1bdc8892dcbf` | 11 832 |
| `mcp/server/mcp_server.R` | `4d2d2de912c8180ffc2dce14055c3b09b0042753` | 199 434 |

`tests/testthat/test-mcp*`:

- `origin/main` (2): `test-mcp-sc-local.R`, `test-mcp-spatial-local.R`
- `main` / `mcp-complete` (5): those two **plus** `test-mcp-guard-inflight.R`,
  `test-mcp-spatial-e2e.R`, `test-mcp-wgcna-local.R`

`.gitignore` rules (identical blob `5052ed0` on `origin/main`, `main`,
`backup-pre-rewrite`, `backup-pre-squash`). The publication-relevant lines:

```
.gitignore            <- the file ignores ITSELF
AGENTS.md
docs/                 <- blanket; this is why docs/ has 0 tracked files everywhere
.zcode
.workbuddy-ai/
QC
mcp.examples/
/python_env_sccoda/
full_suite_results.txt
_e2e_out.txt
_suite_stdout.log
repomix.config.json / repomix.core.config.json / repomix-output.xml / repomix-core-output.xml
tools/_drive/*.json   <- handshake files never committed
tools/_drive/*.tmp
/omnipathr-log/
tests/testthat/tools/
```

`mcp-complete` adds exactly 6 lines (+1 blank, +5 rule lines):

```
+ # --- MCP extraction project (branch mcp-complete) ---
+ # The blanket `docs/` rule above also matches mcp/docs/; the extracted MCP
+ # project's documentation is SOURCE, not local notes — re-include it.
+ !mcp/docs/
+ !mcp/docs/**
```

---

## 4. Content provenance

Path sets compared with `git ls-tree -r` (blob SHA + path):

```
origin/main : 376 paths
main        : 406 paths
shared      : 376   (100 % of origin/main's paths are present in main)
only in main        : 30
only in origin/main : 0
```

### 4.1 Of the 376 shared paths: 352 identical blobs, 24 differing

| Path | blob `origin/main` | blob `main` |
|---|---|---|
| `R/bulk/bulk_network.R` | `af15ed231a4ffae8d9837e3b540d5bdf880a3a5e` | `12b3dc60d10000776a6c0e578b5d49f8df6010e4` |
| `R/bulk/bulk_wgcna.R` | `d981e2b69f67cf2c4a5c782f79b53ac33075a07f` | `8120b3e2049c82e87038d4a9e83fbeef9572ff52` |
| `R/core/drive_allowlist.R` | `e4fd40de32343ee32864cb1173dae5107b31cfb4` | `01ab38c19d02192641874a0e8e695c8c9d37ec57` |
| `R/core/drive_watcher.R` | `378bc32c23be91651d223d8678d7e5bbe13cba01` | `9a631af123a1d707f7bbd0f7572a0a2d1453b8b9` |
| `app.R` | `66e21bae20fc58cd5b9b7332741ace916c011620` | `efe34b616781f773e198463b742c494bf19295f0` |
| `modules/bulk/mod_bulk_filter.R` | `28d835c9610d188a5fcd80f13db97033e771eb9f` | `23c313fdbe92d9b3bce0ce1635397011935a0ebd` |
| `modules/bulk/mod_bulk_network.R` | `5a2e2e6ce544bb9fef9350e12bb14b85a06f0cb9` | `f342dadf2dee17059d7489a95cefd3b2ebbb58fb` |
| `modules/bulk/mod_bulk_pathways.R` | `113281d9782811d064045698660ea456c2a05fbd` | `96b2fa9d92493d05d839b47a1e46dcb298755b87` |
| `modules/bulk/mod_bulk_pattern.R` | `accc222e852346647b6f8ef257f5f89a251f3269` | `dea404dca62e47c4b324a277c23e3d71add4a607` |
| `modules/bulk/mod_bulk_signatures.R` | `88c99d05e7544cc60132be13e3ecb5f5ab9e860e` | `b343a803f2439bc06824fea4866b62d7eb0bfee6` |
| `modules/bulk/mod_bulk_wgcna.R` | `616b109dbb966e6b30831868e53160b48dc19a95` | `a16ac9af03747e9586d839ba3b71bb7e15d36ea5` |
| `modules/bulk_de/mod_bulk_de.R` | `9d5e39bfa640c47a5b68110b54b5335fffffb592` | `06189f04aa055dfb8f94c82b3106fa5471ee0b41` |
| `modules/bulk_de/mod_bulk_de_run.R` | `eb009a646006fa0c423476168021d5c6c95871bf` | `1f1f786df322ee3e33dd2c7c7a93ff9d810c0576` |
| `modules/sc/mod_sc_markers.R` | `813cb321a0b94db33eda970231b5ac4c4445af93` | `b2f502951ee7c5db09f1e06e4bf227f8ae2ab2fd` |
| `modules/sc/mod_sc_pathways.R` | `fc1180d0861cfcd9c0d391ab40968e0887d366a5` | `c0313ae7ab1f7dea83465feb6b3519bbf89c1b87` |
| `scripts/mcp_server.R` | `eb1be41935a52909087c3ef152395ee142769593` | `427279c997e52561708990cd24df20e90d80ce75` |
| `tests/testthat/test-bulk-network.R` | `5dac3bf286761b206259d4290be4c58d112a0084` | `56dd693e519ebc5cdcfd337873db076e61f91c3a` |
| `tests/testthat/test-bulk-wgcna.R` | `4d02f93764900e045b326b81b11d093b40c6edd1` | `b610d1efef57aa115a30d2be9d2ec52f4d35a5ac` |
| `tests/testthat/test-drive-allowlist.R` | `14ca5a9077767866991313b45b1f69db18da57a4` | `641f237de12c341a00a0bb02853685d5b25866be` |
| `tests/testthat/test-drive-watcher.R` | `7823e78a36981e7fd7e7b0712ee9a7d903e5323c` | `47b33d85e434c54f1147d457d77c100efabcae86` |
| `tests/testthat/test-mcp-sc-local.R` | `b0cd19f2ad7e069524ccf2c979753459266c9fd5` | `d6b5f82a9145298234c461285c66d450b2c31484` |
| `tests/testthat/test-mcp-spatial-local.R` | `4f9cbabfc40a6e0a271060256598f70aee10ad23` | `f8b5a258fc09f01209713bbfd9598341d51c6a1d` |
| `tests/testthat/test-mod-import-sc-drive.R` | `d2370976b65ba4609ea9f2222c807e335f554cc8` | `9080ae49c32223fca6277001f9068e35964f19ac` |
| `tests/testthat/test-mod-spatial-qc-export.R` | `6f0e96aa7c7c6f5d41177ff3cb240ec6dcf3ea08` | `1743926d902924dd5fcf876a67209ba4e8835edb` |

**All 24 differences are the MCP/drive work itself** (drive core, the module
export seams, the MCP server, and the MCP/drive test files). No unrelated file
was touched — measured, not inferred from names.

### 4.2 Paths identical on all three refs (spot-check, content-verified by blob SHA)

| Path | blob (identical on `origin/main`, `main`, `mcp-complete`) |
|---|---|
| `global.R` | `864ed082c81…` |
| `tools/launch_dev_drive.R` | `41cd16ccbb7…` |
| `tests/testthat/helper-app-driver.R` | `b82a1573675…` |
| `.Renviron` | `5cfe357a6508c8e292a200c3b946a1faca125a40` |
| `.Rprofile` | `81b960f5c6a8ee40840badd620f8af9743787730` |

### 4.3 First commit introducing each key path

| Path | on `origin/main` | on `main` |
|---|---|---|
| `scripts/mcp_server.R` | `06b826f` 2026-09-27 *"chore(mcp): track scripts/mcp_server.R…"* | `93cbecb` 2026-10-03 *(root — squash)* |
| `R/core/drive_allowlist.R` | `db0785e` 2026-09-21 *"drive: live-session control protocol…"* | `93cbecb` |
| `R/core/drive_watcher.R` | `db0785e` 2026-09-21 | `93cbecb` |
| `app.R` | `39ee2e7` 2026-05-15 *"Premier commit…"* | `93cbecb` |
| `tools/launch_dev_drive.R` | `db0785e` 2026-09-21 | `93cbecb` |
| `tests/testthat/test-mcp-sc-local.R` | `3897228` 2026-09-25 | `93cbecb` |
| `mcp/*` (4 files) | — | `c2ed117` 2026-10-06 |

**Provenance reading**: every path on `main` is "introduced" by `93cbecb`
because the squash made it the root — a *history artefact*, not real provenance.
The genuine origin of each path is the `origin/main` row. **`main`'s history is
therefore unusable for provenance/blame purposes.**

### 4.4 `mcp/` extraction fidelity

`mcp/server/mcp_server.R` (199 434 B) vs `scripts/mcp_server.R` on
`mcp-complete` (198 528 B):

```
git diff --numstat <mcp/server/mcp_server.R> <scripts/mcp_server.R>
0    13   {mcp/server => scripts}/mcp_server.R
```

Zero insertions, 13 deletions ⇒ the extracted copy is the in-repo copy **plus a
13-line provenance header**. `c2ed117`'s message states it is "a verbatim
snapshot of `scripts/mcp_server.R` at `b494404`, provenance-stamped" — **measured
and confirmed**.

---

## 5. Sensitive / publication audit

Nothing was deleted, untracked or moved. **No secret value is printed below.**

| Path | Tracked? | Blob | Classification | Notes |
|---|---|---|---|---|
| `.Renviron` | **yes** (all refs) | `5cfe357a…` (409 B, 6 lines) | **required source** | Contains exactly one setting: `RENV_CONFIG_SANDBOX_ENABLED = FALSE` + a French comment. **No credential, token or path.** Safe to publish. |
| `.Rprofile` | **yes** (all refs) | `81b960f5…` (26 B) | **required source** | One line: `source("renv/activate.R")`. No secret. |
| `config/` | yes (3 files) | — | **required source** | `config/.gitkeep`, `config/defaults.R`, `config/thresholds.R`. Constants only. |
| `archive/` | yes (3 files) | — | **disposable archive** | `archive/2026-09-25_mcp_tasks_1_3/` — README + 2 baseline manifests. |
| `reports/` | yes (4 files) | — | **required source** | 4 `.Rmd` report templates. |
| `scripts/` | yes (4 files) | — | **required source + reproducible tool** | `mcp_server.R` (source), `benchmark_stage18.R`, `renv_bootstrap.R`, `verify_release_gates.R` (tools). |
| `tools/` | yes (12 files) | — | **reproducible test/tool** | Guards + runners. **`tools/_drive/README.md` is tracked on purpose** (the protocol doc); the JSON handshakes are ignored by `tools/_drive/*.json`. |
| `docs/` | **no — 0 tracked** | — | **public documentation (untracked)** | 126 files on disk under `docs/`, `docs/archive/`, `docs/audits/`, `docs/contracts/`, `docs/proposals/`, `docs/release/`. The blanket `docs/` rule in `.gitignore` means **none of it is published**. |
| `mcp/` | only on `mcp-complete` | see §3 | **public documentation + required source** | `mcp/docs/*` is documentation the branch deliberately **re-includes** via `!mcp/docs/`; `mcp/server/mcp_server.R` is source. **Not present on disk** in the current worktree (branch not checked out). |

**Secret scan** over the tracked tree of `main` for secret-bearing filenames
(`.env`, `.netrc`, `.npmrc`, `.pypirc`, `id_rsa`, `*.pem|key|p12|pfx|crt`,
`*secret*`, `*credential*`, `*password*`, `*token*`): **zero matches.**

⚠️ **Untracked-but-present material that must NOT be published**: `docs/`,
`.workbuddy-ai/` (agent memory, audits, temp drivers), `QC/`, `mcp.examples/`,
`python_env_sccoda/` (18 003 files), `renv/` (2 899 files), repomix outputs.
All are already covered by `.gitignore`.

**Proposed actions (proposals only — nothing executed):**

1. Keep `.Renviron`/`.Rprofile` tracked; both are verified secret-free.
2. Decide explicitly whether `docs/` should stay unpublished. It currently holds
   126 files of project truth (`STATUS.md`, `PITFALLS.md`, roadmaps, contracts)
   that a fresh clone cannot see — a **reproducibility** decision, not a
   security one.
3. `archive/2026-09-25_mcp_tasks_1_3/` is a candidate for archival removal from
   the tree, but it is 3 small files — **leave it** unless the owner wants a
   clean tree.
4. Add a CI/local check that fails on a tracked file matching the secret-name
   patterns above (currently zero, so the check would be a ratchet).

---

## 6. Architecture report (proposals only — nothing moved)

### 6.1 What the topology tells us

The repository currently carries **two incompatible histories for one project**,
and the MCP layer exists in **three places**:

| Location | Ref(s) | Content |
|---|---|---|
| `scripts/mcp_server.R` | **all** refs | the live, in-app server (198 528 B on `main`) |
| `mcp/server/mcp_server.R` | `mcp-complete` only | a provenance-stamped snapshot (+13 lines) |
| `scripts/mcp_server.R` | `origin/main` | the **older** 127 937 B version |

Because `main` and `origin/main` are unrelated, **the extraction branch
(`mcp-complete`) is built on a root that the remote cannot see.**

### 6.2 Proposals

**Canonical MCP server location.** Keep `scripts/mcp_server.R` in the app
repository as the single executable source while the server still sources the
drive core from the app checkout (its `cwd` is the Cerberus root). Do **not**
promote `mcp/server/mcp_server.R` to canonical while it is a stamped copy —
two canonical copies will drift. If the extraction is to become real, it must be
a *move* (one location, a thin wrapper in the other), not a duplicate.

**App-side drive adapter.** Stays where it is: `R/core/drive_watcher.R` (the
poller/verdict engine) and `R/core/drive_allowlist.R` (frozen tables). These are
the protocol's app half and belong to the app, not to the MCP project.

**Shared protocol contract.** `tools/_drive/README.md` (tracked) is the only
published description of the four-file handshake. It is the natural home for the
frozen contract; if `mcp/docs/mcp_protocol.md` duplicates it, one must be
declared derivative and the other canonical — **do not maintain two prose
copies of a frozen contract.**

**Tests.** The 5 `tests/testthat/test-mcp*.R` + the `test-drive-*` /
`test-mod-*-drive.R` families exercise the app-side seams and must stay with the
app. An extracted MCP project would need its own harness, which does not exist
yet.

**Docs.** `docs/` is unpublished by design. If the MCP project is extracted for
external consumers, `mcp/docs/` is the correct publication surface (the branch
already re-includes it).

**Compatibility wrapper strategy.** If the server moves out, keep
`scripts/mcp_server.R` as a **thin re-exporting shim** that sources the moved
file, so existing MCP client configs (`mcp.examples/*.json`, `.zcode/config.json`,
`~/.workbuddy-ai/mcp.json`) keep working unchanged. Deleting the shim in the
same change would break every configured client at once.

**Branch/repository strategy.** This is the decision the topology actually
forces, and it is **not** a mechanical one:

- Option A — **re-attach `main` to the published line.** Replay the 18 squashed
  commits onto `origin/main` (`rebase --onto origin/main 93cbecb main`, or a
  cherry-pick of the 16 post-root commits) so the two histories share an
  ancestor and a normal fast-forward push becomes possible. Cost: a second
  rewrite, and the commits keep their messages but get new SHAs again.
- Option B — **declare `main` canonical and retire `origin/main`.** Accept the
  severed history, push `main` with a forced update (or as a new default
  branch), and keep `backup-pre-rewrite` as the archive of the published line.
  Cost: the public history loses its continuity; anyone who cloned
  `origin/main` is orphaned.
- Option C — **separate repositories.** Keep Cerberus on the published line and
  put the MCP layer in its own repository with `mcp-complete` as its first
  history. Cost: the extraction is not yet self-contained (the server still
  sources the drive core from the app checkout).

**The two backup branches are the safety net for all three options and must not
be deleted until the owner chooses.** `backup-pre-rewrite` is the only ref that
still holds the complete published ancestry.

---

## 7. Stop condition

- **No changes made.**
- **No push performed.**
- **No force-push performed.**

### Exact next decision required from the owner

> **Which line is canonical — the published `origin/main` ancestry, or the
> rewritten `main`?**
>
> Concretely, choose one:
> 1. **A — re-attach**: authorise replaying `main`'s 18 commits onto
>    `origin/main` so the histories share an ancestor (a second, SHA-changing
>    rewrite; `backup-pre-rewrite` remains the safety net).
> 2. **B — `main` wins**: authorise treating the severed history as the new
>    truth and force-updating the remote (published history is orphaned).
> 3. **C — split**: authorise keeping Cerberus on the published line and giving
>    the MCP layer its own repository, which requires first making the extracted
>    server self-contained (it currently sources the drive core from the app
>    checkout).
>
> A secondary, independent decision is also required: **should `docs/` (126
> untracked files) be published?** It is currently invisible to any clone.
>
> **No action of any kind will be taken until the owner answers.**

*End of report. No changes made. No push performed. No force-push performed.*

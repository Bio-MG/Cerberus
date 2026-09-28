# MCP propagation — v2 record

This is the narrow MCP propagation record for the scope frozen on 2026-09-25. It
is not a reconciliation of `docs/STATUS.md`; it records the Tasks 1–3 work, the
first Single-Cell button propagation, and the manifest convention used for the
anchors around them.

## 1. Manifest algorithm v2 and anchors

The manifest algorithm is v2. The following command is recorded verbatim:

```
find . -path ./renv -prune -o -path ./python_env_sccoda -prune -o -path ./.git -prune \
  -o -path ./QC -prune -o -path ./.Rproj.user -prune -o -path ./node_modules -prune \
  -o -path ./.workbuddy-ai -prune -o -path ./tools/_drive -prune \
  -o -type f ! -name full_suite_results.txt ! -name .RData ! -name .RDataTmp \
  ! -name .Rhistory -print0 | sort -z | xargs -0 sha256sum
```

The exclusion set is therefore `renv/`, `python_env_sccoda/`, `.git/`, `QC/`,
`.Rproj.user/`, `node_modules/`, `.workbuddy-ai/`, `tools/_drive/`, and files
named `full_suite_results.txt`, `.RData`, `.RDataTmp`, or `.Rhistory` at any
included level. `tools/_drive/` is excluded because it is the live IPC channel.

⚠️ **THE ALGORITHM ABOVE, RUN VERBATIM, DOES NOT REPRODUCE THE PUBLISHED §1.4
ANCHOR — by exactly one file.** Re-measuring `c0e645f` for Phase E gave
**327 entries / `912d5857c92314f50d59cf31b587fc9328832f0163df48970e02c7c62e38d45b`**,
while §1.4 records 328 / `63357a01…`. The difference is
`./tools/_drive/README.md`: it is **tracked by git** (so it is inside a
`git archive` extraction) and it is exactly the kind of file
`-o -path ./tools/_drive -prune` exists to remove. A first PowerShell port of the
algorithm pruned by **directory NAME** instead of by **PATH**, which let that one
file through and reproduced `63357a01…` / 328 — a false confirmation, and the
clearest possible demonstration that a port validated against a single anchor can
still be wrong. The port now prunes by ancestor PATH, as `-path` does, and the
§1.4 row is kept below as published with its deviation stated rather than silently
replaced: the algorithm TEXT is the spec, and every Phase E value below follows it.
Anyone comparing a new value with `63357a01…` must add or remove that one file.

| State | Fingerprint | Entries | Meaning |
|---|---|---:|---|
| **pre-Phase A** — post-housekeeping v2 baseline | `044e3944ea7358d3697c7238d59b53059cbfb1d623c9b651afcfab0cff149494` | 436 | Clean baseline before any SC button propagation |
| **post-Phase A** — after the first SC button | `e2a04cde4bd562bd53dcef64e58a3adf26123ad4e8062237bedafb4c88115d15` | 438 | After the `sc-annotation-run_annot` propagation |
| **post-Phase A** — final | `674afd5bfe1bf13325567b6531c08d5103fd162fb87be921595ed6a9143dcae8` | 438 | Phase A end state, after the maxcells / isolation pass. Recorded during Phase A; not re-measured since. |
| **post-Phase B** — baseline (v2) | `9b82d3d6abd1f56e9ad6da6792947606070fb18b9ca5cefd997dfb9ab9ad96cd` | 441 | Commit `3897228`. `run_pipeline` covers **8 modules / 9 buttons**, the SC drive wrappers publish `n_results = 0L` on error, and `docs/bulk_mcp_extension.md` is part of the record. |

The post-Phase B row is the first anchor whose **content** is committed: every
tracked file it covers is exactly the content of `3897228`. The three earlier
values remain the accepted anchors for the states they describe; they are not
comparable with the post-Phase B value except as "before" and "after".

### 1.1 How the post-Phase B value was taken, and what is in the population

Measured on the working tree immediately after commit `3897228`, with the
algorithm of §1 verbatim. Composition of the 441 entries, relative to the 438 of
the post-Phase A state:

- `+1` `docs/bulk_mcp_extension.md` (the Phase B design note).
- `+2` files owned by a concurrent agent's commits, not by this propagation:
  `tests/testthat/test-mod-bulk-de-run.R` and
  `tests/testthat/test-sc-communication-liana-engine.R`.
- `0` from Phase B's code, which modified existing files only.

The tree was **not quiescent** when the value was taken: the uncommitted Spatial
pipeline work, `renv.lock`, `archive/` and
`tests/testthat/test-mcp-spatial-local.R` are in the population as well, and
`HEAD` had moved `b3cbe11 → 9cb1a06` earlier in the same window. A quiescent
tree would report a smaller, different number; the value above is therefore
reproducible only for this exact working-tree state.

A fingerprint cannot include its own recording: this file is itself a manifest
member, so writing the value changes it. Two intermediate measurements of the
same 441-entry population, taken while this file was being edited, were
`9685302cd3739fd50d6904eb05b756f0899801057502c9d478e274c29be1acaa` and
`b8016637eddac47ef7588d3adb3ba38a288310d7797a8654a2a7572db63e16e9`. Re-measure to
compare trees; never compare against a number written inside one of them.

### 1.2 A reproducible post-Phase B anchor

The live-tree value above cannot be reproduced from the commit, because the
population it hashes also contains the foreign working-tree files. Applying the
same algorithm to the **committed content only** removes that dependency, and
the result is immutable: `git archive` of a commit always yields the same bytes.

| Anchor | Fingerprint | Entries | How to reproduce |
|---|---|---:|---|
| **post-Phase B** — commit `3897228`, tracked content only | `7b5adf06864eb682d4575ee784f974dd375d6d1cddbfeac2299f20ac2430d1fc` | 327 | `git archive --format=zip -o c.zip 3897228`, unzip, run the §1 algorithm in the extracted root |

327 entries, not 329: `renv/activate.R` and `renv/settings.json` are pruned by the
algorithm, exactly as in the live-tree run. This is the value to compare against
when checking whether a later commit changed anything the propagation owns.

### 1.3 Post–Phase C (live tree)

| Measurement | Fingerprint | Entries | Composition |
|---|---|---:|---|
| After the `bulk-signatures-run_signatures` drive action | `cc53ad5e7aea8c4abbfeffc6037693f3b62ebe2098aaaede19736b665d46ee6e` | 442 | 441 at the post-Phase B state, `+1` `tests/testthat/test-mod-bulk-signatures-drive.R`, and 4 modified files (`R/core/drive_allowlist.R`, `R/core/drive_watcher.R`, `modules/bulk/mod_bulk_signatures.R` and two tests) |

The tree now includes the `bulk-signatures-run_signatures` drive action:
`run_pipeline` covers **9 modules / 10 buttons** and the MCP inventory remains at
**7 tools**. The same caveat as §1.1 applies — the population still contains the
uncommitted Spatial work, `renv.lock`, `archive/` and
`tests/testthat/test-mcp-spatial-local.R`, and `HEAD` had not been committed for
Phase C when this was measured. A commit-scoped anchor should be added with the
Phase C commit; the value above is reproducible only for this working-tree state.

Two facts measured while implementing it, both of which changed the code:

- **The job status vocabulary is NARROWER than the view vocabulary.**
  `ts_drive_job_set_pending()` accepts only `done` / `error` / `invalid` /
  `timeout` / `session_lost`. An empty signature result therefore closes the job
  as `done` while the published view reports `empty`; passing `"empty"` would
  have been refused, leaving the job `running` until its timeout.
- **`scripts/mcp_server.R` keeps its OWN `setdiff()` list of modules that refuse
  `set_inputs`.** Adding an allowlist module without adding it there would have
  left the new action settable from a remote caller — the one failure direction
  that matters. The two lists (this one and the guard in
  `ts_drive_apply_scenario()`) are not mechanically tied together; the only
  automated check that they agree is `test-mcp-sc-local.R`.

Also measured, and deliberately NOT added: no `TS_BULK_SIGNATURES_TIMEOUT_S`.
Both existing Bulk long jobs (`bulk-pathways-run_pathway`,
`bulk-pathways-run_scores`) publish with `long = TRUE` and no `timeout_s`, so the
poller's default ceiling applies. This supersedes §6.3.6 of
`docs/bulk_mcp_extension.md`, which assumed a declared constant was needed.

### 1.4 Post–Phase C (committed)

| Anchor | Fingerprint | Entries | How to reproduce |
|---|---|---:|---|
| **Post–Phase C** — commit `c0e645f`, tracked content only | `63357a01bacbea3b446ee17497ef73fc91668519f0407a7d0f3a0c99f1f4b47f` | 328 | `git archive --format=zip -o c.zip c0e645f`, unzip, run the §1 algorithm in the extracted root |
| **Post–Phase C**, re-measured under §1 verbatim | `912d5857c92314f50d59cf31b587fc9328832f0163df48970e02c7c62e38d45b` | **327** | same, with `tools/_drive/` pruned **by path** — see the ⚠️ at the top of §1 |
| **Post–Phase D** — commit `9015044`, tracked content only | `25ca14602bb0610bcfc718c281b1964ecb97a7d843f36068183050ce4c56e4cf` | **332** | `git archive --format=zip -o c.zip 9015044`, unzip, run the §1 algorithm in the extracted root |

This is the **reproducible, commit-scoped** value, and it is the one to compare
against from now on. Unlike the live-tree value in §1.3, it depends on nothing
outside the commit: no uncommitted Spatial work, no `renv.lock`, no `archive/`,
no concurrent agent's commits. `git archive` of a commit always yields the same
bytes, so the number cannot drift.

- 328 entries, not 330: `renv/activate.R` and `renv/settings.json` are pruned by
  the algorithm, exactly as in every other run of it.
- 327 at `3897228` → 328 here: `+1` for
  `tests/testthat/test-mod-bulk-signatures-drive.R`. The other six committed
  files changed CONTENT, not membership.
- This commit contains the §1.3 fingerprint record itself, so §1.3's live-tree
  value (`cc53ad5e`, 442 entries) remains the correct description of the
  pre-commit working tree, and this row is the description of the commit.
- ⚠️ The 328/327 discrepancy is `tools/_drive/README.md`, and it is a property of
  the PORT, not of the commit: see the ⚠️ at the top of §1. The 327 row is the one
  that follows the algorithm as written.
- **Phase D** (`9015044`) is 332 under the same rule: `+4` over Phase C's 327, for
  the two new tests (`test-mod-bulk-pattern-drive.R`,
  `test-mcp-spatial-local.R`), `archive/2026-09-25_mcp_tasks_1_3/` (3 files), and
  nothing else in membership — the eleven modified sources changed CONTENT only.
  §1.5's live-tree value for Phase D is superseded by this row, as §1.5 itself asked
  for.


⚠️ `scripts/mcp_server.R` is **not** in this commit: it is gitignored
(`.gitignore:43`). The action is therefore committed on the app side — allowlist,
dispatcher, module, tests — while the `run_pipeline` mapping that exposes it to a
client lives in an unversioned file. It is consistent with the commit
(`--check` reports 9 modules / 10 buttons), but a fresh clone has no
`transcripto_drive_run` mapping for `bulk_signatures` until that file is tracked.
Deciding whether the MCP server belongs in version control is a repository-policy
question, deliberately not taken here.


### 1.5 Post–Phase D (live tree)

| Measurement | Fingerprint | Entries | Composition |
|---|---|---:|---|
| After the `bulk-pattern-run_pattern` drive action | `a56f662e19957afbb1156523ffa7ef5816694a54b7d8a24239378ca7ace9aea2` | 446 | 442 at the post-Phase C live-tree state, `+1` `tests/testthat/test-mod-bulk-pattern-drive.R`, plus the modified sources: `modules/bulk/mod_bulk_pattern.R`, `R/core/drive_allowlist.R`, `R/core/drive_watcher.R`, `scripts/mcp_server.R` and two tests |

The tree now includes the `bulk-pattern-run_pattern` drive action:
`run_pipeline` covers **10 modules / 11 buttons** and the MCP inventory remains at
**7 tools**. The same caveat as §1.1 and §1.3 applies — the population still
contains the uncommitted Spatial work, `renv.lock`, `archive/` and
`tests/testthat/test-mcp-spatial-local.R` — so this value describes this working
tree, not a commit. It is recorded for review and **not committed**; a
commit-scoped anchor (§1.2 pattern) should be added when Phase D is committed.
✅ That anchor now exists, in the §1.4 table: `9015044` = **332 / `25ca1460…`**.

### 1.6 Post–Phase E (live tree)

| Measurement | Fingerprint | Entries | Composition |
|---|---|---:|---|
| **Post–Phase E**, after the three new actions and the §8–§10 live chains | `0b44b54211d3277b47500dabdfcb513737fccb8a390f9a006f88b3e27aaaea6c` | **444** | 332 (the `9015044` commit) `+3` for the Phase E tests `+109` for pre-existing untracked/gitignored files that no commit contains |

`run_pipeline` covers **13 modules / 14 buttons**; the MCP inventory is still
**7 tools**. Measured on the working tree with the §1 algorithm, pruning
`tools/_drive/` by path.

⚠️ **This value is self-referential, and the difference is exactly one file.**
It was measured with §1.6/§8–§11 already written, so the only content the
measurement does NOT include is the fingerprint line you are reading: writing that
line back changed `docs/mcp_propagation.md`, hence the manifest, hence the value.
This is the same property every live-tree row in §1 has, and it is exactly why
§1.4's commit-scoped anchors exist — a live-tree fingerprint can never describe a
tree that contains its own description of itself. The Phase E value to compare
against is the **332 / `25ca1460…`** commit-scoped row for `9015044`, plus the three
test files named below.

⚠️ **The `+109` is not Phase E work and must not be read as such.** It is the
`docs/` tree (89 files), `mcp.examples/` (6), `repomix.config.js…`,
`scripts/mcp_server.R`, `.zcode/` (4), `.posit/`, `.gitignore`, `AGENTS.md`,
`SHINYAPP test.Rproj` and the empty `QC` marker — all of them present in the
working tree and absent from every commit, because `.gitignore` excludes `docs`
and `scripts/mcp_server.R` among others. That is a long-standing property of this
manifest and it is the reason §1.4 exists: a **commit-scoped** value is the only
one that is comparable. **Exactly three** of the 112 new entries are Phase E:
`tests/testthat/test-mod-bulk-network-drive.R`,
`tests/testthat/test-mod-sc-pathways-drive.R` and
`tests/testthat/test-mod-spatial-qc-drive.R`. Everything else Phase E touched is a
CONTENT change to a file already inside `9015044`.

### 1.7 Post–Phase F (live tree), and TWO corrections to the anchors above

| Measurement | Fingerprint | Entries | Composition |
|---|---|---:|---|
| **Post–Phase F**, after the `import_spatial` importer, the Spatial hotspot CSV export, the i18n key and the Phase F tests | `a8943d60c96caf0167a89fa3ad45d0dae1ef5cc32042ae8c832db8f4d38994d3` | **446** | the 444 of §1.6 `+1` `tests/testthat/test-mod-import-spatial-drive.R` `+1` `i18n/translation.json` (a content change) — and the self-reference described below |

`run_pipeline` now covers **14 modules / 15 buttons**; the MCP inventory is still
**7 tools**. No statistical method, default parameter or plot was touched.

⚠️ **Self-referential, as every live-tree row is.** The value was measured with
§1.7 and §8–§11 written but with the fingerprint line itself still absent, so
writing that line back changed `docs/mcp_propagation.md`, hence the manifest,
hence the value. The stable, comparable figure is the entry count and the
population; the hash is reproducible only for this exact byte state.

#### 🔴 The live-tree fingerprint on THIS host is NOT stable across a `git stash` round-trip

MEASURED, twice, in one session. The port is deterministic on a quiescent tree —
two consecutive runs with no git operation in between returned byte-identical
values. But:

| State | Entries | Fingerprint |
|---|---:|---|
| live tree, before any `git stash` | 446 | `41201c2d1850bda94142cc510614e86b7c892e5e7a4fcf3cb9a7d38503dd9744` |
| live tree, **after** `git stash push --include-untracked` + `git stash pop` | 446 | `a8943d60c96caf0167a89fa3ad45d0dae1ef5cc32042ae8c832db8f4d38994d3` |

The entry count is unchanged and `git status` reports the same 20 paths, so the
**file set did not move — the file BYTES did.** The cause is the repository's
mixed LF/CRLF state with `core.autocrlf = true` (already recorded in
`AGENTS.md` §2ca): a stash round-trip rewrites the files it touches, and some
come back with the other line ending. Two consequences, and they matter more than
the hash:

1. **Any Phase-to-Phase live-tree comparison that used a stash round-trip is
   unreliable**, including the "before" figure of this very section. A 446-entry
   manifest cannot distinguish "Phase F changed 5 files" from "a checkout
   flipped 40 line endings".
2. **The commit-scoped anchors of §1.2/§1.4 are the only trustworthy
   comparison**, which is the reason they exist — and §1.7 below shows the
   published one cannot be reproduced under the algorithm at all.

#### 🔴 Neither published anchor follows the §1 algorithm, and §1.2 is wrong about its own arithmetic

§1 records this for **§1.4**. It is true of **§1.2** as well, and the entry count
proves it. `git ls-tree -r 3897228` lists **329** tracked files. The algorithm's
own prunes are three, not two:

| Pruned by the algorithm | Why |
|---|---|
| `renv/activate.R` | under `-path ./renv -prune` |
| `renv/settings.json` | under `-path ./renv -prune` |
| **`tools/_drive/README.md`** | under **`-path ./tools/_drive -prune`** — and it is TRACKED by git, so it is present in a `git archive` extraction |

`329 − 3 = 326`, which is what the corrected port produces. §1.2 states "327
entries, not 329: `renv/activate.R` and `renv/settings.json` are pruned" — that
arithmetic reaches 327 only by **not** pruning `tools/_drive/README.md`, i.e. by
making exactly the mistake §1 attributes to the first PowerShell port. So the
population under the algorithm is:

> **326 entries for `3897228`**, fingerprint `cab962087e77cb78022be4f463f83808cd7a89d8050c677961b297b7863cc02f`
> (ordinal sort, `<hash>  ./<path>` lines, sha256 over the listing).

**How this port is validated, given that it reproduces neither published value.**
Not by a hash — that is impossible, since both published values are known to
include a file the algorithm excludes. It is validated on the **POPULATION**,
which is the part that can be checked without trusting the earlier run:

* the port's file set is **exactly** `git ls-tree -r --name-only 3897228` minus
  those three paths, and nothing else — a stronger statement than a hash match,
  because a hash cannot say *which* file differs;
* it is **deterministic**: two consecutive runs on a quiescent tree are
  byte-identical;
* it prunes **by ancestor PATH**, not by directory NAME, which is the specific
  defect §1 warns produced a false confirmation.

Anyone comparing a new value with `7b5adf06…` or `63357a01…` must add
`tools/_drive/README.md` back, or compare entry counts against 327 rather than
326.

### 1.8 Post-merge `cline/8283c` — the branch reconciled with main and its declared actions wired

| Measurement | Fingerprint | Entries | Composition |
|---|---|---:|---|
| **Post-merge**, commit `9f5cdac` (merge of main + the SC/Bulk audit work + the completed drive actions + the LIANA/OmniPathR pins), measured on the COMMITTED tree | `1fd17872c43cffa806900eef31727c4fd3ac7cfd39abede0d968d026c195a5cb` | **380** | full test suite at this commit: **0 FAIL / 10318 PASS / 3 SKIP** (shinytest2 e2e included, headless Chrome); `run_pipeline` covers the 14 modules / 15 buttons of Phase F — and the three buttons §1.6 declared but whose modules never shipped a `ts_drive_publish_token()` (`bulk-network-run_network`, `sc-pathways-run_pathway`, `import_spatial-btn_import`) are NOW WIRED in this tree |

⚠️ **This anchor is COMMIT-SCOPED, not a verbatim §1 run on the working tree.**
The working-tree algorithm on this host picks up untracked runtime artefacts
(`omnipathr-log/`, `tests/testthat/tools/` — both now gitignored) and the
`python_env_sccoda/` venv, so a verbatim run is polluted and unstable. The
value above extracts `git archive 9f5cdac` and applies the §1 exclusions
verbatim inside the extraction (the recipe of `tools/verify_committed_tree.R`:
"measure the tree a COMMIT ships"). Entry counts are comparable across
commit-scoped anchors; the hash is reproducible only for this exact commit.

## 2. Scope summary

Tasks 1–3 covered, at a high level:

- **Task 1:** the Drive job lifecycle, including owned dispatch, terminal
  publication, timeout and session-loss outcomes, and truthful `running` versus
  terminal states.
- **Task 2:** the Spatial false-success fix, so a failed or invalid stage is
  not reported as a successful pipeline.
- **Task 3:** minimal Spatial MCP propagation, including its single owned
  button, readiness refusal, fixed navigation, and sanitized diagnostics.

The `sc-pipeline-run_auto_pipeline` drive action was already part of the
pre-Phase A baseline. It calls `run_sc_auto_pipeline()` with a frozen, declared
input set, publishes a long-job state, and reports per-step outcomes using only
the existing five-state vocabulary. The human modal button was not bound as a
drive action.

## 3. MCP inventory, per state

The MCP inventory contains **7 tools** and never gained a tool during any of the
propagation steps:

| Tool | Nature |
|---|---|
| `transcripto_drive_status` | read-only |
| `transcripto_drive_read_result` | read-only |
| `transcripto_drive_snapshot` | passive, read-only |
| `transcripto_drive_set_inputs` | controlled write, `scenario.json` only |
| `transcripto_drive_run` | controlled run, `run_pipeline` only |
| `transcripto_drive_wait` | bounded observation |
| `transcripto_drive_set_armed` | arm/disarm, `arm.json` only |

`run_pipeline` coverage grew by exactly one module and one button per phase:

| State | Modules / buttons | Delta |
|---|---|---|
| **pre-Phase A** — `044e3944`, 436 entries | **6 / 7** | baseline table below |
| **post-Phase A** — `e2a04cde`, 438 entries | **7 / 8** | `+ sc_annotation` → `sc-annotation-run_annot` |
| **post-Phase B** — `9b82d3d6`, 441 entries, commit `3897228` | **8 / 9** | `+ sc_markers` → `sc-markers-run_markers` |
| **post-Phase C** — `cc53ad5e`, 442 entries (§1.3, uncommitted) | **9 / 10** | `+ bulk_signatures` → `bulk-signatures-run_signatures` |
| **post-Phase D** — `a56f662e`, 446 entries (§1.5, uncommitted) | **10 / 11** | `+ bulk_pattern` → `bulk-pattern-run_pattern` |
| **post-Phase D committed** — `25ca1460`, 332 entries, commit `9015044` | **10 / 11** | the commit-scoped anchor for the row above |
| **post-Phase E** — `0b44b542`, 444 entries (§1.6, live tree) | **13 / 14** | `+ bulk_network` → `bulk-network-run_network`, `+ sc_pathways` → `sc-pathways-run_pathway`, `+ spatial_qc` → `spatial-qc-btn_hotspots` |

⚠️ **Phase E is the first phase that added THREE modules at once, and it is the
first that widened two CLOSED rails.** The `sc-` set went from three declared keys
to four, and the `spatial-` set from one to two. Both rails exist precisely so that
this shows up as a visible edit rather than a default, and both edits are in
`ts_drive_allowlist_problems()` with the names spelled out. The three new actions
also span all three solutions to the "undeclared parameter" problem, which is why
they are a useful set:

| Action | The undeclared parameter | How it is resolved |
|---|---|---|
| `bulk-network-run_network` | none — measured, not an omission | the active contrast is a **prerequisite**, gated by readiness, never a parameter |
| `sc-pathways-run_pathway` | the gene source | a **declared prerequisite**: the marker table, with readiness naming the step that is missing |
| `spatial-qc-btn_hotspots` | the QC metric | a **rule** over a declared preference order, refusing `not_ready` when none is usable |

The three are also the three that make `n_results` mean three different things
honestly: retained **nodes**, enriched **pathways**, and **significant spots** —
the last deliberately NOT `nrow()`, because a per-element table would report ~1000
for a run that found two hotspots.

The pre-Phase A baseline table:

| Module | Drivable button |
|---|---|
| `import_bulk` | `import_bulk-btn_load` |
| `bulk_filter` | `bulk-filter-run_filter_norm` |
| `bulk_de` | `bulk-de-run_de` |
| `bulk_pathways` | `bulk-pathways-run_pathway` |
| `bulk_pathways` | `bulk-pathways-run_scores` |
| `spatial_pipeline` | `spatial-pipeline-btn_run_all` |
| `sc_pipeline` | `sc-pipeline-run_auto_pipeline` |

`set_inputs` is refused for every module except `import_bulk`: their inputs are
frozen at the action boundary and are not a settable Shiny-input surface. The
refusal covers `spatial_pipeline`, `sc_pipeline`, `sc_annotation`,
`sc_markers` and, since Phase C, `bulk_signatures` — where it also keeps the
`fileInput` path `sig_rds` out of reach of a remote caller.

## 4. The two added SC actions (Phase A, then Phase B)

Phase A and Phase B each added exactly one action, with the frozen input set
declared at the action boundary:

| Phase | Module | Drivable button | Frozen inputs |
|---|---|---|---|
| A | `sc_annotation` | `sc-annotation-run_annot` | `ref_singler = "hpca"`, `label_level = "main"`, `maxcells = 50000L` |
| B | `sc_markers` | `sc-markers-run_markers` | `marker_test = "wilcox"`, `marker_min_pct = 0.10`, `marker_logfc = 0.25`, `group_col = "seurat_clusters"`, `max_per_group = 5000L`, `only_pos = TRUE`, `verbose = FALSE` |

Both call their corresponding R action through a published drive token, do not
bind the DOM button, and publish a long-job state with the existing five-state
step vocabulary.

The **post-Phase B** mapping is therefore **8 modules / 9 buttons**: the seven
buttons of the pre-Phase A table plus the two rows above.

### 4.1 The first Bulk action outside import/filter/DE/pathway (Phase C)

| Module | Drivable button | Frozen inputs |
|---|---|---|
| `bulk_signatures` | `bulk-signatures-run_signatures` | `sig_resource = "hallmark"`, `sig_organism = "human"`, `sig_method = "ssgsea"`, `sig_min_size = TS_BULK_GSVA_MIN_SIZE` (10), `sig_max_size = TS_BULK_GSVA_MAX_SIZE` (500) |

`run_signatures()` composes `bulk_load_signatures()` then
`bulk_score_signatures()` — the same two calls, in the same order, as the human
observer — after an availability pre-check on
`bulk_signature_resources()$available`, which is local-only: an unavailable
resource is refused with `state = "missing_dependency"` BEFORE a job is
declared. `sig_rds` is a `fileInput` path and is deliberately absent from the
frozen set, so it cannot be injected. `R/bulk/bulk_signatures.R` is **untouched**:
its exported surface is frozen by `test-bulk-signatures-contract-freeze.R`, which
would reject a new public name, so the wrapper is private to the module. The
**post-Phase C** mapping is **9 modules / 10 buttons**. The MCP inventory remains
**7 tools**, and no Bulk button's behaviour changed.

## 5. Live validation: bulk_signatures

**VERDICT: PASS (qualified) — after a fix, re-validated live on 2026-09-25.**

The first pass (recorded in 5.5) returned FAIL on one criterion: the drive path
stored a bare matrix where the slot holds the scoring record, which broke the
panel while the drive state still reported `done`. That defect is **fixed and the
fix is verified live**. One criterion — the heatmap image and the data table
actually rendering — could **not** be positively confirmed in this session, for a
reason that is demonstrably not attributable to this action (5.4). On that basis
`bulk-pattern-run_pattern` is unblocked as a Phase D candidate.

### 5.1 Session and dataset

- Real Chrome tab with the CDP pilot attached, `ready.json.viewer = "visible"`,
  heartbeat `hb_n` climbing (8 → 14 after arming, 97 → 99 near the end), armed via
  `arm.json`. Session identity pinned by the derived `session_id`, never the token.
- Dataset: **synthetic** counts, 427 genes × 8 samples, two conditions and one
  batch. Gene identifiers are drawn from the app's own offline MSigDB Hallmark
  catalogue (three complete sets, so each reaches 100% overlap and a size inside
  the `[10, 500]` band); the counts themselves are generated. No real sample,
  patient or probe identifier is recorded anywhere in this document. Staged in the
  app's `tempdir()`, which is an allowlisted import root.

### 5.2 The fix, and what it changed

- New single writer `.bulk_signatures_store(res, global_data, shared_rv)` in
  `modules/bulk/mod_bulk_signatures.R`, called by **both** the human observer and
  the drive wrapper. It stores the whole record in `shared_rv$signature_scores`
  and the matrix in the contractual `bulk_obj$pathways$signatures`, and it
  **refuses** anything that is not a record carrying `$scores` — so the divergent
  shape can no longer be written silently.
- `run_signatures()` now returns `record` (the scoring result with `resource`
  attached, exactly as the human observer builds it) and derives `n_results` from
  it, so the count and the stored object cannot disagree.
- The state probe reads `$scores` from the record, with a narrow matrix fallback so
  a future divergence would still report a true count instead of a silent `0`.
- The frozen contract needed no change: `bo$pathways$signatures <- res$scores` is
  pinned as a literal by `test-bulk-signatures-contract-freeze.R:165`, so the
  helper was written to reproduce that line byte for byte rather than renaming the
  freeze test's expectation.
- A reader-side regression test runs **both** triggers — the real human observer
  through `testServer`, and the real drive wrapper — and compares the two stored
  objects. It failed (`18 failures, 0 errors`) when the matrix bug was
  reintroduced by hand, reproducing the live error message verbatim, then passed
  on restore.

### 5.3 Observed states and timing, after the fix

Every action went through the MCP drive over native JSON-RPC; the import used the
scenario file-drop because `run` is `run_pipeline`-only by design.

| seq | action | `result.status` | job | published `bulk_signatures` state |
|---:|---|---|---|---|
| 1 | `import_file` (427 genes) | `done` | — | `not_ready` (no VST yet) |
| 2 | `run_pipeline` `bulk_filter` | `done` | — | `not_ready` (snapshot predates the run) |
| 3 | `run_pipeline` `bulk_signatures` | **`done`** | `job-00000001`, 1.6465 s, `done` | `done`, `n_results: 4`, `steps.signatures: ran`, `ready: true` |

- **The full transition was observed this time**, including `running`: the app log
  records `seq=3 ... status=running` followed by
  `job ... finished status=done elapsed=0.2s`.
- No stale count: `n_results: 4` on success, `0` on every failure across both
  validation sessions.
- The previously broken reader now renders, over CDP:
  `sig_status` shows **"✓ 4 signatures scorées x 8 échantillons [ ssgsea ]"**.
- **No output errors in the Shiny log.** The two
  `Error in $: $ operator is invalid for atomic vectors` entries for `sig_status`
  and `sig_heatmap` are gone.
- Panel navigation: `aria-expanded="true"`, collapse visible, 572 px tall, host
  `bulk-acc_bulk`, inputs namespaced `bulk-signatures-*`, `sig_resource` at
  `"hallmark"`, DOM button present and unbound.
- **Export gating is correct in both directions**: with no result the CSV and RDS
  links are disabled; after the `done` verdict both are enabled (no
  `shiny-disabled` class, `pointer-events: auto`). The href binding lags in this
  session (see 5.4).

### 5.4 The one item not positively confirmed, and why it is not this action

> **Resolved by §6.5.1.** A later session showed the plot and table DO render
> once the action selects its own results tab; the "stuck in `recalculating`"
> observation below was about hidden panes. The analysis in this subsection still
> stands as the evidence that the symptom was not the writer/reader contract, but
> the open item is now closed rather than pending.

The heatmap `<img>` and the data table did not render in this session. Evidence
that this is not attributable to the writer/reader contract:

- **The human button behaves identically.** Clicking "Lancer Scores de signatures"
  in the same session, with the same data, leaves the heatmap in the same state and
  produces the same correct `sig_status` text.
- **It is app-wide, not module-wide**: 72 of 74 `.shiny-plot-output` elements were
  stuck with the `recalculating` class, across SC, Spatial and Bulk modules that
  this work never touches, while **text outputs rendered normally** and the
  session heartbeat kept climbing (so nothing was blocking the event loop).
- Two plots did render — the Step 1 PCA and scree, i.e. the ones in the tab that
  was active at the time.

So plot delivery in this background `Rscript` session is unreliable, and no
inference about the signature slot can be drawn from it. **Confirming the heatmap
and table need one run in a normal RStudio "Run App" session**, or a re-run of this
harness once the output-suspension behaviour is understood. The criterion is
explicitly left open rather than claimed.

### 5.5 First pass (FAIL), kept for the record

| seq | action | `result.status` | job | published state |
|---:|---|---|---|---|
| 1 | `import_file` (60-gene probe) | `done` | — | `not_ready`, `has_data: true` |
| 2 | `run_pipeline` `bulk_filter` | `done` | — | (snapshot predates the run) |
| 3 | `run_pipeline` `bulk_signatures` | **`error`** | `job-00000001`, 0.9973 s, `error` | `error`, `n_results: 0`, `steps.signatures: error` |

The failure was **correct behaviour on unsuitable data** (synthetic identifiers
with no overlap yield `no_gene_sets`, reported as `error`, never as `done`) and,
separately, the structural defect: `mod_bulk_signatures.R:188` stored
`res$scores` (a matrix) where `:361` stored the record and `:316` read
`sc$scores`/`sc$method`. The 84 offline assertions had been green because they
asserted the writer's own assumption. That is the whole argument for validating
live before stacking further actions on unvalidated wiring.

Two protocol facts recorded by the first pass and confirmed again here:
`transcripto_drive_set_armed` demands a fresh heartbeat while `hb_n` only
advances *while armed*, so the first arm must go through `arm.json`; and opening
a second tab creates a NEW Shiny session that rewrites `ready.json` with a new
token and resets `last_seq` to 0.

### 5.6 Remaining, out of scope for this fix

1. Confirm the heatmap and table render in a normal RStudio session (5.4).
2. The nav plan opens the sidebar panel but does not select the `tab_signatures`
   results tab (`tab = NULL`, exactly as `bulk_de`/`bulk_pathways` do), and the
   drive path does not perform the human `nav_select("sig_tabs", …)`. Pre-existing
   pattern for every Bulk action, not a regression.
3. A verdict's embedded snapshot is captured **before** a synchronous action
   finishes, and the read-only `snapshot`/`status` tools are passive reads of the
   last verdict (they self-report `stale: true` past 15 s). Optional and
   cross-cutting: re-publish the snapshot after a long job reaches its terminal
   state. Not to be done casually — it changes every action.
## 6. Live validation: bulk_pattern

**VERDICT: PASS.** Every criterion was observed in a real visible session, with
one qualification recorded in 6.5 (the ambiguous branch of the group-column rule
is not reachable through the app's own flow, so it rests on the offline tests).

### 6.1 Session and dataset

- Real Chrome tab with the CDP pilot attached, `ready.json.viewer = "visible"`,
  heartbeat climbing, armed via `arm.json`; session identity pinned by the derived
  `session_id`, never the token.
- Dataset: **synthetic** counts, 427 genes × 8 samples, two conditions and one
  batch, so the metadata carries **two** usable grouping columns. Identifiers come
  from the app's own offline MSigDB Hallmark catalogue and the counts are
  generated, with a log2 fold-change **gradient** (≈1 → 6) across one complete
  set. No real sample, patient or probe identifier appears in this document.
- Every action below went through the MCP drive over native JSON-RPC; the import
  used the scenario file-drop because `run` is `run_pipeline`-only by design.

### 6.2 Observed sequence, states and timing

| seq | action | `result.status` | job | published `bulk_pattern` state |
|---:|---|---|---|---|
| 3 | `run_pipeline` `bulk_pattern` **before** Step 2 | **`invalid`** | none dispatched | `not_ready`, `ready: false`, `steps.pattern: skipped`, `n_results: 0` |
| 9 | `run_pipeline` `bulk_pattern` (uniform-effect data) | **`error`** | `job-00000003`, 0.25 s, `error` | `error`, `n_results: 0`, `steps.pattern: error` |
| 10 | `import_file` | `done` | — | — |
| 11 | `run_pipeline` `bulk_filter` | `done` | — | — |
| 12 | `run_pipeline` `bulk_de` | `done` | `running` → `done`, 1.1 s | 131 significant genes, active contrast `B_vs_A` |
| 13 | `run_pipeline` `bulk_pattern` | **`done`** | `job-00000005`, 0.1486 s, `done` | `done`, `n_results: 4`, `steps.pattern: ran`, `ready: true` |

- **`running` was observed on the wire** (the app log records
  `seq=13 ... status=running` then `finished status=done elapsed=0.1s`).
- **No stale count anywhere**: `n_results: 0` on the readiness refusal and on the
  domain error, `4` on success. `n_results` counts the clusters **actually
  produced** from the stored `clusters` column, not the frozen `k`.
- **No errors in the Shiny log for the successful run.** The only error lines
  belong to the seq 9 attempt and are the domain's own message.

### 6.3 The group-column rule, live

- **No contrast yet** (seq 3): the run was refused before any job was created —
  `status = invalid`, reason *"Step 2 has not produced an active contrast"* — and
  the published state was `not_ready` / `steps.pattern: skipped` / `n_results: 0`.
  This is the refusal working: a doomed job was never dispatched.
- **After Step 2** (seq 13): readiness was `true` and the drive used the column
  the DE step recorded, confirmed in the DOM — the panel's group select reads
  `condition`, and the status line reports *"✓ 131 gènes regroupés en 4 clusters
  (2 groupes, graine 15)"*.
- **Ambiguous** (a contrast with no recorded column and several candidates):
  **not reachable through the app's own flow**, because both DE paths write
  `shared_rv$active_condition_col` (`mod_bulk_de_engine.R:198`,
  `mod_bulk.R:430,454`). The branch is a safety net, and it is covered offline —
  all six outcomes of `.bulk_pattern_group_column()` are asserted in
  `test-mod-bulk-pattern-drive.R`, including that the reason names only the
  **count** of candidates and never a column.

### 6.4 The result, the readers, and the navigation

- Stored through the single writer `.bulk_pattern_store()`, shared with the human
  observer: the whole record in `shared_rv$pattern_result`, and
  `active_tab = "tab_pattern"`.
- All three readers were exercised over CDP and all three work:
  - `pattern_plot` → a real image, **1335 × 840**;
  - `pattern_table` → **15 rows, "Showing 1 to 15 of 131 entries"**, columns
    `gene` / `cluster` — the expected data frame of cluster assignments;
  - `dl_pattern` → enabled (`shiny-disabled` absent) and bound to a real href.
- **Navigation**: the nav plan opened `panel_pattern`
  (`aria-expanded="true"`, collapse visible, 528 px, host `bulk-acc_bulk`), and
  because the writer selects the results tab, the Bulk tabset moved to
  "Clustering de profils" on its own.

### 6.5 Findings and discrepancies against the offline tests

1. ✅ **This also corrects §5.4.** Plot outputs *do* render when the action selects
   its own results tab — the pattern writer sets `active_tab`, and both the plot
   and the table rendered. The "72 of 74 stuck in `recalculating`" observation from
   the signature session was about **hidden panes**, not a broken renderer.
2. ⚠️ **The DE's four metadata-driven selects are NOT MCP-settable, by design.**
   `set_inputs` on `bulk_de` for `condition_col` / `group_ref` / `group_target` /
   `de_engine` was refused with `INPUT_NOT_ALLOWED` for all seven values, matching
   the comment in `scripts/mcp_server.R:1013`. The DE still ran because the app
   derives those selects from the loaded metadata. Consequence for an agent: it
   can trigger Step 2 but **cannot choose the design remotely** — and any action
   that depends on the DE having recorded a group column inherits that default.
3. ℹ️ **A domain error is logged even when the protocol verdict is right.** The
   seq 9 attempt (a dataset where every selected gene had the same effect, so
   kmeans had fewer than 4 distinct points) surfaced as
   `[drive] tick error: le nombre de centres de classes est supérieur au nombre de
   points distincts` with `n_results: 0`. That refusal is correct behaviour, and
   it is the `< 10` gene floor and the domain guard doing their job.
4. ℹ️ **Two data-generation bugs of mine, not app defects**, recorded because they
   shaped the run: `unique()` on a *list* does not flatten, so my first generator
   added no signal at all and the DE correctly found 0 significant genes; and a
   second dataset with a *uniform* fold change made every selected gene collapse
   onto one z-profile. Both were fixed in the generator, not in the app, and in
   both cases the app reported the truth instead of inventing a result.
5. ℹ️ Re-confirmed: a verdict's embedded snapshot is captured before a synchronous
   action finishes (§5.6), so the state seen at a given `seq` is the pre-run one.

### 6.6 Ready for commit

`bulk-pattern-run_pattern` is **validated and ready for commit**. No code change
was needed to pass: nothing in this section produced a defect in the action.

## 7. Invariants

- The live `tools/_drive/` directory is not a manifest or test fixture.
- No MCP tool was added: the inventory remains at seven.
- No statistical method, default scientific parameter, or plot was changed.
- A readiness or state probe failure is not converted into a successful run.
- The single-SC action reports step states from observable outcomes, not from a
  request that was merely accepted.
- A step that reports failure publishes **no** result count: both SC wrappers
  force `n_results = 0L` when the action did not return `ok = TRUE`, so a stale
  table can never be read next to a failure as if it were the outcome of that
  run. The rule is asserted in both directions and was falsified by removing
  the guard.
- **Live-validated actions.** `bulk-signatures-run_signatures` (§5, PASS after
  the writer fix) and `bulk-pattern-run_pattern` (§6, PASS) have each been
  exercised in a real visible session through the MCP drive, with the job
  lifecycle, the readiness refusals, the stored shape read back by the module's
  own readers, and the absence of stale counts all observed on the wire. Both are
  ready for commit; the drive remains at 10 modules / 11 buttons and 7 tools.
- **Phase E adds three more, and the three together cover the whole contract**:
  - `bulk-network-run_network` — live (§8): `running` → `error` with the domain's
    own cap message and `n_results: 0`, plus a **real** `reactome.db` run offline
    proving the `done` path and that `n_results` is identical to `qc$n_nodes`.
  - `sc-pathways-run_pathway` — live (§9): `running` → `done` in 21.4 s, 414
    pathways, and the module's own plot reader rendered a 1153 × 750 image from
    the stored record.
  - `spatial-qc-btn_hotspots` — live only as far as the modality allows (§10): a
    well-formed `not_ready` / `n_results: 0` record in a session with no Spatial
    data, plus a **real** Getis-Ord execution offline (RANN, not a mock).
- **Every Phase E action is `long = TRUE`, and every one showed `running` on the
  wire** when it ran. That is the property worth stating as an invariant: `running`
  is not a decoration on a synchronous job, it is the only evidence the agent gets
  that a long step is in flight rather than already lost.
- **A frozen input set may not contain a session-derived value**, and the three
  new actions show the three admissible ways to deal with one that cannot be
  frozen: no parameter at all (`bulk_network`), a declared prerequisite
  (`sc_pathways`), or a rule (`spatial_qc`). What is NOT admissible is a silent
  fallback to whatever the session happens to hold.
- **Phase E did not widen the tool surface**: still seven tools, and
  `set_inputs` was extended to refuse `bulk_network`, `sc_pathways` and
  `spatial_qc` for the same reason the other frozen-input modules refuse it.


## 8. End-to-end validation: Bulk

> **§8.6 is the Phase F re-run, and it supersedes the verdict below for the three
> stages it could reach.** The §8.1–§8.5 record below is kept as it was measured.
> Read §8.6 for the current state, including the first **EXPORT** verification of
> this chain.

**VERDICT (Phase E, superseded): PASS, with two data-bound refusals that are
themselves correct behaviour** (one on each of the two Phase C/D actions) and one
limitation found (§8.5.2).

Session: a real visible Chrome tab with the CDP pilot attached,
`ready.json.viewer = "visible"`, heartbeat climbing (`hb_n` 0 → 125), armed through
`arm.json`, session pinned throughout by the **derived** `session_id`. Datasets:
the MCP test corpus, `bulk/` — two public GEO-derived sets, no new identifiers
invented and no patient, sample or probe identifier recorded in this document.

### 8.1 Sequence and observed states

| seq | action | `result.status` | job | published state |
|---:|---|---|---|---|
| 1 | `import_file` (`import_bulk`, file-drop) | `done` | — | `has_data: true`, 500 genes × 6 samples |
| 2 | `bulk-filter-run_filter_norm` | `done` | — | `filtered_counts` and `vst_mat` = 500 × 6 |
| 3 | `bulk-de-run_de` (first dataset) | **`invalid`** | `job-00000003`, 0.1 s | `bulk_de` untouched; the design selects had no usable column |
| 6 | `import_file` (second dataset) | `done` | — | `has_data: true`, 500 genes × 8 samples |
| 7 | `bulk-filter-run_filter_norm` | `done` | — | `filtered_counts` / `vst_mat` present |
| 8 | `bulk-de-run_de` | `done` | **`running` → `done`, 2.0 s** | 1 contrast, `active_contrast` recorded |
| 9 | `bulk-signatures-run_signatures` | **`error`** | `running` → `error`, 1.2 s | `n_results: 0`, `steps.signatures: error` |
| 10 | `bulk-pattern-run_pattern` | **`done`** | **`running` → `done`, 0.4 s** | `n_results` observed, `steps.pattern: ran` |
| 11 | `bulk-network-run_network` | **`error`** | `running` → `error`, 14.3 s | `n_results: 0`, `steps.network: error` |

Every action went through the MCP drive over native JSON-RPC, except the two
imports: `transcripto_drive_run` is `run_pipeline`-only **by declared design**
(`TS_MCP_RUN_ACTIONS <- c("run_pipeline")`), so `import_file` necessarily goes
through the scenario file-drop. That is the same route §5 and §6 used.

**`running` was observed on the wire for all three late actions** — the app log
records `status=running` followed by `job seq=N … finished status=…` in each case,
which is the only way to prove the `long = TRUE` declaration earns its keep.

### 8.2 Criteria

- **Job transitions** ✅ accepted → running → terminal, for all four analysis
  actions, with the terminal state matching what was published.
- **No stale `n_results`** ✅ `0` on both refusals, and the state probe re-counted
  from the stored record rather than remembering. After a deliberate page reload
  that wiped the session's data, **every** module including the three new ones
  reported `not_ready` with `n_results: 0` — a stale count cannot survive a
  re-import.
- **Panels open, readers render** ✅ the drive's own nav plan is what opened them,
  and the resulting accordion input values were read back from the live session:
  `bulk-acc_bulk = ["panel_filter","panel_de","panel_signatures","panel_pattern",
  "panel_network"]` — `panel_network` and `panel_pattern` are the two new ones.
- **Expected slots** ✅ `shared_rv$pattern_result` / `$network_result` were written
  through the shared writers, which is what the offline reader-side tests assert.
- **No Shiny/MCP errors on the successful runs** ✅ the only error lines in the app
  log belong to the two refused actions, and each is the domain's own message.

### 8.3 The two refusals, and why they are correct

**`bulk-signatures-run_signatures` → `error`.** The corpus' airway set carries
**Entrez** gene identifiers, so no Hallmark set reaches the `[10, 500]` size band
with 20 % overlap. The surfaced message is the domain's own
(`compute_pathway_scores(): aucun jeu de gènes ne survit aux filtres`) and the
action reported `n_results: 0`. This is the readiness contract working: a wrong
identifier space is refused, not papered over. An agent should read it as "this
action needs SYMBOLS", which is exactly what the message says.

**`bulk-network-run_network` → `error`.** The DE produced **231 significant genes**,
and the network's declared cap is `TS_BULK_NETWORK_MAX_NODES = 200`. The domain
refused with its own message, and the action reported `n_results: 0`. Correct
behaviour, and the cap is documented as protective rather than decorative.

The `done` path of that action was therefore verified **offline, against the real
`reactome.db`**, on the same corpus counts with a 120-prime stand-in contrast:

```
ok=TRUE  n_results=91   record status=valid_with_warnings
qc: n_terminals=54  n_relay=37  n_nodes=91  n_edges=71
n_results identical to qc$n_nodes : TRUE
map_rate 0.954   species hsapiens
build_bulk_network_table_export(): 91 rows, columns
  node,symbol,role,prize,degree,species,source_db
```

That is the load-bearing property: **`n_results` cannot disagree with the stored
record**, because it is read out of `qc$n_nodes` rather than computed twice.

### 8.4 Discrepancies against the offline tests

1. ✅ **None in the wiring.** The frozen inputs, the readiness reasons, the shared
   writers and the count semantics all behaved exactly as the offline tests assert.
2. ℹ️ **`n_results` for the network is unreachable on a "normal" DE result.** The
   action freezes `padj < 0.05` and `|log2FC| > 1`, so its prime set is *exactly*
   the significant set, and 231 > 200 refuses. The action deliberately does NOT
   read the session's thresholds (that is the point of freezing them), so **an agent
   has no MCP-settable way to narrow the gene set**. Recorded as a limitation, not
   a defect: the alternative — inheriting the session thresholds — would make a
   driven run's result depend on state the protocol refuses to set.
3. ℹ️ **The first dataset could not produce a DE at all.** `gse164073_eye` offers the
   DE a single usable grouping column (`tissue`, one level), so the design
   `validate()` refuses and Step 2 answers `invalid`. The app is right; the dataset
   simply has no contrast. This is a property of the corpus, not of the drive.

### 8.5 Two protocol findings

**8.5.1 — The DE design is NOT MCP-settable, re-measured with the exact text.**
`transcripto_drive_set_inputs` on `bulk_de` for `condition_col`, `group_ref`,
`group_target`, `de_engine`, `lfc_thresh` and `padj_thresh` was refused with
`INPUT_NOT_ALLOWED: 6 input(s) refused; NOTHING was written`, every one of them
`"not on the app's allowlist"` — even though all six ARE in
`TS_DRIVE_ALLOWLIST`. The MCP grade simply does not expose them (29 inputs exposed,
5 allowlisted but not exposed). This confirms §6.5 finding 2 in a stronger form than
before: it is not that the injector refuses them, it is that the **MCP tool surface**
omits them. An agent can trigger Step 2 but cannot choose the design, and any action
that needs a group column inherits whatever the app derives from the metadata.

**8.5.2 — A fresh session cannot be armed through `transcripto_drive_set_armed`.**
The heartbeat only advances **while armed** (`if (cursor$armed) cursor$hb_n <-
cursor$hb_n + 1L`), and `ts_drive_ready_fresh()` requires an age ≤ 15 s — so the
first call to the arm tool is refused with `STALE_SESSION` on a session that has
never been armed, and the session can only start beating once something else arms
it. The bootstrap that works is the **ARM/DISARM transition**: writing `arm.json`
with `armed: true` and the fresh token causes the app's very next tick to see a
transition and rewrite the handshake. Both sessions in §8/§9 were armed that way.
This is a real ordering defect in the tool surface, not a usage error, and it is why
§11 lists it as something to fix before a push.

### 8.6 Phase F re-run: the Bulk chain, through export

**VERDICT: PARTIAL PASS.** Import, Step 1 and signatures are drivable and their
**exports are verified on disk**. Step 2 (DE) is still refused for the reason
recorded in §8.5.1, so `bulk_pattern` and `bulk_network` never ran and their
exports have no content — the same data-bound refusal Phase E recorded, reached
by the same cause, and **not** a regression from anything Phase F changed.

Dataset: `bulk/gse164073_eye` — a public GEO-derived set, 500 genes × 6 samples in
two groups of three. No biological identifier is recorded here. Session: a real
Chrome tab driven over CDP, `ready.json.viewer = "headless"`, armed by writing
`arm.json`, heartbeat proven live before the first scenario.

| seq | action | `result.status` | wall clock | published state |
|---:|---|---|---:|---|
| 1 | `import_file` (`import_bulk`) | `done` | **1.2 s** | `has_data: true`, 500 genes × 6 samples |
| 3 | `bulk-filter-run_filter_norm` | `done` | **1.2 s** | `filtered_counts` and `vst_mat` = 500 × 6, all six sample names echoed |
| 5 | `bulk-de-run_de` | **`invalid`** | 1.2 s | `n_contrasts: 0` — see below |
| 7 | `bulk-signatures-run_signatures` | `done` | **2.4 s** | `n_results: 2`, `steps.signatures: ran` |
| 9 | `bulk-pattern-run_pattern` | **`invalid`** | 1.2 s | `not_ready` — "Step 2 has not produced an active contrast" |
| 11 | `bulk-network-run_network` | **`invalid`** | 1.2 s | `not_ready` — same reason, correctly refused rather than run on nothing |

**Export verification.** Each stage was activated through the app's OWN
navigation — a no-op `set_inputs` scenario carrying `expect.nav`, i.e. the same
nav table the drive uses — then its download links were clicked and the files read
off disk.

| Stage | Rendered plots | Download link | File on disk | Content check |
|---|---:|---|---|---|
| PCA / QC | 2 | `bulk-filter-dl_pca_png` | `pca_bulk_2026-09-26.png`, **95 664 B** | PNG magic `89 50 4e 47 0d 0a 1a 0a` |
| PCA / QC | — | `bulk-filter-dl_scree_png` | `scree_plot_bulk_2026-09-26.png`, **89 511 B** | PNG magic as above |
| signatures | 1 | `bulk-signatures-dl_sig_csv` | `signature_scores_ssgsea_2026-09-26.csv`, **2 643 B** | **12 data rows**, header `signature,sample,score,method,analysis_id,disclaimer` |
| signatures | — | `bulk-signatures-dl_sig_rds` | `signature_scores_ssgsea_2026-09-26.rds`, **2 737 B** | RDS |
| Volcano / DE | 0 | `bulk-de-dl_volcano_png` | **none** | link ENABLED, click produced no file — no DE ran |
| pattern | 0 | `bulk-pattern-dl_pattern` | **none** | link ENABLED, no file — no pattern ran |
| network | 0 | `bulk-network-dl_network` | **none** | link ENABLED, no file — no network ran |

**Export is therefore reachable through the actions the chain already had, and no
new export action was needed for Bulk.** Every download handler the drivable
stages own is reachable, produces a file, and the CSV carries the expected
columns. The three empty ones are empty because their upstream analysis was
refused — a download handler over a NULL result is the one thing that cannot
succeed, and Shiny leaving the link *enabled* while the server then fails is the
app behaving normally rather than a broken export.

#### 8.6.1 Why the DE design still could not be set, in the app's own terms

`bulk-de-condition_col` offers exactly one choice for this dataset (`tissue`), and
setting it is **server-confirmed** — `Shiny.$inputValues` reads
`condition_col = "tissue"` — but `bulk-de-group_ref` and `bulk-de-group_target`
then still offer only `[""]`. The group choices are populated by server-side
reactives that did not run under a client-side `setInputValue`, so no contrast can
be formed and the DE refuses with `n_contrasts: 0`.

This is **§8.5.1 unchanged**, and it is a *capability* boundary rather than a
defect: the four metadata-driven DE inputs are deliberately absent from the MCP
`set_inputs` schema because their value domain comes from the live session's
metadata and the MCP server cannot validate it. The honest statement is that
Bulk's Step 2 → pattern → network tail **requires a human to choose the design**,
and that Phase F did not change it.

## 9. End-to-end validation: SC

> **§9.5 records the Phase F position: the SC chain was NOT re-run, and the reason
> is structural, not an omission of effort.** Read it before relying on §9.1–§9.4,
> which remain the last live measurement of this chain.

**VERDICT: PASS.** All four actions, including the new `sc-pathways-run_pathway`,
were exercised in a real visible session through the MCP drive, and the new
action's plot reader rendered a real image.

### 9.1 Dataset and how it got in

`sc/pbmc_2cond_1000cells` (10x v3 triplet, 1000 cells, two conditions). The drive
has **no `import_file` importer for `import_sc`** — only `import_bulk` publishes
one — so the object was loaded through the app's own **Option B** file input, driven
over CDP: the 10x triplet was converted to a Seurat `.rds` **from the corpus
counts** (10 210 genes × 989 cells after `min.cells = 3` / `min.features = 200`;
gene symbols taken from column 2 of `features.tsv`), and that file was set on
`import_sc-file_upload` and loaded with `import_sc-btn_load_file`. The app then
reported `🟡 Mono`, 989 cells, 10 210 genes.

Two obstacles are worth recording because they cost real time and will cost it
again:

- The import UI lives in a **dropdown** of the main navbar, not in a link. Until
  "📥 Import Données" is opened and "Single-Cell" chosen, the file input is inside
  a `display: none` pane and `DOM.setFileInputFiles` on it is accepted by Chrome
  while Shiny never sees a byte.
- Chrome's `DOM.setFileInputFiles` does **not** make Shiny's custom file binding
  re-read the `FileList`. The `change` event has to be dispatched explicitly.
  Without it the module stays at "⚪ Inactif" with no error anywhere.

### 9.2 Sequence and observed states

| seq | action | `result.status` | job | published state |
|---:|---|---|---|---|
| 12 | `sc-pipeline-run_auto_pipeline` | `done` | **`running` → `done`, 167.9 s** | `n_results: 6` clusters; `qc/norm/pca/clusters/umap/tsne/markers/trajectory` = `ran`, `mapping/singler/pathway/correlation` = `skipped` |
| 13 | `sc-annotation-run_annot` | `done` | **`running` → `done`, 121.4 s** | `n_results: 15` labels |
| 14 | `sc-markers-run_markers` | `done` | **`running` → `done`, 0.8 s** | `n_results: 4163` markers |
| 15 | **`sc-pathways-run_pathway`** (new) | **`done`** | **`running` → `done`, 21.4 s** | `n_results: 414` pathways, `steps.pathway: ran`, `ready: true` |

All four are `long = TRUE` and all four showed `running` on the wire. The final
`snapshot` carried `sc_pathways.status = done`, `n_results = 414`,
`ready = true`, `steps.pathway = ran` — read back from the file the app wrote, not
from the tool's acknowledgement.

### 9.3 Criteria

- **Job transitions** ✅ for all four.
- **No stale `n_results`** ✅ the probe counts from the stored object; a `status`
  of `not_ready` always carried `n_results: 0`.
- **Panels open, readers render** ✅ the nav plan opened `6_pathway`, confirmed by
  reading the live accordion input: `sc-acc_analyse = ["2_annotation","4_markers",
  "6_pathway"]`. 🔑 **That read also validated a constant**: `TS_DRIVE_SC_PATHWAYS_PANELS
  <- "6_pathway"` is the real bslib value (singular, and nested inside the
  "Analyse" group) — a value that could not have been guessed and that the DOM
  does not expose as an attribute.
  The new action's own plot reader rendered a **real image, 1153 × 750**
  (`#sc-pathways-pathway_barplot` → `<img>`), which is the strongest reader evidence
  available: the renderer read the record that the shared writer stored.
- **Expected slots** ✅ `pathway_rv` (module-local) and `shared_rv$pathway_results`
  / `$pathway_db` moved together, which is what the offline reader-side test
  asserts by running both triggers.
- **No Shiny/MCP errors** ✅.

### 9.4 Findings and discrepancies

1. ⚠️ **`shared_rv$pathway_results` is SHARED with the Bulk pathway module.**
   `modules/bulk/mod_bulk_pathways.R:645,702` and `modules/sc/mod_sc_pathways.R:129`
   write the SAME slot, and both set `active_tab <- "tab_pathway"`. This is
   PRE-EXISTING app behaviour (both human observers did it before Phase E) and the
   SC writer preserves the SC human behaviour exactly. But it is a real hazard for
   an agent: running the Bulk enrichment and then the SC enrichment in one session
   leaves only the last one in the shared slot, and the Bulk panel reads that same
   slot. It is listed in §11 as a limitation, and fixing it means changing an app
   slot, which is out of Phase E's scope.
2. ℹ️ **The SC auto-pipeline does NOT run the pathway step.** Its own step record
   shows `pathway: skipped` and `singler: skipped`, which is why the separate
   `sc-annotation-run_annot` and `sc-pathways-run_pathway` actions are worth
   having: they cover steps the one-click pipeline leaves out.
3. ℹ️ **`sc_pathways` is the first action whose PREREQUISITE is another MCP
   action.** Readiness is `sc_obj` + `markers_data`, and its refusal names the
   marker step by name. Measured: before seq 12 every module read `not_ready` with
   `n_results: 0`; after the pipeline, `sc_pathways.ready` was `true` while
   `sc_annotation` was still `idle` — i.e. readiness is per-action, not global.
4. ℹ️ **One unintended side effect, disclosed.** A DOM probe meant to open an
   accordion matched on a broad text pattern and clicked the real
   `sc-pseudobulk-run_aggregate` button, which ran a pseudobulk aggregation in the
   live session. It does not affect any verdict above (the SC states quoted are
   from `result.json`, written by the app), but the run was not purely
   non-interactive and saying so is the point of recording it.

### 9.5 Phase F: the SC chain was not re-run, and cannot be bootstrapped by MCP at all

**This section is a negative result, and it is a measured one rather than a
budget decision.**

**Why the chain cannot start.** `import_file` routes to a module's **published
importer**, and after Phase F exactly two modules publish one:

```
TS_DRIVE_IMPORT_MODULES  ->  "import_bulk"  "import_spatial"
```

There is no `import_sc` importer, so there is no way to get a Seurat object into
an SC session through the protocol. §9.1 achieved it by driving the import
through the **UI** — file-drop into a `fileInput`, which is precisely the thing
spec S5 forbids the drive from faking. The chain therefore starts, on a fresh
session, only with a human at the keyboard. That is a real gap, it is recorded as
such in §11.2, and it is **the same class of gap Phase F just closed for
Spatial** — which is the strongest available evidence that the gap is a missing
importer and not an inherent limit.

**What was not done, therefore.** No SC action was dispatched in Phase F. Nothing
in §9.1–§9.4 is invalidated — the last live measurement still stands — but no SC
export was verified on disk in this phase, and this document does not claim one.

**What Phase F did change on the SC side: nothing.** No file under `modules/sc/`,
`R/sc/` or `R/core/` (SC paths) was touched. The drive surface gained
`import_spatial`, which is additive; `test-mcp-sc-local.R` and
`test-mod-sc-*.R` were re-run and are green. The pre-existing
`shared_rv$pathway_results` collision between the Bulk and SC pathway modules
(§9.4.1) is **untouched and still open** — it is a product defect, not a protocol
one, and closing it would change what the Bulk panel reads.

**Recommendation.** An `import_sc` importer is the single highest-value remaining
item for SC, and it is now a *known* amount of work: the Spatial importer is the
template, and the SC module's load path differs only in that its dataset is a
`.rds`/`.h5ad` file rather than a folder. It is not in Phase F's frozen scope and
was not started.

## 10. End-to-end validation: Spatial

> **§10.4 supersedes §10.1's diagnosis.** The zero-height picker was MEASURED to
> be a measurement artefact, and the Spatial chain is no longer BLOCKED: Phase F
> added the `import_spatial` importer and ran import → pipeline → hotspots to
> `done` on a live session. What remains open is a different, and pre-existing,
> presentation defect, recorded in §10.4.4.

**VERDICT: BLOCKED at import — the Spatial chain could not be run, and the reason
is measured rather than inferred.** The new `spatial-qc-btn_hotspots` action is
nonetheless live-validated on the half that does not need a dataset.

### 10.1 What blocked it, precisely

`import_spatial` offers exactly one dataset input: a
`shinyFiles::shinyDirButton` (`modules/import/mod_import_spatial.R:317`), i.e. a
**BUTTON**. Chrome's `DOM.setFileInputFiles` answers
`{"code":-32000,"message":"Node is not a file input element"}`, and the hidden
`webkitdirectory` input only exists after a click that opens a native dialog.
CDP's own answer to that is `Page.setInterceptFileChooserDialog`; the interception
was implemented and armed, and **no `Page.fileChooserOpened` event ever arrived**,
because the button never produced a file chooser: it went `disabled` instead.

The underlying cause is visible in the DOM and is the real finding:

```
#import_spatial-dir_select_ui  ->  width 367, height 0, innerHTML length 0
```

The container that should hold the picker has **zero height**, so Shiny suspends
the `uiOutput`, so the `shinyDirButton` is never rendered, so there is nothing to
click. Enlarging the viewport to 2200 × 2400 did not change the measurement
(`h: 0` before and after). `shinyFiles` **is** installed on this host, so this is
a layout/suspension problem in the session, not a missing package — but either way
the Spatial import is unreachable from an automated pilot, and the drive offers no
alternative because it has no `import_file` importer for `import_spatial`.

### 10.2 What WAS live-validated

- **The action is wired and publishes a well-formed record.** From the very first
  `import_file` of the session — before any dataset existed — the app published:

  ```json
  "spatial_qc": { "module": "spatial_qc", "action": "run_pipeline",
                  "status": "not_ready", "n_results": 0, "has_data": false,
                  "ready": false, "steps": { "hotspots": "skipped" } }
  ```

  Same after the deliberate reload: `not_ready`, `n_results: 0`, no stale count.
  The new action is the only Spatial action besides the pipeline, and it refuses
  cleanly instead of dispatching a doomed job.
- **Its `done` path is covered by a REAL domain execution offline**, not only by a
  mock: `test-mod-spatial-qc-drive.R` calls the unmocked
  `compute_getis_ord_hotspots()` (RANN installed) on 40 spots with a genuinely
  elevated block, and requires `n_results > 0`, 40 scored elements, and the
  domain's own three-value classification. It also asserts the constant-variance
  guard is the DOMAIN's refusal rather than a thin success.
- **The Spatial pipeline action** was not re-run in this session for the same
  reason; it remains validated by §5/§6-era runs and by
  `test-mod-spatial-pipeline.R`.

### 10.3 What a Spatial chain would need

1. A fix to the zero-height container (or a `renderUI` that does not depend on a
   zero-size slot) so the folder picker exists; **or**
2. an `import_file` importer for `import_spatial` on the drive, which is the
   change that would actually make the modality drivable end to end.

Neither is in Phase E's frozen scope. Both are in §11.

### 10.4 Phase F: the UI "blocker" was a measurement artefact, and the chain now runs

**VERDICT: the MCP chain is PASS (import → pipeline → hotspots, all `done` with
real numbers). The hotspot PANEL does not render, and that defect is
pre-existing, reproduces on the human click path, and is not an MCP regression.**

#### 10.4.1 The zero-height container is a `display: contents` artefact, and the button was never missing

§10.1 recorded, as the cause of the Spatial block:

```
#import_spatial-dir_select_ui  ->  width 367, height 0, innerHTML length 0
```

and inferred a three-step chain: zero height ⇒ Shiny suspends the `uiOutput` ⇒
the `shinyDirButton` is never rendered. **MEASURED on a live session, every link
of that chain is false.**

| # | §10.1's claim | Measurement |
|---|---|---|
| 1 | the container has height 0 | `getComputedStyle(el).display === **"contents"**` — a box-less wrapper. `getBoundingClientRect()` on it therefore returns 0 × 0 **by construction**, whatever it contains. Its own width read 367 and 352 on two different runs while its height read 0 both times: a box that genuinely had no content would have had no width either. |
| 2 | the `uiOutput` is suspended | `offsetParent !== null` once its tab is active, and `innerHTML` fills to 326 bytes. `display: contents` is Shiny's own CSS, not a layout fault. |
| 3 | the button is never rendered | `#import_spatial-dir_select` exists, `disabled: false`, **352 × 37 px at y = 641 in a 1200 px viewport**, `visibility: visible`, `opacity: 1`, and it renders its own `📁 Choisir le dossier` label. |

Two further measurements close the loop:

* The button is genuinely **clickable and enabled**. Clicking it puts it into
  `disabled: true` — which is `shinyFiles` waiting for a directory chooser that
  will never be answered — while CDP's `Page.setInterceptFileChooserDialog`
  reports **no `Page.fileChooserOpened` event**. §10.1 read that `disabled` as the
  *cause*; it is the *effect* of a native chooser an automated client cannot
  answer. The button was never broken.
* The earlier `innerHTML length 0` was a **timing** artefact. Measured 4 s after
  selecting the tab: 0 bytes. Measured 6 s after: 326 bytes. A conclusion drawn
  from the first reading describes a panel that had not finished loading.

**Consequence for the report:** §10.1's causal chain is withdrawn. There is no
UI defect to fix in `mod_import_spatial.R`, and no CSS rule, explicit height or
layout change was applied — inventing one would have "fixed" a container that was
never the problem. The real blocker is stated in §10.1 in one clause and was
already correct there: *the only way in is a native directory chooser an
automated client cannot answer.*

#### 10.4.2 What was changed instead: the `import_spatial` importer

`import_file` bypasses widgets by design (spec G3/S5) and hands the module a
validated PATH. Bulk already had an importer; **Spatial did not**, and §10.3
already named adding one as the change that "would actually make the modality
drivable end to end". That is what Phase F built.

* `TS_DRIVE_IMPORT_SCHEMA` replaces the single flat `TS_DRIVE_IMPORT_KEYS`. Two
  importers have different payloads — a counts FILE versus a data FOLDER — and one
  global key vector would have handed a directory to `smart_read()`.
* `ts_drive_validate_import_dir()` mirrors `ts_drive_validate_import_path()` and
  shares its security-relevant half, `.ts_drive_resolve_import_path()`, so the
  `..` refusal and the roots confinement are written once for both.
* `mod_import_spatial.R`'s 180-line import body became
  `run_spatial_import(path, sample_name, technology)` — ONE writer, two callers:
  the button and the importer. The repo's own rule (extend, never duplicate)
  applied to a 180-line BPCells conversion.
* The bound button `import_spatial-btn_import` is a **real** `actionButton`, so it
  needs no counter, and a session a human populated through the native picker
  becomes drivable afterwards.

#### 10.4.3 🔴 A SECOND, independent `counts_path` assumption, found only on the wire

The first live attempt failed with:

```
`import_file` needs an `import` block carrying `counts_path`
```

for a Spatial import that carried a `dir_path`. The key schema was already
per-module, so this was a **second copy of the same rule**, in the injector,
naming the payload key as a literal. A grep for the obvious spelling finds the
schema and misses this. The injector now reads the required key from
`TS_DRIVE_IMPORT_SCHEMA[[module]]$required`, and `test-drive-watcher.R` pins both
halves — the schema-driven read, and the absence of any `req$counts_path` literal
in the watcher.

The lesson is the §1 one, third instance: **two copies of a rule are worse than
none, because each looks correct in isolation.** A static check could not have
found this; it needed a live attempt with a payload the first copy had never seen.

#### 10.4.4 The chain on a live session, and the one thing that does not render

Dataset: `spatial/visium_mouse_brain` — a vendor Visium V1 `outs/` tree kept
intact (2702 spots). No biological identifier is recorded here.

| seq | action | `result.status` | wall clock | published state |
|---:|---|---|---:|---|
| 1 | `import_file` (`import_spatial`, `technology: "visium"`) | **`done`** | **16.9 s** | import value boxes: `2,702` on disk / `2,702` in RAM / `32,285` genes / `🟢 visium (LogNorm)` |
| 3 | `spatial-pipeline-btn_run_all` | **`done`** | **36.1 s** | `n_results: 9`, `has_data: true`, `ready: true` |
| 5 | `spatial-qc-btn_hotspots` | **`done`** | **1.5 s** | `n_results: 1200`, `steps.hotspots: "ran"` |

The import's own console log confirms a real conversion, not a stub: four
histology images detected, a BPCells directory created under the app cache, and
`✓ Objet brut charge : 32285 genes x 2702 spots`. The app's own `ns_results`
numbers are the ones a human reads in the value boxes, and they agree.

**The hotspot PANEL does not render, and it is not the drive's doing.**

| Evidence | Measurement |
|---|---|
| the result SLOT is correct | the sibling output `hotspot_status_ui`, which reads the **same** `shared_rv$hotspot_result`, renders: `585 hotspot(s), 615 coldspot(s) sur 2702 elements (p < 0.05)` |
| the outputs are REGISTERED | `hotspot_table`, `hotspot_map`, `hotspot_hist` all carry `shiny-bound-output`; the server body did not abort |
| the app is IDLE, not busy | `hb_n` advanced by exactly 10 every 10 s for 120 s while the outputs stayed pending — a free event loop, so not a slow render |
| the outputs are ABORTING | they sit in Shiny's `recalculating` class indefinitely, which is the signature of an expression that never returns a value (a `req()` that never satisfies) rather than one that is computing |
| **it is not the drive** | clicking the **DOM** button `spatial-qc-btn_hotspots` — the human path, which touches none of the drive code — produces the **identical** empty panel |
| the new export button is present and correctly disabled | `spatial-qc-dl_hotspot_csv` renders at 275 × 30 with its French label, `bound: true`, and class `shiny-download-link disabled` — Shiny disabling a download link until its table has content, i.e. the export refusing to ship an empty file, which is the behaviour to want |

**So the hotspot CSV export could not be exercised live, and this document does
not claim it was.** It is wired, bound, translated into the i18n catalogue,
falsified, and its refusal is exactly Shiny's. Exercising it needs the table to
render, which is a pre-existing defect in `mod_spatial_qc.R`'s hotspot pane.

**Not fixed here, deliberately.** Two candidate causes are visible in the source
and neither was changed, because a guess is worse than a recorded open item:
`output$hotspot_map` reads an `eventReactive` keyed on `input$btn_hotspots`
(line 892) while `hotspot_table` and `hotspot_hist` read `shared_rv`
(lines 941, 915) — so the two groups of outputs are invalidated by different
things, and only the `shared_rv` group should have re-rendered. Isolating which
of the two is at fault is a small, well-scoped piece of work on a module Phase F
already touches, and it is the obvious next step.

#### 10.4.5 What the Spatial import and the Spatial pipeline now prove

* The Spatial modality is **bootstrappable through the protocol**. It was the one
  blocking gap of §11, and it is closed.
* Its nav plan is a **deliberate empty** one. `nav_panel(i18n$t("Spatial"), …)`
  passes no `value=`, so bslib derives the navbar value from the i18n title TAG;
  read off a live session it is
  `"<span class=\"i18n\" data-key=\"Spatial\">Spatial</span>"` — a 47-character
  HTML fragment, not an id. Hard-coding it would buy a navigation effect whose
  failure mode is a silent no-op, so the plan is empty and the import announces
  itself with a `showNotification` instead. `ts_drive_perform_nav()` answers an
  empty plan explicitly rather than relying on a swallowed error.
* The **drive's** Spatial navigation is correct where it exists: the outer navset
  read `results_pipeline` and the QC action moved it to `results_qc`, both the
  values the frozen constants declare.

## 11. Ready for push assessment

### 11.1 Is MCP functionally complete for a first release?

> **§11.1 is rewritten for Phase F.** The previous answer was "yes for two of
> three modalities, no for Spatial"; Phase F closed the Spatial half. The SC half
> is unchanged, and §9.5 says precisely why it cannot be closed this phase.

**Two of three modalities are drivable end to end, from a fresh session, through
import → analyses → export. The third — SC — is drivable from an already-loaded
dataset and cannot be bootstrapped by the protocol at all.** Concretely:

| Modality | import | analyses | export | Verdict |
|---|---|---|---|---|
| **Bulk** | ✅ `import_file`, `done` in 1.2 s | ✅ Step 1, signatures. ❌ Step 2 → pattern → network, all refused for want of a DE *design* | ✅ **verified on disk**: 2 PNGs (95 664 B, 89 511 B, valid magic) + a 12-row signature CSV with the expected header, + an RDS | **partial** — the DE design needs a human (§8.6) |
| **SC** | ❌ **no `import_sc` importer** — UI only | ✅ auto-pipeline, annotation, markers, pathways (last live: §9) | ⬜ not verified this phase | **partial** — cannot start on a fresh session (§9.5) |
| **Spatial** | ✅ `import_file`, `done` in 16.9 s, value boxes read 2 702 / 32 285 / `🟢 visium (LogNorm)` | ✅ `run_all` `done` 36.1 s, `btn_hotspots` `done` 1.5 s (`n_results: 1200`) | ⬜ wired and bound, but the hotspot **table never renders** (§10.4.4) | **partial** — a pre-existing presentation defect, not a protocol one |

- **14 modules / 15 buttons**, **7 tools**, unchanged tool count, no statistical
  method, default parameter or plot touched.

**Fully covered (import → analysis → export, on a fresh session, no human step):
Bulk up to signatures, and Spatial up to the analyses.** Partially covered: the
Bulk DE tail, the whole SC modality, and the Spatial export. None of the three
gaps is a protocol limitation; each is a missing or unrendered product surface,
and each is named below with what would close it.

### 11.2 Known limitations

0. **The Spatial hotspot PANEL does not render** (§10.4.4) — new to this list, and
   the most consequential open item for Spatial. The action reports `done` with
   `n_results: 1200` and the result slot is verifiably correct (a sibling output
   reads it: `585 hotspot(s), 615 coldspot(s) sur 2702 elements`), but the table,
   the map and the histogram deliver no value while Shiny's event loop is
   measurably idle, and a **human click reproduces it exactly**. The export
   therefore cannot be exercised. This is a pre-existing defect in
   `modules/spatial/mod_spatial_qc.R`; Phase F added a download handler to the same
   module and did not touch the three outputs.
1. **The DE design is not MCP-settable** (§8.5.1, re-measured §8.6.1). Six
   allowlisted inputs are absent from the MCP `set_inputs` surface, so the design
   is whatever the app derives — and the group selects stay empty even when the
   column is set from the client, so no contrast forms and the DE tail is
   unreachable. Consequence: Bulk's pattern and network actions inherit it.
2. **A fresh session cannot be armed through the MCP tool** (§8.5.2). The
   heartbeat only advances while armed, and the arm tool refuses a stale
   handshake. The bootstrap that works is writing `arm.json` directly. **This is
   the single most annoying item on the list**: the first action of every session
   has to bypass the tool.
3. **No SC `import_file` importer.** `TS_DRIVE_IMPORT_MODULES` is now
   `import_bulk` + `import_spatial`, so an agent can bootstrap **Bulk and Spatial**
   remotely and **not SC** (§9.5). This is the one remaining gap of the same class
   Phase F just closed for Spatial, and the Spatial importer is the template.
4. **`shared_rv$pathway_results` is shared between the Bulk and SC pathway
   modules** (§9.4.1), pre-existing and **still open**. Two enrichments in one
   session overwrite each other, and the Bulk panel reads the SC result. It was
   left alone deliberately: closing it changes what the Bulk panel reads, which is
   a behaviour change and not a protocol change.
5. **The network action's frozen thresholds can make it unreachable** on a dataset
   with more significant genes than the declared 200-node cap, with no remote way to
   narrow the gene set (§8.4.2).
6. **`import_file` is unreachable from `transcripto_drive_run`**, by declared design
   (`TS_MCP_RUN_ACTIONS <- c("run_pipeline")`). It needs the scenario file-drop, so
   it is the one action a client cannot perform through the tool it has.
7. **`scripts/mcp_server.R` is gitignored** (`.gitignore:43`). A fresh clone has no
   `run_pipeline` mapping for ANY of the 15 buttons, including the four added in
   Phases E and F. This is a repository-policy question, deliberately not taken
   here — but it is the difference between "propagated" and "usable from a fresh
   clone".
8. **Neither published manifest anchor follows the §1 algorithm** (§1.7). One
   tracked file, `tools/_drive/README.md`, is included by **both** published
   values and excluded by `-path ./tools/_drive -prune`, and §1.2's own arithmetic
   ("327, not 329") only reaches 327 by making that same omission. Every Phase F
   value follows the algorithm; both old rows are kept with the deviation stated.
9. **The live-tree fingerprint is not stable across a `git stash` round-trip on
   this host** (§1.7) — the repo is mixed LF/CRLF with `core.autocrlf = true`, so
   a stash/pop rewrites bytes without changing the file set. Cross-phase live-tree
   comparisons are unreliable; commit-scoped anchors are the only sound basis.

### 11.3 What is missing before a push would be reasonable

Ordered by what each would change.

1. **An `import_sc` importer** (§9.5). The Spatial importer is the template and
   the work is now a known quantity. This is the only item that would make **all
   three** modalities drivable from a fresh session, and it is a **verdict**
   change, not an improvement.
2. **The Spatial hotspot panel rendering** (§10.4.4). A pre-existing defect in
   `mod_spatial_qc.R`; the two output groups read different sources, and isolating
   which is at fault is a small, well-scoped piece of work. It gates the Spatial
   export and therefore the "through export" claim for that modality.
3. **The DE design settable from the protocol** (§8.6.1). Not by adding the four
   inputs to `set_inputs` — the MCP server cannot validate a value domain it
   cannot read. The honest fix is for the app to DERIVE a default design the way
   it already does, and for the drive to accept that, so a human choice becomes an
   override rather than a prerequisite.
4. **The arming bootstrap fixed in `set_armed`**, or documented as "write
   `arm.json` first" in the client instructions. One of the two; both is better.
5. **A decision on `scripts/mcp_server.R`**: track it, or move the button map into
   `R/core/` where it is versioned with the app. Today the two halves of the MCP
   surface live in different places, and only one of them is in git.
6. **Re-measure a commit-scoped fingerprint** once Phase F is committed, so the
   comparison anchor is reproducible. §1.7 shows the live-tree value is not.
7. **Decide what to do about `shared_rv$pathway_results`** (§11.2 item 4). It is
   the one known product defect that makes one modality's panel display another's
   result, and Phase F left it alone on purpose.

### 11.4 Performance and stability observed during the chains

**Phase F, measured this phase:**

- **Spatial import (new):** `done` in **16.9 s** for 32 285 genes × 2 702 spots,
  including the BPCells conversion and four histology images.
- **Spatial pipeline:** `done` in **36.1 s** (`n_results: 9`).
- **Spatial hotspots:** `done` in **1.5 s** (`n_results: 1200`) — the cheapest
  action in the whole surface.
- **Bulk:** import 1.2 s, Step 1 1.2 s, signatures 2.4 s.
- **Longest action overall** remains the SC auto-pipeline at 167.9 s (Phase E).
  Every Phase F action is an order of magnitude inside the 600 s `wait` budget.
- **`set_inputs` answers `applied`, not a terminal status** — by design (inputs
  updated, pipeline not started). A driver that polls for a terminal will wait its
  full timeout on a *successful* navigation. Phase F's own harness hit this and
  recovered; it is a sharp edge in the protocol, and `wait` documents the split.
- **The heartbeat is the reliable liveness signal, and it is worth using as one.**
  Proving `hb_n` advances before issuing the first scenario avoided a 1-hour
  silent stall: this host's Spatial module initialises six mirai daemons, and the
  app accepts scenarios long before it answers them. A driver that does not check
  liveness will read that as a hung app.
- **Stability:** no Shiny error, no MCP transport error and no session loss across
  the Phase F chains. One non-fatal 404 for a missing static resource on every
  page load, present before Phase F and unrelated to the drive.
- **Session lifecycle, observed honestly:** `ready.json` is removed by
  `ts_drive_invalidate_ready()` on session end, so a driver that outlives its
  page must re-read rather than reuse. Two separate probe scripts that each
  navigated created a second Shiny session and rotated the token — the documented
  behaviour, and the reason every Phase F chain runs in ONE process.

**Phase E, retained:** the SC auto-pipeline 167.9 s, SingleR 121.4 s, SC
enrichment 21.4 s, DE 2.0 s, Bulk pattern 0.4 s, Bulk network 14.3 s; the `wait`
tool distinguished `running` / `done` / `empty` / `timeout` correctly throughout;
a deliberate page reload recovered with a new token and all modules `not_ready`
with `n_results: 0`.

### 11.5 Verdict

**Push the Bulk and Spatial surface as a documented beta, with the ten limitations
above shipped alongside it. Not ready as "MCP is complete", and the gap is now SC
rather than Spatial.**

What changed in Phase F, in one line: **the modality that was blocked is no longer
blocked, and the modality that was merely inconvenient is now the blocked one.**
Spatial imports through the protocol (`done`, 16.9 s, with the app's own value
boxes agreeing), and its pipeline and hotspots both run on a fresh session. SC
cannot start without a human, because no `import_sc` importer exists — and that is
a known amount of work with a template already in the tree.

Three things should be said plainly rather than smoothed over:

1. **The Spatial "UI blocker" was a measurement artefact** (§10.4.1), and the
   fix that actually unblocked the modality was a protocol change, not a CSS
   rule. No UI change was made, because there was nothing in the UI to change.
2. **The Spatial export is wired but unexercised** (§10.4.4), because a
   pre-existing presentation defect leaves the hotspot table empty — on the human
   path too. This document does not claim a Spatial export it did not observe.
3. **The SC chain was not re-run this phase** (§9.5), so §9.1–§9.4 remain the last
   live measurement of it. That is stated rather than papered over with a verdict.

**The single highest-value next action is the `import_sc` importer.** It is the
only open item that changes a verdict rather than an observation, it is the last
instance of a class Phase F has now solved once, and it would make the "import →
analysis → export" claim true for all three modalities rather than two.

The two actions added in Phase F are themselves push-ready. The
`import_spatial` importer is frozen-input, rule-resolved where a rule is needed
(`sample_name` follows the module's own `basename()` rule), single-writer (the
180-line body became one function with two callers), offline-tested with a **real
domain execution** — a synthesised Slide-seq tree through the real
`load_spatial_slideseq()` and the real `convert_to_bpcells_and_fov()` — and
**falsified three times**: routing the importer away from the shared writer
produced 8 red assertions; inverting the allowlist's success branch made the
source-time guard fail at load; and reporting a loader failure as `invalid`
instead of `error` produced 3 red. The hotspot CSV export is additive,
bound, i18n-registered, refusal-tested, and source-locked.

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

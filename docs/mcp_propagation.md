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

## 5. Invariants

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

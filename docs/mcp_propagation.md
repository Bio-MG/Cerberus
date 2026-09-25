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

| Anchor | Fingerprint | Entries | Meaning |
|---|---|---:|---|
| Post-housekeeping v2 baseline | `044e3944ea7358d3697c7238d59b53059cbfb1d623c9b651afcfab0cff149494` | 436 | Pre-work state before the single SC button propagation |
| Post–single-SC-button v2 baseline | `e2a04cde4bd562bd53dcef64e58a3adf26123ad4e8062237bedafb4c88115d15` | 438 | State after the single SC button propagation |

These are the two v2 anchors surrounding the previously completed propagation.
The documentation refresh and any later code change necessarily produce a new
live-tree fingerprint; the two values above remain the accepted anchors for the
states they describe.

### 1.1 Post–second-SC-button measurement (not an accepted anchor)

| Measurement | Fingerprint | Entries | Composition |
|---|---|---:|---|
| After `sc_markers` propagation + the Bulk design note | `b8016637eddac47ef7588d3adb3ba38a288310d7797a8654a2a7572db63e16e9` | 441 | 438 at the post-Phase-A state, `+1` this note, `+2` files owned by a concurrent agent's commits (`tests/testthat/test-mod-bulk-de-run.R`, `tests/testthat/test-sc-communication-liana-engine.R`) |

This value is **recorded, not accepted**: the tree was not quiescent when it was
taken (`HEAD` moved `b3cbe11 → 9cb1a06` during the same window, and a concurrent
agent had modified two of the files this propagation also touches,
`config/defaults.R` and `modules/sc/mod_sc.R`). Re-measure after the tree is
quiescent before treating any number here as an anchor.

The value above was measured **immediately before this row was written**, and the
predecessor measurement of the same tree with this file not yet edited was
`9685302cd3739fd50d6904eb05b756f0899801057502c9d478e274c29be1acaa` (same 441
entries). A fingerprint cannot include its own recording: any edit to a manifest
member invalidates the number it reports. That is inherent to the algorithm, not a
defect — re-measure to compare trees, never to compare against a number written
inside one of them.


## 2. Scope summary

Tasks 1–3 covered, at a high level:

- **Task 1:** the Drive job lifecycle, including owned dispatch, terminal
  publication, timeout and session-loss outcomes, and truthful `running` versus
  terminal states.
- **Task 2:** the Spatial false-success fix, so a failed or invalid stage is
  not reported as a successful pipeline.
- **Task 3:** minimal Spatial MCP propagation, including its single owned
  button, readiness refusal, fixed navigation, and sanitized diagnostics.

The subsequent single-SC propagation added the `sc-pipeline-run_auto_pipeline`
drive action. It calls `run_sc_auto_pipeline()` with a frozen, declared input
set, publishes a long-job state, and reports per-step outcomes using only the
existing five-state vocabulary. The human modal button was not bound as a drive
action.

## 3. Current MCP inventory at the v2 anchor

The MCP inventory contains **7 tools** and did not gain a tool during the
propagation:

| Tool | Nature |
|---|---|
| `transcripto_drive_status` | read-only |
| `transcripto_drive_read_result` | read-only |
| `transcripto_drive_snapshot` | passive, read-only |
| `transcripto_drive_set_inputs` | controlled write, `scenario.json` only |
| `transcripto_drive_run` | controlled run, `run_pipeline` only |
| `transcripto_drive_wait` | bounded observation |
| `transcripto_drive_set_armed` | arm/disarm, `arm.json` only |

At the post–single-SC-button v2 anchor, `run_pipeline` covers **6 modules / 7
buttons**:

| Module | Drivable button |
|---|---|
| `import_bulk` | `import_bulk-btn_load` |
| `bulk_filter` | `bulk-filter-run_filter_norm` |
| `bulk_de` | `bulk-de-run_de` |
| `bulk_pathways` | `bulk-pathways-run_pathway` |
| `bulk_pathways` | `bulk-pathways-run_scores` |
| `spatial_pipeline` | `spatial-pipeline-btn_run_all` |
| `sc_pipeline` | `sc-pipeline-run_auto_pipeline` |

`set_inputs` is refused for `spatial_pipeline` and `sc_pipeline`: their inputs
are frozen at the action boundary and are not a settable Shiny-input surface.
The single-SC action also remains outside the `set_inputs` tool.

## 4. Additional SC actions after the v2 anchor

The live tree now adds exactly one action beyond the v2 anchor in each of the
two accepted propagation steps:

| Module | Drivable button | Frozen inputs |
|---|---|---|
| `sc_annotation` | `sc-annotation-run_annot` | `ref_singler = "hpca"`, `label_level = "main"`, `maxcells = 50000L` |
| `sc_markers` | `sc-markers-run_markers` | `marker_test = "wilcox"`, `marker_min_pct = 0.10`, `marker_logfc = 0.25`, `group_col = "seurat_clusters"`, `max_per_group = 5000L`, `only_pos = TRUE`, `verbose = FALSE` |

Both call their corresponding R action through a published drive token, do not
bind the DOM button, and publish a long-job state with the existing five-state
step vocabulary. `set_inputs` is refused for `sc_annotation` and `sc_markers` as
well.

Accordingly, the current live mapping is **8 modules / 9 buttons**: the seven
buttons in the v2 anchor table plus `sc-annotation-run_annot` and
`sc-markers-run_markers`. The MCP inventory remains **7 tools**. No Bulk code is
changed by this SC propagation; the separate Bulk extension note is design-only.

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

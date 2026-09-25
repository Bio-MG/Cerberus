# Bulk MCP extension — design note (NO CODE)

**Status:** DESIGN ONLY. No file under `modules/bulk/`, `R/bulk/` or `config/` is modified by this
note. It exists to decide *which* Bulk actions would be exposed next and *under which frozen
input sets*, before a single line of propagation code is written.

**Companion:** `docs/mcp_propagation.md` (the propagation record + the invariants below).
**Frozen domain contracts:** `docs/contracts/BULK_PATTERN_CONTRACT.md`,
`BULK_SIGNATURES_CONTRACT.md`, `BULK_WGCNA_CONTRACT.md`, `BULK_NETWORK_CONTRACT.md`,
`BULK_GSVA_CONTRACT.md`.

---

## 1. The invariants any new Bulk action must satisfy

These are not new rules; they are the ones the Bulk actions already in the allowlist obey.

| # | Invariant | Where it lives |
|---|---|---|
| 1 | Literal button ID in `TS_DRIVE_BUTTONS`; module in `TS_DRIVE_MODULES` | `R/core/drive_allowlist.R` |
| 2 | The literal is the `ns()`-resolved ID **including the nested module prefix** (`bulk-filter-run_filter_norm`, never `bulk-run_filter_norm`) | allowlist §"prefix is `bulk-filter-`, NOT `bulk-`" |
| 3 | **No DOM binding.** The observer calls the same `run_*()` function a human click would call | `ts_drive_publish_token()`, `ts_drive_bind_button()` = 0 |
| 4 | **Frozen input set** — the action reads a declared list, never `input$` | `.bulk_*_drive_inputs()` |
| 5 | Five-state step truthfulness: `ran` / `skipped` / `error` / `empty` / `not_ready`; a failed or empty step is never readable as `done` | `drive_state()` probe |
| 6 | Long job: `long = TRUE`, declare + close the job around the synchronous body | `TS_DRIVE_JOB_*` |
| 7 | `set_inputs` **refuses** the module (all modules except `import_bulk`) | `R/core/drive_watcher.R` |
| 8 | The MCP inventory stays at **7 tools** — propagation adds a button to `run_pipeline`, never a tool | `scripts/mcp_server.R` |

Consequence of #4 for Bulk specifically: any action whose real inputs are **derived from the
session** (a metadata column name, a multi-select of traits, a file path) cannot be given a
frozen set without a product decision. Those candidates are marked *blocked* in §4.

---

## 2. What already exists (measured)

| Literal button | Module | Prerequisite | Frozen inputs already in the allowlist |
|---|---|---|---|
| `import_bulk-btn_load` | `import_bulk` | file chosen in the UI | 12 inputs (`bulk_import_mode`, `counts_format`, headers, delimiter, `project_name`, `min_counts`, …) |
| `bulk-filter-run_filter_norm` | `bulk_filter` | `bulk_obj` loaded | `min_count=10`, `min_samples=1`, `min_count_per_sample=1` |
| `bulk-de-run_de` | `bulk_de` | `bulk_obj` + `vst_mat` | 9 inputs (`de_engine`, `lfc_thresh`, `padj_thresh`, …) |
| `bulk-pathways-run_pathway` | `bulk_pathways` | DE result | `enrich_mode`, `pathway_source`, `pathway_db`, `pathway_org`, `pathway_pval` |
| `bulk-pathways-run_scores` | `bulk_pathways` | `vst_mat` | `scores_source`, `scores_org`, `scores_method`, `scores_min_size`, `scores_max_size` |

### 2.1 Answers to the three names in the request

- **`bulk-import` — EXISTS, already exposed.** `import_bulk-btn_load`. Nothing to add.
- **`bulk-normalize` — DOES NOT EXIST as a button.** Normalization is *inside* Step 1: the button
  is labelled "Lancer Filtrage & VST" and produces `shared_rv$vst_mat` together with the filtering.
  There is no separate normalize step, so exposing one would mean *inventing* a product step.
- **`bulk-clustering` — NO generic clustering button exists.** Two different things do exist and
  must not be conflated:
  - `bulk-pattern-run_pattern` — **per-gene profile clustering** (STAT-S3, k-means on per-gene
    z-scores across groups). It is *not* sample clustering.
  - Sample-level structure inside the filter module (PCA, sample correlation) — **plots, not
    actions**; they have no `actionButton` and therefore no drive token to publish.

---

## 3. Candidate ranking (recommended order)

| Rank | Literal button | Module | Why it ranks here |
|---|---|---|---|
| 1 | `bulk-pattern-run_pattern` | `bulk_pattern` | Descriptive, pure-R contract, cheap, deterministic (seed frozen). One blocker: the group column. |
| 2 | `bulk-signatures-run_signatures` | `bulk_signatures` | Fully numeric frozen set, offline resources for the 3 built-in sources. One blocker: the `rds_local` file path. |
| 3 | `bulk-network-run_network` | `bulk_network` | Entirely numeric frozen set, offline Reactome (no network access needed → headless-friendly). Needs DE + `vst_mat`. |
| 4 | `bulk-wgcna-run_wgcna_power` then `bulk-wgcna-run_wgcna_modules` | `bulk_wgcna` | Two dependent steps + dynamic trait selection. Last, and only as a **pair**. |
| — | `bulk-mapping-run_mapping` | `bulk_mapping` | Technically the cheapest, but it **mutates** the working matrix (`counts_mapped` + `mapping_applied`, with an `undo`). See §4. |

---

## 4. Per-candidate design

### 4.1 `bulk-pattern-run_pattern` (rank 1) — prefix `bulk-pattern-`

Frozen input set (all values read from `modules/bulk/mod_bulk_pattern.R`):

```r
list(
  pattern_source = "up",        # up | down | all_sig   (UI default: first radio)
  pattern_k      = 4,           # numericInput, min 2, max 12
  pattern_seed   = 15           # numericInput, min 1
)
```

- Prerequisites: `shared_rv$vst_mat` **and** a DE result for the active contrast
  (`shared_rv$active_contrast` in `shared_rv$contrasts`). The button is `toggleState`-disabled
  without `vst_mat` — the drive readiness guard must mirror that, not rely on the toggle.
- Output: `shared_rv$pattern_result` → `n_results` = number of clusters.
- **Blocker:** `pattern_group` is a `selectInput(..., choices = NULL)` filled from the loaded
  metadata. It is a *data selector*, not a parameter. Freezing it to a hard-coded column name
  would silently pick the wrong column for another dataset, and freezing it to the first choice
  makes the result depend on column order. A future phase must first decide the policy
  (declared constant in `config/`? taken from the DE contrast column? refused?). **This note does
  not decide it.**

### 4.2 `bulk-signatures-run_signatures` (rank 2) — prefix `bulk-signatures-`

```r
list(
  sig_resource = "hallmark",     # hallmark | progeny | dorothea (rds_local excluded)
  sig_organism = "human",        # human | mouse
  sig_method   = "ssgsea",       # ssgsea | gsva | zscore | ulm_decoupleR
  sig_min_size = TS_BULK_GSVA_MIN_SIZE,   # 10
  sig_max_size = TS_BULK_GSVA_MAX_SIZE    # 500
)
```

- Prerequisite: `shared_rv$vst_mat`. Output: `shared_rv$signature_scores` (also re-annotates
  `global_data$bulk_obj`).
- **Blocker:** `sig_resource = "rds_local"` requires a `fileInput` path. A drive caller must never
  be able to hand a filesystem path to an action, so the frozen set must pin one of the three
  built-in resources. The `ulm_decoupleR` method is also worth a second look before freezing it
  (it changes what the output means).

### 4.3 `bulk-network-run_network` (rank 3) — prefix `bulk-network-`

```r
list(
  network_species   = "hsapiens",  # only choice today
  network_source    = "up",        # all_sig | up | down
  network_prize     = "padj",      # padj | lfc
  network_threshold = 1.3,         # = p 0.05 on the -log10(padj) scale
  network_omega     = 10,
  network_beta      = 1,
  network_mu        = 1
)
```

- Prerequisites: `vst_mat` + DE. Sources are **offline** (`mmuReactome.db`), so the action is
  headless-capable.
- ⚠️ Per `STATUS.md` §2bf this is a **pathway-derived network, not a PPI**: a relay is a predicted
  co-member of a pathway. The docs of the exposed token must say so, or the token invites a
  wrong scientific reading.

### 4.4 `bulk-wgcna-*` (rank 4) — prefix `bulk-wgcna-`, two steps

```r
# step 1
list(wgcna_n_genes = TS_BULK_WGCNA_MAX_GENES)   # 5000, slider 2000..5000
# step 2
list(wgcna_power_override = NA)                  # empty = keep the step-1 choice
```

- **Blockers:** (a) a genuine two-step dependency — exposing one button alone is meaningless;
  (b) `wgcna_traits` is a `selectizeInput(..., multiple = TRUE)` over the clinical metadata, i.e.
  dynamic again; (c) hard stop below `TS_BULK_WGCNA_MIN_SAMPLES` = 15 samples, so the readiness
  guard must report `not_ready` with that reason instead of letting the job fail.

### 4.5 `bulk-mapping-run_mapping` — prefix `bulk-mapping-` (deferred, not forbidden)

```r
list(
  map_organism         = "human",   # human | mouse
  map_from_type        = "ensembl", # ensembl | entrez | affy_probe
  strip_ensembl_version = TRUE,
  collapse_method      = "sum"      # sum | max_mean
)
```

- Prerequisite: `global_data$bulk_obj` only — the cheapest candidate by far.
- **Why it is not rank 1 despite that:** it is the first Bulk action that **changes the data
  downstream steps see** (it writes `shared_rv$counts_mapped`, sets `mapping_applied`, and keeps
  `counts_original` for `undo_mapping`). Exposing it means the drive surface can silently change
  the meaning of every later token in a session. It is the right candidate *after* the read-only
  ones, once the "a drive session may not mutate shared inputs" rule is written down.

---

## 5. Non-goals of this note

- No Bulk code, no allowlist entry, no MCP tool, no test, no config constant is added here.
- No new dependency, and no change to any frozen `BULK_*_CONTRACT.md`.
- No decision on the three dynamic-input blockers (§4.1, §4.2, §4.4) — they need a product call.
- `renv.lock` is untouched.

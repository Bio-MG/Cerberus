# MCP propagation — Tasks 1–3 provenance archive

This directory records the provenance boundary for the MCP propagation scope
frozen after Tasks 1–3. It contains no application source, tests, or runtime
configuration. The v2 documentation refresh records the state **after Tasks 1–3
and before the additional Single-Cell button propagation**, together with the
state **after that single SC button propagation**.

## v2 manifest anchors

The manifest algorithm is v2:

```
find . -path ./renv -prune -o -path ./python_env_sccoda -prune -o -path ./.git -prune \
  -o -path ./QC -prune -o -path ./.Rproj.user -prune -o -path ./node_modules -prune \
  -o -path ./.workbuddy-ai -prune -o -path ./tools/_drive -prune \
  -o -type f ! -name full_suite_results.txt ! -name .RData ! -name .RDataTmp \
  ! -name .Rhistory -print0 | sort -z | xargs -0 sha256sum
```

| Anchor | Fingerprint | Entries | Boundary |
|---|---|---:|---|
| Post-housekeeping v2 baseline | `044e3944ea7358d3697c7238d59b53059cbfb1d623c9b651afcfab0cff149494` | 436 | State after Tasks 1–3, before the single SC button propagation |
| Post–single-SC-button v2 baseline | `e2a04cde4bd562bd53dcef64e58a3adf26123ad4e8062237bedafb4c88115d15` | 438 | State after the single SC button propagation |

The command excludes `renv/`, `python_env_sccoda/`, `.git/`, `QC/`,
`.Rproj.user/`, `node_modules/`, `.workbuddy-ai/`, and `tools/_drive/`, as well
as files named `full_suite_results.txt`, `.RData`, `.RDataTmp`, and `.Rhistory`.
The latter exclusions are part of the v2 housekeeping convention; the live IPC
directory is never hashed.

## Contents and provenance

The two manifest text files retained in this directory are historical snapshot
copies from the Tasks 1–3 archive. The v2 anchors above are the authoritative
fingerprints for the before/after boundary described by this README; the
historical filenames are not v2 fingerprint names.

| File | Role |
|---|---|
| `manifest_baseline_de48f509.txt` | Historical initial-audit snapshot copy |
| `manifest_final_386840c9.txt` | Historical post-Task-3 snapshot copy |

Intermediate snapshots are not required to understand the v2 boundary: the two
v2 hashes and entry counts identify the states before and after the single SC
button propagation. The archive does not authorize or describe any additional
propagation beyond that recorded boundary.

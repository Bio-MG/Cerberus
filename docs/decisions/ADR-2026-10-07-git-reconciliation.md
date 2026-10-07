# ADR-2026-10-07: Git history reconciliation for TranscriptoShiny

## Status

Accepted

## Context

- The original repository carried **two unrelated histories**. `git merge-base
  --all main origin/main` returned **empty**, and
  `git rev-list --left-right --count main...origin/main` reported `18 382` —
  there was **no common ancestor**.
- `origin/main` was the **published Cerberus base** at `df3369d`
  (382 commits, tree `4f97c61be60be6bd78eb90ae0593fd99f9b44089`).
- The local `main` was a **rewritten MCP development line** whose root was the
  synthetic commit `93cbecb` (`feat(mcp-S0)`), a **parentless** commit carrying a
  376-file tree. It had 18 commits; its tip was `b494404`.
- That synthetic root was produced by an earlier **squash** of the published
  line: `93cbecb`'s tree is **374/376 byte-identical** to `origin/main`'s tree,
  and the only two differing blobs are the two files that S0 itself touches.
  The rewrite was therefore a *history* operation, not a *content* one.
- No content was lost: the local line was a **superset** of `origin/main` by
  path (376/376 shared, 0 missing) plus the MCP work.
- The goal was **one coherent `main` history** that preserves the published
  ancestry **without losing the validated MCP work**.

## Decision

- All reconciliation was performed in an **independent scratch clone**
  (`git clone --no-hardlinks`); the original repository was **never modified**.
  Object independence was measured (distinct pack inodes, link count 1,
  `git fsck` clean), so a later `gc`/`prune` of the original cannot break it.
- **S0 was applied as a two-file patch** (`df3369d -> 93cbecb`:
  `scripts/mcp_server.R` +52/-10, `tests/testthat/test-mcp-sc-local.R` +100/-1 —
  total +152/-11) **rather than cherry-picked**, because `93cbecb` is a *root*
  commit and cherry-picking it would re-add the entire tree. Applied as commit
  `fd24a05`; the resulting tree was verified as
  `700d1f8e437841595b755a73167ef9c50bf51655`.
- The **17 validated MCP commits** were then replayed **in order**, in batches of
  three, onto the published base.
- Each commit was replayed with `git cherry-pick -n`, and the **staged tree was
  verified against the original commit's tree *before* the commit was created**;
  `git commit -C <original>` preserved author, message and author date.
- The **MCP/drive gate** (`tools/run_tests.R mcp drive`) was run after every
  batch; all batches were green (`failed=0`, `error=0`, exit 0).
- The **final tree** exactly matches the original local tip:
  `c6f07bf21f69f1fd4f3ced004765226d5f3a77da` (= `b494404`'s tree) — the port is
  **byte-exact**.
- `df3369d` is now an **ancestor** of the replayed HEAD
  (`git merge-base --is-ancestor df3369d HEAD` exits 0).
- The replayed branch is **`reconcile` at `0cc9737`**.
- **Nothing was pushed**, no ref of the original repository was changed, and no
  branch was deleted.

## Evidence

Durable documents (repository-relative paths):

- `docs/release/evidence/STEP0_VERIFICATION_2026-10-06.md` — refs, trees and the
  three plan-level blockers.
- `docs/release/evidence/STEP1_SCRATCH_CLONE_AND_BASELINES_2026-10-06.md` —
  scratch clone, evidence backup, exact references, S0 delta, test baselines.
- `docs/release/evidence/STEP2_REPLAY_REPORT_2026-10-06.md` — the batch-by-batch
  replay, per-batch SHAs and trees, gate results, and the final verification.
- `docs/release/evidence/GIT_TOPOLOGY_FORENSIC_2026-10-06.md` — the read-only
  forensic audit of the disjoint histories.
- `docs/release/evidence/GIT_RECONCILIATION_PLAN_2026-10-06.md` — the
  reconciliation plan.
- `docs/release/evidence/gates/` — live gate captures.
- `docs/release/evidence/manifests/` — reproducible file manifests.
- `docs/release/evidence/AUTHORITATIVE_full_suite.txt` — the authoritative full
  suite capture.

Key recorded values:

| Item | Value |
|---|---|
| Published base | `df3369d4d15d5dd7a9d5ec4fb9288b98f84ada3d`, tree `4f97c61be60be6bd78eb90ae0593fd99f9b44089` |
| Synthetic squash root (S0) | `93cbecb6519a48347fb90591268579b9c1d33e92`, tree `700d1f8e437841595b755a73167ef9c50bf51655` |
| Original local tip | `b4944041141a288b3c802e656e007af722a857a9`, tree `c6f07bf21f69f1fd4f3ced004765226d5f3a77da` |
| Replayed branch | `reconcile` at `0cc9737c505886611fcd767f563f72367a78c42f` |
| Final tree (replayed) | `c6f07bf21f69f1fd4f3ced004765226d5f3a77da` — equals the original tip tree |
| Final gate | `filter=mcp|drive, files=26, failed=0 passed=3685 error=0 skipped=0`, exit 0 |

This ADR deliberately contains **no tokens, credentials, private host paths or
host-state captures**; local paths that appear in the historical evidence
reports are incidental records of where an investigation ran and carry no
secrets.

## Consequences

- The **published history and the MCP development line are now joined**: the
  replayed line descends from `df3369d`, so `origin/main`'s ancestry is
  preserved while the validated MCP work is retained.
- **`reconcile` is the candidate for the future `main`.** Promoting it is a
  separate, not-yet-taken decision.
- **No push, no branch deletion, and no modification of the original repository
  has occurred.** The original still sits on `main` at `b494404` with 7 refs.
- The **backup branches remain intact** (`backup-pre-rewrite` `caee471`,
  `backup-pre-squash` `54eb602`) and must be kept until a separate integration
  decision is made. `backup-pre-rewrite` (`caee471`) retains the complete
  published ancestry, including `df3369d`. `backup-pre-squash` (`54eb602`) is
  rooted at the former synthetic S0 commit `93cbecb` and has no common ancestor
  with `df3369d`. Both backup refs remain intact.
- The earlier **squash is now understood as the root cause** of the disjoint
  histories; future history rewrites should be avoided in favour of
  replay-onto-published-base, which is verifiable by tree hash.
- **Future agent artefacts must follow the docs publication policy**: `docs/` is
  ignored by default with an explicit allowlist, so only curated, reviewed
  documents under `docs/release/evidence/`, `docs/mcp/` and `docs/decisions/`
  become visible to Git. Agent memory and scratch output stay outside the
  published tree.

## Alternatives considered

- **Merging the two unrelated histories** (`git merge --allow-unrelated-histories`).
  Rejected: it would create a merge commit with two roots and leave the
  duplicated published content as spurious conflicts, with no tree-hash proof of
  equivalence.
- **Keeping the two histories separate** (two branches, or splitting the MCP
  layer into its own repository). Rejected: it leaves `main` unusable for
  provenance and does not deliver the "one coherent history" goal. It also
  requires the extracted server to become self-contained, which it is not — it
  still sources the drive core from the app checkout.
- **Replaying onto the published base** — **chosen**. It preserves the published
  ancestry, keeps the validated MCP work, and is **verifiable by tree hash**
  (`93cbecb` reproduces `origin/main` + S0; the final replay reproduces
  `b494404`'s tree byte-for-byte).

## References

- `docs/release/evidence/STEP0_VERIFICATION_2026-10-06.md`
- `docs/release/evidence/STEP1_SCRATCH_CLONE_AND_BASELINES_2026-10-06.md`
- `docs/release/evidence/STEP2_REPLAY_REPORT_2026-10-06.md` — contains the
  authoritative final gate: `filter=mcp|drive, files=26, failed=0 passed=3685
  error=0 skipped=0`, exit 0, run on `HEAD = 0cc9737` whose tree
  `c6f07bf21f69f1fd4f3ced004765226d5f3a77da` is byte-identical to the original
  `b494404` tree.
- `docs/release/evidence/gates/` and `docs/release/evidence/manifests/`

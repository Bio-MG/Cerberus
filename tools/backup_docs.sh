#!/usr/bin/env bash
# backup_docs.sh -- verified backup of the governance layer (docs/ + AGENTS.md + PROJECT.md).
#
# Usage   : tools/backup_docs.sh [--push]
#           --push ALSO pushes the git mirror to its origin (private GitHub repo),
#           only if the secrets gate is clean and origin is configured.
# Tier 1  : rotating snapshot -> <DEST_BASE>/<UTC-ts>-<head>/  (keeps last 8; prints what it prunes)
# Tier 2  : git mirror        -> <MIRROR> (full history; `git remote add origin <private-url>` once, then --push)
# Excluded: docs/repomix/ (regenerable packs, 14.7 MB — xpert F3 / decision D-B: regenerable artifacts are not stored)
# Secrets : high-signal gate; a hit never blocks the LOCAL snapshot, always blocks --push.
# Id      : based on docs/agent/skills/snapshot-save (manifest source==copy principle, simplified).
#
# Measured basis (2026-10-28): docs/ = 22.6 MB of which repomix/ = 14.7 MB -> ~8 MB per backup.

set -u
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
DEST_BASE="E:/Backups/Cerberus-docs"
MIRROR="E:/Backups/Cerberus-docs-git"
PUSH=0; [ "${1:-}" = "--push" ] && PUSH=1

cd "$ROOT" || exit 2
HEAD=$(git rev-parse --short HEAD 2>/dev/null || echo no-git)
NOW=$(date -u +%Y-%m-%dT%H%M%SZ)
SCOPE="docs AGENTS.md PROJECT.md"

# --- secrets gate (high-signal: assignment-shaped or known token prefixes) ---
HITS=$(grep -rInE "((api[_-]?key|password|secret|token)[\"']?\s*[:=]\s*[\"'][^\"']{16,}|sk-[A-Za-z0-9]{20,}|ghp_[A-Za-z0-9]{30,}|github_pat_[A-Za-z0-9_]{20,}|AKIA[0-9A-Z]{16}|-----BEGIN [A-Z ]*PRIVATE KEY-----)" \
  docs AGENTS.md PROJECT.md --exclude-dir=repomix 2>/dev/null | grep -v -i "audit:ignore" | head -5)
if [ -n "$HITS" ]; then
  echo "SECRETS GATE: SUSPECT HITS (review before any push):" >&2
  echo "$HITS" >&2
  GATE=BLOCKED
else
  echo "secrets gate: clean (high-signal patterns, repomix excluded)"
  GATE=CLEAN
fi

# --- Tier 1: rotating verified snapshot --------------------------------------
DEST="$DEST_BASE/$NOW-$HEAD"
mkdir -p "$DEST" || { echo "FATAL cannot create $DEST" >&2; exit 2; }
LIST="$DEST/files.nul"
find $SCOPE -type f ! -path "docs/repomix/*" -print0 2>/dev/null | LC_ALL=C sort -z > "$LIST"
N=$(tr -cd '\0' < "$LIST" | wc -c | tr -d ' ')
[ "$N" -eq 0 ] && { echo "FATAL empty inventory" >&2; exit 3; }
BYTES=$(find $SCOPE -type f ! -path "docs/repomix/*" -printf '%s\n' 2>/dev/null | awk '{s+=$1} END {printf "%d", s+0}')
tar --force-local --null -T "$LIST" -cf "$DEST/payload.tar" || { echo "FATAL tar failed" >&2; exit 4; }
xargs -0 sha256sum < "$LIST" | LC_ALL=C sort -k2 > "$DEST/MANIFEST.src"
REH="$DEST/.rehearsal"; rm -rf "$REH"; mkdir -p "$REH"
tar --force-local -xf "$DEST/payload.tar" -C "$REH" || { echo "FATAL extract failed" >&2; exit 5; }
( cd "$REH" && xargs -0 sha256sum < "$LIST" | LC_ALL=C sort -k2 ) > "$DEST/MANIFEST.dst"
IN_TAR=$(tar --force-local -tf "$DEST/payload.tar" | wc -l | tr -d ' ')
rm -rf "$REH"
if cmp -s "$DEST/MANIFEST.src" "$DEST/MANIFEST.dst" && [ "$IN_TAR" -eq "$N" ]; then
  PAY=$(sha256sum "$DEST/payload.tar" | cut -d' ' -f1)
  DIRTY=$(git status --porcelain 2>/dev/null | wc -l | tr -d ' ')
  printf '{\n  "taken_at": "%s",\n  "head": "%s",\n  "file_count": %s,\n  "total_bytes": %s,\n  "payload_sha256": "%s",\n  "dirty_git_entries": %s,\n  "secrets_gate": "%s",\n  "excluded": ["docs/repomix/ (regenerable)"]\n}\n' \
    "$NOW" "$HEAD" "$N" "$BYTES" "$PAY" "$DIRTY" "$GATE" > "$DEST/snapshot.json"
  echo "TIER1 OK : $DEST ($N files / $BYTES bytes, manifest source==copy, payload $PAY)"
else
  echo "TIER1 FAILED (manifest mismatch) -- snapshot unusable" >&2; exit 4
fi

# --- retention: keep last 8, never silent (snapshot-save skill §7) -----------
ls -1d "$DEST_BASE"/2* 2>/dev/null | sort | head -n -8 | while read -r old; do
  echo "RETENTION: pruning $old (keeping last 8)"; rm -rf "$old"
done

# --- Tier 2: git mirror (full history) ---------------------------------------
if [ ! -d "$MIRROR/.git" ]; then
  mkdir -p "$MIRROR" && git -C "$MIRROR" init -b main >/dev/null 2>&1 && \
    echo "TIER2 INIT: git mirror created at $MIRROR"
fi
if [ -d "$MIRROR/.git" ]; then
  ( cd "$MIRROR"
    # Tier 2 fed from the VERIFIED Tier-1 payload (manifest source==copy): rsync is
    # absent on this machine and a plain `cp -r` fallback ignores --exclude, which
    # had polluted the mirror with a nested docs/docs/ and docs/repomix/ (15.4 MB).
    # The payload entries are "docs/...", "AGENTS.md", "PROJECT.md" relative to the
    # repo root, so they extract AT THE MIRROR ROOT (a `-C docs` would re-nest them
    # as docs/docs/). Extraction also drops files deleted since the previous run.
    rm -rf docs
    tar --force-local -xf "$DEST/payload.tar" -C .
    cp "$ROOT/AGENTS.md" "$ROOT/PROJECT.md" . 2>/dev/null
    git add -A >/dev/null 2>&1
    if ! git diff --cached --quiet 2>/dev/null; then
      git commit -q -m "docs backup $NOW @ $HEAD (auto: tools/backup_docs.sh)" && echo "TIER2 OK : history commit on mirror ($(git rev-parse --short HEAD))"
    else
      echo "TIER2 OK : mirror already up to date"
    fi
    if [ "$PUSH" -eq 1 ]; then
      if [ "$GATE" != "CLEAN" ]; then echo "TIER2 PUSH REFUSED: secrets gate not clean" >&2; exit 0; fi
      if git remote get-url origin >/dev/null 2>&1; then
        git push -q origin main && echo "TIER2 PUSH OK : origin main updated"
      else
        echo "TIER2 PUSH SKIPPED: no origin set (git -C $MIRROR remote add origin <PRIVATE-REPO-URL>)" >&2
      fi
    fi
  ) || exit 5
fi

echo "DONE. Next GitHub step (once): create empty PRIVATE repo, then:"
echo "  git -C $MIRROR remote add origin https://github.com/<you>/cerberus-docs-private.git"
echo "  bash tools/backup_docs.sh --push"
exit 0

# MCP transport correction — M1/M2 only (2026-09-24)

**Authorisation** : explicit, **transport-correction pass only**. **M3 and M4 were NOT started.**
Fixes audit finding **F-1** (blocking, server-side framing) plus **F-2** (`ping`) and **F-3**
(`armed` inconsistency). The over-redaction of `pathways` was **deliberately not addressed** (no
disclosure policy was proposed first, per instruction).

---

## 1. Pre-edit gate

| Control | Result |
|---|---|
| Manifest recomputed **before** | `bccc980bd46c0bceda8534c91bac467500a2c1bfabed6d94472d6d4855e16fc1` (437 f.) |
| Required | `bccc980bd46c0bceda8534c91bac467500a2c1bfabed6d94472d6d4855e16fc1` |
| Verdict | ✅ **PASS** — no drift, editing allowed |
| Target file before | `scripts/mcp_server.R` = `6065207d3fc06a79…` (LF, 688 lines, no BOM) |
| Tests pinning the old framing | **none** (`grep` over `tests/` + `tools/`: no `Content-Length`, `write_frame`, `read_header`, `mcp_server`) |

## 2. Implementation plan (as executed)

**Files to modify: exactly ONE — `scripts/mcp_server.R`.** Nothing else is touched: the drive
protocol (`R/core/drive_*.R`), `app.R`, modules, `tests/`, `renv.lock`, documentation, client
configurations, allowlists and convention guards are all untouched. No dependency added, no second
server, no `snapshot`/`set_inputs`/`run`/`wait`, no arbitrary R evaluation.

1. **Framing → NDJSON.** Replace the `Content-Length` layer with a line-oriented one:
   - `.ts_json()` — unchanged shape, plus a `fixed` guard substituting any literal CR/LF so a body
     can never contain an embedded newline.
   - `.ts_write_message()` — `cat(body, "\n")` + `flush(stdout())`; one message per line, nothing
     else on stdout. `cat()` is kept deliberately (`writeBin`/`writeChar` write nothing to stdout on
     this host — measured).
   - `.ts_read_message()` — read bytes until LF; `NULL` on clean EOF, `eof=TRUE` for a final line
     with no trailing LF, `too_long` guard.
   - `.ts_strip_cr()` — drop one trailing CR via `endsWith()` (no regex, so the `\s` trap cannot bite).
   - Removed: `.ts_eol`, `.ts_write_frame`, `.ts_read_header`.
2. **Serve loop** — line-based; **blank lines ignored** (not messages); a malformed line answers
   `-32700` and the loop **CONTINUES** (recovery, not shutdown); diagnostics stay on stderr.
3. **`ping`** — return `.ts_empty_object()` (`setNames(list(), character(0))`) so the envelope is
   `{}`; jsonlite renders a bare `list()` as `[]`.
4. **`armed` consistency** — `.ts_tool_status()` now reports `isTRUE(hb$armed)`, i.e. the
   **app-published `ready.json.armed`**. The `ts_drive_arm_state()` call was removed: it is gated by
   `ts_drive_interactive()` and therefore answered `"non-interactive session" -> FALSE` forever under
   `Rscript`. All session/heartbeat validation is untouched.
5. Header comment updated to document the new framing and why it replaced the old one.

## 3. Before / after

| | Value |
|---|---|
| Manifest **before** | `bccc980bd46c0bceda8534c91bac467500a2c1bfabed6d94472d6d4855e16fc1` (437 f.) |
| Manifest **after** | `48f758e35f4962ac57074d60590b0c7f78ccef21519e3e0308b5ee49a49115f1` (437 f.) |
| **Exact delta** | **1 entry — `scripts/mcp_server.R` only** |

```
< 6065207d3fc06a79c719de8f82758745ada2b433e2ec1f268c4dc8115984d8a3 *./scripts/mcp_server.R
> 14a6620e50695073461b7afe8858137052427855c7a3d8dd2c166bc5467454b8 *./scripts/mcp_server.R
```

**Nothing else changed.** `tools/_drive/` = `README.md` alone · `git status` = ` M renv.lock`
(pre-existing) · `HEAD` = `9cb1a06` · file still **LF**, no BOM.
**Guards re-run: `0 erreur(s), 59 avertissement(s)`** — identical to the documented baseline.

## 4. Protocol evidence — official `@modelcontextprotocol/sdk` **1.30.1**

Against the fixture root (session present), a **real third-party client**, no longer a hand-rolled probe:

| Step | Result |
|---|---|
| `connect()` / `initialize` | ✅ **`connect_ok: true`** (previously `-32001` timeout) |
| `serverInfo` | `{"name":"transcriptoshiny-drive","version":"0.2.0-m2"}` |
| capabilities | `{"tools":{"listChanged":false}}` |
| `tools/list` | ✅ **exactly 3** — `transcripto_drive_read_result`, `transcripto_drive_set_armed`, `transcripto_drive_status` |
| `set_armed` | ✅ **visible, never invoked** |
| `tools/call` `…_status` | ✅ `isError:false`, structured payload |
| `tools/call` `…_read_result` | ✅ `isError:false`, structured payload |
| **`ping`** | ✅ **`ping_result: {}`** (was `[]`) |
| unknown tool | ✅ `MCP error -32602: Unknown tool: no_such_tool` |
| `close()` | ✅ `closed_cleanly: true` |

Against the **real root** (no session): `connect_ok: true`, 3 tools, both read-only calls return
structured **`NO_SESSION`** (`isError:true`), `ping {}`, clean close — **the SDK path works in both
states**.

### F-3 resolved (measured)
One fixture session with `ready.json.armed = true`:

| Tool | before | after |
|---|---|---|
| `transcripto_drive_status` | `armed: false` | **`armed: true`** |
| `transcripto_drive_read_result` | `armed: true` | `armed: true` |

The two read-only tools now **agree**.

### Regression sweep (scripted NDJSON client)
| Launch vector | purity | initialize | tools | errors | recovery | ping | CRLF line | leak |
|---|---|---|---|---|---|---|---|---|
| WorkBuddy (exact config, real root) | ✅ | ✅ | 3/3 | ✅ | ✅ | `{}` | ✅ | none |
| ZCode (exact config, real root) | ✅ | ✅ | 3/3 | ✅ | ✅ | `{}` | ✅ | none |
| WorkBuddy / ZCode / bare (fixture) | ✅ | ✅ | 3/3 | ✅ | ✅ | `{}` | ✅ | none |
| **NEGATIVE CONTROL** (`--no-init-file` omitted) | 🔴 **`False`** | — | — | — | — | — | — | — |

The negative control **still detects pollution**, so the purity check is load-bearing, not decorative.

### Validation preserved
- `STALE_SESSION` ✅ — aged handshake (`age 601.7 s > timeout 15 s`) refused by **both** read-only tools.
- `NO_SESSION` ✅ — real root, both tools.
- `-32701/-32702/-32700` ✅ — unknown method / unknown tool / malformed line.
- **Recovery** ✅ — ids `7, 8, 9` answered **after** the malformed line and the blank line.
- Blank line ✅ — ignored, no spurious reply.
- **CRLF-terminated input** ✅ — accepted (one trailing CR stripped).
- **Clean EOF** ✅ — exit `0` with nothing buffered.
- **stdout purity** ✅ — every line parsed as a JSON-RPC message.
- **No leakage** ✅ — session token, fixture absolute path, a planted hidden `gene_names` array and a
  `raw_path` field appear on **neither** stdout **nor** stderr; `session_token` = `"<redacted>"`.
- Wildcard refusal ✅ — **unchanged**; it lives in `transcripto_drive_set_armed`, which was not touched.

## 5. Tool inventory (unchanged)

**Exactly three**: `transcripto_drive_status` (read-only) · `transcripto_drive_read_result`
(read-only) · `transcripto_drive_set_armed` (arm/disarm, writes `arm.json` only).
No `snapshot`, `set_inputs`, `run`, or `wait`. `set_armed` was **never invoked** during this pass.

## 6. Declaration

> **One file modified**: `scripts/mcp_server.R`. The drive protocol, `app.R`, Shiny modules,
> `tests/`, `renv.lock`, documentation, client configurations, allowlists and convention guards are
> **untouched**. **No dependency added**, no second server. **M3 and M4 were NOT started.**

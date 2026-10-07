# MCP multi-client compatibility audit — READ-ONLY (2026-09-24)

**Authorisation** : read-only, explicit. **Nothing was modified** in the server, the app, the
drive protocol, `tests/`, `renv.lock`, the documentation, or any client configuration.
**M3 / M4 : not started.**

---

## 1. Pre-flight gate

| Control | Result |
|---|---|
| Frozen manifest recomputed **before** | `bccc980bd46c0bceda8534c91bac467500a2c1bfabed6d94472d6d4855e16fc1` (**437** files) |
| Required | `bccc980bd46c0bceda8534c91bac467500a2c1bfabed6d94472d6d4855e16fc1` |
| Verdict | ✅ **PASS** — no unexpected drift, audit allowed to proceed |
| `git status` / `HEAD` | ` M renv.lock` (pre-existing) · `9cb1a06` |

## 2. Clients actually tested

| # | Client | Version | How it was exercised | Result |
|---|---|---|---|---|
| 1 | **WorkBuddy** | host app; config `~/.workbuddy-ai/mcp.json` (sha256 `0febf769…`) | **Launch vector replayed** byte-for-byte from the live config, driven by a scripted stdio client | Launch vector ✅ / protocol see §4 |
| 2 | **ZCode** | config `<repo>/.zcode/config.json` (sha256 `4746a850…`) | **Launch vector replayed** byte-for-byte, incl. `cwd` + `env` + `timeoutMs` | Launch vector ✅ / protocol see §4 |
| 3 | **Official MCP SDK** (`@modelcontextprotocol/sdk`) | **1.30.1** | **Independent third-party client implementation** (`Client` + `StdioClientTransport`), installed into the isolated node workspace | 🔴 **FAILED at `initialize`** — see F-1 |

**Honest limitation.** The WorkBuddy and ZCode **GUI applications cannot be driven headlessly on this
host**, so their *client internals* were **not** exercised. What was tested for them is the **launch
vector** (command + args + cwd + env exactly as configured) plus the **server side of the protocol**.
"No `zcode` binary on `PATH`" and "no scriptable WorkBuddy MCP surface" are facts, not oversights.

**Additional clients surveyed but not usable here:** `claude` CLI — **not installed**; Claude Desktop
— **no config present**; VS Code 1.138.0 — binary present but **no MCP config**, and creating one
would modify a client configuration (**out of scope**); Python `mcp` SDK — **not installed**.

## 3. Exact configuration used

WorkBuddy (`~/.workbuddy-ai/mcp.json`, unchanged):
```json
{"mcpServers":{"transcriptoshiny-r-btw":{"command":"D:/Data_science/R-4.4.2/bin/Rscript.exe",
  "args":["--no-init-file","D:/Data_science/SHINYAPP test (git work)/SHINYAPP test/scripts/mcp_server.R"]}}}
```
ZCode (`<repo>/.zcode/config.json`, unchanged): same command/args plus
`"cwd":"D:/Data_science/SHINYAPP test (git work)/SHINYAPP test"`, `"env":{}`,
`"enabled":true`, `"timeoutMs":60000`.

**Fixture.** Because the IPC directory is hard-wired to `<root>/tools/_drive` (no env override), the
fixture was an **out-of-repo root** at
`%TEMP%/ts_mcp_audit/fixture_root` containing byte-identical copies of `scripts/mcp_server.R`,
`R/core/drive_watcher.R`, `R/core/drive_allowlist.R` (verified by sha256), plus stub `app.R` and
`renv/activate.R`, and a synthetic `tools/_drive/{ready.json,result.json}`. **`tools/_drive/` in the
real repo was never written to** and still contains `README.md` alone.
`transcripto_drive_set_armed` was **never invoked**; no real session was armed or disarmed.

## 4. Pass/fail per protocol step

| Step | WorkBuddy vector | ZCode vector | SDK 1.30.1 |
|---|---|---|---|
| Server starts | ✅ | ✅ | ✅ (process starts) |
| Uses `--no-init-file` | ✅ present, first in `args` | ✅ present, first in `args` | ✅ passed by us |
| `initialize` succeeds | ✅ `2025-06-18` | ✅ `2025-06-18` | 🔴 **timeout `-32001`** |
| `tools/list` = exactly 3 | ✅ | ✅ | ⛔ never reached |
| `tools/call` read-only ×2 | ✅ (structured) | ✅ (structured) | ⛔ never reached |
| `set_armed` visible, not called | ✅ | ✅ | ⛔ never reached |
| stdout framing pure | ✅ | ✅ | ⛔ |
| Errors structured | ✅ `-32601/-32602/-32700` | ✅ same | ⛔ |
| No token/path/data leak | ✅ | ✅ | n/a |

Server identity as negotiated: `serverInfo = {"name":"transcriptoshiny-drive","version":"0.2.0-m2"}`,
`capabilities = {"tools":{"listChanged":false}}`, `protocolVersion = "2025-06-18"`.
Read-only calls returned real structured payloads against the fixture, and structured **`NO_SESSION`**
against the real root (no session attached) and **`STALE_SESSION`** against an aged handshake.

### Positive control for `--no-init-file`
The flag was deliberately **omitted** against the **real** root (which has a `.Rprofile`):
`framing_pure = False`, `initialize = False`, `0` messages parsed. The harness therefore **can**
detect stdout pollution, and the `--no-init-file` requirement is **measured**, not assumed.

## 5. Findings

### 🔴 F-1 — Server-side, spec-level: stdio framing does not match MCP (BLOCKING for interop)
The server speaks **LSP-style framing** (`Content-Length: N\r\n\r\n{json}`), while the MCP stdio
specification mandates **newline-delimited JSON**:

> "Messages are delimited by newlines, and **MUST NOT** contain embedded newlines."
> "The server **MUST NOT** write anything to its `stdout` that is not a valid MCP message."
> — MCP specification, `basic/transports` §stdio (2025-06-18; unchanged in 2026-07-28)

The reference implementation agrees: `@modelcontextprotocol/sdk` v1.30.1 **serialises as
`JSON.stringify(msg) + '\n'`** and reads by splitting on `\n`. It contains **no `Content-Length`
stdio transport at all**.

**Proven in both directions:**
1. **SDK → server**: the SDK's `initialize` **times out** (`-32001`) — the server waits for
   `\r\n\r\n`, which a bare JSON line never contains. A bare NDJSON line fed to the server yields
   **0 bytes** on stdout.
2. **server → SDK**: feeding the server's real output to the SDK's own
   `deserializeMessage()` produces the line `"Content-Length: 409"` →
   **`PARSE FAILURE: Unexpected token 'C'`**.

**Client-specific or server-specific?** **Server-specific.** Any spec-conformant client fails;
only a client that happens to implement LSP-style framing can talk to this server. This is an
**interoperability defect, not a security defect** — no leakage was observed.

### 🟡 F-2 — Server-side, minor: `ping` returns an array
`ping` answers `result: []` instead of `{}`. Valid JSON, but the MCP result envelope is an object;
a client validating the result schema may reject it.

### 🟡 F-3 — Server-side, read-only observability: the two read-only tools contradict each other
Against **one** fixture session with `ready.json.armed = true` and `result.json.armed = true`:

| Tool | reported `armed` |
|---|---|
| `transcripto_drive_status` | **`false`** |
| `transcripto_drive_read_result` | **`true`** |

Cause: `.ts_tool_status()` reads `ts_drive_arm_state()`, which returns
`list(armed = FALSE, reason = "non-interactive session")` whenever `ts_drive_interactive()` is FALSE —
**always** the case under `Rscript`. The M2 arm/disarm tool deliberately avoids this by reading
`ready.json.armed` instead, so the M1 status tool is the outlier. **No security impact** (the write
path is unaffected), but the field is **misleading**: it can never report `true`.

### 🟡 Observation — over-redaction
The warning `"bulk_pathways: 2 pathways found [GOBP]"` was published as
`"bulk_pathways: 2 <redacted> found [GOBP]"`: `pathways` is 8 characters and is not in the `known`
vocabulary (`TS_DRIVE_STATUSES ∪ MODULES ∪ ACTIONS ∪ VIEWERS`), so the "8+ characters" heuristic
masks it. Same family as the documented `"snapshot"` trap; diagnostic readability only.

### 🟢 Security — clean
Scanned stdout **and** stderr for: the session token (`AUDITSECRET42`), the fixture absolute path,
a hidden `gene_names` array, and a `raw_path` field planted in the fixture snapshot. **None**
appeared. `session_token` is published as `"<redacted>"`; the snapshot projection keeps only
`has_data` / `object_class` / `n_genes` / `n_samples`; `arm_file` is a **basename**, never a path.

## 6. Integrity — no file modified

| Control | Before | After |
|---|---|---|
| Frozen manifest | `bccc980bd46c0bce…` | **`bccc980bd46c0bceda8534c91bac467500a2c1bfabed6d94472d6d4855e16fc1`** |
| Files | 437 | 437 |
| Delta | — | **0 lines** |
| `tools/_drive/` | `README.md` only | `README.md` only |
| `.zcode/config.json` | `4746a850…` | `4746a850…` |
| `~/.workbuddy-ai/mcp.json` | `0febf769…` | `0febf769…` |
| `git status` / `HEAD` | ` M renv.lock` · `9cb1a06` | ` M renv.lock` · `9cb1a06` |

Artifacts written **only** under `.workbuddy-ai/audits/mcp_client_audit/` (excluded from the
manifest) and the out-of-repo temp fixture.

## 7. Conclusion

The **launch vectors of both configured clients are correct** and the server is internally
consistent, leak-free, and structured on errors. **But the server does not implement the MCP stdio
framing**, so it is **not interoperable with the reference MCP client**. `F-1` should be treated as
the blocker for any claim that "the MCP server is compatible"; `F-2` and `F-3` are minor.

**No remediation was performed — this was a read-only audit.** M3 and M4 were **not started**.

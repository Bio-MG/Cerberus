# =============================================================================
# tools/launch_dev_drive.R — launch the app with the Live Control Protocol ON
# =============================================================================
# The ONLY supported way to start a drive-enabled session (spec §4, G4 #16).
#
# WHY A HELPER AND NOT A `.Renviron` LINE
#   `TRANSCRIPTO_DEV_DRIVE=1` lets `arm.json` carry the wildcard token `"*"`,
#   which means "arm whichever session is listening". That is exactly what you
#   want from a launcher you just started yourself, and exactly what you must
#   NOT have in a committed environment file: it would let anyone who can write
#   a file into `tools/_drive/` drive the app. So the flag is set for THIS
#   process only and dies with it. Spec §1 is explicit: never in `.Renviron`.
#
# USAGE
#   Rscript tools/launch_dev_drive.R                # Mode 1 "headless" (default)
#   Rscript tools/launch_dev_drive.R --visible      # Mode 2 "visible"
#   Rscript tools/launch_dev_drive.R --print        # sanity check, do not run
#
# TWO MODES, ONE PROTOCOL (spec §4)
#   Mode 1 `headless` (the default) — `launch.browser = FALSE`, no window. This
#     is the automated mode: a real httpuv server plus a real chromote client.
#     `ready.json` appears only once a client has CONNECTED, because the
#     handshake is written from `server()` and Shiny creates a session only when
#     a client does.
#   Mode 2 `visible` — `launch.browser = TRUE`: the RStudio Viewer, or a visible
#     localhost Chrome/Edge tab.
#
#   Nothing else differs. The same `ready.json`, `arm.json`, `scenario.json` and
#   `result.json`; the same session-selection and heartbeat rules; the same
#   passive badge. There is deliberately NO visible-mode protocol. The mode is
#   PUBLISHED in `ready.json`'s `viewer` field, so an agent never has to guess
#   whether a human is watching.
#
#   ⚠️ Opening a NEW tab creates a NEW session, which REWRITES `ready.json` with
#   a NEW token — the last connected session wins. An agent must therefore
#   re-read `ready.json` AFTER the tab is open, and must NEVER reuse a token it
#   read earlier: arming with a stale token is refused, and the refusal is
#   indistinguishable from "nobody armed me".
#
# WHAT IT DOES NOT DO
#   It does not attach to anything, does not write into `tools/_drive/`, and
#   does not modify `app.R`. It sets two environment variables and calls
#   `shiny::runApp()`. All protocol logic lives in `R/core/drive_watcher.R`.
#
# NOTE FOR THE RSTUDIO WORKFLOW
#   RStudio's "Run App" button does NOT go through this script, so the wildcard
#   token is unavailable there — you must arm with the REAL token printed on the
#   console (`session_token=...`). That is the safe default, and it is why the
#   console line exists at all.
#
#   Run App is nevertheless a supported Mode 2 ("visible") session: nobody
#   declared the mode, so `ts_drive_viewer()` derives it from `interactive()`,
#   which is TRUE there, and ready.json still reports `viewer = "visible"`.
#   Declared beats derived; derived beats guessing; `unknown` is the honest
#   fallback when neither applies.
# =============================================================================

Sys.unsetenv("LC_ALL")
Sys.setlocale("LC_CTYPE", "fr_FR.UTF-8")

# Liveness knobs, published in ready.json for the agent to read rather than
# hardcode (see ts_drive_hb_interval / ts_drive_hb_timeout).
Sys.setenv(
  TRANSCRIPTO_DEV_DRIVE          = "1",
  TRANSCRIPTO_DEV_DRIVE_HB_TIMEOUT = Sys.getenv("TRANSCRIPTO_DEV_DRIVE_HB_TIMEOUT", "15"),
  TRANSCRIPTO_DEV_DRIVE_HB_INTERVAL = Sys.getenv("TRANSCRIPTO_DEV_DRIVE_HB_INTERVAL", "3")
)

# ── THE ARM GATE MUST BE OPEN, or this launcher produces an INERT app ───────
# `ts_drive_interactive()` is `isTRUE(interactive())` unless overridden, and
# `Rscript` is NEVER interactive. Without the line below the poller's observer
# returns on its FIRST statement, so `invalidateLater()` is never called and the
# arm gate answers "non-interactive session" to every arm request. The app looks
# perfectly healthy while it happens: it listens, it serves, `ready.json` is
# written ONCE at attach and then never updated, and NOTHING in the log says
# why. An agent sees a handshake that is permanently stale and has no way to
# tell it from a dead session.
#
# MEASURED before the fix (2026-09-23): the app listened on 127.0.0.1:7067 and
# logged no `session_token=`, because the handshake is written from `server()`
# and a Shiny session exists only after a client CONNECTS. Nothing outside
# `tests/` ever set this option, so every earlier live grade must have been run
# from the RStudio Viewer (where `interactive()` is TRUE) — which is exactly
# what "a Stop + Run App is required" was describing.
#
# It is decided HERE, and not in `app.R`, for the same reason as
# `TRANSCRIPTO_DEV_DRIVE=1` above: the gate exists to keep PRODUCTION inert, and
# production never runs this script.
options(ts.drive.interactive = TRUE)

args <- commandArgs(trailingOnly = TRUE)

# ── Visibility mode (spec §4) — the ONLY axis the two modes differ on ────────
# `headless` is the default, which is what this script has always effectively
# done under Rscript (`runApp()`'s default is `interactive()`, FALSE there), so
# existing automated callers keep the exact behaviour they already had.
unknown_args <- setdiff(args, c("--headless", "--visible", "--print"))
if (length(unknown_args) > 0L) {
  stop(sprintf("[launch_dev_drive] unknown flag(s): %s (expected --headless, --visible or --print)",
               paste(unknown_args, collapse = " ")), call. = FALSE)
}
mode_flags <- intersect(args, c("--headless", "--visible"))
if (length(mode_flags) > 1L) {
  stop("[launch_dev_drive] --headless and --visible are mutually exclusive.",
       call. = FALSE)
}
viewer <- if (length(mode_flags) == 1L) sub("^--", "", mode_flags) else "headless"
launch_browser <- identical(viewer, "visible")

# DECLARED, not guessed: `ts_drive_viewer()` reads this and publishes it in
# ready.json's `viewer` field, so the agent can tell whether a human is watching.
Sys.setenv(TRANSCRIPTO_DEV_DRIVE_VIEWER = viewer)

message("[launch_dev_drive] TRANSCRIPTO_DEV_DRIVE=1 (wildcard arm token accepted)")
message("[launch_dev_drive] visibility mode: ", viewer,
        " (launch.browser=", launch_browser, ")")
message("[launch_dev_drive] heartbeat interval=",
        Sys.getenv("TRANSCRIPTO_DEV_DRIVE_HB_INTERVAL"), "s timeout=",
        Sys.getenv("TRANSCRIPTO_DEV_DRIVE_HB_TIMEOUT"), "s")
if (launch_browser) {
  message("[launch_dev_drive] a visible tab/Viewer will open. Read the FRESH")
  message("[launch_dev_drive] tools/_drive/ready.json AFTER it has connected: a new")
  message("[launch_dev_drive] session rewrites the token, so never reuse an older one.")
} else {
  message("[launch_dev_drive] no window. Connect a real client (chromote), then read")
  message("[launch_dev_drive] tools/_drive/ready.json for pid/port/session_token.")
}

if ("--print" %in% args) {
  message("[launch_dev_drive] --print given: environment prepared, app NOT started.")
  quit(save = "no", status = 0)
}

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("shiny is not installed in this renv library; run from the project root.",
       call. = FALSE)
}

# `appDir` is the project root, so runApp() finds app.R regardless of the
# caller's working directory. This script is standalone (it must run BEFORE any
# project file is sourced), so it cannot rely on `%||%` from global.R.
.script_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")),
                        error = function(e) getwd())
root <- normalizePath(file.path(.script_dir, ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(root, "app.R"))) {
  root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}
message("[launch_dev_drive] app root: ", root)

# `launch.browser` is passed EXPLICITLY rather than left to `runApp()`'s default
# (`interactive()`): the mode is a decision this launcher made, and a decision
# that is only implied is a decision that drifts.
shiny::runApp(appDir = root, launch.browser = launch_browser)

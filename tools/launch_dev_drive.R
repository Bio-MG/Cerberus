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
#   Rscript tools/launch_dev_drive.R            # sets the flag, then runs app
#   Rscript tools/launch_dev_drive.R --print    # sanity check, do not run
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

args <- commandArgs(trailingOnly = TRUE)

message("[launch_dev_drive] TRANSCRIPTO_DEV_DRIVE=1 (wildcard arm token accepted)")
message("[launch_dev_drive] heartbeat interval=",
        Sys.getenv("TRANSCRIPTO_DEV_DRIVE_HB_INTERVAL"), "s timeout=",
        Sys.getenv("TRANSCRIPTO_DEV_DRIVE_HB_TIMEOUT"), "s")
message("[launch_dev_drive] once the app is up, open the Viewer or the localhost tab;")
message("[launch_dev_drive] then read tools/_drive/ready.json for pid/port/session_token.")

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

shiny::runApp(appDir = root)

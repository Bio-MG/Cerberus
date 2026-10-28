# Manual verification of .ensure_10x_features() against the real failing shapes:
#   1. genes.tsv 1-col + stale 3-col features.tsv.gz   (CID4461 current state)
#   2. genes.tsv 1-col + plain 1-col features.tsv      (CID4463/4471 shape)
#   3. genes.tsv 2-col                                  (pbmc3k regression — must be untouched)
# Each case is exercised on a throwaway copy in tempdir(); real data is never modified.
source <- function(...) NULL  # placeholder in case of future edits
# Root: TRANSCRIPTO_APP_ROOT when the operator sets it, else derived from
# --file= (recipe of tools/run_all_gates.R:123-129). This script lives in
# tests/manual/, so two levels up is the project root.
root <- Sys.getenv("TRANSCRIPTO_APP_ROOT", unset = "")
if (!nzchar(root)) {
  a <- commandArgs(trailingOnly = FALSE)
  h <- grep("^--file=", a, value = TRUE)
  b <- if (length(h)) dirname(normalizePath(sub("^--file=", "", h[1]),
                                            mustWork = FALSE)) else getwd()
  root <- normalizePath(file.path(b, "..", ".."), mustWork = FALSE)
}
file <- file.path(root, "modules", "import", "mod_import_sc.R")
# Source only the helper: parse the file and eval the function definition
p <- parse(file)
fn_def <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
                   identical(deparse(x[[2]]), ".ensure_10x_features"), p)
stopifnot(length(fn_def) == 1)
eval(fn_def[[1]], envir = globalenv())

log_msgs <- character(0)
log_fn <- function(m) { log_msgs <<- c(log_msgs, m); cat("   LOG:", m, "\n") }

copy_dir <- function(from) {
  to <- file.path(tempdir(), paste0("t10x_", basename(from), "_",
                                    as.integer(Sys.time()), "_", sample.int(1e9, 1)))
  stopifnot(dir.create(to, recursive = TRUE))
  for (f in list.files(from, full.names = TRUE)) file.copy(f, to, overwrite = TRUE)
  to
}

read_cols <- function(path) {
  if (grepl("\\.gz$", path)) {
    con <- gzfile(path, "rt"); on.exit(close(con)); lines <- readLines(con)
  } else lines <- readLines(path)
  ncol(read.table(text = lines[1:min(5, length(lines))], sep = "\t", header = FALSE,
                  quote = "", nrows = 1))
}

# Real data lives OUTSIDE the repo: UPSTREAM/ is a sibling of the project root.
base <- Sys.getenv("TRANSCRIPTO_UPSTREAM_OUT", file.path(dirname(root), "UPSTREAM", "outputs"))

# Case 1: CID4461 (genes.tsv 1 col; app previously added features.tsv.gz)
d1 <- copy_dir(file.path(base, "run-breast-wu2021-triplet-real/scrna/CID4461/filtered"))
cat("CASE 1 before: genes.tsv exists =", file.exists(file.path(d1, "genes.tsv")),
    "; features.tsv.gz cols =", read_cols(file.path(d1, "features.tsv.gz")), "\n")
invisible(.ensure_10x_features(d1, log_fn))
cat("CASE 1 after : genes.tsv exists =", file.exists(file.path(d1, "genes.tsv")),
    "; genes.tsv.bak exists =", file.exists(file.path(d1, "genes.tsv.bak")),
    "; features.tsv.gz cols =", read_cols(file.path(d1, "features.tsv.gz")), "\n")
stopifnot(!file.exists(file.path(d1, "genes.tsv")),
          file.exists(file.path(d1, "genes.tsv.bak")),
          read_cols(file.path(d1, "features.tsv.gz")) == 3)

# Case 2: CID4463 (genes.tsv 1 col + plain 1-col features.tsv)
d2 <- copy_dir(file.path(base, "run-breast-wu2021-triplet-real/scrna/CID4463/filtered"))
cat("CASE 2 before: genes.tsv cols =", read_cols(file.path(d2, "genes.tsv")),
    "; features.tsv cols =", read_cols(file.path(d2, "features.tsv")),
    "; features.tsv.gz exists =", file.exists(file.path(d2, "features.tsv.gz")), "\n")
invisible(.ensure_10x_features(d2, log_fn))
cat("CASE 2 after : genes.tsv exists =", file.exists(file.path(d2, "genes.tsv")),
    "; features.tsv.gz cols =", read_cols(file.path(d2, "features.tsv.gz")), "\n")
stopifnot(!file.exists(file.path(d2, "genes.tsv")),
          read_cols(file.path(d2, "features.tsv.gz")) == 3)

# Case 3: pbmc3k regression (2-col genes.tsv) — helper must be a no-op
d3 <- copy_dir(file.path(base, "run-pbmc3k-real/scrna/PBMC_3k/filtered"))
before <- file.info(list.files(d3, full.names = TRUE))$mtime
log_msgs_before <- length(log_msgs)
invisible(.ensure_10x_features(d3, log_fn))
after <- file.info(list.files(d3, full.names = TRUE))$mtime
cat("CASE 3: files changed =", any(names(before) %in% names(after) &
      (before[names(before) %in% names(after)] != after[names(before) %in% names(after)])),
    "; new log lines =", length(log_msgs) - log_msgs_before, "\n")
stopifnot(identical(sort(before), sort(after)),
          !file.exists(file.path(d3, "genes.tsv.bak")))

cat("\nALL CASES PASSED\n")

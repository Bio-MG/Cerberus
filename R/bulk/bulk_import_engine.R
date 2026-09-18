# =============================================================================
# R/bulk/bulk_import_engine.R — Bulk import parsing & validation engine
# (extracted from mod_import_bulk.R, Block 9 refactor)
# =============================================================================
# Pure functions: no Shiny reactivity. Called by mod_import_bulk_server().
# Depends on: readr, dplyr, helpers_io.R (%||%)
# =============================================================================

.guess_delim_per_sample <- function(filename) {
  x <- tolower(sub("\\.gz$", "", basename(filename)))
  if (grepl("\\.csv$", x)) "," else "\t"
}

.read_per_sample_file <- function(path, filename) {
  src <- if (grepl("\\.gz$", tolower(filename))) gzfile(path, open = "rt") else path
  on.exit({
    if (inherits(src, "connection")) try(close(src), silent = TRUE)
  }, add = TRUE)
  
  df <- tryCatch(
    readr::read_delim(
      file = src,
      delim = .guess_delim_per_sample(filename),
      trim_ws = TRUE,
      show_col_types = FALSE,
      guess_max = 5000,
      progress = FALSE
    ),
    error = function(e) stop(errorCondition(sprintf("Lecture %s : %s", filename, e$message), class = "bulk_import_engine_error"))
  )
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  names(df) <- trimws(names(df))
  df
}

.numeric_frac <- function(x) {
  y <- suppressWarnings(as.numeric(trimws(as.character(x))))
  mean(!is.na(y))
}

.infer_sample_id <- function(filename) {
  x <- sub("\\.gz$", "", basename(filename))
  x <- sub("\\.[^.]+$", "", x)
  trimws(gsub("[^A-Za-z0-9._-]+", "_", x))
}

.infer_condition <- function(filename) {
  x <- .infer_sample_id(filename)
  parts <- unlist(strsplit(x, "[._-]+", perl = TRUE))
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return(NA_character_)
  bad <- c("^s[0-9]+$", "^sample[0-9]*$", "^rep[0-9]+$",
           "^r[0-9]+$", "^lane[0-9]+$", "^count[s]?$")
  keep <- parts[!grepl(paste(bad, collapse = "|"), tolower(parts))]
  if (length(keep)) keep[[1]] else NA_character_
}

.detect_columns_per_sample <- function(df) {
  nms <- names(df)
  if (!length(nms)) {
    return(list(gene_candidates = character(0), count_candidates = character(0),
                auto_gene = NA_character_, auto_count = NA_character_))
  }
  
  num_frac   <- vapply(df, .numeric_frac, numeric(1))
  lower_nms  <- tolower(nms)
  
  gene_hits  <- grepl("gene|feature|symbol|ensembl|entrez|geneid|featureid|id$",
                      lower_nms)
  count_hits <- grepl("count|counts|raw|readcount|read_count|numreads|expected",
                      lower_nms)
  
  gene_cands  <- nms[gene_hits]
  if (!length(gene_cands)) gene_cands <- nms[num_frac < 0.5]
  auto_gene   <- gene_cands[1] %||% NA_character_
  
  count_cands <- setdiff(nms[count_hits & num_frac > 0.8], auto_gene)
  if (!length(count_cands)) count_cands <- setdiff(nms[num_frac > 0.8], auto_gene)
  auto_count  <- count_cands[1] %||% NA_character_
  
  list(gene_candidates  = gene_cands,  count_candidates = count_cands,
       auto_gene = auto_gene, auto_count = auto_count)
}

.prepare_one_sample <- function(df, sample_id, gene_col, count_col,
                                dup_threshold = 0.05) {
  fail <- function(msg, id_type = "mixed/unknown")
    list(ok = FALSE, status = msg, data = NULL, id_type = id_type,
         integer_like = NA)
  
  if (!gene_col  %in% names(df)) return(fail("Colonne gene_id introuvable"))
  if (!count_col %in% names(df)) return(fail("Colonne count introuvable"))
  
  feat <- trimws(as.character(df[[gene_col]]))
  cnt  <- suppressWarnings(as.numeric(trimws(as.character(df[[count_col]]))))
  
  if (any(is.na(feat) | !nzchar(feat)))
    return(fail("Gene IDs manquants"))
  if (all(is.na(cnt)))
    return(fail("La colonne count n'est pas numérique"))
  
  out <- data.frame(feature_id = feat, count = cnt,
                    stringsAsFactors = FALSE, check.names = FALSE)
  
  dup_frac <- mean(duplicated(out$feature_id))
  if (dup_frac > dup_threshold)
    return(fail(sprintf("Gene IDs dupliqués : %.1f%% > seuil %.1f%%",
                        100 * dup_frac, 100 * dup_threshold),
                id_type = detect_gene_id_type(feat)))
  
  collapsed <- FALSE
  if (anyDuplicated(out$feature_id)) {
    collapsed <- TRUE
    out <- out |>
      dplyr::group_by(feature_id) |>
      dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop")
  }
  
  int_like <- all(is.na(out$count) | abs(out$count - round(out$count)) < 1e-8)
  names(out)[2] <- sample_id
  
  list(
    ok          = TRUE,
    status      = if (collapsed) "OK (doublons fusionnés)" else "OK",
    data        = out,
    id_type     = detect_gene_id_type(out$feature_id),
    integer_like = int_like
  )
}

.merge_per_sample_tables <- function(tbls) {
  Reduce(function(a, b) dplyr::full_join(a, b, by = "feature_id"), tbls)
}

.validate_design <- function(counts_mat, metadata) {
  msgs <- character(0); ok <- TRUE
  
  if (anyDuplicated(metadata$sample_id)) {
    ok <- FALSE
    msgs <- c(msgs, "sample_id dupliqués dans la métadata.")
  }
  if (!setequal(colnames(counts_mat), metadata$sample_id)) {
    ok <- FALSE
    msgs <- c(msgs, "Colonnes counts ≠ sample_id de la métadata.")
  } else {
    metadata <- metadata[match(colnames(counts_mat), metadata$sample_id), , drop = FALSE]
  }
  
  cond <- as.character(metadata$condition)
  cond[is.na(cond) | !nzchar(trimws(cond))] <- NA_character_
  tab <- table(cond, useNA = "no")
  
  de_ok <- FALSE
  if (length(tab) < 2) {
    msgs <- c(msgs, "1 seule condition : QC/PCA actifs, DE/pathway désactivés.")
  } else if (any(tab < 2)) {
    msgs <- c(msgs, "Au moins un groupe a < 2 réplicats : DE/pathway désactivés.")
  } else if (ok) {
    de_ok <- TRUE
  }
  
  list(ok = ok, de_ok = de_ok, messages = unique(msgs), metadata = metadata)
}

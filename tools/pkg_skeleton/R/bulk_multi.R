# tools/pkg_skeleton/R/bulk_multi.R — MD-1 : conteneur `bulk_datasets` & jeux bulk nommés
# =============================================================================
# Contrat gelé : docs/contracts/BULK_MULTI_CONTRACT.md
# Test de gel  : tests/testthat/test-bulk-multi-contract-freeze.R
#
# Pure domain logic (no Shiny symbols — guarded by the freeze test). Every
# function returns a value; register()/remove() return the NEW named list
# invisibly and never mutate a global — callers assign it to
# `global_data$bulk_datasets` themselves.
#
# Consumed by (MD-1 wiring):
#   - modules/import/mod_import_bulk.R  (producer = "import", optional label)
#   - modules/bulk/mod_bulk_datasets.R  (producer = "pipeline_save", manage)
# Reserved for MD-3: producer = "pseudobulk".
# =============================================================================

#' Public API surface (frozen by the contract freeze test)
#' @return Character vector of exported function names.
bulk_multi_public_api <- function() {
  c(
    "bulk_multi_public_api", "bulk_multi_error_states",
    "bulk_multi_pipeline_fields", "bulk_multi_check_label",
    "bulk_multi_check_obj", "bulk_multi_capture_pipeline",
    "bulk_multi_register", "bulk_multi_remove", "bulk_multi_get",
    "bulk_multi_summary"
  )
}

#' Frozen error states (contract §5 — MD-1 storage + MD-2 comparison)
#' @return Character vector of `state` attribute values.
bulk_multi_error_states <- function() {
  c("invalid_input", "invalid_label", "invalid_obj", "invalid_pipeline",
    "duplicate_label", "unknown_label", "capacity_exceeded",
    "insufficient_datasets", "no_common_contrast", "no_significant_genes")
}

#' Frozen pipeline fields captured in a registered dataset (contract §4.3)
#' @return Character vector of `shared_rv` field names.
bulk_multi_pipeline_fields <- function() {
  c(
    "mapping_applied", "mapping_summary", "filtered_counts", "vst_mat",
    "contrasts", "active_contrast", "multimethod_de",
    "lfc_thresh", "padj_thresh", "pathway_results", "pathway_db",
    "pathway_mode"
  )
}

#' Classed error constructor (bulk_multi_error, contract §5)
#' @param msg French error message.
#' @param state One of bulk_multi_error_states().
#' @return An errorCondition with class `bulk_multi_error`.
.bulk_multi_stop <- function(msg, state) {
  errorCondition(msg, class = "bulk_multi_error", state = state)
}

#' Validate and trim a dataset label (contract §3)
#' @param label Candidate label.
#' @return The trimmed label (invisible).
bulk_multi_check_label <- function(label) {
  if (!is.character(label) || length(label) != 1L) {
    stop(.bulk_multi_stop(
      "Label de dataset invalide : attendu une chaîne de caractères unique.",
      state = "invalid_label"))
  }
  lbl <- trimws(label)
  if (!nzchar(lbl)) {
    stop(.bulk_multi_stop(
      "Label de dataset vide — renseignez un nom pour enregistrer le jeu.",
      state = "invalid_label"))
  }
  if (nchar(lbl) > 80L) {
    stop(.bulk_multi_stop(
      sprintf("Label de dataset trop long (%d caractères) — 80 caractères maximum : « %s… ».",
              nchar(lbl), substr(lbl, 1L, 40L)),
      state = "invalid_label"))
  }
  if (grepl("[[:cntrl:]]", lbl)) {
    stop(.bulk_multi_stop(
      "Label de dataset invalide : caractères de contrôle (saut de ligne, tabulation…) interdits.",
      state = "invalid_label"))
  }
  invisible(lbl)
}

#' Validate a bulk_obj-shaped object (contract §3)
#' @param obj Candidate bulk_obj list.
#' @return TRUE invisibly; classed error otherwise.
bulk_multi_check_obj <- function(obj) {
  if (!is.list(obj)) {
    stop(.bulk_multi_stop(
      "Jeu bulk invalide : liste attendue (structure bulk_obj).",
      state = "invalid_obj"))
  }
  counts <- obj$counts
  if (is.null(counts) || !is.matrix(counts) ||
      is.null(dimnames(counts)) || is.null(colnames(counts))) {
    stop(.bulk_multi_stop(
      "Jeu bulk invalide : matrice de counts (avec noms de gènes et d'échantillons) requise.",
      state = "invalid_obj"))
  }
  meta <- obj$metadata
  if (!is.null(meta) && !is.data.frame(meta)) {
    stop(.bulk_multi_stop(
      "Jeu bulk invalide : les métadonnées doivent être un data.frame.",
      state = "invalid_obj"))
  }
  if (!is.null(meta) && nrow(meta) != ncol(counts)) {
    stop(.bulk_multi_stop(
      sprintf("Jeu bulk incohérent : %d échantillons dans les counts mais %d lignes de métadonnées.",
              ncol(counts), nrow(meta)),
      state = "invalid_obj"))
  }
  invisible(TRUE)
}

#' Capture the frozen pipeline fields from shared_rv (or a plain named list)
#' @param state A reactivevalues object (environment subclass) or a named list.
#' @return Plain named list with exactly bulk_multi_pipeline_fields().
bulk_multi_capture_pipeline <- function(state) {
  if (!is.list(state) && !is.environment(state)) {
    stop(.bulk_multi_stop(
      "Capture du pipeline impossible : reactiveValues ou liste nommée attendue.",
      state = "invalid_input"))
  }
  fields <- bulk_multi_pipeline_fields()
  out <- vector("list", length(fields))
  names(out) <- fields
  # out[i] <- list(NULL) CONSERVE le nom pour un champ absent/NULL —
  # out[[f]] <- NULL retirerait l'élément d'une liste nommée.
  for (i in seq_along(fields)) out[i] <- list(state[[fields[i]]])
  out
}

#' Validate a captured pipeline snapshot (contract §3 — exact field set)
#' @param pipeline_state NULL or a named list.
#' @return TRUE invisibly.
.bulk_multi_check_pipeline <- function(pipeline_state) {
  if (is.null(pipeline_state)) return(invisible(TRUE))
  if (!is.list(pipeline_state) ||
      !setequal(names(pipeline_state), bulk_multi_pipeline_fields())) {
    stop(.bulk_multi_stop(
      "État de pipeline invalide : les champs capturés doivent correspondre exactement à bulk_multi_pipeline_fields().",
      state = "invalid_pipeline"))
  }
  invisible(TRUE)
}

#' Validate the container argument (NULL or named list)
#' @param datasets Container.
#' @return TRUE invisibly.
.bulk_multi_check_datasets <- function(datasets) {
  if (is.null(datasets)) return(invisible(TRUE))
  if (!is.list(datasets) ||
      (length(datasets) > 0L && (is.null(names(datasets)) ||
        any(!nzchar(names(datasets)))))) {
    stop(.bulk_multi_stop(
      "Conteneur bulk_datasets invalide : liste nommée attendue.",
      state = "invalid_input"))
  }
  invisible(TRUE)
}

#' Resolve the container capacity (config threshold with fallback)
#' @param max_datasets NULL or a positive integer.
#' @return Integer capacity.
.bulk_multi_resolve_capacity <- function(max_datasets) {
  if (is.null(max_datasets)) {
    max_datasets <- if (exists("TS_BULK_MULTI_MAX_DATASETS", inherits = TRUE)) {
      TS_BULK_MULTI_MAX_DATASETS
    } else {
      20L
    }
  }
  if (!is.numeric(max_datasets) || length(max_datasets) != 1L ||
      is.na(max_datasets) || max_datasets < 1L) {
    stop(.bulk_multi_stop(
      "Capacité du conteneur bulk_datasets invalide : entier >= 1 attendu (TS_BULK_MULTI_MAX_DATASETS).",
      state = "invalid_input"))
  }
  as.integer(max_datasets)
}

#' Validate the producer tag (contract §2.8)
#' @param producer Producer tag.
#' @return TRUE invisibly.
.bulk_multi_check_producer <- function(producer) {
  if (!is.character(producer) || length(producer) != 1L ||
      !producer %in% c("import", "pipeline_save", "pseudobulk")) {
    stop(.bulk_multi_stop(
      sprintf("Producteur invalide : « %s » — attendu import, pipeline_save ou pseudobulk.",
              paste(producer, collapse = ",")),
      state = "invalid_input"))
  }
  invisible(TRUE)
}

#' Register (or update) a named bulk dataset (pure — returns the new list)
#' @param datasets Current container (NULL or named list).
#' @param label Dataset label (validated via bulk_multi_check_label()).
#' @param obj bulk_obj-shaped list to snapshot (read-only, copied).
#' @param pipeline_state NULL or bulk_multi_capture_pipeline() output.
#' @param producer One of "import", "pipeline_save", "pseudobulk".
#' @param overwrite Allow replacing an existing label (registered_at kept).
#' @param max_datasets Container capacity (default from TS_BULK_MULTI_MAX_DATASETS).
#' @return The new named list (invisible).
bulk_multi_register <- function(datasets, label, obj, pipeline_state = NULL,
                                producer = "pipeline_save",
                                overwrite = FALSE, max_datasets = NULL) {
  .bulk_multi_check_datasets(datasets)
  .bulk_multi_check_producer(producer)
  lbl <- bulk_multi_check_label(label)
  bulk_multi_check_obj(obj)
  .bulk_multi_check_pipeline(pipeline_state)
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop(.bulk_multi_stop("overwrite doit être TRUE ou FALSE.",
                          state = "invalid_input"))
  }
  capacity <- .bulk_multi_resolve_capacity(max_datasets)

  exists_lbl <- lbl %in% names(datasets)
  if (!exists_lbl && length(datasets) >= capacity) {
    stop(.bulk_multi_stop(
      sprintf("Plafond du conteneur bulk_datasets atteint (%d jeux) — supprimez un jeu avant d'enregistrer « %s ».",
              capacity, lbl),
      state = "capacity_exceeded"))
  }
  if (exists_lbl && !isTRUE(overwrite)) {
    stop(.bulk_multi_stop(
      sprintf("Un dataset « %s » existe déjà — choisissez un autre label ou autorisez explicitement le remplacement (overwrite = TRUE).",
              lbl),
      state = "duplicate_label"))
  }

  now <- Sys.time()
  prev <- if (exists_lbl) datasets[[lbl]] else NULL
  entry <- list(
    label         = lbl,
    producer      = producer,
    registered_at = if (!is.null(prev)) prev$registered_at else now,
    updated_at    = now,
    obj           = obj,
    pipeline      = pipeline_state
  )
  datasets[[lbl]] <- entry
  invisible(datasets)
}

#' Remove a named dataset (pure — returns the new list)
#' @param datasets Current container.
#' @param label Label to remove.
#' @return The new named list (invisible).
bulk_multi_remove <- function(datasets, label) {
  .bulk_multi_check_datasets(datasets)
  lbl <- bulk_multi_check_label(label)
  if (!lbl %in% names(datasets)) {
    stop(.bulk_multi_stop(
      sprintf("Dataset « %s » introuvable dans le conteneur bulk_datasets.", lbl),
      state = "unknown_label"))
  }
  datasets[[lbl]] <- NULL
  invisible(datasets)
}

#' Get one entry (pure)
#' @param datasets Current container.
#' @param label Label to fetch.
#' @return The entry (invisible).
bulk_multi_get <- function(datasets, label) {
  .bulk_multi_check_datasets(datasets)
  lbl <- bulk_multi_check_label(label)
  if (!lbl %in% names(datasets)) {
    stop(.bulk_multi_stop(
      sprintf("Dataset « %s » introuvable dans le conteneur bulk_datasets.", lbl),
      state = "unknown_label"))
  }
  invisible(datasets[[lbl]])
}

#' Container summary (frozen columns, contract §4.2)
#' @param datasets Current container.
#' @return data.frame (0 rows when empty).
bulk_multi_summary <- function(datasets) {
  .bulk_multi_check_datasets(datasets)
  cols <- c("label", "producer", "n_genes", "n_samples", "has_filtered",
            "n_contrasts", "has_pathways", "import_mode", "registered_at",
            "updated_at")
  if (is.null(datasets) || length(datasets) == 0L) {
    empty <- data.frame(
      label = character(0), producer = character(0),
      n_genes = integer(0), n_samples = integer(0),
      has_filtered = logical(0), n_contrasts = integer(0),
      has_pathways = logical(0), import_mode = character(0),
      registered_at = character(0), updated_at = character(0),
      stringsAsFactors = FALSE
    )
    return(empty)
  }
  rows <- lapply(names(datasets), function(nm) {
    e <- datasets[[nm]]
    obj <- if (is.null(e$obj)) list() else e$obj
    pipe <- e$pipeline
    filtered <- if (!is.null(pipe)) pipe$filtered_counts else NULL
    contrasts <- if (!is.null(pipe)) pipe$contrasts else NULL
    pathways <- if (!is.null(pipe)) pipe$pathway_results else NULL
    producer <- if (is.null(e$producer)) NA_character_ else as.character(e$producer)
    import_mode <- if (is.null(obj$import_mode)) NA_character_ else as.character(obj$import_mode)
    data.frame(
      label         = nm,
      producer      = producer,
      n_genes       = if (!is.null(obj$counts)) nrow(obj$counts) else NA_integer_,
      n_samples     = if (!is.null(obj$counts)) ncol(obj$counts) else NA_integer_,
      has_filtered  = !is.null(filtered),
      n_contrasts   = if (!is.null(contrasts)) length(contrasts) else 0L,
      has_pathways  = !is.null(pathways),
      import_mode   = import_mode,
      registered_at = format(e$registered_at, "%Y-%m-%d %H:%M"),
      updated_at    = format(e$updated_at, "%Y-%m-%d %H:%M"),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

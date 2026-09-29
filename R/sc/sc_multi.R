# R/sc/sc_multi.R — MD-4 : conteneur `sc_datasets` & double jeu SC (modes 1/2)
# =============================================================================
# Contrat gelé : docs/contracts/SC_MULTI_CONTRACT.md
# Test de gel  : tests/testthat/test-sc-multi-contract-freeze.R
#
# Pure domain logic (no Shiny symbols — guarded by the freeze test). Every
# function returns a value; register()/remove() return the NEW named list
# invisibly and never mutate a global — callers assign it to
# `global_data$sc_datasets` themselves.
#
# MIRROR of R/bulk/bulk_multi.R (MD-1) applied to the Single-Cell domain —
# same structure, same error class, but:
#   - obj is a Seurat object (not a bulk_obj list) — heavy: capacity default
#     TS_SC_MULTI_MAX_DATASETS = 20 (RAM 32 GB budget; raised from 5 on
#     2026-09-27 so a 6-replicate design fits — aligned with bulk);
#   - NO pipeline capture: the SC pipeline state LIVES INSIDE the Seurat
#     object itself (assays, reductions, clusters). Instead, each entry
#     carries a DECLARED RELATION (décision 5, ROADMAP.md §4):
#       "standalone"      — jeu indépendant (aucune relation déclarée) ;
#       "shared_params"   — mode 1 : analyses séparées, paramètres partagés
#                           (traiter avec les MÊMES réglages que le jeu de
#                           référence, pour que les différences observées
#                           soient biologiques et non méthodologiques) ;
#       "distinct_params" — mode 2 : analyses séparées, paramètres distincts
#                           (seuils propres au jeu : profondeur, tissu…).
#     The relation is a DECLARED ANNOTATION recorded for provenance — it is
#     never enforced mechanically (the pipeline parameters are module inputs,
#     not captured state).
#
# Zero behavior change: the existing SC pipeline keeps reading
# `global_data$sc_obj` and never references `sc_datasets`.
# Consumed by (MD-4 wiring):
#   - modules/import/mod_import_sc.R  (producer = "import", optional label)
#   - modules/sc/mod_sc_datasets.R    (producer = "pipeline_save", manage)
# =============================================================================

#' Public API surface (frozen by the contract freeze test)
#' @return Character vector of exported function names.
sc_multi_public_api <- function() {
  c(
    "sc_multi_public_api", "sc_multi_error_states", "sc_multi_relations",
    "sc_multi_check_label", "sc_multi_check_obj", "sc_multi_check_relation",
    "sc_multi_register", "sc_multi_remove", "sc_multi_get",
    "sc_multi_summary"
  )
}

#' Frozen error states (contract §5)
#' @return Character vector of `state` attribute values.
sc_multi_error_states <- function() {
  c("invalid_input", "invalid_label", "invalid_obj", "invalid_relation",
    "duplicate_label", "unknown_label", "capacity_exceeded")
}

#' Frozen declared-relation values (décision 5 — modes 1/2)
#' @return Character vector of relation values.
sc_multi_relations <- function() {
  c("standalone", "shared_params", "distinct_params")
}

#' Classed error constructor (sc_multi_error, contract §5)
#' @param msg French error message.
#' @param state One of sc_multi_error_states().
#' @return An errorCondition with class `sc_multi_error`.
.sc_multi_stop <- function(msg, state) {
  errorCondition(msg, class = "sc_multi_error", state = state)
}

#' Validate and trim a dataset label (contract §3)
#' @param label Candidate label.
#' @return The trimmed label (invisible).
sc_multi_check_label <- function(label) {
  if (!is.character(label) || length(label) != 1L) {
    stop(.sc_multi_stop(
      "Label de dataset SC invalide : attendu une chaîne de caractères unique.",
      state = "invalid_label"))
  }
  lbl <- trimws(label)
  if (!nzchar(lbl)) {
    stop(.sc_multi_stop(
      "Label de dataset SC vide — renseignez un nom pour enregistrer le jeu.",
      state = "invalid_label"))
  }
  if (nchar(lbl) > 80L) {
    stop(.sc_multi_stop(
      sprintf("Label de dataset SC trop long (%d caractères) — 80 caractères maximum : « %s… ».",
              nchar(lbl), substr(lbl, 1L, 40L)),
      state = "invalid_label"))
  }
  if (grepl("[[:cntrl:]]", lbl)) {
    stop(.sc_multi_stop(
      "Label de dataset SC invalide : caractères de contrôle (saut de ligne, tabulation…) interdits.",
      state = "invalid_label"))
  }
  invisible(lbl)
}

#' Validate a Seurat object (contract §3)
#' @param obj Candidate object.
#' @return TRUE invisibly; classed error otherwise.
sc_multi_check_obj <- function(obj) {
  if (!inherits(obj, "Seurat")) {
    stop(.sc_multi_stop(
      "Jeu SC invalide : un objet Seurat est requis.",
      state = "invalid_obj"))
  }
  n_cells <- tryCatch(length(SeuratObject::Cells(obj)), error = function(e) NULL)
  if (is.null(n_cells) || !is.numeric(n_cells) || n_cells < 1L) {
    stop(.sc_multi_stop(
      "Jeu SC invalide : l'objet Seurat ne contient aucune cellule.",
      state = "invalid_obj"))
  }
  invisible(TRUE)
}

#' Validate a declared relation (contract §3)
#' @param relation Candidate relation value.
#' @return TRUE invisibly.
sc_multi_check_relation <- function(relation) {
  if (!is.character(relation) || length(relation) != 1L ||
      !relation %in% sc_multi_relations()) {
    stop(.sc_multi_stop(
      sprintf("Relation déclarée invalide : « %s » — attendu standalone, shared_params (mode 1) ou distinct_params (mode 2).",
              paste(relation, collapse = ",")),
      state = "invalid_relation"))
  }
  invisible(TRUE)
}

#' Validate the container argument (NULL or named list)
#' @param datasets Container.
#' @return TRUE invisibly.
.sc_multi_check_datasets <- function(datasets) {
  if (is.null(datasets)) return(invisible(TRUE))
  if (!is.list(datasets) ||
      (length(datasets) > 0L && (is.null(names(datasets)) ||
        any(!nzchar(names(datasets)))))) {
    stop(.sc_multi_stop(
      "Conteneur sc_datasets invalide : liste nommée attendue.",
      state = "invalid_input"))
  }
  invisible(TRUE)
}

#' Resolve the container capacity (config threshold with fallback)
#' @param max_datasets NULL or a positive integer.
#' @return Integer capacity.
.sc_multi_resolve_capacity <- function(max_datasets) {
  if (is.null(max_datasets)) {
    max_datasets <- if (exists("TS_SC_MULTI_MAX_DATASETS", inherits = TRUE)) {
      TS_SC_MULTI_MAX_DATASETS
    } else {
      20L
    }
  }
  if (!is.numeric(max_datasets) || length(max_datasets) != 1L ||
      is.na(max_datasets) || max_datasets < 1L) {
    stop(.sc_multi_stop(
      "Capacité du conteneur sc_datasets invalide : entier >= 1 attendu (TS_SC_MULTI_MAX_DATASETS).",
      state = "invalid_input"))
  }
  as.integer(max_datasets)
}

#' Validate the producer tag (contract §2.7)
#' @param producer Producer tag.
#' @return TRUE invisibly.
.sc_multi_check_producer <- function(producer) {
  if (!is.character(producer) || length(producer) != 1L ||
      !producer %in% c("import", "pipeline_save")) {
    stop(.sc_multi_stop(
      sprintf("Producteur invalide : « %s » — attendu import ou pipeline_save.",
              paste(producer, collapse = ",")),
      state = "invalid_input"))
  }
  invisible(TRUE)
}

#' Register (or update) a named SC dataset (pure — returns the new list)
#' @param datasets Current container (NULL or named list).
#' @param label Dataset label (validated via sc_multi_check_label()).
#' @param obj Seurat object to snapshot (stored as-is — R semantics).
#' @param relation Declared relation (décision 5): "standalone",
#'   "shared_params" (mode 1) or "distinct_params" (mode 2).
#' @param producer One of "import", "pipeline_save".
#' @param overwrite Allow replacing an existing label (registered_at kept).
#' @param max_datasets Container capacity (default from TS_SC_MULTI_MAX_DATASETS).
#' @return The new named list (invisible).
sc_multi_register <- function(datasets, label, obj,
                              relation = "standalone",
                              producer = "pipeline_save",
                              overwrite = FALSE, max_datasets = NULL) {
  .sc_multi_check_datasets(datasets)
  .sc_multi_check_producer(producer)
  lbl <- sc_multi_check_label(label)
  sc_multi_check_obj(obj)
  sc_multi_check_relation(relation)
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop(.sc_multi_stop("overwrite doit être TRUE ou FALSE.",
                        state = "invalid_input"))
  }
  capacity <- .sc_multi_resolve_capacity(max_datasets)

  exists_lbl <- lbl %in% names(datasets)
  if (!exists_lbl && length(datasets) >= capacity) {
    stop(.sc_multi_stop(
      sprintf("Plafond du conteneur sc_datasets atteint (%d jeux) — supprimez un jeu avant d'enregistrer « %s » (budget RAM : chaque entrée est un objet Seurat complet).",
              capacity, lbl),
      state = "capacity_exceeded"))
  }
  if (exists_lbl && !isTRUE(overwrite)) {
    stop(.sc_multi_stop(
      sprintf("Un dataset SC « %s » existe déjà — choisissez un autre label ou autorisez explicitement le remplacement (overwrite = TRUE).",
              lbl),
      state = "duplicate_label"))
  }

  now <- Sys.time()
  prev <- if (exists_lbl) datasets[[lbl]] else NULL
  entry <- list(
    label         = lbl,
    producer      = producer,
    relation      = relation,
    registered_at = if (!is.null(prev)) prev$registered_at else now,
    updated_at    = now,
    obj           = obj
  )
  datasets[[lbl]] <- entry
  invisible(datasets)
}

#' Remove a named SC dataset (pure — returns the new list)
#' @param datasets Current container.
#' @param label Label to remove.
#' @return The new named list (invisible).
sc_multi_remove <- function(datasets, label) {
  .sc_multi_check_datasets(datasets)
  lbl <- sc_multi_check_label(label)
  if (!lbl %in% names(datasets)) {
    stop(.sc_multi_stop(
      sprintf("Dataset SC « %s » introuvable dans le conteneur sc_datasets.", lbl),
      state = "unknown_label"))
  }
  datasets[[lbl]] <- NULL
  invisible(datasets)
}

#' Get one entry (pure)
#' @param datasets Current container.
#' @param label Label to fetch.
#' @return The entry (invisible).
sc_multi_get <- function(datasets, label) {
  .sc_multi_check_datasets(datasets)
  lbl <- sc_multi_check_label(label)
  if (!lbl %in% names(datasets)) {
    stop(.sc_multi_stop(
      sprintf("Dataset SC « %s » introuvable dans le conteneur sc_datasets.", lbl),
      state = "unknown_label"))
  }
  invisible(datasets[[lbl]])
}

#' Container summary (frozen columns, contract §4.2)
#' @param datasets Current container.
#' @return data.frame (0 rows when empty).
sc_multi_summary <- function(datasets) {
  .sc_multi_check_datasets(datasets)
  cols <- c("label", "producer", "relation", "n_cells", "n_genes",
            "n_samples", "has_clusters", "median_nFeature_RNA",
            "median_nCount_RNA", "median_percent_mt",
            "registered_at", "updated_at")
  if (is.null(datasets) || length(datasets) == 0L) {
    empty <- data.frame(
      label = character(0), producer = character(0),
      relation = character(0), n_cells = integer(0), n_genes = integer(0),
      n_samples = integer(0), has_clusters = logical(0),
      median_nFeature_RNA = integer(0), median_nCount_RNA = integer(0),
      median_percent_mt = numeric(0),
      registered_at = character(0), updated_at = character(0),
      stringsAsFactors = FALSE
    )
    return(empty)
  }
  rows <- lapply(names(datasets), function(nm) {
    e <- datasets[[nm]]
    obj <- e$obj
    producer <- if (is.null(e$producer)) NA_character_ else as.character(e$producer)
    relation <- if (is.null(e$relation)) NA_character_ else as.character(e$relation)
    n_cells <- tryCatch(as.integer(length(SeuratObject::Cells(obj))),
                        error = function(e) NA_integer_)
    n_genes <- tryCatch(as.integer(length(SeuratObject::Features(obj))),
                        error = function(e) NA_integer_)
    ident <- tryCatch(unique(as.character(SeuratObject::Idents(obj))), error = function(e) NULL)
    orig <- tryCatch({
      m <- obj@meta.data
      if ("orig.ident" %in% colnames(m)) unique(as.character(m$orig.ident)) else ident
    }, error = function(e) NULL)
    clusters <- tryCatch("seurat_clusters" %in% colnames(obj@meta.data),
                         error = function(e) FALSE)
    # QC par dataset (roadmap 4.2, 2026-09-27) : médianes de nFeature_RNA /
    # nCount_RNA / percent.mt — NA quand la métrique est absente de l'objet.
    med <- tryCatch({
      m <- obj@meta.data
      c(
        if ("nFeature_RNA" %in% colnames(m)) as.integer(stats::median(m$nFeature_RNA, na.rm = TRUE)) else NA_integer_,
        if ("nCount_RNA" %in% colnames(m)) as.integer(stats::median(m$nCount_RNA, na.rm = TRUE)) else NA_integer_,
        if ("percent.mt" %in% colnames(m)) round(as.numeric(stats::median(m$percent.mt, na.rm = TRUE)), 2) else NA_real_
      )
    }, error = function(e) c(NA_integer_, NA_integer_, NA_real_))
    data.frame(
      label         = nm,
      producer      = producer,
      relation      = relation,
      n_cells       = n_cells,
      n_genes       = n_genes,
      n_samples     = if (is.null(orig)) NA_integer_ else length(orig),
      has_clusters  = isTRUE(clusters),
      median_nFeature_RNA = med[1],
      median_nCount_RNA   = med[2],
      median_percent_mt   = med[3],
      registered_at = format(e$registered_at, "%Y-%m-%d %H:%M"),
      updated_at    = format(e$updated_at, "%Y-%m-%d %H:%M"),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

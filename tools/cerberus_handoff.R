#!/usr/bin/env Rscript
# =============================================================================
# tools/cerberus_handoff.R — CLI de validation headless pour cerberus-handoff
#                            TranscriptoShiny ("Cerberus")
# =============================================================================
# Valide un bundle de données déposé par une application amont selon le contrat
# `cerberus-exchange/v1`.
#
# USAGE:
#   Rscript tools/cerberus_handoff.R --drop=<path> --out=<output_dir> [--strict] [--quiet]
#
# ARGUMENTS:
#   --drop=<path>       : Répertoire contenant le drop amont (manifest.json,
#                         matrice de counts, samplesheet). Obligatoire.
#   --out=<output_dir>  : Répertoire de sortie pour `validation.json`. Obligatoire.
#   --strict            : Traite les avertissements comme des erreurs bloquantes.
#   --quiet             : N'affiche aucun résumé sur la console.
#   --help, -h          : Affiche l'aide et quitte (code 0).
#
# CODES DE SORTIE:
#   0 : Validation réussie (status == "VALID").
#   1 : Validation échouée (status == "INVALID") ou erreur d'arguments.
#   2 : Erreur fatale d'environnement (ex: impossible d'écrire dans --out).
# =============================================================================

# ── Helper opérateur null-coalescing local -----------------------------------
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x

# ── Résolution de la racine du projet ---------------------------------------
cerberus_handoff_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", args, value = TRUE)
  start <- if (length(hit) > 0L) dirname(sub("^--file=", "", hit[1L])) else getwd()
  d <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in seq_len(8L)) {
    if (file.exists(file.path(d, "app.R"))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  getwd()
}

# ── Chargement du moteur de validation et dépendances -----------------------
cerberus_handoff_load_engine <- function(root = cerberus_handoff_root()) {
  # J1 CERBERUS (2026-10-26) : le moteur suit l'arbre du paquet (D2 M5) —
  # ex-R/core/cerberus_handoff.R, dernier etranger de R/core (git mv, sha256
  # inchange 6208f2bf). Le moteur est pur R : il garde ses replis exists().
  engine_file <- file.path(root, "tools", "pkg_skeleton", "R", "cerberus_handoff.R")
  if (!file.exists(engine_file)) {
    stop(sprintf("Moteur de validation introuvable : %s", engine_file), call. = FALSE)
  }
  if (!exists("bulk_assert_raw_counts", envir = globalenv(), inherits = FALSE)) {
    bc_file <- file.path(root, "tools", "pkg_skeleton", "R", "batch_correction.R")
    if (!file.exists(bc_file)) bc_file <- file.path(root, "R", "bulk", "batch_correction.R")
    if (file.exists(bc_file)) sys.source(bc_file, envir = globalenv())
  }
  if (!exists("detect_gene_id_type", envir = globalenv(), inherits = FALSE)) {
    io_file <- file.path(root, "tools", "pkg_skeleton", "R", "io_helpers.R")
    if (file.exists(io_file)) sys.source(io_file, envir = globalenv())
  }
  if (!exists("check_design_confounding", envir = globalenv(), inherits = FALSE)) {
    val_file <- file.path(root, "R", "core", "validation.R")
    if (file.exists(val_file)) sys.source(val_file, envir = globalenv())
  }
  sys.source(engine_file, envir = globalenv())
  invisible(TRUE)
}

# ── Parsing des arguments de la ligne de commande ----------------------------
cerberus_handoff_args <- function(argv = commandArgs(trailingOnly = TRUE)) {
  opt <- list(
    drop = NULL,
    out = NULL,
    strict = FALSE,
    quiet = FALSE,
    help = FALSE
  )
  i <- 1L
  while (i <= length(argv)) {
    a <- argv[i]
    if (identical(a, "--help") || identical(a, "-h")) {
      opt$help <- TRUE
    } else if (grepl("^--drop=", a)) {
      opt$drop <- sub("^--drop=", "", a)
    } else if (identical(a, "--drop") && i < length(argv)) {
      i <- i + 1L
      opt$drop <- argv[i]
    } else if (grepl("^--out=", a)) {
      opt$out <- sub("^--out=", "", a)
    } else if (identical(a, "--out") && i < length(argv)) {
      i <- i + 1L
      opt$out <- argv[i]
    } else if (identical(a, "--strict")) {
      opt$strict <- TRUE
    } else if (identical(a, "--quiet")) {
      opt$quiet <- TRUE
    } else if (startsWith(a, "--")) {
      stop(sprintf("Option inconnue : %s (voir --help)", a), call. = FALSE)
    } else {
      stop(sprintf("Argument positionnel inattendu : %s (voir --help)", a), call. = FALSE)
    }
    i <- i + 1L
  }
  opt
}

# ── Affichage de l'aide -----------------------------------------------------
cerberus_handoff_usage <- function() {
  cat(
    "Usage: Rscript tools/cerberus_handoff.R --drop=<path> --out=<output_dir> [options]\n\n",
    "Options:\n",
    "  --drop=<path>       Repertoire du drop amont (obligatoire)\n",
    "  --out=<output_dir>  Repertoire de sortie pour validation.json (obligatoire)\n",
    "  --strict            Traite les avertissements comme des erreurs bloquantes\n",
    "  --quiet             Mode silencieux, n'affiche rien sur stdout\n",
    "  --help, -h          Affiche cette aide et quitte (exit 0)\n\n",
    "Codes de sortie:\n",
    "  0 : Drop valide (status == 'VALID')\n",
    "  1 : Drop invalide (status == 'INVALID') ou erreur d'arguments\n",
    "  2 : Erreur fatale d'environnement\n",
    sep = ""
  )
}

# ── Écriture atomique du rapport JSON ---------------------------------------
cerberus_handoff_write_json <- function(obj, out_path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Le paquet 'jsonlite' est requis pour serialiser validation.json", call. = FALSE)
  }
  dir_target <- dirname(out_path)
  if (!dir.exists(dir_target)) {
    dir.create(dir_target, recursive = TRUE, showWarnings = FALSE)
  }
  json_txt <- jsonlite::toJSON(obj, pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null")
  tmp_file <- tempfile(pattern = "val_", tmpdir = dir_target, fileext = ".tmp")
  writeLines(json_txt, tmp_file, useBytes = TRUE)
  if (!file.rename(tmp_file, out_path)) {
    file.copy(tmp_file, out_path, overwrite = TRUE)
    unlink(tmp_file)
  }
  invisible(out_path)
}

# ── Point d'entrée principal ------------------------------------------------
cerberus_handoff_main <- function(argv = commandArgs(trailingOnly = TRUE)) {
  opt <- tryCatch(cerberus_handoff_args(argv), error = function(e) e)
  if (inherits(opt, "condition")) {
    message("ERREUR CLI: ", conditionMessage(opt))
    cerberus_handoff_usage()
    return(invisible(1L))
  }
  if (isTRUE(opt$help)) {
    cerberus_handoff_usage()
    return(invisible(0L))
  }

  if (is.null(opt$out) || !nzchar(trimws(opt$out))) {
    message("ERREUR CLI: L'argument --out=<output_dir> est obligatoire.")
    cerberus_handoff_usage()
    return(invisible(1L))
  }
  out_dir <- normalizePath(opt$out, winslash = "/", mustWork = FALSE)
  if (!dir.exists(out_dir)) {
    created <- dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    if (!created && !dir.exists(out_dir)) {
      message(sprintf("ERREUR FATALE: Impossible de creer le repertoire de sortie : %s", out_dir))
      return(invisible(2L))
    }
  }
  val_json_path <- file.path(out_dir, "validation.json")

  if (is.null(opt$drop) || !nzchar(trimws(opt$drop))) {
    err_msg <- "L'argument --drop=<path> est obligatoire."
    message("ERREUR CLI: ", err_msg)
    payload <- list(
      valid = FALSE,
      status = "INVALID",
      schema_version = "cerberus-exchange/v1",
      dataset_id = NA_character_,
      validated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      summary = list(
        valid = FALSE,
        n_errors = 1L,
        n_warnings = 0L,
        message = err_msg
      ),
      errors = list(list(
        code = "manifest_missing",
        field = "drop",
        message = err_msg
      )),
      warnings = list()
    )
    cerberus_handoff_write_json(payload, val_json_path)
    return(invisible(1L))
  }

  drop_dir <- normalizePath(opt$drop, winslash = "/", mustWork = FALSE)
  if (!dir.exists(drop_dir)) {
    err_msg <- sprintf("Le repertoire de drop amont n'existe pas : %s", drop_dir)
    if (!opt$quiet) message("ERREUR: ", err_msg)
    payload <- list(
      valid = FALSE,
      status = "INVALID",
      schema_version = "cerberus-exchange/v1",
      dataset_id = NA_character_,
      validated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      summary = list(
        valid = FALSE,
        n_errors = 1L,
        n_warnings = 0L,
        message = err_msg
      ),
      drop_info = list(drop_dir = drop_dir),
      errors = list(list(
        code = "file_not_found",
        field = "drop",
        message = err_msg
      )),
      warnings = list()
    )
    cerberus_handoff_write_json(payload, val_json_path)
    return(invisible(1L))
  }

  root <- cerberus_handoff_root()
  load_err <- tryCatch(cerberus_handoff_load_engine(root), error = function(e) e)
  if (inherits(load_err, "condition")) {
    err_msg <- sprintf("Echec chargement moteur de validation : %s", conditionMessage(load_err))
    if (!opt$quiet) message("ERREUR: ", err_msg)
    payload <- list(
      valid = FALSE,
      status = "INVALID",
      schema_version = "cerberus-exchange/v1",
      dataset_id = NA_character_,
      validated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      summary = list(
        valid = FALSE,
        n_errors = 1L,
        n_warnings = 0L,
        message = err_msg
      ),
      errors = list(list(
        code = "file_corrupted",
        field = "engine",
        message = err_msg
      )),
      warnings = list()
    )
    cerberus_handoff_write_json(payload, val_json_path)
    return(invisible(1L))
  }

  t0 <- Sys.time()
  val_res <- tryCatch(
    cerberus_handoff_validate_drop(drop_dir = drop_dir, out_dir = NULL),
    error = function(e) {
      list(
        valid = FALSE,
        status = "INVALID",
        schema_version = "cerberus-exchange/v1",
        dataset_id = NA_character_,
        validated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        summary = list(
          valid = FALSE,
          n_errors = 1L,
          n_warnings = 0L,
          duration_seconds = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 3),
          message = paste0("Exception interne durant la validation : ", conditionMessage(e))
        ),
        errors = list(list(
          code = "file_corrupted",
          field = "validator",
          message = conditionMessage(e)
        )),
        warnings = list()
      )
    }
  )

  # Normalisation du statut
  if (is.null(val_res$status)) {
    val_res$status <- if (isTRUE(val_res$valid)) "VALID" else "INVALID"
  } else {
    val_res$status <- toupper(val_res$status)
  }

  # Prise en compte de --strict
  if (isTRUE(opt$strict) && length(val_res$warnings) > 0L) {
    val_res$status <- "INVALID"
    val_res$valid <- FALSE
    if (!is.null(val_res$summary)) val_res$summary$valid <- FALSE
  }

  write_err <- tryCatch(
    cerberus_handoff_write_json(val_res, val_json_path),
    error = function(e) e
  )
  if (inherits(write_err, "condition")) {
    message(sprintf("ERREUR FATALE: Impossible d'ecrire validation.json dans %s : %s",
                    val_json_path, conditionMessage(write_err)))
    return(invisible(2L))
  }

  if (!isTRUE(opt$quiet)) {
    cat("\n====================================================================\n")
    cat(" TranscriptoShiny ('Cerberus') — Handoff Ingestion Validator\n")
    cat("====================================================================\n")
    cat(sprintf("Drop directory : %s\n", drop_dir))
    cat(sprintf("Output report  : %s\n", val_json_path))
    cat(sprintf("Status         : %s\n", val_res$status))
    if (!is.null(val_res$dataset_id) && !is.na(val_res$dataset_id)) {
      cat(sprintf("Dataset ID     : %s\n", val_res$dataset_id))
    }
    if (!is.null(val_res$checks$matrix_semantics)) {
      m <- val_res$checks$matrix_semantics
      cat(sprintf("Matrix QC      : %d features x %d samples (type=%s, frac_int=%.4f)\n",
                  m$n_features %||% 0L, m$n_samples %||% 0L,
                  m$matrix_type %||% "unknown", m$frac_integer %||% 0))
    }
    if (!is.null(val_res$checks$experimental_design)) {
      d <- val_res$checks$experimental_design
      cat(sprintf("Design QC      : Condition '%s', %d samples, replicates sufficient=%s\n",
                  d$condition_col %||% "unknown", d$n_samples %||% 0L,
                  isTRUE(d$sufficient_replicates)))
    }
    if (!is.null(val_res$checks$species_and_identifiers)) {
      s <- val_res$checks$species_and_identifiers
      cat(sprintf("Organism / IDs : Species=%s (detected=%s), IDs=%s (detected=%s)\n",
                  s$declared_species %||% "unknown", s$detected_species %||% "unknown",
                  s$declared_id_type %||% "unknown", s$detected_id_type %||% "unknown"))
    }
    if (length(val_res$errors)) {
      cat(sprintf("\n-- ERREURS (%d) --\n", length(val_res$errors)))
      for (e in val_res$errors) {
        cat(sprintf("  [%s] %s: %s\n", e$code, e$field, e$message))
      }
    }
    if (length(val_res$warnings)) {
      cat(sprintf("\n-- AVERTISSEMENTS (%d) --\n", length(val_res$warnings)))
      for (w in val_res$warnings) {
        cat(sprintf("  [%s] %s: %s\n", w$code, w$field, w$message))
      }
    }
    cat("--------------------------------------------------------------------\n")
    cat(sprintf("Verdict        : %s\n", val_res$status))
    cat("====================================================================\n\n")
  }

  if (identical(val_res$status, "VALID")) invisible(0L) else invisible(1L)
}

# ── Point d'entrée script Rscript (jamais au source) -------------------------
if (identical(environment(), globalenv()) && sys.nframe() == 0L &&
    !interactive() && length(grep("--file=", commandArgs(trailingOnly = FALSE))) > 0L) {
  status <- cerberus_handoff_main(commandArgs(trailingOnly = TRUE))
  quit(status = status, save = "no")
}

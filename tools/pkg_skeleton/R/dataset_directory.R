# =============================================================================
# tools/pkg_skeleton/R/dataset_directory.R — PoC « répertoire de datasets »
# (option O3, moteur pur)
# =============================================================================
# Fiche (fait foi) :
# docs/proposals/PROPOSAL_DATASET_DIRECTORY_POC_2026-11-02.md
#   §1 AC1 (états valid|warn|invalid|empty, raisons = vocabulaire v1 tel quel),
#      AC2 (strictement lecture seule, jamais au cold-boot),
#      AC5 (RED-first, freeze test) ;
#   §3.1 (racine 3 niveaux, motif ts_session_checkpoint_dir(),
#         session_checkpoint.R:120) ;
#   §3.2 (moteur pur : UNE profondeur de sous-répertoires, délégation
#         biomentat_handoff_validate_drop() — ZÉRO nouvelle règle) ;
#   §5 (échec d'un drop isolé — jamais d'erreur propagée ; le scan n'écrit
#       rien, l'état applicatif reste dans global_data/ASR — vue passive).
#
# Ce que ce fichier N'EST PAS (fiche §2/§7) :
#   * PAS le contrat v2 (SC/spatial .h5ad hors PoC) ;
#   * PAS un watcher filesystem (O4 différé) ;
#   * PAS une ingestion : aucun appel au scan au cold-boot (AC2) — le
#     branchement UI bulk est le jalon P1, l'outil drive passif
#     transcripto_drive_datasets le jalon P2 ; tant que P1 n'est pas livré,
#     biomentat_handoff.R reste « étranger non câblé » dans app.R (bloc 03) —
#     la dépendance runtime du scan est résolue À L'APPEL, jamais au
#     chargement.
#
# Purity rails (règle maison, motif session_checkpoint.R:9-22) :
#   * NO shiny, NO reactive, NO session — ce fichier doit sourcer() partout où
#     tourne un process R pur ;
#   * STRICTEMENT LECTURE SEULE (AC2) : le scan n'écrit RIEN. La seule
#     écriture tolérée dans la chaîne est validation.json, écrite par le
#     moteur v1 biomentat_handoff_write_validation() UNIQUEMENT quand un
#     out_dir explicite est fourni (biomentat_handoff.R:755-757) — le scan ne
#     passe JAMAIS out_dir ;
#   * ZÉRO nouvelle règle de validation (AC1) : chaque sous-répertoire est
#     délégué tel quel au moteur v1 biomentat_handoff_validate_drop()
#     (biomentat_handoff.R:634, alias biomentat_validate_drop) ; les raisons
#     retournées sont les états v1 (biomentat_handoff_states(),
#     biomentat_handoff.R:104-123) repris verbatim ;
#   * la racine n'est JAMAIS créée ici (motif checkpoint : création au moment
#     de l'écriture — il n'y a pas d'écriture) ; une racine absente donne une
#     table vide, jamais une erreur ;
#   * helpers transverses réutilisés du voisin session_checkpoint.R (zéro
#     duplication) : .sc_project_root (:96, remontée app.R) et
#     .sc_scalar_text (:81, garde scalaire NA/vide -> NULL).
# =============================================================================

# ── Résolution de la racine des datasets (fiche §3.1) ────────────────────────

#' Resolve the datasets root directory (fiche PoC §3.1).
#'
#' Resolution order — motif `ts_session_checkpoint_dir()`
#' (session_checkpoint.R:120), arbitrage racine tranché (opérateur « continue »
#' + best-judgment délégué, jalon P0) :
#' (1) explicit `dir` argument (tests / appel programmatique) ;
#' (2) env `TRANSCRIPTO_DATASETS_DIR` (knob dédié) ;
#' (3) PREMIÈRE entrée du knob opérateur existant `TRANSCRIPTO_DRIVE_DATA_DIR`
#'     + "/datasets" (même lecture multi-chemins que le checkpoint,
#'     session_checkpoint.R:128-135) ;
#' (4) `<project root>/data_sc/datasets`, la racine projet résolue par la
#'     MÊME remontée app.R que le checkpoint (`.sc_project_root()`,
#'     session_checkpoint.R:96 — réutilisée, zéro duplication).
#' Le répertoire n'est PAS créé ici (appelant lecture-seule, AC2) — aucun
#' effet de bord disque, jamais tempdir() seul.
#'
#' @param dir Racine explicite ; prioritaire quand non NULL/vide.
#' @return Chemin absolu (slashes avant), non créé.
ts_datasets_dir <- function(dir = NULL) {
  if (!is.null(.sc_scalar_text(dir))) {
    return(normalizePath(dir, winslash = "/", mustWork = FALSE))
  }
  env <- Sys.getenv("TRANSCRIPTO_DATASETS_DIR", "")
  if (nzchar(env)) {
    return(normalizePath(env, winslash = "/", mustWork = FALSE))
  }
  data_env <- Sys.getenv("TRANSCRIPTO_DRIVE_DATA_DIR", "")
  if (nzchar(data_env)) {
    first <- strsplit(data_env, .Platform$path.sep, fixed = TRUE)[[1]][1]
    if (nzchar(first)) {
      return(normalizePath(file.path(first, "datasets"),
                           winslash = "/", mustWork = FALSE))
    }
  }
  normalizePath(file.path(.sc_project_root(), "data_sc", "datasets"),
                winslash = "/", mustWork = FALSE)
}

# ── Scanner passif (fiche §3.2 — UNE profondeur, délégation v1) ──────────────

#' Frozen status vocabulary of the scan table (AC1).
#'
#' Motif `biomentat_handoff_states()` (biomentat_handoff.R:104) : le vocabulaire
#' est exposé pour être figé par le test T5 (motif test-report-contract-freeze.R).
#' "empty" = sous-répertoire sans rien à valider (aucun fichier, aucun
#' sous-répertoire) ; sa raison "empty_directory" est la SEULE raison hors
#' vocabulaire v1 (documentée fiche §1 AC1 / §3.1 — un dépot vide n'est pas
#' une validation échouée).
ts_dataset_dir_scan_states <- function() {
  c("valid", "warn", "invalid", "empty")
}

# Chemin absolu à slashes avant, sans exiger l'existence (mustWork = FALSE).
.ts_datasets_path <- function(...) {
  normalizePath(file.path(...), winslash = "/", mustWork = FALSE)
}

# Champ scalaire du contrat de sortie : NA_character_ quand indisponible
# (schéma figé T5 — jamais NULL, pour un consumeur UI/JSON homogène).
.ts_datasets_field <- function(x) {
  v <- .sc_scalar_text(x)
  if (is.null(v)) NA_character_ else v
}

# Lecture du manifeste v1 UNE fois, par délégation au lecteur EXISTANT
# biomentat_handoff_read_manifest() (biomentat_handoff.R:142) — zéro nouvelle
# règle de parsing ; tout échec -> NULL, jamais d'erreur propagée (§5). Le
# résultat sert le résumé ET la résolution des chemins (pas de double I/O).
.ts_datasets_read_manifest <- function(drop_dir) {
  tryCatch(biomentat_handoff_read_manifest(drop_dir), error = function(e) NULL)
}

# Scan d'UN sous-répertoire (UNE profondeur — fiche §3.2/§5) : résumé du
# manifeste + chemins résolus, mtime, puis délégation de la validation au
# moteur v1. Échec d'un drop = entrée invalid isolée, les autres continuent.
.ts_datasets_scan_one <- function(drop_dir, validate = TRUE) {
  dir_norm <- .ts_datasets_path(drop_dir)
  man <- .ts_datasets_read_manifest(drop_dir)

  entry <- list(
    status = NA_character_,
    raison = NA_character_,
    manifest = list(
      dataset_id = .ts_datasets_field(man$dataset_id),
      species = .ts_datasets_field(man$species),
      feature_id_type = .ts_datasets_field(man$feature_id_type),
      matrix_type = .ts_datasets_field(man$matrix_type)
    ),
    paths = list(
      dir = dir_norm,
      manifest = .ts_datasets_path(drop_dir, "manifest.json"),
      matrix = NA_character_,
      samplesheet = NA_character_
    ),
    mtime = file.mtime(dir_norm),
    checks = list()
  )
  # chemins de matrice/samplesheet résolus depuis le manifeste v1 (chemins
  # RELATIFS exigés par le contrat v1 — ici on ne fait qu'assembler, la
  # conformité des chemins reste la validation déléguée)
  if (!is.null(man)) {
    mp <- .sc_scalar_text(man$matrix_path)
    sp <- .sc_scalar_text(man$samplesheet_path)
    if (!is.null(mp)) entry$paths$matrix <- .ts_datasets_path(drop_dir, mp)
    if (!is.null(sp)) entry$paths$samplesheet <- .ts_datasets_path(drop_dir, sp)
  }

  # sous-répertoire sans rien à valider -> "empty" (AC1 ; seule raison hors v1)
  has_files <- length(list.files(drop_dir, all.files = FALSE, no.. = TRUE)) > 0L
  has_subdirs <- length(list.dirs(drop_dir, full.names = FALSE,
                                  recursive = FALSE)) > 0L
  if (!has_files && !has_subdirs) {
    entry$status <- "empty"
    entry$raison <- "empty_directory"
    return(entry)
  }

  # validate = FALSE : listing brut (table rapide pour UI/drive), SANS
  # validation — status/raison restent NA, résumé + chemins portés
  if (!isTRUE(validate)) {
    return(entry)
  }

  # Délégation v1 — ZÉRO nouvelle règle (AC1). out_dir jamais fourni : le
  # scan n'écrit RIEN (AC2 ; la seule écriture tolérée de la chaîne,
  # validation.json, n'est déclenchée que par un out_dir explicite,
  # biomentat_handoff.R:755-757).
  res <- tryCatch(
    biomentat_handoff_validate_drop(drop_dir = drop_dir, out_dir = NULL),
    error = function(e) NULL
  )
  if (is.null(res)) {
    # filet §5 : un drop qui ferait tomber le moteur lui-même est isolé —
    # raison = l'état catch-all du moteur v1 ("file_corrupted",
    # biomentat_handoff.R:727-728), jamais une erreur propagée
    entry$status <- "invalid"
    entry$raison <- "file_corrupted"
    return(entry)
  }

  entry$checks <- if (is.null(res$checks)) list() else res$checks
  if (isTRUE(res$valid)) {
    entry$status <- if (length(res$warnings) > 0L) "warn" else "valid"
  } else {
    entry$status <- "invalid"
    # raison = PREMIER code d'erreur v1, repris tel quel (AC1) — l'ordre des
    # erreurs est celui du moteur v1 (étapes manifeste -> matrice -> samplesheet)
    first_err <- if (length(res$errors) > 0L) res$errors[[1L]]$code else NULL
    entry$raison <- .ts_datasets_field(first_err)
  }
  # dataset_id : la parole est au manifeste VALIDÉ par le moteur v1
  validated_id <- .sc_scalar_text(res$dataset_id)
  if (!is.null(validated_id)) entry$manifest$dataset_id <- validated_id
  # enrichissement du résumé depuis les checks v1 (déclaré vs détecté)
  if (!is.null(entry$checks$species_and_identifiers)) {
    spc <- .sc_scalar_text(entry$checks$species_and_identifiers$declared_species)
    fid <- .sc_scalar_text(entry$checks$species_and_identifiers$declared_id_type)
    if (!is.null(spc)) entry$manifest$species <- spc
    if (!is.null(fid)) entry$manifest$feature_id_type <- fid
  }
  if (!is.null(entry$checks$matrix_semantics)) {
    mtp <- .sc_scalar_text(entry$checks$matrix_semantics$matrix_type)
    if (!is.null(mtp)) entry$manifest$matrix_type <- mtp
  }
  entry
}

#' Scan the datasets root directory (fiche PoC §3.2 — passive, read-only).
#'
#' Lists ONE level of subdirectories of `datasets_dir` and delegates each one
#' to the existing v1 engine `biomentat_handoff_validate_drop()`
#' (biomentat_handoff.R:634) — the scan adds ZERO validation rule (AC1). The
#' result is a NAMED LIST keyed by the v1 `dataset_id` (fallback: the
#' subdirectory basename when no manifest is readable), each entry carrying:
#' `status` (valid | warn | invalid | empty — AC1, `ts_dataset_dir_scan_states()`),
#' `raison` (v1 vocabulary verbatim — first v1 error code; "empty_directory"
#' for an empty subdirectory; NA when valid or unvalidated), `manifest`
#' (dataset_id/species/feature_id_type/matrix_type summary), `paths`
#' (dir/manifest/matrix/samplesheet — resolved from the v1 manifest),
#' `mtime` (subdirectory mtime) and `checks` (the v1 checks, inherited
#' verbatim). With `validate = FALSE` the table is a bare listing:
#' status/raison stay NA, the manifest summary and paths are still carried.
#'
#' STRICT READ-ONLY (AC2) : nothing is written anywhere — the engine's only
#' write path (validation.json) requires an explicit `out_dir` which the scan
#' NEVER passes ; a failing subdirectory is isolated (fiche §5, never a
#' propagated error) ; a missing or non-directory root yields an empty table
#' (the scan never blocks and never creates anything).
#'
#' @param datasets_dir Root to scan (default: `ts_datasets_dir()`).
#' @param validate Delegate each subdirectory to the v1 engine (default TRUE).
#' @return Named list of per-dataset entries (see details); empty list for an
#'   empty or missing root.
ts_dataset_dir_scan <- function(datasets_dir = ts_datasets_dir(), validate = TRUE) {
  # racine absente / non répertoire : table vide, jamais d'erreur (AC2, §5)
  root <- .sc_scalar_text(datasets_dir)
  if (is.null(root) || !dir.exists(root)) {
    return(list())
  }
  # ordre canonique indépendant de la locale (motif tools/sources_manifest.R :
  # LC_COLLATE="C" autour du list.*, l'ordre épinglé est documental)
  old_coll <- Sys.setlocale("LC_COLLATE", "C")
  on.exit(Sys.setlocale("LC_COLLATE", old_coll), add = TRUE)
  subdirs <- sort(list.dirs(root, full.names = TRUE, recursive = FALSE))

  out <- list()
  for (d in subdirs) {
    entry <- .ts_datasets_scan_one(d, validate = validate)
    key <- .sc_scalar_text(entry$manifest$dataset_id)
    if (is.null(key)) key <- basename(d)   # repli : basename du sous-répertoire
    out[[key]] <- entry
  }
  out
}

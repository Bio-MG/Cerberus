# =============================================================================
# test-dataset-dir-scan.R -- PoC « répertoire de datasets » : scanner passif
# (RED-first)
# =============================================================================
# FICHE (fait foi) :
# docs/proposals/PROPOSAL_DATASET_DIRECTORY_POC_2026-11-02.md
#   §1 AC1 (états valid|warn|invalid|empty + raison vocabulaire v1 tel quel),
#      AC2 (strictement lecture seule, jamais au cold-boot),
#      AC5 (garde RED-first + test freeze) ;
#   §3.1 (racine 3 niveaux, motif ts_session_checkpoint_dir(),
#         session_checkpoint.R:120) ;
#   §3.2 (moteur pur : UNE profondeur, délégation biomentat_handoff_validate_drop(),
#         biomentat_handoff.R:634 — ZÉRO nouvelle règle de validation) ;
#   §4 T1-T5 ; §5 (échec d'un drop isolé — jamais d'erreur propagée).
#
# Jalon P0 : ts_datasets_dir() + ts_dataset_dir_scan() + tests T1-T5
# (fiche §6 :99). L'UI bulk (T6 de la fiche) et l'outil drive passif (T7)
# sont les jalons P1/P2 — PAS testés ici.
#
# RED : tant que tools/pkg_skeleton/R/dataset_directory.R n'existe pas,
# chaque bloc T* échoue sur son premier expect_true explicite (jamais un
# skip — un skip n'est pas une porte rouge ; motif test-spmv1-meta-col-
# validation.R:118-122, lui-même motif test-biostat-tree.R:53-67).
#
# Fixtures : gabarit test-biomentat-handoff.R (.make_test_drop, :23-63) —
# drop bulk minimal construit DANS le test, en tempdir : 120 gènes Ensembl,
# rpois(lambda=300) seed 42, TSV col.names=NA, manifest biomentat-exchange/v1.
# Le drop VALIDE porte 4 échantillons / 2 conditions (2x2) : c'est le
# PLANCHER du plan anti-pseudoréplication v1 (biomentat_handoff.R:588,
# min_rep = 2) — 3 échantillons sur 2 conditions est TOUJOURS déficient
# d'un côté (le « 3 échantillons / 2 conditions » de la fiche §3.5 sert
# donc le cas T4 : control, control, treated -> treated = 1 réplicat ->
# pseudoreplication_detected, mesuré).
#
# QU'EST ASSERTABLE HEADLESS (honneteté du garde, AC5) :
#   * états + raisons (vocabulaire v1 tel quel, biomentat_handoff_states(),
#     biomentat_handoff.R:104-123), checks v1 hérités, chemins résolus,
#     mtime, noms de la table ;
#   * le schéma de sortie (noms de champs, états) est FIGÉ (T5, motif
#     test-report-contract-freeze.R : surface figée = code + test ensemble) ;
#   * la lecture-seule (AC2) : snapshot du répertoire avant/après scan,
#     validation.json exclu (SEULE écriture tolérée de la chaîne, produite
#     par le moteur v1 quand un out_dir explicite est fourni,
#     biomentat_handoff.R:755-757 — le scan ne passe JAMAIS out_dir).
#
# ASCII pur dans les motifs de garde (C7) ; paste0 des que multi-arguments.
# =============================================================================

.dds_comp_rel  <- "tools/pkg_skeleton/R/dataset_directory.R"
.dds_comp_path <- file.path(ts_project_root(), .dds_comp_rel)
.dds_present   <- file.exists(.dds_comp_path)
.dds_red_info  <- paste0(
  .dds_comp_rel, " absent : PoC répertoire de datasets (fiche ",
  "PROPOSAL_DATASET_DIRECTORY_POC_2026-11-02.md §3.1-3.2, jalon P0) non ",
  "livré -- sans effet exécutant tant que le fichier n'existe pas"
)

# Chargement miroir de app.R bloc 03 (ordre d'exécution du bloc) : les
# fournisseurs du moteur v1 puis le motif de résolution racine
# (.sc_project_root, session_checkpoint.R:96) puis le composant PoC.
source_project_file("tools/pkg_skeleton/R/io_helpers.R")
source_project_file("tools/pkg_skeleton/R/error_state.R")
source_project_file("tools/pkg_skeleton/R/session_checkpoint.R")
source_project_file("tools/pkg_skeleton/R/biomentat_handoff.R")
if (.dds_present) {
  source_project_file(.dds_comp_rel)
}

# ── Fixtures (gabarit test-biomentat-handoff.R:23-63, ~3/2 échantillons) ------

.dds_root <- function() {
  d <- tempfile(pattern = "datasets_poc_")
  dir.create(d, recursive = TRUE)
  d
}

.dds_write_drop <- function(root, dataset_id,
                            n_genes = 120L,
                            samples = paste0("sample_", seq_len(4L)),
                            sheet_samples = NULL,
                            conditions = c("control", "control", "treated", "treated"),
                            batch = c("b1", "b2", "b1", "b2"),
                            write_manifest = TRUE,
                            manifest_raw = NULL) {
  drop_dir <- file.path(root, dataset_id)
  dir.create(drop_dir, recursive = TRUE)

  # Matrice de comptage (gabarit : 120 gènes humains Ensembl, counts entiers)
  g_ids <- sprintf("ENSG%011d", seq_len(n_genes))
  set.seed(42)
  counts <- matrix(as.integer(rpois(n_genes * length(samples), lambda = 300L)),
                   nrow = n_genes, ncol = length(samples),
                   dimnames = list(g_ids, samples))
  mat_path <- file.path(drop_dir, "counts.tsv")
  utils::write.table(as.data.frame(counts), file = mat_path,
                     sep = "\t", quote = FALSE, col.names = NA)

  # Samplesheet : sample + condition (+ batch non confondu, gabarit).
  # sheet_samples : noms de la samplesheet DÉCOUPLÉS des colonnes de la
  # matrice — c'est la discordance du T4a (concordance rompue,
  # biomentat_handoff.R:564-571).
  sheet_ids <- if (is.null(sheet_samples)) samples else sheet_samples
  sheet_path <- file.path(drop_dir, "samplesheet.tsv")
  utils::write.table(
    data.frame(sample = sheet_ids, condition = conditions, batch = batch,
               stringsAsFactors = FALSE),
    file = sheet_path, sep = "\t", quote = FALSE, row.names = FALSE)

  if (!is.null(manifest_raw)) {
    writeLines(manifest_raw, file.path(drop_dir, "manifest.json"), useBytes = TRUE)
  } else if (isTRUE(write_manifest)) {
    man <- list(
      schema_version = "biomentat-exchange/v1",
      dataset_id = dataset_id,
      species = "human",
      genome_assembly = "GRCh38",
      feature_id_type = "ensembl",
      matrix_path = "counts.tsv",
      matrix_type = "raw_counts",
      samplesheet_path = "samplesheet.tsv"
    )
    writeLines(jsonlite::toJSON(man, auto_unbox = TRUE, pretty = TRUE),
               file.path(drop_dir, "manifest.json"))
  }
  drop_dir
}

# Snapshot récursif chemins + tailles + mtimes, chemins RELATIFS à la racine
# (comparaison avant/après scan — la base du test lecture-seule AC2).
# Chemin ATTENDU normalisé à slashes avant (convention maison du moteur, motif
# ts_session_checkpoint_dir session_checkpoint.R:122 : normalizePath
# winslash="/") — file.path() seul donnerait des backslashes Windows.
.dds_p <- function(...) {
  normalizePath(file.path(...), winslash = "/", mustWork = FALSE)
}

.dds_snapshot <- function(root) {
  rel_files <- gsub("\\\\", "/",
                    list.files(root, recursive = TRUE, all.files = FALSE,
                               no.. = TRUE))
  rel_dirs <- gsub("\\\\", "/",
                   list.dirs(root, recursive = TRUE, full.names = FALSE))
  rel_dirs <- setdiff(rel_dirs, ".")
  full <- file.path(root, rel_files)
  data.frame(path = rel_files,
             size = file.info(full)$size,
             mtime = as.numeric(file.info(full)$mtime),
             stringsAsFactors = FALSE)
}

# ── T0 -- porte RED explicite (le fichier existe ; jamais un skip) -----------

test_that("T0 RED : le fichier PoC tools/pkg_skeleton/R/dataset_directory.R existe", {
  expect_true(.dds_present, info = .dds_red_info)
})

# ── T1 -- scan d'un répertoire vide : 0 dataset, pas d'erreur (fiche §4) -----

test_that("T1 scan d'un répertoire vide : 0 dataset, pas d'erreur (jamais de blocage)", {
  expect_true(.dds_present, info = .dds_red_info)
  if (!.dds_present) return(invisible(NULL))

  # (a) racine vide : 0 sous-répertoire -> table vide, aucune erreur levée
  root <- .dds_root()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  res <- ts_dataset_dir_scan(datasets_dir = root)
  expect_identical(res, list(),
                   info = "racine vide : la table de scan est une liste vide")

  # (b) racine INEXISTANTE : pareil — le scanner est passif (AC2, fiche §5 :
  #     jamais de blocage), il ne crée rien et ne lève rien
  missing_root <- file.path(tempdir(), paste0("datasets_absents_",
                                              as.integer(Sys.time())))
  res2 <- ts_dataset_dir_scan(datasets_dir = missing_root)
  expect_identical(res2, list(),
                   info = "racine absente : table vide, aucune erreur")
  expect_false(dir.exists(missing_root),
               info = "le scan ne crée JAMAIS la racine (lecture seule, AC2)")
})

# ── T2 -- drop v1 valide : status valid + checks v1 verts + paths résolus ----

test_that("T2 drop v1 valide : status valid, checks v1 verts, paths résolus, nommage par dataset_id", {
  expect_true(.dds_present, info = .dds_red_info)
  if (!.dds_present) return(invisible(NULL))

  root <- .dds_root()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  drop <- .dds_write_drop(root, "DS_SCAN_VALID")

  res <- ts_dataset_dir_scan(datasets_dir = root)
  expect_length(res, 1L)
  expect_identical(names(res), "DS_SCAN_VALID",
                   info = "la table est nommée par le dataset_id du manifeste")
  entry <- res[["DS_SCAN_VALID"]]

  expect_identical(entry$status, "valid",
                   info = "drop v1 conforme : état valid (gabarit 120 gènes x 4 échantillons 2x2, 0 warning)")
  expect_true(is.na(entry$raison),
              info = "pas de raison quand le drop est valide")

  # checks v1 hérités, tous verts (délégation biomentat_handoff_validate_drop)
  expect_true(is.list(entry$checks) && length(entry$checks) >= 4L,
              info = "les 4 familles de checks v1 sont héritées")
  expect_identical(entry$checks$manifest_syntax$valid, TRUE)
  expect_identical(entry$checks$matrix_semantics$valid, TRUE)
  expect_identical(entry$checks$experimental_design$valid, TRUE)
  expect_identical(entry$checks$species_and_identifiers$valid, TRUE)

  # paths résolus
  expect_identical(entry$paths$dir, .dds_p(drop))
  expect_identical(entry$paths$manifest, .dds_p(drop, "manifest.json"))
  expect_identical(entry$paths$matrix, .dds_p(drop, "counts.tsv"))
  expect_identical(entry$paths$samplesheet, .dds_p(drop, "samplesheet.tsv"))
  expect_true(file.exists(entry$paths$matrix) && file.exists(entry$paths$samplesheet),
              info = "chemins de matrice/samplesheet résolus depuis le manifeste v1")

  # résumé du manifeste
  expect_identical(entry$manifest$dataset_id, "DS_SCAN_VALID")
  expect_identical(entry$manifest$species, "human")
  expect_identical(entry$manifest$feature_id_type, "ensembl")
  expect_identical(entry$manifest$matrix_type, "raw_counts")

  # mtime présent
  expect_s3_class(entry$mtime, "POSIXct")
})

# ── T3 -- manifest absent / JSON malformé : invalid + raison v1 + isolation --

test_that("T3 manifest absent / malformé : invalid + raison v1, un drop cassé isole les autres (§5)", {
  expect_true(.dds_present, info = .dds_red_info)
  if (!.dds_present) return(invisible(NULL))

  root <- .dds_root()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  .dds_write_drop(root, "DS_SCAN_NO_MANIFEST", write_manifest = FALSE)
  .dds_write_drop(root, "DS_SCAN_BAD_JSON",
                  manifest_raw = "{ ce json est ] malformé [")
  .dds_write_drop(root, "DS_SCAN_VALID")   # témoin d'isolation (fiche §5)

  res <- ts_dataset_dir_scan(datasets_dir = root)
  expect_length(res, 3L)
  expect_identical(res[["DS_SCAN_NO_MANIFEST"]]$status, "invalid")
  expect_identical(res[["DS_SCAN_NO_MANIFEST"]]$raison, "manifest_missing",
                   info = "vocabulaire v1 tel quel (biomentat_handoff.R:156)")
  expect_identical(res[["DS_SCAN_BAD_JSON"]]$status, "invalid")
  expect_identical(res[["DS_SCAN_BAD_JSON"]]$raison, "manifest_malformed",
                   info = "vocabulaire v1 tel quel (biomentat_handoff.R:167/174)")
  # nommage de repli : pas de dataset_id lisible -> basename du sous-répertoire
  expect_true(all(c("DS_SCAN_NO_MANIFEST", "DS_SCAN_BAD_JSON", "DS_SCAN_VALID")
                  %in% names(res)),
              info = "repli de nommage sur le basename du sous-répertoire quand le manifeste est illisible")
  # échec d'un drop isolé : les autres continuent (fiche §5)
  expect_identical(res[["DS_SCAN_VALID"]]$status, "valid",
                   info = "un drop cassé n'affecte PAS les autres drops (§5)")
})

# ── T4 -- samplesheet discordante / 1 seul réplicat : invalid + raison v1 ----

test_that("T4 samplesheet discordante -> samplesheet_matrix_mismatch ; 1 réplicat -> pseudoreplication_detected (v1 tel quel)", {
  expect_true(.dds_present, info = .dds_red_info)
  if (!.dds_present) return(invisible(NULL))

  root <- .dds_root()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  # (a) samplesheet discordante : un échantillon renommé DANS LA SAMPLESHEET
  #     SEULE (la matrice garde sample_4) -> concordance rompue
  #     (biomentat_handoff.R:564-571, setequal samp_ids vs colnames matrice)
  drop_mismatch <- .dds_write_drop(
    root, "DS_SCAN_MISMATCH",
    sheet_samples = c("sample_1", "sample_2", "sample_3", "sample_9"))
  # (b) 1 seul réplicat : 3 échantillons / 2 conditions (control, control,
  #     treated) -> treated deficiente (biomentat_handoff.R:588-594)
  drop_pseudo <- .dds_write_drop(
    root, "DS_SCAN_PSEUDO",
    samples = paste0("sample_", seq_len(3L)),
    conditions = c("control", "control", "treated"),
    batch = c("b1", "b2", "b1"))

  res <- ts_dataset_dir_scan(datasets_dir = root)
  expect_length(res, 2L)
  expect_identical(res[["DS_SCAN_MISMATCH"]]$status, "invalid")
  expect_identical(res[["DS_SCAN_MISMATCH"]]$raison, "samplesheet_matrix_mismatch",
                   info = "vocabulaire v1 tel quel (biomentat_handoff.R:570)")
  expect_identical(res[["DS_SCAN_PSEUDO"]]$status, "invalid")
  expect_identical(res[["DS_SCAN_PSEUDO"]]$raison, "pseudoreplication_detected",
                   info = "vocabulaire v1 tel quel (biomentat_handoff.R:593)")
  # checks v1 hérités aussi sur l'échec (aucune règle maison ajoutée — AC1)
  expect_true(is.list(res[["DS_SCAN_MISMATCH"]]$checks),
              info = "checks v1 (structure) hérités même en échec")
  # le drop invalide reste nommé par le dataset_id lisible de son manifeste
  expect_identical(names(res)[order(names(res))],
                   c("DS_SCAN_MISMATCH", "DS_SCAN_PSEUDO"))
  expect_false(file.exists(file.path(drop_mismatch, "validation.json")))
  expect_false(file.exists(file.path(drop_pseudo, "validation.json")),
               info = "le scan ne déclenche AUCUNE écriture de validation.json (AC2)")
})

# ── T5 -- FREEZE du schéma de sortie (motif test-report-contract-freeze.R) ---

test_that("T5 freeze du schéma de sortie : noms de champs, états, vocabulaire des raisons", {
  expect_true(.dds_present, info = .dds_red_info)
  if (!.dds_present) return(invisible(NULL))

  # vocabulaire des états figé, exposé par le moteur (motif
  # biomentat_handoff_states(), biomentat_handoff.R:104)
  expect_identical(
    ts_dataset_dir_scan_states(), c("valid", "warn", "invalid", "empty"),
    info = "états AC1 figés : valid | warn | invalid | empty")

  root <- .dds_root()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  .dds_write_drop(root, "DS_SCAN_VALID")
  .dds_write_drop(root, "DS_SCAN_PSEUDO",
                  samples = paste0("sample_", seq_len(3L)),
                  conditions = c("control", "control", "treated"),
                  batch = c("b1", "b2", "b1"))
  dir.create(file.path(root, "DS_SCAN_EMPTY"))   # sous-répertoire vide
  .dds_write_drop(root, "DS_SCAN_NO_MANIFEST", write_manifest = FALSE)

  res <- ts_dataset_dir_scan(datasets_dir = root)
  expect_length(res, 4L)

  # 6 champs de premier niveau, exactement (fiche §3.2 + §1 AC1)
  for (nm in names(res)) {
    expect_identical(sort(names(res[[nm]])),
                     sort(c("status", "raison", "manifest", "paths", "mtime", "checks")),
                     info = paste0("schéma de sortie figé pour le drop ", nm,
                                   " (status, raison, manifest, paths, mtime, checks)"))
    expect_identical(sort(names(res[[nm]]$paths)),
                     sort(c("dir", "manifest", "matrix", "samplesheet")),
                     info = paste0("champs paths figés pour ", nm))
    expect_identical(sort(names(res[[nm]]$manifest)),
                     sort(c("dataset_id", "species", "feature_id_type", "matrix_type")),
                     info = paste0("champs du résumé manifeste figés pour ", nm))
    expect_true(res[[nm]]$status %in% ts_dataset_dir_scan_states(),
                info = paste0("état dans le vocabulaire figé pour ", nm))
  }

  # états observés pour les 4 formes de fixtures
  expect_identical(res[["DS_SCAN_VALID"]]$status, "valid")
  expect_identical(res[["DS_SCAN_PSEUDO"]]$status, "invalid")
  expect_identical(res[["DS_SCAN_EMPTY"]]$status, "empty")
  expect_identical(res[["DS_SCAN_EMPTY"]]$raison, "empty_directory",
                   info = "seule raison hors vocabulaire v1 : sous-répertoire sans rien à valider (documentée, AC1)")
  expect_identical(res[["DS_SCAN_NO_MANIFEST"]]$status, "invalid")

  # raison NA quand le drop est valide
  expect_true(is.na(res[["DS_SCAN_VALID"]]$raison))

  # validate = FALSE : listing brut SANS validation — status/raison NA,
  # résumé + chemins toujours portés (contrat documenté du paramètre)
  res_nov <- ts_dataset_dir_scan(datasets_dir = root, validate = FALSE)
  expect_length(res_nov, 4L)
  expect_true(is.na(res_nov[["DS_SCAN_VALID"]]$status))
  expect_true(is.na(res_nov[["DS_SCAN_VALID"]]$raison))
  expect_identical(res_nov[["DS_SCAN_VALID"]]$manifest$dataset_id, "DS_SCAN_VALID",
                   info = "validate=FALSE : le résumé manifeste reste porté (lecture seule du lecteur v1)")
  expect_identical(res_nov[["DS_SCAN_VALID"]]$paths$matrix,
                   .dds_p(root, "DS_SCAN_VALID", "counts.tsv"))
  # la validation n'a pas eu lieu : aucun état appris
  expect_true(is.na(res_nov[["DS_SCAN_PSEUDO"]]$status))
})

# ── T6 -- lecture seule (AC2) : snapshot avant/après, validation.json exclu --

test_that("T6 le scan est strictement lecture seule (AC2) : snapshot du répertoire inchangé hors validation.json", {
  expect_true(.dds_present, info = .dds_red_info)
  if (!.dds_present) return(invisible(NULL))

  root <- .dds_root()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  # un drop valide (celui qui, avec un out_dir, écrirait validation.json),
  # un drop invalide, un sous-répertoire vide : les 3 formes traversées
  .dds_write_drop(root, "DS_SCAN_VALID")
  .dds_write_drop(root, "DS_SCAN_PSEUDO",
                  samples = paste0("sample_", seq_len(3L)),
                  conditions = c("control", "control", "treated"),
                  batch = c("b1", "b2", "b1"))
  dir.create(file.path(root, "DS_SCAN_EMPTY"))

  before <- .dds_snapshot(root)

  res <- ts_dataset_dir_scan(datasets_dir = root)
  expect_length(res, 3L)   # le scan a bien traversé les 3 sous-répertoires
  expect_identical(res[["DS_SCAN_VALID"]]$status, "valid")

  after <- .dds_snapshot(root)

  # (a) validation.json n'apparaît PAS du tout : le scan ne passe pas out_dir
  #     au moteur v1 (biomentat_handoff.R:755-757) -> la seule écriture tolérée
  #     de la chaîne n'est même pas déclenchée
  expect_false(any(grepl("validation\\.json$", before$path)),
               info = "pré-condition : pas de validation.json avant le scan")
  expect_false(any(grepl("validation\\.json$", after$path)),
               info = "AUCUN validation.json écrit par le scan (AC2, out_dir jamais fourni)")

  # (b) snapshot COMPLET identique (le filtrage validation.json devient sans
  #     objet — il ne peut pas apparaître) : chemins, tailles, mtimes
  expect_identical(after, before,
                   info = paste0("aucun fichier créé/modifié/re-touché par le scan ",
                                 "(AC2 : strictement lecture seule)"))
})

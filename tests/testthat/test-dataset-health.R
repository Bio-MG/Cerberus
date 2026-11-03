# =============================================================================
# test-dataset-health.R — N5 dataset-doctor (satellites slice S2) : moteur PUR
# =============================================================================
# FICHE (fait foi) :
# docs/proposals/PROPOSAL_N_SATELLITES_LAUNCH_ROADMAP_2026-10-26.md
#   §slice S2 (N5 dataset-doctor) §1 : détecteurs sur fixtures synthétiques,
#   RED mesuré puis implémentation puis GREEN ; §52 (0 nouvelle dépendance) ;
#   §0 N5 (« a blood test before surgery »).
# Brief jalon round8 (slice S2 N5 dataset-doctor) :
#   AC2.2 — (a) matrice TPM-like (valeurs non entières, colSums ≈ 1e6 dans la
#   fenêtre [TS_BULK_TPM_COLSUM_LOW=5e5, TS_BULK_TPM_COLSUM_HIGH=5e6],
#   fraction de valeurs dans ]0;1[ >= 0.01) -> finding échelle portant le
#   verdict "tpm_fpkm_likely" DÉLÉGUÉ de bulk_counts_transformed_signature()
#   (O16 — tools/pkg_skeleton/R/batch_correction.R:156, JAMAIS ré-implémenté)
#   ET le libellé du vocabulaire bulk_provenance_known_normalizations()
#   (tools/pkg_skeleton/R/bulk_provenance.R:38) ;
#   (b) counts bruts entiers >= 0 -> verdict raw, AUCUN warn d'échelle ;
#   (c) rownames dupliqués injectés -> finding détecté ;
#   (d) matrice sans gènes ^MT- / ^mt- (motifs paramétrables) -> finding warn
#   MT, JAMAIS error ;
#   (e) contenu texte brut avec séparateur ';' ou décimales ',' -> finding
#   encodage/séparateur détecté.
#   AC2.4 — warn + chemin d'import utilisable immédiatement : le moteur n'est
#   JAMAIS bloquant (aucun stop(), aucune sévérité "error" émise par les
#   détecteurs v1 — la décision de bloquer reste opérateur, fiche §3-D3).
# =============================================================================

source_project_file("tools/pkg_skeleton/R/defaults.R")
source_project_file("tools/pkg_skeleton/R/bulk_provenance.R")
source_project_file("tools/pkg_skeleton/R/batch_correction.R")
source_project_file("tools/pkg_skeleton/R/dataset_health.R")

# ── Porte RED sur le SOURCE (motif test-dataset-dir-ui.R:57-69 : asserted on
#    the SOURCE so the test cannot survive a delegation deletion) -------------
.DH_MOD <- "tools/pkg_skeleton/R/dataset_health.R"
.dh_src <- paste(readLines(file.path(ts_project_root(), .DH_MOD),
                            warn = FALSE, encoding = "UTF-8"), collapse = "\n")
# Code sans commentaires (lignes # et suffixes inline) : le verrou de délégation
# porte sur le CODE exécutable, pas sur la prose d'en-tête (mesuré RED 2026-11-03 :
# le nom de l'O16 dans un commentaire du stub fait passer un grep naïf — faux témoin).
.dh_src_code <- paste(sub("#.*$", "", sub("^\\s*#.*$", "",
                    readLines(file.path(ts_project_root(), .DH_MOD),
                            warn = FALSE, encoding = "UTF-8"))), collapse = "\n")

# ── Fixtures synthétiques (fiche S2 §1 : "synthetic fixtures") --------------

# (a) TPM-like : ~5 % de valeurs dans ]0;1[ (>= 1 %), reste dans [10;990[
#     (continu = non entier), colonnes sommant ≈ 1e6 (fenêtre O16). SANS gène
#     MT (ids ENSG) : la fixture isole le finding d'échelle.
.dh_tpm_matrix <- function(genes = 2000L, samples = 4L, seed = 11) {
  set.seed(seed)
  n <- as.integer(genes) * as.integer(samples)
  small <- runif(n) < 0.05
  v <- ifelse(small, runif(n, 0.001, 0.9), runif(n, 10, 990))
  m <- matrix(v, nrow = as.integer(genes))
  dimnames(m) <- list(sprintf("ENSG%011d", seq_len(genes)),
                      paste0("S", seq_len(samples)))
  m
}

# (b/c/d) Counts bruts : entiers >= 0. with_mt = TRUE injecte 3 gènes
#     mitochondriaux (le rapport est alors propre) ; FALSE isole le finding
#     d'absence MT. Noms uniques, sans doublon, par défaut.
.dh_raw_matrix <- function(genes = 200L, samples = 4L, seed = 12,
                           with_mt = TRUE) {
  set.seed(seed)
  m <- matrix(rpois(as.integer(genes) * as.integer(samples), lambda = 200),
              nrow = as.integer(genes))
  rn <- sprintf("GENE%04d", seq_len(genes))
  if (with_mt) {
    rn[1L] <- "MT-CO1"; rn[2L] <- "MT-ND1"; rn[3L] <- "mt-cyb"
  }
  dimnames(m) <- list(rn, paste0("S", seq_len(samples)))
  m
}

# ── Contrat : vocabulaire figé + schéma findings (jamais NULL) --------------

test_that("contrat N5 : vocabulaire des sévérités figé et schéma findings complet", {
  expect_identical(ts_dataset_health_severities(), c("ok", "warn", "error"))
  res <- ts_dataset_health()
  expect_s3_class(res$findings, "data.frame")
  expect_identical(names(res$findings),
                   c("id", "severity", "label", "message", "verdict"))
  expect_type(res$findings$id, "character")
  expect_type(res$findings$severity, "character")
  expect_type(res$findings$label, "character")
  expect_type(res$findings$message, "character")
  expect_type(res$findings$verdict, "character")
})

# ── Porte de DÉLÉGATION (fiche S2 §1 : reuse de l'O16, jamais ré-implémenté) -

test_that("N5 : l'échelle est DÉLÉGUÉE à bulk_counts_transformed_signature (source)", {
  expect_true(grepl("bulk_counts_transformed_signature(", .dh_src_code, fixed = TRUE),
              info = paste("l'heuristique TPM/FPKM doit être déléguée à l'O16",
                           "(batch_correction.R:156), jamais ré-implémentée dans",
                           .DH_MOD, "— verrou sur le CODE, commentaires exclus"))
})

# ── AC2.2 (a) : signature TPM -> finding échelle warn, verdict délégué -------

test_that("AC2.2a : matrice TPM-like -> finding échelle warn portant le verdict tpm_fpkm_likely délégué + libellé du vocabulaire provenance", {
  m <- .dh_tpm_matrix()
  sig <- bulk_counts_transformed_signature(m)   # l'O16 tel quel (délégation mesurée)
  expect_identical(sig$verdict, "tpm_fpkm_likely",
                   info = "la fixture doit tomber dans la signature TPM de l'O16 (fenêtre TS_BULK_TPM_COLSUM_*, fraction ]0;1[ >= TS_BULK_TPM_UNIT_FRAC)")

  res <- ts_dataset_health(mat = m)
  expect_identical(res$scale_verdict, "tpm_fpkm_likely")
  expect_identical(res$scale_verdict, sig$verdict,
                   info = "le verdict du doctor EST le verdict délégué de l'O16")

  fd <- res$findings[res$findings$id == "scale", , drop = FALSE]
  expect_identical(nrow(fd), 1L,
                   info = "UN finding d'échelle (pas de ré-implémentation parallèle)")
  expect_identical(fd$severity, "warn")
  expect_identical(fd$verdict, "tpm_fpkm_likely",
                   info = "le finding porte le verdict délégué")
  expect_match(fd$message, sig$message, fixed = TRUE,
               info = "le message affiché EST le message FR actionnable délégué de l'O16")
  expect_match(fd$message, "counts bruts", fixed = TRUE,
               info = "message orienté ACTION : fournir des counts bruts")

  # libellé construit DEPUIS le vocabulaire bulk_provenance (fiche S2 §1 « reuse
  # the provenance vocabulary bulk_provenance.R:40 ») — aucune chaîne codée en dur
  expect_match(fd$label, "TPM", fixed = TRUE)
  expect_true("TPM" %in% bulk_provenance_known_normalizations())
  # garde RED-safe : label absent (stub) => expectation, jamais une erreur (règle error=0)
  parts <- if (length(fd$label) == 1L) strsplit(fd$label, " / ", fixed = TRUE)[[1L]] else character(0)
  expect_true(length(parts) > 0L)
  expect_true(all(parts %in% bulk_provenance_known_normalizations()),
              info = "chaque terme du libellé d'échelle vient du vocabulaire de provenance")
  # verdict global : au moins un warn => "warn" (jamais "error", AC2.4)
  expect_identical(res$verdict, "warn")
  expect_false(any(res$findings$severity == "error"))
})

# ── AC2.2 (b) : counts bruts -> verdict raw, AUCUN warn d'échelle ------------

test_that("AC2.2b : counts bruts entiers >= 0 -> verdict raw délégué, AUCUN warn d'échelle", {
  raw <- .dh_raw_matrix()
  res <- ts_dataset_health(mat = raw)
  expect_identical(res$scale_verdict, "raw")
  expect_identical(res$scale_verdict,
                   bulk_counts_transformed_signature(raw)$verdict)

  fdw <- res$findings[res$findings$id == "scale" &
                        res$findings$severity == "warn", , drop = FALSE]
  expect_identical(nrow(fdw), 0L,
                   info = "AUCUN warn d'échelle sur des counts bruts")
  fdo <- res$findings[res$findings$id == "scale" &
                        res$findings$severity == "ok", , drop = FALSE]
  expect_identical(nrow(fdo), 1L)
  expect_true(fdo$label %in% bulk_provenance_known_normalizations(),
              info = "le libellé « counts bruts » vient du vocabulaire de provenance")
  # rapport propre (MT présents, aucun doublon) => verdict global "ok"
  expect_identical(res$verdict, "ok")
  expect_false(any(res$findings$severity == "error"))
})

# ── AC2.2 (c) : rownames dupliqués -> finding détecté ------------------------

test_that("AC2.2c : rownames dupliqués injectés -> finding détecté (warn, message citant le gène)", {
  raw <- .dh_raw_matrix()
  duplicated_gene <- rownames(raw)[1L]
  rownames(raw)[3L] <- duplicated_gene           # doublon injecté
  res <- ts_dataset_health(mat = raw)

  dup <- res$findings[res$findings$id == "dup_rownames", , drop = FALSE]
  expect_identical(nrow(dup), 1L)
  expect_identical(dup$severity, "warn")
  expect_match(dup$message, duplicated_gene, fixed = TRUE,
               info = "le message cite le gène dupliqué (actionnable)")
  expect_identical(res$verdict, "warn")
  expect_false(any(res$findings$severity == "error"))
})

# ── AC2.2 (d) : absence de gènes MT (motifs paramétrables) -> warn, pas error -

test_that("AC2.2d : sans gène ^MT-/^mt- -> finding warn MT (jamais error) ; motifs paramétrables", {
  raw <- .dh_raw_matrix(with_mt = FALSE)   # ids ENSG : aucun motif MT
  res <- ts_dataset_health(mat = raw)

  mt <- res$findings[res$findings$id == "mt_absent", , drop = FALSE]
  expect_identical(nrow(mt), 1L)
  expect_identical(mt$severity, "warn",
                   info = "l'absence de gènes MT est un warn — JAMAIS error (AC2.4)")
  expect_false(any(res$findings$severity == "error"))
  expect_match(mt$message, "\\^MT-",
               info = "le message énonce les motifs par défaut (actionnable)")

  # paramétrable : un motif CUSTOM couvrant les ids de la fixture fait taire le finding
  res2 <- ts_dataset_health(mat = raw, mt_patterns = "^GENE")
  expect_identical(nrow(res2$findings[res2$findings$id == "mt_absent", , drop = FALSE]), 0L)
  # et un motif custom qui ne matche rien maintient le finding (paramétrage réel)
  res2b <- ts_dataset_health(mat = raw, mt_patterns = "^chrM$")
  expect_identical(nrow(res2b$findings[res2b$findings$id == "mt_absent", , drop = FALSE]), 1L)

  # motifs par défaut : la fixture AVEC gènes MT ne déclenche pas le finding
  raw_mt <- .dh_raw_matrix(with_mt = TRUE)
  res3 <- ts_dataset_health(mat = raw_mt, mt_patterns = c("^MT-", "^mt-"))
  expect_identical(nrow(res3$findings[res3$findings$id == "mt_absent", , drop = FALSE]), 0L)
})

# ── AC2.2 (e) : texte brut ';' / décimales ',' -> finding encodage/séparateur -

test_that("AC2.2e : contenu texte brut ';' ou décimales ',' -> finding encodage/séparateur ; TSV propre -> aucun", {
  txt_fr <- c("gene;S1;S2",
              "G1;1,5;2,3",
              "G2;0,4;9,1")
  res <- ts_dataset_health(text = txt_fr)

  te <- res$findings[res$findings$id == "text_encoding", , drop = FALSE]
  expect_identical(nrow(te), 1L)
  expect_identical(te$severity, "warn")
  expect_match(te$message, ";", fixed = TRUE)
  expect_match(te$message, ",", fixed = TRUE,
               info = "le message nomme le séparateur ';' ET les décimales ',' détectés")
  expect_identical(res$verdict, "warn")
  expect_false(any(res$findings$severity == "error"))

  # séparateur seul (sans décimale ',') -> finding aussi (export Excel FR typique)
  res2 <- ts_dataset_health(text = c("gene;S1;S2", "G1;15;23"))
  expect_identical(nrow(res2$findings[res2$findings$id == "text_encoding", , drop = FALSE]), 1L)

  # contenu propre (TSV, point décimal) -> AUCUN finding texte
  res3 <- ts_dataset_health(text = c("gene\tS1\tS2", "G1\t1.5\t2.3", "G2\t0.4\t9.1"))
  expect_identical(nrow(res3$findings[res3$findings$id == "text_encoding", , drop = FALSE]), 0L)
})

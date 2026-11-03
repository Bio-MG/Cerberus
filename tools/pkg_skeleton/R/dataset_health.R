# =============================================================================
# tools/pkg_skeleton/R/dataset_health.R — « dataset doctor » N5 (satellites
# slice S2, fiche docs/proposals/
# PROPOSAL_N_SATELLITES_LAUNCH_ROADMAP_2026-10-26.md §slice S2, jalon round8)
# =============================================================================
# PRINCIPE (« a blood test before surgery », fiche §0 N5) : aujourd'hui une
# matrice transformée n'est découverte qu'au lancement de l'analyse (garde
# aval bulk_assert_raw_counts, piège mesuré du 2026-10-24 —
# docs/audits/COUNTS_NON_INTEGER_2026-10-24.md). Le doctor pose UN diagnostic
# UNIFIÉ DÈS L'IMPORT, en log pure, à partir :
#   - d'une matrice (gènes x échantillons) : échelle, doublons de rownames,
#     gènes mitochondriaux ;
#   - d'un contenu texte brut (lignes lues telles quelles) : anomalies de
#     séparateur / décimales / encodage (export FR d'Excel).
#
# Rails (motif dataset_directory.R:27-45, règle maison « purity rails ») :
#   * NO shiny, NO reactive, NO session — ce fichier doit sourcer() partout où
#     tourne un process R pur ;
#   * ÉCHELLE = DÉLÉGATION (fiche S2 §1 : « reuse ») : l'heuristique
#     TPM/FPKM/CPM n'est JAMAIS ré-implémentée ici — le verdict vient de
#     bulk_counts_transformed_signature() (O16, batch_correction.R:156) et le
#     libellé du VOCABULAIRE bulk_provenance_known_normalizations()
#     (bulk_provenance.R:38). Zéro nouvelle règle de détection d'échelle ;
#   * JAMAIS BLOQUANT (AC2.4 du jalon round8) : le doctor rend un RAPPORT
#     (findings data.frame {id, severity, label, message, verdict}) ; aucun
#     stop(), aucune exception — un détecteur qui plante ne prévient de rien.
#     Les sévérités produites par les détecteurs v1 sont "ok"|"warn" ; la
#     sévérité "error" existe au contrat (vocabulaire figé) mais n'est pas
#     émise par ces détecteurs — bloquer reste une décision opérateur (fiche
#     S2 §2 / §3-D3) ;
#   * messages FR ACTIONNABLES (fiche S2 §2 : « what to DO, not what failed »)
#     — style « ⛔ Valeurs TPM détectées — l'analyse différentielle requiert
#     des counts bruts ; ré-exportez depuis Salmon/tximport » ;
#   * base R only (fiche S2 §52 : 0 nouvelle dépendance).
#
# ÉTAT (GREEN, round8 jalon 2 — RED mesuré avant implémentation : FAIL 29 /
# PASS 20 / 0 erreur, transcript test-dataset-health.R du 2026-11-03) :
# détecteurs implémentés en base R pur — l'échelle reste une DÉLÉGATION (le
# verdict ET le message viennent de l'O16 verbatim), tout le reste est de la
# détection structurelle triviale (duplicated, grepl sur rownames, grepl sur
# le texte brut).
# =============================================================================

#' Vocabulaire figé des sévérités du rapport de santé (AC2.2, jalon round8).
#'
#' Motif `ts_dataset_dir_scan_states()` (dataset_directory.R:98) : le
#' vocabulaire est exposé pour être figé par le test (freeze test). "error"
#' existe au contrat mais AUCUN détecteur v1 ne l'émet — bloquer l'import est
#' une décision opérateur (AC2.4 : warn + proceed, jamais de stop()).
#'
#' @return Vecteur character c("ok", "warn", "error").
ts_dataset_health_severities <- function() {
  c("ok", "warn", "error")
}

# Cadre vide du contrat findings : schéma FIGÉ (jamais NULL, pour un
# consumeur UI/JSON homogène — motif .ts_datasets_field(), dataset_directory.R:107).
# `verdict` porte le verdict DÉLÉGUÉ (échelle : verdict O16 brut ; NA ailleurs).
.ts_health_empty_findings <- function() {
  data.frame(
    id       = character(0),
    severity = character(0),
    label    = character(0),
    message  = character(0),
    verdict  = character(0),
    stringsAsFactors = FALSE
  )
}

# Libellé d'échelle : dérivé du VOCABULAIRE bulk_provenance_known_
# normalizations() (bulk_provenance.R:38) — JAMAIS une chaîne codée en dur
# hors vocabulaire (fiche S2 §1 : « reuse the provenance vocabulary »). Si le
# vocabulaire ne porte plus le terme attendu, le libellé devient NA (le test
# de délégation échouera bruyamment — c'est voulu, pas un repli silencieux).
.ts_health_scale_label <- function(verdict) {
  vocab <- bulk_provenance_known_normalizations()
  if (identical(verdict, "raw")) {
    lab <- intersect(vocab, "Counts bruts")
    return(if (length(lab)) lab else NA_character_)
  }
  if (identical(verdict, "tpm_fpkm_likely")) {
    lab <- intersect(vocab, c("TPM", "FPKM"))
    return(paste(lab, collapse = " / "))
  }
  if (identical(verdict, "transformed_generic")) {
    lab <- intersect(vocab, c("VST (DESeq2)", "log2-CPM", "TMM log2-CPM (edgeR)"))
    return(paste(lab, collapse = " / "))
  }
  NA_character_
}

# Doublons de rownames (AC2.2c) : détection structurelle triviale
# duplicated(rownames()) — aucun finding si pas de noms ou aucun doublon.
.ts_health_dup_rownames_finding <- function(mat) {
  rn <- rownames(mat)
  if (is.null(rn) || length(rn) == 0L || !anyDuplicated(rn)) {
    return(.ts_health_empty_findings())
  }
  dups <- unique(rn[duplicated(rn)])
  data.frame(
    id       = "dup_rownames",
    severity = "warn",
    label    = "Noms de gènes dupliqués",
    message  = paste0(
      "⛔ ", length(dups), " nom(s) de gène(s) dupliqué(s) dans la matrice (ex. : ",
      paste(head(dups, 3L), collapse = ", "), ") — les agrégations par gène et les ",
      "tests différentiels seraient faussés : dédoublonnez/renommez les lignes ",
      "AVANT l'import."),
    verdict  = NA_character_,
    stringsAsFactors = FALSE
  )
}

# Gènes mitochondriaux (AC2.2d) : motifs PARAMÉTRABLES (défaut ^MT- / ^mt-) ;
# absence = "warn" (jamais "error" — attendu pour certaines espèces), présence
# = simple constat "ok".
.ts_health_mt_finding <- function(mat, mt_patterns = c("^MT-", "^mt-")) {
  rn <- rownames(mat)
  if (is.null(rn) || length(rn) == 0L) {
    return(.ts_health_empty_findings())
  }
  if (!is.character(mt_patterns) || length(mt_patterns) == 0L ||
      all(!nzchar(mt_patterns))) {
    return(.ts_health_empty_findings())   # motifs vides = détection éteinte (paramétrable)
  }
  hits <- rn[grepl(paste(mt_patterns, collapse = "|"), rn)]
  if (length(hits) > 0L) {
    return(data.frame(
      id       = "mt",
      severity = "ok",
      label    = "Gènes mitochondriaux",
      message  = paste0("✓ ", length(hits), " gène(s) mitochondrial/aux détecté(s) ",
                        "(motifs ", paste(mt_patterns, collapse = " | "), ") — ",
                        "métriques QC mito disponibles."),
      verdict  = NA_character_,
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    id       = "mt_absent",
    severity = "warn",
    label    = "Aucun gène mitochondrial",
    message  = paste0(
      "⚠️ Aucun gène mitochondrial détecté (motifs ", paste(mt_patterns, collapse = " / "),
      ") — vérifiez l'espèce et l'annotation : les métriques QC mito (pourcentage MT) ",
      "seront absentes. Si c'est attendu (ex. plante, ERCC seul), ignorez ce signal."),
    verdict  = NA_character_,
    stringsAsFactors = FALSE
  )
}

# Texte brut (AC2.2e) : anomalies de séparateur (';'), de décimales (',') et
# d'encodage (BOM en tête) — export FR d'Excel typique. Le contenu "propre"
# (TSV, point décimal) ne produit AUCUN finding.
.ts_health_text_finding <- function(text) {
  if (is.null(text)) return(.ts_health_empty_findings())
  txt <- paste(as.character(text), collapse = "\n")
  if (!nzchar(txt)) return(.ts_health_empty_findings())
  issues <- character(0)
  if (grepl(";", txt, fixed = TRUE)) {
    issues <- c(issues, "séparateur « ; » (attendu : tabulation ou « , »)")
  }
  if (grepl("[0-9],[0-9]", txt)) {
    issues <- c(issues, "décimales à virgule « , » (attendu : point)")
  }
  if (startsWith(txt, "\ufeff")) {
    issues <- c(issues, "BOM UTF-8 en tête de fichier")
  }
  if (length(issues) == 0L) return(.ts_health_empty_findings())
  data.frame(
    id       = "text_encoding",
    severity = "warn",
    label    = "Format du fichier texte",
    message  = paste0(
      "⛔ Format texte suspect : ", paste(issues, collapse = " ; "), ". ",
      "Export FR d'Excel probable — ré-exportez en CSV/TSV UTF-8 (séparateur ",
      "tabulation ou « , », point décimal) avant l'import, sinon les colonnes ",
      "seront mal découpées."),
    verdict  = NA_character_,
    stringsAsFactors = FALSE
  )
}

#' Rapport de santé d'un dataset bulk (N5 dataset-doctor, satellites slice S2).
#'
#' PURE, non bloquante (AC2.4 : warn + chemin d'import utilisable immédiatement
#' — aucun stop(), aucune exception ; entrée invalide => findings partiel ou
#' vide, jamais une erreur). Détecteurs (fiche S2 §1) :
#'   - échelle  : DÉLÉGATION bulk_counts_transformed_signature() (O16,
#'     batch_correction.R:156) — verdict ET message repris verbatim, l'heuristique
#'     TPM/FPKM/CPM n'est PAS ré-implémentée ici ; libellé dérivé du vocabulaire
#'     bulk_provenance_known_normalizations() ;
#'   - doublons : duplicated(rownames()) — warn citant les gènes en cause ;
#'   - MT       : motifs paramétrables (défaut ^MT- / ^mt-), absence = warn
#'     (jamais error — attendu pour certaines espèces) ;
#'   - texte    : séparateur ';' / décimales ',' / BOM (export FR d'Excel).
#' Le verdict global est la sévérité max constatée ("ok"|"warn"|"error") ;
#' les détecteurs v1 n'émettent que "ok"/"warn" — bloquer reste une décision
#' opérateur (fiche §3-D3).
#'
#' @param mat Matrice numérique (gènes x échantillons) ou NULL.
#' @param text Contenu texte brut (character, lignes) ou NULL.
#' @param mt_patterns Motifs regex des gènes mitochondriaux (paramétrable).
#' @return list(findings = data.frame(id, severity, label, message, verdict),
#'   verdict = "ok"|"warn"|"error", scale_verdict = verdict O16 délégué ou NA).
ts_dataset_health <- function(mat = NULL, text = NULL,
                              mt_patterns = c("^MT-", "^mt-")) {
  findings <- .ts_health_empty_findings()
  scale_verdict <- NA_character_

  # 1) ÉCHELLE — DÉLÉGATION O16 (fiche S2 §1 : reuse, zéro ré-implémentation)
  if (!is.null(mat)) {
    sig <- tryCatch(bulk_counts_transformed_signature(mat),
                    error = function(e) NULL)
    if (!is.null(sig) && !identical(sig$verdict, "unknown")) {
      scale_verdict <- sig$verdict
      if (identical(sig$verdict, "raw")) {
        findings <- rbind(findings, data.frame(
          id       = "scale",
          severity = "ok",
          label    = .ts_health_scale_label("raw"),
          message  = paste0("✓ ", sig$message, " — aucune conversion à faire."),
          verdict  = sig$verdict,
          stringsAsFactors = FALSE
        ))
      } else {
        findings <- rbind(findings, data.frame(
          id       = "scale",
          severity = "warn",
          label    = .ts_health_scale_label(sig$verdict),
          message  = sig$message,   # message FR actionnable délégué, verbatim
          verdict  = sig$verdict,
          stringsAsFactors = FALSE
        ))
      }
    }
    findings <- rbind(findings, .ts_health_dup_rownames_finding(mat))
    findings <- rbind(findings, .ts_health_mt_finding(mat, mt_patterns))
  }

  # 2) TEXTE BRUT (indépendant de la matrice : un fichier peut être inspecté seul)
  if (!is.null(text)) {
    findings <- rbind(findings, .ts_health_text_finding(text))
  }

  # 3) Verdict global = sévérité max constatée (jamais bloquant par soi-même)
  verdict <- "ok"
  if (any(findings$severity == "warn")) verdict <- "warn"
  if (any(findings$severity == "error")) verdict <- "error"

  list(findings = findings, verdict = verdict, scale_verdict = scale_verdict)
}

# =============================================================================
# R/sc/sc_abundance_sccoda.R — Abondance differentielle 4E-2 : scCODA (Stage 15)
# =============================================================================
# But : DA COMPOSITIONNELLE au niveau ECHANTILLON (modele bayesien
# spike-and-slab avec identite de reference declaree), DISTINCTE de Milo
# (4E-1, niveau voisinage) — les deux identites de resultats ne se melangent
# jamais. GATED sur le design Stage 13 (assert_da_design_result("sccoda")).
#
# Principe central : l'unite de composition est l'ECHANTILLON ; les cellules
# ne sont PAS des replicats biologiques. Le modele exige une identite de
# REFERENCE declaree explicitement (supposee inchangee entre conditions) —
# politique par defaut "identite la plus abondante", toujours enregistree et
# modifiable. Le modele porte sur les DEUX conditions du contraste choisi
# (echantillons de la paire ; echantillons hors contraste exclus et comptes).
#
# Couche d'integration Python (jamais de repli silencieux) :
#   - detection EXPLICITE de l'environnement (resolveur injectable pour les
#     tests) : variable TS_SCCODA_PYTHON, venv projet python_env_sccoda/,
#     python reticulate par defaut ;
#   - absence de reticulate/Python/sccoda = ERREUR classee environment_missing
#     avec guidance d'installation — AUCUN calcul de substitution ;
#   - versions Python/sccoda/tensorflow/arviz enregistrees dans la provenance.
#
# Pur domaine : aucune reactivite Shiny. Sourced in app.R AFTER
# R/sc/sc_abundance_design.R et AFTER R/sc/sc_abundance_milo.R. Constantes
# TS_DA_SCCODA_* definies dans config/defaults.R.
#
# ── FLUX (Stage 15) ─────────────────────────────────────────────────────────
# objet Seurat valide + design Stage 13 eligible ("sccoda")
#   -> preparation : matrice de composition echantillon x identite (colonne
#      identite du design OBLIGATOIRE ; cellules sans identite comptees a
#      part, jamais fusionnees), echantillons restreints a la paire de
#      conditions du contraste
#   -> identite de reference explicite
#   -> modele scCODA via reticulate (HMC tensorflow, formule
#      "condition [+ batch]", categories triees -> reference de condition =
#      premiere par ordre alphabetique, coefficient reorienté en
#      cible vs reference)
#   -> diagnostics de convergence (az r_hat/ESS ; r_hat indisponible pour une
#      chaine unique = note, jamais masque) — echec => AUCUN resultat
#   -> resultat canonique finalize_sccoda_result() + provenance produite ici.
#
# ── CONTRAT DE RESULTAT CANONIQUE (Stage 15) ────────────────────────────────
#   type                    "sccoda_da" (constant)
#   status                  etat de validite (sccoda_validity_states())
#   compositional_unit      "sample" — l'unite de composition est EXPLICITE
#   design                  recapitulatif du design Stage 13 consomme
#   parameters              parametres TOUT enregistres (colonnes, contraste,
#                           reference, MCMC, seuil FDR, exclusions)
#   model_specification     list(formula, condition_base, reference_policy,
#                           reference_identity, num_results, num_burnin,
#                           fdr_target, backend "tensorflow HMC", seed)
#   convergence_diagnostics  list(rhat_max, ess_min, n_divergences, acc_rate,
#                           num_results, num_burnin, notes)
#   composition_table       data.frame echantillon x identite (comptages) +
#                           condition/batch — la matrice ANALYSEE
#   effect_table            data.frame par covariable x identite (hors
#                           reference) : covariate, identity, effect (cible vs
#                           reference), hdi_low, hdi_high, sd,
#                           inclusion_probability, log2_fold_change, credible
#   credible_effects        vecteur character des identites credibles pour la
#                           covariable condition (seuil fdr_target) —
#                           "credible au sens bayesien", PAS une p-value
#   reference_identity      identite de reference utilisee
#   package_versions        list(python, sccoda, tensorflow,
#                           tensorflow_probability, arviz, numpy, pandas, R)
#   object_identity         list(fingerprint, method, seurat_dims) — empreinte
#                           v2 REUTILISEE (build_object_identity_v2)
#   warnings                vecteur character (design + calcul, prefixes)
#   provenance              entree new_provenance_entry() enrichie
#                           (analysis_type = "sccoda_da")
#   analysis_id             "sc-da-sccoda"
#   timestamp_utc           horodatage UTC ISO-8601
#
# ── ETATS DE VALIDITE ───────────────────────────────────────────────────────
#   valid / valid_with_warnings / invalid_input / environment_missing /
#   compute_failed / convergence_failure / design_not_eligible /
#   stale_against_current_seurat_object (libelles : sccoda_status_labels())
#
# ── API PUBLIQUE FIGEE (Stage 15) ───────────────────────────────────────────
# sccoda_public_api() ; test de freeze
# (tests/testthat/test-sccoda-contract-freeze.R). Helpers internes prefixes
# d'un point (.sccoda_*).
# =============================================================================

#' Champs documentes du contrat de resultat scCODA canonique
#'
#' Source de verite partagee par le code, le test de freeze et
#' docs/contracts/SCCODA_RESULT_CONTRACT.md — toute modification passe par les
#' TROIS simultanement.
#'
#' @return Vecteur character des 17 champs contractuels.
#' @export
sccoda_contract_fields <- function() {
  c(
    "type", "status", "compositional_unit", "design", "parameters",
    "model_specification", "convergence_diagnostics", "composition_table",
    "effect_table", "credible_effects", "reference_identity",
    "package_versions", "object_identity", "warnings", "provenance",
    "analysis_id", "timestamp_utc"
  )
}

# Etats de validite (source de verite ; l'accesseur public est l'unique
# lecture autorisee ailleurs).
.SCCODA_STATUS_STATES <- c(
  "valid",
  "valid_with_warnings",
  "invalid_input",
  "environment_missing",
  "compute_failed",
  "convergence_failure",
  "design_not_eligible",
  "stale_against_current_seurat_object"
)

#' Etats de validite du contrat scCODA
#'
#' @return Vecteur character des 8 etats documentes.
#' @export
sccoda_validity_states <- function() .SCCODA_STATUS_STATES

#' Libelles francais des etats de validite (affichage utilisateur)
#'
#' @return Vecteur character nomme par etat.
#' @export
sccoda_status_labels <- function() {
  c(
    valid = paste0(
      "Analyse scCODA terminee : DA compositionnelle au niveau ECHANTILLON ",
      "(unite de composition = echantillon). Les effets sont des INTERVALLES ",
      "DE CREDIBILITE bayesiens — pas des p-values."
    ),
    valid_with_warnings = paste0(
      "Analyse scCODA terminee avec avertissements : lisez les avertissements ",
      "(design, MCMC, effectifs) avant toute interpretation."
    ),
    invalid_input = paste0(
      "Entree invalide : objet/design/identite de reference inexploitables — ",
      "aucun resultat scCODA n'est produit."
    ),
    environment_missing = paste0(
      "Environnement scCODA indisponible : Python + le package 'sccoda' ",
      "(via reticulate) sont requis. Aucun repli silencieux n'est effectue. ",
      "Installation : creez un environnement Python avec sccoda ",
      "(python -m venv python_env_sccoda ; pip install sccoda tensorflow-",
      "probability tf-keras \"arviz==0.21.0\" \"matplotlib==3.9.4\") puis ",
      "renseignez la variable d'environnement TS_SCCODA_PYTHON — voir le ",
      "contrat SCCODA_RESULT_CONTRACT.md."
    ),
    compute_failed = paste0(
      "Echec du calcul scCODA : le modele n'a pas produit de resultats ",
      "exploitables — aucun resultat partiel n'est expose."
    ),
    convergence_failure = paste0(
      "Echec de convergence MCMC : les diagnostics (Rhat/ESS) depassent les ",
      "seuils — les effets ne sont PAS interpretables. Relancez avec plus ",
      "d'iterations (TS_DA_SCCODA_NUM_RESULTS / TS_DA_SCCODA_NUM_BURNIN) ou ",
      "examinez le design."
    ),
    design_not_eligible = paste0(
      "Design non eligible : scCODA exige un design Stage 13 valide ",
      "(replicats biologiques suffisants, deux conditions au moins, colonne ",
      "identite declaree) — un design bloque n'est JAMAIS consommable."
    ),
    stale_against_current_seurat_object = paste0(
      "Perime : le resultat scCODA (ou son design) correspond a un objet ",
      "Seurat anterieur — relancez l'analyse."
    )
  )
}

#' Erreur scCODA avec etat structure
.sccoda_stop <- function(state, message) {
  stop(errorCondition(
    message,
    state = state,
    class = "sccoda_error"
  ))
}

#' Etat de validite associe a une erreur scCODA
#'
#' @param e Condition (erreur) capturee.
#' @return Chaine d'etat structuree, NA_character_ pour une erreur sans etat.
#' @export
# §14.1 : délégation à l'accesseur générique. Famille F1 MESURÉE — la CLASSE
# est vérifiée : l'état d'une erreur d'un autre domaine rend NA.
sccoda_error_state <- function(e) ts_error_state(e, "sccoda_error")

#' Champ vide ? (NA, "" ou whitespace)
.sccoda_is_blank <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

#' Recapitulatif du design Stage 13 consomme par scCODA
#'
#' Delegation a da_design_recap() (Stage 13, partage avec Milo) — le corps
#' n'est PAS duplique entre les deux methodes.
.sccoda_design_recap <- function(da_design_result) {
  da_design_recap(da_design_result)
}

# ── Detection d'environnement (resolveur injectable pour les tests) ────────
# Racine projet : l'app tourne avec cwd = racine (regle environnement), mais
# testthat tourne dans tests/testthat — on remonte jusqu'a app.R (marqueur)
# pour resoudre les chemins relatifs de facon robuste.
.sccoda_project_root <- function(start = getwd(), max_up = 8L) {
  d <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in seq_len(max_up)) {
    if (file.exists(file.path(d, "app.R"))) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  normalizePath(start, winslash = "/", mustWork = FALSE)
}

# Probe SANS effet de bord de session : system2("python", "-c import...").
# La liaison reticulate reelle ne se produit que dans le calcul.
.sccoda_probe_command <- function(python_path) {
  out <- suppressWarnings(system2(
    python_path,
    c("-c", shQuote("import sys, sccoda; print(sys.version.split()[0]); print(sccoda.__version__)")),
    stdout = TRUE, stderr = FALSE
  ))
  status <- attr(out, "status") %||% 0L
  if (!identical(as.integer(status), 0L) || length(out) < 2L) return(NULL)
  list(python_version = trimws(out[1L]), sccoda_version = trimws(out[2L]))
}

#' Resolveur par defaut de l'environnement scCODA
#'
#' Ordre : (1) variable d'environnement TS_SCCODA_PYTHON (executable Python
#' explicite), (2) venv projet 'python_env_sccoda' (Scripts/python.exe sous
#' Windows, bin/python sinon), (3) python reticulate par defaut. Aucun
#' binding de session R a cette etape — la liaison reticulate reelle ne se
#' produit qu'au calcul.
.sccoda_default_env_resolver <- function() {
  candidates <- character(0)
  sources <- character(0)
  env_path <- Sys.getenv("TS_SCCODA_PYTHON", unset = "")
  if (nzchar(env_path)) {
    candidates <- c(candidates, env_path)
    sources <- c(sources, "TS_SCCODA_PYTHON")
  }
  venv_python <- file.path(.sccoda_project_root(), "python_env_sccoda",
                           if (.Platform$OS.type == "windows") {
                             file.path("Scripts", "python.exe")
                           } else {
                             file.path("bin", "python")
                           })
  if (file.exists(venv_python)) {
    candidates <- c(candidates, venv_python)
    sources <- c(sources, "venv projet python_env_sccoda")
  }
  for (i in seq_along(candidates)) {
    probe <- .sccoda_probe_command(candidates[i])
    if (!is.null(probe)) {
      return(list(
        available = TRUE,
        python_path = candidates[i],
        source = sources[i],
        python_version = probe$python_version,
        sccoda_version = probe$sccoda_version
      ))
    }
  }
  if (requireNamespace("reticulate", quietly = TRUE)) {
    py <- tryCatch(reticulate::py_config()$python, error = function(e) NULL)
    if (!is.null(py) && file.exists(py)) {
      probe <- .sccoda_probe_command(py)
      if (!is.null(probe)) {
        return(list(
          available = TRUE,
          python_path = py,
          source = "reticulate (python par defaut)",
          python_version = probe$python_version,
          sccoda_version = probe$sccoda_version
        ))
      }
    }
  }
  list(
    available = FALSE,
    python_path = NA_character_,
    source = NA_character_,
    python_version = NA_character_,
    sccoda_version = NA_character_,
    error = paste0(
      "Aucun Python avec le package 'sccoda' detecte (TS_SCCODA_PYTHON, ",
      "venv projet 'python_env_sccoda', python reticulate par defaut). ",
      "Installation : python -m venv python_env_sccoda puis pip install ",
      "sccoda tensorflow-probability tf-keras \"arviz==0.21.0\" ",
      "\"matplotlib==3.9.4\" — voir le contrat SCCODA_RESULT_CONTRACT.md."
    )
  )
}

#' scCODA est-il disponible ? (detection explicite, resolveur injectable)
#'
#' @param resolver Fonction de detection injectable (tests) ; NULL = resolveur
#'   par defaut.
#' @param stop_if_missing Logical — lever une erreur classee environment_missing.
#' @return list(available, python_path, source, python_version, sccoda_version,
#'   [error]) ; erreur classee sccoda_error si absent et stop_if_missing.
#' @export
sccoda_available <- function(resolver = NULL, stop_if_missing = FALSE) {
  if (is.null(resolver)) resolver <- .sccoda_default_env_resolver
  env <- resolver()
  if (!is.list(env) || is.null(env$available)) {
    .sccoda_stop(
      "invalid_input",
      "sccoda_available() : le resolveur d'environnement doit retourner une list(available, ...)."
    )
  }
  if (!isTRUE(env$available) && isTRUE(stop_if_missing)) {
    .sccoda_stop("environment_missing",
                 env$error %||% "Environnement scCODA indisponible.")
  }
  env
}

# ── Preparation (pure R — testable sans Python) ─────────────────────────────
#' Matrice de composition echantillon x identite (pure, testable sans Python)
#'
#' Construit la table ANALYSEE a partir des metadonnees : une ligne par
#' echantillon, comptages par identite, plus condition et batch. Les cellules
#' sans sample_id sont exclues et comptees ; les cellules sans identite sont
#' comptees dans une colonne technique "sans_identite" (JAMAIS fusionnees
#' avec une identite reelle, et EXCLUES du modele).
#'
#' @param metadata data.frame de metadonnees de cellules (rownames = barcodes).
#' @param sample_id,condition,batch,identity Noms de colonnes (du design).
#' @param context Contexte cite dans les messages d'erreur.
#' @return list(counts (data.frame echantillon x identite), covariates
#'   (data.frame echantillon x condition [+ batch]), n_cells_excluded,
#'   n_cells_missing_identity).
#' @export
sccoda_composition_matrix <- function(metadata, sample_id, condition,
                                      batch = NULL, identity,
                                      context = "preparation scCODA") {
  if (is.null(metadata) || !is.data.frame(metadata) || nrow(metadata) == 0L) {
    .sccoda_stop(
      "invalid_input",
      sprintf("Echec %s : metadonnees de cellules non vides requises.", context)
    )
  }
  required <- c(sample_id = sample_id, condition = condition,
                identity = identity)
  missing_cols <- required[!required %in% colnames(metadata)]
  if (length(missing_cols)) {
    .sccoda_stop(
      "invalid_input",
      sprintf(
        paste0("Echec %s : colonne(s) requise(s) absente(s) des metadonnees : %s."),
        context, paste(missing_cols, collapse = ", ")
      )
    )
  }
  sid <- as.character(metadata[[sample_id]])
  keep <- !.sccoda_is_blank(sid)
  n_excluded <- sum(!keep)
  meta <- metadata[keep, , drop = FALSE]
  sid <- sid[keep]
  ident_raw <- as.character(meta[[identity]])
  miss_ident <- .sccoda_is_blank(ident_raw)
  n_missing_identity <- sum(miss_ident)
  ident <- ident_raw
  ident[miss_ident] <- "sans_identite"
  conds <- as.character(meta[[condition]])
  bat <- if (!is.null(batch) && !is.na(batch) && nzchar(batch)) {
    as.character(meta[[batch]])
  } else NULL

  samples <- unique(sid)
  identities_all <- sort(unique(ident))
  counts_all <- as.data.frame(
    matrix(0L, nrow = length(samples), ncol = length(identities_all),
           dimnames = list(samples, identities_all)),
    stringsAsFactors = FALSE
  )
  covariates <- data.frame(
    sample = samples,
    condition = vapply(samples, function(s) {
      cv <- unique(conds[sid == s])
      if (length(cv) == 1L) cv else NA_character_
    }, character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
  if (!is.null(bat)) {
    covariates$batch <- vapply(samples, function(s) {
      bv <- unique(bat[sid == s])
      if (length(bv) == 1L) bv else NA_character_
    }, character(1), USE.NAMES = FALSE)
  }
  for (s in samples) {
    idx <- which(sid == s)
    tab <- table(factor(ident[idx], levels = identities_all))
    counts_all[s, ] <- as.integer(tab)
  }
  # Colonne technique separee (modele : identites reelles uniquement).
  tech <- counts_all$sans_identite %||% NULL
  counts <- counts_all[, setdiff(colnames(counts_all), "sans_identite"),
                       drop = FALSE]
  list(
    counts = counts,
    counts_technical = if (is.null(tech)) NULL else tech,
    covariates = covariates,
    n_cells_excluded = as.integer(n_excluded),
    n_cells_missing_identity = as.integer(n_missing_identity)
  )
}

#' Choisir l'identite de reference selon une politique declaree
#'
#' @param counts data.frame echantillon x identite (comptages).
#' @param identity_value Identite choisie explicitement, ou NULL/NA pour
#'   appliquer la politique.
#' @param policy "most_abundant" (defaut) — la plus abondante toutes
#'   echantillons confondus. Toute autre politique est une erreur.
#' @param context Contexte cite dans les messages d'erreur.
#' @return Chaine (nom d'identite).
#' @export
sccoda_reference_identity <- function(counts, identity_value = NULL,
                                      policy = "most_abundant",
                                      context = "reference scCODA") {
  identities <- setdiff(colnames(counts), "sans_identite")
  if (length(identities) < 2L) {
    .sccoda_stop(
      "invalid_input",
      sprintf(
        paste0("Echec %s : au moins deux identites exploitables sont ",
               "requises pour un modele compositionnel (recu : %d)."),
        context, length(identities)
      )
    )
  }
  if (!is.null(identity_value) && !is.na(identity_value) &&
      nzchar(identity_value)) {
    if (!identity_value %in% identities) {
      .sccoda_stop(
        "invalid_input",
        sprintf(
          paste0("Echec %s : l'identite de reference '%s' n'existe pas ",
                 "(identites : %s)."),
          context, identity_value, paste(identities, collapse = ", ")
        )
      )
    }
    return(as.character(identity_value))
  }
  if (!identical(policy, "most_abundant")) {
    .sccoda_stop(
      "invalid_input",
      sprintf("Echec %s : politique de reference inconnue '%s'.", context, policy)
    )
  }
  totals <- colSums(counts[, identities, drop = FALSE])
  as.character(names(which.max(totals)))
}

# ── Diagnostics de convergence (pur R — testable sans Python) ───────────────
#' Evaluer la convergence MCMC (pure, testable sans Python)
#'
#' Calibrage empirique (backend HMC tensorflow, chaine unique) : l'ESS des
#' effets est structurellement deprimee par le prior spike-and-slab (sauts
#' discrets des indicateurs) — ESS < TS_DA_SCCODA_ESS_FAIL est un ECHEC
#' (posteriorite inutilisable), ESS < TS_DA_SCCODA_ESS_MIN un AVERTISSEMENT.
#' r_hat est indisponible pour une chaine unique et les divergences ne
#' s'appliquent qu'a NUTS : valeurs NA = NOTE explicite, jamais un echec
#' silencieux ni un blocage artificiel.
#'
#' @param diagnostics list(rhat_max, ess_min, n_divergences, acc_rate).
#' @param rhat_threshold Seuil Rhat max (TS_DA_SCCODA_RHAT_MAX).
#' @param ess_fail_threshold ESS en dessous de laquelle le resultat est REFUSE
#'   (TS_DA_SCCODA_ESS_FAIL).
#' @param ess_warning_threshold ESS en dessous de laquelle un avertissement
#'   est produit (TS_DA_SCCODA_ESS_MIN).
#' @param divergence_threshold Divergences max (TS_DA_SCCODA_MAX_DIVERGENCES).
#' @return list(converged, failures, warnings_suggested, notes).
#' @export
sccoda_convergence_assessment <- function(diagnostics,
                                          rhat_threshold = TS_DA_SCCODA_RHAT_MAX,
                                          ess_fail_threshold = TS_DA_SCCODA_ESS_FAIL,
                                          ess_warning_threshold = TS_DA_SCCODA_ESS_MIN,
                                          divergence_threshold = TS_DA_SCCODA_MAX_DIVERGENCES) {
  if (!is.list(diagnostics)) {
    .sccoda_stop(
      "invalid_input",
      "sccoda_convergence_assessment() : diagnostics attendus sous forme de list()."
    )
  }
  rhat <- suppressWarnings(as.numeric(diagnostics$rhat_max %||% NA_real_))
  ess <- suppressWarnings(as.numeric(diagnostics$ess_min %||% NA_real_))
  ndiv <- suppressWarnings(as.numeric(diagnostics$n_divergences %||% NA_real_))
  failures <- character(0)
  warn_suggested <- character(0)
  notes <- character(0)
  if (is.finite(rhat) && rhat > rhat_threshold) {
    failures <- c(failures, sprintf(
      "Rhat maximal %.3f > %.2f : les chaines ne sont pas melangees.",
      rhat, rhat_threshold
    ))
  }
  if (is.finite(ess) && ess < ess_fail_threshold) {
    failures <- c(failures, sprintf(
      "ESS minimal %.0f < %.0f : posteriorite inutilisable (chaine quasi stationnaire).",
      ess, ess_fail_threshold
    ))
  } else if (is.finite(ess) && ess < ess_warning_threshold) {
    warn_suggested <- c(warn_suggested, sprintf(
      paste0("ESS minimal %.0f < %.0f : le prior spike-and-slab deprime ",
             "l'ESS du HMC — interpretation prudente des effets."),
      ess, ess_warning_threshold
    ))
  }
  if (is.finite(ndiv) && ndiv > divergence_threshold) {
    failures <- c(failures, sprintf(
      "%d divergences NUTS (> %d) : la posteriorite n'est pas fiable.",
      as.integer(ndiv), as.integer(divergence_threshold)
    ))
  }
  if (!is.finite(rhat)) {
    notes <- c(notes, paste0(
      "Rhat indisponible (chaines MCMC uniques dans le backend HMC ",
      "tensorflow) — la convergence n'a pas pu etre verifiee par Rhat ",
      "(note enregistree, jamais masquee)."
    ))
  }
  if (!is.finite(ess)) {
    notes <- c(notes, paste0(
      "ESS indisponible dans ce backend — la convergence n'a pas pu etre ",
      "verifiee par taille d'echantillon efficace."
    ))
  }
  if (!is.finite(ndiv)) {
    notes <- c(notes, paste0(
      "Divergences non applicables (backend HMC, pas NUTS) — taux ",
      "d'acceptation enregistre a la place."
    ))
  }
  list(
    converged = length(failures) == 0L,
    failures = failures,
    warnings_suggested = warn_suggested,
    notes = notes
  )
}

# ── Passerelle Python (reticulate) — SEULE fonction touchant Python ─────────
.SCCODA_PY_BRIDGE <- '
import warnings, json
warnings.filterwarnings("ignore")
import pandas as pd
import tensorflow as tf
tf.random.set_seed(int(r_seed))
import sccoda
from sccoda.util import cell_composition_data as dat
from sccoda.util import comp_ana as mod
import arviz as az
import importlib.metadata as im

# 1. Table echantillon x (comptages + covariables) ; categories triees
#    (reference de condition = premiere par ordre alphabetique, deterministe).
df = pd.DataFrame(r_counts_py)
cov = pd.DataFrame(r_cov_py).set_index("sample")
df = df.join(cov)
df["condition"] = pd.Categorical(df["condition"], categories=sorted(df["condition"].unique()))
cov_cols = ["condition"]
if r_use_batch:
    df["batch"] = pd.Categorical(df["batch"], categories=sorted(df["batch"].unique()))
    cov_cols.append("batch")
data = dat.from_pandas(df, covariate_columns=cov_cols)

# 2. Modele + HMC (graine appliquee avant sampling).
model = mod.CompositionalAnalysis(data, formula=r_formula, reference_cell_type=r_reference)
res = model.sample_hmc(num_results=int(r_num_results), num_burnin=int(r_num_burnin),
                       num_leapfrog_steps=int(r_num_leapfrog), step_size=float(r_step_size))

# 3. Diagnostics (r_hat indisponible pour une chaine unique : NaN conserve).
#    Restriction a alpha/beta : les indicateurs spike-and-slab ("ind") ont
#    une ESS non interpretable (sauts discrets) — jamais retenus comme
#    diagnostic de convergence.
diag = az.summary(res, kind="diagnostics", var_names=["alpha", "beta"])
ess_cols = [c for c in diag.columns if c.startswith("ess")]
rhat_max = float(diag["r_hat"].max()) if "r_hat" in diag.columns else float("nan")
ess_min = float(diag[ess_cols].min().min()) if ess_cols else float("nan")

# 4. Effets bruts + effets credibles (seuil FDR), structures aplaties.
inter, eff = res.summary_prepare(est_fdr=float(r_fdr_target))
eff = eff.reset_index()
ce = res.credible_effects(est_fdr=float(r_fdr_target)).reset_index()
ce.columns = ["Covariate", "Cell Type", "credible_raw"]
merged = eff.merge(ce, on=["Covariate", "Cell Type"], how="left")

out = {
    "covariate": list(merged["Covariate"].astype(str)),
    "identity": list(merged["Cell Type"].astype(str)),
    "effect_raw": [float(x) for x in merged["Final Parameter"]],
    "hdi_3_raw": [float(x) for x in merged[["HDI 3%" if "HDI 3%" in merged.columns else "HDI 3.0%"]].iloc[:, 0]],
    "hdi_97_raw": [float(x) for x in merged[["HDI 97%" if "HDI 97%" in merged.columns else "HDI 97.0%"]].iloc[:, 0]],
    "sd": [float(x) for x in merged["SD"]],
    "inclusion_probability": [float(x) for x in merged["Inclusion probability"]],
    "log2_fold_change_raw": [float(x) for x in merged["log2-fold change"]],
    "credible_raw": [bool(x) for x in merged["credible_raw"].fillna(False)],
    "condition_base": str(df["condition"].cat.categories[0]),
    "diagnostics": {
        "rhat_max": rhat_max,
        "ess_min": ess_min,
        "n_divergences": float("nan"),
        "acc_rate": float(res.sampling_stats.get("acc_rate", float("nan"))),
        "num_results": int(res.sampling_stats.get("chain_length", int(r_num_results))),
        "num_burnin": int(res.sampling_stats.get("num_burnin", int(r_num_burnin))),
    },
    "versions": {
        "python": ".".join(str(v) for v in __import__("sys").version_info[:3]),
        "sccoda": im.version("sccoda"),
        "tensorflow": im.version("tensorflow"),
        "tensorflow_probability": im.version("tensorflow_probability"),
        "arviz": im.version("arviz"),
        "numpy": im.version("numpy"),
        "pandas": im.version("pandas"),
    },
}
r_result = out
'

#' Version HDI robuste : arviz nomme les colonnes "HDI 3%" (ou "HDI 3.0%")
#' selon la version — resolu cote Python, jamais cote R.

#' Resoudre le chemin du venv reticulate depuis l'executable Python
.sccoda_venv_dir <- function(python_path) {
  p <- normalizePath(python_path, winslash = "/", mustWork = FALSE)
  d <- dirname(p)
  if (basename(d) %in% c("Scripts", "bin")) dirname(d) else d
}

#' Calcul scCODA via reticulate (interne — la SEULE porte vers Python)
#'
#' @param counts data.frame echantillon x identites (comptages).
#' @param covariates data.frame echantillon x condition [+ batch].
#' @param reference_identity Identite de reference.
#' @param use_batch Inclure le terme batch dans la formule.
#' @param num_results,num_burnin MCMC HMC.
#' @param fdr_target Seuil FDR des effets credibles.
#' @param seed Graine tensorflow (reproductibilite declaree).
#' @param python_path Executable Python du resolveur.
#' @param context Contexte cite dans les messages d'erreur.
#' @return list(effect, condition_base, diagnostics, versions) — structures
#'   BRUTES, sans interpretation.
.sccoda_run_bridge <- function(counts, covariates, reference_identity,
                               use_batch, num_results, num_burnin, fdr_target,
                               seed, python_path, num_leapfrog_steps,
                               step_size,
                               context = "calcul scCODA") {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    .sccoda_stop(
      "environment_missing",
      paste0("Echec ", context, " : le package R 'reticulate' est requis ",
             "pour scCODA et n'est pas disponible — aucune execution Python ",
             "n'est possible.")
    )
  }
  venv_dir <- .sccoda_venv_dir(python_path)
  tryCatch(
    reticulate::use_virtualenv(venv_dir, required = TRUE),
    error = function(e) reticulate::use_python(python_path, required = TRUE)
  )
  main <- reticulate::import_main(convert = TRUE)
  main$r_counts_py <- as.data.frame(counts)
  main$r_cov_py <- as.data.frame(covariates)
  main$r_reference <- as.character(reference_identity)
  main$r_use_batch <- isTRUE(use_batch)
  main$r_formula <- if (isTRUE(use_batch)) "condition + batch" else "condition"
  main$r_num_results <- as.integer(num_results)
  main$r_num_burnin <- as.integer(num_burnin)
  main$r_num_leapfrog <- as.integer(num_leapfrog_steps)
  main$r_step_size <- as.numeric(step_size)
  main$r_fdr_target <- as.numeric(fdr_target)
  main$r_seed <- as.integer(seed)
  tryCatch(
    reticulate::py_run_string(.SCCODA_PY_BRIDGE),
    error = function(e) {
      .sccoda_stop(
        "compute_failed",
        sprintf("Echec %s : le modele scCODA (Python) a echoue — %s",
                context, conditionMessage(e))
      )
    }
  )
  out <- main$r_result
  if (is.null(out)) {
    .sccoda_stop(
      "compute_failed",
      sprintf("Echec %s : aucun resultat renvoye par le bridge Python.", context)
    )
  }
  list(
    effect = list(
      covariate = as.character(out$covariate),
      identity = as.character(out$identity),
      effect_raw = as.numeric(out$effect_raw),
      hdi_3_raw = as.numeric(out$hdi_3_raw),
      hdi_97_raw = as.numeric(out$hdi_97_raw),
      sd = as.numeric(out$sd),
      inclusion_probability = as.numeric(out$inclusion_probability),
      log2_fold_change_raw = as.numeric(out$log2_fold_change_raw),
      credible_raw = as.logical(out$credible_raw)
    ),
    condition_base = as.character(out$condition_base),
    diagnostics = list(
      rhat_max = suppressWarnings(as.numeric(out$diagnostics$rhat_max)),
      ess_min = suppressWarnings(as.numeric(out$diagnostics$ess_min)),
      n_divergences = suppressWarnings(as.numeric(out$diagnostics$n_divergences)),
      acc_rate = suppressWarnings(as.numeric(out$diagnostics$acc_rate)),
      num_results = suppressWarnings(as.integer(out$diagnostics$num_results)),
      num_burnin = suppressWarnings(as.integer(out$diagnostics$num_burnin))
    ),
    versions = lapply(out$versions, as.character)
  )
}

# ── Orchestration ───────────────────────────────────────────────────────────
#' Executer l'analyse scCODA (4E-2) — GATED sur le design Stage 13
#'
#' Valide TOUT prerequis AVANT le calcul (design Stage 13 eligible et non
#' perime, colonne identite declaree, conditions du contraste distinctes et
#' connues, environnement sccoda disponible), construit la matrice de
#' composition sur les echantillons de la PAIRE de conditions choisie,
#' choisit la reference, execute le modele via reticulate, verifie la
#' convergence (echec => AUCUN resultat), puis finalise le resultat canonique
#' avec les effets reorientes cible vs reference.
#'
#' @param seurat_obj Objet Seurat valide.
#' @param da_design_result Resultat canonique du design Stage 13 — REQUIS.
#' @param target_condition Condition cible (effet > 0 = enrichie dans la
#'   cible, apres reorientation).
#' @param reference_condition Condition de reference.
#' @param reference_identity Identite de reference (NULL = politique
#'   "most_abundant", enregistree).
#' @param use_batch Inclure le terme batch du design (defaut : TRUE si le
#'   design declare un batch).
#' @param num_results,num_burnin MCMC HMC (config TS_DA_SCCODA_*).
#' @param fdr_target Seuil FDR des effets credibles.
#' @param seed Graine tensorflow enregistree.
#' @param resolver Resolveur d'environnement injectable (tests).
#' @param context Contexte cite dans les messages d'erreur.
#' @return Resultat canonique (champs sccoda_contract_fields()).
#' @export
run_sccoda_da <- function(seurat_obj,
                          da_design_result,
                          target_condition,
                          reference_condition,
                          reference_identity = NULL,
                          use_batch = TRUE,
                          num_results = TS_DA_SCCODA_NUM_RESULTS,
                          num_burnin = TS_DA_SCCODA_NUM_BURNIN,
                          num_leapfrog_steps = TS_DA_SCCODA_NUM_LEAPFROG,
                          step_size = TS_DA_SCCODA_STEP_SIZE,
                          fdr_target = TS_DA_SCCODA_FDR_TARGET,
                          seed = TS_DA_SCCODA_SEED,
                          resolver = NULL,
                          context = "abondance differentielle scCODA") {
  # ── 0. Prerequis — TOUT valide avant le calcul ───────────────────────────
  seurat_obj <- assert_seurat(seurat_obj, context = context)
  assert_da_design_result(
    da_design_result,
    method = "sccoda",
    seurat_obj = seurat_obj,
    context = context
  )
  if (is.null(target_condition) || is.null(reference_condition) ||
      is.na(target_condition) || is.na(reference_condition) ||
      identical(target_condition, reference_condition)) {
    .sccoda_stop(
      "invalid_input",
      sprintf(
        paste0("Echec %s : le contraste exige deux conditions DISTINCTES ",
               "(cible = '%s', reference = '%s')."),
        context, target_condition %||% NA_character_,
        reference_condition %||% NA_character_
      )
    )
  }
  design_recap <- .sccoda_design_recap(da_design_result)
  conditions <- if (!is.null(da_design_result$condition_summary) &&
                    nrow(da_design_result$condition_summary)) {
    as.character(da_design_result$condition_summary$condition)
  } else character(0)
  for (lv in c(target_condition, reference_condition)) {
    if (!lv %in% conditions) {
      .sccoda_stop(
        "invalid_input",
        sprintf(
          paste0("Echec %s : la condition '%s' n'appartient pas au design ",
                 "valide (conditions connues : %s)."),
          context, lv, paste(conditions, collapse = ", ")
        )
      )
    }
  }
  cfg <- da_design_result$config %||% list()
  identity_col <- cfg$identity
  if (is.null(identity_col) || is.na(identity_col) || !nzchar(identity_col)) {
    .sccoda_stop(
      "invalid_input",
      sprintf(
        paste0("Echec %s : scCODA exige une colonne d'identite declaree ",
               "dans le design (labels cellulaires de la composition) — ",
               "revalidez le design avec une colonne identite."),
        context
      )
    )
  }
  sample_col <- cfg$sample_id
  condition_col <- cfg$condition
  batch_col <- if (isTRUE(use_batch) && !is.na(cfg$batch %||% NA_character_) &&
                   nzchar(cfg$batch %||% "")) cfg$batch else NULL

  # ── 1. Preparation (pure R) ──────────────────────────────────────────────
  meta <- seurat_obj@meta.data
  prep <- sccoda_composition_matrix(
    metadata = meta, sample_id = sample_col, condition = condition_col,
    batch = batch_col, identity = identity_col, context = context
  )
  # Restreindre aux echantillons de la PAIRE de conditions (portee declaree
  # du modele ; les autres echantillons sont exclus et comptes).
  keep_s <- prep$covariates$condition %in% c(target_condition, reference_condition)
  counts <- prep$counts[keep_s, , drop = FALSE]
  covariates <- prep$covariates[keep_s, , drop = FALSE]
  n_cells_outside_contrast <- sum(prep$counts[!keep_s, , drop = FALSE])
  if (nrow(counts) < 2L) {
    .sccoda_stop(
      "invalid_input",
      sprintf(
        paste0("Echec %s : la paire de conditions choisie ne couvre que %d ",
               "echantillon(s) — au moins deux sont requis (un par condition ",
               "au minimum)."),
        context, nrow(counts)
      )
    )
  }
  ref_id <- sccoda_reference_identity(
    counts, identity_value = reference_identity,
    policy = "most_abundant", context = context
  )

  # ── 2. Environnement + calcul Python ─────────────────────────────────────
  env <- sccoda_available(resolver = resolver, stop_if_missing = TRUE)
  raw <- .sccoda_run_bridge(
    counts = counts, covariates = covariates,
    reference_identity = ref_id, use_batch = !is.null(batch_col),
    num_results = num_results, num_burnin = num_burnin,
    fdr_target = fdr_target, seed = seed, python_path = env$python_path,
    num_leapfrog_steps = num_leapfrog_steps, step_size = step_size,
    context = context
  )

  # ── 3. Convergence — echec => AUCUN resultat ─────────────────────────────
  conv <- sccoda_convergence_assessment(raw$diagnostics)
  if (!isTRUE(conv$converged)) {
    .sccoda_stop(
      "convergence_failure",
      paste0(
        sprintf("Echec %s : ", context),
        paste(conv$failures, collapse = " | ")
      )
    )
  }

  # ── 4. Resultat canonique + provenance produite ICI (regle 7) ────────────
  finalize_sccoda_result(
    raw = raw,
    counts = counts,
    covariates = covariates,
    reference_identity = ref_id,
    target_condition = target_condition,
    reference_condition = reference_condition,
    use_batch = !is.null(batch_col),
    num_results = num_results,
    num_burnin = num_burnin,
    num_leapfrog_steps = num_leapfrog_steps,
    step_size = step_size,
    fdr_target = fdr_target,
    seed = seed,
    env = env,
    design_recap = design_recap,
    n_cells_excluded = prep$n_cells_excluded,
    n_cells_missing_identity = prep$n_cells_missing_identity,
    n_cells_outside_contrast = n_cells_outside_contrast,
    conv_notes = conv$notes,
    seurat_obj = seurat_obj,
    extra_warnings = c(design_recap$design_warnings, conv$warnings_suggested),
    context = context
  )
}

#' Reorienter les effets bruts en "cible vs reference" (pur R)
#'
#' Le coefficient patsy "condition[T.X]" vaut pour X contre la categorie de
#' base. Si la cible est la categorie de base, les effets sont inverses
#' (effet et bornes HDI) — la reorientation est EXPLICITE et tracee.
.sccoda_orient_effects <- function(raw, condition_base, target_condition,
                                   reference_condition) {
  covs <- raw$covariate
  signs <- vapply(covs, function(cv) {
    m <- regmatches(cv, regexec("^condition\\[T\\.(.*)\\]$", cv))
    if (length(m[[1]]) > 1L) {
      lv <- m[[1]][2L]
      if (identical(lv, target_condition)) 1
      else if (identical(lv, reference_condition)) -1
      else 1
    } else 1
  }, numeric(1), USE.NAMES = FALSE)
  oriented <- signs < 0
  effect <- raw$effect_raw * signs
  hdi_low <- ifelse(oriented, -raw$hdi_97_raw, raw$hdi_3_raw)
  hdi_high <- ifelse(oriented, -raw$hdi_3_raw, raw$hdi_97_raw)
  l2fc <- raw$log2_fold_change_raw * signs
  data.frame(
    covariate = covs,
    identity = raw$identity,
    effect = effect,
    hdi_low = hdi_low,
    hdi_high = hdi_high,
    sd = raw$sd,
    inclusion_probability = raw$inclusion_probability,
    log2_fold_change = l2fc,
    credible = raw$credible_raw,
    effect_sign_flipped = oriented,
    stringsAsFactors = FALSE
  )
}

#' Finaliser le resultat scCODA canonique (Stage 15)
#'
#' Assemble l'objet canonique documente (en-tete de fichier) a partir des
#' structures brutes du bridge Python. La provenance est PRODUITE ici
#' (regle 7 AGENTS.md) ; l'appelant l'append a l'etat partage.
#'
#' @param raw Structure brute du bridge (.sccoda_run_bridge).
#' @param counts,covariates Matrice de composition analysee (paire de conditions).
#' @param reference_identity Identite de reference utilisee.
#' @param target_condition,reference_condition Contraste.
#' @param use_batch Term batch inclus ?
#' @param num_results,num_burnin,fdr_target,seed Parametres enregistres.
#' @param env Sortie du resolveur d'environnement.
#' @param design_recap Recapitulatif du design Stage 13.
#' @param n_cells_excluded,n_cells_missing_identity,n_cells_outside_contrast Exclusions comptees.
#' @param conv_notes Notes de convergence (rhat indisponible etc.).
#' @param seurat_obj Objet Seurat analyse.
#' @param extra_warnings Avertissements supplementaires.
#' @param context Contexte cite dans les messages d'erreur.
#' @return L'objet canonique (champs sccoda_contract_fields()).
#' @export
finalize_sccoda_result <- function(raw,
                                   counts,
                                   covariates,
                                   reference_identity,
                                   target_condition,
                                   reference_condition,
                                   use_batch,
                                   num_results,
                                   num_burnin,
                                   num_leapfrog_steps,
                                   step_size,
                                   fdr_target,
                                   seed,
                                   env,
                                   design_recap,
                                   n_cells_excluded = 0L,
                                   n_cells_missing_identity = 0L,
                                   n_cells_outside_contrast = 0L,
                                   conv_notes = character(0),
                                   seurat_obj = NULL,
                                   extra_warnings = character(0),
                                   context = "abondance differentielle scCODA") {
  if (!is.list(raw) || is.null(raw$effect)) {
    .sccoda_stop(
      "invalid_input",
      "finalize_sccoda_result() : structures brutes du bridge absentes — run_sccoda_da() doit etre appelee d'abord."
    )
  }
  effect_table <- .sccoda_orient_effects(
    raw$effect, raw$condition_base, target_condition, reference_condition
  )
  # La reference est epinglee par le modele (effet nul) : hors table d'effets.
  effect_table <- effect_table[effect_table$identity != reference_identity,
                               , drop = FALSE]
  rownames(effect_table) <- NULL
  condition_rows <- grepl("^condition\\[", effect_table$covariate)
  credible_effects <- sort(unique(effect_table$identity[condition_rows &
                                                          effect_table$credible]))

  composition_table <- data.frame(
    sample = rownames(counts),
    counts,
    condition = covariates$condition,
    batch = if (!is.null(covariates$batch)) covariates$batch else NA_character_,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  warnings_all <- unique(c(
    as.character(design_recap$design_warnings %||% character(0)),
    as.character(conv_notes %||% character(0)),
    as.character(extra_warnings)
  ))
  status <- if (length(warnings_all)) "valid_with_warnings" else "valid"

  result <- list()
  result$type <- "sccoda_da"
  result$status <- status
  result$compositional_unit <- "sample"
  result$design <- design_recap
  result$parameters <- list(
    target_condition = target_condition,
    reference_condition = reference_condition,
    sample_id_column = design_recap$sample_id_column,
    condition_column = design_recap$condition_column,
    batch_column = design_recap$batch_column,
    batch_in_model = isTRUE(use_batch),
    identity_column = design_recap$identity_column,
    reference_policy = "most_abundant",
    fdr_target = as.numeric(fdr_target),
    n_samples_in_model = nrow(counts),
    n_cells_excluded_missing_sample = as.integer(n_cells_excluded),
    n_cells_missing_identity = as.integer(n_cells_missing_identity),
    n_cells_outside_contrast = as.integer(n_cells_outside_contrast),
    n_cells_in_model = sum(counts),
    environment_source = env$source %||% NA_character_,
    composition_unit = "sample"
  )
  result$model_specification <- list(
    formula = if (isTRUE(use_batch)) "condition + batch" else "condition",
    condition_base = raw$condition_base,
    reference_policy = "most_abundant",
    reference_identity = reference_identity,
    num_results = as.integer(num_results),
    num_burnin = as.integer(num_burnin),
    num_leapfrog_steps = as.integer(num_leapfrog_steps),
    step_size = as.numeric(step_size),
    fdr_target = as.numeric(fdr_target),
    backend = "tensorflow HMC (scCODA spike-and-slab NB)",
    seed = as.integer(seed)
  )
  result$convergence_diagnostics <- list(
    rhat_max = raw$diagnostics$rhat_max,
    ess_min = raw$diagnostics$ess_min,
    n_divergences = raw$diagnostics$n_divergences,
    acc_rate = raw$diagnostics$acc_rate,
    num_results = raw$diagnostics$num_results %||% as.integer(num_results),
    num_burnin = raw$diagnostics$num_burnin %||% as.integer(num_burnin),
    notes = as.character(conv_notes %||% character(0))
  )
  result$composition_table <- composition_table
  result$effect_table <- effect_table
  result$credible_effects <- credible_effects
  result$reference_identity <- as.character(reference_identity)
  result$package_versions <- c(raw$versions, list(R = paste(R.version$major,
                                                            R.version$minor,
                                                            sep = ".")))
  result$object_identity <- build_object_identity_v2(seurat_obj)
  result$warnings <- warnings_all

  entry <- new_provenance_entry(
    analysis_id = "sc-da-sccoda",
    method = "sccoda_compositional_da",
    parameters = list(
      target_condition = target_condition,
      reference_condition = reference_condition,
      reference_identity = reference_identity,
      reference_policy = "most_abundant",
      formula = if (isTRUE(use_batch)) "condition + batch" else "condition",
      batch_in_model = isTRUE(use_batch),
      num_results = as.integer(num_results),
      num_burnin = as.integer(num_burnin),
      fdr_target = as.numeric(fdr_target),
      seed = as.integer(seed),
      n_samples_in_model = nrow(counts),
      n_cells_in_model = as.integer(sum(counts)),
      n_cells_excluded_missing_sample = as.integer(n_cells_excluded),
      n_cells_missing_identity = as.integer(n_cells_missing_identity),
      n_cells_outside_contrast = as.integer(n_cells_outside_contrast),
      environment_source = env$source %||% NA_character_,
      python_version = env$python_version %||% NA_character_,
      sccoda_version = env$sccoda_version %||% NA_character_,
      design_fingerprint = design_recap$design_fingerprint,
      composition_unit = "sample",
      object_fingerprint = result$object_identity$fingerprint
    ),
    dataset = seurat_obj,
    cells_used = as.integer(sum(counts)),
    cells_excluded = as.integer(n_cells_excluded + n_cells_missing_identity +
                                  n_cells_outside_contrast),
    seed = seed,
    warnings = warnings_all
  )
  entry$analysis_type <- "sccoda_da"
  entry$status <- status
  entry$timestamp_utc <- format(entry$timestamp, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  result$provenance <- entry
  result$analysis_id <- "sc-da-sccoda"
  result$timestamp_utc <- entry$timestamp_utc

  result
}

#' Le resultat scCODA est-il perime vis-a-vis de l'objet Seurat courant ?
#'
#' @param sccoda_result Resultat canonique.
#' @param seurat_obj Objet Seurat courant (ou tout objet a dimnames).
#' @return TRUE si les empreintes divergent, FALSE sinon, NA si
#'   indeterminable.
#' @export
sccoda_result_is_stale <- function(sccoda_result, seurat_obj) {
  if (is.null(sccoda_result)) return(NA)
  fp <- sccoda_result$object_identity$fingerprint %||% NULL
  if (is.null(fp) || is.null(seurat_obj)) return(NA)
  !identical(fp, velocity_object_fingerprint(seurat_obj))
}

#' Verifier que l'objet est un resultat scCODA canonique
#'
#' Garde de contrat pour TOUT consommateur (vues Stage 16, rapport 4F).
#'
#' @param sccoda_result Objet a verifier (resultat canonique attendu).
#' @param seurat_obj Objet Seurat courant optionnel (peremption).
#' @param context Contexte cite dans les messages d'erreur.
#' @return Le resultat, invisible (pipable, conforme au style assert_*).
#' @export
assert_sccoda_result <- function(sccoda_result,
                                 seurat_obj = NULL,
                                 context = "resultat scCODA") {
  if (!is.list(sccoda_result) ||
      !identical(sccoda_result$type %||% NULL, "sccoda_da") ||
      is.null(sccoda_result$status)) {
    .sccoda_stop(
      "invalid_input",
      sprintf(
        paste0("Echec %s : resultat scCODA canonique requis (run_sccoda_da() / ",
               "finalize_sccoda_result()) — recu : %s."),
        context,
        if (is.null(sccoda_result)) "NULL"
        else paste(class(sccoda_result), collapse = "/")
      )
    )
  }
  if (!sccoda_result$status %in% sccoda_validity_states()) {
    .sccoda_stop(
      "invalid_input",
      sprintf("Echec %s : statut scCODA inconnu '%s'.", context,
              sccoda_result$status)
    )
  }
  if (sccoda_result$status %in% c("invalid_input", "environment_missing",
                                  "compute_failed", "convergence_failure",
                                  "design_not_eligible")) {
    .sccoda_stop(
      sccoda_result$status,
      sprintf("Echec %s : %s", context,
              sccoda_status_labels()[[sccoda_result$status]])
    )
  }
  if (!is.null(sccoda_result$effect_table) &&
      !is.data.frame(sccoda_result$effect_table)) {
    .sccoda_stop(
      "invalid_input",
      sprintf("Echec %s : effect_table doit etre un data.frame.", context)
    )
  }
  if (!is.null(seurat_obj) &&
      isTRUE(sccoda_result_is_stale(sccoda_result, seurat_obj))) {
    .sccoda_stop(
      "stale_against_current_seurat_object",
      sprintf("Echec %s : %s", context,
              sccoda_status_labels()[["stale_against_current_seurat_object"]])
    )
  }
  invisible(sccoda_result)
}

# ── Exports ─────────────────────────────────────────────────────────────────
#' Resume de l'analyse scCODA pour export CSV (Stage 15)
#'
#' @param sccoda_result Resultat canonique.
#' @return data.frame a une ligne, colonnes character.
#' @export
build_sccoda_summary <- function(sccoda_result) {
  r <- assert_sccoda_result(sccoda_result, context = "resume scCODA")
  data.frame(
    analysis_id = r$analysis_id %||% NA_character_,
    analysis_type = r$type %||% "sccoda_da",
    status = r$status %||% NA_character_,
    timestamp_utc = r$timestamp_utc %||% NA_character_,
    design_analysis_id = r$design$design_analysis_id %||% NA_character_,
    compositional_unit = r$compositional_unit %||% "sample",
    target_condition = r$parameters$target_condition %||% NA_character_,
    reference_condition = r$parameters$reference_condition %||% NA_character_,
    reference_identity = r$reference_identity %||% NA_character_,
    model_formula = r$model_specification$formula %||% NA_character_,
    num_results = as.character(r$model_specification$num_results %||% NA_integer_),
    num_burnin = as.character(r$model_specification$num_burnin %||% NA_integer_),
    fdr_target = as.character(r$model_specification$fdr_target %||% NA_real_),
    ess_min = as.character(round(as.numeric(
      r$convergence_diagnostics$ess_min %||% NA_real_), 2)),
    acc_rate = as.character(round(as.numeric(
      r$convergence_diagnostics$acc_rate %||% NA_real_), 4)),
    credible_effects = paste(r$credible_effects %||% character(0), collapse = " | "),
    sccoda_version = r$package_versions$sccoda %||% NA_character_,
    python_version = r$package_versions$python %||% NA_character_,
    object_fingerprint = r$object_identity$fingerprint %||% NA_character_,
    warnings = paste(r$warnings %||% character(0), collapse = " | "),
    stringsAsFactors = FALSE
  )
}

#' Export CSV de la matrice de composition analysee (Stage 15)
#'
#' @param sccoda_result Resultat canonique.
#' @return data.frame (n echantillons lignes).
#' @export
build_sccoda_composition_export <- function(sccoda_result) {
  r <- assert_sccoda_result(sccoda_result, context = "export composition scCODA")
  ct <- r$composition_table %||% NULL
  if (is.null(ct) || !is.data.frame(ct)) {
    .sccoda_stop(
      "invalid_input",
      "Aucune matrice de composition disponible dans le resultat canonique."
    )
  }
  data.frame(
    ct,
    analysis_id = rep(r$analysis_id %||% NA_character_, nrow(ct)),
    timestamp_utc = rep(r$timestamp_utc %||% NA_character_, nrow(ct)),
    stringsAsFactors = FALSE
  )
}

#' Export CSV de la table d'effets (Stage 15)
#'
#' Effets REORIENTES cible vs reference (logFC > 0 = enrichi dans la cible) ;
#' les identites credibles portent credible = TRUE au seuil fdr_target.
#'
#' @param sccoda_result Resultat canonique.
#' @return data.frame (covariables x identites, hors reference).
#' @export
build_sccoda_effect_table_export <- function(sccoda_result) {
  r <- assert_sccoda_result(sccoda_result, context = "export effets scCODA")
  et <- r$effect_table %||% NULL
  if (is.null(et) || !is.data.frame(et)) {
    .sccoda_stop(
      "invalid_input",
      "Aucune table d'effets disponible dans le resultat canonique."
    )
  }
  data.frame(
    et,
    analysis_id = rep(r$analysis_id %||% NA_character_, nrow(et)),
    timestamp_utc = rep(r$timestamp_utc %||% NA_character_, nrow(et)),
    stringsAsFactors = FALSE
  )
}

#' Nom de fichier d'export trace par l'identifiant d'analyse (Stage 15)
#'
#' @param sccoda_result Resultat canonique.
#' @param kind Prefixe descriptif (ex. "sccoda_effects").
#' @param ext Extension ("csv", "rds").
#' @return Chaine "<kind>_<analysis_id>_<date>.<ext>".
#' @export
sccoda_export_filename <- function(sccoda_result, kind, ext) {
  if (is.null(sccoda_result) || !is.list(sccoda_result)) {
    .sccoda_stop(
      "invalid_input",
      "sccoda_export_filename() : resultat scCODA canonique requis."
    )
  }
  aid <- sccoda_result$analysis_id %||% "sc-da-sccoda"
  sprintf(
    "%s_%s_%s.%s",
    as.character(kind)[1L],
    aid,
    format(Sys.Date(), "%Y-%m-%d"),
    as.character(ext)[1L]
  )
}

#' Surface publique figee de R/sc/sc_abundance_sccoda.R (Stage 15)
#'
#' Le test de freeze refuse toute fonction top-level non prefixee d'un point
#' qui ne figure pas dans cette liste.
#'
#' @return Vecteur character des noms de fonctions publiques.
#' @export
sccoda_public_api <- function() {
  c(
    "sccoda_contract_fields", "sccoda_validity_states", "sccoda_status_labels",
    "sccoda_error_state", "sccoda_available", "sccoda_composition_matrix",
    "sccoda_reference_identity", "sccoda_convergence_assessment",
    "run_sccoda_da", "finalize_sccoda_result", "sccoda_result_is_stale",
    "assert_sccoda_result", "build_sccoda_summary",
    "build_sccoda_composition_export", "build_sccoda_effect_table_export",
    "sccoda_export_filename", "sccoda_public_api"
  )
}

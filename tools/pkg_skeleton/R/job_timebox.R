# =============================================================================
# tools/pkg_skeleton/R/job_timebox.R — « timebox » des jobs N7 (satellites
# slice S3, fiche docs/proposals/
# PROPOSAL_N_SATELLITES_LAUNCH_ROADMAP_2026-10-26.md §slice S3, jalon round8)
# =============================================================================
# PRINCIPE (fiche §0 N7 / brief round8 jalon 3, 2026-11-03) : « ne jamais
# regarder un écran figé » — pendant l'attente d'un job, l'utilisateur voit la
# durée écoulée EN REGARD du budget DÉCLARÉ du job (« en cours — 3 min 12 s
# (typique 5–10 min) »), et un dépassement est consigné en provenance. Aucune
# performance ici (ça reste E5/async) : de l'honnêteté pendant l'attente.
#
# Rails (motif dataset_health.R:14-45, règle maison « purity rails ») :
#   * NO shiny, NO reactive, NO session — ce fichier doit sourcer() partout où
#     tourne un process R pur ;
#   * LES BUDGETS VIVENT DANS defaults.R (TS_JOB_BUDGET_S_*, à côté des
#     TS_BULK_TPM_* d'O16) — ce fichier ne déclare AUCUN budget, il lit les
#     constantes (source unique, convention QW-2 anti-C-1 : l'UI et le canal
#     drive lisent CES valeurs, jamais un littéral local) ;
#   * BUDGET ≠ TIMEOUT : le budget est un plafond ANNONCÉ (généreux, revu au
#     milestone — défaut D4), jamais un kill ; les plafonds durs restent les
#     TS_*_TIMEOUT_* (defaults.R) ;
#   * JOB INCONNU → fallback TS_JOB_BUDGET_S_UNDECLARED (budget généreux
#     documenté) + attribut "untested" = TRUE — jamais un budget inventé ;
#   * PROVENANCE = CANAL EXISTANT : l'entrée de dépassement est construite par
#     new_provenance_entry() (provenance.R:128) et posée par
#     provenance_append() (provenance.R:168) — réutilisation, jamais une
#     structure parallèle ;
#   * i18n : les libellés passent par un injecteur tr= (motif des builders de
#     plots R/plotting) — sans injecteur, libellés FR par défaut ; les clés FR
#     sont append-only dans i18n/translation.json ;
#   * base R only (fiche §52 : 0 nouvelle dépendance).
#
# ÉTAT (GREEN, round8 jalon 3 — RED mesuré avant implémentation : FAIL 22 /
# PASS 0 / SKIP 2 / 0 erreur, transcript test-job-timebox.R du 2026-11-03) :
# trois helpers purs — lookup de constante (source unique defaults.R),
# formatage pur de libellé, construction d'entrée de provenance via le canal
# existant. Aucun état, aucun I/O, aucun appel Shiny.
# =============================================================================

#' Interpolation nommée de template {x} (motif .t_fmt, global.R:277)
#'
#' Interne : les valeurs arrivent PRÉ-FORMATÉES (chaînes) — pas de format()
#' ici, la mise en forme des durées est le travail de .tb_fmt_duration().
#' Piège R évité ici : un argument formel nommé `template` CAPTURE par
#' correspondance partielle un appelant qui passerait un placeholder `t =`
#' ("t" est un préfixe de "template" — mesuré le 2026-11-03 : le libellé
#' perdait son préfixe « en cours — »). Le formel est donc dot-préfixé :
#' aucun nom de placeholder ne peut le préfixer.
.tb_fmt <- function(.template, ...) {
  vals <- list(...)
  for (nm in names(vals)) {
    .template <- gsub(paste0("{", nm, "}"), vals[[nm]], .template, fixed = TRUE)
  }
  .template
}

#' Fourchette « typique » borne basse–haute, unité FACTORIÉE
#'
#' Interne. "5–10 min" (et non "5 min–10 min" — format de référence du brief
#' jalon 3) ; classes d'unité : s (< 60), min (< 3600), h (au-delà) ; classes
#' mixtes -> deux durées complètes ("45 s–2 min").
.tb_range <- function(low_s, high_s) {
  cls <- function(s) if (s < 60) "s" else if (s < 3600) "min" else "h"
  if (identical(cls(low_s), cls(high_s))) {
    if (cls(high_s) == "s") {
      return(sprintf("%d\u2013%d s", round(low_s), round(high_s)))
    }
    if (cls(high_s) == "min") {
      return(sprintf("%d\u2013%d min", floor(low_s / 60), floor(high_s / 60)))
    }
    return(sprintf("%d\u2013%d h", floor(low_s / 3600), floor(high_s / 3600)))
  }
  paste(.tb_fmt_duration(low_s), .tb_fmt_duration(high_s), sep = "\u2013")
}

#' Durée en secondes -> libellé h/min/s (pur, déterministe)
#'
#' Interne. Conventions : "45 s" ; "3 min 12 s" (secondes NON paddées) ;
#'
#' "1 h 01 min" (minutes paddées sur 2 chiffres) ; minutes nulles omises.
.tb_fmt_duration <- function(s) {
  s <- round(s)
  if (s < 60) return(sprintf("%d s", s))
  m <- floor(s / 60)
  if (m < 60) {
    sec <- s - m * 60
    return(if (sec == 0L) sprintf("%d min", m) else sprintf("%d min %d s", m, sec))
  }
  h <- floor(m / 60)
  sprintf("%d h %02d min", h, m - h * 60)
}

#' Budget de durée d'un job (constante TS_JOB_BUDGET_S_* de defaults.R)
#'
#' @param job Identifiant drive du job (ex. "bulk-pathways-run_pathway") —
#'   normalisé en suffixe de constante (non-alphanumériques -> "_").
#' @return Le budget (secondes, numérique positif) de la constante
#'   TS_JOB_BUDGET_S_<JOB> ; job inconnu (ou constante invalide) -> budget
#'   généreux TS_JOB_BUDGET_S_UNDECLARED avec attribut "untested" = TRUE.
ts_job_budget <- function(job) {
  job <- as.character(job)
  if (length(job) != 1L || is.na(job) || !nzchar(job)) {
    stop("ts_job_budget() : 'job' requis (cha\u00eene non vide).", call. = FALSE)
  }
  key <- toupper(gsub("[^A-Za-z0-9]+", "_", job))
  const_name <- paste0("TS_JOB_BUDGET_S_", key)
  b <- NULL
  if (exists(const_name, inherits = TRUE)) {
    v <- suppressWarnings(as.numeric(get(const_name)))
    # constante déclarée mais corrompue = comme absente : on retombe sur le
    # fallback documenté (un helper d'affichage ne doit pas tuer un rendu).
    if (length(v) == 1L && is.finite(v) && v > 0) b <- v
  }
  if (is.null(b)) {
    fb <- suppressWarnings(as.numeric(get0("TS_JOB_BUDGET_S_UNDECLARED",
                                           inherits = TRUE)))
    if (length(fb) != 1L || !is.finite(fb) || fb <= 0) {
      stop("ts_job_budget() : fallback 'TS_JOB_BUDGET_S_UNDECLARED' ",
           "indisponible ou invalide \u2014 sourcez ",
           "tools/pkg_skeleton/R/defaults.R.", call. = FALSE)
    }
    attr(fb, "untested") <- TRUE
    return(fb)
  }
  b
}

#' Libellé « elapsed vs budget » d'un job en vol (formatage pur)
#'
#' @param elapsed_s Durée écoulée (secondes, >= 0).
#' @param budget_s Budget déclaré du job (secondes, > 0).
#' @param typical_low_s Borne basse de la fourchette « typique » (défaut :
#'   budget_s / 2 — la fourchette est DÉRIVÉE des constantes, jamais devinée).
#' @param tr Injecteur i18n optionnel (fonction clé -> template) ; les clés FR
#'   portent les placeholders {elapsed} / {range} / {ratio}.
#' @return "en cours — 3 min 12 s (typique 5–10 min)" ; au-delà du budget, la
#'   variante overrun porte « dépassé » et le ratio : "en cours — 12 min 5 s
#'   (dépassé — typique 5–10 min, ×1.2)".
ts_job_budget_label <- function(elapsed_s, budget_s, typical_low_s = budget_s / 2,
                                tr = NULL) {
  .tr <- if (is.function(tr)) tr else function(x) x
  if (!is.numeric(elapsed_s) || length(elapsed_s) != 1L || !is.finite(elapsed_s) ||
      elapsed_s < 0) {
    stop("ts_job_budget_label() : 'elapsed_s' doit \u00eatre un nombre ",
         "positif fini.", call. = FALSE)
  }
  if (!is.numeric(budget_s) || length(budget_s) != 1L || !is.finite(budget_s) ||
      budget_s <= 0) {
    stop("ts_job_budget_label() : 'budget_s' doit \u00eatre un nombre ",
         "strictement positif fini.", call. = FALSE)
  }
  if (!is.numeric(typical_low_s) || length(typical_low_s) != 1L ||
      !is.finite(typical_low_s) || typical_low_s <= 0 || typical_low_s > budget_s) {
    stop("ts_job_budget_label() : 'typical_low_s' doit \u00eatre un nombre ",
         "fini dans ]0 ; budget_s].", call. = FALSE)
  }
  elapsed_str <- .tb_fmt_duration(elapsed_s)
  range_str <- .tb_range(typical_low_s, budget_s)
  head <- .tb_fmt(.tr("en cours \u2014 {elapsed}"), elapsed = elapsed_str)
  if (elapsed_s > budget_s) {
    ratio_str <- sprintf("%.1f", elapsed_s / budget_s)
    tail <- paste0("(", .tb_fmt(
      .tr("d\u00e9pass\u00e9 \u2014 typique {range}, \u00d7{ratio}"),
      range = range_str, ratio = ratio_str), ")")
  } else {
    tail <- paste0("(", .tb_fmt(.tr("typique {range}"),
                                range = range_str), ")")
  }
  paste(head, tail)
}

#' Entrée de provenance d'un dépassement de budget (canal EXISTANT)
#'
#' Construit {job, budget, actual, ratio} via new_provenance_entry()
#' (provenance.R:128) pour être posé par provenance_append()
#' (provenance.R:168) — réutilisation du canal, jamais une structure
#' parallèle. NE documente QUE les dépassements (actual_s > budget_s,
#' ratio > 1) : un job dans son budget ne produit AUCUNE ligne.
#'
#' @param job Identifiant drive du job (ex. "bulk-pathways-run_pathway").
#' @param budget_s Budget déclaré (secondes, > 0).
#' @param actual_s Durée réellement mesurée (secondes, > budget_s).
#' @return Entrée de provenance (liste plate, contrat new_provenance_entry).
ts_job_overrun_entry <- function(job, budget_s, actual_s) {
  job <- as.character(job)
  if (length(job) != 1L || is.na(job) || !nzchar(job)) {
    stop("ts_job_overrun_entry() : 'job' requis (cha\u00eene non vide).", call. = FALSE)
  }
  budget_s <- suppressWarnings(as.numeric(budget_s))
  actual_s <- suppressWarnings(as.numeric(actual_s))
  if (length(budget_s) != 1L || !is.finite(budget_s) || budget_s <= 0) {
    stop("ts_job_overrun_entry() : 'budget_s' doit \u00eatre un nombre ",
         "strictement positif fini.", call. = FALSE)
  }
  if (length(actual_s) != 1L || !is.finite(actual_s) || actual_s < 0) {
    stop("ts_job_overrun_entry() : 'actual_s' doit \u00eatre un nombre ",
         "positif fini.", call. = FALSE)
  }
  if (actual_s <= budget_s) {
    stop("ts_job_overrun_entry() : 'actual_s' (", format(actual_s),
         " s) doit d\u00e9passer 'budget_s' (", format(budget_s),
         " s) \u2014 cette entr\u00e9e ne documente QUE les d\u00e9passements ",
         "(sinon : pas de ligne de provenance).", call. = FALSE)
  }
  ratio <- actual_s / budget_s
  new_provenance_entry(
    analysis_id = paste0("job-overrun-", job),
    method = "ts_job_timebox_overrun",
    parameters = list(job = job, budget_s = budget_s, actual_s = actual_s,
                      ratio = ratio),
    warnings = paste0(
      "Job '", job, "' : budget d\u00e9pass\u00e9 \u2014 ",
      .tb_fmt_duration(actual_s), " pour un budget de ",
      .tb_fmt_duration(budget_s), " (\u00d7", sprintf("%.1f", ratio), ")."
    )
  )
}

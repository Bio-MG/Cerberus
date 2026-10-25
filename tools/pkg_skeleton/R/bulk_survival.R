# =============================================================================
# tools/pkg_skeleton/R/bulk_survival.R — Survie & associations cliniques (roadmap Bulk V2,
# Milestone 5 — chantier Flux E).
# =============================================================================
# GARDES DE LA MISSION (§2.5 — non négociables) :
#   1. Colonnes validées : time NUMÉRIQUE et > 0 ; status dans {0,1}
#      (codage {1,2} accepté UNIQUEMENT si déclaré explicitement) ; au moins
#      TS_BULK_SURV_MIN_EVENTS (10) événements observés.
#   2. Découpes par MÉDIANE ou QUARTILES uniquement — AUCUNE recherche de
#      cutpoint optimal (erreur si autre valeur demandée).
#   3. Tests multiples : ajustement BH sur les Cox univariés dès >= 2
#      variables testées, avec avertissement.
#
# KM : survival::survfit + survdiff (log-rank). Cox : survival::coxph.
# Figure : survminer::ggsurvplot si présent, sinon ggplot pur (courbes en
# marches depuis la survfit). Erreurs classées `bulk_survival_error`.
# =============================================================================

#' Surface publique figée du domaine survie (gel par test de freeze)
bulk_survival_public_api <- function() {
  c("bulk_survival_public_api",
    "bulk_survival_candidates",
    "bulk_survival_validate_metadata",
    "bulk_survival_split_groups",
    "bulk_survival_km",
    "bulk_survival_cox",
    "plot_survival_km",
    "build_survival_export")
}

#' Colonnes candidates temps/statut dans les métadonnées (filtrage UI)
#'
#' Heuristique NOMINALE uniquement (jamais validatrice — la validation
#' complète reste bulk_survival_validate_metadata) :
#'   - temps : colonnes numériques avec valeurs > 0 ;
#'   - statut : colonnes numériques à <= 3 valeurs distinctes toutes dans
#'     {0, 1, 2}.
#'
#' @param metadata data.frame.
#' @return list(time_cols, status_cols).
bulk_survival_candidates <- function(metadata) {
  if (is.null(metadata) || !is.data.frame(metadata)) {
    return(list(time_cols = character(0), status_cols = character(0)))
  }
  time_cols <- character(0); status_cols <- character(0)
  for (cl in colnames(metadata)) {
    x <- metadata[[cl]]
    if (!is.numeric(x)) next
    xv <- x[is.finite(x)]
    if (length(xv) == 0L) next
    if (all(xv > 0)) time_cols <- c(time_cols, cl)
    if (length(unique(xv)) <= 3L && all(xv %in% c(0, 1, 2))) status_cols <- c(status_cols, cl)
  }
  list(time_cols = time_cols, status_cols = status_cols)
}

#' Validation complète des colonnes temps/statut (garde mission §1)
#'
#' @param metadata data.frame (rownames = échantillons).
#' @param time_col Nom de colonne temps (numérique, > 0).
#' @param status_col Nom de colonne statut (0/1 ; 1/2 si déclaré).
#' @param status_coding "0/1" (0 = censuré, 1 = événement) ou "1/2"
#'   (1 = censuré, 2 = événement).
#' @return list(time, status (tous deux re-calibrés : status 0/1), n_obs,
#'   n_events, n_censored, dropped_na, warnings) — sinon erreur classée.
bulk_survival_validate_metadata <- function(metadata, time_col, status_col,
                                            status_coding = "0/1") {
  if (is.null(metadata) || !is.data.frame(metadata) || nrow(metadata) == 0L) {
    stop(errorCondition("bulk_survival_validate_metadata() : métadonnées vides.",
                        class = "bulk_survival_error", state = "invalid_input"))
  }
  if (length(time_col) != 1L || !time_col %in% colnames(metadata)) {
    stop(errorCondition(sprintf(
      "bulk_survival_validate_metadata() : colonne temps '%s' introuvable (disponibles : %s).",
      time_col, paste(colnames(metadata), collapse = ", ")),
      class = "bulk_survival_error", state = "invalid_input"))
  }
  if (length(status_col) != 1L || !status_col %in% colnames(metadata)) {
    stop(errorCondition(sprintf(
      "bulk_survival_validate_metadata() : colonne statut '%s' introuvable (disponibles : %s).",
      status_col, paste(colnames(metadata), collapse = ", ")),
      class = "bulk_survival_error", state = "invalid_input"))
  }
  time_v <- metadata[[time_col]]
  if (!is.numeric(time_v)) {
    stop(errorCondition(sprintf(
      "Échec survie : la colonne temps '%s' doit être NUMÉRIQUE (type actuel : %s).",
      time_col, class(time_v)[1]),
      class = "bulk_survival_error", state = "invalid_time"))
  }
  status_v <- metadata[[status_col]]
  if (!is.numeric(status_v) && !is.logical(status_v)) {
    stop(errorCondition(sprintf(
      "Échec survie : la colonne statut '%s' doit être numérique 0/1 (type actuel : %s).",
      status_col, class(status_v)[1]),
      class = "bulk_survival_error", state = "invalid_status"))
  }
  status_v <- as.numeric(status_v)
  if (!status_coding %in% c("0/1", "1/2")) {
    stop(errorCondition("bulk_survival_validate_metadata() : status_coding doit être '0/1' ou '1/2'.",
                        class = "bulk_survival_error", state = "invalid_input"))
  }
  allowed <- if (status_coding == "0/1") c(0, 1) else c(1, 2)
  if (!all(status_v[is.finite(status_v)] %in% allowed)) {
    stop(errorCondition(sprintf(
      paste0("Échec survie : la colonne statut '%s' contient des valeurs hors codage %s ",
             "(valeurs observées : %s). Choisissez le bon codage dans l'UI."),
      status_col, status_coding,
      paste(sort(unique(status_v[is.finite(status_v)])), collapse = ", ")),
      class = "bulk_survival_error", state = "invalid_status"))
  }
  status01 <- if (status_coding == "1/2") as.numeric(status_v == 2) else status_v

  warnings <- character(0)
  ok <- is.finite(time_v) & is.finite(status01) & time_v > 0
  dropped_na <- sum(!ok)
  if (dropped_na > 0L) {
    warnings <- c(warnings, sprintf(
      "%d échantillon(s) exclu(s) (temps manquant/négatif ou statut manquant).", dropped_na))
  }
  if (!all(time_v[ok] > 0)) {
    stop(errorCondition("Échec survie : des temps non positifs subsistent après nettoyage.",
                        class = "bulk_survival_error", state = "invalid_time"))
  }
  n_obs <- sum(ok)
  if (n_obs < 3L) {
    stop(errorCondition(sprintf(
      "Échec survie : %d observation(s) valide(s) seulement — au moins 3 requises.", n_obs),
      class = "bulk_survival_error", state = "invalid_input"))
  }
  n_events <- sum(status01[ok] == 1)
  min_ev <- if (exists("TS_BULK_SURV_MIN_EVENTS", inherits = TRUE)) TS_BULK_SURV_MIN_EVENTS else 10L
  if (n_events < min_ev) {
    stop(errorCondition(sprintf(paste0(
      "Échec survie : %d événement(s) observé(s) seulement — au moins %d requis ",
      "(garde de la mission). Les courbes de Kaplan-Meier et les Cox sur moins ",
      "d'événements sont instables et trompeurs."),
      n_events, min_ev),
      class = "bulk_survival_error", state = "min_events"))
  }
  list(
    time        = as.numeric(time_v[ok]),
    status      = as.numeric(status01[ok]),
    samples     = rownames(metadata)[ok],
    n_obs       = n_obs,
    n_events    = n_events,
    n_censored  = n_obs - n_events,
    dropped_na  = dropped_na,
    status_coding = status_coding,
    warnings    = warnings
  )
}

#' Découpe des échantillons par médiane ou quartiles (PAS de cutpoint optimal)
#'
#' médiane -> High/Low ; quartiles -> Q4 (High) vs Q1 (Low), les 50 % du
#' milieu sont EXCLUS (documenté, jamais silencieux).
#'
#' @param feature_values Vecteur numérique nommé (échantillon -> valeur) ou
#'   non nommé (ordre = échantillons validés).
#' @param split "median" ou "quartile" (mission §2.5 — toute autre valeur
#'   est une ERREUR, notamment toute recherche de cutpoint optimal).
#' @return list(groups = factor (High/Low, NA = exclu), split, n_excluded,
#'   threshold_label).
bulk_survival_split_groups <- function(feature_values, split = "median") {
  if (!split %in% c("median", "quartile")) {
    stop(errorCondition(paste0(
      "Échec survie : découpe '", split, "' non autorisée — seuls la médiane ",
      "et les quartiles sont admis (PAS de recherche de cutpoint optimal : ",
      "c'est une pratique de multi-tests qui gonfle artificiellement la ",
      "significativité)."),
      class = "bulk_survival_error", state = "invalid_input"))
  }
  x <- as.numeric(feature_values)
  if (split == "median") {
    thr <- stats::median(x, na.rm = TRUE)
    grp <- ifelse(x > thr, "High", "Low")
    grp[!is.finite(x)] <- NA
    list(groups = factor(grp, levels = c("Low", "High")), split = "median",
         n_excluded = sum(is.na(grp)),
         threshold_label = sprintf("médiane = %.3g", thr))
  } else {
    q <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = TRUE, type = 7)
    grp <- rep(NA_character_, length(x))
    grp[x >= q[2]] <- "High"
    grp[x <= q[1]] <- "Low"
    list(groups = factor(grp, levels = c("Low", "High")), split = "quartile",
         n_excluded = sum(is.na(grp)),
         threshold_label = sprintf("Q1 = %.3g ; Q3 = %.3g (50 %% du milieu exclus)", q[1], q[2]))
  }
}

#' Kaplan-Meier par groupe de découpe + log-rank
#'
#' @param feature_values Vecteur numérique (valeur du gène/score par échantillon
#'   validé — même ordre que surv$time/surv$status).
#' @param surv Résultat de bulk_survival_validate_metadata().
#' @param split "median" ou "quartile".
#' @param feature_label Libellé de la variable (figure/export).
#' @return list(type = "bulk_survival_km", status, feature_label, split,
#'   threshold_label, fit (survfit), logrank_p (survdiff), n_groups,
#'   group_counts, warnings, provenance, timestamp_utc).
bulk_survival_km <- function(feature_values, surv, split = "median",
                             feature_label = "feature") {
  grp <- bulk_survival_split_groups(feature_values, split = split)
  keep <- !is.na(grp$groups)
  if (length(unique(grp$groups[keep])) < 2L) {
    stop(errorCondition("Échec survie : la découpe ne produit pas 2 groupes exploitables.",
                        class = "bulk_survival_error", state = "invalid_input"))
  }
  sdat <- data.frame(time = surv$time[keep], status = surv$status[keep],
                     group = grp$groups[keep])
  fit <- tryCatch(
    survival::survfit(survival::Surv(time, status) ~ group, data = sdat),
    error = function(e) stop(errorCondition(paste0(
      "Échec survie : survfit a échoué — ", conditionMessage(e)),
      class = "bulk_survival_error", state = "compute_failed")))
  sd <- tryCatch(survival::survdiff(survival::Surv(time, status) ~ group, data = sdat),
                 error = function(e) NULL)
  logrank_p <- if (!is.null(sd)) {
    stats::pchisq(q = sd$chisq, df = length(sd$n) - 1, lower.tail = FALSE)
  } else NA_real_
  provenance <- new_provenance_entry(
    analysis_id = "bulk-survival-km",
    method      = "survival::survfit + survdiff (log-rank)",
    parameters  = list(feature = feature_label, split = grp$split,
                       threshold = grp$threshold_label,
                       n_obs = sum(keep), cutpoint_search = FALSE),
    dataset     = data.frame(time = surv$time[keep], status = surv$status[keep]),
    warnings    = surv$warnings
  )
  list(
    type            = "bulk_survival_km",
    status          = "valid",
    feature_label   = feature_label,
    split           = grp$split,
    threshold_label = grp$threshold_label,
    fit             = fit,
    data            = sdat,
    logrank_p       = logrank_p,
    n_groups        = table(grp$groups[keep]),
    n_excluded      = grp$n_excluded,
    warnings        = c(surv$warnings,
                        if (grp$n_excluded > 0L) sprintf(
                          "%d échantillon(s) exclu(s) de la découpe en quartiles (50 %% du milieu).",
                          grp$n_excluded) else character(0)),
    provenance      = provenance,
    timestamp_utc   = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

#' Cox univariés (une variable à la fois) + ajustement BH
#'
#' Chaque variable est testée SEULE (Surv ~ variable, continue) — jamais de
#' panneau de cutpoints ni de sélection itérative. Un ajustement BH est
#' appliqué dès 2 variables et signalé par avertissement.
#'
#' @param feature_matrix Matrice (variables x échantillons) OU data.frame —
#'   colonnes alignées sur surv$samples ; un vecteur nommé est accepté.
#' @param surv Résultat de bulk_survival_validate_metadata().
#' @param max_features Plafond de variables testées (garde UI/temps).
#' @return data.frame(feature, n, events, coef, hr, hr_lower, hr_upper,
#'   z, p, p_adj_BH, concordance) avec avertissements en attribut.
bulk_survival_cox <- function(feature_matrix, surv, max_features = 100L) {
  if (is.null(feature_matrix)) {
    stop(errorCondition("bulk_survival_cox() : aucune variable fournie.",
                        class = "bulk_survival_error", state = "invalid_input"))
  }
  if (is.vector(feature_matrix) || (!is.matrix(feature_matrix) && !is.data.frame(feature_matrix))) {
    feature_matrix <- as.matrix(t(feature_matrix))
  }
  if (is.matrix(feature_matrix)) feature_matrix <- as.data.frame(feature_matrix)
  if (ncol(feature_matrix) > max_features) {
    feature_matrix <- feature_matrix[, seq_len(max_features), drop = FALSE]
  }
  idx <- match(surv$samples, colnames(feature_matrix))
  if (anyNA(idx)) {
    stop(errorCondition(sprintf(
      "bulk_survival_cox() : %d échantillon(s) validé(s) absent(s) de la matrice de variables.",
      sum(is.na(idx))),
      class = "bulk_survival_error", state = "invalid_input"))
  }
  fmat <- as.data.frame(t(feature_matrix[, idx, drop = FALSE]))
  # t() : lignes = échantillons validés, colnames déjà = noms des variables.

  rows <- list()
  for (cl in colnames(fmat)) {
    x <- suppressWarnings(as.numeric(fmat[[cl]]))
    d <- data.frame(time = surv$time, status = surv$status, x = x)
    d <- d[is.finite(d$x), , drop = FALSE]
    if (nrow(d) < 10L || length(unique(d$x)) < 2L) next
    fit <- tryCatch(
      survival::coxph(survival::Surv(time, status) ~ x, data = d),
      error = function(e) NULL)
    if (is.null(fit)) next
    s <- summary(fit)
    ci <- s$conf.int
    rows[[length(rows) + 1L]] <- data.frame(
      feature = cl, n = nrow(d), events = sum(d$status == 1),
      coef = s$coefficients[1, "coef"],
      hr = ci[1, "exp(coef)"], hr_lower = ci[1, "lower .95"], hr_upper = ci[1, "upper .95"],
      z = s$coefficients[1, "z"], p = s$coefficients[1, "Pr(>|z|)"],
      concordance = if (!is.null(s$concordance)) s$concordance["C"] else NA_real_,
      stringsAsFactors = FALSE)
  }
  if (length(rows) == 0L) {
    stop(errorCondition("bulk_survival_cox() : aucune variable ajustable (constante ou trop de valeurs manquantes).",
                        class = "bulk_survival_error", state = "compute_failed"))
  }
  out <- do.call(rbind, rows)
  if (nrow(out) >= 2L) {
    out$p_adj_BH <- stats::p.adjust(out$p, method = "BH")
    attr(out, "warnings") <- sprintf(paste0(
      "%d variables testées — p ajustées (BH) disponibles dans p_adj_BH : ",
      "les p brutes de Cox univariés en rafale gonflent le taux de faux positifs."),
      nrow(out))
  } else {
    out$p_adj_BH <- NA_real_
    attr(out, "warnings") <- character(0)
  }
  out[order(out$p), ]
}

#' Courbe de Kaplan-Meier (survminer si présent, sinon ggplot pur)
#'
#' @param km_result Résultat bulk_survival_km().
#' @param tr Fonction de traduction.
#' @return Objet imprimable : ggsurvplot (si survminer) ou ggplot.
plot_survival_km <- function(km_result, tr = NULL) {
  tr <- tr %||% function(x) x
  sdat <- km_result$data
  if (requireNamespace("survminer", quietly = TRUE)) {
    p <- survminer::ggsurvplot(
      survival::survfit(survival::Surv(time, status) ~ group, data = sdat),
      data = sdat, pval = TRUE, conf.int = TRUE,
      risk.table = TRUE, palette = c("#2980B9", "#E74C3C"),
      legend.labs = levels(sdat$group),
      title = tr(sprintf("Kaplan-Meier — %s (%s)", km_result$feature_label,
                         km_result$threshold_label)),
      xlab = tr("Temps"), ylab = tr("Probabilité de survie"),
      legend.title = "")
    return(p)
  }
  # Repli ggplot pur : courbes en marches depuis la survfit.
  sf <- survival::survfit(survival::Surv(time, status) ~ group, data = sdat)
  summ <- summary(sf)
  df <- data.frame(time = summ$time, surv = summ$surv,
                   group = summ$strata %||% rep("Low", length(summ$time)),
                   stringsAsFactors = FALSE)
  df$group <- sub("^[^=]+=", "", as.character(df$group))
  ggplot2::ggplot(df, ggplot2::aes(time, surv, color = group)) +
    ggplot2::geom_step(linewidth = 0.9) +
    ggplot2::scale_color_manual(values = c(Low = "#2980B9", High = "#E74C3C")) +
    ggplot2::labs(
      title = tr(sprintf("Kaplan-Meier — %s (%s)", km_result$feature_label,
                         km_result$threshold_label)),
      subtitle = tr(sprintf("Log-rank p = %.3g", km_result$logrank_p)),
      x = tr("Temps"), y = tr("Probabilité de survie"), color = NULL) +
    ts_theme("minimal", 12)
}

#' Export plat des Cox univariés
#'
#' @param cox_df data.frame issu de bulk_survival_cox().
#' @param multiple_testing_note Note explicite si >= 2 variables testées.
#' @return Le data.frame avec la note en attribut (exportée via commentaires
#'   RDS/CSV du module).
build_survival_export <- function(cox_df, multiple_testing_note = NULL) {
  if (!is.data.frame(cox_df) || !all(c("feature", "hr", "p") %in% colnames(cox_df))) {
    stop(errorCondition("build_survival_export() : table Cox invalide.",
                        class = "bulk_survival_error", state = "invalid_input"))
  }
  out <- cox_df
  if (is.null(multiple_testing_note) && nrow(cox_df) >= 2L) {
    multiple_testing_note <- attr(cox_df, "warnings")
  }
  attr(out, "multiple_testing_note") <- multiple_testing_note %||% ""
  out
}

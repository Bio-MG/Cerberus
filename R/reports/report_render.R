# =============================================================================
# R/reports/report_render.R — Rapport consolide 4F : RENDU HTML (Stage 17)
# =============================================================================
# Rendu SANS moteur markdown externe (rmarkdown/knitr ecartes volontairement) :
# le rapport est un COMPILATEUR — des chunks R re-evalues n'apporteraient rien
# (aucune analyse a relancer) et un moteur externe n'est pas garanti dans
# l'environnement portable. Sortie = document HTML autonome (css inline,
# UTF-8), construit en htmltools pur — testable hors application Shiny
# (tous les appels htmltools sont namespaces : aucun attach global).
#
# Regles de rendu (contrat CONSOLIDATED_REPORT_CONTRACT.md) :
#   - chaque section d'analyse porte son bandeau d'etat de validation ;
#   - chaque resultat affiche ses analysis_id (tracabilite, gate de release) ;
#   - sections absentes -> message gracieux, jamais une erreur technique ;
#   - sections "blocked" (provenance absente) -> refus explicite, contenu
#     non rendu ;
#   - tables HTML plafonnees par TS_REPORT_MAX_TABLE_ROWS (les exports du
#     bundle ne sont PAS plafonnes) ;
#   - aucune donnee brute (matrices de comptage) n'est embarquee.
# =============================================================================

.report_render_stop <- function(msg) {
  stop(errorCondition(
    msg,
    state = "invalid_input",
    class = "report_error"
  ))
}

# Libellés français figés des domaines (ordre d'affichage = ordre figé du
# collecteur).
.report_domain_labels <- c(
  markers       = "Marqueurs",
  pseudobulk    = "Pseudobulk DE (conditions)",
  correlation   = "Réseau de corrélation (gène cible)",
  pathways      = "Enrichissement de pathways",
  trajectory    = "Trajectoire / pseudotemps",
  velocity      = "RNA velocity",
  communication = "Communication cellulaire",
  da_design     = "Abondance différentielle — design expérimental",
  da_milo       = "Abondance différentielle — Milo (voisinages)",
  da_sccoda     = "Abondance différentielle — scCODA (composition)",
  da_cross      = "Vues croisées Milo × scCODA",
  bulk_multi_comparison = "Comparaison multi-jeux Bulk (DEGs)"
)

.report_banner_colors <- c(
  absent       = list(bg = "#f4f5f7", fg = "#555b60", border = "#cfd4d9"),
  valid        = list(bg = "#e8f5e9", fg = "#1b5e20", border = "#a5d6a7"),
  valid_legacy = list(bg = "#f1f8e9", fg = "#33691e", border = "#c5e1a5"),
  stale        = list(bg = "#fff3e0", fg = "#a05a00", border = "#ffcc80"),
  invalid      = list(bg = "#fdecea", fg = "#b71c1c", border = "#f5c6cb"),
  unknown      = list(bg = "#fff8e1", fg = "#8a6d00", border = "#ffe082"),
  blocked      = list(bg = "#fdecea", fg = "#b71c1c", border = "#f5c6cb")
)

.report_css <- paste(
  "body{font-family:'Segoe UI',Arial,sans-serif;color:#212529;margin:32px auto;",
  "  max-width:980px;padding:0 16px;line-height:1.45;}",
  "h1{font-size:1.5em;border-bottom:3px solid #2C3E50;padding-bottom:6px;}",
  "h2{font-size:1.2em;color:#2C3E50;margin-top:28px;border-bottom:1px solid #d9dde1;",
  "  padding-bottom:3px;}",
  "h3{font-size:1.02em;margin:18px 0 6px;}",
  ".subtitle{color:#5a6570;font-size:0.95em;}",
  ".banner{padding:7px 10px;border-radius:5px;margin:8px 0;font-size:0.86em;",
  "  border-left:4px solid;}",
  "table{border-collapse:collapse;width:100%;font-size:0.82em;margin:8px 0;}",
  "th{background:#2C3E50;color:#fff;text-align:left;padding:4px 7px;}",
  "td{border-bottom:1px solid #e3e6e8;padding:3px 7px;vertical-align:top;}",
  "tr:nth-child(even) td{background:#f7f9fa;}",
  "pre{background:#f4f5f7;border:1px solid #d9dde1;border-radius:5px;",
  "  padding:10px;font-size:0.72em;white-space:pre-wrap;word-break:break-word;",
  "  max-height:420px;overflow-y:auto;}",
  ".ids{font-family:Consolas,monospace;font-size:0.78em;color:#18BC9C;}",
  ".note{color:#5a6570;font-size:0.78em;}",
  ".footer{margin-top:34px;border-top:1px solid #d9dde1;padding-top:8px;",
  "  color:#5a6570;font-size:0.75em;}",
  "@media print{body{margin:8mm;max-width:none;}pre{max-height:none;}}"
, sep = "\n")

#' Table HTML plafonnée (affichage) — les exports restent complets
.report_html_table <- function(df, max_rows = TS_REPORT_MAX_TABLE_ROWS,
                               caption = NULL) {
  tg <- htmltools::tags
  if (!is.data.frame(df)) {
    return(tg$p(class = "note", "Table non disponible."))
  }
  if (nrow(df) == 0L) {
    return(tg$p(class = "note", "Aucune entrée."))
  }
  df <- df[seq_len(min(nrow(df), max_rows)), , drop = FALSE]
  cells <- lapply(seq_len(nrow(df)), function(i) {
    do.call(tg$tr, lapply(vapply(df[i, ], function(v) {
      paste(format(v), collapse = ", ")
    }, character(1)), function(v) tg$td(if (is.na(v) || !nzchar(v)) "" else v)))
  })
  tg$table(
    if (!is.null(caption)) tg$caption(class = "note", caption) else NULL,
    tg$thead(tg$tr(lapply(colnames(df), function(cn) tg$th(cn)))),
    tg$tbody(cells),
    if (nrow(df) >= max_rows)
      tg$p(class = "note", sprintf(
        "Affichage plafonné à %d lignes — export complet dans le bundle.", max_rows))
    else NULL
  )
}

#' Bandeau d'état de validation d'une section
.report_banner <- function(state, label) {
  tg <- htmltools::tags
  col <- .report_banner_colors[[state]] %||% .report_banner_colors[["unknown"]]
  tg$div(class = "banner",
         style = sprintf("background:%s;color:%s;border-left-color:%s;",
                         col$bg, col$fg, col$border),
         label)
}

#' Construire le contenu HTML du rapport consolidé (COMPILATEUR — pure vue)
#'
#' @param report_input Sortie de \code{collect_consolidated_report_input()}.
#' @param validation Sortie de \code{validate_consolidated_report_input()}.
#' @return Objet \code{shiny.tag}/tagList htmltools (testable hors Shiny).
#' @errors \code{report_error} si les entrées ne sont pas celles attendues.
build_consolidated_report_html <- function(report_input, validation) {
  tg <- htmltools::tags
  tl <- htmltools::tagList
  if (!is.list(report_input) ||
      !identical(report_input$type, "consolidated_report_input")) {
    .report_render_stop(paste0(
      "build_consolidated_report_html() : attend la sortie de ",
      "collect_consolidated_report_input()."
    ))
  }
  if (!is.list(validation) ||
      !identical(validation$type, "consolidated_report_validation")) {
    .report_render_stop(paste0(
      "build_consolidated_report_html() : attend la sortie de ",
      "validate_consolidated_report_input()."
    ))
  }
  opts <- report_input$options
  ds <- report_input$dataset

  # ── En-tête ─────────────────────────────────────────────────────────────────
  header <- tl(
    tg$h1(opts$title),
    if (nzchar(opts$subtitle)) tg$p(class = "subtitle", opts$subtitle) else NULL,
    tg$p(class = "subtitle", sprintf(
      "Généré le %s · %d cellules × %d gènes · backend : %s",
      format(report_input$generated_at, "%Y-%m-%d %H:%M:%S"),
      ds$n_cells, ds$n_genes, ds$backend
    )),
    if (nzchar(opts$notes)) tg$blockquote(class = "subtitle", opts$notes) else NULL
  )

  # ── Jeu de données ─────────────────────────────────────────────────────────
  idents_str <- if (is.data.frame(ds$identities) && nrow(ds$identities) > 0L)
    paste0(ds$identities[[1]], " (", ds$identities[[2]], ")", collapse = ", ") else ""
  dataset_section <- tl(
    tg$h2("1. Jeu de données"),
    .report_html_table(.report_kv_df(c(
      n_cellules = ds$n_cells, n_genes = ds$n_genes,
      assays = paste(ds$assays, collapse = "; "),
      assay_defaut = ds$default_assay,
      reductions = paste(ds$reductions, collapse = "; "),
      identites = idents_str,
      colonnes_metadata = paste(ds$metadata_columns, collapse = "; "),
      backend = ds$backend
    ))),
    tg$p(class = "note", sprintf(
      "Empreinte objet (v2) : %s",
      ds$fingerprint$fingerprint %||% "indisponible"
    ))
  )

  # ── Pipeline ────────────────────────────────────────────────────────────────
  pipeline_section <- tl(
    tg$h2("2. Pipeline"),
    .report_html_table(report_input$pipeline$summary),
    if (isTRUE(report_input$pipeline$qc_present))
      tg$p(class = "note", "Snapshot QC du panneau 1 présent dans l'état (restitué tel quel).")
    else tg$p(class = "note", "Aucun snapshot QC disponible dans l'état.")
  )

  # ── Analyses (une sous-section par domaine figé) ───────────────────────────
  analysis_blocks <- lapply(validation$verdicts, function(v) {
    ids <- if (length(v$analysis_ids))
      paste(v$analysis_ids, collapse = ", ") else "—"
    communication <- report_input$analyses$communication
    has_liana <- identical(v$section, "communication") &&
      !identical(v$state, "blocked") && isTRUE(communication$present) &&
      is.data.frame(communication$extras$sample_manifest) &&
      is.data.frame(communication$extras$condition_summary) &&
      is.data.frame(communication$extras$by_sample)
    liana_note <- if (has_liana) {
      tg$p(
        class = "note",
        paste0(
          "Tableaux descriptifs uniquement : l'échantillon biologique est ",
          "l'unité de réplication ; les cellules ne sont pas des réplicats. ",
          "Aucun test entre conditions ni conclusion causale n'est effectué."
        )
      )
    } else {
      NULL
    }
    liana_tables <- if (has_liana && isTRUE(report_input$options$include_tables)) {
      tl(
        tg$h4("Manifeste des échantillons"),
        .report_html_table(communication$extras$sample_manifest),
        tg$h4("Résumé descriptif des conditions"),
        .report_html_table(communication$extras$condition_summary),
        tg$h4("Interactions canoniques par échantillon"),
        .report_html_table(communication$extras$by_sample)
      )
    } else {
      NULL
    }
    tl(
      tg$h3(.report_domain_labels[[v$section]] %||% v$section),
      .report_banner(v$state, v$label),
      if (length(v$identity_note)) tg$p(class = "note", v$identity_note) else NULL,
      tg$p(tg$strong("analysis_id : "), tg$span(class = "ids", ids)),
       if (isTRUE(report_input$options$include_tables))
         .report_html_table(v$summary, caption = "Résumé descriptif (aucune re-analyse)")
       else NULL,
       liana_note,
       liana_tables
    )
  })
  analyses_section <- tl(tg$h2("3. Analyses"), analysis_blocks)

  # ── Reproductibilité ────────────────────────────────────────────────────────
  prov <- report_input$provenance_df
  repro_section <- tl(
    tg$h2("4. Reproductibilité"),
    tg$h3("Manifeste de provenance consolidé"),
    .report_html_table(prov, max_rows = TS_REPORT_MAX_PROVENANCE_ROWS,
                       caption = "Entrées PRODUITES à chaque étape d'analyse (règle 7) — consolidées ici, jamais reconstruites."),
    tg$h3("Configuration (constantes TS_*)"),
    .report_html_table(report_input$config_snapshot),
    tg$h3("Versions logicielles clés"),
    .report_html_table(report_input$session_info$key_packages),
    tg$h3("Session R"),
    tg$pre(report_input$session_info$full_text),
    tg$h3("Limitations connues"),
    tg$ul(
      tg$li("Rapport compilé : aucune analyse n'est ré-exécutée ; les sections absentes sont signalées, jamais fabriquées."),
      tg$li("Les visualisations sauvegardées ne sont pas embarquées dans cette version — le rapport par domaine (panneau 9) les restitue."),
      tg$li("L'identité résultat ↔ objet courant est vérifiée uniquement pour les domaines à contrat (velocity, communication, DA) via l'empreinte v2."),
      tg$li("Aucune donnée brute (matrices de comptage) n'est embarquée — seuls des résumés et des tables de résultats sont exportés.")
    )
  )

  footer <- tg$div(class = "footer", sprintf(
    "Rapport consolidé Cerberus (4F) · analysis_id : %s · version %s · compilateur d'état et de provenance — aucune analyse ré-exécutée.",
    report_input$analysis_id, report_input$version
  ))

  tl(header, dataset_section, pipeline_section, analyses_section,
     repro_section, footer)
}

#' Écrire le rapport consolidé en fichier HTML autonome (css inline, UTF-8)
#'
#' @param report_input Sortie du collecteur.
#' @param validation Sortie du validateur.
#' @param path Chemin du fichier .html à écrire.
#' @return Le chemin, invisible.
write_consolidated_report_html <- function(report_input, validation, path) {
  tg <- htmltools::tags
  content <- build_consolidated_report_html(report_input, validation)
  doc <- tg$html(
    tg$head(
      tg$meta(charset = "utf-8"),
      tg$title(report_input$options$title %||% "Rapport consolidé"),
      tg$style(htmltools::HTML(.report_css))
    ),
    tg$body(content)
  )
  html_str <- as.character(htmltools::doRenderTags(doc))
  con <- file(path, open = "wb", encoding = "native.enc")
  on.exit(close(con), add = TRUE)
  writeLines(enc2utf8(html_str), con = con, useBytes = TRUE)
  invisible(path)
}

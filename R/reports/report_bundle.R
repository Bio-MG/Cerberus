# =============================================================================
# R/reports/report_bundle.R — Rapport consolide 4F : BUNDLE EXPORT (Stage 17)
# =============================================================================
# Assemble le bundle d'export projet : rapport HTML + manifeste de sections +
# provenance consolidee + tables de resultats (copies fidèles des objets
# canoniques — JAMAIS recalculées) + script R reproductible (capture a la
# collection) + session info + README. Aucune donnee brute (matrices de
# comptage) n'est embarquee par defaut.
#
# Tables exportees (champs figes des contrats) :
#   markers            -> marqueurs.csv
#   pathways           -> pathways.csv
#   communication      -> communication_canonical.csv
#   da_design          -> da_design_conditions.csv / da_design_samples.csv /
#                         da_design_exclusions.csv
#   da_milo            -> milo_da_table.csv / milo_neighbourhood_summary.csv /
#                         milo_sample_composition.csv (l'affectation
#                         cellule-à-voisinage, volumineuse, est volontairement
#                         exclue)
#   da_sccoda          -> sccoda_effect_table.csv / sccoda_composition_table.csv
#   da_cross           -> da_cross_concordance.csv / da_cross_disagreement.csv
#   bulk_multi_comparison (4F-EXT) -> bulk_multi_per_dataset.csv /
#                         bulk_multi_concordance.csv / bulk_multi_intersection.csv
# =============================================================================

.report_bundle_stop <- function(msg) {
  stop(errorCondition(
    msg,
    state = "invalid_input",
    class = "report_error"
  ))
}

#' Nom de fichier d'export du rapport consolidé
#'
#' @param kind "rapport_consolide" (fichier HTML) ou "bundle_consolide" (zip).
#' @param ext Extension ("html", "zip").
#' @return \code{<kind>_sc-report-consolide_<date>.<ext>} (pattern des autres
#'   domaines : kind + analysis_id + date).
consolidated_report_export_filename <- function(kind, ext) {
  sprintf(
    "%s_%s_%s.%s",
    as.character(kind)[1L],
    "sc-report-consolide",
    format(Sys.Date(), "%Y-%m-%d"),
    as.character(ext)[1L]
  )
}

# Extraire les tables exportables d'un domaine (data.frames uniquement —
# copies fideles ; rien n'est reconstruit ni re-derive).
.report_bundle_tables <- function(report_input, validation) {
  A <- report_input$analyses
  V <- validation$verdicts
  state_of <- function(dm) V[[dm]]$state
  out <- list()  # named list: filename -> list(df, section, contenu)
  add <- function(file, df, section, contenu) {
    if (is.data.frame(df) && nrow(df) > 0L) {
      out[[file]] <<- list(df = df, section = section, contenu = contenu)
    }
  }
  # Domaines legacy : tables completes capturees par le collecteur (extras).
  if (state_of("markers") != "absent")
    add("marqueurs.csv", A$markers$extras$table, "markers", "table des marqueurs")
  if (state_of("correlation") != "absent")
    add("correlation.csv", A$correlation$extras$table, "correlation",
        "table de correlation (gene cible)")
  if (state_of("pseudobulk") != "absent")
    add("pseudobulk_de.csv", A$pseudobulk$extras$de_table, "pseudobulk",
        "resultats DE pseudobulk")
  if (state_of("pathways") != "absent")
    add("pathways.csv", A$pathways$extras$table, "pathways", "table pathways")
  if (!state_of("communication") %in% c("absent", "blocked")) {
    add("communication_canonical.csv",
        A$communication$extras$canonical_table, "communication", "table canonique")
    add("communication_sample_manifest.csv",
        A$communication$extras$sample_manifest, "communication",
        "manifeste des echantillons")
    add("communication_condition_summary.csv",
        A$communication$extras$condition_summary, "communication",
        "resume descriptif des conditions")
    add("communication_by_sample.csv",
        A$communication$extras$by_sample, "communication",
        "table canonique par echantillon")
  }
  add("da_design_conditions.csv", A$da_design$extras$condition_summary,
      "da_design", "resume conditions")
  add("da_design_samples.csv", A$da_design$extras$sample_summary,
      "da_design", "resume echantillons")
  add("da_design_exclusions.csv", A$da_design$extras$exclusions,
      "da_design", "exclusions")
  add("milo_da_table.csv", A$da_milo$extras$DA_table,
      "da_milo", "table DA par voisinage")
  add("milo_neighbourhood_summary.csv", A$da_milo$extras$neighbourhood_summary,
      "da_milo", "resume voisinages")
  add("milo_sample_composition.csv", A$da_milo$extras$sample_composition,
      "da_milo", "composition par echantillon")
  add("sccoda_effect_table.csv", A$da_sccoda$extras$effect_table,
      "da_sccoda", "table effets")
  add("sccoda_composition_table.csv", A$da_sccoda$extras$composition_table,
      "da_sccoda", "matrice de composition analysee")
  conc <- A$da_cross$extras$method_summary$concordance %||% NULL
  disa <- A$da_cross$extras$method_summary$disagreement %||% NULL
  add("da_cross_concordance.csv", conc, "da_cross", "concordance par identite")
  add("da_cross_disagreement.csv", disa, "da_cross", "desaccords")
  # 4F-EXT : comparaison multi-jeux bulk (resultat global auto-date).
  add("bulk_multi_per_dataset.csv", A$bulk_multi_comparison$extras$per_dataset,
      "bulk_multi_comparison", "detail par dataset compare")
  add("bulk_multi_concordance.csv", A$bulk_multi_comparison$extras$concordance,
      "bulk_multi_comparison", "concordance de direction par paire")
  add("bulk_multi_intersection.csv", A$bulk_multi_comparison$extras$intersection_dt,
      "bulk_multi_comparison", "intersection des DEGs")
  out
}

.report_bundle_readme <- function(report_input, validation) {
  paste0(
    "BUNDLE D'EXPORT PROJET — RAPPORT CONSOLIDÉ (TranscriptoShiny, 4F)\n",
    "=================================================================\n\n",
    "analysis_id : ", report_input$analysis_id, "\n",
    "Généré le : ", format(report_input$generated_at, "%Y-%m-%d %H:%M:%S"), "\n\n",
    "CONTENU\n",
    "  rapport_consolide.html        rapport compilé (état + provenance)\n",
    "  manifest_sections.csv         sections, états, analysis_id, fichiers\n",
    "  provenance.csv                manifeste de provenance consolidé\n",
    "  tables/*.csv                  tables de résultats (copies fidèles)\n",
    "  script_analyse_reproductible.R  script SC reproductible (si disponible)\n",
    "  session_info.txt              session R complète\n\n",
    "PRINCIPE\n",
    "  Le rapport est un COMPILATEUR : aucune analyse n'est ré-exécutée, les\n",
    "  sections absentes sont signalées (jamais fabriquées) et les sections\n",
    "  sans provenance sont refusées (règle 7).\n\n",
    "LIMITATIONS\n",
    "  - Visualisations non embarquées (voir le rapport par domaine, panneau 9).\n",
    "  - Identité résultat ↔ objet vérifiée pour les domaines à contrat via\n",
    "    l'empreinte v2 uniquement.\n",
    "  - Aucune donnée brute (matrices de comptage) n'est embarquée.\n\n",
    "STATUTS DES SECTIONS\n",
    paste(vapply(validation$verdicts, function(v)
      sprintf("  %-12s %-13s %s", v$section, v$state,
              paste(v$analysis_ids, collapse = ", ")), character(1)),
      collapse = "\n"),
    "\n"
  )
}

#' Assembler le bundle d'export projet (rapport + manifeste + tables + script)
#'
#' @param bundle_dir Répertoire cible (créé si absent ; contenu existant
#'   conservé — l'appelant fournit un répertoire temporaire dédié).
#' @param report_input Sortie de \code{collect_consolidated_report_input()}.
#' @param validation Sortie de \code{validate_consolidated_report_input()}.
#' @return Liste invisible : \code{bundle_dir}, \code{files} (chemins relatifs
#'   écrits), \code{manifest_df}, \code{html_path}.
#' @errors \code{report_error} si les entrées sont invalides.
build_report_bundle <- function(bundle_dir, report_input, validation) {
  if (!is.list(report_input) ||
      !identical(report_input$type, "consolidated_report_input") ||
      !is.list(validation) ||
      !identical(validation$type, "consolidated_report_validation")) {
    .report_bundle_stop(paste0(
      "build_report_bundle() : attend la sortie du collecteur et du ",
      "validateur du rapport consolidé."
    ))
  }
  dir.create(bundle_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(bundle_dir, "tables"), recursive = TRUE, showWarnings = FALSE)
  written <- character(0)

  # 1. Rapport HTML autonome
  html_path <- file.path(bundle_dir, "rapport_consolide.html")
  write_consolidated_report_html(report_input, validation, html_path)
  written <- c(written, "rapport_consolide.html")

  # 2. Tables exportables (copies fideles des resultats canoniques)
  tables <- .report_bundle_tables(report_input, validation)
  table_rows <- lapply(names(tables), function(nm) {
    t <- tables[[nm]]
    rel <- file.path("tables", nm)
    utils::write.csv(t$df, file.path(bundle_dir, rel), row.names = FALSE,
                     fileEncoding = "UTF-8")
    written <<- c(written, rel)
     ids <- validation$verdicts[[t$section]]$analysis_ids %||% character(0)
     data.frame(section = t$section, fichier = rel, etat = "export",
                analysis_ids = paste(ids, collapse = ","),
                contenu = t$contenu, stringsAsFactors = FALSE)
  })

  # 3. Provenance consolidee
  prov_path <- file.path(bundle_dir, "provenance.csv")
  utils::write.csv(report_input$provenance_df, prov_path, row.names = FALSE,
                   fileEncoding = "UTF-8")
  written <- c(written, "provenance.csv")

  # 4. Script R reproductible (capture a la collection)
  if (!is.null(report_input$r_script_text) &&
      nzchar(report_input$r_script_text)) {
    script_path <- file.path(bundle_dir, "script_analyse_reproductible.R")
    writeLines(report_input$r_script_text, script_path, useBytes = TRUE)
    written <- c(written, "script_analyse_reproductible.R")
  }

  # 5. Session info
  si_path <- file.path(bundle_dir, "session_info.txt")
  writeLines(report_input$session_info$full_text, si_path, useBytes = TRUE)
  written <- c(written, "session_info.txt")

  # 6. Manifeste des sections (verdicts du validateur) — ecrit en DERNIER
  section_rows <- lapply(validation$verdicts, function(v) {
    data.frame(section = v$section, fichier = "", etat = v$state,
               analysis_ids = paste(v$analysis_ids, collapse = ","),
               contenu = v$label, stringsAsFactors = FALSE)
  })
  extra_rows <- list(
    data.frame(section = "rapport", fichier = "rapport_consolide.html",
               etat = "export", analysis_ids = report_input$analysis_id,
               contenu = "rapport compile", stringsAsFactors = FALSE),
    data.frame(section = "provenance", fichier = "provenance.csv",
               etat = "export", analysis_ids = report_input$analysis_id,
               contenu = "manifeste de provenance consolide",
               stringsAsFactors = FALSE)
  )
  if (!is.null(report_input$r_script_text) && nzchar(report_input$r_script_text)) {
    extra_rows <- c(extra_rows, list(
      data.frame(section = "script", fichier = "script_analyse_reproductible.R",
                 etat = "export", analysis_ids = report_input$analysis_id,
                 contenu = "script R reproductible", stringsAsFactors = FALSE)))
  }
  manifest_df <- do.call(rbind, c(section_rows, table_rows, extra_rows))
  utils::write.csv(manifest_df, file.path(bundle_dir, "manifest_sections.csv"),
                   row.names = FALSE, fileEncoding = "UTF-8")
  written <- c(written, "manifest_sections.csv")

  # 7. README (description + limitations + statuts)
  writeLines(.report_bundle_readme(report_input, validation),
             file.path(bundle_dir, "README.txt"), useBytes = TRUE)
  written <- c(written, "README.txt")

  invisible(list(
    bundle_dir = bundle_dir,
    files = written,
    manifest_df = manifest_df,
    html_path = html_path
  ))
}

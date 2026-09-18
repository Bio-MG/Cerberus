# =============================================================================
# mod_bulk_report.R  —  Bulk Child 4: Rapport HTML/PDF + Export Script R reproductible
# =============================================================================
# State contract (shared_rv) READ ONLY:
#   vst_mat, contrasts, active_contrast, pathway_results, pca_color_by,
#   pca_palette, pca_manual_colors, volcano_role_colors, lfc_thresh,
#   padj_thresh, heatmap_top_n, pathway_db,
#   active_condition_col   <- Step-3.0: mirrors DE condition column name
#   multimethod_de, multimethod_consensus <- Step-3.5: mirrored by mod_bulk_de.R
# =============================================================================


mod_bulk_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-light", style = "font-size:0.85em;border-left:3px solid #2C3E50;",
        i18n$t("Rapport autonome (PCA, QC, Volcano, Heatmap, Pathways) \u2014 partageable sans R.")),

    textInput(ns("report_title"), i18n$t("Titre du rapport"),
              value = i18n_plain("Analyse RNA-seq Bulk"), placeholder = "Ex: Projet — Cov2 vs Mock"),
    textInput(ns("report_subtitle"), i18n$t("Sous-titre (optionnel)"), placeholder = "Ex: GSE164073"),
    textAreaInput(ns("report_notes"), i18n$t("Notes / Commentaires (markdown supporté)"),
                  rows = 3, placeholder = "Ex: file GSE164073, donor D3."),

    checkboxGroupInput(ns("report_sections"), i18n$t("Sections \u00e0 inclure"),
      choices = stats::setNames(c("pca","qc","volcano","heatmap","table","pathway"),
        c("PCA", .tr_plain("QC \u00c9chantillons"), .tr_plain("Volcano + MA-Plot"), .tr_plain("Heatmap Top G\u00e8nes"), .tr_plain("Table DE compl\u00e8te"), "Pathway Enrichment")),
      selected = c("pca", "qc", "volcano", "heatmap", "table", "pathway")),

    # Step-3.5: choix de mise en page pour la section "Toutes les paires"
    # (Volcano/MA/Heatmap pairwise) — grille compacte ou 1 plot pleine page.
    radioButtons(ns("pairwise_layout"), i18n$t("Mise en page \"Toutes les paires\" (Volcano/MA/Heatmap)"),
      choices = stats::setNames(c("grid","full"),
        c(.tr_plain("Petits multiples compil\u00e9s (grille)"), .tr_plain("Grand format (1 par contraste)"))),
      selected = "grid", inline = TRUE),

    radioButtons(ns("report_format"), i18n$t("Format de sortie"),
      choices = stats::setNames(c("html","pdf","both"),
        c(.tr_plain("HTML interactif"), .tr_plain("PDF statique"), .tr_plain("Les deux (.zip)"))),
      selected = "html"),

    conditionalPanel(condition = "input.report_format != 'pdf'", ns = ns,
      checkboxInput(ns("report_interactive"),
                    i18n$t("Graphiques interactifs (PCA, Volcano, MA individuel) — HTML uniquement"), value = TRUE)),
    div(class = "small text-muted", i18n$t("Le PDF requiert LaTeX (tinytex::install_tinytex()).")),

    downloadButton(ns("dl_report"), i18n$t("📄 Générer le Rapport"),
                   class = "btn-dark w-100 mt-2"),
    hr(),
    div(class = "alert alert-light", style = "font-size:0.82em;border-left:3px solid #18BC9C;",
        bsicons::bs_icon("code-slash"),
        " ",
        i18n$t("Export script R reproductible (.zip) \u2014 contient le script + counts_raw.rds + metadata.rds. Pour l'ex\u00e9cuter : d\u00e9compressez le .zip, ouvrez R/RStudio dans CE dossier (ou faites setwd() dessus), puis source(\"analyse_bulk_....R\") ou ex\u00e9cutez-le ligne par ligne.")),
    downloadButton(ns("dl_r_script"), i18n$t("🧾 Export Script R Reproductible (.zip)"),
                   class = "btn-outline-secondary w-100"),
    div(class = "small text-muted mt-1", textOutput(ns("report_status")))
  )
}

mod_bulk_report_server <- function(id, global_data, shared_rv) {
  moduleServer(id, function(input, output, session) {

    .tr <- function(key) {
      tr <- global_data$i18n
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }


    observe({
      shinyjs::toggleState("dl_report",   condition = !is.null(shared_rv$vst_mat))
      shinyjs::toggleState("dl_r_script", condition = !is.null(shared_rv$filtered_counts))
    })

    # ── i18n: push translated labels/choices on language switch ──────────
    # (.tr_plain() froze the choice DISPLAY NAMES at build time — this observer
    # re-sends them in the CURRENT session language; values never change.)
    observeEvent(global_data$language, {
      updateTextInput(session, "report_title",    label = .tr("Titre du rapport"))
      updateTextInput(session, "report_subtitle", label = .tr("Sous-titre (optionnel)"))
      updateTextAreaInput(session, "report_notes", label = .tr("Notes / Commentaires (markdown support\u00e9)"))
      updateCheckboxGroupInput(session, "report_sections", label = .tr("Sections \u00e0 inclure"),
        choices = stats::setNames(c("pca","qc","volcano","heatmap","table","pathway"),
          c("PCA", .tr("QC \u00c9chantillons"), .tr("Volcano + MA-Plot"), .tr("Heatmap Top G\u00e8nes"),
            .tr("Table DE compl\u00e8te"), "Pathway Enrichment")))
      updateRadioButtons(session, "pairwise_layout",
        label = .tr("Mise en page \"Toutes les paires\" (Volcano/MA/Heatmap)"),
        choices = stats::setNames(c("grid","full"),
          c(.tr("Petits multiples compil\u00e9s (grille)"), .tr("Grand format (1 par contraste)"))))
      updateRadioButtons(session, "report_format", label = .tr("Format de sortie"),
        choices = stats::setNames(c("html","pdf","both"),
          c(.tr("HTML interactif"), .tr("PDF statique"), .tr("Les deux (.zip)"))))
      updateCheckboxInput(session, "report_interactive",
        label = .tr("Graphiques interactifs (PCA, Volcano, MA individuel) \u2014 HTML uniquement"))
    }, ignoreInit = TRUE)

    output$report_status <- renderText({
      global_data$language
      if (is.null(shared_rv$vst_mat)) .tr("Lancez d'abord l'\u00e9tape 1 (Filtrage & VST).")
      else .tr("Pr\u00eat \u2014 s\u00e9lectionnez les sections puis cliquez sur 'G\u00e9n\u00e9rer Rapport'.")
    })

    # ── HTML / PDF Report ──────────────────────────────────────────────────
    output$dl_report <- downloadHandler(
      filename = function() {
        ext <- switch(input$report_format, html = "html", pdf = "pdf", both = "zip")
        # Step-3.5: horodatage à la seconde — évite l'écrasement si plusieurs
        # exports sont générés le même jour (Sys.Date() seul ne suffisait pas).
        paste0("rapport_bulk_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
      },
      content = function(file) {
        req(shared_rv$vst_mat)
        ac        <- shared_rv$active_contrast
        de_results <- if (!is.null(ac) && ac %in% names(shared_rv$contrasts))
                        shared_rv$contrasts[[ac]] else NULL
        padj_now  <- shared_rv$padj_thresh %||% 0.05
        lfc_now   <- shared_rv$lfc_thresh  %||% 1
        all_c     <- shared_rv$contrasts %||% list()
        all_c_sum <- if (length(all_c) == 0) NULL else
                       summarize_contrasts_updown(all_c, lfc_now, padj_now, ac)

        template_path <- tryCatch(.find_bulk_report_template(), error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = 15)
          NULL
        })
        req(template_path)
        tmp_rmd <- file.path(tempdir(), "bulk_report_template.Rmd")
        file.copy(template_path, tmp_rmd, overwrite = TRUE)

        # ── Phase 4: resolve ALL report strings for the CURRENT language ────
        # The Rmd is decoupled from shiny.i18n — it receives a plain named
        # list, never the Translator. Missing keys fall back to French (the key).
        local_tr <- .tr_fn(global_data)
        report_keys <- c(
          "Résumé", "Aucune analyse différentielle disponible au moment de l'export.",
          "Tous les contrastes calculés", "Seuils appliqués : |Log2FC| > {lfc}, P-adj < {padj}.",
          "Contraste", "Gènes testés",
          "Résumé de TOUS les contrastes calculés dans cette session (pas seulement le contraste actif détaillé plus bas).",
          "Comparaison des gènes significatifs entre contrastes (UpSet)", "Diagramme non disponible :",
          "PCA — Échantillons", "QC — Corrélation Inter-Échantillons",
          "Détecte les échantillons mal étiquetés ou les outliers avant interprétation du contraste.",
          "Volcano Plot & MA-Plot", "Heatmap — Top Gènes Différentiels",
          "Pas assez de gènes communs pour générer la heatmap.",
          "Toutes les paires — Volcano & MA-Plot", "grille compacte", "grand format (1 par contraste)",
          "Package 'patchwork' manquant — grille ignorée.", "Heatmap top gènes — par paire",
          "Table Complète — Expression Différentielle",
          "Top 100 gènes (table complète disponible en export CSV depuis l'application)",
          "Pathway Enrichment", "Multi-méthodes — DESeq2 / edgeR / limma-voom",
          "Table consensus (top 20 par rang moyen)",
          "Consensus de rang — table complète disponible en export CSV depuis l'app.",
          "Aucune comparaison multi-méthodes disponible — cliquez \"Comparer\" avant l'export.",
          "Rapport généré automatiquement par TranscriptoShiny — module Bulk RNA-seq.",
          "Contraste actif : {c} | Échantillons : {n} | Gènes testés : {g} | Gènes significatifs : {s}",
          "Petits multiples pour chacun des {n} contrastes calculés — mise en page : {layout}.",
          "Heatmaps par paire non affichées ({n} contrastes > 6) — changez le contraste actif dans l'onglet Heatmap.",
          "Comparaison du même contraste avec {n} méthodes : {m}.",
          # shared plot helpers used by the report
          "Volcano Plot — Analyse Différentielle", "Log2 Fold Change", "-Log10(P-adj)", "Statut",
          "MA-Plot", "Log10(Expression Moyenne + 1)", "Heatmap — Gènes Différentiels", "Z-score",
          "Corrélation Inter-Échantillons (QC)", "PCA — Échantillons Bulk RNA-seq",
          "Top", "Pathways", "Nombre de gènes", "Dotplot Pathways", "Ratio de gènes", "Nb Gènes"
        )
        i18n_strings <- stats::setNames(vapply(report_keys, local_tr, character(1)), report_keys)

        render_params <- list(
          vst_mat               = shared_rv$vst_mat,
          metadata              = global_data$bulk_obj$metadata,
          de_results            = de_results,
          contrast_name         = ac,
          all_contrasts_summary = all_c_sum,
          all_contrasts         = all_c,
          pathway_results       = shared_rv$pathway_results,
          pathway_db            = shared_rv$pathway_db %||% "GOBP",
          sections              = input$report_sections %||% character(0),
          pca_color_by          = if (nzchar(shared_rv$pca_color_by %||% "")) shared_rv$pca_color_by else NULL,
          pca_palette           = shared_rv$bulk_palette %||% "default",
          pca_manual_colors     = shared_rv$pca_manual_colors,
          role_colors           = shared_rv$volcano_role_colors,
          heatmap_top_n         = shared_rv$heatmap_top_n %||% 30,
          lfc_thresh            = lfc_now,
          padj_thresh           = padj_now,
          # Step-3.5: Multi-méthodes (consensus DESeq2/edgeR/limma) — miroir
          # écrit par mod_bulk_de.R après un run "🔬 Comparer".
          multimethod_de        = shared_rv$multimethod_de,
          multimethod_consensus = shared_rv$multimethod_consensus,
          # Step-3.5: choix de mise en page pour "Toutes les paires".
          pairwise_layout       = input$pairwise_layout %||% "grid",
          report_title          = if (is.null(input$report_title) || nchar(input$report_title) == 0) .tr("Analyse RNA-seq Bulk") else input$report_title,
          report_subtitle       = if (is.null(input$report_subtitle) || nchar(input$report_subtitle) == 0) "" else input$report_subtitle,
          report_notes          = if (is.null(input$report_notes) || nchar(input$report_notes) == 0) "" else input$report_notes,
          i18n_strings          = i18n_strings,          # NEW (Phase 4)
          report_language       = global_data$language,  # NEW (for date/number formatting if needed)
          interactive           = isTRUE(input$report_interactive) && input$report_format != "pdf",
          # Mode exploratoire sans réplicat (dispersion imposée) : doit figurer
          # dans le rapport, sinon des p-values non inférentielles y seraient
          # présentées comme une DE ordinaire.
          no_rep_bypass         = shared_rv$de_bypass
        )

        withProgress(message = .tr("G\u00e9n\u00e9ration du rapport..."), value = 0.2, {
          formats_needed <- switch(input$report_format,
            html = "html_document", pdf = "pdf_document",
            both = c("html_document", "pdf_document"))
          out_files <- character(0)
          for (fmt in formats_needed) {
            incProgress(0.3, detail = paste("Rendu", fmt))
            # Step-3.5 FIX: tempfile() avec fileext explicite -- l'ancienne
            # version encodait les secondes fractionnaires (%OS3) DANS le nom
            # de base, qui contient donc un "." (ex: "..._143022.456").
            # rmarkdown interprete CE point comme l'extension du fichier et
            # n'ajoute PAS le vrai ".html"/".pdf" -- inoffensif pour un export
            # simple (Shiny renomme via downloadHandler$filename), mais casse
            # le mode "Les deux (.zip)" : les 2 fichiers dans l'archive
            # n'avaient plus d'extension reconnaissable et n'etaient plus
            # ouvrables directement une fois decompresses.
            ext_i    <- if (fmt == "html_document") "html" else "pdf"
            out_path <- tempfile(pattern = paste0("bulk_report_", ext_i, "_"), fileext = paste0(".", ext_i))
            res <- tryCatch(
              rmarkdown::render(input = tmp_rmd, output_format = fmt, output_file = out_path,
                                params = render_params, envir = new.env(parent = globalenv()), quiet = TRUE),
              error = function(e) {
                showNotification(paste0("\u274c ", fmt, ": ", conditionMessage(e)), type = "error", duration = 12)
                NULL
              })
            if (!is.null(res)) out_files <- c(out_files, res)
          }
          if (length(out_files) == 0) stop(errorCondition(.tr("Aucun format g\u00e9n\u00e9r\u00e9."), class = "bulk_report_error"))
          else if (length(out_files) == 1) file.copy(out_files[1], file, overwrite = TRUE)
          else zip::zip(file, files = out_files, mode = "cherry-pick")
        })
      }
    )

    # ── Script R reproductible ─────────────────────────────────────────────
    output$dl_r_script <- downloadHandler(
      # Step-3.5: horodatage à la seconde — même logique que dl_report.
      filename = function() paste0("analyse_bulk_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"),
      content  = function(file) {
        req(global_data$bulk_obj)
        ac    <- shared_rv$active_contrast %||% ""
        parts <- strsplit(ac, "_vs_")[[1]]
        target <- if (length(parts) >= 2) paste(parts[-length(parts)], collapse = "_") else "GroupA"
        ref    <- if (length(parts) >= 2) parts[length(parts)] else "GroupB"

        script_text <- bulk_r_script_text(
          n_genes       = if (!is.null(shared_rv$filtered_counts)) nrow(shared_rv$filtered_counts) else "?",
          n_samp        = if (!is.null(shared_rv$filtered_counts)) ncol(shared_rv$filtered_counts) else "?",
          lfc           = shared_rv$lfc_thresh  %||% 1,
          padj          = shared_rv$padj_thresh %||% 0.05,
          contrast_name = ac,
          condition_col = shared_rv$active_condition_col %||% "condition",
          group_target  = target,
          group_ref     = ref,
          palette_colors = shared_rv$volcano_role_colors,
          all_contrast_names = names(shared_rv$contrasts),
          pathway_mode  = shared_rv$pathway_mode %||% "ora",
          # STAT-Q1 : la méthode de correction choisie (Step 2) est reprise
          # dans le script généré, pour que le .zip soit réellement
          # reproductible — pas seulement « reproductible en BH ».
          padj_method   = shared_rv$padj_method %||% "BH",
          # STAT-Q1 : le panneau Pathways a son propre sélecteur ; on retombe
          # sur la méthode DE s'il n'a pas encore été initialisé.
          pathway_padj_method = shared_rv$pathway_padj_method %||% shared_rv$padj_method %||% "BH"
        )

        # ── Bundle companion data (Step-3.5 fix) ──────────────────────────
        # The script reads `counts_raw.rds` / `metadata.rds` from its own
        # working directory (see section 0 of the generated script) — these
        # were never actually produced before, making the script impossible
        # to run standalone. RAW (unfiltered, pre-VST) counts are used, since
        # the script re-does its own filtering — same matrix mod_bulk_filter
        # would start from (mapped IDs if Step-0 mapping was applied).
        tmp_dir <- tempfile("bulk_script_"); dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

        # Step-3.5: le nom du .R interne au zip est lui aussi horodaté à la
        # seconde — cohérent avec le nom du .zip, et évite toute confusion
        # si l'utilisateur extrait plusieurs zips dans le même dossier.
        script_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        script_path <- file.path(tmp_dir, paste0("analyse_bulk_", script_stamp, ".R"))
        writeLines(script_text, script_path)

        raw_counts <- shared_rv$counts_mapped %||% global_data$bulk_obj$counts
        saveRDS(raw_counts, file.path(tmp_dir, "counts_raw.rds"))
        saveRDS(global_data$bulk_obj$metadata, file.path(tmp_dir, "metadata.rds"))

        zip::zip(file, files = c(script_path,
                                 file.path(tmp_dir, "counts_raw.rds"),
                                 file.path(tmp_dir, "metadata.rds")),
                 mode = "cherry-pick")
      }
    )

  }) # /moduleServer
}

# ── Helper: generate reproducible R script (Step-3.0: MA, Heatmap, Pathway;
#    Step-3.0b: pairwise section when the app computed >1 contrast) ─────────
#' @param n_genes,n_samp Dataset dimensions (informational, in header comment).
#' @param lfc,padj Thresholds mirrored from shared_rv at click time.
#' @param condition_col The actual metadata column name used in the DE design.
#' @param palette_colors Named vector c(Up=, Down=, NS=) for volcano colours.
#' @param all_contrast_names Names of every contrast in shared_rv$contrasts —
#'   when length > 1 (pairwise-auto or repeated manual runs), an extra
#'   "3bis. Pairwise" section is generated computing/exporting every OTHER
#'   pair by reusing the same fitted `dds` (results()/lfcShrink only, no
#'   refit) — keeps the exported script a faithful, complete reproduction of
#'   the session rather than just the single active contrast.
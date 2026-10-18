# =============================================================================
# mod_bulk_de_viz.R — Bulk Child 2: Volcano / MA-Plot / Heatmap / Table DE
# (Step-3.6 refactor — extracted from the monolithic mod_bulk_de.R)
# =============================================================================
# Owns the 4 tabs that visualise the CURRENTLY ACTIVE contrast:
#   - active_de_results(): reactive accessor for shared_rv$contrasts[[active]]
#   - sync_warning: desync banner shown on BOTH Volcano and Heatmap tabs,
#     detecting when the active contrast's gene set no longer matches the
#     CURRENT filtered_counts (e.g. Step 1 was re-run after this DE pass)
#   - Volcano (static ggplot + optional interactive plotly)
#   - MA-Plot (static ggplot + optional interactive plotly)
#   - Heatmap (ComplexHeatmap, dedicated PNG/PDF export — not ggsave-able)
#   - Table DE (DT + CSV/Excel export)
#
# Also mirrors shared_rv$volcano_role_colors (Step-3.0), read by
# mod_bulk_report.R / the exported R script, since it owns volcano_role_colors().
#
# Depends on helpers_bulk.R: plot_volcano_bulk(), plot_ma_bulk(),
#   plot_heatmap_bulk(), build_de_results_dt(), bulk_role_colors(),
#   manual_color_picker_ui(), .default_manual_colors().
# =============================================================================

#' Texte de survol du volcano (STAT-Q2)
#'
#' Construit la colonne `hover` consommée par plotly. Isolé du serveur pour
#' deux raisons : (1) la logique de dégradation — `lfcSE` n'existe que pour
#' DESeq2 — mérite d'être testée explicitement, hors session Shiny ; (2) une
#' référence à une colonne absente DANS la formule plotly ferait échouer le
#' rendu, alors qu'ici le texte est construit en R avant l'appel.
#'
#' @param res data.frame de résultats DE (colonnes gene, log2FoldChange, padj,
#'   status ; `lfcSE` optionnel).
#' @return `res` avec une colonne `hover` (character) ajoutée.
.de_volcano_hover <- function(res) {
  se_txt <- if ("lfcSE" %in% names(res)) {
    se_val <- round(res$lfcSE, 3)
    ifelse(is.na(se_val), "", paste0("<br>SE: ", se_val))
  } else rep("", nrow(res))
  res$hover <- paste0("<b>", res$gene, "</b><br>Log2FC: ", round(res$log2FoldChange, 3),
                      se_txt,
                      "<br>-log10(padj): ", round(-log10(res$padj + 1e-300), 2),
                      "<br>Statut: ", res$status)
  res
}

.de_viz_server <- function(input, output, session, ns, global_data, shared_rv) {

  .tr <- function(key) {
    tr <- global_data$i18n
    if (is.null(tr)) return(key)
    tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
  }


  # Reactive accessor for the currently displayed DE result (local to this
  # function — sibling functions read shared_rv$contrasts[[shared_rv$active_contrast]]
  # directly since they don't need req()-based reactive semantics here)
  #
  # STAT-Q1 — recalcul « live » de padj. Le résultat stocké a été calculé avec
  # une méthode de correction donnée ; si l'utilisateur en choisit une autre
  # dans le panneau Step 2, on rappelle simplement DESeq2::results() sur le dds
  # DÉJÀ ajusté (contexte mémorisé par helpers$remember_padj_ctx()) : aucun
  # DESeq() relancé, donc c'est immédiat. Seuls les contrastes DESeq2 possèdent
  # un tel contexte — pour edgeR/limma la méthode est appliquée au moment du run
  # (pas de modèle en cache à ré-interroger), on renvoie donc le résultat tel quel.
  active_de_results <- reactive({
    ac <- shared_rv$active_contrast
    req(ac, shared_rv$contrasts[[ac]])
    base_res <- shared_rv$contrasts[[ac]]

    # M13 : le moteur estampillé sur le résultat stocké. Le recalcul ci-dessous
    # RECONSTRUIT le data.frame (extract_deseq2_contrast) — et un data.frame
    # neuf ne transporte pas les attributs personnalisés : sans re-stampille,
    # le sous-titre deviendrait muet précisément quand l'utilisateur change la
    # méthode de correction.
    base_engine <- bulk_de_engine_of(base_res)

    method <- input$padj_method %||% TS_PADJ_METHOD_DEFAULT
    ctx    <- (shared_rv$de_padj_ctx %||% list())[[ac]]
    if (is.null(ctx) || identical(ctx$method, method)) return(base_res)

    tryCatch(
      bulk_de_stamp_engine(
        extract_deseq2_contrast(ctx$dds, ctx$condition_col, ctx$group_target, ctx$group_ref,
                                shrink = ctx$shrink, p_adjust_method = method),
        base_engine),
      # Repli silencieux : un échec de recalcul ne doit JAMAIS effacer un
      # résultat déjà affiché (l'utilisateur garde la version précédente).
      error = function(e) base_res
    )
  })

  # =========================================================================
  # SYNC WARNING — shared by Volcano + Heatmap tabs. Detects when the active
  # contrast's genes no longer match the CURRENT filtered_counts (Step 1 was
  # re-run after this DE pass). Defense-in-depth: mod_bulk_filter already
  # wipes shared_rv$contrasts on re-filter, but this catches any remaining
  # edge case (e.g. that safety being bypassed by a future code change)
  # instead of failing silently downstream.
  # =========================================================================
  sync_warning <- reactive({
    global_data$language  # i18n: re-evaluate the banner on language switch
    res <- tryCatch(active_de_results(), error = function(e) NULL)
    if (is.null(res) || is.null(shared_rv$filtered_counts)) return(NULL)
    total   <- nrow(res)
    present <- sum(res$gene %in% rownames(shared_rv$filtered_counts))
    pct_missing <- if (total > 0) 100 * (total - present) / total else 0
    if (pct_missing > 1) {
      .t_fmt(.tr("\u26a0\ufe0f {pct}% des g\u00e8nes du contraste '{c}' ne sont plus pr\u00e9sents dans les donn\u00e9es filtr\u00e9es actuelles. Le filtrage (\u00c9tape 1) a probablement \u00e9t\u00e9 relanc\u00e9 apr\u00e8s ce calcul \u2014 relancez l'\u00c9tape 2 pour resynchroniser."),
             pct = sprintf("%.0f", pct_missing), c = shared_rv$active_contrast)
    } else NULL
  })

  .sync_banner <- function() {
    msg <- sync_warning()
    if (is.null(msg)) return(NULL)
    div(class = "alert alert-danger", style = "font-size:0.82em;", icon("triangle-exclamation"), " ", msg)
  }
  output$sync_warning_banner         <- renderUI({ .sync_banner() })
  output$sync_warning_banner_heatmap <- renderUI({ .sync_banner() })

  # =========================================================================
  # VOLCANO — static (export) + optional interactive (plotly, native tooltip)
  # =========================================================================
  # Up/Down/NS are FIXED semantic roles (not an arbitrary N-level grouping),
  # so this reuses bulk_role_colors() rather than the per-level picker used
  # for PCA/Heatmap/QC — same "Manuel" MODE (shared_rv$bulk_palette, set in
  # the Step 1 sidebar), but only 3 swatches regardless of dataset.
  output$volcano_manual_palette_ui <- renderUI({
    global_data$language  # i18n
    if (!identical(shared_rv$bulk_palette, "manual")) return(NULL)
    div(
      class = "border rounded p-2 mb-2", style = "background:#f8f9fa;",
      h6(.tr("Couleurs manuelles \u2014 Up / Down / Non-significatif"),
         style = "font-size:0.85em;font-weight:bold;margin-bottom:6px;"),
      manual_color_picker_ui(ns, c("role_color_up", "role_color_down", "role_color_ns"),
                             c("Up-régulé", "Down-régulé", "Non-significatif"),
                             c("#E74C3C", "#2980B9", "#BDC3C7"))
    )
  })

  volcano_role_colors <- reactive({
    pal <- shared_rv$bulk_palette %||% "default"
    manual_vec <- if (identical(pal, "manual")) {
      c(Up = input$role_color_up %||% "#E74C3C",
        Down = input$role_color_down %||% "#2980B9",
        NS = input$role_color_ns %||% "#BDC3C7")
    } else NULL
    bulk_role_colors(pal, manual_vec)
  })

  # Step-3.0: mirror resolved role colors → used by report Rmd and R script
  observe({
    shared_rv$volcano_role_colors <- tryCatch(volcano_role_colors(), error = function(e) NULL)
  })

  # Human-readable DE engine label for the statistical subtitles (PLOT-Q3).
  # M13 (2026-10-17) : lit le moteur ESTAMPILLÉ SUR LE RÉSULTAT affiché, jamais
  # `input$de_engine` — celui-ci n'est que la sélection courante du panneau, et
  # elle ne décrit le contraste que par coïncidence : le panneau pairwise
  # dispatche sur `input$ap_engine`, et le mode exploratoire sans réplicat
  # impose "edger". Lire la sélection revenait à réécrire le passé dès qu'on
  # changeait le select après le run. Moteur inconnu (contraste restauré d'une
  # session antérieure) ⇒ "" ⇒ le sous-titre OMET le moteur au lieu de deviner.
  .engine_label <- function() {
    bulk_de_active_engine_label(shared_rv$contrasts, shared_rv$active_contrast)
  }

  volcano_plot <- reactive({
    global_data$language                     # i18n trigger
    req(active_de_results())
    rc <- volcano_role_colors()
    plot_volcano_bulk(active_de_results(), lfc_thresh = input$lfc_thresh, padj_thresh = input$padj_thresh,
                      up_color = rc[["Up"]], down_color = rc[["Down"]], ns_color = rc[["NS"]],
                      engine = .engine_label(),
                      theme_choice = input$plot_theme %||% TS_THEME_DEFAULT,
                      base_size    = input$base_size %||% TS_BASE_SIZE_DEFAULT,
                      tr = .tr_fn(global_data))
  })
  output$plot_volcano <- renderPlot({ volcano_plot() })

  output$volcano_container <- renderUI({
    if (isTRUE(input$volcano_interactive)) plotlyOutput(ns("plot_volcano_ly"), height = "650px")
    else plotOutput(ns("plot_volcano"), height = "650px")
  })

  output$plot_volcano_ly <- renderPlotly({
    req(active_de_results())
    res <- active_de_results()
    res <- res[!is.na(res$padj), ]
    lfc <- input$lfc_thresh; pval <- input$padj_thresh
    res$status <- dplyr::case_when(
      res$padj < pval & res$log2FoldChange >  lfc ~ "Up",
      res$padj < pval & res$log2FoldChange < -lfc ~ "Down",
      TRUE ~ "NS"
    )
    color_map <- volcano_role_colors()

    # STAT-Q2 : texte de survol (ajoute SE: <lfcSE> quand DESeq2 l'a fourni —
    # jamais pour edgeR/limma). Voir .de_volcano_hover() en tête de fichier.
    res <- .de_volcano_hover(res)

    plot_ly(
      data = res, x = ~log2FoldChange, y = ~-log10(padj + 1e-300),
      type = "scatter", mode = "markers",
      marker = list(color = ~color_map[status], size = 6, opacity = 0.75, line = list(width = 0)),
      hovertext = ~hover,
      hoverinfo = "text"
    ) |>
      layout(
        title  = paste("Volcano —", shared_rv$active_contrast %||% ""),
        xaxis  = list(title = "Log2 Fold Change", zeroline = TRUE),
        yaxis  = list(title = "-log10(P-adj)"),
        shapes = list(
          list(type = "line", x0 = lfc, x1 = lfc, y0 = 0, y1 = 1, yref = "paper",
              line = list(dash = "dot", color = unname(color_map[["Up"]]), width = 1)),
          list(type = "line", x0 = -lfc, x1 = -lfc, y0 = 0, y1 = 1, yref = "paper",
              line = list(dash = "dot", color = unname(color_map[["Down"]]), width = 1)),
          list(type = "line", x0 = min(res$log2FoldChange, na.rm = TRUE),
              x1 = max(res$log2FoldChange, na.rm = TRUE),
              y0 = -log10(pval), y1 = -log10(pval),
              line = list(dash = "dot", color = "#7F8C8D", width = 1))
        ),
        showlegend = FALSE
      )
  })

  output$dl_volcano_png <- downloadHandler(
    # PLOT-S5 — `volcano_export_fmt` etait un controle MORT : declare dans l'UI
    # mais jamais lu, et l'extension `.png` codee en dur ici faisait qu'un choix
    # "PDF" produisait quand meme un PNG. Le format pilote desormais le fichier
    # ET le device, comme MA-Plot juste en dessous.
    filename = function() paste0("volcano_", shared_rv$active_contrast, "_", Sys.Date(),
                                 ".", input$volcano_export_fmt %||% "png"),
    content  = function(file) ts_export_plot(file, volcano_plot(), width = 8, height = 6,
                                             dpi = 300,
                                             format = input$volcano_export_fmt %||% "png")
  )

  # =========================================================================
  # MA-PLOT — static (export) + optional interactive (plotly, native tooltip)
  # =========================================================================
  # Reuses the SAME role-color picker as Volcano (Up = "significant" here,
  # NS unchanged) — one fewer control to keep in sync, see helpText on the
  # MA-Plot tab pointing back to Volcano.
  ma_plot <- reactive({
    global_data$language                     # i18n trigger
    req(active_de_results())
    rc <- volcano_role_colors()
    plot_ma_bulk(active_de_results(), lfc_thresh = input$lfc_thresh, padj_thresh = input$padj_thresh,
                sig_color = rc[["Up"]], ns_color = rc[["NS"]],
                engine = .engine_label(),
                theme_choice = input$plot_theme %||% TS_THEME_DEFAULT,
                base_size    = input$base_size %||% TS_BASE_SIZE_DEFAULT,
                tr = .tr_fn(global_data))
  })
  output$plot_ma <- renderPlot({ ma_plot() })

  output$ma_container <- renderUI({
    if (isTRUE(input$ma_interactive)) plotlyOutput(ns("plot_ma_ly"), height = "650px")
    else plotOutput(ns("plot_ma"), height = "650px")
  })

  output$plot_ma_ly <- renderPlotly({
    req(active_de_results())
    res <- active_de_results()
    res <- res[!is.na(res$padj) & !is.na(res$baseMean), ]
    lfc <- input$lfc_thresh; pval <- input$padj_thresh
    res$sig <- res$padj < pval & abs(res$log2FoldChange) > lfc

    plot_ly(
      data = res, x = ~log10(baseMean + 1), y = ~log2FoldChange,
      type = "scatter", mode = "markers",
      marker = list(color = ~ifelse(sig, "#E74C3C", "#BDC3C7"), size = 6, opacity = 0.7, line = list(width = 0)),
      hovertext = ~paste0("<b>", gene, "</b><br>BaseMean: ", round(baseMean, 1),
                          "<br>Log2FC: ", round(log2FoldChange, 3),
                          "<br>padj: ", format(padj, scientific = TRUE, digits = 2)),
      hoverinfo = "text"
    ) |>
      layout(
        title = paste("MA-Plot —", shared_rv$active_contrast %||% ""),
        xaxis = list(title = "Log10(Expression Moyenne + 1)"),
        yaxis = list(title = "Log2 Fold Change"),
        shapes = list(list(type = "line", x0 = 0, x1 = max(log10(res$baseMean + 1)), y0 = 0, y1 = 0,
                           line = list(color = "grey30", width = 1))),
        showlegend = FALSE
      )
  })

  output$dl_ma_png <- downloadHandler(
    filename = function() paste0("ma_plot_", shared_rv$active_contrast, "_", Sys.Date(),
                                 ".", input$ma_export_fmt %||% "png"),
    # PLOT-S5 — le format pilote directement `device` : la branche if/else
    # historique (pdf vs reste) ne connaissait que png/pdf, elle ignorait "svg".
    # `dpi` est sans effet sur un device vectoriel (ggsave l'accepte et
    # l'ignore) : le passer inconditionnellement ne change rien au PDF.
    content  = function(file) ts_export_plot(file, ma_plot(), width = 8, height = 6,
                                             dpi = 300,
                                             format = input$ma_export_fmt %||% "png")
  )

  # =========================================================================
  # HEATMAP — defensive rewrite
  # =========================================================================
  # Hardened against the "objet 'res' introuvable" class of crash during the
  # pairwise auto-pipeline's tab flip: active_de_results() is resolved inside
  # tryCatch() so an absent/mismatched contrast short-circuits via req(NULL)
  # instead of propagating a raw error, every threshold input is null-safe,
  # and no bare symbol is ever referenced outside its local scope.
  heatmap_genes <- reactive({
    req(shared_rv$vst_mat)
    res <- tryCatch(active_de_results(), error = function(e) NULL)
    req(res)                                   # short-circuit if no DE result yet
    if (!"padj" %in% colnames(res)) return(character(0))
    res <- res[!is.na(res$padj), , drop = FALSE]
    req(nrow(res) > 0)

    # ── Directional subset (BingleSeq-style: Tous/Sig/Up/Down/Non-sig) ────
    # Applied BEFORE ranking by p-adj, on the CURRENT lfc/padj thresholds —
    # consistent with Volcano/MA-plot which already read the same inputs.
    dir_choice <- input$heatmap_direction %||% "all"
    lfc  <- input$lfc_thresh  %||% 1
    pval <- input$padj_thresh %||% 0.05
    is_sig <- !is.na(res$padj) & res$padj < pval & abs(res$log2FoldChange) > lfc
    res <- switch(dir_choice,
      sig  = res[is_sig, , drop = FALSE],
      up   = res[is_sig & res$log2FoldChange > 0, , drop = FALSE],
      down = res[is_sig & res$log2FoldChange < 0, , drop = FALSE],
      ns   = res[!is_sig, , drop = FALSE],
      res  # "all" — unchanged, original behaviour
    )
    validate(need(nrow(res) > 0,
                  .tr("Aucun g\u00e8ne dans ce sous-ensemble (Up/Down/Sig/Non-sig) avec les seuils actuels.")))

    ranked_genes <- res$gene[order(res$padj)]
    valid_genes  <- intersect(ranked_genes, rownames(shared_rv$vst_mat))
    genes        <- head(valid_genes, input$heatmap_top_n %||% 30)
    validate(need(
      length(genes) >= 2,
      .t_fmt(.tr("Pas assez de g\u00e8nes communs entre le contraste actif et la matrice VST actuelle ({n} trouv\u00e9s). Relancez l'\u00e9tape 2 (DE) apr\u00e8s tout changement de filtrage."),
             n = length(valid_genes))
    ))
    genes
  })

  # ── Manual palette: own picker, keyed to heatmap_annot's levels — kept
  #    SEPARATE from PCA/QC pickers since heatmap_annot may point to yet
  #    another metadata column. Same shared_rv$bulk_palette MODE (Step 1
  #    sidebar) decides whether "Manuel" is active app-wide.
  manual_heatmap_levels <- reactive({
    req(global_data$bulk_obj$metadata, input$heatmap_annot)
    req(nzchar(input$heatmap_annot))
    lvls <- sort(unique(stats::na.omit(as.character(global_data$bulk_obj$metadata[[input$heatmap_annot]]))))
    req(length(lvls) > 0)
    lvls
  })

  output$heatmap_manual_palette_ui <- renderUI({
    global_data$language  # i18n
    if (!identical(shared_rv$bulk_palette, "manual")) return(NULL)
    if (!nzchar(input$heatmap_annot %||% "")) {
      return(div(class = "alert alert-warning", style = "font-size:0.8em;",
                 .tr("S\u00e9lectionnez d'abord une \"Annotation colonnes\" pour personnaliser ses couleurs.")))
    }
    lvls <- tryCatch(manual_heatmap_levels(), error = function(e) character(0))
    if (length(lvls) == 0) return(NULL)
    ids <- paste0("heatmap_manual_color_", seq_along(lvls))
    div(
      class = "border rounded p-2 mb-2", style = "background:#f8f9fa;",
      h6(.t_fmt(.tr("Couleurs manuelles \u2014 {var}"), var = input$heatmap_annot),
         style = "font-size:0.85em;font-weight:bold;margin-bottom:6px;"),
      manual_color_picker_ui(ns, ids, lvls, .default_manual_colors(length(lvls)))
    )
  })

  heatmap_manual_colors <- reactive({
    if (!identical(shared_rv$bulk_palette, "manual")) return(NULL)
    lvls <- tryCatch(manual_heatmap_levels(), error = function(e) character(0))
    if (length(lvls) == 0) return(NULL)
    defaults <- .default_manual_colors(length(lvls))
    vals <- vapply(seq_along(lvls), function(i) {
      v <- input[[paste0("heatmap_manual_color_", i)]]
      if (is.null(v) || !nzchar(v)) defaults[i] else v
    }, character(1))
    setNames(vals, lvls)
  })

  # Statistical subtitle for the heatmap (PLOT-Q3) — same Up/Down/tested +
  # thresholds contract as Volcano/MA, computed from the SAME active contrast
  # and the SAME live input thresholds, so all three panels of a contrast
  # always agree.
  .heatmap_stat_subtitle <- function() {
    res <- tryCatch(active_de_results(), error = function(e) NULL)
    if (is.null(res) || !all(c("padj", "log2FoldChange") %in% names(res))) return(NULL)
    lfc <- input$lfc_thresh %||% 1
    pv  <- input$padj_thresh %||% 0.05
    ok  <- !is.na(res$padj)
    n_up   <- sum(ok & res$padj < pv & res$log2FoldChange >  lfc)
    n_down <- sum(ok & res$padj < pv & res$log2FoldChange < -lfc)
    # M13 : le moteur est OMIS quand il est inconnu — sinon on obtiendrait un
    # séparateur orphelin (« … —  (padj<… ) »), la même classe de piège que le
    # préfixe orphelin de `paste0("x", character(0))`.
    eng <- .engine_label()
    tail <- if (nzchar(eng)) sprintf(" — %s (padj<%.2g, |log2FC|>%.2g)", eng, pv, lfc)
            else sprintf(" (padj<%.2g, |log2FC|>%.2g)", pv, lfc)
    sprintf("%d Up · %d Down · %d testés%s", n_up, n_down, sum(ok), tail)
  }

  .heatmap_obj <- function() {
    global_data$language                     # i18n trigger
    annot <- if (nzchar(input$heatmap_annot %||% "")) input$heatmap_annot else NULL
    pal   <- shared_rv$bulk_palette %||% "default"
    # PLOT-S4 — reglages de clustering. k < 2 (ou vide) = pas de decoupage,
    # ce qui reproduit exactement le rendu anterieur au jalon.
    k_row <- suppressWarnings(as.integer(input$heatmap_k_row %||% 0))
    if (length(k_row) != 1L || is.na(k_row) || k_row < 2L) k_row <- NULL
    plot_heatmap_bulk(shared_rv$vst_mat, heatmap_genes(), global_data$bulk_obj$metadata,
                      annotation_col = annot, palette = pal,
                      manual_colors = if (identical(pal, "manual")) heatmap_manual_colors() else NULL,
                      subtitle = .heatmap_stat_subtitle(),
                      theme_choice = input$plot_theme %||% TS_THEME_DEFAULT,
                      base_size    = input$base_size %||% TS_BASE_SIZE_DEFAULT,
                      clustering_distance = input$heatmap_clust_distance %||% "euclidean",
                      clustering_method   = input$heatmap_clust_method %||% "complete",
                      k_row               = k_row,
                      tr = .tr_fn(global_data))
  }

  output$plot_heatmap <- renderPlot({
    .safe_plot_render(session, "plot_heatmap", function() .heatmap_obj())
  })

  output$dl_heatmap <- downloadHandler(
    filename = function() paste0("heatmap_", shared_rv$active_contrast, "_", Sys.Date(), ".",
                                 input$heatmap_export_fmt %||% "png"),
    # PLOT-S5 — ComplexHeatmap n'est pas un ggplot : ts_export_plot() (ggsave)
    # ne s'applique pas ici, on ouvre donc le device nous-memes. Branche `svg`
    # ajoutee (svglite), meme taille physique 9x8 in que pdf/png. Les branches
    # png/pdf sont inchangees.
    content  = function(file) {
      fmt <- input$heatmap_export_fmt %||% "png"
      if (identical(fmt, "svg")) {
        svglite::svglite(file, width = 9, height = 8)
      } else if (identical(fmt, "pdf")) {
        pdf(file, width = 9, height = 8)
      } else {
        png(file, width = 9, height = 8, units = "in", res = 300)
      }
      .heatmap_obj()  # draws as a side effect on the device just opened above
      dev.off()
    }
  )

  # ── DE Table ──────────────────────────────────────────────────────────────
  output$table_de <- renderDT({
    req(active_de_results())
    build_de_results_dt(active_de_results())
  })

  output$dl_de_csv <- downloadHandler(
    filename = function() paste0("DE_", shared_rv$active_contrast, "_", Sys.Date(), ".csv"),
    content  = function(file) { req(active_de_results()); write.csv(active_de_results(), file, row.names = FALSE) }
  )
  output$dl_de_excel <- downloadHandler(
    # STAT-Q4 : l'extension suit le format REELLEMENT ecrit par le helper
    # partage (openxlsx si dispo, sinon CSV) — avant, un CSV pouvait se
    # retrouver nomme .xlsx quand openxlsx manquait.
    filename = function() paste0("DE_", shared_rv$active_contrast, "_", Sys.Date(),
                                 if (requireNamespace("openxlsx", quietly = TRUE)) ".xlsx" else ".csv"),
    content  = function(file) {
      req(active_de_results())
      write_table_excel_or_csv(active_de_results(), file)
    }
  )
}

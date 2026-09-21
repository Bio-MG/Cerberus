# =============================================================================
# mod_bulk_de_run.R — Bulk Child 2: single-pair DE + ad-hoc contrast
# (Step-3.6 refactor — extracted from the monolithic mod_bulk_de.R)
# =============================================================================
# Owns:
#   - Step 2 "Lancer l'Analyse Différentielle" (single Cible vs Référence pair)
#   - Contraste Ad-hoc (BingleSeq pattern): manual Group A/B sample selection,
#     bypasses condition_col entirely with a synthetic 2-level metadata.
#
# Both hard-block confounded/single-level covariates BEFORE fitting (see
# helpers_bulk.R: check_design_confounding(), validate_bulk_design()) —
# DESeq2 would otherwise fail deep inside DESeq() with a cryptic linear-
# algebra error, or silently ignore a single-level covariate without warning.
#
# Depends on helpers_bulk.R: check_design_confounding(), build_dds(),
#   run_bulk_de_dispatch(), .normalize_de_cols().
# Depends on: helpers$design_str(), helpers$register_contrast() —
#   see mod_bulk_de_engine.R::.de_make_helpers().
# =============================================================================

.de_run_server <- function(input, output, session, ns, global_data, shared_rv, helpers) {

  # ── DRIVE LIVE CONTROL (docs/DRIVE_LIVE_CONTROL_PLAN.md, grade G2) ───────
  # The DE run button is fired by the file-drop poller through a counter it
  # publishes in the session-scoped registry (see ts_drive_publish_token()).
  # `updateActionButton()` does NOT click; this second trigger does.
  drive_counter <- shiny::reactiveVal(0L)
  ts_drive_publish_token(global_data, "bulk-de-run_de", drive_counter)
  drive_trigger <- shiny::reactive(list(drive_counter(), input$run_de))

  .tr <- function(key) {
    tr <- global_data$i18n
    if (is.null(tr)) return(key)
    tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
  }


  # ── Polish UI: disable the DE button until Step 1 has actually run ──────
  observe({
    shinyjs::toggleState("run_de", condition = !is.null(shared_rv$filtered_counts))
  })

  # ── Plan sans réplicat (n = p) ─────────────────────────────────────────────
  # Avec autant d'échantillons que de coefficients, il ne reste aucun degré de
  # liberté pour estimer une dispersion. Mesuré le 2026-09-17 (renv projet) :
  #   DESeq2  refuse dans estimateDispersionsGeneEst() ;
  #   edgeR   ne refuse pas — estimateDisp() pose une dispersion NA et
  #           glmQLFit() échoue ensuite ("NA dispersions not allowed") ;
  #   limma   passe voom() puis eBayes() échoue ("No residual degrees of
  #           freedom in linear model fits").
  # Le seul contournement tenable est une dispersion IMPOSÉE (edgeR), dont
  # l'utilisateur doit assumer le choix : c'est l'objet de l'attestation.
  output$no_rep_bypass_ui <- renderUI({
    req(input$condition_col)
    meta <- global_data$bulk_obj$metadata
    if (is.null(meta) || !input$condition_col %in% colnames(meta)) return(NULL)
    sat <- design_saturation(meta, input$condition_col, input$covariates %||% character(0))
    if (!sat$saturated) return(NULL)

    on <- isTRUE(input$no_rep_enable)
    tagList(
      div(class = "alert alert-danger", style = "font-size:0.76em;padding:6px 10px;",
          tags$strong(.tr("Plan sans r\u00e9plicat d\u00e9tect\u00e9")),
          tags$div(.t_fmt(.tr("{n} \u00e9chantillon(s) pour {p} coefficient(s) : aucun degr\u00e9 de libert\u00e9 r\u00e9siduel. DESeq2 refusera, edgeR rendra une dispersion NA, limma \u00e9chouera dans eBayes()."),
                          n = sat$n, p = sat$p))),
      checkboxInput(ns("no_rep_enable"),
                    .tr("Mode exploratoire sans r\u00e9plicat (edgeR, dispersion impos\u00e9e)"),
                    value = on),
      if (on) tagList(
        numericInput(ns("no_rep_bcv"), .tr("BCV impos\u00e9 (dispersion = BCV\u00b2)"),
                     value = 0.4, min = 0.01, max = 2, step = 0.05),
        helpText(style = "font-size:0.72em;",
                 .tr("Le nombre de g\u00e8nes significatifs D\u00c9PEND ENTI\u00c8REMENT de ce r\u00e9glage : mesur\u00e9 sur 4 \u00e9chantillons \u00d7 4 conditions, 1197 g\u00e8nes \u00e0 FDR<0.05 pour BCV 0.1, contre 2 pour BCV 0.4 et 0 pour BCV 0.8 \u2014 m\u00eames donn\u00e9es.")),
        checkboxInput(ns("no_rep_attest"),
                      .tr("J'atteste comprendre que ces p-values reposent sur une dispersion impos\u00e9e et ne sont PAS inf\u00e9rentielles."),
                      value = isTRUE(input$no_rep_attest))
      )
    )
  })

  # =========================================================================
  # STEP 2 — Differential Expression (single pair)
  # =========================================================================
  observeEvent(drive_trigger(), {
    req(input$run_de > 0 || shiny::isolate(drive_counter()) > 0)
    req(shared_rv$filtered_counts, input$condition_col, input$group_ref, input$group_target,
        input$de_engine)

    if (input$group_ref == input$group_target) {
      showNotification(.tr("⚠️ Le groupe Référence et le groupe Cible doivent être différents."),
                       type = "warning"); return()
    }

    meta <- global_data$bulk_obj$metadata
    grp_n <- table(meta[[input$condition_col]])
    if (any(grp_n[c(input$group_ref, input$group_target)] < 2)) {
      showNotification(.tr("⚠️ Au moins un groupe a < 2 réplicats — résultats peu fiables."),
                       type = "warning", duration = 6)
    }

    # HARD BLOCK: confounded covariate would make DESeq2's design matrix
    # lose full rank, producing a cryptic linear-algebra error deep inside
    # DESeq(). Catch it here with an actionable message instead.
    covariates_in_use <- input$covariates %||% character(0)
    confounded <- Filter(
      function(cov) check_design_confounding(meta, input$condition_col, cov),
      covariates_in_use
    )
    if (length(confounded) > 0) {
      showNotification(
        .t_fmt(.tr("\u274c Covariable(s) confondue(s) avec '{col}' : {covs}. Retirez-la(les) du design ou revoyez votre plan d'exp\u00e9rience."),
               col = input$condition_col, covs = paste(confounded, collapse = ", ")),
        type = "error", duration = 10
      )
      return()
    }

    # HARD BLOCK: a single-level covariate contributes nothing to the
    # model — R's contrast coding produces zero columns for it, so DESeq2
    # would silently fit ~ condition_col alone while the user believes
    # they are also correcting for this covariate. No crash, no warning
    # from DESeq2 itself — catch it explicitly instead of letting the
    # analysis "succeed" on the wrong design.
    single_level <- Filter(
      function(cov) length(unique(na.omit(meta[[cov]]))) < 2,
      covariates_in_use
    )
    if (length(single_level) > 0) {
      showNotification(
        .t_fmt(.tr("\u274c Covariable(s) \u00e0 une seule modalit\u00e9 : {covs}. Elle(s) n'apporterai(en)t aucune information \u2014 retirez-la(les) du design."),
               covs = paste(single_level, collapse = ", ")),
        type = "error", duration = 10
      )
      return()
    }

    # HARD BLOCK : plan saturé (n = p). Sans réplicat il n'existe AUCUN degré de
    # liberté pour estimer une dispersion — les trois moteurs échouent. Le
    # passage n'est autorisé qu'en mode exploratoire EXPLICITEMENT attesté :
    # l'utilisateur fournit alors lui-même la dispersion (BCV) et les p-values
    # qui en découlent ne sont pas inférentielles.
    sat    <- design_saturation(meta, input$condition_col, covariates_in_use)
    bypass <- FALSE
    if (sat$saturated) {
      if (!isTRUE(input$no_rep_enable) || !isTRUE(input$no_rep_attest)) {
        showNotification(
          .t_fmt(.tr("\u274c Plan sans r\u00e9plicat : {n} \u00e9chantillon(s) pour {p} coefficient(s) \u2014 aucune dispersion estimable. Activez le mode exploratoire ET cochez l'attestation pour continuer."),
                 n = sat$n, p = sat$p),
          type = "error", duration = 12)
        return()
      }
      bypass <- TRUE
    }
    fixed_disp <- if (bypass) (input$no_rep_bcv %||% 0.4)^2 else NULL

    p <- shiny::Progress$new(); on.exit(p$close())
    p$set(message = .tr("Analyse différentielle..."), value = 0.2)

    tryCatch({
      design_str <- helpers$design_str()

      # STAT-Q1 : méthode de correction choisie dans le panneau Step 2.
      padj_method <- input$padj_method %||% TS_PADJ_METHOD_DEFAULT

      res <- NULL
      dds_full <- NULL
      if (bypass) {
        p$set(0.4, .tr("Ajustement edgeR (dispersion impos\u00e9e)..."))
        res <- run_bulk_de_dispatch("edger", shared_rv$filtered_counts, meta,
                                    input$condition_col, input$group_target, input$group_ref,
                                    covariates = covariates_in_use,
                                    p_adjust_method = padj_method,
                                    fixed_dispersion = fixed_disp)
      } else if (input$de_engine == "deseq2") {
        p$set(0.4, .tr("Ajustement DESeq2..."))
        dds_full <- build_dds(shared_rv$filtered_counts, meta, design_formula = design_str, run_deseq = TRUE)
        shared_rv$dds_full <- dds_full
        res <- run_bulk_de_dispatch("deseq2", shared_rv$filtered_counts, meta, input$condition_col,
                                    input$group_target, input$group_ref,
                                    dds = dds_full, shrink = input$shrink_lfc,
                                    p_adjust_method = padj_method)
      } else {
        p$set(0.5, .t_fmt(.tr("Ajustement {engine} ..."), engine = input$de_engine))
        res <- run_bulk_de_dispatch(input$de_engine, shared_rv$filtered_counts, meta,
                                    input$condition_col, input$group_target, input$group_ref,
                                    covariates = input$covariates %||% character(0),
                                    p_adjust_method = padj_method)
      }

      res <- .normalize_de_cols(res, counts_for_basemean = shared_rv$filtered_counts)

      # Traçabilité du contournement : l'information voyage AVEC le résultat
      # pour que le rapport, l'export et la provenance ne puissent jamais le
      # présenter comme une analyse différentielle ordinaire.
      shared_rv$de_bypass <- if (bypass) {
        list(engine = "edger", bcv = sqrt(fixed_disp), dispersion = fixed_disp,
             condition_col = input$condition_col, n = sat$n, p = sat$p,
             attested = TRUE, timestamp = Sys.time())
      } else NULL

      contrast_name <- if (nchar(trimws(input$contrast_name)) > 0) {
        trimws(input$contrast_name)
      } else {
        paste0(input$group_target, "_vs_", input$group_ref)
      }

      helpers$register_contrast(contrast_name, res)
      shared_rv$active_contrast <- contrast_name

      # STAT-Q1 : mémorise le contexte de recalcul de padj. Non-NULL uniquement
      # quand le moteur est DESeq2 (dds_full non NULL) — c'est ce qui autorise
      # le recalcul « live » depuis l'onglet DE, sans réajuster le modèle.
      if (!is.null(dds_full)) {
        helpers$remember_padj_ctx(contrast_name, dds_full, input$condition_col,
                                  input$group_target, input$group_ref,
                                  input$shrink_lfc, padj_method)
      }

      updateSelectInput(session, "active_contrast_view",
                        choices = names(shared_rv$contrasts), selected = contrast_name)

      n_sig <- sum(res$padj < input$padj_thresh & abs(res$log2FoldChange) > input$lfc_thresh, na.rm = TRUE)
      showNotification(.t_fmt(.tr("\u2713 Contraste '{c}': {n} g\u00e8nes significatifs"),
                              c = contrast_name, n = n_sig),
                       type = "message", duration = 6)

      if (bypass) {
        showNotification(
          .t_fmt(.tr("\u26a0\ufe0f Mode exploratoire sans r\u00e9plicat : dispersion impos\u00e9e (BCV = {b}), p-values NON inf\u00e9rentielles \u2014 \u00e0 ne pas publier comme une DE classique."),
                 b = sprintf("%.2f", sqrt(fixed_disp))),
          type = "warning", duration = 18)
      }

    }, error = function(e) {
      showNotification(paste(.tr("Erreur DE:"), e$message), type = "error", duration = 10)
    })
  })

  # =========================================================================
  # CONTRASTE AD-HOC (BingleSeq pattern) — manual Group A/B sample selection
  # =========================================================================
  observeEvent(shared_rv$filtered_counts, {
    req(shared_rv$filtered_counts)
    samples <- colnames(shared_rv$filtered_counts)
    updateCheckboxGroupInput(session, "adhoc_group_a", choices = samples, selected = character(0))
    updateCheckboxGroupInput(session, "adhoc_group_b", choices = samples, selected = character(0))
  })

  output$adhoc_readiness <- renderUI({
    global_data$language
    a <- input$adhoc_group_a %||% character(0)
    b <- input$adhoc_group_b %||% character(0)
    issues <- character(0)
    if (length(intersect(a, b)) > 0) issues <- c(issues, .tr("\u00c9chantillon(s) pr\u00e9sent(s) dans les 2 groupes."))
    if (length(a) == 0 || length(b) == 0) issues <- c(issues, .tr("S\u00e9lectionnez au moins 1 \u00e9chantillon / groupe."))
    else if (length(a) < 2 || length(b) < 2) issues <- c(issues, .tr("Un groupe a < 2 r\u00e9plicats \u2014 r\u00e9sultats peu fiables."))
    if (length(issues) == 0) return(NULL)
    div(class = "alert alert-warning", style = "font-size:0.78em;padding:4px 8px;",
        lapply(issues, tags$div))
  })

  observeEvent(input$run_de_adhoc, {
    req(shared_rv$filtered_counts, input$de_engine)
    a <- input$adhoc_group_a %||% character(0)
    b <- input$adhoc_group_b %||% character(0)
    if (length(intersect(a, b)) > 0) { showNotification(.tr("❌ Même échantillon dans les 2 groupes."), type = "error", duration = 6); return() }
    if (length(a) == 0 || length(b) == 0) { showNotification(.tr("❌ Sélectionnez au moins 1 échantillon / groupe."), type = "error", duration = 6); return() }

    p <- shiny::Progress$new(); on.exit(p$close())
    p$set(message = .tr("Analyse ad-hoc..."), value = 0.2)
    tryCatch({
      counts_sub <- shared_rv$filtered_counts[, c(a, b), drop = FALSE]
      meta_adhoc <- data.frame(
        condition = factor(c(rep("GroupA", length(a)), rep("GroupB", length(b))), levels = c("GroupB", "GroupA")),
        row.names = c(a, b)
      )
      padj_method <- input$padj_method %||% TS_PADJ_METHOD_DEFAULT
      dds_a <- NULL
      res <- if (input$de_engine == "deseq2") {
        dds_a <- build_dds(counts_sub, meta_adhoc, "~condition", run_deseq = TRUE)
        run_bulk_de_dispatch("deseq2", counts_sub, meta_adhoc, "condition", "GroupA", "GroupB", dds = dds_a, shrink = input$shrink_lfc,
                             p_adjust_method = padj_method)
      } else {
        run_bulk_de_dispatch(input$de_engine, counts_sub, meta_adhoc, "condition", "GroupA", "GroupB",
                             p_adjust_method = padj_method)
      }
      res <- .normalize_de_cols(res, counts_for_basemean = counts_sub)
      cname <- if (nchar(trimws(input$adhoc_contrast_name %||% "")) > 0) trimws(input$adhoc_contrast_name) else "GroupA_vs_GroupB_adhoc"
      helpers$register_contrast(cname, res)
      shared_rv$active_contrast <- cname
      # STAT-Q1 : contexte de recalcul padj (DESeq2 ad-hoc uniquement)
      if (!is.null(dds_a)) {
        helpers$remember_padj_ctx(cname, dds_a, "condition", "GroupA", "GroupB",
                                  input$shrink_lfc, padj_method)
      }
      updateSelectInput(session, "active_contrast_view", choices = names(shared_rv$contrasts), selected = cname)
      n_sig <- sum(res$padj < input$padj_thresh & abs(res$log2FoldChange) > input$lfc_thresh, na.rm = TRUE)
      showNotification(.t_fmt(.tr("\u2713 Ad-hoc '{c}': {n} g\u00e8nes sig."), c = cname, n = n_sig),
                       type = "message", duration = 6)
    }, error = function(e) showNotification(paste(.tr("Erreur DE ad-hoc:"), e$message), type = "error", duration = 10))
  })
}

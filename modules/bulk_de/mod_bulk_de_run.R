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
  # READINESS GUARD (G2). The observer below opens with
  # `req(shared_rv$filtered_counts, ...)`, which aborts in SILENCE when Step 1
  # has not run. Without this guard the poller would fire the token, the
  # observer would quietly do nothing, and `result.json` would report `done`
  # for a DE analysis that never started. The guard mirrors the observer's OWN
  # precondition — it is measured from the line below, not guessed — and
  # `shiny::isolate()` keeps the read out of the poller's dependency set
  # (reactivity is the module's business).
  drive_ready <- function() {
    if (is.null(shiny::isolate(shared_rv$filtered_counts))) {
      "no bulk object loaded (shared_rv$filtered_counts is NULL)"
    } else TRUE
  }
  # STATE PROBE (G3, third milestone) — see `state` in ts_drive_publish_token().
  #
  # `status: done` means only that the token MOVED, i.e. that this module's
  # observeEvent was TRIGGERED. It does not mean a contrast exists. The observer
  # below opens with `req(shared_rv$filtered_counts, input$condition_col,
  # input$group_ref, input$group_target, input$de_engine)`; when the design or
  # contrast injection has not taken, that `req()` aborts in SILENCE and the
  # agent is told `done` for an analysis that never ran. The probe below makes
  # "a contrast exists" OBSERVABLE instead of inferred.
  #
  # MEASURED live (2026-09-22, DESeq2, 17 925 genes x 18 samples): the token
  # answered `done` in 2.1 s, and the contrast was first observable 852.7 s
  # later. Fourteen minutes of that window look exactly like a run that never
  # started — unless the state is published, which is why it is.
  #
  # COUNTS ONLY, and every read isolate()-guarded: this runs inside the poller's
  # reactive beat (spec §6), so a bare read would enrol the DE result in the
  # poller's dependency set. Nothing here may ever return the result frame.
  #
  # `n_significant` is computed under a FIXED, NAMED convention rather than the
  # panel's thresholds: those are INPUTS, so a probe that borrowed them would
  # describe the last click instead of the state.
  #
  # SHRINKAGE (publication only — no line of the statistical path is touched).
  # The convention above names `|log2FoldChange| > 1`, and that half is evaluated
  # on WHATEVER the LFC column holds: shrunken, or raw MLE. Both are correct
  # counts; they are not the same reading, and nothing on the wire said which.
  # MEASURED on this host: `apeglm` is NOT installed, and for a `~condition`
  # design `use_coef` is TRUE, so the DEFAULT run takes the branch that asks
  # for apeglm, finds nothing, and publishes UNSHRUNK log2FoldChange with a
  # warning. An agent reading only `n_significant` would take those LFCs for
  # shrunken ones. `bulk_de_shrinkage_state()` reads the three attributes
  # `extract_deseq2_contrast()` already sets, so nothing is recomputed here.
  drive_state <- function() {
    contrasts <- shiny::isolate(shared_rv$contrasts)
    active    <- shiny::isolate(shared_rv$active_contrast)
    res <- if (!is.null(contrasts) && !is.null(active)) contrasts[[active]] else NULL
    out <- list(
      n_contrasts     = length(contrasts),
      active_contrast = active,
      n_genes         = NULL,
      n_padj_finite   = NULL,
      n_significant   = NULL,
      convention      = "padj < 0.05 & |log2FoldChange| > 1",
      bypass          = !is.null(shiny::isolate(shared_rv$de_bypass)),
      # Three scalars, and NULL — not FALSE — where they do not apply: an
      # edgeR/limma result has no shrinkage step, and "absent" is the fact.
      shrunk          = NULL,
      shrink_requested = NULL,
      shrink_method   = NULL
    )
    if (!is.null(res)) {
      out$n_genes       <- nrow(res)
      out$n_padj_finite <- sum(is.finite(res$padj))
      out$n_significant <- sum(res$padj < 0.05 & abs(res$log2FoldChange) > 1,
                               na.rm = TRUE)
      shr <- bulk_de_shrinkage_state(res)
      out$shrunk           <- shr$shrunk
      out$shrink_requested <- shr$shrink_requested
      out$shrink_method    <- shr$shrink_method
    }
    out
  }
  # LONG JOB (drive job contract, spec §5). This button is DECLARED long, so
  # `run_pipeline` answers `running` at dispatch instead of `done`, and this
  # module owes the protocol a terminal status through
  # `ts_drive_job_finish()` — see the observer below.
  #
  # The declaration is a MEASUREMENT, not a guess: MEASURED live 2026-09-22
  # (DESeq2, 17 925 genes x 18 samples), the token answered `done` in 2.1 s and
  # the contrast first became observable 852.7 s later. `done` is terminal, so
  # an agent was told in 2.1 s that a fourteen-minute job had finished.
  # S6 — INPUT CONFIRMATION (the Bulk DE pilot).
  #
  # The poller injects widget values with `shiny::update*Input()`, a CLIENT
  # ROUND-TRIP, so a value sent with a scenario is not in `input` yet when the same
  # tick fires the button. MEASURED live over four runs: run N read run N-1's
  # values, so `shrink_lfc = FALSE` was ignored, and a run with no prior injection
  # hit this observer's own `req()` and was recorded `invalid` with no reason.
  #
  # So the poller asks the OWNING MODULE what it actually observes, and only fires
  # once the answer matches the values it validated for THIS scenario. This is the
  # only side that can know, and it is the only place the read is honest.
  #
  # `differs` carries CONTROL NAMES only, never their values: the values are the
  # user's condition and sample labels. A human edit is reported, never
  # overwritten — the poller refuses and a new scenario is required.
  #
  # 🔴 THREE STATES, not two, and `prior` is what makes the difference. The drive
  # passes the values `input` held BEFORE it injected, and that is the only way to
  # tell "the round trip has not landed yet" from "a human changed this". A
  # two-state test cannot: MEASURED live (2026-09-27), the first version compared
  # observed against wanted only, and refused with "a human changed the control" on
  # a session with no human in it — blaming the user for a lag the protocol caused.
  #   observed == wanted -> fine
  #   observed == prior  -> not landed yet; report `waiting`, never `differs`
  #   observed == neither-> a real edit; report `differs`
  drive_confirm_inputs <- function(values = NULL, session_token = NULL,
                                   prior = NULL) {
    observed <- list(
      `bulk-de-condition_col` = input$condition_col,
      `bulk-de-group_target`  = input$group_target,
      `bulk-de-group_ref`     = input$group_ref,
      `bulk-de-de_engine`     = input$de_engine,
      `bulk-de-shrink_lfc`    = input$shrink_lfc
    )
    # 🔴 SCOPE: answer only about the controls THIS scenario carried.
    # The first version walked all five observed ids, so a control the scenario
    # never mentioned was still checked, and `group_ref` / `group_target` — selects
    # whose choices are rebuilt and which read as NULL mid-render — were reported
    # as `missing`. MEASURED live (2026-09-27): a scenario injecting ONLY
    # `bulk-de-shrink_lfc` was refused with "one or more controls never matched"
    # and an EMPTY id list, while the DOM readback showed
    # `bulk-de-shrink_lfc: checked=false` — the value had landed correctly. The
    # probe was refusing a run over two controls it had never been asked about.
    ids <- if (is.null(values) || !length(values)) names(observed) else names(values)
    missing <- character(0)
    differs <- character(0)
    waiting <- character(0)
    for (id in ids) {
      got <- observed[[id]]
      if (is.null(got) || (is.character(got) && !nzchar(trimws(got)))) {
        missing <- c(missing, id)
        next
      }
      if (id %in% names(values)) {
        want <- values[[id]]
        if (!isTRUE(all.equal(as.character(got), as.character(want)))) {
          # Not the wanted value. Is it still the one the session held before the
          # injection? Then the client round trip simply has not completed.
          was <- if (!is.null(prior) && id %in% names(prior)) prior[[id]] else NULL
          if (!is.null(was) &&
              isTRUE(all.equal(as.character(got), as.character(was)))) {
            waiting <- c(waiting, id)
          } else {
            differs <- c(differs, id)
          }
        }
      }
    }
    # `seq` is NULL, and DELIBERATELY so: a module cannot know the drive's
    # scenario sequence - it is not a widget and nothing the module can read - so
    # a required echo could only ever be NA, and the drive refused every run with
    # "is for a different scenario (seq NA, expected 5)". MEASURED live
    # 2026-09-27. What binds a confirmation to its scenario is the pending RECORD
    # the drive holds (one at a time, carrying that scenario's own values, key and
    # session token, cleared on a verdict), which is structural. A module MAY
    # return a `seq` and will then be held to it.
    list(ok = !length(missing) && !length(differs) && !length(waiting),
         seq = NULL,
         differs = differs,
         missing = missing,
         waiting = waiting,
         observed = observed)
  }

  ts_drive_publish_token(global_data, "bulk-de-run_de", drive_counter,
                         ready = drive_ready, state = drive_state, long = TRUE,
                         confirm_inputs = drive_confirm_inputs)
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
    # ── DECLARE THE JOB OVER (drive job contract, spec §5) ──────────────────
    # This is the TERMINAL PRODUCER that `long = TRUE` commits the module to.
    # It is registered BEFORE the first `req()`, so every path out of this
    # observer reports something: the early `req()` abort and the four
    # design-guard `return()`s all leave `job_state$outcome` at "refused", the
    # successful path sets "ok", and the computation's own catch sets "failed".
    # `on.exit()` is used rather than a call per exit path precisely so that a
    # guard ADDED LATER cannot forget to close the job.
    #
    # `job_state$outcome` is the MEASUREMENT of what happened, never a guess, and it
    # maps onto the FROZEN status enum instead of inventing a second one:
    #   ok      -> done     the contrast was registered
    #   failed  -> error    the computation raised
    #   refused -> invalid  a guard bailed; nothing ran, which is not a failure
    #                       (same reasoning as `invalid` elsewhere: a refusal
    #                       must not teach the operator to ignore a red badge)
    #
    # REGISTERED ONLY WHEN A DRIVE JOB IS ACTUALLY IN FLIGHT for this button.
    # `ts_drive_job_finish()` matches on the button id alone, so an
    # unconditional `on.exit()` would let a HUMAN clicking the same button close
    # a job the agent started. For a blocking job that is unreachable — the
    # event loop is frozen, so no click can be processed — but the guard is
    # written rather than relied upon, because the async conversion will make
    # that window real.
    # A MUTABLE CELL, NOT A LOCAL BINDING — and that distinction is the whole
    # reason this line is an environment.
    #
    # `<-` inside the `error = function(e)` handler below binds to the HANDLER's
    # frame, because a handler is a CLOSURE. A plain `job_outcome <- "failed"`
    # there left THIS frame at "refused", so the `on.exit()` published
    # `invalid` — "a guard bailed, nothing ran" — for a DE that had actually
    # RAISED, with `error = NULL` and no trace on the wire. Measured 2026-09-24
    # (STATUS.md §2do.4): the operator is invited to ignore a red badge, and a
    # real execution error is hidden. An environment has REFERENCE semantics,
    # so the handler's write IS the one the exit path reads.
    job_state <- new.env(parent = emptyenv())
    job_state$outcome <- "refused"
    job_state$error   <- NULL
    if (isTRUE(ts_drive_job_busy()) &&
        identical(ts_drive_job_state()$button, "bulk-de-run_de")) {
      on.exit(ts_drive_job_finish(
        "bulk-de-run_de",
        status = switch(job_state$outcome,
                        ok     = "done",
                        failed = "error",
                        "invalid"),
        error = if (identical(job_state$outcome, "failed")) {
          job_state$error %||% "the DE computation raised"
        } else NULL
      ), add = TRUE)
    }

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

    # `add = TRUE` is LOAD-BEARING, not style. A bare `on.exit()` REPLACES every
    # expression already pending, so this line silently discarded the drive job
    # declaration registered at the top of the observer above — the job would
    # have stayed `running` forever, and the agent would have polled a
    # completion that could never be written. Exactly the silent-seam failure
    # this repo keeps finding: nothing errors, nothing warns, the wire just
    # stops telling the truth.
    p <- shiny::Progress$new(); on.exit(p$close(), add = TRUE)
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

      # Last statement of the body, so it is reached only when the model was
      # fitted AND the contrast registered — the measurement the job
      # declaration above reports as `done`.
      job_state$outcome <- "ok"

    }, error = function(e) {
      # Written into the CELL. This handler is a closure, so a local assignment
      # here would never reach the `on.exit()` above — which is exactly the
      # defect this line replaces.
      job_state$outcome <- "failed"
      # The ONLY free field of the job contract, and the one that crosses the
      # badge into the UI: it must be sanitized, because an R condition message
      # can carry an absolute path or a token. Same 200-char budget as
      # `bulk_pathways`. The engine is named so the message stays actionable.
      job_state$error <- ts_drive_badge_sanitize(
        paste0("DE failed (", input$de_engine, "): ", conditionMessage(e)), 200L)
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

    # `add = TRUE` is LOAD-BEARING, not style. A bare `on.exit()` REPLACES every
    # expression already pending, so this line silently discarded the drive job
    # declaration registered at the top of the observer above — the job would
    # have stayed `running` forever, and the agent would have polled a
    # completion that could never be written. Exactly the silent-seam failure
    # this repo keeps finding: nothing errors, nothing warns, the wire just
    # stops telling the truth.
    p <- shiny::Progress$new(); on.exit(p$close(), add = TRUE)
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

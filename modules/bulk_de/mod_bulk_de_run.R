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
  # VOCABULARY PROBE (Slice 3) — the domain of the module's session-derived
  # selects, published by index (`TS_DRIVE_SESSION_INPUTS`). THE SAME closure
  # feeds the published state (below) and the applier's index resolution
  # (`vocab =` in ts_drive_publish_token), so the rev the agent pins and the
  # choices resolved at apply time cannot drift apart.
  #
  # Derivations mirror the module's OWN observers EXACTLY (mod_bulk_de_engine.R):
  # condition_col / covariates = the character/factor metadata columns (all names
  # as fallback), group_levels = unique non-NA levels of the ACTIVE condition
  # column (mirrored to shared_rv$active_condition_col by an existing observe —
  # reading it keeps the probe out of `input`'s reactive domain). Empty lists are
  # the honest "not ready": the drive refuses INPUT_NOT_READY until data exists.
  #
  # `vocab_rev` is a MONOTONE counter bumped whenever the three lists change
  # (fingerprint compare) — the probe is the only writer, so no existing
  # observer is touched and the human UI is unchanged. Reactive reads are
  # isolate()-guarded: this runs inside the poller's beat (spec §6).
  drive_vocab_state <- new.env(parent = emptyenv())
  drive_vocab_state$rev <- 0L
  drive_vocab_state$fp  <- NULL
  drive_vocabulary <- function() {
    meta <- tryCatch(shiny::isolate(global_data$bulk_obj)$metadata, error = function(e) NULL)
    cat_cols <- if (is.null(meta) || !ncol(meta)) character(0) else {
      cols <- names(meta)[vapply(meta, function(x) is.character(x) || is.factor(x), logical(1))]
      if (!length(cols)) names(meta) else cols
    }
    col <- tryCatch(shiny::isolate(shared_rv$active_condition_col), error = function(e) NULL)
    lvls <- if (is.null(meta) || is.null(col) || is.na(col) ||
                !col %in% names(meta)) character(0) else {
      unique(na.omit(as.character(meta[[col]])))
    }
    fp <- paste(c(length(cat_cols), cat_cols, length(lvls), lvls),
                collapse = "\u0001")
    if (!identical(fp, drive_vocab_state$fp)) {
      drive_vocab_state$rev <- drive_vocab_state$rev + 1L
      drive_vocab_state$fp  <- fp
    }
    list(
      condition_col = cat_cols,
      covariates    = cat_cols,
      group_levels  = lvls,
      vocab_rev     = drive_vocab_state$rev
    )
  }
  # Publish the vocabulary INSIDE the state (the wire channel): the poller
  # writes it with every result.json snapshot, projected by
  # ts_drive_project_vocabulary (D1-verbatim choices, closed keep-set).
  drive_state2 <- function() {
    s <- drive_state()
    s$vocabulary <- drive_vocabulary()
    s
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
                         ready = drive_ready, state = drive_state2, long = TRUE,
                         confirm_inputs = drive_confirm_inputs,
                         vocab = drive_vocabulary)
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
  # ── D4(a) (2026-10-23) : la DE pilotée ne bloque plus le thread ───────────
  # Suite de 45049a2 (ad-hoc) et prescription `docs/agent/HANDOFF.md`. Le chemin
  # piloté — qui sert AUSSI le bouton humain via drive_trigger — tournait encore
  # en synchrone : DESeq2 mesurée à 852,7 s gelait l'interface. Le calcul part
  # donc dans le pool mirai par le MÊME run_de_core() (daemon et repli
  # synchrone exécutent le MÊME code, règle 3).
  #
  # LE CONTRAT DU JOB DÉMÉNAGE. Un bloc qui REND LA MAIN à la soumission ne
  # peut pas fermer le job par `on.exit()` : l'agent lirait `done` 2,1 s après
  # le token pour un calcul de quatorze minutes — exactement le mensonge que la
  # déclaration `long = TRUE` existait pour corriger. La clôture appartient à
  # l'OBSERVATEUR DE RÉSULTAT :
  #   refused   -> invalid  fermé synchrone (un garde a renoncé, rien n'a tourné)
  #   submitted -> PAS de fermeture ici : la cellule est déposée dans
  #                de_drive_job$cell, l'observateur de statut la consomme
  #   ok/failed (repli synchrone) -> fermé explicitement en ligne
  # L'`on.exit()` de secours ne ferme que ce qui est RESTÉ à "refused" — un
  # garde AJOUTÉ PLUS TARD ne peut pas oublier de fermer le job (la propriété
  # que l'ancien contrat garantissait, conservée pour les refus).
  de_drive_task <- ExtendedTask$new(function(counts, meta, engine, condition_col,
                                             group_target, group_ref, covariates,
                                             shrink_lfc, padj_method, fixed_disp,
                                             design_formula) {
    mirai::mirai(
      run_de_core(counts, meta, engine, condition_col, group_target, group_ref,
                  shrink = shrink_lfc, p_adjust_method = padj_method,
                  covariates = covariates, fixed_dispersion = fixed_disp,
                  design_formula = design_formula),
      counts = counts, meta = meta, engine = engine,
      condition_col = condition_col, group_target = group_target,
      group_ref = group_ref, covariates = covariates, shrink_lfc = shrink_lfc,
      padj_method = padj_method, fixed_disp = fixed_disp,
      design_formula = design_formula
    )
  })
  # La cellule du job drive en vol — une RÉFÉRENCE, pas une copie : le trigger
  # la DÉPOSE, l'observateur de statut la CONSOMME. NULL = rien en attente.
  de_drive_job <- new.env(parent = emptyenv())
  de_drive_job$cell <- NULL

  # La publication du résultat (interface + traçabilité), commune au chemin
  # asynchrone et au repli synchrone. `ctx` porte les DÉCISIONS D'INTERFACE
  # (nom du contraste, seuils d'affichage, infos de contournement) : elles sont
  # prises ICI, jamais dans le daemon.
  .de_drive_commit <- function(out, ctx) {
    res <- out$res
    if (!is.null(out$dds)) shared_rv$dds_full <- out$dds
    shared_rv$de_bypass <- ctx$bypass_info
    helpers$register_contrast(ctx$cname, res, engine = ctx$engine)
    shared_rv$active_contrast <- ctx$cname
    if (!is.null(out$dds)) {
      helpers$remember_padj_ctx(ctx$cname, out$dds, ctx$condition_col,
                                ctx$group_target, ctx$group_ref,
                                ctx$shrink_lfc, ctx$padj_method)
    }
    updateSelectInput(session, "active_contrast_view",
                      choices = names(shared_rv$contrasts), selected = ctx$cname)
    n_sig <- sum(res$padj < ctx$padj_thresh &
                   abs(res$log2FoldChange) > ctx$lfc_thresh, na.rm = TRUE)
    showNotification(.t_fmt(.tr("\u2713 Contraste '{c}': {n} g\u00e8nes significatifs"),
                            c = ctx$cname, n = n_sig),
                     type = "message", duration = 6)
    if (!is.null(ctx$bypass_info)) {
      showNotification(
        .t_fmt(.tr("\u26a0\ufe0f Mode exploratoire sans r\u00e9plicat : dispersion impos\u00e9e (BCV = {b}), p-values NON inf\u00e9rentielles \u2014 \u00e0 ne pas publier comme une DE classique."),
               b = sprintf("%.2f", sqrt(ctx$bypass_info$dispersion))),
        type = "warning", duration = 18)
    }
  }
  # Notification + message caviardé pour le champ `error` du contrat (même
  # budget de 200 caractères que `bulk_pathways` ; le moteur est nommé pour que
  # le message reste actionnable).
  .de_drive_fail <- function(ctx, msg) {
    showNotification(paste(.tr("Erreur DE:"), msg), type = "error", duration = 10)
    ts_drive_badge_sanitize(paste0("DE failed (", ctx$engine, "): ", msg), 200L)
  }

  observeEvent(drive_trigger(), {
    # ── CONTRAT DU JOB (drive, spec §5) ─────────────────────────────────────
    # La cellule mutable reste la MEASURE de ce qui s'est passé, et elle est
    # crée AVANT le premier `req()` pour que chaque sortie rapporte quelque
    # chose. `owns_job` est décidé À L'ENTRÉE (comme l'ancien on.exit
    # conditionnel) : `ts_drive_job_finish()` matchant sur l'id du bouton seul,
    # un clic HUMAIN sur le même bouton ne doit jamais fermer un job que
    # l'agent a lancé.
    job_state <- new.env(parent = emptyenv())
    job_state$outcome <- "refused"
    job_state$closed  <- FALSE
    job_state$owns_job <- isTRUE(ts_drive_job_busy()) &&
      identical(ts_drive_job_state()$button, "bulk-de-run_de")
    job_state$finish <- function(status, error = NULL) {
      if (isTRUE(job_state$closed) || !isTRUE(job_state$owns_job)) return(invisible(NULL))
      job_state$closed <- TRUE
      ts_drive_job_finish("bulk-de-run_de", status = status, error = error)
    }
    # Filet de secours : à la sortie du frame, ce qui est RESTÉ à "refused" est
    # un refus (abort de `req()`, garde `return()`) -> `invalid`. La raison d'un
    # refus voyage sur le wire (F4, 2026-10-06). Une SOUMISSION passe `outcome`
    # à "submitted" AVANT de rendre la main : le filet ne la touche pas. Le
    # repli synchrone ferme explicitement — `closed` interdit la double
    # fermeture.
    on.exit(
      if (identical(job_state$outcome, "refused"))
        job_state$finish("invalid", job_state$reason),
      add = TRUE)

    req(input$run_de > 0 || shiny::isolate(drive_counter()) > 0)
    req(shared_rv$filtered_counts, input$condition_col, input$group_ref, input$group_target,
        input$de_engine)

    if (input$group_ref == input$group_target) {
      showNotification(.tr("⚠️ Le groupe Référence et le groupe Cible doivent être différents."),
                       type = "warning")
      job_state$reason <- "ref and target name the same group"
      return()
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
      job_state$reason <- "a covariate fully mirrors the condition; drop it from the design"
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
      job_state$reason <- "a covariate has one level only; it adds nothing to the design"
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
        job_state$reason <- "not enough samples per coef: enable the attest box to run anyway"
        return()
      }
      bypass <- TRUE
    }
    fixed_disp <- if (bypass) (input$no_rep_bcv %||% 0.4)^2 else NULL

    # ── DÉCISIONS D'INTERFACE, prises AVANT de rendre la main ────────────────
    # STAT-Q1 : méthode de correction choisie dans le panneau Step 2.
    padj_method <- input$padj_method %||% TS_PADJ_METHOD_DEFAULT
    design_str  <- helpers$design_str()
    # M13 : le moteur qui a RÉELLEMENT produit ce contraste — "edger" quand le
    # mode exploratoire sans réplicat impose la dispersion, quelle que soit la
    # sélection affichée dans le panneau.
    engine_run   <- if (bypass) "edger" else input$de_engine
    contrast_name <- if (nchar(trimws(input$contrast_name)) > 0) {
      trimws(input$contrast_name)
    } else {
      paste0(input$group_target, "_vs_", input$group_ref)
    }
    ctx <- list(
      cname = contrast_name, engine = engine_run,
      condition_col = input$condition_col, group_target = input$group_target,
      group_ref = input$group_ref, shrink_lfc = input$shrink_lfc,
      padj_method = padj_method, padj_thresh = input$padj_thresh,
      lfc_thresh = input$lfc_thresh,
      # Traçabilité du contournement : l'information voyage AVEC le résultat
      # pour que le rapport, l'export et la provenance ne puissent jamais le
      # présenter comme une analyse différentielle ordinaire.
      bypass_info = if (bypass) {
        list(engine = "edger", bcv = sqrt(fixed_disp), dispersion = fixed_disp,
             condition_col = input$condition_col, n = sat$n, p = sat$p,
             attested = TRUE, timestamp = Sys.time())
      } else NULL
    )
    # La charge PURE du calcul — le MÊME run_de_core() pour le daemon et le
    # repli synchrone (préchargé via APP_DAEMON_SOURCE_FILES, ordre prouvé).
    # Les clés sont les FORMALS de run_de_core() : le repli en fait un
    # `do.call` direct, la tâche les repasse nommés à son corps mirai.
    core <- list(
      counts_matrix = shared_rv$filtered_counts, metadata = meta,
      engine = engine_run,
      condition_col = input$condition_col, group_target = input$group_target,
      group_ref = input$group_ref, covariates = covariates_in_use,
      shrink = input$shrink_lfc, p_adjust_method = padj_method,
      fixed_dispersion = fixed_disp, design_formula = design_str)

    # Pool disponible ? Même doctrine que l'ad-hoc : le repli synchrone est
    # DÉCLARÉ (notification), jamais muet.
    pool_ok <- requireNamespace("mirai", quietly = TRUE) &&
      isTRUE(tryCatch(ensure_app_daemons(), error = function(e) FALSE)) &&
      isTRUE(mirai::daemons_set())

    if (!pool_ok) {
      showNotification(
        .tr("Pool de calcul indisponible : l'analyse diff\u00e9rentielle tourne en synchrone \u2014 l'interface restera bloqu\u00e9e pendant le calcul."),
        type = "warning", duration = 10)
      # `add = TRUE` is LOAD-BEARING (leçon conservée du contrat sync) : un
      # `on.exit()` nu REMPLACERAIT le filet de refus ci-dessus.
      p <- shiny::Progress$new(); on.exit(p$close(), add = TRUE)
      p$set(message = .tr("Analyse diff\u00e9rentielle..."), value = 0.2)
      out <- tryCatch(do.call(run_de_core, core), error = function(e) e)
      if (inherits(out, "error")) {
        job_state$outcome <- "failed"
        job_state$finish("error", .de_drive_fail(ctx, conditionMessage(out)))
      } else {
        job_state$outcome <- "ok"
        .de_drive_commit(out, ctx)
        job_state$finish("done", NULL)
      }
      return()
    }

    # ── SOUMISSION NON BLOQUANTE ─────────────────────────────────────────────
    # La cellule part dans le réflecteur du module ; l'observateur de statut
    # (ci-dessous) la consomme et ferme le job. `outcome` passe à "submitted"
    # AVANT le return pour que le filet de refus ne touche pas à la soumission.
    job_state$outcome <- "submitted"
    job_state$ctx     <- ctx
    de_drive_job$cell <- job_state
    showNotification(.tr("Analyse diff\u00e9rentielle en cours..."),
                     id = "de_drive_progress", type = "message", duration = NULL)
    de_drive_task$invoke(core$counts_matrix, core$metadata, core$engine,
                         core$condition_col, core$group_target, core$group_ref,
                         core$covariates, core$shrink, core$p_adjust_method,
                         core$fixed_dispersion, core$design_formula)
  })

  # ── Observateur de RÉSULTAT : le SEUL endroit où un job soumis peut être ──
  # fermé `done`/`error` (contrat D4(a)). Il CONSOMME la cellule — jamais deux
  # fermetures pour une soumission — et suit la convention du dépôt
  # (mod_spatial_cluster.R) : `result()` n'est appelé QUE protégé, un statut
  # `error` ne doit jamais lever à son tour.
  observeEvent(de_drive_task$status(), {
    st <- de_drive_task$status()
    cell <- de_drive_job$cell
    de_drive_job$cell <- NULL
    if (is.null(cell)) return()   # défensif : un statut sans soumission ne ferme rien
    if (identical(st, "success")) {
      removeNotification("de_drive_progress")
      out <- tryCatch(de_drive_task$result(), error = function(e) e)
      if (inherits(out, "error")) {
        cell$finish("error", .de_drive_fail(cell$ctx, conditionMessage(out)))
      } else {
        .de_drive_commit(out, cell$ctx)
        cell$finish("done", NULL)
      }
    } else if (identical(st, "error")) {
      removeNotification("de_drive_progress")
      err <- tryCatch(de_drive_task$result(), error = function(e) e)
      msg <- if (inherits(err, "error")) conditionMessage(err)
             else "le calcul DE a \u00e9chou\u00e9 dans le pool"
      cell$finish("error", .de_drive_fail(cell$ctx, msg))
    }
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

  # ── D4 (2026-10-20) : calcul DE ad-hoc NON BLOQUANT ──────────────────────
  # Pattern identique aux sous-onglets spatiaux : ExtendedTask$new(fn) où fn
  # rend un mirai. Le corps appelle run_de_core() (R/bulk/bulk_helpers.R,
  # préchargé dans les daemons via APP_DAEMON_SOURCE_FILES) : le daemon et le
  # repli synchrone exécutent donc le MÊME code, pas deux copies.
  de_adhoc_task <- ExtendedTask$new(function(counts, meta, engine, shrink_lfc, padj_method) {
    mirai::mirai(
      run_de_core(counts, meta, engine, "condition", "GroupA", "GroupB",
                  shrink = shrink_lfc, p_adjust_method = padj_method),
      counts = counts, meta = meta, engine = engine,
      shrink_lfc = shrink_lfc, padj_method = padj_method
    )
  })
  # Le résultat ne transporte que le CALCUL : le nom du contraste est une
  # décision d'interface, prise ici, jamais dans le daemon.
  de_adhoc_ctx <- shiny::reactiveVal(NULL)

  .de_adhoc_commit <- function(out, ctx) {
    res <- out$res
    helpers$register_contrast(ctx$cname, res, engine = ctx$engine)
    shared_rv$active_contrast <- ctx$cname
    # STAT-Q1 : le dds revient du daemon avec le résultat, donc le recalcul
    # « live » de padj reste disponible sans réajuster le modèle.
    if (!is.null(out$dds)) {
      helpers$remember_padj_ctx(ctx$cname, out$dds, "condition", "GroupA", "GroupB",
                                ctx$shrink_lfc, ctx$padj_method)
    }
    updateSelectInput(session, "active_contrast_view",
                      choices = names(shared_rv$contrasts), selected = ctx$cname)
    n_sig <- sum(res$padj < input$padj_thresh & abs(res$log2FoldChange) > input$lfc_thresh, na.rm = TRUE)
    showNotification(.t_fmt(.tr("\u2713 Ad-hoc '{c}': {n} g\u00e8nes sig."), c = ctx$cname, n = n_sig),
                     type = "message", duration = 6)
  }

  observeEvent(input$run_de_adhoc, {
    req(shared_rv$filtered_counts, input$de_engine)
    a <- input$adhoc_group_a %||% character(0)
    b <- input$adhoc_group_b %||% character(0)
    if (length(intersect(a, b)) > 0) { showNotification(.tr("❌ Même échantillon dans les 2 groupes."), type = "error", duration = 6); return() }
    if (length(a) == 0 || length(b) == 0) { showNotification(.tr("❌ Sélectionnez au moins 1 échantillon / groupe."), type = "error", duration = 6); return() }

    counts_sub <- shared_rv$filtered_counts[, c(a, b), drop = FALSE]
    meta_adhoc <- data.frame(
      condition = factor(c(rep("GroupA", length(a)), rep("GroupB", length(b))), levels = c("GroupB", "GroupA")),
      row.names = c(a, b)
    )
    padj_method <- input$padj_method %||% TS_PADJ_METHOD_DEFAULT
    ctx <- list(
      cname = if (nchar(trimws(input$adhoc_contrast_name %||% "")) > 0) trimws(input$adhoc_contrast_name) else "GroupA_vs_GroupB_adhoc",
      engine = input$de_engine,
      shrink_lfc = input$shrink_lfc,
      padj_method = padj_method
    )
    de_adhoc_ctx(ctx)

    # Le repli synchrone est DÉCLARÉ (notification), jamais muet : même
    # doctrine que mod_sc_da_milo.R — l'utilisateur doit savoir que
    # l'interface va rester bloquée.
    pool_ok <- isTRUE(ensure_app_daemons()) &&
               requireNamespace("mirai", quietly = TRUE) &&
               isTRUE(mirai::daemons_set())
    if (!pool_ok) {
      showNotification(
        .tr("Pool de calcul indisponible : l'analyse ad-hoc tourne en synchrone — l'interface restera bloquée pendant le calcul."),
        type = "warning", duration = 10)
      out <- tryCatch(
        run_de_core(counts_sub, meta_adhoc, ctx$engine, "condition", "GroupA", "GroupB",
                    shrink = ctx$shrink_lfc, p_adjust_method = ctx$padj_method),
        error = function(e) e)
      if (inherits(out, "error")) {
        showNotification(paste(.tr("Erreur DE ad-hoc:"), conditionMessage(out)),
                         type = "error", duration = 10)
      } else {
        .de_adhoc_commit(out, ctx)
      }
      return()
    }

    showNotification(.tr("Analyse ad-hoc en cours..."),
                     id = "de_adhoc_progress", type = "message", duration = NULL)
    de_adhoc_task$invoke(counts_sub, meta_adhoc, ctx$engine, ctx$shrink_lfc, ctx$padj_method)
  })

  # Convention du dépôt (cf. mod_spatial_cluster.R) : on observe le STATUT et
  # on n'appelle result() QUE sur succès — un result() sur un job échoué lève.
  observeEvent(de_adhoc_task$status(), {
    st <- de_adhoc_task$status()
    if (identical(st, "success")) {
      removeNotification("de_adhoc_progress")
      out <- de_adhoc_task$result()
      ctx <- de_adhoc_ctx()
      if (!is.null(ctx)) .de_adhoc_commit(out, ctx)
    } else if (identical(st, "error")) {
      removeNotification("de_adhoc_progress")
      showNotification(
        .tr("Erreur pendant l'analyse ad-hoc — voir le journal. Relancez, ou utilisez 'Reinitialiser les daemons'."),
        type = "error", duration = 10)
    }
  })
}

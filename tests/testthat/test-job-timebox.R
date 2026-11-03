# =============================================================================
# test-job-timebox.R — N7 timebox (satellites slice S3) : helpers PURS
# =============================================================================
# FICHE (fait foi) :
# docs/proposals/PROPOSAL_N_SATELLITES_LAUNCH_ROADMAP_2026-10-26.md
#   §slice S3 (N7 timebox) : budgets déclarés (defaults.R), elapsed vs budget
#   rendu sur la surface existante, overrun -> provenance. Brief jalon round8
#   (docs/agent/swarm/round8/NEXT_SESSION_BRIEF_2026-11-03.md jalon 3) :
#   AC3.1 — verrou structurel grep : noms TS_JOB_BUDGET_S_ + valeurs
#           numériques positives + commentaire de fallback documenté ;
#   AC3.2 — overrun simulé : budget dépassé -> ligne {job, budget, actual,
#           ratio} mesurée dans la sortie provenance (canal EXISTANT
#           new_provenance_entry() provenance.R:128 / provenance_append()
#           provenance.R:168 — réutilisation, pas duplication) ;
#   AC3.3 — formatage pur « en cours — X (typique Y–Z) » (test pur, pas de
#           testServer : le formatage est extrait pur).
#   Ancres : 537 s mesurés (fiche docs/ideation/SC_EXTENDED_TASK_ASYNC.md:5,13
#   — job sc-pipeline-run_auto_pipeline, PBMC 1k synchrone ; le brief jalon 3
#   cite cette ancre comme « job DE réel 537 s ») ; budgets GÉNÉREUX déclarés
#   dans defaults.R, jamais devinés fin ; job inconnu -> fallback + UNTESTED
#   (pas d'invention).
#
# Rails RED (règle error=0) : au stade RED les corps sont des STUBS (NULL) —
# chaque expectation échoue comme ÉCHEC, jamais comme erreur ; la section
# dépendante du canal provenance est skip_if_not(is.list(entry)) pour qu'un
# stub NULL ne transforme pas le rouge en crash.
# =============================================================================

source_project_file("tools/pkg_skeleton/R/defaults.R")
source_project_file("tools/pkg_skeleton/R/state.R")
source_project_file("tools/pkg_skeleton/R/provenance.R")
source_project_file("tools/pkg_skeleton/R/job_timebox.R")

# ── (a) Verrou structurel grep sur defaults.R (AC3.1) ------------------------

test_that("(a) constantes TS_JOB_BUDGET_S_* présentes dans defaults.R, positives, fallback documenté", {
  lines <- readLines(file.path(ts_project_root(), "tools/pkg_skeleton/R/defaults.R"),
                     warn = FALSE, encoding = "UTF-8")
  # Le verrou porte sur le BLOC « Timebox » (du commentaire de section au
  # séparateur suivant) — un « 537 » ailleurs dans defaults.R ne doit pas
  # faire passer le lock.
  i0 <- grep("Timebox des jobs", lines)
  expect_length(i0, 1L)
  skip_if_not(length(i0) == 1L)
  ends <- grep("^# --- ", lines)
  ends <- ends[ends > i0]
  block <- lines[i0:(if (length(ends)) ends[1L] - 1L else length(lines))]
  block_src <- paste(block, collapse = "\n")

  # noms TS_JOB_BUDGET_S_ + valeurs numériques POSITIVES (littérales)
  defs <- grep("^TS_JOB_BUDGET_S_[A-Z0-9_]+\\s*<-\\s*[0-9]", block, value = TRUE)
  expect_true(length(defs) >= 2L,
              info = "au moins le job câblé ET le fallback doivent être déclarés")
  skip_if_not(length(defs) >= 2L)
  tb_names <- unique(sub("^(TS_JOB_BUDGET_S_[A-Z0-9_]+)\\s*<-.*$", "\\1", defs))
  for (nm in tb_names) {
    v <- get(nm, envir = globalenv())
    expect_true(is.numeric(v) && length(v) == 1L && is.finite(v) && v > 0,
                info = paste(nm, "doit être un nombre strictement positif"))
  }
  # constante du job CÂBLÉ (surface mod_bulk_pathways) + fallback déclarés
  expect_true("TS_JOB_BUDGET_S_BULK_PATHWAYS_RUN_PATHWAY" %in% tb_names,
              info = "budget du job pathway (bulk-pathways-run_pathway)")
  expect_true("TS_JOB_BUDGET_S_UNDECLARED" %in% tb_names,
              info = "fallback « job inconnu » déclaré")
  # commentaire de FALLBACK documenté + ancre mesurée 537 s + mention UNTESTED
  expect_true(grepl("FALLBACK|fallback", block_src),
              info = "le fallback doit être documenté dans le commentaire")
  expect_true(grepl("537", block_src),
              info = "l'ancre mesurée (537 s) doit être citée, jamais devinée fin")
  expect_true(grepl("UNTESTED", block_src),
              info = "les budgets sans mesure propre portent la mention UNTESTED")
})

# ── (b) ts_job_budget() : constante correspondante / fallback UNTESTED -------

test_that("(b) ts_job_budget() retourne le budget de la constante correspondante ; job inconnu -> fallback UNTESTED", {
  # job CÂBLÉ de la surface mod_bulk_pathways (boutons drive bulk-pathways-*)
  expected_pathway <- tryCatch(
    get("TS_JOB_BUDGET_S_BULK_PATHWAYS_RUN_PATHWAY", envir = globalenv()),
    error = function(e) NA_real_)
  expect_identical(as.numeric(ts_job_budget("bulk-pathways-run_pathway")),
                   as.numeric(expected_pathway))
  expected_scores <- tryCatch(
    get("TS_JOB_BUDGET_S_BULK_PATHWAYS_RUN_SCORES", envir = globalenv()),
    error = function(e) NA_real_)
  expect_identical(as.numeric(ts_job_budget("bulk-pathways-run_scores")),
                   as.numeric(expected_scores))
  # job ANCRÉ (537 s mesurés — fiche SC_EXTENDED_TASK_ASYNC.md:13)
  expected_sc <- tryCatch(
    get("TS_JOB_BUDGET_S_SC_PIPELINE_RUN_AUTO_PIPELINE", envir = globalenv()),
    error = function(e) NA_real_)
  expect_identical(as.numeric(ts_job_budget("sc-pipeline-run_auto_pipeline")),
                   as.numeric(expected_sc))
  # job inconnu -> budget généreux documenté + attribut UNTESTED (pas d'invention)
  fb <- ts_job_budget("job-inconnu-nimportequoi")
  expected_fb <- tryCatch(
    get("TS_JOB_BUDGET_S_UNDECLARED", envir = globalenv()),
    error = function(e) NA_real_)
  expect_identical(as.numeric(fb), as.numeric(expected_fb))
  expect_true(isTRUE(attr(fb, "untested")),
              info = "le fallback porte l'attribut/flag UNTESTED")
  # entrées invalides -> échec dur (jamais un budget mensonger)
  expect_error(ts_job_budget(""))
  expect_error(ts_job_budget(NA_character_))
  expect_error(ts_job_budget(c("a", "b")))
})

# ── (c) ts_job_budget_label() : formatage pur (AC3.3) ------------------------

test_that("(c) ts_job_budget_label() rend elapsed h/min/s + fourchette typique ; variantes overrun", {
  # format de référence du brief jalon 3 : « en cours — 3 min 12 s (typique 5–10 min) »
  expect_identical(ts_job_budget_label(192, 600),
                   "en cours \u2014 3 min 12 s (typique 5\u201310 min)")
  # secondes seules / fourchette dérivée (budget/2 = 60 s -> classe « min »)
  expect_identical(ts_job_budget_label(45, 120),
                   "en cours \u2014 45 s (typique 1\u20132 min)")
  # fourchette à classes d'unité MIXTES (basse explicite) -> deux durées
  expect_identical(ts_job_budget_label(45, 120, typical_low_s = 30),
                   "en cours \u2014 45 s (typique 30 s\u20132 min)")
  # heures : unité factorisée sur la fourchette
  expect_identical(ts_job_budget_label(3661, 7200),
                   "en cours \u2014 1 h 01 min (typique 1\u20132 h)")
  # variante OVERRUN (« dépassé ») : le job tourne toujours, le ratio est rendu
  .lab_over <- ts_job_budget_label(725, 600)
  expect_true(grepl("en cours", .lab_over),
              info = "un job au-delà de son budget est toujours « en cours »")
  expect_true(grepl("d\u00e9pass\u00e9", .lab_over),
              info = "la variante overrun porte « dépassé »")
  expect_identical(ts_job_budget_label(725, 600),
                   paste0("en cours \u2014 12 min 5 s (d\u00e9pass\u00e9 \u2014 typique ",
                          "5\u201310 min, \u00d71.2)"))
  # injecteur i18n (motif builders R/plotting) : clé FR -> template traduit
  .tr_dict <- function(k) {
    d <- list("en cours \u2014 {elapsed}" = "in progress \u2014 {elapsed}")
    d[[k]] %||% k
  }
  expect_identical(ts_job_budget_label(192, 600, tr = .tr_dict),
                   "in progress \u2014 3 min 12 s (typique 5\u201310 min)")
  # entrées invalides -> échec dur
  expect_error(ts_job_budget_label(-1, 600))
  expect_error(ts_job_budget_label(NA_real_, 600))
  expect_error(ts_job_budget_label(10, 0))
  expect_error(ts_job_budget_label(10, "x"))
  expect_error(ts_job_budget_label(10, 600, typical_low_s = 900),
               info = "borne basse > budget : bug d'appelant")
})

# ── (d) ts_job_overrun_entry() : canal provenance EXISTANT (AC3.2) -----------

test_that("(d) ts_job_overrun_entry() construit {job, budget, actual, ratio} via new_provenance_entry()/provenance_append()", {
  entry <- ts_job_overrun_entry("bulk-pathways-run_pathway", 600, 725)
  expect_true(is.list(entry), info = "l'entrée est posable dans l'état partagé")
  skip_if_not(is.list(entry))  # RED : stub NULL -> échec propre, jamais un crash
  # champs métier {job, budget, actual, ratio}
  expect_equal(entry$parameters$job, "bulk-pathways-run_pathway")
  expect_equal(entry$parameters$budget_s, 600)
  expect_equal(entry$parameters$actual_s, 725)
  expect_true(entry$parameters$ratio > 1, info = "ratio > 1 au dépassement")
  expect_equal(entry$parameters$ratio, 725 / 600)
  # structure du canal EXISTANT (contrat new_provenance_entry, provenance.R:128)
  expect_true(nzchar(entry$analysis_id))
  expect_true(nzchar(entry$method))
  expect_true(nzchar(entry$warnings[1]),
              info = "l'entrée porte un avertissement lisible")
  # le canal EXISTANT l'accepte : append dans un état partagé (motif modules —
  # state_get/state_set de state.R, reactiveValues comme shared_rv en module)
  state <- shiny::reactiveValues()
  idx <- provenance_append(state, entry)
  expect_identical(idx, 1L)
  prov <- state_get(state, "provenance")
  expect_length(prov, 1L)
  expect_equal(prov[[1L]]$parameters$ratio, 725 / 600)
  expect_equal(prov[[1L]]$parameters$job, "bulk-pathways-run_pathway")
  expect_equal(prov[[1L]]$parameters$actual_s, 725)
  # gardes : PAS un dépassement -> pas d'entrée (jamais une ligne mensongère)
  expect_error(ts_job_overrun_entry("bulk-pathways-run_pathway", 600, 600))
  expect_error(ts_job_overrun_entry("bulk-pathways-run_pathway", 600, 100))
  expect_error(ts_job_overrun_entry("", 600, 900))
  expect_error(ts_job_overrun_entry("bulk-pathways-run_pathway", -1, 900))
  expect_error(ts_job_overrun_entry("bulk-pathways-run_pathway", 600, NA_real_))
})

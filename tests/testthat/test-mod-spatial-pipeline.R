# =============================================================================
# test-mod-spatial-pipeline.R — tests for modules/spatial/mod_spatial_pipeline.R
# =============================================================================
# 26ᵉ incrément de la dette de conventions (2026-09-18, §2cl).
# Créé AVANT la conversion (règle 5 : test d'abord, rouge d'abord) ⇒ ce fichier
# doit ÉCHOUER sur l'assertion de classe tant que le `stop()` ne porte pas
# `class = "spatial_pipeline_error"`.
#
# 🟢 **Lot choisi par le critère §2cj.1, MESURÉ sur les 12 sites de `modules/`** :
# le site est dans un corps **`mirai::mirai({ … })`** = du **R PUR** exécuté dans
# un démon, donc extractible par l'**AST** et `eval()`-able dans un
# **environnement enfant**. Le site est la **PREMIÈRE instruction** du corps ⇒
# il tire **immédiatement** quand `requireNamespace()` est mocké, sans aucune
# fixture (ni objet Seurat, ni fichier, ni répertoire BPCells).
#
# ⚠️ Le fichier contient **7** corps `mirai` (lignes 286, 338, 358, 372, 399,
# 437, 452) et **UN SEUL** `stop()` — celui du premier corps. On sélectionne donc
# le corps **PAR CONTENU**, jamais par index : un 8ᵉ corps ajouté décalerait tout
# silencieusement.
#
# ⚠️ **PIÈGE AST** (§2cj.2) : un parcours qui fait `for (part in as.list(x))`
# meurt sur `argument "part" is missing` dès qu'un appel porte un argument
# manquant (`mat[, pass_idx, drop = FALSE]`, présent dans ce corps). ⇒ itérer
# par **INDEX**, et tester la sous-expression sous `tryCatch`.
#
# ⚠️ **C16 / §2bn** : le message de ce site est à **UN SEUL** argument
# (« Package 'RANN' requis. ») ⇒ **aucun `paste0()`** requis. Il est
# entièrement statique ⇒ on assère le message **ENTIER** (§2bx.3).
#
# ⚠️ **Le message DIFFÈRE de celui de `mod_spatial_cluster.R`** — qui dit
# « Package 'RANN' requis (install.packages('RANN')). » : deux fichiers, deux
# messages. C'est pourquoi le test assère la chaîne **exacte** de CE fichier,
# jamais un préfixe commun.
#
# ⚠️ **`write_mirai_log` est BOUCHONNÉ** : il n'est pas le sujet du test et sert
# de **TÉMOIN DE TRAVERSÉE** (§2cj.3) — sans lui, le contrôle de borne serait
# **vacuant** (« notre classe est absente » est vrai aussi si le code échoue
# AVANT la garde).
# =============================================================================

source_project_file("modules/spatial/mod_spatial_pipeline.R")

.MSP_FILE <- "modules/spatial/mod_spatial_pipeline.R"

# --- Extraction AST, robuste aux arguments manquants (cf. en-tête) -----------
.msp_mirai_bodies <- function() {
  p <- parse(file.path(ts_project_root(), .MSP_FILE))
  collect <- function(x, out = list()) {
    if (is.call(x)) {
      is_mirai <- tryCatch(identical(x[[1]], quote(mirai::mirai)),
                           error = function(e) FALSE)
      if (isTRUE(is_mirai)) out[[length(out) + 1L]] <- x[[2]]
      l <- as.list(x)
      for (i in seq_along(l)) {
        ok <- tryCatch(is.call(l[[i]]), error = function(e) FALSE)
        if (isTRUE(ok)) out <- collect(l[[i]], out)
      }
    }
    out
  }
  out <- list()
  for (i in seq_along(p)) out <- collect(p[[i]], out)
  out
}

.msp_body_with <- function(needle) {
  bs  <- .msp_mirai_bodies()
  hit <- Filter(function(b) grepl(needle, paste(deparse(b), collapse = "\n"), fixed = TRUE), bs)
  if (length(hit) != 1L) {
    stop("attendu exactement 1 corps contenant '", needle, "', trouve ", length(hit),
         call. = FALSE)
  }
  hit[[1]]
}

# --- Environnement enfant : mock de `requireNamespace` + bouchon du logger ---
.msp_env <- function(rann_ok = FALSE, log = NULL, extra = list()) {
  e <- new.env(parent = globalenv())
  e$requireNamespace <- function(package, ...) {
    if (identical(package, "RANN")) rann_ok else base::requireNamespace(package, ...)
  }
  e$write_mirai_log <- function(file, message, step = NULL, total = NULL) {
    if (!is.null(log)) log(message)
    invisible(NULL)
  }
  for (nm in names(extra)) assign(nm, extra[[nm]], envir = e)
  e
}

# `suppressWarnings()` : une erreur provoquee peut s'accompagner d'un
# avertissement attendu, que testthat compterait (§2ce).
.msp_err <- function(expr, envir) {
  tryCatch({ suppressWarnings(eval(expr, envir = envir)); NULL },
           error = function(e) list(msg = conditionMessage(e), class = class(e)))
}

.MSP_RANN_MSG <- "Package 'RANN' requis."

# ---------------------------------------------------------------------------
# Site 288 — garde de dependance RANN, 1ʳᵉ instruction du corps du démon
# ---------------------------------------------------------------------------
test_that("mod_spatial_pipeline : RANN absent ⇒ classe (site 288)", {
  b <- .msp_body_with("'RANN'")
  res <- .msp_err(b, .msp_env(rann_ok = FALSE))
  expect_false(is.null(res))                                   # erreur levee…
  expect_true("spatial_pipeline_error" %in% res$class)         # …avec NOTRE classe
  # Message a UN SEUL argument ⇒ aucun `paste0()` requis, et il est entierement
  # statique ⇒ on assere le message ENTIER, jamais un prefixe (§2bx.3).
  expect_identical(res$msg, .MSP_RANN_MSG)
})

# ---------------------------------------------------------------------------
# Contrôle de BORNE (site 288) — la garde est CONDITIONNELLE
# ---------------------------------------------------------------------------
test_that("la garde RANN est conditionnelle : RANN present ⇒ pas notre classe", {
  skip_if_not_installed("RANN")
  expect_true(requireNamespace("RANN", quietly = TRUE))         # precondition
  b <- .msp_body_with("'RANN'")

  log <- character(0)
  # On franchit la garde ; l'echec qui suit vient de `BPCells::open_matrix_dir()`
  # sur un repertoire inexistant ⇒ on n'asserte QUE l'absence de notre classe.
  e <- .msp_env(rann_ok = TRUE,
                log = function(msg) log <<- c(log, msg),
                extra = list(log_file = tempfile(fileext = ".log"),
                             bpcells_dir = tempfile("absent_"),
                             pass_idx = NULL))
  res <- .msp_err(b, e)

  # ⚠️ Contrôle de BORNE NON VACUANT (§2cj.3) : sans temoin, un echec survenu
  # AVANT la garde ferait passer le test a vide. L'etape journalisee juste APRES
  # la garde prouve que le corps l'a bien franchie.
  expect_true(any(grepl("[2/9] Ouverture BPCells", log, fixed = TRUE)))
  if (!is.null(res)) expect_false("spatial_pipeline_error" %in% res$class)
})

# ---------------------------------------------------------------------------
# Verrou source : mod_spatial_pipeline.R ne doit plus contribuer aucun C10
# ---------------------------------------------------------------------------
test_that("mod_spatial_pipeline.R ne contribue aucun signalement C10", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools/check_conventions.R"), envir = e)
  before <- length(e$.REPORT$warns)
  e$check_c10_error_style(file.path(ts_project_root(), .MSP_FILE))
  w <- e$.REPORT$warns
  n <- length(w) - before
  if (n > 0L) {
    lignes <- vapply(w[(before + 1L):length(w)], function(x) x$line, integer(1))
    cat("Sites C10 restants dans mod_spatial_pipeline.R :",
        paste(sort(lignes), collapse = ", "), "\n")
  }
  expect_equal(n, 0L)
})

suppressPackageStartupMessages(library(shiny))

.msp_task_stage <- function(fn) {
  nms <- names(formals(fn))
  if (all(c("bpcells_dir", "k_geom") %in% nms)) return("cluster")
  if ("mode" %in% nms) return("deconv")
  if ("n_niches" %in% nms) return("niche")
  if ("sketch_path" %in% nms) return("umap")
  if ("n_hvg" %in% nms) return("moran")
  if (all(c("k_neighbors", "n_perm") %in% nms)) return("enrichment")
  if ("target_level" %in% nms) return("ripley")
  "unknown"
}

.msp_fixture <- function() {
  state <- new.env(parent = emptyenv())
  state$tasks <- list()
  state$invocations <- list()
  state$arguments <- list()
  state$status <- list()
  state$log <- character(0)
  state$notifications <- list()
  state$hotspot_error <- FALSE
  state$daemons_ready <- TRUE
  state$bpcells_dir <- tempfile("msp-bpcells-")
  dir.create(state$bpcells_dir, recursive = TRUE, showWarnings = FALSE)

  ids <- paste0("c", seq_len(12))
  coords <- data.frame(id = ids, x = seq_len(12), y = seq_len(12) * 2)
  qc <- data.frame(
    id = ids, nCount = 1000, nFeature = 900, pct_mt = 5,
    pct_ribo = 10, log_nCount = log(1000), row.names = NULL
  )
  cluster <- stats::setNames(rep(c("A", "B"), each = 6), ids)
  deconv <- data.frame(id = ids, T1 = 0.6, T2 = 0.4, row.names = NULL)
  niche <- list(
    assignments = data.frame(id = ids, niche = rep(c("N1", "N2"), each = 6),
                             row.names = NULL, stringsAsFactors = FALSE),
    niche_composition = data.frame(niche = c("N1", "N2"), A = c(0.8, 0.2),
                                   B = c(0.2, 0.8), row.names = NULL)
  )
  umap <- data.frame(dim1 = seq_len(12), dim2 = seq_len(12) / 2,
                      id = ids, row.names = NULL)
  moran <- data.frame(gene = paste0("g", seq_len(6)), moran_i = c(NA_real_, 1:5),
                      p_value = c(NA_real_, seq(0.5, 0.01, length.out = 5)),
                      row.names = NULL)
  enrichment <- list(
    enrichment = data.frame(from = c("A", "B"), to = c("B", "A"),
                            observed = c(1, 1), z_score = c(2, 2),
                            row.names = NULL),
    matrix = diag(2), levels = c("A", "B"), k_neighbors = 30, n_perm = 200
  )
  hotspot <- data.frame(
    id = ids, value = seq_len(12), gi_star = seq_len(12) / 2,
    p_value = rep(0.01, 12), hotspot = rep("Hot", 12), row.names = NULL
  )
  ripley <- list(
    curve = data.frame(r = c(1, 2), k_observed = c(1, 2),
                        k_perm_mean = c(0.5, 1), k_perm_lo = c(0.2, 0.6),
                        k_perm_hi = c(0.8, 1.4), signif = c("NS", "Aggregation"),
                        row.names = NULL),
    target_level = "A", n_target = 6, n_total = 12, n_perm = 199,
    subsampled = FALSE
  )
  state$values <- list(cluster = cluster, deconv = deconv, niche = niche,
                       umap = umap, moran = moran, enrichment = enrichment,
                       hotspot = hotspot, ripley = ripley)

  task_generator <- list(new = function(fn) {
    stage <- .msp_task_stage(fn)
    task <- new.env(parent = emptyenv())
    task$status_value <- shiny::reactiveVal("initial")
    task$result_value <- shiny::reactiveVal(NULL)
    task$invoke <- function(...) {
      state$invocations[[stage]] <- (state$invocations[[stage]] %||% 0L) + 1L
      state$arguments[[stage]] <- list(...)
      state$status[[stage]] <- "running"
      task$status_value("running")
      invisible(task)
    }
    task$status <- function() task$status_value()
    task$result <- function() task$result_value()
    task$resolve <- function(value, status) {
      task$result_value(value)
      state$status[[stage]] <- status
      task$status_value(status)
      invisible(task)
    }
    state$tasks[[stage]] <- task
    task
  })

  mod_env <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "config", "defaults.R"), envir = mod_env)
  sys.source(file.path(ts_project_root(), "R", "core", "drive_allowlist.R"), envir = mod_env)
  sys.source(file.path(ts_project_root(), "R", "core", "drive_watcher.R"), envir = mod_env)
  sys.source(file.path(ts_project_root(), .MSP_FILE), envir = mod_env)
  mod_env$ts_drive_job_clear()
  mod_env$ExtendedTask <- task_generator
  mod_env$LABEL_TRANSFER_MIN_SHARED_GENES <- 50L
  mod_env$spatial_daemons_ready <- function() isTRUE(state$daemons_ready)
  mod_env$spatial_log_path <- function(...) tempfile(fileext = ".log")
  mod_env$create_reactive_tracker <- function(...) shiny::reactiveVal(character(0))
  mod_env$reset_log <- function(...) state$log <- character(0)
  mod_env$write_mirai_log <- function(file, message, step = NULL, total = NULL) {
    state$log <- c(state$log, as.character(message))
    invisible(NULL)
  }
  mod_env$showNotification <- function(message, type = "message", ...) {
    state$notifications[[length(state$notifications) + 1L]] <-
      list(message = as.character(message), type = as.character(type))
    invisible(NULL)
  }
  mod_env$compute_qc_metrics_fast <- function(...) qc
  mod_env$compute_getis_ord_hotspots <- function(...) {
    if (isTRUE(state$hotspot_error)) stop("hotspot failure", call. = FALSE)
    hotspot
  }
  mod_env$saveRDS <- function(object, file, ...) file

  list(
    env = mod_env, state = state, coords = coords, qc = qc,
    bpcells_dir = state$bpcells_dir,
    cluster = cluster, deconv = deconv, niche = niche, umap = umap,
    moran = moran, enrichment = enrichment, hotspot = hotspot, ripley = ripley
  )
}

.msp_shared_state <- function() {
  shiny::reactiveValues(
    qc_metrics = NULL, qc_pass_idx = NULL, qc_params = NULL,
    moran_results = NULL, moran_params = NULL,
    cluster_labels = NULL, cluster_params = NULL,
    deconv_props = NULL, deconv_params = NULL,
    niche_labels = NULL, niche_composition = NULL, niche_params = NULL,
    umap_df = NULL, enrichment_result = NULL, enrichment_params = NULL,
    hotspot_result = NULL, hotspot_params = NULL,
    ripley_result = NULL, ripley_params = NULL
  )
}

.msp_gd <- function(f, drive_registry = FALSE) {
  args <- list(
    spatial_obj = list(bpcells_dir = f$bpcells_dir, coords = f$coords,
                       sketch = list()),
    spatial_reference = NULL, i18n = NULL, language = "fr"
  )
  if (isTRUE(drive_registry)) args$drive_registry <- new.env(parent = emptyenv())
  do.call(shiny::reactiveValues, args)
}

.msp_set_inputs <- function(session, deconv_mode = "none", compute_umap = FALSE,
                            compute_moran = FALSE, compute_enrichment = FALSE,
                            compute_hotspots = FALSE, compute_ripley = FALSE) {
  session$setInputs(
    qc_min_count = 0, qc_min_features = 0, qc_max_pct_mt = 100,
    lambda = 0.8, resolution = 0.8, deconv_mode = deconv_mode,
    lt_norm_method = "lognorm", lt_ncells = 3000, lt_npcs = 30,
    n_topics = 6, n_top_od = 1000, n_niches = 2,
    compute_umap = compute_umap, compute_moran = compute_moran,
    n_hvg_moran = 1000, compute_enrichment = compute_enrichment,
    k_neighbors_enrich = 30, n_perm_enrich = 200,
    compute_hotspots = compute_hotspots, hotspot_metric = "log_nCount",
    k_neighbors_hotspot = 30, compute_ripley = compute_ripley,
    n_perm_ripley = 199
  )
}

.msp_status_text <- function(output) {
  paste(as.character(output$pipeline_status_ui), collapse = " ")
}

test_that("mod_spatial_pipeline : un hotspot silencieux termine en erreur, pas en succes", {
  f <- .msp_fixture()
  f$state$hotspot_error <- TRUE
  gd <- shiny::reactiveValues(
    spatial_obj = list(bpcells_dir = "virtual://bpcells", coords = f$coords,
                       sketch = list()),
    spatial_reference = NULL, i18n = NULL, language = "fr"
  )
  rv <- .msp_shared_state()

  testServer(f$env$mod_spatial_pipeline_server,
             args = list(id = "pipeline", global_data = gd, shared_rv = rv), {
    .msp_set_inputs(session, compute_hotspots = TRUE)
    session$setInputs(btn_run_all = 1)
    session$flushReact()
    f$state$tasks$cluster$resolve(f$cluster, "success")
    session$flushReact()
    f$state$tasks$niche$resolve(f$niche, "success")
    session$flushReact()

    expect_match(.msp_status_text(output), "Erreur")
    expect_identical(f$state$invocations$cluster, 1L)
    expect_identical(f$state$invocations$niche, 1L)
    expect_null(shiny::isolate(rv$hotspot_result))
    expect_true(any(vapply(f$state$notifications,
                           function(x) identical(x$type, "error"), logical(1))))
    expect_false(any(vapply(f$state$notifications,
                            function(x) identical(x$type, "message"), logical(1))))
  })
})

test_that("mod_spatial_pipeline : une erreur de deconvolution selectionnee n'aboutit jamais a done", {
  f <- .msp_fixture()
  gd <- shiny::reactiveValues(
    spatial_obj = list(bpcells_dir = "virtual://bpcells", coords = f$coords,
                       sketch = list()),
    spatial_reference = NULL, i18n = NULL, language = "fr"
  )
  rv <- .msp_shared_state()

  testServer(f$env$mod_spatial_pipeline_server,
             args = list(id = "pipeline", global_data = gd, shared_rv = rv), {
    .msp_set_inputs(session, deconv_mode = "stdeconvolve")
    session$setInputs(btn_run_all = 1)
    session$flushReact()
    f$state$tasks$cluster$resolve(f$cluster, "success")
    session$flushReact()

    expect_identical(f$state$invocations$deconv, 1L)
    f$state$tasks$deconv$resolve(simpleError("deconv failure"), "error")
    session$flushReact()

    expect_match(.msp_status_text(output), "Erreur")
    expect_null(f$state$invocations$niche)
    expect_null(shiny::isolate(rv$deconv_props))
    expect_false(any(vapply(f$state$notifications,
                            function(x) identical(x$type, "message"), logical(1))))
  })
})

test_that("mod_spatial_pipeline : un resultat de clustering invalide arrete avant les niches", {
  f <- .msp_fixture()
  gd <- shiny::reactiveValues(
    spatial_obj = list(bpcells_dir = "virtual://bpcells", coords = f$coords,
                       sketch = list()),
    spatial_reference = NULL, i18n = NULL, language = "fr"
  )
  rv <- .msp_shared_state()

  testServer(f$env$mod_spatial_pipeline_server,
             args = list(id = "pipeline", global_data = gd, shared_rv = rv), {
    .msp_set_inputs(session)
    session$setInputs(btn_run_all = 1)
    session$flushReact()
    f$state$tasks$cluster$resolve(NULL, "success")
    session$flushReact()

    expect_match(.msp_status_text(output), "Erreur")
    expect_null(f$state$invocations$niche)
    expect_null(shiny::isolate(rv$cluster_labels))
  })
})

test_that("mod_spatial_pipeline : un resultat de niches partiel ne deroule jamais la suite", {
  f <- .msp_fixture()
  gd <- shiny::reactiveValues(
    spatial_obj = list(bpcells_dir = "virtual://bpcells", coords = f$coords,
                       sketch = list()),
    spatial_reference = NULL, i18n = NULL, language = "fr"
  )
  rv <- .msp_shared_state()

  testServer(f$env$mod_spatial_pipeline_server,
             args = list(id = "pipeline", global_data = gd, shared_rv = rv), {
    .msp_set_inputs(session, compute_umap = TRUE)
    session$setInputs(btn_run_all = 1)
    session$flushReact()
    f$state$tasks$cluster$resolve(f$cluster, "success")
    session$flushReact()
    f$state$tasks$niche$resolve(
      list(assignments = data.frame(id = f$coords$id),
           niche_composition = data.frame(niche = "N1")),
      "success"
    )
    session$flushReact()

    expect_match(.msp_status_text(output), "Erreur")
    expect_null(f$state$invocations$umap)
    expect_null(shiny::isolate(rv$niche_labels))
    expect_null(shiny::isolate(rv$niche_composition))
  })
})

test_that("mod_spatial_pipeline : seule la resolution terminale enchaine et termine le pipeline", {
  f <- .msp_fixture()
  gd <- shiny::reactiveValues(
    spatial_obj = list(bpcells_dir = "virtual://bpcells", coords = f$coords,
                       sketch = list()),
    spatial_reference = NULL, i18n = NULL, language = "fr"
  )
  rv <- .msp_shared_state()

  testServer(f$env$mod_spatial_pipeline_server,
             args = list(id = "pipeline", global_data = gd, shared_rv = rv), {
    .msp_set_inputs(session, compute_umap = TRUE, compute_moran = TRUE,
                    compute_enrichment = TRUE, compute_ripley = TRUE)
    session$setInputs(btn_run_all = 1)
    session$flushReact()
    f$state$tasks$cluster$resolve(f$cluster, "success")
    session$flushReact()
    f$state$tasks$niche$resolve(f$niche, "success")
    session$flushReact()
    f$state$tasks$umap$resolve(f$umap, "success")
    session$flushReact()

    expect_identical(f$state$invocations$moran, 1L)
    expect_null(f$state$invocations$enrichment)
    expect_match(.msp_status_text(output), "cours")

    f$state$tasks$moran$resolve(f$moran, "success")
    session$flushReact()
    expect_identical(f$state$invocations$enrichment, 1L)
    expect_null(f$state$invocations$ripley)

    f$state$tasks$enrichment$resolve(f$enrichment, "success")
    session$flushReact()
    expect_identical(f$state$invocations$ripley, 1L)
    expect_match(.msp_status_text(output), "cours")

    f$state$tasks$ripley$resolve(f$ripley, "success")
    session$flushReact()
    expect_match(.msp_status_text(output), "termin")
    expect_identical(f$state$invocations$cluster, 1L)
    expect_identical(f$state$invocations$niche, 1L)
    expect_identical(f$state$invocations$umap, 1L)
    expect_identical(f$state$invocations$moran, 1L)
    expect_identical(f$state$invocations$enrichment, 1L)
    expect_identical(f$state$invocations$ripley, 1L)
    expect_true(any(vapply(f$state$notifications,
                           function(x) identical(x$type, "message"), logical(1))))
  })
})

test_that("mod_spatial_pipeline publie un jeton long, pret et strictement scalar", {
  f <- .msp_fixture()
  gd <- .msp_gd(f, drive_registry = TRUE)
  rv <- .msp_shared_state()
  button <- "spatial-pipeline-btn_run_all"

  testServer(f$env$mod_spatial_pipeline_server,
             args = list(id = "pipeline", global_data = gd, shared_rv = rv), {
    .msp_set_inputs(session)
    registry <- shiny::isolate(gd$drive_registry)
    expect_setequal(ls(registry), button)
    entry <- registry[[button]]
    expect_true(is.function(entry$counter))
    expect_true(is.function(entry$ready))
    expect_true(is.function(entry$state))
    expect_true(isTRUE(entry$long))
    expect_identical(entry$timeout_s, f$env$TS_SPATIAL_PIPELINE_TIMEOUT_S)
    task_budget_s <- (6 * f$env$TS_MIRAI_TIMEOUT_MS +
                      max(f$env$TS_RCTD_TIMEOUT_MS,
                          f$env$TS_LABEL_TRANSFER_TIMEOUT)) / 1000
    expect_gte(entry$timeout_s, task_budget_s + 60)
    expect_true(entry$ready())

    state <- entry$state()
    expect_setequal(names(state), c(
      "module", "action", "status", "elapsed_s", "seq",
      "n_results", "has_data", "ready"
    ))
    expect_true(all(vapply(state, function(x) length(x) <= 1L, logical(1))))
    expect_identical(state$module, "spatial_pipeline")
    expect_identical(state$action, "run_pipeline")
    expect_identical(state$status, "idle")
    expect_identical(state$n_results, 0L)
    expect_true(state$has_data)
    expect_true(state$ready)
    expect_identical(state$seq, 0L)
    expect_identical(state$elapsed_s, 0)

    f$state$daemons_ready <- FALSE
    expect_match(entry$ready(), "daemon")
    expect_identical(entry$state()$status, "not_ready")
    f$state$daemons_ready <- TRUE
    shiny::isolate(gd$spatial_obj <- NULL)
    expect_match(entry$ready(), "spatial_obj")
  })
})

test_that("mod_spatial_pipeline : le jeton drive reste running jusqu'a la resolution terminale", {
  f <- .msp_fixture()
  gd <- .msp_gd(f, drive_registry = TRUE)
  rv <- .msp_shared_state()
  button <- "spatial-pipeline-btn_run_all"

  testServer(f$env$mod_spatial_pipeline_server,
             args = list(id = "pipeline", global_data = gd, shared_rv = rv), {
    .msp_set_inputs(session)
    entry <- shiny::isolate(gd$drive_registry)[[button]]
    job <- f$env$ts_drive_job_begin(
      201L, "spatial_pipeline", "run_pipeline", button,
      timeout_s = entry$timeout_s
    )
    expect_identical(job$status, "running")
    expect_true(f$env$ts_drive_bump_token(entry))
    session$flushReact()

    expect_identical(f$state$invocations$cluster, 1L)
    expect_identical(f$env$ts_drive_job_state()$status, "running")
    expect_null(f$env$ts_drive_job_pending())
    expect_match(.msp_status_text(output), "cours")

    f$state$tasks$cluster$resolve(f$cluster, "success")
    session$flushReact()
    expect_identical(f$state$invocations$niche, 1L)
    expect_identical(f$env$ts_drive_job_state()$status, "running")

    f$state$tasks$niche$resolve(f$niche, "success")
    session$flushReact()
    expect_match(.msp_status_text(output), "termin")
    expect_identical(f$env$ts_drive_job_state()$status, "done")
    expect_identical(f$env$ts_drive_job_pending()$status, "done")
    expect_identical(entry$state()$status, "done")
    expect_identical(entry$state()$seq, 201L)
    expect_gte(entry$state()$n_results, 5L)
  })
})

test_that("mod_spatial_pipeline : un echec drive termine le job avec error", {
  f <- .msp_fixture()
  gd <- .msp_gd(f, drive_registry = TRUE)
  rv <- .msp_shared_state()
  button <- "spatial-pipeline-btn_run_all"

  testServer(f$env$mod_spatial_pipeline_server,
             args = list(id = "pipeline", global_data = gd, shared_rv = rv), {
    .msp_set_inputs(session)
    entry <- shiny::isolate(gd$drive_registry)[[button]]
    f$env$ts_drive_job_begin(
      202L, "spatial_pipeline", "run_pipeline", button,
      timeout_s = entry$timeout_s
    )
    expect_true(f$env$ts_drive_bump_token(entry))
    session$flushReact()
    f$state$tasks$cluster$resolve(NULL, "success")
    session$flushReact()

    expect_match(.msp_status_text(output), "Erreur")
    expect_null(f$state$invocations$niche)
    expect_identical(f$env$ts_drive_job_state()$status, "error")
    expect_identical(f$env$ts_drive_job_pending()$status, "error")
    expect_true(nzchar(f$env$ts_drive_job_pending()$error))
    expect_false(grepl("SENTINEL", f$env$ts_drive_job_pending()$error, fixed = TRUE))
  })
})

test_that("mod_spatial_pipeline : un clic humain ne cree ni ne ferme un job drive", {
  f <- .msp_fixture()
  gd <- .msp_gd(f, drive_registry = TRUE)
  rv <- .msp_shared_state()

  testServer(f$env$mod_spatial_pipeline_server,
             args = list(id = "pipeline", global_data = gd, shared_rv = rv), {
    .msp_set_inputs(session)
    session$setInputs(btn_run_all = 1)
    session$flushReact()
    f$state$tasks$cluster$resolve(f$cluster, "success")
    session$flushReact()
    f$state$tasks$niche$resolve(f$niche, "success")
    session$flushReact()

    expect_match(.msp_status_text(output), "termin")
    expect_false(f$env$ts_drive_job_busy())
    expect_null(f$env$ts_drive_job_pending())
  })
})

test_that("mod_spatial_pipeline : les predicats de resultat rejettent les objets invalides", {
  f <- .msp_fixture()
  expect_true(f$env$.spatial_pipeline_result_ok(f$cluster, "cluster"))
  expect_true(f$env$.spatial_pipeline_result_ok(f$deconv, "deconv"))
  expect_true(f$env$.spatial_pipeline_result_ok(f$niche, "niche"))
  expect_true(f$env$.spatial_pipeline_result_ok(f$umap, "umap"))
  expect_true(f$env$.spatial_pipeline_result_ok(f$moran, "moran"))
  expect_true(f$env$.spatial_pipeline_result_ok(f$enrichment, "enrichment"))
  expect_true(f$env$.spatial_pipeline_result_ok(f$hotspot, "hotspot"))
  expect_true(f$env$.spatial_pipeline_result_ok(f$ripley, "ripley"))

  bad <- list(NULL, list(), data.frame(), "not-a-result")
  for (x in bad) {
    for (kind in c("cluster", "deconv", "niche", "umap", "moran",
                   "enrichment", "hotspot", "ripley")) {
      expect_false(f$env$.spatial_pipeline_result_ok(x, kind))
    }
  }
  expect_false(f$env$.spatial_pipeline_result_ok(
    stats::setNames(rep("A", 12), paste0("c", seq_len(12))), "cluster"))
  expect_false(f$env$.spatial_pipeline_result_ok(
    list(assignments = f$niche$assignments), "niche"))
})

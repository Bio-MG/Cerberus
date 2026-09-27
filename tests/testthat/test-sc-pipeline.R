# Tests for R/sc/sc_pipeline.R
source_project_file("R/sc/sc_helpers.R")
source_project_file("R/sc/sc_pipeline.R")

test_that("resolve_sketch_preset returns valid structure", {
  for (preset in c("fast","light","medium","standard","high","max")) {
    result <- resolve_sketch_preset(preset, n_total_cells = 200000)
    expect_true(all(c("ncells","max_per_cluster","npcs") %in% names(result)))
    expect_true(result$ncells > 0)
    expect_true(result$npcs > 0)
  }
})

test_that("resolve_sketch_preset caps at n_total_cells", {
  result <- resolve_sketch_preset("high", n_total_cells = 5000)
  expect_equal(result$ncells, 5000)  # capped below 100000
})

# ---------------------------------------------------------------------------
# 25ᵉ incrément — REWRITE du verrou (roadmap 5.2, audit 2026-09-27 §1.9).
# Historiquement ce verrou INTERDISAIT le `stop(` dans le gestionnaire final :
# la classe `sc_pipeline_error` était inobservable (§2cd.2). Le contrat est
# INVERSÉ : le gestionnaire journalise ET notifie, puis RELANCE — l'erreur
# devient observable par les appelants (.sc_ap_run_drive marque le job en
# échec ; les observateurs UI enveloppent dans tryCatch). Le verrou source
# ci-dessous gèle la NOUVELLE propriété : journalisation + notification +
# relance. Les assertions de classe complètes restent dans
# test-sc-auto-pipeline.R (exécution e2e).
# ---------------------------------------------------------------------------

test_that("sc_pipeline.R ne contribue aucun signalement C10", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools/check_conventions.R"), envir = e)
  before <- length(e$.REPORT$warns)
  e$check_c10_error_style(file.path(ts_project_root(), "R/sc/sc_pipeline.R"))
  w <- e$.REPORT$warns
  n <- length(w) - before
  if (n > 0L) {
    lignes <- vapply(w[(before + 1L):length(w)], function(x) x$line, integer(1))
    cat("Sites C10 restants dans sc_pipeline.R :",
        paste(sort(lignes), collapse = ", "), "\n")
  }
  expect_equal(n, 0L)
})

test_that("run_sc_auto_pipeline journalise, notifie PUIS relance (erreur observable)", {
  src <- readLines(file.path(ts_project_root(), "R/sc/sc_pipeline.R"), warn = FALSE)
  h <- grep("error *= *function", src)
  expect_true(length(h) > 0L)
  # Le DERNIER gestionnaire du fichier est celui du tryCatch englobant (30->408).
  tail_src <- paste(src[seq_along(src) > max(h)], collapse = "\n")
  # Il journalise ET notifie…
  expect_true(grepl("log_sc\\(", tail_src))
  expect_true(grepl("showNotification\\(", tail_src))
  # …puis RELANCE : la classe sc_pipeline_error est observable (roadmap 5.2).
  expect_true(grepl("stop\\(e\\)", tail_src))
})

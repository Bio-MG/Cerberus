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
# 24ᵉ incrément de la dette de conventions (2026-09-18, §2ci).
# ⚠️ LOT « VERROU SOURCE SEUL » — la classe `sc_pipeline_error` est
# INOBSERVABLE ici, pour DEUX raisons MESURÉES (sonde, §2ci.1) :
#   1. `run_sc_auto_pipeline()` n'est PAS pilotable hors session Shiny :
#      ligne 19 `removeModal()` ⇒ « attempt to apply non-function » et ligne 28
#      `shiny::Progress$new()` ⇒ « Can only use Progress$new() inside a Shiny
#      app » (les deux MESURÉS, pas supposés).
#   2. Même atteint, le site serait AVALÉ : le `tryCatch` ouvert ligne 30 se
#      ferme ligne 408 sur un gestionnaire qui **retourne** une valeur
#      (`log_sc()` + `showNotification()`) au lieu de `stop()` (§2cd.2).
# ⇒ Le seul témoin possible serait le TEXTE du message via `sc_log_rv` — hors
#   de portée sans session. La preuve se limite donc au **VERROU SOURCE**,
#   comme pour les lots de `modules/` (§2bs).
# ⚠️ La justification est rendue EXÉCUTABLE ci-dessous : une justification non
#   vérifiée pourrit (§2bp, §2cb.2). Si le second test se met à ÉCHOUER, c'est
#   que le gestionnaire s'est mis à relancer ⇒ le lot devient PROUVABLE à
#   l'exécution et ce test doit être remplacé par des assertions de classe.
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

test_that("run_sc_auto_pipeline AVALE son erreur (justification du verrou source)", {
  src <- readLines(file.path(ts_project_root(), "R/sc/sc_pipeline.R"), warn = FALSE)
  h <- grep("error *= *function", src)
  expect_true(length(h) > 0L)
  # Le DERNIER gestionnaire du fichier est celui du tryCatch englobant (30->408).
  tail_src <- paste(src[seq_along(src) > max(h)], collapse = "\n")
  # Il journalise ET notifie…
  expect_true(grepl("log_sc\\(", tail_src))
  expect_true(grepl("showNotification\\(", tail_src))
  # …mais il NE RELANCE PAS ⇒ la classe est inobservable (§2cd.2).
  expect_false(grepl("stop\\(", tail_src))
})

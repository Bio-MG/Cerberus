# =============================================================================
# test-tour-registry.R — N4 biomentat-tour (satellites slice S6) : registre PUR
# =============================================================================
# FICHE (fait foi) :
# docs/proposals/PROPOSAL_N_SATELLITES_LAUNCH_ROADMAP_2026-10-26.md
#   §slice S6 (N4 biomentat-tour) : « Steps anchored on the FROZEN allowlist DOM
#   ids (drive_allowlist.R) so tour steps = drive-checkable anchors » ;
#   « RED-first on the step-registry pure helper » ; « 0 new dep (hand-rolled
#   bslib, §3-D2 default) » ; « each step's anchor testable by the drive
#   channel (the tour IS an acceptance suite) ».
# Mission round8 slice S6 : registre PUR ancres ⊆ vocabulaire GELÉ — le verrou
#   est la propriété « chaque step = une ancre drive-checkable » : l'ancre d'un
#   step bouton DOIT être un token de TS_DRIVE_BUTTONS, le tab d'un step DOIT
#   être une valeur nav_top gelée (constantes TS_DRIVE_*_TOP_TAB). Un id DOM
#   inventé par la visite serait un id que le canal drive ne peut jamais
#   vérifier — exactement ce que la fiche interdit.
# Rails RED (règle error=0) : au stade RED les corps sont des STUBS (NULL) —
#   chaque expectation échoue comme ÉCHEC, jamais comme erreur ; les sections
#   qui itèrent le registre sont skip_if_not(is.list(...)) pour qu'un stub
#   NULL ne transforme pas le rouge en crash.
# C2 : ce fichier testé est PUR (aucun appel Shiny) — comme drive_allowlist.R.
# =============================================================================

source_project_file("tools/pkg_skeleton/R/drive_allowlist.R") # vocabulaire GELÉ (données)
source_project_file("tools/pkg_skeleton/R/tour_registry.R")

# ── (a) Le registre existe, non vide, shape verbatim -------------------------

test_that("(a) ts_tour_steps() rend un registre non vide au shape figé", {
  steps <- ts_tour_steps()
  expect_true(is.list(steps),
              info = "le registre n'existe pas (stub NULL) — FAIL attendu au stade RED")
  skip_if_not(is.list(steps))
  expect_true(length(steps) >= 4L,
              info = "une visite de 0/1 step n'est pas une visite")

  for (i in seq_along(steps)) {
    s <- steps[[i]]
    expect_true(is.list(s), info = paste("step", i, "n'est pas une liste"))
    expect_setequal(names(s), c("id", "anchor", "tab", "kind", "title", "text"))
    expect_true(is.character(s$id) && length(s$id) == 1L && nzchar(s$id),
                info = paste("step", i, ": id invalide"))
    expect_true(is.character(s$anchor) && length(s$anchor) == 1L && nzchar(s$anchor),
                info = paste("step", i, ": ancre invalide"))
    expect_true(is.character(s$title) && length(s$title) == 1L && nzchar(s$title),
                info = paste("step", i, ": titre (clé i18n) invalide"))
    expect_true(is.character(s$text) && length(s$text) == 1L && nzchar(s$text),
                info = paste("step", i, ": texte (clé i18n) invalide"))
    # tab : soit NULL (surface d'atterrissage — aucun nav requis), soit UNE
    # valeur nav_top ; jamais NA/""/vecteur.
    expect_true(is.null(s$tab) ||
                  (is.character(s$tab) && length(s$tab) == 1L && nzchar(s$tab)),
                info = paste("step", i, ": tab ni NULL ni valeur nav_top"))
    expect_true(s$kind %in% c("button", "nav_top"),
                info = paste("step", i, ": kind hors vocabulaire"))
  }

  ids <- vapply(steps, function(s) s$id, character(1))
  expect_false(any(duplicated(ids)), info = "ids de steps dupliqués")
  expect_true(all(nzchar(ids)))
})

# ── (b) LE VERROU : chaque step = une ancre drive-checkable ------------------

test_that("(b) ancres ⊆ vocabulaire GELÉ (boutons TS_DRIVE_BUTTONS + tabs nav_top)", {
  steps <- ts_tour_steps()
  vocab <- ts_tour_anchor_vocabulary()

  # Le vocabulaire lui-même est GELÉ : exactement l'union des constantes
  # drive_allowlist.R — aucune entrée locale à la visite (un id inventé ici
  # serait un id que le drive ne peut jamais vérifier). Verrou AVANT le
  # skip-stub : au stade RED (vocab NULL) c'est un FAIL, jamais un crash
  # (expect_setequal(NULL) est une ERREUR en testthat 3e — garde FAIL d'abord).
  expect_true(is.character(vocab),
              info = "le vocabulaire n'existe pas (stub NULL) — FAIL attendu au stade RED")
  skip_if_not(is.character(vocab))
  expect_setequal(
    vocab,
    c(TS_DRIVE_BUTTONS, TS_DRIVE_SC_TOP_TAB,
      TS_DRIVE_BULK_SIGNATURES_TOP_TAB, TS_DRIVE_SPATIAL_TOP_TAB)
  )
  expect_false(any(duplicated(vocab)))

  skip_if_not(is.list(steps))

  tabs_gel <- c(TS_DRIVE_SC_TOP_TAB, TS_DRIVE_BULK_SIGNATURES_TOP_TAB,
                TS_DRIVE_SPATIAL_TOP_TAB)

  for (i in seq_along(steps)) {
    s <- steps[[i]]
    expect_true(s$anchor %in% vocab,
                info = sprintf("step %s : ancre '%s' HORS vocabulaire gelé — la visite promettrait un id que le drive ne peut pas vérifier", s$id, s$anchor))
    expect_true(s$kind == "button" && s$anchor %in% TS_DRIVE_BUTTONS ||
                  s$kind == "nav_top" && s$anchor %in% tabs_gel,
                info = sprintf("step %s : kind '%s' incohérent avec l'ancre '%s'", s$id, s$kind, s$anchor))
    expect_true(is.null(s$tab) || s$tab %in% tabs_gel,
                info = sprintf("step %s : tab '%s' n'est pas une valeur nav_top gelée", s$id, s$tab))
  }
})

# ── (c) Couverture : les trois domaines d'analyse sont visités ---------------

test_that("(c) la visite couvre import + Bulk + Single-Cell + Spatial", {
  steps <- ts_tour_steps()
  skip_if_not(is.list(steps))
  anchors <- vapply(steps, function(s) s$anchor, character(1))
  # Import (bouton Charger de l'importeur bulk — ancre G0 historique),
  # chaîne Bulk (filtrage + DE + pathway), SC (annotation), Spatial (pipeline).
  expect_true("import_bulk-btn_load" %in% anchors,
              info = "l'import bulk n'est pas visité")
  expect_true("bulk-filter-run_filter_norm" %in% anchors,
              info = "le Step 1 Bulk (Filtrage & VST) n'est pas visité")
  expect_true("bulk-de-run_de" %in% anchors, info = "la DE Bulk n'est pas visitée")
  expect_true("bulk-pathways-run_pathway" %in% anchors,
              info = "les pathways Bulk ne sont pas visités")
  expect_true("sc-annotation-run_annot" %in% anchors,
              info = "l'annotation SC n'est pas visitée")
  expect_true("spatial-pipeline-btn_run_all" %in% anchors,
              info = "le pipeline Spatial n'est pas visité")

  tabs <- unique(unlist(Filter(Negate(is.null), lapply(steps, function(s) s$tab))))
  expect_setequal(tabs, c(TS_DRIVE_SC_TOP_TAB, TS_DRIVE_BULK_SIGNATURES_TOP_TAB,
                          TS_DRIVE_SPATIAL_TOP_TAB))
})

# ── (d) Les clés i18n des steps existent (fr ET en non vides) ----------------

test_that("(d) chaque titre/texte de step est une clé i18n présente (fr + en)", {
  steps <- ts_tour_steps()
  skip_if_not(is.list(steps))
  path <- file.path(ts_project_root(), "i18n", "translation.json")
  expect_true(file.exists(path))
  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  fr <- vapply(j$translation, function(x) x$fr %||% "", character(1))
  en <- vapply(j$translation, function(x) x$en %||% "", character(1))

  for (i in seq_along(steps)) {
    s <- steps[[i]]
    for (k in c(title = s$title, text = s$text)) {
      expect_true(k %in% fr,
                  info = sprintf("step %s : clé i18n absente de translation.json : %s", s$id, k))
      expect_true(nzchar(en[match(k, fr)]),
                  info = sprintf("step %s : traduction EN manquante pour : %s", s$id, k))
    }
  }
})

# ── (e) Le validateur : TRUE sur le registre réel, FALSE sur corruptions ----

test_that("(e) ts_tour_steps_valid() — TRUE sur le réel, FALSE sur 5 corruptions", {
  steps <- ts_tour_steps()
  expect_true(isTRUE(ts_tour_steps_valid(steps)),
              info = "le registre réel doit valider (FAIL attendu au stade RED : stub NULL)")
  skip_if_not(is.list(steps))

  # Corruption 1 : ancre inventée (hors vocabulaire gelé).
  bad <- steps; bad[[1]] <- within(bad[[1]], anchor <- "tour-btn_invente")
  expect_false(isTRUE(ts_tour_steps_valid(bad)),
               info = "une ancre hors allowlist doit invalider le registre")

  # Corruption 2 : id dupliqué.
  bad <- steps; bad[[2]] <- within(bad[[2]], id <- bad[[1]]$id)
  expect_false(isTRUE(ts_tour_steps_valid(bad)))

  # Corruption 3 : texte vide.
  bad <- steps; bad[[1]] <- within(bad[[1]], text <- "")
  expect_false(isTRUE(ts_tour_steps_valid(bad)))

  # Corruption 4 : kind incohérent avec l'ancre.
  bad <- steps; bad[[1]] <- within(bad[[1]], kind <- "nav_top")
  expect_false(isTRUE(ts_tour_steps_valid(bad)))

  # Corruption 5 : tab hors valeurs nav_top gelées.
  bad <- steps; bad[[2]] <- within(bad[[2]], tab <- "tab_import_invente")
  expect_false(isTRUE(ts_tour_steps_valid(bad)))

  # Le validateur n'explose JAMAIS sur une entrée malformée (pure, pas stop).
  expect_false(isTRUE(ts_tour_steps_valid(list())))
  expect_false(isTRUE(ts_tour_steps_valid(NULL)))
})

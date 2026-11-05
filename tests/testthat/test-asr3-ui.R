# =============================================================================
# test-asr3-ui.R -- ASR-3 : UI des checkpoints de session (O29) — RED-first
# =============================================================================
# POURQUOI CE FICHIER
#
# Jalon ASR-3 (fiche SESSION_CHECKPOINT_RESTORE_PROPOSAL.md §6 :118-120 ;
# contrat SESSION_CHECKPOINT_RESTORE_CONTRACT.md §4 toggle opt-in, §5.1
# bannière non intrusive, §8 row ASR-3) : toggle off par défaut, bannière
# « Reprendre la session du <horodatage> (n jeux de données) » qui REND le
# payload calculé par ASR-2 (ts_session_checkpoint_restore_plan()$banner —
# ASR-3 rend, ne recalcule jamais), ligne d'état checkpoint, clés i18n fr/en,
# 0 CSS www/. OFF = comportement actuel byte pour byte (aucune écriture,
# aucune bannière, AUCUN appel moteur depuis le chemin UI ; contrat §7.5).
#
# QU'EST ASSERTABLE HEADLESS (honnêteté du garde, cf.
# test-sres1-allow-reconnect.R) :
#   * Le RENDU NAVIGATEUR RÉEL de la bannière et du toggle n'est PAS
#     assertable (contrat §8 row ASR-3 « Partiel » et §9) — testServer couvre
#     la logique serveur et le HTML des outputs, pas un navigateur.
#   * app.R n'est jamais sourcé par la suite (convention test-app-sourcing.R ;
#     test-sres1-allow-reconnect.R:29-35 — le boot complet de global.R :
#     plan future, poller drive — n'est pas un harnais de test). La cible
#     testServer est donc le MODULE ASR-3 que app.R monte : le garde lexical
#     « wiring » (§7 ci-dessous) épingle le montage réel dans app.R.
#   * « Byte pour byte » hors navigateur est prouvé AVEC CE QUI EST
#     OBSERVABLE : toggle par défaut FALSE ; ZÉRO appel moteur tant que le
#     toggle n'est pas activé (compteur d'appels autour du moteur RÉEL) ;
#     bannière et ligne d'état rendent NULL. La comparaison octet du DOM
#     complet exige un navigateur — non affirmé ici.
#
# OPEN-POINT DECISIONS (placements fiche-silencieux — tranchés
# conservativement par ASR-3, chacun documenté) :
#   1. Surface testable : le jalon UI vit dans le module dédié
#      modules/session/mod_session_checkpoint.R, monté par app.R (source +
#      sidebar + header + server). app.R seul n'est pas testServer-able sans
#      violer la convention « jamais sourcé » ; le module est la surface
#      testable, app.R n'en est que le montage (garde lexical §7).
#   2. Placement du toggle : panneau sidebar « Checkpoints de session »,
#      dans la section Session (voisin de « Sauvegarder/Charger Session » —
#      la persistance de session), hr + h6 + bslib::input_switch (primitive
#      bslib, 0 CSS). La présence du toggle lui-même est la seule différence
#      UI du mode OFF — un switch invisible ne pourrait jamais être activé ;
#      la byte-identité OFF porte sur le chemin d'exécution (zéro appel
#      moteur, zéro écriture, zéro bannière/ligne rendues).
#   3. Placement de la bannière : slot `header=` du page_navbar (même
#      tagList que le shim i18n et le badge drive, app.R:347-350) — précedent
#      documenté du uiOutput NULL = conteneur vide, empreinte nulle ; pleine
#      largeur au-dessus des onglets = non intrusif.
#   4. Ligne d'état : DANS le panneau du toggle (sidebar, sous le switch) —
#      elle rapporte l'état du magasin que le toggle gouverne ; renderUI
#      NULL quand OFF (aucun élément, règle du sprintf-trap app.R:801-810).
#   5. Offre = le DERNIER checkpoint complet du magasin qui VALIDE (plan
#      ASR-2 refusé => aucune offre, contrat §5.1) ; le plan est construit
#      UNIQUEMENT à l'activation du toggle (événement, jamais de timer) ;
#      le payload $banner (timestamp/counts/losses) est rendu verbatim.
#   6. Accepter = ts_session_checkpoint_restore_apply(plan, global_data) —
#      reactiveValues = environnement : rebinding sur place, bump F08 inclus
#      par le plan (epoch_action "bump", décision ASR-2) ; refuser = purge de
#      l'offre SEULEMENT — zéro appel apply, zéro écriture (contrat §7.3).
# =============================================================================

# Le moteur ASR-1/2 est sourcé DIRECTEMENT (précedent
# test-session-checkpoint-restore.R:104) : le payload de la bannière est le
# sien. Le module ASR-3 est la surface testée (RED : le fichier n'existe pas).
source_project_file("tools/pkg_skeleton/R/session_checkpoint.R")
source_project_file("modules/session/mod_session_checkpoint.R")

# i18n : global.R fournit le vrai Translator à l'exécution ; les tests reçoivent
# le repli no-op (même sémantique que global.R:253-258) si absent — les clés
# SONT les chaînes françaises, donc $t() repli = clé elle-même.
if (!exists("i18n", envir = globalenv(), inherits = FALSE)) {
  assign("i18n", list(
    t                        = function(key, ...) key,
    set_translation_language = function(language) invisible(NULL),
    get_translation_language = function() "fr"
  ), envir = globalenv())
}
# Interpolation nommée (global.R .t_fmt) — repli identique si absent.
if (!exists(".t_fmt", envir = globalenv(), inherits = FALSE)) {
  assign(".t_fmt", function(template, ...) {
    vals <- list(...)
    for (nm in names(vals)) {
      template <- gsub(paste0("{", nm, "}"), format(vals[[nm]]), template, fixed = TRUE)
    }
    template
  }, envir = globalenv())
}

suppressPackageStartupMessages(library(shiny))

# ── Clés i18n ASR-3 (fr = clé, en = paire) — la même liste que le §9 ---------

.asr3_keys <- list(
  c("Checkpoints de session", "Session checkpoints"),
  c("Activer les checkpoints de session", "Enable session checkpoints"),
  c("Reprendre la session du {ts} ({n} jeu(x) de donn\u00e9es)",
    "Resume the session from {ts} ({n} dataset(s))"),
  c("Contenu : {r} \u00e9l\u00e9ment(s) restaurable(s), {s} sans valeur conserv\u00e9e, {l} perte(s).",
    "Contents: {r} restorable item(s), {s} with no retained value, {l} loss(es)."),
  c("Pertes :", "Losses:"),
  c("Reprendre la session", "Resume the session"),
  c("Continuer sans restaurer", "Continue without restoring"),
  c("Dernier checkpoint : {ts} \u2014 port\u00e9e : {scope}",
    "Last checkpoint: {ts} — scope: {scope}"),
  c("Aucun checkpoint disponible.", "No checkpoint available."),
  c("(aucun dataset)", "(no dataset)"),
  c("Session restaur\u00e9e depuis le checkpoint du {ts} \u2014 {r} \u00e9l\u00e9ment(s) restaur\u00e9(s), {s} ignor\u00e9(s), {l} perte(s).",
    "Session restored from the checkpoint of {ts} — {r} item(s) restored, {s} skipped, {l} loss(es)."),
  c("\u00c9chec de la restauration du checkpoint : {err}",
    "Checkpoint restore failed: {err}")
)

# ── Fixture : un checkpoint RÉEL (écrit par le moteur ASR-1) dans un magasin
#    bac à sable — jamais l'arbre data_sc/ du projet. Situation couche-3 : un
#    actif présent, un récupérable depuis la source d'import, un perdu. -------

.asr3_store <- function() {
  d <- tempfile(pattern = "ts_asr3_store_")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

.asr3_ckpt <- function(store) {
  bpc <- tempfile(pattern = "ts_asr3_bpc_"); dir.create(bpc, recursive = TRUE)
  fb  <- tempfile(pattern = "ts_asr3_src_"); writeLines("h5", fb)
  layers <- list(
    layer1 = list(
      sc_obj      = data.frame(cell = 1:3, cluster = c("a", "a", "b")),
      bulk_obj    = list(counts = matrix(1:4, 2), metadata = data.frame(cond = c("x", "y"))),
      spatial_obj = list(bpcells_dir = bpc, coords = data.frame(x = c(1, 2), y = c(3, 4)))
    ),
    layer2 = ts_session_checkpoint_layer2(
      pipeline_steps_completed = c("import" = TRUE),
      manual_pipeline = list(status = "done", step = "deconv"),
      business_state = list(has_data = TRUE, object_class = "data.frame",
                            n_elements = 3, n_clusters = 2, deconv_done = FALSE),
      deconv_props = NULL,  # rien de retenu -> item "skipped" (compte honnête)
      derived_tables = list(),
      banners = list()
    ),
    layer3 = ts_session_checkpoint_layer3(list(
      bpcells_dir        = list(path = bpc, kind = "dir"),
      reference_manifest = list(path = file.path(bpc, "absent_manifest.rds"),
                                kind = "file", fallback_source = fb),
      sketch_path        = list(path = file.path(bpc, "absent_sketch.rds"), kind = "file")
    ))
  )
  w <- ts_session_checkpoint_write(
    layers, dir = store, trigger = "manual", dataset_ids = c("ds1", "ds2"),
    now = as.POSIXct("2026-10-31 12:00:00", tz = "UTC"))
  stopifnot(isTRUE(w$ok))  # le moteur ASR-1 doit fonctionner — sinon le test
  #                          échoue pour SA raison, jamais silencieusement.
  list(id = w$id, sc_obj = layers$layer1$sc_obj)
}

# Le payload ASR-2 attendu — produit par le MÊME moteur que le module consomme,
# jamais recalculé à la main dans le test.
.asr3_expected_banner <- function(store, id) {
  plan <- ts_session_checkpoint_restore_plan(id, dir = store)
  stopifnot(isTRUE(plan$ok))
  plan$banner
}

# Horodatage affiché : ISO-8601 UTC du manifeste -> "AAAA-MM-JJ HH:MM UTC"
.asr3_disp_ts <- function(ts_string) {
  t <- as.POSIXct(ts_string, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  if (is.na(t)) return(as.character(ts_string))
  paste0(format(t, "%Y-%m-%d %H:%M", tz = "UTC"), " UTC")
}

# ── Compteur d'appels moteur : WRAPPERS autour du moteur RÉEL (pas des
#    remplaçants) — le payload reste authentique, seules les invocations sont
#    comptées. Restitué à la sortie du test. ----------------------------------

.asr3_counter <- new.env(parent = emptyenv())
.asr3_counter$calls <- character(0)

.asr3_wrap_engines <- function() {
  .asr3_counter$calls <- character(0)
  saved <- new.env(parent = emptyenv())
  for (nm in c("ts_session_checkpoint_list", "ts_session_checkpoint_restore_plan",
               "ts_session_checkpoint_restore_apply")) {
    real <- get(nm, envir = globalenv())
    saved[[nm]] <- real
    # Wrapper qui compte SANS remplacer : il délègue au moteur RÉEL, donc le
    # payload consommé par le module reste authentique (ASR-2 producteur).
    assign(nm, local({
      nm_ <- nm; real_ <- real
      function(...) {
        .asr3_counter$calls <- c(.asr3_counter$calls, nm_)
        real_(...)
      }
    }), envir = globalenv())
  }
  withr::defer({
    for (nm in ls(saved)) assign(nm, saved[[nm]], envir = globalenv())
  }, envir = parent.frame())
  invisible(saved)
}

.asr3_gd <- function() {
  shiny::reactiveValues(
    sc_obj = NULL, sc_obj_epoch = 0L,
    bulk_obj = NULL, bulk_obj_epoch = 0L,
    spatial_obj = NULL, spatial_datasets = list(),
    language = "fr", i18n = NULL
  )
}

# ── 1. La surface existe (RED : le module n'existe pas) ----------------------

test_that("ASR-3 surface: the module exposes ui/banner/server entry points", {
  expect_true(exists("mod_session_checkpoint_ui", envir = globalenv()),
              info = "mod_session_checkpoint_ui() absente — jalon ASR-3 non livré")
  expect_true(exists("mod_session_checkpoint_banner_ui", envir = globalenv()),
              info = "mod_session_checkpoint_banner_ui() absente — bannière header= non livrée")
  expect_true(exists("mod_session_checkpoint_server", envir = globalenv()),
              info = "mod_session_checkpoint_server() absente — jalon ASR-3 non livré")
})

# ── 2. Toggle OFF par défaut (structurel) ------------------------------------

test_that("ASR-3 toggle: default OFF (opt-in, no 'checked' attribute), status + banner outputs declared", {
  panel <- mod_session_checkpoint_ui("session_ckpt")
  html  <- paste(collapse = "", as.character(panel))
  expect_true(grepl('id="session_ckpt-enabled"', html),
              info = "le switch opt-in doit porter l'id session_ckpt-enabled")
  expect_false(grepl("checked", html),
              info = "le toggle doit être DÉSACTIVÉ PAR DÉFAUT (opt-in, contrat §4)")
  expect_true(grepl('id="session_ckpt-status_line"', html),
              info = "la ligne d'état (uiOutput) doit exister dans le panneau")
  b <- mod_session_checkpoint_banner_ui("session_ckpt")
  bh <- paste(collapse = "", as.character(b))
  expect_true(grepl('id="session_ckpt-restore_banner"', bh),
              info = "la bannière (uiOutput header=) doit porter l'id session_ckpt-restore_banner")
})

# ── 3. OFF = comportement actuel : AUCUN appel moteur, AUCUNE UI ASR-3 -------
#    (le magasin contient POURTANT un checkpoint valide — l'offre ne peut
#    apparaître que par une action explicite de l'opérateur, contrat §4/§7.5)

test_that("ASR-3 OFF: zero engine calls and zero rendered checkpoint UI even when a validated checkpoint exists", {
  store <- .asr3_store(); on.exit(unlink(store, recursive = TRUE, force = TRUE), add = TRUE)
  ck <- .asr3_ckpt(store)
  id <- ck$id
  gd <- .asr3_gd()
  .asr3_wrap_engines()
  testServer(mod_session_checkpoint_server,
             args = list(id = "session_ckpt", global_data = gd, store_dir = store), expr = {
    session$flushReact()
    # Zéro appel moteur tant que l'opérateur n'a rien touché :
    expect_identical(.asr3_counter$calls, character(0),
                     info = paste("des appels moteur ont eu lieu toggle OFF —",
                                  "contrat §7.5 (aucune écriture/bannière) et §4",
                                  "(byte pour byte) violés :", paste(.asr3_counter$calls, collapse = ", ")))
    expect_null(session$output$restore_banner,
                info = "bannière rendue toggle OFF — interdit (contrat §4)")
    expect_null(session$output$status_line,
                info = "ligne d'état rendue toggle OFF — interdit (contrat §4)")
  })
})

# ── 4. OFF -> ON -> OFF : l'extinction purge l'offre sans rappeler le moteur --

test_that("ASR-3 OFF again: clearing the toggle removes the offer and makes no further engine call", {
  store <- .asr3_store(); on.exit(unlink(store, recursive = TRUE, force = TRUE), add = TRUE)
  ck <- .asr3_ckpt(store)
  id <- ck$id
  gd <- .asr3_gd()
  .asr3_wrap_engines()
  testServer(mod_session_checkpoint_server,
             args = list(id = "session_ckpt", global_data = gd, store_dir = store), expr = {
    session$setInputs(enabled = TRUE); session$flushReact()
    expect_true(length(.asr3_counter$calls) >= 2L)  # list + plan : le scan événementiel
    expect_false(is.null(session$output$restore_banner))
    n_before <- length(.asr3_counter$calls)
    session$setInputs(enabled = FALSE); session$flushReact()
    expect_null(session$output$restore_banner, info = "l'offre survit à l'extinction du toggle")
    expect_null(session$output$status_line)
    expect_identical(length(.asr3_counter$calls), n_before,
                     info = "l'extinction ne doit PAS rappeler le moteur")
  })
})

# ── 5. ON + checkpoint validé : la bannière rend le payload ASR-2 ------------
#    (timestamp/counts/losses verbatim, jamais recalculés) + action explicite.

test_that("ASR-3 ON: the banner renders the ASR-2 plan payload (timestamp, dataset scope, counts, losses) with an explicit accept action", {
  store <- .asr3_store(); on.exit(unlink(store, recursive = TRUE, force = TRUE), add = TRUE)
  ck <- .asr3_ckpt(store)
  id <- ck$id
  expected <- .asr3_expected_banner(store, id)  # payload producteur ASR-2
  gd <- .asr3_gd()
  .asr3_wrap_engines()
  testServer(mod_session_checkpoint_server,
             args = list(id = "session_ckpt", global_data = gd, store_dir = store), expr = {
    session$setInputs(enabled = TRUE); session$flushReact()
    html <- paste(collapse = "", as.character(session$output$restore_banner))
    # « Reprendre la session du <horodatage> (n jeux de données) » — fiche :89-92
    expect_true(grepl("Reprendre la session du", html, fixed = TRUE),
                info = "le libellé de reprise attendu est absent de la bannière")
    expect_true(grepl(.asr3_disp_ts(expected$timestamp), html, fixed = TRUE),
                info = paste("l'horodatage du payload ASR-2 n'est pas rendu :",
                             expected$timestamp))
    expect_true(grepl(paste0("(", format(expected$n_datasets), " jeu(x) de donn\u00e9es)"), html, fixed = TRUE),
                info = "le nombre de jeux de données du payload n'est pas rendu")
    # counts rendus verbatim (restored / skipped / lost) — ASR-3 rend :
    expect_true(grepl(format(expected$counts[["restored"]]), html, fixed = TRUE),
                info = "count restored absent de la bannière")
    expect_true(grepl(format(expected$counts[["skipped"]]), html, fixed = TRUE),
                info = "count skipped absent de la bannière")
    expect_true(grepl(format(expected$counts[["lost"]]), html, fixed = TRUE),
                info = "count lost absent de la bannière")
    # pertes honnêtes (contrat §6) : le fixture a un actif perdu
    expect_true(length(expected$losses) >= 1L)
    expect_true(grepl("Pertes :", html, fixed = TRUE), info = "les pertes ne sont pas annoncées")
    expect_true(grepl(expected$losses[[1]], html, fixed = TRUE),
                info = "le texte de perte du payload n'est pas rendu verbatim")
    # l'action EXPLICITE (jamais d'injection silencieuse, contrat §7.3)
    expect_true(grepl('id="session_ckpt-restore_accept"', html, fixed = TRUE),
                info = "aucun bouton d'acceptation explicite dans la bannière")
    expect_true(grepl('id="session_ckpt-restore_decline"', html, fixed = TRUE),
                info = "aucun bouton de refus dans la bannière")
    # la ligne d'état vit pendant que le toggle est ON
    st <- paste(collapse = "", as.character(session$output$status_line))
    expect_true(grepl("Dernier checkpoint :", st, fixed = TRUE),
                info = "la ligne d'état ne rend pas pendant que le toggle est ON")
    expect_true(grepl(.asr3_disp_ts(expected$timestamp), st, fixed = TRUE),
                info = "la ligne d'état ne porte pas l'horodatage du dernier checkpoint")
    expect_true(grepl("ds1, ds2", st, fixed = TRUE),
                info = "la ligne d'état ne porte pas la portée datasets du checkpoint")
  })
})

# ── 6. Refuser = rien n'est appliqué (contrat §7.3) ---------------------------

test_that("ASR-3 decline: nothing is applied, no engine apply call, banner cleared", {
  store <- .asr3_store(); on.exit(unlink(store, recursive = TRUE, force = TRUE), add = TRUE)
  ck <- .asr3_ckpt(store)
  id <- ck$id
  gd <- .asr3_gd()
  .asr3_wrap_engines()
  testServer(mod_session_checkpoint_server,
             args = list(id = "session_ckpt", global_data = gd, store_dir = store), expr = {
    session$setInputs(enabled = TRUE); session$flushReact()
    expect_false(is.null(session$output$restore_banner))
    session$setInputs(restore_decline = 1); session$flushReact()
    expect_null(session$output$restore_banner,
                info = "l'offre doit disparaître après refus explicite")
    expect_null(gd$sc_obj, info = "un refus a rebinding des données — interdit (contrat §7.3)")
    expect_null(gd$bulk_obj)
    expect_null(gd$spatial_obj)
    expect_identical(gd$sc_obj_epoch, 0L, info = "un refus a bumpé les epochs — interdit")
    expect_false("ts_session_checkpoint_restore_apply" %in% .asr3_counter$calls,
                 info = "un refus a appelé restore_apply — interdit (contrat §7.3)")
  })
})

# ── 7. Accepter = le plan ASR-2 est appliqué dans global_data -----------------
#    (action explicite ; rebinding sur place + bump F08 par le plan ASR-2)

test_that("ASR-3 accept: the ASR-2 plan is applied into global_data once, offer cleared after", {
  store <- .asr3_store(); on.exit(unlink(store, recursive = TRUE, force = TRUE), add = TRUE)
  ck <- .asr3_ckpt(store)
  id <- ck$id
  expected <- .asr3_expected_banner(store, id)
  gd <- .asr3_gd()
  .asr3_wrap_engines()
  testServer(mod_session_checkpoint_server,
             args = list(id = "session_ckpt", global_data = gd, store_dir = store), expr = {
    session$setInputs(enabled = TRUE); session$flushReact()
    session$setInputs(restore_accept = 1); session$flushReact()
    expect_true("ts_session_checkpoint_restore_apply" %in% .asr3_counter$calls,
                info = "l'acceptation n'a pas exécuté restore_apply (plan ASR-2)")
    expect_identical(gd$sc_obj, ck$sc_obj,
                     info = "le rebinding layer-1 sc_obj ne correspond pas au payload")
    expect_identical(gd$bulk_obj$counts, matrix(1:4, 2), info = "bulk_obj non rebinding")
    # F08 : le bump d'epoch est exécuté par apply() (décision ASR-2) — de 0L à 1L
    expect_identical(gd$sc_obj_epoch, 1L)
    expect_identical(gd$bulk_obj_epoch, 1L)
    # couche 3 : la table des actifs résolus est rebindée (cible gelée ASR-2)
    expect_false(is.null(gd$session_checkpoint_assets))
    expect_true(all(c("bpcells_dir", "reference_manifest") %in% names(gd$session_checkpoint_assets)))
    # l'offre est purgée après application
    expect_null(session$output$restore_banner)
  })
})

# ── 8. Le chemin moteur reste HORS d'app.R : app.R ne fait que MONTER --------

test_that("ASR-3 wiring: app.R sources and mounts the module, and never calls the engine directly", {
  app_lines <- readLines(file.path(ts_project_root(), "app.R"), warn = FALSE)
  app_src <- paste(app_lines, collapse = "\n")
  # Garde moteur : sur le CODE seulement (lignes pleinement commentées
  # retirées) — un commentaire qui MENTIONNE le moteur n'est pas un APPEL.
  # (passe de fix d'héritage 2026-11-05 : app.R:115 documente la résolution
  # de racine partagée et tripait ce garde sans être un appel.)
  app_code <- paste(app_lines[!grepl("^\\s*#", app_lines)], collapse = "\n")
  # le module est sourcé par app.R (garde d'exhaustivité test-app-sourcing.R)
  expect_true(any(grepl('source\\("modules/session/mod_session_checkpoint\\.R"\\)', app_lines, perl = TRUE)),
              info = "app.R doit sourcer modules/session/mod_session_checkpoint.R")
  # les trois montages, id "session_ckpt"
  expect_true(grepl('mod_session_checkpoint_ui\\("session_ckpt"\\)', app_src),
              info = "le panneau toggle/ligne d'état n'est pas monté dans la sidebar")
  expect_true(grepl('mod_session_checkpoint_banner_ui\\("session_ckpt"\\)', app_src),
              info = "la bannière n'est pas montée dans le header= du page_navbar")
  expect_true(grepl('mod_session_checkpoint_server\\("session_ckpt", global_data\\)', app_src),
              info = "le serveur du module n'est pas monté dans server()")
  # AUCUN appel moteur direct depuis app.R (l'accès moteur vit dans le module)
  expect_false(grepl("ts_session_checkpoint_", app_code),
               info = paste("app.R appelle le moteur directement — l'accès moteur",
                            "doit rester dans le module ASR-3 (un seul appel,",
                            "derrière le toggle)"))
})

# ── 9. 0 CSS www/ (règle post-O13) : primitives bslib/shiny seulement ---------

test_that("ASR-3 introduces no CSS in www/ and no style injection", {
  mod_path <- file.path(ts_project_root(), "modules", "session", "mod_session_checkpoint.R")
  mod_src <- paste(readLines(mod_path, warn = FALSE), collapse = "\n")
  expect_false(grepl("tags\\$style|includeCSS|tags\\$head", mod_src),
               info = "le module ASR-3 injecte du style — interdit (0 CSS, primitives seulement)")
  www <- file.path(ts_project_root(), "www")
  if (dir.exists(www)) {
    css <- list.files(www, pattern = "css$", recursive = TRUE, ignore.case = TRUE, full.names = FALSE)
    # Passe de fix (2026-11-01) : expect_length() n'a PAS d'argument info= —
    # ce garde aurait levé « unused argument » s'il avait déjà tourné (www/
    # absent du dépôt : jamais évalué, bug latent signalé par la vérification
    # ASR-4). expect_equal(length(.), 0L, info=) = même verdict + le message.
    expect_equal(length(css[grep("asr3|checkpoint|session_ckpt", css, ignore.case = TRUE)]), 0L,
                 info = "un fichier CSS ASR-3 est apparu dans www/ — interdit (règle post-O13)")
  }
})

# ── 10. Clés i18n fr/en (mécanisme add_i18n_keys.R SCOPÉ — leçon B1/B5/DSTV-3) -

test_that("ASR-3 i18n: every fr/en key pair exists in i18n/translation.json (scoped append)", {
  path <- file.path(ts_project_root(), "i18n", "translation.json")
  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  fr <- vapply(j$translation, function(x) if (is.null(x$fr)) "" else x$fr, character(1))
  en <- vapply(j$translation, function(x) if (is.null(x$en)) "" else x$en, character(1))
  for (k in .asr3_keys) {
    idx <- match(k[1], fr)
    expect_false(is.na(idx),
                 info = paste("clé FR absente de translation.json :", k[1]))
    expect_identical(en[[idx]], k[2],
                     info = paste("paire EN absente ou divergente pour :", k[1]))
  }
})

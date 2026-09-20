# =============================================================================
# test-core-error-state.R — l'accesseur GÉNÉRIQUE d'état d'erreur (§14.1)
# =============================================================================
# CONVENTIONS §14.1 prescrit UN accesseur générique `ts_error_state(e)` au lieu
# d'un par domaine, les 11 noms existants subsistant en délégations d'une ligne.
#
# ⚠️ LA PRÉMISSE ÉCRITE DE §14.1 EST FAUSSE, ET C'EST MESURÉ ICI.
# Le texte enregistré dit : « les 11 corps sont TEXTUELLEMENT IDENTIQUES et
# sans logique de domaine ». Mesure du 2026-09-20 : ils forment TROIS familles
# distinctes — 7 gardés par leur CLASSE, 3 aveugles à la classe, 1 gardé sur
# `"condition"` avec tryCatch + contrôle de longueur. La divergence est
# OBSERVABLE : les 4 aveugles rendent l'état d'une erreur de classe ÉTRANGÈRE,
# les 7 autres rendent NA. Ce fichier épingle la table mesurée (direction 7)
# pour que la consolidation ne puisse pas la déplacer en silence.
# =============================================================================
source_project_file("R/core/error_state.R")

# --- Le jeu des 11 accesseurs, avec la classe que CHACUN garde (NA = aucune) --
# Mesuré le 2026-09-20 : `communication_error_state` garde
# `communication_import_error`, PAS `communication_error` — piège de lecture.
.es_accessors <- function() {
  list(
    list(fn = "bulk_network_error_state",        file = "R/bulk/bulk_network.R",           classe = NA_character_),
    list(fn = "bulk_pattern_error_state",        file = "R/bulk/bulk_pattern.R",           classe = NA_character_),
    list(fn = "bulk_dose_error_state",           file = "R/bulk/dose_response.R",          classe = NA_character_),
    list(fn = "da_design_error_state",           file = "R/sc/sc_abundance_design.R",      classe = "da_design_error"),
    list(fn = "milo_error_state",                file = "R/sc/sc_abundance_milo.R",        classe = "milo_error"),
    list(fn = "sccoda_error_state",              file = "R/sc/sc_abundance_sccoda.R",      classe = "sccoda_error"),
    list(fn = "communication_error_state",       file = "R/sc/sc_communication.R",         classe = "communication_import_error"),
    list(fn = "cellchat_engine_error_state",     file = "R/sc/sc_communication_engine.R",  classe = NA_character_),
    list(fn = "cellchat_input_error_state",      file = "R/sc/sc_communication_input.R",   classe = "cellchat_input_error"),
    list(fn = "population_rarity_error_state",   file = "R/sc/sc_population_rarity.R",     classe = "population_rarity_error"),
    list(fn = "velocity_error_state",            file = "R/sc/sc_velocity.R",              classe = "velocity_validation_error")
  )
}

.es_charge <- function() {
  for (a in .es_accessors()) source_project_file(a$file)
  invisible(TRUE)
}

.es_call <- function(fn, e) get(fn, envir = globalenv())(e)

# =============================================================================
# 1. L'accesseur existe, et sa forme est celle qui est prescrite
# =============================================================================
test_that("ts_error_state existe et porte le filtre de classe optionnel", {
  expect_true(is.function(ts_error_state))
  expect_true("class" %in% names(formals(ts_error_state)))
  # Le défaut doit être NULL : un domaine NOUVEAU appelle `ts_error_state(e)`
  # sans rien savoir de sa classe — c'est la raison d'être de §14.1.
  expect_null(formals(ts_error_state)$class)
})

# =============================================================================
# 2. Domaine du contrat : condition classée + état scalaire
# =============================================================================
test_that("domaine du contrat : l'état d'une erreur classée est rendu tel quel", {
  e <- errorCondition("msg", class = "zzz_error", state = "invalid_input")
  expect_identical(ts_error_state(e), "invalid_input")
  expect_identical(ts_error_state(e, "zzz_error"), "invalid_input")
  # La classe doit survivre : c'est l'axe de ROUTAGE (§14.1), on n'y touche pas.
  expect_true(inherits(e, "zzz_error"))
})

# =============================================================================
# 3. Erreur NON classée : NA, jamais une chaîne vide ni NULL
# =============================================================================
test_that("une erreur non classée rend NA_character_", {
  expect_identical(ts_error_state(simpleError("nu")), NA_character_)
  expect_true(is.na(ts_error_state(simpleError("nu"))))
})

# =============================================================================
# 4. Entrée qui n'est pas une condition : NA (jamais une erreur)
# =============================================================================
test_that("une entrée non-condition rend NA au lieu de lever", {
  expect_identical(ts_error_state(list(state = "x")), NA_character_)
  expect_identical(ts_error_state(NULL), NA_character_)
  expect_identical(ts_error_state("texte"), NA_character_)
  expect_identical(ts_error_state(42L), NA_character_)
})

# =============================================================================
# 5. Le filtre de classe — c'est LUI qui préserve les 7 accesseurs gardés
# =============================================================================
test_that("le filtre de classe discrimine, et son absence ne filtre rien", {
  etrangere <- errorCondition("m", class = "autre_domaine_error", state = "s_foreign")
  # sans filtre : l'état est rendu (familles aveugles à la classe)
  expect_identical(ts_error_state(etrangere), "s_foreign")
  # filtre qui MATCHE
  expect_identical(ts_error_state(etrangere, "autre_domaine_error"), "s_foreign")
  # filtre qui NE matche PAS : NA, et c'est la propriété des 7 accesseurs gardés
  expect_identical(ts_error_state(etrangere, "milo_error"), NA_character_)
})

# =============================================================================
# 6. Normalisation des entrées PATHOLOGIQUES
# =============================================================================
# Le contrat documenté est « l'état, ou NA ». Trois familles historiques le
# violaient : `state` de longueur 2 était rendu tel quel, et un `state` entier
# sortait en `integer`. La garde normalise : hors domaine, c'est NA.
test_that("hors domaine du contrat, l'accesseur normalise au lieu de fuiter", {
  expect_identical(ts_error_state(errorCondition("m", state = c("a", "b"))), NA_character_)
  expect_identical(ts_error_state(errorCondition("m", state = NA_character_)), NA_character_)
  expect_identical(ts_error_state(errorCondition("m", state = character(0))), NA_character_)
  # Un état non-character est converti, jamais rendu dans son type d'origine.
  expect_identical(ts_error_state(errorCondition("m", state = 1L)), "1")
  expect_type(ts_error_state(errorCondition("m", state = 1L)), "character")
})

# =============================================================================
# 7. CARACTÉRISATION des 11 accesseurs — la table MESURÉE
# =============================================================================
# Avant la consolidation, `_probe41c.R`/`_probe41d.R` ont mesuré, accesseur par
# accesseur, le verdict sur trois entrées. On épingle ces verdicts : la
# consolidation doit les REPRODUIRE, pas les déplacer.
test_that("les 11 accesseurs rendent la table mesurée (propre / nu / étranger)", {
  .es_charge()
  for (a in .es_accessors()) {
    propre <- if (is.na(a$classe)) {
      .es_call(a$fn, errorCondition("m", class = "ts_probe_own_error", state = "s_own"))
    } else {
      .es_call(a$fn, errorCondition("m", class = a$classe, state = "s_own"))
    }
    expect_identical(propre, "s_own", info = a$fn)

    expect_identical(.es_call(a$fn, simpleError("nu")), NA_character_, info = a$fn)

    # L'état d'une erreur de classe ÉTRANGÈRE : rendu par les 4 aveugles,
    # refusé (NA) par les 7 gardés. C'est LA divergence mesurée.
    etranger <- .es_call(a$fn,
      errorCondition("m", class = "ts_probe_foreign_error", state = "s_foreign"))
    if (is.na(a$classe)) {
      expect_identical(etranger, "s_foreign", info = a$fn)
    } else {
      expect_identical(etranger, NA_character_, info = a$fn)
    }
  }
})

# =============================================================================
# 8. La consolidation est EFFECTIVE : plus aucun corps dupliqué
# =============================================================================
# Sans cette direction, on pourrait créer `ts_error_state()` et laisser les 11
# corps intacts : le test passerait, la dette resterait. On assère donc la
# DISPARITION du corps historique (`e$state` lu à la main) et la PRÉSENCE de la
# délégation.
test_that("les 11 corps dupliqués ont disparu au profit d'une délégation", {
  for (a in .es_accessors()) {
    src <- readLines(file.path(ts_project_root(), a$file), warn = FALSE)
    i <- grep(paste0("^", a$fn, " <- function"), src)
    expect_length(i, 1L)
    bloc <- paste(src[i:min(i + 5L, length(src))], collapse = "\n")
    expect_true(grepl("ts_error_state(", bloc, fixed = TRUE), info = a$fn)
    expect_false(grepl("e$state", bloc, fixed = TRUE), info = a$fn)
    expect_false(grepl("is.null(st)", bloc, fixed = TRUE), info = a$fn)
  }
})

# =============================================================================
# 9. Les 7 gardés TRANSMETTENT leur classe (sinon le filtre serait perdu)
# =============================================================================
# La direction 7 prouve le comportement ; celle-ci prouve le MÉCANISME, et
# échoue pour une raison DIFFÉRENTE (une délégation sans argument de classe
# passerait la 7 si l'entrée étrangère n'était jamais testée... elle l'est,
# mais l'erreur serait alors imputée au générique, pas à l'accesseur).
test_that("chaque accesseur gardé déclare SA classe dans la délégation", {
  for (a in .es_accessors()) {
    if (is.na(a$classe)) next
    src <- readLines(file.path(ts_project_root(), a$file), warn = FALSE)
    i <- grep(paste0("^", a$fn, " <- function"), src)
    bloc <- paste(src[i:min(i + 5L, length(src))], collapse = "\n")
    expect_true(grepl(paste0('"', a$classe, '"'), bloc, fixed = TRUE), info = a$fn)
  }
})

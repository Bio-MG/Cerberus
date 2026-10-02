# =============================================================================
# test-sc-population-rarity-contract-freeze.R — gel du contrat (CCC 9, Q1)
# =============================================================================
# REFUSE toute evolution incompatible : champs du resultat canonique, etats de
# validite + libelles, regles autorisees, surface publique de
# R/sc/sc_population_rarity.R, orchestration module, cablage dans mod_sc.R et
# app.R, ABSENCE de moteur duplique (regle 3), ABSENCE de porte Stage 13
# (inapplicable : aucune affirmation differentielle), ABSENCE de seuil par
# defaut dans config/, synchronisation code <-> POPULATION_RARITY_CONTRACT.md.
# =============================================================================

.pr_freeze_top_level_assignments <- function(relpath) {
  exprs <- parse(file.path(ts_project_root(), relpath), keep.source = FALSE)
  nms <- character(0)
  for (e in exprs) {
    if (is.call(e) && identical(deparse(e[[1L]]), "<-") && is.symbol(e[[2L]])) {
      nms <- c(nms, as.character(e[[2L]]))
    }
  }
  unique(nms)
}

.pr_freeze_src <- function(relpath) {
  if (grepl("^docs/contracts/", relpath) &&
      !file.exists(file.path(ts_project_root(), relpath))) {
    skip(.ts_contract_skip_msg(relpath))
  }
  paste(readLines(file.path(ts_project_root(), relpath), warn = FALSE,
                  encoding = "UTF-8"), collapse = "\n")
}

# Lignes de CODE uniquement (les commentaires sont retires). Necessaire aux
# gardes du type « ce symbole n'est jamais UTILISE » : un commentaire qui
# INTERDIT explicitement un symbole (ex. « ne jamais reutiliser TS_DA_... »)
# n'est pas une utilisation et ne doit pas faire echouer la garde.
.pr_freeze_code_lines <- function(relpath) {
  if (grepl("^docs/contracts/", relpath) &&
      !file.exists(file.path(ts_project_root(), relpath))) {
    skip(.ts_contract_skip_msg(relpath))
  }
  ln <- readLines(file.path(ts_project_root(), relpath), warn = FALSE,
                  encoding = "UTF-8")
  ln[!grepl("^\\s*#", ln)]
}

# Normalisation des documents de contrat : la prose francaise porte des accents
# et une capitalisation libre ("Règle 3", "Aucun seuil implicite"), alors que
# les jetons verifies sont ASCII. Sans normalisation, la garde de
# synchronisation code <-> document dependrait du clavier, pas du contenu.
# Les paires sont ecrites en \uXXXX (aucune ambiguite d'encodage) et les deux
# cotes de chartr() sont DERIVES des memes paires : impossible de les
# desaligner (une paire oubliee ne peut pas produire un decalage silencieux).
.pr_freeze_norm <- function(x) {
  pairs <- c(
    "\u00e0" = "a", "\u00e2" = "a", "\u00e4" = "a", "\u00e7" = "c",
    "\u00e9" = "e", "\u00e8" = "e", "\u00ea" = "e", "\u00eb" = "e",
    "\u00ee" = "i", "\u00ef" = "i", "\u00f4" = "o", "\u00f6" = "o",
    "\u00f9" = "u", "\u00fb" = "u", "\u00fc" = "u",
    "\u00c0" = "A", "\u00c2" = "A", "\u00c4" = "A", "\u00c7" = "C",
    "\u00c9" = "E", "\u00c8" = "E", "\u00ca" = "E", "\u00cb" = "E",
    "\u00ce" = "I", "\u00cf" = "I", "\u00d4" = "O", "\u00d6" = "O",
    "\u00d9" = "U", "\u00db" = "U", "\u00dc" = "U"
  )
  x <- paste(x, collapse = "\n")
  x <- chartr(paste(names(pairs), collapse = ""),
              paste(unname(pairs), collapse = ""), x)
  tolower(x)
}

# ── Schema du resultat canonique ────────────────────────────────────────────
test_that("canonical rarity result exposes exactly the frozen contract fields", {
  canon <- .pr_run()
  expect_identical(names(canon), population_rarity_contract_fields())
  expect_identical(canon$type, "sc_population_rarity")
  expect_identical(canon$analysis_id, "sc-population-rarity")
})

test_that("frozen per-field shapes on the canonical rarity result", {
  canon <- .pr_run()
  expect_true(canon$status %in% population_rarity_validity_states())
  expect_identical(canon$rarity_rule$direction, "below")
  expect_identical(names(canon$rarity_rule), c("rule_type", "threshold", "direction"))
  expect_identical(
    colnames(canon$population_table),
    c("population", "n_cells", "fraction", "is_rare")
  )
  expect_identical(
    names(canon$summary),
    c("n_populations", "n_rare", "n_cells_total", "n_cells_counted",
      "identity_column", "declared_rule_label")
  )
  expect_identical(
    names(canon$identity_summary),
    c("n_levels", "n_levels_empty", "n_labels_na", "n_cells_counted")
  )
  expect_identical(
    names(canon$parameters),
    c("identity_column", "rule_type", "threshold", "sample_column")
  )
  expect_identical(
    names(canon$qc),
    c("n_cells_total", "n_cells_counted", "n_labels_na", "n_levels_empty",
      "empty_levels")
  )
  expect_identical(
    names(canon$object_identity),
    c("fingerprint", "method", "seurat_dims")
  )
  expect_identical(canon$provenance$analysis_id, "sc-population-rarity")
  expect_identical(canon$provenance$analysis_type, "population_rarity")
  expect_match(canon$timestamp_utc, "^\\d{4}-\\d{2}-\\d{2}T")
})

test_that("validity states, labels and rule types are frozen", {
  expect_setequal(population_rarity_validity_states(), c(
    "valid", "valid_with_warnings", "unavailable_single_population",
    "invalid_input"
  ))
  expect_setequal(names(population_rarity_status_labels()),
                  population_rarity_validity_states())
  expect_setequal(population_rarity_rule_types(),
                  c("absolute_n_cells", "relative_fraction"))
})

# ── Surfaces publiques figees ───────────────────────────────────────────────
test_that("public API surface of sc_population_rarity.R is frozen", {
  defined <- .pr_freeze_top_level_assignments("R/sc/sc_population_rarity.R")
  public <- defined[!grepl("^\\.", defined)]
  expect_setequal(public, population_rarity_public_api())
  expect_true(all(c(".POPULATION_RARITY_STATUS_STATES",
                    ".POPULATION_RARITY_ANALYSIS_ID",
                    ".population_rarity_stop",
                    ".population_rarity_is_blank",
                    ".population_rarity_bad_scalar",
                    ".population_rarity_floor",
                    ".population_rarity_rule_label",
                    ".population_rarity_check_threshold") %in%
                    setdiff(defined, public)))
})

# ── Regle 3 : aucun moteur duplique, aucun graphe kNN ───────────────────────
test_that("the rarity domain reuses no engine and builds no kNN graph", {
  src <- .pr_freeze_src("R/sc/sc_population_rarity.R")
  # Aucune primitive miloR / graphe : la question ne requiert aucun voisinage.
  expect_false(grepl("miloR::|buildGraph\\(|makeNhoods\\(|testNhoods\\(|calcNhoodDistance\\(",
                     src))
  # La table d'identites du DESIGN DA n'est ni reutilisee ni reecrite.
  expect_false(grepl("validate_da_design\\(|finalize_da_design_result\\(",
                     src))
  # Aucun modele / test statistique.
  expect_false(grepl("model\\.matrix|glm\\(|p\\.adjust|edgeR::", src))
})

# ── Porte Stage 13 : INAPPLICABLE (motif ecrit dans le contrat) ─────────────
test_that("the Stage 13 gate is NOT called and its inapplicability is documented", {
  src <- .pr_freeze_src("R/sc/sc_population_rarity.R")
  # La porte n'est JAMAIS appelee : aucune affirmation differentielle.
  expect_false(grepl("assert_da_design_result\\(", src))
  # Le motif est ECRIT dans l'en-tete du domaine...
  expect_match(src, "INAPPLICABLE", fixed = TRUE)
  expect_match(src, "descriptive_only", fixed = TRUE)
  # ...et dans le contrat (exigence explicite de l'audit Milo §4).
  doc <- .pr_freeze_src("docs/contracts/POPULATION_RARITY_CONTRACT.md")
  expect_match(doc, "assert_da_design_result", fixed = TRUE)
  expect_match(doc, "INAPPLICABLE", fixed = TRUE)
  expect_match(doc, "descriptive_only", fixed = TRUE)
  # ...et dans la provenance produite.
  canon <- .pr_run()
  expect_identical(canon$provenance$descriptive_only, TRUE)
  expect_identical(canon$provenance$parameters$descriptive_only, TRUE)
})

# ── Aucun seuil de rarete par defaut ────────────────────────────────────────
test_that("no default rarity threshold exists anywhere", {
  cfg <- .pr_freeze_src("config/defaults.R")
  expect_match(cfg, "TS_POPULATION_RARITY_RULES", fixed = TRUE)
  expect_match(cfg, "TS_POPULATION_RARITY_MIN_CELLS_TOTAL", fixed = TRUE)
  # Ni seuil par defaut, ni reutilisation du plancher du design DA.
  expect_false(grepl("TS_POPULATION_RARITY_DEFAULT|TS_POPULATION_RARITY_THRESHOLD",
                     cfg))
  # CODE uniquement : l'en-tete INTERDIT explicitement ce symbole (c'est un
  # rappel pedagogique), ce n'est pas une utilisation.
  expect_false(any(grepl("TS_DA_MIN_IDENTITY_CELLS_PER_SAMPLE",
                         .pr_freeze_code_lines("R/sc/sc_population_rarity.R"),
                         fixed = TRUE)))
  # Le domaine REFUSE un appel sans seuil declare (aucun repli implicite).
  expect_error(.pr_run(rule_type = NULL), class = "population_rarity_error")
  expect_error(.pr_run(threshold = NULL), class = "population_rarity_error")
})

# ── Cablage : app.R + mod_sc.R ──────────────────────────────────────────────
test_that("the rarity domain and module are sourced exactly once in app.R", {
  app <- .pr_freeze_src("app.R")
  expect_match(app, 'source("R/sc/sc_population_rarity.R")', fixed = TRUE)
  expect_match(app, 'source("modules/sc/mod_sc_rarity.R")', fixed = TRUE)
  # Le domaine est source APRES sc_velocity.R (empreinte v2 reutilisee).
  expect_true(regexpr('source\\("R/sc/sc_velocity.R"\\)', app)[1L] <
                regexpr('source\\("R/sc/sc_population_rarity.R"\\)', app)[1L])
})

test_that("the rarity module defines exactly its three orchestration functions", {
  expect_setequal(
    .pr_freeze_top_level_assignments("modules/sc/mod_sc_rarity.R"),
    c("mod_sc_rarity_ui", "mod_sc_rarity_output_ui", "mod_sc_rarity_server")
  )
  src <- .pr_freeze_src("modules/sc/mod_sc_rarity.R")
  # Le module CONSOMME le domaine — il ne recompte jamais lui-meme.
  expect_match(src, "compute_population_rarity(", fixed = TRUE)
  expect_match(src, "assert_population_rarity_result(", fixed = TRUE)
  expect_match(src, "population_rarity_rule_types(", fixed = TRUE)
  expect_match(src, "build_population_rarity_summary(", fixed = TRUE)
  expect_match(src, "build_population_rarity_table_export(", fixed = TRUE)
  expect_match(src, "population_rarity_export_filename(", fixed = TRUE)
  expect_match(src, "provenance_append(", fixed = TRUE)
  # \b : evite le faux positif « ts_datatable( » (le module DOIT l'utiliser).
  expect_false(grepl("\\btabulate\\(|\\btable\\(|miloR::|buildGraph\\(", src))
})

test_that("rarity is mounted once in mod_sc.R (panel 2b + output tab)", {
  mod_sc_src <- .pr_freeze_src("modules/sc/mod_sc.R")
  expect_match(mod_sc_src, "mod_sc_rarity_ui", fixed = TRUE)
  expect_match(mod_sc_src, "mod_sc_rarity_output_ui", fixed = TRUE)
  expect_match(mod_sc_src, "mod_sc_rarity_server", fixed = TRUE)
  expect_match(mod_sc_src, "2b_rarity", fixed = TRUE)
  expect_match(mod_sc_src, "tab_rarity", fixed = TRUE)
  # Le cablage ne fait AUCUN calcul (aucun appel de domaine dans mod_sc.R).
  expect_false(grepl("compute_population_rarity", mod_sc_src))
})

# ── Synchronisation code <-> contrat documentaire ───────────────────────────
test_that("contract document is in sync with the frozen code", {
  doc_path <- file.path(ts_project_root(), "docs", "contracts",
                        "POPULATION_RARITY_CONTRACT.md")
  skip_if_not(file.exists(doc_path), .ts_contract_skip_msg(doc_path))
  doc <- paste(.ts_contract_readlines(doc_path, warn = FALSE, encoding = "UTF-8"),
               collapse = "\n")
  for (f in population_rarity_contract_fields()) {
    expect_match(doc, paste0("`", f, "`"), fixed = TRUE,
                 info = paste("champ contrat absent du document :", f))
  }
  for (st in population_rarity_validity_states()) {
    expect_match(doc, st, fixed = TRUE,
                 info = paste("etat absent du document :", st))
  }
  for (fn in c("compute_population_rarity", "assert_population_rarity_result",
               "population_rarity_contract_fields",
               "population_rarity_validity_states",
               "population_rarity_public_api")) {
    expect_match(doc, fn, fixed = TRUE,
                 info = paste("fonction absente du document :", fn))
  }
  # La regle 3 (aucun moteur duplique) et l'absence de seuil par defaut sont
  # documentees, pas seulement affirmees dans le code. Comparaison NORMALISEE
  # (sans accents, minuscules) : la prose du contrat reste en francais correct.
  doc_norm <- .pr_freeze_norm(doc)
  expect_match(doc_norm, .pr_freeze_norm("Regle 3"), fixed = TRUE)
  expect_match(doc_norm, .pr_freeze_norm("aucun seuil implicite"), fixed = TRUE)
})

test_that("the report section is descriptive and never re-runs the analysis", {
  tpl <- .pr_freeze_src("reports/sc_report_template.Rmd")
  expect_match(tpl, "rarity", fixed = TRUE)
  expect_false(grepl("compute_population_rarity\\(", tpl))
})

# =============================================================================
# test-sc-communication-engine.R — Moteur CellChat natif (Path B)
# =============================================================================
# Nommage : ce fichier est le test EPONYME de R/sc/sc_communication_engine.R
# (regle C9 du garde de conventions). Comme les contrats BULK_DOSE_RESPONSE,
# BULK_PATTERN, PLOT_DATATABLE, PLOT_EXPORT et PLOT_HEATMAP, il porte aussi
# les assertions de GEL du contrat — un seul fichier, deux roles, aucun
# contrat sans test (C8 = 0).
# =============================================================================
# Ce fichier REFUSE toute evolution incompatible du contrat
# R/sc/sc_communication_engine.R :
#   - surface publique (cellchat_engine_public_api()) ;
#   - les 6 etats de validite ;
#   - LA REGLE DES DEUX VOIES : le moteur produit les 12 champs canoniques du
#     contrat Stage 11, exactement comme l'import (aucun champ invente) ;
#   - L'INVARIANT DU MOTEUR EPHEMERE : l'objet CellChat n'est jamais stocke
#     dans l'etat partage (verifie STATIQUEMENT — aucune affectation d'un tel
#     objet — et DYNAMIQUEMENT sur la classe du resultat) ;
#   - les interdits d'ingenierie : pas de updateCellChatDB(), pas de
#     set.seed() global, pas de plan future imbrique, pas de p.adjust() ;
#   - le piege nomme par la proposition §9.4 : le parametre de permutation
#     s'appelle `nboot`, PAS `nPerm` ;
#   - synchronisation code <-> docs/contracts/CELLCHAT_ENGINE_CONTRACT.md.
#
# La majorite des assertions est STATIQUE (elles s'executent sans CellChat).
# Seul le bloc "parcours reel" exige le paquet : il est skipe sinon, jamais
# en echec — la dependance est GitHub et PARESSeUSE par decision (§2al).
# =============================================================================

source_project_file("R/core/io_helpers.R")
source_project_file("R/core/provenance.R")
source_project_file("R/plotting/palettes.R")
source_project_file("R/sc/sc_velocity.R")
source_project_file("R/sc/sc_communication.R")
source_project_file("R/sc/sc_communication_input.R")
source_project_file("R/sc/sc_communication_engine.R")

.cc_eng_path <- "R/sc/sc_communication_engine.R"

.cc_eng_top_level <- function(relpath) {
  exprs <- parse(file.path(ts_project_root(), relpath), keep.source = FALSE)
  nms <- character(0)
  for (e in exprs) {
    if (is.call(e) && identical(deparse(e[[1L]]), "<-") && is.symbol(e[[2L]])) {
      nms <- c(nms, as.character(e[[2L]]))
    }
  }
  unique(nms)
}

# Le texte du fichier, SANS les chaines ni les commentaires : les assertions
# statiques ne doivent pas reagir a une chaine qui cite un nom interdit.
.cc_eng_code <- function() {
  exprs <- parse(file.path(ts_project_root(), .cc_eng_path), keep.source = FALSE)
  txt <- paste(vapply(exprs, function(e) paste(deparse(e), collapse = "\n"),
                      character(1)), collapse = "\n")
  # retire les chaines litterales (les messages d'erreur citent volontairement
  # les noms interdits pour expliquer l'interdit)
  gsub('"[^"]*"', '""', txt)
}

# Le meme texte, AVEC les chaines litterales : necessaire pour les assertions
# positives (« tel argument est passe avec telle valeur »).
.cc_eng_raw <- function() {
  exprs <- parse(file.path(ts_project_root(), .cc_eng_path), keep.source = FALSE)
  paste(vapply(exprs, function(e) paste(deparse(e), collapse = "\n"),
               character(1)), collapse = "\n")
}

# ── Surface publique ────────────────────────────────────────────────────────
test_that("the public API surface is EXACTLY the frozen list", {
  top <- .cc_eng_top_level(.cc_eng_path)
  public <- setdiff(top, grep("^\\.", top, value = TRUE))
  expected <- c(cellchat_engine_public_api(), liana_engine_public_api())
  expect_setequal(public, expected)
  expect_false(anyDuplicated(expected) > 0)
  expect_identical(cellchat_engine_public_api(), sort(cellchat_engine_public_api()))
  expect_identical(liana_engine_public_api(), sort(liana_engine_public_api()))
})

test_that("every internal helper stays dot-prefixed", {
  top <- .cc_eng_top_level(.cc_eng_path)
  internals <- grep("^\\.", top, value = TRUE)
  expect_true(length(internals) > 0)
  expect_true(all(grepl("^\\.", internals)))
  expect_setequal(
    setdiff(
      .cc_eng_top_level(.cc_eng_path),
      c(cellchat_engine_public_api(), liana_engine_public_api())
    ),
    internals
  )
})

# ── Etats de validite ───────────────────────────────────────────────────────
test_that("the engine validity states are frozen", {
  expect_identical(
    cellchat_engine_states(),
    c("valid", "missing_dependency", "invalid_input", "invalid_parameters",
      "engine_failure", "no_interactions")
  )
})

test_that("a classified error carries its state", {
  e <- tryCatch(
    cellchat_analysis_identity(list()),
    error = function(e) e
  )
  expect_s3_class(e, "cellchat_engine_error")
  expect_identical(cellchat_engine_error_state(e), "invalid_input")
  expect_identical(cellchat_engine_error_state(simpleError("autre")), NA_character_)
})

# ── Interdits d'ingenierie (statiques) ──────────────────────────────────────
test_that("the engine never calls updateCellChatDB (base jamais mutee)", {
  # La base LR ne doit jamais changer silencieusement entre deux runs : deux
  # resultats portant la meme database_version doivent rester comparables.
  expect_false(grepl("updateCellChatDB", .cc_eng_code(), fixed = TRUE))
})

test_that("the seed is a declared parameter, never a global side effect", {
  expect_false(grepl("set\\.seed", .cc_eng_code()))
  # `seed` n'a PAS de valeur par defaut : un run sans graine tracee est refuse.
  f <- formals(run_cellchat)
  expect_true(is.symbol(f$seed))
  expect_false(nzchar(as.character(f$seed)))
})

test_that("no nested future plan: sequential is forced inside the job", {
  code <- .cc_eng_code()
  expect_true(grepl("future::plan", code, fixed = TRUE))
  expect_true(grepl("sequential", code, fixed = TRUE))
  # aucun autre mecanisme de workers (regle dure du depot)
  expect_false(grepl("MulticoreParam", code, fixed = TRUE))
  expect_false(grepl("enableWGCNAThreads", code, fixed = TRUE))
})

test_that("the permutation parameter is nboot, not the invented nPerm", {
  code <- .cc_eng_code()
  expect_true(grepl("nboot", code, fixed = TRUE))
  expect_false(grepl("nPerm", code, fixed = TRUE))
})

test_that("no adjusted p-value is fabricated", {
  # p_adjusted reste NA : aucun export standard de CellChat ne le produit au
  # niveau LR. Le fabriquer serait inventer une donnee (regle 1).
  code <- .cc_eng_code()
  expect_false(grepl("p\\.adjust", code))
  expect_false(grepl("BH", code, fixed = TRUE))
})

test_that("ligand/receptor/pathway are resolved from @LR$LRsig, never split", {
  # Le nom d'interaction CellChat ("L_R" voire "L_R1_R2") n'a pas de decoupage
  # univoque : le deviner serait inventer une donnee. On lit LRsig.
  code <- .cc_eng_code()
  expect_true(grepl("LRsig", code, fixed = TRUE))
  expect_false(grepl("strsplit", code, fixed = TRUE))
})

test_that("the engine never reaches into Shiny reactive state", {
  code <- .cc_eng_code()
  for (sym in c("reactiveValues", "reactiveVal", "observeEvent", "renderUI")) {
    expect_false(grepl(sym, code, fixed = TRUE))
  }
})

# ── Regle des deux voies ────────────────────────────────────────────────────
test_that("the engine targets the SAME 12 canonical fields as the import", {
  expect_identical(
    communication_contract_fields(),
    c("sender", "receiver", "ligand", "receptor", "interaction", "pathway",
      "score", "p_value", "p_adjusted", "source_method", "source_file",
      "source_cell_identity_level")
  )
  # le moteur ne declare aucun champ de resultat qui lui serait propre :
  # il reutilise finalize_communication_result().
  code <- .cc_eng_code()
  expect_true(grepl("finalize_communication_result", code, fixed = TRUE))
  expect_true(grepl('computation = "engine"', .cc_eng_raw(), fixed = TRUE))
})

test_that("a computed result is marked import_only = FALSE (Path B)", {
  # Contrat : l'import reste import_only = TRUE (verifie par le freeze test
  # Stage 11) ; le calcul dans l'app doit pouvoir etre DISTINGUE d'un import.
  f <- formals(finalize_communication_result)
  # la valeur par defaut est un appel c("import","engine") : on l'evalue.
  expect_identical(eval(f$computation), c("import", "engine"))
})

# ── Identite d'analyse : derivee de la provenance, jamais dupliquee ─────────
test_that("cellchat_analysis_identity derives from provenance and adds only 2", {
  fake <- list(
    provenance = list(
      analysis_id = "sc-communication-engine",
      dataset_hash = "abc123",
      hash_exact = TRUE,
      dataset_dims = c(2000L, 500L)
    ),
    engine = list(
      engine = "CellChat", engine_version = "2.2.0.9001",
      engine_sha = "deadbeef", database = "CellChatDB.human",
      database_version = "2", seed = 1L, nboot = 100L, n_populations = 5L
    )
  )
  id <- cellchat_analysis_identity(fake)
  # les champs deja portes par new_provenance_entry() sont DERIVES...
  expect_identical(id$analysis_id, "sc-communication-engine")
  expect_identical(id$input_fingerprint, "abc123")
  expect_true(id$input_fingerprint_exact)
  expect_identical(id$dataset_dims, c(2000L, 500L))
  # ...et seuls les deux champs manquants sont ajoutes.
  expect_identical(id$engine_sha, "deadbeef")
  expect_identical(id$database_version, "2")
  expect_identical(id$seed, 1L)
  expect_identical(id$nboot, 100L)
  # aucune duplication : l'identite ne recopie pas `parameters`/`versions`
  expect_null(id$parameters)
  expect_null(id$versions)
})

test_that("cellchat_analysis_identity refuses a foreign object", {
  expect_error(cellchat_analysis_identity("pas un resultat"),
               class = "cellchat_engine_error")
  expect_error(cellchat_analysis_identity(list(provenance = NULL)),
               class = "cellchat_engine_error")
})

# ── Disponibilite paresseuse ────────────────────────────────────────────────
test_that("cellchat_engine_available never loads CellChat", {
  expect_true(is.logical(cellchat_engine_available()))
  expect_length(cellchat_engine_available(), 1L)
})

# ── Documentation synchronisee ───────────────────────────────────────────────
test_that("the contract document exists and states the frozen rules", {
  doc <- file.path(ts_project_root(), "docs", "contracts",
                   "CELLCHAT_ENGINE_CONTRACT.md")
  skip_if_not(file.exists(doc), .ts_contract_skip_msg(doc))
  txt <- paste(.ts_contract_readlines(doc, warn = FALSE), collapse = "\n")
  for (needle in c("database_version", "updateCellChatDB", "nboot",
                   "source_cell_identity_level", "run_cellchat",
                   "cellchat_analysis_identity")) {
    expect_true(grepl(needle, txt, fixed = TRUE),
                label = paste("le contrat doit mentionner", needle))
  }
})

# ── Parcours reel (exige CellChat : dependance GitHub paresseuse) ────────────
test_that("a real run produces a canonical result with no CellChat object", {
  skip_if_not(cellchat_engine_available(), "CellChat absent (dependance GitHub)")

  # Jeu VIABLE : de vrais genes de signalisation (issus de la base CellChat) et
  # une surexpression par groupe. Un jeu jouet (quelques genes, expression
  # uniforme) ne produit AUCUNE paire ligand-recepteur surexprimee et fait
  # echouer CellChat sur « subscript out of bounds » — constate, pas suppose.
  set.seed(20260914)
  cc_env <- new.env(parent = emptyenv())
  utils::data(list = "CellChatDB.human", package = "CellChat", envir = cc_env)
  cc_db <- get("CellChatDB.human", envir = cc_env)
  cc_pw <- c("TGFb", "MIF", "MHC-I", "MHC-II", "GALECTIN", "CD99",
             "ANNEXIN", "VISFATIN", "COMPLEMENT", "CXCL")
  cc_sel <- cc_db$interaction[cc_db$interaction$pathway_name %in% cc_pw,
                              c("ligand", "receptor")]
  genes <- sort(unique(c(cc_sel$ligand, cc_sel$receptor)))
  genes <- genes[nzchar(genes)]

  cells <- 240L
  feats <- c(genes, paste0("FILL", seq_len(60L)))
  grp <- rep(c("A", "B", "C"), length.out = cells)
  data <- matrix(rpois(length(feats) * cells, 2), nrow = length(feats),
                 dimnames = list(feats, paste0("k", seq_len(cells))))
  lig <- intersect(cc_sel$ligand, feats)
  rec <- intersect(cc_sel$receptor, feats)
  iA <- which(grp == "A")
  iB <- which(grp == "B")
  data[lig, iA] <- data[lig, iA] + rpois(length(lig) * length(iA), 25)
  data[rec, iB] <- data[rec, iB] + rpois(length(rec) * length(iB), 25)
  labels <- factor(grp)

  input <- cellchat_input_from_matrix(
    data = data, features = feats, labels = labels, species = "human",
    log_normalize = TRUE
  )

  res <- run_cellchat(input, seed = 1L, nboot = 5L)

  # INVARIANT DYNAMIQUE : le resultat n'est PAS un objet CellChat.
  expect_false(any(grepl("CellChat", class(res), fixed = TRUE)))
  expect_true(is.data.frame(res$canonical_table))
  expect_true(all(communication_contract_fields() %in%
                    colnames(res$canonical_table)))
  expect_identical(unique(res$canonical_table$source_method), "cellchat")
  expect_false(isTRUE(res$provenance$import_only))
  expect_identical(res$provenance$method, "cellchat")
  expect_identical(res$analysis_id, "sc-communication-engine")
  # moteur ephemeral : aucun champ du resultat ne porte la classe CellChat
  classes <- unique(vapply(res, function(x) class(x)[1L], character(1)))
  expect_false(any(grepl("^CellChat", classes)))
  # la graine et la base sont tracees
  expect_identical(res$engine$seed, 1L)
  expect_identical(res$engine$nboot, 5L)
  expect_true(nzchar(as.character(res$engine$database)))
  # CellChatDB n'expose pas de champ `version` : la version est lue dans
  # `interaction$version` (base livree MIXTE v1+v2). Elle doit etre tracée.
  expect_true(nzchar(as.character(res$engine$database_version)))
  # p_adjusted reste NA (jamais fabrique)
  expect_true(all(is.na(res$canonical_table$p_adjusted)))
  # le resultat passe la garde des consommateurs (voie commune).
  # NB : assert_communication_result() rend une liste BASE, pas un objet S3 —
  # la garde VALIDE, elle ne requalifie pas le type.
  expect_true(is.list(assert_communication_result(res)))
})

test_that("a run with no usable LR pair fails as no_interactions, not as a crash", {
  skip_if_not(cellchat_engine_available(), "CellChat absent (dependance GitHub)")

  # Jeu jouet MESURE : aucune paire ligand-recepteur surexprimee n'en ressort,
  # donc CellChat ne peut rien inferer. Sans la garde, computeCommunProb()
  # echoue sur « subscript out of bounds » — un indice de tableau, pas une
  # cause. L'etat `no_interactions` doit porter le diagnostic.
  set.seed(20260914)
  cells <- 120L
  genes <- c("TGFB1", "TGFBR1", "TGFBR2", "CD74", "APP", "MIF", "CD44",
             paste0("GENE", 1:40))
  data <- matrix(rpois(cells * length(genes), 3), nrow = length(genes),
                 dimnames = list(genes, paste0("c", seq_len(cells))))
  labels <- factor(rep(c("A", "B", "C"), length.out = cells))

  input <- cellchat_input_from_matrix(
    data = data, features = genes, labels = labels, species = "human",
    log_normalize = TRUE
  )

  err <- tryCatch(run_cellchat(input, seed = 1L, nboot = 5L),
                  error = function(e) e)
  expect_s3_class(err, "cellchat_engine_error")
  expect_identical(cellchat_engine_error_state(err), "no_interactions")
  expect_true(grepl("ligand", conditionMessage(err), fixed = TRUE))
})

test_that("parse_cellchat_object reads a REAL CellChat object (regression)", {
  skip_if_not(cellchat_engine_available(), "CellChat absent (dependance GitHub)")

  # REGRESSION. Jusqu'au 2026-09-15, parse_cellchat_object() lisait net$prob
  # comme [ligand, recepteur, "sender|receiver"] : une forme FICTIVE qu'aucune
  # version de CellChat ne produit. Tout objet reel (mesure : 3 x 3 x 109, et
  # 0/109 noms d'interaction ne contiennent '|') faisait donc echouer la route
  # en accusant le fichier de l'utilisateur. Aucun test ne construisait de
  # VRAI objet — ils passaient tous par un stub de la forme fictive. Celui-ci
  # en construit un (meme fixture que le parcours reel ci-dessus).
  set.seed(20260914)
  cc_env <- new.env(parent = emptyenv())
  utils::data(list = "CellChatDB.human", package = "CellChat", envir = cc_env)
  cc_db <- get("CellChatDB.human", envir = cc_env)
  cc_pw <- c("TGFb", "MIF", "MHC-I", "MHC-II", "GALECTIN", "CD99",
             "ANNEXIN", "VISFATIN", "COMPLEMENT", "CXCL")
  cc_sel <- cc_db$interaction[cc_db$interaction$pathway_name %in% cc_pw,
                              c("ligand", "receptor")]
  genes <- sort(unique(c(cc_sel$ligand, cc_sel$receptor)))
  genes <- genes[nzchar(genes)]

  cells <- 240L
  feats <- c(genes, paste0("FILL", seq_len(60L)))
  grp <- rep(c("A", "B", "C"), length.out = cells)
  mat <- matrix(rpois(length(feats) * cells, 2), nrow = length(feats),
                dimnames = list(feats, paste0("k", seq_len(cells))))
  lig <- intersect(cc_sel$ligand, feats)
  rec <- intersect(cc_sel$receptor, feats)
  iA <- which(grp == "A"); iB <- which(grp == "B")
  mat[lig, iA] <- mat[lig, iA] + rpois(length(lig) * length(iA), 25)
  mat[rec, iB] <- mat[rec, iB] + rpois(length(rec) * length(iB), 25)

  # group_by VOLONTAIREMENT different de "labels" : c'est le cas REEL (la
  # colonne d'identites d'un Seurat s'appelle celltype / seurat_clusters...).
  # Avec `group.by = input$group_by` ce test passait alors que le moteur
  # cassait en production ; il exerce desormais la vraie dissymetrie.
  input <- cellchat_input_from_matrix(
    data = mat, features = feats, labels = factor(grp), species = "human",
    group_by = "celltype", log_normalize = TRUE
  )
  object <- CellChat::createCellChat(object = input$data, meta = input$meta,
                                     group.by = cellchat_group_by_column(input))
  object@DB <- .cellchat_engine_db("human")$db
  object <- CellChat::subsetData(object)
  object <- CellChat::identifyOverExpressedGenes(object)
  object <- CellChat::identifyOverExpressedInteractions(object)
  object <- CellChat::computeCommunProb(object, nboot = 5L, seed.use = 1L)

  # La forme reelle EST la specification de l'extraction : on la re-assert ici
  # pour qu'un upgrade de CellChat qui la changerait casse CE test.
  prob <- methods::slot(object, "net")$prob
  expect_identical(length(dim(prob)), 3L)
  expect_identical(dimnames(prob)[[1L]], dimnames(prob)[[2L]])
  expect_false(any(grepl("|", dimnames(prob)[[3L]], fixed = TRUE)))

  parsed <- parse_cellchat_object(object, source_file = "real.rds")
  tab <- parsed$table
  expect_true(nrow(tab) > 0L)
  expect_true(all(communication_contract_fields() %in% colnames(tab)))
  expect_setequal(unique(tab$sender), c("A", "B", "C"))
  expect_setequal(unique(tab$receiver), c("A", "B", "C"))
  # ligand/receptor/pathway sont RESOLUS via @LR$LRsig, pas laisses a NA.
  expect_false(any(is.na(tab$ligand)))
  expect_false(any(is.na(tab$receptor)))
  expect_false(any(is.na(tab$pathway)))
  # p_adjusted reste NA sur les deux voies (jamais fabrique).
  expect_true(all(is.na(tab$p_adjusted)))
})

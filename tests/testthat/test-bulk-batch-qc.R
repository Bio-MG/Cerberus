# =============================================================================
# test-bulk-batch-qc.R — Diagnostics batch Bulk (roadmap Bulk V2, M1)
# =============================================================================
# Couvre : garde anti-counts-bruts, contrôle du plan d'expérience (batch ×
# condition, collinéarité via check_design_confounding réutilisé), décomposition
# de variance (voie variancePartition OU repli R pur — les deux déclarées),
# plafond mémoire de gènes déterministe, plots consommateurs purs.
# =============================================================================
source_project_file("R/core/validation.R")
source_project_file("R/plotting/theme.R")      # ts_theme() — resolveur de theme partage
source_project_file("R/bulk/bulk_helpers.R")
source_project_file("R/bulk/bulk_batch_qc.R")

# ggplot2 est attache par global.R dans l'app, mais PAS dans ce harnais.
# plot_bulk_varpart() prefixe ses propres appels (il doit rester autonome) ;
# en revanche plot_bulk_batch_scree() delegue a plot_scree_bulk(), fonction
# PREEXISTANTE qui appelle encore ggplot() nu — d'ou le library() ci-dessous.
# suppressPackageStartupMessages : evite le warning benigne "built under R
# 4.4.3" qui polluerait le decompte de la suite complete.
suppressWarnings(suppressPackageStartupMessages(library(ggplot2)))

.vst_like <- function(genes = 60, samples = 12, seed = 1) {
  set.seed(seed)
  m <- matrix(rnorm(genes * samples), genes, samples,
              dimnames = list(paste0("g", seq_len(genes)),
                              paste0("s", seq_len(samples))))
  m + 10  # amplitude continue bornée, typique d'une VST
}

.meta_bq <- function(n = 12, collinear = FALSE) {
  b <- if (collinear) rep(c("b1", "b2"), each = n / 2) else rep(c("b1", "b2"), length.out = n)
  data.frame(row.names = paste0("s", seq_len(n)),
             batch = b,
             condition = rep(c("A", "B"), each = n / 2))
}

test_that("surface publique figée", {
  expect_setequal(
    bulk_batch_qc_public_api(),
    c("bulk_batch_qc_public_api", "bulk_assert_transformed_matrix",
      "bulk_batch_design_check", "bulk_variance_partition",
      "plot_bulk_varpart", "plot_bulk_batch_scree")
  )
})

test_that("garde anti-counts-bruts : VST acceptée, counts Poissonniens refusés", {
  expect_no_error(bulk_assert_transformed_matrix(.vst_like()))
  # counts simulés lambda modéré (max ~600 — sous tout seuil d'amplitude naïf)
  set.seed(2)
  counts <- matrix(rpois(50 * 12, lambda = 300), 50, 12)
  err <- tryCatch(bulk_assert_transformed_matrix(counts, "test GSVA"),
                  error = function(e) e)
  expect_s3_class(err, "bulk_batch_qc_error")
  expect_identical(err$state, "raw_counts_rejected")
  expect_match(conditionMessage(err), "VST")
  # erreurs structurelles
  expect_error(bulk_assert_transformed_matrix(NULL),
               class = "bulk_batch_qc_error")
  e1 <- tryCatch(bulk_assert_transformed_matrix(matrix(1.5, 5, 1)), error = function(e) e)
  expect_identical(e1$state, "invalid_input")
})

test_that("contrôle du plan : équilibré silencieux, confondu détecté + message", {
  chk <- bulk_batch_design_check(.meta_bq(12), "batch", "condition")
  expect_false(chk$fully_collinear)
  expect_identical(chk$empty_cells, 0L)
  expect_length(chk$warning_messages, 0L)
  expect_identical(dim(chk$cross_table), c(2L, 2L))

  conf <- bulk_batch_design_check(.meta_bq(12, collinear = TRUE), "batch", "condition")
  expect_true(conf$fully_collinear)
  expect_match(conf$warning_messages[1], "ENTI[ÈE]REMENT collin[ée]aires")

  # batch sans condition : contrôle lot seul
  solo <- bulk_batch_design_check(.meta_bq(12), "batch")
  expect_identical(solo$n_condition_levels, NA_integer_)
  expect_false(solo$fully_collinear)
  # modalité unique de batch
  one <- bulk_batch_design_check(
    data.frame(row.names = paste0("s", 1:4), batch = "b1", condition = c("A", "A", "B", "B")),
    "batch", "condition")
  expect_match(one$warning_messages[1], "une seule modalit[ée]")
  # colonne inconnue -> erreur classée
  expect_error(bulk_batch_design_check(.meta_bq(6), "inexistant"),
               class = "bulk_batch_qc_error")
})

test_that("variance partition : résultat structuré, fractions bornées [0,1]", {
  vp <- bulk_variance_partition(.vst_like(30), .meta_bq(12), c("batch", "condition"))
  expect_true(vp$method %in% c("variancePartition::fitExtractVarPartModel", "pur_lm_partial_r2"))
  expect_identical(nrow(vp$var_part), 30L)
  expect_true(all(c("batch", "condition") %in% names(vp$var_part)))
  vals <- as.vector(as.matrix(vp$var_part))
  vals <- vals[is.finite(vals)]
  expect_true(all(vals >= 0 & vals <= 1))
  expect_identical(vp$n_genes_total, 30L)
  expect_identical(vp$n_genes_used, 30L)
  expect_match(vp$formula, "^~ batch \\+ condition$")
  expect_true(nzchar(vp$timestamp_utc))
})

test_that("variance partition : plafond mémoire déterministe (seed)", {
  big <- .vst_like(genes = 300, samples = 12, seed = 5)
  vp1 <- bulk_variance_partition(big, .meta_bq(12), "batch", max_genes = 40, seed = 7)
  vp2 <- bulk_variance_partition(big, .meta_bq(12), "batch", max_genes = 40, seed = 7)
  expect_identical(vp1$n_genes_total, 300L)
  expect_identical(vp1$n_genes_used, 40L)
  expect_identical(rownames(vp1$var_part), rownames(vp2$var_part))  # même seed, mêmes gènes
  vp3 <- bulk_variance_partition(big, .meta_bq(12), "batch", max_genes = 40, seed = 8)
  expect_false(identical(rownames(vp1$var_part), rownames(vp3$var_part)))
})

test_that("variance partition : erreurs classées", {
  expect_error(bulk_variance_partition(.vst_like(), .meta_bq(12), "inexistant"),
               class = "bulk_batch_qc_error")
  expect_error(bulk_variance_partition(.vst_like(10), .meta_bq(12), "batch",
                                       max_genes = 40, seed = 1)[1], NA)
  # covariable à modalité unique
  meta1 <- .meta_bq(12); meta1$batch <- "b1"
  e <- tryCatch(bulk_variance_partition(.vst_like(), meta1, "batch"), error = function(e) e)
  expect_s3_class(e, "bulk_batch_qc_error")
  # moins de 3 échantillons communs
  m2 <- .vst_like(genes = 10, samples = 2)
  expect_error(bulk_variance_partition(m2, .meta_bq(2), "batch"),
               class = "bulk_batch_qc_error")
})

test_that("repli R pur : fractions cohérentes, somme par gène <= 1", {
  expr <- .vst_like(8, 12, seed = 3)
  out <- .varpart_pure_r_fallback(expr, .meta_bq(12), c("batch", "condition"))
  expect_identical(nrow(out), 8L)
  expect_identical(colnames(out), c("batch", "condition", "Residuals"))
  expect_true(all(complete.cases(out)))
  expect_true(all(rowSums(out) <= 1 + 1e-8))
  expect_true(all(as.matrix(out) >= 0))
})

test_that("plots : consommateurs purs ggplot", {
  vp <- bulk_variance_partition(.vst_like(20), .meta_bq(12), "batch")
  expect_s3_class(plot_bulk_varpart(vp), "ggplot")
  expect_s3_class(plot_bulk_batch_scree(.vst_like(20)), "ggplot")
})

# =============================================================================
# Garde 236 — dependance optionnelle `variancePartition` (35e increment C10)
# =============================================================================
# Site : R/bulk/bulk_batch_qc.R:236 — `stop("absent")`, branche
#   `if (!requireNamespace("variancePartition", quietly = TRUE))` placee dans le
#   `tryCatch(..., error = function(e) ...)` de bulk_variance_partition().
#
# Les DEUX verdicts sont MESURES (recon du 35e lot), jamais supposes :
#
#   (1) JOIGNABILITE : CONDITIONNELLE, et le site est INJOIGNABLE en l'etat.
#       La branche n'est prise que si le paquet est ABSENT ; mesure du jour :
#       `requireNamespace("variancePartition", quietly = TRUE)` vaut TRUE
#       => sans bouchon, le garde ne tire PAS. Le test est rendu INDEPENDANT de
#       l'environnement en bouchonnant l'INTERROGATION (`requireNamespace`),
#       jamais le paquet : on ne bouchonne que ce qu'on OBSERVE.
#
#   (2) REMONTEE : la classe est AVALEE. Le gestionnaire `error = function(e)`
#       du tryCatch englobant RETOURNE le repli R pur au lieu de relancer
#       (`RELANCE = FALSE`, mesure par AST) => aucune erreur ne s'echappe => la
#       classe est INOBSERVABLE a l'execution => preuve = VERROU SOURCE, rendu
#       FALSIFIABLE par le test « le gestionnaire ne contient aucun stop() ».
#
# La classe retenue est celle du FICHIER (`bulk_batch_qc_error`, deja portee par
# les 10 autres sites convertis).
#
# 🔴 PAS de `state` sur ce site — decision MESUREE, pas un oubli. Le vocabulaire
# de `state` est celui du ROUTAGE du domaine : il est gele par
# BULK_BATCH_QC_CONTRACT.md §6 (« le module branche son affichage dessus ») et
# garde contre l'inflation par test-bulk-batch-qc-contract-freeze.R. Or cette
# erreur est AVALEE, donc jamais routable : y figer une valeur serait INERTE.
# Mesure : les occurrences de `missing_dependency` du depot vivent dans des
# gardes ESCAPANTES (bulk_gsva.R, bulk_wgcna.R, bulk_signatures.R,
# batch_correction.R, bulk_network.R, sc_communication_engine.R) — c'est
# l'OBSERVABILITE qui les distingue, ni le dossier ni le nom.
# =============================================================================

#' Le noeud `if (!requireNamespace("variancePartition", ...))` du fichier.
.bq_guard <- function() {
  p   <- ts_ast_parse("R/bulk/bulk_batch_qc.R")
  hit <- ts_ast_find(p, function(x) {
    if (!ts_ast_is_call_to(x, "if")) return(FALSE)
    l <- as.list(x)
    if (length(l) != 3L) return(FALSE)
    grepl("variancePartition", ts_ast_deparse(l[[2]]), fixed = TRUE)
  })
  if (is.null(hit)) {
    stop("garde `if (!requireNamespace(\"variancePartition\", ...))` introuvable",
         call. = FALSE)
  }
  hit
}

#' Le `stop(...)` du corps du garde (unique).
.bq_guard_stop <- function() {
  body <- as.list(.bq_guard())[[3]]
  if (ts_ast_is_call_to(body, "stop")) return(body)
  ts_ast_find(body, function(x) ts_ast_is_call_to(x, "stop"))
}

#' Classe / state / message du garde, lus SANS l'executer.
#'
#' ⚠️ On evalue l'ARGUMENT du `stop()` (`call[[2]]`), JAMAIS le `stop()` lui-meme :
#' `eval()` d'un appel `stop(...)` l'EXECUTE (§2ct.2). Lire la CONDITION
#' construite (et non la FORME du noeud) garde l'assertion valide dans les DEUX
#' etats — avant et apres conversion — donc le ROUGE reste lisible.
.bq_guard_condition <- function() {
  st  <- .bq_guard_stop()
  arg <- as.list(st)[[2]]
  v   <- eval(arg, envir = globalenv())
  if (inherits(v, "condition")) {
    list(class = class(v), state = v$state, message = conditionMessage(v))
  } else {
    list(class = character(0), state = NULL, message = as.character(v))
  }
}

#' Execute `code` avec `variancePartition` declare ABSENT (bouchon spy).
#'
#' Le bouchon est pose dans `globalenv()` et RESTAURE en sortie (idiome du depot :
#' `with_mocked_bindings(.env = globalenv())` ECHOUE ici, §2bz.3). Il ne peut pas
#' LEVER (il delegue a `base::requireNamespace` pour tout autre paquet) : il ne
#' contamine donc pas le canal temoin. `seen` est un ENVIRONNEMENT (semantique de
#' reference) : c'est le seul temoin NON VACUISTE que le garde a ete evalue.
.bq_without_variancePartition <- function(code, seen = NULL) {
  old <- get0("requireNamespace", envir = globalenv(), inherits = FALSE)
  assign("requireNamespace", function(package, ...) {
    if (!is.null(seen)) seen$pkgs <- c(seen$pkgs, package)
    if (identical(package, "variancePartition")) return(FALSE)
    base::requireNamespace(package, ...)
  }, envir = globalenv())
  on.exit({
    if (is.null(old)) rm("requireNamespace", envir = globalenv())
    else assign("requireNamespace", old, envir = globalenv())
  }, add = TRUE)
  code
}

test_that("garde 236 : temoin de traversee (le garde est evalue PUIS mene au repli)", {
  seen <- new.env()
  seen$pkgs <- character(0)

  vp <- .bq_without_variancePartition(
    bulk_variance_partition(.vst_like(20), .meta_bq(12), c("batch", "condition")),
    seen = seen)

  # (a) TEMOIN NON VACUISTE : l'interrogation a bien eu lieu.
  expect_true("variancePartition" %in% seen$pkgs)
  # (b) La chaine est FERMEE : corps du garde = { stop(...) } et rien d'autre.
  #     Garde evalue (a) + condition FAUSSE + corps reduit au SEUL stop() => le
  #     stop() a NECESSAIREMENT ete execute (sans quoi (a) serait un temoin mort).
  body  <- as.list(.bq_guard())[[3]]
  stmts <- if (ts_ast_is_call_to(body, "{")) as.list(body)[-1] else list(body)
  expect_length(stmts, 1L)
  expect_true(ts_ast_is_call_to(stmts[[1]], "stop"))
  # (c) Le repli declare a pris le relais, et le resultat reste bien forme.
  expect_identical(vp$method, "pur_lm_partial_r2")
  expect_true(any(grepl("indisponible", vp$warnings)))
  expect_identical(nrow(vp$var_part), 20L)
})

test_that("garde 236 : la classe est AVALEE — rien ne s'echappe (inobservable)", {
  seen <- new.env()
  seen$pkgs <- character(0)

  expect_no_error(
    .bq_without_variancePartition(
      bulk_variance_partition(.vst_like(20), .meta_bq(12), "batch"),
      seen = seen))
  # Le garde A tire (le bouchon rend FALSE) et pourtant AUCUNE erreur ne remonte :
  # c'est la justification du verrou source qui suit.
  expect_true("variancePartition" %in% seen$pkgs)
})

test_that("garde 236 : le gestionnaire englobant ne RELANCE pas (verrou source falsifiable)", {
  h <- ts_ast_trycatch_handler("R/bulk/bulk_batch_qc.R", ".varpart_pure_r_fallback")
  expect_null(ts_ast_find(h, function(x) ts_ast_is_call_to(x, "stop")))
})

test_that("garde 236 : la classe est gelee, le `state` est ABSENT (verrou source cible)", {
  cond <- .bq_guard_condition()
  expect_true("bulk_batch_qc_error" %in% cond$class)
  # 🔴 PAS de `state` — decision MESUREE, pas un oubli (cf. en-tete du bloc).
  # ⚠️ Assertion valide dans les DEUX etats (NULL avant comme apres conversion) :
  # elle GELE la decision, elle ne fabrique pas de rouge.
  expect_null(cond$state)
  expect_match(cond$message, "variancePartition", fixed = TRUE)
  # Queue du message : detecteur de TRONCATURE (C16) — un message multi-arguments
  # non passe par paste0() perdrait tout ce qui suit le 1er argument (§2bn).
  expect_match(cond$message, "BiocManager::install", fixed = TRUE)
  expect_gt(nchar(cond$message),
            nchar("bulk_variance_partition() : package 'variancePartition' absent"))
})

test_that("bulk_batch_qc.R ne contribue aucun signalement C10 (35e increment)", {
  expect_identical(ts_c10_sites("R/bulk/bulk_batch_qc.R"), integer(0))
})

# =============================================================================
# test-helpers_bulk.R — pure-function tests for helpers_bulk.R
# =============================================================================
# Scope: functions with no Shiny reactivity and no hard Bioconductor/DESeq2
# dependency (filtering, design validation, contrast-set/summary builders,
# palette resolvers). DESeq2/edgeR/limma-dependent functions (build_dds(),
# run_edger_de(), extract_deseq2_contrast(), get_vst_matrix(), getAllDE(),
# rankConsensus()) and plot builders needing ggplot2/ComplexHeatmap are out
# of scope here — they need a live statistical engine / graphics device and
# are better covered by an integration test with a real toy DESeq2 fit.
#
# NOTE (real finding, see PR description / handoff): bulk_role_colors() and
# bulk_annotation_colors() are defined TWICE in this codebase — once here in
# helpers_bulk.R (missing the requireNamespace() guard around viridisLite/
# RColorBrewer for bulk_role_colors(), which — because it's built via an
# eagerly-evaluated list() — throws on EVERY call, even palette="default",
# whenever viridisLite/RColorBrewer aren't installed) and once in
# R/palettes.R with a safe guard. app.R sources R/palettes.R LAST, so its
# safe copy currently wins and shadows the unsafe one in helpers_bulk.R —
# but the unsafe copy is dead, diverged code that will bite the first time
# someone reorders sourcing or calls it directly (e.g. from a script or a
# test). We source both files here, in app.R's real order, so this test
# suite exercises the ACTUAL deployed behavior — see tools/check_duplication.R
# for the static-analysis guard that catches this class of bug going forward.
# =============================================================================

source_project_file("R/core/io_helpers.R")   # defines %||%, sourced first in app.R
source_project_file("R/core/validation.R")     # canonical guards (app.R loads before bulk_helpers)
source_project_file("R/bulk/bulk_helpers.R")
# PLOT-S6c (P0) : build_dds() et run_bulk_de_dispatch() appellent la garde de
# counts de bulk_assert_raw_counts() (domaine correction de batch) — le
# fichier doit donc être chargé ici aussi.
source_project_file("R/bulk/batch_correction.R")
source_project_file("R/plotting/palettes.R")   # sourced last in app.R — wins on name clashes
source_project_file("R/plotting/datatable.R")   # PLOT-S3 : build_de_results_dt() -> ts_datatable()

# ---------------------------------------------------------------------------
# filter_bulk_counts()
# ---------------------------------------------------------------------------
test_that("filter_bulk_counts keeps genes passing both thresholds", {
  m <- matrix(
    c(20, 0, 0,     # gene1: total 20, but only 1 sample >=1 -> depends on min_samples
      5, 5, 5,      # gene2: total 15, all samples >=1
      0, 0, 0),     # gene3: all zero -> filtered
    nrow = 3, byrow = TRUE,
    dimnames = list(c("gene1", "gene2", "gene3"), c("s1", "s2", "s3"))
  )
  out <- filter_bulk_counts(m, min_count = 10, min_samples = 2, min_count_per_sample = 1)
  expect_true("gene2" %in% rownames(out))
  expect_false("gene3" %in% rownames(out))
  expect_false("gene1" %in% rownames(out))  # only 1 sample >= 1 count, min_samples=2
})

test_that("filter_bulk_counts errors when nothing passes (explicit French message)", {
  m <- matrix(0, nrow = 3, ncol = 2, dimnames = list(c("g1","g2","g3"), c("s1","s2")))
  # ASCII-only substring: French accented text can trip grepl() under a
  # non-UTF-8 locale (common on minimal CI images) — match the unaccented part.
  expect_error(filter_bulk_counts(m, min_count = 10), "ne passe le filtre")
})

test_that("filter_bulk_counts preserves column order/names", {
  m <- matrix(c(10, 10), nrow = 1, dimnames = list("g1", c("sB", "sA")))
  out <- filter_bulk_counts(m, min_count = 5, min_samples = 1, min_count_per_sample = 1)
  expect_identical(colnames(out), c("sB", "sA"))
})

# ---------------------------------------------------------------------------
# check_design_confounding()
# ---------------------------------------------------------------------------
test_that("check_design_confounding detects a fully confounded covariate", {
  meta <- data.frame(
    condition = c("KO","KO","WT","WT"),
    batch     = c("b1","b1","b2","b2"),   # each batch level maps to exactly one condition
    row.names = paste0("s", 1:4)
  )
  expect_true(check_design_confounding(meta, "condition", "batch"))
})

test_that("check_design_confounding returns FALSE for a balanced covariate", {
  meta <- data.frame(
    condition = c("KO","KO","WT","WT"),
    batch     = c("b1","b2","b1","b2"),   # each batch spans both conditions
    row.names = paste0("s", 1:4)
  )
  expect_false(check_design_confounding(meta, "condition", "batch"))
})

test_that("check_design_confounding returns FALSE for missing columns", {
  meta <- data.frame(condition = c("KO","WT"), row.names = c("s1","s2"))
  expect_false(check_design_confounding(meta, "condition", "not_a_column"))
})

# ---------------------------------------------------------------------------
# validate_bulk_design()
# ---------------------------------------------------------------------------
test_that("validate_bulk_design returns no problems for a clean 2x2 design", {
  meta <- data.frame(
    condition = c("KO","KO","WT","WT"),
    batch     = c("b1","b2","b1","b2"),
    row.names = paste0("s", 1:4)
  )
  problems <- validate_bulk_design(meta, "condition", "batch")
  expect_length(problems, 0)
})

test_that("validate_bulk_design flags a single-replicate group", {
  meta <- data.frame(condition = c("KO","WT","WT"), row.names = paste0("s", 1:3))
  problems <- validate_bulk_design(meta, "condition")
  expect_true(any(grepl("Groupe.s. avec un seul", problems)))
})

test_that("validate_bulk_design flags a single-level covariate", {
  meta <- data.frame(
    condition = c("KO","KO","WT","WT"),
    batch     = c("b1","b1","b1","b1"),
    row.names = paste0("s", 1:4)
  )
  problems <- validate_bulk_design(meta, "condition", "batch")
  expect_true(any(grepl("seule modalit", problems)))
})

test_that("validate_bulk_design flags a fully confounded covariate", {
  meta <- data.frame(
    condition = c("KO","KO","WT","WT"),
    batch     = c("b1","b1","b2","b2"),
    row.names = paste0("s", 1:4)
  )
  problems <- validate_bulk_design(meta, "condition", "batch")
  expect_true(any(grepl("confondue", problems)))
})

test_that("validate_bulk_design catches NA-covariate shrinking a group below n=2", {
  # condition_col alone looks fine (2 KO / 2 WT), but batch has an NA on one
  # WT sample -> complete-case WT count drops to 1.
  meta <- data.frame(
    condition = c("KO","KO","WT","WT"),
    batch     = c("b1","b2","b1", NA),
    row.names = paste0("s", 1:4)
  )
  problems <- validate_bulk_design(meta, "condition", "batch")
  expect_true(any(grepl("exclusion de", problems)))
})

test_that("validate_bulk_design flags missing (NA) condition values", {
  meta <- data.frame(condition = c("KO", NA, "WT", "WT"), row.names = paste0("s", 1:4))
  problems <- validate_bulk_design(meta, "condition")
  expect_true(any(grepl("valeur manquante", problems)))
})

test_that("validate_bulk_design is a no-op when condition_col is absent", {
  meta <- data.frame(x = 1:3, row.names = paste0("s", 1:3))
  expect_length(validate_bulk_design(meta, "not_there"), 0)
})

# ---------------------------------------------------------------------------
# .normalize_de_cols()
# ---------------------------------------------------------------------------
test_that(".normalize_de_cols fills missing standard columns", {
  df <- data.frame(logFC = c(1, -2), row.names = c("g1", "g2"))
  out <- .normalize_de_cols(df)
  expect_true(all(c("gene","log2FoldChange","pvalue","padj","baseMean") %in% colnames(out)))
  expect_identical(out$gene, c("g1", "g2"))
  expect_true(all(is.na(out$log2FoldChange)))
})

test_that(".normalize_de_cols computes baseMean from counts when missing", {
  df <- data.frame(gene = c("g1","g2"), log2FoldChange = c(1, -1),
                   pvalue = c(0.01, 0.02), padj = c(0.05, 0.06))
  counts <- matrix(c(10, 20, 30, 40), nrow = 2, dimnames = list(c("g1","g2"), c("s1","s2")))
  out <- .normalize_de_cols(df, counts_for_basemean = counts)
  # data.frame `$<-` assignment drops the names() attribute of the vector
  # being stored (standard R behavior) — compare unnamed.
  expect_equal(out$baseMean, unname(rowMeans(counts)))
})

test_that(".normalize_de_cols passes through NULL/empty unchanged", {
  expect_null(.normalize_de_cols(NULL))
  empty <- data.frame(gene = character(0))
  expect_identical(.normalize_de_cols(empty), empty)
})

# ---------------------------------------------------------------------------
# build_contrast_gene_sets()
# ---------------------------------------------------------------------------
test_that("build_contrast_gene_sets keeps only genes above both thresholds", {
  # build_contrast_gene_sets() requires >= 2 contrasts (stopifnot) — the
  # 2nd entry is a filler, assertions below only look at A_vs_B.
  ctr <- list(
    A_vs_B = data.frame(gene = c("g1","g2","g3","g4"),
                        log2FoldChange = c(2, -3, 0.1, 5),
                        padj = c(0.001, 0.001, 0.001, 0.2)),
    filler = data.frame(gene = "g99", log2FoldChange = 0, padj = 1)
  )
  sets <- build_contrast_gene_sets(ctr, lfc_thresh = 1, padj_thresh = 0.05)
  expect_setequal(sets$A_vs_B, c("g1", "g2"))
})

test_that("build_contrast_gene_sets direction_aware splits Up/Down into 2 sets", {
  ctr <- list(
    A_vs_B = data.frame(gene = c("g1","g2"), log2FoldChange = c(2, -3), padj = c(0.001, 0.001)),
    filler = data.frame(gene = "g99", log2FoldChange = 0, padj = 1)
  )
  sets <- build_contrast_gene_sets(ctr, lfc_thresh = 1, padj_thresh = 0.05, direction_aware = TRUE)
  expect_true(all(c("A_vs_B (Up)", "A_vs_B (Down)") %in% names(sets)))
  expect_identical(sets[["A_vs_B (Up)"]], "g1")
  expect_identical(sets[["A_vs_B (Down)"]], "g2")
})

test_that("build_contrast_gene_sets requires >= 2 contrasts", {
  expect_error(build_contrast_gene_sets(list(a = data.frame(gene="g1", log2FoldChange=1, padj=0.01))))
})

# ---------------------------------------------------------------------------
# summarize_contrasts_updown()
# ---------------------------------------------------------------------------
test_that("summarize_contrasts_updown returns a 0-row frame for an empty list", {
  out <- summarize_contrasts_updown(list())
  expect_equal(nrow(out), 0)
  expect_identical(colnames(out), c("Contraste","n_testes","n_sig","n_up","n_down","actif"))
})

test_that("summarize_contrasts_updown counts up/down correctly and flags the active contrast", {
  ctr <- list(
    A_vs_B = data.frame(log2FoldChange = c(2, -3, 0.1), padj = c(0.01, 0.01, 0.9)),
    C_vs_D = data.frame(log2FoldChange = c(1, 1),        padj = c(0.5, 0.5))
  )
  out <- summarize_contrasts_updown(ctr, lfc_thresh = 1, padj_thresh = 0.05, active_contrast = "A_vs_B")
  row_a <- out[out$Contraste == "A_vs_B", ]
  expect_equal(row_a$n_sig, 2)
  expect_equal(row_a$n_up, 1)
  expect_equal(row_a$n_down, 1)
  expect_true(row_a$actif)
  row_c <- out[out$Contraste == "C_vs_D", ]
  expect_equal(row_c$n_sig, 0)
  expect_false(row_c$actif)
})

# ---------------------------------------------------------------------------
# .default_manual_colors()
# ---------------------------------------------------------------------------
test_that(".default_manual_colors recycles the Okabe-Ito base palette", {
  cols <- .default_manual_colors(10)
  expect_length(cols, 10)
  expect_equal(cols[9], cols[1])   # recycled: position 9 wraps back to position 1
  expect_equal(cols[10], cols[2])
})

test_that(".default_manual_colors returns exactly n colors, all valid hex", {
  cols <- .default_manual_colors(3)
  expect_length(cols, 3)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols)))
})

# ---------------------------------------------------------------------------
# bulk_annotation_colors() — "default" and "manual" branches only (package-
# free; viridis/set2 branches are lazily evaluated by switch() and covered
# only when the optional packages are installed, see skip guard).
# ---------------------------------------------------------------------------
test_that("bulk_annotation_colors returns NULL for palette='default' (caller keeps its own default)", {
  expect_null(bulk_annotation_colors(c("A","B"), palette = "default"))
})

test_that("bulk_annotation_colors 'manual' uses supplied colors, falls back for missing levels", {
  cols <- bulk_annotation_colors(c("A","B","C"), palette = "manual",
                                 manual_colors = list(A = "#111111", C = "#333333"))
  expect_equal(unname(cols["A"]), "#111111")
  expect_equal(unname(cols["C"]), "#333333")
  expect_true(grepl("^#", cols["B"]))  # fell back to a default swatch, not NA
})

test_that("bulk_annotation_colors 'okabeito' needs no extra package", {
  cols <- bulk_annotation_colors(c("A","B"), palette = "okabeito")
  expect_length(cols, 2)
  expect_true(all(grepl("^#", cols)))
})

# ---------------------------------------------------------------------------
# bulk_role_colors() — see the file-header note: tested here via the REAL
# app.R sourcing order (helpers_bulk.R then R/palettes.R), so this exercises
# actual production behavior, not the shadowed/unsafe copy.
# ---------------------------------------------------------------------------
test_that("bulk_role_colors default preset has the 3 fixed semantic roles", {
  cols <- bulk_role_colors("default")
  expect_named(cols, c("Up", "Down", "NS"), ignore.order = TRUE)
})

test_that("bulk_role_colors 'manual' overrides only the supplied roles", {
  cols <- bulk_role_colors("manual", manual_colors = list(Up = "#ABCDEF"))
  expect_equal(unname(cols["Up"]), "#ABCDEF")
  expect_equal(unname(cols["Down"]), unname(bulk_role_colors("default")["Down"]))
})

test_that("bulk_role_colors falls back to 'default' for an unknown palette name", {
  expect_equal(bulk_role_colors("not_a_real_palette"), bulk_role_colors("default"))
})

# =============================================================================
# PLOT-S6c (P0) — counts non entiers : plus d'arrondi SILENCIEUX dans le DE
# =============================================================================
# Défaut mesuré (audit externe, STATUS §2an) : `round()` était appliqué sans
# trace sur le chemin DE — avec un simple warning dans build_dds(), et SANS
# aucun avertissement dans les branches edgeR / limma (DGEList(counts = round(...))).
# Les chiffres analysés n'étaient donc plus ceux fournis.
.plots6c_counts <- function(seed = 11, genes = 40, samples = 6) {
  set.seed(seed)
  m <- matrix(rpois(genes * samples, lambda = 200), genes, samples,
              dimnames = list(paste0("G", seq_len(genes)), paste0("s", seq_len(samples))))
  meta <- data.frame(condition = rep(c("A", "B"), each = samples %/% 2),
                     row.names = colnames(m))
  list(counts = m, meta = meta)
}

test_that("build_dds refuse des counts non entiers au lieu de les arrondir (P0)", {
  d <- .plots6c_counts()

  # counts entiers : inchangé (DESeqDataSet est un objet S4)
  expect_s4_class(build_dds(d$counts, d$meta, "~ condition", run_deseq = FALSE),
                  "DESeqDataSet")

  # quelques valeurs continues : REFUS CLASSÉ (avant : warning + arrondi muet)
  mixed <- d$counts
  mixed[1:3, 1] <- mixed[1:3, 1] + 0.4
  err <- tryCatch(build_dds(mixed, d$meta, "~ condition", run_deseq = FALSE),
                  error = function(e) e)
  expect_s3_class(err, "bulk_batch_correction_error")
  expect_identical(err$state, "not_raw_counts")

  # opt-in EXPLICITE : l'arrondi redevient possible, mais il est annoncé
  expect_warning(
    dds <- build_dds(mixed, d$meta, "~ condition", run_deseq = FALSE,
                     allow_non_integer = TRUE),
    "non-enti"
  )
  expect_true(all(DESeq2::counts(dds) == round(mixed)))
})

test_that("run_bulk_de_dispatch (edgeR/limma) refuse aussi les counts non entiers (P0)", {
  d <- .plots6c_counts()
  mixed <- d$counts
  mixed[1:2, 2] <- mixed[1:2, 2] + 0.25

  for (engine in c("edger", "limma")) {
    err <- tryCatch(
      run_bulk_de_dispatch(engine, mixed, d$meta, "condition", "B", "A"),
      error = function(e) e)
    expect_s3_class(err, "bulk_batch_correction_error")
    expect_identical(err$state, "not_raw_counts")
  }
  # des counts entiers passent toujours
  expect_true(is.data.frame(
    run_bulk_de_dispatch("edger", d$counts, d$meta, "condition", "B", "A")))
})

# ---------------------------------------------------------------------------
# C10 — les erreurs du moteur DE portent la classe bulk_de_error
# ---------------------------------------------------------------------------
test_that("bulk_helpers : les stop() du moteur DE sont CLASSEES bulk_de_error", {
  # P0 : ces assertions DOIVENT echouer AVANT la conversion — un `stop()` nu
  # leve un simpleError, pas une condition classee. Sans ce rouge prealable,
  # elles ne prouveraient rien (CONVENTIONS.md §7 : forme attendue).
  # Elles bornent la conversion des 19 sites de `R/bulk/bulk_helpers.R` : les
  # 3 sites DEJA convertis portaient `bulk_de_error`, les 19 autres non.
  # RegEx ASCII seulement (cf. l'avertissement sur la locale, plus haut).
  m <- matrix(0, nrow = 3, ncol = 2, dimnames = list(c("g1", "g2", "g3"), c("s1", "s2")))
  expect_error(filter_bulk_counts(m, min_count = 10), "ne passe le filtre",
               class = "bulk_de_error")

  vst1 <- matrix(c(1, 2), nrow = 1, dimnames = list("g1", c("s1", "s2")))
  expect_error(plot_heatmap_bulk(vst1, genes = "g1", metadata = data.frame()),
               "Au moins 2 g", class = "bulk_de_error")

  one_col <- matrix(1, nrow = 2, dimnames = list(c("g1", "g2"), "s1"))
  expect_error(plot_sample_correlation_heatmap(one_col),
               "Au moins 2", class = "bulk_de_error")

  d <- .plots6c_counts()
  expect_error(run_bulk_de_dispatch("deseq2", d$counts, d$meta, "condition", "B", "A"),
               "DESeqDataSet manquant", class = "bulk_de_error")
  expect_error(run_bulk_de_dispatch("moteur_inconnu", d$counts, d$meta, "condition", "B", "A"),
               "non support", class = "bulk_de_error")
})

# ---------------------------------------------------------------------------
# P0 — un message a PLUSIEURS arguments ne doit PAS etre TRONQUE
# ---------------------------------------------------------------------------
test_that("bulk_helpers : un message multi-arguments n'est pas TRONQUE par errorCondition", {
  # Regression MESUREE le 2026-09-17 : errorCondition(message, ...) n'AGREGE PAS
  # ses arguments supplementaires — ils deviennent des champs de la condition,
  # pas du message :
  #   stop("A : ", "B", " fin")                   -> "A : B fin"
  #   errorCondition("A : ", "B", " fin", class=)  -> "A : "   <-- TRONQUE
  # Les 3 sites de R/bulk/bulk_helpers.R dont le message tient en PLUSIEURS
  # arguments doivent donc passer par paste0(). Ces assertions etaient ROUGES
  # avant le correctif : le message s'arretait a la 1re portion.
  # RegEx ASCII seulement (cf. l'avertissement sur la locale, plus haut).
  venn <- list(a = "1", b = "2", c = "3", d = "4", e = "5")
  e1 <- tryCatch(plot_venn_contrasts(venn), error = function(e) e)
  expect_s3_class(e1, "bulk_de_error")
  expect_true(grepl("avez 5", conditionMessage(e1), fixed = TRUE))
  expect_true(grepl("UpSet", conditionMessage(e1), fixed = TRUE))

  d <- .plots6c_counts()
  e2 <- tryCatch(
    run_bulk_de_dispatch("moteur_inconnu", d$counts, d$meta, "condition", "B", "A"),
    error = function(e) e
  )
  expect_s3_class(e2, "bulk_de_error")
  expect_true(grepl("moteur_inconnu", conditionMessage(e2), fixed = TRUE))
})

# ---------------------------------------------------------------------------
# bulk_de_shrinkage_state() — is the PUBLISHED log2FoldChange actually SHRUNK?
# ---------------------------------------------------------------------------
# WHY THIS EXISTS. `extract_deseq2_contrast()` records the shrinkage outcome in
# three attributes on the result it returns (`shrunk`, `shrunk_method`,
# `shrunk_reason`; see R/bulk/bulk_helpers.R ~l.323-349). Nothing on the drive
# protocol published them, so a snapshot reported
#
#     n_significant = <n>   under   convention = "padj < 0.05 & |log2FoldChange| > 1"
#
# and the `|log2FoldChange| > 1` half of that convention is evaluated on
# WHATEVER the LFC column happens to hold — shrunken or not. The count is
# correct either way; its MEANING is not, and nothing said which.
#
# 🔴 THIS HOST MAKES IT ACUTE, and that is measured, not assumed:
# `apeglm` is NOT installed (the lockfile lists it, the library does not have
# it — the known hermeticity gap). For a `~condition` design `use_coef` is TRUE,
# so the branch that normally runs asks for `type = "apeglm"`, finds no
# package, and leaves the table UNSHRUNK while warning. So on this machine the
# default run publishes a "significant" count computed on raw MLE log2FC, under
# a convention that reads as if it were computed on shrunken values.

.shr_frame <- function(shrunk, method = NA_character_, reason = NA_character_) {
  d <- data.frame(gene = c("g1", "g2"), padj = c(0.01, 0.2),
                  log2FoldChange = c(2, 0.1))
  attr(d, "shrunk")        <- shrunk
  attr(d, "shrunk_method") <- method
  attr(d, "shrunk_reason") <- reason
  d
}

# 🔴 LA FONCTION N'EXISTE PAS ENCORE, et c'est le ROUGE qui doit le montrer.
# Un appel direct d'un symbole absent lève « could not find function » : le
# fichier sort en ERROR, et un ROUGE qui PLANTE dit « le test est cassé », pas
# « la fonctionnalité manque » (CONVENTIONS.md, jalon §2dn). Le garde transforme
# l'absence en un FAIL net portant sur le CONTRAT, et laisse les assertions
# suivantes s'executer des que la fonction existe — donc le vert prouve le
# contrat entier, pas seulement son premier maillon.
.shr <- function(res) {
  if (!exists("bulk_de_shrinkage_state", mode = "function")) return(NULL)
  bulk_de_shrinkage_state(res)
}

test_that("bulk_de_shrinkage_state : la fonction existe et rend un triple nomme", {
  expect_true(exists("bulk_de_shrinkage_state", mode = "function"),
              info = "la derivation de l'etat du shrinkage n'est pas ecrite")
  st <- .shr(.shr_frame(TRUE, "apeglm"))
  expect_true(is.list(st))
  expect_named(st, c("shrunk", "shrink_requested", "shrink_method"))
})

test_that("bulk_de_shrinkage_state : ABSENT is not FALSE (edgeR/limma shrink nothing)", {
  # 🔴 The honesty property, and the one most likely to be got wrong. edgeR and
  # limma do not shrink, and their result frames carry NO `shrunk` attribute at
  # all. Publishing `shrunk = FALSE` there would assert "we tried and did not
  # shrink"; the truth is "this engine has no shrinkage step". Those are
  # different, and only the second one is a fact.
  st <- .shr(NULL)
  expect_null(st$shrunk)
  expect_null(st$shrink_requested)
  expect_null(st$shrink_method)

  bare <- data.frame(gene = "g1", padj = 0.01, log2FoldChange = 2)
  st2 <- .shr(bare)
  expect_null(st2$shrunk, "a frame with no shrunk attribute is NOT a FALSE")
  expect_null(st2$shrink_requested)
  expect_null(st2$shrink_method)
})

test_that("bulk_de_shrinkage_state : the DESeq2 outcomes are told apart", {
  # Requested AND applied — the apeglm path.
  a <- .shr(.shr_frame(TRUE, "apeglm"))
  expect_identical(a$shrunk, TRUE)
  expect_identical(a$shrink_requested, TRUE)
  expect_identical(a$shrink_method, "apeglm")

  # Requested AND applied — the OTHER shrinker, `normal`. Without this case a
  # hardcoded `shrink_method <- "apeglm"` would satisfy every assertion above:
  # `apeglm` is the only method the other cases ever carry, and `use_coef` is
  # TRUE for a plain `~condition` design, so `normal` is reached only when the
  # coefficient is ABSENT from `resultsNames()`. It is a real branch, so it is
  # measured here rather than assumed unreachable.
  nrm <- .shr(.shr_frame(TRUE, "normal"))
  expect_identical(nrm$shrunk, TRUE)
  expect_identical(nrm$shrink_requested, TRUE)
  expect_identical(nrm$shrink_method, "normal")
  expect_false(identical(a$shrink_method, nrm$shrink_method))

  # Requested, NOT applied (apeglm absent, or lfcShrink raised). `shrunk_reason`
  # is NON-NA on both failure paths, so "reason present" is what proves the
  # user ASKED for shrinkage and did not get it. That is the case the count is
  # most misleading in, and it must be distinguishable from "never asked".
  b <- .shr(.shr_frame(FALSE, NA_character_, "apeglm absent"))
  expect_identical(b$shrunk, FALSE)
  expect_identical(b$shrink_requested, TRUE)
  expect_null(b$shrink_method, "no method is named when nothing was applied")

  # NOT requested at all: `shrink = FALSE` skips the whole block, so `shrunk`
  # is FALSE and `shrunk_reason` is still NA. FALSE/FALSE is the only pair
  # that means "the box was unticked" — and it is a legitimate state, not an
  # absence.
  c3 <- .shr(.shr_frame(FALSE))
  expect_identical(c3$shrunk, FALSE)
  expect_identical(c3$shrink_requested, FALSE)
  expect_null(c3$shrink_method)

  # 🔴 The three DESeq2 outcomes must NOT collapse into one another — and the
  # claim is about the PAIR, not about `shrink_requested` alone. A first version
  # asserted `a$shrink_requested != b$shrink_requested`; that is WRONG, and the
  # green run said so: `a` (applied) and `b` (asked, failed) were BOTH requested,
  # so what separates them is `shrunk`, not the request. The three states are
  #
  #   applied            -> (TRUE,  TRUE)
  #   asked, not applied -> (FALSE, TRUE)
  #   never asked        -> (FALSE, FALSE)
  #
  # and the point of publishing two fields is exactly that no single one of them
  # separates all three.
  pair <- function(s) list(s$shrunk, s$shrink_requested)
  expect_identical(pair(a), list(TRUE,  TRUE))
  expect_identical(pair(b), list(FALSE, TRUE))
  expect_identical(pair(c3), list(FALSE, FALSE))
  expect_false(identical(pair(a), pair(b)))
  expect_false(identical(pair(b), pair(c3)))
  expect_false(identical(pair(a), pair(c3)))
})

test_that("bulk_de_shrinkage_state : bounded scalars only, never the frame or a row value", {
  # COUNTS AND FLAGS ONLY. The probe runs inside the poller's reactive beat, so
  # returning anything row-shaped would enrol the DE result in the poller's
  # dependency set — and a gene name is user data.
  st <- .shr(.shr_frame(TRUE, "apeglm"))
  expect_named(st, c("shrunk", "shrink_requested", "shrink_method"))
  for (v in st) expect_true(is.atomic(v) && length(v) <= 1L)
  expect_false(any(grepl("g1|g2", unlist(st), ignore.case = TRUE)))
})

test_that("bulk_de_shrinkage_state : agrees with a REAL DESeq2 fit on this host", {
  skip_if_not_installed("DESeq2")
  set.seed(1)
  ng <- 400L
  counts <- matrix(rnbinom(ng * 6L, mu = 100, size = 3), nrow = ng,
                   dimnames = list(paste0("g", seq_len(ng)),
                                   paste0("s", 1:6)))
  # a real signal, so the fit is not degenerate
  counts[1:40, 4:6] <- counts[1:40, 4:6] * 8L
  meta <- data.frame(condition = rep(c("ctrl", "treat"), each = 3L),
                     row.names = paste0("s", 1:6))

  # 🔴 `run_bulk_de_dispatch("deseq2", ...)` does NOT build a dds from counts: the
  # deseq2 branch REFUSES a NULL `dds` ("DESeqDataSet manquant", l.632). So a
  # first version of this test called it that way and the block ERRORED — a red
  # that says "the test is broken", not "the feature is missing". The dds is
  # built and fitted here, exactly as `mod_bulk_de_run.R:386` does it.
  dds <- build_dds(counts, meta, design_formula = "~ condition", run_deseq = TRUE)
  expect_s4_class(dds, "DESeqDataSet")

  run <- function(shrink) {
    suppressWarnings(
      run_bulk_de_dispatch("deseq2", counts, meta, "condition", "treat", "ctrl",
                           dds = dds, shrink = shrink))
  }
  asked  <- run(TRUE)
  untick <- run(FALSE)

  # The attributes really are on the returned frame, and are booleans/strings —
  # a contract this test now pins instead of assuming.
  expect_true("shrunk" %in% names(attributes(asked)))
  expect_type(attr(asked, "shrunk"), "logical")

  sa <- .shr(asked)
  su <- .shr(untick)
  # On THIS host `apeglm` is absent, so asking produces `shrunk = FALSE` WITH a
  # reason. If a future environment installs it, the two states split the other
  # way; either way `shrink_requested` must be TRUE for the asked run and FALSE
  # for the unticked one. Asserting the REQUEST rather than the outcome is what
  # makes this test portable.
  expect_identical(sa$shrink_requested, TRUE, "shrink = TRUE was requested")
  expect_identical(su$shrink_requested, FALSE, "shrink = FALSE was not requested")
  expect_identical(sa$shrunk, FALSE, "apeglm is absent on this host, so nothing is shrunk")
  expect_null(sa$shrink_method)
  # and the outcome is the same in both cases — which is exactly why the flag
  # has to travel beside the count.
  expect_identical(attr(asked, "shrunk"), attr(untick, "shrunk"))
})

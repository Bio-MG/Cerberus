# =============================================================================
# test-plot-theme.R — PLOT-S1 : resolveur de theme partage ts_theme()
# =============================================================================
# Ce fichier gele le contrat du resolveur partage (R/plotting/theme.R) ET
# l'invariant "zero changement visuel" de la migration :
#   * ts_theme() rend EXACTEMENT le meme objet que les theme_*() nus de
#     ggplot2 pour base_size = 11 (defaut de ggplot2) ;
#   * chaque site migre conserve son base_size historique (12/13/15) ;
#   * un choix inconnu / NA / NULL retombe sur "minimal" SANS erreur ;
#   * aucune occurrence nue de theme_minimal()/theme_bw()/theme_classic()/
#     theme_void() ne subsiste dans les fichiers migres (garde anti-regression,
#     meme esprit que le test STAT-Q4) ;
#   * les fonctions Bulk migrees acceptent theme_choice / base_size.
# =============================================================================

source_project_file("R/core/io_helpers.R")     # %||% (sourced first in app.R)
# D2 M4 (2026-10-25) : R/plotting vit desormais dans l'arbre du paquet
# (tools/pkg_skeleton/R/) — charge via load_project_pkg() (cf. helper-source.R).
load_project_pkg()
source_project_file("R/bulk/bulk_helpers.R")

# bulk_helpers.R builds plots with BARE ggplot()/aes()/geom_*() calls (global.R
# attaches ggplot2 in the app) -> the test must attach it too.
library(ggplot2)

# Fichiers migres par PLOT-S1 (Spatial : remplacement mecanique, valeurs
# historiques conservees). Volontairement EXCLUS :
#   - R/bulk/bulk_batch_qc.R      (WIP non suivi)
#   - R/bulk/bulk_report_engine.R (theme a l'interieur d'un script R genere)
.plot_s1_migrated_files <- c(
  "R/sc/sc_plotting.R",
  "R/bulk/bulk_helpers.R",
  "R/spatial/spatial_export.R",
  "modules/spatial/mod_spatial_lr.R",
  "modules/spatial/mod_spatial_multi.R",
  "modules/spatial/mod_spatial_niche.R",
  "modules/spatial/mod_spatial_qc.R",
  "modules/spatial/mod_spatial_viz.R",
  "modules/spatial/deconv/mod_spatial_deconv_outputs.R",
  "modules/spatial/deconv/mod_spatial_deconv_refviz.R"
)

# ---------------------------------------------------------------------------
# 1. Contrat du resolveur
# ---------------------------------------------------------------------------
test_that("ts_theme returns a ggplot theme for every declared choice", {
  for (ch in TS_THEME_CHOICES) {
    th <- ts_theme(ch)
    expect_true(inherits(th, "theme"), info = ch)
    expect_equal(th$text$size, 11, info = ch)
  }
})

test_that("ts_theme defaults reproduce ggplot2's own defaults EXACTLY (zero visual change)", {
  # base_size 11 IS ggplot2's default => migrating a bare theme_minimal() to
  # ts_theme("minimal") must be a byte-identical theme object.
  expect_identical(ts_theme(),                       ggplot2::theme_minimal())
  expect_identical(ts_theme("minimal"),              ggplot2::theme_minimal())
  expect_identical(ts_theme("classic"),              ggplot2::theme_classic())
  expect_identical(ts_theme("bw"),                   ggplot2::theme_bw())
  expect_identical(ts_theme("void"),                 ggplot2::theme_void())
  # explicit 11 == default
  expect_identical(ts_theme("minimal", 11),          ggplot2::theme_minimal())
})

test_that("ts_theme propagates base_size", {
  expect_equal(ts_theme("minimal", 12)$text$size, 12)
  expect_equal(ts_theme("void",    15)$text$size, 15)
  expect_equal(ts_theme("bw",      13)$text$size, 13)
  expect_identical(ts_theme("minimal", 12), ggplot2::theme_minimal(base_size = 12))
})

test_that("an unknown / NA / NULL / length-0 choice falls back to minimal WITHOUT error", {
  expect_silent(th <- ts_theme("nope"))
  expect_identical(th, ggplot2::theme_minimal())
  expect_identical(ts_theme(NA_character_), ggplot2::theme_minimal())
  expect_identical(ts_theme(NULL),          ggplot2::theme_minimal())
  expect_identical(ts_theme(character(0)),  ggplot2::theme_minimal())
  # base_size defensif
  expect_equal(ts_theme("minimal", NA_real_)$text$size, 11)
  expect_equal(ts_theme("minimal", NULL)$text$size,     11)
})

test_that("the declared constants match the frozen contract", {
  expect_identical(TS_THEME_CHOICES, c("minimal", "classic", "bw", "void"))
  expect_identical(TS_THEME_DEFAULT, "minimal")
  expect_identical(TS_BASE_SIZE_DEFAULT, 11)
  # "fdr"-style trap avoided: no duplicated / alias choice
  expect_false(anyDuplicated(TS_THEME_CHOICES) > 0)
})

# ---------------------------------------------------------------------------
# 2. Garde anti-regression : plus de theme_* nu dans les fichiers migres
# ---------------------------------------------------------------------------
test_that("no bare theme_minimal()/theme_bw()/theme_classic()/theme_void() remains in migrated files", {
  pat <- "theme_minimal\\(|theme_bw\\(|theme_classic\\(|theme_void\\("
  for (f in .plot_s1_migrated_files) {
    full <- file.path(ts_project_root(), f)
    expect_true(file.exists(full), info = f)
    txt <- readLines(full, warn = FALSE)
    hits <- grep(pat, txt)
    expect_equal(length(hits), 0L,
                 info = paste0(f, " still has bare theme_*() at line(s): ",
                               paste(hits, collapse = ", ")))
    # ... and the shared resolver IS used instead
    expect_true(any(grepl("ts_theme\\(", txt)), info = f)
  }
})

# ---------------------------------------------------------------------------
# 3. Les fonctions Bulk migrees acceptent theme_choice / base_size
# ---------------------------------------------------------------------------
.bulk_res_df <- data.frame(
  gene            = paste0("g", 1:20),
  log2FoldChange  = c(seq(-3, 3, length.out = 20)),
  padj            = c(seq(0.001, 0.2, length.out = 20)),
  baseMean        = c(seq(10, 200, length.out = 20)),
  stringsAsFactors = FALSE
)

test_that("plot_volcano_bulk honours theme_choice / base_size", {
  p_min  <- plot_volcano_bulk(.bulk_res_df)
  p_void <- plot_volcano_bulk(.bulk_res_df, theme_choice = "void")
  p_big  <- plot_volcano_bulk(.bulk_res_df, base_size = 16)
  expect_s3_class(p_min, "ggplot")
  # the plot also adds its own theme(plot.title=...), so $theme is the MERGED
  # theme -> assert on the elements the resolver controls, not on identical().
  expect_equal(p_min$theme$text$size, 11)
  expect_equal(p_big$theme$text$size, 16)
  # void is distinguishable from minimal by its blank axis text
  expect_true(inherits(p_void$theme$axis.text, "element_blank"))
  expect_false(inherits(p_min$theme$axis.text, "element_blank"))
})

test_that("plot_ma_bulk honours theme_choice / base_size", {
  p <- plot_ma_bulk(.bulk_res_df, theme_choice = "bw", base_size = 13)
  expect_s3_class(p, "ggplot")
  expect_equal(p$theme$text$size, 13)
})

test_that("plot_bulk_pca honours theme_choice / base_size", {
  set.seed(1)
  m  <- matrix(rnorm(50 * 6), nrow = 50, dimnames = list(paste0("g", 1:50), paste0("s", 1:6)))
  md <- data.frame(row.names = colnames(m))
  p  <- plot_bulk_pca(m, md, theme_choice = "classic", base_size = 14)
  expect_s3_class(p, "ggplot")
  expect_equal(p$theme$text$size, 14)
})

test_that("plot_updown_barchart / plot_scree_bulk keep their historical base_size = 12", {
  smry <- data.frame(Contraste = c("A", "B"), n_up = c(3L, 5L), n_down = c(1L, 2L),
                     stringsAsFactors = FALSE)
  p1 <- plot_updown_barchart(smry)
  expect_equal(p1$theme$text$size, 12)
  set.seed(2)
  m  <- matrix(rnorm(30 * 8), nrow = 30, dimnames = list(paste0("g", 1:30), paste0("s", 1:8)))
  p2 <- plot_scree_bulk(m)
  expect_equal(p2$theme$text$size, 12)
})

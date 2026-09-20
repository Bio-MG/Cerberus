# =============================================================================
# test-spatial-plotting.R — les helpers PURS de R/spatial/spatial_plotting.R
# =============================================================================
# 42ᵉ incrément (§2db). Premier lot C9 choisi sur la MESURE de COUVERTURE, et
# non sur la liste des fichiers signalés.
#
# 🔴 MESURE DU 2026-09-20 : les **10** fonctions de ce fichier n'étaient citées
# dans **AUCUN** fichier de `tests/` (vérifié fonction par fonction). C'est le
# plus gros trou de couverture du dépôt : `R/spatial/spatial_plotting.R` est le
# seul fichier de `R/` signalé par C9 dont **aucune** fonction n'est exercée —
# les 15 autres fichiers signalés sont, eux, **sourcés** par au moins un test
# (§2da.5). ⇒ Écrire un test éponyme pour ceux-là serait bureaucratique ;
# écrire celui-ci bouche un VRAI trou.
#
# Le fichier est en revanche très testable : **10 fonctions pures**, aucun
# `stop()`, aucune réactivité Shiny, aucune dépendance hors `grDevices` (le
# `plotly`/`ggplot2` annoncé en tête n'est utilisé par AUCUNE de ces 10
# fonctions — elles fabriquent des listes et des chaînes). Aucun mock requis.
#
# Les attentes ci-dessous sont MESURÉES avant d'être écrites (sonde hors dépôt),
# jamais déduites du code.
# =============================================================================
source_project_file("R/spatial/spatial_plotting.R")

# Overlay histologique VALIDE, réutilisé par plusieurs directions.
.hist_ok <- function() {
  list(
    rgba = array(0.5, c(3, 4, 4)),
    bounds = list(x = c(10, 30), y = c(5, 25))
  )
}

# =============================================================================
# 1. get_available_resolutions — quatre branches, dans un ORDRE qui compte
# =============================================================================
test_that("get_available_resolutions couvre ses quatre branches dans l'ordre", {
  expect_identical(get_available_resolutions(NULL), character(0))

  # Cas 1 — une liste `images` : ses NOMS. ⚠️ Elle PRIME sur le cas 3.
  h <- list(images = list(lowres = 1, hires = 2), lowres = 99)
  expect_identical(get_available_resolutions(h), c("lowres", "hires"))

  # Cas 2 — éléments de premier niveau portant un raster ou un rgba.
  expect_identical(
    get_available_resolutions(list(A = list(raster = 1), B = list(rgba = 2), C = 3)),
    c("A", "B")
  )

  # Cas 3 — noms connus, en dernier recours seulement.
  expect_identical(get_available_resolutions(list(lowres = 1, autre = 2)), "lowres")

  # Aucune branche : vecteur VIDE, jamais NULL ni erreur.
  expect_identical(get_available_resolutions(list(foo = 1)), character(0))
  expect_identical(get_available_resolutions(list(images = "pas une liste")), character(0))
})

# =============================================================================
# 2. make_plotly_histology_image — quatre gardes, puis le MIROIR en y
# =============================================================================
test_that("make_plotly_histology_image rend NULL sur chacune de ses gardes", {
  b <- list(x = c(10, 30), y = c(5, 25))
  expect_null(make_plotly_histology_image(NULL))
  expect_null(make_plotly_histology_image(list(bounds = b)))                 # pas de data_uri
  expect_null(make_plotly_histology_image(list(data_uri = "", bounds = b)))  # data_uri vide
  expect_null(make_plotly_histology_image(list(data_uri = "data:x")))        # pas de bounds
})

test_that("make_plotly_histology_image applique le miroir en y (points traces en -y)", {
  b <- list(x = c(10, 30), y = c(5, 25))
  im <- make_plotly_histology_image(list(data_uri = "data:image/png;base64,AA", bounds = b))
  expect_type(im, "list")
  expect_identical(im$source, "data:image/png;base64,AA")
  expect_identical(im$xref, "x")
  expect_identical(im$yref, "y")
  expect_identical(im$x, 10)
  # ⚠️ Le bord INFÉRIEUR de l'image vaut -ymax : les points sont tracés en -y.
  expect_identical(im$y, -25)
  expect_identical(im$sizex, 20)
  expect_identical(im$sizey, 20)
  expect_identical(im$xanchor, "left")
  expect_identical(im$yanchor, "bottom")
  expect_identical(im$sizing, "stretch")
  expect_identical(im$layer, "below")
  expect_identical(im$opacity, 0.7)
  # L'opacité est un ARGUMENT, pas une constante.
  im2 <- make_plotly_histology_image(list(data_uri = "d", bounds = b), opacity = 0.3)
  expect_identical(im2$opacity, 0.3)
})

# =============================================================================
# 3. compute_spatial_ranges — fenêtre des points, étendue par l'histologie
# =============================================================================
test_that("compute_spatial_ranges trace en -y et n'etend PAS la fenetre par defaut", {
  df <- data.frame(x = c(1, 5), y = c(2, 8))
  r <- compute_spatial_ranges(df)
  expect_identical(r$x, c(1, 5))
  expect_identical(r$y, c(-8, -2))
  # show_hist = FALSE : l'overlay est présent mais n'étend PAS la fenêtre.
  ov <- list(bounds = list(x = c(10, 30), y = c(5, 25)))
  expect_identical(compute_spatial_ranges(df, ov, show_hist = FALSE)$x, c(1, 5))
  expect_identical(compute_spatial_ranges(df, ov, show_hist = FALSE)$y, c(-8, -2))
})

test_that("compute_spatial_ranges etend la fenetre quand l'histologie est DEMANDEE", {
  df <- data.frame(x = c(1, 5), y = c(2, 8))
  ov <- list(bounds = list(x = c(10, 30), y = c(5, 25)))
  r <- compute_spatial_ranges(df, ov, show_hist = TRUE)
  expect_identical(r$x, c(1, 30))
  expect_identical(r$y, c(-25, -2))
})

test_that("compute_spatial_ranges IGNORE des bornes degenerees (correctif d'audit 3.12)", {
  df <- data.frame(x = c(1, 5), y = c(2, 8))
  # Un changement de résolution produit transitoirement des bornes de largeur
  # nulle ; les injecter dans coord_fixed() faisait planter l'aperçu PNG
  # statique (graphics::plot.new). On les ignore pour cette image-là.
  ov_deg <- list(bounds = list(x = c(7, 7), y = c(3, 3)))
  expect_identical(compute_spatial_ranges(df, ov_deg, show_hist = TRUE)$x, c(1, 5))
  expect_identical(compute_spatial_ranges(df, ov_deg, show_hist = TRUE)$y, c(-8, -2))
  # Bornes non finies : même repli.
  ov_nan <- list(bounds = list(x = c(NaN, 3), y = c(1, 2)))
  expect_identical(compute_spatial_ranges(df, ov_nan, show_hist = TRUE)$x, c(1, 5))
  # Pas de bounds du tout : même repli.
  expect_identical(compute_spatial_ranges(df, list(), show_hist = TRUE)$x, c(1, 5))
})

# =============================================================================
# 4. is_valid_histology_overlay — CHAQUE condition de rejet
# =============================================================================
test_that("is_valid_histology_overlay rejette chacune de ses conditions", {
  rgba <- array(1, c(2, 2, 4))
  b <- list(x = c(0, 1), y = c(0, 1))
  expect_false(is_valid_histology_overlay(NULL))
  expect_false(is_valid_histology_overlay(list(rgba = rgba)))                              # pas de bounds
  expect_false(is_valid_histology_overlay(list(bounds = b)))                               # pas de rgba
  expect_false(is_valid_histology_overlay(list(rgba = array(1, c(2, 2)), bounds = b)))     # 2 dimensions
  expect_false(is_valid_histology_overlay(list(rgba = array(1, c(2, 2, 2)), bounds = b)))  # < 3 canaux
  expect_false(is_valid_histology_overlay(list(rgba = array(1, c(0, 2, 4)), bounds = b)))  # 0 ligne
  expect_false(is_valid_histology_overlay(list(rgba = rgba, bounds = list(x = c(0, 1, 2), y = c(0, 1)))))
  expect_false(is_valid_histology_overlay(list(rgba = rgba, bounds = list(x = c(1, 1), y = c(0, 1)))))
  expect_false(is_valid_histology_overlay(list(rgba = rgba, bounds = list(x = c(0, 1), y = c(NaN, 1)))))
  expect_true(is_valid_histology_overlay(list(rgba = rgba, bounds = b)))
})

# =============================================================================
# 5. safe_static_histology_raster — plafond mémoire et canaux
# =============================================================================
test_that("safe_static_histology_raster garde la taille memoire et fabrique l'alpha", {
  r <- safe_static_histology_raster(.hist_ok())
  expect_false(is.null(r))
  expect_identical(dim(r), c(3L, 4L))
  # Le plafond de pixels est un ARGUMENT : au-delà, on rend NULL (jamais une
  # allocation géante, jamais une erreur).
  expect_null(safe_static_histology_raster(.hist_ok(), max_pixels = 1L))
  # Overlay invalide : NULL, jamais une erreur.
  expect_null(safe_static_histology_raster(list()))
  expect_null(safe_static_histology_raster(NULL))
  # 3 canaux (pas d'alpha) : l'alpha est FABRIQUÉ à 1, et la forme est gardée.
  r3 <- safe_static_histology_raster(list(rgba = array(0.5, c(2, 2, 3)),
                                          bounds = list(x = c(0, 1), y = c(0, 1))))
  expect_identical(dim(r3), c(2L, 2L))
})

# =============================================================================
# 6. safe_plot_range — une fenêtre TOUJOURS exploitable
# =============================================================================
test_that("safe_plot_range rend une fenetre utilisable meme degeneree", {
  expect_identical(safe_plot_range(c(3, 1, 9)), c(1, 9))
  # Valeur constante : la fenêtre est ÉLARGIE (sinon coord_fixed() échoue).
  expect_identical(safe_plot_range(c(5, 5)), c(4, 6))
  expect_identical(safe_plot_range(c(0, 0)), c(-1, 1))
  # Le plancher de 1 domine ; au-delà, c'est 2 % de la valeur.
  expect_identical(safe_plot_range(c(-100, -100)), c(-102, -98))
  # Les non finis sont ÉCARTÉS avant le range.
  expect_identical(safe_plot_range(c(Inf, 1, -Inf, 5)), c(1, 5))
  # Rien d'exploitable : repli DÉCLARÉ, jamais NaN.
  expect_identical(safe_plot_range(c(NA_real_, NA_real_)), c(0, 1))
  expect_identical(safe_plot_range(numeric(0)), c(0, 1))
  expect_identical(safe_plot_range(numeric(0), fallback = c(-2, 2)), c(-2, 2))
})

# =============================================================================
# 7. build_histology_debug_text — la queue n'existe QUE si `diag` existe
# =============================================================================
test_that("build_histology_debug_text assemble un diagnostic lisible", {
  expect_identical(build_histology_debug_text(NULL), "Histology overlay = NULL")

  ov <- list(data_uri = "d", raster_obj = array(1, c(2, 3, 4)),
             bounds = list(x = c(0, 1.5), y = c(2, 3)))
  t <- build_histology_debug_text(ov)
  expect_true(grepl("data_uri: TRUE", t, fixed = TRUE))
  expect_true(grepl("raster dims: 2 x 3 x 4", t, fixed = TRUE))
  expect_true(grepl("x=[0, 1.5]", t, fixed = TRUE))
  expect_true(grepl("y=[2, 3]", t, fixed = TRUE))
  # Sans `diag`, la queue est ABSENTE — pas « NA ».
  expect_false(grepl("pct_near_white", t, fixed = TRUE))

  ov$diag <- list(pct_near_white = 12.34, n_unique_colors = 7L, mean_rgb = c(0.1, 0.2, 0.3))
  t2 <- build_histology_debug_text(ov)
  expect_true(grepl("pct_near_white=12.3%", t2, fixed = TRUE))
  expect_true(grepl("n_unique_colors=7", t2, fixed = TRUE))
  expect_true(grepl("mean_rgb=[0.1,0.2,0.3]", t2, fixed = TRUE))
})

# =============================================================================
# 8. .esc_js et scale_alpha_by_value
# =============================================================================
test_that(".esc_js echappe les apostrophes pour un litteral JS", {
  expect_identical(.esc_js("a'b'c"), "a\\'b\\'c")
  expect_identical(.esc_js("sans apostrophe"), "sans apostrophe")
})

test_that("scale_alpha_by_value normalise dans alpha_range sans jamais diviser par zero", {
  # Valeur constante : tout au MAXIMUM (pas de division par zéro).
  expect_identical(scale_alpha_by_value(c(4, 4, 4)), c(1, 1, 1))
  expect_equal(scale_alpha_by_value(c(0, 5, 10)), c(0.15, 0.575, 1), tolerance = 1e-12)
  expect_equal(scale_alpha_by_value(c(0, 10), alpha_range = c(0, 1)), c(0, 1), tolerance = 1e-12)
  # Un NA n'est ni propagé ni rejeté : il tombe sur le BAS de la plage.
  expect_equal(scale_alpha_by_value(c(0, NA, 10)), c(0.15, 0.15, 1), tolerance = 1e-12)
})

# =============================================================================
# 9. sort_cluster_labels — tri NUMERIQUE des libellés de clusters
# =============================================================================
test_that("sort_cluster_labels trie numeriquement, pas lexicographiquement", {
  # « 10 » doit SUIVRE « 9 » ; un tri lexicographique le placerait AVANT.
  expect_identical(sort_cluster_labels(c("10", "9", "2")), c("2", "9", "10"))
  expect_identical(sort_cluster_labels(c("beta", "alpha")), c("alpha", "beta"))
  # Dès qu'un libellé n'est pas numérique, on retombe sur le tri lexicographique.
  expect_identical(sort_cluster_labels(c("2", "a")), c("2", "a"))
  # NA écarté, doublons repliés.
  expect_identical(sort_cluster_labels(c("2", NA, "2", "1")), c("1", "2"))
  expect_identical(sort_cluster_labels(character(0)), character(0))
  # Entrée non-character : convertie.
  expect_identical(sort_cluster_labels(c(3L, 1L)), c("1", "3"))
})

# =============================================================================
# 10. Garde structurelle : les 10 helpers sont présents
# =============================================================================
test_that("les 10 helpers du fichier sont presents et sont des fonctions", {
  fns <- c("get_available_resolutions", "make_plotly_histology_image",
           "compute_spatial_ranges", "is_valid_histology_overlay",
           "safe_static_histology_raster", "safe_plot_range",
           "build_histology_debug_text", "scale_alpha_by_value",
           "sort_cluster_labels")
  for (f in fns) expect_true(is.function(get(f, envir = globalenv())), info = f)
  # `.esc_js` est PRIVÉ (point initial) : il vit dans le même fichier.
  expect_true(is.function(.esc_js))
})

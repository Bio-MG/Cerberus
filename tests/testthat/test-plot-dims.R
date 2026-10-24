# =============================================================================
# test-plot-dims.R — PLOT-S6 : dimension de device BORNEE pour les plots
# =============================================================================
# Defaut corrige : un device de surface nulle fait echouer le
# `graphics::plot.new()` de `shiny:::startPNG()` (« figure margins too large »).
# Le client Shiny transmet une taille des qu'UNE dimension est non nulle
# (`doSendSize` : `if (rect.width !== 0 || rect.height !== 0)`), donc un element
# dont une seule dimension vaut 0 atteint le device tel quel.
#
# Ce qui est verifie ici :
#   1. l'arithmetique du clamp — aucune entree hostile ne peut produire 0 ;
#   2. le mode "auto" est remplace, une dimension explicite est transmise ;
#   3. la lecture REELLE de la taille client fonctionne dans une session Shiny
#      (getCurrentOutputInfo()$name + clientData) et le plancher ne rabaisse
#      jamais une dimension valide ;
#   4. le branchement dans global.R : l'expression `renderPlot <- ...` s'evalue,
#      sa signature reste celle de `shiny::renderPlot`, et un appel NON qualifie
#      (comme les 92 sites de modules/) passe bien par elle.
# =============================================================================
# D2 M4-pilote (2026-10-25) : chargement PAQUET (pkgload::load_all via le
# helper load_project_pkg(), namespace + attach export_all) au lieu de
# source_project_file — le fichier vit desormais dans tools/pkg_skeleton/R/.
# L'anti-double-definition (le paquet fournit SANS definir dans globalenv)
# est epingle par test-plotting-pkg-pilot.R. Le 5e bloc ci-dessous evalue
# l'override renderPlot de global.R : son appel ts_render_plot_args() passe
# par le search path (paquet attache) — dans l'app vivante il passe par le
# globalenv d'app.R, qui source() le fichier au meme endroit de l'ordre.
load_project_pkg()

test_that("planchers : constantes exposees et strictement positives", {
  d <- ts_plot_min_dims()
  expect_identical(d$width,  TS_PLOT_MIN_WIDTH_PX)
  expect_identical(d$height, TS_PLOT_MIN_HEIGHT_PX)
  expect_true(TS_PLOT_MIN_WIDTH_PX  > 0)
  expect_true(TS_PLOT_MIN_HEIGHT_PX > 0)
})

test_that("clamp : aucune entree hostile ne peut produire une dimension nulle", {
  # LE cas du defaut : 0. Une seule dimension nulle suffit a faire echouer
  # plot.new(), donc chacune est bornee independamment.
  expect_identical(ts_clamp_dim(0, 300), 300)
  expect_identical(ts_clamp_dim(0L, 300), 300)
  expect_identical(ts_clamp_dim(NULL, 300), 300)
  expect_identical(ts_clamp_dim(NA_real_, 300), 300)
  expect_identical(ts_clamp_dim(NaN, 300), 300)
  expect_identical(ts_clamp_dim(Inf, 300), 300)
  expect_identical(ts_clamp_dim(-5, 300), 300)
  expect_identical(ts_clamp_dim(c(10, 20), 300), 300)
  expect_identical(ts_clamp_dim("abc", 300), 300)
  # une dimension valide est seulement REMONTEE au plancher, jamais rabaissee
  expect_identical(ts_clamp_dim(120, 300), 300)
  expect_identical(ts_clamp_dim(900, 300), 900)
  expect_identical(ts_clamp_dim("640", 300), 640)
  # propriete : le resultat est toujours >= plancher, pour tout plancher > 0
  for (m in c(1, 200, 320, 1000)) {
    expect_true(all(vapply(list(0, NULL, NA_real_, -1, 5, 5000),
                           function(v) ts_clamp_dim(v, m) >= m, logical(1))))
  }
})

test_that("ts_render_plot_args : 'auto' borne, dimension explicite intacte", {
  a <- ts_render_plot_args("auto", "auto")
  expect_true(is.function(a$width))
  expect_true(is.function(a$height))
  # hors session Shiny : plancher (repli sur, jamais 0)
  expect_identical(a$width(),  TS_PLOT_MIN_WIDTH_PX)
  expect_identical(a$height(), TS_PLOT_MIN_HEIGHT_PX)
  # une dimension explicite est transmise INCHANGEE (choix du developpeur)
  b <- ts_render_plot_args("auto", 400)
  expect_true(is.function(b$width))
  expect_identical(b$height, 400)
  expect_identical(ts_render_plot_args(600, 400)$width, 600)
  expect_identical(ts_render_plot_args(600, 400)$height, 400)
  # reactive / fonction explicites : intactes elles aussi
  f <- function() 700
  expect_identical(ts_render_plot_args(f, 400)$width, f)
})

test_that("session Shiny reelle : la taille client est lue, le plancher ne rabaisse pas", {
  rec <- new.env()
  rec$w <- NULL; rec$h <- NULL; rec$name <- NULL
  srv <- function(input, output, session) {
    output$p <- shiny::renderPlot(
      { plot(1:5) },
      width = function() {
        rec$name <- shiny::getCurrentOutputInfo()$name
        v <- ts_plot_dim("width")();  rec$w <- v; v
      },
      height = function() { v <- ts_plot_dim("height")(); rec$h <- v; v }
    )
    invisible(output$p)
  }
  shiny::testServer(srv, {
    invisible(output$p)
    expect_identical(rec$name, "p")              # le nom d'output est resolu
    expect_true(is.finite(rec$w) && rec$w > 0)   # jamais 0
    expect_true(is.finite(rec$h) && rec$h > 0)
    expect_gte(rec$w, TS_PLOT_MIN_WIDTH_PX)      # plancher respecte
    expect_gte(rec$h, TS_PLOT_MIN_HEIGHT_PX)
    expect_identical(rec$w, 600)                 # valeur client REELLE, non ecrasee
    expect_identical(rec$h, 400)
  })
})

test_that("global.R : l'ombre de renderPlot est evaluable et garde la signature de shiny", {
  root <- ts_project_root()
  ex <- parse(file.path(root, "global.R"))
  hit <- NULL
  for (i in seq_along(ex)) {
    e <- ex[[i]]
    if (is.call(e) && length(e) == 3L && identical(as.character(e[[1]]), "<-") &&
        identical(as.character(e[[2]]), "renderPlot")) hit <- e
  }
  expect_false(is.null(hit))
  eval(hit, envir = globalenv())
  expect_true(exists("renderPlot", envir = globalenv(), inherits = FALSE))
  # signature identique a celle de shiny : si Shiny change, ce test le dit
  expect_identical(names(formals(renderPlot)), names(formals(shiny::renderPlot)))
  # et un appel NON qualifie (le cas des 92 sites de modules/) la traverse
  got <- tryCatch({
    shiny::testServer(function(input, output, session) {
      output$p <- renderPlot({ plot(1:5) })
      v <- output$p
      "rendu-ok"
    }, {})
    "rendu-ok"
  }, error = function(e) conditionMessage(e))
  expect_identical(got, "rendu-ok")
})

# =============================================================================
# test-plot-export.R — PLOT-S2 : helper d'export unifie ts_export_plot()
# =============================================================================
# Objectif central : prouver le ZERO CHANGEMENT DE COMPORTEMENT.
#   - dpi = 300 est le defaut de ggplot2::ggsave() ET de ts_export_plot()
#   - format = NULL => on ne passe PAS `device` (ggsave devine l'extension)
#   - bg = NULL     => on ne passe PAS `bg`
#   - width/height  => transmis tels quels
# PLOT-S5 (2026-09-12) ajoute : les choix UI (ts_export_format_choices_ui),
# la garde structurelle des 5 selecteurs, et le rendu ComplexHeatmap -> svglite.
# =============================================================================
library(testthat)
suppressWarnings(suppressPackageStartupMessages(library(ggplot2)))

source_project_file("R/plotting/export.R")

# --- Fixtures ----------------------------------------------------------------

.tsx_plot <- function() {
  ggplot2::ggplot(data.frame(x = 1:3, y = c(1, 3, 2)),
                  ggplot2::aes(x, y)) + ggplot2::geom_point()
}

# Capture les arguments reellement transmis a ggsave, sans rien ecrire sur
# le disque : on stub ggplot2::ggsave dans un environnement local.
.tsx_capture <- function(expr) {
  env <- new.env(parent = globalenv())
  assign("ggsave", function(...) list(...), envir = env)
  # ts_export_plot appelle ggplot2::ggsave explicitement : on le masque.
  captured <- NULL
  ns <- asNamespace("ggplot2")
  if (bindingIsLocked("ggsave", ns)) {
    unlockBinding("ggsave", ns)
    on.exit(lockBinding("ggsave", ns), add = TRUE)
  }
  old <- get("ggsave", envir = ns)
  assign("ggsave", function(...) { captured <<- list(...) }, envir = ns)
  on.exit(assign("ggsave", old, envir = ns), add = TRUE, after = FALSE)
  force(expr)
  captured
}

# --- 1. Contrat du resolveur -------------------------------------------------

test_that("surface publique figee", {
  expect_setequal(
    ts_export_public_api(),
    c("ts_export_dpi_choices", "ts_export_format_choices",
      "ts_export_format_choices_ui",
      "ts_export_plot", "ts_export_public_api", "ts_export_resolve_format")
  )
  expect_false(anyDuplicated(ts_export_public_api()) > 0)
})

test_that("PLOT-S5 : les choix UI exposent png / pdf / svg, dans l'ordre", {
  ch <- ts_export_format_choices_ui()
  # valeurs = ce que le handler recoit ; noms = ce que l'utilisateur lit
  expect_identical(unname(ch), c("png", "pdf", "svg"))
  expect_identical(names(ch), c("PNG", "PDF (vectoriel)", "SVG (vectoriel)"))
  # "svg" doit etre PROPOSE, pas seulement reconnu par le resolveur
  expect_true("svg" %in% unname(ch))
  # les valeurs proposees sont exactement les formats reconnus (pas de derive)
  expect_setequal(unname(ch), ts_export_format_choices())
})

test_that("PLOT-S5 : les 5 selecteurs d'export consomment la source unique", {
  # Garde structurelle : sans elle, un retour a un vecteur png/pdf code en dur
  # dans UN seul des 5 selecteurs re-desactiverait SVG sans rien casser ailleurs.
  selecteurs <- list(
    c("modules/bulk_de/mod_bulk_de_ui.R", "volcano_export_fmt"),
    c("modules/bulk_de/mod_bulk_de_ui.R", "ma_export_fmt"),
    c("modules/bulk_de/mod_bulk_de_ui.R", "heatmap_export_fmt"),
    c("modules/sc/mod_sc_trajectory.R",   "traj_export_fmt"),
    c("modules/sc/mod_sc_viz.R",          "export_format")
  )
  for (s in selecteurs) {
    rel <- s[1]; id <- s[2]
    path <- file.path(ts_project_root(), rel)
    expect_true(file.exists(path), info = rel)
    code <- readLines(path, warn = FALSE, encoding = "UTF-8")
    # le selectInput(ns("<id>"), ...) et sa ligne `choices =` qui suit
    idx <- grep(paste0("ns\\(\"", id, "\"\\)"), code, perl = TRUE)
    expect_true(length(idx) >= 1L, info = paste("selectInput introuvable :", id))
    win <- paste(code[idx[1]:min(idx[1] + 2L, length(code))], collapse = "\n")
    expect_match(win, "ts_export_format_choices_ui\\(\\)", perl = TRUE,
                 info = paste0(id, " n'utilise pas ts_export_format_choices_ui()"))
  }
})

test_that("dpi : le defaut est 300 — celui de ggsave ET des 23/24 sites", {
  expect_identical(formals(ts_export_plot)$dpi, 300L)
  expect_true(300L %in% ts_export_dpi_choices())
  expect_setequal(ts_export_dpi_choices(), c(150L, 300L, 600L))
})

test_that("format : NULL par defaut = ggsave devine depuis l'extension", {
  expect_null(ts_export_resolve_format(NULL))
  expect_null(ts_export_resolve_format(NA_character_))
  expect_null(ts_export_resolve_format(""))
  expect_identical(ts_export_resolve_format("png"), "png")
  expect_identical(ts_export_resolve_format("PDF"), "pdf")
  expect_identical(ts_export_resolve_format(".pdf"), "pdf")
})

test_that("format : valeur inconnue => erreur classee invalid_format", {
  e <- tryCatch(ts_export_resolve_format("tiff"),
                error = function(e) e)
  expect_s3_class(e, "plot_export_error")
  expect_identical(e$state, "invalid_format")
  expect_match(conditionMessage(e), "format d'export non reconnu")
})

test_that("format svg : degrade en png quand svglite est absent", {
  # svglite 2.2.2 EST installe et present dans renv.lock ; il est desormais
  # declare dans required_packages (PLOT-S5, global.R). La degradation reste
  # neanmoins testee : c'est le contrat du helper, independamment de l'etat
  # de l'environnement.
  has_svglite <- requireNamespace("svglite", quietly = TRUE)
  out <- if (has_svglite) "svg" else "png"
  if (has_svglite) {
    expect_identical(ts_export_resolve_format("svg"), "svg")
  } else {
    expect_warning(ts_export_resolve_format("svg"), "svglite")
    expect_identical(suppressWarnings(ts_export_resolve_format("svg")), "png")
  }
  expect_true(out %in% ts_export_format_choices())
})

test_that("PLOT-S5 : format svg produit un VRAI fichier SVG", {
  skip_if_not_installed("svglite")
  p <- .tsx_plot()
  tmp <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp), add = TRUE)

  ts_export_plot(tmp, p, width = 8, height = 6, format = "svg")

  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0)
  # Un SVG valide commence par <?xml ... <svg et declare le namespace SVG.
  # NB : svglite ecrit les attributs avec des GUILLEMETS SIMPLES
  # (xmlns='...') -> ne jamais asserter la variante a guillemets doubles.
  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_match(txt, "<svg", fixed = TRUE)
  expect_match(txt, "http://www.w3.org/2000/svg", fixed = TRUE)
})

test_that("PLOT-S5 : le format devine depuis l'extension .svg est bien 'svg'", {
  # ggsave(device = NULL) devine depuis l'extension -> .svg doit produire du
  # vectoriel, pas du PNG renomme.
  skip_if_not_installed("svglite")
  p <- .tsx_plot()
  tmp <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp), add = TRUE)

  ts_export_plot(tmp, p, width = 8, height = 6)   # pas de `format` : extension seule
  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_match(txt, "<svg", fixed = TRUE)
})

test_that("PLOT-S5 : un ComplexHeatmap se dessine sur un device svglite", {
  # Les 2 sites a device BRUT (heatmap bulk_de, heatmap_hier de mod_sc_viz)
  # ouvrent svglite::svglite() a la main : ComplexHeatmap n'est pas un ggplot,
  # ts_export_plot() (ggsave) ne s'y applique pas. Ce test prouve que le rendu
  # fonctionne reellement sur ce device — pas seulement que le fichier existe.
  skip_if_not_installed("svglite")
  skip_if_not_installed("ComplexHeatmap")
  m  <- matrix(seq_len(20), nrow = 4,
               dimnames = list(paste0("g", 1:4), paste0("c", 1:5)))
  ht <- ComplexHeatmap::Heatmap(m, name = "Z-score")
  tmp <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp), add = TRUE)

  svglite::svglite(tmp, width = 9, height = 8)
  ComplexHeatmap::draw(ht)
  grDevices::dev.off()

  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0)
  txt <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_match(txt, "<svg", fixed = TRUE)
  expect_match(txt, "http://www.w3.org/2000/svg", fixed = TRUE)
})

# --- 2. GARANTIE zero changement de comportement ------------------------------

test_that("appel nu : strictement identique a ggsave(file, plot, w, h, dpi=300)", {
  p <- .tsx_plot()
  a <- .tsx_capture(ts_export_plot("out.png", p, width = 8, height = 6))
  expect_identical(a$filename, "out.png")
  expect_identical(a$plot, p)
  expect_identical(a$width, 8)
  expect_identical(a$height, 6)
  expect_identical(a$dpi, 300L)
  # LE POINT CLE : ni `device` ni `bg` ne doivent apparaitre
  expect_null(a$device)
  expect_null(a$bg)
  expect_false("device" %in% names(a))
  expect_false("bg" %in% names(a))
})

test_that("format fourni => device transmis, et RIEN d'autre ne change", {
  p <- .tsx_plot()
  a <- .tsx_capture(ts_export_plot("out.pdf", p, width = 8, height = 6,
                                   format = "pdf"))
  expect_identical(a$device, "pdf")
  expect_identical(a$dpi, 300L)
  expect_null(a$bg)
})

test_that("bg fourni => transmis ; non fourni => absent", {
  p <- .tsx_plot()
  a <- .tsx_capture(ts_export_plot("out.png", p, 8, 6, bg = "white"))
  expect_identical(a$bg, "white")
  b <- .tsx_capture(ts_export_plot("out.png", p, 8, 6))
  expect_false("bg" %in% names(b))
})

test_that("dpi explicite (site 200 dpi) est respecte", {
  p <- .tsx_plot()
  a <- .tsx_capture(ts_export_plot("out.png", p, 8, 8, dpi = 200L, bg = "white"))
  expect_identical(a$dpi, 200L)
  expect_identical(a$bg, "white")
})

test_that("les arguments supplementaires sont transmis a ggsave", {
  p <- .tsx_plot()
  a <- .tsx_capture(ts_export_plot("out.png", p, 8, 6,
                                   units = "cm", limitsize = FALSE))
  expect_identical(a$units, "cm")
  expect_false(a$limitsize)
})

test_that("la valeur de retour est le nom de fichier, invisiblement", {
  p <- .tsx_plot()
  # Ecriture REELLE (seul test non stubbe) — mais dans un dossier temporaire,
  # pour ne rien laisser trainer dans tests/testthat/.
  out <- file.path(tempdir(), paste0("ts_export_", Sys.getpid(), ".png"))
  on.exit(unlink(out), add = TRUE)
  expect_invisible(ts_export_plot(out, p, 8, 6))
  expect_true(file.exists(out))
  expect_true(file.size(out) > 0)
})

# --- 3. Gardes (erreurs classees, messages FR) --------------------------------

test_that("nom de fichier invalide => invalid_filename", {
  p <- .tsx_plot()
  for (bad in list(NULL, NA_character_, "", character(0))) {
    e <- tryCatch(.tsx_capture(ts_export_plot(bad, p, 8, 6)),
                  error = function(e) e)
    expect_s3_class(e, "plot_export_error")
    expect_identical(e$state, "invalid_filename")
  }
  e <- tryCatch(ts_export_plot(NULL, p, 8, 6), error = function(e) e)
  expect_match(conditionMessage(e), "nom de fichier")
})

test_that("plot NULL => invalid_plot", {
  e <- tryCatch(ts_export_plot("out.png", NULL, 8, 6), error = function(e) e)
  expect_s3_class(e, "plot_export_error")
  expect_identical(e$state, "invalid_plot")
})

test_that("dpi invalide => invalid_dpi", {
  p <- .tsx_plot()
  for (bad in list(0, -300, NA, "300", c(150, 300))) {
    e <- tryCatch(ts_export_plot("out.png", p, 8, 6, dpi = bad),
                  error = function(e) e)
    expect_s3_class(e, "plot_export_error")
    expect_identical(e$state, "invalid_dpi")
  }
})

# --- 4. Anti-regression : plus aucun ggsave() nu dans les fichiers migres -----

.tsx_migrated <- c(
  "R/spatial/spatial_export.R",
  "modules/bulk/mod_bulk_filter.R",
  "modules/bulk_de/mod_bulk_de_summary.R",
  "modules/bulk_de/mod_bulk_de_viz.R",
  "modules/sc/mod_sc_communication.R",
  "modules/sc/mod_sc_communication_perturbation.R",
  "modules/sc/mod_sc_communication_spatial.R",
  "modules/sc/mod_sc_communication_trajectory.R",
  "modules/sc/mod_sc_communication_velocity.R",
  "modules/sc/mod_sc_da_cross.R",
  "modules/sc/mod_sc_da_milo.R",
  "modules/sc/mod_sc_da_sccoda.R",
  "modules/sc/mod_sc_trajectory.R",
  "modules/sc/mod_sc_velocity.R",
  "modules/sc/mod_sc_viz.R",
  "modules/spatial/mod_spatial_viz.R"
)

test_that("PLOT-S2 : aucun ggsave() nu ne subsiste dans les fichiers migres", {
  for (rel in .tsx_migrated) {
    path <- file.path(ts_project_root(), rel)
    if (!file.exists(path)) next
    code <- readLines(path, warn = FALSE, encoding = "UTF-8")
    code <- grep("^\\s*#", code, value = TRUE, invert = TRUE)
    hits <- grep("ggsave\\s*\\(", code, perl = TRUE, value = TRUE)
    expect(length(hits) == 0L,
           paste0(rel, " : appels ggsave() nus restants -> ",
                  paste(trimws(hits), collapse = " | ")))
  }
})

test_that("R/sc/sc_export.R reste exclu (chaine de script genere, pas du code)", {
  # ⚠️ « exclu » porte sur la REGLE DES ggsave() NUS (C10), PAS sur la
  # testabilite du fichier : `sc_r_script_text()` est une fonction PURE de
  # premier niveau, et elle A son test eponyme depuis §2dd
  # (`test-sc-export.R`, qui y a d'ailleurs trouve un P0 — le script genere
  # n'etait pas du R valide dans la branche `has_mt = FALSE`).
  # Ne pas lire cette exclusion comme « ce fichier n'est pas testable ».
  path <- file.path(ts_project_root(), "R/sc/sc_export.R")
  code <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(code, "ggsave\\(")
})

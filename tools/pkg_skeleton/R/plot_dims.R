# =============================================================================
# R/plotting/plot_dims.R — PLOT-S6 : dimension de device MINIMALE des plots
# =============================================================================
# POURQUOI CE FICHIER
#
# `shiny:::startPNG()` ouvre le device PNG puis, juste apres, execute :
#     op <- graphics::par(mar = rep(0, 4))
#     tryCatch(graphics::plot.new(), finally = graphics::par(op))
# Sur un device dont UNE dimension vaut 0, ce `plot.new()` leve
# « figure margins too large ». Mesure du 2026-09-15 :
#   ragg::agg_png(0, 0)   -> le device s'ouvre SANS erreur
#   Cairo::CairoPNG(0, 0) -> idem
#   puis plot.new()       -> ERREUR dans les deux cas
#   grDevices::png(0, 0)  -> erreur des la creation (« invalid 'width' or
#                            'height' ») : le LIBELLE du message dit donc quel
#                            backend Shiny a choisi.
# Le plot lui-meme n'est jamais fautif : les helpers du depot sont des ggplot /
# ComplexHeatmap, rendus verifies jusqu'a 1 x 1 px.
#
# Cote client, `doSendSize()` (shiny.js) ne filtre que le cas ou LES DEUX
# dimensions sont nulles :
#     if (rect.width !== 0 || rect.height !== 0) { ...envoie les deux... }
# Un element dont une SEULE dimension vaut 0 est donc transmis tel quel, et le
# serveur ouvre un device 0 x N. On borne donc la dimension AVANT le device.
#
# Les bornes sont volontairement BASSES : elles evitent un device de surface
# nulle, elles ne pilotent PAS la taille d'affichage — celle-ci reste fixee par
# le CSS de `plotOutput(height = ...)`.
#
# Le branchement dans `renderPlot()` lui-meme vit dans `global.R` (le motif
# `shiny::renderPlot(` est interdit dans R/ par la garde C2 : R/ = logique pure).
# =============================================================================

#' Dimension minimale d'un device de plot (px). Voir l'en-tete du fichier.
TS_PLOT_MIN_WIDTH_PX  <- 320L
TS_PLOT_MIN_HEIGHT_PX <- 240L

#' Bornes courantes, sous forme nommee (utilisable telle quelle par les tests
#' et par un appelant qui veut construire ses propres wrappers).
ts_plot_min_dims <- function() {
  list(width = TS_PLOT_MIN_WIDTH_PX, height = TS_PLOT_MIN_HEIGHT_PX)
}

#' Borner une dimension de device.
#'
#' Toute valeur absente, non finie, nulle ou negative retombe sur `min_px` :
#' c'est exactement le cas qui faisait echouer `plot.new()`. Une valeur
#' exploitable est seulement remontee au plancher, jamais rabaissee — un plot
#' large reste large.
#'
#' @param value Dimension demandee (px) ; NULL / NA / 0 / negatif = non exploitable.
#' @param min_px Plancher en pixels.
#' @return Un `numeric` scalaire >= min_px.
ts_clamp_dim <- function(value, min_px) {
  if (is.null(value) || length(value) != 1L) return(min_px)
  v <- suppressWarnings(as.numeric(value))
  if (!is.finite(v) || v <= 0) return(min_px)
  max(min_px, v)
}

#' Wrapper de dimension pour `renderPlot()`.
#'
#' Renvoie une FONCTION (et non une valeur) : `shiny::renderPlot()` evalue ses
#' arguments `width`/`height` dans un contexte reactif, au moment du rendu. La
#' taille reellement demandee par le client est relue dans `clientData` — la cle
#' est `output_<id>_<axis>`, ou l'id est le nom NAMESPACE de l'output, obtenu par
#' `shiny::getCurrentOutputInfo()$name` (verifie disponible depuis ce wrapper).
#'
#' Si l'information n'est pas exploitable (hors session Shiny, clientData absent,
#' dimension nulle), le plancher est renvoye : c'est le repli sur qui rend la
#' fonction sure par construction.
#'
#' @param axis "width" ou "height".
#' @param min_px Plancher ; par defaut la constante du fichier pour cet axe.
#' @return Une fonction sans argument renvoyant la dimension bornee (px).
ts_plot_dim <- function(axis = c("width", "height"), min_px = NULL) {
  axis <- match.arg(axis)
  if (is.null(min_px)) {
    min_px <- if (identical(axis, "width")) TS_PLOT_MIN_WIDTH_PX else TS_PLOT_MIN_HEIGHT_PX
  }
  force(axis)
  force(min_px)
  function() {
    sess <- tryCatch(shiny::getDefaultReactiveDomain(), error = function(e) NULL)
    info <- tryCatch(shiny::getCurrentOutputInfo(), error = function(e) NULL)
    if (is.null(sess) || is.null(info) || is.null(info$name)) return(min_px)
    key <- paste0("output_", info$name, "_", axis)
    ts_clamp_dim(tryCatch(sess$clientData[[key]], error = function(e) NULL), min_px)
  }
}

#' Arguments `width` / `height` a transmettre a `shiny::renderPlot()`.
#'
#' Seul le mode `"auto"` (taille pilotee par le client — donc le seul exposé au
#' device de surface nulle) est remplace par un wrapper borne. Une dimension
#' explicite (nombre, reactive, fonction) est transmise INCHANGEE : elle est
#' choisie par le developpeur, la borner changerait son intention.
#'
#' @param width,height Valeurs recues par `renderPlot()`.
#' @return `list(width = , height = )`.
ts_render_plot_args <- function(width = "auto", height = "auto") {
  list(
    width  = if (identical(width,  "auto")) ts_plot_dim("width")  else width,
    height = if (identical(height, "auto")) ts_plot_dim("height") else height
  )
}

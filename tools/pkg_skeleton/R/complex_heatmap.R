# =============================================================================
# R/plotting/complex_heatmap.R — PLOT-S4 : coeur ComplexHeatmap unifie
#                               (ts_complex_heatmap)
# =============================================================================
# Un seul endroit pour construire les heatmaps ComplexHeatmap de l'application,
# la ou trois implementations coexistaient :
#   - plot_heatmap_bulk()              (R/bulk/bulk_helpers.R)
#   - plot_sample_correlation_heatmap()(R/bulk/bulk_helpers.R)
#   - build_sc_hierarchical_heatmap()  (R/sc/sc_helpers.R)
#
# MESURE PREALABLE (2026-09-12) — les trois sites DIVERGENT sur 9 axes. Aucune
# valeur n'est neutre : chaque wrapper doit donc passer la sienne
# EXPLICITEMENT (meme discipline que base_size en PLOT-S1 et page_length en
# PLOT-S3). Tableau des divergences relevees dans le depot :
#
#   axe                    | plot_heatmap_bulk   | plot_sample_corr    | SC hierarchique
#   -----------------------|---------------------|---------------------|------------------
#   name (legende)         | tr("Z-score")       | "Pearson"/"Spearman"| "Z-score" (non traduit)
#   rampe                  | diverging(range)    | sequential(c(min,1))| diverging(range)
#   show_row_names         | nrow(mat) <= 60     | TRUE                | nrow(mat) <= 60
#   show_column_names      | defaut (TRUE)       | TRUE                | ncol(mat) <= 60
#   column_title           | tr("Heatmap — ...") | tr("Correlation ...")| "Heatmap Hierarchique -- ..."
#   cell_fun               | aucun               | sprintf("%.2f")     | aucun
#   clustering             | defauts (eucl/comp) | defauts             | euclidean/complete explicite
#   dessin                 | draw() + marge sure | AUCUN (objet rendu) | draw() + marge sure
#   repli sans CH          | ggplot              | ggplot              | stop()
#
# DEUX ECARTS ASSUMES, volontairement PRESERVES ici (zero changement) :
#   1. `draw = FALSE` existe pour plot_sample_correlation_heatmap(), qui rend
#      aujourd'hui l'objet Heatmap brut et laisse l'appelant faire print().
#      ⚠️ Ce site n'est PAS protege contre l'erreur "figure margins too large"
#      (meme classe de bug que Venn/UpSet, corrigee ailleurs). Le corriger est
#      un changement de comportement -> a traiter dans un jalon dedie, pas ici.
#   2. Le name de la heatmap SC est "Z-score" en dur, NON traduit, alors que
#      Bulk le passe par tr(). Conserve tel quel (changement visible sinon).
#
# NOUVEAU (demande roadmap §3) : choix distance/methode de clustering, decoupage
# en k clusters de lignes (k_row -> row_split), et annotations MULTIPLES
# (col_meta / row_meta acceptent un data.frame, pas une seule colonne).
#
# REGLE : ZERO CHANGEMENT DE COMPORTEMENT sur les 3 sites existants.
#
# Pure R : testable hors de Shiny, erreurs classees, messages FR, call. = FALSE.
# =============================================================================
# Cree le 2026-09-12 (PLOT-S4). Toute modification doit passer SIMULTANEMENT
# par : ce fichier, tests/testthat/test-plot-complex-heatmap.R et
# docs/contracts/PLOT_HEATMAP_CONTRACT.md.
# =============================================================================

# --- Constantes ---------------------------------------------------------------

.ts_hm_const <- function(name, fallback) {
  if (exists(name, envir = globalenv(), inherits = TRUE)) {
    get(name, envir = globalenv(), inherits = TRUE)
  } else {
    fallback
  }
}

#' Seuil au-dela duquel les noms de lignes/colonnes sont masques (defaut).
#' Valeur historique commune aux trois implementations (60).
ts_heatmap_max_names <- function() {
  as.integer(.ts_hm_const("TS_HEATMAP_MAX_NAMES", 60L))
}

#' Distances de clustering proposees. "pearson"/"spearman" sont des distances
#' de correlation natives de ComplexHeatmap::Heatmap().
ts_heatmap_distances <- function() {
  .ts_hm_const("TS_HEATMAP_DISTANCES", c("euclidean", "pearson", "spearman"))
}

#' Methodes de clustering proposees (hclust).
ts_heatmap_methods <- function() {
  .ts_hm_const("TS_HEATMAP_METHODS", c("complete", "ward.D2", "average"))
}

#' Rampe de couleurs : diverging (Z-score, centre sur 0) ou sequential
#' (correlation, de min a 1).
ts_heatmap_ramps <- function() {
  c("diverging", "sequential")
}

#' Surface publique figee (verifiee par le test de gel).
ts_complex_heatmap_public_api <- function() {
  c("ts_complex_heatmap", "ts_heatmap_max_names", "ts_heatmap_distances",
    "ts_heatmap_methods", "ts_heatmap_ramps", "ts_complex_heatmap_public_api")
}

# --- Erreurs classees --------------------------------------------------------

.ts_hm_stop <- function(state, message) {
  stop(errorCondition(
    message,
    state = state,
    class = c("plot_heatmap_error", "error", "condition")
  ))
}

# --- Interne -----------------------------------------------------------------

#' Normalise une annotation en LISTE NOMMEE `annotation -> vecteur`.
#' Accepte : NULL, une liste nommee (forme canonique), ou un data.frame
#' (plusieurs colonnes = plusieurs annotations). Renvoie NULL si vide.
#' La forme liste est privilegiee : `HeatmapAnnotation(group = v)` recoit
#' alors EXACTEMENT le meme argument que dans le code historique.
.ts_hm_as_meta <- function(meta, arg_name) {
  if (is.null(meta)) return(NULL)
  if (is.data.frame(meta)) {
    if (ncol(meta) == 0L || nrow(meta) == 0L) return(NULL)
    return(as.list(meta))
  }
  if (is.list(meta)) {
    if (length(meta) == 0L) return(NULL)
    if (is.null(names(meta)) || any(!nzchar(names(meta)))) {
      .ts_hm_stop("invalid_meta",
                  sprintf("ts_complex_heatmap() : les elements de `%s` doivent etre nommes.", arg_name))
    }
    return(meta)
  }
  .ts_hm_stop("invalid_meta",
              sprintf("ts_complex_heatmap() : `%s` doit etre une liste nommee, un data.frame ou NULL.",
                      arg_name))
}

#' Longueur commune des vecteurs d'une liste d'annotations (0 si NULL).
.ts_hm_meta_len <- function(meta) {
  if (is.null(meta) || length(meta) == 0L) return(0L)
  length(meta[[1L]])
}

#' Construit une HeatmapAnnotation a partir d'une liste nommee d'annotations et
#' d'une liste de couleurs (nom d'annotation -> vecteur nomme niveau -> couleur).
.ts_hm_annotation <- function(meta, colors) {
  if (is.null(meta)) return(NULL)
  cols <- list()
  for (nm in names(meta)) {
    if (!is.null(colors) && !is.null(colors[[nm]])) cols[[nm]] <- colors[[nm]]
  }
  if (length(cols) > 0L) {
    do.call(ComplexHeatmap::HeatmapAnnotation, c(meta, list(col = cols)))
  } else {
    do.call(ComplexHeatmap::HeatmapAnnotation, meta)
  }
}

#' Resout la rampe de couleurs a partir de son type et de son domaine.
.ts_hm_ramp <- function(ramp, domain, palette, manual_colors, midpoint) {
  switch(ramp,
    diverging  = bulk_diverging_ramp(domain, palette = palette,
                                     manual_colors = manual_colors, midpoint = midpoint),
    sequential = bulk_sequential_ramp(domain, palette = palette,
                                      manual_colors = manual_colors),
    .ts_hm_stop("invalid_ramp",
                sprintf("ts_complex_heatmap() : rampe inconnue '%s' (attendu : %s).",
                        ramp, paste(ts_heatmap_ramps(), collapse = ", ")))
  )
}

#' Dessin protege contre "figure margins too large".
#' Meme convention que plot_upset_contrasts()/plot_venn_contrasts() et
#' plot_heatmap_bulk() : on fixe la marge AVANT draw() et on la restaure.
.ts_hm_draw_safe <- function(ht) {
  old_mar <- graphics::par("mar")
  on.exit(graphics::par(mar = old_mar), add = TRUE)
  graphics::par(mar = c(1, 1, 1, 1))
  invisible(ComplexHeatmap::draw(ht))
}

# --- Helper principal --------------------------------------------------------

#' Heatmap ComplexHeatmap unifiee ("publication-ready").
#'
#' Coeur commun des trois heatmaps de l'application. Les wrappers
#' (`plot_heatmap_bulk`, `plot_sample_correlation_heatmap`,
#' `build_sc_hierarchical_heatmap`) passent TOUTES leurs valeurs explicitement :
#' ce helper ne devine aucun defaut a leur place (cf. en-tete du fichier).
#'
#' @param mat Matrice numerique (lignes = features, colonnes = echantillons).
#' @param name Titre de la legende de couleur.
#' @param column_title,row_title Titres des axes (NULL = aucun).
#' @param col_meta,row_meta Annotations de colonnes / de lignes. Une liste
#'   nommee `annotation -> vecteur` (forme privilegiee : elle produit
#'   exactement le meme appel `HeatmapAnnotation()` que le code historique),
#'   ou un data.frame (plusieurs colonnes = plusieurs annotations).
#' @param col_annotation_colors,row_annotation_colors Liste nommee
#'   `annotation -> vecteur nomme niveau -> couleur`. La resolution des
#'   couleurs reste a la charge de l'appelant (`bulk_annotation_colors()` cote
#'   Bulk, `sc_discrete_colors()` cote SC) : les deux resolveurs divergent sur
#'   le mode "manual", on ne les fusionne donc pas.
#' @param ramp "diverging" (centre sur `midpoint`) ou "sequential".
#' @param ramp_domain Domaine de la rampe. NULL = `range(mat, na.rm = TRUE)`.
#' @param palette,manual_colors,midpoint Transmis aux rampes de `palettes.R`.
#' @param show_row_names,show_column_names Logiques explicites. NULL = regle
#'   automatique `<= ts_heatmap_max_names()`.
#' @param cell_fun Fonction `function(j, i, x, y, width, height, fill)` pour
#'   ecrire dans les cellules (NULL = aucune).
#' @param clustering_distance Distance de clustering (voir
#'   `ts_heatmap_distances()`). Utilisee pour les lignes ET les colonnes.
#' @param clustering_method Methode de clustering hclust (voir
#'   `ts_heatmap_methods()`). Utilisee pour les lignes ET les colonnes.
#' @param cluster_rows,cluster_columns Activer le clustering.
#' @param k_row NULL = pas de decoupage. Entier >= 2 = `row_split`, ce qui
#'   decoupe le dendrogramme de lignes en k groupes.
#' @param draw TRUE (defaut) = dessine immediatement avec une marge sure et
#'   renvoie l'objet invisiblement. FALSE = renvoie l'objet Heatmap tel quel,
#'   sans le dessiner (l'appelant fait `print()`), ce que fait aujourd'hui
#'   `plot_sample_correlation_heatmap()`.
#' @param tr Fonction de traduction (dernier parametre : ne jamais inserer de
#'   nouveau parametre apres `tr`).
#' @return Invisiblement l'objet `ComplexHeatmap::Heatmap` si `draw = TRUE`,
#'   l'objet lui-meme sinon.
ts_complex_heatmap <- function(
    mat,
    name = "Z-score",
    column_title = NULL,
    row_title = NULL,
    col_meta = NULL,
    row_meta = NULL,
    col_annotation_colors = NULL,
    row_annotation_colors = NULL,
    ramp = "diverging",
    ramp_domain = NULL,
    palette = "default",
    manual_colors = NULL,
    midpoint = 0,
    show_row_names = NULL,
    show_column_names = NULL,
    cell_fun = NULL,
    clustering_distance = "euclidean",
    clustering_method = "complete",
    cluster_rows = TRUE,
    cluster_columns = TRUE,
    k_row = NULL,
    draw = TRUE,
    tr = NULL) {

  tr <- tr %||% function(x) x

  # --- Gardes ---
  if (!is.matrix(mat) && !is.data.frame(mat)) {
    .ts_hm_stop("invalid_matrix",
                "ts_complex_heatmap() : `mat` doit etre une matrice numerique.")
  }
  mat <- as.matrix(mat)
  if (!is.numeric(mat)) {
    .ts_hm_stop("invalid_matrix",
                "ts_complex_heatmap() : `mat` doit etre numerique.")
  }
  if (nrow(mat) < 2L || ncol(mat) < 2L) {
    .ts_hm_stop("invalid_matrix",
                "ts_complex_heatmap() : la matrice doit avoir au moins 2 lignes et 2 colonnes.")
  }

  # Validation EXPLICITE (et non match.arg) pour produire des erreurs CLASSEES
  # `plot_heatmap_error` plutot qu'un simpleError de match.arg.
  if (length(clustering_distance) != 1L || is.na(clustering_distance) ||
      !clustering_distance %in% ts_heatmap_distances()) {
    .ts_hm_stop("invalid_distance",
                sprintf("ts_complex_heatmap() : distance de clustering inconnue '%s' (attendu : %s).",
                        paste(clustering_distance, collapse = ", "),
                        paste(ts_heatmap_distances(), collapse = ", ")))
  }
  if (length(clustering_method) != 1L || is.na(clustering_method) ||
      !clustering_method %in% ts_heatmap_methods()) {
    .ts_hm_stop("invalid_method",
                sprintf("ts_complex_heatmap() : methode de clustering inconnue '%s' (attendu : %s).",
                        paste(clustering_method, collapse = ", "),
                        paste(ts_heatmap_methods(), collapse = ", ")))
  }
  if (length(ramp) != 1L || is.na(ramp) || !ramp %in% ts_heatmap_ramps()) {
    .ts_hm_stop("invalid_ramp",
                sprintf("ts_complex_heatmap() : rampe inconnue '%s' (attendu : %s).",
                        paste(ramp, collapse = ", "),
                        paste(ts_heatmap_ramps(), collapse = ", ")))
  }

  if (!is.null(k_row)) {
    if (!is.numeric(k_row) || length(k_row) != 1L || is.na(k_row) || k_row < 2L) {
      .ts_hm_stop("invalid_k_row",
                  "ts_complex_heatmap() : `k_row` doit etre NULL ou un entier >= 2.")
    }
    if (k_row > nrow(mat)) {
      .ts_hm_stop("invalid_k_row",
                  sprintf("ts_complex_heatmap() : `k_row` (%d) depasse le nombre de lignes (%d).",
                          as.integer(k_row), nrow(mat)))
    }
    k_row <- as.integer(k_row)
    if (!isTRUE(cluster_rows)) {
      .ts_hm_stop("invalid_k_row",
                  "ts_complex_heatmap() : `k_row` exige cluster_rows = TRUE.")
    }
  }

  col_meta <- .ts_hm_as_meta(col_meta, "col_meta")
  row_meta <- .ts_hm_as_meta(row_meta, "row_meta")

  n_col_meta <- .ts_hm_meta_len(col_meta)
  n_row_meta <- .ts_hm_meta_len(row_meta)

  if (n_col_meta != 0L && n_col_meta != ncol(mat)) {
    .ts_hm_stop("meta_mismatch",
                sprintf("ts_complex_heatmap() : `col_meta` a %d elements pour %d colonnes de matrice.",
                        n_col_meta, ncol(mat)))
  }
  if (n_row_meta != 0L && n_row_meta != nrow(mat)) {
    .ts_hm_stop("meta_mismatch",
                sprintf("ts_complex_heatmap() : `row_meta` a %d elements pour %d lignes de matrice.",
                        n_row_meta, nrow(mat)))
  }

  max_names <- ts_heatmap_max_names()
  if (is.null(show_row_names))    show_row_names    <- nrow(mat) <= max_names
  if (is.null(show_column_names)) show_column_names <- ncol(mat) <= max_names

  # --- Rampe ---
  domain <- ramp_domain %||% range(mat, na.rm = TRUE)
  col_ramp <- .ts_hm_ramp(ramp, domain, palette, manual_colors, midpoint)

  # --- Annotations ---
  top_ann <- .ts_hm_annotation(col_meta, col_annotation_colors)
  left_ann <- .ts_hm_annotation(row_meta, row_annotation_colors)

  # --- Construction ---
  args <- list(
    mat,
    name                  = name,
    col                   = col_ramp,
    show_row_names        = show_row_names,
    show_column_names     = show_column_names,
    clustering_distance_rows    = clustering_distance,
    clustering_method_rows      = clustering_method,
    clustering_distance_columns = clustering_distance,
    clustering_method_columns   = clustering_method,
    cluster_rows          = cluster_rows,
    cluster_columns       = cluster_columns
  )
  if (!is.null(top_ann))      args$top_annotation  <- top_ann
  if (!is.null(left_ann))     args$left_annotation <- left_ann
  if (!is.null(column_title)) args$column_title    <- column_title
  if (!is.null(row_title))    args$row_title       <- row_title
  if (!is.null(cell_fun))     args$cell_fun        <- cell_fun
  if (!is.null(k_row))        args$row_split       <- k_row

  ht <- do.call(ComplexHeatmap::Heatmap, args)

  if (isTRUE(draw)) return(.ts_hm_draw_safe(ht))
  ht
}

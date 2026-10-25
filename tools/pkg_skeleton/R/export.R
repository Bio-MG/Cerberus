# =============================================================================
# R/plotting/export.R — PLOT-S2 : helper d'export unifie (dpi + format)
# =============================================================================
# Un seul endroit pour choisir dpi / format / dimensions, au lieu de ~25 appels
# ggsave() eparpilles dans R/ et modules/.
#
# REGLE ABSOLUE (comme PLOT-S1) : ZERO CHANGEMENT DE COMPORTEMENT.
#   - dpi = 300 est A LA FOIS le defaut de ggplot2::ggsave() et la valeur
#     explicite de 23 sites sur 24. C'est donc un defaut neutre.
#   - format = NULL signifie "devine depuis l'extension du fichier", qui est
#     exactement le comportement de ggsave(device = NULL). Le helper ne passe
#     `device` QUE si l'appelant le fournit.
#   - bg = NULL : le helper ne passe `bg` QUE si l'appelant le fournit.
#   - width/height : aucun defaut n'est impose ; tous les sites actuels les
#     passent deja explicitement.
#
# Pure R : testable hors de Shiny, aucun effet de bord, erreurs classees.
# =============================================================================
# Cree le 2026-09-11 (PLOT-S2). Toute modification doit passer SIMULTANEMENT
# par : ce fichier, tests/testthat/test-plot-export.R et
# docs/contracts/PLOT_EXPORT_CONTRACT.md.
# PLOT-S5 (2026-09-12) : ajoute ts_export_format_choices_ui() — source unique
# des choix de format des 5 selecteurs d'export (expose enfin "svg" dans l'UI).
# =============================================================================

# --- Constantes exportees ----------------------------------------------------
# Les choix proposes a l'utilisateur (pas encore cables : PLOT-S2 est une
# factorisation, pas une feature — voir decision n°4 du handoff).

# Lecture des constantes declarees dans config/defaults.R (regle : consommer
# TS_*, jamais coder en dur). Repli si le fichier de config n'est pas source
# (le helper doit rester utilisable de facon autonome).
.ts_export_const <- function(name, fallback) {
  if (exists(name, envir = globalenv(), inherits = TRUE)) {
    get(name, envir = globalenv(), inherits = TRUE)
  } else {
    fallback
  }
}

#' Choix de dpi proposes a l'utilisateur (ecran / impression / poster).
ts_export_dpi_choices <- function() {
  .ts_export_const("TS_EXPORT_DPI_CHOICES", c(150L, 300L, 600L))
}

#' Formats d'export reconnus. "svg" exige svglite et se degrade en png.
ts_export_format_choices <- function() {
  .ts_export_const("TS_EXPORT_FORMATS", c("png", "pdf", "svg"))
}

#' Choix de formats prets pour un `selectInput()` : valeurs issues de
#' `TS_EXPORT_FORMATS`, libelles informatifs.
#'
#' PLOT-S5 — SOURCE UNIQUE des 5 selecteurs d'export de l'application
#' (`volcano_export_fmt`, `ma_export_fmt`, `heatmap_export_fmt`,
#' `traj_export_fmt`, `export_format`). Ajouter un format ici l'expose aux cinq
#' d'un coup : c'est ce qui evite le retour a des vecteurs `png`/`pdf` codes en
#' dur qui derivent les uns des autres (cf. le controle mort `volcano_export_fmt`
#' corrige en PLOT-S5).
#'
#' Les libelles sont TECHNIQUES et volontairement identiques en FR et EN — ce ne
#' sont donc PAS des cles i18n (comportement inchange depuis PLOT-Q4 pour
#' "PNG" / "PDF (vectoriel)"). PLOT-S5 unifie seulement les 3 sites qui
#' affichaient "PDF" sans le suffixe.
ts_export_format_choices_ui <- function() {
  labels  <- c(png = "PNG", pdf = "PDF (vectoriel)", svg = "SVG (vectoriel)")
  formats <- ts_export_format_choices()
  labs    <- unname(labels[formats])
  labs[is.na(labs)] <- formats[is.na(labs)]   # format declare sans libelle connu
  stats::setNames(formats, labs)
}

#' Surface publique figee (verifiee par le test de gel).
ts_export_public_api <- function() {
  c("ts_export_dpi_choices", "ts_export_format_choices",
    "ts_export_format_choices_ui",
    "ts_export_plot", "ts_export_public_api", "ts_export_resolve_format")
}

# --- Erreurs classees --------------------------------------------------------

.ts_export_stop <- function(state, message) {
  stop(errorCondition(
    message,
    state = state,
    class = c("plot_export_error", "error", "condition")
  ))
}

# --- Resolution du format ----------------------------------------------------
#
#' @param format NULL (devine depuis l'extension), ou "png" / "pdf" / "svg".
#' @param filename uniquement pour la degradation svg -> png.
#' @return la valeur a passer a l'argument `device` de ggsave, ou NULL.
ts_export_resolve_format <- function(format = NULL, filename = NULL) {
  if (is.null(format)) return(NULL)
  if (length(format) != 1L || is.na(format) || !nzchar(as.character(format))) {
    return(NULL)
  }
  fmt <- tolower(as.character(format))

  # Variantes tolerees : ".pdf", "PDF", "cairo_pdf"
  fmt <- sub("^\\.", "", fmt)
  if (!fmt %in% ts_export_format_choices()) {
    .ts_export_stop(
      "invalid_format",
      sprintf(paste0("ts_export_plot() : format d'export non reconnu (%s). ",
                     "Formats acceptes : %s."),
              fmt, paste(ts_export_format_choices(), collapse = ", "))
    )
  }
  if (identical(fmt, "svg") && !requireNamespace("svglite", quietly = TRUE)) {
    warning("svglite n'est pas installe — export PNG a la place.", call. = FALSE)
    return("png")
  }
  fmt
}

# --- Helper principal --------------------------------------------------------

#' Export unifie d'un graphique (dpi + format + dimensions).
#'
#' Encapsule `ggplot2::ggsave()` : memes defauts, donc **aucun changement de
#' rendu** par rapport aux appels directs existants.
#'
#' @param filename Chemin du fichier de sortie.
#' @param plot Graphique (defaut : dernier graphique affiche).
#' @param width,height Dimensions (obligatoires en pratique ; NA = laisse
#'   `ggsave()` decider, comme avant).
#' @param dpi Resolution. Defaut **300** = defaut de ggsave ET valeur de 23/24
#'   des sites existants.
#' @param format NULL = devine depuis l'extension (comportement ggsave) ;
#'   sinon "png" / "pdf" / "svg". "svg" degrade en "png" si svglite est absent.
#' @param bg Couleur de fond. NULL = ne pas passer l'argument (defaut ggsave).
#' @param ... Transmis a `ggplot2::ggsave()` (units, limitsize, scale, ...).
#' @return `filename`, de facon invisible (comme ggsave).
#'
#' @examples
#' \dontrun{
#' ts_export_plot("volcano.png", p, width = 8, height = 6)
#' ts_export_plot("volcano.pdf", p, width = 8, height = 6, format = "pdf")
#' }
ts_export_plot <- function(filename, plot = ggplot2::last_plot(),
                           width = NA, height = NA,
                           dpi = 300L, format = NULL, bg = NULL, ...) {
  # --- Gardes (erreurs classees, messages FR, call. = FALSE) ---
  if (missing(filename) || is.null(filename) ||
      length(filename) != 1L || is.na(filename) ||
      !nzchar(as.character(filename))) {
    .ts_export_stop("invalid_filename",
                    "ts_export_plot() : nom de fichier de sortie manquant ou invalide.")
  }
  filename <- as.character(filename)

  if (is.null(plot)) {
    .ts_export_stop("invalid_plot",
                    "ts_export_plot() : aucun graphique a exporter (plot = NULL).")
  }
  if (!is.numeric(dpi) || length(dpi) != 1L || is.na(dpi) || dpi <= 0) {
    .ts_export_stop("invalid_dpi",
                    "ts_export_plot() : dpi invalide (attendu : un nombre > 0).")
  }

  device <- ts_export_resolve_format(format, filename)

  # --- Appel : on ne passe `device` / `bg` QUE s'ils sont fournis, pour
  #     reproduire exactement le comportement de chaque site d'origine. ---
  args <- list(filename = filename, plot = plot, width = width,
               height = height, dpi = dpi, ...)
  if (!is.null(device)) args$device <- device
  if (!is.null(bg))     args$bg     <- bg

  out <- do.call(ggplot2::ggsave, args)
  invisible(out)
}

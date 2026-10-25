# =============================================================================
# R/plotting/datatable.R — PLOT-S3 : wrapper DT harmonise (ts_datatable)
# =============================================================================
# Un seul endroit pour construire les tables de resultats de l'application.
#
# MESURE PREALABLE (2026-09-11) — contrairement a PLOT-S2, IL Y A UN PIEGE :
#   pageLength : 10 (x22), 15 (x15), 8 (x5), 20 (x3), 6 (x1)
#                => AUCUNE valeur par defaut neutre (meme situation que le
#                   base_size de PLOT-S1). C'est pourquoi `page_length` est
#                   OBLIGATOIRE : le helper refuse de choisir a la place de
#                   l'appelant.
#   boutons    : 1 seul site sur 49 declare extensions = "Buttons" + dom =
#                "Bfrtip" (la table pathways). Les 48 autres n'en ont aucun.
#                => `buttons = FALSE` par defaut : activer les boutons est un
#                   OPT-IN explicite, jamais un effet de bord de la migration.
#                [MISE A JOUR jalon DT-EXPORT 2026-09-14] : le defaut est
#                   desormais TS_DT_BUTTONS_DEFAULT = TRUE (contrat §6, option
#                   B) — les tables de resultats sont boutonnees ; les tables
#                   d'APERCU passent buttons = FALSE explicitement.
#
# REGLE : ZERO CHANGEMENT DE COMPORTEMENT. Chaque site migre garde ses valeurs
#         actuelles ecrites explicitement.
#
# Pure R : testable hors de Shiny, erreurs classees, messages FR, call. = FALSE.
# =============================================================================
# Cree le 2026-09-11 (PLOT-S3). Toute modification doit passer SIMULTANEMENT
# par : ce fichier, tests/testthat/test-plot-datatable.R et
# docs/contracts/PLOT_DATATABLE_CONTRACT.md.
# =============================================================================

# --- Constantes ---------------------------------------------------------------

.ts_dt_const <- function(name, fallback) {
  if (exists(name, envir = globalenv(), inherits = TRUE)) {
    get(name, envir = globalenv(), inherits = TRUE)
  } else {
    fallback
  }
}

#' Tailles de page effectivement presentes dans l'application.
#' Il n'y en a AUCUNE par defaut — voir l'en-tete du fichier.
ts_datatable_page_lengths <- function() {
  .ts_dt_const("TS_DT_PAGE_LENGTHS", c(6L, 8L, 10L, 15L, 20L))
}

#' Jeu de boutons d'export propose (utilise uniquement si `filename_base` est
#' fourni ; sans nom de fichier, DT applique ses boutons par defaut).
ts_datatable_buttons <- function(filename_base) {
  list("copy", "print",
       list(extend = "csv",   filename = filename_base),
       list(extend = "excel", filename = filename_base))
}

#' Surface publique figee (verifiee par le test de gel).
ts_datatable_public_api <- function() {
  c("ts_datatable", "ts_datatable_buttons", "ts_datatable_page_lengths",
    "ts_datatable_public_api")
}

# --- Erreurs classees --------------------------------------------------------

.ts_dt_stop <- function(state, message) {
  stop(errorCondition(
    message,
    state = state,
    class = c("plot_datatable_error", "error", "condition")
  ))
}

# --- Helper principal --------------------------------------------------------

#' Table de resultats harmonisee.
#'
#' Encapsule `DT::datatable()`. **Aucun changement de rendu** par rapport aux
#' appels directs existants, a condition de passer explicitement `page_length`.
#'
#' @param df data.frame (ou coercible) a afficher.
#' @param page_length Nombre de lignes par page. **Obligatoire** : aucune
#'   valeur par defaut n'est neutre (10, 15, 8, 20 et 6 coexistent dans l'app).
#' @param filename_base NULL = aucun bouton d'export nomme. Si fourni, ajoute
#'   les boutons copy / print / csv / excel avec ce nom de fichier.
#' @param filter Emplacement du filtre ("top" / "none" / "bottom").
#' @param rownames Afficher les noms de lignes.
#' @param scroll_x Defilement horizontal.
#' @param buttons Ajouter l'extension Buttons + `dom = "Bfrtip"`.
#'   **Defaut = `TS_DT_BUTTONS_DEFAULT` (TRUE depuis le jalon DT-EXPORT,
#'   2026-09-14 — contrat §6 option B)** : les tables de resultats sont
#'   boutonnees par defaut. Les tables d'APERCU (previews `dom = "t"/"tip"`,
#'   petites tables QC) passent `buttons = FALSE` explicitement.
#' @param dom,extensions Surcharges manuelles (prioritaires sur `buttons`).
#' @param extra_options Liste nommee fusionnee dans `options` APRES tout le
#'   reste (priorite maximale) — canal unique pour les options DT que le
#'   wrapper n'expose pas (`language`, `lengthMenu`, `order`, `autoWidth`...).
#'   Ne jamais passer `options =` via `...` : collision avec l'option interne.
#' @param ... Transmis a `DT::datatable()` (selection, caption, ...).
#' @return Un objet `DT::datatable()` (htmlwidget).
#'
#' @examples
#' \dontrun{
#' ts_datatable(df, page_length = 15)
#' ts_datatable(df, page_length = 10, buttons = TRUE, filename_base = "pathways")
#' }
ts_datatable <- function(df, page_length, filename_base = NULL,
                         filter = "top", rownames = FALSE, scroll_x = TRUE,
                         buttons = .ts_dt_const("TS_DT_BUTTONS_DEFAULT", TRUE),
                         dom = NULL, extensions = NULL, extra_options = NULL,
                         ...) {
  # --- Gardes ---
  if (missing(page_length) || is.null(page_length) ||
      length(page_length) != 1L || is.na(page_length) ||
      !is.numeric(page_length) || page_length < 1) {
    .ts_dt_stop("invalid_page_length",
                paste0("ts_datatable() : page_length est obligatoire (entier >= 1). ",
                       "Aucune valeur par defaut n'est neutre : l'application ",
                       "utilise aujourd'hui 10, 15, 8, 20 et 6 selon les tables."))
  }
  if (!is.data.frame(df)) {
    if (is.matrix(df) || is.list(df)) {
      df <- as.data.frame(df, check.names = FALSE)
    } else {
      .ts_dt_stop("invalid_data",
                  "ts_datatable() : donnees invalides (data.frame, matrix ou liste attendus).")
    }
  }
  if (!is.logical(buttons) || length(buttons) != 1L || is.na(buttons)) {
    .ts_dt_stop("invalid_buttons",
                "ts_datatable() : buttons doit etre TRUE ou FALSE.")
  }
  if (!is.null(extra_options) &&
      (!is.list(extra_options) || is.null(names(extra_options)) ||
       any(!nzchar(names(extra_options))))) {
    .ts_dt_stop("invalid_extra_options",
                paste0("ts_datatable() : extra_options doit etre une liste ",
                       "nommee (options DT fusionnees dans options)."))
  }

  # --- Options : on ne pose `dom` QUE s'il est demande (zero changement) ---
  # page_length est transmis TEL QUEL (pas de coercion) : les sites historiques
  # ecrivent pageLength = 15 (double) et le JSON produit est identique, mais on
  # ne prend aucun risque de changer la valeur serialisee.
  opts <- list(pageLength = page_length, scrollX = scroll_x)

  ext <- extensions
  if (isTRUE(buttons)) {
    if (is.null(ext)) ext <- "Buttons"
    if (is.null(dom))  dom  <- "Bfrtip"
  }
  if (!is.null(dom)) opts$dom <- dom
  if (!is.null(filename_base)) opts$buttons <- ts_datatable_buttons(filename_base)
  # Priorite maximale : extra_options ecrase les cles posees ci-dessus.
  if (!is.null(extra_options)) opts <- utils::modifyList(opts, extra_options)

  # --- Appel ---
  args <- list(data = df, filter = filter, rownames = rownames,
               options = opts, ...)
  if (!is.null(ext)) args$extensions <- ext

  do.call(DT::datatable, args)
}

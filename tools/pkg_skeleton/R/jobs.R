# =============================================================================
# R/core/jobs.R — Wrapper fin d'execution sync/async (CHRYSALIS 2D)
# =============================================================================
# REGLE 9 (AGENTS.md) : run_job() est un wrapper FIN. Aucun second framework
# asynchrone : le chemin async=TRUE imite le pattern mirai EXISTANT de
# R/spatial/spatial_async.R (mirai::mirai(...)[], collect bloquant, cf.
# .verify_spatial_daemons()) et exige un pool DEJA initialise par le module
# spatial (init_spatial_daemons(), differe a l'ouverture de l'onglet Spatial).
# R/core/jobs.R ne demarre JAMAIS de daemons lui-meme et ne depend d'aucun
# fichier de domaine : sans pool actif (ou sans 'mirai'), repli synchrone
# transparent avec un warning.
#
# Chemin sync : execution inline de fn(...) — le comportement de tous les
# modules actuels, preserve a l'identique.
#
# fn doit etre AUTONOME sur le chemin async (serialise vers le daemon) :
# une fonction pure ou une closure qui n'appelle que des fonctions preloadees
# dans le pool (cf. source_files de init_spatial_daemons()).
#
# ── DETERMINISME : le daemon n'a PAS le RNGkind de l'appelant (2026-09-16) ──
# Mesure : un daemon mirai tourne en RNGkind "L'Ecuyer-CMRG" alors que le
# processus principal tourne en "Mersenne-Twister" (defaut R) — mirai impose
# son propre flux pour des raisons de parallelisme. Consequence SILENCIEUSE :
# `set.seed(s)` DANS le job ne produit pas la meme suite que `set.seed(s)` en
# synchrone. Mesure sur Milo (miloR::makeNhoods est stochastique) : 20 -> 19
# voisinages et somme des logFC de signe INVERSE, avec le MEME `seed`
# enregistre en provenance et le meme statut `valid` — donc invisible.
# run_job() restaure donc le RNGkind de l'APPELANT dans le daemon avant fn(),
# puis rend au daemon son propre RNGkind (le pool est partage : le job suivant
# ne doit pas heriter de notre etat). `rng_kind = NULL` desactive la
# restauration (comportement mirai brut, a reserver aux jobs non stochastiques).
# =============================================================================

#' Extraire un message lisible d'une erreur R ou d'une errorValue mirai
.err_job_message <- function(e) {
  if (inherits(e, "condition")) {
    conditionMessage(e)
  } else {
    tryCatch(paste(as.character(e)[1], collapse = " "), error = function(e2) "Erreur mirai inconnue")
  }
}

#' Executer un calcul long via le contrat commun sync/async
#'
#' @param fn Fonction a executer (autonome si async=TRUE, voir en-tete).
#' @param ... Arguments passes a fn().
#' @param async Si TRUE et qu'un pool mirai est actif, delegation au pattern
#'   spatial (collect BLOQUANT — l'integration non-bloquante ExtendedTask des
#'   modules n'est pas du ressort de ce wrapper). Sans pool : warning + repli
#'   synchrone. Si FALSE (defaut) : execution inline, comportement historique.
#' @param on_progress Callback optionnel function(message character) appele
#'   aux etapes "demarrage"/"termine" (+"soumission mirai" en async). Les
#'   erreurs du callback sont ignorees (jamais fatales au job).
#' @param on_error Callback optionnel function(condition) : si fourni, une
#'   erreur de fn() est transmise au callback et run_job() retourne NULL
#'   (invisible) — le module garde la main sur l'UI. Si NULL (defaut),
#'   l'erreur est relancee (comportement historique des tryCatch modules).
#' @param timeout_ms Plafond du job mirai en ms (argument .timeout natif de
#'   mirai ; NULL = pas de plafond propre au wrapper, les plafonds spatiaux
#'   MIRAI_TASK_TIMEOUT_MS etc. restent geres par les modules).
#' @param rng_kind Vecteur RNGkind() a restaurer DANS le daemon avant fn()
#'   (defaut : le RNGkind de l'appelant, evalue paresseusement dans le
#'   processus appelant). Sans cela un calcul stochastique ne rend PAS le meme
#'   resultat qu'en synchrone (voir l'en-tete). NULL = ne rien restaurer.
#' @return Le resultat de fn(...), ou NULL (invisible) si on_error a absorbe
#'   une erreur.
run_job <- function(fn, ..., async = FALSE, on_progress = NULL, on_error = NULL,
                    timeout_ms = NULL, rng_kind = RNGkind()) {
  if (!is.function(fn)) {
    stop("run_job() : 'fn' doit etre une fonction.", call. = FALSE)
  }
  .progress <- function(msg) {
    if (is.function(on_progress)) {
      tryCatch(on_progress(msg), error = function(e) NULL)
    }
  }
  .handle_error <- function(e) {
    if (is.function(on_error)) {
      tryCatch(on_error(e), error = function(e2) NULL)
      invisible(NULL)
    } else {
      stop(e)
    }
  }
  .run_sync <- function() {
    .progress("demarrage")
    tryCatch(
      {
        res <- fn(...)
        .progress("termine")
        res
      },
      error = function(e) .handle_error(e)
    )
  }

  if (!isTRUE(async)) return(.run_sync())

  # ── Chemin async — delegation au pattern mirai existant ───────────────────
  if (!requireNamespace("mirai", quietly = TRUE)) {
    warning("Package 'mirai' absent : execution synchrone de secours pour ce job.",
            call. = FALSE)
    return(.run_sync())
  }
  if (!mirai::daemons_set()) {
    warning("Aucun pool mirai actif (init_spatial_daemons() non appele) : ",
            "execution synchrone de secours pour ce job.", call. = FALSE)
    return(.run_sync())
  }

  .progress("soumission mirai")
  job_args <- list(...)
  # Le RNGkind de l'appelant est fige ICI (processus principal) et voyage avec
  # le job : le daemon ne peut pas le deviner.
  rng_kind_job <- if (is.null(rng_kind)) NULL else as.character(rng_kind)
  res <- tryCatch(
    mirai::mirai(
      {
        # Restauration du RNGkind de l'appelant, puis remise en etat du daemon.
        # tryCatch imbrique plutot que on.exit : on ne depend pas de la
        # semantique de frame du daemon mirai, et l'erreur du job reste
        # remontee a l'appelant comme une valeur (traitee plus bas).
        .prev_kind <- RNGkind()
        .out <- tryCatch(
          {
            if (!is.null(rng_kind_job)) {
              do.call(RNGkind, as.list(rng_kind_job))
            }
            do.call(fn, job_args)
          },
          error = function(e) e
        )
        tryCatch(do.call(RNGkind, as.list(.prev_kind)), error = function(e2) NULL)
        .out
      },
      fn = fn, job_args = job_args, rng_kind_job = rng_kind_job,
      .timeout = timeout_ms
    )[],
    error = function(e) e
  )

  err <- NULL
  if (inherits(res, "error")) {
    err <- res
  } else if (requireNamespace("mirai", quietly = TRUE) &&
             isTRUE(mirai::is_error_value(res))) {
    # Timeout mirai ou echec daemon : errorValue (pas une condition R native)
    err <- simpleError(.err_job_message(res))
  }
  if (!is.null(err)) return(.handle_error(err))

  .progress("termine")
  res
}

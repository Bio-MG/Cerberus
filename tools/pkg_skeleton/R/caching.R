# =============================================================================
# R/core/caching.R — Memoisation minimale a portee contrainte (CHRYSALIS 2D)
# =============================================================================
# PERIMETRE VOLONTAIREMENT LIMITE (regle 9 AGENTS.md) : seuls CINQ calculs
# peuvent etre caches — trajectory, velocity, markers, pathways,
# spatial_deconv. Toute autre portee est refusee a la construction de cle
# comme a l'acces : le garde-fou est dans le code, pas dans la discipline.
#
# Stockage : environnement au niveau du PROCESSUS R (partage entre sessions
# Shiny du meme process, comme les daemons mirai). Les resultats de ces cinq
# calculs etant lourds, une eviction FIFO borne la memoire retenue
# (CERBERUS_CACHE_MAX_ENTRIES).
#
# Format de cle : "<scope>:<hash xxhash64>" — le scope est porté par la cle
# pour que cerberus_cache_get()/cerberus_cache_set() puissent refuser une cle
# forgée hors perimetre. Storer NULL est un no-op : une absence et une valeur
# NULL sont indistinguables (convention "pas encore calcule" du schema d'etat).
# =============================================================================

#' Portees de cache autorisees (calculs cibles par la regle 9)
CERBERUS_CACHE_SCOPES <- c(
  "trajectory", "velocity", "markers", "pathways", "spatial_deconv"
)

#' Nombre maximal d'entrees retenues (eviction FIFO au-dela)
CERBERUS_CACHE_MAX_ENTRIES <- 32L

.cerberus_cache_env <- new.env(parent = emptyenv())
.cerberus_cache_env$store <- new.env(parent = emptyenv())
.cerberus_cache_env$order <- character(0)

.assert_cache_scope <- function(scope) {
  if (!scope %in% CERBERUS_CACHE_SCOPES) {
    stop(sprintf(
      "Portee de cache '%s' non autorisee (autorisées uniquement : %s).",
      scope, paste(CERBERUS_CACHE_SCOPES, collapse = ", ")
    ), call. = FALSE)
  }
  invisible(scope)
}

#' Cle de cache deterministe pour un calcul du perimetre
#'
#' @param scope Une valeur de CERBERUS_CACHE_SCOPES.
#' @param ... Tout ingredients determinants du calcul (matrices, parametres,
#'   identifiants) — serialises et haches via digest (xxhash64).
#' @return Chaine "<scope>:<hash>". Deux appels d'arguments identiques
#'   produisent la MEME cle (determinisme teste).
cerberus_cache_key <- function(scope, ...) {
  .assert_cache_scope(scope)
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package 'digest' requis pour le cache Cerberus (cerberus_cache_key).",
         call. = FALSE)
  }
  paste0(scope, ":", digest::digest(list(...), algo = "xxhash64"))
}

#' Verifier qu'une cle appartient bien au perimetre autorise
.cerberus_cache_check_key <- function(key) {
  scope <- sub(":.*$", "", key)
  .assert_cache_scope(scope)
  invisible(scope)
}

#' Lire une valeur du cache (NULL = absent)
#'
#' @param key Cle produite par cerberus_cache_key().
#' @return La valeur stockee, ou NULL si absente (ou cle hors perimetre —
#'   dans ce cas une erreur est levee, pas de retour silencieux).
cerberus_cache_get <- function(key) {
  .cerberus_cache_check_key(key)
  if (!exists(key, envir = .cerberus_cache_env$store, inherits = FALSE)) {
    return(NULL)
  }
  .cerberus_cache_env$store[[key]]
}

#' Stocker une valeur dans le cache (no-op si value est NULL)
#'
#' @param key Cle produite par cerberus_cache_key().
#' @param value Valeur a memoiser (NULL : no-op explicite).
#' @return invisible(key).
cerberus_cache_set <- function(key, value) {
  .cerberus_cache_check_key(key)
  if (is.null(value)) return(invisible(key))

  .cerberus_cache_env$store[[key]] <- value
  # FIFO : la cle re-set passe en fin de file ; on evite au-dela du plafond.
  ord <- c(.cerberus_cache_env$order[.cerberus_cache_env$order != key], key)
  while (length(ord) > CERBERUS_CACHE_MAX_ENTRIES) {
    evicted <- ord[1]
    ord <- ord[-1]
    remove(list = evicted, envir = .cerberus_cache_env$store,
           inherits = FALSE)
  }
  .cerberus_cache_env$order <- ord
  invisible(key)
}

#' Vider entierement le cache (hygiene de test / liberation memoire manuelle)
#'
#' @return invisible(TRUE).
cerberus_cache_clear <- function() {
  .cerberus_cache_env$store <- new.env(parent = emptyenv())
  .cerberus_cache_env$order <- character(0)
  invisible(TRUE)
}

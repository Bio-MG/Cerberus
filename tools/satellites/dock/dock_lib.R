# =============================================================================
# tools/satellites/dock/dock_lib.R — N3 cerberus-dock : lib PUR
# =============================================================================
# Fiche (fait foi) : PROPOSAL_N_SATELLITES_LAUNCH_ROADMAP_2026-10-26.md §2-S7 ;
# contrat figé : tools/satellites/dock/MANIFEST.md v1.0.0.
#
# RÔLE : helpers PURS du dock (charge/valide le manifeste, décision de santé,
# chemin de pid-file) — testables hors session, sans réseau, sans process
# (test-cerberus-dock.R). Les EFFETS (spawn/kill/HTTP) vivent dans run_dock.R,
# qui consomme cette lib. Ce fichier est sous tools/ : hors conventions gate
# par construction (note MANIFEST.md §1) — le test éponyme est la seule porte.
#
# Règles portées : format FIGÉ 1.0.0 (version autre = refus, jamais interprétée ;
# champ inconnu = refus) · O6 = shares_app_process false + ports uniques ·
# validates never stop() (TRUE/FALSE), les loaders stoppent (français,
# call. = FALSE) sur fichier illisible.
# =============================================================================

# jsonlite (simplifyVector = FALSE) rend les tableaux de scalaires des LISTES
# de strings — normaliser en character vector pour les champs argv/reads/refs.
.as_char_vec <- function(x) {
  if (is.character(x)) return(x)
  if (is.list(x) && length(x) > 0L && all(vapply(x, function(e)
    is.character(e) && length(e) == 1L && !is.na(e), logical(1)))) {
    return(vapply(x, function(e) e[[1]], character(1)))
  }
  NULL
}

#' Charge le manifeste JSON (stop clair s'il est illisible ou malformé).
ts_dock_manifest_load <- function(path) {
  if (!file.exists(path)) {
    stop(paste0("Manifeste dock introuvable : ", path), call. = FALSE)
  }
  json <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) {
      stop(paste0("Manifeste dock illisible (JSON) : ", path, " — ",
                  conditionMessage(e)), call. = FALSE)
    }
  )
  json
}

#' Valide le manifeste contre le format FIGÉ v1.0.0 — jamais stop() (TRUE/FALSE).
ts_dock_manifest_valid <- function(manifest) {
  if (!is.list(manifest) || !all(c("format_version", "satellites") %in% names(manifest)))
    return(FALSE)
  # frozen format : TOUT autre champ top-level est refusé
  if (!identical(sort(names(manifest)), c("format_version", "satellites")))
    return(FALSE)
  # version figée — autre valeur = refus honnête (jamais interprétée)
  if (!identical(manifest$format_version, "1.0.0")) return(FALSE)
  sats <- manifest$satellites
  if (!is.list(sats) || length(sats) == 0L) return(FALSE)
  needed <- c("name", "command", "cwd", "port", "health", "capability",
              "enabled", "notes")
  names_seen <- character(0)
  ports <- numeric(0)
  for (s in sats) {
    if (!is.list(s) || !all(needed %in% names(s))) return(FALSE)
    if (!identical(sort(names(s)), sort(needed))) return(FALSE)
    if (!is.character(s$name) || length(s$name) != 1L ||
        !grepl("^[a-z0-9_]+$", s$name)) return(FALSE)
    names_seen <- c(names_seen, s$name)
    cmd <- .as_char_vec(s$command)
    if (is.null(cmd) || length(cmd) < 1L || any(!nzchar(cmd)))
      return(FALSE)
    if (!is.character(s$cwd) || length(s$cwd) != 1L || !nzchar(s$cwd))
      return(FALSE)
    if (!is.numeric(s$port) || length(s$port) != 1L ||
        is.na(s$port) || s$port != as.integer(s$port) ||
        s$port < 1 || s$port > 65535)
      return(FALSE)
    ports <- c(ports, s$port)
    h <- s$health
    if (!is.list(h) || !all(c("url", "expect_status", "timeout_s", "interval_s") %in% names(h)))
      return(FALSE)
    if (!is.character(h$url) || length(h$url) != 1L ||
        !grepl("^https?://", h$url)) return(FALSE)
    if (!is.numeric(h$expect_status) || length(h$expect_status) != 1L ||
        is.na(h$expect_status))
      return(FALSE)
    if (!is.numeric(h$timeout_s) || length(h$timeout_s) != 1L ||
        is.na(h$timeout_s) || h$timeout_s <= 0) return(FALSE)
    if (!is.numeric(h$interval_s) || length(h$interval_s) != 1L ||
        is.na(h$interval_s) || h$interval_s <= 0) return(FALSE)
    if (!is.character(s$capability) || length(s$capability) != 1L || !nzchar(s$capability))
      return(FALSE)
    if (!is.logical(s$enabled) || length(s$enabled) != 1L || is.na(s$enabled))
      return(FALSE)
    if (!is.character(s$notes) || length(s$notes) != 1L) return(FALSE)
  }
  if (anyDuplicated(names_seen) > 0L) return(FALSE)
  if (anyDuplicated(ports) > 0L) return(FALSE) # un port par satellite (O6)
  TRUE
}

#' Valide un capability contre le schéma figé v1.0.0 (MANIFEST.md §3).
ts_dock_capability_valid <- function(capability) {
  if (!is.list(capability)) return(FALSE)
  needed <- c("capability_version", "satellite", "satellite_version", "role",
              "surface", "process", "contract_refs")
  if (!all(needed %in% names(capability))) return(FALSE)
  if (!identical(sort(names(capability)), sort(needed))) return(FALSE)
  if (!identical(capability$capability_version, "1.0.0")) return(FALSE)
  for (k in c("satellite", "satellite_version", "role")) {
    if (!is.character(capability[[k]]) || length(capability[[k]]) != 1L ||
        !nzchar(capability[[k]])) return(FALSE)
  }
  surf <- capability$surface
  if (!is.list(surf) ||
      !identical(sort(names(surf)), c("entry", "kind", "reads", "writes")))
    return(FALSE)
  if (!surf$kind %in% c("static_page", "r_script")) return(FALSE) # vocabulaire fermé v1
  if (!is.character(surf$entry) || length(surf$entry) != 1L || !nzchar(surf$entry))
    return(FALSE)
  if (is.null(.as_char_vec(surf$reads))) return(FALSE)
  if (!is.list(surf$writes) || length(surf$writes) != 0L)
    return(FALSE) # v1 : satellites read-only (writes vide)
  proc <- capability$process
  if (!is.list(proc) ||
      !identical(sort(names(proc)), c("kind", "owner", "shares_app_process")))
    return(FALSE)
  if (!proc$kind %in% c("static_http_server", "r_process")) return(FALSE)
  if (!identical(proc$owner, "dock")) return(FALSE)
  if (!identical(proc$shares_app_process, FALSE)) return(FALSE) # O6 — verrou
  if (is.null(.as_char_vec(capability$contract_refs)) ||
      length(.as_char_vec(capability$contract_refs)) < 1L)
    return(FALSE)
  TRUE
}

#' L'entrée d'un satellite par nom — NULL si inconnu (jamais stop).
ts_dock_satellite <- function(manifest, name) {
  if (!is.list(manifest) || !is.list(manifest$satellites)) return(NULL)
  for (s in manifest$satellites) {
    if (is.list(s) && identical(s$name, name)) return(s)
  }
  NULL
}

#' Décision PURE de santé — jamais stop(), verdict OK/FAIL/TIMEOUT.
#'
#' FAIL si le status est faux (PRIME sur le timeout : le status est le signal
#' le plus informatif) ; TIMEOUT si le délai dépasse le budget avec un status
#' bon ou absent ; NA n'importe où = FAIL (sonde malformée = verdict
#' défavorable honnête, jamais une erreur).
ts_dock_health_verdict <- function(status_code, expect_status, elapsed_s, timeout_s) {
  bad <- function(x) is.null(x) || is.na(x) || !is.finite(x)
  if (bad(status_code) || bad(expect_status)) {
    return(list(verdict = "FAIL",
                detail = "sonde malformée (status manquant)"))
  }
  if (bad(elapsed_s) || bad(timeout_s)) {
    return(list(verdict = "FAIL",
                detail = "sonde malformée (délai manquant)"))
  }
  if (as.integer(status_code) == as.integer(expect_status)) {
    if (elapsed_s > timeout_s) {
      return(list(verdict = "TIMEOUT",
                  detail = sprintf("%.1fs > budget %.1fs", elapsed_s, timeout_s)))
    }
    return(list(verdict = "OK",
                detail = sprintf("HTTP %s en %.1fs", as.integer(status_code), elapsed_s)))
  }
  list(verdict = "FAIL",
       detail = sprintf("HTTP %s (attendu %s) en %.1fs",
                        as.integer(status_code), as.integer(expect_status), elapsed_s))
}

#' Chemin FIGÉ du pid-file d'un satellite : tools/_drive/satellites/<name>.pid
#' (répertoire runtime gitigné — jamais commité ; le nom du satellite est
#' validé [a-z0-9_]+ par le manifeste, aucun caractère de chemin possible).
ts_dock_pid_file <- function(project_root, name) {
  file.path(project_root, "tools", "_drive", "satellites", paste0(name, ".pid"))
}

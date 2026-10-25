# =============================================================================
# R/core/rdata_io.R — Inspection & extraction securisee des fichiers .rda/.RData
# =============================================================================
# Contrat : docs/contracts/RDATA_IMPORT_CONTRACT.md (geler via
# tests/testthat/test-rdata-contract-freeze.R).
#
# Principe "Inspecter d'abord, importer ensuite" : un workspace R sauvegarde
# (save.image() ou liste mixte) n'est JAMAIS charge dans l'environnement
# global de l'application. Le fichier est ouvert dans un environnement
# isolé (parent = emptyenv()) que l'appelant (module d'import) garde en
# memoire le temps de la session de preview ; l'utilisateur choisit un objet,
# sa classe est validee AVANT toute ecriture dans global_data.
#
# Regles de securite :
#   - jamais de attach() ni de load() vers globalenv ;
#   - la classification (rdata_classify_object) est AFFICHAGE UNIQUEMENT —
#     elle ne sert jamais a selectionner automatiquement un objet ;
#   - erreurs francaises classees (errorCondition).
# =============================================================================

# Codes de classification neutral-linguistiquement (affichages traduits cote
# module via i18n). Geles par test-rdata-contract-freeze.R.
.RDATA_TYPE_CODES <- c(
  "seurat", "sce", "cellchat", "velocity", "metadata",
  "matrix", "image", "other"
)

# Sous-ensemble signature du contrat velocity (liste nommee contenant au
# minimum spliced + unspliced — meme signature que read_velocity_rds(),
# R/sc/sc_velocity.R, qui reste le validateur faisant foi).
.RDATA_VELOCITY_SIGNATURE <- c("spliced", "unspliced")


# Erreurs classees du domaine : message francais, sans trace d'appel, classe
# "rdata_import_error" (attrapable par tryCatch cote modules).
.rdata_stop <- function(msg) {
  stop(errorCondition(msg, class = "rdata_import_error", call = NULL))
}

#' Extensions .RData supportees (consomme TS_IMPORT_RDA_EXTENSIONS si declare)
#'
#' @return Vecteur character des extensions (sans point, minuscules).
rdata_supported_extensions <- function() {
  if (exists("TS_IMPORT_RDA_EXTENSIONS", inherits = TRUE)) {
    exts <- tolower(gsub("^\\.", "", TS_IMPORT_RDA_EXTENSIONS))
    if (length(exts) > 0L) return(exts)
  }
  c("rda", "rdata")
}

#' Verifier qu'un chemin est un fichier .rda/.RData supporte
#'
#' @param path Chemin du fichier (datapath d'upload accepte).
#' @return TRUE ou FALSE (jamais d'erreur).
rdata_is_supported_file <- function(path) {
  if (is.null(path) || !nzchar(path)) return(FALSE)
  tolower(tools::file_ext(path)) %in% rdata_supported_extensions()
}

#' Charger un fichier .rda/.RData dans un environnement isole
#'
#' Le fichier est INTEGRALEMENT charge en memoire dans un environnement
#' nouveau (parent = emptyenv()) — jamais globalenv. L'appelant garde
#' l'environnement (reactiveVal) et doit appeler rdata_free() pour liberer.
#'
#' @param path Chemin du fichier .rda/.RData.
#' @return L'environnement contenant les objets du fichier.
rdata_load_env <- function(path) {
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    .rdata_stop(
      "Chemin de fichier .RData invalide (vide ou absent)."
    )
  }
  if (!file.exists(path)) {
    .rdata_stop(
      sprintf("Fichier .RData introuvable : %s.", path)
    )
  }
  env <- new.env(parent = emptyenv())
  res <- tryCatch({ load(path, envir = env); TRUE },
                  error = function(e) e)
  if (!identical(res, TRUE)) {
    .rdata_stop(
      sprintf("Impossible de lire le fichier .RData : %s.",
              conditionMessage(res))
    )
  }
  if (length(ls(envir = env)) == 0L) {
    .rdata_stop(
      "Le fichier .RData ne contient aucun objet."
    )
  }
  invisible(env)
}

#' Classifier un objet (code neutre, AFFICHAGE UNIQUEMENT)
#'
#' Ne sert JAMAIS a selectionner automatiquement un objet a importer —
#' la selection appartient a l'utilisateur, la validation a rdata_assert_class.
#'
#' @param obj Objet R quelconque.
#' @return Character scalar parmi .RDATA_TYPE_CODES.
rdata_classify_object <- function(obj) {
  if (inherits(obj, "Seurat")) return("seurat")
  if (inherits(obj, "SingleCellExperiment")) return("sce")
  if (inherits(obj, "CellChat")) return("cellchat")
  if (is.list(obj) && !is.data.frame(obj) && !is.null(names(obj)) &&
      all(.RDATA_VELOCITY_SIGNATURE %in% names(obj))) return("velocity")
  if (is.data.frame(obj)) return("metadata")
  if (is.matrix(obj) && is.numeric(obj)) return("matrix")
  if (!is.matrix(obj) && inherits(obj, "Matrix") && is.numeric(as.vector(obj[1:min(100, length(obj))]))) {
    return("matrix")
  }
  if (inherits(obj, c("gg", "ggplot", "grob", "raster", "recordedplot", "rasterlite"))) {
    return("image")
  }
  "other"
}

#' Decrire le contenu d'un environnement .RData (table de preview)
#'
#' @param env Environnement retourne par rdata_load_env().
#' @return data.frame : name, class, dimensions, size_mb, type_code.
#'         Zero ligne si l'environnement est vide.
rdata_describe_objects <- function(env) {
  if (!is.environment(env)) {
    .rdata_stop(
      "rdata_describe_objects() requiert un environnement (rdata_load_env)."
    )
  }
  empty_df <- data.frame(
    name = character(0), class = character(0), dimensions = character(0),
    size_mb = numeric(0), type_code = character(0),
    stringsAsFactors = FALSE
  )
  nms <- ls(envir = env)
  if (length(nms) == 0L) return(empty_df)
  info <- lapply(nms, function(nm) {
    obj <- tryCatch(get(nm, envir = env), error = function(e) NULL)
    .rdata_describe_row(nm, obj)
  })
  do.call(rbind, info)
}

# Ligne de description d'un objet (usage interne : describe_objects + flatten).
.rdata_describe_row <- function(path, obj) {
  # dim() peut echouer sur un objet incomplet/invalide sauvegarde par une
  # autre session (classes S4/S3 sans slots attendus) — l'inspection ne
  # doit JAMAIS echouer : dimensions "-" dans ce cas.
  d <- tryCatch(if (is.null(obj)) NULL else dim(obj), error = function(e) NULL)
  data.frame(
    name = path,
    class = if (is.null(obj)) "?" else paste(class(obj), collapse = "/"),
    dimensions = if (is.null(d) || length(d) == 0L) "-" else paste(d, collapse = " x "),
    size_mb = if (is.null(obj)) NA_real_
              else round(as.numeric(object.size(obj)) / 1024^2, 2),
    type_code = if (is.null(obj)) "other" else rdata_classify_object(obj),
    stringsAsFactors = FALSE
  )
}

# Un conteneur nommé est explorable : liste nommée hors S4/data.frame/env.
.rdata_is_named_container <- function(obj) {
  is.list(obj) && !is.data.frame(obj) && !isS4(obj) &&
    !is.environment(obj) && length(names(obj)) > 0L
}

#' Aplatir le contenu d'un environnement en chemins explorables
#'
#' Explore les listes nommees (ex. un objet unique `data_humanSkin` contenant
#' `$data`, `$meta`, voire `$data$NL`/`$data$LS`) et produit UNE ligne par
#' noeud : l'objet top-level ET ses descendants, sous forme de chemins
#' `objet$enfant$...` extractibles via rdata_extract_path(). Les S4 (Seurat,
#' CellChat...) et data.frames sont des feuilles (jamais descendus).
#'
#' @param env Environnement retourne par rdata_load_env()/rdata_read_file_env().
#' @param max_depth Profondeur maximale d'exploration (1 = top-level seul).
#' @param max_rows Nombre maximal de lignes produites (garde-fou).
#' @return data.frame name (chemin), class, dimensions, size_mb, type_code.
rdata_flatten_env <- function(env, max_depth = 3L, max_rows = 500L) {
  if (!is.environment(env)) {
    .rdata_stop(
      "rdata_flatten_env() requiert un environnement (rdata_load_env)."
    )
  }
  rows <- list()
  recurse <- function(prefix, obj, depth) {
    if (length(rows) >= max_rows) return(invisible(NULL))
    obj <- tryCatch(obj, error = function(e) NULL)
    if (is.null(obj)) return(invisible(NULL))
    rows[[length(rows) + 1L]] <<- .rdata_describe_row(prefix, obj)
    if (depth >= max_depth || !.rdata_is_named_container(obj)) {
      return(invisible(NULL))
    }
    for (nm in names(obj)) {
      if (length(rows) >= max_rows) break
      if (is.na(nm) || !nzchar(nm)) next  # enfant sans nom : non adressable
      child <- tryCatch(obj[[nm]], error = function(e) NULL)
      if (is.null(child)) next  # enfant NULL : pas de ligne, pas de descente
      recurse(paste0(prefix, "$", nm), child, depth + 1L)
    }
    invisible(NULL)
  }
  for (nm in ls(envir = env)) {
    recurse(nm, tryCatch(get(nm, envir = env), error = function(e) NULL), 1L)
    if (length(rows) >= max_rows) break
  }
  if (length(rows) == 0L) {
    return(data.frame(name = character(0), class = character(0),
                      dimensions = character(0), size_mb = numeric(0),
                      type_code = character(0), stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

#' Extraire un objet par chemin explore (ex. "data_humanSkin$data$NL")
#'
#' @param env Environnement retourne par rdata_load_env()/rdata_read_file_env().
#' @param path Chemin "objet$enfant$..." tel que produit par
#'   rdata_flatten_env().
#' @return L'objet situe au chemin.
rdata_extract_path <- function(env, path) {
  if (!is.environment(env)) {
    .rdata_stop(
      "rdata_extract_path() requiert un environnement (rdata_load_env)."
    )
  }
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    .rdata_stop("Chemin d'objet .RData invalide (vide ou multiple).")
  }
  parts <- strsplit(path, "$", fixed = TRUE)[[1]]
  if (length(parts) == 0L || !parts[1] %in% ls(envir = env)) {
    .rdata_stop(
      sprintf("Objet '%s' introuvable dans le fichier .RData.", path)
    )
  }
  obj <- get(parts[1], envir = env)
  if (length(parts) > 1L) {
    for (p in parts[-1]) {
      # [[ ]] sur une liste retourne NULL silencieusement pour un nom absent :
      # distinguer "introuvable" (nom absent) de "vide" (element NULL reel).
      if (is.list(obj) && !is.null(names(obj)) && !p %in% names(obj)) {
        .rdata_stop(sprintf("Chemin '%s' introuvable (element '%s').", path, p))
      }
      obj <- tryCatch(obj[[p]], error = function(e) {
        .rdata_stop(sprintf("Chemin '%s' introuvable (element '%s').", path, p))
      })
      if (is.null(obj)) {
        .rdata_stop(sprintf("Chemin '%s' : element '%s' est vide (NULL).", path, p))
      }
    }
  }
  obj
}

#' Verifier qu'un fichier est explorable (.rda/.RData/.rds)
#'
#' Les .rds font partie du perimetre "Inspect & Select" : un .rds contenant
#' une liste nommee (workspace, objet CellChat tutorial...) est explore
#' comme un .rda.
rdata_is_explorable_file <- function(path) {
  if (is.null(path) || !nzchar(path)) return(FALSE)
  tolower(tools::file_ext(path)) %in% c(rdata_supported_extensions(), "rds")
}

#' Nom R valide pour ranger un objet lu d'un .rds dans un environnement
rdata_safe_name <- function(path) {
  nm <- make.names(tolower(tools::file_path_sans_ext(basename(path))))
  if (!nzchar(nm) || nm == "NA") nm <- "objet"
  nm
}

#' Charger un fichier explorable (.rda/.RData/.rds) dans un environnement isole
#'
#' Pour un .rds, l'objet unique est range sous un nom valide derive du nom
#' de fichier (les descendants restent adressables via les chemins).
rdata_read_file_env <- function(path) {
  if (!is.character(path) || length(path) != 1L || !nzchar(path) ||
      !file.exists(path)) {
    .rdata_stop(sprintf("Fichier explorable introuvable : %s.", path))
  }
  ext <- tolower(tools::file_ext(path))
  if (ext %in% rdata_supported_extensions()) return(rdata_load_env(path))
  if (identical(ext, "rds")) {
    obj <- tryCatch(readRDS(path), error = function(e) {
      .rdata_stop(sprintf("Impossible de lire le fichier .rds : %s.",
                          conditionMessage(e)))
    })
    if (is.null(obj)) {
      .rdata_stop("Le fichier .rds est vide (NULL).")
    }
    env <- new.env(parent = emptyenv())
    assign(rdata_safe_name(path), obj, envir = env)
    return(invisible(env))
  }
  .rdata_stop(sprintf(
    "Format non supporte pour l'exploration : .%s (attendu : .rda, .RData, .rds).",
    ext
  ))
}

#' Extraire UN objet nomme d'un environnement .RData
#'
#' @param env Environnement retourne par rdata_load_env().
#' @param object_name Nom exact de l'objet (character scalar non vide).
#' @return L'objet extrait.
rdata_extract_object <- function(env, object_name) {
  if (!is.environment(env)) {
    .rdata_stop(
      "rdata_extract_object() requiert un environnement (rdata_load_env)."
    )
  }
  if (!is.character(object_name) || length(object_name) != 1L || !nzchar(object_name)) {
    .rdata_stop(
      "Nom d'objet .RData invalide (vide ou multiple)."
    )
  }
  if (!object_name %in% ls(envir = env)) {
    .rdata_stop(
      sprintf("Objet '%s' introuvable dans le fichier .RData.", object_name)
    )
  }
  get(object_name, envir = env)
}

#' Valider la classe d'un objet AVANT import dans global_data
#'
#' Style de la famille assert_* de R/core/validation.R : message francais
#' clair, call. = FALSE, retour invisible (pipable). expected = NULL accepte
#' tout objet (l'appelant assume alors sa propre validation aval).
#'
#' @param obj Objet extrait (rdata_extract_object).
#' @param expected Vecteur character des classes acceptees
#'   (ex. c("Seurat", "SingleCellExperiment", "matrix", "dgCMatrix")) ;
#'   NULL desactive la verification.
#' @param context Contexte cite dans le message d'erreur (ex. "import single-cell").
#' @return L'objet, invisible.
rdata_assert_class <- function(obj, expected = NULL, context = "import .RData") {
  if (is.null(expected)) return(invisible(obj))
  if (is.null(obj) || !inherits(obj, expected)) {
    stop(sprintf(
      paste0("Echec %s : l'objet selectionne est de classe '%s', ",
             "mais l'import attend l'une des classes : %s."),
      context,
      if (is.null(obj)) "NULL" else paste(class(obj), collapse = "/"),
      paste(expected, collapse = ", ")
    ), call. = FALSE)
  }
  invisible(obj)
}

#' Exporter une selection d'objets vers un nouveau fichier .RData
#'
#' Utilise par le bouton "Exporter la selection" de la preview : les objets
#' sont SAUVEGARDES HORS de l'application (jamais ecrits dans global_data).
#'
#' @param env Environnement retourne par rdata_load_env().
#' @param object_names Noms d'objets a sauvegarder.
#' @param file Chemin de destination (downloadHandler fournit un fichier temp).
#' @return Les noms effectivement exportes, invisible.
rdata_export_selection <- function(env, object_names, file) {
  if (!is.environment(env)) {
    .rdata_stop(
      "rdata_export_selection() requiert un environnement (rdata_load_env)."
    )
  }
  nms <- intersect(as.character(object_names), ls(envir = env))
  if (length(nms) == 0L) {
    .rdata_stop(
      "Aucun objet valide dans la selection a exporter."
    )
  }
  save(list = nms, file = file, envir = env)
  invisible(nms)
}

#' Exporter des chemins explores vers un nouveau fichier .RData
#'
#' Variante de rdata_export_selection() pour des chemins imbriques
#' ("data_humanSkin$data$NL") : chaque objet extrait est range dans un
#' environnement propre sous un nom R valide derive du chemin ($ -> _),
#' puis sauvegarde HORS de l'application.
#'
#' @param env Environnement source (rdata_load_env()/rdata_read_file_env()).
#' @param paths Chemins tels que produits par rdata_flatten_env().
#' @param file Chemin de destination (downloadHandler fournit un fichier temp).
#' @return Les noms R effectivement exportes, invisible.
rdata_export_paths <- function(env, paths, file) {
  if (!is.environment(env)) {
    .rdata_stop(
      "rdata_export_paths() requiert un environnement (rdata_load_env)."
    )
  }
  paths <- as.character(paths)
  if (length(paths) == 0L) {
    .rdata_stop("Aucun objet valide dans la selection a exporter.")
  }
  bundle <- new.env(parent = emptyenv())
  for (p in paths) {
    obj <- rdata_extract_path(env, p)
    nm <- make.names(gsub("$", "_", p, fixed = TRUE))
    while (nm %in% ls(envir = bundle)) nm <- paste0(nm, "_2")
    assign(nm, obj, envir = bundle)
  }
  save(list = ls(envir = bundle), file = file, envir = bundle)
  invisible(ls(envir = bundle))
}

#' Vider un environnement .RData (liberation memoire)
#'
#' @param env Environnement retourne par rdata_load_env() (NULL tolerated).
#' @return invisible(NULL).
rdata_free <- function(env) {
  if (is.environment(env)) {
    rm(list = ls(envir = env, all.names = TRUE), envir = env)
  }
  invisible(NULL)
}

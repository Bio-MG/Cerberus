# =============================================================================
# R/bulk/bulk_network.R — Interactome LOCAL + conversion d'identifiants (NEW-3)
# =============================================================================
# Jalons N-1 (conversion UniProt -> SYMBOL, taux tracé), N-2 (chargement +
# mémoïsation du réseau Reactome dédupliqué) et N-3 (moteur PCSF heuristique
# déterministe) de la proposition docs/proposals/V1X_PCSF_NETWORK_PROPOSAL.md §10.
# N-4 (contrat gelé + freeze test + module UI) vit dans
# docs/contracts/BULK_NETWORK_CONTRACT.md et modules/bulk/.
#
# -----------------------------------------------------------------------------
# MESURES DU 2026-09-16 — elles CORRIGENT la proposition §1.3
# -----------------------------------------------------------------------------
# La sonde de la proposition annonçait « 19 107 arêtes dédupliquées, degré moyen
# ~1,7, graphe creux ». Re-mesuré ici, sur `graphite::edges(which = "protein")`
# agrégé sur les 2 593 voies Reactome (reactome.db 1.96.0) :
#
#   voies Reactome ....................... 2 593 (lues hors ligne)
#   arêtes brutes (which = "protein") .... 2 128 244
#   dont 6 à type NON-UniProt ............ écartées ET comptées
#   arêtes UniProt brutes ................ 2 128 238
#   arêtes DÉDUPLIQUÉES (paire non orientée) 292 895   (annoncé : 19 107)
#   nœuds uniques ........................ 11 030
#   degré moyen .......................... 53,1        (annoncé : ~1,7)
#   densité .............................. 0,004815
#   composantes .......................... 54, dont la plus grande : 10 808 nœuds (98 %)
#   diamètre ............................. 12
#   table d'arêtes en mémoire ............ 5,5 Mo
#   construction complète ................ ~10,6 s
#   conversion UniProt -> SYMBOL ......... 10 523 / 11 030 = 95,4 %, 0,4 s
#   dont ambiguës (plusieurs symboles) ... 76
#
# Le réseau est donc DENSE en degré et CREUX en densité (n est grand) : les deux
# lectures ne s'opposent pas, mais « 19 107 arêtes » n'est pas reproductible.
#
# ⚠️ Les 507 nœuds non convertis (4,6 %) le sont TOUS parce que leur accession
# est HORS de l'espace de clés UNIPROT d'`org.Hs.eg.db` — mesuré : 507 hors
# espace, 507 non convertis, et **0 dans l'espace mais sans symbole**. La perte
# n'est donc PAS un trou d'annotation SYMBOL, c'est un trou d'ESPACE DE CLÉS
# (accessions Reactome absentes d'`org.Hs.eg.db`). La distinction est portée par
# `n_out_of_space` et non devinée — c'est la seule des deux causes qui soit
# actionnable côté utilisateur.
#
# ⚠️ Identifiants : ce sont des accessions UniProt NUES (« P05231 »), le type est
# porté par la colonne `src_type`/`dest_type` de `graphite` — il n'y a PAS de
# préfixe « UNIPROT: » à retirer (la proposition le croyait).
#
# -----------------------------------------------------------------------------
# ⚠️ SÉMANTIQUE — à ne jamais perdre de vue
# -----------------------------------------------------------------------------
# Ce réseau est DÉRIVÉ DE LA TOPOLOGIE DES VOIES Reactome. Une arête signifie
# « ces deux protéines participent à la même voie/réaction », PAS « interaction
# physique démontrée ». Un relais prédit est un CO-MEMBRE DE VOIE, pas un
# partenaire de liaison. Toute UI consommatrice doit l'écrire noir sur blanc.
#
# -----------------------------------------------------------------------------
# ⚠️ ESPÈCE — `hsapiens` seul, et la raison a changé
# -----------------------------------------------------------------------------
# `org.Mm.eg.db` (annotation de GÈNES) est bien installé — la prémisse de
# `STATUS.md` §2bd.3 était fausse sur ce point. Mais `graphite::pathways()`
# échoue pour `mmusculus` HORS LIGNE : il tente un téléchargement sur
# graphiteweb.bio.unipd.it parce que `mmuReactome.db` (la base de VOIES) n'est
# PAS installé. ⇒ La conclusion « humain seul » tient, pour une raison MESURÉE
# et différente : ce n'est pas l'annotation qui manque, c'est la base de voies,
# et la combler exigerait un accès réseau (interdit par local-first).
#
# -----------------------------------------------------------------------------
# Cache : mémoïsation de SESSION uniquement
# -----------------------------------------------------------------------------
# `CONVENTIONS.md` §10 limite les portées du cache applicatif (trajectory,
# velocity, markers, pathways, déconvolution spatiale) et impose de DEMANDER
# avant d'en ajouter une. « network » n'y figure pas : ce fichier se contente
# donc d'une mémoïsation en mémoire, par (espèce, source). La construction coûte
# ~10 s, une seule fois par session — pas besoin d'un cache disque.
#
# Erreurs classées `bulk_network_error` (français, errorCondition, state).
# =============================================================================

#' Champs contractuels du résultat de réseau PCSF (gelé par le freeze test)
#'
#' @return vecteur character des champs canoniques.
#' @export
bulk_network_contract_fields <- function() {
  c("type", "status", "nodes", "edges", "prizes", "node_role", "species",
    "source_db", "source_version", "id_type", "map_rate", "parameters",
    "qc", "warnings", "provenance", "analysis_id", "timestamp_utc")
}

#' États de validité du domaine réseau
#' @export
bulk_network_validity_states <- function() {
  c("valid", "valid_with_warnings")
}

#' Rôles d'un nœud dans le sous-réseau retenu
#'
#' `terminal` = nœud porteur d'une prime (mesuré) ; `relay` = nœud ajouté par
#' l'algorithme pour connecter les terminaux (PRÉDIT, non mesuré).
#' @export
bulk_network_node_roles <- function() {
  c("terminal", "relay")
}

.bulk_network_stop <- function(state, message) {
  stop(errorCondition(message, class = "bulk_network_error", state = state))
}

#' État d'une erreur classée bulk_network_error
#'
#' @param e condition attrapée.
#' @return character(1) : le champ `state`, ou NA_character_ si non classée.
#' @export
# §14.1 : délégation à l'accesseur générique. Famille F2 MESURÉE — la classe
# n'est PAS vérifiée : l'état est rendu dès qu'il existe.
bulk_network_error_state <- function(e) ts_error_state(e)

#' Surface publique figée (le freeze test refuse toute fonction non listée)
#'
#' Volontairement TRIÉE : le freeze test vérifie `identical(pub, sort(pub))`, ce
#' qui rend tout ajout ou retrait visible dans le diff sans dépendre de l'ordre
#' de déclaration (même convention que `cellchat_engine_public_api()`).
#' @export
bulk_network_public_api <- function() {
  c("assert_bulk_network_object", "assert_bulk_network_result",
    "build_bulk_network_table_export", "bulk_network_contract_fields",
    "bulk_network_error_state", "bulk_network_map_ids",
    "bulk_network_memo_clear", "bulk_network_node_roles",
    "bulk_network_pcsf_params", "bulk_network_pcsf_params_default",
    "bulk_network_public_api", "bulk_network_source_available",
    "bulk_network_species_supported",
    "bulk_network_species_unavailable_reason", "bulk_network_validity_states",
    "load_bulk_network", "plot_bulk_network", "run_bulk_network_pcsf")
}

.bulk_network_config <- function(name, fallback) {
  if (exists(name, inherits = TRUE)) get(name) else fallback
}

# -- Espèce -------------------------------------------------------------------

#' Espèces disposant d'un réseau de voies HORS LIGNE
#'
#' Mesuré le 2026-09-16 : `hsapiens` est lu depuis `reactome.db` local sans
#' aucun accès réseau ; `mmusculus` échoue (téléchargement tenté, base
#' `mmuReactome.db` absente). Voir `bulk_network_species_unavailable_reason()`.
#'
#' @return character : espèces supportées hors ligne.
#' @export
bulk_network_species_supported <- function() {
  "hsapiens"
}

#' Pourquoi une espèce est indisponible hors ligne (ou NA si elle l'est)
#'
#' @param species nom d'espèce (`graphite` : `hsapiens`, `mmusculus`…).
#' @return character(1) : motif en français, ou NA_character_ si supportée.
#' @export
bulk_network_species_unavailable_reason <- function(species) {
  if (species %in% bulk_network_species_supported()) return(NA_character_)
  if (identical(species, "mmusculus")) {
    return(paste0(
      "Souris indisponible HORS LIGNE : la base de VOIES `mmuReactome.db` n'est ",
      "pas installée, et `graphite::pathways()` tente alors un téléchargement ",
      "(graphiteweb.bio.unipd.it). `org.Mm.eg.db` est bien présent, mais c'est ",
      "l'annotation de GÈNES, pas le réseau de voies."
    ))
  }
  sprintf("Espèce '%s' non couverte : le réseau de voies n'est disponible hors ligne que pour %s.",
          species, paste(bulk_network_species_supported(), collapse = ", "))
}

#' Le réseau de voies est-il exploitable hors ligne pour cette espèce ?
#'
#' @param species nom d'espèce.
#' @return logical(1).
#' @export
bulk_network_source_available <- function(species = "hsapiens") {
  if (!is.character(species) || length(species) != 1L || is.na(species)) {
    return(FALSE)
  }
  if (!species %in% bulk_network_species_supported()) return(FALSE)
  requireNamespace("graphite", quietly = TRUE) &&
    requireNamespace("reactome.db", quietly = TRUE)
}

# -- Mémoïsation de session ---------------------------------------------------
# Portée volontairement limitée à la session : ce n'est PAS le cache applicatif
# (règle 8 / CONVENTIONS §10 — une portée « network » n'y est pas autorisée).
.bulk_network_memo <- new.env(parent = emptyenv())

#' Vide la mémoïsation de session (utile aux tests et à un rechargement forcé)
#' @return invisible(TRUE).
#' @export
bulk_network_memo_clear <- function() {
  rm(list = ls(envir = .bulk_network_memo), envir = .bulk_network_memo)
  invisible(TRUE)
}

# -- N-2 : chargement du réseau ----------------------------------------------

#' Charge le réseau protéine-à-protéine dérivé des voies Reactome
#'
#' Le réseau est construit depuis `reactome.db` LOCAL (aucun accès réseau) puis
#' mémoïsé pour la session. Les arêtes sont DÉDUPLIQUÉES sur la paire non
#' orientée : une arête = une paire de protéines co-membres d'au moins une voie.
#'
#' @param species espèce (`hsapiens` seul hors ligne — cf.
#'   `bulk_network_species_supported()`).
#' @param source base de voies (seule `"reactome"` est disponible hors ligne :
#'   `KEGG.db` n'est pas installé).
#' @param refresh si TRUE, ignore la mémoïsation et reconstruit.
#' @return liste : `edges` (data.frame from/to, accessions UniProt),
#'   `nodes`, compteurs (`n_edges`, `n_nodes`, `mean_degree`, `density`,
#'   `n_raw_edges`, `n_dropped_non_protein`), `source_db`, `source_version`,
#'   `id_type`, `n_pathways`, `build_seconds` (**NA** si le réseau a été servi
#'   depuis la mémoïsation : il n'y a alors pas eu de construction), `from_memo`.
#' @export
load_bulk_network <- function(species = "hsapiens", source = "reactome",
                              refresh = FALSE) {
  if (!is.character(species) || length(species) != 1L || is.na(species)) {
    .bulk_network_stop("invalid_input",
      "load_bulk_network : 'species' doit être un nom d'espèce (ex. \"hsapiens\").")
  }
  if (!identical(source, "reactome")) {
    .bulk_network_stop("invalid_input", sprintf(
      paste0("load_bulk_network : source '%s' non supportée. Seule \"reactome\" ",
             "est disponible hors ligne (KEGG.db n'est pas installé)."), source))
  }
  reason <- bulk_network_species_unavailable_reason(species)
  if (!is.na(reason)) {
    .bulk_network_stop("source_unavailable", reason)
  }
  if (!bulk_network_source_available(species)) {
    .bulk_network_stop("missing_dependency", paste0(
      "load_bulk_network : les paquets 'graphite' et 'reactome.db' sont requis ",
      "et doivent être présents dans renv.lock (aucune installation à la volée)."))
  }

  key <- paste(species, source, sep = "::")
  if (!isTRUE(refresh) && exists(key, envir = .bulk_network_memo, inherits = FALSE)) {
    hit <- get(key, envir = .bulk_network_memo, inherits = FALSE)
    hit$from_memo <- TRUE
    # `build_seconds` décrit la CONSTRUCTION du réseau, pas l'appel : servi
    # depuis la mémoïsation, il n'y a pas eu de construction. Le laisser tel
    # quel faisait afficher « 27,2 s » pour un appel instantané (mesuré le
    # 2026-09-16) — un chiffre faux vaut moins qu'un NA honnête.
    hit$build_seconds <- NA_real_
    return(hit)
  }

  t0 <- Sys.time()
  pathways <- graphite::pathways(species, source)
  if (!length(pathways)) {
    .bulk_network_stop("empty_source",
      sprintf("load_bulk_network : aucune voie retournée pour %s/%s.", species, source))
  }

  # Aretes PROTEINE uniquement : les arêtes « mixed » embarquent des métabolites
  # (CHEBI), qui ne sont pas des gènes et n'ont rien à faire dans un réseau de
  # gènes. ⚠️ `which = "protein"` n'est PAS parfaitement pur : mesuré le
  # 2026-09-16, 6 arêtes sur 2 128 244 portent encore un type non-UniProt. Elles
  # sont écartées ET comptées (un changement amont de `graphite` doit se voir).
  #
  # Optimisation MESURÉE : on ne construit que DEUX colonnes par voie. Une
  # variante à quatre colonnes (en gardant `from_type`/`to_type` pour tracer)
  # allouait 2,1 M lignes × 4 chaînes et faisait passer la construction de
  # ~10 s à ~27 s — pour des colonnes qui ne servent qu'au filtrage, jamais au
  # résultat. Les types sont donc consommés dans la boucle.
  pieces <- lapply(pathways, function(pw) {
    e <- graphite::edges(pw, which = "protein")
    # Toujours une LISTE, jamais NULL : une voie sans arête protéine doit
    # contribuer 0 au compteur d'écartées, pas casser le vapply (payé une fois).
    if (!nrow(e)) return(list(df = NULL, dropped = 0))
    keep <- e$src_type == "UNIPROT" & e$dest_type == "UNIPROT"
    list(df = if (any(keep)) {
      data.frame(from = e$src[keep], to = e$dest[keep], stringsAsFactors = FALSE)
    } else NULL,
    dropped = sum(!keep))
  })
  n_dropped_non_protein <- sum(vapply(pieces, function(p) p$dropped, numeric(1)))
  pieces <- lapply(pieces, function(p) p$df)
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  if (!length(pieces)) {
    .bulk_network_stop("empty_source",
      sprintf("load_bulk_network : aucune arête protéine pour %s/%s.", species, source))
  }
  raw <- do.call(rbind, pieces)

  # Déduplication sur la paire NON ORIENTÉE : une arête = « co-membres d'au
  # moins une voie », la direction Reactome n'a pas de sens pour un Steiner.
  a <- pmin(raw$from, raw$to)
  b <- pmax(raw$from, raw$to)
  keep <- !duplicated(paste(a, b, sep = "\r"))
  edges <- data.frame(from = a[keep], to = b[keep], stringsAsFactors = FALSE)
  edges <- edges[order(edges$from, edges$to), , drop = FALSE]
  rownames(edges) <- NULL

  nodes <- sort(unique(c(edges$from, edges$to)))
  n_nodes <- length(nodes)
  n_edges <- nrow(edges)

  res <- list(
    edges              = edges,
    nodes              = nodes,
    n_edges            = n_edges,
    n_nodes            = n_nodes,
    mean_degree        = if (n_nodes > 0L) 2 * n_edges / n_nodes else NA_real_,
    density            = if (n_nodes > 1L) {
      (2 * n_edges) / (n_nodes * (n_nodes - 1))
    } else NA_real_,
    n_pathways         = length(pathways),
    n_raw_edges        = nrow(raw),
    n_dropped_non_protein = as.integer(n_dropped_non_protein),
    source_db          = source,
    source_version     = as.character(utils::packageVersion("reactome.db")),
    id_type            = "UNIPROT",
    build_seconds      = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    from_memo          = FALSE
  )
  assign(key, res, envir = .bulk_network_memo)
  res
}

# -- N-1 : conversion des identifiants ---------------------------------------

# `AnnotationDbi::mapIds()` ne rend pas NA sur une clé inconnue : il ÉCHOUE si
# AUCUNE clé n'appartient à l'espace (mesuré le 2026-09-16 : « None of the keys
# entered are valid keys for 'UNIPROT' »). C'est exactement le cas qu'un
# plancher de correspondance doit rendre LISIBLE et non bruyant : un appelant
# qui fournit des symboles là où on attend des accessions n'obtient qu'un
# message d'AnnotationDbi, sans taux ni cause. On pré-filtre donc sur l'espace
# de clés, et on COMPTE les clés hors espace — c'est une cause de
# non-conversion distincte d'« accession valide sans symbole », et c'est la
# seule des deux qui soit actionnable côté utilisateur.
#
# L'espace est mémoïsé (116 570 clés, 0,14 s mesurés) : le recalculer à chaque
# appel coûterait plus cher que la conversion elle-même.
.bulk_network_key_universe <- function(db, anno) {
  key <- paste0("keyspace::", db, "::UNIPROT")
  if (exists(key, envir = .bulk_network_memo, inherits = FALSE)) {
    return(get(key, envir = .bulk_network_memo, inherits = FALSE))
  }
  u <- AnnotationDbi::keys(anno, keytype = "UNIPROT")
  assign(key, u, envir = .bulk_network_memo)
  u
}

#' Convertit des accessions UniProt en symboles de gènes, avec taux tracé
#'
#' La correspondance est **plusieurs-vers-plusieurs** (un gène a plusieurs
#' protéines, une accession peut porter plusieurs symboles). Le taux de
#' correspondance est **tracé**, jamais tu : un réseau amputé en silence
#' produirait un sous-graphe faux sans que rien ne le signale.
#'
#' @param ids accessions UniProt (character, sans préfixe).
#' @param species espèce (détermine la base d'annotation).
#' @return liste : `symbol` (character nommé par `ids`, NA si non converti),
#'   `n_input`, `n_mapped`, `n_ambiguous`, `n_unmapped`, `n_in_space`,
#'   `n_out_of_space` (clés qui ne sont pas des accessions UniProt — cause
#'   distincte d'un simple échec de correspondance), `map_rate`, `min_map_rate`,
#'   `below_floor`, `unmapped_ids`, `annotation_db`.
#' @export
bulk_network_map_ids <- function(ids, species = "hsapiens") {
  if (!is.character(ids)) {
    .bulk_network_stop("invalid_input", sprintf(
      "bulk_network_map_ids : 'ids' doit être un vecteur character (reçu : %s).",
      class(ids)[1]))
  }
  ids <- unique(ids[!is.na(ids) & nzchar(ids)])
  if (!length(ids)) {
    .bulk_network_stop("invalid_input",
      "bulk_network_map_ids : aucun identifiant exploitable (vecteur vide ou tout NA).")
  }
  reason <- bulk_network_species_unavailable_reason(species)
  if (!is.na(reason)) {
    .bulk_network_stop("source_unavailable", reason)
  }

  db <- switch(species,
    hsapiens = "org.Hs.eg.db",
    mmusculus = "org.Mm.eg.db",
    .bulk_network_stop("invalid_input",
      sprintf("bulk_network_map_ids : aucune base d'annotation connue pour '%s'.", species))
  )
  if (!requireNamespace(db, quietly = TRUE)) {
    .bulk_network_stop("missing_dependency", sprintf(
      "bulk_network_map_ids : le paquet d'annotation '%s' est requis (renv.lock).", db))
  }
  anno <- getExportedValue(db, db)
  universe <- .bulk_network_key_universe(db, anno)
  in_space <- ids %in% universe
  n_out_of_space <- as.integer(sum(!in_space))

  # Appel sur le SEUL sous-ensemble garanti dans l'espace : `mapIds` ne peut
  # donc plus échouer faute de clé valide, et les hors-espace restent NA sans
  # qu'on ait besoin d'attraper une erreur (un tryCatch ici masquerait aussi
  # les vraies erreurs d'AnnotationDbi).
  #
  # multiVals = "list" : on garde TOUTES les correspondances pour pouvoir
  # compter les ambiguïtés, puis on retient la première pour l'usage courant.
  hits <- if (any(in_space)) {
    suppressWarnings(AnnotationDbi::mapIds(
      anno, keys = ids[in_space], column = "SYMBOL", keytype = "UNIPROT",
      multiVals = "list"
    ))
  } else {
    list()
  }

  first_symbol <- vapply(ids, function(id) {
    x <- hits[[id]]
    x <- x[!is.na(x) & nzchar(x)]
    if (length(x)) as.character(x[1]) else NA_character_
  }, character(1))
  names(first_symbol) <- ids
  n_ambiguous <- if (length(hits)) {
    sum(vapply(hits, function(x) {
      length(unique(x[!is.na(x) & nzchar(x)])) > 1L
    }, logical(1)))
  } else {
    0L
  }
  n_mapped <- sum(!is.na(first_symbol))
  n_input <- length(ids)
  map_rate <- if (n_input > 0L) n_mapped / n_input else NA_real_
  floor_rate <- .bulk_network_config("TS_BULK_NETWORK_MIN_MAP_RATE", 0.50)

  list(
    symbol        = first_symbol,
    n_input       = n_input,
    n_mapped      = n_mapped,
    n_ambiguous   = as.integer(n_ambiguous),
    n_unmapped    = as.integer(n_input - n_mapped),
    n_in_space    = as.integer(n_input - n_out_of_space),
    n_out_of_space = n_out_of_space,
    map_rate      = map_rate,
    # Le plancher est DÉCLARÉ (config/thresholds.R) et rendu explicite : un
    # réseau amputé sous ce seuil ne doit pas être interprété comme s'il était
    # complet. C'est à l'appelant d'en faire un avertissement visible.
    min_map_rate  = floor_rate,
    below_floor   = isTRUE(map_rate < floor_rate),
    unmapped_ids  = names(first_symbol)[is.na(first_symbol)],
    annotation_db = db
  )
}

#' Refuse un objet qui n'est pas un réseau canonique (sortie de load_bulk_network)
#'
#' @param network objet à valider.
#' @param context contexte inclus dans le message d'erreur.
#' @return invisible(TRUE), ou stop() classé.
#' @export
assert_bulk_network_object <- function(network, context = "") {
  ctx <- if (nzchar(context)) paste0(" (", context, ")") else ""
  if (is.null(network) || !is.list(network)) {
    .bulk_network_stop("invalid_input",
      paste0("assert_bulk_network_object", ctx, " : objet non NULL/list attendu."))
  }
  if (!is.data.frame(network$edges) ||
      !all(c("from", "to") %in% colnames(network$edges))) {
    .bulk_network_stop("invalid_input", paste0(
      "assert_bulk_network_object", ctx,
      " : 'edges' doit être un data.frame avec les colonnes from/to."))
  }
  if (!is.character(network$nodes) || !length(network$nodes)) {
    .bulk_network_stop("invalid_input", paste0(
      "assert_bulk_network_object", ctx, " : 'nodes' doit être un character non vide."))
  }
  invisible(TRUE)
}

# =============================================================================
# N-3 — moteur PCSF heuristique
# =============================================================================
# ⚠️ HEURISTIQUE, PAS UN OPTIMUM. L'option exacte (PLNE) exigerait ROI/ompr, une
# dépendance nouvelle non justifiée (proposition §6). Le résultat est donc un
# SOUS-GRAPHE CANDIDAT : il collecte les primes atteignables à coût négatif et
# les relie, mais rien ne garantit qu'aucun autre sous-graphe ne fasse mieux.
# L'UI doit le dire ; `qc$algorithm` porte le libellé exact pour que ce soit
# vérifiable et non déclaratif.
#
# Objectif (PCSF classique, signe « à maximiser ») :
#     score = somme(primes retenues) − ω·(nb d'arbres) − β·(nb d'arêtes)
#                                   − μ·(nb de relais)
# où un RELAIS est un nœud retenu qui ne porte PAS de prime (il paie μ, plus
# β par arête). Les primes sont positives par construction (cf. D2).
#
# Algorithme (déterministe, sans tirage — cf. `seed` ci-dessous) :
#   1. primes ← nœuds d'intérêt ∧ nœuds du réseau ; les autres sont COMPTÉS
#      (`n_prizes_outside_network`) — un recouvrement nul est refusé, pas
#      silencieusement vide.
#   2. distances k×k entre primes (`igraph::distances`, niveau C).
#   3. fusion agglomérative par lien simple : à chaque étape on relie la paire
#      de groupes la plus proche si `ω > β·d` (ouvrir un arbre de moins rapporte
#      ω, la liaison coûte β par arête). Tie-break par NOM de nœud, jamais par
#      ordre de hachage.
#   4. les chemins retenus sont matérialisés (`igraph::shortest_paths`) puis
#      unifiés ; un arbre couvrant par composante casse les cycles créés par le
#      recouvrement des chemins (c'est le « + MST » de l'option (a)).
#   5. élagage : on retire itérativement toute FEUILLE sans prime (elle coûte
#      β + μ et ne rapporte rien) jusqu'à point fixe.
#   6. rôles : `terminal` (porte une prime) / `relay` (ajouté pour relier).
#
# `seed` : l'algorithme ne tire AUCUN nombre aléatoire. Le champ est conservé au
# contrat pour rester aligné sur les autres moteurs, mais il vaut NA avec un
# motif explicite — enregistrer une graine qui ne sert à rien ferait croire à une
# reproductibilité qui vient en réalité du déterminisme. Même arbitrage que
# `build_seconds` (un chiffre faux vaut moins qu'un NA honnête).

#' Paramètres par défaut du moteur PCSF (déclarés, jamais implicites)
#'
#' Les défauts sont CALIBRÉS sur l'échelle des primes attendues (`−log10(padj)`,
#' typiquement 1 à 30) et rendus transparents par `max_join_hops` : une fusion
#' n'est retenue que si `ω > β·d + μ·(d−1)`, soit **d ≤ 5 arêtes** avec ces
#' défauts. Des défauts plus petits (ω = 1) rendraient le moteur DÉGÉNÉRÉ : sur
#' un réseau de degré moyen 53, deux nœuds quelconques sont à ~2 arêtes, donc
#' `1 > 1·2` est faux et rien ne se relierait jamais (constaté en exécutant le
#' 2026-09-16 — un PCSF qui ne relie rien n'est pas un PCSF).
#'
#' @return liste : `omega`, `beta`, `mu`, `max_nodes`, `seed` (`max_join_hops`
#'   est ajouté par `bulk_network_pcsf_params()`, qui valide).
#' @export
bulk_network_pcsf_params_default <- function() {
  list(
    omega = 10,   # coût d'ouverture d'un arbre
    beta  = 1,    # coût par arête
    mu    = 1,    # coût par nœud retenu SANS prime (relais)
    max_nodes = .bulk_network_config("TS_BULK_NETWORK_MAX_NODES", 200L),
    seed  = NA_real_
  )
}

# Nombre de sauts au-delà duquel une fusion n'est plus rentable :
#   ω > β·d + μ·(d−1)  ⟺  d < (ω + μ) / (β + μ)
# Rendue explicite pour que « ω = 10 » ne soit pas un chiffre opaque.
.bulk_network_max_join_hops <- function(p) {
  denom <- p$beta + p$mu
  if (!is.finite(denom) || denom <= 0) return(Inf)
  d <- (p$omega + p$mu) / denom
  # d STRICTEMENT inférieur à la borne : le plus grand entier qui satisfait
  # l'inégalité stricte.
  ceiling(d - 1e-9) - 1
}

#' Paramètres du moteur, relus depuis la config (source unique)
#'
#' @param overrides liste nommée de valeurs à forcer (testabilité).
#' @return liste de paramètres validée, avec `max_join_hops` dérivé.
#' @export
bulk_network_pcsf_params <- function(overrides = list()) {
  p <- utils::modifyList(bulk_network_pcsf_params_default(), overrides)
  for (nm in c("omega", "beta", "mu")) {
    v <- p[[nm]]
    if (!is.numeric(v) || length(v) != 1L || is.na(v) || v < 0) {
      .bulk_network_stop("invalid_input", sprintf(
        "bulk_network_pcsf_params : '%s' doit être un nombre >= 0 (reçu : %s).",
        nm, paste(format(v), collapse = ", ")))
    }
  }
  mn <- p$max_nodes
  if (!is.numeric(mn) || length(mn) != 1L || is.na(mn) || mn < 2) {
    .bulk_network_stop("invalid_input", sprintf(
      "bulk_network_pcsf_params : 'max_nodes' doit être >= 2 (reçu : %s).",
      paste(format(mn), collapse = ", ")))
  }
  p$max_join_hops <- .bulk_network_max_join_hops(p)
  p
}

# Arbre couvrant (un par composante) d'un sous-ensemble d'arêtes — casse les
# cycles introduits par le recouvrement des plus courts chemins.
#
# ⚠️ RÉUTILISATION, PAS RÉIMPLÉMENTATION. La première version parcourait le
# graphe « à la main » (file de frontière en R) : c'est exactement la
# duplication de moteur que la règle 3 interdit, alors qu'`igraph` est déjà une
# dépendance du dépôt. `igraph::mst()` fait le travail, y compris sur un graphe
# NON CONNEXE où il rend une FORÊT (mesuré le 2026-09-16 : 3 composantes /
# 7 nœuds -> 5 arêtes, soit n − composantes).
.bulk_network_spanning <- function(edges, nodes) {
  empty <- data.frame(from = character(0), to = character(0),
                      stringsAsFactors = FALSE)
  if (!nrow(edges)) return(empty)
  gs <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
  tr <- igraph::mst(gs)
  te <- igraph::as_edgelist(tr, names = TRUE)
  if (!length(te)) return(empty)
  out <- data.frame(from = te[, 1L], to = te[, 2L], stringsAsFactors = FALSE)
  # `mst()` ne promet AUCUN ordre de sortie : on trie pour que le résultat soit
  # reproductible à l'identique d'une exécution à l'autre (exigence de gel).
  out <- out[order(out$from, out$to), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Calcule un sous-réseau PCSF heuristique à partir de primes par gène
#'
#' **Heuristique, pas un optimum** — voir l'en-tête de section. Le résultat est
#' un sous-graphe CANDIDAT qui collecte les primes atteignables à coût négatif.
#'
#' @param prizes data.frame avec une colonne de gène (`gene` ou `symbol`) et une
#'   colonne de prime (`score` / `prize` / `log2FoldChange` / `padj`), ou vecteur
#'   numérique nommé. Les primes doivent être **positives** (PCSF : une prime
#'   négative n'a pas de sens) ; les non-positives sont écartées ET comptées.
#' @param network réseau canonique (`load_bulk_network()`).
#' @param species espèce déclarée (pas de défaut deviné).
#' @param threshold seuil DÉCLARÉ par l'utilisateur : ne sont des primes que les
#'   gènes dont la prime est **strictement supérieure** au seuil.
#' @param params paramètres du moteur (`bulk_network_pcsf_params()`).
#' @param convert_ids si TRUE, convertit les accessions en symboles pour le
#'   rapport (le graphe, lui, travaille toujours en identifiants du réseau).
#' @return liste à `bulk_network_contract_fields()`.
#' @export
run_bulk_network_pcsf <- function(prizes, network = NULL, species = "hsapiens",
                                  threshold = NULL, params = list(),
                                  convert_ids = TRUE) {
  if (is.null(network)) network <- load_bulk_network(species)
  assert_bulk_network_object(network, context = "run_bulk_network_pcsf")
  reason <- bulk_network_species_unavailable_reason(species)
  if (!is.na(reason)) .bulk_network_stop("source_unavailable", reason)
  if (!is.null(threshold) &&
      (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold))) {
    .bulk_network_stop("invalid_input",
      "run_bulk_network_pcsf : 'threshold' doit être un nombre (le seuil est DÉCLARÉ, jamais implicite).")
  }
  p <- bulk_network_pcsf_params(params)

  # -- 1. Normalisation des primes -------------------------------------------
  if (is.data.frame(prizes)) {
    gcol <- intersect(c("gene", "symbol", "id"), colnames(prizes))
    scol <- intersect(c("score", "prize", "log2FoldChange", "padj"), colnames(prizes))
    if (!length(gcol) || !length(scol)) {
      .bulk_network_stop("invalid_input", sprintf(
        paste0("run_bulk_network_pcsf : la table de primes doit porter une colonne ",
               "de gène (gene/symbol/id) et une colonne de score (score/prize/" ,
               "log2FoldChange/padj). Colonnes reçues : %s."),
        paste(colnames(prizes), collapse = ", ")))
    }
    g <- as.character(prizes[[gcol[1]]])
    s <- suppressWarnings(as.numeric(prizes[[scol[1]]]))
  } else if (is.numeric(prizes) && !is.null(names(prizes))) {
    g <- names(prizes)
    s <- as.numeric(prizes)
  } else {
    .bulk_network_stop("invalid_input",
      "run_bulk_network_pcsf : 'prizes' doit être un data.frame gene+score ou un vecteur numérique nommé.")
  }
  ok <- !is.na(g) & nzchar(g) & !is.na(s)
  g <- g[ok]; s <- s[ok]
  if (!length(g)) {
    .bulk_network_stop("invalid_input",
      "run_bulk_network_pcsf : aucune prime exploitable (gènes ou scores tous manquants).")
  }
  # Une prime doit être POSITIVE (PCSF) ; le seuil, s'il est déclaré, s'y ajoute.
  n_prizes_input <- length(g)
  pos <- s > 0
  n_non_positive <- sum(!pos)
  g <- g[pos]; s <- s[pos]
  n_below_threshold <- 0L
  if (!is.null(threshold)) {
    above <- s > threshold
    n_below_threshold <- sum(!above)
    g <- g[above]; s <- s[above]
  }
  if (!length(g)) {
    .bulk_network_stop("invalid_input", sprintf(
      paste0("run_bulk_network_pcsf : aucune prime ne passe le filtre ",
             "(seuil = %s ; %d non positives, %d sous le seuil). ",
             "Le seuil est DÉCLARÉ : baissez-le ou vérifiez la colonne de score."),
      if (is.null(threshold)) "aucun" else format(threshold),
      n_non_positive, n_below_threshold))
  }

  # Doublons de gène : on garde la prime MAXIMALE (une seule prime par nœud).
  if (anyDuplicated(g)) {
    ord <- order(g, -s)
    g <- g[ord]; s <- s[ord]
    first <- !duplicated(g)
    g <- g[first]; s <- s[first]
  }
  if (length(g) > p$max_nodes) {
    .bulk_network_stop("too_many_prizes", sprintf(
      paste0("run_bulk_network_pcsf : %d primes après filtrage, plafond déclaré ",
             "TS_BULK_NETWORK_MAX_NODES = %d. Réduisez le jeu ou montez le seuil ",
             "(le plafond protège la session, il n'est pas décoratif)."),
      length(g), as.integer(p$max_nodes)))
  }

  # -- 2. Intersection avec le réseau ----------------------------------------
  # Les identifiants d'entrée sont des SYMBOLES côté utilisateur ; le réseau est
  # en accessions UniProt. On convertit donc dans le sens utilisateur -> réseau.
  if (isTRUE(convert_ids)) {
    m <- suppressWarnings(bulk_network_map_ids(network$nodes, species = species))
    sym2acc <- stats::setNames(network$nodes, m$symbol)
    sym2acc <- sym2acc[!is.na(names(sym2acc)) & nzchar(names(sym2acc))]
    # Un symbole ambigu (plusieurs accessions) garde la PREMIÈRE accession
    # triée : choix déterministe, et le compte d'ambiguïtés reste visible.
    sym2acc <- sym2acc[!duplicated(names(sym2acc))]
    acc <- unname(sym2acc[g])
    map_rate <- m$map_rate
  } else {
    acc <- g
    map_rate <- NA_real_
  }
  in_net <- !is.na(acc) & acc %in% network$nodes
  n_outside <- sum(!in_net)
  if (!any(in_net)) {
    .bulk_network_stop("no_overlap", sprintf(
      paste0("run_bulk_network_pcsf : recouvrement NUL — aucune des %d primes ",
             "n'appartient au réseau (%s, %d nœuds). Vérifiez que les gènes sont ",
             "des SYMBOLES humains et que l'espèce déclarée est la bonne."),
      length(g), network$source_db, network$n_nodes))
  }
  acc <- acc[in_net]; sym <- g[in_net]; pri <- s[in_net]
  if (anyDuplicated(acc)) {
    ord <- order(acc, -pri)
    acc <- acc[ord]; sym <- sym[ord]; pri <- pri[ord]
    first <- !duplicated(acc)
    acc <- acc[first]; sym <- sym[first]; pri <- pri[first]
  }
  n_terminals <- length(acc)

  # -- 3. Fusion agglomérative par lien simple -------------------------------
  gph <- igraph::graph_from_data_frame(network$edges, directed = FALSE,
                                       vertices = network$nodes)
  d <- igraph::distances(gph, v = acc, to = acc, weights = NA)  # nb d'arêtes

  # ⚠️ COÛT — la première version construisait un `data.frame()` PAR PAIRE de
  # groupes à chaque itération : mesuré 0,43 s (k=10), 5,82 s (k=50), et
  # plusieurs MINUTES au plafond k=200 (la boucle est en O(k^3) constructions
  # d'objets R). Réécrit en lien simple VECTORISÉ : `M` porte les distances
  # inter-groupes, et une fusion se contente de mettre à `Inf` les entrées
  # devenues INTRA-groupe. Le minimum global de `M` est alors exactement la
  # prochaine fusion candidate — même résultat, sans reconstruire quoi que ce
  # soit.
  #
  # Pourquoi c'est exact : en lien simple, dist(A∪B, C) = min(dist(A,C),
  # dist(B,C)) = min sur les membres. Le minimum global sur les paires
  # INTER-groupes ne change donc pas quand on fusionne ; il suffit d'éliminer
  # les paires qui viennent de devenir internes.
  #
  # ⚠️ PIÈGE PAYÉ — l'appartenance à un groupe doit être lue dans `cluster` et
  # JAMAIS dans une liste de membres indexée par le nœud d'absorption : seule
  # l'entrée du nœud absorbant était mise à jour, si bien qu'un nœud du même
  # groupe gardait une liste singleton. Les paires intra-groupe restaient alors
  # finies, `min(M)` pouvait désigner deux nœuds DÉJÀ dans le même groupe, et
  # la fusion ne faisait plus décroître le nombre de groupes ⇒ boucle infinie
  # (constatée : > 10 min, processus tué). `which(cluster == ci)` est la seule
  # lecture correcte ; la borne `n_terminals - 1` itérations rend en plus la
  # terminaison PROUVABLE, indépendamment de la logique de fusion.
  M <- d
  diag(M) <- Inf
  cluster <- seq_len(n_terminals)
  links <- list()
  for (iter in seq_len(max(0L, n_terminals - 1L))) {
    v <- min(M)
    if (!is.finite(v)) break
    # Rentabilité : ω > β·d + μ·(d−1). Le seuil est précalculé
    # (`max_join_hops`) et le minimum global est croissant, donc dès qu'il le
    # dépasse on peut s'arrêter — aucune paire restante n'est rentable.
    if (v > p$max_join_hops) break
    w <- which(M == v, arr.ind = TRUE)
    # tie-break DÉTERMINISTE : distance, puis NOM de nœud (jamais un ordre de
    # hachage ni un tirage). `M` étant symétrique, chaque paire apparaît deux
    # fois : on la canonicalise en (min, max) avant de trier.
    ka <- acc[w[, 1L]]; kb <- acc[w[, 2L]]
    lo <- ifelse(ka <= kb, ka, kb)
    hi <- ifelse(ka <= kb, kb, ka)
    ord <- order(lo, hi)
    i <- w[ord[1L], 1L]; j <- w[ord[1L], 2L]
    ci <- cluster[i]; cj <- cluster[j]
    if (ci == cj) {
      # ne devrait pas arriver : garde-fou de terminaison
      M[i, j] <- Inf; M[j, i] <- Inf
      next
    }
    links[[length(links) + 1L]] <- c(acc[i], acc[j])
    mi <- which(cluster == ci); mj <- which(cluster == cj)
    M[mi, mj] <- Inf
    M[mj, mi] <- Inf
    cluster[cluster == cj] <- ci
  }

  # -- 4. Matérialisation des chemins + arbre couvrant -----------------------
  if (length(links)) {
    sp <- igraph::shortest_paths(gph, from = vapply(links, `[`, character(1), 1L),
                                 to = vapply(links, `[`, character(1), 2L))
    pe <- do.call(rbind, lapply(sp$vpath, function(vp) {
      n <- igraph::V(gph)$name[as.integer(vp)]
      if (length(n) < 2L) return(NULL)
      data.frame(from = n[-length(n)], to = n[-1L], stringsAsFactors = FALSE)
    }))
    if (is.null(pe)) {
      pe <- data.frame(from = character(0), to = character(0),
                       stringsAsFactors = FALSE)
    }
    # arêtes non orientées, dédupliquées, puis arbre couvrant (casse les cycles
    # introduits par le recouvrement de chemins partageant des arêtes)
    a <- pmin(pe$from, pe$to); b <- pmax(pe$from, pe$to)
    pe <- data.frame(from = a, to = b, stringsAsFactors = FALSE)
    pe <- pe[!duplicated(paste(pe$from, pe$to, sep = "\r")), , drop = FALSE]
    nodes_kept <- sort(unique(c(acc, pe$from, pe$to)))
    tree <- .bulk_network_spanning(pe, nodes_kept)
  } else {
    # aucune liaison : chaque prime est son propre arbre
    tree <- data.frame(from = character(0), to = character(0),
                       stringsAsFactors = FALSE)
  }

  # -- 5. Élagage des feuilles sans prime (jusqu'à point fixe) ---------------
  repeat {
    if (!nrow(tree)) break
    deg <- table(c(tree$from, tree$to))
    leaves <- names(deg)[deg == 1L]
    drop <- setdiff(leaves, acc)          # feuille SANS prime -> inutile
    if (!length(drop)) break
    tree <- tree[!(tree$from %in% drop) & !(tree$to %in% drop), , drop = FALSE]
  }
  # nœuds isolés (prime seule, aucun chemin retenu) : conservés, ce sont des
  # terminaux — un terminal isolé reste un résultat légitime.
  nodes_final <- sort(unique(c(acc, tree$from, tree$to)))
  # Nombre de composantes du résultat FINAL, et non nombre de groupes
  # agglomérés : l'élagage peut scinder un arbre, et un terminal isolé est sa
  # propre composante. On mesure le résultat, pas l'intention — le score en
  # dépend directement.
  n_trees <- if (nrow(tree)) {
    gt <- igraph::graph_from_data_frame(tree, directed = FALSE,
                                        vertices = nodes_final)
    igraph::components(gt)$no
  } else {
    length(nodes_final)
  }
  role <- stats::setNames(ifelse(nodes_final %in% acc, "terminal", "relay"),
                          nodes_final)
  if (!identical(sort(unique(role)), sort(bulk_network_node_roles()))) {
    # garde-fou : un rôle hors contrat ferait mentir le freeze test
    role[role != "terminal"] <- "relay"
  }

  n_relay <- sum(role == "relay")
  total_prize <- sum(pri[acc %in% nodes_final])
  score <- total_prize - p$omega * n_trees - p$beta * nrow(tree) - p$mu * n_relay

  # symboles des nœuds retenus (l'UI lit des SYMBOLES, le graphe des accessions)
  acc2sym <- stats::setNames(sym, acc)
  if (isTRUE(convert_ids)) {
    sym_of <- acc2sym[nodes_final]
    # les relais ne sont pas dans les primes : on les résout via la conversion
    need <- is.na(sym_of)
    if (any(need)) {
      sym_of[need] <- unname(m$symbol[nodes_final[need]])
    }
  } else {
    sym_of <- stats::setNames(nodes_final, nodes_final)
  }

  warnings_out <- character(0)
  if (n_outside > 0L) {
    warnings_out <- c(warnings_out, sprintf(
      "%d prime(s) hors réseau (sur %d) : elles ne peuvent pas être reliées et sont exclues du sous-graphe.",
      n_outside, length(g)))
  }
  # Le plancher vient de la config (source unique), il n'est pas recopié ici.
  floor_rate <- .bulk_network_config("TS_BULK_NETWORK_MIN_MAP_RATE", 0.50)
  if (!is.na(map_rate) && isTRUE(map_rate < floor_rate)) {
    warnings_out <- c(warnings_out, sprintf(
      "Taux de correspondance UniProt -> SYMBOL faible (%.1f %%, plancher déclaré %.1f %%) : le réseau a pu être amputé.",
      100 * map_rate, 100 * floor_rate))
  }
  if (n_trees > 1L) {
    warnings_out <- c(warnings_out, sprintf(
      "Le sous-graphe retenu compte %d composantes : le réseau Reactome n'est pas connexe, certaines primes ne sont pas reliables.", n_trees))
  }

  result <- list(
    type         = "bulk_network_pcsf",
    status       = if (length(warnings_out)) "valid_with_warnings" else "valid",
    nodes        = data.frame(
      node   = nodes_final,
      symbol = unname(sym_of),
      prize  = unname(stats::setNames(pri, acc)[nodes_final]),
      role   = unname(role),
      stringsAsFactors = FALSE),
    edges        = tree,
    prizes       = stats::setNames(pri, acc),
    node_role    = role,
    species      = species,
    source_db    = network$source_db,
    source_version = network$source_version,
    id_type      = network$id_type,
    map_rate     = map_rate,
    parameters   = p,
    qc           = list(
      algorithm             = "shortest-path + spanning-tree + pruning (HEURISTIQUE, pas un optimum)",
      threshold             = threshold,
      n_prizes_input        = n_prizes_input,
      n_prizes_non_positive = as.integer(n_non_positive),
      n_prizes_below_threshold = as.integer(n_below_threshold),
      n_terminals           = n_terminals,
      n_prizes_outside_network = as.integer(n_outside),
      n_trees               = as.integer(n_trees),
      n_relay               = as.integer(n_relay),
      n_nodes               = length(nodes_final),
      n_edges               = nrow(tree),
      total_prize           = total_prize,
      score                 = score,
      # Rend « ω = 10 » interprétable : au-delà de ce nombre d'arêtes, une
      # fusion coûte plus qu'elle ne rapporte.
      max_join_hops         = p$max_join_hops,
      network_nodes         = network$n_nodes,
      network_edges         = network$n_edges,
      seed_used             = FALSE),
    warnings     = warnings_out,
    provenance   = list(
      network_built = !isTRUE(network$from_memo),
      memo_hit      = isTRUE(network$from_memo),
      igraph_version = as.character(utils::packageVersion("igraph")),
      rng_kind      = paste(RNGkind(), collapse = "/"),
      seed          = p$seed,
      seed_reason   = if (is.null(p$seed) || is.na(p$seed)) {
        "aucun tirage aléatoire : l'heuristique est déterministe (tie-break par nom de nœud)"
      } else {
        "graine fournie par l'appelant ; sans effet, l'algorithme ne tire pas de nombre"
      }),
    analysis_id  = sprintf("pcsf-%s", format(as.numeric(Sys.time()), digits = 15)),
    timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
  if (!identical(names(result), bulk_network_contract_fields())) {
    .bulk_network_stop("contract_violation", paste0(
      "run_bulk_network_pcsf : le résultat ne respecte pas bulk_network_contract_fields(). ",
      "Attendu : ", paste(bulk_network_contract_fields(), collapse = ", "),
      " ; obtenu : ", paste(names(result), collapse = ", "), "."))
  }
  result
}

#' Refuse un résultat PCSF non canonique (sortie de run_bulk_network_pcsf)
#'
#' @param result objet à valider.
#' @param context contexte inclus dans le message d'erreur.
#' @return invisible(TRUE), ou stop() classé.
#' @export
assert_bulk_network_result <- function(result, context = "") {
  ctx <- if (nzchar(context)) paste0(" (", context, ")") else ""
  if (is.null(result) || !is.list(result)) {
    .bulk_network_stop("invalid_input",
      paste0("assert_bulk_network_result", ctx, " : objet non NULL/list attendu."))
  }
  miss <- setdiff(bulk_network_contract_fields(), names(result))
  if (length(miss)) {
    .bulk_network_stop("contract_violation", paste0(
      "assert_bulk_network_result", ctx, " : champ(s) contractuel(s) manquant(s) : ",
      paste(miss, collapse = ", "), "."))
  }
  if (!identical(result$type, "bulk_network_pcsf")) {
    .bulk_network_stop("invalid_input", paste0(
      "assert_bulk_network_result", ctx, " : 'type' doit valoir \"bulk_network_pcsf\"."))
  }
  if (!result$status %in% bulk_network_validity_states()) {
    .bulk_network_stop("invalid_input", paste0(
      "assert_bulk_network_result", ctx, " : 'status' hors contrat (",
      paste(result$status, collapse = ", "), ")."))
  }
  if (!is.data.frame(result$nodes) ||
      !all(c("node", "symbol", "prize", "role") %in% colnames(result$nodes))) {
    .bulk_network_stop("invalid_input", paste0(
      "assert_bulk_network_result", ctx,
      " : 'nodes' doit être un data.frame avec les colonnes node/symbol/prize/role."))
  }
  bad <- setdiff(unique(result$nodes$role), bulk_network_node_roles())
  if (length(bad)) {
    .bulk_network_stop("invalid_input", paste0(
      "assert_bulk_network_result", ctx, " : rôle(s) hors contrat : ",
      paste(bad, collapse = ", "), "."))
  }
  if (!is.data.frame(result$edges) ||
      !all(c("from", "to") %in% colnames(result$edges))) {
    .bulk_network_stop("invalid_input", paste0(
      "assert_bulk_network_result", ctx,
      " : 'edges' doit être un data.frame avec les colonnes from/to."))
  }
  invisible(TRUE)
}

# -- N-4 : restitution (tracé + table exportable) -----------------------------

#' Affiche le sous-réseau PCSF (terminaux vs relais)
#'
#' **Le tracé porte la mise en garde sémantique** : un relais est un
#' CO-MEMBRE DE VOIE, pas un partenaire d'interaction physique. L'appelant
#' fournit `tr` pour que le titre et la légende suivent la langue active.
#'
#' @param result sortie de `run_bulk_network_pcsf()`.
#' @param tr fonction de traduction (défaut : identité).
#' @param max_label_nodes au-delà, les noms de nœuds ne sont plus affichés
#'   (un sous-graphe de 300 nœuds étiquetés est illisible).
#' @param layout_seed graine du placement (le placement est stochastique : on
#'   la fixe ET on restaure l'état du générateur, cf. leçon `RNGkind`).
#' @return invisible(NULL), après tracé.
#' @export
plot_bulk_network <- function(result, tr = function(k) k, max_label_nodes = 60L,
                              layout_seed = 15) {
  assert_bulk_network_result(result, context = "plot_bulk_network")
  n <- nrow(result$nodes)
  if (!n) {
    graphics::plot.new()
    graphics::title(main = tr("Réseau vide : aucune prime exploitable."))
    return(invisible(NULL))
  }
  g <- igraph::graph_from_data_frame(
    result$edges, directed = FALSE,
    vertices = data.frame(name = result$nodes$node,
                          prize = result$nodes$prize,
                          role = result$nodes$role,
                          stringsAsFactors = FALSE))
  # Placement stochastique : graine fixée PUIS état du générateur restauré —
  # sinon un tracé décalerait silencieusement les tirages de l'analyse suivante.
  # withr::with_seed remplace la sauvegarde manuelle de .Random.seed (même
  # garantie, y compris quand .Random.seed n'existe pas encore — parité SC).
  lay <- withr::with_seed(layout_seed, igraph::layout_with_fr(g))

  is_term <- igraph::V(g)$role == "terminal"
  pri <- igraph::V(g)$prize
  pri[is.na(pri)] <- 0
  # Les terminaux portent la prime : taille proportionnelle (bornée), les
  # relais restent petits et neutres — la distinction doit sauter aux yeux.
  cex_v <- ifelse(is_term,
                  1.2 + 2.0 * (pri - min(pri)) / max(1e-9, max(pri) - min(pri)),
                  0.8)
  col_v <- ifelse(is_term, "#E74C3C", "#95A5A6")
  graphics::par(mar = c(1, 1, 3, 1))
  igraph::plot.igraph(
    g, layout = lay,
    vertex.size = cex_v * 3, vertex.color = col_v,
    vertex.frame.color = "#2C3E50",
    vertex.label = if (n <= max_label_nodes) igraph::V(g)$name else NA,
    vertex.label.cex = 0.7, vertex.label.color = "#2C3E50",
    edge.color = grDevices::adjustcolor("#7F8C8D", alpha.f = 0.6),
    edge.width = 1.4, edge.arrow.mode = 0)
  graphics::title(main = tr("Sous-réseau PCSF (heuristique)"))
  graphics::mtext(
    side = 3, line = 0.2, cex = 0.75, col = "#7F8C8D",
    text = tr("Rouge = prime (mesurée) · Gris = relais (PRÉDIT, co-membre de voie — pas une interaction physique)"))
  invisible(NULL)
}

#' Table exportable des nœuds du sous-réseau (rôle + prime)
#'
#' @param result sortie de `run_bulk_network_pcsf()`.
#' @return data.frame prêt à l'export CSV.
#' @export
build_bulk_network_table_export <- function(result) {
  assert_bulk_network_result(result, context = "build_bulk_network_table_export")
  nd <- result$nodes
  # degré DANS le sous-graphe retenu (pas dans le réseau complet) : c'est la
  # seule mesure qui décrive la place du nœud dans le résultat affiché.
  # 🇱🇷 2026-10-03 : `nb = nrow(nd)` — sans lui, `tabulate()` dimensionne son
  # vecteur au plus grand index PRÉSENT, et un dernier nœud sans arête
  # produisait un `degree` plus court que `nd` : data.frame() levait
  # "differing number of rows" et le téléchargement humain comme la route
  # drive échouaient (mesuré par le fixture N4 isolé de
  # test-mod-bulk-network-export.R). Le zéro est la valeur honnête.
  deg <- if (nrow(result$edges)) {
    tabulate(match(c(result$edges$from, result$edges$to), nd$node), nb = nrow(nd))
  } else {
    rep(0L, nrow(nd))
  }
  data.frame(
    node     = nd$node,
    symbol   = nd$symbol,
    role     = nd$role,
    prize    = nd$prize,
    degree   = deg,
    species  = result$species,
    source_db = result$source_db,
    stringsAsFactors = FALSE)
}

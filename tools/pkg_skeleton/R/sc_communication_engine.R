# =============================================================================
# R/sc/sc_communication_engine.R — Moteur CellChat natif (Path B)
# =============================================================================
# But : executer CellChat DANS l'application et reduire IMMEDIATEMENT son
# resultat aux 12 champs canoniques du contrat Stage 11. Ce fichier est la
# voie « Path B » de docs/proposals/V1X_CELLCHAT_ENGINE_PROPOSAL.md ; la voie
# « Path A » (import CSV/TSV externe, R/sc/sc_communication.R) reste ENTIERE et
# INCHANGEE — B est un AJOUT, jamais un remplacement.
#
# ── REGLE DES DEUX VOIES ────────────────────────────────────────────────────
# Les 12 champs canoniques sont la CIBLE COMMUNE. Le moteur doit produire
# exactement ce que l'import produit ; toute divergence entre les deux voies
# est un BUG, pas une variante. C'est pourquoi ce fichier ne fabrique aucun
# champ de resultat lui-meme : il delegue la finalisation a
# finalize_communication_result() (R/sc/sc_communication.R).
#
# ── INVARIANT DE GEL : MOTEUR EPHEMERE ──────────────────────────────────────
#     etat reactif = resultat canonique  !=  moteur externe
# L'objet `cellchat` embarque la base LR et des tableaux 3-D ; il n'est JAMAIS
# stocke (ni dans un reactiveValues, ni dans le resultat). Il vit le temps de
# run_cellchat() puis est abandonne au ramasse-miettes. Le test de gel verifie
# cet invariant statiquement (aucune affectation d'un objet de classe CellChat
# hors de ce fichier) et dynamiquement (classe de l'objet stocke).
#
# ── DEPENDANCE PARESSeUSE ───────────────────────────────────────────────────
# `CellChat` n'est JAMAIS exige au demarrage : requireNamespace() + erreur
# classee avec guidage d'installation (precedent STAT-S1 « l'app doit demarrer
# sans »). Source : GitHub, epingle par SHA — ni CRAN, ni Bioconductor.
#
# ── CE QUE CE FICHIER NE FAIT PAS ───────────────────────────────────────────
#   - aucune modification de build_cellchat_input() (contrat 4D-3 inchange) ;
#   - aucun recalcul, aucune imputation, aucun melange de sources ;
#   - aucune comparaison INTER-CONDITION (porte DA — decision Phase 9) ;
#   - aucun appel a updateCellChatDB() : la base ne doit jamais changer
#     silencieusement entre deux runs ;
#   - aucun parallelisme imbrique : future est force a "sequential" a
#     L'INTERIEUR du job, mirai reste le seul orchestrateur (regle du depot).
#
# Pur domaine : aucune reactivite Shiny. Source dans app.R APRES
# R/sc/sc_communication_input.R. Constantes TS_CELLCHAT_* dans config/defaults.R.
# Contrat : docs/contracts/CELLCHAT_ENGINE_CONTRACT.md
# =============================================================================

.CELLCHAT_ENGINE_STATES <- c(
  "valid",
  "missing_dependency",   # paquet CellChat absent
  "invalid_input",        # entree non conforme au contrat 4D-3
  "invalid_parameters",   # graine absente, nboot invalide, < 2 populations
  "engine_failure",       # CellChat a echoue (erreur remontee telle quelle)
  "no_interactions"       # aucune interaction non nulle : rien a produire
)

cellchat_engine_states <- function() .CELLCHAT_ENGINE_STATES

#' Etat porte par une erreur du moteur (lecture defensive)
# §14.1 : délégation à l'accesseur générique. Famille F3 MESURÉE — c'est ce
# corps qui a servi de modèle à `ts_error_state()` ; la classe n'est PAS
# vérifiée au-delà de `"condition"`.
cellchat_engine_error_state <- function(e) ts_error_state(e)

.cellchat_engine_stop <- function(state, message) {
  stop(errorCondition(
    sprintf("Moteur CellChat — %s", message),
    class = c("cellchat_engine_error", "error", "condition"),
    state = state
  ))
}

#' Le moteur est-il utilisable dans cette session ?
#'
#' Test PARESSeUX : ne charge jamais CellChat, ne l'exige jamais. Sert a
#' l'UI pour griser l'action et aux tests pour skiper proprement.
cellchat_engine_available <- function() {
  requireNamespace("CellChat", quietly = TRUE)
}

#' Exiger CellChat, avec un guidage d'installation explicite
#'
#' Erreur classee `cellchat_engine_error` / `missing_dependency` : l'absence
#' du paquet est un etat PREVU (dependance GitHub non obligatoire au
#' demarrage), jamais un crash opaque.
.cellchat_engine_require <- function() {
  if (requireNamespace("CellChat", quietly = TRUE)) return(invisible(TRUE))
  .cellchat_engine_stop(
    "missing_dependency",
    paste0(
      "le package 'CellChat' n'est pas installe. Il est distribue uniquement ",
      "sur GitHub (ni CRAN, ni Bioconductor) : ",
      "remotes::install_github(\"jinworks/CellChat\"). ",
      "L'import CSV/TSV externe (Path A) reste disponible sans cette ",
      "dependance."
    )
  )
}

#' Empreinte du moteur : version + SHA du remote GitHub
#'
#' Un remote GitHub n'est reproductible QUE par son SHA (la version du paquet
#' ne suffit pas : elle ne change pas entre deux commits). `engine_sha` est
#' l'un des deux seuls champs d'identite d'analyse reellement nouveaux par
#' rapport a new_provenance_entry() (audit n°3, §10.2).
.cellchat_engine_identity <- function() {
  desc <- tryCatch(utils::packageDescription("CellChat"), error = function(e) NULL)
  if (is.null(desc)) {
    return(list(engine = "CellChat", engine_version = NA_character_,
                engine_sha = NA_character_))
  }
  # packageDescription() lit de PREFERENCE Meta/package.rds (cache ecrit a
  # l'installation), qui ne porte PAS les champs Remote*/Github*. Verifie sur le
  # paquet installe : read.dcf voit 29 champs dont RemoteSha,
  # packageDescription() n'en voit aucun. Comme un remote GitHub n'est
  # reproductible QUE par son SHA, on le relit dans le DESCRIPTION lui-meme.
  dcf <- tryCatch(
    read.dcf(file.path(find.package("CellChat"), "DESCRIPTION"))[1L, ],
    error = function(e) NULL
  )
  get_field <- function(name) {
    v <- desc[[name]]
    if (!is.null(v) && length(v) == 1L && !is.na(v) && nzchar(as.character(v)))
      return(as.character(v))
    if (!is.null(dcf) && name %in% names(dcf)) {
      v2 <- dcf[[name]]
      if (length(v2) == 1L && !is.na(v2) && nzchar(as.character(v2)))
        return(as.character(v2))
    }
    NA_character_
  }
  sha <- get_field("RemoteSha")
  if (is.na(sha)) sha <- get_field("GithubSHA1")
  list(
    engine         = "CellChat",
    engine_version = get_field("Version"),
    engine_sha     = sha
  )
}

#' Resoudre la base LR declaree par l'espece, SANS jamais la mettre a jour
#'
#' `updateCellChatDB()` n'est JAMAIS appele : il changerait la base
#' silencieusement entre deux runs et rendrait deux resultats non comparables
#' sans que rien ne l'indique.
.cellchat_engine_db <- function(species) {
  db_name <- tryCatch(cellchat_database_for_species(species),
                      error = function(e) NULL)
  if (is.null(db_name) || length(db_name) != 1L || is.na(db_name)) {
    .cellchat_engine_stop(
      "invalid_input",
      sprintf("espece '%s' non resolvable en base CellChat (attendu : human|mouse).",
              paste(as.character(species), collapse = ","))
    )
  }
  # Un jeu de donnees paresseux (LazyData) n'est PAS une liaison du namespace :
  # get(db_name, envir = asNamespace("CellChat")) echoue des qu'on s'est contente
  # de requireNamespace() — verifie sur le paquet installe (et reverifye sur un
  # paquet sain : le comportement est identique, ce n'est donc pas un symptome
  # d'installation incomplete). utils::data() est la voie DOCUMENTEE et elle ne
  # demande pas l'attachement du paquet.
  db <- tryCatch(suppressWarnings({
    env <- new.env(parent = emptyenv())
    utils::data(list = db_name, package = "CellChat", envir = env)
    get(db_name, envir = env, inherits = FALSE)
  }), error = function(e) NULL)
  if (is.null(db)) {
    .cellchat_engine_stop(
      "invalid_input",
      sprintf("base '%s' absente du package CellChat installe.", db_name)
    )
  }
  # CellChatDB n'expose PAS de champ `version` (verifie : db$version est NULL).
  # La version est portee par chaque ligne de `interaction`, et la base livree
  # est MIXTE (v1 + v2). On ne choisit pas, on n'invente rien : on rapporte les
  # valeurs distinctes reellement presentes.
  ver <- tryCatch(db[["interaction"]][["version"]], error = function(e) NULL)
  version <- if (is.null(ver) || !length(ver)) NA_character_ else
    paste(sort(unique(as.character(ver))), collapse = "; ")
  list(
    db           = db,
    name         = as.character(db_name),
    version      = version
  )
}

#' Extraire la table canonique des 12 champs depuis l'objet CellChat
#'
#' Structure REELLE de CellChat (verifiee sur le code amont,
#' R/modeling.R `computeCommunProb`) :
#'   net$prob  : array 3-D [source, target, interaction_name]
#'   net$pval  : meme forme (p-values de permutation)
#'   netP$prob : array 3-D [source, target, pathway_name]
#' Les colonnes `ligand` / `receptor` / `pathway` ne sont PAS devinees par
#' decoupage de chaine : elles sont lues dans object@LR$LRsig, qui porte
#' exactement ces colonnes indexees par interaction_name.
#' Accepte l'objet S4 CellChat OU une liste nommee repliquant les slots `net`
#' et `LR` (route de test, et objet relu sans le paquet).
#' NB : cette fonction est la SEULE a connaitre la structure interne de
#' CellChat. Un upgrade du paquet ne casse donc qu'ici — et l'import d'un
#' objet .rds (parse_cellchat_object()) lui DELEGUE l'extraction, plutot que
#' de relire net$prob une deuxieme fois (regle 3 : etendre, ne pas dupliquer).
# Acces tolerant aux "slots" : objet S4 CellChat OU liste nommee qui les
# replique (route de test, et objet relu sans le paquet). Un SEUL accesseur,
# pour que l'extraction de net$prob reste ecrite UNE fois et soit partagee
# par le moteur ET par l'import d'objet (regle 3 : etendre, ne pas dupliquer).
.cellchat_engine_slot <- function(object, name) {
  if (isS4(object)) tryCatch(methods::slot(object, name), error = function(e) NULL)
  else if (is.list(object)) object[[name]]
  else NULL
}

.cellchat_engine_extract <- function(object, source_file = NA_character_) {
  net <- .cellchat_engine_slot(object, "net")
  if (is.null(net) || is.null(net$prob)) {
    .cellchat_engine_stop(
      "engine_failure",
      "l'objet CellChat ne porte aucun resultat (slot net$prob absent)."
    )
  }
  prob <- net$prob
  if (length(dim(prob)) != 3L || is.null(dimnames(prob)) ||
      any(vapply(dimnames(prob), is.null, logical(1)))) {
    .cellchat_engine_stop(
      "engine_failure",
      sprintf(paste0("net$prob inattendu : array 3-D avec dimnames attendu ",
                     "(source x target x interaction) — forme recue : %s."),
              if (is.null(dim(prob))) paste(class(prob), collapse = "/")
              else paste(dim(prob), collapse = " x "))
    )
  }

  pval <- net$pval
  pval_ok <- !is.null(pval) && identical(dim(pval), dim(prob))
  pval_warning <- if (!is.null(pval) && !pval_ok) {
    paste0("net$pval present mais de forme differente de net$prob : ",
           "p_value laissee a NA (jamais fabriquee).")
  } else character(0)

  lrsig <- tryCatch(.cellchat_engine_slot(object, "LR")[["LRsig"]],
                    error = function(e) NULL)
  if (is.null(lrsig) || !is.data.frame(lrsig) || !nrow(lrsig)) {
    .cellchat_engine_stop(
      "engine_failure",
      "object@LR$LRsig absent : impossible de resoudre ligand/receptor/pathway."
    )
  }

  dn <- dimnames(prob)
  idx <- which(!is.na(prob) & prob != 0, arr.ind = TRUE)
  if (!nrow(idx)) {
    .cellchat_engine_stop(
      "no_interactions",
      paste0("aucune interaction de probabilite non nulle : CellChat n'a rien ",
             "infere sur ce jeu (populations trop petites, ou base LR non ",
             "couvrante). Aucun resultat canonique n'est produit.")
    )
  }

  pairs <- as.character(dn[[3L]][idx[, 3L]])
  senders <- as.character(dn[[1L]][idx[, 1L]])
  receivers <- as.character(dn[[2L]][idx[, 2L]])

  # Resolution ligand/receptor/pathway par appariement EXACT sur le nom
  # d'interaction : aucune supposition, aucune reconstruction.
  ridx <- match(pairs, rownames(lrsig))
  ligand <- ifelse(is.na(ridx), NA_character_,
                   as.character(lrsig$ligand[ridx]))
  receptor <- ifelse(is.na(ridx), NA_character_,
                     as.character(lrsig$receptor[ridx]))
  pathway <- if (is.null(lrsig$pathway_name)) rep(NA_character_, length(pairs)) else
    ifelse(is.na(ridx), NA_character_, as.character(lrsig$pathway_name[ridx]))

  score <- as.numeric(prob[idx])
  p_value <- if (pval_ok) {
    v <- as.numeric(pval[idx])
    ifelse(is.finite(v), v, NA_real_)
  } else rep(NA_real_, nrow(idx))

  table <- data.frame(
    sender   = senders,
    receiver = receivers,
    ligand   = ligand,
    receptor = receptor,
    interaction = paste(ligand, receptor, sep = " -> "),
    pathway  = pathway,
    score    = score,
    p_value  = p_value,
    p_adjusted = NA_real_,
    source_method = "cellchat",
    source_file = as.character(source_file)[1L],
    source_cell_identity_level = NA_character_,
    stringsAsFactors = FALSE
  )

  # p_adjusted n'est PAS calcule ici : aucun export standard de CellChat ne
  # produit de p-value ajustee au niveau LR, et la fabriquer serait inventer
  # une donnee (regle 1). Le champ reste NA, comme sur la voie import.
  warnings <- c(
    pval_warning,
    if (any(is.na(ridx))) sprintf(
      "%d interaction(s) sans correspondance dans @LR$LRsig : ligand/receptor/pathway laisses a NA (jamais devines).",
      sum(is.na(ridx))
    ) else character(0)
  )

  list(
    table = table,
    column_mapping = list(
      sender   = "dimnames(net$prob)[[1]] (groupe source)",
      receiver = "dimnames(net$prob)[[2]] (groupe cible)",
      ligand   = "object@LR$LRsig$ligand (apparie par interaction_name)",
      receptor = "object@LR$LRsig$receptor (apparie par interaction_name)",
      pathway  = "object@LR$LRsig$pathway_name (apparie par interaction_name)",
      score    = "net$prob (valeurs non nulles)",
      p_value  = if (pval_ok) "net$pval (meme forme que net$prob)" else NULL
    ),
    n_input_rows = as.integer(length(dn[[3L]])),
    warnings = warnings,
    n_pathways = length(unique(table$pathway[!is.na(table$pathway)]))
  )
}

#' Executer CellChat dans l'application (Path B)
#'
#'Consomme un objet `cellchat_input` DEJA construit (contrat 4D-3,
#' `build_cellchat_input()`) — il n'est jamais reconstruit ici — et produit un
#' resultat de communication STRICTEMENT identique en forme a celui de la voie
#' import (12 champs canoniques).
#'
#' @param cellchat_input Objet `cellchat_input` valide (assert_cellchat_input).
#' @param seed Graine : OBLIGATOIRE, jamais implicite. CellChat permute
#'   (`nboot`) pour les p-values ; la graine est un PARAMETRE DU CALCUL, pas un
#'   effet de bord de session (jamais de set.seed() global).
#' @param nboot Nombre de permutations (parametre REEL de
#'   `computeCommunProb()` ; `nPerm` n'existe pas).
#' @param seurat_obj Objet Seurat courant, pour l'empreinte objet v2 et la
#'   detection de peremption. NULL tolere (empreinte NA).
#' @param on_progress Callback optionnel function(message) — ignore si NULL.
#' @return Le resultat canonique produit par finalize_communication_result(),
#'   avec `provenance$import_only = FALSE` et les champs d'identite moteur.
#' @export
run_cellchat <- function(cellchat_input, seed, nboot = TS_CELLCHAT_NBOOT_DEFAULT,
                         seurat_obj = NULL, on_progress = NULL) {
  .cellchat_engine_require()

  .progress <- function(msg) {
    if (is.function(on_progress)) tryCatch(on_progress(msg), error = function(e) { ts_log_swallow("sc_communication_engine.progress", e); NULL })
  }

  if (!exists("assert_cellchat_input", mode = "function")) {
    .cellchat_engine_stop(
      "invalid_input",
      paste0("R/sc/sc_communication_input.R doit etre source avant ce ",
             "fichier (assert_cellchat_input reutilise, jamais duplique).")
    )
  }
  assert_cellchat_input(cellchat_input, context = "moteur CellChat")

  if (is.null(seed) || length(seed) != 1L || is.na(seed)) {
    .cellchat_engine_stop(
      "invalid_parameters",
      paste0("la graine est OBLIGATOIRE et doit etre declaree : les p-values ",
             "de CellChat viennent d'une permutation, un resultat sans graine ",
             "tracée n'est pas reproductible.")
    )
  }
  seed <- as.integer(seed)
  if (!is.finite(seed)) {
    .cellchat_engine_stop("invalid_parameters",
                          sprintf("graine non entiere (recu : %s).", deparse(seed)))
  }
  nboot <- as.integer(nboot)
  if (length(nboot) != 1L || is.na(nboot) || nboot < 1L) {
    .cellchat_engine_stop(
      "invalid_parameters",
      sprintf("nbout de permutations invalide (recu : %s) — entier >= 1 attendu.",
              paste(format(nboot), collapse = ","))
    )
  }

  groups <- unique(as.character(cellchat_input$meta[["labels"]]))
  groups <- groups[!is.na(groups)]
  if (length(groups) < TS_CELLCHAT_MIN_GROUPS) {
    .cellchat_engine_stop(
      "invalid_parameters",
      sprintf(paste0("%d population(s) annotée(s) : CellChat exige au moins ",
                     "%d groupes pour inferer une communication."),
              length(groups), TS_CELLCHAT_MIN_GROUPS)
    )
  }

  db <- .cellchat_engine_db(cellchat_input$species)
  eng <- .cellchat_engine_identity()

  # ── Parallellisme : jamais de plan future imbrique dans un daemon mirai ──
  # computeCommunProb() peut demander un plan future ; un plan non sequentiel
  # lance DANS un worker mirai creerait des workers imbriques
  # (sursouscription). On force donc "sequential" pour la duree du calcul et
  # on restaure le plan precedent a la sortie.
  prev_plan <- NULL
  has_future <- requireNamespace("future", quietly = TRUE)
  if (has_future) {
    prev_plan <- tryCatch(future::plan(future::sequential),
                          error = function(e) { ts_log_swallow("sc_communication_engine.cellchat_run", e); NULL })
    on.exit(tryCatch(future::plan(prev_plan), error = function(e) { ts_log_swallow("sc_communication_engine.cellchat_run", e); NULL }), add = TRUE)
  }

  .progress("creation de l'objet CellChat")
  # `group.by` doit etre une COLONNE de `meta`. build_cellchat_input() NORMALISE
  # les identites en `meta$labels` alors que `$group_by` garde le nom d'origine
  # (ex. "celltype") pour la provenance : passer `$group_by` ici faisait
  # echouer TOUT le run avec « The 'group.by' is not a column name in the
  # `meta` » — le moteur ne marchait que si la colonne s'appelait "labels".
  # Calcule AVANT le tryCatch pour qu'une entree non conforme garde son etat.
  group_col <- cellchat_group_by_column(cellchat_input)
  object <- tryCatch(
    CellChat::createCellChat(
      object  = cellchat_input$data,
      meta    = cellchat_input$meta,
      group.by = group_col
    ),
    error = function(e) .cellchat_engine_stop(
      "engine_failure",
      sprintf("createCellChat() a echoue : %s", conditionMessage(e))
    )
  )
  object@DB <- db$db

  # Les trois etapes qui suivent sont des etapes OFFICIELLES du workflow
  # CellChat, pas des optimisations maison (regle 3 : reutiliser, ne pas
  # reinventer). `subsetData()` est documentee comme « subset the expression
  # data of signaling genes for saving computation cost » : elle est reelle et
  # defendable, elle n'est pas a reimplementer.
  # Elles sont neanmoins NON BLOQUANTES : un echec local (version de paquet
  # differente) ne doit pas faire perdre le run — on continue sans l'etape et
  # on le trace, plutot que d'avorter sur un confort de calcul.
  .step <- function(label, fn) {
    .progress(label)
    out <- tryCatch(fn(object), error = function(e) { ts_log_swallow("sc_communication_engine.cellchat_step", e); NULL })
    if (!is.null(out)) object <<- out
    invisible(!is.null(out))
  }
  ran_sub <- .step("restriction aux gènes de signalisation",
                   function(o) CellChat::subsetData(o))
  ran_oe <- .step("gènes surexprimés",
                  function(o) CellChat::identifyOverExpressedGenes(o))
  ran_oi <- .step("interactions surexprimées",
                  function(o) CellChat::identifyOverExpressedInteractions(o))

  # Un jeu sans aucune paire LR exploitable fait echouer computeCommunProb() sur
  # une erreur opaque (« subscript out of bounds ») — verifie sur un jeu jouet.
  # On detecte le cas AVANT l'appel et on le traduit en etat `no_interactions`,
  # avec un message qui dit ce qui manque au lieu d'un indice de tableau.
  lrsig0 <- tryCatch(object@LR$LRsig, error = function(e) NULL)
  if (!is.null(lrsig0) && is.data.frame(lrsig0) && nrow(lrsig0) == 0L) {
    .cellchat_engine_stop(
      "no_interactions",
      paste0("aucune paire ligand–récepteur exploitable : CellChat n'a retenu ",
             "aucune interaction surexprimée sur ce jeu (gènes de signalisation ",
             "absents de la matrice, ou expression non différentielle entre les ",
             "populations). Le calcul est impossible ; aucun résultat n'est ",
             "fabriqué à partir de rien.")
    )
  }

  .progress(sprintf("inférence des communications (%d permutations)", nboot))
  object <- tryCatch(
    CellChat::computeCommunProb(object, nboot = nboot, seed.use = seed),
    error = function(e) .cellchat_engine_stop(
      "engine_failure",
      sprintf("computeCommunProb() a echoue : %s", conditionMessage(e))
    )
  )

  # Niveau pathway (netP) : indispensable pour que l'onglet « Heatmap
  # pathways » soit couvert au moins comme par la voie import (§3.6 de la
  # proposition). Les tableaux netP ne sont pas conserves.
  .progress("agrégation par voie de signalisation")
  n_pathways_significant <- NA_integer_
  netp <- tryCatch(CellChat::computeCommunProbPathway(object),
                   error = function(e) { ts_log_swallow("sc_communication_engine.cellchat_netp", e); NULL })
  ran_netp <- !is.null(netp)
  if (ran_netp) {
    object <- netp
    n_pathways_significant <- length(object@netP$pathways %||% character(0))
  }

  extracted <- .cellchat_engine_extract(object)

  # ── MOTEUR EPHEMERE : on retire la reference des la extraction faite ──────
  rm(object)

  identities <- unique(as.character(cellchat_input$meta[["labels"]]))
  identity_column <- as.character(cellchat_input$group_by)[1L]
  harm <- harmonize_communication_identities(
    extracted$table, identities, identity_column,
    context = "moteur CellChat"
  )
  qcr <- communication_import_qc(harm$table)

  if (nrow(qcr$table) == 0L) {
    .cellchat_engine_stop(
      "no_interactions",
      paste0("toutes les lignes extraites ont ete supprimees au QC ",
             "(sender/receiver/ligand/receptor vides) : aucun resultat ",
             "canonique produit.")
    )
  }

  # Etapes officiellement attendues mais non bloquantes : si l'une n'a pas
  # tourne, le resultat le DIT (jamais un silence qui change le calcul).
  steps <- c(
    if (ran_sub) "subsetData" else NULL,
    if (ran_oe) "identifyOverExpressedGenes" else NULL,
    if (ran_oi) "identifyOverExpressedInteractions" else NULL,
    if (ran_netp) "computeCommunProbPathway" else NULL
  )
  steps_warning <- if (length(steps) < 4L) {
    sprintf(paste0("%d etape(s) CellChat non executee(s) (%s) : le calcul a ",
                   "abouti differemment du workflow complet — la difference ",
                   "est tracee, pas masquee."),
            4L - length(steps),
            paste(setdiff(c("subsetData", "identifyOverExpressedGenes",
                            "identifyOverExpressedInteractions",
                            "computeCommunProbPathway"), steps), collapse = ", "))
  } else character(0)

  warnings_all <- unique(c(extracted$warnings, harm$warnings, qcr$warnings,
                           steps_warning))

  .progress("finalisation du résultat canonique")
  result <- finalize_communication_result(
    canonical_table   = qcr$table,
    source_method     = "cellchat",
    source_files      = list(),
    identity_column   = identity_column,
    identity_mapping  = harm$mapping,
    identity_summary  = harm$summary,
    column_mapping    = extracted$column_mapping,
    qc                = qcr$counts,
    n_input_rows      = extracted$n_input_rows,
    seurat_obj        = seurat_obj,
    extra_warnings    = warnings_all,
    analysis_id       = "sc-communication-engine",
    computation       = "engine"
  )

  # Champs d'identite moteur : ENRICHISSEMENT de la provenance deja produite,
  # jamais un mecanisme parallele (regle 3 — audit n°3 §10.2).
  result$provenance$engine           <- eng$engine
  result$provenance$engine_version   <- eng$engine_version
  result$provenance$engine_sha       <- eng$engine_sha
  result$provenance$database         <- db$name
  result$provenance$database_version <- db$version
  result$engine <- list(
    engine = eng$engine, engine_version = eng$engine_version,
    engine_sha = eng$engine_sha, database = db$name,
    database_version = db$version, seed = seed, nboot = nboot,
    n_populations = length(groups),
    n_pathways_significant = as.integer(n_pathways_significant),
    n_interactions = nrow(qcr$table),
    steps = steps
  )
  result$engine_path <- "B"

  .progress("terminé")
  result
}

#' Identite d'analyse — DERIVEE de la provenance, jamais dupliquee
#'
#' Un run n'est pas defini par la graine et la base seules : il faut aussi
#' l'entree, le moteur et ses parametres. Tous ces champs existent deja dans
#' `new_provenance_entry()` (analysis_id, seed, parameters, dataset_hash,
#' dataset_dims, versions) — cette fonction les DERIVE et n'y ajoute que les
#' deux champs que la provenance ne porte pas (`engine_sha`,
#' `database_version`). Aucun champ n'est recopie deux fois.
#'
#' @param result Resultat produit par run_cellchat().
#' @return Liste nommee plate.
cellchat_analysis_identity <- function(result) {
  if (!is.list(result) || is.null(result$provenance)) {
    .cellchat_engine_stop(
      "invalid_input",
      "cellchat_analysis_identity() : resultat run_cellchat() attendu."
    )
  }
  p <- result$provenance
  eng <- result$engine %||% list()
  list(
    analysis_id         = p$analysis_id %||% NA_character_,
    input_fingerprint   = p$dataset_hash %||% NA_character_,
    input_fingerprint_exact = isTRUE(p$hash_exact),
    engine              = eng$engine %||% NA_character_,
    engine_version      = eng$engine_version %||% NA_character_,
    engine_sha          = eng$engine_sha %||% NA_character_,
    database            = eng$database %||% NA_character_,
    database_version    = eng$database_version %||% NA_character_,
    seed                = eng$seed %||% NA_integer_,
    nboot               = eng$nboot %||% NA_integer_,
    n_populations       = eng$n_populations %||% NA_integer_,
    dataset_dims        = p$dataset_dims %||% c(NA_integer_, NA_integer_)
  )
}

#' Resume court du run (affichage UI / rapport)
cellchat_engine_summary <- function(result) {
  eng <- result$engine %||% list()
  data.frame(
    stringsAsFactors = FALSE,
    champ = c("Moteur", "Version", "Base LR", "Version base", "Graine",
              "Permutations", "Populations", "Interactions", "Voies significatives"),
    valeur = c(
      eng$engine %||% NA_character_,
      eng$engine_version %||% NA_character_,
      eng$database %||% NA_character_,
      if (is.null(eng$database_version) || is.na(eng$database_version))
        "non exposée par l'objet" else eng$database_version,
      format(eng$seed %||% NA),
      format(eng$nboot %||% NA),
      format(eng$n_populations %||% NA),
      format(eng$n_interactions %||% NA),
      format(eng$n_pathways_significant %||% NA)
    )
  )
}

#' Surface publique — gelée par tests/testthat/test-sc-communication-engine.R
#' (test éponyme C9 + assertions de gel du contrat)
cellchat_engine_public_api <- function() {
  c(
    "cellchat_analysis_identity", "cellchat_engine_available",
    "cellchat_engine_error_state", "cellchat_engine_public_api",
    "cellchat_engine_states", "cellchat_engine_summary",
    "run_cellchat"
  )
}

.LIANA_ENGINE_STATES <- c(
  "valid",
  "missing_dependency",
  "invalid_input",
  "invalid_parameters",
  "invalid_design",
  "engine_failure",
  "no_interactions"
)

.liana_engine_stop <- function(state, message) {
  stop(errorCondition(
    paste0("Moteur LIANA — ", message),
    class = c("liana_engine_error", "error", "condition"),
    state = state
  ))
}

.liana_description_field <- function(package, field) {
  value <- NULL
  description <- tryCatch(
    utils::packageDescription(package),
    error = function(e) NULL
  )
  if (!is.null(description) && field %in% names(description)) {
    value <- description[[field]]
  }
  if (is.null(value) || length(value) != 1L || is.na(value) ||
      !nzchar(as.character(value))) {
    dcf <- tryCatch(
      read.dcf(file.path(find.package(package), "DESCRIPTION"))[1L, ],
      error = function(e) NULL
    )
    if (!is.null(dcf) && field %in% names(dcf)) value <- dcf[[field]]
  }
  if (is.null(value) || length(value) != 1L || is.na(value) ||
      !nzchar(as.character(value))) {
    return(NA_character_)
  }
  as.character(value)
}

.liana_package_sha <- function(package) {
  value <- .liana_description_field(package, "RemoteSha")
  if (is.na(value)) value <- .liana_description_field(package, "GithubSHA1")
  value
}

.liana_resource_md5 <- function() {
  path <- tryCatch(
    system.file("omni_resources.rds", package = "liana"),
    error = function(e) ""
  )
  if (length(path) != 1L || !nzchar(path) || !file.exists(path)) {
    return(NA_character_)
  }
  unname(tools::md5sum(path))
}

.liana_engine_identity <- function() {
  list(
    engine = "LIANA",
    engine_version = .liana_description_field("liana", "Version"),
    engine_sha = .liana_package_sha("liana"),
    liana_version = .liana_description_field("liana", "Version"),
    liana_sha = .liana_package_sha("liana"),
    omnipathr_version = .liana_description_field("OmnipathR", "Version"),
    omnipathr_sha = .liana_package_sha("OmnipathR"),
    resource = "Consensus",
    resource_md5 = .liana_resource_md5()
  )
}

.liana_expected_resource_md5 <- function() {
  if (exists("TS_LIANA_RESOURCE_MD5", inherits = TRUE)) {
    return(as.character(TS_LIANA_RESOURCE_MD5)[1L])
  }
  "b807279afdb1b2f34fa83a3dd51bbcb5"
}

.liana_require_real <- function() {
  omnipathr_loaded <- "OmnipathR" %in% loadedNamespaces()
  previous_env <- Sys.getenv("OMNIPATHR_LOGFILE", unset = NA_character_)
  Sys.setenv(OMNIPATHR_LOGFILE = "none")
  previous_options <- options(omnipathr.logfile = "none")
  on.exit({
    options(previous_options)
    if (is.na(previous_env)) {
      Sys.unsetenv("OMNIPATHR_LOGFILE")
    } else {
      Sys.setenv(OMNIPATHR_LOGFILE = previous_env)
    }
  }, add = TRUE)
  required <- c("liana", "OmnipathR", "SingleCellExperiment", "SeuratObject", "Matrix")
  available <- vapply(
    required,
    function(package) requireNamespace(package, quietly = TRUE),
    logical(1)
  )
  if (any(!available)) {
    .liana_engine_stop(
      "missing_dependency",
      paste0(
        "paquet(s) requis absent(s) : ",
        paste(required[!available], collapse = ", "),
        ". Le moteur LIANA natif n'est pas disponible."
      )
    )
  }
  if (omnipathr_loaded) {
    tryCatch(
      OmnipathR::omnipath_set_loglevel("off", target = "logfile"),
      error = function(e) NULL
    )
  }
  actual <- .liana_resource_md5()
  expected <- .liana_expected_resource_md5()
  if (is.na(actual) || is.na(expected) || !identical(actual, expected)) {
    .liana_engine_stop(
      "engine_failure",
      sprintf(
        "ressource LIANA Consensus absente ou empreinte inattendue : %s.",
        actual
      )
    )
  }
  invisible(TRUE)
}

.liana_validate_scalar_text <- function(value, name, state = "invalid_input") {
  if (is.null(value) || length(value) != 1L || !is.atomic(value) ||
      is.na(value) || !nzchar(trimws(as.character(value)))) {
    .liana_engine_stop(
      state,
      sprintf("%s doit etre une chaine non vide.", name)
    )
  }
  trimws(as.character(value))
}

.liana_blank <- function(value) {
  is.na(value) | !nzchar(trimws(as.character(value)))
}

.liana_validate_run_parameters <- function(obj, idents_col, method, resource,
                                          seed, min_cells, assay = NULL) {
  if (is.null(obj) || !inherits(obj, "Seurat")) {
    .liana_engine_stop("invalid_input", "un objet Seurat est requis.")
  }
  idents_col <- .liana_validate_scalar_text(idents_col, "idents_col")
  method <- .liana_validate_scalar_text(method, "method", "invalid_parameters")
  resource <- .liana_validate_scalar_text(
    resource,
    "resource",
    "invalid_parameters"
  )
  method_expected <- if (exists("TS_LIANA_METHOD_DEFAULT", inherits = TRUE)) {
    as.character(TS_LIANA_METHOD_DEFAULT)[1L]
  } else "natmi"
  resource_expected <- if (exists("TS_LIANA_RESOURCE_DEFAULT", inherits = TRUE)) {
    as.character(TS_LIANA_RESOURCE_DEFAULT)[1L]
  } else "Consensus"
  if (!identical(tolower(method), tolower(method_expected))) {
    .liana_engine_stop(
      "invalid_parameters",
      sprintf("methode non supportee : %s.", method)
    )
  }
  if (!identical(tolower(resource), tolower(resource_expected))) {
    .liana_engine_stop(
      "invalid_parameters",
      sprintf("ressource non supportee : %s.", resource)
    )
  }
  if (is.null(seed) || length(seed) != 1L || !is.numeric(seed) ||
      is.na(seed) || !is.finite(seed) || seed != floor(seed)) {
    .liana_engine_stop(
      "invalid_parameters",
      "la graine doit etre un entier fini explicite."
    )
  }
  seed <- as.integer(seed)
  if (is.na(seed)) {
    .liana_engine_stop(
      "invalid_parameters",
      "la graine doit etre un entier fini explicite."
    )
  }
  if (is.null(min_cells) || length(min_cells) != 1L ||
      !is.numeric(min_cells) || is.na(min_cells) ||
      !is.finite(min_cells) || min_cells != floor(min_cells) ||
      min_cells < 1L) {
    .liana_engine_stop(
      "invalid_parameters",
      "min_cells doit etre un entier positif explicite."
    )
  }
  min_cells <- as.integer(min_cells)
  meta <- tryCatch(
    as.data.frame(obj[[]], stringsAsFactors = FALSE),
    error = function(e) NULL
  )
  if (!is.data.frame(meta) || ncol(obj) < 1L || nrow(meta) != ncol(obj)) {
    .liana_engine_stop(
      "invalid_input",
      "les metadonnees de l'objet Seurat sont absentes ou desalignees."
    )
  }
  if (!idents_col %in% colnames(meta)) {
    .liana_engine_stop(
      "invalid_input",
      sprintf("colonne d'identites '%s' absente.", idents_col)
    )
  }
  identities <- as.character(meta[[idents_col]])
  if (any(.liana_blank(identities))) {
    .liana_engine_stop(
      "invalid_input",
      sprintf("la colonne d'identites '%s' contient NA ou une valeur vide.", idents_col)
    )
  }
  identities <- trimws(identities)
  if (length(unique(identities)) < 2L) {
    .liana_engine_stop(
      "invalid_input",
      "au moins deux populations d'identites sont requises."
    )
  }
  assay <- if (is.null(assay)) {
    tryCatch(SeuratObject::DefaultAssay(obj), error = function(e) NULL)
  } else {
    .liana_validate_scalar_text(assay, "assay")
  }
  if (is.null(assay) || !nzchar(trimws(as.character(assay)))) {
    .liana_engine_stop("invalid_input", "assay non resolu.")
  }
  assay <- trimws(as.character(assay))
  available_assays <- tryCatch(names(obj@assays), error = function(e) character(0))
  if (!assay %in% available_assays) {
    .liana_engine_stop(
      "invalid_input",
      sprintf("assay '%s' absent de l'objet Seurat.", assay)
    )
  }
  list(
    obj = obj,
    meta = meta,
    idents_col = idents_col,
    identities = unique(identities),
    method = method_expected,
    resource = resource_expected,
    seed = seed,
    min_cells = min_cells,
    assay = assay,
    n_cells = as.integer(ncol(obj)),
    n_populations = as.integer(length(unique(identities)))
  )
}

.liana_validate_backend <- function(backend) {
  if (is.null(backend)) {
    .liana_require_real()
    return(.liana_real_backend())
  }
  if (!is.list(backend) || is.null(backend$run) || !is.function(backend$run)) {
    .liana_engine_stop(
      "invalid_parameters",
      "backend doit etre une liste contenant une fonction run(sce, ...)."
    )
  }
  backend
}

.liana_real_backend <- function() {
  list(
    run = function(sce, method, resource, idents_col, seed, min_cells,
                   assay = NULL, ...) {
      previous_env <- Sys.getenv("OMNIPATHR_LOGFILE", unset = NA_character_)
      Sys.setenv(OMNIPATHR_LOGFILE = "none")
      previous_options <- options(omnipathr.logfile = "none")
      on.exit({
        options(previous_options)
        if (is.na(previous_env)) {
          Sys.unsetenv("OMNIPATHR_LOGFILE")
        } else {
          Sys.setenv(OMNIPATHR_LOGFILE = previous_env)
        }
      }, add = TRUE)
      liana::liana_wrap(
        sce = sce,
         method = method,
         resource = resource,
        idents_col = idents_col,
        min_cells = min_cells,
        seed = seed,
        assay = assay,
        verbose = FALSE
      )
    }
  )
}

.liana_get_layer <- function(obj, assay, layer) {
  value <- tryCatch(
    SeuratObject::GetAssayData(obj, assay = assay, layer = layer),
    error = function(e) NULL
  )
  if (is.null(value)) {
    value <- tryCatch(
      SeuratObject::GetAssayData(obj, assay = assay, slot = layer),
      error = function(e) NULL
    )
  }
  if (is.null(value)) {
    .liana_engine_stop(
      "invalid_input",
      sprintf("couche '%s' indisponible dans l'assay '%s'.", layer, assay)
    )
  }
  value
}

.liana_build_sce <- function(obj, meta, assay) {
  counts <- .liana_get_layer(obj, assay, "counts")
  logcounts <- .liana_get_layer(obj, assay, "data")
  if ((!is.matrix(counts) && !inherits(counts, "Matrix")) ||
      (!is.matrix(logcounts) && !inherits(logcounts, "Matrix")) ||
      length(dim(counts)) != 2L || length(dim(logcounts)) != 2L ||
      nrow(counts) < 1L || ncol(counts) < 1L ||
      !identical(dim(counts), dim(logcounts))) {
    .liana_engine_stop(
      "invalid_input",
      "les couches counts et logcounts doivent etre deux matrices compatibles."
    )
  }
  cells <- colnames(obj)
  if (is.null(cells) || any(.liana_blank(cells))) {
    cells <- rownames(meta)
  }
  if (is.null(cells) || length(cells) != ncol(counts) ||
      any(.liana_blank(cells)) || anyDuplicated(cells)) {
    cells <- paste0("cell_", seq_len(ncol(counts)))
  }
  if (is.null(colnames(counts))) colnames(counts) <- cells
  if (is.null(colnames(logcounts))) colnames(logcounts) <- cells
  if (!identical(colnames(counts), cells) ||
      !identical(colnames(logcounts), cells)) {
    .liana_engine_stop(
      "invalid_input",
      "les noms de cellules des couches expression ne sont pas alignes."
    )
  }
  counts <- Matrix::Matrix(counts, sparse = TRUE)
  logcounts <- Matrix::Matrix(logcounts, sparse = TRUE)
  coldata <- as.data.frame(meta, stringsAsFactors = FALSE)
  rownames(coldata) <- cells
  SingleCellExperiment::SingleCellExperiment(
    list(counts = counts, logcounts = logcounts),
    colData = coldata
  )
}

.liana_pick_column <- function(tab, candidates) {
  names <- colnames(tab)
  for (candidate in candidates) {
    hit <- which(tolower(names) == tolower(candidate))
    if (length(hit)) return(names[hit[1L]])
  }
  NA_character_
}

.liana_input_usage <- function(sce, validated, apply_resource_filter = FALSE) {
  counts <- SummarizedExperiment::assay(sce, "counts")
  cells <- colnames(sce)
  meta <- validated$meta
  if (is.null(cells)) cells <- as.character(seq_len(ncol(counts)))
  if (nrow(meta) != length(cells)) {
    .liana_engine_stop("invalid_input", "les cellules du SCE et ses metadonnees sont desalignees.")
  }
  if (is.null(rownames(meta))) rownames(meta) <- cells
  if (!identical(as.character(rownames(meta)), as.character(cells))) {
    index <- match(cells, rownames(meta))
    if (anyNA(index)) {
      .liana_engine_stop("invalid_input", "les noms de cellules du SCE sont absents des metadonnees.")
    }
    meta <- meta[index, , drop = FALSE]
  }
  identities <- trimws(as.character(meta[[validated$idents_col]]))
  input_counts <- table(identities)
  retained <- names(input_counts)[input_counts >= validated$min_cells]
  nonzero <- Matrix::colSums(counts) > 0
  used <- nonzero & identities %in% retained
  if (isTRUE(apply_resource_filter)) {
    resource_path <- system.file("omni_resources.rds", package = "liana")
    resource <- tryCatch(
      readRDS(resource_path)[["Consensus"]],
      error = function(e) { ts_log_swallow("sc_communication_engine.liana_resource", e); NULL }
    )
    if (!is.data.frame(resource) ||
        !all(c("source_genesymbol", "target_genesymbol") %in% colnames(resource))) {
      .liana_engine_stop(
        "engine_failure",
        "la ressource LIANA Consensus ne contient pas les genes ligand-recepteur."
      )
    }
    split_genes <- function(values) {
      values <- as.character(values)
      values <- values[!is.na(values) & nzchar(values)]
      unique(unlist(stringr::str_split(values, "[_]"), use.names = FALSE))
    }
    entity_genes <- unique(c(
      split_genes(resource$source_genesymbol),
      split_genes(resource$target_genesymbol)
    ))
    lr_rows <- intersect(rownames(counts), entity_genes)
    lr_nonzero <- Matrix::colSums(counts[lr_rows, , drop = FALSE]) > 0
    used <- used & lr_nonzero
  }
  used_identities <- identities[used]
  used_counts <- table(used_identities)
  list(
    n_cells_input = as.integer(length(cells)),
    n_cells_used = as.integer(sum(used)),
    n_cells_excluded = as.integer(sum(!used)),
    n_genes_input = as.integer(nrow(counts)),
    n_genes_used = as.integer(sum(Matrix::rowSums(counts) > 0)),
    n_populations_input = as.integer(length(input_counts)),
    n_populations_used = as.integer(length(used_counts)),
    identities_used = names(used_counts),
    identities_excluded = setdiff(names(input_counts), names(used_counts)),
    cells_used_ids = cells[used],
    cells_by_identity = input_counts
  )
}

.liana_raw_to_canonical <- function(raw) {
  tab <- tryCatch(as.data.frame(raw), error = function(e) { ts_log_swallow("sc_communication_engine.liana_canonical", e); NULL })
  if (!is.data.frame(tab) || nrow(tab) == 0L) {
    .liana_engine_stop(
      "no_interactions",
      "le backend LIANA n'a retourne aucune interaction."
    )
  }
  source_col <- .liana_pick_column(tab, c("source", "sender"))
  receiver_col <- .liana_pick_column(tab, c("target", "receiver"))
  ligand_col <- .liana_pick_column(tab, c("ligand"))
  receptor_col <- .liana_pick_column(tab, c("receptor"))
  ligand_complex_col <- .liana_pick_column(tab, c("ligand.complex", "ligand_complex"))
  receptor_complex_col <- .liana_pick_column(tab, c("receptor.complex", "receptor_complex"))
  score_col <- .liana_pick_column(tab, c("edge_specificity"))
  if (is.na(ligand_col) && is.na(ligand_complex_col)) {
    ligand_col <- ligand_complex_col
  }
  if (is.na(receptor_col) && is.na(receptor_complex_col)) {
    receptor_col <- receptor_complex_col
  }
  required <- c(source_col, receiver_col, ligand_col, receptor_col, score_col)
  if (anyNA(required)) {
    .liana_engine_stop(
      "engine_failure",
      "la sortie LIANA ne contient pas les colonnes canoniques attendues."
    )
  }
  raw_score <- tab[[score_col]]
  score <- suppressWarnings(as.numeric(as.character(raw_score)))
  invalid_score <- is.na(score) & !.liana_blank(raw_score)
  invalid_score <- invalid_score | (!is.na(score) & !is.finite(score))
  score[!is.finite(score)] <- NA_real_
  if (!any(is.finite(score))) {
    .liana_engine_stop(
      "no_interactions",
      "la sortie LIANA ne contient aucune valeur edge_specificity finie."
    )
  }
  ligand <- as.character(tab[[ligand_col]])
  receptor <- as.character(tab[[receptor_col]])
  if (!is.na(ligand_complex_col)) {
    ligand_complex <- as.character(tab[[ligand_complex_col]])
    ligand[.liana_blank(ligand)] <- ligand_complex[.liana_blank(ligand)]
  } else {
    ligand_complex <- ligand
  }
  if (!is.na(receptor_complex_col)) {
    receptor_complex <- as.character(tab[[receptor_complex_col]])
    receptor[.liana_blank(receptor)] <- receptor_complex[.liana_blank(receptor)]
  } else {
    receptor_complex <- receptor
  }
  n <- nrow(tab)
  table <- data.frame(
    sender = as.character(tab[[source_col]]),
    receiver = as.character(tab[[receiver_col]]),
    ligand = ligand,
    receptor = receptor,
    interaction = paste(ligand, receptor, sep = " -> "),
    pathway = rep(NA_character_, n),
    score = score,
    p_value = rep(NA_real_, n),
    p_adjusted = rep(NA_real_, n),
    source_method = rep("liana", n),
    source_file = rep(NA_character_, n),
    source_cell_identity_level = rep(NA_character_, n),
    ligand_complex = ligand_complex,
    receptor_complex = receptor_complex,
    stringsAsFactors = FALSE
  )
  warnings <- character(0)
  if (any(invalid_score)) {
    warnings <- c(warnings, sprintf(
      "%d valeur(s) edge_specificity non finie(s) mises a NA.",
      sum(invalid_score)
    ))
  }
  list(
    table = table,
    column_mapping = list(
      sender = source_col,
      receiver = receiver_col,
      ligand = ligand_col,
      receptor = receptor_col,
      score = score_col,
      pathway = NA_character_,
      p_value = NA_character_,
      p_adjusted = NA_character_
    ),
    n_input_rows = as.integer(n),
    warnings = warnings
  )
}

.liana_finalize_result <- function(extracted, validated, usage) {
  harm <- tryCatch(
    harmonize_communication_identities(
      extracted$table,
      validated$identities,
      validated$idents_col,
      context = "moteur LIANA"
    ),
    error = function(e) {
      if (inherits(e, "liana_engine_error")) stop(e)
      .liana_engine_stop(
        "engine_failure",
        paste0("harmonisation LIANA impossible : ", conditionMessage(e))
      )
    }
  )
  qcr <- tryCatch(
    communication_import_qc(harm$table),
    error = function(e) {
      if (inherits(e, "liana_engine_error")) stop(e)
      .liana_engine_stop(
        "engine_failure",
        paste0("QC LIANA impossible : ", conditionMessage(e))
      )
    }
  )
  if (nrow(qcr$table) == 0L) {
    .liana_engine_stop(
      "no_interactions",
      "toutes les lignes LIANA ont ete supprimees au QC."
    )
  }
  result <- tryCatch(
    finalize_communication_result(
      canonical_table = qcr$table,
      source_method = "liana",
      source_files = list(),
      identity_column = validated$idents_col,
      identity_mapping = harm$mapping,
      identity_summary = harm$summary,
      column_mapping = extracted$column_mapping,
      qc = qcr$counts,
      n_input_rows = extracted$n_input_rows,
      seurat_obj = validated$obj,
      extra_warnings = unique(c(extracted$warnings, harm$warnings, qcr$warnings)),
      analysis_id = "sc-communication-liana-engine",
      computation = "engine"
    ),
    error = function(e) {
      if (inherits(e, "liana_engine_error")) stop(e)
      .liana_engine_stop(
        "engine_failure",
        paste0("finalisation LIANA impossible : ", conditionMessage(e))
      )
    }
  )
  identity <- .liana_engine_identity()
  provenance <- result$provenance
  provenance$engine <- identity$engine
  provenance$engine_version <- identity$engine_version
  provenance$engine_sha <- identity$engine_sha
  provenance$liana_version <- identity$liana_version
  provenance$liana_sha <- identity$liana_sha
  provenance$omnipathr_version <- identity$omnipathr_version
  provenance$omnipathr_sha <- identity$omnipathr_sha
  provenance$engine_method <- validated$method
  provenance$engine_resource <- validated$resource
  provenance$resource <- validated$resource
  provenance$resource_md5 <- identity$resource_md5
  provenance$seed <- validated$seed
  provenance$cells_used <- usage$n_cells_used
  provenance$cells_excluded <- usage$n_cells_excluded
  provenance$parameters$method <- validated$method
  provenance$parameters$resource <- validated$resource
  provenance$parameters$resource_md5 <- identity$resource_md5
  provenance$parameters$assay <- validated$assay
  provenance$parameters$min_cells <- validated$min_cells
  provenance$parameters$seed <- validated$seed
  provenance$parameters$n_cells <- validated$n_cells
  provenance$parameters$n_cells_input <- usage$n_cells_input
  provenance$parameters$n_cells_used <- usage$n_cells_used
  provenance$parameters$n_cells_excluded <- usage$n_cells_excluded
  provenance$parameters$n_genes_input <- usage$n_genes_input
  provenance$parameters$n_genes_used <- usage$n_genes_used
  provenance$parameters$n_populations <- validated$n_populations
  provenance$parameters$n_populations_used <- usage$n_populations_used
  provenance$parameters$identities_excluded <- usage$identities_excluded
  provenance$parameters$n_interactions <- as.integer(nrow(qcr$table))
  result$provenance <- provenance
  result$engine <- list(
    engine = identity$engine,
    engine_version = identity$engine_version,
    engine_sha = identity$engine_sha,
    liana_version = identity$liana_version,
    liana_sha = identity$liana_sha,
    omnipathr_version = identity$omnipathr_version,
    omnipathr_sha = identity$omnipathr_sha,
    method = validated$method,
    resource = validated$resource,
    resource_md5 = identity$resource_md5,
    assay = validated$assay,
    min_cells = validated$min_cells,
    seed = validated$seed,
    n_cells_input = usage$n_cells_input,
    n_cells_used = usage$n_cells_used,
    n_cells_excluded = usage$n_cells_excluded,
    n_genes_input = usage$n_genes_input,
    n_genes_used = usage$n_genes_used,
    n_populations_input = usage$n_populations_input,
    n_populations_used = usage$n_populations_used,
    identities_excluded = usage$identities_excluded,
    cells_used_ids = usage$cells_used_ids,
    n_populations = validated$n_populations,
    n_input_rows = extracted$n_input_rows,
    n_interactions = as.integer(nrow(qcr$table))
  )
  result$engine_path <- "B"
  result
}

liana_engine_states <- function() .LIANA_ENGINE_STATES

liana_engine_error_state <- function(e) ts_error_state(e, "liana_engine_error")

liana_engine_available <- function() {
  previous_env <- Sys.getenv("OMNIPATHR_LOGFILE", unset = NA_character_)
  Sys.setenv(OMNIPATHR_LOGFILE = "none")
  previous_options <- options(omnipathr.logfile = "none")
  on.exit({
    options(previous_options)
    if (is.na(previous_env)) {
      Sys.unsetenv("OMNIPATHR_LOGFILE")
    } else {
      Sys.setenv(OMNIPATHR_LOGFILE = previous_env)
    }
  }, add = TRUE)
  required <- c("liana", "OmnipathR", "SingleCellExperiment", "SeuratObject", "Matrix")
  available <- all(vapply(
    required,
    function(package) requireNamespace(package, quietly = TRUE),
    logical(1)
  ))
  if (available &&
      !nzchar(system.file("omni_resources.rds", package = "liana"))) {
    available <- FALSE
  }
  if (available) {
    tryCatch(
      OmnipathR::omnipath_set_loglevel("off", target = "logfile"),
      error = function(e) NULL
    )
  }
  available
}

liana_engine_public_api <- function() {
  c(
    "assert_liana_collection",
    "build_liana_collection_table",
    "liana_collection_active",
    "liana_collection_condition_summary",
    "liana_collection_sample_manifest",
    "liana_engine_available",
    "liana_engine_error_state",
    "liana_engine_public_api",
    "liana_engine_states",
    "run_liana",
    "run_liana_by_sample"
  )
}

run_liana <- function(obj, idents_col, method, resource, seed, min_cells = 1L,
                      assay = NULL, backend = NULL, on_progress = NULL) {
  if (missing(obj) || missing(idents_col) || missing(method) ||
      missing(resource) || missing(seed)) {
    .liana_engine_stop(
      "invalid_parameters",
      "obj, idents_col, method, resource, seed et min_cells sont requis."
    )
  }
  if (!is.null(on_progress) && !is.function(on_progress)) {
    .liana_engine_stop(
      "invalid_parameters",
      "on_progress doit etre une fonction ou NULL."
    )
  }
  validated <- .liana_validate_run_parameters(
    obj, idents_col, method, resource, seed, min_cells, assay
  )
  real_backend <- is.null(backend)
  backend <- .liana_validate_backend(backend)
  progress <- function(message) {
    if (is.function(on_progress)) {
      tryCatch(on_progress(message), error = function(e) { ts_log_swallow("sc_communication_engine.liana_progress", e); NULL })
    }
  }
  progress("construction du SCE sparse")
  sce <- .liana_build_sce(validated$obj, validated$meta, validated$assay)
  usage <- .liana_input_usage(
    sce,
    validated,
    apply_resource_filter = real_backend
  )
  if (usage$n_populations_used < 2L) {
    .liana_engine_stop(
      "no_interactions",
      "moins de deux populations restent disponibles apres le filtrage LIANA."
    )
  }
  on.exit({
    if (exists("sce", envir = environment(), inherits = FALSE)) {
      rm(list = "sce", envir = environment())
    }
    if (exists("raw", envir = environment(), inherits = FALSE)) {
      rm(list = "raw", envir = environment())
    }
    if (exists("usage", envir = environment(), inherits = FALSE)) {
      rm(list = "usage", envir = environment())
    }
  }, add = TRUE)
  progress("execution du backend LIANA")
  raw <- tryCatch(
    backend$run(
      sce,
      method = validated$method,
      resource = validated$resource,
      idents_col = validated$idents_col,
      seed = validated$seed,
      min_cells = validated$min_cells,
      assay = validated$assay
    ),
    error = function(e) {
      if (inherits(e, "liana_engine_error")) stop(e)
      .liana_engine_stop(
        "engine_failure",
        paste0("le backend LIANA a echoue : ", conditionMessage(e))
      )
    }
  )
  rm(list = "sce", envir = environment())
  extracted <- .liana_raw_to_canonical(raw)
  rm(list = "raw", envir = environment())
  progress("finalisation du resultat canonique")
  .liana_finalize_result(extracted, validated, usage)
}

.liana_validate_sample_design <- function(obj, sample_col, condition_col,
                                           idents_col, min_cells) {
  sample_col <- .liana_validate_scalar_text(sample_col, "sample_col")
  condition_col <- .liana_validate_scalar_text(condition_col, "condition_col")
  idents_col <- .liana_validate_scalar_text(idents_col, "idents_col")
  if (!inherits(obj, "Seurat")) {
    .liana_engine_stop("invalid_input", "un objet Seurat est requis.")
  }
  meta <- tryCatch(
    as.data.frame(obj[[]], stringsAsFactors = FALSE),
    error = function(e) { ts_log_swallow("sc_communication_engine.liana_meta", e); NULL }
  )
  if (!is.data.frame(meta) || ncol(obj) < 1L || nrow(meta) != ncol(obj)) {
    .liana_engine_stop(
      "invalid_input",
      "les metadonnees de l'objet Seurat sont absentes ou desalignees."
    )
  }
  required_columns <- c(sample_col, condition_col, idents_col)
  missing_columns <- setdiff(required_columns, colnames(meta))
  if (length(missing_columns)) {
    .liana_engine_stop(
      "invalid_input",
      sprintf("colonne(s) absente(s) : %s.", paste(missing_columns, collapse = ", "))
    )
  }
  sample_raw <- as.character(meta[[sample_col]])
  condition_raw <- as.character(meta[[condition_col]])
  identity_raw <- as.character(meta[[idents_col]])
  if (any(.liana_blank(sample_raw)) || any(.liana_blank(condition_raw)) ||
      any(.liana_blank(identity_raw))) {
    .liana_engine_stop(
      "invalid_input",
      "sample, condition et idents ne peuvent contenir ni NA ni valeur vide."
    )
  }
  sample_ids <- trimws(sample_raw)
  conditions <- trimws(condition_raw)
  identities <- trimws(identity_raw)
  if (any(grepl("::", sample_ids, fixed = TRUE)) ||
      any(grepl("::", conditions, fixed = TRUE))) {
    .liana_engine_stop(
      "invalid_design",
      "sample_id et condition ne peuvent contenir le separateur '::'."
    )
  }
  cells <- colnames(obj)
  if (is.null(cells)) cells <- rownames(meta)
  if (!is.null(cells) && any(tolower(sample_ids) %in% tolower(cells))) {
    .liana_engine_stop(
      "invalid_design",
      "un sample_id est identique a un code-barres de cellule."
    )
  }
  if (any(tolower(sample_ids) %in% tolower(conditions))) {
    .liana_engine_stop(
      "invalid_design",
      "un sample_id est identique a un nom de condition."
    )
  }
  sample_order <- unique(sample_ids)
  condition_order <- unique(conditions)
  condition_by_sample <- lapply(sample_order, function(sample) {
    unique(conditions[sample_ids == sample])
  })
  if (any(vapply(condition_by_sample, length, integer(1)) != 1L)) {
    .liana_engine_stop(
      "invalid_design",
      "un echantillon est associe a plusieurs conditions."
    )
  }
  sample_conditions <- unname(vapply(
    seq_along(sample_order),
    function(i) condition_by_sample[[i]],
    character(1L)
  ))
  sample_counts <- vapply(
    sample_order,
    function(sample) sum(sample_ids == sample),
    integer(1L)
  )
  if (any(sample_counts < min_cells)) {
    .liana_engine_stop(
      "invalid_input",
      "un echantillon contient moins de min_cells cellules."
    )
  }
  sample_identities <- lapply(sample_order, function(sample) {
    unique(identities[sample_ids == sample])
  })
  if (any(vapply(sample_identities, length, integer(1)) < 2L)) {
    .liana_engine_stop(
      "invalid_design",
      "chaque echantillon doit contenir au moins deux populations."
    )
  }
  list(
    meta = meta,
    sample_col = sample_col,
    condition_col = condition_col,
    idents_col = idents_col,
    sample_ids = sample_ids,
    conditions = conditions,
    sample_order = sample_order,
    condition_order = condition_order,
     sample_conditions = sample_conditions,
     sample_counts = as.integer(sample_counts),
     identities = identities
   )
}

run_liana_by_sample <- function(obj, sample_col, condition_col, idents_col,
                                method, resource, seed, min_cells = 1L,
                                assay = NULL, backend = NULL, on_progress = NULL) {
  if (missing(obj) || missing(sample_col) || missing(condition_col) ||
      missing(idents_col) || missing(method) || missing(resource) ||
      missing(seed)) {
    .liana_engine_stop(
      "invalid_parameters",
      "obj, sample_col, condition_col, idents_col, method, resource, seed et min_cells sont requis."
    )
  }
  if (!is.null(on_progress) && !is.function(on_progress)) {
    .liana_engine_stop(
      "invalid_parameters",
      "on_progress doit etre une fonction ou NULL."
    )
  }
  validated <- .liana_validate_run_parameters(
    obj, idents_col, method, resource, seed, min_cells, assay
  )
  design <- .liana_validate_sample_design(
    validated$obj,
    sample_col,
    condition_col,
    validated$idents_col,
    validated$min_cells
  )
  real_backend <- is.null(backend)
  backend <- .liana_validate_backend(backend)
  progress <- function(message) {
    if (is.function(on_progress)) {
      tryCatch(on_progress(message), error = function(e) { ts_log_swallow("sc_communication_engine.liana_progress", e); NULL })
    }
  }
  sample_keys <- paste0(design$sample_conditions, "::", design$sample_order)
  backend_run <- backend$run
  sample_backend <- if (real_backend) {
    NULL
  } else {
    list(
      run = function(sce, ...) backend_run(sce, ...)
    )
  }
  results <- vector("list", length(design$sample_order))
  for (i in seq_along(design$sample_order)) {
    cells <- which(design$sample_ids == design$sample_order[[i]])
    progress(sprintf("execution LIANA pour l'echantillon %s", sample_keys[[i]]))
    subset_obj <- tryCatch(
      validated$obj[, cells, drop = FALSE],
      error = function(e) { ts_log_swallow("sc_communication_engine.liana_subset", e); NULL }
    )
    if (is.null(subset_obj)) {
      .liana_engine_stop(
        "engine_failure",
        sprintf("echantillon %s : impossible de construire le sous-ensemble.", sample_keys[[i]])
      )
    }
    result <- tryCatch(
      run_liana(
        subset_obj,
        idents_col = validated$idents_col,
        method = validated$method,
        resource = validated$resource,
        seed = validated$seed,
        min_cells = validated$min_cells,
        assay = validated$assay,
        backend = sample_backend,
        on_progress = on_progress
      ),
      error = function(e) e
    )
    if (inherits(result, "condition")) {
      state <- liana_engine_error_state(result)
      if (is.na(state) || !state %in% .LIANA_ENGINE_STATES) {
        state <- "engine_failure"
      }
      .liana_engine_stop(
        state,
        sprintf(
          "echantillon %s : %s",
          sample_keys[[i]],
          conditionMessage(result)
        )
      )
    }
    result$canonical_table$sample_id <- rep(
      design$sample_order[[i]],
      nrow(result$canonical_table)
    )
    result$canonical_table$condition <- rep(
      design$sample_conditions[[i]],
      nrow(result$canonical_table)
    )
    result$canonical_table$sample_key <- rep(
      sample_keys[[i]],
      nrow(result$canonical_table)
    )
    result$sample_id <- design$sample_order[[i]]
    result$condition <- design$sample_conditions[[i]]
    result$sample_key <- sample_keys[[i]]
    sample_analysis_id <- paste0(
      "sc-communication-liana-",
      gsub("[^A-Za-z0-9]+", "-", result$sample_key)
    )
    result$analysis_id <- sample_analysis_id
    result$provenance$analysis_id <- sample_analysis_id
    result$provenance$sample_id <- result$sample_id
    result$provenance$condition <- result$condition
    result$provenance$sample_key <- result$sample_key
    result$provenance$parameters$sample_id <- result$sample_id
    result$provenance$parameters$condition <- result$condition
    result$provenance$parameters$sample_key <- result$sample_key
    result$provenance$parameters$base_analysis_id <- "sc-communication-liana-engine"
    result$engine$sample_id <- result$sample_id
    result$engine$condition <- result$condition
    result$engine$sample_key <- result$sample_key
    results[[i]] <- result
    rm(list = c("subset_obj", "result"), envir = environment())
  }
  interaction_counts <- vapply(
    results,
    function(result) nrow(result$canonical_table),
    integer(1L)
  )
  manifest <- data.frame(
    sample_id = design$sample_order,
    condition = design$sample_conditions,
    sample_key = sample_keys,
    analysis_id = vapply(results, function(result) result$analysis_id, character(1L)),
    n_cells = design$sample_counts,
    n_cells_input = vapply(results, function(result) result$engine$n_cells_input, integer(1L)),
    n_cells_used = vapply(results, function(result) result$engine$n_cells_used, integer(1L)),
    n_cells_excluded = vapply(results, function(result) result$engine$n_cells_excluded, integer(1L)),
    n_populations_used = vapply(results, function(result) result$engine$n_populations_used, integer(1L)),
    n_interactions = as.integer(interaction_counts),
    stringsAsFactors = FALSE
  )
  samples_per_condition <- vapply(
    design$condition_order,
    function(condition) sum(design$sample_conditions == condition),
    integer(1L)
  )
  cells_per_condition <- vapply(
    design$condition_order,
    function(condition) {
      sum(design$sample_counts[design$sample_conditions == condition])
    },
    integer(1L)
  )
  used_cells_per_condition <- vapply(
    design$condition_order,
    function(condition) {
      sum(vapply(
        results[design$sample_conditions == condition],
        function(result) result$engine$n_cells_used,
        integer(1L)
      ))
    },
    integer(1L)
  )
  condition_summary <- data.frame(
    condition = design$condition_order,
    n_samples = as.integer(samples_per_condition),
    n_cells = as.integer(cells_per_condition),
    n_cells_used = as.integer(used_cells_per_condition),
    stringsAsFactors = FALSE
  )
  identity <- .liana_engine_identity()
  collection_provenance <- new_provenance_entry(
    analysis_id = "sc-communication-liana-collection",
    method = "liana",
    parameters = list(
      method = validated$method,
      resource = validated$resource,
      resource_md5 = identity$resource_md5,
      sample_col = design$sample_col,
      condition_col = design$condition_col,
      idents_col = design$idents_col,
      seed = validated$seed,
       min_cells = validated$min_cells,
       n_samples = as.integer(length(design$sample_order)),
       n_cells = as.integer(sum(design$sample_counts)),
       n_cells_used = as.integer(sum(vapply(
         results,
         function(result) result$engine$n_cells_used,
         integer(1L)
       ))),
       n_cells_excluded = as.integer(sum(vapply(
         results,
         function(result) result$engine$n_cells_excluded,
         integer(1L)
       ))),
       n_populations = validated$n_populations,
       n_interactions = as.integer(sum(interaction_counts))
    ),
    dataset = validated$obj,
    cells_used = as.integer(sum(vapply(
      results,
      function(result) result$engine$n_cells_used,
      integer(1L)
    ))),
    cells_excluded = as.integer(sum(vapply(
      results,
      function(result) result$engine$n_cells_excluded,
      integer(1L)
    ))),
    seed = validated$seed
  )
  collection_provenance$engine <- identity$engine
  collection_provenance$engine_version <- identity$engine_version
  collection_provenance$engine_sha <- identity$engine_sha
  collection_provenance$liana_version <- identity$liana_version
  collection_provenance$liana_sha <- identity$liana_sha
  collection_provenance$omnipathr_version <- identity$omnipathr_version
  collection_provenance$omnipathr_sha <- identity$omnipathr_sha
  collection_provenance$resource <- validated$resource
  collection_provenance$resource_md5 <- identity$resource_md5
  collection_provenance$import_only <- FALSE
  collection_provenance$computation_path <- "B"
  cell_ids <- rownames(design$meta)
  if (is.null(cell_ids)) cell_ids <- colnames(validated$obj)
  if (is.null(cell_ids)) cell_ids <- as.character(seq_len(nrow(design$meta)))
  cell_design <- data.frame(
    cell_id = as.character(cell_ids),
    sample_id = as.character(design$sample_ids),
    condition = as.character(design$conditions),
    identity = as.character(design$identities),
    stringsAsFactors = FALSE
  )
  collection <- list(
    type = "liana_collection",
    status = "valid",
    source_method = "liana",
    analysis_id = "sc-communication-liana-collection",
    timestamp_utc = format(
      collection_provenance$timestamp,
      "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    sample_col = design$sample_col,
    condition_col = design$condition_col,
    idents_col = design$idents_col,
    method = validated$method,
    resource = validated$resource,
    resource_md5 = identity$resource_md5,
    engine = identity,
    object_identity = build_object_identity_v2(validated$obj),
    active_sample_key = sample_keys[[1L]],
    sample_manifest = manifest,
    condition_summary = condition_summary,
    cell_design = cell_design,
    results = results,
    provenance = collection_provenance
  )
  names(collection$results) <- sample_keys
  class(collection) <- c("liana_collection", "list")
  collection
}

.liana_collection_has_forbidden <- function(value) {
  if (is.null(value)) return(FALSE)
  if (is.environment(value) || is.function(value) ||
      inherits(value, "SingleCellExperiment") ||
      inherits(value, "Seurat") || inherits(value, "CellChat") ||
      inherits(value, "Matrix")) {
    return(TRUE)
  }
  if (is.list(value)) {
    for (element in value) {
      if (.liana_collection_has_forbidden(element)) return(TRUE)
    }
  }
  FALSE
}

assert_liana_collection <- function(collection, seurat_obj = NULL) {
  invalid <- function(message) .liana_engine_stop("invalid_input", message)
  if (!is.list(collection) || !inherits(collection, "liana_collection") ||
      !identical(collection$type, "liana_collection") ||
      !identical(collection$status, "valid")) {
    invalid("collection LIANA valide requise.")
  }
  results <- collection$results
  manifest <- collection$sample_manifest
  summary <- collection$condition_summary
  cell_design <- collection$cell_design
  if (!is.list(results) || is.data.frame(results) || !length(results) ||
      !is.data.frame(manifest) || !is.data.frame(summary) ||
      !is.data.frame(cell_design) ||
      !all(c("cell_id", "sample_id", "condition", "identity") %in%
            colnames(cell_design)) || anyDuplicated(cell_design$cell_id)) {
     invalid("la collection LIANA ne contient pas ses tables et resultats.")
   }
  if (any(.liana_blank(cell_design$cell_id)) ||
      any(.liana_blank(cell_design$sample_id)) ||
      any(.liana_blank(cell_design$condition)) ||
      any(.liana_blank(cell_design$identity))) {
    invalid("le design cellulaire de la collection est incomplet.")
  }
  if (!all(c("sample_id", "condition", "sample_key", "analysis_id", "n_cells",
            "n_cells_input", "n_cells_used", "n_cells_excluded",
            "n_populations_used", "n_interactions") %in% colnames(manifest)) ||
      !all(c("condition", "n_samples", "n_cells", "n_cells_used") %in%
            colnames(summary))) {
    invalid("le manifeste ou le resume de la collection est incomplet.")
  }
  keys <- as.character(manifest$sample_key)
  analysis_ids <- as.character(manifest$analysis_id)
  if (any(.liana_blank(keys)) || anyDuplicated(keys) ||
      any(.liana_blank(analysis_ids)) || anyDuplicated(analysis_ids) ||
      !setequal(keys, names(results)) ||
      !identical(as.character(manifest$sample_key),
                 paste(manifest$condition, manifest$sample_id, sep = "::"))) {
    invalid("les echantillons ou analyses de la collection ne sont pas uniques et complets.")
  }
  if (length(collection$active_sample_key) != 1L ||
      is.na(collection$active_sample_key) ||
      !as.character(collection$active_sample_key) %in% keys) {
    invalid("l'echantillon actif de la collection est invalide.")
  }
  if (any(manifest$n_cells < 1L) || any(manifest$n_cells_input < 1L) ||
      any(manifest$n_cells_used < 1L) || any(manifest$n_cells_excluded < 0L) ||
      any(manifest$n_populations_used < 2L) || any(manifest$n_interactions < 1L) ||
      any(summary$n_samples < 1L) || any(summary$n_cells < 1L) ||
      any(summary$n_cells_used < 1L)) {
    invalid("les compteurs de la collection sont invalides.")
  }
  expected_conditions <- unique(as.character(manifest$condition))
  expected_samples <- vapply(
    expected_conditions,
    function(condition) sum(as.character(manifest$condition) == condition),
    integer(1L)
  )
  expected_cells <- vapply(
    expected_conditions,
    function(condition) {
      sum(as.integer(manifest$n_cells[as.character(manifest$condition) == condition]))
    },
    integer(1L)
  )
  expected_used <- vapply(
    expected_conditions,
    function(condition) {
      sum(as.integer(manifest$n_cells_used[as.character(manifest$condition) == condition]))
    },
    integer(1L)
  )
  if (!identical(as.character(summary$condition), expected_conditions) ||
      !identical(as.integer(summary$n_samples), as.integer(expected_samples)) ||
      !identical(as.integer(summary$n_cells), as.integer(expected_cells)) ||
      !identical(as.integer(summary$n_cells_used), as.integer(expected_used))) {
    invalid("le resume par condition ne reconcilie pas le manifeste.")
  }
  for (i in seq_along(results)) {
    result <- results[[i]]
    valid_result <- tryCatch({
      assert_communication_result(
        result,
        context = "collection LIANA"
      )
      is.data.frame(result$canonical_table) &&
        nrow(result$canonical_table) > 0L &&
        all(c("sample_id", "condition", "sample_key") %in%
              colnames(result$canonical_table))
    }, error = function(e) { ts_log_swallow("sc_communication_engine.liana_collection_valid", e); FALSE })
    if (!valid_result) invalid("un resultat de la collection est invalide.")
    if (!identical(as.character(unique(result$canonical_table$sample_key)),
                   names(results)[[i]]) ||
        !identical(as.character(unique(result$canonical_table$sample_id)),
                   as.character(manifest$sample_id[[i]])) ||
        !identical(as.character(unique(result$canonical_table$condition)),
                   as.character(manifest$condition[[i]])) ||
        !identical(as.character(result$analysis_id %||% NA_character_),
                   as.character(manifest$analysis_id[[i]])) ||
        !identical(as.character(result$provenance$analysis_id %||% NA_character_),
                   as.character(manifest$analysis_id[[i]])) ||
        !identical(as.integer(nrow(result$canonical_table)),
                   as.integer(manifest$n_interactions[[i]])) ||
        !identical(as.integer(result$engine$n_cells_input),
                   as.integer(manifest$n_cells_input[[i]])) ||
        !identical(as.integer(result$engine$n_cells_used),
                   as.integer(manifest$n_cells_used[[i]])) ||
        !identical(as.integer(result$engine$n_cells_excluded),
                   as.integer(manifest$n_cells_excluded[[i]])) ||
        !identical(as.integer(result$engine$n_populations_used),
                   as.integer(manifest$n_populations_used[[i]]))) {
      invalid("un resultat n'est pas aligne avec son echantillon.")
    }
  }
  if (.liana_collection_has_forbidden(collection)) {
    invalid("la collection contient un objet ephemere interdit.")
  }
  if (!is.null(seurat_obj)) {
    if (!inherits(seurat_obj, "Seurat")) {
      invalid("l'objet courant doit etre un Seurat.")
    }
    expected_fingerprint <- collection$object_identity$fingerprint
    if (!is.null(expected_fingerprint) &&
        exists("velocity_object_fingerprint", mode = "function") &&
        !identical(expected_fingerprint, velocity_object_fingerprint(seurat_obj))) {
      invalid("l'objet courant ne correspond plus a l'empreinte de la collection.")
    }
    meta <- tryCatch(
      as.data.frame(seurat_obj[[]], stringsAsFactors = FALSE),
      error = function(e) { ts_log_swallow("sc_communication_engine.liana_collection_meta", e); NULL }
    )
    sample_col <- collection$sample_col
    condition_col <- collection$condition_col
    idents_col <- collection$idents_col
    if (!is.data.frame(meta) || is.null(sample_col) || is.null(condition_col) ||
        is.null(idents_col) || !sample_col %in% colnames(meta) ||
        !condition_col %in% colnames(meta) || !idents_col %in% colnames(meta)) {
      invalid("l'objet courant ne porte pas le design de la collection.")
    }
    current_cells <- rownames(meta)
    if (is.null(current_cells)) current_cells <- colnames(seurat_obj)
    current_design <- data.frame(
      cell_id = as.character(current_cells),
      sample_id = trimws(as.character(meta[[sample_col]])),
      condition = trimws(as.character(meta[[condition_col]])),
      identity = trimws(as.character(meta[[idents_col]])),
      stringsAsFactors = FALSE
    )
    if (!identical(current_design, cell_design)) {
      invalid("le design cellulaire courant ne correspond pas a la collection.")
    }
    current_samples <- trimws(as.character(meta[[sample_col]]))
    current_conditions <- trimws(as.character(meta[[condition_col]]))
    current_order <- unique(current_samples)
    current_counts <- vapply(
      current_order,
      function(sample) sum(current_samples == sample),
      integer(1L)
    )
    current_condition <- vapply(
      current_order,
      function(sample) unique(current_conditions[current_samples == sample])[1L],
      character(1L)
    )
    index <- match(manifest$sample_id, current_order)
    if (anyNA(index) || any(current_counts[index] != as.integer(manifest$n_cells)) ||
        any(current_condition[index] != as.character(manifest$condition))) {
      invalid("l'objet courant ne correspond pas au manifeste de la collection.")
    }
  }
  invisible(collection)
}

liana_collection_active <- function(collection, sample_id) {
  assert_liana_collection(collection)
  sample_id <- .liana_validate_scalar_text(sample_id, "sample_id")
  index <- match(sample_id, names(collection$results))
  if (is.na(index)) {
    index <- which(as.character(collection$sample_manifest$sample_id) == sample_id)
  }
  if (length(index) != 1L) {
    .liana_engine_stop(
      "invalid_input",
      sprintf("echantillon LIANA '%s' absent de la collection.", sample_id)
    )
  }
  collection$results[[index]]
}

liana_collection_sample_manifest <- function(collection) {
  assert_liana_collection(collection)
  collection$sample_manifest
}

liana_collection_condition_summary <- function(collection) {
  assert_liana_collection(collection)
  collection$condition_summary
}

build_liana_collection_table <- function(collection) {
  assert_liana_collection(collection)
  tables <- lapply(collection$results, function(result) {
    as.data.frame(result$canonical_table, stringsAsFactors = FALSE)
  })
  output <- do.call(rbind, tables)
  rownames(output) <- NULL
  output
}

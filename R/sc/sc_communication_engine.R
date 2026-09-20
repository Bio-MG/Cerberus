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
    if (is.function(on_progress)) tryCatch(on_progress(msg), error = function(e) NULL)
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
                          error = function(e) NULL)
    on.exit(tryCatch(future::plan(prev_plan), error = function(e) NULL), add = TRUE)
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
    out <- tryCatch(fn(object), error = function(e) NULL)
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
                   error = function(e) NULL)
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

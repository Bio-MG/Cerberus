# =============================================================================
# helper-communication-fixtures.R — fixtures deterministes communication
# (Stage 11, 4D-1)
# =============================================================================
# Auto-source par testthat avant les test-*.R. NB : ce fichier est source
# AVANT helper-source.R (ordre alphabetique : c < s) — si source_project_file()
# n'existe pas encore, on source helper-source.R soi-meme apres avoir remonte
# a la racine du projet (marqueur app.R). Noms prefixes .comm_* pour eviter
# toute collision entre fichiers de test. Sourcing des contrats requis
# (io_helpers fournit %||%) + sc_velocity.R (velocity_object_fingerprint est
# REUTILISE, jamais duplique) + sc_communication.R.
# =============================================================================

if (!exists("source_project_file", envir = globalenv(), mode = "function")) {
  .comm_root <- getwd()
  for (.i in 1:8) {
    if (file.exists(file.path(.comm_root, "app.R"))) break
    .parent <- dirname(.comm_root)
    if (identical(.parent, .comm_root)) break
    .comm_root <- .parent
  }
  sys.source(file.path(.comm_root, "tests", "testthat", "helper-source.R"),
             envir = globalenv())
}

source_project_file("tools/pkg_skeleton/R/io_helpers.R")   # %||%
source_project_file("tools/pkg_skeleton/R/rdata_io.R")     # parse_cellchat_object() accepte .rda
source_project_file("tools/pkg_skeleton/R/state.R")
source_project_file("tools/pkg_skeleton/R/provenance.R")
source_project_file("tools/pkg_skeleton/R/sc_velocity.R")
source_project_file("tools/pkg_skeleton/R/sc_communication.R")
# parse_cellchat_object() DELEGUE l'extraction a .cellchat_engine_extract() :
# une SEULE fonction connait la structure interne de CellChat (regle 3).
source_project_file("tools/pkg_skeleton/R/sc_communication_engine.R")
source_project_file("tools/pkg_skeleton/R/sc_communication_views.R")

# ── Fixture CellChat : table exportee (subsetCommunication-like) ────────────
.comm_cellchat_tab <- function() {
  data.frame(
    source   = c("CD4 T", "B", "CD4 T"),
    target   = c("B", "CD8 T", "CD8 T"),
    ligand   = c("IL7", "CD40", "CCL5"),
    receptor = c("IL7R", "CD40", "CCR5"),
    prob     = c(0.3, 0.5, 0.2),
    pathway  = c("IL7 signaling", "CD40 signaling", "CCL signaling"),
    stringsAsFactors = FALSE
  )
}

# Table avec colonnes d'origine utiles en plus (groupes ligand/receptor).
.comm_cellchat_tab_groups <- function() {
  tab <- .comm_cellchat_tab()
  tab$ligand.group <- c("Cytokine", "TNF", "Chemokine")
  tab$receptor.group <- c("Cytokine receptor", "TNF receptor", "Chemokine receptor")
  tab
}

# ── Fixture CellPhoneDB : means.txt + pvalues.txt (format v2 large) ─────────
.comm_cellphonedb_means <- function() {
  data.frame(
    id_cp_interaction = c("CII-1", "CII-2", "CII-3"),
    interacting_pair = c("IL7|IL7R", "CD40|CD40", "CCL5|CCR5"),
    partner_a = c("IL7", "CD40", "CCL5"),
    partner_b = c("IL7R", "CD40", "CCR5"),
    gene_a = c("IL7", "CD40", "CCL5"),
    gene_b = c("IL7R", "CD40", "CCR5"),
    receptor_a = c(FALSE, FALSE, FALSE),
    receptor_b = c(TRUE, TRUE, TRUE),
    "CD4 T|B" = c(0.3, 0.0, 0.1),
    "B|CD4 T" = c(0.1, 0.5, 0.0),
    "CD8 T|CD8 T" = c(0.0, 0.0, 0.2),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

.comm_cellphonedb_pvalues <- function() {
  data.frame(
    id_cp_interaction = c("CII-1", "CII-2", "CII-3"),
    interacting_pair = c("IL7|IL7R", "CD40|CD40", "CCL5|CCR5"),
    partner_a = c("IL7", "CD40", "CCL5"),
    partner_b = c("IL7R", "CD40", "CCR5"),
    gene_a = c("IL7", "CD40", "CCL5"),
    gene_b = c("IL7R", "CD40", "CCR5"),
    "CD4 T|B" = c(0.01, 0.90, 0.20),
    "B|CD4 T" = c(0.20, 0.03, 0.80),
    "CD8 T|CD8 T" = c(0.70, 0.70, 0.02),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# ── Fixture LIANA : table agregee (CCC 7-8 route (b)) ──────────────────────
# Schema reel d'une sortie liana_aggregate()/rank_aggregate() : identite de
# l'interaction (source/target + complexes), rangs PAR methode
# ({methode}.rank), consensus mean_rank + aggregate_rank (p-value RRA).
# `ligand`/`receptor` sont les colonnes DECOMPLEXIFIEES que LIANA produit en
# plus des `.complex`.
.comm_liana_tab <- function() {
  data.frame(
    source = c("CD4 T", "B", "CD4 T", "NK"),
    target = c("B", "CD8 T", "CD8 T", "B"),
    ligand.complex = c("IL7", "CD40", "CCL5", "GZMB"),
    receptor.complex = c("IL7R", "CD40", "CCR5", "NKG7"),
    ligand = c("IL7", "CD40", "CCL5", "GZMB"),
    receptor = c("IL7R", "CD40", "CCR5", "NKG7"),
    natmi.edge_specificity = c(0.9, 0.7, 0.5, 0.3),
    natmi.rank = c(1, 2, 3, 4),
    cellphonedb.pvalue = c(0.01, 0.03, 0.20, 0.50),
    cellphonedb.rank = c(1, 2, 3, 4),
    mean_rank = c(1.0, 2.0, 3.0, 4.0),
    aggregate_rank = c(0.001, 0.010, 0.200, 0.500),
    stringsAsFactors = FALSE
  )
}

# Variante SANS aucune colonne de consensus : uniquement des rangs PAR
# methode. Prouve que external_consensus vaut FALSE quand la table ne porte
# aucune valeur agregee inter-methodes.
.comm_liana_tab_single_method <- function() {
  tab <- .comm_liana_tab()
  tab[, setdiff(colnames(tab), c("mean_rank", "aggregate_rank"))]
}

# Variante SANS colonnes decomplexifiees : uniquement les `.complex`. Le
# parseur doit alors les reprendre TELLES QUELLES — jamais les decouper.
.comm_liana_tab_complex_only <- function() {
  tab <- .comm_liana_tab()
  tab[, setdiff(colnames(tab), c("ligand", "receptor"))]
}

# Resultat canonique LIANA (rangs) — orchestration miroir du module.
.comm_liana_result <- function(rank_column = "mean_rank",
                               aggregation_mode = "specificity",
                               tab = .comm_liana_tab(),
                               source_files = list(table = "liana_aggr.csv")) {
  parsed <- parse_liana_import(
    tab, rank_column = rank_column, aggregation_mode = aggregation_mode,
    source_file = source_files$table
  )
  .comm_import_and_finalize(
    parsed, source_files = source_files,
    external_consensus = isTRUE(parsed$external_consensus)
  )
}

# Identites Seurat de la colonne choisie (extraites par le module depuis
# obj@meta.data) — le domaine consomme un vecteur, pas un objet Seurat.
.comm_identities <- c("CD4 T", "CD8 T", "B", "NK")

# Objet a dimnames (stub) : seule l'identite (velocity_object_fingerprint)
# est extraite — testabilite hors Shiny, comme les fixtures velocity.
.comm_obj_stub <- function() {
  m <- matrix(0, nrow = 6, ncol = 4,
              dimnames = list(paste0("gene", 1:6),
                              c("c1", "c2", "c3", "c4")))
  m
}

# Table riche pour les vues d'aggregation (Stage 12) : 3 senders x 3
# receivers, pathways partiellement renseignes, auto-interaction, label sans
# correspondance ("Mono").
.comm_cellchat_tab_big <- function() {
  data.frame(
    source   = c("CD4 T", "CD4 T", "CD4 T", "B", "B", "NK", "NK", "NK", "Mono"),
    target   = c("B", "B", "NK", "CD4 T", "NK", "CD4 T", "B", "NK", "B"),
    ligand   = c("IL7", "CCL5", "IL7", "CD40", "TNF", "GZMB", "CCL5", "PTGDS", "S100A8"),
    receptor = c("IL7R", "CCR5", "IL7R", "CD40", "TNFRSF1B", "NKG7", "CCR5", "PTGDR", "TLR4"),
    prob     = c(0.3, 0.5, 0.2, 0.7, 0.4, 0.1, 0.6, 0.8, 0.9),
    p_value  = c(0.01, 0.50, 0.02, 0.30, 0.90, 0.40, 0.05, 0.01, 0.20),
    pathway  = c("IL7 signaling", NA, "IL7 signaling", "CD40 signaling", NA,
                 "Cytotoxicity", "CCL signaling", "PTGDS signaling", "TLR signaling"),
    stringsAsFactors = FALSE
  )
}

# ── Objet CellChat : forme REELLE de net$prob (MESUREE, pas supposee) ───────
# net$prob = array 3D [groupe SOURCE, groupe CIBLE, interaction_name], et
# LR$LRsig (rownames = interaction_name) porte ligand/receptor/pathway_name.
#
# MESURE du 2026-09-15 sur un objet CellChat 2.2.0.9001 reel : dim = 3 x 3 x
# 109, dimnames[[3]] = "CXCL1_ACKR1", "TGFB1_TGFBR1_TGFBR2"... et 0/109 de ces
# noms ne contiennent '|'. La forme [ligand, recepteur, "sender|receiver"]
# historiquement documentee est donc FICTIVE : aucune version de CellChat ne la
# produit. Elle est conservee ci-dessous UNIQUEMENT pour verifier qu'elle est
# refusee (jamais interpretee a tort).
.comm_cellchat_object_real <- function(with_pval = TRUE) {
  src <- c("CD4 T", "B")
  tgt <- c("CD4 T", "B")
  inter <- c("IL7_IL7R", "CCL5_CCR5")
  prob <- array(0, dim = c(2L, 2L, 2L), dimnames = list(src, tgt, inter))
  prob["CD4 T", "B", "IL7_IL7R"]   <- 0.5
  prob["B", "CD4 T", "CCL5_CCR5"]  <- 0.7
  prob["B", "B", "IL7_IL7R"]       <- 0.3
  net <- list(prob = prob)
  if (with_pval) {
    pv <- array(NA_real_, dim = c(2L, 2L, 2L), dimnames = list(src, tgt, inter))
    pv["CD4 T", "B", "IL7_IL7R"]   <- 0.01
    pv["B", "CD4 T", "CCL5_CCR5"]  <- 0.02
    pv["B", "B", "IL7_IL7R"]       <- 0.03
    net$pval <- pv
  }
  lrsig <- data.frame(
    interaction_name = inter,
    pathway_name     = c("IL7 signaling", "CCL signaling"),
    ligand           = c("IL7", "CCL5"),
    receptor         = c("IL7R", "CCR5"),
    row.names        = inter,
    stringsAsFactors = FALSE
  )
  list(net = net, LR = list(LRsig = lrsig))
}

# Forme FICTIVE historique : [ligand, recepteur, "sender|receiver"], sans LR.
# Aucun objet CellChat reel ne la produit — elle doit etre REFUSEE.
.comm_cellchat_object_legacy_shape <- function() {
  lig <- c("IL7", "CCL5")
  rec <- c("IL7R", "CCR5")
  prs <- c("CD4 T|B", "B|CD4 T")
  prob <- array(c(0.5, 0.0, 0.0, 0.3,
                  0.2, 0.4, 0.0, 0.1),
                dim = c(2, 2, 2),
                dimnames = list(lig, rec, prs))
  list(net = list(prob = prob))
}

# ── Resultat canonique riche (vues Stage 12) ────────────────────────────────
.comm_result_big <- function() {
  parsed <- parse_cellchat_import(.comm_cellchat_tab_big(),
                                  source_file = "cellchat_big.csv")
  .comm_import_and_finalize(parsed, source_files = list(table = "cellchat_big.csv"))
}

# ── Orchestration miroir du module (sequence d'import Stage 11) ─────────────
# reproduce EXACTEMENT la sequence du module : harmonisation -> QC ->
# finalisation, avertissements fusionnes via extra_warnings. La route LIANA
# transmet en plus le marqueur de consensus externe (parse_liana_import()$-
# external_consensus), comme le fait le module — jamais recalcule ici.
.comm_import_and_finalize <- function(parsed,
                                      identities = .comm_identities,
                                      identity_column = "cell_type",
                                      source_files = list(table = "import.csv"),
                                      seurat_obj = .comm_obj_stub(),
                                      external_consensus = FALSE) {
  harm <- harmonize_communication_identities(
    parsed$table, identities, identity_column,
    context = "communication import"
  )
  qcr <- communication_import_qc(harm$table)
  warnings_all <- c(parsed$warnings, harm$warnings, qcr$warnings)
  finalize_communication_result(
    canonical_table = qcr$table,
    source_method   = unique(parsed$table$source_method)[1L],
    source_files    = source_files,
    identity_column = identity_column,
    identity_mapping = harm$mapping,
    identity_summary = harm$summary,
    column_mapping  = parsed$column_mapping,
    qc              = qcr$counts,
    n_input_rows    = parsed$n_input_rows,
    seurat_obj      = seurat_obj,
    extra_warnings  = warnings_all,
    external_consensus = external_consensus
  )
}

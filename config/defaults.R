# =============================================================================
# config/defaults.R — Default parameter values (sourced by app.R)
# =============================================================================
# Single source of truth for default values used across modules.
# Modules must read these values; do NOT hardcode them in module code.
# =============================================================================

# --- Parallelism / async ---------------------------------------------------
TS_MIRAI_TIMEOUT_MS        <- 20 * 60 * 1000L   # 20 min generic task ceiling
TS_RCTD_TIMEOUT_MS         <- 40 * 60 * 1000L   # RCTD-specific ceiling
TS_LABEL_TRANSFER_TIMEOUT  <- 45 * 60 * 1000L   # Label Transfer ceiling
TS_SPATIAL_PIPELINE_TIMEOUT_S <-
  (6 * TS_MIRAI_TIMEOUT_MS + max(TS_RCTD_TIMEOUT_MS, TS_LABEL_TRANSFER_TIMEOUT)) / 1000 + 15 * 60
# Plafond du job DRIVE `sc-pipeline-run_auto_pipeline` (4 h). Le pipeline SC est
# PLUS LOURD que le Spatial : la le pipeline enchaîne Normalisation -> PCA ->
# FindNeighbors -> FindClustering -> UMAP -> t-SNE, plus SingleR / FindAllMarkers
# / ORA / correlation / pseudotemps selon les options, et les gardes
# .AUTO_TSNE_MAX_CELLS (30 000) et .MAX_TRAJECTORY_CELLS (100 000) ne
# plafonnent QUE les etapes optionnelles : normalisation, PCA, clustering et UMAP
# ne sont pas bornes. D'ou une valeur DECLAREE, distincte du plafond Spatial, et
# non une reutilisation de TS_MIRAI_TIMEOUT_MS (dimensionne pour des taches
# spatiales isolees, cf. le meme raisonnement que le plafond Milo plus bas).
TS_SC_AUTO_PIPELINE_TIMEOUT_S <- 4 * 60 * 60
TS_SC_ANNOTATION_TIMEOUT_S <- TS_SC_AUTO_PIPELINE_TIMEOUT_S
TS_SC_MARKERS_TIMEOUT_S <- TS_SC_AUTO_PIPELINE_TIMEOUT_S
TS_MIRAI_N_DAEMONS         <- 6L

# --- Single-Cell sketch presets (cells) ------------------------------------
TS_SKETCH_FAST             <- 5000L
TS_SKETCH_LIGHT            <- 10000L
TS_SKETCH_MEDIUM           <- 25000L
TS_SKETCH_STANDARD         <- 50000L
TS_SKETCH_HIGH             <- 100000L

# --- Single-Cell analysis caps ---------------------------------------------
TS_MAX_PER_CLUSTER_MARKERS <- 1000L   # subsample cap for FindAllMarkers
TS_MAX_PER_SAMPLE_CORR     <- 5000L   # subsample cap for correlation
TS_MAX_HEATMAP_CELLS       <- 5000L   # max cells before heatmap aggregation
TS_MAX_HEATMAP_FEATURES    <- 50L
TS_MAX_CORR_FEATURES       <- 50L
TS_MAX_CORR_GENES          <- 200L    # genes in correlation matrix
TS_MAX_CORRELATED_TOP_N    <- 50L     # top correlated genes returned

# --- Spatial ----------------------------------------------------------------
TS_RCTD_MIN_CELLS_PER_TYPE <- 25L     # RCTD minimum per cell type
TS_DECONV_DEFAULT_N_HVG    <- 2000L   # HVG cap before deconvolution
TS_MAX_SVG_HEATMAP         <- 60L     # max SVGs in grid display

# --- Velocity ----------------------------------------------------------------
TS_VELOCITY_MAX_PORTRAIT_CELLS <- 50000L  # phase portrait subsample cap
TS_VELOCITY_MAX_EMBED_CELLS    <- 5000L   # embedding plot subsample cap

# --- Differential abundance design (4E-0) ------------------------------------
# Principe central : les cellules ne sont PAS des replicats biologiques
# (pas de pseudoreplication) — le blocage est hard en dessous du plancher.
TS_DA_MIN_REPLICATES_PER_CONDITION  <- 2L   # blocage en dessous (par condition)
TS_DA_MIN_CELLS_PER_SAMPLE          <- 10L  # avertissement en dessous (echantillon faible)
TS_DA_MIN_IDENTITY_CELLS_PER_SAMPLE <- 5L   # avertissement en dessous (identite sous-representee)
TS_DA_CELLS_IMBALANCE_RATIO         <- 3    # avertissement au-dessus (ratio max/min cellules par echantillon)

# --- Milo (4E-1) --------------------------------------------------------------
# Construction des voisinages + modele DA par voisinage (miloR). La graine
# est enregistree pour la reproductibilite declaree (le graphe kNN exact est
# deterministe ; la graine couvre les eventuels liens/empiriques).
TS_DA_MILO_K                     <- 30L        # k du graphe kNN et des voisinages
TS_DA_MILO_PROP                  <- 0.1        # proportion de voisinages conserves (makeNhoods)
TS_DA_MILO_D                     <- 30L        # dimensionnalite latente (plafonnee a ncol de la reduction)
TS_DA_MILO_REFINEMENT_SCHEME     <- "graph"    # makeNhoods refinement_scheme
TS_DA_MILO_FDR_WEIGHTING         <- "k-distance" # graphSpatialFDR (ponderation k-distance)
TS_DA_MILO_MIN_MEAN              <- 0          # seuil d'expression minimale testNhoods (defaut miloR)
TS_DA_MILO_ROBUST                <- TRUE       # glmQLFit robust (defaut miloR)
TS_DA_MILO_IDENTITY_FRACTION_MIN <- 0.7        # fraction min d'identite pour annoter un voisinage (convention miloR)
TS_DA_MILO_DISPLAY_ALPHA         <- 0.1        # seuil SpatialFDR d'affichage des voisinages significatifs (vues)
TS_DA_MILO_SEED                  <- 14L        # graine enregistree dans la provenance
# Plafond propre au job DA Milo (4E-4). Le calcul tourne desormais dans un
# daemon mirai (run_job(async = TRUE)) : sans plafond, un run pathologique
# bloquerait la session indefiniment. On NE releve PAS le plafond partage
# TS_MIRAI_TIMEOUT_MS (20 min, dimensionne pour les taches spatiales) : on en
# declare un dedie, meme raisonnement que RCTD / Label Transfer
# (R/spatial/spatial_async.R). Valeur : Milo enchaine kNN + makeNhoods +
# calcNhoodDistance + countCells + glmQLFit par voisinage, et le nombre de
# voisinages croit avec le nombre de cellules.
TS_DA_MILO_TIMEOUT_MS            <- 30 * 60 * 1000L  # 30 min

# --- scCODA (4E-2) ------------------------------------------------------------
# DA compositionnelle au niveau ECHANTILLON via l'environnement Python sccoda
# (detection explicite — jamais de repli silencieux). Defauts MCMC = defauts
# scCODA (resultats/burnin/leapfrog/step size) ; la convergence est evaluee
# en pure R : ESS < ESS_FAIL = ECHEC (posteriorite inutilisable) ; ESS <
# ESS_MIN = avertissement (le spike-and-slab deprime structurellement l'ESS du
# HMC tensorflow — calibre empiriquement : ESS ~11 a 20000 tirages sur le
# fixture) ; r_hat/divergences bloquent quand ils sont disponibles (NA = note).
TS_DA_SCCODA_NUM_RESULTS    <- 20000L  # echantillons MCMC conserves (defaut scCODA)
TS_DA_SCCODA_NUM_BURNIN     <- 5000L   # burnin MCMC (defaut scCODA)
TS_DA_SCCODA_NUM_LEAPFROG   <- 10L     # pas de leapfrog HMC (defaut scCODA)
TS_DA_SCCODA_STEP_SIZE      <- 0.01    # step size initial HMC (defaut scCODA)
TS_DA_SCCODA_FDR_TARGET     <- 0.05    # seuil FDR des effets credibles
TS_DA_SCCODA_RHAT_MAX       <- 1.01    # echec de convergence au-dela (NA = note)
TS_DA_SCCODA_ESS_FAIL       <- 10      # echec de convergence en dessous
TS_DA_SCCODA_ESS_MIN        <- 100     # avertissement de convergence en dessous
TS_DA_SCCODA_MAX_DIVERGENCES<- 0       # divergences NUTS tolerees (NA = note)
TS_DA_SCCODA_SEED           <- 15L     # graine tensorflow enregistree

# --- Vues croisées Milo x scCODA (4E-3 / Stage 16) -----------------------------
# La comparaison est DESCRIPTIVE (regles explicites, jamais un score composite
# ni une p-value de consensus). "Signal" Milo = fraction de voisinages
# significatifs de l'identite >= ce plancher (seuil SpatialFDR : reutilise
# TS_DA_MILO_DISPLAY_ALPHA).
TS_DA_CROSS_SIGNIF_FRACTION <- 0.5    # plancher de voisinages significatifs (Milo)

# --- Rarete par population annotee (CCC 9, question 1) ------------------------
# Jalon DESCRIPTIF et MONO-CONDITION : compter les cellules par niveau d'une
# colonne d'identite declaree et qualifier de "rare" celles qui passent sous un
# seuil DECLARE. AUCUNE comparaison entre conditions -> la porte Stage 13
# (assert_da_design_result) ne s'applique pas (motif ecrit dans
# docs/contracts/POPULATION_RARITY_CONTRACT.md).
#
# ATTENTION : il n'y a PAS de seuil de rarete par defaut, et il ne doit pas y
# en avoir. Le seuil est un CHOIX DECLARE de l'utilisateur (meme discipline que
# le mode d'agregation de CCC 7-8) : `compute_population_rarity()` refuse un
# appel sans regle ni seuil (invalid_input). Ce qui est declare ici, ce sont
# uniquement les REGLES AUTORISEES et un PLANCHER DE GARDE.
#
# ⚠️ Ne JAMAIS reutiliser TS_DA_MIN_IDENTITY_CELLS_PER_SAMPLE (plancher de
# testabilite du design DA) comme seuil de rarete : ce serait a la fois un
# defaut implicite interdit ET une confusion semantique entre deux notions
# differentes.
TS_POPULATION_RARITY_RULES <- c("absolute_n_cells", "relative_fraction")
TS_POPULATION_RARITY_MIN_CELLS_TOTAL <- 50L   # refus en dessous (rarefaction illusoire)

# --- Import .rda/.RData (Inspect & Select) -------------------------------------
# Extensions acceptees par les modules d'import pour le mode "Inspecter
# d'abord, importer ensuite" (R/core/rdata_io.R, contrat
# docs/contracts/RDATA_IMPORT_CONTRACT.md). Le seuil ci-dessous ne declenche
# qu'un AVERTISSEMENT (le workspace entier est charge en memoire pour la
# preview) — jamais un blocage.
TS_IMPORT_RDA_EXTENSIONS <- c("rda", "rdata")
TS_IMPORT_RDA_WARN_MB    <- 500

# --- Rapport consolide (4F / Stage 17) -----------------------------------------
# Le rapport est un COMPILATEUR d'etat + de provenance : aucune re-execution
# d'analyse. Les plafonds ci-dessous ne concernent que l'AFFICHAGE (tables
# HTML inline) — les exports du bundle ne sont jamais plafonnes.
TS_REPORT_MAX_TABLE_ROWS       <- 200L   # lignes max par table HTML inline
TS_REPORT_MAX_PROVENANCE_ROWS  <- 500L   # lignes max de la table de provenance inline

# --- Correction pour tests multiples (STAT-Q1) ---------------------------------
# Methodes de correction proposees dans l'interface, pour les tests DE
# (DESeq2/edgeR/limma) ET l'enrichissement de pathways (ORA/GSEA).
# Volontairement SANS "fdr" : dans stats::p.adjust.methods, "fdr" est un ALIAS
# de "BH" — exposer les deux serait redondant et preterait a confusion pour un
# biologiste. Le defaut reste BH => zero changement de comportement sur les
# resultats existants.
# Note : place ici (defaults) et non dans thresholds.R — c'est un jeu de
# valeurs par defaut, pas un seuil scientifique.
TS_PADJ_METHODS        <- c("BH", "BY", "bonferroni", "holm")
TS_PADJ_METHOD_DEFAULT <- "BH"

# --- Theme ggplot partage (PLOT-S1) -------------------------------------------
# Choix de theme + base_size exposes par ts_theme() (R/plotting/theme.R).
# Le defaut base_size = 11 est celui de ggplot2 lui-meme (verifie sur ggplot2
# 4.0.3 : theme_minimal()$text$size == 11) => ts_theme("minimal") rend
# EXACTEMENT comme theme_minimal(). Les sites historiquement codes en 12/13/15
# conservent leur valeur explicitement (zero changement visuel).
TS_THEME_CHOICES      <- c("minimal", "classic", "bw", "void")
TS_THEME_DEFAULT      <- "minimal"
TS_BASE_SIZE_DEFAULT  <- 11

# --- PLOT-S2 : export des graphiques ----------------------------------------
# TS_EXPORT_DPI_DEFAULT = 300 est A LA FOIS le defaut de ggplot2::ggsave() et
# la valeur explicite de 23 des 24 sites existants => defaut neutre, migration
# sans changement de comportement. (Contrairement a base_size en PLOT-S1, il
# n'y a donc PAS de piege de valeur par defaut ici.)
# TS_EXPORT_FORMAT_DEFAULT = NULL signifie "deviner depuis l'extension du
# fichier" — c'est le comportement de ggsave(device = NULL), a ne pas changer.
TS_EXPORT_DPI_CHOICES    <- c(150L, 300L, 600L)
TS_EXPORT_DPI_DEFAULT    <- 300L
TS_EXPORT_FORMATS        <- c("png", "pdf", "svg")
TS_EXPORT_FORMAT_DEFAULT <- NULL

# --- PLOT-S3 : tables de resultats (DT) --------------------------------------
# HISTORIQUE (migration PLOT-S3, 2026-09-11) : il n'y avait PAS de taille de
# page par defaut — 10 (x22), 15 (x15), 8 (x5), 20 (x3) et 6 (x1) coexistaient,
# c'est pourquoi ts_datatable() exigeait `page_length` explicitement.
# JALON DT-EXPORT (2026-09-14, contrat PLOT_DATATABLE_CONTRACT.md §6, option
# B) : les tables de RESULTATS sont normalisees a
# `TS_DT_PAGE_LENGTH_DEFAULT = 15` et gagnent les boutons d'export
# (TS_DT_BUTTONS_DEFAULT = TRUE). `page_length` reste OBLIGATOIRE dans
# ts_datatable() : chaque site ecrit sa valeur explicitement (15L pour les
# tables de resultats, valeur preservee pour les tables d'APERCU — previews
# `dom = "t"/"tip"` et petites tables QC — ni normalisees ni boutonnees).
TS_DT_PAGE_LENGTHS        <- c(6L, 8L, 10L, 15L, 20L)
TS_DT_PAGE_LENGTH_DEFAULT <- 15L
TS_DT_BUTTONS_DEFAULT     <- TRUE

# --- PLOT-S4 : heatmap ComplexHeatmap unifiee --------------------------------
# TS_HEATMAP_MAX_NAMES = 60 est la valeur historique PARTAGEE par les trois
# implementations (plot_heatmap_bulk, build_sc_hierarchical_heatmap) : au-dela,
# les noms de lignes/colonnes sont masques. plot_sample_correlation_heatmap ne
# l'utilise PAS (elle force TRUE/TRUE) -> elle le passe explicitement.
# TS_HEATMAP_DISTANCES : "pearson"/"spearman" sont des distances de correlation
# natives de ComplexHeatmap::Heatmap() (get_dist).
# TS_HEATMAP_METHODS : methodes hclust proposees a l'utilisateur.
TS_HEATMAP_MAX_NAMES     <- 60L
TS_HEATMAP_DISTANCES     <- c("euclidean", "pearson", "spearman")
TS_HEATMAP_METHODS       <- c("complete", "ward.D2", "average")

# --- Moteur CellChat natif (CC-1..CC-5) --------------------------------------
# Aucun seuil « de confort » ici : seuls les parametres que le moteur exige ou
# que l'utilisateur doit DECLARER. Les plafonds RAM / nombre de cellules /
# nombre de clusters sont VOLONTAIREMENT ABSENTS : la proposition
# (V1X_CELLCHAT_ENGINE_PROPOSAL.md §9.3) l'impose — « benchmark d'abord,
# contrat ensuite ». Aucun ETA ni plafond ne sera ajoute avant mesure.
#
# TS_CELLCHAT_NBOOT_DEFAULT : nombre de permutations de `computeCommunProb()`
# (parametre REEL = `nboot` ; le nom `nPerm` n'existe PAS dans la signature —
# piege signale par la proposition §9.4 et verifie sur le code amont).
# C'est la valeur par defaut de CellChat elle-meme ; ce n'est pas un choix
# scientifique de l'application, et il est TRACE en provenance.
# TS_CELLCHAT_SEED_DEFAULT : graine proposee par l'UI. Elle est un PARAMETRE
# DU CALCUL (jamais un `set.seed()` global en session Shiny) : run_cellchat()
# exige une graine explicite, cette constante ne fait que la proposer.
# TS_CELLCHAT_MIN_GROUPS : CellChat ne peut rien inferer avec moins de deux
# populations (deja bloquant dans CELLCHAT_INPUT_CONTRACT.md §3.3) ; re-verifie
# ici car le moteur consomme l'entree sans la reconstruire.
TS_CELLCHAT_NBOOT_DEFAULT <- 100L
TS_CELLCHAT_SEED_DEFAULT  <- 1L
TS_CELLCHAT_MIN_GROUPS    <- 2L

TS_LIANA_METHOD_DEFAULT      <- "natmi"
TS_LIANA_RESOURCE_DEFAULT    <- "Consensus"
TS_LIANA_MIN_CELLS_DEFAULT   <- 5L
TS_LIANA_SEED_DEFAULT        <- 1004L
TS_LIANA_RESOURCE_MD5        <- "b807279afdb1b2f34fa83a3dd51bbcb5"

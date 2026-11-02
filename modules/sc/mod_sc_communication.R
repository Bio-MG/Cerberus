# =============================================================================
# mod_sc_communication.R — Communication cellule-cellule 4D-1 + 4D-2
# (Stage 11 : import et validation / Stage 12 : exploration, vues, exports)
# =============================================================================
# Stage 11 : le module ORCHESTRE uniquement — parseurs par source (CellChat
# table, CellChat objet .rds, CellPhoneDB means/pvalues), harmonisation des
# identites (exact match), QC d'import, statut canonique, provenance et
# exports residuent dans R/sc/sc_communication.R.
# Stage 12 : les vues (DotPlot, heatmap pathways, reseau circulaire,
# centralite) sont des CONSOMMATRICES pures du resultat canonique via
# R/sc/sc_communication_views.R ; les filtres sont des operations d'affichage
# (la table canonique n'est jamais modifiee) et sont capturés dans la
# provenance a l'export. Le type de score et la methode source sont affiches
# sur chaque vue — les scores de sources differentes ne sont jamais compares.
# Aucun calcul CellPhoneDB, aucune inference sur les imports, aucun matching
# flou. Depuis CC-5 le module peut aussi CALCULER (source « Calculer dans
# l'application ») : c'est le moteur R/sc/sc_communication_engine.R qui fait le
# calcul — le module ne fait qu'orchestrer, et le resultat est depose
# EXACTEMENT comme un import (regle des deux voies : meme forme, donc memes
# vues et memes exports, sans qu'aucun n'ait eu a changer).
# =============================================================================
# ⚠️ Coordination B4/freeze (2026-10-30) : ce fichier ne doit contenir AUCUNE
# affectation top-level hors ses 3 fonctions d'orchestration — gel
# test-communication-contract-freeze.R:140-145 (helper :23-35, sans filtre
# point). La carte des sous-panneaux 2F/2L (`.comm_drawer_groups`) vit donc
# EN LOCAL de fonction, en DEUX exemplaires jumelles (rendu tiroir dans
# mod_sc_communication_output_ui, observateurs de saut dans le serveur),
# égalité verrouillée par test-mod-sc-communication-ui.R — même motif que le
# defaut `comm_source` « déclaré ici ET repris par le repli serveur » (:34-36).
# =============================================================================

mod_sc_communication_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "alert alert-light",
        style = "font-size:0.9em;border-left:3px solid #2980B9;",
         i18n$t("Communication cellule-cellule — import de resultats externes (CellChat / CellPhoneDB / LIANA) ou calcul CellChat / LIANA dans l'application. "),
        i18n$t("Aucun score recompose. Les scores de sources differentes ne sont pas comparables.")),

    # Le moteur natif (CC-5) n'est pas un repli mais la voie NORMALE : il est
    # donc propose EN TETE et SELECTIONNE par defaut (decision utilisateur du
    # 2026-09-16). Les quatre sources d'import restent disponibles, dans leur
    # ordre d'origine. ⚠️ Le defaut est DECLARE ici ET repris par le repli
    # serveur (`input$comm_source %||% ...`) : les deux doivent rester
    # d'accord — garde test-sc-communication-engine-ui.R.
    radioButtons(ns("comm_source"), i18n$t("Source des resultats"),
                 choices = setNames(
                   c("cellchat_engine", "cellchat", "cellchat_object", "cellphonedb", "liana", "liana_engine"),
                   c(.tr_plain("Calculer dans l'application (CellChat)"),
                     .tr_plain("CellChat (table exportee)"),
                     .tr_plain("Objet CellChat (.rds, resultats deja calcules)"),
                     .tr_plain("CellPhoneDB (means.txt)"),
                     .tr_plain("LIANA (rangs agreges)"),
                     .tr_plain("Calculer dans l'application (LIANA)"))),
                 selected = "cellchat_engine"),

    conditionalPanel(
      condition = "input.comm_source == 'cellchat'", ns = ns,
      fileInput(ns("comm_cellchat_file"), i18n$t("Table CellChat exportee (CSV/TSV)"),
                accept = c(".csv", ".tsv", ".txt"), width = "100%"),
      div(class = "small text-muted mb-2",
          i18n$t("Colonnes attendues : source, target, ligand, receptor, prob (pathway/pval optionnels)."))
    ),

    conditionalPanel(
      condition = "input.comm_source == 'cellchat_object'", ns = ns,
      fileInput(ns("comm_cellchat_obj"), i18n$t("Objet CellChat sauvegarde (.rds)"),
                accept = c(".rds", ".RDS", ".rda", ".RData"), width = "100%"),
      div(class = "small text-muted mb-2",
          i18n$t("Extraction des resultats deja calcules (net$prob) — aucune methode CellChat n'est relancee. La colonne pathway reste vide pour cette route."))
    ),

    conditionalPanel(
      condition = "input.comm_source == 'cellphonedb'", ns = ns,
      fileInput(ns("comm_cpdb_means"), i18n$t("CellPhoneDB means.txt (requis)"),
                accept = c(".txt", ".tsv", ".csv"), width = "100%"),
      fileInput(ns("comm_cpdb_pvalues"), i18n$t("CellPhoneDB pvalues.txt (optionnel)"),
                accept = c(".txt", ".tsv", ".csv"), width = "100%"),
      div(class = "small text-muted mb-2",
          i18n$t("Format v2 : colonne interacting_pair (ligand|receptor) + une colonne par paire sender|receiver."))
    ),

    # ── CCC 7-8 route (b) : import de RANGS LIANA produits hors de l'app ────
    # Aucune inference : l'app lit une table agregee deja calculee. Deux choix
    # EXPLICITES sont exiges (mode d'agregation + colonne de rang) car ni l'un
    # ni l'autre n'est deductible du contenu du fichier.
    conditionalPanel(
      condition = "input.comm_source == 'liana'", ns = ns,
      fileInput(ns("comm_liana_file"), i18n$t("Table LIANA agregee (CSV/TSV)"),
                accept = c(".csv", ".tsv", ".txt"), width = "100%"),
      radioButtons(ns("comm_liana_mode"), i18n$t("Mode d'agregation LIANA (requis)"),
                   choices = setNames(c("specificity", "magnitude"),
                                      c(.tr_plain("Specificite — l'interaction est-elle specifique de ces types cellulaires ?"),
                                        .tr_plain("Magnitude — l'interaction est-elle abondante ?"))),
                   selected = character(0)),
      selectInput(ns("comm_liana_rank"), i18n$t("Colonne de rang a importer (requis)"),
                  choices = character(0), width = "100%"),
      div(class = "small text-muted mb-2",
          i18n$t("Colonnes de rang : mean_rank, aggregate_rank ou {methode}.rank. Dans LIANA, rang 1 = meilleur (inverse de prob) : aucun score n'est reconstitue, et les modes specificite/magnitude ne sont pas comparables entre eux."))
    ),

    # ── CC-5 : Path B — calculer CellChat DANS l'application ──────────────────
    # Le moteur (R/sc/sc_communication_engine.R) produit EXACTEMENT la meme forme
    # qu'un import (les 12 champs canoniques du contrat Stage 11) et le resultat
    # est depose au MEME endroit : toutes les vues et tous les exports sont donc
    # conserves SANS modification. Regle des deux voies : la moindre divergence
    # de forme entre import et calcul serait un BUG, pas une variante.
    conditionalPanel(
      condition = "input.comm_source == 'cellchat_engine'", ns = ns,
      selectInput(ns("comm_engine_species"), i18n$t("Espece (base ligand-recepteur)"),
                  choices = setNames(c("human", "mouse"),
                                     c(.tr_plain("Humain (CellChatDB.human)"),
                                       .tr_plain("Souris (CellChatDB.mouse)"))),
                  selected = character(0), width = "100%"),
      div(class = "small text-muted mb-2",
          i18n$t("Choix OBLIGATOIRE : la base ligand-recepteur en decoule, elle ne se devine pas a partir des donnees.")),
      numericInput(ns("comm_engine_seed"), i18n$t("Graine"),
                   value = TS_CELLCHAT_SEED_DEFAULT, min = 1, step = 1, width = "100%"),
      numericInput(ns("comm_engine_nboot"), i18n$t("Nombre de permutations"),
                   value = TS_CELLCHAT_NBOOT_DEFAULT, min = 1, step = 1, width = "100%"),
      div(class = "small text-muted mb-2",
          i18n$t("Les p-values viennent d'une permutation : la graine est un parametre du calcul, elle est tracee dans la provenance. Plus de permutations = calcul plus long."))
    ),

    conditionalPanel(
      condition = "input.comm_source == 'liana_engine'", ns = ns,
      div(class = "small text-muted mb-2",
          i18n$t("LIANA natif : les résultats sont calculés par échantillon biologique.")),
      selectInput(ns("comm_liana_sample_col"), i18n$t("Colonne d'echantillon (LIANA)"),
                  choices = character(0), selected = character(0), width = "100%"),
      selectInput(ns("comm_liana_condition_col"), i18n$t("Colonne de condition (LIANA)"),
                  choices = character(0), selected = character(0), width = "100%"),
      div(class = "small text-muted mb-2",
          i18n$t("La colonne d'identites utilise le selecteur commun.")),
      selectInput(ns("comm_liana_method"), i18n$t("Methode LIANA"),
                  choices = setNames(TS_LIANA_METHOD_DEFAULT, TS_LIANA_METHOD_DEFAULT),
                  selected = TS_LIANA_METHOD_DEFAULT, width = "100%"),
      selectInput(ns("comm_liana_resource"), i18n$t("Ressource LIANA"),
                  choices = setNames(TS_LIANA_RESOURCE_DEFAULT, TS_LIANA_RESOURCE_DEFAULT),
                  selected = TS_LIANA_RESOURCE_DEFAULT, width = "100%"),
      numericInput(ns("comm_liana_seed"), i18n$t("Graine LIANA"),
                   value = TS_LIANA_SEED_DEFAULT, min = 1, step = 1, width = "100%"),
      numericInput(ns("comm_liana_min_cells"), i18n$t("Cellules minimum par echantillon"),
                   value = TS_LIANA_MIN_CELLS_DEFAULT, min = 1, step = 1, width = "100%"),
      div(class = "small text-muted mb-2",
          i18n$t("La graine et le minimum de cellules sont des paramètres du calcul et sont conservés dans la provenance.")),
      actionButton(ns("comm_compute_liana"), i18n$t("Lancer le calcul LIANA"),
                   class = "btn-primary w-100", icon = icon("cogs")),
      selectInput(ns("comm_active_sample"), i18n$t("Echantillon actif (LIANA)"),
                  choices = character(0), width = "100%"),
      div(class = "small text-muted mt-1", textOutput(ns("comm_collection_status")))
    ),

    hr(),
    selectInput(ns("comm_identity_column"),
                i18n$t("Colonne d'identites cellulaires (Seurat)"),
                choices = character(0), width = "100%"),
    # CQF-2 (fiche SC_COMM_QUICKFIXES_PROPOSAL_2026-11-02 §3) : indice passif
    # sous le sélecteur — purement informatif, jamais bloquant (aucun validate).
    div(class = "small text-muted mb-2",
        textOutput(ns("comm_identity_hint"))),
    div(class = "small text-muted mb-2",
        i18n$t("Harmonisation par exact match uniquement : aucun renommage de populations, labels sans correspondance listes integralement.")),
    # 3E (O28, spéc §2.17) : pointeur VISIBLE vers la surface propose/valide —
    # le constat 3E est précisément que l'échec de l'exact match restait
    # discret. Changement minimal : une ligne de plus, même style.
    div(class = "small text-muted mb-2",
        i18n$t("Correspondances proposees : sous-onglet « Harmonisation identites » (validation explicite avant application).")),
    # Le bouton d'import n'a pas de sens pour la source « calcul dans
    # l'application » : il lancerait la branche import sur une entree absente.
    conditionalPanel(
      condition = "input.comm_source == 'cellchat' || input.comm_source == 'cellchat_object' || input.comm_source == 'cellphonedb' || input.comm_source == 'liana'", ns = ns,
      actionButton(ns("comm_import"), i18n$t("Importer et valider"),
                   class = "btn-primary w-100", icon = icon("check"))
    ),
    conditionalPanel(
      condition = "input.comm_source == 'cellchat_engine'", ns = ns,
      actionButton(ns("comm_compute"), i18n$t("Lancer le calcul CellChat"),
                   class = "btn-primary w-100", icon = icon("cogs"))
    ),
    div(class = "small text-muted mt-1", textOutput(ns("comm_status"))),
    hr(),
    downloadButton(ns("dl_comm_table"), i18n$t("Exporter table canonique (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_comm_filtered"), i18n$t("Exporter table filtree (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_comm_mapping"), i18n$t("Exporter harmonisation identites (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_comm_summary"), i18n$t("Exporter resume d'import (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_comm_liana_manifest"), i18n$t("Exporter manifeste des echantillons (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_comm_liana_condition_summary"), i18n$t("Exporter resume des conditions (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_comm_centrality"), i18n$t("Exporter centralite (CSV)"), class = "btn-sm btn-info w-100 mt-1"),
    downloadButton(ns("dl_comm_rds"), i18n$t("Exporter resultat valide (RDS)"), class = "btn-sm btn-info w-100 mt-1")
  )
}

mod_sc_communication_output_ui <- function(id) {
  ns <- NS(id)
  # 2F (O28, spéc §2.14) + 2L (O28, spéc §2.23 point 2) : carte des 14
  # sous-panneaux CCC en 4 groupes thématiques — consommée par le tiroir
  # offcanvas (boutons de saut 2L) ; les mêmes groupes structurent le navset
  # via les `value` ASCII explicites posés plus bas. ⚠️ JUMELLE : la même
  # carte est redéclarée EN LOCAL dans mod_sc_communication_server (pas de
  # constante top-level — gel test-communication-contract-freeze.R:140-145) ;
  # toute modification doit être faite AUX DEUX endroits — égalité verrouillée
  # par test-mod-sc-communication-ui.R. Ordre INTERNE du groupe 3 : l'ordre
  # physique du code est conservé (Schema source → Harmonisation identites →
  # QC import) — la surface 3E/B5 (« Harmonisation identites ») n'est pas
  # déplacée.
  .comm_drawer_groups <- list(
    "Exploration des interactions" = list(
      list(value = "ccc_tab_interactions", label = "Table interactions"),
      list(value = "ccc_tab_table_filtres", label = "Table (filtres)"),
      list(value = "ccc_tab_dotplot", label = "DotPlot"),
      list(value = "ccc_tab_heatmap", label = "Heatmap pathways"),
      list(value = "ccc_tab_reseau", label = "Réseau"),
      list(value = "ccc_tab_centralite", label = "Centralité")),
    "Contextes multi-modaux" = list(
      list(value = "ccc_tab_spatial", label = "Contexte spatial"),
      list(value = "ccc_tab_trajectoire", label = "Contexte trajectoire"),
      list(value = "ccc_tab_velocite", label = "Contexte vélocité"),
      list(value = "ccc_tab_perturbation", label = "Perturbation (in silico)")),
    "Traçabilité & qualité" = list(
      list(value = "ccc_tab_schema", label = "Schema source"),
      list(value = "ccc_tab_harmonisation", label = "Harmonisation identites"),
      list(value = "ccc_tab_qc_import", label = "QC import")),
    "Documentation & méthodes" = list(
      list(value = "ccc_tab_limites", label = "Limites"))
  )
  card(
    full_screen = TRUE,
    # 2L (O28, spéc §2.23 point 2 — re-scopé par le re-sweep 1920×1080 du
    # 2026-10-30 : la rétractabilité de la sidebar existe déjà en natif bslib,
    # sonde 420→0→420 px) : pastille ouvrant le tiroir offcanvas secondaire
    # (navigation panneaux + résumé de filtrage) SANS replier la sidebar ni
    # dérouler l'accordéon de filtres 2I. Arbitrage avec 2I : les filtres
    # restent dans l'accordéon (une seule instance), le tiroir ne les duplique
    # pas — il porte le résumé et l'accès direct aux panneaux.
    card_header(
      div(class = "d-flex justify-content-between align-items-center gap-2",
          tags$span(i18n$t("Communication cellule-cellule — resultats importes")),
          tags$button(type = "button",
                      class = "btn btn-sm btn-outline-secondary flex-shrink-0",
                      `data-bs-toggle` = "offcanvas",
                      `data-bs-target` = paste0("#", ns("comm_drawer")),
                      `aria-controls` = ns("comm_drawer"),
                      icon("list-ul"), " ", i18n$t("Navigation & filtres")))
    ),
    # ── Filtres d'affichage (operations d'affichage, captures en provenance
    # a l'export — la table canonique du resultat n'est jamais modifiee) ────
    # 2I (O28, spéc §2.20 point 1) : la zone de filtres STATIQUE (couche L4 du
    # goulot vertical : 276 px mesurés à 1920×1080, re-sweep du 2026-10-30,
    # bas de pile 754 px) devient un accordéon FERMÉ par défaut — ~276 px
    # rendus au boot. Les ids des inputs (comm_f_*) sont INCHANGÉS : les
    # filtres restent des opérations d'affichage, câblées à l'identique côté
    # serveur (comm_filtered), et les inputs restent lisibles panne replié.
    div(class = "m-2",
      accordion(
        id = ns("comm_filters_accordion"),
        open = FALSE,
        accordion_panel(
          title = tagList(icon("filter"), " ", i18n$t("Filtres d'interaction")),
          value = "comm_filters_panel",
          fluidRow(
            column(3, numericInput(ns("comm_f_score_min"), i18n$t("Score minimum (importé)"),
                                   value = NA, min = 0, width = "100%")),
            column(3, numericInput(ns("comm_f_pmax"), i18n$t("P-value maximum (importée)"),
                                   value = NA, min = 0, max = 1, width = "100%")),
            column(6, selectizeInput(ns("comm_f_pathways"), i18n$t("Pathways"),
                                     choices = character(0), multiple = TRUE,
                                     options = list(placeholder = .tr_plain("Tous"))))),
          fluidRow(
            column(3, numericInput(ns("comm_f_rank_max"), i18n$t("Rang maximum (importé)"),
                                   value = NA, min = 0, width = "100%"))),
          fluidRow(
            column(4, selectizeInput(ns("comm_f_senders"), i18n$t("Senders"),
                                     choices = character(0), multiple = TRUE,
                                     options = list(placeholder = .tr_plain("Tous")))),
            column(4, selectizeInput(ns("comm_f_receivers"), i18n$t("Receivers"),
                                     choices = character(0), multiple = TRUE,
                                     options = list(placeholder = .tr_plain("Tous")))),
            column(4, checkboxInput(ns("comm_f_include_self"),
                                    i18n$t("Inclure les auto-interactions"), value = TRUE))),
          div(class = "small text-muted", textOutput(ns("comm_filter_summary")))
        )
      )
    ),
    # ── 2F (O28, spéc §2.14) : les 14 sous-onglets consécutifs (wrap 3 lignes
    # à 1920×1080, re-sweep du 2026-10-30 — couche L5 du goulot 2I) sont
    # regroupés en 4 nav_menu thématiques (.comm_drawer_groups, une seule
    # source de vérité). Les contenus des panneaux sont INCHANGÉS (surfaces
    # 3E/B5 « Harmonisation identites » et 2G « Limites » respectées verbatim) ;
    # les `value` ASCII explicites sont consommés par le tiroir 2L.
    navset_tab(
      id = ns("comm_tabs"),
      nav_menu(i18n$t("Exploration des interactions"),
        nav_panel(i18n$t("Table interactions"), value = "ccc_tab_interactions",
                  DT::dataTableOutput(ns("comm_table"), height = "520px")),
        nav_panel(i18n$t("Table (filtres)"), value = "ccc_tab_table_filtres",
                  DT::dataTableOutput(ns("comm_table_filtered"), height = "520px")),
        nav_panel(i18n$t("DotPlot"), value = "ccc_tab_dotplot",
                plotOutput(ns("comm_dotplot"), height = "480px"),
                fluidRow(
                  column(6, downloadButton(ns("dl_comm_dotplot_png"), i18n$t("PNG"), class = "btn-sm btn-outline-secondary")),
                  column(6, downloadButton(ns("dl_comm_dotplot_pdf"), i18n$t("PDF"), class = "btn-sm btn-outline-secondary")))),
      nav_panel(i18n$t("Heatmap pathways"), value = "ccc_tab_heatmap",
                plotOutput(ns("comm_heatmap"), height = "480px"),
                fluidRow(
                  column(6, downloadButton(ns("dl_comm_heatmap_png"), i18n$t("PNG"), class = "btn-sm btn-outline-secondary")),
                  column(6, downloadButton(ns("dl_comm_heatmap_pdf"), i18n$t("PDF"), class = "btn-sm btn-outline-secondary")))),
      nav_panel(i18n$t("Réseau"), value = "ccc_tab_reseau",
                plotOutput(ns("comm_circle"), height = "520px"),
                fluidRow(
                  column(6, downloadButton(ns("dl_comm_circle_png"), i18n$t("PNG"), class = "btn-sm btn-outline-secondary")),
                  column(6, downloadButton(ns("dl_comm_circle_pdf"), i18n$t("PDF"), class = "btn-sm btn-outline-secondary")))),
      nav_panel(i18n$t("Centralité"), value = "ccc_tab_centralite",
                div(class = "small text-muted m-2",
                    i18n$t("Centralité dérivée du réseau, descriptive — ce n'est pas un contrôle biologique.")),
                DT::dataTableOutput(ns("comm_centrality"), height = "420px"))
      ),
      nav_menu(i18n$t("Contextes multi-modaux"),
      # ── Contextes V1.x (roadmap CCC avancée : contrainte/ordre/contexte —
      # consommateurs purs, aucun recalcul ; docs/ROADMAP_CCC_ADVANCED.md) ──
      nav_panel(i18n$t("Contexte spatial"), value = "ccc_tab_spatial",
                mod_sc_communication_spatial_ui(ns("spatial"))),
      nav_panel(i18n$t("Contexte trajectoire"), value = "ccc_tab_trajectoire",
                mod_sc_communication_trajectory_ui(ns("trajectory"))),
      nav_panel(i18n$t("Contexte vélocité"), value = "ccc_tab_velocite",
                mod_sc_communication_velocity_ui(ns("velocity_ctx"))),
      nav_panel(i18n$t("Perturbation (in silico)"), value = "ccc_tab_perturbation",
                mod_sc_communication_perturbation_ui(ns("perturbation")))
      ),
      nav_menu(i18n$t("Traçabilité & qualité"),
      nav_panel(i18n$t("Schema source"), value = "ccc_tab_schema",
                DT::dataTableOutput(ns("comm_schema"), height = "380px"),
                div(class = "small text-muted mt-2", textOutput(ns("comm_schema_note")))),
      nav_panel(i18n$t("Harmonisation identites"), value = "ccc_tab_harmonisation",
                # ── 3E (O28, spéc §2.17) : surface de validation propose/valide ──
                # DT éditable pré-rempli avec les correspondances exactes et
                # suggérées (preuve par ligne : rule/distance/status/detail) ;
                # l'utilisateur édite ou vide « identite validee », puis
                # applique AVEC confirmation (modal). Rien n'est appliqué
                # sans cette validation explicite — la voie exact match
                # historique reste en vigueur tant que rien n'est validé.
                # (Région disjointe de l'onglet « Limites » — 3D, réservé B2.)
                div(class = "small text-muted m-2",
                    textOutput(ns("comm_mapping_hint"))),
                fluidRow(
                  column(4, actionButton(ns("comm_propose_identities"),
                                         i18n$t("Proposer des correspondances"),
                                         class = "btn-info btn-sm w-100",
                                         icon = icon("magic"))),
                  column(4, actionButton(ns("comm_apply_identities"),
                                         i18n$t("Appliquer le mapping valide"),
                                         class = "btn-primary btn-sm w-100",
                                         icon = icon("check")))
                ),
                br(),
                DT::dataTableOutput(ns("comm_proposals"), height = "300px"),
                hr(),
                DT::dataTableOutput(ns("comm_mapping"), height = "380px")),
      nav_panel(i18n$t("QC import"), value = "ccc_tab_qc_import",
                uiOutput(ns("comm_qc")))
      ),
      nav_menu(i18n$t("Documentation & méthodes"),
      nav_panel(i18n$t("Limites"), value = "ccc_tab_limites",
                # 2G (O28) : les 5 bannières `alert-*` empilées (constat 2G,
                # packet round-7 : 1 warning, 2 info, 2 light) sont regroupées en
                # UNE carte synthétique à puces thématiques (spéc §2.15).
                # ⚠️ Coordination 3D (batch B2, 2026-10-31) : le TEXTE de la 5ᵉ
                # puce (objet RDS CellChat) ÉTAIT la bannière partagée avec le
                # constat 3D — B3 l'avait reproduite verbatim ; B2 corrige
                # désormais le drift UI/code : le moteur résout le mapping
                # ligand-receptor → pathway par appariement EXACT sur
                # @LR$LRsig (sc_communication_engine.R:260-268) et remonte un
                # engine_failure explicite quand @LR$LRsig est absent
                # (sc_communication_engine.R:238-243). L'ancien texte (« …n'est
                # pas reconstitue ») contredisait le moteur — verrou test
                # test-mod-sc-communication-ui.R mis à jour en conséquence.
                # La hiérarchie de sévérité d'origine est conservée via la
                # couleur des puces (warning > info > light).
                card(
                  card_header(i18n$t("Points d'attention — lecture des résultats")),
                  card_body(
                    tags$ul(class = "list-unstyled m-0",
                            tags$li(class = "mb-2",
                                    tags$span(class = "badge bg-warning text-dark me-2",
                                              "⚠️ ", i18n$t("Validité technique vs biologique")),
                                    i18n$t("Validite technique uniquement : un import valide n'implique aucune validite biologique.")),
                            tags$li(class = "mb-2",
                                    tags$span(class = "badge bg-info text-dark me-2",
                                              "⚖️ ", i18n$t("Comparabilité des scores")),
                                    i18n$t("Les scores ne sont pas compares entre sources differentes (CellChat vs CellPhoneDB).")),
                            tags$li(class = "mb-2",
                                    tags$span(class = "badge bg-info text-dark me-2",
                                              "🔗 ", i18n$t("Causalité")),
                                    i18n$t("Une association ligand-receptor n'etablit aucune causalite.")),
                            tags$li(class = "mb-2",
                                    tags$span(class = "badge bg-secondary text-white me-2",
                                              "🏷️ ", i18n$t("Labels cellulaires")),
                                    i18n$t("Les labels sans correspondance exacte dans la colonne choisie sont conserves tels quels et listes — jamais renommes.")),
                            tags$li(class = "mb-0",
                                    tags$span(class = "badge bg-secondary text-white me-2",
                                              "🧬 ", i18n$t("Objets RDS CellChat")),
                                    # 3D (O28, spéc §2.16) : texte aligné sur le moteur —
                                    # appariement EXACT (jamais « non reconstitué ») :
                                    i18n$t("Objet CellChat (.rds) : extraction de resultats deja calcules uniquement ; les paires ligand-receptor sont appariées EXACTEMENT sur @LR$LRsig (ligand, receptor, pathway_name) — aucune reconstruction heuristique ; un @LR$LRsig absent est remonté comme « engine_failure » explicite."))))))
      ),
    ),
    # ── 2L (O28, spéc §2.23 point 2) : tiroir offcanvas secondaire (droite) ──
    # Re-scope du re-sweep 1920×1080 (2026-10-30) : la rétractabilité sidebar
    # est déjà native (bslib collapse-toggle, sonde 420→0→420 px) — ce que le
    # constat 2L demande EN PLUS est l'accès aux panneaux SANS replier le
    # workflow. Tiroir Bootstrap 5 natif (.offcanvas.offcanvas-end, déclenché
    # par la pastille de l'en-tête de carte) : résumé de filtrage vivant +
    # sauts directs vers les 14 panneaux groupés comme en 2F. Les filtres eux-
    # mêmes ne sont PAS dupliqués (instance unique dans l'accordéon 2I).
    tags$div(
      id = ns("comm_drawer"), class = "offcanvas offcanvas-end", tabindex = "-1",
      `aria-labelledby` = ns("comm_drawer_title"),
      tags$div(class = "offcanvas-header",
               h5(id = ns("comm_drawer_title"), class = "offcanvas-title",
                  icon("list-ul"), " ", i18n$t("Navigation CCC")),
               tags$button(type = "button", class = "btn-close",
                           `data-bs-dismiss` = "offcanvas", `aria-label` = "Close")),
      tags$div(class = "offcanvas-body",
        h6(i18n$t("Filtres d'interaction")),
        div(class = "small text-muted", textOutput(ns("comm_filter_summary_drawer"))),
        hr(),
        lapply(names(.comm_drawer_groups), function(grp) {
          tags$div(class = "mb-3",
            h6(i18n$t(grp)),
            lapply(.comm_drawer_groups[[grp]], function(pn) {
              actionButton(ns(paste0("comm_jump_", pn$value)),
                           label = i18n$t(pn$label),
                           class = "btn btn-link btn-sm text-start p-0 d-block")
            }))
        })
      )
    )
  )
}

mod_sc_communication_server <- function(id, global_data, shared_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    # Passe de fix frontière (2026-11-01) — liaison C15 : le serveur appelle
    # ns() (notification comm_apply_confirm) ; sans cette liaison, l'erreur
    # « impossible de trouver la fonction "ns" » ne levait QU'À L'AFFICHAGE
    # (garde statique test-release-hardening.R C15, héritage round 7).
    ns <- session$ns
    # ── i18n proxy ──────────────────────────────────────────────────────────
    .tr <- function(key) {
      tr <- isolate(global_data$i18n)
      if (is.null(tr)) return(key)
      tryCatch(.strip_i18n_html(tr$t(key)), error = function(e) key)
    }

    # 2L (O28, spéc §2.23 point 2) : carte des sous-panneaux pour les sauts du
    # tiroir offcanvas. ⚠️ JUMELLE de la carte déclarée en local dans
    # mod_sc_communication_output_ui (pas de constante top-level — gel
    # test-communication-contract-freeze.R:140-145) ; toute modification doit
    # être faite AUX DEUX endroits — égalité verrouillée par
    # test-mod-sc-communication-ui.R.
    .comm_drawer_groups <- list(
      "Exploration des interactions" = list(
        list(value = "ccc_tab_interactions", label = "Table interactions"),
        list(value = "ccc_tab_table_filtres", label = "Table (filtres)"),
        list(value = "ccc_tab_dotplot", label = "DotPlot"),
        list(value = "ccc_tab_heatmap", label = "Heatmap pathways"),
        list(value = "ccc_tab_reseau", label = "Réseau"),
        list(value = "ccc_tab_centralite", label = "Centralité")),
      "Contextes multi-modaux" = list(
        list(value = "ccc_tab_spatial", label = "Contexte spatial"),
        list(value = "ccc_tab_trajectoire", label = "Contexte trajectoire"),
        list(value = "ccc_tab_velocite", label = "Contexte vélocité"),
        list(value = "ccc_tab_perturbation", label = "Perturbation (in silico)")),
      "Traçabilité & qualité" = list(
        list(value = "ccc_tab_schema", label = "Schema source"),
        list(value = "ccc_tab_harmonisation", label = "Harmonisation identites"),
        list(value = "ccc_tab_qc_import", label = "QC import")),
      "Documentation & méthodes" = list(
        list(value = "ccc_tab_limites", label = "Limites"))
    )

    # Libelle francais d'un etat de validite (fallback = code d'etat).
    .status_label <- function(st) {
      lab <- communication_status_labels()
      if (is.character(st) && length(st) == 1L && st %in% names(lab)) lab[[st]] else st
    }

    # Plot d'erreur standardise (message de validation exploitable).
    .error_plot <- function(e) {
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = conditionMessage(e), size = 5, colour = "red") +
        ggplot2::theme_void()
    }

    comm_state <- reactiveValues(
      result = NULL,
      object_fingerprint = NULL,
      collection = NULL,
      active_sample = NULL,
      context_cells = NULL
    )
    comm_status_rv <- reactiveVal(.tr("En attente d'import de resultats de communication..."))

    output$comm_status <- renderText({ comm_status_rv() })

    output$comm_collection_status <- renderText({
      collection <- comm_state$collection
      if (is.null(collection)) return(.tr("Aucune collection LIANA."))
      manifest <- liana_collection_sample_manifest(collection)
      conditions <- liana_collection_condition_summary(collection)
      .t_fmt(
        .tr("Collection LIANA : statut {status} — {n} echantillon(s), {c} condition(s)."),
        status = as.character(collection$status %||% NA_character_),
        n = nrow(manifest), c = nrow(conditions)
      )
    })

    # ── Lecture CSV/TSV explicite (check.names = FALSE : les colonnes de
    # paires CellPhoneDB "A|B" ne doivent jamais etre mangles en "A.B") ────
    .read_table_auto <- function(path) {
      ext <- tolower(tools::file_ext(path))
      read_fn <- switch(ext,
        "csv" = function(p) utils::read.csv(p, check.names = FALSE, stringsAsFactors = FALSE, comment.char = ""),
        function(p) utils::read.delim(p, check.names = FALSE, stringsAsFactors = FALSE, comment.char = "")
      )
      tryCatch(read_fn(path), error = function(e) {
        stop(sprintf(
          "Lecture du fichier impossible (%s) : %s",
          basename(path), conditionMessage(e)
        ), call. = FALSE)
      })
    }

    # Garde de peremption avant tout rendu/export (etat derive du contrat).
    .check_fingerprint <- function() {
      req(global_data$sc_obj)
      req(comm_state$object_fingerprint)
      current_fp <- velocity_object_fingerprint(global_data$sc_obj)
      if (!identical(comm_state$object_fingerprint, current_fp)) {
        stale_msg <- .status_label("stale_against_current_seurat_object")
        showNotification(stale_msg, type = "error", duration = 8)
        shiny::validate(shiny::need(FALSE, stale_msg))
      }
      invisible(TRUE)
    }

    # ── Choix de la colonne d'identites, synchronise avec l'objet Seurat ───
    # UN SEUL observateur par declencheur (garde anti-duplication) : reset du
    # resultat (aucun import ne survit a un changement d'objet — resultat
    # perime par construction) + resynchronisation des choix de colonne.
    observeEvent(global_data$sc_obj, {
      .clear_result_state()
      comm_status_rv(.tr("Objet Seurat modifie : reimportez la table de communication."))
      updateSelectInput(session, "comm_identity_column", choices = character(0),
                        selected = character(0))
      updateSelectInput(session, "comm_liana_sample_col", choices = character(0),
                        selected = character(0))
      updateSelectInput(session, "comm_liana_condition_col", choices = character(0),
                        selected = character(0))
      obj <- global_data$sc_obj
      if (is.null(obj)) return()
      meta_cols <- colnames(obj@meta.data)
      # CQF-2 (fiche SC_COMM_QUICKFIXES_PROPOSAL_2026-11-02 §3) : choix GROUPÉS
      # (optgroups natifs) — annotation SingleR EN TÊTE, puis seurat_clusters,
      # puis le reste. AUCUNE colonne retirée (principe LIANA : proposées,
      # jamais imposées) ; le défaut reste seurat_clusters (zéro changement de
      # choix). Motif d'annotation : point unique sc_communication_input.R.
      # Libellés via .tr() en variables d'appel : aucune valeur de choices
      # n'est un appel traduit (convention C13).
      updateSelectInput(session, "comm_identity_column",
                        choices = .comm_identity_choice_groups(
                          meta_cols,
                          label_annot = .tr("Annotation (SingleR)"),
                          label_clusters = .tr("Clusters"),
                          label_rest = .tr("Autres colonnes")),
                        selected = if ("seurat_clusters" %in% meta_cols) "seurat_clusters" else meta_cols[1])
      updateSelectInput(session, "comm_liana_sample_col", choices = meta_cols,
                        selected = character(0))
      updateSelectInput(session, "comm_liana_condition_col", choices = meta_cols,
                        selected = character(0))
    }, ignoreInit = TRUE)

    # ── CQF-2 : indice passif sous le sélecteur d'identités ─────────────────
    # Si la colonne sélectionnée est numérique ou préfixée cluster, les paires
    # CellChat restent numérotées : pointer vers l'annotation SingleR. Jamais
    # bloquant (aucun validate/need — purement informatif) ; chaîne vide sinon.
    output$comm_identity_hint <- renderText({
      col <- input$comm_identity_column
      obj <- global_data$sc_obj
      if (is.null(obj) || is.null(col) || length(col) != 1L || !nzchar(col)) {
        return("")
      }
      meta <- tryCatch(obj@meta.data, error = function(e) NULL)
      if (is.null(meta) || !col %in% colnames(meta)) return("")
      if (.comm_identity_is_cluster_like(col, is.numeric(meta[[col]]))) {
        .tr(paste0(
          "Colonne de clusters \u2014 les paires seront num\u00e9rot\u00e9es. ",
          "Lancez l'annotation (SingleR) pour des types cellulaires nomm\u00e9s."))
      } else ""
    })

    # ── Colonnes de rang LIANA : proposees depuis l'en-tete du fichier ─────
    # La liste est PROPOSEE, jamais imposee : comme le mode d'agregation, le
    # choix reste explicite (aucun element preselectionne). On met en avant les
    # colonnes qui ressemblent a des rangs ; si aucune n'est reconnue, on
    # propose TOUTES les colonnes — le parseur diagnostiquera un choix
    # inexploitable plutot que d'interdire l'import.
    observeEvent(input$comm_liana_file, {
      f <- input$comm_liana_file
      tab <- if (is.null(f)) NULL else
        tryCatch(.read_table_auto(f$datapath), error = function(e) NULL)
      nms <- if (is.null(tab)) character(0) else colnames(tab)
      rank_like <- grep("(^mean_rank$)|(^aggregate_rank$)|(\\.rank$)",
                        nms, value = TRUE, ignore.case = TRUE)
      updateSelectInput(session, "comm_liana_rank",
                        choices = if (length(rank_like)) rank_like else nms,
                        selected = character(0))
    }, ignoreInit = TRUE)

    .view_object <- function() {
      if (is.null(comm_state$collection)) global_data$sc_obj else NULL
    }

    .liana_ui_stop <- function(state, message) {
      stop(errorCondition(
        paste0("Moteur LIANA — ", message),
        class = c("liana_engine_error", "error", "condition"),
        state = state
      ))
    }

    .import_ui_stop <- function(state, message) {
      stop(errorCondition(
        paste0("Import communication — ", message),
        class = c("communication_import_error", "error", "condition"),
        state = state
      ))
    }

    .clear_result_state <- function() {
      comm_state$result <- NULL
      comm_state$object_fingerprint <- NULL
      comm_state$collection <- NULL
      comm_state$active_sample <- NULL
      comm_state$context_cells <- NULL
      shared_rv$communication_result <- NULL
      shared_rv$communication_collection <- NULL
      shared_rv$active_communication_sample <- NULL
      updateNumericInput(session, "comm_f_score_min", value = NA)
      updateNumericInput(session, "comm_f_pmax", value = NA)
      updateNumericInput(session, "comm_f_rank_max", value = NA)
      updateSelectizeInput(session, "comm_f_pathways", choices = character(0),
                           selected = character(0))
      updateSelectizeInput(session, "comm_f_senders", choices = character(0),
                           selected = character(0))
      updateSelectizeInput(session, "comm_f_receivers", choices = character(0),
                           selected = character(0))
      updateSelectInput(session, "comm_active_sample", choices = character(0),
                        selected = character(0))
      # 3E (O28) : aucune proposition de mapping ne survit a un reset d'etat
      # (elles ne seraient plus a jour face au resultat courant).
      prop_rv(NULL)
      .prop_apply_pending_rv(NULL)
      invisible(NULL)
    }

    # ── Filtres reactifs (operation d'affichage uniquement) ────────────────
    .num_filter <- function(x) {
      if (is.null(x) || length(x) != 1L || is.na(x)) NA_real_ else as.numeric(x)
    }
    comm_filtered <- reactive({
      req(comm_state$result)
      communication_apply_filters(comm_state$result, list(
        score_min    = .num_filter(input$comm_f_score_min),
        rank_max     = .num_filter(input$comm_f_rank_max),
        p_value_max  = .num_filter(input$comm_f_pmax),
        pathways     = input$comm_f_pathways %||% character(0),
        senders      = input$comm_f_senders %||% character(0),
        receivers    = input$comm_f_receivers %||% character(0),
        include_self = isTRUE(input$comm_f_include_self)
      ))
    })

    # Résumé de filtrage : UNE source réactive, DEUX sorties (ligne sous les
    # filtres 2I + tiroir offcanvas 2L) — même texte, aucune divergence.
    .filter_summary_txt <- reactive({
      req(comm_state$result)
      fs <- comm_filtered()$summary
      sprintf(
        .tr("%d interaction(s) affichée(s) sur %d — retirées : score %d, rang %d, p-value %d, pathway %d, sender %d, receiver %d, auto %d"),
        fs$n_after, fs$n_before, fs$dropped_score, fs$dropped_rank,
        fs$dropped_p_value, fs$dropped_pathway, fs$dropped_sender,
        fs$dropped_receiver, fs$dropped_self
      )
    })

    output$comm_filter_summary <- renderText({ .filter_summary_txt() })
    output$comm_filter_summary_drawer <- renderText({ .filter_summary_txt() })

    # ── 3F (O28, spéc §2.18 point 4) : normalisation contextuelle des filtres
    # score / rang selon la source détectée. Un rang n'est pas un score
    # (communication_apply_filters REFUSE score_min + rank_max simultanés) :
    # l'intitulé et les bornes du filtre suivent donc la colonne qui EXISTE
    # dans la table canonique de la source — jamais un label générique qui
    # invite à saisir un rang dans le filtre score (refus moteur à la clé).
    # Bornes calculées SUR la table importée (aucune borne fabriquée) ; les
    # valeurs saisies ne sont jamais écrasées (seuls label/min/max bougent).
    observeEvent(comm_state$result, {
      r <- comm_state$result
      if (is.null(r)) {
        updateNumericInput(session, "comm_f_score_min",
                           label = .tr("Score minimum (importé)"), min = 0)
        updateNumericInput(session, "comm_f_rank_max",
                           label = .tr("Rang maximum (importé)"), min = 0)
        return()
      }
      src <- as.character(r$source_method %||% NA_character_)
      t <- tryCatch(r$canonical_table, error = function(e) NULL)
      score_max <- if (!is.null(t) && "score" %in% colnames(t) && any(!is.na(t$score)))
        suppressWarnings(max(t$score, na.rm = TRUE)) else NA_real_
      rank_max <- if (!is.null(t) && "rank" %in% colnames(t) && any(!is.na(t$rank)))
        suppressWarnings(max(t$rank, na.rm = TRUE)) else NA_real_
      if (identical(src, "liana")) {
        updateNumericInput(session, "comm_f_score_min",
                           label = .tr("Score minimum — sans objet pour LIANA (un rang n'est pas un score : utiliser Rang maximum)"),
                           min = 0)
        if (!is.na(rank_max)) {
          updateNumericInput(session, "comm_f_rank_max",
                             label = .tr("Rang agrégé [1 - N] (1 = plus significatif)"),
                             min = 1, max = rank_max)
        }
      } else if (identical(src, "cellphonedb")) {
        updateNumericInput(session, "comm_f_score_min",
                           label = .tr("Moyenne d'expression [0.0 - Max]"),
                           min = 0, max = if (!is.na(score_max)) score_max else NA)
        updateNumericInput(session, "comm_f_rank_max",
                           label = .tr("Rang maximum — sans objet pour CellPhoneDB (utiliser le score)"),
                           min = 0)
      } else {
        # cellchat (import, objet ou moteur) et défaut : probabilité [0.0 - 1.0].
        updateNumericInput(session, "comm_f_score_min",
                           label = .tr("Probabilité de communication [0.0 - 1.0]"),
                           min = 0, max = 1)
        updateNumericInput(session, "comm_f_rank_max",
                           label = .tr("Rang maximum — sans objet pour CellChat (utiliser le score)"),
                           min = 0)
      }
    })

    # ── 2L (O28, spéc §2.23 point 2) : saut direct vers un sous-panneau CCC
    # depuis le tiroir offcanvas. nav_select vise les `value` ASCII posés par
    # 2F (indépendants de la traduction) ; le tiroir RESTE OUVERT après le saut
    # (accès aux panneaux sans replier le workflow — verdict re-sweep).
    lapply(.comm_drawer_groups, function(grp) {
      lapply(grp, function(pn) {
        observeEvent(input[[paste0("comm_jump_", pn$value)]], {
          nav_select("comm_tabs", selected = pn$value)
        })
      })
    })

    # Depot d'un resultat canonique : IDENTIQUE pour les deux voies (import =
    # Path A, calcul = Path B). Factorise parce que la regle des deux voies
    # exige que les deux chemins deposent le resultat au MEME endroit — sinon
    # elles divergeraient, et une divergence est un bug.
    .store_result <- function(canonical, obj, append_provenance = TRUE,
                              collection = NULL, active_sample = NULL) {
      context_cells <- NULL
      if (!is.null(collection) && length(active_sample) == 1L &&
          !is.na(active_sample) && nzchar(as.character(active_sample))) {
        manifest <- liana_collection_sample_manifest(collection)
        sample_index <- which(as.character(manifest$sample_key) ==
                              as.character(active_sample))
        if (length(sample_index) == 1L) {
          sample_id <- as.character(manifest$sample_id[[sample_index]])
          sample_col <- as.character(collection$sample_col)
          meta <- obj@meta.data
          if (length(sample_col) == 1L && !is.na(sample_col) &&
              sample_col %in% colnames(meta)) {
            context_cells <- rownames(meta)[
              trimws(as.character(meta[[sample_col]])) == sample_id
            ]
          }
          used_cells <- canonical$engine$cells_used_ids
          if (length(used_cells)) context_cells <- as.character(used_cells)
        }
      }
      if (!is.null(collection) && length(active_sample) == 1L &&
          !is.na(active_sample) && nzchar(as.character(active_sample))) {
        collection$active_sample_key <- as.character(active_sample)
      }
      comm_state$result <- canonical
      comm_state$object_fingerprint <- velocity_object_fingerprint(obj)
      comm_state$collection <- collection
      comm_state$active_sample <- active_sample
      comm_state$context_cells <- context_cells
      shared_rv$communication_result <- canonical
      shared_rv$communication_collection <- collection
      shared_rv$active_communication_sample <- active_sample
      if (isTRUE(append_provenance)) {
        provenance_append(shared_rv, canonical$provenance)
      }
      updateNumericInput(session, "comm_f_score_min", value = NA)
      updateNumericInput(session, "comm_f_pmax", value = NA)
      updateNumericInput(session, "comm_f_rank_max", value = NA)
      t0 <- canonical$canonical_table
      sender_nodes <- ifelse(!is.na(t0$sender_mapped), t0$sender_mapped, t0$sender)
      receiver_nodes <- ifelse(!is.na(t0$receiver_mapped), t0$receiver_mapped, t0$receiver)
      updateSelectizeInput(session, "comm_f_pathways",
                           choices = sort(unique(stats::na.omit(t0$pathway))),
                           selected = character(0))
      updateSelectizeInput(session, "comm_f_senders",
                           choices = sort(unique(sender_nodes)),
                           selected = character(0))
      updateSelectizeInput(session, "comm_f_receivers",
                           choices = sort(unique(receiver_nodes)),
                           selected = character(0))
      # 3E (O28) : un nouveau depot invalide les propositions en attente.
      prop_rv(NULL)
      .prop_apply_pending_rv(NULL)
    }

    # ── Import + validation (orchestration uniquement) ─────────────────────
    observeEvent(input$comm_import, {
      req(global_data$sc_obj)
      tryCatch({
        src <- input$comm_source %||% "cellchat_engine"
        if (identical(src, "cellchat_engine") || identical(src, "liana_engine")) {
           .import_ui_stop(
             "invalid_schema",
             .tr("Les sources natives ne sont pas importables : utilisez « Lancer le calcul CellChat » ou « Lancer le calcul LIANA ».")
           )
        }
        # CHRYSALIS 2E : garde d'entree explicite (message FR dans le canal
        # d'erreur au lieu d'un req() silencieux).
        obj <- assert_seurat(global_data$sc_obj, context = "communication import")

        identity_col <- input$comm_identity_column
        if (is.null(identity_col) || !nzchar(identity_col)) {
           .import_ui_stop(
             "invalid_input",
             "Choisissez d'abord la colonne de metadonnees Seurat decrivant les identites des populations."
           )
        }
        assert_metadata_column(obj, identity_col, context = "communication import")

        warnings_all <- character(0)

        if (identical(src, "cellchat")) {
          req(input$comm_cellchat_file)
          tab <- .read_table_auto(input$comm_cellchat_file$datapath)
          parsed <- parse_cellchat_import(tab, source_file = input$comm_cellchat_file$name)
          files <- list(table = input$comm_cellchat_file$name)
        } else if (identical(src, "cellchat_object")) {
          req(input$comm_cellchat_obj)
          parsed <- parse_cellchat_object(input$comm_cellchat_obj$datapath,
                                          source_file = input$comm_cellchat_obj$name)
          files <- list(object = input$comm_cellchat_obj$name)
        } else if (identical(src, "cellphonedb")) {
          req(input$comm_cpdb_means)
          means <- .read_table_auto(input$comm_cpdb_means$datapath)
          pvals <- NULL
          files <- list(means = input$comm_cpdb_means$name)
          if (!is.null(input$comm_cpdb_pvalues)) {
            pvals <- .read_table_auto(input$comm_cpdb_pvalues$datapath)
            files$pvalues <- input$comm_cpdb_pvalues$name
          }
          parsed <- parse_cellphonedb_import(means, pvals, source_file = input$comm_cpdb_means$name)
        } else if (identical(src, "liana")) {
          # ── LIANA (CCC 7-8 route (b)) : import de RANGS produits hors de
          # l'app. Les deux choix ci-dessous sont EXIGES — ni le mode
          # d'agregation ni la colonne de rang ne sont deductibles du fichier,
          # et un defaut implicite produirait un artefact d'analyse.
          req(input$comm_liana_file)
          if (is.null(input$comm_liana_mode) || !nzchar(input$comm_liana_mode)) {
             .import_ui_stop(
               "invalid_input",
               paste0(
                 "Choisissez le mode d'agregation LIANA (specificite ou magnitude) : ",
                 "les deux repondent a des questions differentes et ne sont pas ",
                 "comparables entre elles."
               )
             )
          }
          rank_col <- input$comm_liana_rank
          if (is.null(rank_col) || !nzchar(rank_col)) {
             .import_ui_stop(
               "invalid_input",
               paste0(
                 "Choisissez la colonne de rang LIANA a importer (mean_rank, ",
                 "aggregate_rank ou {methode}.rank) — la route (b) importe des ",
                 "rangs : sans rang, il n'y a pas de mesure."
               )
             )
          }
          liana_tab <- .read_table_auto(input$comm_liana_file$datapath)
          parsed <- parse_liana_import(
            liana_tab, rank_column = rank_col,
            aggregation_mode = input$comm_liana_mode,
            source_file = input$comm_liana_file$name
          )
          files <- list(table = input$comm_liana_file$name)
        } else {
           .import_ui_stop(
             "invalid_schema",
             sprintf(.tr("Source d'import inconnue : %s."), as.character(src)[1L])
           )
        }
        warnings_all <- c(warnings_all, parsed$warnings)

        # Harmonisation des identites : vecteur d'identites extrait de la
        # colonne EXPLICITEMENT choisie (exact match, jamais de renommage).
        identities <- unique(as.character(obj@meta.data[[identity_col]]))
        harm <- harmonize_communication_identities(
          parsed$table, identities, identity_col, context = "communication import"
        )
        warnings_all <- c(warnings_all, harm$warnings)

        # QC d'import : comptages avant/apres, duplications, auto-interactions.
        qcr <- communication_import_qc(harm$table)
        warnings_all <- c(warnings_all, qcr$warnings)

        if (nrow(qcr$table) == 0L) {
           .import_ui_stop(
             "invalid_input",
             "Toutes les lignes ont ete supprimees au QC (sender/receiver/ligand/receptor vides) : aucun resultat canonique produit."
           )
        }

        # Resultat canonique — contrat documente dans
        # docs/contracts/COMMUNICATION_RESULT_CONTRACT.md. Provenance PRODUITE
        # ici puis appendee a l'etat partage (regle 7 AGENTS.md).
        canonical <- finalize_communication_result(
          canonical_table = qcr$table,
          source_method   = unique(qcr$table$source_method)[1L],
          source_files    = files,
          identity_column = identity_col,
          identity_mapping = harm$mapping,
          identity_summary = harm$summary,
          column_mapping  = parsed$column_mapping,
          qc              = qcr$counts,
          n_input_rows    = parsed$n_input_rows,
          seurat_obj      = obj,
          extra_warnings  = warnings_all,
          # Route LIANA : la table porte un agregat inter-methodes calcule par
          # LIANA (mean_rank/aggregate_rank). L'app n'agrege rien — elle MARQUE
          # (NULL pour les autres sources -> FALSE).
          external_consensus = isTRUE(parsed$external_consensus),
          analysis_id     = "sc-communication-import"
        )
        .store_result(canonical, obj)

        msg <- sprintf(
          paste0("Import OK : %d lignes canoniques (%d lignes source). ",
                 "Source : %s. Colonne d'identites : %s. ",
                 "Labels harmonises : %d/%d."),
          canonical$input_summary$n_rows_canonical,
          canonical$input_summary$n_rows_input,
          unique(qcr$table$source_method)[1L], identity_col,
          harm$summary$n_labels_matched, harm$summary$n_labels
        )
        if (harm$summary$n_labels_unmatched > 0L) {
          msg <- paste0(msg, "\n", sprintf(
            "%d label(s) sans correspondance (conserves tels quels) : %s",
            harm$summary$n_labels_unmatched,
            paste(utils::head(harm$summary$unmatched_labels, 5), collapse = ", ")
          ))
        }
        comm_status_rv(paste0("[", .status_label(canonical$status), "]\n", msg))
        showNotification(.tr("Import communication valide."), type = "message", duration = 4)
        # ── 3E (O28) : signal VISIBLE quand l'exact match laisse des labels
        # non mappés (le constat 3E est que cet échec restait discret) —
        # notification warning distincte du succès, orientant vers la surface
        # de validation du sous-onglet « Harmonisation identites ».
        if (harm$summary$n_labels_unmatched > 0L) {
          showNotification(
            sprintf(
              .tr("Harmonisation : %d label(s) sans correspondance exacte — propositions de correspondance dans le sous-onglet « Harmonisation identites »."),
              harm$summary$n_labels_unmatched
            ),
            type = "warning", duration = 8
          )
        }

       }, error = function(e) {
         .clear_result_state()
         st <- communication_error_state(e)
         detail <- if (is.na(st)) conditionMessage(e) else
           paste0(conditionMessage(e), " [etat : ", st, "]")
         comm_status_rv(paste(.tr("Erreur import communication :"), detail))
         showNotification(
           paste(.tr("Erreur import communication :"), detail),
           type = "error", duration = 10
         )
       })
    })

    # ── CC-5 : calcul CellChat DANS l'application (Path B) ──────────────────
    # Meme depot, memes vues, memes exports que l'import : le resultat du
    # moteur EST un resultat canonique (regle des deux voies). Aucune vue ni
    # aucun export n'a eu a changer — c'est la preuve que la forme est bonne.
    observeEvent(input$comm_compute, {
      req(global_data$sc_obj)
      tryCatch({
        obj <- assert_seurat(global_data$sc_obj, context = "communication engine")

        identity_col <- input$comm_identity_column
        if (is.null(identity_col) || !nzchar(identity_col)) {
          stop(errorCondition(
            "Choisissez d'abord la colonne de metadonnees Seurat decrivant les identites des populations.",
            class = c("cellchat_engine_error", "error", "condition"),
            state = "invalid_input"
          ))
        }
        if (!identity_col %in% colnames(obj@meta.data)) {
          stop(errorCondition(
            sprintf("Colonne d'identites absente : %s.", identity_col),
            class = c("cellchat_engine_error", "error", "condition"),
            state = "invalid_input"
          ))
        }

        species <- input$comm_engine_species
        if (is.null(species) || !nzchar(species)) {
          stop(errorCondition(
            paste0("Choisissez l'espece : la base ligand-recepteur en ",
                   "decoule et ne se devine pas a partir des donnees."),
            class = c("cellchat_engine_error", "error", "condition"),
            state = "invalid_input"
          ))
        }

        # Dependance PARESSeUSE : l'absence de CellChat est un etat PREVU. On
        # garde la main sur le texte (avec le remede) plutot que de laisser
        # l'erreur du moteur arriver telle quelle dans la notification.
        if (!cellchat_engine_available()) {
          stop(errorCondition(
            paste0(
              "Le package 'CellChat' n'est pas installe : le calcul dans ",
              "l'application est impossible. Il est distribue uniquement sur ",
              "GitHub — remotes::install_github(\"jinworks/CellChat\"). L'import ",
              "de resultats externes reste disponible sans cette dependance."
            ),
            class = c("cellchat_engine_error", "error", "condition"),
            state = "missing_dependency"
          ))
        }

        cc_input <- build_cellchat_input(obj, group_by = identity_col,
                                         species = species)

        result <- withProgress(message = .tr("Calcul CellChat en cours..."),
                               value = 0, {
          run_cellchat(cc_input,
                       seed  = as.integer(input$comm_engine_seed),
                       nboot = as.integer(input$comm_engine_nboot),
                       seurat_obj = obj,
                       on_progress = function(msg) incProgress(0, detail = msg))
        })

        .store_result(result, obj)

        eng <- result$engine
        msg <- sprintf(
          paste0("Calcul OK : %s interactions, %s populations, %s voies ",
                 "significatives. Moteur %s %s — base %s (%s) — graine %s, ",
                 "%s permutations."),
          format(eng$n_interactions), format(eng$n_populations),
          format(eng$n_pathways_significant), eng$engine, eng$engine_version,
          eng$database, eng$database_version, format(eng$seed), format(eng$nboot)
        )
        comm_status_rv(paste0("[", .status_label(result$status), "]\n", msg))
        showNotification(.tr("Calcul CellChat termine."), type = "message", duration = 4)

      }, error = function(e) {
        .clear_result_state()
        # Les erreurs du moteur sont CLASSEES (6 etats) : on affiche l'etat EN
        # PLUS du texte — « no_interactions » ou « invalid_parameters » ne se
        # devinent pas dans un message generique.
        st <- cellchat_engine_error_state(e)
        detail <- if (is.na(st)) conditionMessage(e) else
          paste0(conditionMessage(e), " [etat : ", st, "]")
        comm_status_rv(paste(.tr("Erreur calcul communication :"), detail))
        showNotification(paste(.tr("Erreur calcul communication :"), detail),
                         type = "error", duration = 10)
      })
    })

    observeEvent(input$comm_compute_liana, {
      req(global_data$sc_obj)
      tryCatch({
        obj <- assert_seurat(global_data$sc_obj, context = "moteur LIANA")

        identity_col <- input$comm_identity_column
        if (is.null(identity_col) || !nzchar(identity_col)) {
          .liana_ui_stop(
            "invalid_input",
            .tr("Choisissez d'abord la colonne de metadonnees Seurat decrivant les identites des populations.")
          )
        }
        if (!identity_col %in% colnames(obj@meta.data)) {
          .liana_ui_stop("invalid_input", sprintf("Colonne d'identites absente : %s.", identity_col))
        }

        sample_col <- input$comm_liana_sample_col
        if (is.null(sample_col) || !nzchar(sample_col)) {
          .liana_ui_stop("invalid_input", .tr("Choisissez la colonne d'echantillon LIANA."))
        }
        if (!sample_col %in% colnames(obj@meta.data)) {
          .liana_ui_stop("invalid_input", sprintf("Colonne d'echantillon absente : %s.", sample_col))
        }

        condition_col <- input$comm_liana_condition_col
        if (is.null(condition_col) || !nzchar(condition_col)) {
          .liana_ui_stop("invalid_input", .tr("Choisissez la colonne de condition LIANA."))
        }
        if (!condition_col %in% colnames(obj@meta.data)) {
          .liana_ui_stop("invalid_input", sprintf("Colonne de condition absente : %s.", condition_col))
        }

        if (!liana_engine_available()) {
          stop(errorCondition(
            .tr("Le moteur LIANA natif n'est pas disponible : installez les paquets requis."),
            class = c("liana_engine_error", "error", "condition"),
            state = "missing_dependency"
          ))
        }

        method <- input$comm_liana_method %||% TS_LIANA_METHOD_DEFAULT
        resource <- input$comm_liana_resource %||% TS_LIANA_RESOURCE_DEFAULT
        collection <- withProgress(
          message = .tr("Calcul LIANA par echantillon en cours..."),
          value = 0,
          {
            run_liana_by_sample(
              obj,
              sample_col = sample_col,
              condition_col = condition_col,
              idents_col = identity_col,
              method = method,
              resource = resource,
              seed = as.integer(input$comm_liana_seed),
              min_cells = as.integer(input$comm_liana_min_cells),
              on_progress = function(msg) incProgress(0, detail = msg)
            )
          }
        )

        manifest <- liana_collection_sample_manifest(collection)
        keys <- as.character(manifest$sample_key)
        if (!length(keys)) {
          .liana_ui_stop("no_interactions", .tr("La collection LIANA ne contient aucun echantillon."))
        }
        first_key <- keys[[1L]]
        first_result <- liana_collection_active(collection, first_key)
        .store_result(first_result, obj, collection = collection,
                      active_sample = first_key)
        if (length(keys) > 1L) {
          for (key in keys[-1L]) {
            sample_result <- liana_collection_active(collection, key)
            provenance_append(shared_rv, sample_result$provenance)
          }
        }
        provenance_append(shared_rv, collection$provenance)

        updateSelectInput(session, "comm_active_sample", choices = keys,
                          selected = first_key)
        condition_summary <- liana_collection_condition_summary(collection)
        msg <- .t_fmt(
          .tr("Collection LIANA : statut {status} — {n} echantillon(s), {c} condition(s)."),
          status = as.character(collection$status %||% NA_character_),
          n = length(keys), c = nrow(condition_summary)
        )
        comm_status_rv(paste0("[", as.character(collection$status %||% NA_character_), "]\n", msg))
        showNotification(.tr("Calcul LIANA termine."), type = "message", duration = 4)
      }, error = function(e) {
        .clear_result_state()
        st <- liana_engine_error_state(e)
        detail <- if (is.na(st)) conditionMessage(e) else
          paste0(conditionMessage(e), " [etat : ", st, "]")
        comm_status_rv(paste(.tr("Erreur calcul LIANA :"), detail))
        showNotification(paste(.tr("Erreur calcul LIANA :"), detail),
                         type = "error", duration = 10)
      })
    })

    observeEvent(input$comm_active_sample, {
      req(global_data$sc_obj, comm_state$collection, input$comm_active_sample)
      key <- as.character(input$comm_active_sample)
      tryCatch({
        keys <- as.character(liana_collection_sample_manifest(comm_state$collection)$sample_key)
        if (!key %in% keys) {
          stop(.tr("Echantillon actif absent de la collection LIANA."), call. = FALSE)
        }
        active_result <- liana_collection_active(comm_state$collection, key)
        .store_result(active_result, global_data$sc_obj,
                      append_provenance = FALSE,
                      collection = comm_state$collection,
                      active_sample = key)
        comm_status_rv(.t_fmt(.tr("Echantillon actif : {name}"), name = key))
      }, error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 8)
      })
    }, ignoreInit = TRUE)

    # ── Vues (consommatrices pures du resultat canonique) ──────────────────
    output$comm_table <- DT::renderDataTable({
      req(comm_state$result)
      .check_fingerprint()
      ts_datatable(
        comm_state$result$canonical_table,
        page_length = 15L,
        filename_base = "sc_comm_canonical_table"
      )
    })

    output$comm_table_filtered <- DT::renderDataTable({
      req(comm_state$result)
      .check_fingerprint()
      ts_datatable(
        comm_filtered()$table,
        page_length = 15L,
        filename_base = "sc_comm_filtered_table",
        filter = "none"
      )
    })

    output$comm_dotplot <- renderPlot({
      req(comm_state$result)
      .check_fingerprint()
      tryCatch(
        plot_communication_dotplot(comm_state$result, comm_filtered()$table,
                                   seurat_obj = .view_object()),
        error = function(e) .error_plot(e)
      )
    })

    output$comm_heatmap <- renderPlot({
      req(comm_state$result)
      .check_fingerprint()
      tryCatch(
        plot_communication_pathway_heatmap(comm_state$result, comm_filtered()$table,
                                           seurat_obj = .view_object()),
        error = function(e) .error_plot(e)
      )
    })

    output$comm_circle <- renderPlot({
      req(comm_state$result)
      .check_fingerprint()
      tryCatch(
        plot_communication_circle(comm_state$result, comm_filtered()$table,
                                  seurat_obj = .view_object()),
        error = function(e) .error_plot(e)
      )
    })

    output$comm_centrality <- DT::renderDataTable({
      req(comm_state$result)
      .check_fingerprint()
      ts_datatable(
        build_communication_centrality(comm_state$result, comm_filtered()$table),
        page_length = 15L,
        filename_base = "sc_comm_centrality",
        filter = "none"
      )
    })

    output$comm_schema <- DT::renderDataTable({
      req(comm_state$result)
      cm <- comm_state$result$column_mapping %||% list()
      df <- data.frame(
        champ_canonique = names(cm),
        colonne_source = vapply(cm, paste, character(1), collapse = " / "),
        stringsAsFactors = FALSE
      )
      ts_datatable(df, page_length = 15L, filename_base = "sc_comm_column_mapping",
                   filter = "none", scroll_x = FALSE)
    })

    output$comm_schema_note <- renderText({
      req(comm_state$result)
      .tr("Mapping resolu de facon deterministe a l'import — aucune conversion supposee entre schemas incompatibles.")
    })

    output$comm_mapping <- DT::renderDataTable({
      req(comm_state$result)
      req(comm_state$result$identity_mapping)
      ts_datatable(
        comm_state$result$identity_mapping,
        page_length = 15L,
        filename_base = "sc_comm_identity_mapping"
      )
    })

    # ── 3E (O28, spéc §2.17) : surface de validation propose/valide ──────────
    # Propositions PURES (propose_communication_identity_mapping — aucune
    # application possible de ce cote) affichées dans un DT éditable pré-rempli ;
    # application UNIQUEMENT via apply_communication_identity_mapping avec un
    # mapping explicitement validé (colonne « identite validee » + modal de
    # confirmation). Aucun effet sans validation : la voie exact match
    # historique reste en vigueur tant que rien n'est appliqué.
    prop_rv <- shiny::reactiveVal(NULL)
    .prop_apply_pending_rv <- shiny::reactiveVal(NULL)

    output$comm_mapping_hint <- renderText({
      .tr("Propositions pre-remplies (exact + suggerees, preuve par ligne). Editez ou videz la colonne « identite validee », puis appliquez avec confirmation : rien n'est applique sans validation et l'exact match reste en vigueur.")
    })

    # DT éditable : seule la colonne « identite validee » est éditable —
    # les colonnes de preuve (rule/distance/status/detail) restent lisibles.
    output$comm_proposals <- DT::renderDataTable({
      p <- prop_rv()
      req(p)
      ts_datatable(
        p,
        page_length = 10L,
        filename_base = "sc_comm_identity_proposals",
        editable = list(
          target = "column",
          indices = which(colnames(p) == "identity_validee") - 1L
        )
      )
    })

    observeEvent(input$comm_proposals_cell_edit, {
      info <- input$comm_proposals_cell_edit
      p <- prop_rv()
      req(p, info$row, !is.null(info$col))
      editable_idx <- which(colnames(p) == "identity_validee") - 1L
      if (!identical(as.integer(info$col), as.integer(editable_idx))) return()
      val <- trimws(as.character(info$value))
      p[info$row, "identity_validee"] <- if (nzchar(val)) val else NA_character_
      prop_rv(p)
    })

    observeEvent(input$comm_propose_identities, {
      # Garde explicite (leçon 3C : un req() silencieux n'oriente personne) :
      # sans résultat importé, la surface n'a rien à proposer — dire-le.
      if (is.null(comm_state$result)) {
        showNotification(
          .tr("Aucun resultat importe — importez d'abord une table de communication."),
          type = "warning", duration = 6
        )
        return()
      }
      tryCatch({
        r <- comm_state$result
        # Surface limitée aux resultats IMPORTES : la voie calculée dans
        # l'application (CellChat/LIANA natifs) harmonise déjà sur la colonne
        # de l'objet lui-même — un re-mapping n'y a pas d'objet.
        if (!isTRUE(r$provenance$import_only)) {
          showNotification(
            .tr("Mapping interactif disponible pour les resultats importes uniquement."),
            type = "warning", duration = 6
          )
          return()
        }
        obj <- assert_seurat(global_data$sc_obj,
                             context = "propositions d'identites communication")
        identities <- unique(as.character(obj@meta.data[[r$identity_column]]))
        m <- r$identity_mapping
        p <- propose_communication_identity_mapping(
          labels = m$label,
          seurat_identities = identities,
          roles = m$roles
        )
        # Pré-remplissage : SEULES les suggestions (status "proposed") sont
        # pré-copiées ; les exact match (déjà en vigueur) et les lignes
        # ambiguës/non appariées restent vides = ne seront PAS appliquées
        # sans édition explicite de l'utilisateur.
        p$identity_validee <- ifelse(p$status == "proposed",
                                     p$candidate_identity, NA_character_)
        prop_rv(p)
      }, error = function(e) {
        showNotification(
          paste(.tr("Erreur propositions d'identites :"), conditionMessage(e)),
          type = "error", duration = 10
        )
      })
    })

    observeEvent(input$comm_apply_identities, {
      # Garde explicite (leçon 3C) : aucune proposition en attente = rien à
      # valider — l'exact match reste en vigueur, et c'est DIT.
      if (is.null(prop_rv())) {
        showNotification(
          .tr("Aucune proposition en attente — cliquez d'abord sur « Proposer des correspondances »."),
          type = "warning", duration = 6
        )
        return()
      }
      p <- prop_rv()
      req(comm_state$result)
      am <- p[!is.na(p$identity_validee) &
                nzchar(trimws(as.character(p$identity_validee))),
              c("label", "identity_validee"), drop = FALSE]
      colnames(am) <- c("label", "identity")
      if (nrow(am) == 0L) {
        showNotification(
          .tr("Aucune correspondance validee — rien a appliquer (l'exact match reste en vigueur)."),
          type = "warning", duration = 6
        )
        return()
      }
      .prop_apply_pending_rv(am)
      # Confirmation EXPLICITE : la modal liste exactement ce qui sera appliqué.
      showModal(modalDialog(
        title = .tr("Confirmer le mapping d'identites valide"),
        tags$p(.tr("Ces paires seront appliquees a la table canonique (aucune autre modification) :")),
        tags$ul(lapply(seq_len(nrow(am)), function(i) {
          row <- p[p$label == am$label[i], , drop = FALSE]
          ev <- if (nrow(row)) sprintf("[%s — %s]", row$rule[1], row$status[1]) else ""
          tags$li(sprintf("%s -> %s %s", am$label[i], am$identity[i], ev))
        })),
        tags$p(.tr("Les identites inconnues de la colonne choisie seront refusees ; une identite vide vaut « laisser non-apparie ».")),
        footer = tagList(
          modalButton(.tr("Annuler")),
          actionButton(ns("comm_apply_confirm"), .tr("Appliquer"),
                       class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$comm_apply_confirm, {
      removeModal()
      am <- .prop_apply_pending_rv()
      r <- comm_state$result
      req(am, r)
      tryCatch({
        obj <- assert_seurat(global_data$sc_obj,
                             context = "application du mapping d'identites")
        .check_fingerprint()
        identities <- unique(as.character(obj@meta.data[[r$identity_column]]))
        mapped <- apply_communication_identity_mapping(
          r$canonical_table, identities, r$identity_column, am
        )
        qcr <- communication_import_qc(mapped$table)
        canonical <- finalize_communication_result(
          canonical_table = qcr$table,
          source_method   = unique(qcr$table$source_method)[1L],
          source_files    = r$input_summary$files,
          identity_column = r$identity_column,
          identity_mapping = mapped$mapping,
          identity_summary = mapped$summary,
          column_mapping  = r$column_mapping,
          qc              = qcr$counts,
          n_input_rows    = r$input_summary$n_rows_input,
          seurat_obj      = obj,
          extra_warnings  = c(mapped$warnings, qcr$warnings),
          external_consensus = isTRUE(r$provenance$is_external_consensus),
          analysis_id     = "sc-communication-import"
        )
        .store_result(canonical, obj)
        prop_rv(NULL)
        .prop_apply_pending_rv(NULL)
        showNotification(
          sprintf(.tr("Mapping d'identites applique : %d paire(s) validee(s)."),
                  nrow(am)),
          type = "message", duration = 5
        )
      }, error = function(e) {
        # validate() (garde de péremption .check_fingerprint) : erreur
        # silencieuse shiny, déjà notifiée — ne pas doubler la notification.
        if (inherits(e, "shiny.silent.error")) return()
        showNotification(
          paste(.tr("Erreur application du mapping :"), conditionMessage(e)),
          type = "error", duration = 10
        )
      })
    })

    output$comm_qc <- renderUI({
      req(comm_state$result)
      r <- comm_state$result
      qc <- r$qc %||% list()
      ids <- r$identity_summary %||% list()

      .row <- function(k, v) tags$tr(tags$th(style = "width:65%;", k), tags$td(v))
      rows <- list(
        .row(.tr("Lignes source (avant conversion)"), as.character(r$input_summary$n_rows_input)),
        .row(.tr("Lignes canoniques (apres QC)"), as.character(r$input_summary$n_rows_canonical)),
        .row(.tr("Lignes supprimees (champs requis vides)"), as.character(qc$n_dropped_required_fields %||% 0L)),
        .row(.tr("Auto-interactions (sender = receiver)"), as.character(qc$n_self_interactions %||% 0L)),
        .row(.tr("Interactions dupliquees (conservees et flagees)"), as.character(qc$n_duplicate_interactions %||% 0L)),
        .row(.tr("Pathways manquants"), as.character(qc$n_pathway_missing %||% 0L)),
        .row(.tr("P-values hors [0,1]"), as.character(qc$n_p_value_out_of_range %||% 0L)),
        .row(.tr("Labels sources distincts"), as.character(ids$n_labels %||% NA_integer_)),
        .row(.tr("Labels harmonises (exact match)"), as.character(ids$n_labels_matched %||% NA_integer_)),
        .row(.tr("Labels sans correspondance"), as.character(ids$n_labels_unmatched %||% NA_integer_))
      )
      warn <- r$warnings %||% character(0)
      tagList(
        div(class = "m-3",
            tags$table(class = "table table-sm table-bordered",
                       tags$tbody(lapply(rows, function(rw) rw)))),
        if (length(warn)) div(class = "m-3", h6(.tr("Avertissements produits a l'import")),
                              tags$ul(lapply(warn, function(w) tags$li(w))))
      )
    })

    # ── Exports (traces par analysis_id ; filtres captures en provenance) ──
    .export_figure <- function(plot_fn, file) {
      .check_fingerprint()
      p <- plot_fn()
      ts_export_plot(file, p, width = 8, height = 6.5, dpi = 300)
    }

    # Chaque export filtre PRODUIT une entree de provenance (filtres figes
    # dans les parametres) appended a l'etat partage — jamais reconstruite.
    .append_filter_provenance <- function() {
      fs <- comm_filtered()
      provenance_append(
        shared_rv,
        build_communication_filter_provenance(comm_state$result, fs)
      )
    }

    output$dl_comm_table <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_import_table", "csv"),
      content = function(file) {
        req(comm_state$result)
        .check_fingerprint()
        utils::write.csv(comm_state$result$canonical_table, file, row.names = FALSE)
      }
    )

    output$dl_comm_filtered <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_filtered_table", "csv"),
      content = function(file) {
        req(comm_state$result)
        .check_fingerprint()
        fs <- comm_filtered()
        .append_filter_provenance()
        df <- build_communication_filtered_export(
          comm_state$result, fs$table, fs$description
        )
        utils::write.csv(df, file, row.names = FALSE)
      }
    )

    output$dl_comm_centrality <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_centrality", "csv"),
      content = function(file) {
        req(comm_state$result)
        .check_fingerprint()
        .append_filter_provenance()
        df <- build_communication_centrality(comm_state$result, comm_filtered()$table)
        utils::write.csv(df, file, row.names = FALSE)
      }
    )

    output$dl_comm_dotplot_png <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_dotplot", "png"),
      content = function(file) .export_figure(function() {
        plot_communication_dotplot(comm_state$result, comm_filtered()$table,
                                   seurat_obj = .view_object())
      }, file)
    )
    output$dl_comm_dotplot_pdf <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_dotplot", "pdf"),
      content = function(file) .export_figure(function() {
        plot_communication_dotplot(comm_state$result, comm_filtered()$table,
                                   seurat_obj = .view_object())
      }, file)
    )
    output$dl_comm_heatmap_png <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_pathway_heatmap", "png"),
      content = function(file) .export_figure(function() {
        plot_communication_pathway_heatmap(comm_state$result, comm_filtered()$table,
                                           seurat_obj = .view_object())
      }, file)
    )
    output$dl_comm_heatmap_pdf <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_pathway_heatmap", "pdf"),
      content = function(file) .export_figure(function() {
        plot_communication_pathway_heatmap(comm_state$result, comm_filtered()$table,
                                           seurat_obj = .view_object())
      }, file)
    )
    output$dl_comm_circle_png <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_circle_network", "png"),
      content = function(file) .export_figure(function() {
        plot_communication_circle(comm_state$result, comm_filtered()$table,
                                  seurat_obj = .view_object())
      }, file)
    )
    output$dl_comm_circle_pdf <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_circle_network", "pdf"),
      content = function(file) .export_figure(function() {
        plot_communication_circle(comm_state$result, comm_filtered()$table,
                                  seurat_obj = .view_object())
      }, file)
    )

    output$dl_comm_mapping <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_identity_mapping", "csv"),
      content = function(file) {
        req(comm_state$result)
        .check_fingerprint()
        df <- build_communication_identity_mapping_export(comm_state$result)
        utils::write.csv(df, file, row.names = FALSE)
      }
    )

    output$dl_comm_summary <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_import_summary", "csv"),
      content = function(file) {
        req(comm_state$result)
        .check_fingerprint()
        df <- build_communication_import_summary(comm_state$result)
        utils::write.csv(df, file, row.names = FALSE)
      }
    )

    output$dl_comm_liana_manifest <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "liana_sample_manifest", "csv"),
      content = function(file) {
        req(comm_state$collection)
        .check_fingerprint()
        utils::write.csv(liana_collection_sample_manifest(comm_state$collection),
                         file, row.names = FALSE)
      }
    )

    output$dl_comm_liana_condition_summary <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "liana_condition_summary", "csv"),
      content = function(file) {
        req(comm_state$collection)
        .check_fingerprint()
        utils::write.csv(liana_collection_condition_summary(comm_state$collection),
                         file, row.names = FALSE)
      }
    )

    output$dl_comm_rds <- downloadHandler(
      filename = function() communication_export_filename(comm_state$result, "communication_result", "rds"),
       content = function(file) {
         req(comm_state$result)
         .check_fingerprint()
         payload <- comm_state$result
         if (!is.null(comm_state$collection)) {
           payload <- list(
             type = "liana_collection",
             collection = comm_state$collection,
             active_sample = comm_state$active_sample,
             active_result = comm_state$result
           )
         }
         saveRDS(payload, file)
       }
     )

    # ── Contextes V1.x (roadmap CCC avancée) : sous-modules montés dans le
    # panneau existant — consommateurs du résultat canonique (comm_state est
    # partagé par référence), aucun recalcul de score, aucun nouveau panneau
    # latéral (friction UX n°2). ────────────────────────────────────────────
    mod_sc_communication_spatial_server("spatial", comm_state, global_data, shared_rv)
    mod_sc_communication_trajectory_server("trajectory", comm_state, global_data, shared_rv)
    mod_sc_communication_velocity_server("velocity_ctx", comm_state, global_data, shared_rv)
    mod_sc_communication_perturbation_server("perturbation", comm_state, global_data, shared_rv)

    # Expose state for tests (optional)
    return(comm_state)
  })
}

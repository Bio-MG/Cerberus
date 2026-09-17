# =============================================================================
# mod_bulk_de_ui.R — Bulk Child 2: all UI builders (i18n Phase 3.1)
# =============================================================================
# Pure UI-builder functions (no server logic, no reactivity) extracted
# verbatim from the pre-refactor monolithic mod_bulk_de.R. Kept together in
# one file since they are low-complexity and share no state — splitting them
# further would add file-count without reducing real complexity.
#
# i18n Phase 3.1:
#   - STATIC labels via i18n$t()
#   - Choice display names via .tr_plain() inside stats::setNames() (never
#     i18n$t() inside setNames() — would emit <span> and crash at UI build time)
#   - Choice VALUES stay stable ASCII
# =============================================================================


# ── UI: sidebar controls ──────────────────────────────────────────────────────

mod_bulk_de_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("condition_col"), i18n$t("Variable de Condition (principale)"), choices = NULL),
    selectizeInput(ns("covariates"), i18n$t("Covariables additionnelles (optionnel)"),
                   choices = NULL, multiple = TRUE,
                   options = list(placeholder = "Ex: batch, sex")),

    verbatimTextOutput(ns("design_formula_preview")),

    uiOutput(ns("de_readiness_check")),
    uiOutput(ns("no_rep_active_banner")),

    fluidRow(
      column(6, selectInput(ns("group_ref"),    i18n$t("Groupe R\u00e9f\u00e9rence"), choices = NULL)),
      column(6, selectInput(ns("group_target"), i18n$t("Groupe Cible"),     choices = NULL))
    ),

    selectInput(ns("de_engine"), i18n$t("Moteur Statistique"), choices = NULL),
    div(style = "display:flex;align-items:center;gap:6px;",
        checkboxInput(ns("shrink_lfc"), i18n$t("Shrinkage LFC (apeglm) \u2014 DESeq2 uniquement"), value = TRUE),
        tooltip(bsicons::bs_icon("info-circle"),
                i18n$t("R\u00e9duit les Log2FC artificiellement \u00e9lev\u00e9s sur les g\u00e8nes \u00e0 faible expression (haute variance d'\u00e9chantillonnage). Recommand\u00e9 pour le classement/visualisation ; laissez activ\u00e9 sauf besoin sp\u00e9cifique."))),

    # Plan sans r\u00e9plicat (n = p) : d\u00e9tection + contournement attest\u00e9.
    # Rendu c\u00f4t\u00e9 serveur (le diagnostic d\u00e9pend de la colonne condition
    # choisie) — voir mod_bulk_de_run.R::output$no_rep_bypass_ui.
    uiOutput(ns("no_rep_bypass_ui")),

    fluidRow(
      column(6, numericInput(ns("lfc_thresh"),  i18n$t("Seuil |Log2FC|"), value = 1,    min = 0, step = 0.1)),
      column(6, numericInput(ns("padj_thresh"), i18n$t("Seuil p-adj"),    value = 0.05, min = 0, max = 1, step = 0.01))
    ),

    # STAT-Q1 — correction pour tests multiples. Choix déclarés dans
    # config/defaults.R (TS_PADJ_METHODS) : jamais de liste en dur ici.
    # "fdr" est volontairement absent (alias de BH dans p.adjust.methods).
    selectInput(ns("padj_method"), i18n$t("M\u00e9thode de correction (p-adj)"),
                choices  = stats::setNames(TS_PADJ_METHODS, TS_PADJ_METHODS),
                selected = TS_PADJ_METHOD_DEFAULT),
    helpText(style = "font-size:0.78em;",
             i18n$t("S'applique aux tests DE (DESeq2/edgeR/limma) et \u00e0 l'enrichissement. Pour DESeq2, changer la m\u00e9thode recalcule p-adj \u00e0 partir du mod\u00e8le d\u00e9j\u00e0 ajust\u00e9 \u2014 sans r\u00e9ajustement.")),

    # PLOT-S1 — apparence des graphiques (thème + base_size). Résolu par
    # ts_theme() (R/plotting/theme.R) et appliqué à Volcano / MA / Heatmap.
    # Le défaut (minimal, 11) reproduit EXACTEMENT le rendu antérieur.
    hr(),
    h6(i18n$t("Apparence des graphiques"), style = "font-weight:bold;"),
    selectInput(ns("plot_theme"), i18n$t("Thème"),
                choices = stats::setNames(
                  TS_THEME_CHOICES,
                  c(.tr_plain("Minimal"), .tr_plain("Classique"),
                    .tr_plain("BW"), .tr_plain("Vide")))),
    sliderInput(ns("base_size"), i18n$t("Taille de police (base)"),
                min = 10, max = 20, value = TS_BASE_SIZE_DEFAULT, step = 1),

    textInput(ns("contrast_name"), i18n$t("Nom du contraste (auto si vide)"), placeholder = "Ex: KO_vs_WT"),

    actionButton(ns("run_de"), tagList(icon("play"), i18n$t("Lancer l'Analyse Diff\u00e9rentielle")),
                 class = "btn-success w-100"),

    uiOutput(ns("pairwise_btn_ui")),

    actionButton(ns("run_multimethod"),
                 tagList(icon("scale-balanced"), i18n$t("Comparer DESeq2 / edgeR / limma-voom")),
                 class = "btn-outline-warning w-100 mt-1"),
    helpText(i18n$t("Lance le M\u00caME contraste (Cible/R\u00e9f\u00e9rence ci-dessus) avec les 3 moteurs, puis calcule un consensus de rang \u2014 voir l'onglet \"Multi-m\u00e9thodes\".")),

    hr(),

    div(
      class = "border rounded p-2 mb-2", style = "background:#fff8e1;",
      checkboxInput(ns("adhoc_mode"),
                    tags$strong(tagList("\U0001f3af", i18n$t("Contraste Ad-hoc (s\u00e9lection manuelle d'\u00e9chantillons)"))),
                    value = FALSE),
      conditionalPanel(
        condition = "input.adhoc_mode == true", ns = ns,
        helpText(style = "font-size:0.8em;",
                 i18n$t("Ignore la colonne condition \u2014 reconstruit une m\u00e9tadonn\u00e9e minimale \u00e0 la vol\u00e9e.")),
        splitLayout(
          checkboxGroupInput(ns("adhoc_group_a"), i18n$t("Groupe A"), choices = NULL),
          checkboxGroupInput(ns("adhoc_group_b"), i18n$t("Groupe B"), choices = NULL)
        ),
        textInput(ns("adhoc_contrast_name"), i18n$t("Nom du contraste"), placeholder = "Ex: KO_vs_WT"),
        uiOutput(ns("adhoc_readiness")),
        actionButton(ns("run_de_adhoc"), tagList(icon("play"), i18n$t("Lancer l'Analyse (Ad-hoc)")),
                     class = "btn-warning w-100")
      )
    ),

    hr(),
    h6(i18n$t("Contrastes calcul\u00e9s:"), style = "font-weight:bold;"),
    selectInput(ns("active_contrast_view"), NULL, choices = NULL),
    div(class = "small text-muted mt-1", textOutput(ns("de_status")))
  )
}


# ── UI: Volcano Plot tab ──────────────────────────────────────────────────────

mod_bulk_de_volcano_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    max_height  = "850px",
    card_header("Volcano Plot"),
    uiOutput(ns("sync_warning_banner")),
    checkboxInput(ns("volcano_interactive"), i18n$t("Interactif (Plotly \u2014 survol pour d\u00e9tails g\u00e8ne)"), value = FALSE),
    uiOutput(ns("volcano_manual_palette_ui")),
    uiOutput(ns("volcano_container")),
    fluidRow(
      column(6, selectInput(ns("volcano_export_fmt"), i18n$t("Format export"),
                            choices = ts_export_format_choices_ui())),
      column(6, div(style = "margin-top:25px;",
                    downloadButton(ns("dl_volcano_png"), i18n$t("Export Volcano (statique)"),
                                   class = "btn-sm btn-secondary w-100")))
    )
  )
}


# ── UI: MA-Plot tab ────────────────────────────────────────────────────────────

mod_bulk_de_ma_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    max_height  = "850px",
    card_header("MA-Plot"),
    checkboxInput(ns("ma_interactive"), i18n$t("Interactif (Plotly \u2014 survol pour d\u00e9tails g\u00e8ne)"), value = FALSE),
    helpText(style = "font-size:0.8em;",
            i18n$t("Couleurs (Significatif / Non-sig.) r\u00e9glables depuis l'onglet Volcano Plot (palette \"Manuel\" dans la barre lat\u00e9rale \u00c9tape 1).")),
    uiOutput(ns("ma_container")),
    fluidRow(
      column(6, selectInput(ns("ma_export_fmt"), i18n$t("Format export"),
                            choices = ts_export_format_choices_ui())),
      column(6, div(style = "margin-top:25px;",
                    downloadButton(ns("dl_ma_png"), i18n$t("Export MA-Plot (statique)"),
                                   class = "btn-sm btn-secondary w-100")))
    )
  )
}


# ── UI: Heatmap tab ────────────────────────────────────────────────────────────

mod_bulk_de_heatmap_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE, max_height = "900px",
    card_header("Heatmap"),
    uiOutput(ns("sync_warning_banner_heatmap")),
    fluidRow(
      column(4, numericInput(ns("heatmap_top_n"), i18n$t("Top N g\u00e8nes (par p-adj)"), value = 30, min = 2, max = 200)),
      column(4, selectizeInput(ns("heatmap_annot"), i18n$t("Annotation colonnes"), choices = NULL,
                               options = list(placeholder = "\u2014", allowEmptyOption = TRUE))),
      column(4, selectInput(ns("heatmap_direction"), i18n$t("Sous-ensemble"),
                            choices = stats::setNames(c("all","sig","up","down","ns"),
                              c(.tr_plain("Tous (par p-adj)"), .tr_plain("Significatifs (Up+Down)"),
                                .tr_plain("Up-r\u00e9gul\u00e9s"), .tr_plain("Down-r\u00e9gul\u00e9s"), .tr_plain("Non-significatifs")))))
    ),
    uiOutput(ns("heatmap_manual_palette_ui")),
    # PLOT-S4 — reglages de clustering exposes par ts_complex_heatmap().
    # Les libelles de choix sont des termes techniques (euclidean, ward.D2…)
    # identiques en FR et EN : pas de cle i18n a ajouter pour eux.
    fluidRow(
      column(4, selectInput(ns("heatmap_clust_distance"), i18n$t("Distance de clustering"),
                            choices = stats::setNames(TS_HEATMAP_DISTANCES, TS_HEATMAP_DISTANCES),
                            selected = "euclidean")),
      column(4, selectInput(ns("heatmap_clust_method"), i18n$t("Méthode de clustering"),
                            choices = stats::setNames(TS_HEATMAP_METHODS, TS_HEATMAP_METHODS),
                            selected = "complete")),
      column(4, numericInput(ns("heatmap_k_row"),
                             i18n$t("Découpage en k groupes de lignes (0 = aucun)"),
                             value = 0, min = 0, max = 20, step = 1))
    ),
    plotOutput(ns("plot_heatmap"), height = "660px"),
    fluidRow(
      column(6, selectInput(ns("heatmap_export_fmt"), i18n$t("Format export"),
                            choices = ts_export_format_choices_ui())),
      column(6, div(style = "margin-top:25px;",
                    downloadButton(ns("dl_heatmap"), tagList("\U0001f4e5", i18n$t("Export Heatmap")),
                                   class = "btn-sm btn-secondary w-100")))
    )
  )
}


# ── UI: Table DE tab ───────────────────────────────────────────────────────────

mod_bulk_de_table_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    max_height  = "850px",
    card_header(i18n$t("Table DE")),
    div(
      style = "display:flex;justify-content:flex-end;gap:5px;margin-bottom:5px;",
      downloadButton(ns("dl_de_csv"),   "CSV",   class = "btn-sm btn-primary"),
      downloadButton(ns("dl_de_excel"), "Excel", class = "btn-sm btn-success")
    ),
    DTOutput(ns("table_de"))
  )
}


# ── UI: Résumé Up/Down tab (barchart, all computed contrasts) ────────────────

mod_bulk_de_summary_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    max_height  = "1100px",
    card_header(i18n$t("R\u00e9sum\u00e9 Up/Down")),
    div(class = "alert alert-light", style = "font-size:0.85em;border-left:3px solid #18BC9C;",
        bsicons::bs_icon("info-circle"),
        " ", i18n$t("Nombre de g\u00e8nes Up / Down par contraste calcul\u00e9, selon les seuils |Log2FC| / p-adj actuels (\u00c9tape 2) \u2014 se met \u00e0 jour en direct si vous changez les seuils, sans recalcul DE.")),
    plotOutput(ns("plot_updown"), height = "620px"),
    downloadButton(ns("dl_updown_png"), i18n$t("Export PNG"), class = "btn-sm btn-secondary mt-2"),
    hr(),
    h6(i18n$t("Table r\u00e9capitulative"), style = "font-weight:bold;"),
    DTOutput(ns("table_updown")),
    downloadButton(ns("dl_updown_csv"), i18n$t("Export CSV"), class = "btn-sm btn-info mt-2")
  )
}


# ── UI: Venn/UpSet tab (compare gene sets across contrasts) ──────────────────

mod_bulk_de_venn_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header("Venn / UpSet"),
    div(class = "alert alert-light", style = "font-size:0.85em;border-left:3px solid #9B59B6;",
       bsicons::bs_icon("info-circle"),
       " ", i18n$t("Compare les g\u00e8nes significatifs ENTRE plusieurs contrastes (utile apr\u00e8s un run Pairwise auto). Seuils utilis\u00e9s : ceux du panneau Step 2 (|Log2FC| / p-adj) \u2014 se mettent \u00e0 jour en direct si vous les changez, sans recalcul DE.")),
    uiOutput(ns("venn_gate_message")),
    fluidRow(
      column(7, selectizeInput(ns("venn_contrasts"), i18n$t("Contrastes \u00e0 comparer"),
                               choices = NULL, multiple = TRUE,
                               options = list(maxItems = 6, placeholder = .tr_plain("2 \u00e0 6 contrastes")))),
      column(5, radioButtons(ns("venn_type"), i18n$t("Type de diagramme"),
                             choices = stats::setNames(c("upset","venn"),
                               c(.tr_plain("UpSet (recommand\u00e9)"), .tr_plain("Venn (2-4 contrastes)"))),
                             selected = "upset"))
    ),
    checkboxInput(ns("venn_direction_aware"),
                 i18n$t("Distinguer Up / Down (chaque contraste devient 2 ensembles)"), value = FALSE),
    div(style = "min-width:600px;overflow-x:auto;height:580px;",
       plotOutput(ns("venn_plot"), height = "560px")),
    fluidRow(
      column(6, downloadButton(ns("dl_venn_png"), i18n$t("Export PNG"), class = "btn-sm btn-secondary w-100")),
      column(6, downloadButton(ns("dl_venn_genes_csv"), i18n$t("Export g\u00e8nes par intersection (CSV)"),
                               class = "btn-sm btn-info w-100"))
    ),
    hr(),
    h6(i18n$t("Table des intersections"), style = "font-weight:bold;"),
    div(style = "min-height:200px;overflow-y:auto;",
        DTOutput(ns("venn_intersection_table")))
  )
}


# ── UI: Multi-méthodes tab (getAllDE + rankConsensus + Venn 3-méthodes) ──────

mod_bulk_de_multimethod_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    max_height  = "950px",
    card_header(i18n$t("Multi-m\u00e9thodes (DESeq2 / edgeR / limma-voom)")),
    div(class = "alert alert-light", style = "font-size:0.85em;border-left:3px solid #F39C12;",
       bsicons::bs_icon("info-circle"),
       " ", i18n$t("Compare le M\u00caME contraste (Cible/R\u00e9f\u00e9rence du panneau Step 2) avec les 3 moteurs statistiques disponibles. Le consensus de rang moyenne le classement p-value de chaque m\u00e9thode \u2014 un g\u00e8ne coh\u00e9rent entre m\u00e9thodes (rang faible, m\u00eame sens du Log2FC) est un candidat plus robuste qu'un g\u00e8ne significatif sur une seule m\u00e9thode.")),

    uiOutput(ns("multimethod_status_ui")),

    navset_tab(
      nav_panel(
        i18n$t("Venn / UpSet (m\u00e9thodes)"),
        radioButtons(ns("mm_venn_type"), i18n$t("Type de diagramme"),
                    choices = stats::setNames(c("upset","venn"),
                      c(.tr_plain("UpSet (recommand\u00e9)"), .tr_plain("Venn (2-3 m\u00e9thodes)"))),
                    selected = "upset", inline = TRUE),
        plotOutput(ns("mm_venn_plot"), height = "480px"),
        downloadButton(ns("dl_mm_venn_png"), i18n$t("Export PNG"), class = "btn-sm btn-secondary mt-2")
      ),
      nav_panel(
        i18n$t("Table consensus"),
        helpText(i18n$t("Tri\u00e9e par rang moyen (mean_rank) \u2014 les g\u00e8nes les plus consistants entre m\u00e9thodes apparaissent en premier.")),
        DTOutput(ns("mm_consensus_table")),
        downloadButton(ns("dl_mm_consensus_csv"), i18n$t("Export CSV"), class = "btn-sm btn-info mt-2")
      )
    )
  )
}

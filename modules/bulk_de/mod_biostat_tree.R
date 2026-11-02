# =============================================================================
# modules/bulk_de/mod_biostat_tree.R — M3 (O27 2026-10-29) : assistant
# biostatistique — accordéon latéral de l'onglet Bulk DE + miroir lecture SC
# =============================================================================
# CONTRAT GELÉ : docs/contracts/BIOSTAT_DECISION_CONTRACT.md (SPÉC GELÉE
# v1.0.0) — ce module SUIT le contrat, il ne le modifie pas. Moteur : M2
# (tools/pkg_skeleton/R/biostat_tree.R — biostat_build_tree / biostat_resolve
# / biostat_decision_record, commit 12ad2a8) : ce module ORCHESTRE, il ne
# recalcule rien (C17 — aucune logique métier ici ; motif ASR-3 :
# modules/session/mod_session_checkpoint.R, app.R n'en fait que le montage).
#
# ORIENTATION SEULE (O27 §12.2) : l'utilisateur parcourt l'arbre (radioButtons
# par question, valeurs = clés de réponse stables ASCII, libellés = clés
# biostat.a.* via tr), voit l'orientation de chaque feuille (method_id + clé
# biostat.r.* via tr — I7 : jamais de texte brut) et confirme éventuellement
# (« Valider ce choix » — contrat §2 : user_confirmed TRUE uniquement après ce
# clic). AUCUN test statistique n'est lancé ici (garde lexicale T2 de
# tests/testthat/test-biostat-m3-ui.R) ; la couche diagnostic (Shapiro/Levene,
# contrat §4) reste hors surface M3.
#
# PROVENANCE (contrat §5-I2/I3 — couche module) : la confirmation produit le
# record §2 via biostat_decision_record() puis l'append 4F par le canal
# EXISTANT new_provenance_entry() + provenance_append(global_data, .)
# (tools/pkg_skeleton/R/provenance.R) — PRODUITE, jamais reconstruite (règle 7
# du rapport consolidé) ; PAS de validation = PAS de trace (aucune écriture au
# montage, aucune écriture au parcours).
#
# I1 — conseiller, jamais sélectionner : ce module N'ÉCRIT JAMAIS vers
# input$de_engine ni aucun champ du run Bulk (le run reste lancé par
# l'utilisateur sur le chemin existant). Le `engine` du record est un argument
# injectable (défaut "") — le câblage lecture seule vers la déclaration du
# panneau Étape 2 est M4.
#
# MIROIR LECTURE SC (O27 : « miroir lecture SC ») : mod_biostat_mirror_ui/
# _server rend la recommandation VALIDÉE par l'instance Bulk verbatim depuis
# global_data$biostat_decision — la MÊME source, JAMAIS de re-dérivation
# (aucun appel moteur M2 dans le corps du serveur miroir, assertion
# structurelle T5 du test) ; à vide, uiOutput NULL = conteneur vide, empreinte
# nulle (motif mod_session_checkpoint.R) ; lecture seule — aucun bouton.
#
# PREFILL (contrat §2) : valeurs par défaut list(groups = "two", paired =
# FALSE) — la déduction depuis les métadonnées du jeu actif est M4 (proposition
# §3 : rv$tree_prefill) ; aucune lecture de metadata ici (aucune hypothèse sur
# le nom des colonnes).
#
# 0 CSS www/ (règle post-O13) : primitives bslib/shiny + classes Bootstrap
# existantes et styles inline seulement — aucun fichier CSS, aucune injection
# de style (garde lexicale T7 du test).
#
# i18n (§5-I7) : clés uniquement. Libellés UI = texte français (convention
# maison « clé = texte FR », rendus via tr — global_data$i18n en session, repli
# global i18n, repli clé). Clés pointillées biostat.q.* / biostat.a.* /
# biostat.r.* portées par l'arbre gelé M2 : atterries dans
# i18n/translation.json par le mécanisme SCOPÉ tools/add_i18n_keys.R (lot M3
# seul, tool restauré à HEAD ensuite — leçon B1/B5/DSTV-3) ; résorbe la réserve
# du vérificateur M2. Dynamiques : re-render sur global_data$language (motif
# app.R).
#
# MONTAGE app.R (pins d'arbre, leçon frontière M2 — le module est sourcé et
# monté, manifeste régénéré, test-app-sourcing.R re-basé 162 -> 163 daté) :
#   source("modules/bulk_de/mod_biostat_tree.R")            (bloc 11, bulk_de)
#   nav_panel "Analyse Bulk RNA" : mod_biostat_tree_ui("biostat_bulk")
#   nav_panel "Analyse Single-Cell" : mod_biostat_mirror_ui("biostat_sc")
#   server() : mod_biostat_tree_server("biostat_bulk", global_data)
#              mod_biostat_mirror_server("biostat_sc", global_data)
# =============================================================================

# Traduction session-scopée : global_data$i18n (session) d'abord, repli global
# i18n (libellés statiques), repli clé (honnête — jamais de texte brut ici).
.biostat_m3_tr <- function(gd, key) {
  tr <- NULL
  if (!is.null(gd) && !is.null(gd$i18n)) {
    tr <- gd$i18n
  } else if (exists("i18n", envir = globalenv(), inherits = FALSE)) {
    tr <- get("i18n", envir = globalenv())
  }
  if (is.null(tr)) return(key)
  tryCatch(tr$t(key), error = function(e) key)
}

# ── UI — accordéon latéral de l'onglet Bulk DE ────────────────────────────────

mod_biostat_tree_ui <- function(id) {
  ns <- NS(id)
  # open = FALSE : monté REPLIÉ par défaut (bslib 0.11.0 mesuré — open=NULL
  # ouvrirait le premier panel ; open=FALSE rend class="accordion-collapse
  # collapse" sans "show" + boutons "accordion-button collapsed").
  bslib::accordion(
    id = ns("acc_biostat"),
    open = FALSE,
    bslib::accordion_panel(
      i18n$t("Assistant biostatistique \u2014 orientation"),
      value = "panel_biostat",
      icon = icon("compass"),
      helpText(i18n$t("Parcourez les questions ; l'orientation seule est affich\u00e9e \u2014 aucun test n'est lanc\u00e9.")),
      uiOutput(ns("biostat_walk_ui")),
      uiOutput(ns("biostat_orientation_ui")),
      uiOutput(ns("biostat_confirm_ui")),
      actionButton(ns("biostat_restart"),
                   tagList(icon("rotate-left"), i18n$t("Recommencer")),
                   class = "btn-outline-secondary btn-sm mt-1"),
      uiOutput(ns("biostat_status_ui"))
    )
  )
}

# ── UI — miroir lecture SC (conteneur statique ; contenu = publication Bulk) ──

mod_biostat_mirror_ui <- function(id) {
  ns <- NS(id)
  div(class = "border rounded p-2 mb-2",
      uiOutput(ns("mirror_content")),
      helpText(i18n$t("La recommandation valid\u00e9e c\u00f4t\u00e9 \u00ab Analyse Bulk RNA \u00bb appara\u00eet ici (lecture seule).")))
}

# ── SERVER — parcours de l'arbre + confirmation + provenance (couche module) ──

mod_biostat_tree_server <- function(id, global_data, engine = "") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    .tr <- function(key) .biostat_m3_tr(global_data, key)

    tree <- biostat_build_tree()
    rv <- reactiveValues(
      node    = tree,   # noeud courant (question)
      leaf    = NULL,   # feuille atteinte (orientation)
      answers = list(), # carte id de question -> clé de réponse (consumée)
      status  = NULL    # ligne d'état (confirmation)
    )

    # Question courante : radioButtons SANS présélection (selected =
    # character(0) mesuré : aucun "checked" — l'avance ne se déclenche que sur
    # un clic explicite) ; valeurs = clés de réponse ASCII stables (convention
    # mod_bulk_de_ui.R), libellés = clés biostat.a.* via tr.
    output$biostat_walk_ui <- renderUI({
      global_data$language  # re-render sur changement de langue (motif app.R)
      nd <- rv$node
      if (is.null(nd) || !is.null(rv$leaf)) return(NULL)
      radioButtons(ns("biostat_choice"),
                   label   = .tr(nd$i18n_key),
                   choiceValues = names(nd$answers),
                   choiceNames  = vapply(as.character(nd$answers_i18n), .tr, character(1)),
                   selected     = character(0))
    })

    # Avance d'UN pas par clic : garde `val %in% names(answers)` — une valeur
    # obsolète (question précédente) est ignorée, jamais interprétée.
    observeEvent(input$biostat_choice, {
      nd <- rv$node
      req(!is.null(nd), !is.null(input$biostat_choice))
      val <- input$biostat_choice
      if (!val %in% names(nd$answers)) return()
      rv$answers[[nd$id]] <- val
      target <- nd$answers[[val]]
      child <- NULL
      for (k in nd$children) {
        if (identical(k$name, target) || identical(k$id, target)) {
          child <- k
          break
        }
      }
      req(!is.null(child))
      if (!is.null(child$method_id) && length(child$method_id) == 1L &&
          nzchar(child$method_id)) {
        rv$leaf <- child   # feuille = orientation (JAMAIS un lancement)
      } else {
        rv$node <- child
        rv$leaf <- NULL
      }
    })

    # Orientation de la feuille : method_id + clé biostat.r.* via tr (I7).
    output$biostat_orientation_ui <- renderUI({
      global_data$language
      leaf <- rv$leaf
      if (is.null(leaf)) return(NULL)
      div(h6(.tr("Orientation recommand\u00e9e"), style = "font-weight:bold;"),
          div(tags$code(leaf$method_id)),
          div(.tr(leaf$rationale_key)))
    })

    # Action explicite unique : « Valider ce choix » (contrat §2).
    output$biostat_confirm_ui <- renderUI({
      global_data$language
      if (is.null(rv$leaf)) return(NULL)
      actionButton(ns("biostat_confirm"), .tr("Valider ce choix"),
                   class = "btn-success btn-sm mt-1 w-100")
    })

    # I2/I3 — UNE validation = UNE entrée ; pas de validation, pas de trace.
    observeEvent(input$biostat_confirm, {
      req(!is.null(rv$leaf))
      leaf <- rv$leaf
      res  <- biostat_resolve(tree, rv$answers)
      rec  <- biostat_decision_record(
        node_path      = res$node_path,
        method_id      = res$method_id,
        engine         = engine,               # déclaré par l'utilisateur (M4)
        rationale_keys = leaf$rationale_key,
        diagnostics    = list(),               # §5-I4 : vide pour bulk_*
        prefilled      = list(groups = "two", paired = FALSE),
        user_confirmed = TRUE
      )
      # Canal 4F EXISTANT (contrat §1) — PRODUIT ici, jamais reconstruit :
      entry <- new_provenance_entry(
        analysis_id = "biostat_decision",
        method      = paste0("decision_tree_v", biostat_tree_version()),
        parameters  = list(
          node_path   = rec$node_path,
          method_id   = rec$method_id,
          engine      = rec$engine,
          rationale   = rec$rationale_keys,
          diagnostics = rec$diagnostics,
          prefilled   = rec$prefilled
        ),
        dataset = global_data$bulk_obj
      )
      provenance_append(global_data, entry)
      # Publication pour le miroir lecture SC (MÊME source, jamais re-dérivée).
      global_data$biostat_decision <- rec
      rv$status <- .tr("Choix enregistr\u00e9 dans la provenance.")
      # Nouveau parcours vierge (une validation = une entrée, pas une boucle).
      rv$node    <- tree
      rv$leaf    <- NULL
      rv$answers <- list()
    })

    # Recommencer : purge le parcours — AUCUNE écriture (I2 : rien confirmé).
    observeEvent(input$biostat_restart, {
      rv$node    <- tree
      rv$leaf    <- NULL
      rv$answers <- list()
      rv$status  <- NULL
    })

    output$biostat_status_ui <- renderUI({
      global_data$language
      if (is.null(rv$status)) return(NULL)
      div(class = "small text-success mt-1", rv$status)
    })
  })
}

# ── SERVER — miroir lecture SC : rend la publication Bulk, ne dérive RIEN ─────

mod_biostat_mirror_server <- function(id, global_data) {
  moduleServer(id, function(input, output, session) {
    .tr <- function(key) .biostat_m3_tr(global_data, key)
    output$mirror_content <- renderUI({
      global_data$language
      # MÊME source que l'instance Bulk (publication à la confirmation) —
      # AUCUN appel moteur ici (assertion structurelle T5) : lecture seule.
      rec <- global_data$biostat_decision
      if (is.null(rec)) return(NULL)  # uiOutput NULL = empreinte nulle
      div(h6(.tr("Recommandation de l'assistant biostatistique (lecture seule)"),
             style = "font-weight:bold;"),
          div(tags$code(rec$method_id)),
          div(vapply(rec$rationale_keys, .tr, character(1))))
    })
  })
}

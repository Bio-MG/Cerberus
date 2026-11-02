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
# l'utilisateur sur le chemin existant). Le `engine` effectif d'une feuille
# bulk_* est la DÉCLARATION de l'utilisateur lue EN LECTURE SEULE de
# shared_rv$declared_de_engine au moment du clic (couture 1 ligne posée dans
# l'observe des miroirs de mod_bulk_de_engine.R — jalon M4), avec repli sur
# l'argument injectable (défaut "") si la déclaration est absente/vide ; pour
# toute feuille non bulk_*, chaîne vide (contrat §2 — le record M2 normalise).
#
# MIROIR LECTURE SC (O27 : « miroir lecture SC ») : mod_biostat_mirror_ui/
# _server rend la recommandation VALIDÉE par l'instance Bulk verbatim depuis
# global_data$biostat_decision — la MÊME source, JAMAIS de re-dérivation
# (aucun appel moteur M2 dans le corps du serveur miroir, assertion
# structurelle T5 du test) ; à vide, uiOutput NULL = conteneur vide, empreinte
# nulle (motif mod_session_checkpoint.R) ; lecture seule — aucun bouton.
#
# PREFILL (contrat §2) — M4 : quand shared_rv est fourni, un observe de
# pré-remplissage lit UNIQUEMENT les métadonnées du jeu actif via le moteur
# PUR biostat_prefill_from_metadata() (biostat_tree.R, câblage 2 args :
# colonne de condition PUBLIÉE shared_rv$active_condition_col — aucune
# hypothèse sur le nom des colonnes subject, donc paired n'est pas déduit
# côté module aujourd'hui) et alimente rv$prefill ; la déduction devient la
# `selected` du radioButtons quand la question la porte (proposition §3),
# l'utilisateur reste maître : la réponse contredite est TRACÉE (warnings
# ASCII contrat §1 — prefill_groups_overridden / prefill_paired_overridden —
# passés à new_provenance_entry(warnings =), signature mesurée), jamais
# bloquée. shared_rv omis (défaut NULL) : comportement M3 byte-pour-byte,
# record aux défauts list(groups = "two", paired = FALSE).
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

# M4 — présélection déduite pour la question courante (PURE, hors Shiny).
# La déduction ne pré-sélectionne QUE les questions qui la portent : celles
# dont les réponses portent la signature two/three_plus (mêmes clés ASCII sur
# les branches bulk et continue — même question « combien de groupes »), et
# la question paired seulement si la déduction vaut TRUE (une déduction FALSE
# est une non-déduction : rien n'est présélectionné, l'utilisateur répond
# librement). prefill NULL (shared_rv omis, M3) ou noeud sans correspondance :
# character(0) — byte-pour-byte M3.
.biostat_m4_selected <- function(nd, prefill) {
  if (is.null(prefill) || is.null(nd) || is.null(nd$answers)) {
    return(character(0))
  }
  ans <- names(nd$answers)
  g <- prefill$groups
  if (!is.null(g) && length(g) == 1L && !is.na(g) &&
      all(c("two", "three_plus") %in% ans) && g %in% ans) {
    return(g)
  }
  if (isTRUE(prefill$paired) && "paired" %in% ans) {
    return("paired")
  }
  character(0)
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
# M4 : shared_rv = NULL (défaut) préservé M3 byte-pour-byte ; fourni, il porte
# la colonne de condition publiée (active_condition_col) et la déclaration du
# moteur (declared_de_engine) — le module ne les LIT jamais autrement (I1).

mod_biostat_tree_server <- function(id, global_data, shared_rv = NULL,
                                    engine = "") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    .tr <- function(key) .biostat_m3_tr(global_data, key)

    tree <- biostat_build_tree()
    rv <- reactiveValues(
      node    = tree,   # noeud courant (question)
      leaf    = NULL,   # feuille atteinte (orientation)
      answers = list(), # carte id de question -> clé de réponse (consumée)
      status  = NULL,   # ligne d'état (confirmation)
      prefill = NULL    # M4 : list(groups =, paired =) déduite (shared_rv fourni)
    )

    # M4 — PREFILL (proposition §3) : lecture UNIQUEMENT (I1/I2 — aucune
    # écriture au montage), depuis les métadonnées du jeu actif et la colonne
    # de condition publiée par le panneau Bulk DE (couture M4 :
    # shared_rv$active_condition_col). Observateur instancié SEULEMENT quand
    # shared_rv est fourni : omis, rv$prefill reste NULL et le comportement
    # M3 est préservé byte-pour-byte. Le re-rendu sur global_data$language
    # est porté par les renderUI (déjà dépendants) ; la déduction, elle, ne
    # dépend pas de la langue.
    if (!is.null(shared_rv)) {
      observe({
        req(shared_rv, global_data$bulk_obj, shared_rv$active_condition_col)
        rv$prefill <- biostat_prefill_from_metadata(
          global_data$bulk_obj$metadata, shared_rv$active_condition_col)
      })
    }

    # Question courante : radioButtons SANS présélection (M3, selected =
    # character(0) mesuré : aucun "checked" — l'avance ne se déclenche que sur
    # un clic explicite) ; M4 : présélection déduite quand la question la
    # porte (.biostat_m4_selected — reste MODIFIABLE, conseiller jamais
    # imposer) ; valeurs = clés de réponse ASCII stables (convention
    # mod_bulk_de_ui.R), libellés = clés biostat.a.* via tr.
    # REPARIÉ M4 (bug latent M3, mesuré 2026-11-02) : vapply préserve les noms
    # de nd$answers_i18n — choiceNames NOMMÉ = radioButtons() refusait de
    # rendre (shiny 1.14.0 normalizeChoicesArgs « must not be named », MESURÉ
    # : aucun test M3 ne lisait biostat_walk_ui, l'accordéon live était cassé
    # au premier rendu de question). unname() ne change AUCUN libellé.
    output$biostat_walk_ui <- renderUI({
      global_data$language  # re-render sur changement de langue (motif app.R)
      nd <- rv$node
      if (is.null(nd) || !is.null(rv$leaf)) return(NULL)
      radioButtons(ns("biostat_choice"),
                   label   = .tr(nd$i18n_key),
                   choiceValues = names(nd$answers),
                   choiceNames  = unname(vapply(as.character(nd$answers_i18n), .tr, character(1))),
                   selected     = .biostat_m4_selected(nd, rv$prefill))
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
      # M4 — engine effectif (contrat §2) : pour une feuille bulk_*, la
      # DÉCLARATION de l'utilisateur lue EN LECTURE SEULE de shared_rv AU
      # MOMENT DU CLIC ; repli sur l'argument `engine` (M3) si absente/vide ;
      # chaîne vide pour toute feuille non bulk_* (« chaîne vide sinon » —
      # le record M2 normalise aussi, garde gelée inchangée).
      eng <- engine
      if (!is.null(shared_rv)) {
        dec <- shared_rv$declared_de_engine
        if (is.character(dec) && length(dec) == 1L && !is.na(dec) &&
            nzchar(dec)) {
          eng <- dec
        }
      }
      eng_eff <- if (startsWith(res$method_id, "bulk_")) eng else ""
      # M4 — prefilled (contrat §2 : le record porte TOUJOURS groups+paired) :
      # valeurs déduites si disponibles, sinon défauts M3 (groups = NA n'est
      # pas une valeur du contrat §2 — repli "two").
      pf        <- rv$prefill
      prefilled <- list(groups = "two", paired = FALSE)
      warns     <- character(0)
      if (!is.null(pf)) {
        pg <- pf$groups
        if (is.character(pg) && length(pg) == 1L && !is.na(pg)) {
          prefilled$groups <- pg
        }
        if (isTRUE(pf$paired)) prefilled$paired <- TRUE
        # Warnings contrat §1 (« pré-remplissage contredit par l'utilisateur »)
        # : vecteur character ASCII stable, passés à new_provenance_entry()
        # (signature mesurée : warnings = character(0) en défaut). Un warning
        # ne vise qu'une valeur RÉELLEMENT pré-remplie : la déduction paired
        # (FALSE = non-déduction avec le câblage 2 args, cf. en-tête) n'est
        # jamais « contredite » — prefill_paired_overridden ne peut naître
        # que d'un futur câblage subject_col, la branche est déjà câblée.
        ug <- NA_character_   # réponse utilisateur aux questions two/three_plus
        up <- NULL            # réponse utilisateur à la question paired
        for (qid in names(res$answers)) {
          a <- res$answers[[qid]]
          if (is.character(a) && length(a) == 1L) {
            if (identical(a, "two") || identical(a, "three_plus")) ug <- a
            if (identical(a, "paired"))      up <- TRUE
            if (identical(a, "independent")) up <- FALSE
          }
        }
        if (is.character(pg) && length(pg) == 1L && !is.na(pg) &&
            !is.na(ug) && !identical(ug, pg)) {
          warns <- c(warns, "prefill_groups_overridden")
        }
        if (isTRUE(pf$paired) && identical(up, FALSE)) {
          warns <- c(warns, "prefill_paired_overridden")
        }
      }
      rec  <- biostat_decision_record(
        node_path      = res$node_path,
        method_id      = res$method_id,
        engine         = eng_eff,             # déclaré par l'utilisateur (M4, I1)
        rationale_keys = leaf$rationale_key,
        diagnostics    = list(),               # §5-I4 : vide pour bulk_*
        prefilled      = prefilled,           # déduit (M4) sinon défauts M3
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
        dataset  = global_data$bulk_obj,
        warnings = warns
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

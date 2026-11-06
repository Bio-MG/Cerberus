# =============================================================================
# global.R — Packages, global options, theme (slim, post-refactor)
# =============================================================================
# Domain helper functions used to live here (3251 lines, "fourre-tout") — they
# have been split out into:
#   helpers_io.R       — multi-format loading, gene-ID / GEO metadata mapping
#   helpers_sc.R       — Seurat: scatter/violin/correlation/trajectory plots
#   helpers_bulk.R     — DESeq2/edgeR/limma engine + bulk plots
#   helpers_pathway.R  — ORA + GSEA, shared by mod_sc_pathways.R and
#                         mod_bulk_pathways.R
#   R/utils_spatial_io.R / R/utils_spatial_async.R — BPCells conversion +
#                         mirai daemon pool for the Spatial module (v3)
# global.R itself now ONLY holds: package loading, global options, future
# parallel plan, and the bslib theme. source() order in app.R: this file
# first (defines packages/options other helpers may rely on at call time),
# then the 4 helpers_*.R (order among them does not matter — R resolves
# function-to-function calls at call time, not at source time), then the
# R/utils_spatial_*.R pair (spatial daemons init), then modules.
#
# F2 (2026-11-06, candidat 4 « réordonnancement partiel ») : le chargement
# d'ANALYSE et le plan future ne sont plus payés ici — ils sont DIFFÉRÉS
# après la mise à l'écoute (.biomentat_deferred_boot_run(), schedulée par
# app.R derrière une porte TCP, voir la section 3bis en fin de fichier). Ce
# fichier ne charge plus à la source que le LIGHT : options, packages du
# build UI, i18n, thème. Garde : tests/testthat/test-deferred-boot-init.R.
#
# AUDIT FIX (quick win): R/utils_spatial_io.R used to be source()'d HERE,
# BEFORE any package is loaded, AND AGAIN in app.R (after packages are
# loaded, alongside the rest of the spatial infrastructure). Two independent
# audits flagged this double-sourcing as a latent divergence risk (harmless
# today since source() just redefines the same functions identically, but a
# future edit to one call site and not the other would silently diverge).
# Single source of truth now lives in app.R, where it belongs alongside
# R/utils_spatial_async.R / _multi.R / _niche.R / _reference.R and AFTER
# every package is guaranteed loaded.
# =============================================================================

# global.R v3.txt

# Charge les librairies et définit les options globales

# --- 1. MEMOIRE & OPTIONS ---

options(future.globals.maxSize = 10000 * 1024^2)

options(shiny.maxRequestSize = 5000 * 1024^2)

# --- 1bis. RENDU DES PLOTS : backend PNG EPINGLE + dimension MINIMALE -------
# PLOT-S6. Deux defauts distincts, MESURES le 2026-09-15 ; le detail des mesures
# et la raison d'etre des planchers sont dans tools/pkg_skeleton/R/plot_dims.R
# (D2 M4-pilote : le fichier vit desormais dans l'arbre du paquet).
#
# (1) BACKEND PNG NON DECLARE.
#     `shiny:::startPNG()` choisit son device par `is_installed("ragg")` ->
#     `Cairo::CairoPNG` -> `grDevices::png`. Ni ragg ni Cairo ne sont des
#     entrees de renv.lock : le rendu dependait donc de paquets presents ou non
#     dans la bibliotheque SYSTEME, sans le dire — et c'est le libelle de
#     l'erreur (« figure margins too large » avec ragg/Cairo, « invalid 'width'
#     or 'height' » avec grDevices::png) qui trahissait le choix. On epingle :
#     ragg s'il est declare, sinon grDevices::png (base R) ; JAMAIS Cairo, qui
#     n'est pas declare. Le choix devient reproductible d'une machine a l'autre.
options(shiny.usecairo = FALSE)
options(shiny.useragg  = requireNamespace("ragg", quietly = TRUE))
#
# (2) DEVICE DE SURFACE NULLE -> « figure margins too large ».
#     Le client n'envoie la taille d'un output que si `width != 0 || height != 0`
#     (shiny.js, doSendSize) : un element dont UNE SEULE dimension est nulle
#     passe donc tel quel, et `startPNG()` ouvre un device 0 x N ou son
#     `plot.new()` echoue. Le plot n'est jamais fautif.
#     Correctif : `renderPlot()` est enveloppe pour que le mode "auto" — le seul
#     pilote par le client — passe par des dimensions BORNEES. Les 92 sites de
#     modules/ sont couverts sans etre touches : tous appellent `renderPlot()`
#     sans qualification, aucun n'ecrit `shiny::renderPlot()`.
#     `ts_render_plot_args()` vit dans tools/pkg_skeleton/R/plot_dims.R
#     (D2 M4-pilote), source plus loin par app.R : resolu a l'APPEL, pas au
#     chargement de ce fichier.
renderPlot <- function(expr, width = "auto", height = "auto", res = 72, ...,
                       alt = NA, env = parent.frame(), quoted = FALSE,
                       execOnResize = FALSE, outputArgs = list()) {
  expr_q <- if (isTRUE(quoted)) expr else substitute(expr)
  dims   <- ts_render_plot_args(width, height)
  shiny::renderPlot(
    expr_q,
    width = dims$width, height = dims$height,
    res = res, ..., alt = alt, env = env, quoted = TRUE,
    execOnResize = execOnResize, outputArgs = outputArgs
  )
}



# --- 2. PACKAGES (CRAN / Bioconductor — installables via install.packages()/BiocManager) ---
# new package install.packages(c("RANN", "irlba", "topicmodels", "slam")) , deconvonvolution label transfer : BiocManager::install('glmGamPoi')
#
# F2 (2026-11-06, candidat 4 « réordonnancement partiel ») : split MESURÉ.
#   - LIGHT (ci-dessous, chargé à la source) : les packages du BUILD UI —
#     95 corps de mod_*_ui scannés par AST le 2026-11-06 : 0 appel aux
#     packages différés ; app.R lui-même : 0 appel de code (chaînes
#     Seurat/BPCells décoratives mises à part).
#   - DEFERRED (deferred_packages + loops optionnels/bioc, en fin de
#     fichier) : les packages d'analyse, un processus frais chacun,
#     mesurés le 2026-11-06 : celldex 7.3 s, destiny 7.3 s, Seurat 5.3 s,
#     SingleR 5.1 s, DESeq2 4.3 s, slingshot 4.1 s, sva 3.7 s,
#     SingleCellExperiment 3.4 s, BPCells 2.5 s — ils ne précèdent plus le
#     bind (naissance→« Listening » ~38 s → ~10-15 s espéré, marker.log
#     QC/boot_diagnose/20261105T211025) et leur coût ne peut plus tomber
#     DANS la fenêtre bind→listen (la porte TCP l'interdit, section 3bis).
required_packages <- c(
  "shiny", "bslib", "ggplot2", "dplyr", "DT", "patchwork", "viridis", "ggrepel",
  "plotly", "bsicons", "future", "shinyFiles",
  "fs", "igraph", "Matrix", "reshape2",
  "shinyjs", "circlize", "rmarkdown", "zip",
  "mirai", "leaflet", "scattermore",
  "ape", "RANN", "irlba", "png", "RColorBrewer", "shinyWidgets", "shinycssloaders",
  # PLOT-S5 — export SVG. svglite est le device vectoriel de la branche
  # format == "svg" de ts_export_plot() (R/plotting/export.R). Il etait deja
  # installe ET present dans renv.lock (2.2.2) mais NON declare : la branche
  # svg retombait donc silencieusement sur png. Le declarer la rend
  # garantie (et non plus dependante d'une installation fortuite).
  "svglite"
)

# F2 — packages d'analyse DIFFÉRÉS (chargés par .biomentat_deferred_boot_run(),
# après l'armement de l'accept ; consommés à l'APPEL par les modules —
# résolution runtime, jamais au build UI, mesure AST ci-dessus).
deferred_packages <- c(
  "Seurat", "SeuratObject",
  "SingleR", "celldex", "SingleCellExperiment",
  "harmony", "destiny", "sf"
)

optional_packages <- c(
  "leaflet.extras2",
  "leiden",
  "topicmodels",
  "slam",
  "schard"
)

bioc_packages <- c(
  "DESeq2", "edgeR", "limma", "ComplexHeatmap",
  # Slingshot : inference de lineees pour le module Trajectoire (option
  # "Slingshot — inférence de lignées"). Bioconductor :
  #   BiocManager::install("slingshot")
  # Absent => seul le pseudotemps exploratoire kNN reste disponible.
  "slingshot",
  # STAT-S1 — correction de batch ComBat-seq. Bioconductor :
  #   BiocManager::install("sva")
  # Volontairement ICI (et non dans required_packages) : la correction de
  # batch est un etage OPTIONNEL du pipeline bulk. Absent, l'app demarre
  # normalement et run_combat_seq() renvoie une erreur classee
  # `missing_dependency` citant le remede — jamais de repli silencieux.
  "sva"
)


# F2 : LIGHT loop uniquement — les loops optional/bioc/deferred vivent dans
# .biomentat_deferred_boot_run() (fin de fichier), mêmes messages/warnings.
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    warning(sprintf("Paquet requis manquant : %s", pkg))
  }
}

# F2 — flags d'analyse consommés au FLUX DES OBSERVERS (session déjà ouverte,
# mod_bulk_de_engine.R:116 pour has_limma, bulk_helpers.R:933 pour
# isTRUE(has_limma)) : limma/edgeR sont LIGHT (0.09/0.25 s mesurés) donc
# restent pré-écoute, pour qu'une session précoce voie le vrai flag.
has_edger <- requireNamespace("edgeR", quietly = TRUE)

has_limma <- requireNamespace("limma", quietly = TRUE)

# F2 — SCAFFOLDING des flags différés : pré-DÉCLARÉS FALSE (sémantique
# « package pas encore chargé » — les modules sont construits pour la
# dégradation gracieuse, jamais un « object not found ») ; la vraie valeur
# est posée par .biomentat_deferred_boot_run(). Mesure consommateurs
# 2026-11-06 : AUCUN de ces 11 symboles n'est lu hors global.R (le
# requireNamespace inline des modules/spatial_async.R/io_helpers.R fabrique
# ses propres flags locaux) — le FALSE précoce est donc invisible au build
# UI et ne dégrade que la fenêtre de race (~init, post-écoute).
has_deseq2       <- FALSE
has_bpcells      <- FALSE
has_spacexr      <- FALSE
has_stdeconvolve <- FALSE
has_leafgl       <- FALSE
has_mirai        <- FALSE
has_rann         <- FALSE
has_slingshot    <- FALSE
has_schard       <- FALSE
has_arrow        <- FALSE



# --- Spatial v3 : dependances non-CRAN / optionnelles ---------------------
# F2 (2026-11-06) : ce bloc (requires + messages) est DÉPLACÉ VERBATIM dans
# .biomentat_deferred_boot_run() (fin de fichier) — les namespaces
# BPCells 2.5 s / spacexr 0.5 s / STdeconvolve 0.5 s / slingshot 4.1 s /
# schard 0.5 s / arrow 0.9 s (mesures processus frais 2026-11-06) ne
# précèdent plus le bind. Les 10 flags correspondants sont pré-déclarés
# FALSE ci-dessus (scaffolding) et y sont posés à leur vraie valeur. Aucun
# consommateur hors global.R (mesure grep 2026-11-06 : les modules
# fabrique leurs propres flags locaux par requireNamespace inline). Le
# commentaire d'origine (commandes d'installation) suit le bloc.



# PARALLÉLISATION (fix 2026-08 : ce bloc était dupliqué — deux plan(multisession,
# workers = detectCores()-2) lançaient 2 x 14 workers PSOCK au démarrage, en
# plus des 6 daemons mirai => tempête de ~20+ processus R simultanés. Sous
# Windows, chaque enfant paie l'activation renv (~54 s observee), les
# connexions PSOCK expiraient ("Cluster setup failed ... 13 of 14 workers")
# et l'app mourait au lancement. Un seul plan, plafonne a 4 workers ; les
# gros calculs async passent deja par le pool mirai (R/utils_spatial_async.R).
#
# F2 (2026-11-06) : le plan() lui-même est DIFFÉRÉ dans
# .biomentat_deferred_boot_run() — marker.log 211025 : plan + 8 pids workers
# x renv 0.9-1.2 s ≈ 6.5 s payés avant « Listening » aujourd'hui ; demain le
# spawn ne peut plus tomber DANS la fenêtre bind→listen (la porte TCP du run
# l'interdit). future résout le plan paresseusement au premier job : entre
# la source et l'init différée, un job éventuel tournerait en sequential
# (comportement « plan absent », identique à une installation sans future).
# Plafond mirai 1-2 workers intouché (MODE_C_audit:143) — ce bloc ne
# concerne que le plan future, jamais le pool mirai.





# --- i18n (shiny.i18n) --------------------------------------------------------
#   - Keys ARE the French strings (shiny.i18n pattern); fr/en values live in
#     i18n/translation.json.
#   - The GLOBAL `i18n` object is used ONLY at UI build time (static labels in
#     *_ui() functions). Dynamic/server content uses the per-session
#     translator `global_data$i18n` (created in app.R via
#     .new_session_i18n()) so concurrent users never share language state.
I18N_AVAILABLE        <- requireNamespace("shiny.i18n", quietly = TRUE)
I18N_DEFAULT_LANG     <- "fr"
I18N_TRANSLATION_FILE <- file.path("i18n", "translation.json")

if (I18N_AVAILABLE) {
  i18n <- shiny.i18n::Translator$new(translation_json_path = I18N_TRANSLATION_FILE)
  i18n$set_translation_language(I18N_DEFAULT_LANG)
  # CRITICAL (i18n live-switch fix): without use_js(), $t() returns PLAIN
  # strings and NO <span class="i18n" data-key> wrappers are ever emitted.
  # page_navbar() materialises its nav children BEFORE evaluating header=,
  # so usei18n()'s own use_js() call came too late — the client shim
  # (#i18n-state binding) had zero elements to translate and every static
  # label stayed French after switching language. Calling use_js() HERE makes
  # every build-time $t() emit a translatable span. Server-side $t() calls
  # (with an active reactive domain) are unaffected — they still return
  # plain strings via raw_translate().
  i18n$use_js()
} else {
  warning("Package 'shiny.i18n' absent — interface affichée en français uniquement. ",
          "Installez-le via install.packages('shiny.i18n') pour activer le multilingue.")
  # Minimal no-op fallback: every i18n$t() / i18n_plain() call site keeps
  # working (returns the French key unchanged) instead of crashing the app.
  i18n <- list(
    t                        = function(key, ...) key,
    set_translation_language = function(language) invisible(NULL),
    get_translation_language = function() I18N_DEFAULT_LANG
  )
}

#' Fresh session-scoped translator (called once per session in app.R server).
.new_session_i18n <- function() {
  if (I18N_AVAILABLE) {
    tr <- shiny.i18n::Translator$new(translation_json_path = I18N_TRANSLATION_FILE)
    tr$set_translation_language(I18N_DEFAULT_LANG)
    tr
  } else {
    i18n  # global fallback object
  }
}

#' Named-placeholder interpolation for translated templates.
#' Usage: .t_fmt(global_data$i18n$t("✓ {n} gènes retenus."), n = 42)
.t_fmt <- function(template, ...) {
  vals <- list(...)
  for (nm in names(vals)) {
    template <- gsub(paste0("{", nm, "}"), format(vals[[nm]]), template, fixed = TRUE)
  }
  template
}

#' Translation-function factory for pure helpers (plot/report builders).
#' Returns the session translator's `$t` method, or identity (French) if the
#' translator is unavailable. ALWAYS call inside a reactive context that ALSO
#' reads `global_data$language`, so plots re-render on language switch.
#' (Reading `global_data$i18n` alone is NOT enough: set_translation_language()
#' mutates the R6 object in place, which does not invalidate reactive readers.)
.tr_fn <- function(gd) {
  tr <- gd$i18n
  if (is.null(tr)) return(function(x, ...) x)
  tr$t
}

#' shiny.i18n usei18n() with raw-JSON fix.
#'
#' usei18n() embeds the translation dictionary via
#'   tags$script(glue("var i18n_translations = {toJSON(...)}"))
#' and htmltools renders that script TEXT ENTITY-ESCAPED ("&" -> "&amp;",
#' quotes -> numeric entities). The client shim then compares each element's
#' browser-DECODED data-key (e.g. "1. Filtrage & VST") against the ESCAPED
#' "_row" values ("1. Filtrage &amp; VST"), so every key containing & ' " < >
#' silently fails to translate while plain keys work — a very confusing
#' partial failure. Wrapping the generated JS in htmltools::HTML() keeps the
#' JSON raw so lookups match exactly.
.usei18n_fixed <- function(translator) {
  ui <- shiny.i18n::usei18n(translator)
  # ui is tagList(tags$head(script(dict), script(src=...)), i18n_state(...))
  idx_head <- Position(function(x) inherits(x, "shiny.tag") && identical(x$name, "head"),
                       ui, nomatch = NA_integer_)
  if (!is.na(idx_head)) {
    head_tag <- ui[[idx_head]]
    for (i in seq_along(head_tag$children)) {
      ch <- head_tag$children[[i]]
      if (inherits(ch, "shiny.tag") && identical(ch$name, "script") &&
          is.null(ch$attribs$src) && length(ch$children) >= 1 &&
          is.character(ch$children[[1]]) && !inherits(ch$children[[1]], "html")) {
        head_tag$children[[i]]$children[[1]] <- htmltools::HTML(ch$children[[1]])
      }
    }
    ui[[idx_head]] <- head_tag
  }
  ui
}

# --- i18n helpers (Phase 2 hardening) ----------------------------------------
# Strip shiny.i18n's <span class="i18n" data-key="...">...</span> wrapping
# (emitted whenever $t() runs OUTSIDE an active session) back to a plain
# length-1 string. Safe to call on already-plain strings too.
.strip_i18n_html <- function(res) {
  if (inherits(res, c("shiny.tag", "shiny.tag.list")) || length(res) > 1L) {
    flat <- paste(as.character(res), collapse = "")
    return(gsub("^<span[^>]*>|</span\\s*>$", "", flat))
  }
  res
}

#' Plain-scalar translation for UI BUILD TIME, non-HTML contexts ONLY:
#' setNames()/choices vectors, plot labels, anything requiring length-1 text.
#' Returns the DEFAULT language (fr) at build time; live switching for these
#' elements is done SERVER-SIDE via update*Input() on language change —
#' they are deliberately invisible to the client-side JS shim.
.tr_plain <- function(key) {
  if (!exists("I18N_AVAILABLE") || !isTRUE(I18N_AVAILABLE)) return(key)
  res <- tryCatch(i18n$t(key), error = function(e) key)
  .strip_i18n_html(res)
}

#' Scalar-safe translation for NON-HTML contexts: setNames() name vectors,
#' sprintf templates, notification/log strings...
#'
#' The GLOBAL Translator runs in usei18n()/JS-shim mode: at UI-build time
#' (no active Shiny session) $t() wraps each translation in a
#' <span class="i18n" data-key="..."> shiny.tag so update_lang() can swap
#' static labels client-side. Correct for HTML UI nodes — fatal wherever a
#' length-1 plain string is required ("'names' attribute [6] must be the
#' same length as the vector [2]" from stats::setNames).
#'
#' Static HTML labels MUST keep calling i18n$t(); use this helper ONLY where
#' plain text is needed. In-session calls ($t with reactive domain) already
#' return raw strings and pass through unchanged.
#' Kept as alias to .tr_plain() for backward compatibility (Phase 1 callers).
i18n_plain <- function(key) .tr_plain(key)

#' Simple scalar translation helper (wrapper around session translator or fallback).
#' Returns the English translation when available and active session, otherwise French.
.tr <- function(key) {
  if (!isTRUE(I18N_AVAILABLE)) return(key)
  # Try session-specific translator first (global_data exists in server context)
  if (exists("global_data") && !is.null(global_data$i18n)) {
    res <- tryCatch(global_data$i18n$t(key), error = function(e) key)
    # Session translator may return HTML-wrapped content; strip for scalar use
    if (!is.character(res) || length(res) > 1L) return(as.character(res)[1])
    return(res)
  }
  # Fallback: use global i18n translator (for static labels, source time)
  tryCatch(i18n$t(key), error = function(e) key)
}

my_theme <- bs_theme(

  version = 5,

  bootswatch = "flatly",

  primary = "#2C3E50",

  secondary = "#18BC9C",

  "enable-gradients" = TRUE

)

# O15 (décision opérateur 2026-10-27) : les correctifs CSS passent par le
# THÈME (bs_add_rules) — une seule source de vérité de style, 0 fichier www/
# (la voie www/custom.css est écartée ; IMPORT-10 : ce fichier n'a jamais existé).
# Règles du legs V1X (V1X_BULK_IMPORT_QA_AUDIT_FIXES_PROPOSAL.md §2.4/§2.5) :
#   .bslib-value-box .showcase — l'icône décorative ne chevauche plus le texte
#     des value_box (défaut mesuré « Statut ») ;
#   .bulk-kpi-grid — grille fluide auto-ajustable des 4 KPI (hook posé dans
#     mod_import_bulk.R) ;
#   .bulk-console-scroll — confinement vertical conservateur de la console de
#     logs (max-height, jamais height) ; le confinement complet du viewport
#     reste différé (IMPORT-05 NON VÉRIFIÉ navigateur).
my_theme <- bslib::bs_add_rules(my_theme, '
  .bslib-value-box .showcase {
    opacity: 0.15 !important;
    pointer-events: none;
  }
  .bulk-kpi-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(140px, 1fr));
    gap: 12px;
  }
  .bulk-console-scroll {
    max-height: calc(100vh - 280px);
    overflow-y: auto;
  }
')



clean_mem <- function() { gc() }

# =============================================================================
# --- 3bis. F2 — INIT LOURDE DIFFÉRÉE APRÈS LA MISE À L'ÉCOUTE ----------------
# =============================================================================
# Mécanisme mesuré (O34, docs/agent/HANDOFF.md:57 passe 4-6) : « Listening »
# est imprimé AVANT le bind (shiny 1.14.0, code lu) et la fenêtre bind→listen
# (15-25 s à vide, 300-945 s sous 20+ Rscript concurrents) vit côté C de
# httpuv 1.6.17 — hors de portée d'une passe R. Ce que l'app contrôle : ne
# plus faire PRÉCÉDER le bind par ~28 s de chargement d'analyse + spawn
# workers (mesures 2026-11-06 : celldex 7.3 s, destiny 7.3 s, Seurat 5.3 s,
# SingleR 5.1 s, DESeq2 4.3 s, slingshot 4.1 s, sva 3.7 s,
# SingleCellExperiment 3.4 s, BPCells 2.5 s — un processus frais chacun ;
# plan(multisession) + 8 pids × renv ≈ 6.5 s, marker.log 211025).
#
# LA PORTE (init) : pendant TOUTE la fenêtre, les connexions TCP sont
# REFUSÉES (HANDOFF.md:57 (iii)) ; une socket qui CONNECTE = accept armé.
# L'init ne démarre donc qu'après le premier accept armé — le chargement
# lourd ne peut JAMAIS tomber À L'INTÉRIEUR de la fenêtre ni y ajouter de
# contention. NE RACCOURCIT PAS la fenêtre elle-même (atome C) : le gain est
# naissance→« Listening » ~38 s → ~10-15 s espéré + accept armé ~27 s plus
# tôt. app.R schedule la porte sur la boucle httpuv via later (delay 0).
# Garde : tests/testthat/test-deferred-boot-init.R.
.biomentat_deferred_boot_init <- function() {
  svrs <- tryCatch(httpuv::listServers(), error = function(e) list())
  # rev() : le serveur RÉEL est le dernier démarré (la sonde de port de
  # shiny .setupShinyApp est déjà stoppée et dé-registry à ce stade —
  # vérifié : listServers() renvoie .globals$servers, stopServer dé-registre).
  for (s in rev(svrs)) {
    port <- tryCatch(s$getPort(), error = function(e) NA_integer_)
    host <- tryCatch(s$getHost(), error = function(e) "127.0.0.1")
    if (is.null(port) || is.na(port)) next
    con <- suppressWarnings(tryCatch(
      socketConnection(host = host, port = port, server = FALSE,
                       open = "r+b", timeout = 1),
      error = function(e) NULL
    ))
    if (!is.null(con)) {
      close(con)  # connexion probe jetable : un connect sans données HTTP est refermé par httpuv
      .biomentat_deferred_boot_run()
      return(invisible())
    }
  }
  # accept pas encore armé : re-sonde dans 1 s. Coût ~0 ; la boucle httpuv
  # ne tourne que tant que l'app vit, le re-schedul meurt avec elle.
  later::later(.biomentat_deferred_boot_init, delay = 1)
}

.biomentat_deferred_boot_run <- function() {
  # 1) Optionnels — messages d'origine inchangés (loop optionnel pré-F2).
  for (pkg in optional_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(sprintf("Paquet optionnel absent : %s", pkg))
    }
  }
  # 2) Bioconductor — warnings d'origine inchangés (loop bioc pré-F2).
  for (pkg in bioc_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      warning(sprintf("Paquet Bioconductor manquant : %s", pkg))
    }
  }
  # 3) Packages d'analyse lourds — même sémantique require()/warning que le
  #    loop required pré-F2 (dégradation gracieuse, jamais de crash dur).
  for (pkg in deferred_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      warning(sprintf("Paquet requis manquant : %s", pkg))
    }
  }
  # 4) Flags d'analyse (<<- : environnement global — les consommateurs
  #    runtime lisent les globales ; has_deseq2 pré-déclaré FALSE au light).
  has_deseq2 <<- requireNamespace("DESeq2", quietly = TRUE)
  # 5) Spatial v3 — bloc déplacé VERBATIM du light (flags + messages) ;
  #    les requireNamespace y remplacent les FALSE du scaffolding.
  has_bpcells      <<- requireNamespace("BPCells", quietly = TRUE)
  has_spacexr      <<- requireNamespace("spacexr", quietly = TRUE)
  has_stdeconvolve <<- requireNamespace("STdeconvolve", quietly = TRUE) &&
    requireNamespace("topicmodels", quietly = TRUE) &&
    requireNamespace("slam", quietly = TRUE)
  has_leafgl       <<- requireNamespace("leafgl", quietly = TRUE)
  has_mirai        <<- requireNamespace("mirai", quietly = TRUE)
  has_rann         <<- requireNamespace("RANN", quietly = TRUE)
  # Trajectoire : Slingshot optionnel a l'echelle de l'app — le module
  # Trajectoire refuse proprement l'option "slingshot" avec un message en
  # francais si ce flag est FALSE (voir helpers_sc.R::calculate_slingshot_pseudotime()).
  has_slingshot    <<- requireNamespace("slingshot", quietly = TRUE)
  # Phase 5 : lecteur .h5ad robuste (reference de deconvolution RCTD/Label
  # Transfer + import Single-Cell) — voir helpers_io.R::load_single_cell_data().
  # Optionnel : a defaut, l'app retombe sur SeuratDisk (moins fiable pour
  # AnnData) si celui-ci est installe, sinon .h5ad est simplement indisponible.
  has_schard       <<- requireNamespace("schard", quietly = TRUE)
  # Phase 5 : requis pour lire les .parquet des exports Visium HD "binned"
  # (tissue_positions.parquet sous binned_outputs/square_0XXum/spatial/) —
  # voir helpers_io.R::load_spatial_visium_hd(), qui verifie aussi ce point
  # lui-meme avant d'importer (message clair plutot que l'erreur Seurat brute
  # "Please install arrow to read parquet files"). Souvent difficile a
  # installer depuis les sources sous Windows sans toolchain complet — si
  # aucun binaire CRAN n'est disponible pour votre version de R, essayez :
  # install.packages("arrow", repos = c("https://apache.r-universe.dev", getOption("repos")))
  has_arrow        <<- requireNamespace("arrow", quietly = TRUE)

  for (dep in c("bpcells", "spacexr", "stdeconvolve", "leafgl", "mirai", "rann", "schard", "arrow")) {
    if (!get(paste0("has_", dep))) {
      message(sprintf("[spatial] Package(s) pour '%s' non installe(s) — fonctionnalite associee indisponible tant que non installee (voir commentaire ci-dessus pour la commande d'installation).", dep))
    }
  }
  # 6) Plan future — bloc déplacé VERBATIM du light (voir le commentaire
  #    PARALLÉLISATION en amont : un seul plan, plafonné, jamais le pool
  #    mirai). Spawn des workers post-écoute, jamais dans la fenêtre.
  if (require("future", quietly = TRUE)) {
    plan(multisession, workers = min(4L, max(1L, parallel::detectCores() - 2)))
    options(future.globals.maxSize = 10000 * 1024^2)  # 10GB
  }
  # Marqueur non bufferisé pour le log de boot (boot_diagnose lit app_r.log).
  message("[F2] deferred boot init done: analysis packages attached, future plan=",
          if (require("future", quietly = TRUE)) class(future::plan())[1] else "absent",
          " workers=", if (require("future", quietly = TRUE)) future::nbrOfWorkers() else 0L)
}

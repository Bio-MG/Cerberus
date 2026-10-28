# =============================================================================
# test-mod-bulk.R — tests for modules/bulk/mod_bulk.R
# =============================================================================
# 30ᵉ incrément de la dette de conventions (2026-09-18, §2cp).
# Créé AVANT la conversion (règle 5 : test d'abord, rouge d'abord) ⇒ ce fichier
# doit ÉCHOUER sur l'assertion de classe et sur le verrou source tant que le
# `stop()` de la ligne 427 ne porte pas `class = "mod_bulk_error"`.
#
# 🔑 CLASSE `mod_bulk_error` — décision §2co.1 appliquée. `mod_bulk.R` est le
# **2ᵉ routeur parent** (« Auto-pipeline » de l'onglet Bulk) : mapping mécanique
# ⇒ `bulk_error`, nom **trop large** au même titre que `sc_error` (il cohabite
# avec `bulk_de_error`, `bulk_dose_error`, `bulk_merge_error`, … — **17**
# classes `bulk_*_error` existent, et AUCUNE n'est `bulk_error`). Branche (b) de
# `CONVENTIONS.md` §7 : la classe porte le FICHIER ⇒ **`mod_bulk_error`**.
#
# ---------------------------------------------------------------------------
# 🔴 RÉSULTAT CENTRAL DE CE LOT — « ATTEIGNABLE » n'est PAS « OBSERVABLE »
# ---------------------------------------------------------------------------
# Le site 427 est **ATTEINT à l'exécution** (prouvé ci-dessous par témoin) mais
# sa classe est **INOBSERVABLE** : le `tryCatch` ouvert L344 ferme **L528** et
# son gestionnaire (L525-528) **journalise + notifie** au lieu de **relancer**
# ⇒ il **AVALE** l'erreur. C'est le critère §2cd.2 (« ce n'est pas `tryCatch`
# qui condamne un site, c'est le GESTIONNAIRE »).
#
# ⇒ Raffinement de §2cn.1 : §2cn.1 a montré que « réactif » n'est pas
# « inobservable » (le CORPS est du R pur extractible). Ce lot ajoute la couche
# **indépendante** suivante : même un corps **prouvé atteint** peut rester
# **inobservable**, parce qu'un gestionnaire avaleur se trouve **sur le chemin
# de sortie**. ⇒ **Deux verdicts séparés** : (1) le corps est-il atteignable ?
# (2) l'erreur remonte-t-elle ? Le lot `mod_bulk.R` répond **oui** puis **non**.
#
# 🟢 TECHNIQUE NOUVELLE, MESURÉE (§2cp.2) : deux primitives du prologue exigent
# une SESSION et échouent hors app — `removeModal()` (« attempt to apply
# non-function ») et `shiny::Progress$new()` (« Can only use Progress$new()
# inside a Shiny app »). Cette seconde est appelée via `::` ⇒ **non mockable**,
# et c'est précisément le motif qui avait fait conclure « injoignable » (§2cc).
# ⇒ **`shiny::withReactiveDomain(shiny::MockShinySession$new(), …)` les
# débloque TOUTES LES DEUX.** Le corps devient donc atteignable.
#
# ⚠️ **3ᵉ verrou : `DESeq2::estimateSizeFactors()` (L383), aussi via `::`.**
# Comme `::` résout le VRAI namespace (`substitute(pkg)`), un binding local nommé
# `DESeq2` est IGNORÉ. ⇒ Le mock de `build_dds()` doit rendre un **VRAI**
# `DESeqDataSet` (`DESeq2::DESeqDataSetFromMatrix`), que `estimateSizeFactors()`
# accepte. C'est le prologue le plus lourd du chantier (~14 bouchons), mais il
# est **payé** : il prouve le témoin.
#
# ⚠️ **Le handler d'`observeEvent` est un BLOC, pas une DÉFINITION** : le 3ᵉ
# élément de l'appel est `{ … }`. Il faut donc l'**évaluer** (`eval`), **pas**
# l'appeler — c'est l'**inverse** du piège §2cn.3 (« `eval()` d'une définition
# n'est pas un appel »). Se tromper donne `could not find function "f"`.
#
# ⚠️ **C16 / §2bn sans objet** : message à **UN SEUL** argument, statique.
#
# 🔴 **PREUVE = VERROU SOURCE + TÉMOIN D'EXÉCUTION** (et non exécution de la
# classe). Le verrou source est **rendu falsifiable** : le dernier test assère
# que le gestionnaire englobant journalise + notifie **SANS `stop(`** ⇒ si un
# jour il relance, le test **échoue** et signale que le lot devient
# **prouvable à l'exécution** (§2cc, motif repris).
# =============================================================================

source_project_file("modules/bulk/mod_bulk.R")

.MBK_FILE <- "modules/bulk/mod_bulk.R"

# Fragment ASCII du message du garde (le littéral source porte des echappements
# \u00xx : on assère une tranche SANS accent pour rester insensible a l'encodage).
.MBK_MSG_ASCII <- "Aucune paire n'a pu"

# --- AST : le BLOC (3ᵉ élément) de `observeEvent(input$ap_confirm, …)` ---------
# Harnais partage `helper-ast.R` (§2dg) : ce fichier portait TROIS copies du
# parcours recursif `walk()` — les 1ʳᵉ et 2ᵉ sont remplacees ici, la 3ᵉ l'a ete
# juste en dessous (verrou source). C'etait 3 des **14** du depot.
.mbk_handler_expr <- function() ts_ast_observe_block(.MBK_FILE, "ap_confirm")

# --- AST : le GESTIONNAIRE `error = function(e) …` qui porte « Erreur pipeline »
.mbk_outer_handler <- function() ts_ast_trycatch_handler(.MBK_FILE, "Erreur pipeline")

# --- Environnement enfant ----------------------------------------------------
# `rec` enregistre les etapes (temoins) ; `dispatch_ok` sert au controle de BORNE.
.mbk_env <- function(rec, dispatch_ok = FALSE, input_extra = list()) {
  e <- new.env(parent = globalenv())
  e$.tr    <- function(s, ...) s
  e$.t_fmt <- function(fmt, ...) fmt
  # cf. R/core/io_helpers.R:47
  e$`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  e$req    <- shiny::req            # REEL (mesure §2cn.4)
  e$removeModal <- function() invisible(NULL)   # primitive d'UI
  e$showNotification <- function(msg, ...) {
    rec(paste0("notify:", paste0(msg))); invisible(NULL)
  }
  e$auto_log_rv <- function(...) { rec("log"); invisible(NULL) }
  # Helpers maison : a BOUCHONNER (la recherche retomberait sinon sur globalenv).
  e$design_saturation  <- function(meta, col) list(saturated = FALSE, n = nrow(meta), p = 1L)
  e$detect_gene_id_type <- function(x) "unknown"
  e$filter_bulk_counts  <- function(counts, ...) counts
  # ⚠️ `DESeq2::estimateSizeFactors()` (L383) est appele via `::` => NON mockable
  # => `build_dds` doit rendre un VRAI DESeqDataSet (cf. en-tete).
  e$build_dds <- function(...) {
    rec("build_dds")
    m  <- matrix(as.integer(seq_len(40)), nrow = 10,
                 dimnames = list(paste0("g", 1:10), paste0("s", 1:4)))
    cd <- data.frame(condition = factor(c("A", "A", "B", "B")))
    rownames(cd) <- colnames(m)
    DESeq2::DESeqDataSetFromMatrix(m, cd, ~1)
  }
  e$get_vst_matrix <- function(dds) { rec("get_vst_matrix"); matrix(1, 10, 4) }
  e$run_bulk_de_dispatch <- function(...) {
    rec("dispatch")
    if (dispatch_ok) {
      data.frame(gene = "g1", log2FoldChange = 1, padj = 0.01, stringsAsFactors = FALSE)
    } else {
      stop("__dispatch_ko__")
    }
  }
  e$.normalize_de_cols <- function(x, ...) x
  # M13 : chaque résultat pairwise réussi est estampillé du moteur
  # (mod_bulk.R ~L424, bulk_de_stamp_engine). BOUCHON au harnais, comme
  # build_dds/run_bulk_de_dispatch : la version réelle vit dans l'arbre du
  # paquet (tools/pkg_skeleton/R/bulk_helpers.R:841) et un load_all ici
  # rendrait le fichier OTAGE de l'état transitoire de cet arbre étranger en
  # vol (ex. drive_allowlist.R en cours de réécriture par un autre agent) ;
  # le contrat du harnais n'exige que l'identité du résultat.
  e$bulk_de_stamp_engine <- function(res, engine) res
  meta <- data.frame(condition = c("A", "A", "B", "B", "C", "C"))
  e$global_data <- list(bulk_obj = list(counts = matrix(1, 10, 6), metadata = meta))
  e$shared_rv   <- list()
  e$input <- list(ap_condition = "condition", ap_no_rep_enable = FALSE,
                  ap_no_rep_attest = FALSE, ap_no_rep_bcv = 0.4,
                  ap_map_ids = FALSE, ap_map_organism = "human",
                  ap_min_count = 10, ap_min_samples = 2,
                  ap_pairwise = TRUE, ap_engine = "edger",
                  ap_lfc = 1, ap_padj = 0.05)
  # BULK-06 : extension PUR — les options supplementaires (ex. ap_run_pathway)
  # se fusionnent ici ; les appels existants (sans input_extra) sont inchanges.
  e$input <- utils::modifyList(e$input, input_extra)
  e
}

# --- Execution : le corps est un BLOC => on l'EVALUE (jamais on ne l'appelle).
# ⚠️ `withReactiveDomain(MockShinySession)` est REQUIS : sans lui le corps
# echoue a `shiny::Progress$new()` (L337) AVANT d'atteindre le site.
.mbk_run <- function(envir) {
  err <- tryCatch({
    shiny::withReactiveDomain(
      shiny::MockShinySession$new(),
      eval(.mbk_handler_expr(), envir = envir)
    )
    NULL
  }, error = function(e) list(msg = conditionMessage(e), class = class(e)))
  err
}

# ---------------------------------------------------------------------------
# TEMOIN D'EXECUTION : le garde 427 S'EXECUTE (prouve) mais l'erreur est AVALEE
# ---------------------------------------------------------------------------
test_that("mod_bulk : le garde 427 est ATTEINT (temoin) puis AVALE", {
  skip_if_not(exists("MockShinySession", envir = asNamespace("shiny")),
              "shiny::MockShinySession indisponible")

  steps <- character(0)
  err <- .mbk_run(.mbk_env(function(s) steps <<- c(steps, s)))

  # 1) TEMOIN DE TRAVERSEE NON VACUANT : le gestionnaire L525-528 a notifie le
  #    message DU GARDE => le garde s'est bien execute (ce n'est pas une
  #    deduction, c'est son message qui ressort par la notification).
  expect_true(any(grepl(.MBK_MSG_ASCII, steps, fixed = TRUE)))
  # 2) ...et les 3 paires ont bien ete tentees avant lui.
  expect_true(sum(steps == "dispatch") >= 3L)
  # 3) 🔴 AUCUNE erreur ne s'echappe => le gestionnaire l'a AVALEE.
  expect_null(err)
})

# ---------------------------------------------------------------------------
# La classe est INOBSERVABLE : c'est la justification du verrou source
# ---------------------------------------------------------------------------
test_that("mod_bulk : la classe mod_bulk_error est INOBSERVABLE a l'execution", {
  skip_if_not(exists("MockShinySession", envir = asNamespace("shiny")),
              "shiny::MockShinySession indisponible")

  steps <- character(0)
  err <- .mbk_run(.mbk_env(function(s) steps <<- c(steps, s)))

  # La classe n'apparait dans AUCUN canal observable : ni en erreur remontee...
  expect_null(err)
  # ...ni dans la notification, qui ne porte que le MESSAGE.
  expect_false(any(grepl("mod_bulk_error", steps, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# Controle de BORNE : le garde est CONDITIONNEL (une paire reussit => pas de tir)
# ---------------------------------------------------------------------------
test_that("le garde 427 est conditionnel : une paire reussit => il ne tire pas", {
  skip_if_not(exists("MockShinySession", envir = asNamespace("shiny")),
              "shiny::MockShinySession indisponible")

  steps <- character(0)
  err <- .mbk_run(.mbk_env(function(s) steps <<- c(steps, s), dispatch_ok = TRUE))

  # Temoin de traversee : les 3 paires ont ete tentees (on est bien alle jusque-la)...
  expect_true(sum(steps == "dispatch") >= 3L)
  # ...et le garde n'a PAS tire, puisque `ok > 0`.
  expect_false(any(grepl(.MBK_MSG_ASCII, steps, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# BULK-06 (§2.10 V1X_BULK_QA_AUDIT_FIXES_PROPOSAL.md ; registre round 2,
# collector_BULK_claims.md claim BULK-06) — ROUGE d'abord.
# ---------------------------------------------------------------------------
# En mode pairwise, `res` n'est assigne QUE dans la branche non-pairwise
# (modules/bulk/mod_bulk.R ~L438-450, fermeture de branche ~L461) ; en pairwise
# les resultats vivent dans `res_i` -> shared_rv$contrasts (~L420-430). Les
# blocs pathway du tronc commun lisent pourtant `res` : GSEA ~L492
# (run_gsea_enrichment(res,...)) et ORA ~L503-504 (res$gene[...]). Avec
# `ap_run_pathway` coche en pairwise, le bloc ORA (defaut `ap_pathway_mode =
# "ora"`) leve « object 'res' not found » (EN) / « objet 'res' introuvable »
# (FR), attrape par le tryCatch EXTERNE (~L528-530) : le pipeline auto se
# termine en ERREUR — notification « Erreur pipeline: ... » — au lieu du
# « ✓ Pipeline termine » (~L521-526). (En mode GSEA le defaut est avale par le
# tryCatch interne ~L491-494 : GSEA sautee, pipeline termine — d'ou le choix
# du mode ORA ici : c'est la voie ou l'echec est OBSERVABLE.)
#
# Harnais : le bloc observeEvent(input$ap_confirm) est evalue comme pour les
# tests ci-dessus (helper-ast.R + MockShinySession, cf. .mbk_run). La branche
# pairwise REUSSIE appelle `bulk_de_stamp_engine` (M13) : BOUCHONNE dans
# .mbk_engine comme les autres helpers — PAS de load_all ici, pour garder le
# fichier HERMETIQUE a l'etat transitoire de l'arbre du paquet (etranger).
test_that("BULK-06 : pairwise + Pathway ORA => l'auto-pipeline termine SANS « Erreur pipeline: 'res' »", {
  skip_if_not(exists("MockShinySession", envir = asNamespace("shiny")),
              "shiny::MockShinySession indisponible")

  steps <- character(0)
  err <- .mbk_run(.mbk_env(function(s) steps <<- c(steps, s),
                           dispatch_ok = TRUE,
                           input_extra = list(ap_run_pathway = TRUE,
                                              ap_pathway_mode = "ora")))

  # Temoin d'ancrage : on est bien en pairwise REUSSI — 3 paires (A/B/C) tentees.
  expect_true(sum(steps == "dispatch") >= 3L)
  # Temoin anti-faux-rouge : PAS le garde 427, PAS de paire echouee — l'echec
  # observe doit etre celui du bloc pathway, pas un accident d'amont.
  expect_false(any(grepl(.MBK_MSG_ASCII, steps, fixed = TRUE)))
  # 🔴 RED (BULK-06) : le bloc ORA lit `res` jamais assigne en pairwise =>
  # AUCUNE notification « Erreur pipeline: » ne doit s'emettre. En l'etat elle
  # porte « object 'res' not found » (EN) / « objet 'res' introuvable » (FR) —
  # la sortie d'echec ci-dessous le montre mot pour mot.
  erreurs <- grep("Erreur pipeline:", steps, fixed = TRUE, value = TRUE)
  expect_length(erreurs, 0L)
  # ...et le « ✓ Pipeline termine » doit etre emis (pipeline auto complet).
  expect_true(any(grepl("Pipeline termin", steps, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# Justification EXECUTABLE du verrou source : le gestionnaire ne RELANCE pas
# ---------------------------------------------------------------------------
test_that("le gestionnaire englobant journalise + notifie SANS stop( (falsifiable)", {
  d <- paste(deparse(.mbk_outer_handler()), collapse = "\n")
  # Il rend la main en notifiant...
  expect_true(grepl("showNotification", d, fixed = TRUE))
  # ...et NE relance PAS. Si un jour il relancait, ce test echouerait et
  # signalerait que le lot devient prouvable a l'execution (§2cd.2).
  expect_false(grepl("stop(", d, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Verrou source CIBLE : le garde 427 porte bien NOTRE classe
# ---------------------------------------------------------------------------
test_that("le garde 427 porte la classe mod_bulk_error (verrou source cible)", {
  # ⚠️ La classe etant INOBSERVABLE a l'execution, la SOURCE est le seul canal
  # ou elle est verifiable — et c'est exactement ce que C10 mesure. On assere
  # donc le `stop()` du site, precisement, plutot que le seul compteur C10.
  # Harnais partage `helper-ast.R` (§2dg) : ce bloc portait sa PROPRE copie du
  # parcours recursif — la 3ᵉ de ce fichier, et l'une des 14 du depot.
  hits <- ts_ast_stop_sites(.MBK_FILE, .MBK_MSG_ASCII)

  expect_length(hits, 1L)                                   # exactement UN site
  expect_true(grepl("errorCondition", hits[[1]], fixed = TRUE))
  expect_true(grepl('class = "mod_bulk_error"', hits[[1]], fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Verrou source : mod_bulk.R ne doit plus contribuer aucun C10
# ---------------------------------------------------------------------------
test_that("mod_bulk.R ne contribue aucun signalement C10", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(ts_project_root(), "tools/check_conventions.R"), envir = e)
  before <- length(e$.REPORT$warns)
  e$check_c10_error_style(file.path(ts_project_root(), .MBK_FILE))
  w <- e$.REPORT$warns
  n <- length(w) - before
  if (n > 0L) {
    lignes <- vapply(w[(before + 1L):length(w)], function(x) x$line, integer(1))
    cat("Sites C10 restants dans mod_bulk.R :", paste(sort(lignes), collapse = ", "), "\n")
  }
  expect_equal(n, 0L)
})

# ---------------------------------------------------------------------------
# B1-E (round 5, §2.3) — Enrichissement du panneau « Résumé Pipeline Bulk »
# ---------------------------------------------------------------------------
# Le bloc `output$bulk_resume_panel` vit sous contrat affiché en tête :
# « Reads EXISTING shared_rv fields only — zero new computation ». Les pins
# ci-dessous sont des verrous SOURCE (readLines, aucun execution context) :
#   (1) les nouvelles sections (WGCNA, Signatures cellulaires, mini-matrice
#       des contrastes) sont bien présentes dans le bloc du panneau ;
#   (2) le contrat tient TOUJOURS — le bloc n'ajoute aucun appel réactif
#       calculant et n'écrit JAMAIS dans shared_rv (lecture seule) ;
#   (3) la position de l'onglet Résumé ne bouge pas (le réordonnancement
#       3g/4 de la sidebar relève de B1-D, déjà livré — hors périmètre).
# Honnêteté mesurée : n_up/n_down ne sont PAS portés comme champs stockés
# dans shared_rv$contrasts (liste nommée de data.frames DE, mod_bulk.R) ;
# la mini-matrice doit donc réutiliser la source unique EXISTANTE
# `summarize_contrasts_updown()` (bulk_helpers.R, déjà consommée par l'onglet
# Résumé Up/Down et le rapport) sur les champs déjà calculés.
.MBK_RESUME_BLOCK <- function() {
  # encoding = "UTF-8" : les libellés FR du panneau portent des accents
  # LITTÉRAUX (pas des \u00xx) — sans ce re-encodage, readLines décode les
  # octets UTF-8 via la locale C du harnais Rscript et le pin à accent rate
  # (mesuré : match FALSE sans, TRUE avec — même pattern que C8 de
  # tools/check_conventions.R).
  ln <- readLines(file.path(ts_project_root(), .MBK_FILE), warn = FALSE,
                  encoding = "UTF-8")
  i0 <- grep("Reads EXISTING shared_rv fields only", ln, fixed = TRUE)
  stopifnot(length(i0) == 1L)
  i1 <- grep("Auto-pipeline modal", ln, fixed = TRUE)
  i1 <- i1[i1 > i0][1L]
  paste(ln[i0:i1], collapse = "\n")
}

test_that("B1-E : le panneau Résumé affiche WGCNA + Signatures cellulaires (champs existants)", {
  blk <- .MBK_RESUME_BLOCK()
  # WGCNA — power choisi + nb de modules : champs DÉJÀ écrits par
  # mod_bulk_wgcna.R (wgcna_power ~L273, wgcna_modules ~L327) — le panneau
  # ne doit les lire que là, jamais les recalculer.
  expect_true(grepl("shared_rv$wgcna_power",   blk, fixed = TRUE))
  expect_true(grepl("shared_rv$wgcna_modules", blk, fixed = TRUE))
  # Signatures / ssGSEA — banque ($resource) + sets scorés : champ écrit par
  # .bulk_signatures_store (mod_bulk_signatures.R), $scores nrow = sets scorés.
  expect_true(grepl("shared_rv$signature_scores", blk, fixed = TRUE))
  # Les deux nouvelles LIGNES de la table portent des libellés .tr().
  expect_true(grepl('tr("WGCNA (power / modules)")', blk, fixed = TRUE))
  expect_true(grepl('tr("Signatures cellulaires")',  blk, fixed = TRUE))
})

test_that("B1-E : mini-matrice des contrastes pairwise dans le panneau Résumé", {
  blk <- .MBK_RESUME_BLOCK()
  # n_up/n_down dérivés par la source unique EXISTANTE (zéro nouvelle
  # computation stockée) — pas de ré-implémentation locale du comptage.
  expect_true(grepl("summarize_contrasts_updown(", blk, fixed = TRUE))
  # Section titrée en français (clé i18n existante, réutilisée du rapport).
  expect_true(grepl("Contrastes calcul\u00e9s:", blk, fixed = TRUE))
  # Colonnes n_up/n_down affichées depuis le data.frame du helper.
  expect_true(grepl("n_up",   blk, fixed = TRUE))
  expect_true(grepl("n_down", blk, fixed = TRUE))
})

test_that("B1-E : le contrat du panneau tient — zéro appel réactif, zéro écriture shared_rv", {
  blk <- .MBK_RESUME_BLOCK()
  # Le commentaire de contrat d'origine reste affiché, INTACT.
  expect_true(grepl("Reads EXISTING shared_rv fields only", blk, fixed = TRUE))
  expect_true(grepl("zero new computation", blk, fixed = TRUE))
  # Aucun appel réactif dans le bloc : le renderUI reste la SEULE expression
  # réactive du panneau (un enrichissement qui calculerait cacherait un
  # `reactive(` ici).
  expect_equal(lengths(regmatches(blk, gregexpr("reactive(", blk, fixed = TRUE))), 0L)
  # Le panneau est en LECTURE SEULE sur l'état partagé : aucune affectation.
  expect_equal(lengths(regmatches(blk, gregexpr("shared_rv$[a-z_]+ *<-", blk))), 0L)
})

test_that("B1-E : la position de l'onglet Résumé est inchangée (3g/4 relève de B1-D)", {
  ln <- readLines(file.path(ts_project_root(), .MBK_FILE), warn = FALSE,
                  encoding = "UTF-8")
  i_res   <- grep('value = "tab_resume"',      ln, fixed = TRUE)
  i_multi <- grep('value = "tab_bulk_multi"',  ln, fixed = TRUE)
  i_merge <- grep('value = "tab_bulk_merge"',  ln, fixed = TRUE)
  i_net   <- grep('value = "tab_bulk_network"', ln, fixed = TRUE)
  expect_length(i_res, 1L)
  # Résumé suivi de Multi-jeux, Fusion, Réseau PCSF — ordre inchangé.
  expect_true(i_res < i_multi && i_multi < i_merge && i_merge < i_net)
})

# =============================================================================
# B1-F (§2.4, round 5) — élargissement du périmètre du Pipeline Auto (1 clic)
# =============================================================================
# ÉTAT MESURÉ AVANT (packet d'arbitrage, §B1-F) : la modal ap_* ne porte AUCUNE
# option PCA/ssGSEA/rapport, et le bloc pathway tourne sur le SEUL contraste
# actif (commentaire de report explicite « Extension future B1-F »).
#
# ROUGE D'ABORD — épingles demandées par l'item (readLines/grep) :
#   (1) les 3 checkboxInput ap_run_pca / ap_run_ssgsea / ap_run_report dans la
#       modal, DÉFAUT FALSE (jamais d'élargissement silencieux) ;
#   (2) la boucle pathway itère sur TOUS les contrastes de shared_rv$contrasts
#       (chaque contraste consommé, plus de résultat unique res_ap) ;
#   (3) exécution : agrégation multi-contrastes avec colonne `contraste` ;
#   (4) exécution : les 3 étapes opt-in tournent, et un échec optionnel
#       tryCatch-é n'annule JAMAIS le pipeline.
# Le test hérité BULK-06 ci-dessus doit rester VERT : le chemin 1-contraste
# préserve son comportement par défaut (les étapes opt-in sont LAZY — aucune
# recherche de stub tant que la case n'est pas cochée).
# =============================================================================

test_that("B1-F : la modal porte les 3 cases opt-in, défaut FALSE (élargissement jamais silencieux)", {
  ln <- readLines(file.path(ts_project_root(), .MBK_FILE), warn = FALSE,
                  encoding = "UTF-8")
  for (opt in c("ap_run_pca", "ap_run_ssgsea", "ap_run_report")) {
    hits <- grep(sprintf('ns_m("%s")', opt), ln, fixed = TRUE)
    # La case existe, exactement une fois (seule la modal référence ns_m(...)).
    expect_equal(length(hits), 1L,
                 info = paste(opt, ": input absent de la modal"))
    # ...et c'est une checkboxInput à défaut FALSE : l'appel peut être rabattu
    # sur deux lignes, on regarde la ligne de l'input et les deux suivantes.
    zone <- paste(ln[max(1, hits[1] - 1):min(length(ln), hits[1] + 2)], collapse = " ")
    expect_true(grepl("checkboxInput", zone, fixed = TRUE),
                info = paste(opt, ": pas une checkboxInput"))
    expect_true(grepl("value = FALSE", zone, fixed = TRUE),
                info = paste(opt, ": défaut opt-in FALSE exigé"))
  }
})

test_that("B1-F : le bloc pathway itère sur TOUS les contrastes (multi-contrastes), report disparu", {
  handler <- paste(deparse(.mbk_handler_expr()), collapse = "\n")
  # (a) boucle sur l'ensemble des contrastes calculés...
  expect_true(grepl("c_ap in names(shared_rv$contrasts)", handler, fixed = TRUE),
              info = "boucle pathway multi-contrastes absente du handler ap_confirm")
  # ...qui consomme CHAQUE contraste (et non plus un résultat unique)...
  expect_true(grepl("shared_rv$contrasts[[c_ap]]", handler, fixed = TRUE),
              info = "consommation par contraste absente")
  # ...l'ancien résultat unique res_ap a disparu du handler (code + commentaire).
  expect_false(grepl("res_ap", handler, fixed = TRUE),
               info = "l'ancien résultat unique res_ap doit disparaître du handler")
  # (b) le commentaire de report explicite a disparu du fichier.
  src <- paste(readLines(file.path(ts_project_root(), .MBK_FILE), warn = FALSE,
                         encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("Extension future B1-F", src, fixed = TRUE),
               info = "commentaire de report B1-F encore présent")
})

# --- Harnais B1-F : ORA réellement appelé sur CHAQUE contraste ---------------
# .mbk_env bouchonne déjà le dispatch (1 gène) ; ici chaque contraste rend
# 12 gènes significatifs (seuil >= 10 franchi) pour que run_pathway_enrichment
# soit appelé par contraste, pas seulement sur le contraste actif.
.mbk_ora_env <- function(rec, input_extra = list()) {
  env <- .mbk_env(rec, dispatch_ok = TRUE, input_extra = input_extra)
  env$run_bulk_de_dispatch <- function(...) {
    data.frame(gene = paste0("g", 1:12), log2FoldChange = 2, padj = 0.001,
               stringsAsFactors = FALSE)
  }
  env$run_pathway_enrichment <- function(genes, organism = "human",
                                         database = "GOBP", pval_cutoff = 0.05,
                                         universe = NULL, ...) {
    data.frame(ID = paste0("PW_", database %||% "GOBP"),
               Description = "pathway t\u00e9moin", p.adjust = 0.01,
               Count = length(genes), stringsAsFactors = FALSE)
  }
  env
}

test_that("B1-F : pathway ORA multi-contrastes => table agrégée avec colonne contraste (3 paires)", {
  skip_if_not(exists("MockShinySession", envir = asNamespace("shiny")),
              "shiny::MockShinySession indisponible")

  steps <- character(0)
  env <- .mbk_ora_env(function(s) steps <<- c(steps, s),
                      input_extra = list(ap_run_pathway = TRUE,
                                         ap_pathway_mode = "ora"))
  err <- .mbk_run(env)

  pw <- env$shared_rv$pathway_results
  expect_false(is.null(pw), info = "aucune table pathway stockée")
  # 3 paires (A/B, A/C, B/C) — l'ancien code ne stockait que le contraste actif.
  expect_equal(nrow(pw), 3L, info = "une seule ligne = contraste actif seul")
  expect_true("contraste" %in% colnames(pw), info = "colonne contraste absente")
  expect_setequal(pw$contraste, c("B_vs_A", "C_vs_A", "C_vs_B"))
  expect_false(any(grepl("Erreur pipeline:", steps, fixed = TRUE)))
  expect_true(any(grepl("Pipeline termin", steps, fixed = TRUE)))
})

test_that("B1-F : les 3 étapes opt-in tournent (témoins) — PCA, ssGSEA Hallmark, rapport", {
  skip_if_not(exists("MockShinySession", envir = asNamespace("shiny")),
              "shiny::MockShinySession indisponible")

  steps <- character(0)
  rec <- function(s) steps <<- c(steps, s)
  env <- .mbk_env(rec, dispatch_ok = TRUE,
                  input_extra = list(ap_run_pca = TRUE, ap_run_ssgsea = TRUE,
                                     ap_run_report = TRUE))
  env$.tr_fn <- function(gd) function(s, ...) s
  env$plot_bulk_pca <- function(...) { rec("pca"); "PCA_PLOT" }
  env$run_signatures <- function(...) {
    rec("ssgsea")
    list(ok = TRUE, n_results = 2L,
         record = list(scores = data.frame(a = 1:2), resource = "hallmark",
                       method = "ssgsea"))
  }
  env$.bulk_signatures_store <- function(...) { rec("store_sig"); invisible(NULL) }
  env$bulk_render_report_file <- function(...) { rec("report"); "out.html" }
  env$file.copy <- function(...) { rec("copy_report"); TRUE }
  env$withProgress <- function(expr, ...) expr
  env$.find_bulk_report_template <- function() "T.Rmd"
  err <- .mbk_run(env)

  expect_null(err)
  # Témoins de traverée des 3 étapes, dans l'ordre PCA -> ssGSEA -> rapport.
  expect_true("pca" %in% steps, info = "étape PCA opt-in non exécutée")
  expect_true("ssgsea" %in% steps, info = "étape ssGSEA opt-in non exécutée")
  expect_true("store_sig" %in% steps, info = "écriture signature_scores absente")
  expect_true("report" %in% steps, info = "étape rapport opt-in non exécutée")
  expect_true("copy_report" %in% steps, info = "copie du rapport absent")
  # Le pipeline reste COMPLET et sans erreur.
  expect_false(any(grepl("Erreur pipeline:", steps, fixed = TRUE)))
  expect_true(any(grepl("Pipeline termin", steps, fixed = TRUE)))
})

test_that("B1-F : une étape optionnelle en échec n'annule JAMAIS le pipeline", {
  skip_if_not(exists("MockShinySession", envir = asNamespace("shiny")),
              "shiny::MockShinySession indisponible")

  steps <- character(0)
  env <- .mbk_env(function(s) steps <<- c(steps, s), dispatch_ok = TRUE,
                  input_extra = list(ap_run_pca = TRUE, ap_run_ssgsea = TRUE,
                                     ap_run_report = TRUE))
  env$.tr_fn <- function(gd) function(s, ...) s
  env$plot_bulk_pca <- function(...) stop("pca_ko")
  env$run_signatures <- function(...) stop("ssgsea_ko")
  env$bulk_render_report_file <- function(...) stop("report_ko")
  err <- .mbk_run(env)

  # Aucune erreur ne s'échappe : les échecs optionnels sont tryCatch-és.
  expect_null(err)
  # Le pipeline reste COMPLET — le tryCatch externe n'a PAS tiré.
  expect_false(any(grepl("Erreur pipeline:", steps, fixed = TRUE)))
  expect_true(any(grepl("Pipeline termin", steps, fixed = TRUE)))
  # ...et chaque échec est journalisé (log_fn) — témoin message par message.
  expect_true(any(grepl("pca_ko", steps, fixed = TRUE)),
              info = "échec PCA non journalisé")
  expect_true(any(grepl("ssgsea_ko", steps, fixed = TRUE)),
              info = "échec ssGSEA non journalisé")
  expect_true(any(grepl("report_ko", steps, fixed = TRUE)),
              info = "échec rapport non journalisé")
})

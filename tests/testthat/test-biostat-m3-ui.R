# =============================================================================
# test-biostat-m3-ui.R -- M3 (O27 2026-10-29) : UI accordéon Bulk DE + miroir
# lecture SC de l'assistant biostatistique — RED d'abord (contract-first)
# =============================================================================
# CONTRAT : docs/contracts/BIOSTAT_DECISION_CONTRACT.md — SPÉC GELÉE v1.0.0
# (c'est LA source ; ce test le SUIT, il ne le modifie pas). §8 row M3 :
# l'implémentation M3 atterrit (module + montage + clés i18n) ; ce freeze
# test lève le voile AVANT le code (discipline RED mesuré, motif M2 :
# test-biostat-tree.R, RED 6 FAIL avant implémentation).
#
# ARCHITECTURE FIGÉE PAR CE TEST (décisions de coordination, chacune
# documentée dans les infos des expect) :
#   * Module NOUVEAU modules/bulk_de/mod_biostat_tree.R : accordéon latéral
#     de l'onglet Bulk DE (bslib::accordion, primitives seulement, 0 CSS).
#     L'utilisateur parcourt l'arbre (descente biostat_resolve(), moteur M2),
#     voit l'orientation de chaque feuille (clé biostat.r.* via tr) et
#     confirme éventuellement (« Valider ce choix » — contrat §2 :
#     user_confirmed TRUE uniquement après ce clic).
#   * Miroir lecture SC : mod_biostat_mirror_ui/_server — rendu LECTURE SEULE
#     de la MÊME recommandation publiée par l'instance Bulk (même source :
#     global_data$biostat_decision), JAMAIS de re-dérivation (aucun appel
#     biostat_build_tree/biostat_resolve/biostat_decision_record dans le
#     corps du serveur miroir — assertion structurelle T5).
#   * Provenance (I2/I3 du contrat §5 — couche module) : la confirmation
#     produit new_provenance_entry() + provenance_append(global_data, .) ;
#     PAS de validation = PAS de trace (aucune écriture au montage, T1).
#   * i18n (§5-I7) : clés uniquement. Les clés biostat.q.* / biostat.a.* /
#     biostat.r.* portées par l'arbre EXISTENT désormais dans
#     i18n/translation.json (append SCOPÉ via tools/add_i18n_keys.R) —
#     résorbe la réserve du vérificateur M2. Realisation mécanique mesurée
#     (Rscript 2026-11-02) : dans translation.json, la clé du dictionnaire
#     EST le champ `fr` (shiny.i18n:::column_to_row sur la colonne fr) ;
#     les clés pointillées biostat.* atterrissent donc comme
#     {fr: "<clé pointillée>", en: "<texte anglais>"}. En mode fr
#     (langue == key_translation) shiny.i18n renvoie le keyword verbatim
#     (Translator$private_methods$raw_translate) : l'affichage FR d'une clé
#     biostat.* est la clé elle-même — toute autre réalisation exigerait de
#     casser le format du fichier ou l'arbre gelé M2.
#   * Pins d'arbre (leçon frontière M2 — le moteur M2 a été livré avec ses
#     pins dans le même jalon) : app.R source() le module (+1, 162 -> 163),
#     manifeste R/.sources_manifest.txt RÉGÉNÉRÉ
#     (Rscript tools/sources_manifest.R --build),
#     tests/testthat/test-app-sourcing.R re-basé 162 -> 163 avec trace
#     datée. La population pilot (tools/pkg_skeleton/) n'est PAS touchée :
#     module ≠ paquet.
#
# QU'EST ASSERTABLE HEADLESS (honnêteté du garde, motif test-asr3-ui.R) :
#   * Le rendu navigateur réel de l'accordéon n'est PAS assertable —
#     testServer couvre la logique serveur et le HTML des outputs.
#   * app.R n'est jamais sourcé par la suite (convention test-app-sourcing.R)
#     : la cible testServer est le MODULE que app.R monte ; le montage réel
#     est épinglé lexicalement (T6, motif test-asr3-ui.R §7).
#
# Ce qui est épinglé (T0..T7) :
#   T0  porte RED explicite : le fichier module existe (jamais un skip).
#   T1  surface + structure : entry points ui/server + miroir ; accordéon
#       PRÉSENT dans l'onglet Bulk DE, monté REPLIÉ PAR DÉFAUT (bslib 0.11.0
#       mesuré : open = FALSE -> class="accordion-collapse collapse" sans
#       "show" + boutons "accordion-button collapsed") ; AUCUNE écriture au
#       montage (provenance + publication vierges, orientation non rendue).
#   T2  orientation seule (O27 §12.2) : AUCUN appel de test statistique
#       depuis le module — garde lexicale sur le CODE (commentaires reliés,
#       motif .biostat_tree_code de test-biostat-tree.R).
#   T3  i18n : chaque chaîne visible a sa paire fr/en — les clés pointillées
#       portées par l'arbre (questions, réponses, raisons — collectées
#       MÉCANIQUEMENT depuis biostat_build_tree(), jamais copiées à la main)
#       existent dans i18n/translation.json, et chaque libellé UI du module
#       (clé = texte FR, convention maison) a sa paire EN.
#   T4  provenance : la confirmation produit le record §2 via
#       biostat_decision_record() puis l'append I2/I3 — entry 4F (§1),
#       UNE validation = UNE entrée ; sans confirmation, AUCUNE trace.
#   T5  miroir SC : lecture seule, MÊME source (l'instance Bulk publie, le
#       miroir rend), aucune re-dérivation (testServer + assertion
#       structurelle sur le corps du serveur miroir).
#   T6  pins : app.R source + monte le module et le miroir ; manifeste
#       régénéré ; test-app-sourcing.R re-basé 163 avec trace datée.
#   T7  0 CSS www/ (règle post-O13, motif test-asr3-ui.R §9) : primitives
#       bslib/shiny seulement, aucune injection de style, aucun fichier CSS
#       biostat dans www/.
#
# RED : tant que le module n'existe pas, chaque bloc T* échoue sur son
# premier expect explicite (jamais un skip — un skip n'est pas une porte
# rouge, motif test-biostat-tree.R:52-54). ASCII pur pour les gardes (C7) ;
# les libellés français sont écrits en échappements \uXXXX (convention du
# fichier de traduction, locale C sous Git Bash).
# =============================================================================

.biostat_m3_rel  <- "modules/bulk_de/mod_biostat_tree.R"
.biostat_m3_path <- file.path(ts_project_root(), .biostat_m3_rel)
.biostat_m3_present <- file.exists(.biostat_m3_path)
.biostat_m3_red_info <- paste(
  .biostat_m3_rel, "absent : M3 (UI accordéon Bulk DE + miroir lecture SC,",
  "O27) non livré -- contrat BIOSTAT_DECISION_CONTRACT.md §8 : l'UI suit",
  "le contrat gelé v1.0.0, le moteur M2 fournit build/resolve/record"
)

if (.biostat_m3_present) {
  source_project_file(.biostat_m3_rel)
}

# Moteur M2 (jamais modifié par M3 — interdit) + canal 4F + accesseurs d'état
# (provenance_append() consomme state_get/state_set, state.R).
source_project_file("tools/pkg_skeleton/R/biostat_tree.R")
source_project_file("tools/pkg_skeleton/R/state.R")
source_project_file("tools/pkg_skeleton/R/provenance.R")

# i18n : global.R fournit le vrai Translator à l'exécution ; les tests reçoivent
# le repli no-op si absent (motif test-asr3-ui.R:71-77 — les clés UI sont les
# chaînes françaises, donc $t() repli = clé elle-même).
if (!exists("i18n", envir = globalenv(), inherits = FALSE)) {
  assign("i18n", list(
    t                        = function(key, ...) key,
    set_translation_language = function(language) invisible(NULL),
    get_translation_language = function() "fr"
  ), envir = globalenv())
}

suppressPackageStartupMessages(library(shiny))

# --- Code du module, commentaires reliés (motif test-biostat-tree.R:81-84) ---
.biostat_m3_code <- function() {
  lines <- readLines(.biostat_m3_path, warn = FALSE)
  paste(sub("#.*$", "", lines), collapse = "\n")
}

# --- global_data de test (motif .asr3_gd) ------------------------------------
.biostat_m3_gd <- function() {
  shiny::reactiveValues(
    sc_obj = NULL, sc_obj_epoch = 0L,
    bulk_obj = NULL, bulk_obj_epoch = 0L,
    language = "fr", i18n = NULL,
    provenance = NULL,          # canal 4F (I2/I3 — vide au départ)
    biostat_decision = NULL     # publication lue par le miroir SC
  )
}

# --- Collecte MÉCANIQUE des clés i18n portées par l'arbre gelé M2 ------------
# (jamais de copie à la main : l'arbre est la source, le test le parcourt —
# motif .biostat_walk de test-biostat-tree.R, parcours agnostique).
.biostat_m3_tree_keys <- function() {
  keys <- character(0)
  walk <- function(nd) {
    for (f in c("i18n_key", "rationale_key")) {
      v <- tryCatch(nd[[f]], error = function(e) NULL)
      if (!is.null(v) && length(v) == 1L && nzchar(v)) keys <<- c(keys, v)
    }
    ai <- tryCatch(nd$answers_i18n, error = function(e) NULL)
    if (!is.null(ai)) keys <<- c(keys, as.character(ai))
    kids <- tryCatch(nd$children, error = function(e) NULL)
    if (!is.null(kids)) for (k in kids) walk(k)
    invisible(NULL)
  }
  walk(biostat_build_tree())
  sort(unique(keys))
}

# --- Clés i18n des libellés UI du module (fr = texte français, convention
#     maison « clé = texte FR » ; en = paire appendue SCOPÉEMENT) --------------
.biostat_m3_ui_keys <- list(
  c("Assistant biostatistique \u2014 orientation",
    "Biostatistics assistant \u2014 orientation"),
  c("Parcourez les questions ; l'orientation seule est affich\u00e9e \u2014 aucun test n'est lanc\u00e9.",
    "Walk the questions; orientation only is shown \u2014 no test is run."),
  c("Orientation recommand\u00e9e", "Recommended orientation"),
  c("Valider ce choix", "Validate this choice"),
  c("Recommencer", "Start over"),
  c("Choix enregistr\u00e9 dans la provenance.", "Choice recorded in the provenance."),
  c("Recommandation de l'assistant biostatistique (lecture seule)",
    "Biostatistics assistant recommendation (read-only)"),
  # AJUSTEMENT MINEUR (passage VERT 2026-11-02) : la version RED de ce libellé
  # (« Aucune recommandation validée — ... ») ne pouvait être rendue nulle part
  # sans casser l'état vide uiOutput NULL du miroir (T5 : empreinte nulle) —
  # remplacé par l'infobulle statique du CONTENEUR miroir, exacte dans les deux
  # états (vide comme rempli) ; même nombre de clés, même propriété T3.
  c("La recommandation valid\u00e9e c\u00f4t\u00e9 \u00ab Analyse Bulk RNA \u00bb appara\u00eet ici (lecture seule).",
    "The recommendation validated in the 'Bulk RNA analysis' tab appears here (read-only).")
)

# =============================================================================
# T0 — porte RED explicite (le fichier existe ; jamais un skip)
# =============================================================================
test_that("T0 RED : le fichier module M3 modules/bulk_de/mod_biostat_tree.R existe", {
  expect_true(.biostat_m3_present, info = .biostat_m3_red_info)
})

# =============================================================================
# T1 — surface + structure : accordéon rendu, replié par défaut, aucune
#      écriture au montage (contrat §5-I2 : pas de validation, pas de trace)
# =============================================================================
test_that("T1 surface : entry points + accordéon Bulk DE replié par défaut, montage sans écriture", {
  expect_true(.biostat_m3_present, info = .biostat_m3_red_info)
  if (!.biostat_m3_present) return(invisible(NULL))

  surface <- c(
    mod_biostat_tree_ui     = "accordéon latéral de l'onglet Bulk DE (O27 §12.4)",
    mod_biostat_tree_server = "parcours de l'arbre + confirmation + provenance I2/I3",
    mod_biostat_mirror_ui   = "miroir lecture SC — conteneur (O27 : miroir lecture SC)",
    mod_biostat_mirror_server = "miroir lecture SC — rendu de la publication Bulk"
  )
  for (i in seq_along(surface)) {
    expect_true(
      exists(names(surface)[i], envir = globalenv(), inherits = FALSE),
      info = paste("entry point M3 absent :", names(surface)[i],
                   "--", unname(surface)[i])
    )
  }

  # Structure UI : l'accordéon porte l'id du module et est monté REPLIÉ
  # PAR DÉFAUT (bslib 0.11.0 mesuré : open = FALSE -> aucun panel « show »,
  # boutons « accordion-button collapsed »).
  ui_html <- paste(collapse = "", as.character(mod_biostat_tree_ui("biostat_bulk")))
  expect_true(grepl('id="biostat_bulk-acc_biostat"', ui_html, fixed = TRUE),
              info = "l'accordéon biostat doit porter l'id biostat_bulk-acc_biostat")
  expect_false(grepl('class="accordion-collapse collapse show"', ui_html, fixed = TRUE),
               info = paste("un panneau de l'accordéon biostat est ouvert au",
                            "montage — le module doit monter REPLIÉ par défaut",
                            "(bslib accordion(open = FALSE), mesuré 2026-11-02)"))
  expect_true(grepl('accordion-button collapsed', ui_html, fixed = TRUE),
              info = "les boutons de l'accordéon doivent porter l'état collapsed (replié)")
  # Le miroir expose son conteneur (motif uiOutput, empreinte nulle à vide).
  m_html <- paste(collapse = "", as.character(mod_biostat_mirror_ui("biostat_sc")))
  expect_true(grepl('id="biostat_sc-mirror_content"', m_html, fixed = TRUE),
              info = "le miroir SC doit exposer le conteneur biostat_sc-mirror_content")

  # AUCUNE écriture au montage (I2) : provenance et publication vierges,
  # orientation non rendue tant que l'utilisateur n'a rien parcouru.
  gd <- .biostat_m3_gd()
  testServer(mod_biostat_tree_server,
             args = list(id = "biostat_bulk", global_data = gd), expr = {
    session$flushReact()
    expect_null(isolate(gd$provenance),
                info = paste("une entrée de provenance existe au montage —",
                             "interdit (contrat §5-I2 : pas de validation, pas",
                             "de trace)"))
    expect_null(isolate(gd$biostat_decision),
                info = paste("une recommandation est publiée au montage —",
                             "interdit (I2 ; la publication ne suit que la",
                             "confirmation explicite)"))
    expect_null(session$output$biostat_orientation_ui,
                info = "une orientation est rendue avant tout parcours — interdit")
  })
})

# =============================================================================
# T2 — orientation seule (O27 §12.2) : aucun appel de test statistique
#      depuis le module (lexical, code commentaires reliés)
# =============================================================================
test_that("T2 orientation seule : aucun appel de test statistique dans le module M3", {
  expect_true(.biostat_m3_present, info = .biostat_m3_red_info)
  if (!.biostat_m3_present) return(invisible(NULL))

  code <- .biostat_m3_code()
  # Formes d'APPEL uniquement (motif T2 du freeze M2 : une chaîne "DESeq2"
  # déclarée n'est pas un appel) — l'UI rend l'orientation, ne lance rien ;
  # la couche diagnostic (Shapiro/Levene) reste hors surface M3.
  forbidden <- c(
    "de::"                  = "alias de differential expression (de::)",
    "wilcox\\.test\\s*\\("  = "wilcox.test()",
    "t\\.test\\s*\\("       = "t.test()",
    "shapiro\\.test\\s*\\(" = "shapiro.test() (couche diagnostic, hors M3)",
    "leveneTest"            = "car::leveneTest() (couche diagnostic, hors M3)",
    "kruskal\\.test\\s*\\(" = "kruskal.test()",
    "aov\\s*\\("            = "aov()",
    "edgeR\\s*(::|\\()"     = "edgeR (paquet ou appel)",
    "DESeq2\\s*(::|\\()"    = "DESeq2 (paquet ou appel)",
    "limma\\s*(::|\\()"     = "limma (paquet ou appel)"
  )
  for (i in seq_along(forbidden)) {
    expect_false(
      grepl(names(forbidden)[i], code),
      info = paste("le module M3 lance un test statistique :", forbidden[i],
                   "-- O27 §12.2 : orientation seule, l'UI orchestre le",
                   "moteur M2, jamais un lancement")
    )
  }
})

# =============================================================================
# T3 — i18n (§5-I7) : chaque chaîne visible a sa paire fr/en ; les clés
#      biostat.* de l'arbre EXISTENT désormais (résorbe la réserve M2)
# =============================================================================
test_that("T3 i18n : clés biostat.* de l'arbre + libellés UI présents dans translation.json", {
  expect_true(.biostat_m3_present, info = .biostat_m3_red_info)
  if (!.biostat_m3_present) return(invisible(NULL))

  path <- file.path(ts_project_root(), "i18n", "translation.json")
  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  fr <- vapply(j$translation, function(x) if (is.null(x$fr)) "" else x$fr, character(1))
  en <- vapply(j$translation, function(x) if (is.null(x$en)) "" else x$en, character(1))

  # (a) clés pointillées portées par l'arbre gelé (questions biostat.q.*,
  #     réponses biostat.a.*, raisons biostat.r.*) — collectées mécaniquement.
  tree_keys <- .biostat_m3_tree_keys()
  expect_true(length(tree_keys) >= 13L,
              info = "l'arbre gelé doit porter au moins les 13 clés biostat.r.* des feuilles")
  expect_true(all(grepl("^biostat\\.(q|a|r)\\.", tree_keys)),
              info = paste("clé hors namespaces biostat.q/a/r dans l'arbre :",
                           paste(grep("^biostat\\.(q|a|r)\\.", tree_keys,
                                      invert = TRUE, value = TRUE), collapse = ", ")))
  for (k in tree_keys) {
    idx <- match(k, fr)
    expect_false(is.na(idx),
                 info = paste("clé biostat absente de translation.json (réserve",
                              "vérificateur M2, contrat §5-I7) :", k))
    if (!is.na(idx)) {
      expect_true(nzchar(en[[idx]]),
                  info = paste("paire EN absente ou vide pour la clé biostat :", k))
    }
  }
  # les 13 raisons de feuilles — enum §3, jamais de texte brut (I7)
  r_keys <- grep("^biostat\\.r\\.", tree_keys, value = TRUE)
  expect_true(length(r_keys) >= 13L,
              info = paste("les 13 feuilles method_id du contrat §3 doivent",
                           "porter une clé biostat.r.* (contrat §2 :", paste(r_keys, collapse = ", "), ")"))

  # (b) libellés UI du module (clé = texte FR, convention maison) — paire EN.
  for (k in .biostat_m3_ui_keys) {
    idx <- match(k[1], fr)
    expect_false(is.na(idx),
                 info = paste("libellé UI M3 absent de translation.json :", k[1]))
    if (!is.na(idx)) {
      expect_identical(en[[idx]], k[2],
                       info = paste("paire EN absente ou divergente pour :", k[1]))
    }
  }
})

# =============================================================================
# T4 — provenance (I2/I3, contrat §1/§2/§5) : la confirmation produit le
#      record §2 via biostat_decision_record() puis l'append 4F
# =============================================================================
test_that("T4 provenance : une validation = une entrée 4F ; sans validation, aucune trace", {
  expect_true(.biostat_m3_present, info = .biostat_m3_red_info)
  if (!.biostat_m3_present) return(invisible(NULL))

  gd <- .biostat_m3_gd()
  # (a) I2 — parcours SANS confirmation : aucune entrée de provenance.
  testServer(mod_biostat_tree_server,
             args = list(id = "biostat_bulk", global_data = gd), expr = {
    session$setInputs(biostat_choice = "counts");      session$flushReact()
    session$setInputs(biostat_choice = "ok");          session$flushReact()
    session$setInputs(biostat_choice = "three_plus");  session$flushReact()
    session$setInputs(biostat_choice = "confirm");     session$flushReact()
    # l'orientation de la feuille est rendue (clé biostat.r.* via tr — repli
    # no-op du test : la clé elle-même)
    orient <- paste(collapse = "", as.character(session$output$biostat_orientation_ui))
    expect_true(grepl("bulk_deseq2_lrt", orient, fixed = TRUE),
                info = "l'orientation rendue ne porte pas le method_id de la feuille")
    expect_true(grepl("biostat.r.lrt", orient, fixed = TRUE),
                info = "l'orientation rendue ne porte pas la clé biostat.r.* de la feuille (I7 : jamais de texte brut)")
    expect_null(isolate(gd$provenance),
                info = paste("parcours sans confirmation a écrit une entrée de",
                             "provenance — interdit (contrat §5-I2 : pas de",
                             "validation, pas de trace)"))
  })
  expect_null(isolate(gd$biostat_decision),
              info = "parcours sans confirmation a publié une recommandation — interdit (I2)")

  # (b) I3 — UNE validation = UNE entrée : record §2 + append 4F (§1).
  gd2 <- .biostat_m3_gd()
  testServer(mod_biostat_tree_server,
             args = list(id = "biostat_bulk", global_data = gd2), expr = {
    session$setInputs(biostat_choice = "counts");      session$flushReact()
    session$setInputs(biostat_choice = "ok");          session$flushReact()
    session$setInputs(biostat_choice = "three_plus");  session$flushReact()
    session$setInputs(biostat_choice = "confirm");     session$flushReact()
    session$setInputs(biostat_confirm = 1L);           session$flushReact()
    expect_false(is.null(isolate(gd2$provenance)),
                 info = "la confirmation n'a produit AUCUNE entrée de provenance — interdit (contrat §5-I3)")
    expect_identical(length(isolate(gd2$provenance)), 1L,
                     info = "une validation doit produire EXACTEMENT une entrée (I3)")
    entry <- isolate(gd2$provenance[[1]])
    expect_identical(entry$analysis_id, "biostat_decision",
                     info = "contrat §1 : analysis_id constant 'biostat_decision'")
    expect_identical(entry$method, "decision_tree_v1.0.0",
                     info = "contrat §1 : method = 'decision_tree_v' ++ biostat_tree_version()")
    # contrats §1 : parameters = liste plate aux 6 clés gelées — PAS de info=
    # sur expect_setequal (testthat verrouillé du dépôt, motif
    # test-biostat-tree.R:273-275) ; le verdict imprime les deux vecteurs.
    expect_setequal(
      names(entry$parameters),
      c("node_path", "method_id", "engine", "rationale", "diagnostics", "prefilled")
    )
    rec <- isolate(gd2$biostat_decision)
    expect_false(is.null(rec), info = "la confirmation n'a pas publié le record §2 pour le miroir SC")
    expect_identical(rec$version, "1.0.0",
                     info = "contrat §2 : version egale a biostat_tree_version()")
    expect_identical(rec$node_path[[1]], "root",
                     info = "contrat §2 : premier élément de node_path toujours 'root'")
    expect_identical(rec$node_path[[length(rec$node_path)]], rec$method_id,
                     info = "contrat §2 : dernier élément de node_path = feuille method_id")
    expect_true(all(grepl("^biostat\\.r\\.", rec$rationale_keys)),
                info = "contrat §2/§5-I7 : rationale_keys = clés biostat.r.*, jamais de texte brut")
    expect_true(isTRUE(rec$user_confirmed),
                info = "contrat §2 : user_confirmed TRUE uniquement après clic 'Valider ce choix'")
    expect_identical(rec$diagnostics, list(),
                     info = "contrat §5-I4 : diagnostics vide pour toute feuille bulk_*")
    status <- paste(collapse = "", as.character(session$output$biostat_status_ui))
    expect_true(grepl("Choix enregistr\u00e9 dans la provenance.", status, fixed = TRUE),
                info = "la ligne d'état ne confirme pas l'enregistrement en provenance")
  })
})

# =============================================================================
# T5 — miroir SC : lecture seule, MÊME source, aucune re-dérivation
# =============================================================================
test_that("T5 miroir SC : rend la publication Bulk verbatim, sans bouton ni re-dérivation", {
  expect_true(.biostat_m3_present, info = .biostat_m3_red_info)
  if (!.biostat_m3_present) return(invisible(NULL))

  gd <- .biostat_m3_gd()
  # Instance Bulk : produire la publication (même mécanique que T4b).
  testServer(mod_biostat_tree_server,
             args = list(id = "biostat_bulk", global_data = gd), expr = {
    session$setInputs(biostat_choice = "counts");      session$flushReact()
    session$setInputs(biostat_choice = "ok");          session$flushReact()
    session$setInputs(biostat_choice = "three_plus");  session$flushReact()
    session$setInputs(biostat_choice = "confirm");     session$flushReact()
    session$setInputs(biostat_confirm = 1L);           session$flushReact()
  })
  rec <- isolate(gd$biostat_decision)
  expect_false(is.null(rec), info = "fixture : la publication Bulk doit exister avant le miroir")

  # Miroir : rend la MÊME recommandation (même source : global_data), SANS
  # action et SOUT recalcul.
  testServer(mod_biostat_mirror_server,
             args = list(id = "biostat_sc", global_data = gd), expr = {
    session$flushReact()
    html <- paste(collapse = "", as.character(session$output$mirror_content))
    expect_true(grepl(rec$method_id, html, fixed = TRUE),
                info = "le miroir SC ne rend pas le method_id de la publication Bulk (même source exigée)")
    expect_true(grepl(rec$rationale_keys[[1]], html, fixed = TRUE),
                info = "le miroir SC ne rend pas la clé biostat.r.* de la publication (re-dérivation ou texte brut)")
    expect_false(grepl("biostat_confirm", html, fixed = TRUE),
                 info = "le miroir SC expose une action de confirmation — lecture seule exigée (O27 : miroir lecture)")
    expect_false(grepl("<button", html, fixed = TRUE),
                 info = "le miroir SC rend un bouton — lecture seule exigée")
  })

  # Assertion structurelle : le corps du serveur miroir ne dérive RIEN —
  # aucun appel moteur M2, il lit uniquement la publication (même source).
  mbody <- paste(deparse(body(mod_biostat_mirror_server)), collapse = "\n")
  expect_false(
    grepl("biostat_build_tree|biostat_resolve|biostat_decision_record", mbody),
    info = paste("le serveur miroir appelle le moteur M2 — re-dérivation",
                 "interdite (O27 : pas de re-dérivation, le MÊME moteur M2",
                 "a produit la publication côté Bulk)"))
  expect_true(
    grepl("biostat_decision", mbody),
    info = "le serveur miroir ne lit pas la publication global_data$biostat_decision (même source exigée)")

  # Miroir à vide : conteneur vide (motif uiOutput NULL = empreinte nulle,
  # motif mod_session_checkpoint.R).
  gd_empty <- .biostat_m3_gd()
  testServer(mod_biostat_mirror_server,
             args = list(id = "biostat_sc", global_data = gd_empty), expr = {
    session$flushReact()
    expect_null(session$output$mirror_content,
                info = "le miroir SC rend quelque chose sans publication — empreinte nulle exigée")
  })
})

# =============================================================================
# T6 — pins d'arbre (leçon frontière M2) : app.R + manifeste + re-basage 163
# =============================================================================
test_that("T6 pins : app.R source et monte le module/miroir ; manifeste régénéré ; app-sourcing 163 daté", {
  app_lines <- readLines(file.path(ts_project_root(), "app.R"), warn = FALSE)
  app_src <- paste(app_lines, collapse = "\n")
  expect_true(any(grepl('source\\("modules/bulk_de/mod_biostat_tree\\.R"\\)', app_lines, perl = TRUE)),
              info = "app.R doit sourcer modules/bulk_de/mod_biostat_tree.R (pin d'arbre, leçon frontière M2)")
  expect_true(grepl('mod_biostat_tree_ui\\("biostat_bulk"\\)', app_src),
              info = "l'accordéon biostat n'est pas monté dans l'onglet Bulk DE (nav_panel 'Analyse Bulk RNA')")
  # RE-BASAGE MÉCANIQUE M4 (2026-11-02, décision coordination) : le montage
  # gagne shared_rv = isolate(global_data$bulk_shared_rv) (pont bulk_shared_rv
  # publié par mod_bulk_server, lecture seule I1 — jalon M4 ; isolate()
  # obligatoire, la lecture nue au montage tue la session réelle, mesuré) ;
  # épingle mise à jour sur la ligne réelle.
  expect_true(grepl(paste0('mod_biostat_tree_server\\("biostat_bulk", global_data,\\s*',
                           'shared_rv = isolate\\(global_data\\$bulk_shared_rv\\)\\)'), app_src),
              info = "le serveur du module biostat n'est pas monté dans server()")
  expect_true(grepl('mod_biostat_mirror_ui\\("biostat_sc"\\)', app_src),
              info = "le miroir lecture SC n'est pas monté dans l'onglet Single-Cell")
  expect_true(grepl('mod_biostat_mirror_server\\("biostat_sc", global_data\\)', app_src),
              info = "le serveur miroir SC n'est pas monté dans server()")

  manifest <- readLines(file.path(ts_project_root(), "R", ".sources_manifest.txt"),
                        warn = FALSE)
  expect_true("modules/bulk_de/mod_biostat_tree.R" %in% manifest,
              info = paste("le module M3 est absent du manifeste R/.sources_manifest.txt —",
                           "régénérer : Rscript tools/sources_manifest.R --build"))

  asr_path <- file.path(ts_project_root(), "tests", "testthat", "test-app-sourcing.R")
  asr_src <- paste(readLines(asr_path, warn = FALSE), collapse = "\n")
  # RE-BASAGE MÉCANIQUE (2026-11-02) : épingle laissée PÉRIMÉE par le
  # re-basage SPMV-1 163 -> 164 (commit 715438e a déplacé le compte sans
  # re-baser cette ligne — dette ROUGE mesurée à HEAD avant le jalon M4,
  # leçon frontière) ; valeur MESURÉE au HEAD : 164L (test-app-sourcing.R:190).
  expect_true(grepl("164L", asr_src, fixed = TRUE),
              info = "test-app-sourcing.R doit porter le compte 164L (re-basage SPMV-1 mesuré)")
  expect_true(grepl("162 -> 163", asr_src, fixed = TRUE),
              info = "le re-basage 162 -> 163 doit être documenté dans test-app-sourcing.R (motif M2)")
  expect_true(grepl("2026-11", asr_src),
              info = "le re-basage doit porter une trace datée (motif des re-basages précédents)")
  expect_true(grepl("mod_biostat_tree", asr_src),
              info = "le re-basage doit nommer le module biostat M3")
})

# =============================================================================
# T7 — 0 CSS www/ (règle post-O13, motif test-asr3-ui.R §9) : primitives
#      bslib/shiny seulement
# =============================================================================
test_that("T7 M3 introduces no CSS in www/ and no style injection", {
  expect_true(.biostat_m3_present, info = .biostat_m3_red_info)
  if (!.biostat_m3_present) return(invisible(NULL))

  mod_src <- paste(readLines(.biostat_m3_path, warn = FALSE), collapse = "\n")
  expect_false(grepl("tags\\$style|includeCSS|tags\\$head", mod_src),
               info = "le module M3 injecte du style — interdit (0 CSS, primitives bslib/shiny seulement)")
  www <- file.path(ts_project_root(), "www")
  if (dir.exists(www)) {
    css <- list.files(www, pattern = "css$", recursive = TRUE, ignore.case = TRUE, full.names = FALSE)
    expect_equal(length(css[grep("biostat", css, ignore.case = TRUE)]), 0L,
                 info = "un fichier CSS biostat est apparu dans www/ — interdit (règle post-O13)")
  }
})

# =============================================================================
# test-biostat-m4-wiring.R -- M4 (O27 2026-10-29) : couture de l'assistant
# biostatistique avec l'etat bulk partage (prefill + moteur declare), RED d'abord
# =============================================================================
# CONTRAT : docs/contracts/BIOSTAT_DECISION_CONTRACT.md -- SPÉC GELÉE v1.0.0
# (c'est LA source ; ce test le SUIT, il ne le modifie pas). Proposition mere :
# docs/proposals/BIOSTAT_DECISION_TREE_PROPOSAL.md §3 (lectures autorisees :
# input$de_engine pour la pre-selection du noeud moteur -- I1 : conseiller,
# jamais selectionner) et §11 M4 (provenance + 4 fichiers test).
#
# CE QUE CE FICHIER EPINGLE (couture, testServer -- motif test-biostat-m3-ui.R)
#
#   W0  signature : mod_biostat_tree_server(id, global_data, shared_rv = NULL,
#       ...) ; la couture PUBLIE shared_rv$declared_de_engine <- input$de_engine
#       (mod_bulk_de_engine.R, observe existant des miroirs) et le module
#       CONSOMME le prefill (biostat_prefill_from_metadata).
#   W1  publication behaviorale : .de_engine_server publie la valeur courante
#       de de_engine vers shared_rv$declared_de_engine (champ gelé par CE
#       jalon) et suit les changements de l'utilisateur.
#   W2  prefill consomme : deduit groups depuis shared_rv$active_condition_col,
#       PRE-SELECTIONNE la reponse deduite a la question groupes (HTML :
#       value="..." checked), engine bulk_* = declaration lue AU MOMENT DU
#       CLIC, prefilled = valeur deduite (mÊme contredit), warnings §1 =
#       prefill_groups_overridden quand la reponse contredit la deduction.
#   W3  branche continue : aucune pre-selection sur la question paired (le
#       câblage M4 passe 2 args au moteur : paired n'est pas deduit cote
#       module -- reserve documentee), engine non bulk = "" (contrat §2),
#       reponse en accord avec la deduction -> AUCUN warning.
#   W4  repli : shared_rv$declared_de_engine absent/vide -> engine retombe sur
#       l'argument existant ("" par defaut, M3) ; presélection ACCEPTée ->
#       aucun warning.
#   W5  I1 structurel + lecture seule : le module n'ecrit JAMAIS vers
#       input$de_engine ni via update*Input ; il ne fait que LIRE
#       shared_rv$declared_de_engine (la publication reste inchangee apres
#       confirmation).
#   W6  M3 préservé quand shared_rv est OMIS : aucune pre-selection (mÊme avec
#       bulk_obj present), record aux defauts M3 list(groups="two",
#       paired=FALSE), engine = argument, warnings vides.
#
# RED : tant que la couture M4 n'existe pas, chaque bloc W* echoue sur son
# premier expect explicite (jamais un skip -- motif test-biostat-tree.R:52-54).
# ASCII pur pour les gardes (C7) ; testthat verrouillé : pas de info= sur
# expect_setequal (motif test-biostat-m3-ui.R:377-379).
# =============================================================================

# Moteur M2+M4 + canal 4F + etat partage + operateurs + module + couture engine
source_project_file("tools/pkg_skeleton/R/biostat_tree.R")
source_project_file("tools/pkg_skeleton/R/state.R")
source_project_file("tools/pkg_skeleton/R/provenance.R")
source_project_file("tools/pkg_skeleton/R/io_helpers.R")       # %||% canonique
source_project_file("modules/bulk_de/mod_biostat_tree.R")
source_project_file("modules/bulk_de/mod_bulk_de_engine.R")

suppressPackageStartupMessages(library(shiny))

# Stubs global.R (jamais sourcé en test ; .de_engine_server les consulte de
# facon paresseuse). assign() dans globalenv OBLIGATOIRE : un fichier de test
# testthat s'execute dans son propre environment, PAS dans globalenv, alors
# que les fonctions sourcees via source_project_file() vivent en globalenv
# (motif test-biostat-m3-ui.R:110-116 pour i18n ; mesure RED 2026-11-02 :
# « object 'has_limma' not found » sans assign).
if (!exists(".strip_i18n_html", envir = globalenv(), inherits = FALSE)) {
  assign(".strip_i18n_html", function(res) res, envir = globalenv())
}
if (!exists(".t_fmt", envir = globalenv(), inherits = FALSE)) {
  assign(".t_fmt", function(template, ...) {
    txt <- template
    args <- list(...)
    for (i in seq_along(args)) txt <- sub("\\{[^}]*\\}", as.character(args[[i]]), txt)
    txt
  }, envir = globalenv())
}
if (!exists("has_limma", envir = globalenv(), inherits = FALSE)) {
  assign("has_limma", FALSE, envir = globalenv())
}

# --- Code des fichiers modules, commentaires relies (motif M3 T2/T5) ---------
.m4w_mod_code <- function() {
  lines <- readLines(file.path(ts_project_root(), "modules/bulk_de/mod_biostat_tree.R"),
                     warn = FALSE)
  paste(sub("#.*$", "", lines), collapse = "\n")
}
.m4w_eng_code <- function() {
  lines <- readLines(file.path(ts_project_root(), "modules/bulk_de/mod_bulk_de_engine.R"),
                     warn = FALSE)
  paste(sub("#.*$", "", lines), collapse = "\n")
}

# --- Fixtures ----------------------------------------------------------------
# metadata 3 niveaux + sujets dupliques a travers les conditions (paired TRUE
# au niveau MOTEUR si subject_col etait passe -- le module, lui, passe 2 args).
.m4w_meta3 <- function() {
  data.frame(condition = c("A", "A", "B", "B", "C"),
             subject   = c("s1", "s2", "s1", "s2", "s3"),
             stringsAsFactors = FALSE)
}
.m4w_meta2 <- function() {
  data.frame(condition = c("A", "A", "B", "B"),
             subject   = c("s1", "s2", "s1", "s2"),
             stringsAsFactors = FALSE)
}
.m4w_gd <- function(meta) {
  shiny::reactiveValues(
    sc_obj = NULL, sc_obj_epoch = 0L,
    bulk_obj = list(counts = matrix(0L, nrow = 2L, ncol = nrow(meta)),
                    metadata = meta),
    bulk_obj_epoch = 0L,
    language = "fr", i18n = NULL,
    provenance = NULL, biostat_decision = NULL)
}
.m4w_shared <- function(active_col = "condition", declared = NULL) {
  shared <- create_bulk_shared_state()
  shared$active_condition_col <- active_col
  if (!is.null(declared)) shared$declared_de_engine <- declared
  shared
}
.m4w_html_of <- function(out) paste(collapse = "", as.character(out))

# =============================================================================
# W0 -- signature + pose de la couture (portes RED explicites)
# =============================================================================
test_that("W0 signature M4 : shared_rv = NULL par defaut ; couture publiee et consommee", {
  expect_true(all(c("id", "global_data", "shared_rv") %in%
                    names(formals(mod_biostat_tree_server))),
              info = paste("la signature du module doit devenir (id, global_data,",
                           "shared_rv = NULL, ...) -- defaut NULL = comportement",
                           "M3 preserved (jalon M4)"))
  expect_true(is.null(formals(mod_biostat_tree_server)$shared_rv),
              info = "shared_rv doit valoir NULL par defaut (M3 byte-pour-byte quand omis)")
  expect_true(grepl("shared_rv\\$declared_de_engine\\s*<-\\s*input\\$de_engine",
                    .m4w_eng_code()),
              info = paste("la couture n'est pas posee : mod_bulk_de_engine.R doit",
                           "publier shared_rv$declared_de_engine <- input$de_engine",
                           "dans l'observe EXISTANT des miroirs (1 ligne, jalon M4)"))
  expect_true(grepl("biostat_prefill_from_metadata", .m4w_mod_code()),
              info = "le module M4 ne consomme pas le prefill (biostat_prefill_from_metadata absent du code)")
  expect_true(grepl("declared_de_engine", .m4w_mod_code()),
              info = "le module M4 ne lit pas la declaration du moteur (shared_rv$declared_de_engine absent du code)")
})

# =============================================================================
# W1 -- publication behaviorale : de_engine -> shared_rv$declared_de_engine
# =============================================================================
test_that("W1 couture publiee : .de_engine_server publie declared_de_engine et suit l'utilisateur", {
  shared <- create_bulk_shared_state()
  gd     <- .m4w_gd(.m4w_meta2())
  # testthat verrouillé : testServer refuse des args supplementaires sur une
  # fonction serveur simple (« Arguments were provided to a server function »,
  # mesure RED 2026-11-02) -- la fixture passe par la closure, pas par args=.
  .m4w_engine_harness <- function(input, output, session) {
    .de_engine_server(input, output, session, function(x) x, gd, shared)
  }
  testServer(.m4w_engine_harness, expr = {
    session$setInputs(de_engine = "edger")
    session$flushReact()
    expect_identical(isolate(shared$declared_de_engine), "edger",
                     info = paste("la couture M4 n'est pas posee : shared_rv$declared_de_engine",
                                  "doit valoir la declaration courante de l'utilisateur (edger)"))
    session$setInputs(de_engine = "deseq2")
    session$flushReact()
    expect_identical(isolate(shared$declared_de_engine), "deseq2",
                     info = "la publication doit SUIVRE les changements du selecteur de_engine")
  })
})

# =============================================================================
# W2 -- prefill consomme : presélection visible, engine declare au clic,
#       prefilled déduit, warning de contredit
# =============================================================================
test_that("W2 module M4 : presélection déduite, engine bulk_* = déclaration, warning de contredit", {
  gd     <- .m4w_gd(.m4w_meta3())          # 3 niveaux -> groups = three_plus
  shared <- .m4w_shared("condition", "edger")
  testServer(mod_biostat_tree_server,
             args = list(id = "biostat_bulk", global_data = gd, shared_rv = shared),
             expr = {
    session$flushReact()
    # I2 : le prefill est une LECTURE -- aucune trace au montage
    expect_null(isolate(gd$provenance),
                info = "le prefill a écrit une entrée de provenance au montage -- interdit (I2)")
    expect_null(isolate(gd$biostat_decision),
                info = "le prefill a publié une recommandation au montage -- interdit (I2)")

    # Question racine : aucune présélection (le noeud ne porte pas de réponse
    # groups) -- la déduction ne pré-sélectionne QUE les noeuds pertinents.
    root_html <- .m4w_html_of(session$output$biostat_walk_ui)
    expect_false(grepl("checked", root_html, fixed = TRUE),
                 info = "la question racine est présélectionnée -- la déduction ne doit viser que les questions groupes")

    # Descente vers la question groupes (root -> counts -> réplicats -> ok ->
    # groups_n, arbre gelé M2) : la réponse déduite est VISIBLE (présélection,
    # contrat proposition §3) mais reste MODIFIABLE.
    session$setInputs(biostat_choice = "counts"); session$flushReact()
    session$setInputs(biostat_choice = "ok");     session$flushReact()
    g_html <- .m4w_html_of(session$output$biostat_walk_ui)
    expect_true(grepl('value="three_plus" checked', g_html, fixed = TRUE),
                info = paste("la réponse déduite (three_plus) n'est pas présélectionnée",
                             "à la question groupes -- prefill M4 non consommé"))
    expect_false(grepl('value="two" checked', g_html, fixed = TRUE),
                 info = "les deux réponses sont présélectionnées à la fois -- radio incohérent")

    # L'utilisateur CONTREDIT la déduction : two (-> covariates -> cov_none ->
    # deseq2 -> confirm)
    session$setInputs(biostat_choice = "two");     session$flushReact()
    session$setInputs(biostat_choice = "none");    session$flushReact()
    session$setInputs(biostat_choice = "deseq2");  session$flushReact()
    orient_html <- .m4w_html_of(session$output$biostat_orientation_ui)
    expect_true(grepl("bulk_deseq2_wald", orient_html, fixed = TRUE),
                info = "fixture : la feuille bulk_deseq2_wald doit être atteinte avant confirmation")

    session$setInputs(biostat_confirm = 1L); session$flushReact()
    rec <- isolate(gd$biostat_decision)
    expect_false(is.null(rec), info = "la confirmation n'a pas publié le record §2")
    # engine = la déclaration lue AU MOMENT DU CLIC (feuille bulk_*)
    expect_identical(rec$engine, "edger",
                     info = paste("le record doit porter le moteur DÉCLARÉ (shared_rv$declared_de_engine,",
                                  "lu au clic) pour une feuille bulk_* -- contrat §2"))
    # prefilled = valeur DÉDUITE (three_plus), même contredit par l'utilisateur
    expect_identical(rec$prefilled, list(groups = "three_plus", paired = FALSE),
                     info = paste("le record doit porter la valeur DÉDUITE (three_plus) même",
                                  "quand la réponse de l'utilisateur la contredit -- contrat §2"))
    entry <- isolate(gd$provenance[[1]])
    expect_true("prefill_groups_overridden" %in% entry$warnings,
                info = paste("la réponse contredit la valeur pré-remplie : le warning §1",
                             "'prefill_groups_overridden' doit figurer dans entry$warnings"))
    expect_identical(isolate(shared$declared_de_engine), "edger",
                     info = "la confirmation a MUTÉ la publication du moteur -- le module ne fait que la LIRE (I1)")
  })
})

# =============================================================================
# W3 -- branche continue : paired jamais présélectionné (câblage 2 args),
#       engine non bulk = "", accord avec la déduction -> aucun warning
# =============================================================================
test_that("W3 branche continue : pas de présélection paired, engine '', aucun warning en accord", {
  gd     <- .m4w_gd(.m4w_meta2())          # 2 niveaux -> groups = two ; sujets dupliqués
  shared <- .m4w_shared("condition", "edger")
  testServer(mod_biostat_tree_server,
             args = list(id = "biostat_bulk", global_data = gd, shared_rv = shared),
             expr = {
    session$flushReact()
    session$setInputs(biostat_choice = "continuous");   session$flushReact()
    # question groups_c : la déduction two est présélectionnée (2 niveaux)
    g_html <- .m4w_html_of(session$output$biostat_walk_ui)
    expect_true(grepl('value="two" checked', g_html, fixed = TRUE),
                info = "la déduction two (2 niveaux) doit être présélectionnée sur la branche continue aussi")
    session$setInputs(biostat_choice = "two");          session$flushReact()
    # question paired : le câblage M4 passe 2 args au moteur (paired non déduit
    # côté module) -> AUCUNE présélection, comme M3 (reserve documentée)
    p_html <- .m4w_html_of(session$output$biostat_walk_ui)
    expect_false(grepl("checked", p_html, fixed = TRUE),
                 info = paste("la question paired est présélectionnée alors que le câblage",
                              "M4 ne déduit pas paired (2 args) -- incohérent avec la déduction"))
    session$setInputs(biostat_choice = "independent");  session$flushReact()
    session$setInputs(biostat_choice = "ok");           session$flushReact()
    session$setInputs(biostat_choice = "equal");        session$flushReact()
    orient_html <- .m4w_html_of(session$output$biostat_orientation_ui)
    expect_true(grepl("cont_ttest", orient_html, fixed = TRUE),
                info = "fixture : la feuille cont_ttest doit être atteinte avant confirmation")
    session$setInputs(biostat_confirm = 1L); session$flushReact()
    rec <- isolate(gd$biostat_decision)
    expect_false(is.null(rec), info = "la confirmation n'a pas publié le record §2")
    # feuille NON bulk : « chaîne vide sinon » (contrat §2), même moteur déclaré
    expect_identical(rec$engine, "",
                     info = "feuille non bulk_* : engine doit être la chaîne vide (contrat §2)")
    expect_identical(rec$prefilled, list(groups = "two", paired = FALSE),
                     info = "la valeur déduite (two) doit figurer dans le record")
    entry <- isolate(gd$provenance[[1]])
    expect_identical(entry$warnings, character(0),
                     info = paste("la réponse EST la valeur pré-remplie : aucun warning",
                                  "de contredit ne doit être émis"))
  })
})

# =============================================================================
# W4 -- repli : declaration absente ou vide -> argument engine existant (M3) ;
#       présélection ACCEPTée -> aucun warning
# =============================================================================
test_that("W4 repli engine : declared absent/vide -> argument M3 ; acceptation -> 0 warning", {
  # (a) declared_de_engine ABSENT (schema purge / session fraîche)
  gd1     <- .m4w_gd(.m4w_meta3())
  shared1 <- .m4w_shared("condition", NULL)
  testServer(mod_biostat_tree_server,
             args = list(id = "biostat_bulk", global_data = gd1, shared_rv = shared1),
             expr = {
    session$flushReact()
    session$setInputs(biostat_choice = "counts");     session$flushReact()
    session$setInputs(biostat_choice = "ok");         session$flushReact()
    # l'utilisateur ACCEPTE la présélection déduite (three_plus)
    session$setInputs(biostat_choice = "three_plus"); session$flushReact()
    session$setInputs(biostat_choice = "confirm");    session$flushReact()
    session$setInputs(biostat_confirm = 1L);          session$flushReact()
    rec <- isolate(gd1$biostat_decision)
    expect_false(is.null(rec), info = "la confirmation n'a pas publié le record §2")
    expect_identical(rec$engine, "",
                     info = paste("declaration absente : engine doit retomber sur l'argument",
                                  "existant (defaut '', M3) pour cette feuille bulk_*"))
    expect_identical(rec$prefilled, list(groups = "three_plus", paired = FALSE),
                     info = "la valeur déduite doit porter dans le record même sans déclaration de moteur")
    entry <- isolate(gd1$provenance[[1]])
    expect_identical(entry$warnings, character(0),
                     info = "présélection ACCEPTée : aucun warning de contredit")
  })
  # (b) declared_de_engine VIDE ("")
  gd2     <- .m4w_gd(.m4w_meta3())
  shared2 <- .m4w_shared("condition", "")
  testServer(mod_biostat_tree_server,
             args = list(id = "biostat_bulk", global_data = gd2, shared_rv = shared2),
             expr = {
    session$flushReact()
    session$setInputs(biostat_choice = "counts");     session$flushReact()
    session$setInputs(biostat_choice = "ok");         session$flushReact()
    session$setInputs(biostat_choice = "three_plus"); session$flushReact()
    session$setInputs(biostat_choice = "confirm");    session$flushReact()
    session$setInputs(biostat_confirm = 1L);          session$flushReact()
    rec <- isolate(gd2$biostat_decision)
    expect_false(is.null(rec), info = "la confirmation n'a pas publié le record §2")
    expect_identical(rec$engine, "",
                     info = "declaration vide ('') : repli sur l'argument engine existant (defaut '', M3)")
  })
})

# =============================================================================
# W5 -- I1 structurel : le module ne sélectionne JAMAIS le moteur du run bulk
# =============================================================================
test_that("W5 I1 structurel : aucune écriture vers de_engine, lecture seule de la déclaration", {
  code <- .m4w_mod_code()
  expect_true(grepl("declared_de_engine", code),
              info = "le module M4 ne lit pas la déclaration du moteur (couture absente)")
  expect_false(grepl("input\\$de_engine", code),
               info = paste("le module accède à input$de_engine -- interdit (I1 : conseiller,",
                            "jamais sélectionner ; la lecture passe par shared_rv$declared_de_engine)"))
  expect_false(grepl("updateSelectInput|updateRadioButtons|updateCheckboxInput|updateNumericInput",
                     code),
               info = paste("le module pousse un update*Input -- interdit (I1 : le run bulk reste",
                            "lancé par l'utilisateur sur le chemin existant, jamais piloté)"))
  expect_false(grepl("session\\$sendInput|session\\$setInputs", code),
               info = "le module manipule les inputs par canal caché -- interdit (I1)")
})

# =============================================================================
# W6 -- M3 préservé quand shared_rv est OMIS (signature étendue, zéro effet)
# =============================================================================
test_that("W6 M3 préservé : shared_rv omis -> aucune présélection, défauts M3, warnings vides", {
  expect_true("shared_rv" %in% names(formals(mod_biostat_tree_server)),
              info = "signature M4 attendue (shared_rv optionnel) -- le bloc épingle le comportement M3 quand il est omis")
  gd <- .m4w_gd(.m4w_meta3())   # bulk_obj PRÉSENT : il doit être IGNORÉ sans shared_rv
  testServer(mod_biostat_tree_server,
             args = list(id = "biostat_bulk", global_data = gd), expr = {
    session$flushReact()
    session$setInputs(biostat_choice = "counts"); session$flushReact()
    session$setInputs(biostat_choice = "ok");     session$flushReact()  # -> question groups_n
    g_html <- .m4w_html_of(session$output$biostat_walk_ui)
    expect_false(grepl("checked", g_html, fixed = TRUE),
                 info = paste("une réponse est présélectionnée alors que shared_rv est omis --",
                              "le comportement M3 (selected = character(0)) doit être préservé",
                              "byte-pour-byte même si bulk_obj est présent"))
    session$setInputs(biostat_choice = "three_plus"); session$flushReact()
    session$setInputs(biostat_choice = "confirm");    session$flushReact()
    session$setInputs(biostat_confirm = 1L);          session$flushReact()
    rec <- isolate(gd$biostat_decision)
    expect_false(is.null(rec), info = "la confirmation n'a pas publié le record §2")
    expect_identical(rec$prefilled, list(groups = "two", paired = FALSE),
                     info = "défauts M3 (list(groups='two', paired=FALSE)) requis quand shared_rv est omis -- contrat §2")
    expect_identical(rec$engine, "",
                     info = "engine = argument (defaut '', M3) quand shared_rv est omis")
    entry <- isolate(gd$provenance[[1]])
    expect_identical(entry$warnings, character(0),
                     info = "aucun warning de contredit quand aucune valeur n'a été pré-remplie")
  })
})

# =============================================================================
# W7 -- pont global_data$bulk_shared_rv (pièce D, décision coordination
#       2026-11-02, option b) : mod_bulk_server publie le handle (1 ligne,
#       champ gelé par CE jalon), app.R monte le module M4 avec ; le module
#       biostat n'y écrit JAMAIS (I1 élargi au pont -- ASSERTION structurelle,
#       pas un commentaire) ; pont NULL (bulk jamais lancé) -> M3 tel quel.
# =============================================================================
test_that("W7 pont bulk_shared_rv : publié par mod_bulk, monté dans app.R, lecture seule", {
  # (a) le pont existe dans mod_bulk_server (1 ligne, champ gelé bulk_shared_rv)
  bulk_lines <- readLines(file.path(ts_project_root(), "modules/bulk/mod_bulk.R"),
                          warn = FALSE)
  bulk_code <- paste(sub("#.*$", "", bulk_lines), collapse = "\n")
  expect_true(grepl("global_data\\$bulk_shared_rv\\s*<-\\s*shared_rv", bulk_code),
              info = paste("le pont n'est pas posé : mod_bulk_server doit publier",
                           "global_data$bulk_shared_rv <- shared_rv juste après la création",
                           "du handle (pièce D, décision coordination 2026-11-02)"))

  # (b) le montage app.R passe le handle (NULL-safe : M3 si bulk n'a pas tourné)
  #     -- isolate() obligatoire : lecture hors consommateur au montage, la
  #     lecture nue tue la session réelle (mesuré 2026-11-02, shinytest2
  #     test-shinytest2-bulk.R « bulk_ids » 404 sur dataobj/shinytest).
  app_src <- paste(readLines(file.path(ts_project_root(), "app.R"), warn = FALSE),
                   collapse = "\n")
  expect_true(grepl(paste0('mod_biostat_tree_server\\("biostat_bulk", global_data,\\s*',
                           'shared_rv = isolate\\(global_data\\$bulk_shared_rv\\)\\)'), app_src),
              info = paste("app.R:1102 doit monter l'assistant M4 avec",
                           "shared_rv = isolate(global_data$bulk_shared_rv) (pièce D)"))

  # (c) comportement via le pont + I1 ÉLARGI au pont : instantané complet du
  #     handle avant/après un parcours + confirmation -- identique (assertion).
  #     Les lectures de test hors session passent par isolate() (shiny 1.14.0
  #     refuse une lecture hors consommateur ; le corps server()/moduleServer
  #     du montage LIVE, lui, l'autorise -- mesuré 2026-11-02).
  gd3 <- .m4w_gd(.m4w_meta3())
  gd3$bulk_shared_rv <- .m4w_shared("condition", "edger")
  snap_before <- isolate(reactiveValuesToList(gd3$bulk_shared_rv))
  testServer(mod_biostat_tree_server,
             args = list(id = "biostat_bulk", global_data = gd3,
                         shared_rv = isolate(gd3$bulk_shared_rv)), expr = {
    session$flushReact()
    session$setInputs(biostat_choice = "counts");     session$flushReact()
    session$setInputs(biostat_choice = "ok");         session$flushReact()
    session$setInputs(biostat_choice = "three_plus"); session$flushReact()
    session$setInputs(biostat_choice = "confirm");    session$flushReact()
    session$setInputs(biostat_confirm = 1L);          session$flushReact()
    rec <- isolate(gd3$biostat_decision)
    expect_false(is.null(rec), info = "fixture : le record doit exister après confirmation")
    # feuille bulk_deseq2_lrt (bulk_*) : engine = déclaration lue via le pont
    expect_identical(rec$engine, "edger",
                     info = "via le pont, le record doit porter le moteur déclaré pour une feuille bulk_*")
    expect_identical(rec$prefilled, list(groups = "three_plus", paired = FALSE),
                     info = "via le pont, la valeur déduite doit porter dans le record")
  })
  snap_after <- isolate(reactiveValuesToList(gd3$bulk_shared_rv))
  expect_identical(snap_after, snap_before,
                   info = paste("le module biostat a ÉCRIT dans le handle partagé -- interdit",
                                "(I1 élargi au pont : lecture seule de global_data$bulk_shared_rv",
                                "et de TOUS ses champs, assertion structurelle)"))
  expect_identical(isolate(gd3$bulk_shared_rv$declared_de_engine), "edger",
                   info = "la confirmation a muté la déclaration publiée -- lecture seule exigée (I1)")

  # (d) pont NULL (bulk jamais lancé dans la session) -> M3 byte-pour-byte
  gd4 <- .m4w_gd(.m4w_meta3())   # bulk_shared_rv jamais publié
  testServer(mod_biostat_tree_server,
             args = list(id = "biostat_bulk", global_data = gd4,
                         shared_rv = isolate(gd4$bulk_shared_rv)), expr = {
    session$flushReact()
    session$setInputs(biostat_choice = "counts"); session$flushReact()
    session$setInputs(biostat_choice = "ok");     session$flushReact()
    expect_false(grepl("checked", .m4w_html_of(session$output$biostat_walk_ui),
                       fixed = TRUE),
                 info = paste("pont NULL : une réponse est présélectionnée -- le handle absent",
                              "doit donner le comportement M3 (selected = character(0)) exactement"))
  })
})

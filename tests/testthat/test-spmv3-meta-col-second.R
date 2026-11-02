# =============================================================================
# test-spmv3-meta-col-second.R -- SPMV-3 : SECOND selecteur « Colonne 'type
# cellulaire' » (shared_ref_celltype_col) de modules/import/mod_import_spatial.R
# (RED-first)
# =============================================================================
# FICHE (fait foi) : docs/proposals/SPATIAL_META_COL_VALIDATION_PROPOSAL.md
# (2026-10-26, arbitrage GO, operateur 2026-11-02). Jalon SPMV-3 (§6 :141) :
# « Extension aux selecteurs SC/Bulk — meme composant, zero copie ». Le site
# vise ici est le SECOND selecteur porte-parole du motif fenetre+fallback que
# SPMV-1/SPMV-2 ont remplace ailleurs : le champ partage de reference scrna-seq
# de l'import Spatial (§4d :120-121 limite le perimetre initial a
# ref_celltype_col ; les autres surfaces §2b ne changent pas).
#
# ETAT MESURE (arbre 2784926 = SPMV-2 livre ; re-mesure de cette session) :
#   * SITE : modules/import/mod_import_spatial.R:926-948 —
#     output$shared_ref_celltype_col_ui porte ENCORE le motif fenetre muette
#     + fallback silencieux identique a l'ancien site SPMV-1 :
#       :931-932  n_levels <- vapply(...) ; useful <- cols[n_levels >= 2 &
#                 n_levels <= 200]            <- fenetre muette [2, 200]
#       :933      useful <- useful[order(-n_levels[useful])] <- tri silencieux
#                 par cardinalite decroissante
#       :934      choices <- if (length(useful) > 0) useful else cols
#                                             <- fallback silencieux « else cols »
#     Consequence MESUREE sous testServer (sonde Rscript de cette session,
#     fichier hors depot) : avec une reference ok/warn/danger distincts, le
#     rendu est <option>barcode_col</option> (SELECTEE PAR DEFAUT : 60 niveaux
#     uniques = identifiant par cellule, triee en tete par cardinalite),
#     score_num, nCount_RNA, cell_type, donor_id, nFeature_RNA — sans badge,
#     sans bandeau, sans explication. Scenarios §3 E2/E3 vivants sur ce site.
#   * COMPOSANT (SPMV-1, consomme TEL QUEL — jamais duplique) :
#     tools/pkg_skeleton/R/spatial_meta_col_validation.R:71-162,
#     spatial_meta_col_validation(meta_data, col) -> list(level, reason_key,
#     detail).
#   * DOCTRINE LIVREE A REPLICUER (SPMV-2, modules/spatial/deconv/
#     mod_spatial_deconv_reference.R:248-348) : verdicts par colonne (:253-257),
#     tri visible ok->warn->danger STABLE au sein d'un niveau — l'ordre
#     meta.data fait foi (:258, :224-226), badge par option (:263-275),
#     bandeau recapitulatif AU-DESSUS du selecteur (:298-302), ligne
#     d'explication du tri traduite (:331-332).
#   * PREPARE/CONVERSION (zero changement scientifique, §5 :131-133) :
#     observeEvent(input$shared_ref_file, ...) -> read_reference_scrna
#     (mod_import_spatial.R:875) -> prepare_reference_seurat (:885) ;
#     btn_prepare_shared_ref -> prepare_reference_artifact (:955-961,
#     min_cells_per_type = 25L :959) -> global_data$spatial_reference
#     (:969-974). Le composant NE TRANSFORME rien (§4a, en-tete :18-21).
#   * i18n : TOUTES les cles que ce site consommera EXISTENT deja fr/en
#     (i18n/translation.json, zone :11649-11698 — bandeau, ligne de tri,
#     « {n} classes », 8 reason_keys SPMV-1 ; verifie par lecture JSON de
#     cette session). AUCUN append n'est requis (decision D4).
#   * www/ n'existe pas sur l'arbre (0 CSS — regle post-O13).
#
# QU'EST ASSERTABLE HEADLESS (sonde Rscript de cette session, hors depot) :
#   * shiny::testServer boote le serveur REEL mod_import_spatial_server
#     (moduleServer, shiny 1.14.0 — support module mesure) ; l'upload
#     fileInput passe par session$setInputs(shared_ref_file = list(name=,
#     size=, type=, datapath=)) et declenche le VRAI pipeline de lecture
#     (read_reference_scrna sur .rds list(counts, meta) -> VRAIE conversion
#     prepare_reference_seurat) — Seurat reel du renv, objet 500 x 20.
#   * Primitives UI bouchonnees (withProgress/incProgress/showNotification,
#     motif test-mod-import-spatial.R « withProgress() DOIT etre bouchonne —
#     mesure » ; sauvegarde/restauration on.exit, motif qc-tabs :335-356).
#   * app.R n'est jamais source (convention test-app-sourcing.R) ; l'ordre de
#     source du boot est celui de la sonde mesuree : io_helpers, state,
#     drive_allowlist, drive_watcher (ts_drive_bind_button :2175 est appele au
#     boot du module, mod_import_spatial.R:387), rdata_io, spatial_reference,
#     composant SPMV-1, module.
#
# DECISIONS TRANCHEES (chaque alternative documentee) :
#   D1 Perimetre du site (fiche §4d/§6) : SPMV-3 = verdicts du composant +
#      tri visible et explique + suppression du fallback silencieux, SANS la
#      case « Je confirme » et SANS blocage du bouton : la fiche reserve
#      case/blocage au cablage SPMV-2 de ref_celltype_col (§6 :140) ; la ligne
#      SPMV-3 (§6 :141) n'exige que « meme composant, zero copie ». T3 epingle
#      donc en NEGATIF : aucune case « Je confirme », bouton
#      btn_prepare_shared_ref jamais desactive, meme sur colonne danger.
#   D2 Doctrine = celle du site SPMV-2 LIVRE : TOUTES les colonnes proposees
#      (§4c :114 — aucune colonne retiree du choix), tri ok->warn->danger
#      stable (ordre meta.data au sein d'un niveau), badge par option
#      (§4c « chacune avec son badge »), bandeau recapitulatif au-dessus du
#      selecteur (§4c ; idem SPMV-2 T2), ligne d'explication du tri traduite
#      (SPMV-2 :331-332).
#   D3 L'ordre attendu est DERIVE DU COMPOSANT (pas de re-implementation) :
#      le test rejoue la conversion reelle (read_reference_scrna +
#      prepare_reference_seurat — les memes fonctions que le module) pour
#      connaitre le meta.data converti, applique le composant a chaque
#      colonne et calcule cols[order(match(levels, c("ok","warn","danger")),
#      method="radix")] — l'idiome exact du site SPMV-2 (:258). T3 compare
#      l'ORDRE rendu a cet ordre-la.
#   D4 i18n : AUCUN libelle nouveau — les cles consommees existent fr/en
#     (mesure, zone :11649-11698). T5 epingle la non-regression fr/en des
#     cles consommees (bandeau, ligne de tri, « {n} classes », reason_keys
#     derives du composant) ; translation.json ne doit PAS bouger dans cette
#     lane sauf libelle nouveau (append SCOPE, lecon B1).
#   D5 T4 = structurel (tokens du flux prepare/conversion et des garde-fous,
#     juges en code SANS commentaires — precedent test-biostat-tree.R:69-74 ;
#     moteur tools/pkg_skeleton/R/spatial_reference.R intact ; composant pur)
#     + passthrough comportemental (stub de prepare_reference_artifact dans
#     globalenv, sauvegarde/restauration, motif spmv2 D7) : colonne,
#     merge_rare, cap/max et min 25L passent INTACTS au moteur, meta.data
#     byte-identique (serialize). Sur l'arbre ACTUEL T0/T4/T5 sont des
#     garde-fous verts ; la porte RED est portee par T1 (lexical),
#     T2 (structurel) et T3 (comportemental) — motif spmv2 D8 : un skip
#     n'est pas une porte rouge, chaque expect echoue explicitement.
#   D6 T1 lexical cible le FICHIER ENTIER en code sans commentaires (mesure :
#     le motif n'existe nulle part ailleurs que :931-934 dans ce fichier) —
#     trois interdits : le fallback « else cols », la fenetre muette
#     « n_levels >= 2 & n_levels <= 200 », le tri silencieux par cardinalite
#     « order(-n_levels » (fiche §4c :115-116 interdit les trois ensemble).
#   D7 Les stubs et leur restauration sont locaux a chaque test (on.exit) —
#     aucun etat global ne fuit vers les autres fichiers du processus.
# =============================================================================

# ── Sources (composant SPMV-1 livre 715438e, consomme TEL QUEL) --------------

if (!exists("%||%", envir = globalenv(), inherits = FALSE)) {
  source_project_file("tools/pkg_skeleton/R/io_helpers.R")
}
if (!exists("state_get", envir = globalenv(), inherits = FALSE)) {
  source_project_file("tools/pkg_skeleton/R/state.R")
}
if (!exists("TS_DRIVE_SPATIAL_IMPORT_BUTTON", envir = globalenv(), inherits = FALSE)) {
  source_project_file("tools/pkg_skeleton/R/drive_allowlist.R")
}
if (!exists("ts_drive_bind_button", envir = globalenv(), inherits = FALSE)) {
  source_project_file("tools/pkg_skeleton/R/drive_watcher.R")
}
source_project_file("tools/pkg_skeleton/R/rdata_io.R")            # branche .rda de read_reference_scrna
source_project_file("tools/pkg_skeleton/R/spatial_reference.R")   # read/prepare — consommees TELLES QUELLES
source_project_file("tools/pkg_skeleton/R/spatial_meta_col_validation.R")  # SPMV-1, TEL QUEL
source_project_file("modules/import/mod_import_spatial.R")

suppressPackageStartupMessages(library(shiny))

# i18n : global.R fournit le vrai Translator a l'execution ; les tests recoivent
# le repli no-op (i18n = NULL -> .tr() renvoie la cle elle-meme, cles = fr).
if (!exists(".t_fmt", envir = globalenv(), inherits = FALSE)) {
  assign(".t_fmt", function(template, ...) {
    vals <- list(...)
    for (nm in names(vals)) {
      template <- gsub(paste0("{", nm, "}"), format(vals[[nm]]), template, fixed = TRUE)
    }
    template
  }, envir = globalenv())
}
if (!exists(".strip_i18n_html", envir = globalenv(), inherits = FALSE)) {
  assign(".strip_i18n_html", function(res) {
    if (inherits(res, c("shiny.tag", "shiny.tag.list")) || length(res) > 1L) {
      flat <- paste(as.character(res), collapse = "")
      return(gsub("^<span[^>]*>|</span\\s*>$", "", flat))
    }
    res
  }, envir = globalenv())
}

# ── Contrat i18n SPMV-3 : cles DEJA livrees fr/en (decision D4) ---------------

.spmv3_key_banner <- paste0("Colonnes evaluees : {n_ok} compatibles, ",
                            "{n_warn} a verifier, {n_danger} incompatibles.")
.spmv3_key_tri    <- paste0("Colonnes triees : compatibles en premier (vert), a verifier (orange), ",
                            "incompatibles (rouge) — chaque colonne est evaluee par le composant ",
                            "de validation des metadonnees.")
.spmv3_key_nclass <- "{n} classes"
.spmv3_key_confirm <- "Je confirme : je veux utiliser cette colonne malgre l'avertissement"

# ── Fixture (table §4b, convention test-spmv2 :143-166) — verdicts DISTINCTS --

.spmv3_n <- 500L

.spmv3_meta <- function() {
  data.frame(
    cell_type      = rep(c("A", "B", "C", "D", "E"), each = 100),
    cell_type_fact = factor(rep(c("Ast", "Oli", "Mic", "Exc", "Inh"), each = 100)),
    flag_logical   = rep(c(TRUE, FALSE), times = 250),
    histo_region   = rep(c("r1", "r2", "r3", "r4"), length.out = .spmv3_n),
    atlas_types    = rep(paste0("t", seq_len(150)), length.out = .spmv3_n),
    donor_id       = rep(c("D1", "D2", "D3"), c(200, 200, 100)),
    batch_label    = rep(c("b1", "b2", "b3", "b4"), each = 125),
    score_num      = seq(0, 1, length.out = .spmv3_n),
    barcode_col    = paste0("BC", seq_len(.spmv3_n)),
    row.names      = paste0("cell_", seq_len(.spmv3_n)),
    stringsAsFactors = FALSE
  )
}

.spmv3_ok     <- c("cell_type", "cell_type_fact", "flag_logical", "histo_region")
.spmv3_warn   <- c("atlas_types", "donor_id", "batch_label")
.spmv3_danger <- c("score_num", "barcode_col")

# Fixture .rds (contrat read_reference_scrna, tools/pkg_skeleton/R/
# spatial_reference.R:37-42 : liste counts/meta) — le module passe par le VRAI
# pipeline de lecture + conversion.
.spmv3_rds <- function() {
  n  <- .spmv3_n
  meta <- .spmv3_meta()
  cnt <- Matrix::Matrix(stats::rpois(20 * n, 5), nrow = 20, ncol = n, sparse = TRUE)
  colnames(cnt) <- rownames(meta)
  p <- tempfile(fileext = ".rds")
  saveRDS(list(counts = cnt, meta = meta), p)
  p
}

# Conversion REELLE rejouee par le test (meme pipeline que le module :
# mod_import_spatial.R:875 + :885) — base du calcul d'ordre D3.
.spmv3_converted_meta <- function(rds_path) {
  conv <- prepare_reference_seurat(read_reference_scrna(rds_path),
                                   project_name = "SharedReference")
  conv@meta.data
}

.spmv3_verdict <- function(meta, col) spatial_meta_col_validation(meta, col)$level

# Ordre de doctrine (D3) — l'idiome EXACT du site SPMV-2 livre
# (mod_spatial_deconv_reference.R:258), applique aux verdicts du composant.
.spmv3_expected_order <- function(meta) {
  cols   <- colnames(meta)
  levels <- vapply(cols, function(cn) spatial_meta_col_validation(meta, cn)$level, character(1))
  cols[order(match(levels, c("ok", "warn", "danger")), method = "radix")]
}

# ── Extracteurs HTML (motif test-spmv2 :174-205, tolerants a NULL/"" ---------

.spmv3_html <- function(out) {
  if (is.null(out)) return("")
  if (is.list(out)) out <- out$html
  paste(collapse = "", as.character(out))
}

.spmv3_before_select <- function(html) {
  if (is.null(html) || !nzchar(html)) return("")
  i <- regexpr("<select", html, fixed = TRUE)
  if (i < 0) html else substr(html, 1L, i - 1L)
}

.spmv3_options <- function(html) {
  if (is.null(html) || !nzchar(html)) return(character(0))
  as.character(regmatches(html, gregexpr("<option[^>]*>.*?</option>", html, perl = TRUE))[[1]])
}

.spmv3_option_values <- function(html) {
  opts <- .spmv3_options(html)
  vapply(opts, function(o) {
    m <- regmatches(o, regexpr('value="[^"]+"', o))
    if (length(m) == 1L) sub('value="([^"]+)"', "\\1", m) else ""
  }, "", USE.NAMES = FALSE)
}

.spmv3_option_of <- function(html, col) {
  opts <- .spmv3_options(html)
  hits <- opts[grepl(paste0('<option value="', col, '"'), opts, fixed = TRUE)]
  if (length(hits) == 1L) hits else character(0)
}

.spmv3_btn <- function(html) {
  if (is.null(html) || !nzchar(html)) return(character(0))
  as.character(regmatches(html, regexpr("<button[^>]*btn_prepare_shared_ref[^>]*>.*?</button>",
                                        html, perl = TRUE)))
}

# ── Boot e2e (sonde mesuree) : serveur REEL passe DIRECTEMENT a testServer
# (moduleServer, support module de shiny 1.14.0 -- setInputs/output non
# prefixes y sont auto-namespaces par le proxy, mesure par sonde). ----------

.spmv3_boot <- function(rds_path) {
  gd <- shiny::reactiveValues(language = "fr", i18n = NULL,
                              spatial_reference = NULL, spatial_obj = NULL)
  list(gd = gd, srv = mod_import_spatial_server, rds = rds_path)
}

.spmv3_upload <- function(rds_path) {
  list(name = "ref_fixture.rds", size = 1000,
       type = "application/octet-stream", datapath = rds_path)
}

# Stub des primitives UI de progression/notification (motif
# test-mod-import-spatial.R « withProgress DOIT etre bouchonne — mesure »),
# avec restauration (D7).
.spmv3_stub_ui <- function() {
  names_ <- c("withProgress", "incProgress", "showNotification")
  saved <- lapply(names_, function(nm) {
    existed <- exists(nm, envir = globalenv(), inherits = FALSE)
    old <- if (existed) get(nm, envir = globalenv(), inherits = FALSE) else NULL
    assign(nm,
           switch(nm,
                  withProgress     = function(expr, ...) expr,
                  incProgress      = function(...) NULL,
                  showNotification = function(...) NULL),
           envir = globalenv())
    list(name = nm, existed = existed, old = old)
  })
  function() {
    for (p in saved) {
      if (isTRUE(p$existed)) assign(p$name, p$old, envir = globalenv())
      else if (exists(p$name, envir = globalenv(), inherits = FALSE))
        rm(list = p$name, envir = globalenv())
    }
  }
}

# Code SANS commentaires d'un fichier du depot (motif spmv2 T5a — precedent
# test-biostat-tree.R:69-74).
.spmv3_code <- function(relpath) {
  lines <- readLines(file.path(ts_project_root(), relpath), warn = FALSE)
  paste(sub("#.*$", "", lines), collapse = "\n")
}

# Marqueurs de verdict visibles (D2) — primitives Bootstrap + icones.
.spmv3_badge_pat   <- "badge|text-bg-|bg-warning|bg-danger|text-warning|text-danger|exclamation|octagon"
.spmv3_badwarn_pat <- "bg-warning|bg-danger|text-warning|text-danger|text-bg-warning|text-bg-danger|exclamation|octagon"

# ── T0 -- surface : composant livre, fixture d'accord avec la table §4b -------

test_that("T0 SPMV-3 surface : le composant livre est source et la fixture colle a la table de verite §4b", {
  expect_true(exists("spatial_meta_col_validation", envir = globalenv()),
              info = "spatial_meta_col_validation introuvable -- SPMV-1 (commit 715438e) doit etre source")
  expect_true(exists("mod_import_spatial_server", envir = globalenv()),
              info = "mod_import_spatial_server introuvable -- module non source")
  meta <- .spmv3_meta()
  for (col in .spmv3_ok) {
    expect_identical(.spmv3_verdict(meta, col), "ok",
                     info = paste("fixture drift :", col, "devrait valoir ok (§4b)"))
  }
  for (col in .spmv3_warn) {
    expect_identical(.spmv3_verdict(meta, col), "warn",
                     info = paste("fixture drift :", col, "devrait valoir warn (§4b)"))
  }
  for (col in .spmv3_danger) {
    expect_identical(.spmv3_verdict(meta, col), "danger",
                     info = paste("fixture drift :", col, "devrait valoir danger (§4b)"))
  }
})

# ── T1 -- plus de fallback : fenetre muette et « else cols » ABSENTS (D6) -----

test_that("T1 SPMV-3 plus de fallback : ni « else cols », ni fenetre muette [2,200], ni tri silencieux par cardinalite (code sans commentaires)", {
  mod_code  <- .spmv3_code("modules/import/mod_import_spatial.R")
  mod_norm  <- gsub("[[:space:]]+", " ", mod_code)

  expect_false(grepl("else cols", mod_norm, fixed = TRUE),
               info = paste("fallback silencieux « else cols » encore present dans",
                            "modules/import/mod_import_spatial.R (mesure :934) --",
                            "SPMV-3 doit le supprimer : TOUTES les colonnes proposees avec",
                            "leur verdict, jamais un repli muet (fiche §2d-1/§4c)"))
  expect_false(grepl("n_levels >= 2 & n_levels <= 200", mod_norm, fixed = TRUE),
               info = paste("fenetre muette [2, 200] encore present dans",
                            "modules/import/mod_import_spatial.R (mesure :932) -- la",
                            "fenetre aveugle interdit sans dire pourquoi (fiche §8 :154-156) ;",
                            "le verdict du composant la remplace"))
  expect_false(grepl("order(-n_levels", mod_norm, fixed = TRUE),
               info = paste("tri silencieux par cardinalite decroissante encore present",
                            "(mesure :933) -- le tri devient ok->warn->danger, visible et",
                            "explique (fiche §4c :115-116)"))
})

# ── T2 -- verdicts = composant : le second selecteur le consomme (structurel) --

test_that("T2 SPMV-3 verdicts = composant : mod_import_spatial.R consomme spatial_meta_col_validation (code sans commentaires)", {
  mod_code <- .spmv3_code("modules/import/mod_import_spatial.R")
  expect_true(grepl("spatial_meta_col_validation", mod_code),
              info = paste("le second selecteur (shared_ref_celltype_col) ne consomme PAS le",
                           "composant SPMV-1 -- l'evaluation des colonnes doit venir de",
                           "tools/pkg_skeleton/R/spatial_meta_col_validation.R:71-162,",
                           "JAMAIS d'une fenetre locale re-implementee (zero copie, §6 :141 ;",
                           "garde anti-duplication)"))
})

# ── T3 -- tri visible et explique : ordre du composant + bandeau + ligne de tri

test_that("T3 SPMV-3 tri visible et explique : toutes les colonnes, ordre ok->warn->danger du composant, badges, bandeau au-dessus, ligne de tri traduite ; PAS de case ni de blocage (D1)", {
  restore <- .spmv3_stub_ui()
  on.exit(restore(), add = TRUE)

  rds <- .spmv3_rds()
  conv_meta <- .spmv3_converted_meta(rds)
  expected_order <- .spmv3_expected_order(conv_meta)
  lv <- vapply(colnames(conv_meta),
               function(cn) spatial_meta_col_validation(conv_meta, cn)$level, character(1))
  n_ok <- sum(lv == "ok"); n_warn <- sum(lv == "warn"); n_danger <- sum(lv == "danger")
  expected_banner <- .t_fmt(.spmv3_key_banner, n_ok = n_ok, n_warn = n_warn, n_danger = n_danger)

  st <- .spmv3_boot(rds)
  shiny::testServer(st$srv, args = list(global_data = st$gd), {
    session$setInputs(shared_ref_file = .spmv3_upload(st$rds))
    session$flushReact()
    html <- .spmv3_html(tryCatch(output$shared_ref_celltype_col_ui, error = function(e) NULL))

    if (!nzchar(html)) {
      expect_true(FALSE, info = "output$shared_ref_celltype_col_ui doit se rendre (site :926-948) -- rendu vide/absent")
    } else {
      # (a) TOUTES les colonnes du meta.data converti restent proposees,
      #     chacune une fois (§4c :114 -- la fenetre ne retire plus rien).
      got <- .spmv3_option_values(html)
      expect_true(setequal(got, colnames(conv_meta)) && !any(duplicated(got[got != ""])),
                  info = paste("le selecteur doit proposer TOUTES les colonnes du meta.data converti",
                               "(§4c) -- manque :", paste(setdiff(colnames(conv_meta), got), collapse = ", "),
                               "| en trop :", paste(setdiff(got, colnames(conv_meta)), collapse = ", ")))

      # (b) Ordre = verdicts du composant : ok d'abord, puis warn, puis danger ;
      #     stable au sein d'un niveau (D3).
      expect_identical(got, expected_order,
                       info = paste("ordre rendu != ordre de doctrine ok->warn->danger derive du",
                                    "composant (mod_spatial_deconv_reference.R:258). Attendu :",
                                    paste(expected_order, collapse = ", "), "-- rendu :",
                                    paste(got, collapse = ", ")))
      #     Le cas mesure avant SPMV-3 : barcode_col (identifiant par cellule)
      #     etait SELECTIEE EN TETE par tri de cardinalite — verbatim du
      #     scenario §3 E2/E3 sur ce site.
      expect_identical(got[1], expected_order[1],
                       info = paste("la premiere option proposee doit etre une colonne compatible (ok),",
                                    "pas un identifiant/barcode trie par cardinalite -- attendu en tete :",
                                    expected_order[1]))

      # (c) Chaque option warn/danger porte son verdict visible : marqueur de
      #     badge + reason_key traduite du composant (D2 ; §4c « chacune avec
      #     son badge »). Une ok n'emporte aucun marqueur warn/danger.
      for (col in colnames(conv_meta)) {
        v <- spatial_meta_col_validation(conv_meta, col)
        opt <- .spmv3_option_of(html, col)
        expect_true(length(opt) == 1L && nzchar(opt),
                    info = paste("option absente pour", col))
        if (length(opt) == 1L && nzchar(opt)) {
          if (identical(v$level, "ok")) {
            expect_false(grepl(.spmv3_badwarn_pat, opt),
                         info = paste("l'option ok", col, "porte un marqueur warn/danger -- interdit"))
          } else {
            expect_true(grepl(.spmv3_badge_pat, opt),
                        info = paste("AUCUN marqueur de badge visible sur l'option", col,
                                     "(", v$level, ") -- chaque option doit porter son verdict (§4c)"))
            expect_true(nzchar(v$reason_key) && grepl(v$reason_key, opt, fixed = TRUE),
                        info = paste("la reason_key du composant doit etre rendue (traduite) sur",
                                     "l'option", col, ":", v$reason_key))
          }
        }
      }

      # (d) Bandeau recapitulatif AU-DESSUS du selecteur (D2 ; SPMV-2 T2).
      before <- .spmv3_before_select(html)
      expect_true(grepl(expected_banner, before, fixed = TRUE),
                  info = paste("bandeau recapitulatif absent AVANT le selecteur -- attendu",
                               "(repli i18n = cle interpolee) :", expected_banner))
      expect_true(grepl("alert", before),
                  info = "le bandeau doit etre une primitive alert Bootstrap/bslib (0 CSS www/)")

      # (e) Ligne d'explication du tri, traduite (fr = cle en repli test).
      expect_true(grepl(.spmv3_key_tri, html, fixed = TRUE),
                  info = paste("ligne d'explication du tri absente du rendu -- attendue :",
                               .spmv3_key_tri))

      # (f) D1 (fiche §6 :141) : PAS de case « Je confirme », PAS de blocage
      #     du bouton -- meme colonne danger retenue (le blocage est le propre
      #     du cablage SPMV-2, ref_celltype_col).
      expect_false(grepl(.spmv3_key_confirm, html, fixed = TRUE),
                   info = "une case « Je confirme » est apparue sur le site SPMV-3 -- hors perimetre (fiche §6 :141)")
      session$setInputs(shared_ref_celltype_col = "barcode_col")
      session$flushReact()
      html_danger <- .spmv3_html(tryCatch(output$shared_ref_celltype_col_ui, error = function(e) NULL))
      btn <- .spmv3_btn(html_danger)
      expect_true(length(btn) == 1L && nzchar(btn),
                  info = "bouton « Preparer + partager cette reference » introuvable avec colonne danger retenue")
      if (length(btn) == 1L) {
        expect_false(grepl("\\bdisabled\\b", btn),
                     info = "le bouton de preparation est desactive sur colonne danger -- le blocage SPMV-2 ne doit PAS etre duplique ici (fiche §6 :141, zero copie)")
      }
    }
  })
})

# ── T4 -- zero changement scientifique (§5 :131-133) : structure + passthrough -

test_that("T4 SPMV-3 zero changement scientifique : flux prepare/conversion intact, passthrough du clic, meta.data immune", {
  # (a) Structurel : les fonctions du flux et les garde-fous restent en place.
  mod_code <- .spmv3_code("modules/import/mod_import_spatial.R")
  expect_true(all(vapply(c("read_reference_scrna", "prepare_reference_seurat",
                           "prepare_reference_artifact", "shared_merge_rare_types",
                           "shared_cap_ref_cells", "shared_max_cells_per_type",
                           "btn_prepare_shared_ref", "shared_ref_celltype_col",
                           "global_data$spatial_reference"),
                         function(tok) grepl(tok, mod_code, fixed = TRUE), logical(1))),
              info = "le flux d'import shared-ref (lecture/conversion/preparation + garde-fous) doit rester intact (§5 :131-133)")
  ref_code <- .spmv3_code("tools/pkg_skeleton/R/spatial_reference.R")
  expect_true(all(vapply(c("read_reference_scrna <- function",
                           "prepare_reference_seurat <- function",
                           "prepare_reference_artifact <- function"),
                         function(tok) grepl(tok, ref_code, fixed = TRUE), logical(1))),
              info = "le moteur de reference (tools/pkg_skeleton/R/spatial_reference.R) doit rester intact")
  comp_code <- .spmv3_code("tools/pkg_skeleton/R/spatial_meta_col_validation.R")
  expect_false(grepl("selectInput|actionButton|checkboxInput|renderUI", comp_code),
               info = "le composant SPMV-1 ne doit pas se mettre a rendre -- il reste pur (§4a)")

  # (b) Passthrough comportemental : stub du moteur (sauvegarde/restauration,
  #     motif spmv2 D7), puis clic -- colonne et garde-fous passent INTACTS.
  restore <- .spmv3_stub_ui()
  on.exit(restore(), add = TRUE)
  rds <- .spmv3_rds()
  meta0 <- .spmv3_converted_meta(rds)
  ser_before <- serialize(meta0, NULL)
  calls <- list()
  had_prepare <- exists("prepare_reference_artifact", envir = globalenv(), inherits = FALSE)
  old_prepare <- if (had_prepare) get("prepare_reference_artifact", envir = globalenv(), inherits = FALSE) else NULL
  assign("prepare_reference_artifact",
         function(ref_obj, celltype_col, merge_rare_types, min_cells_per_type,
                  max_cells_per_type, ...) {
           calls[[length(calls) + 1L]] <<- list(celltype_col = celltype_col,
                                                merge_rare_types = merge_rare_types,
                                                min_cells_per_type = min_cells_per_type,
                                                max_cells_per_type = max_cells_per_type)
           list(path = tempfile(fileext = ".rds"), n_cells = 10L, n_genes = 5L,
                backend = "test", n_dropped_rare = 0L)
         },
         envir = globalenv())
  on.exit({
    if (isTRUE(had_prepare)) assign("prepare_reference_artifact", old_prepare, envir = globalenv())
    else if (exists("prepare_reference_artifact", envir = globalenv(), inherits = FALSE))
      rm(list = "prepare_reference_artifact", envir = globalenv())
  }, add = TRUE)

  st <- .spmv3_boot(rds)
  shiny::testServer(st$srv, args = list(global_data = st$gd), {
    session$setInputs(shared_ref_file = .spmv3_upload(st$rds))
    session$flushReact()
    session$setInputs(shared_ref_celltype_col = "cell_type",
                      shared_merge_rare_types = FALSE,
                      shared_cap_ref_cells = TRUE,
                      shared_max_cells_per_type = 100)
    session$flushReact()
    session$setInputs(btn_prepare_shared_ref = 1L)
    session$flushReact()

    expect_true(length(calls) >= 1L,
                info = "le clic « Preparer + partager cette reference » doit appeler le moteur prepare_reference_artifact")
    if (length(calls) >= 1L) {
      expect_identical(calls[[1]]$celltype_col, "cell_type",
                       info = "la colonne passee au moteur doit etre la selection (inchangee)")
      expect_false(isTRUE(calls[[1]]$merge_rare_types),
                   info = "shared_merge_rare_types doit passer intact au moteur")
      expect_identical(calls[[1]]$max_cells_per_type, 100,
                       info = "shared_cap_ref_cells/shared_max_cells_per_type doivent passer intacts au moteur")
      expect_identical(calls[[1]]$min_cells_per_type, 25L,
                       info = "min_cells_per_type = 25L (site :959) doit rester intact")
    }
    expect_false(is.null(st$gd$spatial_reference),
                 info = "global_data$spatial_reference doit etre pose apres preparation reussie (site :969-974)")
    if (!is.null(st$gd$spatial_reference)) {
      expect_identical(st$gd$spatial_reference$celltype_col, "cell_type",
                       info = "l'artefact partage doit tracer la colonne choisie")
    }
    # La couche de validation ne modifie rien (§5) : meta.data byte-identique.
    expect_identical(serialize(.spmv3_converted_meta(rds), NULL), ser_before,
                     info = "meta.data modifie par le pipeline lecture/conversion rejoue -- le composant et le site ne doivent RIEN transformer (§5 :131-133)")
    expect_identical(rownames(meta0), paste0("cell_", seq_len(.spmv3_n)),
                     info = "row.names de meta.data modifies -- interdit")
  })
})

# ── T5 -- i18n fr/en des cles consommees (D4) + 0 CSS www/ --------------------

test_that("T5 SPMV-3 i18n : cles consommees en paire fr/en (aucun libelle nouveau requis) ; 0 CSS www/", {
  path <- file.path(ts_project_root(), "i18n", "translation.json")
  expect_true(file.exists(path), info = "i18n/translation.json absent")
  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  fr <- vapply(j$translation, function(x) if (is.null(x$fr)) "" else x$fr, character(1))
  en <- vapply(j$translation, function(x) if (is.null(x$en)) "" else x$en, character(1))

  # (a) Cles de mise en page consommees par le site SPMV-3 (deja livrees,
  #     decision D4 -- mesure zone :11649-11698).
  for (k in c(.spmv3_key_banner, .spmv3_key_tri, .spmv3_key_nclass)) {
    idx <- match(k, fr)
    expect_false(is.na(idx),
                 info = paste("cle FR absente de translation.json :", k,
                              "-- si ce libelle est NOUVEAU, append SCOPE requis (tool au lot",
                              "seul puis restaure, lecon B1) ; sinon reutiliser la cle livree"))
    if (!is.na(idx)) {
      expect_true(nzchar(en[[idx]]),
                  info = paste("paire EN absente ou vide pour :", k))
    }
  }

  # (b) Non-regression : les reason_keys du composant (consommees TELLES
  #     QUELLES par les badges) restent en paire fr/en.
  conv_meta <- .spmv3_converted_meta(.spmv3_rds())
  reason_keys <- unique(vapply(colnames(conv_meta), function(col) {
    r <- spatial_meta_col_validation(conv_meta, col)
    if (identical(r$level, "ok")) "" else paste(as.character(r$reason_key), collapse = " ")
  }, ""))
  reason_keys <- reason_keys[nzchar(reason_keys)]
  expect_true(length(reason_keys) >= 4L,
              info = "au moins 4 reason_keys distincts attendus sur la fixture (causes §4b)")
  for (k in reason_keys) {
    idx <- match(k, fr)
    expect_false(is.na(idx), info = paste("reason_key du composant disparu de translation.json :", k))
    if (!is.na(idx)) expect_true(nzchar(en[[idx]]),
                                 info = paste("paire EN absente pour le reason_key :", k))
  }

  # (c) 0 CSS www/ (regle post-O13) : aucune feuille de style SPMV, aucune
  #     injection de style dans le module (primitives bslib/shiny seulement).
  www <- file.path(ts_project_root(), "www")
  if (dir.exists(www)) {
    css <- list.files(www, pattern = "css$", recursive = TRUE, ignore.case = TRUE, full.names = FALSE)
    expect_equal(length(css[grep("spmv|meta_col", css, ignore.case = TRUE)]), 0L,
                 info = "un fichier CSS SPMV-3 est apparu dans www/ -- interdit (regle post-O13)")
  }
  mod_code <- .spmv3_code("modules/import/mod_import_spatial.R")
  expect_false(grepl("tags\\$style|includeCSS|tags\\$head", mod_code),
               info = "le site SPMV-3 injecte du style -- interdit (0 CSS, primitives seulement)")
})

# =============================================================================
# test-spmv2-meta-col-render.R -- SPMV-2 : rendu de la validation des colonnes
# de metadonnees au selecteur ref_celltype_col (RED-first)
# =============================================================================
# FICHE (fait foi) : docs/proposals/SPATIAL_META_COL_VALIDATION_PROPOSAL.md
# (2026-10-26, arbitrage GO sur recommandation retenue, operateur 2026-11-02).
#
# Jalon SPMV-2 (fiche §6 :140) :
#   « Cablage ref_celltype_col : badges + ordre de tri visible + case
#     « Je confirme » + suppression du fallback silencieux »
#   Gate : « test e2e cible du module ; i18n fr/en completes ; falsification
#   prouvee ». Criteres §7 :145-149, en particulier :
#   §7-1 :145 (E1 : donor_id -> o visible avant selection, execution POSSIBLE
#   mais eclairee) ; §7-2 :146 (E2 : colonne barcode -> bouton « 1) Preparer
#   la reference » DESACTIVE sans case explicite) ; §7-4 :148 (aucun
#   comportement silencieux nouveau : chaque restriction expliquee fr/en).
#
# ETAT MESURE (arbre courant, commit 715438e = SPMV-1 livre) :
#   * composant unique LIVRE et consomme TEL QUEL :
#     tools/pkg_skeleton/R/spatial_meta_col_validation.R:71-162 (fonction
#     spatial_meta_col_validation -> list(level, reason_key, detail)) ;
#   * le site modules/spatial/deconv/mod_spatial_deconv_reference.R:220-258
#     trie deja les choix par verdict (tri visible SPMV-1, :234-241) mais NE
#     REND AUCUN verdict : mesure sous testServer sur cet arbre, les
#     <option> sont NUS (ex. <option value="donor_id">donor_id</option>),
#     il n'y a AUCUN bandeau avant le <select>, AUCUNE case « Je confirme »,
#     et le bouton « 1) Preparer la reference (disque) » (:254-255) n'est
#     JAMAIS desactive -- y compris colonne danger selectionnee.
#
# QU'EST ASSERTABLE HEADLESS (honnetete du garde, motif test-asr3-ui.R:15-29) :
#   * shiny::testServer boote le serveur REEL .deconv_reference_server
#     (modules/spatial/deconv/mod_spatial_deconv_reference.R:19, meme forme
#     d'appel que l'orchestrateur mod_spatial_deconv.R:72 : fonctions passees
#     en argument, ns identite) ; l'observable est le HTML des outputs rendus
#     (output$ref_celltype_col_ui -> list(html=, deps=), mesure) -- pas un
#     navigateur. Lecture de l'output IN-EXPR (l'expr de testServer n'est pas
#     une fonction : pas de return(), et un helper de niveau fichier ne voit
#     pas `output` -- mesure RED 1re passe).
#   * Objet reference = S4 minimal a slot meta.data : le module n'accede qu'a
#     @meta.data sur les chemins testes (:223, :319) -- zero dependance
#     Seurat dans le garde.
#   * app.R n'est jamais source (convention test-app-sourcing.R) ; les
#     fallbacks .t_fmt/.strip_i18n_html suivent test-asr3-ui.R:71-87.
#
# DECISIONS TRANCHÉES (chaque alternative documentée) :
#   D1 T3 vs §7-1 : « choisir warn/danger EXIGE la case » est realise comme
#      ceci -- la case « Je confirme » est RENDUE pour toute selection warn
#      OU danger (fraiche par colonne, D5) et ABSENTE pour ok ; le BLOCAGE
#      du bouton reste reserve au danger sans confirmation (T4, §7-2 :146).
#      Une warn selectionnee sans confirmation laisse le bouton ACTIF :
#      la fiche §7-1 (« execution possible mais eclairee ») est LA spec et
#      l'emporte sur la lettre du T3.
#   D2 Surface assertee = output$ref_celltype_col_ui (le site mesure
#      :220-258) : bandeau, badges, case et bouton y sont asserts ; le
#      bandeau doit preceder le <select> (« au-dessus du selecteur »).
#   D3 T1 litteral (« chaque option du selecteur porte son verdict ») : dans
#      chaque <option value="<colonne>">, une warn/danger porte un marqueur
#      de badge (classes primitives Bootstrap ou icone : badge|text-bg-|
#      bg-warning|bg-danger|text-warning|text-danger|exclamation|octagon)
#      PLUS la reason_key traduite ; une ok ne porte NI marqueur warn/danger
#      NI raison non-ok (le badge vert discret §4b est facultatif -- « rien
#      ou vert » -- et non epingle). Toutes les colonnes restent proposees
#      (§4c :114, aucune colonne retiree du choix).
#   D4 T4 : l'etat observable du blocage est l'attribut disabled du bouton
#      dans le HTML rendu (token \bdisabled\b -- couvre shiny::disabled(),
#      tagAppendAttributes(disabled=NA) et la classe Bootstrap .disabled),
#      mechanisme-agnostique. La garde serveur au clic n'est PAS exigee : un
#      bouton desactive est in-cliquable pour un utilisateur reel. En vertu
#      de §7-4, l'etat desactive doit etre EXPLIQUE a l'ecran (cle D6-3).
#   D5 Confirmation fraiche par colonne : apres avoir confirme un danger A,
#      passer a un danger B (sans nouvelle confirmation) RE-BLOQUE le bouton
#      -- la case est « obligatoire pour choisir » CHAQUE colonne warn/danger,
#      pas une fois pour toutes.
#   D6 Cles i18n SPMV-2 EPINGLEES ici (contrat, fr = cle, ASCII, motif
#      test-asr3-ui.R:93-111) ; elles n'existent pas encore dans
#      i18n/translation.json (append SCOPÉ au lot GREEN, lecon B1) :
#        1. « Je confirme : je veux utiliser cette colonne malgre
#           l'avertissement » (label de la case, warn et danger) ;
#        2. « Colonnes evaluees : {n_ok} compatibles, {n_warn} a verifier,
#           {n_danger} incompatibles. » (bandeau, .t_fmt interpole) ;
#        3. « Preparation bloquee : cochez la case de confirmation pour
#           utiliser cette colonne. » (explication de l'etat desactive, §7-4).
#      Les 8 reason_keys SPMV-1 EXISTENT deja (i18n/translation.json:11649-11678)
#      -- T6 n'en verifie que la non-regression fr/en.
#   D7 T5 : le passthrough du clic preparation est epingle par STUB de
#      prepare_reference_artifact dans globalenv (sauvegarde/restauration,
#      motif test-mod-spatial-qc-tabs.R:335-356) : colonne, merge_rare,
#      cap/max et RCTD_CELL_MIN_INSTANCE passent INTACTS au moteur ; une warn
#      sans confirmation prepare ENCORE (§7-1) ; meta.data est byte-identique
#      (serialize) apres toute la sequence (la validation ne modifie rien).
#   D8 La porte RED attend T1, T2, T3, T4 et T6 en ECHEC sur l'arbre actuel ;
#      T0 et T5 sont des garde-fous de non-regression (verts). Un skip n'est
#      pas une porte rouge (motif test-spmv1-meta-col-validation.R:43-45) :
#      chaque bloc echoue sur un expect explicite, jamais un skip -- les
#      continuations conditionnelles sont des if() explicites.
# =============================================================================

# ── Sources (composant SPMV-1 livre 715438e, consomme TEL QUEL) --------------

source_project_file("tools/pkg_skeleton/R/spatial_meta_col_validation.R")
source_project_file("modules/spatial/deconv/mod_spatial_deconv_reference.R")
# L'orchestrateur definit RCTD_CELL_MIN_INSTANCE (mod_spatial_deconv.R:19)
# consomme par le gestionnaire de preparation (:276) -- source pour la constante.
source_project_file("modules/spatial/deconv/mod_spatial_deconv.R")

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

# ── Contrat i18n SPMV-2 (fr = cle, decision D6) -------------------------------

.spmv2_key_confirm <- "Je confirme : je veux utiliser cette colonne malgre l'avertissement"
.spmv2_key_banner  <- paste0("Colonnes evaluees : {n_ok} compatibles, ",
                             "{n_warn} a verifier, {n_danger} incompatibles.")
.spmv2_key_block   <- "Preparation bloquee : cochez la case de confirmation pour utiliser cette colonne."

# ── Fixture : table §4b (meme convention que test-spmv1 :64-97), comptes
#    DISTINCTS par niveau pour le bandeau T2 (ok=4, warn=3, danger=2). --------

setClass("spmv2_fake_ref", slots = c(meta.data = "data.frame"))

.spmv2_n <- 500L

.spmv2_meta <- function() {
  data.frame(
    cell_type      = rep(c("A", "B", "C", "D", "E"), each = 100),
    cell_type_fact = factor(rep(c("Ast", "Oli", "Mic", "Exc", "Inh"), each = 100)),
    flag_logical   = rep(c(TRUE, FALSE), times = 250),
    histo_region   = rep(c("r1", "r2", "r3", "r4"), length.out = .spmv2_n),
    atlas_types    = rep(paste0("t", seq_len(150)), length.out = .spmv2_n),
    donor_id       = rep(c("D1", "D2", "D3"), c(200, 200, 100)),
    batch_label    = rep(c("b1", "b2", "b3", "b4"), each = 125),
    score_num      = seq(0, 1, length.out = .spmv2_n),
    barcode_col    = paste0("BC", seq_len(.spmv2_n)),
    row.names      = paste0("cell_", seq_len(.spmv2_n)),
    stringsAsFactors = FALSE
  )
}

.spmv2_ok       <- c("cell_type", "cell_type_fact", "flag_logical", "histo_region")
.spmv2_warn     <- c("atlas_types", "donor_id", "batch_label")
.spmv2_danger   <- c("score_num", "barcode_col")
.spmv2_cols     <- c(.spmv2_ok, .spmv2_warn, .spmv2_danger)
.spmv2_danger_a <- "barcode_col"   # danger A (confirmation fraiche, D5)
.spmv2_danger_b <- "score_num"     # danger B (confirmation fraiche, D5)
.spmv2_warn_col <- "donor_id"      # §7-1 : execution possible mais eclairee
.spmv2_ok_col   <- "cell_type"

.spmv2_verdict <- function(meta, col) spatial_meta_col_validation(meta, col)$level

# ── Extracteurs HTML (mesure testServer : renderUI -> list(html=, deps=)) ----
# Tolérants a NULL/"" : un rendu manquant produit un echec EXPLICITE au site
# d'appel (expect), jamais une erreur de harnais.

.spmv2_html <- function(out) {
  if (is.null(out)) return("")
  if (is.list(out)) out <- out$html
  paste(collapse = "", as.character(out))
}

.spmv2_before_select <- function(html) {
  if (is.null(html) || !nzchar(html)) return("")
  i <- regexpr("<select", html, fixed = TRUE)
  if (i < 0) html else substr(html, 1L, i - 1L)
}

.spmv2_btn <- function(html) {
  if (is.null(html) || !nzchar(html)) return(character(0))
  as.character(regmatches(html, regexpr("<button[^>]*btn_prepare_ref[^>]*>.*?</button>",
                                        html, perl = TRUE)))
}

.spmv2_btn_disabled <- function(btn_html) {
  length(btn_html) == 1L && nzchar(btn_html) && grepl("\\bdisabled\\b", btn_html)
}

.spmv2_options <- function(html) {
  if (is.null(html) || !nzchar(html)) return(character(0))
  as.character(regmatches(html, gregexpr("<option[^>]*>.*?</option>", html, perl = TRUE))[[1]])
}

.spmv2_option_of <- function(html, col) {
  opts <- .spmv2_options(html)
  hits <- opts[grepl(paste0('<option value="', col, '"'), opts, fixed = TRUE)]
  if (length(hits) == 1L) hits else character(0)
}

# Case « Je confirme » : id du checkbox/switch dont le label porte la cle
# (decouverte DYNAMIQUE -- l'id d'implementation n'est pas epingle).
.spmv2_confirm_id <- function(html) {
  if (is.null(html) || length(html) != 1L || is.na(html) || !nzchar(html)) return(NULL)
  m <- gregexpr("<input[^>]*>", html, perl = TRUE)[[1]]
  if (m[1] < 0) return(NULL)
  for (i in seq_along(m)) {
    tag <- substr(html, m[i], m[i] + attr(m, "match.length")[i] - 1L)
    if (!grepl('type="checkbox"|role="switch"', tag)) next
    idm <- regmatches(tag, regexpr('id="[^"]+"', tag))
    if (length(idm) == 0L) next
    id <- sub('id="([^"]+)"', "\\1", idm)
    start  <- m[i] + attr(m, "match.length")[i]
    after  <- substr(html, start, start + 400L)
    before <- substr(html, max(1L, m[i] - 400L), m[i])
    if (grepl("Je confirme", after, fixed = TRUE) ||
        grepl("Je confirme", before, fixed = TRUE)) return(id)
  }
  NULL
}

# ── Boot e2e : serveur REEL, ns identite, etat seme (motif qc-tabs :321-331) --

.spmv2_setup <- function() {
  meta <- .spmv2_meta()
  gd <- shiny::reactiveValues(language = "fr", i18n = NULL, spatial_reference = NULL)
  rv <- shiny::reactiveValues()
  handles <- new.env(parent = emptyenv())
  srv <- function(input, output, session) {
    ns <- function(x) x
    handles$ref <- .deconv_reference_server(input, output, session, ns, gd, rv)
    invisible(NULL)
  }
  list(meta = meta, srv = srv, handles = handles)
}

.spmv2_seed <- function(ref, meta) {
  ref$ref_state$obj    <- new("spmv2_fake_ref", meta.data = meta)
  ref$ref_state$status <- "loaded"
  ref$ref_meta_cols(colnames(meta))
}

# Marqueurs de verdict visibles (D3) -- classes primitives Bootstrap + icones.
.spmv2_badge_pat   <- "badge|text-bg-|bg-warning|bg-danger|text-warning|text-danger|exclamation|octagon"
.spmv2_badwarn_pat <- "bg-warning|bg-danger|text-warning|text-danger|text-bg-warning|text-bg-danger|exclamation|octagon"

# ── T0 -- surface : composant livre, fixture d'accord avec la table §4b -------

test_that("T0 SPMV-2 surface : le composant livre est source et la fixture colle a la table de verite §4b", {
  expect_true(exists("spatial_meta_col_validation", envir = globalenv()),
              info = "spatial_meta_col_validation introuvable -- SPMV-1 (commit 715438e) doit etre source")
  meta <- .spmv2_meta()
  for (col in .spmv2_ok) {
    expect_identical(.spmv2_verdict(meta, col), "ok",
                     info = paste("fixture drift :", col, "devrait valoir ok (§4b)"))
  }
  for (col in .spmv2_warn) {
    expect_identical(.spmv2_verdict(meta, col), "warn",
                     info = paste("fixture drift :", col, "devrait valoir warn (§4b)"))
  }
  for (col in .spmv2_danger) {
    expect_identical(.spmv2_verdict(meta, col), "danger",
                     info = paste("fixture drift :", col, "devrait valoir danger (§4b)"))
  }
  expect_identical(c(.spmv2_ok, .spmv2_warn, .spmv2_danger), colnames(meta),
                   info = "la fixture doit couvrir exactement les colonnes du meta.data")
})

# ── T1 -- badges : chaque option du selecteur porte son verdict visible -------

test_that("T1 SPMV-2 badges : chaque option porte son verdict (warn/danger = badge + reason_key traduit ; ok = ni l'un ni l'autre)", {
  st <- .spmv2_setup()
  shiny::testServer(st$srv, {
    ref <- st$handles$ref
    .spmv2_seed(ref, st$meta)
    session$flushReact()
    session$setInputs(ref_celltype_col = .spmv2_ok_col)
    session$flushReact()
    html <- .spmv2_html(tryCatch(output$ref_celltype_col_ui, error = function(e) NULL))

    if (!nzchar(html)) {
      expect_true(FALSE, info = "output$ref_celltype_col_ui doit se rendre (site :220-258) -- rendu vide/absent")
    } else {
      # Toutes les colonnes restent proposees (§4c :114 -- aucune colonne retiree).
      got_values <- vapply(.spmv2_options(html), function(o) {
        m <- regmatches(o, regexpr('value="[^"]+"', o))
        if (length(m) == 1L) sub('value="([^"]+)"', "\\1", m) else ""
      }, "")
      expect_true(setequal(got_values, .spmv2_cols) && !any(duplicated(got_values)),
                  info = paste("le selecteur doit proposer TOUTES les colonnes (§4c), chacune une fois",
                               "-- manque :", paste(setdiff(.spmv2_cols, got_values), collapse = ", "),
                               "| en trop :", paste(setdiff(got_values, .spmv2_cols), collapse = ", ")))

      # warn/danger : badge + reason_key traduit DANS l'option (D3).
      keys <- list()
      for (col in .spmv2_cols) {
        keys[[col]] <- spatial_meta_col_validation(st$meta, col)$reason_key
      }
      for (col in c(.spmv2_warn, .spmv2_danger)) {
        opt <- .spmv2_option_of(html, col)
        expect_true(length(opt) == 1L && nzchar(opt),
                    info = paste("option absente pour", col))
        if (length(opt) == 1L && nzchar(opt)) {
          expect_true(grepl(.spmv2_badge_pat, opt),
                      info = paste("AUCUN marqueur de badge visible sur l'option", col,
                                   "-- chaque option warn/danger doit porter son verdict (§6 :140)"))
          expect_true(nzchar(keys[[col]]) && grepl(keys[[col]], opt, fixed = TRUE),
                      info = paste("la reason_key du composant doit etre rendue (traduite) sur l'option",
                                   col, ":", keys[[col]]))
        }
      }

      # ok : ni marqueur warn/danger, ni raison non-ok (D3 -- « rien ou vert »).
      for (col in .spmv2_ok) {
        opt <- .spmv2_option_of(html, col)
        expect_true(length(opt) == 1L && nzchar(opt),
                    info = paste("option absente pour", col))
        if (length(opt) == 1L && nzchar(opt)) {
          expect_false(grepl(.spmv2_badwarn_pat, opt),
                       info = paste("l'option ok", col, "porte un marqueur warn/danger -- interdit"))
          for (other in c(.spmv2_warn, .spmv2_danger)) {
            if (nzchar(keys[[other]])) {
              expect_false(grepl(keys[[other]], opt, fixed = TRUE),
                           info = paste("l'option ok", col, "fuit la raison de", other))
            }
          }
        }
      }
    }
  })
})

# ── T2 -- bandeau : recapitulatif (n ok / n warn / n danger) AU-DESSUS du selecteur

test_that("T2 SPMV-2 bandeau : le recapitulatif n_ok/n_warn/n_danger est rendu au-dessus du selecteur", {
  st <- .spmv2_setup()
  shiny::testServer(st$srv, {
    ref <- st$handles$ref
    .spmv2_seed(ref, st$meta)
    session$flushReact()
    session$setInputs(ref_celltype_col = .spmv2_ok_col)
    session$flushReact()
    html <- .spmv2_html(tryCatch(output$ref_celltype_col_ui, error = function(e) NULL))

    if (!nzchar(html)) {
      fail("output$ref_celltype_col_ui doit se rendre -- rendu vide/absent")
    } else {
      n_ok     <- sum(vapply(.spmv2_ok,     function(c) identical(.spmv2_verdict(st$meta, c), "ok"), logical(1)))
      n_warn   <- sum(vapply(.spmv2_warn,   function(c) identical(.spmv2_verdict(st$meta, c), "warn"), logical(1)))
      n_danger <- sum(vapply(.spmv2_danger, function(c) identical(.spmv2_verdict(st$meta, c), "danger"), logical(1)))
      expected_banner <- .t_fmt(.spmv2_key_banner,
                                n_ok = n_ok, n_warn = n_warn, n_danger = n_danger)

      expect_true(grepl("<select", html, fixed = TRUE),
                  info = "le selecteur ref_celltype_col doit etre dans le rendu")
      before <- .spmv2_before_select(html)
      expect_true(grepl(expected_banner, before, fixed = TRUE),
                  info = paste("bandeau recapitulatif absent AVANT le selecteur -- attendu (rendu,",
                               "repli i18n = cle interpolee) :", expected_banner))
      expect_true(grepl("alert", before),
                  info = "le bandeau doit etre une primitive alert Bootstrap/bslib (0 CSS www/)")
    }
  })
})

# ── T3 -- confirmation : la case « Je confirme » pour warn/danger, pas pour ok -

test_that("T3 SPMV-2 confirmation : la case « Je confirme » est rendue pour warn/danger (fraiche par colonne), absente pour ok", {
  st <- .spmv2_setup()
  shiny::testServer(st$srv, {
    ref <- st$handles$ref
    .spmv2_seed(ref, st$meta)
    session$flushReact()

    read_ui <- function(col) {
      session$setInputs(ref_celltype_col = col)
      session$flushReact()
      .spmv2_html(tryCatch(output$ref_celltype_col_ui, error = function(e) NULL))
    }

    # ok : AUCUNE case exigee (D1).
    html_ok <- read_ui(.spmv2_ok_col)
    if (!nzchar(html_ok)) {
      expect_true(FALSE, info = "rendu ok manquant (site :220-258)")
    } else {
      expect_false(grepl("Je confirme", html_ok, fixed = TRUE),
                   info = "la case « Je confirme » ne doit PAS etre rendue pour une colonne ok (ok ne l'exige pas)")

      # warn : case rendue.
      html_warn <- read_ui(.spmv2_warn_col)
      expect_true(nzchar(html_warn) && grepl("Je confirme", html_warn, fixed = TRUE),
                  info = "choisir une colonne warn EXIGE la case « Je confirme » -- absente du rendu")

      # danger : case rendue.
      html_danger <- read_ui(.spmv2_danger_a)
      expect_true(nzchar(html_danger) && grepl("Je confirme", html_danger, fixed = TRUE),
                  info = "choisir une colonne danger EXIGE la case « Je confirme » -- absente du rendu")

      # Fraiche par colonne (D5) : confirmer le danger A, puis passer au danger
      # B sans nouvelle confirmation -> le bouton est RE-BLOQUE.
      confirm_id <- .spmv2_confirm_id(html_danger)
      expect_false(is.null(confirm_id),
                   info = "impossible de localiser le checkbox « Je confirme » dans le rendu danger")
      if (!is.null(confirm_id)) {
        do.call(session$setInputs, setNames(list(TRUE), confirm_id))
        session$flushReact()
        html_confirmed <- read_ui(.spmv2_danger_a)
        btn_a <- .spmv2_btn(html_confirmed)
        expect_true(length(btn_a) == 1L && !.spmv2_btn_disabled(btn_a),
                    info = "apres confirmation explicite, le danger A doit etre utilisable (bouton actif)")

        html_b <- read_ui(.spmv2_danger_b)
        btn_b <- .spmv2_btn(html_b)
        expect_true(length(btn_b) == 1L && .spmv2_btn_disabled(btn_b),
                    info = paste("la confirmation du danger A ne doit PAS s'appliquer au danger B",
                                 "(case « Je confirme » obligatoire pour choisir CHAQUE colonne, D5) :",
                                 .spmv2_danger_b, "restait actif sans nouvelle confirmation"))
      }
    }
  })
})

# ── T4 -- blocage (§7-2) : bouton desactive ssi danger sans confirmation ------

test_that("T4 SPMV-2 blocage §7-2 : bouton desactive ssi danger sans confirmation, avec explication visible (§7-4)", {
  st <- .spmv2_setup()
  shiny::testServer(st$srv, {
    ref <- st$handles$ref
    .spmv2_seed(ref, st$meta)
    session$flushReact()

    read_ui <- function(col) {
      session$setInputs(ref_celltype_col = col)
      session$flushReact()
      .spmv2_html(tryCatch(output$ref_celltype_col_ui, error = function(e) NULL))
    }

    # (a) ok -> actif.
    html_ok <- read_ui(.spmv2_ok_col)
    btn_ok <- .spmv2_btn(html_ok)
    expect_true(length(btn_ok) == 1L && nzchar(btn_ok),
                info = "bouton « 1) Preparer la reference » introuvable dans le rendu ok")
    if (length(btn_ok) == 1L) {
      expect_false(.spmv2_btn_disabled(btn_ok),
                   info = "une colonne ok ne doit pas desactiver le bouton de preparation")
    }

    # (b) danger sans confirmation -> DESACTIVE + explique (§7-2, §7-4).
    html_danger <- read_ui(.spmv2_danger_a)
    btn_danger <- .spmv2_btn(html_danger)
    expect_true(length(btn_danger) == 1L && .spmv2_btn_disabled(btn_danger),
                info = paste("§7-2 : le bouton « 1) Preparer la reference » doit etre DESACTIVE",
                             "tant que la colonne retenue est danger (", .spmv2_danger_a,
                             ") sans confirmation explicite -- mesure : actif"))
    expect_true(nzchar(html_danger) && grepl(.spmv2_key_block, html_danger, fixed = TRUE),
                info = "l'etat desactive doit etre explique a l'ecran (§7-4, aucun comportement silencieux)")

    # (c) danger + confirmation explicite -> actif.
    confirm_id <- .spmv2_confirm_id(html_danger)
    expect_false(is.null(confirm_id),
                 info = "case « Je confirme » introuvable -- le blocage §7-2 ne peut pas etre leve")
    if (!is.null(confirm_id)) {
      do.call(session$setInputs, setNames(list(TRUE), confirm_id))
      session$flushReact()
      html_conf <- read_ui(.spmv2_danger_a)
      btn_conf <- .spmv2_btn(html_conf)
      expect_true(length(btn_conf) == 1L && !.spmv2_btn_disabled(btn_conf),
                  info = "apres confirmation explicite, le bouton doit redevenir actif (contournement explicite, §8 :159-160)")
    }

    # (d) warn SANS confirmation -> actif (§7-1 : execution possible mais eclairee).
    html_warn <- read_ui(.spmv2_warn_col)
    btn_warn <- .spmv2_btn(html_warn)
    expect_true(length(btn_warn) == 1L && !.spmv2_btn_disabled(btn_warn),
                info = paste("une colonne warn ne doit pas desactiver le bouton (§7-1 :",
                             "execution possible mais eclairee)"))
  })
})

# ── T5 -- zero changement scientifique (§5 :132-133) : structure + passthrough -

test_that("T5 SPMV-2 zero changement scientifique : merge_rare/cap/moteur intacts, passthrough du clic preparation, meta.data immune", {
  # (a) Structurel : les garde-fous du site et le moteur restent en place
  #     (motif spmv1 T2e -- jugement sur le CODE, commentaires exclus,
  #     precedent test-biostat-tree.R:69-74).
  mod_lines <- readLines(file.path(ts_project_root(), "modules", "spatial", "deconv",
                                   "mod_spatial_deconv_reference.R"), warn = FALSE)
  mod_code <- paste(sub("#.*$", "", mod_lines), collapse = "\n")
  expect_true(all(vapply(c("merge_rare_types", "cap_ref_cells", "max_cells_per_type",
                           "prepare_reference_artifact", "RCTD_CELL_MIN_INSTANCE",
                           "btn_prepare_ref", "spatial_meta_col_validation"),
                         function(tok) grepl(tok, mod_code, fixed = TRUE), logical(1))),
              info = "les garde-fous scientifiques du site doivent rester intacts (§5)")
  eng_lines <- readLines(file.path(ts_project_root(), "tools", "pkg_skeleton", "R",
                                   "spatial_deconv_tasks.R"), warn = FALSE)
  expect_true(grepl("run_spatial_deconv_body <- function", paste(eng_lines, collapse = "\n"), fixed = TRUE),
              info = paste("le moteur de deconvolution (run_spatial_deconv_body, mesure dans",
                           "tools/pkg_skeleton/R/spatial_deconv_tasks.R:72) doit rester intact"))
  orch_lines <- readLines(file.path(ts_project_root(), "modules", "spatial", "deconv",
                                    "mod_spatial_deconv.R"), warn = FALSE)
  orch_code <- paste(sub("#.*$", "", orch_lines), collapse = "\n")
  expect_true(grepl("reference_is_ready", orch_code, fixed = TRUE) &&
                grepl(".deconv_reference_server", orch_code, fixed = TRUE),
              info = "le cablage orchestrateur -> reference (porte reference_is_ready) doit rester intact")
  # Le composant livre ne rend RIEN (aucune primitive UI) -- il juge (en-tete :48-49).
  comp_lines <- readLines(file.path(ts_project_root(), "tools", "pkg_skeleton", "R",
                                    "spatial_meta_col_validation.R"), warn = FALSE)
  comp_code <- paste(sub("#.*$", "", comp_lines), collapse = "\n")
  expect_false(grepl("selectInput|actionButton|checkboxInput|renderUI", comp_code),
               info = "le composant SPMV-1 ne doit pas se mettre a rendre -- il reste pur (§4a)")

  # (b)+(c) Comportemental : stub du moteur de preparation (sauvegarde/
  #     restauration, motif qc-tabs:335-356), puis passthrough et immunité.
  st <- .spmv2_setup()
  meta <- st$meta
  ser_before <- serialize(meta, NULL)
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

  shiny::testServer(st$srv, {
    ref <- st$handles$ref
    .spmv2_seed(ref, meta)
    session$flushReact()

    # Colonne ok, merge_rare DECoche, cap actif a 100 : les valeurs passent
    # INTACTES au moteur (la couche de validation n'ajoute rien).
    session$setInputs(ref_celltype_col = .spmv2_ok_col, merge_rare_types = FALSE,
                      cap_ref_cells = TRUE, max_cells_per_type = 100)
    session$flushReact()
    session$setInputs(btn_prepare_ref = 1L)
    session$flushReact()
    expect_true(length(calls) >= 1L,
                info = "le clic preparation sur colonne ok doit appeler le moteur")
    if (length(calls) >= 1L) {
      expect_identical(calls[[1]]$celltype_col, .spmv2_ok_col,
                       info = "la colonne passee au moteur doit etre la selection (inchangee)")
      expect_false(isTRUE(calls[[1]]$merge_rare_types),
                   info = "merge_rare_types doit passer intact au moteur")
      expect_identical(calls[[1]]$max_cells_per_type, 100,
                       info = "cap/max_cells_per_type doit passer intact au moteur")
      expect_identical(calls[[1]]$min_cells_per_type,
                       if (exists("RCTD_CELL_MIN_INSTANCE")) RCTD_CELL_MIN_INSTANCE else 25L,
                       info = "RCTD_CELL_MIN_INSTANCE doit passer intact au moteur")
      expect_false(is.null(ref$ref_path()),
                   info = "apres preparation reussie, ref_path doit etre pose")
    }

    # §7-1 : une warn SANS confirmation prepare ENCORE (execution eclairee).
    session$setInputs(ref_celltype_col = .spmv2_warn_col)
    session$flushReact()
    n_before_warn <- length(calls)
    session$setInputs(btn_prepare_ref = 1L)
    session$flushReact()
    expect_true(length(calls) > n_before_warn,
                info = "§7-1 : une colonne warn sans confirmation doit rester preparable (execution possible mais eclairee)")

    # La validation ne modifie rien (§5) : meta.data byte-identique.
    expect_identical(serialize(meta, NULL), ser_before,
                     info = "meta.data modifie par la couche de rendu/validation -- interdit (§5 :132-133)")
    expect_identical(rownames(meta), paste0("cell_", seq_len(.spmv2_n)),
                     info = "row.names de meta.data modifies -- interdit")
  })
})

# ── T6 -- i18n fr/en des libelles du rendu (append SCOPE) + 0 CSS www/ --------

test_that("T6 SPMV-2 i18n : les 3 cles du rendu ont leur paire fr/en (append SCOPE), reason_keys SPMV-1 intacts ; 0 CSS www/", {
  path <- file.path(ts_project_root(), "i18n", "translation.json")
  expect_true(file.exists(path), info = "i18n/translation.json absent")
  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  fr <- vapply(j$translation, function(x) if (is.null(x$fr)) "" else x$fr, character(1))
  en <- vapply(j$translation, function(x) if (is.null(x$en)) "" else x$en, character(1))

  # (a) Les 3 cles SPMV-2 epinglees (D6) -- absentes de l'arbre actuel : RED.
  for (k in c(.spmv2_key_confirm, .spmv2_key_banner, .spmv2_key_block)) {
    idx <- match(k, fr)
    expect_false(is.na(idx),
                 info = paste("cle FR absente de translation.json :", k,
                              "-- append SCOPE requis (tool au lot seul puis restaure, lecon B1)"))
    if (!is.na(idx)) {
      expect_true(nzchar(en[[idx]]),
                  info = paste("paire EN absente ou vide pour :", k))
    }
  }

  # (b) Non-regression : les reason_keys SPMV-1 (translation.json:11649-11678)
  #     restent paires fr/en -- les badges les consomment tels quels.
  meta <- .spmv2_meta()
  reason_keys <- unique(vapply(.spmv2_cols, function(col) {
    r <- spatial_meta_col_validation(meta, col)
    if (identical(r$level, "ok")) "" else paste(as.character(r$reason_key), collapse = " ")
  }, ""))
  reason_keys <- reason_keys[nzchar(reason_keys)]
  expect_true(length(reason_keys) >= 4L,
              info = "au moins 4 reason_keys distincts attendus (causes §4b)")
  for (k in reason_keys) {
    idx <- match(k, fr)
    expect_false(is.na(idx), info = paste("reason_key SPMV-1 disparu de translation.json :", k))
    if (!is.na(idx)) expect_true(nzchar(en[[idx]]),
                                 info = paste("paire EN absente pour le reason_key :", k))
  }

  # (c) 0 CSS www/ (regle post-O13) : aucune feuille de style SPMV, aucune
  #     injection de style dans le module (primitives bslib/shiny seulement).
  www <- file.path(ts_project_root(), "www")
  if (dir.exists(www)) {
    css <- list.files(www, pattern = "css$", recursive = TRUE, ignore.case = TRUE, full.names = FALSE)
    expect_equal(length(css[grep("spmv|meta_col", css, ignore.case = TRUE)]), 0L,
                 info = "un fichier CSS SPMV-2 est apparu dans www/ -- interdit (regle post-O13)")
  }
  mod_lines <- readLines(file.path(ts_project_root(), "modules", "spatial", "deconv",
                                   "mod_spatial_deconv_reference.R"), warn = FALSE)
  mod_code <- paste(sub("#.*$", "", mod_lines), collapse = "\n")
  expect_false(grepl("tags\\$style|includeCSS|tags\\$head", mod_code),
               info = "le rendu SPMV-2 injecte du style -- interdit (0 CSS, primitives seulement)")
})

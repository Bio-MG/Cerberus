# =============================================================================
# test-spmv1-meta-col-validation.R -- SPMV-1 : composant unique de validation
# des colonnes de metadonnees Spatial (RED-first)
# =============================================================================
# FICHE (fait foi) : docs/proposals/SPATIAL_META_COL_VALIDATION_PROPOSAL.md
# (2026-10-26, arbitrage GO sur recommandation retenue, operateur 2026-11-02).
#
# Jalon SPMV-1 (fiche §6 :139) :
#   « Composant de validation + tests unitaires (cases o/o/x de §4b,
#     table de verite) » -- Gate : « RED-first sur fixtures ; conventions
#     0 err ; 1 commit ».
#
# Contrat du composant (fiche §4a :92-97) : UNE fonction
#   spatial_meta_col_validation(meta.data, col)
#     -> list(level = "ok" | "warn" | "danger", reason_key, detail)
# dans tools/pkg_skeleton/R/ (convention anti-duplication, precedents
# session_checkpoint.R / biostat_tree.R) -- PAS un patch par site.
# L'outil VALIDE et EXPLIQUE (fallback silencieux -> message explicite,
# fiche §2d-1 :63-66 et §4c :112-116) ; il ne transforme ni ne corrige
# AUCUNE donnee (fiche §5 :132-133 : RCTD / Label Transfer / merge_rare /
# cap inchanges ; la seule couche qui evolue est la selection/affichage).
#
# Table de verite §4b (:101-106) -- les fixtures ci-dessous en sont la
# traduction 1:1 (n = 500 cellules, meta.data-like) :
#   ok     : categoriel (character/factor/logical), 2-100 niveaux, nom sain
#   warn   : 101-200 niveaux ; OU nom suspect
#            (sample|donor|patient|batch|lane|run|orig\.ident|barcode|
#             cell[_ ]?id|spot|uuid|(^|_)id) ; OU un niveau >= 99 % des cellules
#   danger : numerique continue (> 20 valeurs distinctes) ; OU nunique == n
#            (identifiant par cellule) ; OU 1 seul niveau
# Cas d'echec couverts (fiche §3 :80-86) : E1 = donor_id (warn), E2 = barcode
# (danger), E3 = numerique continue (danger).
#
# QU'EST ASSERTABLE HEADLESS (honnetete du garde, motif test-asr3-ui.R :15-29) :
#   * Le composant est de la logique PURE (entree = data.frame + nom de
#     colonne) : verdicts, honnetete des raisons, immutabilite des donnees
#     et pins d'arbre sont assertables ici.
#   * Le RENDU du badge/bandeau et le bouton « Je confirme » sont le jalon
#     SPMV-2 (cablage ref_celltype_col, fiche §6 :140) -- PAS affiches ici ;
#     T2 n'epingle que la CONSOMMATION structurelle (composant unique, pas
#     de patch par site).
#
# RED : tant que tools/pkg_skeleton/R/spatial_meta_col_validation.R n'existe
# pas, chaque bloc T* echoue sur son premier expect_true explicite (jamais un
# skip -- un skip n'est pas une porte rouge ; motif test-biostat-tree.R:53-67).
# Gardes en ASCII pur : un garde ne doit pas dependre de la locale (C7).
# =============================================================================

.spmv_comp_rel  <- "tools/pkg_skeleton/R/spatial_meta_col_validation.R"
.spmv_comp_path <- file.path(ts_project_root(), .spmv_comp_rel)
.spmv_present   <- file.exists(.spmv_comp_path)
.spmv_red_info  <- paste(
  .spmv_comp_rel, "absent : SPMV-1 (composant unique de validation des",
  "colonnes de metadonnees Spatial, fiche SPATIAL_META_COL_VALIDATION_PROPOSAL.md",
  "§4a/§6) non livre -- sans effet executant tant que le fichier n'existe pas"
)

if (.spmv_present) {
  source_project_file(.spmv_comp_rel)
}

# ── Fixtures : table de verite §4b (deterministes, sans NA) ------------------

.spmv_n <- 500L

.spmv_meta <- function() {
  data.frame(
    cell_type       = rep(c("A", "B", "C", "D", "E"), each = 100),
    cell_type_fact  = factor(rep(c("Ast", "Oli", "Mic", "Exc", "Inh"), each = 100)),
    flag_logical    = rep(c(TRUE, FALSE), times = 250),
    atlas_types     = rep(paste0("t", seq_len(150)), length.out = .spmv_n),
    donor_id        = rep(c("D1", "D2", "D3"), c(200, 200, 100)),
    batch_label     = rep(c("b1", "b2", "b3", "b4"), each = 125),
    skewed          = c(rep("maj", 496), "m1", "m2", "m3", "m4"),
    score_num       = seq(0, 1, length.out = .spmv_n),
    barcode_col     = paste0("BC", seq_len(.spmv_n)),
    constant_col    = rep("A", .spmv_n),
    row.names       = paste0("cell_", seq_len(.spmv_n)),
    stringsAsFactors = FALSE
  )
}

# Verdicts attendus, table §4b 1:1 (noms des fixtures choisis SANS motif
# suspect, sauf donor_id/batch_label/barcode_col qui le portent volontairement).

.spmv_expect_map <- c(
  cell_type      = "ok",     # character, 5 niveaux, nom sain
  cell_type_fact = "ok",     # factor, 5 niveaux (§4b « categoriel »)
  flag_logical   = "ok",     # logical, 2 niveaux
  atlas_types    = "warn",   # 101-200 niveaux (borne 100 vs 200, fiche :107-110)
  donor_id       = "warn",   # nom suspect -- scenario E1 (fiche §3 :84)
  batch_label    = "warn",   # nom suspect (motif batch)
  skewed         = "warn",   # un niveau couvre 496/500 = 99.2 % >= 99 %
  score_num      = "danger", # numerique continue (> 20 distinctes) -- E3
  barcode_col    = "danger", # nunique == n (500/500) -- E2
  constant_col   = "danger"  # un seul niveau
)

.spmv_detail_txt <- function(res) {
  # Coercion tolérante : detail peut etre une chaine, un vecteur ou une
  # liste structurée (ex. list(n_levels = 150)) -- format() couvre tout.
  paste(format(res$detail), collapse = " ")
}

.spmv_absent_res <- function(meta) {
  # Colonnes absente : le contrat §4a est une liste typee ; une erreur
  # SIGNALEE explicite est aussi un verdict explicite (jamais un fallback
  # silencieux). On accepte l'un ou l'autre, jamais un "ok".
  tryCatch(.spmv_call(meta, "colonne_absente"), error = function(e) e)
}

.spmv_call <- function(meta, col) {
  spatial_meta_col_validation(meta, col)
}

# ── T0 -- porte RED explicite (le fichier existe ; jamais un skip) -----------

test_that("T0 RED : le fichier SPMV-1 tools/pkg_skeleton/R/spatial_meta_col_validation.R existe", {
  expect_true(.spmv_present, info = .spmv_red_info)
})

# ── T1 -- surface + verdicts explicites (present / absent / multi-modalites) --

test_that("T1 SPMV-1 surface : verdict explicite pour chaque colonne de la table de verite §4b, jamais de fallback silencieux", {
  expect_true(.spmv_present, info = .spmv_red_info)
  if (!.spmv_present) return(invisible(NULL))

  meta <- .spmv_meta()

  # Contrat §4a : sortie = liste typee level/reason_key/detail
  for (col in names(.spmv_expect_map)) {
    res <- .spmv_call(meta, col)
    expect_true(
      is.list(res) && all(c("level", "reason_key", "detail") %in% names(res)),
      info = paste("contrat §4a viole pour", col,
                   ": sortie attendue list(level, reason_key, detail)")
    )
    expect_identical(
      res$level, unname(.spmv_expect_map[[col]]),
      info = paste("table de verite §4b : verdict inattendu pour", col)
    )
  }

  # Multi-modalites : caracteriel ET factor ET logical categoriels restent
  # « ok » (§4b : character/factor/logical) -- le composant ne doit pas
  # degrader un cas legitime (alternative « restreindre strictement »
  # rejetee, fiche §8 :154-156).
  expect_identical(.spmv_call(meta, "cell_type_fact")$level, "ok",
                   info = "un factor categoriel sain doit rester ok")
  expect_identical(.spmv_call(meta, "flag_logical")$level, "ok",
                   info = "un logical categoriel sain doit rester ok")

  # Colonne ABSENTE : verdict explicite -- liste typee non-ok OU erreur
  # signalee avec message ; JAMAIS un "ok" (jamais de fallback silencieux,
  # fiche §2d-1/§4c).
  res_abs <- .spmv_absent_res(meta)
  if (inherits(res_abs, "error")) {
    expect_true(nzchar(trimws(conditionMessage(res_abs))),
                info = "l'erreur sur colonne absente doit etre explicite")
  } else {
    expect_true(
      is.list(res_abs) && identical(res_abs$level %in% c("warn", "danger"), TRUE),
      info = "une colonne absente ne peut JAMAIS valoir ok -- verdict explicite exigé"
    )
    expect_true(nzchar(res_abs$reason_key),
                info = "une colonne absente doit porter une raison")
  }
})

# ── T2 -- integration : le site de la fiche consomme le composant UNIQUE -----

test_that("T2 SPMV-1 integration : consommation du composant UNIQUE au site de la fiche (pas de patch par site), surfaces §2b intactes", {
  # (a) Unicite : exactement UNE definition, dans tools/pkg_skeleton/R/
  #     (fiche §4a « Un composant réutilisable, pas des copies »).
  expect_true(.spmv_present, info = .spmv_red_info)
  roots <- c("modules", "R", "tools/pkg_skeleton/R")
  defs <- character(0)
  for (r in roots) {
    d <- file.path(ts_project_root(), r)
    if (!dir.exists(d)) next
    for (f in list.files(d, pattern = "[.]R$", recursive = TRUE, full.names = TRUE)) {
      src <- paste(readLines(f, warn = FALSE), collapse = "\n")
      if (grepl("spatial_meta_col_validation[[:space:]]*(<-|=)[[:space:]]*function", src)) {
        defs <- c(defs, f)
      }
    }
  }
  expect_identical(
    normalizePath(defs, winslash = "/", mustWork = FALSE),
    normalizePath(.spmv_comp_path, winslash = "/", mustWork = FALSE),
    info = paste("spatial_meta_col_validation doit etre definie UNE seule fois,",
                 "dans tools/pkg_skeleton/R/ (§4a anti-duplication) -- defini dans :",
                 paste(defs, collapse = ", "))
  )

  # (b) Le site du perimetre initial (§4d :120 : ref_celltype_col,
  #     modules/spatial/deconv/mod_spatial_deconv_reference.R) consomme le
  #     composant -- la garde §2a (fenetre 2-200 + tri) est REMPLACEE par le
  #     composant, pas dupliquee a cote.
  mod_lines <- readLines(
    file.path(ts_project_root(), "modules", "spatial", "deconv",
              "mod_spatial_deconv_reference.R"), warn = FALSE)
  # Les gardes jugent le CODE, pas les commentaires (precedent
  # test-biostat-tree.R:69-74 : chaque ligne coupee au premier "#").
  mod_code <- paste(sub("#.*$", "", mod_lines), collapse = "\n")
  expect_true(grepl("spatial_meta_col_validation", mod_code, fixed = TRUE),
              info = paste("le site de la fiche (ref_celltype_col,",
                           "mod_spatial_deconv_reference.R) doit consommer le",
                           "composant unique spatial_meta_col_validation"))
  # (c) Le fallback silencieux (§2d-1 : « else cols ») disparait -- mesure au
  #     dépôt : mod_spatial_deconv_reference.R:228. Le RENDU des badges et
  #     l'ordre visible restent SPMV-2 ; ici on refuse seulement le retour
  #     en arriere vers le fallback muet.
  expect_false(grepl("else cols", mod_code, fixed = TRUE),
               info = paste("le fallback silencieux §2d-1 (else cols) doit",
                            "disparaitre -- chaque restriction est expliquee,",
                            "jamais triee en silence (§4c)"))

  # (d) Surfaces protegees par conception (§2b :41-48) INCHANGEES : valeurs
  #     calculees uniquement, jamais une colonne brute de meta.data.
  viz_src <- paste(readLines(
    file.path(ts_project_root(), "modules", "spatial", "mod_spatial_viz.R"),
    warn = FALSE), collapse = "\n")
  expect_true(grepl('c("qc", "cluster", "deconv", "niche", "gene")', viz_src,
                    fixed = TRUE),
              info = "mod_spatial_viz.R color_by : valeurs calculees -- ne doit jamais exposer une colonne brute (§2b)")
  niche_src <- paste(readLines(
    file.path(ts_project_root(), "modules", "spatial", "mod_spatial_niche.R"),
    warn = FALSE), collapse = "\n")
  expect_true(grepl(".group_choices", niche_src, fixed = TRUE),
              info = "mod_spatial_niche.R : regroupements calcules (.group_choices) inchanges (§2b)")

  # (e) Zéro changement scientifique (fiche §5 :132-133) : merge_rare / cap
  #     / preparation d'artefact du site restent en place. (grepl = UN motif
  #     par appel -- piège run_tests.R:31-36 -- d'où le vapply.)
  expect_true(all(vapply(c("merge_rare_types", "cap_ref_cells", "btn_prepare_ref"),
                         function(tok) grepl(tok, mod_code, fixed = TRUE),
                         logical(1))),
              info = "les garde-fous scientifiques du site (merge_rare/cap/preparation) doivent rester intacts (§5)")
})

# ── T3 -- honnetete : chaque non-ok explique POURQUOI (modalites, type, nom) --

test_that("T3 SPMV-1 honnetete : les raisons distinguent les causes (nom vs modalites vs type) et citent la mesure", {
  expect_true(.spmv_present, info = .spmv_red_info)
  if (!.spmv_present) return(invisible(NULL))

  meta <- .spmv_meta()
  keys <- vapply(names(.spmv_expect_map),
                 function(col) paste(as.character(.spmv_call(meta, col)$reason_key),
                                     collapse = " "),
                 "")
  names(keys) <- names(.spmv_expect_map)

  # Tout non-ok porte une raison non vide (§4b : « texte expliquant pourquoi »).
  for (col in names(.spmv_expect_map)) {
    res <- .spmv_call(meta, col)
    if (!identical(res$level, "ok")) {
      expect_true(nzchar(keys[[col]]),
                  info = paste("raison absente (reason_key vide) pour", col))
      expect_true(nzchar(trimws(.spmv_detail_txt(res))),
                  info = paste("detail vide pour", col, "-- l'explication doit porter la mesure"))
    }
  }

  # Causes de NATURE differente -> cles dediees differentes (§4b « clé i18n
  # dédiée ») : nom suspect (donor_id) vs fenetre de modalites (atlas_types)
  # vs type numerique (score_num) -- T3 « modalites, type ».
  expect_false(identical(keys[["donor_id"]], keys[["atlas_types"]]),
               info = "cause nom suspect vs cause 101-200 niveaux : reason_keys differents exigés")
  expect_false(identical(keys[["donor_id"]], keys[["score_num"]]),
               info = "cause nom suspect vs cause numerique continue : reason_keys differents exigés")
  expect_false(identical(keys[["atlas_types"]], keys[["score_num"]]),
               info = "cause modalites vs cause type numerique : reason_keys differents exigés")

  # Les quatre familles non-ok de la table §4b ne se reduisent pas a une
  # cle unique generique (sinon l'ecran n'explique rien).
  distinct_keys <- unique(c(keys[["atlas_types"]], keys[["donor_id"]],
                            keys[["batch_label"]], keys[["skewed"]],
                            keys[["score_num"]], keys[["barcode_col"]],
                            keys[["constant_col"]]))
  expect_true(length(distinct_keys) >= 4L,
              info = paste("au moins 4 cles de cause distinctes attendues",
                           "(fenetre / nom / dominance / type), observe :",
                           paste(distinct_keys, collapse = " | ")))

  # L'explication cite la MESURE : 150 niveaux pour atlas_types, 500 == n
  # pour barcode_col (nunique == n, §4b danger).
  expect_true(grepl("150", .spmv_detail_txt(.spmv_call(meta, "atlas_types")),
                    fixed = TRUE),
              info = "le detail d'atlas_types doit citer la mesure (150 niveaux)")
  expect_true(grepl("500", .spmv_detail_txt(.spmv_call(meta, "barcode_col")),
                    fixed = TRUE),
              info = "le detail de barcode_col doit citer la mesure (500 valeurs uniques = n)")
})

# ── T4 -- i18n fr/en des chaines visibles (append SCOPE) + 0 CSS www/ ---------

test_that("T4 SPMV-1 i18n : chaque reason_key visible a sa paire fr/en dans translation.json (append scoping) ; 0 CSS www/", {
  expect_true(.spmv_present, info = .spmv_red_info)
  if (!.spmv_present) return(invisible(NULL))

  meta <- .spmv_meta()
  cols <- names(.spmv_expect_map)
  keys <- unique(vapply(cols, function(col) {
    res <- .spmv_call(meta, col)
    if (identical(res$level, "ok")) "" else paste(as.character(res$reason_key), collapse = " ")
  }, ""))
  keys <- keys[nzchar(keys)]
  res_abs <- .spmv_absent_res(meta)
  if (is.list(res_abs) && !identical(res_abs$level, "ok")) {
    keys <- unique(c(keys, as.character(res_abs$reason_key)))
  }
  expect_true(length(keys) >= 4L,
              info = "au moins 4 cles i18n visibles attendues (causes distinctes)")

  path <- file.path(ts_project_root(), "i18n", "translation.json")
  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  fr <- vapply(j$translation, function(x) if (is.null(x$fr)) "" else x$fr, character(1))
  en <- vapply(j$translation, function(x) if (is.null(x$en)) "" else x$en, character(1))
  for (k in keys) {
    idx <- match(k, fr)
    expect_false(is.na(idx),
                 info = paste("cle FR absente de translation.json :", k,
                              "-- append SCOPE requis (tool au lot seul puis",
                              "restaure, lecon B1 add_i18n_keys.R:13-18)"))
    if (!is.na(idx)) {
      expect_true(nzchar(en[[idx]]),
                  info = paste("paire EN absente ou vide pour :", k))
    }
  }

  # 0 CSS www/ (regle post-O13) : primitives bslib/shiny seulement.
  comp_src <- paste(readLines(.spmv_comp_path, warn = FALSE), collapse = "\n")
  expect_false(grepl("tags\\$style|includeCSS|tags\\$head", comp_src),
               info = "le composant SPMV-1 injecte du style -- interdit (0 CSS, primitives seulement)")
  www <- file.path(ts_project_root(), "www")
  if (dir.exists(www)) {
    css <- list.files(www, pattern = "css$", recursive = TRUE, ignore.case = TRUE, full.names = FALSE)
    expect_equal(length(css[grep("spmv|meta_col", css, ignore.case = TRUE)]), 0L,
                 info = "un fichier CSS SPMV-1 est apparu dans www/ -- interdit (regle post-O13)")
  }
})

# ── T5 -- zero changement scientifique : les donnees ne sont JAMAIS modifiees --

test_that("T5 SPMV-1 immunite des donnees : le composant VALIDE et EXPLIQUE, ne transforme ni ne corrige rien", {
  expect_true(.spmv_present, info = .spmv_red_info)
  if (!.spmv_present) return(invisible(NULL))

  meta <- .spmv_meta()
  ser_before <- serialize(meta, NULL)
  cols_copy  <- lapply(meta, function(x) x)

  # Validation de TOUTES les colonnes (tous les chemins de la table §4b)
  # + colonne absente : aucun chemin ne doit ecrire dans meta.data.
  for (col in names(.spmv_expect_map)) invisible(.spmv_call(meta, col))
  invisible(.spmv_absent_res(meta))

  expect_identical(serialize(meta, NULL), ser_before,
                   info = paste("meta.data modifie par le composant -- interdit",
                                "(l'outil VALIDE, il ne transforme rien,",
                                "fiche §5 :132-133)"))
  for (nm in names(meta)) {
    expect_identical(meta[[nm]], cols_copy[[nm]],
                     info = paste("colonne modifiee par le composant :", nm))
  }
  # row.names (signature meta.data Seurat) intacts.
  expect_identical(rownames(meta), paste0("cell_", seq_len(.spmv_n)),
                   info = "row.names de meta.data modifies -- interdit")
})

# ── T6 -- pins d'arbre (nouveau fichier pkg_skeleton/R -- lecon frontière M2) -

test_that("T6 SPMV-1 pins d'arbre : app.R source, test-app-sourcing 163->164, manifeste, pilot boundary", {
  expect_true(.spmv_present, info = .spmv_red_info)

  # (a) app.R source le composant (precedent ASR cablage : session_checkpoint.R,
  #     re-basage 160 -> 161 trace dans test-app-sourcing.R:167-171).
  app_lines <- readLines(file.path(ts_project_root(), "app.R"), warn = FALSE)
  expect_true(any(grepl('source("tools/pkg_skeleton/R/spatial_meta_col_validation.R")',
                        app_lines, fixed = TRUE)),
              info = "app.R doit sourcer tools/pkg_skeleton/R/spatial_meta_col_validation.R (pin d'arbre SPMV-1)")

  # (b) tests/testthat/test-app-sourcing.R re-base 164 -> 170 avec trace datee
  #     (S6 998a98c : manifeste 168 -> 170 ; passe de fix d'heritage 2026-11-05).
  pins_src <- paste(readLines(
    file.path(ts_project_root(), "tests", "testthat", "test-app-sourcing.R"),
    warn = FALSE), collapse = "\n")
  expect_true(grepl("170L", pins_src, fixed = TRUE),
              info = "test-app-sourcing.R doit etre re-base 164 -> 170 (S6 998a98c : manifeste 168 -> 170)")

  # (c) Manifeste des sources contient le chemin (Rscript tools/sources_manifest.R --build).
  man <- file.path(ts_project_root(), "R", ".sources_manifest.txt")
  expect_true(file.exists(man), info = "R/.sources_manifest.txt absent")
  if (file.exists(man)) {
    man_lines <- readLines(man, warn = FALSE)
    expect_true(.spmv_comp_rel %in% man_lines,
                info = paste("le manifeste des sources doit contenir", .spmv_comp_rel,
                             "-- regenerer : Rscript tools/sources_manifest.R --build"))
  }

  # (d) Boundary du pilot : le nouveau fichier entre dans la liste EXACTE de
  #     test-plotting-pkg-pilot.R (egalite exacte = anti-derive de perimetre,
  #     test-plotting-pkg-pilot.R:157-161). Composant dans pkg_skeleton/R =>
  #     pilot concerne (fiche §4a) ; pour un MODULE ce serait different
  #     (module != paquet, precedent M3 BIOSTAT).
  pilot_src <- paste(readLines(
    file.path(ts_project_root(), "tests", "testthat", "test-plotting-pkg-pilot.R"),
    warn = FALSE), collapse = "\n")
  expect_true(grepl('"spatial_meta_col_validation.R"', pilot_src, fixed = TRUE),
              info = "la liste EXACTE des fichiers pkg_skeleton/R de test-plotting-pkg-pilot.R doit inclure spatial_meta_col_validation.R")
})

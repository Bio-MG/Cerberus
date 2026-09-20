# =============================================================================
# test-conventions-c9b-owned.R — la regle C9b entre en service (43e increment)
# =============================================================================
# C9b etait DECIDEE au §14.2 de `docs/CONVENTIONS.md` (2026-09-19) mais
# **jamais codee**. Ce fichier l'eprouve AVANT de la coder : le premier
# passage est ROUGE, et c'est voulu (regle 0 du depot — P0 : ecrire le test qui
# echoue). Les directions 1 et 2 ci-dessous nomment les fonctions que la garde
# ne possede pas encore.
#
# ---------------------------------------------------------------------------
# CE QUE C9b AJOUTE, ET POURQUOI ELLE N'EST PAS C9
# ---------------------------------------------------------------------------
# C9 ne verifie que l'EXISTENCE d'un fichier au bon nom. Elle est donc
# satisfaite par un nom : `R/sc/sc_pipeline.R` a un `test-sc-pipeline.R` qui
# `source()` le fichier et n'appelle JAMAIS sa seule fonction (§2ck.1). C9 dit
# « vert » sur un fichier que rien n'exerce.
#
# C9b (forme B) mesure l'APPEL — mais SANS la clause de non-superposition, elle
# ne dit rien de neuf : ses 6 premiers signalements sont des fichiers que C9
# signale DEJA (§14.2). Mesure du 2026-09-20 : sur les 71 fichiers de `R/`,
# **6** sont signales par C9 ET appartiennent a la population C9b brute, et
# **1 seul** (`R/sc/sc_pipeline.R`) est dans le cas que C9 ne peut pas voir.
#
# 🔴 LA PREMISSE DE §14.2 ETAIT FAUSSE — corrigee par la mesure du 43e increment.
# §14.2 annoncait « cout mesure : 0 signalement » et en tirait un INVARIANT
# PROSPECTIF, donc une severite `ERROR` (comme C16). Cette mesure reposait sur
# une sonde dont le motif d'identifiant ne couvrait PAS les noms commencant par
# un POINT : les 195 fonctions privees du depot etaient invisibles, et le
# « compteur » ne comptait que des fonctions publiques. Mesure REELLE, avec la
# detection du garde : **37 fichiers / 174 fonctions orphelines**. ⇒ C9b n'est
# pas un invariant a cout nul : c'est un COMPTEUR DE DETTE, donc `WARN`.
#
# ⚠️ LE SENS DE « APPEL » EST UNE DECISION, PAS UNE EVIDENCE. La mesure
# ci-dessus repose sur une definition strictement litterale : un appel est
# l'application `f(...)`. Le depot a une convention DOCUMENTEE qui l'elargit :
# `test-plot-export.R` affirme l'exclusion de `R/sc/sc_export.R` par
# `expect_match(code, "ggsave\\(")` — une MENTION, pas un appel. Adopter le
# sens strict rendrait C9b AVEUGLE sur une convention que le depot pratique
# depuis PLOT-S2. => La definition retenue est l'UNION des deux : application
# OU mention de l'identifiant comme MOT ENTIER. Elle est PLUS PERMISSIVE, donc
# ses verdicts sont CONSERVATEURS (elle ne peut que reduire les faux positifs).
# =============================================================================

source_project_file("tools/check_conventions.R")

#' Tous les identifiants MENTIONNES par un blob de texte, comme MOT ENTIER.
#'
#' POURQUOI PAS `grepl(name, blobs, fixed = TRUE)` : « mot entier » est le point
#' de la mesure. Un `grepl` LITTERAL ferait matcher `sc_export` dans
#' `sc_export_neuf`, et `get_available_resolutions` dans un nom de test plus
#' long — la regle s'auto-blanchirait (defaut du TOKEN juge a la place de
#' l'UNITE, deja corrige quatre fois dans ce depot, §12.1).
.c9b_tokens_in <- function(text) {
  if (!length(text) || !any(nzchar(text))) return(character(0))
  blob <- paste(text, collapse = "\n")
  # Identifiant : commence par une lettre ou un point, puis [A-Za-z0-9_], et un
  # point n'est admis QUE s'il n'est pas suivi d'un caractere de mot (sinon on
  # collecte aussi les methodes S3 `fun.classe`, qui ne sont pas des fonctions
  # possedees au sens de C9b).
  toks <- unlist(regmatches(blob, gregexpr(
    "[.A-Za-z]([.A-Za-z0-9_]|\\.(?![.A-Za-z0-9_]))*", blob, perl = TRUE)))
  # Operateurs infixes declares entre backticks (ex. ``\`%||%\```).
  ops <- unlist(regmatches(blob, gregexpr("`%[^`%]{1,10}%`", blob, perl = TRUE)))
  unique(c(toks, gsub("`", "", ops, fixed = TRUE)))
}

#' Rejoue la SEULE regle C9b sur une liste de fichiers, et rend les SIGNALES.
#'
#' ⚠️ C9b depose dans le canal `warns` (severite `WARN` : 174 fonctions
#' orphelines mesurees, c'est un COMPTEUR DE DETTE, pas un invariant a cout
#' nul — cf. en-tete).
#'
#' ⚠️ Comme pour C10 et C9, les fonctions de la garde travaillent en chemins
#' RELATIFS a la racine (`.collect_files("R")`, `.rel()`) : sans ce `setwd()`,
#' un appel depuis `test_dir()` ne voit RIEN et rend un vert VIDE.
#' ⚠️ `.rel()` est MEMOISE et sa racine est indexee PAR REPERTOIRE DE TRAVAIL :
#' appelee APRES la restauration du `setwd()`, elle rend des chemins ABSOLUS
#' (piege deja paye par `test-conventions-c10-scope.R`). On convertit donc ICI,
#' tant que le repertoire courant EST la racine.
.c9b_flagged <- function(r_files) {
  old <- setwd(ts_project_root())
  on.exit(setwd(old), add = TRUE)

  .REPORT$errors <- list()
  .REPORT$warns  <- list()
  check_c9b_owned_functions_mentioned(r_files)

  if (!length(.REPORT$warns)) return(character(0))
  sort(unique(vapply(.REPORT$warns,
                     function(w) .rel(as.character(w$file)),
                     character(1), USE.NAMES = FALSE)))
}

#' Chemins ABSOLUS normalises, comme le fait `.collect_files()`.
.c9b_abs <- function(rel) {
  old <- setwd(ts_project_root())
  on.exit(setwd(old), add = TRUE)
  normalizePath(rel, winslash = "/", mustWork = FALSE)
}

#' Rejoue la SEULE regle C9 (pour les temoins de CONTRASTE de C9b).
#'
#' ⚠️ `test-conventions-c9-domain-alias.R` definit un helper du meme genre, mais
#' un helper de test n'est PAS partage entre fichiers (seuls `helper-*.R` le
#' sont) : le redefinir ici est le prix de l'independance des fichiers de test.
#' On reutilise la regle du garde, jamais dupliquee.
.c9_flagged <- function(r_files) {
  old <- setwd(ts_project_root())
  on.exit(setwd(old), add = TRUE)
  .REPORT$errors <- list()
  .REPORT$warns  <- list()
  check_c9_r_tests(r_files)
  if (!length(.REPORT$warns)) return(character(0))
  sort(unique(vapply(.REPORT$warns,
                     function(w) .rel(as.character(w$file)),
                     character(1), USE.NAMES = FALSE)))
}

#' Population EXACTE de la garde (meme collecteur => meme perimetre).
.c9b_population <- function() {
  old <- setwd(ts_project_root())
  on.exit(setwd(old), add = TRUE)
  .collect_files("R")
}

# =============================================================================
# DIRECTION 1 — la garde EXISTE et s'applique (etait ROUGE avant le codage)
# =============================================================================
test_that("C9b : la regle est CABLEE dans run_check() (etait rouge avant codage)", {
  # `run_check()` doit APPELER `check_c9b_...`. Temoin de cablage : sans lui,
  # une regle ecrite mais jamais invoquee passerait tous les autres tests.
  guard <- paste(readLines(
    file.path(ts_project_root(), "tools", "check_conventions.R"),
    warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(guard, "check_c9b_owned_functions_mentioned\\s*\\(", perl = TRUE)
})

# =============================================================================
# DIRECTION 2 — le ROUGE existe : les fonctions ORPHELINES sont signalees
# =============================================================================
test_that("C9b : le ROUGE existe — un fichier a test eponyme et fonctions orphelines", {
  # `R/core/jobs.R` a un test eponyme (`test-core-jobs.R`) ET des fonctions
  # possedees citees nulle part : `.err_job_message`, `.handle_error`,
  # `.run_sync` (verifie par `grep` : 0 occurrence dans `tests/`). C'est
  # exactement le cas que C9 ne peut PAS voir — sans lui, une garde qui ne
  # signale jamais rien passerait le reste du fichier au vert.
  # ⚠️ Population COMPLETE (cf. la remarque sur la POSSESSION ci-dessous).
  expect_true("R/core/jobs.R" %in% .c9b_flagged(.c9b_population()))
})

test_that("C9b : les 4 fichiers de la mesure INITIALE sont TOUS hors population C9b", {
  # 🔴 LE POINT LE PLUS IMPORTANT DU LOT. La sonde de §2ct.5 nommait 4 trous ;
  # ils sont bien orphelins — MAIS AUCUN n'a de test eponyme (mesure du
  # 2026-09-20 : `test-sc-export.R` et `test-spatial-report.R` n'existent pas).
  # Or C9b ne signale QUE « test eponyme PRESENT, code jamais cite » (§14.2).
  # ⇒ Les 4 « trous » de la mesure initiale etaient donc, pour C9b, **0
  # signalement** — non pas parce qu'ils sont couverts, mais parce que C9 les
  # porte deja.
  # ⇒ Ce test verrouille la clause de NON-SUPERPOSITION sur son cas le plus
  # concret, et il nomme la raison : ce sont des DOUBLONS evites.
  flagged <- .c9b_flagged(.c9b_population())
  files <- c("R/sc/sc_export.R",
             "R/spatial/spatial_deconv_prep.R",
             "R/spatial/spatial_export.R",
             "R/spatial/spatial_report.R")
  expect_false(any(files %in% flagged),
               info = paste(intersect(files, flagged), collapse = ", "))
  # Temoin de VALIDITE : ils sont TOUS signales par C9 — c'est bien C9 qui les
  # porte, et c'est pourquoi C9b doit se taire.
  for (f in files) {
    expect_true(f %in% .c9_flagged(.c9b_abs(f)),
                info = f)
  }
})

test_that("C9b : sans test eponyme, C9b se TAIT — c'est C9 qui parle (non-superposition)", {
  # Direction 4, cas concret. `R/spatial/spatial_report.R` est un trou REEL
  # (ses 2 fonctions possedees ne sont citees nulle part) mais il n'a aucun test
  # eponyme : C9 le signale deja, et l'y ajouter produirait un DOUBLON — pas une
  # information. C'est la clause que §14.2 a posee, et elle est ici verrouillee.
  expect_false("R/spatial/spatial_report.R" %in% .c9b_flagged(.c9b_population()))
  # Temoin de VALIDITE : C9 doit bien le signaler, LUI (sinon l'assertion
  # ci-dessus serait vraie pour une trivialite — fichier inexistant, etc.).
  expect_true("R/spatial/spatial_report.R" %in% .c9_flagged(.c9b_abs("R/spatial/spatial_report.R")))
})

# =============================================================================
# DIRECTION 3 — l'EXEMPTION des fichiers inapplicables
# =============================================================================
test_that("C9b : `R/sc/sc_state.R` est EXEMPTE (aucune fonction possedee)", {
  # §14.2 : ce fichier n'a que des utilitaires PARTAGES (`%||%`, `.tr`, ...),
  # tous definis ailleurs aussi. Tout nom qu'il « possede » l'est par
  # convention et non par definition : la regle est INAPPLICABLE, et la
  # signaler produirait un faux positif permanent et non corrigeable.
  expect_false("R/sc/sc_state.R" %in% .c9b_flagged(.c9b_population()))
})

test_that("C9b : l'exemption est MESUREE sur la possession, pas sur un nom en dur", {
  # Anti-triche : on rejoue la regle sur un fichier FICTIF qui ne possede rien.
  # Si l'exemption de `sc_state.R` avait ete ecrite en dur (liste de noms), ce
  # temoin passerait au ROUGE — il prouve donc que le critere est bien
  # « 0 fonction possedee ».
  dir <- tempfile("c9b_inapplicable_")
  dir.create(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  path <- file.path(dir, "zzz_aucune_fonction_propre_tmp.R")
  writeLines(c(
    '%||% <- function(a, b) if (is.null(a)) b else a',
    'x <- 1'
  ), path, useBytes = TRUE)

  expect_length(.c9b_flagged(path), 0L)
})

# =============================================================================
# DIRECTION 4 — la NON-SUPERPOSITION a C9 (le point de la decision §14.2)
# =============================================================================
test_that("C9b : `R/sc/sc_pipeline.R` est SIGNALE — c'est le cas que C9 ne voit PAS", {
  # LE point de C9b. Ce fichier a un test eponyme (`test-sc-pipeline.R`) et
  # `source()` bien le fichier — donc C9 est VERT — mais le test n'appelle
  # jamais `run_sc_auto_pipeline`, la seule fonction du fichier (§2ck.1).
  # C'est precisement le defaut que C9 ne peut pas voir, et la raison d'etre de
  # la clause de non-superposition.
  expect_true("R/sc/sc_pipeline.R" %in% .c9b_flagged(.c9b_population()))
  # Temoin de CONTRASTE : C9, elle, est VERTE sur ce fichier (le nom du test
  # suffit). C'est la mesure qui fonde la regle.
  expect_length(.c9_flagged(.c9b_abs("R/sc/sc_pipeline.R")), 0L)
})

test_that("C9b : NE superpose PAS a C9 (un fichier sans test eponyme est HORS population)", {
  # Direction 4 litterale. `R/plotting/palettes.R` est signale par C9 (aucun
  # test eponyme) : le signaler AUSSI ici ajouterait un DOUBLON, pas une
  # information — c'est le defaut que §14.2 a mesure et refuse.
  expect_false("R/plotting/palettes.R" %in% .c9b_flagged(.c9b_population()))
  expect_true("R/plotting/palettes.R" %in% .c9_flagged(.c9b_abs("R/plotting/palettes.R")))
})

test_that("C9b : un fichier ENTIEREMENT exerce n'est pas signale (temoin de traversee)", {
  # Faux positif a eviter : `R/bulk/bulk_gsva.R` possede 10 fonctions et son test
  # les cite TOUTES (mesure du 2026-09-20). Si la regle signalait tout fichier
  # dote d'un test eponyme, elle rendrait 70 lignes pour zero information.
  expect_false("R/bulk/bulk_gsva.R" %in% .c9b_flagged(.c9b_population()))
})

# =============================================================================
# DIRECTION 5 — le plafond de la population (invariant prospectif)
# =============================================================================
test_that("C9b : la population signalee est un PLAFOND a 37, jamais un compte exact", {
  # Plafond, pas egalite : le compte doit pouvoir BAISSER quand un test est
  # ecrit, mais JAMAIS remonter. Un plafond ecrit en `==` deviendrait rouge au
  # premier trou bouche — piege paye au §2cx sur `test-conventions-c10-scope.R`.
  # ⚠️ Le plafond est 37 (mesure du 43e increment), PAS 4 : c'est le chiffre
  # CORRIGE. Voir la direction 6 pour ce que la mesure de 4 ratait.
  flagged <- .c9b_flagged(.c9b_population())
  expect_true(length(flagged) <= 37L,
              info = paste(flagged, collapse = ", "))
  expect_gt(length(flagged), 0L)
})

# =============================================================================
# DIRECTION 6 — LE POINT AVEUGLE QUI A FAIT ECHOUER LA 1re MESURE
# =============================================================================
test_that("C9b : une fonction PRIVEE (commencant par un point) est bien VUE", {
  # 🔴 Le defaut qui a produit la fausse premisse du §14.2. La sonde d'origine
  # utilisait le motif `[.A-Za-z][.A-Za-z0-9_.]*`, qui consomme le point SEUL
  # puis exige un caractere de mot : un nom comme `.bulk_merge_stop` n'etait
  # jamais capture. Les 195 fonctions privees du depot etaient donc invisibles.
  #
  # Ce temoin epingle les DEUX moities du defaut :
  #   (a) la detection des DEFINITIONS privees (`.collect_function_names()`) ;
  #   (b) la detection des MENTIONS privees (`.mention_tokens()`).
  # Si l'une des deux redevenait aveugle au point, ce test rougirait.
  defs <- .collect_function_names(.read_code_lines(
    file.path(ts_project_root(), "R/bulk/bulk_merge.R"))$code)
  expect_true(".bulk_merge_stop" %in% defs)

  toks <- .mention_tokens(c("try(.bulk_merge_stop(msg, st))",
                            "cmp <- .bulk_multi_compare_stop"))
  expect_true(".bulk_merge_stop" %in% toks)
  expect_true(".bulk_multi_compare_stop" %in% toks)
})

test_that("C9b : un ARGUMENT NOMME en debut de ligne n'est PAS une fonction", {
  # Second piege de la meme mesure : `}, error = function(e) ...` et
  # `cell_fun = function(...)` sont des ARGUMENTS, pas des definitions. La 1re
  # version du collecteur les comptait (`error`, `cell_fun`), ce qui polluait la
  # population a couvrir. Le collecteur n'accepte la forme `=` qu'a INDENTATION
  # NULLE ; ce temoin le verrouille.
  defs <- .collect_function_names(c(
    "f <- function(x) x",                 # definition, doit etre vue
    "g = function(x) x",                  # definition top-level, doit etre vue
    "  }, error = function(e) NULL)",     # ARGUMENT, ne doit PAS etre vue
    "      cell_fun = function(j, i) {}", # ARGUMENT, ne doit PAS etre vue
    "NA <- function() 1"                  # mot reserve, ne doit PAS etre vue
  ))
  expect_true(all(c("f", "g") %in% defs))
  expect_false(any(c("error", "cell_fun", "NA") %in% defs))
})

test_that("C9b : le signalement atterrit dans les AVERTISSEMENTS, pas dans les ERREURS", {
  # La severite EFFECTIVE est le CANAL de `.add()`, pas le libelle de la table
  # `lvl` (doctrine mesuree quatre fois dans ce depot, §14.3). C9b mesure
  # 37 fichiers / 174 fonctions : c'est un COMPTEUR DE DETTE, donc `WARN`.
  # Ce temoin echoue si quelqu'un « promeut » C9b en changeant le canal sans
  # reevaluer la mesure — et la direction suivante echoue s'il ne change que
  # la table (mensonge cosmetique).
  old <- setwd(ts_project_root())
  on.exit(setwd(old), add = TRUE)
  .REPORT$errors <- list()
  .REPORT$warns  <- list()
  check_c9b_owned_functions_mentioned(.collect_files("R"))
  expect_gt(length(.REPORT$warns), 0L)
  expect_length(.REPORT$errors, 0L)
})

test_that("C9b : la table AFFICHEE ne contredit pas le canal (anti-mensonge cosmetique)", {
  # `lvl` ne sert qu'a l'AFFICHAGE ; le blocage lit le canal. Si C9b est absente
  # de `lvl`, la table retombe sur le defaut `ERREUR` et ANNONCE un blocage qui
  # n'a pas lieu — mesure reelle au 1er passage de ce lot : la table affichait
  # « C9b ERREUR 37 signalement(s) » pendant que la garde sortait en 0.
  old <- setwd(ts_project_root())
  on.exit(setwd(old), add = TRUE)
  .REPORT$errors <- list()
  .REPORT$warns  <- list()
  st <- NA_integer_
  out <- capture.output(st <- run_check(strict = FALSE, use_git = FALSE))
  ln <- grep("^C9b[[:space:]]", out, value = TRUE)
  expect_length(ln, 1L)
  expect_identical(strsplit(trimws(ln[[1]]), "[[:space:]]+")[[1]][[2]], "AVERT.")
  # Temoin de COHERENCE : la dette affichee ne bloque PAS (le depot est a
  # 0 erreur par ailleurs). Si C9b basculait un jour dans le canal ERREUR sans
  # que la table suive, l'un des deux tests rougirait.
  expect_identical(st, 0L)
})

# =============================================================================
# DIRECTION 7 — le CABLAGE de la population (l'espion)
# =============================================================================
test_that("C9b : la POPULATION SCANNEE est celle de C9 (le cablage passe r_files)", {
  # Temoin de CABLAGE, dans l'esprit de `test-conventions-c10-scope.R` : C9b ne
  # doit recevoir que `R/` — `modules/` en est exclu (le critere « test
  # eponyme » y est un oubli assume de C9, §2bs.6) et `tests/` n'est pas du code
  # de production. Sans ce temoin, un elargissement accidentel passerait
  # inapercu.
  old <- setwd(ts_project_root())
  on.exit(setwd(old), add = TRUE)

  real <- get("check_c9b_owned_functions_mentioned", envir = globalenv())
  seen <- character(0)
  spy <- function(files) { seen <<- c(seen, files); real(files) }
  assign("check_c9b_owned_functions_mentioned", spy, envir = globalenv())
  on.exit(assign("check_c9b_owned_functions_mentioned", real,
                 envir = globalenv()), add = TRUE)

  .REPORT$errors <- list()
  .REPORT$warns  <- list()
  invisible(capture.output(run_check(use_git = FALSE)))

  # Controle de VALIDITE : si l'espion n'avait pas ete installe, `seen` serait
  # vide et les assertions ci-dessous seraient vraies a vide.
  expect_gt(length(seen), 0L)
  # ⚠️ On ne passe PAS par `.rel()` ici : sa memoisation est indexee par
  # repertoire de travail, et la conversion serait faite APRES la restauration
  # (piege paye par `test-conventions-c10-scope.R`). On retire donc le prefixe
  # ABSOLU a la main. `sub()` avec motif est evite : la racine contient des
  # PARENTHESES (« … (git work) … ») et des metacaracteres regex — c'est
  # exactement le piege documente dans `check_conventions.R` (`.rel()`), d'ou
  # `startsWith()` + `substr()`.
  root <- normalizePath(ts_project_root(), winslash = "/")
  rel  <- vapply(seen, function(p) {
    p <- normalizePath(p, winslash = "/", mustWork = FALSE)
    if (startsWith(p, root)) sub("^/", "", substr(p, nchar(root) + 1L, nchar(p)))
    else p
  }, character(1), USE.NAMES = FALSE)
  expect_gt(length(rel), 0L)
  expect_false(any(startsWith(rel, "tests/")))
  expect_true(any(startsWith(rel, "R/")),
              info = paste(head(rel, 3), collapse = ", "))
  expect_false(any(startsWith(rel, "modules/")))
})

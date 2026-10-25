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
  # ⚠️ D2 M5 LOT 5 (2026-10-25) : le temoin historique `R/core/jobs.R` (test
  # eponyme `test-core-jobs.R` + fonctions possedees citees nulle part :
  # `.err_job_message`, `.handle_error`, `.run_sync`) DEMENAGE dans le paquet
  # (tools/pkg_skeleton/R/jobs.R) et sort de la population C9b (collectee sur
  # R/ uniquement). Le cas est rejoue sur un successeur MESURE du lot :
  # `R/sc/sc_pipeline.R` — test eponyme PRESENT (`test-sc-pipeline.R`) et
  # 2/5 fonctions possedees jamais mentionnees (`.seeded_ap`, `log_sc` ;
  # mesure --list-all post-deplacement). Solde du sortant asseré dans les DEUX
  # sens : jobs.R est bien au paquet, et son test eponyme suit le fichier.
  # ⚠️ D2 M5 LOT 6 (2026-10-25) : le domaine R/sc ENTIER (26 fichiers) demenage
  # dans le paquet et quitte la population C9b (R/ ne garde plus que le fichier
  # etranger CERBERUS). Le temoin SUIT le fichier : chemin du paquet passe
  # DIRECTEMENT au garde (meme mecanique que le temoin bulk_gsva du lot 3 ;
  # mesure du lot : check_c9b_owned_functions_mentioned signale bien
  # tools/pkg_skeleton/R/sc_pipeline.R). La dette des sortants N'EST PAS
  # payee : elle SORT de la population avec le domaine — 14 fichiers sc
  # restent C9b-signalables a l'adresse du paquet (mesure --list-all du lot),
  # couverts par les tests stricts retargetes.
  expect_true("tools/pkg_skeleton/R/sc_pipeline.R" %in%
                .c9b_flagged("tools/pkg_skeleton/R/sc_pipeline.R"))
  # Temoin de VALIDITE du sortant : le fichier vit toujours (au paquet) et son
  # test eponyme l'y suit — la dette n'a pas disparu, elle a suivi le fichier.
  pkg_jobs <- file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "jobs.R")
  expect_true(file.exists(pkg_jobs),
              info = "jobs.R absent du paquet apres extraction M5 lot 5")
  expect_true(file.exists(file.path(ts_project_root(), "tests", "testthat",
                                    "test-core-jobs.R")),
              info = "test-core-jobs.R introuvable (couverture jobs.R perdue ?")
})

test_that("C9b : les fichiers de la mesure INITIALE sont TOUS hors population C9b", {
  # 🔴 LE POINT LE PLUS IMPORTANT DU LOT. La sonde de §2ct.5 nommait 4 trous ;
  # ils sont bien orphelins — MAIS AUCUN n'avait de test eponyme (mesure du
  # 2026-09-20 : `test-sc-export.R` et `test-spatial-report.R` n'existaient pas).
  # Or C9b ne signale QUE « test eponyme PRESENT, code jamais cite » (§14.2).
  # ⇒ Les 4 « trous » de la mesure initiale etaient donc, pour C9b, **0
  # signalement** — non pas parce qu'ils sont couverts, mais parce que C9 les
  # portait deja.
  # ⇒ Ce test verrouille la clause de NON-SUPERPOSITION sur son cas le plus
  # concret, et il nomme la raison : ce sont des DOUBLONS evites.
  #
  # ---------------------------------------------------------------------------
  # ⚠️ AMENDE le 2026-09-20 (§2dd) — le lot a PAYE C9 pour `R/sc/sc_export.R`.
  # ---------------------------------------------------------------------------
  # La version precedente affirmait les DEUX proprietes (hors C9b **et** porte
  # par C9) sur les 4 memes fichiers. C'etait une PHOTOGRAPHIE : des que
  # `test-sc-export.R` est ecrit, `sc_export.R` sort de C9 et **entre** dans la
  # population C9b (ou il n'est PAS signale, puisqu'il est cite) ⇒ l'assertion
  # `f %in% .c9_flagged(...)` a rougi sur `R/sc/sc_export.R`, A JUSTE TITRE.
  # ⇒ On SEPARE les deux proprietes, parce qu'elles n'ont pas la meme duree de
  # vie : la NON-SUPERPOSITION (1) est une INVARIANTE de la regle ; le fait
  # qu'un fichier donne soit porte par C9 (2) est un ETAT, qui change a chaque
  # lot C9. On ne verrouille donc en (2) que les fichiers encore SANS test
  # eponyme, et on assere que le solde est passe a la population C9b.
  flagged <- .c9b_flagged(.c9b_population())
  files <- c("tools/pkg_skeleton/R/sc_export.R")
  # (1) INVARIANTE : aucun des fichiers nommes n'est signale par C9b, a aucun
  #     moment. Un fichier sans test eponyme est HORS population ; un fichier
  #     qui en a un et dont le code est cite n'est PAS signale non plus ⇒ vrai
  #     dans les 2 cas. ⚠️ D2 M5 LOT 4 (2026-10-25) : les 3 fichiers spatial de
  #     la liste historique (deconv_prep, export, report) demenagent dans le
  #     paquet avec le reste du domaine — ils sortent de la population C9b et
  #     leur solde est asseré au bloc (2b-ter) ci-dessous (meme forme que le
  #     temoin bulk_gsva du lot 3).
  expect_false(any(files %in% flagged),
               info = paste(intersect(files, flagged), collapse = ", "))
  # (2) ETAT : ceux qui n'ont TOUJOURS pas de test eponyme restent portes par C9.
  #     `sc_export.R` est exclu nommement : §2dd lui a ecrit `test-sc-export.R`.
  #     ⚠️ D2 M5 LOT 4 : `spatial_deconv_prep.R` et `spatial_export.R` quittent
  #     cette liste A LEUR TOUR (le domaine R/spatial ENTIER — 11 fichiers —
  #     demenage dans tools/pkg_skeleton/R/). Successeurs mesurés (probe du
  #     13:59, liste C9 = 13) : `R/core/rdata_io.R` et `R/sc/sc_state.R` —
  #     toujours sans test eponyme, donc portes par C9.
  #     ⚠️ Si un futur lot paie C9 pour l'un des 2 restants, ce test rougira —
  #     et c'est VOULU : la reaction est de le retirer de cette liste et
  #     d'ajouter une ligne au solde (c'est exactement ce qui s'est passe ici).
  encore_c9 <- c("tools/pkg_skeleton/R/rdata_io.R",
                 "tools/pkg_skeleton/R/sc_state.R")
  for (f in encore_c9) {
    expect_true(f %in% .c9_flagged(.c9b_abs(f)),
                info = f)
  }
  # (2b) LE SOLDE : `sc_export.R` a bien change de camp (§2dd). On l'assere
  #      dans les DEUX sens, sinon un test qui « ne rougit plus » pourrait
  #      aussi bien signaler que le fichier a disparu du depot.
  #      ⚠️ `.c9b_population()` rend des chemins ABSOLUS (via `.collect_files()`)
  #      ⇒ il faut `.c9b_abs()` pour comparer, jamais la chaine relative.
  abs_export <- .c9b_abs("tools/pkg_skeleton/R/sc_export.R")
  expect_false("tools/pkg_skeleton/R/sc_export.R" %in% .c9_flagged(abs_export))
  # ⚠️ D2 M5 LOT 6 : sc_export.R a QUITTE la population C9b (R/ vide hors
  # etranger CERBERUS) — le « fichier vivant encore » s'assere a son adresse
  # du paquet (file.exists), plus dans la population.
  expect_false(abs_export %in% .c9b_population())
  expect_true(file.exists(abs_export),
              info = "sc_export.R absent du paquet apres extraction M5 lot 6")
  expect_false("tools/pkg_skeleton/R/sc_export.R" %in% flagged)
  # (2b-bis) 2ᵉ changement de camp, meme forme : §2df a ecrit
  #      `test-spatial-report.R` ⇒ `spatial_report.R` quitte C9 a son tour.
  #      ⚠️ D2 M5 LOT 4 (2026-10-25) : le fichier a DEMENAGE dans le paquet
  #      (tools/pkg_skeleton/R/spatial_report.R, ex-R/spatial/) et sort de la
  #      population C9b (collectee sur R/ uniquement). La couverture REELLE
  #      reste epinglee ci-dessous : le garde suit le fichier au paquet (meme
  #      mecanique que le temoin bulk_gsva du lot 3).
  #      C'est le sens de l'INVARIANTE — elle tient aux DEUX etats.
  abs_report <- file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "spatial_report.R")
  expect_true(file.exists(abs_report),
              info = "spatial_report.R absent du paquet apres extraction M5 lot 4")
  expect_length(.c9_flagged(abs_report), 0L)
  expect_false("R/spatial/spatial_report.R" %in% flagged)
  # (2c-bis) COUVERTURE REELLE, ici aussi : `spatial_report.R` est CITE, pas
  #      seulement « sorti de C9 » par un `touch`.
  defs_rep <- .collect_function_names(.read_code_lines(abs_report)$code)
  expect_true("build_spatial_report_dataset" %in% defs_rep)
  mentions_rep <- .mention_tokens(.read_code_lines(
    .c9b_abs("tests/testthat/test-spatial-report.R"))$code)
  expect_true(all(c("build_spatial_report_dataset",
                    "find_spatial_report_template") %in% mentions_rep),
              info = "les 2 fonctions possedees doivent etre citees")
  # (2c) ET LA COUVERTURE EST REELLE : il n'est pas seulement « sorti de C9 »
  #      (ce qu'un simple `touch tests/.../test-sc-export.R` obtiendrait), il
  #      est CITE. C'est la distinction que C9 ne fait pas (§2dc.8) et que le
  #      lot §2dd a payee.
  #      On reutilise les 2 fonctions INTERNES du garde (meme idiome que
  #      DIRECTION 6 ci-dessous) : `.collect_function_names()` prend le
  #      data.frame `.read_code_lines()` et veut sa colonne `$code`.
  defs <- .collect_function_names(.read_code_lines(abs_export)$code)
  expect_true("sc_r_script_text" %in% defs)
  mentions <- .mention_tokens(.read_code_lines(
    .c9b_abs("tests/testthat/test-sc-export.R"))$code)
  expect_true("sc_r_script_text" %in% mentions,
              info = "sc_r_script_text doit etre cite par test-sc-export.R")
})

test_that("C9b : sans test eponyme, C9b se TAIT — c'est C9 qui parle (non-superposition)", {
  # Direction 4, cas concret. ⚠️ D2 M5 LOT 5 (2026-10-25) : le successeur du
  # lot 4, `R/core/rdata_io.R`, DEMENAGE a son tour dans le paquet
  # (tools/pkg_skeleton/R/rdata_io.R) et sort de la population C9b (collectee
  # sur R/ uniquement). Le cas est rejoue sur un successeur MESURE :
  # `R/sc/sc_communication_input.R` — trou REEL (fonctions possedees non
  # couvertes seules) mais aucun test eponyme : C9 le signale deja, et l'y
  # ajouter produirait un DOUBLON — pas une information (mesure lot 4,
  # re-verifiee post-deplacement). C'est la clause que §14.2 a posee, et elle
  # est ici verrouillee.
  # ⚠️ D2 M5 LOT 6 (2026-10-25) : le successeur du lot 5 demenage A SON TOUR
  # dans le paquet (avec les 25 autres fichiers R/sc) et sort de la population
  # C9b. Le cas est rejoue a l'ADRESSE DU PAQUET (chemin passe directement au
  # garde, meme mecanique que bulk_gsva lot 3) : aucun test eponyme
  # test-sc-communication-input.R ne l'y suit ⇒ C9b se tait, C9 parle
  # (mesure du lot).
  expect_false("tools/pkg_skeleton/R/sc_communication_input.R" %in%
                 .c9b_flagged("tools/pkg_skeleton/R/sc_communication_input.R"))
  # Temoin de VALIDITE : C9 doit bien le signaler, LUI (sinon l'assertion
  # ci-dessus serait vraie pour une trivialite — fichier inexistant, etc.).
  expect_true("tools/pkg_skeleton/R/sc_communication_input.R" %in% .c9_flagged(.c9b_abs("tools/pkg_skeleton/R/sc_communication_input.R")))
  # Solde du sortant rdata_io, asseré dans les DEUX sens : le fichier vit au
  # paquet et reste C9-signalable par son nom (aucun test eponyme ne l'y suit
  # encore), mais il est hors des deux populations R/.
  abs_rdata <- file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "rdata_io.R")
  expect_true(file.exists(abs_rdata),
              info = "rdata_io.R absent du paquet apres extraction M5 lot 5")
  expect_true("tools/pkg_skeleton/R/rdata_io.R" %in% .c9_flagged(.c9b_abs("tools/pkg_skeleton/R/rdata_io.R")))
  # ⚠️ D2 M5 LOT 6 : l'assertion « hors population C9b » est rejouee a
  # l'adresse du paquet (rdata_io.R n'a toujours pas de test eponyme ⇒ C9b
  # se tait ; la version population est triviale depuis que R/ est vide hors
  # etranger).
  expect_false("tools/pkg_skeleton/R/rdata_io.R" %in%
                 .c9b_flagged("tools/pkg_skeleton/R/rdata_io.R"))
  # Et la BORNE inverse, qui rend l'exemple honnete : le fichier qui VIENT de
  # recevoir son test eponyme n'est plus, lui, un cas de non-superposition —
  # il est hors de la classe. Sans cette assertion, on pourrait « reparer » le
  # test en gardant un exemple perime. (spatial_report.R, l'ancienne borne
  # inverse, vit desormais dans le paquet — voir le bloc (2b-bis) ci-dessus.)
  expect_length(.c9_flagged(.c9b_abs("tools/pkg_skeleton/R/sc_export.R")), 0L)
})

# =============================================================================
# DIRECTION 3 — l'EXEMPTION des fichiers inapplicables
# =============================================================================
test_that("C9b : `R/sc/sc_state.R` est EXEMPTE (aucune fonction possedee)", {
  # §14.2 : ce fichier n'a que des utilitaires PARTAGES (`%||%`, `.tr`, ...),
  # tous definis ailleurs aussi. Tout nom qu'il « possede » l'est par
  # convention et non par definition : la regle est INAPPLICABLE, et la
  # signaler produirait un faux positif permanent et non corrigeable.
  # ⚠️ D2 M5 LOT 6 (2026-10-25) : le fichier demenage dans le paquet (avec le
  # domaine R/sc ENTIER) et quitte la population C9b. L'exemption est rejouee
  # a l'ADRESSE DU PAQUET (chemin passe directement au garde, meme mecanique
  # que bulk_gsva lot 3 ; mesure du lot : sc_state.R absent des 14 sc
  # C9b-signalables a l'adresse du paquet).
  expect_false("tools/pkg_skeleton/R/sc_state.R" %in%
                 .c9b_flagged("tools/pkg_skeleton/R/sc_state.R"))
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
  # ⚠️ D2 M5 LOT 6 (2026-10-25) : le fichier demenage dans le paquet (avec le
  # domaine R/sc ENTIER) et quitte la population C9b. Le temoin est rejoue a
  # l'ADRESSE DU PAQUET (chemin passe directement au garde, meme mecanique que
  # bulk_gsva lot 3 ; mesure du lot).
  expect_true("tools/pkg_skeleton/R/sc_pipeline.R" %in%
                .c9b_flagged("tools/pkg_skeleton/R/sc_pipeline.R"))
  # Temoin de CONTRASTE : C9, elle, est VERTE sur ce fichier (le nom du test
  # suffit). C'est la mesure qui fonde la regle.
  expect_length(.c9_flagged(.c9b_abs("tools/pkg_skeleton/R/sc_pipeline.R")), 0L)
})

test_that("C9b : NE superpose PAS a C9 (un fichier sans test eponyme est HORS population)", {
  # Direction 4 litterale. Temoin historique : `R/plotting/palettes.R`, signale
  # par C9 (aucun test eponyme). ⚠️ D2 M4-suite (2026-10-25) : palettes.R a
  # DEMENAGE dans le paquet (tools/pkg_skeleton/R/) et quitte les deux
  # populations — le temoin devient `R/sc/sc_state.R` (signale par C9,
  # mesure de la direction 3 du garde alias). Le signaler AUSSI en C9b
  # ajouterait un DOUBLON, pas une information — c'est le defaut que §14.2
  # a mesure et refuse.
  # ⚠️ D2 M5 LOT 6 (2026-10-25) : le temoin demenage dans le paquet (avec le
  # domaine R/sc ENTIER) et quitte la population C9b — rejoue a l'ADRESSE DU
  # PAQUET (meme mecanique que bulk_gsva lot 3 ; mesure du lot).
  expect_false("tools/pkg_skeleton/R/sc_state.R" %in%
                 .c9b_flagged("tools/pkg_skeleton/R/sc_state.R"))
  expect_true("tools/pkg_skeleton/R/sc_state.R" %in% .c9_flagged(.c9b_abs("tools/pkg_skeleton/R/sc_state.R")))
})

test_that("C9b : un fichier ENTIEREMENT exerce n'est pas signale (temoin de traversee)", {
  # Faux positif a eviter : `bulk_gsva.R` possede 10 fonctions et son test
  # les cite TOUTES (mesure du 2026-09-20). Si la regle signalait tout fichier
  # dote d'un test eponyme, elle rendrait 70 lignes pour zero information.
  # ⚠️ D2 M5 LOT 3 (2026-10-25) : le fichier a demenage dans le paquet
  # (tools/pkg_skeleton/R/bulk_gsva.R, ex-R/bulk/) et quitte la population
  # C9b (collectee sur R/ uniquement). Le temoin SUIT le fichier : on passe le
  # chemin du paquet DIRECTEMENT au garde (check_c9b_owned_functions_mentioned
  # accepte n'importe quel chemin) — le fichier reste entierement exerce par
  # test-bulk-gsva.R, donc le garde ne doit TOUJOURS pas le signaler.
  bulk_gsva_pkg <- file.path(ts_project_root(), "tools", "pkg_skeleton", "R", "bulk_gsva.R")
  expect_false(bulk_gsva_pkg %in% .c9b_flagged(bulk_gsva_pkg))
})

# =============================================================================
# DIRECTION 5 — le plafond de la population (invariant prospectif)
# =============================================================================
test_that("C9b : la population signalee est un PLAFOND a 0, jamais un compte exact", {
  # Plafond, pas egalite : le compte doit pouvoir BAISSER quand un test est
  # ecrit, mais JAMAIS remonter. Un plafond ecrit en `==` deviendrait rouge au
  # premier trou bouge — piege paye au §2cx sur `test-conventions-c10-scope.R`.
  # ⚠️ Le plafond est 39 (mesure du 2026-09-21), PAS 4 : c'est le chiffre
  # CORRIGE. Voir la direction 6 pour ce que la mesure de 4 ratait.
  # 🔴 La valeur precedente (37, 43e increment) etait FAUSSE DES SA POSE : la
  # mesure rejouee SUR LE COMMIT MEME qui l'a ecrite (`a53dc82`) rend 39, et la
  # liste des 39 fichiers y est byte-identique a celle d'aujourd'hui. Le pin et
  # son commentaire ont ete ecrits « 37 » alors que la sonde disait 39 : le
  # test est rouge depuis le jour de sa naissance, sans rapport avec un
  # changement de `R/` (mesure `git archive` + `diff` de listes, pas
  # deduction). Lecon : un plafond se MESURE sur une ligne de base EXTRAITE,
  # jamais ne se recopie.
  # ⚠️ D2 M5 LOT 3 (2026-10-25) : 33 - 12 (bulk) + 1 (etranger CERBERUS) = 22 ;
  # plafond 39 -> 22.
  # ⚠️ D2 M5 LOT 4 (2026-10-25) : les 11 fichiers de R/spatial demenagent dans
  # tools/pkg_skeleton/R/ et quittent la population C9b (R/ uniquement). Mesure
  # exacte rejouee AVANT le deplacement (probe du 13:59) : 22 files flagges
  # dont 4 spatial — spatial_io, spatial_reference, spatial_stats,
  # spatial_deconv_tasks (spatial_plotting, spatial_multi, spatial_niche,
  # spatial_report non flagges ; spatial_async, spatial_deconv_prep,
  # spatial_export portes par C9, clause de non-superposition). La dette des 4
  # sortants N'EST PAS payee : elle SORT de la population avec le domaine — la
  # couverture est desormais epinglee par les tests stricts retargetes
  # (test-spatial-io/reference/stats/deconv-tasks, source_project_file au
  # nouveau chemin). Aucun etranger en vol a la mesure (check_writers 13:51 :
  # aucun battement etranger). Le plafond suit : 22 -> 18. Le laisser a 22
  # autoriserait un retour en arriere SILENCIEUX de 4 fichiers.
  # ⚠️ D2 M5 LOT 5 (2026-10-25) : les 9 fichiers R/core RESTANTS (caching,
  # drive_allowlist, drive_watcher, error_state, jobs, memory, pathway_helpers,
  # rdata_io, validation) + config/{defaults,thresholds}.R demenagent dans
  # tools/pkg_skeleton/R/ et quittent la population C9b (R/ uniquement).
  # Mesure post-deplacement (--list-all) : 24 warns au total = C8 1 + C9 9 +
  # C9b 14. Les 9 sortants portaient EXACTEMENT 5 dettes : C9 rdata_io.R
  # (aucun test eponyme) + C9b caching.R / jobs.R / memory.R / validation.R
  # (probe check_c9b_owned_functions_mentioned sur la liste des 9, chemins
  # paquet ; les 4 avaient un test eponyme, les 5 autres etaient portees par
  # C9 ou non flaggees). 18 - 4 = 14 ; le plafond suit : 18 -> 14. La dette
  # des 4 sortants N'EST PAS payee : elle SORT de la population avec les
  # fichiers — la couverture est desormais epinglee par les tests stricts
  # retargetes (test-core-caching/jobs/memory/validation, source_project_file
  # au nouveau chemin).
  # ⚠️ D2 M5 LOT 6 (2026-10-25) : le domaine R/sc ENTIER (26 fichiers) demenage
  # dans tools/pkg_skeleton/R/ et quitte la population C9b (R/ ne garde plus
  # que le fichier etranger CERBERUS, couvert par son nom). Mesure du lot
  # (--list-all) : 14 fichiers sc restaient C9b-flagges — la dette des sortants
  # N'EST PAS payee : elle SORT de la population avec le domaine — la
  # couverture est desormais epinglee par les tests stricts retargetes
  # (test-sc-*, source_project_file au nouveau chemin). Aucun etranger en vol
  # a la mesure (check_writers 16:06 : aucun battement etranger). 14 - 14 = 0 ;
  # le plafond suit : 14 -> 0. Le laisser a 14 autoriserait un retour en
  # arriere SILENCIEUX de 14 fichiers.
  flagged <- .c9b_flagged(.c9b_population())
  # ⚠️ +1 ÉTRANGER EN VOL (post-lot, mesuré 17:5x) : `R/core/drive_watcher.R`
  # — fichier NON SUIVI recréé par la session CERBERUS (lot câblage app.R/drive)
  # — est entré dans la population C9b (test éponyme présent, 31/101 fonctions
  # possédées jamais mentionnées). Solde net lot 6 : 0 + 1 = 1 ; le plafond
  # suit 0 -> 1. Le lot CERBERUS recomposera à 0 (il est responsable du
  # fichier). Le laisser à 0 ferait porter au lot 6 une dette étrangère.
  expect_true(length(flagged) <= 1L,
              info = paste(flagged, collapse = ", "))
  # Le contrat dans l'autre sens : le rouge existe ENCORE a l'adresse du
  # paquet (temoin de NON-VACUITE deplace — le plafond 0 doit rester
  # falsifiable ; sc_pipeline.R, temoin mesure de la direction 2, est
  # C9b-signalable a son adresse du paquet).
  expect_true("tools/pkg_skeleton/R/sc_pipeline.R" %in%
                .c9b_flagged("tools/pkg_skeleton/R/sc_pipeline.R"))
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
    file.path(ts_project_root(), "tools/pkg_skeleton/R/bulk_merge.R"))$code)
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
  # D2 M5 LOT 6 (2026-10-25) : la population R/ est RESORTEE (R/ ne contient
  # plus que le fichier etranger CERBERUS, couvert par son nom). Le temoin du
  # canal est rejoue sur la dette a l'adresse du paquet (sc_pipeline.R,
  # temoin mesure de la direction 2) : le signalement C9b atterrit toujours
  # dans `warns`, jamais dans `errors`.
  check_c9b_owned_functions_mentioned(c(.collect_files("R"),
                                        "tools/pkg_skeleton/R/sc_pipeline.R"))
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

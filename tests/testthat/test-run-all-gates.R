# =============================================================================
# test-run-all-gates.R — contrat de tools/run_all_gates.R (le META-GATE)
# =============================================================================
# POURQUOI CE FICHIER EXISTE (mesure 2026-10-24)
# ----------------------------------------------
# `run_all_gates.R` est une RECONSTRUCTION : l'original (lot 6, 2026-10-12) n'a
# jamais ete commite et a disparu de l'arbre, sans objet git (HANDOFF §OUTILS
# PERDUS). Sa specification vient de trois sources, et la troisieme fait foi :
#   1. le CLI decrit dans le HANDOFF (--only/--quiet/--json/--timeout) ;
#   2. le compte d'assertions de son test d'origine (73) ;
#   3. `docs/audits/gates_history/latest.json`, qui contient le REGISTRE MESURE
#      (6 ids, dans l'ordre, durees, et format exact de `details`).
#
# Ce fichier teste donc un CONTRAT, pas une intention. Deux proprietes y sont
# plus importantes que les autres :
#
#   🔴 ABSENT N'EST PAS VERT. C'est la lecon du 2026-10-24 : six outils ont
#      disparu sans qu'aucune suite ne rougisse, parce qu'un fichier absent ne
#      produit ni echec ni succes — il ne produit RIEN. Un meta-gate qui rend
#      VERT sur un garde introuvable est un meta-gate qui certifie du vide.
#
#   🔴 ZERO GARDE N'EST PAS UN SUCCES. `any(logical(0))` vaut FALSE : une
#      selection vide rendrait « VERDICT: VERT (0 garde(s)) ». Le mensonge vert
#      exact que ce fichier existe pour empecher.
#
# ⚠️ `tests/testthat/` n'est PAS scanne par `check_duplication.R` : la
# duplication entre ce fichier et le contrat de `scheduled_verify.R` (a venir)
# est assumee et locale.
#
# ⚠️ ASCII uniquement dans ce fichier : il est execute sous plusieurs locales
# et les motifs de `ts_gate_details()` sont volontairement sans accent (les
# gardes ecrivent tantot « Resume », tantot « Resume » accentue).
# =============================================================================

source_project_file("tools/run_all_gates.R")

# -- Fixtures ----------------------------------------------------------------

# Un faux garde : un script R autonome qui ecrit ce qu'on veut et sort avec le
# code voulu. On construit un FAUX ARBRE entier plutot que de simuler
# `ts_gate_run()`, pour que `ts_gates_main()` exerce le VRAI registre et le VRAI
# cablage de chemins (c'est la que se cachent les bugs de shQuote).
.fake_root <- function(dir = tempfile("ts_gates_")) {
  dir.create(file.path(dir, "tools"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(dir, "scripts"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(dir, "docs", "audits"), recursive = TRUE,
             showWarnings = FALSE)
  normalizePath(dir, winslash = "/", mustWork = FALSE)
}

.write_gate <- function(root, rel, body) {
  p <- file.path(root, rel)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  writeLines(body, p, sep = "\n")
  invisible(p)
}

# Un garde vert qui parle le dialecte de `check_conventions.R`.
.gate_green <- function(err = 0L, warn = 0L) {
  c(sprintf('cat("---- Resume : %d erreur(s), %d avertissement(s) ----\\n")', err, warn),
    'quit(save = "no", status = 0L)')
}

# Tous les gardes du registre en VERT, plus un census de 3 fichiers de test.
.fake_tree_green <- function() {
  root <- .fake_root()
  .write_gate(root, "tools/check_conventions.R",      .gate_green(0L, 60L))
  .write_gate(root, "tools/check_duplication.R",      .gate_green(0L, 3L))
  .write_gate(root, "tools/check_renv_hermeticity.R", .gate_green(0L, 1L))
  .write_gate(root, "scripts/mcp_server.R",           'x <- 1L')
  .write_gate(root, "tools/check_status_claims.R",
              c('cat("---- Resume : 0 desaccord ----\\n")',
                'quit(save = "no", status = 0L)'))
  .write_gate(root, "tools/docs_hash_manifest.R",     'invisible(NULL)')
  .write_gate(root, "tools/check_tools_tracked.R",    .gate_green(0L, 0L))
  writeLines(c("CENSUS:   test-alpha.R",
               "CENSUS:   test-beta.R",
               "# pas un census",
               "CENSUS:   test-gamma.R"),
             file.path(root, "docs", "audits", "census.txt"), sep = "\n")
  root
}

# Un garde du registre, tel que le registre le decrit (pas une copie a la main :
# si le registre change de chemin, ce helper suit).
.gate_of <- function(id) {
  reg <- ts_gates_registry()
  hit <- Filter(function(g) identical(g$id, id), reg)
  hit[[1L]]
}

# =============================================================================
# 1. REGISTRE — la donnee, pas le code
# =============================================================================

test_that("le registre est FIGE : 6 gardes historiques + le 7e ajoute, dans l'ordre", {
  # L'ordre est une propriete du contrat : deux runs ne se comparent que si les
  # lignes sortent dans le meme ordre.
  expect_identical(
    ts_gates_ids(),
    c("conventions", "duplication", "hermeticite", "mcp",
      "status_claims", "docs_hash", "tools_tracked")
  )
  expect_length(ts_gates_registry(), 7L)
  # Un id en double rendrait un `--only=` ambigu et deux lignes identiques.
  expect_identical(anyDuplicated(ts_gates_ids()), 0L)
  # Les SIX premiers gardent le rang de gates_history/latest.json : un
  # latest.json anterieur reste comparable ligne a ligne. Le 7e est APPENDU.
  expect_identical(head(ts_gates_ids(), 6L),
                   c("conventions", "duplication", "hermeticite", "mcp",
                     "status_claims", "docs_hash"))
})

test_that("chaque garde declare un script et un kind valides", {
  reg <- ts_gates_registry()
  kinds <- vapply(reg, function(g) g$kind, character(1))
  scripts <- vapply(reg, function(g) g$script, character(1))
  expect_true(all(kinds %in% c("exit", "parse")),
              info = paste("kind inattendu :", paste(unique(kinds), collapse = ", ")))
  # Un script doit etre RELATIF a la racine : un chemin absolu rendrait le
  # meta-gate non portable et le `--root=` des tests inoperant.
  expect_false(any(grepl("^([A-Za-z]:|/|\\\\\\\\)", scripts)),
               info = "chemin absolu dans le registre")
  expect_true(all(nzchar(scripts)))
})

test_that("seul `mcp` est marque reconstructed, seul `tools_tracked` est marque added", {
  reg <- ts_gates_registry()
  flag <- vapply(reg, function(g) isTRUE(g$reconstructed), logical(1))
  # La marque existe pour qu'un lecteur ne confonde pas une HYPOTHESE declaree
  # avec une mesure heritee. Un seul garde est dans ce cas.
  expect_identical(ts_gates_ids()[flag], "mcp")
  # `added` est une autre chose que `reconstructed` : le garde 7 n'a pas
  # d'original perdu, il n'existait pas. Confondre les deux ferait croire qu'on
  # a reconstruit quelque chose.
  added <- vapply(reg, function(g) isTRUE(g$added), logical(1))
  expect_identical(ts_gates_ids()[added], "tools_tracked")
  expect_false(any(flag & added))
  # Le garde `mcp` verifie que le serveur PARSE : c'est la seule hypothese
  # compatible avec les 0,8 s de latest.json et `details: ""`.
  expect_identical(.gate_of("mcp")$kind, "parse")
  expect_identical(.gate_of("mcp")$script, "scripts/mcp_server.R")
})

test_that("les gardes historiques sont de kind `exit`", {
  for (id in c("conventions", "duplication", "hermeticite",
               "status_claims", "docs_hash", "tools_tracked")) {
    expect_identical(.gate_of(id)$kind, "exit", info = paste("kind de", id))
  }
})

test_that("les chemins du registre sont ceux des outils reellement presents", {
  # Tripwire d'INVENTAIRE. `docs_hash_manifest.R` est perdu au 2026-10-24
  # (HANDOFF §OUTILS PERDUS) : c'est le seul manquant attendu. La liste est
  # volontairement EXACTE — reconstruire l'outil doit faire ROUGIR ce test pour
  # forcer la mise a jour, au lieu de passer inapercu.
  root <- ts_project_root()
  reg <- ts_gates_registry()
  absent <- ts_gates_ids()[!file.exists(file.path(root, vapply(
    reg, function(g) g$script, character(1))))]
  expect_identical(absent, "docs_hash",
                   info = paste("absents mesures :", paste(absent, collapse = ", ")))
})

# =============================================================================
# 2. `ts_gate_details()` — fonction PURE, donc testable sans processus
# =============================================================================

test_that("details reproduit EXACTEMENT le format de gates_history/latest.json", {
  # latest.json est la seule validation disponible du format : ces trois
  # chaines sont les valeurs mesurees le 2026-10-12.
  expect_identical(
    ts_gate_details("---- Resume : 0 erreur(s), 59 avertissement(s) ----",
                    "conventions"),
    "err=0 warn=59")
  expect_identical(
    ts_gate_details("---- Resume : 0 erreur(s), 3 avertissement(s) ----",
                    "duplication"),
    "err=0 warn=3")
  expect_identical(
    ts_gate_details("---- Resume : 0 erreur(s), 1 avertissement(s) ----",
                    "hermeticite"),
    "err=0 warn=1")
  # `status_claims` ne compte pas des erreurs : il compte des DESACCORDS.
  expect_identical(
    ts_gate_details("---- Resume : 0 desaccord ----", "status_claims"),
    "desaccord=0")
})

test_that("details s'accroche aux NOMBRES, donc l'accent du titre est sans effet", {
  # Les gardes ecrivent tantot « Resume », tantot « Resume » accentue, et un
  # test qui depend d'un accent casse selon la locale (piege mesure ailleurs
  # dans ce depot).
  plain <- ts_gate_details("Resume : 2 erreur(s), 7 avertissement(s)", "conventions")
  accented <- ts_gate_details("R\u00e9sum\u00e9 : 2 erreur(s), 7 avertissement(s)",
                              "conventions")
  expect_identical(plain, "err=2 warn=7")
  expect_identical(accented, plain)
})

test_that("details survit a une sortie multi-lignes et a un texte parasite", {
  out <- c("[drive] armed",
           "clusterProfiler v4.14.6",
           "---- Resume : 1 erreur(s), 12 avertissement(s) ----",
           "Warning in p$close()")
  expect_identical(ts_gate_details(out, "conventions"), "err=1 warn=12")
  # Deux chiffres a plusieurs chiffres : la regex doit prendre le NOMBRE ENTIER.
  expect_identical(ts_gate_details("Resume : 15 erreur(s), 104 avertissement(s)",
                                   "conventions"),
                   "err=15 warn=104")
})

test_that("details ne MENT pas quand le motif est absent", {
  # Un garde muet ne doit pas produire « err=0 warn=0 » : ce serait affirmer une
  # mesure qui n'a pas eu lieu. On rend la chaine vide, qui se lit comme telle.
  expect_identical(ts_gate_details("rien a signaler", "conventions"), "")
  expect_identical(ts_gate_details(character(0), "conventions"), "")
  expect_identical(ts_gate_details("", "docs_hash"), "")
})

test_that("details rend un compte PARTIEL plutot qu'un faux zero", {
  # Seul `warn` present : ne pas inventer `err=0`.
  expect_identical(ts_gate_details("Resume : 4 avertissement(s)", "conventions"),
                   "warn=4")
  # Seul `err` present : symetriquement.
  expect_identical(ts_gate_details("Resume : 4 erreur(s)", "conventions"),
                   "err=4")
})

test_that("details ne confond pas un DESACCORD avec une erreur", {
  # `check_status_claims.R` dit « 0 desaccord » et non « 0 erreur(s) ». Le
  # brancher sur le motif `erreur` rendrait "" — un garde vert avec details
  # vide, indistinguable d'un garde muet.
  expect_identical(ts_gate_details("Resume : 0 desaccord", "status_claims"),
                   "desaccord=0")
  expect_identical(ts_gate_details("Resume : 3 desaccords", "status_claims"),
                   "desaccord=3")
  # Et inversement : un `desaccord` vu par un autre garde ne doit pas etre lu.
  expect_identical(ts_gate_details("Resume : 3 desaccord", "conventions"), "")
})

# =============================================================================
# 3. `ts_gate_run()` — trois verdicts, pas quatre
# =============================================================================

test_that("un garde ABSENT est ABSENT — surtout pas VERT", {
  root <- .fake_root()
  r <- ts_gate_run(root, list(id = "fantome", script = "tools/inexistant.R",
                              kind = "exit"), 10)
  expect_identical(r$verdict, "ABSENT")
  expect_false(identical(r$verdict, "VERT"))
  # Pas de code de sortie : il n'y a pas eu de processus.
  expect_true(is.na(r$exit))
  expect_identical(r$seconds, 0)
  expect_identical(r$details, "")
})

test_that("un garde qui sort 0 est VERT et ses details sont lus", {
  root <- .fake_root()
  .write_gate(root, "tools/g.R", .gate_green(0L, 59L))
  r <- ts_gate_run(root, list(id = "conventions", script = "tools/g.R",
                              kind = "exit"), 60)
  expect_identical(r$verdict, "VERT")
  expect_identical(r$exit, 0L)
  expect_identical(r$details, "err=0 warn=59")
  expect_true(r$seconds >= 0)
})

test_that("un garde qui sort != 0 est ROUGE, avec son code et ses details", {
  root <- .fake_root()
  .write_gate(root, "tools/g.R",
              c('cat("---- Resume : 2 erreur(s), 1 avertissement(s) ----\\n")',
                'quit(save = "no", status = 3L)'))
  r <- ts_gate_run(root, list(id = "conventions", script = "tools/g.R",
                              kind = "exit"), 60)
  expect_identical(r$verdict, "ROUGE")
  expect_identical(r$exit, 3L)
  # Les details restent EXTRAITS d'un garde rouge : c'est la que le nombre
  # d'erreurs sert le plus.
  expect_identical(r$details, "err=2 warn=1")
})

test_that("kind=parse : un script qui parse est VERT, un script casse est ROUGE", {
  root <- .fake_root()
  .write_gate(root, "scripts/ok.R", "f <- function(x) x + 1L")
  .write_gate(root, "scripts/broken.R", "f <- function(x { x + 1L }")
  gate <- function(rel) list(id = "mcp", script = rel, kind = "parse")

  ok <- ts_gate_run(root, gate("scripts/ok.R"), 60)
  expect_identical(ok$verdict, "VERT")
  expect_identical(ok$exit, 0L)
  # Un parse ne produit pas de resume : `details` reste vide (comme latest.json).
  expect_identical(ok$details, "")

  bad <- ts_gate_run(root, gate("scripts/broken.R"), 60)
  expect_identical(bad$verdict, "ROUGE")
  expect_identical(bad$details, "parse_error")
  expect_true(length(bad$output) >= 1L)
  # Anti-vacuite : le faux script est bien CASSE, pas juste mal ecrit.
  expect_error(parse(file.path(root, "scripts/broken.R")), class = "error")
})

test_that("le depassement de --timeout rend ROUGE, meme si le garde sort 0", {
  # ⚠️ `system2()` n'a pas de `timeout=` en base R : le depassement est mesure
  # APRES le retour. Ce test verifie donc le VERDICT, pas une mise a mort — et
  # le dit, pour qu'aucun lecteur ne croie a une interruption.
  root <- .fake_root()
  .write_gate(root, "tools/slow.R",
              c('Sys.sleep(1.5)', 'quit(save = "no", status = 0L)'))
  t0 <- Sys.time()
  r <- ts_gate_run(root, list(id = "lent", script = "tools/slow.R",
                              kind = "exit"), 0.3)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_identical(r$verdict, "ROUGE")
  expect_true(grepl("timeout", r$details, fixed = TRUE),
              info = paste("details =", r$details))
  # Le processus a bien ete LAISSE aller au bout : la duree le prouve.
  expect_true(elapsed >= 1.5, info = paste("elapsed =", round(elapsed, 2)))
})

test_that("un garde rapide ne declenche PAS le timeout (controle negatif)", {
  root <- .fake_root()
  .write_gate(root, "tools/g.R", .gate_green(0L, 0L))
  r <- ts_gate_run(root, list(id = "conventions", script = "tools/g.R",
                              kind = "exit"), 60)
  expect_identical(r$verdict, "VERT")
  expect_false(grepl("timeout", r$details, fixed = TRUE))
})

# =============================================================================
# 4. `ts_gates_census()` — identite de l'arbre
# =============================================================================

test_that("le census compte les lignes `CENSUS:` de test- seulement", {
  root <- .fake_tree_green()
  c1 <- ts_gates_census(root)
  expect_identical(c1$total, 3L)          # alpha, beta, gamma ; pas le commentaire
  expect_true(grepl("census.txt$", c1$source))
  # Un census absent ne vaut pas 0 : 0 serait une mesure, NA est une absence.
  root2 <- .fake_root()
  c2 <- ts_gates_census(root2)
  expect_true(is.na(c2$total))
  expect_true(is.na(c2$source))
})

# =============================================================================
# 5. `ts_gates_args()` — le CLI
# =============================================================================

test_that("les valeurs par defaut sont sures", {
  opt <- ts_gates_args(character(0))
  expect_null(opt$only)
  expect_false(opt$quiet)
  expect_false(opt$json)
  expect_false(opt$allow_absent)
  expect_identical(opt$timeout, 900)
  expect_null(opt$root)
})

test_that("les drapeaux booleens se lisent", {
  expect_true(ts_gates_args("--quiet")$quiet)
  expect_true(ts_gates_args("--json")$json)
  expect_true(ts_gates_args("--allow-absent")$allow_absent)
})

test_that("--only= accepte une liste, nettoie les espaces et les vides", {
  expect_identical(ts_gates_args("--only=conventions")$only, "conventions")
  expect_identical(ts_gates_args("--only=conventions,mcp")$only,
                   c("conventions", "mcp"))
  # Espaces et virgules en trop : un `--only=a, b` copie depuis un shell ne doit
  # pas produire l'id " b" (qui serait « inconnu »).
  expect_identical(ts_gates_args("--only= conventions , mcp ")$only,
                   c("conventions", "mcp"))
  expect_identical(ts_gates_args("--only=conventions,,mcp,")$only,
                   c("conventions", "mcp"))
})

test_that("--timeout= valide et refuse le non-sens", {
  expect_identical(ts_gates_args("--timeout=5")$timeout, 5)
  expect_identical(ts_gates_args("--timeout=0.5")$timeout, 0.5)
  # 0 ou negatif n'a pas de sens : ce serait un timeout toujours depasse.
  expect_error(ts_gates_args("--timeout=0"), "timeout")
  expect_error(ts_gates_args("--timeout=-3"), "timeout")
  expect_error(ts_gates_args("--timeout=abc"), "timeout")
})

test_that("--root= est capture tel quel", {
  expect_identical(ts_gates_args("--root=/tmp/x")$root, "/tmp/x")
})

test_that("un argument inconnu est une erreur d'usage, pas un silence", {
  # Un `--quite` mal orthographie ne doit pas lancer la melee en mode bavard :
  # il doit s'arreter. Un drapeau ignore est un drapeau qui ment sur ce qu'il a
  # fait.
  expect_error(ts_gates_args("--quite"), "inconnu")
  expect_error(ts_gates_args("positionnel"), "inconnu")
  expect_error(ts_gates_args("--only"), "inconnu")   # la forme = est requise
})

# =============================================================================
# 6. `ts_gates_main()` — le cablage complet, sur un FAUX arbre
# =============================================================================

test_that("un arbre entierement vert rend 0 et annonce VERT", {
  root <- .fake_tree_green()
  out <- capture.output(code <- ts_gates_main(paste0("--root=", root)))
  expect_identical(code, 0L)
  expect_true(any(grepl("^VERDICT: VERT", out)),
              info = paste(out, collapse = "\n"))
  # Les six lignes du registre doivent etre la, dans l'ordre.
  expect_true(any(grepl("conventions\\s+VERT", out)))
  expect_true(any(grepl("desaccord=0", out)))
})

test_that("un seul garde rouge rend 1", {
  root <- .fake_tree_green()
  .write_gate(root, "tools/check_duplication.R",
              c('cat("---- Resume : 2 erreur(s), 0 avertissement(s) ----\\n")',
                'quit(save = "no", status = 1L)'))
  out <- capture.output(code <- ts_gates_main(paste0("--root=", root)))
  expect_identical(code, 1L)
  expect_true(any(grepl("^VERDICT: ROUGE", out)))
  expect_true(any(grepl("err=2", out)))
})

test_that("un garde ABSENT rend 1 : ABSENT n'est pas VERT", {
  root <- .fake_tree_green()
  file.remove(file.path(root, "tools", "docs_hash_manifest.R"))
  out <- capture.output(code <- ts_gates_main(paste0("--root=", root)))
  expect_identical(code, 1L)
  expect_true(any(grepl("docs_hash\\s+ABSENT", out)))
  expect_true(any(grepl("^VERDICT: ROUGE", out)))
})

test_that("--allow-absent tolere l'absence SANS l'effacer", {
  # La difference est tout le sujet : on change la REGLE D'ECHEC, pas le
  # VERDICT. Reecrire ABSENT en VERT effacerait l'information — et c'est
  # exactement ainsi qu'une capacite perdue devient invisible.
  root <- .fake_tree_green()
  file.remove(file.path(root, "tools", "docs_hash_manifest.R"))
  out <- capture.output(code <- ts_gates_main(c(paste0("--root=", root),
                                                "--allow-absent")))
  expect_identical(code, 0L)
  # La ligne reste ABSENT dans la table...
  expect_true(any(grepl("docs_hash\\s+ABSENT", out)),
              info = paste(out, collapse = "\n"))
  # ...et aucune ligne ne pretend VERT a sa place.
  expect_false(any(grepl("docs_hash\\s+VERT", out)))
  expect_true(any(grepl("^VERDICT: VERT", out)))
})

test_that("--allow-absent ne sauve PAS un garde rouge", {
  # L'option porte sur l'ABSENCE, pas sur l'echec. Un garde qui a tourne et
  # echoue reste un echec.
  root <- .fake_tree_green()
  .write_gate(root, "tools/check_duplication.R",
              c('quit(save = "no", status = 1L)'))
  capture.output(code <- ts_gates_main(c(paste0("--root=", root),
                                         "--allow-absent")))
  expect_identical(code, 1L)
})

test_that("--only= ne lance que les gardes nommes", {
  root <- .fake_tree_green()
  # Un garde rouge HORS selection ne doit pas compter : c'est la definition de
  # --only, et c'est ce qui rend les filtres cibles utilisables.
  .write_gate(root, "tools/check_conventions.R",
              c('quit(save = "no", status = 1L)'))
  out <- capture.output(code <- ts_gates_main(c(paste0("--root=", root),
                                                "--only=mcp,status_claims")))
  expect_identical(code, 0L)
  expect_false(any(grepl("^conventions", out)))
  expect_true(any(grepl("^mcp", out)))
  expect_true(any(grepl("^status_claims", out)))
})

test_that("--only= inconnu est une erreur d'usage (code 2)", {
  root <- .fake_tree_green()
  msg <- capture.output(
    code <- suppressMessages(ts_gates_main(c(paste0("--root=", root),
                                             "--only=conventions,fantome"))),
    type = "message")
  expect_identical(code, 2L)
})

test_that("--only= qui ne retient RIEN est une erreur, jamais un VERT vide", {
  # `any(logical(0))` vaut FALSE : sans ce garde, la melee rendrait
  # « VERDICT: VERT (0 garde(s)) ». C'est le mensonge vert que ce fichier existe
  # pour empecher.
  root <- .fake_tree_green()
  capture.output(
    code <- suppressMessages(ts_gates_main(c(paste0("--root=", root), "--only="))),
    type = "message")
  expect_identical(code, 2L)
})

test_that("une racine introuvable est une erreur d'usage (code 2)", {
  capture.output(
    code <- suppressMessages(ts_gates_main("--root=/chemin/qui/n/existe/pas")),
    type = "message")
  expect_identical(code, 2L)
})

test_that("--quiet ne montre que la ligne de verdict", {
  root <- .fake_tree_green()
  out <- capture.output(code <- ts_gates_main(c(paste0("--root=", root),
                                                "--quiet")))
  expect_identical(code, 0L)
  expect_false(any(grepl("^GARDE", out)))
  expect_false(any(grepl("conventions", out)))
  expect_true(any(grepl("^VERDICT:", out)))
})

# =============================================================================
# 7. Sortie JSON — le contrat de `gates_history/`
# =============================================================================

test_that("--json produit du JSON PUR (aucune table avant)", {
  # `latest.json` porte `json_parse_ok: true` : le consommateur PARSE cette
  # sortie. Une table imprimee avant le JSON rendrait cela impossible.
  skip_if_not_installed("jsonlite")
  root <- .fake_tree_green()
  out <- capture.output(code <- ts_gates_main(c(paste0("--root=", root),
                                                "--json")))
  expect_identical(code, 0L)
  expect_false(any(grepl("^GARDE", out)),
               info = "une table a ete imprimee avant le JSON")
  parsed <- jsonlite::fromJSON(paste(out, collapse = "\n"))
  expect_type(parsed, "list")
  expect_true(all(c("horodate_utc", "tree_identity", "gates",
                    "gates_exit", "verdict") %in% names(parsed)))
})

test_that("ts_gates_main ne pollue pas sa sortie machine avec son code de retour", {
  # Mesure 2026-10-24 : appelee SANS affectation, la fonction imprimait `[1] 0`
  # APRES le JSON. Le consommateur voyait « trailing garbage » et
  # `json_parse_ok` serait tombe a `false`. Une fonction qui ecrit une sortie
  # machine ne doit pas y ajouter sa valeur de retour : elle la rend invisible.
  # Ce test appelle donc SANS affectation, exprès.
  skip_if_not_installed("jsonlite")
  root <- .fake_tree_green()
  out <- capture.output(ts_gates_main(c(paste0("--root=", root), "--json")))
  expect_false(any(grepl("^\\[1\\]", out)),
               info = paste("valeur de retour imprimee :", paste(out, collapse = " | ")))
  expect_silent(jsonlite::fromJSON(paste(out, collapse = "\n")))
})

test_that("--json porte le registre entier, dans l'ordre, avec le bon schema", {
  skip_if_not_installed("jsonlite")
  root <- .fake_tree_green()
  out <- capture.output(code <- ts_gates_main(c(paste0("--root=", root), "--json")))
  p <- jsonlite::fromJSON(paste(out, collapse = "\n"))
  expect_identical(code, 0L)
  expect_identical(p$gates$id, ts_gates_ids())
  expect_identical(p$verdict, "VERT")
  expect_identical(p$gates_exit, 0L)
  expect_true(all(p$gates$verdict == "VERT"))
  # `details` est le format mesure : err= / warn= / desaccord=
  expect_identical(p$gates$details[p$gates$id == "conventions"],
                   "err=0 warn=60")
  expect_identical(p$gates$details[p$gates$id == "status_claims"],
                   "desaccord=0")
  # L'identite de l'arbre vient du census, pas d'un recomptage.
  expect_identical(p$tree_identity$census_total, 3L)
  expect_true(grepl("census.txt$", p$tree_identity$census_source))
})

test_that("--json en echec porte gates_exit=1 et le verdict ROUGE", {
  skip_if_not_installed("jsonlite")
  root <- .fake_tree_green()
  file.remove(file.path(root, "tools", "docs_hash_manifest.R"))
  out <- capture.output(code <- ts_gates_main(c(paste0("--root=", root),
                                                "--json")))
  expect_identical(code, 1L)
  p <- jsonlite::fromJSON(paste(out, collapse = "\n"))
  expect_identical(p$verdict, "ROUGE")
  expect_identical(p$gates_exit, 1L)
  expect_identical(p$gates$verdict[p$gates$id == "docs_hash"], "ABSENT")
  # Un ABSENT n'a pas de code de sortie : `null` (=> NA cote R), jamais la
  # chaine "NA" que produirait un `toJSON()` sans `na = "null"`. On teste le
  # TYPE, pas la valeur : c'est le type qui distingue null de "NA".
  expect_false(is.character(p$gates$exit))
  expect_true(is.na(p$gates$exit[p$gates$id == "docs_hash"]))
})

test_that("--json n'emet PAS les cles de verification de scheduled_verify", {
  # `status_claims_exit` et `json_parse_ok` decrivent la VERIFICATION de la
  # sortie, pas la sortie. Elles appartiennent a l'enveloppe. Les emettre ici
  # ferait croire que ce script se verifie lui-meme.
  skip_if_not_installed("jsonlite")
  root <- .fake_tree_green()
  out <- capture.output(code <- ts_gates_main(c(paste0("--root=", root), "--json")))
  p <- jsonlite::fromJSON(paste(out, collapse = "\n"))
  expect_identical(code, 0L)
  expect_false("status_claims_exit" %in% names(p))
  expect_false("json_parse_ok" %in% names(p))
})

# =============================================================================
# 8. Racine et executable — deux pieges mesures
# =============================================================================

test_that("la racine par defaut existe et n'est jamais vide", {
  r <- ts_gates_root()
  expect_true(nzchar(r))
  expect_true(dir.exists(r))
})

test_that("l'executable R est celui de CE R, pas celui du PATH", {
  # Prendre `Rscript` du PATH pourrait lancer une AUTRE version, donc un AUTRE
  # renv — et un garde d'hermeticite evalue par le mauvais R ne mesure rien.
  exe <- ts_gates_rscript()
  expect_true(file.exists(exe))
  expect_identical(normalizePath(dirname(exe), winslash = "/"),
                   normalizePath(R.home("bin"), winslash = "/"))
})

test_that("la commande n'est PAS re-quotee pour system2 (piege mesure)", {
  # 🔴 Mesure du 2026-10-24 : `system2(shQuote(exe), ...)` echoue avec
  #   '"\"D:/.../Rscript.exe\""' not found
  # parce que `system2()` quote DEJA son premier argument sur Windows. La lecon
  # « tout passer par shQuote() » est vraie pour `args` et fausse pour
  # `command`. Ce test garde la forme correcte.
  #
  # ⚠️ On lit le code PARSE et DEPARSE, pas le texte brut : `parse()` jette les
  # commentaires. Sans cela ce test rougit sur sa propre documentation — la
  # meme classe de faux positif que le garde « engine is pure R », qui lit le
  # source commentaires inclus (piege mesure dans ce depot).
  f <- file.path(ts_project_root(), "tools", "run_all_gates.R")
  code <- paste(deparse(parse(f, keep.source = FALSE)), collapse = "\n")
  expect_false(grepl("system2(shQuote(", code, fixed = TRUE),
               info = "l'executable est re-quote : system2 echouera")
  expect_true(grepl("system2(ts_gates_rscript()", code, fixed = TRUE))
  # Les ARGUMENTS, eux, doivent etre quotes : la racine contient des espaces.
  expect_true(grepl("shQuote(path)", code, fixed = TRUE))
  # Anti-vacuite : la lecture a bien ramene du code, et pas une chaine vide.
  expect_true(nchar(code) > 1000L)
  expect_true(grepl("ts_gates_registry", code, fixed = TRUE))
})

# =============================================================================
# 9. Integration reelle (couteuse : un seul garde, le plus rapide)
# =============================================================================

test_that("le garde `mcp` reel : scripts/mcp_server.R parse vraiment", {
  # Le registre declare ce garde comme une HYPOTHESE. Ce test verifie que
  # l'hypothese porte sur un fichier qui existe et qui parse — sans quoi le
  # garde serait vert par vacuite (fichier absent = ABSENT, deja couvert).
  root <- ts_project_root()
  p <- file.path(root, .gate_of("mcp")$script)
  skip_if_not(file.exists(p), "scripts/mcp_server.R absent de cet arbre")
  expect_error(parse(p, keep.source = FALSE), NA)
  r <- ts_gate_run(root, .gate_of("mcp"), 60)
  expect_identical(r$verdict, "VERT")
})

test_that("le meta-gate reel signale l'outil perdu au lieu de l'ignorer", {
  # C'est la PROPRIETE qui justifie tout ce fichier : la disparition des six
  # outils du lot 6-7 n'a fait rougir aucune suite. Elle doit maintenant
  # produire une ligne ABSENT visible.
  #
  # ⚠️ Pas de `skip()` ici : un skip n'est PAS un pass, et un test qui
  # s'auto-desactive le jour ou l'outil revient ne prouve plus rien. Les deux
  # branches assertent quelque chose de reel.
  root <- ts_project_root()
  present <- file.exists(file.path(root, "tools", "docs_hash_manifest.R"))
  r <- ts_gate_run(root, .gate_of("docs_hash"), 60)
  if (!present) {
    expect_identical(r$verdict, "ABSENT")
    expect_false(identical(r$verdict, "VERT"))
  } else {
    # Reconstruit : le garde doit alors s'executer, donc ne plus etre ABSENT.
    expect_false(identical(r$verdict, "ABSENT"))
  }
})

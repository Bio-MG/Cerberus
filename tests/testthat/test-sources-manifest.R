# =============================================================================
# test-sources-manifest.R -- GARDE D2-M1 : le manifeste R/.sources_manifest.txt
# =============================================================================
# POURQUOI CE FICHIER (plan D2 jalon M1,
# docs/proposals/D2_SOURCE_RETIREMENT_PLAN.md rev. 2 -- document local)
#
# app.R source 155 fichiers : 145 source("...") explicites + 10 fichiers via
# la boucle list.files("modules/bulk_de") (app.R:197). Cette liste ordonnee
# est le prochain chantier de retrait (D2 M2+) : avant de la decouper, elle
# doit etre ECRITE quelque part et verifiee par machine. Le manifeste
# R/.sources_manifest.txt est genere par tools/sources_manifest.R (parse AST
# d'app.R, boucle developpee) ; ce garde RE-EXTRAIT la liste par REGEX --
# memes expressions que test-app-sourcing.R:32-58 -- et exige l'accord des
# DEUX methodes independantes.
#
# Ce que le garde epingle :
#   (a) extraction regex d'app.R == manifeste, a l'ordre pres ET dans
#       l'ORDRE EXACT (la position de la boucle dans le texte compte) ;
#   (b) disque R/ + modules/ = manifeste ∪ allowlist (meme propriete
#       d'exhaustivite que test-app-sourcing.R, vue depuis le manifeste) ;
#   (c) contraintes DOUCES (plan §1.4) : les 5 consommateurs TS_* doivent
#       apparaitre APRES config/defaults.R et config/thresholds.R -- sinon
#       les replis codes en dur ecrasent silencieusement la configuration.
#
# FALSIFICATION constatee a l'ecriture du garde (2026-10-24, RED-first) :
# manifeste absent => bloc 1 ROUGE ; retirer une ligne du manifeste =>
# bloc (a) ROUGE ; deplacer un source() apres un consommateur TS_* =>
# bloc (c) ROUGE. Les cas negatifs SYNTHETIQUES ci-dessous gardent les
# detecteurs non-vides pour toujours (meme discipline que
# test-app-sourcing.R:92-103).
#
# ASCII pur : un garde ne doit pas dependre de la locale (cf. CONVENTIONS
# C7). Note : l'expansion de la boucle eponge LC_COLLATE="C" le temps de
# list.files() puis restaure -- l'ordre des 10 fichiers bulk_de doit etre
# le meme quel que soit l'appelant (plan §1.4 : aucune contrainte DURE
# entre eux, l'ordre epingle est canonique, pas comportemental).
# =============================================================================

.sm_root <- ts_project_root()

.sm_app_text <- paste(readLines(file.path(.sm_root, "app.R"), warn = FALSE),
                      collapse = "\n")

.sm_manifest_path <- file.path(.sm_root, "R", ".sources_manifest.txt")

.sm_read_manifest <- function(path = .sm_manifest_path) {
  if (!file.exists(path)) return(NULL)
  readLines(path, warn = FALSE)
}

# Expansion d'une boucle list.files(<dir>, pattern = "\\.R$") : les chemins
# relatifs au projet, forward slashes, ordre = list.files sous LC_COLLATE=C.
.sm_expand_dir <- function(dir, root) {
  full <- file.path(root, dir)
  if (!dir.exists(full)) return(character(0))
  old_coll <- Sys.setlocale("LC_COLLATE", "C")
  on.exit(Sys.setlocale("LC_COLLATE", old_coll), add = TRUE)
  file.path(dir, list.files(full, pattern = "\\.R$"))
}

# Extraction ORDONNEE : un seul balayage du texte d'app.R, les source("...")
# ET les list.files("...") dans l'ordre ou ils apparaissent -- la boucle est
# developpee a SA position textuelle (entre mod_bulk_filter.R et
# mod_bulk_pathways_export.R), pas rejetee en fin de liste.
.sm_ordered <- function(text, root) {
  pat <- 'source\\("([^"]+)"\\)|list\\.files\\(\\s*"([^"]+)"'
  m <- gregexpr(pat, text, perl = TRUE)[[1L]]
  if (!length(m) || m[1L] == -1L) return(character(0))
  starts <- attr(m, "capture.start")
  lens <- attr(m, "capture.length")
  out <- character(0)
  for (i in seq_along(m)) {
    if (lens[i, 1L] > 0L) {
      out <- c(out, substr(text, starts[i, 1L],
                           starts[i, 1L] + lens[i, 1L] - 1L))
    } else if (lens[i, 2L] > 0L) {
      d <- substr(text, starts[i, 2L], starts[i, 2L] + lens[i, 2L] - 1L)
      out <- c(out, .sm_expand_dir(d, root))
    }
  }
  out
}

# Les 5 consommateurs TS_* du plan §1.4 (re-verifies par grep en revision 2)
# -- chacun lit une constante config/ DERRIERE un garde exists() a repli
# code en dur : sourcer un consommateur AVANT config/ remplace la valeur
# configuree par le repli SANS aucune erreur.
.sm_ts_consumers <- c(
  "tools/pkg_skeleton/R/spatial_async.R",         # TS_MIRAI_TIMEOUT_MS, TS_RCTD_TIMEOUT_MS, TS_LABEL_TRANSFER_TIMEOUT <- defaults (ex-R/spatial/, D2 M5 lot 4)
  "tools/pkg_skeleton/R/spatial_deconv_prep.R",   # TS_DECONV_DEFAULT_N_HVG <- defaults (ex-R/spatial/, D2 M5 lot 4)
  "modules/sc/mod_sc_pipeline.R",                 # TS_AUTO_TSNE_MAX_CELLS, TS_BPCELLS_AUTO_THRESHOLD <- thresholds
  "modules/sc/mod_sc_trajectory.R",               # TS_MAX_TRAJECTORY_CELLS <- thresholds
  "modules/spatial/deconv/mod_spatial_deconv.R"   # TS_RCTD_MIN_CELLS_PER_TYPE <- defaults
)

# D2 M5 lot 5 : defaults.R + thresholds.R vivent dans l'arbre du paquet
# (ex-config/*, app.R Bloc 02 mêmes positions) — l'épingle d'ordre suit.
.sm_config <- c("tools/pkg_skeleton/R/defaults.R", "tools/pkg_skeleton/R/thresholds.R")

# Doit rester IDENTIQUE a .app_allowlist (test-app-sourcing.R:74-85) : ce
# sont des decisions documentees, pas des oublis. Le bloc (d) verifie la
# coherence des DEUX cotes.
# D2 M5 lot 6 : sc_state.R suit son domaine dans l'arbre du paquet
# (ex-R/sc/) — entree INERTE face au scan disque, conservee pour la coherence
# (cf. test-app-sourcing.R).
.sm_allowlist <- c("tools/pkg_skeleton/R/sc_state.R", "modules/spatial/mod_spatial_lr.R")

.sm_disk <- function(root) {
  sort(unlist(lapply(c("R", "modules"), function(d) {
    full <- file.path(root, d)
    if (!dir.exists(full)) return(character(0))
    gsub("\\\\", "/", file.path(d, list.files(full, pattern = "\\.R$",
                                              recursive = TRUE)))
  }), use.names = FALSE))
}

# Detecteur d'ordre isole (falsifiable sur donnees synthetiques) : tous les
# consommateurs APRES toutes les config/.
.sm_order_ok <- function(ordered, consumers, config) {
  pc <- match(config, ordered)
  pf <- match(consumers, ordered)
  length(ordered) > 0L && !any(is.na(pc)) && !any(is.na(pf)) &&
    all(pf > max(pc))
}

# --- Cas NEGATIFS synthetiques : les detecteurs detectent -------------------

test_that("the ordered extractor itself works (negative case)", {
  fake <- paste(c(
    'source("R/a.R")',
    'for (f in list.files("modules/bulk_de", pattern = "\\\\.R$", full.names = TRUE)) source(f)',
    'source("R/b.R")'
  ), collapse = "\n")
  got <- .sm_ordered(fake, .sm_root)
  # la boucle est developpee a SA POSITION : a, puis les 10 bulk_de, puis b
  expect_identical(got[1L], "R/a.R")
  expect_identical(got[length(got)], "R/b.R")
  expect_identical(length(got) - 2L, 10L)
  expect_true(all(grepl("^modules/bulk_de/", got[-c(1L, length(got))]))
              && !anyDuplicated(got))
  # un texte sans directive rend vide -- pas une erreur silencieuse
  expect_length(.sm_ordered("aucune directive ici", .sm_root), 0L)
})

test_that("the order detector flags an inverted list (negative case)", {
  config <- c("tools/pkg_skeleton/R/defaults.R", "tools/pkg_skeleton/R/thresholds.R")
  good <- c(config, "R/a.R", "R/b.R")
  bad <- c("R/a.R", config, "R/b.R")   # consommateur R/a.R AVANT config/
  expect_true(.sm_order_ok(good, c("R/a.R", "R/b.R"), config))
  expect_false(.sm_order_ok(bad, c("R/a.R", "R/b.R"), config))
  # manifeste truque : une entree retiree est VUE par le comparateur d'ensemble
  expect_false(setequal(good, good[-2L]))
})

# --- Le garde lui-meme ------------------------------------------------------

test_that("le manifeste existe et n'est pas trivial", {
  lines <- .sm_read_manifest()
  expect_false(is.null(lines),
    info = paste("R/.sources_manifest.txt ABSENT -- le generer :",
                 "Rscript tools/sources_manifest.R --build"))
  if (is.null(lines)) return(invisible(NULL))
  # tripwire de vacuite : 155 fichiers attendus (145 explicites + 10 boucle) ;
  # un extracteur regex casse rendrait peu ou rien -- 100 = plancher.
  expect_true(length(lines) >= 100L,
    info = sprintf("manifeste trivial : %d lignes (155 attendues)", length(lines)))
  # aucune ligne vide ni doublon : le format est une liste ORDONNEE de chemins
  expect_false(any(!nzchar(lines)))
  expect_identical(anyDuplicated(lines), 0L)
})

test_that("(a) extraction regex d'app.R == manifeste, ordre exact", {
  extracted <- .sm_ordered(.sm_app_text, .sm_root)
  lines <- .sm_read_manifest()
  if (is.null(lines)) lines <- character(0)
  expect_true(setequal(extracted, lines),
    info = "le manifeste et la liste reellement sourcee par app.R different")
  expect_identical(extracted, lines,
    info = paste("ORDRE divergent entre app.R et le manifeste :",
                 "la position de la boucle bulk_de et des blocs compte (D2 M2)"))
})

test_that("(b) disque R/ + modules/ = manifeste U allowlist", {
  lines <- .sm_read_manifest()
  if (is.null(lines)) lines <- character(0)
  under <- lines[grepl("^(R|modules)/", lines)]
  on_disk <- .sm_disk(.sm_root)
  missing <- setdiff(on_disk, c(under, .sm_allowlist))
  extra <- setdiff(under, on_disk)
  expect_identical(missing, character(0),
    info = paste0("fichier(s) sur disque JAMAIS source(s) (manifeste + allowlist) : ",
                  paste(missing, collapse = ", ")))
  expect_identical(extra, character(0),
    info = paste0("entree(s) du manifeste sans fichier sur disque : ",
                  paste(extra, collapse = ", ")))
})

test_that("(c) contraintes douces : 5 consommateurs TS_* APRES config/*", {
  lines <- .sm_read_manifest()
  if (is.null(lines)) lines <- character(0)
  expect_true(.sm_order_ok(lines, .sm_ts_consumers, .sm_config),
    info = paste0("un consommateur TS_* (repli exists() silencieux, plan §1.4) ",
                  "apparait AVANT config/defaults.R ou config/thresholds.R ",
                  "dans le manifeste : la valeur configuree serait ecrasee ",
                  "par le repli code en dur SANS erreur."))
})

test_that("(d) l'allowlist du manifeste == celle du garde d'exhaustivite", {
  # Les DEUX gardes doivent nommer les memes exceptions documentees : si l'une
  # diverge, l'une des deux proprietes d'exhaustivite est fausse. On ne lit
  # QUE le bloc .app_allowlist <- c( ... ) -- le reste du garde cite d'autres
  # chemins (fixtures de son cas negatif) qui ne sont PAS des exceptions.
  guard <- readLines(file.path(.sm_root, "tests/testthat/test-app-sourcing.R"),
                     warn = FALSE)
  i0 <- grep("\\.app_allowlist <- c\\(", guard)[1L]
  expect_false(is.na(i0),
    info = ".app_allowlist introuvable dans test-app-sourcing.R")
  if (is.na(i0)) return(invisible(NULL))
  after <- which(grepl("^\\)", guard[(i0 + 1L):length(guard)]))
  i1 <- if (length(after)) i0 + after[1L] else NA_integer_
  expect_false(is.na(i1),
    info = "bloc .app_allowlist non ferme dans test-app-sourcing.R")
  if (is.na(i1)) return(invisible(NULL))
  block <- paste(guard[i0:i1], collapse = "\n")
  # D2 M5 lot 6 : les exceptions peuvent vivre dans l'arbre du paquet
  # (sc_state.R, ex-R/sc/, INERTE face au scan disque) — le motif suit.
  quoted <- unlist(regmatches(
    block, gregexpr('"((R|modules)/|tools/pkg_skeleton/R/)[^"]+\\.R"', block, perl = TRUE)))
  guard_listed <- sort(unique(gsub('"', "", quoted)))
  expect_setequal(sort(.sm_allowlist), guard_listed)
})

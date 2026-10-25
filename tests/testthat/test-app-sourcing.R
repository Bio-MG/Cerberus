# =============================================================================
# test-app-sourcing.R -- GARDE : app.R source TOUT fichier de R/ et modules/
# =============================================================================
# POURQUOI CE FICHIER
#
# Incident du 2026-09-16 : l'application ne demarrait plus du tout --
#   Erreur dans bulk_gene_set_choices(.tr_plain) :
#   impossible de trouver la fonction "bulk_gene_set_choices"
#   Called from: hasGroups(choices)
#
# Cause : R/bulk/bulk_gene_sets.R (ajoute par 440878b) existait et etait
# TESTE, mais n'etait PAS source par app.R. Le test le sourcait lui-meme
# (source_project_file), donc il passait pendant que l'app etait cassee :
# le harnais de test contournait la liste de sources de app.R.
#
# MEME CLASSE, trouvee en verifiant : R/plotting/plot_dims.R (PLOT-S6) n'etait
# pas source non plus, alors que global.R enveloppe renderPlot() et appelle
# ts_render_plot_args() a CHAQUE renderPlot() => deuxieme P0, masque par le
# premier (l'erreur d'UI tombe avant le premier plot).
#
# PRECEDENT (le "patch CellChat") : test-sc-communication-engine-ui.R portait
# deja un test_that("app.R sources the engine") pour UN fichier precis. Ici on
# generalise : la propriete verifiee est l'EXHAUSTIVITE, pas un fichier.
#
# M2 (plan D2, 2026-10-24, docs/proposals/D2_SOURCE_RETIREMENT_PLAN.md rev. 2) :
# les 155 source() d'app.R vivent desormais en 12 BLOCS DE DOMAINE INLINE
# (bandereaux "# -- Bloc NN : <domaine> --" ; blocs inline, PAS de fichiers de
# blocs sous R/ : .app_on_disk() les flotterait et R CMD check les parserait
# comme code de package). L'ancienne boucle list.files('modules/bulk_de') est
# DEPLIEE en 10 source() explicites (bloc 11) : plus de tri alphabetique
# implicite comme variable cachee. L'ordre inter-bloc ET intra-bloc est FIGE
# par R/.sources_manifest.txt (garde soeur : test-sources-manifest.R).
#
# ASCII pur : un garde ne doit pas dependre de la locale (cf. CONVENTIONS C7).
# =============================================================================

.app_src <- paste(readLines(file.path(ts_project_root(), "app.R"), warn = FALSE),
                  collapse = "\n")

# --- 1. Sources declarees explicitement : source("...") ---------------------
.app_explicit <- function(src) {
  hits <- regmatches(src, gregexpr('source\\("([^"]+)"\\)', src))[[1L]]
  if (!length(hits)) return(character(0))
  sub('"\\)$', "", sub('^source\\("', "", hits))
}

# --- 2. Boucles de sourcing list.files(<dir>, pattern = "\\.R$") -------------
# AVANT M2 : app.R faisait
#   for (f in list.files("modules/bulk_de", pattern = "\\.R$", ...)) source(f)
# DEPUIS M2 (2026-10-24) : la boucle est DEPLIEE, cette fonction ne doit plus
# rien trouver -- elle reste comme detecteur de REINTRODUCTION (une boucle
# de sourcing re-cree le tri alphabetique implicite, variable cachee).
.app_listed <- function(src) {
  dirs <- regmatches(src, gregexpr('list\\.files\\(\\s*"([^"]+)"', src))[[1L]]
  if (!length(dirs)) return(character(0))
  # ATTENTION : regmatches() renvoie la correspondance ENTIERE, guillemet
  # fermant INCLUS ("list.files(\"modules/bulk_de\""). Ne retirer que le prefixe
  # laissait un " en fin de chaine => dir.exists() FALSE => 0 fichier, et le
  # garde signalait 9 faux manquants (mesure 2026-09-16). D'ou les DEUX sub().
  dirs <- sub('^list\\.files\\(\\s*"', "", dirs)
  dirs <- sub('"$', "", dirs)
  out <- character(0)
  for (d in dirs) {
    full <- file.path(ts_project_root(), d)
    if (dir.exists(full)) {
      out <- c(out, file.path(d, list.files(full, pattern = "\\.R$")))
    }
  }
  out
}

# --- 2b. Blocs de domaine inline (M2) ----------------------------------------
# Extrait, ligne a ligne, les bandereaux "# ... Bloc NN :" et regroupe sous
# chaque bandereau les source("...") qui le suivent. Le motif est volontairement
# ASCII ("Bloc NN :") : le bandereau reel d'app.R porte des tirets box-drawing
# UTF-8, le garde ne doit jamais dependre de la locale (C7).
.app_block_pattern <- '^#.*Bloc ([0-9]{2}) :'

.app_blocks <- function(lines) {
  blocks <- list()
  current <- NA_character_
  for (ln in lines) {
    m <- regmatches(ln, regexec(.app_block_pattern, ln))[[1L]]
    if (length(m) == 2L) {
      current <- m[2L]
      if (is.null(blocks[[current]])) blocks[[current]] <- character(0)
      next
    }
    s <- regmatches(ln, regexec('^source\\("([^"]+)"\\)', ln))[[1L]]
    if (length(s) == 2L && !is.na(current)) {
      blocks[[current]] <- c(blocks[[current]], s[2L])
    }
  }
  blocks
}

# --- 3. Tous les fichiers .R du domaine, sur disque -------------------------
.app_on_disk <- function() {
  out <- character(0)
  for (d in c("R", "modules")) {
    full <- file.path(ts_project_root(), d)
    if (dir.exists(full)) {
      out <- c(out, file.path(d, list.files(full, pattern = "\\.R$",
                                            recursive = TRUE)))
    }
  }
  sort(gsub("\\\\", "/", out))
}

# --- 4. Exceptions DECLAREES (chacune justifiee) ----------------------------
.app_allowlist <- c(
  # Re-export LEGACY : la definition canonique est state.R (M5 lot 1 : vit
  # dans tools/pkg_skeleton/R/), source avant lui. Son propre en-tete dit
  # "app.R no longer sources this file as canonical" -- il ne sert qu'aux
  # tests/appels externes qui le sourcent directement.
  "R/sc/sc_state.R",
  # Module PARQUE (vague 8 / backlog, session separee) : definit
  # mod_spatial_lr_ui/_server mais AUCUN appelant dans le depot (verifie :
  # grep "mod_spatial_lr" hors de ce fichier => 0). L'app demarre sans.
  # A RETIRER de cette liste le jour ou il est branche.
  "modules/spatial/mod_spatial_lr.R"
)

.app_missing <- function(on_disk, declared, allow = .app_allowlist) {
  setdiff(setdiff(on_disk, declared), allow)
}

.app_manifest <- function() {
  readLines(file.path(ts_project_root(), "R", ".sources_manifest.txt"),
            warn = FALSE)
}

# --- 5. Le detecteur lui-meme (cas NEGATIF, obligatoire) -------------------
test_that("the detector itself works (negative case)", {
  # Sans ce test, un detecteur casse renverrait "0 manquant" et le garde
  # serait vert POUR TOUJOURS -- exactement le faux temoin qu'on veut eviter
  # (meme classe que le filtre de run_tests.R, corrige le meme jour).
  fake_disk <- c("R/a.R", "R/b.R", "R/sc/sc_state.R")
  expect_identical(
    .app_missing(fake_disk, declared = c("R/a.R"), allow = c("R/sc/sc_state.R")),
    "R/b.R"
  )
  # et il ne doit RIEN signaler quand tout est declare
  expect_length(.app_missing(fake_disk, fake_disk), 0L)
})

test_that("M2 semantics: 155 explicit sources, zero hidden sourcing loop", {
  # ANCIEN seuil (2026-09-16) : expect_gt(length(.app_explicit(.app_src)), 50L)
  # -- plancher anti-extracteur-cassé, remplacé en M2 par un épingle EXACT :
  # 155 = 145 source() historiques + 10 de l'ancienne boucle bulk_de DEPLIEE
  # (bloc 11). Tout ajout/retrait de fichier source DOIT reecrire ce nombre
  # consciemment, pas glisser sous un plancher magique.
  expect_identical(
    length(.app_explicit(.app_src)), 155L,
    info = paste("app.R doit porter exactement 155 source() explicites",
                 "(M2 : 145 + 10 bulk_de deplies) -- le compte observe differe")
  )
  # et plus AUCUNE boucle list.files() de sourcing : M2 a deplie la derniere,
  # le tri alphabetique implicite etait une variable cachee.
  expect_identical(
    length(.app_listed(.app_src)), 0L,
    info = paste("une boucle list.files() de sourcing est reapparue dans",
                 "app.R : la deplier en source() explicites (M2)")
  )
})

test_that("every R/ and modules/ file is sourced by app.R", {
  missing <- .app_missing(.app_on_disk(),
                          c(.app_explicit(.app_src), .app_listed(.app_src)))
  expect_identical(
    missing, character(0),
    info = paste0(
      "Fichier(s) present(s) sur disque mais JAMAIS source(s) par app.R : ",
      paste(missing, collapse = ", "),
      " -- l'app plantera au premier appel (impossible de trouver la fonction). ",
      "Ajouter le source() dans app.R, ou inscrire le fichier dans ",
      ".app_allowlist avec sa justification."
    )
  )
})

test_that("no stale source() entry points at a missing file", {
  declared <- .app_explicit(.app_src)
  declared <- declared[grepl("\\.R$", declared)]
  absent <- declared[!file.exists(file.path(ts_project_root(), declared))]
  expect_identical(
    absent, character(0),
    info = paste("source() vers un fichier inexistant :",
                 paste(absent, collapse = ", "))
  )
})

test_that("the 12 inline domain blocks mirror the manifest (order frozen)", {
  lines <- readLines(file.path(ts_project_root(), "app.R"), warn = FALSE)
  blocks <- .app_blocks(lines)
  expect_identical(
    length(blocks), 12L,
    info = sprintf("12 blocs de domaine inline attendus (M2), %d trouves",
                   length(blocks))
  )
  expect_identical(
    names(blocks), sprintf("%02d", 1:12),
    info = paste("les bandereaux Bloc 01..Bloc 12 doivent etre numerotes",
                 "consecutivement et dans l'ordre -- observes :",
                 paste(names(blocks), collapse = ", "))
  )
  expect_true(
    all(lengths(blocks) >= 1L),
    info = "chaque bloc de domaine doit contenir au moins un source()"
  )
  # La propriete CENTRALE de M2 : l'ordre inter-bloc et intra-bloc est figé
  # par le manifeste -- la concatenation des blocs EST le manifeste.
  expect_identical(
    unlist(blocks, use.names = FALSE), .app_manifest(),
    info = paste("la liste ordonnee des source() d'app.R (blocs concatenes)",
                 "differe du manifeste R/.sources_manifest.txt --",
                 "l'ordre de sourcing est FIGE (plan D2 M2)")
  )
})

test_that("the two 2026-09-16 P0 fixes stay pinned (via the manifest)", {
  # Regression exacte de l'incident : ces deux fichiers ne doivent jamais
  # disparaitre de la liste. Depuis M2, l'epingle est ecrite CONTRE LE
  # MANIFESTE (liste ordonnee machine-verifiee, cf. test-sources-manifest.R)
  # et contre l'extraction directe d'app.R -- les deux cotes, parce que
  # l'incident etait l'ABSENCE de sourcing, pas l'ordre.
  # D2 M4-pilote (2026-10-25) : plot_dims.R a DEMENAGE dans l'arbre du paquet
  # (tools/pkg_skeleton/R/) — app.R le source() TOUJOURS, au meme endroit de
  # l'ordre du manifeste. L'epingle suit le chemin : la propriete pie ne
  # change pas (ce fichier doit rester source par app.R, P0 2026-09-16).
  # D2 M5 lot 3 (2026-10-25) : bulk_gene_sets.R suit (domaine R/bulk ENTIER
  # dans l'arbre du paquet) — meme epingle, meme propriete.
  # Cf. test-plotting-pkg-pilot.R (frontiere du pilote) et le rapport M4.
  p0 <- c("tools/pkg_skeleton/R/bulk_gene_sets.R", "tools/pkg_skeleton/R/plot_dims.R")
  manifest <- .app_manifest()
  for (f in p0) {
    expect_true(
      f %in% manifest,
      info = paste(f, "ABSENT du manifeste R/.sources_manifest.txt (P0 2026-09-16)")
    )
    expect_true(
      f %in% .app_explicit(.app_src),
      info = paste(f, "n'est plus source() explicitement par app.R (P0 2026-09-16)")
    )
  }
})

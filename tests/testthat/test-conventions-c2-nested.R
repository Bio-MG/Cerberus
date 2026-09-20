# =============================================================================
# test-conventions-c2-nested.R — ancrage de la détection de fonctions (L311)
# =============================================================================
# Défaut RÉEL (instrut au 46e incrément, §2dg) : le motif de détection de
# fonctions de `tools/check_conventions.R` (L311) est ancré en `^` **sans
# `\\s*`**. Or `ann$code` provient de `.strip_code_lines()`, qui **ne trim pas** :
# il conserve la colonne d'origine. Une définition **imbriquée** (donc INDENTÉE)
# n'est donc **jamais** vue comme « fonction englobante ».
#
# Le consommateur est **C2** (`check_c2_shiny_in_r`) : il rafraîchit
# `formals <- .parse_formals(sig)` à chaque définition reconnue, puis décide
#
#     if (grepl(.SHINY_SOFT[[nm]], ln) && !(nm %in% formals)) .add("ERROR", ...)
#
# ⇒ Un helper local qui **déclare** `input`/`session` en paramètre — la façon
#   CORRECTE d'écrire du code réutilisable — voit ses appels signalés en
#   **ERROR** : la déclaration est invisible, donc absent de `formals`.
#
# ⚠️ PORTÉE RÉELLE, MESURÉE (ne pas présumer) :
#   - la sonde EMPIRIQUE ci-dessous PROUVE le faux positif (2 signalements,
#     mesuré hors test avant d'écrire ce fichier) ;
#   - la population `input$`/`session$` de `R/` **réellement** en faux positif
#     vaut **0** (mesuré avant correctif) ⇒ le dépôt ne porte pas de verdict
#     FAUX aujourd'hui. Défaut **LATENT**, même famille que §2dc.2 : *une regex
#     de nom mal ancrée rend une règle aveugle sur une population entière, sans
#     jamais lever d'erreur.*
#
# ⚠️ `source_project_file()` charge la garde dans `globalenv()` (`sys.source`),
# donc `.REPORT` vit dans `globalenv()` : on le vide et on le lit LÀ.
#   ⚠️ Et `check_c2_shiny_in_r()` reçoit ses fichiers en ARGUMENT ⇒ on lui passe
#   une sonde hors de l'arbre (`tempdir()`) : rien n'est écrit dans `R/`.
# =============================================================================

source_project_file("tools/check_conventions.R")
source_project_file("R/core/io_helpers.R")

# ⚠️ PIÈGE MESURÉ : `.code_cache` est mémoïsé par CHEMIN. Deux écritures du même
# fichier temporaire rendent la PREMIÈRE annotation — un test qui réécrit la
# même sonde mesurerait l'ancienne version (et passerait au vert à tort).
# D'où : nom de fichier UNIQUE par sonde + purge du cache par sécurité.
.c2n_write <- function(lines) {
  p <- file.path(tempdir(), paste0("zzz_c2n_", basename(tempfile("p")), ".R"))
  writeLines(lines, p, useBytes = TRUE)
  if (exists(".code_cache", envir = globalenv())) {
    cc <- get(".code_cache", envir = globalenv())
    if (is.environment(cc)) rm(list = ls(cc), envir = cc)
  }
  p
}

# Rejoue C2 SEULE sur un fichier, et rend ses signalements ERROR.
.c2n_c2 <- function(path) {
  .REPORT$errors <- list()
  .REPORT$warns  <- list()
  invisible(capture.output(check_c2_shiny_in_r(path)))
  Filter(function(e) identical(e$rule, "C2"), .REPORT$errors)
}

# =============================================================================
# 1. LE DÉFAUT : une définition NOMMÉE IMBRIQUÉE déclarant input/session
# =============================================================================
test_that("C2 ne signale PAS un helper imbrique qui declare input/session (L311)", {
  p <- .c2n_write(c(
    "# sonde §2dg",
    "outer_probe <- function(dataset) {",
    "  inner_probe <- function(input, session) {",
    "    v <- input$value",
    "    session$sendCustomMessage(\"x\", v)",
    "    v",
    "  }",
    "  inner_probe",
    "}"
  ))
  on.exit(unlink(p), add = TRUE)
  c2 <- .c2n_c2(p)
  expect_length(c2, 0L)
})

# =============================================================================
# 2. LA BORNE : le même code en COLONNE 0 déclenche bien le signal (témoin)
# =============================================================================
# Sans cette direction, un « correctif » qui désactiverait C2 passerait le test 1
# sans rien corriger.
test_that("C2 signale bien un input$ HORS parametre (temoin de non-vacuite)", {
  p <- .c2n_write(c(
    "# sonde §2dg (temoin)",
    "outer_probe <- function(dataset) {",
    "  v <- input$value",
    "  v",
    "}"
  ))
  on.exit(unlink(p), add = TRUE)
  c2 <- .c2n_c2(p)
  expect_gt(length(c2), 0L)
})

# =============================================================================
# 3. LA RÉGRESSION ÉVITÉE : une définition imbriquée ne doit PAS écraser
#    `formals` pour la FONCTION EXTERNE (cœur du défaut §2dg)
# =============================================================================
# C'est l'assertion la plus importante du fichier. Le premier correctif
# (tolérer `\s*` sans tenir de PILE) faisait exactement cela : `log_sc <-
# function(msg)` écrasait `formals`, et les `input$` de la fonction externe —
# POURTANT déclarés — devenaient 31 faux positifs sur `R/` (mesuré).
test_that("une def imbriquee n'ecrase PAS les formals de la fonction externe", {
  p <- .c2n_write(c(
    "# sonde §2dg (restauration de portee)",
    "outer_probe <- function(input, session) {",
    "  helper <- function(msg) {",
    "    paste(msg)",
    "  }",
    "  v <- input$value",          # APRES le helper : doit rester LEGAL
    "  session$sendCustomMessage(\"x\", v)",
    "  v",
    "}"
  ))
  on.exit(unlink(p), add = TRUE)
  c2 <- .c2n_c2(p)
  expect_length(c2, 0L)
})

# =============================================================================
# 4. VERROU SOURCE : le motif tolère l'indentation, ET la pile existe
# =============================================================================
test_that("le motif de detection de fonctions tolere l'indentation (verrou source)", {
  src <- readLines(file.path(ts_project_root(), "tools/check_conventions.R"),
                   warn = FALSE, encoding = "UTF-8")
  # La ligne du motif : contient `grepl(` et la classe de noms de fonction.
  cand <- grep("A-Za-z_.", src, value = TRUE, fixed = TRUE)
  cand <- cand[grepl("grepl(", cand, fixed = TRUE) &
                 grepl("function", cand, fixed = TRUE)]
  expect_gte(length(cand), 1L)
  # On isole ce qui SUIT l'ancre `^` : le motif doit y porter l'échappement
  # `\\s*` (indentation optionnelle). S'il commence DIRECTEMENT par la classe
  # `[A-Za-z_.]`, l'ancre est nue ⇒ une définition imbriquée est invisible.
  after_anchor <- sub("^.*?\\^", "", cand[1])
  # Le fichier source écrit `\\s*` (deux caractères antislash dans le TEXTE, R
  # les relit tels quels) : on compare donc à la chaîne LITTÉRALE `\\s*`.
  expect_true(startsWith(after_anchor, "\\\\s*"),
              info = paste("motif L311 ancre sans \\s* :", substr(after_anchor, 1, 24)))
})

test_that("la PILE de portee est presente (la tolerance seule ne suffit pas)", {
  src <- readLines(file.path(ts_project_root(), "tools/check_conventions.R"),
                   warn = FALSE, encoding = "UTF-8")
  code <- sub("#.*$", "", src)
  # la pile de portee et l'helper de solde d'accolades doivent exister
  expect_true(any(grepl("\\.brace_balance\\s*<-\\s*function", code)))
  expect_true(any(grepl("scope\\[\\[length\\(scope\\) \\+ 1L\\]\\]", code)),
              info = "pile de portee absente : un helper local ecraserait formals")
})

# =============================================================================
# 5. INVARIANT : aucune erreur C2 réelle sur R/ après le correctif
# =============================================================================
test_that("aucune erreur C2 sur R/ apres le correctif", {
  root <- ts_project_root()
  r_files <- list.files(file.path(root, "R"), pattern = "\\.R$",
                        recursive = TRUE, full.names = TRUE)
  .REPORT$errors <- list()
  .REPORT$warns  <- list()
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  invisible(capture.output(check_c2_shiny_in_r(r_files)))
  c2 <- Filter(function(e) identical(e$rule, "C2"), .REPORT$errors)
  expect_length(c2, 0L)
  # Contrôle de validité : la sonde a bien examiné des fichiers.
  expect_gt(length(r_files), 0L)
})
